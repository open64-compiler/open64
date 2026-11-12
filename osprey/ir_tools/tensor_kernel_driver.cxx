/* Copyright (C) 2026 Open64 Project */
/* Focused driver for PU-local tensor lowering and mapped device checkpoints.
 * Design: doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md. Only this driver
 * selects PU contexts and owns JSON handoffs, snapshots and process lifetime.
 * No JsonCpp or frontend builder dependency is introduced into be.so. */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <json/json.h>
#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bread.h"
#include "ir_bwrite.h"
#include "glob.h"
#include "errors.h"
#include "erglob.h"
#include "err_host.tab"
#include "config.h"
#include "config_opt.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_gatekeeper.h"
#include "dsl_shape.h"
#include "../torch2whirl/python/native/quantized_cache_inference.h"
#include "tensor_kernel_lower.h"
#include "tensor_attention_search.h"
#include "tensor_schedule_source.h"
#ifdef OPEN64_TENSOR_SELECTED_BRIDGE
#include "tensor_selected_bridge.h"
#include "../be/lno/dsl_high_binding_keys.h"
#ifdef OPEN64_TENSOR_BRIDGE_TEST
#include "dep_graph.h"
#endif
#include "tensor_selected_key.h"
#include "be_symtab.h"
#endif

BOOL Run_vsaopt=FALSE;
INT8 Debug_Level=0;
/* No compiler-owned signal cleanup is needed by this process driver. */
void Signal_Cleanup(INT) {}
/* Native driver diagnostics use no target-specific formatting extension. */
const char *Host_Format_Parm(INT,MEM_PTR) {return "tensor-kernel";}

namespace {
UINT64 native_initializations=0,materialized_kernels=0;
/* Reject malformed handoffs/options before publishing any artifact. */
void need(bool ok,const char *message) {if(!ok) throw std::runtime_error(message);}
/* Initialize the established n64 compiler context for source or device mode. */
void initialize(bool device) {
    ++native_initializations;
    MEM_Initialize();Set_Error_Tables(Phases,host_errlist);Init_Error_Handler(10);Set_Error_File(NULL);Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure();Init_Controls_Tbl();ABI_Name="n64";Configure();IR_reader_init();WHIRL_Mldid_Mstid_On=TRUE;
    OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
    if(device) {Initialize_Symbol_Tables(TRUE);DST_Init(NULL,0);need(DSL_Builder_Begin_Program(),"cannot begin device program");}
}
/* The detached recipe uses a deterministic logical operation identifier. */
std::string kernel_name(size_t ordinal);
/* Exact schema for an offline device envelope; no arch-to-SM-count guess. */
const char *resource_names[]={"max_blocks","max_warps","max_threads","max_threads_per_cta",
    "registers_per_sm","max_registers_per_thread","register_quantum","register_warp_quantum",
    "shared_capacity","shared_budget","shared_quantum","shared_reserved"};
Json::Value profile_json(const VHO_TENSOR_GPU_PROFILE &g) {
    Json::Value j;j["schema"]="open64.mla-gpu-profile.v1";j["id"]=g.id;j["architecture"]=g.architecture;
    j["provenance"]="provided-capacities-estimated-registers";j["sms"]=Json::UInt64(g.sms);
    j["registers_per_thread_estimate"]=Json::UInt64(g.registers_per_thread);
    j["target_ctas_per_sm"]=Json::UInt64(g.target_ctas_per_sm);j["scratch_budget_bytes"]=Json::UInt64(g.scratch_budget);
    const DSL_HARDWARE_RESOURCE_RECORD &r=g.resources;
    const uint64_t *v[]={&r.max_blocks,&r.max_warps,&r.max_threads,&r.max_threads_per_cta,
        &r.registers_per_sm,&r.max_registers_per_thread,&r.register_quantum,&r.register_warp_quantum,
        &r.shared_capacity,&r.shared_budget,&r.shared_quantum,&r.shared_reserved};
    for(UINT32 i=0;i<12;++i) j["resources"][resource_names[i]]=Json::UInt64(*v[i]);return j;
}
VHO_TENSOR_GPU_PROFILE profile_decode(const Json::Value &j) {
    need(j.isObject() && j.size()==9 && j["schema"].isString() && j["schema"].asString()=="open64.mla-gpu-profile.v1" &&
        j["id"].isString() && j["architecture"].isString() && j["provenance"].isString() &&
        j["provenance"].asString()=="provided-capacities-estimated-registers" &&
        j["sms"].isUInt64() && j["registers_per_thread_estimate"].isUInt64() &&
        j["target_ctas_per_sm"].isUInt64() && j["scratch_budget_bytes"].isUInt64() &&
        j["resources"].isObject() && j["resources"].size()==12,"invalid MLA GPU profile schema");
    VHO_TENSOR_GPU_PROFILE g;g.id=j["id"].asString();g.architecture=j["architecture"].asString();
    g.sms=j["sms"].asUInt64();g.registers_per_thread=j["registers_per_thread_estimate"].asUInt64();
    g.target_ctas_per_sm=j["target_ctas_per_sm"].asUInt64();g.scratch_budget=j["scratch_budget_bytes"].asUInt64();
    DSL_HARDWARE_RESOURCE_RECORD &r=g.resources;
    uint64_t *v[]={&r.max_blocks,&r.max_warps,&r.max_threads,&r.max_threads_per_cta,
        &r.registers_per_sm,&r.max_registers_per_thread,&r.register_quantum,&r.register_warp_quantum,
        &r.shared_capacity,&r.shared_budget,&r.shared_quantum,&r.shared_reserved};
    for(UINT32 i=0;i<12;++i) {
        const Json::Value &x=j["resources"][resource_names[i]];need(x.isUInt64(),"invalid/missing MLA resource field");*v[i]=x.asUInt64();
    }
    need(VHO_tensor_gpu_profile_valid(g,stderr),"MLA GPU profile rejected");return g;
}
VHO_TENSOR_GPU_PROFILE read_profile(const char *path) {
    std::ifstream in(path);Json::Value j;Json::CharReaderBuilder reader;std::string errors;
    reader["allowComments"]=false;reader["collectComments"]=false;reader["strictRoot"]=true;
    reader["failIfExtra"]=true;reader["rejectDupKeys"]=true;
    need(in.good() && Json::parseFromStream(reader,in,&j,&errors),"invalid MLA GPU profile JSON");
    return profile_decode(j);
}
/* Persist the complete deterministic candidate ledger, including rejections. */
Json::Value selection_json(const VHO_TENSOR_PROGRAM &p) {
    VHO_TENSOR_CHUNK_SELECTION s;need(VHO_tensor_select_attention_chunks(p,p.gpu,&s,stderr),"automatic selection rejected");
    Json::Value j;j["schema"]="open64.mla-chunk-selection.v1";j["policy"]=VHO_tensor_chunk_policy_name(p);
    j["profile"]=profile_json(p.gpu);j["selected_width"]=s.width;j["short_direct"]=s.short_direct;
    j["candidates"]=Json::Value(Json::arrayValue);
    for(size_t i=0;i<s.candidates.size();++i) {
        const VHO_TENSOR_CHUNK_CANDIDATE &c=s.candidates[i];Json::Value x;
        if(p.chunk_policy>=3) x["block_x"]=Json::UInt64(c.threads);
        if(p.chunk_policy>=4) x["statistics"]=p.chunk_policy==6 && c.width==64?"warp_owned_partials":c.width==32?"warp_register_broadcast":p.chunk_policy==5 && c.width==64?"two_warp_register_broadcast":"shared";
        x["width"]=Json::UInt64(c.width);x["chunks"]=Json::UInt64(c.chunks);x["ctas"]=Json::UInt64(c.ctas);
        x["local_shared_bytes"]=Json::UInt64(c.local_shared);x["merge_shared_bytes"]=Json::UInt64(c.merge_shared);
        x["scratch_bytes"]=Json::UInt64(c.scratch);x["estimated_resident_ctas"]=Json::UInt64(c.resident);
        x["target_ctas"]=Json::UInt64(c.goal);x["coverage_ctas"]=Json::UInt64(c.coverage);
        x["eligible"]=c.eligible;x["reason"]=c.reason;j["candidates"].append(x);
    }
    return j;
}

/* JSON remains standalone-driver-owned; the VHO estimator has typed inputs. */
Json::Value encode(const VHO_TENSOR_PROGRAM &);
VHO_TENSOR_PROGRAM decode_json(const Json::Value &);
#include "tensor_attention_search_json.inc"

/* Serialize copied recipe facts only; generated arithmetic remains native WHIRL. */
Json::Value encode(const VHO_TENSOR_PROGRAM &p) {
    Json::Value j;j["schema"]="open64.tensor-kernel.v1";j["source_pu"]=p.source_pu;j["source_file"]=p.source_file;
    j["opt_level"]=p.opt_level;j["threads"]=p.threads;j["values"]=Json::Value(Json::arrayValue);j["operations"]=Json::Value(Json::arrayValue);
    if(p.fuse) {
        j["schema"]="open64.tensor-kernel.v2";j["fuse"]=true;j["split_width"]=p.split_width;
        j["reassociation"]=(p.split_width || p.cooperative)?"allow":"forbid";
        if(p.cooperative) {j["schema"]="open64.tensor-kernel.v3";j["cooperative"]=true;}
        if(p.fuse_attention) {j["schema"]="open64.tensor-kernel.v4";j["fuse_attention"]=true;j["attention_numerics"]="fp32-local-softmax-rescaled-merge";}
    }
    if(p.register_latent) {j["schema"]="open64.tensor-kernel.v5";j["register_latent"]=true;}
    if(p.auto_chunks) {j["schema"]="open64.tensor-kernel.v6";j["chunk_selection"]=selection_json(p);}
    if(p.search_selected) {j["schema"]="open64.tensor-kernel.v7";j["attention_search"]=search_evidence(p);}
    /* Fixed physical policy survives the driver handoff without an automatic
     * selector or search certificate that would reopen candidate exploration. */
    if(!p.auto_chunks && !p.search_selected && p.register_latent && p.chunk_policy!=1) {
        j["schema"]="open64.tensor-kernel.v8";j["fixed_chunk_policy"]=p.chunk_policy;
    }
    for(size_t i=0;i<p.values.size();++i) {
        const VHO_TENSOR_VALUE &v=p.values[i];Json::Value x;x["name"]=v.name;x["dtype"]=v.dtype;x["role"]=v.role;x["ordinal"]=v.ordinal;x["storage"]=v.storage;x["shape"]=Json::Value(Json::arrayValue);
        for(size_t a=0;a<v.shape.size();++a) x["shape"].append(Json::UInt64(v.shape[a]));j["values"].append(x);
    }
    /* Version the optional semantic extension independently of the unchanged
     * physical recipe versions. Legacy unannotated JSON stays identical. */
    Json::Value traits;traits["schema"]="open64.tensor-cache-traits.v1";
    traits["values"]=Json::Value(Json::arrayValue);
    for(size_t i=0;i<p.values.size();++i) if(!p.values[i].cache_trait.empty()) {
        Json::Value value;value["value"]=Json::UInt(i);value["trait"]=p.values[i].cache_trait;
        traits["values"].append(value);
    }
    if(traits["values"].size()) j["tensor_cache_traits"]=traits;
    for(size_t i=0;i<p.operations.size();++i) {
        const VHO_TENSOR_OPERATION &op=p.operations[i];Json::Value x;x["name"]=op.name;x["version"]=op.version;x["source_node"]=op.source_node;x["source_line"]=op.source_line;x["output"]=op.output;
        x["inputs"]=Json::Value(Json::arrayValue);x["attributes"]=Json::Value(Json::objectValue);
        for(size_t a=0;a<op.inputs.size();++a) x["inputs"].append(op.inputs[a]);
        for(std::map<std::string,std::string>::const_iterator a=op.attributes.begin();a!=op.attributes.end();++a) x["attributes"][a->first]=a->second;
        x["kernel"]=op.name=="common.reshape"?"":kernel_name(i);
        x["block_x"]=p.threads;x["grid_x"]=Json::UInt64(p.opt_level==3?(VHO_tensor_elements(p.values[op.output])+p.threads-1)/p.threads:1);
        j["operations"].append(x);
    }
    /* This checksum detects stale/corrupted detached handoffs. The retained
     * source-image SHA256 is the artifact identity, not this numeric seal. */
    Json::StyledWriter canonical;std::string bytes=canonical.write(j);UINT64 seal=1469598103934665603ULL;
    for(size_t i=0;i<bytes.size();++i) seal=(seal^(unsigned char)bytes[i])*UINT64(1099511628211ULL);
    std::ostringstream stamp;stamp<<seal;j["recipe_fingerprint"]=stamp.str();return j;
}
/* Format a logical kernel identifier, avoiding source identifier injection. */
std::string kernel_name(size_t ordinal) {std::ostringstream s;s<<"tensor_op_"<<ordinal;return s.str();}
/* Bind each actual device symbol to the sealed complete specialization. This
 * allows different lengths/levels to coexist in one AOT CUDA translation unit. */
std::string kernel_symbol(const VHO_TENSOR_PROGRAM &p,size_t ordinal) {
    std::ostringstream s;s<<"tensor_"<<encode(p)["recipe_fingerprint"].asString()<<"_op_"<<ordinal;return s.str();
}
/* Read only the explicit bounded JSON recipe schema; no coercion of fields. */
VHO_TENSOR_PROGRAM decode_json(const Json::Value &j) {
    if(j["schema"].isString() && j["schema"].asString()=="open64.tensor-kernel.v7") {
        VHO_TENSOR_PROGRAM p=search_admit(j["attention_search"]);
        need(search_text(encode(p))==search_text(j),"stale selected MLA recipe");return p;
    }
    bool fixed=j["schema"].asString()=="open64.tensor-kernel.v8";
    bool automatic=j["schema"].asString()=="open64.tensor-kernel.v6";
    bool registers=fixed || automatic || j["schema"].asString()=="open64.tensor-kernel.v5";
    bool attention=registers || j["schema"].asString()=="open64.tensor-kernel.v4";
    bool cooperative=attention || j["schema"].asString()=="open64.tensor-kernel.v3";
    bool optimized=cooperative || j["schema"].asString()=="open64.tensor-kernel.v2";
    need(j.isObject() && j.size()==((automatic||fixed)?16:registers?15:attention?14:cooperative?12:optimized?11:8)+j.isMember("tensor_cache_traits") && j["recipe_fingerprint"].isString() && (optimized || j["schema"].asString()=="open64.tensor-kernel.v1") && j["source_pu"].isString() && j["source_file"].isString() &&
        j["opt_level"].isUInt() && j["threads"].isUInt() && j["values"].isArray() && j["operations"].isArray(),"invalid recipe header");
    need(j["values"].size()<=4096 && j["operations"].size()<=4096,"oversized recipe");
    VHO_TENSOR_PROGRAM p;p.source_pu=j["source_pu"].asString();p.source_file=j["source_file"].asString();p.opt_level=j["opt_level"].asUInt();p.threads=j["threads"].asUInt();
    if(optimized) {
        need(j["fuse"].isBool() && j["fuse"].asBool() && j["split_width"].isUInt() && j["reassociation"].isString(),"invalid optimization policy");
        p.fuse=true;p.split_width=j["split_width"].asUInt();
        if(cooperative) {need(j["cooperative"].isBool() && j["cooperative"].asBool(),"invalid cooperative policy");p.cooperative=true;}
        if(attention) {need(j["fuse_attention"].isBool() && j["fuse_attention"].asBool() && j["attention_numerics"].isString() && j["attention_numerics"].asString()=="fp32-local-softmax-rescaled-merge","invalid attention numerical policy");p.fuse_attention=true;}
        if(registers) {need(j["register_latent"].isBool() && j["register_latent"].asBool(),"invalid register accumulation policy");p.register_latent=true;}
        if(fixed) {
            need(j["fixed_chunk_policy"].isUInt() && j["fixed_chunk_policy"].asUInt()>=2 &&
                j["fixed_chunk_policy"].asUInt()<=6,"invalid fixed physical policy");p.chunk_policy=j["fixed_chunk_policy"].asUInt();
        }
        if(automatic) {
            const Json::Value &selection=j["chunk_selection"];
            need(selection.isObject() && selection.size()==6 && selection["schema"].isString() &&
                selection["schema"].asString()=="open64.mla-chunk-selection.v1" && selection["policy"].isString() &&
                (selection["policy"].asString()=="coverage-v1" || selection["policy"].asString()=="coverage-v2" || selection["policy"].asString()=="coverage-v3" || selection["policy"].asString()=="coverage-v4" || selection["policy"].asString()=="coverage-v5" || selection["policy"].asString()=="coverage-v6"),"invalid automatic chunk schema/policy");
            p.chunk_policy=selection["policy"].asString()=="coverage-v1"?1:selection["policy"].asString()=="coverage-v2"?2:selection["policy"].asString()=="coverage-v3"?3:selection["policy"].asString()=="coverage-v4"?4:selection["policy"].asString()=="coverage-v5"?5:6;
            p.auto_chunks=true;p.gpu=profile_decode(selection["profile"]);
        }
        need(j["reassociation"].asString()==((p.split_width || p.cooperative)?"allow":"forbid"),"reassociation policy mismatch");
    }
    for(Json::ArrayIndex i=0;i<j["values"].size();++i) {
        const Json::Value &x=j["values"][i];need(x.isObject() && x.size()==6 && x["name"].isString() && x["dtype"].isString() && x["role"].isUInt() && x["ordinal"].isUInt() && x["storage"].isUInt() && x["shape"].isArray(),"invalid value record");
        VHO_TENSOR_VALUE v;v.name=x["name"].asString();v.dtype=x["dtype"].asString();v.role=x["role"].asUInt();v.ordinal=x["ordinal"].asUInt();v.storage=x["storage"].asUInt();
        for(Json::ArrayIndex a=0;a<x["shape"].size();++a) {need(x["shape"][a].isUInt64(),"invalid extent");v.shape.push_back(x["shape"][a].asUInt64());}p.values.push_back(v);
    }
    if(j.isMember("tensor_cache_traits")) {
        const Json::Value &traits=j["tensor_cache_traits"];
        need(traits.isObject() && traits.size()==2 && traits["schema"].isString() &&
            traits["schema"].asString()=="open64.tensor-cache-traits.v1" && traits["values"].isArray() &&
            traits["values"].size()>0 && traits["values"].size()<=p.values.size(),"invalid cache trait extension");
        for(Json::ArrayIndex i=0;i<traits["values"].size();++i) {
            const Json::Value &value=traits["values"][i];
            need(value.isObject() && value.size()==2 && value["value"].isUInt() &&
                value["value"].asUInt()<p.values.size() && value["trait"].isString(),"invalid cache trait record");
            VHO_TENSOR_VALUE &tensor=p.values[value["value"].asUInt()];
            need(tensor.cache_trait.empty() && DSL_tensor_cache_trait_known(value["trait"].asString().c_str()),
                "duplicate or unsupported cache trait");
            tensor.cache_trait=value["trait"].asString();
        }
    }
    for(Json::ArrayIndex i=0;i<j["operations"].size();++i) {
        const Json::Value &x=j["operations"][i];need(x.isObject() && x.size()==10 && x["name"].isString() && x["version"].isUInt() && x["source_node"].isUInt() && x["source_line"].isUInt() && x["output"].isUInt() && x["inputs"].isArray() && x["attributes"].isObject(),"invalid operation record");
        VHO_TENSOR_OPERATION op;op.name=x["name"].asString();op.version=x["version"].asUInt();op.source_node=x["source_node"].asUInt();op.source_line=x["source_line"].asUInt();op.output=x["output"].asUInt();
        for(Json::ArrayIndex a=0;a<x["inputs"].size();++a) {need(x["inputs"][a].isUInt(),"invalid operand");op.inputs.push_back(x["inputs"][a].asUInt());}
        Json::Value::Members names=x["attributes"].getMemberNames();
        for(size_t a=0;a<names.size();++a) {need(x["attributes"][names[a]].isString(),"non-string attribute");op.attributes[names[a]]=x["attributes"][names[a]].asString();}
        need(x["kernel"].isString() && x["kernel"].asString()==(op.name=="common.reshape"?"":kernel_name(i)),"kernel identity mismatch");
        p.operations.push_back(op);
    }
    need(VHO_tensor_validate_program(p,stderr),"recipe admission failed");
    Json::StyledWriter canonical;
    need(canonical.write(encode(p))==canonical.write(j),"noncanonical or stale launch recipe");return p;
}
/* Parse recipe files with the shared strict standalone document reader. */
VHO_TENSOR_PROGRAM decode(const char *path) {return decode_json(search_read(path));}
/* Publish a fresh JSON handoff; failed writes do not masquerade as valid plans. */
void write_json(const char *path,const Json::Value &j) {
    int fd=open(path,O_WRONLY|O_CREAT|O_EXCL,0600);need(fd>=0,"recipe output already exists or is unavailable");
    FILE *out=fdopen(fd,"w");need(out!=NULL,"cannot open recipe stream");Json::StyledWriter writer;std::string text=writer.write(j);
    bool ok=fwrite(text.data(),1,text.size(),out)==text.size();if(fclose(out)) ok=false;if(!ok) unlink(path);need(ok,"recipe write failed");
}
/* Publish a complete source view without replacing an existing file. */
void write_source(const char *path,const std::string &source) {
    std::ostringstream temporary;temporary<<path<<".tmp-"<<getpid();std::string temp=temporary.str();
    int fd=open(temp.c_str(),O_WRONLY|O_CREAT|O_EXCL,0600);need(fd>=0,"cannot create source staging file");
    FILE *out=fdopen(fd,"w");if(!out) {close(fd);unlink(temp.c_str());need(false,"cannot open source staging stream");}
    bool ok=fwrite(source.data(),1,source.size(),out)==source.size();if(fclose(out)) ok=false;
    if(!ok) {unlink(temp.c_str());need(false,"source write failed");}
    int result=link(temp.c_str(),path);unlink(temp.c_str());need(result==0,"source destination exists or is unavailable");
}
/* A child writes snapshots because the standard writer consumes subsection
 * state. Parent PU trees/tables remain valid for the subsequent transformation. */
void checkpoint(const std::vector<PU_Info *> &pus,const char *path) {
    need(access(path,F_OK)!=0,"checkpoint already exists");std::string temp=std::string(path)+".tmp";need(access(temp.c_str(),F_OK)!=0,"checkpoint staging exists");
    fflush(NULL);pid_t child=fork();need(child>=0,"checkpoint fork failed");
    if(!child) {
        Irb_File_Name=const_cast<char *>(temp.c_str());if(!Open_Output_Info(Irb_File_Name)) _exit(1);
        for(size_t i=0;i<pus.size();++i) {if(!DSL_Builder_Select_PU(pus[i])) _exit(1);Write_PU_Info(pus[i]);}
        Write_Global_Info(pus[0]);Close_Output_Info();if(link(temp.c_str(),path)) _exit(1);_exit(unlink(temp.c_str())?1:0);
    }
    int status=0;need(waitpid(child,&status,0)==child && WIFEXITED(status) && !WEXITSTATUS(status),"checkpoint failed");
}
/* Read a required persisted binding, never synthesizing a missing device fact. */
std::string metadata(ST_IDX st,const char *key) {
    const char *value=ST_tensor_metadata(st,key);need(value!=NULL,"missing device metadata");return value;
}
/* Locate an actual executable loop only for deliberate audit-negative controls. */
WN *find_loop(WN *n) {
    if(WN_operator(n)==OPR_DO_LOOP) return n;
    if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) {WN *r=find_loop(s);if(r) return r;}}
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) {WN *r=find_loop(WN_kid(n,i));if(r) return r;}
    return NULL;
}
/* Locate typed collectives solely for mutation tests on reopened products. */
WN *find_collective(WN *n,const char *name) {
    if(WN_operator(n)==OPR_CALL && !strcmp(ST_name(WN_st(n)),name)) return n;
    if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) {WN *r=find_collective(s,name);if(r) return r;}}
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) {WN *r=find_collective(WN_kid(n,i),name);if(r) return r;}
    return NULL;
}
/* Negative control for the critical disjoint max/denominator slot proof. */
WN *find_denominator_slot(WN *n) {
    if(WN_operator(n)==OPR_ARRAY && WN_kid_count(n)==3 && WN_operator(WN_kid0(n))==OPR_LDA &&
       !strcmp(ST_name(WN_st(WN_kid0(n))),"attention_shared_warp_summaries")) {
        WN *index=WN_kid2(n);
        if(WN_operator(index)==OPR_ADD) for(INT i=0;i<2;++i)
            if(WN_operator(WN_kid(index,i))==OPR_INTCONST && WN_const_val(WN_kid(index,i))==2) return WN_kid(index,i);
    }
    if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) {WN *r=find_denominator_slot(s);if(r) return r;}}
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) {WN *r=find_denominator_slot(WN_kid(n,i));if(r) return r;}
    return NULL;
}
/* Find the selected tuple's warp stride only for an aliasing negative control.
 * Mutating it makes the two producers overlap; normal admission must reject it. */
WN *find_warp_partial_stride(WN *n) {
    if(WN_operator(n)==OPR_ARRAY && WN_kid_count(n)==3 && WN_operator(WN_kid0(n))==OPR_LDA &&
       !strcmp(ST_name(WN_st(WN_kid0(n))),"attention_shared_warp_partials")) {
        WN *index=WN_kid2(n);
        if(WN_operator(index)==OPR_ADD && WN_operator(WN_kid0(index))==OPR_MPY &&
           WN_operator(WN_kid1(WN_kid0(index)))==OPR_INTCONST) return WN_kid1(WN_kid0(index));
    }
    if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) {WN *r=find_warp_partial_stride(s);if(r) return r;}}
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) {WN *r=find_warp_partial_stride(WN_kid(n,i));if(r) return r;}
    return NULL;
}
/* Remove exactly one barrier from its owning BLOCK as a negative control. */
bool remove_barrier(WN *n,const char *name="__syncthreads") {
    if(WN_operator(n)==OPR_BLOCK) {
        for(WN *s=WN_first(n);s;s=WN_next(s)) {
            if(WN_operator(s)==OPR_CALL && !strcmp(ST_name(WN_st(s)),name)) {WN_EXTRACT_FromBlock(n,s);return true;}
            if(remove_barrier(s,name)) return true;
        }
    } else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i) && remove_barrier(WN_kid(n,i),name)) return true;
    return false;
}
#ifdef OPEN64_TENSOR_SELECTED_BRIDGE
#include "tensor_selected_bridge_driver.inc"
#endif
#include "tensor_quantized_review.inc"
} // namespace

/* The executable establishes exactly one source PU or fresh device translation
 * unit. The VHO service never expands its compilation scope or writes files. */
int main(int argc,char **argv) {
    try {
        if(argc==4 && !strcmp(argv[1],"q1-inspect")) {
            need(access(argv[3],F_OK)!=0,"Q1 review output exists");initialize(false);
            Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
            PU_Info *pu=Read_Global_Info(NULL);need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"Q1 requires one source PU");
            Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);
            Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
            write_json(argv[3],q1_source_review(pu));return 0;
        }
#ifdef OPEN64_TENSOR_SELECTED_BRIDGE
#ifdef OPEN64_TENSOR_BRIDGE_TEST
        if(argc>=2 && (!strcmp(argv[1],"bridge-test-multiple") || !strcmp(argv[1],"bridge-test-graph"))) return selected_bridge_command(argc,argv);
#endif
        if(argc>=2 && (!strcmp(argv[1],"bridge") || !strcmp(argv[1],"bridge-production") || !strcmp(argv[1],"bridge-audit") || !strcmp(argv[1],"bridge-ptx") || !strcmp(argv[1],"bridge-code-key")))
            return selected_bridge_command(argc,argv);
#endif
        if(argc==6 && !strcmp(argv[1],"high")) {
            need(access(argv[3],F_OK)!=0 && access(argv[4],F_OK)!=0 && access(argv[5],F_OK)!=0,
                "High checkpoint/manifest destination exists");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);
            need(!p.fuse && !p.cooperative && !p.split_width && !p.search_selected && !p.auto_chunks,
                "High algorithm requires the unoptimized semantic recipe");
            initialize(true);
            PU_Info *pu=DSL_Builder_Create_Minimal_PU("open64_common_high");
            need(pu && DSL_Builder_Select_PU(pu),"cannot create High algorithm PU");
            std::vector<PU_Info *> pus(1,pu);
            UINT32 file=DSL_Builder_Register_Source_File(pu,p.source_file.c_str());
            USRCPOS source;USRCPOS_clear(source);USRCPOS_filenum(source)=file;
            USRCPOS_linenum(source)=p.operations[0].source_line?p.operations[0].source_line:1;
            UINT32 types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);
            need(!VHO_tensor_materialize_high(p,0,NULL) && types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),
                "rejected High preflight changed tables");
            OPT_Fast_Math=TRUE;BOOL unsafe_high=VHO_tensor_materialize_high(p,USRCPOS_srcpos(source),NULL);OPT_Fast_Math=FALSE;
            need(!unsafe_high && types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),
                "unsafe High preflight changed tables");
            checkpoint(pus,argv[3]);
            need(VHO_tensor_materialize_high(p,USRCPOS_srcpos(source),stderr),"High algorithm admission failed");
            types=TY_Table_Size();symbols=ST_Table_Size(CURRENT_SYMTAB);
            need(!VHO_tensor_materialize_high(p,USRCPOS_srcpos(source),NULL) && types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),
                "nonempty High target accepted or mutated");
            need(DSL_Gatekeeper_Verify_No_Native_PU(pu,stderr),"unlowered common tensor remains");
            checkpoint(pus,argv[4]);
            Json::Value manifest;manifest["schema"]="open64.common-high.v1";
            manifest["function"]="open64_common_high";manifest["buffer_contract"]="disjoint-storage-formals";
            manifest["recipe"]=encode(p);manifest["buffers"]=Json::Value(Json::arrayValue);
            for(UINT32 i=0;i<p.values.size();++i) if(p.values[i].storage==i) manifest["buffers"].append(i);
            write_json(argv[5],manifest);
            printf("common_high=PASS operations=%lu buffers=%u runtime_calls=0 gpu_mapping=false\n",
                (unsigned long)p.operations.size(),manifest["buffers"].size());return 0;
        }
        if((argc==9 || argc==11) && !strcmp(argv[1],"explore-attention")) {
            need(!strcmp(argv[4],"--gpu-profile") && !strcmp(argv[6],"--cost-profile") && !strcmp(argv[8],"--allow-reassociate"),"exploration requires target, explicit costs and numerical permission");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);p.auto_chunks=false;p.search_selected=false;p.search_evidence.clear();p.search_identity.clear();
            p.register_latent=true;p.chunk_policy=1;p.gpu=VHO_TENSOR_GPU_PROFILE();
            VHO_MLA_SEARCH_OPTIONS options;
            if(argc==11) {need(!strcmp(argv[9],"--search-options"),"invalid search control option");options=search_options_decode(search_read(argv[10]));}
            Json::Value report=search_report(p,read_profile(argv[5]),search_cost_decode(search_read(argv[7])),options);
            need(native_initializations==0 && materialized_kernels==0,"exploration crossed native mutation boundary");
            write_json(argv[3],report);printf("estimation_only=true candidate_estimates=%s native_initializations=0 materialized_kernels=0\n",report["candidate_estimates"].asString().c_str());return 0;
        }
        if(argc==4 && !strcmp(argv[1],"select-attention")) {
            VHO_TENSOR_PROGRAM p=search_admit(search_read(argv[2]));
            write_json(argv[3],encode(p));printf("selected_only=true policy=%u width=%u materialized_kernels=0\n",p.chunk_policy,p.split_width);return 0;
        }
        if((argc==7 || argc==9) && !strcmp(argv[1],"auto-attention")) {
            need(!strcmp(argv[4],"--gpu-profile") && !strcmp(argv[6],"--allow-reassociate"),"automatic attention requires a GPU profile and explicit numerical permission");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);p.auto_chunks=false;p.search_selected=false;p.search_evidence.clear();p.search_identity.clear();p.register_latent=true;
            p.chunk_policy=6;
            if(argc==9) {
                need(!strcmp(argv[7],"--chunk-policy") && (!strcmp(argv[8],"coverage-v1") || !strcmp(argv[8],"coverage-v2") || !strcmp(argv[8],"coverage-v3") || !strcmp(argv[8],"coverage-v4") || !strcmp(argv[8],"coverage-v5") || !strcmp(argv[8],"coverage-v6")),"unknown automatic chunk policy");
                p.chunk_policy=!strcmp(argv[8],"coverage-v1")?1:!strcmp(argv[8],"coverage-v2")?2:!strcmp(argv[8],"coverage-v3")?3:!strcmp(argv[8],"coverage-v4")?4:!strcmp(argv[8],"coverage-v5")?5:6;
            }
            VHO_TENSOR_GPU_PROFILE gpu=read_profile(argv[5]);VHO_TENSOR_CHUNK_SELECTION choice;
            need(VHO_tensor_select_attention_chunks(p,gpu,&choice,stderr),"no automatic attention plan");
            p.gpu=gpu;p.split_width=choice.width;p.auto_chunks=true;VHO_TENSOR_SCHEDULE plan;
            need(VHO_tensor_schedule(p,&plan,stderr),"automatic attention rejected");write_json(argv[3],encode(p));return 0;
        }
        if(argc==7 && !strcmp(argv[1],"stream-attention")) {
            need(!strcmp(argv[4],"--split-width") && !strcmp(argv[6],"--allow-reassociate"),"register attention requires explicit split width and numerical permission");
            std::string width=argv[5];need(!width.empty() && width.find_first_not_of("0123456789")==std::string::npos && width.size()<=4,"invalid attention split width");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);need(p.fuse_attention,"register accumulation requires fused attention");
            p.auto_chunks=false;p.search_selected=false;p.search_evidence.clear();p.search_identity.clear();p.chunk_policy=1;p.gpu=VHO_TENSOR_GPU_PROFILE();p.register_latent=true;p.split_width=strtoul(argv[5],NULL,10);VHO_TENSOR_SCHEDULE plan;
            need(VHO_tensor_schedule(p,&plan,stderr),"register attention rejected");write_json(argv[3],encode(p));return 0;
        }
        if(argc==5 && !strcmp(argv[1],"fuse-attention")) {
            need(!strcmp(argv[4],"--allow-reassociate"),"attention fusion requires explicit numerical permission");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);need(!p.search_selected,"invalidate fixed search selection before refusion");p.fuse_attention=true;VHO_TENSOR_SCHEDULE plan;
            need(VHO_tensor_schedule(p,&plan,stderr),"attention fusion rejected");write_json(argv[3],encode(p));return 0;
        }
        if(argc==5 && !strcmp(argv[1],"cooperate")) {
            need(!strcmp(argv[4],"--allow-reassociate"),"cooperative trees require explicit reassociation permission");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);need(!p.search_selected,"invalidate fixed search selection before remapping");p.cooperative=true;VHO_TENSOR_SCHEDULE plan;
            need(VHO_tensor_schedule(p,&plan,stderr),"cooperative schedule rejected");write_json(argv[3],encode(p));return 0;
        }
        if(argc==4 && !strcmp(argv[1],"schedule-source")) {
            VHO_TENSOR_PROGRAM p=decode(argv[2]);VHO_TENSOR_SCHEDULE plan;
            need(VHO_tensor_schedule(p,&plan,stderr),"schedule rejected");
            write_source(argv[3],W2C_tensor_schedule_source(plan,VHO_tensor_explanations(plan),encode(p)["recipe_fingerprint"].asString()));
            printf("schedule_source=PASS stages=%lu\n",(unsigned long)plan.kernels.size());return 0;
        }
        if((argc==5 || argc==8) && !strcmp(argv[1],"optimize")) {
            need(!strcmp(argv[4],"--fuse"),"expected --fuse");VHO_TENSOR_PROGRAM p=decode(argv[2]);need(!p.search_selected,"invalidate fixed search selection before optimization");p.fuse=true;
            if(argc==8) {
                need(!strcmp(argv[5],"--split-width") && !strcmp(argv[7],"--allow-reassociate"),"split reductions require explicit reassociation permission");
                char *end=NULL;unsigned long width=strtoul(argv[6],&end,10);
                need(end && !*end && width>=2 && width<=4096,"invalid split width");p.split_width=width;
            }
            VHO_TENSOR_SCHEDULE plan;need(VHO_tensor_schedule(p,&plan,stderr),"schedule selection failed");
            write_json(argv[3],encode(p));printf("selected_kernels=%lu fusion=true split_width=%u\n",(unsigned long)plan.kernels.size(),p.split_width);return 0;
        }
        if(argc==6 && !strcmp(argv[1],"prepare")) {
            need(!strcmp(argv[4],"--verify"),"expected --verify");
            need(strlen(argv[5])==1 && argv[5][0]>='0' && argv[5][0]<='3',"invalid optimization level");initialize(false);
            Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
            PU_Info *pu=Read_Global_Info(NULL);need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"requires one source PU");
            Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
            IR_Dwarf_Gen_File_Table(FALSE);VHO_TENSOR_PROGRAM p;UINT32 symbols=ST_Table_Size(CURRENT_SYMTAB),types=TY_Table_Size();
            need(VHO_tensor_prepare_program(pu,argv[5][0]-'0',&p,stderr),"source preparation failed");
            need(symbols==ST_Table_Size(CURRENT_SYMTAB) && types==TY_Table_Size(),"source preparation mutated tables");
            need(!p.source_file.empty(),"missing source mapping");write_json(argv[3],encode(p));
            printf("source_pu=%s operations=%lu values=%lu opt=%u source_unchanged=true\n",p.source_pu.c_str(),(unsigned long)p.operations.size(),(unsigned long)p.values.size(),p.opt_level);
            return 0;
        }
        if(argc==5 && (!strcmp(argv[1],"device") || !strcmp(argv[1],"device-explain"))) {
            bool explain_checkpoint=!strcmp(argv[1],"device-explain");
            need(access(argv[3],F_OK)!=0 && access(argv[4],F_OK)!=0,"checkpoint destination exists");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);VHO_TENSOR_SCHEDULE plan;need(VHO_tensor_schedule(p,&plan,stderr),"schedule rejected");
            initialize(true);std::vector<PU_Info *> pus;std::vector<UINT32> operations;
            for(size_t k=0;k<plan.kernels.size();++k) {UINT32 i=plan.kernels[k].id;
                PU_Info *pu=DSL_Builder_Create_Minimal_PU(kernel_symbol(p,i).c_str());need(pu && DSL_Builder_Select_PU(pu),"cannot create device PU");
                ST &function=St_Table[PU_Info_proc_sym(pu)];Set_ST_device_symbol_kind(function,DEVICE_SYMBOL_KERNEL);Set_ST_export(function,EXPORT_PREEMPTIBLE);pus.push_back(pu);operations.push_back(i);
            }
            need(!pus.empty(),"no executable kernels");if(!explain_checkpoint) checkpoint(pus,argv[3]);
            for(size_t i=0;i<pus.size();++i) {
                need(DSL_Builder_Select_PU(pus[i]),"cannot select device PU");UINT32 file=DSL_Builder_Register_Source_File(pus[i],p.source_file.c_str());
                const VHO_TENSOR_OPERATION &op=p.operations[plan.kernels[i].root];
                USRCPOS source;USRCPOS_clear(source);USRCPOS_filenum(source)=file;USRCPOS_linenum(source)=op.source_line?op.source_line:1;
                UINT32 types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);
                need(!VHO_tensor_materialize_kernel(p,operations[i],0,NULL),"zero source accepted");
                OPT_Fast_Math=TRUE;BOOL unsafe=VHO_tensor_materialize_kernel(p,operations[i],USRCPOS_srcpos(source),NULL);OPT_Fast_Math=FALSE;
                need(!unsafe && types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),"failed preflight mutated tables");
                ++materialized_kernels;need(VHO_tensor_materialize_kernel(p,operations[i],USRCPOS_srcpos(source),stderr),"kernel materialization failed");
                need(VHO_tensor_verify_kernel(p,operations[i],stderr) && !VHO_tensor_materialize_kernel(p,operations[i],USRCPOS_srcpos(source),NULL),"device verification/nonempty admission failed");
                need(DSL_Gatekeeper_Verify_No_Native_PU(pus[i],stderr),"unlowered tensor remains");
                printf("kernel=%s operation=%s elements=%llu threads=%u stage=%u\n",kernel_symbol(p,operations[i]).c_str(),op.name.c_str(),(unsigned long long)plan.kernels[i].elements,plan.kernels[i].threads,plan.kernels[i].stage);
            }
            if(explain_checkpoint) checkpoint(pus,argv[3]);
            for(size_t i=0;i<pus.size();++i) {
                need(DSL_Builder_Select_PU(pus[i]),"cannot select annotation PU");
                need(VHO_tensor_annotate_kernel(p,operations[i],stderr) && VHO_tensor_verify_kernel(p,operations[i],stderr),"WHIRL explanation failed");
                UINT32 types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);
                need(!VHO_tensor_annotate_kernel(p,operations[i],NULL) && types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),"repeated annotation mutated tables");
            }
            checkpoint(pus,argv[4]);if(p.search_selected) printf("selected_only=true native_initializations=%llu materialized_kernels=%llu\n",(unsigned long long)native_initializations,(unsigned long long)materialized_kernels);return 0;
        }
        if(argc==5 && (!strcmp(argv[1],"audit") || !strcmp(argv[1],"audit-mutate") || !strcmp(argv[1],"audit-binding") || !strcmp(argv[1],"audit-prototype") || !strcmp(argv[1],"audit-policy") || !strcmp(argv[1],"audit-comment") || !strcmp(argv[1],"audit-comment-missing") || !strcmp(argv[1],"audit-mask") || !strcmp(argv[1],"audit-barrier") || !strcmp(argv[1],"audit-resource") || !strcmp(argv[1],"audit-shared") || !strcmp(argv[1],"audit-collective-prototype") || !strcmp(argv[1],"audit-attention-layout") || !strcmp(argv[1],"audit-attention-accumulation") || !strcmp(argv[1],"audit-attention-selection") || !strcmp(argv[1],"audit-attention-statistics") || !strcmp(argv[1],"audit-two-warp-summary-alias") || !strcmp(argv[1],"audit-warp-partial-alias") || !strcmp(argv[1],"audit-warp-partial-barrier") || !strcmp(argv[1],"audit-broadcast-mask") || !strcmp(argv[1],"audit-broadcast-source") || !strcmp(argv[1],"audit-broadcast-width") || !strcmp(argv[1],"audit-broadcast-prototype") || !strcmp(argv[1],"audit-broadcast-missing") || !strcmp(argv[1],"audit-attention-block") || !strcmp(argv[1],"audit-warp-mask") || !strcmp(argv[1],"audit-warp-prototype") || !strcmp(argv[1],"audit-warp-barrier"))) {
            VHO_TENSOR_PROGRAM p=decode(argv[3]);VHO_TENSOR_SCHEDULE plan;need(VHO_tensor_schedule(p,&plan,stderr),"schedule rejected");
            initialize(false);Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
            PU_Info *pu=Read_Global_Info(NULL);Json::Value result;result["schema"]="open64.device-tensor-launch.v1";result["kernels"]=Json::Value(Json::arrayValue);
            std::map<UINT32,Json::Value> storages;
            for(size_t k=0;k<plan.kernels.size();++k) {UINT32 i=plan.kernels[k].id;
                need(pu && !PU_Info_child(pu),"missing/nested device PU");Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;
                Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
                ST_IDX st=PU_Info_proc_sym(pu);need(kernel_symbol(p,i)==ST_name(st),"device order/name mismatch");
                if(!strcmp(argv[1],"audit-mutate")) {WN *loop=find_loop(PU_Info_tree_ptr(pu));need(loop,"missing mutation target");WN_const_val(WN_kid1(WN_end(loop)))+=1;}
                if(!strcmp(argv[1],"audit-binding")) ST_tensor_bind_metadata(st,"tensor.kernel.block_x","64");
                if(!strcmp(argv[1],"audit-policy")) ST_tensor_bind_metadata(st,"tensor.kernel.split_width","1");
                if(!strcmp(argv[1],"audit-comment") || !strcmp(argv[1],"audit-comment-missing")) {
                    WN *body=WN_func_body(PU_Info_tree_ptr(pu)),*comment=WN_first(body);need(comment && WN_operator(comment)==OPR_COMMENT,"missing comment mutation target");
                    if(!strcmp(argv[1],"audit-comment")) Set_ST_name(*WN_st(comment),Save_Str("changed explanation"));
                    else WN_EXTRACT_FromBlock(body,comment);
                }
                if(!strcmp(argv[1],"audit-prototype")) {
                    TY_IDX wrong=TY_pointed(ST_type(WN_st(WN_formal(PU_Info_tree_ptr(pu),0))));
                    Set_TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(&St_Table[st]))],wrong);
                }
                if(!strcmp(argv[1],"audit-attention-block") && plan.kernels[k].mapping==3)
                    ST_tensor_bind_metadata(st,"tensor.kernel.block_x",plan.kernels[k].threads==128?"64":"128");
                WN *warp_fence=find_collective(PU_Info_tree_ptr(pu),"__syncwarp");
                if(warp_fence && !strcmp(argv[1],"audit-warp-mask")) WN_const_val(WN_kid0(WN_kid0(warp_fence)))=0xffff;
                if(warp_fence && !strcmp(argv[1],"audit-warp-prototype"))
                    Set_TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(WN_st(warp_fence)))+1],MTYPE_To_TY(MTYPE_I4));
                if(warp_fence && !strcmp(argv[1],"audit-warp-barrier")) remove_barrier(PU_Info_tree_ptr(pu),"__syncwarp");
                if(!strcmp(argv[1],"audit-attention-statistics")) ST_tensor_bind_metadata(st,"tensor.kernel.attention_statistics","stale_statistics");
                if(!strcmp(argv[1],"audit-two-warp-summary-alias")) {WN *slot=find_denominator_slot(PU_Info_tree_ptr(pu));if(slot) WN_const_val(slot)=0;}
                if(plan.kernels[k].warp_owned_partials && !strcmp(argv[1],"audit-warp-partial-alias")) {
                    WN *stride=find_warp_partial_stride(PU_Info_tree_ptr(pu));need(stride,"missing warp tuple stride target");WN_const_val(stride)=0;
                }
                if(plan.kernels[k].warp_owned_partials && !strcmp(argv[1],"audit-warp-partial-barrier"))
                    need(remove_barrier(PU_Info_tree_ptr(pu)),"missing warp tuple barrier target");
                WN *broadcast=find_collective(PU_Info_tree_ptr(pu),"__shfl_sync");
                if(broadcast && !strcmp(argv[1],"audit-broadcast-mask")) WN_const_val(WN_kid0(WN_kid0(broadcast)))=0xffff;
                if(broadcast && !strcmp(argv[1],"audit-broadcast-source")) WN_const_val(WN_kid0(WN_kid(broadcast,2)))=1;
                if(broadcast && !strcmp(argv[1],"audit-broadcast-width")) WN_const_val(WN_kid0(WN_kid(broadcast,3)))=16;
                if(broadcast && !strcmp(argv[1],"audit-broadcast-prototype"))
                    Set_TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(WN_st(broadcast)))+3],MTYPE_To_TY(MTYPE_U4));
                if(broadcast && !strcmp(argv[1],"audit-broadcast-missing")) remove_barrier(PU_Info_tree_ptr(pu),"__shfl_sync");
                WN *shuffle=find_collective(PU_Info_tree_ptr(pu),"__shfl_down_sync");
                if(!strcmp(argv[1],"audit-mask") && shuffle) WN_const_val(WN_kid0(WN_kid0(shuffle)))=0xffff;
                if(!strcmp(argv[1],"audit-collective-prototype") && shuffle)
                    Set_TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(WN_st(shuffle)))+1],MTYPE_To_TY(MTYPE_I4));
                if(!strcmp(argv[1],"audit-barrier")) remove_barrier(PU_Info_tree_ptr(pu));
                if(!strcmp(argv[1],"audit-attention-selection")) ST_tensor_bind_metadata(st,p.search_selected?"tensor.kernel.attention_search":"tensor.kernel.chunk_selection","stale_choice");
                if(!strcmp(argv[1],"audit-attention-accumulation")) ST_tensor_bind_metadata(st,"tensor.kernel.attention_accumulation","incorrect_ownership");
                if(!strcmp(argv[1],"audit-attention-layout")) ST_tensor_bind_metadata(st,"tensor.kernel.attention_layout","incorrect_tuple");
                if(!strcmp(argv[1],"audit-resource")) ST_tensor_bind_metadata(st,"tensor.kernel.shared_bytes","128");
                if(!strcmp(argv[1],"audit-shared")) {
                    const ST *local;ST_IDX local_index;
                    FOREACH_SYMBOL(CURRENT_SYMTAB,local,local_index) if(ST_device_symbol_kind(*local)==DEVICE_SYMBOL_SHARED) {
                        ARB_HANDLE bound=TY_arb(ST_type(local));Set_ARB_ubnd_val(bound,ARB_ubnd_val(bound)+1);break;
                    }
                }
                need(VHO_tensor_verify_kernel(p,i,stderr) && DSL_Gatekeeper_Verify_No_Native_PU(pu,stderr),"reopened kernel rejected");
                Json::Value kernel;kernel["name"]=ST_name(st);kernel["operation"]=metadata(st,"tensor.kernel.operation");
                kernel["source_operator"]=metadata(st,"tensor.kernel.source_operator");kernel["source_version"]=metadata(st,"tensor.kernel.source_version");kernel["source_node"]=metadata(st,"tensor.kernel.source_node");
                if(p.chunk_policy>=4) kernel["attention_statistics"]=metadata(st,"tensor.kernel.attention_statistics");
                kernel["grid_x"]=metadata(st,"tensor.kernel.grid_x");kernel["block_x"]=metadata(st,"tensor.kernel.block_x");kernel["arguments"]=Json::Value(Json::arrayValue);
                kernel["fused"]=metadata(st,"tensor.kernel.fused");kernel["split_stage"]=metadata(st,"tensor.kernel.split_stage");kernel["split_width"]=metadata(st,"tensor.kernel.split_width");
                if(p.search_selected) kernel["attention_search"]=metadata(st,"tensor.kernel.attention_search");
                if(p.auto_chunks) kernel["chunk_selection"]=metadata(st,"tensor.kernel.chunk_selection");
                if(p.register_latent) kernel["attention_accumulation"]=metadata(st,"tensor.kernel.attention_accumulation");
                if(p.fuse_attention) kernel["attention_layout"]=metadata(st,"tensor.kernel.attention_layout");
                if(p.cooperative) {kernel["mapping"]=metadata(st,"tensor.kernel.mapping");kernel["shared_bytes"]=metadata(st,"tensor.kernel.shared_bytes");}
                WN *entry=PU_Info_tree_ptr(pu);
                for(INT a=0;a<WN_num_formals(entry);++a) {
                    std::ostringstream argument_ordinal;argument_ordinal<<a;Json::Value arg,storage;
                    const char *keys[]={"storage","role","ordinal","elements","bytes","dtype","name"};
                    for(UINT32 k=0;k<7;++k) storage[keys[k]]=metadata(st,(std::string("tensor.argument.")+argument_ordinal.str()+"."+keys[k]).c_str());
                    UINT32 id=strtoul(storage["storage"].asCString(),NULL,10);
                    need(!storages.count(id) || storages[id]==storage,"inconsistent storage joins");storages[id]=storage;
                    arg["storage"]=storage["storage"];arg["dtype"]=storage["dtype"];kernel["arguments"].append(arg);
                }
                result["kernels"].append(kernel);pu=PU_Info_next(pu);
            }
            need(!pu,"unexpected extra device PU");result["storages"]=Json::Value(Json::arrayValue);
            if(p.search_selected) result["attention_search"]=search_evidence(p);
            if(p.auto_chunks) result["chunk_selection"]=selection_json(p);
            for(std::map<UINT32,Json::Value>::const_iterator i=storages.begin();i!=storages.end();++i) result["storages"].append(i->second);
            write_json(argv[4],result);printf("mapped_device_audit=PASS kernels=%u storages=%u\n",result["kernels"].size(),result["storages"].size());return 0;
        }
        fprintf(stderr,"usage: tensor_kernel_lower explore-attention input.json search.json --gpu-profile gpu.json --cost-profile costs.json --allow-reassociate [--search-options options.json] | select-attention search.json selected.json | prepare input.B recipe.json --verify level | high recipe.json high.before.B high.after.B buffers.json | optimize baseline.json selected.json --fuse [--split-width N --allow-reassociate] | cooperate fused.json cooperative.json --allow-reassociate | fuse-attention cooperative.json fused.json --allow-reassociate | auto-attention fused.json selected.json --gpu-profile profile.json --allow-reassociate [--chunk-policy coverage-v1|coverage-v2|coverage-v3|coverage-v4|coverage-v5|coverage-v6] | stream-attention fused.json register.json --split-width N --allow-reassociate | schedule-source selected.json optimized.cu | device recipe.json device.before.B device.after.B | device-explain recipe.json comments.before.B comments.after.B | audit device.after.B recipe.json launch.json\n");return 2;
    } catch(const std::exception &e) {fprintf(stderr,"tensor kernel driver: %s\n",e.what());return 1;}
}
