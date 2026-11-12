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
#include "tensor_kernel_lower.h"
#include "tensor_schedule_source.h"

BOOL Run_vsaopt=FALSE;
INT8 Debug_Level=0;
/* No compiler-owned signal cleanup is needed by this process driver. */
void Signal_Cleanup(INT) {}
/* Native driver diagnostics use no target-specific formatting extension. */
const char *Host_Format_Parm(INT,MEM_PTR) {return "tensor-kernel";}

namespace {
/* Reject malformed handoffs/options before publishing any artifact. */
void need(bool ok,const char *message) {if(!ok) throw std::runtime_error(message);}
/* Initialize the established n64 compiler context for source or device mode. */
void initialize(bool device) {
    MEM_Initialize();Set_Error_Tables(Phases,host_errlist);Init_Error_Handler(10);Set_Error_File(NULL);Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure();Init_Controls_Tbl();ABI_Name="n64";Configure();IR_reader_init();WHIRL_Mldid_Mstid_On=TRUE;
    OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
    if(device) {Initialize_Symbol_Tables(TRUE);DST_Init(NULL,0);need(DSL_Builder_Begin_Program(),"cannot begin device program");}
}
/* The detached recipe uses a deterministic logical operation identifier. */
std::string kernel_name(size_t ordinal);
/* Serialize copied recipe facts only; generated arithmetic remains native WHIRL. */
Json::Value encode(const VHO_TENSOR_PROGRAM &p) {
    Json::Value j;j["schema"]="open64.tensor-kernel.v1";j["source_pu"]=p.source_pu;j["source_file"]=p.source_file;
    j["opt_level"]=p.opt_level;j["threads"]=p.threads;j["values"]=Json::Value(Json::arrayValue);j["operations"]=Json::Value(Json::arrayValue);
    if(p.fuse) {
        j["schema"]="open64.tensor-kernel.v2";j["fuse"]=true;j["split_width"]=p.split_width;
        j["reassociation"]=p.split_width?"allow":"forbid";
    }
    for(size_t i=0;i<p.values.size();++i) {
        const VHO_TENSOR_VALUE &v=p.values[i];Json::Value x;x["name"]=v.name;x["dtype"]=v.dtype;x["role"]=v.role;x["ordinal"]=v.ordinal;x["storage"]=v.storage;x["shape"]=Json::Value(Json::arrayValue);
        for(size_t a=0;a<v.shape.size();++a) x["shape"].append(Json::UInt64(v.shape[a]));j["values"].append(x);
    }
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
VHO_TENSOR_PROGRAM decode(const char *path) {
    std::ifstream in(path);Json::Value j;Json::CharReaderBuilder reader;std::string errors;
    reader["allowComments"]=false;reader["collectComments"]=false;reader["strictRoot"]=true;
    reader["failIfExtra"]=true;reader["rejectDupKeys"]=true;
    need(in.good() && Json::parseFromStream(reader,in,&j,&errors),"invalid recipe JSON");
    bool optimized=j["schema"].asString()=="open64.tensor-kernel.v2";
    need(j.isObject() && j.size()==(optimized?11:8) && j["recipe_fingerprint"].isString() && (optimized || j["schema"].asString()=="open64.tensor-kernel.v1") && j["source_pu"].isString() && j["source_file"].isString() &&
        j["opt_level"].isUInt() && j["threads"].isUInt() && j["values"].isArray() && j["operations"].isArray(),"invalid recipe header");
    need(j["values"].size()<=4096 && j["operations"].size()<=4096,"oversized recipe");
    VHO_TENSOR_PROGRAM p;p.source_pu=j["source_pu"].asString();p.source_file=j["source_file"].asString();p.opt_level=j["opt_level"].asUInt();p.threads=j["threads"].asUInt();
    if(optimized) {
        need(j["fuse"].isBool() && j["fuse"].asBool() && j["split_width"].isUInt() && j["reassociation"].isString(),"invalid optimization policy");
        p.fuse=true;p.split_width=j["split_width"].asUInt();
        need(j["reassociation"].asString()==(p.split_width?"allow":"forbid"),"reassociation policy mismatch");
    }
    for(Json::ArrayIndex i=0;i<j["values"].size();++i) {
        const Json::Value &x=j["values"][i];need(x.isObject() && x.size()==6 && x["name"].isString() && x["dtype"].isString() && x["role"].isUInt() && x["ordinal"].isUInt() && x["storage"].isUInt() && x["shape"].isArray(),"invalid value record");
        VHO_TENSOR_VALUE v;v.name=x["name"].asString();v.dtype=x["dtype"].asString();v.role=x["role"].asUInt();v.ordinal=x["ordinal"].asUInt();v.storage=x["storage"].asUInt();
        for(Json::ArrayIndex a=0;a<x["shape"].size();++a) {need(x["shape"][a].isUInt64(),"invalid extent");v.shape.push_back(x["shape"][a].asUInt64());}p.values.push_back(v);
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
} // namespace

/* The executable establishes exactly one source PU or fresh device translation
 * unit. The VHO service never expands its compilation scope or writes files. */
int main(int argc,char **argv) {
    try {
        if(argc==4 && !strcmp(argv[1],"schedule-source")) {
            VHO_TENSOR_PROGRAM p=decode(argv[2]);VHO_TENSOR_SCHEDULE plan;
            need(VHO_tensor_schedule(p,&plan,stderr),"schedule rejected");
            write_source(argv[3],W2C_tensor_schedule_source(plan,encode(p)["recipe_fingerprint"].asString()));
            printf("schedule_source=PASS stages=%lu\n",(unsigned long)plan.kernels.size());return 0;
        }
        if((argc==5 || argc==8) && !strcmp(argv[1],"optimize")) {
            need(!strcmp(argv[4],"--fuse"),"expected --fuse");VHO_TENSOR_PROGRAM p=decode(argv[2]);p.fuse=true;
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
        if(argc==5 && !strcmp(argv[1],"device")) {
            need(access(argv[3],F_OK)!=0 && access(argv[4],F_OK)!=0,"checkpoint destination exists");
            VHO_TENSOR_PROGRAM p=decode(argv[2]);VHO_TENSOR_SCHEDULE plan;need(VHO_tensor_schedule(p,&plan,stderr),"schedule rejected");
            initialize(true);std::vector<PU_Info *> pus;std::vector<UINT32> operations;
            for(size_t k=0;k<plan.kernels.size();++k) {UINT32 i=plan.kernels[k].id;
                PU_Info *pu=DSL_Builder_Create_Minimal_PU(kernel_symbol(p,i).c_str());need(pu && DSL_Builder_Select_PU(pu),"cannot create device PU");
                ST &function=St_Table[PU_Info_proc_sym(pu)];Set_ST_device_symbol_kind(function,DEVICE_SYMBOL_KERNEL);Set_ST_export(function,EXPORT_PREEMPTIBLE);pus.push_back(pu);operations.push_back(i);
            }
            need(!pus.empty(),"no executable kernels");checkpoint(pus,argv[3]);
            for(size_t i=0;i<pus.size();++i) {
                need(DSL_Builder_Select_PU(pus[i]),"cannot select device PU");UINT32 file=DSL_Builder_Register_Source_File(pus[i],p.source_file.c_str());
                const VHO_TENSOR_OPERATION &op=p.operations[plan.kernels[i].root];
                USRCPOS source;USRCPOS_clear(source);USRCPOS_filenum(source)=file;USRCPOS_linenum(source)=op.source_line?op.source_line:1;
                UINT32 types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);
                need(!VHO_tensor_materialize_kernel(p,operations[i],0,NULL),"zero source accepted");
                OPT_Fast_Math=TRUE;BOOL unsafe=VHO_tensor_materialize_kernel(p,operations[i],USRCPOS_srcpos(source),NULL);OPT_Fast_Math=FALSE;
                need(!unsafe && types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),"failed preflight mutated tables");
                need(VHO_tensor_materialize_kernel(p,operations[i],USRCPOS_srcpos(source),stderr),"kernel materialization failed");
                need(VHO_tensor_verify_kernel(p,operations[i],stderr) && !VHO_tensor_materialize_kernel(p,operations[i],USRCPOS_srcpos(source),NULL),"device verification/nonempty admission failed");
                need(DSL_Gatekeeper_Verify_No_Native_PU(pus[i],stderr),"unlowered tensor remains");
                printf("kernel=%s operation=%s elements=%llu threads=%u stage=%u\n",kernel_symbol(p,operations[i]).c_str(),op.name.c_str(),(unsigned long long)plan.kernels[i].elements,p.threads,plan.kernels[i].stage);
            }
            checkpoint(pus,argv[4]);return 0;
        }
        if(argc==5 && (!strcmp(argv[1],"audit") || !strcmp(argv[1],"audit-mutate") || !strcmp(argv[1],"audit-binding") || !strcmp(argv[1],"audit-prototype") || !strcmp(argv[1],"audit-policy"))) {
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
                if(!strcmp(argv[1],"audit-prototype")) {
                    TY_IDX wrong=TY_pointed(ST_type(WN_st(WN_formal(PU_Info_tree_ptr(pu),0))));
                    Set_TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(&St_Table[st]))],wrong);
                }
                need(VHO_tensor_verify_kernel(p,i,stderr) && DSL_Gatekeeper_Verify_No_Native_PU(pu,stderr),"reopened kernel rejected");
                Json::Value kernel;kernel["name"]=ST_name(st);kernel["operation"]=metadata(st,"tensor.kernel.operation");
                kernel["source_operator"]=metadata(st,"tensor.kernel.source_operator");kernel["source_version"]=metadata(st,"tensor.kernel.source_version");kernel["source_node"]=metadata(st,"tensor.kernel.source_node");
                kernel["grid_x"]=metadata(st,"tensor.kernel.grid_x");kernel["block_x"]=metadata(st,"tensor.kernel.block_x");kernel["arguments"]=Json::Value(Json::arrayValue);
                kernel["fused"]=metadata(st,"tensor.kernel.fused");kernel["split_stage"]=metadata(st,"tensor.kernel.split_stage");kernel["split_width"]=metadata(st,"tensor.kernel.split_width");
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
            for(std::map<UINT32,Json::Value>::const_iterator i=storages.begin();i!=storages.end();++i) result["storages"].append(i->second);
            write_json(argv[4],result);printf("mapped_device_audit=PASS kernels=%u storages=%u\n",result["kernels"].size(),result["storages"].size());return 0;
        }
        fprintf(stderr,"usage: tensor_kernel_lower prepare input.B recipe.json --verify level | optimize baseline.json selected.json --fuse [--split-width N --allow-reassociate] | schedule-source selected.json optimized.cu | device recipe.json device.before.B device.after.B | audit device.after.B recipe.json launch.json\n");return 2;
    } catch(const std::exception &e) {fprintf(stderr,"tensor kernel driver: %s\n",e.what());return 1;}
}
