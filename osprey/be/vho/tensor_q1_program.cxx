/* Copyright (C) 2026 Open64 Project */
/* Q1 semantic program metadata and persistent ST operation joins. Uses the
 * repository's header-only RapidJSON, never JsonCpp or frontend APIs.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. */
#include "tensor_q1_program.h"
#include "tensor_workload.h"
#include "../../rapidjson/document.h"
#include "../../rapidjson/writer.h"
#include "../../rapidjson/stringbuffer.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
#include "pu_info.h"
#include "config_dsl.h"
#include "ir_bwrite.h"
#include "glob.h"
#include <unistd.h>
#include <sys/wait.h>
#include <stdexcept>
#include <sstream>
#include <set>
namespace {
/* Refuse a whole copied record before any compiler mutation. */
void need(bool valid) {if(!valid) throw std::runtime_error("q1_algorithm_metadata_invalid");}
/* No coercions or NUL-bearing strings enter native metadata joins. */
std::string string(const rapidjson::Value &v) {
    need(v.IsString() && v.GetStringLength()<=65536);
    std::string s(v.GetString(),v.GetStringLength());need(s.find('\0')==std::string::npos);return s;
}
/* Require exact unsigned scalars and avoid truncating version/identity fields. */
unsigned integer(const rapidjson::Value &v) {need(v.IsUint());return v.GetUint();}
/* Write exact source strings, including explicit empty unknown facts. */
void text(rapidjson::Writer<rapidjson::StringBuffer> &w,const std::string &s) {w.String(s.c_str(),s.size());}
/* Collect operation joins before annotation. LNO SIMD assumes expression
 * children on loop-body statements, so keep explanations at function entry. */
void operations(WN *node,const VHO_TENSOR_PROGRAM &p,std::set<unsigned> &ids) {
    if(WN_operator(node)==OPR_DO_LOOP) {
        const char *id=ST_tensor_metadata(WN_st_idx(WN_index(node)),"tensor.q1.operation");
        if(id) {char *end;unsigned long op=strtoul(id,&end,10);if(end!=id && !*end && id[0]>='0' && id[0]<='9' && op<p.operations.size()) ids.insert(op);}
    }
    if(WN_operator(node)==OPR_BLOCK) {
        for(WN *s=WN_first(node);s;s=WN_next(s)) operations(s,p,ids);
    } else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) operations(WN_kid(node,i),p,ids);
}
}
/* Persist all source semantics, including alias scale/evidence associations. */
std::string VHO_q1_program_encode(const VHO_TENSOR_PROGRAM &p) {
    rapidjson::StringBuffer b;rapidjson::Writer<rapidjson::StringBuffer> w(b);
    w.StartObject();w.Key("schema");w.String("open64.q1.algorithm.v1");w.Key("source_pu");text(w,p.source_pu);
    w.Key("source_file");text(w,p.source_file);w.Key("values");w.StartArray();
    for(unsigned i=0;i<p.values.size();++i) {
        const VHO_TENSOR_VALUE &v=p.values[i];w.StartArray();text(w,v.name);text(w,v.dtype);text(w,v.cache_trait);
        text(w,v.cache_encoding);text(w,v.cache_scale);text(w,v.cache_evidence);
        w.Uint(v.role);w.Uint(v.ordinal);w.Uint(v.storage);w.StartArray();
        for(unsigned d=0;d<v.shape.size();++d) w.Uint64(v.shape[d]);w.EndArray();w.EndArray();
    }
    w.EndArray();w.Key("operations");w.StartArray();
    for(unsigned i=0;i<p.operations.size();++i) {
        const VHO_TENSOR_OPERATION &o=p.operations[i];w.StartArray();text(w,o.name);w.Uint(o.version);w.Uint(o.source_node);w.Uint(o.source_line);
        w.StartArray();for(unsigned k=0;k<o.inputs.size();++k) w.Uint(o.inputs[k]);w.EndArray();w.Uint(o.output);w.StartObject();
        for(std::map<std::string,std::string>::const_iterator a=o.attributes.begin();a!=o.attributes.end();++a) {w.Key(a->first.c_str());text(w,a->second);}
        w.EndObject();w.EndArray();
    }
    w.EndArray();w.EndObject();return std::string(b.GetString(),b.GetSize());
}
/* Decode only the canonical emitted record, then repeat detached admission. */
bool VHO_q1_program_decode(const std::string &text,VHO_TENSOR_PROGRAM &out,std::string &why) {
    try {
        need(text.size()<=4*1024*1024 && text.find('\0')==std::string::npos);
        rapidjson::Document d;d.Parse<rapidjson::kParseValidateEncodingFlag|rapidjson::kParseIterativeFlag>(text.c_str());
        need(!d.HasParseError() && d.IsObject() && d.MemberCount()==5 && d.HasMember("schema") &&
            d.HasMember("source_pu") && d.HasMember("source_file") && d.HasMember("values") && d.HasMember("operations") &&
            string(d["schema"])=="open64.q1.algorithm.v1");
        VHO_TENSOR_PROGRAM p;p.source_pu=string(d["source_pu"]);p.source_file=string(d["source_file"]);
        const rapidjson::Value &values=d["values"],&operations=d["operations"];
        need(values.IsArray() && values.Size()<=4096 && operations.IsArray() && operations.Size()<=4096);
        for(unsigned i=0;i<values.Size();++i) {
            const rapidjson::Value &v=values[i];need(v.IsArray() && v.Size()==10 && v[9].IsArray() && v[9].Size()<=16);
            VHO_TENSOR_VALUE x;x.name=string(v[0]);x.dtype=string(v[1]);x.cache_trait=string(v[2]);x.cache_encoding=string(v[3]);
            x.cache_scale=string(v[4]);x.cache_evidence=string(v[5]);x.role=integer(v[6]);x.ordinal=integer(v[7]);x.storage=integer(v[8]);
            for(unsigned j=0;j<v[9].Size();++j) {need(v[9][j].IsUint64());x.shape.push_back(v[9][j].GetUint64());}p.values.push_back(x);
        }
        for(unsigned i=0;i<operations.Size();++i) {
            const rapidjson::Value &v=operations[i];need(v.IsArray() && v.Size()==7 && v[4].IsArray() && v[4].Size()<=3 && v[6].IsObject() && v[6].MemberCount()<=16);
            VHO_TENSOR_OPERATION o;o.name=string(v[0]);o.version=integer(v[1]);o.source_node=integer(v[2]);o.source_line=integer(v[3]);o.output=integer(v[5]);
            for(unsigned k=0;k<v[4].Size();++k) o.inputs.push_back(integer(v[4][k]));
            for(rapidjson::Value::ConstMemberIterator a=v[6].MemberBegin();a!=v[6].MemberEnd();++a) need(o.attributes.insert(std::make_pair(string(a->name),string(a->value))).second);
            p.operations.push_back(o);
        }
        need(VHO_tensor_validate_quantized_high(p,NULL) && VHO_q1_program_encode(p)==text);out=p;why.clear();return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* Require exact producer and persistent algorithm seals at each phase entry. */
bool VHO_q1_program_read(WN *entry,VHO_TENSOR_PROGRAM &out,std::string &why) {
    if(!entry || WN_operator(entry)!=OPR_FUNC_ENTRY) {why="q1_scope";return false;}
    ST_IDX st=WN_st_idx(entry);const char *schema=ST_tensor_metadata(st,"tensor.high.schema");
    const char *wire=ST_tensor_metadata(st,"tensor.q1.algorithm"),*key=ST_tensor_metadata(st,"tensor.high.recipe_key");
    if(!schema || strcmp(schema,"quantized-common-high-v1") || !wire || !key) {why="q1_no_persistent_binding";return false;}
    VHO_TENSOR_PROGRAM p;if(!VHO_q1_program_decode(wire,p,why)) return false;
    if(VHO_tensor_quantized_high_key(p)!=key) {why="q1_stale_persistent_binding";return false;}out=p;return true;
}
/* Explanation restoration is an explicit annotation phase, not fact proof. */
unsigned VHO_q1_restore_comments(WN *entry,const VHO_TENSOR_PROGRAM &p) {
    std::set<unsigned> ids;operations(entry,p,ids);WN *body=WN_func_body(entry);
    WN *first=WN_first(body);
    WN *workload=WN_CreateComment(VHO_tensor_workload_explanation(p).c_str());
    WN_Set_Linenum(workload,WN_Get_Linenum(entry));WN_INSERT_BlockBefore(body,first,workload);
    WN *contract=WN_CreateComment("Q1 semantic contract after PREOPT: exact byte sign/zero extension and promoted arithmetic; ordered F32 reductions. Gather bounds, checked index arithmetic, integral byte conversion and unique fresh updates guard unsafe operations. Status 0 succeeds, 1 refuses; discard partial scratch/results on refusal. Reshape aliases caller storage. Cache/scale versions and lifetimes are invocation-local; no serving publication, cross-head reuse or GPU permission is inferred.");
    WN_Set_Linenum(contract,WN_Get_Linenum(entry));WN_INSERT_BlockBefore(body,first,contract);
    for(std::set<unsigned>::const_iterator i=ids.begin();i!=ids.end();++i) {
        std::ostringstream message;message<<"Q1 PREOPT operation "<<*i<<": "<<p.operations[*i].name
            <<"; source-stage join retained on a surviving loop. Fresh native facts replace pre-PREOPT facts; "
            <<"guarded memory and strict F32 contract; no search, GPU mapping or serving publication is implied.";
        WN *comment=WN_CreateComment(message.str().c_str());WN_Set_Linenum(comment,WN_Get_Linenum(entry));
        WN_INSERT_BlockBefore(body,first,comment);
    }
    return ids.size();
}

/* Keep normal writer state in the parent. Snapshots immediately bracket the
 * owning phase; dependence graphs are absent at these early High boundaries. */
bool VHO_q1_checkpoint(WN *tree,const char *stage,bool standard) {
    if(!VHO_DSL_Q1_Checkpoint) return true;
    std::string path=std::string(VHO_DSL_Q1_Checkpoint)+"."+stage+".B",temp=path+".tmp";
    if(!Current_PU_Info || access(path.c_str(),F_OK)==0 || access(temp.c_str(),F_OK)==0) return false;
    fflush(NULL);pid_t child=fork();if(child<0) return false;
    if(!child) {
        PU_Info image=*Current_PU_Info;PU_Info_next(&image)=NULL;PU_Info_child(&image)=NULL;
        Set_PU_Info_tree_ptr(&image,tree);Set_PU_Info_state(&image,WT_TREE,Subsect_InMem);
        Set_PU_Info_depgraph_ptr(&image,NULL);Set_PU_Info_state(&image,WT_DEPGRAPH,Subsect_Missing);
        Output_File *output=standard?Open_Standard_Only_Output_Info((char *)temp.c_str(),&image):Open_Output_Info((char *)temp.c_str());
        if(!output) _exit(1);Write_PU_Info(&image);Write_Global_Info(&image);Close_Output_Info();
        if(link(temp.c_str(),path.c_str())) _exit(1);_exit(unlink(temp.c_str())?1:0);
    }
    int status=0;return waitpid(child,&status,0)==child && WIFEXITED(status) && WEXITSTATUS(status)==0;
}
