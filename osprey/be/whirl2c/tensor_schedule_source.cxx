/* Copyright (C) 2026 Open64 Project */
/* Read-only tensor-level CUDA presentation of the VHO-selected stage graph.
 * Design: doc/AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.2.md. This printer never
 * chooses fusion, cuts, chunk widths, GPU mapping, or an attention algorithm. */
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include "tensor_kernel_lower.h"
#include "tensor_schedule_source.h"

namespace {
/* Refuse a malformed presentation rather than publishing partial source. */
void require(bool ok,const char *why) { if(!ok) throw std::runtime_error(why); }
/* Make names readable without accepting source text as C++ syntax. */
std::string safe(const std::string &name) {
    std::string result;
    for(size_t i=0;i<name.size();++i) {
        if(std::isalnum((unsigned char)name[i])) result+=name[i];
        else if(!result.empty() && result[result.size()-1]!='_') result+='_';
    }
    if(!result.empty() && result[result.size()-1]=='_') result.resize(result.size()-1);
    return result.empty()?"unnamed":result;
}
/* Keep every source value uniquely named even when source spellings collide. */
std::string name(const VHO_TENSOR_PROGRAM &p,UINT32 id) {
    std::ostringstream out;out<<"value_"<<safe(p.values[id].name)<<"_v"<<id;return out.str();
}
/* Print exact static logical dimensions from the admitted source descriptor. */
std::string type(const VHO_TENSOR_VALUE &v) {
    std::ostringstream out;out<<"TENSOR<"<<(v.dtype=="bool"?"bool":"float");
    for(size_t i=0;i<v.shape.size();++i) out<<","<<v.shape[i];out<<">";return out.str();
}
/* Avoid ambiguity between pointer parameters, lazy views, and allocated arrays. */
std::string pointer(const VHO_TENSOR_PROGRAM &p,UINT32 id) { std::ostringstream out;out<<"data_"<<safe(p.values[id].name)<<"_v"<<id;return out.str(); }

class Printer {
    const VHO_TENSOR_SCHEDULE &_plan;
    const VHO_TENSOR_PROGRAM &_p;
    std::map<UINT32,UINT32> _producer;
    std::set<UINT32> _bound,_printed;
    const std::vector<VHO_TENSOR_EXPLANATION> &_comments;
    UINT32 _stage;
    std::ostringstream _out;
public:
    /* Borrow detached facts only; no compiler table or live PU is consulted. */
    Printer(const VHO_TENSOR_SCHEDULE &plan,const std::vector<VHO_TENSOR_EXPLANATION> &comments):_plan(plan),_p(plan.bindings),_comments(comments),_stage(0) {
        for(UINT32 i=0;i<_p.operations.size();++i) _producer[_p.operations[i].output]=i;
    }
    /* Print the exact shared WHIRL COMMENT text, without reconstructing policy. */
    void Explain(UINT32 kind,UINT32 value=0) {
        for(size_t i=0;i<_comments.size();++i) {
            const VHO_TENSOR_EXPLANATION &record=_comments[i];
            if(record.kind==kind && record.value==value &&
               ((kind==VHO_TENSOR_EXPLAIN_OVERVIEW || kind==VHO_TENSOR_EXPLAIN_STORAGE) || record.kernel_id==_stage)) {
                _out<<"/* "<<record.text<<" */\n";return;
            }
        }
        require(false,"missing selected explanation record");
    }
    /* Show each selected fused producer once as a named nonallocating view. */
    std::string Value(UINT32 id) {
        if(_bound.count(id) || _printed.count(id)) return name(_p,id);
        require(_producer.count(id),"missing selected producer");
        const VHO_TENSOR_OPERATION &op=_p.operations[_producer[id]];std::vector<std::string> args;
        if(op.name!="common.fill_like") for(size_t i=0;i<op.inputs.size();++i) args.push_back(Value(op.inputs[i]));
        std::string call;
        if(op.name=="copy" || op.name=="common.reshape") call="reshape_view<"+type(_p.values[id])+">";
        else if(op.name=="common.transpose") {
            std::map<std::string,std::string>::const_iterator perm=op.attributes.find("attr.permutation");
            require(perm!=op.attributes.end(),"missing selected permutation");call="transpose_view<"+type(_p.values[id])+","+perm->second+">";
        } else if(op.name=="common.fill_like") {
            call="constant<"+type(_p.values[id])+">";args.push_back("0x"+op.attributes.find("attr.value_bits")->second+"u");
        } else {
            const char *operators[]={"common.add","common.sub","common.mul","common.div","common.greater","common.exp","common.log","common.isfinite","common.logical_not","common.select"};
            const char *spellings[]={"add","subtract","multiply","divide","greater","exponential","logarithm","is_finite","logical_not","choose"};
            for(UINT32 i=0;i<10;++i) if(op.name==operators[i]) call=std::string(spellings[i])+"<"+type(_p.values[id])+">";
        }
        require(!call.empty(),"presentation crossed a selected materialization boundary");
        Explain(VHO_TENSOR_EXPLAIN_PRODUCER,id);
        _out<<"    // Fused source node "<<op.source_node<<": "<<op.name<<".v"<<op.version<<"\n";
        _out<<"    const auto "<<name(_p,id)<<" = "<<call<<"(";
        for(size_t i=0;i<args.size();++i) {if(i) _out<<", ";_out<<args[i];}
        _out<<");";
        if(op.name=="common.fill_like") {
            UINT32 bits=strtoul(op.attributes.find("attr.value_bits")->second.c_str(),NULL,16);float value;memcpy(&value,&bits,4);
            _out<<" // F32 value: "<<std::setprecision(9)<<value;
        }
        _out<<"\n";_printed.insert(id);return name(_p,id);
    }
    /* Print one authoritative selected stage; serial helpers expose semantics
     * but do not purport to execute a mapped CUDA kernel launch. */
    void Stage(const VHO_TENSOR_KERNEL &k,UINT32 ordinal) {
        require(k.root<_p.operations.size() && k.output<_p.values.size(),"invalid selected stage");
        const VHO_TENSOR_OPERATION &op=_p.operations[k.root];
        _stage=k.id;
        _bound.clear();_printed.clear();_bound.insert(k.inputs.begin(),k.inputs.end());
        Explain(VHO_TENSOR_EXPLAIN_STAGE);
        _out<<"\n  // Stage "<<ordinal+1<<": "<<(k.stage==1?"PARALLEL CHUNK PARTIALS":k.stage==2?(k.mapping?"WARP CHUNK MERGE":"ORDERED CHUNK MERGE"):"FUSED OUTPUT")<<"\n";
        _out<<"  // kernel_id="<<k.id<<"; source node="<<op.source_node<<"; "<<op.name<<".v"<<op.version<<"\n";
        _out<<"  // Selected independent work items="<<k.elements;
        if(k.stage) _out<<"; chunk_width="<<k.split_width<<"; chunks="<<k.chunks<<"; scratch=chunk_major";
        _out<<"\n  {\n";
        std::vector<std::string> args;
        if(k.stage==2) args.push_back(name(_p,k.inputs[0]));
        else for(size_t i=0;i<op.inputs.size();++i) args.push_back(Value(op.inputs[i]));
        std::ostringstream call;std::string root_type=type(_p.values[op.output]);
        if(k.stage==2) call<<(k.mapping?"warp_merge_to<":"merge_to<")<<root_type<<","<<(op.name=="common.reduce_max"?"true":"false")<<","<<k.chunks<<">";
        else if(op.name=="common.matmul") {
            call<<(k.mapping==2?"staged_matmul_to<":k.stage==1?"split_matmul_to<":"matmul_to<")<<root_type;
            if(k.stage==1 || k.mapping==2) call<<","<<(k.stage==1?k.split_width:_p.values[op.inputs[0]].shape.back())<<","<<(k.stage==1?k.chunks:1);call<<">";
        } else if(op.name=="common.reduce_max" || op.name=="common.reduce_sum") {
            call<<(k.mapping?"warp_reduce_to<":k.stage==1?"split_reduce_to<":"reduce_to<")<<root_type<<","<<(op.name=="common.reduce_max"?"true":"false");
            if(k.stage==1 || k.mapping) call<<","<<(k.stage==1?k.split_width:_p.values[op.inputs[0]].shape.back())<<","<<(k.stage==1?k.chunks:1);call<<">";
        } else {require(op.name=="copy","unsupported selected root");call<<"materialize_to<"<<root_type<<">";}
        _out<<"    "<<call.str()<<"("<<pointer(_p,k.output);
        if(_p.values[k.output].role==0) _out<<".data";
        for(size_t i=0;i<args.size();++i) _out<<", "<<args[i];_out<<");\n  }\n";
    }
    /* Preserve the source interface and allocate only selected stage storage. */
    std::string Print(const std::string &seal) {
        require(_p.fuse && !_plan.kernels.empty(),"optimized selected schedule required");
        require(!seal.empty() && seal.find_first_not_of("0123456789")==std::string::npos,"invalid recipe fingerprint");
        Explain(VHO_TENSOR_EXPLAIN_OVERVIEW);
        _out<<"// Generated selected-schedule review CUDA, BEFORE low-level WHIRL expansion.\n";
        _out<<"// recipe_fingerprint="<<seal<<"; stages="<<_plan.kernels.size()<<"; reassociation="<<(_p.split_width?"allow":"forbid")<<"\n";
        _out<<"// Named views below are lazy and allocate no intermediate tensor.\n";
        _out<<"// Helpers execute stages serially for checking; actual GPU kernels use the same selected plan.\n";
        _out<<"#include \"open64_tensor_schedule.cuh\"\nusing namespace open64_schedule;\n\n";
        _out<<"extern \"C\" O64_HD void open64_optimized_"<<safe(_p.source_pu)<<"_"<<seal<<"(\n";
        bool first=true;
        for(UINT32 role=1;role<=2;++role) for(UINT32 i=0;i<_p.values.size();++i) if(_p.values[i].role==role) {
            if(!first) _out<<",\n";first=false;
            _out<<"  "<<(role==1?"const ":"")<<(_p.values[i].dtype=="bool"?"bool":"float")<<" *"<<pointer(_p,i)<<" /* "<<name(_p,i)<<" */";
        }
        _out<<") {\n";
        for(UINT32 i=0;i<_p.values.size();++i) if(_p.values[i].role==1)
            _out<<"  const auto "<<name(_p,i)<<" = tensor_view<"<<type(_p.values[i])<<">("<<pointer(_p,i)<<");\n";
        std::set<UINT32> allocated;
        for(size_t k=0;k<_plan.kernels.size();++k) {
            UINT32 i=_plan.kernels[k].output;
            if(_p.values[i].role==0 && allocated.insert(i).second) {
                Explain(VHO_TENSOR_EXPLAIN_STORAGE,i);
                _out<<"  "<<type(_p.values[i])<<" "<<pointer(_p,i)<<";\n";
                _out<<"  const auto "<<name(_p,i)<<" = tensor_view<"<<type(_p.values[i])<<">("<<pointer(_p,i)<<".data);\n";
            }
        }
        for(UINT32 i=0;i<_plan.kernels.size();++i) Stage(_plan.kernels[i],i);
        _out<<"}\n";return _out.str();
    }
};
} // namespace

/* The driver validates the recipe and obtains the plan from VHO first. This
 * read-only printer consumes those exact cuts and chunk sizes without policy. */
std::string W2C_tensor_schedule_source(const VHO_TENSOR_SCHEDULE &plan,const std::vector<VHO_TENSOR_EXPLANATION> &comments,const std::string &seal) {
    return Printer(plan,comments).Print(seal);
}
