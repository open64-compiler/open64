/* Copyright (C) 2026 Open64 Project */
/* VHO-owned descriptions of an already selected PU-local tensor schedule.
 * Design: doc/AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.2.md. These detached
 * strings feed real WHIRL COMMENT nodes and the readable source view equally;
 * no optimization choice, source emission, or PU switching occurs here. */
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include "tensor_kernel_explain.h"
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
/* Format detached sizes without depending on mapped compiler tables. */
std::string number(UINT64 value) {std::ostringstream out;out<<value;return out.str();}

class ExplanationBuilder {
    const VHO_TENSOR_SCHEDULE &_plan;
    const VHO_TENSOR_PROGRAM &_p;
    std::map<UINT32,UINT32> _producer,_owners,_first_fused_stage;
    std::set<UINT32> _bound,_printed;
    UINT32 _stage,_kernel,_kind,_value;
    std::vector<VHO_TENSOR_EXPLANATION> _records;
public:
    /* Borrow detached facts without consulting another PU or compiler table. */
    explicit ExplanationBuilder(const VHO_TENSOR_SCHEDULE &plan):_plan(plan),_p(plan.bindings),_stage(0),_kernel(0),_kind(0),_value(0) {
        for(UINT32 i=0;i<_p.operations.size();++i) _producer[_p.operations[i].output]=i;
        for(UINT32 i=0;i<plan.kernels.size();++i) _owners[plan.kernels[i].output]=i;
    }
    /* Store the exact text that WN_CreateComment and the view printer consume. */
    void Explain(const std::string &title,const std::string &text,const char * = "") {
        VHO_TENSOR_EXPLANATION record;record.kernel_id=_kernel;record.kind=_kind;record.value=_value;
        std::ostringstream out;out<<"OPEN64 GENERATED EXPLANATION: "<<title<<"\n";
        std::istringstream lines(text);std::string line;
        while(std::getline(lines,line)) out<<" * "<<line<<"\n";
        record.text=out.str();_records.push_back(record);
    }
    /* Recognize exact source constants only to describe an existing guard. */
    bool IsFill(UINT32 value,const char *bits) const {
        std::map<UINT32,UINT32>::const_iterator found=_producer.find(value);
        if(found==_producer.end()) return false;
        const VHO_TENSOR_OPERATION &op=_p.operations[found->second];
        return op.name=="common.fill_like" && op.attributes.find("attr.value_bits")->second==bits;
    }
    /* Describe the actual view semantics, not a guessed source-level role. */
    void ExplainValue(UINT32 id,const VHO_TENSOR_OPERATION &op) {
        std::ostringstream text;text<<name(_p,id)<<" has logical type "<<type(_p.values[id])<<".\n";
        if(op.name=="common.reshape" || op.name=="copy")
            text<<"Lazy dense shape view of "<<name(_p,op.inputs[0])<<"; preserve element order without a copy.\n";
        else if(op.name=="common.transpose")
            text<<"Lazy axis permutation ["<<op.attributes.find("attr.permutation")->second<<"] of "<<name(_p,op.inputs[0])<<".\nRead source coordinates on demand; no transposed buffer is allocated.\n";
        else if(op.name=="common.fill_like") {
            const std::string &bits=op.attributes.find("attr.value_bits")->second;
            UINT32 word=strtoul(bits.c_str(),NULL,16);float value;memcpy(&value,&word,4);
            text<<"Virtual broadcast of 0x"<<bits<<" (F32 "<<std::setprecision(9)<<value<<"); no filled array or input-value read.\n";
        } else if(op.name=="common.select") {
            text<<"Choose "<<name(_p,op.inputs[1])<<" when "<<name(_p,op.inputs[0])<<" is true, otherwise "<<name(_p,op.inputs[2])<<".\nThis preserves the source mask/guard and its broadcast shape.\n";
            if(_producer.count(op.inputs[0])) {
                const VHO_TENSOR_OPERATION &condition=_p.operations[_producer[op.inputs[0]]];
                if(condition.name=="common.isfinite" && condition.inputs[0]==op.inputs[1] && IsFill(op.inputs[2],"00000000"))
                    text<<"Finite-value guard: keep the input when finite, otherwise use the source's zero fallback.\n";
                if(condition.name=="common.greater" && condition.inputs[0]==op.inputs[1] && IsFill(condition.inputs[1],"00000000") && IsFill(op.inputs[2],"3f800000"))
                    text<<"Safe-divisor guard: keep a positive input, otherwise use the source's F32 one fallback.\n";
            }
        } else if(op.name=="common.isfinite")
            text<<"Test whether "<<name(_p,op.inputs[0])<<" is finite; the consuming choose decides the fallback.\n";
        else if(op.name=="common.logical_not")
            text<<"Invert the boolean predicate in "<<name(_p,op.inputs[0])<<" for its consuming operation.\n";
        else if(op.name=="common.exp" || op.name=="common.log")
            text<<"Evaluate "<<(op.name=="common.exp"?"expf":"logf")<<" of "<<name(_p,op.inputs[0])<<" on demand, retaining F32 results.\n";
        else {
            const char *symbol=op.name=="common.add"?"+":op.name=="common.sub"?"-":op.name=="common.mul"?"*":op.name=="common.div"?"/":">";
            text<<"Evaluate "<<name(_p,op.inputs[0])<<" "<<symbol<<" "<<name(_p,op.inputs[1])<<" with right-aligned broadcasting.\n";
            if(op.name=="common.div") text<<"The divisor is the selected expression above; no new normalization is invented here.\n";
        }
        text<<"Fused into this stage: this logical value is a lazy view, not a materialized tensor.\n";
        if(_first_fused_stage.count(id))
            text<<"Recomputed here; this source value was also expanded in stage "<<_first_fused_stage[id]<<".\nReuse of the expression does not allocate an intermediate or share results across output threads.\n";
        else _first_fused_stage[id]=_stage;
        Explain("FUSED PRODUCER",text.str(),"    ");
    }
    /* Join boundary operands to their selected producers. Final merged values
     * are common dependencies of every chunk, including normalization inputs. */
    void ExplainDependencies(const VHO_TENSOR_KERNEL &k,std::ostringstream &text) {
        for(size_t i=0;i<k.inputs.size();++i) {
            UINT32 value=k.inputs[i];text<<"Boundary input "<<name(_p,value)<<": "<<type(_p.values[value]);
            if(!_owners.count(value)) text<<", runtime input or dense alias (may include an ABI-only input).\n";
            else {
                UINT32 owner=_owners[value];const VHO_TENSOR_KERNEL &producer=_plan.kernels[owner];
                text<<", produced by stage "<<owner+1<<".\n";
                if(producer.stage==2) {
                    const std::string &operation=_p.operations[producer.root].name;
                    text<<"  This is a completed global "<<(operation=="common.reduce_max"?"maximum":"sum")<<" from all "<<producer.chunks<<" chunks.\n";
                    if(k.stage==1) text<<"  Every current chunk uses this same finalized value; no chunk-local replacement is used.\n";
                }
            }
        }
    }
    /* Render formulas, tails, work ownership and numerical order directly
     * from the supplied plan; these facts do not select a new schedule. */
    void ExplainStage(const VHO_TENSOR_KERNEL &k) {
        const VHO_TENSOR_OPERATION &op=_p.operations[k.root];
        UINT64 outputs=VHO_tensor_elements(_p.values[op.output]);std::ostringstream text;
        text<<"Selected result: "<<name(_p,k.output)<<"; "<<type(_p.values[k.output])<<"; "<<k.elements<<" work items.\n";
        if(_p.opt_level==3) text<<"Mapped GPU launch: "<<(k.elements+_p.threads-1)/_p.threads<<" blocks of "<<_p.threads<<" threads, with excess work items guarded.\n";
        else text<<"Single-thread semantic lowering: visit outputs in a serial loop.\n";
        if(k.stage==2) {
            bool maximum=op.name=="common.reduce_max";
            text<<"For each output i in [0,"<<outputs<<"), start at "<<(maximum?"-infinity":"+0")<<" and visit c=0.."<<k.chunks-1<<" in order.\n";
            text<<"out[i] = "<<(maximum?"max":"sum")<<"_c partial[c*"<<outputs<<"+i]. The outer merge remains serial per output.\n";
            text<<"The merge "<<(maximum?"compares":"adds")<<" already-computed partials; it performs no re-normalization, averaging, or exponential rescaling.\n";
            if(maximum) text<<"Maximum uses the selected NaN-propagating rule and the -infinity neutral value.\n";
            else text<<"Chunk grouping changes F32 addition order relative to an unsplit reduction; explicit reassociation permission is required.\n";
        } else if(op.name=="common.matmul" || op.name=="common.reduce_max" || op.name=="common.reduce_sum") {
            UINT64 width=_p.values[op.inputs[0]].shape.back();
            bool maximum=op.name=="common.reduce_max",matmul=op.name=="common.matmul";
            if(matmul) text<<"Contract "<<name(_p,op.inputs[0])<<" with "<<name(_p,op.inputs[1])<<" over K="<<width<<" using source batch broadcasting.\n";
            else text<<"Reduce the final axis of "<<name(_p,op.inputs[0])<<", width K="<<width<<", with "<<(maximum?"maximum":"addition")<<".\n";
            if(k.stage==1) {
                UINT64 tail=width-UINT64(k.chunks-1)*k.split_width;
                text<<"Chunk c covers [c*"<<k.split_width<<", min((c+1)*"<<k.split_width<<","<<width<<")).\n";
                text<<"There are "<<k.chunks<<" chunks; the final chunk starts at "<<UINT64(k.chunks-1)*k.split_width<<" and contains "<<tail<<" terms.\n";
                text<<"Let N="<<outputs<<". work_item=c*N+i owns exactly partial[c*N+i] in chunk-major scratch.\n";
                text<<"Each partial starts at "<<(maximum?"-infinity":"+0")<<" and "<<(maximum?"compares source values":matmul?"adds source products":"adds source values")<<" in ascending source-index order.\n";
                text<<"The inner loop has at most "<<k.split_width<<" terms and remains serial in each work item.\n";
                text<<"Distinct work items write distinct partial slots. The ordered merge stage consumes completed scratch on the same stream.\n";
                text<<"This plan requires no intra-kernel cross-thread reads, atomics, barriers, or warp/CTA tree reduction.\n";
            } else {
                text<<"Each independent output traverses all "<<width<<" terms in ascending order, starting at "<<(maximum?"-infinity":"+0")<<".\n";
                text<<"The contraction/reduction remains a materialization boundary; its inner loop is serial.\n";
            }
        } else if(_p.values[k.output].role==2) text<<"Write the selected fused expression to the caller-owned result pointer, including its final shape view.\nThis result store remains a separate selected stage.\n";
        else text<<"Compute the selected source operation and materialize its logical output.\n";
        ExplainDependencies(k,text);
        if(_p.fuse) text<<"Source producers are fused views. Only the selected stage result is stored.\n";
        else text<<"Operands are materialized buffers or dense aliases; this stage stores only its own result.\n";
        Explain("STAGE SEMANTICS",text.str(),"  ");
    }
    /* Explain interfaces and actual allocations without assuming a model name
     * or claiming device execution/performance evidence that the plan lacks. */
    void ExplainOverview() {
        std::ostringstream text;std::set<UINT32> allocations;UINT64 bytes=0;
        text<<"These explanations describe the selected logical schedule and its scalar GPU realization.\n";
        text<<"The tensor-level review uses serial helpers; production kernels use the same VHO-selected plan.\n";
        text<<"Fusion, materialization cuts, chunk sizes, and numerical policy were selected before printing.\n";
        text<<"Read stages in order. Fusion enabled: "<<(_p.fuse?"true":"false")<<". With fusion, logical producers are lazy views and may be recomputed.\n";
        text<<"Reshape/transpose views allocate no copied/transposed tensor; exp and pointwise views allocate no result array.\n";
        text<<"Matmuls and reductions retain their selected materialization boundaries.\n";
        for(UINT32 i=0;i<_p.values.size();++i) if(_p.values[i].role)
            text<<(_p.values[i].role==1?"Input ":"Output ")<<pointer(_p,i)<<" => "<<name(_p,i)<<", "<<type(_p.values[i])<<".\n";
        for(size_t i=0;i<_plan.kernels.size();++i) {
            UINT32 value=_plan.kernels[i].output;
            if(!_p.values[value].role && allocations.insert(value).second)
                bytes+=VHO_tensor_elements(_p.values[value])*(_p.values[value].dtype=="bool"?1:4);
        }
        text<<"Selected internal tensor storage: "<<allocations.size()<<" buffers, "<<bytes<<" bytes.\n";
        text<<"This excludes runtime inputs/results, test guards, and the review executor's small view objects.\n";
        text<<"Split reassociation permission: "<<(_p.split_width?"allow":"forbid")<<". Arithmetic and accumulators retain the selected F32 order.\n";
        text<<"Compilation/host checking does not establish GPU execution or speedup; consult the validation receipt.\n";
        Explain("SELECTED ALGORITHM AND INTERFACE",text.str());
    }
    /* Follow exactly the supplied fused cuts in source operand order. */
    void Collect(UINT32 value) {
        if(_bound.count(value) || _printed.count(value)) return;
        require(_p.fuse && _producer.count(value),"missing explanation producer");
        const VHO_TENSOR_OPERATION &op=_p.operations[_producer[value]];
        if(op.name!="common.fill_like") for(size_t i=0;i<op.inputs.size();++i) Collect(op.inputs[i]);
        _kind=VHO_TENSOR_EXPLAIN_PRODUCER;_value=value;ExplainValue(value,op);_printed.insert(value);
    }
    /* Assign global descriptions to the first kernel and stage descriptions
     * to their actual owning kernel. This ordering is also the COMMENT order. */
    std::vector<VHO_TENSOR_EXPLANATION> Build() {
        require(!_plan.kernels.empty(),"empty explanation schedule");
        _kernel=_plan.kernels[0].id;_kind=VHO_TENSOR_EXPLAIN_OVERVIEW;ExplainOverview();
        std::set<UINT32> allocated;
        for(UINT32 k=0;k<_plan.kernels.size();++k) {
            UINT32 value=_plan.kernels[k].output;
            if(_p.values[value].role==0 && allocated.insert(value).second) {
                std::ostringstream text;text<<name(_p,value)<<" is materialized by stage "<<k+1<<": "<<type(_p.values[value])<<"; "
                    <<number(VHO_tensor_elements(_p.values[value])*(_p.values[value].dtype=="bool"?1:4))<<" bytes.\n";
                if(_plan.kernels[k].stage==1) text<<"Chunk-major partials: [chunk, flattened root output], indexed c*N+i. The next merge reads this buffer.\n";
                else text<<"This allocation is a retained boundary result; subsequent lazy views only reference it.\n";
                _kind=VHO_TENSOR_EXPLAIN_STORAGE;_value=value;Explain("MATERIALIZED STORAGE",text.str());
            }
        }
        for(UINT32 k=0;k<_plan.kernels.size();++k) {
            const VHO_TENSOR_KERNEL &kernel=_plan.kernels[k];_kernel=kernel.id;_stage=k+1;_value=0;
            _kind=VHO_TENSOR_EXPLAIN_STAGE;ExplainStage(kernel);
            _bound.clear();_printed.clear();_bound.insert(kernel.inputs.begin(),kernel.inputs.end());
            if(kernel.stage!=2 && _p.fuse) {
                const VHO_TENSOR_OPERATION &op=_p.operations[kernel.root];
                for(size_t i=0;i<op.inputs.size();++i) Collect(op.inputs[i]);
            }
        }
        return _records;
    }
};
} // namespace
/* Format selected facts once for all consumers; presentation never selects. */
std::vector<VHO_TENSOR_EXPLANATION> VHO_tensor_explanations(const VHO_TENSOR_SCHEDULE &plan) {
    return ExplanationBuilder(plan).Build();
}
