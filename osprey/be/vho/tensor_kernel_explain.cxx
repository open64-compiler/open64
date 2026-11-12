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
            text<<"Recomputed here; this source value was also expanded in stage "<<_first_fused_stage[id]<<".\nNo global intermediate is allocated; CTA-local sharing follows the selected stage mapping.\n";
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
    /* Explain structural work and costs relative to the prior thread/output
     * realization of the same selected cuts. Counts are not a latency model. */
    void ExplainCooperativeComparison(const VHO_TENSOR_KERNEL &k,std::ostringstream &text) {
        const VHO_TENSOR_OPERATION &op=_p.operations[k.root];
        UINT64 width=k.stage==2?k.chunks:_p.values[op.inputs[0]].shape.back();
        UINT64 span=k.stage==1?k.split_width:width;
        UINT64 outputs=VHO_tensor_elements(_p.values[op.output]);
        text<<"WALKTHROUGH: lane = thread modulo 32; warp = thread / 32. All four warps are present.\n";
        if(k.mapping==1) {
            text<<"Warp work item = block*4 + warp. ";
            if(k.stage==1) text<<"Decode chunk = work item / "<<outputs<<" and output i = work item modulo "<<outputs<<".\n";
            else text<<"It is output i; guard i < "<<outputs<<" only for reads/stores, never for shuffle participation.\n";
        } else {
            UINT64 n=_p.values[op.output].shape.back(),rows=outputs/n,tiles=(n+3)/4;
            text<<"A CTA owns one row and columns j0..j0+3; warp w owns column j0+w.\n";
            text<<"There are "<<rows<<" logical rows and "<<tiles<<" column tiles per row; the column bound is "<<n<<".\n";
            text<<"For tile base t: thread ids 0..31 stage left[row,t+lane] once per token;\n"
                <<"all threads stage right[t+thread/4,j0+thread%4], with guarded zero-fill.\n"
                <<"Every thread reaches the publish barrier. Warp w then accumulates\n"
                <<"shared_left[lane] * shared_right[4*lane + (w XOR (lane/8))].\n"
                <<"Every thread reaches the release barrier before the next tile can overwrite shared memory.\n";
            text<<"Shared-right producer index: 4*(thread/4) + ((thread%4) XOR ((thread/4)/8)).\n"
                <<"XOR changes physical bank placement only; the logical token/column pairing is unchanged.\n";
            text<<"A full segment has "<<(span+31)/32<<" token tiles and "<<2*((span+31)/32)
                <<" CTA barrier calls along each thread's path, plus the final warp tree.\n";
            text<<"PREVIOUS VERSION COMPARISON: across this root's complete K="<<width<<", the logical left expression\n"
                <<"was evaluated "<<outputs*width<<" times (once per output/token). Staging evaluates it "<<rows*tiles*width
                <<" times (once per row/column-tile/token).\n";
            text<<"Reuse factor for this selected n is n/ceil(n/4) = "<<n<<"/"<<tiles
                <<"; it is four for complete four-column groups, smaller with a partial column tile.\n";
            text<<"For attention weights this reduces repeated probability exp/normalization evaluation across latent columns.\n"
                <<"It does not remove the right-operand products or guarantee the same reduction in HBM traffic:\n"
                <<"the previous version may already hit cache or receive compiler reuse. Each logical right element\n"
                <<"is still loaded per output row/column; cache reuse across heads or CTAs is not created here.\n";
            text<<"Adjacent groups of four load neighboring columns; a whole warp's loads need not form one contiguous transaction.\n";
        }
        text<<"Warp reduction example: lane 0 accumulates begin+0, begin+32, ...; lane 1 uses begin+1, begin+33, ... .\n"
            <<"At each d=16,8,4,2,1 all lanes shuffle; only lanes with lane+d<32 combine the received value.\n"
            <<"This tree brings every lane's partial into lane zero, without atomics or a shared reduction array.\n";
        text<<"PREVIOUS VERSION COMPARISON: one thread traversed up to "<<span
            <<" terms for each output; now a lane traverses at most "<<(span+31)/32
            <<" terms, followed by five shuffle/combine rounds.\n";
        text<<"This describes dependency depth, not elapsed cycles, total instruction count, or a 32x speedup.\n";
        if(k.stage==1) {
            UINT64 tail=width-UINT64(k.chunks-1)*k.split_width;
            text<<"The prior candidate already used these same split chunks; compare against "<<k.split_width<<" serial terms, not the complete context.\n";
            text<<"The final chunk has "<<tail<<" terms: at most "<<(tail+31)/32<<" terms per lane";
            if(k.mapping==2) text<<", "<<(tail+31)/32<<" shared tiles and "<<2*((tail+31)/32)<<" CTA barriers per thread";
            text<<". Inactive lanes still carry neutral values through all five shuffles.\n";
        }
        if(k.stage==2)
            text<<"The earlier merge visited chunks in scalar ascending order. This warp merge visits c=lane,lane+32,...\n"
                <<"before the tree. It combines completed partials, with no additional exponential rescaling or normalization.\n";
        text<<"Launch comparison for this stage: "<<(k.elements+_p.threads-1)/_p.threads
            <<" prior blocks -> "<<VHO_tensor_grid(_p,k)<<" selected blocks, each with "<<_p.threads<<" threads.\n";
        text<<"EXPECTED BENEFIT: shorter serial chains and more independent warps/CTAs can expose additional SM parallelism.\n"
            <<"Reduction inputs contiguous on the reduced axis can also be read by neighboring lanes; fused source views\n"
            <<"determine the final addresses. More CTAs do not imply a proportional speedup.\n"
            <<"COSTS: five shuffles, extra live lane state, more scheduled threads, and (for staged dots) shared loads/stores\n"
            <<"and CTA barriers. Small reductions and short tails can lose; compare the previous AOT candidate on hardware.\n";
    }
    /* Render formulas, tails, work ownership and numerical order directly
     * from the supplied plan; these facts do not select a new schedule. */
    void ExplainStage(const VHO_TENSOR_KERNEL &k) {
        const VHO_TENSOR_OPERATION &op=_p.operations[k.root];
        UINT64 outputs=VHO_tensor_elements(_p.values[op.output]);std::ostringstream text;
        text<<"Selected result: "<<name(_p,k.output)<<"; "<<type(_p.values[k.output])<<"; "<<k.elements<<" work items.\n";
        if(_p.opt_level==3) text<<"Mapped GPU launch: "<<VHO_tensor_grid(_p,k)<<" blocks of "<<_p.threads<<" threads, with excess work items guarded.\n";
        else text<<"Single-thread semantic lowering: visit outputs in a serial loop.\n";
        if(k.stage!=2 && op.name=="common.matmul")
            text<<"LOGICAL ROOT FORMULA (a partial restricts k to its chunk): "<<name(_p,op.output)<<"[batch,row,col] = sum_k "<<name(_p,op.inputs[0])
                <<"[broadcast_batch_A,row,k] * "<<name(_p,op.inputs[1])<<"[broadcast_batch_B,k,col].\n";
        else if(k.stage!=2 && (op.name=="common.reduce_max" || op.name=="common.reduce_sum"))
            text<<"LOGICAL ROOT FORMULA (a partial restricts k to its chunk): output[i] = "<<(op.name=="common.reduce_max"?"max":"sum")<<"_k "
                <<name(_p,op.inputs[0])<<"[i*K+k], K="<<_p.values[op.inputs[0]].shape.back()<<".\n";
        if(k.mapping) {
            ExplainCooperativeComparison(k,text);
            UINT64 width=k.stage==2?k.chunks:_p.values[op.inputs[0]].shape.back();
            text<<"Cooperative mapping: four complete 32-lane warps per CTA; each warp reduces one output.\n";
            text<<"Each lane accumulates indices begin+lane, begin+lane+32, ... in F32, then all lanes call\n"
                <<"__shfl_down_sync with full mask 0xffffffff and offsets 16,8,4,2,1. Lane zero owns the result store.\n";
            text<<"All lanes participate, including neutral padding for invalid outputs and tails; no lane exits before collectives.\n";
            text<<"The neutral value is "<<(op.name=="common.reduce_max"?"-infinity; maxima propagate NaNs":"+0; sums retain F32 additions")<<".\n";
            if(k.mapping==2)
                text<<"Shared staging: 32 left-operand values plus 32x4 right-operand values (640 bytes per CTA).\n"
                    <<"Shared right-operand index is 4*k + (column XOR (k/8)); producer and consumer warps avoid bank conflicts.\n"
                    <<"The left expression is evaluated once per token within each tile and reused across four adjacent output columns.\n"
                    <<"A CTA barrier publishes each tile and a second CTA barrier releases it before overwrite.\n"
                    <<"Column and token tails are zero-filled. Both barriers are outside every lane/tail predicate.\n";
            if(k.stage==1) text<<"Chunk c covers [c*"<<k.split_width<<",min((c+1)*"<<k.split_width<<","<<width<<")); final chunk starts at "<<UINT64(k.chunks-1)*k.split_width<<" and contains "<<width-UINT64(k.chunks-1)*k.split_width<<" terms.\n"
                <<"Scratch remains chunk-major at c*"<<outputs<<"+i; the subsequent merge reads completed partials.\n";
            if(k.stage==2) text<<"Merge lanes traverse chunk-major partial[c*"<<outputs<<"+i] for c=lane, lane+32, ... .\n";
            text<<"Only the lane-local accumulation loop remains serial; the warp tree changes F32 grouping and requires explicit reassociation.\n";
        } else if(k.stage==2) {
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
        text<<"Reassociation permission: "<<((_p.split_width || _p.cooperative)?"allow":"forbid")<<". Arithmetic follows the selected F32 grouping.\n";
        text<<"Compilation/host checking does not establish GPU execution or speedup; consult the validation receipt.\n";
        if(_p.cooperative) {
            text<<"READING GUIDE: this selected plan has "<<_plan.kernels.size()<<" launches, as did the previous\n"
                <<"fused/split thread-per-output candidate. Cooperative mapping changes work inside those stages;\n"
                <<"it does not remove launches, materialized score tensors, or split scratch buffers.\n";
            text<<"Read the formula, work ownership, dependency, previous-version comparison, and expected costs in each stage.\n";
            for(UINT32 i=0;i<_plan.kernels.size();++i) {
                const VHO_TENSOR_KERNEL &k=_plan.kernels[i];const VHO_TENSOR_OPERATION &op=_p.operations[k.root];
                text<<"  Step "<<i+1<<" / kernel "<<k.id<<": "<<op.name
                    <<(k.stage==1?" partials":k.stage==2?" merge":" direct")<<" -> "<<name(_p,k.output)
                    <<"; "<<(k.mapping==2?"shared staged dot":k.mapping==1?"warp reduction":"previous scalar mapping retained")<<".\n";
            }
            text<<"For attention: query absorption and score contractions precede global max; stable exponential sums\n"
                <<"depend on that max, normalized weighted-latent products depend on the global sum, and output\n"
                <<"projection consumes the finalized latent value. The producer/dependency blocks name the actual tensors.\n";
            text<<"EXPECTED PERFORMANCE, NOT A MEASUREMENT: long reductions have more opportunity to amortize collective\n"
                <<"overhead; short contexts can be dominated by launch/synchronization cost. Neither outcome is guaranteed.\n"
                <<"If the modified stages previously occupied fraction f of total time and achieve net stage speedup s,\n"
                <<"whole-call speedup is 1 / ((1-f) + f/s), assuming other costs stay constant. Include all staging,\n"
                <<"shuffle and scheduling costs in s; unchanged score/projection/launch costs remain in 1-f.\n"
                <<"No calibrated f or s is attached to this plan, so no numerical runtime speedup is asserted.\n"
                <<"A static architecture model could estimate cycles with sufficient latency/resource information;\n"
                <<"these comments provide operation/ownership counts and do not claim such a calibrated model.\n";
        }
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
