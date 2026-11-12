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
                if(producer.mapping>=3) {
                    text<<(producer.stage==1?"  This is completed chunk-major [m_c,d_c,z_c] scratch, not the final latent/LSE bundle.\n":
                        "  This is the completed fused latent/LSE bundle; no global score buffer is read.\n");
                } else if(producer.stage==2) {
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
    /* Compare structural work with the preceding cooperative score-buffer
     * cuts. These counts describe source/selected facts, not a timing model. */
    void ExplainAttentionPerformance(std::ostringstream &text,UINT64 selected_bytes) {
        const VHO_TENSOR_ATTENTION &a=_plan.attention;
        UINT64 old_bytes=0,old_launches=0;
        for(size_t i=0;i<_p.operations.size();++i) {
            const VHO_TENSOR_OPERATION &op=_p.operations[i];const VHO_TENSOR_VALUE &v=_p.values[op.output];
            bool boundary=op.name=="common.matmul" || op.name=="common.reduce_max" || op.name=="common.reduce_sum";
            if(!boundary && v.role!=2) continue;
            ++old_launches;
            if(v.role==0) old_bytes+=4*VHO_tensor_elements(v);
            UINT64 width=boundary?_p.values[op.inputs[0]].shape.back():0;
            if(_p.split_width && width>_p.split_width) {
                ++old_launches;old_bytes+=4*VHO_tensor_elements(v)*((width+_p.split_width-1)/_p.split_width);
            }
        }
        UINT64 rows=a.batch*a.heads,span=_p.split_width && a.length>_p.split_width?_p.split_width:a.length;
        UINT64 quantum=_p.chunk_policy>=3 && _p.split_width && span<=64?span:128;
        UINT64 chunks=(a.length+span-1)/span,capacity=((span+quantum-1)/quantum)*quantum;
        UINT64 old_probability_groups=a.length>=32?(a.rank+3)/4:a.rank;
        text<<"PERFORMANCE COMPARISON BASELINE: the same captured DAG with the previous cooperative score-buffer cuts.\n";
        text<<"Total launches "<<old_launches<<" -> "<<_plan.kernels.size()<<"; global internal allocation bytes "<<old_bytes<<" -> "<<selected_bytes<<".\n";
        text<<"The two score allocations account for "<<8*rows*a.length<<" bytes before fusion and zero afterward.\n"
            <<"Allocation footprint is not total HBM traffic: fusion also removes their stores/reloads, whose cache behavior is workload dependent.\n";
        text<<"Previous nominal token exp evaluations: "<<(1+old_probability_groups)*rows*a.length
            <<" = rows*L*(one denominator pass + "<<old_probability_groups<<" weighted-latent column groups).\n";
        text<<"Fused valid-token exp evaluations: "<<rows*a.length<<". The selected loops actually visit "<<rows*chunks*capacity
            <<(((_p.chunk_policy==4 && span==32) || (_p.chunk_policy>=5 && _p.split_width && (span==32 || span==64)))?" lane exponential positions including neutral tails":" shared exp slots including padding");
        if(chunks>1) text<<", plus at most "<<rows*chunks<<" chunk-rescale exps (empty chunks take zero)";
        text<<".\nThis is source-level work accounting; compiler folding/predication and instruction throughput can change executed cost.\n";
        if(_p.chunk_policy==6 && _p.split_width==64)
            text<<"Warp-owned local merging adds up to four rescaling exps per CTA (two in each leader), plus two coefficient broadcasts per warp. Empty warp masses skip exp.\n";
        UINT64 prior_weighted_ctas=a.length>=32?rows*((a.rank+3)/4)*chunks:(rows*a.rank*chunks+127)/128;
        text<<"The previous weighted-latent launch had "<<prior_weighted_ctas<<" CTAs; the fused local-attention launch has "<<rows*chunks
            <<" CTAs, now also doing scoring and softmax. This can reduce parallelism even while eliminating work and traffic.\n";
        text<<"The source score dots are evaluated once per allowed row/token; false masks can skip those dots, but irregular masks can diverge.\n"
            <<"Query absorption and value projection still execute. Cache values are still read for scoring and weighted accumulation.\n"
            <<"Fewer launches can matter most for launch-dominated short contexts; reduced score traffic and repeated exponentials\n"
            <<"can matter more with longer contexts. More shared work or fewer independent CTAs can offset those benefits.\n"
            <<"If the changed portion occupied old-time fraction f and achieves net speedup s including all new overhead,\n"
            <<"whole-call speedup is 1/((1-f)+f/s), assuming other costs stay constant. For illustration only, f=1/2 and s=2\n"
            <<"would mean 4/3 total speedup, not 2. Neither f nor s is calibrated or measured for this plan.\n"
            <<"A static architecture latency/resource model can estimate costs; these comments report structural counts, not calibrated cycles.\n";
    }
    /* Describe the fused score/softmax/latent stage and its tuple merge from
     * the owning phase's selected facts, never from CUDA spelling heuristics. */
    /* Narrow physical ownership is a versioned VHO decision. Keep legacy
     * descriptions exact so already sealed v1/v2 recipes remain reproducible. */
    /* Lane-private one-token ownership is selected by VHO; no shared-memory
     * ordering assumption is delegated to the CUDA printer. */
    void ExplainWarpRegisterStage(const VHO_TENSOR_KERNEL &k) {
        const VHO_TENSOR_ATTENTION &a=_plan.attention;std::ostringstream text;
        UINT64 rows=a.batch*a.heads,tail=a.length-UINT64(k.chunks-1)*32;
        text<<"FUSED ATTENTION: SINGLE-WARP REGISTER STATISTICS; 32 threads, zero shared bytes.\n"
            <<"Launch "<<VHO_tensor_grid(_p,k)<<" CTAs: row=block modulo "<<rows<<", chunk=block / "<<rows<<". Each lane owns token chunk*32+lane.\n"
            <<"PHASE 1: compute scale*(dot(projected_query,latent_cache)+dot(rope_query,rope_cache)) in ascending F32 K order.\n"
            <<"Keep score in a lane-private register; false masks and tail lanes keep -infinity without source reads.\n"
            <<"PHASE 2: all lanes execute the full-mask shuffle-down max tree at offsets 16,8,4,2,1.\n"
            <<"__shfl_sync(0xffffffff,warp_maximum,0,32) broadcasts lane zero's raw maximum into every lane's register.\n"
            <<"Keep raw maximum for the partial tuple, and derive the finite-safe maximum (zero fallback) in registers.\n"
            <<"PHASE 3: compute exp(score-safe_max) once per lane, retain it in a register, and execute the F32 sum tree.\n"
            <<"A second full-mask __shfl_sync from lane zero broadcasts the denominator to every lane.\n"
            <<"PHASE 4: normalize the lane-private exponential by denominator>0?denominator:1, retaining the probability register.\n"
            <<"ZERO MEMORY FENCES: no __syncwarp or CTA barrier, no shared score/probability/statistics array.\n"
            <<"Shuffle collectives still rendezvous the complete warp and exchange registers; they are not general shared-memory fences.\n"
            <<"WHY THE THIRD FENCE DISAPPEARS: width=threads=32 gives exactly one token per lane. Every column group reads only its owning lane's probability.\n"
            <<"No lane reads another lane's shared write; the probability register is immutable across all column groups.\n"
            <<"This proof requires one full warp, local split width32 and register latent accumulation. It does not apply to width64, wider spans or the merge.\n"
            <<"PHASE 5: warp0 owns columns group*4+j (j=0..3), with ceil(R/4)="<<(a.rank+3)/4<<" groups for R="<<a.rank<<".\n"
            <<"Reset four lane-private sums for each group, multiply the same probability register by valid cache columns, then reduce each sum with the full-mask tree.\n"
            <<"Lane zero writes valid columns. Column/tail predicates guard memory/stores, never shuffle participation.\n"
            <<"At R16 the four groups cover columns0..3,4..7,8..11,12..15. The 80 latent warp-level shuffle calls remain; statistics add ten down-shuffles and two broadcasts.\n"
            <<"Both broadcasts use mask0xffffffff, source lane0, width32, and their F4 return-preg is read immediately in WHIRL.\n"
            <<"FINAL TAIL: "<<tail<<" real token(s), "<<32-tail<<" neutral lanes per row. Every lane still executes every collective.\n"
            <<"An allowed finite one-token chunk has d_c=1,z_c=its cache vector; an empty chunk has d_c=0,z_c=0.\n"
            <<"Write [raw_m_c,d_c,normalized_z_c] at (chunk*"<<rows<<"+row)*(R+2); no global score/probability buffers.\n"
            <<"The retained 128-thread merge uses m=max m_c, t_c=d_c*exp(m_c-m), D=sum t_c, z=sum(t_c/D)*z_c, LSE=m+log(D).\n"
            <<"Select zero mass before exp for empty chunks. All-empty rows give zero latent and -infinity LSE.\n"
            <<"RESOURCE TRADEOFF: selected local shared storage drops160->0 bytes and three warp memory fences disappear, replaced by two register broadcasts.\n"
            <<"Lane-private score, maximum, denominator and probability can increase register pressure; inspect ptxas spills/resources and whole-call GPU timing.\n"
            <<"The block/chunk count, four-column grouping, tuple traffic, merge and projection stages are unchanged at this width. No measured speedup is asserted.\n";
        text<<"REGISTER LIFETIME WALKTHROUGH: attention_masked_score survives the max tree; accumulator/next_value are tree scratch.\n"
            <<"attention_raw_maximum receives the first broadcast; attention_maximum is its finite-safe copy. Raw and safe maxima remain distinct for tuple semantics.\n"
            <<"attention_weight first holds this lane's exponential, survives the denominator tree, then holds its normalized probability.\n"
            <<"attention_denominator receives the second broadcast and is separate from later column-reduction scratch.\n"
            <<"Four latent sum registers reset per column group, while the probability register is never overwritten by the column trees.\n"
            <<"WHY BROADCAST AFTER A TREE: lane zero is guaranteed to own the complete 32-lane reduction; other lanes can hold only partial results.\n"
            <<"A shuffle-down tree alone is not an all-lanes reduction. Source lane0 supplies the same completed maximum/denominator to every lane.\n"
            <<"FENCE 1 REMOVAL (maximum): old lane0 shared max store -> warp memory fence -> all-lane shared max loads.\n"
            <<"New: full-mask max tree -> register broadcast -> each lane derives its safe maximum. The shared producer/consumer edge no longer exists.\n"
            <<"FENCE 2 REMOVAL (denominator): old lane0 shared denominator store -> warp memory fence -> shared denominator loads.\n"
            <<"New: preserve each lane's exponential, sum through scratch registers, broadcast lane0's completed sum, and divide the preserved exponential.\n"
            <<"FENCE 3 REMOVAL (probabilities): old shared probability stores -> publication fence -> latent shared probability loads.\n"
            <<"New: lane l retains p_l and uses it for every valid cache column in every group. No lane reads a probability produced by another lane.\n"
            <<"For R16, lane l updates p_l*cache[token,0..3], then 4..7,8..11,12..15 using the same p_l. Only the product sums cross lanes, through shuffle trees.\n"
            <<"INDEPENDENT-SCHEDULING PROOF: each private value follows its own lane's program order; each cross-lane scalar dependency is an explicit full-mask shuffle.\n"
            <<"All 32 non-exited lanes must reach the same collectives with mask0xffffffff. Masked/tail predicates surround memory/arithmetic, not collective calls.\n"
            <<"The broadcasts replace register communication needs, not the memory-order semantics of a fence. There is no shared-memory communication to order.\n"
            <<"TWO-TOKEN MATHEMATICAL EXAMPLE: scores 0 and log(3), with all other lanes masked, give m=log(3), e0=1/3,e1=1, D=4/3.\n"
            <<"Broadcasts copy m and D into all lanes; lane0 keeps p0=1/4 and lane1 keeps p1=3/4. If a cache column is 2 and 10, its reduced z_c is8.\n"
            <<"This is an illustrative real-arithmetic example; actual F32 exp/reductions obey the existing numerical policy and reference tolerance.\n"
            <<"EMPTY/TAIL PARTICIPATION: an empty chunk has raw maximum=-infinity, safe maximum=0, exponential=0, denominator=0 and normalized probability=0.\n"
            <<"Neutral lanes still broadcast and shuffle. The final "<<tail<<"-token chunk changes memory guards, not collective participation or group count.\n"
            <<"The readable serial helper's arrays emulate lane state; the actual WHIRL-derived local kernel has register scalars and no shared arrays.\n";
        ExplainDependencies(k,text);Explain("STAGE SEMANTICS",text.str());
    }
    /* Two warp leaders fold immutable, disjoint shared summaries, then each
     * broadcasts the complete result within its own 32-lane warp. */
    void ExplainTwoWarpRegisterStage(const VHO_TENSOR_KERNEL &k) {
        const VHO_TENSOR_ATTENTION &a=_plan.attention;std::ostringstream text;
        UINT64 rows=a.batch*a.heads,tail=a.length-UINT64(k.chunks-1)*64;
        text<<"FUSED ATTENTION: TWO-WARP REGISTER BROADCASTS; 64 threads, three CTA barriers, 272 selected shared bytes.\n"
            <<"Launch "<<VHO_tensor_grid(_p,k)<<" CTAs. Row=block modulo "<<rows<<", chunk=block / "<<rows<<". Thread t owns score token chunk*64+t.\n"
            <<"Scores and unnormalized exponentials stay lane-private. Mask-false/tail scores are -infinity and skip source dot reads.\n"
            <<"SHARED LAYOUT: probabilities[64] plus summaries[4]. Maxima use summaries[0,1]; denominators use summaries[2,3].\n"
            <<"REGISTER LIFETIME WALKTHROUGH: preserve score s_t while scratch registers run the max tree; preserve e_t while scratch runs the sum tree.\n"
            <<"A shuffle-down tree leaves a complete reduction in lane0 only. The following source0 broadcast supplies that result to every lane needing exp or division.\n"
            <<"CTA BARRIER 1: each warp reduces its 32 score registers with full-mask down-shuffles 16,8,4,2,1; lane zero publishes its warp maximum.\n"
            <<"The first unconditional CTA barrier publishes both maxima. Lane zero in EACH warp folds max summaries0 then1 in the original F32 order.\n"
            <<"Each warp executes __shfl_sync(mask0xffffffff,value,source0,width32) to broadcast its leader's identical complete maximum.\n"
            <<"WARP-RELATIVE SOURCE: source0 means physical thread0 in warp0 and thread32 in warp1, not thread0 for the entire64-thread CTA.\n"
            <<"Both leaders recompute the same two-summary fold so neither warp needs a shared final statistic produced by the other.\n"
            <<"Raw and finite-safe maxima remain registers. No shared final-maximum store/publication barrier is required.\n"
            <<"Each lane computes exp(score-safe_max), keeps that exponential private, and performs the existing warp denominator sum tree.\n"
            <<"CTA BARRIER 2: lane zero of each warp writes its denominator to summaries[2+warp]; this barrier publishes both denominator partials.\n"
            <<"Both warp leaders fold denominator summaries2 then3 from +0 in the same F32 order, then broadcast the completed denominator within their warp.\n"
            <<"DISJOINT-SLOT PROOF: max slots0,1 are never overwritten by denominator slots2,3. A fast warp cannot clobber a maximum that the other leader has not read.\n"
            <<"FAST/SLOW WARP TIMELINE: A both leaders store maxima; B barrier1 publishes them; C warp0 reads maxima while warp1 pauses before its reads.\n"
            <<"D warp0 computes exponentials and writes its denominator at slot2; E warp1 resumes and still reads unchanged max slots0,1.\n"
            <<"If D wrote slot0 instead, E could mistake a sum for a maximum. Barrier1 orders pre-barrier stores; it does not order these later reads against later stores.\n"
            <<"Denominator slots2,3 likewise remain unchanged while the separate probability array is populated; a fast warp cannot overwrite a slow leader's sum inputs.\n"
            <<"Reusing two slots for both reductions would require a release barrier and invalidate this three-barrier schedule.\n"
            <<"CTA BARRIER 3: normalize each lane's private exponential using denominator>0?denominator:1, store probabilities[tid], then publish all64 probabilities.\n"
            <<"WHY THIS BARRIER REMAINS: each latent-column warp visits tokens lane and lane+32, so each consumes probabilities produced by the other warp.\n"
            <<"THREE PUBLICATION EDGES: barrier1 orders both max stores before either leader's max loads; barrier2 orders both sum stores before either leader's sum loads.\n"
            <<"Barrier3 orders all64 probability stores before either column warp loads them. Each edge includes shared data written by the other warp.\n"
            <<"Removing either former final-statistic barrier is safe because the final max and denominator travel only through registers and warp-local broadcasts.\n"
            <<"A 32-lane shuffle does not communicate across warps and is not a general shared-memory fence. CTA barriers1,2,3 supply the remaining cross-warp ordering.\n"
            <<"All64 threads reach all three barriers. All32 lanes per warp reach every full-mask shuffle/broadcast, including neutral mask/tail lanes.\n"
            <<"COLUMN OWNERSHIP: warp w owns group*8+w*4+j, j=0..3; ceil(R/8)="<<(a.rank+7)/8<<" groups for R="<<a.rank<<".\n"
            <<"TOKEN/COLUMN CROSSING: lane l of BOTH column warps visits tokens l and l+32; score ownership is one token per thread, column ownership is four columns per warp.\n"
            <<"For example warp0 lane0 needs p0 and p32 for columns0..3; warp1 lane0 needs the same p0,p32 for columns4..7. Each reads the other warp's probability.\n"
            <<"Each group resets four private sums and reads immutable shared probabilities; lane zero stores valid reduced columns.\n"
            <<"At R16, group0 covers warp0 columns0..3/warp1 columns4..7, group1 covers8..11/12..15. The 80 latent warp-level down-shuffles remain.\n"
            <<"Each max/denominator broadcast CALL executes once per warp: two static broadcast sites, four warp-level broadcasts per CTA.\n"
            <<"This replaces five CTA barrier phases with three, without changing score arithmetic, column grouping, chunk count or tuple traffic.\n"
            <<"FINAL TAIL: "<<tail<<" valid token(s), "<<64-tail<<" neutral score lanes per row. A completely empty warp still publishes neutral summaries and joins all collectives.\n"
            <<"For a35-token tail, warp0 supplies32 scores and warp1 supplies3; both leaders still fold both summaries and both column warps consume all35 probabilities.\n"
            <<"For a one-token tail, warp1 has no score tokens but still contributes neutral max/sum, broadcasts and computes its valid latent columns using p0 after barrier3.\n"
            <<"Finite one-token tails retain d_c=1,z_c=that cache vector; empty chunks retain d_c=0,z_c=0. Raw and safe maximum fallbacks remain distinct.\n"
            <<"Write [raw_m_c,d_c,normalized_z_c] at (chunk*"<<rows<<"+row)*(R+2). The separate128-thread merge retains stable zero-mass-guarded rescaling and its five CTA barriers.\n"
            <<"Selected shared storage is4*(64+4)=272 bytes (prior4*(64+8)=288). Only normalized probabilities are written to the64-slot array.\n"
            <<"PROOF SCOPE: coverage-v5, split local mapping3/stage1, threads=width=64 and register latent accumulation. Other widths or changed ownership require reanalysis.\n"
            <<"Register pressure, duplicate two-summary folds and extra broadcasts can offset barrier/shared-traffic savings. Inspect backend resources and GPU timing; no measured speedup is asserted.\n";
        ExplainDependencies(k,text);Explain("STAGE SEMANTICS",text.str());
    }
    /* Describe the VHO-selected ownership and sole publication edge; this
     * shared payload also becomes native COMMENT nodes and normal-whirl2c CUDA. */
    void Explain_warp_owned_stage(const VHO_TENSOR_KERNEL &k) {
        const VHO_TENSOR_ATTENTION &a=_plan.attention;std::ostringstream text;
        text<<"WARP-OWNED TOKEN PARTIALS: 64 threads, one unconditional CTA barrier, "<<8*(a.rank+2)<<" selected shared bytes.\n"
            <<"Launch "<<VHO_tensor_grid(_p,k)<<" CTAs. Row=block modulo "<<a.batch*a.heads<<", chunk=block / "<<a.batch*a.heads<<"; token=chunk*64+warp*32+lane.\n"
            <<"OWNERSHIP CHANGE: each warp owns its32 tokens AND ALL latent columns, rather than owning four columns while reading both token halves.\n"
            <<"Each score is still computed once. Scores, exponentials and normalized probabilities stay lane-private; no shared score/probability array exists.\n"
            <<"REGISTER VALUE FLOW: attention_masked_score=s_t stays private while accumulator/next_value run the max tree; attention_raw_maximum=m_w and attention_maximum=safe_m_w are broadcast-derived scalars.\n"
            <<"attention_weight then holds e_t, survives the denominator scratch reduction, and is overwritten by p_t after division. No other lane reads this private probability.\n"
            <<"WARP-LOCAL SOFTMAX: reduce raw maximum m_w through full-mask down-shuffles16,8,4,2,1, broadcast lane0 with source0/width32, derive safe maximum.\n"
            <<"Preserve e_t=exp(s_t-safe_m_w) while scratch registers reduce d_w; broadcast d_w and retain p_t=e_t/(d_w>0?d_w:1) in each lane.\n"
            <<"LOCAL NORMALIZATION: p_t is normalized over this warp's allowed tokens only. The final64-token probability is alpha_w*p_t; local normalization alone does not combine the warps.\n"
            <<"For finite nonempty scores, e_t<=1 and 1<=d_w<=32; d_w measures exponential mass relative to m_w and is not generally the token count.\n"
            <<"BROADCAST SOURCE0: lane0 means physical thread0 in warp0 and thread32 in warp1. A reduction tree leaves its complete result in the leader, so both max and denominator need broadcasts.\n"
            <<"RAW/Safe DISTINCTION: preserve raw m_w=-infinity for empty warps, use safe_m_w=0 only for arithmetic. All neutral lanes still reach every collective.\n"
            <<"LATENT OWNERSHIP: group g covers columns4*g+j, j0..3, in BOTH warps; ceil(R/4)="<<(a.rank+3)/4<<" groups. Each lane uses only its own token and probability.\n"
            <<"Four private sums reset per group. Full-mask warp trees produce z_w[j]=sum_{t in warp} p_t*cache[t,j]; each leader writes its own tuple.\n"
            <<"At R16 BOTH warps visit column groups0..3,4..7,8..11,12..15. Warp0 uses only tokens0..31 and warp1 only32..63; together they compute64*R token/cache products, not twice that work.\n"
            <<"TUPLE MEANING: z_w is a normalized local expectation, not an unnormalized numerator. Retain BOTH m_w and d_w so the final merge can restore each warp's relative mass.\n"
            <<"IMMUTABLE SHARED TUPLES: partials[warp*(R+2)+component], component0=raw_m_w,1=d_w,2+j=z_w[j]; extent2*(R+2)="<<2*(a.rank+2)<<" floats.\n"
            <<"TUPLE SLOT MAP: warp0 uses m[0],d[1],z[2.."<<a.rank+1<<"]; warp1 uses m["<<a.rank+2<<"],d["<<a.rank+3<<"],z["<<a.rank+4<<".."<<2*a.rank+3<<"].\n"
            <<"For R16 this is [m0,d0,z0[0..15]] at slots0..17 and [m1,d1,z1[0..15]] at18..35:36 floats,144 bytes. These are CTA-local partials, not extra global scratch.\n"
            <<"SOLE CTA BARRIER: both warps finish all tuple stores before any thread reads either tuple. Slots have one producing warp, disjoint writes and no reuse.\n"
            <<"Old max and denominator CTA barriers disappear because softmax statistics are warp-local. The old probability barrier disappears because probabilities never cross warps.\n"
            <<"NEW CROSS-WARP EDGE: complete [m_w,d_w,z_w] tuples cross warps together at this one publication; no read occurs before that barrier.\n"
            <<"PUBLICATION TIMELINE: a fast warp may finish its entire tuple while the other still processes columns, but it waits at the barrier before loading either tuple.\n"
            <<"After both arrive, every shared element has its final value. Consumers never overwrite these slots, so no read/reuse release barrier is needed.\n"
            <<"STABLE TWO-WARP MERGE: both leaders fold m=max(m_0,m_1) in order0,1. For d_w==0 set t_w=0 BEFORE exp; otherwise t_w=d_w*exp(safe_m_w-safe_m).\n"
            <<"DERIVATION: d_w*z_w[j]=sum_{t in warp} exp(s_t-m_w)*cache[t,j]. Multiplying by exp(m_w-m) expresses this numerator on one common exponential origin.\n"
            <<"Therefore t_w*z_w[j] is that warp's common-origin numerator and sum_w t_w is the common-origin denominator; their ratio is the64-token expectation.\n"
            <<"For finite participating maxima, m_w-m<=0, so the merge does not evaluate exp of a large positive raw score. Averaging z0,z1 without their masses would be incorrect in general.\n"
            <<"Compute D=(0+t_0)+t_1, alpha_w=t_w/(D>0?D:1), and broadcast alpha0 then alpha1 from lane0 within each warp.\n"
            <<"Final column j is z[j]=(0+alpha0*z_0[j])+alpha1*z_1[j]. Thread tid owns columns tid,tid+64,... with guarded stores. Thread0 writes raw m and D.\n"
            <<"LEADER RECOMPUTATION: both leaders independently derive the same m,D,alpha0,alpha1 in order0,1. Only thread0 stores m,D; other lanes need only the two broadcast coefficients.\n"
            <<"This intentional duplicate scalar fold avoids publishing another shared final statistic and introducing a second CTA barrier.\n"
            <<"At R16 final stores belong to threads0..15; threads16..63 still join all broadcasts. For R>64 each owner continues at tid+64,tid+128,... .\n"
            <<"NUMERICAL EXAMPLE: in one example cache column, warp0 has one allowed score0 with value2; warp1 has two allowed scores log(3) with values10 and14; all other lanes are masked.\n"
            <<"Then (m0,d0,z0)=(0,1,2), (m1,d1,z1)=(log(3),2,12); common masses are1/3 and2, D=7/3, coefficients1/7 and6/7, final z=74/7.\n"
            <<"The direct token weights are1,3,3, giving (2+30+42)/7=74/7. An unweighted average of the local expectations would give7 and would be wrong.\n"
            <<"This is an illustrative real-arithmetic example; actual F32 exp/trees/division retain the admitted numerical policy and reference tolerance.\n"
            <<"Four static register-broadcast sites execute per warp: local max, local denominator, alpha0, alpha1. A32-lane shuffle never crosses warps or replaces a memory fence.\n"
            <<"These four broadcast sites execute in EACH warp: eight warp-level broadcasts per CTA. The serial readable helper emulates lane state; native WHIRL owns the actual memory/barrier schedule.\n"
            <<"All64 threads reach the sole CTA barrier; all32 lanes reach every full-mask collective outside tail/mask/column predicates. Independent scheduling is supported.\n"
            <<"TAILS: a35-token tail gives32+3 tokens; a one-token tail gives1+0. Empty warps publish [-infinity,0,zero_vector] and still participate.\n"
            <<"EMPTY-WARP GUARD EXAMPLE: if a nonempty warp has m=-1000 and the other has d=0, evaluating its fallback exp(0-(-1000)) could overflow before multiplying by zero. Branch on d==0 first.\n"
            <<"If both warps are empty, raw m=-infinity, safe m=0, D=0, both coefficients=0 and z=zero_vector. A finite one-token tail has D=1 and z=that token's cache vector.\n"
            <<"In that one-token tail, the empty second warp produces its OWN zero latent vector; it never consumes warp0's probability. Its tuple is read only after the shared barrier.\n"
            <<"The combined output remains [raw_m_c,d_c,normalized_z_c]. The separate128-thread inter-chunk merge keeps its existing stable rescaling and five CTA barriers.\n"
            <<"NUMERICAL CHANGE: an additional stable hierarchical reduction changes F32 rounding; O3 explicit allow-reassociate and FP64 reference tolerances are required, not bitwise equality with coverage-v5.\n"
            <<"COST: token/cache products are not duplicated, but both warps reduce every column. At R16 latent warp-level down-shuffles rise80->160; extra rescaling and broadcasts can offset barrier savings.\n"
            <<"Shared bytes=8*(R+2), versus272 in coverage-v5; below272 only for R<32, equal at R32, larger above32. Resource admission uses the actual rank.\n"
            <<"PROOF SCOPE: coverage-v6, split local width=threads=64, register latent accumulation, finite admitted shape. Other widths/direct/merge retain their prior algorithms.\n"
            <<"coverage-v5 remains selectable for the three-barrier ownership. GPU latency, race-sanitizer and measured speedup remain hardware-gated.\n";
        ExplainDependencies(k,text);Explain("STAGE SEMANTICS",text.str());
    }
    void ExplainNarrowBlockStage(const VHO_TENSOR_KERNEL &k) {
        if(k.warp_owned_partials) {Explain_warp_owned_stage(k);return;}
        if(k.two_warp_statistics) {ExplainTwoWarpRegisterStage(k);return;}
        if(k.register_statistics) {ExplainWarpRegisterStage(k);return;}
        const VHO_TENSOR_ATTENTION &a=_plan.attention;std::ostringstream text;
        UINT64 rows=a.batch*a.heads,warps=k.threads/32,columns=warps*4;
        text<<"FUSED ATTENTION: chunk-local attention; NARROW BLOCK OWNERSHIP.\n"
            <<"Launch "<<VHO_tensor_grid(_p,k)<<" CTAs of "<<k.threads<<" threads ("<<warps
            <<" full warp(s)); selected shared storage "<<VHO_tensor_shared_bytes(_p,k)<<" bytes per CTA.\n"
            <<"CTA row = block modulo "<<rows<<"; chunk = block / "<<rows<<". Chunk interval [c*"
            <<k.split_width<<",min((c+1)*"<<k.split_width<<","<<a.length<<")).\n"
            <<"Each thread scores local token tid; full chunks have no block-padding tokens. Tail reads are guarded.\n"
            <<"score = scale*(dot(projected_query,latent_cache[token])+dot(rope_query,rope_cache[token])).\n"
            <<"Both dots keep ascending F32 K order. Mask-false and tail slots store -infinity without source reads.\n"
            <<"Shared score capacity "<<k.split_width<<" is overwritten with exp(score-safe_max), then normalized probabilities.\n"
            <<"Max and denominator use full-mask shuffle-down trees at offsets 16,8,4,2,1; every lane participates.\n";
        if(warps==1) text<<"ONE-WARP STATISTICS: lane zero directly publishes maximum/safe maximum or denominator.\n"
            <<"__syncwarp(0xffffffff) publishes each statistic before other lanes read it. A third warp fence publishes normalized weights.\n"
            <<"Three unconditional warp publication barriers, zero CTA barriers; no cross-warp partial array fold is needed.\n"
            <<"Explicit memory ordering is required under independent thread scheduling; lockstep is not assumed.\n";
        else text<<"TWO-WARP STATISTICS: lane zero of each warp publishes one summary. A CTA barrier publishes both;\n"
            <<"thread zero folds exactly two summaries in ascending warp order and publishes the statistic with a second CTA barrier.\n"
            <<"Max and denominator require four CTA barriers; normalized-weight publication adds the fifth.\n"
            <<"Barrier count remains five, but only 64 threads participate and neutral scoring warps disappear.\n";
        text<<"TOKEN OWNERSHIP WALKTHROUGH: tid=32*warp+lane. Scoring uses tid, whereas each latent-column warp visits lane,lane+32,... .\n";
        if(warps==2) text<<"For a full width-64 chunk, warp 0 scores tokens 0..31 and warp 1 scores tokens 32..63.\n"
            <<"During latent accumulation both warps read probabilities for tokens 0..63, but they own different columns.\n"
            <<"Lane 0 in either warp visits tokens 0,32; lane 31 visits 31,63. Token duplication across column warps computes different outputs.\n"
            <<"CTA PHASES: (1) publish two warp maxima; (2) publish thread zero's raw/safe maximum;\n"
            <<"(3) publish two warp denominator sums; (4) publish thread zero's denominator; (5) publish normalized probabilities.\n";
        else text<<"For a full width-32 chunk, tid=lane scores token lane. Each column group revisits those same 32 probabilities.\n"
            <<"WARP PHASES: (1) publish lane zero's raw/safe maximum; (2) publish lane zero's denominator;\n"
            <<"(3) publish normalized probabilities. All 32 lanes reach each full-mask fence, even for a masked or one-token chunk.\n"
            <<"The third fence preserves the canonical immutable-probability publication contract; no claim of minimal possible synchronization is made.\n";
        text<<"The statistics allocation contains eight F32 slots: raw maximum [4], safe maximum [5], denominator [6].\n"
            <<"A two-warp block also uses [0] and [1] for warp summaries; a one-warp block skips that cross-warp fold.\n"
            <<"Selected local storage = 4*("<<k.split_width<<" score/weight slots + 8 statistics slots) = "
            <<VHO_tensor_shared_bytes(_p,k)<<" bytes. Backend allocation can shrink unused statistics slots.\n";
        text<<"REGISTER ACCUMULATION: warp w owns columns group*"<<columns<<"+w*4+j, j=0..3.\n"
            <<"For R="<<a.rank<<", there are ceil(R/"<<columns<<")="<<(a.rank+columns-1)/columns<<" column groups.\n"
            <<"Lane l visits tokens l,l+32,... within the actual chunk, reusing each probability for four lane-private F32 sums.\n"
            <<"No shared cache tile or latent-accumulation barrier exists. Every lane executes all five shuffle rounds for all four sums;\n"
            <<"lane zero writes valid columns. Column-tail predicates guard reads/stores, never collective participation.\n"
            <<"A width-64 lane visits l and l+32; a width-32 lane visits l once on a full chunk.\n"
            <<"Only tuple [m_c,d_c,z_c[0..R-1]] is written at (c*"<<rows<<"+row)*(R+2). z_c is locally normalized.\n"
            <<"Finite-max fallback is zero and nonpositive denominator fallback is one; empty chunks have d_c=0,z_c=0.\n"
            <<"The separate 128-thread merge computes m=max m_c, t_c=d_c*exp(m_c-m) for nonempty chunks, D=sum t_c,\n"
            <<"z=sum (t_c/D)*z_c and LSE=m+log(D). Select zero mass before exp for empty chunks to avoid 0*infinity.\n"
            <<"All-empty rows return zero latent and -infinity LSE. Query/value projection and output stages are retained.\n"
            <<"COST TRADEOFF: fewer padding exp/shared slots and fewer participating threads; at 32, cross-warp folds disappear.\n"
            <<"Fewer warps also mean more column groups (R=16: two groups at 64, four at 32, versus one at 128).\n"
            <<"Tuple traffic, chunk count and merge work are unchanged at a fixed width. Less local parallelism can offset the savings.\n"
            <<"This AOT policy permits F32 regrouping; verify against the reference. No measured GPU speedup is asserted.\n";
        text<<"COLUMN-GROUP WALKTHROUGH: zero the four lane-private sums at the start of each group, traverse its tokens,\n"
            <<"reduce each sum to lane zero, store the four valid columns, then advance the group. Shared probabilities stay immutable.\n";
        if(a.rank==16) {
            if(warps==2) text<<"R=16, block=64: group 0 gives warp 0 columns 0..3 and warp 1 columns 4..7;\n"
                <<"group 1 gives warp 0 columns 8..11 and warp 1 columns 12..15. Both warps participate in both groups.\n";
            else text<<"R=16, block=32: warp 0 handles group 0 columns 0..3, group 1 columns 4..7,\n"
                <<"group 2 columns 8..11, and group 3 columns 12..15, in sequence.\n";
            text<<"Each group has four five-round warp trees: 20 shuffle calls per warp/group.\n"
                <<"R=16 latent work per block is 4 warps*1 group*20 = 2 warps*2 groups*20 = 1 warp*4 groups*20 = 80 warp-level shuffle calls.\n"
                <<"Only the distribution changes: each warp executes 20, 40 or 80 such calls at block sizes 128,64 or32.\n"
                <<"This count excludes max/denominator trees and is not a cycle count. Fewer resident work warps can lengthen the critical path.\n";
        }
        UINT64 tail=a.length-UINT64(k.chunks-1)*k.split_width;
        text<<"TAIL WALKTHROUGH: final chunk begins at "<<UINT64(k.chunks-1)*k.split_width<<", contains "<<tail
            <<" real token(s), and has "<<k.split_width-tail<<" neutral score slots per row.\n"
            <<"Neutral lanes skip source reads but remain in every shuffle/fence. Column tails similarly guard memory/stores without changing participation.\n";
        if(tail==1) text<<"For an allowed finite one-token tail, local d_c=1 and z_c=that cache vector; the global merge weight still depends on its score.\n"
            <<"If that token is masked, d_c=0 and z_c=0. A one-token chunk still executes the full synchronization and column-group structure.\n";
        text<<"PARALLELISM TRADEOFF: the selected chunk count and CTA grid do not grow merely because the block shrinks.\n"
            <<"At the same fixed width there are fewer launched warps, more sequential column groups, and the same compact tuple/merge traffic.\n"
            <<"A supplied four-warp register allocation quantum may keep allocated registers per CTA unchanged for one/two-warp blocks.\n"
            <<"CTA-goal coverage is not achieved warp occupancy. Compare complete-call GPU time with the retained 128-thread candidate.\n";
        ExplainDependencies(k,text);Explain("STAGE SEMANTICS",text.str());
    }
    void ExplainAttentionStage(const VHO_TENSOR_KERNEL &k) {
        if(k.threads<128) {ExplainNarrowBlockStage(k);return;}
        const VHO_TENSOR_ATTENTION &a=_plan.attention;std::ostringstream text;
        UINT64 rows=a.batch*a.heads,capacity=(((k.mapping==3?k.split_width:k.chunks)+127)/128)*128;
        text<<"FUSED ATTENTION: "<<(k.mapping==3?(k.stage==1?"chunk-local attention":"complete short-context attention"):"stable rescaled chunk merge")<<".\n";
        text<<"Launch "<<VHO_tensor_grid(_p,k)<<" CTAs of 128 threads; selected shared storage "<<VHO_tensor_shared_bytes(_p,k)<<" bytes per CTA.\n";
        text<<"Rows = B*H = "<<rows<<", tokens L="<<a.length<<", latent R="<<a.rank<<", RoPE width="<<a.rope<<".\n";
        if(k.mapping==3) {
            text<<"CTA row = block modulo "<<rows<<"; chunk = block / "<<rows<<". Each thread visits local token tid,tid+128,... .\n";
            text<<"For every allowed token, compute score = scale * (dot(projected_query,latent_cache[token])\n"
                <<"+ dot(rope_query,rope_cache[token])). Both contractions retain ascending F32 inner loops.\n"
                <<"Mask-false tokens and padding use -infinity, with guarded source reads. Source scale bits are 0x"<<std::hex<<a.scale_bits<<std::dec<<".\n";
            text<<"Chunk interval is [c*"<<k.split_width<<",min((c+1)*"<<k.split_width<<","<<a.length<<")); "<<k.chunks<<" chunk(s).\n";
            UINT64 groups=(a.rank+3)/4,full_barriers=_p.register_latent?5:5+2*groups*((k.split_width+31)/32);
            text<<"CTA WALKTHROUGH: four warps first cooperate across TOKENS for the max and denominator.\n"
                <<"Each warp uses shuffle offsets 16,8,4,2,1; lane zero publishes its partial, then thread zero folds four warp partials.\n"
                <<(_p.register_latent?"After normalization, each warp reduces TOKENS for four private latent columns.\n":"After normalization, ownership changes: a complete warp reduces TOKENS for one latent COLUMN.\n")
                <<"The two max-fold barriers, two denominator-fold barriers and probability-publication barrier contribute five CTA barriers.\n";
            if(!_p.register_latent) text<<"With "<<groups<<" column groups, a full segment adds 2*"<<groups<<"*ceil("<<k.split_width
                <<"/32) cache-tile barriers, for "<<full_barriers<<" CTA barrier calls along each thread's path.\n";
            if(k.stage==1) {
                UINT64 tail=a.length-UINT64(k.chunks-1)*k.split_width;
                text<<"The final chunk starts at "<<UINT64(k.chunks-1)*k.split_width<<" and has "<<tail<<" real token(s), but still uses capacity "<<capacity
                    <<" and all collectives; its actual CTA barrier count is "<<(_p.register_latent?5:5+2*groups*((tail+31)/32))<<".\n";
            } else text<<"This direct CTA covers the whole row, so its local softmax is already the global softmax; no rescaling merge follows.\n";
            text<<"Score storage is shared["<<capacity<<"], overwritten in place with stable exponentials, then normalized local probabilities.\n"
                <<"Four full-warp max trees publish four shared partials; thread zero folds those four and publishes the maximum.\n"
                <<"The same CTA reduction computes d_c=sum exp(score-m_c). Normalize probabilities once in shared memory.\n"
                <<"A nonfinite maximum uses the source zero fallback; a nonpositive denominator uses one. Fully masked rows produce zero latent and LSE -infinity.\n";
            if(!_p.register_latent) text<<"Warp w owns latent columns group*4+w. Stage 32x4 cache tiles using the selected XOR bank layout;\n"
                <<"barriers publish each cache tile and release it before reuse. Multiply the shared local probabilities\n"
                <<"by staged cache values, then use five shuffle-down rounds to obtain each column's local latent value.\n";
            if(k.stage==1) text<<"Only tuple [m_c,d_c,z_c[0..R-1]] is written globally at (c*"<<rows<<"+row)*(R+2).\n"
                <<"z_c is already normalized by d_c; it is not an unnormalized numerator. The merge must rescale this local distribution.\n";
            else text<<"Write only [z[0..R-1],LSE] at row*(R+1). No attention partial scratch is needed.\n";
            text<<"EXPECTED BENEFIT: latent/RoPE score contractions, mask, softmax and weighted latent share one CTA.\n"
                <<"Each score and exponential is evaluated once per row/token, and its probability is reused across all R columns.\n"
                <<"The two global score tensors and separate max/denominator launches are eliminated. Cache values are read\n"
                <<"for score and weighted accumulation; this does not eliminate all cache traffic or projection work.\n";
        } else {
            text<<"MERGE WALKTHROUGH: each z_c is a local expectation, not a raw weighted sum or one global probability.\n"
                <<"For finite nonempty chunks, d_c=sum_{t in c} exp(s_t-m_c) and\n"
                <<"z_c=(1/d_c)*sum_{t in c} exp(s_t-m_c)*cache[t]. d_c is not generally the token count.\n"
                <<"Changing the exponential origin from m_c to m multiplies every unnormalized weight in chunk c by exp(m_c-m).\n"
                <<"Its mass on that common scale is t_c=d_c*exp(m_c-m); D=sum_c t_c. Therefore\n"
                <<"sum_t exp(s_t-m)*cache[t] = sum_c t_c*z_c, and dividing by D gives z=sum_c (t_c/D)*z_c.\n"
                <<"The unshifted partition is exp(m)*D, so its log is m+log(D), without ever evaluating exp(m).\n"
                <<"NUMERICAL EXAMPLE ONLY: two scalar chunks (m0=0,d0=1,z0=2) and (m1=log(2),d1=1,z1=8)\n"
                <<"give m=log(2), t0=1/2, t1=1, D=3/2, alpha0=1/3, alpha1=2/3, z=6 and LSE=log(3).\n"
                <<"A plain sum gives 10 and an unweighted average gives 5; neither reconstructs the global softmax.\n";
            text<<"For max and D, thread tid owns chunks tid,tid+128,...; four warp summaries are folded by thread zero.\n";
            if(k.chunks==33) text<<"Here tids 0..32 each begin with one chunk; other threads are neutral for those CTA folds.\n"
                <<"For each latent-column warp, lane zero instead accumulates chunks 0 and 32; lanes 1..31 take their own chunk before the tree.\n";
            text<<"The normalized alpha weights occupy shared["<<capacity<<"]. Five uniform CTA barriers publish max, D and alpha;\n"
                <<"the column-wise chunk trees need register shuffles but no additional CTA barriers.\n";
            text<<"An allowed finite one-token tail has d_c=1 and z_c=that token's cache vector, yet its global alpha\n"
                <<"depends on its score relative to all other chunks; it is not automatically 1/chunk_count.\n"
                <<"An empty chunk has d_c=0 and z_c=0; select t_c=0 before forming the exponential product.\n"
                <<"This matters when m is very negative: using its safe local m_c=0 could otherwise form 0*exp(0-m)=0*infinity.\n";
            text<<"Read only compact chunk-major [m_c,d_c,z_c] tuples; no query, score or probability tensor is read.\n"
                <<"Compute m=max_c m_c; D=sum_c d_c*exp(m_c-m). For empty chunks d_c=0, take zero before exp.\n"
                <<"Then alpha_c=d_c*exp(m_c-m)/D and z=sum_c alpha_c*z_c; LSE=m+log(D).\n"
                <<"Use the source safe maximum and denominator fallbacks; all-empty chunks give zero output and LSE -infinity.\n"
                <<"The rescaling is essential because each z_c used its own chunk-local softmax. A plain sum or average is incorrect.\n"
                <<"CTA reductions combine maxima and D; shared normalized alpha weights are reused by all latent columns.\n"
                <<(_p.register_latent?"Each warp maintains four private column sums; each uses offsets 16,8,4,2,1.\n":"One full warp per latent column reduces chunk contributions with offsets 16,8,4,2,1.\n")
                <<"Write only [z[0..R-1],LSE] at row*(R+1), consumed by the retained value projection and output stores.\n";
        }
        if(_p.register_latent) {
            text<<"REGISTER ACCUMULATION: warp w owns columns group*16+w*4+j, j=0..3.\n"
                <<"Lane l visits local tokens (or chunks) l,l+32,... and reads each shared normalized weight once for four columns.\n"
                <<"Four F32 register sums are lane-private. Cache or partial-tuple values are read directly, with column/tail bounds.\n"
                <<"No shared cache tile exists and no CTA barriers occur in latent accumulation. The remaining five CTA barriers\n"
                <<"publish four-warp max summaries, max, denominator summaries, denominator, and normalized weights.\n"
                <<"After the fifth barrier the shared weights are immutable; only lane-private state changes until full-warp shuffles.\n"
                <<"Every lane executes all five shuffle rounds for all four sums, including inactive column tails; lane zero writes valid columns.\n"
                <<"For the same partition, each column keeps its lane-strided F32 additions and tree; changing chunk width can change rounding.\n"
                <<"Four columns expose independent arithmetic but may increase registers or sacrifice cache-tile locality; verify resources and GPU timing.\n";
            text<<"WHY TILE BARRIERS DISAPPEAR: the old cooperative cache tile was written and read by different warps.\n"
                <<"Its first barrier prevented reads before publication; its second prevented overwriting values still in use.\n"
                <<"Direct cache/tuple reads now feed only the owning lane's four sums. There is no shared cache tile to publish or overwrite.\n"
                <<"Removing those dependencies makes both tile barriers unnecessary; merely deleting them from the staged path would be unsafe.\n"
                <<"BARRIER 1: all warp maxima are visible before thread zero reads them.\n"
                <<"BARRIER 2: thread zero's maximum is visible before any thread computes exponential weights.\n"
                <<"BARRIER 3: all warp denominator sums are visible before thread zero reads them.\n"
                <<"BARRIER 4: thread zero's denominator is visible before any thread normalizes its weights.\n"
                <<"BARRIER 5: all normalized weights are visible before any warp accumulates latent columns.\n"
                <<"These are five dynamic CTA barrier calls per participating thread, not five different hardware barrier IDs.\n"
                <<"Register accumulation still needs warp communication: shuffle rounds reduce lane-private sums without shared cache storage.\n"
                <<"Reading a weight once means once per lane/token within its four-column group; the other warps read it for their own columns.\n";
            if(a.rank==16) text<<"For this R=16 selection, warp 0 owns columns 0..3, warp 1 owns 4..7, warp 2 owns 8..11, warp 3 owns 12..15.\n";
            if(k.mapping==3 && k.stage==0 && a.length==127)
                text<<"SHORT L=127 EXAMPLE: lane 0 accumulates tokens 0,32,64,96 into four private sums.\n"
                    <<"Lane 31 visits 31,63,95; token 127 is out of range and is never read. Score padding still participates neutrally in CTA folds.\n"
                    <<"After the five shuffle rounds, lane zero writes its warp's four completed latent columns.\n";
            if(k.mapping==4 && k.chunks==65)
                text<<"LONG C=65 MERGE EXAMPLE: tids 0..64 each supply one chunk to max/D; tids 65..127 are neutral.\n"
                    <<"For each column group, lane 0 accumulates chunks 0,32,64; lane 1 takes 1,33; lane 31 takes 31,63.\n"
                    <<"Each of the four column sums then uses the same full-warp tree, and lane zero writes the final columns.\n";
            if(k.mapping==3) text<<"At this partition the staged path would execute "<<5+2*((a.rank+3)/4)*((k.split_width+31)/32)
                <<" barriers per full segment; this path executes five, including every tail.\n";
            if(k.mapping==3 && k.stage==1) {
                UINT64 old_width=2*k.split_width,old_chunks=(a.length+old_width-1)/old_width;
                text<<"TWO INDEPENDENT CHANGES: register accumulation removes tile barriers at any admitted chunk width.\n"
                    <<"Reducing the chunk width separately shortens local token loops and increases independently schedulable CTAs.\n"
                    <<"It also repeats fixed five-barrier CTA work for more chunks; smaller chunks alone do not eliminate synchronization.\n";
                if(old_width<=4096 && old_chunks>1) {
                    UINT64 old_span=old_width<a.length?old_width:a.length;
                    text<<"Comparison with a staged width-"<<old_width<<" candidate: "<<old_chunks<<" -> "<<k.chunks
                        <<" chunks, "<<rows*old_chunks<<" -> "<<rows*k.chunks<<" local CTAs.\n";
                    text<<"At fixed width "<<old_width<<", register ownership alone changes full-chunk barriers from "
                        <<5+2*((a.rank+3)/4)*((old_span+31)/32)<<" to five. Selected width "<<k.split_width<<" also uses five.\n";
                    text<<"Partial tuple allocation "<<4*rows*old_chunks*(a.rank+2)<<" -> "<<4*rows*k.chunks*(a.rank+2)
                        <<" bytes; "<<_plan.kernels.size()<<" selected launches. More merge work accompanies the larger grid.\n";
                }
                text<<"The partial attention scratch is chunks*B*H*(R+2) F32 values: more chunks mean more tuple stores, loads and rescaling work.\n"
                    <<"The separate merge still has one CTA per row; its chunk scan and vector reductions grow with the chunk count.\n"
                    <<"For fully populated chunks the token count and score-dot work over the whole sequence are unchanged.\n"
                    <<"The one-token tail, when present, still pays five barriers and full-warp trees; it has little useful work to amortize them.\n"
                    <<"Smaller chunks can fill idle SMs or expose more scheduling work, but after saturation the extra merge/fixed work can dominate.\n"
                    <<"Register pressure and direct-load cache behavior must be measured separately from shared-memory savings.\n"
                    <<"Use whole-call latency, including unchanged projections and launches, to choose between retained AOT candidates.\n";
            }
        }
        if(_p.auto_chunks && _p.chunk_policy==2 && (_p.split_width==32 || _p.split_width==64)) {
            if(k.mapping==3) {
                text<<"NARROW LOCAL OWNERSHIP: width "<<_p.split_width<<" changes token span, not the 128-thread launch or four-column register ownership.\n";
                if(_p.split_width==64) text<<"On a full chunk each latent lane visits local tokens lane and lane+32; lane 0 takes 0,32 and lane 31 takes 31,63.\n";
                else text<<"On a full chunk each latent lane visits exactly local token lane (0..31), then its four sums enter the full-warp trees.\n";
                text<<"The final tail uses bounds checks, but every lane still executes the same collective rounds and all five CTA barriers.\n";
            } else {
                text<<"NARROW MERGE OWNERSHIP: statistics thread t visits chunks t,t+128,...; tid 0 visits "
                    <<(k.chunks+127)/128<<" real chunk(s). Vector lane l visits l,l+32,...; lane 0 visits "
                    <<(k.chunks+31)/32<<" chunk(s).\n";
                if(k.chunks==129) text<<"For C=129, tid 0 folds chunks 0,128; other tids take one. Vector lane 0 takes 0,32,64,96,128; other lanes take four.\n";
                if(k.chunks==257) text<<"For C=257, tid 0 folds 0,128,256; other tids take two. Vector lane 0 takes 0,32,64,96,128,160,192,224,256; other lanes take eight.\n";
                text<<"The native merge allocates "<<capacity<<" weight slots but processes only "<<k.chunks
                    <<". Unwritten allocation-padding slots are never read, normalized or exponentiated.\n"
                    <<"Local score padding is different: those neutral slots are explicitly written and visited by local exp/normalization loops.\n"
                    <<"The same zero-mass guard precedes rescaling: empty chunks contribute zero mass; an all-empty row returns zero latent output and LSE -infinity.\n";
            }
        }
        text<<"All CTA barriers and shuffle rounds are uniform across the full 128-thread block, including token/column tails.\n"
            <<"This explicit AOT option permits F32 regrouping and chunk-local normalization; compare with the FP64 oracle, not old F32 bytes.\n"
            <<"Shared-memory use, barriers, register pressure, small grids and score dot work can limit speed; no measured GPU speedup is asserted.\n";
        ExplainDependencies(k,text);Explain("STAGE SEMANTICS",text.str());
    }
    /* Render formulas, tails, work ownership and numerical order directly
     * from the supplied plan; these facts do not select a new schedule. */
    void ExplainStage(const VHO_TENSOR_KERNEL &k) {
        if(k.mapping>=3) {ExplainAttentionStage(k);return;}
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
    /* Explain source-level work from the already selected ledger; no search. */
    void ExplainNarrowCosts(std::ostringstream &text,const VHO_TENSOR_CHUNK_CANDIDATE &winner) {
        if(_p.chunk_policy==6) {
            text<<"WARP-OWNED COST WALKTHROUGH: width64 owns tokens within each warp and computes all columns, then publishes two tuples at one CTA barrier.\n"
                <<"Shared storage is8*(R+2) bytes; no shared probabilities. Two warp-local softmax reductions and a stable two-tuple merge change F32 association.\n"
                <<"Both warps reduce every column, doubling column shuffle work; extra rescaling/broadcasts may offset synchronization savings. No latency claim follows from barrier count.\n"
                <<"Width32 remains register-only; direct/wider/128-thread inter-chunk merge retain prior resource and ordering contracts.\n";
            return;
        }
        if(_p.chunk_policy==5) {
            text<<"TWO-WARP COST WALKTHROUGH: width64 retains private scores/exp and register final statistics, with272 shared bytes and three CTA barrier phases.\n"
                <<"Each warp publishes max and denominator partials into disjoint slots, folds both and broadcasts within the warp.\n"
                <<"Normalized probabilities still cross warp boundaries, requiring the third CTA barrier. Width32 retains zero shared bytes and zero memory fences.\n"
                <<"The128-thread merge and wider local kernels retain their prior resource/ordering contracts. Coverage is not measured occupancy or latency.\n";
            return;
        }
        if(_p.chunk_policy==4) {
            text<<"WARP-REGISTER COST WALKTHROUGH: width32 keeps one score/exponential/probability per lane and two broadcast statistics.\n"
                <<"Its selected shared footprint is zero, with zero memory fences and two full-mask register broadcasts per local CTA.\n"
                <<"Width64 retains two warps, shared storage288 bytes and five CTA barriers. Other mappings retain their existing storage and synchronization.\n"
                <<"Changing register demand can offset shared-memory savings; profile allocation quanta still apply and coverage is not measured occupancy.\n";
            return;
        }
        if(_p.chunk_policy==3 && (winner.width==32 || winner.width==64)) {
            const VHO_TENSOR_ATTENTION &a=_plan.attention;UINT64 real=a.batch*a.heads*a.length;
            text<<"NARROW-KERNEL COST WALKTHROUGH: selected width "<<winner.width<<", block "<<winner.threads<<" threads.\n";
            for(size_t i=0;i<_plan.chunk_selection.candidates.size();++i) {
                const VHO_TENSOR_CHUNK_CANDIDATE &c=_plan.chunk_selection.candidates[i];
                if(!c.width || c.width>128) continue;
                UINT64 slots=c.ctas*((c.width+c.threads-1)/c.threads)*c.threads;
                text<<"LOCAL COST width="<<c.width<<" block_x="<<c.threads<<" chunks="<<c.chunks<<" CTAs="<<c.ctas
                    <<" shared_exp_slots="<<slots<<" padding_slots="<<slots-real<<" local_shared_bytes="<<c.local_shared
                    <<" local_CTA_barrier_phases="<<(c.threads==32?0:5*c.ctas)
                    <<" local_warp_barrier_phases="<<(c.threads==32?3*c.ctas:0)
                    <<" partial_bytes="<<c.scratch<<" merge_shared_bytes="<<c.merge_shared<<" status="<<c.reason<<".\n";
                if(c.width==32 || c.width==64) {
                    UINT64 old_slots=c.ctas*128;
                    text<<"PADDING COMPARISON at fixed width="<<c.width<<": old 128-thread capacity visited "<<old_slots
                        <<" exp slots with "<<old_slots-real<<" geometric padding slots; selected block visits "<<slots
                        <<" with "<<slots-real<<" padding slots. Useful row/token positions remain "<<real<<".\n";
                }
            }
            text<<"These are selected source-loop slots. Mask-false tokens are separate from geometric padding; compiler folding/predication can change executed cost.\n";
            text<<"Only geometric tail padding remains at widths 32/64; masked positions are separate. Counts describe source work, not time.\n"
                <<"The 128-thread merge still visits every real tuple and ignores unwritten allocation padding.\n"
                <<"Coverage ranking still prefers fewer chunks after coverage ties. Smaller blocks do not remove scratch/merge costs.\n"
                <<"Fewer warps require more four-column groups; profile actual occupancy and whole-call latency before claiming a speedup.\n";
            return;
        }
        if(_p.chunk_policy!=2 || (winner.width!=32 && winner.width!=64)) return;
        const VHO_TENSOR_ATTENTION &a=_plan.attention;UINT64 rows=a.batch*a.heads;
        UINT64 capacity=((winner.width+127)/128)*128;
        text<<"NARROW-KERNEL COST WALKTHROUGH: selected width "<<winner.width<<" tokens; blocks still contain 128 threads.\n";
        text<<"On a full chunk, tids 0.."<<winner.width-1<<" score real tokens; tids "<<winner.width
            <<"..127 write neutral -infinity without reading source cache/query data for those padding tokens.\n";
        text<<"All 128 threads still enter the four-warp max/denominator folds and the five CTA barriers.\n"
            <<"The shared array is then reused for exponentials and normalized probabilities. Its capacity is "
            <<capacity<<" even though a full chunk has only "<<winner.width<<" real tokens.\n";
        text<<"The local exp/normalization loops visit every shared slot, including neutral padding. Real masked positions can also be neutral.\n"
            <<"Padding avoids source score-dot reads, but still carries shared-array/reduction work in the selected source.\n";
        text<<"COST TABLE: logical row/token positions = "<<rows<<"*"<<a.length<<"="<<rows*a.length
            <<". Padding counts are unused shared slots beyond each chunk's actual token span, independent of masks. A corresponding global index may belong to another chunk.\n";
        const UINT64 widths[]={128,64,32};
        for(UINT32 w=0;w<3;++w) for(size_t i=0;i<_plan.chunk_selection.candidates.size();++i) {
            const VHO_TENSOR_CHUNK_CANDIDATE &c=_plan.chunk_selection.candidates[i];
            if(c.width!=widths[w]) continue;
            UINT64 slots=c.ctas*((c.width+127)/128)*128;
            text<<"LOCAL COST width="<<c.width<<" chunks="<<c.chunks<<" CTAs="<<c.ctas
                <<" shared_exp_slots="<<slots<<" padding_slots="<<slots-rows*a.length
                <<" local_CTA_barrier_phases="<<5*c.ctas<<" partial_bytes="<<c.scratch
                <<" merge_shared_bytes="<<c.merge_shared<<" status="<<c.reason<<".\n";
        }
        text<<"These are source-loop slots and CTA rendezvous counts, not measured instructions, cycles or a sequential sum of GPU latency.\n"
            <<"CTAs can overlap; compiler folding/predication can change executed padding cost. Allowed-token score dots still cover the same context.\n"
            <<"A normalized weight is reused by four lane-private column sums. All four warps do useful latent-column work for R=16,\n"
            <<"even though only one/two warps score real tokens on a full width-32/64 chunk.\n";
        text<<"MERGE COST: "<<winner.chunks<<" tuples per row, each holding R+2="<<a.rank+2
            <<" F32 values; partial storage "<<winner.scratch<<" bytes. The merge still launches only "<<rows<<" row CTAs.\n";
        text<<"More tuples increase global tuple stores/loads, chunk-rescaling exponentials and vector contributions. They do not add a merge launch.\n"
            <<"Both narrow variants retain the six-stage fused long-context plan; unchanged projections and output stores remain part of whole-call time.\n"
            <<"Goal coverage can improve while whole-call latency worsens; select measured performance separately from this coverage heuristic.\n";
    }
    /* Read the sealed phase result; printing never changes width or resources. */
    void ExplainAutoChunks(std::ostringstream &text) {
        if(!_p.auto_chunks) return;
        const VHO_TENSOR_CHUNK_SELECTION &s=_plan.chunk_selection;
        if(s.candidates.empty() || s.width!=_p.split_width)
            throw std::runtime_error("stale automatic chunk walkthrough");
        const VHO_TENSOR_GPU_PROFILE &g=_p.gpu;
        text<<"AUTOMATIC COMPILE-TIME CHUNKS: "<<VHO_tensor_chunk_policy_name(_p)<<"; profile "<<g.id<<", architecture "<<g.architecture<<".\n"
            <<"Explicit device/partition SM count "<<g.sms<<"; estimated registers/thread "<<g.registers_per_thread
            <<"; requested local CTAs/SM "<<g.target_ctas_per_sm<<"; partial scratch budget "<<g.scratch_budget<<" bytes.\n";
        text<<"Capacities are supplied planning inputs; register demand is an estimate, not a measured occupancy or latency promise.\n"
            <<"The existing occupancy service rounds register/shared allocations and applies block, warp, thread, register and shared limits.\n"
            <<"Each candidate must pass native admission, all-stage resource fit and the partial-scratch budget, including the merge.\n"
            <<"Coverage goal = SMs * min(requested CTAs/SM, estimated resident local CTAs/SM).\n"
            <<"Rank by min(local CTAs,goal)/goal, then fewer chunks to limit scratch, merge and fixed CTA work.\n"
            <<"If a direct plan fits and L<=256, keep it to avoid short-row split/merge overhead. Candidate split widths are "<<(_p.chunk_policy>=2?"32":"128")<<"..4096 powers of two.\n";
        text<<"Selected width "<<s.width<<(s.width?"":" (zero means direct whole row)")
            <<"; short-direct rule "<<(s.short_direct?"applied":"not applied")<<". No runtime device query or JIT is required.\n";
        for(size_t i=0;i<s.candidates.size();++i) {
            const VHO_TENSOR_CHUNK_CANDIDATE &c=s.candidates[i];
            text<<"CANDIDATE width="<<c.width<<" chunks="<<c.chunks<<" CTAs="<<c.ctas
                <<" shared(local/merge)="<<c.local_shared<<"/"<<c.merge_shared
                <<" scratch="<<c.scratch<<" estimated_resident="<<c.resident
                <<" coverage="<<c.coverage<<"/"<<c.goal<<" status="<<c.reason;
            if(c.eligible && c.width==s.width) text<<" SELECTED";
            text<<".\n";
        }
        const VHO_TENSOR_ATTENTION &a=_plan.attention;
        const VHO_TENSOR_CHUNK_CANDIDATE *winner=NULL;size_t eligible=0;
        for(size_t i=0;i<s.candidates.size();++i) {
            const VHO_TENSOR_CHUNK_CANDIDATE &c=s.candidates[i];
            if(c.eligible) ++eligible;
            if(c.eligible && c.width==s.width) winner=&c;
        }
        if(!winner) throw std::runtime_error("missing selected automatic candidate");
        text<<"WINNER WALKTHROUGH: B*H="<<a.batch*a.heads<<", L="<<a.length<<", R="<<a.rank<<".\n";
        text<<"The selected local resource estimate permits "<<winner->resident<<" resident CTAs/SM; the profile requests "
            <<g.target_ctas_per_sm<<". Thus goal="<<g.sms<<"*min("<<g.target_ctas_per_sm<<","<<winner->resident
            <<")="<<winner->goal<<" CTAs.\n";
        text<<"Maximum estimated residency and requested CTA population have different roles: only their minimum sets this policy goal.\n"
            <<"Coverage is a count of available CTAs relative to that goal. It is not measured GPU utilization or achieved warp occupancy.\n";
        text<<eligible<<" of "<<s.candidates.size()<<" enumerated candidates pass all capacity/resource/scratch checks";
        if(eligible==s.candidates.size()) text<<"; the winner comparisons here are ranking decisions, not resource rejections";
        text<<".\n";
        if(s.short_direct) {
            text<<"WHY SHORT STAYS DIRECT: L="<<a.length<<" is within the <=256 short-row rule and direct attention fits.\n"
                <<"Selected width=0 is a sentinel for span L="<<a.length<<", not a zero-length chunk. One CTA owns each of "
                <<a.batch*a.heads<<" rows; there is no partial scratch or merge launch.\n";
            UINT64 minimum=_p.chunk_policy>=2?32:128;
            if(a.length<=minimum) text<<"All split widths in this policy are >="<<minimum<<" and >=this L; duplicate whole-row split plans are omitted.\n";
            else if(_p.chunk_policy>=2) text<<"Widths 32/64 can now form short-row split plans, but the legal direct plan still has priority under the short-row rule.\n";
            if(winner->coverage<winner->goal) text<<"The small grid intentionally remains below its "<<winner->goal
                <<"-CTA target to avoid short-row splitting/merging overhead. This rule is heuristic, not a timing measurement.\n";
            else text<<"The direct plan already reaches the requested coverage without short-row splitting/merging overhead.\n";
        } else if(winner->width) {
            UINT64 tail=a.length-(winner->chunks-1)*winner->width;
            text<<"Selected C=ceil("<<a.length<<"/"<<winner->width<<")="<<winner->chunks
                <<"; local CTAs="<<a.batch*a.heads<<"*"<<winner->chunks<<"="<<winner->ctas
                <<"; the last chunk has "<<tail<<" token(s).\n";
            if(a.length>4096) text<<"Direct attention is absent because L exceeds the admitted direct limit 4096; this is a native capacity rule.\n";
            if(winner->coverage<winner->goal)
                text<<"WHY THIS WIDTH WINS: no enumerated feasible candidate reaches full coverage. The selected "
                    <<winner->coverage<<"/"<<winner->goal<<" is the greatest available fraction.\n"
                    <<"Width "<<(_p.chunk_policy>=2?32:128)<<" is the smallest split candidate in "<<VHO_tensor_chunk_policy_name(_p)<<"; narrower widths are outside this search, so no optimum over them is claimed.\n";
            else text<<"WHY THIS WIDTH WINS: selected coverage is "<<winner->coverage<<"/"<<winner->goal
                    <<". Extra CTAs beyond the goal receive no further coverage credit; fewer chunks then reduce scratch and merge/fixed work.\n";
            text<<"Each chunk writes R+2="<<a.rank+2<<" F32 scalars per row. Total partial scratch = "
                <<winner->chunks<<"*"<<a.batch*a.heads<<"*"<<a.rank+2<<"*4="<<winner->scratch<<" bytes.\n";
        } else text<<"WHY DIRECT WINS: whole-row attention already provides the selected coverage with one chunk and zero partial scratch.\n";
        for(size_t i=0;i<s.candidates.size();++i) {
            const VHO_TENSOR_CHUNK_CANDIDATE &c=s.candidates[i];
            if(&c==winner) continue;
            text<<"ALTERNATIVE width="<<c.width<<": ";
            if(!c.eligible) text<<"excluded by "<<c.reason;
            else if(s.short_direct) text<<"the legal direct plan takes priority under the short-row rule";
            else if(c.coverage*winner->goal<winner->coverage*c.goal)
                text<<"coverage "<<c.coverage<<"/"<<c.goal<<" is below the selected "<<winner->coverage<<"/"<<winner->goal
                    <<" even though this candidate passes resource/scratch checks";
            else if(c.chunks>winner->chunks)
                text<<"coverage ties at "<<c.coverage<<"/"<<c.goal<<", but "<<c.chunks<<" chunks and "<<c.scratch
                    <<" scratch bytes exceed the selected "<<winner->chunks<<" chunks and "<<winner->scratch<<" bytes";
            else text<<"coverage/chunk-count tie uses the policy's stable width order";
            text<<".\n";
        }
        if(_p.chunk_policy==2) text<<"SMALL-CHUNK EXTENSION: coverage-v2 adds 32 and 64 to the legacy coverage-v1 candidate set.\n"
            <<"Blocks remain 128 threads and score capacity still rounds up to 128 slots. On full chunks, width 64 has two useful scoring warps; width 32 has one.\n"
            <<"Every warp still participates in max/denominator folds and full-mask trees, with neutral padding and guarded source reads.\n"
            <<"More chunks can reach coverage sooner, but repeat five-barrier CTA work, padded exponential work, tuple traffic and merge contributions.\n"
            <<"Once coverage ties, fewer chunks retain priority. Width 32 therefore does not automatically beat width 64.\n"
            <<"Merge shared storage follows the rounded chunk count, including counts above 128/256; reject a candidate if its merge no longer fits.\n"
            <<"Legacy v1 recipes remain bound to their old candidate set; --chunk-policy coverage-v1 explicitly reproduces that policy.\n";
        if(_p.chunk_policy==3) text<<"SMALL-BLOCK EXTENSION: coverage-v3 keeps the expanded width set and selects 64/32 threads for split local width 64/32.\n"
            <<"Other local kernels, the merge and projections retain 128-thread blocks. Score capacity rounds to the selected local block size.\n"
            <<"Resource admission uses each kernel's actual thread count; profile register/shared allocation quanta remain authoritative.\n"
            <<"A one-warp local kernel uses three full-mask warp fences, while a two-warp local kernel retains five CTA barriers.\n"
            <<"Legacy coverage-v1/v2 recipes keep 128-thread geometry; the policy and candidate block_x ledger seal the new choice.\n";
        if(_p.chunk_policy==4) text<<"SINGLE-WARP REGISTER EXTENSION: coverage-v4 retains narrow block geometry and selects register-broadcast statistics only for split local width32.\n"
            <<"The per-lane probability is immutable and private across column groups; no shared publication or memory fence is required.\n"
            <<"Candidate statistics and native metadata seal this ownership proof; coverage-v3 reproduces the prior three-fence realization.\n";
        if(_p.chunk_policy==6) text<<"WARP-OWNED EXTENSION: coverage-v6 selects two independent32-token distributions for local width64, merged after one CTA tuple barrier.\n";
        if(_p.chunk_policy==5) text<<"TWO-WARP BROADCAST EXTENSION: coverage-v5 selects disjoint-summary register broadcasts for split local width64, reducing five CTA barriers to three.\n"
            <<"The one-warp width32 register path is retained unchanged; explicit coverage-v4 reproduces the previous five-barrier width64 realization.\n";
        ExplainNarrowCosts(text,*winner);
        text<<"The profiles describe an offline planning envelope. Architecture sm_90 alone never supplies an enabled SM count.\n"
            <<"Changing SM count changes the coverage target even when per-SM resources, register estimates and numerical semantics are unchanged.\n";
        text<<"This is a resource/coverage heuristic, not a calibrated instruction-stall model or proof of the fastest kernel.\n"
            <<"The selected v6 recipe seals the profile and full ledger. Recompute after shape, mask semantics, resources or policy changes.\n"
            <<"stream-attention with an explicit width overrides automatic selection and emits the manual v5 policy.\n";
    }
    /* Explain interfaces and actual allocations without assuming a model name
     * or claiming device execution/performance evidence that the plan lacks. */
    void ExplainOverview() {
        std::ostringstream text;std::set<UINT32> allocations;UINT64 bytes=0;
        if(_p.search_selected) text<<"TWO-PHASE ESTIMATION: this fixed plan was selected from detached MLA candidates before native WHIRL allocation.\n"
            <<"Search identity "<<_p.search_identity<<"; selected policy "<<_p.chunk_policy<<", chunk width "<<_p.split_width<<".\n"
            <<"Only this selected implementation is materialized; correctness tests apply to it, while other candidates remain estimates.\n"
            <<"The additive-wave cost model and portfolio stopping evidence are retained in the sealed recipe. No GPU optimum or calibrated latency is claimed.\n"
            <<"Lowering and emission consume this plan without per-kernel re-exploration. Existing algorithm, tuple, synchronization and numerical explanations below remain authoritative.\n";
        ExplainAutoChunks(text);
        text<<"These explanations describe the selected logical schedule and its scalar GPU realization.\n";
        text<<"The tensor-level review uses serial helpers; production kernels use the same VHO-selected plan.\n";
        text<<"Fusion, materialization cuts, chunk sizes, and numerical policy were selected before printing.\n";
        text<<"Read stages in order. Fusion enabled: "<<(_p.fuse?"true":"false")<<". With fusion, logical producers are lazy views and may be recomputed.\n";
        text<<"Reshape/transpose views allocate no copied/transposed tensor; exp and pointwise views allocate no result array.\n";
        text<<(_p.fuse_attention?"Score, softmax and latent accumulation share one fused boundary; query/value projections remain materialized.\n":"Matmuls and reductions retain their selected materialization boundaries.\n");
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
        if(_p.fuse_attention) {
            const VHO_TENSOR_ATTENTION &a=_plan.attention;
            text<<"FUSED SCORE/SOFTMAX/LATENT PLAN: "<<_plan.kernels.size()<<" launches; no global latent-score, RoPE-score or probability buffer.\n";
            text<<"Query absorption stays separate, followed by "<<(a.length>_p.split_width && _p.split_width?"chunk-local fused attention and stable tuple merge":"one fused attention stage")
                <<", value projection, output store and LSE store.\n";
            text<<"The shared score array is CTA-local and reused for exponentials and probabilities. Only compact final/partial tuples cross launches.\n";
            text<<"The numerical policy is explicit FP32 local softmax and rescaled merge. New walkthrough wording alone does not change this selected algorithm.\n";
            for(UINT32 i=0;i<_plan.kernels.size();++i) {
                const VHO_TENSOR_KERNEL &k=_plan.kernels[i];
                text<<"  Step "<<i+1<<" / kernel "<<k.id<<": "
                    <<(k.mapping==3?(k.stage==1?"fused chunk attention":"fused complete-row attention"):k.mapping==4?"stable rescaling merge":_p.operations[k.root].name.c_str())
                    <<" -> "<<name(_p,k.output)<<".\n";
            }
            ExplainAttentionPerformance(text,bytes);
        } else if(_p.cooperative) {
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
        if(_plan.attention.enabled && (value==_plan.attention.latent || value==_plan.attention.lse)) {
            _kind=VHO_TENSOR_EXPLAIN_PRODUCER;_value=value;std::ostringstream text;
            text<<name(_p,value)<<" is a logical view of the finalized fused bundle "<<name(_p,_plan.attention.bundle)<<".\n";
            text<<(value==_plan.attention.latent?"Read the first R columns in each row; no latent copy is allocated.":"Read component R (LSE) from each row; do not recompute scores or softmax.")<<"\n";
            Explain("FUSED PRODUCER",text.str());_printed.insert(value);return;
        }
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
                if(_plan.kernels[k].mapping==3 && _plan.kernels[k].stage==1) text<<"Chunk-major [maximum,denominator,normalized latent] tuples; no score buffer.\n";
                else if(_plan.kernels[k].stage==1) text<<"Chunk-major partials: [chunk, flattened root output], indexed c*N+i. The next merge reads this buffer.\n";
                else text<<"This allocation is a retained boundary result; subsequent lazy views only reference it.\n";
                _kind=VHO_TENSOR_EXPLAIN_STORAGE;_value=value;Explain("MATERIALIZED STORAGE",text.str());
            }
        }
        for(UINT32 k=0;k<_plan.kernels.size();++k) {
            const VHO_TENSOR_KERNEL &kernel=_plan.kernels[k];_kernel=kernel.id;_stage=k+1;_value=0;
            _kind=VHO_TENSOR_EXPLAIN_STAGE;ExplainStage(kernel);
            _bound.clear();_printed.clear();_bound.insert(kernel.inputs.begin(),kernel.inputs.end());
            if(kernel.mapping<3 && kernel.stage!=2 && _p.fuse) {
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
