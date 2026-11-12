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
        UINT64 chunks=(a.length+span-1)/span,capacity=((span+127)/128)*128;
        UINT64 old_probability_groups=a.length>=32?(a.rank+3)/4:a.rank;
        text<<"PERFORMANCE COMPARISON BASELINE: the same captured DAG with the previous cooperative score-buffer cuts.\n";
        text<<"Total launches "<<old_launches<<" -> "<<_plan.kernels.size()<<"; global internal allocation bytes "<<old_bytes<<" -> "<<selected_bytes<<".\n";
        text<<"The two score allocations account for "<<8*rows*a.length<<" bytes before fusion and zero afterward.\n"
            <<"Allocation footprint is not total HBM traffic: fusion also removes their stores/reloads, whose cache behavior is workload dependent.\n";
        text<<"Previous nominal token exp evaluations: "<<(1+old_probability_groups)*rows*a.length
            <<" = rows*L*(one denominator pass + "<<old_probability_groups<<" weighted-latent column groups).\n";
        text<<"Fused valid-token exp evaluations: "<<rows*a.length<<". The selected loops actually visit "<<rows*chunks*capacity
            <<" shared exp slots including padding";
        if(chunks>1) text<<", plus at most "<<rows*chunks<<" chunk-rescale exps (empty chunks take zero)";
        text<<".\nThis is source-level work accounting; compiler folding/predication and instruction throughput can change executed cost.\n";
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
    void ExplainAttentionStage(const VHO_TENSOR_KERNEL &k) {
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
            <<"If a direct plan fits and L<=256, keep it to avoid short-row split/merge overhead. Candidate split widths are "<<(_p.chunk_policy==2?"32":"128")<<"..4096 powers of two.\n";
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
            UINT64 minimum=_p.chunk_policy==2?32:128;
            if(a.length<=minimum) text<<"All split widths in this policy are >="<<minimum<<" and >=this L; duplicate whole-row split plans are omitted.\n";
            else if(_p.chunk_policy==2) text<<"Widths 32/64 can now form short-row split plans, but the legal direct plan still has priority under the short-row rule.\n";
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
                    <<"Width "<<(_p.chunk_policy==2?32:128)<<" is the smallest split candidate in "<<VHO_tensor_chunk_policy_name(_p)<<"; narrower widths are outside this search, so no optimum over them is claimed.\n";
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
