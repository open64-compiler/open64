/* Copyright (C) 2026 Open64 Project */
/* LNO-owned H1c detached consumer test against real manifest/estimator/keys.
 * No native initialization, WN/ST/TY service or candidate materialization.
 * Design: doc/AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md. */
#include "dsl_high_observer.h"
#include "dsl_high_binding_keys.h"
#include "../../vho/dsl_fetch_pipeline_opt.h"
#include "../../com/dsl_gpu_profile.h"
#include <stdlib.h>

/* Fixed test clock is compiled only into the dedicated test estimator. */
UINT64 VHO_mla_search_test_clock_us() { return 100; }
/* Preserve each failed production boundary independently of NDEBUG. */
static void require_contract(bool condition,const char *why)
{
    if(!condition) {fprintf(stderr,"H1c MLA: %s\n",why);exit(1);}
}
/* Import the actual service, then copy it into the detached GPU envelope. */
static DSL_GPU_PLAN_SNAPSHOT load(const char *device,const char *file,const char *sha)
{
    DSL_HARDWARE_TARGET_SELECTION target;DSL_GPU_DEVICE_CONTEXT_OWNER owner;
    require_contract(DSL_hardware_target_resolve(NULL,device,&target,NULL),"target");
    require_contract(DSL_gpu_profile_resolve(target,file,sha,"fixture-toolchain-v1",&owner,stderr),"profile");
    return DSL_GPU_PLAN_SNAPSHOT::Capture(owner.Context());
}
/* Explore once per distinct owned input; each invocation owns its lazy cache.
 * Resource/budget unknowns cannot borrow the generic winner. Calibration-only
 * changes invalidate model keys without changing legacy cycle-scenario totals. */
int main(int argc,char **argv)
{
    require_contract(argc==13,"INPUT DEVICE and five PROFILE SHA pairs");
    DSL_FETCH_PLAN_RECORD original,rebuilt;std::memset(&original,0,sizeof(original));
    original.engine=3;original.stage_count=2;original.wait_point=7;original.resources.shared_bytes=8192;
    rebuilt=original;rebuilt.id=99;rebuilt.optimization_plan_id=101;
    require_contract(VHO_DSL_fetch_same_specification(original,rebuilt),"rebuilt IDs are not semantics");
    rebuilt.wait_point++;
    require_contract(!VHO_DSL_fetch_same_specification(original,rebuilt),"changed wait cannot replace pair");
    rebuilt=original;rebuilt.resources.shared_bytes++;
    require_contract(!VHO_DSL_fetch_same_specification(original,rebuilt),"changed resources cannot replace pair");
    DSL_HIGH_SEARCH_INPUT input;std::string reason;
    require_contract(LNO_DSL_read_search_input(argv[1],input,reason),"manifest");
    input.production_controls=true;
    DSL_HIGH_SEARCH_REPORT generic;
    require_contract(LNO_DSL_observe_search(input,generic,reason),"generic exploration");
    const std::string legacy_resource=generic.resource_key,legacy_model=generic.model_key;
    for(unsigned i=0;i<4;++i) {
        DSL_HIGH_SEARCH_INPUT current=input;current.gpu.device=load(argv[2],argv[3+2*i],argv[4+2*i]);
        current.gpu.high_private_reuse=true;
        DSL_HIGH_SEARCH_REPORT keys;LNO_DSL_fill_search_keys(current,keys);
        require_contract(keys.resource_key!=legacy_resource && keys.model_key!=legacy_model,"cache partition");
        DSL_HIGH_SEARCH_REPORT result;
        const bool observed=LNO_DSL_observe_search(current,result,reason);
        if(i==0 || i==1) {
            require_contract(observed && result.production.has_winner && result.exploration.complete,"bound exploration");
            const VHO_MLA_SEARCH_CANDIDATE &candidate=result.exploration.candidates[result.exploration.winner];
            require_contract(candidate.total==generic.exploration.candidates[generic.exploration.winner].total,"no implicit cycle conversion");
            bool service=false;
            for(size_t j=0;j<candidate.stages.size();++j)
                service=service || (candidate.stages[j].physical_hbm_known && candidate.stages[j].physical_hbm_upper_ns>0);
            require_contract(service,"physical service uses imported rate");
            if(i==0) {
                input.gpu.device=current.gpu.device;
                generic=result;
            } else {
                require_contract(keys.resource_key==generic.resource_key && keys.model_key!=generic.model_key,"calibration dependency separation");
                for(size_t j=0;j<candidate.stages.size();++j)
                    require_contract(candidate.stages[j].physical_hbm_lower_ns==
                        generic.exploration.candidates[generic.exploration.winner].stages[j].physical_hbm_lower_ns/2,
                        "physical bandwidth change");
            }
        } else if(i==2) require_contract(observed && !result.production.has_winner && result.exploration.evaluations==12,"budget refuses every candidate without lowering");
        else require_contract(!observed || !result.production.has_winner,"unknown resources cannot win");
    }
    DSL_HIGH_SEARCH_INPUT long_input=input;
    VHO_TENSOR_PROGRAM geometry=input.algorithm;
    geometry.fuse=geometry.cooperative=geometry.fuse_attention=geometry.register_latent=true;
    const UINT64 old_length=VHO_tensor_attention(geometry).length,old_rank=VHO_tensor_attention(geometry).rank;
    require_contract(old_length && old_length!=8193,"source length");
    for(size_t i=0;i<long_input.algorithm.values.size();++i)
        for(size_t d=0;d<long_input.algorithm.values[i].shape.size();++d)
            if(long_input.algorithm.values[i].shape[d]==old_length) long_input.algorithm.values[i].shape[d]=8193;
            else if(long_input.algorithm.values[i].shape[d]==old_rank) long_input.algorithm.values[i].shape[d]=64;
    for(size_t i=0;i<long_input.algorithm.operations.size();++i) {
        std::map<std::string,std::string> &attributes=long_input.algorithm.operations[i].attributes;
        if(attributes.count("attr.target_shape")) {
            std::istringstream fields(attributes["attr.target_shape"]);std::ostringstream shape;std::string token;
            while(std::getline(fields,token,',')) {
                if(shape.tellp()>0) shape<<',';
                if(strtoull(token.c_str(),NULL,10)==old_length) shape<<8193;
                else if(strtoull(token.c_str(),NULL,10)==old_rank) shape<<64;else shape<<token;
            }
            attributes["attr.target_shape"]=shape.str();
        }
    }
    long_input.gpu.device=load(argv[2],argv[11],argv[12]);long_input.gpu.high_private_reuse=true;
    DSL_HIGH_SEARCH_REPORT reused,additional;
    require_contract(LNO_DSL_observe_search(long_input,reused,reason) && reused.production.has_winner,
        "long split fits exact source backing without duplicate scratch");
    bool wider=false;
    for(size_t i=0;i<reused.exploration.candidates.size();++i) {
        const VHO_MLA_SEARCH_CANDIDATE &candidate=reused.exploration.candidates[i];
        if(candidate.width==32 || candidate.width==64)
            require_contract(!candidate.eligible && candidate.reason=="high_private_tuple_capacity","narrow tuple capacity excludes incumbent");
        if(candidate.width==128 && candidate.eligible) wider=true;
    }
    require_contract(wider,"admissible wider tuple candidate remains");
    long_input.gpu.high_private_reuse=false;
    require_contract(LNO_DSL_observe_search(long_input,additional,reason) && !additional.production.has_winner,
        "separate allocation still charges split scratch");
    require_contract(reused.resource_key!=additional.resource_key,"realization accounting partitions cache");
    printf("H1C_MLA PASS device=%s candidate_native_api=unavailable probes=0 lowerings=0\n",argv[2]);
    return 0;
}
