/* Copyright (C) 2026 Open64 Project */
/* H1c production snapshot/query/key oracle. Exercise owned evidence and LNO
 * binding equality without native initialization, GPU or per-candidate work.
 * Design: doc/AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md. */
#include "dsl_gpu_profile.h"
#include "dsl_gpu_plan.h"
#include "../../lno/dsl_high_snapshot.h"
#include <stdlib.h>

/* Refuse a broken contract even when compiler development assertions are off. */
static void require_contract(bool condition, const char *reason)
{
    if (!condition) { fprintf(stderr,"H1c contract: %s\n",reason); exit(1); }
}

/* Resolve only the production importer; fixtures are externally pinned and
 * all imported quantities remain explicitly synthetic evidence. */
static DSL_GPU_PLAN_SNAPSHOT load(const char *device, const char *file,
    const char *sha, DSL_GPU_DEVICE_CONTEXT_OWNER &owner)
{
    DSL_HARDWARE_TARGET_SELECTION target;
    require_contract(DSL_hardware_target_resolve(NULL,device,&target,stderr),"target");
    require_contract(DSL_gpu_profile_resolve(target,file,sha,"fixture-toolchain-v1",&owner,stderr),"import");
    return DSL_GPU_PLAN_SNAPSHOT::Capture(owner.Context());
}

/* Nine related snapshots prove independent legality/cost/budget invalidation,
 * unknown refusals, context rebinding and owned survival after owner cleanup. */
int main(int argc, char **argv)
{
    require_contract(argc==20,"DEVICE and nine FILE SHA pairs");
    DSL_GPU_DEVICE_CONTEXT_OWNER owners[9];
    DSL_GPU_PLAN_SNAPSHOT snapshots[9];
    for(unsigned i=0;i<9;++i) snapshots[i]=load(argv[1],argv[2+2*i],argv[3+2*i],owners[i]);
    const DSL_GPU_PLAN_SNAPSHOT &base=snapshots[0];
    require_contract(base.Bound() && base.Is_current(owners[0].Context()),"bound/current");
    require_contract(!base.Is_current(NULL),"cannot strip imported context");
    require_contract(base.Legality_key()==snapshots[1].Legality_key() &&
        base.Cache_key()!=snapshots[1].Cache_key(),"calibration invalidates cost only");
    require_contract(base.Stamp().resource==std::string(snapshots[2].Stamp().resource) &&
        std::string(base.Stamp().execution)==snapshots[2].Stamp().execution &&
        base.Legality_key()!=snapshots[2].Legality_key(),"budget independent keys");
    for(unsigned i=1;i<9;++i)
        require_contract(!base.Is_current(owners[i].Context()),"stale context accepted");
    double lower=-1,upper=-1;
    require_contract(base.HBM_service_ns(1000,&lower,&upper) && lower==100 && upper==100,"physical service ns");
    require_contract(snapshots[1].HBM_service_ns(1000,&lower,&upper) && lower==50 && upper==50,"rate affects physical component");
    lower=upper=-1;
    require_contract(!snapshots[4].HBM_service_ns(1000,&lower,&upper) && lower==-1 && upper==-1,"unknown service is not free");
    require_contract(base.Fits_CTA(128,32,4096),"admitted CTA");
    require_contract(!snapshots[3].Fits_CTA(128,32,4096),"reduced thread capacity constrains candidate");
    require_contract(!snapshots[7].Fits_CTA(128,32,4096),"uncertain allocation quantum refuses");
    require_contract(snapshots[8].Fits_CTA(128,32,0) && !snapshots[8].Fits_CTA(128,33,0),"per-warp RF rounding refuses 4352 versus 5120");
    require_contract(!snapshots[8].Fits_CTA(32,33,0),"allocated warp groups retained");
    require_contract(!base.Fits_CTA(128,0,0),"unknown registers cannot win");
    require_contract(!base.Fits_CTA(128,~uint64_t(0),0),"register overflow");
    require_contract(!base.Fits_CTA(128,32,~uint64_t(0)),"shared overflow");
    require_contract(base.Fits_storage(8192) && !snapshots[2].Fits_storage(8192),"budget eligibility");
    require_contract(!snapshots[4].Fits_storage(1) && !snapshots[4].Fits_CTA(32,1,0),"unknown resource/budget refuses");
    DSL_GPU_PLAN_SNAPSHOT generic;
    require_contract(generic.Cache_key().empty() && generic.Legality_key().empty() &&
        generic.Is_current(NULL) && !generic.Is_current(owners[0].Context()) &&
        generic.Fits_CTA(128,32,4096),"architecture-only preservation");
    DSL_HIGH_BINDINGS a,b;a.device=base;b.device=base;
    require_contract(a==b,"LNO equal snapshot binding");
    b.device=snapshots[1];require_contract(!(a==b),"LNO stale calibration binding");
    b.device=snapshots[2];require_contract(!(a==b),"LNO stale budget binding");
    b.device=snapshots[5];require_contract(!(a==b),"LNO stale resource revision");
    b.device=snapshots[6];require_contract(!(a==b),"LNO stale partition binding");
    DSL_HARDWARE_TARGET_SELECTION identity_target;
    DSL_GPU_DEVICE_CONTEXT_OWNER identity_owner;
    require_contract(DSL_hardware_target_resolve(NULL,argv[1],&identity_target,NULL) &&
        DSL_gpu_profile_resolve(identity_target,NULL,NULL,NULL,&identity_owner,NULL),"identity-only resolution");
    DSL_GPU_PLAN_SNAPSHOT identity=DSL_GPU_PLAN_SNAPSHOT::Capture(identity_owner.Context());
    require_contract(!identity.Bound() && !identity.Cache_key().empty() &&
        !identity.Is_current(NULL) && !generic.Is_current(identity_owner.Context()) &&
        identity.Fits_CTA(128,32,4096),"identity-only freshness without resource assumptions");
    const std::string key=base.Cache_key();owners[0].Reset();
    require_contract(base.Cache_key()==key && base.Fits_CTA(128,32,4096),"owned snapshot survives owner");
    const char *families[]={"GEMM","MLA","LayerNorm","Reduction"};
    for(unsigned i=0;i<4;++i) base.Print(stdout,families[i],1000);
    printf("H1C_PLAN_CONTRACT PASS device=%s native_mutations=0 probes=0 candidate_compiles=0\n",argv[1]);
    return 0;
}
