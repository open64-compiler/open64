/* Copyright (C) 2026 Open64 Project */
/* H1d production import/host-contract oracle, with synthetic profiles only.
 * No GPU, native IR or candidate work. Design:
 * doc/AI-COMPILER-H1D-DEVICE-HOST-CONTRACTS-v1.0.md. */
#include "dsl_gpu_profile.h"
#include "dsl_gpu_contract.h"
#include <stdlib.h>
#include <fstream>
#include <iterator>
/* Refuse independently of development assertion settings. */
static void H1D_require(bool value,const char *why)
{
    if(!value) {fprintf(stderr,"H1d: %s\n",why);exit(1);}
}
/* Exercise owned pin lifetime, exact device identities and runtime obligations. */
int main(int argc,char **argv)
{
    H1D_require(argc==4,"DEVICE PROFILE SHA");
    DSL_HARDWARE_TARGET_SELECTION target;DSL_GPU_DEVICE_CONTEXT_OWNER owner;
    H1D_require(DSL_hardware_target_resolve(NULL,argv[1],&target,stderr),"device");
    H1D_require(DSL_gpu_profile_resolve(target,argv[2],argv[3],"fixture-toolchain-v1",&owner,stderr),"import");
    std::ifstream file(argv[2],std::ios::binary);
    std::string bytes((std::istreambuf_iterator<char>(file)),std::istreambuf_iterator<char>());
    H1D_require(owner.Context()->Input_bytes()==bytes,"exact owned pin");
    DSL_GPU_HOST_REQUIREMENTS required=DSL_GPU_HOST_REQUIREMENTS::Capture(owner.Context());
    DSL_GPU_RUNTIME_OBSERVATION current;current.device=required.device;current.architecture=required.architecture;
    current.partition=required.partition;current.toolchain=required.toolchain;current.resource_key=required.resource_key;
    current.execution_key=required.execution_key;current.budget_key=required.budget_key;
    current.context_generation=1;current.budget_generation=2;current.reservation_id=3;current.reserved_bytes=8192;
    current.known=current.reservation_live=true;owner.Reset();
    std::string why;bool reused=false;
    H1D_require(required.Check(current,8192,reused,why) && reused,"owned exact compatibility");
    DSL_GPU_RUNTIME_OBSERVATION changed=current;changed.execution_key="changed-calibration";
    H1D_require(required.Check(changed,8192,reused,why) && !reused,"calibration only loses evidence reuse");
    for(unsigned i=0;i<12;++i) {
        changed=current;
        switch(i) {
        case 0:changed.known=false;break;
        case 1:changed.device="other-device";break;
        case 2:changed.architecture="sm_90a";break;
        case 3:changed.partition="other-partition";break;
        case 4:changed.toolchain="other-toolchain";break;
        case 5:changed.resource_key="other-resource";break;
        case 6:changed.budget_key="other-budget";break;
        case 7:changed.context_generation=0;break;
        case 8:changed.budget_generation=0;break;
        case 9:changed.reservation_id=0;break;
        case 10:changed.reservation_live=false;break;
        case 11:changed.reserved_bytes=8191;break;
        }
        H1D_require(!required.Check(changed,8192,reused,why) && !reused && !why.empty(),"mismatch refusal");
    }
    changed=current;++changed.budget_generation;
    H1D_require(changed.Lease_key()!=current.Lease_key(),"budget generation freshness");
    DSL_GPU_HOST_REQUIREMENTS generic;
    H1D_require(generic.Check(DSL_GPU_RUNTIME_OBSERVATION(),~uint64_t(0),reused,why) && !reused,"architecture-only unchanged/unqualified");
    H1D_require(DSL_gpu_profile_resolve(target,NULL,NULL,NULL,&owner,stderr),"identity-only");
    DSL_GPU_HOST_REQUIREMENTS identity=DSL_GPU_HOST_REQUIREMENTS::Capture(owner.Context());
    H1D_require(owner.Context()->Input_bytes().empty() && identity.Check(current,8192,reused,why) && !reused,"identity-only no tuning claim");
    current.device="other-device";H1D_require(!identity.Check(current,0,reused,why),"identity-only still exact device");
    printf("H1d host contract PASS device=%s mismatch_cases=12 GPU=false\n",argv[1]);return 0;
}
