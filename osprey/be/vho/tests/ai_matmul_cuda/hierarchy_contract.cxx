/* Copyright (C) 2026 Open64 Project */
/* H4 detached work oracle for the production VHO count service. No native
 * tree, GPU observation or candidate execution is used by this test.
 * Design: doc/AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md. */
#include "dsl_gemm_hierarchy.h"
#include <cstdio>
#include <cstdlib>
/* Always-active independent assertions include deliberate rejected inputs. */
static void DSL_h4_require(bool ok,const char *name) {
    if(!ok) {fprintf(stderr,"FAIL H4 %s\n",name);exit(1);}printf("PASS H4 %s\n",name);
}
/* Hand-count four output rows and four columns per thread; enumerate the
 * original and retained consumer independently of the production formula. */
int main() {
    DSL_GEMM_HIERARCHY_WORK original,retained;std::string why;
    for(unsigned k=1;k<=145;++k) {
        DSL_h4_require(VHO_DSL_gemm_hierarchy_work(64,64,16,4,4,k,false,original,why) &&
            VHO_DSL_gemm_hierarchy_work(64,64,16,4,4,k,true,retained,why),"actual-production-work");
        uint64_t a=0,b=0,r=0,ops=0;
        for(unsigned step=0;step<(k+15)/16;++step) for(unsigned q=0;q<16;++q) for(unsigned thread=0;thread<256;++thread) {
            for(unsigned i=0;i<4;++i) {++a;for(unsigned j=0;j<4;++j) {++b;ops+=2;}}
            for(unsigned j=0;j<4;++j) ++r;
        }
        DSL_h4_require(original.shared_reads==4*(a+b) && retained.shared_reads==4*(a+r) &&
            original.b_loads==b && retained.b_loads==r && original.scalar_ops==ops && retained.scalar_ops==ops &&
            original.shared_writes==4*128*16*((k+15)/16) && retained.shared_writes==original.shared_writes &&
            retained.extra_registers==4 && original.extra_registers==0,"independent-enumerated-loads-and-ordered-work");
    }
    for(unsigned fault=0;fault<5;++fault) {
        uint64_t bm=64,bn=64,bk=16,tm=4,tn=4,k=145;
        if(fault==0) tm=1;if(fault==1) tn=64;if(fault==2) k=~uint64_t(0);if(fault==3) bk=0;if(fault==4) bm=~uint64_t(0);
        retained=original;
        DSL_h4_require(!VHO_DSL_gemm_hierarchy_work(bm,bn,bk,tm,tn,k,true,retained,why) && !retained.scalar_ops,"overflow-shape-clears-output");
    }
    return 0;
}
