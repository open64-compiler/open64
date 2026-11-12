/* Copyright (C) 2026 Open64 Project */
/* M6 scalar host evidence uses Open64's existing vector-math runtime ABI.
 * This adapter fixes only the legacy whirl2c vector-call C spelling; it adds
 * no MLA algorithm. Contract:
 * doc/AI-COMPILER-SEARCH-M6-PRODUCTION-ROUTE-v1.0.md. */
#ifndef selected_production_host_INCLUDED
#define selected_production_host_INCLUDED
#include "../../lno/tests/common_high_host.h"
/* Open64 libm/vexpf.c owns this strided-vector function. The generated legacy
 * call carries addresses with float** casts; the runtime consumes float* and
 * long count/strides. Preserve those address values with the declared ABI. */
extern void __vexpf(float *,float *,long,long,long);
#define vexpf(x,y,n,sx,sy) __vexpf((float *)(void *)(x),(float *)(void *)(y),(long)(n),(long)(sx),(long)(sy))
#endif
