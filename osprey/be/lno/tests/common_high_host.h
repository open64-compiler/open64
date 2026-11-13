/* Copyright (C) 2026 Open64 Project */
/* Host compatibility for ordinary whirl2c's legacy F32 intrinsic/inf names.
 * Design: doc/AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md.
 * This supplies math spellings and the existing vector-runtime ABI; all
 * algorithm statements come from mapped High WHIRL. The review harness links
 * the production runtime, with no GPU or schedule semantics added. */
#ifndef common_high_host_INCLUDED
#define common_high_host_INCLUDED
#include <math.h>
#define infF INFINITY
#define F4EXP expf
#define F4LOG logf
/* Declare the production strided F32 runtime with its actual C signature. */
extern void vexpf(float *input, float *output, long count,
                 long input_stride, long output_stride);
/* Normalize the legacy emitter's pointer-to-pointer address spelling before
 * calling the production runtime on caller-owned arrays. Only argument types
 * change; counts, strides, storage, arithmetic and runtime behavior do not. */
static inline void COMMON_HIGH_host_vexpf(void *input, void *output, long count,
                                        long input_stride, long output_stride)
{
    vexpf((float *)input, (float *)output, count, input_stride, output_stride);
}
#define vexpf COMMON_HIGH_host_vexpf
#endif
