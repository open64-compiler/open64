/* Host-only MMA5-H protocol test substitutions, never device implementations.
 * Design: doc/AI-COMPILER-MMA5-H-HOPPER-PIPELINE.md. */
#include "../wgmma_cuda_stub/cuda_runtime.h"
void VHO_DSL_pipeline_host_batch(float *, unsigned short *, unsigned short *, unsigned);
void VHO_DSL_pipeline_host_issue(unsigned short *, unsigned short *, unsigned short *, unsigned short *,
    unsigned long long *, unsigned, unsigned);
void VHO_DSL_pipeline_host_wait(unsigned long long *);
