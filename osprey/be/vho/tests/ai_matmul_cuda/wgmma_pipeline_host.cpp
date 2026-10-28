/* Host-only geometry/protocol replay of audited MMA5-H WHIRL-derived CUDA.
 * Deferred copies and completed-atom models are not GPU/ISA certification.
 * Design: doc/AI-COMPILER-MMA5-H-HOPPER-PIPELINE.md. */
#define VHO_DSL_PIPELINE_HOST
#include "wgmma_host.cpp"
#include <map>

struct VHO_DSL_PIPELINE_COPY {
    unsigned short *raw_a, *raw_b, *input_a, *input_b;
    unsigned stride_a, stride_b;
};
static std::map<unsigned long long *, VHO_DSL_PIPELINE_COPY> VHO_DSL_pipeline_pending;

/* Elected-thread issue only records a pending transaction; no raw bytes are
 * visible yet. Reissuing against an incomplete generation is a test failure. */
void VHO_DSL_pipeline_host_issue(unsigned short *raw_a, unsigned short *raw_b,
    unsigned short *input_a, unsigned short *input_b, unsigned long long *ready,
    unsigned stride_a, unsigned stride_b)
{
    if (threadIdx.x || VHO_DSL_wgmma_phase || VHO_DSL_pipeline_pending.count(ready)) std::abort();
    VHO_DSL_PIPELINE_COPY copy = {raw_a, raw_b, input_a, input_b, stride_a, stride_b};
    VHO_DSL_pipeline_pending[ready] = copy;
}

/* Complete exactly the barrier's raw slot before the CTA readiness join.
 * This is independent of the later compute/result/reuse protocol. */
void VHO_DSL_pipeline_host_wait(unsigned long long *ready)
{
    if (threadIdx.x || VHO_DSL_wgmma_phase || !VHO_DSL_pipeline_pending.count(ready)) std::abort();
    VHO_DSL_PIPELINE_COPY copy = VHO_DSL_pipeline_pending[ready];
    for (unsigned row = 0; row < 64; ++row)
        std::copy(copy.input_a + row * copy.stride_a, copy.input_a + row * copy.stride_a + 16, copy.raw_a + row * 16);
    for (unsigned row = 0; row < 16; ++row)
        std::copy(copy.input_b + row * copy.stride_b, copy.input_b + row * copy.stride_b + 64, copy.raw_b + row * 64);
    VHO_DSL_pipeline_pending.erase(ready);
}

/* Apply the audited carrier's completed mathematical effect. The model's
 * instant completion is explicitly not an ISA latency/rounding claim. */
void VHO_DSL_pipeline_host_batch(float *fragment, unsigned short *input_a, unsigned short *input_b, unsigned atoms)
{
    VHO_DSL_wgmma_host_atom(fragment, input_a, input_b);
    if (atoms == 2) {
        VHO_DSL_wgmma_phase = 2;
        VHO_DSL_wgmma_host_atom(fragment, input_a + 1024, input_b + 1024);
    } else if (atoms != 1) std::abort();
}
