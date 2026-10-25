/* Fixed-shape M2 review adapter. It binds one compiler-emitted CUDA source
 * to the test tensor runtime; it is not the production tensor provider. */

#include "generated_runtime.h"
#include "matmul.generated.cu"

#include <stddef.h>

extern "C" int
ai_matmul_m2_launch(const float *a, const float *b, float *c,
                    uint32_t m, uint32_t n, uint32_t k,
                    const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 *plan)
{
  if (!a || !b || !c || !plan || m != M || n != N || k != K ||
      plan->tile_plan_id != AI_TILE_PLAN_ID ||
      plan->optimization_plan_id != AI_OPTIMIZATION_PLAN_ID ||
      plan->cta_m != BM || plan->cta_n != BN || plan->cta_k != BK ||
      plan->thread_m != TM || plan->thread_n != TN ||
      plan->threads_per_cta != THREADS ||
      plan->shared_bytes_per_stage !=
          (AI_A_SHARED_ELEMS + BK * BN) * sizeof(float))
    return 1;

  int device = 0;
  cudaDeviceProp prop{};
  cudaFuncAttributes attributes{};
  if (cudaGetDevice(&device) != cudaSuccess ||
      cudaGetDeviceProperties(&prop, device) != cudaSuccess ||
      prop.major < 9 ||
      cudaFuncGetAttributes(&attributes, ai_matmul_tile_f32) != cudaSuccess ||
      THREADS > prop.maxThreadsPerBlock ||
      THREADS > attributes.maxThreadsPerBlock ||
      attributes.sharedSizeBytes > plan->shared_bytes_per_stage ||
      attributes.sharedSizeBytes > prop.sharedMemPerBlock)
    return 1;

  float *da = nullptr, *db = nullptr, *dc = nullptr;
  const size_t a_bytes = size_t(M) * K * sizeof(float);
  const size_t b_bytes = size_t(K) * N * sizeof(float);
  const size_t c_bytes = size_t(M) * N * sizeof(float);
  int result = 1;
  if (cudaMalloc(reinterpret_cast<void **>(&da), a_bytes) != cudaSuccess ||
      cudaMalloc(reinterpret_cast<void **>(&db), b_bytes) != cudaSuccess ||
      cudaMalloc(reinterpret_cast<void **>(&dc), c_bytes) != cudaSuccess)
    goto cleanup;
  if (cudaMemcpy(da, a, a_bytes, cudaMemcpyHostToDevice) != cudaSuccess ||
      cudaMemcpy(db, b, b_bytes, cudaMemcpyHostToDevice) != cudaSuccess)
    goto cleanup;
  ai_matmul_tile_f32<<<dim3((N + BN - 1) / BN, (M + BM - 1) / BM),
                       THREADS>>>(da, db, dc);
  if (cudaGetLastError() != cudaSuccess ||
      cudaDeviceSynchronize() != cudaSuccess ||
      cudaMemcpy(c, dc, c_bytes, cudaMemcpyDeviceToHost) != cudaSuccess)
    goto cleanup;
  result = 0;

cleanup:
  if (da && cudaFree(da) != cudaSuccess) result = 1;
  if (db && cudaFree(db) != cudaSuccess) result = 1;
  if (dc && cudaFree(dc) != cudaSuccess) result = 1;
  return result;
}
