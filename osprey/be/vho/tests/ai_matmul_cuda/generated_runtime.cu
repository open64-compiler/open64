/* Fixed-shape M2 review adapter. It binds one compiler-emitted CUDA source
 * to the test tensor runtime; it is not the production tensor provider. */

#include "generated_runtime.h"
#include "matmul.generated.cu"
#include "open64_target_buffer_service.h"

#include <stddef.h>
#include <limits>

static int
cuda_buffer_allocate(void *context, size_t bytes, size_t,
                     OPEN64_TARGET_BUFFER_VIEW *view)
{
  void *address = nullptr;
  if (cudaMalloc(&address, bytes) != cudaSuccess)
    return 1;
  view->address = address;
  view->bytes = bytes;
  view->tier = OPEN64_TARGET_BUFFER_DEVICE;
  view->device_id = *static_cast<int *>(context);
  return 0;
}

static int
cuda_buffer_transfer(void *, const OPEN64_TARGET_BUFFER_VIEW *destination,
                     const OPEN64_TARGET_BUFFER_VIEW *source, size_t bytes)
{
  cudaMemcpyKind direction;
  if (source->tier == OPEN64_TARGET_BUFFER_HOST &&
      destination->tier == OPEN64_TARGET_BUFFER_DEVICE)
    direction = cudaMemcpyHostToDevice;
  else if (source->tier == OPEN64_TARGET_BUFFER_DEVICE &&
           destination->tier == OPEN64_TARGET_BUFFER_HOST)
    direction = cudaMemcpyDeviceToHost;
  else if (source->tier == OPEN64_TARGET_BUFFER_DEVICE &&
           destination->tier == OPEN64_TARGET_BUFFER_DEVICE)
    direction = cudaMemcpyDeviceToDevice;
  else
    return 1;
  return cudaMemcpy(destination->address, source->address, bytes,
                    direction) == cudaSuccess ? 0 : 1;
}

static int
cuda_buffer_validate_import(void *context,
                            const OPEN64_TARGET_BUFFER_VIEW *view,
                            size_t, size_t)
{
  cudaPointerAttributes attributes{};
  return cudaPointerGetAttributes(&attributes, view->address) == cudaSuccess &&
         attributes.type == cudaMemoryTypeDevice &&
         attributes.device == *static_cast<int *>(context) ? 0 : 1;
}

static int
cuda_buffer_release(void *, const OPEN64_TARGET_BUFFER_VIEW *view)
{
  return cudaFree(view->address) == cudaSuccess ? 0 : 1;
}

static bool
matrix_bytes(uint32_t rows, uint32_t columns, size_t *bytes)
{
  const size_t limit = std::numeric_limits<size_t>::max() / sizeof(float);
  if (rows == 0 || columns == 0 || size_t(rows) > limit / columns)
    return false;
  *bytes = size_t(rows) * columns * sizeof(float);
  return true;
}

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
      !((plan->target_profile_id ==
             OPEN64_DSL_TARGET_PROFILE_NVIDIA_HOPPER && prop.major == 9) ||
        (plan->target_profile_id ==
             OPEN64_DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
             (prop.major == 10 || prop.major == 12))) ||
      cudaFuncGetAttributes(&attributes, ai_matmul_tile_f32) != cudaSuccess ||
      THREADS > prop.maxThreadsPerBlock ||
      THREADS > attributes.maxThreadsPerBlock ||
      attributes.sharedSizeBytes > plan->shared_bytes_per_stage ||
      attributes.sharedSizeBytes > prop.sharedMemPerBlock)
    return 1;

  size_t a_bytes, b_bytes, c_bytes;
  if (!matrix_bytes(M, K, &a_bytes) || !matrix_bytes(K, N, &b_bytes) ||
      !matrix_bytes(M, N, &c_bytes))
    return 1;
  OPEN64_TARGET_BUFFER_SERVICE buffers = {
      OPEN64_TARGET_BUFFER_SERVICE_V1, OPEN64_TARGET_NVIDIA,
      static_cast<uint32_t>(device), &device,
      cuda_buffer_allocate, cuda_buffer_transfer,
      cuda_buffer_validate_import, cuda_buffer_release};
  OPEN64_TARGET_BUFFER_VIEW a_host = {
      const_cast<float *>(a), a_bytes, OPEN64_TARGET_BUFFER_HOST, 0};
  OPEN64_TARGET_BUFFER_VIEW b_host = {
      const_cast<float *>(b), b_bytes, OPEN64_TARGET_BUFFER_HOST, 0};
  OPEN64_TARGET_BUFFER_VIEW c_host = {
      c, c_bytes, OPEN64_TARGET_BUFFER_HOST, 0};
  OPEN64_TARGET_BUFFER_BINDING da{}, db{}, dc{};
  cudaError_t launch_error = cudaSuccess;
  cudaError_t completion_error = cudaSuccess;
  int result = 1;
  if (open64_target_buffer_bind_input(&buffers, &a_host, a_bytes,
                                       alignof(float), &da) != 0 ||
      open64_target_buffer_bind_input(&buffers, &b_host, b_bytes,
                                       alignof(float), &db) != 0 ||
      open64_target_buffer_bind_output(&buffers, &c_host, c_bytes,
                                        alignof(float), &dc) != 0)
    goto cleanup;
  ai_matmul_tile_f32<<<dim3((N + BN - 1) / BN, (M + BM - 1) / BM),
                       THREADS>>>(static_cast<float *>(da.view.address),
                                  static_cast<float *>(db.view.address),
                                  static_cast<float *>(dc.view.address));
  launch_error = cudaGetLastError();
  completion_error = cudaDeviceSynchronize();
  if (launch_error != cudaSuccess || completion_error != cudaSuccess ||
      open64_target_buffer_finish_output(&buffers, &dc, &c_host,
                                         c_bytes) != 0)
    goto cleanup;
  result = 0;

cleanup:
  if (open64_target_buffer_release(&buffers, &dc) != 0) result = 1;
  if (open64_target_buffer_release(&buffers, &db) != 0) result = 1;
  if (open64_target_buffer_release(&buffers, &da) != 0) result = 1;
  return result;
}
