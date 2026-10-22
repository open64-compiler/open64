/* Validation-only untiled CUDA control for the compiler-selected M2 tile.
 * This is not an Open64-generated baseline or an M2 acceptance receipt. */

#include <cuda_runtime.h>

#include <cmath>
#include <cstdio>
#include <vector>

#include "matmul.generated.cu"

__global__ void ai_matmul_direct_control(const float *a, const float *b,
                                         float *c) {
  const int row = blockIdx.y * blockDim.y + threadIdx.y;
  const int col = blockIdx.x * blockDim.x + threadIdx.x;
  if (row >= M || col >= N) return;
  float sum = 0.0f;
  for (int q = 0; q < K; ++q)
    sum += a[row * K + q] * b[q * N + col];
  c[row * N + col] = sum;
}

static bool check(cudaError_t status, const char *step) {
  if (status == cudaSuccess) return true;
  std::fprintf(stderr, "%s: %s\n", step, cudaGetErrorString(status));
  return false;
}

static bool launch(bool tiled, const float *a, const float *b, float *c,
                   cudaStream_t stream) {
  if (tiled) {
    ai_matmul_tile_f32<<<dim3((N + BN - 1) / BN, (M + BM - 1) / BM),
                         THREADS, 0, stream>>>(a, b, c);
  } else {
    ai_matmul_direct_control<<<dim3((N + 15) / 16, (M + 15) / 16),
                               dim3(16, 16), 0, stream>>>(a, b, c);
  }
  return check(cudaGetLastError(), "kernel launch");
}

int main() {
  std::vector<float> a(M * K), b(K * N), expected(M * N), got(M * N);
  for (int row = 0; row < M; ++row)
    for (int col = 0; col < K; ++col)
      a[row * K + col] = float((row * 7 + col * 3) % 13 - 6) * 0.125f;
  for (int row = 0; row < K; ++row)
    for (int col = 0; col < N; ++col)
      b[row * N + col] = float((row * 5 + col * 11) % 17 - 8) * 0.125f;
  for (int row = 0; row < M; ++row)
    for (int col = 0; col < N; ++col) {
      double sum = 0.0;
      for (int q = 0; q < K; ++q)
        sum += double(a[row * K + q]) * double(b[q * N + col]);
      expected[row * N + col] = float(sum);
    }

  float *da = nullptr, *db = nullptr, *dc = nullptr;
  cudaStream_t stream = nullptr;
  cudaEvent_t start = nullptr, stop = nullptr;
  const size_t a_bytes = a.size() * sizeof(float);
  const size_t b_bytes = b.size() * sizeof(float);
  const size_t c_bytes = got.size() * sizeof(float);
  bool ok = false;
  if (!check(cudaMalloc(reinterpret_cast<void **>(&da), a_bytes), "allocate A") ||
      !check(cudaMalloc(reinterpret_cast<void **>(&db), b_bytes), "allocate B") ||
      !check(cudaMalloc(reinterpret_cast<void **>(&dc), c_bytes), "allocate C") ||
      !check(cudaMemcpy(da, a.data(), a_bytes, cudaMemcpyHostToDevice), "copy A") ||
      !check(cudaMemcpy(db, b.data(), b_bytes, cudaMemcpyHostToDevice), "copy B") ||
      !check(cudaStreamCreate(&stream), "create stream") ||
      !check(cudaEventCreate(&start), "create start event") ||
      !check(cudaEventCreate(&stop), "create stop event"))
    goto cleanup;

  for (int variant = 0; variant < 2; ++variant) {
    if (!launch(variant == 1, da, db, dc, stream) ||
        !check(cudaStreamSynchronize(stream), "correctness sync") ||
        !check(cudaMemcpy(got.data(), dc, c_bytes, cudaMemcpyDeviceToHost),
               "copy result"))
      goto cleanup;
    for (size_t i = 0; i < got.size(); ++i) {
      const float tolerance = 0.001f + 0.0001f * std::fabs(expected[i]);
      if (!std::isfinite(got[i]) ||
          std::fabs(got[i] - expected[i]) > tolerance) {
        std::fprintf(stderr,
                     "mismatch variant=%s index=%zu got=%g expected=%g\n",
                     variant ? "selected_tile" : "untiled_control", i,
                     got[i], expected[i]);
        goto cleanup;
      }
    }
  }

  for (int warmup = 0; warmup < 5; ++warmup)
    for (int variant = 0; variant < 2; ++variant)
      if (!launch(variant == 1, da, db, dc, stream)) goto cleanup;
  if (!check(cudaStreamSynchronize(stream), "warmup sync")) goto cleanup;

  std::puts("sample,variant,elapsed_ms");
  for (int sample = 0; sample < 30; ++sample)
    for (int step = 0; step < 2; ++step) {
      const int variant = (sample & 1) ? 1 - step : step;
      float elapsed_ms = 0.0f;
      if (!check(cudaEventRecord(start, stream), "start event") ||
          !launch(variant == 1, da, db, dc, stream) ||
          !check(cudaEventRecord(stop, stream), "stop event") ||
          !check(cudaEventSynchronize(stop), "timed sync") ||
          !check(cudaEventElapsedTime(&elapsed_ms, start, stop), "elapsed time"))
        goto cleanup;
      std::printf("%d,%s,%.9f\n", sample,
                  variant ? "selected_tile" : "untiled_control", elapsed_ms);
    }
  ok = true;

cleanup:
  if (start && !check(cudaEventDestroy(start), "destroy start event")) ok = false;
  if (stop && !check(cudaEventDestroy(stop), "destroy stop event")) ok = false;
  if (stream && !check(cudaStreamDestroy(stream), "destroy stream")) ok = false;
  if (da && !check(cudaFree(da), "free A")) ok = false;
  if (db && !check(cudaFree(db), "free B")) ok = false;
  if (dc && !check(cudaFree(dc), "free C")) ok = false;
  return ok ? 0 : 1;
}
