#include <cuda_runtime.h>

#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <vector>

#include "matmul.generated.cu"

static void check(cudaError_t status, const char *step) {
  if (status != cudaSuccess) {
    std::fprintf(stderr, "%s: %s\n", step, cudaGetErrorString(status));
    std::exit(1);
  }
}

static void launch(bool tma, const float *a, const float *b, float *c,
                   cudaStream_t stream) {
  if (tma) {
    bool used_tma = false;
    check(ai_matmul_tile_f32_launch_ex(a, b, c, stream, &used_tma),
          "selected TMA launch");
    if (!used_tma) {
      std::fprintf(stderr, "selected launcher fell back to synchronous copy\n");
      std::exit(1);
    }
  } else {
    ai_matmul_tile_f32_sync<<<dim3(N / BN, M / BM), THREADS, 0, stream>>>(
        a, b, c);
    check(cudaGetLastError(), "synchronous launch");
  }
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
  check(cudaMalloc(reinterpret_cast<void **>(&da), a.size() * sizeof(float)),
        "allocate A");
  check(cudaMalloc(reinterpret_cast<void **>(&db), b.size() * sizeof(float)),
        "allocate B");
  check(cudaMalloc(reinterpret_cast<void **>(&dc), got.size() * sizeof(float)),
        "allocate C");
  check(cudaMemcpy(da, a.data(), a.size() * sizeof(float),
                   cudaMemcpyHostToDevice), "copy A");
  check(cudaMemcpy(db, b.data(), b.size() * sizeof(float),
                   cudaMemcpyHostToDevice), "copy B");

  cudaStream_t stream;
  check(cudaStreamCreate(&stream), "create stream");
  for (int variant = 0; variant < 2; ++variant) {
    const bool tma = variant == 1;
    check(cudaMemsetAsync(dc, 0, got.size() * sizeof(float), stream),
          "clear C");
    launch(tma, da, db, dc, stream);
    check(cudaStreamSynchronize(stream), "finish correctness launch");
    check(cudaMemcpy(got.data(), dc, got.size() * sizeof(float),
                     cudaMemcpyDeviceToHost), "copy C");
    for (size_t index = 0; index < got.size(); ++index) {
      const float tolerance = 0.001f + 0.0001f * std::fabs(expected[index]);
      if (std::fabs(got[index] - expected[index]) > tolerance) {
        std::fprintf(stderr, "mismatch variant=%s index=%zu got=%g expected=%g\n",
                     tma ? "tma" : "sync", index, got[index],
                     expected[index]);
        return 1;
      }
    }
    std::fprintf(stderr, "PASS shape=%dx%dx%d variant=%s\n", M, N, K,
                 tma ? "tma" : "sync");
  }

  std::printf("sample,variant,elapsed_ms\n");
  for (int variant = 0; variant < 2; ++variant) {
    const bool tma = variant == 1;
    for (int warmup = 0; warmup < 5; ++warmup)
      launch(tma, da, db, dc, stream);
    check(cudaStreamSynchronize(stream), "finish warmup");
    for (int sample = 0; sample < 30; ++sample) {
      const auto start = std::chrono::steady_clock::now();
      launch(tma, da, db, dc, stream);
      check(cudaStreamSynchronize(stream), "finish sample");
      const auto stop = std::chrono::steady_clock::now();
      const double elapsed_ms =
          std::chrono::duration<double, std::milli>(stop - start).count();
      std::printf("%d,%s,%.9f\n", sample, tma ? "tma" : "sync",
                  elapsed_ms);
    }
  }
  check(cudaStreamDestroy(stream), "destroy stream");
  check(cudaFree(da), "free A");
  check(cudaFree(db), "free B");
  check(cudaFree(dc), "free C");
  return 0;
}
