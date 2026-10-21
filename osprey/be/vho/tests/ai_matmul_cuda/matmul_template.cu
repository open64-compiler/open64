// Standalone CUDA validation adapter for an explicit AIO-9-shaped tile plan.
// This is not emitted from WHIRL and is not a compiler-lowered optimization.
#include <cuda_runtime.h>

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <vector>

#define BM @BM@
#define BN @BN@
#define BK @BK@
#define TM @TM@
#define TN @TN@
#define THREADS ((BM / TM) * (BN / TN))

static void check(cudaError_t status, const char *where) {
  if (status != cudaSuccess) {
    std::fprintf(stderr, "%s: %s\n", where, cudaGetErrorString(status));
    std::exit(2);
  }
}

__global__ void matmul_tile(const float *a, const float *b, float *c,
                            int m, int n, int k) {
  __shared__ float as[BM][BK];
  __shared__ float bs[BK][BN];
  const int tid = threadIdx.x;
  const int local_row = (tid / (BN / TN)) * TM;
  const int local_col = (tid % (BN / TN)) * TN;
  const int row0 = blockIdx.y * BM;
  const int col0 = blockIdx.x * BN;
  float acc[TM][TN] = {};

  for (int base = 0; base < k; base += BK) {
    for (int offset = tid; offset < BM * BK; offset += THREADS) {
      const int r = offset / BK;
      const int q = offset % BK;
      as[r][q] = row0 + r < m && base + q < k
          ? a[(row0 + r) * k + base + q] : 0.0f;
    }
    for (int offset = tid; offset < BK * BN; offset += THREADS) {
      const int q = offset / BN;
      const int col = offset % BN;
      bs[q][col] = base + q < k && col0 + col < n
          ? b[(base + q) * n + col0 + col] : 0.0f;
    }
    __syncthreads();
    for (int q = 0; q < BK; ++q)
      for (int i = 0; i < TM; ++i) {
        const float av = as[local_row + i][q];
        for (int j = 0; j < TN; ++j)
          acc[i][j] += av * bs[q][local_col + j];
      }
    __syncthreads();
  }
  for (int i = 0; i < TM; ++i)
    for (int j = 0; j < TN; ++j)
      if (row0 + local_row + i < m && col0 + local_col + j < n)
        c[(row0 + local_row + i) * n + col0 + local_col + j] = acc[i][j];
}

static void exercise(int m, int n, int k, bool timed) {
  std::vector<float> a(size_t(m) * k), b(size_t(k) * n), got(size_t(m) * n);
  for (size_t i = 0; i < a.size(); ++i)
    a[i] = float(int((i * 17 + 13) % 31) - 15) / 16.0f;
  for (size_t i = 0; i < b.size(); ++i)
    b[i] = float(int((i * 7 + 19) % 29) - 14) / 16.0f;
  float *da = nullptr, *db = nullptr, *dc = nullptr;
  check(cudaMalloc(&da, a.size() * sizeof(float)), "cudaMalloc A");
  check(cudaMalloc(&db, b.size() * sizeof(float)), "cudaMalloc B");
  check(cudaMalloc(&dc, got.size() * sizeof(float)), "cudaMalloc C");
  check(cudaMemcpy(da, a.data(), a.size() * sizeof(float), cudaMemcpyHostToDevice), "copy A");
  check(cudaMemcpy(db, b.data(), b.size() * sizeof(float), cudaMemcpyHostToDevice), "copy B");
  dim3 grid((n + BN - 1) / BN, (m + BM - 1) / BM);
  matmul_tile<<<grid, THREADS>>>(da, db, dc, m, n, k);
  check(cudaGetLastError(), "kernel launch");
  check(cudaDeviceSynchronize(), "kernel execution");
  check(cudaMemcpy(got.data(), dc, got.size() * sizeof(float), cudaMemcpyDeviceToHost), "copy C");
  double max_abs_error = 0;
  for (int row = 0; row < m; ++row)
    for (int col = 0; col < n; ++col) {
      double want = 0;
      for (int q = 0; q < k; ++q)
        want += double(a[row * k + q]) * double(b[q * n + col]);
      const double error = std::abs(double(got[row * n + col]) - want);
      max_abs_error = std::max(max_abs_error, error);
      if (error > 1e-3 + 1e-4 * std::abs(want)) {
        std::fprintf(stderr, "FAIL shape=%dx%dx%d at (%d,%d): got=%g want=%g\n",
                     m, n, k, row, col, got[row * n + col], want);
        std::exit(3);
      }
    }
  std::fprintf(stderr, "PASS shape=%dx%dx%d max_abs_error=%.9g\n",
               m, n, k, max_abs_error);
  if (timed) {
    cudaEvent_t start, stop;
    check(cudaEventCreate(&start), "event start");
    check(cudaEventCreate(&stop), "event stop");
    for (int i = 0; i < 5; ++i)
      matmul_tile<<<grid, THREADS>>>(da, db, dc, m, n, k);
    check(cudaDeviceSynchronize(), "warmup");
    std::puts("sample,elapsed_ms");
    for (int i = 0; i < 30; ++i) {
      check(cudaEventRecord(start), "start record");
      matmul_tile<<<grid, THREADS>>>(da, db, dc, m, n, k);
      check(cudaEventRecord(stop), "stop record");
      check(cudaEventSynchronize(stop), "stop sync");
      float elapsed = 0;
      check(cudaEventElapsedTime(&elapsed, start, stop), "event elapsed");
      std::printf("%d,%.9g\n", i, elapsed);
    }
    check(cudaEventDestroy(start), "event destroy start");
    check(cudaEventDestroy(stop), "event destroy stop");
  }
  check(cudaFree(da), "free A");
  check(cudaFree(db), "free B");
  check(cudaFree(dc), "free C");
}

int main() {
  cudaDeviceProp prop;
  check(cudaGetDeviceProperties(&prop, 0), "device properties");
  if (prop.major < 9) {
    std::fprintf(stderr, "Hopper or later required; got sm_%d%d\n", prop.major, prop.minor);
    return 4;
  }
  std::fprintf(stderr, "device=%s sm_%d%d\n", prop.name, prop.major, prop.minor);
  exercise(130, 141, 127, false);  // Predicated M, N, and K edges.
  exercise(128, 128, 128, true);
  return 0;
}
