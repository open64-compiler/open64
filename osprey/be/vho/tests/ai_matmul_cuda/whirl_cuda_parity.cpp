// Host CTA simulation comparing VHO device-WHIRL translation, legacy CUDA,
// and reference GEMM. It checks source semantics, not native GPU behavior.
// Design: doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md.
#include <cmath>
#include <condition_variable>
#include <cstdio>
#include <mutex>
#include <thread>
#include <vector>

#include <cuda_runtime.h>

namespace {
constexpr int M = 128;
constexpr int N = 128;
constexpr int K = 128;
constexpr int Threads = 256;

std::mutex barrier_mutex;
std::condition_variable barrier_condition;
int barrier_arrived = 0;
int barrier_generation = 0;
}  // namespace

thread_local dim3 threadIdx = {0, 0, 0};
dim3 blockIdx = {0, 0, 0};

void cuda_stub_barrier() {
  std::unique_lock<std::mutex> lock(barrier_mutex);
  const int generation = barrier_generation;
  if (++barrier_arrived == Threads) {
    barrier_arrived = 0;
    ++barrier_generation;
    barrier_condition.notify_all();
  } else {
    barrier_condition.wait(lock, [generation] {
      return barrier_generation != generation;
    });
  }
}

extern "C" void ai_matmul_whirl(float *, float *, float *);
extern "C" void ai_matmul_legacy(const float *, const float *, float *);

template <class Kernel>
void run_cta(Kernel kernel, float *a, float *b, float *c) {
  std::vector<std::thread> workers;
  workers.reserve(Threads);
  for (int tid = 0; tid != Threads; ++tid) {
    workers.emplace_back([=] {
      threadIdx = {static_cast<unsigned>(tid), 0, 0};
      kernel(a, b, c);
    });
  }
  for (auto &worker : workers) worker.join();
}

int main() {
  std::vector<float> a(M * K), b(K * N), actual(M * N), legacy(M * N);
  for (int i = 0; i != M * K; ++i) a[i] = ((i * 13) % 17 - 8) / 8.0f;
  for (int i = 0; i != K * N; ++i) b[i] = ((i * 7) % 19 - 9) / 9.0f;
  run_cta(ai_matmul_whirl, a.data(), b.data(), actual.data());
  run_cta(ai_matmul_legacy, a.data(), b.data(), legacy.data());
  float max_reference_error = 0.0f;
  float max_legacy_error = 0.0f;
  float max_pair_error = 0.0f;
  for (int row = 0; row != M; ++row) {
    for (int col = 0; col != N; ++col) {
      float reference = 0.0f;
      for (int q = 0; q != K; ++q)
        reference += a[row * K + q] * b[q * N + col];
      const int index = row * N + col;
      max_reference_error = std::fmax(max_reference_error,
                                      std::fabs(actual[index] - reference));
      max_legacy_error = std::fmax(max_legacy_error,
                                   std::fabs(legacy[index] - reference));
      max_pair_error = std::fmax(max_pair_error,
                                 std::fabs(actual[index] - legacy[index]));
    }
  }
  std::printf("whirl_vs_reference=%g legacy_vs_reference=%g whirl_vs_legacy=%g\n",
              max_reference_error, max_legacy_error, max_pair_error);
  return max_reference_error < 0.001f && max_legacy_error < 0.001f &&
                 max_pair_error < 0.001f
             ? 0 : 1;
}
