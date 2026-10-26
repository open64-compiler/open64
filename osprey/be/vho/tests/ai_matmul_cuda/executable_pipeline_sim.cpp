// Execute the actual WHIRL-derived kernel with delayed copy completion and
// per-slot ownership checks. This is not a GPU/PTX execution model.
#include <algorithm>
#include <cmath>
#include <condition_variable>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <map>
#include <mutex>
#include <stdexcept>
#include <thread>
#include <vector>
#include <cuda_runtime.h>

namespace {
const int M = 128, N = 128, K = AI_TEST_K;
const int BM = AI_TEST_BM, BN = AI_TEST_BN, BK = AI_TEST_BK;
const int Threads = AI_TEST_THREADS;
struct TensorMap { float *data; int rows, cols, box_rows, box_cols; };
struct Copy { float *dst; TensorMap *map; int x, y; };
struct Slot {
  int generation = 0, expected = 0, waited = 0, released = 0;
  bool initialized = false, pending = false, complete = false;
  std::vector<Copy> copies;
};
std::mutex state_mutex, cta_mutex;
std::condition_variable cta_condition;
std::condition_variable ready_condition;
std::map<void *, Slot> slots;
int cta_arrived = 0, cta_generation = 0;
int issued_pairs = 0, delayed_pairs = 0, released_pairs = 0;
int maximum_pending = 0;
bool proxy_published = false;
thread_local void *consumer_slot = nullptr;
thread_local int consumer_generation = -1;

void require(bool condition, const char *message) {
  if (!condition) throw std::runtime_error(message);
}
}  // namespace

thread_local dim3 threadIdx = {0, 0, 0};
dim3 blockIdx = {0, 0, 0};

void cuda_stub_barrier() {
  std::unique_lock<std::mutex> cta_lock(cta_mutex);
  if (consumer_slot) {
    std::lock_guard<std::mutex> state_lock(state_mutex);
    Slot &slot = slots.at(consumer_slot);
    require(slot.complete && slot.generation == consumer_generation,
            "release before completion or slot overwritten during compute");
    require(++slot.released <= Threads, "duplicate slot release");
    if (slot.released == Threads) {
      require(slot.waited == Threads, "not every consumer waited before release");
      ++released_pairs;
    }
    consumer_slot = nullptr;
  }
  const int generation = cta_generation;
  if (++cta_arrived == Threads) {
    cta_arrived = 0;
    ++cta_generation;
    cta_condition.notify_all();
  } else {
    cta_condition.wait(cta_lock, [generation] { return cta_generation != generation; });
  }
}

void __open64_cuda_barrier_init(void *bar) {
  std::lock_guard<std::mutex> lock(state_mutex);
  require(threadIdx.x == 0 && !slots[bar].initialized, "bad barrier initialization");
  slots[bar] = Slot();
  slots[bar].initialized = true;
}

void __open64_cuda_proxy_publish() {
  std::lock_guard<std::mutex> lock(state_mutex);
  require(threadIdx.x == 0 && slots.size() == 2, "initialization was not published");
  proxy_published = true;
}

void __open64_cuda_tma_acquire(void *bar, int bytes) {
  std::lock_guard<std::mutex> lock(state_mutex);
  Slot &slot = slots.at(bar);
  require(threadIdx.x == 0 && proxy_published && slot.initialized && !slot.pending,
          "acquire of busy/uninitialized slot");
  require(!slot.complete || slot.released == Threads, "slot reused before CTA release");
  if (slot.complete) ++slot.generation;
  slot.expected = bytes;
  slot.waited = slot.released = 0;
  slot.complete = false;
  slot.pending = true;
  slot.copies.clear();
  ++issued_pairs;
  int outstanding = 0;
  for (const auto &entry : slots) outstanding += entry.second.pending;
  maximum_pending = std::max(maximum_pending, outstanding);
}

void __open64_cuda_tma_issue(void *dst, void *map, int x, int y, void *bar) {
  std::lock_guard<std::mutex> lock(state_mutex);
  Slot &slot = slots.at(bar);
  TensorMap *tensor = static_cast<TensorMap *>(map);
  require(threadIdx.x == 0 && slot.pending && slot.copies.size() < 2,
          "issue without acquire or duplicate issue");
  require(x >= 0 && y >= 0 && x + tensor->box_cols <= tensor->cols &&
          y + tensor->box_rows <= tensor->rows, "TMA coordinates out of bounds");
  slot.copies.push_back({static_cast<float *>(dst), tensor, x, y});
  ready_condition.notify_all();
}

void __open64_cuda_tma_wait(void *bar, int phase) {
  std::unique_lock<std::mutex> lock(state_mutex);
  Slot &slot = slots.at(bar);
  require(ready_condition.wait_for(lock, std::chrono::seconds(10), [&] {
    return slot.initialized && phase == slot.generation % 2 &&
           (slot.complete || slot.copies.size() == 2);
  }), "wait has no issued matching generation");
  if (slot.pending) {
    require(slot.copies.size() == 2, "wait before both operand issues");
    int bytes = 0;
    // Complete in reverse issue order to avoid relying on A-before-B completion.
    for (auto copy = slot.copies.rbegin(); copy != slot.copies.rend(); ++copy) {
      const TensorMap &map = *copy->map;
      for (int row = 0; row < map.box_rows; ++row)
        std::memcpy(copy->dst + row * map.box_cols,
                    map.data + (copy->y + row) * map.cols + copy->x,
                    map.box_cols * sizeof(float));
      bytes += map.box_rows * map.box_cols * sizeof(float);
    }
    require(bytes == slot.expected, "expected transaction byte count mismatch");
    slot.pending = false;
    slot.complete = true;
    ++delayed_pairs;
  }
  ++slot.waited;
  require(slot.complete && slot.waited <= Threads, "wait without matching issue");
  consumer_slot = bar;
  consumer_generation = slot.generation;
}

void __open64_cuda_barrier_invalidate(void *bar) {
  std::lock_guard<std::mutex> lock(state_mutex);
  Slot &slot = slots.at(bar);
  require(threadIdx.x == 0 && !slot.pending &&
          (!slot.complete || slot.released == Threads), "invalidation before drain");
  slot.initialized = false;
}

#if AI_TEST_TMA
extern "C" void ai_matmul_tile_f32(float *, float *, float *, void *, void *);
#else
extern "C" void ai_matmul_tile_f32(float *, float *, float *);
#endif

int main() {
  std::vector<float> a(M*K), b(K*N), c(M*N);
  for (int i = 0; i < M*K; ++i) a[i] = ((i*13)%17-8)/8.0f;
  for (int i = 0; i < K*N; ++i) b[i] = ((i*7)%19-9)/9.0f;
  TensorMap a_map{a.data(), M, K, BM, BK}, b_map{b.data(), K, N, BK, BN};
  int ctas = 0;
  for (int row = 0; row < M; row += BM) {
    for (int col = 0; col < N; col += BN) {
      slots.clear();
      proxy_published = false;
      blockIdx = {static_cast<unsigned>(col/BN), static_cast<unsigned>(row/BM), 0};
      std::vector<std::thread> workers;
      for (int tid = 0; tid < Threads; ++tid) {
        workers.emplace_back([&, tid] {
          threadIdx = {static_cast<unsigned>(tid), 0, 0};
#if AI_TEST_TMA
          ai_matmul_tile_f32(a.data(), b.data(), c.data(), &a_map, &b_map);
#else
          ai_matmul_tile_f32(a.data(), b.data(), c.data());
#endif
        });
      }
      for (auto &worker : workers) worker.join();
      for (const auto &entry : slots)
        require(!entry.second.initialized && !entry.second.pending, "undrained slot");
      ++ctas;
    }
  }
  float error = 0;
  for (int row = 0; row < M; ++row)
    for (int col = 0; col < N; ++col) {
      float reference = 0;
      for (int q = 0; q < K; ++q) reference += a[row*K+q]*b[q*N+col];
      require(std::isfinite(c[row*N+col]), "nonfinite result");
      error = std::fmax(error, std::fabs(c[row*N+col]-reference));
    }
#if AI_TEST_TMA
  std::printf("census ctas=%d tiles=%d issued=%d completed=%d released=%d max_pending=%d\n",
              ctas, K/BK, issued_pairs, delayed_pairs, released_pairs, maximum_pending);
  std::fflush(stdout);
  require(issued_pairs == ctas*(K/BK) && delayed_pairs == issued_pairs &&
          released_pairs == issued_pairs && maximum_pending >= 1 &&
          maximum_pending <= std::min(2,K/BK),
          "lost, duplicate, or undrained tile generation");
#endif
  std::printf("reference_max_error=%g issued_pairs=%d delayed_pairs=%d "
              "released_pairs=%d max_pending=%d gpu_verified=no\n",
              error, issued_pairs, delayed_pairs, released_pairs, maximum_pending);
  return error > 0.001f;
}
