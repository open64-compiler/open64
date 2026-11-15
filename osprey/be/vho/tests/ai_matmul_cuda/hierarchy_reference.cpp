/* Copyright (C) 2026 Open64 Project */
/* H4 host-only execution oracle for actual WHIRL-derived CUDA. Reuse the
 * delayed-copy/CTA-release model; add ordered F32 exceptional-value checks.
 * This is not GPU execution or a GPU race/performance qualification.
 * Design: doc/AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md. */
#define main DSL_h4_finite_reference
#include "executable_pipeline_sim.cpp"
#undef main
#include <limits>

/* Reuse the actual kernel and existing async lifetime checks with new payloads.
 * Numerical comparisons below are independent of the compiler cost model. */
static void DSL_h4_special_case(int pattern) {
  std::vector<float> a(M*K,0.0f), b(K*N,1.0f), c(M*N,0.0f);
  for(int row=0;row<M;++row) for(int q=0;q<K;++q) {
    float x=0.0f;
    if(pattern==0) x=q%3==0?1e20f:q%3==1?1.0f:-1e20f;
    if(pattern==1 && q==0) x=std::numeric_limits<float>::quiet_NaN();
    if(pattern==2 && q==0) x=(row%2?-1.0f:1.0f)*std::numeric_limits<float>::infinity();
    if(pattern==3 && q==0) x=-std::numeric_limits<float>::denorm_min();
    if(pattern==4) x=-0.0f;
    a[row*K+q]=x;
  }
  TensorMap a_map{a.data(),M,K,BM,BK},b_map{b.data(),K,N,BK,BN};
  for(int row=0;row<M;row+=BM) for(int col=0;col<N;col+=BN) {
    slots.clear();proxy_published=false;
    blockIdx={static_cast<unsigned>(col/BN),static_cast<unsigned>(row/BM),0};
    std::vector<std::thread> workers;
    for(int tid=0;tid<Threads;++tid) workers.emplace_back([&,tid] {
      threadIdx={static_cast<unsigned>(tid),0,0};
#if AI_TEST_TMA
      ai_matmul_tile_f32(a.data(),b.data(),c.data(),&a_map,&b_map);
#else
      ai_matmul_tile_f32(a.data(),b.data(),c.data());
#endif
    });
    for(auto &worker:workers) worker.join();
    for(const auto &entry:slots) require(!entry.second.initialized && !entry.second.pending,"H4 special case did not drain");
  }
  for(int row=0;row<M;++row) for(int col=0;col<N;++col) {
    float reference=0.0f;
    for(int q=0;q<K;++q) reference+=a[row*K+q]*b[q*N+col];
    float actual=c[row*N+col];
    require(std::isnan(reference)?std::isnan(actual):std::memcmp(&actual,&reference,sizeof(float))==0,
            "H4 changed ordered F32 value/classification/signed zero");
  }
  std::printf("PASS H4 ordered_payload=%d elements=%d\n",pattern,M*N);
}
/* Run original finite data plus cancellation, NaN, infinity, subnormal and
 * signed-zero payloads through the actual emitted kernel, outside search. */
int main() {
  int status=DSL_h4_finite_reference();if(status) return status;
  for(int pattern=0;pattern<5;++pattern) DSL_h4_special_case(pattern);
  return 0;
}
