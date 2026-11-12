// Copyright (C) 2026 Open64 Project. Distributed under the GNU GPL v2.
// Executable review semantics for a selected schedule, before GPU mapping.
// Design: doc/AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.1.md. Views are lazy;
// only selected stage outputs allocate tensors. No schedule is chosen here.
#ifndef OPEN64_TENSOR_SCHEDULE_CUH
#define OPEN64_TENSOR_SCHEDULE_CUH
#include "open64_tensor_reference.cuh"
namespace open64_schedule {
using open64_reference::TENSOR;

template<class O> struct Dimensions;
template<class T,uint64_t... D> struct Dimensions<TENSOR<T,D...>> {
  // Preserve compile-time extents for selected chunk-count assertions.
  O64_HD static constexpr uint64_t dim(int axis) { const uint64_t dims[]={D...}; return dims[axis]; }
};

// Expose logical shape to the existing reference broadcast/batch services.
template<class O> struct Shape {
  using value_type = typename O::value_type;
  static constexpr int rank = O::rank;
  static constexpr uint64_t size = O::size;
  O64_HD static constexpr uint64_t dim(int axis) { return Dimensions<O>::dim(axis); }
};
template<class O> struct View : Shape<O> {
  const typename O::value_type *data;
  O64_HD explicit View(const typename O::value_type *p):data(p) {}
  // Read one logical element; the schedule owns all allocation lifetimes.
  O64_HD typename O::value_type at(uint64_t i) const { return data[i]; }
};
// Borrow either a runtime input or a previously materialized stage result.
template<class O> O64_HD View<O> tensor_view(const typename O::value_type *p) { return View<O>(p); }

template<class O,class I> struct Reshape : Shape<O> {
  const I *input;
  O64_HD explicit Reshape(const I &x):input(&x) { static_assert(O::size==I::size,"reshape size"); }
  // Dense reshape changes coordinates without allocating or copying.
  O64_HD typename O::value_type at(uint64_t i) const { return input->at(i); }
};
template<class O,class I> O64_HD Reshape<O,I> reshape_view(const I &x) { return Reshape<O,I>(x); }

template<class O,class I,int... P> struct Transpose : Shape<O> {
  const I *input;
  O64_HD explicit Transpose(const I &x):input(&x) { static_assert(sizeof...(P)==O::rank,"permutation rank"); }
  // Reuse the source logical permutation as a lazy coordinate map.
  O64_HD typename O::value_type at(uint64_t i) const {
    const int permutation[]={P...}; uint64_t coordinates[I::rank], index=0;
    for(int axis=O::rank-1;axis>=0;--axis) { coordinates[permutation[axis]]=i%O::dim(axis); i/=O::dim(axis); }
    for(int axis=0;axis<I::rank;++axis) index=index*I::dim(axis)+coordinates[axis];
    return input->at(index);
  }
};
template<class O,int... P,class I> O64_HD Transpose<O,I,P...> transpose_view(const I &x) { return Transpose<O,I,P...>(x); }

template<class O> struct Constant : Shape<O> {
  float value;
  O64_HD explicit Constant(uint32_t bits):value(open64_reference::bits_float(bits)) {}
  // Keep the original binary32 bits, including signed infinity.
  O64_HD float at(uint64_t) const { return value; }
};
template<class O> O64_HD Constant<O> constant(uint32_t bits) { return Constant<O>(bits); }

template<class O,int Op,class A,class B> struct Binary : Shape<O> {
  const A *a; const B *b;
  O64_HD Binary(const A &x,const B &y):a(&x),b(&y) {}
  // Every typed node returns its own scalar value, retaining F32 boundaries.
  O64_HD typename O::value_type at(uint64_t i) const {
    auto x=a->at(open64_reference::broadcast_index<A,O>(i));
    auto y=b->at(open64_reference::broadcast_index<B,O>(i));
    if constexpr(Op==0) return x+y;
    if constexpr(Op==1) return x-y;
    if constexpr(Op==2) return x*y;
    if constexpr(Op==3) return x/y;
    if constexpr(Op==4) return x>y;
  }
};
// Spell scalar arithmetic by name rather than numeric opcode selectors.
template<class O,class A,class B> O64_HD Binary<O,0,A,B> add(const A &a,const B &b) { return Binary<O,0,A,B>(a,b); }
template<class O,class A,class B> O64_HD Binary<O,1,A,B> subtract(const A &a,const B &b) { return Binary<O,1,A,B>(a,b); }
template<class O,class A,class B> O64_HD Binary<O,2,A,B> multiply(const A &a,const B &b) { return Binary<O,2,A,B>(a,b); }
template<class O,class A,class B> O64_HD Binary<O,3,A,B> divide(const A &a,const B &b) { return Binary<O,3,A,B>(a,b); }
template<class O,class A,class B> O64_HD Binary<O,4,A,B> greater(const A &a,const B &b) { return Binary<O,4,A,B>(a,b); }

template<class O,int Op,class I> struct Unary : Shape<O> {
  const I *input;
  O64_HD explicit Unary(const I &x):input(&x) {}
  // Pure math follows the same expf/logf and IEEE predicates as the reference.
  O64_HD typename O::value_type at(uint64_t i) const {
    auto x=input->at(i);
    if constexpr(Op==0) return ::expf(x);
    if constexpr(Op==1) return ::logf(x);
    if constexpr(Op==2) return open64_reference::finite(x);
    if constexpr(Op==3) return !x;
  }
};
template<class O,class I> O64_HD Unary<O,0,I> exponential(const I &x) { return Unary<O,0,I>(x); }
template<class O,class I> O64_HD Unary<O,1,I> logarithm(const I &x) { return Unary<O,1,I>(x); }
template<class O,class I> O64_HD Unary<O,2,I> is_finite(const I &x) { return Unary<O,2,I>(x); }
template<class O,class I> O64_HD Unary<O,3,I> logical_not(const I &x) { return Unary<O,3,I>(x); }

template<class O,class C,class A,class B> struct Select : Shape<O> {
  const C *condition; const A *yes; const B *no;
  O64_HD Select(const C &c,const A &a,const B &b):condition(&c),yes(&a),no(&b) {}
  // Select is pure; inactive branches introduce no observable memory effects.
  O64_HD typename O::value_type at(uint64_t i) const {
    return condition->at(open64_reference::broadcast_index<C,O>(i)) ?
      yes->at(open64_reference::broadcast_index<A,O>(i)) : no->at(open64_reference::broadcast_index<B,O>(i));
  }
};
template<class O,class C,class A,class B> O64_HD Select<O,C,A,B> choose(const C &c,const A &a,const B &b) { return Select<O,C,A,B>(c,a,b); }

// Materialize only a stage selected by VHO; these loops are a review executor.
template<class O,class I> O64_HD void materialize_to(typename O::value_type *out,const I &x) {
  for(uint64_t i=0;i<O::size;++i) out[i]=x.at(i);
}
// Ascending sums and NaN-preserving maxima match the ordinary WHIRL lowering.
template<bool Max> O64_HD float accumulate(float total,float next) {
  if constexpr(Max) return open64_reference::nan(next) || next>total ? next : total;
  else return total+next;
}
template<bool Max> O64_HD float neutral() { return Max ? open64_reference::bits_float(0xff800000u) : 0.0f; }
// Evaluate the explicitly bounded reduction segment for one output coordinate.
template<class O,bool Max,class I> O64_HD float reduce_segment(const I &x,uint64_t output,uint64_t begin,uint64_t end) {
  float total=neutral<Max>(); const uint64_t width=I::dim(I::rank-1);
  for(uint64_t k=begin;k<end;++k) total=accumulate<Max>(total,x.at(output*width+k));
  return total;
}
template<class O,bool Max,class I> O64_HD void reduce_to(float *out,const I &x) {
  for(uint64_t i=0;i<O::size;++i) out[i]=reduce_segment<O,Max>(x,i,0,I::dim(I::rank-1));
}
// Chunk width/count are supplied by the selected plan; no heuristic is used.
template<class O,bool Max,uint64_t W,uint64_t C,class I> O64_HD void split_reduce_to(float *out,const I &x) {
  const uint64_t width=I::dim(I::rank-1);
  static_assert(W>0 && C==(I::dim(I::rank-1)+W-1)/W,"selected reduction chunks");
  for(uint64_t c=0;c<C;++c) {
    uint64_t end=(c+1)*W<width ? (c+1)*W : width;
    for(uint64_t i=0;i<O::size;++i) out[c*O::size+i]=reduce_segment<O,Max>(x,i,c*W,end);
  }
}
// Reuse the reference batch-coordinate service for a selected dot segment.
template<class O,class A,class B> O64_HD float matmul_segment(const A &a,const B &b,uint64_t i,uint64_t begin,uint64_t end) {
  const uint64_t m=O::dim(O::rank-2), n=O::dim(O::rank-1), width=A::dim(A::rank-1);
  uint64_t batch=i/(m*n), row=(i/n)%m, column=i%n;
  uint64_t ai=open64_reference::matrix_batch<A,O>(batch), bi=open64_reference::matrix_batch<B,O>(batch);
  float total=0.0f;
  for(uint64_t k=begin;k<end;++k) total=total+a.at(ai+row*width+k)*b.at(bi+k*n+column);
  return total;
}
template<class O,class A,class B> O64_HD void matmul_to(float *out,const A &a,const B &b) {
  for(uint64_t i=0;i<O::size;++i) out[i]=matmul_segment<O>(a,b,i,0,A::dim(A::rank-1));
}
template<class O,uint64_t W,uint64_t C,class A,class B> O64_HD void split_matmul_to(float *out,const A &a,const B &b) {
  const uint64_t width=A::dim(A::rank-1);
  static_assert(W>0 && C==(A::dim(A::rank-1)+W-1)/W,"selected matmul chunks");
  for(uint64_t c=0;c<C;++c) {
    uint64_t end=(c+1)*W<width ? (c+1)*W : width;
    for(uint64_t i=0;i<O::size;++i) out[c*O::size+i]=matmul_segment<O>(a,b,i,c*W,end);
  }
}
// Merge chunk-major scratch in selected chunk order, preserving the outer policy.
template<class O,bool Max,uint64_t C,class I> O64_HD void merge_to(float *out,const I &partial) {
  static_assert(I::size==C*O::size,"selected scratch size");
  for(uint64_t i=0;i<O::size;++i) {
    float total=neutral<Max>();
    for(uint64_t c=0;c<C;++c) total=accumulate<Max>(total,partial.at(c*O::size+i));
    out[i]=total;
  }
}
// Match the five full-warp shuffle steps; each lane retains an F32 boundary.
template<bool Max> O64_HD float warp_tree(float (&lanes)[32]) {
  for(unsigned delta=16;delta;delta/=2)
    for(unsigned lane=0;lane+delta<32;++lane) lanes[lane]=accumulate<Max>(lanes[lane],lanes[lane+delta]);
  return lanes[0];
}
// This serial review executor reproduces lane-strided accumulation and tree
// grouping. The selected GPU mapping gives one complete warp to each output.
template<class O,bool Max,uint64_t W,uint64_t C,class I> O64_HD void warp_reduce_to(float *out,const I &x) {
  static_assert(W>0 && C==(I::dim(I::rank-1)+W-1)/W,"warp reduction chunks");
  const uint64_t width=I::dim(I::rank-1);
  for(uint64_t c=0;c<C;++c) for(uint64_t i=0;i<O::size;++i) {
    float lanes[32];
    for(unsigned lane=0;lane<32;++lane) {
      lanes[lane]=neutral<Max>();
      for(uint64_t k=c*W+lane;k<(c+1)*W && k<width;k+=32)
        lanes[lane]=accumulate<Max>(lanes[lane],x.at(i*width+k));
    }
    out[c*O::size+i]=warp_tree<Max>(lanes);
  }
}
// Merges retain the selected chunk-major layout but reduce chunks across lanes.
template<class O,bool Max,uint64_t C,class I> O64_HD void warp_merge_to(float *out,const I &partial) {
  for(uint64_t i=0;i<O::size;++i) {
    float lanes[32];
    for(unsigned lane=0;lane<32;++lane) {
      lanes[lane]=neutral<Max>();
      for(uint64_t c=lane;c<C;c+=32) lanes[lane]=accumulate<Max>(lanes[lane],partial.at(c*O::size+i));
    }
    out[i]=warp_tree<Max>(lanes);
  }
}
// Reproduce the staged GPU dot's lane arithmetic. Actual shared publication
// and four-column reuse are explicit WHIRL in the mapped implementation.
template<class O,uint64_t W,uint64_t C,class A,class B> O64_HD void staged_matmul_to(float *out,const A &a,const B &b) {
  static_assert(W>0 && C==(A::dim(A::rank-1)+W-1)/W,"staged matmul chunks");
  const uint64_t m=O::dim(O::rank-2),n=O::dim(O::rank-1),width=A::dim(A::rank-1);
  for(uint64_t c=0;c<C;++c) for(uint64_t i=0;i<O::size;++i) {
    uint64_t batch=i/(m*n),row=(i/n)%m,column=i%n;
    uint64_t ai=open64_reference::matrix_batch<A,O>(batch),bi=open64_reference::matrix_batch<B,O>(batch);
    float lanes[32];
    for(unsigned lane=0;lane<32;++lane) {
      lanes[lane]=0.0f;
      for(uint64_t k=c*W+lane;k<(c+1)*W && k<width;k+=32)
        lanes[lane]=lanes[lane]+a.at(ai+row*width+k)*b.at(bi+k*n+column);
    }
    out[c*O::size+i]=warp_tree<false>(lanes);
  }
}

// The physical fused bundle exposes logical latent/LSE values without copies.
template<class O,uint64_t R,bool LSE,class I> struct AttentionBundle : Shape<O> {
  const I *input;
  O64_HD explicit AttentionBundle(const I &x):input(&x) {}
  O64_HD float at(uint64_t i) const { return input->at(LSE ? i*(R+1)+R : (i/R)*(R+1)+i%R); }
};
template<class O,uint64_t R,bool LSE,class I> O64_HD AttentionBundle<O,R,LSE,I> attention_bundle_view(const I &x) {
  return AttentionBundle<O,R,LSE,I>(x);
}
// Match native CTA grouping: four warp trees, then thread zero's F32 fold.
template<bool Max> O64_HD float attention_cta_reduce(float (&values)[128]) {
  float result=neutral<Max>();
  for(unsigned warp=0;warp<4;++warp) {
    float lanes[32];for(unsigned lane=0;lane<32;++lane) lanes[lane]=values[warp*32+lane];
    result=accumulate<Max>(result,warp_tree<Max>(lanes));
  }
  return result;
}
// Serial review of the selected fused CTA. The mapped WHIRL owns all actual
// shared storage, masks, barriers and launch geometry; no policy is chosen here.
template<uint64_t B,uint64_t H,uint64_t L,uint64_t R,uint64_t Dr,uint64_t W,uint64_t C,bool Partial,
         class Q,class QR,class Cache,class Rope,class Mask>
O64_HD void fused_attention_to(float *out,const Q &q,const QR &qr,const Cache &cache,
                              const Rope &rope,const Mask &mask,uint32_t scale_bits) {
  static_assert(W>0 && C==(L+W-1)/W,"selected attention chunks");
  const uint64_t capacity=((W+127)/128)*128,rows=B*H;
  const float scale=open64_reference::bits_float(scale_bits);
  for(uint64_t chunk=0;chunk<C;++chunk) for(uint64_t row=0;row<rows;++row) {
    const uint64_t begin=chunk*W,end=(chunk+1)*W<L?(chunk+1)*W:L;
    float weights[capacity],partials[128];
    for(unsigned tid=0;tid<128;++tid) {
      partials[tid]=neutral<true>();
      for(uint64_t t=tid;t<capacity;t+=128) {
        float score=neutral<true>();
        if(begin+t<end && mask.at(row*L+begin+t)) {
          float latent=0.0f,position=0.0f;
          for(uint64_t k=0;k<R;++k) latent=latent+q.at(row*R+k)*cache.at(((row/H)*L+begin+t)*R+k);
          for(uint64_t k=0;k<Dr;++k) position=position+qr.at(row*Dr+k)*rope.at(((row/H)*L+begin+t)*Dr+k);
          score=(latent+position)*scale;
        }
        weights[t]=score;partials[tid]=accumulate<true>(partials[tid],score);
      }
    }
    const float maximum=attention_cta_reduce<true>(partials);
    const float safe_max=open64_reference::finite(maximum)?maximum:0.0f;
    for(unsigned tid=0;tid<128;++tid) {
      partials[tid]=0.0f;
      for(uint64_t t=tid;t<capacity;t+=128) {
        weights[t]=::expf(weights[t]-safe_max);partials[tid]=partials[tid]+weights[t];
      }
    }
    const float denominator=attention_cta_reduce<false>(partials),safe_d=denominator>0.0f?denominator:1.0f;
    for(uint64_t t=0;t<capacity;++t) weights[t]=weights[t]/safe_d;
    const uint64_t base=Partial?(chunk*rows+row)*(R+2):row*(R+1);
    if constexpr(Partial) {out[base]=maximum;out[base+1]=denominator;}
    else out[base+R]=denominator>0.0f?safe_max+::logf(safe_d):neutral<true>();
    for(uint64_t column=0;column<R;++column) {
      float lanes[32];
      for(unsigned lane=0;lane<32;++lane) {
        lanes[lane]=0.0f;
        for(uint64_t t=lane;t<end-begin;t+=32)
          lanes[lane]=lanes[lane]+weights[t]*cache.at(((row/H)*L+begin+t)*R+column);
      }
      out[base+(Partial?2:0)+column]=warp_tree<false>(lanes);
    }
  }
}
// Serial counterpart of the selected one/two/four-warp native reduction.
// A one-warp block publishes its tree result directly; larger blocks fold
// precisely their participating warp summaries in ascending warp order.
template<bool Max,unsigned Threads> O64_HD float attention_block_reduce(float (&values)[Threads]) {
  static_assert(Threads==32 || Threads==64 || Threads==128,"selected block size");
  float result=neutral<Max>();
  for(unsigned warp=0;warp<Threads/32;++warp) {
    float lanes[32];for(unsigned lane=0;lane<32;++lane) lanes[lane]=values[warp*32+lane];
    float value=warp_tree<Max>(lanes);
    if constexpr(Threads==32) return value;
    result=accumulate<Max>(result,value);
  }
  return result;
}
// Serial review of the selected fused CTA. The mapped WHIRL owns all actual
// shared storage, masks, barriers and launch geometry; no policy is chosen here.
template<uint64_t B,uint64_t H,uint64_t L,uint64_t R,uint64_t Dr,uint64_t W,uint64_t C,bool Partial,unsigned Threads,
         class Q,class QR,class Cache,class Rope,class Mask>
O64_HD void block_fused_attention_to(float *out,const Q &q,const QR &qr,const Cache &cache,
                              const Rope &rope,const Mask &mask,uint32_t scale_bits) {
  static_assert(W>0 && C==(L+W-1)/W,"selected attention chunks");
  const uint64_t capacity=((W+Threads-1)/Threads)*Threads,rows=B*H;
  const float scale=open64_reference::bits_float(scale_bits);
  for(uint64_t chunk=0;chunk<C;++chunk) for(uint64_t row=0;row<rows;++row) {
    const uint64_t begin=chunk*W,end=(chunk+1)*W<L?(chunk+1)*W:L;
    float weights[capacity],partials[Threads];
    for(unsigned tid=0;tid<Threads;++tid) {
      partials[tid]=neutral<true>();
      for(uint64_t t=tid;t<capacity;t+=Threads) {
        float score=neutral<true>();
        if(begin+t<end && mask.at(row*L+begin+t)) {
          float latent=0.0f,position=0.0f;
          for(uint64_t k=0;k<R;++k) latent=latent+q.at(row*R+k)*cache.at(((row/H)*L+begin+t)*R+k);
          for(uint64_t k=0;k<Dr;++k) position=position+qr.at(row*Dr+k)*rope.at(((row/H)*L+begin+t)*Dr+k);
          score=(latent+position)*scale;
        }
        weights[t]=score;partials[tid]=accumulate<true>(partials[tid],score);
      }
    }
    const float maximum=attention_block_reduce<true,Threads>(partials);
    const float safe_max=open64_reference::finite(maximum)?maximum:0.0f;
    for(unsigned tid=0;tid<Threads;++tid) {
      partials[tid]=0.0f;
      for(uint64_t t=tid;t<capacity;t+=Threads) {
        weights[t]=::expf(weights[t]-safe_max);partials[tid]=partials[tid]+weights[t];
      }
    }
    const float denominator=attention_block_reduce<false,Threads>(partials),safe_d=denominator>0.0f?denominator:1.0f;
    for(uint64_t t=0;t<capacity;++t) weights[t]=weights[t]/safe_d;
    const uint64_t base=Partial?(chunk*rows+row)*(R+2):row*(R+1);
    if constexpr(Partial) {out[base]=maximum;out[base+1]=denominator;}
    else out[base+R]=denominator>0.0f?safe_max+::logf(safe_d):neutral<true>();
    for(uint64_t column=0;column<R;++column) {
      float lanes[32];
      for(unsigned lane=0;lane<32;++lane) {
        lanes[lane]=0.0f;
        for(uint64_t t=lane;t<end-begin;t+=32)
          lanes[lane]=lanes[lane]+weights[t]*cache.at(((row/H)*L+begin+t)*R+column);
      }
      out[base+(Partial?2:0)+column]=warp_tree<false>(lanes);
    }
  }
}
// Serial emulation of the selected lane-private warp-register schedule.
// Arrays represent lane registers here, not GPU shared storage. Native WHIRL
// owns the two broadcasts and the fence-free register lifetime/ownership proof.
template<uint64_t B,uint64_t H,uint64_t L,uint64_t R,uint64_t DR,uint64_t W,uint64_t C,bool Partial,unsigned Threads,
         class Q,class QR,class Cache,class Rope,class Mask>
O64_HD void warp_register_fused_attention_to(float *out,const Q &q,const QR &qr,const Cache &cache,
                                           const Rope &rope,const Mask &mask,uint32_t scale) {
    static_assert(Threads==32 && W==32 && Partial && C>1,"one token per lane in split attention");
    block_fused_attention_to<B,H,L,R,DR,W,C,Partial,Threads>(out,q,qr,cache,rope,mask,scale);
}
// Merge normalized local distributions with stable exponential weights.
// Empty chunks contribute exactly zero before evaluating a rescaling exp.
template<uint64_t B,uint64_t H,uint64_t L,uint64_t R,uint64_t C,class I>
O64_HD void merge_attention_to(float *out,const I &input) {
  const uint64_t rows=B*H;float weights[C];
  for(uint64_t row=0;row<rows;++row) {
    float partials[128];
    for(unsigned tid=0;tid<128;++tid) {
      partials[tid]=neutral<true>();
      for(uint64_t c=tid;c<C;c+=128) partials[tid]=accumulate<true>(partials[tid],input.at((c*rows+row)*(R+2)));
    }
    const float maximum=attention_cta_reduce<true>(partials),safe_max=open64_reference::finite(maximum)?maximum:0.0f;
    for(unsigned tid=0;tid<128;++tid) {
      partials[tid]=0.0f;
      for(uint64_t c=tid;c<C;c+=128) {
        const float m=input.at((c*rows+row)*(R+2)),d=input.at((c*rows+row)*(R+2)+1);
        weights[c]=d==0.0f?0.0f:d*::expf((open64_reference::finite(m)?m:0.0f)-safe_max);
        partials[tid]=partials[tid]+weights[c];
      }
    }
    const float denominator=attention_cta_reduce<false>(partials),safe_d=denominator>0.0f?denominator:1.0f;
    for(uint64_t c=0;c<C;++c) weights[c]=weights[c]/safe_d;
    out[row*(R+1)+R]=denominator>0.0f?safe_max+::logf(safe_d):neutral<true>();
    for(uint64_t column=0;column<R;++column) {
      float lanes[32];
      for(unsigned lane=0;lane<32;++lane) {
        lanes[lane]=0.0f;
        for(uint64_t c=lane;c<C;c+=32) lanes[lane]=lanes[lane]+weights[c]*input.at((c*rows+row)*(R+2)+2+column);
      }
      out[row*(R+1)+column]=warp_tree<false>(lanes);
    }
  }
}
/* Register ownership is explicit in native WHIRL. This serial review uses the
 * same per-column F32 lane sums and shuffle tree; only physical ownership and
 * cache staging differ. These helpers do not select a GPU schedule. */
template<uint64_t B,uint64_t H,uint64_t L,uint64_t R,uint64_t DR,uint64_t W,uint64_t C,bool Partial,
         class Q,class QR,class Cache,class Rope,class Mask>
O64_HD void register_fused_attention_to(float *out,const Q &q,const QR &qr,const Cache &cache,
                                       const Rope &rope,const Mask &mask,uint32_t scale) {
    fused_attention_to<B,H,L,R,DR,W,C,Partial>(out,q,qr,cache,rope,mask,scale);
}
template<uint64_t B,uint64_t H,uint64_t L,uint64_t R,uint64_t C,class I>
O64_HD void register_merge_attention_to(float *out,const I &input) {
    merge_attention_to<B,H,L,R,C>(out,input);
}
} // namespace open64_schedule
#endif
