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
} // namespace open64_schedule
#endif
