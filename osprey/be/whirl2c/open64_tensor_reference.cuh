// Copyright (C) 2026. Distributed under the GNU GPL v2.
// Reference semantics for doc/AI-COMPILER-TENSOR-SOURCE-v0.1.md.
// These serial value operations do not select a GPU execution plan.
#ifndef OPEN64_TENSOR_REFERENCE_CUH
#define OPEN64_TENSOR_REFERENCE_CUH
#include <cstdint>
#include <cmath>
#include <cstring>
#ifdef __CUDACC__
#define O64_HD __host__ __device__
#else
#define O64_HD
#endif
namespace open64_reference {
// Use the host/device library predicate without weakening IEEE finite checks.
O64_HD inline bool finite(float x) {
#ifdef __CUDA_ARCH__
  return ::isfinite(x);
#else
  return std::isfinite(x);
#endif
}
// Preserve NaN propagation for maximum reductions on both execution paths.
O64_HD inline bool nan(float x) {
#ifdef __CUDA_ARCH__
  return ::isnan(x);
#else
  return std::isnan(x);
#endif
}
template<class T, uint64_t... D> struct TENSOR {
  using value_type = T;
  static constexpr int rank = sizeof...(D);
  static constexpr uint64_t size = (uint64_t(1) * ... * D);
  T data[size];
  // Return a logical extent without referring to host-only static storage.
  O64_HD static uint64_t dim(int axis) { const uint64_t d[] = {D...}; return d[axis]; }
};
// Map an output flat index to a right-aligned broadcast input index.
template<class I, class O> O64_HD uint64_t broadcast_index(uint64_t n) {
  uint64_t index = 0, stride = 1;
  for (int a = O::rank - 1; a >= 0; --a) {
    uint64_t c = n % O::dim(a); n /= O::dim(a);
    int b = a - (O::rank - I::rank);
    if (b >= 0) { if (I::dim(b) != 1) index += c * stride; stride *= I::dim(b); }
  }
  return index;
}
// Reinterpret the source's exact binary32 literal on host or device.
O64_HD inline float bits_float(uint32_t bits) {
#ifdef __CUDA_ARCH__
  return __uint_as_float(bits);
#else
  float value; std::memcpy(&value, &bits, sizeof(value)); return value;
#endif
}
// Copy a logical contiguous value through a shape-only operation.
template<class O, class I> O64_HD O reshape(const I &x) {
  static_assert(O::size == I::size, "reshape element count");
  O y; for (uint64_t i=0; i<O::size; ++i) y.data[i]=x.data[i]; return y;
}
// Permute logical axes; this reference deliberately materializes the value.
template<class O, class I> O64_HD O transpose(const I &x, const int *perm) {
  O y;
  for (uint64_t i=0; i<O::size; ++i) {
    uint64_t n=i, coord[I::rank], index=0;
    for (int a=O::rank-1; a>=0; --a) {coord[perm[a]]=n%O::dim(a); n/=O::dim(a);}
    for (int a=0; a<I::rank; ++a) index=index*I::dim(a)+coord[a];
    y.data[i]=x.data[index];
  }
  return y;
}
// Apply one admitted elementwise binary operation with numpy broadcasting.
template<class O, int Op, class A, class B> O64_HD O binary(const A &a, const B &b) {
  O y;
  for (uint64_t i=0; i<O::size; ++i) {
    auto x=a.data[broadcast_index<A,O>(i)], z=b.data[broadcast_index<B,O>(i)];
    if constexpr (Op==0) y.data[i]=x+z;
    if constexpr (Op==1) y.data[i]=x-z;
    if constexpr (Op==2) y.data[i]=x*z;
    if constexpr (Op==3) y.data[i]=x/z;
    if constexpr (Op==4) y.data[i]=x>z;
  }
  return y;
}
// Evaluate one admitted same-shape unary operation without graph fusion.
template<class O, int Op, class I> O64_HD O unary(const I &x) {
  O y;
  for (uint64_t i=0; i<O::size; ++i) {
    if constexpr (Op==0) y.data[i]=::expf(x.data[i]);
    if constexpr (Op==1) y.data[i]=::logf(x.data[i]);
    if constexpr (Op==2) y.data[i]=finite(x.data[i]);
    if constexpr (Op==3) y.data[i]=!x.data[i];
  }
  return y;
}
// Fill an explicit result tensor using a validated source binary32 literal.
template<class O, class I> O64_HD O fill_like(const I &, uint32_t bits) {
  O y; for (uint64_t i=0; i<O::size; ++i) y.data[i]=bits_float(bits); return y;
}
// Select values using the source boolean mask and numpy broadcasting.
template<class O, class C, class A, class B> O64_HD O select(const C &c,const A &a,const B &b) {
  O y;
  for (uint64_t i=0; i<O::size; ++i)
    y.data[i]=c.data[broadcast_index<C,O>(i)] ? a.data[broadcast_index<A,O>(i)] : b.data[broadcast_index<B,O>(i)];
  return y;
}
// Reduce the last logical axis in source order using float32 accumulation.
template<class O, bool Max, class I> O64_HD O reduce(const I &x) {
  O y; const uint64_t width=I::dim(I::rank-1);
  for (uint64_t i=0; i<O::size; ++i) {
    float value=Max ? bits_float(0xff800000u) : 0.0f;
    for (uint64_t j=0; j<width; ++j) {
      float next=x.data[i*width+j];
      if constexpr (Max) { if (nan(next) || next>value) value=next; }
      else value=value+next;
    }
    y.data[i]=value;
  }
  return y;
}
// Locate one right-aligned broadcast matrix batch from an output batch index.
template<class I,class O> O64_HD uint64_t matrix_batch(uint64_t n) {
  uint64_t index=0,stride=1;
  for (int a=O::rank-3; a>=0; --a) {
    uint64_t c=n%O::dim(a); n/=O::dim(a); int b=a-(O::rank-I::rank);
    if (b>=0) {if (I::dim(b)!=1) index+=c*stride; stride*=I::dim(b);}
  }
  return index*I::dim(I::rank-2)*I::dim(I::rank-1);
}
// Evaluate common.matmul.v3; no transpose flags, FMA or split reduction.
template<class O,class A,class B> O64_HD O matmul(const A &a,const B &b) {
  O y; const uint64_t m=O::dim(O::rank-2), n=O::dim(O::rank-1), k=A::dim(A::rank-1);
  for (uint64_t q=0; q<O::size/(m*n); ++q) {
    uint64_t ai=matrix_batch<A,O>(q), bi=matrix_batch<B,O>(q);
    for (uint64_t row=0;row<m;++row) for(uint64_t col=0;col<n;++col) {
      float sum=0.0f;
      for(uint64_t t=0;t<k;++t) sum=sum+a.data[ai+row*k+t]*b.data[bi+t*n+col];
      y.data[q*m*n+row*n+col]=sum;
    }
  }
  return y;
}
} // namespace open64_reference
#endif
