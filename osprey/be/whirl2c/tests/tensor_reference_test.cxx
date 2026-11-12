// Copyright (C) 2026. Distributed under the GNU GPL v2.
// Reference arithmetic checks for doc/AI-COMPILER-TENSOR-SOURCE-v0.1.md;
// exercise broadcast batch axes and IEEE edge cases absent from finite MLA.
#include "open64_tensor_reference.cuh"
#include <cassert>
using namespace open64_reference;

// Check known broadcast contractions, permutation, reductions and IEEE values.
int main() {
  TENSOR<float,2,1,3,5> a;
  TENSOR<float,4,5,2> b;
  for(uint64_t i=0;i<a.size;++i) a.data[i]=float(i%7)-3;
  for(uint64_t i=0;i<b.size;++i) b.data[i]=float(i%5)-2;
  auto c=matmul<TENSOR<float,2,4,3,2>>(a,b);
  for(int batch=0;batch<2;++batch) for(int head=0;head<4;++head)
    for(int row=0;row<3;++row) for(int col=0;col<2;++col) {
      float want=0;
      for(int k=0;k<5;++k) want+=a.data[batch*15+row*5+k]*b.data[head*10+k*2+col];
      assert(c.data[((batch*4+head)*3+row)*2+col]==want);
    }
  const int perm[]={3,0,2,1};
  auto t=transpose<TENSOR<float,2,2,3,4>>(c,perm);
  for(int col=0;col<2;++col) for(int batch=0;batch<2;++batch)
    for(int row=0;row<3;++row) for(int head=0;head<4;++head)
      assert(t.data[((col*2+batch)*3+row)*4+head]==c.data[((batch*4+head)*3+row)*2+col]);
  auto sum=reduce<TENSOR<float,2,4,3>,false>(c);
  for(uint64_t i=0;i<sum.size;++i) assert(sum.data[i]==c.data[2*i]+c.data[2*i+1]);
  TENSOR<float,2,3> special{{1,bits_float(0x7fc00000),3,-0.0f,bits_float(0xff800000),-1}};
  auto max=reduce<TENSOR<float,2>,true>(special);
  assert(nan(max.data[0]) && max.data[1]==0.0f && std::signbit(max.data[1]));
  auto flags=unary<TENSOR<bool,2,3>,2>(special);
  assert(flags.data[0] && !flags.data[1] && !flags.data[4]);
  auto filled=fill_like<TENSOR<float,2,3>>(special,0xff800000);
  for(float x:filled.data) assert(std::isinf(x) && x<0);
  TENSOR<bool,1,3> mask{{true,false,true}};
  auto selected=select<TENSOR<float,2,3>>(mask,special,filled);
  assert(selected.data[0]==1 && std::isinf(selected.data[1]) && selected.data[2]==3);
  assert(selected.data[3]==0 && std::isinf(selected.data[4]) && selected.data[5]==-1);
  return 0;
}
