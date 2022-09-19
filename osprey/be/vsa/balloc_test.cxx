/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ==================================================================
// balloc_test.cxx
//
// unit test for block allocator
// $ g++ -DUNIT_TEST balloc_test.cxx
// $ ./a.out
//   ...
//   Test done.
// ==================================================================
#ifdef UNIT_TEST

#include <stdio.h>
#include <time.h>
#include <assert.h>

// wrappers for types and macros used in block_allocator.h
typedef int BOOL;
typedef unsigned long UINT64;
typedef unsigned int UINT32;
#define CLEAR_BUFFER 1
#define Is_True(x, y) \
  if (!(x)) { printf y; abort(); }
//#define Trace(x) printf x
#define Trace(x)

// include block_allocator.h
#include "block_allocator.h"

// stat
BALLOC_STAT stat;

template<UINT32 bs, UINT32 align> void
TestBlock() {
  int i;
  BLOCK_ALLOCATOR<bs, align> mem(&stat);
  const int adjust = BLOCK_ALLOCATOR<bs, align>::IDX_ADJUST;
  UINT64 mem_before = stat.Total_size();

  // test alloc small block
  UINT32 total = 0;
  for (i = 0; i < align; ++i) {
    UINT32 idx = mem.Allocate(i+1);
    assert(idx == i + adjust);
    char *ptr = mem.template Get_ptr<char>(idx);
    *ptr = i;
    total += align;
    Trace(("Alloc(%d) returns %d %p\n", i, idx, ptr));
  }
  for (i = 0; i < align; ++i) {
    char *ptr = mem.template Get_ptr<char>(i + adjust);
    assert(*ptr == i);
  }

  // test alloc large block
  UINT32 base = total / bs;
  if (total % bs)
    ++ base;
  UINT32 incr = bs / align;
  for (i = 0; i < 10; ++i) {
    UINT32 idx = mem.Allocate(bs);
    assert(idx == (base + i) * incr + adjust);
    int *ptr = mem.template Get_ptr<int>(idx);
    *ptr = i;
    Trace(("Alloc(%d) returns %d %p\n", bs, idx, ptr));
  }
  for (i = 0; i < 10; ++i) {
    int *ptr = mem.template Get_ptr<int>((base + i) * incr + adjust);
    assert(*ptr == i);
  }
  // clear current allocator
  mem.Clear();

  // half block allocation
  total = 0;
  for (i = 0; i < 100; ++i) {
    UINT32 sz = bs/2 - 1;
    UINT32 idx = mem.Allocate(sz);
    assert(idx == total/align + adjust);
    int *ptr = mem.template Get_ptr<int>(idx);
    *ptr = i*65536+i;
    total += ((sz%align) ? (sz/align + 1) : (sz/align)) * align;
    Trace(("Alloc(%d) returns %d %p\n", bs/2-1, idx, ptr));
  }
  total = 0;
  for (i = 0; i < 100; ++i) {
    int *ptr = mem.template Get_ptr<int>(total/align + adjust);
    UINT32 sz = bs/2 - 1;
    total += ((sz%align) ? (sz/align + 1) : (sz/align)) * align;
    assert(*ptr == i*65536+i);
  }
  // clear current allocator
  mem.Clear();

  // test alloc random size
  total = 0;
  srand(time(NULL));
  UINT32 sz_array[100];
  UINT32 idx_array[100];
  short *ptr_array[100];
  for (i = 0; i < 100; ++i) {
    UINT32 sz = rand() % bs + 1;
    UINT32 idx = mem.Allocate(sz);
    short *ptr = mem.template Get_ptr<short>(idx);
    *ptr = i*256 + i;
    sz_array[i] = sz;
    idx_array[i] = idx;
    ptr_array[i] = ptr;
    total += ((sz%align) ? (sz/align + 1) : (sz/align)) * align;
    Trace(("Alloc(%d) returns %d %p\n", sz, idx, ptr));
  }
  for (i = 0; i < 100; ++i) {
    short *ptr = mem.template Get_ptr<short>(idx_array[i]);
    assert(ptr == ptr_array[i]);
    assert(*ptr == i*256+i);
  }
  UINT64 mem_after = stat.Total_size();
  // clear current allocator
  mem.Clear();
  printf("memory usage from %ld to %ld to %ld\n", mem_before, mem_after, stat.Total_size());
}

int main() {
  TestBlock<8, 4>();
  TestBlock<8, 8>();
  TestBlock<16, 4>();
  TestBlock<16, 8>();
  TestBlock<1024, 4>();
  TestBlock<1024, 8>();
  TestBlock<4096, 4>();
  TestBlock<4096, 8>();
  TestBlock<16384, 32>();
  TestBlock<16384, 128>();
  printf("Test done.\n");
}

#endif  /* UNIT_TEST */
