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
// block_allocator.h
//
// allocate memory in block and return offset to ease the handling of
// swap in/out
// ==================================================================
#ifndef block_allocator_INCLUDED
#define block_allocator_INCLUDED

// ==================================================================
// BLOCK_ALLOCATOR
//
// allocate block from system and do allocation in the block. return
// index made up by <page_index, offset> to caller so that the memory
// can be swapped in/out without changes in caller where the index is
// is used
//
// Typical usage:
// For the use case of D-U chain, each DNA can hold a block allocator
// to keep D-U info for this function. All DNA's D-U shares the same
// BALLOC_STAT to get the total size of memory used by D-U. When the
// D-U of a function is used, the block allocator is in "locked".
// When the total memory exceeds the upper watermark, "unlocked"
// block allocator can be swapped out and memory is freed to lower
// watermark.
// ==================================================================

#include <stdlib.h>
#include <string.h>
#include <vector>

#ifndef UNIT_TEST
#include "defs.h"
#include "errors.h"
#endif

// ==================================================================
// BALLOC_STAT
// statistic for a group of block allocator
// ==================================================================
class BALLOC_STAT {
private:
  UINT64 _total_size;

public:
  BALLOC_STAT() : _total_size(0) { }

  void   Increase(UINT32 size) { _total_size += size; }
  void   Decrease(UINT32 size) { _total_size -= size; }
  UINT64 Total_size() const    { return _total_size;  }
};  // BALLOC_STAT

// ==================================================================
// BALLOC_FLAG
// flags for block allocator
// ==================================================================
enum BALLOC_FLAG {
  BF_LOCKED    = 0x0001,        // block allocator is locked
};  // BALLOC_FLAG

// ==================================================================
// BALLOC_BASE
// base class for block allocator too wrap block allocation/free
// ==================================================================
class BALLOC_BASE {
protected:
  BALLOC_STAT       *_stat;     // statistics for the whole group
  std::vector<void*> _blocks;   // vector to track page pointers
  UINT32             _avail;    // bytes available in current page
  UINT32             _flags;    // flags for this allocator

protected:
  // constructor
  BALLOC_BASE(BALLOC_STAT *stat)
    : _stat(stat), _avail(0), _flags(0) {
  }

  // destructor
  ~BALLOC_BASE() {
  }

protected:
  // allocate block
  UINT32 Allocate_page(UINT32 psize) {
    void *page = malloc(psize);
    Is_True(page != NULL, ("out of memory"));
#ifdef Is_True_On
    memset(page, 0xcc, psize);
#endif
    UINT32 page_idx = _blocks.size();
    _blocks.push_back(page);
    _avail = psize;
    _stat->Increase(psize);     // increase stats
    return page_idx;
  }

  // clear all pages
  void Clear_pages(UINT32 psize) {
    Is_True(!Is_locked(), ("clear locked allocator"));
    // decrease stats
    _stat->Decrease(psize * _blocks.size());
    std::vector<void*>::iterator it = _blocks.begin();
    std::vector<void*>::iterator end = _blocks.end();
    while (it != end) {
      free(*it);
      ++ it;
    }
    _blocks.clear();
    _avail = 0;
  }

public:
  // check if the allocator is empty
  BOOL Empty() {
    return _avail == 0 && _blocks.empty();
  }

  // check if the allocator is locked
  BOOL Is_locked() const  { return (_flags & BF_LOCKED) == BF_LOCKED; }
  void Lock()             { Is_True(!Is_locked(), ("already locked"));
                            _flags |= BF_LOCKED;                      }
  void Unlock()           { Is_True(Is_locked(), ("not locked"));
                            _flags &= ~BF_LOCKED;                     }
};  // BALLOC_BASE

// BALLOC_LOG2
// helper function to calculate log2 at compile time
template<UINT32 n> struct BALLOC_LOG2 {
  enum { Value = BALLOC_LOG2< (n>>1) >::Value + 1 };
};

// BALLOC_LOG2<4>
// minimal alignment is 4 = 2^2
template<> struct BALLOC_LOG2<4> {
  enum { Value = 2 };
};

// ==================================================================
// BLOCK_ALLOCATOR<BLOCK_SIZE, ALIGN>
// the block size is BLOCK_SIZE. The memory allocated is ALIGN-byte
// aligned
// ==================================================================
template<UINT32 BLOCK_SIZE, UINT32 ALIGN>
class BLOCK_ALLOCATOR : public BALLOC_BASE {
public:
  enum {
    IDX_INVALID = 0,             // reserve 0 for null checking
    IDX_ADJUST  = 1,             // adjust index by 1 because 0 is reserved
   };

private:
  // align memory to be allocated
  UINT32 Round(UINT32 sz) const {
    const UINT32 mask = ALIGN - 1;
    return (sz & mask) == 0 ? sz
                            : (sz & ~mask) + ALIGN;
  }

  // convert index into pointer
  void *Idx_2_ptr(UINT32 idx) const {
    const UINT32 ofst_mask = BLOCK_SIZE - 1;
    const UINT32 ofst_bits = BALLOC_LOG2<ALIGN>::Value;
    const UINT32 page_bits = BALLOC_LOG2<BLOCK_SIZE>::Value;

    const UINT32 page = idx >> (page_bits - ofst_bits);
    const UINT32 ofst = ((idx << ofst_bits) & ofst_mask);
    Is_True(ofst < BLOCK_SIZE, ("bad ofst"));
    Is_True(page < _blocks.size(), ("bad page"));
    return (void*)((char*)_blocks[page] + ofst);
  }

  // comvert <page, ofst> into index
  UINT32 Ptr_2_idx(UINT32 page, UINT32 ofst) const {
    const UINT32 mask = ALIGN - 1;
    const UINT32 pages_bits = BALLOC_LOG2<BLOCK_SIZE>::Value;
    const UINT32 ofst_bits = BALLOC_LOG2<ALIGN>::Value;

    Is_True((ofst & mask) == 0, ("ofst not aligned"));
    return (page << (pages_bits - ofst_bits)) | (ofst >> ofst_bits);
  }

public:
  // allocate 'sz' bytes from block
  UINT32 Allocate(UINT32 sz) {
    Is_True(sz > 0 && sz <= BLOCK_SIZE, ("bad size"));
    UINT32 alloc_sz = Round(sz);
    UINT32 page = (_avail >= alloc_sz) ?
                     _blocks.size() - 1 : Allocate_page(BLOCK_SIZE);
    UINT32 ofst = BLOCK_SIZE - _avail;
    _avail -= alloc_sz;
    return Ptr_2_idx(page, ofst) + IDX_ADJUST;
  }

  // clean all blocks
  void Clear() {
    Clear_pages(BLOCK_SIZE);
  }

  // return pointer indicated by index
  template<typename T>
  T *Get_ptr(UINT32 idx) {
    if (idx == IDX_INVALID)
      return NULL;
    return (T *)Idx_2_ptr(idx - IDX_ADJUST);
  }

  // return pointer indicated by index
  template<typename T>
  const T *Get_ptr(UINT32 idx) const {
    if (idx == IDX_INVALID)
      return NULL;
    return (const T *)Idx_2_ptr(idx - IDX_ADJUST);
  }

  // total size used by this allocator
  UINT64 Total_size() const {
    return _blocks.size() * BLOCK_SIZE;
  }

public:
  // constructor
  BLOCK_ALLOCATOR(BALLOC_STAT *stat) : BALLOC_BASE(stat) {
  }

  // destructor
  ~BLOCK_ALLOCATOR() {
    Clear_pages(BLOCK_SIZE);
  }

};  // BLOCK_ALLOCATOR

// ==================================================================
// BALLOC_LOCKER
// automatically lock/unlock the BLOCK_ALLOCATOR
// ==================================================================
class BALLOC_LOCKER {
private:
  BALLOC_BASE *_base;

  BALLOC_LOCKER(const BALLOC_LOCKER&);             // disable copy ctor
  BALLOC_LOCKER& operator=(const BALLOC_LOCKER&);  // disable assign
  void *operator new(std::size_t);                 // disable new
  void *operator new[](std::size_t);               // disable new[]

public:
  // constructor and lock the allocator
  BALLOC_LOCKER(BALLOC_BASE *base) : _base(base) {
    _base->Lock();
  }

  // destructor and unlock the allocator
  ~BALLOC_LOCKER() {
    _base->Unlock();
  }
};


#endif /* block_allocator_INCLUDED */
