/*
  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement
  or the like.  Any license provided herein, whether implied or
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with
  other software, or any other product whatsoever.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include "gspin-tree.h"
#include "gspin-mempool.h"

// following 3 defines must be in sync with each other
#define GS_MEMBLOCK_SIZE 0x1000000// must be power of 2 and multiple of
				 // sizeof(struct gspin) so that we can easily
				 // convert between index and byte address
#define GS_BLOCK_IDX_SHIFT_AMT 24
#define GS_OFST_IN_BLK_MASK 0xffffff

gs_arena_t gs_mempool[GS_ARENA_COUNT] = {

  { NULL, NULL, 0 }, // GS_ARENA (For the core struct gspin type).
  { NULL, NULL, 0 }, // IB_STRING_ARENA (IB_STRING ONLY!!!)

};

// allocate a new memory block of size GS_MEMBLOCK_SIZE and keep track of it
gs_memblock_t *gs_new_memblock(int id)
{
  gs_memblock_t *memblk = (gs_memblock_t *)malloc(sizeof(gs_memblock_t));
  memblk->block_id = id;
  memblk->mem = (char *)calloc(GS_MEMBLOCK_SIZE, 1);
  if (memblk->mem == NULL)
    fprintf(stderr, "Out of memory.\n");
  memblk->next = NULL;
  return memblk;
}

// for GS_ARENA, count must be 4, 8, 16 or 20
void *__gs_mempool_alloc(unsigned int arena_id, unsigned int count)
{
  gs_arena_t *arena = &gs_mempool[arena_id];
  gs_memblock_t *memblk = arena->lastblock;
  if (memblk == NULL) {
    memblk = gs_new_memblock(0);
    arena->firstblock = arena->lastblock = memblk;
    arena->current_index = 0;
  }
  else if ((arena->current_index + count - 1) >=
           ((memblk->block_id + 1) << GS_BLOCK_IDX_SHIFT_AMT)) {
    memblk = gs_new_memblock(memblk->block_id + 1);
    arena->lastblock->next = memblk;
    arena->lastblock = memblk;
    if (count > 1) // this is needed to gaurantee locations to be contiguous
      arena->current_index = memblk->block_id * GS_MEMBLOCK_SIZE;
  }
  gs_void_t *p = &memblk->mem[arena->current_index & GS_OFST_IN_BLK_MASK];
  arena->current_index += count;
  return p;
}

static gs_memblock_t *memblk_cache[GS_ARENA_COUNT] = {NULL, NULL}; // as cache

// convert from index to memory address
gs_void_t *gs_mempool_idx2address(int arena_id, int cell_index)
{
  gs_arena_t *arena = &gs_mempool[arena_id];
  int cur_block_id = cell_index >> GS_BLOCK_IDX_SHIFT_AMT;
  if (memblk_cache[arena_id] == NULL || memblk_cache[arena_id]->block_id > cur_block_id)
    memblk_cache[arena_id] = arena->firstblock;
  while (memblk_cache[arena_id]->block_id != cur_block_id) 
    memblk_cache[arena_id] = memblk_cache[arena_id]->next;
  int block_byte_ofst = cell_index & GS_OFST_IN_BLK_MASK;
  return memblk_cache[arena_id]->mem + block_byte_ofst;
}

extern char *mem_seg;

// convert from memory address to byte offset from very first cell that has
// offset 0.  If called from a gspin producer, the address will belong to one of
// many non-contiguous blocks managed by mempool.  If called from a gspin 
// consumer, there are two possibilities: (1) addresses for anything in the 
// input must belong to the mmap'ed block, whose base is mem_seg; (2) addresses
// for gspin nodes allocated by the consumer are manged by mempool, and thus
// will belong to one of those mempool-managed non-contiguous blocks.
long gs_mempool_address2byteofst(int arena_id, char *p) {
  if (memblk_cache[arena_id] == NULL || 
      !(p >= memblk_cache[arena_id]->mem &&
        p - memblk_cache[arena_id]->mem < GS_MEMBLOCK_SIZE)) {
    memblk_cache[arena_id] = gs_mempool[arena_id].firstblock;
    // printf("+++++++++++++++++++ memblk_cache cache reset\n");
  }
  while (memblk_cache[arena_id] != NULL &&
         !(p >= memblk_cache[arena_id]->mem &&
    	   p - memblk_cache[arena_id]->mem < GS_MEMBLOCK_SIZE))
    memblk_cache[arena_id] = memblk_cache[arena_id]->next;
  if (memblk_cache[arena_id] != NULL)
    return memblk_cache[arena_id]->block_id * GS_MEMBLOCK_SIZE + p - memblk_cache[arena_id]->mem;
  // must be in the mmap'ed block for gspin input in consumer of .spin file
  GS_ASSERT(arena_id == GS_ARENA, "gs_mempool_address2byteofst failure");
  return p - mem_seg;
} 
