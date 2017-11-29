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

#ifndef __GSPIN_GS_MEMPOOL_H__
#define __GSPIN_GS_MEMPOOL_H__

#define GS_ARENA         0
#define IB_STRING_ARENA  1
#define GS_ARENA_COUNT 2

/* represent a block of allocation of size GS_MEMBLOCK_SIZE */
typedef struct gs_memblock gs_memblock_t;
struct gs_memblock {
  int block_id;			/* index into each block in current arena */
  char *mem; 			/* point to base of allocated block */
  gs_memblock_t *next;		/* the next block in linked list */
};

typedef struct gs_arena {
  gs_memblock_t *firstblock; /* the first of the list of allocated blocks */
  gs_memblock_t *lastblock;  /* the last of the list of allocated blocks where */
  			     /* unused memory is given out */
  gs_count_t current_index;  /* not in bytes; the next available unit */
} gs_arena_t;

extern gs_arena_t gs_mempool[GS_ARENA_COUNT];

extern void *__gs_mempool_alloc(unsigned int mempool_id, unsigned int count);
extern void *gs_mempool_idx2address(int arena_id, int cell_index);
extern long gs_mempool_address2byteofst(int arena_id, char *p);

#endif /* __GSPIN_GS_MEMPOOL_H__ */
