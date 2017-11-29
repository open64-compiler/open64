/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>

#include "gspin-tree.h"
#include "gspin-list.h"
#include "gspin-tel.h"		// for gs_program
#include "gspin-assert.h"
#include "gspin-io.h"
#include "gspin-mempool.h"

#ifdef Is_True_On
void flag_reachable_nodes(gs_t t) {
  if (t == NULL)
    return;
  if (gs_em(t) == true)
    return;
  _gs_em(t, true);
  int j;
  for (j = 0; j < gs_code_arity(gs_code(t)); j++)
    flag_reachable_nodes(gs_operand(t, j));
}
#endif

////////////////////////////////////////////////////////////////////////////////
//
// Ref. "Advanced Programming in the UNIX Environment" by W. Richard Stevens.
// Addison-Wesley. 1993. 26th Printing January 2003
// Section 12.9 "Memory Mapped I/O", pp. 407 to 413.
//
////////////////////////////////////////////////////////////////////////////////

// ASSUMPTIONS: Whatever happens, the root of the tree that is written out ought
//              to be leftmost, in mem_seg. i.e. (gs_t) mem_seg[0] should have the
//              root node.
//              
//              Also, we assume that the format is a run of gs nodes followed by
//              strings (all pointed to from code IB_STRING gs nodes).

// gs_write lays out into a linear segment, each arena in gs_mempool. For each 
// arena, compute the total number of bytes that the mem sections consume. 
// Allocate a segment that long + an initial section that contains the length/
// offset of each data section. mmap(2) this segment and copy the gs_mempool 
// arenas over into it converting physical addresses into offsets into the 
// memory segment.

char *mem_seg = NULL;	// base of the memory map

gs_void_t gs_write (const gs_string_t filename)
{
  gs_count_t gs_length, unsigned_char_length, mem_seg_length;
  gs_int_t fd, lseek_rv, write_rv;

#ifdef Is_True_On
  flag_reachable_nodes((gs_t) gs_mempool[GS_ARENA].firstblock->mem);
#endif

  gs_arena_t *gs_arena = &gs_mempool[GS_ARENA];
  gs_arena_t *gs_unsigned_char_arena = &gs_mempool[IB_STRING_ARENA];

  gs_length = gs_arena->current_index;
  unsigned_char_length = gs_unsigned_char_arena->current_index;
  mem_seg_length = gs_length + unsigned_char_length;

  fd = open (filename, O_TRUNC|O_CREAT|O_RDWR, 0666);
  GS_ASSERT (!(fd < 0), "open failed.\n");
  lseek_rv = lseek (fd, mem_seg_length - 1, SEEK_SET);
  GS_ASSERT (lseek_rv != -1, "lseek failed.\n"); 
  write_rv = write (fd, " ", 1);
  GS_ASSERT (write_rv == 1, "write failed.\n");
  mem_seg = mmap (0, mem_seg_length, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
  GS_ASSERT (mem_seg != MAP_FAILED, "mmap failure.\n");

  // Convert physical addresses to indexes within the mem_seg.
  int i = 0;
  int size;
  gs_t p_in_memseg;
  gs_t t = (gs_t) gs_mempool_idx2address(GS_ARENA, 0);
  while (i < gs_arena->current_index) {
    p_in_memseg = (gs_t) (mem_seg + i);
    size = gspin_node_size(gs_code(t));
    memcpy(p_in_memseg, t, size);
#ifdef Is_True_On
    _gs_em(p_in_memseg, false);
#ifdef CHECK_SPIN_LEAKS
    if (gs_em(t) == false) {
      printf("leaked node: ");
      gs_dump(t);
    }
#endif
#endif

    if (gs_code_arity(gs_code(t)) > 0) { // convert kid pointers to indices
      int j;
      for (j = 0; j < gs_code_arity(gs_code(t)); j++)
        if (gs_operand(t, j) != NULL)
	  gs_set_operand(p_in_memseg, j, 
	        (gs_t) gs_mempool_address2byteofst(GS_ARENA, (char *)gs_operand(t, j)));
    }
    else if (gs_code(t) == IB_STRING) { // convert string pointer to index
	gs_string_t s = gs_s(t);
        _gs_u(p_in_memseg, 
	      gs_length + gs_mempool_address2byteofst(IB_STRING_ARENA, s));
        memcpy (mem_seg + gs_u(p_in_memseg), s, gs_slen(p_in_memseg));
    }

    i += gspin_node_size(gs_code(t));
    t = (gs_t) gs_mempool_idx2address(GS_ARENA, i);
    while (i < gs_arena->current_index && gs_code(t) == DOT) {
      i += 4;
      t = (gs_t) gs_mempool_idx2address(GS_ARENA, i);
    }
  }

  close (fd);
  return;
}

gs_unsigned_char_t *gs_read (const gs_string_t filename)
{
  gs_int_t fd, fstat_rv;
  struct stat statbuf;
  char *p, *string_section;
 
  fd = open (filename, O_RDONLY); 
  GS_ASSERT (fd != -1, "open failed.\n");
  fstat_rv = fstat (fd, &statbuf);
  // printf ("statbuf.st_size: %ld\n", statbuf.st_size);
  // printf ("struct gspin size: %u\n", sizeof (struct gspin));
  GS_ASSERT (fstat_rv == 0, "fstat failed.\n");
  mem_seg = mmap (0, statbuf.st_size, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);
  GS_ASSERT (mem_seg != MAP_FAILED, "mmap failure.\n");
  GS_ASSERT (mem_seg != NULL, "mem_seg null!.\n");

  // Convert indexes within the mem_seg to physical addresses.
  p = mem_seg;
  string_section = (char *) (~0L);
  while (p - mem_seg < statbuf.st_size && p < string_section) {
    gs_t q = (gs_t) p;
    if (gs_code_arity(gs_code(q)) > 0) { // convert kid indicese to pointers
      int j;
      for (j = 0; j < gs_code_arity(gs_code(q)); j++)
        if (gs_operand(q, j) != NULL) {
	  GS_ASSERT((long)gs_operand(q, j) < statbuf.st_size, 
	  	    "right offset out of bounds!.\n");
	  gs_set_operand(q, j, (gs_t) (mem_seg + (int) gs_operand(q,j)));
	}
    }
    else if (gs_code(q) == IB_STRING) { // convert string index to pointer
      if (string_section == (char *) (~0L))
	string_section = mem_seg + gs_u(q);
      GS_ASSERT(gs_u(q) < statbuf.st_size, "left offset out of bounds!.\n");
      _gs_s_no_alloc (q, (gs_unsigned_char_t *) (mem_seg + gs_u(q)));
    }

    p += gspin_node_size(gs_code(q));
    while (p - mem_seg < statbuf.st_size && p < string_section && 
           gs_code((gs_t) p) == DOT)
      p += 4;
  }

  close (fd);
  return mem_seg;
}

gs_t gs_read_file (const gs_string_t filename) 
{
  gs_t p;
  GS_ASSERT (filename != NULL, "open failed.\n");
  p = (gs_t) gs_read (filename);
  gs_program = p;
  return p;
}
