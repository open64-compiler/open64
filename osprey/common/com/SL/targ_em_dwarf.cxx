/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include "elf_stuff.h"
#include <elfaccess.h>
#include "libelf/libelf.h"
#include "libdwarf.h"
#include "targ_em_dwarf.h"
#include <assert.h>	// temporary
#define USE_STANDARD_TYPES 1
#include "defs.h"

struct UINT32_unaligned {
  UINT32 val;
} __attribute__ ((aligned (1)));

struct UINT64_unaligned {
  UINT64 val;
} __attribute__ ((aligned (1)));

static Elf32_Rel *
translate_reloc32(Dwarf_Relocation_Data       rentry,
		  Cg_Dwarf_Sym_To_Elfsym_Ofst translate_symndx,
		  Dwarf_Ptr                   buffer,
		  Dwarf_Unsigned              bufsize)
{
  static Elf32_Rel retval;
  Dwarf_Unsigned   elf_symidx, elf_symoff;

  REL32_offset(retval) = rentry->drd_offset;

  translate_symndx(rentry->drd_symbol_index, &elf_symidx, &elf_symoff);
  Set_REL32_sym(retval, elf_symidx);

  assert(rentry->drd_offset <= bufsize - sizeof(long));

  ((UINT32_unaligned *) ((char *) buffer + rentry->drd_offset))->val +=
      (UINT32) elf_symoff;

  switch (rentry->drd_type) {
  case dwarf_drt_data_reloc:
    Set_REL32_type(retval, R_IA_64_DIR32LSB);
    break;
  case dwarf_drt_segment_rel:
    Set_REL32_type(retval, R_IA_64_SEGREL32LSB);
    break;
  default:
    fprintf(stderr, "ERROR: Unrecognized symbolic relocation type\n");
    exit(-1);
  }
  return &retval;
}

static Elf64_AltRel *
translate_reloc64(Dwarf_Relocation_Data       rentry,
		  Cg_Dwarf_Sym_To_Elfsym_Ofst translate_symndx,
		  Dwarf_Ptr                   buffer,
		  Dwarf_Unsigned              bufsize)
{
  static Elf64_AltRel retval;
  Dwarf_Unsigned   elf_symidx, elf_symoff;

  REL_offset(retval) = rentry->drd_offset;

  translate_symndx(rentry->drd_symbol_index, &elf_symidx, &elf_symoff);
  Set_REL64_sym(retval, elf_symidx);

  assert(rentry->drd_offset <= bufsize - sizeof(long long));

  ((UINT64_unaligned *) ((char *) buffer + rentry->drd_offset))->val +=
    (UINT64) elf_symoff;

  switch (rentry->drd_type) {
  case dwarf_drt_data_reloc:
    if(rentry->drd_length == 8) {
      Set_REL64_type(retval, R_IA_64_DIR64LSB);
    } else {
      // if not 8, better be 4
      Set_REL64_type(retval, R_IA_64_DIR32LSB);
    }
    break;
  case dwarf_drt_segment_rel:
    Set_REL64_type(retval, R_IA_64_SEGREL64LSB);
    break;
  default:
    fprintf(stderr, "ERROR: Unrecognized symbolic relocation type\n");
    exit(-1);
  }
  return &retval;
}

Dwarf_Ptr
Em_Dwarf_Symbolic_Relocs_To_Elf(next_buffer_retriever     get_buffer,
				next_bufsize_retriever    get_bufsize,
				advancer_to_next_stream   advance_stream,
				Dwarf_Signed              buffer_scndx,
				Dwarf_Relocation_Data     reloc_buf,
				Dwarf_Unsigned            entry_count,
				int                       is_64bit,
				Cg_Dwarf_Sym_To_Elfsym_Ofst translate_symndx,
				Dwarf_Unsigned           *result_buf_size)
{
  unsigned i;
  unsigned step_size = (is_64bit ? sizeof(Elf64_Rel) : sizeof(Elf32_Rel));

  Dwarf_Ptr  result_buf = (Dwarf_Ptr *) malloc(step_size * entry_count);
  char *cur_reloc = (char *) result_buf;

  Dwarf_Unsigned offset_offset = 0;
  Dwarf_Unsigned bufsize = 0;
  Dwarf_Ptr      buffer = NULL;

  for (i = 0; i < entry_count; ++i) {
    if (reloc_buf->drd_type == dwarf_drt_none) {
    }
    else if (reloc_buf->drd_type == dwarf_drt_first_of_length_pair) {
      if ((i == (entry_count - 1)) ||
	  reloc_buf->drd_offset != (reloc_buf+1)->drd_offset ||
	  (reloc_buf+1)->drd_type != dwarf_drt_second_of_length_pair) {
	fprintf(stderr, "ERROR: Malformed symbol-difference pair");
	exit(-1);
      }
      else {

	Dwarf_Unsigned offset1, offset2;
	Dwarf_Unsigned scn_sym1, scn_sym2;
	translate_symndx(reloc_buf->drd_symbol_index,
			 &scn_sym1,
			 &offset1);
	++reloc_buf;
	++i;
	translate_symndx(reloc_buf->drd_symbol_index,
			 &scn_sym2,
			 &offset2);
	if (scn_sym1 != scn_sym2) {
	  fprintf(stderr,
		  "ERROR: Cannot compute label difference "
		  "across sections");
	  exit(-1);
	}
	while (reloc_buf->drd_offset >= bufsize + offset_offset) {
	  offset_offset += bufsize;
	  if (!advance_stream(buffer_scndx)) {
	    assert(FALSE);
	  }
	  buffer = get_buffer();
	  bufsize = get_bufsize();
	}
	if (is_64bit) {
	  ((UINT64_unaligned *) ((char *) buffer +
				 reloc_buf->drd_offset - offset_offset))->val +=
	    offset2 - offset1;
	}
	else {
	  ((UINT32_unaligned *) ((char *) buffer +
				 reloc_buf->drd_offset - offset_offset))->val +=
	    offset2 - offset1;
	}
      }
    }
    else {

	while (reloc_buf->drd_offset >= bufsize + offset_offset) {
	  offset_offset += bufsize;
	  if (!advance_stream(buffer_scndx)) {
	    assert(FALSE);
	  }
	  buffer = get_buffer();
	  bufsize = get_bufsize();
	}
      if (is_64bit) {
	*((Elf64_AltRel *) cur_reloc) =
	  *translate_reloc64(reloc_buf,
			     translate_symndx,
			     // DON'T TRY THIS AT HOME
			     (Dwarf_Ptr) ((char *) buffer - offset_offset),
			     bufsize + offset_offset);
      }
      else {
	*((Elf32_Rel *) cur_reloc) =
	  *translate_reloc32(reloc_buf,
			     translate_symndx,
			     // DON'T TRY THIS AT HOME
			     (Dwarf_Ptr) ((char *) buffer - offset_offset),
			     bufsize + offset_offset);
      }
      cur_reloc += step_size;
    }
    ++reloc_buf;
  }
  *result_buf_size = cur_reloc - (char *) result_buf;
  return (Dwarf_Ptr) result_buf;
}
