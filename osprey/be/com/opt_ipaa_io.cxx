/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


// ====================================================================
// ====================================================================
//
// Module: opt_ipaa_io.cxx
// $Revision$
// $Date$
// $Author$
// $Source$
//
// Revision history:
//  22-Nov-95 - Original Version
//
// Description:
//
// I/O routines for writing the per-file IPAA summary information for
// WOPT.
//
// ====================================================================
// ====================================================================

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#include <sys/types.h>	    /* for off_t */
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */

#include "defs.h"
#include "mempool.h"

#include "dwarf_DST.h"
#include "stab.h"
#include "irbdata.h"
#include "pu_info.h"
#include "wn.h"
#include "wn_map.h"

#ifdef __MINGW32__
#include <WINDOWS.h>
#endif /* __MINGW32__ */
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "opt_ipaa_io.h"

#define OPT_IPAA_IO
#include "opt_ipaa_summary.h"

// Macros for I/O function interfaces:
#define ERROR_RETURN (void *)(-1)
#define CONVERT_OFFSET(typ, fld) \
    if ((INTPTR)(fld) >= size) return ERROR_RETURN; \
    else (fld) = (typ)(base + (INTPTR)(fld))

// Read table from .B file
extern "C" void *
IPAA_CALLSITES_Read ( char *base, UINT32 size )
{
  IPAA_CALLSITES *cs = (IPAA_CALLSITES *)base;
  cs->_mpool = NULL;

  /* fixup the pointer field */
  if (cs->_callsites == (IPAA_CALLSITE *)-1) {
    cs->_callsites = NULL;
  } else {
    CONVERT_OFFSET(IPAA_CALLSITE*, cs->_callsites);
  }

  return (void *)cs;
}


// Write the table to the .B file
extern "C" void
IPAA_CALLSITES_Write ( void *callsites, Output_File *fl )
{
  IPAA_CALLSITES *cs = (IPAA_CALLSITES *)callsites;
  Elf64_Word cur_offset, nxt_offset;

#define IPAA_CALLSITES_ADDR(offset) ((IPAA_CALLSITES *)(fl->map_addr + offset))

  cur_offset = ir_b_save_buf ( callsites, sizeof(IPAA_CALLSITES),
			       sizeof(mINT32), 0, fl );

  nxt_offset = (Elf64_Word)-1;
  if (cs->_size > 0) {
    nxt_offset = ir_b_save_buf ( (void *)cs->_callsites,
				 cs->_size * sizeof(IPAA_CALLSITE),
				 sizeof(mINT32), 0, fl );
    nxt_offset -= cur_offset;
  }
  IPAA_CALLSITES_ADDR(cur_offset)->_callsites =
      (IPAA_CALLSITE *)(INTPTR)nxt_offset;

}

extern "C" void *
IPAA_LOCAL_MAP_Read ( char *base, UINT32 size )
{
  IPAA_LOCAL_MAP *lm = (IPAA_LOCAL_MAP *)base;
  lm->_mpool = NULL;

  /* fixup the pointer fields */
  if (lm->_symtab_id == (mINT16 *)-1) {
    lm->_symtab_id = NULL;
  } else {
    CONVERT_OFFSET(mINT16*, lm->_symtab_id);
  }
  if (lm->_st_id == (mINT32 *)-1) {
    lm->_st_id = NULL;
  } else {
    CONVERT_OFFSET(mINT32*, lm->_st_id);
  }

  return (void *)lm;
}


// Write the table to the .B file
extern "C" void
IPAA_LOCAL_MAP_Write ( void *localmap, Output_File *fl )
{
  IPAA_LOCAL_MAP *lm = (IPAA_LOCAL_MAP *)localmap;
  Elf64_Word cur_offset, nxt_offset;

#define IPAA_LOCAL_MAP_ADDR(offset) ((IPAA_LOCAL_MAP *)(fl->map_addr + offset))

  cur_offset = ir_b_save_buf ( localmap, sizeof(IPAA_LOCAL_MAP),
			       sizeof(mINT32), 0, fl);

  /* write out the _symtab_id array */
  nxt_offset = (Elf64_Word)-1;
  if (lm->_size > 0) {
    nxt_offset = ir_b_save_buf ( (void *)lm->_symtab_id,
				 lm->_size * sizeof(mINT16),
				 sizeof(mINT16), 0, fl);
  }
  IPAA_LOCAL_MAP_ADDR(cur_offset)->_symtab_id =
      (mINT16 *)(INTPTR)(nxt_offset - cur_offset);

  /* write out the _st_id array */
  nxt_offset = (Elf64_Word)-1;
  if (lm->_size > 0) {
    nxt_offset = ir_b_save_buf ( (void *)lm->_st_id,
				 lm->_size * sizeof(mINT32),
				 sizeof(mINT32), 0, fl);
  }
  IPAA_LOCAL_MAP_ADDR(cur_offset)->_st_id =
      (mINT32 *)(INTPTR)(nxt_offset - cur_offset);

}
