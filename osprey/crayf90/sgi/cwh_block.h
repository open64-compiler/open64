/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_block.h
 * $Revision: 1.4 $
 * $Date: 05/09/22 10:54:46-07:00 $
 * $Author: gautam@jacinth.keyresearch $
 * $Source: crayf90/sgi/SCCS/s.cwh_block.h $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Entry points into cwh_block.cxx
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_BLOCK_INCLUDED
#define CWH_BLOCK_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_block.h $ $Revision: 1.4 $";
#endif /* _KEEP_RCS_ID */

/* block stack routines */

extern void cwh_block_push_region(WN *region);
extern WN * cwh_block_pop_region(void);

extern WN * cwh_block_set_region_pragmas(BOOL assert = TRUE);
extern void cwh_block_push_block(WN *deferred, WN *append, BOOL is_top_pdo) ;
extern void cwh_block_pop_block(void) ;
extern void cwh_block_add_to_enclosing_regions(WN_PRAGMA_ID id, ST * st);


/* block maintenance routines */

extern WN * cwh_block_current(void);
extern void cwh_block_set_current(WN * blk );
extern WN * cwh_block_new_and_current(void) ;
extern WN * cwh_block_exchange_current(WN * blk );

extern void cwh_block_init_pu() ;
extern BOOL cwh_block_toggle_debug(BOOL debug) ;


/* adding items to blocks */

extern void cwh_block_append(WN *wn) ; 
extern void cwh_block_append_given_block(WN *wn, WN* block); 
extern void cwh_block_insert_after(WN *wn, WN*in) ;


/* predefined blocks of code, to be added at convenient times */
/* via  enum to describe possible blocks                      */

enum block_id{
  Defer_Block,              /* deferred after a statement */
  Preamble_Block,           /* Last part of preamble      */
  First_Block,              /* First block after preamble */
  Top_of_Loop_Block         /* 1st block in loop          */
}; 

extern void cwh_block_append_given_id(WN *wn, enum block_id block,BOOL first) ;
extern void cwh_block_append_given(enum block_id id) ;



#endif /* CWH_BLOCK_INCLUDED */

