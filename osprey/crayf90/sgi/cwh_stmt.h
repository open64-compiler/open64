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
 * Module: cwh_stmt.h
 * $Revision: 1.4 $
 * $Date: 05/03/10 16:47:56-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_stmt.h $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Entry points into cwh_stmt.c
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_STMT_INCLUDED
#define CWH_STMT_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_stmt.h $ $Revision: 1.4 $";
#endif /* _KEEP_RCS_ID */


extern void cwh_stmt_init_pu(ST * st, INT32 lineno ) ;
extern WN * cwh_stmt_end_pu(void)  ;
extern void cwh_stmt_init_file(BOOL is_mp)  ;
extern void cwh_stmt_character_icall(INTRINSIC intrinsic);
extern WN * cwh_stmt_add_arrayexp(WN *wn); 
extern WN * cwh_stmt_call_helper(INT32 numargs, TY_IDX ty, INT32 inline_state,
                                 INT64 flags);
extern WN * cwh_stmt_return_scalar(ST *st, WN * rv, TY_IDX  rty, BOOL write);
extern void cwh_stmt_add_pragma(WN_PRAGMA_ID  wn_pragma_id,
				BOOL is_omp = FALSE,
				ST *st      = (ST_IDX) NULL,
				INT32  arg1 = 0,
				INT32 arg2  = 0) ;
#ifdef KEY /* Bug 2660 */
extern void cwh_stmt_add_options_pragma(ST *st);
#endif /* KEY Bug 2660 */
extern void cwh_stmt_add_xpragma(WN_PRAGMA_ID wn_pragma_id, 
				 BOOL is_omp = FALSE,
				 WN* expr = NULL);
extern void cwh_stmt_postprocess_pu(void);


/* enum to describe where to put pragmas in OPC_FUNCENTRY */

enum site {
  block_ca,                 /* call site pragmas */
  block_pu                  /* pu pragmas in OPC_FUNCENTRY */
}; 


extern BOOL cwh_stmt_add_to_preamble(WN *wn,enum site block);

extern USRCPOS current_srcpos ; /* line of current statement  */

/* structure describes nested DO (for parallel and lno directives) */
struct nested_do {
  int	depth;		/* depth of current nest */
  int	current;	/* the current nest level */
  BOOL  explicit_end;   /* True if an explicit end_* is present for region */
  WN_PRAGMA_ID type;	/* what directive introduced the nest */
  };

extern struct nested_do nested_do_descriptor;
extern WN *top_of_loop_additions;	/* whirl to stick into the top
					   of the innermost loop in a
					   set of pseudonested loops (or
					   the loop of a regular parallel
					   loop) */

extern BOOL fe_invoke_inliner;
	/* flag set when we need to invoke inliner after exit */

extern BOOL still_in_preamble ;  /* preamble code ie: generated before fei_user_code_start */    

#endif /* CWH_STMT_INCLUDED */

