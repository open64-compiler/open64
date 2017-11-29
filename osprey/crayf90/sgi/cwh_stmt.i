/*

  Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.

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


#define COMPGOTO_IF_ELSE 6
#define PREG2_OFFSET(p1,p2) \
        Max(TY_size(ST_type(pr1)), (UINT64) TY_align(ST_type(pr2)))


USRCPOS current_srcpos ;       /* line of current statement         */

/* sundry WHIRL blocks */

static WN * WN_tree          ; /* Top of WN tree for this PU          */
static WN * WN_pragma_pu     ; /* block for PU pragmas                */
static WN * WN_pragma_ca     ; /* block for call site pragmas         */


/* -------------------------------------------------- */

static BOOL cwh_stmt_sgi_mp_flag ; /* saw -ump flag (SGI mp directives) */
struct nested_do nested_do_descriptor;

BOOL fe_invoke_inliner;		/* flag set when we need to invoke inliner
				   after exit */

BOOL still_in_preamble;         /* flag set by fei_proc_def, cleared by fei_user_code_start */

/* forward references */

static WN * cwh_stmt_str_falsebr_util(OPERATOR opr, W_node expr[2], W_node vall[2], INT32 label,WN *last_node);
static void cwh_stmt_init_srcpos(INT32 lineno) ;
static WN * cwh_stmt_truebr(WN *expr, WN *val, TY_IDX ty, OPERATOR opr, INT32 label_no) ;
static WN * cwh_stmt_falsebr(WN *expr, WN *val, TY_IDX ty, OPERATOR opr, INT32 label_no) ;
static void cwh_stmt_append_truebr(WN *expr, INT64 con, OPERATOR opr, INT32 label_no) ;
static void cwh_stmt_goto(LABEL_IDX label);
static void cwh_stmt_return_altentry(ST *st) ;
static void cwh_stmt_conformance_checks(WN *tree);
static void cwh_stmt_character_store(TYPE result_type);

