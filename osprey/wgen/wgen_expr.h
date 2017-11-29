/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 * File modified October 9, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.3.1 release.
 */


/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* WGEN == WHIRL Front End */
/* translate gnu expr trees to whirl */

#ifndef wgen_expr_INCLUDED
#define wgen_expr_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif



/* One time initialization */
extern void WGEN_Expr_Init (void);

/* expand one eddy stmt tree into symtab & whirl */
extern void WGEN_One_Stmt (gs_t exp, WN* target_wn = NULL);

/* same as WGEN_One_Stmt but use new label indexes */
extern void WGEN_One_Stmt_Cleanup (gs_t exp);

/* generate a RET statement */
extern void WGEN_Null_Return (void);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
// expand one gnu expr tree into whirl; called only from C++ files
extern WN * WGEN_Expand_Expr (gs_t exp, 
			     bool need_result = TRUE,
			     TY_IDX nop_ty_idx = 0,
			     TY_IDX component_ty_idx = 0,
			     WN* component_offset = NULL,
			     UINT32 field_id = 0,
			     bool is_bit_field = FALSE,
			     bool is_aggr_init_via_ctor = FALSE
			     , WN *target_wn = NULL
			     );

extern WN  *WGEN_Rcomma_Block;
extern int  WGEN_Disable_Rcomma;
extern WN* WGEN_Expand_Expr_With_Sequence_Point (gs_t exp, TYPE_ID mtype,
						WN *target_wn = NULL);

/* rhs is the WN representing the rhs of a MODIFY_EXPR node; this routine
 * processes the lhs of the node and generate the appropriate form of store
 */
extern WN * WGEN_Lhs_Of_Modify_Expr(gs_code_t assign_code,
				    gs_t lhs,
				    /* if lhs contains CALL, promote the CALL and pass its return value by lhs_retval */
				    /* only used in f()=g()=h(); so far */
				    WN* lhs_retval,
				    bool need_result,
				    TY_IDX component_ty_idx,
				    WN* component_offset,
				    UINT32 field_id,
				    bool is_bit_field,
				    WN *rhs_wn,
				    PREG_NUM rhs_preg_num,
				    bool is_realpart,
				    bool is_imagpart);

extern TYPE_ID Widen_Mtype(TYPE_ID);

/* traverse the tree and addr_saved if address of a variable is taken */
extern void WGEN_Set_ST_Addr_Saved (WN *);

// Convert inplace target order words in 'buf' to host order. 'buf' is a two
// word array. 
extern void WGEN_Convert_To_Host_Order (long *buf);
extern void Setup_EH_Region (bool =false);

extern INT32 wgen_save_expr_stack_last;
extern INT32 wgen_save_expr_level;
extern INT32 wgen_last_save_expr_level;

BOOL& REFERENCED_BY_WHIRL(gs_t);

#ifdef KEY
extern gs_t enclosing_cleanup_point_expr;
#endif

#endif /* __cplusplus */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern void WGEN_Expand_Start_Stmt_Expr (gs_t);
extern void WGEN_Expand_End_Stmt_Expr (gs_t);
extern gs_t first_in_compound_expr(gs_t);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
