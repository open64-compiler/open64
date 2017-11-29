/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

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



static char USMID[] = "\n@(#)5.0_pl/sources/s_rcnstrct.c	5.5	09/29/99 17:38:13\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "s_globals.m"
# include "debug.m"
# include "s_asg_expr.m"
# include "s_cnstrct.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"
# include "s_rcnstrct.h"

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
# include <fortran.h>
# endif



/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static void    check_for_dependencies(opnd_type	*, size_level_type *);
static void    create_array_constructor_asg(opnd_type *, opnd_type *, int, int);
static void    do_slice_asg(int, opnd_type *, int, int);
static void    determine_slice_size(int, opnd_type *, size_level_type *);
static void    create_interp_stmts(int, int);
static void    do_single_asg(opnd_type *, expr_arg_type *, opnd_type *, int, 
                             int);
static void    create_struct_constructor_asg(opnd_type *, opnd_type *);
static void    increment_subscript(int);
static void    test_size_stmts(int, int, int);
static void    expand_stmts(opnd_type *, expr_arg_type *);
static void    check_for_constructors(opnd_type *, expr_arg_type *);

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This is the main entry for the run time array constructor system.     *|
|*      It drives the routines which generate the necessary assignments and   *|
|*      loops to create a constructor temp that is not constant.              *|
|*	There are three basic approaches ...                                  *|
|*		1. Simple_Expr_Size. The tmp size can be determined by a      *|
|*                 single run time expression. The tmp is then alloc'd at     *|
|*		   size and run time assignments follow.                      *|
|*              2. Interp_Loop_Size. The tmp size is determined by runtime    *|
|*                 interpretation of the implied do loops.                    *|
|*              3. "your guess is as good as mine" size. The tmp size is      *|
|*                 dependent on function calls, so we just allocate a large   *|
|*                 allocatable array tmp and after every assignment test to   *|
|*                 see if we have enough room. If not, we reallocate the      *|
|*                 array with another large increment.                        *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - original tree (array constructor)                          *|
|*	exp_desc - expression descriptor for constructor.                     *|
|*									      *|
|* Output parameters:							      *|
|*	top_opnd - points to tmp reference.                                   *|
|*      exp_desc - some fields are modified.                                  *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

boolean	create_runtime_array_constructor(opnd_type	*top_opnd,
				         expr_arg_type	*exp_desc)

{
   int			alloc_idx;
   int			allocate_tmp_idx;
   int			asg_idx;
   int			base_asg_idx;
   int			base_tmp_idx;
   int			bd_idx;
   int			call_idx;
   opnd_type		char_len_opnd;
   int			char_len_tmp;
   int			cn_idx;
   int			col;
   size_level_type	constructor_size_level;
   int			dealloc_idx;
   int			dump_dv_idx;
   int			dv_idx;
   int			ir_idx;
   opnd_type		l_opnd;
   int			line;
   int			list_idx;
   expr_arg_type	loc_exp_desc;
   int			loc_idx;
   int			max_idx;
   int			minus_idx;
   opnd_type		num_opnd;
   int			num_tmp_idx;
   boolean		ok = TRUE;
   int			realloc_size_attr;
   boolean		save_defer_stmt_expansion;
   boolean		save_in_constructor;
   int			save_curr_stmt_sh_idx;
   int			shift_idx;
   size_offset_type	size;
   int			size_limit_attr;
   opnd_type		size_opnd;
   int			size_tmp_idx;
   size_offset_type     stride;
   int			subscript_idx;
   opnd_type		target_base_opnd;
   long			the_constant;
   int			tmp_idx;
   int			tmp_sub_idx;
   int			type_idx;


   TRACE (Func_Entry, "create_runtime_array_constructor", NULL);

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   ir_idx = OPND_IDX((*top_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);

   save_in_constructor = in_constructor;
   in_constructor = TRUE;

   COPY_OPND(num_opnd, (exp_desc->shape[0]));
   constructor_size_level = (size_level_type) exp_desc->constructor_size_level;

   GEN_COMPILER_TMP_ASG(asg_idx,
                        tmp_sub_idx,
                        TRUE,	/* Semantics is done */
                        line,
                        col,
                        SA_INTEGER_DEFAULT_TYPE,
                        Priv);
   
   IR_FLD_R(asg_idx)	= CN_Tbl_Idx;
   IR_IDX_R(asg_idx)	= CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(asg_idx) = line;
   IR_COL_NUM_R(asg_idx)  = col;

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   if (TYP_TYPE(IR_TYPE_IDX(ir_idx)) == Character) {
      /* determine max char length */

      copy_subtree(&(exp_desc->char_len), &char_len_opnd);
      OPND_LINE_NUM(char_len_opnd) = line;
      OPND_COL_NUM(char_len_opnd)  = col;

      process_char_len(&char_len_opnd);

      expand_stmts(&char_len_opnd, NULL);

      if (OPND_FLD(char_len_opnd) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX(char_len_opnd)) != Subscript_Opr &&
          IR_OPR(OPND_IDX(char_len_opnd)) != Whole_Subscript_Opr &&
          IR_OPR(OPND_IDX(char_len_opnd)) != Section_Subscript_Opr &&
          IR_OPR(OPND_IDX(char_len_opnd)) != Substring_Opr &&
          IR_OPR(OPND_IDX(char_len_opnd)) != Whole_Substring_Opr &&
          IR_OPR(OPND_IDX(char_len_opnd)) != Struct_Opr &&
          IR_OPR(OPND_IDX(char_len_opnd)) != Dv_Deref_Opr) {

         loc_exp_desc = init_exp_desc;

         loc_exp_desc.type_idx = SA_INTEGER_DEFAULT_TYPE;
         loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
         loc_exp_desc.linear_type = SA_INTEGER_DEFAULT_TYPE;

         char_len_tmp = create_tmp_asg(&char_len_opnd, &loc_exp_desc,
                                       &l_opnd, Intent_In, FALSE, FALSE);
         OPND_FLD(char_len_opnd) = AT_Tbl_Idx;
         OPND_IDX(char_len_opnd) = char_len_tmp;
         OPND_LINE_NUM(char_len_opnd) = line;
         OPND_COL_NUM(char_len_opnd)  = col;
      }
      

# ifdef _DEBUG
      if (OPND_FLD(char_len_opnd) == NO_Tbl_Idx) {
         PRINTMSG(line, 902, Internal, col);
      }
# endif

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)       = Character;
      TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
      TYP_CHAR_CLASS(TYP_WORK_IDX) = (OPND_FLD(char_len_opnd) == CN_Tbl_Idx ?
                                      Const_Len_Char : Var_Len_Char);
      TYP_FLD(TYP_WORK_IDX)        = OPND_FLD(char_len_opnd);
      TYP_IDX(TYP_WORK_IDX)        = OPND_IDX(char_len_opnd);

      if (TYP_CHAR_CLASS(TYP_WORK_IDX) == Var_Len_Char) {
         TYP_ORIG_LEN_IDX(TYP_WORK_IDX) = OPND_IDX(char_len_opnd);
      }

      type_idx         = ntr_type_tbl();
      COPY_OPND(exp_desc->char_len, char_len_opnd);
   }
   else {

      type_idx = IR_TYPE_IDX(ir_idx);
   }

   exp_desc->type_idx = type_idx;

   if (constructor_size_level == Simple_Expr_Size) {

      /* try to fold */
      ok = expr_semantics(&num_opnd, &loc_exp_desc);

      if (OPND_FLD(num_opnd) == CN_Tbl_Idx &&
          (TYP_TYPE(IR_TYPE_IDX(ir_idx)) != Character ||
           OPND_FLD(char_len_opnd) == CN_Tbl_Idx)) {

         /* get stack tmp of constant size */

         tmp_idx		= gen_compiler_tmp(line, col, Priv, TRUE);
         AT_SEMANTICS_DONE(tmp_idx)	= TRUE;
         ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_STACK_IDX(curr_scp_idx); 

         ATD_TYPE_IDX(tmp_idx)     = type_idx;
         exp_desc->shape[0].fld    = OPND_FLD(num_opnd);
         exp_desc->shape[0].idx    = OPND_IDX(num_opnd);
         exp_desc->rank            = 1;
         ATD_ARRAY_IDX(tmp_idx)    = create_bd_ntry_for_const(exp_desc,
                                                              line,
                                                              col);

         OPND_FLD(target_base_opnd) = AT_Tbl_Idx;
         OPND_IDX(target_base_opnd) = tmp_idx;
         OPND_LINE_NUM(target_base_opnd) = line;
         OPND_COL_NUM(target_base_opnd)  = col;

         create_array_constructor_asg(top_opnd, 
                                     &target_base_opnd, 
                                      tmp_sub_idx,
                                      0);

      }
      else {
         COPY_OPND(size_opnd, num_opnd);
         OPND_LINE_NUM(size_opnd) = line;
         OPND_COL_NUM(size_opnd) = col;

         /* set up size tmp, alloc actual tmp */

         determine_tmp_size(&size_opnd,  type_idx);

         NTR_IR_TBL(max_idx);
         IR_OPR(max_idx) = Max_Opr;
         IR_LINE_NUM(max_idx)   = line;
         IR_COL_NUM(max_idx)    = col;
         IR_TYPE_IDX(max_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_FLD_L(max_idx) = IL_Tbl_Idx;
         IR_LIST_CNT_L(max_idx) = 2;

         NTR_IR_LIST_TBL(list_idx);
         IR_IDX_L(max_idx) = list_idx;
 
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         COPY_OPND(IL_OPND(list_idx), size_opnd);

         OPND_FLD(size_opnd) = IR_Tbl_Idx;
         OPND_IDX(size_opnd) = max_idx;


         GEN_COMPILER_TMP_ASG(asg_idx,
                              size_tmp_idx,
                              TRUE,	/* Semantics is done */
                              line,
                              col,
                              SA_INTEGER_DEFAULT_TYPE,
                              Priv);

         COPY_OPND(IR_OPND_R(asg_idx), size_opnd);

         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         GEN_COMPILER_TMP_ASG(asg_idx,
                              num_tmp_idx,
                              TRUE,	/* Semantics is done */
                              line,
                              col,
                              SA_INTEGER_DEFAULT_TYPE,
                              Priv);

         COPY_OPND(IR_OPND_R(asg_idx), num_opnd);

         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         tmp_idx		= gen_compiler_tmp(line, col, Priv, TRUE);
         AT_SEMANTICS_DONE(tmp_idx)	= TRUE;
         ATD_TYPE_IDX(tmp_idx)		= type_idx;
         ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_BASED_IDX(curr_scp_idx);
         exp_desc->shape[0].fld		= OPND_FLD(num_opnd);
         exp_desc->shape[0].idx		= OPND_IDX(num_opnd);
         exp_desc->rank			= 1;

         bd_idx				= reserve_array_ntry(1);
         BD_RANK(bd_idx)		= 1;
         BD_LINE_NUM(bd_idx)		= line;
         BD_COLUMN_NUM(bd_idx)		= col;
         BD_LEN_FLD(bd_idx)		= AT_Tbl_Idx;
         BD_LEN_IDX(bd_idx)		= num_tmp_idx;
         BD_ARRAY_CLASS(bd_idx)		= Explicit_Shape;
         BD_ARRAY_SIZE(bd_idx)		= Var_Len_Array;

         BD_LB_FLD(bd_idx,1)		= CN_Tbl_Idx;
         BD_LB_IDX(bd_idx,1)		= CN_INTEGER_ONE_IDX;

         BD_UB_FLD(bd_idx,1)		= AT_Tbl_Idx;
         BD_UB_IDX(bd_idx,1)		= num_tmp_idx;

         BD_XT_FLD(bd_idx,1)		= AT_Tbl_Idx;
         BD_XT_IDX(bd_idx,1)		= num_tmp_idx;

         gen_copyin_bounds_stmt(num_tmp_idx);

         set_stride_for_first_dim(type_idx, &stride);

         BD_SM_FLD(bd_idx, 1)		= stride.fld;
         BD_SM_IDX(bd_idx, 1)		= stride.idx;

         BD_RESOLVED(bd_idx)            = TRUE;

         BD_FLOW_DEPENDENT(bd_idx)	= TRUE;

         ATD_ARRAY_IDX(tmp_idx)		= ntr_array_in_bd_tbl(bd_idx);
         ATD_AUTOMATIC(tmp_idx)		= TRUE;

         GEN_COMPILER_TMP_ASG(base_asg_idx,
                              base_tmp_idx,
                              TRUE,	/* Semantics is done */
                              line,
                              col,
                              SA_INTEGER_DEFAULT_TYPE,
                              Priv);

         ATD_AUTO_BASE_IDX(tmp_idx)	= base_tmp_idx;

         NTR_IR_TBL(alloc_idx);
         IR_OPR(alloc_idx) = Alloc_Opr;
         IR_TYPE_IDX(alloc_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(alloc_idx)   = line;
         IR_COL_NUM(alloc_idx)    = col;
         IR_FLD_L(alloc_idx) = AT_Tbl_Idx;
         IR_IDX_L(alloc_idx) = size_tmp_idx;
         IR_LINE_NUM_L(alloc_idx)   = line;
         IR_COL_NUM_L(alloc_idx)    = col;
         IR_FLD_R(base_asg_idx) = IR_Tbl_Idx;
         IR_IDX_R(base_asg_idx) = alloc_idx;

         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = base_asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         NTR_IR_TBL(dealloc_idx);
         IR_OPR(dealloc_idx) = Dealloc_Opr;
         IR_TYPE_IDX(dealloc_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(dealloc_idx)   = line;
         IR_COL_NUM(dealloc_idx)    = col;
         COPY_OPND(IR_OPND_L(dealloc_idx), IR_OPND_L(base_asg_idx));

         gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         OPND_FLD(target_base_opnd) = AT_Tbl_Idx;
         OPND_IDX(target_base_opnd) = tmp_idx;
         OPND_LINE_NUM(target_base_opnd) = line;
         OPND_COL_NUM(target_base_opnd)  = col;

         create_array_constructor_asg(top_opnd, 
                                      &target_base_opnd, 
                                      tmp_sub_idx,
                                      0);
      }

      OPND_FLD((*top_opnd)) = AT_Tbl_Idx;
      OPND_IDX((*top_opnd)) = tmp_idx;
      OPND_LINE_NUM((*top_opnd)) = line;
      OPND_COL_NUM((*top_opnd))  = col;

      /* gen_whole_subscript calls gen_whole_substring */
      ok = gen_whole_subscript(top_opnd, exp_desc) && ok;

      exp_desc->tmp_reference = TRUE;
      exp_desc->contig_array = TRUE;
   }
   else if (constructor_size_level == Interp_Loop_Size) {

      GEN_COMPILER_TMP_ASG(asg_idx,
                           num_tmp_idx,
                           TRUE,	/* Semantics is done */
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      IR_FLD_R(asg_idx) = CN_Tbl_Idx;
      IR_IDX_R(asg_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_R(asg_idx) = line;
      IR_COL_NUM_R(asg_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      create_interp_stmts(OPND_IDX(num_opnd), num_tmp_idx);

      OPND_FLD(size_opnd) = AT_Tbl_Idx;
      OPND_IDX(size_opnd) = num_tmp_idx;
      OPND_LINE_NUM(size_opnd) = line;
      OPND_COL_NUM(size_opnd) = col;

      determine_tmp_size(&size_opnd, type_idx);

      NTR_IR_TBL(max_idx);
      IR_OPR(max_idx) = Max_Opr;
      IR_TYPE_IDX(max_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(max_idx)   = line;
      IR_COL_NUM(max_idx)    = col;
      IR_FLD_L(max_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_L(max_idx) = 2;

      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_L(max_idx) = list_idx;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx), size_opnd);

      OPND_FLD(size_opnd) = IR_Tbl_Idx;
      OPND_IDX(size_opnd) = max_idx;

      GEN_COMPILER_TMP_ASG(asg_idx,
                           size_tmp_idx,
                           TRUE,	/* Semantics is done */
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      COPY_OPND(IR_OPND_R(asg_idx), size_opnd);

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      tmp_idx			= gen_compiler_tmp(line, col, Priv, TRUE);
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;

      ATD_TYPE_IDX(tmp_idx)	= type_idx;
      ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_BASED_IDX(curr_scp_idx);
      exp_desc->shape[0].fld	= OPND_FLD(num_opnd);
      exp_desc->shape[0].idx	= OPND_IDX(num_opnd);
      exp_desc->rank		= 1;

      bd_idx			= reserve_array_ntry(1);
      BD_RESOLVED(bd_idx)       = TRUE;
      BD_RANK(bd_idx)		= 1;
      BD_LINE_NUM(bd_idx)	= line;
      BD_COLUMN_NUM(bd_idx)	= col;
      BD_LEN_FLD(bd_idx)	= AT_Tbl_Idx;
      BD_LEN_IDX(bd_idx)	= num_tmp_idx;

      BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
      BD_ARRAY_SIZE(bd_idx)	= Var_Len_Array;

      BD_LB_FLD(bd_idx,1)	= CN_Tbl_Idx;
      BD_LB_IDX(bd_idx,1)	= CN_INTEGER_ONE_IDX;

      BD_UB_FLD(bd_idx,1)	= AT_Tbl_Idx;
      BD_UB_IDX(bd_idx,1)	= num_tmp_idx;

      BD_XT_FLD(bd_idx,1)	= AT_Tbl_Idx;
      BD_XT_IDX(bd_idx,1)	= num_tmp_idx;

      gen_copyin_bounds_stmt(num_tmp_idx);

      set_stride_for_first_dim(type_idx, &stride);

      BD_SM_FLD(bd_idx, 1)	= stride.fld;
      BD_SM_IDX(bd_idx, 1)	= stride.idx;

      BD_FLOW_DEPENDENT(bd_idx) = TRUE;

      ATD_ARRAY_IDX(tmp_idx)	= ntr_array_in_bd_tbl(bd_idx);
      ATD_AUTOMATIC(tmp_idx)	= TRUE;

      GEN_COMPILER_TMP_ASG(base_asg_idx,
                           base_tmp_idx,
                           TRUE,	/* Semantics is done */
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      ATD_AUTO_BASE_IDX(tmp_idx)	= base_tmp_idx;

      NTR_IR_TBL(alloc_idx);
      IR_OPR(alloc_idx) = Alloc_Opr;
      IR_TYPE_IDX(alloc_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(alloc_idx)   = line;
      IR_COL_NUM(alloc_idx)    = col;
      IR_FLD_L(alloc_idx) = AT_Tbl_Idx;
      IR_IDX_L(alloc_idx) = size_tmp_idx;
      IR_LINE_NUM_L(alloc_idx)   = line;
      IR_COL_NUM_L(alloc_idx)    = col;
      IR_FLD_R(base_asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(base_asg_idx) = alloc_idx;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = base_asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      NTR_IR_TBL(dealloc_idx);
      IR_OPR(dealloc_idx) = Dealloc_Opr;
      IR_TYPE_IDX(dealloc_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(dealloc_idx)   = line;
      IR_COL_NUM(dealloc_idx)    = col;
      COPY_OPND(IR_OPND_L(dealloc_idx), IR_OPND_L(base_asg_idx));

      gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      OPND_FLD(target_base_opnd) = AT_Tbl_Idx;
      OPND_IDX(target_base_opnd) = tmp_idx;
      OPND_LINE_NUM(target_base_opnd) = line;
      OPND_COL_NUM(target_base_opnd)  = col;

      create_array_constructor_asg(top_opnd, 
                                   &target_base_opnd, 
                                   tmp_sub_idx,
                                   0);


      OPND_FLD((*top_opnd)) = AT_Tbl_Idx;
      OPND_IDX((*top_opnd)) = tmp_idx;
      OPND_LINE_NUM((*top_opnd)) = line;
      OPND_COL_NUM((*top_opnd))  = col;

      ok = gen_whole_subscript(top_opnd, exp_desc) && ok;

      exp_desc->tmp_reference = TRUE;
      exp_desc->contig_array = TRUE;
   }
   else {
      /* this is the guess type */

      tmp_idx			= gen_compiler_tmp(line, col, Priv, TRUE);
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;
#ifdef KEY /* Bug 5515 */
      /* We allocate the temp at a "guessed" size. Then as we store into the
       * temp at runtime, we check for overflow and realloc if needed. Thus,
       * no user error can cause an out-of-bounds reference. A bounds check
       * would use the extent in the dope vector for the temp, which is
       * erroneous because it's set to the "guessed" size and does not get
       * updated as we reallocate the temp.
       */
      ATD_BOUNDS_CHECK(tmp_idx) = FALSE;
      ATD_NOBOUNDS_CHECK(tmp_idx) = TRUE;
#endif /* KEY Bug 5515 */

      ATD_TYPE_IDX(tmp_idx)	= type_idx;

      assign_storage_blk(tmp_idx);

      ATD_IM_A_DOPE(tmp_idx)	= TRUE;
      ATD_ALLOCATABLE(tmp_idx)	= TRUE;

      ATD_ARRAY_IDX(tmp_idx)	= BD_DEFERRED_1_IDX;

      save_curr_stmt_sh_idx	= curr_stmt_sh_idx;
      curr_stmt_sh_idx		= SH_PREV_IDX(curr_stmt_sh_idx);

      gen_entry_dope_code(tmp_idx);

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

      /* set up original size */

      GEN_COMPILER_TMP_ASG(asg_idx,
                           size_limit_attr,
                           TRUE,	/* Semantics is done */
                           stmt_start_line,
                           stmt_start_col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      IR_FLD_R(asg_idx) = CN_Tbl_Idx;
      IR_IDX_R(asg_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                      CONSTRUCTOR_GUESS_SIZE);
      IR_LINE_NUM_R(asg_idx) = line;
      IR_COL_NUM_R(asg_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Low_Bound;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx)  = col;

      IR_FLD_L(dv_idx) = AT_Tbl_Idx;
      IR_IDX_L(dv_idx) = tmp_idx;
      IR_LINE_NUM_L(dv_idx) = line;
      IR_COL_NUM_L(dv_idx)  = col;

      IR_FLD_R(dv_idx) = CN_Tbl_Idx;
      IR_IDX_R(dv_idx) = CN_INTEGER_ONE_IDX;
      IR_LINE_NUM_R(dv_idx) = line;
      IR_COL_NUM_R(dv_idx)  = col;

      IR_DV_DIM(dv_idx) = 1;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Extent;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx)  = col;

      IR_FLD_L(dv_idx) = AT_Tbl_Idx;
      IR_IDX_L(dv_idx) = tmp_idx;
      IR_LINE_NUM_L(dv_idx) = line;
      IR_COL_NUM_L(dv_idx)  = col;

      IR_FLD_R(dv_idx) = CN_Tbl_Idx;
      IR_IDX_R(dv_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                     CONSTRUCTOR_GUESS_SIZE);
      IR_LINE_NUM_R(dv_idx) = line;
      IR_COL_NUM_R(dv_idx)  = col;

      IR_DV_DIM(dv_idx) = 1;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Stride_Mult;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx)  = col;

      IR_FLD_L(dv_idx)	= AT_Tbl_Idx;
      IR_IDX_L(dv_idx)	= tmp_idx;
      IR_LINE_NUM_L(dv_idx) = line;
      IR_COL_NUM_L(dv_idx)  = col;
      type_idx		= ATD_TYPE_IDX(tmp_idx);

      switch (TYP_TYPE(type_idx)) {

         case Typeless:
            IR_FLD_R(dv_idx) = CN_Tbl_Idx;
            IR_IDX_R(dv_idx) = C_INT_TO_CN(NULL_IDX,
                                     STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx)));
            IR_LINE_NUM_R(dv_idx) = line;
            IR_COL_NUM_R(dv_idx)  = col;
            break;

         case Integer:
         case Logical:
         case CRI_Ptr:
         case CRI_Ch_Ptr:
         case Real:
         case Complex:
            the_constant	= TARGET_BITS_TO_WORDS(storage_bit_size_tbl[
                                                       TYP_LINEAR(type_idx)]);
            IR_FLD_R(dv_idx)	= CN_Tbl_Idx;
            IR_IDX_R(dv_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              the_constant);
            IR_LINE_NUM_R(dv_idx) = line;
            IR_COL_NUM_R(dv_idx)  = col;
            break;

         case Character:  /* This is really number of bytes */
            IR_FLD_R(dv_idx)	= TYP_FLD(type_idx);
            IR_IDX_R(dv_idx)	= TYP_IDX(type_idx);
            IR_LINE_NUM_R(dv_idx) = line;
            IR_COL_NUM_R(dv_idx)  = col;

            break;

         case Structure:
            size.fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
            size.idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

            BITS_TO_WORDS(size, TARGET_BITS_PER_WORD);

            if (size.fld == NO_Tbl_Idx) {
               IR_FLD_R(dv_idx)	= CN_Tbl_Idx;
               IR_IDX_R(dv_idx)	= ntr_const_tbl(size.type_idx,
                                                FALSE,
                                                size.constant);
            }
            else {
               IR_FLD_R(dv_idx)	= size.fld;
               IR_IDX_R(dv_idx)	= size.idx;
            }

            IR_LINE_NUM_R(dv_idx) = line;
            IR_COL_NUM_R(dv_idx)  = col;
            break;

      }  /* end switch */

      IR_DV_DIM(dv_idx) = 1;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


      allocate_tmp_idx = create_alloc_descriptor(1,line,col,FALSE);

      /* put loc of dope vector into tmp_array */


      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)		= Aloc_Opr;
      IR_TYPE_IDX(loc_idx)	= CRI_Ptr_8;
      IR_LINE_NUM(loc_idx)	= line;
      IR_COL_NUM(loc_idx)	= col;
      IR_FLD_L(loc_idx)		= AT_Tbl_Idx;
      IR_IDX_L(loc_idx)		= tmp_idx;
      IR_LINE_NUM_L(loc_idx)	= line;
      IR_COL_NUM_L(loc_idx)	= col;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = loc_idx;

      NTR_IR_TBL(subscript_idx);
      IR_OPR(subscript_idx) = Subscript_Opr;
      IR_TYPE_IDX(subscript_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(subscript_idx) = line;
      IR_COL_NUM(subscript_idx)  = col;
      IR_FLD_L(subscript_idx) = AT_Tbl_Idx;
      IR_IDX_L(subscript_idx) = allocate_tmp_idx;
      IR_LINE_NUM_L(subscript_idx) = line;
      IR_COL_NUM_L(subscript_idx)  = col;

      IR_FLD_L(asg_idx) = IR_Tbl_Idx;
      IR_IDX_L(asg_idx) = subscript_idx;

      the_constant = 2;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (TYP_LINEAR(ATD_TYPE_IDX(allocate_tmp_idx)) == Integer_4) {
         the_constant++;
      }
# endif

      NTR_IR_LIST_TBL(list_idx);
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, the_constant);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      IR_FLD_R(subscript_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_R(subscript_idx) = 1;
      IR_IDX_R(subscript_idx) = list_idx;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;

      /* set up call to _ALLOCATE */

      NTR_IR_TBL(call_idx);
      IR_OPR(call_idx) = Call_Opr;
      IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(call_idx)   = line;
      IR_COL_NUM(call_idx)    = col;
      IR_LINE_NUM_L(call_idx)   = line;
      IR_COL_NUM_L(call_idx)    = col;
      IR_FLD_L(call_idx) = AT_Tbl_Idx;

      if (glb_tbl_idx[Allocate_Attr_Idx] == NULL_IDX) {
         glb_tbl_idx[Allocate_Attr_Idx] = create_lib_entry_attr(
                                                   ALLOCATE_LIB_ENTRY,
                                                   ALLOCATE_NAME_LEN,
                                                   line,
                                                   col);
      }

      ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Allocate_Attr_Idx]);

      IR_IDX_L(call_idx) = glb_tbl_idx[Allocate_Attr_Idx];
      IR_FLD_R(call_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_R(call_idx) = 2;
      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_R(call_idx) = list_idx;

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)		= Aloc_Opr;
      IR_TYPE_IDX(loc_idx)	= CRI_Ptr_8;
      IR_LINE_NUM(loc_idx)   = line;
      IR_COL_NUM(loc_idx)    = col;
      IR_FLD_L(loc_idx)		= AT_Tbl_Idx;
      IR_IDX_L(loc_idx)		= allocate_tmp_idx;
      IR_LINE_NUM_L(loc_idx)   = line;
      IR_COL_NUM_L(loc_idx)    = col;
      IL_FLD(list_idx)		= IR_Tbl_Idx;
      IL_IDX(list_idx)		= loc_idx;
   

      /* no stat */

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      gen_sh(Before, Call_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = call_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Deref_Opr;
      IR_TYPE_IDX(dv_idx) = ATD_TYPE_IDX(tmp_idx);
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx)  = col;

      IR_FLD_L(dv_idx) = AT_Tbl_Idx;
      IR_IDX_L(dv_idx) = tmp_idx;
      IR_LINE_NUM_L(dv_idx) = line;
      IR_COL_NUM_L(dv_idx)  = col;

      OPND_FLD(target_base_opnd) = IR_Tbl_Idx;
      OPND_IDX(target_base_opnd) = dv_idx;

      create_array_constructor_asg(top_opnd,
                                  &target_base_opnd,
                                   tmp_sub_idx,
                                   size_limit_attr);


      /* set the index variable back 1 to real size */

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(tmp_sub_idx);
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      IR_FLD_L(asg_idx) = AT_Tbl_Idx;
      IR_IDX_L(asg_idx) = tmp_sub_idx;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;

      NTR_IR_TBL(minus_idx);
      IR_OPR(minus_idx) = Minus_Opr;
      IR_TYPE_IDX(minus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(minus_idx) = line;
      IR_COL_NUM(minus_idx) = col;
      IR_FLD_L(minus_idx) = AT_Tbl_Idx;
      IR_IDX_L(minus_idx) = tmp_sub_idx;
      IR_LINE_NUM_L(minus_idx) = line;
      IR_COL_NUM_L(minus_idx) = col;
      IR_FLD_R(minus_idx) = CN_Tbl_Idx;
      IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
      IR_LINE_NUM_R(minus_idx) = line;
      IR_COL_NUM_R(minus_idx) = col;

      IR_FLD_R(asg_idx)   = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)   = minus_idx;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* compute new bit size */

      GEN_COMPILER_TMP_ASG(asg_idx,
                           realloc_size_attr,
                           TRUE,   /* Semantics is done */
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Mult_Opr;
      IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)   = line;
      IR_COL_NUM(ir_idx)    = col;
      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = tmp_sub_idx;
      IR_LINE_NUM_L(ir_idx)   = line;
      IR_COL_NUM_L(ir_idx)    = col;

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Access_El_Len;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx)   = line;
      IR_COL_NUM(dv_idx)    = col;
      IR_FLD_L(dv_idx) = AT_Tbl_Idx;
      IR_IDX_L(dv_idx) = tmp_idx;
      IR_LINE_NUM_L(dv_idx)   = line;
      IR_COL_NUM_L(dv_idx)    = col;

      IR_FLD_R(ir_idx) = IR_Tbl_Idx;
      IR_IDX_R(ir_idx) = dv_idx;

      if (char_len_in_bytes) {
         if (TYP_TYPE(type_idx) == Character) {
            /* el len is in bytes */
            NTR_IR_TBL(shift_idx);
            IR_TYPE_IDX(shift_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(shift_idx) = line;
            IR_COL_NUM(shift_idx) = col;
            IR_OPR(shift_idx) = Shiftl_Opr;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_L(shift_idx) = IL_Tbl_Idx;
            IR_IDX_L(shift_idx) = list_idx;
            IR_LIST_CNT_L(shift_idx) = 2;

            COPY_OPND(IL_OPND(list_idx), IR_OPND_R(ir_idx));

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx) = col;

            IL_FLD(list_idx) = CN_Tbl_Idx;
            IL_IDX(list_idx) = CN_INTEGER_THREE_IDX;
            IR_IDX_R(ir_idx) = shift_idx;
         }
      }

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = ir_idx;

      gen_sh(Before, Assignment_Stmt, stmt_start_line,
             stmt_start_col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* realloc the tmp to final size */
      /* new size is in bits. (elements * element bit size) */

      if (glb_tbl_idx[Realloc_Attr_Idx] == NULL_IDX) {
         glb_tbl_idx[Realloc_Attr_Idx] = 
                           create_lib_entry_attr(REALLOC_LIB_ENTRY,
                                                 REALLOC_NAME_LEN,
                                                 line,
                                                 col);
      }

      ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Realloc_Attr_Idx]);
   
      NTR_IR_TBL(call_idx);
      IR_OPR(call_idx) = Call_Opr;
      IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(call_idx) = line;
      IR_COL_NUM(call_idx)  = col;
      IR_FLD_L(call_idx) = AT_Tbl_Idx;
      IR_IDX_L(call_idx) = glb_tbl_idx[Realloc_Attr_Idx];
      IR_LINE_NUM_L(call_idx) = line;
      IR_COL_NUM_L(call_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(call_idx) = IL_Tbl_Idx;
      IR_IDX_R(call_idx) = list_idx;
      IR_LIST_CNT_R(call_idx) = 2;

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)		= Aloc_Opr;
      IR_TYPE_IDX(ir_idx)	= CRI_Ptr_8;
      IR_LINE_NUM(ir_idx)	= line;
      IR_COL_NUM(ir_idx)	= col;

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = ir_idx;
   
      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = tmp_idx;
      IR_LINE_NUM_L(ir_idx)	= line;
      IR_COL_NUM_L(ir_idx)	= col;
      
   
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   
      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Aloc_Opr;
      IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
      IR_LINE_NUM(ir_idx) = line;
      IR_COL_NUM(ir_idx)  = col;
   
      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = ir_idx;
   
      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = realloc_size_attr;
      IR_LINE_NUM_L(ir_idx)	= line;
      IR_COL_NUM_L(ir_idx)	= col;
   
      gen_sh(Before, Call_Stmt, stmt_start_line,
             stmt_start_col, FALSE, FALSE, TRUE);
   
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = call_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


      /* reset the extent */

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Extent;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_DV_DIM(dv_idx) = 1;
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx)  = col;

      IR_FLD_L(dv_idx) = AT_Tbl_Idx;
      IR_IDX_L(dv_idx) = tmp_idx;
      IR_LINE_NUM_L(dv_idx) = line;
      IR_COL_NUM_L(dv_idx)  = col;

      IR_FLD_R(dv_idx) = AT_Tbl_Idx;
      IR_IDX_R(dv_idx) = tmp_sub_idx;
      IR_LINE_NUM_R(dv_idx) = line;
      IR_COL_NUM_R(dv_idx)  = col;

      gen_sh(Before, Assignment_Stmt, stmt_start_line,
             stmt_start_col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Deref_Opr;
      IR_TYPE_IDX(dv_idx) = ATD_TYPE_IDX(tmp_idx);
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx)  = col;

      IR_FLD_L(dv_idx) = AT_Tbl_Idx;
      IR_IDX_L(dv_idx) = tmp_idx;
      IR_LINE_NUM_L(dv_idx) = line;
      IR_COL_NUM_L(dv_idx)  = col;

      OPND_FLD((*top_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*top_opnd)) = dv_idx;

      NTR_IR_TBL(subscript_idx);
      IR_OPR(subscript_idx) = Whole_Subscript_Opr;
      IR_TYPE_IDX(subscript_idx) = ATD_TYPE_IDX(tmp_idx);
      IR_LINE_NUM(subscript_idx) = line;
      IR_COL_NUM(subscript_idx)  = col;

      COPY_OPND(IR_OPND_L(subscript_idx), (*top_opnd));

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(subscript_idx) = IL_Tbl_Idx;
      IR_IDX_R(subscript_idx) = list_idx;
      IR_LIST_CNT_R(subscript_idx) = 1;

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Triplet_Opr;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx) = col;

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = dv_idx;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_L(dv_idx) = IL_Tbl_Idx;
      IR_IDX_L(dv_idx) = list_idx;
      IR_LIST_CNT_L(dv_idx) = 3;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = AT_Tbl_Idx;
      IL_IDX(list_idx) = tmp_sub_idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      COPY_OPND((exp_desc->shape[0]), IL_OPND(list_idx));
      exp_desc->rank = 1;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      OPND_FLD((*top_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*top_opnd)) = subscript_idx;


      exp_desc->reference   = TRUE;
      exp_desc->allocatable = TRUE;
      exp_desc->contig_array = TRUE;

      if (exp_desc->type == Character) {
         ok = gen_whole_substring(top_opnd, exp_desc->rank) && ok;
      }


      if (glb_tbl_idx[Dealloc_Attr_Idx] == NULL_IDX) {
         glb_tbl_idx[Dealloc_Attr_Idx] = create_lib_entry_attr(
                                                    DEALLOC_LIB_ENTRY,
                                                    DEALLOC_NAME_LEN,
                                                    line,
                                                    col);
      }

      ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Dealloc_Attr_Idx]);

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;

# ifdef _ALLOCATE_IS_CALL
      /* create array to send to DEALLOC */

      allocate_tmp_idx = create_alloc_descriptor(1, line, col,FALSE);

      /* put loc of dope vector, tmp_idx, into allocate_tmp_idx(1) */

      NTR_IR_TBL(subscript_idx);
      IR_OPR(subscript_idx) = Subscript_Opr;
      IR_TYPE_IDX(subscript_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(subscript_idx) = line;
      IR_COL_NUM(subscript_idx)  = col;
      IR_FLD_L(subscript_idx)    = AT_Tbl_Idx;
      IR_IDX_L(subscript_idx)    = allocate_tmp_idx;
      IR_LINE_NUM_L(subscript_idx) = line;
      IR_COL_NUM_L(subscript_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(subscript_idx) = IL_Tbl_Idx;
      IR_IDX_R(subscript_idx) = list_idx;
      IR_LIST_CNT_R(subscript_idx) = 1;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      the_constant     = 2L;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (TYP_LINEAR(ATD_TYPE_IDX(allocate_tmp_idx)) == Integer_4) {
         the_constant++;
      }
# endif

      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                     the_constant);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;

      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      IR_FLD_L(asg_idx)    = IR_Tbl_Idx;
      IR_IDX_L(asg_idx)    = subscript_idx;
      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)      = Loc_Opr;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;

      IR_FLD_R(asg_idx)    = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)    = loc_idx;

      IR_FLD_L(loc_idx)    = AT_Tbl_Idx;
      IR_IDX_L(loc_idx)    = tmp_idx;
      IR_LINE_NUM_L(loc_idx) = line;
      IR_COL_NUM_L(loc_idx)  = col;

      gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      /* now set up call to DEALLOC */

      NTR_IR_TBL(call_idx);
      IR_OPR(call_idx) = Call_Opr;
      IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(call_idx) = line;
      IR_COL_NUM(call_idx)  = col;
      IR_FLD_L(call_idx) = AT_Tbl_Idx;
      IR_IDX_L(call_idx) = glb_tbl_idx[Dealloc_Attr_Idx];
      IR_LINE_NUM_L(call_idx) = line;
      IR_COL_NUM_L(call_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(call_idx) = IL_Tbl_Idx;
      IR_IDX_R(call_idx) = list_idx;
      IR_LIST_CNT_R(call_idx) = 1;

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)    = Aloc_Opr;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      IR_FLD_L(loc_idx)  = AT_Tbl_Idx;
      IR_IDX_L(loc_idx)  = allocate_tmp_idx;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;
      IR_LINE_NUM_L(loc_idx) = line;
      IR_COL_NUM_L(loc_idx)  = col;
      IL_FLD(list_idx)   = IR_Tbl_Idx;
      IL_IDX(list_idx)   = loc_idx;

      gen_sh(After, Call_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = call_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

# else
      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Deallocate_Opr;
      IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_L(asg_idx) = IL_Tbl_Idx;
      IR_IDX_L(asg_idx) = list_idx;
      IR_LIST_CNT_L(asg_idx) = 1;

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)             = Aloc_Opr;
      IR_TYPE_IDX(loc_idx)        = CRI_Ptr_8;
      IR_FLD_L(loc_idx)           = AT_Tbl_Idx;
      IR_IDX_L(loc_idx)           = tmp_idx;
      IR_LINE_NUM(loc_idx)        = line;
      IR_COL_NUM(loc_idx)         = col;
      IR_LINE_NUM_L(loc_idx)      = line;
      IR_COL_NUM_L(loc_idx)       = col;
      IL_FLD(list_idx)            = IR_Tbl_Idx;
      IL_IDX(list_idx)            = loc_idx;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(asg_idx) = IL_Tbl_Idx;
      IR_IDX_R(asg_idx) = list_idx;
      IR_LIST_CNT_R(asg_idx) = 3;
      IL_FLD(list_idx) = AT_Tbl_Idx;
      IL_IDX(list_idx) = glb_tbl_idx[Dealloc_Attr_Idx];
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = gen_alloc_header_const(Integer_8,
                                                1, 
                                                FALSE,
                                                &cn_idx);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      gen_sh(After, Call_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
# endif

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }

   in_constructor = save_in_constructor;

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(top_opnd);

   TRACE (Func_Exit, "create_runtime_array_constructor", NULL);

   return(ok);

}  /* create_runtime_array_constructor */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	The main entry point for run time struct constructors. This is a much *|
|*      simpler problem than the run time array constructors since we know    *|
|*      the size at compile time. We just need to generate the run time       *|
|*      assignments.                                                          *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - original tree.                                             *|
|*									      *|
|* Output parameters:							      *|
|*	top_opnd - tmp reference.                                             *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

boolean create_runtime_struct_constructor(opnd_type	*top_opnd)

{
   int			col;
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   boolean		save_defer_stmt_expansion;
   int			tmp_idx;


   TRACE (Func_Entry, "create_runtime_struct_constructor", NULL);

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   ir_idx			= OPND_IDX((*top_opnd));
   line				= IR_LINE_NUM(ir_idx);
   col				= IR_COL_NUM(ir_idx);
   tmp_idx			= gen_compiler_tmp(line, col, Priv, TRUE);
   AT_SEMANTICS_DONE(tmp_idx)	= TRUE;
   ATD_TYPE_IDX(tmp_idx)	= IR_TYPE_IDX(ir_idx);
   ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_STACK_IDX(curr_scp_idx);

   OPND_FLD(opnd) = AT_Tbl_Idx;
   OPND_IDX(opnd) = tmp_idx;
   OPND_LINE_NUM(opnd) = line;
   OPND_COL_NUM(opnd)  = col;

   create_struct_constructor_asg(top_opnd,
                                 &opnd);

   OPND_FLD((*top_opnd)) = AT_Tbl_Idx;
   OPND_IDX((*top_opnd)) = tmp_idx;
   OPND_LINE_NUM((*top_opnd)) = line;
   OPND_COL_NUM((*top_opnd))  = col;

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(top_opnd);

   TRACE (Func_Exit, "create_runtime_struct_constructor", NULL);

   return(ok);

}  /* create_runtime_struct_constructor */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Analyse implied do loops to see if their size can be determined with  *|
|*	a simple expression, runtime loop interpretation, or just a guess/    *|
|*      realloc.                                                              *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - tree to analyse.                                           *|
|*									      *|
|* Output parameters:							      *|
|*	size_opnd - pass back the size expression for this loop. Could involve*|
|*		    other implied do loops.                                   *|
|*      constructor_size_level - simple expression, interpret loops, or guess *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void analyse_loops(opnd_type       *top_opnd,
		   opnd_type       *size_opnd,
                   size_level_type *constructor_size_level)

{
   int			col;
   int			div_idx;
   opnd_type		end_opnd;
   opnd_type		inc_opnd;
   int			ir_idx;
   int			line;
   int			lcv_attr;
   int			list_idx;
   int			list2_idx;
   int			max_idx;
   int			minus_idx;
   opnd_type		mopnd;
   int			mult_idx;
   opnd_type		opnd;
   int			plus_idx;
   opnd_type		popnd;
   size_level_type      size_level_l;
   size_level_type      size_level_r;
   opnd_type		size_opnd_l;
   opnd_type		size_opnd_r;
   opnd_type		slice_size_opnd;
   opnd_type		start_opnd;


   TRACE (Func_Entry, "analyse_loops", NULL);

   find_opnd_line_and_column(top_opnd, &line, &col);

   *size_opnd = null_opnd;
   OPND_LINE_NUM((*size_opnd)) = line;
   OPND_COL_NUM((*size_opnd))  = col;

   switch(OPND_FLD((*top_opnd))) {
      case CN_Tbl_Idx :
         OPND_FLD((*size_opnd)) = CN_Tbl_Idx;
         OPND_IDX((*size_opnd)) = CN_INTEGER_ONE_IDX;
         break;

      case AT_Tbl_Idx  :
         OPND_FLD((*size_opnd)) = CN_Tbl_Idx;
         OPND_IDX((*size_opnd)) = CN_INTEGER_ONE_IDX;
         break;

      case IR_Tbl_Idx  :

         ir_idx = OPND_IDX((*top_opnd));

         switch(IR_OPR(ir_idx)) {

            case Array_Construct_Opr   :
            case Constant_Array_Construct_Opr   :

               /* determine size expr for slice */
               determine_slice_size(IR_IDX_R(ir_idx), size_opnd, 
                                    constructor_size_level);

               break;

            case Struct_Construct_Opr  :
            case Constant_Struct_Construct_Opr  :

               OPND_FLD((*size_opnd)) = CN_Tbl_Idx;
               OPND_IDX((*size_opnd)) = CN_INTEGER_ONE_IDX;
               break;

            case Implied_Do_Opr        :

               determine_slice_size(IR_IDX_L(ir_idx), &slice_size_opnd,
                                    constructor_size_level);

               line = IR_LINE_NUM(ir_idx);
               col  = IR_COL_NUM(ir_idx);

               list_idx = IR_IDX_R(ir_idx);
               lcv_attr = IL_IDX(list_idx);

               list_idx = IL_NEXT_LIST_IDX(list_idx);
               COPY_OPND(start_opnd, IL_OPND(list_idx));
               
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               COPY_OPND(end_opnd, IL_OPND(list_idx));
               
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               COPY_OPND(inc_opnd, IL_OPND(list_idx));
               
               if (*constructor_size_level == Simple_Expr_Size) {
                  /* see if trip count is invariant */

                  minus_idx = gen_ir(OPND_FLD(end_opnd), OPND_IDX(end_opnd),
                                 Minus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                     OPND_FLD(start_opnd),OPND_IDX(start_opnd));

                  plus_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                                Plus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                    OPND_FLD(inc_opnd), OPND_IDX(inc_opnd));

                  div_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                               Div_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                   OPND_FLD(inc_opnd), OPND_IDX(inc_opnd));

                  NTR_IR_TBL(max_idx);
                  IR_OPR(max_idx)       = Max_Opr;
                  IR_TYPE_IDX(max_idx)  = SA_INTEGER_DEFAULT_TYPE;
                  IR_LINE_NUM(max_idx)  = line;
                  IR_COL_NUM(max_idx)   = col;
                  IR_FLD_L(max_idx)     = IL_Tbl_Idx;
                  IR_LIST_CNT_L(max_idx)= 2;
                  NTR_IR_LIST_TBL(list2_idx);
                  IR_IDX_L(max_idx)     = list2_idx;
                  IL_FLD(list2_idx)     = CN_Tbl_Idx;
                  IL_IDX(list2_idx)     = CN_INTEGER_ZERO_IDX;
                  IL_LINE_NUM(list2_idx) = line;
                  IL_COL_NUM(list2_idx)  = col;

                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
                  list2_idx = IL_NEXT_LIST_IDX(list2_idx);

                  IL_FLD(list2_idx) = IR_Tbl_Idx;
                  IL_IDX(list2_idx) = div_idx;

                  OPND_FLD(mopnd) = IR_Tbl_Idx;
                  OPND_IDX(mopnd) = max_idx;

                  check_for_dependencies(&mopnd, constructor_size_level);
               }
   
               if (*constructor_size_level == Guess_Size) {
                  *size_opnd = null_opnd;
                  goto EXIT;
               }
               else if (*constructor_size_level == Simple_Expr_Size) {
                  mult_idx = gen_ir(OPND_FLD(mopnd), OPND_IDX(mopnd),
                                Mult_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                    OPND_FLD(slice_size_opnd),
                                            OPND_IDX(slice_size_opnd));

                  OPND_FLD((*size_opnd)) = IR_Tbl_Idx;
                  OPND_IDX((*size_opnd)) = mult_idx;

               }
               else {
                  /* set up implied do around slice size expr */
                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx) = Implied_Do_Opr;
                  IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
                  IR_LINE_NUM(ir_idx) = line;
                  IR_COL_NUM(ir_idx)  = col;

                  NTR_IR_LIST_TBL(list_idx);
                  IR_FLD_R(ir_idx) = IL_Tbl_Idx;
                  IR_IDX_R(ir_idx) = list_idx;
                  IR_LIST_CNT_R(ir_idx) = 4;
     
                  IL_FLD(list_idx) = AT_Tbl_Idx;
                  IL_IDX(list_idx) = lcv_attr;
                  IL_LINE_NUM(list_idx) = line;
                  IL_COL_NUM(list_idx)  = col;
                  
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);

                  COPY_OPND(IL_OPND(list_idx), start_opnd);

                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);

                  COPY_OPND(IL_OPND(list_idx), end_opnd);

                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);

                  COPY_OPND(IL_OPND(list_idx), inc_opnd);

                  COPY_OPND(IR_OPND_L(ir_idx), slice_size_opnd);

                  OPND_FLD((*size_opnd)) = IR_Tbl_Idx;
                  OPND_IDX((*size_opnd)) = ir_idx;
               }
               
               break;

            
            case Uplus_Opr             :
            case Uminus_Opr            :
            case Paren_Opr             :
            case Not_Opr               :
            case Bnot_Opr              :
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               analyse_loops(&opnd, size_opnd, constructor_size_level);
               break;

            case Power_Opr             :
            case Mult_Opr              :
            case Div_Opr               :
            case Minus_Opr             :
            case Plus_Opr              :
            case Concat_Opr            :
            case Eq_Opr                :
            case Ne_Opr                :
            case Lg_Opr                :
            case Lt_Opr                :
            case Le_Opr                :
            case Gt_Opr                :
            case Ge_Opr                :
            case And_Opr               :
            case Or_Opr                :
            case Eqv_Opr               :
            case Neqv_Opr              :
            case Band_Opr              :
            case Bor_Opr               :
            case Beqv_Opr              :
            case Bneqv_Opr             :
               size_level_l = *constructor_size_level;
               size_level_r = *constructor_size_level;
               
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               analyse_loops(&opnd, &size_opnd_l, &size_level_l);
               COPY_OPND(opnd, IR_OPND_R(ir_idx));
               analyse_loops(&opnd, &size_opnd_r, &size_level_r);

               if (OPND_FLD(size_opnd_l) == CN_Tbl_Idx &&
                   compare_cn_and_value(OPND_IDX(size_opnd_l), 1, Eq_Opr)) {
                  COPY_OPND((*size_opnd), size_opnd_r);
                  *constructor_size_level = size_level_r;
               }
               else if (OPND_FLD(size_opnd_r) == CN_Tbl_Idx &&
                        compare_cn_and_value(OPND_IDX(size_opnd_r), 
                                             1, 
                                             Eq_Opr)) {
                  COPY_OPND((*size_opnd), size_opnd_l);
                  *constructor_size_level = size_level_l;
               }
               else if (size_level_l < size_level_r) {
                  COPY_OPND((*size_opnd), size_opnd_l);
                  *constructor_size_level = size_level_l;
               }
               else {
                  COPY_OPND((*size_opnd), size_opnd_r);
                  *constructor_size_level = size_level_r;
               }
               break;

            case Whole_Subscript_Opr   :
            case Section_Subscript_Opr :

               mopnd = null_opnd;

               list_idx = IR_IDX_R(ir_idx);

               while (list_idx) {

                  COPY_OPND(opnd, IL_OPND(list_idx));
                  analyse_loops(&opnd, &popnd, constructor_size_level);

                  if (*constructor_size_level == Guess_Size) {
                     *size_opnd = null_opnd;
                     goto EXIT;
                  }

                  if (OPND_FLD(mopnd) == NO_Tbl_Idx) {
                     COPY_OPND(mopnd, popnd);
                  }
                  else {
                     NTR_IR_TBL(mult_idx);
                     IR_OPR(mult_idx) = Mult_Opr;
                     IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
                     IR_LINE_NUM(mult_idx)   = line;
                     IR_COL_NUM(mult_idx)    = col;
                     COPY_OPND(IR_OPND_L(mult_idx), mopnd);
                     COPY_OPND(IR_OPND_R(mult_idx), popnd);
                     OPND_FLD(mopnd) = IR_Tbl_Idx;
                     OPND_IDX(mopnd) = mult_idx;
                  }
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }

               COPY_OPND((*size_opnd), mopnd);
               break;

            case Subscript_Opr         :
            case Struct_Opr            :
            case Whole_Substring_Opr   :
            case Substring_Opr         :
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               analyse_loops(&opnd, size_opnd, constructor_size_level);
               break;

            case Call_Opr              :
            /* what about function result sizes that depend on constructors */
               break;

            default :
               *constructor_size_level = Guess_Size;
               *size_opnd = null_opnd;
               break;
         }
         break;

      case IL_Tbl_Idx :
         break;
   }

EXIT:

   TRACE (Func_Exit, "analyse_loops", NULL);

   return;

}  /* analyse_loops */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Called from analyse_loops, this routine looks at the size expression  *|
|*      and determines the constructor_size_level.                            *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - top of size expression.                                        *|
|*									      *|
|* Output parameters:							      *|
|*	constructor_size_level - simple expr, interp loops, or guess.         *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void check_for_dependencies(opnd_type	   *opnd,
				   size_level_type *constructor_size_level)

{
   int			attr_idx;
   int			col;
   int			line;
   int			list_idx;
   opnd_type		topnd;

   TRACE (Func_Entry, "check_for_dependencies", NULL);

   if (*constructor_size_level == Guess_Size) {
      TRACE (Func_Exit, "check_for_dependencies", NULL);
      return;
   }


   switch (OPND_FLD((*opnd))) {
      case AT_Tbl_Idx :
         attr_idx = OPND_IDX((*opnd));
         find_opnd_line_and_column(opnd, &line, &col);

         if (ATD_IMP_DO_LCV(attr_idx)) {
            *constructor_size_level = Interp_Loop_Size;
         }
         else if (ATD_CLASS(attr_idx) == Compiler_Tmp &&
                  ATD_FLD(attr_idx) == IR_Tbl_Idx &&
                  IR_OPR(ATD_TMP_IDX(attr_idx)) == Asg_Opr &&
                  line <= AT_DEF_LINE(attr_idx)) {

            /* replace with the right hand side */
            COPY_OPND((*opnd), IR_OPND_R(ATD_TMP_IDX(attr_idx)));
            check_for_dependencies(opnd, constructor_size_level);
         }
         break;
      case IR_Tbl_Idx :
         if (IR_OPR(OPND_IDX((*opnd))) == Call_Opr) {
            *constructor_size_level = Guess_Size;
         }
         else if (IR_OPR(OPND_IDX((*opnd))) == Stmt_Expansion_Opr) {
            *constructor_size_level = Guess_Size;
         }
         else if (IR_OPR(OPND_IDX((*opnd))) == Dv_Access_El_Len ||
                  IR_OPR(OPND_IDX((*opnd))) == Dv_Access_Low_Bound ||
                  IR_OPR(OPND_IDX((*opnd))) == Dv_Access_Extent ||
                  IR_OPR(OPND_IDX((*opnd))) == Dv_Access_Stride_Mult) {

            *constructor_size_level = Guess_Size;
         }
         else {
            COPY_OPND(topnd, IR_OPND_L(OPND_IDX((*opnd))));
            check_for_dependencies(&topnd, constructor_size_level);
            COPY_OPND(IR_OPND_L(OPND_IDX((*opnd))), topnd);

            if (*constructor_size_level != Guess_Size) {
               COPY_OPND(topnd, IR_OPND_R(OPND_IDX((*opnd))));
               check_for_dependencies(&topnd, constructor_size_level);
               COPY_OPND(IR_OPND_R(OPND_IDX((*opnd))), topnd);
            }
         }

         break;

      case IL_Tbl_Idx :
         list_idx = OPND_IDX((*opnd));
         while (list_idx) {

            COPY_OPND(topnd, IL_OPND(list_idx));
            check_for_dependencies(&topnd, constructor_size_level);
            COPY_OPND(IL_OPND(list_idx), topnd);

            if (*constructor_size_level == Guess_Size) {
               break;
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      case CN_Tbl_Idx :
      case NO_Tbl_Idx :
         break;
   }

   TRACE (Func_Exit, "check_for_dependencies", NULL);

   return;

}  /* check_for_dependencies */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	After the tmp has been allocated, this routine drives the generation  *|
|*	of the run time assignments.                                          *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - top of constructor tree.                                   *|
|*	target_base_opnd - base of right hand side array.                     *|
|*	target_sub_idx   - subscript attr idx.                                *|
|*	size_limit_attr  - attr used for size test (for guess thing)          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void create_array_constructor_asg(opnd_type *top_opnd,
					 opnd_type *target_base_opnd,
					 int	    target_sub_idx,
                                         int	    size_limit_attr)

{
   int			attr_idx;
   int			col;
   opnd_type		end_opnd;
   expr_arg_type	exp_desc;
   opnd_type		inc_opnd;
   int			ir_idx;
   int			lcv_attr;
   int			line;
   int			list_idx;
   opnd_type		opnd;
   int			place_idx;
   int			save_curr_stmt_sh_idx;
   int			sh_idx;
   opnd_type		start_opnd;
   int			sub_idx;


   TRACE (Func_Entry, "create_array_constructor_asg", NULL);

   if (OPND_FLD((*top_opnd)) == IR_Tbl_Idx) {
      ir_idx = OPND_IDX((*top_opnd));
   }
   else {
      find_opnd_line_and_column(top_opnd, &line, &col);
      PRINTMSG(line, 985, Internal, col);
      return;
   }

   if (IR_OPR(ir_idx) == Array_Construct_Opr) {

      do_slice_asg(IR_IDX_R(ir_idx), target_base_opnd, target_sub_idx,
                   size_limit_attr);
   }
   else if (IR_OPR(ir_idx) == Constant_Array_Construct_Opr) {

      exp_desc = arg_info_list[IR_IDX_L(ir_idx)].ed;
      create_constructor_constant(top_opnd, &exp_desc);

      do_single_asg(top_opnd, &exp_desc, target_base_opnd, target_sub_idx,
                    size_limit_attr);
      
   }
   else if (IR_OPR(ir_idx) == Implied_Do_Opr) {
      gen_sh(Before, Assignment_Stmt, IR_LINE_NUM(ir_idx), IR_COL_NUM(ir_idx),
             FALSE, FALSE, TRUE);
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx  = SH_PREV_IDX(curr_stmt_sh_idx);
      NTR_IR_TBL(place_idx);
      IR_OPR(place_idx) = Null_Opr;
      SH_IR_IDX(curr_stmt_sh_idx) = place_idx;

      list_idx = IR_IDX_R(ir_idx);
      lcv_attr = IL_IDX(list_idx);
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(start_opnd, IL_OPND(list_idx));
      expand_stmts(&start_opnd, NULL);

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(end_opnd, IL_OPND(list_idx));
      expand_stmts(&end_opnd, NULL);

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(inc_opnd, IL_OPND(list_idx));
      expand_stmts(&inc_opnd, NULL);

      create_loop_stmts(lcv_attr, 
                       &start_opnd,
                       &end_opnd,
                       &inc_opnd,
                        curr_stmt_sh_idx,       /* body start sh idx */
                        curr_stmt_sh_idx);      /* body end sh idx */

      do_slice_asg(IR_IDX_L(ir_idx), target_base_opnd, target_sub_idx,
                   size_limit_attr);

      /* now remove null place holder stmt. should still be curr_stmt */

      sh_idx = curr_stmt_sh_idx;

      remove_sh(sh_idx);
      FREE_IR_NODE(SH_IR_IDX(sh_idx));
      FREE_SH_NODE(sh_idx);

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
   else if (IR_OPR(ir_idx) == Struct_Construct_Opr) {

      line = IR_LINE_NUM(ir_idx);
      col  = IR_COL_NUM(ir_idx);

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx) = col;
      COPY_OPND(IR_OPND_L(sub_idx), (*target_base_opnd));
      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;
      IL_FLD(list_idx) = AT_Tbl_Idx;
      IL_IDX(list_idx) = target_sub_idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
      
      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = sub_idx;

      create_struct_constructor_asg(top_opnd, &opnd);

      increment_subscript(target_sub_idx);

      if (size_limit_attr) {
         attr_idx = find_left_attr(target_base_opnd);
         test_size_stmts(attr_idx, target_sub_idx, size_limit_attr);
      }
   }
   else if (IR_OPR(ir_idx) == Constant_Struct_Construct_Opr) {

      create_constructor_constant(top_opnd, &exp_desc);
      do_single_asg(top_opnd, &exp_desc, target_base_opnd, target_sub_idx,
                    size_limit_attr);
   }
   else {
      PRINTMSG(IR_LINE_NUM(ir_idx), 986, Internal, IR_COL_NUM(ir_idx));
   }

   TRACE (Func_Exit, "create_array_constructor_asg", NULL);

   return;

}  /* create_array_constructor_asg */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine drives the generation of a slice of an array constructor *|
|*      (inside an implied do).                                               *|
|*									      *|
|* Input parameters:							      *|
|*      list_idx - start of slice list.                                       *|
|*      target_base_opnd - base of right hand side array.                     *|
|*      target_sub_idx   - subscript attr idx.                                *|
|*      size_limit_attr  - attr used for size test (for guess thing)          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void do_slice_asg(int		list_idx,
			 opnd_type	*target_base_opnd,
			 int		target_sub_idx,
			 int		size_limit_attr)

{
   expr_arg_type        exp_desc;
   int			info_idx;
   opnd_type            opnd;


   TRACE (Func_Entry, "do_slice_asg", NULL);

   while (list_idx) {

      if (IL_FLD(list_idx) == IR_Tbl_Idx                        &&
          (IR_OPR(IL_IDX(list_idx)) == Array_Construct_Opr  ||
           IR_OPR(IL_IDX(list_idx)) == Struct_Construct_Opr ||
           IR_OPR(IL_IDX(list_idx)) == Constant_Array_Construct_Opr  ||
           IR_OPR(IL_IDX(list_idx)) == Constant_Struct_Construct_Opr ||
           IR_OPR(IL_IDX(list_idx)) == Implied_Do_Opr))         {

         COPY_OPND(opnd, IL_OPND(list_idx));
         create_array_constructor_asg(&opnd, target_base_opnd, target_sub_idx,
				      size_limit_attr);
      }
      else {

         info_idx = IL_ARG_DESC_IDX(list_idx);
         COPY_OPND(opnd, IL_OPND(list_idx));
         exp_desc = arg_info_list[info_idx].ed;
         expand_stmts(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         do_single_asg(&opnd, &exp_desc, target_base_opnd, target_sub_idx,
                       size_limit_attr);
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "do_slice_asg", NULL);

   return;

}  /* do_slice_asg */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Called from analyse_loops to create the size expression for a loop    *|
|*      slice.                                                                *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx - start of slice list.                                       *|
|*									      *|
|* Output parameters:							      *|
|*	size_opnd - top of size expression returned.                          *|
|*	constructor_size_level - simple expr, interp loops, or just guess.    *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void determine_slice_size(int		 list_idx,
				 opnd_type 	 *size_opnd,
                                 size_level_type *constructor_size_level)

{
   expr_arg_type        exp_desc;
   int                  i;
   opnd_type            mopnd;
   int                  mult_idx;
   opnd_type            opnd;
   int                  plus_idx;
   opnd_type            popnd;
   long_type            scalar_cnt;


   TRACE (Func_Entry, "determine_slice_size", NULL);

   scalar_cnt = 0L;

   popnd = null_opnd;

   while (list_idx) {

      if (IL_FLD(list_idx) == IR_Tbl_Idx &&
          (IR_OPR(IL_IDX(list_idx)) == Array_Construct_Opr ||
           IR_OPR(IL_IDX(list_idx)) == Constant_Array_Construct_Opr ||
           IR_OPR(IL_IDX(list_idx)) == Implied_Do_Opr)) {

         COPY_OPND(opnd, IL_OPND(list_idx));
         analyse_loops(&opnd, &mopnd, constructor_size_level);

         if (*constructor_size_level == Guess_Size) {
            *size_opnd = null_opnd;
            goto EXIT;
         }

         if (OPND_FLD(popnd) == NO_Tbl_Idx) {
            COPY_OPND(popnd, mopnd);
         }
         else {
            NTR_IR_TBL(plus_idx);
            IR_OPR(plus_idx) = Plus_Opr;
            IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(plus_idx) = stmt_start_line;
            IR_COL_NUM(plus_idx)  = stmt_start_col;
            COPY_OPND(IR_OPND_L(plus_idx), popnd);
            COPY_OPND(IR_OPND_R(plus_idx), mopnd);

            OPND_FLD(popnd) = IR_Tbl_Idx;
            OPND_IDX(popnd) = plus_idx;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         continue;
      }

      exp_desc = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed;

      if (exp_desc.has_constructor) {

         COPY_OPND(opnd, IL_OPND(list_idx));
         analyse_loops(&opnd, &mopnd, constructor_size_level);

         if (*constructor_size_level == Guess_Size) {
            *size_opnd = null_opnd;
            goto EXIT;
         }

         if (OPND_FLD(popnd) == NO_Tbl_Idx) {
            COPY_OPND(popnd, mopnd);
         }
         else {
            NTR_IR_TBL(plus_idx);
            IR_OPR(plus_idx) = Plus_Opr;
            IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(plus_idx) = stmt_start_line;
            IR_COL_NUM(plus_idx)  = stmt_start_col;
            COPY_OPND(IR_OPND_L(plus_idx), popnd);
            COPY_OPND(IR_OPND_R(plus_idx), mopnd);
            OPND_FLD(popnd) = IR_Tbl_Idx;
            OPND_IDX(popnd) = plus_idx;
         }
      }
      else if (exp_desc.rank) {
         for (i = 0; i < exp_desc.rank; i++) {

            if (exp_desc.shape[i].fld == NO_Tbl_Idx) {
               *constructor_size_level = Guess_Size;
               *size_opnd = null_opnd;
               goto EXIT;
            }

            if (i == 0) {
               COPY_OPND(mopnd, exp_desc.shape[i]);
               OPND_LINE_NUM(mopnd) = stmt_start_line;
               OPND_COL_NUM(mopnd)  = stmt_start_col;
            }
            else {
               NTR_IR_TBL(mult_idx);
               IR_OPR(mult_idx) = Mult_Opr;
               IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(mult_idx) = stmt_start_line;
               IR_COL_NUM(mult_idx)  = stmt_start_col;
               COPY_OPND(IR_OPND_L(mult_idx), mopnd);
               COPY_OPND(IR_OPND_R(mult_idx), exp_desc.shape[i]);
               IR_LINE_NUM_R(mult_idx) = stmt_start_line;
               IR_COL_NUM_R(mult_idx)  = stmt_start_col;
               OPND_FLD(mopnd) = IR_Tbl_Idx;
               OPND_IDX(mopnd) = mult_idx;
            }
         }

         check_for_dependencies(&mopnd, constructor_size_level);

         if (*constructor_size_level == Guess_Size) {
            *size_opnd = null_opnd;
            goto EXIT;
         }

         if (OPND_FLD(popnd) == NO_Tbl_Idx) {
            COPY_OPND(popnd, mopnd);
         }
         else {
            NTR_IR_TBL(plus_idx);
            IR_OPR(plus_idx) = Plus_Opr;
            IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(plus_idx) = stmt_start_line;
            IR_COL_NUM(plus_idx)  = stmt_start_col;
            COPY_OPND(IR_OPND_L(plus_idx), popnd);
            COPY_OPND(IR_OPND_R(plus_idx), mopnd);
            OPND_FLD(popnd) = IR_Tbl_Idx;
            OPND_IDX(popnd) = plus_idx;
         }
      }
      else {
         scalar_cnt++;
      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (scalar_cnt > 0) {
      OPND_FLD(mopnd) = CN_Tbl_Idx;
      OPND_IDX(mopnd) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, scalar_cnt);
      OPND_LINE_NUM(mopnd) = stmt_start_line;
      OPND_COL_NUM(mopnd)  = stmt_start_col;

      if (OPND_FLD(popnd) == NO_Tbl_Idx) {
            COPY_OPND(popnd, mopnd);
      }
      else {
         NTR_IR_TBL(plus_idx);
         IR_OPR(plus_idx) = Plus_Opr;
         IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(plus_idx) = stmt_start_line;
         IR_COL_NUM(plus_idx)  = stmt_start_col;
         COPY_OPND(IR_OPND_L(plus_idx), popnd);
         COPY_OPND(IR_OPND_R(plus_idx), mopnd);
         OPND_FLD(popnd) = IR_Tbl_Idx;
         OPND_IDX(popnd) = plus_idx;
      }
   }

   COPY_OPND((*size_opnd), popnd);

EXIT:

   TRACE (Func_Exit, "determine_slice_size", NULL);

   return;

}  /* determine_slice_size */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine drives the generation of the loop interpretation stmst.  *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx - top operator of loop                                         *|
|*      size_tmp_idx - attr idx that holds the cumulative size.               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void create_interp_stmts(int	   ir_idx,
				int	   size_tmp_idx)

{
   int			asg_idx;
   int			col;
   opnd_type            end_opnd;
   opnd_type            inc_opnd;
   int			lcv_attr;
   int			line;
   int			list_idx;
   int			plus_idx;
   int			save_curr_stmt_sh_idx;
   int			sh_idx;
   opnd_type		start_opnd;


   TRACE (Func_Entry, "create_interp_stmts", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (IR_OPR(ir_idx) == Implied_Do_Opr) {
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Null_Opr;

      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;

      list_idx = IR_IDX_R(ir_idx);

      lcv_attr = IL_IDX(list_idx);

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(start_opnd, IL_OPND(list_idx));
      
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(end_opnd, IL_OPND(list_idx));
      
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(inc_opnd, IL_OPND(list_idx));

      /* there better not be any functions in these expressions */

      create_loop_stmts(lcv_attr,
                       &start_opnd,
                       &end_opnd,
                       &inc_opnd,
                        curr_stmt_sh_idx,	/* body start sh idx */
                        curr_stmt_sh_idx);      /* body end sh idx */

      if (IR_FLD_L(ir_idx) == IR_Tbl_Idx && 
          (IR_OPR(IR_IDX_L(ir_idx)) == Implied_Do_Opr ||
           IR_OPR(IR_IDX_L(ir_idx)) == Plus_Opr)) {


         create_interp_stmts(IR_IDX_L(ir_idx), size_tmp_idx);
      }
      else {
         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(size_tmp_idx);
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;
         IR_FLD_L(asg_idx) = AT_Tbl_Idx;
         IR_IDX_L(asg_idx) = size_tmp_idx;
         IR_LINE_NUM_L(asg_idx) = line;
         IR_COL_NUM_L(asg_idx)  = col;

         NTR_IR_TBL(plus_idx);
         IR_OPR(plus_idx) = Plus_Opr;
         IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(plus_idx) = line;
         IR_COL_NUM(plus_idx)  = col;
         IR_FLD_R(asg_idx) = IR_Tbl_Idx;
         IR_IDX_R(asg_idx) = plus_idx;
         IR_FLD_L(plus_idx) = AT_Tbl_Idx;
         IR_IDX_L(plus_idx) = size_tmp_idx;
         IR_LINE_NUM_L(plus_idx) = line;
         IR_COL_NUM_L(plus_idx)  = col;
         COPY_OPND(IR_OPND_R(plus_idx), IR_OPND_L(ir_idx));

         gen_sh(Before, Assignment_Stmt, line, col,
                FALSE, FALSE, TRUE);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }

      /* remove null place holder stmt */
      sh_idx = curr_stmt_sh_idx;
      remove_sh(sh_idx);
      FREE_IR_NODE(SH_IR_IDX(sh_idx));
      FREE_SH_NODE(sh_idx);

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
   else if (IR_OPR(ir_idx) == Plus_Opr) {

      if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
          (IR_OPR(IR_IDX_L(ir_idx)) == Implied_Do_Opr ||
           IR_OPR(IR_IDX_L(ir_idx)) == Plus_Opr)) {

         create_interp_stmts(IR_IDX_L(ir_idx), size_tmp_idx);
      }
      else {
         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;
         IR_FLD_L(asg_idx) = AT_Tbl_Idx;
         IR_IDX_L(asg_idx) = size_tmp_idx;
         IR_LINE_NUM_L(asg_idx) = line;
         IR_COL_NUM_L(asg_idx)  = col;

         NTR_IR_TBL(plus_idx);
         IR_OPR(plus_idx) = Plus_Opr;
         IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(plus_idx) = line;
         IR_COL_NUM(plus_idx)  = col;
         IR_FLD_R(asg_idx) = IR_Tbl_Idx;
         IR_IDX_R(asg_idx) = plus_idx;
         IR_FLD_L(plus_idx) = AT_Tbl_Idx;
         IR_IDX_L(plus_idx) = size_tmp_idx;
         IR_LINE_NUM_L(plus_idx) = line;
         IR_COL_NUM_L(plus_idx)  = col;
         COPY_OPND(IR_OPND_R(plus_idx), IR_OPND_L(ir_idx));

         gen_sh(Before, Assignment_Stmt, line, col,
                FALSE, FALSE, TRUE);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }

      if (IR_FLD_R(ir_idx) == IR_Tbl_Idx &&
          (IR_OPR(IR_IDX_R(ir_idx)) == Implied_Do_Opr ||
           IR_OPR(IR_IDX_R(ir_idx)) == Plus_Opr)) {

         create_interp_stmts(IR_IDX_R(ir_idx), size_tmp_idx);
      }
      else {
         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;
         IR_FLD_L(asg_idx) = AT_Tbl_Idx;
         IR_IDX_L(asg_idx) = size_tmp_idx;
         IR_LINE_NUM_L(asg_idx) = line;
         IR_COL_NUM_L(asg_idx)  = col;

         NTR_IR_TBL(plus_idx);
         IR_OPR(plus_idx) = Plus_Opr;
         IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(plus_idx) = line;
         IR_COL_NUM(plus_idx)  = col;
         IR_FLD_R(asg_idx) = IR_Tbl_Idx;
         IR_IDX_R(asg_idx) = plus_idx;
         IR_FLD_L(plus_idx) = AT_Tbl_Idx;
         IR_IDX_L(plus_idx) = size_tmp_idx;
         IR_LINE_NUM_L(plus_idx) = line;
         IR_COL_NUM_L(plus_idx)  = col;
         COPY_OPND(IR_OPND_R(plus_idx), IR_OPND_R(ir_idx));

         gen_sh(Before, Assignment_Stmt, line, col,
                FALSE, FALSE, TRUE);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }
   }

   TRACE (Func_Exit, "create_interp_stmts", NULL);

   return;

}  /* create_interp_stmts */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create single assignment for run time constructors                    *|
|*									      *|
|* Input parameters:							      *|
|*	r_opnd - right hand side expression.                                  *|
|*	exp_desc - right hand side expression descriptor.                     *|
|*      target_base_opnd - base of array tmp.                                 *|
|*	target_sub_idx - subscript attr idx.                                  *|
|*	size_limit_attr - attr for size test (guess)                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void do_single_asg(opnd_type		*r_opnd,
			  expr_arg_type		*exp_desc,
			  opnd_type		*target_base_opnd,
			  int			 target_sub_idx,
                          int			 size_limit_attr)

{
   int                  asg_idx;
   int                  asg_idx2;
   int			attr_idx;
   int                  col;
   int			i;
   int                  line;
   int                  list2_idx;
   int                  minus_idx;
   int			mult_idx;
   opnd_type		opnd;
   int                  plus_idx;
   int                  sub_idx;
   int			tmp_idx;
   int                  trip_idx;


   TRACE (Func_Entry, "do_single_asg", NULL);

   find_opnd_line_and_column(r_opnd, &line, &col);

   if (exp_desc->rank == 0) {

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = exp_desc->type_idx;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;
      COPY_OPND(IR_OPND_L(sub_idx), (*target_base_opnd));

      NTR_IR_LIST_TBL(list2_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list2_idx;
      IR_LIST_CNT_R(sub_idx) = 1;

      IL_FLD(list2_idx) = AT_Tbl_Idx;
      IL_IDX(list2_idx) = target_sub_idx;
      IL_LINE_NUM(list2_idx) = line;
      IL_COL_NUM(list2_idx)  = col;

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = sub_idx;

      if (exp_desc->type == Character) {
         gen_whole_substring(&opnd, 0);
      }

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = exp_desc->type_idx;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      COPY_OPND(IR_OPND_L(asg_idx), opnd);
      COPY_OPND(IR_OPND_R(asg_idx), (*r_opnd));

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      increment_subscript(target_sub_idx);

      if (size_limit_attr) {
         attr_idx = find_left_attr(target_base_opnd);
         test_size_stmts(attr_idx, target_sub_idx, size_limit_attr);
      }
   }
   else if (exp_desc->rank == 1) {

      /* increment target_sub_idx first so we can check it */

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      IR_FLD_L(asg_idx)    = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)    = target_sub_idx;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      NTR_IR_TBL(plus_idx);
      IR_OPR(plus_idx) = Plus_Opr;
      IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(plus_idx) = line;
      IR_COL_NUM(plus_idx)  = col;
      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = plus_idx;

      IR_FLD_L(plus_idx) = AT_Tbl_Idx;
      IR_IDX_L(plus_idx) = target_sub_idx;
      IR_LINE_NUM_L(plus_idx) = line;
      IR_COL_NUM_L(plus_idx)  = col;
      IR_FLD_R(plus_idx) = exp_desc->shape[0].fld;
      IR_IDX_R(plus_idx) = exp_desc->shape[0].idx;
      IR_LINE_NUM_R(plus_idx) = line;
      IR_COL_NUM_R(plus_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      if (size_limit_attr) {
         attr_idx = find_left_attr(target_base_opnd);
         test_size_stmts(attr_idx, target_sub_idx, size_limit_attr);
      }

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Section_Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = exp_desc->type_idx;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;
      COPY_OPND(IR_OPND_L(sub_idx), (*target_base_opnd));

      NTR_IR_LIST_TBL(list2_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list2_idx;
      IR_LIST_CNT_R(sub_idx) = 1;

      NTR_IR_TBL(trip_idx);
      IR_OPR(trip_idx) = Triplet_Opr;
      IR_TYPE_IDX(trip_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(trip_idx) = line;
      IR_COL_NUM(trip_idx)  = col;

      IL_FLD(list2_idx) = IR_Tbl_Idx;
      IL_IDX(list2_idx) = trip_idx;

      NTR_IR_LIST_TBL(list2_idx);
      IR_FLD_L(trip_idx) = IL_Tbl_Idx;
      IR_IDX_L(trip_idx) = list2_idx;
      IR_LIST_CNT_L(trip_idx) = 3;

      NTR_IR_TBL(minus_idx);
      IR_OPR(minus_idx) = Minus_Opr;
      IR_TYPE_IDX(minus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(minus_idx) = line;
      IR_COL_NUM(minus_idx)  = col;
      IR_FLD_L(minus_idx) = AT_Tbl_Idx;
      IR_IDX_L(minus_idx) = target_sub_idx;
      IR_LINE_NUM_L(minus_idx) = line;
      IR_COL_NUM_L(minus_idx)  = col;
      IR_FLD_R(minus_idx) = exp_desc->shape[0].fld;
      IR_IDX_R(minus_idx) = exp_desc->shape[0].idx;
      IR_LINE_NUM_R(minus_idx) = line;
      IR_COL_NUM_R(minus_idx)  = col;

      IL_FLD(list2_idx) = IR_Tbl_Idx;
      IL_IDX(list2_idx) = minus_idx;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
      list2_idx = IL_NEXT_LIST_IDX(list2_idx);

      NTR_IR_TBL(minus_idx);
      IR_OPR(minus_idx) = Minus_Opr;
      IR_TYPE_IDX(minus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(minus_idx) = line;
      IR_COL_NUM(minus_idx)  = col;
      IR_FLD_L(minus_idx) = AT_Tbl_Idx;
      IR_IDX_L(minus_idx) = target_sub_idx;
      IR_LINE_NUM_L(minus_idx) = line;
      IR_COL_NUM_L(minus_idx)  = col;
      IR_FLD_R(minus_idx) = CN_Tbl_Idx;
      IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
      IR_LINE_NUM_R(minus_idx) = line;
      IR_COL_NUM_R(minus_idx)  = col;

      IL_FLD(list2_idx) = IR_Tbl_Idx;
      IL_IDX(list2_idx) = minus_idx;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
      list2_idx = IL_NEXT_LIST_IDX(list2_idx);
      IL_FLD(list2_idx) = CN_Tbl_Idx;
      IL_IDX(list2_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list2_idx) = line;
      IL_COL_NUM(list2_idx)  = col;

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = sub_idx;

      if (exp_desc->type == Character) {
         gen_whole_substring(&opnd, 1);
      }

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = exp_desc->type_idx;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      COPY_OPND(IR_OPND_L(asg_idx), opnd);

      COPY_OPND(IR_OPND_R(asg_idx), (*r_opnd));

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   }
   else {

      /* increment target_sub_idx first so we can check it */

      /* tmp_idx holds the product of the shapes. It is used twice. */

      tmp_idx			= gen_compiler_tmp(line,col, Priv, TRUE);
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;
      ATD_TYPE_IDX(tmp_idx)	= SA_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      NTR_IR_TBL(asg_idx2);
      IR_OPR(asg_idx2)        = Asg_Opr;
      IR_TYPE_IDX(asg_idx2) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx2)   = line;
      IR_COL_NUM(asg_idx2)    = col;
      IR_FLD_L(asg_idx2)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx2)      = tmp_idx;
      IR_LINE_NUM_L(asg_idx2) = line;
      IR_COL_NUM_L(asg_idx2)  = col;

      COPY_OPND(opnd, exp_desc->shape[0]);
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      for (i = 1; i < exp_desc->rank; i++) {                
         NTR_IR_TBL(mult_idx);
         IR_OPR(mult_idx) = Mult_Opr;
         IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(mult_idx) = line;
         IR_COL_NUM(mult_idx)  = col;

         COPY_OPND(IR_OPND_L(mult_idx), opnd);
 
         COPY_OPND(IR_OPND_R(mult_idx), exp_desc->shape[i]);
         IR_LINE_NUM_R(mult_idx) = line;
         IR_COL_NUM_R(mult_idx)  = col;

         OPND_FLD(opnd) = IR_Tbl_Idx;
         OPND_IDX(opnd) = mult_idx;
      }

      COPY_OPND(IR_OPND_R(asg_idx2), opnd);

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx2;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      IR_FLD_L(asg_idx)    = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)    = target_sub_idx;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      NTR_IR_TBL(plus_idx);
      IR_OPR(plus_idx) = Plus_Opr;
      IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(plus_idx) = line;
      IR_COL_NUM(plus_idx)  = col;
      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = plus_idx;

      IR_FLD_L(plus_idx) = AT_Tbl_Idx;
      IR_IDX_L(plus_idx) = target_sub_idx;
      IR_LINE_NUM_L(plus_idx) = line;
      IR_COL_NUM_L(plus_idx)  = col;
      IR_FLD_R(plus_idx) = AT_Tbl_Idx;
      IR_IDX_R(plus_idx) = tmp_idx;
      IR_LINE_NUM_R(plus_idx) = line;
      IR_COL_NUM_R(plus_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      if (size_limit_attr) {
         attr_idx = find_left_attr(target_base_opnd);
         test_size_stmts(attr_idx, target_sub_idx, size_limit_attr);
      }

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Section_Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = exp_desc->type_idx;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;
      COPY_OPND(IR_OPND_L(sub_idx), (*target_base_opnd));

      NTR_IR_LIST_TBL(list2_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list2_idx;
      IR_LIST_CNT_R(sub_idx) = 1;

      NTR_IR_TBL(trip_idx);
      IR_OPR(trip_idx) = Triplet_Opr;
      IR_TYPE_IDX(trip_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(trip_idx) = line;
      IR_COL_NUM(trip_idx)  = col;

      IL_FLD(list2_idx) = IR_Tbl_Idx;
      IL_IDX(list2_idx) = trip_idx;

      NTR_IR_LIST_TBL(list2_idx);
      IR_FLD_L(trip_idx) = IL_Tbl_Idx;
      IR_IDX_L(trip_idx) = list2_idx;
      IR_LIST_CNT_L(trip_idx) = 3;

      NTR_IR_TBL(minus_idx);
      IR_OPR(minus_idx) = Minus_Opr;
      IR_TYPE_IDX(minus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(minus_idx) = line;
      IR_COL_NUM(minus_idx)  = col;
      IR_FLD_L(minus_idx) = AT_Tbl_Idx;
      IR_IDX_L(minus_idx) = target_sub_idx;
      IR_LINE_NUM_L(minus_idx) = line;
      IR_COL_NUM_L(minus_idx)  = col;
      IR_FLD_R(minus_idx) = AT_Tbl_Idx;
      IR_IDX_R(minus_idx) = tmp_idx;
      IR_LINE_NUM_R(minus_idx) = line;
      IR_COL_NUM_R(minus_idx)  = col;

      IL_FLD(list2_idx) = IR_Tbl_Idx;
      IL_IDX(list2_idx) = minus_idx;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
      list2_idx = IL_NEXT_LIST_IDX(list2_idx);

      NTR_IR_TBL(minus_idx);
      IR_OPR(minus_idx) = Minus_Opr;
      IR_TYPE_IDX(minus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(minus_idx) = line;
      IR_COL_NUM(minus_idx)  = col;
      IR_FLD_L(minus_idx) = AT_Tbl_Idx;
      IR_IDX_L(minus_idx) = target_sub_idx;
      IR_LINE_NUM_L(minus_idx) = line;
      IR_COL_NUM_L(minus_idx)  = col;
      IR_FLD_R(minus_idx) = CN_Tbl_Idx;
      IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
      IR_LINE_NUM_R(minus_idx) = line;
      IR_COL_NUM_R(minus_idx)  = col;

      IL_FLD(list2_idx) = IR_Tbl_Idx;
      IL_IDX(list2_idx) = minus_idx;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
      list2_idx = IL_NEXT_LIST_IDX(list2_idx);
      IL_FLD(list2_idx) = CN_Tbl_Idx;
      IL_IDX(list2_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list2_idx) = line;
      IL_COL_NUM(list2_idx)  = col;

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = sub_idx;

      if (exp_desc->type == Character) {
         gen_whole_substring(&opnd, exp_desc->rank);
      }

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Flat_Array_Asg_Opr;
      IR_TYPE_IDX(asg_idx) = exp_desc->type_idx;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      COPY_OPND(IR_OPND_L(asg_idx), opnd);

      COPY_OPND(IR_OPND_R(asg_idx), (*r_opnd));

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      
   }


   TRACE (Func_Exit, "do_single_asg", NULL);

   return;

}  /* do_single_asg */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine drives the generation of assignment stmts for structure  *|
|*	constructors.                                                         *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - top of constructor tree.                                   *|
|*	target_base_opnd - left hand side base (of derived type).             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void create_struct_constructor_asg(opnd_type	*top_opnd,
					  opnd_type	*target_base_opnd)

{
   int			asg_idx;
   int			attr_idx;
   int			col;
   int			ir_idx;
   expr_arg_type	l_exp_desc;
   opnd_type		l_opnd;
   int			line;
   int			list_idx;
   opnd_type		opnd;
   int			sn_idx;
   int			struct_idx;


   TRACE (Func_Entry, "create_struct_constructor_asg", NULL);

# ifdef _DEBUG
   if (OPND_FLD((*top_opnd)) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX((*top_opnd))) != Struct_Construct_Opr) {
      find_opnd_line_and_column(top_opnd, &line, &col);
      PRINTMSG(line, 987, Internal, col);
   }
# endif

   ir_idx = OPND_IDX((*top_opnd));

   list_idx = IR_IDX_R(ir_idx);
   sn_idx   = ATT_FIRST_CPNT_IDX(IR_IDX_L(ir_idx));

   while (list_idx) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx), &line, &col);

      attr_idx = SN_ATTR_IDX(sn_idx);

      NTR_IR_TBL(struct_idx);
      IR_OPR(struct_idx) = Struct_Opr;
      IR_TYPE_IDX(struct_idx) = ATD_TYPE_IDX(attr_idx);
      IR_LINE_NUM(struct_idx) = line;
      IR_COL_NUM(struct_idx)  = col;
      COPY_OPND(IR_OPND_L(struct_idx), (*target_base_opnd));
      IR_FLD_R(struct_idx) = AT_Tbl_Idx;
      IR_IDX_R(struct_idx) = attr_idx;
      IR_LINE_NUM_R(struct_idx) = line;
      IR_COL_NUM_R(struct_idx) = col;
      
      
      OPND_FLD(l_opnd) = IR_Tbl_Idx;
      OPND_IDX(l_opnd) = struct_idx;

#ifdef KEY /* Bug 6845 */
      if (! (ATD_POINTER(attr_idx) || ATD_ALLOCATABLE(attr_idx)))
#else /* KEY Bug 6845 */
      if (! ATD_POINTER(attr_idx))
#endif /* KEY Bug 6845 */
      {

         if (ATD_ARRAY_IDX(attr_idx)) {
            gen_whole_subscript(&l_opnd, &l_exp_desc);
         }
         else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
            gen_whole_substring(&l_opnd, 0);
         }
      }

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(attr_idx);
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx) = col;
      COPY_OPND(IR_OPND_L(asg_idx), l_opnd);

/* BHJ may want to analyse each item and prevent the sub constructors */

      l_exp_desc = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed;
      COPY_OPND(opnd, IL_OPND(list_idx));
      expand_stmts(&opnd, &l_exp_desc);


      COPY_OPND(IR_OPND_R(asg_idx), opnd);

      if (ATD_POINTER(attr_idx)) {

         /* first initialize the dv */

         gen_dv_whole_def_init(&l_opnd, attr_idx, Before);

         IR_OPR(asg_idx) = Ptr_Asg_Opr;

         /* do the stmt thing here */

         COPY_OPND(l_opnd, IR_OPND_L(asg_idx));

         if (l_exp_desc.pointer || l_exp_desc.allocatable) {
            ptr_assign_from_ptr(&l_opnd, &opnd);
            list_idx  = IL_NEXT_LIST_IDX(list_idx);
            sn_idx    = SN_SIBLING_LINK(sn_idx);
            continue;
         }
         else if (l_exp_desc.target) {
            dope_vector_setup(&opnd, &l_exp_desc, &l_opnd,
                                   TRUE);
         }
      }
#ifdef KEY /* Bug 6845 */
      else if (ATD_ALLOCATABLE(attr_idx)) {
	/* LHS and RHS both are dope vectors: just copy dope, since temp on
	 * LHS will never be deallocated */
	asg_idx = gen_ir(OPND_FLD(l_opnd), OPND_IDX(l_opnd),
	  Dv_Whole_Copy_Opr, TYPELESS_DEFAULT_TYPE, line, col,
	  OPND_FLD(opnd), OPND_IDX(opnd));
      }

      gen_sh(Before,
        (ATD_ALLOCATABLE(attr_idx) && ! l_exp_desc.allocatable) ?
	  Call_Stmt :
	  Assignment_Stmt,
	line, col, FALSE, FALSE, TRUE);
#else /* KEY Bug 6845 */

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
#endif /* KEY Bug 6845 */

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


      list_idx	= IL_NEXT_LIST_IDX(list_idx);
      sn_idx	= SN_SIBLING_LINK(sn_idx);
   }

   TRACE (Func_Exit, "create_struct_constructor_asg", NULL);

   return;

}  /* create_struct_constructor_asg */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Generate the stmts to increment the subscript tmp for run time array  *|
|*	constructors.                                                         *|
|*									      *|
|* Input parameters:							      *|
|*	target_sub_idx - attr idx for subscript attr.                         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void increment_subscript(int	target_sub_idx)

{
   int		asg_idx;
   int		plus_idx;

   TRACE (Func_Entry, "increment_subscript", NULL);

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Asg_Opr;
   IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = stmt_start_line;
   IR_COL_NUM(asg_idx)  = stmt_start_col;
   IR_FLD_L(asg_idx)    = AT_Tbl_Idx;
   IR_IDX_L(asg_idx)    = target_sub_idx;
   IR_LINE_NUM_L(asg_idx) = stmt_start_line;
   IR_COL_NUM_L(asg_idx)  = stmt_start_col;
   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = stmt_start_line;
   IR_COL_NUM(plus_idx)  = stmt_start_col;
   IR_FLD_R(asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(asg_idx) = plus_idx;

   IR_FLD_L(plus_idx) = AT_Tbl_Idx;
   IR_IDX_L(plus_idx) = target_sub_idx;
   IR_LINE_NUM_L(plus_idx) = stmt_start_line;
   IR_COL_NUM_L(plus_idx)  = stmt_start_col;
   IR_FLD_R(plus_idx) = CN_Tbl_Idx;
   IR_IDX_R(plus_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(plus_idx) = stmt_start_line;
   IR_COL_NUM_R(plus_idx)  = stmt_start_col;

   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
          FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


   TRACE (Func_Exit, "increment_subscript", NULL);

   return;

}  /* increment_subscript */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Insert the size test for the realloc call.                            *|
|*									      *|
|* Input parameters:							      *|
|*	target_dope_idx - dope vector attr idx                                *|
|*	target_sub_idx  - subscript attr idx.                                 *|
|*	size_idx        - attr idx for attr size.                             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void test_size_stmts(int		target_dope_idx,
                            int		target_sub_idx,
			    int		size_idx)

{
   int		asg_idx;
   int		br_idx;
   int		call_idx;
   int		dv_idx;
   int		ir_idx;
   int		lt_idx;
   int		label_idx;
   int		list_idx;
   int		minus_idx;
   int		plus_idx;
   int		realloc_size_attr;
   int		save_curr_stmt_sh_idx;


   TRACE (Func_Entry, "test_size_stmts", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   /* gen branch around label */

   label_idx = gen_internal_lbl(stmt_start_line);

   gen_sh(Before, Continue_Stmt, stmt_start_line, stmt_start_col, 
          FALSE, TRUE, TRUE);

   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   IR_OPR(ir_idx)              = Label_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = stmt_start_line;
   IR_COL_NUM(ir_idx)          = stmt_start_col;
   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = label_idx;
   IR_COL_NUM_L(ir_idx)        = stmt_start_col;
   IR_LINE_NUM_L(ir_idx)       = stmt_start_line;

   AT_DEFINED(label_idx)       = TRUE;
   ATL_DEF_STMT_IDX(label_idx) = curr_stmt_sh_idx;

   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;


   /* now push back test */

   NTR_IR_TBL(br_idx);
   IR_OPR(br_idx) = Br_True_Opr;
   IR_TYPE_IDX(br_idx) = LOGICAL_DEFAULT_TYPE;
   IR_LINE_NUM(br_idx) = stmt_start_line;
   IR_COL_NUM(br_idx)  = stmt_start_col;

   NTR_IR_TBL(lt_idx);
   IR_OPR(lt_idx) = Lt_Opr;
   IR_TYPE_IDX(lt_idx) = LOGICAL_DEFAULT_TYPE;
   IR_LINE_NUM(lt_idx) = stmt_start_line;
   IR_COL_NUM(lt_idx)  = stmt_start_col;

   IR_FLD_L(lt_idx) = AT_Tbl_Idx;
   IR_IDX_L(lt_idx) = target_sub_idx;
   IR_LINE_NUM_L(lt_idx) = stmt_start_line;
   IR_COL_NUM_L(lt_idx)  = stmt_start_col;
   
   IR_FLD_R(lt_idx) = AT_Tbl_Idx;
   IR_IDX_R(lt_idx) = size_idx;
   IR_LINE_NUM_R(lt_idx) = stmt_start_line;
   IR_COL_NUM_R(lt_idx)  = stmt_start_col;

   IR_FLD_L(br_idx) = IR_Tbl_Idx;
   IR_IDX_L(br_idx) = lt_idx;

   IR_FLD_R(br_idx) = AT_Tbl_Idx;
   IR_IDX_R(br_idx) = label_idx;
   IR_LINE_NUM_R(br_idx) = stmt_start_line;
   IR_COL_NUM_R(br_idx)  = stmt_start_col;

   gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
          FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = br_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /* now for the realloc call */
   /* new size is in bits. (elements * element bit size) */

   
   if (glb_tbl_idx[Realloc_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Realloc_Attr_Idx] = create_lib_entry_attr(REALLOC_LIB_ENTRY,
                                                            REALLOC_NAME_LEN,
                                                            stmt_start_line,
                                                            stmt_start_col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Realloc_Attr_Idx]);

   /* increment size tmp */

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Asg_Opr;
   IR_TYPE_IDX(asg_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = stmt_start_line;
   IR_COL_NUM(asg_idx)  = stmt_start_col;
   IR_FLD_L(asg_idx) = AT_Tbl_Idx;
   IR_IDX_L(asg_idx) = size_idx;
   IR_LINE_NUM_L(asg_idx) = stmt_start_line;
   IR_COL_NUM_L(asg_idx)  = stmt_start_col;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Plus_Opr;
   IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = stmt_start_line;
   IR_COL_NUM(ir_idx)  = stmt_start_col;
   IR_FLD_R(ir_idx) = AT_Tbl_Idx;
   IR_IDX_R(ir_idx) = size_idx;
   IR_LINE_NUM_R(ir_idx) = stmt_start_line;
   IR_COL_NUM_R(ir_idx)  = stmt_start_col;

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = stmt_start_line;
   IR_COL_NUM(plus_idx)  = stmt_start_col;
   IR_FLD_L(ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(ir_idx) = plus_idx;

   IR_FLD_R(plus_idx) = CN_Tbl_Idx;
   IR_IDX_R(plus_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                    CONSTRUCTOR_GUESS_SIZE);
   IR_LINE_NUM_R(plus_idx) = stmt_start_line;
   IR_COL_NUM_R(plus_idx)  = stmt_start_col;

   NTR_IR_TBL(minus_idx);
   IR_OPR(minus_idx) = Minus_Opr;
   IR_TYPE_IDX(minus_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(minus_idx) = stmt_start_line;
   IR_COL_NUM(minus_idx)  = stmt_start_col;
   IR_FLD_L(minus_idx) = AT_Tbl_Idx;
   IR_IDX_L(minus_idx) = target_sub_idx;
   IR_LINE_NUM_L(minus_idx) = stmt_start_line;
   IR_COL_NUM_L(minus_idx)  = stmt_start_col;
   IR_FLD_R(minus_idx) = AT_Tbl_Idx;
   IR_IDX_R(minus_idx) = size_idx;
   IR_LINE_NUM_R(minus_idx) = stmt_start_line;
   IR_COL_NUM_R(minus_idx)  = stmt_start_col;

   IR_FLD_L(plus_idx) = IR_Tbl_Idx;
   IR_IDX_L(plus_idx) = minus_idx;

   IR_FLD_R(asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(asg_idx) = ir_idx;

   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col, 
          FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /* compute new bit size */

   GEN_COMPILER_TMP_ASG(asg_idx,
                        realloc_size_attr,
                        TRUE,	/* Semantics is done */
                        stmt_start_line,
                        stmt_start_col,
                        SA_INTEGER_DEFAULT_TYPE,
                        Priv);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Mult_Opr;
   IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = stmt_start_line;
   IR_COL_NUM(ir_idx)  = stmt_start_col;
   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = size_idx;
   IR_LINE_NUM_L(ir_idx) = stmt_start_line;
   IR_COL_NUM_L(ir_idx)  = stmt_start_col;
   
   NTR_IR_TBL(dv_idx);
   IR_OPR(dv_idx) = Dv_Access_El_Len;
   IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = stmt_start_line;
   IR_COL_NUM(dv_idx)  = stmt_start_col;
   IR_FLD_L(dv_idx) = AT_Tbl_Idx;
   IR_IDX_L(dv_idx) = target_dope_idx;
   IR_LINE_NUM_L(dv_idx) = stmt_start_line;
   IR_COL_NUM_L(dv_idx)  = stmt_start_col;

   IR_FLD_R(ir_idx) = IR_Tbl_Idx;
   IR_IDX_R(ir_idx) = dv_idx;

   IR_FLD_R(asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(asg_idx) = ir_idx;

   gen_sh(Before, Assignment_Stmt, stmt_start_line,
          stmt_start_col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /* now for the call */

   NTR_IR_TBL(call_idx);
   IR_OPR(call_idx) = Call_Opr;
   IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(call_idx) = stmt_start_line;
   IR_COL_NUM(call_idx)  = stmt_start_col;
   IR_FLD_L(call_idx) = AT_Tbl_Idx;
   IR_IDX_L(call_idx) = glb_tbl_idx[Realloc_Attr_Idx];
   IR_LINE_NUM_L(call_idx) = stmt_start_line;
   IR_COL_NUM_L(call_idx)  = stmt_start_col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(call_idx) = IL_Tbl_Idx;
   IR_IDX_R(call_idx) = list_idx;
   IR_LIST_CNT_R(call_idx) = 2;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Aloc_Opr;
   IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
   IR_LINE_NUM(ir_idx) = stmt_start_line;
   IR_COL_NUM(ir_idx)  = stmt_start_col;

   IL_FLD(list_idx) = IR_Tbl_Idx;
   IL_IDX(list_idx) = ir_idx;

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = target_dope_idx;
   IR_LINE_NUM_L(ir_idx) = stmt_start_line;
   IR_COL_NUM_L(ir_idx)  = stmt_start_col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)	= Aloc_Opr;
   IR_TYPE_IDX(ir_idx)	= CRI_Ptr_8;
   IR_LINE_NUM(ir_idx)	= stmt_start_line;
   IR_COL_NUM(ir_idx)	= stmt_start_col;

   IL_FLD(list_idx)	= IR_Tbl_Idx;
   IL_IDX(list_idx)	= ir_idx;

   IR_FLD_L(ir_idx)	= AT_Tbl_Idx;
   IR_IDX_L(ir_idx)	= realloc_size_attr;
   IR_LINE_NUM_L(ir_idx)	= stmt_start_line;
   IR_COL_NUM_L(ir_idx)	= stmt_start_col;

   gen_sh(Before, Call_Stmt, stmt_start_line,
          stmt_start_col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = call_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "test_size_stmts", NULL);

   return;

}  /* test_size_stmts */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process the character length for run time constructors replacing lcv's*|
|*	with their start values.                                              *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - top of char_len expression.                                *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void process_char_len(opnd_type	*top_opnd)

{
   expr_arg_type	exp_desc;
   int			list_idx;
   opnd_type		opnd;
   cif_usage_code_type  save_xref_state;


   TRACE (Func_Entry, "process_char_len", NULL);

   switch (OPND_FLD((*top_opnd))) {
      case IR_Tbl_Idx:
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX((*top_opnd))));
         process_char_len(&opnd);
         COPY_OPND(IR_OPND_L(OPND_IDX((*top_opnd))), opnd);

         COPY_OPND(opnd, IR_OPND_R(OPND_IDX((*top_opnd))));
         process_char_len(&opnd);
         COPY_OPND(IR_OPND_R(OPND_IDX((*top_opnd))), opnd);

         if (IR_OPR(OPND_IDX((*top_opnd))) == Call_Opr) {
            /* get new arg_descriptors for the arguments */
            list_idx = IR_IDX_R(OPND_IDX((*top_opnd)));

            while (list_idx) {

               if (IL_FLD(list_idx) != NO_Tbl_Idx) {

                  COPY_OPND(opnd, IL_OPND(list_idx));
                  exp_desc.rank = 0;
                  save_xref_state = xref_state;
                  xref_state      = CIF_No_Usage_Rec;
                  expr_sem(&opnd, &exp_desc);
                  xref_state      = save_xref_state;

                  /* save exp_desc */
                  arg_info_list_base      = arg_info_list_top;
                  arg_info_list_top       = arg_info_list_base + 1;

                  if (arg_info_list_top >= arg_info_list_size) {
                     enlarge_info_list_table();
                  }

                  arg_info_list[arg_info_list_top] = 
                                  arg_info_list[IL_ARG_DESC_IDX(list_idx)];
                  IL_ARG_DESC_IDX(list_idx) = arg_info_list_top;
                  arg_info_list[arg_info_list_top].ed = exp_desc;
               }

               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }
         break;

      case AT_Tbl_Idx:

         if (AT_OBJ_CLASS(OPND_IDX((*top_opnd))) == Data_Obj &&
             ATD_IMP_DO_LCV(OPND_IDX((*top_opnd)))) {

            gen_opnd(&opnd, ATD_TMP_IDX(OPND_IDX((*top_opnd))),
                     (fld_type) ATD_FLD(OPND_IDX((*top_opnd))),
                     OPND_LINE_NUM((*top_opnd)),
                     OPND_COL_NUM((*top_opnd)));
            copy_subtree(&opnd, &opnd);

            process_char_len(&opnd);
            COPY_OPND((*top_opnd), opnd);
         }
         break;

      case IL_Tbl_Idx:

         list_idx = OPND_IDX((*top_opnd));

         while (list_idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            process_char_len(&opnd);
            COPY_OPND(IL_OPND(list_idx), opnd);

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      case NO_Tbl_Idx:
         break;
   }

   TRACE (Func_Exit, "process_char_len", NULL);

   return;

}  /* process_char_len */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void expand_stmts(opnd_type 	*top_opnd,
                         expr_arg_type	*exp_desc)

{


   TRACE (Func_Entry, "expand_stmts", NULL);

   check_for_constructors(top_opnd, exp_desc);

   process_deferred_functions(top_opnd);

   TRACE (Func_Exit, "expand_stmts", NULL);

   return;

}  /* expand_stmts */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void check_for_constructors(opnd_type 		*top_opnd,
                         	   expr_arg_type	*exp_desc)

{
   int			ir_idx;
   int			list_idx;
   expr_arg_type	loc_exp_desc;
   boolean		ok;
   opnd_type		tmp_opnd;

   TRACE (Func_Entry, "check_for_constructors", NULL);

   switch (OPND_FLD((*top_opnd))) {
   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*top_opnd));

      switch (IR_OPR(ir_idx)) {

      case Array_Construct_Opr :

         loc_exp_desc = arg_info_list[IR_IDX_L(ir_idx)].ed;
         ok = create_runtime_array_constructor(top_opnd, &loc_exp_desc);
         if (exp_desc != NULL) {
            COPY_SHAPE((exp_desc->shape),
                       loc_exp_desc.shape, loc_exp_desc.rank);
         }
         break;

      case Constant_Array_Construct_Opr :

         loc_exp_desc = arg_info_list[IR_IDX_L(ir_idx)].ed;
         ok = create_constructor_constant(top_opnd, &loc_exp_desc);
         if (exp_desc != NULL) {
            COPY_SHAPE((exp_desc->shape),
                       loc_exp_desc.shape, loc_exp_desc.rank);
         }
         break;

      case Struct_Construct_Opr :
         ok = create_runtime_struct_constructor(top_opnd);
         break;

      case Constant_Struct_Construct_Opr :
         ok = create_constructor_constant(top_opnd, &loc_exp_desc);
         break;

      default:
         if (exp_desc != NULL) {
            loc_exp_desc.dope_vector = exp_desc->dope_vector;
            loc_exp_desc.pointer = exp_desc->pointer;
            loc_exp_desc.reference = exp_desc->reference;
            loc_exp_desc.tmp_reference = exp_desc->tmp_reference;
         }

         COPY_OPND(tmp_opnd, IR_OPND_L(ir_idx));
         check_for_constructors(&tmp_opnd, exp_desc);
         COPY_OPND(IR_OPND_L(ir_idx), tmp_opnd);

         COPY_OPND(tmp_opnd, IR_OPND_R(ir_idx));
         check_for_constructors(&tmp_opnd, exp_desc);
         COPY_OPND(IR_OPND_R(ir_idx), tmp_opnd);

         if (exp_desc != NULL) {
            exp_desc->dope_vector = loc_exp_desc.dope_vector;
            exp_desc->pointer = loc_exp_desc.pointer;
            exp_desc->reference = loc_exp_desc.reference;
            exp_desc->tmp_reference = loc_exp_desc.tmp_reference;
         }

         break;

      }
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*top_opnd));

      while (list_idx) {
         COPY_OPND(tmp_opnd, IL_OPND(list_idx));
         check_for_constructors(&tmp_opnd, exp_desc);
         COPY_OPND(IL_OPND(list_idx), tmp_opnd);

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;
   }

   TRACE (Func_Exit, "check_for_constructors", NULL);

   return;

}  /* check_for_constructors */
