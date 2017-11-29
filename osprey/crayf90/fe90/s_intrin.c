/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2008, 2009. PathScale, LLC. All Rights Reserved.
 */
/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_intrin.c	5.31	10/27/99 16:50:34\n";

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
# include "fmath.h"
# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
# include <fortran.h>
# endif


extern boolean has_present_opr;
#ifdef KEY /* Bug 5089 */
#ifdef TARG_X8664
extern boolean Target_SSE2;
extern boolean Target_SSE3;
#endif
#endif /* KEY Bug 5089 */

#ifdef KEY /* Bug 10410 */
/*
 * list_idx	IL_Tbl_Idx for an actual argument of the intrinsic call
 * return attr_idx if in this call, an optional dummy argument belonging to the
 *    caller is being passed as the actual argument of the intrinsic call; or
 *    return NULL_IDX otherwise
 */
static int
is_optional_dummy(int list_idx) {
  if (list_idx == NULL_IDX || IL_IDX(list_idx) == NULL_IDX) {
    return FALSE;
    }
  if (IL_FLD(list_idx) == AT_Tbl_Idx) {
    return AT_OPTIONAL(IL_IDX(list_idx));
    }
  int ignore_line, ignore_col;
  int attr_idx = find_base_attr(&IL_OPND(list_idx), &ignore_line, &ignore_col);
  return (attr_idx != NULL_IDX && AT_OPTIONAL(attr_idx)) ? attr_idx : NULL_IDX;
  }

/*
 * Generate a Cselect_Opr using "present(dummy)" as its predicate. This is
 * useful when an optional dummy argument is used as the actual argument
 * for a call to an intrinsic. We use this when we need to choose whether to
 * pass the dummy argument or a default value to the intrinsic, and also when
 * we need to choose between two different forms of calling the intrinsic.
 *
 * line			Line number for intrinsic call
 * col			Column number for intrinsic call
 * dummy_idx		AT_Tbl_Idx index for optional dummy argument
 * true_fld		fld_type for true_idx
 * true_idx		Index for true part of Cselect_Opr
 * false_fld		fld_type for false_idx
 * false_idx		Index for false part of Cselect_Opr
 * result_type_idx	Index for result type of Cselect_Opr
 * return		Index in IL_Tbl_Idx for Cselect_Opr
 */
static boolean
gen_select_present(int line, int col, int dummy_idx,
  fld_type true_fld, int true_idx,
  fld_type false_fld, int false_idx,
  int result_type_idx) {
  int present_idx = gen_ir(AT_Tbl_Idx, dummy_idx,
    Present_Opr, CG_LOGICAL_DEFAULT_TYPE, line, col,
    NO_Tbl_Idx, NULL_IDX);
  int select_arglist_idx = gen_il(3, TRUE, line, col,
    true_fld, true_idx,
    false_fld, false_idx,
    IR_Tbl_Idx, present_idx);
  int select_idx = gen_ir(IL_Tbl_Idx, select_arglist_idx,
    Cselect_Opr,
    result_type_idx, line, col,
    NO_Tbl_Idx, NULL_IDX);
  return select_idx;
  }

/*
 * When a caller is passing an optional dummy argument as the actual argument
 * in a call to an intrinsic, replace the dummy argument "d" with a "select"
 * expression, e.g. "(present(attr_idx) ? list_idx : default)".
 * list_idx	IL_Tbl_Idx for the optional dummy argument which is being
 *		passed as the actual argument
 * default_fld	fld_type for default_idx
 * default_idx	Index for a value to be used if the optional argument
 *		is not present
 * default_type_idx	Type index for default value
 * need_temp	Store the constant into a compiler temp and pass that (needed
 *		for call-by-reference)
 */
static void
pass_dummy_or_default(int list_idx, fld_type default_fld, int default_idx,
  int default_type_idx, boolean need_temp) {
  int ignore_line, ignore_col;
  int attr_idx = find_base_attr(&IL_OPND(list_idx), &ignore_line, &ignore_col);
  int select_idx =
    gen_select_present(IL_LINE_NUM(list_idx), IL_COL_NUM(list_idx), attr_idx,
      IL_FLD(list_idx), IL_IDX(list_idx),
      default_fld, default_idx, default_type_idx);
  int line = IL_LINE_NUM(list_idx);
  int col = IL_COL_NUM(list_idx);
  if (need_temp) {
    int tmp_attr = gen_compiler_tmp(line, col, Priv, TRUE);
    int cn_type_idx = ATD_TYPE_IDX(tmp_attr) = CN_TYPE_IDX(default_idx);
    ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
    AT_SEMANTICS_DONE(tmp_attr) = TRUE;
    int asg_idx = gen_ir(AT_Tbl_Idx, tmp_attr, Asg_Opr, cn_type_idx, line, col,
      IR_Tbl_Idx, select_idx);
    gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
    SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
    IL_FLD(list_idx) = AT_Tbl_Idx;
    IL_IDX(list_idx) = tmp_attr;
    }
  else {
    IL_FLD(list_idx) = IR_Tbl_Idx;
    IL_IDX(list_idx) = select_idx;
    }
}

/* Like pass_dummy_or_default, but default must be constant */
static void
pass_dummy_or_default_const(int list_idx, int default_idx, boolean need_temp) {
  pass_dummy_or_default(list_idx, CN_Tbl_Idx, default_idx,
    CN_TYPE_IDX(default_idx), need_temp);
  }
#endif /* KEY Bug 10410 */
#ifdef KEY /* Bug 12482 */
/*
 * When a call to the REAL intrinsic is given a typeless constant argument,
 * use this function to convert the typeless value to an integer or a real
 * bit pattern
 *
 * list_idx1	IL_Tbl_Idx for first (A) argument, assumed typeless
 * list_idx2	IL_Tbl_Idx for second (KIND) argument, or NULL_IDX
 */
static void
typeless_to_type(int list_idx1, Uint result_type_idx) {
  long_type dst[MAX_WORDS_FOR_INTEGER];
  memset(dst, 0, MAX_WORDS_FOR_INTEGER * sizeof *dst);
  long_type *src = (long_type *) &CN_CONST(IL_IDX(list_idx1));
  int info_idx1 = IL_ARG_DESC_IDX(list_idx1);
  Uint src_type_idx = arg_info_list[info_idx1].ed.type_idx;
  int src_len = TYP_BIT_LEN(src_type_idx) / TARGET_BITS_PER_WORD;

  /* F2003 treats boz constant as giant integer, creates real having same
   * bit pattern; but g77 treats typeless constant as integer, generating
   * a real having the same magnitude. Our strategy is to create a new
   * constant having an integer or real type, copying into it the original
   * bit pattern padded or truncated to fit that type, and let the "real"
   * intrinsic operate on that constant. */
  if (!(on_off_flags.issue_ansi_messages || on_off_flags.fortran2003)) {
    result_type_idx = Integer_8;
    }
  linear_type_type result_linear_type = TYP_LINEAR(result_type_idx);
  int dst_len = num_host_wds[result_linear_type];

  copy_and_pad_boz(dst, dst_len, src, src_len);
  IL_IDX(list_idx1) = ntr_const_tbl(result_type_idx, TRUE, dst);
}
#endif /* KEY Bug 12482 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      generate an array constructor of lower and upper bounds from a bd ntry*|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

static void generate_bounds_list(int            bd_idx,
                                 opnd_type      *result_opnd,
                                 expr_arg_type  *exp_desc)

{

   int                  col;
   int                  i;
   int                  ir_idx;
   int                  line;
   int                  list_idx = NULL_IDX;
   opnd_type            opnd;
   cif_usage_code_type  save_xref_state;


   TRACE (Func_Entry, "generate_bounds_list", NULL);

   find_opnd_line_and_column(result_opnd, &line, &col);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Array_Construct_Opr;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_R(ir_idx) = 2 * BD_RANK(bd_idx);


   for (i = 1; i <= BD_RANK(bd_idx); i++) {
      if (list_idx == NULL_IDX) {
         NTR_IR_LIST_TBL(list_idx);
         IR_IDX_R(ir_idx) = list_idx;
      }
      else {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
      IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = col;

      COPY_OPND(opnd, IL_OPND(list_idx));
      cast_opnd_to_type_idx(&opnd, CG_INTEGER_DEFAULT_TYPE);
      COPY_OPND(IL_OPND(list_idx), opnd);

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size &&
          i == BD_RANK(bd_idx)) {

         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      }
      else {
         IL_FLD(list_idx) = BD_UB_FLD(bd_idx, i);
         IL_IDX(list_idx) = BD_UB_IDX(bd_idx, i);
      }

      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = col;

      COPY_OPND(opnd, IL_OPND(list_idx));
      cast_opnd_to_type_idx(&opnd, CG_INTEGER_DEFAULT_TYPE);
      COPY_OPND(IL_OPND(list_idx), opnd);
   }

   save_xref_state = xref_state;
   xref_state = CIF_No_Usage_Rec;
   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = ir_idx;
   exp_desc->rank = 0;
   expr_semantics(result_opnd, exp_desc);
   xref_state = save_xref_state;

   TRACE (Func_Exit, "generate_bounds_list", NULL);

   return;

}  /* generate_bounds_list */



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

static int cri_ptr_type(int	type_idx)

{
   int		ptr_type;


   TRACE (Func_Entry, "cri_ptr_type", NULL);

   ptr_type = CRI_Ptr_8;

# ifdef _TRANSFORM_CHAR_SEQUENCE
   if (TYP_TYPE(type_idx) == Character ||
       (TYP_TYPE(type_idx) == Structure &&
        ATT_CHAR_SEQ(TYP_IDX(type_idx))))
# else
   if (TYP_TYPE(type_idx) == Character)
# endif
                                          {

      ptr_type = CRI_Ch_Ptr_8;
   }
# ifdef _TARGET32
   else if (TARGET_32BIT_DOUBLE_WORD_STORAGE_TYPE(type_idx) ||
            TYP_LINEAR(type_idx) == Complex_4) {

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)                    = CRI_Ptr;
      TYP_LINEAR(TYP_WORK_IDX)                  = CRI_Ptr_8;
      TYP_PTR_INCREMENT(TYP_WORK_IDX)           = 64;
      ptr_type  = ntr_type_tbl();

   }
# endif

# ifdef _TARGET_OS_MAX
   else if (TARGET_MAX_HALF_WORD_STORAGE_TYPE(type_idx)) {

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)                    = CRI_Ptr;
      TYP_LINEAR(TYP_WORK_IDX)                  = CRI_Ptr_8;
      TYP_PTR_INCREMENT(TYP_WORK_IDX)           = 32;
      ptr_type  = ntr_type_tbl();
   }
# endif


   TRACE (Func_Exit, "cri_ptr_type", NULL);

   return(ptr_type);

}  /* cri_ptr_type */


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

#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#if COMPILER_VERSION < 730
static void dummydummydummy(void *a, void *b){}
#endif
#endif

static boolean optimize_reshape(opnd_type	*result_opnd,
			        expr_arg_type	*res_exp_desc)

{
   int			asg_idx;
   int			attr_idx;
   int			bd_idx;
   int			col;
   expr_arg_type	exp_desc1;
   expr_arg_type	exp_desc2;
   expr_arg_type	exp_desc4;
   long			i;
   int			info_idx1;
   int			info_idx2;
   int			info_idx4;
   int			ir_idx;
   int			line;
   int			list_idx1;
   int			list_idx2;
   int			list_idx3;
   int			list_idx4;
   expr_arg_type	loc_exp_desc;
   int			loc_idx;
   opnd_type		l_opnd;
   boolean		ok;
   opnd_type		opnd;
   boolean		optimized = FALSE;
   boolean		equal     = TRUE;
   int			ptee_idx;
   int			ptr_idx;
   opnd_type		r_opnd;
   int			type_idx;
   int			unused1;
   int			unused2;


   TRACE (Func_Entry, "optimize_reshape", NULL);

   if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX((*result_opnd))) == Call_Opr) {

      ir_idx = OPND_IDX((*result_opnd));

      list_idx1 = IR_IDX_R(ir_idx);
      list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
      list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
      list_idx4 = IL_NEXT_LIST_IDX(list_idx3);

      info_idx1 = IL_ARG_DESC_IDX(list_idx1);
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);

#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#if COMPILER_VERSION < 730
      /* Work around 7.2.1.2 optimizer bug */
      dummydummydummy(&info_idx1,&info_idx2);
#endif
#endif

      exp_desc1 = arg_info_list[info_idx1].ed;
      exp_desc2 = arg_info_list[info_idx2].ed;
 
      if (IL_FLD(list_idx4) != NO_Tbl_Idx) {
         info_idx4 = IL_ARG_DESC_IDX(list_idx4);
         exp_desc4 = arg_info_list[info_idx4].ed;

         if (exp_desc4.foldable) {

            attr_idx = find_base_attr(&IL_OPND(list_idx4), &line, &col);
            loc_exp_desc = init_exp_desc;
            loc_exp_desc.type_idx = ATD_TYPE_IDX(attr_idx);
            loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
            loc_exp_desc.linear_type = TYP_LINEAR(loc_exp_desc.type_idx);

            loc_exp_desc.foldable = TRUE;
            loc_exp_desc.constant = TRUE;

            for (i = 1; i <= res_exp_desc->rank; i++) {
               change_section_to_this_element(&IL_OPND(list_idx4),
                                              &opnd,
                                              i);

               ok = fold_aggragate_expression(&opnd,
                                              &loc_exp_desc,
                                              TRUE);

               equal = equal && compare_cn_and_value(OPND_IDX(opnd), i, Eq_Opr);
            }

            if (equal && compare_cn_and_value(OPND_IDX(exp_desc4.shape[0]), 
                                              (long) res_exp_desc->rank, 
                                              Eq_Opr)) {
               IL_OPND(list_idx4) = null_opnd;   
            }
         }

      }

      if (IL_FLD(list_idx3) == NO_Tbl_Idx &&
          IL_FLD(list_idx4) == NO_Tbl_Idx) {

         if (exp_desc1.reference    ||
             exp_desc1.tmp_reference) {

            if (! exp_desc1.contig_array) {
               goto EXIT;
            }

            attr_idx = find_base_attr(&IL_OPND(list_idx1), &line, &col);

            if (ATD_POINTER(attr_idx)) {
               goto EXIT;
            }

            if (ATD_ARRAY_IDX(attr_idx) &&
                BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape) {
               goto EXIT;
            }
         }
         else {
            /* not a reference, this would be a copy in anyway */

            COPY_OPND(r_opnd, IL_OPND(list_idx1));
            attr_idx = create_tmp_asg(&r_opnd,
                                      &exp_desc1,
                                      &l_opnd,
                                      Intent_In,
                                      FALSE,
                                      FALSE);

            COPY_OPND(IL_OPND(list_idx1), l_opnd);
            arg_info_list[info_idx1].ed = exp_desc1;
         }
              

         if (! exp_desc2.reference &&
             ! exp_desc2.tmp_reference) {

            COPY_OPND(r_opnd, IL_OPND(list_idx2));
            attr_idx = create_tmp_asg(&r_opnd,
                                      &exp_desc2,
                                      &l_opnd,
                                      Intent_In,
                                      FALSE,
                                      FALSE);

            COPY_OPND(IL_OPND(list_idx2), l_opnd);
            arg_info_list[info_idx2].ed = exp_desc2;
         }

         attr_idx = find_base_attr(&IL_OPND(list_idx2), &line, &col);
         loc_exp_desc = init_exp_desc;
         loc_exp_desc.type_idx = ATD_TYPE_IDX(attr_idx);
         loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
         loc_exp_desc.linear_type = TYP_LINEAR(loc_exp_desc.type_idx);

         if (exp_desc2.foldable) {
            loc_exp_desc.foldable = TRUE;
            loc_exp_desc.constant = TRUE;
         }

         for (i = 1; i <= res_exp_desc->rank; i++) {

            change_section_to_this_element(&IL_OPND(list_idx2),
                                           &opnd,
                                           i);
            if (exp_desc2.foldable) {
               ok = fold_aggragate_expression(&opnd,
                                              &loc_exp_desc,
                                              TRUE);
            }

            COPY_OPND(res_exp_desc->shape[i-1], opnd);
         }

         if (gen_bd_entry(NULL, res_exp_desc, &bd_idx, line, col)) {
            /* intentionally blank */
         }
   
         type_idx = cri_ptr_type(exp_desc1.type_idx);

         /* generate the ptr/pointee pair */

         ptr_idx  = gen_compiler_tmp(line, col, Shared, TRUE);
         ATD_TYPE_IDX(ptr_idx) = type_idx;
         AT_SEMANTICS_DONE(ptr_idx) = TRUE;
         ATD_STOR_BLK_IDX(ptr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

         ptee_idx = gen_compiler_tmp(line, col, Shared, TRUE);
         ATD_CLASS(ptee_idx) = CRI__Pointee;
         AT_SEMANTICS_DONE(ptee_idx) = TRUE;
         ATD_STOR_BLK_IDX(ptee_idx) = SCP_SB_BASED_IDX(curr_scp_idx);
         ATD_TYPE_IDX(ptee_idx) = exp_desc1.type_idx;
         ATD_ARRAY_IDX(ptee_idx) = bd_idx;
         ATD_PTR_IDX(ptee_idx) = ptr_idx;

         /* generate assignment to ptr */

         attr_idx = find_base_attr(&IL_OPND(list_idx1), &unused1, &unused2);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_NOT_PT_UNIQUE_MEM(attr_idx) = TRUE;
         }
# endif

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             ATD_CLASS(attr_idx) == Compiler_Tmp &&
             exp_desc1.type != Character &&
             ATD_IM_A_DOPE(attr_idx)) {

            asg_idx = gen_ir(AT_Tbl_Idx, ptr_idx,
                         Asg_Opr, type_idx, line, col,
                             IR_Tbl_Idx, gen_ir(AT_Tbl_Idx, attr_idx,
                                           Dv_Access_Base_Addr,
                                             SA_INTEGER_DEFAULT_TYPE,line,col,
                                                NO_Tbl_Idx, NULL_IDX));

         }
         else {

         COPY_OPND(opnd, IL_OPND(list_idx1));
         unused1 = NULL_IDX;
         unused2 = NULL_IDX;
         make_base_subtree(&opnd, &r_opnd, &unused1, &unused2);

         loc_idx = gen_ir(OPND_FLD(r_opnd), OPND_IDX(r_opnd),
                      Loc_Opr, type_idx, line, col,
                          NO_Tbl_Idx, NULL_IDX);

# ifdef _TRANSFORM_CHAR_SEQUENCE
         if (exp_desc1.type == Structure &&
             ATT_CHAR_SEQ(TYP_IDX(exp_desc1.type_idx))) {

            COPY_OPND(opnd, IR_OPND_L(loc_idx));
            transform_char_sequence_ref(&opnd, exp_desc1.type_idx);
            COPY_OPND(IR_OPND_L(loc_idx), opnd);
         }
# endif

         asg_idx = gen_ir(AT_Tbl_Idx, ptr_idx,
                      Asg_Opr, type_idx, line, col,
                          IR_Tbl_Idx, loc_idx);

         }
         
         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         gen_opnd(result_opnd, ptee_idx, AT_Tbl_Idx, line, col);

         res_exp_desc->tmp_reference = TRUE;
         ok = gen_whole_subscript(result_opnd, res_exp_desc);

         optimized = TRUE;
      }
   }

EXIT:

   TRACE (Func_Exit, "optimize_reshape", NULL);

   return(optimized);

}  /* optimize_reshape */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Check conformance of the operands to an elemental intrinsic.          *|
|*      Also, return the index of the argument to extract the rank/shape from.*|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void   conform_check(int           check_args,
                     int           ir_idx,
                     expr_arg_type *res_exp_desc,
                     int           *spec_idx,
		     boolean	   assumed_size_allowed)
{
   int            line;
   int            col;
#ifdef KEY /* Bug 10177 */
   int            which_arg = 0;
#else /* KEY Bug 10177 */
   int            which_arg;
#endif /* KEY Bug 10177 */
   int            max_rank;
   int            attr_idx;
   int		  temp_ir_idx;
   int            i;
   int		  info_idx;


   TRACE (Func_Entry, "conform_check", NULL);

   max_rank = 0;
 
   temp_ir_idx = IR_IDX_R(ir_idx);

   if (temp_ir_idx != NULL_IDX) {  /* are there any arguments */
      which_arg = IL_ARG_DESC_IDX(temp_ir_idx);
   }

   res_exp_desc->will_fold_later = TRUE;
   res_exp_desc->foldable = TRUE;

   for (i = 1; i <= IR_LIST_CNT_R(ir_idx); i++) {

       if (IL_FLD(temp_ir_idx) == NO_Tbl_Idx) {
          temp_ir_idx = IL_NEXT_LIST_IDX(temp_ir_idx);
          continue;
       }

       info_idx = IL_ARG_DESC_IDX(temp_ir_idx);

       if (! assumed_size_allowed &&
           arg_info_list[info_idx].ed.rank != 0 &&
           (IL_FLD(temp_ir_idx) == AT_Tbl_Idx ||
            (IL_FLD(temp_ir_idx) == IR_Tbl_Idx &&
             IR_OPR(IL_IDX(temp_ir_idx)) == Whole_Substring_Opr &&
             IR_FLD_L(IL_IDX(temp_ir_idx)) == AT_Tbl_Idx))) {

           PRINTMSG(arg_info_list[info_idx].line, 412, Error,
                    arg_info_list[info_idx].col);
       }

       attr_idx = 0;
       if ((IL_FLD(temp_ir_idx) == IR_Tbl_Idx) &&
          ((IR_OPR(IL_IDX(temp_ir_idx)) == Whole_Subscript_Opr) ||
           (IR_OPR(IL_IDX(temp_ir_idx)) == Section_Subscript_Opr))) {
          attr_idx = find_base_attr(&IL_OPND(temp_ir_idx), &line, &col);
       }

       if ((check_args != 0) &&
           (i >= check_args) &&
           (arg_info_list[info_idx].ed.rank != max_rank) &&
           (attr_idx != 0) &&
           (!(ATP_INTRIN_ENUM(*spec_idx) == Present_Intrinsic)) &&
           (AT_OPTIONAL(attr_idx))) {
           PRINTMSG(arg_info_list[info_idx].line, 947,  Error, 
                    arg_info_list[info_idx].col);
       }

       if (!arg_info_list[info_idx].ed.foldable && 
           !arg_info_list[info_idx].ed.will_fold_later) {
          res_exp_desc->will_fold_later = FALSE;
       }

       if (! arg_info_list[info_idx].ed.foldable) {
          res_exp_desc->foldable = FALSE;
       }

       if (max_rank != 0 &&       
           AT_ELEMENTAL_INTRIN(*spec_idx) &&
           arg_info_list[info_idx].ed.rank != 0 &&
           max_rank != arg_info_list[info_idx].ed.rank) {
          PRINTMSG(arg_info_list[info_idx].line, 363,  Error, 
                   arg_info_list[info_idx].col);
       }

       if (arg_info_list[info_idx].ed.rank > max_rank) {
          max_rank = arg_info_list[info_idx].ed.rank;
          which_arg = info_idx;
       }

       temp_ir_idx = IL_NEXT_LIST_IDX(temp_ir_idx);
   }

   if (ATP_PGM_UNIT(*spec_idx) != Subroutine) {
      res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
      res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
   }
   res_exp_desc->rank = max_rank;

   if (max_rank > 0 && AT_ELEMENTAL_INTRIN(*spec_idx))  {
      COPY_SHAPE(res_exp_desc->shape,
                 arg_info_list[which_arg].ed.shape,
                 arg_info_list[which_arg].ed.rank);
   }

   TRACE (Func_Exit, "conform_check", NULL);

}  /* conform_check */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SIN(X) intrinsic.                                         *|
|*      Function    DSIN(X) intrinsic.                                        *|
|*      Function    QSIN(X) intrinsic.                                        *|
|*      Function    CSIN(X) intrinsic.                                        *|
|*      Function    CDSIN(X) intrinsic.                                       *|
|*      Function    CQSIN(X) intrinsic.                                       *|
|*      Function    SIND(X) intrinsic.                                        *|
|*      Function    DSIND(X) intrinsic.                                       *|
|*      Function    QSIND(X) intrinsic.                                       *|
|*      Function    SINH(X) intrinsic.                                        *|
|*      Function    DSINH(X) intrinsic.                                       *|
|*      Function    QSINH(X) intrinsic.                                       *|
|*      Function    ASIN(X) intrinsic.                                        *|
|*      Function    DASIN(X) intrinsic.                                       *|
|*      Function    QASIN(X) intrinsic.                                       *|
|*      Function    ASIND(X) intrinsic.                                       *|
|*      Function    DASIND(X) intrinsic.                                      *|
|*      Function    QASIND(X) intrinsic.                                      *|
|*      Function    COS(X) intrinsic.                                         *|
|*      Function    DCOS(X) intrinsic.                                        *|
|*      Function    QCOS(X) intrinsic.                                        *|
|*      Function    CCOS(X) intrinsic.                                        *|
|*      Function    CDCOS(X) intrinsic.                                       *|
|*      Function    CQCOS(X) intrinsic.                                       *|
|*      Function    COSD(X) intrinsic.                                        *|
|*      Function    DCOSD(X) intrinsic.                                       *|
|*      Function    QCOSD(X) intrinsic.                                       *|
|*      Function    COSH(X) intrinsic.                                        *|
|*      Function    DCOSH(X) intrinsic.                                       *|
|*      Function    QCOSH(X) intrinsic.                                       *|
|*      Function    ACOS(X) intrinsic.                                        *|
|*      Function    DACOS(X) intrinsic.                                       *|
|*      Function    QACOS(X) intrinsic.                                       *|
|*      Function    ACOSD(X) intrinsic.                                       *|
|*      Function    DACOSD(X) intrinsic.                                      *|
|*      Function    QACOSD(X) intrinsic.                                      *|
|*      Function    TAN(X) intrinsic.                                         *|
|*      Function    DTAN(X) intrinsic.                                        *|
|*      Function    QTAN(X) intrinsic.                                        *|
|*      Function    TAND(X) intrinsic.                                        *|
|*      Function    DTAND(X) intrinsic.                                       *|
|*      Function    QTAND(X) intrinsic.                                       *|
|*      Function    TANH(X) intrinsic.                                        *|
|*      Function    DTANH(X) intrinsic.                                       *|
|*      Function    QTANH(X) intrinsic.                                       *|
|*      Function    ATAN(X) intrinsic.                                        *|
|*      Function    DATAN(X) intrinsic.                                       *|
|*      Function    QATAN(X) intrinsic.                                       *|
|*      Function    ATAND(X) intrinsic.                                       *|
|*      Function    DATAND(X) intrinsic.                                      *|
|*      Function    QATAND(X) intrinsic.                                      *|
|*      Function    LOG(X) intrinsic.                                         *|
|*      Function    DLOG(X) intrinsic.                                        *|
|*      Function    QLOG(X) intrinsic.                                        *|
|*      Function    CDLOG(X) intrinsic.                                       *|
|*      Function    CQLOG(X) intrinsic.                                       *|
|*      Function    LOG10(X) intrinsic.                                       *|
|*      Function    DLOG10(X) intrinsic.                                      *|
|*      Function    QLOG10(X) intrinsic.                                      *|
|*      Function    EXP(X) intrinsic.                                         *|
|*      Function    DEXP(X) intrinsic.                                        *|
|*      Function    QEXP(X) intrinsic.                                        *|
|*      Function    CEXP(X) intrinsic.                                        *|
|*      Function    CDEXP(X) intrinsic.                                       *|
|*      Function    CQEXP(X) intrinsic.                                       *|
|*      Function    COT(X) intrinsic.                                         *|
|*      Function    DCOT(X) intrinsic.                                        *|
|*      Function    QCOT(X) intrinsic.                                        *|
|*      Function    SQRT(X) intrinsic.                                        *|
|*      Function    DSQRT(X) intrinsic.                                       *|
|*      Function    QSQRT(X) intrinsic.                                       *|
|*      Function    CSQRT(X) intrinsic.                                       *|
|*      Function    CDSQRT(X) intrinsic.                                      *|
|*      Function    CQSQRT(X) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    sin_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{

   int		info_idx1;
   int		list_idx1;
   int		ir_idx;


   TRACE (Func_Entry, "sin_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
      case Sin_Intrinsic:
      case Dsin_Intrinsic:
      case Qsin_Intrinsic:
      case Csin_Intrinsic:
      case Cdsin_Intrinsic:
      case Cqsin_Intrinsic:
# ifdef KEY
      case Zsin_Intrinsic:
# endif
         IR_OPR(ir_idx) = Sin_Opr;
         break;

      case Sind_Intrinsic:
      case Dsind_Intrinsic:
      case Qsind_Intrinsic:
         IR_OPR(ir_idx) = Sind_Opr;
         break;

      case Cos_Intrinsic:
      case Dcos_Intrinsic:
      case Qcos_Intrinsic:
      case Ccos_Intrinsic:
      case Cdcos_Intrinsic:
      case Cqcos_Intrinsic:
# ifdef KEY
      case Zcos_Intrinsic:
# endif
         IR_OPR(ir_idx) = Cos_Opr;
         break;

      case Cosd_Intrinsic:
      case Dcosd_Intrinsic:
      case Qcosd_Intrinsic:
         IR_OPR(ir_idx) = Cosd_Opr;
         break;

      case Log_Intrinsic:
      case Alog_Intrinsic:
      case Dlog_Intrinsic:
      case Qlog_Intrinsic:
      case Clog_Intrinsic:
      case Cdlog_Intrinsic:
      case Cqlog_Intrinsic:
# ifdef KEY
      case Zlog_Intrinsic:
# endif
         if ((IL_FLD(list_idx1) == CN_Tbl_Idx) &&
             (arg_info_list[info_idx1].ed.type == Real)) {

            if (fold_relationals(IL_IDX(list_idx1),
                                 CN_INTEGER_ZERO_IDX,
                                 Le_Opr)) {

               PRINTMSG(arg_info_list[info_idx1].line, 1062, Error,
                        arg_info_list[info_idx1].col);
            }
         }

         IR_OPR(ir_idx) = Log_E_Opr;
         break;

      case Log10_Intrinsic:
      case Alog10_Intrinsic:
      case Dlog10_Intrinsic:
      case Qlog10_Intrinsic:
         IR_OPR(ir_idx) = Log_10_Opr;
         break;

      case Tan_Intrinsic:
      case Dtan_Intrinsic:
      case Qtan_Intrinsic:
         IR_OPR(ir_idx) = Tan_Opr;
         break;

      case Tand_Intrinsic:
      case Dtand_Intrinsic:
      case Qtand_Intrinsic:
         IR_OPR(ir_idx) = Tand_Opr;
         break;

      case Tanh_Intrinsic:
      case Dtanh_Intrinsic:
      case Qtanh_Intrinsic:
         IR_OPR(ir_idx) = Tanh_Opr;
         break;

      case Sinh_Intrinsic:
      case Dsinh_Intrinsic:
      case Qsinh_Intrinsic:
         IR_OPR(ir_idx) = Sinh_Opr;
         break;

      case Cosh_Intrinsic:
      case Dcosh_Intrinsic:
      case Qcosh_Intrinsic:
         IR_OPR(ir_idx) = Cosh_Opr;
         break;

      case Acos_Intrinsic:
      case Dacos_Intrinsic:
      case Qacos_Intrinsic:
         IR_OPR(ir_idx) = Acos_Opr;
         break;

      case Acosd_Intrinsic:
      case Dacosd_Intrinsic:
      case Qacosd_Intrinsic:
         IR_OPR(ir_idx) = Acosd_Opr;
         break;

      case Asin_Intrinsic:
      case Dasin_Intrinsic:
      case Qasin_Intrinsic:
         IR_OPR(ir_idx) = Asin_Opr;
         break;

      case Asind_Intrinsic:
      case Dasind_Intrinsic:
      case Qasind_Intrinsic:
         IR_OPR(ir_idx) = Asind_Opr;
         break;

      case Atan_Intrinsic:
      case Datan_Intrinsic:
      case Qatan_Intrinsic:
         IR_OPR(ir_idx) = Atan_Opr;
         break;

      case Atand_Intrinsic:
      case Datand_Intrinsic:
      case Qatand_Intrinsic:
         IR_OPR(ir_idx) = Atand_Opr;
         break;

      case Cot_Intrinsic:
      case Dcot_Intrinsic:
      case Qcot_Intrinsic:
         IR_OPR(ir_idx) = Cot_Opr;
         break;

      case Exp_Intrinsic:
      case Dexp_Intrinsic:
      case Qexp_Intrinsic:
      case Cexp_Intrinsic:
      case Cdexp_Intrinsic:
      case Cqexp_Intrinsic:
# ifdef KEY
      case Zexp_Intrinsic:
# endif
         IR_OPR(ir_idx) = Exp_Opr;
         break;

      case Sqrt_Intrinsic:
      case Dsqrt_Intrinsic:
      case Qsqrt_Intrinsic:
      case Csqrt_Intrinsic:
      case Cdsqrt_Intrinsic:
      case Cqsqrt_Intrinsic:
# ifdef KEY
      case Zsqrt_Intrinsic:
# endif
         if ((IL_FLD(list_idx1) == CN_Tbl_Idx) &&
             (arg_info_list[info_idx1].ed.type == Real)) {

            if (fold_relationals(IL_IDX(list_idx1),
                                 CN_INTEGER_ZERO_IDX,
                                 Lt_Opr)) {

               PRINTMSG(arg_info_list[info_idx1].line, 1062, Error,
                        arg_info_list[info_idx1].col);
            }
         }

         IR_OPR(ir_idx) = Sqrt_Opr;
         break;

      default:
         PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal, IR_COL_NUM(ir_idx),
                  "sin_intrinsic");
         break;
   }

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

# if defined(_USE_FOLD_DOT_f)
   if (IR_OPR(ir_idx) != Sqrt_Opr) {      
# endif
      res_exp_desc->foldable = FALSE;
      res_exp_desc->will_fold_later = FALSE;
# if defined(_USE_FOLD_DOT_f)
   }
# endif

   /* set this flag so this opr is pulled off io lists */
   io_item_must_flatten = TRUE;

   TRACE (Func_Exit, "sin_intrinsic", NULL);

}  /* sin_intrinsic */
#ifdef KEY /* Bug 1324 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ERF(X) intrinsic.                                         *|
|*      Function    ERFC(X) intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    erf_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{

   int		info_idx1;
   int		list_idx1;
   int		ir_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   opnd_type	  opnd;
# endif

   TRACE (Func_Entry, "erf_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 4232 */
   /* If we're defining a statement function X which calls some
    * other function Y, there's no need to generate code to copy
    * into temp(s) the actual argument(s) to Y, because we will
    * do that when the user program calls the statement function.
    * It could be harmful to do that now, since the actual arg to
    * Y might be a dummy arg of X, which has no actual address. */
   if (!defining_stmt_func) {
#endif /* KEY Bug 4232 */
     COPY_OPND(opnd, IR_OPND_R(ir_idx));
     final_arg_work(&opnd, IR_IDX_L(ir_idx), IR_LIST_CNT_R(ir_idx), NULL);
     COPY_OPND(IR_OPND_R(ir_idx), opnd);
#ifdef KEY /* Bug 4232 */
   }
#endif /* KEY Bug 4232 */
# endif

   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
      case Derf_Intrinsic:
      case Erf_Intrinsic:
         IR_OPR(ir_idx) = Erf_Opr;
         break;

      case Derfc_Intrinsic:
      case Erfc_Intrinsic:
         IR_OPR(ir_idx) = Erfc_Opr;
         break;

      default:
         PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal, IR_COL_NUM(ir_idx),
                  "erf_intrinsic");
         break;
   }

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   /* set this flag so this opr is pulled off io lists */
   io_item_must_flatten = TRUE;

   TRACE (Func_Exit, "erf_intrinsic", NULL);

}  /* erf_intrinsic */
#endif /* KEY Bug 1324 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ABS(A) intrinsic.                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    abs_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            ir_idx;
   int            info_idx1;
   int            list_idx1;
   int            type_idx;


   TRACE (Func_Entry, "abs_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   type_idx = arg_info_list[info_idx1].ed.type_idx;

   if (TYP_TYPE(type_idx) == Complex) {
      switch (TYP_LINEAR(type_idx)) {
        case Complex_16:
          type_idx = Real_16;
          break;

        case Complex_8: 
          type_idx = Real_8;
          break;

        case Complex_4: 
          type_idx = Real_4;
          break;
      }
   }

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);
   res_exp_desc->type = TYP_TYPE(type_idx);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx && 
       arg_info_list[info_idx1].ed.type == Integer &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     Abs_Opr)) {

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Abs_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;

      if (arg_info_list[info_idx1].ed.type != Integer) {
         /* must reset foldable and will_fold_later because there is no */
         /* folder for this intrinsic in constructors.                  */

         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;
      }
   }

   TRACE (Func_Exit, "abs_intrinsic", NULL);

}  /* abs_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ATAN2(Y, X) intrinsic.                                    *|
|*      Function    ATAN2D(Y, X) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    atan2_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;


   TRACE (Func_Entry, "atan2_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (arg_info_list[info_idx1].ed.linear_type !=
       arg_info_list[info_idx2].ed.linear_type) {
      PRINTMSG(arg_info_list[info_idx2].line, 774, Error,
               arg_info_list[info_idx2].col);
   }     

   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
      case Atan2_Intrinsic:
      case Datan2_Intrinsic:
      case Qatan2_Intrinsic:
         IR_OPR(ir_idx) = Atan2_Opr;
         break;

      case Atan2d_Intrinsic:
      case Datan2d_Intrinsic:
      case Qatan2d_Intrinsic:
         IR_OPR(ir_idx) = Atan2d_Opr;
         break;

      default:
         PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal, IR_COL_NUM(ir_idx),
                  "atan2_intrinsic");
         break;
   }

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "atan2_intrinsic", NULL);

}  /* atan2_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    AIMAG(Z) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    aimag_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
#ifdef KEY /* Bug 10177 */
   int            type_idx = 0;
#else /* KEY Bug 10177 */
   int            type_idx;
#endif /* KEY Bug 10177 */
   int            info_idx1;
   int            list_idx1;


   TRACE (Func_Entry, "aimag_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   switch (arg_info_list[info_idx1].ed.linear_type) {
     case Complex_4:   type_idx = Real_4;   break;
     case Complex_8:   type_idx = Real_8;   break;
     case Complex_16:  type_idx = Real_16;  break;
   }

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   IR_OPR(ir_idx) = Aimag_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "aimag_intrinsic", NULL);

}  /* aimag_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SHORT(A) intrinsic.                    		      *|
|*      Function    LONG(A) intrinsic.                    		      *|
|*      Function    IDINT(A) intrinsic.                                       *|
|*      Function    IIDINT(A) intrinsic.                                      *|
|*      Function    JIDINT(A) intrinsic.                                      *|
|*      Function    KIDINT(A) intrinsic.                                      *|
|*      Function    IQINT(A) intrinsic.                                       *|
|*      Function    IIQINT(A) intrinsic.                                      *|
|*      Function    JIQINT(A) intrinsic.                                      *|
|*      Function    KIQINT(A) intrinsic.                                      *|
|*      Function    INT(A, KIND) intrinsic.                                   *|
|*      Function    INT1(A) intrinsic.                                        *|
|*      Function    INT2(A) intrinsic.                                        *|
|*      Function    INT4(A) intrinsic.                                        *|
|*      Function    INT8(A) intrinsic.                                        *|
|*      Function    IINT(A) intrinsic.                                        *|
|*      Function    JINT(A) intrinsic.                                        *|
|*      Function    KINT(A) intrinsic.                                        *|
|*      Function    IFIX(A) intrinsic.                                        *|
|*      Function    IIFIX(A) intrinsic.                                       *|
|*      Function    JIFIX(A) intrinsic.                                       *|
|*      Function    KIFIX(A) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void   int_intrinsic(opnd_type     *result_opnd,
                     expr_arg_type *res_exp_desc,
                     int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   opnd_type	  opnd;
   int 		  type_idx;


   TRACE (Func_Entry, "int_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))), 
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }

   if (ATP_INTRIN_ENUM(*spec_idx) == Int1_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_1;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Short_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Int2_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iint_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iifix_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iidint_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iiqint_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_2;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Long_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Int4_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Jint_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Jifix_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Jidint_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Jiqint_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_4;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Kint_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Int8_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Kifix_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Kidint_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Kiqint_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
   }

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (arg_info_list[info_idx1].ed.type == Real) {
      COPY_OPND(opnd, IL_OPND(list_idx1));
      look_for_real_div(&opnd);
      COPY_OPND(IL_OPND(list_idx1), opnd);
   }

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

#ifdef KEY /* Bug 12482 */
   if (arg_info_list[info_idx1].ed.linear_type == Short_Typeless_Const) {
      typeless_to_type(list_idx1, type_idx);
      COPY_OPND(*result_opnd, IL_OPND(list_idx1));
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else
#endif /* KEY Bug 12482 */

   if (IL_FLD(list_idx1) == CN_Tbl_Idx && 
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                        arg_info_list[info_idx1].ed.type_idx,
                        NULL,
                        NULL_IDX,
                        folded_const,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        1,
                        Int_Opr)) {

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {       
      IR_OPR(ir_idx) = Int_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
      IR_LIST_CNT_L(ir_idx) = 1;
      IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx)) = NULL_IDX;
   }

   TRACE (Func_Exit, "int_intrinsic", NULL);

}  /* int_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    IAND(I, J) intrinsic.                                     *|
|*      Function    IIAND(I, J) intrinsic.                                    *|
|*      Function    JIAND(I, J) intrinsic.                                    *|
|*      Function    KIAND(I, J) intrinsic.                                    *|
|*      Function    AND(I, J) intrinsic.                                      *|
|*      Function    IEOR(I, J) intrinsic.                                     *|
|*      Function    IIEOR(I, J) intrinsic.                                    *|
|*      Function    JIEOR(I, J) intrinsic.                                    *|
|*      Function    KIEOR(I, J) intrinsic.                                    *|
|*      Function    NEQV(I, J) intrinsic.                                     *|
|*      Function    XOR(I, J) intrinsic.                                      *|
|*      Function    IOR(I, J) intrinsic.                                      *|
|*      Function    IIOR(I, J) intrinsic.                                     *|
|*      Function    JIOR(I, J) intrinsic.                                     *|
|*      Function    KIOR(I, J) intrinsic.                                     *|
|*      Function    OR(I, J) intrinsic.                                       *|
|*      Function    EQV(I, J) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    iand_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   opnd_type      opnd;
   int            cn_idx;
   int            cn_idx2;
   int            typeless_idx;
   int            minus_idx;
   int            column;
   int            info_idx1;
   int            info_idx2;
   int            line;
   int            list_idx1;
   int            list_idx2;
   long		  num;
   int            shiftl_idx;
   int            shiftr_idx;
   int            first_idx;
   int            second_idx;
   int            not_idx;
   int            ir_idx;
   boolean        ok = TRUE;
#ifdef KEY /* Bug 10177 */
   operator_type  opr = Null_Opr;
#else /* KEY Bug 10177 */
   operator_type  opr;
#endif /* KEY Bug 10177 */
   int            type_idx;


   TRACE (Func_Entry, "iand_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       (arg_info_list[info_idx1].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx1].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx1),
                                &line,
                                &column);

      if (arg_info_list[info_idx1].ed.type == Character) {
         PRINTMSG(line, 161, Ansi, column);
      }

      type_idx = arg_info_list[info_idx2].ed.type_idx;

      if (arg_info_list[info_idx2].ed.type == Character ||
          arg_info_list[info_idx2].ed.type == Typeless) {
         type_idx = INTEGER_DEFAULT_TYPE;
      }

      IL_IDX(list_idx1) = cast_typeless_constant(IL_IDX(list_idx1),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx1].ed.type_idx = type_idx;
      arg_info_list[info_idx1].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx1].ed.linear_type = TYP_LINEAR(type_idx);
   }

   if (IL_FLD(list_idx2) == CN_Tbl_Idx &&
       (arg_info_list[info_idx2].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx2].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx2),
                                &line,
                                &column);

      if (arg_info_list[info_idx2].ed.type == Character) {
         PRINTMSG(line, 161, Ansi, column);
      }

      type_idx = arg_info_list[info_idx1].ed.type_idx;

      if (arg_info_list[info_idx1].ed.type == Character ||
          arg_info_list[info_idx1].ed.type == Typeless) {
         type_idx = INTEGER_DEFAULT_TYPE;
      }

      IL_IDX(list_idx2) = cast_typeless_constant(IL_IDX(list_idx2),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx2].ed.type_idx = type_idx;
      arg_info_list[info_idx2].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx2].ed.linear_type = TYP_LINEAR(type_idx);
   }


   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   if (arg_info_list[info_idx1].ed.type == Integer) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
      arg_info_list[info_idx1].ed.linear_type;
   }
# endif

# ifdef _TARGET32
   if (arg_info_list[info_idx1].ed.linear_type == Integer_8 ||
       arg_info_list[info_idx1].ed.linear_type == Typeless_8 ||
       arg_info_list[info_idx1].ed.linear_type == Real_8) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
# endif
   }
# endif

# ifdef _TARGET_OS_MAX
   if (arg_info_list[info_idx1].ed.linear_type == Integer_1 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_2 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_4 ||
       arg_info_list[info_idx1].ed.linear_type == Typeless_4 ||
       arg_info_list[info_idx1].ed.linear_type == Real_4) {
       ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_4;
   }
# endif


   if (ATP_INTRIN_ENUM(*spec_idx) == Iand_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iiand_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Jiand_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Kiand_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Ior_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iior_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Jior_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Kior_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Ieor_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iieor_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Jieor_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Kieor_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
      arg_info_list[info_idx1].ed.type_idx;

      if (arg_info_list[info_idx1].ed.type == Typeless ||
          arg_info_list[info_idx2].ed.type == Typeless) {
         PRINTMSG(arg_info_list[info_idx1].line, 1076, Ansi,
                  arg_info_list[info_idx1].col);

         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
      }

# ifdef _TARGET32
      if (arg_info_list[info_idx1].ed.linear_type == Integer_8 ||
          arg_info_list[info_idx1].ed.linear_type == Typeless_8) {
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
      }
# endif

      if (arg_info_list[info_idx1].ed.linear_type !=
          arg_info_list[info_idx2].ed.linear_type) {
         PRINTMSG(arg_info_list[info_idx2].line, 774, Error,
                  arg_info_list[info_idx2].col);
         ok = FALSE;
      }
   }



   switch (ATP_INTRIN_ENUM(*spec_idx)) {
      case Iand_Intrinsic:
      case Iiand_Intrinsic:
      case Jiand_Intrinsic:
      case Kiand_Intrinsic:
           opr = Band_Opr;
           break;

      case Ior_Intrinsic:
      case Iior_Intrinsic:
      case Jior_Intrinsic:
      case Kior_Intrinsic:
           opr = Bor_Opr;
           break;

      case Ieor_Intrinsic:
      case Iieor_Intrinsic:
      case Jieor_Intrinsic:
      case Kieor_Intrinsic:
           opr = Bneqv_Opr;
           break;

      case And_Intrinsic:
           opr = Band_Opr;
           if (storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] !=
               storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type]) {
              PRINTMSG(IR_LINE_NUM(ir_idx), 1188, Error,
                       IR_COL_NUM(ir_idx));
              ok = FALSE;
           }
           else if (arg_info_list[info_idx1].ed.type == Logical &&
                    arg_info_list[info_idx2].ed.type == Logical) {
              ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
              opr = And_Opr;

           }
           break;

      case Or_Intrinsic:
           opr = Bor_Opr;
           if (storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] !=
               storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type]) {
              PRINTMSG(IR_LINE_NUM(ir_idx), 1188, Error,
                       IR_COL_NUM(ir_idx));
              ok = FALSE;
           }
           else if (arg_info_list[info_idx1].ed.type == Logical &&
                    arg_info_list[info_idx2].ed.type == Logical) {
              ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
              opr = Or_Opr;
           }
           break;

      case Xor_Intrinsic:
           opr = Bneqv_Opr;
           if (storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] !=
               storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type]) {
              PRINTMSG(IR_LINE_NUM(ir_idx), 1188, Error,
                       IR_COL_NUM(ir_idx));
              ok = FALSE;
           }
#ifdef KEY /* Bug 1683 */
#else
	   /* g77 compatibility requires an "xor" which operates bitwise
	    * regardless of data type. Seems stupid for Xor_Intrinsic and
	    * Neqv_Intrinsic to do the same thing, so make Xor_Intrinsic
	    * operate bitwise, and then the table in intrin.h can map
	    * each user-visible intrinsic name onto whichever operation
	    * we desire.  */
           else if (arg_info_list[info_idx1].ed.type == Logical &&
                    arg_info_list[info_idx2].ed.type == Logical) {
              ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
              opr = Neqv_Opr;
           }
#endif /* KEY Bug 1683 */
           break;

      case Neqv_Intrinsic:
           opr = Bneqv_Opr;
           if (storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] !=
               storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type]) {
              PRINTMSG(IR_LINE_NUM(ir_idx), 1188, Error,
                       IR_COL_NUM(ir_idx));
              ok = FALSE;
           }
           else if (arg_info_list[info_idx1].ed.type == Logical &&
                    arg_info_list[info_idx2].ed.type == Logical) {
              ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
              opr = Neqv_Opr;
           }
           break;

      case Eqv_Intrinsic:
           opr = Beqv_Opr;
           if (storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] !=
               storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type]) {
              PRINTMSG(IR_LINE_NUM(ir_idx), 1188, Error,
                       IR_COL_NUM(ir_idx));
              ok = FALSE;
           }
           else if (arg_info_list[info_idx1].ed.type == Logical &&
                    arg_info_list[info_idx2].ed.type == Logical) {
              ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
              opr = Eqv_Opr;
           }
           break;

      default:
         PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal, IR_COL_NUM(ir_idx),
                  "iand_intrinsic");
         break;
   }

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_8 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Typeless_8) {
      typeless_idx = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      typeless_idx = Integer_8;
# endif
   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      typeless_idx = INTEGER_DEFAULT_TYPE;
      if (arg_info_list[info_idx1].ed.type == Integer) {
         typeless_idx = arg_info_list[info_idx1].ed.linear_type;
      }
# endif
   }

# ifdef _TARGET_OS_MAX
   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_1 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_2 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Typeless_4 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif
   
   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (opr == And_Opr ||
       opr == Or_Opr ||
       opr == Eqv_Opr ||
       opr == Neqv_Opr) {
      IR_OPR(ir_idx) = opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }
   else {


   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   not_idx = gen_ir(IL_FLD(list_idx1), IL_IDX(list_idx1),
                 opr, typeless_idx, line, column,
                    IL_FLD(list_idx2), IL_IDX(list_idx2));

   num=storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
              num = BITSIZE_INT1_F90;
              break;

         case Integer_2:
              num = BITSIZE_INT2_F90;
              break;

         case Integer_4:
         case Typeless_4:
              num = BITSIZE_INT4_F90;
              break;

         case Integer_8:
         case Typeless_8:
              num = BITSIZE_INT8_F90;
              break;
   }

   cn_idx2 = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                 Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, column,
                      CN_Tbl_Idx, cn_idx2);


   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = not_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                  Shiftl_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                  Shiftr_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   if (TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer) {
      IR_OPR(shiftr_idx) = Shifta_Opr;
   }

   IR_OPR(ir_idx) = Cvrt_Opr;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_FLD_L(ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(ir_idx) = shiftr_idx;
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
   IR_FLD_R(ir_idx) = NO_Tbl_Idx;
   IR_IDX_R(ir_idx) = NULL_IDX;

   if (ok &&
       IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx) {
      COPY_OPND(opnd, (*result_opnd));
      ok = fold_aggragate_expression(&opnd, res_exp_desc, FALSE);
      COPY_OPND((*result_opnd), opnd);
   }

   }

   TRACE (Func_Exit, "iand_intrinsic", NULL);

}  /* iand_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MOD(A, P) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mod_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "mod_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (arg_info_list[info_idx1].ed.linear_type != 
       arg_info_list[info_idx2].ed.linear_type) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 774,  Error, 
               IR_COL_NUM(ir_idx));
   }

   if (arg_info_list[info_idx1].ed.type == Integer &&
       IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     2,
                     Mod_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Mod_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;

      if (arg_info_list[info_idx1].ed.type != Integer) {
         /* must reset foldable and will_fold_later because there is no */
         /* folder for this intrinsic in constructors.                  */

         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;
      }
   } 

   TRACE (Func_Exit, "mod_intrinsic", NULL);

}  /* mod_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  FREE(P) intrinsic.    	                              *|
|*      Subroutine  TIME(BUF) intrinsic.    	                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    free_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "free_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      IR_OPR(ir_idx) = Free_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "free_intrinsic", NULL);

}  /* free_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MALLOC(P) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    malloc_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "malloc_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CRI_Ptr_8;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      IR_OPR(ir_idx) = Malloc_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "malloc_intrinsic", NULL);

}  /* malloc_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    NULL(MOLD) intrinsic.                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    null_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            info_idx1;
   int            ir_idx;
   int            line;
   int            col;
   int            list_idx1;
   int            tmp_dv_idx;
   int            attr_idx;
   opnd_type      dv_opnd;


   TRACE (Func_Entry, "null_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);

   line = IR_LINE_NUM(ir_idx);
   col = IR_COL_NUM(ir_idx);

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (list_idx1 == NULL_IDX || IL_IDX(list_idx1) == NULL_IDX) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;
      ATD_POINTER(ATP_RSLT_IDX(*spec_idx)) = TRUE;
      IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      IR_RANK(ir_idx) = res_exp_desc->rank;
      res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
      res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
      res_exp_desc->pointer = TRUE;

      IR_OPR(ir_idx) = Null_Intrinsic_Opr;
      IR_OPND_R(ir_idx) = null_opnd;
      IR_LINE_NUM_R(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_R(ir_idx) = IR_COL_NUM(ir_idx);
      IR_OPND_R(ir_idx) = null_opnd;
      IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);

      res_exp_desc->foldable = FALSE;
      res_exp_desc->will_fold_later = FALSE;
   } 
   else {
      info_idx1 = IL_ARG_DESC_IDX(list_idx1);

      if (TYP_TYPE(arg_info_list[info_idx1].ed.type_idx) == Character) {
         COPY_OPND((res_exp_desc->char_len),
                   (arg_info_list[info_idx1].ed.char_len));
      }

      attr_idx = find_base_attr(&IL_OPND(list_idx1), &line, &col);

      if (IL_FLD(list_idx1) == CN_Tbl_Idx || !ATD_POINTER(attr_idx)) {
         PRINTMSG(arg_info_list[info_idx1].line, 1574, Error,
                  arg_info_list[info_idx1].col);
         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;
      } 

      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
                    arg_info_list[info_idx1].ed.type_idx;
      ATD_POINTER(ATP_RSLT_IDX(*spec_idx)) = TRUE;

      tmp_dv_idx = gen_compiler_tmp(line, col, Priv, TRUE);
      ATD_TYPE_IDX(tmp_dv_idx) = ATD_TYPE_IDX(attr_idx);
      ATD_STOR_BLK_IDX(tmp_dv_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
      ATD_ARRAY_IDX(tmp_dv_idx) = ATD_ARRAY_IDX(attr_idx);
      ATD_POINTER(tmp_dv_idx) = TRUE;
      ATD_IM_A_DOPE(tmp_dv_idx) = TRUE;

      gen_opnd(&dv_opnd, tmp_dv_idx, AT_Tbl_Idx, line, col);
      gen_dv_whole_def_init(&dv_opnd,
                            tmp_dv_idx,
                            Before);

      IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      IR_RANK(ir_idx) = res_exp_desc->rank;
      res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      res_exp_desc->type = 
              TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      res_exp_desc->linear_type = 
              TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      res_exp_desc->pointer = TRUE;
      res_exp_desc->tmp_reference = TRUE;

      gen_opnd(&dv_opnd, 
               gen_ir(AT_Tbl_Idx, 
                      tmp_dv_idx,
                      Dv_Deref_Opr, 
                      res_exp_desc->type_idx, 
                      line, 
                      col,
                      NO_Tbl_Idx, 
                      NULL_IDX),
               IR_Tbl_Idx, 
               line, 
               col);

      if (res_exp_desc->rank > 0) {
         gen_whole_subscript(&dv_opnd, res_exp_desc);
      }

      OPND_IDX((*result_opnd)) = OPND_IDX(dv_opnd);
      OPND_FLD((*result_opnd)) = OPND_FLD(dv_opnd);
   }

   TRACE (Func_Exit, "null_intrinsic", NULL);

}  /* null_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ANINT(A, KIND) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    anint_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            info_idx1;
   int            info_idx2;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;


   TRACE (Func_Entry, "anint_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
      arg_info_list[info_idx1].ed.type_idx;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   IR_OPR(ir_idx) = Anint_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
   IR_LIST_CNT_L(ir_idx) = 1;
   IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx)) = NULL_IDX;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "anint_intrinsic", NULL);

}  /* anint_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    NINT(A, KIND) intrinsic.                                  *|
|*      Function    ININT(A) intrinsic.                                       *|
|*      Function    JNINT(A) intrinsic.                                       *|
|*      Function    KNINT(A) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    nint_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            info_idx1;
   int            info_idx2;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int 		  type_idx;


   TRACE (Func_Entry, "nint_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }

   if (ATP_INTRIN_ENUM(*spec_idx) == Inint_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_2;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Jnint_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_4;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Knint_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
   }

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     Nint_Opr)) {

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Nint_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
      IR_LIST_CNT_L(ir_idx) = 1;
      IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx)) = NULL_IDX;
   }

   TRACE (Func_Exit, "nint_intrinsic", NULL);

}  /* nint_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SIGN(A, B) intrinsic.                                     *|
|*      Function    ISIGN(A, B) intrinsic.                                    *|
|*      Function    IISIGN(A, B) intrinsic.                                   *|
|*      Function    JISIGN(A, B) intrinsic.                                   *|
|*      Function    KISIGN(A, B) intrinsic.                                   *|
|*      Function    DSIGN(A, B) intrinsic.                                    *|
|*      Function    QSIGN(A, B) intrinsic.                                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    sign_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   int            ir_idx;
   int            type_idx;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];


   TRACE (Func_Entry, "sign_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;
   type_idx  = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (arg_info_list[info_idx1].ed.linear_type == Real_16) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
   }
   else {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }
# endif

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);
 
   if (arg_info_list[info_idx1].ed.linear_type != 
       arg_info_list[info_idx2].ed.linear_type) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 774,  Error, 
               IR_COL_NUM(ir_idx));
   }

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      if (arg_info_list[info_idx1].ed.type == Integer &&
          IL_FLD(list_idx1) == CN_Tbl_Idx &&
          IL_FLD(list_idx2) == CN_Tbl_Idx && 
          folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&CN_CONST(IL_IDX(list_idx2)),
                        arg_info_list[info_idx2].ed.type_idx,
                        folded_const,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        2,
                        Sign_Opr)) {

         OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
         OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                                  FALSE,
                                                  folded_const);
         OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
         res_exp_desc->constant = TRUE;
         res_exp_desc->foldable = TRUE;
      }
      else {
         IR_OPR(ir_idx) = Sign_Opr;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (on_off_flags.recognize_minus_zero &&
             arg_info_list[info_idx1].ed.type == Real) {
            IR_OPR(ir_idx) = Ieee_Copy_Sign_Opr;
         }
# endif
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
         IR_OPND_R(ir_idx) = null_opnd;

         if (arg_info_list[info_idx1].ed.type != Integer) {
            /* must reset foldable and will_fold_later because there is no */
            /* folder for this intrinsic in constructors.                  */

            res_exp_desc->foldable = FALSE;
            res_exp_desc->will_fold_later = FALSE;
         }
      }
   }
   else {
      /* must reset foldable and will_fold_later because there is no */
      /* folder for this intrinsic in constructors.                  */

      res_exp_desc->foldable = FALSE;
      res_exp_desc->will_fold_later = FALSE;
   }

   TRACE (Func_Exit, "sign_intrinsic", NULL);

}  /* sign_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MODULO(A, P) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    modulo_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            ir_idx;
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;
   int            type_idx;


   TRACE (Func_Entry, "modulo_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (arg_info_list[info_idx1].ed.linear_type !=
       arg_info_list[info_idx2].ed.linear_type) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 774, Error,
               IR_COL_NUM(ir_idx));
   }

   if (arg_info_list[info_idx1].ed.type == Integer &&
       IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     2,
                     Modulo_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Modulo_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;

      if (arg_info_list[info_idx1].ed.type != Integer) {
         /* must reset foldable and will_fold_later because there is no */
         /* folder for this intrinsic in constructors.                  */

         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;
      }
   }

   TRACE (Func_Exit, "modulo_intrinsic", NULL);

}  /* modulo_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SHIFT(I, J) intrinsic.                                    *|
|*      Function    SHIFTL(I, J) intrinsic.                                   *|
|*      Function    LSHIFT(I, POSITIVE_SHIFT) intrinsic.                      *|
|*      Function    SHIFTR(I, J) intrinsic.                                   *|
|*      Function    RSHIFT(I, NEGATIVE_SHIFT) intrinsic.                      *|
|*      Function    SHIFTA(I, J) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    shift_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            list_idx1;
   int            list_idx2;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */
   int            info_idx1;
   int            info_idx2;
   int            ir_idx;
#ifdef KEY /* Bug 10177 */
   operator_type  opr = Null_Opr;
#else /* KEY Bug 10177 */
   operator_type  opr;
#endif /* KEY Bug 10177 */
   int 		  type_idx;
   int 		  cn_idx;
   int 		  line;
   int 		  column;


   TRACE (Func_Entry, "shift_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       (arg_info_list[info_idx1].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx1].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx1),
                                &line,
                                &column);

      if (arg_info_list[info_idx1].ed.type == Character) {
         PRINTMSG(line, 161, Ansi, column);
      }

      type_idx = INTEGER_DEFAULT_TYPE;

      IL_IDX(list_idx1) = cast_typeless_constant(IL_IDX(list_idx1),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx1].ed.type_idx = type_idx;
      arg_info_list[info_idx1].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx1].ed.linear_type = TYP_LINEAR(type_idx);
   }


   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   if (arg_info_list[info_idx1].ed.type == Integer) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
                              arg_info_list[info_idx1].ed.linear_type;
   }
# endif


# ifdef _TARGET32
   if (arg_info_list[info_idx1].ed.linear_type == Integer_8 ||
       arg_info_list[info_idx1].ed.linear_type == Typeless_8 ||
       arg_info_list[info_idx1].ed.linear_type == Real_8) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
# endif
   }
# endif


# ifdef _TARGET_OS_MAX
   if (arg_info_list[info_idx1].ed.linear_type == Integer_1 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_2 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_4 ||
       arg_info_list[info_idx1].ed.linear_type == Typeless_4 ||
       arg_info_list[info_idx1].ed.linear_type == Real_4) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_4;
   }
# endif

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
      case Shift_Intrinsic:
           opr = Shift_Opr;
           break;

      case Shifta_Intrinsic:
           opr = Shifta_Opr;
           break;

      case Lshift_Intrinsic:
      case Shiftl_Intrinsic:
           opr = Shiftl_Opr;
           break;

      case Rshift_Intrinsic:
      case Shiftr_Intrinsic:
           opr = Shiftr_Opr;
           break;

      default:
           PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal,
                    IR_COL_NUM(ir_idx),
                    "shift_intrinsic");
         break;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
         case Typeless_1:
              num = BITSIZE_INT1_F90;
              break;

         case Integer_2:
         case Typeless_2:
              num = BITSIZE_INT2_F90;
              break;

         case Integer_4:
         case Typeless_4:
         case Real_4:
              num = BITSIZE_INT4_F90;
              break;

         case Integer_8:
         case Typeless_8:
         case Real_8:
              num = BITSIZE_INT8_F90;
              break;

         default:
              PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal,
                       IR_COL_NUM(ir_idx),
                       "shift_intrinsic");
         break;
   }

   if (IL_FLD(list_idx2) == CN_Tbl_Idx) {
      if (compare_cn_and_value(IL_IDX(list_idx2), num, Gt_Opr) ||
          compare_cn_and_value(IL_IDX(list_idx2), 0, Lt_Opr)) {
         PRINTMSG(arg_info_list[info_idx2].line, 1062, Error,
                  arg_info_list[info_idx2].col);
      }
   }

   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_TYPE_IDX(ir_idx) = type_idx;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx && 
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       arg_info_list[info_idx1].ed.type != Real) {

      if (opr == Shifta_Opr) {
         if (CN_INT_TO_C(IL_IDX(list_idx2)) == 8 &&
             (arg_info_list[info_idx1].ed.linear_type == Integer_1 ||
              (arg_info_list[info_idx1].ed.type == Typeless &&
               TYP_BIT_LEN(arg_info_list[info_idx1].ed.type_idx) == 8) ||
              arg_info_list[info_idx1].ed.linear_type == Typeless_1)) {

            cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, 7);
            IL_IDX(list_idx2) = cn_idx;
         }

         else if (CN_INT_TO_C(IL_IDX(list_idx2)) == 16 &&
                  (arg_info_list[info_idx1].ed.linear_type == Integer_2 ||
                   (arg_info_list[info_idx1].ed.type == Typeless &&
                    TYP_BIT_LEN(arg_info_list[info_idx1].ed.type_idx) == 16) ||
                   arg_info_list[info_idx1].ed.linear_type == Typeless_2)) {

            cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, 15);

            IL_IDX(list_idx2) = cn_idx;
         }

         else if (CN_INT_TO_C(IL_IDX(list_idx2)) == 32 &&
                  (arg_info_list[info_idx1].ed.linear_type == Integer_4 ||
                   (arg_info_list[info_idx1].ed.type == Typeless &&
                    TYP_BIT_LEN(arg_info_list[info_idx1].ed.type_idx) == 32) ||
                   arg_info_list[info_idx1].ed.linear_type == Typeless_4)) {

            cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, 31);

            IL_IDX(list_idx2) = cn_idx;
         }

         else if (CN_INT_TO_C(IL_IDX(list_idx2)) == 64 &&
                  (arg_info_list[info_idx1].ed.linear_type == Integer_8 ||
                   (arg_info_list[info_idx1].ed.type == Typeless &&
                    TYP_BIT_LEN(arg_info_list[info_idx1].ed.type_idx) == 64) ||
                   arg_info_list[info_idx1].ed.linear_type == Typeless_8)) {

            cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, 63);

            IL_IDX(list_idx2) = cn_idx;
         }
      }

      if (folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&CN_CONST(IL_IDX(list_idx2)),
                        arg_info_list[info_idx2].ed.type_idx,
                        folded_const,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        2,
                        opr)) {

         OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
         OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                                  FALSE,
                                                  folded_const);
         OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
         res_exp_desc->constant = TRUE;
         res_exp_desc->foldable = TRUE;
      }
   }
   else {       
      IR_OPR(ir_idx) = opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;

      if (arg_info_list[info_idx1].ed.type == Real) {
         /* must reset foldable and will_fold_later because there is no */
         /* folder for this intrinsic in constructors.                  */

         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;
      }
   }

   TRACE (Func_Exit, "shift_intrinsic", NULL);

}  /* shift_intrinsic */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    NUM_IMAGES() intrinsic.                                   *|
|*      Function    REM_IMAGES() intrinsic.                                   *|
|*      Function    LOG2_IMAGES() intrinsic.                                  *|
|*      Function    THIS_IMAGE([array[,dim]]) intrinsic.                      *|
|*      Subroutine  SYNC_IMAGES([image]) intrinsic.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    num_images_intrinsic(opnd_type     *result_opnd,
                             expr_arg_type *res_exp_desc,
                             int           *spec_idx)
{
   int            line;
   int            column;
   int            ir_idx;
   int            cn_idx;
   int            plus_idx;
   int            power_idx;
   int            div_idx;
   int            info_idx1;
   int            int_idx;
   int            mod_idx;
   int            list_idx1;
   int            list_idx2;
   opnd_type      opnd;
   int            opnd_line;
   int            opnd_col;
   int            l_log10_idx;
   int            r_log10_idx;
   float          point_five;
   float          f_two;
   int            sn_idx;
   int            attr_idx;
   expr_arg_type  loc_exp_desc;


   TRACE (Func_Entry, "num_images_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   if (ATP_INTRIN_ENUM(*spec_idx) != Sync_Images_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
      IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   }
   else {
      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   }

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (ATP_INTRIN_ENUM(*spec_idx) == Rem_Images_Intrinsic) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
      point_five = 0.5;

/* JEFFL - Do we need to convert endian? - BRIANJ */
/* We could call arith to do 1/2 and then we would have it correct for sure. */

/* JBL - this won't work when float is not the same as REAL_DEFAULT_TYPE - BHJ*/

      cn_idx = ntr_const_tbl(REAL_DEFAULT_TYPE, FALSE,(long_type *)&point_five);
      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
      copy_subtree(&opnd, &opnd);
      plus_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                  Plus_Opr, REAL_DEFAULT_TYPE, line, column,
                     CN_Tbl_Idx, cn_idx);

      f_two = 2.0;
      cn_idx = ntr_const_tbl(REAL_DEFAULT_TYPE, FALSE, (long_type *)&f_two);

      r_log10_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Log_10_Opr, REAL_DEFAULT_TYPE, line, column,
                     NO_Tbl_Idx, NULL_IDX);

      l_log10_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                  Log_10_Opr, REAL_DEFAULT_TYPE, line, column,
                     NO_Tbl_Idx, NULL_IDX);


      div_idx = gen_ir(IR_Tbl_Idx, l_log10_idx,
                  Div_Opr, REAL_DEFAULT_TYPE, line, column,
                     IR_Tbl_Idx, r_log10_idx);

      int_idx = gen_ir(IR_Tbl_Idx, div_idx,
                  Int_Opr, INTEGER_DEFAULT_TYPE, line, column,
                     NO_Tbl_Idx, NULL_IDX);

      cn_idx = CN_INTEGER_TWO_IDX;

      power_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Power_Opr, INTEGER_DEFAULT_TYPE, line, column,
                        IR_Tbl_Idx, int_idx);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
      copy_subtree(&opnd, &opnd);
      mod_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                  Mod_Opr, INTEGER_DEFAULT_TYPE, line, column,
                     IR_Tbl_Idx, power_idx);

      IR_IDX_L(ir_idx) = mod_idx;
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_OPND_R(ir_idx) = null_opnd;
      IR_OPR(ir_idx) = Int_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Log2_Images_Intrinsic) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
      point_five = 0.5;
      cn_idx = ntr_const_tbl(REAL_DEFAULT_TYPE, FALSE,(long_type *)&point_five);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
      copy_subtree(&opnd, &opnd);
      plus_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                  Plus_Opr, REAL_DEFAULT_TYPE, line, column,
                     CN_Tbl_Idx, cn_idx);

      f_two = 2.0;
      cn_idx = ntr_const_tbl(REAL_DEFAULT_TYPE, FALSE, (long_type *)&f_two);

      r_log10_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Log_10_Opr, REAL_DEFAULT_TYPE, line, column,
                     NO_Tbl_Idx, NULL_IDX);

      l_log10_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                  Log_10_Opr, REAL_DEFAULT_TYPE, line, column,
                     NO_Tbl_Idx, NULL_IDX);

      div_idx = gen_ir(IR_Tbl_Idx, l_log10_idx,
                  Div_Opr, REAL_DEFAULT_TYPE, line, column,
                     IR_Tbl_Idx, r_log10_idx);

      IR_IDX_L(ir_idx) = div_idx;
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_OPND_R(ir_idx) = null_opnd;
      IR_OPR(ir_idx) = Int_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == This_Image_Intrinsic) {

      if (IR_LIST_CNT_R(ir_idx) > 0) {

         list_idx1 = IR_IDX_R(ir_idx);
         info_idx1 = IL_ARG_DESC_IDX(list_idx1);

         if (IR_LIST_CNT_R(ir_idx) == 2) {
            list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
         }

         if (arg_info_list[info_idx1].ed.reference) {
            attr_idx = find_base_attr(&IL_OPND(list_idx1),
                                      &opnd_line, &opnd_col);

            if (AT_DCL_ERR(attr_idx)) {
               goto EXIT;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX &&
                IR_LIST_CNT_R(ir_idx) == 1 &&
                BD_RANK(ATD_PE_ARRAY_IDX(attr_idx)) == 1) {

               /* change to this_image3 with dim == 1 */

               sn_idx = ATI_FIRST_SPECIFIC_IDX(ATP_INTERFACE_IDX(*spec_idx));

               while (sn_idx) {
                  if (ATP_NUM_DARGS(SN_ATTR_IDX(sn_idx)) == 2) {
                     break;
                  }
                  sn_idx = SN_SIBLING_LINK(sn_idx);
               }

               if (sn_idx != NULL_IDX) {
                  IR_IDX_L(ir_idx) = SN_ATTR_IDX(sn_idx);
                  *spec_idx = SN_ATTR_IDX(sn_idx);
                  ATP_EXTERNAL_INTRIN((*spec_idx)) = TRUE;
                  ATD_TYPE_IDX(ATP_RSLT_IDX((*spec_idx))) =
                                           INTEGER_DEFAULT_TYPE;

                  NTR_IR_LIST_TBL(list_idx2);
                  IL_NEXT_LIST_IDX(list_idx1) = list_idx2;
                  IL_ARG_DESC_VARIANT(list_idx2) = TRUE;
                  IR_LIST_CNT_R(ir_idx) += 1;

                  IL_FLD(list_idx2) = CN_Tbl_Idx;
                  IL_IDX(list_idx2) = CN_INTEGER_ONE_IDX;
                  IL_LINE_NUM(list_idx2) = line;
                  IL_COL_NUM(list_idx2) = column;

                  arg_info_list_base = arg_info_list_top;
                  arg_info_list_top = arg_info_list_base + 1;

                  if (arg_info_list_top >= arg_info_list_size) {
                     enlarge_info_list_table();
                  }

                  IL_ARG_DESC_IDX(list_idx2) = arg_info_list_top;
                  arg_info_list[arg_info_list_top] = init_arg_info;
                  arg_info_list[arg_info_list_top].ed.constant = TRUE;
                  arg_info_list[arg_info_list_top].ed.foldable = TRUE;
                  arg_info_list[arg_info_list_top].ed.type     = Integer;
                  arg_info_list[arg_info_list_top].ed.type_idx =
                                                    CG_INTEGER_DEFAULT_TYPE;
                  arg_info_list[arg_info_list_top].ed.linear_type =
                                                    CG_INTEGER_DEFAULT_TYPE;
                  arg_info_list[arg_info_list_top].line = line;
                  arg_info_list[arg_info_list_top].col = column;
               }
            }
         }

         if (! arg_info_list[info_idx1].ed.reference) {
            /* error, not a co-array */
            find_opnd_line_and_column(&IL_OPND(list_idx1),
                                      &opnd_line, &opnd_col);
            PRINTMSG(opnd_line, 1575, Error, opnd_col);
         }
         else {
            attr_idx = find_base_attr(&IL_OPND(list_idx1),
                                      &opnd_line, &opnd_col);

            if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                ATD_PE_ARRAY_IDX(attr_idx) == NULL_IDX) {
               /* error, not a co-array */
               PRINTMSG(opnd_line, 1575, Error, opnd_col);
            }
            else {

               if (ATD_ALLOCATABLE(attr_idx)) {
                  attr_idx = ATD_VARIABLE_TMP_IDX(attr_idx);
               }

               COPY_OPND(opnd, IL_OPND(list_idx1));
               generate_bounds_list(ATD_PE_ARRAY_IDX(attr_idx),
                                    &opnd,
                                    &loc_exp_desc);
               COPY_OPND(IL_OPND(list_idx1), opnd);
               arg_info_list[info_idx1].ed = loc_exp_desc;

            }
         }
      }
   }

EXIT:

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "num_images_intrinsic", NULL);

}  /* num_images_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LEADZ(I) intrinsic.                                       *|
|*      Function    POPCNT(I) intrinsic.                                      *|
|*      Function    POPPAR(I) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    leadz_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   int            info_idx1;


   TRACE (Func_Entry, "leadz_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   if (storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] > 64) {
      PRINTMSG(arg_info_list[info_idx1].line, 774,  Error, 
               arg_info_list[info_idx1].col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (ATP_INTRIN_ENUM(*spec_idx) == Popcnt_Intrinsic) {
      IR_OPR(ir_idx) = Popcnt_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Poppar_Intrinsic) {
      IR_OPR(ir_idx) = Poppar_Opr;
   }
   else {
      IR_OPR(ir_idx) = Leadz_Opr;
   }

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "leadz_intrinsic", NULL);

}  /* leadz_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    NOT(I) intrinsic.                                         *|
|*      Function    INOT(I) intrinsic.                                        *|
|*      Function    JNOT(I) intrinsic.                                        *|
|*      Function    KNOT(I) intrinsic.                                        *|
|*      Function    COMPL(I) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    not_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   opnd_type      opnd;
   int            info_idx1;
   int            ir_idx;
   int            list_idx1;
   long		  num;
   operator_type  opr;
   int            first_idx;
   int            cn_idx;
   int            cn_idx2;
   int            typeless_idx;
   int            second_idx;
   int            minus_idx;
   int            type_idx;
   int            not_idx;
   int            shiftl_idx;
   int            shiftr_idx;
   int		  line;
   int		  column;


   TRACE (Func_Entry, "not_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if (arg_info_list[info_idx1].ed.type == Logical) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
      opr = Not_Opr;
   }
   else {
      if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
          (arg_info_list[info_idx1].ed.linear_type == 
                                               Short_Typeless_Const ||
           arg_info_list[info_idx1].ed.linear_type == 
                                               Short_Char_Const)) {
   
         find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx1),
                                   &line,
                                   &column);

         if (arg_info_list[info_idx1].ed.type == Character) {
            PRINTMSG(line, 161, Ansi, column);
         }

         type_idx = INTEGER_DEFAULT_TYPE;

         IL_IDX(list_idx1) = cast_typeless_constant(IL_IDX(list_idx1),
                                                    type_idx,
                                                    line,
                                                    column);

         arg_info_list[info_idx1].ed.type_idx = type_idx;
         arg_info_list[info_idx1].ed.type = TYP_TYPE(type_idx);
         arg_info_list[info_idx1].ed.linear_type = TYP_LINEAR(type_idx);
      }

      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
      arg_info_list[info_idx1].ed.type_idx;

      if (ATP_INTRIN_ENUM(*spec_idx) == Compl_Intrinsic) {
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
         if (arg_info_list[info_idx1].ed.type == Integer) {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) =
                              arg_info_list[info_idx1].ed.linear_type;
         }
# endif


# ifdef _TARGET32
         if ((arg_info_list[info_idx1].ed.linear_type == Integer_8) ||
             (arg_info_list[info_idx1].ed.linear_type == Typeless_8) ||
             (arg_info_list[info_idx1].ed.linear_type == Real_8)) { 
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
              ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
# endif
         }
# endif

# ifdef _TARGET_OS_MAX
         if (arg_info_list[info_idx1].ed.linear_type == Integer_1 ||
             arg_info_list[info_idx1].ed.linear_type == Integer_2 ||
             arg_info_list[info_idx1].ed.linear_type == Integer_4 ||
             arg_info_list[info_idx1].ed.linear_type == Typeless_4 ||
             arg_info_list[info_idx1].ed.linear_type == Real_4) {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_4;
         }
# endif
      }
      opr = Bnot_Opr;
   }

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_8 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Typeless_8) {
      typeless_idx = Typeless_8;
   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
   }

# ifdef _TARGET_OS_MAX
   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_1 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_2 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Typeless_4 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   res_exp_desc->linear_type = TYP_LINEAR(IR_TYPE_IDX(ir_idx));

   if (opr == Not_Opr) {
      IR_OPR(ir_idx) = opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }
   else {

      line = IR_LINE_NUM(ir_idx);
      column = IR_COL_NUM(ir_idx);

      not_idx = gen_ir(IL_FLD(list_idx1), IL_IDX(list_idx1),
                       opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);
      num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                            ATP_RSLT_IDX(*spec_idx)))];

      cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

      switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
              num = BITSIZE_INT1_F90;
              break;

         case Integer_2:
              num = BITSIZE_INT2_F90;
              break;

         case Integer_4:
         case Typeless_4:
              num = BITSIZE_INT4_F90;
              break;

         case Integer_8:
         case Typeless_8:
              num = BITSIZE_INT8_F90;
              break;
      }

      cn_idx2 = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

      minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                         Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, column,
                         CN_Tbl_Idx, cn_idx2);

      NTR_IR_LIST_TBL(first_idx);
      IL_FLD(first_idx) = IR_Tbl_Idx;
      IL_IDX(first_idx) = not_idx;
      NTR_IR_LIST_TBL(second_idx);
      IL_FLD(second_idx) = IR_Tbl_Idx;
      IL_IDX(second_idx) = minus_idx;
      IL_NEXT_LIST_IDX(first_idx) = second_idx;

      shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                          Shiftl_Opr, typeless_idx, line, column,
                          NO_Tbl_Idx, NULL_IDX);

      NTR_IR_LIST_TBL(first_idx);
      IL_FLD(first_idx) = IR_Tbl_Idx;
      IL_IDX(first_idx) = shiftl_idx;
      NTR_IR_LIST_TBL(second_idx);
      IL_FLD(second_idx) = IR_Tbl_Idx;
      IL_IDX(second_idx) = minus_idx;
      IL_NEXT_LIST_IDX(first_idx) = second_idx;

      shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                          Shiftr_Opr, typeless_idx, line, column,
                          NO_Tbl_Idx, NULL_IDX);

      if (TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer) {
         IR_OPR(shiftr_idx) = Shifta_Opr;
      }

      IR_OPR(ir_idx) = Cvrt_Opr;
      IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_IDX_L(ir_idx) = shiftr_idx;
      IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
      IR_OPND_R(ir_idx) = null_opnd;

      if (IL_FLD(list_idx1) == CN_Tbl_Idx) {
         COPY_OPND(opnd, (*result_opnd));
         fold_aggragate_expression(&opnd, res_exp_desc, FALSE);
         COPY_OPND((*result_opnd), opnd);
      }
   }

   TRACE (Func_Exit, "not_intrinsic", NULL);

}  /* not_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    AINT(A,KIND) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    aint_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;
   int            ir_idx;


   TRACE (Func_Entry, "aint_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) =
      arg_info_list[info_idx1].ed.type_idx;
   }

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   IR_OPR(ir_idx) = Aint_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
   IR_LIST_CNT_L(ir_idx) = 1;
   IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx)) = NULL_IDX;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "aint_intrinsic", NULL);

}  /* aint_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ILEN(I) intrinsic.                                        *|
|*      JBL - you must add folding of this intrinsic in fold_drive.c          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ilen_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            info_idx1;
   int            ir_idx;
   int            list_idx1;


   TRACE (Func_Entry, "ilen_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->linear_type = 
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   /* set this flag so this opr is pulled off io lists */
   io_item_must_flatten = TRUE;

   TRACE (Func_Exit, "ilen_intrinsic", NULL);

}  /* ilen_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DIM(X,Y) intrinsic.                                       *|
|*      Function    DDIM(X,Y) intrinsic.                                      *|
|*      Function    QDIM(X,Y) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    dim_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            info_idx1;
   int            info_idx2;
   int            arg1;
   int            arg2;
   int            arg3;
   int            ir_idx;
   int            gt_idx;
   int            type_idx;
   int            zero_idx;
   int            minus_idx;
   int            select_idx;
   int            list_idx1;
   int            list_idx2;
   int            line;
   int            column;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];


   TRACE (Func_Entry, "dim_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (arg_info_list[info_idx1].ed.linear_type !=
       arg_info_list[info_idx2].ed.linear_type) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 774,  Error, 
               IR_COL_NUM(ir_idx));
   }

   if (arg_info_list[info_idx1].ed.type == Integer &&
       IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     2,
                     Dim_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx1),
                                &line,
                                &column);

      gt_idx = gen_ir(IL_FLD(list_idx1), IL_IDX(list_idx1),
                  Gt_Opr, LOGICAL_DEFAULT_TYPE, line, column,
                      IL_FLD(list_idx2), IL_IDX(list_idx2));

      minus_idx = gen_ir(IL_FLD(list_idx1), IL_IDX(list_idx1),
                     Minus_Opr, arg_info_list[info_idx1].ed.type_idx, 
                         line, column,
                      IL_FLD(list_idx2), IL_IDX(list_idx2));

      zero_idx = (TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx) == 
                  CG_INTEGER_DEFAULT_TYPE) ? CN_INTEGER_ZERO_IDX :
                  C_INT_TO_CN(arg_info_list[info_idx1].ed.type_idx, 0);

      NTR_IR_LIST_TBL(arg1);
      IL_ARG_DESC_VARIANT(arg1) = TRUE;
      NTR_IR_LIST_TBL(arg2);
      IL_ARG_DESC_VARIANT(arg2) = TRUE;
      NTR_IR_LIST_TBL(arg3);
      IL_ARG_DESC_VARIANT(arg3) = TRUE;

      /* link list together */
      IL_NEXT_LIST_IDX(arg1) = arg2;
      IL_NEXT_LIST_IDX(arg2) = arg3;

      IL_IDX(arg1) = minus_idx;
      IL_FLD(arg1) = IR_Tbl_Idx;
      IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg1)  = IR_COL_NUM(ir_idx);
      IL_IDX(arg2) = zero_idx;
      IL_FLD(arg2) = CN_Tbl_Idx;
      IL_LINE_NUM(arg2) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg2)  = IR_COL_NUM(ir_idx);
      IL_IDX(arg3) = gt_idx;
      IL_FLD(arg3) = IR_Tbl_Idx;
      IL_LINE_NUM(arg3) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg3)  = IR_COL_NUM(ir_idx);

      select_idx = gen_ir(IL_Tbl_Idx, arg1,
                          Cvmgt_Opr, 
                          arg_info_list[info_idx1].ed.type_idx, 
                          IR_LINE_NUM(ir_idx), 
                          IR_COL_NUM(ir_idx),
                          NO_Tbl_Idx, NULL_IDX);

      /* set this flag so this opr is pulled off io lists */
      io_item_must_flatten = TRUE;
 
      IR_LIST_CNT_L(select_idx) = 3;

      IR_OPR(ir_idx) = Cvrt_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_IDX_L(ir_idx) = select_idx;
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_OPND_R(ir_idx) = null_opnd;
      }

      if (arg_info_list[info_idx1].ed.type != Integer) {
         /* must reset foldable and will_fold_later because there is no */
         /* folder for this intrinsic in constructors.                  */

         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;
      }
   }

   TRACE (Func_Exit, "dim_intrinsic", NULL);

}  /* dim_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MAX(A1, A2, ... A63) intrinsic.                           *|
|*      Function    MIN(A1, A2, ... A63) intrinsic.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    max_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            col		= 0; 
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   boolean        fold_it;
   boolean        casting_needed= FALSE;
   int            info_idx1;
   int            largest_linear_type;
   int            ir_idx;
   int            line		= 0;
   int            n_idx;
   operator_type  opr;
   opnd_type      opnd;
   int            t_idx;
   int            tmp_idx;
   int            type_idx;


   TRACE (Func_Entry, "max_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));

   conform_check(3, 
                 ir_idx,                 
                 res_exp_desc,
                 spec_idx,
                 FALSE);


   t_idx = IR_IDX_R(ir_idx);
   n_idx = IL_NEXT_LIST_IDX(t_idx);
#ifdef KEY /* Bug 14010 */
   int first_il_idx = t_idx;
#endif /* KEY Bug 14010 */

   largest_linear_type = arg_info_list[IL_ARG_DESC_IDX(t_idx)].ed.linear_type;

   fold_it = (IL_FLD(t_idx) == CN_Tbl_Idx);

   while ((n_idx != NULL_IDX) && (IL_IDX(n_idx) != NULL_IDX)) {
      if (arg_info_list[IL_ARG_DESC_IDX(t_idx)].ed.type !=
          arg_info_list[IL_ARG_DESC_IDX(n_idx)].ed.type) {
         PRINTMSG(IR_LINE_NUM(ir_idx), 774,  Error, 
                  IR_COL_NUM(ir_idx));
         fold_it = FALSE;
         break;
      }

      if ((opt_flags.set_fastint_option || 
           opt_flags.set_allfastint_option) &&
          (arg_info_list[IL_ARG_DESC_IDX(t_idx)].ed.type == Integer)) { 
         if (opt_flags.set_allfastint_option || 
             (TYP_DESC(arg_info_list[IL_ARG_DESC_IDX(t_idx)].ed.type_idx) == 
                               Default_Typed)) {
            casting_needed = TRUE;
         }

         if (opt_flags.set_allfastint_option || 
             (TYP_DESC(arg_info_list[IL_ARG_DESC_IDX(n_idx)].ed.type_idx) == 
                               Default_Typed)) {
            casting_needed = TRUE;
         }
      }

      if (arg_info_list[IL_ARG_DESC_IDX(t_idx)].ed.linear_type !=
          arg_info_list[IL_ARG_DESC_IDX(n_idx)].ed.linear_type) {
         PRINTMSG(arg_info_list[IL_ARG_DESC_IDX(n_idx)].line, 1323, Ansi, 
                  arg_info_list[IL_ARG_DESC_IDX(n_idx)].col);

         casting_needed = TRUE;
         if (largest_linear_type <
             arg_info_list[IL_ARG_DESC_IDX(n_idx)].ed.linear_type) {
            largest_linear_type = 
                   arg_info_list[IL_ARG_DESC_IDX(n_idx)].ed.linear_type;
         }
      }

      fold_it = fold_it && (IL_FLD(n_idx) == CN_Tbl_Idx);

      t_idx = n_idx;
      n_idx = IL_NEXT_LIST_IDX(n_idx);
   }

   if (casting_needed) {
      t_idx = IR_IDX_R(ir_idx);

      while ((t_idx != NULL_IDX) && (IL_IDX(t_idx) != NULL_IDX)) {
         COPY_OPND(opnd, IL_OPND(t_idx));
         cast_to_type_idx(&opnd,
                          &arg_info_list[IL_ARG_DESC_IDX(t_idx)].ed,
                          largest_linear_type);
         COPY_OPND(IL_OPND(t_idx), opnd);

         t_idx = IL_NEXT_LIST_IDX(t_idx);
      }
   }

#ifdef KEY /* Bug 14010 */
   /*
    * First and second args must be present, so if a later actual argument is
    * an optional dummy belonging to the caller, and it is not present, then
    * substitute the first arg.
    */
   int count = 0;
   for (t_idx = IR_IDX_R(ir_idx); t_idx != NULL_IDX;
     t_idx = IL_NEXT_LIST_IDX(t_idx)) {
     if (++count > 2 && NULL_IDX != is_optional_dummy(t_idx)) {
       pass_dummy_or_default(t_idx, IL_FLD(first_il_idx), IL_IDX(first_il_idx),
	 largest_linear_type, FALSE);
     }
   }
#endif /* KEY Bug 14010 */

   if ((ATP_INTRIN_ENUM(*spec_idx) == Amax0_Intrinsic) ||
       (ATP_INTRIN_ENUM(*spec_idx) == Amin0_Intrinsic)) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = REAL_DEFAULT_TYPE;
   }
   else if ((ATP_INTRIN_ENUM(*spec_idx) == Max1_Intrinsic) ||
            (ATP_INTRIN_ENUM(*spec_idx) == Min1_Intrinsic)) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = largest_linear_type;
   }

   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   type_idx = res_exp_desc->type_idx;

   if (ATP_INTRIN_ENUM(*spec_idx) == Max_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Amax0_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Amax1_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Dmax1_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Max0_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Max1_Intrinsic) {
      IR_OPR(ir_idx) = Lt_Opr;
      opr = Max_Opr;
   }
   else {
      IR_OPR(ir_idx) = Gt_Opr;
      opr = Min_Opr;
   }

   if (fold_it &&
       res_exp_desc->type == Integer &&
       arg_info_list[info_idx1].ed.type == Integer) {
      t_idx = IR_IDX_R(ir_idx);
      n_idx = IL_NEXT_LIST_IDX(t_idx);

      while ((n_idx != NULL_IDX) && (IL_IDX(n_idx) != NULL_IDX)) {
         fold_it = folder_driver((char *)&CN_CONST(IL_IDX(t_idx)),
                             arg_info_list[IL_ARG_DESC_IDX(t_idx)].ed.type_idx,
                             (char *)&CN_CONST(IL_IDX(n_idx)),
                             arg_info_list[IL_ARG_DESC_IDX(n_idx)].ed.type_idx,
                             folded_const,
                             &type_idx,
                             line,
                             col,
                             2,
                             IR_OPR(ir_idx));

         if (THIS_IS_TRUE(folded_const, type_idx)) {
            t_idx = n_idx;
         }


         OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
         OPND_IDX((*result_opnd)) = ntr_const_tbl(res_exp_desc->type_idx,
                                                  FALSE,
                                                  &CN_CONST(IL_IDX(t_idx)));
         OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
         res_exp_desc->constant = TRUE;
         res_exp_desc->foldable = TRUE;

         n_idx = IL_NEXT_LIST_IDX(n_idx);
      }
   }
   else {
      tmp_idx = gen_ir(IR_FLD_R(ir_idx), IR_IDX_R(ir_idx),
                   opr, IR_TYPE_IDX(ir_idx), IR_LINE_NUM(ir_idx), 
                                             IR_COL_NUM(ir_idx),
                       NO_Tbl_Idx, NULL_IDX);

      IR_OPR(ir_idx) = Cvrt_Opr;
      IR_IDX_L(ir_idx) = tmp_idx;
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
      IR_OPND_R(ir_idx) = null_opnd;

      if (res_exp_desc->type != Integer) {
         /* must reset foldable and will_fold_later because there is no */
         /* folder for this intrinsic in constructors.                  */

         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;
      }
   }

   TRACE (Func_Exit, "max_intrinsic", NULL);

}  /* max_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    RANGET(I) intrinsic.                                      *|
|*      Function    RANSET(I) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ranget_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int		  info_idx1;
   int            ir_idx;
   int            list_idx1;
   int            tmp_attr;
   int		  unused1	= NULL_IDX;
   int		  unused2	= NULL_IDX;
   opnd_type	  old_opnd;
   opnd_type	  base_opnd;


   TRACE (Func_Entry, "ranget_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
# endif

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (IL_IDX(list_idx1) == NULL_IDX) {  /* argument not present */
                                         /* insert one           */
      IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      IR_RANK(ir_idx) = res_exp_desc->rank;

      tmp_attr = gen_compiler_tmp(IR_LINE_NUM(ir_idx),
                                  IR_COL_NUM(ir_idx),
                                  Priv, TRUE);
      ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
      ATD_TYPE_IDX(tmp_attr) = INTEGER_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      ATD_TYPE_IDX(tmp_attr) = Integer_8;
# endif
      AT_SEMANTICS_DONE(tmp_attr) = TRUE;

      IL_FLD(list_idx1) = AT_Tbl_Idx;
      IL_IDX(list_idx1) = tmp_attr;
      IL_LINE_NUM(list_idx1) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx1) = IR_COL_NUM(ir_idx);
   }
   else {
      COPY_OPND(old_opnd, IL_OPND(list_idx1));

      if (! arg_info_list[info_idx1].ed.reference &&
          ! arg_info_list[info_idx1].ed.tmp_reference) {
     
         tmp_attr = create_tmp_asg(&old_opnd,
                      (expr_arg_type *)&(arg_info_list[info_idx1].ed),
                                   &base_opnd,
                                   Intent_In,
                                   TRUE,
                                   FALSE);

         COPY_OPND(old_opnd, base_opnd);
      }

      if (arg_info_list[info_idx1].ed.rank > 0) {
         make_base_subtree(&old_opnd, &base_opnd, &unused1, &unused2);
         COPY_OPND(IL_OPND(list_idx1), base_opnd);
      }
      else {
         COPY_OPND(IL_OPND(list_idx1), old_opnd);
      }
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   COPY_OPND(old_opnd, IL_OPND(list_idx1));
   cast_to_type_idx(&old_opnd, &arg_info_list[info_idx1].ed, Integer_8);
   COPY_OPND(IL_OPND(list_idx1), old_opnd);
# else
   COPY_OPND(old_opnd, IL_OPND(list_idx1));
   cast_to_cg_default(&old_opnd, &(arg_info_list[info_idx1].ed));
   COPY_OPND(IL_OPND(list_idx1), old_opnd);
# endif

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   if (ATP_INTRIN_ENUM(*spec_idx) == Ranget_Intrinsic) {
      IR_OPR(ir_idx) = Ranget_Opr;
   }
   else {
      IR_OPR(ir_idx) = Ranset_Opr;
   }
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "ranget_intrinsic", NULL);

}  /* ranget_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    RANF() intrinsic.                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ranf_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "ranf_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_8;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Ranf_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;
   tree_has_ranf = TRUE;

   TRACE (Func_Exit, "ranf_intrinsic", NULL);

}  /* ranf_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    REAL(A, KIND) intrinsic.                                  *|
|*      Function    FLOATI(A) intrinsic.                                      *|
|*      Function    FLOATJ(A) intrinsic.                                      *|
|*      Function    FLOATK(A) intrinsic.                                      *|
|*      Function    QFLOAT(A) intrinsic.                                      *|
|*      Function    QFLOATI(A) intrinsic.                                     *|
|*      Function    QFLOATJ(A) intrinsic.                                     *|
|*      Function    QFLOATK(A) intrinsic.                                     *|
|*      Function    QREAL(A) intrinsic.                                       *|
|*      Function    QEXT(A) intrinsic.                                        *|
|*      Function    SNGL(A) intrinsic.                                        *|
|*      Function    SNGLQ(A) intrinsic.                                       *|
|*      Function    DBLE(A) intrinsic.                                        *|
|*      Function    DBLEQ(A) intrinsic.                                       *|
|*      Function    DFLOAT(A) intrinsic.                                      *|
|*      Function    DFLOATI(A) intrinsic.                                     *|
|*      Function    DFLOATJ(A) intrinsic.                                     *|
|*      Function    DFLOATK(A) intrinsic.                                     *|
|*      Function    DREAL(A) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    real_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int			list_idx1;
   int			list_idx2;
   int			ir_idx;
   int			info_idx1;
   int			info_idx2;


   TRACE (Func_Entry, "real_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }
   else {
      switch (arg_info_list[info_idx1].ed.type) {
         case Integer:
         case Typeless:
         case Real:
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = REAL_DEFAULT_TYPE;
            break;

         case Complex:
            switch (arg_info_list[info_idx1].ed.linear_type) {
               case Complex_4:
                  ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_4;
                  break;
               case Complex_8:
                  ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_8;
                  break;
               case Complex_16:
                  ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_16;
                  break;
            }
            break;
      }
   }

   if (ATP_INTRIN_ENUM(*spec_idx) == Dfloat_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Dreal_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Dble_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Dbleq_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Dfloati_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Dfloatj_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Dfloatk_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = DOUBLE_DEFAULT_TYPE;
   }

   if (ATP_INTRIN_ENUM(*spec_idx) == Qfloat_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Qext_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Qreal_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Qfloati_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Qfloatj_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Qfloatk_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_16;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

#ifdef KEY /* Bug 12482 */
   if (arg_info_list[info_idx1].ed.linear_type == Short_Typeless_Const) {
     typeless_to_type(list_idx1, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
   }
#endif /* KEY Bug 12482 */

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Real_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
   IR_LIST_CNT_L(ir_idx) = 1;
   IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "real_intrinsic", NULL);

}  /* real_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MASK(I) intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mask_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int		  info_idx1;
   int            ir_idx;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int		  list_idx1;
   int            type_idx;


   TRACE (Func_Entry, "mask_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   if (arg_info_list[info_idx1].ed.type == Integer) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) =
                           arg_info_list[info_idx1].ed.linear_type;
   }
# endif

   IR_RANK(ir_idx) = res_exp_desc->rank;

# ifdef _TARGET32
   if (arg_info_list[info_idx1].ed.linear_type == Integer_8) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
# endif
   }
# endif

# ifdef _TARGET_OS_MAX
   if (arg_info_list[info_idx1].ed.linear_type == Integer_1 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_2 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_4) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_4;
   }
# endif

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     Mask_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Mask_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   TRACE (Func_Exit, "mask_intrinsic", NULL);

}  /* mask_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    CONJG(Z) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    conjg_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   int            info_idx1;


   TRACE (Func_Entry, "conjg_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Conjg_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "conjg_intrinsic", NULL);

}  /* conjg_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DPROD(X, Y) intrinsic.                                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    dprod_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   opnd_type      opnd;


   TRACE (Func_Entry, "dprod_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = DOUBLE_DEFAULT_TYPE;

   if (ATP_INTRIN_ENUM(*spec_idx) == Qprod_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_16;
   }

  if ((TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx) != REAL_DEFAULT_TYPE) ||
      (TYP_LINEAR(arg_info_list[info_idx2].ed.type_idx) != REAL_DEFAULT_TYPE)) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 361,  Error, 
               IR_COL_NUM(ir_idx));
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   COPY_OPND(opnd, IL_OPND(list_idx1));
   cast_to_type_idx(&opnd, 
                    &arg_info_list[info_idx1].ed, 
                    ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
   COPY_OPND(IL_OPND(list_idx1), opnd);

   COPY_OPND(opnd, IL_OPND(list_idx2));
   cast_to_type_idx(&opnd, 
                    &arg_info_list[info_idx2].ed, 
                    ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
   COPY_OPND(IL_OPND(list_idx2), opnd);


   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Dprod_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "dprod_intrinsic", NULL);

}  /* dprod_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LENGTH(I) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    length_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   opnd_type	  opnd;
# endif


   TRACE (Func_Entry, "length_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 4232 */
   /* See comment in erf_intrinsic() */
   if (!defining_stmt_func) {
#endif /* KEY Bug 4232 */
     COPY_OPND(opnd, IR_OPND_R(ir_idx));
     final_arg_work(&opnd, IR_IDX_L(ir_idx), IR_LIST_CNT_R(ir_idx), NULL);
     COPY_OPND(IR_OPND_R(ir_idx), opnd);
#ifdef KEY /* Bug 4232 */
   }
#endif /* KEY Bug 4232 */

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Length_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
# else 
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Length_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
# endif

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "length_intrinsic", NULL);

}  /* length_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    GETPOS(I) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    getpos_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "getpos_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Getpos_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "getpos_intrinsic", NULL);

}  /* getpos_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    UNIT(I) intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    unit_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   opnd_type	  opnd;
# endif


   TRACE (Func_Entry, "unit_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = REAL_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 4232 */
   /* See comment in erf_intrinsic() */
   if (!defining_stmt_func) {
#endif /* KEY Bug 4232 */
     COPY_OPND(opnd, IR_OPND_R(ir_idx));
     final_arg_work(&opnd, IR_IDX_L(ir_idx), IR_LIST_CNT_R(ir_idx), NULL);
     COPY_OPND(IR_OPND_R(ir_idx), opnd);
#ifdef KEY /* Bug 4232 */
   }
#endif /* KEY Bug 4232 */

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Unit_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
# else
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Unit_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
# endif

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "unit_intrinsic", NULL);

}  /* unit_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    CMPLX(X, Y, KIND) intrinsic.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    cmplx_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int		  column;
   int		  line;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            ir_idx;
   int            list_idx;
   operator_type  opr;
#ifdef KEY /* Bug 10177 */
   int            type_idx = 0;
#else /* KEY Bug 10177 */
   int            type_idx;
#endif /* KEY Bug 10177 */
   opnd_type	  opnd;


   TRACE (Func_Entry, "cmplx_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   opr = Cmplx_Opr;

   if ((list_idx3 != NULL_IDX) && (IL_IDX(list_idx3) != NULL_IDX)) {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);
      kind_to_linear_type(&((IL_OPND(list_idx3))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx3].ed.kind0seen,
                          arg_info_list[info_idx3].ed.kind0E0seen,
                          arg_info_list[info_idx3].ed.kind0D0seen,
                          ! arg_info_list[info_idx3].ed.kindnotconst);
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = COMPLEX_DEFAULT_TYPE;
   }

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Complex_4:
            type_idx = Real_4;
            break;

         case Complex_8:
            type_idx = Real_8;
            break;

         case Complex_16:
            type_idx = Real_16;
            break;
   }

   if ((ATP_INTRIN_ENUM(*spec_idx) == Dcmplx_Intrinsic)  &&
       (on_off_flags.enable_double_precision)) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = DOUBLE_COMPLEX_DEFAULT_TYPE;
   }

   if (ATP_INTRIN_ENUM(*spec_idx) == Qcmplx_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Complex_16;
   }

   conform_check(2, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

#ifdef KEY /* Bug 12482 */
   if (arg_info_list[info_idx1].ed.linear_type == Short_Typeless_Const) {
     typeless_to_type(list_idx1, type_idx);
   }
   if (list_idx2 != NULL_IDX &&
     arg_info_list[info_idx2].ed.linear_type == Short_Typeless_Const) {
     typeless_to_type(list_idx2, type_idx);
   }
#endif /* KEY Bug 12482 */

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (arg_info_list[info_idx1].ed.type == Integer) { 
      COPY_OPND(opnd, IL_OPND(list_idx1));
      cast_to_type_idx(&opnd, &arg_info_list[info_idx1].ed, type_idx);
      COPY_OPND(IL_OPND(list_idx1), opnd);
   }

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      if (arg_info_list[info_idx2].ed.type == Integer) { 
         COPY_OPND(opnd, IL_OPND(list_idx2));
         cast_to_type_idx(&opnd, &arg_info_list[info_idx2].ed, type_idx);
         COPY_OPND(IL_OPND(list_idx2), opnd);
      }

      if (arg_info_list[info_idx1].ed.type == Complex) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx2),
                                   &line,
                                   &column);
         PRINTMSG(line, 738, Error, column);
      }
   }
   else {  /* Y is not present */

      if (arg_info_list[info_idx1].ed.type == Complex) {  /* X is complex */
         opr = Cvrt_Opr;
      }
      else { /* X is not Complex */
         IL_FLD(list_idx2) = CN_Tbl_Idx;
         IL_IDX(list_idx2) = cvrt_str_to_cn("0.0",
                                            REAL_DEFAULT_TYPE);
         IL_LINE_NUM(list_idx2) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(list_idx2)  = IR_COL_NUM(ir_idx);
      }
   }

   IR_OPR(ir_idx) = opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   if (opr == Cvrt_Opr) {
      IR_LIST_CNT_L(ir_idx) = 1;
      list_idx = IR_IDX_L(ir_idx);
      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
   }
   else {
      IR_LIST_CNT_L(ir_idx) = 2;
      list_idx = IR_IDX_L(ir_idx);
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
   }


   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "cmplx_intrinsic", NULL);

}  /* cmplx_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LEN(STRING) intrinsic.                                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    len_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int		  unused_idx;
   int            ir_idx;
   int            line;
   int            col;


   TRACE (Func_Entry, "len_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   if (cmd_line_flags.runtime_substring &&
       IR_OPR(IL_IDX(IR_IDX_R(ir_idx))) == Substring_Opr) {
      gen_runtime_substring(IL_IDX(IR_IDX_R(ir_idx)));
   }

   res_exp_desc->rank = 0;

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Clen_Opr;
  
   unused_idx = find_base_attr(&IL_OPND(IR_IDX_R(ir_idx)), &line, &col);

   COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(IR_IDX_R(ir_idx)));
   IR_OPND_R(ir_idx) = null_opnd;

   fold_clen_opr(result_opnd, res_exp_desc);

   cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->linear_type = 
                      TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));

   /* must reset will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "len_intrinsic", NULL);

}  /* len_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ICHAR(C) intrinsic or IACHAR(C) intrinsic.                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ichar_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;
   int            list_idx1;
   long_type      cnst[MAX_WORDS_FOR_NUMERIC];
   int 		  type_idx;


   TRACE (Func_Entry, "ichar_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if ((OPND_FLD(arg_info_list[info_idx1].ed.char_len) == CN_Tbl_Idx) &&
       (CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx1].ed.char_len)) != 1)) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 327,  Ansi,
               IR_COL_NUM(ir_idx));
   }

   if (IL_FLD(list_idx1) == CN_Tbl_Idx && 
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     cnst,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     Ichar_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               cnst);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Ichar_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   TRACE (Func_Exit, "ichar_intrinsic", NULL);

}  /* ichar_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    CHAR(I, KIND) intrinsic or ACHAR(I) intrinsic.            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    char_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            list_idx1;
   int            list_idx2;
   long_type      cnst[MAX_WORDS_FOR_NUMERIC];
   int            ir_idx;
   int            info_idx1;
   int            info_idx2;
   int 		  type_idx;


   TRACE (Func_Entry, "char_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Character_1;
   }

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   res_exp_desc->char_len.fld = CN_Tbl_Idx;
   res_exp_desc->char_len.idx = CN_INTEGER_ONE_IDX;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx && 
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     cnst,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     Char_Opr)) {
      if (compare_cn_and_value(IL_IDX(list_idx1), 255, Gt_Opr) ||
          compare_cn_and_value(IL_IDX(list_idx1), 0, Lt_Opr)) {
         PRINTMSG(arg_info_list[info_idx1].line, 999,  Error, 
                  arg_info_list[info_idx1].col);
      }

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               cnst);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Char_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;

      IR_LIST_CNT_L(ir_idx) = 1;
      IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;

      /* set this flag so this opr is pulled off io lists */
      io_item_must_flatten = TRUE;
   }


   TRACE (Func_Exit, "char_intrinsic", NULL);

}  /* char_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    NEW_LINE(A) intrinsic                                     *|
|*                                                                            *|
|* Ignores the kind type of A and always returns CHARACTER_DEFAULT_TYPE.      *|
|* Ignores the OS and always returns '\n'                                     *|
|*                                                                            *|
\******************************************************************************/

void    newline_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            list_idx1;
   long_type      cnst[MAX_WORDS_FOR_NUMERIC];
   int            ir_idx;
   int 		  type_idx;


   TRACE (Func_Entry, "char_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CHARACTER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   res_exp_desc->char_len.fld = CN_Tbl_Idx;
   res_exp_desc->char_len.idx = CN_INTEGER_ONE_IDX;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   * (char *) cnst = '\n';
   OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx, FALSE, cnst);
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "newline_intrinsic", NULL);

}  /* newline_intrinsic */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    INDEX(STRING, SUBSTRING, BACK) intrinsic.                 *|
|*      Function    SCAN(STRING, SET, BACK) intrinsic.                        *|
|*      Function    VERIFY(STRING, SET, BACK) intrinsic.                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    index_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            cn_idx;
   long_type      cnst[MAX_WORDS_FOR_NUMERIC];
   int            ir_idx;
   int		  info_idx1;
   int		  info_idx2;
   int		  info_idx3;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int 		  type_idx;
   operator_type  opr;
   opnd_type	  opnd;


   TRACE (Func_Entry, "index_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(3, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (IL_IDX(list_idx3) == NULL_IDX) { /* if BACK is not present */
      cn_idx = set_up_logical_constant(cnst,
                                       CG_LOGICAL_DEFAULT_TYPE,
                                       FALSE_VALUE,
                                       TRUE);

      IL_FLD(list_idx3) = CN_Tbl_Idx;
      IL_IDX(list_idx3) = cn_idx;
      IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx3)  = IR_COL_NUM(ir_idx);

      arg_info_list_base = arg_info_list_top;
      arg_info_list_top = arg_info_list_base + 1;

      if (arg_info_list_top >= arg_info_list_size) {
         enlarge_info_list_table();
      }

      IL_ARG_DESC_IDX(list_idx3) = arg_info_list_top;
      arg_info_list[arg_info_list_top] = init_arg_info;
      arg_info_list[arg_info_list_top].ed.type_idx = CG_LOGICAL_DEFAULT_TYPE;
      arg_info_list[arg_info_list_top].ed.type = Logical;
      arg_info_list[arg_info_list_top].ed.linear_type= CG_LOGICAL_DEFAULT_TYPE;
      arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
      arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);
   }
#ifdef KEY /* Bug 10410 */
   else if (NULL_IDX != is_optional_dummy(list_idx3)) {
     pass_dummy_or_default_const(list_idx3,
       set_up_logical_constant(cnst, CG_LOGICAL_DEFAULT_TYPE, FALSE_VALUE,
         TRUE),
       FALSE);
   }
#endif /* KEY Bug 10410 */

   info_idx3 = IL_ARG_DESC_IDX(list_idx3);

   if (ATP_INTRIN_ENUM(*spec_idx) == Index_Intrinsic) {
      opr = Index_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Verify_Intrinsic) {
      opr = Verify_Opr;
   }
   else {
      opr = Scan_Opr;
# ifdef _TARGET_OS_MAX
      ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
# endif
   }

   if ((list_idx3 != NULL_IDX) && (IL_IDX(list_idx3) != NULL_IDX)) {
      COPY_OPND(opnd, IL_OPND(list_idx3));
      cast_to_cg_default(&opnd, &(arg_info_list[info_idx3].ed));
      COPY_OPND(IL_OPND(list_idx3), opnd);
   }

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       IL_FLD(list_idx3) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     3,
                     opr,
                     (char *)&CN_CONST(IL_IDX(list_idx3)),
                     (long)arg_info_list[info_idx3].ed.type_idx)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
         IR_OPR(ir_idx) = opr;
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
         IR_OPND_R(ir_idx) = null_opnd;
      }
   }

   TRACE (Func_Exit, "index_intrinsic", NULL);

}  /* index_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LGE(STRING_A, STRING_B) intrinsic.                        *|
|*      Function    LGT(STRING_A, STRING_B) intrinsic.                        *|
|*      Function    LLE(STRING_A, STRING_B) intrinsic.                        *|
|*      Function    LLT(STRING_A, STRING_B) intrinsic.                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    lge_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int		  info_idx1;
   int		  info_idx2;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int 		  type_idx;


   TRACE (Func_Entry, "lge_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (ATP_INTRIN_ENUM(*spec_idx) == Lge_Intrinsic) {
      IR_OPR(ir_idx) = Ge_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Llt_Intrinsic) {
      IR_OPR(ir_idx) = Lt_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Lle_Intrinsic) {
      IR_OPR(ir_idx) = Le_Opr;
   }
   else {
      IR_OPR(ir_idx) = Gt_Opr;
   }

   if (IL_FLD(list_idx1) == CN_Tbl_Idx && 
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     2,
                     IR_OPR(ir_idx))) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   TRACE (Func_Exit, "lge_intrinsic", NULL);

}  /* lge_intrinsic */


#ifdef KEY /* Bug 14150 */
/* This area deserves a cleanup some day, to make it mirror in logic and
 * naming the F2003 standard terminology regarding "interoperable type" vs
 * "interoperable variable" (the latter encompasses array elements and
 * substrings) vs interoperable entities. */

/*
 * info_idx	Index into arg_info_list for an actual arg
 * returns	0 if the arg has character type and we can statically tell that
 *		its len is not 1
 */
static int
check_interoperable_char(int info_idx) {
  return arg_info_list[info_idx].ed.type != Character ||
    OPND_FLD(arg_info_list[info_idx].ed.char_len) != CN_Tbl_Idx ||
    CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx].ed.char_len)) == 1;
  }

/*
 * Check argument of ISO_C_BINDING function c_loc
 *
 * attr_idx		AT_Tbl_Idx for base attribute of this arg of c_loc
 * info_idx		Index into arg_info_list for this argument
 * return		error message number, or 0 for no error
 */
static int
c_loc_iso_arg_check(int attr_idx, int info_idx) {
  int found_error = 0;
  if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
    found_error = 700;
  }
  else {
    int allocatable = arg_info_list[info_idx].ed.allocatable;
    int pointer = arg_info_list[info_idx].ed.pointer;
    int target = arg_info_list[info_idx].ed.target;
    int rank = arg_info_list[info_idx].ed.rank;
    found_error = (target || pointer) ? 1692 : 418;
    /* F2003 15.1.2.5 (1) */
    if ((target && interoperable_variable(attr_idx) &&
       check_interoperable_char(info_idx)) || /* (1a) */
      (allocatable && target &&
       check_interoperable_type(attr_idx, TRUE, FALSE) &&
       check_interoperable_char(info_idx)) || /* (1b) */
      (rank == 0 && pointer &&
       check_interoperable_type(attr_idx, TRUE, FALSE) &&
       check_interoperable_char(info_idx))) { /* (1c) */
      found_error = 0;
    }
    /* F2003 15.1.2.5 (2) */
    if (found_error && rank == 0 && no_length_type_param(attr_idx)) {
      if (((!allocatable) && (!pointer) && target) || /* (a) */
	(allocatable && target) || /* (b) */
	pointer) { /* (c) */
	found_error = 0;
      }
    }
  }
  return found_error;
}
#endif /* KEY Bug 14150 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LOC(I) intrinsic.                                         *|
|*      Function    CLOC(C) intrinsic.                                        *|
|*      Function    C_LOC(X) intrinsic (traditional Cray).                    *|
|*      Function    C_LOC(X) intrinsic (iso_c_binding).		              *|
|*      Function    C_FUNLOC(X) intrinsic.                                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    loc_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   opnd_type	  base_opnd;
   int            ir_idx;
   int            attr_idx;
   int            info_idx1;
   int            list_idx1;
   opnd_type	  old_opnd;
   int		  unused1	= NULL_IDX;
   int		  unused2	= NULL_IDX;


   TRACE (Func_Entry, "loc_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
#ifdef KEY /* Bug 14150 */
   intrinsic_type which = ATP_INTRIN_ENUM(*spec_idx);
   if (which == C_Loc_Iso_Intrinsic || which == C_Funloc_Intrinsic) {
     /* Type is already set correctly */
   }
   else
#endif /* KEY Bug 14150 */
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CRI_Ptr_8;

   if (ATP_INTRIN_ENUM(*spec_idx) == Cloc_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CRI_Ch_Ptr_8;
   }

   if (ATP_INTRIN_ENUM(*spec_idx) == C_Loc_Intrinsic &&
       arg_info_list[info_idx1].ed.type == Character) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CRI_Ch_Ptr_8;
   }

   if ((strcmp(AT_OBJ_NAME_PTR(*spec_idx), "LOC@") == 0) &&
       arg_info_list[info_idx1].ed.type == Character) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CRI_Ch_Ptr_8;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;


# ifdef _TARGET_OS_MAX
   if (arg_info_list[info_idx1].ed.linear_type == Integer_4 ||
       arg_info_list[info_idx1].ed.linear_type == Real_4 ||
       arg_info_list[info_idx1].ed.linear_type == Logical_4) {

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX) = CRI_Ptr;
      TYP_LINEAR(TYP_WORK_IDX) = CRI_Ptr_8;
      TYP_PTR_INCREMENT(TYP_WORK_IDX) = 32;
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = ntr_type_tbl();
      IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   }
# endif


   res_exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   res_exp_desc->type = TYP_TYPE(IR_TYPE_IDX(ir_idx));
   res_exp_desc->linear_type = TYP_LINEAR(IR_TYPE_IDX(ir_idx));

   if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
       (IL_FLD(list_idx1) == IR_Tbl_Idx &&
        (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
         IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr ||
         IR_OPR(IL_IDX(list_idx1)) == Struct_Opr ||
         IR_OPR(IL_IDX(list_idx1)) == Dv_Deref_Opr ||
         IR_OPR(IL_IDX(list_idx1)) == Subscript_Opr ||
         IR_OPR(IL_IDX(list_idx1)) == Substring_Opr ||
         IR_OPR(IL_IDX(list_idx1)) == Section_Subscript_Opr))) {
      attr_idx = find_base_attr(&IL_OPND(list_idx1), &unused1, &unused2);

#ifdef KEY /* Bug 14150 */
      intrinsic_type which_intrinsic = ATP_INTRIN_ENUM(*spec_idx);
      if (which_intrinsic == C_Loc_Iso_Intrinsic ||
	which_intrinsic == C_Funloc_Intrinsic) {
	int found_error = (which_intrinsic == C_Loc_Iso_Intrinsic) ?
	  c_loc_iso_arg_check(attr_idx, info_idx1) :
	  (AT_BIND_ATTR(attr_idx) ? 0 : 1692);
	if (found_error) {
	  PRINTMSG(arg_info_list[info_idx1].line, found_error, Error,
	    arg_info_list[info_idx1].col, AT_OBJ_NAME_PTR(*spec_idx));
	}
	/* For now, call external procedure because giving Loc_Opr a result
	 * type of type(c_ptr) or type(c_funptr) blows up elsewhere in the
	 * front end. Sigh. See also table entry in p_driver.c */
	goto EXIT;
      }
#endif /* KEY Bug 14150 */

      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
         PRINTMSG(arg_info_list[info_idx1].line, 779, Error,
                  arg_info_list[info_idx1].col, AT_OBJ_NAME_PTR(attr_idx));
         goto EXIT;
      }


      if ((AT_OBJ_CLASS(attr_idx) == Data_Obj) && ATD_AUXILIARY(attr_idx)) {
         PRINTMSG(arg_info_list[info_idx1].line, 990,  Error, 
                  arg_info_list[info_idx1].col);
         goto EXIT;
      }
   }
   else {
      PRINTMSG(arg_info_list[info_idx1].line, 779,  Error, 
               arg_info_list[info_idx1].col);
      goto EXIT;
   }

   IR_OPR(ir_idx) = Loc_Opr;

   COPY_OPND(old_opnd, IL_OPND(IR_IDX_R(ir_idx)));

   unused1 = 0;
   unused2 = 0;

   make_base_subtree(&old_opnd, &base_opnd, &unused1, &unused2);

   COPY_OPND(IR_OPND_L(ir_idx), base_opnd);

   IR_OPND_R(ir_idx) = null_opnd;

EXIT:

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "loc_intrinsic", NULL);

}  /* loc_intrinsic */
#ifdef KEY /* Bug 14150 */

/*
 * For c_f_pointer(), return false if number of elements in "shape" argument
 * doesn't match rank of "fptr" argument
 * shape	shape[0] of argument "shape"
 * rank2	rank of argument "fptr"
 */
static boolean
compare_length(opnd_type shape, int rank2) {
  if (OPND_FLD(shape) != CN_Tbl_Idx) {
    return TRUE; /* Can't check */
  }
  return compare_cn_and_value(OPND_IDX(shape), rank2, Eq_Opr);
}
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    C_F_POINTER(CPTR, FPTR [, SHAPE]) intrinsic.              *|
|*      Function    C_F_PROCPOINTER(CPTR, FPTR [, SHAPE]) intrinsic.          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    c_f_pointer_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            attr_idx = NULL_IDX;
   int		  unused1	= NULL_IDX;
   int		  unused2	= NULL_IDX;

   TRACE (Func_Entry, "c_f_pointer_intrinsic", NULL);

   int ir_idx = OPND_IDX((*result_opnd));
   boolean has_shape_arg = IR_LIST_CNT_R(ir_idx) == 3;
   int list_idx1 = IR_IDX_R(ir_idx);
   int list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   int list_idx3 =  has_shape_arg ? IL_NEXT_LIST_IDX(list_idx2) : 0;
   int info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   int info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   int info_idx3 = has_shape_arg ? IL_ARG_DESC_IDX(list_idx3) : 0;
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CRI_Ptr_8;
   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   res_exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   res_exp_desc->type = TYP_TYPE(IR_TYPE_IDX(ir_idx));
   res_exp_desc->linear_type = TYP_LINEAR(IR_TYPE_IDX(ir_idx));

   if (IL_FLD(list_idx2) == AT_Tbl_Idx ||
       (IL_FLD(list_idx2) == IR_Tbl_Idx &&
        (IR_OPR(IL_IDX(list_idx2)) == Whole_Subscript_Opr ||
         IR_OPR(IL_IDX(list_idx2)) == Whole_Substring_Opr ||
         IR_OPR(IL_IDX(list_idx2)) == Struct_Opr ||
         IR_OPR(IL_IDX(list_idx2)) == Dv_Deref_Opr ||
         IR_OPR(IL_IDX(list_idx2)) == Subscript_Opr ||
         IR_OPR(IL_IDX(list_idx2)) == Substring_Opr ||
         IR_OPR(IL_IDX(list_idx2)) == Section_Subscript_Opr))) {
      attr_idx = find_base_attr(&IL_OPND(list_idx2), &unused1, &unused2);

      if (ATP_INTRIN_ENUM(*spec_idx) == C_F_Pointer_Intrinsic &&
	(AT_OBJ_CLASS(attr_idx) != Data_Obj || !ATD_POINTER(attr_idx))) {
	PRINTMSG(arg_info_list[info_idx2].line, 700,  Error, 
		 arg_info_list[info_idx2].col, AT_OBJ_NAME_PTR(*spec_idx));
      }
      else if (ATP_INTRIN_ENUM(*spec_idx) == C_F_Procpointer_Intrinsic) {
	/* This will need more work once we have procedure pointer vars */
      }
   }

   int shape_error = FALSE;
   int rank2 = arg_info_list[info_idx2].ed.rank;
   /* If fptr is array, "shape" argument must be present with number of
    * elements matching rank of fptr */
   if (rank2) {
     if ((!has_shape_arg) || list_idx3 == NULL_IDX ||
       IL_IDX(list_idx3) == NULL_IDX) {
       shape_error = TRUE;
     }
     else if (arg_info_list[info_idx3].ed.assumed_size) {
       /* Assume ok because no way to know length of assumed-size array */
     }
     else if (arg_info_list[info_idx3].ed.rank != 1 ||
         !compare_length(arg_info_list[info_idx3].ed.shape[0], rank2)) {
       shape_error = TRUE;
     }
   } else {
     shape_error = has_shape_arg ||
       (list_idx3 != NULL_IDX && IL_IDX(list_idx3) != NULL_IDX);
   }
   if (shape_error) {
     PRINTMSG(arg_info_list[info_idx2].line, 1698, Error,
       arg_info_list[info_idx2].col);
   }

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "c_f_pointer_intrinsic", NULL);

}  /* c_f_pointer_intrinsic */
#endif /* KEY Bug 14150 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    FCD(I, J) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    fcd_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "fcd_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = CRI_Ch_Ptr_8;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Fcd_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "fcd_intrinsic", NULL);

}  /* fcd_intrinsic */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    FETCH_AND_ADD(I, J) intrinsic.                            *|
|*      Function    FETCH_AND_AND(I, J) intrinsic.                            *|
|*      Function    FETCH_AND_NAND(I, J) intrinsic.                           *|
|*      Function    FETCH_AND_OR(I, J) intrinsic.                             *|
|*      Function    FETCH_AND_SUB(I, J) intrinsic.                            *|
|*      Function    FETCH_AND_XOR(I, J) intrinsic.                            *|
|*      Function    ADD_AND_FETCH(I, J) intrinsic.                            *|
|*      Function    AND_AND_FETCH(I, J) intrinsic.                            *|
|*      Function    NAND_AND_FETCH(I, J) intrinsic.                           *|
|*      Function    OR_AND_FETCH(I, J) intrinsic.                             *|
|*      Function    SUB_AND_FETCH(I, J) intrinsic.                            *|
|*      Function    XOR_AND_FETCH(I, J) intrinsic.                            *|
|*      Function    LOCK_TEST_AND_SET(I, J) intrinsic.                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    fetch_and_add_intrinsic(opnd_type     *result_opnd,
                                expr_arg_type *res_exp_desc,
                                int           *spec_idx) 
{
   int            ir_idx;
   int            list_idx1;
   int            info_idx1;


   TRACE (Func_Entry, "fetch_and_add_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   io_item_must_flatten = TRUE;

   if (ATP_INTRIN_ENUM(*spec_idx) == Fetch_And_Add_Intrinsic) {
      IR_OPR(ir_idx) = Fetch_And_Add_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Fetch_And_And_Intrinsic) {
      IR_OPR(ir_idx) = Fetch_And_And_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Fetch_And_Nand_Intrinsic) {
      IR_OPR(ir_idx) = Fetch_And_Nand_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Fetch_And_Or_Intrinsic) {
      IR_OPR(ir_idx) = Fetch_And_Or_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Fetch_And_Xor_Intrinsic) {
      IR_OPR(ir_idx) = Fetch_And_Xor_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Fetch_And_Sub_Intrinsic) {
      IR_OPR(ir_idx) = Fetch_And_Sub_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Add_And_Fetch_Intrinsic) {
      IR_OPR(ir_idx) = Add_And_Fetch_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == And_And_Fetch_Intrinsic) {
      IR_OPR(ir_idx) = And_And_Fetch_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Nand_And_Fetch_Intrinsic) {
      IR_OPR(ir_idx) = Nand_And_Fetch_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Or_And_Fetch_Intrinsic) {
      IR_OPR(ir_idx) = Or_And_Fetch_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Sub_And_Fetch_Intrinsic) {
      IR_OPR(ir_idx) = Sub_And_Fetch_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Xor_And_Fetch_Intrinsic) {
      IR_OPR(ir_idx) = Xor_And_Fetch_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Lock_Test_And_Set_Intrinsic) {
      IR_OPR(ir_idx) = Lock_Test_And_Set_Opr;
   }

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "fetch_and_add_intrinsic", NULL);

}  /* fetch_and_add_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    NUMARG() intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    numarg_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "numarg_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Numarg_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "numarg_intrinsic", NULL);

}  /* numarg_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    READ@SM() intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    readsm_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;

   TRACE (Func_Entry, "readsm_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Readsm_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "readsm_intrinsic", NULL);

}  /* readsm_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  MEMORY_BARRIER() intrinsic.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    memory_barrier_intrinsic(opnd_type     *result_opnd,
                                 expr_arg_type *res_exp_desc,
                                 int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "memory_barrier_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Memory_Barrier_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "memory_barrier_intrinsic", NULL);

}  /* memory_barrier_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  REMOTE_WRITE_BARRIER() intrinsic.                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    remote_write_barrier_intrinsic(opnd_type     *result_opnd,
                                       expr_arg_type *res_exp_desc,
                                       int           *spec_idx) 
{
   int            ir_idx;


   TRACE (Func_Entry, "remote_write_barrier_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Remote_Write_Barrier_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "remote_write_barrier_intrinsic", NULL);

}  /* remote_write_barrier_intrinsic */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  WRITE_MEMORY_BARRIER() intrinsic.                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    write_memory_barrier_intrinsic(opnd_type     *result_opnd,
                                       expr_arg_type *res_exp_desc,
                                       int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "write_memory_barrier_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Write_Memory_Barrier_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "write_memory_barrier_intrinsic", NULL);

}  /* write_memory_barrier_intrinsic */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  SYNCHRONIZE() intrinsic.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    synchronize_intrinsic(opnd_type     *result_opnd,
                              expr_arg_type *res_exp_desc,
                              int           *spec_idx) 
{
   int            ir_idx;


   TRACE (Func_Entry, "synchronize_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Synchronize_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   io_item_must_flatten = TRUE;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "synchronize_intrinsic", NULL);

}  /* synchronize_intrinsic */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    RTC() intrinsic.                                          *|
|*      Function    IRTC() intrinsic.                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    rtc_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "rtc_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   if (ATP_INTRIN_ENUM(*spec_idx) == Irtc_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_8;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Rtc_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "rtc_intrinsic", NULL);

}  /* rtc_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MY_PE() intrinsic (MPP Only).                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    my_pe_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "my_pe_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = My_Pe_Opr;

   IR_OPND_L(ir_idx) = null_opnd;
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   /* Set this flag so this opr is pulled off io lists.  This is   */
   /* needed because pdgcs feels compelled to treat fei_new_my_pe  */
   /* as a data object which it can take the address of.  Problem  */
   /* is, this is not a data object.                               */

   io_item_must_flatten = TRUE;

   TRACE (Func_Exit, "my_pe_intrinsic", NULL);

}  /* my_pe_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    CVMGP(I, J, K) intrinsic.                                 *|
|*      Function    CVMGM(I, J, K) intrinsic.                                 *|
|*      Function    CVMGZ(I, J, K) intrinsic.                                 *|
|*      Function    CVMGN(I, J, K) intrinsic.                                 *|
|*      Function    CVMGT(I, J, K) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    cvmgp_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int		  column;
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            ir_idx;
   int		  line;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            new_idx;
#ifdef KEY /* Bug 10177 */
   operator_type  opr1 = Null_Opr;
#else /* KEY Bug 10177 */
   operator_type  opr1;
#endif /* KEY Bug 10177 */
   int            type_idx;


   TRACE (Func_Entry, "cvmgp_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   info_idx3 = IL_ARG_DESC_IDX(list_idx3);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       (arg_info_list[info_idx1].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx1].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx1),
                                &line,
                                &column);

      type_idx = arg_info_list[info_idx2].ed.type_idx;

      if (arg_info_list[info_idx2].ed.type == Character ||
          arg_info_list[info_idx2].ed.type == Typeless) {
         type_idx = INTEGER_DEFAULT_TYPE;
      }

      IL_IDX(list_idx1) = cast_typeless_constant(IL_IDX(list_idx1),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx1].ed.type_idx = type_idx;
      arg_info_list[info_idx1].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx1].ed.linear_type = TYP_LINEAR(type_idx);
   }


   if (IL_FLD(list_idx2) == CN_Tbl_Idx &&
       (arg_info_list[info_idx2].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx2].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx2),
                                &line,
                                &column);

      type_idx = arg_info_list[info_idx1].ed.type_idx;

      if (arg_info_list[info_idx1].ed.type == Character ||
          arg_info_list[info_idx1].ed.type == Typeless) {
         type_idx = INTEGER_DEFAULT_TYPE;
      }

      IL_IDX(list_idx2) = cast_typeless_constant(IL_IDX(list_idx2),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx2].ed.type_idx = type_idx;
      arg_info_list[info_idx2].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx2].ed.linear_type = TYP_LINEAR(type_idx);
   }

   if (IL_FLD(list_idx3) == CN_Tbl_Idx &&
       (arg_info_list[info_idx3].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx3].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx3),
                                &line,
                                &column);

      type_idx = INTEGER_DEFAULT_TYPE;

      IL_IDX(list_idx3) = cast_typeless_constant(IL_IDX(list_idx3),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx3].ed.type_idx = type_idx;
      arg_info_list[info_idx3].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx3].ed.linear_type = TYP_LINEAR(type_idx);
   }



   if (arg_info_list[info_idx1].ed.type == Logical) {
      type_idx = LOGICAL_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (arg_info_list[info_idx1].ed.type == Logical) {
         type_idx = arg_info_list[info_idx1].ed.linear_type;
      }
# endif
   }
   else {
      type_idx = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      type_idx = INTEGER_DEFAULT_TYPE;
      if (arg_info_list[info_idx1].ed.type == Integer) {
         type_idx = arg_info_list[info_idx1].ed.linear_type;
      }
# endif


# ifdef _TARGET32
      if ((arg_info_list[info_idx1].ed.linear_type == Integer_8) ||
          (arg_info_list[info_idx1].ed.linear_type == Typeless_8) ||
          (arg_info_list[info_idx1].ed.linear_type == Real_8) ||
          (arg_info_list[info_idx2].ed.linear_type == Integer_8) ||
          (arg_info_list[info_idx2].ed.linear_type == Typeless_8) ||
          (arg_info_list[info_idx2].ed.linear_type == Real_8)) { 
         type_idx = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         type_idx = Integer_8;
# endif
      }

      if (arg_info_list[info_idx1].ed.type == Real &&
          arg_info_list[info_idx2].ed.type == Real) {
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         type_idx = arg_info_list[info_idx1].ed.linear_type;
# endif
      }

# endif
   }

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
      case Cvmgp_Intrinsic:
           opr1 = Ge_Opr;
           break;

      case Cvmgm_Intrinsic:
           opr1 = Lt_Opr;
           break;

      case Cvmgz_Intrinsic:
           opr1 = Eq_Opr;
           break;

      case Cvmgn_Intrinsic:
           opr1 = Ne_Opr;
           break;
   }

   if (ATP_INTRIN_ENUM(*spec_idx) != Cvmgt_Intrinsic) {

      new_idx = gen_ir(IL_FLD(list_idx3), IL_IDX(list_idx3),
                   opr1, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                               IR_COL_NUM(ir_idx),
                       CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

      IL_FLD(list_idx3) = IR_Tbl_Idx;
      IL_IDX(list_idx3) = new_idx;
   }

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Cvmgt_Opr;

   /* set this flag so this opr is pulled off io lists */
   io_item_must_flatten = TRUE;

   if (storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] !=
       storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type]) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 1188, Error,
               IR_COL_NUM(ir_idx));
   }

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   TRACE (Func_Exit, "cvmgp_intrinsic", NULL);

}  /* cvmgp_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    COMPARE_AND_SWAP(I, J, K) intrinsic.                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    compare_and_swap_intrinsic(opnd_type     *result_opnd,
                                   expr_arg_type *res_exp_desc,
                                   int           *spec_idx) 
{
   int            ir_idx;


   TRACE (Func_Entry, "compare_and_swap_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   conform_check(0, ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   IR_OPR(ir_idx) = Compare_And_Swap_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   io_item_must_flatten = TRUE;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "compare_and_swap_intrinsic", NULL);

}  /* compare_and_swap_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    CSMG(I, J, K) intrinsic.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    csmg_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            ir_idx;
   int            line;
   int            column;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            type_idx;


   TRACE (Func_Entry, "csmg_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   info_idx3 = IL_ARG_DESC_IDX(list_idx3);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       (arg_info_list[info_idx1].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx1].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx1),
                                &line,
                                &column);

      type_idx = arg_info_list[info_idx2].ed.type_idx;

      if (arg_info_list[info_idx2].ed.type == Character ||
          arg_info_list[info_idx2].ed.type == Typeless) {
         type_idx = INTEGER_DEFAULT_TYPE;
      }

      IL_IDX(list_idx1) = cast_typeless_constant(IL_IDX(list_idx1),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx1].ed.type_idx = type_idx;
      arg_info_list[info_idx1].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx1].ed.linear_type = TYP_LINEAR(type_idx);
   }

   if (IL_FLD(list_idx2) == CN_Tbl_Idx &&
       (arg_info_list[info_idx2].ed.linear_type == Short_Typeless_Const ||
        arg_info_list[info_idx2].ed.linear_type == Short_Char_Const)) {

      find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx2),
                                &line,
                                &column);

      type_idx = arg_info_list[info_idx1].ed.type_idx;

      if (arg_info_list[info_idx1].ed.type == Character ||
          arg_info_list[info_idx1].ed.type == Typeless) {
         type_idx = INTEGER_DEFAULT_TYPE;
      }

      IL_IDX(list_idx2) = cast_typeless_constant(IL_IDX(list_idx2),
                                                 type_idx,
                                                 line,
                                                 column);

      arg_info_list[info_idx2].ed.type_idx = type_idx;
      arg_info_list[info_idx2].ed.type = TYP_TYPE(type_idx);
      arg_info_list[info_idx2].ed.linear_type = TYP_LINEAR(type_idx);
   }



   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   if (arg_info_list[info_idx1].ed.type == Integer) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) =
                              arg_info_list[info_idx1].ed.linear_type;
   }
# endif


# ifdef _TARGET32
   if ((arg_info_list[info_idx1].ed.linear_type == Integer_8) ||
       (arg_info_list[info_idx1].ed.linear_type == Typeless_8) ||
       (arg_info_list[info_idx1].ed.linear_type == Real_8) ||
       (arg_info_list[info_idx2].ed.linear_type == Integer_8) ||
       (arg_info_list[info_idx2].ed.linear_type == Typeless_8) ||
       (arg_info_list[info_idx2].ed.linear_type == Real_8) ||
       (arg_info_list[info_idx3].ed.linear_type == Integer_8) ||
       (arg_info_list[info_idx3].ed.linear_type == Typeless_8) ||
       (arg_info_list[info_idx3].ed.linear_type == Real_8)) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
# endif
   }
# endif

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       IL_FLD(list_idx3) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     3,
                     Csmg_Opr,
                     (char *)&CN_CONST(IL_IDX(list_idx3)),
                     (long)arg_info_list[info_idx3].ed.type_idx)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Csmg_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   TRACE (Func_Exit, "csmg_intrinsic", NULL);

}  /* csmg_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MERGE(TSOURCE, FSOURCE, MASK) intrinsic.                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mergee_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   int            ir_idx;
   int            type_idx;
   int            type_idx2;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];


   TRACE (Func_Entry, "mergee_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   type_idx = arg_info_list[info_idx1].ed.type_idx;
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   if (arg_info_list[info_idx1].ed.linear_type != 
       arg_info_list[info_idx2].ed.linear_type) {
     
      if (arg_info_list[info_idx1].ed.type == Character &&
          arg_info_list[info_idx2].ed.type == Character) {
         /* intentionally blank */
      }
      else {
         PRINTMSG(arg_info_list[info_idx2].line, 774, Error,
                  arg_info_list[info_idx2].col);
      }
   }

   type_idx2 = CG_LOGICAL_DEFAULT_TYPE;
   if (arg_info_list[info_idx1].ed.type == Character &&
       arg_info_list[info_idx2].ed.type == Character &&
       arg_info_list[info_idx2].ed.char_len.fld == CN_Tbl_Idx &&
       arg_info_list[info_idx1].ed.char_len.fld == CN_Tbl_Idx &&
       folder_driver(
              (char *)&CN_CONST(arg_info_list[info_idx2].ed.char_len.idx),
              arg_info_list[info_idx2].ed.type_idx,
              (char *)&CN_CONST(arg_info_list[info_idx1].ed.char_len.idx),
              arg_info_list[info_idx1].ed.type_idx,
              folded_const,
              &type_idx2,
              IR_LINE_NUM(ir_idx),
              IR_COL_NUM(ir_idx),
              2,
              Ne_Opr)) {

      if (THIS_IS_TRUE(folded_const, type_idx2)) {
         PRINTMSG(arg_info_list[info_idx2].line, 774, Error,
                  arg_info_list[info_idx2].col);
      }
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (TYP_TYPE(type_idx) == Character) {
      COPY_OPND((res_exp_desc->char_len),
                (arg_info_list[info_idx1].ed.char_len));
   }

   IR_OPR(ir_idx) = Cvmgt_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* set this flag so this opr is pulled off io lists */
   io_item_must_flatten = TRUE;

   TRACE (Func_Exit, "mergee_intrinsic", NULL);

}  /* mergee_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ADJUSTL(STRING) intrinsic.                                *|
|*      Function    ADJUSTR(STRING) intrinsic.                                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    adjustl_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   expr_arg_type  exp_desc;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int            info_idx1;
   int            ir_idx;
   opnd_type      l_opnd;
   int		  list_idx1;
   int            new_idx;
   boolean	  ok;
   operator_type  opr;
   opnd_type      opnd;
   opnd_type      opnd2;
   int            unused;
   int            type_idx;


   TRACE (Func_Entry, "adjustl_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   type_idx  = arg_info_list[info_idx1].ed.type_idx;
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   COPY_OPND(res_exp_desc->char_len, arg_info_list[info_idx1].ed.char_len);
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);

   if (ATP_INTRIN_ENUM(*spec_idx) == Adjustl_Intrinsic) {
      opr = Adjustl_Opr;
   }
   else {
      opr = Adjustr_Opr;
   }

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = folded_const[0];
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
      IR_TYPE_IDX(ir_idx) = type_idx;
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      io_item_must_flatten = TRUE;
      COPY_OPND(opnd2, IR_OPND_R(ir_idx));
      ok = final_arg_work(&opnd2,
                          IR_IDX_L(ir_idx),
                          IR_LIST_CNT_R(ir_idx),
                          NULL);
      COPY_OPND(IR_OPND_R(ir_idx), opnd2);

      new_idx = gen_ir(IR_FLD_R(ir_idx), IR_IDX_R(ir_idx),
                  opr, res_exp_desc->type_idx,
                   IR_LINE_NUM(ir_idx), IR_COL_NUM(ir_idx),
                       NO_Tbl_Idx, NULL_IDX);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = new_idx;

      if (IL_FLD(list_idx1) == IR_Tbl_Idx &&
          IR_OPR(IL_IDX(list_idx1)) == Aloc_Opr) {
         COPY_OPND(IL_OPND(list_idx1), IR_OPND_L(IL_IDX(list_idx1)));
      }

      if (IL_FLD(list_idx1) == AT_Tbl_Idx &&
          AT_OBJ_CLASS(IL_IDX(list_idx1)) == Data_Obj &&
          ATD_ARRAY_IDX(IL_IDX(list_idx1)) != NULL_IDX) {
         COPY_OPND(opnd2, IL_OPND(list_idx1));
         ok = gen_whole_subscript(&opnd2, &exp_desc);
         COPY_OPND(IL_OPND(list_idx1), opnd2);
      }

      unused = create_tmp_asg(&opnd,
                               res_exp_desc,
                               &l_opnd,
                               Intent_In,
                               TRUE,
                               FALSE);

      COPY_OPND((*result_opnd), l_opnd);

      /* must reset foldable and will_fold_later because there is no */
      /* folder for this intrinsic in constructors.                  */

      res_exp_desc->foldable = FALSE;
      res_exp_desc->will_fold_later = FALSE;
   }

   TRACE (Func_Exit, "adjustl_intrinsic", NULL);

}  /* adjustl_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    CEILING(A) intrinsic.                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ceiling_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   int		  info_idx2;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;


   TRACE (Func_Entry, "ceiling_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Ceiling_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
   IR_LIST_CNT_L(ir_idx) = 1;
   IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "ceiling_intrinsic", NULL);

}  /* ceiling_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DIGITS(X) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    digits_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            cn_idx;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */
   int            info_idx1;
   int            ir_idx;


   TRACE (Func_Entry, "digits_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           num = DIGITS_REAL4_F90;
           break;

      case Real_8:
           num = DIGITS_REAL8_F90;
           break;

      case Real_16:
           num = DIGITS_REAL16_F90;
           break;

      case Integer_1:
           num = DIGITS_INT1_F90;
           break;

      case Integer_2:
           num = DIGITS_INT2_F90;
           break;

      case Integer_4:
           num = DIGITS_INT4_F90;
           break;

      case Integer_8:
           num = DIGITS_INT8_F90;

# ifdef _TARGET_HAS_FAST_INTEGER
           if (opt_flags.set_allfastint_option ||
               (opt_flags.set_fastint_option &&
                (TYP_DESC(arg_info_list[info_idx1].ed.type_idx) ==
                                                           Default_Typed))) {
              num = 46;
           }
# endif

           break;
   }

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "digits_intrinsic", NULL);

}  /* digits_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    EPSILON(X) intrinsic.                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    epsilon_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
#ifdef KEY /* Bug 10177 */
   int            cn_idx = 0;
#else /* KEY Bug 10177 */
   int            cn_idx;
#endif /* KEY Bug 10177 */
   int            info_idx1;
   int            ir_idx;


   TRACE (Func_Entry, "epsilon_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           cn_idx = cvrt_str_to_cn(EPSILON_REAL4_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Real_8:
           cn_idx = cvrt_str_to_cn(EPSILON_REAL8_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Real_16:
           cn_idx = cvrt_str_to_cn(EPSILON_REAL16_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;
   }


   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "epsilon_intrinsic", NULL);

}  /* epsilon_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    EXPONENT(X) intrinsic.                                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    exponent_intrinsic(opnd_type     *result_opnd,
                           expr_arg_type *res_exp_desc,
                           int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "exponent_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Exponent_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "exponent_intrinsic", NULL);

}  /* exponent_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    FLOOR(A) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    floor_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int		  info_idx2;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;


   TRACE (Func_Entry, "floor_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Floor_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
   IR_LIST_CNT_L(ir_idx) = 1;
   IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "floor_intrinsic", NULL);

}  /* floor_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    FRACTION(X) intrinsic.                                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    fraction_intrinsic(opnd_type     *result_opnd,
                           expr_arg_type *res_exp_desc,
                           int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;

   TRACE (Func_Entry, "fraction_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Fraction_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "fraction_intrinsic", NULL);

}  /* fraction_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    HUGE(X) intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    huge_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
#ifdef KEY /* Bug 10177 */
   int            cn_idx = 0;
#else /* KEY Bug 10177 */
   int            cn_idx;
#endif /* KEY Bug 10177 */
   int            info_idx1;
   int            ir_idx;


   TRACE (Func_Entry, "huge_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           cn_idx = cvrt_str_to_cn(HUGE_REAL4_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Real_8:
           cn_idx = cvrt_str_to_cn(HUGE_REAL8_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Real_16:
           cn_idx = cvrt_str_to_cn(HUGE_REAL16_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Integer_1:
           cn_idx = cvrt_str_to_cn(HUGE_INT1_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Integer_2:
           cn_idx = cvrt_str_to_cn(HUGE_INT2_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Integer_4:
           cn_idx = cvrt_str_to_cn(HUGE_INT4_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Integer_8:
           cn_idx = cvrt_str_to_cn(HUGE_INT8_F90,
                                   arg_info_list[info_idx1].ed.linear_type);

# ifdef _TARGET_HAS_FAST_INTEGER
           if (opt_flags.set_allfastint_option || 
               (opt_flags.set_fastint_option && 
                (TYP_DESC(arg_info_list[info_idx1].ed.type_idx) == 
                                                              Default_Typed))) {
              cn_idx = C_INT_TO_CN(IR_TYPE_IDX(ir_idx), 70368744177663L);
           }
# endif
           break;
   }


   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "huge_intrinsic", NULL);

}  /* huge_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    IBITS(I, POS, LEN) intrinsic.                             *|
|*      Function    IIBITS(I, POS, LEN) intrinsic.                            *|
|*      Function    JIBITS(I, POS, LEN) intrinsic.                            *|
|*      Function    KIBITS(I, POS, LEN) intrinsic.                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ibits_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   boolean        fold_it = FALSE;
   int            ir_idx;
   int            info_idx1;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   opnd_type	  opnd;
   int            typeless_idx;

# if !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int            cn_idx;
   int            cn_idx2;
   long		  num;
   int            shiftl_idx;
   int            shiftr_idx;
   int            shifta_idx;
   int            first_idx;
   int            second_idx;
   int            mask_idx;
   int            band_idx;
   int            minus_idx;
   int		  line;
   int		  column;
# endif


   TRACE (Func_Entry, "ibits_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   if (arg_info_list[info_idx1].ed.type == Typeless) {
      PRINTMSG(arg_info_list[info_idx1].line, 1076, Ansi, 
               arg_info_list[info_idx1].col);

      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_8) {
      typeless_idx = Typeless_8;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      typeless_idx = Integer_8;
# endif

   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      typeless_idx = INTEGER_DEFAULT_TYPE;
      if (arg_info_list[info_idx1].ed.type == Integer) {
         typeless_idx = arg_info_list[info_idx1].ed.linear_type;
      }
# endif

   }

# ifdef _TARGET_OS_MAX
   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_1 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_2 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       IL_FLD(list_idx3) == CN_Tbl_Idx) {
      fold_it = TRUE;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   IR_OPR(ir_idx) = Ibits_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

# else

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                         ATP_RSLT_IDX(*spec_idx)))] * 2; 

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
        Minus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), line, column,
                IL_FLD(list_idx3), IL_IDX(list_idx3));

   mask_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                Mask_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX); 

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = mask_idx;
   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx2));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                  Shiftl_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   COPY_OPND(opnd, IL_OPND(list_idx1));
   cast_opnd_to_type_idx(&opnd, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
   COPY_OPND(IL_OPND(list_idx1), opnd);

   band_idx = gen_ir(IR_Tbl_Idx, shiftl_idx,
                Band_Opr, typeless_idx, line, column,
                     IL_FLD(list_idx1), IL_IDX(list_idx1));

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = band_idx;
   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx2));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   
   shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                  Shiftr_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   num =storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
              num = BITSIZE_INT1_F90;
              break;

         case Integer_2:
              num = BITSIZE_INT2_F90;
              break;

         case Integer_4:
              num = BITSIZE_INT4_F90;
              break;

         case Integer_8:
              num = BITSIZE_INT8_F90;
              break;
   }

   cn_idx2 = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                 Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, column,
                      CN_Tbl_Idx, cn_idx2);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftr_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shiftl_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shifta_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shifta_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   IR_OPR(ir_idx) = Cvrt_Opr;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_FLD_L(ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(ir_idx) = shifta_idx;
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
   IR_OPND_R(ir_idx) = null_opnd;

# endif

   if (fold_it) {
      COPY_OPND(opnd, (*result_opnd));
      fold_aggragate_expression(&opnd, res_exp_desc, FALSE);
      COPY_OPND((*result_opnd), opnd);
   }

   TRACE (Func_Exit, "ibits_intrinsic", NULL);

}  /* ibits_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    BTEST(I, POS) intrinsic.                                  *|
|*      Function    BITEST(I, POS) intrinsic.                                 *|
|*      Function    BJTEST(I, POS) intrinsic.                                 *|
|*      Function    BKTEST(I, POS) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    btest_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            minus_idx;
   int            shiftl_idx;
   int            typeless_idx;
   int            first_idx;
   int            second_idx;
   int            shiftr_idx;
   int            info_idx1;
   int            list_idx1;
   int            list_idx2;
   int		  type_idx;
   int            line;
   int            column;
   long		  num;


   TRACE (Func_Entry, "btest_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;

   if (ATP_INTRIN_ENUM(*spec_idx) == Bitest_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Logical_2;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Bjtest_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Logical_4;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Bktest_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Logical_8;
   }

   if (arg_info_list[info_idx1].ed.linear_type == Integer_8) {
      typeless_idx = Typeless_8;
   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
   }

# ifdef _TARGET_OS_MAX
   if (arg_info_list[info_idx1].ed.linear_type == Integer_1 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_2 ||
       arg_info_list[info_idx1].ed.linear_type == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   type_idx = INTEGER_DEFAULT_TYPE;

# ifdef _TARGET32
   if (arg_info_list[info_idx1].ed.linear_type == Integer_8) {
      type_idx = Integer_8;
   }
# endif

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   num = storage_bit_size_tbl[TYP_LINEAR(typeless_idx)] - 1;

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   line = IR_LINE_NUM(ir_idx);    
   column = IR_COL_NUM(ir_idx);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                 Minus_Opr, type_idx, line, column,
                      IL_FLD(list_idx2), IL_IDX(list_idx2));

   NTR_IR_LIST_TBL(first_idx);
   COPY_OPND(IL_OPND(first_idx), IL_OPND(list_idx1));
   NTR_IR_LIST_TBL(second_idx);
   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                  Shiftl_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   IL_FLD(second_idx) = CN_Tbl_Idx;
   IL_IDX(second_idx) = cn_idx;
   IL_LINE_NUM(second_idx) = IR_LINE_NUM(ir_idx);
   IL_COL_NUM(second_idx)  = IR_COL_NUM(ir_idx);

   shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                  Shifta_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   if (target_ieee) {
      IR_OPR(shiftr_idx) = Shiftr_Opr;
   }


   IR_OPR(ir_idx) = Cvrt_Opr;
   IR_FLD_L(ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(ir_idx) = shiftr_idx;
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "btest_intrinsic", NULL);

}  /* btest_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    IBSET(I, POS) intrinsic.                                  *|
|*      Function    IIBSET(I, POS) intrinsic.                                 *|
|*      Function    JIBSET(I, POS) intrinsic.                                 *|
|*      Function    KIBSET(I, POS) intrinsic.                                 *|
|*      Function    IBCLR(I, POS) intrinsic.                                  *|
|*      Function    IIBCLR(I, POS) intrinsic.                                 *|
|*      Function    JIBCLR(I, POS) intrinsic.                                 *|
|*      Function    KIBCLR(I, POS) intrinsic.                                 *|
|*      Function    IBCHNG(I, POS) intrinsic.                                 *|
|*      Function    IIBCHNG(I, POS) intrinsic.                                *|
|*      Function    JIBCHNG(I, POS) intrinsic.                                *|
|*      Function    KIBCHNG(I, POS) intrinsic.                                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    ibset_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            cn_idx2;
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;
#ifdef KEY /* Bug 10177 */
   long		  num1 = 0;
   long		  num2 = 0;
#else /* KEY Bug 10177 */
   long		  num1;
   long		  num2;
#endif /* KEY Bug 10177 */
   int            shiftl_idx;
   int            shifta_idx;
   int            csmg_idx;
   int            minus_idx;
   int            first_idx;
   int            second_idx;
   int            third_idx;
   int            bor_idx;
   int            band_idx;
   int            bnot_idx;
   int            bnot_idx1;
#ifdef KEY /* Bug 10177 */
   int            typeless_idx = 0;
#else /* KEY Bug 10177 */
   int            typeless_idx;
#endif /* KEY Bug 10177 */
   opnd_type	  opnd;
   boolean        fold_it 		= FALSE;
   int            line;
   int            column;


   TRACE (Func_Entry, "ibset_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   if (arg_info_list[info_idx1].ed.type == Typeless) {
      PRINTMSG(arg_info_list[info_idx1].line, 1076, Ansi, 
               arg_info_list[info_idx1].col);
   }
   
   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Typeless_1:
      case Integer_1:
           ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_1;
           typeless_idx = Typeless_1;
# ifdef _TARGET_OS_MAX
           typeless_idx = Typeless_4;
# endif
# ifdef _TARGET_OS_UNICOS
           typeless_idx = Typeless_8;
# endif
           num1 = BITSIZE_INT1_F90 - 1;
           num2 = BITSIZE_INT1_F90;
           break;

      case Typeless_2:
      case Integer_2:
           ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_2;
           typeless_idx = Typeless_2;
# ifdef _TARGET_OS_MAX
           typeless_idx = Typeless_4;
# endif
# ifdef _TARGET_OS_UNICOS
           typeless_idx = Typeless_8;
# endif
           num1 = BITSIZE_INT2_F90 - 1;
           num2 = BITSIZE_INT2_F90;
           break;

      case Typeless_4:
      case Integer_4:
           ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_4;
           typeless_idx = Typeless_4;
# ifdef _TARGET_OS_UNICOS
           typeless_idx = Typeless_8;
# endif
           num1 = BITSIZE_INT4_F90 - 1;
           num2 = BITSIZE_INT4_F90;
           break;

      case Typeless_8:
      case Integer_8:
           ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
           typeless_idx = Typeless_8;
           num1 = BITSIZE_INT8_F90 - 1;
           num2 = BITSIZE_INT8_F90;
           break;
   
      default:
           PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal,
                    IR_COL_NUM(ir_idx),
                    "ibset_intrinsic");
           break;
   }

   if (IL_FLD(list_idx2) == CN_Tbl_Idx) {
      if (compare_cn_and_value(IL_IDX(list_idx2), 0, Lt_Opr) ||
          compare_cn_and_value(IL_IDX(list_idx2), num1, Gt_Opr)) {
         PRINTMSG(arg_info_list[info_idx2].line, 1062, Error,
                  arg_info_list[info_idx2].col);
      }
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx) {
      fold_it = TRUE;
   }

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);
   cn_idx = (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == 
                        CG_INTEGER_DEFAULT_TYPE) ? 
            CN_INTEGER_ONE_IDX :
            C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), 1);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = CN_Tbl_Idx;
   IL_IDX(first_idx) = cn_idx;
   IL_LINE_NUM(first_idx) = line;
   IL_COL_NUM(first_idx) = column;

   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx2));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                       Shiftl_Opr, 
      	               typeless_idx, 
	    	       line, column,
                       NO_Tbl_Idx, NULL_IDX);

   COPY_OPND(opnd, IL_OPND(list_idx1));
   cast_opnd_to_type_idx(&opnd, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
   COPY_OPND(IL_OPND(list_idx1), opnd);

   num1=storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num1);
   cn_idx2 = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num2);

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
     case Ibset_Intrinsic:
     case Iibset_Intrinsic:
     case Jibset_Intrinsic:
     case Kibset_Intrinsic:
   	  bor_idx = gen_ir(IR_Tbl_Idx, shiftl_idx,
                	   Bor_Opr, 
                           typeless_idx, 
                           line, column,
                    	   IL_FLD(list_idx1), IL_IDX(list_idx1));

   	  NTR_IR_LIST_TBL(first_idx);
   	  IL_FLD(first_idx) = IR_Tbl_Idx;
   	  IL_IDX(first_idx) = bor_idx;
          break;


     case Ibclr_Intrinsic:
     case Iibclr_Intrinsic:
     case Jibclr_Intrinsic:
     case Kibclr_Intrinsic:
   	  bnot_idx = gen_ir(IR_Tbl_Idx, shiftl_idx,
                            Bnot_Opr, 
                            typeless_idx, 
                            line, column,
                            NO_Tbl_Idx, NULL_IDX);
   
   	  band_idx = gen_ir(IR_Tbl_Idx, bnot_idx,
                            Band_Opr, 
                            typeless_idx, 
                            line, column,
                            IL_FLD(list_idx1), IL_IDX(list_idx1));
   
   	  NTR_IR_LIST_TBL(first_idx);
   	  IL_FLD(first_idx) = IR_Tbl_Idx;
   	  IL_IDX(first_idx) = band_idx;
          break;


     case Ibchng_Intrinsic:
     case Iibchng_Intrinsic:
     case Jibchng_Intrinsic:
     case Kibchng_Intrinsic:
   	  bnot_idx = gen_ir(IR_Tbl_Idx, shiftl_idx,
                            Bnot_Opr, 
                            typeless_idx, 
                            line, column,
                            NO_Tbl_Idx, NULL_IDX);

   	  COPY_OPND(opnd, IL_OPND(list_idx1));
   	  copy_subtree(&opnd, &opnd);
   	  bnot_idx1 = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                             Bnot_Opr, 
                             typeless_idx, 
                             line, column,
                             NO_Tbl_Idx, NULL_IDX);

   	  NTR_IR_LIST_TBL(first_idx);
   	  COPY_OPND(opnd, IL_OPND(list_idx1));
   	  copy_subtree(&opnd, &opnd);
   	  COPY_OPND(IL_OPND(first_idx), opnd);

   	  NTR_IR_LIST_TBL(second_idx);
   	  IL_FLD(second_idx) = IR_Tbl_Idx;
   	  IL_IDX(second_idx) = bnot_idx1;

   	  NTR_IR_LIST_TBL(third_idx);
   	  IL_FLD(third_idx) = IR_Tbl_Idx;
   	  IL_IDX(third_idx) = bnot_idx;

   	  IL_NEXT_LIST_IDX(first_idx) = second_idx;
   	  IL_NEXT_LIST_IDX(second_idx) = third_idx;

   	  csmg_idx = gen_ir(IL_Tbl_Idx, first_idx,
                            Csmg_Opr, 
                            typeless_idx, 
                            line, column,
                            NO_Tbl_Idx, NULL_IDX);


   	  NTR_IR_LIST_TBL(first_idx);
   	  IL_FLD(first_idx) = IR_Tbl_Idx;
   	  IL_IDX(first_idx) = csmg_idx;
          break;


      default:
          PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal,
                   IR_COL_NUM(ir_idx),
                   "ibset_intrinsic");
          break;
   }

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                      Minus_Opr, 
                      CG_INTEGER_DEFAULT_TYPE, 
                      line, column,
                      CN_Tbl_Idx, cn_idx2);

   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                       Shiftl_Opr, 
                       typeless_idx, 
                       line, column,
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;

   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shifta_idx = gen_ir(IL_Tbl_Idx, first_idx,
                       Shifta_Opr, 
                       typeless_idx, 
                       line, column,
                       NO_Tbl_Idx, NULL_IDX);

   IR_OPR(ir_idx) = Cvrt_Opr;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_FLD_L(ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(ir_idx) = shifta_idx;
   IR_OPND_R(ir_idx) = null_opnd;

   if (fold_it) {
      COPY_OPND(opnd, (*result_opnd));
      fold_aggragate_expression(&opnd, res_exp_desc, FALSE);
      COPY_OPND((*result_opnd), opnd);
   }

   TRACE (Func_Exit, "ibset_intrinsic", NULL);

}  /* ibset_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ISHFT(I, SHIFT) intrinsic.                                *|
|*      Function    ISHA(I, SHIFT) intrinsic.                                 *|
|*      Function    ISHL(I, SHIFT) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ishft_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            gt_idx;
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   int            minus_idx;
   int            first_idx;
   int            second_idx;
   int            third_idx;
   int            shiftl_idx;
   int            shiftr_idx;
   int            shifta_idx;
   int            shiftr_idx2;
   int            cvmgt_idx;
   int            typeless_idx;
   int            cn_idx;
   operator_type  opr;
   int            cn_idx2;
   opnd_type      opnd;
   boolean        fold_it 		= FALSE;
   int            line;
   int            column;
#ifdef KEY /* Bug 10177 */
   long           num1 = 0;
   long           num2 = 0;
#else /* KEY Bug 10177 */
   long           num1;
   long           num2;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "ishft_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   if (ATP_INTRIN_ENUM(*spec_idx) == Isha_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Iisha_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Jisha_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Kisha_Intrinsic) {
      opr = Shifta_Opr;
   }
   else {
      opr = Shiftr_Opr;
   }

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   if (arg_info_list[info_idx1].ed.type == Typeless) {
      PRINTMSG(arg_info_list[info_idx1].line, 1076, Ansi, 
               arg_info_list[info_idx1].col);

      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_8) {
      typeless_idx = Typeless_8;
   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
   }

# ifdef _TARGET_OS_MAX
   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_1 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_2 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif

   if (IL_FLD(list_idx2) == CN_Tbl_Idx) {
      switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
              num1 = BITSIZE_INT1_F90;
              num2 = -BITSIZE_INT1_F90;
              break;

         case Integer_2:
              num1 = BITSIZE_INT2_F90;
              num2 = -BITSIZE_INT2_F90;
              break;

         case Integer_4:
              num1 = BITSIZE_INT4_F90;
              num2 = -BITSIZE_INT4_F90;
              break;

         case Integer_8:
              num1 = BITSIZE_INT8_F90;
              num2 = -BITSIZE_INT8_F90;
              break;
      }
  
      if (compare_cn_and_value(IL_IDX(list_idx2), num1, Gt_Opr) ||
          compare_cn_and_value(IL_IDX(list_idx2), num2, Lt_Opr)) {
         PRINTMSG(arg_info_list[info_idx2].line, 1062, Error,
                  arg_info_list[info_idx2].col);
      }
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) !=
          TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx)) {

      /* cast arg 1 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx1));
      cast_to_type_idx(&opnd,
                       &arg_info_list[info_idx1].ed,
                       ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      COPY_OPND(IL_OPND(list_idx1), opnd);

   }

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) !=
          TYP_LINEAR(arg_info_list[info_idx2].ed.type_idx)) {

      /* cast arg 2 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx2));
      cast_to_type_idx(&opnd,
                       &arg_info_list[info_idx2].ed,
                       ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      COPY_OPND(IL_OPND(list_idx2), opnd);

   }

   if (opr == Shifta_Opr &&
       IL_FLD(list_idx2) == CN_Tbl_Idx) {
   
      if (CN_INT_TO_C(IL_IDX(list_idx2)) == -8 &&
          arg_info_list[info_idx1].ed.linear_type == Integer_1) {
         cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, -7);
         IL_IDX(list_idx2) = cn_idx;
      }

      else if (CN_INT_TO_C(IL_IDX(list_idx2)) == -16 &&
               arg_info_list[info_idx1].ed.linear_type == Integer_2) {
         cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, -15);
         IL_IDX(list_idx2) = cn_idx;
      }

      else if (CN_INT_TO_C(IL_IDX(list_idx2)) == -32 &&
               arg_info_list[info_idx1].ed.linear_type == Integer_4) {
         cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, -31);
         IL_IDX(list_idx2) = cn_idx;
      }

      else if (CN_INT_TO_C(IL_IDX(list_idx2)) == -64 &&
               arg_info_list[info_idx1].ed.linear_type == Integer_8) {
         cn_idx = C_INT_TO_CN(arg_info_list[info_idx2].ed.type_idx, -63);
         IL_IDX(list_idx2) = cn_idx;
      }
   }

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx) {
      fold_it = TRUE;
   }

   num1 = register_bit_size_tbl[
                   TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num1);

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
              num1 = BITSIZE_INT1_F90;
              break;

         case Integer_2:
              num1 = BITSIZE_INT2_F90;
              break;

         case Integer_4:
              num1 = BITSIZE_INT4_F90;
              break;

         case Integer_8:
              num1 = BITSIZE_INT8_F90;
              break;
   }

   cn_idx2 = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num1);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                 Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, column,
                      CN_Tbl_Idx, cn_idx2);

   NTR_IR_LIST_TBL(first_idx);
   COPY_OPND(IL_OPND(first_idx), IL_OPND(list_idx1));
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                  Shiftl_Opr, typeless_idx, line, column, 
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftr_idx2 = gen_ir(IL_Tbl_Idx, first_idx,
                    opr, typeless_idx, line, column, 
                       NO_Tbl_Idx, NULL_IDX);

   /* compute shiftl_idx */
   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftr_idx2;
   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx2));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                    Shiftl_Opr, typeless_idx, line, column, 
                       NO_Tbl_Idx, NULL_IDX);

   /* compute shiftr_idx */

   COPY_OPND(opnd, IL_OPND(list_idx2));
   copy_subtree(&opnd, &opnd);

   minus_idx = gen_ir(CN_Tbl_Idx, CN_INTEGER_ZERO_IDX,
                  Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, column,
                      OPND_FLD(opnd), OPND_IDX(opnd));

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftr_idx2;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                    opr, typeless_idx, line, column, 
                       NO_Tbl_Idx, NULL_IDX);

   /* compute the condition */

   COPY_OPND(opnd, IL_OPND(list_idx2));
   copy_subtree(&opnd, &opnd);

   gt_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
               Gt_Opr, LOGICAL_DEFAULT_TYPE, line, column,
                   CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

   /* set up CVMGT */
   NTR_IR_LIST_TBL(first_idx);
   IL_ARG_DESC_VARIANT(first_idx) = TRUE;
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;

   NTR_IR_LIST_TBL(second_idx);
   IL_ARG_DESC_VARIANT(second_idx) = TRUE;
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = shiftr_idx;

   NTR_IR_LIST_TBL(third_idx);
   IL_ARG_DESC_VARIANT(third_idx) = TRUE;
   IL_FLD(third_idx) = IR_Tbl_Idx;
   IL_IDX(third_idx) = gt_idx;

   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   IL_NEXT_LIST_IDX(second_idx) = third_idx;

   cvmgt_idx = gen_ir(IL_Tbl_Idx, first_idx,
                 Cvmgt_Opr, typeless_idx, line, column,
                      NO_Tbl_Idx, NULL_IDX);

   /* set this flag so this opr is pulled off io lists */
   io_item_must_flatten = TRUE;

   if (fold_it) {
      if (compare_cn_and_value(IL_IDX(list_idx2), 0, Gt_Opr)) {
         cvmgt_idx = shiftl_idx;
      }
      else {
         cvmgt_idx = shiftr_idx;
      }
   }

   num1 = register_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                       ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num1);

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
              num1 = BITSIZE_INT1_F90;
              break;

         case Integer_2:
              num1 = BITSIZE_INT2_F90;
              break;

         case Integer_4:
              num1 = BITSIZE_INT4_F90;
              break;

         case Integer_8:
              num1 = BITSIZE_INT8_F90;
              break;
   }

   cn_idx2 = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num1);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                 Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, column,
                      CN_Tbl_Idx, cn_idx2);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = cvmgt_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shiftl_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shifta_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shifta_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

#ifdef KEY /* Bug 8840 */
   /* The horribly complicated sequence above doesn't work right (on X86 and
    * X86_64, anyway) when the magnitude of shift amount is greater than or
    * equal to the bit size of the data. Putting "shfta" inside a conditional
    * move worked badly because shfta itself expands to a conditional move,
    * and the back end generated jumps for the outer one, so we use bitwise
    * AND with a mask instead.
    *
    * ishft(i, n) =>
    *   ((abs(n) >= 32) ? 0 : -1) & shfta(i, n)
    */
   int abs_width_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd), Abs_Opr,
     arg_info_list[info_idx2].ed.type_idx, line, column, NO_Tbl_Idx,
     NULL_IDX);
   int ge_width_idx = gen_ir(IR_Tbl_Idx, abs_width_idx, Ge_Opr,
     LOGICAL_DEFAULT_TYPE, line, column, CN_Tbl_Idx, cn_idx2);

   int zero_idx = CN_INTEGER_ZERO_IDX;
   int minusone_idx = CN_INTEGER_NEG_ONE_IDX;
   fld_type zero_fld = CN_Tbl_Idx;
   fld_type minusone_fld = CN_Tbl_Idx;
   if (arg_info_list[info_idx1].ed.type_idx != Integer_4) {
     zero_idx = gen_ir(CN_Tbl_Idx, zero_idx, Cvrt_Opr,
       arg_info_list[info_idx1].ed.type_idx, line, column,
       NO_Tbl_Idx, NULL_IDX);
     minusone_idx = gen_ir(CN_Tbl_Idx, minusone_idx, Cvrt_Opr,
       arg_info_list[info_idx1].ed.type_idx, line, column,
       NO_Tbl_Idx, NULL_IDX);
     zero_fld = IR_Tbl_Idx;
     minusone_fld = IR_Tbl_Idx;
   }

   NTR_IR_LIST_TBL(first_idx);
   IL_ARG_DESC_VARIANT(first_idx) = TRUE;
   IL_FLD(first_idx) = zero_fld;
   IL_IDX(first_idx) = zero_idx;
   IL_LINE_NUM(first_idx) = line;
   IL_COL_NUM(first_idx) =  column;

   NTR_IR_LIST_TBL(second_idx);
   IL_ARG_DESC_VARIANT(second_idx) = TRUE;
   IL_FLD(second_idx) = minusone_fld;
   IL_IDX(second_idx) = minusone_idx;
   IL_LINE_NUM(second_idx) = line;
   IL_COL_NUM(second_idx) =  column;

   NTR_IR_LIST_TBL(third_idx);
   IL_ARG_DESC_VARIANT(third_idx) = TRUE;
   IL_FLD(third_idx) = IR_Tbl_Idx;
   IL_IDX(third_idx) = ge_width_idx;

   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   IL_NEXT_LIST_IDX(second_idx) = third_idx;

   int cmove_idx = gen_ir(IL_Tbl_Idx, first_idx, Cvmgt_Opr, typeless_idx, line,
     column, NO_Tbl_Idx, NULL_IDX);

   shifta_idx = gen_ir(IR_Tbl_Idx, cmove_idx, Band_Opr, typeless_idx, line,
     column, IR_Tbl_Idx, shifta_idx);
#endif /* KEY Bug 8840 */

   IR_OPR(ir_idx) = Cvrt_Opr;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_FLD_L(ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(ir_idx) = shifta_idx;
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
   IR_FLD_R(ir_idx) = NO_Tbl_Idx;
   IR_IDX_R(ir_idx) = NULL_IDX;

   if (fold_it) {
      COPY_OPND(opnd, (*result_opnd));
      fold_aggragate_expression(&opnd, res_exp_desc, FALSE);
      COPY_OPND((*result_opnd), opnd);
   }

   TRACE (Func_Exit, "ishft_intrinsic", NULL);

}  /* ishft_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ISHFTC(I, SHIFT, SIZE) intrinsic.                         *|
|*      Function    ISHC(I, SHIFT) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ishftc_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{

# if !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int            ishft2_idx;
   int            minus_idx;
   int            uminus_idx;
   int            shift_idx;
   int            shiftl_idx;
   int            shifta_idx;
   int            mask_idx;
   int            sign_idx;
   int            csmg_idx;
   int            abs_idx;
   int            ior_idx;
   int            plus_idx;
   int            band_idx;
   int            band1_idx;
   int            first_idx;
   int            second_idx;
   int            third_idx;
   int            cn_idx2;
   opnd_type      save_opnd;
   int		  line;
   int		  column;
   int            ishft1_idx;
# endif

   int            cn_idx;
   boolean        fold_it = FALSE;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            info_idx1;
   int            info_idx2;
#ifdef KEY /* Bug 10177 */
   int            info_idx3 = 0;
   long		  num = 0;
#else /* KEY Bug 10177 */
   int            info_idx3;
   long		  num;
#endif /* KEY Bug 10177 */
   opnd_type      opnd;
   int            typeless_idx;


   TRACE (Func_Entry, "ishftc_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   if (arg_info_list[info_idx1].ed.type == Typeless) {
      PRINTMSG(arg_info_list[info_idx1].line, 1076, Ansi, 
               arg_info_list[info_idx1].col);

      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_8) {
      typeless_idx = Typeless_8;
   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
   }

# ifdef _TARGET_OS_MAX
   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_1 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_2 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif

   conform_check(3, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
      case Integer_1:
           num = BITSIZE_INT1_F90;
           break;

      case Integer_2:
           num = BITSIZE_INT2_F90;
           break;

      case Integer_4:
           num = BITSIZE_INT4_F90;
           break;

      case Integer_8:
           num = BITSIZE_INT8_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   if (ATP_INTRIN_ENUM(*spec_idx) != Ishc_Intrinsic &&
       ATP_INTRIN_ENUM(*spec_idx) != Iishc_Intrinsic &&
       ATP_INTRIN_ENUM(*spec_idx) != Jishc_Intrinsic &&
       ATP_INTRIN_ENUM(*spec_idx) != Kishc_Intrinsic) {
      if (IL_IDX(list_idx3) == NULL_IDX) {
         IL_FLD(list_idx3) = CN_Tbl_Idx;
         IL_IDX(list_idx3) = cn_idx;
         IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(list_idx3) = IR_COL_NUM(ir_idx);

         arg_info_list_base = arg_info_list_top;
         arg_info_list_top = arg_info_list_base + 1;

         if (arg_info_list_top >= arg_info_list_size) {
            enlarge_info_list_table();
         }

         IL_ARG_DESC_IDX(list_idx3) = arg_info_list_top;
         arg_info_list[arg_info_list_top] = init_arg_info;
         arg_info_list[arg_info_list_top].ed.type_idx =
                 CG_INTEGER_DEFAULT_TYPE;
         arg_info_list[arg_info_list_top].ed.type = Integer;
         arg_info_list[arg_info_list_top].ed.linear_type =
                 CG_INTEGER_DEFAULT_TYPE;
         arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
         arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);

         info_idx3 = IL_ARG_DESC_IDX(list_idx3);
      }
      else {
         info_idx3 = IL_ARG_DESC_IDX(list_idx3);
#ifdef KEY /* Bug 10410 */
         if (NULL_IDX != is_optional_dummy(list_idx3)) {
	   pass_dummy_or_default_const(list_idx3, cn_idx, FALSE);
	 }
#endif /* KEY Bug 10410 */
      }
   }
   else {
      NTR_IR_LIST_TBL(list_idx3);
      IL_FLD(list_idx3) = CN_Tbl_Idx;
      IL_IDX(list_idx3) = cn_idx;
      IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx3) = IR_COL_NUM(ir_idx);
      IL_NEXT_LIST_IDX(list_idx2) = list_idx3;
      IR_LIST_CNT_R(ir_idx) = 3;
   }

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) !=
       TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx)) {

      /* cast arg 1 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx1));
      cast_to_type_idx(&opnd, 
                       &arg_info_list[info_idx1].ed,
                       ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      COPY_OPND(IL_OPND(list_idx1), opnd);
   }
   
   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) !=
       TYP_LINEAR(arg_info_list[info_idx2].ed.type_idx)) {

      /* cast arg 2 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx2));
      cast_to_type_idx(&opnd, 
                       &arg_info_list[info_idx2].ed,
                       ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      COPY_OPND(IL_OPND(list_idx2), opnd);
   }
   
   if (ATP_INTRIN_ENUM(*spec_idx) != Ishc_Intrinsic &&
       ATP_INTRIN_ENUM(*spec_idx) != Iishc_Intrinsic &&
       ATP_INTRIN_ENUM(*spec_idx) != Jishc_Intrinsic &&
       ATP_INTRIN_ENUM(*spec_idx) != Kishc_Intrinsic) {
      if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) !=
          TYP_LINEAR(arg_info_list[info_idx3].ed.type_idx)) {

         /* cast arg 3 to the result type. */

         COPY_OPND(opnd, IL_OPND(list_idx3));
         cast_to_type_idx(&opnd, 
                          &arg_info_list[info_idx3].ed,
                          ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
         COPY_OPND(IL_OPND(list_idx3), opnd);
      }

      if (IL_FLD(list_idx3) == CN_Tbl_Idx) {
         if (compare_cn_and_value(IL_IDX(list_idx3), num, Gt_Opr)) {
            PRINTMSG(arg_info_list[info_idx3].line, 1062, Error,
                     arg_info_list[info_idx3].col);
         }
      }
   }

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       IL_FLD(list_idx3) == CN_Tbl_Idx) {
      fold_it = TRUE;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   IR_OPR(ir_idx) = Ishftc_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

# else

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   /* start computing band1_idx */

   num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                    ATP_RSLT_IDX(*spec_idx)))] * 2;
   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Minus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                      IL_FLD(list_idx3), IL_IDX(list_idx3));

   mask_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Mask_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX);

   COPY_OPND(opnd, IL_OPND(list_idx1));
   cast_opnd_to_type_idx(&opnd, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
   COPY_OPND(IL_OPND(list_idx1), opnd);

   band1_idx = gen_ir(IR_Tbl_Idx, mask_idx,
                  Band_Opr, typeless_idx, line, column,
                      IL_FLD(list_idx1), IL_IDX(list_idx1));

   /* start computing ishft1_idx */

   num =storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Minus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                      IL_FLD(list_idx2), IL_IDX(list_idx2));

   mask_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Mask_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX);

   num =storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   COPY_OPND(opnd, IL_OPND(list_idx2));
   copy_subtree(&opnd, &opnd);

   plus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Plus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                     OPND_FLD(opnd), OPND_IDX(opnd));

   num = storage_bit_size_tbl[TYP_LINEAR(
                              ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))] - 1;

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   band_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                Band_Opr, typeless_idx, line, column,
                     CN_Tbl_Idx, cn_idx);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = band1_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = band_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shift_idx = gen_ir(IL_Tbl_Idx, first_idx,
                 Shift_Opr, typeless_idx, line, column,
                      NO_Tbl_Idx, NULL_IDX);

   ishft1_idx = gen_ir(IR_Tbl_Idx, shift_idx,
                   Band_Opr, typeless_idx, line, column,
                       IR_Tbl_Idx, mask_idx);

   /* start computing sign_idx */

   COPY_OPND(opnd, IL_OPND(list_idx2));
   copy_subtree(&opnd, &opnd);

   abs_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
               Abs_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                    NO_Tbl_Idx, NULL_IDX);

   COPY_OPND(opnd, IL_OPND(list_idx3));
   copy_subtree(&opnd, &opnd);

   minus_idx = gen_ir(IR_Tbl_Idx, abs_idx,
                  Minus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                      OPND_FLD(opnd), OPND_IDX(opnd));

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = minus_idx;
   NTR_IR_LIST_TBL(second_idx);

   COPY_OPND(opnd, IL_OPND(list_idx2));
   copy_subtree(&opnd, &opnd);

   COPY_OPND(IL_OPND(second_idx), opnd);
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   sign_idx = gen_ir(IL_Tbl_Idx, first_idx,
                 Sign_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                     NO_Tbl_Idx, NULL_IDX);

   uminus_idx = gen_ir(IR_Tbl_Idx, sign_idx,
               Uminus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                       NO_Tbl_Idx, NULL_IDX);

   /* start computing ishft2_idx */

   num =storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Minus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                      IR_Tbl_Idx, uminus_idx);

   mask_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Mask_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX);

   num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                 ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   plus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                Plus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                     IR_Tbl_Idx, uminus_idx);

   num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                 ATP_RSLT_IDX(*spec_idx)))] - 1;

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   band_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                Band_Opr, typeless_idx, line, column,
                     CN_Tbl_Idx, cn_idx);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = band1_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = band_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shift_idx = gen_ir(IL_Tbl_Idx, first_idx,
                 Shift_Opr, typeless_idx, line, column,
                      NO_Tbl_Idx, NULL_IDX);

   ishft2_idx = gen_ir(IR_Tbl_Idx, shift_idx,
                   Band_Opr, typeless_idx, line, column,
                       IR_Tbl_Idx, mask_idx);

   /* OR together the two ishfts */

   ior_idx = gen_ir(IR_Tbl_Idx, ishft1_idx,
                Bor_Opr, typeless_idx, line, column,
                    IR_Tbl_Idx, ishft2_idx);

   /* compute third argument to CSMG */

   num =storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   COPY_OPND(opnd, IL_OPND(list_idx3));
   copy_subtree(&opnd, &opnd);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Minus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                      OPND_FLD(opnd), OPND_IDX(opnd));

   mask_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Mask_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX);

   /* set up arguments */

   NTR_IR_LIST_TBL(first_idx);
   IL_ARG_DESC_VARIANT(first_idx) = TRUE;
   COPY_OPND(opnd, IL_OPND(list_idx1));
   copy_subtree(&opnd, &opnd);
   COPY_OPND(IL_OPND(first_idx), opnd);

   NTR_IR_LIST_TBL(second_idx);
   IL_ARG_DESC_VARIANT(second_idx) = TRUE;
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = ior_idx;

   NTR_IR_LIST_TBL(third_idx);
   IL_ARG_DESC_VARIANT(third_idx) = TRUE;
   IL_FLD(third_idx) = IR_Tbl_Idx;
   IL_IDX(third_idx) = mask_idx;

   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   IL_NEXT_LIST_IDX(second_idx) = third_idx;

   csmg_idx = gen_ir(IL_Tbl_Idx, first_idx,
                 Csmg_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX);

   num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                         ATP_RSLT_IDX(*spec_idx)))];

   cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   switch (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)))) {
         case Integer_1:
              num = BITSIZE_INT1_F90;
              break;

         case Integer_2:
              num = BITSIZE_INT2_F90;
              break;

         case Integer_4:
              num = BITSIZE_INT4_F90;
              break;

         case Integer_8:
              num = BITSIZE_INT8_F90;
              break;
   }

   cn_idx2 = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                 Minus_Opr, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                      CN_Tbl_Idx, cn_idx2);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = csmg_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shiftl_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;
   NTR_IR_LIST_TBL(second_idx);
   IL_FLD(second_idx) = IR_Tbl_Idx;
   IL_IDX(second_idx) = minus_idx;
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shifta_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shifta_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   IR_OPR(ir_idx) = Cvrt_Opr;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_FLD_L(ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(ir_idx) = shifta_idx;
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
   IR_OPND_R(ir_idx) = null_opnd;

# endif

   if (fold_it) {
      COPY_OPND(opnd, (*result_opnd));
      fold_aggragate_expression(&opnd, res_exp_desc, FALSE);
      COPY_OPND((*result_opnd), opnd);
   }

   TRACE (Func_Exit, "ishftc_intrinsic", NULL);

}  /* ishftc_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  MVBITS(FROM, FROMPOS, LEN, TO, TOPOS) intrinsic.          *|
|*      Subroutine  IMVBITS(FROM, FROMPOS, LEN, TO, TOPOS) intrinsic.         *|
|*      Subroutine  JMVBITS(FROM, FROMPOS, LEN, TO, TOPOS) intrinsic.         *|
|*      Subroutine  KMVBITS(FROM, FROMPOS, LEN, TO, TOPOS) intrinsic.         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mvbits_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            info_idx4;
   int            info_idx5;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            list_idx4;
   int            list_idx5;
   int            mask_idx;
   int            minus_idx;
   int            shiftr_idx;
   int            shiftl_idx;
   int            shiftl1_idx;
   int            shiftl2_idx;
   int            csmg_idx;
   int            band_idx;
   int            first_idx;
   int            second_idx;
   int            third_idx;
   int            cn_idx;
   int            u_idx;
   int            type_idx;
   int            typeless_idx;
   opnd_type	  opnd;
   opnd_type	  left_hand_side_opnd;
   int            line; 
   int            column;
   long		  num;


   TRACE (Func_Entry, "mvbits_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   list_idx4 = IL_NEXT_LIST_IDX(list_idx3);
   list_idx5 = IL_NEXT_LIST_IDX(list_idx4);

   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   info_idx3 = IL_ARG_DESC_IDX(list_idx3);
   info_idx4 = IL_ARG_DESC_IDX(list_idx4);
   info_idx5 = IL_ARG_DESC_IDX(list_idx5);

   if (TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx) !=
       TYP_LINEAR(arg_info_list[info_idx4].ed.type_idx)) {
      PRINTMSG(arg_info_list[info_idx1].line, 727, Error,
               arg_info_list[info_idx1].col);
   }

   if (arg_info_list[info_idx1].ed.linear_type == Integer_8 ||
       arg_info_list[info_idx4].ed.linear_type == Integer_8) {
      type_idx = Integer_8;
   }
   else {
      type_idx = INTEGER_DEFAULT_TYPE;
   }

   if (TYP_LINEAR(type_idx) == Integer_8) {
      typeless_idx = Typeless_8;
   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
   }

# ifdef _TARGET_OS_MAX
   if (TYP_LINEAR(type_idx) == Integer_1 ||
       TYP_LINEAR(type_idx) == Integer_2 ||
       TYP_LINEAR(type_idx) == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif

   if (res_exp_desc->rank != arg_info_list[info_idx4].ed.rank) {
      PRINTMSG(arg_info_list[info_idx4].line, 1093, Error,
               arg_info_list[info_idx4].col);
   }

   if (TYP_LINEAR(type_idx) !=
          TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx)) {

      /* cast arg 1 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx1));
      cast_to_type_idx(&opnd,
                       &arg_info_list[info_idx1].ed,
                       type_idx);
      COPY_OPND(IL_OPND(list_idx1), opnd);

   }

   if (TYP_LINEAR(type_idx) !=
          TYP_LINEAR(arg_info_list[info_idx2].ed.type_idx)) {

      /* cast arg 2 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx2));
      cast_to_type_idx(&opnd,
                       &arg_info_list[info_idx2].ed,
                       type_idx);
      COPY_OPND(IL_OPND(list_idx2), opnd);

   }

   if (TYP_LINEAR(type_idx) !=
          TYP_LINEAR(arg_info_list[info_idx3].ed.type_idx)) {

      /* cast arg 3 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx3));
      cast_to_type_idx(&opnd,
                       &arg_info_list[info_idx3].ed,
                       type_idx);
      COPY_OPND(IL_OPND(list_idx3), opnd);

   }

   /* save the original arg 4 for the left side of assignment. */

   COPY_OPND(left_hand_side_opnd, IL_OPND(list_idx4));

   if (TYP_LINEAR(type_idx) !=
          TYP_LINEAR(arg_info_list[info_idx4].ed.type_idx)) {

      /* cast arg 4 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx4));
      cast_to_type_idx(&opnd,
                       &arg_info_list[info_idx4].ed,
                       type_idx);
      COPY_OPND(IL_OPND(list_idx4), opnd);

   }

   if (TYP_LINEAR(type_idx) !=
          TYP_LINEAR(arg_info_list[info_idx5].ed.type_idx)) {

      /* cast arg 5 to the result type. */

      COPY_OPND(opnd, IL_OPND(list_idx5));
      cast_to_type_idx(&opnd,
                       &arg_info_list[info_idx5].ed,
                       type_idx);
      COPY_OPND(IL_OPND(list_idx5), opnd);

   }

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   /* start computing band_idx */

   num    = storage_bit_size_tbl[TYP_LINEAR(typeless_idx)] * 2;
   cn_idx = C_INT_TO_CN(type_idx, num);

   minus_idx = gen_ir(CN_Tbl_Idx, cn_idx, 
                  Minus_Opr, type_idx, line, column,
                      IL_FLD(list_idx3), IL_IDX(list_idx3));

   mask_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Mask_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX);
   
   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = mask_idx;
   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx5));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl1_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shiftl_Opr, typeless_idx, line, column,
                        NO_Tbl_Idx, NULL_IDX);
   
   /* compute shiftl2_idx */
   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = mask_idx;
   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx2));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   
   shiftl2_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shiftl_Opr, typeless_idx, line, column,
                        NO_Tbl_Idx, NULL_IDX);

   band_idx = gen_ir(IR_Tbl_Idx, shiftl2_idx,
                Band_Opr, typeless_idx, line, column,
                     IL_FLD(list_idx1), IL_IDX(list_idx1));

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = band_idx;
   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx2));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shiftr_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);

   NTR_IR_LIST_TBL(first_idx);
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftr_idx;
   NTR_IR_LIST_TBL(second_idx);
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx5));
   IL_NEXT_LIST_IDX(first_idx) = second_idx;

   shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                   Shiftl_Opr, typeless_idx, line, column,
                       NO_Tbl_Idx, NULL_IDX);
   
   /* set up arguments to CSMG */

   NTR_IR_LIST_TBL(first_idx);
   IL_ARG_DESC_VARIANT(first_idx) = TRUE;
   IL_FLD(first_idx) = IR_Tbl_Idx;
   IL_IDX(first_idx) = shiftl_idx;
   
   NTR_IR_LIST_TBL(second_idx);
   IL_ARG_DESC_VARIANT(second_idx) = TRUE;
   COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx4));
   
   NTR_IR_LIST_TBL(third_idx);
   IL_ARG_DESC_VARIANT(third_idx) = TRUE;
   IL_FLD(third_idx) = IR_Tbl_Idx;
   IL_IDX(third_idx) = shiftl1_idx;
   
   IL_NEXT_LIST_IDX(first_idx) = second_idx;
   IL_NEXT_LIST_IDX(second_idx) = third_idx;
   
   csmg_idx = gen_ir(IL_Tbl_Idx, first_idx,
                 Csmg_Opr, typeless_idx, line, column,
                     NO_Tbl_Idx, NULL_IDX);

   u_idx = gen_ir(IR_Tbl_Idx, csmg_idx,
                  Cvrt_Unsigned_Opr, type_idx, line, column,
                  NO_Tbl_Idx, NULL_IDX);
   
   IR_OPR(ir_idx) = Asg_Opr;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   IR_OPR(ir_idx) = Mvbits_Opr;
# else
   IR_FLD_R(ir_idx) = IR_Tbl_Idx;
   IR_IDX_R(ir_idx) = u_idx;
# endif

   IR_TYPE_IDX(ir_idx) = type_idx;
   COPY_OPND(IR_OPND_L(ir_idx), left_hand_side_opnd);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "mvbits_intrinsic", NULL);

}  /* mvbits_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  EXIT(STATUS) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void   exit_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "exit_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "exit_intrinsic", NULL);

}  /* exit_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  SYSTEM_CLOCK(COUNT, COUNT_RATE, COUNT_MAX) intrinsic.     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void   system_clock_intrinsic(opnd_type     *result_opnd,
                              expr_arg_type *res_exp_desc,
                              int           *spec_idx)
{
   int            ir_idx;
   int		  info_idx1;
   int		  info_idx2;
   int		  info_idx3;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;


   TRACE (Func_Entry, "system_clock_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);

#ifdef KEY /* Bug 6009 */
   /* Before the fix, the specific (singly-indented) intrin_tbl entries
    * under "system_clock" had zero in the intrin_enum member, so this function
    * was never called and, because.the loop that processes specific entries
    * inside complete_intrinsic_definition requires_a nonzero intrin_enum to
    * recognize each specific entry, the integer*8 version was not created and
    * integer*8 arguments were not allowed. The integer*4 version bypassed
    * the call to (*intrinsic_semantics[])() but somehow emitted a working
    * function call anyway.
    *
    * The integer*4 specific fcn actually had I1_MASK|I2_MASK|I4_MASK on each
    * argument, but the runtime library function would erroneously store
    * integer*4 values into integer*1 and integer*2 arguments.
    *
    * The integer*4 specific entry also has Integer*4 in the data_type member,
    * and the integer*8 specific entry had Integer_8.
    *
    * I've removed all this strangeness; I wish it was an almost-working
    * attempt to allow all combinations of integer argument types, but I
    * don't see any evidence of that. And the tests below, which seem
    * inexplicable (why not perform the check by putting I4_MASK in each
    * argument entry?) weren't getting executed before, so I've removed
    * them because they cause trouble when calling the integer*8 specific
    * in -i4 mode.
    */
#else /* KEY Bug 6009 */
   if ((list_idx3 != NULL_IDX) && (IL_IDX(list_idx3) != NULL_IDX)) {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);
      if (arg_info_list[info_idx3].ed.type_idx != INTEGER_DEFAULT_TYPE) {
         PRINTMSG(arg_info_list[info_idx3].line, 1533, Error, 
                  arg_info_list[info_idx3].col);
      }
   } 

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      if (arg_info_list[info_idx2].ed.type_idx != INTEGER_DEFAULT_TYPE) {
         PRINTMSG(arg_info_list[info_idx2].line, 1533, Error, 
                  arg_info_list[info_idx2].col);
      }
   }     

   if ((list_idx1 != NULL_IDX) && (IL_IDX(list_idx1) != NULL_IDX)) {
      info_idx1 = IL_ARG_DESC_IDX(list_idx1);
      if (arg_info_list[info_idx1].ed.type_idx != INTEGER_DEFAULT_TYPE) {
         PRINTMSG(arg_info_list[info_idx1].line, 1533, Error, 
                  arg_info_list[info_idx1].col);
      }
   }     
#endif /* KEY Bug 6009 */

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "system_clock_intrinsic", NULL);

}  /* system_clock_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  IDATE(I, J, K) intrinsic.       		              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    idate_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;

   TRACE (Func_Entry, "idate_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   info_idx3 = IL_ARG_DESC_IDX(list_idx3);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx) {
      PRINTMSG(arg_info_list[info_idx1].line, 1650, Error, 
               arg_info_list[info_idx1].col);
   }

   if (IL_FLD(list_idx2) == CN_Tbl_Idx) {
      PRINTMSG(arg_info_list[info_idx2].line, 1650, Error, 
               arg_info_list[info_idx2].col);
   }

   if (IL_FLD(list_idx3) == CN_Tbl_Idx) {
      PRINTMSG(arg_info_list[info_idx3].line, 1650, Error, 
               arg_info_list[info_idx3].col);
   }


   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "idate_intrinsic", NULL);

}  /* idate_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  RANDOM_SEED(SIZE, PUT, GET) intrinsic.                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    random_seed_intrinsic(opnd_type     *result_opnd,
                              expr_arg_type *res_exp_desc,
                              int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int		  info_idx1;
   int		  info_idx2;
   int		  info_idx3;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int		  loc_idx;
   int            ranget_idx;
   int            ranset_idx;
   int            ranf_idx;
   int            tmp_attr;
   int		  unused1	= NULL_IDX;
   int		  unused2	= NULL_IDX;
   opnd_type	  old_opnd;
   opnd_type	  base_opnd;
   int            line;
   int            column;


   TRACE (Func_Entry, "random_seed_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

#ifdef KEY /* Bug 10410 */
      int args[3];
      args[0] = list_idx1 = IR_IDX_R(ir_idx);
      args[1] = list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
      args[2] = list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
      /* If the actual arguments are optional dummy arguments belonging to
       * the caller, then we can't assume at compile time that the prohibition
       * prohibition against multiple arguments is being violated: maybe the
       * optional arguments are not present. But if have more than one
       * non-optional argument, emit an error per the standard. */
      int nargs = 0;
      for (int i = 0; i < (sizeof(args) / sizeof(*args)); i += 1) {
        nargs += NULL_IDX != IL_IDX(args[i]) &&
	  NULL_IDX == is_optional_dummy(args[i]);
      }
      if (nargs > 1)
#else /* KEY Bug 10410 */
      list_idx1 = IR_IDX_R(ir_idx);
      list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
      list_idx3 = IL_NEXT_LIST_IDX(list_idx2);

      if (((IL_IDX(list_idx1) != NULL_IDX) &&
           (IL_IDX(list_idx2) != NULL_IDX)) ||
          ((IL_IDX(list_idx1) != NULL_IDX) &&
           (IL_IDX(list_idx3) != NULL_IDX)) ||
          ((IL_IDX(list_idx2) != NULL_IDX) &&
           (IL_IDX(list_idx3) != NULL_IDX)))
#endif /* KEY Bug 10410 */
      { 
         PRINTMSG(IR_LINE_NUM(ir_idx), 830,  Error, 
                  IR_COL_NUM(ir_idx));
      }
   
   
      if ((list_idx3 != NULL_IDX) && (IL_IDX(list_idx3) != NULL_IDX)) {

         COPY_OPND(old_opnd, IL_OPND(list_idx3));
         info_idx3 = IL_ARG_DESC_IDX(list_idx3);

         if (IL_FLD(list_idx3) == CN_Tbl_Idx) {
            PRINTMSG(arg_info_list[info_idx3].line, 1214, Error,
                     arg_info_list[info_idx3].col);
         }

         if (! arg_info_list[info_idx3].ed.reference &&
             ! arg_info_list[info_idx3].ed.tmp_reference) {

            tmp_attr = create_tmp_asg(&old_opnd,
                         (expr_arg_type *)&(arg_info_list[info_idx3].ed),
                                      &base_opnd,
                                      Intent_In,
                                      TRUE,
                                      FALSE);

            COPY_OPND(old_opnd, base_opnd);
         }

         if (arg_info_list[info_idx3].ed.rank > 0) {
            make_base_subtree(&old_opnd, &base_opnd, &unused1, &unused2);
         }
         else {
            COPY_OPND(base_opnd, old_opnd);
         }

         loc_idx = gen_ir(OPND_FLD(base_opnd), OPND_IDX(base_opnd), 
                       Aloc_Opr, CRI_Ptr_8, line, column,
                          NO_Tbl_Idx, NULL_IDX);

         ranget_idx = gen_ir(IR_Tbl_Idx, loc_idx,
                        Ranget_Opr, TYPELESS_DEFAULT_TYPE, line, column,
                             NO_Tbl_Idx, NULL_IDX);
   
         IR_OPR(ir_idx) = Asg_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(loc_idx));
         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = ranget_idx;
         IR_LINE_NUM_R(ir_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(ir_idx) = IR_COL_NUM(ir_idx);
      } 
      else if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {

         COPY_OPND(old_opnd, IL_OPND(list_idx2));
         info_idx2 = IL_ARG_DESC_IDX(list_idx2);

         if (! arg_info_list[info_idx2].ed.reference &&
             ! arg_info_list[info_idx2].ed.tmp_reference) {
            tmp_attr = create_tmp_asg(&old_opnd,
                         (expr_arg_type *)&(arg_info_list[info_idx2].ed),
                                      &base_opnd,
                                      Intent_In,
                                      TRUE,
                                      FALSE);

            COPY_OPND(old_opnd, base_opnd);
         }

         if (arg_info_list[info_idx2].ed.rank > 0) {
            make_base_subtree(&old_opnd, &base_opnd, &unused1, &unused2);
         }
         else {
            COPY_OPND(base_opnd, old_opnd);
         }

         ranset_idx = gen_ir(OPND_FLD(base_opnd), OPND_IDX(base_opnd),
                          Ranset_Opr, TYPELESS_DEFAULT_TYPE, line, column,
                             NO_Tbl_Idx, NULL_IDX);

         IR_OPR(ir_idx) = Asg_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(ranset_idx));
         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = ranset_idx;
         IR_LINE_NUM_R(ir_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(ir_idx) = IR_COL_NUM(ir_idx);
      } 
      else if ((list_idx1 != NULL_IDX) && (IL_IDX(list_idx1) != NULL_IDX)) {
         info_idx1 = IL_ARG_DESC_IDX(list_idx1);

         if (IL_FLD(list_idx1) == CN_Tbl_Idx) {
            PRINTMSG(arg_info_list[info_idx1].line, 1214, Error,
                     arg_info_list[info_idx1].col);
         }


# if (defined(KEY))
         cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 32);
# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 64);
# else
         cn_idx = CN_INTEGER_ONE_IDX;
# endif

         IR_OPR(ir_idx) = Asg_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx1));
         IR_FLD_R(ir_idx) = CN_Tbl_Idx;
         IR_IDX_R(ir_idx) = cn_idx;
         IR_LINE_NUM_R(ir_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(ir_idx) = IR_COL_NUM(ir_idx);
      }     
      else {

         ranf_idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
                       Ranf_Opr, REAL_DEFAULT_TYPE, line, column,
                           NO_Tbl_Idx, NULL_IDX);

         tree_has_ranf = TRUE;
   
         tmp_attr = gen_compiler_tmp(IR_LINE_NUM(ir_idx),
                                     IR_COL_NUM(ir_idx),
                                     Priv, TRUE);
         ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
         ATD_TYPE_IDX(tmp_attr) = REAL_DEFAULT_TYPE;
         AT_SEMANTICS_DONE(tmp_attr) = TRUE;
   
         IR_OPR(ir_idx) = Asg_Opr;
         IR_FLD_L(ir_idx) = AT_Tbl_Idx;
         IR_IDX_L(ir_idx) = tmp_attr;
         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = ranf_idx;
         IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
      }

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "random_seed_intrinsic", NULL);

}  /* random_seed_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  GET_IEEE_STATUS(STATUS) intrinsic.                        *|
|*      Subroutine  SET_IEEE_STATUS(STATUS) intrinsic.                        *|
|*      Subroutine  GET_IEEE_EXCEPTIONS(STATUS) intrinsic.                    *|
|*      Subroutine  SET_IEEE_EXCEPTIONS(STATUS) intrinsic.                    *|
|*      Subroutine  GET_IEEE_INTERRUPTS(STATUS) intrinsic.                    *|
|*      Subroutine  SET_IEEE_INTERRUPTS(STATUS) intrinsic.                    *|
|*      Subroutine  GET_IEEE_ROUNDING_MODE(STATUS) intrinsic.                 *|
|*      Subroutine  SET_IEEE_ROUNDING_MODE(STATUS) intrinsic.                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    get_ieee_status_intrinsic(opnd_type     *result_opnd,
                                  expr_arg_type *res_exp_desc,
                                  int           *spec_idx)
{
   int            idx;
   int            idx1;
   int            ir_idx;
   int		  info_idx1;
   int            list_idx1;
   int            line;
   int            column;


   TRACE (Func_Entry, "get_ieee_status_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   switch (ATP_INTRIN_ENUM(*spec_idx)) {

      case Get_Ieee_Status_Intrinsic:
         IR_OPR(ir_idx) = Asg_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx1));

         NTR_IR_LIST_TBL(idx1);
         IL_FLD(idx1) = CN_Tbl_Idx;
         IL_IDX(idx1) = CN_INTEGER_ZERO_IDX;
         IL_LINE_NUM(idx1) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(idx1) = IR_COL_NUM(ir_idx);

         idx = gen_ir(IL_Tbl_Idx, idx1,
                  Get_Ieee_Status_Opr, arg_info_list[info_idx1].ed.type_idx,
                                        line, column,
                      NO_Tbl_Idx, NULL_IDX);

         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = idx;
         break;

      case Set_Ieee_Status_Intrinsic:
         IR_OPR(ir_idx) = Set_Ieee_Status_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
         IR_IDX_R(ir_idx) = NULL_IDX;
         IR_FLD_R(ir_idx) = NO_Tbl_Idx;
         break;

      case Get_Ieee_Exceptions_Intrinsic:
         IR_OPR(ir_idx) = Asg_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx1));

         idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
                  Get_Ieee_Exceptions_Opr, arg_info_list[info_idx1].ed.type_idx,
                                           line, column,
                      NO_Tbl_Idx, NULL_IDX);

         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = idx;
         break;

      case Set_Ieee_Exceptions_Intrinsic:
         IR_OPR(ir_idx) = Set_Ieee_Exceptions_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
         IR_OPND_R(ir_idx) = null_opnd;
         break;

      case Get_Ieee_Interrupts_Intrinsic:
         IR_OPR(ir_idx) = Asg_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx1));

         idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
                  Get_Ieee_Interrupts_Opr, arg_info_list[info_idx1].ed.type_idx,
                                           line, column,
                      NO_Tbl_Idx, NULL_IDX);

         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = idx;
         break;

      case Set_Ieee_Interrupts_Intrinsic:
         IR_OPR(ir_idx) = Set_Ieee_Interrupts_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
         IR_OPND_R(ir_idx) = null_opnd;
         break;

      case Get_Ieee_Rounding_Mode_Intrinsic:
         IR_OPR(ir_idx) = Asg_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx1));

         idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
                  Get_Ieee_Rounding_Mode_Opr, 
                           arg_info_list[info_idx1].ed.type_idx, line, column,
                      NO_Tbl_Idx, NULL_IDX);

         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = idx;
         break;

      case Set_Ieee_Rounding_Mode_Intrinsic:
         IR_OPR(ir_idx) = Set_Ieee_Rounding_Mode_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
         IR_OPND_R(ir_idx) = null_opnd;
         break;
   }

   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "get_ieee_status_intrinsic", NULL);

}  /* get_ieee_status_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    TEST_IEEE_INTERRUPT(INTERRUPT) intrinsic.                 *|
|*      Function    TEST_IEEE_EXCEPTION(EXCEPTION) intrinsic.                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    test_ieee_interrupt_intrinsic(opnd_type     *result_opnd,
                                      expr_arg_type *res_exp_desc,
                                      int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "test_ieee_interrupt_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   switch (ATP_INTRIN_ENUM(*spec_idx)) {

      case Test_Ieee_Interrupt_Intrinsic:
         IR_OPR(ir_idx) = Test_Ieee_Interrupt_Opr;
         break;

      case Test_Ieee_Exception_Intrinsic:
         IR_OPR(ir_idx) = Test_Ieee_Exception_Opr;
         break;
   }

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "test_ieee_interrupt_intrinsic", NULL);

}  /* test_ieee_interrupt_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  SET_IEEE_EXCEPTION(EXCEPTION) intrinsic.                  *|
|*      Subroutine  CLEAR_IEEE_EXCEPTION(EXCEPTION) intrinsic.                *|
|*      Subroutine  ENABLE_IEEE_INTERRUPT(INTERRUPT) intrinsic.               *|
|*      Subroutine  DISABLE_IEEE_INTERRUPT(INTERRUPT) intrinsic.              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    set_ieee_exception_intrinsic(opnd_type     *result_opnd,
                                     expr_arg_type *res_exp_desc,
                                     int           *spec_idx)
{
   int            ir_idx;
   int            idx;
   int		  info_idx1;
   int            list_idx1;

   TRACE (Func_Entry, "set_ieee_exception_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   switch (ATP_INTRIN_ENUM(*spec_idx)) {

      case Set_Ieee_Exception_Intrinsic:
         IR_OPR(ir_idx) = Set_Ieee_Exception_Opr;

         NTR_IR_LIST_TBL(idx);
         IL_NEXT_LIST_IDX(list_idx1) = idx;
         IL_FLD(idx) = CN_Tbl_Idx;
         IL_IDX(idx) = CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(idx) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(idx) = IR_COL_NUM(ir_idx);
         IR_LIST_CNT_R(ir_idx) = 2;
         break;

      case Clear_Ieee_Exception_Intrinsic:
         IR_OPR(ir_idx) = Clear_Ieee_Exception_Opr;

         NTR_IR_LIST_TBL(idx);
         IL_NEXT_LIST_IDX(list_idx1) = idx;
         IL_FLD(idx) = CN_Tbl_Idx;
         IL_IDX(idx) = CN_INTEGER_ZERO_IDX;
         IL_LINE_NUM(idx) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(idx) = IR_COL_NUM(ir_idx);
         IR_LIST_CNT_R(ir_idx) = 2;
         break;

      case Enable_Ieee_Interrupt_Intrinsic:
         IR_OPR(ir_idx) = Enable_Ieee_Interrupt_Opr;
         break;

      case Disable_Ieee_Interrupt_Intrinsic:
         IR_OPR(ir_idx) = Disable_Ieee_Interrupt_Opr;
         break;
   }

   if (arg_info_list[info_idx1].ed.rank > 1) {
      PRINTMSG(arg_info_list[info_idx1].line, 654, Error,
               arg_info_list[info_idx1].col);
   }

   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "set_ieee_exception_intrinsic", NULL);

}  /* set_ieee_exception_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    IEEE_BINARY_SCALE(Y, N) intrinsic.                        *|
|*      Function    IEEE_COPY_SIGN(X, Y) intrinsic.                           *|
|*      Function    IEEE_EXPONENT(X, Y) intrinsic.                            *|
|*      Function    IEEE_INT(X, Y) intrinsic.                                 *|
|*      Function    INT_MULT_UPPER(I, J) intrinsic.  	                      *|
|*      Function    IEEE_NEXT_AFTER(X, Y) intrinsic.                          *|
|*      Function    IEEE_REAL(X, Y) intrinsic.                                *|
|*      Function    IEEE_REMAINDER(X, Y) intrinsic.                           *|
|*      Function    IEEE_UNORDERED(X, Y) intrinsic.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ieee_real_intrinsic(opnd_type     *result_opnd,
                            expr_arg_type *res_exp_desc,
                            int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
#ifdef KEY /* Bug 10177 */
   int            info_idx2 = 0;
#else /* KEY Bug 10177 */
   int            info_idx2;
#endif /* KEY Bug 10177 */
   opnd_type      opnd;


   TRACE (Func_Entry, "ieee_real_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   }

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
      case Ieee_Int_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

         if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
            arg_info_list[info_idx2].ed.type_idx;
         }

         IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;
         IR_LIST_CNT_R(ir_idx) = 1;
         IR_OPR(ir_idx) = Ieee_Int_Opr;
         break;

      case Ieee_Real_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = REAL_DEFAULT_TYPE;

         if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
            arg_info_list[info_idx2].ed.type_idx;
         }

         IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;
         IR_LIST_CNT_R(ir_idx) = 1;
         IR_OPR(ir_idx) = Ieee_Real_Opr;
         break;

      case Int_Mult_Upper_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) =
         arg_info_list[info_idx1].ed.type_idx;

         if (arg_info_list[info_idx1].ed.type == Typeless) {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

            COPY_OPND(opnd, IL_OPND(list_idx1));
            cast_opnd_to_type_idx(&opnd, INTEGER_DEFAULT_TYPE);
            COPY_OPND(IL_OPND(list_idx1), opnd);

            COPY_OPND(opnd, IL_OPND(list_idx2));
            cast_opnd_to_type_idx(&opnd, INTEGER_DEFAULT_TYPE);
            COPY_OPND(IL_OPND(list_idx2), opnd);
         }

         IR_OPR(ir_idx) = Int_Mult_Upper_Opr;
         break;

      case Ieee_Exponent_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

         if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
            arg_info_list[info_idx2].ed.type_idx;

            if (arg_info_list[info_idx2].ed.rank != 0) {
               PRINTMSG(arg_info_list[info_idx2].line, 654, Error,
                        arg_info_list[info_idx2].col);
            }
         }

         IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;
         IR_LIST_CNT_R(ir_idx) = 1;
         IR_OPR(ir_idx) = Ieee_Exponent_Opr;
         break;

      case Ieee_Remainder_Intrinsic:
         if (TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx) >
             TYP_LINEAR(arg_info_list[info_idx2].ed.type_idx)) {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
            arg_info_list[info_idx1].ed.type_idx;
         }
         else {
            ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
            arg_info_list[info_idx2].ed.type_idx;
         }
         IR_OPR(ir_idx) = Ieee_Remainder_Opr;
         break;

      case Ieee_Unordered_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
         IR_OPR(ir_idx) = Ieee_Unordered_Opr;
         break;

      case Ieee_Binary_Scale_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
         arg_info_list[info_idx1].ed.type_idx;
         IR_OPR(ir_idx) = Ieee_Binary_Scale_Opr;
         break;

      case Ieee_Next_After_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
         arg_info_list[info_idx1].ed.type_idx;
         IR_OPR(ir_idx) = Ieee_Next_After_Opr;
         break;

      case Ieee_Copy_Sign_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
         arg_info_list[info_idx1].ed.type_idx;
         IR_OPR(ir_idx) = Ieee_Copy_Sign_Opr;
         break;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "ieee_real_intrinsic", NULL);

}  /* ieee_real_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    IEEE_FINITE(X) intrinsic.  		                      *|
|*      Function    IEEE_IS_NAN(X) intrinsic.  		                      *|
|*      Function    ISNAN(X) intrinsic.  		                      *|
|*      Function    IEEE_CLASS(X) intrinsic.  		                      *|
|*      Function    FP_CLASS(X) intrinsic.     		                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    ieee_finite_intrinsic(opnd_type     *result_opnd,
                              expr_arg_type *res_exp_desc,
                              int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "ieee_finite_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   switch (ATP_INTRIN_ENUM(*spec_idx)) {

      case Ieee_Finite_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
         IR_OPR(ir_idx) = Ieee_Finite_Opr;
         break;

      case Ieee_Is_Nan_Intrinsic:
      case Isnan_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
         IR_OPR(ir_idx) = Ieee_Is_Nan_Opr;
         break;

      case Ieee_Class_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
         IR_OPR(ir_idx) = Ieee_Class_Opr;
         break;

      case Fp_Class_Intrinsic:
         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
         break;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (ATP_INTRIN_ENUM(*spec_idx) != Fp_Class_Intrinsic) {
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "ieee_finite_intrinsic", NULL);

}  /* ieee_finite_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  LOCK_RELEASE(I) intrinsic.                                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    lock_release_intrinsic(opnd_type     *result_opnd,
                               expr_arg_type *res_exp_desc,
                               int           *spec_idx) 
{
   int            ir_idx;


   TRACE (Func_Entry, "lock_release_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   IR_TYPE_IDX(ir_idx) = REAL_DEFAULT_TYPE;

   IR_OPR(ir_idx) = Lock_Release_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   io_item_must_flatten = TRUE;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "lock_release_intrinsic", NULL);

}  /* lock_release_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine  RANDOM_NUMBER(HARVEST) intrinsic.                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    random_number_intrinsic(opnd_type     *result_opnd,
                                expr_arg_type *res_exp_desc,
                                int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   int            info_idx1;
   int            ranf_idx;
   int            attr_idx;
   int            line;
   int            col;


   TRACE (Func_Entry, "random_number_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   IR_TYPE_IDX(ir_idx) = REAL_DEFAULT_TYPE;
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   IR_TYPE_IDX(ir_idx) = arg_info_list[info_idx1].ed.type_idx;

   if (arg_info_list[info_idx1].ed.reference) {
      attr_idx = find_base_attr(&IL_OPND(list_idx1), &line, &col);
      AT_DEFINED(attr_idx) = TRUE;
  
      if ((AT_OBJ_CLASS(attr_idx) == Data_Obj) &&
          (ATD_CLASS(attr_idx) == Function_Result) &&
          (ATD_FUNC_IDX(attr_idx) != NULL_IDX)) {
         AT_DEFINED(ATD_FUNC_IDX(attr_idx)) = TRUE;
      }
   }

   if (IL_FLD(list_idx1) == CN_Tbl_Idx) { 
      PRINTMSG(arg_info_list[info_idx1].line, 1214,  Error, 
               arg_info_list[info_idx1].col);
   }

   ranf_idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
                 Ranf_Opr, IR_TYPE_IDX(ir_idx), IR_LINE_NUM(ir_idx),
                                              IR_COL_NUM(ir_idx),
                     NO_Tbl_Idx, NULL_IDX);

   IR_OPR(ir_idx) = Asg_Opr;
   IR_FLD_L(ir_idx) = IL_FLD(list_idx1);
   IR_IDX_L(ir_idx) = IL_IDX(list_idx1);
   IR_FLD_R(ir_idx) = IR_Tbl_Idx;
   IR_IDX_R(ir_idx) = ranf_idx;
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
   IR_LINE_NUM_R(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_R(ir_idx) = IR_COL_NUM(ir_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "random_number_intrinsic", NULL);

}  /* random_number_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ALL(MASK, DIM) intrinsic.                                 *|
|*      Function    ANY(MASK, DIM) intrinsic.                                 *|
|*      Function    COUNT(MASK, DIM) intrinsic.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    all_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   int            attr_idx;
   int            ir_idx;
   int            i;
   int            j;
   int            line;
   int            col;
   opnd_type	  opnd;


   TRACE (Func_Entry, "all_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   if (ATP_INTRIN_ENUM(*spec_idx) == Count_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = 
      arg_info_list[info_idx1].ed.type_idx;
   }

   if (arg_info_list[info_idx1].ed.rank < 1) {
      PRINTMSG(arg_info_list[info_idx1].line, 640,  Error, 
               arg_info_list[info_idx1].col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (list_idx2 != NULL_IDX && IL_IDX(list_idx2) != NULL_IDX) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);

      if (IL_FLD(list_idx2) == CN_Tbl_Idx) { /* DIM is a constant */
         if (compare_cn_and_value(IL_IDX(list_idx2), 1, Lt_Opr) ||
             compare_cn_and_value(IL_IDX(list_idx2),
                                  (long) arg_info_list[info_idx1].ed.rank,
                                  Gt_Opr)) {

            PRINTMSG(arg_info_list[info_idx2].line, 881, Error,
                     arg_info_list[info_idx2].col);
         }

         res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
         res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
         res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
         j = 1;
         for (i = 1; i < 8; i++) {
            if (i == CN_INT_TO_C(IL_IDX(list_idx2))) {
               j = j + 1;
            }

            COPY_OPND(res_exp_desc->shape[i-1],
                      arg_info_list[info_idx1].ed.shape[j-1]);
            j = j + 1;
         }

# ifdef _INLINE_INTRINSICS
         ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif
      }
      else {   /* DIM is not constant */
         if (arg_info_list[info_idx2].ed.reference) {
            attr_idx = find_base_attr(&IL_OPND(list_idx2), &line, &col);

            if ((AT_OPTIONAL(attr_idx)) && 
                (arg_info_list[info_idx2].line != 0)) {
               PRINTMSG(arg_info_list[info_idx2].line, 875, Error,
                        arg_info_list[info_idx2].col);
            }
         }
      }

      COPY_OPND(opnd, IL_OPND(list_idx2));
      cast_to_cg_default(&opnd, &(arg_info_list[info_idx2].ed));
      COPY_OPND(IL_OPND(list_idx2), opnd);

      res_exp_desc->rank = arg_info_list[info_idx1].ed.rank - 1;
   } 
   else {
      res_exp_desc->rank = 0;  /* result is scalar */
      NTR_IR_LIST_TBL(list_idx2);
      IL_INTRIN_PLACE_HOLDER(list_idx2) = TRUE;
      IL_ARG_DESC_VARIANT(list_idx2) = TRUE;
      IL_NEXT_LIST_IDX(list_idx1) = list_idx2;
      IR_LIST_CNT_R(ir_idx) = 2;
# ifdef _INLINE_INTRINSICS
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif
   }

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      io_item_must_flatten = TRUE;
      if (ATP_INTRIN_ENUM(*spec_idx) == Any_Intrinsic) {
         IR_OPR(ir_idx) = Any_Opr;
      }
      else if (ATP_INTRIN_ENUM(*spec_idx) == All_Intrinsic) {
         IR_OPR(ir_idx) = All_Opr;
      }
      else {
         IR_OPR(ir_idx) = Count_Opr;
      }

      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
      IR_LIST_CNT_L(ir_idx) = IR_LIST_CNT_R(ir_idx);
   }

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "all_intrinsic", NULL);

}  /* all_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    TINY(X) intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    tiny_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
#ifdef KEY /* Bug 10177 */
   int            cn_idx = 0;
#else /* KEY Bug 10177 */
   int            cn_idx;
#endif /* KEY Bug 10177 */
   int            info_idx1;
   int            ir_idx;


   TRACE (Func_Entry, "tiny_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           cn_idx = cvrt_str_to_cn(TINY_REAL4_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Real_8:
           cn_idx = cvrt_str_to_cn(TINY_REAL8_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;

      case Real_16:
           cn_idx = cvrt_str_to_cn(TINY_REAL16_F90,
                                   arg_info_list[info_idx1].ed.linear_type);
           break;
   }


   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "tiny_intrinsic", NULL);

}  /* tiny_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SPACING(X) intrinsic.                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    spacing_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            info_idx1;
   int            list_idx1;
   int            list_idx2;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "spacing_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Spacing_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_LIST_CNT_L(ir_idx) = 2;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           num = DIGITS_REAL4_F90;
           break;

      case Real_8:
           num = DIGITS_REAL8_F90;
           break;

      case Real_16:
           num = DIGITS_REAL16_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   NTR_IR_LIST_TBL(list_idx2);
   IL_ARG_DESC_VARIANT(list_idx2) = TRUE;

   /* link list together */
   IL_NEXT_LIST_IDX(list_idx1) = list_idx2;

   IL_IDX(list_idx2) = cn_idx;
   IL_FLD(list_idx2) = CN_Tbl_Idx;

   IL_LINE_NUM(list_idx2) = IL_LINE_NUM(list_idx1);
   IL_COL_NUM(list_idx2) = IL_COL_NUM(list_idx1);

   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "spacing_intrinsic", NULL);

}  /* spacing_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    CSHIFT(ARRAY, SHIFT, DIM) intrinsic.                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    cshift_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            type_idx;
   opnd_type	  opnd;


   TRACE (Func_Entry, "cshift_intrinsic", NULL);

# ifdef _INLINE_INTRINSICS
   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
# endif

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   type_idx = arg_info_list[info_idx1].ed.type_idx;

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   if ((arg_info_list[info_idx1].ed.rank == 1) &&
       (arg_info_list[info_idx2].ed.rank != 0)) {
      PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
               arg_info_list[info_idx2].col);
   }
   else if ((arg_info_list[info_idx2].ed.rank != 0) &&
            (arg_info_list[info_idx2].ed.rank != 
             (arg_info_list[info_idx1].ed.rank - 1))) {
      PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
               arg_info_list[info_idx2].col);
   }

   if (list_idx3 != NULL_IDX && IL_IDX(list_idx3) != NULL_IDX) {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);

      if (arg_info_list[info_idx3].ed.rank != 0) {
         PRINTMSG(arg_info_list[info_idx3].line, 654,  Error, 
                  arg_info_list[info_idx3].col);
      }

      if (IL_FLD(list_idx3) == CN_Tbl_Idx) {
         if (compare_cn_and_value(IL_IDX(list_idx3), 
                                  (long) arg_info_list[info_idx1].ed.rank,
                                  Gt_Opr) ||
             compare_cn_and_value(IL_IDX(list_idx3), 1, Lt_Opr)) {

            PRINTMSG(arg_info_list[info_idx3].line, 1017, Error, 
                     arg_info_list[info_idx3].col);
         }
      }
#ifdef KEY /* Bug 10410 */
      else if (NULL_IDX != is_optional_dummy(list_idx3)) {
	cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ? 
	  CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
	pass_dummy_or_default_const(list_idx3, cn_idx, TRUE);
      }
#endif /* KEY Bug 10410 */
   }
   else {  /* DIM is not present */

      cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ? 
               CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
      IL_FLD(list_idx3) = CN_Tbl_Idx;
      IL_IDX(list_idx3) = cn_idx;
      IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx3) = IR_COL_NUM(ir_idx);

      arg_info_list_base = arg_info_list_top;
      arg_info_list_top = arg_info_list_base + 1;

      if (arg_info_list_top >= arg_info_list_size) {
         enlarge_info_list_table();
      }

      IL_ARG_DESC_IDX(list_idx3) = arg_info_list_top;
      arg_info_list[arg_info_list_top] = init_arg_info;
      arg_info_list[arg_info_list_top].ed.type_idx = INTEGER_DEFAULT_TYPE;
      arg_info_list[arg_info_list_top].ed.type = Integer;
      arg_info_list[arg_info_list_top].ed.linear_type = INTEGER_DEFAULT_TYPE;
      arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
      arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);

      info_idx3 = IL_ARG_DESC_IDX(list_idx3);
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (list_idx3 != NULL_IDX && 
       IL_IDX(list_idx3) != NULL_IDX &&
       IL_FLD(list_idx3) == CN_Tbl_Idx) {
# ifdef _INLINE_INTRINSICS
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif
   }
# endif

   COPY_OPND(opnd, IL_OPND(list_idx3));
   cast_to_cg_default(&opnd, &(arg_info_list[info_idx3].ed));
   COPY_OPND(IL_OPND(list_idx3), opnd);


   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   COPY_OPND(res_exp_desc->shape[0], arg_info_list[info_idx1].ed.shape[0]);
   COPY_OPND(res_exp_desc->shape[1], arg_info_list[info_idx1].ed.shape[1]);
   COPY_OPND(res_exp_desc->shape[2], arg_info_list[info_idx1].ed.shape[2]);
   COPY_OPND(res_exp_desc->shape[3], arg_info_list[info_idx1].ed.shape[3]);
   COPY_OPND(res_exp_desc->shape[4], arg_info_list[info_idx1].ed.shape[4]);
   COPY_OPND(res_exp_desc->shape[5], arg_info_list[info_idx1].ed.shape[5]);
   COPY_OPND(res_exp_desc->shape[6], arg_info_list[info_idx1].ed.shape[6]);

   COPY_OPND(res_exp_desc->char_len, arg_info_list[info_idx1].ed.char_len);

   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      io_item_must_flatten = TRUE;
      IR_OPR(ir_idx) = Cshift_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "cshift_intrinsic", NULL);

}  /* cshift_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    EOSHIFT(ARRAY, SHIFT, BOUNDARY, DIM) intrinsic.           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    eoshift_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   long_type      cnst[MAX_WORDS_FOR_INTEGER];
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            list_idx4;
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            info_idx4;
   int            input_type_idx;
   int            output_type_idx;
   int            cn_idx;
   opnd_type	  opnd;


   TRACE (Func_Entry, "eoshift_intrinsic", NULL);

# ifdef _INLINE_INTRINSICS
   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   list_idx4 = IL_NEXT_LIST_IDX(list_idx3);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   if ((arg_info_list[info_idx1].ed.rank == 1) &&
       (arg_info_list[info_idx2].ed.rank != 0)) {
      PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
               arg_info_list[info_idx2].col);
   }
   else {
      if ((arg_info_list[info_idx2].ed.rank != 0) &&
          (arg_info_list[info_idx2].ed.rank != 
           (arg_info_list[info_idx1].ed.rank - 1))) {
         PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
                  arg_info_list[info_idx2].col);
      }
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);


   if (list_idx3 != NULL_IDX && IL_IDX(list_idx3) != NULL_IDX) {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);

      if (TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx) !=
          TYP_LINEAR(arg_info_list[info_idx3].ed.type_idx)) {
         PRINTMSG(arg_info_list[info_idx3].line, 727, Error,
                  arg_info_list[info_idx3].col);
      }

      if ((arg_info_list[info_idx1].ed.rank == 1) &&
          (arg_info_list[info_idx3].ed.rank != 0)) {
         PRINTMSG(arg_info_list[info_idx3].line, 654,  Error, 
                  arg_info_list[info_idx3].col);
      }
      else {
         if ((arg_info_list[info_idx3].ed.rank != 0) &&
             (arg_info_list[info_idx3].ed.rank != 
              (arg_info_list[info_idx1].ed.rank - 1))) {
            PRINTMSG(arg_info_list[info_idx3].line, 654,  Error, 
                     arg_info_list[info_idx3].col);
         }
      }
#ifdef KEY /* Bug 10410 */
      if (NULL_IDX != is_optional_dummy(list_idx3)) {
        int idx3_type_idx = arg_info_list[info_idx3].ed.type_idx;
	basic_type_type idx3_type = arg_info_list[info_idx3].ed.type;
	/* Doesn't seem to be a systematic way of creating these constants.
	 * Integer and real parse uses kludge_input_conversion(); logical uses
	 * set_up_logical_constant(); complex does something weird to fuse
	 * two integer or real constants inside parse_operand(). At the
	 * risk of assuming that bit pattern zero is appropriate for all
	 * of these types, we do it the easy way. */
	switch (idx3_type) {
	  case Integer:
	  case Real:
	  case Complex:
	  case Logical: {
	    long_type constant[MAX_WORDS_FOR_NUMERIC];
	    memset(constant, 0, MAX_WORDS_FOR_NUMERIC * TARGET_BYTES_PER_WORD);
	    cn_idx = ntr_const_tbl(idx3_type_idx, TRUE, constant);
	    pass_dummy_or_default_const(list_idx3, cn_idx, TRUE);
	    }
	    break;
	  case Character: {
	    ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE; 
	    /*
	    cn_idx = ntr_const_tbl(idx3_type_idx, TRUE, NULL_IDX);
	    long *cp = &CN_CONST(cn_idx);
	    size_t byte_len = (size_t) CN_INT_TO_C(idx3_type_idx);
	    memset(cp, ' ', byte_len);
	    */
	    }
	    break;
	  }
      }
#endif /* KEY Bug 10410 */
   }
   else {  /* boundary not present */
      switch (arg_info_list[info_idx1].ed.type) {
         case Structure :  
              PRINTMSG(arg_info_list[info_idx1].line, 888,  Error, 
                       arg_info_list[info_idx1].col);
              break;

         case Integer :  

              cn_idx = (TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx) ==
                           CG_INTEGER_DEFAULT_TYPE) ? CN_INTEGER_ZERO_IDX :
                           C_INT_TO_CN(arg_info_list[info_idx1].ed.type_idx, 0);

              IL_FLD(list_idx3) = CN_Tbl_Idx;
              IL_IDX(list_idx3) = cn_idx;
              IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
              IL_COL_NUM(list_idx3) = IR_COL_NUM(ir_idx);

              arg_info_list_base = arg_info_list_top;
              arg_info_list_top = arg_info_list_base + 1;

              if (arg_info_list_top >= arg_info_list_size) {
                 enlarge_info_list_table();
              }

              IL_ARG_DESC_IDX(list_idx3) = arg_info_list_top;
              arg_info_list[arg_info_list_top] = init_arg_info;
              arg_info_list[arg_info_list_top].ed.type_idx = 
                 arg_info_list[info_idx1].ed.type_idx;
              arg_info_list[arg_info_list_top].ed.type = Integer;
              arg_info_list[arg_info_list_top].ed.linear_type = 
                 TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx);
              arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
              arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);

              info_idx3 = IL_ARG_DESC_IDX(list_idx3);
              break;

         case Real :  
              output_type_idx = arg_info_list[info_idx1].ed.type_idx;
              input_type_idx = CG_INTEGER_DEFAULT_TYPE;

              if (folder_driver((char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                                input_type_idx,
                                NULL,
                                NULL_IDX,
                                folded_const,
                                &output_type_idx,
                                IR_LINE_NUM(ir_idx),
                                IR_COL_NUM(ir_idx),
                                1,
                                Cvrt_Opr)) {
              }

              cn_idx = ntr_const_tbl(output_type_idx,
                                     FALSE,
                                     folded_const);

              IL_FLD(list_idx3) = CN_Tbl_Idx;
              IL_IDX(list_idx3) = cn_idx;
              IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
              IL_COL_NUM(list_idx3) = IR_COL_NUM(ir_idx);

              arg_info_list_base = arg_info_list_top;
              arg_info_list_top = arg_info_list_base + 1;

              if (arg_info_list_top >= arg_info_list_size) {
                 enlarge_info_list_table();
              }

              IL_ARG_DESC_IDX(list_idx3) = arg_info_list_top;
              arg_info_list[arg_info_list_top] = init_arg_info;
              arg_info_list[arg_info_list_top].ed.type_idx =
                 arg_info_list[info_idx1].ed.type_idx;
              arg_info_list[arg_info_list_top].ed.type = Real;
              arg_info_list[arg_info_list_top].ed.linear_type =
                 TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx);
              arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
              arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);
              break;

         case Complex :  
              output_type_idx = arg_info_list[info_idx1].ed.type_idx;
              input_type_idx = CG_INTEGER_DEFAULT_TYPE;

              if (folder_driver((char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                                input_type_idx,
                                NULL,
                                NULL_IDX,
                                folded_const,
                                &output_type_idx,
                                IR_LINE_NUM(ir_idx),
                                IR_COL_NUM(ir_idx),
                                1,
                                Cvrt_Opr)) {
              }

              cn_idx = ntr_const_tbl(output_type_idx,
                                     FALSE,
                                     folded_const);

              IL_FLD(list_idx3) = CN_Tbl_Idx;
              IL_IDX(list_idx3) = cn_idx;
              IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
              IL_COL_NUM(list_idx3) = IR_COL_NUM(ir_idx);

              arg_info_list_base = arg_info_list_top;
              arg_info_list_top = arg_info_list_base + 1;

              if (arg_info_list_top >= arg_info_list_size) {
                 enlarge_info_list_table();
              }

              IL_ARG_DESC_IDX(list_idx3) = arg_info_list_top;
              arg_info_list[arg_info_list_top] = init_arg_info;
              arg_info_list[arg_info_list_top].ed.type_idx =
                 arg_info_list[info_idx1].ed.type_idx;
              arg_info_list[arg_info_list_top].ed.type = Complex;
              arg_info_list[arg_info_list_top].ed.linear_type =
                 TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx);
              arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
              arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);
              break;

         case Logical :  
              cn_idx = set_up_logical_constant(cnst,
                                           arg_info_list[info_idx1].ed.type_idx,
                                           FALSE_VALUE,
                                           TRUE);
              IL_FLD(list_idx3) = CN_Tbl_Idx;
              IL_IDX(list_idx3) = cn_idx;
              IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
              IL_COL_NUM(list_idx3) = IR_COL_NUM(ir_idx);

              arg_info_list_base = arg_info_list_top;
              arg_info_list_top = arg_info_list_base + 1;

              if (arg_info_list_top >= arg_info_list_size) {
                 enlarge_info_list_table();
              }

              IL_ARG_DESC_IDX(list_idx3) = arg_info_list_top;
              arg_info_list[arg_info_list_top] = init_arg_info;
              arg_info_list[arg_info_list_top].ed.type_idx =
                 arg_info_list[info_idx1].ed.type_idx;
              arg_info_list[arg_info_list_top].ed.type = Logical;
              arg_info_list[arg_info_list_top].ed.linear_type =
                 TYP_LINEAR(arg_info_list[info_idx1].ed.type_idx);
              arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
              arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);

              info_idx3 = IL_ARG_DESC_IDX(list_idx3);
              break;

         case Character :  
              ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE; 
              break;
      }
   }

   if (list_idx4 != NULL_IDX && IL_IDX(list_idx4) != NULL_IDX) {
      info_idx4 = IL_ARG_DESC_IDX(list_idx4);

      if (arg_info_list[info_idx4].ed.rank != 0) {
         PRINTMSG(arg_info_list[info_idx4].line, 654,  Error, 
                  arg_info_list[info_idx4].col);
      }

      if (IL_FLD(list_idx4) == CN_Tbl_Idx) {
         if (compare_cn_and_value(IL_IDX(list_idx4),
                                  (long) arg_info_list[info_idx1].ed.rank,
                                  Gt_Opr) ||
             compare_cn_and_value(IL_IDX(list_idx4), 1, Lt_Opr)) {

            PRINTMSG(arg_info_list[info_idx4].line, 1017, Error,
                     arg_info_list[info_idx4].col);
         }
      }
#ifdef KEY /* Bug 10410 */
      else if (NULL_IDX != is_optional_dummy(list_idx4)) {
	cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ? 
	  CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
	pass_dummy_or_default_const(list_idx4, cn_idx, TRUE);
      }
#endif /* KEY Bug 10410 */
   }
   else {  /* DIM is not present */

      cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?  
                CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
      IL_FLD(list_idx4) = CN_Tbl_Idx;
      IL_IDX(list_idx4) = cn_idx;
      IL_LINE_NUM(list_idx4) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx4) = IR_COL_NUM(ir_idx);

      arg_info_list_base = arg_info_list_top;
      arg_info_list_top = arg_info_list_base + 1;

      if (arg_info_list_top >= arg_info_list_size) {
         enlarge_info_list_table();
      }

      IL_ARG_DESC_IDX(list_idx4) = arg_info_list_top;
      arg_info_list[arg_info_list_top] = init_arg_info;
      arg_info_list[arg_info_list_top].ed.type_idx = INTEGER_DEFAULT_TYPE;
      arg_info_list[arg_info_list_top].ed.type = Integer;
      arg_info_list[arg_info_list_top].ed.linear_type = INTEGER_DEFAULT_TYPE;
      arg_info_list[arg_info_list_top].line = IR_LINE_NUM(ir_idx);
      arg_info_list[arg_info_list_top].col = IR_COL_NUM(ir_idx);

      info_idx4 = IL_ARG_DESC_IDX(list_idx4);
   }

   if (IL_FLD(list_idx4) != CN_Tbl_Idx) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE; 
   }

   COPY_OPND(opnd, IL_OPND(list_idx4));
   cast_to_cg_default(&opnd, &(arg_info_list[info_idx4].ed));
   COPY_OPND(IL_OPND(list_idx4), opnd);

   COPY_OPND(res_exp_desc->shape[0], arg_info_list[info_idx1].ed.shape[0]);
   COPY_OPND(res_exp_desc->shape[1], arg_info_list[info_idx1].ed.shape[1]);
   COPY_OPND(res_exp_desc->shape[2], arg_info_list[info_idx1].ed.shape[2]);
   COPY_OPND(res_exp_desc->shape[3], arg_info_list[info_idx1].ed.shape[3]);
   COPY_OPND(res_exp_desc->shape[4], arg_info_list[info_idx1].ed.shape[4]);
   COPY_OPND(res_exp_desc->shape[5], arg_info_list[info_idx1].ed.shape[5]);
   COPY_OPND(res_exp_desc->shape[6], arg_info_list[info_idx1].ed.shape[6]);

   COPY_OPND(res_exp_desc->char_len, arg_info_list[info_idx1].ed.char_len);

   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) { 
      io_item_must_flatten = TRUE;
      IR_OPR(ir_idx) = Eoshift_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "eoshift_intrinsic", NULL);

}  /* eoshift_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MINEXPONENT(X) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    minexponent_intrinsic(opnd_type     *result_opnd,
                              expr_arg_type *res_exp_desc,
                              int           *spec_idx)
{
   int            ir_idx;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */
   int            info_idx1;
   int            cn_idx;


   TRACE (Func_Entry, "minexponent_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           num = MINEXPONENT_REAL4_F90;
           break;

      case Real_8:
           num = MINEXPONENT_REAL8_F90;
           break;

      case Real_16:
           num = MINEXPONENT_REAL16_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "minexponent_intrinsic", NULL);

}  /* minexponent_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MAXEXPONENT(X) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    maxexponent_intrinsic(opnd_type     *result_opnd,
                              expr_arg_type *res_exp_desc,
                              int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;
   int            cn_idx;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "maxexponent_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           num = MAXEXPONENT_REAL4_F90;
           break;

      case Real_8:
           num = MAXEXPONENT_REAL8_F90;
           break;

      case Real_16:
           num = MAXEXPONENT_REAL16_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "maxexponent_intrinsic", NULL);

}  /* maxexponent_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    RADIX(X) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    radix_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;


   TRACE (Func_Entry, "radix_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, RADIX_F90);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "radix_intrinsic", NULL);

}  /* radix_intrinsic */

 
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    RANGE(X) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    range_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            info_idx1;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "range_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Complex_4:
           num = RANGE_REAL4_F90;
           break;

      case Complex_8:
           num = RANGE_REAL8_F90;
           break;

      case Complex_16:
           num = RANGE_REAL16_F90;
           break;

      case Real_4:
           num = RANGE_REAL4_F90;
           break;

      case Real_8:
           num = RANGE_REAL8_F90;
           break;

      case Real_16:
           num = RANGE_REAL16_F90;
           break;

      case Integer_1:
           num = RANGE_INT1_F90;
           break;

      case Integer_2:
           num = RANGE_INT2_F90;
           break;

      case Integer_4:
           num = RANGE_INT4_F90;
           break;

      case Integer_8:
           num = RANGE_INT8_F90;

# ifdef _TARGET_HAS_FAST_INTEGER
           if (opt_flags.set_allfastint_option ||
               (opt_flags.set_fastint_option &&
                (TYP_DESC(arg_info_list[info_idx1].ed.type_idx) ==
                                                           Default_Typed))) {
              num = 13;
           }
# endif

           break;
   }


   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "range_intrinsic", NULL);

}  /* range_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    PRECISION(X) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    precision_intrinsic(opnd_type     *result_opnd,
                            expr_arg_type *res_exp_desc,
                            int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            info_idx1;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "precision_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Complex_4:
           num = PRECISION_REAL4_F90;
           break;

      case Complex_8:
           num = PRECISION_REAL8_F90;
           break;

      case Complex_16:
           num = PRECISION_REAL16_F90;
           break;

      case Real_4:
           num = PRECISION_REAL4_F90;
           break;

      case Real_8:
           num = PRECISION_REAL8_F90;
           break;

      case Real_16:
           num = PRECISION_REAL16_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "precision_intrinsic", NULL);

}  /* precision_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    KIND(X) intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    kind_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            list_idx1;
   int            info_idx1;
#ifdef KEY /* Bug 10177 */
   long		  num = 0;
#else /* KEY Bug 10177 */
   long		  num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "kind_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   if (IL_FLD(list_idx1) == AT_Tbl_Idx) {
      AT_ARG_TO_KIND(IL_IDX(list_idx1)) = TRUE;
   }
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Complex_4:
           num = 4;
           break;

      case Complex_8:
           num = 8;
           break;

      case Complex_16:
           num = 16;
           break;

      case Real_4:
           num = 4;
           break;

      case Real_8:
           num = 8;
           break;

      case Real_16:
           num = 16;
           break;

      case Integer_1:
           num = 1;
           break;

      case Integer_2:
           num = 2;
           break;

      case Integer_4:
           num = 4;
           break;

      case Integer_8:
           num = 8;
           break;

      case Logical_1:
           num = 1;
           break;

      case Logical_2:
           num = 2;
           break;

      case Logical_4:
           num = 4;
           break;

      case Logical_8:
           num = 8;
           break;

      case Short_Char_Const:
           num = 1;
           break;

      case Character_1:
           num = 1;
           break;

      case Character_2:
           num = 2;
           break;

      case Character_4:
           num = 4;
           break;
   }

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   if (TYP_DESC(arg_info_list[info_idx1].ed.type_idx) == Default_Typed) {

      if (arg_info_list[info_idx1].ed.linear_type == 
                                   init_default_linear_type[Fortran_Double] ||
          (TYP_DP_HIT_ME(arg_info_list[info_idx1].ed.type_idx) &&
           arg_info_list[info_idx1].ed.linear_type ==
                                    half_linear_type[Fortran_Double])) {

         res_exp_desc->kind0D0seen = TRUE;
      }
      else if (arg_info_list[info_idx1].ed.linear_type == REAL_DEFAULT_TYPE &&
               ! TYP_DP_HIT_ME(arg_info_list[info_idx1].ed.type_idx)) {

          res_exp_desc->kind0E0seen = TRUE;
      }
      else if (arg_info_list[info_idx1].ed.linear_type == 
                                                   INTEGER_DEFAULT_TYPE ||
               arg_info_list[info_idx1].ed.linear_type == 
                                                   LOGICAL_DEFAULT_TYPE)  {

          res_exp_desc->kind0seen = TRUE;
      }
      else {
          res_exp_desc->kindnotconst = TRUE;
      }
   }
      
          

   TRACE (Func_Exit, "kind_intrinsic", NULL);

}  /* kind_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    BIT_SIZE(I) intrinsic.                                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    bit_size_intrinsic(opnd_type     *result_opnd,
                           expr_arg_type *res_exp_desc,
                           int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            info_idx1;
#ifdef KEY /* Bug 10177 */
   long           num = 0;
#else /* KEY Bug 10177 */
   long           num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "bit_size_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Integer_1:
           num = BITSIZE_INT1_F90;
           break;

      case Integer_2:
           num = BITSIZE_INT2_F90;
           break;

      case Integer_4:
           num = BITSIZE_INT4_F90;
           break;

      case Integer_8:
           num = BITSIZE_INT8_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(arg_info_list[info_idx1].ed.type_idx, num);

   OPND_IDX((*result_opnd)) = cn_idx;
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
   OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
   OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
   res_exp_desc->constant = TRUE;
   res_exp_desc->foldable = TRUE;

   TRACE (Func_Exit, "bit_size_intrinsic", NULL);

}  /* bit_size_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LBOUND(ARRAY, DIM) intrinsic.                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    lbound_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
#ifdef KEY /* Bug 10177 */
   int            select = 0;
#else /* KEY Bug 10177 */
   int            select;
#endif /* KEY Bug 10177 */
   int		  asg_idx;
   int		  attr_idx	= NULL_IDX;
   int		  subscript_idx;
   long64	  bit_length;
   int		  constant_type_idx;
   long		  dim;
   int            arg1;
   int            arg2;
   int            arg3;
   int            ir_idx;
   int            il_idx;
   int            le_idx;
   int            eq_idx;
   int            array_attr;
   boolean        ok;
   int            i;
   int		  idx;
   int		  idx2;
   int            bd_idx;
   int            new_idx;
   int            cn_idx;
   opnd_type	  opnd;
   opnd_type	  base_opnd;
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;
   int            line;
   int            col;
   boolean	  make_const_tmp = FALSE;
   int		  the_cn_idx;
#ifdef KEY /* Bug 10177 */
   int		  tmp_idx = 0;
#else /* KEY Bug 10177 */
   int		  tmp_idx;
#endif /* KEY Bug 10177 */
   expr_arg_type  loc_exp_desc;
   int            expr_IDX[MAX_NUM_DIMS];
   fld_type       expr_FLD[MAX_NUM_DIMS];
   int		  save_arg3;
# ifdef _WHIRL_HOST64_TARGET64
   int            const_array[MAX_NUM_DIMS];
# else
   long_type   	  const_array[MAX_NUM_DIMS];
# endif /* _WHIRL_HOST64_TARGET64 */
   long64     	  host_array[MAX_NUM_DIMS];


   TRACE (Func_Entry, "lbound_intrinsic", NULL);

   for (i = 0; i < MAX_NUM_DIMS; i++) {
      expr_IDX[i]	= NULL_IDX;
      expr_FLD[i]	= NO_Tbl_Idx;
      host_array[i]	= 0;
   }

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   if (arg_info_list[info_idx1].ed.reference) {
      attr_idx = find_base_attr(&IL_OPND(list_idx1), &line, &col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);


   /* assume these for now */
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   if (arg_info_list[info_idx1].ed.rank == 0) {
      PRINTMSG(arg_info_list[info_idx1].line, 640, Error,
               arg_info_list[info_idx1].col);
   }

   if (list_idx2 != NULL_IDX && IL_IDX(list_idx2) != NULL_IDX) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);

      if (IL_FLD(list_idx2) == CN_Tbl_Idx &&
          (compare_cn_and_value(IL_IDX(list_idx2), 1, Lt_Opr) ||
           compare_cn_and_value(IL_IDX(list_idx2),
                                (long) arg_info_list[info_idx1].ed.rank,
                                Gt_Opr))) {

         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx2),
                                   &line,
                                   &col);
         PRINTMSG(line, 1012, Error, col);
         goto EXIT;
      }

      if (arg_info_list[info_idx2].ed.rank != 0) {
         PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
                  arg_info_list[info_idx2].col);
         goto EXIT;
      }

      res_exp_desc->rank = 0;

      if (arg_info_list[info_idx2].ed.reference) {
         attr_idx = find_base_attr(&IL_OPND(list_idx2), &line, &col);

         if (AT_OPTIONAL(attr_idx)) {
            PRINTMSG(arg_info_list[info_idx2].line, 875, Error,
                     arg_info_list[info_idx2].col);
         }
      }

      if (IL_FLD(list_idx2) == CN_Tbl_Idx) { /* DIM is a constant */

         dim = (long) CN_INT_TO_C(IL_IDX(list_idx2));

         if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
             (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
              (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
               IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
               IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

            COPY_OPND(opnd, IL_OPND(list_idx1));
            array_attr = find_base_attr(&opnd, &line, &col);

            bd_idx = ATD_ARRAY_IDX(array_attr);

            /* find the whole_subscript for lower bound info */
            /* bounds entries don't have it for dope vectors */

            idx = IL_IDX(list_idx1);

            if (IR_OPR(idx) == Whole_Substring_Opr) {
               idx = IR_IDX_L(idx);
            }

            idx = IR_IDX_R(idx);	/* first dim IL */

            for (i = 1; i < dim; i++) {
               idx = IL_NEXT_LIST_IDX(idx);
            }
            idx = IL_IDX(idx);           /* sitting at Triplet_Opr */
            idx = IR_IDX_L(idx);         /* sitting at start value IL */

            if (arg_info_list[info_idx1].ed.shape[dim-1].fld == CN_Tbl_Idx) {

               if (compare_cn_and_value(
                    arg_info_list[info_idx1].ed.shape[dim-1].idx, 0, Le_Opr)) {

                  /* lbound of zero size dimension is 1 */

                  cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                                       CN_INTEGER_ONE_IDX : 
                                       C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);

                  OPND_IDX((*result_opnd)) = cn_idx;
                  OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
                  OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
                  OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
               }
               else {
                  /* copy lbound from triplet */
                  COPY_OPND((*result_opnd), IL_OPND(idx));
                  cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
                  res_exp_desc->type_idx = 
                  ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
                  res_exp_desc->linear_type = 
                      TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
               }

               if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
                  res_exp_desc->constant = TRUE;
                  res_exp_desc->foldable = TRUE;
               }
            }
            else {

               /* set up switch on the extent */

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;

               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;

               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               IR_OPR(ir_idx) = Cvmgt_Opr;
               IR_FLD_L(ir_idx) = IL_Tbl_Idx;
               IR_IDX_L(ir_idx) = arg1;
               IR_LIST_CNT_L(ir_idx) = 3;

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               /* clear out right side, used to be arg list */
               IR_OPND_R(ir_idx) = null_opnd;

               IL_FLD(arg1) = CN_Tbl_Idx;
               IL_IDX(arg1) = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

               COPY_OPND(IL_OPND(arg2), IL_OPND(idx));

               le_idx=gen_ir(OPND_FLD(arg_info_list[info_idx1].ed.shape[dim-1]),
                             OPND_IDX(arg_info_list[info_idx1].ed.shape[dim-1]),
                          Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = le_idx;
               IL_LINE_NUM(arg3) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg3) = IR_COL_NUM(ir_idx);
            }
         }
         else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
                  (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                   IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

            /* it is assumed size array */
            /* and whole array reference */

            if (IL_FLD(list_idx1) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list_idx1);
            }
            else {
               attr_idx = IR_IDX_L(IL_IDX(list_idx1));
            }

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
            bd_idx = ATD_ARRAY_IDX(attr_idx);

            if (dim == BD_RANK(bd_idx)) {
               OPND_IDX((*result_opnd)) = BD_LB_IDX(bd_idx, dim);
               OPND_FLD((*result_opnd)) = BD_LB_FLD(bd_idx, dim);
               OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
               OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);

               cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
               res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
               res_exp_desc->linear_type = 
                      TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));

               if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
                  res_exp_desc->constant = TRUE;
                  res_exp_desc->foldable = TRUE;
               }
            }
            else if (BD_XT_FLD(bd_idx, dim) == CN_Tbl_Idx) {

               if (compare_cn_and_value(BD_XT_IDX(bd_idx, dim), 0, Le_Opr)) {

                  /* lbound of zero size dimension is 1 */

                  cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                                       CN_INTEGER_ONE_IDX : 
                                       C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);

                  OPND_IDX((*result_opnd)) = cn_idx;
                  OPND_FLD((*result_opnd)) = CN_Tbl_Idx; 
                  OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
                  OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
               }
               else {
                  OPND_IDX((*result_opnd)) = BD_LB_IDX(bd_idx, dim);
                  OPND_FLD((*result_opnd)) = BD_LB_FLD(bd_idx, dim);
                  OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
                  OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
                  cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
                  res_exp_desc->type_idx = 
                      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
                  res_exp_desc->linear_type = 
                      TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
               }

               if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
                  res_exp_desc->constant = TRUE;
                  res_exp_desc->foldable = TRUE;
               }
            }
            else {

               /* set up switch on the extent */

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;

               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;

               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               IR_OPR(ir_idx) = Cvmgt_Opr;
               IR_FLD_L(ir_idx) = IL_Tbl_Idx;
               IR_IDX_L(ir_idx) = arg1;
               IR_LIST_CNT_L(ir_idx) = 3;

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               /* clear out right side, used to be arg list */
               IR_OPND_R(ir_idx) = null_opnd;

               IL_FLD(arg1) = CN_Tbl_Idx;
               IL_IDX(arg1) = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

               IL_FLD(arg2) = BD_LB_FLD(bd_idx, dim);
               IL_IDX(arg2) = BD_LB_IDX(bd_idx, dim);
               IL_LINE_NUM(arg2) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg2) = IR_COL_NUM(ir_idx);

               le_idx = gen_ir(BD_XT_FLD(bd_idx, dim), BD_XT_IDX(bd_idx, dim),
                           Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = le_idx;
            }
         }
         else if (arg_info_list[info_idx1].ed.section || 
                  ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
                   (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {

            /* lbound is always one for section */

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

            cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                                         CN_INTEGER_ONE_IDX : 
                                         C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
            OPND_IDX((*result_opnd)) = cn_idx;   
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
            res_exp_desc->constant = TRUE;
            res_exp_desc->foldable = TRUE;
         }
      }
      else {
         /* dim is present, but not constant */

         COPY_OPND(opnd, IL_OPND(list_idx2));
         cast_to_cg_default(&opnd, &(arg_info_list[info_idx2].ed));
         COPY_OPND(IL_OPND(list_idx2), opnd);

         if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
             (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
              (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
               IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
               IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {
            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

            COPY_OPND(opnd, IL_OPND(list_idx1));
            array_attr = find_base_attr(&opnd, &line, &col);

            bd_idx = ATD_ARRAY_IDX(array_attr);

            /* find the whole_subscript for lower bound info */
            /* bounds entries don't have it for dope vectors */

            idx = IL_IDX(list_idx1);

            if (IR_OPR(idx) == Whole_Substring_Opr) {
               idx = IR_IDX_L(idx);
            }

            il_idx = IR_IDX_R(idx);	/* first dim IL */
            idx = IL_IDX(il_idx);       /* sitting at Triplet_Opr */
            idx = IR_IDX_L(idx);        /* sitting at start value IL */

            OPND_FLD(base_opnd) = CN_Tbl_Idx;
            OPND_IDX(base_opnd) = CN_INTEGER_ZERO_IDX;
            OPND_LINE_NUM(base_opnd) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM(base_opnd) = IR_COL_NUM(ir_idx);

            for (i = 1; i <= arg_info_list[info_idx1].ed.rank; i++) {

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;
               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;
               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               select = gen_ir(IL_Tbl_Idx, arg1,
                           Cvmgt_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                            IR_COL_NUM(ir_idx),
                               NO_Tbl_Idx, NULL_IDX);

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               COPY_OPND(IL_OPND(arg1), IL_OPND(idx));
               il_idx = IL_NEXT_LIST_IDX(il_idx);
               idx = IL_IDX(il_idx);        /* sitting at Triplet_Opr */
               idx = IR_IDX_L(idx);         /* sitting at start value IL */

               COPY_OPND(IL_OPND(arg2), base_opnd);

               cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);

               eq_idx = gen_ir(IL_FLD(list_idx2), IL_IDX(list_idx2),
                           Eq_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, cn_idx);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = eq_idx;
               IL_LINE_NUM(arg3) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg3)  = IR_COL_NUM(ir_idx);

               OPND_FLD(base_opnd) = IR_Tbl_Idx;
               OPND_IDX(base_opnd) = select;
            }

            /* set up switch on the extent */

            NTR_IR_LIST_TBL(arg1);
            IL_ARG_DESC_VARIANT(arg1) = TRUE;

            NTR_IR_LIST_TBL(arg2);
            IL_ARG_DESC_VARIANT(arg2) = TRUE;

            NTR_IR_LIST_TBL(arg3);
            IL_ARG_DESC_VARIANT(arg3) = TRUE;

            /* link list together */
            IL_NEXT_LIST_IDX(arg1) = arg2;
            IL_NEXT_LIST_IDX(arg2) = arg3;

            IR_OPR(ir_idx) = Cvmgt_Opr;
            IR_FLD_L(ir_idx) = IL_Tbl_Idx;
            IR_IDX_L(ir_idx) = arg1;
            IR_LIST_CNT_L(ir_idx) = 3;

            /* set this flag so this opr is pulled off io lists */
            io_item_must_flatten = TRUE;

            /* clear out right side, used to be arg list */
            IR_OPND_R(ir_idx) = null_opnd;

            IL_FLD(arg1) = CN_Tbl_Idx;
            IL_IDX(arg1) = CN_INTEGER_ONE_IDX;
            IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
            IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

            IL_FLD(arg2) = IR_Tbl_Idx;
            IL_IDX(arg2) = select;

            save_arg3 = arg3;

            OPND_FLD(base_opnd) = CN_Tbl_Idx;
            OPND_IDX(base_opnd) = CN_INTEGER_ZERO_IDX;
            OPND_LINE_NUM(base_opnd) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM(base_opnd) = IR_COL_NUM(ir_idx);

            for (i = 1; i <= arg_info_list[info_idx1].ed.rank; i++) {

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;
               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;
               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               select = gen_ir(IL_Tbl_Idx, arg1,
                           Cvmgt_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                            IR_COL_NUM(ir_idx),
                               NO_Tbl_Idx, NULL_IDX);

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               COPY_OPND(IL_OPND(arg1),
                         arg_info_list[info_idx1].ed.shape[i-1]);
               COPY_OPND(IL_OPND(arg2), base_opnd);

               cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);

               eq_idx = gen_ir(IL_FLD(list_idx2), IL_IDX(list_idx2),
                           Eq_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, cn_idx);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = eq_idx;
               IL_LINE_NUM(arg3) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg3)  = IR_COL_NUM(ir_idx);

               OPND_FLD(base_opnd) = IR_Tbl_Idx;
               OPND_IDX(base_opnd) = select; 
            }

            le_idx = gen_ir(IR_Tbl_Idx, select,
                        Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                      IR_COL_NUM(ir_idx),
                            CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

            IL_FLD(save_arg3) = IR_Tbl_Idx;
            IL_IDX(save_arg3) = le_idx;
         }
         else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
                  (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                   IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

            /* it is assumed size array */
            /* and whole array reference */

            /* this case will still go to an external library call */
            ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
         }
         else if (arg_info_list[info_idx1].ed.section || 
                  ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
                   (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {
            /* lbound is always one for section */
            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

            cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                                         CN_INTEGER_ONE_IDX : 
                                         C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);

            OPND_IDX((*result_opnd)) = cn_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
            res_exp_desc->constant = TRUE;
            res_exp_desc->foldable = TRUE;
         }
      }
   }
   else { /* DIM is not present */

      res_exp_desc->shape[0].fld = CN_Tbl_Idx;
      res_exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                               res_exp_desc->rank);
      SHAPE_WILL_FOLD_LATER(res_exp_desc->shape[0]) = TRUE;
      SHAPE_FOLDABLE(res_exp_desc->shape[0]) = TRUE;

      res_exp_desc->rank = 1;

      if (IR_LIST_CNT_R(ir_idx) == 1) {
         IR_LIST_CNT_R(ir_idx) = 2;
         NTR_IR_LIST_TBL(new_idx);
         IL_INTRIN_PLACE_HOLDER(new_idx) = TRUE;
         IL_ARG_DESC_VARIANT(new_idx) = TRUE;
         IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)) = new_idx;
      }


      if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
          (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
           (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
            IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
            IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {


         COPY_OPND(opnd, IL_OPND(list_idx1));
         array_attr = find_base_attr(&opnd, &line, &col);

         bd_idx = ATD_ARRAY_IDX(array_attr);

         /* find the whole_subscript for lower bound info */
         /* bounds entries don't have it for dope vectors */

         idx = IL_IDX(list_idx1);

         if (IR_OPR(idx) == Whole_Substring_Opr) {
            idx = IR_IDX_L(idx);
         }

         idx = IR_IDX_R(idx);        /* first dim IL */

         res_exp_desc->will_fold_later = TRUE;

         for (i = 0; i < BD_RANK(bd_idx); i++) {

            idx2 = IL_IDX(idx);           /* sitting at Triplet_Opr */
            idx2 = IR_IDX_L(idx2);        /* sitting at start value IL */

            if (arg_info_list[info_idx1].ed.shape[i].fld != CN_Tbl_Idx ||
                IL_FLD(idx2) != CN_Tbl_Idx) {

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;
               IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);
               IL_FLD(arg1) = CN_Tbl_Idx;

               /* lbound of zero size dimension is 1 */

               cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                                         CN_INTEGER_ONE_IDX : 
                                         C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);

               IL_IDX(arg1) = cn_idx;

               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;
               IL_LINE_NUM(arg2) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg2) = IR_COL_NUM(ir_idx);
               IL_FLD(arg2) = IL_FLD(idx2);
               IL_IDX(arg2) = IL_IDX(idx2);

               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               le_idx = gen_ir(OPND_FLD(arg_info_list[info_idx1].ed.shape[i]),
                               OPND_IDX(arg_info_list[info_idx1].ed.shape[i]),
                           Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = le_idx;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               select = gen_ir(IL_Tbl_Idx, arg1,
                           Cvmgt_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                            IR_COL_NUM(ir_idx),
                               NO_Tbl_Idx, NULL_IDX);

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               expr_IDX[i] = select;
               expr_FLD[i] = IR_Tbl_Idx;
               host_array[i] = 0;
            }
            else if (compare_cn_and_value(arg_info_list[info_idx1].ed.
                                                        shape[i].idx,
                                          0,
                                          Le_Opr)) {
               host_array[i] = 1;
            }
            else {
               host_array[i] = CN_INT_TO_C(IL_IDX(idx2));
            }

            idx = IL_NEXT_LIST_IDX(idx);
         }
      }
      else if (arg_info_list[info_idx1].ed.section ||
          ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
           (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {

         res_exp_desc->will_fold_later = TRUE;

         for (i = 0; i < arg_info_list[info_idx1].ed.rank; i++) {
            host_array[i] = 1;
         }
      }
      else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
               (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

         /* it is assumed size array */

         if (IL_FLD(list_idx1) == AT_Tbl_Idx) {
            attr_idx = IL_IDX(list_idx1);
         }
         else {
            attr_idx = IR_IDX_L(IL_IDX(list_idx1));
         }

         bd_idx = ATD_ARRAY_IDX(attr_idx);

         res_exp_desc->will_fold_later = TRUE;

         for (i = 1; i < BD_RANK(bd_idx); i++) {

            if (BD_LB_FLD(bd_idx, i) != CN_Tbl_Idx ||
                BD_XT_FLD(bd_idx, i) != CN_Tbl_Idx) {

               res_exp_desc->will_fold_later = FALSE;
               break;
            }
            else if (compare_cn_and_value(BD_XT_IDX(bd_idx, i), 0, Le_Opr)) {
               host_array[(i-1)] = 1;
            }
            else {
               host_array[(i-1)] = CN_INT_TO_C(BD_LB_IDX(bd_idx,i));
            }
         }

         if (BD_LB_FLD(bd_idx, BD_RANK(bd_idx)) != CN_Tbl_Idx) {
            res_exp_desc->will_fold_later = FALSE;
         }
         else {
            host_array[(BD_RANK(bd_idx)-1)] = CN_INT_TO_C(
                                           BD_LB_IDX(bd_idx, BD_RANK(bd_idx)));
         }
      }

      if (res_exp_desc->will_fold_later) {
         make_const_tmp = TRUE;
      }
   }

   if (make_const_tmp) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
      bit_length = TARGET_BITS_PER_WORD * arg_info_list[info_idx1].ed.rank;
# ifdef _WHIRL_HOST64_TARGET64
      bit_length >>= 1;
# endif /* _WHIRL_HOST64_TARGET64 */

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX) = Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= bit_length;
      constant_type_idx	= ntr_type_tbl();

      for (i = 0; i < MAX_NUM_DIMS; i++) {

# if defined(_TARGET32)

         /* Make sure that if Integer_8 is default that */
         /* the values still fit in the long container. */

         if (INTEGER_DEFAULT_TYPE == Integer_8) {
            /* JEFFL - Need overflow check here for each array entry */

         }
# endif
         /* JEFFL - This needs to be converted from host to */
         /*         target if we decide that is necessary.  */

         const_array[i] = (long_type) host_array[i];
      }

      the_cn_idx = ntr_const_tbl(constant_type_idx,
                                 FALSE,
                                 const_array);


      tmp_idx = gen_compiler_tmp(IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx),
                                 Shared, TRUE);

      AT_SEMANTICS_DONE(tmp_idx) = TRUE;
      ATD_TYPE_IDX(tmp_idx) = CG_INTEGER_DEFAULT_TYPE;

      loc_exp_desc = *res_exp_desc;
      loc_exp_desc.type_idx = CG_INTEGER_DEFAULT_TYPE;
      loc_exp_desc.type = Integer;
      loc_exp_desc.linear_type = CG_INTEGER_DEFAULT_TYPE;

      ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(&loc_exp_desc,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));

      ATD_SAVED(tmp_idx) = TRUE;
      ATD_DATA_INIT(tmp_idx) = TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      ATD_FLD(tmp_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(tmp_idx) = the_cn_idx;
      ATD_TMP_INIT_NOT_DONE(tmp_idx) = TRUE;

      OPND_IDX((*result_opnd)) = tmp_idx;
      OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);

      ok = gen_whole_subscript(result_opnd, res_exp_desc);

      if (CG_INTEGER_DEFAULT_TYPE != INTEGER_DEFAULT_TYPE) {
         cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);

         ok = fold_aggragate_expression(result_opnd,
                                          res_exp_desc,
                                          FALSE);  

         if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx) {
            idx = OPND_IDX((*result_opnd));
            if (IR_FLD_L(idx) == AT_Tbl_Idx) {
               tmp_idx = IR_IDX_L(idx);
            }
         }
      }

      AT_REFERENCED(tmp_idx) = Referenced;
      AT_DEFINED(tmp_idx) = TRUE;

      res_exp_desc->foldable = TRUE;
      res_exp_desc->tmp_reference = TRUE;
   }

   /* This for loop generates individual assignment statements */
   /* in the IR stream to update those elements of the result  */
   /* array that are runtime values.                           */
   for (i = 0; i < MAX_NUM_DIMS; i++) {
      if (expr_IDX[i] != NULL_IDX) {
         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;

         NTR_IR_LIST_TBL(idx);
         IL_FLD(idx) = CN_Tbl_Idx;

         IL_IDX(idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i+1);
         IL_LINE_NUM(idx) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(idx) = IR_COL_NUM(ir_idx);

         NTR_IR_TBL(subscript_idx);
         IR_TYPE_IDX(subscript_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_OPR(subscript_idx) = Subscript_Opr;
         IR_LINE_NUM(subscript_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM(subscript_idx) = IR_COL_NUM(ir_idx);
         IR_FLD_L(subscript_idx) = AT_Tbl_Idx;
         IR_IDX_L(subscript_idx) = tmp_idx;
         IR_LINE_NUM_L(subscript_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_L(subscript_idx) = IR_COL_NUM(ir_idx);
         IR_FLD_R(subscript_idx) = IL_Tbl_Idx;
         IR_IDX_R(subscript_idx) = idx;
         IR_LINE_NUM_R(subscript_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(subscript_idx) = IR_COL_NUM(ir_idx);
         IR_LIST_CNT_R(subscript_idx) = 1;

         asg_idx = gen_ir(IR_Tbl_Idx, subscript_idx,
                      Asg_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                     IR_COL_NUM(ir_idx),
                          expr_FLD[i], expr_IDX[i]);

         gen_sh(Before,
             Assignment_Stmt,
             IR_LINE_NUM(ir_idx),
             IR_COL_NUM(ir_idx),
             FALSE,
             FALSE,
             TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }
   }



EXIT:

   if (OPND_FLD((*result_opnd)) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX((*result_opnd))) != Call_Opr) {

      cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
   }

   IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   TRACE (Func_Exit, "lbound_intrinsic", NULL);

} /* lbound_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    UBOUND(ARRAY, DIM) intrinsic.                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    ubound_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int		  asg_idx;
   int		  attr_idx	= NULL_IDX;
   int		  select;
   long64	  bit_length;
   int		  constant_type_idx;
   long 	  dim;
   int            arg1;
   int            arg2;
   int            arg3;
   int            ir_idx;
   int            il_idx;
   int            le_idx;
   int            eq_idx;
   int            array_attr;
# ifdef _WHIRL_HOST64_TARGET64
   int            const_array[MAX_NUM_DIMS];
# else
   long_type   	  const_array[MAX_NUM_DIMS];
# endif /* _WHIRL_HOST64_TARGET64 */
   long64     	  host_array[MAX_NUM_DIMS];
   int            expr_IDX[MAX_NUM_DIMS];
   fld_type       expr_FLD[MAX_NUM_DIMS];
   boolean        ok;
   int            idx;
   int            idx2;
   int            i;
   int            bd_idx;
   int            new_idx;
   int            cn_idx;
   opnd_type	  opnd;
   opnd_type	  base_opnd;
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;
   int            line;
   int            col;
   boolean	  make_const_tmp = FALSE;
   int		  the_cn_idx;
   int		  tmp_idx;
   int		  subscript_idx;
   expr_arg_type  loc_exp_desc;
   int            save_arg3;


   TRACE (Func_Entry, "ubound_intrinsic", NULL);

   for (i = 0; i < MAX_NUM_DIMS; i++) {
      expr_IDX[i] = NULL_IDX;
      expr_FLD[i] = NO_Tbl_Idx;
      host_array[i] = 0;
   }

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   if (arg_info_list[info_idx1].ed.reference) {
      attr_idx = find_base_attr(&IL_OPND(list_idx1), &line, &col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   /* assume these for now */
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   if (arg_info_list[info_idx1].ed.rank == 0) {
      PRINTMSG(arg_info_list[info_idx1].line, 640, Error,
               arg_info_list[info_idx1].col);
   }

   if (list_idx2 != NULL_IDX && IL_IDX(list_idx2) != NULL_IDX) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);

      if (IL_FLD(list_idx2) == CN_Tbl_Idx &&
          (compare_cn_and_value(IL_IDX(list_idx2), 1, Lt_Opr) ||
           compare_cn_and_value(IL_IDX(list_idx2),
                                (long) arg_info_list[info_idx1].ed.rank,
                                Gt_Opr))) {

         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx2),
                                   &line,
                                   &col);
         PRINTMSG(line, 1012, Error, col);
         goto EXIT;
      }


      if (arg_info_list[info_idx2].ed.rank != 0) {
         PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
                  arg_info_list[info_idx2].col);
         goto EXIT;
      }

      res_exp_desc->rank = 0;

      if (arg_info_list[info_idx2].ed.reference) {
         attr_idx = find_base_attr(&IL_OPND(list_idx2), &line, &col);

         if (AT_OPTIONAL(attr_idx)) {
            PRINTMSG(arg_info_list[info_idx2].line, 875, Error,
                     arg_info_list[info_idx2].col);
         }
      }

      if (IL_FLD(list_idx2) == CN_Tbl_Idx) { /* DIM is a constant */
         dim = (long) CN_INT_TO_C(IL_IDX(list_idx2));

         if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
             (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
              (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
               IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
               IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

            idx = IL_IDX(list_idx1);     /* sitting at Whole_Subscript_Opr */

            if (IR_OPR(idx) == Whole_Substring_Opr) {
               idx = IR_IDX_L(idx);
            }

            bd_idx = idx;
            idx = IR_IDX_R(idx);         /* sitting at first IL */

            COPY_OPND(opnd, IR_OPND_L(bd_idx));
            array_attr = find_base_attr(&opnd, &line, &col);

            bd_idx = ATD_ARRAY_IDX(array_attr);

            for (i = 1; i < dim; i++) {
               idx = IL_NEXT_LIST_IDX(idx); 
            }
            idx = IL_IDX(idx);           /* sitting at Triplet_Opr */
            idx = IR_IDX_L(idx);         /* sitting at start IL */
            idx = IL_NEXT_LIST_IDX(idx); /* sitting at finish IL */

            if (arg_info_list[info_idx1].ed.shape[dim-1].fld == CN_Tbl_Idx) {

               if (compare_cn_and_value(
                    arg_info_list[info_idx1].ed.shape[dim-1].idx, 0, Le_Opr)) {
                  /* ubound of zero size dim is 0 */

                  OPND_IDX((*result_opnd)) = (CG_INTEGER_DEFAULT_TYPE == 
                                                 INTEGER_DEFAULT_TYPE) ?
                                                 CN_INTEGER_ZERO_IDX :
                                           C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 0);
                  OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
                  OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
                  OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
               }
               else {
                  /* copy ubound from triplet */
                  COPY_OPND((*result_opnd), IL_OPND(idx));
               }

               if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
                  res_exp_desc->constant = TRUE;
                  res_exp_desc->foldable = TRUE;
               }
            }
            else {

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;

               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;

               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               IR_OPR(ir_idx) = Cvmgt_Opr;
               IR_FLD_L(ir_idx) = IL_Tbl_Idx;
               IR_IDX_L(ir_idx) = arg1;
               IR_LIST_CNT_L(ir_idx) = 3;

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               /* clear out right side, used to be arg list */
               IR_OPND_R(ir_idx) = null_opnd;

               IL_FLD(arg1) = CN_Tbl_Idx;
               IL_IDX(arg1) = CN_INTEGER_ZERO_IDX;
               IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg1)  = IR_COL_NUM(ir_idx);

               COPY_OPND(IL_OPND(arg2), IL_OPND(idx));

               le_idx=gen_ir(OPND_FLD(arg_info_list[info_idx1].ed.shape[dim-1]),
                             OPND_IDX(arg_info_list[info_idx1].ed.shape[dim-1]),
                           Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = le_idx;


            }
         }
         else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
                  (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                   IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

            /* it is assumed size array */

            if (IL_FLD(list_idx1) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list_idx1);
            }
            else {
               attr_idx = IR_IDX_L(IL_IDX(list_idx1));
            }

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
            bd_idx = ATD_ARRAY_IDX(attr_idx);

            if (compare_cn_and_value(IL_IDX(list_idx2), 
                                     (long) BD_RANK(bd_idx), 
                                     Eq_Opr)) {

               PRINTMSG(arg_info_list[info_idx1].line, 889, Error,
                        arg_info_list[info_idx1].col);
            }
            else if (BD_XT_FLD(bd_idx, dim) == CN_Tbl_Idx) {

               if (compare_cn_and_value(BD_XT_IDX(bd_idx, dim), 0, Le_Opr)) {
                  /* ubound of zero size dimension is 0 */
                  OPND_IDX((*result_opnd)) = CN_INTEGER_ZERO_IDX;
                  OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
                  OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
                  OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
               }
               else {
                  OPND_IDX((*result_opnd)) = BD_UB_IDX(bd_idx, dim);
                  OPND_FLD((*result_opnd)) = BD_UB_FLD(bd_idx, dim);
                  OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
                  OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
               }

               if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
                  res_exp_desc->constant = TRUE;
                  res_exp_desc->foldable = TRUE;
               }
            }
            else {

               /* set up switch on the extent */

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;

               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;

               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               IR_OPR(ir_idx) = Cvmgt_Opr;
               IR_FLD_L(ir_idx) = IL_Tbl_Idx;
               IR_IDX_L(ir_idx) = arg1;
               IR_LIST_CNT_L(ir_idx) = 3;

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               /* clear out right side, used to be arg list */
               IR_OPND_R(ir_idx) = null_opnd;

               IL_FLD(arg1) = CN_Tbl_Idx;
               IL_IDX(arg1) = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

               IL_FLD(arg2) = BD_UB_FLD(bd_idx, dim);
               IL_IDX(arg2) = BD_UB_IDX(bd_idx, dim);
               IL_LINE_NUM(arg2) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg2) = IR_COL_NUM(ir_idx);

               le_idx = gen_ir(BD_XT_FLD(bd_idx, dim), BD_XT_IDX(bd_idx, dim),
                           Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = le_idx;
            }
         }
         else if (arg_info_list[info_idx1].ed.section || 
                  ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
                   (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
            COPY_OPND((*result_opnd),  
                      arg_info_list[info_idx1].ed.shape[dim-1]);

            cast_opnd_to_type_idx(result_opnd, res_exp_desc->type_idx);

            if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
               res_exp_desc->constant = TRUE;
               res_exp_desc->foldable = TRUE;
            }
            else if (SHAPE_WILL_FOLD_LATER((*result_opnd)) ||
                     SHAPE_FOLDABLE((*result_opnd)))       {

               res_exp_desc->will_fold_later = TRUE;
            }

            /* clear the two shape flags on the result opnd */
            SHAPE_FOLDABLE((*result_opnd)) = FALSE;
            SHAPE_WILL_FOLD_LATER((*result_opnd)) = FALSE;
         }
      }
      else {
         /* dim is present, but not constant */

         COPY_OPND(opnd, IL_OPND(list_idx2));
         cast_to_cg_default(&opnd, &(arg_info_list[info_idx2].ed));
         COPY_OPND(IL_OPND(list_idx2), opnd);

         if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
             (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
              (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
               IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
               IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

            idx = IL_IDX(list_idx1);     /* sitting at Whole_Subscript_Opr */

            if (IR_OPR(idx) == Whole_Substring_Opr) {
               idx = IR_IDX_L(idx);
            }

            bd_idx = idx;
            il_idx = IR_IDX_R(idx);         /* sitting at first IL */

            COPY_OPND(opnd, IR_OPND_L(bd_idx));
            array_attr = find_base_attr(&opnd, &line, &col);

            bd_idx = ATD_ARRAY_IDX(array_attr);

            idx = IL_IDX(il_idx);           /* sitting at Triplet_Opr */
            idx = IR_IDX_L(idx);            /* sitting at start IL */
            idx = IL_NEXT_LIST_IDX(idx);    /* sitting at finish IL */

            OPND_IDX(base_opnd) = CN_INTEGER_ZERO_IDX;
            OPND_FLD(base_opnd) = CN_Tbl_Idx;
            OPND_LINE_NUM(base_opnd) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM(base_opnd)  = IR_COL_NUM(ir_idx);

            for (i = 1; i <= arg_info_list[info_idx1].ed.rank; i++) {

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;
               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;
               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               select = gen_ir(IL_Tbl_Idx, arg1,
                           Cvmgt_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                            IR_COL_NUM(ir_idx),
                               NO_Tbl_Idx, NULL_IDX);

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               COPY_OPND(IL_OPND(arg1), IL_OPND(idx));
               il_idx = IL_NEXT_LIST_IDX(il_idx);
               idx = IL_IDX(il_idx);           /* sitting at Triplet_Opr */
               idx = IR_IDX_L(idx);            /* sitting at start IL */
               idx = IL_NEXT_LIST_IDX(idx);    /* sitting at finish IL */

               COPY_OPND(IL_OPND(arg2), base_opnd);

               cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);

               eq_idx = gen_ir(IL_FLD(list_idx2), IL_IDX(list_idx2),
                           Eq_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, cn_idx);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = eq_idx;

               OPND_FLD(base_opnd) = IR_Tbl_Idx;
               OPND_IDX(base_opnd) = select;
            }


            NTR_IR_LIST_TBL(arg1);
            IL_ARG_DESC_VARIANT(arg1) = TRUE;
            NTR_IR_LIST_TBL(arg2);
            IL_ARG_DESC_VARIANT(arg2) = TRUE;
            NTR_IR_LIST_TBL(arg3);
            IL_ARG_DESC_VARIANT(arg3) = TRUE;

            /* link list together */
            IL_NEXT_LIST_IDX(arg1) = arg2;
            IL_NEXT_LIST_IDX(arg2) = arg3;

            IR_OPR(ir_idx) = Cvmgt_Opr;
            IR_FLD_L(ir_idx) = IL_Tbl_Idx;
            IR_IDX_L(ir_idx) = arg1;
            IR_LIST_CNT_L(ir_idx) = 3;

            /* set this flag so this opr is pulled off io lists */
            io_item_must_flatten = TRUE;

            /* clear out right side, used to be arg list */
            IR_OPND_R(ir_idx) = null_opnd;

            IL_FLD(arg1) = CN_Tbl_Idx;
            IL_IDX(arg1) = CN_INTEGER_ZERO_IDX;
            IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
            IL_COL_NUM(arg1)  = IR_COL_NUM(ir_idx);

            IL_FLD(arg2) = IR_Tbl_Idx;
            IL_IDX(arg2) = select;

            save_arg3 = arg3;

            OPND_IDX(base_opnd) = CN_INTEGER_ZERO_IDX;
            OPND_FLD(base_opnd) = CN_Tbl_Idx;
            OPND_LINE_NUM(base_opnd) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM(base_opnd)  = IR_COL_NUM(ir_idx);

            for (i = 1; i <= arg_info_list[info_idx1].ed.rank; i++) {

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;
               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;
               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               select = gen_ir(IL_Tbl_Idx, arg1,
                           Cvmgt_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                            IR_COL_NUM(ir_idx),
                               NO_Tbl_Idx, NULL_IDX);

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               COPY_OPND(IL_OPND(arg1),
                         arg_info_list[info_idx1].ed.shape[i-1]);
               COPY_OPND(IL_OPND(arg2), base_opnd);

               cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);

               eq_idx = gen_ir(IL_FLD(list_idx2), IL_IDX(list_idx2),
                           Eq_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, cn_idx);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = eq_idx;

               OPND_FLD(base_opnd) = IR_Tbl_Idx;
               OPND_IDX(base_opnd) = select;
            }

            le_idx = gen_ir(IR_Tbl_Idx, select,
                        Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                      IR_COL_NUM(ir_idx),
                            CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

            IL_FLD(save_arg3) = IR_Tbl_Idx;
            IL_IDX(save_arg3) = le_idx;
         }
         else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
                  (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                   IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

            /* it is assumed size array */
            ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
         }
         else if (arg_info_list[info_idx1].ed.section || 
                  ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
                   (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {

            ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
         }
      }
   }
   else { /* DIM is not present */

      res_exp_desc->shape[0].fld = CN_Tbl_Idx;
      res_exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                               res_exp_desc->rank);
      SHAPE_WILL_FOLD_LATER(res_exp_desc->shape[0]) = TRUE;
      SHAPE_FOLDABLE(res_exp_desc->shape[0]) = TRUE;

      res_exp_desc->rank = 1;

      if (IR_LIST_CNT_R(ir_idx) == 1) {
         IR_LIST_CNT_R(ir_idx) = 2;
         NTR_IR_LIST_TBL(new_idx);
         IL_INTRIN_PLACE_HOLDER(new_idx) = TRUE;
         IL_ARG_DESC_VARIANT(new_idx) = TRUE;
         IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)) = new_idx;
      }


      /* UBOUND, one arg */

      if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
          (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
           (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
            IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
            IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {

         COPY_OPND(opnd, IL_OPND(list_idx1));
         array_attr = find_base_attr(&opnd, &line, &col);

         bd_idx = ATD_ARRAY_IDX(array_attr);

         /* find the whole_subscript for lower bound info */
         /* bounds entries don't have it for dope vectors */

         idx = IL_IDX(list_idx1);

         if (IR_OPR(idx) == Whole_Substring_Opr) {
            idx = IR_IDX_L(idx);
         }

         idx = IR_IDX_R(idx);        /* first dim IL */

         res_exp_desc->will_fold_later = TRUE;

         for (i = 0; i < BD_RANK(bd_idx); i++) {
            idx2 = IL_IDX(idx);           /* sitting at Triplet_Opr */
            idx2 = IR_IDX_L(idx2);        /* sitting at start value IL */
            idx2 = IL_NEXT_LIST_IDX(idx2);/* at finish value IL */

            if (arg_info_list[info_idx1].ed.shape[i].fld != CN_Tbl_Idx ||
                IL_FLD(idx2) != CN_Tbl_Idx) {

               NTR_IR_LIST_TBL(arg1);
               IL_ARG_DESC_VARIANT(arg1) = TRUE;
               IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);
               IL_FLD(arg1) = CN_Tbl_Idx;

               /* lbound of zero size dimension is 1 */

               cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 0);

               IL_IDX(arg1) = cn_idx;

               NTR_IR_LIST_TBL(arg2);
               IL_ARG_DESC_VARIANT(arg2) = TRUE;
               IL_LINE_NUM(arg2) = IR_LINE_NUM(ir_idx);
               IL_COL_NUM(arg2) = IR_COL_NUM(ir_idx);
               IL_FLD(arg2) = IL_FLD(idx2);
               IL_IDX(arg2) = IL_IDX(idx2);

               NTR_IR_LIST_TBL(arg3);
               IL_ARG_DESC_VARIANT(arg3) = TRUE;

               le_idx = gen_ir(OPND_FLD(arg_info_list[info_idx1].ed.shape[i]),
                               OPND_IDX(arg_info_list[info_idx1].ed.shape[i]),
                           Le_Opr, LOGICAL_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               IL_FLD(arg3) = IR_Tbl_Idx;
               IL_IDX(arg3) = le_idx;

               /* link list together */
               IL_NEXT_LIST_IDX(arg1) = arg2;
               IL_NEXT_LIST_IDX(arg2) = arg3;

               select = gen_ir(IL_Tbl_Idx, arg1,
                           Cvmgt_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                               NO_Tbl_Idx, NULL_IDX);

               /* set this flag so this opr is pulled off io lists */
               io_item_must_flatten = TRUE;

               expr_IDX[i] = select;
               expr_FLD[i] = IR_Tbl_Idx;
               host_array[i] = 0;
            }
            else if (compare_cn_and_value(
                arg_info_list[info_idx1].ed.shape[i].idx, 0, Le_Opr)) {
               host_array[i] = 0;
            }
            else {
               host_array[i] = (long_type) CN_INT_TO_C(IL_IDX(idx2));
            }

            idx = IL_NEXT_LIST_IDX(idx);
         }

         if (res_exp_desc->will_fold_later) {
            make_const_tmp = TRUE;
         }

      }
      else if (arg_info_list[info_idx1].ed.section ||
          ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
           (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {

         res_exp_desc->will_fold_later = TRUE;
         res_exp_desc->foldable = TRUE;

         for (i = 0; i < arg_info_list[info_idx1].ed.rank; i++) {
            if (arg_info_list[info_idx1].ed.shape[i].fld != CN_Tbl_Idx) {
               res_exp_desc->foldable = FALSE;
            }
            else {
               host_array[i] = (long_type) 
                        CN_INT_TO_C(arg_info_list[info_idx1].ed.shape[i].idx);
            }

            if (! SHAPE_WILL_FOLD_LATER(arg_info_list[info_idx1].ed.shape[i])) {
               res_exp_desc->will_fold_later = FALSE;
            }
         }

         if (res_exp_desc->foldable) {
            make_const_tmp = TRUE;
         }
      }
      else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
               (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

         /* it is assumed size array */
         PRINTMSG(arg_info_list[info_idx1].line, 889, Error,
                  arg_info_list[info_idx1].col);
      }
   }

   if (make_const_tmp) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
      bit_length = TARGET_BITS_PER_WORD* (long)arg_info_list[info_idx1].ed.rank;
# ifdef _WHIRL_HOST64_TARGET64
      bit_length >>= 1;
# endif /* _WHIRL_HOST64_TARGET64 */

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX) = Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= bit_length;
      constant_type_idx	 = ntr_type_tbl();

      for (i = 0; i < MAX_NUM_DIMS; i++) {

# if defined(_TARGET32)

         /* Make sure that if Integer_8 is default that */
         /* the values still fit in the long container. */

         if (INTEGER_DEFAULT_TYPE == Integer_8) {
            /* JEFFL - Need overflow check here for each array entry */

         }
# endif
         /* JEFFL - This needs to be converted from host to */
         /*         target if we decide that is necessary.  */

         const_array[i] = (long_type) host_array[i];
      }

      the_cn_idx = ntr_const_tbl(constant_type_idx,
                                 FALSE,
                                 const_array);

      tmp_idx = gen_compiler_tmp(IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx),
                                 Shared, TRUE);

      AT_SEMANTICS_DONE(tmp_idx)= TRUE;
      ATD_TYPE_IDX(tmp_idx) = CG_INTEGER_DEFAULT_TYPE;

      loc_exp_desc = *res_exp_desc;
      loc_exp_desc.type_idx = CG_INTEGER_DEFAULT_TYPE;
      loc_exp_desc.type = Integer;
      loc_exp_desc.linear_type = CG_INTEGER_DEFAULT_TYPE;

      ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(&loc_exp_desc,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));

      ATD_SAVED(tmp_idx) = TRUE;
      ATD_DATA_INIT(tmp_idx) = TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      ATD_FLD(tmp_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(tmp_idx) = the_cn_idx;
      ATD_TMP_INIT_NOT_DONE(tmp_idx) = TRUE;

      OPND_IDX((*result_opnd)) = tmp_idx;
      OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);

      ok = gen_whole_subscript(result_opnd, res_exp_desc);

      if (CG_INTEGER_DEFAULT_TYPE != INTEGER_DEFAULT_TYPE) {
         cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);

         ok = fold_aggragate_expression(result_opnd,
                                        res_exp_desc,
                                        FALSE);

         if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx) {
            idx = OPND_IDX((*result_opnd));
            if (IR_FLD_L(idx) == AT_Tbl_Idx) {
               tmp_idx = IR_IDX_L(idx);
            }
         }
      }

      AT_REFERENCED(tmp_idx) = Referenced;
      AT_DEFINED(tmp_idx) = TRUE;

      res_exp_desc->foldable = TRUE;
      res_exp_desc->tmp_reference = TRUE;
   }

   /* This for loop generates individual assignment statements */
   /* in the IR stream to update those elements of the result  */
   /* array that are runtime values.                           */
   for (i = 0; i < MAX_NUM_DIMS; i++) {
      if (expr_IDX[i] != NULL_IDX) {
         res_exp_desc->foldable = FALSE;
         res_exp_desc->will_fold_later = FALSE;

         NTR_IR_LIST_TBL(idx);
         IL_FLD(idx) = CN_Tbl_Idx;

         IL_IDX(idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i+1);

         IL_LINE_NUM(idx) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(idx) = IR_COL_NUM(ir_idx);

         NTR_IR_TBL(subscript_idx);
         IR_TYPE_IDX(subscript_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_OPR(subscript_idx) = Subscript_Opr;
         IR_LINE_NUM(subscript_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM(subscript_idx) = IR_COL_NUM(ir_idx);
         IR_FLD_L(subscript_idx) = AT_Tbl_Idx;
         IR_IDX_L(subscript_idx) = tmp_idx;
         IR_LINE_NUM_L(subscript_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_L(subscript_idx) = IR_COL_NUM(ir_idx);
         IR_FLD_R(subscript_idx) = IL_Tbl_Idx;
         IR_IDX_R(subscript_idx) = idx;
         IR_LINE_NUM_R(subscript_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(subscript_idx) = IR_COL_NUM(ir_idx);
         IR_LIST_CNT_R(subscript_idx) = 1;

         asg_idx = gen_ir(IR_Tbl_Idx, subscript_idx,
                      Asg_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                     IR_COL_NUM(ir_idx),
                          expr_FLD[i], expr_IDX[i]);

         gen_sh(Before,
             Assignment_Stmt,
             IR_LINE_NUM(ir_idx),
             IR_COL_NUM(ir_idx),
             FALSE,
             FALSE,
             TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }
   }


EXIT:

   if (OPND_FLD((*result_opnd)) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX((*result_opnd))) != Call_Opr) {

      cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
   }

   IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   TRACE (Func_Exit, "ubound_intrinsic", NULL);

} /* ubound_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SIZE(ARRAY, DIM) intrinsic.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    size_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   long		  dim;
   int            ir_idx;
   int            array_attr;
   int            attr_idx	= NULL_IDX;
   boolean        constant_result;
   int            idx1;
   int		  idx2;
   int            i;
   int            bd_idx;
   int            cn_idx;
   int            new_idx;
   opnd_type	  opnd;
   int            info_idx1;
   int            info_idx2;
   int            list_idx1;
   int            list_idx2;
   int            line;
   int            col;
   boolean	  result_will_fold;
   long64	  num;


   TRACE (Func_Entry, "size_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   if (arg_info_list[info_idx1].ed.reference) {
      attr_idx = find_base_attr(&IL_OPND(list_idx1), &line, &col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);


   /* assume these for now */
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   /* size result is scalar */
   res_exp_desc->rank = 0;

   if (arg_info_list[info_idx1].ed.rank == 0) {
      PRINTMSG(arg_info_list[info_idx1].line, 640, Error,
               arg_info_list[info_idx1].col);
   }

   if (list_idx2 != NULL_IDX &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       (compare_cn_and_value(IL_IDX(list_idx2), 1, Lt_Opr) ||
        compare_cn_and_value(IL_IDX(list_idx2),
                             (long) arg_info_list[info_idx1].ed.rank,
                             Gt_Opr))) {

      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx2),
                                &line,
                                &col);
      PRINTMSG(line, 1012, Error, col);
      goto EXIT;
   }

   if (list_idx2 != NULL_IDX && IL_IDX(list_idx2) != NULL_IDX) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);

      if (arg_info_list[info_idx2].ed.rank != 0) {
         PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
                  arg_info_list[info_idx2].col);
         goto EXIT;
      }

      res_exp_desc->rank = 0;

      if (arg_info_list[info_idx2].ed.reference) {
         attr_idx = find_base_attr(&IL_OPND(list_idx2), &line, &col);

         if (AT_OPTIONAL(attr_idx)) {
#ifdef KEY /* Bug 10410 */
            /* Standard allows actual argument to be optional dummy */
#else /* KEY Bug 10410 */
            PRINTMSG(arg_info_list[info_idx2].line, 875, Error,
                     arg_info_list[info_idx2].col);
#endif /* KEY Bug 10410 */
         }
      }

      if (IL_FLD(list_idx2) == CN_Tbl_Idx) { /* DIM is a constant */

         dim = (long) CN_INT_TO_C(IL_IDX(list_idx2));
         ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

         if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
             (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
              (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
               IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
               IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {

            COPY_OPND((*result_opnd),
                      arg_info_list[info_idx1].ed.shape[dim-1]);

            cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
            res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
            res_exp_desc->linear_type = 
               TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));

            if (SHAPE_WILL_FOLD_LATER((*result_opnd)) ||
                SHAPE_FOLDABLE((*result_opnd))) {
               res_exp_desc->will_fold_later = TRUE;
            }

            /* clear the two shape flags on the result opnd */
            SHAPE_FOLDABLE((*result_opnd)) = FALSE;
            SHAPE_WILL_FOLD_LATER((*result_opnd)) = FALSE;

            if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
               res_exp_desc->constant = TRUE;
               res_exp_desc->foldable = TRUE;
            }
         }
         else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
                  (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                   IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

            /* it is assumed size array */

            if (IL_FLD(list_idx1) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list_idx1);
            }
            else {
               attr_idx = IR_IDX_L(IL_IDX(list_idx1));
            }

            if (dim == arg_info_list[info_idx1].ed.rank) {
               PRINTMSG(arg_info_list[info_idx1].line, 889, Error,
                        arg_info_list[info_idx1].col);
            }
            else {
               OPND_FLD((*result_opnd)) = 
                       BD_XT_FLD(ATD_ARRAY_IDX(attr_idx), dim);
               OPND_IDX((*result_opnd)) = 
                       BD_XT_IDX(ATD_ARRAY_IDX(attr_idx), dim);
               OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
               OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);

               if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
                  res_exp_desc->constant = TRUE;
                  res_exp_desc->foldable = TRUE;
               }
            }
         }
         else if (arg_info_list[info_idx1].ed.section || 
                  ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
                   (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {
#ifdef KEY
// Bug 570
            if (arg_info_list[info_idx1].ed.shape[dim-1].fld == CN_Tbl_Idx){
              OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
              int const_idx = arg_info_list[info_idx1].ed.shape[dim-1].idx;
              if (CN_CONST (const_idx) > 0) 
                OPND_IDX((*result_opnd)) = const_idx;
              else
                OPND_IDX((*result_opnd)) = CN_INTEGER_ZERO_IDX;
            }
            else{
#endif
            NTR_IR_LIST_TBL(idx1);
            COPY_OPND(IL_OPND(idx1),
                      arg_info_list[info_idx1].ed.shape[dim-1]);

            NTR_IR_LIST_TBL(idx2);
            IL_NEXT_LIST_IDX(idx1) = idx2;
            IL_IDX(idx2) = CN_INTEGER_ZERO_IDX;
            IL_FLD(idx2) = CN_Tbl_Idx;
            IL_LINE_NUM(idx2) = IR_LINE_NUM(ir_idx);
            IL_COL_NUM(idx2)  = IR_COL_NUM(ir_idx);

            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
            IR_OPR(ir_idx) = Max_Opr;

            IR_IDX_L(ir_idx) = idx1;
            IR_FLD_L(ir_idx) = IL_Tbl_Idx;
            IR_LIST_CNT_L(ir_idx) = 2;
            IR_OPND_R(ir_idx) = null_opnd;
#ifdef KEY
            }
#endif
            if (OPND_FLD((*result_opnd)) == CN_Tbl_Idx) {
               res_exp_desc->constant = TRUE;
               res_exp_desc->foldable = TRUE;
            }
            else if (SHAPE_WILL_FOLD_LATER((*result_opnd)) ||
                     SHAPE_FOLDABLE((*result_opnd)))       {

               res_exp_desc->will_fold_later = TRUE;
            }

            /* clear the two shape flags on the result opnd */
            SHAPE_FOLDABLE((*result_opnd)) = FALSE;
            SHAPE_WILL_FOLD_LATER((*result_opnd)) = FALSE;
         }
      }
      else {
         /* dim is present, but not constant */

         COPY_OPND(opnd, IL_OPND(list_idx2));
         cast_to_cg_default(&opnd, &(arg_info_list[info_idx2].ed));
         COPY_OPND(IL_OPND(list_idx2), opnd);
#ifdef KEY /* Bug 10410 */
	 int opt_dummy_idx = is_optional_dummy(list_idx2);
         if (NULL_IDX != opt_dummy_idx &&
	   IL_FLD(list_idx2) == IR_Tbl_Idx &&
	   IR_OPR(IL_IDX(list_idx2)) == Cvrt_Opr) {
	   /*
	    * If cast_to_cg_default() performed no conversion, we can just
	    * pass the optional dummy argument as the actual argument, since
	    * the runtime library function recognizes a nil pointer to mean
	    * that the argument is absent. But if list_idx2 is a conversion,
	    * we need to generate this and pass it as list_idx2:
	    *   temp = list_idx2
	    *   %val(present(opt_dummy) ? loc(temp) : nullpointer)
	    *
	    * I tried generating this and returning it in *result:
	    *   %val(temp ? size(array, temp) : size(array, nullptr)
	    * but that blows up later because the front end "knows" that
	    * the IR returned by size_intrinsic() is a simple call.
	    *
	    * I also tried setting arg_info_list.ed.percent_val_arg, but then
	    * the WHIRL generated for "present(opt_dummy_dim)" fails to take
	    * the address of the argument.
	    *
	    * Merely adding %val() here doesn't work because the front end
	    * also "knows" that %val() has already been removed and
	    * arg_info_list.ed.percent_val_arg has already been set, so
	    * I had to add an ugly special case in final_arg_work().
	    */
	   int tmp_attr = gen_compiler_tmp(line, col, Priv, TRUE);
	   ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
	   ATD_TYPE_IDX(tmp_attr) = CG_INTEGER_DEFAULT_TYPE;
	   AT_SEMANTICS_DONE(tmp_attr) = TRUE;
	   int asg_idx = gen_ir(AT_Tbl_Idx, tmp_attr, Asg_Opr,
	     CG_INTEGER_DEFAULT_TYPE, line, col,
	     IL_FLD(list_idx2), IL_IDX(list_idx2));
	   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
	   int loc_idx = gen_ir(AT_Tbl_Idx, tmp_attr, Aloc_Opr,
	     SA_INTEGER_DEFAULT_TYPE, line, col,
	     NO_Tbl_Idx, NULL_IDX);
	   int zero_constant_idx =
	     (SA_INTEGER_DEFAULT_TYPE == CG_INTEGER_DEFAULT_TYPE) ?
	     CN_INTEGER_ZERO_IDX : 
	     C_INT_TO_CN(SA_INTEGER_DEFAULT_TYPE, 0);
	   int select_idx = gen_select_present(line, col, opt_dummy_idx,
	     IR_Tbl_Idx, loc_idx,
	     CN_Tbl_Idx, zero_constant_idx,
	     SA_INTEGER_DEFAULT_TYPE);
	   int val_idx = gen_ir(IR_Tbl_Idx, select_idx, Percent_Val_Opr,
	     SA_INTEGER_DEFAULT_TYPE, line, col,
	     NO_Tbl_Idx, NULL_IDX);
	   IL_FLD(list_idx2) = IR_Tbl_Idx;
	   IL_IDX(list_idx2) = val_idx;
	 }
#endif /* KEY Bug 10410 */
      }
   }
   else { /* second arg not present */
      if (IR_LIST_CNT_R(ir_idx) == 1) {
         IR_LIST_CNT_R(ir_idx) = 2;
         NTR_IR_LIST_TBL(new_idx);
         IL_INTRIN_PLACE_HOLDER(new_idx) = TRUE;
         IL_ARG_DESC_VARIANT(new_idx) = TRUE;
         IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)) = new_idx;
      }

   
      if ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
          (IR_OPR(IL_IDX(list_idx1)) == Whole_Subscript_Opr ||
           (IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
            IR_FLD_L(IL_IDX(list_idx1)) == IR_Tbl_Idx        &&
            IR_OPR(IR_IDX_L(IL_IDX(list_idx1))) == Whole_Subscript_Opr))) {

         COPY_OPND(opnd, IL_OPND(list_idx1));
         array_attr = find_base_attr(&opnd, &line, &col);

         bd_idx = ATD_ARRAY_IDX(array_attr);

         constant_result = TRUE;

         num = 1;

         for (i = 0; i < BD_RANK(bd_idx); i++) {

            if (arg_info_list[info_idx1].ed.shape[i].fld != CN_Tbl_Idx) {
               constant_result = FALSE;
               break;
            }
            else {
               num *= CN_INT_TO_C(arg_info_list[info_idx1].ed.shape[i].idx);
            }
         }

         if (constant_result) {
            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
            res_exp_desc->constant = TRUE;
            res_exp_desc->foldable = TRUE;

            cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

            OPND_IDX((*result_opnd)) = cn_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
         }
      }
      else if (arg_info_list[info_idx1].ed.section ||
               ((IL_FLD(list_idx1) == IR_Tbl_Idx) &&
                (IR_OPR(IL_IDX(list_idx1)) != Whole_Subscript_Opr))) {

         constant_result = TRUE;
         result_will_fold = TRUE;
         num = 1;
         for (i = 0; i < arg_info_list[info_idx1].ed.rank; i++) {

            if (arg_info_list[info_idx1].ed.shape[i].fld != CN_Tbl_Idx) {
               constant_result = FALSE;

               if (! SHAPE_FOLDABLE(arg_info_list[info_idx1].ed.shape[i]) &&
                ! SHAPE_WILL_FOLD_LATER(arg_info_list[info_idx1].ed.shape[i])) {

                  result_will_fold = FALSE;
               }
            }
            else {
               num *= CN_INT_TO_C(arg_info_list[info_idx1].ed.shape[i].idx);
            }
         }

         if (constant_result) {
            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
            res_exp_desc->constant = TRUE;
            res_exp_desc->foldable = TRUE;

            cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

            OPND_IDX((*result_opnd)) = cn_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
         }
         else if (result_will_fold) {
            res_exp_desc->will_fold_later = TRUE;
         }
      }
      else if (IL_FLD(list_idx1) == AT_Tbl_Idx ||
               (IL_FLD(list_idx1) == IR_Tbl_Idx &&
                IR_OPR(IL_IDX(list_idx1)) == Whole_Substring_Opr &&
                IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx)) {

         /* it is assumed size array */
         PRINTMSG(arg_info_list[info_idx1].line, 889, Error,
                  arg_info_list[info_idx1].col);
      }
   }
   

EXIT:

   if (OPND_FLD((*result_opnd)) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX((*result_opnd))) != Call_Opr) {

      cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
      res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
   }

   IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   TRACE (Func_Exit, "size_intrinsic", NULL);

} /* size_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SHAPE(SOURCE) intrinsic.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    shape_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            asg_idx;
   int            subscript_idx;
   int            triplet_idx;
   long64         bit_length;
   int            constant_type_idx;
# ifdef _WHIRL_HOST64_TARGET64
   int            const_array[MAX_NUM_DIMS];
# else
   long_type  	  const_array[MAX_NUM_DIMS];
# endif /* _WHIRL_HOST64_TARGET64 */
   long64     	  host_array[MAX_NUM_DIMS];
   int            ir_idx;
   int            cn_idx;
   int		  info_idx1;
   int		  i;
   boolean        ok;
   int            list_idx1;
   int            list_idx;
   int            the_cn_idx;
   int            tmp_idx;
   expr_arg_type  loc_exp_desc;


   TRACE (Func_Entry, "shape_intrinsic", NULL);

   for (i = 0; i < MAX_NUM_DIMS; i++) {
      host_array[i] = 0;
   }

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   res_exp_desc->rank = 1;
   IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   res_exp_desc->shape[0].fld = CN_Tbl_Idx;
   res_exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                            arg_info_list[info_idx1].ed.rank);

   SHAPE_WILL_FOLD_LATER(res_exp_desc->shape[0]) = TRUE;
   SHAPE_FOLDABLE(res_exp_desc->shape[0]) = TRUE;

   res_exp_desc->foldable = TRUE;
   res_exp_desc->will_fold_later = TRUE;

   for (i = 0; i < arg_info_list[info_idx1].ed.rank; i++) {

      if (arg_info_list[info_idx1].ed.shape[i].fld != CN_Tbl_Idx) {
         res_exp_desc->foldable = FALSE;
      }
      else {
         host_array[i] = CN_CONST(arg_info_list[info_idx1].ed.shape[i].idx);
      }

      if (! SHAPE_FOLDABLE(arg_info_list[info_idx1].ed.shape[i]) &&
          ! SHAPE_WILL_FOLD_LATER(arg_info_list[info_idx1].ed.shape[i])) {
         res_exp_desc->will_fold_later = FALSE;
      }
   }

   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

   if (res_exp_desc->foldable) {
      bit_length = TARGET_BITS_PER_WORD* (long)arg_info_list[info_idx1].ed.rank;
# ifdef _WHIRL_HOST64_TARGET64
      bit_length >>= 1;
# endif /* _WHIRL_HOST64_TARGET64 */

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= bit_length;
      constant_type_idx		= ntr_type_tbl();

      for (i = 0; i < MAX_NUM_DIMS; i++) {

# if defined(_TARGET32)

         /* Make sure that if Integer_8 is default that */
         /* the values still fit in the long container. */

         if (INTEGER_DEFAULT_TYPE == Integer_8) {
            /* JEFFL - Need overflow check here for each array entry */

         }
# endif
         /* JEFFL - This needs to be converted from host to */
         /*         target if we decide that is necessary.  */

         const_array[i] = (long_type) host_array[i];
      }

      the_cn_idx = ntr_const_tbl(constant_type_idx,
                                 FALSE,
                                 const_array);

      tmp_idx = gen_compiler_tmp(IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx),
                                 Shared, TRUE);

      ATD_TYPE_IDX(tmp_idx) = CG_INTEGER_DEFAULT_TYPE;
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;

      loc_exp_desc = *res_exp_desc;
      loc_exp_desc.type_idx = CG_INTEGER_DEFAULT_TYPE;
      loc_exp_desc.type = Integer;
      loc_exp_desc.linear_type = CG_INTEGER_DEFAULT_TYPE;

      ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(&loc_exp_desc,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));

      ATD_SAVED(tmp_idx) = TRUE;
      ATD_DATA_INIT(tmp_idx) = TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      ATD_FLD(tmp_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(tmp_idx) = the_cn_idx;
      ATD_TMP_INIT_NOT_DONE(tmp_idx) = TRUE;

      OPND_IDX((*result_opnd)) = tmp_idx;
      OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);

      ok = gen_whole_subscript(result_opnd, res_exp_desc);

      if (CG_INTEGER_DEFAULT_TYPE != INTEGER_DEFAULT_TYPE) {
         cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);

         ok = fold_aggragate_expression(result_opnd,
                                        res_exp_desc,
                                        FALSE);
      }


      AT_REFERENCED(tmp_idx) = Referenced;
      AT_DEFINED(tmp_idx) = TRUE;

      res_exp_desc->foldable = TRUE;
      res_exp_desc->tmp_reference = TRUE;
   }
   else {
      io_item_must_flatten = TRUE;
      tmp_idx = gen_compiler_tmp(IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx),
                                 Priv, TRUE);

      ATD_TYPE_IDX(tmp_idx) = INTEGER_DEFAULT_TYPE;
      AT_SEMANTICS_DONE(tmp_idx) = TRUE;

      loc_exp_desc = *res_exp_desc;
      loc_exp_desc.type_idx = INTEGER_DEFAULT_TYPE;
      loc_exp_desc.type = Integer;
      loc_exp_desc.linear_type = TYP_LINEAR(INTEGER_DEFAULT_TYPE);

      ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(&loc_exp_desc,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));

      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      for (i = 0; i < arg_info_list[info_idx1].ed.rank; i++) {
      /* create data init stmt */
      NTR_IR_TBL(subscript_idx);
      IR_TYPE_IDX(subscript_idx) = INTEGER_DEFAULT_TYPE;
      IR_OPR(subscript_idx) = Subscript_Opr;
      IR_LINE_NUM(subscript_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM(subscript_idx) = IR_COL_NUM(ir_idx);

      asg_idx = gen_ir(IR_Tbl_Idx, subscript_idx,
                       Asg_Opr, 
		       INTEGER_DEFAULT_TYPE, 
                       IR_LINE_NUM(ir_idx),
                       IR_COL_NUM(ir_idx),
                       OPND_FLD(arg_info_list[info_idx1].ed.shape[i]),
                       OPND_IDX(arg_info_list[info_idx1].ed.shape[i]));

      IR_FLD_L(subscript_idx) = AT_Tbl_Idx;
      IR_IDX_L(subscript_idx) = tmp_idx;
      IR_LINE_NUM_L(subscript_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(subscript_idx) = IR_COL_NUM(ir_idx);

      NTR_IR_LIST_TBL(list_idx);
      cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i+1);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = cn_idx;
      IL_LINE_NUM(list_idx) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx) = IR_COL_NUM(ir_idx);

      IR_LIST_CNT_R(subscript_idx) = 1;
      IR_FLD_R(subscript_idx) = IL_Tbl_Idx;
      IR_IDX_R(subscript_idx) = list_idx;

      gen_sh(Before,
             Assignment_Stmt,
             IR_LINE_NUM(ir_idx),
             IR_COL_NUM(ir_idx),
             FALSE,
             FALSE,
             TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }

      IR_OPR(ir_idx) = Whole_Subscript_Opr;
      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = tmp_idx; 
      IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(ir_idx) = IL_Tbl_Idx;
      IR_IDX_R(ir_idx) = list_idx;
      IR_LIST_CNT_R(ir_idx) = 1;

      NTR_IR_TBL(triplet_idx);
      IR_TYPE_IDX(triplet_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_OPR(triplet_idx) = Triplet_Opr;
      IR_LINE_NUM(triplet_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM(triplet_idx)  = IR_COL_NUM(ir_idx);

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = triplet_idx;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_L(triplet_idx) = IL_Tbl_Idx;
      IR_IDX_L(triplet_idx) = list_idx;
      IR_LIST_CNT_L(triplet_idx) = 3;

      cn_idx = CN_INTEGER_ONE_IDX;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = cn_idx;
      IL_LINE_NUM(list_idx) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx) = IR_COL_NUM(ir_idx);

      NTR_IR_LIST_TBL(tmp_idx);
      IL_NEXT_LIST_IDX(list_idx) = tmp_idx;

      cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);

      IL_FLD(tmp_idx) = CN_Tbl_Idx;
      IL_IDX(tmp_idx) = cn_idx;
      IL_LINE_NUM(tmp_idx) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(tmp_idx) = IR_COL_NUM(ir_idx);

      NTR_IR_LIST_TBL(list_idx);
      IL_NEXT_LIST_IDX(tmp_idx) = list_idx;

      cn_idx = CN_INTEGER_ONE_IDX;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = cn_idx;
      IL_LINE_NUM(list_idx) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx) = IR_COL_NUM(ir_idx);
     

      /* must reset foldable and will_fold_later because there is no */
      /* folder for this intrinsic in constructors.                  */

      res_exp_desc->foldable = FALSE;
      res_exp_desc->will_fold_later = FALSE;
   }

   if (OPND_FLD((*result_opnd)) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX((*result_opnd))) != Call_Opr) {

      cast_opnd_to_type_idx(result_opnd, INTEGER_DEFAULT_TYPE);
   }

   TRACE (Func_Exit, "shape_intrinsic", NULL);

}  /* shape_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    PRESENT(A) intrinsic.                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    present_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   int		  attr_idx;
   int	 	  info_idx1;
   int            ir_idx;
   int            list_idx;
   opnd_type	  opnd;


   TRACE (Func_Entry, "present_intrinsic", NULL);

   has_present_opr = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 TRUE);

   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = Logical;
   res_exp_desc->linear_type = 
           TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   list_idx = IR_IDX_R(ir_idx);

   /* Verify that the actual argument passed to PRESENT is actually */
   /* a dummy argument.                                             */

   COPY_OPND(opnd, IL_OPND(list_idx));

   if (IL_FLD(list_idx) == AT_Tbl_Idx) {

      attr_idx = IL_IDX(list_idx);

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          ATD_CLASS(attr_idx) == Compiler_Tmp &&
          ATD_COPY_ASSUMED_SHAPE(attr_idx) &&
          ATD_TMP_IDX(attr_idx) != NULL_IDX) {

         attr_idx = ATD_TMP_IDX(attr_idx);
      }

      if ((!AT_IS_DARG(attr_idx)) || (!AT_OPTIONAL(attr_idx))) {
         PRINTMSG(arg_info_list[info_idx1].line, 777, Error,
                  arg_info_list[info_idx1].col);
      }
   }
   else {  /* not AT */

      if (OPND_FLD(opnd) == IR_Tbl_Idx) {

         while (OPND_FLD(opnd) == IR_Tbl_Idx &&
                (IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr ||
                 IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr ||
                 IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr)) {

            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         if (OPND_FLD(opnd) != AT_Tbl_Idx) {
            PRINTMSG(arg_info_list[info_idx1].line, 1080, Error,
                     arg_info_list[info_idx1].col);
         }
      }

      while (OPND_FLD(opnd) == IR_Tbl_Idx) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      attr_idx = OPND_IDX(opnd);

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          ATD_CLASS(attr_idx) == Compiler_Tmp &&
          ATD_COPY_ASSUMED_SHAPE(attr_idx) &&
          ATD_TMP_IDX(attr_idx) != NULL_IDX) {

         attr_idx = ATD_TMP_IDX(attr_idx);
         OPND_IDX(opnd) = attr_idx;
      }

      if ((OPND_FLD(opnd) != AT_Tbl_Idx) ||
          (!AT_IS_DARG(OPND_IDX(opnd))) ||
          (!AT_OPTIONAL(OPND_IDX(opnd)))) {
         PRINTMSG(arg_info_list[info_idx1].line, 777, Error,
                  arg_info_list[info_idx1].col);
      }
   }

   IR_OPR(ir_idx) = Present_Opr;
   IR_TYPE_IDX(ir_idx) = LOGICAL_DEFAULT_TYPE;

   IR_IDX_L(ir_idx) = attr_idx;          
   IR_FLD_L(ir_idx) = AT_Tbl_Idx;         
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "present_intrinsic", NULL);

}  /* present_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LOGICAL(L, KIND) intrinsic.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    logical_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   int		  info_idx2;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;


   TRACE (Func_Entry, "logical_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);

   if ((list_idx2 != NULL_IDX) && (IL_IDX(list_idx2) != NULL_IDX)) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      kind_to_linear_type(&((IL_OPND(list_idx2))),
                          ATP_RSLT_IDX(*spec_idx),
                          arg_info_list[info_idx2].ed.kind0seen,
                          arg_info_list[info_idx2].ed.kind0E0seen,
                          arg_info_list[info_idx2].ed.kind0D0seen,
                          ! arg_info_list[info_idx2].ed.kindnotconst);
   }
   else {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Logical_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;
   IR_LIST_CNT_L(ir_idx) = 1;
   IL_NEXT_LIST_IDX(list_idx1) = NULL_IDX;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;


   TRACE (Func_Exit, "logical_intrinsic", NULL);

}  /* logical_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    LEN_TRIM(STRING) intrinsic.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    len_trim_intrinsic(opnd_type     *result_opnd,
                           expr_arg_type *res_exp_desc,
                           int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int		  type_idx;
   int		  info_idx1;


   TRACE (Func_Entry, "len_trim_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     Len_Trim_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      IR_OPR(ir_idx) = Len_Trim_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   TRACE (Func_Exit, "len_trim_intrinsic", NULL);

}  /* len_trim_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    NEAREST(X,S) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    nearest_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            info_idx1;
#ifdef KEY /* Bug 10177 */
   int            num = 0;
#else /* KEY Bug 10177 */
   int            num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "nearest_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Nearest_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_LIST_CNT_L(ir_idx) = 3;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           num = DIGITS_REAL4_F90;
           break;

      case Real_8:
           num = DIGITS_REAL8_F90;
           break;

      case Real_16:
           num = DIGITS_REAL16_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   NTR_IR_LIST_TBL(list_idx3);
   IL_ARG_DESC_VARIANT(list_idx3) = TRUE;

   /* link list together */
   IL_NEXT_LIST_IDX(list_idx2) = list_idx3;

   IL_IDX(list_idx3) = cn_idx;
   IL_FLD(list_idx3) = CN_Tbl_Idx;
   IL_LINE_NUM(list_idx3) = IR_LINE_NUM(ir_idx);
   IL_COL_NUM(list_idx3)  = IR_COL_NUM(ir_idx);

   IL_LINE_NUM(list_idx3) = IL_LINE_NUM(list_idx1);
   IL_COL_NUM(list_idx3) = IL_COL_NUM(list_idx1);

   IR_OPND_R(ir_idx) = null_opnd;


   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "nearest_intrinsic", NULL);

}  /* nearest_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    RRSPACING(X) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    rrspacing_intrinsic(opnd_type     *result_opnd,
                            expr_arg_type *res_exp_desc,
                            int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int            info_idx1;
   int            list_idx1;
   int            list_idx2;
#ifdef KEY /* Bug 10177 */
   int		  num = 0;
#else /* KEY Bug 10177 */
   int		  num;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "rrspacing_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Rrspacing_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_LIST_CNT_L(ir_idx) = 2;

   switch (arg_info_list[info_idx1].ed.linear_type) {
      case Real_4:
           num = DIGITS_REAL4_F90;
           break;

      case Real_8:
           num = DIGITS_REAL8_F90;
           break;

      case Real_16:
           num = DIGITS_REAL16_F90;
           break;
   }

   cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

   NTR_IR_LIST_TBL(list_idx2);
   IL_ARG_DESC_VARIANT(list_idx2) = TRUE;

   /* link list together */
   IL_NEXT_LIST_IDX(list_idx1) = list_idx2;

   IL_IDX(list_idx2) = cn_idx;
   IL_FLD(list_idx2) = CN_Tbl_Idx;

   IL_LINE_NUM(list_idx2) = IR_LINE_NUM(ir_idx);
   IL_COL_NUM(list_idx2) = IR_COL_NUM(ir_idx);

   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "rrspacing_intrinsic", NULL);

}  /* rrspacing_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SCALE(X,I) intrinsic.                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    scale_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;


   TRACE (Func_Entry, "scale_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   IR_OPR(ir_idx) = Scale_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "scale_intrinsic", NULL);

}  /* scale_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SET_EXPONENT(X,I) intrinsic.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    set_exponent_intrinsic(opnd_type     *result_opnd,
                               expr_arg_type *res_exp_desc,
                               int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;


   TRACE (Func_Entry, "set_exponent_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   IR_OPR(ir_idx) = Set_Exponent_Opr;
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "set_exponent_intrinsic", NULL);

}  /* set_exponent_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DSHIFTL(I, J, K) intrinsic.                               *|
|*      Function    DSHIFTR(I, J, K) intrinsic.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    dshiftl_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   int            ir_idx;
   int            cn_idx;
   int		  info_idx1;
   int		  info_idx2;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            minus_idx;
   int            mask_idx;
   int            shiftl_idx;
   int            shiftr_idx;
   int            first_idx;
   int            second_idx;
   int            band_idx;
   int            typeless_idx;
   opnd_type	  opnd;
   int            line;
   long		  num;
   int            column;


   TRACE (Func_Entry, "dshiftl_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_8) {
      typeless_idx = Typeless_8;
   }
   else {
      typeless_idx = TYPELESS_DEFAULT_TYPE;
   }

# ifdef _TARGET_OS_MAX
   if (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_1 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_2 ||
       TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx))) == Integer_4) {
      typeless_idx = Typeless_4;
   }
# endif

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (arg_info_list[info_idx1].ed.linear_type !=
       arg_info_list[info_idx2].ed.linear_type) {
      PRINTMSG(arg_info_list[info_idx2].line, 774, Error,
               arg_info_list[info_idx2].col);
   }     


   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (ATP_INTRIN_ENUM(*spec_idx) == Dshiftl_Intrinsic) {
      mask_idx = gen_ir(IL_FLD(list_idx3), IL_IDX(list_idx3),
                    Mask_Opr, typeless_idx, line, column,
                        NO_Tbl_Idx, NULL_IDX);

      COPY_OPND(opnd, IL_OPND(list_idx2));
      cast_opnd_to_type_idx(&opnd, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      COPY_OPND(IL_OPND(list_idx2), opnd);

      band_idx = gen_ir(IR_Tbl_Idx, mask_idx,
                    Band_Opr, typeless_idx, line, column,
                        IL_FLD(list_idx2), IL_IDX(list_idx2));

      
      num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                 ATP_RSLT_IDX(*spec_idx)))];

      cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

      minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Minus_Opr,ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                         IL_FLD(list_idx3), IL_IDX(list_idx3));


      NTR_IR_LIST_TBL(first_idx);
      IL_FLD(first_idx) = IR_Tbl_Idx;
      IL_IDX(first_idx) = band_idx;


      NTR_IR_LIST_TBL(second_idx);
      IL_FLD(second_idx) = IR_Tbl_Idx;
      IL_IDX(second_idx) = minus_idx;

      IL_NEXT_LIST_IDX(first_idx) = second_idx;


      shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                      Shiftr_Opr, typeless_idx, line, column,
                          NO_Tbl_Idx, NULL_IDX);

      NTR_IR_LIST_TBL(first_idx);
      COPY_OPND(IL_OPND(first_idx), IL_OPND(list_idx1));
      NTR_IR_LIST_TBL(second_idx);
      COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx3));
      IL_NEXT_LIST_IDX(first_idx) = second_idx;


      shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                      Shiftl_Opr, typeless_idx, line, column,
                          NO_Tbl_Idx, NULL_IDX);

      IR_OPR(ir_idx) = Bor_Opr;
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_IDX_L(ir_idx) = shiftr_idx;
      IR_FLD_R(ir_idx) = IR_Tbl_Idx;
      IR_IDX_R(ir_idx) = shiftl_idx;
   }
   else {

      num =  storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                  ATP_RSLT_IDX(*spec_idx)))]*2;

      cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

      minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Minus_Opr,ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                         IL_FLD(list_idx3), IL_IDX(list_idx3));

      mask_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                    Mask_Opr, typeless_idx, line, column,
                        NO_Tbl_Idx, NULL_IDX);

      COPY_OPND(opnd, IL_OPND(list_idx1));
      cast_opnd_to_type_idx(&opnd, ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));
      COPY_OPND(IL_OPND(list_idx1), opnd);

      band_idx = gen_ir(IR_Tbl_Idx, mask_idx,
                    Band_Opr, typeless_idx, line, column,
                        IL_FLD(list_idx1), IL_IDX(list_idx1));


      num = storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(
                                 ATP_RSLT_IDX(*spec_idx)))];

      cn_idx = C_INT_TO_CN(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)), num);

      minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                  Minus_Opr,ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)),line,column,
                         IL_FLD(list_idx3), IL_IDX(list_idx3));


      NTR_IR_LIST_TBL(first_idx);
      IL_FLD(first_idx) = IR_Tbl_Idx;
      IL_IDX(first_idx) = band_idx;

      NTR_IR_LIST_TBL(second_idx);
      IL_FLD(second_idx) = IR_Tbl_Idx;
      IL_IDX(second_idx) = minus_idx;

      IL_NEXT_LIST_IDX(first_idx) = second_idx;


      shiftl_idx = gen_ir(IL_Tbl_Idx, first_idx,
                      Shiftl_Opr, typeless_idx, line, column,
                          NO_Tbl_Idx, NULL_IDX);

      NTR_IR_LIST_TBL(first_idx);
      COPY_OPND(IL_OPND(first_idx), IL_OPND(list_idx2));
      NTR_IR_LIST_TBL(second_idx);
      COPY_OPND(IL_OPND(second_idx), IL_OPND(list_idx3));
      IL_NEXT_LIST_IDX(first_idx) = second_idx;


      shiftr_idx = gen_ir(IL_Tbl_Idx, first_idx,
                      Shiftr_Opr, typeless_idx, line, column,
                          NO_Tbl_Idx, NULL_IDX);

      IR_OPR(ir_idx) = Bor_Opr;
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_IDX_L(ir_idx) = shiftl_idx;
      IR_FLD_R(ir_idx) = IR_Tbl_Idx;
      IR_IDX_R(ir_idx) = shiftr_idx;
      IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
      IR_LINE_NUM_R(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_R(ir_idx) = IR_COL_NUM(ir_idx);
   }

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "dshiftl_intrinsic", NULL);

}  /* dshiftl_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MINVAL(ARRAY, DIM, MASK) intrinsic.                       *|
|*      Function    MINLOC(ARRAY, DIM, MASK) intrinsic.                       *|
|*      Function    MAXVAL(ARRAY, DIM, MASK) intrinsic.                       *|
|*      Function    MAXLOC(ARRAY, DIM, MASK) intrinsic.                       *|
|*      Function    PRODUCT(ARRAY, DIM, MASK) intrinsic.                      *|
|*      Function    SUM(ARRAY, DIM, MASK) intrinsic.                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    minval_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            i;
   int            j;
   int            ir_idx;
   int            attr_idx;
   int            info_idx1;
#ifdef KEY /* Bug 10177 */
   int            info_idx2 = 0;
#else /* KEY Bug 10177 */
   int            info_idx2;
#endif /* KEY Bug 10177 */
   int            info_idx3;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3 = NULL_IDX;
   int            tmp_idx;
   int            line;
   int            col;

# ifdef _TARGET_HAS_FAST_INTEGER
   int            name_idx;
   char          *name_ptr;
   token_type     ext_token;
# endif


   TRACE (Func_Entry, "minval_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   if (list_idx2 != NULL_IDX) {
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);
      list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   }

   if (list_idx3 != NULL_IDX) {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);
   }
      
   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   if (ATP_INTRIN_ENUM(*spec_idx) == Minloc_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Maxloc_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }

   if (arg_info_list[info_idx1].ed.rank < 1) {
      PRINTMSG(arg_info_list[info_idx1].line, 640,  Error, 
               arg_info_list[info_idx1].col);
   }

# ifdef _INLINE_INTRINSICS
#ifdef KEY /* Bug 10410 */
   /* If actual argument is an optional dummy argument belonging to the caller,
    * we require an external call to runtime library which can detect the
    * case in which that argument is absent at runtime. Otherwise, certain
    * combinations of arguments allow inline code to eval the intrinsic. */
   boolean optional = is_optional_dummy(list_idx3);
#endif /* KEY Bug 10410 */
   if (list_idx2 != NULL_IDX) {
      if (arg_info_list[info_idx2].ed.type == Integer &&
          IL_FLD(list_idx2) == CN_Tbl_Idx) {
#ifdef KEY /* Bug 10410 */
         ATP_EXTERNAL_INTRIN(*spec_idx) = optional;/* DIM constant */
#else /* KEY Bug 10410 */
         ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;   /* DIM constant */
#endif /* KEY Bug 10410 */
      }
      else if (arg_info_list[info_idx2].ed.type == Logical) {
#ifdef KEY /* Bug 10410 */
         ATP_EXTERNAL_INTRIN(*spec_idx) = optional;/* just ARRAY and MASK */
#else /* KEY Bug 10410 */
         ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;   /* just ARRAY and MASK */
#endif /* KEY Bug 10410 */
      }
   }
   else {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }
# endif

# ifdef _TARGET_HAS_FAST_INTEGER
      if ((opt_flags.set_fastint_option && 
           arg_info_list[info_idx1].ed.linear_type == Integer_8 &&
           ATP_EXTERNAL_INTRIN(*spec_idx) &&
           TYP_DESC(arg_info_list[info_idx1].ed.type_idx) == Default_Typed) ||
          (opt_flags.set_allfastint_option && 
           arg_info_list[info_idx1].ed.linear_type == Integer_8 &&
           ATP_EXTERNAL_INTRIN(*spec_idx))) {
         name_ptr = AT_OBJ_NAME_PTR(*spec_idx);

         j = -1;
         if (name_ptr[6] == 'J') {
            j = 6;   
         }
         else if (name_ptr[7] == 'J') {
            j = 7;   
         }
         else if (name_ptr[8] == 'J') {
            j = 8;   
         }
         else if (name_ptr[9] == 'J') {
            j = 9;   
         }
         else if (name_ptr[10] == 'J') {
            j = 10;   
         }

         NTR_ATTR_TBL(tmp_idx);
         COPY_COMMON_ATTR_INFO(*spec_idx,
                               tmp_idx,
                               Pgm_Unit);

         COPY_VARIANT_ATTR_INFO(*spec_idx,
                                tmp_idx,
                                Pgm_Unit);


         for (i = 0;  i < AT_NAME_LEN(*spec_idx);  i++) {
            if (j == i) {
               TOKEN_STR(ext_token)[i] = 'I';
            }
            else {
               TOKEN_STR(ext_token)[i] = name_ptr[i];
            }
         }

         TOKEN_STR(ext_token)[i] = '\0';

         NTR_NAME_POOL(TOKEN_ID(ext_token).words, 
                       AT_NAME_LEN(*spec_idx), 
                       name_idx);

         AT_NAME_IDX(tmp_idx) = name_idx;
         ATP_EXT_NAME_IDX(tmp_idx) = name_idx;
         *spec_idx = tmp_idx;
      }
# endif

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (list_idx2 == NULL_IDX) {  /* only one thing was in the list */
      if (ATP_INTRIN_ENUM(*spec_idx) == Minloc_Intrinsic ||
          ATP_INTRIN_ENUM(*spec_idx) == Maxloc_Intrinsic) {
         res_exp_desc->rank = 1;
         res_exp_desc->shape[0].fld = CN_Tbl_Idx;
         res_exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              arg_info_list[info_idx1].ed.rank);
      }
      else {
         res_exp_desc->rank = 0;
      }

      if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
         NTR_IR_LIST_TBL(list_idx2);
         IL_ARG_DESC_VARIANT(list_idx2) = TRUE;
         IL_INTRIN_PLACE_HOLDER(list_idx2) = TRUE;
         NTR_IR_LIST_TBL(list_idx3);
         IL_ARG_DESC_VARIANT(list_idx3) = TRUE;
         IL_INTRIN_PLACE_HOLDER(list_idx3) = TRUE;
         IL_NEXT_LIST_IDX(list_idx1) = list_idx2;
         IL_NEXT_LIST_IDX(list_idx2) = list_idx3;
         IR_LIST_CNT_R(ir_idx) = 3;  
      }
   }
   else {
      if (arg_info_list[info_idx2].ed.type == Logical) {  /* MASK present */
         if (cmd_line_flags.runtime_conformance) {
            gen_runtime_conformance(&IL_OPND(list_idx1),
                                    &(arg_info_list[info_idx1].ed),
                                    &IL_OPND(list_idx2),
                                    &(arg_info_list[info_idx2].ed));
         }
#ifdef KEY /* Bug10410 */
	 if (is_optional_dummy(list_idx2)) {
	   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
	 }
#endif /* KEY Bug10410 */

         if (ATP_INTRIN_ENUM(*spec_idx) == Minloc_Intrinsic ||
             ATP_INTRIN_ENUM(*spec_idx) == Maxloc_Intrinsic) {
            res_exp_desc->rank = 1;
            res_exp_desc->shape[0].fld = CN_Tbl_Idx;
            res_exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             arg_info_list[info_idx1].ed.rank);
         }
         else {
            res_exp_desc->rank = 0;
         }

         if (arg_info_list[info_idx2].ed.rank > 0) {
            if (arg_info_list[info_idx1].ed.rank != 
                arg_info_list[info_idx2].ed.rank) {
               PRINTMSG(arg_info_list[info_idx2].line, 654, Error,
                        arg_info_list[info_idx2].col);
            }
         }

         if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
            NTR_IR_LIST_TBL(tmp_idx);
            IL_ARG_DESC_VARIANT(tmp_idx) = TRUE;
            IL_INTRIN_PLACE_HOLDER(tmp_idx) = TRUE;
            IL_NEXT_LIST_IDX(list_idx1) = tmp_idx;
            IL_NEXT_LIST_IDX(tmp_idx) = list_idx2;
            IR_LIST_CNT_R(ir_idx) = 3;  
         }
      }
      else if (arg_info_list[info_idx2].ed.type == Integer) { /* DIM present */
         if (arg_info_list[info_idx2].ed.rank != 0) {
            PRINTMSG(arg_info_list[info_idx2].line, 654, Error,
                     arg_info_list[info_idx2].col);
         }

         if (arg_info_list[info_idx2].ed.reference) {
            attr_idx = find_base_attr(&IL_OPND(list_idx2), &line, &col);

            if (AT_OPTIONAL(attr_idx)) {
               PRINTMSG(arg_info_list[info_idx2].line, 875, Error,
                        arg_info_list[info_idx2].col);
            }
         }

         if (IL_FLD(list_idx2) == CN_Tbl_Idx) {
            j = 1;
            for (i = 1; i < 8; i++) {   /* KAY - Use compare_cn_and_value */
               if (i == (long) CN_INT_TO_C(IL_IDX(list_idx2))) {
                  j = j + 1;
               }

               COPY_OPND(res_exp_desc->shape[i-1],
                         arg_info_list[info_idx1].ed.shape[j-1]);
               j = j + 1;
            }

            if (compare_cn_and_value(IL_IDX(list_idx2),
                                     (long) arg_info_list[info_idx1].ed.rank,
                                     Gt_Opr) ||
                compare_cn_and_value(IL_IDX(list_idx2), 1, Lt_Opr)) {

               PRINTMSG(arg_info_list[info_idx2].line, 540, Error,
                        arg_info_list[info_idx2].col);
            }
         }

         res_exp_desc->rank = res_exp_desc->rank - 1;

         if (list_idx3 == NULL_IDX) {
            if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
               NTR_IR_LIST_TBL(tmp_idx);
               IL_ARG_DESC_VARIANT(tmp_idx) = TRUE;
               IL_INTRIN_PLACE_HOLDER(tmp_idx) = TRUE;
               IL_NEXT_LIST_IDX(list_idx2) = tmp_idx;
               IR_LIST_CNT_R(ir_idx) = 3;  
            }
         }
         else {
            info_idx3 = IL_ARG_DESC_IDX(list_idx3);
            if (arg_info_list[info_idx3].ed.rank > 0) {
               if (arg_info_list[info_idx1].ed.rank != 
                   arg_info_list[info_idx3].ed.rank) {
                  PRINTMSG(arg_info_list[info_idx3].line, 654, Error,
                           arg_info_list[info_idx3].col);
               }
            }

            if (cmd_line_flags.runtime_conformance) {
               gen_runtime_conformance(&IL_OPND(list_idx1),
                                       &(arg_info_list[info_idx1].ed),
                                       &IL_OPND(list_idx3),
                                       &(arg_info_list[info_idx3].ed));
            }
         }
      }
   }

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      io_item_must_flatten = TRUE;
      if (ATP_INTRIN_ENUM(*spec_idx) == Sum_Intrinsic) {
         IR_OPR(ir_idx) = Sum_Opr;
      }
      else if (ATP_INTRIN_ENUM(*spec_idx) == Product_Intrinsic) {
         IR_OPR(ir_idx) = Product_Opr;
      }
      else if (ATP_INTRIN_ENUM(*spec_idx) == Minval_Intrinsic) {
         IR_OPR(ir_idx) = Minval_Opr;
      }
      else if (ATP_INTRIN_ENUM(*spec_idx) == Minloc_Intrinsic) {
         IR_OPR(ir_idx) = Minloc_Opr;
      }
      else if (ATP_INTRIN_ENUM(*spec_idx) == Maxloc_Intrinsic) {
         IR_OPR(ir_idx) = Maxloc_Opr;
      }
      else {
         IR_OPR(ir_idx) = Maxval_Opr;
      }

      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_IDX_R(ir_idx) = NULL_IDX;
   }
   else {
      if (list_idx2 == NULL_IDX) {
         NTR_IR_LIST_TBL(list_idx2);
         IL_ARG_DESC_VARIANT(list_idx2) = TRUE;
         IL_INTRIN_PLACE_HOLDER(list_idx2) = TRUE;
         IL_NEXT_LIST_IDX(list_idx1) = list_idx2;
         IR_LIST_CNT_R(ir_idx) = 3;
      }

      if (list_idx3 == NULL_IDX) {
         NTR_IR_LIST_TBL(list_idx3);
         IL_ARG_DESC_VARIANT(list_idx3) = TRUE;
         IL_INTRIN_PLACE_HOLDER(list_idx3) = TRUE;
         IL_NEXT_LIST_IDX(list_idx2) = list_idx3;
         IR_LIST_CNT_R(ir_idx) = 3;
      }
   }

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "minval_intrinsic", NULL);

}  /* minval_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DSM_CHUNKSIZE() intrinsic.                                *|
|*      Function    DSM_DISTRIBUTION_BLOCK() intrinsic.                       *|
|*      Function    DSM_DISTRIBUTION_CYCLIC() intrinsic.                      *|
|*      Function    DSM_DISTRIBUTION_STAR() intrinsic.                        *|
|*      Function    DSM_ISDISTRIBUTED() intrinsic.                            *|
|*      Function    DSM_ISRESHAPED() intrinsic.                               *|
|*      Function    DSM_NUMTHREADS() intrinsic.                               *|
|*      Function    DSM_NUMCHUNKS() intrinsic.                                *|
|*      Function    DSM_REM_CHUNKSIZE() intrinsic.                            *|
|*      Function    DSM_THIS_CHUNKSIZE() intrinsic.                           *|
|*      Function    DSM_THIS_STARTINGINDEX() intrinsic.                       *|
|*      Function    DSM_THIS_THREADNUM() intrinsic.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    dsm_numthreads_intrinsic(opnd_type     *result_opnd,
                                 expr_arg_type *res_exp_desc,
                                 int           *spec_idx)

{
   int            cn_idx;
   int            ir_idx;
   int            list_idx;
   int            info_idx;
   int            info_idx1;
   int            list_idx1;
   int            list_idx2;
   int            minus_idx;
   opnd_type      new_opnd;


   TRACE (Func_Entry, "dsm_numthreads_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (list_idx2 != NULL_IDX) {
      cn_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                           arg_info_list[info_idx1].ed.rank);
      minus_idx = gen_ir(CN_Tbl_Idx, cn_idx,
                     Minus_Opr, CG_INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                         IR_COL_NUM(ir_idx),
                         IL_FLD(list_idx2), IL_IDX(list_idx2));

      IL_IDX(list_idx2) = minus_idx;
      IL_FLD(list_idx2) = IR_Tbl_Idx;
   }

   list_idx = IR_IDX_R(ir_idx);
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   while (list_idx != NULL_IDX) {
      info_idx = IL_ARG_DESC_IDX(list_idx);
      COPY_OPND(new_opnd, IL_OPND(list_idx));
      cast_to_type_idx(&new_opnd, &arg_info_list[info_idx].ed, Integer_8);
      COPY_OPND(IL_OPND(list_idx), new_opnd);
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   list_idx = IR_IDX_R(ir_idx);
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   while (list_idx != NULL_IDX) {
      info_idx = IL_ARG_DESC_IDX(list_idx);
      arg_info_list[info_idx].ed.percent_val_arg = TRUE;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->linear_type = 
   TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)));

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "dsm_numthreads_intrinsic", NULL);

}  /* dsm_numthreads_intrinsic */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    OMP_GET_MAX_THREADS() intrinsic.                          *|
|*      Function    OMP_GET_NUM_PROCS() intrinsic.                            *|
|*      Function    OMP_GET_NUM_THREADS() intrinsic.                          *|
|*      Function    OMP_GET_THREAD_NUM() intrinsic.                           *|
|*      Function    OMP_GET_DYNAMIC() intrinsic.                              *|
|*      Function    OMP_GET_NESTED() intrinsic.                               *|
|*      Function    OMP_IN_PARALLEL() intrinsic.                              *|
        Function    OMP_TEST_NEST_LOCK() intrinsic.
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    omp_get_max_threads_intrinsic(opnd_type     *result_opnd,
                                      expr_arg_type *res_exp_desc,
                                      int           *spec_idx)

{
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "omp_get_max_threads", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;

   if (ATP_INTRIN_ENUM(*spec_idx) == Omp_Get_Max_Threads_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Omp_Get_Num_Procs_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Omp_Get_Num_Threads_Intrinsic ||
       ATP_INTRIN_ENUM(*spec_idx) == Omp_Get_Thread_Num_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
   }
#ifdef KEY
   else if (ATP_INTRIN_ENUM(*spec_idx) == Omp_Get_Wtime_Intrinsic ||
            ATP_INTRIN_ENUM(*spec_idx) == Omp_Get_Wtick_Intrinsic)
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = DOUBLE_DEFAULT_TYPE;
#endif
#ifdef KEY /* Bug 12681 */
   else if (ATP_INTRIN_ENUM(*spec_idx) == Omp_Test_Nest_Lock_Intrinsic)
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
#endif /* KEY Bug 12681 */

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "omp_get_max_threads", NULL);

}  /* omp_get_max_threads_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Subroutine    OMP_SET_LOCK(LOCK) intrinsic.                           *|
|*      Function      OMP_TEST_LOCK(LOCK) intrinsic.                          *|
|*      Subroutine    OMP_UNSET_LOCK(LOCK) intrinsic.                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    omp_set_lock_intrinsic(opnd_type     *result_opnd,
                               expr_arg_type *res_exp_desc,
                               int           *spec_idx)

{
   int            ir_idx;
   int            type_idx;
   int            info_idx1;
   int            list_idx1;


   TRACE (Func_Entry, "omp_set_lock_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (cmd_line_flags.s_pointer8 &&
       arg_info_list[info_idx1].ed.linear_type == Integer_4) {
      PRINTMSG(arg_info_list[info_idx1].line, 
               1664, 
               Error, 
               arg_info_list[info_idx1].col);
   }

   if (ATP_INTRIN_ENUM(*spec_idx) == Omp_Test_Lock_Intrinsic) {
      ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;
      type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      IR_TYPE_IDX(ir_idx) = type_idx;
      IR_RANK(ir_idx) = res_exp_desc->rank;
      res_exp_desc->type_idx = type_idx;
      res_exp_desc->type = TYP_TYPE(type_idx);
      res_exp_desc->linear_type = TYP_LINEAR(type_idx);
   }

   io_item_must_flatten = TRUE;

   if (ATP_INTRIN_ENUM(*spec_idx) == Omp_Set_Lock_Intrinsic) {
      IR_OPR(ir_idx) = Omp_Set_Lock_Opr;
   }
   else if (ATP_INTRIN_ENUM(*spec_idx) == Omp_Unset_Lock_Intrinsic) {
      IR_OPR(ir_idx) = Omp_Unset_Lock_Opr;
   }
   else {
      IR_OPR(ir_idx) = Omp_Test_Lock_Opr;
   }
   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "omp_set_lock_intrinsic", NULL);

}  /* omp_set_lock_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DATE() intrinsic.                                         *|
|*      Function    FDATE() intrinsic.                                        *|
|*      Function    JDATE() intrinsic.                                        *|
|*      Function    CLOCK() intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    clock_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)

{
   int            type_idx;
   int            info_idx1;
   int            ir_idx;
   int            list_idx1;


   TRACE (Func_Entry, "clock_intrinsic", NULL);


# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX) = Character;
   TYP_LINEAR(TYP_WORK_IDX) = CHARACTER_DEFAULT_TYPE;
   TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
   TYP_FLD(TYP_WORK_IDX) = CN_Tbl_Idx;
# ifdef KEY
   TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 24);
# else
   TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 8);
# endif
   type_idx = ntr_type_tbl();

   res_exp_desc->type_idx = type_idx;
   res_exp_desc->char_len.fld = TYP_FLD(type_idx);
   res_exp_desc->char_len.idx = TYP_IDX(type_idx);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;
# endif

# ifdef _TARGET_OS_MAX
   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);

   if (list_idx1 != NULL_IDX) {
      info_idx1 = IL_ARG_DESC_IDX(list_idx1);
      if ((arg_info_list[info_idx1].ed.linear_type == Integer_1) ||
          (arg_info_list[info_idx1].ed.linear_type == Integer_2) ||
          (arg_info_list[info_idx1].ed.linear_type == Integer_4)) {
         PRINTMSG(arg_info_list[info_idx1].line, 1054, Error, 
                  arg_info_list[info_idx1].col);
      }
   }
# endif


   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "clock_intrinsic", NULL);

}  /* clock_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    PACK(ARRAY, MASK, VECTOR) intrinsic.                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    pack_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            ir_idx;
   int            i;


   TRACE (Func_Entry, "pack_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   io_item_must_flatten = TRUE;

   if (arg_info_list[info_idx1].ed.rank < 1) {
      PRINTMSG(arg_info_list[info_idx1].line, 640,  Error, 
               arg_info_list[info_idx1].col);
   }

   for (i = 0; i < arg_info_list[info_idx1].ed.rank; i++) {
       if (OPND_FLD(arg_info_list[info_idx1].ed.shape[i]) == CN_Tbl_Idx &&
           OPND_FLD(arg_info_list[info_idx2].ed.shape[i]) == CN_Tbl_Idx) {
          if (CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx1].ed.shape[i])) !=
              CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx2].ed.shape[i]))) {
             PRINTMSG(arg_info_list[info_idx2].line, 1155, Error, 
                      arg_info_list[info_idx2].col);
          }
       }
   }

   if (list_idx3 != NULL_IDX && IL_IDX(list_idx3) != NULL_IDX) {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);
      COPY_OPND(res_exp_desc->shape[0],arg_info_list[info_idx3].ed.shape[0]);
      COPY_OPND(res_exp_desc->char_len,arg_info_list[info_idx3].ed.char_len);

# ifdef _INLINE_INTRINSICS
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif

      if ((TYP_CHAR_CLASS(arg_info_list[info_idx1].ed.type_idx) == 
                                                     Const_Len_Char) &&
          (TYP_CHAR_CLASS(arg_info_list[info_idx3].ed.type_idx) == 
                                                     Const_Len_Char)) {
         if (CN_INT_TO_C(TYP_IDX(arg_info_list[info_idx1].ed.type_idx)) !=
             CN_INT_TO_C(TYP_IDX(arg_info_list[info_idx3].ed.type_idx))) {
            PRINTMSG(arg_info_list[info_idx3].line, 1153, Error,
                     arg_info_list[info_idx3].col);
         }
      }

      if ((arg_info_list[info_idx1].ed.linear_type !=
           arg_info_list[info_idx3].ed.linear_type) ||
          (arg_info_list[info_idx3].ed.rank != 1)) { 
         PRINTMSG(arg_info_list[info_idx3].line, 1153,  Error, 
                  arg_info_list[info_idx3].col);
      }

      if (cmd_line_flags.runtime_conformance) {
         gen_runtime_conformance(&IL_OPND(list_idx1),
                                 &(arg_info_list[info_idx1].ed),
                                 &IL_OPND(list_idx3), 
                                 &(arg_info_list[info_idx3].ed));
      }
#ifdef KEY /* Bug 10410 */
      if (NULL_IDX != is_optional_dummy(list_idx3)) {
	 ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
      }
#endif /* KEY Bug 10410 */
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (cmd_line_flags.runtime_conformance) {
      gen_runtime_conformance(&IL_OPND(list_idx1),
                              &(arg_info_list[info_idx1].ed),
                              &IL_OPND(list_idx2), 
                              &(arg_info_list[info_idx2].ed));
   }

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      IR_OPR(ir_idx) = Pack_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   res_exp_desc->rank = 1;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

#ifdef KEY /* Bug 8165 */
   /* Seems like this is a general problem which ought to be solved for
    * intrinsics in general, but for now we'll solve it for this one. */
   if (Character == res_exp_desc->type) {
     res_exp_desc->char_len = arg_info_list[info_idx1].ed.char_len;
   }
#endif /* KEY Bug 8165 */

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "pack_intrinsic", NULL);

}  /* pack_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    UNPACK(VECTOR, MASK, FIELD) intrinsic.                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    unpack_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            i;
   int            ir_idx;


   TRACE (Func_Entry, "unpack_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   info_idx3 = IL_ARG_DESC_IDX(list_idx3);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = arg_info_list[info_idx1].ed.type_idx;

   io_item_must_flatten = TRUE;
# ifdef _INLINE_INTRINSICS
   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif

   if (arg_info_list[info_idx1].ed.rank != 1) {
      PRINTMSG(arg_info_list[info_idx1].line, 654,  Error, 
               arg_info_list[info_idx1].col);
   }

   if ((TYP_CHAR_CLASS(arg_info_list[info_idx1].ed.type_idx) == 
                                                Const_Len_Char) &&
       (TYP_CHAR_CLASS(arg_info_list[info_idx3].ed.type_idx) == 
                                                Const_Len_Char)) {
      if (CN_INT_TO_C(TYP_IDX(arg_info_list[info_idx1].ed.type_idx)) !=
          CN_INT_TO_C(TYP_IDX(arg_info_list[info_idx3].ed.type_idx))) {
         PRINTMSG(arg_info_list[info_idx3].line, 1154, Error,
                  arg_info_list[info_idx3].col);
      }
   }

   if ((arg_info_list[info_idx1].ed.linear_type != Short_Char_Const) &&
       (arg_info_list[info_idx3].ed.linear_type != Short_Char_Const)) {
      if (arg_info_list[info_idx1].ed.linear_type !=
          arg_info_list[info_idx3].ed.linear_type) {
         PRINTMSG(arg_info_list[info_idx3].line, 1154, Error,
                  arg_info_list[info_idx3].col);
      }
   }

   if (arg_info_list[info_idx2].ed.rank !=arg_info_list[info_idx3].ed.rank) {
      if (arg_info_list[info_idx3].ed.rank != 0) {
         PRINTMSG(arg_info_list[info_idx3].line, 1222, Error,
                  arg_info_list[info_idx3].col);
      }
   }
   else {
      for (i = 1; i <= arg_info_list[info_idx2].ed.rank; i++) {
         if (OPND_FLD(arg_info_list[info_idx2].ed.shape[i-1])== CN_Tbl_Idx &&
             OPND_FLD(arg_info_list[info_idx3].ed.shape[i-1])== CN_Tbl_Idx &&
             CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx2].ed.shape[i-1])) !=
             CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx3].ed.shape[i-1]))) {
            PRINTMSG(arg_info_list[info_idx3].line, 1222, Error,
                     arg_info_list[info_idx3].col);
            break;
         }
      }
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (cmd_line_flags.runtime_conformance) {
      gen_runtime_conformance(&IL_OPND(list_idx2),
                              &(arg_info_list[info_idx2].ed),
                              &IL_OPND(list_idx3),
                              &(arg_info_list[info_idx3].ed));
   }

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      IR_OPR(ir_idx) = Unpack_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   res_exp_desc->rank = arg_info_list[info_idx2].ed.rank;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   COPY_OPND(res_exp_desc->shape[0], arg_info_list[info_idx2].ed.shape[0]);
   COPY_OPND(res_exp_desc->shape[1], arg_info_list[info_idx2].ed.shape[1]);
   COPY_OPND(res_exp_desc->shape[2], arg_info_list[info_idx2].ed.shape[2]);
   COPY_OPND(res_exp_desc->shape[3], arg_info_list[info_idx2].ed.shape[3]);
   COPY_OPND(res_exp_desc->shape[4], arg_info_list[info_idx2].ed.shape[4]);
   COPY_OPND(res_exp_desc->shape[5], arg_info_list[info_idx2].ed.shape[5]);
   COPY_OPND(res_exp_desc->shape[6], arg_info_list[info_idx2].ed.shape[6]);
   COPY_OPND(res_exp_desc->char_len, arg_info_list[info_idx1].ed.char_len);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "unpack_intrinsic", NULL);

}  /* unpack_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    TRIM(STRING) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void    trim_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)

{
   long_type	  folded_const[MAX_WORDS_FOR_INTEGER];
   int            info_idx1;
   int            ir_idx;
   int		  len_idx;
   int            list_idx1;
   opnd_type      opnd;
   int            type_idx;


   TRACE (Func_Entry, "trim_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE; 

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Character_1;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   if (arg_info_list[info_idx1].ed.rank != 0) {
      PRINTMSG(arg_info_list[info_idx1].line, 654,  Error, 
               arg_info_list[info_idx1].col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                        arg_info_list[info_idx1].ed.type_idx,
                        NULL,
                        NULL_IDX,
                        folded_const,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        1,
                        Trim_Opr)) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

      /* folder_driver returns a CN_Tbl_Idx in result for Trim */

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = (int) F_INT_TO_C(folded_const, type_idx);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);

      res_exp_desc->char_len.fld = TYP_FLD(type_idx);
      res_exp_desc->char_len.idx = TYP_IDX(type_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      copy_subtree(&IR_OPND_R(ir_idx), &opnd);

      len_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                       Len_Trim_Opr, 
   		       INTEGER_DEFAULT_TYPE, 
		       IR_LINE_NUM(ir_idx),
                       IR_COL_NUM(ir_idx),
                       NO_Tbl_Idx, NULL_IDX);

      res_exp_desc->char_len.fld = IR_Tbl_Idx;
      res_exp_desc->char_len.idx = len_idx;
      
      ATD_CHAR_LEN_IN_DV(ATP_RSLT_IDX(*spec_idx)) = TRUE;
   }

   res_exp_desc->type_idx = type_idx;
   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   TRACE (Func_Exit, "trim_intrinsic", NULL);

}  /* trim_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    TRANSPOSE(MATRIX) intrinsic.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    transpose_intrinsic(opnd_type     *result_opnd,
                            expr_arg_type *res_exp_desc,
                            int           *spec_idx)
{
   int            info_idx1;
   int            list_idx1;
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "transpose_intrinsic", NULL);

# ifdef _INLINE_INTRINSICS
   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   type_idx = arg_info_list[info_idx1].ed.type_idx;
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   if (arg_info_list[info_idx1].ed.rank != 2) {
      PRINTMSG(arg_info_list[info_idx1].line, 654,  Error, 
               arg_info_list[info_idx1].col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
   COPY_OPND(res_exp_desc->shape[0], arg_info_list[info_idx1].ed.shape[1]);
   COPY_OPND(res_exp_desc->shape[1], arg_info_list[info_idx1].ed.shape[0]);
   COPY_OPND(res_exp_desc->char_len, arg_info_list[info_idx1].ed.char_len);

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      io_item_must_flatten = TRUE;
      IR_OPR(ir_idx) = Transpose_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "transpose_intrinsic", NULL);

}  /* transpose_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SPREAD(SOURCE, DIM, NCOPIES) intrinsic.                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    spread_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            info_idx1;
   int            info_idx2;
   int            info_idx3;
   int            idx;
   int            idx1;
   int            idx2;
   int            ir_idx;
   int            i;
   int            j;
   int            type_idx;
   opnd_type	  opnd;
   opnd_type      shape_opnd;


   TRACE (Func_Entry, "spread_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);
   info_idx3 = IL_ARG_DESC_IDX(list_idx3);
   type_idx = arg_info_list[info_idx1].ed.type_idx;
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   COPY_OPND(res_exp_desc->char_len, arg_info_list[info_idx1].ed.char_len);
   res_exp_desc->rank = arg_info_list[info_idx1].ed.rank + 1;

   if (IL_FLD(list_idx2) == CN_Tbl_Idx) {
      if ((compare_cn_and_value(IL_IDX(list_idx2), 
                                (long) arg_info_list[info_idx1].ed.rank+1,
                                Gt_Opr) ||
           compare_cn_and_value(IL_IDX(list_idx2), 1, Lt_Opr))) {

         PRINTMSG(arg_info_list[info_idx2].line, 1120, Error,
                  arg_info_list[info_idx2].col);
      }

      j = 1;
      for (i = 1; i <= res_exp_desc->rank; i++) {
          if (compare_cn_and_value(IL_IDX(list_idx2),
                                   i,
                                   Eq_Opr)) {
             OPND_LINE_NUM(shape_opnd) = IR_LINE_NUM(ir_idx);
             OPND_COL_NUM(shape_opnd) = IR_COL_NUM(ir_idx);

             NTR_IR_LIST_TBL(idx1);
             NTR_IR_LIST_TBL(idx2);
             IL_NEXT_LIST_IDX(idx1) = idx2;
             IL_IDX(idx2) = CN_INTEGER_ZERO_IDX;
             IL_FLD(idx2) = CN_Tbl_Idx;
             IL_LINE_NUM(idx2) = IR_LINE_NUM(ir_idx);
             IL_COL_NUM(idx2) = IR_COL_NUM(ir_idx);

             IL_IDX(idx1) = IL_IDX(list_idx3);
             IL_FLD(idx1) = IL_FLD(list_idx3);
             IL_LINE_NUM(idx1) = IR_LINE_NUM(ir_idx);
             IL_COL_NUM(idx1) = IR_COL_NUM(ir_idx);

             idx = gen_ir(IL_Tbl_Idx, idx1,
                      Max_Opr, INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                     IR_COL_NUM(ir_idx),
                          NO_Tbl_Idx, NULL_IDX);

             OPND_FLD(shape_opnd) = IR_Tbl_Idx;
             OPND_IDX(shape_opnd) = idx;

             COPY_OPND(res_exp_desc->shape[i-1], shape_opnd);
          }
          else {
             COPY_OPND(res_exp_desc->shape[i-1],
                       arg_info_list[info_idx1].ed.shape[j-1]);
             j = j + 1;
          }
      }

# ifdef _INLINE_INTRINSICS
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif
   }

   COPY_OPND(opnd, IL_OPND(list_idx2));
   cast_to_cg_default(&opnd, &(arg_info_list[info_idx2].ed));
   COPY_OPND(IL_OPND(list_idx2), opnd);

   COPY_OPND(opnd, IL_OPND(list_idx3));
   cast_to_cg_default(&opnd, &(arg_info_list[info_idx3].ed));
   COPY_OPND(IL_OPND(list_idx3), opnd);

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      io_item_must_flatten = TRUE;
      IR_OPR(ir_idx) = Spread_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
      IR_LIST_CNT_L(ir_idx) = IR_LIST_CNT_R(ir_idx);
   }

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "spread_intrinsic", NULL);

}  /* spread_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SELECTED_INT_KIND(R) intrinsic.                           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    selected_int_kind_intrinsic(opnd_type     *result_opnd,
                                    expr_arg_type *res_exp_desc,
                                    int           *spec_idx)
{
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   int		  info_idx1;
   int            ir_idx;
   int            type_idx;
   int            list_idx1;
   int            fifth_select;
   int            fourth_select;
   int            third_select;
   int            second_select;
   int            arg1;
   int            arg2;
   int            arg3;
   int            le_idx;
   int            cn_idx;


   TRACE (Func_Entry, "selected_int_kind_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   if (arg_info_list[info_idx1].ed.rank != 0) {
      PRINTMSG(arg_info_list[info_idx1].line, 654,  Error, 
               arg_info_list[info_idx1].col);
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = Integer;
   res_exp_desc->linear_type = INTEGER_DEFAULT_TYPE;
   type_idx = INTEGER_DEFAULT_TYPE;

   if (IL_FLD(list_idx1) == CN_Tbl_Idx && 
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     NULL,
                     NULL_IDX,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     1,
                     SIK_Opr)) {

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
   }
   else {
      NTR_IR_LIST_TBL(arg1);
      IL_ARG_DESC_VARIANT(arg1) = TRUE;

      NTR_IR_LIST_TBL(arg2);
      IL_ARG_DESC_VARIANT(arg2) = TRUE;

      NTR_IR_LIST_TBL(arg3);
      IL_ARG_DESC_VARIANT(arg3) = TRUE;

      /* link list together */
      IL_NEXT_LIST_IDX(arg1) = arg2;
      IL_NEXT_LIST_IDX(arg2) = arg3;

      fifth_select = gen_ir(IL_Tbl_Idx, arg1,
                            Cvmgt_Opr,
			    INTEGER_DEFAULT_TYPE,
			    IR_LINE_NUM(ir_idx),
                            IR_COL_NUM(ir_idx),
                            NO_Tbl_Idx, NULL_IDX);

      /* set this flag so this opr is pulled off io lists */
      io_item_must_flatten = TRUE;

      

      cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 8);
      IL_FLD(arg1) = CN_Tbl_Idx;
      IL_IDX(arg1) = cn_idx;
      IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

      cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                CN_INTEGER_NEG_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, -1);
      IL_FLD(arg2) = CN_Tbl_Idx;
      IL_IDX(arg2) = cn_idx;
      IL_LINE_NUM(arg2) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg2) = IR_COL_NUM(ir_idx);

      cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, RANGE_INT8_F90);

      le_idx = gen_ir(IL_FLD(IR_IDX_R(ir_idx)), IL_IDX(IR_IDX_R(ir_idx)),
                             Le_Opr,
			     LOGICAL_DEFAULT_TYPE,
			     IR_LINE_NUM(ir_idx),
                             IR_COL_NUM(ir_idx),
                      	     CN_Tbl_Idx, cn_idx);

      IL_FLD(arg3) = IR_Tbl_Idx;
      IL_IDX(arg3) = le_idx;


      NTR_IR_LIST_TBL(arg1);
      IL_ARG_DESC_VARIANT(arg1) = TRUE;

      NTR_IR_LIST_TBL(arg2);
      IL_ARG_DESC_VARIANT(arg2) = TRUE;

      NTR_IR_LIST_TBL(arg3);
      IL_ARG_DESC_VARIANT(arg3) = TRUE;

      /* link list together */
      IL_NEXT_LIST_IDX(arg1) = arg2;
      IL_NEXT_LIST_IDX(arg2) = arg3;

      fourth_select = gen_ir(IL_Tbl_Idx, arg1,
                             Cvmgt_Opr,
			     INTEGER_DEFAULT_TYPE,
	    		     IR_LINE_NUM(ir_idx),
                             IR_COL_NUM(ir_idx),
                             NO_Tbl_Idx, NULL_IDX);

      /* set this flag so this opr is pulled off io lists */
      io_item_must_flatten = TRUE;

      cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 4);
      IL_FLD(arg1) = CN_Tbl_Idx;
      IL_IDX(arg1) = cn_idx;
      IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

      IL_FLD(arg2) = IR_Tbl_Idx;
      IL_IDX(arg2) = fifth_select;

      cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, RANGE_INT4_F90);

      le_idx = gen_ir(IL_FLD(IR_IDX_R(ir_idx)), IL_IDX(IR_IDX_R(ir_idx)),
                      Le_Opr,
 		      LOGICAL_DEFAULT_TYPE,
		      IR_LINE_NUM(ir_idx),
                      IR_COL_NUM(ir_idx),
                      CN_Tbl_Idx, cn_idx);

      IL_FLD(arg3) = IR_Tbl_Idx;
      IL_IDX(arg3) = le_idx;






      NTR_IR_LIST_TBL(arg1);
      IL_ARG_DESC_VARIANT(arg1) = TRUE;

      NTR_IR_LIST_TBL(arg2);
      IL_ARG_DESC_VARIANT(arg2) = TRUE;

      NTR_IR_LIST_TBL(arg3);
      IL_ARG_DESC_VARIANT(arg3) = TRUE;

      /* link list together */
      IL_NEXT_LIST_IDX(arg1) = arg2;
      IL_NEXT_LIST_IDX(arg2) = arg3;

      third_select = gen_ir(IL_Tbl_Idx, arg1,
                            Cvmgt_Opr,
			    INTEGER_DEFAULT_TYPE,
			    IR_LINE_NUM(ir_idx),
                            IR_COL_NUM(ir_idx),
                            NO_Tbl_Idx, NULL_IDX);

      /* set this flag so this opr is pulled off io lists */
      io_item_must_flatten = TRUE;

      cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                CN_INTEGER_TWO_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 2);
      IL_FLD(arg1) = CN_Tbl_Idx;
      IL_IDX(arg1) = cn_idx;
      IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

      IL_FLD(arg2) = IR_Tbl_Idx;
      IL_IDX(arg2) = fourth_select;

      cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, RANGE_INT2_F90);

      le_idx = gen_ir(IL_FLD(IR_IDX_R(ir_idx)), IL_IDX(IR_IDX_R(ir_idx)),
                  Le_Opr,LOGICAL_DEFAULT_TYPE,IR_LINE_NUM(ir_idx),
                                                IR_COL_NUM(ir_idx),
                      CN_Tbl_Idx, cn_idx);

      IL_FLD(arg3) = IR_Tbl_Idx;
      IL_IDX(arg3) = le_idx;



      NTR_IR_LIST_TBL(arg1);
      IL_ARG_DESC_VARIANT(arg1) = TRUE;

      NTR_IR_LIST_TBL(arg2);
      IL_ARG_DESC_VARIANT(arg2) = TRUE;

      NTR_IR_LIST_TBL(arg3);
      IL_ARG_DESC_VARIANT(arg3) = TRUE;

      /* link list together */
      IL_NEXT_LIST_IDX(arg1) = arg2;
      IL_NEXT_LIST_IDX(arg2) = arg3;

      second_select = gen_ir(IL_Tbl_Idx, arg1,
                             Cvmgt_Opr,
			     INTEGER_DEFAULT_TYPE,
			     IR_LINE_NUM(ir_idx),
                             IR_COL_NUM(ir_idx),
                             NO_Tbl_Idx, NULL_IDX);

      /* set this flag so this opr is pulled off io lists */
      io_item_must_flatten = TRUE;

      cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
      IL_FLD(arg1) = CN_Tbl_Idx;
      IL_IDX(arg1) = cn_idx;
      IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

      IL_FLD(arg2) = IR_Tbl_Idx;
      IL_IDX(arg2) = third_select;

      cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, RANGE_INT1_F90);

      le_idx = gen_ir(IL_FLD(IR_IDX_R(ir_idx)), IL_IDX(IR_IDX_R(ir_idx)),
                      Le_Opr,
		      LOGICAL_DEFAULT_TYPE,
		      IR_LINE_NUM(ir_idx),
                      IR_COL_NUM(ir_idx),
                      CN_Tbl_Idx, cn_idx);

      IL_FLD(arg3) = IR_Tbl_Idx;
      IL_IDX(arg3) = le_idx;






      NTR_IR_LIST_TBL(arg1);
      IL_ARG_DESC_VARIANT(arg1) = TRUE;

      NTR_IR_LIST_TBL(arg2);
      IL_ARG_DESC_VARIANT(arg2) = TRUE;

      NTR_IR_LIST_TBL(arg3);
      IL_ARG_DESC_VARIANT(arg3) = TRUE;

      /* link list together */
      IL_NEXT_LIST_IDX(arg1) = arg2;
      IL_NEXT_LIST_IDX(arg2) = arg3;

      IR_OPR(ir_idx) = Cvmgt_Opr;
      IR_FLD_L(ir_idx) = IL_Tbl_Idx;
      IR_IDX_L(ir_idx) = arg1;       
      IR_LIST_CNT_L(ir_idx) = 3;

      /* set this flag so this opr is pulled off io lists */
      io_item_must_flatten = TRUE;

      cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ?
                CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
      IL_FLD(arg1) = CN_Tbl_Idx;
      IL_IDX(arg1) = cn_idx;
      IL_LINE_NUM(arg1) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(arg1) = IR_COL_NUM(ir_idx);

      IL_FLD(arg2) = IR_Tbl_Idx;
      IL_IDX(arg2) = second_select;

      le_idx = gen_ir(IL_FLD(IR_IDX_R(ir_idx)), IL_IDX(IR_IDX_R(ir_idx)),
                      Le_Opr,
		      LOGICAL_DEFAULT_TYPE,
		      IR_LINE_NUM(ir_idx),
                      IR_COL_NUM(ir_idx),
                      CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

      IL_FLD(arg3) = IR_Tbl_Idx;
      IL_IDX(arg3) = le_idx;


      IR_OPND_R(ir_idx) = null_opnd;
      IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
      IR_RANK(ir_idx) = res_exp_desc->rank;

      /* must reset foldable and will_fold_later because there is no */
      /* folder for this intrinsic in constructors.                  */

      res_exp_desc->foldable = FALSE;
      res_exp_desc->will_fold_later = FALSE;
   }

   TRACE (Func_Exit, "selected_int_kind_intrinsic", NULL);

}  /* selected_int_kind_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SELECTED_REAL_KIND(P,R) intrinsic.                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    selected_real_kind_intrinsic(opnd_type     *result_opnd,
                                     expr_arg_type *res_exp_desc,
                                     int           *spec_idx) 
{
   int            ir_idx;
   int            type_idx;
#ifdef KEY /* Bug 10177 */
   int            info_idx1 = 0;
   int            info_idx2 = 0;
#else /* KEY Bug 10177 */
   int            info_idx1;
   int            info_idx2;
#endif /* KEY Bug 10177 */
   int            list_idx1;
   int            list_idx2;
   long_type      folded_const[MAX_WORDS_FOR_NUMERIC];
   opnd_type	  opnd;


   TRACE (Func_Entry, "selected_real_kind_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (list_idx1 != NULL_IDX && IL_IDX(list_idx1) != NULL_IDX) {
      info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));

      if (arg_info_list[IL_ARG_DESC_IDX(list_idx1)].ed.rank != 0) {
         PRINTMSG(arg_info_list[IL_ARG_DESC_IDX(list_idx1)].line, 654,  Error, 
                  arg_info_list[IL_ARG_DESC_IDX(list_idx1)].col);
      }
   }


   if (list_idx2 != NULL_IDX && IL_IDX(list_idx2) != NULL_IDX) {
      info_idx2 = IL_ARG_DESC_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)));

      if (arg_info_list[IL_ARG_DESC_IDX(list_idx2)].ed.rank != 0) {
         PRINTMSG(arg_info_list[IL_ARG_DESC_IDX(list_idx2)].line, 654,  Error, 
                  arg_info_list[IL_ARG_DESC_IDX(list_idx2)].col);
      }
   }

   if ((IL_IDX(list_idx1) == NULL_IDX) && (IL_IDX(list_idx2) == NULL_IDX)) { 
      PRINTMSG(IR_LINE_NUM(ir_idx), 728,  Error, 
               IR_COL_NUM(ir_idx));
   }

   if (IL_IDX(list_idx1) != NULL_IDX) { /* if P is present */
#ifdef KEY /* Bug 10410 */
      if (NULL_IDX != is_optional_dummy(list_idx1)) {
	int cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ? 
	  CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
	pass_dummy_or_default_const(list_idx1, cn_idx, TRUE);
      }
#endif /* KEY Bug 10410 */
      COPY_OPND(opnd, IL_OPND(list_idx1));
      cast_to_cg_default(&opnd, &(arg_info_list[info_idx1].ed));
      COPY_OPND(IL_OPND(list_idx1), opnd);
   }

   if (IL_IDX(list_idx2) != NULL_IDX) { /* if R is present */
#ifdef KEY /* Bug 10410 */
      if (NULL_IDX != is_optional_dummy(list_idx2)) {
	int cn_idx = (CG_INTEGER_DEFAULT_TYPE == INTEGER_DEFAULT_TYPE) ? 
	  CN_INTEGER_ONE_IDX : C_INT_TO_CN(INTEGER_DEFAULT_TYPE, 1);
	pass_dummy_or_default_const(list_idx2, cn_idx, TRUE);
      }
#endif /* KEY Bug 10410 */
      COPY_OPND(opnd, IL_OPND(list_idx2));
      cast_to_cg_default(&opnd, &(arg_info_list[info_idx2].ed));
      COPY_OPND(IL_OPND(list_idx2), opnd);
   }

   IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = INTEGER_DEFAULT_TYPE;
   type_idx = INTEGER_DEFAULT_TYPE;
   res_exp_desc->type = Integer;
   res_exp_desc->linear_type = INTEGER_DEFAULT_TYPE;

   if (IL_IDX(list_idx1) != NULL_IDX && /* if P is present */ 
       IL_IDX(list_idx2) != NULL_IDX && /* if R is present */ 
       IL_FLD(list_idx1) == CN_Tbl_Idx && 
       IL_FLD(list_idx2) == CN_Tbl_Idx && 
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     2,
                     SRK_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }
   else if (IL_IDX(list_idx1) != NULL_IDX && /* if P is present */ 
            IL_IDX(list_idx2) == NULL_IDX && /* if R is not present */ 
            IL_FLD(list_idx1) == CN_Tbl_Idx &&  
            folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                          arg_info_list[info_idx1].ed.type_idx,
                          NULL,
                          NULL_IDX,
                          folded_const,
                          &type_idx,
                          IR_LINE_NUM(ir_idx),
                          IR_COL_NUM(ir_idx),
                          2,
                          SRK_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }
   else if (IL_IDX(list_idx2) != NULL_IDX && /* if R is present */ 
            IL_IDX(list_idx1) == NULL_IDX && /* if P is not present */ 
            IL_FLD(list_idx2) == CN_Tbl_Idx &&  
            folder_driver(NULL,
                          NULL_IDX,
                          (char *)&CN_CONST(IL_IDX(list_idx2)),
                          arg_info_list[info_idx2].ed.type_idx,
                          folded_const,
                          &type_idx,
                          IR_LINE_NUM(ir_idx),
                          IR_COL_NUM(ir_idx),
                          2,
                          SRK_Opr)) {
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ntr_const_tbl(type_idx,
                                               FALSE,
                                               folded_const);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }


   TRACE (Func_Exit, "selected_real_kind_intrinsic", NULL);

}  /* selected_real_kind_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    REPEAT(STRING, NCOPIES) intrinsic.                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    repeat_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   long_type	  folded_const[MAX_WORDS_FOR_INTEGER];
   int            info_idx1;
   int		  info_idx2;
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int		  mult_idx;
   opnd_type      opnd;
   opnd_type      opnd2;
   int            type_idx;


   TRACE (Func_Entry, "repeat_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Character_1;
   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   if (arg_info_list[info_idx1].ed.rank != 0) {
      PRINTMSG(arg_info_list[info_idx1].line, 654,  Error, 
               arg_info_list[info_idx1].col);
   }

   if (arg_info_list[info_idx2].ed.rank != 0) {
      PRINTMSG(arg_info_list[info_idx2].line, 654,  Error, 
               arg_info_list[info_idx2].col);
   }

   if (IL_FLD(list_idx2) == CN_Tbl_Idx) {
      if (compare_cn_and_value(IL_IDX(list_idx2), 0, Lt_Opr)) {
         PRINTMSG(arg_info_list[info_idx2].line, 1056, Error, 
                  arg_info_list[info_idx2].col);
      }

   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (IL_FLD(list_idx1) == CN_Tbl_Idx &&
       IL_FLD(list_idx2) == CN_Tbl_Idx &&
       folder_driver((char *)&CN_CONST(IL_IDX(list_idx1)),
                     arg_info_list[info_idx1].ed.type_idx,
                     (char *)&CN_CONST(IL_IDX(list_idx2)),
                     arg_info_list[info_idx2].ed.type_idx,
                     folded_const,
                     &type_idx,
                     IR_LINE_NUM(ir_idx),
                     IR_COL_NUM(ir_idx),
                     2,
                     Repeat_Opr)) {

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = (int) F_INT_TO_C(folded_const, type_idx);
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);

      res_exp_desc->char_len.fld = TYP_FLD(type_idx);
      res_exp_desc->char_len.idx = TYP_IDX(type_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }
   else {
      COPY_OPND(opnd, arg_info_list[info_idx1].ed.char_len);
      copy_subtree(&opnd, &opnd);

      COPY_OPND(opnd2, IL_OPND(list_idx2));
      copy_subtree(&opnd2, &opnd2);

      mult_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                    Mult_Opr, CG_INTEGER_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                       IR_COL_NUM(ir_idx),
                        OPND_FLD(opnd2), OPND_IDX(opnd2));

      res_exp_desc->char_len.fld = IR_Tbl_Idx;
      res_exp_desc->char_len.idx = mult_idx;

      ATD_CHAR_LEN_IN_DV(ATP_RSLT_IDX(*spec_idx)) = TRUE;
   }

   COPY_OPND(opnd, IL_OPND(list_idx2));
   cast_to_cg_default(&opnd, &(arg_info_list[info_idx2].ed));
   COPY_OPND(IL_OPND(list_idx2), opnd);

   res_exp_desc->type_idx = type_idx;
   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   TRACE (Func_Exit, "repeat_intrinsic", NULL);

}  /* repeat_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DOT_PRODUCT(VECTOR_A, VECTOR_B) intrinsic.                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    dot_product_intrinsic(opnd_type     *result_opnd,
                              expr_arg_type *res_exp_desc,
                              int           *spec_idx)
{
   int            ir_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int            list_idx1;
   int            info_idx1;
# endif


   TRACE (Func_Entry, "dot_product_intrinsic", NULL);

# ifdef _INLINE_INTRINSICS
   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif

   ir_idx = OPND_IDX((*result_opnd));

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   list_idx1 = IR_IDX_R(ir_idx);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
# endif

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   res_exp_desc->rank = 0;
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      io_item_must_flatten = TRUE;
      IR_OPR(ir_idx) = Dot_Product_Opr;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (arg_info_list[info_idx1].ed.type == Logical) {
         IR_OPR(ir_idx) = Dot_Product_Logical_Opr;
      }
# endif
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   IR_TYPE_IDX(ir_idx) = res_exp_desc->type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "dot_product_intrinsic", NULL);

}  /* dot_product_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    MATMUL(MATRIX_A, MATRIX_B) intrinsic.                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    matmul_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   opnd_type	  temp_opnd;


   TRACE (Func_Entry, "matmul_intrinsic", NULL);

# ifdef _INLINE_INTRINSICS
   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
# endif

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   res_exp_desc->rank = BD_RANK(ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)));
   res_exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   res_exp_desc->type = TYP_TYPE(res_exp_desc->type_idx);
   res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);

   if (arg_info_list[info_idx1].ed.rank == 2) {
      COPY_OPND(temp_opnd,arg_info_list[info_idx1].ed.shape[1]);
   }

   if (arg_info_list[info_idx1].ed.rank == 1) {
      COPY_OPND(res_exp_desc->shape[0],arg_info_list[info_idx2].ed.shape[1]);
      COPY_OPND(temp_opnd,arg_info_list[info_idx1].ed.shape[0]);
   }
   else if (arg_info_list[info_idx2].ed.rank == 1) {
      COPY_OPND(res_exp_desc->shape[0],arg_info_list[info_idx1].ed.shape[0]);
   }
   else {
      COPY_OPND(res_exp_desc->shape[0],arg_info_list[info_idx1].ed.shape[0]);
      COPY_OPND(res_exp_desc->shape[1],arg_info_list[info_idx2].ed.shape[1]);
   }

   if ((OPND_FLD(arg_info_list[info_idx2].ed.shape[0]) == CN_Tbl_Idx) &&
       (OPND_FLD(temp_opnd) == CN_Tbl_Idx)) {
      if (CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx2].ed.shape[0])) !=
          CN_INT_TO_C(OPND_IDX(temp_opnd))) {
         PRINTMSG(arg_info_list[info_idx1].line, 1152, Error,
                  arg_info_list[info_idx1].col);
      }
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   if (res_exp_desc->rank == 1) {
       ATP_EXTERNAL_INTRIN(*spec_idx) = !opt_flags.mv_matmul_inline;
   }
   else {
       ATP_EXTERNAL_INTRIN(*spec_idx) = !opt_flags.matmul_inline;
   }
# endif

   if (!ATP_EXTERNAL_INTRIN(*spec_idx)) {
      io_item_must_flatten = TRUE;
      IR_OPR(ir_idx) = Matmul_Opr;
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
      IR_OPND_R(ir_idx) = null_opnd;
   }

   IR_TYPE_IDX(ir_idx) = res_exp_desc->type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "matmul_intrinsic", NULL);

}  /* matmul_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    TRANSFER(SOURCE, MOLD, SIZE) intrinsic.                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    transfer_intrinsic(opnd_type     *result_opnd,
                           expr_arg_type *res_exp_desc,
                           int           *spec_idx)
{
   int            line;
   int            col;
   int		  ch_asg_idx;
   int            info_idx1;
   int            info_idx2;
#ifdef KEY /* Bug 10177 */
   int            info_idx3 = 0;
#else /* KEY Bug 10177 */
   int            info_idx3;
#endif /* KEY Bug 10177 */
   int            ir_idx;
   opnd_type      length_opnd;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   expr_arg_type  loc_exp_desc;
   int            new_idx;
   int            type_idx;
   int_dope_type  dope_1;
   int_dope_type  dope_2;
   opnd_type      opnd;
   boolean        fold_it;
   int            the_cn_idx;
   int            i;
   int            tmp_idx;
   int            or_idx;
   int            attr_idx;
   int            constant_type_idx;
   long64	  bit_length;	
   int_dope_type  dope_result;
   cif_usage_code_type  save_xref_state;
   opnd_type      shape_opnd;
   boolean        ok;
   long_type	  the_constant[MAX_WORDS_FOR_NUMERIC];  /* JEFFL */


   TRACE (Func_Entry, "transfer_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   fold_it = arg_info_list[info_idx1].ed.foldable &&
             arg_info_list[info_idx2].ed.foldable;

   type_idx = arg_info_list[info_idx2].ed.type_idx;

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   res_exp_desc->rank = 0;
   res_exp_desc->type_idx = type_idx;

   if (TYP_TYPE(type_idx) == Character) {
      COPY_OPND((res_exp_desc->char_len),
                (arg_info_list[info_idx2].ed.char_len));
   }

   if (list_idx3 == NULL_IDX) {  /* no third argument */
      if (arg_info_list[info_idx2].ed.rank > 0) {
         res_exp_desc->rank = 1;
      }
   }
   else {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);

      if (arg_info_list[info_idx3].ed.reference) {
         attr_idx = find_base_attr(&IL_OPND(list_idx3), &line, &col);

         if (AT_OPTIONAL(attr_idx)) {
            PRINTMSG(arg_info_list[info_idx3].line, 875, Error,
                     arg_info_list[info_idx3].col);
         }
      }

      res_exp_desc->rank = 1;
      fold_it = fold_it && arg_info_list[info_idx3].ed.foldable;
   }

   if (fold_it) {
      COPY_OPND(opnd, IL_OPND(list_idx1));
      gen_internal_dope_vector(&dope_1, 
                               &opnd, 
                               FALSE, 
                               &arg_info_list[info_idx1].ed);

      COPY_OPND(opnd, IL_OPND(list_idx2));
      gen_internal_dope_vector(&dope_2, 
                               &opnd, 
                               FALSE, 
                               &arg_info_list[info_idx2].ed);

      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

      gen_internal_dope_vector(&dope_result, 
                               &opnd, 
                               TRUE,  
                               &arg_info_list[info_idx2].ed);

      dope_result.num_dims = res_exp_desc->rank;

      if (list_idx3 == NULL_IDX) {
         if (folder_driver((char *)&dope_1,
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&dope_2,
                        arg_info_list[info_idx2].ed.type_idx,
                        (long_type *)&dope_result,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        3,
                        Transfer_Opr,
                        0L,
                        0L)) {
         }
      }
      else {
         if (folder_driver((char *)&dope_1,
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&dope_2,
                        arg_info_list[info_idx2].ed.type_idx,
                        (long_type *)&dope_result,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        3,
                        Transfer_Opr,
                        (char *)&CN_CONST(IL_IDX(list_idx3)),
                        (long)arg_info_list[info_idx3].ed.type_idx)) {
         }
      }

      res_exp_desc->type = arg_info_list[info_idx2].ed.type;
      res_exp_desc->linear_type = arg_info_list[info_idx2].ed.linear_type;
      res_exp_desc->type_idx = arg_info_list[info_idx2].ed.type_idx;

      if (res_exp_desc->rank == 0 && res_exp_desc->type != Structure) {

/* JEFFL - This is max so it probably can stay the same, but it would */
/*         be nice to be consistent with other places.                */

# ifdef _TARGET_OS_MAX

         if (TYP_LINEAR(type_idx) == Complex_4) {
            /* we need to unpack it into two words */
            the_constant[0] = ((long_type *)dope_result.base_addr)[0];
            the_constant[1] = the_constant[0] & 0xFFFFFFFF;
            the_constant[0] = the_constant[0] >> 32;

            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(res_exp_desc->type_idx,
                                                     FALSE,
                                                     the_constant);
         }
         else
# endif
         if (res_exp_desc->type != Character &&
             storage_bit_size_tbl[res_exp_desc->linear_type] <
                                      TARGET_BITS_PER_WORD) {
            /* JEFFL */

            the_constant[0] = ((long_type *)dope_result.base_addr)[0] >>
                      (TARGET_BITS_PER_WORD -
                       storage_bit_size_tbl[res_exp_desc->linear_type]);
            
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(res_exp_desc->type_idx,
                                                     FALSE,
                                                     the_constant);
         }
         else {
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(res_exp_desc->type_idx,
                                                     FALSE,
                                     (long_type *)(dope_result.base_addr));
         }

         OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
         res_exp_desc->foldable = TRUE;
         res_exp_desc->constant = TRUE;
      }
      else {
         bit_length = 1;
         for (i = 1; i <= dope_result.num_dims; i++) {
             bit_length = bit_length * dope_result.dim[i-1].extent;
         }
         bit_length = bit_length * dope_result.el_len;

         if (char_len_in_bytes) {
            if (TYP_TYPE(type_idx) == Character) {
               /* el_len was in bytes, so change to bits */
               bit_length *= CHAR_BIT;
            }
         }
         
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Typeless;
         TYP_BIT_LEN(TYP_WORK_IDX)	= bit_length;
         constant_type_idx		= ntr_type_tbl();

         /* JEFFL */
         the_cn_idx = ntr_const_tbl(constant_type_idx, 
                                    FALSE,
                                    (long_type *)(dope_result.base_addr));

         tmp_idx = gen_compiler_tmp(IR_LINE_NUM(ir_idx), 
                                    IR_COL_NUM(ir_idx),
                                    Shared, TRUE);

         ATD_TYPE_IDX(tmp_idx)	= type_idx;
         AT_SEMANTICS_DONE(tmp_idx)= TRUE;

         for (i = 1; i <= dope_result.num_dims; i++) {
             OPND_FLD(shape_opnd) = CN_Tbl_Idx;
             OPND_IDX(shape_opnd) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                                dope_result.dim[i-1].extent);

             OPND_LINE_NUM(shape_opnd) = IR_LINE_NUM(ir_idx);
             OPND_COL_NUM(shape_opnd)  = IR_COL_NUM(ir_idx);
             SHAPE_WILL_FOLD_LATER(shape_opnd) = TRUE;
             SHAPE_FOLDABLE(shape_opnd) = TRUE;
             res_exp_desc->shape[i-1] = shape_opnd;
         }

         ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(res_exp_desc,
                                                           IR_LINE_NUM(ir_idx),
                                                           IR_COL_NUM(ir_idx));

         ATD_SAVED(tmp_idx) = TRUE;
         ATD_DATA_INIT(tmp_idx) = TRUE;
         ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
         ATD_FLD(tmp_idx) = CN_Tbl_Idx;
         ATD_TMP_IDX(tmp_idx) = the_cn_idx;
         ATD_TMP_INIT_NOT_DONE(tmp_idx) = TRUE;

         OPND_IDX((*result_opnd)) = tmp_idx;
         OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
         OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);

         if (insert_subs_ok) {
            if (res_exp_desc->rank) {
               ok = gen_whole_subscript(result_opnd, res_exp_desc);
            }
            else if (res_exp_desc->type == Character) {
               ok = gen_whole_substring(result_opnd, res_exp_desc->rank);
            }
         }

         AT_REFERENCED(tmp_idx) = Referenced;
         AT_DEFINED(tmp_idx) = TRUE;

         res_exp_desc->foldable = TRUE;
         res_exp_desc->tmp_reference = TRUE; 
      }
   }
   else {

      /* must reset foldable and will_fold_later because there is no */
      /* folder for this intrinsic in constructors.                  */

      res_exp_desc->foldable = FALSE;
      res_exp_desc->will_fold_later = FALSE;

      io_item_must_flatten = TRUE;
   
      if (arg_info_list[info_idx2].ed.type == Character &&
          (arg_info_list[info_idx2].ed.char_len.fld != 
                        TYP_FLD(arg_info_list[info_idx2].ed.type_idx) ||
           arg_info_list[info_idx2].ed.char_len.idx != 
                        TYP_IDX(arg_info_list[info_idx2].ed.type_idx) ||
           (IL_FLD(list_idx2)         == IR_Tbl_Idx &&
            IR_OPR(IL_IDX(list_idx2)) == Concat_Opr))) {

         /* create a new type table index for this character type. */

         loc_exp_desc.rank = 0;

         if (IL_FLD(list_idx2)         == IR_Tbl_Idx &&
             IR_OPR(IL_IDX(list_idx2)) == Concat_Opr) {

            get_concat_len(IL_IDX(list_idx2), &length_opnd);
         }
         else {
            COPY_OPND(length_opnd, (arg_info_list[info_idx2].ed.char_len));
         }

         save_xref_state = xref_state;
         xref_state = CIF_No_Usage_Rec;
         ok = expr_semantics(&length_opnd, &loc_exp_desc);
         xref_state = save_xref_state;

         if (loc_exp_desc.constant) {
            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

            TYP_TYPE(TYP_WORK_IDX)         = Character;
            TYP_LINEAR(TYP_WORK_IDX)       = CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)   = Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)          = CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)          = OPND_IDX(length_opnd);
            res_exp_desc->type_idx         = ntr_type_tbl();
            res_exp_desc->type             = Character;
            res_exp_desc->linear_type      = CHARACTER_DEFAULT_TYPE;
         }
         else { /* non constant character length means an alloc'd item */

            GEN_COMPILER_TMP_ASG(ch_asg_idx,
                                 tmp_idx,
                                 TRUE,     /* Semantics done */
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx),
                                 loc_exp_desc.type_idx,
                                 Priv);

            COPY_OPND(IR_OPND_R(ch_asg_idx), length_opnd);

            gen_sh(Before, Assignment_Stmt, stmt_start_line,
                            stmt_start_col, FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ch_asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

            TYP_TYPE(TYP_WORK_IDX)         = Character;
            TYP_LINEAR(TYP_WORK_IDX)       = CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)   = Var_Len_Char;
            TYP_FLD(TYP_WORK_IDX)          = AT_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)          = tmp_idx;
            TYP_ORIG_LEN_IDX(TYP_WORK_IDX) = tmp_idx;
            res_exp_desc->type_idx         = ntr_type_tbl();
            res_exp_desc->type             = Character;
            res_exp_desc->linear_type      = CHARACTER_DEFAULT_TYPE;
         }

         ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = res_exp_desc->type_idx;
         arg_info_list[info_idx2].ed.type_idx = res_exp_desc->type_idx;
         arg_info_list[info_idx2].ed.char_len.fld = 
                                               TYP_FLD(res_exp_desc->type_idx);
         arg_info_list[info_idx2].ed.char_len.idx = 
                                               TYP_IDX(res_exp_desc->type_idx);
      }

      
      IR_LIST_CNT_R(ir_idx) = 3;

      if (list_idx3 == NULL_IDX) {  /* no third argument */
         NTR_IR_LIST_TBL(new_idx);
         IL_INTRIN_PLACE_HOLDER(new_idx) = TRUE;
         IL_NEXT_LIST_IDX(list_idx2) = new_idx;
         IL_ARG_DESC_VARIANT(new_idx) = TRUE;
      }
      else {
         COPY_OPND(opnd, IL_OPND(list_idx3));
         cast_to_cg_default(&opnd, &(arg_info_list[info_idx3].ed));
         COPY_OPND(IL_OPND(list_idx3), opnd);
      }


      IR_TYPE_IDX(ir_idx) = type_idx;
      IR_RANK(ir_idx) = res_exp_desc->rank;

      if (res_exp_desc->type == Character) {
         res_exp_desc->char_len.fld = TYP_FLD(res_exp_desc->type_idx);
         res_exp_desc->char_len.idx = TYP_IDX(res_exp_desc->type_idx);
      }

      if (
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
          FALSE &&     /* never inline this intrinsic for IRIX */
# endif
          arg_info_list[info_idx1].ed.type != Character &&
          arg_info_list[info_idx2].ed.type != Character &&
          storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] ==
               storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type] &&
          storage_bit_size_tbl[TYPELESS_DEFAULT_TYPE] ==
               storage_bit_size_tbl[arg_info_list[info_idx2].ed.linear_type] &&
          arg_info_list[info_idx1].ed.rank == 
               arg_info_list[info_idx2].ed.rank &&
          arg_info_list[info_idx2].ed.rank <= 1) {

         /*
         If SIZE is present make sure it fits the parameters to 
         do this intrinsic inline.
         */
         if (!(list_idx3 != NULL_IDX &&
               IL_FLD(list_idx3) == CN_Tbl_Idx &&
               OPND_FLD(arg_info_list[info_idx1].ed.shape[0]) == CN_Tbl_Idx &&
               IL_IDX(list_idx3) != 
                OPND_IDX(arg_info_list[info_idx1].ed.shape[0]))) {

            res_exp_desc->type = arg_info_list[info_idx2].ed.type;
            res_exp_desc->linear_type = arg_info_list[info_idx2].ed.linear_type;
            res_exp_desc->type_idx = arg_info_list[info_idx2].ed.type_idx;

            COPY_OPND(res_exp_desc->shape[0], 
                      arg_info_list[info_idx1].ed.shape[0]);

            or_idx = gen_ir(IL_FLD(list_idx1), IL_IDX(list_idx1),
                        Bor_Opr, TYPELESS_DEFAULT_TYPE, IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx),
                            CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

            IR_OPR(ir_idx) = Cvrt_Opr;
            IR_FLD_L(ir_idx) = IR_Tbl_Idx;
            IR_IDX_L(ir_idx) = or_idx;
            IR_OPND_R(ir_idx) = null_opnd;
            IR_TYPE_IDX(ir_idx) = res_exp_desc->type_idx;
            ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
         }
      }
   }

   TRACE (Func_Exit, "transfer_intrinsic", NULL);

}  /* transfer_intrinsic */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SIZEOF(X) intrinsic.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    sizeof_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;
   int            info_idx1;
   int            cn_idx;
   long		  num;


   TRACE (Func_Entry, "sizeof_intrinsic", NULL);

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   res_exp_desc->rank = 0;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   if (arg_info_list[info_idx1].ed.rank == 0 &&
       arg_info_list[info_idx1].ed.type != Character) {

      num = storage_bit_size_tbl[arg_info_list[info_idx1].ed.linear_type] / 
            CHAR_BIT;

      cn_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, num);

      OPND_IDX((*result_opnd)) = cn_idx;
      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
      res_exp_desc->constant = TRUE;
      res_exp_desc->foldable = TRUE;
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }

   TRACE (Func_Exit, "sizeof_intrinsic", NULL);

}  /* sizeof_intrinsic */




/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ALLOCATED(ARRAY) intrinsic.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    allocated_intrinsic(opnd_type     *result_opnd,
                            expr_arg_type *res_exp_desc,
                            int           *spec_idx)
{
   int		  col;
   int		  dv_idx;
   int            ir_idx;
   int            info_idx1;
   int		  line;
   opnd_type	  opnd;


   TRACE (Func_Entry, "allocated_intrinsic", NULL);

   has_present_opr = TRUE;

   ir_idx = OPND_IDX((*result_opnd));
   info_idx1 = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;

   COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));
   line = IR_LINE_NUM(ir_idx);
   col = IR_COL_NUM(ir_idx);

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (!arg_info_list[info_idx1].ed.allocatable) {
      PRINTMSG(arg_info_list[info_idx1].line, 833, Error,
               arg_info_list[info_idx1].col);
   }

   res_exp_desc->rank = 0;
   ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;


   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       (IR_OPR(OPND_IDX(opnd)) == Substring_Opr  ||
        IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr)) {

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr  ||
        IR_OPR(OPND_IDX(opnd)) == Section_Subscript_Opr  ||
        IR_OPR(OPND_IDX(opnd)) == Subscript_Opr)) {

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {
      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   dv_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
               Dv_Access_Assoc, CG_INTEGER_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   ir_idx = gen_ir(IR_Tbl_Idx, dv_idx,
               Eq_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = ir_idx;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "allocted_intrinsic", NULL);

}  /* allocated_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    ASSOCIATED(POINTER, TARGET) intrinsic.                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    associated_intrinsic(opnd_type     *result_opnd,
                             expr_arg_type *res_exp_desc,
                             int           *spec_idx)
{
   int		  col;
   int		  dv_idx;
   int            info_idx1;
   int            info_idx2;
   int            ir_idx;
   int		  line;
   int            list_idx1;
   int            list_idx2;
   opnd_type      opnd;


   TRACE (Func_Entry, "associated_intrinsic", NULL);

   has_present_opr = TRUE;

   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE;
   ir_idx = OPND_IDX((*result_opnd));
   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   if (!arg_info_list[info_idx1].ed.pointer) {
      PRINTMSG(arg_info_list[info_idx1].line, 784, Error,
               arg_info_list[info_idx1].col);
   }

   if (list_idx2 == NULL_IDX) {
      /* TARGET is not present */

      COPY_OPND(opnd, IL_OPND(list_idx1));
      line = IR_LINE_NUM(ir_idx);
      col = IR_COL_NUM(ir_idx);

      res_exp_desc->rank = 0;
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

      if (OPND_FLD(opnd) == IR_Tbl_Idx &&
          (IR_OPR(OPND_IDX(opnd)) == Substring_Opr  ||
           IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr)) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (OPND_FLD(opnd) == IR_Tbl_Idx &&
          (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr  ||
           IR_OPR(OPND_IDX(opnd)) == Section_Subscript_Opr  ||
           IR_OPR(OPND_IDX(opnd)) == Subscript_Opr)) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }
   
      if (OPND_FLD(opnd) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      dv_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                  Dv_Access_Assoc, CG_INTEGER_DEFAULT_TYPE, line, col,
                      NO_Tbl_Idx, NULL_IDX);

      ir_idx = gen_ir(IR_Tbl_Idx, dv_idx,
                  Eq_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                      CN_Tbl_Idx, CN_INTEGER_ONE_IDX);
   
      OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ir_idx;

   }
   else {  /* TARGET is present */
      info_idx2 = IL_ARG_DESC_IDX(list_idx2);

      if ((!arg_info_list[info_idx2].ed.pointer) &&  
          (!arg_info_list[info_idx2].ed.target)) {  
         PRINTMSG(arg_info_list[info_idx2].line, 783,  Error, 
                  arg_info_list[info_idx2].col);
      }

      IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
      res_exp_desc->rank = 0;
      IR_RANK(ir_idx) = res_exp_desc->rank;
   }

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "associated_intrinsic", NULL);

}  /* associated_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    RESHAPE(SOURCE, SHAPE, PAD, ORDER) intrinsic.             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    reshape_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)

{
   int            info_idx1;
   int            info_idx2;
#ifdef KEY /* Bug 10177 */
   int            info_idx3 = 0;
   int            info_idx4 = 0;
#else /* KEY Bug 10177 */
   int            info_idx3;
   int            info_idx4;
#endif /* KEY Bug 10177 */
   int            ir_idx;
   int            line;
   int            col;
   int            the_cn_idx;
   int            cn_idx;
   int            i;
   int            tmp_idx;
   opnd_type      new_opnd;
   int            list_idx;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            list_idx4;
   int            type_idx;
   int            lhs_type;
   int            rhs_type;
   int            attr_idx;
   int            constant_type_idx;
   long64         bit_length;	
   int_dope_type  dope_result;
   int_dope_type  dope_1;
   int_dope_type  dope_2;
   int_dope_type  dope_3;
   int_dope_type  dope_4;
   opnd_type      opnd;
   opnd_type      shape_opnd;
   int		  sub_idx;
   int		  left_idx;
   int		  left_fld;
   long64	  rank;
   boolean        fold_it;
   boolean        optimize =		TRUE;
   boolean        ok;
   long64	  vv;
   int		  valu1;
   long		  valu2;
   expr_arg_type  exp_desc;


   TRACE (Func_Entry, "reshape_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATP_EXTERNAL_INTRIN(*spec_idx) = TRUE; 

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
   list_idx4 = IL_NEXT_LIST_IDX(list_idx3);

   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   type_idx = arg_info_list[info_idx1].ed.type_idx;

   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;
  
   fold_it = arg_info_list[info_idx1].ed.foldable &&
             arg_info_list[info_idx2].ed.foldable;

   if (arg_info_list[info_idx1].ed.rank < 1) {
      PRINTMSG(arg_info_list[info_idx1].line, 640,  Error, 
               arg_info_list[info_idx1].col);
      fold_it = FALSE;
      optimize = FALSE;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

#ifdef KEY /* Bug 4165 */
   /* The code in the "else" part does nothing unless the statement is a
    * simple assignment of a call to "reshape". In that case, it bashes
    * the data type of the first argument to "reshape" so that it has the
    * same shape as the target, and uses the first argument in place of the
    * "reshape" itself. But that may not be valid. For example, the source
    * program may exhibit an erroneous mismatch between the shapes of the
    * lhs and rhs of the assignment, which the "optimization" ignores. Or
    * the source program may be correct, but the first argument may have
    * excess elements which need to be discarded, and simply changing
    * its data type without adjusting the entry in the constant table
    * which defines its value creates a subtle bug (4165) in the assembly.
    * Fortunately, the front end contains other code (see the call to
    * "folder_driver" below) to simplify "reshape", and I haven't found
    * any case (including that of bug 2183) where the other code fails to
    * do at least as good a job as this invalid code, so it seems better
    * to eliminate it than to correct it.
    */
#else /* KEY Bug 4165 */
   /*
   This block of code will optimize a call to RESHAPE by
   completely eliminating the call.   This is attempted
   if just the first and second argument to reshape are present.
   Also, the result must have rank 2.
   */

   if (list_idx1 != NULL_IDX && IL_IDX(list_idx1) != NULL_IDX &&
       list_idx2 != NULL_IDX && IL_IDX(list_idx2) != NULL_IDX &&
       list_idx3 != NULL_IDX && IL_IDX(list_idx3) == NULL_IDX &&
       list_idx4 != NULL_IDX && IL_IDX(list_idx4) == NULL_IDX) {
      if (IR_FLD_R(ir_idx) == IL_Tbl_Idx &&
          IL_FLD(list_idx1) == IR_Tbl_Idx &&
          IL_FLD(list_idx2) == IR_Tbl_Idx &&
          IR_FLD_L(IL_IDX(list_idx1)) == AT_Tbl_Idx &&
          IR_FLD_L(IL_IDX(list_idx2)) == AT_Tbl_Idx &&
          AT_OBJ_CLASS(IR_IDX_L(IL_IDX(list_idx1))) == Data_Obj &&
          ATD_CLASS(IR_IDX_L(IL_IDX(list_idx1))) == Compiler_Tmp &&
          ATD_TMP_INIT_NOT_DONE(IR_IDX_L(IL_IDX(list_idx1)))) {
         rhs_type = TYP_LINEAR(ATD_TYPE_IDX(IR_IDX_L(IL_IDX(list_idx1))));

         list_idx = IR_IDX_R(IL_IDX(list_idx2));
         list_idx = IL_IDX(list_idx);
         list_idx = IR_IDX_L(list_idx);
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         if (IL_FLD(list_idx) == CN_Tbl_Idx) {
            rank = (long) CN_INT_TO_C(IL_IDX(list_idx));
            if (rank == 2 &&
                IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) == Asg_Opr) {
               left_idx = IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx));
               left_fld = IR_FLD_L(SH_IR_IDX(curr_stmt_sh_idx));
               lhs_type = TYP_LINEAR(IR_TYPE_IDX(left_idx));
               if (left_fld == IR_Tbl_Idx && 
                   IR_RANK(left_idx) == rank &&
                   rhs_type == lhs_type) {
                  copy_subtree(&IR_OPND_L(SH_IR_IDX(curr_stmt_sh_idx)), 
                               &new_opnd);
                  if (IR_FLD_L(OPND_IDX(new_opnd)) == AT_Tbl_Idx) {
                     attr_idx = IR_IDX_L(OPND_IDX(new_opnd));
                     IR_IDX_L(OPND_IDX(new_opnd)) = IR_IDX_L(IL_IDX(list_idx1));
                     ATD_ARRAY_IDX(IR_IDX_L(IL_IDX(list_idx1))) =
                     ATD_ARRAY_IDX(attr_idx);
// Bug 2183
# ifdef KEY
                    int para_idx = IR_IDX_R(OPND_IDX(new_opnd));
                    while (para_idx != NULL_IDX) {
                      if (IL_FLD(para_idx) == CN_Tbl_Idx &&
                          IL_IDX(para_idx) != CN_INTEGER_ONE_IDX) 
                        IL_IDX(para_idx) = CN_INTEGER_ONE_IDX;
                      else if (IL_FLD(para_idx) != IR_Tbl_Idx || IR_OPR(IL_IDX(para_idx)) != Triplet_Opr){
                        IL_FLD(para_idx) = CN_Tbl_Idx;
                        IL_IDX(para_idx) = CN_INTEGER_ONE_IDX;
                      }
                      para_idx = IL_NEXT_LIST_IDX(para_idx);
                    }
# endif
                     res_exp_desc->rank = 2;
                     ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
                     fold_it = FALSE;
                     OPND_IDX((*result_opnd)) = OPND_IDX(new_opnd);
                     OPND_FLD((*result_opnd)) = OPND_FLD(new_opnd);
                     OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
                     OPND_COL_NUM((*result_opnd)) = IR_COL_NUM(ir_idx);
                  }
               }
            }
         }
      }
   }
#endif /* KEY Bug 4165 */

   if (OPND_FLD(arg_info_list[info_idx2].ed.shape[0]) == IR_Tbl_Idx) {
      PRINTMSG(arg_info_list[info_idx2].line, 1106, Error, 
               arg_info_list[info_idx2].col);

      res_exp_desc->rank = 0;
      fold_it = FALSE;
      optimize = FALSE;
   }
   else {
      res_exp_desc->rank =  (long)
          CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx2].ed.shape[0]));

      if (res_exp_desc->rank > MAX_NUM_DIMS) {
         PRINTMSG(arg_info_list[info_idx2].line, 1106, Error, 
                  arg_info_list[info_idx2].col);

         res_exp_desc->rank = 0;
         fold_it = FALSE;
         optimize = FALSE;
      }
      else if (arg_info_list[info_idx2].ed.foldable) {
         /* check that each element is >= 0 */

         attr_idx = find_base_attr(&IL_OPND(list_idx2), &line, &col);

# ifdef _DEBUG
         if (attr_idx == NULL_IDX ||
             AT_OBJ_CLASS(attr_idx) != Data_Obj ||
             ATD_CLASS(attr_idx) != Compiler_Tmp ||
             ATD_FLD(attr_idx) != CN_Tbl_Idx ||
             ATD_TMP_IDX(attr_idx) == NULL_IDX) {

            PRINTMSG(arg_info_list[info_idx2].line, 626, Internal,
                     arg_info_list[info_idx2].col,
                     "array constant", "reshape_intrinsic");
         }
# endif

         NTR_IR_TBL(sub_idx);
         IR_OPR(sub_idx) = Subscript_Opr;
         IR_TYPE_IDX(sub_idx) = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(sub_idx) = line;
         IR_COL_NUM(sub_idx) = col;

         IR_FLD_L(sub_idx) = AT_Tbl_Idx;
         IR_IDX_L(sub_idx) = attr_idx;

         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_LIST_CNT_R(sub_idx) = 1;
         NTR_IR_LIST_TBL(list_idx);

         IR_IDX_R(sub_idx) = list_idx;

         IL_FLD(list_idx) = CN_Tbl_Idx;

         exp_desc = init_exp_desc;
         exp_desc.type_idx = ATD_TYPE_IDX(attr_idx);
         exp_desc.type = TYP_TYPE(exp_desc.type_idx);
         exp_desc.linear_type = TYP_LINEAR(exp_desc.type_idx);
         exp_desc.foldable = TRUE;
         exp_desc.constant = TRUE;

         for (i = 0; i < res_exp_desc->rank; i++) {
            IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i+1);

            OPND_FLD(opnd) = IR_Tbl_Idx;
            OPND_IDX(opnd) = sub_idx;

            ok = fold_aggragate_expression(&opnd,
                                           &exp_desc,
                                           TRUE);  

            if (compare_cn_and_value(OPND_IDX(opnd), 0, Lt_Opr)) {
               PRINTMSG(arg_info_list[info_idx2].line, 1176, Error,
                        arg_info_list[info_idx2].col);

               fold_it = FALSE;
               optimize = FALSE;
               break;
            }
         }

         FREE_IR_NODE(sub_idx);
         FREE_IR_LIST_NODE(list_idx);
      }
   }

   switch (res_exp_desc->rank) { 
         case 0: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = NULL_IDX;
                 break;
         case 1: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = BD_DEFERRED_1_IDX;
                 break;
         case 2: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = BD_DEFERRED_2_IDX;
                 break;
         case 3: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = BD_DEFERRED_3_IDX;
                 break;
         case 4: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = BD_DEFERRED_4_IDX;
                 break;
         case 5: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = BD_DEFERRED_5_IDX;
                 break;
         case 6: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = BD_DEFERRED_6_IDX;
                 break;
         case 7: ATD_ARRAY_IDX(ATP_RSLT_IDX(*spec_idx)) = BD_DEFERRED_7_IDX;
                 break;
   }


   if (list_idx3 != NULL_IDX && IL_IDX(list_idx3) != NULL_IDX) {
      info_idx3 = IL_ARG_DESC_IDX(list_idx3);

      fold_it = fold_it && arg_info_list[info_idx3].ed.foldable;

      if (arg_info_list[info_idx3].ed.rank < 1) {
         PRINTMSG(arg_info_list[info_idx3].line, 640,  Error, 
                  arg_info_list[info_idx3].col);
         fold_it = FALSE;
         optimize = FALSE;
      }
   }
   else {
      if (fold_it) {
         valu2 = 1;
         for (i = 1; i <= res_exp_desc->rank; i++) {
             COPY_OPND(opnd, IL_OPND(list_idx2));
             vv = i;
             cn_idx = get_next_array_expr_element(&opnd, &vv);
             valu2 =  valu2 * (long) CN_INT_TO_C(cn_idx);
             COPY_OPND(IL_OPND(list_idx2), opnd);
         }

         valu1 = 1;
         for (i = 1; i <= arg_info_list[info_idx1].ed.rank; i++) {
             valu1 =  valu1 * (long)
                 CN_INT_TO_C(OPND_IDX(arg_info_list[info_idx1].ed.shape[i-1]));
         }

         if (valu1 < valu2) {
            PRINTMSG(arg_info_list[info_idx2].line, 1187, Error, 
                     arg_info_list[info_idx2].col);
            fold_it = FALSE;
            optimize = FALSE;
         }
      }
   }


   if (list_idx4 != NULL_IDX && IL_IDX(list_idx4) != NULL_IDX) {
      info_idx4 = IL_ARG_DESC_IDX(list_idx4);
      fold_it = fold_it && arg_info_list[info_idx4].ed.foldable;

      if (arg_info_list[info_idx4].ed.rank != 1) {
         PRINTMSG(arg_info_list[info_idx4].line, 654,  Error, 
                  arg_info_list[info_idx4].col);
         fold_it = FALSE;
         optimize = FALSE;
      }
   }

#ifdef KEY /* Bug 9046 */
   /* Blows up in fei_len() if we try to optimize this, so don't. Maybe someday
    * try to fix up the WHIRL conversion code to handle it. */
   if (TYP_CHAR_CLASS(arg_info_list[info_idx1].ed.type_idx) ==
     Assumed_Size_Char) {
     optimize = FALSE;
   }
#endif /* KEY Bug 9046 */

   if (fold_it) { 

      COPY_OPND(opnd, IL_OPND(list_idx1));
      gen_internal_dope_vector(&dope_1, 
                               &opnd, 
                               FALSE, 
                               &arg_info_list[info_idx1].ed);

      /* Set the compiler tmp for the array to Not_Referenced */
      /* so that space will not be wasted in static space.    */
      /* After the fold of reshape, these arguments are not   */
      /* needed.                                              */

      tmp_idx = find_base_attr(&opnd, &line, &col);

      if (AT_OBJ_CLASS(tmp_idx) == Data_Obj &&
          ATD_CLASS(tmp_idx) == Compiler_Tmp) {

         AT_REFERENCED(tmp_idx) = Not_Referenced;
      }

      COPY_OPND(opnd, IL_OPND(list_idx2));
      gen_internal_dope_vector(&dope_2, 
                               &opnd, 
                               FALSE, 
                               &arg_info_list[info_idx2].ed);

      /* Set the compiler tmp for the array to Not_Referenced */
      /* so that space will not be wasted in static space.    */
      /* After the fold of reshape, these arguments are not   */
      /* needed.                                              */

      tmp_idx = find_base_attr(&opnd, &line, &col);

      if (AT_OBJ_CLASS(tmp_idx) == Data_Obj &&
          ATD_CLASS(tmp_idx) == Compiler_Tmp) {

         AT_REFERENCED(tmp_idx) = Not_Referenced;
      }

      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;

      gen_internal_dope_vector(&dope_result, 
                               &opnd, 
                               TRUE,  
                               &arg_info_list[info_idx1].ed);

      /* must reset the dope_result.rank to the result rank */
      dope_result.num_dims = res_exp_desc->rank;

      if ((IL_IDX(list_idx3) == NULL_IDX) && (IL_IDX(list_idx4) == NULL_IDX)) {
         if (folder_driver((char *)&dope_1,
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&dope_2,
                        arg_info_list[info_idx2].ed.type_idx,
                        (long_type *)&dope_result,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        4,
                        Reshape_Opr,
                        0L,
                        0L,
                        0L,
                        0L)) {
         }
      }
      else if (IL_IDX(list_idx4) == NULL_IDX) {

         COPY_OPND(opnd, IL_OPND(list_idx3));
         gen_internal_dope_vector(&dope_3, 
                                  &opnd, 
                                  FALSE, 
                                  &arg_info_list[info_idx3].ed);

         /* Set the compiler tmp for the array to Not_Referenced */
         /* so that space will not be wasted in static space.    */
         /* After the fold of reshape, these arguments are not   */
         /* needed.                                              */

         tmp_idx = find_base_attr(&opnd, &line, &col);

         if (AT_OBJ_CLASS(tmp_idx) == Data_Obj &&
             ATD_CLASS(tmp_idx) == Compiler_Tmp) {

            AT_REFERENCED(tmp_idx) = Not_Referenced;
         }

         if (folder_driver((char *)&dope_1,
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&dope_2,
                        arg_info_list[info_idx2].ed.type_idx,
                        (long_type *)&dope_result,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        4,
                        Reshape_Opr,
                        (char *)&dope_3,
                        (long)arg_info_list[info_idx3].ed.type_idx,
                        0L,
                        0L)) {
         }
      }
      else if (IL_IDX(list_idx3) == NULL_IDX) {

         COPY_OPND(opnd, IL_OPND(list_idx4));
         gen_internal_dope_vector(&dope_4,
                                  &opnd,
                                  FALSE,
                                  &arg_info_list[info_idx4].ed);

         /* Set the compiler tmp for the array to Not_Referenced */
         /* so that space will not be wasted in static space.    */
         /* After the fold of reshape, these arguments are not   */
         /* needed.                                              */

         tmp_idx = find_base_attr(&opnd, &line, &col);

         if (AT_OBJ_CLASS(tmp_idx) == Data_Obj &&
             ATD_CLASS(tmp_idx) == Compiler_Tmp) {

            AT_REFERENCED(tmp_idx) = Not_Referenced;
         }

         if (folder_driver((char *)&dope_1,
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&dope_2,
                        arg_info_list[info_idx2].ed.type_idx,
                        (long_type *)&dope_result,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        4,
                        Reshape_Opr,
                        0L,
                        0L,
                        (char *)&dope_4,
                        (long)arg_info_list[info_idx4].ed.type_idx)) {
         }
      }
      else {
         COPY_OPND(opnd, IL_OPND(list_idx3));
         gen_internal_dope_vector(&dope_3, 
                                  &opnd, 
                                  FALSE, 
                                  &arg_info_list[info_idx3].ed);

         /* Set the compiler tmp for the array to Not_Referenced */
         /* so that space will not be wasted in static space.    */
         /* After the fold of reshape, these arguments are not   */
         /* needed.                                              */

         tmp_idx = find_base_attr(&opnd, &line, &col);

         if (AT_OBJ_CLASS(tmp_idx) == Data_Obj &&
             ATD_CLASS(tmp_idx) == Compiler_Tmp) {

            AT_REFERENCED(tmp_idx) = Not_Referenced;
         }

         COPY_OPND(opnd, IL_OPND(list_idx4));
         gen_internal_dope_vector(&dope_4, 
                                  &opnd, 
                                  FALSE, 
                                  &arg_info_list[info_idx4].ed);

         /* Set the compiler tmp for the array to Not_Referenced */
         /* so that space will not be wasted in static space.    */
         /* After the fold of reshape, these arguments are not   */
         /* needed.                                              */

         tmp_idx = find_base_attr(&opnd, &line, &col);

         if (AT_OBJ_CLASS(tmp_idx) == Data_Obj &&
             ATD_CLASS(tmp_idx) == Compiler_Tmp) {

            AT_REFERENCED(tmp_idx) = Not_Referenced;
         }

         if (folder_driver((char *)&dope_1,
                        arg_info_list[info_idx1].ed.type_idx,
                        (char *)&dope_2,
                        arg_info_list[info_idx2].ed.type_idx,
                        (long_type *)&dope_result,
                        &type_idx,
                        IR_LINE_NUM(ir_idx),
                        IR_COL_NUM(ir_idx),
                        4,
                        Reshape_Opr,
                        (char *)&dope_3,
                        (long)arg_info_list[info_idx3].ed.type_idx,
                        (char *)&dope_4,
                        (long)arg_info_list[info_idx4].ed.type_idx)) {
         }
      }

      bit_length = 1;
      for (i = 1; i <= dope_result.num_dims; i++) {
          bit_length = bit_length * dope_result.dim[i-1].extent;
      }
      bit_length = bit_length * dope_result.el_len;

      if (char_len_in_bytes) {
         if (TYP_TYPE(type_idx) == Character) {
            /* el_len was in bytes, so change to bits */
            bit_length *= CHAR_BIT;
         }
      }

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= bit_length;
      constant_type_idx		= ntr_type_tbl();

      /* JEFFL */
      the_cn_idx = ntr_const_tbl(constant_type_idx, 
                                 FALSE,
                                 (long_type *)(dope_result.base_addr));

      tmp_idx = gen_compiler_tmp(IR_LINE_NUM(ir_idx), 
                                 IR_COL_NUM(ir_idx),
                                 Shared, TRUE);

      ATD_TYPE_IDX(tmp_idx) = type_idx;
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;

      for (i = 1; i <= dope_result.num_dims; i++) {
          OPND_FLD(shape_opnd) = CN_Tbl_Idx;
          OPND_IDX(shape_opnd) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                             dope_result.dim[i-1].extent);
          OPND_LINE_NUM(shape_opnd) = IR_LINE_NUM(ir_idx);
          OPND_COL_NUM(shape_opnd)  = IR_COL_NUM(ir_idx);

          SHAPE_WILL_FOLD_LATER(shape_opnd) = TRUE;
          SHAPE_FOLDABLE(shape_opnd) = TRUE;
          res_exp_desc->shape[i-1] = shape_opnd;
      }

      res_exp_desc->type = arg_info_list[info_idx1].ed.type;
      res_exp_desc->linear_type = arg_info_list[info_idx1].ed.linear_type;
      res_exp_desc->type_idx = arg_info_list[info_idx1].ed.type_idx;

      ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(res_exp_desc,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));

      ATD_SAVED(tmp_idx) = TRUE;
      ATD_DATA_INIT(tmp_idx) = TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      ATD_FLD(tmp_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(tmp_idx) = the_cn_idx;
      ATD_TMP_INIT_NOT_DONE(tmp_idx) = TRUE;

      OPND_IDX((*result_opnd)) = tmp_idx;
      OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);

      if (insert_subs_ok) {
         if (res_exp_desc->rank) {
            ok = gen_whole_subscript(result_opnd, res_exp_desc);
         }
         else if (res_exp_desc->type == Character) {
            ok = gen_whole_substring(result_opnd, res_exp_desc->rank);
         }
      }

      AT_REFERENCED(tmp_idx) = Referenced;
      AT_DEFINED(tmp_idx) = TRUE;

      res_exp_desc->foldable = TRUE;
      res_exp_desc->tmp_reference = TRUE; 
   }
   else if (! res_exp_desc->will_fold_later && optimize &&
            optimize_reshape(result_opnd, res_exp_desc)) {
      ATP_EXTERNAL_INTRIN(*spec_idx) = FALSE;
   }

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;

   if (res_exp_desc->type == Character) {
      res_exp_desc->char_len.fld = TYP_FLD(type_idx);
      res_exp_desc->char_len.idx = TYP_IDX(type_idx);
   }

   TRACE (Func_Exit, "reshape_intrinsic", NULL);

}  /* reshape_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    M@MX(X1, X2) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mmx_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "mmx_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Mmx_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "mmx_intrinsic", NULL);

}  /* mmx_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    M@LDMX(X1, X2) intrinsic.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mldmx_intrinsic(opnd_type     *result_opnd,
                        expr_arg_type *res_exp_desc,
                        int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "mldmx_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Mldmx_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "mldmx_intrinsic", NULL);

}  /* mldmx_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    M@LD(X1) intrinsic.                                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mld_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "mld_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Mld_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "mld_intrinsic", NULL);

}  /* mld_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    M@UL() intrinsic.                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mul_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "mul_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Mul_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "mul_intrinsic", NULL);

}  /* mul_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    M@CLR() intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    mclr_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;


   TRACE (Func_Entry, "mclr_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = TYPELESS_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));
   IR_RANK(ir_idx) = res_exp_desc->rank;
   IR_OPR(ir_idx) = Mcbl_Opr;

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
   IR_OPND_R(ir_idx) = null_opnd;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "mclr_intrinsic", NULL);

}  /* mclr_intrinsic */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Issue an error if this ever gets called.  There is a problem with     *|
|*      intrinsic processing.  ATP_INTRIN_ENUM is bad.                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void	unknown_intrinsic(opnd_type     *result_opnd,
                          expr_arg_type *res_exp_desc,
                          int           *spec_idx)
{
   TRACE (Func_Entry, "unknown_intrinsic", NULL);

   PRINTMSG(stmt_start_line, 937, Internal, stmt_start_col);

   TRACE (Func_Exit, "unknown_intrinsic", NULL);

}  /* unknown_intrinsic */

#ifdef KEY 
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    TIME8() or TIME4() intrinsic                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    time_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)

{
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "time_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));

   switch (ATP_INTRIN_ENUM(*spec_idx)) {
     case Time4_Intrinsic:
       ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;
       break;
     case Time8_Intrinsic:
       ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Integer_8;
       break;
     }

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0,
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "time_intrinsic", NULL);

}  /* time_intrinsic */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    FNUM(I) intrinsic.                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    fnum_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "fnum_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "fnum_intrinsic", NULL);

}  /* fnum_intrinsic */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    DTIME(ARRAY) intrinsic.                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    dtime_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "dtime_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
#ifdef KEY /* Bug 3018 */
   /* Why don't any of the funcs in this file get this from the intrin_tbl? */
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_4;
#else
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = Real_8;
#endif /* KEY Bug 3018 */

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "dtime_intrinsic", NULL);

}  /* dtime_intrinsic */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    STAT(File, SArray, Status) intrinsic.                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    stat_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;
   int            type_idx;
   int            cn_type_idx;
   int            list_idx1;
   int            list_idx2;
   int            list_idx3;
   int            info_idx1;
   int            info_idx2;


   TRACE (Func_Entry, "stat_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   cn_type_idx = CN_TYPE_IDX(IL_IDX(list_idx1));

   NTR_IR_LIST_TBL(list_idx3);
   IL_FLD(list_idx3) = CN_Tbl_Idx;
   IL_ARG_DESC_VARIANT(list_idx3) = TRUE;
   IL_LINE_NUM(list_idx3) = IL_LINE_NUM(list_idx1);
   IL_COL_NUM(list_idx3)  = IL_COL_NUM(list_idx1);
   IL_NEXT_LIST_IDX(list_idx2) = IL_NEXT_LIST_IDX(list_idx3);
   IL_IDX(list_idx3) = C_INT_TO_CN(INTEGER_DEFAULT_TYPE, CN_CONST(TYP_IDX(cn_type_idx)));

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "stat_intrinsic", NULL);

}  /* stat_intrinsic */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    SIGNAL(Number, Handler, Status) intrinsic.                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    signal_intrinsic(opnd_type     *result_opnd,
                         expr_arg_type *res_exp_desc,
                         int           *spec_idx)
{
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "signal_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "sigal_intrinsic", NULL);

}  /* signal_intrinsic */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    KILL(Pid, Signal) intrinsic.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    kill_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;
   int            type_idx;


   TRACE (Func_Entry, "kill_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "kill_intrinsic", NULL);

}  /* kill_intrinsic */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Function    FSTAT(File, SArray, Status) intrinsic.                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void   fstat_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;
   int            type_idx;
   int            list_idx1;
   int            list_idx2;
   int            info_idx1;
   int            info_idx2;
   


   TRACE (Func_Entry, "fstat_intrinsic", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = INTEGER_DEFAULT_TYPE;

   type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx));

   list_idx1 = IR_IDX_R(ir_idx);
   list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
   info_idx1 = IL_ARG_DESC_IDX(list_idx1);
   info_idx2 = IL_ARG_DESC_IDX(list_idx2);

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   res_exp_desc->type_idx = type_idx;
   res_exp_desc->linear_type = TYP_LINEAR(type_idx);

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "fstat_intrinsic", NULL);

}  /* fstat_intrinsic */
#endif
#ifdef KEY /* Bug 1683 */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      General purpose function for handling "external" intrinsics           *|
|*      Modeled after "dtime_intrinsic" and "free_intrinsic", but sets        *|
|*      result type automatically based on intrinsic declaration              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void    pathf90_intrinsic(opnd_type     *result_opnd,
                       expr_arg_type *res_exp_desc,
                       int           *spec_idx)
{
   int            ir_idx;
   int            type_idx;

   TRACE (Func_Entry, "pathf90_intrinsic", NULL);

   /* Set the return value type if it's a function */
   ir_idx = OPND_IDX((*result_opnd));
   if (ATP_PGM_UNIT(*spec_idx) == Function) {
     int return_value = ATP_RSLT_IDX(*spec_idx);
     type_idx = ATD_TYPE_IDX(return_value);
     if (type_idx == CHARACTER_DEFAULT_TYPE) {
       CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
       TYP_TYPE(TYP_WORK_IDX) = Character;
       TYP_LINEAR(TYP_WORK_IDX) = CHARACTER_DEFAULT_TYPE;
       TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
       TYP_FLD(TYP_WORK_IDX) = CN_Tbl_Idx;
       TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 24);
       type_idx = ntr_type_tbl();

       res_exp_desc->type_idx = type_idx;
       res_exp_desc->char_len.fld = TYP_FLD(type_idx);
       res_exp_desc->char_len.idx = TYP_IDX(type_idx);
     }
     ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = type_idx;
   }
   else {
     type_idx = TYPELESS_DEFAULT_TYPE;
   }

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);

   IR_TYPE_IDX(ir_idx) = type_idx;
   IR_RANK(ir_idx) = res_exp_desc->rank;
   if (type_idx != TYPELESS_DEFAULT_TYPE) {
     res_exp_desc->type_idx = type_idx;
     res_exp_desc->linear_type = TYP_LINEAR(type_idx);
     }

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */

   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "pathf90_intrinsic", NULL);

}  /* pathf90_intrinsic */
#endif /* KEY Bug 1683 */
#ifdef KEY /* Bug 5089 */
static void    tf_intrinsic_helper(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx,
		      boolean	     generate_true)
{
   int            ir_idx;

   TRACE (Func_Entry, "tf_intrinsic_helper", NULL);

   ir_idx = OPND_IDX((*result_opnd));
   ATD_TYPE_IDX(ATP_RSLT_IDX(*spec_idx)) = LOGICAL_DEFAULT_TYPE;

   conform_check(0, 
                 ir_idx,
                 res_exp_desc,
                 spec_idx,
                 FALSE);
   /* conform_check believes that result has rank of highest-ranking arg. We
    * want a scalar logical result */
   res_exp_desc->rank = 0;

   long constant;
   OPND_IDX((*result_opnd)) = set_up_logical_constant(&constant,
     LOGICAL_DEFAULT_TYPE, generate_true ? TRUE_VALUE : FALSE_VALUE, TRUE);
   OPND_FLD((*result_opnd)) = CN_Tbl_Idx;

   /* must reset foldable and will_fold_later because there is no */
   /* folder for this intrinsic in constructors.                  */
   
   res_exp_desc->foldable = FALSE;
   res_exp_desc->will_fold_later = FALSE;

   TRACE (Func_Exit, "tf_intrinsic_helper", NULL);

}  /* tf_intrinsic_helper */

/* Intrinsic which returns scalar .true. regardless of argument */
void    true_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
  tf_intrinsic_helper(result_opnd, res_exp_desc, spec_idx, TRUE);
}  /* true_intrinsic */

/* Intrinsic which returns scalar .true. or .false., according to whether
 * we can control IEEE gradual underflow.
 */
void    support_uflow_intrinsic(opnd_type     *result_opnd,
                      expr_arg_type *res_exp_desc,
                      int           *spec_idx)
{
#ifdef TARG_X8664
  boolean generate_true = Target_SSE2 || Target_SSE3;
#else
  boolean generate_true = TRUE;
#endif
  tf_intrinsic_helper(result_opnd, res_exp_desc, spec_idx, generate_true);
}  /* support_uflow_intrinsic */
#endif /* KEY Bug 5089 */
