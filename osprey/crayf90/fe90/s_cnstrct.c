/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_cnstrct.c	5.6	09/29/99 00:38:21\n";

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
# include "s_cnstrct.h"
# include "fmath.h"

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
# include <fortran.h>
# endif



/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static boolean interpret_constructor(opnd_type *, expr_arg_type *, 
                                     boolean, long64 *);
static void    increment_count(expr_arg_type *);
static void    write_constant(int);
static boolean interpret_implied_do(int, expr_arg_type *, boolean, long64 *);
static boolean interpret_ref(opnd_type *, expr_arg_type *, boolean, long64 *);
static void    enlarge_char_result_buffer(void);
static void    broadcast_scalar(expr_arg_type *, long64);
static boolean interpret_struct_construct_opr(int, expr_arg_type *,
					      boolean, long64 *);
static boolean interpret_array_construct_opr(int, expr_arg_type *,
					     boolean, long64 *);
static boolean interpret_unary_opr(int, expr_arg_type *, boolean, long64 *);
static boolean interpret_binary_opr(int, expr_arg_type *, boolean, long64 *);
static boolean interpret_concat_opr(int, expr_arg_type *, boolean, long64 *);
static boolean interpret_trim_intrinsic(int, expr_arg_type *, boolean,long64 *);
static boolean interpret_adjustl_intrinsic(int, expr_arg_type *, 
                                           boolean, long64 *);
static boolean interpret_repeat_intrinsic(int, expr_arg_type *, 
                                          boolean, long64 *);
static boolean interpret_transfer_intrinsic(int, expr_arg_type *, 
                                            boolean, long64 *);
static boolean interpret_reshape_intrinsic(int, expr_arg_type *, 
                                           boolean, long64 *);
static boolean interpret_size_intrinsic(int, expr_arg_type *, 
                                        boolean, long64 *);
static boolean interpret_ubound_intrinsic(int, expr_arg_type *, 
                                          boolean, long64 *);
static boolean interpret_shape_intrinsic(int, expr_arg_type *, 
                                         boolean, long64 *);
static boolean interpret_sik_intrinsic(int, expr_arg_type *, 
                                       boolean, long64 *);
static boolean interpret_srk_intrinsic(int, expr_arg_type *, 
                                       boolean, long64 *);
static boolean interpret_unary_intrinsic_opr(int, expr_arg_type *, 
                                             boolean, long64 *);
static boolean interpret_binary_intrinsic_opr(int, expr_arg_type *, 
                                              boolean, long64 *);
static boolean interpret_max_min_opr(int, expr_arg_type *, 
                                     boolean, long64 *);
static boolean interpret_csmg_opr(int, expr_arg_type *, boolean, long64 *);
static boolean interpret_cvmgt_opr(int, expr_arg_type *, boolean, long64 *);
static boolean interpret_index_opr(int, expr_arg_type *, boolean, long64 *);

#ifdef _WHIRL_HOST64_TARGET64
extern int double_stride;
#endif /* _WHIRL_HOST64_TARGET64 */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Interpret a constant constructor and create a constant table entry.   *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - opnd that points to the unprocessed constructor.           *|
|*      exp_desc - the expression descriptor from expr_semantics for the      *|
|*		   constructor.                                               *|
|*									      *|
|* Output parameters:							      *|
|*	top_opnd - on return, points to the constant or tmp.                  *|
|*	exp_desc - some fields are modified (constant, tmp_reference)         *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no problems.                                                  *|
|*									      *|
\******************************************************************************/

boolean create_constructor_constant(opnd_type	   *top_opnd,
   				    expr_arg_type  *exp_desc)

{
   int			asg_idx;
   int			bd_idx;
   opnd_type		char_len_opnd;
   int			col;
   int			i;
   int			ir_idx;
   int			line;
   int			list_idx;
   boolean		ok			= TRUE;
   expr_arg_type	loc_exp_desc;
   int			mult_idx;
   long64		num_elements		= 1;
   boolean		save_defer_stmt_expansion;
   expr_arg_type	save_exp_desc;
   int			save_target_array_idx	= 0;
   int			sub_idx;
   int			tmp_idx;
   int			type_idx;
   long64		zero			= 0;


   TRACE (Func_Entry, "create_constructor_constant", NULL);

   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   single_value_array = FALSE;
   single_value_opnd = null_opnd;

   if (OPND_FLD((*top_opnd)) == CN_Tbl_Idx &&
       exp_desc->type != Character &&
       exp_desc->type != Structure) {
      single_value_array = TRUE;
      COPY_OPND(single_value_opnd, (*top_opnd));
   }

   /* before we clear the exp_desc, check to make sure we can */
   /* do any type conversions requested.                      */

   if (check_type_conversion) {

      if (! check_asg_semantics(target_type_idx, exp_desc->type_idx, -1,0)) {
         check_type_conversion = FALSE;
      }
   }
   
   save_exp_desc = (*exp_desc);
   ir_idx = OPND_IDX((*top_opnd));

   find_opnd_line_and_column(top_opnd, &line, &col);

   char_result_offset = 0;
   bits_in_constructor = 0;

   unequal_char_lens = FALSE;

   if (IR_OPR(ir_idx) != Constant_Struct_Construct_Opr &&
       exp_desc->type == Character) {

      copy_subtree(&(exp_desc->char_len), &char_len_opnd);
      OPND_LINE_NUM(char_len_opnd) = line;
      OPND_COL_NUM(char_len_opnd)  = col;

      if (OPND_FLD(char_len_opnd) != CN_Tbl_Idx) {
         process_char_len(&char_len_opnd);
      }

# ifdef _DEBUG
      if (OPND_FLD(char_len_opnd) != CN_Tbl_Idx) {
         PRINTMSG(line, 1203, Internal, col);
      }
# endif

      if (! check_type_conversion) {

         check_type_conversion = TRUE;
         target_type_idx = Character_1;
         target_char_len_idx = OPND_IDX(char_len_opnd);
      }
   }

   /* do count first */

   if (IR_OPR(ir_idx) != Constant_Struct_Construct_Opr &&
       exp_desc->constructor_size_level == Simple_Expr_Size) {
      /* shape is correct in exp_desc.shape[0] */
      increment_count(exp_desc);
   }
   else {

      (*exp_desc) = init_exp_desc;
      ok = interpret_constructor(top_opnd, exp_desc, TRUE, &zero);
   }

   switch (stmt_type) {
      case Allocate_Stmt :
      case Arith_If_Stmt :
      case Assignment_Stmt :
      case Backspace_Stmt :
      case Buffer_Stmt :
      case Call_Stmt :
      case Case_Stmt :
      case Close_Stmt :
      case Deallocate_Stmt :
      case Decode_Stmt :
      case Do_Iterative_Stmt :
      case Do_While_Stmt :
      case Do_Infinite_Stmt :
      case Else_If_Stmt :
      case Else_Where_Stmt :
      case Encode_Stmt :
      case Endfile_Stmt :
      case If_Cstrct_Stmt :
      case If_Stmt :
      case Inquire_Stmt :
      case Nullify_Stmt :
      case Open_Stmt :
      case Outmoded_If_Stmt :
      case Print_Stmt :
      case Read_Stmt :
      case Rewind_Stmt :
      case Select_Stmt :
      case Where_Cstrct_Stmt :
      case Where_Stmt :
      case Write_Stmt :
         /* These stmt types do not require a folded constructor */
         /* so see if this should be a runtime constructor.      */

         /* if bigger than 5,000 elements, make it runtime */

         if (ok &&
             ! single_value_array &&
             OPND_FLD(exp_desc->shape[0]) == CN_Tbl_Idx &&   /* It should */
             compare_cn_and_value(OPND_IDX(exp_desc->shape[0]),
                                  5000,
                                  Gt_Opr)) {

            /* restore exp_desc to the saved version */
            COPY_OPND((save_exp_desc.shape[0]), (exp_desc->shape[0]));
            (*exp_desc) = save_exp_desc;

            exp_desc->will_fold_later = FALSE;
            exp_desc->foldable = FALSE;
            IR_OPR(ir_idx) = Array_Construct_Opr;
            exp_desc->constructor_size_level = Simple_Expr_Size;

            ok = create_runtime_array_constructor(top_opnd, exp_desc);

            goto EXIT;
         }
         break;
   }

   if (exp_desc->type == Character) {

      if (unequal_char_lens) {
         PRINTMSG(line, 903, Ansi, col);
      }

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

      TYP_TYPE(TYP_WORK_IDX)	= Character;
      TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
      TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)	= target_char_len_idx;

      exp_desc->type_idx	= ntr_type_tbl();
      exp_desc->type		= Character;
      exp_desc->linear_type	= CHARACTER_DEFAULT_TYPE;
      exp_desc->char_len.fld    = TYP_FLD(exp_desc->type_idx);
      exp_desc->char_len.idx    = TYP_IDX(exp_desc->type_idx);
   }
   else if (check_type_conversion) {
      exp_desc->type_idx	= target_type_idx;
      exp_desc->type		= TYP_TYPE(target_type_idx);
      exp_desc->linear_type	= TYP_LINEAR(target_type_idx);
   }

   char_result_offset = 0;

   if (! ok) {
      goto EXIT;
   }

   if (target_array_idx != NULL_IDX) {

      save_target_array_idx = target_array_idx;
   }
      

   if (exp_desc->rank == 0 &&
       target_array_idx != NULL_IDX &&
       BD_RESOLVED(target_array_idx)) {

      if (BD_LEN_FLD(target_array_idx) == CN_Tbl_Idx) {
         num_elements = CN_INT_TO_C(BD_LEN_IDX(target_array_idx));
         bits_in_constructor *= num_elements;
      }

      exp_desc->rank = BD_RANK(target_array_idx);

      for (i = 0; i < BD_RANK(target_array_idx); i++) {
         OPND_FLD(exp_desc->shape[i])  = BD_XT_FLD(target_array_idx, i + 1);
         OPND_IDX(exp_desc->shape[i])  = BD_XT_IDX(target_array_idx, i + 1);
         OPND_LINE_NUM(exp_desc->shape[i]) = line;
         OPND_COL_NUM(exp_desc->shape[i])  = col;
      }
   }

   if (! single_value_array) {
      target_array_idx	   = NULL_IDX;
      words_in_constructor = STORAGE_WORD_SIZE(bits_in_constructor);

      /* then get constant */

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Typeless;
      TYP_LINEAR(TYP_WORK_IDX)  = Long_Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= bits_in_constructor;
      type_idx			= ntr_type_tbl();

     /* Pass NULL so that the caller can fill in the constant. */

      the_cn_idx	= ntr_const_tbl(type_idx, FALSE, NULL);
      the_cn_bit_offset = 0;

      /* fill in the constant */

      if (num_elements > 0) {
         ok = interpret_constructor(top_opnd, &loc_exp_desc, FALSE, &zero);
   
         if (num_elements > 1) {
            bcast_cn_bit_offset = 0;
            broadcast_scalar(exp_desc, num_elements);
         }
      }

# ifdef _DEBUG
# endif

   } /* ! single_value_array */
   else {

      if (check_type_conversion &&
          TYP_LINEAR(CN_TYPE_IDX(OPND_IDX(single_value_opnd))) !=
                TYP_LINEAR(target_type_idx)) {
         /* convert the constant */

         cast_to_type_idx(&single_value_opnd,
                          &save_exp_desc,
                          target_type_idx);
      }
   }

   /* clear check_type_conversion since we are done with it */
   check_type_conversion = FALSE;

   if (! ok) {
      goto EXIT;
   }

   exp_desc->constructor = TRUE;


   /* create tmp init here */

   if (OPND_FLD(init_target_opnd) != NO_Tbl_Idx) {
      tmp_idx = find_left_attr(&init_target_opnd);

      /* create data init stmt */
      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Init_Opr;
      IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;

      if (single_value_array &&
          OPND_FLD(init_target_opnd) == AT_Tbl_Idx) {

         bd_idx = ATD_ARRAY_IDX(tmp_idx);

         NTR_IR_TBL(sub_idx);
         IR_OPR(sub_idx) = Subscript_Opr;
         IR_TYPE_IDX(sub_idx) = ATD_TYPE_IDX(tmp_idx);
         IR_LINE_NUM(sub_idx) = line;
         IR_COL_NUM(sub_idx) = col;
         IR_FLD_L(sub_idx) = AT_Tbl_Idx;
         IR_IDX_L(sub_idx) = tmp_idx;
         IR_LINE_NUM_L(sub_idx) = line;
         IR_COL_NUM_L(sub_idx) = col;

         IR_FLD_L(asg_idx) = IR_Tbl_Idx;
         IR_IDX_L(asg_idx) = sub_idx;

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_LIST_CNT_R(sub_idx) = BD_RANK(bd_idx);
         IR_IDX_R(sub_idx) = list_idx;

         IL_FLD(list_idx) = BD_LB_FLD(bd_idx, 1);
         IL_IDX(list_idx) = BD_LB_IDX(bd_idx, 1);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         for (i = 2; i <= BD_RANK(bd_idx); i++) {
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
            IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;
         }
      }
      else {
         COPY_OPND(IR_OPND_L(asg_idx), init_target_opnd);
      }

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(asg_idx) = IL_Tbl_Idx;
      IR_IDX_R(asg_idx) = list_idx;
      IR_LIST_CNT_R(asg_idx) = 3;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = (single_value_array ? 
                           OPND_IDX(single_value_opnd) : the_cn_idx);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;

      if (single_value_array) {
         IL_IDX(list_idx) = BD_LEN_IDX(ATD_ARRAY_IDX(tmp_idx));
      }
      else {
         IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      }

      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;

      if (single_value_array) {
         IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                 storage_bit_size_tbl[exp_desc->linear_type]);
      }
      else {
         IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      }

      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
   else {
      tmp_idx			= gen_compiler_tmp(line, col, Shared, TRUE);
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;
      ATD_TYPE_IDX(tmp_idx)	= exp_desc->type_idx;

      if (exp_desc->rank) {
#ifdef _WHIRL_HOST64_TARGET64
         if (storage_bit_size_tbl[exp_desc->linear_type] > 32)
            double_stride = 1;
#endif /* _WHIRL_HOST64_TARGET64 */
         ATD_ARRAY_IDX(tmp_idx) = save_target_array_idx ? 
           save_target_array_idx : create_bd_ntry_for_const(exp_desc,
                                                            line,
                                                            col);
#ifdef _WHIRL_HOST64_TARGET64
         double_stride = 0;
#endif /* _WHIRL_HOST64_TARGET64 */
      }

      ATD_SAVED(tmp_idx)        = TRUE;
      ATD_DATA_INIT(tmp_idx)    = TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);

      if (single_value_array) {
         NTR_IR_TBL(mult_idx);
         IR_OPR(mult_idx)        = Mult_Opr;
         IR_TYPE_IDX(mult_idx)   = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(mult_idx)   = line; 
         IR_COL_NUM(mult_idx)    = col;
         IR_FLD_L(mult_idx)      = BD_LEN_FLD(ATD_ARRAY_IDX(tmp_idx));
         IR_IDX_L(mult_idx)      = BD_LEN_IDX(ATD_ARRAY_IDX(tmp_idx));
         IR_LINE_NUM_L(mult_idx) = line;
         IR_COL_NUM_L(mult_idx)  = col;
         COPY_OPND(IR_OPND_R(mult_idx), single_value_opnd);
         IR_LINE_NUM_R(mult_idx) = line;
         IR_COL_NUM_R(mult_idx)  = col;

         ATD_FLD(tmp_idx) = IR_Tbl_Idx;
         ATD_TMP_IDX(tmp_idx) = mult_idx;
      }
      else {
         ATD_FLD(tmp_idx)          = CN_Tbl_Idx;
         ATD_TMP_IDX(tmp_idx)      = the_cn_idx;
      }

      ATD_TMP_INIT_NOT_DONE(tmp_idx) = TRUE;
   }

   OPND_FLD((*top_opnd))	= AT_Tbl_Idx;
   OPND_IDX((*top_opnd))	= tmp_idx;
   OPND_LINE_NUM((*top_opnd))	= line;
   OPND_COL_NUM((*top_opnd))	= col;

   if (insert_subs_ok) {

      if (exp_desc->rank) {

         ok = gen_whole_subscript(top_opnd, &loc_exp_desc);
      }
      else if (exp_desc->type == Character) {
         ok = gen_whole_substring(top_opnd, exp_desc->rank);
      }
   }

   AT_REFERENCED(tmp_idx) = Referenced;
   AT_DEFINED(tmp_idx) = TRUE;

   exp_desc->foldable      = TRUE;
   exp_desc->tmp_reference = TRUE;
   exp_desc->constant      = TRUE;

   if (exp_desc->rank > 0) {
      exp_desc->contig_array = TRUE;
   }

   target_array_idx = save_target_array_idx;

EXIT:

   defer_stmt_expansion = save_defer_stmt_expansion;

   TRACE (Func_Exit, "create_constructor_constant", NULL);

   return(ok);

}  /* create_constructor_constant */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Fold expressions involving aggragate constants.                       *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - opnd that points to the original expression.               *|
|*	exp_desc - expression descriptor for incoming expression.             *|
|*      return_const - TRUE if you don't want to data init a tmp, but just    *|
|*			want the constant idx.                                *|
|*									      *|
|* Output parameters:							      *|
|*	top_opnd - opnd that points to the constant or tmp reference.         *|
|*	exp_desc - some fields are modified.                                  *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

boolean fold_aggragate_expression(opnd_type	*top_opnd,
				  expr_arg_type *exp_desc,
				  boolean	 return_const)

{
   int			asg_idx;
   int			bd_idx;
   char			*char_ptr;
   int			col;
   int			i;
   int			line;
   int			list_idx;
   long64		loc_char_result_offset;
   long64		loc_element;
   expr_arg_type	loc_exp_desc;
   long_type		loc_value[MAX_WORDS_FOR_NUMERIC];
   int			mult_idx;
   long64		num_elements = 1;
   boolean		ok = TRUE;
   expr_arg_type	save_exp_desc;
   int			save_target_array_idx = NULL_IDX;
   int			sub_idx;
   long64		the_constant;
   int			tmp_idx;
   int			type_idx;
   long64		zero = 0;


   TRACE (Func_Entry, "fold_aggragate_expression", NULL);

   single_value_array = FALSE;
   single_value_opnd = null_opnd;

   if (OPND_FLD((*top_opnd)) == CN_Tbl_Idx &&
       ! return_const &&
       exp_desc->type != Character &&
       exp_desc->type != Structure) {
      single_value_array = TRUE;
      COPY_OPND(single_value_opnd, (*top_opnd));
   }

   save_exp_desc = *exp_desc;
       
   find_opnd_line_and_column(top_opnd, &line, &col);

   /* before we clear the exp_desc, check to make sure we can */
   /* do any type conversions requested.                      */

   if (check_type_conversion) {

      if (! check_asg_semantics(target_type_idx, exp_desc->type_idx, 
                                                           line, col)) {
         check_type_conversion = FALSE;
      }
   }

   char_result_offset = 0;

   if (exp_desc->rank   == 0          &&
       target_array_idx == NULL_IDX   &&
#ifdef KEY /* Bug 572 */
       /* We've modified the "false" branch of this "if" to handle constant
        * pointers correctly, so we force them to take that branch. If that
	* ever turns out not to work in some circumstance (e.g. because it
	* creates a temp) then we'll need to figure out how to modify this
	* branch, which isn't obvious--is it safe to create a typeless constant
	* to hold the pointer if the constant is not associated with a temp
	* having the correct type? */
       (!(exp_desc->constant && exp_desc->pointer)) &&
#endif /* KEY Bug 572 */
       exp_desc->type   != Structure) {

      /* create normal CN entry */

      /* COPY_OPND(opnd, (*top_opnd));   BRIANJ - opnd is never used */

      if (exp_desc->type == Character &&
          (! check_type_conversion ||
           TYP_TYPE(target_type_idx) == Character)) {

         bits_in_constructor = 0;
         unequal_char_lens = FALSE;

         ok = interpret_constructor(top_opnd, exp_desc, TRUE, &zero);
   
         if (exp_desc->constant) {
            increment_count(exp_desc);
         }

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

         TYP_TYPE(TYP_WORK_IDX)		= Character;
         TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;

         if (! check_type_conversion) {
            TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(Integer_8, char_result_len);
         }
         else {
            TYP_IDX(TYP_WORK_IDX) = target_char_len_idx;
         }

         exp_desc->type_idx	= ntr_type_tbl();
         exp_desc->type		= Character;
         exp_desc->linear_type	= CHARACTER_DEFAULT_TYPE;
         exp_desc->char_len.fld = TYP_FLD(exp_desc->type_idx);
         exp_desc->char_len.idx = TYP_IDX(exp_desc->type_idx);
         words_in_constructor   = STORAGE_WORD_SIZE(bits_in_constructor);

         /* Pass NULL, so that caller can fill in constant. */

         the_cn_idx		= ntr_const_tbl(exp_desc->type_idx, TRUE, NULL);
         the_cn_bit_offset	= 0;
         ok			= interpret_constructor(top_opnd,
                                                        &loc_exp_desc,
                                                        FALSE,
                                                        &zero);
         char_result_offset	= 0;

         if (loc_exp_desc.constant) {
            write_constant(loc_exp_desc.type_idx);
         }

         the_constant = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(the_cn_idx)));

         /* BRIANJ - character manipulation */

         char_ptr = (char *)&(CN_CONST(the_cn_idx));

         while (the_constant % TARGET_CHARS_PER_WORD != 0) {
            char_ptr[the_constant] = ' ';
            the_constant++;
         }

         OPND_FLD((*top_opnd))		= CN_Tbl_Idx;
         OPND_IDX((*top_opnd))		= the_cn_idx;
         OPND_LINE_NUM((*top_opnd))	= line;
         OPND_COL_NUM((*top_opnd))	= col;
         exp_desc->constant		= TRUE;
         exp_desc->foldable		= TRUE;
      }
      else {
         ok = interpret_constructor(top_opnd, &loc_exp_desc, FALSE, &zero);
#ifdef KEY /* Bug 572 */
         if (!ok) {
	   return ok;
	 }
#endif /* KEY Bug 572 */
        
         if (loc_exp_desc.constant) {

            if (check_type_conversion) {
               type_idx = target_type_idx;
   
               for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
                  loc_value[i] = result_value[i];
               }

               ok &= folder_driver((char *)loc_value,
                                   loc_exp_desc.type_idx,
                                   NULL,
                                   NULL_IDX,
                                   result_value,
                                  &type_idx,
                                   stmt_start_line,
                                   stmt_start_col,
                                   1,
                                   Cvrt_Opr);

               exp_desc->type_idx    = target_type_idx;
               exp_desc->type        = TYP_TYPE(target_type_idx);
               exp_desc->linear_type = TYP_LINEAR(target_type_idx);
            }
            else {
               type_idx		= exp_desc->type_idx;
            }
           
            if (OPND_FLD((*top_opnd)) == CN_Tbl_Idx &&
                (! check_type_conversion ||
                 TYP_LINEAR(CN_TYPE_IDX(OPND_IDX((*top_opnd)))) ==
                                          TYP_LINEAR(target_type_idx))) {

               /* intentionally blank */
               /* just return constant */
            }
            else if ((loc_exp_desc.type == Typeless ||
                      loc_exp_desc.type == Character)  &&
                     TYP_TYPE(type_idx) == Real) {

               OPND_IDX((*top_opnd))	= ntr_unshared_const_tbl(type_idx,
                                                              FALSE,
                                                              result_value);
            }
            else {
               OPND_IDX((*top_opnd))	= ntr_const_tbl(type_idx,
                                                     FALSE,
                                                     result_value);
            }

            OPND_FLD((*top_opnd))	= CN_Tbl_Idx;

            OPND_LINE_NUM((*top_opnd))	= line;
            OPND_COL_NUM((*top_opnd))	= col;
            exp_desc->constant = TRUE;
            exp_desc->foldable = TRUE;
         }
         else {
            PRINTMSG(line, 979, Internal, col);
         }
      }
   }
   else {
   
      bits_in_constructor = 0;
      unequal_char_lens = FALSE;

      if (OPND_FLD((*top_opnd)) == IR_Tbl_Idx     &&
          IR_ARRAY_SYNTAX(OPND_IDX((*top_opnd)))) {

         loc_element = 1;
      }
      else {
         loc_element = 0;
      }

      ok = interpret_constructor(top_opnd, exp_desc, TRUE, &loc_element);

      if (exp_desc->constant) {
#ifdef KEY /* Bug 572 */
	 /* If this is a constant pointer, e.g. "parameter_x%ptr_component_y",
	  * we need to use the size of the dope vector, not the size of the
	  * target of the pointer. */
	 int top_idx, struct_idx;
         if (exp_desc->pointer &&
	    OPND_FLD((*top_opnd)) == IR_Tbl_Idx &&
	    IR_FLD_L(top_idx = OPND_IDX((*top_opnd))) == IR_Tbl_Idx &&
	    IR_FLD_R(struct_idx = IR_IDX_L(top_idx)) == AT_Tbl_Idx) {
	    bits_in_constructor += stor_bit_size_of(IR_IDX_R(struct_idx),
	      TRUE, FALSE).constant[0];
	 }
	 else
#endif /* KEY Bug 572 */
         increment_count(exp_desc);
      }

      if (exp_desc->type == Character &&
          (! check_type_conversion ||
           TYP_TYPE(target_type_idx) == Character)) {

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

         TYP_TYPE(TYP_WORK_IDX)		= Character;
         TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;

         if (! check_type_conversion) {
            TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(Integer_8, char_result_len);
         }
         else {
            TYP_IDX(TYP_WORK_IDX) = target_char_len_idx;
         }

         exp_desc->type_idx		= ntr_type_tbl();
         exp_desc->type			= Character;
         exp_desc->linear_type		= CHARACTER_DEFAULT_TYPE;
         exp_desc->char_len.fld         = TYP_FLD(exp_desc->type_idx);
         exp_desc->char_len.idx         = TYP_IDX(exp_desc->type_idx);
      }
      else if (check_type_conversion) {
         exp_desc->type_idx = target_type_idx;
         exp_desc->type     = TYP_TYPE(target_type_idx);
         exp_desc->linear_type = TYP_LINEAR(target_type_idx);
      }

      if (target_array_idx != NULL_IDX) {
   
         save_target_array_idx = target_array_idx;
      }

      if (exp_desc->rank   == 0         &&
          target_array_idx != NULL_IDX  &&
          BD_RESOLVED(target_array_idx)) {

         if (BD_LEN_FLD(target_array_idx) == CN_Tbl_Idx) {
            num_elements = CN_INT_TO_C(BD_LEN_IDX(target_array_idx));
            bits_in_constructor *= num_elements;
         }
   
         exp_desc->rank = BD_RANK(target_array_idx);
   
         for (i = 0; i < BD_RANK(target_array_idx); i++) {
            OPND_FLD(exp_desc->shape[i])  = BD_XT_FLD(target_array_idx, i + 1);
            OPND_IDX(exp_desc->shape[i])  = BD_XT_IDX(target_array_idx, i + 1);
            OPND_LINE_NUM(exp_desc->shape[i]) = line;
            OPND_COL_NUM(exp_desc->shape[i])  = col;
         }
      }

      if (! single_value_array) {

         target_array_idx	   = NULL_IDX;
         words_in_constructor      = STORAGE_WORD_SIZE(bits_in_constructor);

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Typeless;
         TYP_LINEAR(TYP_WORK_IDX)       = Long_Typeless;
         TYP_BIT_LEN(TYP_WORK_IDX)	= bits_in_constructor;
         type_idx			= ntr_type_tbl();

         /* Pass NULL, so caller can fill in constant. */

         the_cn_idx	= ntr_const_tbl(type_idx, FALSE, NULL);
         the_cn_bit_offset	= 0;

         /* fill in the constant */

         if (num_elements == 0) {
            /* intentionally blank */
         }
         else if (OPND_FLD((*top_opnd)) == IR_Tbl_Idx     &&
                  IR_ARRAY_SYNTAX(OPND_IDX((*top_opnd)))) {

            loc_element = 1;
            while (loc_element >= 0) {
               loc_char_result_offset = char_result_offset;
               ok = interpret_constructor(top_opnd, &loc_exp_desc, FALSE, 
                                          &loc_element);
               char_result_offset= loc_char_result_offset;
   
               if (loc_exp_desc.constant) {
                  write_constant(loc_exp_desc.type_idx);
               }
            }
         }
         else {
            loc_char_result_offset = char_result_offset;
            ok = interpret_constructor(top_opnd, &loc_exp_desc, FALSE, &zero);
            char_result_offset= loc_char_result_offset;
   
            if (loc_exp_desc.constant) {
               write_constant(loc_exp_desc.type_idx);
            }
   
            if (num_elements > 1) {
               bcast_cn_bit_offset = 0;
               broadcast_scalar(exp_desc, num_elements);
            }
         }
      } /* ! single_value_array */
      else {

         if (check_type_conversion &&
             TYP_LINEAR(CN_TYPE_IDX(OPND_IDX(single_value_opnd))) !=
                   TYP_LINEAR(target_type_idx)) {
            /* convert the constant */

            cast_to_type_idx(&single_value_opnd,
                             &save_exp_desc,
                             target_type_idx);
         }
      }


      if (return_const) {
         OPND_FLD((*top_opnd)) = CN_Tbl_Idx;
         OPND_IDX((*top_opnd)) = the_cn_idx;
         OPND_LINE_NUM((*top_opnd)) = line;
         OPND_COL_NUM((*top_opnd))  = col;
         exp_desc->constant = TRUE;
         exp_desc->foldable = TRUE;
         goto EXIT;
      }

      if (OPND_FLD(init_target_opnd) != NO_Tbl_Idx) {
         tmp_idx = find_left_attr(&init_target_opnd);

         if (do_constructor_init) {
   
            /* create data init stmt */
            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Init_Opr;
            IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;
            IR_LINE_NUM_L(asg_idx) = line;
            IR_COL_NUM_L(asg_idx)  = col;

            if (single_value_array &&
                OPND_FLD(init_target_opnd) == AT_Tbl_Idx) {

               bd_idx = ATD_ARRAY_IDX(tmp_idx);

               NTR_IR_TBL(sub_idx);
               IR_OPR(sub_idx) = Subscript_Opr;
               IR_TYPE_IDX(sub_idx) = ATD_TYPE_IDX(tmp_idx);
               IR_LINE_NUM(sub_idx) = line;
               IR_COL_NUM(sub_idx) = col;
               IR_FLD_L(sub_idx) = AT_Tbl_Idx;
               IR_IDX_L(sub_idx) = tmp_idx;
               IR_LINE_NUM_L(sub_idx) = line;
               IR_COL_NUM_L(sub_idx) = col;

               IR_FLD_L(asg_idx) = IR_Tbl_Idx;
               IR_IDX_L(asg_idx) = sub_idx;

               NTR_IR_LIST_TBL(list_idx);
               IR_FLD_R(sub_idx) = IL_Tbl_Idx;
               IR_LIST_CNT_R(sub_idx) = BD_RANK(bd_idx);
               IR_IDX_R(sub_idx) = list_idx;

               IL_FLD(list_idx) = BD_LB_FLD(bd_idx, 1);
               IL_IDX(list_idx) = BD_LB_IDX(bd_idx, 1);
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx)  = col;

               for (i = 2; i <= BD_RANK(bd_idx); i++) {
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);

                  IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
                  IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
                  IL_LINE_NUM(list_idx) = line;
                  IL_COL_NUM(list_idx)  = col;
               }
            }
            else {
               COPY_OPND(IR_OPND_L(asg_idx), init_target_opnd);
            }

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(asg_idx) = IL_Tbl_Idx;
            IR_IDX_R(asg_idx) = list_idx;
            IR_LIST_CNT_R(asg_idx) = 3;

            IL_FLD(list_idx) = CN_Tbl_Idx;
            IL_IDX(list_idx) = (single_value_array ?
                           OPND_IDX(single_value_opnd) : the_cn_idx);
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            IL_FLD(list_idx) = CN_Tbl_Idx;

            if (single_value_array) {
               IL_IDX(list_idx) = BD_LEN_IDX(ATD_ARRAY_IDX(tmp_idx));
            }
            else {
               IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
            }

            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            IL_FLD(list_idx) = CN_Tbl_Idx;

            if (single_value_array) {
               IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                   storage_bit_size_tbl[exp_desc->linear_type]);
            }
            else {
               IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
            }

            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            gen_sh(Before, Assignment_Stmt, line, col,
                   FALSE, FALSE, TRUE);
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         }
      }
      else {
         tmp_idx		= gen_compiler_tmp(line, col, Shared, TRUE);
         AT_SEMANTICS_DONE(tmp_idx)	= TRUE;
         ATD_TYPE_IDX(tmp_idx)		= exp_desc->type_idx;

         if (exp_desc->rank) {
            ATD_ARRAY_IDX(tmp_idx) = save_target_array_idx ?
                  save_target_array_idx : create_bd_ntry_for_const(exp_desc,
                                                                   line,
                                                                   col);
         }

         ATD_SAVED(tmp_idx)        = TRUE;
         ATD_DATA_INIT(tmp_idx)    = TRUE;
         ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);

         if (single_value_array) {
            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx)        = Mult_Opr;
            IR_TYPE_IDX(mult_idx)   = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx)   = line;
            IR_COL_NUM(mult_idx)    = col;
            IR_FLD_L(mult_idx)      = BD_LEN_FLD(ATD_ARRAY_IDX(tmp_idx));
            IR_IDX_L(mult_idx)      = BD_LEN_IDX(ATD_ARRAY_IDX(tmp_idx));
            IR_LINE_NUM_L(mult_idx) = line;
            IR_COL_NUM_L(mult_idx)  = col;
            COPY_OPND(IR_OPND_R(mult_idx), single_value_opnd);
            IR_LINE_NUM_R(mult_idx) = line;
            IR_COL_NUM_R(mult_idx)  = col;

            ATD_FLD(tmp_idx) = IR_Tbl_Idx;
            ATD_TMP_IDX(tmp_idx) = mult_idx;
         }
         else {
            ATD_FLD(tmp_idx)          = CN_Tbl_Idx;
            ATD_TMP_IDX(tmp_idx)      = the_cn_idx;
         }

         if (do_constructor_init) {
            ATD_TMP_INIT_NOT_DONE(tmp_idx) = TRUE;
         }
      }

#ifdef KEY /* Bug 572 */
      /* It's a pointer, e.g. "parameter_x%ptr_component_y", so we must
       * mark the temporary accordingly. We must also put a "Dv_Deref_Opr"
       * atop it, because later semantics processing demands that every
       * pointer will have a "Dv_Deref_Opr", so that it can remove the
       * operator if the context doesn't dictate the dereferencing. */
      if (exp_desc->pointer) {
	 ATD_POINTER(tmp_idx) = ATD_IM_A_DOPE(tmp_idx) = TRUE;
	 int save_tmp_idx = tmp_idx;
	 NTR_IR_TBL(tmp_idx);
	 IR_OPR(tmp_idx)          = Dv_Deref_Opr;
	 IR_LINE_NUM(tmp_idx)     = line;
	 IR_COL_NUM(tmp_idx)      = col;
	 IR_TYPE_IDX(tmp_idx)	= exp_desc->type_idx;
	 IR_FLD_L(tmp_idx)        = AT_Tbl_Idx;
	 IR_IDX_L(tmp_idx)        = save_tmp_idx;
	 IR_LINE_NUM_L(tmp_idx)   = line;
	 IR_COL_NUM_L(tmp_idx)    = col;
	 OPND_FLD((*top_opnd)) = IR_Tbl_Idx;
      }
      else
#endif /* KEY Bug 572 */
      OPND_FLD((*top_opnd)) = AT_Tbl_Idx;
      OPND_IDX((*top_opnd)) = tmp_idx;
      OPND_LINE_NUM((*top_opnd)) = line;
      OPND_COL_NUM((*top_opnd))  = col;

      if (insert_subs_ok) {

         if (exp_desc->rank) {
            ok = gen_whole_subscript(top_opnd, &loc_exp_desc);
         }
         else if (exp_desc->type == Character) {
            ok = gen_whole_substring(top_opnd, 0);
         }
      }

      AT_REFERENCED(tmp_idx) = Referenced;
      AT_DEFINED(tmp_idx) = TRUE;

      exp_desc->foldable      = TRUE;
      exp_desc->constructor   = TRUE;
      exp_desc->tmp_reference = TRUE;
      exp_desc->constant      = TRUE;

      if (exp_desc->rank > 0) {
         exp_desc->contig_array = TRUE;
      }
   }

   target_array_idx = save_target_array_idx;

EXIT:

   TRACE (Func_Exit, "fold_aggragate_expression", NULL);

   return(ok);

}  /* fold_aggragate_expression */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This is a way to use the interpret_constructor system that is used    *|
|*      by data stmt processing to handle subscripts. These subscript         *|
|*      expressions may involve array expressions. This routine will return   *|
|*      the next value from the array expression.                             *|
|*      element should be set to 1 and the variable resent to this routine    *|
|*      for each new value. This system updates element internally.           *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - the array expression, it is modified by this system so the *|
|*                 same opnd must be resent to this routine until the array   *|
|*                 values are exhausted.                                      *|
|*      element - this is a integer flag.                                     *|
|*			0 => scalar ref (not used for data processing)        *|
|*			1 => return the first value, this causes the system   *|
|*   			     to modify the tree to maintain it's position     *|
|*			     in the array expression.                         *|
|*		       >1 => get next value.                                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	CN_Tbl_Idx idx for next value.                                        *|
|*									      *|
\******************************************************************************/

int	get_next_array_expr_element(opnd_type		*top_opnd,
				    long64		*element)

{
   int				const_idx = NULL_IDX;
   expr_arg_type		exp_desc;
   boolean			unused;


   TRACE (Func_Entry, "get_next_array_expr_element", NULL);

   unused = interpret_constructor(top_opnd, &exp_desc, FALSE, element);

   if (! no_result_value) {
      const_idx = ntr_const_tbl(exp_desc.type_idx,
                                FALSE,
                                result_value);
   }

   TRACE (Func_Exit, "get_next_array_expr_element", NULL);

   return(const_idx);

}  /* get_next_array_expr_element */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This is the main processor for constant constructors and aggregate    *|
|*	constant references. It is a recursive routine that is set up like    *|
|*	expr_semantics with 2 nested switches. It calls sub processors to     *|
|* 	handle implied do's and references. The input argument "count"        *|
|*      controls the two basic states for this routine. If count is true,     *|
|*	this routine simply determines the number of elements in the          *|
|* 	expression. It also checks array syntax conformance. If count is      *|
|*	false, the constant values are propagated up in the global variable   *|
|*	result_value or they are placed in the result constant, depending on  *|
|*	the context. Folding routines are called from this routine.           *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - incoming tree.                                             *|
|*	count    - TRUE if this is the count phase.                           *|
|*	element  - flag for array syntax,                                     *|
|*		   0 => scalar operation, no array syntax                     *|
|*		   1 => array syntax, modify tree to maintain position.       *|
|*		  >1 => in array syntax, tree is already modified, get next.  *|
|*	    return -1 means done with array expression.                       *|
|*									      *|
|* Output parameters:							      *|
|*	exp_desc - expression desciptor for tree is returned with basic info. *|
|*	element  - is updated if greater than 0.                              *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

static boolean interpret_constructor(opnd_type		*top_opnd,
                                     expr_arg_type	*exp_desc,
                                     boolean		 count,
				     long64		*element)

{
   int			attr_idx;
   int			new_spec_idx;
   char			*char_ptr;
   char			*char_ptr2;
   long64		char_strct_len;
   int			cn_idx;
   int			col;
   long64		i;
   int			ir_idx;
   long64		k;
   int			line;
   expr_arg_type	loc_exp_desc;
   boolean		ok = TRUE;
   opnd_type            opnd;
   int			param_cn_idx;
   save_env_type        save;
   int			type_idx;


   TRACE (Func_Entry, "interpret_constructor", NULL);

   (*exp_desc)     = init_exp_desc;
   no_result_value = FALSE;

   find_opnd_line_and_column(top_opnd, &line, &col);

   switch (OPND_FLD((*top_opnd))) {

      case NO_Tbl_Idx :
         break;

      case CN_Tbl_Idx:

         cn_idx			= OPND_IDX((*top_opnd));
         type_idx		= CN_TYPE_IDX(cn_idx);
         exp_desc->constant	= TRUE;

         exp_desc->type_idx	= CN_TYPE_IDX(cn_idx);
         exp_desc->type		= TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         if (exp_desc->type == Character &&
             compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                                  MAX_CHARS_IN_TYPELESS,
                                  Le_Opr)) {
            exp_desc->linear_type = Short_Char_Const;
         }

         if (*element > 0 && !count) {
            *element = -1;
         }

         if (exp_desc->linear_type == Short_Typeless_Const &&
             check_type_conversion) {

            cn_idx = cast_typeless_constant(cn_idx,
                                            target_type_idx,
					    line,
					    col);

            type_idx = target_type_idx;
            exp_desc->type_idx = type_idx;
            exp_desc->type     = TYP_TYPE(type_idx);
            exp_desc->linear_type = TYP_LINEAR(type_idx);
            OPND_IDX((*top_opnd)) = cn_idx;
         }

         switch (TYP_TYPE(type_idx)) {
            case Typeless :
               for (i = 0; 
                    i < (TYP_BIT_LEN(type_idx)/TARGET_BITS_PER_WORD); 
                    i++) {

                  result_value[i] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + i);
               }
               break;

            case Integer  :
            case Logical  :
            case Real     :
            case Complex :
               for (i = 0; i < num_host_wds[TYP_LINEAR(type_idx)]; i++) {
                  result_value[i] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + i);
               }
               break;

            case Character :

               if (count) {
                  char_result_len = CN_INT_TO_C(TYP_IDX(exp_desc->type_idx));

                  if (char_result_len < 0) {
                     char_result_len = 0;
                  }
               }
               else {
                  result_value[0] = CN_CONST(cn_idx);
                  char_result_len = CN_INT_TO_C(TYP_IDX(exp_desc->type_idx));

                  if (char_result_len < 0) {
                     char_result_len = 0;
                  }

                  if (char_result_offset + 
                           CN_INT_TO_C(TYP_IDX(exp_desc->type_idx)) >=
                      char_result_buffer_len) {

                     enlarge_char_result_buffer();
                  }

                  char_ptr = (char *)&(CN_CONST(cn_idx));

                  for (i = 0; i < CN_INT_TO_C(TYP_IDX(exp_desc->type_idx)); 
                                                                       i++) {

                     char_result_buffer[char_result_offset] = char_ptr[i];
                     char_result_offset++;
                     
                  }
               }
               break;

         }
         break;

      case AT_Tbl_Idx  :

         attr_idx = OPND_IDX((*top_opnd));
         type_idx = ATD_TYPE_IDX(attr_idx);

         if (*element > 0 && !count) {
            *element = -1;
         }

         exp_desc->type_idx	= ATD_TYPE_IDX(attr_idx);
         exp_desc->type		= TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         if (exp_desc->type == Character &&
             compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                                  MAX_CHARS_IN_TYPELESS,
                                  Le_Opr)) {
            exp_desc->linear_type = Short_Char_Const;
         }

         if (ATD_LCV_IS_CONST(attr_idx)) {

            exp_desc->constant = TRUE;

            switch (TYP_TYPE(type_idx)) {
               case Integer  :
               case Typeless :
               case Real    :
                  GET_LCV_CONST(attr_idx, result_value[0],   /* target const*/
                                num_host_wds[TYP_LINEAR(type_idx)]);
                  break;

               default :
                  PRINTMSG(line, 980, Internal, col); 
                  break;

            }
         }
         else if (TYP_TYPE(type_idx) == Structure) {

            /* whole structure parameter reference */
            if (! count) {

               if (ATD_FLD(attr_idx) != CN_Tbl_Idx) {
                  PRINTMSG(line, 981, Internal, col);
                  break;
               }
               param_cn_idx = ATD_TMP_IDX(attr_idx);

               if (ATT_CHAR_SEQ(TYP_IDX(exp_desc->type_idx))) {

                  /* Should we div by 8?  BRIANJ */

                  char_strct_len = CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(
                                      TYP_IDX(exp_desc->type_idx))) >> 3;

                  char_ptr = (char *) &(CN_CONST(the_cn_idx)) + 
                                                 (the_cn_bit_offset/CHAR_BIT);

                  char_ptr2 = (char *)&(CN_CONST(param_cn_idx));

                  the_cn_bit_offset += char_strct_len * CHAR_BIT;

                  for (i = 0; i < char_strct_len; i++) {
                     char_ptr[i] = char_ptr2[i];
                  }

               }
#ifdef KEY /* Bug 7387 */
	       /* Special case where the constant is "null()" */
	       else if (ATD_POINTER(attr_idx) &&
		 ATD_CLASS(attr_idx) == Compiler_Tmp &&
                 ATD_TMP_INIT_NOT_DONE(attr_idx) &&
                 ATD_FLD(attr_idx) == CN_Tbl_Idx) {
		 k = TARGET_BITS_TO_WORDS(the_cn_bit_offset);
		 for (i = 0;
		   i < STORAGE_WORD_SIZE(TYP_BIT_LEN(CN_TYPE_IDX(ATD_TMP_IDX(attr_idx))));
		   i += 1, k += 1) {
		   CP_CONSTANT(CN_POOL_IDX(the_cn_idx) + k) = 
			       CP_CONSTANT(CN_POOL_IDX(param_cn_idx) + i);
		 }
		 the_cn_bit_offset += i * TARGET_BITS_PER_WORD;
	       }
#endif /* KEY Bug 7387 */
               else {

                  k = TARGET_BITS_TO_WORDS(the_cn_bit_offset);

                  for (i = 0; 
                       i < STORAGE_WORD_SIZE(CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(
                                                TYP_IDX(exp_desc->type_idx))));
                       i++) {
                     CP_CONSTANT(CN_POOL_IDX(the_cn_idx) + k) = 
                                 CP_CONSTANT(CN_POOL_IDX(param_cn_idx) + i);

                     k++;
                  }

                  the_cn_bit_offset += i * TARGET_BITS_PER_WORD;
               }
            }
            else {
               /* count is true, so set constant flag to get count above */
               exp_desc->constant = TRUE;
            }
         }
         else if (ATD_IM_A_DOPE(attr_idx)) {
            /* Null intrinsic temp */
            if (! count) {

               if (ATD_FLD(attr_idx) != CN_Tbl_Idx) {
                  PRINTMSG(line, 981, Internal, col);
                  break;
               }
               param_cn_idx = ATD_TMP_IDX(attr_idx);

               k = TARGET_BITS_TO_WORDS(the_cn_bit_offset);

               for (i = 0;
                    i < STORAGE_WORD_SIZE(
                           TYP_BIT_LEN(CN_TYPE_IDX(param_cn_idx)));
                    i++) {


                  CP_CONSTANT(CN_POOL_IDX(the_cn_idx) + k) =
                              CP_CONSTANT(CN_POOL_IDX(param_cn_idx) + i);

                  k++;
               }

               the_cn_bit_offset += i * TARGET_BITS_PER_WORD;
            }
            else {
               /* count is true, so set constant flag to get count above */
               exp_desc->constant = TRUE;
            }

         }
         else {
            PRINTMSG(line, 982, Internal, col);
         }

         break;

      case IR_Tbl_Idx  :

         ir_idx = OPND_IDX((*top_opnd));

         switch (IR_OPR(ir_idx)) {
            case Null_Opr              :
               break;

            case Dv_Deref_Opr :
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = interpret_constructor(&opnd, exp_desc, count, element);
               break;

            case Struct_Construct_Opr  :
            case Constant_Struct_Construct_Opr  :

               ok = interpret_struct_construct_opr(ir_idx, exp_desc,
                                                   count, element);
               break;

            case Array_Construct_Opr   :
            case Constant_Array_Construct_Opr   :

               ok = interpret_array_construct_opr(ir_idx, exp_desc,
                                                  count, element);
               break;

            case Implied_Do_Opr        :

               ok = interpret_implied_do(ir_idx, exp_desc, count, element);

               exp_desc->type_idx	= IR_TYPE_IDX(ir_idx);
               exp_desc->type		= TYP_TYPE(exp_desc->type_idx);
               exp_desc->linear_type	= TYP_LINEAR(exp_desc->type_idx);
               break;

            case Uplus_Opr             :
            case Uminus_Opr            :
            case Cvrt_Opr              :
            case Cvrt_Unsigned_Opr     :
            case Not_Opr               :
            case Bnot_Opr              :

               ok = interpret_unary_opr(ir_idx, exp_desc, count, element);
               break;


            case Power_Opr             :
            case Mult_Opr              :
            case Div_Opr               :
            case Minus_Opr             :
            case Plus_Opr              :
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

               ok = interpret_binary_opr(ir_idx, exp_desc, count, element);
               break;


            case Concat_Opr            :

               ok = interpret_concat_opr(ir_idx, exp_desc, count, element);
               break;


            case Struct_Opr            :
            case Whole_Subscript_Opr   :
            case Section_Subscript_Opr :
            case Subscript_Opr         :
            case Whole_Substring_Opr   :
            case Substring_Opr         :

               if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
                   IR_OPR(IR_IDX_L(ir_idx)) == Dv_Deref_Opr) {
                  COPY_OPND(opnd, IR_OPND_L(IR_IDX_L(ir_idx)));
                  ok = interpret_constructor(&opnd, exp_desc, count, element);
               }
               else if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
                        IR_FLD_L(IR_IDX_L(ir_idx)) == IR_Tbl_Idx &&
                        IR_OPR(IR_IDX_L(IR_IDX_L(ir_idx))) == Dv_Deref_Opr) {
                  COPY_OPND(opnd, IR_OPND_L(IR_IDX_L(IR_IDX_L(ir_idx))));
                  ok = interpret_constructor(&opnd, exp_desc, count, element);
               }
               else {
                  ok = interpret_ref(top_opnd, exp_desc, count, element);
               }
               break;

            case Stmt_Expansion_Opr    :

               if (IR_LIST_CNT_R(ir_idx) == 5) {
                  /* replace with unflattened call */
                  COPY_OPND(IR_OPND_L(ir_idx),
                            IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                    IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                    IR_IDX_R(ir_idx)))))));

                  IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                    IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                    IR_IDX_R(ir_idx))))) = NULL_IDX;
                  IR_LIST_CNT_R(ir_idx) = 4;
               }

               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = interpret_constructor(&opnd, exp_desc, count, element);
               break;

            case Paren_Opr             :

               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = interpret_constructor(&opnd, exp_desc, count, element);

               break;

            case Stmt_Func_Call_Opr :
               /* expand the stmt function. */
               process_deferred_functions(top_opnd);

               ok = interpret_constructor(top_opnd, exp_desc, count, element);
               break;

            /*********************************\
            |* NEXT COME THE INTRINSIC OPRS. *|
            \*********************************/

            case Call_Opr :
# ifdef _DEBUG
               if (! AT_IS_INTRIN(IR_IDX_L(ir_idx))) {
                  PRINTMSG(IR_LINE_NUM_L(ir_idx), 904, Internal,
                           IR_COL_NUM_L(ir_idx));
               }
# endif


               switch (ATP_INTRIN_ENUM(IR_IDX_L(ir_idx))) {
               case Trim_Intrinsic:

                  ok = interpret_trim_intrinsic(ir_idx, exp_desc, count,
                                                element);
                  break;

               case Adjustl_Intrinsic:
               case Adjustr_Intrinsic:

                  ok = interpret_adjustl_intrinsic(ir_idx, exp_desc, count,
                                                   element);
                  break;

               case Repeat_Intrinsic:

                  ok = interpret_repeat_intrinsic(ir_idx, exp_desc, count,
                                                  element);
                  break;

               case Transfer_Intrinsic:

                  ok = interpret_transfer_intrinsic(ir_idx, exp_desc, count,
                                                    element);
                  break;

               case Reshape_Intrinsic:

                  ok = interpret_reshape_intrinsic(ir_idx, exp_desc, count,
                                                   element);
                  break;

               case Size_Intrinsic:

                  ok = interpret_size_intrinsic(ir_idx, exp_desc, count,
                                                element);
                  break;

               case Ubound_Intrinsic:

                  ok = interpret_ubound_intrinsic(ir_idx, exp_desc, count,
                                                  element);
                  break;

               case Shape_Intrinsic:

                  ok = interpret_shape_intrinsic(ir_idx, exp_desc, count,
                                                 element);
                  break;

               case SIK_Intrinsic:

                  ok = interpret_sik_intrinsic(ir_idx, exp_desc, count,
                                               element);
                  break;

               case SRK_Intrinsic:

                  ok = interpret_srk_intrinsic(ir_idx, exp_desc, count,
                                               element);
                  break;

               default :

                  loc_exp_desc = init_exp_desc;

                  SAVE_ENV;
                  check_type_conversion = FALSE;

                  (*(void (*)())intrinsic_semantics[
                                      ATP_INTRIN_ENUM(IR_IDX_L(ir_idx))] )
                                                    (top_opnd,
                                                     &loc_exp_desc,
                                                     IR_IDX_L(ir_idx),
                                                     &new_spec_idx);

                  RESTORE_ENV;

                  ok = interpret_constructor(top_opnd,exp_desc,count,element);
                  break;

               }
               break;

            /*************************\
            |* UNARY INTRINSIC OPRS. *|
            \*************************/

            case Abs_Opr :
            case Sin_Opr :
            case Cos_Opr :
            case Log_E_Opr :
            case Log_10_Opr :
            case Tan_Opr :
            case Tanh_Opr :
            case Sinh_Opr :
            case Atan_Opr :
            case Cosh_Opr :
            case Aimag_Opr :
            case Sqrt_Opr :
            case Cot_Opr :
            case Exp_Opr :
            case Int_Opr :
            case Anint_Opr :
            case Nint_Opr :
            case Aint_Opr :
            case Exponent_Opr :
            case Fraction_Opr :
            case Spacing_Opr :
            case Len_Trim_Opr :
            case Rrspacing_Opr :
            case Ichar_Opr :
            case Char_Opr :
            case Adjustl_Opr :
            case Adjustr_Opr :
            case Mask_Opr :


               ok = interpret_unary_intrinsic_opr(ir_idx, exp_desc, count,
                                                  element);
               break;


            /**************************\
            |* BINARY INTRINSIC OPRS. *|
            \**************************/

            case Mod_Opr :
            case Modulo_Opr :
            case Shift_Opr :
            case Shiftl_Opr :
            case Shiftr_Opr :
            case Shifta_Opr :
            case Dim_Opr :
            case Sign_Opr :
            case Lge_Opr :
            case Lgt_Opr :
            case Lle_Opr :
            case Llt_Opr :
            case Nearest_Opr :
            case Scale_Opr :
            case Set_Exponent_Opr :

               ok = interpret_binary_intrinsic_opr(ir_idx, exp_desc, count,
                                                   element);
               break;



            case Max_Opr :
            case Min_Opr :

               ok = interpret_max_min_opr(ir_idx, exp_desc, count,
                                          element);
               break;

            case Csmg_Opr :
            case Ishftc_Opr :
            case Ibits_Opr :
               ok = interpret_csmg_opr(ir_idx, exp_desc, count, element);
               break;

#ifdef KEY /* Bug 10410 */
            case Cselect_Opr :
#endif /* KEY Bug 10410 */
            case Cvmgt_Opr :
               ok = interpret_cvmgt_opr(ir_idx, exp_desc, count, element);
               break;

            case Index_Opr :
            case Verify_Opr :
            case Scan_Opr :

               ok = interpret_index_opr(ir_idx, exp_desc, count,
                                        element);
               break;

            /*************************\
            |* N-ARY INTRINSIC OPRS. *|
            |* and other oprs, not   *|
            |* all foldable.         *|
            \*************************/

# ifdef _TARGET_OS_MAX
            case My_Pe_Opr :
# ifdef _F_MINUS_MINUS
               if (cmd_line_flags.co_array_fortran) {
                  /* just fill in 1. It will be stripped off in pdgcs */
                  OPND_FLD((*top_opnd)) = CN_Tbl_Idx;
                  OPND_IDX((*top_opnd)) = CN_INTEGER_ONE_IDX;
                  OPND_LINE_NUM((*top_opnd)) = IR_LINE_NUM(ir_idx);
                  OPND_COL_NUM((*top_opnd)) = IR_COL_NUM(ir_idx);
                  ok = interpret_constructor(top_opnd,exp_desc,count,element);
               }
               else {
                  PRINTMSG(IR_LINE_NUM(ir_idx), 895, Internal,
                           IR_COL_NUM(ir_idx));
               }
               break;
# endif
            /* otherwise this falls through */
# endif

            default:
               PRINTMSG(IR_LINE_NUM(ir_idx), 895, Internal,
                        IR_COL_NUM(ir_idx));
               break;
         }

         break;

      case IL_Tbl_Idx :
         break;

   }

   TRACE (Func_Exit, "interpret_constructor", NULL);

   return(ok);

}  /* interpret_constructor */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	increment the global count variable according to the info in exp_desc.*|
|*									      *|
|* Input parameters:							      *|
|*	exp_desc - this holds type and rank.                                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void increment_count(expr_arg_type	*exp_desc)

{

   int		i;
   long64	num_elements = 1;


   TRACE (Func_Entry, "increment_count", NULL);

   if (exp_desc->rank > 0) {
      for (i = 0; i < exp_desc->rank; i++) {
         num_elements *= CN_INT_TO_C(exp_desc->shape[i].idx);
      }
   }

   if (check_type_conversion) {

      if (TYP_LINEAR(target_type_idx) == Character_1) {

         /* figure length from target_char_len_idx */

         bits_in_constructor += CN_INT_TO_C(target_char_len_idx) *  
                                            num_elements * 8;
      }
      else {
         bits_in_constructor += storage_bit_size_tbl[
                  TYP_LINEAR(target_type_idx)] * num_elements;
      }
   }
   else {
      switch (exp_desc->type) {
         case Typeless :
            bits_in_constructor += TYP_BIT_LEN(exp_desc->type_idx)
                                 * num_elements;
            break;

         case Integer :
         case Logical :
         case Real :
         case Complex :
            bits_in_constructor += storage_bit_size_tbl[
                                   exp_desc->linear_type] * num_elements;
            break;

         case Character:
            bits_in_constructor += char_result_len * num_elements * 8;
            break;

         case Structure :
            bits_in_constructor += CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(
                                       exp_desc->type_idx))) * num_elements;
            break;
      }
   }

   TRACE (Func_Exit, "increment_count", NULL);

   return;

}  /* increment_count */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Write values into the constant entry.                                 *|
|*									      *|
|* Input parameters:							      *|
|*	type_idx = type table idx for value.                                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void write_constant(int			type_idx)

{
#ifdef KEY /* Bug 10177 */
   long64		 bits = 0;
#else /* KEY Bug 10177 */
   long64		 bits;
#endif /* KEY Bug 10177 */
   char			*char_ptr;
   long64		 cn_word_offset;
   long64		 i;
   int			 j;
   long_type		 loc_value[MAX_WORDS_FOR_NUMERIC];
   int			 loc_type_idx;
   long64		 target_char_len;
   basic_type_type	 type;
   long64		 words;


   TRACE (Func_Entry, "write_constant", NULL);

   if (no_result_value) {
      goto DONE;
   }

   type		= TYP_TYPE(type_idx);

   if (check_type_conversion) {
   
      if (TYP_LINEAR(target_type_idx) == Character_1) {

         char_ptr = (char *) &(CN_CONST(the_cn_idx)) + 
                          (the_cn_bit_offset/CHAR_BIT);
   
         target_char_len = CN_INT_TO_C(target_char_len_idx);
         the_cn_bit_offset +=  target_char_len * CHAR_BIT;
  
         if (char_result_len < target_char_len) {
   
            for (i = 0; i < char_result_len; i++) {
               char_ptr[i] = char_result_buffer[char_result_offset + i];
            }
   
            for (i = char_result_len; i < target_char_len; i++) {
               char_ptr[i] = ' ';
            }
         }
         else {
   
            for (i = 0; i < target_char_len; i++) {
               char_ptr[i] = char_result_buffer[char_result_offset + i];
            }
         }
   
         goto DONE;
      }
      else {
         bits = storage_bit_size_tbl[TYP_LINEAR(target_type_idx)];
   
         for (j = 0; j < MAX_WORDS_FOR_NUMERIC; j++) {
            loc_value[j] = result_value[j];
         }

         loc_type_idx = target_type_idx;

         if (folder_driver((char *)loc_value,
                           type_idx,
                           NULL,
                           NULL_IDX,
                           result_value,
                          &loc_type_idx,
                           stmt_start_line,
                           stmt_start_col,
                           1,
                           Cvrt_Opr)) {
             /* intentionally blank */
         }

         type_idx = loc_type_idx;
      }
   }
   else {
      switch (type) {
         case Typeless :
            bits = TYP_BIT_LEN(type_idx);
            break;

         case Integer :
         case Logical :
         case Real :
         case Complex :
            bits = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
            break;

         case Character:
            char_ptr = (char *) &(CN_CONST(the_cn_idx)) 
                          + (the_cn_bit_offset/CHAR_BIT);

            the_cn_bit_offset += char_result_len * CHAR_BIT;

            for (i = 0; i < char_result_len; i++) {
               char_ptr[i] = char_result_buffer[char_result_offset + i];
            }
            goto DONE;

         case Structure :
            printf("invalid type in write_constant\n");
            goto DONE;
      }
   }
   

   if (bits % TARGET_BITS_PER_WORD != 0) {
      if (bits < TARGET_BITS_PER_WORD) {

         cn_word_offset = the_cn_bit_offset/TARGET_BITS_PER_WORD;

         if (bits == 8) {
            result_value[0] = result_value[0] & 0XFF;
         }
         else if (bits == 16) {
            result_value[0] = result_value[0] & 0XFFFF;
         }
         else if (bits == 32) {
            result_value[0] = result_value[0] & 0XFFFFFFFF;
         }

# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
         CP_CONSTANT(CN_POOL_IDX(the_cn_idx)+cn_word_offset) |=
               result_value[0] << (the_cn_bit_offset % TARGET_BITS_PER_WORD);
# else
         CP_CONSTANT(CN_POOL_IDX(the_cn_idx)+cn_word_offset) |=
               result_value[0] << ((TARGET_BITS_PER_WORD - 
                    the_cn_bit_offset % TARGET_BITS_PER_WORD) - bits);
# endif
# ifdef _DEBUG
         if (dump_flags.constant_bits) {
            long neg_one = -2;
            long_type _constant;
            _constant = CP_CONSTANT(CN_POOL_IDX(the_cn_idx)+cn_word_offset);
            write(1,&_constant,
                  sizeof(long_type));
            write(1,&neg_one, 4);
         }
# endif

      }
      else {
         printf("problem in write_constant\n");
      }
   }
   else {
      words = TARGET_BITS_TO_WORDS(bits);

      cn_word_offset = TARGET_BITS_TO_WORDS(the_cn_bit_offset);

      for (i = 0; i < words; i++) {
         CP_CONSTANT(CN_POOL_IDX(the_cn_idx)+cn_word_offset) = result_value[i];
         cn_word_offset++;
      }
   }

   the_cn_bit_offset += bits;

DONE:

   TRACE (Func_Exit, "write_constant", NULL);

   return;

}  /* write_constant */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process implied do's for constant array constructors.                 *|
|*	The basic mechanism is that the loop in run as a "c" for loop with    *|
|*	the current loop control value stored in the lcv_idx (tmp) attr       *|
|*      entry, overwriting the attr information of word 2. The routine        *|
|*      interpret_constructor can then pull this value out of the attr when   *|
|*	it is encountered.                                                    *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx - implied_do_opr.                                              *|
|*      count  - TRUE if this is the count phase.                             *|
|*	element- array syntax flag (see interpret_constructor)                *|
|*									      *|
|* Output parameters:							      *|
|*	exp_desc - some fields are filled in (ie. shape, rank)                *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no error.                                                     *|
|*									      *|
\******************************************************************************/

static boolean interpret_implied_do(int		   ir_idx,
				    expr_arg_type *exp_desc,
				    boolean	   count,
				    long64	  *element)

{
   int			col;
   operator_type	compare_opr = Le_Opr;
   long_type		end_value[MAX_WORDS_FOR_NUMERIC];
   expr_arg_type        exp_desc_l;
   long64		extent;
   int			i;
   int			lcv_idx;
   long_type		lcv_value[MAX_WORDS_FOR_NUMERIC];
   linear_type_type	lin_type;
   int			line;
   int			list_idx;
   int			list2_idx;
   int			list3_idx;
   long64		loc_char_result_offset;
   long64		loc_element = 0;
   long_type		loc_value[MAX_WORDS_FOR_NUMERIC];
   long64		longest_char_len = 0;
   boolean 		ok = TRUE;
   opnd_type		opnd;
   int			position_idx;
#ifdef KEY /* Bug 10177 */
   opnd_type		save_atd_tmp_opnd = INIT_OPND_TYPE;
#else /* KEY Bug 10177 */
   opnd_type		save_atd_tmp_opnd;
#endif /* KEY Bug 10177 */
   long_type		start_value[MAX_WORDS_FOR_NUMERIC];
   long_type		stride_value[MAX_WORDS_FOR_NUMERIC];
   long64		sub_elements;
   int			type_idx;
   int			unused;


   TRACE (Func_Entry, "interpret_implied_do", NULL);

   list_idx = IR_IDX_R(ir_idx);
   lcv_idx = IL_IDX(list_idx);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   extent = 0L;

   if (*element == 0) {

      /* not in array syntax */

      if (! count) {
         /* clear the referenced field so that this tmp does */
         /* not get sent to mif.                             */
   
         AT_REFERENCED(lcv_idx)     = Not_Referenced;
      }
      else {
         OPND_FLD(save_atd_tmp_opnd) = (fld_type) ATD_FLD(lcv_idx);
         OPND_IDX(save_atd_tmp_opnd) = ATD_TMP_IDX(lcv_idx);
      }

      /* save the guts of the lcv_idx attr      */
      /* store them in a constant entry pointed */
      /* to by ATD_TMP_IDX(lcv_idx).            */

      GET_LCV_CONST(lcv_idx, loc_value[0],  /* target const*/
                    num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);

      ATD_FLD(lcv_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(lcv_idx) = ntr_const_tbl(ATD_TYPE_IDX(lcv_idx),
                                           FALSE,
                                           loc_value);


      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                 &loc_element);

      type_idx = ATD_TYPE_IDX(lcv_idx);

      if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

         if (folder_driver((char *)result_value,
                           exp_desc_l.linear_type,
                           NULL,
                           NULL_IDX,
                           start_value,
                          &type_idx,
                           line,
                           col,
                           1,
                           Cvrt_Opr)) {
            /* intentionally blank */
         }
      }
      else {
         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
            start_value[i] = result_value[i];
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                 &loc_element) && ok;

      type_idx = ATD_TYPE_IDX(lcv_idx);

      if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

         if (folder_driver((char *)result_value,
                           exp_desc_l.linear_type,
                           NULL,
                           NULL_IDX,
                           end_value,
                          &type_idx,
                           line,
                           col,
                           1,
                           Cvrt_Opr)) {
            /* intentionally blank */
         }
      }
      else {
         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
            end_value[i] = result_value[i];
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                 &loc_element) && ok;

      type_idx = ATD_TYPE_IDX(lcv_idx);

      if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

         if (folder_driver((char *)result_value,
                           exp_desc_l.linear_type,
                           NULL,
                           NULL_IDX,
                           stride_value,
                          &type_idx,
                           line,
                           col,
                           1,
                           Cvrt_Opr)) {
            /* intentionally blank */
         }
      }
      else {
         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
            stride_value[i] = result_value[i];
         }
      }

      type_idx = CG_LOGICAL_DEFAULT_TYPE;

      if (folder_driver((char *)stride_value,
                        ATD_TYPE_IDX(lcv_idx),
                        (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                        CG_INTEGER_DEFAULT_TYPE,
                        loc_value,
                       &type_idx,
                        line,
                        col,
                        2,
                        Eq_Opr)) {

         if (THIS_IS_TRUE(loc_value, type_idx)) {
            find_opnd_line_and_column(&opnd, &line, &col);
            PRINTMSG(line, 1084, Error, col);
            ok = FALSE;
            goto DONE;
         }
      }

      type_idx = CG_LOGICAL_DEFAULT_TYPE;

      if (folder_driver((char *)stride_value,
                        ATD_TYPE_IDX(lcv_idx),
                        (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                        CG_INTEGER_DEFAULT_TYPE,
                        loc_value,
                       &type_idx,
                        line,
                        col,
                        2,
                        Lt_Opr)) {

         if (THIS_IS_TRUE(loc_value, type_idx)) {
            compare_opr = Ge_Opr;
         }
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
         lcv_value[i] = start_value[i];
      }

      while (TRUE) {

         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         if (folder_driver((char *)lcv_value,
                           ATD_TYPE_IDX(lcv_idx),
                           (char *)end_value,
                           ATD_TYPE_IDX(lcv_idx),
                           loc_value,
                          &type_idx,
                           line,
                           col,
                           2,
                           compare_opr)) {

            if ( ! THIS_IS_TRUE(loc_value, type_idx)) {
               break;
            }
         }
         else {
            break;
         }
# ifdef KEY
         SET_LCV_CONST(lcv_idx, lcv_value[0], 
                       num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))], num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
# else
         SET_LCV_CONST(lcv_idx, lcv_value[0], 
                       num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
# endif

         list_idx = IR_IDX_L(ir_idx);

         while (list_idx) {

            COPY_OPND(opnd, IL_OPND(list_idx));

            if (IL_FLD(list_idx) == IR_Tbl_Idx     &&
                IR_ARRAY_SYNTAX(IL_IDX(list_idx))) {

               /* not in array syntax, but above array syntax */

               loc_element = 1;

               if (count) {

                  ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                             &loc_element) && ok;

                  sub_elements = 1;

                  if (exp_desc_l.rank == 0) {
                     extent++;
                  }
                  else {

                     for (i = 0; i < exp_desc_l.rank; i++) {
                        if (exp_desc_l.shape[i].fld == CN_Tbl_Idx) {
                           sub_elements *= CN_INT_TO_C(exp_desc_l.shape[i].idx);
                        }
                        else {
                           break;
                        }
                     }
                     extent += sub_elements;
                  }

                  if (exp_desc_l.type == Character) {
                     if (char_result_len > longest_char_len) {

                        if (longest_char_len != 0) {
                           unequal_char_lens = TRUE;
                        }
                        longest_char_len = char_result_len;
                     }
                  }
                  else if (exp_desc_l.constant) {
                     increment_count(&exp_desc_l);
                  }
               }
               else {
                  /* not count */
                  /* set up loop around array syntax */

                  loc_element = 1;
                  while (loc_element >= 0) {
                     loc_char_result_offset = char_result_offset;
                     ok = interpret_constructor(&opnd, &exp_desc_l,
                                     count, &loc_element) && ok;
                     char_result_offset = loc_char_result_offset;

                     if (exp_desc_l.constant) {
                        write_constant(exp_desc_l.type_idx);
                     }
                  }
               }
            }
            else {

               /* not in array syntax, not above array syntax */

               loc_element = 0;

               loc_char_result_offset = char_result_offset;
               COPY_OPND(opnd, IL_OPND(list_idx));
               ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                          &loc_element) && ok;
               char_result_offset = loc_char_result_offset;

               if (count) {
                  sub_elements = 1;

                  if (exp_desc_l.rank == 0) {
                     extent++;
                  }
                  else {

                     for (i = 0; i < exp_desc_l.rank; i++) {
                        if (exp_desc_l.shape[i].fld == CN_Tbl_Idx) {
                           sub_elements *= CN_INT_TO_C(exp_desc_l.shape[i].idx);
                        }
                        else {
                           break;
                        }
                     }
                     extent += sub_elements;
                  }

                  if (exp_desc_l.type == Character) {
                     if (char_result_len > longest_char_len) {

                        if (longest_char_len != 0) {
                           unequal_char_lens = TRUE;
                        }
                        longest_char_len = char_result_len;
                     }
                  }
                  else if (exp_desc_l.constant) {
                     increment_count(&exp_desc_l);
                  }

               }
               else {
                  if (exp_desc_l.constant) {
                     write_constant(exp_desc_l.type_idx);
                  }
               }
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         type_idx = ATD_TYPE_IDX(lcv_idx);

         if (folder_driver((char *)lcv_value,
                           ATD_TYPE_IDX(lcv_idx),
                           (char *)stride_value,
                           ATD_TYPE_IDX(lcv_idx),
                           loc_value,
                          &type_idx,
                           line,
                           col,
                           2,
                           Plus_Opr)) {

            for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
               lcv_value[i] = loc_value[i];
            }
         }
         else {
            break;
         }
      }

      /* restore the guts of the lcv temp attr */
# ifdef KEY
      SET_LCV_CONST(lcv_idx, CN_CONST(ATD_TMP_IDX(lcv_idx)),
                    num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))],
                    num_host_wds[TYP_LINEAR(CN_TYPE_IDX(ATD_TMP_IDX(lcv_idx)))]);
# else
      SET_LCV_CONST(lcv_idx, CN_CONST(ATD_TMP_IDX(lcv_idx)),
                    num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
# endif
      

      if (count) {
         exp_desc->rank         = 1;
         exp_desc->shape[0].fld = CN_Tbl_Idx;
         exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, extent);
         char_result_len	= longest_char_len;

         ATD_FLD(lcv_idx) = OPND_FLD(save_atd_tmp_opnd);
         ATD_TMP_IDX(lcv_idx) = OPND_IDX(save_atd_tmp_opnd);
      }
   }
   else {
      /* in array syntax */

      if (count) {

         OPND_FLD(save_atd_tmp_opnd) = (fld_type) ATD_FLD(lcv_idx);
         OPND_IDX(save_atd_tmp_opnd) = ATD_TMP_IDX(lcv_idx);

         /* save the guts of the lcv_idx attr      */
         /* store them in a constant entry pointed */
         /* to by ATD_TMP_IDX(lcv_idx).            */

         GET_LCV_CONST(lcv_idx, loc_value[0],  /* target const*/
                       num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);

         ATD_FLD(lcv_idx) = CN_Tbl_Idx;
         ATD_TMP_IDX(lcv_idx) = ntr_const_tbl(ATD_TYPE_IDX(lcv_idx),
                                              FALSE,
                                              loc_value);

         list_idx = IL_NEXT_LIST_IDX(list_idx);
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                    &loc_element);

         type_idx = ATD_TYPE_IDX(lcv_idx);

         if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

            if (folder_driver((char *)result_value,
                              exp_desc_l.linear_type,
                              NULL,
                              NULL_IDX,
                              start_value,
                             &type_idx,
                              line,
                              col,
                              1,
                              Cvrt_Opr)) {
               /* intentionally blank */
            }
         }
         else {
            for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
               start_value[i] = result_value[i];
            }
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                    &loc_element) && ok;
   
         type_idx = ATD_TYPE_IDX(lcv_idx);

         if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

            if (folder_driver((char *)result_value,
                              exp_desc_l.linear_type,
                              NULL,
                              NULL_IDX,
                              end_value,
                             &type_idx,
                              line,
                              col,
                              1,
                              Cvrt_Opr)) {
               /* intentionally blank */
            }
         }
         else {
            for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
               end_value[i] = result_value[i];
            }
         }
   
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                    &loc_element) && ok;
   
         type_idx = ATD_TYPE_IDX(lcv_idx);

         if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

            if (folder_driver((char *)result_value,
                              exp_desc_l.linear_type,
                              NULL,
                              NULL_IDX,
                              stride_value,
                             &type_idx,
                              line,
                              col,
                              1,
                              Cvrt_Opr)) {
               /* intentionally blank */
            }
         }
         else {
            for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
               stride_value[i] = result_value[i];
            }
         }

         type_idx = CG_LOGICAL_DEFAULT_TYPE;
   
         if (folder_driver((char *)stride_value,
                           ATD_TYPE_IDX(lcv_idx),
                           (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                           CG_INTEGER_DEFAULT_TYPE,
                           loc_value,
                          &type_idx,
                           line,
                           col,
                           2,
                           Eq_Opr)) {

            if (THIS_IS_TRUE(loc_value, type_idx)) {
               find_opnd_line_and_column(&opnd, &line, &col);
               PRINTMSG(line, 1084, Error, col);
               ok = FALSE;
               goto DONE;
            }
         }

         loc_element = 1;

         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         if (folder_driver((char *)stride_value,
                           ATD_TYPE_IDX(lcv_idx),
                           (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                           CG_INTEGER_DEFAULT_TYPE,
                           loc_value,
                          &type_idx,
                           line,
                           col,
                           2,
                           Lt_Opr)) {

            if (THIS_IS_TRUE(loc_value, type_idx)) {
               compare_opr = Ge_Opr;
            }
         }

         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
            lcv_value[i] = start_value[i];
         }

         while (TRUE) {

            type_idx = CG_LOGICAL_DEFAULT_TYPE;

            if (folder_driver((char *)lcv_value,
                              ATD_TYPE_IDX(lcv_idx),
                              (char *)end_value,
                              ATD_TYPE_IDX(lcv_idx),
                              loc_value,
                             &type_idx,
                              line,
                              col,
                              2,
                              compare_opr)) {

               if (! THIS_IS_TRUE(loc_value, type_idx)) {
                  break;
               }
            }
            else {
               break;
            }

# ifdef KEY
            SET_LCV_CONST(lcv_idx, lcv_value[0], 
                          num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))], num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
# else
            SET_LCV_CONST(lcv_idx, lcv_value[0], 
                          num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
# endif

            list_idx = IR_IDX_L(ir_idx);

            while (list_idx) {

               COPY_OPND(opnd, IL_OPND(list_idx));

               ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                          &loc_element) && ok;

               sub_elements = 1;

               if (exp_desc_l.rank == 0) {
                  extent++;
               }
               else {

                  for (i = 0; i < exp_desc_l.rank; i++) {
                     if (exp_desc_l.shape[i].fld == CN_Tbl_Idx) {
                        sub_elements *= CN_INT_TO_C(exp_desc_l.shape[i].idx);
                     }
                     else {
                        break;
                     }
                  }
                  extent += sub_elements;
               }

               if (exp_desc_l.type == Character) {
                  if (char_result_len > longest_char_len) {

                     if (longest_char_len != 0) {
                        unequal_char_lens = TRUE;
                     }
                     longest_char_len = char_result_len;
                  }
               }

               *element += sub_elements;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }

            type_idx = ATD_TYPE_IDX(lcv_idx);

            if (folder_driver((char *)lcv_value,
                              ATD_TYPE_IDX(lcv_idx),
                              (char *)stride_value,
                              ATD_TYPE_IDX(lcv_idx),
                              loc_value,
                             &type_idx,
                              line,
                              col,
                              2,
                              Plus_Opr)) {

               for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
                  lcv_value[i] = loc_value[i];
               }
            }
            else {
               break;
            }
         }

         exp_desc->rank         = 1;
         exp_desc->shape[0].fld = CN_Tbl_Idx;
         exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, extent);
         char_result_len	= longest_char_len;

         /* restore the guts of the lcv temp attr */
#ifdef KEY   
         SET_LCV_CONST(lcv_idx, CN_CONST(ATD_TMP_IDX(lcv_idx)),
                       num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))],
                       num_host_wds[TYP_LINEAR(CN_TYPE_IDX(ATD_TMP_IDX(lcv_idx)))]);
#else
         SET_LCV_CONST(lcv_idx, CN_CONST(ATD_TMP_IDX(lcv_idx)),
                       num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
#endif

         ATD_FLD(lcv_idx) = OPND_FLD(save_atd_tmp_opnd);
         ATD_TMP_IDX(lcv_idx) = OPND_IDX(save_atd_tmp_opnd);
      }
      else {

         /* not count */
         /* in array syntax */
         /* get next value  */

         if (*element == 1) {

/******************************************************************************\
   The implied do tree is modified to maintain the position and state in the
   ir. Each subsequent time down in this routine, the position, element number,
   end value and stride value are retrieved from this modified tree. When the
   implied do is done, the tree is returned to its original form.   

		ORIGINAL TREE

				Implied_Do_Opr
			       /             \
           implied do items <-+               +-> lcv attr
                              .               |
                              .               +-> start expression
                              .               |
                              .               +-> end expression
                              .               |
                                              +-> stride expression

		BECOMES THIS ....

                                Implied_Do_Opr
                               /             \
           implied do items <-+ <--           +-> lcv attr
                              .    \          |
                              .     \         +-> start expression
                              .      \        |
                              .       \       +-> +-> end value
                              .        \      |   |
                                        \     |   +-> original end expr
                                         \    |
                                          \   +-> +-> stride value
                                           \  |   |
                                            \ |   +-> original stride expr
                                             \|
                                              +(position_idx) 

                           position_idx is an IL_Tbl_Idx that holds the
                           current "element" value in place of an opnd
                           (in the second word). It's IL_NEXT_LIST_IDX
                           field points to the first implied do item's
                           list idx. As it proceeds through all the elements
                           in the first implied do item, the element value
                           held inside the position_idx is incremented.
                           When the first implied do item is done, the
                           IL_NEXT_LIST_IDX(position_idx) is advanced to 
                           point to the next implied do item list_idx and
                           the element value is reset to 1.

               When the entire implied do item list is finished, the loop
               control is advanced by the stride value and tested against
               end value. The process above is repeated until the loop is
               finished. Then the tree is reset to it's original state.


\******************************************************************************/


            /* clear the referenced field so that this tmp does */
            /* not get sent to mif.                             */

            AT_REFERENCED(lcv_idx)     = Not_Referenced;

            /* save the guts of the lcv_idx attr      */
            /* store them in a constant entry pointed */
            /* to by ATD_TMP_IDX(lcv_idx).            */

            GET_LCV_CONST(lcv_idx, loc_value[0],  /* target const*/
                          num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);

            ATD_FLD(lcv_idx) = CN_Tbl_Idx;
            ATD_TMP_IDX(lcv_idx) = ntr_const_tbl(ATD_TYPE_IDX(lcv_idx),
                                                 FALSE,
                                                 loc_value);

            list_idx = IL_NEXT_LIST_IDX(list_idx);
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                       &loc_element);
      
            type_idx = ATD_TYPE_IDX(lcv_idx);

            if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

               if (folder_driver((char *)result_value,
                                 exp_desc_l.linear_type,
                                 NULL,
                                 NULL_IDX,
                                 start_value,
                                &type_idx,
                                 line,
                                 col,
                                 1,
                                 Cvrt_Opr)) {
                  /* intentionally blank */
               }
            }
            else {
               for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
                  start_value[i] = result_value[i];
               }
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                       &loc_element) && ok;
      
            type_idx = ATD_TYPE_IDX(lcv_idx);

            if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

               if (folder_driver((char *)result_value,
                                 exp_desc_l.linear_type,
                                 NULL,
                                 NULL_IDX,
                                 end_value,
                                &type_idx,
                                 line,
                                 col,
                                 1,
                                 Cvrt_Opr)) {
                  /* intentionally blank */
               }
            }
            else {
               for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
                  end_value[i] = result_value[i];
               }
            }
      
            list_idx = IL_NEXT_LIST_IDX(list_idx);
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                       &loc_element) && ok;
      
            type_idx = ATD_TYPE_IDX(lcv_idx);

            if (TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

               if (folder_driver((char *)result_value,
                                 exp_desc_l.linear_type,
                                 NULL,
                                 NULL_IDX,
                                 stride_value,
                                &type_idx,
                                 line,
                                 col,
                                 1,
                                 Cvrt_Opr)) {
                  /* intentionally blank */
               }
            }
            else {
               for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i ++) {
                  stride_value[i] = result_value[i];
               }
            }

            type_idx = CG_LOGICAL_DEFAULT_TYPE;

            if (folder_driver((char *)stride_value,
                              ATD_TYPE_IDX(lcv_idx),
                              (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                              CG_INTEGER_DEFAULT_TYPE,
                              loc_value,
                             &type_idx,
                              line,
                              col,
                              2,
                              Eq_Opr)) {

               if (THIS_IS_TRUE(loc_value, type_idx)) {
                  find_opnd_line_and_column(&opnd, &line, &col);
                  PRINTMSG(line, 1084, Error, col);
                  ok = FALSE;
                  goto DONE;
               }
            }


            /* if ((((end_int - start_int) / stride_int) + 1L) < 0) */
            /*  then zero trip count */

            type_idx = ATD_TYPE_IDX(lcv_idx);

            if (folder_driver((char *)end_value,
                              ATD_TYPE_IDX(lcv_idx),
                              (char *)start_value,
                              ATD_TYPE_IDX(lcv_idx),
                              loc_value,
                             &type_idx,
                              line,
                              col,
                              2,
                              Minus_Opr)) {

               if (folder_driver((char *)loc_value,
                                 ATD_TYPE_IDX(lcv_idx),
                                 (char *)stride_value,
                                 ATD_TYPE_IDX(lcv_idx),
                                 loc_value,
                                &type_idx,
                                 line,
                                 col,
                                 2,
                                 Div_Opr)) {

                  if (folder_driver((char *)loc_value,
                                    ATD_TYPE_IDX(lcv_idx),
                                    (char *)&CN_CONST(CN_INTEGER_ONE_IDX),
                                    CG_INTEGER_DEFAULT_TYPE,
                                    loc_value,
                                   &type_idx,
                                    line,
                                    col,
                                    2,
                                    Plus_Opr)) {

                     type_idx = CG_LOGICAL_DEFAULT_TYPE;

                     if (folder_driver((char *)loc_value,
                                       ATD_TYPE_IDX(lcv_idx),
                                       (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                                       CG_INTEGER_DEFAULT_TYPE,
                                       loc_value,
                                      &type_idx,
                                       line,
                                       col,
                                       2,
                                       Lt_Opr)) {

                        if (THIS_IS_TRUE(loc_value, type_idx)) {
                           *element = -1;
                           goto DONE;
                        }
                     }
                  }
               }
            }

            list_idx = IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx));
#ifdef KEY
            SET_LCV_CONST(lcv_idx, start_value[0],
                          num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))], num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
#else
            SET_LCV_CONST(lcv_idx, start_value[0],
                          num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
#endif
      
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            /* save end value */
            NTR_IR_LIST_TBL(list2_idx);
            NTR_IR_LIST_TBL(list3_idx);
            IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
            COPY_OPND(IL_OPND(list3_idx), IL_OPND(list_idx));
            IL_FLD(list2_idx) = CN_Tbl_Idx;
            IL_IDX(list2_idx) = ntr_const_tbl(ATD_TYPE_IDX(lcv_idx),
                                              FALSE,
                                              end_value);
            IL_LINE_NUM(list2_idx) = line;
            IL_COL_NUM(list2_idx)  = col;

# ifdef _DEBUG
            if (IL_FLD(list_idx) == IL_Tbl_Idx) {
               /* DAG */
               PRINTMSG(line, 626, Internal, col,
                        "no DAG", "interpret_implied_do");
            }
# endif

            IL_FLD(list_idx) = IL_Tbl_Idx;
            IL_LIST_CNT(list_idx) = 2;
            IL_IDX(list_idx) = list2_idx;

            list_idx = IL_NEXT_LIST_IDX(list_idx);

            /* save stride value */

            NTR_IR_LIST_TBL(list2_idx);
            NTR_IR_LIST_TBL(list3_idx);
            IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
            COPY_OPND(IL_OPND(list3_idx), IL_OPND(list_idx));
            IL_FLD(list2_idx) = CN_Tbl_Idx;
            IL_IDX(list2_idx) = ntr_const_tbl(ATD_TYPE_IDX(lcv_idx),
                                              FALSE,
                                              stride_value);
            IL_LINE_NUM(list2_idx) = line;
            IL_COL_NUM(list2_idx)  = col;

            IL_FLD(list_idx) = IL_Tbl_Idx;
            IL_LIST_CNT(list_idx) = 2;
            IL_IDX(list_idx) = list2_idx;


            /* create position list node */

            NTR_IR_LIST_TBL(position_idx);
            IL_NEXT_LIST_IDX(list_idx) = position_idx;
            IL_NEXT_LIST_IDX(position_idx) = IR_IDX_L(ir_idx);
            IL_ELEMENT(position_idx)       = 1;


         }
         else {

            list_idx = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(list_idx));

            for (i = 0; 
                 i < num_host_wds[TYP_LINEAR(
                      CN_TYPE_IDX(IL_IDX(IL_IDX(list_idx))))];
                 i++) {

               end_value[i] = 
                   CP_CONSTANT(CN_POOL_IDX(IL_IDX(IL_IDX(list_idx)))+i);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);

            for (i = 0; 
                 i < num_host_wds[TYP_LINEAR(
                      CN_TYPE_IDX(IL_IDX(IL_IDX(list_idx))))];
                 i++) {

               stride_value[i] = 
                   CP_CONSTANT(CN_POOL_IDX(IL_IDX(IL_IDX(list_idx)))+i);
            }


            position_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         loc_char_result_offset = char_result_offset;
         COPY_OPND(opnd, IL_OPND(IL_NEXT_LIST_IDX(position_idx)));
         loc_element = IL_ELEMENT(position_idx);
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                                      &loc_element) && ok;
         char_result_offset = loc_char_result_offset;

         if (loc_element < 0) {

            if (IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(position_idx))) {
               IL_NEXT_LIST_IDX(position_idx) =
                     IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(position_idx));
               IL_ELEMENT(position_idx) = 1;
               (*element)++;
            }
            else {
               lin_type		= TYP_LINEAR(ATD_TYPE_IDX(lcv_idx));

               GET_LCV_CONST(lcv_idx, start_value[0], num_host_wds[lin_type]); 

               unused = ATD_TYPE_IDX(lcv_idx);
               ok = folder_driver((char *)start_value,
                                  ATD_TYPE_IDX(lcv_idx),
                                  (char *)stride_value,
                                  ATD_TYPE_IDX(lcv_idx),
                                  lcv_value,
                                  &unused,
                                  line,
                                  col,
                                  2,
                                  Plus_Opr) && ok;

               unused = CG_LOGICAL_DEFAULT_TYPE;
               if (folder_driver((char *)stride_value,
                                 ATD_TYPE_IDX(lcv_idx),
                                 (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                                 CG_INTEGER_DEFAULT_TYPE,
                                 loc_value,
                                &unused,
                                 line,
                                 col,
                                 2,
                                 Lt_Opr)) {

                  if (THIS_IS_TRUE(loc_value, unused)) {
                     compare_opr = Ge_Opr;
                  }
               }

               unused = CG_LOGICAL_DEFAULT_TYPE;
               ok = folder_driver((char *)lcv_value,
                                  ATD_TYPE_IDX(lcv_idx),
                                  (char *)end_value,
                                  ATD_TYPE_IDX(lcv_idx),
                                  loc_value,
                                  &unused,
                                  line,
                                  col,
                                  2,
                                  compare_opr) && ok;

               if (THIS_IS_TRUE(loc_value, unused)) {
#ifdef KEY
                  SET_LCV_CONST(lcv_idx, lcv_value[0],
                               num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))], num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
#else
                  SET_LCV_CONST(lcv_idx, lcv_value[0],
                               num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
#endif
                  IL_NEXT_LIST_IDX(position_idx) = IR_IDX_L(ir_idx);
                  IL_ELEMENT(position_idx)       = 1;
                  (*element)++;
               }
               else {
                  /* all done, return the ir to original form */
                  (*element) = -1;
                  list_idx = IR_IDX_R(ir_idx);
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
      
                  /* reset end expression */
                  list2_idx = IL_IDX(list_idx);
                  COPY_OPND(IL_OPND(list_idx), 
                            IL_OPND(IL_NEXT_LIST_IDX(list2_idx)));

                  FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list2_idx));
                  FREE_IR_LIST_NODE(list2_idx);

                  list_idx = IL_NEXT_LIST_IDX(list_idx);

                  /* reset stride expression */
                  list2_idx = IL_IDX(list_idx);
                  COPY_OPND(IL_OPND(list_idx),
                            IL_OPND(IL_NEXT_LIST_IDX(list2_idx)));

                  FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list2_idx));
                  FREE_IR_LIST_NODE(list2_idx);

                  /* free up the position list node */
                  FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list_idx));
                  IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;

                  /* restore the guts of the lcv temp attr */
#ifdef KEY
                  SET_LCV_CONST(lcv_idx, CN_CONST(ATD_TMP_IDX(lcv_idx)),
                             num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))],num_host_wds[TYP_LINEAR(CN_TYPE_IDX(ATD_TMP_IDX(lcv_idx)))]);
#else
                  SET_LCV_CONST(lcv_idx, CN_CONST(ATD_TMP_IDX(lcv_idx)),
                             num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(lcv_idx))]);
#endif
               }
            }
         }
         else {
            IL_ELEMENT(position_idx)++;
            (*element)++;
         }
      }
   }

DONE:

   TRACE (Func_Exit, "interpret_implied_do", NULL);

   return(ok);

}  /* interpret_implied_do */

#ifdef KEY /* Bug 572 */
boolean constant_ptr_ok = FALSE;
#endif /* KEY Bug 572 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine handles all the reference ir in constant constructors.   *|
|*      All subscript, substring and struct oprs end up here.                 *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - incoming reference tree.                                   *|
|*      count    - TRUE if this is the count phase.                           *|
|*	element  - array syntax flag.                                         *|
|*									      *|
|* Output parameters:							      *|
|*	exp_desc - some fields are set, shape and type ...                    *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

static boolean interpret_ref(opnd_type		*top_opnd,
                             expr_arg_type	*exp_desc,
                             boolean		 count,
			     long64		*element)

{

   int			base_attr_idx;
#ifdef KEY /* Bug 10177 */
   int			base_cn_idx = 0;
#else /* KEY Bug 10177 */
   int			base_cn_idx;
#endif /* KEY Bug 10177 */
   int			bd_idx;
   long64		bit_offset = 0;
   char			*char_ptr;
   char			*char_ptr2;
#ifdef KEY /* Bug 10177 */
   long64		char_len = 0;
#else /* KEY Bug 10177 */
   long64		char_len;
#endif /* KEY Bug 10177 */
   long64		cn_bit_offset;
   int			col;
   long64		end_array[8];
   long64		end_value;
   long64		extent;
   long64		i;
   long64		index;
   int			index_list;
   long64		index_array[8];
   int			ir_idx;
   boolean		is_vec_subscript[8];
   int			left_attr;
   int			line;
   int			list_idx;
   int			listr_idx;
   int			list2_idx;
   long64		loc_element;
   expr_arg_type        loc_exp_desc;
   long_type		loc_value[MAX_WORDS_FOR_NUMERIC];
   boolean		neg_stride[8];
#ifdef KEY /* Bug 10177 */
   long64		num_bits = 0;
#else /* KEY Bug 10177 */
   long64		num_bits;
#endif /* KEY Bug 10177 */
   long64		num_words;
   boolean		ok = TRUE;
   opnd_type		opnd;
   opnd_type		opnd2;
   int			rank;
   boolean		rank_array[8];
#ifdef KEY /* Bug 10177 */
   int			rank_idx = 0;
#else /* KEY Bug 10177 */
   int			rank_idx;
#endif /* KEY Bug 10177 */
   boolean		single_value_const = FALSE;
   long64		sm_in_bits;
   long64		start_array[8];
   long64   		start_value;
   long64		stride_array[8];
   long64   		stride_value;
   long64		substring_offset = 0;
   int			type_idx;
   long64		word_offset = 0;
   boolean		zero_size_array;


   TRACE (Func_Entry, "interpret_ref", NULL);

   COPY_OPND(opnd, (*top_opnd));

   ir_idx = OPND_IDX(opnd);
   rank   = IR_RANK(ir_idx);

   if (! count) {
      left_attr = find_left_attr(&opnd);

      if (ATD_FLD(left_attr) == IR_Tbl_Idx) {
         single_value_const = TRUE;
         base_cn_idx = IR_IDX_R(ATD_TMP_IDX(left_attr));
      }
      else {
         base_cn_idx = ATD_TMP_IDX(left_attr);
      }
   }

   exp_desc->type_idx	= IR_TYPE_IDX(ir_idx);
   exp_desc->type	= TYP_TYPE(exp_desc->type_idx);
   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (exp_desc->type == Character &&
       rank           == 0         &&
       compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                            MAX_CHARS_IN_TYPELESS,
                            Le_Opr)) {
      exp_desc->linear_type = Short_Char_Const;
   }

   exp_desc->rank = rank;
   exp_desc->constant = TRUE;
   exp_desc->foldable = TRUE;

   switch (exp_desc->type) {
      case Typeless :
         num_bits = TYP_BIT_LEN(exp_desc->type_idx);
         break;

      case Integer :
      case Logical :
      case Real :
      case Complex :
         num_bits = storage_bit_size_tbl[exp_desc->linear_type];
         break;

      case Character:

         list_idx = IR_IDX_R(ir_idx);
         COPY_OPND(opnd2, IL_OPND(list_idx));
         loc_element = 0;
         ok = interpret_constructor(&opnd2, &loc_exp_desc, FALSE,
                                    &loc_element);
         start_value = F_INT_TO_C(result_value, loc_exp_desc.linear_type);

         substring_offset = start_value - 1L;

         list_idx = IL_NEXT_LIST_IDX(list_idx);

         COPY_OPND(opnd2, IL_OPND(list_idx));

         ok = interpret_constructor(&opnd2, &loc_exp_desc, FALSE,
                                    &loc_element);
         end_value = F_INT_TO_C(result_value, loc_exp_desc.linear_type);

         char_len = end_value - start_value + 1L;

         if (char_len < 0) {
            char_len = 0;
         }
         char_result_len = char_len;
         break;

      case Structure :
         num_bits = CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(
                               TYP_IDX(exp_desc->type_idx)));
         break;
   }

#ifdef KEY /* Bug 572 */
   /* According to the F95 standard, an entity can't have both the parameter
    * and target attributes; and an entity can't have both the parameter and
    * pointer attributes. But an entity which has the parameter attribute
    * may be a structure whose component has the pointer attribute. A reference
    * like "parameter_x%ptr_component_y" will bring us to this spot, e.g.:
    *
    *   type x
    *     integer, pointer :: ptr_component_y
    *   end type
    *   type(x), parameter :: parameter_x = x(null())
    *   print *, associated(parameter_x%ptr_component_y)
    *
    * I believe that a constant pointer is always null. An entity cannot have
    * both "parameter" and "target" attributes, so there's no constant target
    * which a constant pointer could point to.
    *
    * The strategy for processing the semantics of pointer references
    * throughout the compiler is that expr_semantics() doesn't know whether to
    * dereference a pointer or not, so it assumes that
    * it should always dereference it. If the caller of expr_semantics()
    * knows otherwise, it may call expr_semantics() and then undo the
    * dereferencing. See, for example, assignment_stmt_semantics(): if it's a
    * "=>" assignment, the code first calls expr_semantics() and then calls
    * lower_ptr_asg() to remove the unwanted Dv_Deref_Opr which expr_semantics()
    * added.
    *
    * That strategy doesn't work for a constant pointer. If the context really
    * implies dereference, it's an error (because a constant pointer is always
    * null.) If the context doesn't imply dereference, we need to know that
    * here so that we can generate a constant representing a null dope vector.
    *
    * Complicating things is the fact that fold_aggragate_expression() is
    * called from interpret_constructor() for two very purposes--to create
    * a structure constructor, or to evaluate a component of a constant
    * structure--and isn't told which.
    *
    * The compiler was apparently designed under the (pre-F95) assumption that
    * you could never have a constant pointer (that restriction appeared at the
    * end of section 4.4.4 in F90.) To fix this as unobtrusively as possible,
    * we normally issue an error message, to handle the general case in which
    * a pointer in an expression is dereferenced. We rely on the caller of
    * expr_semantics() to set "constant_ptr_ok" in the situations where
    * dereference is not implied and we should generate a null pointer value.
    */
   int component_attr = NULL_IDX;
   if (Struct_Opr == IR_OPR(ir_idx) && AT_Tbl_Idx == IR_FLD_R(ir_idx) &&
     ATD_POINTER(component_attr = IR_IDX_R(ir_idx))) {
     exp_desc->pointer = TRUE;
     if (constant_ptr_ok) {
       num_bits = stor_bit_size_of(component_attr, TRUE, FALSE).constant[0];
       if (Character == exp_desc->type) {
	 if (CN_Tbl_Idx == TYP_FLD(exp_desc->type_idx)) {
	   char_result_len = CN_INT_TO_C(TYP_IDX(exp_desc->type_idx));
	 }
	 else {
	   /* If it's ever not CN_Tbl_Idx, we need to add code for that here */
	   PRINTMSG(IR_LINE_NUM(ir_idx), 1024, Internal, IR_COL_NUM(ir_idx));
	 }
       }
     }
     else if (!count) { /* Don't generate the error twice */
       PRINTMSG(IR_LINE_NUM(ir_idx), 1677, Error, IR_COL_NUM(ir_idx));
       ok = FALSE;
     }
   }
#endif /* KEY Bug 572 */

   if (count) {

      if (rank == 0) {
         /* intentionally blank */
      }
      else {
         while (OPND_FLD(opnd) == IR_Tbl_Idx) {
            if (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr ||
                IR_OPR(OPND_IDX(opnd)) == Section_Subscript_Opr) {
               break;
            }
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         ir_idx = OPND_IDX(opnd);
         list_idx = IR_IDX_R(ir_idx);
         loc_element = 0;
         rank = 0;

         while (list_idx &&
                ! IL_PE_SUBSCRIPT(list_idx)) {

            if (IL_FLD(list_idx) == IR_Tbl_Idx           &&
                IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

               list2_idx = IR_IDX_L(IL_IDX(list_idx));
               COPY_OPND(opnd, IL_OPND(list2_idx));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &loc_element);
               start_value = F_INT_TO_C(result_value, loc_exp_desc.linear_type);

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               COPY_OPND(opnd, IL_OPND(list2_idx));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &loc_element);
               end_value = F_INT_TO_C(result_value, loc_exp_desc.linear_type);

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               COPY_OPND(opnd, IL_OPND(list2_idx));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &loc_element);
               stride_value = F_INT_TO_C(result_value, 
                                         loc_exp_desc.linear_type);

               exp_desc->shape[rank].fld = CN_Tbl_Idx;
               extent = ((end_value - start_value) / stride_value) + 1L;

               if (extent < 0L) {
                  extent = 0L;
               }

               exp_desc->shape[rank].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                       extent);
               rank++;
            }
            else {
           
               COPY_OPND(opnd, IL_OPND(list_idx));
               loc_element = 1;
               ok = interpret_constructor(&opnd, &loc_exp_desc, TRUE, 
                                          &loc_element);
               loc_element = 0;

               if (loc_exp_desc.rank > 0) {
                  COPY_OPND(exp_desc->shape[rank], loc_exp_desc.shape[0]);
                  rank++;
               }
            }
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
   }
   else if (*element > 0  &&
            rank     > 0) {


      /* I assume that no references of type structure are in here */
      /* this is array syntax and vector subscript stuff.          */
# ifdef _DEBUG
      if (exp_desc->type == Structure) {
         PRINTMSG(IR_LINE_NUM(ir_idx), 984, Internal, IR_COL_NUM(ir_idx));
      }
# endif

      zero_size_array = FALSE;

      if (*element == 1) {

/*****************************************************************************\

   This is an array valued reference, either a section, a vector subscript
   section, or a whole array reference. First, we find the subscript opr
   that produces the rank (rank_idx). Then we modify the tree to keep track
   of where we are in this section. When the array is exhausted, the tree
   is restored to it's original state.

   bit_offset = cumulative bit offset of the scalar portions of the tree.

   cn_bit_offset = offsets




				ORIGINAL TREE    rank = n

                                    .
				   .
			          .
                            subscript opr (rank_idx)
                           /            \
                        base      dim 1  +-> element or section
                                         |
                                  dim 2  +-> element or section
                                         .
                                         .
                                  dim n  +-> element or section


				BECOMES ...

   cn_bit_offset is held in the second word (opnd) of an IL_Tbl_Idx. This
   list entry is inserted before the subscript list entries.

                            subscript opr (rank_idx)
                           /            \
                        base             +(holds cn_bit_offset)
                                         |
                                  dim 1  +-> element or section
                                         |
                                  dim 2  +-> element or section
                                         .
                                         .
                                  dim n  +-> element or section

   For each dimension, a list entry is created to hold the current element
   value. This is pointed to by index_list. The index_list list entry
   is inserted in the subscript tree and the tree is transformed diferently
   according to whether it is an element subscript, a section subscript or
   a vector subscript section subscript. The current subscript value is
   held inside the index_list list entry in the second word (opnd).

      TRIPLET OPR (section) :

         The start, end and stride expression are evaluated and the values
         are stored on the right side of the triplet opr.

                 .
                 .
        dim x    +----------->   triplet_opr
                 .              /
                 start expr  <-+
                               |
                 end expr    <-+
                               |
                 stride expr <-+

         BECOMES ...

                 .
                 .
        dim x    +---->+(index_list) holds current subscript value 
                 .     |
                       +-------> triplet_opr
                                /          \
                 start expr  <-+            +(holds start value)
                               |            |
                 end expr    <-+            +(holds end value)
                               |            |
                 stride expr <-+            +(holds stride value)


      VECTOR SUBSCRIPT (section) :
                 .
                 .
        dim x    +-> array expression
                 .


          BECOMES ...

                 .
                 .
        dim x    +->+(index_list) holds current subscript value
                 .  |
                    +(holds loc_element for the array expression)
                    |
                    +-> array expression


      ELEMENT :

                 .
                 .
        dim x    +-> scalar expression
                 .
					

         BECOMES ...

                 .
                 .
        dim x    +->+(index_list) holds subscript value
                 .  |
                    +-> scalar expression


   For each pass through, the return value is found by linearizing the
   base constant using cn_bit_offset and the offset determined by the values in
   the "index_list" locations of each dimension. The current subscript
   values are advanced according to the start, end and stride values for
   each triplet, and using the interpret_constructor routine to advance
   vector subscripts. When the entire array reference is exhausted the
   tree is restored to it's original state.

\*****************************************************************************/

         while (OPND_FLD(opnd) == IR_Tbl_Idx) {

            ir_idx = OPND_IDX(opnd);

            switch (IR_OPR(ir_idx)) {

               case Struct_Opr :
                  bit_offset += CN_INT_TO_C(ATD_CPNT_OFFSET_IDX(
                                                    IR_IDX_R(ir_idx)));
                  break;

               case Whole_Subscript_Opr   :
               case Section_Subscript_Opr :

                  rank_idx = ir_idx;
                  break;

               case Subscript_Opr :
                  base_attr_idx = find_base_attr(&opnd, &line, &col);
                  bd_idx = ATD_ARRAY_IDX(base_attr_idx);

                  if (TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Structure &&
                      ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(base_attr_idx)))) {

                     sm_in_bits = 8;
                  }
		  /* OSP_467, #2, we calculate the stride for these type differently
		   * Refer long64 sm_unit_in_bits(int) in sytb.c */
                  else if ( TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Integer ||
                            TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Logical ||
                            TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ptr ||
                            TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ch_Ptr ||
                            TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Real ||
                            TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Complex ) {
                     sm_in_bits = storage_bit_size_tbl[CG_INTEGER_DEFAULT_TYPE];
                  }
                  else {
                     sm_in_bits = sm_unit_in_bits(ATD_TYPE_IDX(base_attr_idx));
                  }

                  list_idx = IR_IDX_R(ir_idx);

                  for (i = 1; i <= BD_RANK(bd_idx); i++) {

                     loc_element = 0;
                     COPY_OPND(opnd2, IL_OPND(list_idx));
                     ok = interpret_constructor(&opnd2, &loc_exp_desc,
                                                FALSE, &loc_element);
 
                     bit_offset += (F_INT_TO_C(result_value,  /* KAYKAY */
                                               loc_exp_desc.linear_type)
                                          - CN_INT_TO_C(BD_LB_IDX(bd_idx,i)))
                                   * CN_INT_TO_C(BD_SM_IDX(bd_idx,i))
                                   * sm_in_bits;
   
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                  }
                  break;
            }

            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         if (exp_desc->type == Character) {

            /* cn_bit_offset is in bits */
            cn_bit_offset = (substring_offset * CHAR_BIT) + bit_offset;
         }
         else {
            /* cn_bit_offset is in bits */
            cn_bit_offset = bit_offset;
         }

         list_idx = IR_IDX_R(rank_idx);
         NTR_IR_LIST_TBL(list2_idx);
         IL_ELEMENT(list2_idx) = cn_bit_offset;
         IL_NEXT_LIST_IDX(list2_idx) = list_idx;
         IR_IDX_R(rank_idx) = list2_idx;

         base_attr_idx = find_base_attr(&(IR_OPND_L(rank_idx)), &line, &col);
         bd_idx = ATD_ARRAY_IDX(base_attr_idx);

         for (i = 1; i <= BD_RANK(bd_idx); i++) {

            NTR_IR_LIST_TBL(index_list);

            if (IL_FLD(list_idx) == IR_Tbl_Idx           &&
                IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

               loc_element = 0;

               NTR_IR_LIST_TBL(listr_idx);

# ifdef _DEBUG
               if (IR_FLD_R(IL_IDX(list_idx)) == IL_Tbl_Idx) {
                  PRINTMSG(line, 626, Internal, col,
                           "no DAG", "interpret_ref");
               }
# endif

               IR_FLD_R(IL_IDX(list_idx)) = IL_Tbl_Idx;
               IR_LIST_CNT_R(IL_IDX(list_idx)) = 3;
               IR_IDX_R(IL_IDX(list_idx)) = listr_idx;

               list2_idx = IR_IDX_L(IL_IDX(list_idx));
               COPY_OPND(opnd, IL_OPND(list2_idx));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &loc_element);

               IL_ELEMENT(index_list) = 
                           F_INT_TO_C(result_value, loc_exp_desc.linear_type);
               IL_ELEMENT(listr_idx) = 
                           F_INT_TO_C(result_value, loc_exp_desc.linear_type);
               start_value = 
                           F_INT_TO_C(result_value, loc_exp_desc.linear_type);
   
               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(listr_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(listr_idx)) = listr_idx;
               listr_idx = IL_NEXT_LIST_IDX(listr_idx);

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               COPY_OPND(opnd, IL_OPND(list2_idx));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &loc_element);
               IL_ELEMENT(listr_idx) = 
                           F_INT_TO_C(result_value, loc_exp_desc.linear_type);
               end_value = F_INT_TO_C(result_value, loc_exp_desc.linear_type);

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(listr_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(listr_idx)) = listr_idx;
               listr_idx = IL_NEXT_LIST_IDX(listr_idx);
   
               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               COPY_OPND(opnd, IL_OPND(list2_idx));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &loc_element);
               IL_ELEMENT(listr_idx) = 
                            F_INT_TO_C(result_value, loc_exp_desc.linear_type);
               stride_value = 
                            F_INT_TO_C(result_value, loc_exp_desc.linear_type);

               if ((((end_value - start_value) / stride_value) + 1L) <= 0) {

                  /* we have a zero sized array   */ 
                  zero_size_array = TRUE;
               }

               /* insert index_list which holds the index for this dim */

               NTR_IR_LIST_TBL(list2_idx);
               COPY_OPND(IL_OPND(list2_idx), IL_OPND(list_idx));

# ifdef _DEBUG
               if (IL_FLD(list_idx) == IL_Tbl_Idx) {
                  PRINTMSG(line, 626, Internal, col,
                           "no DAG", "interpret_ref");
               }
# endif

               IL_FLD(list_idx) = IL_Tbl_Idx;
               IL_IDX(list_idx) = index_list;
               IL_LIST_CNT(list_idx) = 2;
               IL_NEXT_LIST_IDX(index_list) = list2_idx;
   
            }
            else {
               COPY_OPND(opnd, IL_OPND(list_idx));
               loc_element = 1;
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &loc_element);
               IL_ELEMENT(index_list) = 
                            F_INT_TO_C(result_value, loc_exp_desc.linear_type);

               if (no_result_value) {
                  zero_size_array = TRUE;
               }
   
               NTR_IR_LIST_TBL(listr_idx);
               IL_ELEMENT(listr_idx) = loc_element;

               NTR_IR_LIST_TBL(list2_idx);
               COPY_OPND(IL_OPND(list2_idx), IL_OPND(list_idx));

# ifdef _DEBUG
               if (IL_FLD(list_idx) == IL_Tbl_Idx) {
                  PRINTMSG(line, 626, Internal, col,
                           "no DAG", "interpret_ref");
               }
# endif
               IL_FLD(list_idx) = IL_Tbl_Idx;
               IL_IDX(list_idx) = index_list;
               IL_LIST_CNT(list_idx) = 3;
               IL_NEXT_LIST_IDX(index_list) = listr_idx;
               IL_NEXT_LIST_IDX(listr_idx) = list2_idx;
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
      else {

         while (OPND_FLD(opnd) == IR_Tbl_Idx) {
 
            if (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr ||
                IR_OPR(OPND_IDX(opnd)) == Section_Subscript_Opr) {
               rank_idx = OPND_IDX(opnd);
               break;
            }
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         base_attr_idx = find_base_attr(&(IR_OPND_L(rank_idx)), &line, &col);
         bd_idx = ATD_ARRAY_IDX(base_attr_idx);
      }

      if (zero_size_array) {
         list_idx = NULL_IDX;
         no_result_value = TRUE;
         goto ZERO_ARRAY;
      }

      list_idx = IR_IDX_R(rank_idx);
      bit_offset  = IL_ELEMENT(list_idx);
  
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      list2_idx = list_idx;

      if (TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Structure &&
          ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(base_attr_idx)))) {

         sm_in_bits = 8;
      }
      /* OSP_467, #2, we calculate the stride for these type differently
       * Refer long64 sm_unit_in_bits(int) in sytb.c */
      else if ( TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Integer ||
                TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Logical ||
                TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ptr ||
                TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ch_Ptr ||
                TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Real ||
                TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Complex ) {
         sm_in_bits = storage_bit_size_tbl[CG_INTEGER_DEFAULT_TYPE];
      }
      else {
         sm_in_bits = sm_unit_in_bits(ATD_TYPE_IDX(base_attr_idx));
      }

      for (i = 1; i <= BD_RANK(bd_idx); i++) {
         bit_offset += (IL_ELEMENT(IL_IDX(list2_idx)) - 
                                        CN_INT_TO_C(BD_LB_IDX(bd_idx,i)))
                          * CN_INT_TO_C(BD_SM_IDX(bd_idx,i))
                          * sm_in_bits;

         list2_idx = IL_NEXT_LIST_IDX(list2_idx);
      }

      while (list_idx) {
         list2_idx = IL_IDX(list_idx);

         if (IL_VECTOR_SUBSCRIPT(list_idx)) {

            listr_idx = IL_NEXT_LIST_IDX(list2_idx);

            if (IL_ELEMENT(listr_idx) > 0) {
               /* get the next index */
               COPY_OPND(opnd, IL_OPND(IL_NEXT_LIST_IDX(listr_idx)));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &(IL_ELEMENT(listr_idx)));
               IL_ELEMENT(list2_idx) = 
                          F_INT_TO_C(result_value, loc_exp_desc.linear_type);
               break;
            }
            else {

               /* done with this dimension, reset to first value */
               IL_ELEMENT(listr_idx) = 1;
               COPY_OPND(opnd, IL_OPND(IL_NEXT_LIST_IDX(listr_idx)));
               ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                          &(IL_ELEMENT(listr_idx)));
               IL_ELEMENT(list2_idx) = 
                             F_INT_TO_C(result_value, loc_exp_desc.linear_type);
               /* no break, continue on with loop */
            }
         }
         else if (IL_FLD(IL_NEXT_LIST_IDX(list2_idx)) == IR_Tbl_Idx           &&
                  IR_OPR(IL_IDX(IL_NEXT_LIST_IDX(list2_idx))) == Triplet_Opr) {

            listr_idx    = IR_IDX_R(IL_IDX(IL_NEXT_LIST_IDX(list2_idx)));
            start_value  = IL_ELEMENT(listr_idx);
            listr_idx    = IL_NEXT_LIST_IDX(listr_idx);
            end_value    = IL_ELEMENT(listr_idx);
            listr_idx    = IL_NEXT_LIST_IDX(listr_idx);
            stride_value = IL_ELEMENT(listr_idx);
            index        = IL_ELEMENT(list2_idx);

            if (stride_value < 0) {

               if (index + stride_value >= end_value) {
                  IL_ELEMENT(list2_idx) += stride_value;
                  break;
               }
               else {
                  IL_ELEMENT(list2_idx) = start_value;
               }
            }
            else {

               if (index + stride_value <= end_value) {
                  IL_ELEMENT(list2_idx) += stride_value;
                  break;
               }
               else {
                  IL_ELEMENT(list2_idx) = start_value;
               }
            }

         }
         else {
            /* scalar dimension. Intentionally blank */
         }
      
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

ZERO_ARRAY:

      if (list_idx == NULL_IDX) {
         /* all done */
         *element = -1;

         /* reset the tree to its original state */
         list_idx = IR_IDX_R(rank_idx);
         IR_IDX_R(rank_idx) = IL_NEXT_LIST_IDX(list_idx);
         FREE_IR_LIST_NODE(list_idx);

         list_idx = IR_IDX_R(rank_idx);
         while (list_idx) {

            list2_idx = IL_IDX(list_idx);

            if (IL_VECTOR_SUBSCRIPT(list_idx)) {
               COPY_OPND(IL_OPND(list_idx), 
                      IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx))));
               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)));
               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list2_idx));

               FREE_IR_LIST_NODE(list2_idx);
            }
            else if (IL_FLD(IL_NEXT_LIST_IDX(list2_idx)) == IR_Tbl_Idx &&
                     IR_OPR(IL_IDX(IL_NEXT_LIST_IDX(list2_idx))) == 
                                                          Triplet_Opr) {

               COPY_OPND(IL_OPND(list_idx), 
                         IL_OPND(IL_NEXT_LIST_IDX(list2_idx)));

               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list2_idx));
               FREE_IR_LIST_NODE(list2_idx);

               list2_idx = IR_IDX_R(IL_IDX(list_idx));
               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)));
               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list2_idx));
               FREE_IR_LIST_NODE(list2_idx);
               IR_FLD_R(IL_IDX(list_idx)) = NO_Tbl_Idx;
               IR_IDX_R(IL_IDX(list_idx)) = NULL_IDX;
            }
            else {

               COPY_OPND(IL_OPND(list_idx),
                         IL_OPND(IL_NEXT_LIST_IDX(list2_idx)));

               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list2_idx));
               FREE_IR_LIST_NODE(list2_idx);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
      else {
         (*element)++;
      }

      if (single_value_const) {
         bit_offset = 0;
      }

      if (no_result_value) {
         /* intentionally blank */
      }
      else if (exp_desc->type == Character) {

         if ((char_result_offset + char_len) >= char_result_buffer_len) {

            enlarge_char_result_buffer();
         }

         for (i = 0; i < char_len; i++) {  /* BRIANJ */
            char_result_buffer[char_result_offset] =
               *((char *)&(CN_CONST(base_cn_idx))
               + (bit_offset/CHAR_BIT) + i);

            char_result_offset++;
         }
      }
      else {
         if (single_value_const &&
             num_bits < TARGET_BITS_PER_WORD &&
             (exp_desc->type == Integer ||
              exp_desc->type == Real ||
              exp_desc->type == Logical)) {

            result_value[0] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx));
         }
         else if (num_bits % TARGET_BITS_PER_WORD != 0) {

            word_offset = bit_offset/TARGET_BITS_PER_WORD;
        
       /* KAYKAY BRIANJ */

            result_value[0] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx) +
                                          word_offset);

# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
            result_value[0] = result_value[0] >>
                                 (bit_offset % TARGET_BITS_PER_WORD);
            if (num_bits == 8) {
               result_value[0] = result_value[0] & 0XFF;
            }
            else if (num_bits == 16) {
               result_value[0] = result_value[0] & 0XFFFF;
            }
            else if (num_bits == 32) {
               result_value[0] = result_value[0] & 0XFFFFFFFF;
            }
# else

            /* now shift out the bad bits */
            result_value[0] = result_value[0] << 
                    (bit_offset % TARGET_BITS_PER_WORD);

            /* and shift down the good */
            result_value[0] = result_value[0] >>
                    (TARGET_BITS_PER_WORD - num_bits);
# endif

         }
         else {

            word_offset = bit_offset/TARGET_BITS_PER_WORD;
            num_words = num_bits/TARGET_BITS_PER_WORD;

            for (i = 0; i < num_words; i++) {
               result_value[i] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx) +
                                       word_offset + i);
            }
         }
      }

   }
   else if (rank == 0) {

      while (OPND_FLD(opnd) == IR_Tbl_Idx) {

         ir_idx = OPND_IDX(opnd);

         switch (IR_OPR(ir_idx)) {

            case Struct_Opr :
               bit_offset +=CN_INT_TO_C(ATD_CPNT_OFFSET_IDX(IR_IDX_R(ir_idx)));
               break;

            case Subscript_Opr :
               base_attr_idx = find_base_attr(&opnd, &line, &col);
               bd_idx = ATD_ARRAY_IDX(base_attr_idx);

               list_idx = IR_IDX_R(ir_idx);

               if (TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Structure &&
                   ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(base_attr_idx)))) {

                  sm_in_bits = 8;
               }
	       /* OSP_467, #2, we calculate the stride for these type differently
		* Refer long64 sm_unit_in_bits(int) in sytb.c */
               else if ( TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Integer ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Logical ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ptr ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ch_Ptr ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Real ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Complex ) {
                  sm_in_bits = storage_bit_size_tbl[CG_INTEGER_DEFAULT_TYPE];
               }
               else {
                  sm_in_bits = sm_unit_in_bits(ATD_TYPE_IDX(base_attr_idx));
               }

               for (i = 1; i <= BD_RANK(bd_idx); i++) {

                  loc_element = 0;
                  COPY_OPND(opnd2, IL_OPND(list_idx));
                  ok = interpret_constructor(&opnd2, &loc_exp_desc,
                                             FALSE, &loc_element);


                  bit_offset += (F_INT_TO_C(result_value, 
                                            loc_exp_desc.linear_type) - 
                                        CN_INT_TO_C(BD_LB_IDX(bd_idx,i)))
                             * CN_INT_TO_C(BD_SM_IDX(bd_idx,i))
                             * sm_in_bits;

                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
               break;
         }

         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (exp_desc->type == Character) {

         /* add in the substring offset */
         bit_offset = (substring_offset * CHAR_BIT) + bit_offset;
      }

      if (single_value_const) {
         bit_offset = 0;
      }

      if (no_result_value) {
         /* intentionally blank */
      }
      else if (exp_desc->type == Character) {

         if ((char_result_offset + char_len) >= char_result_buffer_len) {

            enlarge_char_result_buffer();
         }

         for (i = 0; i < char_len; i++) {
            char_result_buffer[char_result_offset] =
               *((char *)&(CN_CONST(base_cn_idx))
               + (bit_offset/CHAR_BIT) + i);

            char_result_offset++;
         }
      }
      else if (exp_desc->type == Structure) {
         /* just write it in the constant here, no need and no room */
         /* to pass it up.                                          */

         /* set exp_desc->constant to false to prevent anyone else  */
         /* from writing the constant.                              */
         exp_desc->constant = FALSE;


         /* treat all structures like character since they may be non */
         /* word length because of short types.                       */

         char_ptr = (char *) &(CN_CONST(the_cn_idx))  /* BRIANJ */
                          + (the_cn_bit_offset/CHAR_BIT);

         the_cn_bit_offset += num_bits;

         char_ptr2 = (char *)&(CN_CONST(base_cn_idx)) + (bit_offset/CHAR_BIT);

         char_len = num_bits/CHAR_BIT;

         for (i = 0; i < char_len; i++) {
            char_ptr[i] = char_ptr2[i];
         }
      }
      else {
         if (single_value_const &&
             num_bits < TARGET_BITS_PER_WORD &&
             (exp_desc->type == Integer ||
              exp_desc->type == Real ||
              exp_desc->type == Logical)) {

            result_value[0] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx));
         }
         else if (num_bits % TARGET_BITS_PER_WORD != 0) {

            word_offset = bit_offset/TARGET_BITS_PER_WORD;

            result_value[0] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx) +
                                          word_offset);

# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
            result_value[0] = result_value[0] >> 
                                 (bit_offset % TARGET_BITS_PER_WORD);
            if (num_bits == 8) {
               result_value[0] = result_value[0] & 0XFF;
            }
            else if (num_bits == 16) {
               result_value[0] = result_value[0] & 0XFFFF;
            }
            else if (num_bits == 32) {
               result_value[0] = result_value[0] & 0XFFFFFFFF;
            }
# else

            /* now shift out the bad bits */
            result_value[0] = result_value[0] <<
                    (bit_offset % TARGET_BITS_PER_WORD);

            /* and shift down the good */
            result_value[0] = result_value[0] >>
                    (TARGET_BITS_PER_WORD - num_bits);
# endif

         }
         else {

            word_offset = bit_offset/TARGET_BITS_PER_WORD;
            num_words = num_bits/TARGET_BITS_PER_WORD;

            for (i = 0; i < num_words; i++) {
               result_value[i] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx) +
                                       word_offset + i);
            }
         }
      }

      if (*element > 0) {
         *element = -1;
      }
   }
   else {
      /* not in array syntax, but rank > 0 */
      /* turn off the constant flag so no one else "writes constant" */
      exp_desc->constant = FALSE;
      zero_size_array = FALSE;
      cn_bit_offset = 0;

      while (OPND_FLD(opnd) == IR_Tbl_Idx) {

         ir_idx = OPND_IDX(opnd);

         switch (IR_OPR(ir_idx)) {

            case Struct_Opr :
               cn_bit_offset += CN_INT_TO_C(ATD_CPNT_OFFSET_IDX(
                                               IR_IDX_R(ir_idx)));
               break;

            case Whole_Subscript_Opr   :
            case Section_Subscript_Opr :

               rank_idx = ir_idx;
               break;

            case Subscript_Opr :
               base_attr_idx = find_base_attr(&opnd, &line, &col);
               bd_idx = ATD_ARRAY_IDX(base_attr_idx);

               list_idx = IR_IDX_R(ir_idx);

               if (TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Structure &&
                   ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(base_attr_idx)))) {

                  sm_in_bits = 8;
               }
	       /* OSP_467, #2, we calculate the stride for these type differently
		* Refer long64 sm_unit_in_bits(int) in sytb.c */
               else if ( TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Integer ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Logical ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ptr ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ch_Ptr ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Real ||
                         TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Complex ) {
                  sm_in_bits = storage_bit_size_tbl[CG_INTEGER_DEFAULT_TYPE];
               }
               else {
                  sm_in_bits = sm_unit_in_bits(ATD_TYPE_IDX(base_attr_idx));
               }

               for (i = 1; i <= BD_RANK(bd_idx); i++) {

                  loc_element = 0;
                  COPY_OPND(opnd2, IL_OPND(list_idx));
                  ok = interpret_constructor(&opnd2, &loc_exp_desc,
                                             FALSE, &loc_element);
                  type_idx = Integer_8;

                  ok = folder_driver((char *)result_value,
                                     loc_exp_desc.linear_type,
                                     (char *) CN_CONST(BD_LB_IDX(bd_idx,i)),
                                     CN_TYPE_IDX(BD_LB_IDX(bd_idx,i)),
                                     loc_value,
                                    &type_idx,
                                     line,
                                     col,
                                     2,
                                     Minus_Opr);

                  ok = folder_driver((char *)loc_value,
                                     type_idx,
                                     (char *) CN_CONST(BD_SM_IDX(bd_idx,i)),
                                     CN_TYPE_IDX(BD_SM_IDX(bd_idx,i)),
                                     loc_value,
                                    &type_idx,
                                     line,
                                     col,
                                     2,
                                     Mult_Opr);

                  cn_bit_offset += F_INT_TO_C(loc_value, type_idx) * sm_in_bits;

                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
               break;
         }

         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (exp_desc->type == Character) {
         /* add in substring offset */
         cn_bit_offset += substring_offset * CHAR_BIT;
      }

      base_attr_idx = find_base_attr(&(IR_OPND_L(rank_idx)), &line, &col);
      bd_idx = ATD_ARRAY_IDX(base_attr_idx);
      list_idx = IR_IDX_R(rank_idx);

      for (i = 1; i <= BD_RANK(bd_idx); i++) {

         if (IL_FLD(list_idx) == IR_Tbl_Idx           &&
             IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

            is_vec_subscript[i] = FALSE;
            rank_array[i] = TRUE;
            loc_element = 0;

            list2_idx = IR_IDX_L(IL_IDX(list_idx));
            COPY_OPND(opnd, IL_OPND(list2_idx));
            ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                       &loc_element);
            index_array[i] = F_INT_TO_C(result_value, loc_exp_desc.linear_type);
            start_array[i] = index_array[i];

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            COPY_OPND(opnd, IL_OPND(list2_idx));
            ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                       &loc_element);
            end_array[i] = F_INT_TO_C(result_value, loc_exp_desc.linear_type);

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            COPY_OPND(opnd, IL_OPND(list2_idx));
            ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                       &loc_element);
            stride_array[i] = F_INT_TO_C(result_value,loc_exp_desc.linear_type);

            if ((((end_array[i] - start_array[i]) / stride_array[i]) + 1L) 
                                                                        <= 0) {

               /* we have a zero sized array   */
               zero_size_array = TRUE;
            }

            if (stride_array[i] < 0) {
               neg_stride[i] = TRUE;
            }
            else {
               neg_stride[i] = FALSE;
            }

         }
         else {
            COPY_OPND(opnd, IL_OPND(list_idx));
            loc_element = 1;
            ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                       &loc_element);
            index_array[i] = F_INT_TO_C(result_value, loc_exp_desc.linear_type);

            if (no_result_value) {
               zero_size_array = TRUE;
            }

            if (loc_element < 0) {
               rank_array[i] = FALSE;
            }
            else {
               start_array[i] = loc_element;
               end_array[i] = list_idx;
               is_vec_subscript[i] = TRUE;
               rank_array[i] = TRUE;
            }
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      /* now loop around section */

      if (zero_size_array) {
         goto DONE;
      }

      while (TRUE) {
         bit_offset = 0;

         if (TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Structure &&
             ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(base_attr_idx)))) {

            sm_in_bits = 8;
         }
	 /* OSP_467, #2, we calculate the stride for these type differently
	  * Refer long64 sm_unit_in_bits(int) in sytb.c */
         else if ( TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Integer ||
                   TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Logical ||
                   TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ptr ||
                   TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == CRI_Ch_Ptr ||
                   TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Real ||
                   TYP_TYPE(ATD_TYPE_IDX(base_attr_idx)) == Complex ) {
            sm_in_bits = storage_bit_size_tbl[CG_INTEGER_DEFAULT_TYPE];
         }
         else {
            sm_in_bits = sm_unit_in_bits(ATD_TYPE_IDX(base_attr_idx));
         }

         for (i = 1; i <= BD_RANK(bd_idx); i++) {

            bit_offset += (index_array[i] - CN_INT_TO_C(BD_LB_IDX(bd_idx,i)))
                          * CN_INT_TO_C(BD_SM_IDX(bd_idx,i)) 
                          * sm_in_bits;
         }

         if (single_value_const) {
            bit_offset = 0;
            cn_bit_offset = 0;
         }

         if (exp_desc->type == Structure) {

            /* treat all structures like character since they may be non */
            /* word length because of short types.                       */

            char_ptr = (char *) &(CN_CONST(the_cn_idx))
                             + (the_cn_bit_offset/CHAR_BIT);

            the_cn_bit_offset += num_bits;

            char_ptr2 = (char *)&(CN_CONST(base_cn_idx)) 
                        + ((cn_bit_offset + bit_offset)/CHAR_BIT);

            char_len = num_bits/CHAR_BIT;

            for (i = 0; i < char_len; i++) {
               char_ptr[i] = char_ptr2[i];
            }
         }
         else {

            if (exp_desc->type == Character) {
               char_result_offset = 0;

               if ((char_result_offset + char_len) >= char_result_buffer_len) {

                  enlarge_char_result_buffer();
               }

               for (i = 0; i < char_len; i++) {
                  char_result_buffer[i] = *((char *)&(CN_CONST(base_cn_idx))
                             + ((cn_bit_offset + bit_offset)/CHAR_BIT) + i);
               }
            }
            else {
               if (single_value_const &&
                   num_bits < TARGET_BITS_PER_WORD &&
                   (exp_desc->type == Integer ||
                    exp_desc->type == Real ||
                    exp_desc->type == Logical)) {
      
                  result_value[0] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx));
               }
               else if (num_bits % TARGET_BITS_PER_WORD != 0) {

                  word_offset = (cn_bit_offset + bit_offset)/
                                       TARGET_BITS_PER_WORD;

                  result_value[0] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx) +
                                                word_offset);

# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
                  result_value[0] = result_value[0] >> 
                        ((cn_bit_offset + bit_offset) % TARGET_BITS_PER_WORD);
                  if (num_bits == 8) {
                     result_value[0] = result_value[0] & 0XFF;
                  }
                  else if (num_bits == 16) {
                     result_value[0] = result_value[0] & 0XFFFF;
                  }
                  else if (num_bits == 32) {
                     result_value[0] = result_value[0] & 0XFFFFFFFF;
                  }
# else

                  /* now shift out the bad bits */  /* BRIANJ */
                  result_value[0] = result_value[0] <<
                  ((cn_bit_offset + bit_offset) % TARGET_BITS_PER_WORD);

                  /* and shift down the good */
                  result_value[0] = result_value[0] >>
                          (TARGET_BITS_PER_WORD - num_bits);
# endif
               }
               else {

                  word_offset = (cn_bit_offset + bit_offset)/
                                      TARGET_BITS_PER_WORD;
                  num_words = num_bits/TARGET_BITS_PER_WORD;

                  for (i = 0; i < num_words; i++) {
                     result_value[i] = CP_CONSTANT(CN_POOL_IDX(base_cn_idx) +
                                             word_offset + i);
                  }
               }
            }

            write_constant(exp_desc->type_idx);

         }

         /* advance index array */

         i = 1;
         while (i <= BD_RANK(bd_idx)) {

            if (! rank_array[i]) {
               /* intentionally blank */
            }
            else if (is_vec_subscript[i]) {

               if (start_array[i] > 0) {
                  COPY_OPND(opnd, IL_OPND(end_array[i]));
                  ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                             &(start_array[i]));
                  index_array[i] = 
                          F_INT_TO_C(result_value, loc_exp_desc.linear_type);
                  break;
               }
               else if (i < BD_RANK(bd_idx)) {
                  start_array[i] = 1;
                  COPY_OPND(opnd, IL_OPND(end_array[i]));
                  ok = interpret_constructor(&opnd, &loc_exp_desc, FALSE,
                                             &(start_array[i]));
                  index_array[i] = 
                           F_INT_TO_C(result_value, loc_exp_desc.linear_type);
               }
            }
            else if (neg_stride[i]) {

               if (index_array[i] + stride_array[i] >= end_array[i]) {
                  index_array[i] += stride_array[i];
                  break;
               }
               else {
                  index_array[i] = start_array[i];
               }
            }
            else {

               if (index_array[i] + stride_array[i] <= end_array[i]) {
                  index_array[i] += stride_array[i];
                  break;
               }
               else {
                  index_array[i] = start_array[i];
               }
            }

            i++;

            if (i > BD_RANK(bd_idx)) {
               goto DONE;
            }
         }
      }
   }

DONE:

   TRACE (Func_Exit, "interpret_ref", NULL);

   return(ok);

}  /* interpret_ref */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do the initial alloc of the char_result_buffer if it has not been     *|
|*      allocated. Otherwise, realloc to the needed size.                     *|
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

static void enlarge_char_result_buffer(void)

{
   long64		new_size;


   TRACE (Func_Entry, "enlarge_char_result_buffer", NULL);

   new_size = char_result_buffer_len + 1024;

   if (char_result_buffer_len == 0) {

      /* must do original malloc */

      MEM_ALLOC(char_result_buffer, char, new_size);

   }
   else { /* do realloc */

      MEM_REALLOC(char_result_buffer, char, new_size);

   }

   char_result_buffer_len = new_size;

   TRACE (Func_Exit, "enlarge_char_result_buffer", NULL);

   return;

}  /* enlarge_char_result_buffer */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Copy the first element of an array constant to the rest of the const. *|
|*									      *|
|* Input parameters:							      *|
|*	exp_desc - expression descriptor that holds the shape and type.       *|
|*      num_elements - the number to broadcast.                               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void broadcast_scalar(expr_arg_type	*exp_desc,
		             long64		 num_elements)

{
   long64	 bcast_cn_word_offset;
   long64	 bits = 0;
   long64	 bytes = 0;
   long64	 char_num;
   char		*char_ptr_1;
   char		*char_ptr_2;
   long64	 cn_word_offset;
   long64	 i;
   long64	 k;
   int		 type_idx;
   long64	 words = 0;


   TRACE (Func_Entry, "broadcast_scalar", NULL);

   if (check_type_conversion &&
       exp_desc->type != Character &&
       target_type_idx != exp_desc->type_idx) {

      type_idx = target_type_idx;
   }
   else {
      type_idx = exp_desc->type_idx;
   }

   switch (TYP_TYPE(type_idx)) {
      case Typeless :
         bits = TYP_BIT_LEN(type_idx);
         break;

      case Integer :
      case Logical :
      case Real :
      case Complex :
         bits = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
         break;

      case Character:
         bits = CN_INT_TO_C(TYP_IDX(type_idx)) * CHAR_BIT;
         break;

      case Structure :
         bits = CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx)));
         break;
   }

   if (check_type_conversion && exp_desc->type == Character) {
      bits = CN_INT_TO_C(target_char_len_idx) * CHAR_BIT;
   }

   if (bits % TARGET_BITS_PER_WORD != 0) {
      bytes = bits/CHAR_BIT;
   }
   else {
      words = TARGET_BITS_TO_WORDS(bits);
   }

   if (words) {
      cn_word_offset = TARGET_BITS_TO_WORDS(the_cn_bit_offset);
      bcast_cn_word_offset = TARGET_BITS_TO_WORDS(bcast_cn_bit_offset);

      for (k = 2; k <= num_elements; k++) {
         for (i = 0; i < words; i++) {
            CP_CONSTANT(CN_POOL_IDX(the_cn_idx) + cn_word_offset) = 
              CP_CONSTANT(CN_POOL_IDX(the_cn_idx)  + bcast_cn_word_offset + i);
            cn_word_offset++;
         }
      }
   }
   else {
      char_ptr_2 = (char *) &(CN_CONST(the_cn_idx)) + 
                          (the_cn_bit_offset/CHAR_BIT);
      char_ptr_1 = (char *) &(CN_CONST(the_cn_idx)) +
                       + (bcast_cn_bit_offset/CHAR_BIT);
      char_num = 0;

      for (k = 2; k <= num_elements; k++) {
         for (i = 0; i < bytes; i++) {
            char_ptr_2[char_num] = char_ptr_1[i];
            char_num++;
         }
      }
   }

   the_cn_bit_offset += bits;

   TRACE (Func_Exit, "broadcast_scalar", NULL);

   return;

}  /* broadcast_scalar */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Interpret the Struct_Construct_Opr.                                   *|
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

static boolean interpret_struct_construct_opr(int		ir_idx,
					      expr_arg_type    *exp_desc,
					      boolean		count,
					      long64	       *element)

{
   int			attr_idx;
   int			bd_idx;
   long64		char_result_offset_l;
   expr_arg_type	exp_desc_l;
   int			i;
   int			list_idx;
   long64               loc_bcast_cn_bit_offset;
   long64               loc_element = 0;
   long64		num;
   boolean		ok = TRUE;
   opnd_type            opnd;
   int                  opnd_column;
   int                  opnd_line;
   save_env_type        save;
   int			sn_idx;
   long64               start_cn_bit_offset;


   TRACE (Func_Entry, "interpret_struct_construct_opr", NULL);

   save.check_type_conversion = check_type_conversion;

   /* just get the size from the structure definition */
   /* this is because of dalign stuff.                */

   bits_in_constructor     += CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(
                                                  IR_IDX_L(ir_idx)));
   save.bits_in_constructor = bits_in_constructor;

   check_type_conversion    = FALSE;
   save.target_type_idx     = target_type_idx;
   save.target_char_len_idx = target_char_len_idx;

   exp_desc->type_idx       = IR_TYPE_IDX(ir_idx);
   exp_desc->type           = TYP_TYPE(exp_desc->type_idx);
   exp_desc->linear_type    = TYP_LINEAR(exp_desc->type_idx);
   list_idx                 = IR_IDX_R(ir_idx);
   sn_idx                   = ATT_FIRST_CPNT_IDX(IR_IDX_L(ir_idx));

   start_cn_bit_offset = the_cn_bit_offset;

   if (! count &&
       *element > 0) {
      *element = -1;
   }

   while (list_idx) {

      attr_idx = SN_ATTR_IDX(sn_idx);

      if (! count) {
         the_cn_bit_offset = start_cn_bit_offset +
                             CN_INT_TO_C(ATD_CPNT_OFFSET_IDX(attr_idx));
      }


      switch (TYP_TYPE(ATD_TYPE_IDX(attr_idx))) {
      case Integer:
      case Real:
      case Complex:
      case Logical:
         check_type_conversion = TRUE;
         target_type_idx = ATD_TYPE_IDX(attr_idx);
         break;

      case Character:
         target_char_len_idx = TYP_IDX(ATD_TYPE_IDX(attr_idx));
         char_result_offset = 0;
         target_type_idx    = Character_1;
         check_type_conversion = TRUE;
         break;

      default:
         check_type_conversion = FALSE;
         break;
      }

      if (IL_FLD(list_idx) == IR_Tbl_Idx     &&
          IR_ARRAY_SYNTAX(IL_IDX(list_idx))) {

         bd_idx = ATD_ARRAY_IDX(attr_idx);

         COPY_OPND(opnd, IL_OPND(list_idx));

         if (count) {
            loc_element = 1;
            ok &= interpret_constructor(&opnd, &exp_desc_l, count,
                                        &loc_element);

            /* check conformance */

            for (i = 0; i < BD_RANK(bd_idx); i++) {

               if (fold_relationals(BD_XT_IDX(bd_idx, i + 1),
                                    exp_desc_l.shape[i].idx,
                                    Ne_Opr)) {
                  find_opnd_line_and_column(&opnd,
                                            &opnd_line,
                                            &opnd_column);

                  PRINTMSG(opnd_line, 252, Error, opnd_column);
                  ok = FALSE;
                  break;
               }
            }
         }
         else {
            loc_element = 1;
            while (loc_element > 0) {

               char_result_offset_l = char_result_offset;
               ok &= interpret_constructor(&opnd, &exp_desc_l, count,
                                          &loc_element);

               char_result_offset = char_result_offset_l;

               if (exp_desc_l.constant) {
                  write_constant(exp_desc_l.type_idx);
               }
            }
         }
      }
      else {

         loc_bcast_cn_bit_offset = the_cn_bit_offset;

         char_result_offset_l = char_result_offset;
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                    &loc_element) && ok;

         char_result_offset = char_result_offset_l;

         if (count) {

            if (ATD_ARRAY_IDX(attr_idx)) {

               bd_idx = ATD_ARRAY_IDX(attr_idx);

               if (BD_RANK(bd_idx) == exp_desc_l.rank) {

                  /* check conformance */
                  for (i = 0; i < BD_RANK(bd_idx); i++) {

                     if (fold_relationals(BD_XT_IDX(bd_idx, i + 1),
                                          exp_desc_l.shape[i].idx,
                                          Ne_Opr)) {

                        find_opnd_line_and_column(&opnd,
                                                  &opnd_line,
                                                  &opnd_column);

                        PRINTMSG(opnd_line, 252, Error,
                                 opnd_column);
                        ok = FALSE;
                        break;
                     }
                  }
               }
            }
         }
         else if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Deferred_Shape ||
                  (ATD_ARRAY_IDX(attr_idx) &&
                   compare_cn_and_value(BD_LEN_IDX(ATD_ARRAY_IDX(attr_idx)),
                                       0, Eq_Opr))) {

             
            /* structure component is a pointer array.     */
            /* structure component is a zero_sized array.  */
            /* this is intentionally blank. We do not want */
            /* to write anything in the constant.          */
         }
         else {

            if (exp_desc_l.constant) {
               write_constant(exp_desc_l.type_idx);

            }

            if (ATD_ARRAY_IDX(attr_idx) &&
                exp_desc_l.rank == 0) {

               bcast_cn_bit_offset  = loc_bcast_cn_bit_offset;
               num = CN_INT_TO_C(BD_LEN_IDX(ATD_ARRAY_IDX(attr_idx)));

               broadcast_scalar(&exp_desc_l, num);
            }
         }
      }

      list_idx      = IL_NEXT_LIST_IDX(list_idx);
      sn_idx        = SN_SIBLING_LINK(sn_idx);
   }

   check_type_conversion = save.check_type_conversion;
   target_type_idx       = save.target_type_idx;
   target_char_len_idx   = save.target_char_len_idx;
   bits_in_constructor   = save.bits_in_constructor;

   if (! count) {
      the_cn_bit_offset = start_cn_bit_offset +
             CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(exp_desc->type_idx)));
   }


   TRACE (Func_Exit, "interpret_struct_construct_opr", NULL);

   return(ok);

}  /* interpret_struct_construct_opr */

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

static boolean interpret_array_construct_opr(int		ir_idx,
                                             expr_arg_type     *exp_desc,
                                             boolean            count,
                                             long64            *element)

{
   long64               char_result_offset_l;
   int			col;
   expr_arg_type        exp_desc_l;
#ifdef KEY /* Bug 10177 */
   long64            	extent = 0;
#else /* KEY Bug 10177 */
   long64            	extent;
#endif /* KEY Bug 10177 */
   int			i;
   int			line;
   int			list_idx;
   long64               loc_element = 0;
   long64               longest_char_len = 0;
   boolean		ok = TRUE;
   opnd_type            opnd;
   int                  position_idx;
   long64               sub_elements;


   TRACE (Func_Entry, "interpret_array_construct_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   exp_desc->type_idx       = IR_TYPE_IDX(ir_idx);
   exp_desc->type           = TYP_TYPE(exp_desc->type_idx);
   exp_desc->linear_type    = TYP_LINEAR(exp_desc->type_idx);

   if (*element > 0) {
      /* this means we are in array syntax */
      loc_element = 1;

      if (count) {
         extent = 0L;

         list_idx = IR_IDX_R(ir_idx);
         while (list_idx) {

            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                       &loc_element) && ok;

            sub_elements = 1;

            if (exp_desc_l.type == Character &&
                char_result_len > longest_char_len) {

               if (longest_char_len != 0) {
                  unequal_char_lens = TRUE;
               }
               longest_char_len = char_result_len;
            }

            if (exp_desc_l.rank == 0) {
               extent++;
            }
            else {

               for (i = 0; i < exp_desc_l.rank; i++) {
                  if (exp_desc_l.shape[i].fld == CN_Tbl_Idx) {
                     sub_elements *= CN_INT_TO_C(exp_desc_l.shape[i].idx);
                  }
                  else {
                     break;
                  }
               }
               extent += sub_elements;
            }

            *element += sub_elements;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
      else {
         /* not count, in array syntax */
         /* get next value for array syntax */

/***************************************************************************\

   The position within this array constructor when in array syntax mode,
   is kept by a list entry node pointed to by position_idx. The current
   local element number is stored with the list entry (second word or opnd)
   and the IL_NEXT_LIST_IDX field points to the current array constructor
   item in the constructor list. The position_idx list entry is placed
   in the tree until the array is exhausted, then the tree is restored
   to its original state.

                            Array_Construct_Opr
                            /                 \
             (position_idx)+-----------------> +-> first array const item
                                               .
                                               .
                                               .

\***************************************************************************/

         if (*element == 1) {
            NTR_IR_LIST_TBL(position_idx);
            IR_IDX_L(ir_idx) = position_idx;
            IL_NEXT_LIST_IDX(position_idx) = IR_IDX_R(ir_idx);
            IL_ELEMENT(position_idx) = 1;
         }
         else {
            position_idx = IR_IDX_L(ir_idx);
# ifdef _DEBUG
            if (position_idx == NULL_IDX) {
               PRINTMSG(line, 983, Internal, col);
            }
# endif
         }

         char_result_offset_l = char_result_offset;
         COPY_OPND(opnd, IL_OPND(IL_NEXT_LIST_IDX(position_idx)));
         loc_element = (int) (IL_ELEMENT(position_idx));
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                          &loc_element) && ok;

         char_result_offset = char_result_offset_l;

         if (loc_element < 0) {
            if (IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(position_idx))) {
               IL_NEXT_LIST_IDX(position_idx) =
                   IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(position_idx));
               IL_ELEMENT(position_idx) = 1;
               (*element)++;
            }
            else {
               *element = -1;
               FREE_IR_LIST_NODE(position_idx);
               IR_IDX_L(ir_idx) = NULL_IDX;
            }
         }
         else {
            IL_ELEMENT(position_idx)++;
            (*element)++;
         }
      }
   }
   else {

      /* not in array syntax */

      extent = 0L;

      list_idx = IR_IDX_R(ir_idx);
      while (list_idx) {

         COPY_OPND(opnd, IL_OPND(list_idx));

         if (IL_FLD(list_idx) == IR_Tbl_Idx     &&
             IR_ARRAY_SYNTAX(IL_IDX(list_idx))) {

            /* not in array syntax, but above array syntax */

            loc_element = 1;

            if (count) {

               ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                          &loc_element) && ok;

               sub_elements = 1;

               if (exp_desc_l.rank == 0) {
                  extent++;
               }
               else {

                  for (i = 0; i < exp_desc_l.rank; i++) {
                     if (exp_desc_l.shape[i].fld == CN_Tbl_Idx) {
                        sub_elements *= CN_INT_TO_C(exp_desc_l.shape[i].idx);
                     }
                     else {
                        break;
                     }
                  }
                  extent += sub_elements;
               }

               if (exp_desc_l.type == Character) {
                  if (char_result_len > longest_char_len) {

                     if (longest_char_len != 0) {
                        unequal_char_lens = TRUE;
                     }
                     longest_char_len = char_result_len;
                  }
               }
               else if (exp_desc_l.constant) {
                  increment_count(&exp_desc_l);
               }
            }
            else {
               /* not count */
               /* set up loop around array syntax */

               loc_element = 1;
               while (loc_element >= 0) {
                  char_result_offset_l = char_result_offset;
                  ok = interpret_constructor(&opnd, &exp_desc_l,
                                  count, &loc_element) && ok;

                  char_result_offset = char_result_offset_l;

                  if (exp_desc_l.constant) {

                     write_constant(exp_desc_l.type_idx);
                  }
               }
            }
         }
         else {

            /* not in array syntax, not above array syntax */

            loc_element = 0;

            char_result_offset_l = char_result_offset;
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                       &loc_element) && ok;

            char_result_offset   = char_result_offset_l;

            if (count) {
               sub_elements = 1;

               if (exp_desc_l.rank == 0) {
                  extent++;
               }
               else {

                  for (i = 0; i < exp_desc_l.rank; i++) {
                     if (exp_desc_l.shape[i].fld == CN_Tbl_Idx) {
                        sub_elements *= CN_INT_TO_C(exp_desc_l.shape[i].idx);
                     }
                     else {
                        break;
                     }
                  }
                  extent += sub_elements;
               }

               if (exp_desc_l.type == Character) {
                  if (char_result_len > longest_char_len) {

                     if (longest_char_len != 0) {
                        unequal_char_lens = TRUE;
                     }
                     longest_char_len = char_result_len;
                  }
               }
               else if (exp_desc_l.constant) {
                  increment_count(&exp_desc_l);
               }

            }
            else {
               if (exp_desc_l.constant) {

                  write_constant(exp_desc_l.type_idx);
               }
            }
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   exp_desc->rank = 1;

   if (count) {
      exp_desc->shape[0].fld = CN_Tbl_Idx;
      exp_desc->shape[0].idx = C_INT_TO_CN(NULL_IDX, extent);

      if (exp_desc->type == Character) {
         char_result_len = longest_char_len;
         if (*element == 0) {
            increment_count(exp_desc);
         }
      }
   }


   TRACE (Func_Exit, "interpret_array_construct_opr", NULL);

   return(ok);

}  /* interpret_array_construct_opr */

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

static boolean interpret_unary_opr(int			ir_idx,
                                   expr_arg_type       *exp_desc,
                                   boolean              count,
                                   long64               *element)

{
   int			col;
   expr_arg_type        exp_desc_l;
   int			i;
   int			line;
   long_type            loc_value_l[MAX_WORDS_FOR_NUMERIC]; 
   boolean		ok = TRUE;
   opnd_type		opnd;
   int                  type_idx;


   TRACE (Func_Entry, "interpret_unary_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(IR_IDX_L(ir_idx)));
   }
   else {
      COPY_OPND(opnd, IR_OPND_L(ir_idx));
   }

   if (count) {
      if (IR_RANK(ir_idx) == 0) {
         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      }
      else {

         ok = interpret_constructor(&opnd, exp_desc, count,
                                    element);
         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
      }
   }
   else {
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                 element);
      exp_desc->constant = TRUE;

      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      if (IR_OPR(ir_idx) != Uplus_Opr && ! no_result_value) {

         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            loc_value_l[i] = result_value[i];
         }

         type_idx = exp_desc->type_idx;

         ok &= folder_driver((char *)loc_value_l,
                            exp_desc_l.type_idx,
                            NULL,
                            NULL_IDX,
                            result_value,
                           &type_idx,
                            line,
                            col,
                            1,
                            IR_OPR(ir_idx));

         exp_desc->type_idx = type_idx;

      }
   }

   TRACE (Func_Exit, "interpret_unary_opr", NULL);

   return(ok);

}  /* interpret_unary_opr */

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

static boolean interpret_binary_opr(int                  ir_idx,
                                    expr_arg_type       *exp_desc,
                                    boolean              count,
                                    long64              *element)


{
   long64               char_result_len_l;
   long64               char_result_len_r;
   long64               char_result_offset_l;
   long64               char_result_offset_r;
   int                  col;
   expr_arg_type        exp_desc_l;
   expr_arg_type        exp_desc_r;
   int                  i;
   int                  line;
   long64               loc_element_l = 0;
   long64               loc_element_r = 0;
   boolean              loc_no_result_value = FALSE;
   long_type            loc_value_l[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_r[MAX_WORDS_FOR_NUMERIC];
   boolean              ok = TRUE;
   opnd_type            opnd_l;
   opnd_type            opnd_r;
   int                  type_idx;


   TRACE (Func_Entry, "interpret_binary_opr", NULL);

   line = IR_LINE_NUM(ir_idx); 
   col  = IR_COL_NUM(ir_idx);

   if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {

      COPY_OPND(opnd_l, IL_OPND(IR_IDX_L(ir_idx)));
      COPY_OPND(opnd_r, IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx))));

   }
   else {
      COPY_OPND(opnd_l, IR_OPND_L(ir_idx));
      COPY_OPND(opnd_r, IR_OPND_R(ir_idx));
   }

   if (count) {
      if (IR_RANK(ir_idx) == 0) {
         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      }
      else {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         loc_element_l = *element;
         ok = interpret_constructor(&opnd_l, &exp_desc_l, count, 
                                    &loc_element_l);

         loc_element_l = *element;
         ok &= interpret_constructor(&opnd_r, &exp_desc_r, count, 
                                     &loc_element_l);

         /* check conformance. */

         if (exp_desc_r.rank == exp_desc_l.rank) {

            for (i = 0; i < exp_desc_r.rank; i++) {
               /* assumes that all extents are constant now */

               if (fold_relationals(OPND_IDX(exp_desc_l.shape[i]),
                                    OPND_IDX(exp_desc_r.shape[i]),
                                    Ne_Opr)) {

                  /* non conforming array syntax */
                  PRINTMSG(line, 252, Error, col);
                  ok = FALSE;
                  break;
               }
            }
            exp_desc->rank = exp_desc_r.rank;
            COPY_SHAPE(exp_desc->shape,exp_desc_r.shape,
                       exp_desc_r.rank);
         }
         else if (exp_desc_r.rank > exp_desc_l.rank) {
            exp_desc->rank = exp_desc_r.rank;
            COPY_SHAPE(exp_desc->shape,exp_desc_r.shape,
                       exp_desc_r.rank);
         }
         else {
            exp_desc->rank = exp_desc_l.rank;
            COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                       exp_desc_l.rank);
         }
      }
   }
   else {
      exp_desc->constant = TRUE;

      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      char_result_offset_l = char_result_offset;
      loc_element_l = *element;
      ok = interpret_constructor(&opnd_l, &exp_desc_l, count, &loc_element_l);

      char_result_len_l = char_result_len;

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_l[i] = result_value[i];
      }

      char_result_offset = char_result_offset_l + char_result_len;
      char_result_offset_r = char_result_offset;
      loc_element_r = *element;
      ok &= interpret_constructor(&opnd_r, &exp_desc_r, count, &loc_element_r);

      char_result_len_r = char_result_len;

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_r[i] = result_value[i];
      }

      *element = (loc_element_r > loc_element_l) ?
                  loc_element_r : loc_element_l;

      if (loc_no_result_value) {
         goto EXIT;
      }

      if (exp_desc_l.type == Character &&
          exp_desc_r.type == Character) {

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Character;
         TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_l);
         exp_desc_l.type_idx		= ntr_type_tbl();

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Character;
         TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_r);
         exp_desc_r.type_idx		= ntr_type_tbl();

         type_idx = exp_desc->type_idx;

         ok &= folder_driver(&(char_result_buffer[char_result_offset_l]),
                             exp_desc_l.type_idx,
                             &(char_result_buffer[char_result_offset_r]),
                             exp_desc_r.type_idx,
                             result_value,
                             &type_idx,
                             line,
                             col,
                             2,
                             IR_OPR(ir_idx));

         exp_desc->type_idx = type_idx;
      }
      else {
         type_idx = exp_desc->type_idx;

         ok &= folder_driver((char *)loc_value_l,
                            exp_desc_l.type_idx,
                            (char *)loc_value_r,
                            exp_desc_r.type_idx,
                            result_value,
                            &type_idx,
                            line,
                            col,
                            2,
                            IR_OPR(ir_idx));

         exp_desc->type_idx = type_idx;
      }


      char_result_offset = char_result_offset_l;
   }

EXIT:

   TRACE (Func_Exit, "interpret_binary_opr", NULL);

   return(ok);

}  /* interpret_binary_opr */

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

static boolean interpret_concat_opr(int                  ir_idx,
                                    expr_arg_type       *exp_desc,
                                    boolean              count,
                                    long64              *element)

{
   long64               char_result_offset_l;
   expr_arg_type        exp_desc_l;
   expr_arg_type        exp_desc_r;
   int			i;
   int			list_idx;
   long64               loc_element_l = 0;
   long64               loc_element_r = 0;
   long64               longest_char_len = 0;
   boolean		ok = TRUE;
   opnd_type		opnd;


   TRACE (Func_Entry, "interpret_concat_opr", NULL);

   exp_desc->constant = TRUE;

   exp_desc->type_idx       = IR_TYPE_IDX(ir_idx);
   exp_desc->type           = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (exp_desc->type == Character &&
       IR_RANK(ir_idx) == 0        &&
       compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                            MAX_CHARS_IN_TYPELESS,
                            Le_Opr)) {
      exp_desc->linear_type = Short_Char_Const;
   }

   if (count) {
      longest_char_len      = 0;
      list_idx              = IR_IDX_L(ir_idx);

      while (list_idx) {

         COPY_OPND(opnd, IL_OPND(list_idx));
         loc_element_l = *element;
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                    &loc_element_l);

         longest_char_len += char_result_len;

         if (list_idx == IR_IDX_L(ir_idx)) {
            exp_desc_r = exp_desc_l;
         }
         else {
            /* check conformance */
            if (exp_desc_r.rank == exp_desc_l.rank) {

               for (i = 0; i < exp_desc_r.rank; i++) {
                  /* assumes that all extents are constant now */

                  if (fold_relationals(OPND_IDX(exp_desc_l.shape[i]),
                                       OPND_IDX(exp_desc_r.shape[i]),
                                       Ne_Opr)) {

                     /* non conforming array syntax */
                     PRINTMSG(IR_LINE_NUM(ir_idx), 252, Error,
                              IR_COL_NUM(ir_idx));
                     ok = FALSE;
                     break;
                  }
               }
               exp_desc->rank = exp_desc_r.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_r.shape,
                          exp_desc_r.rank);
            }
            else if (exp_desc_r.rank > exp_desc_l.rank) {
               exp_desc->rank = exp_desc_r.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_r.shape,
                          exp_desc_r.rank);
            }
            else {
               exp_desc->rank = exp_desc_l.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                          exp_desc_l.rank);
               exp_desc_r = exp_desc_l;
            }
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      char_result_len = longest_char_len;
   }
   else {

      char_result_offset_l = char_result_offset;
      longest_char_len     = 0;

      list_idx = IR_IDX_L(ir_idx);

      if (*element > 0) {
         loc_element_r = -1;
      }
      else {
         loc_element_r = 0;
      }

      while (list_idx) {

         char_result_offset = char_result_offset_l + longest_char_len;
         COPY_OPND(opnd, IL_OPND(list_idx));
         loc_element_l = *element;
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                    &loc_element_l);

         longest_char_len += char_result_len;

         if (loc_element_l > loc_element_r) {
            loc_element_r = loc_element_l;
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      char_result_len    = longest_char_len;
      char_result_offset = char_result_offset_l;
      *element           = loc_element_r;
   }


   TRACE (Func_Exit, "interpret_concat_opr", NULL);

   return(ok);

}  /* interpret_concat_opr */

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

static boolean interpret_trim_intrinsic(int                  ir_idx,
                                        expr_arg_type       *exp_desc,
                                        boolean              count,
                                        long64              *element)

{
   long64               char_result_offset_l;
   expr_arg_type        exp_desc_l;
   int			ir2_idx;
   long64               loc_element = 0;
   boolean		ok = TRUE;
   opnd_type		opnd;


   TRACE (Func_Entry, "interpret_trim_intrinsic", NULL);

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (count) {

      NTR_IR_TBL(ir2_idx);
      IR_OPR(ir2_idx) = Len_Trim_Opr;
      IR_TYPE_IDX(ir2_idx) = CG_INTEGER_DEFAULT_TYPE;

      copy_subtree(&IL_OPND(IR_IDX_R(ir_idx)), &opnd);
      COPY_OPND(IR_OPND_L(ir2_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir2_idx;

      loc_element = 0;
      ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                 &loc_element);

      char_result_len = F_INT_TO_C(result_value, exp_desc_l.linear_type);
      if (char_result_len < 0) {
         char_result_len = 0;
      }
   }
   else {

      loc_element = 0;
      char_result_offset_l = char_result_offset;
      COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                 &loc_element);

      while (char_result_len > 0 &&
             char_result_buffer[char_result_offset_l +
                                char_result_len - 1] == ' ') {

         char_result_len--;
         char_result_offset--;
      }

      if (*element > 0) {
         *element = -1;
      }
   }


   TRACE (Func_Exit, "interpret_trim_intrinsic", NULL);

   return(ok);

}  /* interpret_trim_intrinsic */

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

static boolean interpret_adjustl_intrinsic(int                  ir_idx,
                                           expr_arg_type       *exp_desc,
                                           boolean              count,
                                           long64              *element)

{
   long64               char_result_len_l;
   long64               char_result_offset_l;
   int			col;
   expr_arg_type        exp_desc_l;
   int			line;
   long_type            loc_value_l[MAX_WORDS_FOR_NUMERIC];
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			spec_idx;
   opnd_type		tmp_opnd;
   int			type_idx;


   TRACE (Func_Entry, "interpret_adjustl_intrinsic", NULL);

   spec_idx = IR_IDX_L(ir_idx);
   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (count) {

      ok = interpret_constructor(&opnd, exp_desc, count,
                                 element);

   }
   else {

      char_result_offset_l = char_result_offset;
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                       element);

      char_result_offset = char_result_offset_l;
      char_result_len_l = char_result_len;

      *(exp_desc) = exp_desc_l;

      exp_desc->constant = TRUE;

      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)		= Character;
      TYP_LINEAR(TYP_WORK_IDX)		= CHARACTER_DEFAULT_TYPE;
      TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_l);
      type_idx				= ntr_type_tbl();

      exp_desc->type_idx = type_idx;

      ok = folder_driver(&(char_result_buffer[char_result_offset_l]),
                         type_idx,
                         NULL,
                         NULL_IDX,
                         loc_value_l,
                         &type_idx,
                         line,
                         col,
                         1,
              (ATP_INTRIN_ENUM(spec_idx) == Adjustl_Intrinsic ?
                     Adjustl_Opr : Adjustr_Opr));


      OPND_FLD(tmp_opnd) = CN_Tbl_Idx;
      OPND_IDX(tmp_opnd) = loc_value_l[0];  /* BRIANJ */
      OPND_LINE_NUM(tmp_opnd) = line;
      OPND_COL_NUM(tmp_opnd)  = col;

      char_result_offset = char_result_offset_l;

      ok = interpret_constructor(&tmp_opnd, exp_desc, FALSE,
                                 element);

      exp_desc->type_idx = type_idx;
      exp_desc->linear_type = TYP_LINEAR(type_idx);

   }

   TRACE (Func_Exit, "interpret_adjustl_intrinsic", NULL);

   return(ok);

}  /* interpret_adjustl_intrinsic */

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

static boolean interpret_repeat_intrinsic(int                  ir_idx,
                                          expr_arg_type       *exp_desc,
                                          boolean              count,
                                          long64              *element)


{
   char			*char_ptr;
   long64               char_result_offset_l;
   int			cn_idx;
   expr_arg_type        exp_desc_l;
   long64		i;
   int			info_idx;
   int			ir2_idx;
   long64		k;
   int			list_idx;
   long64               loc_element = 0;
   boolean              ok = TRUE;
   opnd_type            opnd;


   TRACE (Func_Entry, "interpret_repeat_intrinsic", NULL);

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (count) {

      info_idx = IL_ARG_DESC_IDX(IR_IDX_R(ir_idx));
      list_idx = IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx));

      NTR_IR_TBL(ir2_idx);
      IR_OPR(ir2_idx) = Mult_Opr;
      IR_TYPE_IDX(ir2_idx) = CG_INTEGER_DEFAULT_TYPE;

      copy_subtree(&(arg_info_list[info_idx].ed.char_len), &opnd);
      COPY_OPND(IR_OPND_L(ir2_idx), opnd);

      copy_subtree(&IL_OPND(list_idx), &opnd);
      COPY_OPND(IR_OPND_R(ir2_idx), opnd);

      IR_LINE_NUM_R(ir2_idx) = stmt_start_line;
      IR_COL_NUM_R(ir2_idx)  = stmt_start_col;

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir2_idx;

      loc_element = 0;
      ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                 &loc_element);

      char_result_len = F_INT_TO_C(result_value, exp_desc_l.linear_type);
      if (char_result_len < 0) {
         char_result_len = 0;
      }
   }
   else {

      loc_element = 0;
      char_result_offset_l = char_result_offset;
      COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                 &loc_element);

      loc_element = 0;
      COPY_OPND(opnd,
                IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))));
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                 &loc_element);

      while ((char_result_offset_l + ((F_INT_TO_C(result_value, 
                                            exp_desc_l.linear_type) - 1)
                                * char_result_len)) >=
             char_result_buffer_len) {

         enlarge_char_result_buffer();
      }

      char_ptr = &(char_result_buffer[char_result_offset_l +
                                      char_result_len]);


      cn_idx = 0;

      for (k = 1; k < F_INT_TO_C(result_value, exp_desc_l.linear_type); k++) {

         for (i = 0; i < char_result_len; i++) {
            char_ptr[cn_idx] = char_result_buffer[i + char_result_offset_l];
            cn_idx++;
            char_result_offset++;
         }
      }

      char_result_len = char_result_len * 
                            F_INT_TO_C(result_value, exp_desc_l.linear_type);

      if (char_result_len < 0) {
         char_result_len = 0;
      }

      if (*element > 0) {
         *element = -1;
      }
   }


   TRACE (Func_Exit, "interpret_repeat_intrinsic", NULL);

   return(ok);

}  /* interpret_repeat_intrinsic */

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

static boolean interpret_transfer_intrinsic(int                  ir_idx,
                                            expr_arg_type       *exp_desc,
                                            boolean              count,
                                            long64              *element)


{
   int			cn_idx;
   int			col;
   int_dope_type        dope_result;
   int_dope_type        dope_1;
   int_dope_type        dope_2;
   expr_arg_type        exp_desc_l;
   long64               extent;
   long64   	 	i;
   long64    		k;
   int			line;
   int			list_idx;
   int                  list_idx1;
   int                  list_idx2;
   int                  list_idx3;
   long64               loc_element_l = 0;
   long64               longest_char_len = 0;
   boolean		ok = TRUE;
   opnd_type		opnd;
   save_env_type        save;
   int			tmp_idx;
   int			type_idx;
   int			type_idx1;
   int			type_idx2;
#ifdef KEY /* Bug 10177 */
   int			type_idx3 = 0;
#else /* KEY Bug 10177 */
   int			type_idx3;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "interpret_transfer_intrinsic", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (count) {

      save.bits_in_constructor = bits_in_constructor;

      list_idx1 = IR_IDX_R(ir_idx);
      list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
      list_idx3 = IL_NEXT_LIST_IDX(list_idx2);

      COPY_OPND(opnd, IL_OPND(list_idx2));
      loc_element_l = 0;
      ok = interpret_constructor(&opnd,
                                 &exp_desc_l,
                                 TRUE,
                                 &loc_element_l);
      bits_in_constructor = 0;
      exp_desc_l.rank = 0;
      increment_count(&exp_desc_l);
      k = bits_in_constructor;

      longest_char_len = char_result_len;

      if (IL_FLD(list_idx3) != NO_Tbl_Idx) {

         COPY_OPND(opnd, IL_OPND(list_idx3));
         loc_element_l = 0;
         ok = interpret_constructor(&opnd,
                                    &exp_desc_l,
                                    FALSE,
                                    &loc_element_l);

         exp_desc->rank = 1;
         exp_desc->shape[0].fld = CN_Tbl_Idx;
         exp_desc->shape[0].idx = ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                                                FALSE,
                                                result_value);
      }
      else if (arg_info_list[IL_ARG_DESC_IDX(list_idx2)].ed.rank) {

         bits_in_constructor = 0;
         COPY_OPND(opnd, IL_OPND(list_idx1));
         loc_element_l = 0;
         ok = interpret_constructor(&opnd,
                                    &exp_desc_l,
                                    TRUE,
                                    &loc_element_l);

         if (exp_desc_l.constant) {
           increment_count(&exp_desc_l);
         }

         extent = bits_in_constructor/k;

         if (bits_in_constructor%k != 0) {
            extent++;
         }

         exp_desc->rank = 1;
         exp_desc->shape[0].fld = CN_Tbl_Idx;
         exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              extent);
      }
      else {
         exp_desc->rank = 0;
      }

      char_result_len = longest_char_len;
      bits_in_constructor = save.bits_in_constructor;
   }
   else if (*element <= 1) {

      SAVE_ENV;
      check_type_conversion = FALSE;

      init_target_opnd = null_opnd;
      do_constructor_init = FALSE;

      list_idx1 = IR_IDX_R(ir_idx);
      list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
      list_idx3 = IL_NEXT_LIST_IDX(list_idx2);

      COPY_OPND(opnd, IL_OPND(list_idx1));

      gen_internal_dope_vector(&dope_1,
                               &opnd,
                               FALSE,
                              &arg_info_list[IL_ARG_DESC_IDX(list_idx1)].ed);

      type_idx1 = arg_info_list[IL_ARG_DESC_IDX(list_idx1)].ed.type_idx;

      COPY_OPND(opnd, IL_OPND(list_idx2));

      gen_internal_dope_vector(&dope_2,
                               &opnd,
                               FALSE,
                              &arg_info_list[IL_ARG_DESC_IDX(list_idx2)].ed);

      type_idx2 = arg_info_list[IL_ARG_DESC_IDX(list_idx2)].ed.type_idx;

      if (IL_FLD(list_idx3) != NO_Tbl_Idx) {
         COPY_OPND(opnd, IL_OPND(list_idx3));

         loc_element_l = 0;
         ok = interpret_constructor(&opnd,
                &arg_info_list[IL_ARG_DESC_IDX(list_idx3)].ed,
                                         FALSE,
                                         &loc_element_l);

         type_idx3 = arg_info_list[IL_ARG_DESC_IDX(list_idx3)].ed.type_idx;
      }

      gen_internal_dope_vector(&dope_result,
                               &opnd,
                               TRUE,
                              &arg_info_list[IL_ARG_DESC_IDX(list_idx2)].ed);

      type_idx = exp_desc->type_idx;

      if (IL_FLD(list_idx3) == NO_Tbl_Idx) {
         ok &= folder_driver((char *)&dope_1,
                             type_idx1,
                             (char *)&dope_2,
                             type_idx2,
                             (long_type *)&dope_result,
                             &type_idx,
                             line,
                             col,
                             3,
                             Transfer_Opr,
                             0,
                             0);
      }
      else {
         ok &= folder_driver((char *)&dope_1,
                             type_idx1,
                             (char *)&dope_2,
                             type_idx2,
                             (long_type *)&dope_result,
                             &type_idx,
                             line,
                             col,
                             3,
                             Transfer_Opr,
                             result_value,
                             type_idx3);
      }

      k = 1;
      for (i = 1; i <= dope_result.num_dims; i++) {
         k = k * dope_result.dim[i-1].extent;
         exp_desc->shape[i-1].fld = CN_Tbl_Idx;
         extent = dope_result.dim[i-1].extent;
         exp_desc->shape[i-1].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                extent);
      }
      k = k * dope_result.el_len;

      if (char_len_in_bytes) {
         if (TYP_TYPE(IR_TYPE_IDX(ir_idx)) == Character) {
            /* el_len was in bytes, so change to bits */
            k *= CHAR_BIT;
         }
      }

      exp_desc->rank = dope_result.num_dims;

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Typeless;
      TYP_LINEAR(TYP_WORK_IDX)  = Long_Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= k;
      type_idx			= ntr_type_tbl();

      /* BHJ - Warning message here */

      cn_idx = ntr_const_tbl(type_idx,
                             FALSE,
                             (long_type *)(dope_result.base_addr));

      tmp_idx                    = gen_compiler_tmp(line, col, Shared, TRUE);
      AT_SEMANTICS_DONE(tmp_idx) = TRUE;
      ATD_TYPE_IDX(tmp_idx)      = IR_TYPE_IDX(ir_idx);

      ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(exp_desc,
                                                        line, col);

      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
      ATD_FLD(tmp_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(tmp_idx) = cn_idx;

      OPND_IDX(opnd) = tmp_idx;
      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      if (exp_desc->rank) {
         ok = gen_whole_subscript(&opnd, exp_desc);
      }
      else if (exp_desc->type == Character) {
         ok = gen_whole_substring(&opnd,
                                  exp_desc->rank);
      }

      if (*element == 1) {
         NTR_IR_LIST_TBL(list_idx);
         IL_NEXT_LIST_IDX(list_idx) = IR_IDX_R(ir_idx);
         IR_IDX_R(ir_idx) = list_idx;
         (IR_LIST_CNT_R(ir_idx))++;
         COPY_OPND(IL_OPND(list_idx), opnd);
      }

      RESTORE_ENV;

      ok = interpret_constructor(&opnd,
                                 exp_desc,
                                 count,
                                 element);
   }
   else {

      COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));

      ok = interpret_constructor(&opnd,
                                 exp_desc,
                                 count,
                                 element);

      if (*element < 0) {
         list_idx = IR_IDX_R(ir_idx);
         IR_IDX_R(ir_idx) = IL_NEXT_LIST_IDX(list_idx);
         (IR_LIST_CNT_R(ir_idx))--;
         FREE_IR_LIST_NODE(list_idx);
      }
   }


   TRACE (Func_Exit, "interpret_transfer_intrinsic", NULL);

   return(ok);

}  /* interpret_transfer_intrinsic */

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

static boolean interpret_reshape_intrinsic(int                  ir_idx,
                                           expr_arg_type       *exp_desc,
                                           boolean              count,
                                           long64              *element)


{
   int                  cn_idx;
   int                  col;
   int_dope_type        dope_result;
   int_dope_type        dope_1;
   int_dope_type        dope_2;
   int_dope_type        dope_3;
   int_dope_type        dope_4;
   expr_arg_type        exp_desc_l;
   long64               extent;
   long64               i;
   long64               k;
   int                  line;
   int                  list_idx;
   long64               loc_element = 0;
   boolean              ok = TRUE;
   opnd_type            opnd;
   save_env_type        save;
   int                  tmp_idx;
   int			type_idx;
   int			type_idx1;
   int			type_idx2;
#ifdef KEY /* Bug 10177 */
   int			type_idx3 = 0;
   int			type_idx4 = 0;
#else /* KEY Bug 10177 */
   int			type_idx3;
   int			type_idx4;
#endif /* KEY Bug 10177 */



   TRACE (Func_Entry, "interpret_reshape_intrinsic", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (count) {

      COPY_OPND(opnd,
                IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))));

      loc_element = 1;
      exp_desc->rank = 0;
      while (loc_element > 0) {

         exp_desc->rank++;
         ok = interpret_constructor(&opnd, &exp_desc_l, FALSE,
                                    &loc_element);

         exp_desc->shape[exp_desc->rank-1].fld = CN_Tbl_Idx;
         exp_desc->shape[exp_desc->rank-1].idx =
                         ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                                       FALSE,
                                       result_value);
      }
   }
   else if (*element <= 1) {

      SAVE_ENV;
      check_type_conversion = FALSE;

      init_target_opnd = null_opnd;
      do_constructor_init = FALSE;

      list_idx = IR_IDX_R(ir_idx);
      COPY_OPND(opnd, IL_OPND(list_idx));

      exp_desc_l = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed;

      gen_internal_dope_vector(&dope_1,
                               &opnd,
                               FALSE,
                               &exp_desc_l);

      type_idx1 = exp_desc_l.type_idx;

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(opnd, IL_OPND(list_idx));

      gen_internal_dope_vector(&dope_2,
                               &opnd,
                               FALSE,
                               &arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed);

      type_idx2 = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type_idx;

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      i = 3;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         i += 4;
         COPY_OPND(opnd, IL_OPND(list_idx));

         gen_internal_dope_vector(&dope_3,
                                  &opnd,
                                  FALSE,
                                 &arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed);

         type_idx3 = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type_idx;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         i += 8;
         COPY_OPND(opnd, IL_OPND(list_idx));

         gen_internal_dope_vector(&dope_4,
                                  &opnd,
                                  FALSE,
                                 &arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed);

         type_idx4 = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type_idx;
      }

      gen_internal_dope_vector(&dope_result,
                               &opnd,
                               TRUE,
                               &exp_desc_l);

      type_idx = exp_desc->type_idx;

      if (i == 3) {
         ok &= folder_driver((char *)&dope_1,
                             type_idx1,
                             (char *)&dope_2,
                             type_idx2,
                             (long_type *)&dope_result,
                             &type_idx,
                             line,
                             col,
                             4,
                             Reshape_Opr,
                             0,
                             0,
                             0,
                             0);
      }
      else if (i == 7) {
         ok &= folder_driver((char *)&dope_1,
                             type_idx1,
                             (char *)&dope_2,
                             type_idx2,
                             (long_type *)&dope_result,
                             &type_idx,
                             line,
                             col,
                             4,
                             Reshape_Opr,
                             (char *)&dope_3,
                             type_idx3,
                             0,
                             0);
      }
      else if (i == 11) {
         ok &= folder_driver((char *)&dope_1,
                             type_idx1,
                             (char *)&dope_2,
                             type_idx2,
                             (long_type *)&dope_result,
                             &type_idx,
                             line,
                             col,
                             4,
                             Reshape_Opr,
                             0,
                             0,
                             (char *)&dope_4,
                             type_idx4);
      }
      else {
         ok &= folder_driver((char *)&dope_1,
                             type_idx1,
                             (char *)&dope_2,
                             type_idx2,
                             (long_type *)&dope_result,
                             &type_idx,
                             line,
                             col,
                             4,
                             Reshape_Opr,
                             (char *)&dope_3,
                             type_idx3,
                             (char *)&dope_4,
                             type_idx4);
      }

      k = 1;
      for (i = 1; i <= dope_result.num_dims; i++) {
         k = k * dope_result.dim[i-1].extent;
         exp_desc->shape[i-1].fld = CN_Tbl_Idx;
         extent = dope_result.dim[i-1].extent;
         exp_desc->shape[i-1].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,extent);
      }
      k = k * dope_result.el_len;

      if (char_len_in_bytes) {
         if (TYP_TYPE(IR_TYPE_IDX(ir_idx)) == Character) {
            /* el_len was in bytes, so change to bits */
            k *= CHAR_BIT;
         }
      }

      exp_desc->rank = dope_result.num_dims;

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Typeless;
      TYP_LINEAR(TYP_WORK_IDX)  = Long_Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= k;
      type_idx			= ntr_type_tbl();

       /* BHJ - Warning message here */

      cn_idx = ntr_const_tbl(type_idx,
                             FALSE,
                             (long_type *)(dope_result.base_addr));

      tmp_idx                    = gen_compiler_tmp(line, col, Shared, TRUE);
      AT_SEMANTICS_DONE(tmp_idx) = TRUE;
      ATD_TYPE_IDX(tmp_idx)      = IR_TYPE_IDX(ir_idx);

      ATD_ARRAY_IDX(tmp_idx) =
             create_bd_ntry_for_const(exp_desc,
                                      line, col);

      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
      ATD_FLD(tmp_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(tmp_idx) = cn_idx;

      OPND_IDX(opnd) = tmp_idx;
      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      if (exp_desc->rank) {
         ok = gen_whole_subscript(&opnd, exp_desc);
      }
      else if (exp_desc->type == Character) {
         ok = gen_whole_substring(&opnd,
                                  exp_desc->rank);
      }

      if (*element == 1) {
         NTR_IR_LIST_TBL(list_idx);
         IL_NEXT_LIST_IDX(list_idx) = IR_IDX_R(ir_idx);
         IR_IDX_R(ir_idx) = list_idx;
         (IR_LIST_CNT_R(ir_idx))++;
         COPY_OPND(IL_OPND(list_idx), opnd);
      }

      RESTORE_ENV;

      ok = interpret_constructor(&opnd,
                                 exp_desc,
                                 count,
                                 element);
   }
   else {

      COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));

      ok = interpret_constructor(&opnd,
                                 exp_desc,
                                 count,
                                 element);

      if (*element < 0) {
         list_idx = IR_IDX_R(ir_idx);
         IR_IDX_R(ir_idx) = IL_NEXT_LIST_IDX(list_idx);
         (IR_LIST_CNT_R(ir_idx))--;
         FREE_IR_LIST_NODE(list_idx);
      }
   }


   TRACE (Func_Exit, "interpret_reshape_intrinsic", NULL);

   return(ok);

}  /* interpret_reshape_intrinsic */

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

static boolean interpret_size_intrinsic(int                  ir_idx,
                                        expr_arg_type       *exp_desc,
                                        boolean              count,
                                        long64              *element)


{
   expr_arg_type        exp_desc_l;
   expr_arg_type        exp_desc_r;
   long64		extent;
   int			i;
   int			info_idx;
   int			list_idx1;
   long64               loc_element = 0;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			type_idx;


   TRACE (Func_Entry, "interpret_size_intrinsic", NULL);

   /* assume only here if DIM not specified */

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   list_idx1 = IR_IDX_R(ir_idx);
   info_idx = IL_ARG_DESC_IDX(list_idx1);


   if (count) {
      /* intentionally blank */
   }
   else {

      if (*element > 0) {
         *element = -1;
      }

      extent = 1;
      exp_desc_l = arg_info_list[info_idx].ed;

      for (i = 0; i < exp_desc_l.rank; i++) {
         COPY_OPND(opnd,
                   exp_desc_l.shape[i]);
         loc_element = 0;
         ok = interpret_constructor(&opnd, &exp_desc_r,
                                    FALSE,
                                    &loc_element) && ok;

         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         if (folder_driver((char *)result_value,
                           exp_desc_r.type_idx,
                           (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                           CG_INTEGER_DEFAULT_TYPE,
                           result_value,
                           &type_idx,
                           IR_LINE_NUM(ir_idx),
                           IR_COL_NUM(ir_idx),
                           2,
                           Le_Opr)) {

            if (THIS_IS_TRUE(result_value, type_idx)) {
               C_TO_F_INT(result_value, 0, exp_desc_r.linear_type);
            }
         }

         extent *= F_INT_TO_C(result_value, exp_desc_r.linear_type);
      }

      C_TO_F_INT(result_value, extent, Integer_8);
   }


   TRACE (Func_Exit, "interpret_size_intrinsic", NULL);

   return(ok);

}  /* interpret_size_intrinsic */

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

static boolean interpret_ubound_intrinsic(int                  ir_idx,
                                          expr_arg_type       *exp_desc,
                                          boolean              count,
                                          long64              *element)

{
   expr_arg_type        exp_desc_r;
   int                  i;
   int			info_idx;
   int			list_idx1;
   long64               loc_element = 0;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			type_idx;


   TRACE (Func_Entry, "interpret_ubound_intrinsic", NULL);

   /* assume only here if DIM not specified */

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   list_idx1 = IR_IDX_R(ir_idx);
   info_idx = IL_ARG_DESC_IDX(list_idx1);

   if (count) {
      exp_desc->rank = 1;
      exp_desc->shape[0].fld = CN_Tbl_Idx;
      exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                           arg_info_list[info_idx].ed.rank);
   }
   else if (*element == 0) {

      for (i = 0; i < arg_info_list[info_idx].ed.rank; i++) {
         COPY_OPND(opnd,
                   arg_info_list[info_idx].ed.shape[i]);
         loc_element = 0;
         ok = interpret_constructor(&opnd, &exp_desc_r,
                                    FALSE,
                                    &loc_element) && ok;

         type_idx = CG_LOGICAL_DEFAULT_TYPE;

         if (folder_driver((char *)result_value,
                           exp_desc_r.type_idx,
                           (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                           CG_INTEGER_DEFAULT_TYPE,
                           result_value,
                           &type_idx,
                           IR_LINE_NUM(ir_idx),
                           IR_COL_NUM(ir_idx),
                           2,
                           Le_Opr)) {

            if (THIS_IS_TRUE(result_value, type_idx)) {
               C_TO_F_INT(result_value, 0, exp_desc_r.linear_type);
            }
         }

         if (exp_desc_r.constant) {
            write_constant(exp_desc_r.type_idx);
         }
      }

      exp_desc->constant = FALSE;
   }
   else {
      COPY_OPND(opnd,
                arg_info_list[info_idx].ed.shape[*element-1]);
      loc_element = 0;
      ok = interpret_constructor(&opnd, &exp_desc_r,
                                 FALSE, &loc_element);

      if (*element == arg_info_list[info_idx].ed.rank) {
         *element = -1;
      }
      else {
         (*element)++;
      }
   }


   TRACE (Func_Exit, "interpret_ubound_intrinsic", NULL);

   return(ok);

}  /* interpret_ubound_intrinsic */

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

static boolean interpret_shape_intrinsic(int                  ir_idx,
                                         expr_arg_type       *exp_desc,
                                         boolean              count,
                                         long64              *element)

{
   expr_arg_type        exp_desc_r;
   int			i;
   int			info_idx;
   int			list_idx1;
   long64               loc_element = 0;
   boolean		ok = TRUE;
   opnd_type		opnd;


   TRACE (Func_Entry, "interpret_shape_intrinsic", NULL);

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   list_idx1 = IR_IDX_R(ir_idx);
   info_idx = IL_ARG_DESC_IDX(list_idx1);

   if (count) {
      exp_desc->rank = 1;
      exp_desc->shape[0].fld = CN_Tbl_Idx;
      exp_desc->shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                           arg_info_list[info_idx].ed.rank);
   }
   else if (*element == 0) {

      for (i = 0; i < arg_info_list[info_idx].ed.rank; i++) {
         COPY_OPND(opnd,
                   arg_info_list[info_idx].ed.shape[i]);
         loc_element = 0;
         ok = interpret_constructor(&opnd, &exp_desc_r,
                                    FALSE,
                                    &loc_element) && ok;

         if (exp_desc_r.constant) {
            write_constant(exp_desc_r.type_idx);
         }
      }

      exp_desc->constant = FALSE;
   }
   else {
      COPY_OPND(opnd,
             arg_info_list[info_idx].ed.shape[*element-1]);
      loc_element = 0;
      ok = interpret_constructor(&opnd, &exp_desc_r,
                                 FALSE, &loc_element);

      if (*element == arg_info_list[info_idx].ed.rank) {
         *element = -1;
      }
      else {
         (*element)++;
      }
   }

   TRACE (Func_Exit, "interpret_shape_intrinsic", NULL);

   return(ok);

}  /* interpret_shape_intrinsic */

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

static boolean interpret_sik_intrinsic(int                  ir_idx,
                                       expr_arg_type       *exp_desc,
                                       boolean              count,
                                       long64              *element)

{
   expr_arg_type        exp_desc_l;
   long64               loc_element = 0;
   boolean		ok = TRUE;
   opnd_type		opnd;
   long64		value;


   TRACE (Func_Entry, "interpret_sik_intrinsic", NULL);

   /* SELECTED_INT_KIND */

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (count) {
      /* intentionally blank */
   }
   else {

      COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));

      loc_element = 0;
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                 &loc_element);

      if (*element > 0) {
         *element = -1;
      }

      value = F_INT_TO_C(result_value, exp_desc_l.linear_type);

# ifdef _TARGET32

      if (value <= RANGE_INT2_F90) {
         value = 1;
      }
      else if (value <= RANGE_INT4_F90) {
         value = 4;
      }
      else {
         value = -1;
      }
# else
      if (value < RANGE_INT4_F90) {
         value = 1;
      }
      else if (value < RANGE_INT8_F90) {
         value = 8;
      }
      else {
         value = -1;
      }
# endif

      C_TO_F_INT(result_value, value, exp_desc->linear_type);
   }

   TRACE (Func_Exit, "interpret_sik_intrinsic", NULL);

   return(ok);

}  /* interpret_sik_intrinsic */

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

static boolean interpret_srk_intrinsic(int                  ir_idx,
                                       expr_arg_type       *exp_desc,
                                       boolean              count,
                                       long64              *element)

{
   expr_arg_type        exp_desc_l;
   expr_arg_type        exp_desc_r;
   int			i;
   int			list_idx;
   int			list_idx2;
   long64               loc_element = 0;
   long_type            loc_value_l[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_r[MAX_WORDS_FOR_NUMERIC];
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			type_idx;


   TRACE (Func_Entry, "interpret_srk_intrinsic", NULL);

   /* SELECTED_REAL_KIND */

   exp_desc->constant = TRUE;
   exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
   exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   if (count) {
      /* intentionally blank */
   }
   else {

      list_idx = IR_IDX_R(ir_idx);

      if (IL_IDX(list_idx) != NULL_IDX) {
         COPY_OPND(opnd, IL_OPND(list_idx));

         loc_element = 0;
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                    &loc_element);


         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            loc_value_l[i] = result_value[i];
         }
      }

      list_idx2 = IL_NEXT_LIST_IDX(list_idx);

      if (IL_IDX(list_idx2) != NULL_IDX) {
         COPY_OPND(opnd, IL_OPND(list_idx2));

         loc_element = 0;
         ok = interpret_constructor(&opnd, &exp_desc_r, count,
                                    &loc_element);


         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            loc_value_r[i] = result_value[i];
         }
      }

      if (*element > 0) {
         *element = -1;
      }

      type_idx = exp_desc->type_idx;

      if (IL_IDX(list_idx) != NULL_IDX &&
          IL_IDX(list_idx2) != NULL_IDX) {

         ok &= folder_driver((char *)loc_value_l,
                             exp_desc_l.type_idx,
                             (char *)loc_value_r,
                             exp_desc_r.type_idx,
                             result_value,
                            &type_idx,
                             IR_LINE_NUM(ir_idx),
                             IR_COL_NUM(ir_idx),
                             2,
                             SRK_Opr);
      }
      else if (IL_IDX(list_idx) != NULL_IDX) {

         ok &= folder_driver((char *)loc_value_l,
                             exp_desc_l.type_idx,
                             NULL,
                             NULL_IDX,
                             result_value,
                            &type_idx,
                             IR_LINE_NUM(ir_idx),
                             IR_COL_NUM(ir_idx),
                             2,
                             SRK_Opr);
      }
      else if (IL_IDX(list_idx2) != NULL_IDX) {

         ok &= folder_driver(NULL,
                             NULL_IDX,
                             (char *)loc_value_r,
                             exp_desc_r.type_idx,
                             result_value,
                            &type_idx,
                             IR_LINE_NUM(ir_idx),
                             IR_COL_NUM(ir_idx),
                             2,
                             SRK_Opr);
      }
   }


   TRACE (Func_Exit, "interpret_srk_intrinsic", NULL);

   return(ok);

}  /* interpret_srk_intrinsic */

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

static boolean interpret_unary_intrinsic_opr(int                  ir_idx,
                                             expr_arg_type       *exp_desc,
                                             boolean              count,
                                             long64              *element)

{
   long64               char_result_len_l;
   long64               char_result_offset_l;
   int			col;
   expr_arg_type        exp_desc_l;
   int			i;
   int			line;
   int			list_idx;
   long_type            loc_value_l[MAX_WORDS_FOR_NUMERIC];
   boolean		ok = TRUE;
   opnd_type		opnd;
   opnd_type		tmp_opnd;
   int			type_idx;


   TRACE (Func_Entry, "interpret_unary_intrinsic_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   /* I assume that the left opnd is still a list item    */

   if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
      list_idx = IR_IDX_L(ir_idx);
      COPY_OPND(opnd, IL_OPND(list_idx));
   }
   else {
      COPY_OPND(opnd, IR_OPND_L(ir_idx));
   }

   if (count) {

      if (IR_RANK(ir_idx) == 0) {

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         if (IR_OPR(ir_idx) == Adjustr_Opr ||
             IR_OPR(ir_idx) == Adjustl_Opr) {

            ok = interpret_constructor(&opnd, exp_desc, count,
                                       element);
         }

         exp_desc->constant = TRUE;

      }
      else {

         ok = interpret_constructor(&opnd, exp_desc, count,
                                    element);
         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      }

      if (IR_OPR(ir_idx) == Char_Opr) {
         char_result_len = 1;
      }
   }
   else {
      char_result_offset_l = char_result_offset;
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                       element);

      char_result_offset = char_result_offset_l;
      char_result_len_l = char_result_len;

      *(exp_desc) = exp_desc_l;

      exp_desc->constant = TRUE;

      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      if (! no_result_value) {

         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            loc_value_l[i] = result_value[i];
         }

         type_idx = exp_desc->type_idx;

         switch (IR_OPR(ir_idx)) {

         case Len_Trim_Opr :

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
            TYP_TYPE(TYP_WORK_IDX)	= Character;
            TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_l);
            exp_desc_l.type_idx		= ntr_type_tbl();

            type_idx = exp_desc->type_idx;

            ok = folder_driver(&(char_result_buffer[char_result_offset_l]),
                               exp_desc_l.type_idx,
                               NULL,
                               NULL_IDX,
                               result_value,
                               &type_idx,
                               line,
                               col,
                               1,
                               IR_OPR(ir_idx));

            exp_desc->type_idx = type_idx;
            exp_desc->linear_type = TYP_LINEAR(type_idx);


            break;


         case Ichar_Opr :  

            /* BRIANJ - Should use size of char_result_buffer for type */

            C_TO_F_INT(result_value,
                       char_result_buffer[char_result_offset_l],
                       exp_desc->linear_type);
            break;

         case Char_Opr :

            if (char_result_offset + 1 >= char_result_buffer_len) {

               enlarge_char_result_buffer();
            }

            char_result_buffer[char_result_offset] = 
                           F_INT_TO_C(result_value, exp_desc_l.linear_type);
            char_result_len = 1;
            break;

         case Adjustl_Opr :
         case Adjustr_Opr :


            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
            TYP_TYPE(TYP_WORK_IDX)	= Character;
            TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_l);
            type_idx			= ntr_type_tbl();

            exp_desc->type_idx = type_idx;

            ok = folder_driver(&(char_result_buffer[char_result_offset_l]),
                               type_idx,
                               NULL,
                               NULL_IDX,
                               loc_value_l,
                               &type_idx,
                               line,
                               col,
                               1,
                               IR_OPR(ir_idx));


            OPND_FLD(tmp_opnd) = CN_Tbl_Idx;
            OPND_IDX(tmp_opnd) = loc_value_l[0];  /* BRIANJ */
            OPND_LINE_NUM(tmp_opnd) = line;
            OPND_COL_NUM(tmp_opnd)  = col;

            char_result_offset = char_result_offset_l;

            ok = interpret_constructor(&tmp_opnd, exp_desc, FALSE,
                                       element);

            exp_desc->type_idx = type_idx;
            exp_desc->linear_type = TYP_LINEAR(type_idx);


            break;

         default :

            ok = folder_driver((char *)loc_value_l,
                               exp_desc_l.type_idx,
                               NULL,
                               NULL_IDX,
                               result_value,
                               &type_idx,
                               line,
                               col,
                               1,
                               IR_OPR(ir_idx));

            exp_desc->type_idx = type_idx;
            exp_desc->linear_type = TYP_LINEAR(type_idx);


            break;


         } /* switch (IR_OPR(ir_idx)) .. inner one */
      } /* if (! no_result_value) */
   }

   TRACE (Func_Exit, "interpret_unary_intrinsic_opr", NULL);

   return(ok);

}  /* interpret_unary_intrinsic_opr */

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

static boolean interpret_binary_intrinsic_opr(int                  ir_idx,
                                              expr_arg_type       *exp_desc,
                                              boolean              count,
                                              long64              *element)

{
   long64               char_result_len_l;
   long64               char_result_len_r;
   long64               char_result_offset_l;
   long64               char_result_offset_r;
   int			col;
   expr_arg_type        exp_desc_l;
   expr_arg_type        exp_desc_r;
   int			i;
   int			line;
   long64               loc_element_l = 0;
   long64               loc_element_r = 0;
   boolean              loc_no_result_value = FALSE;
   long_type            loc_value_l[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_r[MAX_WORDS_FOR_NUMERIC];
   boolean		ok = TRUE;
   opnd_type		opnd_l;
   opnd_type		opnd_r;
   int			type_idx;


   TRACE (Func_Entry, "interpret_binary_intrinsic_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {

      COPY_OPND(opnd_l, IL_OPND(IR_IDX_L(ir_idx)));
      COPY_OPND(opnd_r, IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx))));

   }
   else {
      COPY_OPND(opnd_l, IR_OPND_L(ir_idx));
      COPY_OPND(opnd_r, IR_OPND_R(ir_idx));
   }

   if (count) {
      if (IR_RANK(ir_idx) == 0) {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
      }
      else {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         loc_element_l = *element;
         ok = interpret_constructor(&opnd_l, &exp_desc_l, count,
                                    &loc_element_l);

         loc_element_l = *element;


         ok = interpret_constructor(&opnd_r, &exp_desc_r, count,
                                    &loc_element_l) && ok;


         /* check conformance. */

         if (exp_desc_r.rank == exp_desc_l.rank) {

            for (i = 0; i < exp_desc_r.rank; i++) {
               /* assumes that all extents are constant now */

               if (fold_relationals(OPND_IDX(exp_desc_l.shape[i]),
                                    OPND_IDX(exp_desc_r.shape[i]),
                                    Ne_Opr)) {

                  /* non conforming array syntax */
                  PRINTMSG(line, 252, Error, col);
                  ok = FALSE;
                  break;
               }
            }
            exp_desc->rank = exp_desc_r.rank;
            COPY_SHAPE(exp_desc->shape,exp_desc_r.shape,
                       exp_desc_r.rank);
         }
         else if (exp_desc_r.rank > exp_desc_l.rank) {
            exp_desc->rank = exp_desc_r.rank;
            COPY_SHAPE(exp_desc->shape,exp_desc_r.shape,
                       exp_desc_r.rank);
         }
         else {
            exp_desc->rank = exp_desc_l.rank;
            COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                       exp_desc_l.rank);
         }
      }
   }
   else {
      exp_desc->constant = TRUE;
      exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
      exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      loc_element_l = *element;
      char_result_offset_l = char_result_offset;

      ok = interpret_constructor(&opnd_l, &exp_desc_l, count,
                                 &loc_element_l);

      char_result_len_l = char_result_len;

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_l[i] = result_value[i];
      }

      char_result_offset = char_result_offset_l + char_result_len;
      char_result_offset_r = char_result_offset;
      loc_element_r = *element;
      ok = interpret_constructor(&opnd_r, &exp_desc_r, count,
                                 &loc_element_r) && ok;

      char_result_offset = char_result_offset_l;
      char_result_len_r = char_result_len;

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_r[i] = result_value[i];
      }

      *element = (loc_element_r > loc_element_l) ?
                  loc_element_r : loc_element_l;

      if (loc_no_result_value) {
         goto EXIT;
      }

      type_idx = exp_desc->type_idx;

      switch (IR_OPR(ir_idx)) {

         case Lge_Opr :
         case Lgt_Opr :
         case Lle_Opr :
         case Llt_Opr :

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
            TYP_TYPE(TYP_WORK_IDX)	= Character;
            TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_l);
            exp_desc_l.type_idx		= ntr_type_tbl();

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
            TYP_TYPE(TYP_WORK_IDX)	= Character;
            TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_r);
            exp_desc_r.type_idx		= ntr_type_tbl();

            ok = folder_driver(&(char_result_buffer[char_result_offset_l]),
                               exp_desc_l.type_idx,
                               &(char_result_buffer[char_result_offset_r]),
                               exp_desc_r.type_idx,
                               result_value,
                               &type_idx,
                               line,
                               col,
                               2,
                               IR_OPR(ir_idx));

            exp_desc->type_idx = type_idx;
            exp_desc->linear_type = TYP_LINEAR(type_idx);


            break;

         default :

            ok = folder_driver((char *)loc_value_l,
                               exp_desc_l.type_idx,
                               (char *)loc_value_r,
                               exp_desc_r.type_idx,
                               result_value,
                               &type_idx,
                               line,
                               col,
                               2,
                               IR_OPR(ir_idx));

            exp_desc->type_idx = type_idx;
            exp_desc->linear_type = TYP_LINEAR(type_idx);


            break;

      }
   }

EXIT:

   TRACE (Func_Exit, "interpret_binary_intrinsic_opr", NULL);

   return(ok);

}  /* interpret_binary_intrinsic_opr */

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

static boolean interpret_max_min_opr(int                  ir_idx,
                                     expr_arg_type       *exp_desc,
                                     boolean              count,
                                     long64              *element)

{
   int			col;
   expr_arg_type        exp_desc_l;
   expr_arg_type        exp_desc_r;
   int			i;
   int			line;
   int			list_idx;
   long64               loc_element_l = 0;
   long64               loc_element_r = 0;
   boolean              loc_no_result_value = FALSE;
   long_type            loc_value_l[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_r[MAX_WORDS_FOR_NUMERIC];
   int                  opr;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			type_idx;


   TRACE (Func_Entry, "interpret_max_min_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (count) {

      if (IR_RANK(ir_idx) == 0) {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      }
      else {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);


         list_idx = IR_IDX_L(ir_idx);

         loc_element_l = *element;
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                    &loc_element_l);

         exp_desc->rank = exp_desc_l.rank;
         COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                    exp_desc_l.rank);

         list_idx = IL_NEXT_LIST_IDX(list_idx);

         while (list_idx &&
                (IL_IDX(list_idx) != NULL_IDX) &&
                ok) {

            loc_element_l = *element;
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                       &loc_element_l) && ok;

            /* check conformance. */

            if (exp_desc->rank == exp_desc_l.rank) {

               for (i = 0; i < exp_desc->rank; i++) {
                  /* assumes that all extents are constant now */

                  if (fold_relationals(OPND_IDX(exp_desc_l.shape[i]),
                                       OPND_IDX(exp_desc->shape[i]),
                                       Ne_Opr)) {

                     /* non conforming array syntax */
                     PRINTMSG(line, 252, Error, col);
                     ok = FALSE;
                     break;
                  }
               }
            }
            else if (exp_desc->rank < exp_desc_l.rank) {
               exp_desc->rank = exp_desc_l.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                          exp_desc_l.rank);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
   }
   else {

      exp_desc->constant = TRUE;

      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      list_idx = IR_IDX_L(ir_idx);

      loc_element_l = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                 &loc_element_l);

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_l[i] = result_value[i];
      }

      if (IR_OPR(ir_idx) == Max_Opr) {
         opr = Gt_Opr;
      }
      else {
         opr = Lt_Opr;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      while (list_idx &&
             (IL_IDX(list_idx) != NULL_IDX)) {

         loc_element_r = *element;
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_r, count,
                                    &loc_element_r) && ok;

         if (no_result_value) {
            loc_no_result_value = TRUE;
         }

         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            loc_value_r[i] = result_value[i];
         }

         if (loc_element_r > loc_element_l) {
            loc_element_l = loc_element_r;
         }

         type_idx = exp_desc->type_idx;

         ok = folder_driver((char *)loc_value_r,
                            exp_desc_r.type_idx,
                            (char *)loc_value_l,
                            exp_desc_l.type_idx,
                            result_value,
                            &type_idx,
                            line,
                            col,
                            2,
                            opr) && ok;

         exp_desc->type_idx = type_idx;

         if (THIS_IS_TRUE(result_value, type_idx)) {

            for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
               loc_value_l[i] = loc_value_r[i];
            }
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      if (exp_desc->type != exp_desc_l.type) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)loc_value_l,
                           exp_desc_l.linear_type,
                           NULL,
                           NULL_IDX,
                           result_value,
                          &type_idx,
                           line,
                           col,
                           1,
                           Cvrt_Opr)) {
            /* intentionally blank */
         }
      }
      else {
         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            result_value[i] = loc_value_l[i];
         }
      }

      *element = loc_element_l;

      if (loc_no_result_value) {
         no_result_value = TRUE;
      }
   }


   TRACE (Func_Exit, "interpret_max_min_opr", NULL);

   return(ok);

}  /* interpret_max_min_opr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
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

static boolean interpret_csmg_opr(int                  ir_idx,
                                  expr_arg_type       *exp_desc,
                                  boolean              count,
                                  long64              *element)

{
   int                  col;
   expr_arg_type        exp_desc_x;
   expr_arg_type        exp_desc_y;
   expr_arg_type        exp_desc_z;
   int                  i;
   int                  line;
   int                  list_idx;
   long64               loc_element_x = 0;
   long64               loc_element_y = 0;
   long64               loc_element_z = 0;
   boolean              loc_no_result_value = FALSE;
   long_type            loc_value_x[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_y[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_z[MAX_WORDS_FOR_NUMERIC];
   boolean              ok = TRUE;
   opnd_type            opnd;
   int                  type_idx;


   TRACE (Func_Entry, "interpret_csmg_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (count) {

      if (IR_RANK(ir_idx) == 0) {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
      }
      else {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         list_idx = IR_IDX_L(ir_idx);

         loc_element_x = *element;
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_x, count,
                                    &loc_element_x);

         exp_desc->rank = exp_desc_x.rank;
         COPY_SHAPE(exp_desc->shape,exp_desc_x.shape,
                    exp_desc_x.rank);

         list_idx = IL_NEXT_LIST_IDX(list_idx);

         while (list_idx &&
                (IL_IDX(list_idx) != NULL_IDX) &&
                ok) {

            loc_element_x = *element;
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_x, count,
                                       &loc_element_x) && ok;

            /* check conformance. */

            if (exp_desc->rank == exp_desc_x.rank) {

               for (i = 0; i < exp_desc->rank; i++) {
                  /* assumes that all extents are constant now */

                  if (fold_relationals(OPND_IDX(exp_desc_x.shape[i]),
                                       OPND_IDX(exp_desc->shape[i]),
                                       Ne_Opr)) {

                     /* non conforming array syntax */
                     PRINTMSG(line, 252, Error, col);
                     ok = FALSE;
                     break;
                  }
               }
            }
            else if (exp_desc->rank < exp_desc_x.rank) {
               exp_desc->rank = exp_desc_x.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_x.shape,
                          exp_desc_x.rank);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
   }
   else {

      exp_desc->constant = TRUE;

      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      list_idx = IR_IDX_L(ir_idx);

      loc_element_x = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_x, count,
                                 &loc_element_x);

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_x[i] = result_value[i];
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      loc_element_y = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_y, count,
                                 &loc_element_y);

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_y[i] = result_value[i];
      }

      if (loc_element_y > loc_element_x) {
         loc_element_x = loc_element_y;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      loc_element_z = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_z, count,
                                 &loc_element_z);

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_z[i] = result_value[i];
      }

      if (loc_element_z > loc_element_x) {
         loc_element_x = loc_element_z;
      }

      type_idx = exp_desc->type_idx;

      ok = folder_driver((char *)loc_value_x,
                         exp_desc_x.type_idx,
                         (char *)loc_value_y,
                         exp_desc_y.type_idx,
                         result_value,
                         &type_idx,
                         line,
                         col,
                         3,
                         IR_OPR(ir_idx),
                         (char *)loc_value_z,
                         exp_desc_z.type_idx) && ok;


      *element = loc_element_x;

      if (loc_no_result_value) {
         no_result_value = TRUE;
      }
   }


   TRACE (Func_Exit, "interpret_csmg_opr", NULL);

   return(ok);

}  /* interpret_csmg_opr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
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

static boolean interpret_cvmgt_opr(int                  ir_idx,
                                   expr_arg_type       *exp_desc,
                                   boolean              count,
                                   long64              *element)

{
   int                  col;
   expr_arg_type        exp_desc_x;
   expr_arg_type        exp_desc_y;
   expr_arg_type        exp_desc_z;
   int                  i;
   int                  line;
   int                  list_idx;
   long64               loc_element_x = 0;
   long64               loc_element_y = 0;
   long64               loc_element_z = 0;
   boolean              loc_no_result_value = FALSE;
   long_type            loc_value_x[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_y[MAX_WORDS_FOR_NUMERIC];
   long_type            loc_value_z[MAX_WORDS_FOR_NUMERIC];
   boolean              ok = TRUE;
   opnd_type            opnd;


   TRACE (Func_Entry, "interpret_cvmgt_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (count) {

      if (IR_RANK(ir_idx) == 0) {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
      }
      else {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         list_idx = IR_IDX_L(ir_idx);

         loc_element_x = *element;
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_x, count,
                                    &loc_element_x);

         exp_desc->rank = exp_desc_x.rank;
         COPY_SHAPE(exp_desc->shape,exp_desc_x.shape,
                    exp_desc_x.rank);

         list_idx = IL_NEXT_LIST_IDX(list_idx);

         while (list_idx &&
                (IL_IDX(list_idx) != NULL_IDX) &&
                ok) {

            loc_element_x = *element;
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_x, count,
                                       &loc_element_x) && ok;

            /* check conformance. */

            if (exp_desc->rank == exp_desc_x.rank) {

               for (i = 0; i < exp_desc->rank; i++) {
                  /* assumes that all extents are constant now */

                  if (fold_relationals(OPND_IDX(exp_desc_x.shape[i]),
                                       OPND_IDX(exp_desc->shape[i]),
                                       Ne_Opr)) {

                     /* non conforming array syntax */
                     PRINTMSG(line, 252, Error, col);
                     ok = FALSE;
                     break;
                  }
               }
            }
            else if (exp_desc->rank < exp_desc_x.rank) {
               exp_desc->rank = exp_desc_x.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_x.shape,
                          exp_desc_x.rank);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
   }
   else {

      exp_desc->constant = TRUE;

      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      list_idx = IR_IDX_L(ir_idx);

      loc_element_x = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_x, count,
                                 &loc_element_x);

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_x[i] = result_value[i];
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      loc_element_y = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_y, count,
                                 &loc_element_y);

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_y[i] = result_value[i];
      }

      if (loc_element_y > loc_element_x) {
         loc_element_x = loc_element_y;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      loc_element_z = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_z, count,
                                 &loc_element_z);

      if (no_result_value) {
         loc_no_result_value = TRUE;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_z[i] = result_value[i];
      }

      if (loc_element_z > loc_element_x) {
         loc_element_x = loc_element_z;
      }

      if (THIS_IS_TRUE(loc_value_z, exp_desc_z.type_idx)) {
         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            result_value[i] = loc_value_x[i];
         }
         *exp_desc = exp_desc_x;
      }
      else {
         for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
            result_value[i] = loc_value_y[i];
         }
         *exp_desc = exp_desc_y;
      }

      *element = loc_element_x;

      if (loc_no_result_value) {
         no_result_value = TRUE;
      }
   }


   TRACE (Func_Exit, "interpret_cvmgt_opr", NULL);

   return(ok);

}  /* interpret_cvmgt_opr */

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

static boolean interpret_index_opr(int                  ir_idx,
                                   expr_arg_type       *exp_desc,
                                   boolean              count,
                                   long64              *element)

{
   long64               char_result_len_l;
   long64               char_result_len_r;
   long64               char_result_offset_l;
   long64               char_result_offset_r;
   int			col;
   expr_arg_type        exp_desc_l;
   expr_arg_type        exp_desc_r;
   int			i;
   int			line;
   int			list_idx;
   long64               loc_element_l = 0;
   long64               loc_element_r = 0;
   boolean              loc_no_result_value = FALSE;
   long_type            loc_value_r[MAX_WORDS_FOR_NUMERIC];
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			type_idx;


   TRACE (Func_Entry, "interpret_index_opr", NULL);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (count) {
      if (IR_RANK(ir_idx) == 0) {

         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
      }
      else {


         exp_desc->constant = TRUE;

         exp_desc->type_idx = IR_TYPE_IDX(ir_idx);
         exp_desc->type     = TYP_TYPE(exp_desc->type_idx);

         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

         list_idx = IR_IDX_L(ir_idx);

         loc_element_l = *element;
         COPY_OPND(opnd, IL_OPND(list_idx));
         ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                    &loc_element_l);

         exp_desc->rank = exp_desc_l.rank;
         COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                    exp_desc_l.rank);

         list_idx = IL_NEXT_LIST_IDX(list_idx);

         while (list_idx &&
                (IL_IDX(list_idx) != NULL_IDX) &&
                ok) {

            loc_element_l = *element;
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                       &loc_element_l) && ok;

            /* check conformance. */

            if (exp_desc->rank == exp_desc_l.rank) {

               for (i = 0; i < exp_desc->rank; i++) {
                  /* assumes that all extents are constant now */

                  if (fold_relationals(OPND_IDX(exp_desc_l.shape[i]),
                                       OPND_IDX(exp_desc->shape[i]),
                                       Ne_Opr)) {

                     /* non conforming array syntax */
                     PRINTMSG(line, 252, Error, col);
                     ok = FALSE;
                     break;
                  }
               }
            }
            else if (exp_desc->rank < exp_desc_l.rank) {
               exp_desc->rank = exp_desc_l.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                          exp_desc_l.rank);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
   }
   else {

      exp_desc->constant = TRUE;
      exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

      list_idx = IR_IDX_L(ir_idx);

      char_result_offset_l = char_result_offset;
      loc_element_l = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_l, count,
                                 &loc_element_l);

      char_result_len_l = char_result_len;

      if (no_result_value) {
         loc_no_result_value = TRUE;  /* BRIANJ - Set - but not used */
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      char_result_offset = char_result_offset_l + char_result_len;
      char_result_offset_r = char_result_offset;
      loc_element_r = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_r, count,
                                 &loc_element_r) && ok;

      char_result_offset = char_result_offset_l;
      char_result_len_r = char_result_len;

      if (no_result_value) {
         loc_no_result_value = TRUE;  /* BRIANJ - Set - but not used */
      }

      if (loc_element_r > loc_element_l) {
         loc_element_l = loc_element_r;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      loc_element_r = *element;
      COPY_OPND(opnd, IL_OPND(list_idx));
      ok = interpret_constructor(&opnd, &exp_desc_r, count,
                                 &loc_element_r) && ok;

      if (no_result_value) {
         loc_no_result_value = TRUE;  /* BRIANJ - Set - but not used */
      }

      if (loc_element_r > loc_element_l) {
         loc_element_l = loc_element_r;
      }

      for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
         loc_value_r[i] = result_value[i];
      }

#ifdef KEY /* Bug 4867 */
      /* Pass the result back via "element" (imitating the code in
       * functions interpret_binary_opr, interpret_concat_opr, etc.)
       */
      *element = loc_element_l;
#endif /* KEY Bug 4867 */

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)		= Character;
      TYP_LINEAR(TYP_WORK_IDX)		= CHARACTER_DEFAULT_TYPE;
      TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_l);
      exp_desc_l.type_idx		= ntr_type_tbl();

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)		= Character;
      TYP_LINEAR(TYP_WORK_IDX)		= CHARACTER_DEFAULT_TYPE;
      TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      char_result_len_r);
      exp_desc_r.type_idx		= ntr_type_tbl();

      type_idx = exp_desc->type_idx;

      ok = folder_driver(&(char_result_buffer[char_result_offset_l]),
                         exp_desc_l.type_idx,
                         &(char_result_buffer[char_result_offset_r]),
                         exp_desc_r.type_idx,
                         result_value,
                         &type_idx,
                         line,
                         col,
                         3,
                         IR_OPR(ir_idx),
                         (char *)loc_value_r,
                         LOGICAL_DEFAULT_TYPE);

      exp_desc->type_idx = type_idx;
      exp_desc->linear_type = TYP_LINEAR(type_idx);
   }


   TRACE (Func_Exit, "interpret_index_opr", NULL);

   return(ok);

}  /* interpret_index_opr */
