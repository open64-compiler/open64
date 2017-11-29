/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2007, 2008. PathScale, LLC.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_asg_expr.c	5.10	10/26/99 17:20:56\n";

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

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"

# include "s_asg_expr.h"

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
# include <fortran.h>
# endif

boolean	has_present_opr;

/*****************************************************************\
|* Function prototypes of static functions declared in this file. |
\*****************************************************************/

static boolean  array_construct_semantics(opnd_type *, expr_arg_type *);
static boolean  bin_array_syntax_check(expr_arg_type *, expr_arg_type *,
				       expr_arg_type *, int, int);

static void 	make_logical_array_tmp(opnd_type *, expr_arg_type *);
static void     fold_nested_substrings(int);
static boolean  uplus_opr_handler(opnd_type *, expr_arg_type *);
static boolean  power_opr_handler(opnd_type *, expr_arg_type *);
static boolean  mult_opr_handler(opnd_type *, expr_arg_type *);
static boolean  minus_opr_handler(opnd_type *, expr_arg_type *);
static boolean  plus_opr_handler(opnd_type *, expr_arg_type *);
static boolean  concat_opr_handler(opnd_type *, expr_arg_type *);
static boolean  eq_opr_handler(opnd_type *, expr_arg_type *);
static boolean  lg_opr_handler(opnd_type *, expr_arg_type *);
static boolean  lt_opr_handler(opnd_type *, expr_arg_type *);
static boolean  not_opr_handler(opnd_type *, expr_arg_type *);
static boolean  and_opr_handler(opnd_type *, expr_arg_type *);
static boolean  defined_un_opr_handler(opnd_type *, expr_arg_type *);
static boolean  defined_bin_opr_handler(opnd_type *, expr_arg_type *);
static boolean  max_opr_handler(opnd_type *, expr_arg_type *);
static boolean  struct_opr_handler(opnd_type *, expr_arg_type *, int);
static boolean  struct_construct_opr_handler(opnd_type *, expr_arg_type *);
static boolean  array_construct_opr_handler(opnd_type *, expr_arg_type *);
static boolean  subscript_opr_handler(opnd_type *, expr_arg_type *, int);
static boolean  substring_opr_handler(opnd_type *, expr_arg_type *, int);
static boolean  triplet_opr_handler(opnd_type *, expr_arg_type *);
static boolean  dealloc_obj_opr_handler(opnd_type *, expr_arg_type *, int);
static boolean  alloc_obj_opr_handler(opnd_type *, expr_arg_type *, int);
static boolean  cvrt_opr_handler(opnd_type *, expr_arg_type *);
static boolean  paren_opr_handler(opnd_type *, expr_arg_type *);
static boolean  stmt_func_call_opr_handler(opnd_type *, expr_arg_type *);
static int	implied_do_depth(opnd_type *);
static long64	outer_imp_do_count(opnd_type *);
static void	lower_ptr_asg(expr_arg_type *);
# if defined(_F_MINUS_MINUS)
static void	translate_distant_ref1(opnd_type *, expr_arg_type *, int);

# if defined(_TARGET_OS_MAX)
static void	translate_t3e_distant_ref(opnd_type *, expr_arg_type *, int);
static void	translate_t3e_dv_component(opnd_type *, expr_arg_type *);
static int      capture_bounds_from_dv(int, int, int);
# endif

static void	translate_distant_dv_ref(opnd_type *, expr_arg_type *, int);
static void	translate_distant_ref2(opnd_type *, expr_arg_type *, int);
static int	set_up_pe_offset_attr(void);
static void	gen_bias_ref(opnd_type *);
static void	linearize_pe_dims(int, int, int, int, opnd_type *);
# endif
#ifdef KEY /* Bug 934 */
static boolean expr_sem_d(opnd_type *result_opnd, expr_arg_type *exp_desc,
  boolean derived_assign);
static boolean expr_semantics_d (opnd_type *result_opnd,
  expr_arg_type *exp_desc, boolean derived_assign);
#endif /* KEY Bug 934 */


# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
# pragma inline uplus_opr_handler
# pragma inline power_opr_handler
# pragma inline mult_opr_handler
# pragma inline minus_opr_handler
# pragma inline plus_opr_handler
# pragma inline concat_opr_handler
# pragma inline eq_opr_handler
# pragma inline lg_opr_handler
# pragma inline lt_opr_handler
# pragma inline not_opr_handler
# pragma inline and_opr_handler
# pragma inline defined_un_opr_handler
# pragma inline defined_bin_opr_handler
# pragma inline max_opr_handler
# pragma inline struct_opr_handler
# pragma inline struct_construct_opr_handler
# pragma inline array_construct_opr_handler
# pragma inline subscript_opr_handler
# pragma inline substring_opr_handler
# pragma inline triplet_opr_handler
# pragma inline dealloc_obj_opr_handler
# pragma inline alloc_obj_opr_handler
# pragma inline cvrt_opr_handler
# pragma inline paren_opr_handler
# pragma inline stmt_func_call_opr_handler
# else
# pragma _CRI inline uplus_opr_handler
# pragma _CRI inline power_opr_handler
# pragma _CRI inline mult_opr_handler
# pragma _CRI inline minus_opr_handler
# pragma _CRI inline plus_opr_handler
# pragma _CRI inline concat_opr_handler
# pragma _CRI inline eq_opr_handler
# pragma _CRI inline lg_opr_handler
# pragma _CRI inline lt_opr_handler
# pragma _CRI inline not_opr_handler
# pragma _CRI inline and_opr_handler
# pragma _CRI inline defined_un_opr_handler
# pragma _CRI inline defined_bin_opr_handler
# pragma _CRI inline max_opr_handler
# pragma _CRI inline struct_opr_handler
# pragma _CRI inline struct_construct_opr_handler
# pragma _CRI inline array_construct_opr_handler
# pragma _CRI inline subscript_opr_handler
# pragma _CRI inline substring_opr_handler
# pragma _CRI inline triplet_opr_handler
# pragma _CRI inline dealloc_obj_opr_handler
# pragma _CRI inline alloc_obj_opr_handler
# pragma _CRI inline cvrt_opr_handler
# pragma _CRI inline paren_opr_handler
# pragma _CRI inline stmt_func_call_opr_handler
# endif


#ifdef KEY /* Bug 4810 */
/*
 * Return true if op0 is the same node as op1
 */
static boolean opnd_matches(opnd_type *op0, opnd_type *op1) {
  return op0->fld == op1->fld && op0->idx == op1->idx;
  }

/*
 * An assignment statement following an OMP "atomic" statement is supposed to
 * have one of these forms, with any operators inside "expr" having precedence
 * greater than or equal to that of "op":
 *
 *   lhs = function_call(...)
 *   lhs = expr op lhs
 *   lhs = lhs op expr
 *
 * But in a case like this:
 *
 *   lhs = lhs + expr_b + expr_c
 *
 * the parser generates a left-associative expression:
 *
 *   lhs = (lhs + expr_b) + expr_c
 *
 * and the OMP lowering code cannot find the nested reference to "lhs". We use
 * the Fortran associativity rules to change this to:
 *
 *   lhs = lhs + (expr_b + expr_c)
 *
 * While OMP allows non-commutative operators "-" and "/", it also requires
 * that "lhs op expr" be mathematically equivalent to "lhs op (expr)". That
 * means "lhs + expr_b - expr_c" must work (so we can't just swap "lhs" with
 * "expr_c") but "lhs - expr_b - expr_c" is a user error (which we want to
 * leave alone, so the lowerer can issue a diagnostic.) Thus the operator
 * adjacent to "lhs" must be one of the commutative ones.
 */
static void unbury_lhs_for_omp() {
  /* Previous statement wasn't OMP "atomic" */
  if (curr_stmt_sh_idx == SCP_FIRST_SH_IDX(curr_scp_idx) ||
    IR_OPR(SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))) != Atomic_Open_Mp_Opr) {
    return;
  }
  int stmt_idx = SH_IR_IDX(curr_stmt_sh_idx);
  int rhs_idx = IR_IDX_R(stmt_idx);
  /* RHS isn't an operator node */
  if (IR_FLD_R(stmt_idx) != IR_Tbl_Idx) {
    return;
  }
  operator_type operator = IR_OPR(rhs_idx);
  /* RHS is not an OMP-approved operator */
  if (operator != Plus_Opr && operator != Minus_Opr &&
    operator != Mult_Opr && operator != Div_Opr &&
    operator != And_Opr && operator != Or_Opr &&
    operator != Eqv_Opr && operator != Neqv_Opr) {
    return;
    }
  opnd_type *lhs_opnd = &(IR_OPND_L(stmt_idx));
  /* Top left operand already matches LHS, so no need to swap */
  if (opnd_matches(lhs_opnd, &(IR_OPND_L(rhs_idx)))) {
    return;
  }
  /* Top right operand already matches LHS, so no need to swap */
  if (opnd_matches(lhs_opnd, &(IR_OPND_R(rhs_idx)))) {
    return;
  }

  operator_type alt_operator = operator;
  if (operator == Plus_Opr) {
    alt_operator = Minus_Opr;
    }
  else if (operator == Minus_Opr) {
    alt_operator = Plus_Opr;
    }
  else if (operator == Mult_Opr) {
    alt_operator = Div_Opr;
    }
  else if (operator == Div_Opr) {
    alt_operator = Mult_Opr;
    }
  else if (operator == Eqv_Opr) {
    alt_operator == Neqv_Opr;
    }
  else if (operator == Neqv_Opr) {
    alt_operator == Eqv_Opr;
    }

  int parent_idx = rhs_idx;
  for (int node_idx = IR_IDX_L(parent_idx);
    IR_Tbl_Idx == IR_FLD_L(parent_idx) &&
      (operator == IR_OPR(node_idx) || alt_operator == IR_OPR(node_idx));
    parent_idx = node_idx, node_idx = IR_IDX_L(parent_idx)) {
    /* "-" or "/" can't be the operator we're unburying */
    if (opnd_matches(lhs_opnd, &(IR_OPND_L(node_idx))) &&
      (IR_OPR(node_idx) != Minus_Opr) && (IR_OPR(node_idx) != Div_Opr)) {
      opnd_type save_rhs = IR_OPND_R(stmt_idx);
      opnd_type save_right_opnd = IR_OPND_R(node_idx);
      IR_OPND_R(stmt_idx) = IR_OPND_L(parent_idx);
      IR_OPND_R(node_idx) = save_rhs;
      IR_OPND_L(parent_idx) = save_right_opnd;
      break;
    }
  }
}

#endif /* KEY Bug 4810 */
#ifdef KEY /* Bug 6845 */
static void help_assign_cpnts(int, int, Uint, int, fld_type, int, fld_type);

static void
help_assign_array_of_structure(int line, int col, int idx_l, fld_type fld_l,
  int idx_r, fld_type fld_r) {
  opnd_type opnd_l, opnd_r;
  expr_arg_type exp_desc_l, exp_desc_r;
  int next_sh_idx = NULL_IDX;
  int placeholder_sh_idx = pre_gen_loops(line, col, &next_sh_idx);
  OPND_FLD(opnd_l) = fld_l;
  OPND_IDX(opnd_l) = idx_l;
  OPND_FLD(opnd_r) = fld_r;
  OPND_IDX(opnd_r) = idx_r;
  OPND_LINE_NUM(opnd_l) = OPND_LINE_NUM(opnd_r) = line;
  OPND_COL_NUM(opnd_l) = OPND_COL_NUM(opnd_r) = col;
  gen_loops(&opnd_l, &opnd_r, TRUE);
  help_assign_cpnts(line, col, IR_TYPE_IDX(OPND_IDX(opnd_l)), OPND_IDX(opnd_l),
    OPND_FLD(opnd_l), OPND_IDX(opnd_r), OPND_FLD(opnd_r));
  post_gen_loops(placeholder_sh_idx, next_sh_idx);
}

/*
 * Given an assignment of a structure type known to have one or more
 * allocatable components, replace the assignment with a series of assignments
 * of the individual components, using a runtime system call to handle the
 * allocatable components, since they require automatic reallocation in
 * addition to the copying of data.
 *
 * line		Source line
 * col		Source column
 * type_idx	Type of lvalue (or rvalue) of assignment
 * lvalue_idx	Index of lvalue
 * lvalue_fld	IR_Tbl_Idx or AT_Tbl_Idx of lvalue
 * rvalue_idx	Index of rvalue
 * lvalue_fld	IR_Tbl_Idx or AT_Tbl_Idx of rvalue
 */
static void
help_assign_cpnts(int line, int col, Uint type_idx,
  int lvalue_idx, fld_type lvalue_fld, int rvalue_idx, fld_type rvalue_fld) {

  for (int sn_idx = ATT_FIRST_CPNT_IDX(TYP_IDX(type_idx));
    sn_idx != NULL_IDX;
    sn_idx = SN_SIBLING_LINK(sn_idx)) {
    boolean need_semantics = FALSE;
    int cpnt_attr_idx = SN_ATTR_IDX(sn_idx);
    int l_idx = do_make_struct_opr(line, col, lvalue_idx, lvalue_fld,
      cpnt_attr_idx);

    int r_idx = do_make_struct_opr(line, col, rvalue_idx, rvalue_fld,
      cpnt_attr_idx);
    int new_ir_idx = NULL_IDX;

    /* Allocatable array: generate runtime system call */
    if (ATD_ALLOCATABLE(cpnt_attr_idx)) {
      new_ir_idx = build_call(Assign_Allocatable_Idx, ASSIGN_ALLOCATABLE_ENTRY,
	gen_il(4, TRUE, line, col,
	  IR_Tbl_Idx, pass_by_ref(IR_Tbl_Idx, l_idx, line, col),
	  IR_Tbl_Idx, pass_by_ref(IR_Tbl_Idx, r_idx, line, col),
	  CN_Tbl_Idx, CN_INTEGER_ONE_IDX,
	  CN_Tbl_Idx, CN_INTEGER_ZERO_IDX),
	line, col);
    }

    else if (allocatable_structure_component(cpnt_attr_idx)) {

      /* Non-allocatable array whose element type is a structure having
       * allocatable components or subcomponents: no dope vector, so we can't
       * use ASSIGN_ALLOCATABLE_ARRAY: instead loop, assigning elements */
      if (ATD_ARRAY_IDX(cpnt_attr_idx) != NULL_IDX) {
        help_assign_array_of_structure(line, col, l_idx, IR_Tbl_Idx, r_idx,
	  IR_Tbl_Idx);
      }

      /* Scalar structure having allocatable components or subcomponents:
       * recursively assign them. */
      else {
	help_assign_cpnts(line, col,
	  ATD_TYPE_IDX(cpnt_attr_idx), l_idx, IR_Tbl_Idx, r_idx, IR_Tbl_Idx);
      }
    }

    /* Generate ordinary component assignment */
    else {
      new_ir_idx = gen_ir(IR_Tbl_Idx, l_idx, Asg_Opr,
        ATD_TYPE_IDX(cpnt_attr_idx), line, col, IR_Tbl_Idx, r_idx);
      /* Array, character assignments need expansion by semantics processing */
      need_semantics = (ATD_ARRAY_IDX(cpnt_attr_idx) != NULL_IDX ||
        TYP_TYPE(ATD_TYPE_IDX(cpnt_attr_idx)) == Character);
    }

    if (new_ir_idx != NULL_IDX) {
      gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = new_ir_idx;
      if (need_semantics) {
	assignment_stmt_semantics();
      }
      else {
	SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
      }
    }
  }
}

#endif /* KEY Bug 6845 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Top semantics routine for assignment and pointer assignment.          *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void assignment_stmt_semantics (void)

{
   int		     asg_idx;
   int               attr_idx;
   int               col;
   expr_arg_type     exp_desc_l;
   expr_arg_type     exp_desc_r;
   opnd_type	     forall_tmp_opnd;
   opnd_type	     forall_tmp_opnd_l;
   boolean	     forall_dependence;
   expr_arg_type     forall_exp_desc;
   int		     i;
   int               ir_idx;
   int               idx;
   char              l_err_word[40];
   opnd_type         l_opnd;
   int               line;
   int		     list_idx;
   int		     label_idx;
   boolean           ok 		= TRUE;
   opnd_type	     opnd;
   int		     opnd_col;
   int		     opnd_line;
   char              r_err_word[40];
   opnd_type         r_opnd;
   linear_type_type  result_type;
   int		     save_curr_stmt_sh_idx;
   int		     save_where_ir_idx;


   TRACE (Func_Entry, "assignment_stmt_semantics", NULL);

#ifdef KEY /* Bug 4810 */
   unbury_lhs_for_omp();
#endif /* KEY Bug 4810 */

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (IR_OPR(ir_idx) == Asg_Opr) {


      /* clear the where_ir_idx so that intrinsics on left hand */
      /* side (in subscripts) are handled without mask.          */

      save_where_ir_idx = where_ir_idx;
      where_ir_idx = NULL_IDX;

      if (active_forall_sh_idx) {
        defer_stmt_expansion = TRUE;
      }

      xref_state = CIF_Symbol_Modification;
      COPY_OPND(l_opnd, IR_OPND_L(ir_idx));
      exp_desc_l.rank = 0;
      ok = expr_semantics(&l_opnd, &exp_desc_l);
      COPY_OPND(IR_OPND_L(ir_idx), l_opnd);

      where_ir_idx = save_where_ir_idx;

      if (IR_FLD_R(ir_idx) == IR_Tbl_Idx &&
          IR_OPR(IR_IDX_R(ir_idx)) == Call_Opr &&
          AT_IS_INTRIN(IR_IDX_L(IR_IDX_R(ir_idx))) &&
          (strcmp(AT_OBJ_NAME_PTR(IR_IDX_L(IR_IDX_R(ir_idx))), "NULL") == 0)) {
         ok = FALSE;
         PRINTMSG(IR_LINE_NUM_R(ir_idx), 1557, Error, IR_COL_NUM_R(ir_idx));
      }

      if (! ok) {
         /* intentionally blank */
      }
      else if (exp_desc_l.constant) {
         ok = FALSE;

         if (OPND_FLD(l_opnd) == AT_Tbl_Idx &&
            AT_OBJ_CLASS(OPND_IDX(l_opnd)) == Data_Obj &&
            ATD_SYMBOLIC_CONSTANT(OPND_IDX(l_opnd))) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 1632, Error, IR_COL_NUM(ir_idx),
                     AT_OBJ_NAME_PTR(OPND_IDX(l_opnd)));
         }
         else {
            PRINTMSG(IR_LINE_NUM(ir_idx), 326, Error, IR_COL_NUM(ir_idx));
         }
      }
      else if (SH_COMPILER_GEN(curr_stmt_sh_idx)) {
         /* intentionally empty, to prevent the following clauses */

      }
      else if (! check_for_legal_define(&l_opnd)) {
         ok = FALSE;
      }

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE,
                           (exp_desc_l.rank == 0) ?
                              CIF_Assignment_Stmt : CIF_Array_Assignment_Stmt,
                           statement_number);
      } 

      xref_state = CIF_Symbol_Reference;
      COPY_OPND(r_opnd, IR_OPND_R(ir_idx));
      exp_desc_r.rank = 0;
#ifdef KEY /* Bug 934 */
      ok &= expr_semantics_d(&r_opnd, &exp_desc_r,
        (exp_desc_l.type == Structure));
#else /* KEY Bug 934 */
      ok &= expr_semantics(&r_opnd, &exp_desc_r);
#endif /* KEY Bug 934 */
      COPY_OPND(IR_OPND_R(ir_idx), r_opnd);

      if (! ok) {
         goto EXIT;
      }

      OPND_FLD(r_opnd) = IR_Tbl_Idx;
      OPND_IDX(r_opnd) = ir_idx;

      if (exp_desc_l.rank == exp_desc_r.rank) {
         for (i = 0; i < exp_desc_r.rank; i++) {
            if (OPND_FLD(exp_desc_l.shape[i]) == CN_Tbl_Idx &&
                OPND_FLD(exp_desc_r.shape[i]) == CN_Tbl_Idx &&
                fold_relationals(OPND_IDX(exp_desc_l.shape[i]),
                                 OPND_IDX(exp_desc_r.shape[i]),
                                 Ne_Opr)) {

               /* non conforming array syntax */
               PRINTMSG(IR_LINE_NUM(ir_idx), 253, Error,
                        IR_COL_NUM(ir_idx));
               ok = FALSE;
               break;
            }
         }
      }

      result_type = ASG_TYPE(exp_desc_l.linear_type, exp_desc_r.linear_type);

# if defined(_EXTENDED_CRI_CHAR_POINTER)
      if (result_type == CRI_Ch_Ptr_8 &&
          exp_desc_r.linear_type != CRI_Ch_Ptr_8) {

         transform_cri_ch_ptr(&l_opnd);
         COPY_OPND(IR_OPND_L(ir_idx), l_opnd);
      }
# endif

      if (result_type != Err_Res                                        &&
          result_type != Structure_Type                                 &&
          (exp_desc_l.rank == exp_desc_r.rank || exp_desc_r.rank == 0)) {
   
         if (ASG_EXTN(exp_desc_l.linear_type, exp_desc_r.linear_type)) {
            /* check for defined asg */
   
            if (resolve_ext_opr(&r_opnd, FALSE, FALSE, FALSE,
                                &ok,
                                &exp_desc_l, &exp_desc_r)) {
   
               SH_IR_IDX(curr_stmt_sh_idx)    = OPND_IDX(r_opnd);
               SH_STMT_TYPE(curr_stmt_sh_idx) = Call_Stmt;
               goto CK_WHERE;
            }
            else if (exp_desc_r.type == Character ||
                     exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *) &IR_OPND_R(ir_idx),
                                         &opnd_line, 
                                         &opnd_col);

               if (exp_desc_r.type == Character) {

                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         exp_desc_l.type_idx,
                                                         opnd_line,
                                                         opnd_col);
               exp_desc_r.type_idx    = exp_desc_l.type_idx;
               exp_desc_r.type        = exp_desc_l.type;
               exp_desc_r.linear_type = exp_desc_l.linear_type;
            }
         }
      
         IR_RANK(ir_idx) = exp_desc_l.rank;

         IR_TYPE_IDX(ir_idx)	= exp_desc_l.type_idx;

      }
      else if (result_type == Structure_Type      &&
               (exp_desc_l.rank == exp_desc_r.rank || 
                exp_desc_r.rank == 0)              &&
         compare_derived_types(exp_desc_l.type_idx, exp_desc_r.type_idx)) {
   

         if (resolve_ext_opr(&r_opnd, FALSE, FALSE, FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {
            SH_IR_IDX(curr_stmt_sh_idx)    = OPND_IDX(r_opnd);
            SH_STMT_TYPE(curr_stmt_sh_idx) = Call_Stmt;
         }
         else {
            IR_RANK(ir_idx) = exp_desc_l.rank;

            IR_TYPE_IDX(ir_idx)	= exp_desc_l.type_idx;
         }
      }
      else if (resolve_ext_opr(&r_opnd, TRUE, FALSE, 
                               (result_type == Err_Res ||
                                (result_type == Structure_Type &&
                                 !compare_derived_types(exp_desc_l.type_idx, 
                                                        exp_desc_r.type_idx) )),
                               &ok,
                               &exp_desc_l, &exp_desc_r)) {

         SH_IR_IDX(curr_stmt_sh_idx)    = OPND_IDX(r_opnd);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Call_Stmt;
      }
      else {
         ok = FALSE;
      }

      if (ok &&
          SH_STMT_TYPE(curr_stmt_sh_idx) != Call_Stmt &&
          exp_desc_l.type == Integer &&
          exp_desc_r.type == Real) {
      
         COPY_OPND(r_opnd, IR_OPND_R(ir_idx));
         look_for_real_div(&r_opnd);
         COPY_OPND(IR_OPND_R(ir_idx), r_opnd);
      }

# ifdef _TRANSFORM_CHAR_SEQUENCE
      if (ok &&
          SH_STMT_TYPE(curr_stmt_sh_idx) != Call_Stmt &&
          exp_desc_l.type == Structure &&
          ATT_CHAR_SEQ(TYP_IDX(exp_desc_l.type_idx))) {

         /* change character sequence assignment to character assignment */

         COPY_OPND(l_opnd, IR_OPND_L(ir_idx));
         transform_char_sequence_ref(&l_opnd, exp_desc_l.type_idx);
         COPY_OPND(IR_OPND_L(ir_idx), l_opnd);

         COPY_OPND(r_opnd, IR_OPND_R(ir_idx));
         transform_char_sequence_ref(&r_opnd, exp_desc_r.type_idx);
         COPY_OPND(IR_OPND_R(ir_idx), r_opnd);
      }
# endif

CK_WHERE:

      if (ok &&
          where_ir_idx > 0)   {

         /* we are in a where block */
         
         if (SH_STMT_TYPE(curr_stmt_sh_idx) == Call_Stmt &&
             ! ATP_ELEMENTAL(IR_IDX_L(ir_idx))) {
            PRINTMSG(line, 1638, Error, col);
            ok = FALSE;
         }
         else if (! check_where_conformance(&exp_desc_l)) {

            find_opnd_line_and_column((opnd_type *) &IR_OPND_L(ir_idx),
                                      &opnd_line,
                                      &opnd_col);
            PRINTMSG(opnd_line, 195, Error, opnd_col);
            ok = FALSE;
         }
                  
         if (ok) {
            /* set up list */
            change_asg_to_where(ir_idx);
         }
      }


      if (active_forall_sh_idx) {
         defer_stmt_expansion = FALSE;

         if (IR_OPR(ir_idx) != Call_Opr) {
            /* still an assignment */

            save_curr_stmt_sh_idx = curr_stmt_sh_idx;
            line = IR_LINE_NUM(ir_idx);
            col = IR_COL_NUM(ir_idx);

            forall_dependence = FALSE;
            check_dependence(&forall_dependence,
                             IR_OPND_L(ir_idx),
                             IR_OPND_R(ir_idx));

            if (forall_dependence) {

               /* take the type for the tmp from the lhs, */
               /* take the shape from the rhs */

               forall_exp_desc             = exp_desc_r;
               forall_exp_desc.type_idx    = exp_desc_l.type_idx;
               forall_exp_desc.type        = exp_desc_l.type;
               forall_exp_desc.linear_type = exp_desc_l.linear_type;

               if (exp_desc_l.type == Character) {
                  /* use the base attr's char type idx */

                  COPY_OPND(l_opnd, IR_OPND_L(ir_idx));
                  attr_idx = find_base_attr(&l_opnd, &opnd_line, &opnd_col);
                  forall_exp_desc.type_idx = ATD_TYPE_IDX(attr_idx);
                  forall_exp_desc.type = Character;
                  forall_exp_desc.linear_type = 
                                  TYP_LINEAR(forall_exp_desc.type_idx);
                  forall_exp_desc.char_len.fld = 
                                  TYP_FLD(ATD_TYPE_IDX(attr_idx));
                  forall_exp_desc.char_len.idx = 
                                  TYP_IDX(ATD_TYPE_IDX(attr_idx));
               }

               gen_forall_tmp(&forall_exp_desc, 
                              &forall_tmp_opnd, 
                              line, 
                              col, 
                              FALSE);

               asg_idx = gen_ir(OPND_FLD(forall_tmp_opnd), 
                                       OPND_IDX(forall_tmp_opnd),
                            Asg_Opr, forall_exp_desc.type_idx, line, col,
                                IR_FLD_R(ir_idx), IR_IDX_R(ir_idx));

               gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
               curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

               gen_forall_loops(curr_stmt_sh_idx, curr_stmt_sh_idx);
   
               gen_forall_if_mask(curr_stmt_sh_idx, curr_stmt_sh_idx);

               COPY_OPND(opnd, IR_OPND_R(asg_idx));
               process_deferred_functions(&opnd);
               COPY_OPND(IR_OPND_R(asg_idx), opnd);

               curr_stmt_sh_idx = save_curr_stmt_sh_idx;

               copy_subtree(&forall_tmp_opnd, &forall_tmp_opnd);
               COPY_OPND(IR_OPND_R(ir_idx), forall_tmp_opnd);
   
               gen_forall_loops(curr_stmt_sh_idx, curr_stmt_sh_idx);
   
               gen_forall_if_mask(curr_stmt_sh_idx, curr_stmt_sh_idx);
   
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               process_deferred_functions(&opnd);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
            else {
               gen_forall_loops(curr_stmt_sh_idx, curr_stmt_sh_idx);
   
               gen_forall_if_mask(curr_stmt_sh_idx, curr_stmt_sh_idx);

               COPY_OPND(opnd, IR_OPND_R(ir_idx));
               process_deferred_functions(&opnd);
               COPY_OPND(IR_OPND_R(ir_idx), opnd);

               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               process_deferred_functions(&opnd);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
         }
         else {
            gen_forall_loops(curr_stmt_sh_idx, curr_stmt_sh_idx);
 
            gen_forall_if_mask(curr_stmt_sh_idx, curr_stmt_sh_idx);

            gen_opnd(&opnd, 
                     SH_IR_IDX(curr_stmt_sh_idx), 
                     IR_Tbl_Idx, 
                     line, 
                     col);
            process_deferred_functions(&opnd);
            SH_IR_IDX(curr_stmt_sh_idx) = OPND_IDX(opnd);
         }
      }

#ifdef KEY /* Bug 6845 */
      /* TR15581 requires automatic deallocation and allocation during
       * assignment of scalar structure having allocatable components, but
       * does not require this during assignment of an allocatable array;
       * that's a F2003 feature (which requires more work, and study to figure
       * out how it interacts with 'where' and 'forall'.)
       *
       * Possible optimization would deallocate the allocatable components
       * in the target, bytewise copy the structure, and then allocate and
       * copy the allocatable components. For now, we do it one component
       * at a time.
       */
      int type_idx_l = exp_desc_l.type_idx;
      if (Structure == exp_desc_l.type &&
	ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx_l)) &&
	SH_STMT_TYPE(curr_stmt_sh_idx) == Assignment_Stmt &&
	IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) == Asg_Opr) {

	/* Change original assignment to "continue" (it might be labelled) */
	SH_STMT_TYPE(curr_stmt_sh_idx) = Continue_Stmt;
	int asg_ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
	SH_IR_IDX(curr_stmt_sh_idx) = NULL_IDX;

	/* Non-allocatable array whose element type is a structure containing
	 * allocatable components */
	if (exp_desc_l.rank) {
	  help_assign_array_of_structure(line, col, IR_IDX_L(asg_ir_idx),
	    IR_FLD_L(asg_ir_idx), IR_IDX_R(asg_ir_idx), IR_FLD_R(asg_ir_idx));
	}

	else {
	  help_assign_cpnts(line, col, exp_desc_l.type_idx,
	    IR_IDX_L(asg_ir_idx), IR_FLD_L(asg_ir_idx), IR_IDX_R(asg_ir_idx),
	    IR_FLD_R(asg_ir_idx));
	}
      }
#endif /* KEY Bug 6845 */

      /*
      Generate this label immediately prior to the assignment
      statement.   PDGCS will extract the information from
      this label and put it on the TOP OF LOOP label they
      create when they create the DO loop for this assignent statement.
      */
      if (IR_RANK(ir_idx) > 0) {
         label_idx = gen_internal_lbl(line);
         NTR_IR_TBL(idx);
         IR_OPR(idx)                 = Label_Opr;
         IR_TYPE_IDX(idx)            = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(idx)            = line;
         IR_COL_NUM(idx)             = col;
         IR_FLD_L(idx)               = AT_Tbl_Idx;
         IR_IDX_L(idx)               = label_idx;
         IR_COL_NUM_L(idx)           = col;
         IR_LINE_NUM_L(idx)          = line;
         AT_DEFINED(label_idx)       = TRUE;
         AT_REFERENCED(label_idx)    = Not_Referenced;
         ATL_TOP_OF_LOOP(label_idx)  = TRUE;
         ATL_INFORM_ONLY(label_idx)  = TRUE;

         gen_sh(Before, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = idx;
         ATL_DEF_STMT_IDX(label_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
         set_directives_on_label(label_idx);
      }
   }
   else if (IR_OPR(ir_idx) == Ptr_Asg_Opr) {

      if (IR_FLD_R(ir_idx) == IR_Tbl_Idx &&
          IR_OPR(IR_IDX_R(ir_idx)) == Call_Opr &&
          IR_LIST_CNT_R(IR_IDX_R(ir_idx)) == 0 &&
          AT_IS_INTRIN(IR_IDX_L(IR_IDX_R(ir_idx))) &&
          (strcmp(AT_OBJ_NAME_PTR(IR_IDX_L(IR_IDX_R(ir_idx))), "NULL") == 0)) {

         NTR_IR_LIST_TBL(list_idx);
         attr_idx = find_base_attr(&(IR_OPND_L(ir_idx)), &line, &col);
         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = attr_idx;
         IL_LINE_NUM(list_idx) = IR_LINE_NUM(ir_idx);
         IL_COL_NUM(list_idx) = IR_COL_NUM(ir_idx);

         IR_IDX_R(IR_IDX_R(ir_idx)) = list_idx;
         IR_FLD_R(IR_IDX_R(ir_idx)) = IL_Tbl_Idx;
         IR_LIST_CNT_R(IR_IDX_R(ir_idx)) = 1; 
      }

      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;

      xref_state = CIF_Symbol_Modification;
      COPY_OPND(l_opnd, IR_OPND_L(ir_idx));
      exp_desc_l.rank = 0;
      ok = expr_semantics(&l_opnd, &exp_desc_l);
      COPY_OPND(IR_OPND_L(ir_idx), l_opnd);

      if (! ok) {
         goto EXIT;
      }

      if (! exp_desc_l.pointer) {
         attr_idx = find_base_attr(&l_opnd, &line, &col);
         PRINTMSG(line, 417, Error, col);
         ok = FALSE;
      }
#ifdef KEY /* Bug 572 */
      /* An expression like "parameter_x%ptr_component_y" might be both a
       * pointer and a constant, and a constant is not allowed here. */
      if (exp_desc_l.constant) {
         PRINTMSG(line, 326, Error, col);
         ok = FALSE;
      }
#endif /* KEY Bug 572 */

#ifdef KEY /* Bug 14150 */
      ok &= check_for_legal_assignment_define(&l_opnd,
        IR_OPR(ir_idx) == Ptr_Asg_Opr);
#else /* KEY Bug 14150 */
      ok &= check_for_legal_define(&l_opnd);
#endif /* KEY Bug 14150 */

      attr_idx = find_base_attr(&l_opnd, &line, &col);

      if (attr_idx &&
          AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ATD_PTR_ASSIGNED(attr_idx) = TRUE;
      }

# ifdef _F_MINUS_MINUS
      /* prevent ptr asg to pointer component of co-array */

      if (ok &&
          dump_flags.f_minus_minus &&
          AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          ATD_CLASS(attr_idx) == Struct_Component) {

         attr_idx = find_left_attr(&l_opnd);

         if (ATD_PE_ARRAY_IDX(attr_idx)) {

            PRINTMSG(line, 1572, Error, col);
         }
      }
# endif

      /* The pointer assignment statement really should have its own CIF stmt */
      /* but libcif did not want to add another value at this time.           */
      /* LRR    12 May 1994						      */

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Assignment_Stmt, statement_number); 
      } 

      xref_state = CIF_Symbol_Reference;
      COPY_OPND(r_opnd, IR_OPND_R(ir_idx));
      exp_desc_r.rank = 0;
#ifdef KEY /* Bug 572 */
      /* Pointer assignment definitely allows pointer on RHS */
      int save_constant_ptr_ok = constant_ptr_ok;
      constant_ptr_ok = TRUE;
#endif /* KEY Bug 572 */
      ok = expr_semantics(&r_opnd, &exp_desc_r)
                       && ok;
#ifdef KEY /* Bug 572 */
      constant_ptr_ok = save_constant_ptr_ok;
#endif /* KEY Bug 572 */
      COPY_OPND(IR_OPND_R(ir_idx), r_opnd);

      if (! ok) {
         goto EXIT;
      }

      if (OPND_FLD(r_opnd) == AT_Tbl_Idx) {
         
         if (AT_OBJ_CLASS(OPND_IDX(r_opnd)) == Data_Obj &&
             !ATD_POINTER(OPND_IDX(r_opnd)) && !ATD_TARGET(OPND_IDX(r_opnd)))  {
            PRINTMSG(OPND_LINE_NUM(r_opnd), 418, Error, OPND_COL_NUM(r_opnd));
            ok = FALSE;
         }

         if (AT_OBJ_CLASS(OPND_IDX(r_opnd)) == Data_Obj &&
             ATD_PURE(OPND_IDX(r_opnd))) {
            PRINTMSG(OPND_LINE_NUM(r_opnd), 1270, Error, OPND_COL_NUM(r_opnd),
                     AT_OBJ_NAME_PTR(OPND_IDX(r_opnd)),
                     ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure":"elemental");
            ok = FALSE;
         }
      }
      else if (OPND_FLD(r_opnd) == IR_Tbl_Idx) {
         
         if (IR_OPR(OPND_IDX(r_opnd)) == Call_Opr) {

            if (!ATD_POINTER(ATP_RSLT_IDX(IR_IDX_L(OPND_IDX(r_opnd))))) {
               PRINTMSG(IR_LINE_NUM_L(OPND_IDX(r_opnd)), 421, Error,
                        IR_COL_NUM_L(OPND_IDX(r_opnd)));
               ok = FALSE;
            }
         }
         else if (exp_desc_r.reference      ||
                  exp_desc_r.tmp_reference) {
            attr_idx = find_base_attr(&r_opnd, &line, &col);

            if (! exp_desc_r.pointer && ! exp_desc_r.target) {
               PRINTMSG(line, 418, Error, col);
               ok = FALSE;
            }
            else {
               if (exp_desc_r.rank != 0) {

                  /* check for IL_VECTOR_SUBSCRIPT */

                  if (exp_desc_r.vector_subscript) {

                     /* might want to find a more correct position */

                     PRINTMSG(IR_LINE_NUM(OPND_IDX(r_opnd)), 420, Error,
                              IR_COL_NUM(OPND_IDX(r_opnd)));
                     ok = FALSE;
                  }
               }

               if (IR_OPR(OPND_IDX(r_opnd)) == Dv_Deref_Opr &&
                   IR_FLD_L(OPND_IDX(r_opnd)) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(IR_IDX_L(OPND_IDX(r_opnd))) == Data_Obj &&
                   ATD_PURE(IR_IDX_L(OPND_IDX(r_opnd)))) {
                  ok = FALSE;
                  PRINTMSG(IR_COL_NUM_L(OPND_IDX(r_opnd)), 1270, Error,
                           IR_COL_NUM_L(OPND_IDX(r_opnd)),
                           AT_OBJ_NAME_PTR(IR_IDX_L(OPND_IDX(r_opnd))),
                           ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ?
                                    "pure" : "elemental");
               }
               else {

                  if (AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_PURE(attr_idx)){
                     find_opnd_line_and_column(&r_opnd, &opnd_line, &opnd_col);
                     ok = FALSE;
                     PRINTMSG(opnd_line, 1270, Error, opnd_col,
                              AT_OBJ_NAME_PTR(attr_idx),
                              ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ?
                                       "pure" : "elemental");
                  }
               }
            }
         }
         else { /* an expression other than a call .. error */
            find_opnd_line_and_column(&r_opnd, &opnd_line, &opnd_col);
            PRINTMSG(opnd_line, 421, Error, opnd_col);
            ok = FALSE;
         }
      }
      else { /* error .. must be pointer .. assuming only constants here */
         find_opnd_line_and_column(&r_opnd, &opnd_line, &opnd_col);
         PRINTMSG(opnd_line, 418, Error, opnd_col);
         ok = FALSE;
      }

      if (ok) {

         if (exp_desc_r.rank   != exp_desc_l.rank) {
            /* rank error */
            PRINTMSG(IR_LINE_NUM(ir_idx), 431, Error, IR_COL_NUM(ir_idx));
            ok = FALSE;
         }

         if (exp_desc_r.type != exp_desc_l.type ||
             (exp_desc_r.type == Structure &&
              !compare_derived_types(exp_desc_r.type_idx,exp_desc_l.type_idx))){
            r_err_word[0] = '\0';
            l_err_word[0] = '\0';

            strcat(r_err_word, get_basic_type_str(exp_desc_r.type_idx));

            strcat(l_err_word, get_basic_type_str(exp_desc_l.type_idx));

            PRINTMSG(IR_LINE_NUM(ir_idx), 432, Error,
                     IR_COL_NUM(ir_idx),
                     r_err_word,
                     l_err_word);
            ok = FALSE;
         }

         if (exp_desc_r.type == exp_desc_l.type &&
             exp_desc_r.type != Character       &&
             exp_desc_r.type != Structure       &&
             exp_desc_r.linear_type != exp_desc_l.linear_type) {

            PRINTMSG(IR_LINE_NUM(ir_idx), 419, Error, IR_COL_NUM(ir_idx));
            ok = FALSE;
         }
         else if (exp_desc_r.type == exp_desc_l.type      &&
                  exp_desc_r.type == Character            &&
                  exp_desc_r.char_len.fld == CN_Tbl_Idx   &&
                  exp_desc_l.char_len.fld == CN_Tbl_Idx   &&
                  fold_relationals(exp_desc_r.char_len.idx,
                                   exp_desc_l.char_len.idx,
                                   Ne_Opr)) {

            PRINTMSG(IR_LINE_NUM(ir_idx), 853, Error, IR_COL_NUM(ir_idx));
            ok = FALSE;
         }
      }

      if (ok) {
            
         if (active_forall_sh_idx) {
            defer_stmt_expansion = FALSE;

            save_curr_stmt_sh_idx = curr_stmt_sh_idx;
            line = IR_LINE_NUM(ir_idx);
            col = IR_COL_NUM(ir_idx);

            forall_exp_desc = exp_desc_l;
            gen_forall_tmp(&forall_exp_desc, &forall_tmp_opnd, 
                           line, col, TRUE);

            copy_subtree(&forall_tmp_opnd, &forall_tmp_opnd_l);
            asg_idx = gen_ir(OPND_FLD(forall_tmp_opnd_l),
                                    OPND_IDX(forall_tmp_opnd_l),
                         Ptr_Asg_Opr, exp_desc_r.type_idx, line, col,
                             IR_FLD_R(ir_idx), IR_IDX_R(ir_idx));

            gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

            gen_forall_loops(curr_stmt_sh_idx, curr_stmt_sh_idx);

            gen_forall_if_mask(curr_stmt_sh_idx, curr_stmt_sh_idx);

            if (OPND_FLD(forall_tmp_opnd_l) == IR_Tbl_Idx) {
               if (IR_OPR(OPND_IDX(forall_tmp_opnd_l)) == Whole_Substring_Opr) {
                  COPY_OPND(forall_tmp_opnd_l,
                            IR_OPND_L(OPND_IDX(forall_tmp_opnd_l)));
               }

               if (IR_OPR(OPND_IDX(forall_tmp_opnd_l)) == Whole_Subscript_Opr) {
                  COPY_OPND(forall_tmp_opnd_l,
                            IR_OPND_L(OPND_IDX(forall_tmp_opnd_l)));
               }

               if (IR_OPR(OPND_IDX(forall_tmp_opnd_l)) == Dv_Deref_Opr) {
                  COPY_OPND(forall_tmp_opnd_l,
                            IR_OPND_L(OPND_IDX(forall_tmp_opnd_l)));
               }
            }

            copy_subtree(&forall_tmp_opnd_l, &forall_tmp_opnd_l);

            attr_idx = find_base_attr(&forall_tmp_opnd_l,&opnd_line,&opnd_col);

            gen_dv_whole_def_init(&forall_tmp_opnd_l,
                                  attr_idx,
                                  Before);

            COPY_OPND(opnd, IR_OPND_R(asg_idx));
            process_deferred_functions(&opnd);
            COPY_OPND(IR_OPND_R(asg_idx), opnd);

            lower_ptr_asg(&exp_desc_r);

            curr_stmt_sh_idx = save_curr_stmt_sh_idx;

            COPY_OPND(IR_OPND_R(ir_idx), forall_tmp_opnd);

            gen_forall_loops(curr_stmt_sh_idx, curr_stmt_sh_idx);

            gen_forall_if_mask(curr_stmt_sh_idx, curr_stmt_sh_idx);

            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            process_deferred_functions(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            lower_ptr_asg(&forall_exp_desc);
         }
         else {
            lower_ptr_asg(&exp_desc_r);
         }
      }
   }

EXIT: 

   defer_stmt_expansion = FALSE;

   TRACE (Func_Exit, "assignment_stmt_semantics", NULL);

   return;

}  /* assignment_stmt_semantics */

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

static void lower_ptr_asg(expr_arg_type *exp_desc_r)

{
   int			ir_idx;
   opnd_type		l_opnd;
   opnd_type		r_opnd;
   int			sh_idx;

   TRACE (Func_Entry, "lower_ptr_asg", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
      if (IR_OPR(IR_IDX_L(ir_idx)) == Whole_Substring_Opr) {
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IR_IDX_L(ir_idx)));
      }

      if (IR_OPR(IR_IDX_L(ir_idx)) == Whole_Subscript_Opr) {
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IR_IDX_L(ir_idx)));
      }

      if (IR_OPR(IR_IDX_L(ir_idx)) == Dv_Deref_Opr) {
         COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IR_IDX_L(ir_idx)));
      }
   }
   else {
# ifdef _DEBUG
      print_ir(ir_idx);
# endif
      PRINTMSG(IR_LINE_NUM(ir_idx), 973, Internal,
               IR_COL_NUM(ir_idx));
   }

   /* do the stmt thing here */

   COPY_OPND(l_opnd, IR_OPND_L(ir_idx));
   COPY_OPND(r_opnd, IR_OPND_R(ir_idx));

   if (exp_desc_r->pointer || exp_desc_r->allocatable) {
      sh_idx = curr_stmt_sh_idx;
      ptr_assign_from_ptr(&l_opnd, &r_opnd);

      /* Remove the pointer assignment SH unless it is labeled.  If  */
      /* it was labeled, just turn it into a compiler-generated      */
      /* CONTINUE so the SH index in the Label_Def SH remains        */
      /* correct.                                               */

      if (SH_LABELED(sh_idx)) {

# ifdef _DEBUG
         if (IR_OPR(SH_IR_IDX(sh_idx)) != Ptr_Asg_Opr) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 974, Internal,
                     IR_COL_NUM(ir_idx));
         }
# endif

         SH_STMT_TYPE(sh_idx)    = Continue_Stmt;
         SH_IR_IDX(sh_idx)       = NULL_IDX;
         SH_COMPILER_GEN(sh_idx) = TRUE;


         /* If the pointer assignment stmt is also a loop termination*/
         /* stmt, copy the loop end info to the current assignment   */
         /* SH (for Dv_Set_P_Or_A).                                     */

         if (SH_LOOP_END(sh_idx)) {
            SH_LOOP_END(curr_stmt_sh_idx) = TRUE;
            SH_PARENT_BLK_IDX(curr_stmt_sh_idx) =
               SH_PARENT_BLK_IDX(sh_idx);
         }
      }
      else {

# ifdef _DEBUG
         if (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) != Ptr_Asg_Opr) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 974, Internal,
                     IR_COL_NUM(ir_idx));
         }
# endif

         remove_sh(curr_stmt_sh_idx);
         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      }
   }
   else if (exp_desc_r->target) {
      dope_vector_setup(&r_opnd, exp_desc_r, &l_opnd, TRUE);
   }

   TRACE (Func_Exit, "lower_ptr_asg", NULL);

   return;

}  /* lower_ptr_asg */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine is the wrapper for expr_sem. It will fold any aggregate  *|
|*      expression that are returned by expr_sem().                           *|
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

boolean expr_semantics (opnd_type       *result_opnd,
                        expr_arg_type   *exp_desc)
#ifdef KEY /* Bug 934 */
{
  return expr_semantics_d(result_opnd, exp_desc, FALSE);
}

/*
 * Like expr_semantics(), but capable of passing along the knowledge that
 * we're dealing with the RHS of an assignment of an entire derived type.
 */
static boolean expr_semantics_d (opnd_type     *result_opnd,
                        expr_arg_type   *exp_desc,
			boolean		derived_assign)
#endif /* KEY Bug 934 */

{
   boolean      	ok = TRUE;
   opnd_type    	opnd;
   boolean              save_check_type_conversion;
   int                  save_target_array_idx;
   opnd_type            save_init_target_opnd;
   int			save_target_char_len_idx;
   int                  save_target_type_idx;


   TRACE (Func_Entry, "expr_semantics", NULL);

   save_check_type_conversion   = check_type_conversion;
   save_target_array_idx        = target_array_idx;
   COPY_OPND(save_init_target_opnd, init_target_opnd);
   save_target_char_len_idx     = target_char_len_idx;
   save_target_type_idx         = target_type_idx;

   check_type_conversion        = FALSE;
   target_array_idx             = NULL_IDX;
   init_target_opnd		= null_opnd;

   target_char_len_idx          = NULL_IDX;
   target_type_idx              = NULL_IDX;

#ifdef KEY /* Bug 934 */
   ok = expr_sem_d(result_opnd, exp_desc, derived_assign);
#else /* KEY Bug 934 */
   ok = expr_sem(result_opnd, exp_desc);
#endif /* KEY Bug 934 */

   check_type_conversion        = save_check_type_conversion;
   target_array_idx             = save_target_array_idx;
   COPY_OPND(init_target_opnd, save_init_target_opnd);
   target_char_len_idx          = save_target_char_len_idx;
   target_type_idx              = save_target_type_idx;

   if (ok                            &&
       exp_desc->foldable            &&
       ((OPND_FLD((*result_opnd)) != CN_Tbl_Idx &&
         OPND_FLD((*result_opnd)) != AT_Tbl_Idx &&
         (OPND_FLD((*result_opnd)) != IR_Tbl_Idx ||
          (IR_OPR(OPND_IDX((*result_opnd))) != Whole_Subscript_Opr &&
           (IR_OPR(OPND_IDX((*result_opnd))) != Whole_Substring_Opr ||
            IR_FLD_L(OPND_IDX((*result_opnd))) != IR_Tbl_Idx ||
            IR_OPR(IR_IDX_L(OPND_IDX((*result_opnd)))) !=
                                                    Whole_Subscript_Opr)))) ||
        check_type_conversion == TRUE ||
        OPND_FLD(init_target_opnd) != NO_Tbl_Idx ||
        target_array_idx != NULL_IDX)) {

      COPY_OPND(opnd, (*result_opnd));
      ok = fold_aggragate_expression(&opnd, exp_desc, FALSE) && ok;
      COPY_OPND((*result_opnd), opnd);
   }

   TRACE (Func_Exit, "expr_semantics", NULL);

   return(ok);

}  /* expr_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*   Expr_semantics is the main expression semantics checker. It works        *|
|*   recursively to process the entire subtree it is called with.             *|
|*   Expr_semantics should be called for all references and expressions       *|
|*   that require attr_link and type resolution.                              *|
|*   It does other things too.                                                *|
|*      1. All attr indexes are resolved to the ultimate attr in an attr_link *|
|*         chain. Type, rank and other stuff is propagated up the call chain. *|
|*      2. Semantic checks (type, rank etc) are done on all numeric operators *|
|*         and information is propagated up.                                  *|
|*      3. Folding is done for constant operands of some operators.           *|
|*      4. Function calls are pulled out of expressions and replaced with     *|
|*         temps.                                                             *|
|*      5. Ambiguous array refs or other blah() formations are possibly       *|
|*         changed to function calls.                                         *|
|*      6. Subscript oprs are inserted over whole array references.           *|
|*      7. Substring Oprs are inserted over character variable refs that      *|
|*         weren't substringed by the user.                                   *|
|*      8. Calls to resolve_ext_opr check for overloaded operators.           *|
|*      9. Calls to call_list_semantics check for generic interface calls     *|
|*         and do actual argument semantic checks. (back through here)        *|
|*     10. Allocate and deallocate objects are semantically checked here.     *|
|*     11. Other minor things.                                                *|
|*									      *|
|* Input parameters:							      *|
|*	result_opnd - operand to examine.                                     *|
|*      exp_desc    - exp_arg_type (declared in sytb.h)                       *|
|*                    This is used to propagate information up the call chain *|
|*                    and some information down the chain.                    *|
|*                                                                            *|
|*               exp_desc is declared as follows ...                          *|
|*      struct  expr_semantics_args    {                                      *|
|*                                                                            *|
|*           basic type of subtree  -> basic_type_type   type            : 8; *|
|*           linear type            -> linear_type_type  linear_type     : 8; *|
|*           type index of subtree  -> Uint              type_idx        : 16;*|
|*                                                                            *|
|*           unused                 -> Uint              UNUSED1         : 5; *|
|*           rank of subtree        -> Uint              rank            : 8; *|
|*           subtree is a constant  -> boolean           constant        : 1; *|
|*           subtree is foldable now-> boolean           foldable        : 1; *|
|*                                                                            *|
|*           subtree involves a constant                                      *|
|*           value implied do lcv but will                                    *|
|*           fold when its replaced -> boolean           will_fold_later : 1; *|
|*           has pointer attribute  -> boolean           pointer         : 1; *|
|*           has target attribute   -> boolean           target          : 1; *|
|*           vector subscript ref   -> boolean           vector_subscript: 1; *|
|*           is a data obj ref      -> boolean           reference       : 1; *|
|*           ref is a constructor   -> boolean           constructor     : 1; *|
|*           structure subobject    -> boolean           component       : 1; *|
|*           array section ref      -> boolean           section         : 1; *|
|*           tree is a label ref    -> boolean           label           : 1; *|
|*           tree is array element  -> boolean           array_elt       : 1; *|
|*           whole assumed shape    -> boolean           assumed_shape   : 1; *|
|*           whole assumed size     -> boolean           assumed_size    : 1; *|
|*           allocatable array ref  -> boolean           allocatable     : 1; *|
|*           ref is dope vector     -> boolean           dope_vector     : 1; *|
|*           reference to tmp       -> boolean           tmp_reference   : 1; *|
|*           tree has constructor   -> boolean           has_constructor : 1; *|
|*           optional dummy ref     -> boolean           optional_darg   : 1; *|
|*           expr contains a        -> boolean           has_symbolic    : 1; *|
|*                sybolic constant                                            *|
|*                                                                            *|
|*           unused                 -> Uint              UNUSED2         : 32;*|
|*                                                                            *|
|*           unused                 -> Uint              UNUSED3         : 8; *|
|*           cif id for ref         -> Uint              cif_id          : 24;*|
|*                                                                            *|
|*                                                                            *|
|*           character length       -> opnd_type         char_len;            *|
|*           shape of subtree       -> opnd_type         shape[7];            *|
|*                                     };                                     *|
|*                                                                            *|
|*                                                                            *|
|*               reference means that subtree describes a data object         *|
|*               reference, and is not an expression.                         *|
|*               Most of these flags are for special use and any questions    *|
|*               about specific behavior should be directed to the developer. *|
|*                                                                            *|
|*   =========>  RANK MUST BE SET TO ZERO BEFORE CALLING THIS ROUTINE!!!!!    *|
|*                                                                            *|
|*               The exp_desc->rank variable is used to propagate the rank    *|
|*               of a part-ref to the rest of the reference tree and so is    *|
|*               used to pass information down the call chain. This is to     *|
|*               catch that wonderful constraint that a pointer subobject     *|
|*               cannot have a part-ref to the left that has rank > 0.        *|
|*                                                                            *|
|*               Always copy your operand to a local variable of type         *|
|*               opnd_type before the call to expr_semantics and copy the     *|
|*               returned opnd back to your original. This is because tables  *|
|*               may be realloc'ed and moved.                                 *|
|*                                                                            *|
|*               Use the information from the exp_desc structure if you want  *|
|*               things like type, type_idx, rank ... when you don't care     *|
|*               the tree actually looks like. Constant and reference are     *|
|*               also handy to quickly see what type of subtree you have.     *|
|*									      *|
|* Output parameters:							      *|
|*	result_opnd - output opnd_type                                        *|
|*      exp_desc    - the expression descriptor (see above) that describes    *|
|*                    the result tree.                                        *|
|*									      *|
|* Returns:								      *|
|*      TRUE if no semantic errors.                                           *|
|*      FALSE if errors were issued or if an attr with AT_DCL_ERR was found.  *|
|*									      *|
\******************************************************************************/

boolean expr_sem (opnd_type       *result_opnd,
                  expr_arg_type   *exp_desc)
#ifdef KEY /* Bug 934 */
{
  return expr_sem_d(result_opnd, exp_desc, FALSE);
}

/*
 * Like expr_sem(), but capable of passing in the knowledge that we're dealing
 * with the RHS of an assignment of an entire derived type.
 */
static boolean expr_sem_d(opnd_type      *result_opnd,
                  expr_arg_type   *exp_desc,
		  boolean	  derived_assign)
#endif /* KEY Bug 934 */

{
   int                 al_list_idx;
   int                 attr_idx;
   int		       col;
   int                 dv_idx;
   expr_arg_type       exp_desc_l;
   expr_arg_type       exp_desc_r;
   boolean	       host_associated;
   int                 ir_idx		= NULL_IDX;
   int		       line;
   int		       list_idx;
#ifdef KEY /* Bug 10177 */
   int                 msg_num = 0;
#else /* KEY Bug 10177 */
   int                 msg_num;
#endif /* KEY Bug 10177 */
   opnd_type	       opnd;
   int		       rank_in;
   boolean             junk;
   boolean	       save_in_call_list;
   boolean	       save_in_constructor;
   boolean	       save_no_sub_or_deref;
   boolean             save_insert_subs_ok;
   boolean             ok	= TRUE;


   TRACE (Func_Entry, "expr_sem", NULL);

   /* these are here to initialize so that cases that are incomplete */
   /* do not return wierd stuff.                                     */

   rank_in			= exp_desc->rank;
   (*exp_desc)			= init_exp_desc;
#ifdef KEY /* Bug 934 */
   exp_desc->derived_assign = derived_assign;
#endif /* KEY Bug 934 */
   exp_desc->linear_type	= TYPELESS_DEFAULT_TYPE;
   exp_desc->type_idx		= TYPELESS_DEFAULT_TYPE;

   find_opnd_line_and_column(result_opnd, &line, &col);

   switch (OPND_FLD((*result_opnd))) {

      case NO_Tbl_Idx :
         break;

      case CN_Tbl_Idx:

         exp_desc->type_idx	= CN_TYPE_IDX(OPND_IDX((*result_opnd)));
         exp_desc->type		= TYP_TYPE(exp_desc->type_idx);
         exp_desc->linear_type	= TYP_LINEAR(exp_desc->type_idx);

         if (exp_desc->type == Character) {
            exp_desc->char_len.fld = TYP_FLD(exp_desc->type_idx);
            exp_desc->char_len.idx = TYP_IDX(exp_desc->type_idx);
            OPND_LINE_NUM(exp_desc->char_len) = line;
            OPND_COL_NUM(exp_desc->char_len) = col;
         }

         if (exp_desc->type == Character                             &&
            compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                                 MAX_CHARS_IN_TYPELESS, 
                                 Le_Opr)) {
            exp_desc->linear_type = Short_Char_Const;
         }
                  
         exp_desc->rank        = 0;
         exp_desc->constant    = TRUE;
         exp_desc->foldable    = TRUE;
         exp_desc->will_fold_later = TRUE;
         break;

      case AT_Tbl_Idx  :

         attr_idx		= OPND_IDX((*result_opnd));
         AT_LOCKED_IN(attr_idx) = TRUE;
         host_associated	= FALSE;



         if (expr_mode == Restricted_Imp_Do_Expr) {

            if (in_implied_do               &&
                AT_OBJ_CLASS(attr_idx) == Data_Obj) {

               while (AT_ATTR_LINK(attr_idx) &&
                      ! AT_IGNORE_ATTR_LINK(attr_idx)) {
                  attr_idx                 = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx)   = TRUE;
                  host_associated          = TRUE;
               }

               if (AT_ATTR_LINK(attr_idx)) {
                  attr_idx                 = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx)   = TRUE;
               }
            }
            else {

               while (AT_ATTR_LINK(attr_idx)           &&
                      ! AT_IGNORE_ATTR_LINK(attr_idx)) {

                  if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                      ATD_IMP_DO_LCV(attr_idx)) {
                     break;
                  }
              
                  attr_idx = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx) = TRUE;
               }
            }

            if (AT_NOT_VISIBLE(attr_idx)) {
               PRINTMSG(line, 486, Error,
                        col,
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(AT_MODULE_IDX((attr_idx))));
               ok = FALSE;
               break;
            }

            if (! AT_DCL_ERR(attr_idx)) {

               if (AT_OBJ_CLASS(attr_idx) != Data_Obj           ||
                   (ATD_CLASS(attr_idx) != Constant          &&
                    ATD_CLASS(attr_idx) != Struct_Component  &&
                    ! ATD_IMP_DO_LCV(attr_idx))) {
                  OPND_IDX((*result_opnd)) = attr_idx;
                  PRINTMSG(line, 658, Error, col, AT_OBJ_NAME_PTR(attr_idx));
                  ok = FALSE;
                  break;
               }
            }
         }
         else if (in_implied_do               &&
                  AT_OBJ_CLASS(attr_idx) == Data_Obj) {

            while (AT_ATTR_LINK(attr_idx)           &&
                   ! AT_IGNORE_ATTR_LINK(attr_idx)) {
               attr_idx                 = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx)   = TRUE;
               host_associated          = TRUE;
            }

            if (AT_ATTR_LINK(attr_idx)) {
               attr_idx                 = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx)   = TRUE;
            }


            if (ATD_IMP_DO_LCV(attr_idx) &&
                constructor_level > ATD_TMP_IDX(attr_idx)) {
               constructor_level = ATD_TMP_IDX(attr_idx);
            }
         }
         else {
            while (AT_ATTR_LINK(attr_idx)           &&
                   ! AT_IGNORE_ATTR_LINK(attr_idx)) {

               attr_idx			= AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx)	= TRUE;
               host_associated		= TRUE;
            }

            if (AT_ATTR_LINK(attr_idx) &&
                AT_OBJ_CLASS(AT_ATTR_LINK(attr_idx)) == Data_Obj &&
                ATD_FORALL_INDEX(AT_ATTR_LINK(attr_idx))) {

               attr_idx                 = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx)   = TRUE;
            }
         }

         if (AT_NOT_VISIBLE(attr_idx)) {
            PRINTMSG(line, 486, Error,
                     col,
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(AT_MODULE_IDX((attr_idx))));
            ok = FALSE;
            break; 
         }

         if (expr_mode == Data_Stmt_Target_Expr &&
             (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
              (ATD_CLASS(attr_idx) != Constant &&
               ATD_CLASS(attr_idx) != Struct_Component))) {

            PRINTMSG(line, 705, Error, col, AT_OBJ_NAME_PTR(attr_idx));
            ok = FALSE;
         }

         OPND_IDX((*result_opnd)) = attr_idx;

         if (! in_component_ref              &&
             (cif_flags & XREF_RECS) != 0    &&
             (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
              ATD_CLASS(attr_idx) != Dummy_Argument ||
              ! ATD_PARENT_OBJECT(attr_idx) ||
              ! ATD_SF_DARG(attr_idx)) &&
             xref_state != CIF_No_Usage_Rec) {

            if (in_call_list) { /* output CIF_Symbol_Is_Actual_Arg */
               cif_usage_rec(attr_idx, AT_Tbl_Idx, line, col, 
                             CIF_Symbol_Is_Actual_Arg);
            }
            else { /* output according xref_state */
               cif_usage_rec(attr_idx, AT_Tbl_Idx, line, col, xref_state);
             }
         }

         exp_desc->cif_id = AT_CIF_SYMBOL_ID(attr_idx);

         if (AT_DCL_ERR(attr_idx)) {			/* just quit */
            ok = FALSE;
         }

         if (AT_OPTIONAL(attr_idx)) {
            exp_desc->optional_darg = TRUE;
         }

         switch (AT_OBJ_CLASS(attr_idx)) {

         case Data_Obj:

            if (ATD_CLASS(attr_idx) == Dummy_Argument &&
                ATD_COPY_ASSUMED_SHAPE(attr_idx) &&
                ATD_SF_ARG_IDX(attr_idx) != NULL_IDX) {

               attr_idx = ATD_SF_ARG_IDX(attr_idx);
               OPND_IDX((*result_opnd)) = attr_idx;
            }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# endif


            exp_desc->type_idx	= ATD_TYPE_IDX(attr_idx);
            exp_desc->type	= TYP_TYPE(exp_desc->type_idx);
            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

            if (ATD_PURE(attr_idx) && 
#ifdef KEY /* Bug 934 */
		/* This constraint only applies when assigning an entire
		 * derived type. Note that it's one of the areas where
		 * "allocatable" and "pointer" behave differently. */
                exp_desc->derived_assign &&
#endif /* KEY Bug 934 */
                stmt_type == Assignment_Stmt &&
                exp_desc->type == Structure &&
                ATT_POINTER_CPNT(TYP_IDX(exp_desc->type_idx))) {
               ok = FALSE;
               PRINTMSG(line, 1270, Error, col, AT_OBJ_NAME_PTR(attr_idx),
                        ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ?
                        "pure":"elemental");
            }

            if (exp_desc->type == Character) {
               if (!TYP_RESOLVED(ATD_TYPE_IDX(attr_idx))) {
                  char_bounds_resolution(attr_idx, &junk);
                  exp_desc->type_idx = ATD_TYPE_IDX(attr_idx);
               }

# if defined(_EXTENDED_CRI_CHAR_POINTER)
               if (TYP_FLD(exp_desc->type_idx) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(TYP_IDX(exp_desc->type_idx)) == Data_Obj &&
                   TYP_TYPE(ATD_TYPE_IDX(TYP_IDX(exp_desc->type_idx))) == 
                                                                  CRI_Ch_Ptr) {

                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx) = Clen_Opr;
                  IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
                  IR_LINE_NUM(ir_idx) = line;
                  IR_COL_NUM(ir_idx) = col;
                  IR_FLD_L(ir_idx) = AT_Tbl_Idx;
                  IR_IDX_L(ir_idx) = attr_idx;
                  IR_LINE_NUM_L(ir_idx) = line;
                  IR_COL_NUM_L(ir_idx) = col;

                  exp_desc->char_len.fld = IR_Tbl_Idx;
                  exp_desc->char_len.idx = ir_idx;
               }
               else {
                  exp_desc->char_len.fld = TYP_FLD(exp_desc->type_idx);
                  exp_desc->char_len.idx = TYP_IDX(exp_desc->type_idx);
                  OPND_LINE_NUM(exp_desc->char_len) = line;
                  OPND_COL_NUM(exp_desc->char_len) = col;
               }
# else
               exp_desc->char_len.fld = TYP_FLD(exp_desc->type_idx);
               exp_desc->char_len.idx = TYP_IDX(exp_desc->type_idx);
               OPND_LINE_NUM(exp_desc->char_len) = line;
               OPND_COL_NUM(exp_desc->char_len) = col;
# endif

               if (TYP_FLD(exp_desc->type_idx) == AT_Tbl_Idx) {
                  ADD_TMP_TO_SHARED_LIST(TYP_IDX(exp_desc->type_idx));
               }

               if (ATD_CLASS(attr_idx) == Constant &&
                   compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                                        MAX_CHARS_IN_TYPELESS,
                                        Le_Opr)) {
                  exp_desc->linear_type = Short_Char_Const;
               }
            }

            exp_desc->pointer     = ATD_POINTER(attr_idx);
            exp_desc->target      = ATD_TARGET(attr_idx);
            exp_desc->allocatable = ATD_ALLOCATABLE(attr_idx);
            exp_desc->dope_vector = ATD_IM_A_DOPE(attr_idx);

            if (ATD_POINTER(attr_idx) && rank_in != 0) {
               ok = FALSE;
               PRINTMSG(line, 408, Error, col);
            }

            if (cdir_switches.parallel_region       &&
                ATD_CLASS(attr_idx) != Struct_Component    &&
                ATD_CLASS(attr_idx) != Constant     &&
                ATD_CLASS(attr_idx) != Compiler_Tmp &&
                ATD_CLASS(attr_idx) != CRI__Pointee &&
                (ATD_CLASS(attr_idx) != Dummy_Argument ||
                 ! ATD_SF_DARG(attr_idx))           &&
                ! cdir_switches.autoscope           &&
#ifdef KEY /* Bug 8287 */
                SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) != Threadprivate &&
#endif /* KEY Bug 8287 */
                ! ATD_TASK_PRIVATE(attr_idx)        &&
                ! ATD_TASK_GETFIRST(attr_idx)       &&
                ! ATD_TASK_LASTLOCAL(attr_idx)      &&
                ! ATD_TASK_REDUCTION(attr_idx)      &&
                ! ATD_TASK_LASTTHREAD(attr_idx)     &&
                ! ATD_TASK_FIRSTPRIVATE(attr_idx)   &&
                ! ATD_TASK_COPYIN(attr_idx)         &&
                ! ATD_TASK_LASTPRIVATE(attr_idx)    &&
                ! ATD_TASK_SHARED(attr_idx))        {


               if (dump_flags.open_mp &&
                   OPND_FLD(cdir_switches.first_sh_blk_stk) == IL_Tbl_Idx) {
                   /* this means that we are in some sort of openmp region */
                   /* rather than a cmic region.                           */

                  if (cdir_switches.default_scope_list_idx != NULL_IDX &&
                      CN_INT_TO_C(IL_IDX(cdir_switches.default_scope_list_idx))
                                   == OPEN_MP_DEFAULT_NONE) {

                     PRINTMSG(line, 1510, Error, col,
                              AT_OBJ_NAME_PTR(attr_idx));
                     ok = FALSE;
                     /* add it to the shared list to prevent */
                     /* further errors.                      */
                     ADD_VAR_TO_SHARED_LIST(attr_idx);
                  }
               }
               else if (dump_flags.mp) {

               }
               else {

                  if (processing_do_var) {
                     PRINTMSG(line, 1509, Error, col, 
                              AT_OBJ_NAME_PTR(attr_idx));
                     /* add it to the private list to prevent */
                     /* further errors.                       */
                     ADD_VAR_TO_PRIVATE_LIST(attr_idx);
                  }
                  else {
                     PRINTMSG(line, 960, Error, col, 
                              AT_OBJ_NAME_PTR(attr_idx));
                     /* add it to the shared list to prevent */
                     /* further errors.                      */
                     ADD_VAR_TO_SHARED_LIST(attr_idx);
                  }
                  ok = FALSE;
               }
            }

            ADD_TMP_TO_SHARED_LIST(attr_idx);

            if (ATD_ARRAY_IDX(attr_idx)) {

               if (! BD_RESOLVED(ATD_ARRAY_IDX(attr_idx))) {
                  array_bounds_resolution(attr_idx, &junk);
               }

               exp_desc->assumed_shape = 
                   (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape);
               exp_desc->assumed_size  = 
                   (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Size);

               exp_desc->rank = BD_RANK(ATD_ARRAY_IDX(attr_idx));
               get_shape_from_attr(exp_desc,
                                   attr_idx,
                                   exp_desc->rank,
                                   line,
                                   col);

               /* set contig_array to TRUE even if it is a POINTER */
               /* The a_contig flag in the dope vector will be     */
               /* checked to see if copy in/out is needed.         */

               exp_desc->contig_array = TRUE;
            }

            if (ATD_DISTRIBUTION_IDX(attr_idx) != NULL_IDX &&
                BD_DISTRIBUTE_RESHAPE(ATD_DISTRIBUTION_IDX(attr_idx))) {

               exp_desc->dist_reshape_ref = TRUE;
            }

            if (ATD_IM_A_DOPE(attr_idx) &&
                ! no_sub_or_deref) {

               /* DO NOT SET IR_RANK(dv_idx) */
               /* IT MUST BE ZERO HERE.      */

               NTR_IR_TBL(dv_idx);
               IR_OPR(dv_idx)           = Dv_Deref_Opr;
               IR_LINE_NUM(dv_idx)      = OPND_LINE_NUM((*result_opnd));
               IR_COL_NUM(dv_idx)       = OPND_COL_NUM((*result_opnd));

               IR_TYPE_IDX(dv_idx)	= exp_desc->type_idx;
               IR_FLD_L(dv_idx)         = OPND_FLD((*result_opnd));
               IR_IDX_L(dv_idx)         = OPND_IDX((*result_opnd));
               IR_LINE_NUM_L(dv_idx)    = OPND_LINE_NUM((*result_opnd));
               IR_COL_NUM_L(dv_idx)     = OPND_COL_NUM((*result_opnd));
               OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
               OPND_IDX((*result_opnd)) = dv_idx;
            }

            if (ATD_CLASS(attr_idx) == Constant) {
               exp_desc->constant = TRUE;
               exp_desc->foldable = TRUE;
               exp_desc->will_fold_later = TRUE;

               if (ATD_CONST_IDX(attr_idx) == NULL_IDX) {
                  exp_desc->constant = FALSE;
                  break;
               }

               OPND_IDX((*result_opnd)) = ATD_CONST_IDX(attr_idx);
               OPND_LINE_NUM((*result_opnd)) = line;
               OPND_COL_NUM((*result_opnd))  = col;

               if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
                  OPND_FLD((*result_opnd)) = AT_Tbl_Idx;

                  ADD_TMP_TO_SHARED_LIST(ATD_CONST_IDX(attr_idx));

                  if (insert_subs_ok &&
                      ! no_sub_or_deref) {

# if defined(_TARGET_OS_MAX)
                     if (ATD_ARRAY_IDX(attr_idx) ||
                         ATD_PE_ARRAY_IDX(attr_idx))
# else
                     if (ATD_ARRAY_IDX(attr_idx))
# endif
                                                     {

                        ok &= gen_whole_subscript(result_opnd, exp_desc);
                     }
                     else if (exp_desc->type == Character) {
                        ok &= gen_whole_substring(result_opnd, 0);
                     }
                  }
               }
               else {
                  OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
               }
            }
            else if (ATD_CLASS(attr_idx) == Dummy_Argument &&
                     ATD_SF_DARG(attr_idx))           {

               OPND_FLD((*result_opnd))		= (fld_type) ATD_FLD(attr_idx);
               OPND_IDX((*result_opnd))		= ATD_SF_ARG_IDX(attr_idx);
               OPND_LINE_NUM((*result_opnd))	= line;
               OPND_COL_NUM((*result_opnd))	= col;

               (*exp_desc) = arg_info_list[ATD_SF_LINK(attr_idx)].ed;

               if (OPND_FLD((*result_opnd)) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(OPND_IDX((*result_opnd))) == Data_Obj &&
                   ATD_IM_A_DOPE(OPND_IDX((*result_opnd))) &&
                   ! no_sub_or_deref) {

                  /* DO NOT SET IR_RANK(dv_idx) */
                  /* IT MUST BE ZERO HERE.      */

                  NTR_IR_TBL(dv_idx);
                  IR_OPR(dv_idx)           = Dv_Deref_Opr;
                  IR_LINE_NUM(dv_idx)      = OPND_LINE_NUM((*result_opnd));
                  IR_COL_NUM(dv_idx)       = OPND_COL_NUM((*result_opnd));

                  IR_TYPE_IDX(dv_idx)      = exp_desc->type_idx;
                  COPY_OPND(IR_OPND_L(dv_idx), (*result_opnd));
                  OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
                  OPND_IDX((*result_opnd)) = dv_idx;
               }

               if (OPND_FLD((*result_opnd)) == AT_Tbl_Idx ||
                   (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
                    (IR_OPR(OPND_IDX((*result_opnd))) == Dv_Deref_Opr ||
                     IR_OPR(OPND_IDX((*result_opnd))) == Struct_Opr))) {

                  if (insert_subs_ok &&
                      ! no_sub_or_deref) {

                     if (exp_desc->rank) {
                        ok &= gen_whole_subscript(result_opnd, exp_desc);
                     }
                     else if (exp_desc->type == Character) {
                        ok &= gen_whole_substring(result_opnd, 0);
                     }
                  }
               }
               break;
            }
            else { /* must be variable */

               if (ATD_LCV_IS_CONST(attr_idx)) {
                  exp_desc->will_fold_later = TRUE;
               }

               exp_desc->reference	= TRUE;
               exp_desc->has_symbolic	= ATD_SYMBOLIC_CONSTANT(attr_idx);

               if (insert_subs_ok &&
                   ! no_sub_or_deref) {

# if defined(_TARGET_OS_MAX)
                  if (ATD_ARRAY_IDX(attr_idx) ||
                      ATD_PE_ARRAY_IDX(attr_idx))
# else
                  if (ATD_ARRAY_IDX(attr_idx))
# endif
                                                     {
                     ok &= gen_whole_subscript(result_opnd, exp_desc);
                  }
                  else if (exp_desc->type == Character) {
                     ok &= gen_whole_substring(result_opnd, 0);
                  }
               }
            }


            if (expr_mode == Specification_Expr) {

               /* Only call fnd_semantic_err if there is a problem, to     */
               /* keep things running fast.  There are some problems that  */
               /* fnd_semantic_err won't get.  Issue these msgs here.  To  */
               /* be legal, the data object must be a dummy argument (but  */
               /* not INTENT(OUT) or OPTIONAL), in common, a constant, or  */
               /* host or use associated.                                  */

               switch (ATD_CLASS(attr_idx)) {
               case Dummy_Argument:

                  if (AT_OPTIONAL(attr_idx) ||
                      TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr) {
                     fnd_semantic_err(Obj_Use_Spec_Expr,
                                      line,
                                      col,
                                      attr_idx,
                                      TRUE);
                     ok = FALSE;
                  }
                  else if (ATD_INTENT(attr_idx) == Intent_Out) {
                     PRINTMSG(line, 519, Error, col,
                              AT_OBJ_NAME_PTR(attr_idx));
                     ok = FALSE;
                  }
                  else if (ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
                     PRINTMSG(line, 1439, Error, col,
                              AT_OBJ_NAME_PTR(attr_idx));
                     ok = FALSE;
                  }

                  if (AT_ALT_DARG(attr_idx)) {

                     /* This darg is not at all entry points.  Add to a  */
                     /* list for this specification expression.  This    */
                     /* only happens if there are alternate entry points */
                     /* and bounds expressions.                          */

                     al_list_idx = SCP_TMP_LIST(curr_scp_idx);

                     while (al_list_idx != NULL_IDX &&
                            attr_idx != AL_ATTR_IDX(al_list_idx)) {
                        al_list_idx = AL_NEXT_IDX(al_list_idx);
                     }

                     if (al_list_idx == NULL_IDX) { /* Not on list - add it*/
                        NTR_ATTR_LIST_TBL(al_list_idx);
                        AL_NEXT_IDX(al_list_idx) =SCP_TMP_LIST(curr_scp_idx);
                        AL_ATTR_IDX(al_list_idx) = attr_idx;
                        SCP_TMP_LIST(curr_scp_idx) = al_list_idx;
                     }
                  }
                     
                  break;

               case Variable:
               case Atd_Unknown:

                  if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr) {
                     fnd_semantic_err(Obj_Use_Spec_Expr,
                                      line,
                                      col,
                                      attr_idx,
                                      TRUE);
                     ok = FALSE;
                  }
                  else if (!ATD_IN_COMMON(attr_idx) &&
                           !AT_USE_ASSOCIATED(attr_idx) &&
                           !host_associated &&
                           !ATD_SYMBOLIC_CONSTANT(attr_idx)) { 

                     if (ATD_EQUIV(attr_idx)) {
                        ATD_EQUIV_IN_BNDS_EXPR(attr_idx) = TRUE;
                     }
                     else {

                        if (!AT_DCL_ERR(attr_idx)) {
                           PRINTMSG(line, 521, Error, col,
                                    AT_OBJ_NAME_PTR(attr_idx));
                        }
                        ok = FALSE;
                     }
                  }
                  break;

               case Constant:
               case Struct_Component:
                  break;

               case Function_Result:
               case CRI__Pointee: 
                  fnd_semantic_err(Obj_Use_Spec_Expr,
                                   line,
                                   col,
                                   attr_idx,
                                   TRUE);
                  ok = FALSE;
                  break;
               }  /* End switch */
            }
            else if (expr_mode == Initialization_Expr) {

               if (ATD_CLASS(attr_idx) != Struct_Component &&
                   ! ATD_LCV_IS_CONST(attr_idx)     &&
                   ! ATD_PARENT_OBJECT(attr_idx) &&
                   ATD_CLASS(attr_idx) != Constant) {

                  if (!fnd_semantic_err(Obj_Use_Init_Expr,
                                        line,
                                        col,
                                        attr_idx,
                                        TRUE)) {
                     PRINTMSG(line, 868, Error, col,   /* Must be a constant */
                              AT_OBJ_NAME_PTR(attr_idx));
                     AT_DCL_ERR(attr_idx)	= TRUE;
                  }

                  ok = FALSE;
               }
            }
            break;

         case Pgm_Unit:

            if (ATP_PROC(attr_idx) == Dummy_Proc &&
                ATP_DUMMY_PROC_LINK(attr_idx) != NULL_IDX) {

               attr_idx = ATP_DUMMY_PROC_LINK(attr_idx);
            }

            if (pgm_unit_illegal && !in_call_list) {
               ok = FALSE;

               switch (ATP_PGM_UNIT(attr_idx)) {
                  case Function    :
                     msg_num = 451;
                     break;

                  case Subroutine  :
                     msg_num = 452;
                     break;

                  case Program    :
                     msg_num = 453;
                     break;

                  case Blockdata   :
                     msg_num = 454;
                     break;

                  case Module      :
                     msg_num = 455;
                     break;

                  case Pgm_Unknown :
                     msg_num = 378;
                     break;
               }
               PRINTMSG(line, msg_num, Error, col,
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else if (ATP_PGM_UNIT(attr_idx) == Function) {

               exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));
               exp_desc->type	= TYP_TYPE(exp_desc->type_idx);
               exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

               if (ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx))) {
                  exp_desc->rank=BD_RANK(ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx)));

                  get_shape_from_attr(exp_desc,
                                      ATP_RSLT_IDX(attr_idx),
                                      exp_desc->rank,
                                      line,
                                      col);
               }
               else {
                  exp_desc->rank = 0;
               }
            }
            break;

         case Label:
            if (ATL_CLASS(attr_idx) == Lbl_Construct) {

               /* always an error for a construct name here */

               PRINTMSG(line, 1461, Error, col,
                        AT_OBJ_NAME_PTR(attr_idx));
               ok = FALSE;
            }
            else if (label_allowed) {
               exp_desc->label = TRUE;
            }
            else {
               /* can't have label here */
               PRINTMSG(line, 1462, Error, col,
                        AT_OBJ_NAME_PTR(attr_idx));
               ok = FALSE;
            }
            break;

         case Namelist_Grp:
            if (expr_mode == Specification_Expr) {
               fnd_semantic_err(Obj_Use_Spec_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }
            else if (expr_mode == Initialization_Expr) {
               fnd_semantic_err(Obj_Use_Init_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }
            else if (namelist_illegal) {
               PRINTMSG(line, 512, Error, col,
                        AT_OBJ_NAME_PTR(attr_idx));

               ok = FALSE;
            }
            break;


         case Derived_Type :

            if (!AT_DEFINED(attr_idx)) {

               /* Will not get duplicate messages, because if AT_DCL_ERR */
               /* is TRUE, it will not get here.                         */

               issue_undefined_type_msg(attr_idx, line, col);
               ok = FALSE;
            }
            else if (expr_mode == Specification_Expr) {
               fnd_semantic_err(Obj_Use_Spec_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }
            else if (expr_mode == Initialization_Expr) { 
               fnd_semantic_err(Obj_Use_Init_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }
            break;


         case Interface    :

            if (pgm_unit_illegal) {

               if (in_call_list                        &&
                   ATI_PROC_IDX(attr_idx) != NULL_IDX) {

                  /* change to the specific with same name */
                  attr_idx = ATI_PROC_IDX(attr_idx);
                  OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
                  OPND_IDX((*result_opnd)) = attr_idx;
                  OPND_LINE_NUM((*result_opnd)) = line;
                  OPND_COL_NUM((*result_opnd))  = col;

                  AT_REFERENCED(attr_idx) = (expr_mode == Specification_Expr ||
                                             expr_mode == Stmt_Func_Expr) ?
                                             Dcl_Bound_Ref : Referenced;

                  if (ATP_PGM_UNIT(attr_idx) == Function) {

                     exp_desc->type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));
                     exp_desc->type        = TYP_TYPE(exp_desc->type_idx);
                     exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

                     AT_REFERENCED(ATP_RSLT_IDX(attr_idx)) =
                                                    AT_REFERENCED(attr_idx);

                     if (ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx))) {
                        exp_desc->rank =
                            BD_RANK(ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx)));

                        get_shape_from_attr(exp_desc,
                                            ATP_RSLT_IDX(attr_idx),
                                            exp_desc->rank,
                                            line,
                                            col);
                     }
                     else {
                        exp_desc->rank = 0;
                     }
                  }
               }
               else {
                  /* invalid use of interface */
                  if (!AT_DCL_ERR(attr_idx)) {
                     PRINTMSG(line, 1078, Error, col,
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
                  ok = FALSE;
               }
            }
            else if (expr_mode == Specification_Expr) {
               fnd_semantic_err(Obj_Use_Spec_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }
            else if (expr_mode == Initialization_Expr) { 
               fnd_semantic_err(Obj_Use_Init_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }
            break;

         case Stmt_Func    :

            if (expr_mode == Specification_Expr) {
               fnd_semantic_err(Obj_Use_Spec_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }
            else if (expr_mode == Initialization_Expr) {
               fnd_semantic_err(Obj_Use_Init_Expr,
                                line,
                                col,
                                attr_idx,
                                TRUE);
               ok = FALSE;
            }

            exp_desc->type_idx		= ATD_TYPE_IDX(attr_idx);
            exp_desc->type		= TYP_TYPE(exp_desc->type_idx);
            exp_desc->linear_type	= TYP_LINEAR(exp_desc->type_idx);

            break;

         }
         break;

      case IR_Tbl_Idx :

         namelist_illegal     = TRUE;
         label_allowed        = FALSE;

         ir_idx = OPND_IDX((*result_opnd));

         /* clear rank on the descriptors */
         IR_ARRAY_SYNTAX(ir_idx) = FALSE;

         switch (IR_OPR(ir_idx)) {

            case Null_Opr             :
               break;

            case Defined_Un_Opr       :

               ok = defined_un_opr_handler(result_opnd, exp_desc);
               break;

            case Uplus_Opr            :
            case Uminus_Opr           :

               ok = uplus_opr_handler(result_opnd, exp_desc);
               break;

            case Power_Opr            :

               ok = power_opr_handler(result_opnd, exp_desc);
               break;

            case Mult_Opr             :
            case Div_Opr              :

               ok = mult_opr_handler(result_opnd, exp_desc);
               break;

            case Minus_Opr            :

               ok = minus_opr_handler(result_opnd, exp_desc);
               break;

            case Plus_Opr             :

               ok = plus_opr_handler(result_opnd, exp_desc);
               break;

            case Concat_Opr           :

               ok = concat_opr_handler(result_opnd, exp_desc);
               break;

            case Eq_Opr               :
            case Ne_Opr               :

               ok = eq_opr_handler(result_opnd, exp_desc);
               break;

            case Lg_Opr               :

               ok = lg_opr_handler(result_opnd, exp_desc);
               break;

            case Lt_Opr               :
            case Le_Opr               :
            case Gt_Opr               :
            case Ge_Opr               :

               ok = lt_opr_handler(result_opnd, exp_desc);
               break;

            case Not_Opr              :

               ok = not_opr_handler(result_opnd, exp_desc);
               break;

            case And_Opr              :
            case Or_Opr               :
            case Eqv_Opr              :
            case Neqv_Opr             :

               ok = and_opr_handler(result_opnd, exp_desc);
               break;

            case Defined_Bin_Opr      :

               ok = defined_bin_opr_handler(result_opnd, exp_desc);
               break;

            case Max_Opr              :
            case Min_Opr              :

               ok = max_opr_handler(result_opnd, exp_desc);
               break;

            case Call_Opr             :
               
               if (need_pure_function && 
                   AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Pgm_Unit &&
#ifdef KEY /* Bug 7726 */
	       /* Fortran 95 says every elemental function is a pure function */
                   !(ATP_PURE(IR_IDX_L(ir_idx)) ||
		     ATP_ELEMENTAL(IR_IDX_L(ir_idx))))
#else /* KEY Bug 7726 */
                   !ATP_PURE(IR_IDX_L(ir_idx)))
#endif /* KEY Bug 7726 */
		   {
                  /* KAY - insert call to message here */
                  ok = FALSE;
                  break;
               }

               if (expr_mode == Restricted_Imp_Do_Expr) {
                  PRINTMSG(line, 706, Error, col);
                  ok = FALSE;
                  break;
               }

               save_in_constructor = in_constructor;
               in_constructor = FALSE;

               ok = call_list_semantics(result_opnd, 
                                        exp_desc,
                                        TRUE);

               in_constructor = save_in_constructor;

               if (expr_mode == Data_Stmt_Target_Expr &&
                   !exp_desc->constant) {

                  PRINTMSG(line, 706, Error, col);
                  ok = FALSE;
               }

               break;

            case Struct_Opr           :

               ok = struct_opr_handler(result_opnd, exp_desc, rank_in);
               break;

            case Struct_Construct_Opr :
            case Constant_Struct_Construct_Opr :

               ok = struct_construct_opr_handler(result_opnd, exp_desc);
               break;

            case Array_Construct_Opr :
            case Constant_Array_Construct_Opr :

               ok = array_construct_opr_handler(result_opnd, exp_desc);
               break;

            case Whole_Subscript_Opr  :
            case Section_Subscript_Opr :
            case Subscript_Opr        :

               ok = subscript_opr_handler(result_opnd, exp_desc, rank_in);
               break;

            case Whole_Substring_Opr  :
            case Substring_Opr        :

               ok = substring_opr_handler(result_opnd, exp_desc, rank_in);
               break;

            case Triplet_Opr          :

               ok = triplet_opr_handler(result_opnd, exp_desc);
               break;

            case Dealloc_Obj_Opr      :

               ok = dealloc_obj_opr_handler(result_opnd, exp_desc, rank_in);
               break;

            case Alloc_Obj_Opr        :

               ok = alloc_obj_opr_handler(result_opnd, exp_desc, rank_in);
               break;

            case Cvrt_Opr             :
            case Cvrt_Unsigned_Opr    :

               ok = cvrt_opr_handler(result_opnd, exp_desc);
               break;

            case Paren_Opr            :

               ok = paren_opr_handler(result_opnd, exp_desc);
               break;

            case Kwd_Opr              :
   
               /* must be error in array spec */

               PRINTMSG(IR_LINE_NUM(ir_idx), 197, Error, IR_COL_NUM(ir_idx),
                        ", or )", "=");
               ok = FALSE;
               break;

            case Stmt_Func_Call_Opr   :

               ok = stmt_func_call_opr_handler(result_opnd, exp_desc);
               break;

            case Clen_Opr:

               save_insert_subs_ok = insert_subs_ok;
               insert_subs_ok = FALSE;

               save_in_call_list = in_call_list;

               if (IR_FLD_L(ir_idx) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Pgm_Unit) {
                  in_call_list = TRUE;
               }

               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = expr_sem(&opnd, exp_desc);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
               insert_subs_ok = save_insert_subs_ok;
               in_call_list = save_in_call_list;

               exp_desc->type        = Integer;
               exp_desc->linear_type = INTEGER_DEFAULT_TYPE;
               exp_desc->type_idx    = INTEGER_DEFAULT_TYPE;

               fold_clen_opr(result_opnd, exp_desc);
               break;

            case Percent_Val_Opr :
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = expr_sem(&opnd, exp_desc);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);

               if (OPND_FLD(opnd) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(OPND_IDX(opnd)) == Pgm_Unit) {
                  /* just ignore the %val */
                  COPY_OPND((*result_opnd), opnd);
               }
               else if (exp_desc->rank == 0 &&
                        (exp_desc->type == Integer ||
                         exp_desc->type == Logical ||
                         exp_desc->type == Real)) {

                  COPY_OPND((*result_opnd), opnd);
                  exp_desc->percent_val_arg = TRUE;
               }
               else {
                  PRINTMSG(IR_LINE_NUM(ir_idx), 1125, Error, 
                           IR_COL_NUM(ir_idx));
                  ok = FALSE;
               }
               break;

            /**********************************************************\
            |* These oprs are only seen when we are traversing a tree *|
            |* for the second time in special circumstances.          *|
            \**********************************************************/

            case Dv_Deref_Opr :

               save_no_sub_or_deref = no_sub_or_deref;
               no_sub_or_deref = TRUE;
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = expr_sem(&opnd, exp_desc);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
               no_sub_or_deref = save_no_sub_or_deref;
               break;

            case Dv_Access_Base_Addr:
            case Dv_Access_El_Len:
            case Dv_Access_Assoc:
            case Dv_Access_Ptr_Alloc:
            case Dv_Access_P_Or_A:
            case Dv_Access_A_Contig:
            case Dv_Access_N_Dim:
            case Dv_Access_Typ_Code:
            case Dv_Access_Orig_Base:
            case Dv_Access_Orig_Size:
            case Dv_Access_Low_Bound:
            case Dv_Access_Extent:
            case Dv_Access_Stride_Mult:
               save_no_sub_or_deref = no_sub_or_deref;
               no_sub_or_deref = TRUE;
               exp_desc_l.rank = 0;
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = expr_sem(&opnd, &exp_desc_l);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
               no_sub_or_deref = save_no_sub_or_deref;

               exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
               exp_desc->type        = TYP_TYPE(exp_desc->type_idx);
               exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
               exp_desc->has_symbolic= exp_desc_l.has_symbolic;
               break;

            default :
               save_no_sub_or_deref = no_sub_or_deref;
               no_sub_or_deref = TRUE;
               exp_desc_l.rank = 0;
               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               ok = expr_sem(&opnd, &exp_desc_l);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);

               no_sub_or_deref = TRUE;
               exp_desc_r.rank = 0;
               COPY_OPND(opnd, IR_OPND_R(ir_idx));
               ok = expr_sem(&opnd, &exp_desc_r);
               COPY_OPND(IR_OPND_R(ir_idx), opnd);
               no_sub_or_deref = save_no_sub_or_deref;

               exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
               exp_desc->type        = TYP_TYPE(exp_desc->type_idx);
               exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
               exp_desc->rank        = IR_RANK(ir_idx);
               break;
         }

         break;

      case IL_Tbl_Idx :
         list_idx = OPND_IDX((*result_opnd));
         while (list_idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok = expr_sem(&opnd, &exp_desc_l);
            COPY_OPND(IL_OPND(list_idx), opnd);
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
            
         break;
   }


   TRACE (Func_Exit, "expr_sem", NULL);

   return (ok);

}  /* expr_sem */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	inserts subscript and triplet texts for whole array refs.             *|
|*									      *|
|* Input parameters:							      *|
|*	opnd .. copy of array obj opnd.                                       *|
|*									      *|
|* Output parameters:							      *|
|*      exp_desc .. expression descriptor for opnd. The rank and shape are    *|
|*                  are filled in here.                                       *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

boolean  gen_whole_subscript (opnd_type *opnd, expr_arg_type *exp_desc)

{
   int		attr_idx;
   int		bd_idx;
   int          col;
   int		dv_idx;
   opnd_type	dv_opnd;
   int 		i;
   int          line;
   int		list1_idx = NULL_IDX;
   int		list2_idx;
   expr_arg_type loc_exp_desc;
   int		minus_idx;
   opnd_type	opnd2;
   int		plus_idx;

# if defined(_TARGET_OS_MAX) && defined(_F_MINUS_MINUS)
   int		save_pe_dv_list_idx = NULL_IDX;
# endif

   int		sub_idx;
   boolean      ok = TRUE;
   int		tlst1_idx;
   int		tlst2_idx;
   int		tlst3_idx;
   int		trip_idx;


   TRACE (Func_Entry, "gen_whole_subscript", NULL);

   attr_idx = find_base_attr(opnd, &line, &col);

   bd_idx = ATD_ARRAY_IDX(attr_idx);

   if (bd_idx &&
       BD_ARRAY_CLASS(bd_idx) == Assumed_Size) {

      if (in_call_list) {
         /* it's ok, just don't try to gen the whole subscript */

         if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
            ok = gen_whole_substring(opnd, BD_RANK(bd_idx));
         }
      }
      else {
         /* error .. can't have assumed size here */
         ok = FALSE;

         if (SH_STMT_TYPE(curr_stmt_sh_idx)             == Assignment_Stmt &&
             IR_FLD_L(SH_IR_IDX(curr_stmt_sh_idx))      == AT_Tbl_Idx      &&
             IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx))      == attr_idx        &&
             IR_COL_NUM_L(SH_IR_IDX(curr_stmt_sh_idx))  == col             &&
             IR_LINE_NUM_L(SH_IR_IDX(curr_stmt_sh_idx)) == line)           {
   
            PRINTMSG(line, 411, Error, col);
         }
         else {
            PRINTMSG(line, 412, Error, col);
         }
      }

      goto EXIT;
   }

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx)             = Whole_Subscript_Opr;

# if defined(_TARGET_OS_MAX) &&  defined(_F_MINUS_MINUS)
   if (exp_desc->pe_dim_ref &&
       OPND_FLD((*opnd)) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX((*opnd))) == Subscript_Opr &&
       IR_LIST_CNT_R(OPND_IDX((*opnd))) == 1 &&
       IL_PE_SUBSCRIPT(IR_IDX_R(OPND_IDX((*opnd))))) {

      /* save the pe subscript */
      save_pe_dv_list_idx = IR_IDX_R(OPND_IDX((*opnd)));

      plus_idx = OPND_IDX((*opnd));
      COPY_OPND((*opnd), IR_OPND_L(OPND_IDX((*opnd))));
      FREE_IR_NODE(plus_idx);
   }
# endif

   if (OPND_FLD((*opnd))         == IR_Tbl_Idx    &&
       IR_OPR(OPND_IDX((*opnd))) == Dv_Deref_Opr) {

      COPY_OPND(dv_opnd, IR_OPND_L(OPND_IDX((*opnd))));
   }
   else {
      COPY_OPND(dv_opnd, (*opnd));
   }

   copy_subtree(&dv_opnd, &dv_opnd);

   COPY_OPND(IR_OPND_L(sub_idx), (*opnd));

   /* hook Whole_Subscript text onto *opnd */

   OPND_FLD((*opnd))	= IR_Tbl_Idx;
   OPND_IDX((*opnd))	= sub_idx;

   IR_RANK(sub_idx)	= (bd_idx ? BD_RANK(bd_idx) : 0);
   IR_TYPE_IDX(sub_idx)	= ATD_TYPE_IDX(attr_idx);
   IR_LINE_NUM(sub_idx)	= line;
   IR_COL_NUM(sub_idx)	= col;

   exp_desc->rank	= IR_RANK(sub_idx);

   IR_FLD_R(sub_idx)		= IL_Tbl_Idx;
   IR_LIST_CNT_R(sub_idx)	= IR_RANK(sub_idx);

   for (i = 1 ; i <= IR_LIST_CNT_R(sub_idx); i++) {
      
      /* set up exp_desc->shape */
      if (ATD_IM_A_DOPE(attr_idx)) {
         OPND_FLD(exp_desc->shape[i-1]) = IR_Tbl_Idx;
         NTR_IR_TBL(dv_idx);
         IR_OPR(dv_idx)		= Dv_Access_Extent;
         IR_TYPE_IDX(dv_idx)	= SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(dv_idx)	= line;
         IR_COL_NUM(dv_idx)	= col;
         IR_DV_DIM(dv_idx)	= i;
         COPY_OPND(IR_OPND_L(dv_idx), dv_opnd);
         OPND_IDX(exp_desc->shape[i-1]) = dv_idx;
         SHAPE_FOLDABLE(exp_desc->shape[i-1]) = FALSE;
         SHAPE_WILL_FOLD_LATER(exp_desc->shape[i-1]) = FALSE;
      }
      else {
         OPND_FLD(exp_desc->shape[i-1]) = BD_XT_FLD(bd_idx, i);
         OPND_IDX(exp_desc->shape[i-1]) = BD_XT_IDX(bd_idx, i);

         if (OPND_FLD(exp_desc->shape[i-1]) == AT_Tbl_Idx) {
            ADD_TMP_TO_SHARED_LIST(OPND_IDX(exp_desc->shape[i-1]));
         }

         if (OPND_FLD(exp_desc->shape[i-1]) == CN_Tbl_Idx) {
            SHAPE_FOLDABLE(exp_desc->shape[i-1]) = TRUE;
            SHAPE_WILL_FOLD_LATER(exp_desc->shape[i-1]) = TRUE;
         }
         else if (OPND_FLD(exp_desc->shape[i-1]) == AT_Tbl_Idx &&
                  AT_OBJ_CLASS(OPND_IDX(exp_desc->shape[i-1])) == Data_Obj &&
                  ATD_LCV_IS_CONST(OPND_IDX(exp_desc->shape[i-1]))) {

            SHAPE_FOLDABLE(exp_desc->shape[i-1]) = FALSE;
            SHAPE_WILL_FOLD_LATER(exp_desc->shape[i-1]) = TRUE;
         }
         else {
            SHAPE_FOLDABLE(exp_desc->shape[i-1]) = FALSE;
            SHAPE_WILL_FOLD_LATER(exp_desc->shape[i-1]) = FALSE;
         }
      }

      if (list1_idx == NULL_IDX) {
         NTR_IR_LIST_TBL(list1_idx);
         IR_IDX_R(sub_idx)           = list1_idx;
      }
      else {
         list2_idx = list1_idx;
         NTR_IR_LIST_TBL(list1_idx);
         IL_NEXT_LIST_IDX(list2_idx) = list1_idx;
         IL_PREV_LIST_IDX(list1_idx) = list2_idx;
      }

      IL_FLD(list1_idx)		= IR_Tbl_Idx;
      NTR_IR_TBL(trip_idx);
      IR_OPR(trip_idx)		= Triplet_Opr;
      IR_TYPE_IDX(trip_idx)	= CG_INTEGER_DEFAULT_TYPE;
      IR_RANK(trip_idx)		= 1;
      IR_LINE_NUM(trip_idx)	= line;
      IR_COL_NUM(trip_idx)	= col;
      IL_IDX(list1_idx)		= trip_idx;

      NTR_IR_LIST_TBL(tlst1_idx);
      NTR_IR_LIST_TBL(tlst2_idx);
      NTR_IR_LIST_TBL(tlst3_idx);
      IR_FLD_L(trip_idx)          = IL_Tbl_Idx;
      IR_LIST_CNT_L(trip_idx)     = 3;
      IR_IDX_L(trip_idx)          = tlst1_idx;

      IL_NEXT_LIST_IDX(tlst1_idx) = tlst2_idx;
      IL_PREV_LIST_IDX(tlst2_idx) = tlst1_idx;
      IL_NEXT_LIST_IDX(tlst2_idx) = tlst3_idx;
      IL_PREV_LIST_IDX(tlst3_idx) = tlst2_idx;
      
      if (ATD_IM_A_DOPE(attr_idx)) {

         /* set up first triplet value */

         gen_dv_access_low_bound(&opnd2, &dv_opnd, i);

         COPY_OPND(IL_OPND(tlst1_idx), opnd2);

         /* set up upper bound value */

         NTR_IR_TBL(minus_idx);
         IR_OPR(minus_idx)            = Minus_Opr;
         IR_TYPE_IDX(minus_idx)       = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(minus_idx)       = line;
         IR_COL_NUM(minus_idx)        = col;
         IR_FLD_R(minus_idx)          = CN_Tbl_Idx;
         IR_IDX_R(minus_idx)          = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_R(minus_idx)     = line;
         IR_COL_NUM_R(minus_idx)      = col;

         NTR_IR_TBL(plus_idx);
         IR_OPR(plus_idx)           = Plus_Opr;
         IR_TYPE_IDX(plus_idx)      = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(plus_idx)      = line;
         IR_COL_NUM(plus_idx)       = col;
         IR_FLD_L(minus_idx)          = IR_Tbl_Idx;
         IR_IDX_L(minus_idx)          = plus_idx;

         gen_dv_access_low_bound(&opnd2, &dv_opnd, i);

         COPY_OPND(IR_OPND_R(plus_idx), opnd2);

         NTR_IR_TBL(dv_idx);
         IR_OPR(dv_idx)              = Dv_Access_Extent;
         IR_TYPE_IDX(dv_idx)         = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(dv_idx)         = line;
         IR_COL_NUM(dv_idx)          = col;
         IR_DV_DIM(dv_idx)           = i;
         COPY_OPND(IR_OPND_L(dv_idx), dv_opnd);

         IR_FLD_L(plus_idx)         = IR_Tbl_Idx;
         IR_IDX_L(plus_idx)         = dv_idx;

         IL_FLD(tlst2_idx)           = IR_Tbl_Idx;
         IL_IDX(tlst2_idx)           = minus_idx;
         
      }
      else {
         IL_FLD(tlst1_idx)      = BD_LB_FLD(bd_idx, i);
         IL_IDX(tlst1_idx)      = BD_LB_IDX(bd_idx, i);
         IL_LINE_NUM(tlst1_idx) = line;
         IL_COL_NUM(tlst1_idx)  = col;

         if (IL_FLD(tlst1_idx) == AT_Tbl_Idx) {
            ADD_TMP_TO_SHARED_LIST(IL_IDX(tlst1_idx));
         }

         if (IL_FLD(tlst1_idx) != CN_Tbl_Idx) {

            /* assumes that this is an AT_Tbl_Idx */
            loc_exp_desc.type_idx = ATD_TYPE_IDX(IL_IDX(tlst1_idx));
            loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
            loc_exp_desc.linear_type =
                                 TYP_LINEAR(loc_exp_desc.type_idx);
         }
         else {
            loc_exp_desc.type_idx = CN_TYPE_IDX(IL_IDX(tlst1_idx));
            loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
            loc_exp_desc.linear_type =
                                 TYP_LINEAR(loc_exp_desc.type_idx);
         }

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
         if (in_io_list) {

            /* on mpp, must cast shorts to longs in io lists */
            /* on solaris, must cast Integer_8 to Integer_4 */

            COPY_OPND(opnd2, IL_OPND(tlst1_idx));
            cast_to_cg_default(&opnd2, &loc_exp_desc);
            COPY_OPND(IL_OPND(tlst1_idx), opnd2);
         }
#endif /* KEY Bug 4709 */

         IL_FLD(tlst2_idx)      = BD_UB_FLD(bd_idx, i);
         IL_IDX(tlst2_idx)      = BD_UB_IDX(bd_idx, i);
         IL_LINE_NUM(tlst2_idx) = line;
         IL_COL_NUM(tlst2_idx)  = col;

         if (IL_FLD(tlst2_idx) == AT_Tbl_Idx) {
            ADD_TMP_TO_SHARED_LIST(IL_IDX(tlst2_idx));
         }

         if (IL_FLD(tlst2_idx) != CN_Tbl_Idx) {

            /* assumes that this is an AT_Tbl_Idx */
            loc_exp_desc.type_idx = ATD_TYPE_IDX(IL_IDX(tlst2_idx));
            loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
            loc_exp_desc.linear_type =
                                 TYP_LINEAR(loc_exp_desc.type_idx);
         }
         else {
            loc_exp_desc.type_idx = CN_TYPE_IDX(IL_IDX(tlst2_idx));
            loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
            loc_exp_desc.linear_type =
                                 TYP_LINEAR(loc_exp_desc.type_idx);
         }

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
         if (in_io_list) {

            /* on mpp, must cast shorts to longs in io lists */
            /* on solaris, must cast Integer_8 to Integer_4 */

            COPY_OPND(opnd2, IL_OPND(tlst2_idx));
            cast_to_cg_default(&opnd2, &loc_exp_desc);
            COPY_OPND(IL_OPND(tlst2_idx), opnd2);
         }
#endif /* KEY Bug 4709 */
      }

      IL_FLD(tlst3_idx)      = CN_Tbl_Idx;
      IL_LINE_NUM(tlst3_idx) = line;
      IL_COL_NUM(tlst3_idx)  = col;
      IL_IDX(tlst3_idx)      = CN_INTEGER_ONE_IDX;
   }

# if defined(_TARGET_OS_MAX)

# ifdef _F_MINUS_MINUS
   if (save_pe_dv_list_idx != NULL_IDX) {

      /* add the pe subscript to ir_idx */
      list1_idx = IR_IDX_R(sub_idx);

      while (IL_NEXT_LIST_IDX(list1_idx)) {
         list1_idx = IL_NEXT_LIST_IDX(list1_idx);
      }

      IL_NEXT_LIST_IDX(list1_idx) = save_pe_dv_list_idx;
      IL_PREV_LIST_IDX(save_pe_dv_list_idx) = list1_idx;
      IR_LIST_CNT_R(sub_idx) += 1;
   }
   else if (ATD_PE_ARRAY_IDX(attr_idx) &&
            ! ATD_ALLOCATABLE(attr_idx)) {
      /* supply mype() as pe dim */

      list1_idx = IR_IDX_R(sub_idx);

      if (list1_idx) {
         while (IL_NEXT_LIST_IDX(list1_idx) != NULL_IDX) {
            list1_idx = IL_NEXT_LIST_IDX(list1_idx);
         }

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list1_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list1_idx)) = list1_idx;
         list1_idx = IL_NEXT_LIST_IDX(list1_idx);
         IR_LIST_CNT_R(sub_idx) += 1;
      }
      else {
         NTR_IR_LIST_TBL(list1_idx);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_LIST_CNT_R(sub_idx) = 1;
         IR_IDX_R(sub_idx) = list1_idx;

         IR_OPR(sub_idx) = Subscript_Opr;
      }

      NTR_IR_TBL(plus_idx);
      IR_OPR(plus_idx) = My_Pe_Opr;
      IR_TYPE_IDX(plus_idx) = INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(plus_idx) = IR_LINE_NUM(sub_idx);
      IR_COL_NUM(plus_idx) = IR_COL_NUM(sub_idx);

      IL_FLD(list1_idx) = IR_Tbl_Idx;
      IL_IDX(list1_idx) = plus_idx;

      IL_PE_SUBSCRIPT(list1_idx) = TRUE;
      io_item_must_flatten = TRUE;
   }
# endif
# endif

   if (ok && 
       TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
      ok = gen_whole_substring(opnd, IR_RANK(sub_idx));
   }

   IR_ARRAY_SYNTAX(sub_idx) = FALSE;

EXIT:

   TRACE (Func_Exit, "gen_whole_subscript", NULL);

   return(ok);

}  /* gen_whole_subscript */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      inserts substring texts and bounds for whole character refs.          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      opnd .. copy of array obj opnd.                                       *|
|*      rank .. rank of opnd, it is placed on substring opr.                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if no problem                                                    *|
|*                                                                            *|
\******************************************************************************/

boolean  gen_whole_substring (opnd_type *opnd,
                              int        rank)

{
   int          attr_idx;
   int		clen_idx;
   int          col;
   int		ir_idx;
   int          line;
   int		list_idx;
   int          list1_idx;
   int          list2_idx;
   int		shift_idx;
   int          sub_idx;
   boolean      ok = TRUE;


   TRACE (Func_Entry, "gen_whole_substring", NULL);

   /* what do we do with assumed size character? */

   attr_idx = find_base_attr(opnd, &line, &col);

   NTR_IR_TBL(sub_idx);

   COPY_OPND(IR_OPND_L(sub_idx), (*opnd));

   IR_OPR(sub_idx)		= Whole_Substring_Opr;
   IR_RANK(sub_idx)		= rank;
   IR_TYPE_IDX(sub_idx)		= ATD_TYPE_IDX(attr_idx);
   IR_LINE_NUM(sub_idx)		= line;
   IR_COL_NUM(sub_idx)		= col;

   OPND_FLD((*opnd))		= IR_Tbl_Idx;
   OPND_IDX((*opnd))		= sub_idx;

   IR_FLD_R(sub_idx)		= IL_Tbl_Idx;
   IR_LIST_CNT_R(sub_idx)	= 2;

   NTR_IR_LIST_TBL(list1_idx);
   IR_IDX_R(sub_idx) = list1_idx;
   IL_FLD(list1_idx) = CN_Tbl_Idx;
   IL_IDX(list1_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list1_idx) = line;
   IL_COL_NUM(list1_idx)  = col;

   NTR_IR_LIST_TBL(list2_idx);
   IL_NEXT_LIST_IDX(list1_idx)	= list2_idx;
   IL_PREV_LIST_IDX(list2_idx)	= list1_idx;

   if (ATD_CLASS(attr_idx)                    == CRI__Pointee &&
       TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == Assumed_Size_Char){

      NTR_IR_TBL(clen_idx);
      IR_OPR(clen_idx)        = Clen_Opr;
      IR_TYPE_IDX(clen_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(clen_idx)   = line;
      IR_COL_NUM(clen_idx)    = col;
      IR_FLD_L(clen_idx)      = AT_Tbl_Idx;
      IR_IDX_L(clen_idx)      = attr_idx;
      IR_LINE_NUM_L(clen_idx) = line;
      IR_COL_NUM_L(clen_idx)  = col;
      IL_FLD(list2_idx)       = IR_Tbl_Idx;
      IL_IDX(list2_idx)       = clen_idx;
   }
   else if (ATD_CHAR_LEN_IN_DV(attr_idx)) {
      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)           = Dv_Access_El_Len;
      IR_TYPE_IDX(ir_idx)      = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)      = line;
      IR_COL_NUM(ir_idx)       = col;
      IR_FLD_L(ir_idx)         = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)         = attr_idx;
      IR_LINE_NUM_L(ir_idx)    = line;
      IR_COL_NUM_L(ir_idx)     = col;

      if (char_len_in_bytes) {
         /* Len in dope vector is in bytes for solaris */
         IL_FLD(list2_idx) = IR_Tbl_Idx;
         IL_IDX(list2_idx) = ir_idx;
      }
      else {
         NTR_IR_TBL(shift_idx);
         IR_OPR(shift_idx)        = Shiftr_Opr;
         IR_TYPE_IDX(shift_idx)   = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(shift_idx)   = line;
         IR_COL_NUM(shift_idx)    = col;

         NTR_IR_LIST_TBL(list_idx);

         IR_FLD_L(shift_idx)      = IL_Tbl_Idx;
         IR_IDX_L(shift_idx)      = list_idx;
         IR_LIST_CNT_L(shift_idx) = 2;
         IL_FLD(list_idx)         = IR_Tbl_Idx;
         IL_IDX(list_idx)         = ir_idx;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         IL_FLD(list_idx)         = CN_Tbl_Idx;
         IL_LINE_NUM(list_idx)    = line;
         IL_COL_NUM(list_idx)     = col;
         IL_IDX(list_idx)	  = CN_INTEGER_THREE_IDX;
         IL_FLD(list2_idx)	  = IR_Tbl_Idx;
         IL_IDX(list2_idx)	  = shift_idx;
      }
   }
   else {
      IL_IDX(list2_idx) 	= TYP_IDX(ATD_TYPE_IDX(attr_idx));
      IL_FLD(list2_idx)		= TYP_FLD(ATD_TYPE_IDX(attr_idx));
      IL_LINE_NUM(list2_idx)	= line;
      IL_COL_NUM(list2_idx)	= col;

      if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(IL_IDX(list2_idx));
      }
   }

   add_substring_length(sub_idx);

   IR_ARRAY_SYNTAX(sub_idx) = FALSE;

   TRACE (Func_Exit, "gen_whole_substring", NULL);

   return(ok);

}  /* gen_whole_substring */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine accesses the semantic tables for any operator            *|
|*      to see if the operation (or assignment) is intrinsic.                 *|
|*									      *|
|* Input parameters:							      *|
|*	opr		- operator_type                                       *|
|*      type_idx_l	- type of left operand                                *|
|*      rank_l		- rank of left operand                                *|
|*      type_idx_r	- type of right operand                               *|
|*      rank_r		- rank of right operand                               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if operation is intrinsic.                                       *|
|*									      *|
\******************************************************************************/

boolean  operation_is_intrinsic(operator_type   opr,
				int		type_idx_l,
                                int             rank_l,
				int		type_idx_r,
                                int             rank_r)

{
   linear_type_type	exp_idx_l;
   linear_type_type	exp_idx_r;
   boolean		intrinsic	= TRUE;
   basic_type_type	type_l;
   basic_type_type	type_r;


   TRACE (Func_Entry, "operation_is_intrinsic", NULL);

   if (opr == Null_Opr) {
      intrinsic = FALSE;
      goto EXIT;
   }

   type_l	= TYP_TYPE(type_idx_l);
   type_r	= TYP_TYPE(type_idx_r);
   exp_idx_l	= TYP_LINEAR(type_idx_l);
   exp_idx_r	= TYP_LINEAR(type_idx_r);

   if (type_r != Typeless) {

      if (opr == Asg_Opr) {
   
         if (rank_l != rank_r &&
             rank_r != 0) {
            /* not intrinsic */
            intrinsic = FALSE;
            goto EXIT;
         }
      }
      else {
   
         if (rank_l != rank_r &&
             rank_l * rank_r != 0) {
            /* not intrinsic */
            intrinsic = FALSE;
            goto EXIT;
         }
      }
   }

   switch (opr) {
      case Plus_Opr :

         if (type_r == Typeless) {

            if (UN_PLUS_TYPE(exp_idx_l) == Err_Res ||
                UN_PLUS_EXTN(exp_idx_l)) {
               intrinsic = FALSE;
            }
         }
         else {
            if (BIN_ADD_TYPE(exp_idx_l, exp_idx_r) == Err_Res ||
                BIN_ADD_EXTN(exp_idx_l, exp_idx_r)) {
               intrinsic = FALSE;
            }
         }
         break;

      case Minus_Opr :

         if (type_r == Typeless) {

            if (UN_PLUS_TYPE(exp_idx_l) == Err_Res ||
                UN_PLUS_EXTN(exp_idx_l)) {
               intrinsic = FALSE;
            }
         }
         else {
            if (BIN_SUB_TYPE(exp_idx_l, exp_idx_r) == Err_Res ||
                BIN_SUB_EXTN(exp_idx_l, exp_idx_r)) {
               intrinsic = FALSE;
            }
         }
         break;

      case Power_Opr :

         if (POWER_TYPE(exp_idx_l, exp_idx_r) == Err_Res ||
             POWER_EXTN(exp_idx_l, exp_idx_r)) {
            intrinsic = FALSE;
         }
         break;

      case Div_Opr :
      case Mult_Opr :

         if (MULT_DIV_TYPE(exp_idx_l, exp_idx_r) == Err_Res ||
             MULT_DIV_EXTN(exp_idx_l, exp_idx_r)) {
            intrinsic = FALSE;
         }
         break;

      case Concat_Opr :

         if (type_l != Character || type_r != Character) {
            intrinsic = FALSE;
         }
         break;

      case Eq_Opr :
# ifndef KEY
      case Ge_Opr :
# endif

         if (EQ_NE_TYPE(exp_idx_l, exp_idx_r) == Err_Res ||
             EQ_NE_EXTN(exp_idx_l, exp_idx_r)) {
            intrinsic = FALSE;
         }
         break;
// Bug 2236
# ifdef KEY
      case Ge_Opr :
# endif
      case Gt_Opr :
      case Le_Opr :
      case Lt_Opr :
      case Ne_Opr :

         if (GT_LT_TYPE(exp_idx_l, exp_idx_r) == Err_Res ||
             GT_LT_EXTN(exp_idx_l, exp_idx_r)) {
            intrinsic = FALSE;
         }
         break;

      case And_Opr :
      case Eqv_Opr :
      case Neqv_Opr :
      case Or_Opr :

         if (AND_OR_TYPE(exp_idx_l, exp_idx_r) == Err_Res ||
             AND_OR_EXTN(exp_idx_l, exp_idx_r)) {
            intrinsic = FALSE;
         }
         break;

      case Not_Opr :

         if (NOT_TYPE(exp_idx_l) == Err_Res ||
             NOT_EXTN(exp_idx_l)) {
            intrinsic = FALSE;
         }
         break;

      case Asg_Opr :

         if (ASG_TYPE(exp_idx_l, exp_idx_r) == Err_Res        ||
             ASG_TYPE(exp_idx_l, exp_idx_r) == Structure_Type ||
             ASG_EXTN(exp_idx_l, exp_idx_r)) {
            intrinsic = FALSE;
         }
         break;
   }



EXIT:

   TRACE (Func_Exit, "operation_is_intrinsic", NULL);

   return(intrinsic);

}  /* operation_is_intrinsic */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine takes two constant table indexes and applies the         *|
|*      relational operator to them and returns the boolean result.           *|
|*      It uses the fortran folders and assumes that the input indexes are    *|
|*      constant table indexes. Big trouble could result if they are not.     *|
|*      It issues internal errors if the operator is not a relational or if   *|
|*      the types of the operands are invalid.                                *|
|*									      *|
|* Input parameters:							      *|
|*	idx_1, idx_2 - the two constant table indexes.                        *|
|*      opr          - the operator to use.                                   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The result of the fold.                                               *|
|*									      *|
\******************************************************************************/

boolean fold_relationals(int		idx_1,
		  	 int		idx_2,
			 operator_type  opr)

{
   long_type	        folded_const[MAX_WORDS_FOR_NUMERIC];
   boolean		ok;
   int			unused;


   TRACE (Func_Entry, "fold_relationals", NULL);

   switch (opr) {
      case Eq_Opr:
      case Ne_Opr:
      case Lt_Opr:
      case Le_Opr:
      case Gt_Opr:
      case Ge_Opr:

         unused = CG_LOGICAL_DEFAULT_TYPE;

         ok = folder_driver((char *)&CN_CONST(idx_1),
                            CN_TYPE_IDX(idx_1),
                           (char *)&CN_CONST(idx_2),
                            CN_TYPE_IDX(idx_2),
                           folded_const,
                           &unused,
                            stmt_start_line,
                            stmt_start_col,
                            2,
                            opr);

         break;

      default : 
         PRINTMSG(stmt_start_line, 251, Internal, stmt_start_col);
         break;

   }


   TRACE (Func_Exit, "fold_relationals", NULL);

   return(THIS_IS_TRUE(folded_const,unused));

}  /* fold_relationals */

# ifdef KEY
static boolean is_loop_index_of_forall_loop (int ir_idx)
{
  int stmt_sh_idx, forall_list_idx, index_idx;
  boolean found = FALSE;

  stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
  while (stmt_sh_idx != NULL_IDX){
    if ( IR_OPR(SH_IR_IDX(stmt_sh_idx)) == Forall_Opr ){
      found = TRUE;
      break;
    }
    else if (SH_STMT_TYPE(stmt_sh_idx) == End_Forall_Stmt)
      break;
    stmt_sh_idx = SH_PREV_IDX(stmt_sh_idx);
  }

  if (!found)
    return FALSE;

  forall_list_idx = IR_IDX_R(SH_IR_IDX(stmt_sh_idx));
  index_idx = IL_IDX(forall_list_idx);

  if (IL_FLD(index_idx) == AT_Tbl_Idx){
    index_idx = IL_IDX(index_idx);
    ir_idx = IL_IDX(ir_idx);
    if (AT_ATTR_LINK(index_idx) == ir_idx)
      return TRUE;
  }
  return FALSE;
}
static int ir_contains_variable_subscript(int ir_idx, int pos)
{
  int ret_idx = NULL_IDX;
  int attr_bd_idx;

  if (IR_OPR(ir_idx) == Subscript_Opr)
  {
    int arg_idx = IR_IDX_R(ir_idx);
    for (int index = 1; index < pos; index++)
      arg_idx = IL_NEXT_LIST_IDX(arg_idx);
    if (IL_FLD(arg_idx) == AT_Tbl_Idx){
      int array_idx = IR_IDX_L(ir_idx);
      if (IR_FLD_L(ir_idx) == AT_Tbl_Idx &&
          is_loop_index_of_forall_loop(arg_idx)){
        attr_bd_idx =  ATD_ARRAY_IDX(array_idx);
        if (BD_LB_FLD(attr_bd_idx, pos) == CN_Tbl_Idx &&
          BD_UB_FLD(attr_bd_idx, pos) == CN_Tbl_Idx &&
          BD_SM_FLD(attr_bd_idx, pos) == CN_Tbl_Idx)
          return ir_idx;
      }
    }
  }
  else{
    if (IR_FLD_L(ir_idx) == IR_Tbl_Idx){
      ret_idx = ir_contains_variable_subscript(IR_IDX_L(ir_idx), pos);
      if (ret_idx != NULL_IDX)
        return ret_idx;
     }

    if (IR_FLD_R(ir_idx) == IR_Tbl_Idx){
      ret_idx = ir_contains_variable_subscript(IR_IDX_R(ir_idx), pos);
      if (ret_idx != NULL_IDX)
        return ret_idx;
    }
  }
  return ret_idx;
}
static void change_extent(opnd_type          *result_opnd)
{
  int subscript_idx, list_idx_11, list_idx_12, list_idx_13,list_idx_14, list_idx_15, list_idx_16, maxval_idx, arg_idx, triplet_idx, attr_bd_idx, i, line, col;

  i = 1;
  line = OPND_LINE_NUM((*result_opnd));
  col = OPND_COL_NUM((*result_opnd));
  subscript_idx = NULL_IDX;
  if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx)
    subscript_idx = ir_contains_variable_subscript(OPND_IDX((*result_opnd)),i);
  if (subscript_idx != NULL_IDX)
  {

    NTR_IR_LIST_TBL(list_idx_11);
    maxval_idx = gen_ir(IL_Tbl_Idx, list_idx_11, Maxval_Opr, SA_INTEGER_DEFAULT_TYPE, line, col, NO_Tbl_Idx, NULL_IDX);
    IR_RANK(maxval_idx) = 0;
    IR_LIST_CNT_L(maxval_idx) = 3;
    IR_LIST_CNT_R(maxval_idx) = 0;

    IL_FLD(list_idx_11) = IR_Tbl_Idx;
    IL_ARG_DESC_VARIANT(list_idx_11) = TRUE;
    IL_LINE_NUM(list_idx_11) = line;
    IL_COL_NUM(list_idx_11)  = col;
    IL_IDX(list_idx_11) = OPND_IDX((*result_opnd));

    IR_OPR(subscript_idx) = Whole_Subscript_Opr;
    arg_idx = IR_IDX_R(subscript_idx);
    for (int index = 1; index < i; index++)
      arg_idx = IL_NEXT_LIST_IDX(arg_idx);

    attr_bd_idx =  ATD_ARRAY_IDX(IR_IDX_L(subscript_idx));

    NTR_IR_LIST_TBL(list_idx_14);
    triplet_idx = gen_ir(IL_Tbl_Idx, list_idx_14, Triplet_Opr, SA_INTEGER_DEFAULT_TYPE, line, col, NO_Tbl_Idx, NULL_IDX);
    IR_LIST_CNT_L(triplet_idx) = 3;

    IL_FLD(arg_idx) = IR_Tbl_Idx;
    IL_IDX(arg_idx) = triplet_idx;
    IL_LINE_NUM(arg_idx) = line;
    IL_COL_NUM(arg_idx)  = col;

    IL_ARG_DESC_VARIANT(list_idx_14) = TRUE;
    IL_LINE_NUM(list_idx_14) = line;
    IL_COL_NUM(list_idx_14)  = col;
    IL_FLD(list_idx_14) = BD_LB_FLD(attr_bd_idx, i);
    IL_IDX(list_idx_14) = BD_LB_IDX(attr_bd_idx, i);

    NTR_IR_LIST_TBL(list_idx_15);
    IL_ARG_DESC_VARIANT(list_idx_15) = TRUE;
    IL_LINE_NUM(list_idx_15) = line;
    IL_COL_NUM(list_idx_15)  = col;
    IL_FLD(list_idx_15) = BD_UB_FLD(attr_bd_idx, i);
    IL_IDX(list_idx_15) = BD_UB_IDX(attr_bd_idx, i);
    IL_NEXT_LIST_IDX(list_idx_14) = list_idx_15;
                       
    NTR_IR_LIST_TBL(list_idx_16);
    IL_ARG_DESC_VARIANT(list_idx_16) = TRUE;
    IL_LINE_NUM(list_idx_16) = line;
    IL_COL_NUM(list_idx_16)  = col;
    IL_FLD(list_idx_16) = BD_XT_FLD(attr_bd_idx, i);
    IL_IDX(list_idx_16) = C_INT_TO_CN(SA_INTEGER_DEFAULT_TYPE, CN_INT_TO_C(BD_UB_IDX(attr_bd_idx,i))/CN_INT_TO_C(BD_XT_IDX(attr_bd_idx,i)));
    IL_NEXT_LIST_IDX(list_idx_15) = list_idx_16;
                       
    IL_NEXT_LIST_IDX(list_idx_16) = NULL_IDX;

    NTR_IR_LIST_TBL(list_idx_12);
    IL_FLD(list_idx_12) = CN_Tbl_Idx;
    IL_IDX(list_idx_12) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);;
    IL_ARG_DESC_VARIANT(list_idx_12) = TRUE;
    IL_LINE_NUM(list_idx_12) = line;
    IL_COL_NUM(list_idx_12)  = col;
    IL_NEXT_LIST_IDX(list_idx_11) = list_idx_12;

    NTR_IR_LIST_TBL(list_idx_13);
    IL_FLD(list_idx_13) = NO_Tbl_Idx;
    IL_LINE_NUM(list_idx_13) = line;
    IL_COL_NUM(list_idx_13)  = col;
    IL_ARG_DESC_VARIANT(list_idx_13) = TRUE;
    IL_NEXT_LIST_IDX(list_idx_12) = list_idx_13;
    IL_NEXT_LIST_IDX(list_idx_13) = NULL_IDX;
                      
    OPND_IDX((*result_opnd)) = maxval_idx;
  }
}
#endif
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the expression for the extent of an array section.             *|
|*									      *|
|* Input parameters:							      *|
|*      list_idx - IL_Tbl_Idx, points to start value, linked to end and stride*|
|*									      *|
|* Output parameters:							      *|
|*	opnd - opnd_type, this is the result expression.                      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void	make_triplet_extent_tree(opnd_type	*opnd,
				 int		list_idx)

{
   int		       col;
   int		       div_idx;
   expr_arg_type       exp_desc;
   boolean	       foldable = TRUE;
   int		       line;
   int		       plus_idx;
   int		       list_idx2;
   int                 max_idx;
   expr_mode_type      save_expr_mode;
   cif_usage_code_type save_xref_state;
   int		       sub_idx;
   opnd_type	       topnd;
   boolean             unused;
   boolean	       will_fold_later = TRUE;


   TRACE (Func_Entry, "make_triplet_extent_tree", NULL);

   find_opnd_line_and_column(opnd, &line, &col);

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx)		= Plus_Opr;
   IR_TYPE_IDX(plus_idx)	= CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx)        = line;
   IR_COL_NUM(plus_idx)         = col;

   NTR_IR_TBL(div_idx);
   IR_OPR(div_idx)		= Div_Opr;
   IR_TYPE_IDX(div_idx)		= CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(div_idx)         = line;
   IR_COL_NUM(div_idx)          = col;

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx)		= Minus_Opr;
   IR_TYPE_IDX(sub_idx)		= CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(sub_idx)         = line;
   IR_COL_NUM(sub_idx)          = col;

   NTR_IR_TBL(max_idx);
   IR_OPR(max_idx)              = Max_Opr;
   IR_TYPE_IDX(max_idx)         = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(max_idx)         = line;
   IR_COL_NUM(max_idx)          = col;

   
   OPND_FLD((*opnd))		= IR_Tbl_Idx;
   OPND_IDX((*opnd))		= max_idx;

   NTR_IR_LIST_TBL(list_idx2);
   IR_FLD_L(max_idx) 		= IL_Tbl_Idx;
   IR_LIST_CNT_L(max_idx)	= 2;
   IR_IDX_L(max_idx)		= list_idx2;

   IL_FLD(list_idx2)             = IR_Tbl_Idx;
   IL_IDX(list_idx2)             = div_idx;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
   list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

   IL_FLD(list_idx2) 		= CN_Tbl_Idx;
   IL_IDX(list_idx2)  		= CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx2)	= line;
   IL_COL_NUM(list_idx2) 	= col;

   IR_FLD_L(div_idx)            = IR_Tbl_Idx;
   IR_IDX_L(div_idx)            = plus_idx;

   IR_FLD_L(plus_idx)		= IR_Tbl_Idx;
   IR_IDX_L(plus_idx)		= sub_idx;
   
   /* start */
   COPY_OPND(topnd, IL_OPND(list_idx));
   copy_subtree(&topnd, &topnd);
   COPY_OPND(IR_OPND_R(sub_idx), topnd);

   foldable = foldable && (IL_FLD(list_idx) == CN_Tbl_Idx ||
                           SHAPE_FOLDABLE(IL_OPND(list_idx)));
   will_fold_later = will_fold_later && 
                     SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx));

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /* end */
   COPY_OPND(topnd, IL_OPND(list_idx));
   copy_subtree(&topnd, &topnd);
// Bug 2364
# ifdef KEY
   change_extent(&topnd);
# endif
   COPY_OPND(IR_OPND_L(sub_idx), topnd);

   foldable = foldable && (IL_FLD(list_idx) == CN_Tbl_Idx ||
                           SHAPE_FOLDABLE(IL_OPND(list_idx)));
   will_fold_later = will_fold_later && 
                     SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx));

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /* stride */
   COPY_OPND(topnd, IL_OPND(list_idx));
   copy_subtree(&topnd, &topnd);
   COPY_OPND(IR_OPND_R(div_idx), topnd);

   COPY_OPND(topnd, IL_OPND(list_idx));
   copy_subtree(&topnd, &topnd);
   COPY_OPND(IR_OPND_R(plus_idx), topnd);

   foldable = foldable && (IL_FLD(list_idx) == CN_Tbl_Idx ||
                           SHAPE_FOLDABLE(IL_OPND(list_idx)));
   will_fold_later = will_fold_later && 
                     SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx));

   if (foldable) {
      save_xref_state = xref_state;
      xref_state      = CIF_No_Usage_Rec;
      save_expr_mode  = expr_mode;
      expr_mode       = Regular_Expr;

      exp_desc.rank   = 0;
      unused = expr_semantics(opnd, &exp_desc);
      xref_state = save_xref_state;
      expr_mode  = save_expr_mode;

      SHAPE_FOLDABLE((*opnd))         = exp_desc.foldable;
      SHAPE_WILL_FOLD_LATER((*opnd))  = exp_desc.will_fold_later;
   }
   else {
      SHAPE_FOLDABLE((*opnd))         = foldable;
      SHAPE_WILL_FOLD_LATER((*opnd))  = will_fold_later;
   }


   TRACE (Func_Exit, "make_triplet_extent_tree", NULL);

   return;

}  /* make_triplet_extent_tree */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine provides an interface into the assignment semantics      *|
|*      table. It is provided for parameter and data stmt semantic checking.  *|
|*      The "right hand side" is assumed to be a constant. Rank is not checked*|
|*      If the types and aux types combination is allowed TRUE is returned,   *|
|*      else FALSE.                                                           *|
|*									      *|
|* Input parameters:							      *|
|*	l_type		type index of left hand side.                         *|
|*	r_type		type index of right hand side.                        *|
|*      line, col       line and col to use for messages.                     *|
|*                      if line == -1, don't issue message.                   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if assignment is allowed, FALSE otherwise.                       *|
|*									      *|
\******************************************************************************/

boolean check_asg_semantics(int		l_new_type_idx,
                            int		r_new_type_idx,
                            int		line,
                            int		col)

{
   boolean		correct		= TRUE;
   linear_type_type	exp_idx_l;
   linear_type_type	exp_idx_r;


   TRACE (Func_Entry, "check_asg_semantics", NULL);

   exp_idx_l = TYP_LINEAR(l_new_type_idx);
   exp_idx_r = TYP_LINEAR(r_new_type_idx);

   if (TYP_TYPE(r_new_type_idx) == Character &&
       compare_cn_and_value(TYP_IDX(r_new_type_idx),
                            MAX_CHARS_IN_TYPELESS, 
                            Le_Opr)) {
      exp_idx_r = Short_Char_Const;
   }

   if (ASG_TYPE(exp_idx_l, exp_idx_r) == Err_Res) {
      correct = FALSE;
   }
   else if (ASG_TYPE(exp_idx_l, exp_idx_r) == Structure_Type &&
            !compare_derived_types(l_new_type_idx, r_new_type_idx)) {
      correct = FALSE;
   }

   if (correct                               &&
       ASG_EXTN(exp_idx_l, exp_idx_r)        &&
       TYP_TYPE(r_new_type_idx) == Character &&
       line != -1)                           {

      PRINTMSG(line, 161, Ansi, col);
   }

   TRACE (Func_Exit, "check_asg_semantics", NULL);

   return(correct);

}  /* check_asg_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Creates a whole dope vector copy for pointer assignment from a pointer*|
|*									      *|
|* Input parameters:							      *|
|*	l_opnd - left hand side of ptr assignment.                            *|
|*      r_opnd - right hand side of ptr assignment.                           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void ptr_assign_from_ptr(opnd_type	*l_opnd,
		         opnd_type	*r_opnd)

{
   int          	column;
   int			dv_idx;
   int          	line;
   sh_position_type	location;
   opnd_type    	opnd;


   TRACE (Func_Entry, "ptr_assign_from_ptr", NULL);

   location = (SH_LABELED(curr_stmt_sh_idx)) ? After : Before;


   /**********************************\
   |* VECTOR COPY WHOLE DOPE VECTOR. *|
   \**********************************/

   NTR_IR_TBL(dv_idx);

   IR_OPR(dv_idx) = Dv_Whole_Copy_Opr;
   IR_TYPE_IDX(dv_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = stmt_start_line;
   IR_COL_NUM(dv_idx)  = stmt_start_col;

   COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

   COPY_OPND(opnd, (*r_opnd));

   if (OPND_FLD(opnd) == IR_Tbl_Idx) {

      while (OPND_FLD(opnd) == IR_Tbl_Idx) {
         if (IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {
            break;
         }
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (OPND_FLD(opnd)         != IR_Tbl_Idx ||
          IR_OPR(OPND_IDX(opnd)) != Dv_Deref_Opr) {
         find_opnd_line_and_column(&opnd, &line, &column);
         PRINTMSG(line, 976, Internal, column);
      }
      else {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }
   }
   else {
      find_opnd_line_and_column(&opnd, &line, &column);
      PRINTMSG(line, 977, Internal, column);
   }

   COPY_OPND(IR_OPND_R(dv_idx), opnd);

   gen_sh(location, Assignment_Stmt, stmt_start_line,
          stmt_start_col, FALSE, FALSE, TRUE);

   if (location == Before) {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = dv_idx;
   }


   /*************************************\
   |* SET FLAGS BACK TO ORIGINAL VALUES *|
   \*************************************/

   NTR_IR_TBL(dv_idx);
   IR_OPR(dv_idx) = Dv_Set_P_Or_A;
   IR_TYPE_IDX(dv_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = stmt_start_line;
   IR_COL_NUM(dv_idx)  = stmt_start_col;

   COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

   IR_FLD_R(dv_idx) = CN_Tbl_Idx;
   IR_IDX_R(dv_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(dv_idx) = stmt_start_line;
   IR_COL_NUM_R(dv_idx)  = stmt_start_col;
   
   gen_sh(location, Assignment_Stmt, stmt_start_line,
          stmt_start_col, FALSE, FALSE, TRUE);

   if (location == Before) {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = dv_idx;
   }

   TRACE (Func_Exit, "ptr_assign_from_ptr", NULL);

   return;

}  /* ptr_assign_from_ptr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the length (max(0,length)) operand for substring oprs.         *|
|*									      *|
|* Input parameters:							      *|
|*	sub_idx - IR_Tbl_Idx for substring opr.                               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void	add_substring_length(int	sub_idx)

{
   int		       col;
   int		       end_idx;
   expr_arg_type       exp_desc;
   boolean             foldit;
   int		       line;
   int		       list_idx;
   int		       list2_idx;
   int		       max_idx;
   int		       minus_idx;
   boolean             ok;
   opnd_type           opnd;
   int		       plus_idx;
   expr_mode_type      save_expr_mode;
   cif_usage_code_type save_xref_state;
   int		       start_idx;


   TRACE (Func_Entry, "add_substring_length", NULL);

   start_idx = IR_IDX_R(sub_idx);
   end_idx   = IL_NEXT_LIST_IDX(start_idx);

   if (IL_FLD(start_idx) == NO_Tbl_Idx ||
       IL_FLD(end_idx)   == NO_Tbl_Idx) {

      goto EXIT;
   }

   foldit = (IL_FLD(start_idx) == CN_Tbl_Idx) &&
            (IL_FLD(end_idx)   == CN_Tbl_Idx);

   line      = IR_LINE_NUM(sub_idx);
   col       = IR_COL_NUM(sub_idx);

   save_expr_mode = expr_mode;

   NTR_IR_LIST_TBL(list_idx);
   IL_PREV_LIST_IDX(list_idx) = end_idx;
   IL_NEXT_LIST_IDX(end_idx)  = list_idx;
   IR_LIST_CNT_R(sub_idx)++;

   NTR_IR_TBL(max_idx);
   IR_OPR(max_idx)		= Max_Opr;
   IR_TYPE_IDX(max_idx)		= CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(max_idx)		= line;
   IR_COL_NUM(max_idx)		= col;

   IL_FLD(list_idx) = IR_Tbl_Idx;
#ifdef KEY /* Bug 11922 */
   /*
    * When the -i8 option is on, what should be the type of the integer
    * lengths of character data? Throughout the front end, the assumption is
    * that the lengths are Integer_4 (e.g. in the extra integer arguments
    * passed to a procedure with character*(*) dummy arguments.) And the
    * code below this comment consistently uses CG_INTEGER_DEFAULT_TYPE, which
    * remains Integer_4 even under -i8.
    *
    * However, under -i8 the subscripts of this expression are likely to be
    * Integer_8, and they will force all the Integer_4 stuff to be converted
    * upward. If we do not generate an explicit conversion back to Integer_4,
    * then procedure calls, string comparison intrinsics, etc will fail under
    * -i8 -m32 because they expect Integer_4.
    */
   if (cmd_line_flags.s_integer8) {
     int convert_idx = gen_ir(IR_Tbl_Idx, max_idx, Cvrt_Opr,
       CG_INTEGER_DEFAULT_TYPE, line, col, NO_Tbl_Idx, NULL_IDX);
     IL_IDX(list_idx) = convert_idx;
   }
   else
#endif /* KEY Bug 11922 */
     IL_IDX(list_idx) = max_idx;

   NTR_IR_LIST_TBL(list2_idx);
   IR_FLD_L(max_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_L(max_idx) = 2;
   IR_IDX_L(max_idx) = list2_idx;

   IL_FLD(list2_idx) = CN_Tbl_Idx;
   IL_IDX(list2_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list2_idx) = line;
   IL_COL_NUM(list2_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
   list2_idx = IL_NEXT_LIST_IDX(list2_idx);

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx)  = col;

   IL_FLD(list2_idx) = IR_Tbl_Idx;
   IL_IDX(list2_idx) = plus_idx;

   NTR_IR_TBL(minus_idx);
   IR_OPR(minus_idx) = Minus_Opr;
   IR_TYPE_IDX(minus_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(minus_idx) = line;
   IR_COL_NUM(minus_idx)  = col;

   IR_FLD_R(plus_idx) = IR_Tbl_Idx;
   IR_IDX_R(plus_idx) = minus_idx;

   COPY_OPND(opnd, IL_OPND(start_idx));
   copy_subtree(&opnd, &opnd);
   COPY_OPND(IR_OPND_R(minus_idx), opnd);

   COPY_OPND(opnd, IL_OPND(end_idx));
   copy_subtree(&opnd, &opnd);
   COPY_OPND(IR_OPND_L(plus_idx), opnd);

   IR_FLD_L(minus_idx) = CN_Tbl_Idx;
   IR_IDX_L(minus_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_L(minus_idx) = line;
   IR_COL_NUM_L(minus_idx)  = col;

   if (foldit) {
      expr_mode = Regular_Expr;
      save_xref_state = xref_state;
      xref_state      = CIF_No_Usage_Rec;
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      ok = expr_semantics(&opnd, &exp_desc);
      COPY_OPND(IL_OPND(list_idx), opnd);

      expr_mode  = save_expr_mode;
      xref_state = save_xref_state;
   }

EXIT:

   TRACE (Func_Exit, "add_substring_length", NULL);

   return;

}  /* add_substring_length */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do semantic checks for array constructor implied do's                 *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - opnd pointing to IL_Tbl_Idx.                               *|
|*									      *|
|* Output parameters:							      *|
|*      exp_desc - exp_desc for array constructor.                            *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

static boolean array_construct_semantics(opnd_type      *top_opnd,
			 	         expr_arg_type	*exp_desc)

{
   int			column;
   boolean		constant_trip = TRUE;
#ifdef KEY /* Bug 10177 */
   int			do_var_idx = 0;
#else /* KEY Bug 10177 */
   int			do_var_idx;
#endif /* KEY Bug 10177 */
   boolean              do_var_ok;
   boolean              first_item = TRUE;
   int			line;
   expr_arg_type        loc_exp_desc;
   opnd_type		initial_opnd;
#ifdef KEY /* Bug 10177 */
   int                  list_idx = 0;
   int                  list2_idx;
   int			new_do_var_idx = 0;
#else /* KEY Bug 10177 */
   int                  list_idx;
   int                  list2_idx;
   int			new_do_var_idx;
#endif /* KEY Bug 10177 */
   opnd_type            opnd;
   boolean              ok		= TRUE;
   expr_mode_type	save_expr_mode;
   boolean              save_in_implied_do;
   cif_usage_code_type  save_xref_state;
   long_type		the_constant[MAX_WORDS_FOR_NUMERIC];
   int			type_idx;


   TRACE (Func_Entry, "array_construct_semantics", NULL);

   if (OPND_FLD((*top_opnd)) == NO_Tbl_Idx) {
      goto EXIT;
   }
   if (OPND_FLD((*top_opnd)) == IL_Tbl_Idx) {
      list_idx = OPND_IDX((*top_opnd));
   }
   else {
      find_opnd_line_and_column(top_opnd, &line, &column);
      PRINTMSG(line, 978, Internal, column);
   }

#ifdef KEY /* Bug 8004 */
   boolean needs_char_padding = FALSE;
#endif /* KEY Bug 8004 */
   while (list_idx != NULL_IDX) {

      IL_HAS_FUNCTIONS(list_idx) = FALSE;

      constant_trip = TRUE;

      if (IL_FLD(list_idx)         == IR_Tbl_Idx      &&
          IR_OPR(IL_IDX(list_idx)) == Implied_Do_Opr) {

         list2_idx = IL_NEXT_LIST_IDX(IR_IDX_R(IL_IDX(list_idx)));

         /* skip do variable processing until the control values are done. */

         /***********************\
         |* do do initial value *|
         \***********************/

         COPY_OPND(initial_opnd, IL_OPND(list2_idx));
         loc_exp_desc.rank = 0;
         number_of_functions = 0;
         save_xref_state     = xref_state;
         xref_state          = CIF_Symbol_Reference;
         ok = expr_sem(&initial_opnd, &loc_exp_desc) && ok;
         COPY_OPND(IL_OPND(list2_idx), initial_opnd);
         xref_state          = save_xref_state;

         IL_ARG_DESC_VARIANT(list2_idx) = TRUE;

         /* save exp_desc */
         arg_info_list_base      = arg_info_list_top;
         arg_info_list_top       = arg_info_list_base + 1;

         if (arg_info_list_top >= arg_info_list_size) {
            enlarge_info_list_table();
         }

         IL_ARG_DESC_IDX(list2_idx) = arg_info_list_top;
         arg_info_list[arg_info_list_top]    = init_arg_info;
         arg_info_list[arg_info_list_top].ed = loc_exp_desc;

         constant_trip = loc_exp_desc.foldable ||
                         loc_exp_desc.will_fold_later;

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list2_idx) = TRUE;
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
         }
         else {
            IL_HAS_FUNCTIONS(list2_idx) = FALSE;
         }

         if (loc_exp_desc.rank != 0) {
            find_opnd_line_and_column(&initial_opnd, &line, &column);
            PRINTMSG(line, 476, Error, column);
            ok = FALSE;
         }

         if (loc_exp_desc.linear_type == Long_Typeless) {
            find_opnd_line_and_column(&initial_opnd, &line, &column);
            PRINTMSG(line, 1133, Error, column);
            ok = FALSE;
         }
         else if (loc_exp_desc.type != Integer   &&
                  loc_exp_desc.type != Typeless) {
            find_opnd_line_and_column(&initial_opnd, &line, &column);
            PRINTMSG(line, 962, Error, column);
            ok = FALSE;
         }
         else if (loc_exp_desc.linear_type == Short_Typeless_Const) {
            find_opnd_line_and_column(&initial_opnd, &line, &column);
            IL_IDX(list2_idx) = cast_typeless_constant(IL_IDX(list2_idx),
						       INTEGER_DEFAULT_TYPE,
						       line,
						       column);
            loc_exp_desc.type_idx    = INTEGER_DEFAULT_TYPE;
            loc_exp_desc.type        = Integer;
            loc_exp_desc.linear_type = INTEGER_DEFAULT_TYPE;
            COPY_OPND(initial_opnd, IL_OPND(list2_idx));
         }

         list2_idx = IL_NEXT_LIST_IDX(list2_idx);

         /************************\
         |* do do terminal value *|
         \************************/

         COPY_OPND(opnd, IL_OPND(list2_idx));
         loc_exp_desc.rank = 0;
         number_of_functions = 0;
         save_xref_state     = xref_state;
         xref_state          = CIF_Symbol_Reference;
         ok = expr_sem(&opnd, &loc_exp_desc) && ok;
         COPY_OPND(IL_OPND(list2_idx), opnd);
         xref_state          = save_xref_state;

         IL_ARG_DESC_VARIANT(list2_idx) = TRUE;

         /* save exp_desc */
         arg_info_list_base      = arg_info_list_top;
         arg_info_list_top       = arg_info_list_base + 1;

         if (arg_info_list_top >= arg_info_list_size) {
            enlarge_info_list_table();
         }

         IL_ARG_DESC_IDX(list2_idx) = arg_info_list_top;
         arg_info_list[arg_info_list_top]    = init_arg_info;
         arg_info_list[arg_info_list_top].ed = loc_exp_desc;

         constant_trip &= loc_exp_desc.foldable ||
                         loc_exp_desc.will_fold_later;

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list2_idx) = TRUE;
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
         }
         else {
            IL_HAS_FUNCTIONS(list2_idx) = FALSE;
         }

         if (loc_exp_desc.rank != 0) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 476, Error, column);
            ok = FALSE;
         }

         if (loc_exp_desc.linear_type == Long_Typeless) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 1133, Error, column);
            ok = FALSE;
         }
         else if (loc_exp_desc.type != Integer   &&
                  loc_exp_desc.type != Typeless) {

            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 962, Error, column);
            ok = FALSE;
         }
         else if (loc_exp_desc.linear_type == Short_Typeless_Const) {
            find_opnd_line_and_column(&opnd, &line, &column);
            IL_IDX(list2_idx) = cast_typeless_constant(IL_IDX(list2_idx),
                                                       INTEGER_DEFAULT_TYPE,
                                                       line,
                                                       column);
            loc_exp_desc.type_idx    = INTEGER_DEFAULT_TYPE;
            loc_exp_desc.type        = Integer;
            loc_exp_desc.linear_type = INTEGER_DEFAULT_TYPE;
         }


         /********************************\
         |* do do stride if there is one *|
         \********************************/

         if (IL_NEXT_LIST_IDX(list2_idx) != NULL_IDX) {
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            COPY_OPND(opnd, IL_OPND(list2_idx));
            loc_exp_desc.rank = 0;
            number_of_functions = 0;
            save_xref_state     = xref_state;
            xref_state          = CIF_Symbol_Reference;
            ok = expr_sem(&opnd, &loc_exp_desc) && ok;
            COPY_OPND(IL_OPND(list2_idx), opnd);
            xref_state          = save_xref_state;

            find_opnd_line_and_column(&opnd, &line, &column);

            IL_ARG_DESC_VARIANT(list2_idx) = TRUE;
   
            /* save exp_desc */
            arg_info_list_base      = arg_info_list_top;
            arg_info_list_top       = arg_info_list_base + 1;

            if (arg_info_list_top >= arg_info_list_size) {
               enlarge_info_list_table();
            }

            IL_ARG_DESC_IDX(list2_idx) = arg_info_list_top;
            arg_info_list[arg_info_list_top]    = init_arg_info;
            arg_info_list[arg_info_list_top].ed = loc_exp_desc;

            constant_trip &= loc_exp_desc.foldable ||
                         loc_exp_desc.will_fold_later;

            if (number_of_functions > 0) {
               IL_HAS_FUNCTIONS(list2_idx) = TRUE;
               IL_HAS_FUNCTIONS(list_idx)  = TRUE;
            }
            else {
               IL_HAS_FUNCTIONS(list2_idx) = FALSE;
            }

            if (loc_exp_desc.rank != 0) {
               PRINTMSG(line, 476, Error, column);
               ok = FALSE;
            }

            if (loc_exp_desc.linear_type == Long_Typeless) {
               PRINTMSG(line, 1133, Error, column);
               ok = FALSE;
            }
            else if (loc_exp_desc.type != Integer   &&
                     loc_exp_desc.type != Typeless) {

               PRINTMSG(line, 962, Error, column);
               ok = FALSE;
            }
            else if (loc_exp_desc.linear_type == Short_Typeless_Const) {
               IL_IDX(list2_idx) = cast_typeless_constant(IL_IDX(list2_idx),
                                                          INTEGER_DEFAULT_TYPE,
                                                          line,
                                                          column);
               loc_exp_desc.type_idx    = INTEGER_DEFAULT_TYPE;
               loc_exp_desc.type        = Integer;
               loc_exp_desc.linear_type = INTEGER_DEFAULT_TYPE;
            }

            if (ok &&
                OPND_FLD(opnd) == CN_Tbl_Idx) {

               type_idx = CG_LOGICAL_DEFAULT_TYPE;

               ok &= folder_driver((char *)&CN_CONST(OPND_IDX(opnd)),
                                 loc_exp_desc.type_idx,
                                 (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                                 CG_INTEGER_DEFAULT_TYPE,
                                 the_constant,
                                 &type_idx,
                                 line,
                                 column,
                                 2,
                                 Eq_Opr);

               if (THIS_IS_TRUE(the_constant, type_idx)) {
                  PRINTMSG(line, 1084, Error, column);
                  ok = FALSE;
               }
            }
         }
         else {
            /* fill in default stride here */
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            IR_LIST_CNT_R(IL_IDX(list_idx))++;
            IL_FLD(list2_idx) = CN_Tbl_Idx;
            IL_IDX(list2_idx) = CN_INTEGER_ONE_IDX;
            IL_LINE_NUM(list2_idx) = stmt_start_line;
            IL_COL_NUM(list2_idx)  = stmt_start_col;
            IL_ARG_DESC_VARIANT(list2_idx) = TRUE;

            /* save exp_desc */
            arg_info_list_base      = arg_info_list_top;
            arg_info_list_top       = arg_info_list_base + 1;

            if (arg_info_list_top >= arg_info_list_size) {
               enlarge_info_list_table();
            }

            IL_ARG_DESC_IDX(list2_idx) = arg_info_list_top;
            arg_info_list[arg_info_list_top]		 = init_arg_info;
            arg_info_list[arg_info_list_top].ed.constant = TRUE;
            arg_info_list[arg_info_list_top].ed.foldable = TRUE;
            arg_info_list[arg_info_list_top].ed.type	 = Integer;
            arg_info_list[arg_info_list_top].ed.type_idx = 
                                                       CG_INTEGER_DEFAULT_TYPE;
            arg_info_list[arg_info_list_top].ed.linear_type = 
                                                       CG_INTEGER_DEFAULT_TYPE;
         }

         /**************************\
         |* do do control variable *|
         \**************************/

         list2_idx = IR_IDX_R(IL_IDX(list_idx));

         do_var_ok = TRUE;
         COPY_OPND(opnd, IL_OPND(list2_idx));
         loc_exp_desc.rank   = 0;
         number_of_functions = 0;
         save_xref_state     = xref_state;
         xref_state          = CIF_No_Usage_Rec;
         save_in_implied_do = in_implied_do;
         in_implied_do      = FALSE;
         save_expr_mode = expr_mode;
         expr_mode = Regular_Expr;
         do_var_ok = expr_sem(&opnd, &loc_exp_desc);
         COPY_OPND(IL_OPND(list2_idx), opnd);
         expr_mode     = save_expr_mode;
         in_implied_do = save_in_implied_do;
         xref_state    = save_xref_state;

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list2_idx) = TRUE;
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
         }
         else {
            IL_HAS_FUNCTIONS(list2_idx) = FALSE;
         }

/* BHJ JLS LRR ... need interpretation for this one. imp do var must be */
/* "named" scalar variable, not sub-object.                             */
         if (!loc_exp_desc.reference) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 481, Error, column);
            do_var_ok = FALSE;
         }
         else {

            if (loc_exp_desc.type != Integer) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 675, Error, column);
               do_var_ok = FALSE;
            }

            if (OPND_FLD(opnd) == IR_Tbl_Idx                   &&
                IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr) {
               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            }

            if (OPND_FLD(opnd) == IR_Tbl_Idx            &&
                IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {
               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            }

            if (do_var_ok                     &&
                OPND_FLD(opnd) != AT_Tbl_Idx) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 530, Error, column);
               do_var_ok = FALSE;
            }
            else {
               do_var_idx = OPND_IDX(opnd);
            }

            if (do_var_ok          &&
                loc_exp_desc.rank) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 837, Ansi, column);
            }

         }

         if (do_var_ok) {

            if (AT_ATTR_LINK(do_var_idx)) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 533, Error, column,
                        AT_OBJ_NAME_PTR(do_var_idx));
               do_var_ok = FALSE;
            }
            else {
               find_opnd_line_and_column(&opnd, &line, &column);
               new_do_var_idx = gen_compiler_tmp(line, column, Priv, TRUE);
               AT_SEMANTICS_DONE(new_do_var_idx)= TRUE;
               ATD_TYPE_IDX(new_do_var_idx)	= ATD_TYPE_IDX(do_var_idx);
               ATD_STOR_BLK_IDX(new_do_var_idx) =
                                              SCP_SB_STACK_IDX(curr_scp_idx);

               /* change name to original name */
               AT_NAME_IDX(new_do_var_idx) = AT_NAME_IDX(do_var_idx);
               AT_NAME_LEN(new_do_var_idx) = AT_NAME_LEN(do_var_idx);

               ATD_TMP_IDX(new_do_var_idx)      = constructor_level;
               AT_ATTR_LINK(do_var_idx)         = new_do_var_idx;
               AT_IGNORE_ATTR_LINK(do_var_idx)  = TRUE;

               ATD_IMP_DO_LCV(new_do_var_idx)   = TRUE;
               ATD_LCV_IS_CONST(new_do_var_idx) = constant_trip;
               ATD_TMP_NEEDS_CIF(new_do_var_idx) = TRUE;

               IL_FLD(list2_idx) = AT_Tbl_Idx;
               IL_IDX(list2_idx) = new_do_var_idx;
               IL_LINE_NUM(list2_idx) = line;
               IL_COL_NUM(list2_idx)  = column;

               /* issue a usage rec if needed */
               if ((cif_flags & XREF_RECS) != 0) {
                  cif_usage_rec(new_do_var_idx, AT_Tbl_Idx, line, column, 
                                CIF_Symbol_Modification);
               }

            }
         }

         ok = ok && do_var_ok;

         /***********************\
         |* do list of io items *|
         \***********************/

         in_implied_do = TRUE;
         COPY_OPND(opnd, IR_OPND_L(IL_IDX(list_idx)));
         number_of_functions = 0;
         ok = array_construct_semantics(&opnd, &loc_exp_desc) && ok;
         COPY_OPND(IR_OPND_L(IL_IDX(list_idx)), opnd);

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
         }

         IR_TYPE_IDX(IL_IDX(list_idx))	= loc_exp_desc.type_idx;

         if (do_var_ok) {
            /* clear the AT_ATTR_LINK field of the old do var attr */
            AT_ATTR_LINK(do_var_idx)        = NULL_IDX;
            AT_IGNORE_ATTR_LINK(do_var_idx) = FALSE;

            /* clear the ATD_TMP_IDX on new_do_var_idx. */
            /* it held the constructor_level.           */
            ATD_TMP_IDX(new_do_var_idx) = NULL_IDX;

            /* now set the initial opnd on the tmp_idx field */
            ATD_FLD(new_do_var_idx) = OPND_FLD(initial_opnd);
            ATD_TMP_IDX(new_do_var_idx) = OPND_IDX(initial_opnd);
         }

         in_implied_do = save_in_implied_do;
      }
      else {

         loc_exp_desc.rank = 0;
         COPY_OPND(opnd, IL_OPND(list_idx));
         number_of_functions = 0;

         save_xref_state = xref_state;
         xref_state = CIF_Symbol_Reference;

         ok = expr_sem(&opnd, &loc_exp_desc) && ok;

         xref_state = save_xref_state;

         if (loc_exp_desc.linear_type == Short_Typeless_Const) {
            find_opnd_line_and_column((opnd_type *) &opnd, &line, &column);
            OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                    INTEGER_DEFAULT_TYPE,
                                                    line,
                                                    column);

            loc_exp_desc.type_idx = INTEGER_DEFAULT_TYPE;
            loc_exp_desc.type = Integer;
            loc_exp_desc.linear_type = INTEGER_DEFAULT_TYPE;
         }

         COPY_OPND(IL_OPND(list_idx), opnd);
                                                    
         IL_ARG_DESC_VARIANT(list_idx) = TRUE;

         /* save exp_desc */
         arg_info_list_base      = arg_info_list_top;
         arg_info_list_top       = arg_info_list_base + 1;

         if (arg_info_list_top >= arg_info_list_size) {
            enlarge_info_list_table();
         }

         IL_ARG_DESC_IDX(list_idx) = arg_info_list_top;
         arg_info_list[arg_info_list_top]    = init_arg_info;
         arg_info_list[arg_info_list_top].ed = loc_exp_desc;

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
         }

      }

      if (first_item) {
         if (loc_exp_desc.linear_type == Typeless_4 ||
             loc_exp_desc.linear_type == Typeless_8) {
            exp_desc->type_idx    = INTEGER_DEFAULT_TYPE; 
            exp_desc->type        = Integer;
            exp_desc->linear_type = INTEGER_DEFAULT_TYPE;
         }
         else {
            exp_desc->type        = loc_exp_desc.type;
            exp_desc->type_idx    = loc_exp_desc.type_idx;
            exp_desc->linear_type = loc_exp_desc.linear_type;
         }

         COPY_OPND((exp_desc->char_len), (loc_exp_desc.char_len));
         exp_desc->constant	= loc_exp_desc.constant;
         exp_desc->foldable	= loc_exp_desc.foldable && constant_trip;
         exp_desc->will_fold_later = (loc_exp_desc.will_fold_later ||
                                      loc_exp_desc.foldable) && constant_trip;
         exp_desc->has_symbolic = loc_exp_desc.has_symbolic;
         first_item  = FALSE;
      }
      else {
      
         if ((loc_exp_desc.linear_type == Typeless_4 ||
              loc_exp_desc.linear_type == Typeless_8) &&
             exp_desc->linear_type == INTEGER_DEFAULT_TYPE) {

            /* intentionally blank */
         }
         else if (exp_desc->type != loc_exp_desc.type) {

            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &line, &column);
            PRINTMSG(line, 829, Error, column);
            ok = FALSE;
         }
         else if (exp_desc->type == Structure &&
                 !compare_derived_types(exp_desc->type_idx,
                                        loc_exp_desc.type_idx)) {
            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &line, &column);
            PRINTMSG(line, 829, Error, column);
            ok = FALSE;
         }
         else if (exp_desc->type == Character) {

            if (loc_exp_desc.char_len.fld == CN_Tbl_Idx) {

               if (exp_desc->char_len.fld == CN_Tbl_Idx) {

               if (
#ifdef KEY /* Bug 8004 */
		/*
		 * F95 requires that all char lengths inside an array ctor
		 * be the same, but doesn't state that as a numbered
		 * constraint, so this Ansi message isn't strictly needed.
		 * F2003 requires they be the same unless there's an explicit
		 * type-spec inside the constructor brackets, but still
		 * doesn't state that as a numbered constraint. Due to
		 * this change, our behavior is more generous than even
		 * F2003: we use the max of the lengths if there is
		 * no explicit type-spec. When we add parsing for the
		 * explicit type-spec, that will impact the following code.
		 */
                on_off_flags.issue_ansi_messages &&
#endif /* KEY Bug 8004 */
                  fold_relationals(loc_exp_desc.char_len.idx,
                                       exp_desc->char_len.idx,
                                       Ne_Opr)) {
                     find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                               &line, &column);
#ifdef KEY /* Bug 8004 */
                     PRINTMSG(line, 838, Ansi, column);
#else /* KEY Bug 8004 */
                     PRINTMSG(line, 838, Error, column);
                     ok = FALSE;
#endif /* KEY Bug 8004 */
                  }
/* KEY Bug 8004 # if 0 */
                  /* if we ever extend the above constraint, */
                  /* then include this code.                 */

                  if (fold_relationals(loc_exp_desc.char_len.idx,
                                       exp_desc->char_len.idx,
                                       Gt_Opr)) {

                     COPY_OPND((exp_desc->char_len), (loc_exp_desc.char_len));
#ifdef KEY /* Bug 8004 */
                     exp_desc->type_idx = loc_exp_desc.type_idx;
		     needs_char_padding =
		       (loc_exp_desc.char_len.fld == CN_Tbl_Idx);
#endif /* KEY Bug 8004 */
                  }
/* KEY Bug 8004 # endif */
               }
               else {
                  /* replace the char_len with the simpler length */
                  COPY_OPND((exp_desc->char_len), (loc_exp_desc.char_len));
               }
            }
         }
         else if (exp_desc->linear_type != loc_exp_desc.linear_type) {
            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &line, &column);
            PRINTMSG(line, 829, Error, column);
            ok = FALSE;
         }

         exp_desc->has_symbolic |= loc_exp_desc.has_symbolic;
         exp_desc->constant &= loc_exp_desc.constant;
         exp_desc->foldable &= loc_exp_desc.foldable && constant_trip;
         exp_desc->will_fold_later &= (loc_exp_desc.will_fold_later ||
                                      loc_exp_desc.foldable) &&
                                       constant_trip;
      }
             
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

#ifdef KEY /* Bug 8004 */
   /* We now allow a character constructor to have elements of differing
    * lengths. For variables and for constructors used to initialize
    * fixed-length character types, enabling the change above (which existed
    * in the original Open64 distribution but was disabled) suffices. For
    * a constructor used to initialize a "character*(*), parameter"
    * however, we need to make each element be the correct size (the
    * alternative was more extensive surgery on interpret_constructor and
    * interpret_array_construct_opr in s_cnstrct.c.)
    */
   if (needs_char_padding && exp_desc->constant &&
      exp_desc->char_len.fld == CN_Tbl_Idx) {
      long desired_length = CN_CONST(exp_desc->char_len.idx);
      for (list_idx = OPND_IDX((*top_opnd)); list_idx != NULL_IDX; 
	 list_idx = IL_NEXT_LIST_IDX(list_idx)) {
	 opnd_type opnd = IL_OPND(list_idx);
	 long actual_length = 0;
	 if (opnd.fld == CN_Tbl_Idx && desired_length !=
	   (actual_length = CN_CONST(TYP_IDX(CN_TYPE_IDX(opnd.idx))))) {
	   int char_idx = ntr_const_tbl(exp_desc->type_idx, TRUE, NULL);
	   char *char_ptr = (char *) &CN_CONST(char_idx);
	   memset(char_ptr, ' ', desired_length);
	   memcpy(char_ptr, (char *) &CN_CONST(opnd.idx), actual_length);
	   IL_OPND(list_idx).idx = char_idx;
	 }
      }
   }
#endif /* KEY Bug 8004 */


EXIT:

   TRACE (Func_Exit, "array_construct_semantics", NULL);

   return(ok);

}  /* array_construct_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do semantic checks on the stmt function definition.                   *|
|*									      *|
|* Input parameters:							      *|
|*	stmt_func_idx - attr idx for stmt function.                           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

boolean stmt_func_semantics(int                 stmt_func_idx)

{
   expr_arg_type        exp_desc;
   int                  i;
   linear_type_type     linear_type;
   boolean              ok              = TRUE;
   opnd_type            opnd;
   expr_mode_type       save_expr_mode;
   boolean              save_no_func_expansion;
   boolean		save_parallel_region;
   cif_usage_code_type  save_xref_state;
   int                  sn_idx;


   TRACE (Func_Entry, "stmt_func_semantics", NULL);

#ifdef KEY /* Bug 4232 */
   defining_stmt_func = TRUE;
#endif /* KEY Bug 4232 */

   ATS_SF_SEMANTICS_DONE(stmt_func_idx) = TRUE;

   /* clear the ATD_SF_DARG flag */

   sn_idx = ATP_FIRST_IDX(stmt_func_idx);

   for (i = 0; i < ATP_NUM_DARGS(stmt_func_idx); i++) {
      ATD_SF_DARG(SN_ATTR_IDX(sn_idx)) = FALSE;
      sn_idx++;
   }

   OPND_FLD(opnd) = (fld_type) ATS_SF_FLD(stmt_func_idx);
   OPND_IDX(opnd) = ATS_SF_IDX(stmt_func_idx);
   copy_subtree(&opnd, &opnd);

   ATS_SF_ACTIVE(stmt_func_idx) = TRUE;

   save_parallel_region = cdir_switches.parallel_region;
   cdir_switches.parallel_region = FALSE;
   save_no_func_expansion       = no_func_expansion;
   no_func_expansion            = TRUE;
   save_xref_state              = xref_state;
   xref_state                   = CIF_Symbol_Reference;
   save_expr_mode               = expr_mode;
   expr_mode                    = Stmt_Func_Expr;

   ok                           &= expr_semantics(&opnd, &exp_desc);

   expr_mode                    = save_expr_mode;
   xref_state                   = save_xref_state;
   no_func_expansion            = save_no_func_expansion;
   cdir_switches.parallel_region = save_parallel_region;
   ATS_SF_ACTIVE(stmt_func_idx) = FALSE;

   /* set the ATD_SF_DARG flag */

   sn_idx = ATP_FIRST_IDX(stmt_func_idx);

   for (i = 0; i < ATP_NUM_DARGS(stmt_func_idx); i++) {
      ATD_SF_DARG(SN_ATTR_IDX(sn_idx)) = TRUE;
      sn_idx++;
   }


   if (exp_desc.rank != 0) {

      /* stmt func must be rank zero */

      PRINTMSG(AT_DEF_LINE(stmt_func_idx), 755, Error,
               AT_DEF_COLUMN(stmt_func_idx),
               AT_OBJ_NAME_PTR(stmt_func_idx));
      ok = FALSE;
      AT_DCL_ERR(stmt_func_idx) = TRUE;
   }

   linear_type = TYP_LINEAR(ATD_TYPE_IDX(stmt_func_idx));

   if (ASG_TYPE(linear_type, exp_desc.linear_type) == Err_Res) {
      PRINTMSG(AT_DEF_LINE(stmt_func_idx), 756, Error,
               AT_DEF_COLUMN(stmt_func_idx),
               AT_OBJ_NAME_PTR(stmt_func_idx));
      ok = FALSE;
      AT_DCL_ERR(stmt_func_idx) = TRUE;
   }
   else if (ASG_TYPE(linear_type, exp_desc.linear_type) == Structure_Type) {

      if (!compare_derived_types(ATD_TYPE_IDX(stmt_func_idx),
                                 exp_desc.type_idx)) {
         PRINTMSG(AT_DEF_LINE(stmt_func_idx), 756, Error,
                  AT_DEF_COLUMN(stmt_func_idx),
                  AT_OBJ_NAME_PTR(stmt_func_idx));
         ok = FALSE;
         AT_DCL_ERR(stmt_func_idx) = TRUE;
      }
   }

#ifdef KEY /* Bug 4232 */
   defining_stmt_func = FALSE;
#endif /* KEY Bug 4232 */

   TRACE (Func_Exit, "stmt_func_semantics", NULL);

   return(ok);

}  /* stmt_func_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do conformance checks for array syntax operators. Also determine      *|
|*      "shape" opnd to pass on for the operation based on analysis of        *|
|*      the right and left shape.                                             *|
|*									      *|
|* Input parameters:							      *|
|*	exp_desc_l - expression descriptor for left operand.                  *|
|*      exp_desc_r - expression descriptor for right operand.                 *|
|*      line, col  - line and column to use for messages.                     *|
|*									      *|
|* Output parameters:							      *|
|*	exp_desc - fills in the result shape in this descriptor.              *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors.                                                    *|
|*									      *|
\******************************************************************************/

static boolean bin_array_syntax_check(expr_arg_type	*exp_desc_l,
				      expr_arg_type	*exp_desc_r,
				      expr_arg_type	*exp_desc,
				      int		 line,
				      int		 col)

{
   int			i;
   boolean		ok = TRUE;

   TRACE (Func_Entry, "bin_array_syntax_check", NULL);

   if (exp_desc_r->rank == exp_desc_l->rank) {
      /* conformance check here */

      exp_desc->rank = exp_desc_r->rank;

      for (i = 0; i < exp_desc_r->rank; i++) {

         if (OPND_FLD(exp_desc_l->shape[i]) == CN_Tbl_Idx &&
             OPND_FLD(exp_desc_r->shape[i]) == CN_Tbl_Idx) {

            if (fold_relationals(OPND_IDX(exp_desc_l->shape[i]),
                                 OPND_IDX(exp_desc_r->shape[i]),
                                 Ne_Opr)) {

               /* non conforming array syntax */
               PRINTMSG(line, 252, Error, col);
               ok = FALSE;
               exp_desc->rank = exp_desc_r->rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_r->shape,
                          exp_desc_r->rank);
               break;
            }
            else {
               COPY_OPND(exp_desc->shape[i], exp_desc_l->shape[i]);
            }
         }
         else if (SHAPE_FOLDABLE(exp_desc_l->shape[i])) {
            COPY_OPND(exp_desc->shape[i], exp_desc_l->shape[i]);
         }
         else if (SHAPE_FOLDABLE(exp_desc_r->shape[i])) {
            COPY_OPND(exp_desc->shape[i], exp_desc_r->shape[i]);
         }
         else if (SHAPE_WILL_FOLD_LATER(exp_desc_l->shape[i])) {
            COPY_OPND(exp_desc->shape[i], exp_desc_l->shape[i]);
         }
         else {
            COPY_OPND(exp_desc->shape[i], exp_desc_r->shape[i]);
         }
      }
   }
   else if (exp_desc_r->rank > exp_desc_l->rank) {
      exp_desc->rank = exp_desc_r->rank;
      COPY_SHAPE(exp_desc->shape,exp_desc_r->shape,
                 exp_desc_r->rank);
   }
   else {
      exp_desc->rank = exp_desc_l->rank;
      COPY_SHAPE(exp_desc->shape,exp_desc_l->shape,
                 exp_desc_l->rank);
   }


   TRACE (Func_Exit, "bin_array_syntax_check", NULL);

   return(ok);

}  /* bin_array_syntax_check */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Looks for real division and replaces the div_opr with                 *|
|*      Real_Div_To_Int_Opr if on_off_flags.round_integer_divide is TRUE.     *|
|*      This routine is used when the real division is changed to integer     *|
|*      later (ie. in an assignment).                                         *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - top of tree.                                                   *|
|*									      *|
|* Output parameters:							      *|
|*	opnd - the modified tree.                                             *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void look_for_real_div(opnd_type *opnd)

{
   int		list_idx;
   opnd_type	lopnd;

   TRACE (Func_Entry, "look_for_real_div", NULL);

   switch (OPND_FLD((*opnd))) {
      case IR_Tbl_Idx:

         if (IR_OPR(OPND_IDX((*opnd))) == Div_Opr &&
             TYP_TYPE(IR_TYPE_IDX(OPND_IDX((*opnd)))) == Real) {

            if (on_off_flags.round_integer_divide) {
               IR_OPR(OPND_IDX((*opnd))) = Real_Div_To_Int_Opr;
            }
            else {
               PRINTMSG(IR_LINE_NUM(OPND_IDX((*opnd))), 938, Caution,
                        IR_COL_NUM(OPND_IDX((*opnd))));
            }
         }

         COPY_OPND(lopnd, IR_OPND_L(OPND_IDX((*opnd))));
         look_for_real_div(&lopnd);
         COPY_OPND(IR_OPND_L(OPND_IDX((*opnd))), lopnd);

         COPY_OPND(lopnd, IR_OPND_R(OPND_IDX((*opnd))));
         look_for_real_div(&lopnd);
         COPY_OPND(IR_OPND_R(OPND_IDX((*opnd))), lopnd);

         break;

      case IL_Tbl_Idx:

         list_idx = OPND_IDX((*opnd));

         while (list_idx) {
            COPY_OPND(lopnd, IL_OPND(list_idx));
            look_for_real_div(&lopnd);
            COPY_OPND(IL_OPND(list_idx), lopnd);
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;
   }

   TRACE (Func_Exit, "look_for_real_div", NULL);

   return;

}  /* look_for_real_div */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Creates a logical array tmp thats necessary for zero length character *|
|*      logical operations. (.eq. ....) We must fold these expressions.       *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd -  the logical constant to put in array. (scalar)            *|
|*									      *|
|* Output parameters:							      *|
|*	top_opnd - the array ref result.                                      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void make_logical_array_tmp(opnd_type		*top_opnd,
                                   expr_arg_type	*exp_desc)

{
   int			col;
   boolean		constant_shape = TRUE;
   int			i;
   opnd_type		l_opnd;
   int			line;
   expr_arg_type	loc_exp_desc;
   boolean		ok;
   opnd_type		r_opnd;
   boolean		save_check_type_conversion;
   int			save_target_array_idx;
   opnd_type		save_init_target_opnd;
   int			save_target_type_idx;
   int			unused;

   TRACE (Func_Entry, "make_logical_array_tmp", NULL);

   find_opnd_line_and_column(top_opnd, &line, &col);

   for (i = 0; i < exp_desc->rank; i++) {

      if (! SHAPE_FOLDABLE(exp_desc->shape[i])) {
         constant_shape = FALSE;
         break;
      }
   }

   if (constant_shape) {
      save_check_type_conversion = check_type_conversion;
      save_target_array_idx      = target_array_idx;
      save_target_type_idx       = target_type_idx;
      COPY_OPND(save_init_target_opnd, init_target_opnd);

      target_array_idx = create_bd_ntry_for_const(exp_desc, line, col);

      check_type_conversion = TRUE;
      target_type_idx = exp_desc->type_idx;
      init_target_opnd = null_opnd;

      loc_exp_desc.type = exp_desc->type;
      loc_exp_desc.linear_type = exp_desc->linear_type;
      loc_exp_desc.type_idx = exp_desc->type_idx;
      
      ok = fold_aggragate_expression(top_opnd,
                                    &loc_exp_desc,
                                     FALSE);

      check_type_conversion        = save_check_type_conversion;
      COPY_OPND(init_target_opnd, save_init_target_opnd);
      target_type_idx              = save_target_type_idx;
      target_array_idx             = save_target_array_idx;

      exp_desc->tmp_reference = TRUE;
      exp_desc->foldable = TRUE;
   }
   else {

      COPY_OPND(r_opnd, (*top_opnd));
      unused = create_tmp_asg(&r_opnd,
                               exp_desc,
                               &l_opnd,
			       Intent_In,
                               FALSE,
                               FALSE);
      COPY_OPND((*top_opnd), l_opnd);
   }

   TRACE (Func_Exit, "make_logical_array_tmp", NULL);

   return;

}  /* make_logical_array_tmp */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	In strange variable function result size calculation, a character     *|
|*      substring reference may involve nested substrings. This routine       *|
|*      folds them into one substring. It is not intended for any other       *|
|*      situation.                                                            *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx	-	IR_Tbl_Idx to the upper Substring_Opr                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void fold_nested_substrings(int	ir_idx)

{
   int			col;
   opnd_type		end_opnd;
   expr_arg_type	exp_desc;
   int			line;
   int			list_idx;
   int			minus_idx;
   boolean		ok;
   opnd_type		opnd;
   int			plus_idx;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   opnd_type		start_opnd;


   TRACE (Func_Entry, "fold_nested_substrings", NULL);

   if (IR_OPR(IR_IDX_L(ir_idx)) == Whole_Substring_Opr) {
      /* just get rid of the substring opr */
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IR_IDX_L(ir_idx)));
      goto EXIT;
   }

   list_idx = IR_IDX_R(IR_IDX_L(ir_idx));
   COPY_OPND(start_opnd, IL_OPND(list_idx));

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   COPY_OPND(end_opnd, IL_OPND(list_idx));  /*BRIANJ - end_opnd is not used */

   /* do the start expression */

   list_idx = IR_IDX_R(ir_idx);
   find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx), &line, &col);

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx) = col;

   COPY_OPND(IR_OPND_L(plus_idx), start_opnd);
   COPY_OPND(IR_OPND_R(plus_idx), IL_OPND(list_idx));
   
   NTR_IR_TBL(minus_idx);
   IR_OPR(minus_idx) = Minus_Opr;
   IR_TYPE_IDX(minus_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(minus_idx) = line;
   IR_COL_NUM(minus_idx) = col;

   IR_FLD_L(minus_idx) = IR_Tbl_Idx;
   IR_IDX_L(minus_idx) = plus_idx;
   IR_FLD_R(minus_idx) = CN_Tbl_Idx;
   IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(minus_idx) = line;
   IR_COL_NUM_R(minus_idx) = col;

   OPND_FLD(opnd) = IR_Tbl_Idx;
   OPND_IDX(opnd) = minus_idx;

   /* fold */
   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;
   exp_desc.rank = 0;
   ok = expr_semantics(&opnd, &exp_desc);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;


   COPY_OPND(IL_OPND(list_idx), opnd);


   /* now do the end expression */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx), &line, &col);

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx) = col;

   COPY_OPND(IR_OPND_L(plus_idx), start_opnd);
   COPY_OPND(IR_OPND_R(plus_idx), IL_OPND(list_idx));

   NTR_IR_TBL(minus_idx);
   IR_OPR(minus_idx) = Minus_Opr;
   IR_TYPE_IDX(minus_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(minus_idx) = line;
   IR_COL_NUM(minus_idx) = col;

   IR_FLD_L(minus_idx) = IR_Tbl_Idx;
   IR_IDX_L(minus_idx) = plus_idx;
   IR_FLD_R(minus_idx) = CN_Tbl_Idx;
   IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(minus_idx) = line;
   IR_COL_NUM_R(minus_idx) = col;

   OPND_FLD(opnd) = IR_Tbl_Idx;
   OPND_IDX(opnd) = minus_idx;

   /* fold */
   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;
   exp_desc.rank = 0;
   ok = expr_semantics(&opnd, &exp_desc);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;

   COPY_OPND(IL_OPND(list_idx), opnd);

   /* the length remains unchanged */

   /* now get rid of lower substring */

   COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IR_IDX_L(ir_idx)));

EXIT:

   TRACE (Func_Exit, "fold_nested_substrings", NULL);

   return;

}  /* fold_nested_substrings */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Uplus_Opr and Uminus_Opr.                    *|
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

static boolean uplus_opr_handler(opnd_type		*result_opnd,
				 expr_arg_type		*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "uplus_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor;
   exp_desc->has_symbolic	= exp_desc_l.has_symbolic;

   exp_desc->linear_type = UN_PLUS_TYPE(exp_desc_l.linear_type);

   if (exp_desc->linear_type != Err_Res) {

      if (UN_PLUS_EXTN(exp_desc_l.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else if (exp_desc_l.type == Character ||
                  exp_desc_l.linear_type == Short_Typeless_Const) {
            find_opnd_line_and_column((opnd_type *)
                                      &IR_OPND_L(ir_idx),
                                      &opnd_line,
                                      &opnd_col);
            if (exp_desc_l.type == Character) {
               PRINTMSG(opnd_line, 161, Ansi, opnd_col);
            }

            IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                      exp_desc->linear_type,
                                                      opnd_line,
                                                      opnd_col);

            exp_desc_l.type_idx    = exp_desc->linear_type;
            exp_desc_l.type        = TYP_TYPE(exp_desc->linear_type);
            exp_desc_l.linear_type = exp_desc->linear_type;
            exp_desc->linear_type = UN_PLUS_TYPE(exp_desc_l.linear_type);
         }

      }

      exp_desc->type_idx = exp_desc->linear_type;
      exp_desc->type     = TYP_TYPE(exp_desc->linear_type);
      exp_desc->rank     = exp_desc_l.rank;
      exp_desc->has_symbolic = exp_desc_l.has_symbolic;
      exp_desc->constant = exp_desc_l.constant;
      exp_desc->foldable = exp_desc_l.foldable;
      exp_desc->will_fold_later = exp_desc_l.will_fold_later;

      if (exp_desc->linear_type == Integer_8) {
         /* check whether it should be 'default' typed */

         if (exp_desc_l.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_l.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_l.type_idx;
         }
      }

      COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,exp_desc_l.rank);

      if (IR_OPR(ir_idx) == Uplus_Opr) {
         COPY_OPND((*result_opnd), IR_OPND_L(ir_idx));
      }
      else if (opt_flags.ieeeconform &&
               ! comp_gen_expr       &&
               (exp_desc_l.type == Real ||
                exp_desc_l.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc_l.rank == 0 &&
               exp_desc_l.foldable &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                            NULL,
                            NULL_IDX,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            1,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;

            if (CN_BOZ_CONSTANT(IR_IDX_L(ir_idx))) {
               OPND_IDX((*result_opnd)) =
                           ntr_boz_const_tbl(type_idx, folded_const);
            }
            else {
               OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                        FALSE,
                                                        folded_const);
            }

            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "uplus_opr_handler", NULL);

   return(ok);

}  /* uplus_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Power_Opr.                                   *|
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

static boolean power_opr_handler(opnd_type		*result_opnd,
				 expr_arg_type		*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "power_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->linear_type = POWER_TYPE(exp_desc_l.linear_type,
                                      exp_desc_r.linear_type);
   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))    {

      if (POWER_EXTN(exp_desc_l.linear_type,
                     exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {
            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = exp_desc->linear_type;
               exp_desc_l.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_l.linear_type = exp_desc->linear_type;
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = exp_desc->linear_type;
               exp_desc_r.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_r.linear_type = exp_desc->linear_type;
            }

            /* reset the linear type to reflect any changes above */
            exp_desc->linear_type = POWER_TYPE(exp_desc_l.linear_type,
                                               exp_desc_r.linear_type);

         }
      }

      exp_desc->type_idx    = exp_desc->linear_type;
      exp_desc->type        = TYP_TYPE(exp_desc->linear_type);

      if (exp_desc->linear_type == Integer_8) {
         /* check whether it should be 'default' typed */

         if (exp_desc_l.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_l.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_l.type_idx;
         }
         else if (exp_desc_r.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_r.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_r.type_idx;
         }
      }

      /* can't have negative real raised to real power */

      if (exp_desc_l.foldable                          &&
          exp_desc_l.type == Real                      &&
          exp_desc_r.type == Real                      &&
          IR_FLD_L(ir_idx) == CN_Tbl_Idx) {

          if (fold_relationals(IR_IDX_L(ir_idx),
                               CN_INTEGER_ZERO_IDX,
                               Lt_Opr)) {

            PRINTMSG(line, 538, Error, col);
            ok = FALSE;
         }
      }

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant = exp_desc_l.constant &&
                             exp_desc_r.constant;
      exp_desc->foldable = exp_desc_l.foldable &&
                             exp_desc_r.foldable;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);


      if (opt_flags.ieeeconform &&
          ! comp_gen_expr       &&
          (exp_desc_l.type == Real ||
           exp_desc_l.type == Complex ||
           exp_desc_r.type == Real ||
           exp_desc_r.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc->rank != 0) {
         /* don't do any folding yet */
      }
      else if (exp_desc->foldable    &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx &&
               ok) {

         if (expr_mode == Initialization_Expr &&
             exp_desc_r.type != Integer)      {

            /* must have integer exponent for init expr */

            PRINTMSG(IR_LINE_NUM_R(ir_idx), 206, Error,
                     IR_COL_NUM_R(ir_idx));
            ok = FALSE;
         }


         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                           (char *)&CN_CONST(IR_IDX_R(ir_idx)),
                            exp_desc_r.type_idx,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            2, IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                     FALSE,
                                                     folded_const);

            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX((*result_opnd))) == Power_Opr) {

      /* exponentiation must be pulled off io lists */
      io_item_must_flatten = TRUE;

      IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
      IR_RANK(ir_idx)          = exp_desc->rank;

      if (IR_RANK(ir_idx)) {
         IR_ARRAY_SYNTAX(ir_idx) = TRUE;
      }
   }

EXIT:

   TRACE (Func_Exit, "power_opr_handler", NULL);

   return(ok);

}  /* power_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Mult_Opr and Div_Opr.                        *|
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

static boolean mult_opr_handler(opnd_type		*result_opnd,
			        expr_arg_type		*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "mult_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   exp_desc->linear_type = MULT_DIV_TYPE(exp_desc_l.linear_type,
                                         exp_desc_r.linear_type);

   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))    {

      if (MULT_DIV_EXTN(exp_desc_l.linear_type,
                        exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else { /* aggragate constant problem here BHJ */

            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = exp_desc->linear_type;
               exp_desc_l.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_l.linear_type = exp_desc->linear_type;
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = exp_desc->linear_type;
               exp_desc_r.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_r.linear_type = exp_desc->linear_type;
            }

            /* reset the linear type to reflect any changes above */
            exp_desc->linear_type = MULT_DIV_TYPE(exp_desc_l.linear_type,
                                                  exp_desc_r.linear_type);
         }
      }

      exp_desc->type_idx = exp_desc->linear_type;
      exp_desc->type     = TYP_TYPE(exp_desc->linear_type);

      if (exp_desc->linear_type == Integer_8) {
         /* check whether it should be 'default' typed */

         if (exp_desc_l.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_l.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_l.type_idx;
         }
         else if (exp_desc_r.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_r.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_r.type_idx;
         }
      }
          

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant = exp_desc_l.constant &&
                             exp_desc_r.constant;
      exp_desc->foldable = exp_desc_l.foldable &&
                             exp_desc_r.foldable;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);


      if ((! target_ieee          ||
           exp_desc->type == Integer)           &&
          exp_desc_r.rank            == 0       &&
          IR_OPR(ir_idx)             == Div_Opr &&
          IR_FLD_R(ir_idx)           == CN_Tbl_Idx) {

          if (fold_relationals(IR_IDX_R(ir_idx),
                               CN_INTEGER_ZERO_IDX,
                               Eq_Opr)) {

            /* division by zero */

            if (comp_gen_expr) {
               PRINTMSG(IR_LINE_NUM_R(ir_idx), 721, Error,
                        IR_COL_NUM_R(ir_idx));
               ok = FALSE;
            }
            else {
               PRINTMSG(IR_LINE_NUM_R(ir_idx), 1649, Warning,
                        IR_COL_NUM_R(ir_idx));
               exp_desc->foldable = FALSE;
               exp_desc->will_fold_later = FALSE;
            }
         }
      }

      if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX((*result_opnd))) == Div_Opr &&
          exp_desc->type == Real                      &&
          on_off_flags.round_integer_divide)          {

         IR_OPR(OPND_IDX((*result_opnd))) = Real_Div_To_Int_Opr;
      }

      if (! ok) {
         /* intentionally blank */
      }
      else if (opt_flags.ieeeconform &&
               ! comp_gen_expr       &&
               (exp_desc_l.type == Real ||
                exp_desc_l.type == Complex ||
                exp_desc_r.type == Real ||
                exp_desc_r.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc->rank != 0) {
         /* don't do any folding yet */
      }
      else if (exp_desc->foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                           (char *)&CN_CONST(IR_IDX_R(ir_idx)),
                            exp_desc_r.type_idx,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            2,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                     FALSE,
                                                     folded_const);

            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
      else if (exp_desc_l.foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx) {

         if (exp_desc_l.type   == Integer            &&
             exp_desc_l.type_idx == exp_desc_r.type_idx) {

            if (compare_cn_and_value(IR_IDX_L(ir_idx), 0, Eq_Opr)) {
               /* fold 0 * i or 0 / i => 0 */
               COPY_OPND((*result_opnd), IR_OPND_L(ir_idx));
               exp_desc->constant = TRUE;
               exp_desc->foldable = TRUE;
            }
            else if (compare_cn_and_value(IR_IDX_L(ir_idx), 1, Eq_Opr) &&
                     IR_OPR(ir_idx)             == Mult_Opr) {
               /* fold 1 * i => i */
               COPY_OPND((*result_opnd), IR_OPND_R(ir_idx));
            }
         }
      }
      else if (exp_desc_r.foldable             &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         if (exp_desc_l.type == Integer &&
             exp_desc_l.type_idx == exp_desc_r.type_idx) {

            if (compare_cn_and_value(IR_IDX_R(ir_idx), 1, Eq_Opr)) {
               /* fold i * 1 or i / 1 => i */
               COPY_OPND((*result_opnd), IR_OPND_L(ir_idx));
            }
            else if (compare_cn_and_value(IR_IDX_R(ir_idx), 0, Eq_Opr) &&
                     IR_OPR(ir_idx)             == Mult_Opr) {
               /* fold i * 0 => 0 */
               COPY_OPND((*result_opnd), IR_OPND_R(ir_idx));
               exp_desc->constant = TRUE;
               exp_desc->foldable = TRUE;
            }
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "mult_opr_handler", NULL);

   return(ok);

}  /* mult_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Minus_Opr.                                   *|
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

static boolean minus_opr_handler(opnd_type		*result_opnd,
				 expr_arg_type		*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "minus_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   exp_desc->linear_type = BIN_SUB_TYPE(exp_desc_l.linear_type,
                                        exp_desc_r.linear_type);

   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))   {

      if (BIN_SUB_EXTN(exp_desc_l.linear_type,
                       exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd,  FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {
            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = exp_desc->linear_type;
               exp_desc_l.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_l.linear_type = exp_desc->linear_type;
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = exp_desc->linear_type;
               exp_desc_r.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_r.linear_type = exp_desc->linear_type;
            }

            /* reset the linear type to reflect any changes above */
            exp_desc->linear_type = BIN_SUB_TYPE(exp_desc_l.linear_type,
                                                 exp_desc_r.linear_type);
         }
      }

      exp_desc->type_idx    = exp_desc->linear_type;
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      if (exp_desc->linear_type == Integer_8) {
         /* check whether it should be 'default' typed */

         if (exp_desc_l.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_l.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_l.type_idx;
         }
         else if (exp_desc_r.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_r.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_r.type_idx;
         }
      }

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant = exp_desc_l.constant &&
                             exp_desc_r.constant;
      exp_desc->foldable = exp_desc_l.foldable &&
                             exp_desc_r.foldable;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);

      if (opt_flags.ieeeconform &&
          ! comp_gen_expr       &&
          (exp_desc_l.type == Real ||
           exp_desc_l.type == Complex ||
           exp_desc_r.type == Real ||
           exp_desc_r.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc->rank != 0) {
         /* don't do any folding yet */
      }
      else if (exp_desc->foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                           (char *)&CN_CONST(IR_IDX_R(ir_idx)),
                            exp_desc_r.type_idx,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            2,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                     FALSE,
                                                     folded_const);

            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
      else if (exp_desc_r.foldable            &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         if (exp_desc_l.type   == Integer            &&
             exp_desc_l.type_idx == exp_desc_r.type_idx) {

            if (compare_cn_and_value(IR_IDX_R(ir_idx), 0, Eq_Opr)) {
               /* fold i + 0 or i - 0 => i */
               COPY_OPND((*result_opnd), IR_OPND_L(ir_idx));
            }
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "minus_opr_handler", NULL);

   return(ok);

}  /* minus_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Plus_Opr.                                    *|
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

static boolean plus_opr_handler(opnd_type		*result_opnd,
			        expr_arg_type		*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "plus_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   exp_desc->linear_type = BIN_ADD_TYPE(exp_desc_l.linear_type,
                                        exp_desc_r.linear_type);
   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))   {

      if (BIN_ADD_EXTN(exp_desc_l.linear_type,
                       exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd,  FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {
            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = exp_desc->linear_type;
               exp_desc_l.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_l.linear_type = exp_desc->linear_type;
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = exp_desc->linear_type;
               exp_desc_r.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_r.linear_type = exp_desc->linear_type;
            }

            /* reset the linear type to reflect any changes above */
            exp_desc->linear_type = BIN_ADD_TYPE(exp_desc_l.linear_type,
                                                 exp_desc_r.linear_type);
         }
      }

      exp_desc->type_idx    = exp_desc->linear_type;
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      if (exp_desc->linear_type == Integer_8) {
         /* check whether it should be 'default' typed */

         if (exp_desc_l.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_l.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_l.type_idx;
         }
         else if (exp_desc_r.linear_type == Integer_8 &&
             TYP_DESC(exp_desc_r.type_idx) != Default_Typed) {
            exp_desc->type_idx = exp_desc_r.type_idx;
         }
      }

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant = exp_desc_l.constant &&
                             exp_desc_r.constant;
      exp_desc->foldable = exp_desc_l.foldable &&
                             exp_desc_r.foldable;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);

      if (opt_flags.ieeeconform &&
          ! comp_gen_expr       &&
          (exp_desc_l.type == Real ||
           exp_desc_l.type == Complex ||
           exp_desc_r.type == Real ||
           exp_desc_r.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc->rank != 0) {
         /* don't do any folding yet */
      }
      else if (exp_desc->foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {


         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                           (char *)&CN_CONST(IR_IDX_R(ir_idx)),
                            exp_desc_r.type_idx,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            2,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(
                                                 exp_desc->type_idx,
                                                 FALSE,
                                                 folded_const);

            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
      else if (exp_desc_l.foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx) {

         if (exp_desc_l.type   == Integer            &&
             exp_desc_l.type_idx == exp_desc_r.type_idx) {

            if (compare_cn_and_value(IR_IDX_L(ir_idx), 0, Eq_Opr)) {
               /* fold 0 + i => i */
               COPY_OPND((*result_opnd), IR_OPND_R(ir_idx));
            }
         }
      }
      else if (exp_desc_r.foldable             &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         if (exp_desc_l.type   == Integer            &&
             exp_desc_l.type_idx == exp_desc_r.type_idx) {

            if (compare_cn_and_value(IR_IDX_R(ir_idx), 0, Eq_Opr)) {
               /* fold i + 0 or i - 0 => i */
               COPY_OPND((*result_opnd), IR_OPND_L(ir_idx));
            }
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "plus_opr_handler", NULL);

   return(ok);

}  /* plus_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Concat_Opr.                                  *|
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

static boolean concat_opr_handler(opnd_type		*result_opnd,
				  expr_arg_type		*exp_desc)

{
   char                *char_ptr1;
   char                *char_ptr2;
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long			i;
   int			ir_idx;
   long_type		length[MAX_WORDS_FOR_INTEGER];
   int			line;
   int			list_idx;
   int			k;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			plus_idx;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "concat_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;

#ifdef KEY /* Bug 3273 */
   /* If we already visited this node once and converted it from a node
    * having two children into a node having one child which is itself a
    * list of children, then on the second visit (e.g. due to a call to
    * "process_deferred_io_list") the code in concat_opr_handler will fail.
    * So skip it.
    */
   if (OPND_FLD(IR_OPND_L(ir_idx)) == IL_Tbl_Idx) {
     return ok;
     }
#endif /* KEY Bug 3273 */
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   if (! ok) {
      goto EXIT;
   }

   if (exp_desc_l.type == Character &&
       exp_desc_r.type == Character &&
       (exp_desc_r.rank == exp_desc_l.rank ||
        exp_desc_r.rank * exp_desc_l.rank == 0)) {

      exp_desc->type   = Character;

      /* aux_type is not calculated unless it's for a fold */

      exp_desc->type_idx = exp_desc_l.type_idx;

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant   = exp_desc_l.constant &&
                               exp_desc_r.constant;
      exp_desc->foldable   = exp_desc_l.foldable &&
                               exp_desc_r.foldable;

      exp_desc->has_symbolic = exp_desc_l.has_symbolic || 
                               exp_desc_r.has_symbolic;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);

      if (exp_desc->foldable             &&
          IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
          IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         /* fold the concat in line */

         type_idx = CG_INTEGER_DEFAULT_TYPE;

         if (folder_driver((char *) &CN_CONST(TYP_IDX(exp_desc_r.type_idx)),
                                     CN_TYPE_IDX(TYP_IDX(exp_desc_r.type_idx)),
                           (char *) &CN_CONST(TYP_IDX(exp_desc_l.type_idx)),
                                     CN_TYPE_IDX(TYP_IDX(exp_desc_l.type_idx)),
                                     length,
                                    &type_idx,
                                     line,
                                     col,
                                     2,
                                     Plus_Opr)) {
         }

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

         TYP_TYPE(TYP_WORK_IDX)       = Character;
         TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)        = CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)        = ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                                                      FALSE,
                                                      length);
         exp_desc->type_idx         = ntr_type_tbl();
         exp_desc->char_len.fld     = TYP_FLD(exp_desc->type_idx);
         exp_desc->char_len.idx     = TYP_IDX(exp_desc->type_idx);
         OPND_LINE_NUM(exp_desc->char_len) = line;
         OPND_COL_NUM(exp_desc->char_len) = col;

         OPND_FLD((*result_opnd))      = CN_Tbl_Idx;
         OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);

         /* Set up the new const table entry.  Pass ntr_const_tbl */
         /* a null pointer so the caller can move the constant.   */

         OPND_IDX((*result_opnd))= ntr_const_tbl(exp_desc->type_idx,
                                                 TRUE,
                                                 NULL);

         /* BRIANJ - String manipulation */

         /* copy the first string in */

         char_ptr1  = (char *)&CN_CONST(OPND_IDX((*result_opnd)));
         char_ptr2  = (char *)&CN_CONST(IR_IDX_L(ir_idx));
         k          = 0;

         for (i=0; i < CN_INT_TO_C(TYP_IDX(exp_desc_l.type_idx)); i++){
            char_ptr1[k] = char_ptr2[i];
            k++;
         }

         /* copy the second string in */

         char_ptr2 = (char *)&CN_CONST(IR_IDX_R(ir_idx));

         for (i=0; i < CN_INT_TO_C(TYP_IDX(exp_desc_r.type_idx)); i++){
            char_ptr1[k] = char_ptr2[i];
            k++;
         }

         /* fill in the rest of a word with blanks */

         while (k % TARGET_CHARS_PER_WORD != 0) {
            char_ptr1[k] = ' ';
            k++;
         }
      }
      else {

        io_item_must_flatten = TRUE;

        NTR_IR_TBL(plus_idx);
        IR_OPR(plus_idx) = Plus_Opr;
        IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
        IR_LINE_NUM(plus_idx) = line;
        IR_COL_NUM(plus_idx)  = col;
        COPY_OPND(IR_OPND_L(plus_idx), exp_desc_l.char_len);
        COPY_OPND(IR_OPND_R(plus_idx), exp_desc_r.char_len);

        exp_desc->char_len.fld = IR_Tbl_Idx;
        exp_desc->char_len.idx = plus_idx;

        if (exp_desc_l.char_len.fld == CN_Tbl_Idx &&
            exp_desc_r.char_len.fld == CN_Tbl_Idx) {

           COPY_OPND(opnd, exp_desc->char_len);
           exp_desc_l.rank = 0;
           ok = expr_semantics(&opnd, &exp_desc_l);
           COPY_OPND(exp_desc->char_len, opnd);
        }

        /* switch to n-ary concat */
        if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
            IR_OPR(IR_IDX_L(ir_idx)) == Concat_Opr) {

           COPY_OPND(IR_OPND_L(ir_idx),
                     IR_OPND_L(IR_IDX_L(ir_idx)));

           list_idx = IR_IDX_L(ir_idx);
           while (IL_NEXT_LIST_IDX(list_idx)) {
              list_idx = IL_NEXT_LIST_IDX(list_idx);
           }

           NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
           IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
           list_idx = IL_NEXT_LIST_IDX(list_idx);
           COPY_OPND(IL_OPND(list_idx), IR_OPND_R(ir_idx));
           IR_LIST_CNT_L(ir_idx)++;
           IR_FLD_R(ir_idx) = NO_Tbl_Idx;
           IR_IDX_R(ir_idx) = NULL_IDX;
        }
        else {
           NTR_IR_LIST_TBL(list_idx);
           COPY_OPND(IL_OPND(list_idx), IR_OPND_L(ir_idx));
           IR_FLD_L(ir_idx) = IL_Tbl_Idx;
           IR_IDX_L(ir_idx) = list_idx;
           IR_LIST_CNT_L(ir_idx) = 2;
           NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
           IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
           list_idx = IL_NEXT_LIST_IDX(list_idx);
           COPY_OPND(IL_OPND(list_idx), IR_OPND_R(ir_idx));
           IR_FLD_R(ir_idx) = NO_Tbl_Idx;
           IR_IDX_R(ir_idx) = NULL_IDX;
        }
      }

      if (exp_desc->foldable                               &&
          compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                               MAX_CHARS_IN_TYPELESS,
                               Le_Opr)) {
         exp_desc->linear_type = Short_Char_Const;
      }
      else {
         /* assume one byte character for now */
         exp_desc->linear_type = Character_1;
      }

      type_tbl[TYP_WORK_IDX]        = type_tbl[exp_desc->type_idx];
      TYP_LINEAR(TYP_WORK_IDX)      = exp_desc->linear_type;
      exp_desc->type_idx            = ntr_type_tbl();

   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc_l.type != Character ||
                             exp_desc_r.type != Character),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }


EXIT:

   TRACE (Func_Exit, "concat_opr_handler", NULL);

   return(ok);

}  /* concat_opr_handler */
#ifdef KEY /* Bug 5710 */
/*
 * If we're allowing .eq., .ne., ==, and /= on logical operands as an
 * extension, and this is a case of that, return true else false. When
 * we return true, also set exp_desc->linear_type to the logical result type
 * of the operation.
 */
int eq_ne_on_logical(expr_arg_type *exp_desc, expr_arg_type *exp_desc_l,
   expr_arg_type *exp_desc_r)
{
   if (on_off_flags.issue_ansi_messages) {
     return FALSE;
   }
   int result = EQ_NE_ON_LOGICAL(exp_desc_l->linear_type,
     exp_desc_r->linear_type);
   if (Err_Res == result) {
     return FALSE;
   }
   if (exp_desc) {
     exp_desc->linear_type = result;
   }
   return TRUE;
}

/*
 * Do the work for an intrinsic operator within eq_opr_handler. This simply
 * encapsulates some code that would otherwise need to appear in two different
 * spots within that function.
 */
static void
handle_intrinsic_opr(
   expr_arg_type *exp_desc,
   expr_arg_type *exp_desc_l,
   expr_arg_type *exp_desc_r,
   int line,
   int col,
   boolean *ok,
   int *type_idx,
   opnd_type *result_opnd,
   int ir_idx,
   long_type folded_const[])
{
   exp_desc->type_idx    = exp_desc->linear_type;
   exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

   if (! bin_array_syntax_check(exp_desc_l, exp_desc_r,
				exp_desc, line, col))     {
      *ok = FALSE;
   }

   exp_desc->constant = exp_desc_l->constant &&
			  exp_desc_r->constant;
   exp_desc->foldable = exp_desc_l->foldable &&
			  exp_desc_r->foldable;

   exp_desc->will_fold_later = (exp_desc_l->will_fold_later &
				exp_desc_r->will_fold_later)  |
			       (exp_desc_l->will_fold_later &
				exp_desc_r->foldable)         |
			       (exp_desc_l->foldable &
				exp_desc_r->will_fold_later);

   if (opt_flags.ieeeconform &&
       ! comp_gen_expr       &&
       (exp_desc_l->type == Real ||
	exp_desc_l->type == Complex ||
	exp_desc_r->type == Real ||
	exp_desc_r->type == Complex)) {

      /* don't fold real arithmatic under ieeeconform */

      exp_desc->foldable = FALSE;
      exp_desc->will_fold_later = FALSE;
   }
   else if (exp_desc->foldable             &&
	    IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
	    IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

      *type_idx = exp_desc->type_idx;

      if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
			 exp_desc_l->type_idx,
			(char *)&CN_CONST(IR_IDX_R(ir_idx)),
			 exp_desc_r->type_idx,
			 folded_const,
			type_idx,
			 line,
			 col,
			 2,
			 IR_OPR(ir_idx))) {

	 exp_desc->type_idx    = *type_idx;
	 OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
	 OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
						  FALSE,
						  folded_const);

	 exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
	 OPND_LINE_NUM((*result_opnd)) = line;
	 OPND_COL_NUM((*result_opnd))  = col;
      }
      else {
	 *ok = FALSE;
      }
   }
   else if (exp_desc_l->type == Character            &&
	    exp_desc_r->type == Character            &&
	    exp_desc_l->char_len.fld == CN_Tbl_Idx   &&
	    CN_INT_TO_C(exp_desc_l->char_len.idx) == 0  &&
	    exp_desc_r->char_len.fld == CN_Tbl_Idx   &&
	    CN_INT_TO_C(exp_desc_r->char_len.idx) == 0) {

      /* left and right are zero length char */

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = set_up_logical_constant(folded_const, 
							 exp_desc->type_idx, 
			    (IR_OPR(ir_idx) == Eq_Opr) ? TRUE_VALUE : 
							 FALSE_VALUE,
							 TRUE);



      OPND_LINE_NUM((*result_opnd)) = line;
      OPND_COL_NUM((*result_opnd))  = col;

      if (exp_desc->rank) {
	 make_logical_array_tmp(result_opnd,
				exp_desc);
      }
   }
}
#endif /* KEY Bug 5710 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Eq_Opr and Ne_Opr.                           *|
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

static boolean eq_opr_handler(opnd_type		*result_opnd,
			      expr_arg_type	*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "eq_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic ||
                               exp_desc_r.has_symbolic;

   exp_desc->linear_type = EQ_NE_TYPE(exp_desc_l.linear_type,
                                      exp_desc_r.linear_type);

   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))   {

      if (EQ_NE_EXTN(exp_desc_l.linear_type,
                     exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {
            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               type_idx = exp_desc_r.type_idx;

               if (exp_desc_r.type == Character ||
                   exp_desc_r.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = type_idx;
               exp_desc_l.type        = TYP_TYPE(type_idx);
               exp_desc_l.linear_type = TYP_LINEAR(type_idx);
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               type_idx = exp_desc_l.type_idx;

               if (exp_desc_l.type == Character ||
                   exp_desc_l.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = type_idx;
               exp_desc_r.type        = TYP_TYPE(type_idx);
               exp_desc_r.linear_type = TYP_LINEAR(type_idx);
            }

            /* reset the linear type to reflect any changes above */
            exp_desc->linear_type = EQ_NE_TYPE(exp_desc_l.linear_type,
                                               exp_desc_r.linear_type);
         }
      }

#ifdef KEY /* Bug 5710 */
      handle_intrinsic_opr(exp_desc, &exp_desc_l, &exp_desc_r, line, col,
        &ok, &type_idx, result_opnd, ir_idx, folded_const);
#else /* KEY Bug 5710 */
      /* Contents of handle_intrinsic_opr was previously inline here */
#endif /* KEY Bug 5710 */
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
#ifdef KEY /* Bug 5710 */
   /* Okay, this isn't a standard intrinsic use of .eq. or .ne.; nor is
    * it an extension of such an operator via the "interface operator"
    * mechanism; so check whether it's the common extension to the ANSI
    * standard which allows those operators to be used in place of .eqv.
    * and .neqv. with logical operands.
    */
   else if (eq_ne_on_logical(exp_desc, &exp_desc_l, &exp_desc_r)) {
      handle_intrinsic_opr(exp_desc, &exp_desc_l, &exp_desc_r, line, col,
        &ok, &type_idx, result_opnd, ir_idx, folded_const);
   }
#endif /* KEY Bug 5710 */
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "eq_opr_handler", NULL);

   return(ok);

}  /* eq_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Lg_Opr.                                      *|
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

static boolean lg_opr_handler(opnd_type		*result_opnd,
			      expr_arg_type	*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "lg_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   exp_desc->linear_type = LG_TYPE(exp_desc_l.linear_type,
                                   exp_desc_r.linear_type);

   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))   {

      if (LG_EXTN(exp_desc_l.linear_type,
                  exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {
            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               type_idx = exp_desc_r.type_idx;

               if (exp_desc_r.type == Character ||
                   exp_desc_r.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = type_idx;
               exp_desc_l.type        = TYP_TYPE(type_idx);
               exp_desc_l.linear_type = TYP_LINEAR(type_idx);
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               type_idx = exp_desc_l.type_idx;

               if (exp_desc_l.type == Character ||
                   exp_desc_l.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = type_idx;
               exp_desc_r.type        = TYP_TYPE(type_idx);
               exp_desc_r.linear_type = TYP_LINEAR(type_idx);
            }

            /* reset the linear type to reflect any changes above */
            exp_desc->linear_type = LG_TYPE(exp_desc_l.linear_type,
                                            exp_desc_r.linear_type);
         }
      }

      exp_desc->type_idx    = exp_desc->linear_type;
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant = exp_desc_l.constant &&
                             exp_desc_r.constant;
      exp_desc->foldable = exp_desc_l.foldable &&
                             exp_desc_r.foldable;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);


      if (! target_ieee) {
         /* change to .NE. on non ieee machines */
         IR_OPR(ir_idx) = Ne_Opr;
      }
      else {
         /* for now, do not try to fold these */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }

      if (opt_flags.ieeeconform &&
          ! comp_gen_expr       &&
          (exp_desc_l.type == Real ||
           exp_desc_l.type == Complex ||
           exp_desc_r.type == Real ||
           exp_desc_r.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc->foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                           (char *)&CN_CONST(IR_IDX_R(ir_idx)),
                            exp_desc_r.type_idx,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            2,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                     FALSE,
                                                     folded_const);

            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "lg_opr_handler", NULL);

   return(ok);

}  /* lg_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Lt_Opr, Le_Opr, Gt_Opr, and Ge_Opr.          *|
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

static boolean lt_opr_handler(opnd_type		*result_opnd,
			      expr_arg_type	*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "lt_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   exp_desc->linear_type = GT_LT_TYPE(exp_desc_l.linear_type,
                                      exp_desc_r.linear_type);

   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))   {

      if (GT_LT_EXTN(exp_desc_l.linear_type,
                     exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {
            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               type_idx = exp_desc_r.type_idx;

               if (exp_desc_r.type == Character ||
                   exp_desc_r.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = type_idx;
               exp_desc_l.type        = TYP_TYPE(type_idx);
               exp_desc_l.linear_type = TYP_LINEAR(type_idx);
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               type_idx = exp_desc_l.type_idx;

               if (exp_desc_l.type == Character ||
                   exp_desc_l.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = type_idx;
               exp_desc_r.type        = TYP_TYPE(type_idx);
               exp_desc_r.linear_type = TYP_LINEAR(type_idx);
            }

            /* reset the linear type to reflect any changes above */
            exp_desc->linear_type = GT_LT_TYPE(exp_desc_l.linear_type,
                                               exp_desc_r.linear_type);
         }
      }

      exp_desc->type_idx    = exp_desc->linear_type;
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant = exp_desc_l.constant &&
                             exp_desc_r.constant;
      exp_desc->foldable = exp_desc_l.foldable &&
                             exp_desc_r.foldable;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);

      if (opt_flags.ieeeconform &&
          ! comp_gen_expr       &&
          (exp_desc_l.type == Real ||
           exp_desc_l.type == Complex ||
           exp_desc_r.type == Real ||
           exp_desc_r.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc->foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                           (char *)&CN_CONST(IR_IDX_R(ir_idx)),
                            exp_desc_r.type_idx,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            2,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                     FALSE,
                                                     folded_const);

            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
      else if (exp_desc_l.type == Character            &&
               exp_desc_r.type == Character            &&
               exp_desc_l.char_len.fld == CN_Tbl_Idx   &&
               CN_INT_TO_C(exp_desc_l.char_len.idx) == 0  &&
               exp_desc_r.char_len.fld == CN_Tbl_Idx   &&
               CN_INT_TO_C(exp_desc_r.char_len.idx) == 0) {

         /* left and right are zero length char */

         if (IR_OPR(ir_idx) == Ge_Opr || IR_OPR(ir_idx) == Le_Opr) {

            /* result is TRUE */

            OPND_IDX((*result_opnd)) = set_up_logical_constant(folded_const, 
                                                      exp_desc->type_idx, 
                                                      TRUE_VALUE,
                                                      TRUE);
         }
         else { /* result is FALSE */
            OPND_IDX((*result_opnd)) = set_up_logical_constant(folded_const, 
                                                      exp_desc->type_idx, 
                                                      FALSE_VALUE,
                                                      TRUE);
         }

         OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
         OPND_LINE_NUM((*result_opnd)) = line;
         OPND_COL_NUM((*result_opnd))  = col;

         if (exp_desc->rank) {
            make_logical_array_tmp(result_opnd,
                                   exp_desc);
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "lt_opr_handler", NULL);

   return(ok);

}  /* lt_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Not_Opr.                                     *|
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

static boolean not_opr_handler(opnd_type		*result_opnd,
			       expr_arg_type		*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_in_call_list;
   int			type_idx;


   TRACE (Func_Entry, "not_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic;

   exp_desc->linear_type = NOT_TYPE(exp_desc_l.linear_type);

   if (exp_desc->linear_type != Err_Res) {

      if (NOT_EXTN(exp_desc_l.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {
            /* change opr to bnot */
            IR_OPR(ir_idx) = Bnot_Opr;
            PRINTMSG(IR_LINE_NUM(ir_idx), 395, Ansi,
                     IR_COL_NUM(ir_idx));

            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         exp_desc->linear_type,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = exp_desc->linear_type;
               exp_desc_l.type        = TYP_TYPE(exp_desc->linear_type);
               exp_desc_l.linear_type = exp_desc->linear_type;

               /* reset the linear type to reflect any change from above */
               exp_desc->linear_type = NOT_TYPE(exp_desc_l.linear_type);
            }
         }
      }

      exp_desc->type_idx = exp_desc->linear_type;
      exp_desc->type     = TYP_TYPE(exp_desc->type_idx);
      exp_desc->rank     = exp_desc_l.rank;
      exp_desc->constant = exp_desc_l.constant;
      exp_desc->foldable = exp_desc_l.foldable;
      exp_desc->will_fold_later = exp_desc_l.will_fold_later;

      COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,exp_desc_l.rank);

      if (opt_flags.ieeeconform &&
          ! comp_gen_expr       &&
          (exp_desc_l.type == Real ||
           exp_desc_l.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc_l.foldable             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                            NULL,
                            NULL_IDX,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            1,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx       = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                     FALSE,
                                                     folded_const);

            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "not_opr_handler", NULL);

   return(ok);

}  /* not_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the And_Opr, Or_Opr, Eqv_Opr, Neqv_Opr.          *|
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

static boolean and_opr_handler(opnd_type		*result_opnd,
			       expr_arg_type		*exp_desc)

{
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
# if defined(_HIGH_LEVEL_IF_FORM)
# ifdef KEY /* Bug 10177 */
   boolean		save_has_present_opr = FALSE;
# else /* KEY Bug 10177 */
   boolean		save_has_present_opr;
# endif /* KEY Bug 10177 */
# endif
   boolean		save_in_call_list;
#ifdef KEY /* Bug 10177 */
   int                  save_number_of_functions = 0;
   int                  save_number_of_functions_l = 0;
#else /* KEY Bug 10177 */
   int                  save_number_of_functions;
   int                  save_number_of_functions_l;
#endif /* KEY Bug 10177 */
   int			type_idx;


   TRACE (Func_Entry, "and_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
# if defined(_HIGH_LEVEL_IF_FORM)
   if (in_branch_true) {
      if (opt_flags.short_circuit_lvl == Short_Circuit_Present) {
         save_has_present_opr = has_present_opr;
         has_present_opr = FALSE;
      }
      else if (opt_flags.short_circuit_lvl == Short_Circuit_Functions) {
         save_number_of_functions = number_of_functions;
         number_of_functions = 0;
      }
   }
# else
   if (in_branch_true) {
      save_number_of_functions = number_of_functions;
      number_of_functions = 0;
   }
# endif

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

# if defined(_HIGH_LEVEL_IF_FORM)
   if (in_branch_true) {
      if (opt_flags.short_circuit_lvl == Short_Circuit_Present) {
         save_has_present_opr |= has_present_opr;
         IR_SHORT_CIRCUIT_L(ir_idx) = has_present_opr;
         has_present_opr = FALSE;
      }
      else if (opt_flags.short_circuit_lvl == Short_Circuit_Functions) {
         save_number_of_functions_l = number_of_functions;
         number_of_functions = 0;
      }
   }
# else
   if (in_branch_true) {
      save_number_of_functions_l = number_of_functions;
      number_of_functions = 0;
   }
# endif

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

# if defined(_HIGH_LEVEL_IF_FORM)
   if (in_branch_true) {
      if (opt_flags.short_circuit_lvl == Short_Circuit_Present) {
         save_has_present_opr |= has_present_opr;
         IR_SHORT_CIRCUIT_R(ir_idx) = has_present_opr;
         has_present_opr = save_has_present_opr;
      }
      else if (opt_flags.short_circuit_lvl == Short_Circuit_Functions) {

         if (save_number_of_functions_l == number_of_functions &&
             number_of_functions == 0)                         {
            /* no functions */
            IR_SHORT_CIRCUIT_L(ir_idx) = FALSE;
            IR_SHORT_CIRCUIT_R(ir_idx) = FALSE;
         }
         else if (save_number_of_functions_l <= number_of_functions) {
            IR_SHORT_CIRCUIT_R(ir_idx) = TRUE;
            IR_SHORT_CIRCUIT_L(ir_idx) = FALSE;
         }
         else {
            IR_SHORT_CIRCUIT_L(ir_idx) = TRUE;
            IR_SHORT_CIRCUIT_R(ir_idx) = FALSE;
         }

         number_of_functions += save_number_of_functions_l +
                                save_number_of_functions;
      }
   }
# else
   if (in_branch_true) {

      if (save_number_of_functions_l == number_of_functions &&
          number_of_functions == 0)                         {
         /* no functions */
         IR_SHORT_CIRCUIT_L(ir_idx) = FALSE;
         IR_SHORT_CIRCUIT_R(ir_idx) = FALSE;
      }
      else if (save_number_of_functions_l <= number_of_functions) {
         IR_SHORT_CIRCUIT_R(ir_idx) = TRUE;
         IR_SHORT_CIRCUIT_L(ir_idx) = FALSE;
      }
      else {
         IR_SHORT_CIRCUIT_L(ir_idx) = TRUE;
         IR_SHORT_CIRCUIT_R(ir_idx) = FALSE;
      }

      number_of_functions += save_number_of_functions_l +
                             save_number_of_functions;
   }
# endif

   if (!ok) {
      goto EXIT;
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   exp_desc->linear_type = AND_OR_TYPE(exp_desc_l.linear_type,
                                       exp_desc_r.linear_type);

   if (exp_desc->linear_type != Err_Res &&
       (exp_desc_l.rank == exp_desc_r.rank ||
        exp_desc_l.rank * exp_desc_r.rank == 0))   {

      if (AND_OR_EXTN(exp_desc_l.linear_type,
                      exp_desc_r.linear_type)) {
         /* check for defined operator */
         if (resolve_ext_opr(result_opnd, FALSE, save_in_call_list,
                             FALSE,
                             &ok,
                             &exp_desc_l, &exp_desc_r)) {

            (*exp_desc) = exp_desc_l;

            goto EXIT;
         }
         else {

            /* change to binary oper */
            switch (IR_OPR(ir_idx)) {
               case And_Opr  :
                  IR_OPR(ir_idx) = Band_Opr;
                  break;
               case Or_Opr   :
                  IR_OPR(ir_idx) = Bor_Opr;
                  break;
               case Eqv_Opr  :
                  IR_OPR(ir_idx) = Beqv_Opr;
                  break;
               case Neqv_Opr :
                  IR_OPR(ir_idx) = Bneqv_Opr;
                  break;
            }
            PRINTMSG(IR_LINE_NUM(ir_idx), 395, Ansi,
                     IR_COL_NUM(ir_idx));

            if (exp_desc_l.type == Character ||
                exp_desc_l.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_l.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }

               type_idx = exp_desc_r.type_idx;

               if (exp_desc_r.type == Character ||
                   exp_desc_r.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_l.type_idx    = type_idx;
               exp_desc_l.type        = TYP_TYPE(type_idx);
               exp_desc_l.linear_type = TYP_LINEAR(type_idx);
            }

            if (exp_desc_r.type == Character ||
                exp_desc_r.linear_type == Short_Typeless_Const) {

               find_opnd_line_and_column((opnd_type *)
                                         &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_col);

               if (exp_desc_r.type == Character) {
                  PRINTMSG(opnd_line, 161, Ansi, opnd_col);
               }


               type_idx = exp_desc_l.type_idx;

               if (exp_desc_l.type == Character ||
                   exp_desc_l.type == Typeless) {
                  type_idx = INTEGER_DEFAULT_TYPE;
               }

               IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                         type_idx,
                                                         opnd_line,
                                                         opnd_col);

               exp_desc_r.type_idx    = type_idx;
               exp_desc_r.type        = TYP_TYPE(type_idx);
               exp_desc_r.linear_type = TYP_LINEAR(type_idx);
            }

            /* reset the linear type to reflect any change from above */
            exp_desc->linear_type = AND_OR_TYPE(exp_desc_l.linear_type,
                                                exp_desc_r.linear_type);

            if (num_host_wds[exp_desc_l.linear_type] !=
                num_host_wds[exp_desc_r.linear_type]) {

               PRINTMSG(IR_LINE_NUM(ir_idx),
                        1188,
                        Error,
                        IR_COL_NUM(ir_idx));
               ok = FALSE;
            }
         }
      }

      exp_desc->type_idx = exp_desc->linear_type;
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);

      if (! bin_array_syntax_check(&exp_desc_l, &exp_desc_r,
                                   exp_desc, line, col))     {
         ok = FALSE;
      }

      exp_desc->constant = exp_desc_l.constant && exp_desc_r.constant;
      exp_desc->foldable = exp_desc_l.foldable && exp_desc_r.foldable;

      exp_desc->will_fold_later = (exp_desc_l.will_fold_later &
                                   exp_desc_r.will_fold_later)  |
                                  (exp_desc_l.will_fold_later &
                                   exp_desc_r.foldable)         |
                                  (exp_desc_l.foldable &
                                   exp_desc_r.will_fold_later);


      if (opt_flags.ieeeconform &&
          ! comp_gen_expr       &&
          (exp_desc_l.type == Real ||
           exp_desc_l.type == Complex ||
           exp_desc_r.type == Real ||
           exp_desc_r.type == Complex)) {

         /* don't fold real arithmatic under ieeeconform */

         exp_desc->foldable = FALSE;
         exp_desc->will_fold_later = FALSE;
      }
      else if (exp_desc->foldable             &&
               ok                             &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
               IR_FLD_R(ir_idx) == CN_Tbl_Idx) {

         type_idx = exp_desc->type_idx;

         if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                            exp_desc_l.type_idx,
                           (char *)&CN_CONST(IR_IDX_R(ir_idx)),
                            exp_desc_r.type_idx,
                            folded_const,
                           &type_idx,
                            line,
                            col,
                            2,
                            IR_OPR(ir_idx))) {

            exp_desc->type_idx    = type_idx;
            OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
            OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                     FALSE,
                                                     folded_const);

            OPND_LINE_NUM((*result_opnd)) = line;
            OPND_COL_NUM((*result_opnd))  = col;
         }
         else {
            ok = FALSE;
         }
      }
   }
   else if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                            (exp_desc->linear_type == Err_Res),
                            &ok,
                            &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;

      goto EXIT;
   }
   else {
      ok = FALSE;
   }

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (IR_RANK(ir_idx)) {
      IR_ARRAY_SYNTAX(ir_idx) = TRUE;
   }

EXIT:

   TRACE (Func_Exit, "and_opr_handler", NULL);

   return(ok);

}  /* and_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Defined_Un_Opr.                              *|
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

static boolean defined_un_opr_handler(opnd_type		*result_opnd,
				      expr_arg_type	*exp_desc)

{
   int			attr_idx;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   int			ir_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   boolean		save_in_call_list;


   TRACE (Func_Entry, "defined_un_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   /* Resolve attr link on interface operator attr */

   attr_idx               = IR_IDX_L(ir_idx);
   AT_LOCKED_IN(attr_idx) = TRUE;

   while (AT_ATTR_LINK(attr_idx)           &&
          ! AT_IGNORE_ATTR_LINK(attr_idx)) {

      attr_idx                 = AT_ATTR_LINK(attr_idx);
      AT_LOCKED_IN(attr_idx)   = TRUE;
   }

   IR_IDX_L(ir_idx) = attr_idx;

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   exp_desc->has_symbolic	= exp_desc_l.has_symbolic;

   /* resolve operator */
   if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                       FALSE,
                       &ok,
                       &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;
   }
   else {
      ok = FALSE;
   }

   TRACE (Func_Exit, "defined_un_opr_handler", NULL);

   return(ok);

}  /* defined_un_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Defined_Bin_Opr.                             *|
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

static boolean defined_bin_opr_handler(opnd_type		*result_opnd,
				       expr_arg_type		*exp_desc)

{
   int			attr_idx;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   int			ir_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   boolean		save_in_call_list;


   TRACE (Func_Entry, "defined_bin_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   /* Resolve attr link on interface operator attr */

   attr_idx               = IR_IDX_L(ir_idx);
   AT_LOCKED_IN(attr_idx) = TRUE;

   while (AT_ATTR_LINK(attr_idx)           &&
          ! AT_IGNORE_ATTR_LINK(attr_idx)) {

      attr_idx                 = AT_ATTR_LINK(attr_idx);
      AT_LOCKED_IN(attr_idx)   = TRUE;
   }

   IR_IDX_L(ir_idx) = attr_idx;

   COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IL_OPND(IR_IDX_R(ir_idx)), opnd);

   COPY_OPND(opnd, IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))));
   exp_desc_r.rank = 0;
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))), opnd);

   /* resolve operator */
   if (resolve_ext_opr(result_opnd, TRUE, save_in_call_list,
                       FALSE,
                       &ok,
                       &exp_desc_l, &exp_desc_r)) {

      (*exp_desc) = exp_desc_l;
   }
   else {
      ok = FALSE;
   }


   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   TRACE (Func_Exit, "defined_bin_opr_handler", NULL);

   return(ok);

}  /* defined_bin_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Max_Opr and Min_Opr.                         *|
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

static boolean max_opr_handler(opnd_type		*result_opnd,
			       expr_arg_type		*exp_desc)

{
#ifdef KEY /* Bug 10177 */
   int			comp_idx = 0;
#else /* KEY Bug 10177 */
   int			comp_idx;
#endif /* KEY Bug 10177 */
   expr_arg_type	exp_desc_l;
   int			ir_idx;
   int			list_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   boolean		save_in_call_list;


   TRACE (Func_Entry, "max_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   save_in_call_list = in_call_list;  /* BRIANJ - set but not used */
   in_call_list = FALSE;

   /* these are only compiler gen'd max and min */

   list_idx = IR_IDX_L(ir_idx);

   COPY_OPND(opnd, IL_OPND(list_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IL_OPND(list_idx), opnd);

   /* assumes that these are all scalar things */

   exp_desc->has_symbolic    = exp_desc_l.has_symbolic;
   exp_desc->constant        = exp_desc_l.constant;
   exp_desc->foldable        = exp_desc_l.foldable;
   exp_desc->will_fold_later = exp_desc_l.will_fold_later;

   if (exp_desc_l.type == Typeless) {
      exp_desc->type        = Integer;
      exp_desc->linear_type = CG_INTEGER_DEFAULT_TYPE;
      exp_desc->type_idx    = CG_INTEGER_DEFAULT_TYPE;
   }
   else {
      exp_desc->type        = exp_desc_l.type;
      exp_desc->linear_type = exp_desc_l.linear_type;
      exp_desc->type_idx    = exp_desc_l.type_idx;
   }

   if (exp_desc->foldable) {
      comp_idx = IL_IDX(list_idx);
   }

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   while (list_idx != NULL_IDX) {
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc_l.rank = 0;
      ok &= expr_sem(&opnd, &exp_desc_l);
      COPY_OPND(IL_OPND(list_idx), opnd);

      exp_desc->has_symbolic = exp_desc->has_symbolic || 
                               exp_desc_l.has_symbolic;
      exp_desc->constant = exp_desc->constant && exp_desc_l.constant;
      exp_desc->foldable = exp_desc->foldable && exp_desc_l.foldable;

      exp_desc->will_fold_later = exp_desc->will_fold_later &&
                        (exp_desc_l.will_fold_later ||
                         exp_desc_l.foldable);

      if (exp_desc->foldable) {
         if (fold_relationals(IL_IDX(list_idx),
                              comp_idx,
                              (IR_OPR(ir_idx) == Max_Opr ?
                                    Gt_Opr : Lt_Opr))) {

            comp_idx = IL_IDX(list_idx);
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (exp_desc->foldable) {
      OPND_FLD((*result_opnd))      = CN_Tbl_Idx;
      OPND_IDX((*result_opnd))      = comp_idx;
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
      exp_desc->type_idx    = CN_TYPE_IDX(comp_idx);
      exp_desc->type        = TYP_TYPE(exp_desc->type_idx);
      exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
   }


   TRACE (Func_Exit, "max_opr_handler", NULL);

   return(ok);

}  /* max_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Struct_Opr.                                  *|
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

static boolean struct_opr_handler(opnd_type		*result_opnd,
				  expr_arg_type		*exp_desc,
                                  int			 rank_in)

{
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   boolean              final_component = FALSE;
   int			ir_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   boolean		save_in_call_list;
   boolean		save_insert_subs_ok;

# ifdef _TARGET_OS_MAX
   int			col;
   int			line;
# endif

   TRACE (Func_Entry, "struct_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
# ifdef _TARGET_OS_MAX
   col    = IR_COL_NUM(ir_idx);
   col    = IR_LINE_NUM(ir_idx);
# endif
   save_in_call_list = in_call_list;
   in_call_list = FALSE;
   
   if (! in_component_ref) {
      final_component = TRUE;
      in_component_ref      = TRUE;
   }

   save_insert_subs_ok = insert_subs_ok;

   insert_subs_ok = TRUE;

   exp_desc_l.rank = rank_in;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);
#ifdef KEY /* Bug 572 */
   /* An expression like "parameter_x%ptr_component_y%other_component" is
    * an error because a constant pointer is always null, so we can't
    * dereference it. The constant-folding code assumes no intermediate
    * component is a pointer, so we must catch the error here. */
   if (exp_desc_l.constant && exp_desc_l.pointer) {
      int line_tmp, col_tmp;
      find_opnd_line_and_column(&IR_OPND_L(ir_idx), &line_tmp, &col_tmp);
      PRINTMSG(line_tmp, 1677, Error, col_tmp);
      ok = FALSE;
   }
#endif /* KEY Bug 572 */

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       (IR_OPR(OPND_IDX(opnd)) == Substring_Opr ||
        IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr)) {

      /* this only happens for variable size function results  */
      /* where the struct base is a dummy arg, the actual is   */
      /* a char sequence dt and it has been transformed into a */
      /* substring. Remove the substring.                      */

      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(OPND_IDX(opnd)));
   }

   in_call_list = save_in_call_list;

   exp_desc_r.rank = exp_desc_l.rank;

   insert_subs_ok  = FALSE;
//Bug 370
#ifdef KEY
  if ( IR_OPR(ir_idx) == Struct_Opr &&
       IR_FLD_L(ir_idx) == AT_Tbl_Idx &&
       AT_OBJ_CLASS(IR_OPND_L(ir_idx).idx) == Data_Obj ){
    int attr_idx = TYP_IDX(ATD_TYPE_IDX(IR_OPND_L(ir_idx).idx));
    if (AT_OBJ_CLASS(attr_idx) == Derived_Type) {
      int first_idx = ATT_FIRST_CPNT_IDX(attr_idx);
      if (first_idx != NULL_IDX) {
        for (int i = first_idx;  i != NULL_IDX;  i = SN_SIBLING_LINK(i)) {
          if (AT_OBJ_CLASS(SN_ATTR_IDX(i)) == Data_Obj &&
              ATD_CLASS(SN_ATTR_IDX(i)) == Struct_Component &&
              SN_ATTR_IDX(i) != IR_OPND_R(ir_idx).idx &&
              strcmp(AT_OBJ_NAME_PTR(IR_OPND_R(ir_idx).idx), SN_NAME_PTR(i))==0)
              IR_OPND_R(ir_idx).idx = SN_ATTR_IDX(i);
        }
      }
    }
  }
#endif

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   insert_subs_ok = save_insert_subs_ok;

   exp_desc->has_constructor = exp_desc_l.has_constructor ||
                               exp_desc_r.has_constructor;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic || exp_desc_r.has_symbolic;

   if (final_component) {
      in_component_ref      = FALSE;

      if ((cif_flags & XREF_RECS) != 0 &&
          xref_state != CIF_No_Usage_Rec) {

         if (in_call_list) {
            /* output CIF_Symbol_Is_Actual_Arg */
            cif_usage_rec(ir_idx, IR_Tbl_Idx, IR_LINE_NUM(ir_idx),
                          IR_COL_NUM(ir_idx),
                          CIF_Symbol_Is_Actual_Arg);
         }
         else {
            /* output according xref_state */
            cif_usage_rec(ir_idx, IR_Tbl_Idx, IR_LINE_NUM(ir_idx),
                          IR_COL_NUM(ir_idx),
                          xref_state);
          }
      }
   }

   if (insert_subs_ok) {

      if (exp_desc_l.rank > exp_desc_r.rank) {
         exp_desc->rank = exp_desc_l.rank;
         COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                    exp_desc_l.rank);
      }
      else {
         exp_desc->rank = exp_desc_r.rank;
         COPY_SHAPE(exp_desc->shape,exp_desc_r.shape,
                    exp_desc_r.rank);
      }
   }
   else {
      exp_desc->rank = exp_desc_l.rank;
      COPY_SHAPE(exp_desc->shape,exp_desc_l.shape, exp_desc_l.rank);
   }


   exp_desc->type        = exp_desc_r.type;
   exp_desc->linear_type = exp_desc_r.linear_type;
   exp_desc->type_idx    = exp_desc_r.type_idx;
   COPY_OPND(exp_desc->char_len, exp_desc_r.char_len);
   exp_desc->constant    = exp_desc_l.constant;
   exp_desc->foldable    = exp_desc_l.foldable;
   exp_desc->will_fold_later = exp_desc_l.will_fold_later;


   /* pointer on right means pointer ... */
   exp_desc->pointer  = exp_desc_r.pointer;

   /* pointer or target on left means target */
   exp_desc->target   = exp_desc_l.target ||
                        exp_desc_r.target ||
                        exp_desc_l.pointer;

   exp_desc->vector_subscript = exp_desc_l.vector_subscript;
   exp_desc->reference        = exp_desc_l.reference;
   exp_desc->pe_dim_ref       = exp_desc_l.pe_dim_ref;
   COPY_OPND((exp_desc->bias_opnd), (exp_desc_l.bias_opnd));
   exp_desc->component        = TRUE;

   /* if left has any rank at all it must be treated as */
   /* a section.                                        */
   exp_desc->section          = (exp_desc_l.rank > 0);
   exp_desc->array_elt        = exp_desc_l.array_elt;
   exp_desc->assumed_shape    = exp_desc_l.assumed_shape;
   exp_desc->assumed_size     = exp_desc_l.assumed_size;
#ifdef KEY /* Bug 6845 */
   exp_desc->allocatable      = exp_desc_r.allocatable;
#endif /* KEY Bug 6845 */
   exp_desc->contig_array     = exp_desc_r.contig_array;
   exp_desc->dist_reshape_ref = exp_desc_l.dist_reshape_ref |
                                exp_desc_r.dist_reshape_ref;

   exp_desc->dope_vector      = exp_desc_r.dope_vector;

   if (exp_desc_r.dope_vector &&
       ! no_sub_or_deref) {
      COPY_OPND((*result_opnd), IR_OPND_R(ir_idx));
      COPY_OPND(IR_OPND_R(ir_idx),
                IR_OPND_L(OPND_IDX((*result_opnd))));
      IR_FLD_L(OPND_IDX((*result_opnd))) = IR_Tbl_Idx;
      IR_IDX_L(OPND_IDX((*result_opnd))) = ir_idx;

      IR_TYPE_IDX(OPND_IDX((*result_opnd))) = exp_desc->type_idx;

      /* DO SET IR_RANK HERE TO THE PROPER RANK OF THE STRUCT OPR */

      IR_RANK(OPND_IDX((*result_opnd)))          = exp_desc->rank;
   }

# if defined(_F_MINUS_MINUS)
   if (exp_desc->pe_dim_ref) {

# ifdef _TARGET_OS_MAX
      if (final_component &&
          storage_bit_size_tbl[exp_desc->linear_type] != 64) {

         find_opnd_line_and_column(&IR_OPND_R(ir_idx), &line, &col);
         PRINTMSG(line, 1585, Error, col);
         ok = FALSE;
      }
# endif

      if (exp_desc_r.dope_vector) {

# ifdef _TARGET_OS_MAX
         translate_t3e_dv_component(result_opnd, exp_desc);
# else
         translate_dv_component(result_opnd, exp_desc);
# endif
      }
   }
# endif

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;

   if (insert_subs_ok &&
       ! no_sub_or_deref) {

      if (exp_desc_r.rank > 0 && exp_desc_l.rank > 0) {

         PRINTMSG(IR_LINE_NUM_R(ir_idx), 127, Error,
                  IR_COL_NUM_R(ir_idx));
         ok = FALSE;
      }

      if (ATD_ARRAY_IDX(IR_IDX_R(ir_idx))) {

         ok &= gen_whole_subscript(result_opnd, exp_desc);
      }
      else if (exp_desc->type == Character) {
         ok &= gen_whole_substring(result_opnd, exp_desc->rank);
      }
   }


   TRACE (Func_Exit, "struct_opr_handler", NULL);

   return(ok);

}  /* struct_opr_handler */
#ifdef KEY /* Bug 6845 */
/*
 * Generate code to prepare for setting an allocatable component of a structure
 * constructor. On entry rvalue_opnd is the array expression to be assigned to
 * that component, on exit rvalue_opnd is the dope vector to be assigned in
 * place of the array expression. May create one compiler temp to hold value of
 * array expression; will create another compiler temp to hold dope vector
 * generated to describe that array expression.
 *
 * Assumes that the caller will check that the LHS and RHS are type compatible.
 *
 * This function works whether the assignment is performed as a pointer
 * assignment (which just copies the dope vector into the temp that represents
 * the constructor) or by calling _ASSIGN_ALLOCATABLE. The latter is more
 * expensive, and requires deallocating the temp in the epilog. So far, we see
 * no reason not to copy the dope vector alone--it's a little strange that the
 * temp has a dope vector which isn't marked "allocatable", but nothing bad
 * seems to happen.
 *
 * line		Source line
 * col		Source column
 * exp_desc_l	Description of LHS
 * rvalue_opnd	Source operand to be assigned to allocatable component of
 *		constructor
 * exp_desc_r	Description of rvalue_opnd
 */
static void
help_ctor_array_to_allocatable(int line, int col, expr_arg_type *exp_desc_l,
  opnd_type *rvalue_opnd, expr_arg_type *exp_desc_r) {
  opnd_type tmp_opnd;
  boolean ok = TRUE;

  /* array_construct_opr_handler() doesn't set bounds of exp_desc_r when
   * the Array_Construct_Opr is nested (test for top_constructor bypasses
   * the calculation.) But we need to know it, so we do it ourself here. */
  operator_type ir_opr = IR_OPR(OPND_IDX(*rvalue_opnd));
  if (ir_opr == Array_Construct_Opr || ir_opr == Constant_Array_Construct_Opr) {
    size_level_type constructor_size_level = Simple_Expr_Size;
    opnd_type size_opnd;
    analyse_loops(rvalue_opnd, &size_opnd, &constructor_size_level);
    expr_arg_type loc_exp_desc;
    if (!expr_semantics(&size_opnd, &loc_exp_desc)) {
      PRINTMSG(line, 1044, Internal, col, "help_ctor_array_to_allocatable");
    }
    COPY_OPND((exp_desc_r->shape[0]), size_opnd);
  }

  boolean save_dfe = defer_stmt_expansion;
  defer_stmt_expansion = FALSE;


  /* Need a temp because RHS isn't something that a dope vector can describe? */
  boolean need_tmp_for_rvalue = TRUE;
  switch (get_act_arg_type(exp_desc_r)) {
    case Array_Ptr:
    case Array_Tmp_Ptr:
    case Whole_Allocatable:
    case Whole_Tmp_Allocatable:
    case Whole_Sequence:
    case Whole_Tmp_Sequence:
    case Whole_Ass_Shape:
    case Whole_Array_Constant:
    case Sequence_Array_Section:
    case Constant_Array_Section:
    case Dv_Array_Section:
    case Contig_Section:
    case Dv_Contig_Section:
      need_tmp_for_rvalue = FALSE;
  }

  boolean need_conversion = FALSE;
  if (exp_desc_l->linear_type != exp_desc_r->linear_type) {
    need_conversion = TRUE;
    need_tmp_for_rvalue = TRUE;
  }

  int tmp_idx = NULL_IDX;
  /* Create temp to hold value of array expression */
  if (need_tmp_for_rvalue) {
    tmp_idx = create_tmp_asg(rvalue_opnd, exp_desc_r, &tmp_opnd, Intent_In,
      TRUE, FALSE);
    /* Caller will check that LHS type and RHS type are compatible */
    if (need_conversion) {
      ATD_TYPE_IDX(tmp_idx) = exp_desc_l->type_idx;
    }
  }

  /* Create temp to hold dope vector */
  int tmp_dv_idx = gen_compiler_tmp(line, col, Priv, TRUE);
  ATD_NOT_PT_UNIQUE_MEM(tmp_dv_idx) = TRUE;
  if (need_tmp_for_rvalue) {
    ATD_NOT_PT_UNIQUE_MEM(tmp_idx) = TRUE;
  }
  else {
    ATD_NOT_PT_UNIQUE_MEM((find_left_attr(rvalue_opnd))) = TRUE;
  }

  ATD_TYPE_IDX(tmp_dv_idx) = exp_desc_r->type_idx;
  ATD_STOR_BLK_IDX(tmp_dv_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
  AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;

  if (exp_desc_r->rank) {
    /* Positions 1-7 are deferred shape entries in the bd table. */
    ATD_ARRAY_IDX(tmp_dv_idx) = exp_desc_r->rank;
  }
  ATD_IM_A_DOPE(tmp_dv_idx)    = TRUE;

  if (need_tmp_for_rvalue) {
    opnd_type r_opnd;
    OPND_FLD(r_opnd) = AT_Tbl_Idx;
    OPND_IDX(r_opnd) = tmp_idx;
    OPND_LINE_NUM(r_opnd) = line;
    OPND_COL_NUM(r_opnd)  = col;
   
    if (TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) == Character) {
       ok = gen_whole_substring(&r_opnd, exp_desc_r->rank) && ok;
    }

    OPND_FLD(tmp_opnd) = AT_Tbl_Idx;
    OPND_IDX(tmp_opnd) = tmp_dv_idx;
    OPND_LINE_NUM(tmp_opnd) = line;
    OPND_COL_NUM(tmp_opnd)  = col;

    gen_dv_whole_def(&tmp_opnd, &r_opnd, exp_desc_r);
  }
  else {
    OPND_FLD(tmp_opnd) = AT_Tbl_Idx;
    OPND_IDX(tmp_opnd) = tmp_dv_idx;
    OPND_LINE_NUM(tmp_opnd) = line;
    OPND_COL_NUM(tmp_opnd)  = col;
    gen_dv_whole_def(&tmp_opnd, rvalue_opnd, exp_desc_r);
  }

  COPY_OPND(*rvalue_opnd, tmp_opnd);

  defer_stmt_expansion = save_dfe;
}
#endif /* KEY Bug 6845 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Struct_Construct_Opr and                     *|
|*      Constant_Struct_Construct_Opr.                                        *|
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

static boolean struct_construct_opr_handler(opnd_type		*result_opnd,
				            expr_arg_type	*exp_desc)

{
   int			col;
   int			comp_idx;
   boolean              depends_on_outer_impdo;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   int			i;
   int			ir_idx;
   int			line;
   int			list_idx;
   char                 l_err_word[40];
   boolean		ok = TRUE;
   opnd_type		opnd;
   opnd_type		dv_opnd;
   int			opnd_col;
   int			opnd_line;
   char                 r_err_word[40];
   int			save_constructor_level;
   boolean              save_defer_stmt_expansion;
   boolean              defer_stmt_expansion_save;
   expr_mode_type       save_expr_mode;
   boolean		save_in_call_list;
   boolean		save_io_item_must_flatten;
   int			sn_idx;
   boolean              top_constructor          = FALSE;
   int			type_idx;
   int			tmp_dv_idx;


   TRACE (Func_Entry, "struct_construct_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_io_item_must_flatten = io_item_must_flatten;
   save_in_call_list = in_call_list;  /* BRIANJ set but not used */
   in_call_list = FALSE;
   
   save_expr_mode           = expr_mode;
   expr_mode                = Regular_Expr;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));

   pgm_unit_illegal         = FALSE;
   exp_desc_l.rank = 0;
   ok       = expr_sem(&opnd, &exp_desc_l);
   pgm_unit_illegal         = TRUE;
   expr_mode                = save_expr_mode;

   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   if (AT_OBJ_CLASS(OPND_IDX(opnd)) == Derived_Type) {


      /* expr_sem for the derived_type found a problem or */
      /* AT_DCL_ERR is set for this derived_type.  Either way   */
      /* it is not a valid attribute for a derived_type, so it  */
      /* cannot be used to get a structure constructor.         */

      if (!ok) {
         goto EXIT;
      }

      if (AT_USE_ASSOCIATED(OPND_IDX(opnd)) &&
          ATT_PRIVATE_CPNT(OPND_IDX(opnd))) {
         find_opnd_line_and_column(&opnd,
                                   &opnd_line,
                                   &opnd_col);
         PRINTMSG(opnd_line, 883, Error, opnd_col,
                  AT_OBJ_NAME_PTR(OPND_IDX(opnd)));

         ok = FALSE;
         goto EXIT;
      }

      /* still have structure constructor */

      save_defer_stmt_expansion     = defer_stmt_expansion;
      save_constructor_level        = constructor_level;

      constructor_level++;

      if (! in_constructor) {
         in_constructor             = TRUE;
         top_constructor            = TRUE;
         defer_stmt_expansion       = TRUE;
      }

      exp_desc->rank        = 0;
      exp_desc->type        = Structure;
      exp_desc->linear_type = Structure_Type;

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Structure;
      TYP_LINEAR(TYP_WORK_IDX)	= Structure_Type;
      TYP_IDX(TYP_WORK_IDX)	= OPND_IDX(opnd);
      exp_desc->type_idx	= ntr_type_tbl();

      IR_TYPE_IDX(ir_idx)   = exp_desc->type_idx;
      IR_RANK(ir_idx)       = exp_desc->rank;

      if (ATT_NUM_CPNTS(TYP_IDX(exp_desc->type_idx)) !=
                        IR_LIST_CNT_R(ir_idx)) {

         /* error .. not the right number of components */

         ok = FALSE;
         PRINTMSG(line, 357, Error, col);
         goto EXIT;
      }

      exp_desc->foldable            = TRUE;
      exp_desc->will_fold_later     = TRUE;

      list_idx = IR_IDX_R(ir_idx);
      sn_idx   = ATT_FIRST_CPNT_IDX(TYP_IDX(exp_desc->type_idx));

      for (i = 0; i < IR_LIST_CNT_R(ir_idx); i++) {
         exp_desc_r.rank = 0;

#ifdef KEY /* Bug 6845 */
	 opnd_type save_list_opnd = IL_OPND(list_idx);
#endif /* KEY Bug 6845 */
         COPY_OPND(opnd, IL_OPND(list_idx));
#ifdef KEY /* Bug 6845 */
         comp_idx               = SN_ATTR_IDX(sn_idx);
	 /* For allocatable LHS, we need folding */
	 if (ATD_ALLOCATABLE(comp_idx)) {
	   ok &= expr_semantics(&opnd, &exp_desc_r);
	 }
	 else {
	   ok &= expr_sem(&opnd, &exp_desc_r);
	 }
#else /* KEY Bug 6845 */
         ok &= expr_sem(&opnd, &exp_desc_r);
#endif /* KEY Bug 6845 */
         COPY_OPND(IL_OPND(list_idx), opnd);

         IL_ARG_DESC_VARIANT(list_idx) = TRUE;

         /* save exp_desc */
         arg_info_list_base      = arg_info_list_top;
         arg_info_list_top       = arg_info_list_base + 1;

         if (arg_info_list_top >= arg_info_list_size) {
            enlarge_info_list_table();
         }

         IL_ARG_DESC_IDX(list_idx)           = arg_info_list_top;
         arg_info_list[arg_info_list_top]    = init_arg_info;
         arg_info_list[arg_info_list_top].ed = exp_desc_r;

         exp_desc->has_symbolic |= exp_desc_r.has_symbolic;

         comp_idx               = SN_ATTR_IDX(sn_idx);
         exp_desc_l.type_idx    = ATD_TYPE_IDX(comp_idx);
         exp_desc_l.linear_type = TYP_LINEAR(exp_desc_l.type_idx);
         exp_desc_l.type        = TYP_TYPE(exp_desc_l.type_idx);

         if (ASG_TYPE(exp_desc_l.linear_type,
                      exp_desc_r.linear_type) == Err_Res) {

            /* error .. can't make asg */

            if ((exp_desc_r.type == Typeless) && ATD_POINTER(comp_idx)) {
               /* We have the NULL() intrinsic */
            }
            else {
               ok = FALSE;
               PRINTMSG(IR_LINE_NUM(ir_idx), 358, Error,
                     IR_COL_NUM(ir_idx), i + 1);
            }
         }

         if (ASG_EXTN(exp_desc_l.linear_type,
                      exp_desc_r.linear_type)  &&
             (exp_desc_r.type == Character ||
              exp_desc_r.linear_type == Short_Typeless_Const))     {
            find_opnd_line_and_column(&opnd,
                                      &opnd_line,
                                      &opnd_col);

            if (exp_desc_r.type == Character) {
               PRINTMSG(opnd_line, 161, Ansi, opnd_col);
            }


            IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                      exp_desc_l.linear_type,
                                                      opnd_line,
                                                      opnd_col);

            exp_desc_r.type_idx    = exp_desc_l.linear_type;
            exp_desc_r.type        = TYP_TYPE(exp_desc_l.linear_type);
            exp_desc_r.linear_type = exp_desc_l.linear_type;
         }

         if ((ATD_ARRAY_IDX(comp_idx) == 0 &&
              exp_desc_r.rank != 0)                    ||
             (ATD_ARRAY_IDX(comp_idx)          != 0       &&
              exp_desc_r.rank                  != 0       &&
             BD_RANK(ATD_ARRAY_IDX(comp_idx)) != exp_desc_r.rank)) {
            /* error .. rank doesn't match */
            ok = FALSE;
            find_opnd_line_and_column(&opnd,
                                      &opnd_line,
                                      &opnd_col);
            PRINTMSG(opnd_line, 360, Error, opnd_col, i + 1);
         }

         if (ATD_POINTER(comp_idx) && ok) {

            if (OPND_FLD(opnd) == AT_Tbl_Idx) {

               if (AT_OBJ_CLASS(OPND_IDX(opnd)) != Data_Obj ||
                   (!ATD_TARGET(OPND_IDX(opnd))  &&
                    !ATD_POINTER(OPND_IDX(opnd))))          {

                  ok = FALSE;
                  find_opnd_line_and_column(&opnd,
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 359, Error, opnd_col);
               }
            }
            else if (OPND_FLD(opnd) == IR_Tbl_Idx) {

               if (IR_OPR(OPND_IDX(opnd)) == Null_Intrinsic_Opr) {
                  tmp_dv_idx = gen_compiler_tmp(line, 
						col, 
						Priv, 
						TRUE);

      		  ATD_TYPE_IDX(tmp_dv_idx) = ATD_TYPE_IDX(comp_idx);
                  ATD_STOR_BLK_IDX(tmp_dv_idx) = 
                           SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      	          AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
      		  ATD_ARRAY_IDX(tmp_dv_idx) = ATD_ARRAY_IDX(comp_idx);
      	  	  ATD_POINTER(tmp_dv_idx) = TRUE;
      		  ATD_IM_A_DOPE(tmp_dv_idx) = TRUE;

                  gen_opnd(&dv_opnd, 
			   tmp_dv_idx, 
			   AT_Tbl_Idx, 
			   line, 
			   col);

                  defer_stmt_expansion_save = defer_stmt_expansion;
                  defer_stmt_expansion = FALSE;
      		  gen_static_dv_whole_def(&dv_opnd,
                 		          tmp_dv_idx,
                              		  Before);
 
                  defer_stmt_expansion = defer_stmt_expansion_save;

      		  exp_desc_r.type_idx = ATD_TYPE_IDX(comp_idx);
      		  exp_desc_r.type = TYP_TYPE(ATD_TYPE_IDX(comp_idx));
      		  exp_desc_r.linear_type = TYP_LINEAR(ATD_TYPE_IDX(comp_idx));
      	 	  exp_desc_r.pointer = TRUE;
      		  exp_desc_r.tmp_reference = TRUE;
                  exp_desc_r.foldable = TRUE;
                  exp_desc_r.will_fold_later = TRUE;

            	  if (ATD_ARRAY_IDX(comp_idx) == NULL_IDX) {
               	     exp_desc_r.rank = 0;
            	  }
            	  else {
               	     exp_desc_r.rank = BD_RANK(ATD_ARRAY_IDX(comp_idx));
            	  }


      		  gen_opnd(&dv_opnd,
               		   gen_ir(AT_Tbl_Idx,
                                  tmp_dv_idx,
                                  Dv_Deref_Opr,
                                  exp_desc_r.type_idx,
                                  line,
                                  col,
                                  NO_Tbl_Idx,
                                  NULL_IDX),
                           IR_Tbl_Idx,
               		   line,
               		   col);

      		  if (exp_desc_r.rank > 0) {
         	     ok = gen_whole_subscript(&dv_opnd, &exp_desc_r);
     		  }

                  COPY_OPND(opnd, dv_opnd);
                  COPY_OPND(IL_OPND(list_idx), opnd);
               }
               else if (IR_OPR(OPND_IDX(opnd)) == Call_Opr) {

                  if (!ATD_POINTER(ATP_RSLT_IDX(IR_IDX_L(
                                            OPND_IDX(opnd))))) {
                     ok = FALSE;
                     find_opnd_line_and_column(&opnd,
                                               &opnd_line,
                                               &opnd_col);
                     PRINTMSG(opnd_line, 359, Error, opnd_col);
                  }
               }
               else if (exp_desc_r.reference      ||
                        exp_desc_r.tmp_reference) {

                  if (! exp_desc_r.pointer && ! exp_desc_r.target) {
                     ok = FALSE;
                     find_opnd_line_and_column(&opnd,
                                               &opnd_line,
                                               &opnd_col);
                     PRINTMSG(opnd_line, 359, Error, opnd_col);
                  }
                  else {
                     if (exp_desc_r.rank != 0) {
                        /* check for IL_VECTOR_SUBSCRIPT */
                        if (exp_desc_r.vector_subscript) {

                           find_opnd_line_and_column(&opnd,
                                                     &opnd_line,
                                                     &opnd_col);
                           PRINTMSG(opnd_line, 420, Error,
                                    opnd_col);
                           ok = FALSE;
                        }
                     }
                  }
               }
               else { /* an expression other than a call .. error */
                  ok = FALSE;
                  find_opnd_line_and_column(&opnd,
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 359, Error, opnd_col);
               }
            }
            else {
               /* error ..  assuming only constants here */
               ok = FALSE;
               find_opnd_line_and_column(&opnd,
                                         &opnd_line,
                                         &opnd_col);
               PRINTMSG(opnd_line, 359, Error, opnd_col);
            }

            if (ok &&
                (ATD_ARRAY_IDX(comp_idx) ?
                   BD_RANK(ATD_ARRAY_IDX(comp_idx)) : 0) !=
                                             exp_desc_r.rank) {

               ok = FALSE;
               find_opnd_line_and_column(&opnd,
                                         &opnd_line,
                                         &opnd_col);
               PRINTMSG(opnd_line, 431, Error, opnd_col);
            }

            type_idx = ATD_TYPE_IDX(comp_idx);

            if (ok &&
                (TYP_TYPE(type_idx) != exp_desc_r.type ||
                 (TYP_TYPE(type_idx) == Structure &&
                  !compare_derived_types(type_idx,
                                         exp_desc_r.type_idx)))) {

               r_err_word[0] = '\0';
               l_err_word[0] = '\0';

               strcat(l_err_word, get_basic_type_str(type_idx));

               strcat(r_err_word,
                      get_basic_type_str(exp_desc_r.type_idx));

               find_opnd_line_and_column(&opnd,
                                         &opnd_line,
                                         &opnd_col);

               PRINTMSG(opnd_line, 432, Error, opnd_col,
                        r_err_word,
                        l_err_word);
               ok = FALSE;

            }

            if (ok            &&
                TYP_TYPE(type_idx) != Character &&
                TYP_TYPE(type_idx) != Structure &&
                TYP_LINEAR(type_idx) != exp_desc_r.linear_type) {

               find_opnd_line_and_column(&opnd,
                                         &opnd_line,
                                         &opnd_col);

               PRINTMSG(opnd_line, 419, Error, opnd_col);
               ok = FALSE;
            }

            if (ok                             &&
                TYP_TYPE(type_idx) == Character                  &&
                TYP_FLD(type_idx) == CN_Tbl_Idx &&
                exp_desc_r.char_len.fld == CN_Tbl_Idx            &&
                fold_relationals(TYP_IDX(type_idx), 
                                 exp_desc_r.char_len.idx, Ne_Opr)) {

               ok = FALSE;
               find_opnd_line_and_column(&opnd,
                                         &opnd_line,
                                         &opnd_col);
               PRINTMSG(opnd_line, 853, Error, opnd_col);
            }
         }
#ifdef KEY /* Bug 6845 */
         else if (ATD_ALLOCATABLE(comp_idx) && ok) {
            if (OPND_FLD(opnd) == AT_Tbl_Idx) {
               if (AT_OBJ_CLASS(OPND_IDX(opnd)) != Data_Obj) {
                  ok = FALSE;
                  find_opnd_line_and_column(&opnd, &opnd_line, &opnd_col);
                  PRINTMSG(opnd_line, 358, Error, opnd_col, i + 1);
               }
            }
            else if (OPND_FLD(opnd) == IR_Tbl_Idx) {

	       operator_type ir_opr = IR_OPR(OPND_IDX(opnd));

	       /* null() allowed: code cribbed from pointer case */
               if (ir_opr == Null_Intrinsic_Opr) {
                  tmp_dv_idx = gen_compiler_tmp(line, col, Priv, TRUE);

      		  ATD_TYPE_IDX(tmp_dv_idx) = ATD_TYPE_IDX(comp_idx);
                  ATD_STOR_BLK_IDX(tmp_dv_idx) = 
                           SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      	          AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
      		  ATD_ARRAY_IDX(tmp_dv_idx) = ATD_ARRAY_IDX(comp_idx);
      	  	  ATD_ALLOCATABLE(tmp_dv_idx) = TRUE;
      		  ATD_IM_A_DOPE(tmp_dv_idx) = TRUE;

                  gen_opnd(&dv_opnd, tmp_dv_idx, AT_Tbl_Idx, line, col);

                  defer_stmt_expansion_save = defer_stmt_expansion;
                  defer_stmt_expansion = FALSE;
      		  gen_static_dv_whole_def(&dv_opnd, tmp_dv_idx, Before);
 
                  defer_stmt_expansion = defer_stmt_expansion_save;

      		  exp_desc_r.type_idx = ATD_TYPE_IDX(comp_idx);
      		  exp_desc_r.type = TYP_TYPE(ATD_TYPE_IDX(comp_idx));
      		  exp_desc_r.linear_type = TYP_LINEAR(ATD_TYPE_IDX(comp_idx));
      	 	  exp_desc_r.pointer = FALSE;
      	 	  exp_desc_r.allocatable = TRUE;
      		  exp_desc_r.tmp_reference = TRUE;
                  exp_desc_r.foldable = TRUE;
                  exp_desc_r.will_fold_later = TRUE;

            	  if (ATD_ARRAY_IDX(comp_idx) == NULL_IDX) {
               	     exp_desc_r.rank = 0;
            	  }
            	  else {
               	     exp_desc_r.rank = BD_RANK(ATD_ARRAY_IDX(comp_idx));
            	  }


      		  gen_opnd(&dv_opnd,
               		   gen_ir(AT_Tbl_Idx,
                                  tmp_dv_idx,
                                  Dv_Deref_Opr,
                                  exp_desc_r.type_idx,
                                  line,
                                  col,
                                  NO_Tbl_Idx,
                                  NULL_IDX),
                           IR_Tbl_Idx,
               		   line,
               		   col);

      		  if (exp_desc_r.rank > 0) {
         	     ok = gen_whole_subscript(&dv_opnd, &exp_desc_r);
     		  }

                  COPY_OPND(opnd, dv_opnd);
                  COPY_OPND(IL_OPND(list_idx), opnd);
               }
	       /* If RHS is an allocatable or a dope vector, we can just use
	        * its dope vector because the constructor is supposed to be
		* readonly, so it can't be deallocated or subject to any
		* operations that work only on an allocatable.
		*
		* For some reason, a section of a dope operand has
		* exp_desc_r.dope_vector set even though there isn't a dope
		* vector describing the section. This test is probably
		* compensating for a longstanding bug in expr_semantics(), but
		* who knows? */
	       else if ((exp_desc_r.allocatable || exp_desc_r.dope_vector) &&
		  !exp_desc_r.section) {
		  COPY_OPND(IL_OPND(list_idx), save_list_opnd);
	       }
	       /* Arbitrary array expression: copy to a compiler temp, then
	        * generate a dope vector describing the compiler temp. */
	       else {
	         help_ctor_array_to_allocatable(line, col, &exp_desc_l, &opnd,
		   &exp_desc_r);
		 /* We just added a compiler temp containing a dope vector */
		 exp_desc->foldable = exp_desc->will_fold_later = FALSE;
		 COPY_OPND(IL_OPND(list_idx), opnd);
	       }
            }
            else {
               /* error ..  assuming only constants here */
               ok = FALSE;
               find_opnd_line_and_column(&opnd, &opnd_line, &opnd_col);
               PRINTMSG(opnd_line, 358, Error, opnd_col, i + 1);
            }

	    int rank_l = ATD_ARRAY_IDX(comp_idx) ?
	      BD_RANK(ATD_ARRAY_IDX(comp_idx)) : 0;
            if (ok && rank_l != exp_desc_r.rank) {
               ok = FALSE;
               find_opnd_line_and_column(&opnd, &opnd_line, &opnd_col);
               PRINTMSG(opnd_line, 324, Error, opnd_col, exp_desc_r.rank,
	         rank_l);
            }

            type_idx = ATD_TYPE_IDX(comp_idx);

            if (ok && Err_Res ==
	       ASG_TYPE(TYP_LINEAR(type_idx), exp_desc_r.linear_type)) {
               find_opnd_line_and_column(&opnd, &opnd_line, &opnd_col);
               PRINTMSG(opnd_line, 358, Error, opnd_col, i + 1);
               ok = FALSE;
            }
	 }
#endif /* KEY Bug 6845 */

         exp_desc->foldable = exp_desc->foldable && exp_desc_r.foldable;

         exp_desc->will_fold_later &= (exp_desc_r.will_fold_later ||
                                       exp_desc_r.foldable);

         sn_idx     = SN_SIBLING_LINK(sn_idx);
         list_idx   = IL_NEXT_LIST_IDX(list_idx);
      }

      defer_stmt_expansion = save_defer_stmt_expansion;

      depends_on_outer_impdo = FALSE;

      if (constructor_level > save_constructor_level) {
         constructor_level = save_constructor_level;

         if (exp_desc->foldable ||
             exp_desc->will_fold_later) {

            IR_OPR(ir_idx) = Constant_Struct_Construct_Opr;
         }
      }
      else if (top_constructor) {
         depends_on_outer_impdo = TRUE;
         exp_desc->will_fold_later |= exp_desc->foldable;
         exp_desc->foldable = FALSE;
      }

      if (! top_constructor) {
         exp_desc->has_constructor = TRUE;
      }

      if (top_constructor          &&
          ! no_func_expansion      &&
          ok)    {

         if (exp_desc->foldable ||
             exp_desc->will_fold_later) {

            if (depends_on_outer_impdo) {
               /* intentionally blank */
            }
            else if (expr_mode == Initialization_Expr) {
               exp_desc->foldable = TRUE;
            }
            else if (! create_constructor_constant(result_opnd, exp_desc)) {
               ok = FALSE;
            }
         }
         else {

            ok = create_runtime_struct_constructor(result_opnd);

            exp_desc->tmp_reference = TRUE;
         }
      }

      if (top_constructor) {
         in_constructor = FALSE;
      }

      io_item_must_flatten = save_io_item_must_flatten;
   }
   else if (AT_OBJ_CLASS(OPND_IDX(opnd)) == Pgm_Unit) {

      /* change to function call */
      IR_OPR(ir_idx) = Call_Opr;
      ok = expr_sem(result_opnd, exp_desc);
   }
   else {
      /* error ..  shouldn't be here */
      PRINTMSG(line, 975, Internal, col);
   }

EXIT:

   TRACE (Func_Exit, "struct_construct_opr_handler", NULL);

   return(ok);

}  /* struct_construct_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Array_Construct_Opr and                      *|
|*      Constant_Array_Construct_Opr.                                         *|
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

static boolean array_construct_opr_handler(opnd_type		*result_opnd,
				           expr_arg_type	*exp_desc)

{
   size_level_type	constructor_size_level;
   boolean              depends_on_outer_impdo;
   int			depth;
   int			ir_idx;
   expr_arg_type	loc_exp_desc;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int                  save_constructor_level;
   boolean              save_defer_stmt_expansion;
   boolean              save_in_call_list;
   boolean		save_io_item_must_flatten;
   opnd_type		size_opnd;
   boolean              top_constructor          = FALSE;


   TRACE (Func_Entry, "array_construct_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   save_io_item_must_flatten = io_item_must_flatten;
   save_in_call_list = in_call_list;  /* BRIANJ - Set but not used. */
   in_call_list = FALSE;
   
   save_defer_stmt_expansion = defer_stmt_expansion;
   save_constructor_level = constructor_level;
   constructor_level++;

   if (! in_constructor) {
      top_constructor = TRUE;
      in_constructor  = TRUE;
      defer_stmt_expansion = TRUE;
   }

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   ok = array_construct_semantics(&opnd, exp_desc);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   exp_desc->rank           = 1;
   defer_stmt_expansion     = save_defer_stmt_expansion;

   depends_on_outer_impdo = FALSE;

   if (constructor_level > save_constructor_level) {
      constructor_level = save_constructor_level;

      if (exp_desc->foldable ||
          exp_desc->will_fold_later) {

         IR_OPR(ir_idx) = Constant_Array_Construct_Opr;
      }
   }
   else if (top_constructor) {
      depends_on_outer_impdo = TRUE;
      exp_desc->will_fold_later |= exp_desc->foldable;
      exp_desc->foldable = FALSE;
   }

   if (top_constructor &&
       ok) {

      COPY_OPND(opnd, (*result_opnd));
      constructor_size_level = Simple_Expr_Size;
      analyse_loops(&opnd, &size_opnd, &constructor_size_level);

      if (constructor_size_level == Simple_Expr_Size) {
         ok &= expr_semantics(&size_opnd, &loc_exp_desc);
      }


      COPY_OPND((exp_desc->shape[0]), size_opnd);
      exp_desc->constructor_size_level = constructor_size_level;

      if (exp_desc->foldable ||
          exp_desc->will_fold_later) {

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
 
               if (constructor_size_level == Simple_Expr_Size) {

                  /* if bigger than 5,000 elements, make it runtime */

                  if (OPND_FLD(size_opnd) == CN_Tbl_Idx &&
                      compare_cn_and_value(OPND_IDX(size_opnd),
                                           5000,
                                           Gt_Opr)) {
                  
                     exp_desc->will_fold_later = FALSE;
                     exp_desc->foldable = FALSE;
                     IR_OPR(ir_idx) = Array_Construct_Opr;
                  }
               }
               else if (constructor_size_level == Interp_Loop_Size) {

                  depth = implied_do_depth(&size_opnd);

                  /* if more than 2 nested implied do's, make it runtime */

                  if (depth > 2) {
                     exp_desc->will_fold_later = FALSE;
                     exp_desc->foldable = FALSE;
                     IR_OPR(ir_idx) = Array_Construct_Opr;
                  }
                  else if (outer_imp_do_count(&size_opnd) > 50) {
                     exp_desc->will_fold_later = FALSE;
                     exp_desc->foldable = FALSE;
                     IR_OPR(ir_idx) = Array_Construct_Opr;
                  }
               }
               break;
         }
      }
   }

   if (top_constructor          &&
       ! no_func_expansion      &&
       ok)    {

      if (exp_desc->foldable ||
          exp_desc->will_fold_later) {

         if (depends_on_outer_impdo) {
            /* intentionally blank */
         }
         else if (expr_mode == Initialization_Expr) {
            exp_desc->foldable = TRUE;
         }
         else if (! create_constructor_constant(result_opnd, exp_desc)) {
            ok = FALSE;
         }
      }
      else {
         ok = create_runtime_array_constructor(result_opnd, exp_desc);
      }
   }

   if (! top_constructor) {

      exp_desc->has_constructor = TRUE;

      /* save exp_desc */
      arg_info_list_base      = arg_info_list_top;
      arg_info_list_top       = arg_info_list_base + 1;

      if (arg_info_list_top >= arg_info_list_size) {
         enlarge_info_list_table();
      }

      IR_IDX_L(ir_idx) = arg_info_list_top;
      arg_info_list[arg_info_list_top] = init_arg_info;
      arg_info_list[arg_info_list_top].ed = *exp_desc;

   }

   if (top_constructor) {
      in_constructor = FALSE;
   }

   io_item_must_flatten = save_io_item_must_flatten;

   TRACE (Func_Exit, "array_construct_opr_handler", NULL);

   return(ok);

}  /* array_construct_opr_handler */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Whole_Subscript_Opr, Section_Subscript_Opr,  *|
|*      and Subscript_Opr.                                                    *|
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

static boolean subscript_opr_handler(opnd_type		*result_opnd,
				     expr_arg_type	*exp_desc,
				     int		 rank_in)

{

   enum	section_value		{
				Full_Section,
				Part_Section,
				Element,
				Vector_Section
				};

   typedef enum section_value	section_type;

   int			allocatable_pointee_idx = NULL_IDX;
   section_type		contig_state;
   section_type		curr_section;
   boolean		lb_default;
   boolean		ub_default;
   boolean		st_default;
   int			attr_idx;
   int			bd_idx;
   int			col;
   int			dv_idx;
   opnd_type		dv_opnd;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   int			host_attr_idx;
   int			i;
   int			ir_idx;
   int			line;
   int			listp_idx;
   int			list_idx;
   int			list2_idx;
   int			num_dims;
   int			minus_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   opnd_type		opnd2;
   int			opnd_col;
   int			opnd_line;
   int			pe_dim_list_idx = NULL_IDX;
   int			plus_idx;
   expr_mode_type       save_expr_mode;
   boolean		save_insert_subs_ok;
   boolean		save_in_call_list;
   boolean		save_in_component_ref;
   boolean		save_in_implied_do;
   cif_usage_code_type	save_xref_state;

# if defined(_F_MINUS_MINUS) && defined(_TARGET_OS_MAX)
   int			save_pe_dv_list_idx = NULL_IDX;
# endif


   TRACE (Func_Entry, "subscript_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   
   exp_desc_l.rank = rank_in;

   save_in_implied_do = in_implied_do;

   if (IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
      in_implied_do      = FALSE;
   }

# if defined(_F_MINUS_MINUS)
   attr_idx = find_base_attr(&(IR_OPND_L(ir_idx)), &line, &col);
   host_attr_idx = attr_idx;

   while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX &&
          ! AT_IGNORE_ATTR_LINK(host_attr_idx)) {

      host_attr_idx = AT_ATTR_LINK(host_attr_idx);
   }

   if (AT_OBJ_CLASS(host_attr_idx) == Data_Obj &&
       ATD_CLASS(host_attr_idx) == Variable &&
       host_attr_idx != attr_idx &&
       ATD_PE_ARRAY_IDX(host_attr_idx)   &&
       ATD_ALLOCATABLE(host_attr_idx)   &&
       ATD_VARIABLE_TMP_IDX(host_attr_idx) != NULL_IDX) {

      /* pointee must be in local scope, so use copy */

      ATD_CLASS(attr_idx) = Variable;

      if (ATD_VARIABLE_TMP_IDX(attr_idx) == NULL_IDX) {
         /* get new pointee in local scope */

         allocatable_pointee_idx = gen_compiler_tmp(line, col, Shared, TRUE);

         ATD_CLASS(allocatable_pointee_idx) = CRI__Pointee;
         AT_SEMANTICS_DONE(allocatable_pointee_idx) = TRUE;

         ATD_TYPE_IDX(allocatable_pointee_idx) =
                            ATD_TYPE_IDX(ATD_VARIABLE_TMP_IDX(host_attr_idx));
         ATD_STOR_BLK_IDX(allocatable_pointee_idx) =
                            SCP_SB_BASED_IDX(curr_scp_idx);

         ATD_PTR_IDX(allocatable_pointee_idx) =
                        ATD_PTR_IDX(ATD_VARIABLE_TMP_IDX(host_attr_idx));
         ATD_ARRAY_IDX(allocatable_pointee_idx) =
                        ATD_ARRAY_IDX(ATD_VARIABLE_TMP_IDX(host_attr_idx));
         ATD_PE_ARRAY_IDX(allocatable_pointee_idx) =
                        ATD_PE_ARRAY_IDX(ATD_VARIABLE_TMP_IDX(host_attr_idx));

         ATD_FLD(attr_idx) = AT_Tbl_Idx;
         ATD_VARIABLE_TMP_IDX(attr_idx) = allocatable_pointee_idx;

      }
      else {
         allocatable_pointee_idx = ATD_VARIABLE_TMP_IDX(attr_idx);
      }
   }
# endif

   /* do not change in_call_list for array base (left side) */

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   save_insert_subs_ok = insert_subs_ok;
   insert_subs_ok = FALSE;
   pgm_unit_illegal = FALSE;
   ok = expr_sem(&opnd, &exp_desc_l);
   insert_subs_ok = TRUE;
   pgm_unit_illegal = TRUE;
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   in_implied_do = save_in_implied_do;

   exp_desc->has_constructor = exp_desc_l.has_constructor;

# if defined(_F_MINUS_MINUS) && defined(_TARGET_OS_MAX)
   if (exp_desc_l.pe_dim_ref &&
       IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
       IR_OPR(IR_IDX_L(ir_idx)) == Subscript_Opr &&
       IR_LIST_CNT_R(IR_IDX_L(ir_idx)) == 1 &&
       IL_PE_SUBSCRIPT(IR_IDX_R(IR_IDX_L(ir_idx)))) {

      /* save the pe subscript */
      save_pe_dv_list_idx = IR_IDX_R(IR_IDX_L(ir_idx));

      plus_idx = IR_IDX_L(ir_idx);
      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IR_IDX_L(ir_idx)));
      COPY_OPND(opnd, IR_OPND_L(ir_idx));
      FREE_IR_NODE(plus_idx);

   }
# endif

   attr_idx = find_base_attr(&opnd, &line, &col);

   if (attr_idx                            &&
       AT_OBJ_CLASS(attr_idx) == Data_Obj) {

      /* set in_call_list to false for right hand side */

      save_in_call_list = in_call_list;  /* BRIANJ - Set but not used. */
      in_call_list = FALSE;

      bd_idx = ATD_ARRAY_IDX(attr_idx);

      if (bd_idx &&
          (BD_ARRAY_CLASS(bd_idx) == Explicit_Shape ||
           BD_ARRAY_CLASS(bd_idx) == Assumed_Size)) {

         for (i = 1; i <= BD_RANK(bd_idx); i++) {
            if (BD_LB_FLD(bd_idx,i) == AT_Tbl_Idx) {
               ADD_TMP_TO_SHARED_LIST(BD_LB_IDX(bd_idx,i));
            }
         }
      }

# ifdef _F_MINUS_MINUS
      if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
          ATD_PE_ARRAY_IDX(attr_idx) == NULL_IDX)
# else
      if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX)
# endif
                                                {
         PRINTMSG(line, 546, Internal, col);
      }

      exp_desc->type        = exp_desc_l.type;
      exp_desc->linear_type = exp_desc_l.linear_type;
      exp_desc->type_idx    = exp_desc_l.type_idx;
      exp_desc->rank        = 0;
      exp_desc->constant    = exp_desc_l.constant;
      exp_desc->foldable    = exp_desc_l.foldable;
      exp_desc->will_fold_later = exp_desc_l.will_fold_later;
      exp_desc->reference   = exp_desc_l.reference;
      exp_desc->pe_dim_ref  = exp_desc_l.pe_dim_ref;
      COPY_OPND((exp_desc->bias_opnd), (exp_desc_l.bias_opnd));
      exp_desc->cif_id      = exp_desc_l.cif_id;
      exp_desc->component   = exp_desc_l.component;
      exp_desc->dope_vector = exp_desc_l.dope_vector;
      exp_desc->vector_subscript = exp_desc_l.vector_subscript;
      exp_desc->section     = exp_desc_l.section;
      exp_desc->has_symbolic= exp_desc_l.has_symbolic;

      exp_desc->contig_array = exp_desc_l.contig_array;
      exp_desc->dist_reshape_ref = exp_desc_l.dist_reshape_ref;

      COPY_OPND((exp_desc->char_len), (exp_desc_l.char_len));

      if (IR_FLD_L(ir_idx)         == IR_Tbl_Idx    &&
          IR_OPR(IR_IDX_L(ir_idx)) == Dv_Deref_Opr) {

         COPY_OPND(dv_opnd, IR_OPND_L(IR_IDX_L(ir_idx)));
      }
      else {
         COPY_OPND(dv_opnd, IR_OPND_L(ir_idx));
      }

      copy_subtree(&dv_opnd, &dv_opnd);

      IR_TYPE_IDX(ir_idx)   = exp_desc->type_idx;

      list_idx = IR_IDX_R(ir_idx);
      num_dims = 0;

      while (list_idx != NULL_IDX) {
         if (IL_PE_SUBSCRIPT(list_idx)) {
            pe_dim_list_idx = list_idx;
            break;
         }
         num_dims++;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      if (pe_dim_list_idx != NULL_IDX &&
          num_dims == 0 &&
          bd_idx != NULL_IDX) {
         /* have a whole array reference with pe dimensions. */
         /* must generate a whole subscript opr */

         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         ok &= gen_whole_subscript(&opnd, &exp_desc_l);

         if (ok) {
            list_idx = IR_IDX_R(OPND_IDX(opnd));

            while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
               if (IL_PE_SUBSCRIPT(IL_NEXT_LIST_IDX(list_idx))) {
                  FREE_IR_NODE(IL_IDX(IL_NEXT_LIST_IDX(list_idx)));
                  FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(list_idx));
                  IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
                  IR_LIST_CNT_R(OPND_IDX(opnd)) -= 1;
                  break;
               }
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }

            num_dims = IR_LIST_CNT_R(OPND_IDX(opnd));

            IR_LIST_CNT_R(OPND_IDX(opnd)) += IR_LIST_CNT_R(ir_idx);
            IL_NEXT_LIST_IDX(list_idx) = pe_dim_list_idx;
            IL_PREV_LIST_IDX(pe_dim_list_idx) = list_idx;
            COPY_OPND((*result_opnd), opnd);
            ir_idx = OPND_IDX(opnd);
         }
      }

      if (ok                           &&
          ATD_PE_ARRAY_IDX(attr_idx)   &&
          ATD_ALLOCATABLE(attr_idx)    &&
          ATD_VARIABLE_TMP_IDX(attr_idx) != NULL_IDX &&
          pe_dim_list_idx != NULL_IDX) {

         IR_FLD_L(ir_idx) = AT_Tbl_Idx;

         if (allocatable_pointee_idx != NULL_IDX) {
            IR_IDX_L(ir_idx) = allocatable_pointee_idx;
         }
         else {
            IR_IDX_L(ir_idx) = ATD_VARIABLE_TMP_IDX(attr_idx);
         }
         IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);

         attr_idx = IR_IDX_L(ir_idx);
         bd_idx = ATD_ARRAY_IDX(attr_idx);

         exp_desc_l.dope_vector = FALSE;
         exp_desc->dope_vector = exp_desc_l.dope_vector;
      }

      if (IR_OPR(ir_idx) == Whole_Subscript_Opr) {
         exp_desc->pointer = exp_desc_l.pointer;
         exp_desc->target  = exp_desc_l.target;
      }
      else {
         exp_desc->target   = exp_desc_l.target ||
                                exp_desc_l.pointer;
      }

      if (BD_RANK(bd_idx) < num_dims) {
         ok = FALSE;
         PRINTMSG(line, 204, Error, col);
      }
      else if (IR_LIST_CNT_R(ir_idx) == 0) {
         ok = FALSE;
         PRINTMSG(line, 393, Error, col);
      }
      else {

         save_expr_mode = expr_mode;

         if (expr_mode == Data_Stmt_Target) {
            expr_mode = Data_Stmt_Target_Expr;
         }
         else if (expr_mode == Restricted_Imp_Do_Target) {
            expr_mode = Restricted_Imp_Do_Expr;
         }

         /* process subscripts */
         listp_idx = NULL_IDX;
         list_idx  = IR_IDX_R(ir_idx);

         save_xref_state       = xref_state;

         if (xref_state != CIF_No_Usage_Rec) {
            xref_state         = CIF_Symbol_Reference;
         }
         save_in_component_ref = in_component_ref;
         in_component_ref      = FALSE;

         contig_state = Full_Section;

         for (i = 1; i <= num_dims; i++) {

            curr_section = Full_Section;

            exp_desc_r.rank = 0;

            COPY_OPND(opnd, IL_OPND(list_idx));
            ok &= expr_sem(&opnd, &exp_desc_r);
            COPY_OPND(IL_OPND(list_idx), opnd);

            exp_desc->has_symbolic |= exp_desc_r.has_symbolic;
            exp_desc->has_constructor |= exp_desc_r.has_constructor;
            exp_desc->foldable &= exp_desc_r.foldable;

            exp_desc->will_fold_later &= (exp_desc_r.will_fold_later ||
                                          exp_desc_r.foldable);

            IL_CONSTANT_SUBSCRIPT(list_idx) = exp_desc_r.foldable;

            if (exp_desc_r.rank == 0) {
               curr_section = Element;
            }

            if (exp_desc_r.linear_type == Long_Typeless) {
               find_opnd_line_and_column((opnd_type *)
                                          &IL_OPND(list_idx),
                                         &opnd_line,
                                         &opnd_col);
               PRINTMSG(opnd_line, 1133, Error, opnd_col);
               ok = FALSE;
            }
            else if (exp_desc_r.type != Integer          &&
                     exp_desc_r.type != Typeless         &&
                     exp_desc_r.rank == 0)               {

               find_opnd_line_and_column((opnd_type *)
                                          &IL_OPND(list_idx),
                                         &opnd_line,
                                         &opnd_col);
               PRINTMSG(opnd_line, 319, Error, opnd_col);
               ok = FALSE;
            }
            else if (exp_desc_r.rank == 1 &&
                     (exp_desc_r.type == Integer ||
                      exp_desc_r.type == Typeless)) {

               (exp_desc->rank)++;

               if (IL_FLD(list_idx) == IR_Tbl_Idx  &&
                   IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

                  exp_desc->section = TRUE;

                  list2_idx = IR_IDX_L(IL_IDX(list_idx));

                  if (IL_FLD(list2_idx) == NO_Tbl_Idx) {
                     /* fill in lower bound */

                     lb_default = TRUE;

                     if (exp_desc_l.dope_vector) {
                        gen_dv_access_low_bound(&opnd2, &dv_opnd, i);
                        COPY_OPND(IL_OPND(list2_idx), opnd2);
                        IL_CONSTANT_SUBSCRIPT(list2_idx) = TRUE;

                        if (OPND_FLD(opnd2) != CN_Tbl_Idx) {
                           exp_desc->foldable = FALSE;
                           exp_desc->will_fold_later = FALSE;
                           SHAPE_FOLDABLE(IL_OPND(list2_idx)) = FALSE;
                           SHAPE_WILL_FOLD_LATER(IL_OPND(list2_idx)) = FALSE;
                        }
                     }
                     else {
                        IL_FLD(list2_idx) = BD_LB_FLD(bd_idx, i);
                        IL_IDX(list2_idx) = BD_LB_IDX(bd_idx, i);
                        IL_LINE_NUM(list2_idx) = IR_LINE_NUM(IL_IDX(list_idx));
                        IL_COL_NUM(list2_idx) = IR_COL_NUM(IL_IDX(list_idx));

                        if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
                           ADD_TMP_TO_SHARED_LIST(IL_IDX(list2_idx));
                        }

                        if (IL_FLD(list2_idx) != CN_Tbl_Idx) {
                           exp_desc->foldable = FALSE;
                           exp_desc->will_fold_later = FALSE;

                           /* assumes that this is an AT_Tbl_Idx */
                           exp_desc_r.type_idx = 
                                              ATD_TYPE_IDX(IL_IDX(list2_idx));
                           exp_desc_r.type = TYP_TYPE(exp_desc_r.type_idx);
                           exp_desc_r.linear_type = 
                                                TYP_LINEAR(exp_desc_r.type_idx);
                           SHAPE_FOLDABLE(IL_OPND(list2_idx))
                                                         = FALSE;
                           SHAPE_WILL_FOLD_LATER(
                                     IL_OPND(list2_idx)) = FALSE;
                        }
                        else {
                           SHAPE_FOLDABLE(IL_OPND(list2_idx))
                                                         = TRUE;
                           SHAPE_WILL_FOLD_LATER(
                                     IL_OPND(list2_idx)) = TRUE;
                           exp_desc_r.type_idx = CN_TYPE_IDX(IL_IDX(list2_idx));
                           exp_desc_r.type = TYP_TYPE(exp_desc_r.type_idx);
                           exp_desc_r.linear_type = 
                                                TYP_LINEAR(exp_desc_r.type_idx);
                        }

#ifdef KEY /* Bug 4709 */
		       /* Converting array bounds from to Integer_4 breaks
			* customer code which uses large array bounds, and isn't
			* correct for our 64-bit-oriented runtime.
			*/
#else
                        if (in_io_list) {

                           /* on mpp, must cast shorts to longs in io lists */
                           /* on solaris, must cast Integer_8 to Integer_4 */

                           COPY_OPND(opnd2, IL_OPND(list2_idx));
                           cast_to_cg_default(&opnd2, &exp_desc_r);
                           COPY_OPND(IL_OPND(list2_idx), opnd2);
                        }
#endif /* KEY Bug 4709 */


                        /* assume that lower bound is constant */
                        /* should be in temp.                  */
                        IL_CONSTANT_SUBSCRIPT(list2_idx) = TRUE;
                     }
                  }
                  else if (IL_FLD(list2_idx) == CN_Tbl_Idx &&
                           BD_ARRAY_CLASS(bd_idx) == Explicit_Shape &&
                           BD_LB_FLD(bd_idx, i) == CN_Tbl_Idx &&
                           fold_relationals(IL_IDX(list2_idx),
                                            BD_LB_IDX(bd_idx, i),
                                            Eq_Opr)) {
                     lb_default = TRUE;
                  }
                  else {
                     lb_default = FALSE;
                  }

                  list2_idx = IL_NEXT_LIST_IDX(list2_idx);

                  if (IL_FLD(list2_idx) == NO_Tbl_Idx) {

                     ub_default = TRUE;

                     if (i == BD_RANK(bd_idx)               &&
                         BD_ARRAY_CLASS(bd_idx) == Assumed_Size) {

                        PRINTMSG(IR_LINE_NUM(IL_IDX(list_idx)),
                                 321,Error,
                                 IR_COL_NUM(IL_IDX(list_idx)));
                        ok = FALSE;
                     }
                     else if (exp_desc_l.dope_vector) {

                        gen_dv_access_low_bound(&opnd2, &dv_opnd, i);

                        dv_idx = gen_ir(OPND_FLD(dv_opnd), OPND_IDX(dv_opnd),
                            Dv_Access_Extent,SA_INTEGER_DEFAULT_TYPE,line,col,
                                        NO_Tbl_Idx, NULL_IDX);

                        IR_DV_DIM(dv_idx)           = i;

                        plus_idx = gen_ir(OPND_FLD(opnd2), OPND_IDX(opnd2),
                                Plus_Opr,SA_INTEGER_DEFAULT_TYPE,line,col,
                                          IR_Tbl_Idx, dv_idx);

                        minus_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                                  Minus_Opr,SA_INTEGER_DEFAULT_TYPE,line,col,
                                           CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

                        IL_FLD(list2_idx) = IR_Tbl_Idx;
                        IL_IDX(list2_idx) = minus_idx;
                        IL_CONSTANT_SUBSCRIPT(list2_idx) = TRUE;
                        exp_desc->foldable = FALSE;
                        exp_desc->will_fold_later = FALSE;
                        SHAPE_FOLDABLE(IL_OPND(list2_idx)) = FALSE;
                        SHAPE_WILL_FOLD_LATER(IL_OPND(list2_idx)) = FALSE;
                     }
                     else {
                        /* fill in upper bound */
                        IL_FLD(list2_idx) = BD_UB_FLD(bd_idx, i);
                        IL_IDX(list2_idx) = BD_UB_IDX(bd_idx, i);
                        IL_LINE_NUM(list2_idx) = IR_LINE_NUM(IL_IDX(list_idx));
                        IL_COL_NUM(list2_idx) = IR_COL_NUM(IL_IDX(list_idx));

                        if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
                           ADD_TMP_TO_SHARED_LIST(IL_IDX(list2_idx));
                        }

                        if (IL_FLD(list2_idx) != CN_Tbl_Idx) {
                           exp_desc->foldable = FALSE;
                           exp_desc->will_fold_later = FALSE;
                           /* assumes that this is an AT_Tbl_Idx */
                           exp_desc_r.type_idx = 
                                              ATD_TYPE_IDX(IL_IDX(list2_idx));
                           exp_desc_r.type = TYP_TYPE(exp_desc_r.type_idx);
                           exp_desc_r.linear_type =
                                                TYP_LINEAR(exp_desc_r.type_idx);
                           SHAPE_FOLDABLE(IL_OPND(list2_idx)) = FALSE;
                           SHAPE_WILL_FOLD_LATER(IL_OPND(list2_idx)) = FALSE;
                        }
                        else {
                           SHAPE_FOLDABLE(IL_OPND(list2_idx)) = TRUE;
                           SHAPE_WILL_FOLD_LATER(IL_OPND(list2_idx)) = TRUE;
                           exp_desc_r.type_idx = CN_TYPE_IDX(IL_IDX(list2_idx));
                           exp_desc_r.type = TYP_TYPE(exp_desc_r.type_idx);
                           exp_desc_r.linear_type =
                                                TYP_LINEAR(exp_desc_r.type_idx);
                        }

#ifdef KEY /* Bug 4709 */
		      /* Converting array bounds to Integer_4 breaks
		       * customer code which uses large array bounds, and isn't
		       * correct for our 64-bit-oriented runtime.
		       */
#else
                        if (in_io_list) {

                           /* on mpp, must cast shorts to longs in io lists */
                           /* on solaris, must cast Integer_8 to Integer_4 */

                           COPY_OPND(opnd2, IL_OPND(list2_idx));
                           cast_to_cg_default(&opnd2, &exp_desc_r);
                           COPY_OPND(IL_OPND(list2_idx), opnd2);
                        }
#endif /* KEY Bug 4709 */

                        /* assume that upper bound is constant */
                        /* should be in temp.                  */
                        IL_CONSTANT_SUBSCRIPT(list2_idx) = TRUE;
                     }
                  }
                  else if (IL_FLD(list2_idx) == CN_Tbl_Idx &&
                           BD_ARRAY_CLASS(bd_idx) == Explicit_Shape &&
                           BD_UB_FLD(bd_idx, i) == CN_Tbl_Idx &&
                           fold_relationals(IL_IDX(list2_idx),
                                            BD_UB_IDX(bd_idx, i),
                                            Eq_Opr)) {
                     ub_default = TRUE;
                  }
                  else {
                     ub_default = FALSE;
                  }

                  list2_idx = IL_NEXT_LIST_IDX(list2_idx);

                  st_default = FALSE;

                  if (IL_FLD(list2_idx) == NO_Tbl_Idx) {

                     st_default = TRUE;

                     /* fill in stride = 1 */
                     IL_FLD(list2_idx) = CN_Tbl_Idx;
                     IL_IDX(list2_idx) = CN_INTEGER_ONE_IDX;
                     IL_LINE_NUM(list2_idx) = IR_LINE_NUM(IL_IDX(list_idx));
                     IL_COL_NUM(list2_idx) = IR_COL_NUM(IL_IDX(list_idx));

                     IL_CONSTANT_SUBSCRIPT(list2_idx) = TRUE;
                     SHAPE_FOLDABLE(IL_OPND(list2_idx)) = TRUE;
                     SHAPE_WILL_FOLD_LATER(IL_OPND(list2_idx)) = TRUE;
                  }
                  else if (IL_FLD(list2_idx) == CN_Tbl_Idx &&
                           compare_cn_and_value(IL_IDX(list2_idx), 0, Eq_Opr)) {

                     /* zero stride is illegal */
                     PRINTMSG(IL_LINE_NUM(list2_idx), 1001, Error,
                              IL_COL_NUM(list2_idx));
                     ok = FALSE;
                  }
                  else if (IL_FLD(list2_idx) == CN_Tbl_Idx &&
                           compare_cn_and_value(IL_IDX(list2_idx), 1, Eq_Opr)) {
                     st_default = TRUE;
                  }

                  if (lb_default &&
                      ub_default &&
                      st_default) {
                     curr_section = Full_Section;
                  }
                  else if (st_default) {
                     curr_section = Part_Section;
                  }
                  else {
                     exp_desc->contig_array = FALSE;
                  }

                  if (ok) {
                     make_triplet_extent_tree(&opnd,
                                              IR_IDX_L(IL_IDX(list_idx)));
                     COPY_OPND(exp_desc->shape[exp_desc->rank - 1], opnd);
                  }
               }
               else {
                  /* have vector subscript */
                  IL_VECTOR_SUBSCRIPT(list_idx) = TRUE;
                  exp_desc->vector_subscript    = TRUE;
                  COPY_OPND(exp_desc->shape[exp_desc->rank - 1],
                            exp_desc_r.shape[0]);
                  curr_section = Vector_Section;
               }
            }
            else if (exp_desc_r.rank > 1 ||
                     (exp_desc_r.type != Integer &&
                      exp_desc_r.type != Typeless)) {

              /* error .. vector subscript must be rank 1 integer */

               find_opnd_line_and_column((opnd_type *)
                                          &IL_OPND(list_idx),
                                         &opnd_line,
                                         &opnd_col);
               PRINTMSG(opnd_line, 320, Error,  opnd_col);
               ok = FALSE;
            }
            else if (exp_desc_r.linear_type == Short_Typeless_Const) {
               find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                         &opnd_line,
                                         &opnd_col);
               IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                      INTEGER_DEFAULT_TYPE,
                                                      opnd_line,
                                                      opnd_col);
               exp_desc_r.type_idx    = INTEGER_DEFAULT_TYPE;
               exp_desc_r.type        = Integer;
               exp_desc_r.linear_type = INTEGER_DEFAULT_TYPE;
            }


            if (in_io_list) {

               /* on mpp, must cast shorts to longs in io lists */
               /* on solaris, must cast Integer_8 to Integer_4 */

               if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

                  /* intentionally blank, handled in triplet_opr_handler */
               }
               else {
                  COPY_OPND(opnd, IL_OPND(list_idx));
// Bug 2606
# ifndef KEY
                  cast_to_cg_default(&opnd, &exp_desc_r);
# endif
                  COPY_OPND(IL_OPND(list_idx), opnd);
               }
            }

            if (curr_section == Vector_Section) {
               exp_desc->contig_array = FALSE;
            }
            else if (contig_state == Full_Section) {

               if (curr_section == Part_Section) {
                  contig_state = Part_Section;
               }
               else if (curr_section == Element) {
                  contig_state = Element;
               }
            }
            else if (contig_state == Part_Section) {
               if (curr_section == Full_Section ||
                   curr_section == Part_Section) {
                  exp_desc->contig_array = FALSE;
               }
               else if (curr_section == Element) {
                  contig_state = Element;
               }
            }
            else if (contig_state == Element) {
               if (curr_section != Element) {
                  exp_desc->contig_array = FALSE;
               }
            }

            listp_idx = list_idx;
            list_idx  = IL_NEXT_LIST_IDX(list_idx);
         }

         expr_mode        = save_expr_mode;
         xref_state       = save_xref_state;
         in_component_ref = save_in_component_ref;

         if (exp_desc->rank > 0) {
            IR_OPR(ir_idx) = Section_Subscript_Opr;
         }
         else {
            exp_desc->contig_array = FALSE;
         }

         if (exp_desc_l.rank > 0                         &&
             IR_FLD_L(ir_idx) == IR_Tbl_Idx              &&
             (IR_OPR(IR_IDX_L(ir_idx))   == Struct_Opr ||
              IR_FLD_L(IR_IDX_L(ir_idx)) == IR_Tbl_Idx)) {
            /* the subtree to left has non-zero rank */

            if (exp_desc->rank > 0) {
               PRINTMSG(IR_LINE_NUM(ir_idx), 127, Error,
                        IR_COL_NUM(ir_idx));
               ok = FALSE;
            }
            else {
               exp_desc->rank = exp_desc_l.rank;
               COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,
                          exp_desc_l.rank);
            }
         }

         if (! dump_flags.no_dimension_padding &&
             BD_RANK(bd_idx) > num_dims) {

            ATP_HAS_OVER_INDEXING(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;

            /* warn about fewer subscripts */
            PRINTMSG(line, 375, Warning, col);

            /* issue ansi msg for fewer subscripts */
            PRINTMSG(line, 376, Ansi, col);

            for (i = num_dims + 1;
                        i <= BD_RANK(bd_idx); i++) {
               NTR_IR_LIST_TBL(list_idx);
               IL_PREV_LIST_IDX(list_idx)  = listp_idx;
               IL_NEXT_LIST_IDX(list_idx)  = IL_NEXT_LIST_IDX(listp_idx);
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
               IL_NEXT_LIST_IDX(listp_idx) = list_idx;
            
               IR_LIST_CNT_R(ir_idx) += 1;

               if (exp_desc_l.dope_vector) {
                  gen_dv_access_low_bound(&opnd2, &dv_opnd, i);
                  COPY_OPND(IL_OPND(list_idx), opnd2);
                  IL_CONSTANT_SUBSCRIPT(list_idx) = TRUE;

                  if (OPND_FLD(opnd2) != CN_Tbl_Idx) {
                     exp_desc->foldable = FALSE;
                     exp_desc->will_fold_later = FALSE;
                     SHAPE_FOLDABLE(IL_OPND(list_idx)) = FALSE;
                     SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) = FALSE;
                  }
               }
               else {
                  IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
                  IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
                  IL_LINE_NUM(list_idx) = line;
                  IL_COL_NUM(list_idx) = col;

                  if (IL_FLD(list_idx) == AT_Tbl_Idx) {
                     ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
                  }

                  if (IL_FLD(list_idx) != CN_Tbl_Idx) {
                     exp_desc->foldable = FALSE;
                     exp_desc->will_fold_later = FALSE;
                     SHAPE_FOLDABLE(IL_OPND(list_idx)) = FALSE;
                     SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) = FALSE;
                  }
                  else {
                     SHAPE_FOLDABLE(IL_OPND(list_idx)) = TRUE;
                     SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) = TRUE;
                  }


                  /* assume that lower bound is constant */
                  /* should be in temp.                  */
                  IL_CONSTANT_SUBSCRIPT(list_idx) = TRUE;
               }

               listp_idx = list_idx;
            }
         }

#ifdef _F_MINUS_MINUS
         bd_idx = ATD_PE_ARRAY_IDX(attr_idx);

         if (bd_idx &&
             pe_dim_list_idx != NULL_IDX) {


            num_dims = 0;
            list_idx = pe_dim_list_idx;

            while (list_idx != NULL_IDX) {

               num_dims++;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }

            if (BD_RANK(bd_idx) < num_dims) {
               ok = FALSE;
               PRINTMSG(line, 204, Error, col);
            }
            else {

               save_expr_mode = expr_mode;

               if (expr_mode == Data_Stmt_Target) {
                  expr_mode = Data_Stmt_Target_Expr;
               }
               else if (expr_mode == Restricted_Imp_Do_Target) {
                  expr_mode = Restricted_Imp_Do_Expr;
               }

               /* process subscripts */
               list_idx  = pe_dim_list_idx;
               listp_idx = IL_PREV_LIST_IDX(list_idx);

               save_xref_state       = xref_state;

               if (xref_state != CIF_No_Usage_Rec) {
                  xref_state         = CIF_Symbol_Reference;
               }
               save_in_component_ref = in_component_ref;
               in_component_ref      = FALSE;

               for (i = 1; i <= num_dims; i++) {

                  exp_desc_r.rank = 0;
      
                  COPY_OPND(opnd, IL_OPND(list_idx));
                  ok &= expr_sem(&opnd, &exp_desc_r);
                  COPY_OPND(IL_OPND(list_idx), opnd);

                  exp_desc->has_symbolic |= exp_desc_r.has_symbolic;
                  exp_desc->has_constructor |= exp_desc_r.has_constructor;
                  exp_desc->foldable &= exp_desc_r.foldable;

                  exp_desc->will_fold_later &= (exp_desc_r.will_fold_later ||
                                                exp_desc_r.foldable);

                  IL_CONSTANT_SUBSCRIPT(list_idx) = exp_desc_r.foldable;

                  if (exp_desc_r.linear_type == Long_Typeless) {
                     find_opnd_line_and_column((opnd_type *)
                                                &IL_OPND(list_idx),
                                               &opnd_line,
                                               &opnd_col);
                     PRINTMSG(opnd_line, 1133, Error, opnd_col);
                     ok = FALSE;
                  }
                  else if (exp_desc_r.type != Integer          &&
                           exp_desc_r.type != Typeless         &&
                           exp_desc_r.rank == 0)               {

                     find_opnd_line_and_column((opnd_type *)
                                                &IL_OPND(list_idx),
                                               &opnd_line,
                                               &opnd_col);
                     PRINTMSG(opnd_line, 319, Error, opnd_col);
                     ok = FALSE;
                  }
                  else if (exp_desc_r.rank == 1 &&
                           (exp_desc_r.type == Integer ||
                            exp_desc_r.type == Typeless)) {

                     (exp_desc->rank)++;

                     find_opnd_line_and_column((opnd_type *)
                                                &IL_OPND(list_idx),
                                               &opnd_line,
                                               &opnd_col);
                     PRINTMSG(opnd_line, 1583, Error, opnd_col,
                              "array syntax", "co-array variables");
                     ok = FALSE;

                  }
                  else if (exp_desc_r.rank > 1 ||
                           (exp_desc_r.type != Integer &&
                            exp_desc_r.type != Typeless)) {

                    /* error .. vector subscript must be rank 1 integer */

                     find_opnd_line_and_column((opnd_type *)
                                                &IL_OPND(list_idx),
                                               &opnd_line,
                                               &opnd_col);
                     PRINTMSG(opnd_line, 320, Error,  opnd_col);
                     ok = FALSE;
                  }
                  else if (exp_desc_r.linear_type == Short_Typeless_Const) {
                     find_opnd_line_and_column(
                                (opnd_type *) &IL_OPND(list_idx),
                                               &opnd_line,
                                               &opnd_col);
                     IL_IDX(list_idx) = 
                           cast_typeless_constant(IL_IDX(list_idx),
                                                  INTEGER_DEFAULT_TYPE,
                                                  opnd_line,
                                                  opnd_col);
                     exp_desc_r.type_idx    = INTEGER_DEFAULT_TYPE;
                     exp_desc_r.type        = Integer;
                     exp_desc_r.linear_type = INTEGER_DEFAULT_TYPE;
                  }
      
                  listp_idx = list_idx;
                  list_idx  = IL_NEXT_LIST_IDX(list_idx);
               }

               expr_mode        = save_expr_mode;
               xref_state       = save_xref_state;
               in_component_ref = save_in_component_ref;
      
               if (exp_desc->rank > 0) {
                  IR_OPR(ir_idx) = Section_Subscript_Opr;
               }

               if (! dump_flags.no_dimension_padding &&
                   BD_RANK(bd_idx) > num_dims) {

                  ATP_HAS_OVER_INDEXING(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;

                  /* warn about fewer subscripts */
                  PRINTMSG(line, 375, Warning, col);

                  /* issue ansi msg for fewer subscripts */
                  PRINTMSG(line, 376, Ansi, col);

                  for (i = num_dims + 1;
                              i <= BD_RANK(bd_idx); i++) {
      
                     NTR_IR_LIST_TBL(list_idx);
                     IL_PREV_LIST_IDX(list_idx) = listp_idx;
                     IL_NEXT_LIST_IDX(list_idx)=IL_NEXT_LIST_IDX(listp_idx);
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                     IL_NEXT_LIST_IDX(listp_idx) = list_idx;
      
                     IR_LIST_CNT_R(ir_idx) += 1;
        
                     IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
                     IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
                     IL_LINE_NUM(list_idx) = line;
                     IL_COL_NUM(list_idx) = col;

                     if (IL_FLD(list_idx) == AT_Tbl_Idx) {
                        ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
                     }

                     if (IL_FLD(list_idx) != CN_Tbl_Idx) {
                        exp_desc->foldable = FALSE;
                        exp_desc->will_fold_later = FALSE;
                        SHAPE_FOLDABLE(IL_OPND(list_idx)) = FALSE;
                        SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) = FALSE;
                     }
                     else {
                        SHAPE_FOLDABLE(IL_OPND(list_idx)) = TRUE;
                        SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) = TRUE;
                     }


                     /* assume that lower bound is constant */
                     /* should be in temp.                  */
                     IL_CONSTANT_SUBSCRIPT(list_idx) = TRUE;

                     listp_idx = list_idx;
                  } /* for */
               }
            }
         }
# endif

         /* set accumulated rank on ir */
         IR_RANK(ir_idx)          = exp_desc->rank;

         if (exp_desc->rank == 0        &&
             !exp_desc_l.pointer        &&
             !exp_desc_l.assumed_shape) {
            exp_desc->array_elt = TRUE;
         }

         if (ok) {
            ok = check_array_bounds(ir_idx);
         }

# if defined(_F_MINUS_MINUS)
# if defined(_TARGET_OS_MAX)
         if (save_pe_dv_list_idx != NULL_IDX) {

            /* add the pe subscript to ir_idx */
            list_idx = IR_IDX_R(ir_idx);

            while (IL_NEXT_LIST_IDX(list_idx)) {
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }

            IL_NEXT_LIST_IDX(list_idx) = save_pe_dv_list_idx;
            IL_PREV_LIST_IDX(save_pe_dv_list_idx) = list_idx;
            IR_LIST_CNT_R(ir_idx) += 1;
         } else 
# endif
         if (ok                           &&
             ATD_PE_ARRAY_IDX(attr_idx)) {

            if (pe_dim_list_idx != NULL_IDX) {

               translate_distant_ref(result_opnd, exp_desc, pe_dim_list_idx);
            }
# if defined(_TARGET_OS_MAX)
            else if (! ATD_ALLOCATABLE(attr_idx)) {
               /* supply mype() as pe dim */

               list_idx = IR_IDX_R(ir_idx);
               while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               IR_LIST_CNT_R(ir_idx) += 1;

               NTR_IR_TBL(plus_idx);
               IR_OPR(plus_idx) = My_Pe_Opr;
               IR_TYPE_IDX(plus_idx) = INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(plus_idx) = IR_LINE_NUM(ir_idx);
               IR_COL_NUM(plus_idx) = IR_COL_NUM(ir_idx);

               IL_FLD(list_idx) = IR_Tbl_Idx;
               IL_IDX(list_idx) = plus_idx;

               IL_PE_SUBSCRIPT(list_idx) = TRUE;
               io_item_must_flatten = TRUE;
            }
# endif
         }
# endif

         /* insert substring if allowed */

         if (ok    &&
             save_insert_subs_ok     &&
             ! no_sub_or_deref       &&
             exp_desc->type == Character) {

            ok = gen_whole_substring(result_opnd, exp_desc->rank);
         }
      }
   } /* if array */
   else if (IR_FLD_L(ir_idx) == AT_Tbl_Idx                &&
            AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Pgm_Unit) {

      IR_OPR(ir_idx) = Call_Opr;

      ok = expr_sem(result_opnd, exp_desc);
   }
   else {
      /* some sort of internal error */
      PRINTMSG(line, 975, Internal, col);
   }

   TRACE (Func_Exit, "subscript_opr_handler", NULL);

   return(ok);

}  /* subscript_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Whole_Substring_Opr and Substring_Opr.       *|
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

static boolean substring_opr_handler(opnd_type		*result_opnd,
				     expr_arg_type	*exp_desc,
				     int		 rank_in)

{
#ifdef KEY /* Bug 10177 */
   int			attr_idx = 0;
#else /* KEY Bug 10177 */
   int			attr_idx;
#endif /* KEY Bug 10177 */
   char		       *char_ptr1;
   char		       *char_ptr2;
   int			clen_idx;
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   int			i;
   int			ir_idx;
   int			line;
   int			list_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   boolean		save_defer_stmt_expansion;
   expr_mode_type       save_expr_mode;
   boolean		save_in_component_ref;
   int			save_number_of_functions;
   cif_usage_code_type  save_xref_state;
   int			tmp_idx;
   int			type_idx;


   TRACE (Func_Entry, "substring_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   
   exp_desc_l.rank = rank_in;

   /* do not change in_call_list for the left hand side */

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   insert_subs_ok = FALSE;
   ok = expr_sem(&opnd, &exp_desc_l);
   insert_subs_ok = TRUE;
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   /* set in_call_list to false for right hand side */
   in_call_list = FALSE;

   if (OPND_FLD(opnd) == CN_Tbl_Idx) {
      type_idx = CN_TYPE_IDX(OPND_IDX(opnd));
   }
   else {
      attr_idx = find_base_attr(&opnd, &line, &col);
      type_idx = ATD_TYPE_IDX(attr_idx);
   }

   exp_desc->has_constructor = exp_desc_l.has_constructor;
   exp_desc->has_symbolic = exp_desc_l.has_symbolic;

   exp_desc->constant    = exp_desc_l.constant;
   exp_desc->foldable    = exp_desc_l.foldable;
   exp_desc->will_fold_later = exp_desc_l.will_fold_later;

   exp_desc->rank        = exp_desc_l.rank;
   COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,exp_desc_l.rank);
   exp_desc->type        = exp_desc_l.type;
   exp_desc->linear_type = exp_desc_l.linear_type;
   exp_desc->type_idx    = exp_desc_l.type_idx;

   if (exp_desc->linear_type == Short_Char_Const) {

      /* Assume that the subscript is not constant for now.*/
      /* If it folds down below, it will be changed to     */
      /* Short_Char_Const again.                           */
      /* We cannot allow i = "abcdefg"(1:N)                */

      type_tbl[TYP_WORK_IDX]        = type_tbl[exp_desc->type_idx];
      TYP_LINEAR(TYP_WORK_IDX)      = Character_1;
      exp_desc->type_idx            = ntr_type_tbl();
      exp_desc->linear_type         = Character_1;
   }

   /* length is run time dependent */

   if (IR_OPR(ir_idx) == Whole_Substring_Opr) {
      exp_desc->pointer = exp_desc_l.pointer;
      exp_desc->target  = exp_desc_l.target;
   }
   else {
      exp_desc->target   = exp_desc_l.target ||
                             exp_desc_l.pointer;
   }

   exp_desc->vector_subscript = exp_desc_l.vector_subscript;
   exp_desc->reference        = exp_desc_l.reference;
   exp_desc->pe_dim_ref       = exp_desc_l.pe_dim_ref;
   COPY_OPND((exp_desc->bias_opnd), (exp_desc_l.bias_opnd));
   exp_desc->cif_id           = exp_desc_l.cif_id;
   exp_desc->component        = exp_desc_l.component;
   exp_desc->section          = exp_desc_l.section;
   exp_desc->array_elt        = exp_desc_l.array_elt;
   exp_desc->dope_vector      = exp_desc_l.dope_vector;
   exp_desc->contig_array     = exp_desc_l.contig_array;
   exp_desc->dist_reshape_ref = exp_desc_l.dist_reshape_ref;


   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;


   if (exp_desc_l.type != Character) {
      PRINTMSG(line, 508, Error, col);
      ok = FALSE;
   }

   save_expr_mode        = expr_mode;
   save_xref_state       = xref_state;

   if (xref_state != CIF_No_Usage_Rec) {
      xref_state         = CIF_Symbol_Reference;
   }
   save_in_component_ref = in_component_ref;
   in_component_ref      = FALSE;

   if (expr_mode == Data_Stmt_Target) {
      expr_mode = Data_Stmt_Target_Expr;
   }
   else if (expr_mode == Restricted_Imp_Do_Target) {
      expr_mode = Restricted_Imp_Do_Expr;
   }

   list_idx = IR_IDX_R(ir_idx);

   exp_desc_r.rank = 0;
   save_number_of_functions = number_of_functions;
   number_of_functions = 0;

   COPY_OPND(opnd, IL_OPND(list_idx));
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IL_OPND(list_idx), opnd);

   exp_desc->has_symbolic |= exp_desc_r.has_symbolic;
   exp_desc->has_constructor |= exp_desc_r.has_constructor;

   if (IL_FLD(list_idx) == NO_Tbl_Idx) {
      /* fill in const 1 */
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = col;
      exp_desc_r.foldable             = TRUE;
   }
   else if (exp_desc_r.linear_type == Long_Typeless) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &opnd_line,
                                &opnd_col);
      PRINTMSG(opnd_line, 1133, Error, opnd_col);
      ok = FALSE;
   }
   else if (exp_desc_r.rank != 0 ||
            (exp_desc_r.type != Integer &&
             exp_desc_r.type != Typeless)) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &opnd_line,
                                &opnd_col);
      PRINTMSG(opnd_line, 323, Error, opnd_col);
      ok = FALSE;
   }
   else if (exp_desc_r.linear_type == Short_Typeless_Const) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &opnd_line,
                                &opnd_col);
      IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                INTEGER_DEFAULT_TYPE,
                                                opnd_line,
                                                opnd_col);
      exp_desc_r.type_idx    = INTEGER_DEFAULT_TYPE;
      exp_desc_r.type        = Integer;
      exp_desc_r.linear_type = INTEGER_DEFAULT_TYPE;
   }

   if (ok &&
       IL_FLD(list_idx) == CN_Tbl_Idx &&
       compare_cn_and_value(IL_IDX(list_idx), 1, Eq_Opr)) {
      /* intentionally blank */
   }
   else {
      exp_desc->contig_array = FALSE;
   }

   exp_desc->foldable = exp_desc->foldable &&
                        exp_desc_r.foldable;

   exp_desc->will_fold_later &= (exp_desc_r.will_fold_later ||
                                 exp_desc_r.foldable);

   IL_CONSTANT_SUBSCRIPT(list_idx) = exp_desc_r.foldable;

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   exp_desc_r.rank = 0;

   COPY_OPND(opnd, IL_OPND(list_idx));
   ok &= expr_sem(&opnd, &exp_desc_r);
   COPY_OPND(IL_OPND(list_idx), opnd);

   exp_desc->has_symbolic |= exp_desc_r.has_symbolic;
   exp_desc->has_constructor |= exp_desc_r.has_constructor;

   if (IL_FLD(list_idx) == NO_Tbl_Idx ||
       (IL_FLD(list_idx) == CN_Tbl_Idx &&
        TYP_CHAR_CLASS(type_idx) == Const_Len_Char &&
        TYP_FLD(type_idx) == CN_Tbl_Idx &&
        fold_relationals(IL_IDX(list_idx), TYP_IDX(type_idx), Eq_Opr))) {

      /* intentionally blank */
   }
   else {
      exp_desc->contig_array = FALSE;
   }

   if (IL_FLD(list_idx) == NO_Tbl_Idx) { /* fill in string length */

      if (IR_FLD_L(ir_idx)         != CN_Tbl_Idx   &&
          ATD_CLASS(attr_idx)      == CRI__Pointee &&
          TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {

         NTR_IR_TBL(clen_idx);
         IR_OPR(clen_idx)        = Clen_Opr;
         IR_TYPE_IDX(clen_idx)   = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(clen_idx)   = line;
         IR_COL_NUM(clen_idx)    = col;
         IR_FLD_L(clen_idx)      = AT_Tbl_Idx;
         IR_IDX_L(clen_idx)      = attr_idx;
         IR_LINE_NUM_L(clen_idx) = line;
         IR_COL_NUM_L(clen_idx)  = col;
         IL_FLD(list_idx)        = IR_Tbl_Idx;
         IL_IDX(list_idx)        = clen_idx;
      }
      else {
         IL_FLD(list_idx)   = TYP_FLD(type_idx);
         IL_IDX(list_idx)   = TYP_IDX(type_idx);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = col;

         if (IL_FLD(list_idx) == AT_Tbl_Idx) {
            ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
         }

         if (IL_FLD(list_idx) == CN_Tbl_Idx) {
            exp_desc_r.foldable = TRUE;
         }
      }
   }
   else if (exp_desc_r.linear_type == Long_Typeless) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &opnd_line,
                                &opnd_col);
      PRINTMSG(opnd_line, 1133, Error, opnd_col);
      ok = FALSE;
   }
   else if (exp_desc_r.rank != 0 ||
            (exp_desc_r.type != Integer &&
             exp_desc_r.type != Typeless)) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &opnd_line,
                                &opnd_col);
      PRINTMSG(opnd_line, 323, Error, opnd_col);
      ok = FALSE;
   }
   else if (exp_desc_r.linear_type == Short_Typeless_Const) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &opnd_line,
                                &opnd_col);
      IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                INTEGER_DEFAULT_TYPE,
                                                opnd_line,
                                                opnd_col);
      exp_desc_r.type_idx    = INTEGER_DEFAULT_TYPE;
      exp_desc_r.type        = Integer;
      exp_desc_r.linear_type = INTEGER_DEFAULT_TYPE;
   }

   exp_desc->foldable = exp_desc->foldable && exp_desc_r.foldable;
   exp_desc->will_fold_later &= (exp_desc_r.will_fold_later ||
                                 exp_desc_r.foldable);

   IL_CONSTANT_SUBSCRIPT(list_idx) = exp_desc_r.foldable;

   if (ok) {

      add_substring_length(ir_idx);

      if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
          (IR_OPR(IR_IDX_L(ir_idx)) == Substring_Opr ||
           IR_OPR(IR_IDX_L(ir_idx)) == Whole_Substring_Opr)) {

         /* this is only seen during var size function result */
         /* processing. Fold out the extra substring_opr */

         fold_nested_substrings(ir_idx);
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(exp_desc->char_len, IL_OPND(list_idx));

      ok &= check_substring_bounds(ir_idx);

      if (ok           &&
          IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
          IL_FLD(list_idx) == CN_Tbl_Idx &&
          exp_desc->foldable)            {

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)       = Character;
         TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)        = IL_FLD(list_idx),
         TYP_IDX(TYP_WORK_IDX)        = IL_IDX(list_idx),
         TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
         exp_desc->type               = Character;
         exp_desc->linear_type        = TYP_LINEAR(TYP_WORK_IDX);
         exp_desc->type_idx           = ntr_type_tbl();

         OPND_FLD((*result_opnd))      = CN_Tbl_Idx;
         OPND_LINE_NUM((*result_opnd)) = line;
         OPND_COL_NUM((*result_opnd))  = col;

         /* set up the new const table entry */

         OPND_IDX((*result_opnd))= ntr_const_tbl(exp_desc->type_idx,
                                                 TRUE,
                                                 NULL);
         /* BRIANJ - String manipulation */

         char_ptr1 = (char *)&CN_CONST(OPND_IDX((*result_opnd)));
         char_ptr2 = (char *)&CN_CONST(IR_IDX_L(ir_idx)) +
                      CN_INT_TO_C(IL_IDX(IR_IDX_R(ir_idx))) - 1;

         for (i=0; i < CN_INT_TO_C(IL_IDX(list_idx)); i++) {
            char_ptr1[i] = char_ptr2[i];
         }

         /* fill in the rest of a word with blanks */

         while (i % TARGET_CHARS_PER_WORD != 0) {
            char_ptr1[i] = ' ';
            i++;
         }

         if (compare_cn_and_value(TYP_IDX(exp_desc->type_idx),
                                  MAX_CHARS_IN_TYPELESS, 
                                  Le_Opr)) {
            exp_desc->linear_type   = Short_Char_Const;
            type_tbl[TYP_WORK_IDX]  = type_tbl[exp_desc->type_idx];
            TYP_LINEAR(TYP_WORK_IDX)= Short_Char_Const;
            exp_desc->type_idx      = ntr_type_tbl();

         }
      }
      else if (ok &&
               IR_FLD_L(ir_idx) == CN_Tbl_Idx) {

         stmt_expansion_control_start();
         save_defer_stmt_expansion = defer_stmt_expansion;
         defer_stmt_expansion = FALSE;

         /* substring of character literal that doesn't fold */
         /* like ...  "abcdefg"(1:N)                         */
         /* the literal must be put in a static variable.    */

         tmp_idx = gen_initialized_tmp(IR_IDX_L(ir_idx), line,col);

         IR_FLD_L(ir_idx) = AT_Tbl_Idx;
         IR_IDX_L(ir_idx) = tmp_idx;
         IR_LINE_NUM_L(ir_idx) = line;
         IR_COL_NUM_L(ir_idx)  = col;

         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         defer_stmt_expansion = save_defer_stmt_expansion;
         stmt_expansion_control_end(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);
      }
   }

   number_of_functions += save_number_of_functions;

   expr_mode        = save_expr_mode;
   xref_state       = save_xref_state;
   in_component_ref = save_in_component_ref;


   TRACE (Func_Exit, "substring_opr_handler", NULL);

   return(ok);

}  /* substring_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Triplet_Opr.                                 *|
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

static boolean triplet_opr_handler(opnd_type		*result_opnd,
				   expr_arg_type	*exp_desc)

{
   expr_arg_type	exp_desc_l;
   int			ir_idx;
   int			list_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;


   TRACE (Func_Entry, "triplet_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   in_call_list = FALSE;
   
   exp_desc->constant = TRUE;
   exp_desc->foldable = TRUE;
   exp_desc->will_fold_later = TRUE;

   list_idx = IR_IDX_L(ir_idx);
   COPY_OPND(opnd, IL_OPND(list_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IL_OPND(list_idx), opnd);

   IL_CONSTANT_SUBSCRIPT(list_idx) = exp_desc_l.foldable;

   exp_desc->has_constructor = exp_desc_l.has_constructor;
   exp_desc->has_symbolic = exp_desc_l.has_symbolic;

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      if (exp_desc_l.linear_type == Long_Typeless) {
         find_opnd_line_and_column((opnd_type *)
                                   &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         PRINTMSG(opnd_line, 1133, Error, opnd_col);
         ok = FALSE;
      }
      else if (exp_desc_l.rank > 0 ||
               (exp_desc_l.type != Integer &&
                exp_desc_l.type != Typeless)) {

         /* error .. must be scalar int expr */

         ok = FALSE;
         find_opnd_line_and_column((opnd_type *)
                                   &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         PRINTMSG(opnd_line, 319, Error, opnd_col);
      }
      else if (exp_desc_l.linear_type == Short_Typeless_Const) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                   INTEGER_DEFAULT_TYPE,
                                                   opnd_line,
                                                   opnd_col);
         exp_desc_l.type_idx    = INTEGER_DEFAULT_TYPE;
         exp_desc_l.type        = Integer;
         exp_desc_l.linear_type = INTEGER_DEFAULT_TYPE;
      }

      exp_desc->constant = exp_desc_l.constant;
      exp_desc->foldable = exp_desc_l.foldable;
      exp_desc->will_fold_later = exp_desc_l.will_fold_later ||
                                  exp_desc_l.foldable;
      SHAPE_FOLDABLE(IL_OPND(list_idx)) = exp_desc_l.foldable;
      SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) =
                                     exp_desc_l.will_fold_later;

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
      if (in_io_list) {

         /* on mpp, must cast shorts to longs in io lists */
         /* on solaris, must cast Integer_8 to Integer_4 */

         COPY_OPND(opnd, IL_OPND(list_idx));
         cast_to_cg_default(&opnd, &exp_desc_l);
         COPY_OPND(IL_OPND(list_idx), opnd);
      }
#endif /* KEY Bug 4709 */
   }


   list_idx = IL_NEXT_LIST_IDX(list_idx);
   exp_desc_l.rank = 0;
   COPY_OPND(opnd, IL_OPND(list_idx));
   ok &= expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IL_OPND(list_idx), opnd);

   IL_CONSTANT_SUBSCRIPT(list_idx) = exp_desc_l.foldable;

   exp_desc->has_symbolic |= exp_desc_l.has_symbolic;
   exp_desc->has_constructor |= exp_desc_l.has_constructor;

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      if (exp_desc_l.linear_type == Long_Typeless) {
         find_opnd_line_and_column((opnd_type *)
                                   &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         PRINTMSG(opnd_line, 1133, Error, opnd_col);
         ok = FALSE;
      }
      else if (exp_desc_l.rank > 0 ||
               (exp_desc_l.type != Integer &&
                exp_desc_l.type != Typeless)) {

         /* error .. must be scalar int expr */

         ok = FALSE;
         find_opnd_line_and_column((opnd_type *)
                                   &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         PRINTMSG(opnd_line, 319, Error, opnd_col);
      }
      else if (exp_desc_l.linear_type == Short_Typeless_Const) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                   INTEGER_DEFAULT_TYPE,
                                                   opnd_line,
                                                   opnd_col);
         exp_desc_l.type_idx    = INTEGER_DEFAULT_TYPE;
         exp_desc_l.type        = Integer;
         exp_desc_l.linear_type = INTEGER_DEFAULT_TYPE;
      }

      exp_desc->constant = exp_desc->constant && exp_desc_l.constant;
      exp_desc->foldable = exp_desc->foldable && exp_desc_l.foldable;
      exp_desc->will_fold_later &= (exp_desc_l.will_fold_later ||
                                    exp_desc_l.foldable);
      SHAPE_FOLDABLE(IL_OPND(list_idx)) = exp_desc_l.foldable;
      SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) = exp_desc_l.will_fold_later;

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
      if (in_io_list) {

         /* on mpp, must cast shorts to longs in io lists */
         /* on solaris, must cast Integer_8 to Integer_4 */

         COPY_OPND(opnd, IL_OPND(list_idx));
         cast_to_cg_default(&opnd, &exp_desc_l);
         COPY_OPND(IL_OPND(list_idx), opnd);

      }
#endif /* KEY Bug 4709 */
   }


   exp_desc_l.rank = 0;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(opnd, IL_OPND(list_idx));
   ok &= expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IL_OPND(list_idx), opnd);

   IL_CONSTANT_SUBSCRIPT(list_idx) = exp_desc_l.foldable;

   exp_desc->has_symbolic |= exp_desc_l.has_symbolic;
   exp_desc->has_constructor |= exp_desc_l.has_constructor;

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      if (exp_desc_l.linear_type == Long_Typeless) {
         find_opnd_line_and_column((opnd_type *)
                                   &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         PRINTMSG(opnd_line, 1133, Error, opnd_col);
         ok = FALSE;
      }
      else if (exp_desc_l.rank > 0 ||
               (exp_desc_l.type != Integer &&
                exp_desc_l.type != Typeless)) {

         /* error .. must be scalar int expr */

         ok = FALSE;
         find_opnd_line_and_column((opnd_type *)
                                   &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         PRINTMSG(opnd_line, 319, Error, opnd_col);
      }
      else if (exp_desc_l.linear_type == Short_Typeless_Const) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_col);
         IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                   INTEGER_DEFAULT_TYPE,
                                                   opnd_line,
                                                   opnd_col);
         exp_desc_l.type_idx    = INTEGER_DEFAULT_TYPE;
         exp_desc_l.type        = Integer;
         exp_desc_l.linear_type = INTEGER_DEFAULT_TYPE;
      }

      exp_desc->constant = exp_desc->constant && exp_desc_l.constant;
      exp_desc->foldable = exp_desc->foldable && exp_desc_l.foldable;
      exp_desc->will_fold_later &= (exp_desc_l.will_fold_later ||
                                 exp_desc_l.foldable);
      SHAPE_FOLDABLE(IL_OPND(list_idx)) = exp_desc_l.foldable;
      SHAPE_WILL_FOLD_LATER(IL_OPND(list_idx)) = exp_desc_l.will_fold_later;

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
      if (in_io_list) {

         /* on mpp, must cast shorts to longs in io lists */
         /* on solaris, must cast Integer_8 to Integer_4 */

         COPY_OPND(opnd, IL_OPND(list_idx));
         cast_to_cg_default(&opnd, &exp_desc_l);
         COPY_OPND(IL_OPND(list_idx), opnd);

      }
#endif /* KEY Bug 4709 */
   }

   exp_desc->rank           = 1;
   exp_desc->type           = Integer;
   exp_desc->type_idx       = CG_INTEGER_DEFAULT_TYPE;
   exp_desc->linear_type    = TYP_LINEAR(exp_desc->type_idx);

   IR_TYPE_IDX(ir_idx)      = exp_desc->type_idx;
   IR_RANK(ir_idx)          = exp_desc->rank;


   TRACE (Func_Exit, "triplet_opr_handler", NULL);

   return(ok);

}  /* triplet_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Dealloc_Obj_Opr.                             *|
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

static boolean dealloc_obj_opr_handler(opnd_type	*result_opnd,
				       expr_arg_type	*exp_desc,
                                       int		 rank_in)

{
   int			attr_idx;
   int			col;
   expr_arg_type	exp_desc_l;
   int			ir_idx;
   int			line;
   boolean		ok = TRUE;
   opnd_type		opnd;

   TRACE (Func_Entry, "dealloc_obj_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   in_call_list = FALSE;
   
   exp_desc_l.rank = rank_in;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   insert_subs_ok = FALSE;
   pgm_unit_illegal = FALSE;
   ok = expr_sem(&opnd, &exp_desc_l);
   insert_subs_ok = TRUE;
   pgm_unit_illegal = TRUE;
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   attr_idx = find_base_attr(&opnd, &line, &col);

   if (attr_idx                            &&
       AT_OBJ_CLASS(attr_idx) == Data_Obj) {

      exp_desc->type        = exp_desc_l.type;
      exp_desc->linear_type = exp_desc_l.linear_type;
      exp_desc->type_idx    = exp_desc_l.type_idx;
      exp_desc->rank        = 0;
      exp_desc->constant    = exp_desc_l.constant;
      exp_desc->foldable    = exp_desc_l.foldable;
      exp_desc->reference   = TRUE;
      exp_desc->component   = exp_desc_l.component;
      exp_desc->has_symbolic= exp_desc_l.has_symbolic;

      IR_TYPE_IDX(ir_idx)   = exp_desc->type_idx;
      IR_RANK(ir_idx)       = exp_desc->rank;

      if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {

         if (! ATD_POINTER(attr_idx)) {
            /* error .. scalar must be pointer*/
            ok = FALSE;
            PRINTMSG(line, 428, Error, col);
         }

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
            /* had shape spec and musn't */
            PRINTMSG(line, 975, Internal, col);
         }

         /* might want to remove Alloc_Obj_Opr here */
      }
      else {

         if (!ATD_POINTER(attr_idx) && !ATD_ALLOCATABLE(attr_idx)) {

            /* error .. must be allocatable or pointer */
            ok = FALSE;
            PRINTMSG(line, 428, Error, col);
         }
      }
   }
   else {
      /* error .. must be allocatable or pointer */
      ok = FALSE;
      PRINTMSG(line, 428, Error, col);
   }


   TRACE (Func_Exit, "dealloc_obj_opr_handler", NULL);

   return(ok);

}  /* dealloc_obj_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Alloc_Obj_Opr.                               *|
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

static boolean alloc_obj_opr_handler(opnd_type		*result_opnd,
				     expr_arg_type	*exp_desc,
				     int		 rank_in)

{
   int			attr_idx;
   int			bd_idx;
   int			col;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   int			i;
   int			ir_idx;
   int			line;
   int			listp_idx;
   int			list_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   int			pe_bd_idx;
   boolean		save_in_component_ref;
   cif_usage_code_type	save_xref_state;


   TRACE (Func_Entry, "alloc_obj_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   in_call_list = FALSE;
   
   exp_desc_l.rank = rank_in;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   insert_subs_ok = FALSE;
   pgm_unit_illegal = FALSE;
   ok = expr_sem(&opnd, &exp_desc_l);
   insert_subs_ok = TRUE;
   pgm_unit_illegal = TRUE;
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   attr_idx = find_base_attr(&opnd, &line, &col);

   if (attr_idx                            &&
       AT_OBJ_CLASS(attr_idx) == Data_Obj) {

      exp_desc->type        = exp_desc_l.type;
      exp_desc->linear_type = exp_desc_l.linear_type;
      exp_desc->type_idx    = exp_desc_l.type_idx;
      exp_desc->rank        = 0;
      exp_desc->constant    = exp_desc_l.constant;
      exp_desc->foldable    = exp_desc_l.foldable;
      exp_desc->reference   = TRUE;
      exp_desc->component   = exp_desc_l.component;
      exp_desc->has_symbolic= exp_desc_l.has_symbolic;

      IR_TYPE_IDX(ir_idx)   = exp_desc->type_idx;
      IR_RANK(ir_idx)       = exp_desc->rank;

      bd_idx = ATD_ARRAY_IDX(attr_idx);
      pe_bd_idx = ATD_PE_ARRAY_IDX(attr_idx);

      if (bd_idx == NULL_IDX) {

         if (! ATD_POINTER(attr_idx)) {
            /* error .. scalar must be pointer*/
            ok = FALSE;
            PRINTMSG(line, 201, Error, col);
         }

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
            /* had shape spec and musn't */
            PRINTMSG(line, 975, Internal, col);
         }

         /* might want to remove Alloc_Obj_Opr here */
      }
      else if (IR_FLD_R(ir_idx) == NO_Tbl_Idx) {
         ok = FALSE;
         PRINTMSG(line, 205, Error, col);
      }
      else if (pe_bd_idx &&
               BD_RANK(pe_bd_idx) + BD_RANK(bd_idx) != IR_LIST_CNT_R(ir_idx)) {

         ok = FALSE;
         PRINTMSG(line, 402, Error, col);
      }
      else if (pe_bd_idx == NULL_IDX &&
               BD_RANK(ATD_ARRAY_IDX(attr_idx)) != IR_LIST_CNT_R(ir_idx)) {
         ok = FALSE;
         PRINTMSG(line, 402, Error, col);
      }
      else {

         if (!ATD_POINTER(attr_idx) && !ATD_ALLOCATABLE(attr_idx)) {

            /* error .. must be allocatable or pointer */
            ok = FALSE;
            PRINTMSG(line, 201, Error, col);
         }

         /* process subscripts */
         list_idx              = IR_IDX_R(ir_idx);
         save_xref_state       = xref_state;

         if (xref_state != CIF_No_Usage_Rec) {
            xref_state         = CIF_Symbol_Reference;
         }
         save_in_component_ref = in_component_ref;
         in_component_ref      = FALSE;

         for (i = 1; i <= IR_LIST_CNT_R(ir_idx); i++) {

            if (IL_FLD(list_idx) == IL_Tbl_Idx) {
               /* lower and upper bound here */

               /* lower */

               listp_idx = IL_IDX(list_idx);

               exp_desc_r.rank = 0;

               COPY_OPND(opnd, IL_OPND(listp_idx));
               ok &= expr_sem(&opnd, &exp_desc_r);
               COPY_OPND(IL_OPND(listp_idx), opnd);

               if (exp_desc_r.linear_type == Long_Typeless) {

                  find_opnd_line_and_column((opnd_type *)
                                            &IL_OPND(listp_idx),
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 1133, Error, opnd_col);
                  ok = FALSE;
               }
               else if ((exp_desc_r.type != Integer &&
                         exp_desc_r.type != Typeless) ||
                        exp_desc_r.rank != 0)         {

                  find_opnd_line_and_column((opnd_type *)
                                            &IL_OPND(listp_idx),
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 403, Error, opnd_col);
                  ok = FALSE;
               }
               else if (exp_desc_r.linear_type == Short_Typeless_Const) {
                  find_opnd_line_and_column((opnd_type *) &IL_OPND(listp_idx),
                                            &opnd_line,
                                            &opnd_col);
                  IL_IDX(listp_idx) = cast_typeless_constant(IL_IDX(listp_idx),
                                                         INTEGER_DEFAULT_TYPE,
                                                         opnd_line,
                                                         opnd_col);
                  exp_desc_r.type_idx    = INTEGER_DEFAULT_TYPE;
                  exp_desc_r.type        = Integer;
                  exp_desc_r.linear_type = INTEGER_DEFAULT_TYPE;
               }

               exp_desc->constant = exp_desc->constant && exp_desc_r.constant;
               exp_desc->foldable = exp_desc->foldable && exp_desc_r.foldable;

               /* upper */

               listp_idx = IL_NEXT_LIST_IDX(listp_idx);

               exp_desc_r.rank = 0;

               COPY_OPND(opnd, IL_OPND(listp_idx));
               ok &= expr_sem(&opnd, &exp_desc_r);
               COPY_OPND(IL_OPND(listp_idx), opnd);

               if (exp_desc_r.linear_type == Long_Typeless) {
                  find_opnd_line_and_column((opnd_type *)
                                            &IL_OPND(listp_idx),
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 1133, Error, opnd_col);
                  ok = FALSE;
               }
               else if ((exp_desc_r.type != Integer &&
                         exp_desc_r.type != Typeless) ||
                        exp_desc_r.rank != 0)         {

                  find_opnd_line_and_column((opnd_type *)
                                            &IL_OPND(listp_idx),
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 403, Error, opnd_col);
                  ok = FALSE;
               }
               else if (exp_desc_r.linear_type == Short_Typeless_Const) {
                  find_opnd_line_and_column((opnd_type *) &IL_OPND(listp_idx),
                                            &opnd_line,
                                            &opnd_col);
                  IL_IDX(listp_idx) = cast_typeless_constant(IL_IDX(listp_idx),
                                                         INTEGER_DEFAULT_TYPE,
                                                         opnd_line,
                                                         opnd_col);
                  exp_desc_r.type_idx    = INTEGER_DEFAULT_TYPE;
                  exp_desc_r.type        = Integer;
                  exp_desc_r.linear_type = INTEGER_DEFAULT_TYPE;
               }

               exp_desc->constant = exp_desc->constant &&
                                      exp_desc_r.constant;
               exp_desc->foldable = exp_desc->foldable &&
                                      exp_desc_r.foldable;

            }
            else {
               /* just have upper bound */

               exp_desc_r.rank = 0;

               COPY_OPND(opnd, IL_OPND(list_idx));
               ok &= expr_sem(&opnd, &exp_desc_r);
               COPY_OPND(IL_OPND(list_idx), opnd);


               if (exp_desc_r.linear_type == Long_Typeless) {
                  find_opnd_line_and_column((opnd_type *)
                                            &IL_OPND(list_idx),
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 1133, Error, opnd_col);

                  ok = FALSE;
               }
               else if ((exp_desc_r.type != Integer &&
                         exp_desc_r.type != Typeless)  ||
                        exp_desc_r.rank != 0)          {

                  find_opnd_line_and_column((opnd_type *)
                                            &IL_OPND(list_idx),
                                            &opnd_line,
                                            &opnd_col);
                  PRINTMSG(opnd_line, 403, Error, opnd_col);

                  ok = FALSE;
               }
               else if (exp_desc_r.linear_type == Short_Typeless_Const) {
                  find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                            &opnd_line,
                                            &opnd_col);
                  IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                         INTEGER_DEFAULT_TYPE,
                                                         opnd_line,
                                                         opnd_col);
                  exp_desc_r.type_idx    = INTEGER_DEFAULT_TYPE;
                  exp_desc_r.type        = Integer;
                  exp_desc_r.linear_type = INTEGER_DEFAULT_TYPE;
               }

               exp_desc->constant = exp_desc->constant && exp_desc_r.constant;
               exp_desc->foldable = exp_desc->foldable && exp_desc_r.foldable;
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);

         } /* for ... */

         xref_state       = save_xref_state;
         in_component_ref = save_in_component_ref;

      } /* else process subscripts */
   } /* if data_obj */
   else {
      /* error .. must be allocatable or pointer */
      ok = FALSE;
      PRINTMSG(line, 201, Error, col);
   }


   TRACE (Func_Exit, "alloc_obj_opr_handler", NULL);

   return(ok);

}  /* alloc_obj_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Cvrt_Opr.                                    *|
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

static boolean cvrt_opr_handler(opnd_type		*result_opnd,
				expr_arg_type		*exp_desc)

{
   expr_arg_type	exp_desc_l;
   long_type            folded_const[MAX_WORDS_FOR_NUMERIC];
   int			ir_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			type_idx;


   TRACE (Func_Entry, "cvrt_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   exp_desc->has_constructor = exp_desc_l.has_constructor;

   exp_desc->has_symbolic     = exp_desc_l.has_symbolic;
   exp_desc->constant         = exp_desc_l.constant;
   exp_desc->foldable         = exp_desc_l.foldable;
   exp_desc->will_fold_later  = exp_desc_l.will_fold_later;
   exp_desc->rank             = exp_desc_l.rank;
   exp_desc->type             = TYP_TYPE(IR_TYPE_IDX(ir_idx));
   exp_desc->type_idx         = IR_TYPE_IDX(ir_idx);
   exp_desc->linear_type      = TYP_LINEAR(IR_TYPE_IDX(ir_idx));

   COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,exp_desc_l.rank);
   COPY_OPND(exp_desc->char_len, exp_desc_l.char_len);

   if (exp_desc_l.linear_type == exp_desc->linear_type) {
      /* cvrt_Opr not needed */
      COPY_OPND((*result_opnd), IR_OPND_L(ir_idx));
   }
   else if (opt_flags.ieeeconform &&
            ! comp_gen_expr       &&
            (exp_desc_l.type == Real ||
             exp_desc_l.type == Complex)) {

      /* don't fold real arithmatic under ieeeconform */

      exp_desc->foldable = FALSE;
      exp_desc->will_fold_later = FALSE;
   }
   else if (exp_desc->foldable             &&
            IR_FLD_L(ir_idx) == CN_Tbl_Idx &&
            exp_desc_l.type == Typeless) {

      OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
      OPND_IDX((*result_opnd)) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                        exp_desc->type_idx,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));
      OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
      OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
   }
   else if (exp_desc->foldable             &&
            IR_FLD_L(ir_idx) == CN_Tbl_Idx) {

      type_idx = exp_desc->type_idx;

      if (folder_driver((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                         exp_desc_l.type_idx,
                         NULL,
                         NULL_IDX,
                         folded_const,
                        &type_idx,
                         IR_LINE_NUM(ir_idx),
                         IR_COL_NUM(ir_idx),
                         1,
                         Cvrt_Opr)) {

         exp_desc->type_idx    = type_idx;
         OPND_FLD((*result_opnd)) = CN_Tbl_Idx;
         OPND_IDX((*result_opnd)) = ntr_const_tbl(exp_desc->type_idx,
                                                  FALSE,
                                                  folded_const);

         OPND_LINE_NUM((*result_opnd)) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM((*result_opnd))  = IR_COL_NUM(ir_idx);
      }
      else {
         ok = FALSE;
      }
   }


   TRACE (Func_Exit, "cvrt_opr_handler", NULL);

   return(ok);

}  /* cvrt_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Paren_Opr.                                   *|
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

static boolean paren_opr_handler(opnd_type		*result_opnd,
				 expr_arg_type		*exp_desc)

{
   expr_arg_type	exp_desc_l;
   int			ir_idx;
   boolean		ok = TRUE;
   opnd_type		opnd;


   TRACE (Func_Entry, "paren_opr_handler" , NULL);

   ir_idx = OPND_IDX((*result_opnd));
   in_call_list = FALSE;
   
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   exp_desc_l.rank = 0;
   ok = expr_sem(&opnd, &exp_desc_l);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);


   exp_desc->has_constructor = exp_desc_l.has_constructor;

   exp_desc->has_symbolic     = exp_desc_l.has_symbolic;
   exp_desc->constant         = exp_desc_l.constant;
   exp_desc->foldable         = exp_desc_l.foldable;
   exp_desc->will_fold_later  = exp_desc_l.will_fold_later;
   exp_desc->rank             = exp_desc_l.rank;
   exp_desc->type             = exp_desc_l.type;
   exp_desc->type_idx         = exp_desc_l.type_idx;
   exp_desc->linear_type      = exp_desc_l.linear_type;
   exp_desc->vector_subscript = exp_desc_l.vector_subscript;

   COPY_SHAPE(exp_desc->shape,exp_desc_l.shape,exp_desc_l.rank);
   COPY_OPND(exp_desc->char_len, exp_desc_l.char_len);

   if (exp_desc_l.constant) {
      /* remove the paren_opr */
      COPY_OPND((*result_opnd), opnd);
      /* could free up paren_opr ir */
   }
   else if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
            IR_OPR(IR_IDX_L(ir_idx)) == Concat_Opr) {
      /* remove the paren_opr */
      COPY_OPND((*result_opnd), opnd);
      /* could free up paren_opr ir */
   }
   else if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
            IR_OPR(IR_IDX_L(ir_idx)) == Paren_Opr) {

      /* remove redundant () */

      COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IR_IDX_L(ir_idx)));
      IR_RANK(ir_idx)          = exp_desc_l.rank;

      if (IR_RANK(ir_idx)) {
         IR_ARRAY_SYNTAX(ir_idx) = TRUE;
      }

      IR_TYPE_IDX(ir_idx) = exp_desc->type_idx;
   }
   else {
      IR_RANK(ir_idx)          = exp_desc_l.rank;

      if (IR_RANK(ir_idx)) {
         IR_ARRAY_SYNTAX(ir_idx) = TRUE;
      }

      IR_TYPE_IDX(ir_idx) = exp_desc->type_idx;
   }


   TRACE (Func_Exit, "paren_opr_handler", NULL);

   return(ok);

}  /* paren_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	semantic handler for the Stmt_Func_Call_Opr.                          *|
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

static boolean stmt_func_call_opr_handler(opnd_type	*result_opnd,
				          expr_arg_type	*exp_desc)

{
   int			asg_idx;
   int			col;
   int			dummy_idx;
   expr_arg_type	exp_desc_l;
   expr_arg_type	exp_desc_r;
   int			i;
   int			ir_idx;
   int			line;
   int			list_idx;
   int			loc_info_idx;
   char			l_err_word[40];
   opnd_type		l_opnd;
   boolean		ok = TRUE;
   opnd_type		opnd;
   int			opnd_col;
   int			opnd_line;
   int			paren_idx;
   char			r_err_word[40];
   int			save_arg_info_list_base;
   expr_mode_type	save_expr_mode;
   boolean		save_defer_stmt_expansion;
   int			save_number_of_functions;
   boolean              save_tree_has_ranf;
   boolean		save_io_item_must_flatten;
   boolean              save_check_type_conversion;
   int                  save_target_type_idx;
   int                  save_target_char_len_idx;
   int			sn_idx;
   int			stmt_func_idx;
   opnd_type		stmt_func_opnd;
   int			tmp_idx;
   int			type_idx;


   TRACE (Func_Entry, "stmt_func_call_opr_handler" , NULL);

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   ir_idx = OPND_IDX((*result_opnd));
   line   = IR_LINE_NUM(ir_idx);
   col    = IR_COL_NUM(ir_idx);
   save_io_item_must_flatten = io_item_must_flatten;

   /* BRIANJ - save_tree_has_ranf is never used */

   save_tree_has_ranf = tree_has_ranf;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   ok = expr_sem(&opnd, exp_desc);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);
   stmt_func_idx = IR_IDX_L(ir_idx);

   if (! ATS_SF_SEMANTICS_DONE(stmt_func_idx)) {
      ok = stmt_func_semantics(stmt_func_idx) && ok;
   }

   if (AT_DCL_ERR(stmt_func_idx)) {
      /* previous error, nothing to say, just split */
      ok = FALSE;
      goto EXIT;
   }

   if (ATS_SF_ACTIVE(stmt_func_idx)) {

      /* error , recursive use */

      find_opnd_line_and_column(&opnd,
                                &opnd_line,
                                &opnd_col);
      PRINTMSG(opnd_line, 753, Error, opnd_col,
               AT_OBJ_NAME_PTR(stmt_func_idx));
      ok = FALSE;
      AT_DCL_ERR(stmt_func_idx) = TRUE;
      goto EXIT;
   }

   if (ATP_NUM_DARGS(stmt_func_idx) != IR_LIST_CNT_R(ir_idx)) {

      find_opnd_line_and_column((opnd_type *) &IR_OPND_L(ir_idx),
                                &opnd_line,
                                &opnd_col);
      PRINTMSG(opnd_line, 754, Error, opnd_col,
               AT_OBJ_NAME_PTR(stmt_func_idx));
      ok = FALSE;
      goto EXIT;
   }

   /* do memory management stuff to make sure the tables */
   /* are big enough                                     */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base +
                                           IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }
   loc_info_idx = arg_info_list_base;

   /* hook up the actual args with the dummy args */

   list_idx = IR_IDX_R(ir_idx);
   sn_idx   = ATP_FIRST_IDX(stmt_func_idx);

   for (i = loc_info_idx + 1;
        i <= loc_info_idx + IR_LIST_CNT_R(ir_idx);
        i++) {

      dummy_idx = SN_ATTR_IDX(sn_idx);

      save_number_of_functions = number_of_functions;
      tree_has_ranf = FALSE;
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc_r.rank = 0;
      ok = expr_sem(&opnd, &exp_desc_r) && ok;
      COPY_OPND(IL_OPND(list_idx), opnd);

      exp_desc_r.tree_has_ranf = tree_has_ranf;
      arg_info_list[i]       = init_arg_info;
      arg_info_list[i].ed    = exp_desc_r;

      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
      IL_ARG_DESC_IDX(list_idx) = i;

      /* check type, kind type, and rank */

      type_idx = ATD_TYPE_IDX(dummy_idx);

      find_opnd_line_and_column(&opnd, &opnd_line, &opnd_col);

      if (OPND_FLD(opnd)               == AT_Tbl_Idx &&
          AT_OBJ_CLASS(OPND_IDX(opnd)) != Data_Obj   &&
          fnd_semantic_err(Obj_Sf_Actual_Arg,
                           opnd_line,
                           opnd_col,
                           OPND_IDX(opnd),
                           TRUE))          {

         ok = FALSE;
      }
      else {

         if (exp_desc_r.rank > 0) {
            PRINTMSG(opnd_line, 750, Error, opnd_col,
                     i - loc_info_idx,
                     AT_OBJ_NAME_PTR(stmt_func_idx));
            ok = FALSE;
         }

         if (exp_desc_r.linear_type == Typeless_4 ||
             exp_desc_r.linear_type == Typeless_8 ||
             exp_desc_r.linear_type == Short_Typeless_Const) {

            if (ASG_TYPE(TYP_LINEAR(type_idx),
                         exp_desc_r.linear_type) == Err_Res) {
               r_err_word[0] = '\0';
               l_err_word[0] = '\0';

               strcat(r_err_word,
                      get_basic_type_str(exp_desc_r.type_idx));
               strcat(l_err_word, get_basic_type_str(type_idx));

               PRINTMSG(opnd_line, 751, Error, opnd_col,
                        r_err_word,
                        AT_OBJ_NAME_PTR(dummy_idx),
                        l_err_word);
               ok = FALSE;
            }
            else if (exp_desc_r.linear_type == Short_Typeless_Const) {
               OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                       type_idx,
                                                       opnd_line,
                                                       opnd_col);
               exp_desc_r.linear_type = TYP_LINEAR(type_idx);
               exp_desc_r.type_idx    = type_idx;
               exp_desc_r.type        = TYP_TYPE(type_idx);

               arg_info_list[i].ed    = exp_desc_r;
            }
         }
         else if (exp_desc_r.type != TYP_TYPE(type_idx) ||
                  (exp_desc_r.type == Structure &&
                   !compare_derived_types(exp_desc_r.type_idx,
                                          type_idx))) {

            r_err_word[0] = '\0';
            l_err_word[0] = '\0';

            strcat(r_err_word,
                   get_basic_type_str(exp_desc_r.type_idx));
            strcat(l_err_word, get_basic_type_str(type_idx));

            PRINTMSG(opnd_line, 751, Error, opnd_col,
                     r_err_word,
                     AT_OBJ_NAME_PTR(dummy_idx),
                     l_err_word);

            ok = FALSE;
         }
         else if (exp_desc_r.type != Structure &&
                  exp_desc_r.type != Character &&
                  exp_desc_r.linear_type != TYP_LINEAR(type_idx)) {

            PRINTMSG(opnd_line, 752, Error, opnd_col,
                     i - loc_info_idx,
                     AT_OBJ_NAME_PTR(stmt_func_idx));
            ok = FALSE;
         }
      }

      IL_HAS_FUNCTIONS(list_idx) = FALSE;

      if (number_of_functions > save_number_of_functions) {
         IL_HAS_FUNCTIONS(list_idx) = TRUE;
      }

      if (tree_has_ranf ||
          (exp_desc_r.type == Character &&
           TYP_TYPE(type_idx) == Character)) {

         ok &= validate_char_len(&opnd, &exp_desc_r);
         arg_info_list[i].ed    = exp_desc_r;

         if (TYP_TYPE(type_idx) == Character &&
             exp_desc_r.char_len.fld == CN_Tbl_Idx &&
             TYP_FLD(type_idx) == CN_Tbl_Idx &&
             fold_relationals(exp_desc_r.char_len.idx,
                              TYP_IDX(type_idx),
                              Lt_Opr)) {

            if (IL_FLD(list_idx) == CN_Tbl_Idx) {
               PRINTMSG(opnd_line, 1305, Caution, opnd_col);
               PRINTMSG(opnd_line, 1306, Ansi, opnd_col);
               cast_to_type_idx(&opnd, &exp_desc_r, type_idx);
               arg_info_list[i].ed = exp_desc_r;
               COPY_OPND(IL_OPND(list_idx), opnd);
            }
            else {
               /* error .. actual len is less than dummy len */

               PRINTMSG(opnd_line, 848, Error, opnd_col,
                        AT_OBJ_NAME_PTR(dummy_idx));
               ok = FALSE;
            }
         }

         if (! ok) {
            /* intentionally blank */
         }
         else if (TYP_TYPE(type_idx) == Character &&
                  exp_desc_r.type == Character &&
                  TYP_FLD(type_idx) == CN_Tbl_Idx &&
                  OPND_FLD(opnd) == CN_Tbl_Idx) {

            save_check_type_conversion = check_type_conversion;
            save_target_type_idx = target_type_idx;
            save_target_char_len_idx = target_char_len_idx;

            check_type_conversion = TRUE;
            target_type_idx = Character_1;

            target_char_len_idx = TYP_IDX(type_idx);
            fold_aggragate_expression(&opnd, &exp_desc_r, TRUE);
            COPY_OPND(IL_OPND(list_idx), opnd);

            check_type_conversion = save_check_type_conversion;
            target_type_idx = save_target_type_idx;
            target_char_len_idx = save_target_char_len_idx;

            arg_info_list[i].arg_opnd.fld = OPND_FLD(opnd);
            arg_info_list[i].arg_opnd.idx = OPND_IDX(opnd);
            arg_info_list[i].ed    = exp_desc_r;
         }
         else if (no_func_expansion) {
            arg_info_list[i].arg_opnd.fld = OPND_FLD(opnd);
            arg_info_list[i].arg_opnd.idx = OPND_IDX(opnd);
         }
         else if (tree_has_ranf ||
                  TYP_TYPE(type_idx) == Character) {

            arg_info_list[i].ed.type_idx       = type_idx;
            arg_info_list[i].ed.type           = TYP_TYPE(type_idx);
            arg_info_list[i].ed.linear_type    = TYP_LINEAR(type_idx);
            arg_info_list[i].ed.constant       = FALSE;
            arg_info_list[i].ed.foldable       = FALSE;
            arg_info_list[i].ed.will_fold_later = FALSE;

            if (TYP_TYPE(type_idx) == Character) {
               arg_info_list[i].ed.char_len.fld = TYP_FLD(type_idx);
               arg_info_list[i].ed.char_len.idx = TYP_IDX(type_idx);
               OPND_LINE_NUM(arg_info_list[i].ed.char_len) = line;
               OPND_COL_NUM(arg_info_list[i].ed.char_len) = col;
            }

            tmp_idx = create_tmp_asg(&opnd,
                                     &arg_info_list[i].ed,
                                     &l_opnd,
                                     Intent_In,
                                     FALSE,
                                     FALSE);

            arg_info_list[i].arg_opnd.fld      = AT_Tbl_Idx;
            arg_info_list[i].arg_opnd.idx      = tmp_idx;

            COPY_OPND(opnd, l_opnd);
         }
      }
      else {
         arg_info_list[i].arg_opnd.fld = OPND_FLD(opnd);
         arg_info_list[i].arg_opnd.idx = OPND_IDX(opnd);
      }

      /* put a paren opr over any expression */
      /* so that pdgcs can't mangle it.      */

      if (! no_func_expansion &&
          arg_info_list[i].arg_opnd.fld == IR_Tbl_Idx &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Whole_Subscript_Opr &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Section_Subscript_Opr &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Subscript_Opr &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Substring_Opr &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Whole_Substring_Opr &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Dv_Deref_Opr &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Struct_Opr &&
          IR_OPR(arg_info_list[i].arg_opnd.idx) != Paren_Opr) {

         NTR_IR_TBL(paren_idx);
         IR_OPR(paren_idx) = Paren_Opr;
         IR_TYPE_IDX(paren_idx) = arg_info_list[i].ed.type_idx;
         IR_LINE_NUM(paren_idx) = opnd_line;
         IR_COL_NUM(paren_idx)  = opnd_col;
         COPY_OPND(IR_OPND_L(paren_idx), arg_info_list[i].arg_opnd);
         arg_info_list[i].arg_opnd.fld = IR_Tbl_Idx;
         arg_info_list[i].arg_opnd.idx = paren_idx;
      }

      sn_idx++;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   /* now hook up all the info on the dummy arg attrs. */
   /* can't do until here because of recursive uses.   */

   sn_idx   = ATP_FIRST_IDX(stmt_func_idx);

   for (i = loc_info_idx + 1;
        i <= loc_info_idx + IR_LIST_CNT_R(ir_idx);
        i++) {

      dummy_idx = SN_ATTR_IDX(sn_idx);
      ATD_SF_LINK(dummy_idx) = i;

      ATD_FLD(dummy_idx) = arg_info_list[i].arg_opnd.fld;
      ATD_SF_ARG_IDX(dummy_idx) = arg_info_list[i].arg_opnd.idx;

      sn_idx++;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (! ok) {
      goto EXIT;
   }

   OPND_LINE_NUM(stmt_func_opnd)= line;
   OPND_COL_NUM(stmt_func_opnd)	= col;
   OPND_FLD(stmt_func_opnd)	= (fld_type) ATS_SF_FLD(stmt_func_idx);
   OPND_IDX(stmt_func_opnd)	= ATS_SF_IDX(stmt_func_idx);
   copy_subtree(&stmt_func_opnd, &stmt_func_opnd);

   /* set the stmt func active flag on stmt_func_idx */

   ATS_SF_ACTIVE(stmt_func_idx) = TRUE;

   save_expr_mode = expr_mode;
   expr_mode      = Stmt_Func_Expr;

   exp_desc_l.rank = 0;
   ok = expr_sem(&stmt_func_opnd, &exp_desc_l)
                          && ok;

   expr_mode = save_expr_mode;

   exp_desc->has_symbolic = exp_desc_l.has_symbolic;
   exp_desc->has_constructor = exp_desc_l.has_constructor;
   exp_desc->constant = exp_desc_l.constant;
   exp_desc->foldable = exp_desc_l.foldable;
   exp_desc->will_fold_later = exp_desc_l.will_fold_later;

   type_idx = ATD_TYPE_IDX(stmt_func_idx);

   exp_desc->type_idx       = ATD_TYPE_IDX(stmt_func_idx);
   exp_desc->type           = TYP_TYPE(exp_desc->type_idx);
   exp_desc->linear_type    = TYP_LINEAR(exp_desc->type_idx);

   if (exp_desc->type == Character) {
      exp_desc->char_len.fld = TYP_FLD(exp_desc->type_idx);
      exp_desc->char_len.idx = TYP_IDX(exp_desc->type_idx);
      OPND_LINE_NUM(exp_desc->char_len) = line;
      OPND_COL_NUM(exp_desc->char_len) = col;
   }

   if (ok           &&
       ASG_EXTN(exp_desc->linear_type, exp_desc_l.linear_type) &&
       (exp_desc_l.type == Character ||
        exp_desc_l.linear_type == Short_Typeless_Const))  {
      find_opnd_line_and_column(&stmt_func_opnd,
                                &opnd_line,
                                &opnd_col);
      if (exp_desc_l.type == Character) {
         PRINTMSG(opnd_line, 161, Ansi, opnd_col);
      }


      OPND_IDX(stmt_func_opnd) = 
                         cast_typeless_constant(OPND_IDX(stmt_func_opnd),
                                                type_idx,
                                                opnd_line,
                                                opnd_col);

      exp_desc_l.type_idx    = type_idx;
      exp_desc_l.type        = TYP_TYPE(type_idx);
      exp_desc_l.linear_type = TYP_LINEAR(type_idx);
   }
   else if (ok                    &&
            TYP_TYPE(type_idx) != Character         &&
            TYP_TYPE(type_idx) != Structure         &&
            TYP_LINEAR(type_idx) != exp_desc_l.linear_type) {

      cast_to_type_idx(&stmt_func_opnd,
                        &exp_desc_l,
                        exp_desc->type_idx);
   }

   if (! ok) {
      /* intentionally blank */
   }
   else if (TYP_TYPE(type_idx) == Character &&
            exp_desc_l.type == Character &&
            TYP_FLD(type_idx) == CN_Tbl_Idx &&
            TYP_FLD(exp_desc_l.type_idx) == CN_Tbl_Idx &&
            fold_relationals(TYP_IDX(type_idx),
                             TYP_IDX(exp_desc_l.type_idx),
                             Eq_Opr)) {

      /* intentionally blank */
   }
   else if (TYP_TYPE(type_idx) == Character &&
            exp_desc_l.type == Character &&
            TYP_FLD(type_idx) == CN_Tbl_Idx &&
            OPND_FLD(stmt_func_opnd) == CN_Tbl_Idx) {

         save_check_type_conversion = check_type_conversion;
         save_target_type_idx = target_type_idx;
         save_target_char_len_idx = target_char_len_idx;

         check_type_conversion = TRUE;
         target_type_idx = Character_1;
         target_char_len_idx = TYP_IDX(type_idx);
         fold_aggragate_expression(&stmt_func_opnd, &exp_desc_l, TRUE);

         check_type_conversion = save_check_type_conversion;
         target_type_idx = save_target_type_idx;
         target_char_len_idx = save_target_char_len_idx;
   }
   else if (! no_func_expansion          &&
            TYP_TYPE(type_idx) == Character) {

      /* pull stmt func into tmp to handle padding or trunc */

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,  /* Semantics done */
                           line,
                           col,
                           type_idx,
                           Priv);

      COPY_OPND(IR_OPND_R(asg_idx), stmt_func_opnd);
      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_IDX(opnd) = tmp_idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;
      ok = gen_whole_substring(&opnd, 0);
      COPY_OPND(IR_OPND_L(asg_idx), opnd);

      /* This is no longer a foldable operand. */
      exp_desc->constant = FALSE;
      exp_desc->foldable = FALSE;
      exp_desc->will_fold_later = FALSE;


      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      COPY_OPND(stmt_func_opnd, IR_OPND_L(asg_idx));
   }


   COPY_OPND((*result_opnd), stmt_func_opnd);

   if (OPND_FLD((*result_opnd)) != CN_Tbl_Idx &&
       ! exp_desc->reference &&
       ! exp_desc->tmp_reference) {

      /* put a paren opr over the statement function so pdgcs doesn't */
      /* reassociate anything in the expanded tree. */

      NTR_IR_TBL(paren_idx);
      IR_OPR(paren_idx) = Paren_Opr;
      IR_TYPE_IDX(paren_idx) = exp_desc->type_idx;
      IR_LINE_NUM(paren_idx) = line;
      IR_COL_NUM(paren_idx)  = col;

      COPY_OPND(IR_OPND_L(paren_idx), (*result_opnd));
      OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*result_opnd)) = paren_idx;
   }

   /* clear the stmt func active flag on stmt_func_idx */

   ATS_SF_ACTIVE(stmt_func_idx) = FALSE;

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   if (TYP_TYPE(type_idx) == Character) {
      io_item_must_flatten = save_io_item_must_flatten;
   }

EXIT:

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(result_opnd);

   TRACE (Func_Exit, "stmt_func_call_opr_handler", NULL);

   return(ok);

}  /* stmt_func_call_opr_handler */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	If possible, check substring bounds.                                  *|
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

boolean	check_substring_bounds(int	ir_idx)

{
   int		base_attr;
   int		col;
   int		line;
   boolean	ok = TRUE;
   int		type_idx;

   TRACE (Func_Entry, "check_substring_bounds", NULL);

   if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
      type_idx = CN_TYPE_IDX(IR_IDX_L(ir_idx));
   }
   else {
      base_attr = find_base_attr(&(IR_OPND_L(ir_idx)), &line, &col);
      type_idx  = ATD_TYPE_IDX(base_attr);
   }

   if (IL_FLD(IR_IDX_R(ir_idx)) == CN_Tbl_Idx                          &&
       IL_FLD(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))) == CN_Tbl_Idx        &&
       fold_relationals(IL_IDX(IR_IDX_R(ir_idx)),
                        IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))),
                        Le_Opr)                                        &&
       TYP_FLD(type_idx) == CN_Tbl_Idx) {

      /* check range */

      if (compare_cn_and_value(IL_IDX(IR_IDX_R(ir_idx)), 1, Lt_Opr)) {

         /* out of range, below */

         find_opnd_line_and_column((opnd_type *)
                                   &IL_OPND(IR_IDX_R(ir_idx)),
                                   &line,
                                   &col);
         PRINTMSG(line, 781, Error, col);
         ok = FALSE;
      }
      else if (fold_relationals(IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))),
                                TYP_IDX(type_idx),
                                Gt_Opr)) {

         /* out of range, above */

         find_opnd_line_and_column((opnd_type *)&IL_OPND(IL_NEXT_LIST_IDX(
                                                        IR_IDX_R(ir_idx))),
                                   &line,
                                   &col);
         PRINTMSG(line, 781, Error, col);
         ok = FALSE;
      }
   }

   TRACE (Func_Exit, "check_substring_bounds", NULL);

   return(ok);

}  /* check_substring_bounds */

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

boolean check_array_bounds(int		ir_idx)

{
   int			base_attr;
   int			bd_idx;
   boolean		check_ub = TRUE;
   opnd_type		cond_opnd;
   opnd_type		end_opnd;
   opnd_type		inc_opnd;
   opnd_type		lb_opnd;
   int			line;
   int			list_idx;
   int			col;
   int			i;
   boolean		ok = TRUE;
   opnd_type		start_opnd;
   opnd_type		ub_opnd;


   TRACE (Func_Entry, "check_array_bounds", NULL);

   if (! needs_bounds_check(ir_idx)) {
      goto EXIT;
   }

   base_attr = find_base_attr(&(IR_OPND_L(ir_idx)), &line, &col);

   bd_idx = ATD_ARRAY_IDX(base_attr);

   if (BD_ARRAY_CLASS(bd_idx) != Explicit_Shape &&
       (BD_ARRAY_CLASS(bd_idx) != Assumed_Size ||
        BD_RANK(bd_idx) == 1)) {

      goto EXIT;
   }

   list_idx = IR_IDX_R(ir_idx);
   i = 1;

   while (list_idx != NULL_IDX) {

      if (IL_PE_SUBSCRIPT(list_idx) &&
          ATD_PE_ARRAY_IDX(base_attr) != NULL_IDX &&
          bd_idx != ATD_PE_ARRAY_IDX(base_attr)) {

         bd_idx = ATD_PE_ARRAY_IDX(base_attr);
         i = 1;

         check_ub = TRUE;

         if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size &&
             i == BD_RANK(bd_idx)) {
            check_ub = FALSE;
         }
      }

      if (IL_FLD(list_idx) == CN_Tbl_Idx) {
         if (BD_LB_FLD(bd_idx, i) == CN_Tbl_Idx &&
            fold_relationals(IL_IDX(list_idx), BD_LB_IDX(bd_idx, i), Lt_Opr)) {

            find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx),
                                      &line,
                                      &col);
            PRINTMSG(line, 1197, Error, col, i);
            ok = FALSE;
         }
         else if (BD_UB_FLD(bd_idx, i) == CN_Tbl_Idx &&
                  check_ub &&
            fold_relationals(IL_IDX(list_idx), BD_UB_IDX(bd_idx, i), Gt_Opr)) {

            find_opnd_line_and_column((opnd_type *)&IL_OPND(list_idx),
                                      &line,
                                      &col);
            PRINTMSG(line, 1197, Error, col, i);
            ok = FALSE;
         }
      }
      else if (IL_FLD(list_idx) == IR_Tbl_Idx &&
               check_ub &&
               IR_OPR(IL_IDX(list_idx)) == Triplet_Opr &&
               IL_FLD(IR_IDX_L(IL_IDX(list_idx))) == CN_Tbl_Idx &&
               IL_FLD(IL_NEXT_LIST_IDX(IR_IDX_L(IL_IDX(list_idx)))) 
                                                       == CN_Tbl_Idx &&
               IL_FLD(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                           IR_IDX_L(IL_IDX(list_idx))))) == CN_Tbl_Idx &&
               BD_LB_FLD(bd_idx, i) == CN_Tbl_Idx &&
               BD_UB_FLD(bd_idx, i) == CN_Tbl_Idx) {

         COPY_OPND(start_opnd, 
                   IL_OPND(IR_IDX_L(IL_IDX(list_idx))));

         COPY_OPND(end_opnd, 
                   IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_L(IL_IDX(list_idx)))));

         COPY_OPND(inc_opnd, 
                   IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_L(
                                  IL_IDX(list_idx))))));

         gen_opnd(&lb_opnd, 
                  BD_LB_IDX(bd_idx, i), 
                  BD_LB_FLD(bd_idx, i), 
                  line, 
                  col);

         gen_opnd(&ub_opnd, 
                  BD_UB_IDX(bd_idx, i), 
                  BD_UB_FLD(bd_idx, i), 
                  line, 
                  col);

         gen_rbounds_condition(&cond_opnd,
                               &start_opnd,
                               &end_opnd,
                               &inc_opnd,
                               &lb_opnd,
                               &ub_opnd,
                               line,
                               col);

# ifdef _DEBUG
         if (OPND_FLD(cond_opnd) != CN_Tbl_Idx ||
             TYP_TYPE(CN_TYPE_IDX(OPND_IDX(cond_opnd))) != Logical) {
            PRINTMSG(line, 626, Internal, col,
                     "LOGICAL CN_Tbl_Idx", "check_array_bounds");
         }
# endif

         if (THIS_IS_TRUE(&CN_CONST(OPND_IDX(cond_opnd)),
                          CN_TYPE_IDX(OPND_IDX(cond_opnd)))) {

            find_opnd_line_and_column(&start_opnd, &line, &col);
            PRINTMSG(line, 1197, Error, col, i);
            ok = FALSE;
         }
      }

      i++;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size &&
          i == BD_RANK(bd_idx)) {
         check_ub = FALSE;
      }
   }

EXIT:

   TRACE (Func_Exit, "check_array_bounds", NULL);

   return(ok);

}  /* check_array_bounds */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Count the greatest depth of implied do's in an array constructor.     *|
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

static int implied_do_depth(opnd_type	*top_opnd)

{
   int		depth = 0;
   int		i;
   int		ir_idx;
   int		list_idx;
   opnd_type	opnd;

   TRACE (Func_Entry, "implied_do_depth", NULL);

   switch (OPND_FLD((*top_opnd))) {
      case IR_Tbl_Idx:
         ir_idx = OPND_IDX((*top_opnd));
         if (IR_OPR(ir_idx) == Implied_Do_Opr) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            i = implied_do_depth(&opnd);
            depth = i + 1;
         }
         else {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            depth = implied_do_depth(&opnd);

            COPY_OPND(opnd, IR_OPND_R(ir_idx));
            i = implied_do_depth(&opnd);
            if (i > depth)
               depth = i; 
         }
         break;
      case IL_Tbl_Idx:
         list_idx = OPND_IDX((*top_opnd));
         while (list_idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            i = implied_do_depth(&opnd);
            if (i > depth)
               depth = i;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;
   }

   TRACE (Func_Exit, "implied_do_depth", NULL);

   return(depth);

}  /* implied_do_depth */

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

static long64 outer_imp_do_count(opnd_type	*size_opnd)

{
   int			col;
   long64		count = 0;
   int			div_idx;
   opnd_type		end_opnd;
   expr_arg_type	exp_desc;
   opnd_type		inc_opnd;
   int			ir_idx;
   int			line;
   int			list_idx;
   int			minus_idx;
   boolean		ok;
   opnd_type		opnd;
   int			plus_idx;
   cif_usage_code_type	save_xref_state;
   opnd_type		start_opnd;


   TRACE (Func_Entry, "outer_imp_do_count", NULL);

   COPY_OPND(opnd, (*size_opnd));

   while (OPND_FLD(opnd) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX(opnd)) != Implied_Do_Opr) {
      COPY_OPND(opnd, IR_OPND_R(OPND_IDX(opnd)));
   }

   if (OPND_FLD(opnd) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX(opnd)) != Implied_Do_Opr) {

      goto EXIT;
   }

   ir_idx = OPND_IDX(opnd);
   
   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   list_idx = IR_IDX_R(ir_idx);

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(start_opnd, IL_OPND(list_idx));

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(end_opnd, IL_OPND(list_idx));

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(inc_opnd, IL_OPND(list_idx));

   minus_idx = gen_ir(OPND_FLD(end_opnd), OPND_IDX(end_opnd),
                  Minus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                      OPND_FLD(start_opnd),OPND_IDX(start_opnd));

   plus_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Plus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                     OPND_FLD(inc_opnd), OPND_IDX(inc_opnd));

   div_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                Div_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                    OPND_FLD(inc_opnd), OPND_IDX(inc_opnd));

   gen_opnd(&opnd, div_idx, IR_Tbl_Idx, line, col);
 
   exp_desc        = init_exp_desc;
   exp_desc.rank   = 0;
   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   ok              = expr_semantics(&opnd, &exp_desc);
   xref_state      = save_xref_state;

   if (OPND_FLD(opnd) == CN_Tbl_Idx) {
      count = CN_INT_TO_C(OPND_IDX(opnd));
   }

EXIT:

   TRACE (Func_Exit, "outer_imp_do_count", NULL);

   return(count);

}  /* outer_imp_do_count */

# if defined(_F_MINUS_MINUS)
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Top level routine for f-- distant ref translation. This routine calls *|
|*      the appropriate routine depending on whether the object is a dope     *|
|*      vector or not.                                                        *|
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

void translate_distant_ref(opnd_type		*result_opnd, 
                           expr_arg_type	*exp_desc, 
                           int			pe_dim_list_idx)

{
   int		attr_idx;
   boolean	save_defer_stmt_expansion;
   int		sub_idx;

# if defined(_TARGET_OS_MAX)
   int		line;
   int		col;
# endif


   TRACE (Func_Entry, "translate_distant_ref", NULL);

# if defined(_TARGET_OS_MAX)

   if (IL_FLD(pe_dim_list_idx) == IR_Tbl_Idx &&
       IR_OPR(IL_IDX(pe_dim_list_idx)) == My_Pe_Opr) {

      /* nothing to do, intentionally blank */

      return;
   }

   if (storage_bit_size_tbl[exp_desc->linear_type] != 64 &&
       (exp_desc->type != Structure ||
        ! in_component_ref)) {

      find_opnd_line_and_column(result_opnd, &line, &col);
      PRINTMSG(line, 1585, Error, col);
   }

# endif

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   io_item_must_flatten = TRUE;

   sub_idx = OPND_IDX((*result_opnd));
   
   while (IR_FLD_L(sub_idx) != AT_Tbl_Idx) {
      sub_idx = IR_IDX_L(sub_idx);
   }

   attr_idx = IR_IDX_L(sub_idx);

# if defined(_TARGET_OS_MAX)
   translate_t3e_distant_ref(result_opnd, exp_desc, pe_dim_list_idx);
# else
   if (ATD_IM_A_DOPE(attr_idx)) {
      translate_distant_dv_ref(result_opnd, exp_desc, pe_dim_list_idx);
   }
   else if (dump_flags.fmm1) {
      translate_distant_ref1(result_opnd, exp_desc, pe_dim_list_idx);
   }
   else {
      translate_distant_ref2(result_opnd, exp_desc, pe_dim_list_idx);
   }
# endif

   exp_desc->pe_dim_ref = TRUE;

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(result_opnd);

   TRACE (Func_Exit, "translate_distant_ref", NULL);

   return;

}  /* translate_distant_ref */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Translate pe dimension dope vector reference. A temp dope vector is   *|
|*      created with the adjusted address.                                    *|
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

static void translate_distant_dv_ref(opnd_type            *result_opnd,
                                     expr_arg_type        *exp_desc,
                                     int                  pe_dim_list_idx)

{
   int                  bd_idx;
   int                  col;
   int			deref_idx;
   int			dv_idx;
   int                  i;
   int			ir_idx;
   int			ir_idx2;
   int                  line;
   int                  list_idx;
   opnd_type            opnd;
   int                  plus_idx;
   int                  sub_idx = NULL_IDX;
   int			tmp_dv_idx;

   TRACE (Func_Entry, "translate_distant_dv_ref", NULL);

   find_opnd_line_and_column(result_opnd, &line, &col);

   deref_idx = OPND_IDX((*result_opnd));

   while (IR_FLD_L(deref_idx) != AT_Tbl_Idx) {
      if (IR_OPR(IR_IDX_L(deref_idx)) == Dv_Deref_Opr) {
         sub_idx = deref_idx;
      }
      deref_idx = IR_IDX_L(deref_idx);
   }

# if defined(_DEBUG)
   if (sub_idx == NULL_IDX) {
      PRINTMSG(line, 626, Internal, col,
               "Subscript_Opr", "translate_distant_dv_ref");
   }
# endif

   dv_idx = IR_IDX_L(deref_idx);

   if (ATD_ARRAY_IDX(dv_idx) != NULL_IDX) {
      list_idx = IR_IDX_R(sub_idx);

      for (i = 1; i < BD_RANK(ATD_ARRAY_IDX(dv_idx)); i++) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
      IR_LIST_CNT_R(sub_idx) = BD_RANK(ATD_ARRAY_IDX(dv_idx));
   }

   tmp_dv_idx = gen_compiler_tmp(line, col, Priv, TRUE);
   ATD_TYPE_IDX(tmp_dv_idx)      = ATD_TYPE_IDX(dv_idx);
   ATD_STOR_BLK_IDX(tmp_dv_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
   AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
   ATD_ARRAY_IDX(tmp_dv_idx)     = ATD_ARRAY_IDX(dv_idx);
   ATD_POINTER(tmp_dv_idx)       = ATD_POINTER(dv_idx);
   ATD_IM_A_DOPE(tmp_dv_idx)     = TRUE;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Whole_Copy_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = tmp_dv_idx;
   IR_LINE_NUM_L(ir_idx) = line;
   IR_COL_NUM_L(ir_idx)  = col;

   IR_FLD_R(ir_idx) = AT_Tbl_Idx;
   IR_IDX_R(ir_idx) = dv_idx;
   IR_LINE_NUM_R(ir_idx) = line;
   IR_COL_NUM_R(ir_idx)  = col;

   gen_sh(Before, Assignment_Stmt, SH_GLB_LINE(curr_stmt_sh_idx),
          SH_COL_NUM(curr_stmt_sh_idx), FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   bd_idx = ATD_PE_ARRAY_IDX(dv_idx);

   linearize_pe_dims(pe_dim_list_idx,
                     bd_idx,
                     line,
                     col,
                    &opnd);

   /* generate reference to bias component of pe_offset_attr */

   gen_bias_ref(&opnd);

   COPY_OPND((exp_desc->bias_opnd), opnd);

   /* increment the base address by the bias_opnd */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Set_Base_Addr;
   IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = tmp_dv_idx;
   IR_LINE_NUM_L(ir_idx) = line;
   IR_COL_NUM_L(ir_idx)  = col;

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx)  = col;

   IR_FLD_R(ir_idx) = IR_Tbl_Idx;
   IR_IDX_R(ir_idx) = plus_idx;

   COPY_OPND(IR_OPND_R(plus_idx), opnd);

   NTR_IR_TBL(ir_idx2);
   IR_OPR(ir_idx2) = Dv_Access_Base_Addr;
   IR_TYPE_IDX(ir_idx2) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx2) = line;
   IR_COL_NUM(ir_idx2)  = col;

   IR_FLD_L(ir_idx2) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx2) = tmp_dv_idx;
   IR_LINE_NUM_L(ir_idx2) = line;
   IR_COL_NUM_L(ir_idx2)  = col;

   IR_FLD_L(plus_idx) = IR_Tbl_Idx;
   IR_IDX_L(plus_idx) = ir_idx2;

   gen_sh(Before, Assignment_Stmt, SH_GLB_LINE(curr_stmt_sh_idx),
          SH_COL_NUM(curr_stmt_sh_idx), FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


   /* now replace the original dv_idx with the tmp_dv_idx */

   IR_IDX_L(deref_idx) = tmp_dv_idx;

   TRACE (Func_Exit, "translate_distant_dv_ref", NULL);

   return;

}  /* translate_distant_dv_ref */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Linearize the pe dimensions so that there is one pe dimension.        *|
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
# if defined(_TARGET_OS_MAX)

static void translate_t3e_distant_ref(opnd_type            *result_opnd,
                                      expr_arg_type        *exp_desc,
                                      int                  pe_dim_list_idx)

{
   int                  attr_idx;
   int                  bd_idx;
   int                  col;
   int                  i;
   int                  line;
   int                  list_idx;
   expr_arg_type        loc_exp_desc;
   boolean		ok;
   opnd_type            opnd;
   cif_usage_code_type  save_xref_state;
   int                  sub_idx;


   TRACE (Func_Entry, "translate_t3e_distant_ref", NULL);

   find_opnd_line_and_column(result_opnd, &line, &col);

   sub_idx = OPND_IDX((*result_opnd));

   while (IR_FLD_L(sub_idx) != AT_Tbl_Idx &&
          (IR_FLD_L(sub_idx) != IR_Tbl_Idx ||
           IR_OPR(IR_IDX_L(sub_idx)) != Dv_Deref_Opr)) {
      sub_idx = IR_IDX_L(sub_idx);
   }

   if (IR_FLD_L(sub_idx) == AT_Tbl_Idx) {
      attr_idx = IR_IDX_L(sub_idx);
   }
   else {
      attr_idx = IR_IDX_L(IR_IDX_L(sub_idx));
   }

   bd_idx = ATD_PE_ARRAY_IDX(attr_idx);

   linearize_pe_dims(pe_dim_list_idx,
                     bd_idx,
                     line,
                     col,
                    &opnd);

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   loc_exp_desc.rank = 0;
   ok              = expr_semantics(&opnd, &loc_exp_desc);
   xref_state      = save_xref_state;

   COPY_OPND((exp_desc->bias_opnd), opnd);

   if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
      list_idx = IR_IDX_R(sub_idx);

      for (i = 0; i < BD_RANK(ATD_ARRAY_IDX(attr_idx)); i++) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

# ifdef _DEBUG
      if (list_idx != pe_dim_list_idx) {
         PRINTMSG(line, 626, Internal, col,
                  "list_idx != pe_dim_list_idx",
                  "translate_t3e_distant_ref");
      }
# endif

      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
      IR_LIST_CNT_R(sub_idx) = BD_RANK(ATD_ARRAY_IDX(attr_idx)) + 1;
   }
   else {
      IL_NEXT_LIST_IDX(pe_dim_list_idx) = NULL_IDX;
      IR_LIST_CNT_R(sub_idx) = 1;
   }

   COPY_OPND(IL_OPND(pe_dim_list_idx), opnd);

   TRACE (Func_Exit, "translate_t3e_distant_ref", NULL);

   return;

}  /* translate_t3e_distant_ref */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Translate pe dimension references into pointer/pointee pair.          *|
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

static void translate_distant_ref1(opnd_type		*result_opnd, 
                                   expr_arg_type	*exp_desc, 
                                   int			pe_dim_list_idx)

{
   int                  asg_idx;
   int                  attr_idx;
   int                  bd_idx;
   int                  col;
   int                  i;
   int                  line;
   int                  list_idx;
   expr_arg_type        loc_exp_desc;
   int                  loc_idx;
   boolean              ok = TRUE;
   opnd_type            opnd;
   int                  plus_idx;
   int                  ptr_idx;
   int                  ptee_idx;
   int                  save_curr_stmt_sh_idx;
   cif_usage_code_type  save_xref_state;
   int                  sub_idx;

   TRACE (Func_Entry, "translate_distant_ref1", NULL);

   find_opnd_line_and_column(result_opnd, &line, &col);

   sub_idx = OPND_IDX((*result_opnd));

   while (IR_FLD_L(sub_idx) != AT_Tbl_Idx) {
      sub_idx = IR_IDX_L(sub_idx);
   }

   attr_idx = IR_IDX_L(sub_idx);

   if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
      list_idx = IR_IDX_R(sub_idx);

      for (i = 1; i < BD_RANK(ATD_ARRAY_IDX(attr_idx)); i++) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
      IR_LIST_CNT_R(sub_idx) = BD_RANK(ATD_ARRAY_IDX(attr_idx));
   }

   bd_idx = ATD_PE_ARRAY_IDX(attr_idx);

   /* generate the ptr/pointee pair */

   ptr_idx  = gen_compiler_tmp(line, col, Shared, TRUE);
   ATD_TYPE_IDX(ptr_idx) = CRI_Ptr_8;
   AT_SEMANTICS_DONE(ptr_idx) = TRUE;
   ATD_STOR_BLK_IDX(ptr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

   ptee_idx = gen_compiler_tmp(line, col, Shared, TRUE);
   ATD_CLASS(ptee_idx) = CRI__Pointee;
   AT_SEMANTICS_DONE(ptee_idx) = TRUE;
   ATD_STOR_BLK_IDX(ptee_idx) = SCP_SB_BASED_IDX(curr_scp_idx);
   ATD_TYPE_IDX(ptee_idx) = ATD_TYPE_IDX(attr_idx);
   ATD_ARRAY_IDX(ptee_idx) = ATD_ARRAY_IDX(attr_idx);
   ATD_PTR_IDX(ptee_idx) = ptr_idx;

   /* generate assignment to ptr */

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Asg_Opr;
   IR_TYPE_IDX(asg_idx) = CRI_Ptr_8;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx) = col;

   IR_FLD_L(asg_idx) = AT_Tbl_Idx;
   IR_IDX_L(asg_idx) = ptr_idx;
   IR_LINE_NUM_L(asg_idx) = line;
   IR_COL_NUM_L(asg_idx) = col;

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = CRI_Ptr_8;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx) = col;

   IR_FLD_R(asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(asg_idx) = plus_idx;

   NTR_IR_TBL(loc_idx);
   IR_OPR(loc_idx) = Loc_Opr;
   IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
   IR_LINE_NUM(loc_idx) = line;
   IR_COL_NUM(loc_idx) = col;

   /* do I need to worry about a proper reference tree for loc? BHJ */

   IR_FLD_L(loc_idx) = AT_Tbl_Idx;
   IR_IDX_L(loc_idx) = attr_idx;
   IR_LINE_NUM_L(loc_idx) = line;
   IR_COL_NUM_L(loc_idx) = col;

   IR_FLD_L(plus_idx) = IR_Tbl_Idx;
   IR_IDX_L(plus_idx) = loc_idx;

   linearize_pe_dims(pe_dim_list_idx,
                     bd_idx,
                     line,
                     col,
                    &opnd);

   /* generate reference to bias component of pe_offset_attr */

   gen_bias_ref(&opnd);

   COPY_OPND((exp_desc->bias_opnd), opnd);

   COPY_OPND(IR_OPND_R(plus_idx), opnd);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
          FALSE, FALSE, TRUE);

   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   COPY_OPND(opnd, IR_OPND_R(asg_idx));
   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   loc_exp_desc.rank = 0;
   ok             &= expr_semantics(&opnd, &loc_exp_desc);
   xref_state      = save_xref_state;
   COPY_OPND(IR_OPND_R(asg_idx), opnd);

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;


   /* now replace the original attr with the ptee_idx */

   if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
      if (TYP_TYPE(ATD_TYPE_IDX(ptee_idx)) == Structure) {
         loc_exp_desc = init_exp_desc;
         loc_exp_desc.type_idx = ATD_TYPE_IDX(ptee_idx);
         loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
         loc_exp_desc.linear_type = TYP_LINEAR(loc_exp_desc.type_idx);
         loc_exp_desc.rank = 1;
         loc_exp_desc.shape[0].fld = CN_Tbl_Idx;
         loc_exp_desc.shape[0].idx = CN_INTEGER_ONE_IDX;

         ATD_ARRAY_IDX(ptee_idx) = create_bd_ntry_for_const(&loc_exp_desc,
                                                            line,
                                                            col);

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_LIST_CNT_R(sub_idx) = 1;
         IR_IDX_R(sub_idx) = list_idx;
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         IR_IDX_L(sub_idx) = ptee_idx;
      }
      else if (sub_idx == OPND_IDX((*result_opnd))) {
         OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
         OPND_IDX((*result_opnd)) = ptee_idx;
         OPND_LINE_NUM((*result_opnd)) = line;
         OPND_COL_NUM((*result_opnd)) = col;
      }
      else {
         plus_idx = OPND_IDX((*result_opnd));

         while (IR_IDX_L(plus_idx) != sub_idx) {
            plus_idx = IR_IDX_L(plus_idx);
         }

         IR_FLD_L(plus_idx) = AT_Tbl_Idx;
         IR_IDX_L(plus_idx) = ptee_idx;
         IR_LINE_NUM_L(plus_idx) = line;
         IR_COL_NUM_L(plus_idx) = col;
      }
   }
   else {
      IR_IDX_L(sub_idx) = ptee_idx;
   }

   TRACE (Func_Exit, "translate_distant_ref1", NULL);

   return;

}  /* translate_distant_ref1 */

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

static void linearize_pe_dims(int	pe_dim_list_idx,
			      int	bd_idx,
                              int	line,
                              int	col,
			      opnd_type	*result_opnd)

{
   int		i;
   int		list_idx;
   int		minus_idx;
   int		mult_idx;
   int		plus_idx;

   TRACE (Func_Entry, "linearize_pe_dims", NULL);

   list_idx = pe_dim_list_idx;

   NTR_IR_TBL(minus_idx);
   IR_OPR(minus_idx) = Minus_Opr;
   IR_TYPE_IDX(minus_idx) = INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(minus_idx) = line;
   IR_COL_NUM(minus_idx) = col;

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx) = col;

   COPY_OPND(IR_OPND_L(plus_idx), IL_OPND(list_idx));
   IR_FLD_R(plus_idx) = CN_Tbl_Idx;
   IR_IDX_R(plus_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(plus_idx) = line;
   IR_COL_NUM_R(plus_idx) = col;

   IR_FLD_L(minus_idx) = IR_Tbl_Idx;
   IR_IDX_L(minus_idx) = plus_idx;

   IR_FLD_R(minus_idx) = BD_LB_FLD(bd_idx, 1);
   IR_IDX_R(minus_idx) = BD_LB_IDX(bd_idx, 1);
   IR_LINE_NUM_R(minus_idx) = line;
   IR_COL_NUM_R(minus_idx) = col;

   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = minus_idx;

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   for (i = 2; i <= BD_RANK(bd_idx); i++) {
      NTR_IR_TBL(plus_idx);
      IR_OPR(plus_idx) = Plus_Opr;
      IR_TYPE_IDX(plus_idx) = INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(plus_idx) = line;
      IR_COL_NUM(plus_idx) = col;

      COPY_OPND(IR_OPND_L(plus_idx), (*result_opnd));
      OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*result_opnd)) = plus_idx;

      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx) = col;

      NTR_IR_TBL(minus_idx);
      IR_OPR(minus_idx) = Minus_Opr;
      IR_TYPE_IDX(minus_idx) = INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(minus_idx) = line;
      IR_COL_NUM(minus_idx) = col;

      COPY_OPND(IR_OPND_L(minus_idx), IL_OPND(list_idx));

      IR_FLD_R(minus_idx) = BD_LB_FLD(bd_idx, i);
      IR_IDX_R(minus_idx) = BD_LB_IDX(bd_idx, i);
      IR_LINE_NUM_R(minus_idx) = line;
      IR_COL_NUM_R(minus_idx) = col;

      IR_FLD_L(mult_idx) = IR_Tbl_Idx;
      IR_IDX_L(mult_idx) = minus_idx;

      IR_FLD_R(mult_idx) = BD_SM_FLD(bd_idx, i);
      IR_IDX_R(mult_idx) = BD_SM_IDX(bd_idx, i);
      IR_LINE_NUM_R(mult_idx) = line;
      IR_COL_NUM_R(mult_idx) = col;

      IR_FLD_R(plus_idx) = IR_Tbl_Idx;
      IR_IDX_R(plus_idx) = mult_idx;

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }


   TRACE (Func_Exit, "linearize_pe_dims", NULL);

   return;

}  /* linearize_pe_dims */

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

void translate_dv_component(opnd_type		*result_opnd,
			    expr_arg_type	*exp_desc)

{
   int		col;
   int		dv_idx;
   int		ir_idx;
   int		ir_idx2;
   int		line;
   opnd_type	opnd;
   int		plus_idx;
   boolean      save_defer_stmt_expansion;
   int		tmp_dv_idx;
   int		unused;


   TRACE (Func_Entry, "translate_dv_component", NULL);

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   io_item_must_flatten = TRUE;

   find_opnd_line_and_column(result_opnd, &line, &col);

   COPY_OPND(opnd, (*result_opnd));

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   unused = find_left_attr(&opnd);

# ifdef _DEBUG
   if (OPND_FLD(opnd) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX(opnd)) != Struct_Opr) {
      PRINTMSG(line, 626, Internal, col,
               "Struct_Opr", "translate_dv_component");
   }
# endif

   dv_idx = IR_IDX_R(OPND_IDX(opnd));

   tmp_dv_idx = gen_compiler_tmp(line, col, Priv, TRUE);
   ATD_TYPE_IDX(tmp_dv_idx)      = ATD_TYPE_IDX(dv_idx);
   ATD_STOR_BLK_IDX(tmp_dv_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
   AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
   ATD_ARRAY_IDX(tmp_dv_idx)     = ATD_ARRAY_IDX(dv_idx);
   ATD_POINTER(tmp_dv_idx)       = ATD_POINTER(dv_idx);
   ATD_IM_A_DOPE(tmp_dv_idx)     = TRUE;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Whole_Copy_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = tmp_dv_idx;
   IR_LINE_NUM_L(ir_idx) = line;
   IR_COL_NUM_L(ir_idx)  = col;

   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   gen_sh(Before, Assignment_Stmt, SH_GLB_LINE(curr_stmt_sh_idx),
          SH_COL_NUM(curr_stmt_sh_idx), FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /* increment the base address by the bias_opnd */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Set_Base_Addr;
   IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = tmp_dv_idx;
   IR_LINE_NUM_L(ir_idx) = line;
   IR_COL_NUM_L(ir_idx)  = col;

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx)  = col;

   IR_FLD_R(ir_idx) = IR_Tbl_Idx;
   IR_IDX_R(ir_idx) = plus_idx;

   copy_subtree(&(exp_desc->bias_opnd), &opnd);

   COPY_OPND(IR_OPND_R(plus_idx), opnd);

   NTR_IR_TBL(ir_idx2);
   IR_OPR(ir_idx2) = Dv_Access_Base_Addr;
   IR_TYPE_IDX(ir_idx2) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx2) = line;
   IR_COL_NUM(ir_idx2)  = col;

   IR_FLD_L(ir_idx2) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx2) = tmp_dv_idx;
   IR_LINE_NUM_L(ir_idx2) = line;
   IR_COL_NUM_L(ir_idx2)  = col;

   IR_FLD_L(plus_idx) = IR_Tbl_Idx;
   IR_IDX_L(plus_idx) = ir_idx2;

   gen_sh(Before, Assignment_Stmt, SH_GLB_LINE(curr_stmt_sh_idx),
          SH_COL_NUM(curr_stmt_sh_idx), FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX((*result_opnd))) == Dv_Deref_Opr) {

      OPND_FLD(IR_OPND_L(OPND_IDX((*result_opnd)))) = AT_Tbl_Idx;
      OPND_IDX(IR_OPND_L(OPND_IDX((*result_opnd)))) = tmp_dv_idx;
   }
   else {
      OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
      OPND_IDX((*result_opnd)) = tmp_dv_idx;
   }

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(result_opnd);

   TRACE (Func_Exit, "translate_dv_component", NULL);

   return;

}  /* translate_dv_component */

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

# ifdef _TARGET_OS_MAX
static void translate_t3e_dv_component(opnd_type           *result_opnd,
                                       expr_arg_type       *exp_desc)

{
   /* the allocatable flag means use a ptr/pointee pair.  */
   /* It is on right now for all dv's. They must point to */
   /* contiguous storage. Eventually, it will only be on  */
   /* for ALLOCATABLE arrays.                             */

   boolean      allocatable = TRUE;
   int          asg_idx;
   int          base_attr;
   int          col;
   int          dv_idx;
   int          ir_idx;
   int          line;
   int          list_idx;
   opnd_type    opnd;
   int          ptr_idx;
   int          ptee_idx;
   boolean      save_defer_stmt_expansion;
   int          tmp_dv_idx;


   TRACE (Func_Entry, "translate_t3e_dv_component", NULL);

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   COPY_OPND(opnd, (*result_opnd));

   io_item_must_flatten = TRUE;

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   base_attr = find_left_attr(&opnd);
   dv_idx = find_base_attr(&opnd, &line, &col);

   find_opnd_line_and_column(result_opnd, &line, &col);

   tmp_dv_idx = gen_compiler_tmp(line, col, Priv, TRUE);
   ATD_TYPE_IDX(tmp_dv_idx)      = ATD_TYPE_IDX(dv_idx);
   ATD_STOR_BLK_IDX(tmp_dv_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
   AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
   ATD_ARRAY_IDX(tmp_dv_idx)     = ATD_ARRAY_IDX(dv_idx);
   ATD_POINTER(tmp_dv_idx)       = ATD_POINTER(dv_idx);
   ATD_IM_A_DOPE(tmp_dv_idx)     = TRUE;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Whole_Copy_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = tmp_dv_idx;
   IR_LINE_NUM_L(ir_idx) = line;
   IR_COL_NUM_L(ir_idx)  = col;

   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   ATD_FLD(tmp_dv_idx) = OPND_FLD(opnd);
   ATD_TMP_IDX(tmp_dv_idx) = OPND_IDX(opnd);

   gen_sh(Before, Assignment_Stmt, SH_GLB_LINE(curr_stmt_sh_idx),
          SH_COL_NUM(curr_stmt_sh_idx), FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   if (allocatable) {
      /* generate the ptr/pointee pair */

      ptr_idx  = gen_compiler_tmp(line, col, Shared, TRUE);
      ATD_TYPE_IDX(ptr_idx) = CRI_Ptr_8;
      AT_SEMANTICS_DONE(ptr_idx) = TRUE;
      ATD_STOR_BLK_IDX(ptr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      ptee_idx = gen_compiler_tmp(line, col, Shared, TRUE);
      ATD_CLASS(ptee_idx) = CRI__Pointee;
      AT_SEMANTICS_DONE(ptee_idx) = TRUE;
      ATD_STOR_BLK_IDX(ptee_idx) = SCP_SB_BASED_IDX(curr_scp_idx);
      ATD_TYPE_IDX(ptee_idx) = ATD_TYPE_IDX(dv_idx);
      ATD_PTR_IDX(ptee_idx) = ptr_idx;

      if (ATD_ARRAY_IDX(tmp_dv_idx) != NULL_IDX) {
         ATD_ARRAY_IDX(ptee_idx) = capture_bounds_from_dv(tmp_dv_idx,
                                                          line,
                                                          col);
      }

      ATD_PE_ARRAY_IDX(ptee_idx) = ATD_PE_ARRAY_IDX(base_attr);

      /* set ptr to BASE_ADDRESS(tmp_dv_idx) */

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = CRI_Ptr_8;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx) = col;

      IR_FLD_L(asg_idx) = AT_Tbl_Idx;
      IR_IDX_L(asg_idx) = ptr_idx;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx) = col;

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Dv_Access_Base_Addr;
      IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
      IR_LINE_NUM(ir_idx) = line;
      IR_COL_NUM(ir_idx)  = col;

      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = tmp_dv_idx;
      IR_LINE_NUM_L(ir_idx) = line;
      IR_COL_NUM_L(ir_idx) = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = ir_idx;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
      OPND_IDX((*result_opnd)) = ptee_idx;

      exp_desc->dope_vector = FALSE;
   }
   else {

      if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX((*result_opnd))) == Dv_Deref_Opr) {

         OPND_FLD(IR_OPND_L(OPND_IDX((*result_opnd)))) = AT_Tbl_Idx;
         OPND_IDX(IR_OPND_L(OPND_IDX((*result_opnd)))) = tmp_dv_idx;
      }
      else {
         OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
         OPND_IDX((*result_opnd)) = tmp_dv_idx;
      }

      ATD_PE_ARRAY_IDX(tmp_dv_idx)  = ATD_PE_ARRAY_IDX(base_attr);
   }

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(result_opnd);

   /* for t3e, put the linearized pe subscript on a new subscript_opr */
   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Subscript_Opr;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;
   IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(tmp_dv_idx);

   COPY_OPND(IR_OPND_L(ir_idx), (*result_opnd));

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = list_idx;
   IR_LIST_CNT_R(ir_idx) = 1;
   COPY_OPND(IL_OPND(list_idx), exp_desc->bias_opnd);
   IL_PE_SUBSCRIPT(list_idx) = TRUE;

   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = ir_idx;

   TRACE (Func_Exit, "translate_t3e_dv_component", NULL);

   return;

}  /* translate_t3e_dv_component */
# endif

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

# if defined(_TARGET_OS_MAX)
static int capture_bounds_from_dv(int   dv_attr_idx,
                                  int   line,
                                  int   col)

{
   int          asg_idx;
   int          bd_idx;
   opnd_type	dv_opnd;
   int          i;
   int          ir_idx;
   opnd_type	len_opnd;
   int		minus_idx;
   opnd_type	opnd;
   int		plus_idx;
   int          tmp_idx;

   TRACE (Func_Entry, "capture_bounds_from_dv", NULL);

   bd_idx = reserve_array_ntry(BD_RANK(ATD_ARRAY_IDX(dv_attr_idx)));
   BD_RANK(bd_idx)        = BD_RANK(ATD_ARRAY_IDX(dv_attr_idx));
   BD_LINE_NUM(bd_idx)    = line;
   BD_COLUMN_NUM(bd_idx)  = col;
   BD_ARRAY_SIZE(bd_idx)  = Var_Len_Array;
   BD_ARRAY_CLASS(bd_idx) = Explicit_Shape;
   BD_RESOLVED(bd_idx)    = TRUE;

   gen_opnd(&dv_opnd, dv_attr_idx, AT_Tbl_Idx, line, col);

   for (i =1; i <= BD_RANK(ATD_ARRAY_IDX(dv_attr_idx)); i++) {

      /* capture LB */

      gen_dv_access_low_bound(&opnd, &dv_opnd, i);

      if (OPND_FLD(opnd) == CN_Tbl_Idx ||
          (OPND_FLD(opnd) == AT_Tbl_Idx &&
           ATD_CLASS(OPND_IDX(opnd)) == Compiler_Tmp)) {

         BD_LB_FLD(bd_idx,i) = OPND_FLD(opnd);
         BD_LB_IDX(bd_idx,i) = OPND_IDX(opnd);
      }
      else {
         GEN_COMPILER_TMP_ASG(asg_idx,
                              tmp_idx,
                              TRUE,  /* Semantics done*/
                              line,
                              col,
                              SA_INTEGER_DEFAULT_TYPE,
                              Priv);
   
         COPY_OPND(IR_OPND_R(asg_idx), opnd);

         gen_sh(Before, Assignment_Stmt, line, col,
                FALSE, FALSE, TRUE);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   
         gen_copyin_bounds_stmt(tmp_idx);

         BD_LB_FLD(bd_idx,i) = AT_Tbl_Idx;
         BD_LB_IDX(bd_idx,i) = tmp_idx;
      }

      /* capture XT */

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,  /* Semantics done*/
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Dv_Access_Extent;
      IR_DV_DIM(ir_idx) = i;
      IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = line;
      IR_COL_NUM(ir_idx) = col;

      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = dv_attr_idx;
      IR_LINE_NUM_L(ir_idx) = line;
      IR_COL_NUM_L(ir_idx) = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = ir_idx;

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      gen_copyin_bounds_stmt(tmp_idx);

      BD_XT_FLD(bd_idx,i) = AT_Tbl_Idx;
      BD_XT_IDX(bd_idx,i) = tmp_idx;

      if (i == 1) {
         OPND_FLD(len_opnd) = AT_Tbl_Idx;
         OPND_IDX(len_opnd) = tmp_idx;
         OPND_LINE_NUM(len_opnd) = line;
         OPND_COL_NUM(len_opnd) = col;
      }
      else {
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Mult_Opr;
         IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx) = col;

         COPY_OPND(IR_OPND_L(ir_idx), len_opnd);
         IR_FLD_R(ir_idx) = AT_Tbl_Idx;
         IR_IDX_R(ir_idx) = tmp_idx;
         IR_LINE_NUM_R(ir_idx) = line;
         IR_COL_NUM_R(ir_idx) = col;

         OPND_FLD(len_opnd) = IR_Tbl_Idx;
         OPND_IDX(len_opnd) = ir_idx;
      }

      /* capture SM */

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,  /* Semantics done*/
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Dv_Access_Stride_Mult;
      IR_DV_DIM(ir_idx) = i;
      IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = line;
      IR_COL_NUM(ir_idx) = col;

      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = dv_attr_idx;
      IR_LINE_NUM_L(ir_idx) = line;
      IR_COL_NUM_L(ir_idx) = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = ir_idx;

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      BD_SM_FLD(bd_idx,i) = AT_Tbl_Idx;
      BD_SM_IDX(bd_idx,i) = tmp_idx;

      /* generate UB = (LB + XT) - 1 */

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,  /* Semantics done*/
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      NTR_IR_TBL(plus_idx);
      IR_OPR(plus_idx) = Plus_Opr;
      IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(plus_idx) = line;
      IR_COL_NUM(plus_idx) = col;

      IR_FLD_L(plus_idx) = AT_Tbl_Idx;
      IR_IDX_L(plus_idx) = BD_LB_IDX(bd_idx,i);
      IR_LINE_NUM_L(plus_idx) = line;
      IR_COL_NUM_L(plus_idx) = col;

      IR_FLD_R(plus_idx) = AT_Tbl_Idx;
      IR_IDX_R(plus_idx) = BD_XT_IDX(bd_idx,i);
      IR_LINE_NUM_R(plus_idx) = line;
      IR_COL_NUM_R(plus_idx) = col;

      NTR_IR_TBL(minus_idx);
      IR_OPR(minus_idx) = Minus_Opr;
      IR_TYPE_IDX(minus_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(minus_idx) = line;
      IR_COL_NUM(minus_idx) = col;

      IR_FLD_L(minus_idx) = IR_Tbl_Idx;
      IR_IDX_L(minus_idx) = plus_idx;

      IR_FLD_R(minus_idx) = CN_Tbl_Idx;
      IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
      IR_LINE_NUM_R(minus_idx) = line;
      IR_COL_NUM_R(minus_idx) = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = minus_idx;

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      gen_copyin_bounds_stmt(tmp_idx);

      BD_UB_FLD(bd_idx,i) = AT_Tbl_Idx;
      BD_UB_IDX(bd_idx,i) = tmp_idx;
   }

   GEN_COMPILER_TMP_ASG(asg_idx,
                        tmp_idx,
                        TRUE,  /* Semantics done*/
                        line,
                        col,
                        SA_INTEGER_DEFAULT_TYPE,
                        Priv);

   COPY_OPND(IR_OPND_R(asg_idx), len_opnd);

   gen_sh(Before, Assignment_Stmt, line, col,
          FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   BD_LEN_FLD(bd_idx) = AT_Tbl_Idx;
   BD_LEN_IDX(bd_idx) = tmp_idx;

   BD_FLOW_DEPENDENT(bd_idx) = TRUE;

   bd_idx =  ntr_array_in_bd_tbl(bd_idx);

   TRACE (Func_Exit, "capture_bounds_from_dv", NULL);

   return(bd_idx);

}  /* capture_bounds_from_dv */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Translate pe dimension refs into overindexed refs.                    *|
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

static void translate_distant_ref2(opnd_type		*result_opnd, 
                                   expr_arg_type	*exp_desc, 
                                   int			pe_dim_list_idx)

{
   int                  attr_idx;
   int                  bd_idx;
   long_type		bytes_per_element[MAX_WORDS_FOR_INTEGER];
   opnd_type		bytes_opnd;
   int                  col;
   int			div_idx;
   int                  i;
   int                  line;
   int                  list_idx;
   boolean              ok;
   opnd_type            opnd;
   int                  plus_idx;
   int                  plus_idx2;
   int                  sub_idx;
   int			type_idx;
   int			type1_idx;


   TRACE (Func_Entry, "translate_distant_ref2", NULL);

   find_opnd_line_and_column(result_opnd, &line, &col);

   sub_idx = OPND_IDX((*result_opnd));

   while (IR_FLD_L(sub_idx) != AT_Tbl_Idx) {
      sub_idx = IR_IDX_L(sub_idx);
   }

   attr_idx = IR_IDX_L(sub_idx);

   type_idx = ATD_TYPE_IDX(attr_idx);

   OPND_LINE_NUM(bytes_opnd) = line;
   OPND_COL_NUM(bytes_opnd) = col;

   if (TYP_TYPE(type_idx) == Structure) {
# ifdef _DEBUG
      if (ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx)) != CN_Tbl_Idx) {
         PRINTMSG(line, 626, Internal, col,
                  "CN_Tbl_Idx", "translate_distant_ref2");
      }
# endif
      type1_idx = CN_TYPE_IDX(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx)));

      ok  = folder_driver((char *)
                         &CN_CONST(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx))),
                          type1_idx,
                          (char *)&CN_CONST(CN_INTEGER_CHAR_BIT_IDX),
                          CG_INTEGER_DEFAULT_TYPE,
                          bytes_per_element,
                         &type1_idx,
                          line,
                          col,
                          2,
                          Div_Opr);

      OPND_FLD(bytes_opnd) = CN_Tbl_Idx;
      OPND_IDX(bytes_opnd) = ntr_const_tbl(type1_idx,
                                           FALSE,
                                           bytes_per_element);
   }
   else if (TYP_TYPE(type_idx) == Character) {
      OPND_FLD(bytes_opnd) = TYP_FLD(type_idx);
      OPND_IDX(bytes_opnd) = TYP_IDX(type_idx);
   }
   else {
      OPND_FLD(bytes_opnd) = CN_Tbl_Idx;
      OPND_IDX(bytes_opnd) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                              (storage_bit_size_tbl[TYP_LINEAR(type_idx)] / 8));
   }

   bd_idx = ATD_PE_ARRAY_IDX(attr_idx);

   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx) = Plus_Opr;
   IR_TYPE_IDX(plus_idx) = Integer_8;
   IR_LINE_NUM(plus_idx) = line;
   IR_COL_NUM(plus_idx) = col;

   linearize_pe_dims(pe_dim_list_idx,
                     bd_idx,
                     line,
                     col,
                    &opnd);

   /* generate reference to bias component of pe_offset_attr */

   gen_bias_ref(&opnd);

   COPY_OPND((exp_desc->bias_opnd), opnd);

   NTR_IR_TBL(div_idx);
   IR_OPR(div_idx) = Div_Opr;
   IR_TYPE_IDX(div_idx) = INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(div_idx) = line;
   IR_COL_NUM(div_idx) = col;

   COPY_OPND(IR_OPND_L(div_idx), opnd);
   COPY_OPND(IR_OPND_R(div_idx), bytes_opnd);
      
   IR_FLD_R(plus_idx) = IR_Tbl_Idx;
   IR_IDX_R(plus_idx) = div_idx;

   if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
      /* add this into the first subscript */

      list_idx = IR_IDX_R(sub_idx);

      if (IL_FLD(list_idx) == IR_Tbl_Idx &&
          IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

         /* add to both start and end */
         list_idx = IR_IDX_L(IL_IDX(list_idx));

         gen_opnd(&opnd, plus_idx, IR_Tbl_Idx, line, col);
         copy_subtree(&opnd, &opnd);
         plus_idx2 = OPND_IDX(opnd);
         COPY_OPND(IR_OPND_L(plus_idx), IL_OPND(list_idx));
         IL_FLD(list_idx) = IR_Tbl_Idx;
         IL_IDX(list_idx) = plus_idx;

         list_idx = IL_NEXT_LIST_IDX(list_idx);
         COPY_OPND(IR_OPND_L(plus_idx2), IL_OPND(list_idx));
         IL_FLD(list_idx) = IR_Tbl_Idx;
         IL_IDX(list_idx) = plus_idx2;

         list_idx = IR_IDX_R(sub_idx);
      }
      else {
         COPY_OPND(IR_OPND_L(plus_idx), IL_OPND(list_idx));
         IL_FLD(list_idx) = IR_Tbl_Idx;
         IL_IDX(list_idx) = plus_idx;
      }

      for (i = 1; i < BD_RANK(ATD_ARRAY_IDX(attr_idx)); i++) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
      IR_LIST_CNT_R(sub_idx) = BD_RANK(ATD_ARRAY_IDX(attr_idx));
   }
   else {
      COPY_OPND(IL_OPND(IR_IDX_R(sub_idx)), IR_OPND_R(plus_idx));
      IL_NEXT_LIST_IDX(IR_IDX_R(sub_idx)) = NULL_IDX;
      IR_LIST_CNT_R(sub_idx) = 1;
   }

   TRACE (Func_Exit, "translate_distant_ref2", NULL);

   return;

}  /* translate_distant_ref2 */

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

static int set_up_pe_offset_attr(void)

{

   int			attr_idx;
   expr_arg_type	exp_desc;
   int			name_idx;
   int			sb_idx;

# if !defined(_TARGET_OS_UNICOS)
   int			dt_idx;
   int			np_idx;
   long64		offset;
   int			prev_sn_idx;
   int			sn_idx;
# endif

# if defined(_TARGET_OS_UNICOS)
# define BIAS_SIZE	32
# else
# define BIAS_SIZE	128
# endif

   TRACE (Func_Entry, "set_up_pe_offset_attr", NULL);

   /***********************\
   |* set up common block *|
   \***********************/

# if defined(_TARGET_OS_UNICOS)
   CREATE_ID(TOKEN_ID(token), "_fmm_pe_bias", 12);
   TOKEN_LEN(token)         = 12;
# else
   CREATE_ID(TOKEN_ID(token), "__shmem_local_info", 18);
   TOKEN_LEN(token)         = 18;
# endif
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   sb_idx = srch_stor_blk_tbl(TOKEN_STR(token),
                              TOKEN_LEN(token),
                              curr_scp_idx);

   if (sb_idx == NULL_IDX) {
      sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                TOKEN_LEN(token),
                                TOKEN_LINE(token),
                                TOKEN_COLUMN(token),
# if defined(_TARGET_OS_UNICOS)
                                Task_Common);
# else
                                Common);
# endif

      SB_BLANK_COMMON(sb_idx)        = FALSE;
      SB_COMMON_NEEDS_OFFSET(sb_idx) = FALSE;
      SB_NAME_IN_STONE(sb_idx)       = TRUE;
   }
   else {
      /* error */
   }

# if ! defined(_TARGET_OS_UNICOS)

   /****************************\
   |* create derived type attr *|
   \****************************/

   CREATE_ID(TOKEN_ID(token), "__shmem_local_info_type", 23);
   TOKEN_LEN(token)         = 23;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   dt_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

   if (dt_idx == NULL_IDX) {
      dt_idx                         = ntr_sym_tbl(&token, name_idx);
      AT_OBJ_CLASS(dt_idx)           = Derived_Type;
      ATT_SCP_IDX(dt_idx)            = curr_scp_idx;
      ATT_NUMERIC_CPNT(dt_idx)     = TRUE;
      ATT_DCL_NUMERIC_SEQ(dt_idx)  = TRUE;
      ATT_SEQUENCE_SET(dt_idx)     = TRUE;
#ifdef KEY /* Bug 10140 */
      ATT_ALIGNMENT(dt_idx) = Align_64;
#endif /* KEY Bug 10140 */
   }
   else {
      /* error */
   }

   ATT_NUM_CPNTS(dt_idx) = 0;

   /**************************\
   |* now for the components *|
   \**************************/

   offset = 0;

   /* integer (4) :: anchor */

   CREATE_ID(TOKEN_ID(token), "anchor", 6);
   TOKEN_LEN(token)         = 6;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   NTR_SN_TBL(sn_idx);
   NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)  = Data_Obj;
   AT_DEF_LINE(attr_idx)   = TOKEN_LINE(token);
   AT_DEF_COLUMN(attr_idx) = TOKEN_COLUMN(token);
   AT_NAME_LEN(attr_idx)   = TOKEN_LEN(token);
   AT_NAME_IDX(attr_idx)   = np_idx;
   SN_NAME_LEN(sn_idx)     = TOKEN_LEN(token);
   SN_NAME_IDX(sn_idx)     = np_idx;
   SN_ATTR_IDX(sn_idx)     = attr_idx;

   AT_SEMANTICS_DONE(attr_idx) = TRUE;
   ATD_CLASS(attr_idx)         = Struct_Component;
   ATD_DERIVED_TYPE_IDX(attr_idx)       = dt_idx;

   AT_TYPED(attr_idx)          = TRUE;

   ATD_TYPE_IDX(attr_idx)      = Integer_4;

   ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
   ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
   ATD_CPNT_OFFSET_IDX(attr_idx) = CN_INTEGER_ZERO_IDX;

   offset += storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))];

   ATT_FIRST_CPNT_IDX(dt_idx) = sn_idx;
   ATT_NUM_CPNTS(dt_idx) += 1;

   prev_sn_idx = sn_idx;

   /* integer (4) :: mype */

   CREATE_ID(TOKEN_ID(token), "mype", 4);
   TOKEN_LEN(token)         = 4;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   NTR_SN_TBL(sn_idx);
   NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)  = Data_Obj;
   AT_DEF_LINE(attr_idx)   = TOKEN_LINE(token);
   AT_DEF_COLUMN(attr_idx) = TOKEN_COLUMN(token);
   AT_NAME_LEN(attr_idx)   = TOKEN_LEN(token);
   AT_NAME_IDX(attr_idx)   = np_idx;
   SN_NAME_LEN(sn_idx)     = TOKEN_LEN(token);
   SN_NAME_IDX(sn_idx)     = np_idx;
   SN_ATTR_IDX(sn_idx)     = attr_idx;

   AT_SEMANTICS_DONE(attr_idx) = TRUE;
   ATD_CLASS(attr_idx)         = Struct_Component;
   ATD_DERIVED_TYPE_IDX(attr_idx)       = dt_idx;
   AT_TYPED(attr_idx)          = TRUE;

   ATD_TYPE_IDX(attr_idx)      = Integer_4;

   ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
   ATD_OFFSET_FLD(attr_idx)      = CN_Tbl_Idx;
   ATD_CPNT_OFFSET_IDX(attr_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, offset);

   offset += storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))];

   ATT_NUM_CPNTS(dt_idx) += 1;
   SN_SIBLING_LINK(prev_sn_idx) = sn_idx;

   prev_sn_idx = sn_idx;

   /* integer (4) :: initcomplete */

   CREATE_ID(TOKEN_ID(token), "initcomplete", 12);
   TOKEN_LEN(token)         = 12;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   NTR_SN_TBL(sn_idx);
   NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)  = Data_Obj;
   AT_DEF_LINE(attr_idx)   = TOKEN_LINE(token);
   AT_DEF_COLUMN(attr_idx) = TOKEN_COLUMN(token);
   AT_NAME_LEN(attr_idx)   = TOKEN_LEN(token);
   AT_NAME_IDX(attr_idx)   = np_idx;
   SN_NAME_LEN(sn_idx)     = TOKEN_LEN(token);
   SN_NAME_IDX(sn_idx)     = np_idx;
   SN_ATTR_IDX(sn_idx)     = attr_idx;

   AT_SEMANTICS_DONE(attr_idx) = TRUE;
   ATD_CLASS(attr_idx)         = Struct_Component;
   ATD_DERIVED_TYPE_IDX(attr_idx)       = dt_idx;
   AT_TYPED(attr_idx)          = TRUE;

   ATD_TYPE_IDX(attr_idx)      = Integer_4;

   ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
   ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
   ATD_CPNT_OFFSET_IDX(attr_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, offset);

   offset += storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))] + 32;

   ATT_NUM_CPNTS(dt_idx) += 1;
   SN_SIBLING_LINK(prev_sn_idx) = sn_idx;

   prev_sn_idx = sn_idx;

   /* integer (8) :: bias(128) */

   CREATE_ID(TOKEN_ID(token), "bias", 4);
   TOKEN_LEN(token)         = 4;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   NTR_SN_TBL(sn_idx);
   NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)  = Data_Obj;
   AT_DEF_LINE(attr_idx)   = TOKEN_LINE(token);
   AT_DEF_COLUMN(attr_idx) = TOKEN_COLUMN(token);
   AT_NAME_LEN(attr_idx)   = TOKEN_LEN(token);
   AT_NAME_IDX(attr_idx)   = np_idx;
   SN_NAME_LEN(sn_idx)     = TOKEN_LEN(token);
   SN_NAME_IDX(sn_idx)     = np_idx;
   SN_ATTR_IDX(sn_idx)     = attr_idx;

   AT_SEMANTICS_DONE(attr_idx) = TRUE;
   ATD_CLASS(attr_idx)         = Struct_Component;
   ATD_DERIVED_TYPE_IDX(attr_idx)       = dt_idx;
   AT_TYPED(attr_idx)          = TRUE;

   ATD_TYPE_IDX(attr_idx)      = Integer_8;

   exp_desc = init_exp_desc;
   exp_desc.type_idx = Integer_8;
   exp_desc.linear_type = Integer_8;
   exp_desc.type        = Integer;
   exp_desc.shape[0].fld = CN_Tbl_Idx;

   exp_desc.rank = 1;

   exp_desc.shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, BIAS_SIZE);

   ATD_ARRAY_IDX(attr_idx) = create_bd_ntry_for_const(&exp_desc, 
                                                      stmt_start_line,  
                                                      stmt_start_col);

   ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
   ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
   ATD_CPNT_OFFSET_IDX(attr_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, offset);

   offset += BIAS_SIZE * 
               storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))];

   ATT_NUM_CPNTS(dt_idx) += 1;
   SN_SIBLING_LINK(prev_sn_idx) = sn_idx;

   prev_sn_idx = sn_idx;

   /* integer (SA_INTEGER_DEFAULT_TYPE) :: shheapbase */

   CREATE_ID(TOKEN_ID(token), "shheapbase", 10);
   TOKEN_LEN(token)         = 10;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   NTR_SN_TBL(sn_idx);
   NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)  = Data_Obj;
   AT_DEF_LINE(attr_idx)   = TOKEN_LINE(token);
   AT_DEF_COLUMN(attr_idx) = TOKEN_COLUMN(token);
   AT_NAME_LEN(attr_idx)   = TOKEN_LEN(token);
   AT_NAME_IDX(attr_idx)   = np_idx;
   SN_NAME_LEN(sn_idx)     = TOKEN_LEN(token);
   SN_NAME_IDX(sn_idx)     = np_idx;
   SN_ATTR_IDX(sn_idx)     = attr_idx;

   AT_SEMANTICS_DONE(attr_idx) = TRUE;
   ATD_CLASS(attr_idx)         = Struct_Component;
   ATD_DERIVED_TYPE_IDX(attr_idx)       = dt_idx;
   AT_TYPED(attr_idx)          = TRUE;

   ATD_TYPE_IDX(attr_idx)      = SA_INTEGER_DEFAULT_TYPE;

   ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
   ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
   ATD_CPNT_OFFSET_IDX(attr_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,offset);

   offset += storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))];

   ATT_NUM_CPNTS(dt_idx) += 1;
   SN_SIBLING_LINK(prev_sn_idx) = sn_idx;

   prev_sn_idx = sn_idx;

   /* integer (SA_INTEGER_DEFAULT_TYPE) :: shheapend */

   CREATE_ID(TOKEN_ID(token), "shheapend", 9);
   TOKEN_LEN(token)         = 9;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   NTR_SN_TBL(sn_idx);
   NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)  = Data_Obj;
   AT_DEF_LINE(attr_idx)   = TOKEN_LINE(token);
   AT_DEF_COLUMN(attr_idx) = TOKEN_COLUMN(token);
   AT_NAME_LEN(attr_idx)   = TOKEN_LEN(token);
   AT_NAME_IDX(attr_idx)   = np_idx;
   SN_NAME_LEN(sn_idx)     = TOKEN_LEN(token);
   SN_NAME_IDX(sn_idx)     = np_idx;
   SN_ATTR_IDX(sn_idx)     = attr_idx;

   AT_SEMANTICS_DONE(attr_idx) = TRUE;
   ATD_CLASS(attr_idx)         = Struct_Component;
   ATD_DERIVED_TYPE_IDX(attr_idx)       = dt_idx;
   AT_TYPED(attr_idx)          = TRUE;

   ATD_TYPE_IDX(attr_idx)      = SA_INTEGER_DEFAULT_TYPE;

   ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
   ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
   ATD_CPNT_OFFSET_IDX(attr_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, offset);

   offset += storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))];

   ATT_NUM_CPNTS(dt_idx) += 1;
   SN_SIBLING_LINK(prev_sn_idx) = sn_idx;


   ATT_STRUCT_BIT_LEN_FLD(dt_idx) = CN_Tbl_Idx;
#ifdef KEY /* Bug 10140 */
   ATT_STRUCT_BIT_LEN_IDX(dt_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                  offset);
#else /* KEY Bug 10140 */
   ATT_STRUCT_BIT_LEN_IDX(attr_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                  offset);
#endif /* KEY Bug 10140 */

   /*****************************************\
   |* Gen the data obj of this derived type *|
   \*****************************************/

   CREATE_ID(TOKEN_ID(token), "_shmem_local_info", 17);
   TOKEN_LEN(token)         = 17;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx = ntr_sym_tbl(&token, name_idx);

      AT_OBJ_CLASS(attr_idx)        = Data_Obj;
      AT_REFERENCED(attr_idx)       = Referenced;
      AT_LOCKED_IN(attr_idx)        = TRUE;
      AT_TYPED(attr_idx)            = TRUE;
      AT_SEMANTICS_DONE(attr_idx)   = TRUE;
      ATD_CLASS(attr_idx)           = Variable;
      ATD_IN_COMMON(attr_idx)       = TRUE;
      ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)        = Structure;
      TYP_LINEAR(TYP_WORK_IDX)      = Structure_Type;
      TYP_IDX(TYP_WORK_IDX)         = dt_idx;

      ATD_TYPE_IDX(attr_idx)        = ntr_type_tbl();
      ATD_STOR_BLK_IDX(attr_idx)    = sb_idx;
      ATD_OFFSET_FLD(attr_idx)      = CN_Tbl_Idx;
      ATD_OFFSET_IDX(attr_idx)      = CN_INTEGER_ZERO_IDX;
   }
   else {
      /* error */
   }

   SB_FIRST_ATTR_IDX(sb_idx) = attr_idx;
   SB_LEN_FLD(sb_idx) = CN_Tbl_Idx;
   SB_LEN_IDX(sb_idx) = ATT_STRUCT_BIT_LEN_IDX(dt_idx);

# else

   /*****************************************\
   |* Gen the data obj of this derived type *|
   \*****************************************/

   CREATE_ID(TOKEN_ID(token), "__fmm_pe_bias", 13);
   TOKEN_LEN(token)         = 13;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = stmt_start_line;
   TOKEN_COLUMN(token)      = stmt_start_col;

   attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx = ntr_sym_tbl(&token, name_idx);

      AT_OBJ_CLASS(attr_idx)        = Data_Obj;
      AT_REFERENCED(attr_idx)       = Referenced;
      AT_LOCKED_IN(attr_idx)        = TRUE;
      AT_TYPED(attr_idx)            = TRUE;
      AT_SEMANTICS_DONE(attr_idx)   = TRUE;
      ATD_CLASS(attr_idx)           = Variable;
      ATD_IN_COMMON(attr_idx)       = TRUE;
      ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;

      ATD_TYPE_IDX(attr_idx)        = Integer_8;

      exp_desc = init_exp_desc;
      exp_desc.type_idx = Integer_8;
      exp_desc.linear_type = Integer_8;
      exp_desc.type        = Integer;
      exp_desc.shape[0].fld = CN_Tbl_Idx;

      exp_desc.rank = 1;

      exp_desc.shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, BIAS_SIZE);

      ATD_ARRAY_IDX(attr_idx) = create_bd_ntry_for_const(&exp_desc, 
                                                         stmt_start_line, 
                                                         stmt_start_col);
      ATD_STOR_BLK_IDX(attr_idx)    = sb_idx;
      ATD_OFFSET_FLD(attr_idx)      = CN_Tbl_Idx;
      ATD_OFFSET_IDX(attr_idx)      = CN_INTEGER_ZERO_IDX;
   }
   else {
      /* error */
   }

   SB_FIRST_ATTR_IDX(sb_idx) = attr_idx;
   SB_LEN_FLD(sb_idx) = CN_Tbl_Idx;

   SB_LEN_IDX(sb_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, (BIAS_SIZE * 64));

# endif

   TRACE (Func_Exit, "set_up_pe_offset_attr", NULL);

   return(attr_idx);

}  /* set_up_pe_offset_attr */

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

static void gen_bias_ref(opnd_type *opnd)

{
   int		col;
   int		line;
   int		list_idx;
   int		sub_idx;

# if ! defined(_TARGET_OS_UNICOS)
   int		bias_idx;
   int		struct_idx;
# endif



   TRACE (Func_Entry, "gen_bias_ref", NULL);

   find_opnd_line_and_column(opnd, &line, &col);

   if (glb_tbl_idx[Pe_Offset_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Pe_Offset_Attr_Idx] = set_up_pe_offset_attr();
   }

# if ! defined(_TARGET_OS_UNICOS)
   bias_idx = SN_ATTR_IDX(SN_SIBLING_LINK(SN_SIBLING_LINK(SN_SIBLING_LINK(
        ATT_FIRST_CPNT_IDX(TYP_IDX(ATD_TYPE_IDX(
                      glb_tbl_idx[Pe_Offset_Attr_Idx])))))));

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = ATD_TYPE_IDX(bias_idx);
   IR_LINE_NUM(sub_idx) = line;
   IR_COL_NUM(sub_idx)  = col;
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_R(sub_idx) = 1;
   
   NTR_IR_LIST_TBL(list_idx);
   
   IR_IDX_R(sub_idx) = list_idx;

   COPY_OPND(IL_OPND(list_idx), (*opnd));

   NTR_IR_TBL(struct_idx);
   IR_OPR(struct_idx) = Struct_Opr;
   IR_TYPE_IDX(struct_idx) = ATD_TYPE_IDX(bias_idx);
   IR_LINE_NUM(struct_idx) = line;
   IR_COL_NUM(struct_idx)  = col;
   IR_LINE_NUM_L(struct_idx) = line;
   IR_COL_NUM_L(struct_idx)  = col;
   IR_LINE_NUM_R(struct_idx) = line;
   IR_COL_NUM_R(struct_idx)  = col;

   IR_FLD_L(struct_idx) = AT_Tbl_Idx;
   IR_IDX_L(struct_idx) = glb_tbl_idx[Pe_Offset_Attr_Idx];
   IR_FLD_R(struct_idx) = AT_Tbl_Idx;
   IR_IDX_R(struct_idx) = bias_idx;

   IR_FLD_L(sub_idx) = IR_Tbl_Idx;
   IR_IDX_L(sub_idx) = struct_idx;

   OPND_FLD((*opnd)) = IR_Tbl_Idx;
   OPND_IDX((*opnd)) = sub_idx;

# else

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = Integer_8;
   IR_LINE_NUM(sub_idx) = line;
   IR_COL_NUM(sub_idx)  = col;
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_R(sub_idx) = 1;

   NTR_IR_LIST_TBL(list_idx);

   IR_IDX_R(sub_idx) = list_idx;

   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_IDX_L(sub_idx) = glb_tbl_idx[Pe_Offset_Attr_Idx];
   IR_LINE_NUM_L(sub_idx) = line;
   IR_COL_NUM_L(sub_idx) = col;

   COPY_OPND(IL_OPND(list_idx), (*opnd));

   OPND_FLD((*opnd)) = IR_Tbl_Idx;
   OPND_IDX((*opnd)) = sub_idx;
# endif

   TRACE (Func_Exit, "gen_bias_ref", NULL);

   return;

}  /* gen_bias_ref */
# endif

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      When a cri char pointer is assigned an integer/ptr value by assignment*|
|*      or data initialization, it must be treated as an integer. This routine*|
|*      either creates an equivalenced integer temp, or, if it is a dummy arg,*|
|*      an access through a pointer/pointee pair to set the bits correctly.   *|
|*      The things we do to sell a machine.                                   *|
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

void transform_cri_ch_ptr(opnd_type     *result_opnd)

{
   int          asg_idx;
   int          attr_idx;
   int          col;
   int          eq_idx;
   int          eq_tmp_idx;
   int          line;
   int          loc_idx;
   int          overlay_attr_idx;
   int          ptee_idx;
   int          ptr_idx;
   int          sb_idx;

   TRACE (Func_Entry, "transform_cri_ch_ptr", NULL);

   find_opnd_line_and_column(result_opnd, &line, &col);
   attr_idx = find_left_attr(result_opnd);

# ifdef _DEBUG
   if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
      PRINTMSG(line, 626, Internal, col,
               "Data_Obj", "transform_cri_ch_ptr");
   }
   else if (OPND_FLD((*result_opnd)) != AT_Tbl_Idx) {
      PRINTMSG(line, 626, Internal, col,
               "AT_Tbl_Idx", "transform_cri_ch_ptr");
   }
   else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != CRI_Ch_Ptr) {
      PRINTMSG(line, 626, Internal, col,
               "CRI_Ch_Ptr", "transform_cri_ch_ptr");
   }
   else if (defer_stmt_expansion) {
      PRINTMSG(line, 626, Internal, col,
               "not defer_stmt_expansion", "transform_cri_ch_ptr");
   }
# endif

   if (ATD_CLASS(attr_idx) == Variable) {

      if (ATD_VARIABLE_TMP_IDX(attr_idx) != NULL_IDX) {
         overlay_attr_idx = ATD_VARIABLE_TMP_IDX(attr_idx);
         goto FOUND;
      }

      overlay_attr_idx = gen_compiler_tmp(line, col, Shared, TRUE);
      ATD_CLASS(overlay_attr_idx) = Variable;
#ifdef KEY
      ATD_TYPE_IDX(overlay_attr_idx)         = SA_INTEGER_DEFAULT_TYPE;
#else
      ATD_TYPE_IDX(overlay_attr_idx)         = INTEGER_DEFAULT_TYPE;
#endif
      ATD_STOR_BLK_IDX(overlay_attr_idx)     = ATD_STOR_BLK_IDX(attr_idx);
      ATD_EQUIV(overlay_attr_idx)            = TRUE;
      AT_REFERENCED(overlay_attr_idx)        = Referenced;
      AT_SEMANTICS_DONE(overlay_attr_idx)    = TRUE;
      AT_DEFINED(overlay_attr_idx)           = TRUE;

      ATD_OFFSET_FLD(overlay_attr_idx)      = ATD_OFFSET_FLD(attr_idx);
      ATD_OFFSET_IDX(overlay_attr_idx)      = ATD_OFFSET_IDX(attr_idx);
      ATD_OFFSET_ASSIGNED(overlay_attr_idx) = ATD_OFFSET_ASSIGNED(attr_idx);

      /* The overlay tmp and the variable must have the same offset.    */
      /* Find the equivalence group for the variable and add the tmp to */
      /* the equivalence group.  To do this, create a new equivalence   */
      /* table entry, add it to the group and make ATD_OFFSET be the    */
      /* same for both.  (ATD_OFFSET can be set, even if ATD_OFFSET     */
      /* ASSIGNED is FALSE because this is the equivalence group        */
      /* offset).                                                       */

      if (ATD_EQUIV(attr_idx)) {
         eq_idx   = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

         while (eq_idx != NULL_IDX) {
            eq_tmp_idx    = eq_idx;
            eq_idx        = EQ_NEXT_EQUIV_GRP(eq_idx);

            while (eq_tmp_idx != NULL_IDX) {

               if (EQ_ATTR_IDX(eq_tmp_idx) == attr_idx) { /* Found */
                  NTR_EQ_TBL(eq_idx);
                  COPY_TBL_NTRY(equiv_tbl, eq_idx, eq_tmp_idx);
                  EQ_NEXT_EQUIV_OBJ(eq_tmp_idx)   = eq_idx;
                  EQ_ATTR_IDX(eq_idx)             = overlay_attr_idx;
                  ATD_OFFSET_FLD(overlay_attr_idx)=
                                                 ATD_OFFSET_FLD(attr_idx);
                  ATD_OFFSET_IDX(overlay_attr_idx)=
                                                 ATD_OFFSET_IDX(attr_idx);
                  ATD_EQUIV(attr_idx)             = TRUE;
                  goto FOUND;
               }
               eq_tmp_idx = EQ_NEXT_EQUIV_OBJ(eq_tmp_idx);
            }
         }
      }

      /* It is not in an equivalence group or it is not   */
      /* equivalenced, so make its own equivalence group. */

      NTR_EQ_TBL(eq_idx);
      NTR_EQ_TBL(eq_tmp_idx);

      EQ_NEXT_EQUIV_GRP(eq_idx)   = SCP_FIRST_EQUIV_GRP(curr_scp_idx);
      SCP_FIRST_EQUIV_GRP(curr_scp_idx)   = eq_idx;
      EQ_ATTR_IDX(eq_idx)                 = attr_idx;
      EQ_ATTR_IDX(eq_tmp_idx)             = overlay_attr_idx;
      EQ_NEXT_EQUIV_OBJ(eq_idx)           = eq_tmp_idx;
      ATD_EQUIV(attr_idx)                 = TRUE;
      ATD_VARIABLE_TMP_IDX(attr_idx)      = overlay_attr_idx;
      ATD_FLD(attr_idx)                   = AT_Tbl_Idx;

      sb_idx      = ATD_STOR_BLK_IDX(attr_idx);

      if (SB_BLK_TYPE(sb_idx) == Stack) {
         sb_idx = create_equiv_stor_blk(attr_idx, Stack);
         SB_EQUIVALENCED(sb_idx) = TRUE;
         ATD_STOR_BLK_IDX(overlay_attr_idx) = sb_idx;
         ATD_STOR_BLK_IDX(attr_idx) = sb_idx;
      }

# if defined(_SEPARATE_NONCOMMON_EQUIV_GROUPS)


      if (sb_idx == NULL_IDX ||
          (!SB_MODULE(sb_idx) && !SB_IS_COMMON(sb_idx))) {

         if (SB_HOSTED_STATIC(sb_idx)) {
            sb_idx = create_equiv_stor_blk(attr_idx, SB_BLK_TYPE(sb_idx));
            SB_HOSTED_STATIC(sb_idx)      = TRUE;
         }
         else {
            sb_idx = create_equiv_stor_blk(attr_idx, SB_BLK_TYPE(sb_idx));
         }

         ATD_STOR_BLK_IDX(attr_idx)               = sb_idx;
         ATD_STOR_BLK_IDX(overlay_attr_idx)       = sb_idx;
      }
# endif

FOUND:

      OPND_IDX((*result_opnd)) = overlay_attr_idx;

   }
   else if (ATD_CLASS(attr_idx) == Dummy_Argument) {
      /* create pointer/pointee pair and set pointer to loc(attr_idx) */
      ptr_idx  = gen_compiler_tmp(line, col, Shared, TRUE);
      ATD_TYPE_IDX(ptr_idx) = CRI_Ptr_8;
      AT_SEMANTICS_DONE(ptr_idx) = TRUE;
      ATD_STOR_BLK_IDX(ptr_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      ptee_idx = gen_compiler_tmp(line, col, Shared, TRUE);
      ATD_CLASS(ptee_idx) = CRI__Pointee;
      AT_SEMANTICS_DONE(ptee_idx) = TRUE;
      ATD_STOR_BLK_IDX(ptee_idx) = SCP_SB_BASED_IDX(curr_scp_idx);
      ATD_TYPE_IDX(ptee_idx) = INTEGER_DEFAULT_TYPE;
      ATD_PTR_IDX(ptee_idx) = ptr_idx;

      /* generate assignment to ptr */

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = CRI_Ptr_8;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx) = col;

      IR_FLD_L(asg_idx) = AT_Tbl_Idx;
      IR_IDX_L(asg_idx) = ptr_idx;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx) = col;

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx) = Loc_Opr;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx) = col;

      /* do I need to worry about a proper reference tree for loc? BHJ */

      IR_FLD_L(loc_idx) = AT_Tbl_Idx;
      IR_IDX_L(loc_idx) = attr_idx;
      IR_LINE_NUM_L(loc_idx) = line;
      IR_COL_NUM_L(loc_idx) = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = loc_idx;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      OPND_IDX((*result_opnd)) = ptee_idx;;
   }
   else {
      PRINTMSG(line, 626, Internal, col,
               "variable or dummy arg", "transform_cri_ch_ptr");
   }

   TRACE (Func_Exit, "transform_cri_ch_ptr", NULL);

   return;

}  /* transform_cri_ch_ptr */
