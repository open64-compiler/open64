/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_utils.c	5.12	10/19/99 17:14:30\n";


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
# include "s_utils.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"

# if defined(_HOST_OS_UNICOS) && defined(_TARGET_OS_UNICOS)
# include <fortran.h>
# endif
#ifdef KEY /* Bug 6845 */
/* Incredibly, there seems to be no single central definition for the limit
 * on subscripts in this front end. Often it's a hard-coded "7". This gives
 * us STATIC_SUBSCRIPT_SIZE, which is used in only one other place. Sigh. */
#include "i_cvrt.h"
#endif /* KEY Bug 6845 */


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static int      opr_to_str(operator_type, char *);
static int      create_dv_type_code(int);
static long64   create_imp_do_loops(opnd_type *);
static void     just_find_dope_and_rank(opnd_type *, int *, int *);
static void     compute_char_element_len(opnd_type *,
				               opnd_type *, opnd_type *);
static void	gen_conform_check_call(opnd_type *, opnd_type *, int, int, int);
static void	gen_bounds_check_call(char *, opnd_type *, opnd_type *,
                                      opnd_type *, int, int, int);
static void	gen_rbounds_check_call(char *, opnd_type *, opnd_type *,
                                       opnd_type *, opnd_type *,
                                       opnd_type *, int, int, int);
static void	gen_sbounds_check_call(char *, opnd_type *, opnd_type *, 
                                       opnd_type *, int, int);
static void	gen_ptr_chk_call(char *, int, opnd_type *, int, int);
static int	put_file_name_in_cn(int);
static int	put_c_str_in_cn(char *);
static void	gen_dv_def_loops(opnd_type *);
static void	gen_init_stmt(opnd_type *, int, sh_position_type);
static void	reshape_reference_subscripts(opnd_type *);
static void	gen_dv_stride_mult(opnd_type *, int, opnd_type *,
                                   expr_arg_type *, int, int, int);


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	resolve defined operators and assignment.                             *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - sub tree of operator.                                          *|
|*									      *|
|* Output parameters:							      *|
|*									      *|
|* Returns:								      *|
|*	TRUE - if operator resolved ok.                                       *|
|*									      *|
\******************************************************************************/

boolean resolve_ext_opr(opnd_type 	*opnd,
                        boolean          issue_msg,
                        boolean		 save_in_call_list,
			boolean		 err_res,
                        boolean         *semantically_correct,
                        expr_arg_type   *exp_desc_l,
                        expr_arg_type   *exp_desc_r)

{
   opnd_type	arg_1_opnd;
   opnd_type	arg_2_opnd;
   int		arg_idx;
   int		attr_idx;
   int		col;
   int		darg_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int		false_list_idx 		= NULL_IDX;
# endif

   boolean	found			= FALSE;
   int		gen_idx			= NULL_IDX;
   int		i;
   int		idx;
   int		info_idx;
   int          ir_idx;
   boolean      is_function		= TRUE;
   int          len;
   int		line;
   int		list_idx;
   int		list1_idx;
   int		list2_idx;
   int		loc_idx;
   int		name_idx;
   int		num_args;
   boolean	ok			= TRUE;
   int		opnd_column;
   int		opnd_line;
   int		rslt_idx;
   int          save_arg_info_list_base;
   int		save_curr_stmt_sh_idx;
   int		save_defer_stmt_expansion;
   int		spec_idx		= NULL_IDX;
   int          sn_idx			= NULL_IDX;
   char         str_word[32];
   opnd_type    tmp_opnd;
   char         type_str_l[45];
   char         type_str_r[45];


   TRACE (Func_Entry, "resolve_ext_opr", NULL);

   /* do memory management stuff to make sure the tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;
   arg_info_list_base      = arg_info_list_top;
   arg_info_list_top       = arg_info_list_base + 2;

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

   ir_idx = OPND_IDX((*opnd));
   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

   if (IR_OPR(ir_idx) == Defined_Bin_Opr) {

      gen_idx  = IR_IDX_L(ir_idx);
      strncpy(str_word, AT_OBJ_NAME_PTR(gen_idx), AT_NAME_LEN(gen_idx));
      str_word[AT_NAME_LEN(gen_idx)] = '\0';
      num_args = 2;
      COPY_OPND(arg_1_opnd, IL_OPND(IR_IDX_R(ir_idx)));
      COPY_OPND(arg_2_opnd, IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))));

      if (cif_flags & XREF_RECS) {
         cif_usage_rec(gen_idx, AT_Tbl_Idx, line, col, CIF_Symbol_Reference);
      }
   }
   else if (IR_OPR(ir_idx) == Defined_Un_Opr) {
      gen_idx  = IR_IDX_L(ir_idx);
      strncpy(str_word, AT_OBJ_NAME_PTR(gen_idx), AT_NAME_LEN(gen_idx));
      str_word[AT_NAME_LEN(gen_idx)] = '\0';
      num_args = 1;
      COPY_OPND(arg_1_opnd, IR_OPND_R(ir_idx));

      if (cif_flags & XREF_RECS) {
         cif_usage_rec(gen_idx, AT_Tbl_Idx, line, col, CIF_Symbol_Reference);
      }
   }
   else {
      len	= opr_to_str(IR_OPR(ir_idx), str_word);
      gen_idx	= srch_sym_tbl(str_word, len, &name_idx);

      if (gen_idx == NULL_IDX) {
         gen_idx	 = srch_host_sym_tbl(str_word, len, &name_idx, TRUE);
      }

      COPY_OPND(arg_1_opnd, IR_OPND_L(ir_idx));

      if (IR_FLD_R(ir_idx) == NO_Tbl_Idx) {
         num_args = 1;
      }
      else {
         num_args = 2;
         COPY_OPND(arg_2_opnd, IR_OPND_R(ir_idx));
      }
   }

   if (IR_OPR(ir_idx) == Asg_Opr) {
      is_function = FALSE;
   }

   if (gen_idx   == NULL_IDX               ||
       AT_OBJ_CLASS(gen_idx) != Interface) {
      gen_idx = NULL_IDX;
      goto EXIT;
   }

   for (i = 0; i < ATI_NUM_SPECIFICS(gen_idx); i++) {

      sn_idx	= (sn_idx == NULL_IDX) ? ATI_FIRST_SPECIFIC_IDX(gen_idx) :
                                         SN_SIBLING_LINK(sn_idx);
      spec_idx	= SN_ATTR_IDX(sn_idx);

      /* check number, type etc. for match with arg list */

      if (ATP_EXTRA_DARG(spec_idx)) {

         if (num_args != ATP_NUM_DARGS(spec_idx) - 1) {
            continue;
         }

         darg_idx = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + 1);
      }
      else {

         if (num_args != ATP_NUM_DARGS(spec_idx)) {
            continue;
         }

         darg_idx = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx));
      }

      /* look at each actual arg for match */

      if (darg_idx == NULL_IDX) {
         continue;
      }

      if (AT_OBJ_CLASS(darg_idx) == Data_Obj) {

         if (ATD_IGNORE_TKR(darg_idx)) {
            /* intentionally blank */
            /* This dummy arg will match any type, so skip */
            /* the type and kind type checking below.      */
         }
         else if (OPND_FLD(arg_1_opnd) == IR_Tbl_Idx &&
                  IR_OPR(OPND_IDX(arg_1_opnd)) == Null_Intrinsic_Opr) {
            /* intentionally blank */
            /* Don't know type or rank yet, they come from dummy */
         }
         else if (TYP_TYPE(ATD_TYPE_IDX(darg_idx)) != exp_desc_l->type) {
            continue;
         }
         else if (exp_desc_l->type == Structure) {

            if (!compare_derived_types(exp_desc_l->type_idx,
                                       ATD_TYPE_IDX(darg_idx))) {
               continue;
            }
         }
         else if (exp_desc_l->type != Character   &&
           TYP_LINEAR(ATD_TYPE_IDX(darg_idx)) != exp_desc_l->linear_type) {
            continue;
         }

         if (ATD_IGNORE_TKR(darg_idx)) {
            /* intentionally blank */
            /* This dummy arg will match any rank, so skip */
            /* the rank checking below.      */
         }
         else if (OPND_FLD(arg_1_opnd) == IR_Tbl_Idx &&
                  IR_OPR(OPND_IDX(arg_1_opnd)) == Null_Intrinsic_Opr) {
            /* intentionally blank */
            /* Don't know type or rank yet, they come from dummy */
         }
         else if (ATP_ELEMENTAL(spec_idx)) {
            /* intentionally blank, don't check array conformance */
         }
         else if (ATD_ARRAY_IDX(darg_idx) == NULL_IDX) {
       
            if (exp_desc_l->rank) {
               continue;
            }
         }
         else {

            if (BD_RANK(ATD_ARRAY_IDX(darg_idx)) != exp_desc_l->rank) {
               continue;
            }
         }
      }
      else if (AT_OBJ_CLASS(darg_idx) == Pgm_Unit) {
         /* not sure this is possible */
      }

      if (num_args == 2) {
         if (ATP_EXTRA_DARG(spec_idx)) {
            darg_idx = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + 2);
         }
         else {
            darg_idx = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + 1);
         }
         /* look at each actual arg for match */
   
         if (darg_idx == NULL_IDX) {
            continue;
         }
      
         if (AT_OBJ_CLASS(darg_idx) == Data_Obj) {

            if (ATD_IGNORE_TKR(darg_idx)) {
               /* intentionally blank */
               /* This dummy arg will match any type, so skip */
               /* the type and kind type checking below.      */
            }
            else if (OPND_FLD(arg_2_opnd) == IR_Tbl_Idx &&
                     IR_OPR(OPND_IDX(arg_2_opnd)) == Null_Intrinsic_Opr) {
               /* intentionally blank */
               /* Don't know type or rank yet, they come from dummy */
            }
            else if (TYP_TYPE(ATD_TYPE_IDX(darg_idx)) != exp_desc_r->type) {
               continue;
            }
            else if (exp_desc_r->type == Structure) {

               if (!compare_derived_types(exp_desc_r->type_idx, 
                                          ATD_TYPE_IDX(darg_idx))) {
                  continue;
               }
            }
            else if (exp_desc_r->type != Character && 
            TYP_LINEAR(ATD_TYPE_IDX(darg_idx)) != exp_desc_r->linear_type) {
               continue;
            }

            if (ATD_IGNORE_TKR(darg_idx)) {
               /* intentionally blank */
               /* This dummy arg will match any rank, so skip */
               /* the rank checking below.      */
            }
            else if (OPND_FLD(arg_2_opnd) == IR_Tbl_Idx &&
                     IR_OPR(OPND_IDX(arg_2_opnd)) == Null_Intrinsic_Opr) {
               /* intentionally blank */
               /* Don't know type or rank yet, they come from dummy */
            }
            else if (ATP_ELEMENTAL(spec_idx)) {
               /* intentionally blank, don't check array conformance */
            }
            else if (ATD_ARRAY_IDX(darg_idx) == NULL_IDX) {
          
               if (exp_desc_r->rank) {
                  continue;
               }
            }
            else {

               if (BD_RANK(ATD_ARRAY_IDX(darg_idx)) != exp_desc_r->rank) {
                  continue;
               }
            }
         }
         else if (AT_OBJ_CLASS(darg_idx) == Pgm_Unit) {
            /* not sure this is possible */
         }
      }

      /* if still here, I found it */

      /* only issue usage rec here if overloaded intrinsic opr. */
      /* user defined opers (.opr.) are handled earlier.        */

      if (cif_flags & XREF_RECS &&
          IR_OPR(ir_idx) != Defined_Bin_Opr &&
          IR_OPR(ir_idx) != Defined_Un_Opr) {

         cif_usage_rec(gen_idx, AT_Tbl_Idx, line, col, CIF_Symbol_Reference);
      }

      if (ATP_SCP_IDX(spec_idx) != curr_scp_idx || AT_NOT_VISIBLE(spec_idx)) {

         /* Not visible is checked, because a not visible procedure */
         /* may be referenced via its interface name, even though   */
         /* it cannot be referenced via its own name.               */

         attr_idx = srch_sym_tbl(AT_OBJ_NAME_PTR(spec_idx),
                                 AT_NAME_LEN(spec_idx),
                                 &name_idx);

         if (attr_idx != spec_idx) {

            /* This attr is not in this scope.  It is either host associated */
            /* here, via the interface block, or it is USE_ASSOCIATED, but   */
            /* is not in the local symbol table.                             */

            ADD_ATTR_TO_LOCAL_LIST(spec_idx);
         }
      }

      AT_REFERENCED(spec_idx) = Referenced;

      if (exp_desc_l->reference           &&
          (cif_flags & XREF_RECS) != 0    &&
          xref_state != CIF_No_Usage_Rec) {

         COPY_OPND(tmp_opnd, arg_1_opnd);

         while (OPND_FLD(tmp_opnd)         == IR_Tbl_Idx  &&
                IR_OPR(OPND_IDX(tmp_opnd)) != Struct_Opr) {

            COPY_OPND(tmp_opnd, IR_OPND_L(OPND_IDX(tmp_opnd)));
         }

         find_opnd_line_and_column(&tmp_opnd, &opnd_line, &opnd_column);

         cif_usage_rec(OPND_IDX(tmp_opnd), 
                       OPND_FLD(tmp_opnd), 
                       opnd_line,
                       opnd_column,
                       CIF_Symbol_Defined_Opr_Actual_Arg);
      }

      NTR_IR_LIST_TBL(list1_idx);
      IL_ARG_DESC_VARIANT(list1_idx) = TRUE;
      COPY_OPND(IL_OPND(list1_idx), arg_1_opnd);

      info_idx                               = arg_info_list_base + 1;
      arg_info_list[info_idx]                = init_arg_info;
      arg_info_list[info_idx].ed             = *exp_desc_l;
      arg_info_list[info_idx].maybe_modified = TRUE;
      IL_ARG_DESC_IDX(list1_idx)             = info_idx;
   
      if (num_args == 2) {

         if (exp_desc_r->reference           &&
             (cif_flags & XREF_RECS) != 0    &&
             xref_state != CIF_No_Usage_Rec) {

            COPY_OPND(tmp_opnd, arg_2_opnd);

            while (OPND_FLD(tmp_opnd)         == IR_Tbl_Idx  &&
                   IR_OPR(OPND_IDX(tmp_opnd)) != Struct_Opr) {

               COPY_OPND(tmp_opnd, IR_OPND_L(OPND_IDX(tmp_opnd)));
            }

            find_opnd_line_and_column(&tmp_opnd, &opnd_line, &opnd_column);

            cif_usage_rec(OPND_IDX(tmp_opnd),
                          OPND_FLD(tmp_opnd), 
                          opnd_line,
                          opnd_column,
                          CIF_Symbol_Defined_Opr_Actual_Arg);
         }


         NTR_IR_LIST_TBL(list2_idx);
         IL_ARG_DESC_VARIANT(list2_idx) = TRUE;
         COPY_OPND(IL_OPND(list2_idx), arg_2_opnd);
         IL_NEXT_LIST_IDX(list1_idx) = list2_idx;

         info_idx++;

         arg_info_list[info_idx]                = init_arg_info;
         arg_info_list[info_idx].ed             = *exp_desc_r;
         arg_info_list[info_idx].maybe_modified = TRUE;
         IL_ARG_DESC_IDX(list2_idx)             = info_idx;
      }

      IR_FLD_L(ir_idx)         = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)         = spec_idx;
      IR_LINE_NUM_L(ir_idx)    = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx)     = IR_COL_NUM(ir_idx);
      IR_FLD_R(ir_idx)         = IL_Tbl_Idx;
      IR_IDX_R(ir_idx)         = list1_idx;
      IR_LIST_CNT_R(ir_idx)    = num_args;
      IR_OPR(ir_idx)           = Call_Opr;
      /* set the type to short typeless for now. */
      /* will be changed later.                  */
      IR_TYPE_IDX(ir_idx)      = TYPELESS_DEFAULT_TYPE;

      if (defer_stmt_expansion) {
         number_of_functions++;
      }

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;

      SCP_HAS_CALLS(curr_scp_idx) = TRUE;


      /* If Usage records are not being generated, then don't produce a Call  */
      /* Site record either.  Example:					      */
      /* 								      */
      /*     result = func(arg)						      */
      /* 								      */
      /* where FUNC is a generic identifier pulled in from a module where     */
      /* the specific procedure being called is declared something like       */
      /* 								      */
      /*     FUNCTION func(string) RESULT(char)				      */
      /* 								      */
      /* where CHAR result depends on the value of an expression like	      */
      /* 								      */
      /*     CHARACTER(LEN=SIZE(string%content)) :: char		      */
      /* 								      */
      /* As a part of evaluating FUNC, we don't want to see a Call Site       */
      /* record generated as a part of processing SIZE (it will also have     */
      /* line numbers from the module in its IR tree which are meaningless.   */
      /* See also the cif_call_site_rec call in s_call.c.		      */
   
      if ((cif_flags & MISC_RECS) != 0  &&  xref_state != CIF_No_Usage_Rec) {
         cif_call_site_rec(ir_idx, gen_idx);
      }

      if (AT_OBJ_CLASS(spec_idx)  == Pgm_Unit   &&
          ATP_SCP_ALIVE(spec_idx))              {

         if (ATP_PGM_UNIT(spec_idx)  == Function && 
             !ATP_RSLT_NAME(spec_idx)) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 344, Ansi, IR_COL_NUM(ir_idx));
         }

         if (!ATP_RECURSIVE(spec_idx) && !AT_DCL_ERR(spec_idx) &&
             !on_off_flags.recursive) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 343, Error, IR_COL_NUM(ir_idx));
            *semantically_correct = FALSE;
         }
      }

      if (AT_DCL_ERR(spec_idx)) {
         /* don't do any further processing on this bad boy */

         *semantically_correct = FALSE;
         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
         found = TRUE;
         goto EXIT;
      }

      stmt_expansion_control_start();
      save_defer_stmt_expansion = defer_stmt_expansion;
      defer_stmt_expansion = FALSE;

      if (is_function) {

         /* need to do temp and assign here */

         in_call_list		= save_in_call_list;
         rslt_idx		= ATP_RSLT_IDX(spec_idx);
         (*exp_desc_l)		= init_exp_desc;

         exp_desc_l->type_idx    = ATD_TYPE_IDX(rslt_idx);
         exp_desc_l->type        = TYP_TYPE(exp_desc_l->type_idx);
         exp_desc_l->linear_type = TYP_LINEAR(exp_desc_l->type_idx);
         exp_desc_l->pointer     = ATD_POINTER(rslt_idx);
         exp_desc_l->target      = ATD_TARGET(rslt_idx);
         exp_desc_l->allocatable = ATD_ALLOCATABLE(rslt_idx);
         exp_desc_l->dope_vector = ATD_IM_A_DOPE(rslt_idx);

         IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(rslt_idx);

         if (ATD_ARRAY_IDX(ATP_RSLT_IDX(spec_idx))) {
            exp_desc_l->assumed_shape =
                    (BD_ARRAY_CLASS(ATD_ARRAY_IDX(rslt_idx)) == Assumed_Shape);
            exp_desc_l->assumed_size  =
                    (BD_ARRAY_CLASS(ATD_ARRAY_IDX(rslt_idx)) == Assumed_Size);
            exp_desc_l->rank = BD_RANK(ATD_ARRAY_IDX(rslt_idx));
         }


         if (!no_func_expansion)   {

            flatten_function_call(opnd);

            if (ATP_ELEMENTAL(spec_idx)) {

               attr_idx = find_base_attr(opnd, &line, &col);
               exp_desc_l->rank = BD_RANK(ATD_ARRAY_IDX(attr_idx));
            }


            /* Now that the types for the function result, etc. have been     */
            /* resolved, the Object record that represents the function       */
            /* result can now be output.                                      */

            if ((cif_flags & MISC_RECS) != 0  && 
                xref_state != CIF_No_Usage_Rec) {
               cif_object_rec_for_func_result(spec_idx);
            }

            exp_desc_l->tmp_reference = TRUE;

            if (exp_desc_l->type == Character ||
                exp_desc_l->rank)             {

               attr_idx = find_base_attr(opnd, &line, &col);

               if (exp_desc_l->type == Character) {
                  IR_TYPE_IDX(ir_idx)      = ATD_TYPE_IDX(attr_idx);
                  exp_desc_l->type_idx	   = ATD_TYPE_IDX(attr_idx);
                  exp_desc_l->type	   = TYP_TYPE(exp_desc_l->type_idx);
                  exp_desc_l->linear_type  = TYP_LINEAR(exp_desc_l->type_idx);
                  get_char_len(opnd, &(exp_desc_l->char_len));
               }

               if (exp_desc_l->rank) {
                  get_shape_from_attr(exp_desc_l,
                                      attr_idx,
                                      exp_desc_l->rank,
                                      line,
                                      col);

                  exp_desc_l->contig_array = TRUE;
               }
            }
         }
         else {
            set_shape_for_deferred_funcs(exp_desc_l, ir_idx);
         }

         IR_TYPE_IDX(ir_idx)	= exp_desc_l->type_idx;
         IR_RANK(ir_idx)	= exp_desc_l->rank;
      }

      if (!no_func_expansion)   {

         if (! is_function) {
            /* this was done for functions under flatten_func_call */

            COPY_OPND(tmp_opnd, IR_OPND_R(ir_idx));
            ok = final_arg_work(&tmp_opnd, spec_idx, num_args, NULL) && ok;
            COPY_OPND(IR_OPND_R(ir_idx), tmp_opnd);
         }

         if (ATP_PROC(spec_idx) != Dummy_Proc &&
             ATP_PROC(spec_idx) != Intrin_Proc &&
             ! ATP_VFUNCTION(spec_idx) &&
             (cmd_line_flags.runtime_argument ||
             cmd_line_flags.runtime_arg_call)) {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            list1_idx = IR_IDX_R(ir_idx);
            list2_idx = NULL_IDX;

            idx = 0;

            while (list1_idx) {
               if (IL_FLD(list1_idx) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list1_idx)) == False_Parm_Opr) {

                  false_list_idx = list1_idx;

                  IL_NEXT_LIST_IDX(list2_idx) = NULL_IDX;
                  break;
               }

               list2_idx = list1_idx;
               list1_idx = IL_NEXT_LIST_IDX(list1_idx);
               idx++;
            }

            IR_LIST_CNT_R(ir_idx) = idx;
# endif

            ATP_ARGCHCK_CALL(spec_idx) = TRUE;

            NTR_IR_TBL(loc_idx);
            IR_OPR(loc_idx) = Aloc_Opr;
            IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
            IR_LINE_NUM(loc_idx) = line;
            IR_COL_NUM(loc_idx) = col;
            IR_FLD_L(loc_idx) = AT_Tbl_Idx;

            OPND_FLD(tmp_opnd) = IR_Tbl_Idx;
            OPND_IDX(tmp_opnd) = ir_idx;
            idx = create_argchck_descriptor(&tmp_opnd);
            IR_IDX_L(loc_idx) = idx;
            IR_LINE_NUM_L(loc_idx) = line;
            IR_COL_NUM_L(loc_idx) = col;

            NTR_IR_LIST_TBL(list2_idx);
            IL_ARG_DESC_VARIANT(list2_idx) = TRUE;
            IL_FLD(list2_idx) = IR_Tbl_Idx;
            IL_IDX(list2_idx) = loc_idx;

            if (IR_LIST_CNT_R(ir_idx) == 0) {
               IR_FLD_R(ir_idx) = IL_Tbl_Idx;
               IR_IDX_R(ir_idx) = list2_idx;
               IR_LIST_CNT_R(ir_idx) = 1;
            }
            else {
               list1_idx = IR_IDX_R(ir_idx);
               while (IL_NEXT_LIST_IDX(list1_idx)) {
                  list1_idx = IL_NEXT_LIST_IDX(list1_idx);
               }

               IL_NEXT_LIST_IDX(list1_idx) = list2_idx;
               (IR_LIST_CNT_R(ir_idx))++;
            }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            if (false_list_idx != NULL_IDX) {
               IL_NEXT_LIST_IDX(list2_idx) = false_list_idx;
               list1_idx = false_list_idx;
               while (list1_idx) {
                  (IR_LIST_CNT_R(ir_idx))++;
                  list1_idx = IL_NEXT_LIST_IDX(list1_idx);
               }
            }
# endif
         }
      }

      defer_stmt_expansion = save_defer_stmt_expansion;
      stmt_expansion_control_end(opnd);

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

      found = TRUE;
      break;
   }

EXIT:

   if (ok && found && (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
                       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx)))) {

      if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx))) {

         if (!ATP_PURE(spec_idx) && !ATP_ELEMENTAL(spec_idx)) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 1274, Error, IR_COL_NUM(ir_idx),
                     AT_OBJ_NAME_PTR(spec_idx),
                     "pure or elemental",
                     "pure");

         }
      }
      else if (ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {

         if (!ATP_PURE(spec_idx) && !ATP_ELEMENTAL(spec_idx)) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 1274, Error, IR_COL_NUM(ir_idx),
                     AT_OBJ_NAME_PTR(spec_idx),
                     "pure or elemental",
                     "elemental");

         }
      }

      /* Check to make sure that actual arguments are definable if */
      /* the dummy arg has INTENT(out), INTENT(inout) or POINTER.  */

      list_idx  = IR_IDX_R(ir_idx);

      if (ATP_EXTRA_DARG(spec_idx)) {
         arg_idx        = ATP_FIRST_IDX(spec_idx) + 1;
         idx            = ATP_NUM_DARGS(spec_idx) - 1;
      }
      else {
         arg_idx        = ATP_FIRST_IDX(spec_idx);
         idx            = ATP_NUM_DARGS(spec_idx);
      }
      for (;idx > 0; idx--) {

         if (AT_OBJ_CLASS(SN_ATTR_IDX(arg_idx)) == Data_Obj &&
             (ATD_POINTER(SN_ATTR_IDX(arg_idx)) ||
              ATD_INTENT(SN_ATTR_IDX(arg_idx)) == Intent_Inout ||
              ATD_INTENT(SN_ATTR_IDX(arg_idx)) == Intent_Out)) {
            COPY_OPND(tmp_opnd, IL_OPND(list_idx));
            attr_idx = find_left_attr(&tmp_opnd);

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_PURE(attr_idx)) {
               find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                         &opnd_line,
                                         &opnd_column);
               PRINTMSG(opnd_line, 1273, Error, opnd_column,
                       AT_OBJ_NAME_PTR(attr_idx),
                       AT_OBJ_NAME_PTR(SN_ATTR_IDX(arg_idx)),
                       ATP_PURE(SCP_ATTR_IDX(curr_scp_idx))?"pure":"elemental");
               ok       = FALSE;


            }
         }
         arg_idx++;
         list_idx       = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (found) {

      /* If spec is not equal to gen, that means the names are not the same. */
      /* If the names are not the same, then we didn't actually specify the  */
      /* specific name, so we don't care if it is invisible.                 */

      if (spec_idx == gen_idx && AT_NOT_VISIBLE(spec_idx)) {
          PRINTMSG(IR_LINE_NUM(ir_idx), 486, Error, 
                   IR_COL_NUM(ir_idx),
                   AT_OBJ_NAME_PTR(spec_idx),
                   AT_OBJ_NAME_PTR(AT_MODULE_IDX((spec_idx))));
         *semantically_correct = FALSE;
      }

      switch (expr_mode) {
         case Restricted_Imp_Do_Expr:
         case Data_Stmt_Target_Expr:
            PRINTMSG(IR_LINE_NUM(ir_idx), 62, Error, 
                     IR_COL_NUM(ir_idx),
                     str_word);
            *semantically_correct = FALSE;
            break;

         case Specification_Expr:
            PRINTMSG(IR_LINE_NUM(ir_idx), 880, Error,
                     IR_COL_NUM(ir_idx),
                     str_word);
            *semantically_correct = FALSE;
            break;

         case Stmt_Func_Expr:
            PRINTMSG(IR_LINE_NUM(ir_idx), 757, Error,
                     IR_COL_NUM(ir_idx),
                     str_word);
            *semantically_correct = FALSE;
            break;
      }
   }
   else if (issue_msg) {

      if (gen_idx != NULL_IDX)  {
         PRINTMSG(IR_LINE_NUM(ir_idx), 380, Error, 
                   IR_COL_NUM(ir_idx), str_word);
         *semantically_correct = FALSE;
      }
      else {
      
         if (exp_desc_l->linear_type == Long_Typeless ||
             (num_args == 2 && exp_desc_r->linear_type == Long_Typeless)) {

            if (exp_desc_l->linear_type == Long_Typeless) {
               find_opnd_line_and_column((opnd_type *) &IR_OPND_L(ir_idx),
                                         &opnd_line,
                                         &opnd_column);
               PRINTMSG(opnd_line, 1133, Error, opnd_column);
               *semantically_correct = FALSE;
            }

            if (num_args == 2 &&
                exp_desc_r->linear_type == Long_Typeless) {
               find_opnd_line_and_column((opnd_type *) &IR_OPND_R(ir_idx),
                                         &opnd_line,
                                         &opnd_column);
               PRINTMSG(opnd_line, 1133, Error, opnd_column);
               *semantically_correct = FALSE;
            }
         }
         else if (! is_function) { /* assignment */

            if (exp_desc_r->rank != exp_desc_l->rank && exp_desc_r->rank != 0) {

               /* rank error */

               PRINTMSG(IR_LINE_NUM(ir_idx), 324, Error, IR_COL_NUM(ir_idx),
                        exp_desc_r->rank, exp_desc_l->rank);
               *semantically_correct = FALSE;
            }

            if (err_res) {
               strcpy(type_str_l, get_basic_type_str(exp_desc_l->type_idx));
               strcpy(type_str_r, get_basic_type_str(exp_desc_r->type_idx));

               PRINTMSG(IR_LINE_NUM(ir_idx), 356, Error,
                        IR_COL_NUM(ir_idx),
                        type_str_r,
                        type_str_l);
               *semantically_correct = FALSE;
            }
         }
         else if (expr_mode == Restricted_Imp_Do_Expr ||
                  expr_mode == Data_Stmt_Target_Expr) {

            PRINTMSG(IR_LINE_NUM(ir_idx), 62, Error,
                     IR_COL_NUM(ir_idx), str_word);
            *semantically_correct = FALSE;
         }
         else if (num_args == 1) { /* unary operator */

            PRINTMSG(IR_LINE_NUM(ir_idx), 392, Error,
                     IR_COL_NUM(ir_idx), 
                     get_basic_type_str(exp_desc_l->type_idx),
                     str_word);
            *semantically_correct = FALSE;
         }
         else {
            /* binary operator */

            if (exp_desc_r->rank != exp_desc_l->rank      &&
                exp_desc_r->rank * exp_desc_l->rank != 0) {

               /* rank error */

               PRINTMSG(IR_LINE_NUM(ir_idx), 302, Error, IR_COL_NUM(ir_idx),
                        exp_desc_l->rank, exp_desc_r->rank, str_word);
               *semantically_correct = FALSE;
            }

            if (err_res) {

#ifdef KEY /* Bug 5710, 8094 */
	     /* If we're allowing intrinsic .eq. on logical operands as an
	      * extension, the absence of an overloaded function is not an
	      * error; we'll return to our caller who will use the intrinsic
	      * operator. */
	     if (!((Eq_Opr == IR_OPR(ir_idx) || Ne_Opr == IR_OPR(ir_idx)) &&
	       eq_ne_on_logical(0, exp_desc_l, exp_desc_r))) {
#endif /* KEY Bug 5710, 8094 */

               strcpy(type_str_l, get_basic_type_str(exp_desc_l->type_idx));
               strcpy(type_str_r, get_basic_type_str(exp_desc_r->type_idx));

               PRINTMSG(IR_LINE_NUM(ir_idx), 303, Error,
                        IR_COL_NUM(ir_idx),
                        type_str_l,
                        type_str_r,
                        str_word);
               *semantically_correct = FALSE;
#ifdef KEY /* Bug 5710 */
	     }
#endif /* KEY Bug 5710 */
            }
         }
      }
   }

   if (*semantically_correct &&
       found &&
       ATP_PROC(spec_idx) != Intrin_Proc) {

#ifdef KEY /* Bug 7726 */
      /* Fortran 95 says every elemental procedure is pure */
      if (! (ATP_PURE(spec_idx) || ATP_ELEMENTAL(spec_idx)))
#else /* KEY Bug 7726 */
      if (! ATP_PURE(spec_idx))
#endif /* KEY Bug 7726 */
      {
         if (within_forall_mask_expr) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 1611, Error, IR_COL_NUM(ir_idx), 
                     AT_OBJ_NAME_PTR(spec_idx),
                     "forall scalar-mask-expr");
            *semantically_correct = FALSE;
         }
         else if (within_forall_construct) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 1611, Error, IR_COL_NUM(ir_idx), 
                     AT_OBJ_NAME_PTR(spec_idx),
                     "forall-body-construct");
            *semantically_correct = FALSE;
         }
      }
   }

   if (found) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 399, Comment, IR_COL_NUM(ir_idx),
               str_word, AT_OBJ_NAME_PTR(spec_idx));
   }

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   TRACE (Func_Exit, "resolve_ext_opr", NULL);

   return(found);

}  /* resolve_ext_opr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Return a string for any expression opr.                               *|
|*									      *|
|* Input parameters:							      *|
|*	opr - the operator.                                                   *|
|*									      *|
|* Output parameters:							      *|
|*	str - the string.                                                     *|
|*									      *|
|* Returns:								      *|
|*	length of str							      *|
|*									      *|
\******************************************************************************/

static int  opr_to_str(operator_type	opr,
                       char	       *str)

{
   int	i;
   int  len = 0;

   TRACE (Func_Entry, "opr_to_str", NULL);

   for (i = 0; i < 8; i++) {
      str[i] = '\0';
   }

   switch (opr) {
      case Uplus_Opr  :
         strncpy(str, "+", 1);
         len = 1;
         break;
      case Uminus_Opr :
         strncpy(str, "-", 1);
         len = 1;
         break;
      case Power_Opr  :
         strncpy(str, "**", 2);
         len = 2;
         break;
      case Mult_Opr   :
         strncpy(str, "*", 1);
         len = 1;
         break;
      case Div_Opr    :
         strncpy(str, "/", 1);
         len = 1;
         break;
      case Plus_Opr   :
         strncpy(str, "+", 1);
         len = 1;
         break;
      case Minus_Opr  :
         strncpy(str, "-", 1);
         len = 1;
         break;
      case Concat_Opr :
         strncpy(str, "//", 2);
         len = 2;
         break;
      case Eq_Opr     :
         strncpy(str, "eq", 2);
         len = 2;
         break;
      case Ne_Opr     :
         strncpy(str, "ne", 2);
         len = 2;
         break;
      case Lg_Opr     :
         strncpy(str, "lg", 2);
         len = 2;
         break;
      case Lt_Opr     :
         strncpy(str, "lt", 2);
         len = 2;
         break;
      case Le_Opr     :
         strncpy(str, "le", 2);
         len = 2;
         break;
      case Gt_Opr     :
         strncpy(str, "gt", 2);
         len = 2;
         break;
      case Ge_Opr     :
         strncpy(str, "ge", 2);
         len = 2;
         break;
      case Not_Opr    :
         strncpy(str, "not", 3);
         len = 3;
         break;
      case And_Opr    :
         strncpy(str, "and", 3);
         len = 3;
         break;
      case Or_Opr     :
         strncpy(str, "or", 2);
         len = 2;
         break;
      case Eqv_Opr    :
         strncpy(str, "eqv", 3);
         len = 3;
         break;
      case Neqv_Opr   :
         strncpy(str, "neqv", 4);
         len = 4;
         break;
      case Asg_Opr    :
         strncpy(str, "=", 1);
         len = 1;
         break;
   }

   TRACE (Func_Exit, "opr_to_str", NULL);

   return(len);

}  /* opr_to_str */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	finds the base attr pointer from reference tree.                      *|
|*	The difference between find_base_attr and find_left_attr is:          *|
|*									      *|
|*       a%b%c(1:10)(1:3)                                                     *|
|*									      *|
|*       find_base_attr finds 'c'                                             *|
|*       find_left_attr finds 'a'                                             *|
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

int	find_base_attr(opnd_type       *root_opnd,
                       int	       *line,
                       int	       *col)

{
   int		attr_idx = NULL_IDX;
   opnd_type	opnd;

   TRACE (Func_Entry, "find_base_attr", NULL);

   *line = 0;
   *col  = 0;

   COPY_OPND(opnd, (*root_opnd));

   while (attr_idx == NULL_IDX) {
      switch (OPND_FLD(opnd)) {
         case AT_Tbl_Idx :
            attr_idx = OPND_IDX(opnd);
            *line    = OPND_LINE_NUM(opnd);
            *col     = OPND_COL_NUM(opnd);
            goto EXIT;

         case IR_Tbl_Idx :

            if (IR_OPR(OPND_IDX(opnd)) == Struct_Opr) {
               COPY_OPND(opnd, IR_OPND_R(OPND_IDX(opnd)));
            }
            else {
               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            }
            break;

         case CN_Tbl_Idx :
            *line = OPND_LINE_NUM(opnd);
            *col  = OPND_COL_NUM(opnd);
            goto EXIT;

         default         :
            goto EXIT;
      }
   }
   
EXIT:

   TRACE (Func_Exit, "find_base_attr", ((attr_idx == NULL_IDX) ? NULL :
                                        AT_OBJ_NAME_PTR(attr_idx)));

   return(attr_idx);

}  /* find_base_attr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Find the left most attr in a reference tree.                          *|
|*									      *|
|*	The difference between find_base_attr and find_left_attr is:          *|
|*									      *|
|*       a%b%c(1:10)(1:3)                                                     *|
|*									      *|
|*       find_base_attr finds 'c'                                             *|
|*       find_left_attr finds 'a'                                             *|
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

int	find_left_attr(opnd_type *root_opnd)

{
   int          attr_idx = NULL_IDX;
   opnd_type    opnd;


   TRACE (Func_Entry, "find_left_attr", NULL);

   COPY_OPND(opnd, (*root_opnd));

   while (attr_idx == NULL_IDX) {
      switch (OPND_FLD(opnd)) {
         case AT_Tbl_Idx :
            attr_idx = OPND_IDX(opnd);
            goto EXIT;

         case IR_Tbl_Idx :

            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            break;

         default         :
            goto EXIT;
      }
   }

EXIT:

   TRACE (Func_Exit, "find_left_attr", NULL);

   return(attr_idx);

}  /* find_left_attr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compares reference subtrees to see if they reference the same object. *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      opnd1, opnd2 - the roots of the two trees.                            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE for match.                                                       *|
|*                                                                            *|
\******************************************************************************/

boolean cmp_ref_trees(opnd_type *opnd1,
                      opnd_type *opnd2)

{
   int		column;
   int		line;
   int		list1_idx;
   int		list2_idx;
   boolean	match		= TRUE;


   TRACE (Func_Entry, "cmp_ref_trees", NULL);

   if (OPND_FLD((*opnd1)) != OPND_FLD((*opnd2))) {
      match = FALSE;
   }
   else {
      switch(OPND_FLD((*opnd1))) {
         case NO_Tbl_Idx   :
            match = TRUE;
            break;

         case CN_Tbl_Idx :
         case AT_Tbl_Idx :

            if (OPND_IDX((*opnd1)) == OPND_IDX((*opnd2))) {
               match = TRUE;
            }
            else {
               match = FALSE;
            }
            break;

         case IL_Tbl_Idx :

            if (OPND_LIST_CNT((*opnd1)) == OPND_LIST_CNT((*opnd2))) {
               list1_idx = OPND_IDX((*opnd1));
               list2_idx = OPND_IDX((*opnd2));

               while (list1_idx != NULL_IDX && match) {
                  match = cmp_ref_trees((opnd_type *)&IL_OPND(list1_idx),
                                        (opnd_type *)&IL_OPND(list2_idx));
                  list1_idx = IL_NEXT_LIST_IDX(list1_idx);
                  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               }
            }
            else {
               match = FALSE;
            }
            break;

         case SH_Tbl_Idx :
            find_opnd_line_and_column(opnd1, &line, &column);
            PRINTMSG(line, 963, Internal, column);
            break;

         case IR_Tbl_Idx :

            if (IR_OPR(OPND_IDX((*opnd1))) == IR_OPR(OPND_IDX((*opnd2)))) { 
               match = cmp_ref_trees((opnd_type*)&IR_OPND_L(OPND_IDX((*opnd1))),
                                    (opnd_type*)&IR_OPND_L(OPND_IDX((*opnd2))));
               match = match &&
                     cmp_ref_trees((opnd_type *)&IR_OPND_R(OPND_IDX((*opnd1))),
                                   (opnd_type *)&IR_OPND_R(OPND_IDX((*opnd2))));
            }
            else {
               match = FALSE;
            }
            break;
      }
   }

   TRACE (Func_Exit, "cmp_ref_trees", NULL);

   return(match);

}  /* cmp_ref_trees */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	malloc or realloc the call list arrays.                               *|
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

void enlarge_call_list_tables(void)

{
   int		new_size;

   TRACE (Func_Entry, "enlarge_call_list_tables", NULL);

   /* CALL_LIST_TBL_INC defined in s_utils.m */
   new_size = ((max_call_list_size/CALL_LIST_TBL_INC) + 1)
              * CALL_LIST_TBL_INC;

   if (arg_list_size == 0) {

      /* must do original malloc */

      MEM_ALLOC(arg_list, int, new_size);

   }
   else { /* do realloc */

      MEM_REALLOC(arg_list, int, new_size);

   }

   arg_list_size = new_size;

   TRACE (Func_Exit, "enlarge_call_list_tables", NULL);

   return;

}  /* enlarge_call_list_tables */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Table manager for arg_info_list table.                                *|
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

void enlarge_info_list_table(void)

{
   int          new_size;

   TRACE (Func_Entry, "enlarge_info_list_table", NULL);

   /* CALL_LIST_TBL_INC defined in s_utils.m */
   new_size = arg_info_list_size + ((max_call_list_size/CALL_LIST_TBL_INC) + 1)
              * CALL_LIST_TBL_INC;

   if (arg_info_list_size == 0) {

      /* must do original malloc */

      MEM_ALLOC(arg_info_list, arg_strct_type, new_size);

   }
   else { /* do realloc */

      MEM_REALLOC(arg_info_list, arg_strct_type, new_size);

   }

   arg_info_list_size = new_size;

   TRACE (Func_Exit, "enlarge_info_list_table", NULL);

   return;

}  /* enlarge_info_list_table */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Creates all the dope vector assignments for a ptr assign from a target*|
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

void dope_vector_setup(opnd_type	*r_opnd,
                        expr_arg_type	*exp_desc,
 		        opnd_type	*l_opnd,
		        boolean		 ptr_assign)

{
   act_arg_type	a_type;
   int		attr_idx = NULL_IDX;
   opnd_type	base_opnd;
   int		col;
   int          dim = 1;
   int		dope_idx = NULL_IDX;
   int		dv_idx;
   int		dv2_idx;
   int		i;
   int		line;
   int		list_idx;
   int		loc_idx;
   int		max_idx;
   int		mult_idx;
   opnd_type    opnd;
   int		opnd_column;
   int		opnd_line;
   opnd_type	r_dv_opnd;
   int          rank_idx = NULL_IDX;
   int		stride_idx;
   opnd_type	stride_opnd;
#ifdef KEY /* Bug 10177 */
   int		subscript_idx = 0;
#else /* KEY Bug 10177 */
   int		subscript_idx;
#endif /* KEY Bug 10177 */
   boolean      whole_array;


   TRACE (Func_Entry, "dope_vector_setup", NULL);

   /* This routine expects the left operand to be a dope vector */
   /* reference. Either an attr or a Struct_Opr                 */

    find_opnd_line_and_column(l_opnd, &opnd_line, &opnd_column);

# ifdef _DEBUG

   if (OPND_FLD((*l_opnd)) != AT_Tbl_Idx &&
       (OPND_FLD((*l_opnd)) != IR_Tbl_Idx || 
        IR_OPR(OPND_IDX((*l_opnd))) != Struct_Opr)) {
       PRINTMSG(opnd_line, 624, Internal, opnd_column);
   }
# endif
   /********************\
   |* set BASE address *|
   \********************/


   if (! ptr_assign) {
      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Base_Addr;
      IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = opnd_line;
      IR_COL_NUM(dv_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));
      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)  = Loc_Opr;

      if (exp_desc->type == Character) {
         IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
      }
      else {
         IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      }

      IR_LINE_NUM(loc_idx) = opnd_line;
      IR_COL_NUM(loc_idx)  = opnd_column;

      IR_FLD_R(dv_idx) = IR_Tbl_Idx;
      IR_IDX_R(dv_idx) = loc_idx;
   
      if (exp_desc->rank == 0) {
         COPY_OPND(IR_OPND_L(loc_idx), (*r_opnd));
         just_find_dope_and_rank(r_opnd, &rank_idx, &dope_idx);
      }
      else {
         make_base_subtree(r_opnd, &base_opnd, &rank_idx, &dope_idx);
         COPY_OPND(IR_OPND_L(loc_idx), base_opnd);
      }

# ifdef _TRANSFORM_CHAR_SEQUENCE
# ifdef _TARGET_OS_UNICOS
      if (exp_desc->type == Structure &&
          ATT_CHAR_SEQ(TYP_IDX(exp_desc->type_idx))) {

         IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
         COPY_OPND(opnd, IR_OPND_L(loc_idx));
         transform_char_sequence_ref(&opnd, exp_desc->type_idx);
         COPY_OPND(IR_OPND_L(loc_idx), opnd);
      }
# endif
# endif

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   }
   else {
      just_find_dope_and_rank(r_opnd, &rank_idx, &dope_idx);
   }



   /*************************\
   |* check for whole array *|
   \*************************/

   if (rank_idx != NULL_IDX) {
      attr_idx      = find_base_attr(&IR_OPND_L(rank_idx), &line, &col);

      if (ATD_IM_A_DOPE(attr_idx)) {
         COPY_OPND(r_dv_opnd, IR_OPND_L(IR_IDX_L(rank_idx)));
      }
      subscript_idx = IR_IDX_R(rank_idx);
   }
   else if (exp_desc->rank != 0)              {
      attr_idx    = find_base_attr(r_opnd, &line, &col);
 
      if (ATD_IM_A_DOPE(attr_idx)) {
         COPY_OPND(r_dv_opnd, IR_OPND_L(OPND_IDX((*r_opnd))));
      }
   }
   else {
      find_opnd_line_and_column(r_opnd, &line, &col);
   }

   if (exp_desc->rank > 0 &&
       ! exp_desc->section) {

      whole_array = TRUE;
   }
   else {
      whole_array = FALSE;
   }

   /*************************\
   |* set the a_contig flag *|
   \*************************/

   a_type = get_act_arg_type(exp_desc);

   if (a_type == Array_Ptr ||
       a_type == Array_Tmp_Ptr ||
       a_type == Whole_Ass_Shape ||
       a_type == Dv_Contig_Section) {

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_A_Contig;
      IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = opnd_line;
      IR_COL_NUM(dv_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_A_Contig;
      IR_TYPE_IDX(dv2_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = opnd_line;
      IR_COL_NUM(dv2_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IR_FLD_R(dv_idx) = IR_Tbl_Idx;
      IR_IDX_R(dv_idx) = dv2_idx;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   }
   else if (a_type == Whole_Allocatable ||
            a_type == Whole_Tmp_Allocatable ||
            a_type == Whole_Sequence ||
            a_type == Whole_Tmp_Sequence ||
            a_type == Whole_Array_Constant ||
            a_type == Contig_Section) {

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_A_Contig;
      IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = opnd_line;
      IR_COL_NUM(dv_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));
      IR_FLD_R(dv_idx) = CN_Tbl_Idx;
      IR_IDX_R(dv_idx) = CN_INTEGER_ONE_IDX;
      IR_LINE_NUM_R(dv_idx) = opnd_line;
      IR_COL_NUM_R(dv_idx)  = opnd_column;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
   else {
      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_A_Contig;
      IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = opnd_line;
      IR_COL_NUM(dv_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));
      IR_FLD_R(dv_idx) = CN_Tbl_Idx;
      IR_IDX_R(dv_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_R(dv_idx) = opnd_line;
      IR_COL_NUM_R(dv_idx)  = opnd_column;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
        
   /******************\
   |* set ASSOC flag *|
   \******************/

   NTR_IR_TBL(dv_idx);
   IR_OPR(dv_idx) = Dv_Set_Assoc;
   IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = opnd_line;
   IR_COL_NUM(dv_idx)  = opnd_column;
   COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));
   IR_FLD_R(dv_idx) = CN_Tbl_Idx;
   IR_IDX_R(dv_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(dv_idx) = opnd_line;
   IR_COL_NUM_R(dv_idx)  = opnd_column;

   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


   for (i = 1; i <= exp_desc->rank; i++) {

      /************************************\
      |* set LOW_BOUND for each dimension *|
      \************************************/

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Low_Bound;
      IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = opnd_line;
      IR_COL_NUM(dv_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

      if (whole_array) {
         /* need arrays low bound */
         if (ATD_IM_A_DOPE(attr_idx) &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) != Assumed_Shape) {
            NTR_IR_TBL(dv2_idx);
            IR_OPR(dv2_idx)    = Dv_Access_Low_Bound;
            IR_TYPE_IDX(dv2_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(dv2_idx) = opnd_line;
            IR_COL_NUM(dv2_idx)  = opnd_column;
            COPY_OPND(IR_OPND_L(dv2_idx), r_dv_opnd);
            IR_DV_DIM(dv2_idx) = i;
            IR_FLD_R(dv_idx)   = IR_Tbl_Idx;
            IR_IDX_R(dv_idx)   = dv2_idx;
         }
         else {
            IR_FLD_R(dv_idx) = BD_LB_FLD(ATD_ARRAY_IDX(attr_idx), i);
            IR_IDX_R(dv_idx) = BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), i);
            IR_LINE_NUM_R(dv_idx) = opnd_line;
            IR_COL_NUM_R(dv_idx)  = opnd_column;

            if (IR_FLD_R(dv_idx) == AT_Tbl_Idx) {
               ADD_TMP_TO_SHARED_LIST(IR_IDX_R(dv_idx));
            }
         }
      }
      else {
         /* set to one */
         IR_FLD_R(dv_idx) = CN_Tbl_Idx;
         IR_IDX_R(dv_idx) = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_R(dv_idx) = opnd_line;
         IR_COL_NUM_R(dv_idx)  = opnd_column;
      }

      IR_DV_DIM(dv_idx) = i;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


      /*********************************\
      |* set EXTENT for each dimension *|
      \*********************************/

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Extent;
      IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = opnd_line;
      IR_COL_NUM(dv_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

      NTR_IR_TBL(max_idx);
      IR_OPR(max_idx) = Max_Opr;
      IR_TYPE_IDX(max_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(max_idx) = opnd_line;
      IR_COL_NUM(max_idx)  = opnd_column;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_L(max_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_L(max_idx) = 2;
      IR_IDX_L(max_idx) = list_idx;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = opnd_line;
      IL_COL_NUM(list_idx)  = opnd_column;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx), exp_desc->shape[i-1]);
      IL_LINE_NUM(list_idx) = opnd_line;
      IL_COL_NUM(list_idx) = opnd_column;
 
      IR_FLD_R(dv_idx) = IR_Tbl_Idx;
      IR_IDX_R(dv_idx) = max_idx;
      
      IR_DV_DIM(dv_idx) = i;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      
      /**************************************\
      |* set STRIDE_MULT for each dimension *|
      \**************************************/

      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Set_Stride_Mult;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = opnd_line;
      IR_COL_NUM(dv_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

      if (whole_array) {

         gen_dv_stride_mult(&stride_opnd,
                             attr_idx,
                            &r_dv_opnd,
                             exp_desc,
                             i,
                             opnd_line,
                             opnd_column);

         COPY_OPND(IR_OPND_R(dv_idx), stride_opnd);
      }
      else {
         while (IL_FLD(subscript_idx) != IR_Tbl_Idx ||
                IR_OPR(IL_IDX(subscript_idx)) != Triplet_Opr) {
            subscript_idx = IL_NEXT_LIST_IDX(subscript_idx);
            dim++;
         }

         gen_dv_stride_mult(&stride_opnd,
                             attr_idx,
                            &r_dv_opnd,
                             exp_desc,
                             dim,
                             opnd_line,
                             opnd_column);

         stride_idx = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_L(
                                                      IL_IDX(subscript_idx))));
         mult_idx = gen_ir(OPND_FLD(stride_opnd), OPND_IDX(stride_opnd),
                     Mult_Opr, CG_INTEGER_DEFAULT_TYPE, opnd_line, opnd_column,
                           IL_FLD(stride_idx), IL_IDX(stride_idx));

         IR_FLD_R(dv_idx) = IR_Tbl_Idx;;
         IR_IDX_R(dv_idx) = mult_idx;

         subscript_idx = IL_NEXT_LIST_IDX(subscript_idx);
         dim++;
      }

      IR_DV_DIM(dv_idx) = i;

      gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   }

   /*******************\
   |* clear PTR_ALLOC *|
   \*******************/

   NTR_IR_TBL(dv_idx);
   IR_OPR(dv_idx) = Dv_Set_Ptr_Alloc;
   IR_TYPE_IDX(dv_idx)   = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = opnd_line;
   IR_COL_NUM(dv_idx)  = opnd_column;
   COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

   if (dope_idx != NULL_IDX) {
      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_Ptr_Alloc;
      IR_TYPE_IDX(dv2_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = opnd_line;
      IR_COL_NUM(dv2_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IR_FLD_R(dv_idx) = IR_Tbl_Idx;
      IR_IDX_R(dv_idx) = dv2_idx;
   }
   else {
      IR_FLD_R(dv_idx) = CN_Tbl_Idx;
#ifdef KEY /* Bug 4933 */
      /* If RHS of pointer assignment is a dummy argument, we generally
       * lack dope info for the corresponding formal argument which
       * would tell us whether that formal argument was allocated by
       * pointer. It's too late to change our "Fortran ABI" to pass
       * dope information when a dummy is a pointer target.
       * Optimistically assume that it was allocated by pointer so
       * that subsequent "deallocate" doesn't reject it. */
      IR_IDX_R(dv_idx) = (ptr_assign && NULL_IDX == dope_idx &&
	  AT_Tbl_Idx == OPND_FLD((*r_opnd)) &&
	  AT_IS_DARG(OPND_IDX((*r_opnd)))) ?
        CN_INTEGER_ONE_IDX :
	CN_INTEGER_ZERO_IDX;
#else
      IR_IDX_R(dv_idx) = CN_INTEGER_ZERO_IDX;
#endif /* KEY Bug 4933 */
      IR_LINE_NUM_R(dv_idx) = opnd_line;
      IR_COL_NUM_R(dv_idx)  = opnd_column;
   }

   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /*******************\
   |* clear ORIG_BASE *|
   \*******************/

   NTR_IR_TBL(dv_idx);
   IR_OPR(dv_idx) = Dv_Set_Orig_Base;
   IR_TYPE_IDX(dv_idx)   = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = opnd_line;
   IR_COL_NUM(dv_idx)  = opnd_column;
   COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

   if (dope_idx != NULL_IDX) {
      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_Orig_Base;
      IR_TYPE_IDX(dv2_idx)   = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = opnd_line;
      IR_COL_NUM(dv2_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IR_FLD_R(dv_idx) = IR_Tbl_Idx;
      IR_IDX_R(dv_idx) = dv2_idx;
   }
   else {
      IR_FLD_R(dv_idx) = CN_Tbl_Idx;
      IR_IDX_R(dv_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_R(dv_idx) = opnd_line;
      IR_COL_NUM_R(dv_idx)  = opnd_column;
   }

   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /*******************\
   |* clear ORIG_SIZE *|
   \*******************/

   NTR_IR_TBL(dv_idx);
   IR_OPR(dv_idx) = Dv_Set_Orig_Size;
   IR_TYPE_IDX(dv_idx)   = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = opnd_line;
   IR_COL_NUM(dv_idx)  = opnd_column;
   COPY_OPND(IR_OPND_L(dv_idx), (*l_opnd));

   if (dope_idx != NULL_IDX) {
      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_Orig_Size;
      IR_TYPE_IDX(dv2_idx)   = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = opnd_line;
      IR_COL_NUM(dv2_idx)  = opnd_column;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IR_FLD_R(dv_idx) = IR_Tbl_Idx;
      IR_IDX_R(dv_idx) = dv2_idx;
   }
   else {
      IR_FLD_R(dv_idx) = CN_Tbl_Idx;
      IR_IDX_R(dv_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_R(dv_idx) = opnd_line;
      IR_COL_NUM_R(dv_idx)  = opnd_column;
   }

   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "dope_vector_setup", NULL);

   return;

}  /* dope_vector_setup */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Given the input type, an io type code is assembled.                   *|
|*									      *|
|* Input parameters:							      *|
|*	type_idx - index into type table                                      *|
|*									      *|
|* Output parameters:							      *|
|*	value    - pointer to either a long or a 2 word array of longs.       *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void make_io_type_code(int	     type_idx,   /* BRIANJ */
		       long_type    *value)

{
   long_type	dec_len = 0;
   int          dp_flag = 0;
#ifdef KEY /* Bug 10177 */
   int		dv_type = 0;
#else /* KEY Bug 10177 */
   int		dv_type;
#endif /* KEY Bug 10177 */
   long_type	int_len = 0;
   int		kind_star = 0;

   f90_type_t	*type_code;


   TRACE (Func_Entry, "make_io_type_code", NULL);

   switch(TYP_DESC(type_idx)) {
      case Default_Typed:
         kind_star = DV_DEFAULT_TYPED;
         break;

      case Star_Typed:
         kind_star = DV_STAR_TYPED;
         break;

      case Kind_Typed:
         if (TYP_TYPE(type_idx) == Real &&
             TYP_KIND_DOUBLE(type_idx)) {
            kind_star = DV_KIND_DOUBLE;
         }
         else if (TYP_KIND_CONST(type_idx)) {
            kind_star = DV_KIND_CONST;
         }
         else {
            kind_star = DV_KIND_TYPED;
         }
         break;
   }

# ifndef _TARGET_OS_MAX
   if (TYP_DECLARED_DBL(type_idx) &&
       kind_star == DV_DEFAULT_TYPED) {

      dp_flag = 1;
   }
# endif

   switch (TYP_TYPE(type_idx)) {
      case Typeless:

         /* BRIANJ - These could be long64 type */

         dec_len = (long) TYP_BIT_LEN(type_idx) / TARGET_BYTES_PER_WORD;
         int_len = (long) TYP_BIT_LEN(type_idx);
         dv_type = DV_TYPELESS;

         break;

      case Integer:

         dec_len = (long) TYP_DCL_VALUE(type_idx);
         int_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
         dv_type = DV_INTEGER;

         break;

      case Logical:

         dec_len = (long) TYP_DCL_VALUE(type_idx);
         int_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
         dv_type = DV_LOGICAL;

         break;

      case Real:

         dec_len = (long) TYP_DCL_VALUE(type_idx);
         int_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
         dv_type = DV_REAL;

         break;

      case Complex:

         dec_len = (long) TYP_DCL_VALUE(type_idx);
         int_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
         dv_type = DV_COMPLEX;

         break;

      case Character:

         if (kind_star == DV_DEFAULT_TYPED) {
            dec_len = 0;
         }
         else {
            dec_len = 1;
         }
         int_len = 8;
         dv_type = DV_ASCII_CHAR;

         break;

      case Structure:

         if (ATT_CHAR_SEQ(TYP_IDX(type_idx))) {
            dv_type = DV_ASCII_CHAR_SEQUENCE_STRUCT;
         }
         else {
            dv_type = DV_STRUCT;
         }

         break;

      case CRI_Ptr:
      case CRI_Ch_Ptr:

         int_len = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
         dv_type = DV_INTEGER;

         break;
   }

# ifdef _TYPE_CODE_64_BIT
   type_code = (f90_type_t *)value;

   type_code->unused = 0;
   type_code->type = dv_type;
   type_code->dpflag = dp_flag;
   type_code->kind_or_star = kind_star;
   type_code->int_len = int_len;
   type_code->dec_len = dec_len;
# else

   *value = ((dv_type   << DV_TYPE_SHIFT)         |
             (dp_flag   << DV_DP_SHIFT)           |
             (kind_star << DV_KIND_STAR_SHIFT)  |
             (int_len   << DV_INT_LEN_SHIFT)      |
             (dec_len   << DV_DEC_LEN_SHIFT));
# endif

   TRACE (Func_Exit, "make_io_type_code", NULL);

   return;

}  /* make_io_type_code */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine creates a constant table entry for a dope vector type    *|
|*      code.                                                                 *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx - index for attr.                                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	constant table idx for type code.                                     *|
|*									      *|
\******************************************************************************/

static int create_dv_type_code(int	attr_idx)

{
   int		constant_idx = NULL_IDX;
   long_type    constant[2];

   TRACE (Func_Entry, "create_dv_type_code", NULL);

   make_io_type_code(ATD_TYPE_IDX(attr_idx), constant);

   constant_idx = ntr_const_tbl(IO_TYPE_CODE_TYPE, FALSE, constant);

   TRACE (Func_Exit, "create_dv_type_code", NULL);

   return(constant_idx);

}  /* create_dv_type_code */

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

void gen_common_dv_init(opnd_type            *dv_opnd,
                        int                  dv_attr_idx,
                        sh_position_type     position)

{
   int          	col;
   int			ir_idx;
   size_offset_type	length;
   int			line;
   int			mult_idx;
   size_offset_type	result;
   int          	type_idx;


   TRACE (Func_Entry, "gen_common_dv_init", NULL);

   find_opnd_line_and_column(dv_opnd, &line, &col);

   /*************\
   |* BASE ADDR *|
   \*************/

   /* Do not set */

   /*************\
   |* EL_LEN    *|
   \*************/

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Set_El_Len;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   COPY_OPND(IR_OPND_L(ir_idx), (*dv_opnd));

   type_idx = ATD_TYPE_IDX(dv_attr_idx);

   if (TYP_TYPE(type_idx) == Structure) {
      IR_FLD_R(ir_idx)	= (fld_type) ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
      IR_IDX_R(ir_idx)	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
      IR_LINE_NUM_R(ir_idx)	= line;
      IR_COL_NUM_R(ir_idx)	= col;
   }
   else if (TYP_TYPE(type_idx) == Character) {

      IR_FLD_R(ir_idx)      = TYP_FLD(type_idx);
      IR_IDX_R(ir_idx)      = TYP_IDX(type_idx);
      IR_LINE_NUM_R(ir_idx) = line;
      IR_COL_NUM_R(ir_idx)  = col;

      if (IR_FLD_R(ir_idx) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(IR_IDX_R(ir_idx));
      }

      if (! char_len_in_bytes) {

         /* Len is in bytes on solaris */
         /* Len is in bits for everyone else */

         if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
            result.fld		= CN_Tbl_Idx;
            result.idx		= CN_INTEGER_CHAR_BIT_IDX;
            length.fld		= TYP_FLD(type_idx);
            length.idx		= TYP_IDX(type_idx);

            size_offset_binary_calc(&length,
                                    &result,
                                     Mult_Opr,
                                    &result);

            if (result.fld == NO_Tbl_Idx) {
               IR_FLD_R(ir_idx)       = CN_Tbl_Idx;
               IR_IDX_R(ir_idx)       = ntr_const_tbl(result.type_idx,
                                                      FALSE,
                                                      result.constant);
            }
            else {
               IR_FLD_R(ir_idx)       = result.fld;
               IR_IDX_R(ir_idx)       = result.idx;
            }

            IR_LINE_NUM_R(ir_idx) = line;
            IR_COL_NUM_R(ir_idx)  = col;
         }
         else {
            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx) = Mult_Opr;
            IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx) = line;
            IR_COL_NUM(mult_idx)  = col;
            IR_FLD_L(mult_idx)    = CN_Tbl_Idx;
            IR_IDX_L(mult_idx)    = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 8);
            IR_LINE_NUM_L(mult_idx) = line;
            IR_COL_NUM_L(mult_idx)  = col;

            IR_FLD_R(mult_idx)    = TYP_FLD(type_idx);
            IR_IDX_R(mult_idx)    = TYP_IDX(type_idx);
            IR_LINE_NUM_R(mult_idx) = line;
            IR_COL_NUM_R(mult_idx)  = col;

            IR_FLD_R(ir_idx)      = IR_Tbl_Idx;
            IR_IDX_R(ir_idx)      = mult_idx;
         }
      }
   }
   else {
      IR_FLD_R(ir_idx) = CN_Tbl_Idx;
      IR_IDX_R(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                  storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      IR_LINE_NUM_R(ir_idx) = line;
      IR_COL_NUM_R(ir_idx)  = col;
   }

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }



   /*************\
   |* ASSOC     *|
   \*************/

   /* Do not set */

   /*************\
   |* PTR_ALLOC *|
   \*************/

   /* Do not set */

   /*************\
   |* P_OR_A    *|
   \*************/

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Set_P_Or_A;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   COPY_OPND(IR_OPND_L(ir_idx), (*dv_opnd));

   IR_FLD_R(ir_idx) = CN_Tbl_Idx;

   if (ATD_ALLOCATABLE(dv_attr_idx)) {
      IR_IDX_R(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 2);
   }
   else if (ATD_POINTER(dv_attr_idx)) {
      IR_IDX_R(ir_idx) = CN_INTEGER_ONE_IDX;
   }
   else {
      IR_IDX_R(ir_idx) = CN_INTEGER_ZERO_IDX;
   }
   IR_LINE_NUM_R(ir_idx) = line;
   IR_COL_NUM_R(ir_idx)  = col;

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }


   /*************\
   |* A_CONTIG  *|
   \*************/

   /* if it is in common block, this bit is left untouched */
   if (!ATD_IN_COMMON(dv_attr_idx))
   {
   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Set_A_Contig;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   COPY_OPND(IR_OPND_L(ir_idx), (*dv_opnd));

   IR_FLD_R(ir_idx) = CN_Tbl_Idx;

   if (ATD_ALLOCATABLE(dv_attr_idx)) {
      IR_IDX_R(ir_idx) = CN_INTEGER_ONE_IDX;
   }
   else {
      IR_IDX_R(ir_idx) = CN_INTEGER_ZERO_IDX;
   }
   IR_LINE_NUM_R(ir_idx) = line;
   IR_COL_NUM_R(ir_idx)  = col;

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
   }


   /*************\
   |* N_DIM     *|
   \*************/

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) =Dv_Set_N_Dim ;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   COPY_OPND(IR_OPND_L(ir_idx), (*dv_opnd));

   IR_FLD_R(ir_idx) = CN_Tbl_Idx;
   IR_IDX_R(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                  (ATD_ARRAY_IDX(dv_attr_idx) ? 
                                   BD_RANK(ATD_ARRAY_IDX(dv_attr_idx)) : 0));
   IR_LINE_NUM_R(ir_idx) = line;
   IR_COL_NUM_R(ir_idx)  = col;

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }


   /*************\
   |* TYPE_CODE *|
   \*************/

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Set_Typ_Code;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   COPY_OPND(IR_OPND_L(ir_idx), (*dv_opnd));

   IR_FLD_R(ir_idx) = CN_Tbl_Idx;
   IR_IDX_R(ir_idx) = create_dv_type_code(dv_attr_idx);
   IR_LINE_NUM_R(ir_idx) = line;
   IR_COL_NUM_R(ir_idx)  = col;

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }


   /*************\
   |* ORIG_BASE *|
   \*************/

   /* Do not set */

   /*************\
   |* ORIG_SIZE *|
   \*************/

   /* Do not set */

   TRACE (Func_Exit, "gen_common_dv_init", NULL);

   return;

}  /* gen_common_dv_init */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Create a whole def of a dope vector that is in a module block.        *|
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

void gen_static_dv_whole_def(opnd_type         *dv_opnd,
                             int                attr_idx,
			     sh_position_type	position)

{
   int          	col;
   long_type		constant[2];
   int			const_idx;
   ext_dope_type	*dv_ptr;
   int			ir_idx;
   int			i;
   int          	line;
   int			mult_idx;
   int			num_words;
   long_type         	rank;  /* BRIANJ */
   int                  type_idx;


   TRACE (Func_Entry, "gen_static_dv_whole_def", NULL);

   find_opnd_line_and_column(dv_opnd, &line, &col);

   rank	= (ATD_ARRAY_IDX(attr_idx) ? (long)BD_RANK(ATD_ARRAY_IDX(attr_idx)) :0);

   num_words	= DV_HD_WORD_SIZE + (rank * DV_DIM_WORD_SIZE);

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
#if defined (TARG_X8664) && defined (_HOST64)
   TYP_BIT_LEN(TYP_WORK_IDX)    = num_words * ((SET_POINTER_SIZE)?64:32);
#else
   TYP_BIT_LEN(TYP_WORK_IDX)	= num_words * TARGET_BITS_PER_WORD;
#endif
   type_idx			= ntr_type_tbl();

   const_idx	= ntr_const_tbl(type_idx, FALSE, NULL);

   /* NULL() intrinsic */
   if (ATD_CLASS(attr_idx) == Compiler_Tmp) {
      ATD_FLD(attr_idx) = CN_Tbl_Idx;
      ATD_TMP_IDX(attr_idx) = const_idx;
      ATD_TMP_INIT_NOT_DONE(attr_idx) = TRUE;
   }
   else {
      gen_init_stmt(dv_opnd,
                    const_idx,
                    position);
   }

   dv_ptr = (ext_dope_type *)&CN_CONST(const_idx);
   type_idx = ATD_TYPE_IDX(attr_idx);

   /* the entire constant is initialized to 0's */
   /* so just fill in the non zero parts.       */

   /*************\
   |* EL_LEN    *|
   \*************/

   if (TYP_TYPE(type_idx) == Structure) {

      if (compare_cn_and_value(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx)),
                               MAX_DV_EL_LEN,
                               Ge_Opr)) {
         PRINTMSG(line, 1174, Error, col,
                  CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx))),
                  MAX_DV_EL_LEN);
         DV_SET_EL_LEN(*dv_ptr, MAX_DV_EL_LEN);
      }
      else {  /* BRIANJ */
         DV_SET_EL_LEN(*dv_ptr,
                CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx))));
      }
   }
   else if (TYP_TYPE(type_idx) == Character) {

      if (TYP_FLD(type_idx) == CN_Tbl_Idx) {

         if (char_len_in_bytes) {

            if (compare_cn_and_value(TYP_IDX(type_idx),
                                     MAX_DV_EL_LEN,
                                     Ge_Opr)) {
               PRINTMSG(line, 1174, Error, col, 
                        CN_INT_TO_C(TYP_IDX(type_idx)),
                        MAX_DV_EL_LEN);
               DV_SET_EL_LEN(*dv_ptr, MAX_DV_EL_LEN);
            }
            else {  /* BRIANJ */
               DV_SET_EL_LEN(*dv_ptr, CN_INT_TO_C(TYP_IDX(type_idx)));
            }
         }
         else {

            if (compare_cn_and_value(TYP_IDX(type_idx),
                                     MAX_DV_EL_LEN/8,
                                     Ge_Opr)) {
               PRINTMSG(line, 1174, Error, col, 
                        CN_INT_TO_C(TYP_IDX(type_idx)),
                        MAX_DV_EL_LEN/8);
               DV_SET_EL_LEN(*dv_ptr, MAX_DV_EL_LEN);
            }
            else {  /* BRIANJ */
               DV_SET_EL_LEN(*dv_ptr, CN_INT_TO_C(TYP_IDX(type_idx)) * 8);
            }
         }
      }
      else {
         /* We are here only for variable length char pointers */
         /* They cannot be inside a derived type, so just generate */
         /* an assignment statement to fill in the length at runtime. */

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Dv_Set_El_Len;
         IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx) = col;

         COPY_OPND(IR_OPND_L(ir_idx), (*dv_opnd));

         if (char_len_in_bytes) {

            /* Len is in bytes for solaris */
            IR_FLD_R(ir_idx)      = TYP_FLD(type_idx);
            IR_IDX_R(ir_idx)      = TYP_IDX(type_idx);
            IR_LINE_NUM_R(ir_idx) = line;
            IR_COL_NUM_R(ir_idx)  = col;
         }
         else {
            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx) = Mult_Opr;
            IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx) = line;
            IR_COL_NUM(mult_idx)  = col;
            IR_FLD_L(mult_idx)    = CN_Tbl_Idx;
            IR_IDX_L(mult_idx)    = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 8);
            IR_LINE_NUM_L(mult_idx) = line;
            IR_COL_NUM_L(mult_idx)  = col;

            IR_FLD_R(mult_idx)    = TYP_FLD(type_idx);
            IR_IDX_R(mult_idx)    = TYP_IDX(type_idx);
            IR_LINE_NUM_R(mult_idx) = line;
            IR_COL_NUM_R(mult_idx)  = col;

            IR_FLD_R(ir_idx)      = IR_Tbl_Idx;
            IR_IDX_R(ir_idx)      = mult_idx;
         }

         gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         if (position == After) {
            SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         }
         else {
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         }
      }
   }
   else {
      DV_SET_EL_LEN(*dv_ptr, storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
   }

   /*************\
   |* P_OR_A    *|
   \*************/

   if (ATD_ALLOCATABLE(attr_idx)) {
      DV_SET_P_OR_A(*dv_ptr, 2);
   }
   else if (ATD_POINTER(attr_idx)) {
      DV_SET_P_OR_A(*dv_ptr, 1);
   }

   /*************\
   |* N_DIM     *|
   \*************/

   DV_SET_NUM_DIMS(*dv_ptr, rank);

   /*************\
   |* TYPE_CODE *|
   \*************/

   make_io_type_code(type_idx, constant);
# ifdef _TYPE_CODE_64_BIT
   DV_SET_TYPE_CODE(*dv_ptr, *(f90_type_t *)constant);
# else
   DV_SET_TYPE_CODE(*dv_ptr, *constant);
# endif

   if (cmd_line_flags.runtime_bounds &&
       ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

      for (i = 0; i < BD_RANK(ATD_ARRAY_IDX(attr_idx)); i++) {

         /************************************\
         |* set LOW_BOUND for each dimension *|
         \************************************/

         DV_SET_LOW_BOUND(*dv_ptr, i, 1);

         /*********************************\
         |* set EXTENT for each dimension *|
         \*********************************/

         /* leave as zero */

         /**************************************\
         |* set STRIDE_MULT for each dimension *|
         \**************************************/

         DV_SET_STRIDE_MULT(*dv_ptr, i, 1);

      }
   }

#ifdef KEY /* Bug 9608 */
   /*
    * When we set assoc=0 for an array, we also set contig=1 so that
    * copyinout doesn't blow up if user (illegally) passes the null
    * pointer to a procedure lacking an explicit interface, in the
    * (unjustified) expectation that the pointer won't be
    * dereferenced if the procedure doesn't refer to the dummy
    * argument. This seems cheaper than adding a test for null
    * before and after every call.
    */
   if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX && !DV_ASSOC(*dv_ptr)) {
     DV_SET_A_CONTIG(*dv_ptr, 1);
   }
#endif /* KEY Bug 9608 */

   TRACE (Func_Exit, "gen_static_dv_whole_def", NULL);

   return;

}  /* gen_static_dv_whole_def */

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

static long64 create_imp_do_loops(opnd_type	*top_opnd)

{

   int			col;
   long64		count = 1;
   long64		end;
   int			i;
   int			imp_idx;	
   int			line;
   int			list_idx;
   opnd_type		opnd;
   long64		start;
   int			tmp_idx;
   int			trip_list_idx;


   TRACE (Func_Entry, "create_imp_do_loops", NULL);

   COPY_OPND(opnd, (*top_opnd));
   find_opnd_line_and_column(&opnd, &line, &col);

   while (OPND_FLD(opnd) == IR_Tbl_Idx) {

      if (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr) {

         trip_list_idx = IR_IDX_R(OPND_IDX(opnd));

         for (i = 0; i < IR_LIST_CNT_R(OPND_IDX(opnd)); i++) {

            NTR_IR_TBL(imp_idx);
            IR_OPR(imp_idx)        = Implied_Do_Opr;
            IR_TYPE_IDX(imp_idx)   = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(imp_idx)   = line;
            IR_COL_NUM(imp_idx)    = col;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_L(imp_idx)      = IL_Tbl_Idx;
            IR_LIST_CNT_L(imp_idx) = 1;
            IR_IDX_L(imp_idx)      = list_idx;

            COPY_OPND(IL_OPND(list_idx), (*top_opnd));
            OPND_FLD((*top_opnd)) = IR_Tbl_Idx;
            OPND_IDX((*top_opnd)) = imp_idx;

            /* create the tmp implied do control variable. */

            tmp_idx                   = gen_compiler_tmp(line, col, Priv, TRUE);
            ATD_TYPE_IDX(tmp_idx)     = CG_INTEGER_DEFAULT_TYPE;
            AT_SEMANTICS_DONE(tmp_idx)= TRUE;
            ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
            ATD_IMP_DO_LCV(tmp_idx)   = TRUE;
            ATD_LCV_IS_CONST(tmp_idx) = TRUE;

            /* hook in control var. */

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(imp_idx)      = IL_Tbl_Idx;
            IR_LIST_CNT_R(imp_idx) = 4;
            IR_IDX_R(imp_idx)      = list_idx;

            IL_FLD(list_idx)   = AT_Tbl_Idx;
            IL_IDX(list_idx)   = tmp_idx;
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            /* second is start opnd */

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            COPY_OPND(IL_OPND(list_idx),
                      IL_OPND(IR_IDX_L(IL_IDX(trip_list_idx))));

            start = CN_INT_TO_C(IL_IDX(list_idx));

            /* third is end opnd */

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            COPY_OPND(IL_OPND(list_idx),
                      IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_L(
                               IL_IDX(trip_list_idx)))));

            end = CN_INT_TO_C(IL_IDX(list_idx));

            count = count * ((end - start) + 1);

            /* fourth is stride opnd */

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            COPY_OPND(IL_OPND(list_idx),
                      IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                    IR_IDX_L(IL_IDX(trip_list_idx))))));


            /* replace triplet with tmp control variable */

            IL_FLD(trip_list_idx) = AT_Tbl_Idx;
            IL_IDX(trip_list_idx) = tmp_idx;
            IL_LINE_NUM(trip_list_idx) = line;
            IL_COL_NUM(trip_list_idx)  = col;

            trip_list_idx = IL_NEXT_LIST_IDX(trip_list_idx);
         }
      }

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }


   TRACE (Func_Exit, "create_imp_do_loops", NULL);

   return(count);

}  /* create_imp_do_loops */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine creates a chain of stmts to initialize a dope vector     *|
|*      or a structure with pointers.                                         *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx - idx of variable to process.                                *|
|*									      *|
|* Output parameters:							      *|
|*      exit_sh_idx - exit code chain if needed.                              *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void gen_entry_dope_code(int	 attr_idx)

{
   expr_arg_type exp_desc;
   void          (*func)();
   opnd_type     opnd;
   int		 opr;


   TRACE (Func_Entry, "gen_entry_dope_code", NULL);

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
      func = gen_static_dv_whole_def;
      opr = Init_Opr;
   }
   else if (ATD_AUTOMATIC(attr_idx) ||
            ATD_CLASS(attr_idx) == Function_Result) {
      func = gen_dv_whole_def_init;
      opr = Asg_Opr;
   }
   else if (ATD_IN_COMMON(attr_idx)) {

# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      func = gen_common_dv_init;
      opr = Init_Opr;
# else
      func = gen_static_dv_whole_def;
      opr = Init_Opr;
# endif
   }
   else if (ATD_SAVED(attr_idx) ||
#ifdef KEY /* Bug 10467 */
            ATD_DATA_INIT(attr_idx) ||
#endif /* KEY Bug 10467 */
            ATP_SAVE_ALL(SCP_ATTR_IDX(curr_scp_idx))) {
      func = gen_static_dv_whole_def;
      opr = Init_Opr;
   }
   else {
      func = gen_dv_whole_def_init;
      opr = Asg_Opr;
   }

   if (AT_DCL_ERR(attr_idx)) {
      goto EXIT;
   }

# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module &&
       ATD_IN_COMMON(attr_idx)) {

      /* intentionally blank. We can't initialize common block */
      /* dope vectors from multiple .o's on solaris.           */
   }
   else 
# endif
   if (ATD_IM_A_DOPE(attr_idx)) {
      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_IDX(opnd) = attr_idx;
      OPND_LINE_NUM(opnd) = SH_GLB_LINE(curr_stmt_sh_idx);
      OPND_COL_NUM(opnd)  = SH_COL_NUM(curr_stmt_sh_idx);
      (*func)(&opnd, attr_idx, After);
   }
   else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure           &&
            (ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ||
#ifdef KEY /* Bug 6845 */
            ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ||
#endif /* KEY Bug 6845 */
             ATT_DEFAULT_INITIALIZED(TYP_IDX(ATD_TYPE_IDX(attr_idx))))  &&
            ! AT_DCL_ERR(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {

      OPND_FLD(opnd)      = AT_Tbl_Idx;
      OPND_IDX(opnd)      = attr_idx;
      OPND_LINE_NUM(opnd) = SH_GLB_LINE(curr_stmt_sh_idx);
      OPND_COL_NUM(opnd)  = SH_COL_NUM(curr_stmt_sh_idx);

# if defined(_TARGET_OS_MAX)
      if (ATD_ARRAY_IDX(attr_idx) ||
          ATD_PE_ARRAY_IDX(attr_idx))
# else
      if (ATD_ARRAY_IDX(attr_idx))
# endif
                                    {
         gen_whole_subscript(&opnd, &exp_desc);
      }

      process_cpnt_inits(&opnd, 
                         TYP_IDX(ATD_TYPE_IDX(attr_idx)),
                         func,
			 opr,
                         After);
   }

EXIT:

   TRACE (Func_Exit, "gen_entry_dope_code", NULL);

   return;

}  /* gen_entry_dope_code */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	recursively go through all components of a structure to look for      *|
|*      pointers. Then call the supplied routine func for processing.         *|
|*									      *|
|* Input parameters:							      *|
|*	left_opnd - current base of sub-object reference.                     *|
|*      type_idx  - defined type attr.                                        *|
|*      func      - function to call for processing.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/


void process_cpnt_inits(opnd_type  	*left_opnd,
			int        	type_idx,
			void	  	(*func)(),
			int		opr,
			sh_position_type	position)

{
   int		 attr_idx;
   opnd_type	 cn_opnd;
   int		 col;
#ifdef KEY /* Bug 10177 */
   int		 const_idx = 0;
#else /* KEY Bug 10177 */
   int		 const_idx;
#endif /* KEY Bug 10177 */
   expr_arg_type exp_desc;
   int		 i;
   int		 init_idx;
   int		 ir_idx;
   int		 line;
   int		 list_idx;
   boolean	 need_loops = FALSE;
   opnd_type     opnd;
   int           placeholder_sh_idx = NULL_IDX;
   int           save_curr_stmt_sh_idx;
   int		 save_target_array_idx;
   int		 sub_idx;
   int		 sn_idx;
   int		 tmp_idx;
   opnd_type	 tmp_opnd;

   TRACE (Func_Entry, "process_cpnt_inits", NULL);

   find_opnd_line_and_column(left_opnd, &line, &col);

# ifdef _DEBUG
   if (opr != Asg_Opr &&
       opr != Init_Opr) {
      PRINTMSG(line, 626, Internal, col,
               "Asg_Opr or Init_Opr", "process_cpnt_inits");
   }
# endif

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   if (position == After) {
      save_curr_stmt_sh_idx = SH_NEXT_IDX(save_curr_stmt_sh_idx);
   }

# if defined(_GEN_LOOPS_FOR_DV_WHOLE_DEF)
   if (func == (void (*)())gen_dv_whole_def_init ||
       func == (void (*)())gen_dv_whole_def ||
       func == (void (*)())gen_sf_dv_whole_def) {

      need_loops = TRUE;
   }
# endif

   if (ATT_DEFAULT_INITIALIZED(type_idx) &&
       opr == Asg_Opr) {
      need_loops = TRUE;
   }

   if (need_loops) {
      gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      if (position == Before) {
         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      }
      placeholder_sh_idx = curr_stmt_sh_idx;

      gen_dv_def_loops(left_opnd);

# ifdef _DEBUG
      if (placeholder_sh_idx != curr_stmt_sh_idx) {
         PRINTMSG(line, 626, Internal, col,
                  "placeholder_sh_idx == curr_stmt_sh_idx",
                  "process_cpnt_inits");
      }
# endif
   }

   sn_idx = ATT_FIRST_CPNT_IDX(type_idx);

   while (sn_idx != NULL_IDX) {
      attr_idx = SN_ATTR_IDX(sn_idx);

#ifdef KEY /* Bug 6845 */
      if (ATD_POINTER(attr_idx) || ATD_ALLOCATABLE(attr_idx))
#else /* KEY Bug 6845 */
      if (ATD_POINTER(attr_idx))
#endif /* KEY Bug 6845 */
      {
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Struct_Opr;
         IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx)  = col;
         COPY_OPND(IR_OPND_L(ir_idx), (*left_opnd));
         IR_FLD_R(ir_idx) = AT_Tbl_Idx;
         IR_IDX_R(ir_idx) = attr_idx;
         IR_LINE_NUM_R(ir_idx) = line;
         IR_COL_NUM_R(ir_idx)  = col;
         OPND_FLD(opnd) = IR_Tbl_Idx;
         OPND_IDX(opnd) = ir_idx;

         if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
             IR_RANK(ir_idx) = IR_RANK(IR_IDX_L(ir_idx));
         }

         (*func)(&opnd, attr_idx, position);
      }
      else if (ATD_CPNT_INIT_IDX(attr_idx) != NULL_IDX) {

         NTR_IR_TBL(ir_idx);

         IR_OPR(ir_idx)         = Struct_Opr;
         IR_TYPE_IDX(ir_idx)    = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(ir_idx)    = line;
         IR_COL_NUM(ir_idx)     = col;

         COPY_OPND(IR_OPND_L(ir_idx), (*left_opnd));

         IR_FLD_R(ir_idx)       = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)       = attr_idx;
         IR_LINE_NUM_R(ir_idx)  = line;
         IR_COL_NUM_R(ir_idx)   = col;

         if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
             IR_RANK(ir_idx)    = IR_RANK(IR_IDX_L(ir_idx));
         }

         gen_opnd(&opnd, ir_idx, IR_Tbl_Idx, line, col);

         if (opr == Asg_Opr) {

            if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
               exp_desc = init_exp_desc;
               gen_whole_subscript(&opnd, &exp_desc);
            }
            else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
               gen_whole_substring(&opnd, 0);
            }

            NTR_IR_TBL(init_idx);

            IR_OPR(init_idx)       = Asg_Opr;
            IR_LINE_NUM(init_idx)  = line;
            IR_COL_NUM(init_idx)   = col;
            IR_TYPE_IDX(init_idx)  = ATD_TYPE_IDX(attr_idx);
            COPY_OPND(IR_OPND_L(init_idx), opnd);
            IR_LINE_NUM_L(init_idx)= line;
            IR_COL_NUM_L(init_idx) = col;


            IR_IDX_R(init_idx)       = ATD_CPNT_INIT_IDX(attr_idx);
            IR_FLD_R(init_idx)       = (fld_type) ATD_FLD(attr_idx);
            IR_LINE_NUM_R(init_idx)  = line;
            IR_COL_NUM_R(init_idx)   = col;

            gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

            if (position == After) {
               SH_IR_IDX(curr_stmt_sh_idx) = init_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            }
            else {
               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = init_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
            }
         }
         else {
            /* Init_Opr */

            if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
               NTR_IR_TBL(sub_idx);
               IR_OPR(sub_idx) = Subscript_Opr;
               IR_TYPE_IDX(sub_idx) = ATD_TYPE_IDX(attr_idx);
               IR_LINE_NUM(sub_idx) = line;
               IR_COL_NUM(sub_idx) = col;

               COPY_OPND(IR_OPND_L(sub_idx), opnd);

               NTR_IR_LIST_TBL(list_idx);
               IR_FLD_R(sub_idx) = IL_Tbl_Idx;
               IR_IDX_R(sub_idx) = list_idx;
               IR_LIST_CNT_R(sub_idx) = 1;

               IL_FLD(list_idx) = BD_LB_FLD(ATD_ARRAY_IDX(attr_idx),1);
               IL_IDX(list_idx) = BD_LB_IDX(ATD_ARRAY_IDX(attr_idx),1);
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx) = col;

               for (i = 2; i<= BD_RANK(ATD_ARRAY_IDX(attr_idx)); i++) {
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);

                  IR_LIST_CNT_R(sub_idx) += 1;

                  IL_FLD(list_idx) = BD_LB_FLD(ATD_ARRAY_IDX(attr_idx),i);
                  IL_IDX(list_idx) = BD_LB_IDX(ATD_ARRAY_IDX(attr_idx),i);
                  IL_LINE_NUM(list_idx) = line;
                  IL_COL_NUM(list_idx) = col;
               }

               gen_opnd(&opnd, sub_idx, IR_Tbl_Idx, line, col);
            }

            if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
               gen_whole_substring(&opnd, 0);
            }

            if (ATD_FLD(attr_idx) != CN_Tbl_Idx) {

               gen_opnd(&tmp_opnd, ATD_CPNT_INIT_IDX(attr_idx),
                        (fld_type) ATD_FLD(attr_idx), line, col);

               tmp_idx = find_left_attr(&tmp_opnd);

               if (ATD_FLD(tmp_idx) == CN_Tbl_Idx) {
                  const_idx = ATD_TMP_IDX(tmp_idx);
               }
               else if (ATD_FLD(tmp_idx) == IR_Tbl_Idx &&
                        IR_OPR(ATD_TMP_IDX(tmp_idx)) == Mult_Opr) {
            
                  /* this is a scalar broadcast */
                  /* so broadcast it now. */

                  const_idx = IR_IDX_R(ATD_TMP_IDX(tmp_idx));

                  save_target_array_idx = target_array_idx;
                  target_array_idx = ATD_ARRAY_IDX(attr_idx);

                  exp_desc = init_exp_desc;
                  exp_desc.type_idx = CN_TYPE_IDX(const_idx);
                  exp_desc.type = TYP_TYPE(exp_desc.type_idx);
                  exp_desc.linear_type = TYP_LINEAR(exp_desc.type_idx);
                  exp_desc.constant = TRUE;
                  exp_desc.foldable = TRUE;

                  gen_opnd(&cn_opnd, const_idx, CN_Tbl_Idx, line, col);
                  fold_aggragate_expression(&cn_opnd,
                                            &exp_desc,
                                             TRUE); /* return constant */
                  target_array_idx = save_target_array_idx;

                  const_idx = OPND_IDX(cn_opnd);
               }
            }
            else {
               const_idx = ATD_CPNT_INIT_IDX(attr_idx);
            }

            gen_init_stmt(&opnd,
                          const_idx,
                          position);
         }
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure           &&
               (ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ||
#ifdef KEY /* Bug 6845 */
               ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ||
#endif /* KEY Bug 6845 */
                ATT_DEFAULT_INITIALIZED(TYP_IDX(ATD_TYPE_IDX(attr_idx))))) {

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Struct_Opr;
         IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx)  = col;
         COPY_OPND(IR_OPND_L(ir_idx), (*left_opnd));
         IR_FLD_R(ir_idx) = AT_Tbl_Idx;
         IR_IDX_R(ir_idx) = attr_idx;
         IR_LINE_NUM_R(ir_idx) = line;
         IR_COL_NUM_R(ir_idx)  = col;
         OPND_FLD(opnd) = IR_Tbl_Idx;
         OPND_IDX(opnd) = ir_idx;

         if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
             IR_RANK(ir_idx) = IR_RANK(IR_IDX_L(ir_idx));
         }

         if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
            exp_desc = init_exp_desc;
            gen_whole_subscript(&opnd, &exp_desc);
         }

         process_cpnt_inits(&opnd, 
                            TYP_IDX(ATD_TYPE_IDX(attr_idx)),
                            func,
			    opr,
                            position);

      }

      sn_idx = SN_SIBLING_LINK(sn_idx);
   }

   /* remove placeholder_sh_idx */

   if (placeholder_sh_idx != NULL_IDX) {
      remove_sh(placeholder_sh_idx);
      FREE_SH_NODE(placeholder_sh_idx);
   }

   if (position == Before) {
      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
   else {
      if (save_curr_stmt_sh_idx != NULL_IDX) {
         curr_stmt_sh_idx = SH_PREV_IDX(save_curr_stmt_sh_idx);
      }
      else {
         /* find end of stmts */
          while (SH_NEXT_IDX(curr_stmt_sh_idx) != NULL_IDX) {
            curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
         }
      }
   }

   TRACE (Func_Exit, "process_cpnt_inits", NULL);

   return;

}  /* process_cpnt_inits */

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

static void gen_init_stmt(opnd_type		*left_opnd,
                          int			const_idx,
                          sh_position_type	position)

{
   int                  array_attr_idx;
   opnd_type            base_opnd;
   int                  bd_idx;
   int                  col;
   long64               count = 0;
   int                  init_idx;
   int                  line;
   int                  list_idx;
   int                  mult_idx;
   int                  num_loops = 0;
   opnd_type            opnd;
   int                  rank_idx = NULL_IDX;
   long_type            result[MAX_WORDS_FOR_INTEGER];
   long64	        sm_bits;
   int                  type_idx;
   int                  unused = NULL_IDX;
   int                  unused2;
   long_type            the_constant[MAX_WORDS_FOR_INTEGER];


   TRACE (Func_Entry, "gen_init_stmt", NULL);

   find_opnd_line_and_column(left_opnd, &line, &col);

   NTR_IR_TBL(init_idx);
   IR_OPR(init_idx) = Init_Opr;
   IR_TYPE_IDX(init_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(init_idx) = line;
   IR_COL_NUM(init_idx)  = col;

   COPY_OPND(IR_OPND_L(init_idx), (*left_opnd));

   COPY_OPND(opnd, (*left_opnd));
   while (OPND_FLD(opnd) == IR_Tbl_Idx) {
      if (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr) {
         num_loops++;
      }
      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   if (num_loops > 0) {

      if (num_loops == 1) {
         /* set up as a single init with rep count and stride */
         COPY_OPND(opnd, (*left_opnd));
         make_base_subtree(&opnd, &base_opnd, &rank_idx, &unused);

# ifdef _DEBUG
         if (rank_idx == NULL_IDX) {
            PRINTMSG(line, 626, Internal, col,
                     "whole array subscript",
                     "gen_init_stmt");
         }
# endif
         array_attr_idx = find_base_attr(&IR_OPND_L(rank_idx),
                                         &unused,
                                         &unused2);

         bd_idx = ATD_ARRAY_IDX(array_attr_idx);

         COPY_OPND(IR_OPND_L(init_idx), base_opnd);

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(init_idx) = IL_Tbl_Idx;
         IR_IDX_R(init_idx) = list_idx;
         IR_LIST_CNT_R(init_idx) = 3;

         /* value */

         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = const_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = col;

         /* rep count */

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

# ifdef _DEBUG
         if (BD_LEN_FLD(bd_idx) != CN_Tbl_Idx) {
            PRINTMSG(line, 626, Internal, col,
                     "constant array length",
                     "gen_init_stmt");
         }
# endif
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = BD_LEN_IDX(bd_idx);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = col;

         /* stride in bits */

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

# ifdef _SM_UNIT_IS_ELEMENT
         sm_bits = sm_unit_in_bits(ATD_TYPE_IDX(array_attr_idx));
         C_TO_F_INT(the_constant, sm_bits, Integer_8);
# else
         if (TYP_TYPE(ATD_TYPE_IDX(array_attr_idx)) == Structure &&
             ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(array_attr_idx)))) {
            C_TO_F_INT(the_constant, 8, CG_INTEGER_DEFAULT_TYPE);
         }
         else {
            sm_bits = sm_unit_in_bits(ATD_TYPE_IDX(array_attr_idx));
            C_TO_F_INT(the_constant, sm_bits, Integer_8);
         }
# endif

         type_idx = (CG_INTEGER_DEFAULT_TYPE >
                     TYP_LINEAR(CN_TYPE_IDX(BD_SM_IDX(bd_idx, 1))) ?
                      CG_INTEGER_DEFAULT_TYPE :
                             CN_TYPE_IDX(BD_SM_IDX(bd_idx, 1)));


         if (folder_driver((char *)&CN_CONST(BD_SM_IDX(bd_idx, 1)),
                           CN_TYPE_IDX(BD_SM_IDX(bd_idx, 1)),
                           (char *) the_constant,
                           CG_INTEGER_DEFAULT_TYPE,
                           result,
                          &type_idx,
                           line,
                           col,
                           2,
                           Mult_Opr)) {

            IL_FLD(list_idx) = CN_Tbl_Idx;
            IL_IDX(list_idx) = ntr_const_tbl(type_idx,
                                             FALSE,
                                             result);
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx) = col;
         }
      }
      else {
         /* must be all implied do loops */

         copy_subtree(left_opnd, &opnd);
         count = create_imp_do_loops(&opnd);
         COPY_OPND(IR_OPND_L(init_idx), opnd);

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(init_idx) = IL_Tbl_Idx;
         IR_IDX_R(init_idx) = list_idx;
         IR_LIST_CNT_R(init_idx) = 1;

         NTR_IR_TBL(mult_idx);
         IR_OPR(mult_idx) = Mult_Opr;
         IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(mult_idx) = line;
         IR_COL_NUM(mult_idx)  = col;
         IR_FLD_L(mult_idx) = CN_Tbl_Idx;
         IR_IDX_L(mult_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, count);

         IR_LINE_NUM_L(mult_idx) = line;
         IR_COL_NUM_L(mult_idx)  = col;
         IR_FLD_R(mult_idx) = CN_Tbl_Idx;
         IR_IDX_R(mult_idx) = const_idx;
         IR_LINE_NUM_R(mult_idx) = line;
         IR_COL_NUM_R(mult_idx)  = col;

         IL_FLD(list_idx) = IR_Tbl_Idx;
         IL_IDX(list_idx) = mult_idx;
      }
   }
   else {

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(init_idx) = IL_Tbl_Idx;
      IR_IDX_R(init_idx) = list_idx;
      IR_LIST_CNT_R(init_idx) = 3;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = const_idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx)     = init_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = init_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }


   TRACE (Func_Exit, "gen_init_stmt", NULL);

   return;

}  /* gen_init_stmt */
#ifdef KEY /* Bug 6845 */

/*
 * attr_idx	Attribute table index of entity which dope vector represents
 * is_array	True if dope vector represents an array
 * return	If entity is an allocatable array whose element type is a
 *		derived type, and component(s) of the derived type are
 *		themselves allocatable, return the number of allocatable
 *		components; otherwise, return 0. If a component is itself
 *		a structure containing allocatable array subcomponents, we
 *		count them as well.
 */
int
do_count_allocatable_cpnt(int attr_idx, int is_array) {
  if (!is_array) { /* Not an array */
    return 0;
    }
  int element_type_idx = ATD_TYPE_IDX(attr_idx);
  if (TYP_TYPE(element_type_idx) != Structure ||
    !ATT_ALLOCATABLE_CPNT(TYP_IDX(element_type_idx))) {
    return 0;
    }
  int count = 0;
  for (int sn_idx = ATT_FIRST_CPNT_IDX(TYP_IDX(element_type_idx));
    sn_idx != NULL_IDX;
    sn_idx = SN_SIBLING_LINK(sn_idx)) {
    int cpnt_attr_idx = SN_ATTR_IDX(sn_idx);
    if (ATD_ALLOCATABLE(cpnt_attr_idx)) {
      count += 1;
    }
    /* Child structure contains components which are allocatable arrays */
    else if (TYP_TYPE(ATD_TYPE_IDX(cpnt_attr_idx)) == Structure) {
      count += do_count_allocatable_cpnt(cpnt_attr_idx, 1);
    }
  }
  return count;
}

/*
 * Used by do_alloc_cpnt_offset to append one operand to list
 *
 * line		source line
 * col		source column
 * list_idx	index of current operand list item
 * fld		which table the index belongs to
 * idx		index of value of operand
 * returns	index of newly created operand list item
 */
static int
do_one_operand(int line, int col, int list_idx, fld_type fld, int idx) {
   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = fld;
   IL_IDX(list_idx) = idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;
   return list_idx;
}

/*
 * Append to list of operands of dv_whole_def_opr a boolean indicating
 * whether the dope vector has a list of allocatable component offsets
 *
 * line		source line
 * col		source column
 * list_idx	index of current list item
 * n_allocatable_cpnt	number of allocatable components
 * return	index of newly created list item
 */
static int
do_alloc_cpnt(int line, int col, int list_idx, int n_allocatable_cpnt) {

   /**************\
   |* ALLOC_CPNT *|
   \**************/

   return do_one_operand(line, col, list_idx, CN_Tbl_Idx,
     (n_allocatable_cpnt ? CN_INTEGER_ONE_IDX : CN_INTEGER_ZERO_IDX));
      
}

/*
 * If the dope vector represents an an allocatable array whose element type
 * is a derived type having allocatable components, append to the list of
 * operands of dv_whole_def_opr a series of byte offsets to the allocatable
 * components
 *
 * line		source line
 * col		source column
 * list_idx	index of current list item (stride of highest dimension of
 *		array)
 * attr_idx	Attribute table index of entity which dope vector represents
 * n_allocatable_cpnt	number of allocatable components
 * return	index of last newly created list item (last offset)
 */
static int
do_alloc_cpnt_offset(int line, int col, int list_idx, int attr_idx,
   int n_allocatable_cpnt) {

   if (0 == n_allocatable_cpnt) {
     return list_idx;
   }

   /*********************\
   |* ALLOC CPNT OFFSET *|
   \*********************/

  int element_type_idx = ATD_TYPE_IDX(attr_idx);
  for (int sn_idx = ATT_FIRST_CPNT_IDX(TYP_IDX(element_type_idx));
    sn_idx != NULL_IDX;
    sn_idx = SN_SIBLING_LINK(sn_idx)) {
    int cpnt_attr_idx = SN_ATTR_IDX(sn_idx);
    if (ATD_ALLOCATABLE(cpnt_attr_idx)) {
      list_idx = do_one_operand(line, col, list_idx,
	ATD_OFFSET_FLD(cpnt_attr_idx), ATD_CPNT_OFFSET_IDX(cpnt_attr_idx));
    }
    /* Child structure contains components which are allocatable arrays */
    else if (TYP_TYPE(ATD_TYPE_IDX(cpnt_attr_idx)) == Structure) {
      list_idx = do_alloc_cpnt_offset(line, col, list_idx, cpnt_attr_idx,
        n_allocatable_cpnt);
    }
  }
  return list_idx;
}
#endif /* KEY Bug 6845 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Gen the dv_whole_def_opr to set a dope vector in one operation.       *|
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

void gen_dv_whole_def(opnd_type		*dv_opnd,
		      opnd_type		*r_opnd,
		      expr_arg_type     *exp_desc)

{
   act_arg_type a_type;
   int		asg_idx;
#ifdef KEY /* Bug 10177 */
   int		attr_idx = 0;
#else /* KEY Bug 10177 */
   int		attr_idx;
#endif /* KEY Bug 10177 */
   opnd_type	base_opnd;
   int		col;
   int		dim = 1;
   int		dope_idx = NULL_IDX;
   int		dv_attr_idx;
   int		dv2_idx;
   int		i;
   int		ir_idx;
   opnd_type	len_opnd;
   int		line;
   int		list_idx;
   int		list2_idx;
   int		loc_idx;
   int		max_idx;
   int		mult_idx;
   opnd_type    opnd;
   long		rank;
   int		rank_idx = NULL_IDX;
   opnd_type    r_dv_opnd;
   int          stride_idx;
   opnd_type	stride_opnd;
#ifdef KEY /* Bug 10177 */
   int          subscript_idx = 0;
#else /* KEY Bug 10177 */
   int          subscript_idx;
#endif /* KEY Bug 10177 */
   int          type_idx;
   boolean      whole_array;


   TRACE (Func_Entry, "gen_dv_whole_def", NULL);

   dv_attr_idx = find_base_attr(dv_opnd, &line, &col);

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Dv_Def_Asg_Opr;
   IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Whole_Def_Opr;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   COPY_OPND(IR_OPND_L(asg_idx), (*dv_opnd));
   IR_FLD_R(asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(asg_idx) = ir_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_idx;

   rank = (ATD_ARRAY_IDX(dv_attr_idx) ? 
                         (long) BD_RANK(ATD_ARRAY_IDX(dv_attr_idx)) : 0);
#ifdef KEY /* Bug 6845 */
   int n_allocatable_cpnt = IR_DV_N_ALLOC_CPNT(ir_idx) =
     do_count_allocatable_cpnt(dv_attr_idx, rank);
   IR_LIST_CNT_L(ir_idx) = 11 + (3 * rank) + n_allocatable_cpnt;
#else /* KEY Bug 6845 */
   IR_LIST_CNT_L(ir_idx) = 10 + (3 * rank);
#endif /* KEY Bug 6845 */
   IR_DV_DIM(ir_idx) = rank;

   /*************\
   |* BASE ADDR *|
   \*************/

   NTR_IR_TBL(loc_idx);
   IR_OPR(loc_idx)  = Loc_Opr;
   IR_LINE_NUM(loc_idx) = line;
   IR_COL_NUM(loc_idx)  = col;

   if (exp_desc->type == Character) {
      IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
   }
   else {
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
   }

   IL_FLD(list_idx) = IR_Tbl_Idx;
   IL_IDX(list_idx) = loc_idx;

   if (exp_desc->rank == 0) {
      COPY_OPND(IR_OPND_L(loc_idx), (*r_opnd));
      just_find_dope_and_rank(r_opnd, &rank_idx, &dope_idx);
   }
   else {
      make_base_subtree(r_opnd, &base_opnd, &rank_idx, &dope_idx);
      COPY_OPND(IR_OPND_L(loc_idx), base_opnd);
   }

# ifdef _TRANSFORM_CHAR_SEQUENCE
# ifdef _TARGET_OS_UNICOS
   if (exp_desc->type == Structure &&
       ATT_CHAR_SEQ(TYP_IDX(exp_desc->type_idx))) {

      IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
      COPY_OPND(opnd, IR_OPND_L(loc_idx));
      transform_char_sequence_ref(&opnd, exp_desc->type_idx);
      COPY_OPND(IR_OPND_L(loc_idx), opnd);
   }
# endif
# endif


   /*************************\
   |* check for whole array *|
   \*************************/

   if (rank_idx != NULL_IDX) {
      attr_idx      = find_base_attr(&IR_OPND_L(rank_idx), &line, &col);
  
      if (ATD_IM_A_DOPE(attr_idx)) {
         COPY_OPND(r_dv_opnd, IR_OPND_L(IR_IDX_L(rank_idx)));
      }
      subscript_idx = IR_IDX_R(rank_idx);
   }
   else if (exp_desc->rank != 0)              {
      attr_idx    = find_base_attr(r_opnd, &line, &col);
 
      if (ATD_IM_A_DOPE(attr_idx)) {
         COPY_OPND(r_dv_opnd, IR_OPND_L(OPND_IDX((*r_opnd))));
      }
   }
   else {
      find_opnd_line_and_column(r_opnd, &line, &col);
   }

   if (exp_desc->rank > 0 &&
       ! exp_desc->section) {

      whole_array = TRUE;
   }
   else {
      whole_array = FALSE;
   }

   /*************\
   |* EL_LEN    *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   type_idx = ATD_TYPE_IDX(dv_attr_idx);

   if (TYP_TYPE(type_idx) == Structure) {
      IL_FLD(list_idx)		= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
      IL_IDX(list_idx)		= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
      IL_LINE_NUM(list_idx)	= line;
      IL_COL_NUM(list_idx)	= col;
   }
   else if (TYP_TYPE(type_idx) == Character) {

      COPY_OPND(opnd, exp_desc->char_len);
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd) = col;
      compute_char_element_len(&opnd, r_opnd, &len_opnd);

      COPY_OPND(IL_OPND(list_idx), len_opnd);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = col;
   }
   else {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                    storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }

   /*************\
   |* ASSOC     *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* PTR_ALLOC *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (dope_idx != NULL_IDX) {

      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_Ptr_Alloc;
      IR_TYPE_IDX(dv2_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = line;
      IR_COL_NUM(dv2_idx)  = col;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = dv2_idx;
   }
   else {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }


   /*************\
   |* P_OR_A    *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;

   if (ATD_ALLOCATABLE(dv_attr_idx)) {
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 2);
   }
   else if (ATD_POINTER(dv_attr_idx)) {
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   }
   else {
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   }
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;



   /*************\
   |* A_CONTIG  *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   a_type = get_act_arg_type(exp_desc);

   if (a_type == Array_Ptr ||
       a_type == Array_Tmp_Ptr ||
       a_type == Whole_Ass_Shape ||
       a_type == Dv_Contig_Section) {

      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_A_Contig;
      IR_TYPE_IDX(dv2_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = line;
      IR_COL_NUM(dv2_idx)  = col;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = dv2_idx;

   }
   else if (a_type == Whole_Allocatable ||
            a_type == Whole_Tmp_Allocatable ||
            a_type == Whole_Sequence ||
            a_type == Whole_Tmp_Sequence ||
            a_type == Whole_Array_Constant ||
            a_type == Contig_Section) {

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }
   else {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }


   /*************\
   |* N_DIM     *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, rank);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* TYPE_CODE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = create_dv_type_code(dv_attr_idx);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* ORIG_BASE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (dope_idx != NULL_IDX) {

      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_Orig_Base;
      IR_TYPE_IDX(dv2_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = line;
      IR_COL_NUM(dv2_idx)  = col;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = dv2_idx;
   }
   else {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }


   /*************\
   |* ORIG_SIZE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (dope_idx != NULL_IDX) {

      NTR_IR_TBL(dv2_idx);
      IR_OPR(dv2_idx) = Dv_Access_Orig_Size;
      IR_TYPE_IDX(dv2_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv2_idx) = line;
      IR_COL_NUM(dv2_idx)  = col;
      COPY_OPND(IR_OPND_L(dv2_idx), IR_OPND_L(dope_idx));
      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = dv2_idx;
   }
   else {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }

#ifdef KEY /* Bug 6845 */
   list_idx = do_alloc_cpnt(line, col, list_idx, n_allocatable_cpnt);
#endif /* KEY Bug 6845 */

   for (i = 1; i <= rank; i++) {

      /*************\
      |* DIM i LB  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (whole_array) {
         /* need arrays low bound */
         if (ATD_IM_A_DOPE(attr_idx) &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) != Assumed_Shape) {
            NTR_IR_TBL(dv2_idx);
            IR_OPR(dv2_idx)    = Dv_Access_Low_Bound;
            IR_TYPE_IDX(dv2_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(dv2_idx) = line;
            IR_COL_NUM(dv2_idx)  = col;
            COPY_OPND(IR_OPND_L(dv2_idx), r_dv_opnd);
            IR_DV_DIM(dv2_idx) = i;
            IL_FLD(list_idx)   = IR_Tbl_Idx;
            IL_IDX(list_idx)   = dv2_idx;
         }
         else {
            IL_FLD(list_idx) = BD_LB_FLD(ATD_ARRAY_IDX(attr_idx), i);
            IL_IDX(list_idx) = BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), i);
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            if (IL_FLD(list_idx) == AT_Tbl_Idx) {
               ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
            }
         }
      }
      else {
         /* set to one */
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }


      /*************\
      |* DIM i EX  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      NTR_IR_TBL(max_idx);
      IR_OPR(max_idx) = Max_Opr;
      IR_TYPE_IDX(max_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(max_idx) = line;
      IR_COL_NUM(max_idx)  = col;

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

      COPY_OPND(IL_OPND(list2_idx), exp_desc->shape[i-1]);
      IL_LINE_NUM(list2_idx) = line;
      IL_COL_NUM(list2_idx) = col;

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = max_idx;

      /*************\
      |* DIM i SM  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (whole_array) {

         gen_dv_stride_mult(&stride_opnd,
                             attr_idx,
                            &r_dv_opnd,
                             exp_desc,
                             i,
                             line,
                             col);
                            
         COPY_OPND(IL_OPND(list_idx), stride_opnd);

      }
      else {
         while (IL_FLD(subscript_idx) != IR_Tbl_Idx ||
                IR_OPR(IL_IDX(subscript_idx)) != Triplet_Opr) {
            subscript_idx = IL_NEXT_LIST_IDX(subscript_idx);
            dim++;
         }

         gen_dv_stride_mult(&stride_opnd,
                             attr_idx,
                            &r_dv_opnd,
                             exp_desc,
                             dim,
                             line,
                             col);

         stride_idx = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_L(
                                                      IL_IDX(subscript_idx))));

         mult_idx = gen_ir(OPND_FLD(stride_opnd), OPND_IDX(stride_opnd),
                       Mult_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                           IL_FLD(stride_idx), IL_IDX(stride_idx));

         IL_FLD(list_idx) = IR_Tbl_Idx;
         IL_IDX(list_idx) = mult_idx;

         subscript_idx = IL_NEXT_LIST_IDX(subscript_idx);
         dim++;
      }
   }

#ifdef KEY /* Bug 6845 */
   list_idx = do_alloc_cpnt_offset(line, col, list_idx, dv_attr_idx,
     n_allocatable_cpnt);
#endif /* KEY Bug 6845 */

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "gen_dv_whole_def", NULL);

   return;

}  /* gen_dv_whole_def */

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

static void gen_dv_stride_mult(opnd_type	*stride_opnd,
                               int		 attr_idx,
                               opnd_type	*r_dv_opnd,
                               expr_arg_type	*exp_desc,
                               int		 dim,
                               int		 line,
                               int		 col)

{
# if defined(_EXTENDED_CRI_CHAR_POINTER)
   int		clen_idx;
# endif

   int		cn_idx;
   int		dv_idx;
   int		ir_idx;
   long64	res_sm_unit_in_bits;
   long64	src_sm_unit_in_bits;


   TRACE (Func_Entry, "gen_dv_stride_mult", NULL);

   /* res_sm_unit_in_bits describes the sm unit for the result dv */

   if (exp_desc->type == Structure &&
       ATT_CHAR_SEQ(TYP_IDX(exp_desc->type_idx))) {
      res_sm_unit_in_bits = sm_unit_in_bits(Character_1);
   }
   else {
      res_sm_unit_in_bits = sm_unit_in_bits(exp_desc->type_idx);
   }

   /* src_sm_unit_in_bits describes the sm unit for the arrays bd entry */

   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
       ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {
      src_sm_unit_in_bits = sm_unit_in_bits(Character_1);
   }
   else {
      src_sm_unit_in_bits = sm_unit_in_bits(ATD_TYPE_IDX(attr_idx));
   }

# ifdef _DEBUG
   if (res_sm_unit_in_bits == 0 || src_sm_unit_in_bits == 0) {
      PRINTMSG(line, 626, Internal, col,
               "stride_mult_unit_in_bits",
               "gen_dv_stride_mult");
   }
# endif


   if (ATD_IM_A_DOPE(attr_idx)) {
      NTR_IR_TBL(dv_idx);
      IR_OPR(dv_idx) = Dv_Access_Stride_Mult;
      IR_TYPE_IDX(dv_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(dv_idx) = line;
      IR_COL_NUM(dv_idx)  = col;
      COPY_OPND(IR_OPND_L(dv_idx), (*r_dv_opnd));
      IR_DV_DIM(dv_idx) = dim;

      OPND_FLD((*stride_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*stride_opnd)) = dv_idx;
#ifdef _WHIRL_HOST64_TARGET64
	  /*
	   if we are generating a new dope vector from an existing dope vector
	   we do not need to generate the mulitplier
	   About the stride muliplier:
	   Normal vectors have a multiplier of 1
	   Dope vector(allocatable array) has a multiplier of size(type)/size(int)
	  */
	  res_sm_unit_in_bits=src_sm_unit_in_bits;
#endif
   }
   else {
      OPND_FLD((*stride_opnd)) = BD_SM_FLD(ATD_ARRAY_IDX(attr_idx), dim);
      OPND_IDX((*stride_opnd)) = BD_SM_IDX(ATD_ARRAY_IDX(attr_idx), dim);
      OPND_LINE_NUM((*stride_opnd)) = line;
      OPND_COL_NUM((*stride_opnd))  = col;

      if (OPND_FLD((*stride_opnd)) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(OPND_IDX((*stride_opnd)));
      }

# if defined(_EXTENDED_CRI_CHAR_POINTER) 
      if (ATD_CLASS(attr_idx) == CRI__Pointee &&
# if defined(KEY)
          AT_IS_DARG(attr_idx) &&
# endif
          TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)        = Mult_Opr;
         IR_TYPE_IDX(ir_idx)   = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)   = line;
         IR_COL_NUM(ir_idx)    = col;

         COPY_OPND(IR_OPND_L(ir_idx), (*stride_opnd));

         NTR_IR_TBL(clen_idx);
         IR_OPR(clen_idx) = Clen_Opr;
         IR_TYPE_IDX(clen_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(clen_idx)   = line;
         IR_COL_NUM(clen_idx)    = col;
         IR_FLD_L(clen_idx) = AT_Tbl_Idx;
         IR_IDX_L(clen_idx) = attr_idx;
         IR_LINE_NUM_L(clen_idx)   = line;
         IR_COL_NUM_L(clen_idx)    = col;

         IR_FLD_R(ir_idx) = IR_Tbl_Idx;
         IR_IDX_R(ir_idx) = clen_idx;

         OPND_FLD((*stride_opnd))   = IR_Tbl_Idx;
         OPND_IDX((*stride_opnd))   = ir_idx;
      }
# endif
   }

# ifndef _SM_UNIT_IS_ELEMENT
   if (src_sm_unit_in_bits != res_sm_unit_in_bits) {

      /* BRIANJ - C_INT_TO_CN has the capability of switching this to */
      /* Integer_8 automatically.  See me  KAY */


      cn_idx =  C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                            (src_sm_unit_in_bits / res_sm_unit_in_bits));

      ir_idx = gen_ir(OPND_FLD((*stride_opnd)), 
                      OPND_IDX((*stride_opnd)),
                      Mult_Opr, 
                      CG_INTEGER_DEFAULT_TYPE, 
                      line,
                      col,
                      CN_Tbl_Idx,
                      cn_idx);

      OPND_FLD((*stride_opnd))   = IR_Tbl_Idx;
      OPND_IDX((*stride_opnd))   = ir_idx;
   }
# endif


   TRACE (Func_Exit, "gen_dv_stride_mult", NULL);

   return;

}  /* gen_dv_stride_mult */

#ifdef KEY /* Bug 6845 */
/*
 * Insert Dv_Deref_Opr between Subscript_Opr and array
 *
 * line		Source line
 * col		Source column
 * parent_idx	IR_Tbl_Idx index of parent (Subscript_Opr) node
 * child_idx	AT_Tbl_Idx index of child (array) node
 */
static void
insert_dv_deref(int line, int col, int parent_idx, int child_idx) {
  int dv_deref_idx;
  NTR_IR_TBL(dv_deref_idx);
  IR_OPR(dv_deref_idx) = Dv_Deref_Opr;
  IR_TYPE_IDX(dv_deref_idx) = ATD_TYPE_IDX(child_idx);
  IR_LINE_NUM_L(dv_deref_idx) = IR_LINE_NUM(dv_deref_idx) = line;
  IR_COL_NUM_L(dv_deref_idx) = IR_COL_NUM(dv_deref_idx)  = col;
  IR_FLD_L(dv_deref_idx) = AT_Tbl_Idx;
  IR_IDX_L(dv_deref_idx) = child_idx;
  IR_FLD_R(dv_deref_idx) = NO_Tbl_Idx;
  IR_IDX_R(dv_deref_idx) = NULL_IDX;
  IR_FLD_L(parent_idx) = IR_Tbl_Idx;
  IR_IDX_L(parent_idx) = dv_deref_idx;
}

/*
 * Call this before gen_loops()
 *
 * line		Source line
 * col		Source column
 * return	idx of placeholder for use in post_gen_loops()
 * next_sh_idx	idx of statement that will follow the end of the last loop
 *		once we finish generating loops
 */
int
pre_gen_loops(int line, int col, int *next_sh_idx) {
  gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
  *next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
  return curr_stmt_sh_idx;
}

/*
 * After calling gen_loops() and creating the body of the loops, call this.
 *
 * placeholder_sh_idx	idx from pre_gen_loops()
 * next_sh_idx		idx of the statement that follows the end of the last
 *			loop
 */
void
post_gen_loops(int placeholder_sh_idx, int next_sh_idx) {
  remove_sh(placeholder_sh_idx);
  FREE_SH_NODE(placeholder_sh_idx);
  if (next_sh_idx != NULL_IDX) {
    curr_stmt_sh_idx = SH_PREV_IDX(next_sh_idx);
  }
  else {
    while (SH_NEXT_IDX(curr_stmt_sh_idx) != NULL_IDX) {
      curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
    }
  }
}

/*
 * Generate loops to traverse all elements of an array represented by
 * Whole_Subscript_Opr or Section_Subscript_Opr opnd_l. Transform opnd_l to
 * use the loop index variables as its subscripts. If optional argument
 * opnd_r is present, transform it likewise.
 *
 * The illogic of the statement structure (inherited from gen_dv_def_loops,
 * which we generalized to make gen_loops) is that the loops will surround
 * the statement associated with curr_stmt_sh_idx, leaving curr_stmt_sh_idx
 * set to that statement rather than to the end of the last loop. The usual
 * technique is to create a placeholder, save its index, create the loops,
 * add statements after the placeholder, delete the placeholder, move
 * curr_stmt_sh_idx to the end of the last loop. Yuck. See pre_gen_loops()
 * and post_gen_loops().
 *
 * opnd_l	Whole_Subscript_Opr or Section_Subscript_Opr for destination
 *		array
 * opnd_r	Whole_Subscript_Opr or Section_Subscript_Opr for source
 *		array which must be conformable with opnd_l (or else null)
 * deref	If true, generate dv_deref_idx between the Subscript_Opr and
 *		the array variable if that variable is a dope vector
 */
void
gen_loops(opnd_type *opnd_l, opnd_type *opnd_r, boolean deref)
{
   int		col;
   int		line;
   opnd_type	temp_l;

   TRACE (Func_Entry, "gen_loops", NULL);

   find_opnd_line_and_column(opnd_l, &line, &col);

   opnd_type next;
   int subscripts[STATIC_SUBSCRIPT_SIZE];
   int subscript_cnt = 0;
   for (COPY_OPND(temp_l, *opnd_l);
      (OPND_FLD(temp_l) == IR_Tbl_Idx);
      COPY_OPND(temp_l, next)) {

      operator_type ir_opr = IR_OPR(OPND_IDX(temp_l));
      if (ir_opr == Whole_Subscript_Opr || ir_opr == Section_Subscript_Opr) {

         IR_OPR(OPND_IDX(temp_l)) = Subscript_Opr;

         for (int list_idx = IR_IDX_R(OPND_IDX(temp_l));
	    list_idx != NULL_IDX;
	    list_idx = IL_NEXT_LIST_IDX(list_idx)) {

            if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

               int tmp_idx = subscripts[subscript_cnt++] =
	         gen_compiler_tmp(line, col, Priv, TRUE);
            
               ATD_TYPE_IDX(tmp_idx) = CG_INTEGER_DEFAULT_TYPE;
               ATD_STOR_BLK_IDX(tmp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
               AT_SEMANTICS_DONE(tmp_idx) = TRUE;

               int list_idx2 = IR_IDX_L(IL_IDX(list_idx));
	       opnd_type start_opnd;
	       opnd_type stride_opnd;
	       opnd_type end_opnd;

               COPY_OPND(start_opnd, IL_OPND(list_idx2));

               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
               COPY_OPND(end_opnd, IL_OPND(list_idx2));

               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
               COPY_OPND(stride_opnd, IL_OPND(list_idx2));

               create_loop_stmts(tmp_idx, &start_opnd, &end_opnd, &stride_opnd,
                                 curr_stmt_sh_idx,     /* body start sh idx */
                                 curr_stmt_sh_idx);    /* body end sh idx */

               IL_FLD(list_idx) = AT_Tbl_Idx;
               IL_IDX(list_idx) = tmp_idx;
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx) = col;
            }
         }
      }

      next = IR_OPND_L(OPND_IDX(temp_l));
      if (deref && ir_opr != Dv_Deref_Opr && OPND_FLD(next) == AT_Tbl_Idx &&
        ATD_IM_A_DOPE(OPND_IDX(next))) {
	insert_dv_deref(line, col, OPND_IDX(temp_l), OPND_IDX(next));
      }
   }

   if (opnd_r) {
      subscript_cnt = 0;
      opnd_type temp_r;
      for (COPY_OPND(temp_r, (*opnd_r));
	OPND_FLD(temp_r) == IR_Tbl_Idx;
	COPY_OPND(temp_r, next)) {

	operator_type ir_opr = IR_OPR(OPND_IDX(temp_r));
	if (ir_opr == Whole_Subscript_Opr || ir_opr == Section_Subscript_Opr) {

	   IR_OPR(OPND_IDX(temp_r)) = Subscript_Opr;

	   for (int list_idx = IR_IDX_R(OPND_IDX(temp_r));
	      list_idx != NULL_IDX;
	      list_idx = IL_NEXT_LIST_IDX(list_idx)) {

	      if (IL_FLD(list_idx) == IR_Tbl_Idx &&
		 IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

		 IL_FLD(list_idx) = AT_Tbl_Idx;
		 IL_IDX(list_idx) = subscripts[subscript_cnt++];
		 IL_LINE_NUM(list_idx) = line;
		 IL_COL_NUM(list_idx) = col;
	      }
	   }
	}

	next = IR_OPND_L(OPND_IDX(temp_r));
	if (deref && ir_opr != Dv_Deref_Opr && OPND_FLD(next) == AT_Tbl_Idx &&
	  ATD_IM_A_DOPE(OPND_IDX(next))) {
	  insert_dv_deref(line, col, OPND_IDX(temp_r), OPND_IDX(next));
	}
     }
   }

   TRACE (Func_Exit, "gen_loops", NULL);
}  /* gen_loops */

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

static void
gen_dv_def_loops(opnd_type	*dv_opnd)

{
   gen_loops(dv_opnd, 0, FALSE);
}
#endif /* KEY Bug 6845 */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Gen the dv_whole_def_opr to set a dope vector in one operation.       *|
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

void gen_dv_whole_def_init(opnd_type		*dv_opnd,
			   int		 	dv_attr_idx,
			   sh_position_type	position)

{
   int			asg_idx;
   int			col;
   int			i;
   int			ir_idx;
   size_offset_type	length;
   int			line;
   int			list_idx;
   int			mult_idx;
   long			rank;
   size_offset_type	result;
   int			type_idx;


   TRACE (Func_Entry, "gen_dv_whole_def_init", NULL);

   find_opnd_line_and_column(dv_opnd, &line, &col);

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Dv_Def_Asg_Opr;
   IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Whole_Def_Opr;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   COPY_OPND(IR_OPND_L(asg_idx), (*dv_opnd));
   IR_FLD_R(asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(asg_idx) = ir_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_idx;

   rank = ATD_ARRAY_IDX(dv_attr_idx) ?
                        (long) BD_RANK(ATD_ARRAY_IDX(dv_attr_idx)) : 0;
#ifdef KEY /* Bug 6845 */
   int n_allocatable_cpnt = IR_DV_N_ALLOC_CPNT(ir_idx) =
     do_count_allocatable_cpnt(dv_attr_idx, rank);
   IR_LIST_CNT_L(ir_idx) = 11 + (3 * rank) + n_allocatable_cpnt;
#else /* KEY Bug 6845 */
   IR_LIST_CNT_L(ir_idx) = 10 + (3 * rank);
#endif /* KEY Bug 6845 */
   IR_DV_DIM(ir_idx) = rank;

   /*************\
   |* BASE ADDR *|
   \*************/

#ifdef KEY /* Bug 6106 */
   /* We want to set the base address to null explicitly so the code behaves
    * reproducibly if the user fails to allocate or initialize the variable.
    */
   IL_FLD(list_idx) = IR_Tbl_Idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)	= col;

   int fcd_idx;
   NTR_IR_TBL(fcd_idx);
   IL_IDX(list_idx) = fcd_idx;
   IR_OPR(fcd_idx) = Aloc_Opr;
   IR_TYPE_IDX(fcd_idx) = CRI_Ptr_8;
   IR_LINE_NUM(fcd_idx) = line;
   IR_COL_NUM(fcd_idx)  = col;

   IR_FLD_L(fcd_idx) = CN_Tbl_Idx;
   IR_IDX_L(fcd_idx) = (SA_INTEGER_DEFAULT_TYPE == CG_INTEGER_DEFAULT_TYPE) ?
     CN_INTEGER_ZERO_IDX : 
     C_INT_TO_CN(SA_INTEGER_DEFAULT_TYPE, 0);
   IR_LINE_NUM_L(fcd_idx) = line;
   IR_COL_NUM_L(fcd_idx)  = col;
#else /* KEY Bug 6106 */
   /* leave as null ops */
#endif /* KEY Bug 6106 */

   /*************\
   |* EL_LEN    *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   type_idx = ATD_TYPE_IDX(dv_attr_idx);

   if (TYP_TYPE(type_idx) == Structure) {
      IL_FLD(list_idx)	= (fld_type) ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
      IL_IDX(list_idx)	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
      IL_LINE_NUM(list_idx)	= line;
      IL_COL_NUM(list_idx)	= col;
   }
   else if (TYP_TYPE(type_idx) == Character) {

      IL_FLD(list_idx)      = TYP_FLD(type_idx);
      IL_IDX(list_idx)      = TYP_IDX(type_idx);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      if (IL_FLD(list_idx) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
      }

      if (! char_len_in_bytes) {
         /* Len is in bytes on solaris */
         /* Len is in bits for everyone else */

         if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
            result.fld		= CN_Tbl_Idx;
            result.idx		= CN_INTEGER_CHAR_BIT_IDX;
            length.fld		= TYP_FLD(type_idx);
            length.idx		= TYP_IDX(type_idx);
   
            size_offset_binary_calc(&length,
                                    &result,
                                     Mult_Opr,
                                    &result);

            if (result.fld == NO_Tbl_Idx) {
               IL_FLD(list_idx)       = CN_Tbl_Idx;
               IL_IDX(list_idx)       = ntr_const_tbl(result.type_idx,
                                                      FALSE,
                                                      result.constant);
            }
            else {
               IL_FLD(list_idx)       = result.fld;
               IL_IDX(list_idx)       = result.idx;
            }

            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;
         }
         else {
            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx) = Mult_Opr;
            IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx) = line;
            IR_COL_NUM(mult_idx)  = col;
            IR_FLD_L(mult_idx)    = CN_Tbl_Idx;
            IR_IDX_L(mult_idx)    = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 8);
            IR_LINE_NUM_L(mult_idx) = line;
            IR_COL_NUM_L(mult_idx)  = col;
   
            IR_FLD_R(mult_idx)    = TYP_FLD(type_idx);
            IR_IDX_R(mult_idx)    = TYP_IDX(type_idx);
            IR_LINE_NUM_R(mult_idx) = line;
            IR_COL_NUM_R(mult_idx)  = col;

            IL_FLD(list_idx)      = IR_Tbl_Idx;
            IL_IDX(list_idx)      = mult_idx;
         }
      }
   }
   else {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                    storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }

   /*************\
   |* ASSOC     *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* PTR_ALLOC *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* P_OR_A    *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;

   if (ATD_ALLOCATABLE(dv_attr_idx)) {
      IL_IDX(list_idx) = CN_INTEGER_TWO_IDX;
   }
   else if (ATD_POINTER(dv_attr_idx)) {
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   }
   else {
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   }
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;



   /*************\
   |* A_CONTIG  *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;

   if (ATD_ALLOCATABLE(dv_attr_idx)) {
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   }
   else {
#ifdef KEY /* Bug 9608 */
      /*
       * When we set assoc=0 for an array, we also set contig=1 so that
       * copyinout doesn't blow up if user (illegally) passes the null pointer
       * to a procedure lacking an explicit interface, in the (unjustified)
       * expectation that the pointer won't be dereferenced if the procedure
       * doesn't refer to the dummy argument. This seems cheaper than adding
       * a test for null before and after every call.
       */
      IL_IDX(list_idx) = rank ? CN_INTEGER_ONE_IDX : CN_INTEGER_ZERO_IDX;
#else /* KEY Bug 9608 */
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
#endif /* KEY Bug 9608 */
   }
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* N_DIM     *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, rank);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* TYPE_CODE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = create_dv_type_code(dv_attr_idx);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* ORIG_BASE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* ORIG_SIZE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

#ifdef KEY /* Bug 6845 */
   list_idx = do_alloc_cpnt(line, col, list_idx, n_allocatable_cpnt);
#endif /* KEY Bug 6845 */

   for (i = 1; i <= rank; i++) {

      /*************\
      |* DIM i LB  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (cmd_line_flags.runtime_bounds) {
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = col;
      }

      /*************\
      |* DIM i EX  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (cmd_line_flags.runtime_bounds) {
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = col;
      }

      /*************\
      |* DIM i SM  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (cmd_line_flags.runtime_bounds) {
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = col;
      }
   }

#ifdef KEY /* Bug 6845 */
   list_idx = do_alloc_cpnt_offset(line, col, list_idx, dv_attr_idx,
     n_allocatable_cpnt);
#endif /* KEY Bug 6845 */

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }

   TRACE (Func_Exit, "gen_dv_whole_def_init", NULL);

   return;

} /* gen_dv_whole_def_init */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Make a copy of a reference subtree where sections are replace by      *|
|*      the start value (or lower bound). This is to get the base address     *|
|*      of an array section.                                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      old_opnd - root of original tree.                                     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      new_opnd - root of copy.                                              *|
|*      rank_idx - ir idx to subscript opr that creates the rank.             *|
|*      dope_idx - idx to dv_deref_opr if there is one.                       *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void make_base_subtree(opnd_type  *old_opnd,
                       opnd_type  *new_opnd,
                       int        *rank_idx,
                       int        *dope_idx)

{
   int		col;
   int          dummy_idx;
   fld_type     fld;
   int          idx;
   int		line;
   int          list_idx;
   int          list2_idx;
   int          new_root = NULL_IDX;
   opnd_type    n_opnd;
   opnd_type    o_opnd;


   TRACE (Func_Entry, "make_base_subtree", NULL);

   find_opnd_line_and_column(old_opnd, &line, &col);

   OPND_FLD((*new_opnd)) = OPND_FLD((*old_opnd));
   idx = OPND_IDX((*old_opnd));
   fld = OPND_FLD((*old_opnd));
   

   if (idx != NULL_IDX) {

      switch(fld) {

         case NO_Tbl_Idx   :
            break;

         case IR_Tbl_Idx :

            if (IR_OPR(idx) == Triplet_Opr) {
               COPY_OPND(o_opnd, IL_OPND(IR_IDX_L(idx)));
               make_base_subtree(&o_opnd, new_opnd, rank_idx, &dummy_idx);
               goto SKIP;
            }
            else if (IR_OPR(idx) == Call_Opr) {
               /* don't process a call and it's arguments. This means that */
               /* make_base_subtree was called before deferred function    */
               /* flattening occured.                                      */

               new_root = idx;
            }
            else {

               NTR_IR_TBL(new_root);

               COPY_TBL_NTRY(ir_tbl, new_root, idx);

               /* assume that all ir is now scalar */
               IR_RANK(new_root) = 0;

               if (IR_OPR(new_root) == Whole_Subscript_Opr    ||
                   IR_OPR(new_root) == Section_Subscript_Opr) {

                  if (*rank_idx != NULL_IDX) {
                     PRINTMSG(IR_LINE_NUM(idx), 545, Internal, IR_COL_NUM(idx));
                  }
                  *rank_idx = idx;

                  IR_OPR(new_root)  = Subscript_Opr;
               }
               else if (IR_OPR(idx) == Dv_Deref_Opr &&
                        *dope_idx   == NULL_IDX)    {
                  *dope_idx = idx;
               }

               COPY_OPND(o_opnd, IR_OPND_L(idx));
               make_base_subtree(&o_opnd, &n_opnd, rank_idx, dope_idx);
               COPY_OPND(IR_OPND_L(new_root), n_opnd);

               COPY_OPND(o_opnd, IR_OPND_R(idx));
               make_base_subtree(&o_opnd, &n_opnd, rank_idx, &dummy_idx);
               COPY_OPND(IR_OPND_R(new_root), n_opnd);
            }

            break;

         case AT_Tbl_Idx :
         case CN_Tbl_Idx :

            new_root = idx;
            OPND_LINE_NUM((*new_opnd)) = line;
            OPND_COL_NUM((*new_opnd))  = col;
            break;

         case IL_Tbl_Idx :

            NTR_IR_LIST_TBL(new_root);
            COPY_TBL_NTRY(ir_list_tbl, new_root, idx);
            OPND_LIST_CNT((*new_opnd)) = OPND_LIST_CNT((*old_opnd));
            COPY_OPND(o_opnd, IL_OPND(idx));
            make_base_subtree(&o_opnd, &n_opnd, rank_idx, &dummy_idx);
            COPY_OPND(IL_OPND(new_root), n_opnd);
            list2_idx        = new_root;
            idx              = IL_NEXT_LIST_IDX(idx);

            while (idx != NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);
               COPY_TBL_NTRY(ir_list_tbl, list_idx, idx);

               if (! IL_ARG_DESC_VARIANT(list_idx)) {
                  IL_PREV_LIST_IDX(list_idx)  = list2_idx;
               }
               IL_NEXT_LIST_IDX(list2_idx) = list_idx;
               list2_idx                   = list_idx;

               COPY_OPND(o_opnd, IL_OPND(idx));
               make_base_subtree(&o_opnd, &n_opnd, rank_idx, &dummy_idx);
               COPY_OPND(IL_OPND(list_idx), n_opnd);
               idx              = IL_NEXT_LIST_IDX(idx);
            }
            break;
      }
   }

   OPND_IDX((*new_opnd)) = new_root;
   OPND_FLD((*new_opnd)) = fld;

SKIP:

   TRACE (Func_Exit, "make_base_subtree", NULL);

   return;

}  /* make_base_subtree */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Finds the subcript opr that describes the section of an array         *|
|*      section reference and the Dv_Deref_Opr ir idx if there is one.        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      old_opnd - root of original tree.                                     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      rank_idx - idx of subscript opr that is the section.                  *|
|*      dope_idx - idx of deref opr if there is one.                          *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

static void just_find_dope_and_rank(opnd_type  *old_opnd,
                                    int        *rank_idx,
                                    int        *dope_idx)

{
   opnd_type    opnd;

   TRACE (Func_Entry, "just_find_dope_and_rank", NULL);

   COPY_OPND(opnd, (*old_opnd));

   while (OPND_FLD(opnd) == IR_Tbl_Idx) {

      if (IR_OPR(OPND_IDX(opnd)) == Section_Subscript_Opr ||
          IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr)  {

         if (*rank_idx != NULL_IDX) {
            PRINTMSG(IR_LINE_NUM(OPND_IDX(opnd)), 545, Internal,
                     IR_COL_NUM(OPND_IDX(opnd)));
         }
         *rank_idx = OPND_IDX(opnd);
      }
      else if (IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr &&
               *dope_idx              == NULL_IDX)    {
         *dope_idx = OPND_IDX(opnd);
      }

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   TRACE (Func_Exit, "just_find_dope_and_rank", NULL);

   return;

}  /* just_find_dope_and_rank */


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

void process_deferred_functions(opnd_type	*opnd)

{
   int		col;
   int		ir_idx;
   int		line;
   int		list_idx;
   opnd_type	loc_opnd;
   int		save_curr_stmt_sh_idx;
   int		sh_idx;

   TRACE (Func_Entry, "process_deferred_functions", NULL);

   find_opnd_line_and_column(opnd, &line, &col);

   switch (OPND_FLD((*opnd))) {
   case IR_Tbl_Idx:

      ir_idx = OPND_IDX((*opnd));

      if (IR_OPR(ir_idx) == Stmt_Expansion_Opr) {
# ifdef _DEBUG
         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, IR_COL_NUM(ir_idx),
                     "no dags", "process_deferred_functions");
         }
# endif
         if (STMT_EXPAND_BEFORE_START_SH(ir_idx)) {

            OPND_FLD(loc_opnd) = SH_Tbl_Idx;
            OPND_IDX(loc_opnd) = STMT_EXPAND_BEFORE_START_SH(ir_idx);
            save_curr_stmt_sh_idx = curr_stmt_sh_idx;
            curr_stmt_sh_idx = STMT_EXPAND_BEFORE_START_SH(ir_idx);
            process_deferred_functions(&loc_opnd);
            curr_stmt_sh_idx = save_curr_stmt_sh_idx;

            sh_idx = STMT_EXPAND_BEFORE_START_SH(ir_idx);
            while (SH_PREV_IDX(sh_idx)) {
               sh_idx = SH_PREV_IDX(sh_idx);
            }
            STMT_EXPAND_BEFORE_START_SH(ir_idx) = sh_idx;

            sh_idx = STMT_EXPAND_BEFORE_END_SH(ir_idx);
            while (SH_NEXT_IDX(sh_idx)) {
               sh_idx = SH_NEXT_IDX(sh_idx);
            }
            STMT_EXPAND_BEFORE_END_SH(ir_idx) = sh_idx;

            insert_sh_chain(STMT_EXPAND_BEFORE_START_SH(ir_idx),
                            STMT_EXPAND_BEFORE_END_SH(ir_idx),
                            Before);
         }

         if (STMT_EXPAND_AFTER_START_SH(ir_idx)) {

            OPND_FLD(loc_opnd) = SH_Tbl_Idx;
            OPND_IDX(loc_opnd) = STMT_EXPAND_AFTER_START_SH(ir_idx);
            save_curr_stmt_sh_idx = curr_stmt_sh_idx;
            curr_stmt_sh_idx = STMT_EXPAND_AFTER_START_SH(ir_idx);
            process_deferred_functions(&loc_opnd);
            curr_stmt_sh_idx = save_curr_stmt_sh_idx;

            sh_idx = STMT_EXPAND_AFTER_START_SH(ir_idx);
            while (SH_PREV_IDX(sh_idx)) {
               sh_idx = SH_PREV_IDX(sh_idx);
            }
            STMT_EXPAND_AFTER_START_SH(ir_idx) = sh_idx;

            sh_idx = STMT_EXPAND_AFTER_END_SH(ir_idx);
            while (SH_NEXT_IDX(sh_idx)) {
               sh_idx = SH_NEXT_IDX(sh_idx);
            }
            STMT_EXPAND_AFTER_END_SH(ir_idx) = sh_idx;

            insert_sh_chain(STMT_EXPAND_AFTER_START_SH(ir_idx),
                            STMT_EXPAND_AFTER_END_SH(ir_idx),
                            After);
         }

         COPY_OPND((*opnd), IR_OPND_L(ir_idx));
         IR_OPND_L(ir_idx) = null_opnd;
/*
         free_stmt_expansion_opr(ir_idx);
*/
      }
      else {
	 if (IR_FLD_L(ir_idx) != SH_Tbl_Idx) {
	    process_deferred_functions(&IR_OPND_L(ir_idx));
	 }

	 if (IR_FLD_R(ir_idx) != SH_Tbl_Idx) {
	    process_deferred_functions(&IR_OPND_R(ir_idx));
	 }
      }
      break;

   case SH_Tbl_Idx:
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx = OPND_IDX((*opnd));

      while (curr_stmt_sh_idx != NULL_IDX) {
         OPND_FLD(loc_opnd) = IR_Tbl_Idx;
         OPND_IDX(loc_opnd) = SH_IR_IDX(curr_stmt_sh_idx);
         process_deferred_functions(&loc_opnd);
         SH_IR_IDX(curr_stmt_sh_idx) = OPND_IDX(loc_opnd);
         curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
      }
      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*opnd));
      while (list_idx) {
	 if (IL_FLD(list_idx) != SH_Tbl_Idx) {
	     process_deferred_functions(&IL_OPND(list_idx));
	 }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;

   }

   TRACE (Func_Exit, "process_deferred_functions", NULL);

   return;

}  /* process_deferred_functions */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Perform short circuiting on Br_True_Opr stmts.                        *|
|*      Assumes that curr_stmt_sh_idx is the branch stmt.                     *|
|*      This routine is only called when there was a function encountered     *|
|*      in the condition, so process_deferred_functions must always be called *|
|*      whether short circuiting is done or not.                              *|
|*      The top operator (after NOT is de'morganed) must be logical .and. or  *|
|*      .or. in order for this routine to short circuit.                      *|
|*      The "opt" setting must be considered here to possibly prevent         *|
|*      any short circuiting.                                                 *|
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

void    short_circuit_branch(void)

{
   int		asg_idx;
   int          br_true_idx;
   int		col;
   int          ir_idx;
   int		label_idx;
   boolean	left_is_worse;
   int		line;
   int		log_idx;
   int          not_cnt		= 0;
   int		not_idx;
   opnd_type    not_opnd;
   opnd_type    opnd;
   int		opnd_column;
   int		opnd_line;
   int          save_curr_stmt_sh_idx;
   int		tmp_idx;


   TRACE (Func_Entry, "short_circuit_branch", NULL);

   br_true_idx = SH_IR_IDX(curr_stmt_sh_idx);

   line = IR_LINE_NUM(br_true_idx);
   col  = IR_COL_NUM(br_true_idx);

   COPY_OPND(opnd, IR_OPND_L(br_true_idx));

   while (OPND_FLD(opnd) == IR_Tbl_Idx) {

      switch(IR_OPR(OPND_IDX(opnd))) {
         case Not_Opr:
            not_cnt++;

            if (not_cnt == 1) {
               COPY_OPND(not_opnd, opnd);
            }
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            break;

         case Or_Opr:
         case And_Opr:

            log_idx = OPND_IDX(opnd);

            if (IR_SHORT_CIRCUIT_L(log_idx)) {
               left_is_worse = TRUE;
            }
# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            else {
               left_is_worse = FALSE;
            }
# else
/* in case we change our minds about short circuiting decisions, save this */
            else if (IR_SHORT_CIRCUIT_R(log_idx)) {
               left_is_worse = FALSE;
            }
            else {
               /* no more functions below this operator. */
               if (not_cnt%2 == 0) {
                  /* nots cancel out */
                  COPY_OPND(IR_OPND_L(br_true_idx), opnd);
               }
               else {
                  COPY_OPND(IR_OPND_L(OPND_IDX(not_opnd)), opnd);
                  COPY_OPND(IR_OPND_L(br_true_idx), not_opnd);
               }
               goto OUT;
            }
# endif

            if (not_cnt%2 == 0) {
               /* nots cancel out */
               COPY_OPND(IR_OPND_L(br_true_idx), opnd);
            }
            else {
               /* demorgan it */
               COPY_OPND(IR_OPND_L(br_true_idx), opnd);

               if (IR_OPR(log_idx) == Or_Opr) {
                  IR_OPR(log_idx) = And_Opr;
               }
               else {
                  IR_OPR(log_idx) = Or_Opr;
               }
               COPY_OPND(IR_OPND_L(OPND_IDX(not_opnd)), 
                         IR_OPND_L(log_idx));
               COPY_OPND(IR_OPND_L(log_idx), not_opnd);

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx) = Not_Opr;
               IR_TYPE_IDX(ir_idx) = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx) = IR_LINE_NUM(OPND_IDX(not_opnd));
               IR_COL_NUM(ir_idx)  = IR_COL_NUM(OPND_IDX(not_opnd));
               COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(log_idx));
               IR_FLD_R(log_idx) = IR_Tbl_Idx;
               IR_IDX_R(log_idx) = ir_idx;
            }

            if (IR_OPR(log_idx) == Or_Opr) {

               /* split condition, share label */

               gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)      = Br_True_Opr;
               IR_TYPE_IDX(ir_idx) = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;


               /* Brian:  This is from s_end.c.	 If I'm wrong about needing   */
               /* the temp, let me know and I'll get rid of it in both places.*/
               /* If we're working on an IF construct expression, transfer the*/
               /* branch-around label to the right operand of the Br_True IR  */
               /* (replacing the IL list).  The IL_OPND is copied to a temp   */
               /* first because sometimes assignments get a little funky      */
               /* using these macros if the target is also being used to      */
               /* access the source.				              */
               /* If we're getting tight on space, could also delete the IL   */
               /* nodes. 						      */

               if (IR_FLD_R(br_true_idx) == IL_Tbl_Idx) {
                  COPY_OPND(opnd, IL_OPND(IR_IDX_R(br_true_idx)));
                  COPY_OPND(IR_OPND_R(ir_idx), opnd);
               }
               else {
                  COPY_OPND(IR_OPND_R(ir_idx), IR_OPND_R(br_true_idx));
               }


               if (left_is_worse) {
                  COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(log_idx));
                  COPY_OPND(IR_OPND_L(br_true_idx), IR_OPND_L(log_idx));
               }
               else {
                  COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(log_idx));
                  COPY_OPND(IR_OPND_L(br_true_idx), IR_OPND_R(log_idx));
               }

               save_curr_stmt_sh_idx = curr_stmt_sh_idx;
               curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

               SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

               short_circuit_branch(); 

               curr_stmt_sh_idx = save_curr_stmt_sh_idx;

               short_circuit_branch();
            }
            else {

               /* generate label */
               label_idx = gen_internal_lbl(stmt_start_line);

               gen_sh(After, Continue_Stmt, line, col, FALSE, TRUE, TRUE);

               NTR_IR_TBL(ir_idx);
               SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
               IR_OPR(ir_idx)              = Label_Opr;
               IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)         = line;
               IR_COL_NUM(ir_idx)          = col;
               IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
               IR_IDX_L(ir_idx)            = label_idx;
               AT_REFERENCED(label_idx)    = Referenced;
               IR_COL_NUM_L(ir_idx)        = col;
               IR_LINE_NUM_L(ir_idx)       = line;

               AT_DEFINED(label_idx)       = TRUE;
               ATL_DEF_STMT_IDX(label_idx) = curr_stmt_sh_idx;

               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
               curr_stmt_sh_idx                = SH_PREV_IDX(curr_stmt_sh_idx);

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)        = Br_True_Opr;
               IR_TYPE_IDX(ir_idx)   = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)   = line;
               IR_COL_NUM(ir_idx)    = col;
               IR_FLD_R(ir_idx)      = AT_Tbl_Idx;
               IR_IDX_R(ir_idx)      = label_idx;
               IR_LINE_NUM_R(ir_idx) = line;
               IR_COL_NUM_R(ir_idx)  = col;

               NTR_IR_TBL(not_idx);
               IR_OPR(not_idx)       = Not_Opr;
               IR_TYPE_IDX(not_idx)  = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(not_idx)  = line;
               IR_COL_NUM(not_idx)   = col;
               IR_FLD_L(ir_idx)      = IR_Tbl_Idx;
               IR_IDX_L(ir_idx)      = not_idx;

               if (left_is_worse) {
                  COPY_OPND(IR_OPND_L(not_idx), IR_OPND_R(log_idx));
                  COPY_OPND(IR_OPND_L(br_true_idx), IR_OPND_L(log_idx));
               }
               else {
                  COPY_OPND(IR_OPND_L(not_idx), IR_OPND_L(log_idx));
                  COPY_OPND(IR_OPND_L(br_true_idx), IR_OPND_R(log_idx));
               }
               
               gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

               save_curr_stmt_sh_idx = curr_stmt_sh_idx;
               curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

               SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

               short_circuit_branch();

               curr_stmt_sh_idx = save_curr_stmt_sh_idx;

               short_circuit_branch();
            }

            goto EXIT;

         case Paren_Opr:
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            break;

         default:
            if (not_cnt%2 == 0) {
               /* nots cancel out */
               COPY_OPND(IR_OPND_L(br_true_idx), opnd);
            }
            else {
               COPY_OPND(IR_OPND_L(OPND_IDX(not_opnd)), opnd);
               COPY_OPND(IR_OPND_L(br_true_idx), not_opnd);
            }

            goto OUT;
      }
   }

OUT:

   COPY_OPND(opnd, IR_OPND_L(br_true_idx));

   /* Brian:								      */
   /* Just a reminder that the following block of code was duped into         */
   /* if_stmt-semantics to avoid short-circuiting the IF conditional          */
   /* expression for the high-level form of IF requested by the Mongoose      */
   /* optimizer.					LRR Oct-Nov, 1997     */

   if (tree_produces_dealloc(&opnd)) { /* make logical tmp asg */
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      find_opnd_line_and_column(&opnd, &opnd_line, &opnd_column);

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,       /* Semantics done */
                           opnd_line,
                           opnd_column,
                           LOGICAL_DEFAULT_TYPE,
                           Priv);

      gen_sh(Before, Assignment_Stmt, opnd_line,
             opnd_column, FALSE, FALSE, TRUE);

      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      process_deferred_functions(&opnd);
      COPY_OPND(IR_OPND_R(asg_idx), opnd);

      IR_FLD_L(br_true_idx)      = AT_Tbl_Idx;
      IR_IDX_L(br_true_idx)      = tmp_idx;
      IR_LINE_NUM_L(br_true_idx) = opnd_line;
      IR_COL_NUM_L(br_true_idx)  = opnd_column;
      curr_stmt_sh_idx           = save_curr_stmt_sh_idx;
   }
   else {
      process_deferred_functions(&opnd);
      COPY_OPND(IR_OPND_L(br_true_idx), opnd);
   }


EXIT:

   TRACE (Func_Exit, "short_circuit_branch", NULL);

   return;

}  /* short_circuit_branch */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Search a subtree to see if it has a variable size function call or    *|
|*      a run time constructor, or dope vector result intrinsic.              *|
|*      All of these produce some sort of dealloc stmt (or stmts) after       *|
|*      the current stmt. The result of the tree must be pulled into a        *|
|*      logical tmp if this routine returns TRUE so that the dealloc          *|
|*      stmts are executed before any branch occurs.                          *|
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

boolean tree_produces_dealloc(opnd_type	*root)

{
   int		i;
   int		list_idx;
   opnd_type    opnd;
   boolean 	has_dealloc = FALSE;


   TRACE (Func_Entry, "tree_produces_dealloc", NULL);

   if (OPND_FLD((*root)) == IR_Tbl_Idx) {

      if (IR_OPR(OPND_IDX((*root))) == Stmt_Expansion_Opr) {

         if (STMT_EXPAND_AFTER_START_SH(OPND_IDX((*root))) != NULL_IDX) {
            has_dealloc = TRUE;
         }
      }
      else if (IR_OPR(OPND_IDX((*root))) == Array_Construct_Opr ||
               IR_OPR(OPND_IDX((*root))) == Adjustl_Opr         ||
               IR_OPR(OPND_IDX((*root))) == Adjustr_Opr)        {

         has_dealloc = TRUE;
         goto EXIT;
      }
      else {

         if (IR_FLD_L(OPND_IDX((*root))) == IR_Tbl_Idx ||
             IR_FLD_L(OPND_IDX((*root))) == IL_Tbl_Idx) {

            COPY_OPND(opnd, IR_OPND_L(OPND_IDX((*root))));
            has_dealloc = tree_produces_dealloc(&opnd);

            if (has_dealloc) {
               goto EXIT;
            }
         }

         if (IR_FLD_R(OPND_IDX((*root))) == IR_Tbl_Idx ||
             IR_FLD_R(OPND_IDX((*root))) == IL_Tbl_Idx) {

            COPY_OPND(opnd, IR_OPND_R(OPND_IDX((*root))));
            has_dealloc = tree_produces_dealloc(&opnd);

            if (has_dealloc) {
               goto EXIT;
            }
         }
      }
   }
   else if (OPND_FLD((*root)) == IL_Tbl_Idx) {

      list_idx = OPND_IDX((*root));
 
      for (i = 0; i < OPND_LIST_CNT((*root)); i++) {

         if (IL_FLD(list_idx) == IR_Tbl_Idx ||
             IL_FLD(list_idx) == IL_Tbl_Idx) {

            COPY_OPND(opnd, IL_OPND(list_idx));
            has_dealloc = tree_produces_dealloc(&opnd);

            if (has_dealloc) {
               goto EXIT;
            }
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

EXIT:

   TRACE (Func_Exit, "tree_produces_dealloc", NULL);

   return(has_dealloc);

}  /* tree_produces_dealloc */

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

void create_loop_stmts(int		lcv_attr,
		       opnd_type       *start_opnd,
		       opnd_type       *end_opnd,
		       opnd_type       *inc_opnd,
		       int		body_start_sh_idx,
		       int		body_end_sh_idx)

{
   int		       col;
   int		       ir_idx;
   int		       line;
   int		       save_curr_stmt_sh_idx;

# if !defined(_HIGH_LEVEL_DO_LOOP_FORM)
   int		       asg_idx;
   int		       br_around_label;
   int		       br_back_label;
   int		       div_idx;
   opnd_type	       end_tmp_opnd;
   expr_arg_type       exp_desc;
   opnd_type	       inc_tmp_opnd;
   int		       log_idx;
   int		       minus_idx;
   int		       mult_idx;
   opnd_type           opnd;
   int		       opnd_col;
   int		       opnd_line;
   int		       plus_idx;
   cif_usage_code_type save_xref_state;
   opnd_type	       start_tmp_opnd;
   int		       tmp_idx;
   opnd_type	       trip_count_tmp_opnd;
   opnd_type	       trip_counter_tmp_opnd;
# else
   int		       list_idx;
   int		       list_idx2;
# endif


   TRACE (Func_Entry, "create_loop_stmts", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   line = stmt_start_line;
   col  = stmt_start_col;

# if defined(_HIGH_LEVEL_DO_LOOP_FORM)
   curr_stmt_sh_idx = body_end_sh_idx;

   ir_idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
               Loop_End_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   gen_sh(After, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(curr_stmt_sh_idx)     = ir_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   SH_LOOP_END(curr_stmt_sh_idx) = TRUE;

   curr_stmt_sh_idx = body_start_sh_idx;

   NTR_IR_LIST_TBL(list_idx);
   gen_opnd(&IL_OPND(list_idx), lcv_attr, AT_Tbl_Idx, line, col);

   NTR_IR_LIST_TBL(list_idx2);
   IL_NEXT_LIST_IDX(list_idx) = list_idx2;
   IL_PREV_LIST_IDX(list_idx2) = list_idx;

   COPY_OPND(IL_OPND(list_idx2), (*start_opnd));

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
   list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

   COPY_OPND(IL_OPND(list_idx2), (*end_opnd));

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
   list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

   COPY_OPND(IL_OPND(list_idx2), (*inc_opnd));


   ir_idx = gen_ir(SH_Tbl_Idx, SH_NEXT_IDX(body_end_sh_idx),
               Loop_Info_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                   IL_Tbl_Idx, list_idx);

   gen_sh(Before, Do_Iterative_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   SH_PARENT_BLK_IDX(SH_NEXT_IDX(body_end_sh_idx)) = 
                                           SH_PREV_IDX(curr_stmt_sh_idx);

# else
   /***************************************************************************\
   |* branch around label. Do this first.                                     *|
   \***************************************************************************/

   curr_stmt_sh_idx = body_end_sh_idx;

   br_around_label = gen_internal_lbl(line);

   ir_idx = gen_ir(AT_Tbl_Idx, br_around_label,
               Label_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   gen_sh(After, Continue_Stmt, line, col, FALSE, TRUE, TRUE);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   AT_DEFINED(br_around_label)       = TRUE;
   ATL_DEF_STMT_IDX(br_around_label) = curr_stmt_sh_idx;

   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;


   /***************************************************************************\
   |***************************************************************************|
   |**                        PREFIX CODE                                    **|
   |***************************************************************************|
   \***************************************************************************/

   curr_stmt_sh_idx = body_start_sh_idx;


   /***************************************************************************\
   |* temp = start value                                                      *|
   \***************************************************************************/

   if (OPND_FLD((*start_opnd)) == CN_Tbl_Idx &&
       TYP_LINEAR(CN_TYPE_IDX(OPND_IDX((*start_opnd)))) == 
                                                       Short_Typeless_Const) {

      find_opnd_line_and_column(start_opnd, &opnd_line, &opnd_col);
      OPND_IDX((*start_opnd)) = cast_typeless_constant(OPND_IDX((*start_opnd)),
                                                       ATD_TYPE_IDX(lcv_attr),
                                                       opnd_line,
                                                       opnd_col);
   }

   if (OPND_FLD((*start_opnd)) == CN_Tbl_Idx ||
       (OPND_FLD((*start_opnd)) == AT_Tbl_Idx &&
        ATD_CLASS(OPND_IDX((*start_opnd))) == Compiler_Tmp)) {

      COPY_OPND(start_tmp_opnd, (*start_opnd));
   }
   else {

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,	/* Semantics done */
                           line,
                           col,
                           ATD_TYPE_IDX(lcv_attr),
                           Priv);

      COPY_OPND(IR_OPND_R(asg_idx), (*start_opnd));

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      gen_opnd(&start_tmp_opnd, tmp_idx, AT_Tbl_Idx, line, col);
   }
   
   /***************************************************************************\
   |* temp = end value                                                        *|
   \***************************************************************************/

   if (OPND_FLD((*end_opnd)) == CN_Tbl_Idx &&
       TYP_LINEAR(CN_TYPE_IDX(OPND_IDX((*end_opnd)))) ==
                                                       Short_Typeless_Const) {

      find_opnd_line_and_column(end_opnd, &opnd_line, &opnd_col);
      OPND_IDX((*end_opnd)) = cast_typeless_constant(OPND_IDX((*end_opnd)),
                                                       ATD_TYPE_IDX(lcv_attr),
                                                       opnd_line,
                                                       opnd_col);
   }

   if (OPND_FLD((*end_opnd)) == CN_Tbl_Idx ||
       (OPND_FLD((*end_opnd)) == AT_Tbl_Idx &&
        ATD_CLASS(OPND_IDX((*end_opnd))) == Compiler_Tmp)) {

      COPY_OPND(end_tmp_opnd, (*end_opnd));
   }
   else {

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,	/* Semantics done */
                           line,
                           col,
                           ATD_TYPE_IDX(lcv_attr),
                           Priv);

      COPY_OPND(IR_OPND_R(asg_idx), (*end_opnd));

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      gen_opnd(&end_tmp_opnd, tmp_idx, AT_Tbl_Idx, line, col);
   }

   /***************************************************************************\
   |* temp = increment value                                                  *|
   \***************************************************************************/

   if (OPND_FLD((*inc_opnd)) == CN_Tbl_Idx &&
       TYP_LINEAR(CN_TYPE_IDX(OPND_IDX((*inc_opnd)))) ==
                                                       Short_Typeless_Const) {

      find_opnd_line_and_column(inc_opnd, &opnd_line, &opnd_col);
      OPND_IDX((*inc_opnd)) = cast_typeless_constant(OPND_IDX((*inc_opnd)),
                                                       ATD_TYPE_IDX(lcv_attr),
                                                       opnd_line,
                                                       opnd_col);
   }

   if (OPND_FLD((*inc_opnd)) == CN_Tbl_Idx ||
       (OPND_FLD((*inc_opnd)) == AT_Tbl_Idx &&
        ATD_CLASS(OPND_IDX((*inc_opnd))) == Compiler_Tmp)) {

      COPY_OPND(inc_tmp_opnd, (*inc_opnd));
   }
   else {

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,	/* Semantics done */
                           line,
                           col,
                           ATD_TYPE_IDX(lcv_attr),
                           Priv);

      COPY_OPND(IR_OPND_R(asg_idx), (*inc_opnd));

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      gen_opnd(&inc_tmp_opnd, tmp_idx, AT_Tbl_Idx, line, col);
   }

   /***************************************************************************\
   |* lcv attr = start temp                                                   *|
   \***************************************************************************/

   asg_idx = gen_ir(AT_Tbl_Idx, lcv_attr,
               Asg_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                    OPND_FLD(start_tmp_opnd), OPND_IDX(start_tmp_opnd));

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /***************************************************************************\
   |* temp = trip count expression. ((end - start) + inc)/inc                 *|
   \***************************************************************************/


   minus_idx = gen_ir(OPND_FLD(end_tmp_opnd), OPND_IDX(end_tmp_opnd),
                 Minus_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                      OPND_FLD(start_tmp_opnd), OPND_IDX(start_tmp_opnd));

   plus_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Plus_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                     OPND_FLD(inc_tmp_opnd), OPND_IDX(inc_tmp_opnd));

   div_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                 Div_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                    OPND_FLD(inc_tmp_opnd), OPND_IDX(inc_tmp_opnd));

   OPND_FLD(opnd) = IR_Tbl_Idx;
   OPND_IDX(opnd) = div_idx;

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   expr_semantics(&opnd, &exp_desc);
   xref_state      = save_xref_state;

   if (OPND_FLD(opnd) == CN_Tbl_Idx ||
       (OPND_FLD(opnd) == AT_Tbl_Idx &&
        ATD_CLASS(OPND_IDX(opnd)) == Compiler_Tmp)) {

      COPY_OPND(trip_count_tmp_opnd, opnd);
   }
   else {

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,	/* Semantics done */
                           line,
                           col,
                           exp_desc.type_idx,
                           Priv);

      COPY_OPND(IR_OPND_R(asg_idx), opnd);

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
   
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      gen_opnd(&trip_count_tmp_opnd, tmp_idx, AT_Tbl_Idx, line, col);
   }


   /***************************************************************************\
   |* branch around test for trip count <= 0                                  *|
   \***************************************************************************/

   log_idx = gen_ir(OPND_FLD(trip_count_tmp_opnd),OPND_IDX(trip_count_tmp_opnd),
                Le_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                    CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

   ir_idx = gen_ir(IR_Tbl_Idx, log_idx,
               Br_True_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   AT_Tbl_Idx, br_around_label);

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /***************************************************************************\
   |* trip counter temp = 0                                                   *|
   \***************************************************************************/

   GEN_COMPILER_TMP_ASG(asg_idx,
                        tmp_idx,
                        TRUE,	/* Semantics done */
                        line,
                        col,
                        CG_INTEGER_DEFAULT_TYPE,
                        Priv);

   gen_opnd(&IR_OPND_R(asg_idx), CN_INTEGER_ZERO_IDX, CN_Tbl_Idx, line, col);

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   gen_opnd(&trip_counter_tmp_opnd, tmp_idx, AT_Tbl_Idx, line, col);

   /***************************************************************************\
   |* branch back label                                                       *|
   \***************************************************************************/

   br_back_label = gen_internal_lbl(line);

   ir_idx = gen_ir(AT_Tbl_Idx, br_back_label,
               Label_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   gen_sh(Before, Continue_Stmt, line, col, FALSE, TRUE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

   AT_DEFINED(br_back_label)       = TRUE;
   ATL_DEF_STMT_IDX(br_back_label) = SH_PREV_IDX(curr_stmt_sh_idx);

   if (in_constructor) {
      ATL_CONSTRUCTOR_LOOP(br_back_label) = TRUE;
   }

   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /***************************************************************************\
   |* lcv attr = start temp + (trip counter temp * increment temp)            *|
   \***************************************************************************/

   mult_idx = gen_ir(OPND_FLD(trip_counter_tmp_opnd), 
                                     OPND_IDX(trip_counter_tmp_opnd),
                 Mult_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                     OPND_FLD(inc_tmp_opnd), OPND_IDX(inc_tmp_opnd));

   plus_idx = gen_ir(OPND_FLD(start_tmp_opnd), OPND_IDX(start_tmp_opnd),
                 Plus_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                     IR_Tbl_Idx, mult_idx);

   asg_idx = gen_ir(AT_Tbl_Idx, lcv_attr,
                Asg_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                    IR_Tbl_Idx, plus_idx);

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;



   /***************************************************************************\
   |***************************************************************************|
   |**                        SUFFIX CODE                                    **|
   |***************************************************************************|
   \***************************************************************************/

   curr_stmt_sh_idx = body_end_sh_idx;

   /***************************************************************************\
   |* trip counter temp = trip counter temp + 1                               *|
   \***************************************************************************/

   plus_idx = gen_ir(OPND_FLD(trip_counter_tmp_opnd), 
                                  OPND_IDX(trip_counter_tmp_opnd),
                 Plus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                     CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

   asg_idx = gen_ir(OPND_FLD(trip_counter_tmp_opnd), 
                                  OPND_IDX(trip_counter_tmp_opnd),
                Asg_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                    IR_Tbl_Idx, plus_idx);

   gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   /***************************************************************************\
   |* branch back test for trip counter temp < trip count                     *|
   \***************************************************************************/

   log_idx = gen_ir(OPND_FLD(trip_counter_tmp_opnd),
                             OPND_IDX(trip_counter_tmp_opnd),
                Lt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   OPND_FLD(trip_count_tmp_opnd),OPND_IDX(trip_count_tmp_opnd));

   ir_idx = gen_ir(IR_Tbl_Idx, log_idx,
               Br_True_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   AT_Tbl_Idx, br_back_label);

   gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(curr_stmt_sh_idx)     = ir_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;


   /***************************************************************************\
   |* lcv attr = start temp + (trip count temp * increment temp)              *|
   \***************************************************************************/

   mult_idx =gen_ir(OPND_FLD(trip_count_tmp_opnd),OPND_IDX(trip_count_tmp_opnd),
                 Mult_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                     OPND_FLD(inc_tmp_opnd), OPND_IDX(inc_tmp_opnd));

   plus_idx = gen_ir(OPND_FLD(start_tmp_opnd), OPND_IDX(start_tmp_opnd),
                 Plus_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                     IR_Tbl_Idx, mult_idx);

   asg_idx = gen_ir(AT_Tbl_Idx, lcv_attr,
                Asg_Opr, ATD_TYPE_IDX(lcv_attr), line, col,
                    IR_Tbl_Idx, plus_idx);

   gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

# endif


   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "create_loop_stmts", NULL);

   return;

}  /* create_loop_stmts */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create an array bounds table entry from an expr_arg_type arg          *|
|*      All bounds info must be constant.                                     *|
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

int	create_bd_ntry_for_const(expr_arg_type	*exp_desc,
                                 int		 line,
				 int		 col)

{
   int			bd_idx;
   size_offset_type	extent;
   int			i;
   size_offset_type	num_elements;
   size_offset_type	stride;


   TRACE (Func_Entry, "create_bd_ntry_for_const", NULL);

   bd_idx			= reserve_array_ntry(exp_desc->rank);
   BD_RANK(bd_idx)		= exp_desc->rank;
   BD_LINE_NUM(bd_idx)		= line;
   BD_COLUMN_NUM(bd_idx)	= col;
   BD_ARRAY_SIZE(bd_idx)	= Constant_Size;
   BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
   BD_RESOLVED(bd_idx)		= TRUE;

   num_elements.idx	   	= CN_INTEGER_ONE_IDX;
   num_elements.fld		= CN_Tbl_Idx;

   for (i = 1; i <= exp_desc->rank; i++) {
      BD_LB_FLD(bd_idx,i) = CN_Tbl_Idx;
      BD_LB_IDX(bd_idx,i) = CN_INTEGER_ONE_IDX;

      if (OPND_FLD(exp_desc->shape[i-1]) == CN_Tbl_Idx) {
         BD_UB_FLD(bd_idx,i) = OPND_FLD(exp_desc->shape[i-1]);
         BD_UB_IDX(bd_idx,i) = OPND_IDX(exp_desc->shape[i-1]);
      }
      else {
         PRINTMSG(line, 966, Internal, col);
      }

      BD_XT_FLD(bd_idx,i) = BD_UB_FLD(bd_idx,i);
      BD_XT_IDX(bd_idx,i) = BD_UB_IDX(bd_idx,i);

      extent.fld	= BD_XT_FLD(bd_idx,i);
      extent.idx	= BD_XT_IDX(bd_idx,i);

      size_offset_binary_calc(&extent,
                              &num_elements,
                               Mult_Opr,
                              &num_elements);
   }

   if (num_elements.fld == NO_Tbl_Idx) {
      BD_LEN_FLD(bd_idx) = CN_Tbl_Idx;
      BD_LEN_IDX(bd_idx) = ntr_const_tbl(num_elements.type_idx,
                                         FALSE,
                                         num_elements.constant);
   }
   else {
      BD_LEN_FLD(bd_idx) = num_elements.fld;
      BD_LEN_IDX(bd_idx) = num_elements.idx;
   }

   /* fill in stride multipliers now */

   set_stride_for_first_dim(exp_desc->type_idx, &stride);

   BD_SM_FLD(bd_idx, 1) = stride.fld;
   BD_SM_IDX(bd_idx, 1) = stride.idx;

   for (i = 2; i <= BD_RANK(bd_idx); i++) {
      extent.fld	= BD_XT_FLD(bd_idx,i-1);
      extent.idx	= BD_XT_IDX(bd_idx,i-1);

      size_offset_binary_calc(&extent, &stride, Mult_Opr, &stride);

      if (stride.fld == NO_Tbl_Idx) {
         stride.fld	= CN_Tbl_Idx;
         stride.idx	= ntr_const_tbl(stride.type_idx,
                                        FALSE,
                                        stride.constant);
      }

      BD_SM_FLD(bd_idx, i)	= stride.fld;
      BD_SM_IDX(bd_idx, i)	= stride.idx;
   }

   bd_idx =  ntr_array_in_bd_tbl(bd_idx);

   TRACE (Func_Exit, "create_bd_ntry_for_const", NULL);

   return(bd_idx);

}  /* create_bd_ntry_for_const */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Fold the clen_opr if possible.                                        *|
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

void fold_clen_opr(opnd_type		*opnd,
		   expr_arg_type	*exp_desc)

{
   int		attr_idx;
   int		clen_idx;
   int		col;
   int		ir_idx;
   int		line;
   int		list_idx;
   int		shift_idx;
   int		type_idx;


   TRACE (Func_Entry, "fold_clen_opr", NULL);

   find_opnd_line_and_column(opnd, &line, &col);

   if (OPND_FLD((*opnd)) != IR_Tbl_Idx ||
       IR_OPR(OPND_IDX((*opnd))) != Clen_Opr) {

      goto EXIT;
   }

   clen_idx = OPND_IDX((*opnd));

   exp_desc->type_idx    = IR_TYPE_IDX(clen_idx);
   exp_desc->type        = TYP_TYPE(exp_desc->type_idx);
   exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);

   switch (IR_FLD_L(clen_idx)) {
      case AT_Tbl_Idx :
         attr_idx = IR_IDX_L(clen_idx);

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             (ATD_IM_A_DOPE(attr_idx)    ||
              ATD_POINTER(attr_idx)      ||
              ATD_ALLOCATABLE(attr_idx))) {

            if (char_len_in_bytes) {

               /* the length is already in bytes for solaris */

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)           = Dv_Access_El_Len;
               IR_TYPE_IDX(ir_idx)      = SA_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)      = line;
               IR_COL_NUM(ir_idx)       = col;
               COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(clen_idx));

               OPND_FLD((*opnd)) = IR_Tbl_Idx;
               OPND_IDX((*opnd)) = ir_idx;
            }
            else {

               /* must shift the bits to bytes */

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)           = Dv_Access_El_Len;
               IR_TYPE_IDX(ir_idx)      = SA_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)      = line;
               IR_COL_NUM(ir_idx)       = col;
               COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(clen_idx));
               NTR_IR_TBL(shift_idx);
               IR_OPR(shift_idx)        = Shiftr_Opr;
               IR_TYPE_IDX(shift_idx)   = SA_INTEGER_DEFAULT_TYPE;
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

               IL_FLD(list_idx)      = CN_Tbl_Idx;
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx)  = col;
               IL_IDX(list_idx) = CN_INTEGER_THREE_IDX;
   
               OPND_FLD((*opnd)) = IR_Tbl_Idx;
               OPND_IDX((*opnd)) = shift_idx;
            }

            exp_desc->type_idx = CG_INTEGER_DEFAULT_TYPE;
            exp_desc->type     = Integer;
            exp_desc->linear_type = CG_INTEGER_DEFAULT_TYPE;
         }
         break;

      case CN_Tbl_Idx :
         type_idx		= CN_TYPE_IDX(IR_IDX_L(clen_idx));
         OPND_FLD((*opnd))	= TYP_FLD(type_idx);
         OPND_IDX((*opnd))	= TYP_IDX(type_idx);
         OPND_LINE_NUM((*opnd)) = line;
         OPND_COL_NUM((*opnd))  = col;
         exp_desc->constant	= TRUE;
         exp_desc->foldable	= TRUE;

         if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
            exp_desc->type_idx = CN_TYPE_IDX(TYP_IDX(type_idx));
         }
         else {
            exp_desc->type_idx = ATD_TYPE_IDX(TYP_IDX(type_idx));
         }

         exp_desc->type = TYP_TYPE(exp_desc->type_idx);
         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
         break;

      case IR_Tbl_Idx :

         ir_idx = IR_IDX_L(clen_idx);

         if ((IR_OPR(ir_idx) == Substring_Opr        ||
              IR_OPR(ir_idx) == Whole_Substring_Opr)  &&
             IL_FLD(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)))) 
                                                             != NO_Tbl_Idx) {

            COPY_OPND((*opnd), IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                    IR_IDX_R(ir_idx)))));

            if (OPND_FLD((*opnd)) == CN_Tbl_Idx) {
               exp_desc->type_idx = CN_TYPE_IDX(OPND_IDX((*opnd)));
               exp_desc->constant = TRUE;
               exp_desc->foldable = TRUE;
            }
            else if (OPND_FLD((*opnd)) == IR_Tbl_Idx) {
               exp_desc->type_idx = IR_TYPE_IDX(OPND_IDX((*opnd)));
            }
            else if (OPND_FLD((*opnd)) == AT_Tbl_Idx) {
               exp_desc->type_idx = ATD_TYPE_IDX(OPND_IDX((*opnd)));
            }

            exp_desc->type = TYP_TYPE(exp_desc->type_idx);
            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
         }
         break;
   }

EXIT:

   TRACE (Func_Exit, "fold_clen_opr", NULL);

   return;

}  /* fold_clen_opr */

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

void set_shape_for_deferred_funcs(expr_arg_type         *exp_desc,
                                  int                    call_idx)

{
   int          	attr_idx;
   int          	bd_idx;
   int			ch_idx = NULL_IDX;
   int			col;
   int			dummy_idx;
   boolean		has_sf = FALSE;
   int			i;
   int			ir_idx;
   int			line;
   int          	list_idx;
   expr_arg_type	loc_exp_desc;
   int          	minus_idx;
   opnd_type    	opnd;
   int          	plus_idx;
   int			pgm_idx;
   cif_usage_code_type  save_xref_state;
   int			sn_idx;


   TRACE (Func_Entry, "set_shape_for_deferred_funcs", NULL);

   pgm_idx = IR_IDX_L(call_idx);
   attr_idx = ATP_RSLT_IDX(IR_IDX_L(call_idx));
   bd_idx = ATD_ARRAY_IDX(attr_idx);

   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
      ch_idx = ATD_TYPE_IDX(attr_idx);
   }

   if ((bd_idx && BD_ARRAY_SIZE(bd_idx) == Var_Len_Array)  ||
       (ch_idx && TYP_FLD(ch_idx) == AT_Tbl_Idx)) {

      has_sf = TRUE;

      /* set up the dummy args as stmt func dargs */

      list_idx = IR_IDX_R(call_idx);
      sn_idx = ATP_FIRST_IDX(pgm_idx);

      if (ATP_EXTRA_DARG(pgm_idx)) {
         sn_idx++;
      }

      for (i = 0; i < IR_LIST_CNT_R(call_idx); i++) {
         dummy_idx = SN_ATTR_IDX(sn_idx);

         ATD_SF_DARG(dummy_idx) = TRUE;

         ATD_SF_LINK(dummy_idx) = IL_ARG_DESC_IDX(list_idx);
         COPY_OPND(opnd, IL_OPND(list_idx));

         if (arg_info_list[ATD_SF_LINK(dummy_idx)].ed.reference &&
             OPND_FLD(opnd) == IR_Tbl_Idx)                      {

            if (IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr) {
               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            }

            if (OPND_FLD(opnd) == IR_Tbl_Idx &&
                IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr) {

               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            }

           /* whole subscript and substring need to be removed       */
           /* since we don't know how these args will be referenced. */
           /* I don't think dv_deref_oprs need to be removed.        */
         }

         ATD_FLD(dummy_idx)        = OPND_FLD(opnd);
         ATD_SF_ARG_IDX(dummy_idx) = OPND_IDX(opnd);

         sn_idx++;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   line = IR_LINE_NUM(call_idx);
   col  = IR_COL_NUM(call_idx);

   if (ch_idx) {
      /* fill in exp_desc->char_len */

      if (TYP_CHAR_CLASS(ch_idx) == Const_Len_Char) {
         exp_desc->char_len.fld = TYP_FLD(ch_idx);
         exp_desc->char_len.idx = TYP_IDX(ch_idx);
      }
      else if (TYP_FLD(ch_idx) == AT_Tbl_Idx) {

         if (TYP_CHAR_CLASS(ch_idx) == Assumed_Size_Char) {
            /* TYP_ORIG_LEN_IDX not set for Assumed_Size_Char */
            COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(TYP_IDX(ch_idx))));
         }
         else {
            COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(TYP_ORIG_LEN_IDX(ch_idx))));
         }

         copy_subtree(&opnd, &opnd);

         loc_exp_desc.rank = 0;
         save_xref_state   = xref_state;
         xref_state        = CIF_No_Usage_Rec;
         expr_semantics(&opnd, &loc_exp_desc);
         xref_state        = save_xref_state;

         COPY_OPND((exp_desc->char_len), opnd);
      }
   }

   if (bd_idx) {

      switch (BD_ARRAY_CLASS(bd_idx)) {

         case Explicit_Shape :

            if (BD_ARRAY_SIZE(bd_idx) == Constant_Size)   {
               get_shape_from_attr(exp_desc, 
                                   attr_idx,
                                   exp_desc->rank,
                                   IR_LINE_NUM(call_idx),
                                   IR_COL_NUM(call_idx));
            }
            else if (BD_ARRAY_SIZE(bd_idx) == Var_Len_Array) {

               /* set up extent expression for each dim */

               for (i = 0; i < BD_RANK(bd_idx); i++) {

                  NTR_IR_TBL(plus_idx);
                  IR_OPR(plus_idx) = Plus_Opr;
                  IR_TYPE_IDX(plus_idx)   = CG_INTEGER_DEFAULT_TYPE;
                  IR_LINE_NUM(plus_idx) = line;
                  IR_COL_NUM(plus_idx) = col;

                  IR_FLD_R(plus_idx) = CN_Tbl_Idx;
                  IR_IDX_R(plus_idx) = CN_INTEGER_ONE_IDX;
                  IR_LINE_NUM_R(plus_idx) = line;
                  IR_COL_NUM_R(plus_idx) = col;

                  NTR_IR_TBL(minus_idx);
                  IR_OPR(minus_idx) = Minus_Opr;
                  IR_TYPE_IDX(minus_idx)   = CG_INTEGER_DEFAULT_TYPE;
                  IR_LINE_NUM(minus_idx) = line;
                  IR_COL_NUM(minus_idx) = col;

                  IR_FLD_L(plus_idx) = IR_Tbl_Idx;
                  IR_IDX_L(plus_idx) = minus_idx;

                  if (BD_LB_FLD(bd_idx,i+1) == AT_Tbl_Idx) {
                     COPY_OPND(IR_OPND_R(minus_idx), 
                               IR_OPND_R(ATD_TMP_IDX(BD_LB_IDX(bd_idx,i+1))));
                  }
                  else {
                     IR_FLD_R(minus_idx) = BD_LB_FLD(bd_idx, i+1);
                     IR_IDX_R(minus_idx) = BD_LB_IDX(bd_idx, i+1);
                     IR_LINE_NUM_R(minus_idx) = line;
                     IR_COL_NUM_R(minus_idx) = col;
                  }

                  COPY_OPND(opnd, IR_OPND_R(minus_idx));
                  copy_subtree(&opnd, &opnd);
                  COPY_OPND(IR_OPND_R(minus_idx), opnd);

                  if (BD_UB_FLD(bd_idx,i+1) == AT_Tbl_Idx) {
                     COPY_OPND(IR_OPND_L(minus_idx), 
                               IR_OPND_R(ATD_TMP_IDX(BD_UB_IDX(bd_idx,i+1))));
                  }
                  else {
                     IR_FLD_L(minus_idx) = BD_UB_FLD(bd_idx, i+1);
                     IR_IDX_L(minus_idx) = BD_UB_IDX(bd_idx, i+1);
                     IR_LINE_NUM_L(minus_idx) = line;
                     IR_COL_NUM_L(minus_idx) = col;
                  }

                  COPY_OPND(opnd, IR_OPND_L(minus_idx));
                  copy_subtree(&opnd, &opnd);
                  COPY_OPND(IR_OPND_L(minus_idx), opnd);

                  OPND_FLD(opnd) = IR_Tbl_Idx;
                  OPND_IDX(opnd) = plus_idx;

                  loc_exp_desc.rank = 0;
                  save_xref_state   = xref_state;
                  xref_state        = CIF_No_Usage_Rec;
                  expr_semantics(&opnd, &loc_exp_desc);
                  xref_state        = save_xref_state;

                  COPY_OPND((exp_desc->shape[i]), opnd);
                  SHAPE_FOLDABLE(exp_desc->shape[i]) = loc_exp_desc.foldable;
                  SHAPE_WILL_FOLD_LATER(exp_desc->shape[i]) = 
                                               loc_exp_desc.will_fold_later;
               }
            }
            break;

         case Assumed_Size   :
            /* don't know what to do here */
            /* probable shouldn't get here */
            PRINTMSG(IR_LINE_NUM(call_idx), 968, Internal,
                     IR_COL_NUM(call_idx));

            break;

         case Deferred_Shape :
         case Assumed_Shape  :

            /* these are dope vectors */

            for (i = 0; i < BD_RANK(bd_idx); i++) {

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx) = Dv_Access_Extent;
               IR_TYPE_IDX(ir_idx)   = SA_INTEGER_DEFAULT_TYPE;
               IR_DV_DIM(ir_idx) = i + 1;

               IR_FLD_L(ir_idx) = AT_Tbl_Idx;
               IR_IDX_L(ir_idx) = attr_idx;

               IR_LINE_NUM(ir_idx) = IR_LINE_NUM(call_idx);
               IR_COL_NUM(ir_idx) = IR_COL_NUM(call_idx);
               IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(call_idx);
               IR_COL_NUM_L(ir_idx) = IR_COL_NUM(call_idx);

               exp_desc->shape[i].fld = IR_Tbl_Idx;
               exp_desc->shape[i].idx = ir_idx;
               SHAPE_FOLDABLE(exp_desc->shape[i]) = FALSE;
               SHAPE_WILL_FOLD_LATER(exp_desc->shape[i]) = FALSE;
            }
            break;

   
      }
   }

   if (has_sf) {
      sn_idx = ATP_FIRST_IDX(pgm_idx);

      if (ATP_EXTRA_DARG(pgm_idx)) {
         sn_idx++;
      }

      for (i = 0; i < IR_LIST_CNT_R(call_idx); i++) {
         ATD_SF_DARG(SN_ATTR_IDX(sn_idx)) = FALSE;
         sn_idx++;
      }
   }


   TRACE (Func_Exit, "set_shape_for_deferred_funcs", NULL);

   return;

}  /* set_shape_for_deferred_funcs */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Create an internal dope vector for use in folding array intrinsics.   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      dope_vec - address of internal dope vector to fill in.                *|
|*      r_opnd   - address of opnd pointing to "target".                      *|
|*      just_init- TRUE => just initialize header.                            *|
|*      exp_desc - address of the expression descriptor of target.            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

boolean	gen_internal_dope_vector(int_dope_type		*dope_vec,
				 opnd_type		*r_opnd,
				 boolean		 just_init,
				 expr_arg_type		*exp_desc)

{
#ifdef KEY /* Bug 10177 */
   int			bd_idx = 0;
   int			cn_idx = 0;
#else /* KEY Bug 10177 */
   int			bd_idx;
   int			cn_idx;
#endif /* KEY Bug 10177 */
   int			column;
   long_type         	constant[2];
   int          	i;
# if defined(_HOST_OS_UNICOS) && defined(_TARGET_OS_UNICOS)
   _fcd                 fcd_r;
# endif
   int			line;
   boolean		ok		= TRUE;
   opnd_type		opnd;
   int          	type_idx;


   TRACE (Func_Entry, "gen_internal_dope_vector", NULL);

   type_idx = exp_desc->type_idx;

   /*********************************************\
   |* see if we need to assign r_opnd to a tmp. *|
   \*********************************************/

   if (just_init) {
      /* intentionally blank */
   }
   else if (OPND_FLD((*r_opnd)) == CN_Tbl_Idx) {
      cn_idx = OPND_IDX((*r_opnd));
   }
   else if ((exp_desc->reference  ||
             exp_desc->tmp_reference) &&
            ! exp_desc->section)      {

      COPY_OPND(opnd, (*r_opnd));

      while (OPND_FLD(opnd) == IR_Tbl_Idx) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (ATD_FLD(OPND_IDX(opnd)) == IR_Tbl_Idx) {
         COPY_OPND(opnd, (*r_opnd));

         if (fold_aggragate_expression(&opnd, exp_desc, TRUE)) {
            cn_idx = OPND_IDX(opnd);

            if (exp_desc->rank) {
               bd_idx = create_bd_ntry_for_const(exp_desc,
                                                 stmt_start_line,
                                                 stmt_start_col);
            }
         }
         else {
            ok = FALSE;
            goto EXIT;
         }
      }
      else {
         if (ATD_CLASS(OPND_IDX(opnd)) == Constant) {
            cn_idx = ATD_CONST_IDX(OPND_IDX(opnd));
         }
         else {
            cn_idx = ATD_TMP_IDX(OPND_IDX(opnd));
         }

         bd_idx = ATD_ARRAY_IDX(OPND_IDX(opnd));
      }
   }
   else {
      COPY_OPND(opnd, (*r_opnd));

      if (fold_aggragate_expression(&opnd, exp_desc, TRUE)) {
         cn_idx = OPND_IDX(opnd);

         if (exp_desc->rank) {
            bd_idx = create_bd_ntry_for_const(exp_desc, 
                                              stmt_start_line,
                                              stmt_start_col);
         }
      }
      else {
         ok = FALSE;
         goto EXIT;
      }
   }

# ifdef _TARGET_OS_MAX  /* BRIANJ */
   if (! just_init &&
       TYP_LINEAR(CN_TYPE_IDX(cn_idx)) == Complex_4) {
      /* must pack it into one word (an Integer_8 constant) */

      constant[0] = CN_CONST(cn_idx) << 32;
      constant[0] |= (CP_CONSTANT(CN_POOL_IDX(cn_idx) + 1) & 0xFFFFFFFF);

      cn_idx = ntr_const_tbl(Integer_8,
                             FALSE,
                             constant);
   }
   else 
# endif
   if (! just_init &&
       exp_desc->rank == 0 &&
       exp_desc->type != Character &&
       exp_desc->type != Structure &&
       storage_bit_size_tbl[TYP_LINEAR(CN_TYPE_IDX(cn_idx))] <
                     TARGET_BITS_PER_WORD) {

      /* must shift the constant so that it is left justified */
      /* word size integer (CG_INTEGER_DEFAULT_TYPE)          */

      constant[0] = CN_CONST(cn_idx) << (TARGET_BITS_PER_WORD -
                       storage_bit_size_tbl[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]);

      cn_idx = ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                             FALSE,
                            constant);
   }

   /*************\
   |* BASE ADDR *|
   \*************/

   if (just_init) {
      dope_vec->base_addr = 0;
   }
# if defined(_HOST_OS_UNICOS) && defined(_TARGET_OS_UNICOS)

    /* BRIANJ */

   else if (exp_desc->type == Character) {
      fcd_r = _cptofcd((char *)&CN_CONST(cn_idx),
                       CN_INT_TO_C(TYP_IDX(exp_desc->type_idx)));
      dope_vec->base_addr = *(int *)&fcd_r;
   }
   else if (exp_desc->type == Structure &&
            ATT_CHAR_SEQ(TYP_IDX(exp_desc->type_idx))) {
      fcd_r = _cptofcd((char *)&CN_CONST(cn_idx),
     (CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(exp_desc->type_idx)))) >> 3);
      dope_vec->base_addr = *(int *)&fcd_r;
   }
# endif
   else {
      dope_vec->base_addr = (long)&CN_CONST(cn_idx);
   }

   /*************\
   |* EL_LEN    *|
   \*************/

   find_opnd_line_and_column(r_opnd, &line, &column);

   if (exp_desc->type == Structure) {

      cn_idx = ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
      if (compare_cn_and_value(cn_idx,
                               MAX_DV_EL_LEN,
                               Ge_Opr)) {
         PRINTMSG(line, 1174, Error, column, CN_INT_TO_C(cn_idx),MAX_DV_EL_LEN);
         dope_vec->el_len = MAX_DV_EL_LEN;
      }
      else { /* BRIANJ */
         dope_vec->el_len = CN_INT_TO_C(cn_idx);
      }
   }
   else if (exp_desc->type == Character) {

      if (exp_desc->char_len.fld == CN_Tbl_Idx) {

         if (char_len_in_bytes) {

            if (compare_cn_and_value(exp_desc->char_len.idx,
                                     MAX_DV_EL_LEN,
                                     Ge_Opr)) {
               PRINTMSG(line, 1174, Error, column,
                        CN_INT_TO_C(exp_desc->char_len.idx),
                        MAX_DV_EL_LEN);
               dope_vec->el_len = MAX_DV_EL_LEN;
            }
            else {
               dope_vec->el_len = CN_INT_TO_C(exp_desc->char_len.idx);
            }
         }
         else {

            if (compare_cn_and_value(exp_desc->char_len.idx,
                                     MAX_DV_EL_LEN/8,
                                     Ge_Opr)) {
               PRINTMSG(line, 1174, Error, column,
                        CN_INT_TO_C(exp_desc->char_len.idx),
                        MAX_DV_EL_LEN/8);
               dope_vec->el_len = MAX_DV_EL_LEN;
            }
            else {
               dope_vec->el_len = CN_INT_TO_C(exp_desc->char_len.idx)*8;
            }
         }
      }
      else {
         PRINTMSG(line, 969, Internal, column);
      }
   }
   else {
      dope_vec->el_len = storage_bit_size_tbl[exp_desc->linear_type];
   }

   /*************\
   |* ASSOC     *|
   \*************/

   if (just_init) {
      dope_vec->assoc = 0;
   }
   else {
      dope_vec->assoc = 1;
   }

   /*************\
   |* PTR_ALLOC *|
   \*************/

   dope_vec->ptr_alloc = 0;

   /*************\
   |* P_OR_A    *|
   \*************/

   dope_vec->p_or_a = 1;		/* pointer */

   /*************\
   |* A_CONTIG  *|
   \*************/

   dope_vec->a_contig = 0;

   /*************\
   |* UNUSED 1  *|
   \*************/

   dope_vec->unused_1 = 0;

# if defined(_TARGET64) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   /*************\
   |* UNUSED 2  *|
   \*************/

   dope_vec->unused_2 = 0;
# endif


   /*************\
   |* N_DIM     *|
   \*************/

   dope_vec->num_dims = exp_desc->rank;

# if defined(_TARGET64) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifndef _TYPE_CODE_64_BIT
   /*************\
   |* UNUSED 3  *|
   \*************/

   dope_vec->unused_3 = 0;
# endif
# endif

   /*************\
   |* TYPE_CODE *|
   \*************/

   make_io_type_code(type_idx, constant);
# ifdef _TYPE_CODE_64_BIT
   dope_vec->type_code = *(f90_type_t *)constant;
# else
   dope_vec->type_code = *constant;
# endif

   /*************\
   |* ORIG_BASE *|
   \*************/

   dope_vec->orig_base = 0;

   /*************\
   |* ORIG_SIZE *|
   \*************/

#ifdef KEY /* Bug 6845 */
   /* If this dope vector could have allocatable components, we need to do
    * something here */
#endif /* KEY Bug 6845 */

   dope_vec->orig_size = 0;

   for (i = 0; i < exp_desc->rank; i++) {

      /*************\
      |* DIM i LB  *|
      \*************/

      if (just_init) {
         dope_vec->dim[i].low_bound = 0;
      }
      else {
         /* set to one */
         dope_vec->dim[i].low_bound = 1;
      }


      /*************\
      |* DIM i EX  *|
      \*************/

      if (just_init) {
         dope_vec->dim[i].extent = 0;
      }
      else if (compare_cn_and_value(BD_XT_IDX(bd_idx, i+1), 0, Lt_Opr)) {
         dope_vec->dim[i].extent = 0;
      }
      else { /* BRIANJ */
         dope_vec->dim[i].extent = CN_INT_TO_C(BD_XT_IDX(bd_idx, i+1));
      }

      /*************\
      |* DIM i SM  *|
      \*************/

      if (just_init) {
         dope_vec->dim[i].stride_mult = 0;
      }
      else { /* BRIANJ */
         dope_vec->dim[i].stride_mult = CN_INT_TO_C(BD_SM_IDX(bd_idx, i+1));
      }
   }

EXIT:

   TRACE (Func_Exit, "gen_internal_dope_vector", NULL);

   return(ok);

}  /* gen_internal_dope_vector */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Transform a reference of character sequence derived type to a         *|
|*      substring reference of the first component.                           *|
|*									      *|
|* Input parameters:							      *|
|*	top_opnd - address of top of tree.                                    *|
|*      type_idx - idx to type table.                                         *|
|*									      *|
|* Output parameters:							      *|
|*	top_opnd - address of top of new tree.                                *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void	transform_char_sequence_ref(opnd_type		*top_opnd,
				    int			 type_idx)

{
   int			col;
   int			ir_idx;
   size_offset_type	length;
   int			line;
   int			list_idx;
   size_offset_type	num_chars;
   opnd_type   		opnd;


   TRACE (Func_Entry, "transform_char_sequence_ref", NULL);

   switch (OPND_FLD((*top_opnd))) {
      case AT_Tbl_Idx :

         if (TYP_TYPE(ATD_TYPE_IDX(OPND_IDX((*top_opnd)))) == Structure &&
             ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(OPND_IDX((*top_opnd)))))) {

            goto REFERENCE;
         }
         else {
            goto EXIT;
         }
     
      case IR_Tbl_Idx :

# ifdef _DEBUG
         if (IR_TYPE_IDX(OPND_IDX((*top_opnd))) == NULL_IDX) {
            print_ir(OPND_IDX((*top_opnd)));
            find_opnd_line_and_column(top_opnd, &line, &col);
            PRINTMSG(line, 993, Internal, col);
         }
# endif

         if ((IR_OPR(OPND_IDX((*top_opnd))) == Struct_Opr ||
              IR_OPR(OPND_IDX((*top_opnd))) == Dv_Deref_Opr ||
              IR_OPR(OPND_IDX((*top_opnd))) == Subscript_Opr ||
              IR_OPR(OPND_IDX((*top_opnd))) == Whole_Subscript_Opr ||
              IR_OPR(OPND_IDX((*top_opnd))) == Section_Subscript_Opr) &&
             TYP_TYPE(IR_TYPE_IDX(OPND_IDX((*top_opnd)))) == Structure &&
             ATT_CHAR_SEQ(TYP_IDX(IR_TYPE_IDX(OPND_IDX((*top_opnd)))))) {

            goto REFERENCE;
         }
         else if (TYP_TYPE(IR_TYPE_IDX(OPND_IDX((*top_opnd)))) != Structure ||
                  ! ATT_CHAR_SEQ(TYP_IDX(IR_TYPE_IDX(OPND_IDX((*top_opnd)))))) {

            COPY_OPND(opnd, IR_OPND_L(OPND_IDX((*top_opnd))));
            transform_char_sequence_ref(&opnd, type_idx);
            COPY_OPND(IR_OPND_L(OPND_IDX((*top_opnd))), opnd);

            COPY_OPND(opnd, IR_OPND_R(OPND_IDX((*top_opnd))));
            transform_char_sequence_ref(&opnd, type_idx);
            COPY_OPND(IR_OPND_R(OPND_IDX((*top_opnd))), opnd);

            goto EXIT;
         }
         else {
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX((*top_opnd))));
            transform_char_sequence_ref(&opnd, type_idx);
            COPY_OPND(IR_OPND_L(OPND_IDX((*top_opnd))), opnd);

            COPY_OPND(opnd, IR_OPND_R(OPND_IDX((*top_opnd))));
            transform_char_sequence_ref(&opnd, type_idx);
            COPY_OPND(IR_OPND_R(OPND_IDX((*top_opnd))), opnd);

            find_opnd_line_and_column(top_opnd, &line, &col);

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

            TYP_TYPE(TYP_WORK_IDX)       = Character;
            TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
            TYP_DESC(TYP_WORK_IDX)       = Default_Typed;
            TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)        = CN_Tbl_Idx;

            num_chars.idx	= CN_INTEGER_CHAR_BIT_IDX;
            num_chars.fld	= CN_Tbl_Idx;

            length.fld		= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
            length.idx		= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

            size_offset_binary_calc(&length, &num_chars, Div_Opr, &num_chars);

            if (num_chars.fld == NO_Tbl_Idx) {
               TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
               TYP_IDX(TYP_WORK_IDX)	= ntr_const_tbl(num_chars.type_idx, 
                                                        FALSE,
                                                        num_chars.constant);
            }
            else {
               TYP_FLD(TYP_WORK_IDX)	= num_chars.fld;
               TYP_IDX(TYP_WORK_IDX)	= num_chars.idx;
            }

            IR_TYPE_IDX(OPND_IDX((*top_opnd))) = ntr_type_tbl();
            goto EXIT;
         }

         /* break;  - Both sides of the IF end with GOTOs */

      case IL_Tbl_Idx :
         list_idx = OPND_IDX((*top_opnd));

         while (list_idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            transform_char_sequence_ref(&opnd, type_idx);
            COPY_OPND(IL_OPND(list_idx), opnd);
        
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         goto EXIT;

      case CN_Tbl_Idx :
      case SH_Tbl_Idx :
      case NO_Tbl_Idx   :
         goto EXIT;
   }

REFERENCE:

   find_opnd_line_and_column(top_opnd, &line, &col);

   num_chars.idx	= CN_INTEGER_CHAR_BIT_IDX;
   num_chars.fld	= CN_Tbl_Idx;
   length.fld		= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
   length.idx		= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

   size_offset_binary_calc(&length, &num_chars, Div_Opr, &num_chars);


   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Substring_Opr;
   IR_TYPE_IDX(ir_idx) = CHARACTER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   COPY_OPND(IR_OPND_L(ir_idx), (*top_opnd));
   OPND_FLD((*top_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*top_opnd)) = ir_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = list_idx;
   IR_LIST_CNT_R(ir_idx) = 2;
   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (num_chars.fld == NO_Tbl_Idx) {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = ntr_const_tbl(num_chars.type_idx, 
                                       FALSE, 
                                       num_chars.constant);
   }
   else {
      IL_FLD(list_idx) = num_chars.fld;
      IL_IDX(list_idx) = num_chars.idx;
   }

   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   add_substring_length(ir_idx);

EXIT:

   TRACE (Func_Exit, "transform_char_sequence_ref", NULL);

   return;

}  /* "transform_char_sequence_ref" */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Because of the problems of deferred function expansion of variable    *|
|*      length character functions within concats, this routine creates a new *|
|*      length expression for the concat after the functions have been        *|
|*      processed. (Their length is a tmp at this point).                     *|
|*									      *|
|* Input parameters:							      *|
|*	concat_idx - IR_Tbl_Idx for concat.                                   *|
|*									      *|
|* Output parameters:							      *|
|*	len_opnd - the length expression tree.                                *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void get_concat_len(int		concat_idx,
                    opnd_type	*len_opnd)

{
   int			col;
   int			line;
   int			list_idx;
   opnd_type    	opnd;
   opnd_type    	opnd2;
   int			plus_idx;


   TRACE (Func_Entry, "get_concat_len", NULL);

   line = IR_LINE_NUM(concat_idx);
   col  = IR_COL_NUM(concat_idx);

   list_idx = IR_IDX_L(concat_idx);
   *len_opnd = null_opnd;

   while (list_idx) {

      COPY_OPND(opnd2, IL_OPND(list_idx));
      get_char_len(&opnd2, &opnd);

      if (OPND_FLD((*len_opnd)) == NO_Tbl_Idx) {
         COPY_OPND((*len_opnd), opnd);
      }
      else {
         NTR_IR_TBL(plus_idx);
         IR_OPR(plus_idx) = Plus_Opr;
         IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(plus_idx) = line;
         IR_COL_NUM(plus_idx) = col;

         COPY_OPND(IR_OPND_L(plus_idx), (*len_opnd));
         COPY_OPND(IR_OPND_R(plus_idx), opnd);
         OPND_FLD((*len_opnd)) = IR_Tbl_Idx;
         OPND_IDX((*len_opnd)) = plus_idx;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "get_concat_len", NULL);

   return;

}  /* get_concat_len */

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

void get_char_len(opnd_type	*ref_opnd,
		  opnd_type	*length_opnd)

{
   int		cn_idx;
   int		ir_idx;
   int		line;
   int		col;
   opnd_type	opnd;

   TRACE (Func_Entry, "get_char_len", NULL);

   find_opnd_line_and_column(ref_opnd,
                             &line,
                             &col);

   switch(OPND_FLD((*ref_opnd))) {
      case IR_Tbl_Idx :
         ir_idx = OPND_IDX((*ref_opnd));

         if (IR_OPR(ir_idx) == Substring_Opr ||
             IR_OPR(ir_idx) == Whole_Substring_Opr) {

            COPY_OPND((*length_opnd), IL_OPND(IL_NEXT_LIST_IDX(
                        IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)))));
         }
         else if (IR_OPR(ir_idx) == Stmt_Expansion_Opr ||
                  IR_OPR(ir_idx) == Paren_Opr) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            get_char_len(&opnd, length_opnd);
         }
         else if (IR_TYPE_IDX(ir_idx) != NULL_IDX &&
                  TYP_TYPE(IR_TYPE_IDX(ir_idx)) == Character) {

            OPND_FLD((*length_opnd)) = TYP_FLD(IR_TYPE_IDX(ir_idx));
            OPND_IDX((*length_opnd)) = TYP_IDX(IR_TYPE_IDX(ir_idx));
            OPND_LINE_NUM((*length_opnd)) = line;
            OPND_COL_NUM((*length_opnd))  = col;

            if (OPND_FLD((*length_opnd)) == AT_Tbl_Idx) {
               ADD_TMP_TO_SHARED_LIST(OPND_IDX((*length_opnd)));
            }
         }
         else {
            PRINTMSG(line, 626, Internal, col,
                     "type idx", "get_char_len");
         }
         break;

      case CN_Tbl_Idx :

         cn_idx = OPND_IDX((*ref_opnd));
# ifdef _DEBUG
         if (TYP_TYPE(CN_TYPE_IDX(cn_idx)) != Character) {
            PRINTMSG(line, 626, Internal, col,
                     "CHARACTER type constant"
                     "get_concat_len");
         }
# endif

         OPND_FLD((*length_opnd)) = TYP_FLD(CN_TYPE_IDX(cn_idx));
         OPND_IDX((*length_opnd)) = TYP_IDX(CN_TYPE_IDX(cn_idx));
         OPND_LINE_NUM((*length_opnd)) = line;
         OPND_COL_NUM((*length_opnd))  = col;
         break;

      default :
         PRINTMSG(line, 626, Internal, col,
                  "IR_Tbl_Idx or CN_Tbl_Idx",
                  "get_char_len");
         break;
   }


   TRACE (Func_Exit, "get_char_len", NULL);

   return;

}  /* get_char_len */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Gen the dv_whole_def_opr for variable size function processing.       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attr idx of tmp_dope_vector                                           *|
|*                                                                            *|
\******************************************************************************/

int gen_sf_dv_whole_def(opnd_type         *r_opnd,
                        int		   type_idx,
			int		   bd_idx)

{
   int          	asg_idx;
   opnd_type    	base_opnd;
   int          	col;
   long_type    	constant;
   int          	dope_idx	= NULL_IDX;
   int          	dv_attr_idx;
   int          	i;
   int          	ir_idx;
   int          	line;
   int          	list_idx;
   int          	loc_idx;
   int          	mult_idx;
   size_offset_type	num_chars;
   opnd_type    	opnd;
   long	    		rank;
   int          	rank_idx	= NULL_IDX;
   size_offset_type	result;


   TRACE (Func_Entry, "gen_sf_dv_whole_def", NULL);

   find_opnd_line_and_column(r_opnd, &line, &col);

   dv_attr_idx = gen_compiler_tmp(line, col, Priv, TRUE);

   ATD_TYPE_IDX(dv_attr_idx) = type_idx;
   ATD_STOR_BLK_IDX(dv_attr_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
   AT_SEMANTICS_DONE(dv_attr_idx) = TRUE;

   /* Positions 1-7 are deferred shape entries in bd table. */
   ATD_ARRAY_IDX(dv_attr_idx) = BD_RANK(bd_idx);

   ATD_IM_A_DOPE(dv_attr_idx)    = TRUE;

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Dv_Def_Asg_Opr;
   IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Whole_Def_Opr;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   IR_FLD_L(asg_idx) = AT_Tbl_Idx;
   IR_IDX_L(asg_idx) = dv_attr_idx;
   IR_LINE_NUM_L(asg_idx) = line;
   IR_COL_NUM_L(asg_idx)  = col;

   IR_FLD_R(asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(asg_idx) = ir_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_idx;

   rank = (long) BD_RANK(bd_idx);

   IR_LIST_CNT_L(ir_idx) = 10 + (3 * rank);
   IR_DV_DIM(ir_idx) = rank;

   /*************\
   |* BASE ADDR *|
   \*************/

   if (OPND_FLD((*r_opnd)) == AT_Tbl_Idx &&
       AT_OBJ_CLASS(OPND_IDX((*r_opnd))) == Data_Obj &&
       ATD_CLASS(OPND_IDX((*r_opnd))) == Compiler_Tmp &&
       (TYP_TYPE(ATD_TYPE_IDX(OPND_IDX((*r_opnd)))) == CRI_Ptr ||
        TYP_TYPE(ATD_TYPE_IDX(OPND_IDX((*r_opnd)))) == CRI_Ch_Ptr ||
        ATD_IM_A_DOPE(OPND_IDX((*r_opnd))))) {

      if (ATD_IM_A_DOPE(OPND_IDX((*r_opnd)))) {

         NTR_IR_TBL(loc_idx);
         IR_OPR(loc_idx) = Dv_Access_Base_Addr;
         IR_TYPE_IDX(loc_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(loc_idx) = line;
         IR_COL_NUM(loc_idx) = col;
         COPY_OPND(IR_OPND_L(loc_idx), (*r_opnd));
         IL_FLD(list_idx) = IR_Tbl_Idx;
         IL_IDX(list_idx) = loc_idx;
      }
      else {
         COPY_OPND(IL_OPND(list_idx), (*r_opnd));
      }
   }
   else {
      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)  = Loc_Opr;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;

      if (TYP_TYPE(type_idx) == Character) {
         IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
      }
      else {
         IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      }

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = loc_idx;

      make_base_subtree(r_opnd, &base_opnd, &rank_idx, &dope_idx);
      COPY_OPND(IR_OPND_L(loc_idx), base_opnd);

# ifdef _TRANSFORM_CHAR_SEQUENCE
# ifdef _TARGET_OS_UNICOS
      if (TYP_TYPE(type_idx) == Structure &&
          ATT_CHAR_SEQ(TYP_IDX(type_idx))) {

         IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
         COPY_OPND(opnd, IR_OPND_L(loc_idx));
         transform_char_sequence_ref(&opnd, type_idx);
         COPY_OPND(IR_OPND_L(loc_idx), opnd);
      }
# endif
# endif
   }


   /*************\
   |* EL_LEN    *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (TYP_TYPE(type_idx) == Structure) {
      IL_FLD(list_idx)	= (fld_type) ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
      IL_IDX(list_idx)	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
      IL_LINE_NUM(list_idx)	= line;
      IL_COL_NUM(list_idx)	= col;
   }
   else if (TYP_TYPE(type_idx) == Character) {

      if (TYP_FLD(type_idx) == CN_Tbl_Idx) {

         if (char_len_in_bytes) {      /* Len is in bytes on solaris */

            IL_FLD(list_idx)	= CN_Tbl_Idx;
            IL_IDX(list_idx)	= TYP_IDX(type_idx);
         }
         else {
            result.idx		= CN_INTEGER_CHAR_BIT_IDX;
            result.fld		= CN_Tbl_Idx;

            num_chars.fld	= TYP_FLD(type_idx);
            num_chars.idx	= TYP_IDX(type_idx);

            size_offset_binary_calc(&num_chars, &result, Mult_Opr, &result);

            if (result.fld == NO_Tbl_Idx) {
               IL_FLD(list_idx)	= CN_Tbl_Idx;
               IL_IDX(list_idx)	= ntr_const_tbl(result.type_idx,
                                                FALSE,
                                                result.constant);
            }
            else {
               IL_FLD(list_idx)	= result.fld;
               IL_IDX(list_idx)	= result.idx;
            }
         }
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }
      else {
         if (char_len_in_bytes) {
            /* Len is in bytes on solaris */
            IL_FLD(list_idx)      = TYP_FLD(type_idx);
            IL_IDX(list_idx)      = TYP_IDX(type_idx);
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            if (IL_FLD(list_idx) == AT_Tbl_Idx) {
               ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
            }
         }
         else {

            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx) = Mult_Opr;
            IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx) = line;
            IR_COL_NUM(mult_idx)  = col;
            constant              = 8;
            IR_FLD_L(mult_idx)    = CN_Tbl_Idx;
            IR_IDX_L(mult_idx)    = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 8);
            IR_LINE_NUM_L(mult_idx) = line;
            IR_COL_NUM_L(mult_idx)  = col;
   
            IR_FLD_R(mult_idx)    = TYP_FLD(type_idx);
            IR_IDX_R(mult_idx)    = TYP_IDX(type_idx);
            IR_LINE_NUM_R(mult_idx) = line;
            IR_COL_NUM_R(mult_idx)  = col;

            if (IR_FLD_R(mult_idx) == AT_Tbl_Idx) {
               ADD_TMP_TO_SHARED_LIST(IR_IDX_R(mult_idx));
            }

            IL_FLD(list_idx)      = IR_Tbl_Idx;
            IL_IDX(list_idx)      = mult_idx;
         }
      }
   }
   else {
      constant = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, constant);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }

   /*************\
   |* ASSOC     *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* PTR_ALLOC *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* P_OR_A    *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* A_CONTIG  *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* N_DIM     *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, rank);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* TYPE_CODE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = create_dv_type_code(dv_attr_idx);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /*************\
   |* ORIG_BASE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /*************\
   |* ORIG_SIZE *|
   \*************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

#ifdef KEY /* Bug 6845 */
   /* If this dope vector could have allocatable components, the following
    * code needs to change */
   list_idx = do_alloc_cpnt(line, col, list_idx, 0);
#endif /* KEY Bug 6845 */

   for (i = 1; i <= rank; i++) {

      /*************\
      |* DIM i LB  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
      IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      if (IL_FLD(list_idx) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
      }

      /*************\
      |* DIM i EX  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = BD_XT_FLD(bd_idx, i);
      IL_IDX(list_idx) = BD_XT_IDX(bd_idx, i);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      if (IL_FLD(list_idx) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
      }

      /*************\
      |* DIM i SM  *|
      \*************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = BD_SM_FLD(bd_idx, i);
      IL_IDX(list_idx) = BD_SM_IDX(bd_idx, i);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      if (IL_FLD(list_idx) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
      }
   }

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "gen_sf_dv_whole_def", NULL);

   return(dv_attr_idx);

}  /* gen_sf_dv_whole_def */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine determines the correct character length of an expression *|
|*      for use by the dope vector gen routines. This is necessary, since we  *|
|*      don't create new type entries for each node of a concat and substring *|
|*      tree. Remember the rule, don't use type_idx for character length for  *|
|*      a general case character expression. It must be calculated.           *|
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

static void compute_char_element_len(opnd_type		*char_len,
				     opnd_type		*char_opnd,
				     opnd_type		*result_opnd)

{
   int			col;
   int			line;
   expr_arg_type	loc_exp_desc;
   int			mult_idx;
   cif_usage_code_type  save_xref_state;


   TRACE (Func_Entry, "compute_char_element_len", NULL);

   find_opnd_line_and_column(char_opnd, &line, &col);

   if (OPND_FLD((*char_opnd))         == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX((*char_opnd))) == Concat_Opr) {

      get_concat_len(OPND_IDX((*char_opnd)), result_opnd);
   }
   else {
      COPY_OPND((*result_opnd), (*char_len));
   }

   if (! char_len_in_bytes) {
      /* Len is in bytes for solaris */
      /* Len is in bits for everyone else */

      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx)  = col;
      IR_FLD_L(mult_idx)    = CN_Tbl_Idx;
      IR_IDX_L(mult_idx)    = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 8);
      IR_LINE_NUM_L(mult_idx) = line;
      IR_COL_NUM_L(mult_idx)  = col;
   
      COPY_OPND(IR_OPND_R(mult_idx), (*result_opnd));
   
      OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*result_opnd)) = mult_idx;
   }

   /* try to fold it down */
   loc_exp_desc.rank = 0;
   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   expr_semantics(result_opnd, &loc_exp_desc);
   xref_state      = save_xref_state;

   TRACE (Func_Exit, "compute_char_element_len", NULL);

   return;

}  /* compute_char_element_len */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine determines the correct character length of an expression *|
|*      for use by the dope vector gen routines. This is necessary, since we  *|
|*      don't create new type entries for each node of a concat and substring *|
|*      tree. Remember the rule, don't use type_idx for character length for  *|
|*      a general case character expression. It must be calculated.           *|
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

void get_shape_from_attr(expr_arg_type		*exp_desc,
		         int			 attr_idx,
		         int			 rank,
		         int			 line,
		         int			 column)

{
   int		i;
   int		ir_idx;


   TRACE (Func_Entry, "get_shape_from_attr", NULL);

   if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
      for (i = 0; i < rank; i++) {

         if (ATD_IM_A_DOPE(attr_idx)) {
            OPND_FLD(exp_desc->shape[i])	= IR_Tbl_Idx;
            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)			= Dv_Access_Extent;
            IR_TYPE_IDX(ir_idx)			= SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx)			= line;
            IR_COL_NUM(ir_idx)			= column;
            IR_DV_DIM(ir_idx)			= i + 1;
            IR_FLD_L(ir_idx)			= AT_Tbl_Idx;
            IR_IDX_L(ir_idx)			= attr_idx;
            IR_LINE_NUM_L(ir_idx)		= line;
            IR_COL_NUM_L(ir_idx)		= column;
            OPND_IDX(exp_desc->shape[i])	= ir_idx;

            SHAPE_FOLDABLE(exp_desc->shape[i])		= FALSE;
            SHAPE_WILL_FOLD_LATER(exp_desc->shape[i])	= FALSE;
         }
         else {
            OPND_FLD(exp_desc->shape[i]) = 
                        BD_XT_FLD(ATD_ARRAY_IDX(attr_idx), i+1);
            OPND_IDX(exp_desc->shape[i]) = 
                        BD_XT_IDX(ATD_ARRAY_IDX(attr_idx), i+1);
            OPND_LINE_NUM(exp_desc->shape[i])	= line;
            OPND_COL_NUM(exp_desc->shape[i])	= column;

            if (OPND_FLD(exp_desc->shape[i]) == AT_Tbl_Idx) {
               ADD_TMP_TO_SHARED_LIST(OPND_IDX(exp_desc->shape[i]));
            }

            if (OPND_FLD(exp_desc->shape[i]) == CN_Tbl_Idx) {
               SHAPE_FOLDABLE(exp_desc->shape[i])		= TRUE;
               SHAPE_WILL_FOLD_LATER(exp_desc->shape[i])	= TRUE;
            }
            else if (OPND_FLD(exp_desc->shape[i]) == AT_Tbl_Idx &&
                     AT_OBJ_CLASS(OPND_IDX(exp_desc->shape[i])) == Data_Obj &&
                     ATD_LCV_IS_CONST(OPND_IDX(exp_desc->shape[i]))) {
               SHAPE_FOLDABLE(exp_desc->shape[i])		= FALSE;
               SHAPE_WILL_FOLD_LATER(exp_desc->shape[i])	= TRUE;
            }
            else {
               SHAPE_FOLDABLE(exp_desc->shape[i])		= FALSE;
               SHAPE_WILL_FOLD_LATER(exp_desc->shape[i])	= FALSE;
            }
         }
      }
   }

   TRACE (Func_Exit, "get_shape_from_attr", NULL);

   return;

}  /* get_shape_from_attr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine will generate a Init_Opr statement for compiler temps    *|
|*      and insert the statement before the end statement of the scope. It is *|
|*      used at pdgcs conversion time whenever a compiler temp is             *|
|*      encountered that has its ATD_TMP_INIT_NOT_DONE flag set. This is to   *|
|*      ensure that only the data init's of compiler temps (for constructors  *|
|*      and some folded intrinsics) are only added if the temp is still being *|
|*      referenced at interface time. No one else will optimize these out and *|
|*      they can make the binarys quite large and slow down loading.          *|
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

void insert_init_stmt_for_tmp(int		tmp_idx)

{
   int		asg_idx;
   int		bd_idx;
   int		col;
   int		i;
   int		line;
   int		list_idx;
   int		save_curr_stmt_sh_idx;
   int		sub_idx;
   char		*endptr;
   int		tmpnum;


   TRACE (Func_Entry, "insert_init_stmt_for_tmp", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   curr_stmt_sh_idx = SCP_LAST_SH_IDX(curr_scp_idx);

   line = AT_DEF_LINE(tmp_idx);
   col  = AT_DEF_COLUMN(tmp_idx);
   bd_idx = ATD_ARRAY_IDX(tmp_idx);

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Init_Opr;
   IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;
   IR_LINE_NUM_L(asg_idx) = line;
   IR_COL_NUM_L(asg_idx)  = col;

   if (ATD_FLD(tmp_idx) == IR_Tbl_Idx &&
       bd_idx != NULL_IDX) {

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

      if (IL_FLD(list_idx) == AT_Tbl_Idx) {
         ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
      }

      for (i = 2; i <= BD_RANK(bd_idx); i++) {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
         IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         if (IL_FLD(list_idx) == AT_Tbl_Idx) {
            ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
         }
      }
   }
   else {
      IR_FLD_L(asg_idx)    = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)    = tmp_idx;
   }

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_IDX_R(asg_idx) = list_idx;
   IR_LIST_CNT_R(asg_idx) = 3;

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = (ATD_FLD(tmp_idx) == CN_Tbl_Idx ? ATD_TMP_IDX(tmp_idx) :
                                      IR_IDX_R(ATD_TMP_IDX(tmp_idx)));
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = (ATD_FLD(tmp_idx) == CN_Tbl_Idx ? CN_INTEGER_ONE_IDX :
                               IR_IDX_L(ATD_TMP_IDX(tmp_idx)));
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;

   if (ATD_FLD(tmp_idx) == CN_Tbl_Idx) {
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   }
   else {
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                  storage_bit_size_tbl[TYP_LINEAR(CN_TYPE_IDX(
                                             IR_IDX_R(ATD_TMP_IDX(tmp_idx))))]);
   }

   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   gen_sh(Before, Assignment_Stmt, line, col,
          FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   ATD_TMP_INIT_NOT_DONE(tmp_idx) = FALSE;

   /* Fix for bug 677, compiler temp names used for data initialization
    * need to to have their own namespace, since they can be exported
    * though module files.
    */
   if (strncmp(AT_OBJ_NAME_PTR(tmp_idx), compiler_tmp_prefix, COMPILER_TMP_PREFIX_LEN) == 0
       && strtod(AT_OBJ_NAME_PTR(tmp_idx) + COMPILER_TMP_PREFIX_LEN, &endptr)
       && *endptr == 0) {
      char buf[1000];
      int length;
      int np_idx;

      sprintf(buf, "%s%s", AT_OBJ_NAME_PTR(tmp_idx),
              SB_NAME_PTR(ATD_STOR_BLK_IDX(tmp_idx)));
      length = strlen(buf);
      NTR_NAME_POOL((long *)buf, length, np_idx);
      AT_NAME_LEN(tmp_idx) = length;
      AT_NAME_IDX(tmp_idx) = np_idx;
   }

   TRACE (Func_Exit, "insert_init_stmt_for_tmp", NULL);

   return;

}  /* insert_init_stmt_for_tmp */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	generate a static integer array of the specified size.                *|
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

int gen_static_integer_array_tmp(int	size,
				 int	line,
				 int	col)

{
   expr_arg_type	exp_desc;
   int			tmp_idx;
   int			type_idx;


   TRACE (Func_Entry, "gen_static_integer_array_tmp", NULL);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   type_idx = SA_INTEGER_DEFAULT_TYPE;
# else
   type_idx = CG_INTEGER_DEFAULT_TYPE;
# endif

   tmp_idx                   = gen_compiler_tmp(line,col, Shared, TRUE);
   ATD_TYPE_IDX(tmp_idx)     = type_idx;
   ATD_SAVED(tmp_idx)        = TRUE;
   ATD_DATA_INIT(tmp_idx)    = TRUE;
   ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
   AT_SEMANTICS_DONE(tmp_idx)= TRUE;

   exp_desc = init_exp_desc;
   exp_desc.type                = Integer;
   exp_desc.type_idx            = type_idx;
   exp_desc.linear_type         = TYP_LINEAR(type_idx);
   exp_desc.rank                = 1;
   exp_desc.shape[0].fld        = CN_Tbl_Idx;
   exp_desc.shape[0].idx	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, size);

   ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(&exp_desc,
                                                     line,
                                                     col);


   TRACE (Func_Exit, "gen_static_integer_array_tmp", NULL);

   return(tmp_idx);

}  /* gen_static_integer_array_tmp */

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

int cast_typeless_constant(int		cn_idx,
			   int		type_idx,
			   int		line,
			   int		col)

{
# if defined(_TARGET_OS_UNICOS)
   long_type	another_constant[MAX_WORDS_FOR_NUMERIC];
# endif

   char	       *char_ptr;
   long64	i;
   long64	k;
   int		l;
   int		new_const_idx;
   long64	new_word_size;
   long64	old_word_size;
   boolean	right_justified;
   long_type	the_constant[MAX_WORDS_FOR_NUMERIC];
   boolean	zero_pad;


   TRACE (Func_Entry, "cast_typeless_constant", NULL);

   if (TYP_TYPE(type_idx) == CRI_Ptr ||
       TYP_TYPE(type_idx) == CRI_Parcel_Ptr ||
       TYP_TYPE(type_idx) == CRI_Ch_Ptr) {
      type_idx = TYPELESS_DEFAULT_TYPE;
   }

   if (CN_HOLLERITH_TYPE(cn_idx) == H_Hollerith) {
      right_justified = FALSE;
      zero_pad = FALSE;
      old_word_size = TARGET_BITS_TO_WORDS(TYP_BIT_LEN(CN_TYPE_IDX(cn_idx)));
   }
   else if (CN_HOLLERITH_TYPE(cn_idx) == L_Hollerith) {
      right_justified = FALSE;
      zero_pad = TRUE;
      old_word_size = TARGET_BITS_TO_WORDS(TYP_BIT_LEN(CN_TYPE_IDX(cn_idx)));
   }
   else if (CN_HOLLERITH_TYPE(cn_idx) == R_Hollerith) {
      right_justified = TRUE;
      zero_pad = TRUE;
      old_word_size = TARGET_BITS_TO_WORDS(TYP_BIT_LEN(CN_TYPE_IDX(cn_idx)));
   }
   else if (TYP_TYPE(CN_TYPE_IDX(cn_idx)) == Character) {
      right_justified = FALSE;
      zero_pad = FALSE;
      old_word_size = TARGET_BYTES_TO_WORDS(
                         CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(cn_idx))));
   }
   else {
      /* non hollerith, non character, => typeless */
      right_justified = TRUE;
      zero_pad = TRUE;
      old_word_size = TARGET_BITS_TO_WORDS(TYP_BIT_LEN(CN_TYPE_IDX(cn_idx)));
   }


   if (TYP_TYPE(type_idx) == Typeless) {
      new_word_size = TARGET_BITS_TO_WORDS(TYP_BIT_LEN(type_idx));
   }
   else {
      new_word_size = TARGET_BITS_TO_WORDS(
                        storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
   }

   if (right_justified) {  /* BRIANJ */
      k = old_word_size - 1;
      for (i = new_word_size - 1; i >= 0; i--) {
         if (k < 0) {
            break;
         }
#if defined(TARG_X8664) || defined(TARG_MIPS)
         the_constant[new_word_size-1-i] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + k);
#else
         the_constant[i] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + k);
#endif
         k--;
      }

      while (i >= 0) {
         /* fill in pad */
         if (zero_pad) {
#if defined(TARG_X8664) || defined(TARG_MIPS)
// Bug 1819 (also 10769 for MIPS)
            the_constant[new_word_size-1-i] = 0;
#else
            the_constant[i] = 0;
#endif
         }
         else {
            char_ptr = (char *)&(the_constant[i]);
            for (l = 0; l < TARGET_CHARS_PER_WORD; l++) {
               char_ptr[l] = ' ';
            }
         }

         i--;
      }

      if (k >= 0) {
         /* issue truncation message */
         PRINTMSG(line, 1127, Caution, col);
      }
   }
   else {
      k = 0;
      for (i = 0; i < new_word_size; i++) {
         if (k >= old_word_size) {
            break;
         }
         the_constant[i] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + k);
         k++;
      }

      while (i < new_word_size) {
         /* fill in pad */
         if (zero_pad) {
            the_constant[i] = 0;
         }
         else {
            char_ptr = (char *)&(the_constant[i]);
            for (l = 0; l < TARGET_CHARS_PER_WORD; l++) {
               char_ptr[l] = ' ';
            }
         }

         i++;
      }

      if (k < old_word_size) {
         /* issue truncation message */
         PRINTMSG(line, 1127, Caution, col);
      }

# ifdef _TARGET_OS_MAX
      if (TYP_LINEAR(type_idx) == Integer_1 ||
          TYP_LINEAR(type_idx) == Integer_2 ||
          TYP_LINEAR(type_idx) == Integer_4 ||
          TYP_LINEAR(type_idx) == Real_4    ||
          TYP_LINEAR(type_idx) == Logical_1 ||
          TYP_LINEAR(type_idx) == Logical_2 ||
          TYP_LINEAR(type_idx) == Logical_4) {

         the_constant[0] = the_constant[0] >> 32;
      }
# elif defined(_INTEGER_1_AND_2) && !defined(_TARGET_LITTLE_ENDIAN)

      if (on_off_flags.integer_1_and_2 &&
          (TYP_LINEAR(type_idx) == Integer_1 ||
           TYP_LINEAR(type_idx) == Integer_2 ||
           TYP_LINEAR(type_idx) == Logical_1 ||
           TYP_LINEAR(type_idx) == Logical_2)) {

         the_constant[0] = the_constant[0] >> (TARGET_BITS_PER_WORD - 
                           storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      }
# endif
   }

# if defined(_INTEGER_1_AND_2)

   if (on_off_flags.integer_1_and_2) {

      if (TYP_LINEAR(type_idx) == Integer_1 || 
          TYP_LINEAR(type_idx) == Logical_1) {

         the_constant[0] = the_constant[0] & 0XFF;
      }
      else if (TYP_LINEAR(type_idx) == Integer_2 ||
               TYP_LINEAR(type_idx) == Logical_2) {

         the_constant[0] = the_constant[0] & 0XFFFF;
      }
   }
# endif

# ifdef _TARGET_OS_UNICOS

   /* to get proper sign extension on UNICOS pvp's for short ints, */
   /* convert the 64 bit typeless to short int.                    */

   if (TYP_LINEAR(type_idx) == Integer_1 ||
       TYP_LINEAR(type_idx) == Integer_2 ||
       TYP_LINEAR(type_idx) == Integer_4) {

      if (folder_driver( (char *) the_constant,
                         Integer_8,
                         NULL,
                         NULL_IDX,
                         another_constant,
                        &type_idx,
                         line,
                         col,
                         1,
                         Cvrt_Opr)) {

         for (i=0; i<MAX_WORDS_FOR_INTEGER; i++) {
            the_constant[i] = another_constant[i];
         }
      }
   }
# endif

   if (TYP_TYPE(type_idx) == Typeless &&
       CN_BOZ_CONSTANT(cn_idx)) {
      new_const_idx = ntr_boz_const_tbl(type_idx,
                                        the_constant);
   }
   else if (TYP_TYPE(type_idx) == Typeless &&
            CN_BOOLEAN_CONSTANT(cn_idx)) {
      new_const_idx = ntr_boolean_const_tbl(type_idx,
                                            the_constant);
   }
   else {

      if (TYP_TYPE(type_idx) == Real) {
         new_const_idx = ntr_unshared_const_tbl(type_idx,
                                                FALSE,
                                                the_constant);
      }
      else {
         new_const_idx = ntr_const_tbl(type_idx,
                                       FALSE,
                                       the_constant);
      }
   }

   TRACE (Func_Exit, "cast_typeless_constant", NULL);

   return(new_const_idx);

}  /* cast_typeless_constant */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	In cases where the default integer (logical) type has been changed by *|
|*      the command line, we must cast some arguments to library routines to  *|
|*      machine size integers. This occurs when default types are doubled on  *|
|*      solaris and when they are halved on mpp.                              *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - subtree to put cvrt_opr over.                                  *|
|*	exp_desc - expression descriptor for that opnd.                       *|
|*									      *|
|* Output parameters:							      *|
|*	opnd - holds the new tree.                                            *|
|*      exp_desc - some fields have been changed, like type.                  *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void	cast_to_cg_default(opnd_type		*opnd,
			   expr_arg_type	*exp_desc)

{
   int			col;
   int			cvrt_idx;
   boolean		do_cast = FALSE;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			line;
   int			type_idx;

   TRACE (Func_Entry, "cast_to_cg_default", NULL);

   if (exp_desc->type == Integer) {

      if (storage_bit_size_tbl[exp_desc->linear_type] != 
            storage_bit_size_tbl[TYP_LINEAR(CG_INTEGER_DEFAULT_TYPE)]) {

         do_cast = TRUE;
         type_idx = CG_INTEGER_DEFAULT_TYPE;
      }
   }
   else if (exp_desc->type == Logical) {

      if (storage_bit_size_tbl[exp_desc->linear_type] !=
            storage_bit_size_tbl[TYP_LINEAR(CG_LOGICAL_DEFAULT_TYPE)]) {

         do_cast = TRUE;
         type_idx = CG_LOGICAL_DEFAULT_TYPE;
      }
   }

   if (do_cast) {
      find_opnd_line_and_column(opnd, &line, &col);

      if (OPND_FLD((*opnd)) == CN_Tbl_Idx) {

         if (folder_driver((char *)&CN_CONST(OPND_IDX((*opnd))),
                           exp_desc->type_idx,
                           NULL,
                           NULL_IDX,
                           folded_const,
                          &type_idx,
                           line,
                           col,
                           1,
                           Cvrt_Opr)) {
            /* intentionally blank */
         }

         OPND_IDX((*opnd)) = ntr_const_tbl(type_idx,
                                           FALSE,
                                           folded_const);

      }
      else {
     
         NTR_IR_TBL(cvrt_idx);
         IR_OPR(cvrt_idx) = Cvrt_Opr;
         IR_TYPE_IDX(cvrt_idx) = type_idx;
         IR_LINE_NUM(cvrt_idx) = line;
         IR_COL_NUM(cvrt_idx)  = col;

         IR_RANK(cvrt_idx) = exp_desc->rank;

         COPY_OPND(IR_OPND_L(cvrt_idx), (*opnd));

         if (exp_desc->rank > 0) {
            IR_ARRAY_SYNTAX(cvrt_idx) = TRUE;
         }

         OPND_FLD((*opnd)) = IR_Tbl_Idx;
         OPND_IDX((*opnd)) = cvrt_idx;

         exp_desc->reference = FALSE;
         exp_desc->tmp_reference = FALSE;
      }

      exp_desc->type_idx    = type_idx;
      exp_desc->type        = TYP_TYPE(type_idx);
      exp_desc->linear_type = TYP_LINEAR(type_idx);
   }

   TRACE (Func_Exit, "cast_to_cg_default", NULL);

   return;

}  /* cast_to_cg_default */



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

void cast_opnd_to_type_idx(opnd_type	*opnd,
			   int		 type_idx)

{
   int			col;
   expr_arg_type	exp_desc;
   int			line;

   TRACE (Func_Entry, "cast_opnd_to_type_idx", NULL);

   exp_desc = init_exp_desc;

   if (OPND_FLD((*opnd)) == CN_Tbl_Idx) {
      exp_desc.type_idx = CN_TYPE_IDX(OPND_IDX((*opnd)));
   }
   else if (OPND_FLD((*opnd)) == AT_Tbl_Idx) {
      exp_desc.type_idx = ATD_TYPE_IDX(OPND_IDX((*opnd)));
   }
   else if (OPND_FLD((*opnd)) == IR_Tbl_Idx) {
      exp_desc.type_idx = IR_TYPE_IDX(OPND_IDX((*opnd)));
      exp_desc.rank = IR_RANK(OPND_IDX((*opnd)));
   }
   else {
# ifdef _DEBUG
      find_opnd_line_and_column(opnd, &line, &col);
      PRINTMSG(line, 626, Internal, col,
               "CN, AT, or IR_Tbl_Idx", "cast_opnd_to_type_idx");
# endif
   }

   exp_desc.linear_type = TYP_LINEAR(exp_desc.type_idx);
   exp_desc.type        = TYP_TYPE(exp_desc.type_idx);

   cast_to_type_idx(opnd, &exp_desc, type_idx);

   TRACE (Func_Exit, "cast_opnd_to_type_idx", NULL);

   return;

}  /* cast_opnd_to_type_idx */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Cast an arbitrary opnd to type described in typ_tbl[type_idx].        *|
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

void	cast_to_type_idx(opnd_type		*opnd,
			 expr_arg_type		*exp_desc,
			 int			 type_idx)

{
   char                 *char_ptr1;
   char                 *char_ptr2;
   int                  cn_idx;
   int                  col;
   int                  cvrt_idx;
   long_type            folded_const[MAX_WORDS_FOR_NUMERIC];
   long64               i;
   int                  line;

   TRACE (Func_Entry, "cast_to_type_idx", NULL);

   if ((TYP_TYPE(type_idx) != Character &&
        TYP_LINEAR(type_idx) != exp_desc->linear_type) ||
       (TYP_TYPE(type_idx) == Character &&
        TYP_FLD(type_idx) == CN_Tbl_Idx &&
        TYP_FLD(exp_desc->type_idx) == CN_Tbl_Idx &&
        fold_relationals(TYP_IDX(type_idx),
                         TYP_IDX(exp_desc->type_idx),
                         Ne_Opr))) {

      find_opnd_line_and_column(opnd, &line, &col);

      if (exp_desc->linear_type == Short_Typeless_Const) {
         OPND_IDX((*opnd)) = cast_typeless_constant(OPND_IDX((*opnd)),
                                                    type_idx,
                                                    line,
                                                    col);

      }
      else if (OPND_FLD((*opnd)) == CN_Tbl_Idx) {

         if (TYP_TYPE(type_idx) == Character) {
            cn_idx = ntr_const_tbl(type_idx, TRUE, NULL);
            char_ptr1 = (char *)&CN_CONST(OPND_IDX((*opnd)));
            char_ptr2 = (char *)&CN_CONST(cn_idx);

            for (i = 0;
                 i < CN_INT_TO_C(TYP_IDX(exp_desc->type_idx)) &&
                    i < CN_INT_TO_C(TYP_IDX(type_idx));
                 i++) {
               char_ptr2[i] = char_ptr1[i];
            }

            for (; i < CN_INT_TO_C(TYP_IDX(type_idx)); i++) {
               char_ptr2[i] = ' ';
            }

            while ((i % TARGET_CHARS_PER_WORD) != 0) {
               char_ptr2[i] = ' ';
               i++;
            }

            OPND_IDX((*opnd)) = cn_idx;
            
            if (compare_cn_and_value(TYP_IDX(type_idx),
                                     MAX_CHARS_IN_TYPELESS,
                                     Le_Opr)) {
               exp_desc->linear_type = Short_Char_Const;
            }
            else {
               /* assume one byte character for now */
               exp_desc->linear_type = Character_1;
            }
         }
         else {
            if (folder_driver((char *)&CN_CONST(OPND_IDX((*opnd))),
                              exp_desc->type_idx,
                              NULL,
                              NULL_IDX,
                              folded_const,
                             &type_idx,
                              line,
                              col,
                              1,
                              Cvrt_Opr)) {
               /* intentionally blank */
            }

            OPND_IDX((*opnd)) = ntr_const_tbl(type_idx,
                                              FALSE,
                                              folded_const);
         }
      }
# if _DEBUG
      else if (TYP_TYPE(type_idx) == Character) {
         PRINTMSG(line, 626, Internal, col,
                  "non character operand",
                  "cast_to_type_idx");
      }
# endif
      else {

         NTR_IR_TBL(cvrt_idx);
         IR_OPR(cvrt_idx) = Cvrt_Opr;
         IR_RANK(cvrt_idx) = exp_desc->rank;

         IR_TYPE_IDX(cvrt_idx) = type_idx;
         IR_LINE_NUM(cvrt_idx) = line;
         IR_COL_NUM(cvrt_idx)  = col;

         COPY_OPND(IR_OPND_L(cvrt_idx), (*opnd));

         if (exp_desc->rank > 0) {
            IR_ARRAY_SYNTAX(cvrt_idx) = TRUE;
         }

         OPND_FLD((*opnd)) = IR_Tbl_Idx;
         OPND_IDX((*opnd)) = cvrt_idx;

         exp_desc->reference = FALSE;
         exp_desc->tmp_reference = FALSE;
      }

      exp_desc->type_idx    = type_idx;
      exp_desc->type        = TYP_TYPE(type_idx);
      exp_desc->linear_type = TYP_LINEAR(type_idx);

      if (exp_desc->type == Character) {
         OPND_FLD(exp_desc->char_len) = TYP_FLD(exp_desc->type_idx);
         OPND_IDX(exp_desc->char_len) = TYP_IDX(exp_desc->type_idx);
      }
   }

   TRACE (Func_Exit, "cast_to_type_idx", NULL);

   return;

}  /* cast_to_type_idx */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	set up a logical constant value in an integer array depending on      *|
|*      kind type and platform.                                               *|
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

int	set_up_logical_constant(long_type	*the_constant, 
			        int		type_idx,
			        int		value,
			        boolean		enter_con)

{
   int	cn_idx;


   TRACE (Func_Entry, "set_up_logical_constant", NULL);

/* BRIANJ KAYKAY - Should this use arith? */

# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (TYP_LINEAR(type_idx) == Logical_8) {
# if defined(_HOST_LITTLE_ENDIAN) && defined(_TARGET_LITTLE_ENDIAN)
      *(long long *)the_constant = value;
# else
      the_constant[0] = 0;
      the_constant[1] = value;
# endif
   }
   else {
      the_constant[0] = value;
   }
# else
   the_constant[0] = value;
# endif

   if (enter_con) {
      cn_idx = ntr_const_tbl(type_idx,
                             FALSE,
                             the_constant);
   }
   else {
      cn_idx = NULL_IDX;
   }

   TRACE (Func_Exit, "set_up_logical_constant", NULL);

   return(cn_idx);

}  /* set_up_logical_constant */

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

boolean validate_char_len(opnd_type     *result_opnd,
                          expr_arg_type *exp_desc)

{
   int                  ch_asg_idx;
   int                  col;
   opnd_type            length_opnd;
   int                  line;
   expr_arg_type        loc_exp_desc;
   boolean              ok = TRUE;
   cif_usage_code_type  save_xref_state;
   int                  tmp_idx;

   TRACE (Func_Entry, "validate_char_len", NULL);

   if (exp_desc->type == Character          &&
       (exp_desc->char_len.fld != TYP_FLD(exp_desc->type_idx) ||
        exp_desc->char_len.idx != TYP_IDX(exp_desc->type_idx) ||
        (OPND_FLD((*result_opnd))         == IR_Tbl_Idx &&
         IR_OPR(OPND_IDX((*result_opnd))) == Concat_Opr))) {

      find_opnd_line_and_column(result_opnd, &line, &col);

# ifdef _DEBUG
      if (exp_desc->char_len.fld == NO_Tbl_Idx) {
         PRINTMSG(line, 1018, Internal, col);
      }
# endif

      loc_exp_desc.rank = 0;

      if (OPND_FLD((*result_opnd))         == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX((*result_opnd))) == Concat_Opr) {

         get_concat_len(OPND_IDX((*result_opnd)), &length_opnd);
      }
      else {
         COPY_OPND(length_opnd, (exp_desc->char_len));
      }

      save_xref_state = xref_state;
      xref_state      = CIF_No_Usage_Rec;
      ok = expr_semantics(&length_opnd, &loc_exp_desc);
      xref_state      = save_xref_state;

      COPY_OPND((exp_desc->char_len), length_opnd);

      if (loc_exp_desc.constant) {
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

         TYP_TYPE(TYP_WORK_IDX)         = Character;
         TYP_LINEAR(TYP_WORK_IDX)       = CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX)   = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)          = CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)          = OPND_IDX(length_opnd);
         exp_desc->type_idx             = ntr_type_tbl();
         exp_desc->type                 = Character;
         exp_desc->linear_type          = CHARACTER_DEFAULT_TYPE;
      }
      else { /* non constant character length means an alloc'd item */

         GEN_COMPILER_TMP_ASG(ch_asg_idx,
                              tmp_idx,
                              TRUE,     /* Semantics done */
                              line,
                              col,
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
         exp_desc->type_idx             = ntr_type_tbl();
         exp_desc->type                 = Character;
         exp_desc->linear_type          = CHARACTER_DEFAULT_TYPE;
      }
   }

   if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
       (IR_OPR(OPND_IDX((*result_opnd))) == Substring_Opr ||
        IR_OPR(OPND_IDX((*result_opnd))) == Whole_Substring_Opr)) {

      IR_TYPE_IDX(OPND_IDX((*result_opnd))) = exp_desc->type_idx;
   }


   TRACE (Func_Exit, "validate_char_len", NULL);

   return(ok);

}  /* validate_char_len */

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

void gen_runtime_checks(opnd_type	*top_opnd)

{
   int		ir_idx;
   int		list_idx;
   opnd_type	opnd;

   TRACE (Func_Entry, "gen_runtime_checks", NULL);

   switch (OPND_FLD((*top_opnd))) {
   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*top_opnd));

      if ((IR_OPR(ir_idx) == Subscript_Opr ||
           IR_OPR(ir_idx) == Section_Subscript_Opr) &&
          needs_bounds_check(ir_idx)) {

         gen_runtime_bounds(ir_idx);
      }
      else if (cmd_line_flags.runtime_substring &&
               IR_OPR(ir_idx) == Substring_Opr  &&
               ATD_CLASS(find_left_attr(&IR_OPND_L(ir_idx))) != Compiler_Tmp) {
         gen_runtime_substring(ir_idx);
      }

      COPY_OPND(opnd, IR_OPND_L(ir_idx));
      gen_runtime_checks(&opnd);

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      gen_runtime_checks(&opnd);
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*top_opnd));

      while (list_idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         gen_runtime_checks(&opnd);

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;
   }

   TRACE (Func_Exit, "gen_runtime_checks", NULL);

   return;

}  /* gen_runtime_checks */

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

void gen_runtime_conformance(opnd_type 		*l_opnd,
                             expr_arg_type	*l_exp_desc,
                             opnd_type		*r_opnd,   /* BRIANJ -not used*/
                             expr_arg_type	*r_exp_desc)

{
   int			col;
   int			i;
   expr_arg_type	left_exp_desc;
   int			line;
   expr_arg_type	right_exp_desc;

   TRACE (Func_Entry, "gen_runtime_conformance", NULL);

   left_exp_desc = *l_exp_desc;
   right_exp_desc = *r_exp_desc;

   find_opnd_line_and_column(l_opnd, &line, &col);

# ifdef _DEBUG
   if (defer_stmt_expansion) {
      PRINTMSG(line, 626, Internal, col,
               "defer_stmt_expansion to be FALSE", 
               "gen_runtime_conformance");
   }
# endif

   for (i = 0; i < left_exp_desc.rank; i++) {
       gen_conform_check_call(&(left_exp_desc.shape[i]),
                              &(right_exp_desc.shape[i]),
                              i + 1,
                              line,
                              col);
   }

   TRACE (Func_Exit, "gen_runtime_conformance", NULL);

   return;

}  /* gen_runtime_conformance */

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

void gen_runtime_substring(int		substring_idx)

{
   int		attr_idx;
   int		list_idx;
   int		line;
   int		col;
   opnd_type	size_opnd;
   opnd_type	start_opnd;
   opnd_type	subln_opnd;

   TRACE (Func_Entry, "gen_runtime_substring", NULL);

   attr_idx = find_base_attr(&IR_OPND_L(substring_idx), &line, &col);

# ifdef _DEBUG
   if (defer_stmt_expansion) {
      PRINTMSG(line, 626, Internal, col,
               "defer_stmt_expansion to be FALSE", 
               "gen_runtime_substring");
   }
# endif

   list_idx = IR_IDX_R(substring_idx);

   OPND_FLD(size_opnd) = TYP_FLD(ATD_TYPE_IDX(attr_idx));
   OPND_IDX(size_opnd) = TYP_IDX(ATD_TYPE_IDX(attr_idx));
   OPND_LINE_NUM(size_opnd) = line;
   OPND_COL_NUM(size_opnd)  = col;

   COPY_OPND(start_opnd, IL_OPND(list_idx));
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   list_idx = IL_NEXT_LIST_IDX(list_idx);

# ifdef _DEBUG
   if (list_idx == NULL_IDX) {
      PRINTMSG(line, 626, Internal, col,
                     "substring length",
                     "gen_runtime_substring");
   }
# endif

   COPY_OPND(subln_opnd, IL_OPND(list_idx));

   if (OPND_FLD(start_opnd) == CN_Tbl_Idx &&
       OPND_FLD(subln_opnd) == CN_Tbl_Idx &&
       OPND_FLD(size_opnd) == CN_Tbl_Idx) {

   }
   else {
      gen_sbounds_check_call(AT_OBJ_NAME_PTR(attr_idx), 
                             &size_opnd,
                             &start_opnd,
                             &subln_opnd,
                             line,
                             col);

      IR_BOUNDS_DONE(substring_idx) = TRUE;
   }
   

   TRACE (Func_Exit, "gen_runtime_substring", NULL);

   return;

}  /* gen_runtime_substring */

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

void gen_runtime_ptr_chk(opnd_type	*dv_opnd)

{
   int		attr_idx;
   int		bd_idx;
   int		col;
   int		left_attr;
   int		line;

   TRACE (Func_Entry, "gen_runtime_ptr_chk", NULL);

   attr_idx = find_base_attr(dv_opnd, &line, &col);
   left_attr = find_left_attr(dv_opnd);

   bd_idx = ATD_ARRAY_IDX(attr_idx);

   if (ATD_CLASS(left_attr) == Compiler_Tmp) {
      goto EXIT;
   }

   if (ATD_POINTER(attr_idx)) {
      gen_ptr_chk_call(AT_OBJ_NAME_PTR(attr_idx),
                       1,		/* means POINTER */
                       dv_opnd,
                       line,
                       col);
   }
   else if (ATD_ALLOCATABLE(attr_idx)) {
      gen_ptr_chk_call(AT_OBJ_NAME_PTR(attr_idx),
                       2,		/* means ALLOCATABLE ARRAY */
                       dv_opnd,
                       line,
                       col);
   }
   else if (bd_idx &&
            BD_ARRAY_CLASS(bd_idx) == Assumed_Shape) {
      gen_ptr_chk_call(AT_OBJ_NAME_PTR(attr_idx),
                       3,		/* means ASSUMED SHAPE ARRAY */
                       dv_opnd,
                       line,
                       col);
   }

EXIT:

   TRACE (Func_Exit, "gen_runtime_ptr_chk", NULL);

   return;

}  /* gen_runtime_ptr_chk */

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

void gen_runtime_bounds(int	sub_idx)

{
   int		attr_idx;
   int		bd_idx;
   int		col;
   int		dim;
   opnd_type	end_opnd;
   opnd_type	inc_opnd;
   int		ir_idx2;
   opnd_type	lb_opnd;
   int		line;
   int		list_idx;
   int		list_idx2;
   int		minus_idx;
   opnd_type	opnd;
   opnd_type	opnd2;
   int		plus_idx;
   opnd_type	start_opnd;
   opnd_type	ub_opnd;

   TRACE (Func_Entry, "gen_runtime_bounds", NULL);

   attr_idx = find_base_attr(&IR_OPND_L(sub_idx), &line, &col);


# ifdef _DEBUG
   if (defer_stmt_expansion) {
      PRINTMSG(line, 626, Internal, col,
               "defer_stmt_expansion to be FALSE", 
               "gen_runtime_bounds");
   }
# endif

   bd_idx = ATD_ARRAY_IDX(attr_idx);

   list_idx = IR_IDX_R(sub_idx);
   dim = 1;

   while (list_idx != NULL_IDX) {
      if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size &&
          dim == BD_RANK(bd_idx)) {
         break;
      }

      if (IL_VECTOR_SUBSCRIPT(list_idx)) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         dim++;
         continue;
      }

      if (ATD_IM_A_DOPE(attr_idx)) {
         COPY_OPND(opnd, IR_OPND_L(sub_idx));

         if (OPND_FLD(opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {

            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         gen_dv_access_low_bound(&lb_opnd, &opnd, dim);

         copy_subtree(&lb_opnd, &opnd2);

         ir_idx2 = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                     Dv_Access_Extent, SA_INTEGER_DEFAULT_TYPE,
                                 line, col,
                         NO_Tbl_Idx, NULL_IDX);
         IR_DV_DIM(ir_idx2) = dim;

         plus_idx = gen_ir(OPND_FLD(opnd2), OPND_IDX(opnd2),
                       Plus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                           IR_Tbl_Idx, ir_idx2);

         minus_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                        Minus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                            CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

         gen_opnd(&ub_opnd, minus_idx, IR_Tbl_Idx, line, col);
      }
      else {
         gen_opnd(&lb_opnd, BD_LB_IDX(bd_idx,dim), 
                  BD_LB_FLD(bd_idx, dim), line, col);
         gen_opnd(&ub_opnd, BD_UB_IDX(bd_idx,dim), 
                  BD_UB_FLD(bd_idx, dim), line, col);
      }

      if (IL_FLD(list_idx) == IR_Tbl_Idx &&
          IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

         list_idx2 = IR_IDX_L(IL_IDX(list_idx));
         COPY_OPND(start_opnd, IL_OPND(list_idx2));
         copy_subtree(&start_opnd, &start_opnd);

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
         COPY_OPND(end_opnd, IL_OPND(list_idx2));
         copy_subtree(&end_opnd, &end_opnd);

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
         COPY_OPND(inc_opnd, IL_OPND(list_idx2));
         copy_subtree(&inc_opnd, &inc_opnd);

         if (OPND_FLD(lb_opnd) != CN_Tbl_Idx ||
             OPND_FLD(ub_opnd) != CN_Tbl_Idx ||
             OPND_FLD(start_opnd) != CN_Tbl_Idx ||
             OPND_FLD(end_opnd) != CN_Tbl_Idx ||
             OPND_FLD(inc_opnd) != CN_Tbl_Idx) {

            gen_rbounds_check_call(AT_OBJ_NAME_PTR(attr_idx),
                                   &lb_opnd,
                                   &ub_opnd,
                                   &start_opnd,
                                   &end_opnd,
                                   &inc_opnd,
                                   dim,
                                   line,
                                   col);
            IR_BOUNDS_DONE(sub_idx) = TRUE;
         }
      }
      else if (IL_FLD(list_idx) != CN_Tbl_Idx ||
               OPND_FLD(lb_opnd) != CN_Tbl_Idx ||
               OPND_FLD(ub_opnd) != CN_Tbl_Idx) {

         COPY_OPND(start_opnd, IL_OPND(list_idx));
         copy_subtree(&start_opnd, &start_opnd);

         gen_bounds_check_call(AT_OBJ_NAME_PTR(attr_idx),
                               &lb_opnd,
                               &ub_opnd,
                               &start_opnd,
                               dim,
                               line, 
                               col);

         IR_BOUNDS_DONE(sub_idx) = TRUE;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      dim++;
   }

   TRACE (Func_Exit, "gen_runtime_bounds", NULL);

   return;

}  /* gen_runtime_bounds */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	generate the call to the conformance check lib routine (which only    *|
|*      issues the message). When support exists for a conform_opr, this      *|
|*      routine will generate that.                                           *|
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

static void gen_conform_check_call(opnd_type *l_shape, opnd_type *r_shape,
                                   int dim, int line, int col)

{
   int			call_idx;
   opnd_type		cond_opnd;
   int			dim_idx;
   int			end_sh_idx;
   expr_arg_type	exp_desc;
   int			ir_idx;
   int			line_idx;
   int			list_idx;
   int			max_idx;
   int			max_idx2;
   opnd_type		opnd;
   int			save_curr_stmt_sh_idx;
   expr_mode_type	save_expr_mode;
   cif_usage_code_type	save_xref_state;
   int			start_sh_idx;
   int			tmp_idx;


   TRACE (Func_Entry, "gen_conform_check_call", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   /* save the bounding stmts for the gen_if_stmt call */

   start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   end_sh_idx = curr_stmt_sh_idx;

   /* generate the if condition */

   GEN_MAX_ZERO_IR(max_idx, (*l_shape), line, col);

   GEN_MAX_ZERO_IR(max_idx2, (*r_shape), line, col);

   ir_idx = gen_ir(IR_Tbl_Idx, max_idx,
               Ne_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   IR_Tbl_Idx, max_idx2);

   gen_opnd(&cond_opnd, ir_idx, IR_Tbl_Idx, line, col);

   if (glb_tbl_idx[Conform_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Conform_Attr_Idx] = create_lib_entry_attr(
                                                   CONFORM_LIB_ENTRY,
                                                   CONFORM_NAME_LEN,
                                                   line,
                                                   col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Conform_Attr_Idx]);

   /* count (= 0) must be static temp */

   tmp_idx = gen_initialized_tmp(CN_INTEGER_ZERO_IDX, line, col);

   line_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, line);
   dim_idx  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, dim);

   list_idx = gen_il(4, TRUE, line, col,
                     CN_Tbl_Idx, put_file_name_in_cn(line),	/* file name */
                     CN_Tbl_Idx, line_idx,                      /* line */
                     CN_Tbl_Idx, dim_idx,                       /* dim */
                     AT_Tbl_Idx, tmp_idx);			/* count */

   call_idx = gen_ir(AT_Tbl_Idx, glb_tbl_idx[Conform_Attr_Idx],
                 Call_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                     IL_Tbl_Idx, list_idx);

   gen_sh(Before, Call_Stmt, line, col,
          FALSE, FALSE, TRUE);
   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   gen_opnd(&opnd, call_idx, IR_Tbl_Idx, line, col);

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;

   exp_desc        = init_exp_desc;
   call_list_semantics(&opnd, &exp_desc, FALSE);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;

   gen_if_stmt(&cond_opnd, 
               SH_NEXT_IDX(start_sh_idx),
               SH_PREV_IDX(end_sh_idx),
               NULL_IDX,
               NULL_IDX,
               line,
               col);

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_conform_check_call", NULL);

   return;

}  /* gen_conform_check_call */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      generate the call to the bounds check lib routine (which only         *|
|*      issues the message). When support exists for a bounds_opr, this       *|
|*      routine will generate that.                                           *|
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

static void gen_bounds_check_call(char *var,
				  opnd_type *lb_opnd,
                                  opnd_type *ub_opnd,
				  opnd_type *subscript,
                                  int dim,
				  int line,
				  int col)

{
   int                  call_idx;
   opnd_type		cond_opnd;
   int			dim_idx;
   int			end_sh_idx;
   expr_arg_type        exp_desc;
   int			gt_idx;
   int			line_idx;
   int                  list_idx;
   int			lt_idx;
   int			or_idx;
   opnd_type            opnd;
   int                  save_curr_stmt_sh_idx;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   int			start_sh_idx;
   int                  tmp_idx;


   TRACE (Func_Entry, "gen_bounds_check_call", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   /* save the bounding stmts for the gen_if_stmt call */

   start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   end_sh_idx = curr_stmt_sh_idx;

   /* cond_opnd = (subscript < lb) .or. (subscript > ub) */

   /* subscript < lb */

   lt_idx = gen_ir(OPND_FLD((*subscript)), OPND_IDX((*subscript)),
               Lt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   OPND_FLD((*lb_opnd)), OPND_IDX((*lb_opnd)));

   /* subscript > ub */
   gt_idx = gen_ir(OPND_FLD((*subscript)), OPND_IDX((*subscript)),
               Gt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   OPND_FLD((*ub_opnd)), OPND_IDX((*ub_opnd)));

   or_idx = gen_ir(IR_Tbl_Idx, lt_idx,
               Or_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   IR_Tbl_Idx, gt_idx);


   gen_opnd(&cond_opnd, or_idx, IR_Tbl_Idx, line, col);

   if (glb_tbl_idx[Bounds_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Bounds_Attr_Idx] = create_lib_entry_attr(
                                                   BOUNDS_LIB_ENTRY,
                                                   BOUNDS_NAME_LEN,
                                                   line,
                                                   col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Bounds_Attr_Idx]);

   /* count (= 0) must be static temp */

   tmp_idx  = gen_initialized_tmp(CN_INTEGER_ZERO_IDX, line, col);
   line_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, line);
   dim_idx  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, dim);

   list_idx = gen_il(8, TRUE, line, col,
                     CN_Tbl_Idx, put_file_name_in_cn(line),     /* file name */
                     CN_Tbl_Idx, line_idx,
                     CN_Tbl_Idx, put_c_str_in_cn(var),	    /* var name */
                     CN_Tbl_Idx, dim_idx,
                     OPND_FLD((*lb_opnd)), OPND_IDX((*lb_opnd)),/* lower bd */
                     OPND_FLD((*ub_opnd)), OPND_IDX((*ub_opnd)),/* upper bd */
                     OPND_FLD((*subscript)), OPND_IDX((*subscript)),
                     AT_Tbl_Idx, tmp_idx);                      /* count */

   call_idx = gen_ir(AT_Tbl_Idx, glb_tbl_idx[Bounds_Attr_Idx],
                 Call_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                     IL_Tbl_Idx, list_idx);

   gen_sh(Before, Call_Stmt, line, col,
          FALSE, FALSE, TRUE);
   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   gen_opnd(&opnd, call_idx, IR_Tbl_Idx, line, col);

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;

   exp_desc        = init_exp_desc;
   call_list_semantics(&opnd, &exp_desc, FALSE);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;

   gen_if_stmt(&cond_opnd,
               SH_NEXT_IDX(start_sh_idx),
               SH_PREV_IDX(end_sh_idx),
               NULL_IDX,
               NULL_IDX,
               line,
               col);

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_bounds_check_call", NULL);

   return;

}  /* gen_bounds_check_call */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      generate the call to the bounds check lib routine (which only         *|
|*      issues the message). When support exists for a bounds_opr, this       *|
|*      routine will generate that. This is for range checks for sections.    *|
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

static void gen_rbounds_check_call(char 	*var, 
                                   opnd_type 	*lb_opnd,
                                   opnd_type 	*ub_opnd, 
                                   opnd_type 	*start_opnd,
                                   opnd_type 	*end_opnd,
                                   opnd_type 	*inc_opnd,
                                   int 		dim, 
                                   int 		line, 
                                   int 		col)

{
   int                  call_idx;
   opnd_type            cond_opnd;
   int                  dim_idx;
   int                  end_sh_idx;
   expr_arg_type        exp_desc;
   int                  line_idx;
   int                  list_idx;
   opnd_type            opnd;
   int                  save_curr_stmt_sh_idx;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   int                  start_sh_idx;
   int                  tmp_idx;


   TRACE (Func_Entry, "gen_rbounds_check_call", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   /* save the bounding stmts for the gen_if_stmt call */

   start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   end_sh_idx = curr_stmt_sh_idx;

   gen_rbounds_condition(&cond_opnd,
                         start_opnd,
                         end_opnd,
                         inc_opnd,
                         lb_opnd,
                         ub_opnd,
                         line,
                         col);

   if (glb_tbl_idx[Rbounds_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Rbounds_Attr_Idx] = create_lib_entry_attr(
                                                   RBOUNDS_LIB_ENTRY,
                                                   RBOUNDS_NAME_LEN,
                                                   line,
                                                   col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Rbounds_Attr_Idx]);

   /* count (= 0) must be static temp */

   tmp_idx  = gen_initialized_tmp(CN_INTEGER_ZERO_IDX, line, col);
   line_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, line);
   dim_idx  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, dim);
   list_idx = gen_il(10, TRUE, line, col,
                     CN_Tbl_Idx, put_file_name_in_cn(line),     /* file name */
                     CN_Tbl_Idx, line_idx,                      /* line */
                     CN_Tbl_Idx, put_c_str_in_cn(var),          /* var name */
                     CN_Tbl_Idx, dim_idx,                       /* dim */
                     OPND_FLD((*lb_opnd)), OPND_IDX((*lb_opnd)),/* lower bd */
                     OPND_FLD((*ub_opnd)), OPND_IDX((*ub_opnd)),/* upper bd */
                     OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)),
                     OPND_FLD((*end_opnd)), OPND_IDX((*end_opnd)),
                     OPND_FLD((*inc_opnd)), OPND_IDX((*inc_opnd)),
                     AT_Tbl_Idx, tmp_idx);                      /* count */

   call_idx = gen_ir(AT_Tbl_Idx, glb_tbl_idx[Rbounds_Attr_Idx],
                 Call_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                     IL_Tbl_Idx, list_idx);

   gen_sh(Before, Call_Stmt, line, col,
          FALSE, FALSE, TRUE);
   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   gen_opnd(&opnd, call_idx, IR_Tbl_Idx, line, col);

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;

   exp_desc        = init_exp_desc;
   call_list_semantics(&opnd, &exp_desc, FALSE);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;

   gen_if_stmt(&cond_opnd,
               SH_NEXT_IDX(start_sh_idx),
               SH_PREV_IDX(end_sh_idx),
               NULL_IDX,
               NULL_IDX,
               line,
               col);

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_rbounds_check_call", NULL);

   return;

}  /* gen_rbounds_check_call */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      generate the call to the substring bounds check lib routine (which    *|
|*      issues the message). When support exists for a sbounds_opr, this      *|
|*      routine will generate that.                                           *|
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

static void gen_sbounds_check_call(char *var, opnd_type *size_opnd, 
                                   opnd_type *start_opnd, 
                                   opnd_type *subln_opnd, int line, int col)

{
   int                  call_idx;
   opnd_type		cond_opnd;
   int			end_sh_idx;
   expr_arg_type        exp_desc;
   int			ir_idx;
   int			line_idx;
   int                  list_idx;
   int			lt_idx;
   int			minus_idx;
   int			minus_idx2;
   opnd_type            opnd;
   int			plus_idx;
   int			plus_idx2;
   int                  save_curr_stmt_sh_idx;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   int			start_sh_idx;
   int                  tmp_idx;


   TRACE (Func_Entry, "gen_sbounds_check_call", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   /* save the bounding stmts for the gen_if_stmt call */

   start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   end_sh_idx = curr_stmt_sh_idx;

   /* generate the condition. */

   /* if start is a constant, it is assumed that the start value */
   /* was checked at compile time and is not below 1.            */
   /* (1 + size_opnd) - (start_opnd + subln_opnd) < 0 => error   */
   /* else if start is not a constant ...                        */
   /* (((1 + size_opnd) - (start_opnd + subln_opnd)) .bor.       */
   /*                                      (start_opnd - 1)) < 0 */

   plus_idx = gen_ir(CN_Tbl_Idx, CN_INTEGER_ONE_IDX,
                 Plus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                     OPND_FLD((*size_opnd)), OPND_IDX((*size_opnd)));

   plus_idx2 = gen_ir(OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)),
                  Plus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                      OPND_FLD((*subln_opnd)), OPND_IDX((*subln_opnd)));

   minus_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                  Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                      IR_Tbl_Idx, plus_idx2);

   if (OPND_FLD((*start_opnd)) == CN_Tbl_Idx) {
      lt_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                  Lt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                      CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);
   }
   else {

      minus_idx2 = gen_ir(OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)),
                      Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                          CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

      ir_idx = gen_ir(IR_Tbl_Idx, minus_idx2,
                  Bor_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                      IR_Tbl_Idx, minus_idx);

      lt_idx = gen_ir(IR_Tbl_Idx, ir_idx,
                  Lt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                      CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);
   }

   gen_opnd(&cond_opnd, lt_idx, IR_Tbl_Idx, line, col);

   if (glb_tbl_idx[Sbounds_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Sbounds_Attr_Idx] = create_lib_entry_attr(
                                                   SBOUNDS_LIB_ENTRY,
                                                   SBOUNDS_NAME_LEN,
                                                   line,
                                                   col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Sbounds_Attr_Idx]);

   /* count (= 0) must be static temp */

   tmp_idx = gen_initialized_tmp(CN_INTEGER_ZERO_IDX, line, col);
   line_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, line);
   list_idx = gen_il(7, TRUE, line, col,
                     CN_Tbl_Idx, put_file_name_in_cn(line),     /* file name */
                     CN_Tbl_Idx, line_idx,                      /* line */
                     CN_Tbl_Idx, put_c_str_in_cn(var),		/* var name */
                     OPND_FLD((*size_opnd)), OPND_IDX((*size_opnd)),
                     OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)),
                     OPND_FLD((*subln_opnd)), OPND_IDX((*subln_opnd)),
                     AT_Tbl_Idx, tmp_idx);                      /* count */

   call_idx = gen_ir(AT_Tbl_Idx, glb_tbl_idx[Sbounds_Attr_Idx],
                 Call_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                     IL_Tbl_Idx, list_idx);

   gen_sh(Before, Call_Stmt, line, col,
          FALSE, FALSE, TRUE);
   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   gen_opnd(&opnd, call_idx, IR_Tbl_Idx, line, col);

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;

   exp_desc        = init_exp_desc;
   call_list_semantics(&opnd, &exp_desc, FALSE);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;

   gen_if_stmt(&cond_opnd,
               SH_NEXT_IDX(start_sh_idx),
               SH_PREV_IDX(end_sh_idx),
               NULL_IDX,
               NULL_IDX,
               line,
               col);

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_sbounds_check_call", NULL);

   return;

}  /* gen_sbounds_check_call */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      generate the call to the NULL pointer checking lib routine (which     *|
|*      issues the message). When support exists for a ptr_chk_opr, this      *|
|*      routine will generate that.                                           *|
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

static void gen_ptr_chk_call(char 	*var, 
                             int	dv_desc,
                             opnd_type	*dv_opnd,
                             int 	line, 
                             int 	col)

{
   int                  call_idx;
   opnd_type            cond_opnd;
   int			dv_idx;
   int                  end_sh_idx;
   int                  eq_idx;
   expr_arg_type        exp_desc;
   int                  ir_idx;
   int			line_idx;
   int                  list_idx;
   opnd_type            opnd;
   int                  save_curr_stmt_sh_idx;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   int                  start_sh_idx;
   int                  tmp_idx;


   TRACE (Func_Entry, "gen_ptr_chk_call", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   /* save the bounding stmts for the gen_if_stmt call */

   start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   end_sh_idx = curr_stmt_sh_idx;

   /* generate the condition. */

   ir_idx = gen_ir(OPND_FLD((*dv_opnd)), OPND_IDX((*dv_opnd)),
               Dv_Access_Assoc, CG_INTEGER_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   eq_idx = gen_ir(IR_Tbl_Idx, ir_idx,
               Eq_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

   gen_opnd(&cond_opnd, eq_idx, IR_Tbl_Idx, line, col);

   if (glb_tbl_idx[Ptr_Chk_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Ptr_Chk_Attr_Idx] = create_lib_entry_attr(
                                                   PTR_CHK_LIB_ENTRY,
                                                   PTR_CHK_NAME_LEN,
                                                   line,
                                                   col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Ptr_Chk_Attr_Idx]);

   /* count (= 0) must be static temp */

   tmp_idx = gen_initialized_tmp(CN_INTEGER_ZERO_IDX, line, col);
   line_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, line);
   dv_idx  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, dv_desc);

   list_idx = gen_il(5, TRUE, line, col,
                     CN_Tbl_Idx, put_file_name_in_cn(line),     /* file name */
                     CN_Tbl_Idx, line_idx,                      /* line */
                     CN_Tbl_Idx, put_c_str_in_cn(var),          /* var name */
                     CN_Tbl_Idx, dv_idx,                        /* dv_desc */
                     AT_Tbl_Idx, tmp_idx);                      /* count */

   call_idx = gen_ir(AT_Tbl_Idx, glb_tbl_idx[Ptr_Chk_Attr_Idx],
                 Call_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                     IL_Tbl_Idx, list_idx);

   gen_sh(Before, Call_Stmt, line, col,
          FALSE, FALSE, TRUE);
   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   gen_opnd(&opnd, call_idx, IR_Tbl_Idx, line, col);

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;

   exp_desc        = init_exp_desc;
   call_list_semantics(&opnd, &exp_desc, FALSE);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;

   gen_if_stmt(&cond_opnd,
               SH_NEXT_IDX(start_sh_idx),
               SH_PREV_IDX(end_sh_idx),
               NULL_IDX,
               NULL_IDX,
               line,
               col);

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_ptr_chk_call", NULL);

   return;

}  /* gen_ptr_chk_call */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	generate a static compiler temp with the type of the given contant    *|
|*      and initialize it to the constant.                                    *|
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

int gen_initialized_tmp(int	cn_idx,
                        int	line,
                        int	col)

{
   int	asg_idx;
   int	list_idx;
   int	tmp_idx;

   TRACE (Func_Entry, "gen_initialized_tmp", NULL);

   tmp_idx                    = gen_compiler_tmp(line,col, Shared, TRUE);
   ATD_TYPE_IDX(tmp_idx)      = CN_TYPE_IDX(cn_idx);

   ATD_SAVED(tmp_idx)         = TRUE;
   ATD_DATA_INIT(tmp_idx)     = TRUE;
   ATD_STOR_BLK_IDX(tmp_idx)  = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
   ATD_FLD(tmp_idx)           = CN_Tbl_Idx;
   ATD_TMP_IDX(tmp_idx)       = cn_idx;
   AT_SEMANTICS_DONE(tmp_idx) = TRUE;

   /* create data init stmt */
   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Init_Opr;
   IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;
   IR_LINE_NUM_L(asg_idx) = line;
   IR_COL_NUM_L(asg_idx)  = col;
   IR_FLD_L(asg_idx)    = AT_Tbl_Idx;
   IR_IDX_L(asg_idx)    = tmp_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_IDX_R(asg_idx) = list_idx;
   IR_LIST_CNT_R(asg_idx) = 3;

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = cn_idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   gen_sh(Before, Assignment_Stmt, line, col,
          FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "gen_initialized_tmp", NULL);

   return(tmp_idx);

}  /* gen_initialized_tmp */

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

static int put_file_name_in_cn(int      line)

{
   int          cn_idx;
   int          idx;
   char         name[MAX_FILE_NAME_SIZE];


   TRACE (Func_Entry, "put_file_name_in_cn", NULL);

   /*******************************************************\
   |* THIS ROUTINE IS ONLY FOR RUNTIME CHECKING CALLS !!! *|
   \*******************************************************/

   strcpy(name, global_to_local_file(line));

   for (idx = strlen(name) - 1; idx >= 0; idx--) {
      if (name[idx] == '/')
         break;
   }

   idx++;

   cn_idx = put_c_str_in_cn(&(name[idx]));

   TRACE (Func_Exit, "put_file_name_in_cn", NULL);

   return(cn_idx);

}  /* put_file_name_in_cn */

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

static int put_c_str_in_cn(char *ch_ptr)

{
   int          cn_idx;
   int          i;
   long		length;
   long_type    the_constant[(MAX_FILE_NAME_SIZE + TARGET_CHARS_PER_WORD - 1)/
                             TARGET_CHARS_PER_WORD];
   int          type_idx;

   TRACE (Func_Entry, "put_c_str_in_cn", NULL);

   /*******************************************************\
   |* THIS ROUTINE IS ONLY FOR RUNTIME CHECKING CALLS !!! *|
   \*******************************************************/

   for (i = 0; i < (MAX_FILE_NAME_SIZE + TARGET_CHARS_PER_WORD - 1)/
                             TARGET_CHARS_PER_WORD; i++) {
      the_constant[i] = 0;
   }

   length = (long) strlen(ch_ptr);

   /* add one to length for the null byte */
   length++;

   strcpy((char *)the_constant, ch_ptr);

   if (two_word_fcd) {
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)       = Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)    = WORD_ALIGNED_BIT_LENGTH(length * CHAR_BIT);
      type_idx                     = ntr_type_tbl();
   }
   else {
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)    = Character;
      TYP_LINEAR(TYP_WORK_IDX)  = CHARACTER_DEFAULT_TYPE;
      TYP_DESC(TYP_WORK_IDX)    = Default_Typed;
      TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)     = CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)     = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, length);
      type_idx                  = ntr_type_tbl();
   }

   cn_idx = ntr_const_tbl(type_idx,
                          TRUE,
                          the_constant);

   TRACE (Func_Exit, "put_c_str_in_cn", NULL);

   return(cn_idx);

}  /* put_c_str_in_cn */

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

void gen_internal_call_stmt(char 		*name,
			    opnd_type		*opnd,
			    sh_position_type	position)

{

   int		call_idx;
   int		list_idx;
   int		loc_idx;
   int		lib_idx;

   TRACE (Func_Entry, "gen_internal_call_stmt", NULL);

   lib_idx = create_lib_entry_attr(name,
                                   strlen(name),
                                   stmt_start_line,
                                   stmt_start_col);

   ADD_ATTR_TO_LOCAL_LIST(lib_idx);

   NTR_IR_TBL(call_idx);
   IR_OPR(call_idx) = Call_Opr;
   IR_TYPE_IDX(call_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(call_idx) = stmt_start_line;
   IR_COL_NUM(call_idx) = stmt_start_col;
   IR_FLD_L(call_idx) = AT_Tbl_Idx;
   IR_IDX_L(call_idx) = lib_idx;
   IR_LINE_NUM_L(call_idx) = stmt_start_line;
   IR_COL_NUM_L(call_idx) = stmt_start_col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(call_idx) = IL_Tbl_Idx;
   IR_IDX_R(call_idx) = list_idx;
   IR_LIST_CNT_R(call_idx) = 1;

   NTR_IR_TBL(loc_idx);

   if (OPND_FLD((*opnd)) == CN_Tbl_Idx) {
      IR_OPR(loc_idx) = Const_Tmp_Loc_Opr;
      IR_TYPE_IDX(loc_idx) = CN_TYPE_IDX(OPND_IDX((*opnd)));
   }
   else {
      IR_OPR(loc_idx) = Aloc_Opr;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
   }

   IR_LINE_NUM(loc_idx) = stmt_start_line;
   IR_COL_NUM(loc_idx)  = stmt_start_col;
   IL_FLD(list_idx) = IR_Tbl_Idx;
   IL_IDX(list_idx) = loc_idx;

   COPY_OPND(IR_OPND_L(loc_idx), (*opnd));

   gen_sh(position, Call_Stmt, stmt_start_line,
           stmt_start_col, FALSE, FALSE, TRUE);

   if (position == Before) {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = call_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx)     = call_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }

   TRACE (Func_Exit, "gen_internal_call_stmt", NULL);

   return;

}  /* gen_internal_call_stmt */

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

void gen_lb_array_ref(opnd_type		*result_opnd,
                      int		attr_idx)

{
   int		bd_idx;
   int		i;
   int		list_idx;
   int		sub_idx;

   TRACE (Func_Entry, "gen_lb_array_ref", NULL);

   bd_idx = ATD_ARRAY_IDX(attr_idx);

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = ATD_TYPE_IDX(attr_idx);
   IR_LINE_NUM(sub_idx) = stmt_start_line;
   IR_COL_NUM(sub_idx) = stmt_start_col;
   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_IDX_L(sub_idx) = attr_idx;
   IR_LINE_NUM_L(sub_idx) = stmt_start_line;
   IR_COL_NUM_L(sub_idx) = stmt_start_col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_R(sub_idx) = BD_RANK(bd_idx);
   IR_IDX_R(sub_idx) = list_idx;

   IL_FLD(list_idx) = BD_LB_FLD(bd_idx, 1);
   IL_IDX(list_idx) = BD_LB_IDX(bd_idx, 1);
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx)  = stmt_start_col;

   for (i = 2; i <= BD_RANK(bd_idx); i++) {
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
      IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
      IL_LINE_NUM(list_idx) = stmt_start_line;
      IL_COL_NUM(list_idx)  = stmt_start_col;
   }

   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = sub_idx;

   TRACE (Func_Exit, "gen_lb_array_ref", NULL);

   return;

}  /* gen_lb_array_ref */

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

void set_up_exp_desc(opnd_type          *top_opnd,
                     expr_arg_type      *exp_desc)

{
   int          attr_idx;
   int          col;
   int          line;

   TRACE (Func_Entry, "set_up_exp_desc", NULL);

   (*exp_desc) = init_exp_desc;

   find_opnd_line_and_column(top_opnd, &line, &col);

   switch (OPND_FLD((*top_opnd))) {
      case AT_Tbl_Idx:
         attr_idx = OPND_IDX((*top_opnd));

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            exp_desc->type_idx = ATD_TYPE_IDX(attr_idx);
            exp_desc->type = TYP_TYPE(exp_desc->type_idx);
            exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
         }
# ifdef _DEBUG
         else {
            PRINTMSG(line, 626, Internal, col,
                     "Data_Obj", "set_up_exp_desc");
         }
# endif
         break;

      case IR_Tbl_Idx:
         exp_desc->type_idx = IR_TYPE_IDX(OPND_IDX((*top_opnd)));
         exp_desc->type = TYP_TYPE(exp_desc->type_idx);
         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
         exp_desc->rank = IR_RANK(OPND_IDX((*top_opnd)));
         break;

      case CN_Tbl_Idx:
         exp_desc->type_idx = CN_TYPE_IDX(OPND_IDX((*top_opnd)));
         exp_desc->type = TYP_TYPE(exp_desc->type_idx);
         exp_desc->linear_type = TYP_LINEAR(exp_desc->type_idx);
         break;

      default:
# ifdef _DEBUG
         PRINTMSG(line, 626, Internal, col,
                  "AT_Tbl_Idx, IR_Tbl_Idx, or CN_Tbl_Idx",
                  "set_up_exp_desc");
# endif
         break;
   }

   TRACE (Func_Exit, "set_up_exp_desc", NULL);

   return;

}  /* set_up_exp_desc */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Swap the dimensions for certain array bounds and references for the   *|
|*      current scope.                                                        *|
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

void dim_reshape_pass_driver (void)

{
   int          al_idx;
   int          attr_idx;
   opnd_type    opnd;
   int          save_curr_stmt_sh_idx;


   TRACE (Func_Entry, "dim_reshape_pass_driver", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   /* first, create new bounds table entries for reshape candidates */

   al_idx = SCP_RESHAPE_ARRAY_LIST(curr_scp_idx);

   while (al_idx) {
      attr_idx = AL_ATTR_IDX(al_idx);

# ifdef _DEBUG
      if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
          ! ATD_RESHAPE_ARRAY_OPT(attr_idx)) {

         PRINTMSG(1, 626, Internal, 1,
                  "ATD_RESHAPE_ARRAY_OPT flag", "dim_reshape_pass_driver");
      }

      if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX ||
          ATD_RESHAPE_ARRAY_IDX(attr_idx) == NULL_IDX) {
         PRINTMSG(1, 626, Internal, 1,
                  "ATD_RESHAPE_ARRAY_IDX", "dim_reshape_pass_driver");
      }
# endif

      ATD_ARRAY_IDX(attr_idx) = ATD_RESHAPE_ARRAY_IDX(attr_idx);
      al_idx = AL_NEXT_IDX(al_idx);
   }

   /* second, traverse the ir to reshape reference dimensions */

   curr_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);

   while (curr_stmt_sh_idx != NULL_IDX) {

      if (SH_IR_IDX(curr_stmt_sh_idx) != NULL_IDX) {
         OPND_FLD(opnd) = IR_Tbl_Idx;
         OPND_IDX(opnd) = SH_IR_IDX(curr_stmt_sh_idx);

         reshape_reference_subscripts(&opnd);

         SH_IR_IDX(curr_stmt_sh_idx) = OPND_IDX(opnd);
      }

      curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   PRINT_IR_TBL4;

   TRACE (Func_Exit, "dim_reshape_pass_driver", NULL);

   return;

}  /* dim_reshape_pass_driver */

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

static void reshape_reference_subscripts(opnd_type *result_opnd)

{
   int                  attr_idx;
   int                  col;
   int                  ir_idx;
   int                  line;
   int                  head;
   int                  list_idx;
   opnd_type            opnd;


   TRACE (Func_Entry, "reshape_reference_subscripts", NULL);

   switch (OPND_FLD((*result_opnd))) {
   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*result_opnd));

      COPY_OPND(opnd, IR_OPND_L(ir_idx));
      reshape_reference_subscripts(&opnd);
      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      reshape_reference_subscripts(&opnd);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      if (IR_OPR(ir_idx) == Subscript_Opr ||
          IR_OPR(ir_idx) == Whole_Subscript_Opr ||
          IR_OPR(ir_idx) == Section_Subscript_Opr) {

         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         attr_idx = find_base_attr(&opnd, &line, &col);

         if (ATD_RESHAPE_ARRAY_OPT(attr_idx)) {
            gen_opnd(&opnd, ir_idx, IR_Tbl_Idx, IR_LINE_NUM(ir_idx),
                                                IR_COL_NUM(ir_idx));
            copy_subtree(&opnd, result_opnd);
            ir_idx = OPND_IDX((*result_opnd));

            list_idx = IR_IDX_R(ir_idx);
            head = list_idx;

            while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
            IR_IDX_R(ir_idx) = list_idx;
            IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list_idx)) = NULL_IDX;
            IL_PREV_LIST_IDX(list_idx) = NULL_IDX;
            IL_NEXT_LIST_IDX(list_idx) = head;
            IL_PREV_LIST_IDX(head) = list_idx;
         }
      }
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*result_opnd));

      while (list_idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         reshape_reference_subscripts(&opnd);
         COPY_OPND(IL_OPND(list_idx), opnd);

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;
   }

   TRACE (Func_Exit, "reshape_reference_subscripts", NULL);

   return;

}  /* reshape_reference_subscripts */

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

boolean check_for_legal_define(opnd_type	*top_opnd)
#ifdef KEY /* Bug 14150 */
{
  return check_for_legal_assignment_define(top_opnd, FALSE);
}

/*
 * top_opnd		Top operand of operation which defines lvalue
 * pointer_assign	True if assignment is "=>" not "=" or other defining
 *			operation
 */
boolean check_for_legal_assignment_define(opnd_type *top_opnd,
  boolean pointer_assign)
#endif /* KEY Bug 14150 */
{
   int		attr_idx;
   int		col;
   int		line;
   boolean	ok = TRUE;
   opnd_type	opnd;

   TRACE (Func_Entry, "check_for_legal_define", NULL);

   COPY_OPND(opnd, (*top_opnd));

   while (OPND_FLD(opnd) == IR_Tbl_Idx) {
      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   if (OPND_FLD(opnd) == AT_Tbl_Idx &&
       AT_OBJ_CLASS(OPND_IDX(opnd)) == Data_Obj) {

      attr_idx = OPND_IDX(opnd);
      line = OPND_LINE_NUM(opnd);
      col = OPND_COL_NUM(opnd);

      if (ATD_LIVE_DO_VAR(attr_idx)) {
         PRINTMSG(line, 48, Error, col);
         ok = FALSE;
      }
      else if (ATD_PURE(attr_idx)) {
         PRINTMSG(line, 1270, Error, col,
                  AT_OBJ_NAME_PTR(attr_idx),
                  ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure":"elemental");
         ok = FALSE;
      }
      else if (ATD_CLASS(attr_idx) == Dummy_Argument   &&
#ifdef KEY /* Bug 14150 */
	       /* "l = expr" is ok, "l => expr" is not */
               ((!ATD_POINTER(attr_idx)) || pointer_assign) &&
#endif /* KEY Bug 14150 */
               ATD_INTENT(attr_idx) == Intent_In) {
         PRINTMSG(line, 890, Error, col,
                  AT_OBJ_NAME_PTR(attr_idx));
         ok = FALSE;
      }
      else if (ATD_FORALL_INDEX(attr_idx)) {
         PRINTMSG(line, 1608, Error, col,
                  AT_OBJ_NAME_PTR(attr_idx));
         ok = FALSE;
      }
      else if (ATD_SYMBOLIC_CONSTANT(attr_idx) &&
               (ATD_CLASS(attr_idx) == Variable ||
                ATD_CLASS(attr_idx) == Constant)) {
         PRINTMSG(line, 1632, Error, col,
                  AT_OBJ_NAME_PTR(attr_idx));
         ok = FALSE;
      }
   }


   TRACE (Func_Exit, "check_for_legal_define", NULL);

   return(ok);

}  /* check_for_legal_define */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check for a dependence in an arbitrary expression.                    *|
|*									      *|
|* Input parameters:							      *|
|*	item	opject which we will search for in exp                        *|
|*      exp     this is the expression that is to be searched                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	boolean indicating a dependence was found                             *|
|*									      *|
\******************************************************************************/
void check_dependence(boolean    *dependant,
                      opnd_type  item,
                      opnd_type  exp)

{
   int		attr_idx;
   int		idx;
   int		fld;
   int		line;
   int		col;

static   int		level;
static   boolean	target_found;
static   boolean	pointer_found;
static   boolean	pointer_item;
static   boolean	target_item;

   TRACE (Func_Entry, "check_dependence", NULL);
   level = level + 1;


   attr_idx = find_base_attr(&item, &line, &col);
   if (ATD_POINTER(attr_idx))  pointer_item = TRUE;
   if (ATD_TARGET(attr_idx))  target_item = TRUE;
   if (ATD_CLASS(attr_idx) == CRI__Pointee)  *dependant = TRUE;

   attr_idx = find_left_attr(&item);
   if (ATD_EQUIV(attr_idx))  *dependant = TRUE;

   idx = OPND_IDX(exp);
   fld = OPND_FLD(exp);

   if (idx != NULL_IDX) {

      switch(fld) {
         case IR_Tbl_Idx :
            if (IR_FLD_R(idx) != NO_Tbl_Idx) {
               check_dependence(dependant, item, IR_OPND_R(idx));
            }

            if (IR_FLD_L(idx) != NO_Tbl_Idx) {
               check_dependence(dependant, item, IR_OPND_L(idx));
            }
            break;

         case AT_Tbl_Idx :
            if (AT_OBJ_CLASS(idx) == Data_Obj) {
               if (ATD_TARGET(idx))  target_found = TRUE;
               if (ATD_POINTER(idx))  pointer_found = TRUE;
               if (idx == attr_idx) *dependant = TRUE;
            }
            break;

         case NO_Tbl_Idx :
         case CN_Tbl_Idx :
         case SH_Tbl_Idx :
            break;

         case IL_Tbl_Idx :
            while (idx != NULL_IDX) {
               if (IL_FLD(idx) != NO_Tbl_Idx) {
                  check_dependence(dependant, item, IL_OPND(idx));
               }
               idx = IL_NEXT_LIST_IDX(idx);
            }
            break;
      }
   }


   level = level - 1;
   if (level == 0) {
      if (target_found && pointer_item) *dependant = TRUE;
      if (pointer_found && pointer_item) *dependant = TRUE;
      if (pointer_found && target_item) *dependant = TRUE;
      target_found = FALSE;
      pointer_found = FALSE;
      pointer_item = FALSE;
      target_item = FALSE;
   }

   TRACE (Func_Exit, "check_dependence", NULL);

}  /* check_dependence */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine takes an array section (or whole array) reference and    *|
|*      returns an array element reference corresponding to which_one.        *|
|*	the section must be rank 1 and which_one is 1 based.                  *|
|*									      *|
|* Input parameters:							      *|
|*	section_opnd	- the array section                                   *|
|*	which_one	- which element you want (1,2,3,...)                  *|
|*									      *|
|* Output parameters:							      *|
|*	element_opnd	- the resulting element reference tree.               *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void change_section_to_this_element(opnd_type	*section_opnd,
                                    opnd_type	*element_opnd,
                                    int		which_one)

{
   int			col;
   expr_arg_type	exp_desc;
   int			line;
   int			list_idx;
   int			mult_idx;
   opnd_type		opnd1;
   opnd_type		opnd2;
   int			plus_idx;
   int			rank_idx = NULL_IDX;
   cif_usage_code_type	save_xref_state;
   int			start_list_idx;
   int			stride_list_idx;
   int			trip_idx;
   int			unused = NULL_IDX;

   TRACE (Func_Entry, "change_section_to_this_element", NULL);

   find_opnd_line_and_column(section_opnd, &line, &col);

# ifdef _DEBUG
   if (OPND_FLD((*section_opnd)) != IR_Tbl_Idx ||
       IR_RANK(OPND_IDX((*section_opnd))) != 1) {
      PRINTMSG(line, 626, Internal, col,
               "rank 1 array", "change_section_to_this_element");
   }
# endif

   copy_subtree(section_opnd, element_opnd);

   just_find_dope_and_rank(element_opnd, &rank_idx, &unused);

# ifdef _DEBUG
   if (rank_idx == NULL_IDX) {
      PRINTMSG(line, 626, Internal, col,
               "section subscript", "change_section_to_this_element");
   }
# endif

   IR_OPR(rank_idx) = Subscript_Opr;

   list_idx = IR_IDX_R(rank_idx);

   while (list_idx) {
      if (IL_VECTOR_SUBSCRIPT(list_idx)) {
         COPY_OPND(opnd1, IL_OPND(list_idx));
         change_section_to_this_element(&opnd1, &opnd2, which_one);
         COPY_OPND(IL_OPND(list_idx), opnd2);
         break;
      }
      else if (IL_FLD(list_idx) == IR_Tbl_Idx &&
               IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

         trip_idx = IL_IDX(list_idx);
         start_list_idx = IR_IDX_L(trip_idx);
         stride_list_idx = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(start_list_idx));
         line = IR_LINE_NUM(trip_idx);
         col = IR_COL_NUM(trip_idx);

         
         mult_idx = gen_ir(CN_Tbl_Idx, C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   (which_one - 1)),
                       Mult_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                           IL_FLD(stride_list_idx), IL_IDX(stride_list_idx));

         plus_idx = gen_ir(IL_FLD(start_list_idx), IL_IDX(start_list_idx),
                       Plus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                           IR_Tbl_Idx, mult_idx);

         gen_opnd(&opnd1, plus_idx, IR_Tbl_Idx, line, col);

         exp_desc        = init_exp_desc;
         exp_desc.rank   = 0;
         save_xref_state = xref_state;
         xref_state      = CIF_No_Usage_Rec;
                           expr_semantics(&opnd1, &exp_desc);
         xref_state      = save_xref_state;

         COPY_OPND(IL_OPND(list_idx), opnd1);

         break;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   COPY_OPND(opnd1, (*element_opnd));

   while (OPND_FLD(opnd1) == IR_Tbl_Idx) {
      IR_RANK(OPND_IDX(opnd1)) = 0;
      COPY_OPND(opnd1, IR_OPND_L(OPND_IDX(opnd1)));
   }
   

   TRACE (Func_Exit, "change_section_to_this_element", NULL);

   return;

}  /* change_section_to_this_element */

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

void gen_if_stmt(opnd_type	*cond_opnd,
		 int		true_start_sh_idx,
		 int		true_end_sh_idx,
		 int		false_start_sh_idx,
		 int		false_end_sh_idx,
		 int		line,
		 int		col)

{
   int		else_idx;
   int		endif_idx;
   int		if_idx;
   int		save_curr_stmt_sh_idx;
#ifdef KEY /* Bug 10177 */
   int		type_idx = 0;
#else /* KEY Bug 10177 */
   int		type_idx;
#endif /* KEY Bug 10177 */

# if defined(_HIGH_LEVEL_IF_FORM)
   int		if_sh_idx;
   int		parent_sh_idx;
# else
   int		label1_idx;
   int		label2_idx;
# endif


   TRACE (Func_Entry, "gen_if_stmt", NULL);

# ifdef _DEBUG
   if (SH_PREV_IDX(true_start_sh_idx) == true_end_sh_idx) {
      PRINTMSG(line, 626, Internal, col,
               "proper true block", "gen_if_stmt");
   }

   if (false_start_sh_idx &&
       SH_PREV_IDX(false_start_sh_idx) != true_end_sh_idx) {
      PRINTMSG(line, 626, Internal, col,
               "proper false block", "gen_if_stmt");
   }

   if (false_start_sh_idx &&
       SH_PREV_IDX(false_start_sh_idx) == false_end_sh_idx) {
      PRINTMSG(line, 626, Internal, col,
               "proper false block", "gen_if_stmt");
   }
# endif

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   switch (OPND_FLD((*cond_opnd))) {
   case AT_Tbl_Idx:
      type_idx = ATD_TYPE_IDX(OPND_IDX((*cond_opnd)));
      break;

   case IR_Tbl_Idx:
      type_idx = IR_TYPE_IDX(OPND_IDX((*cond_opnd)));
      break;

   case CN_Tbl_Idx:
      type_idx = CN_TYPE_IDX(OPND_IDX((*cond_opnd)));
      break;

   default:
# ifdef _DEBUG
      PRINTMSG(line, 626, Internal, col,
               "valid logical condition", "gen_if_stmt");
# endif
      break;
   }

   curr_stmt_sh_idx = true_start_sh_idx;

# if defined(_HIGH_LEVEL_IF_FORM)

   if_idx = gen_ir(OPND_FLD((*cond_opnd)), OPND_IDX((*cond_opnd)),
              If_Opr, type_idx, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;

   if_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   curr_stmt_sh_idx = true_end_sh_idx;

   parent_sh_idx = if_sh_idx;

   if (false_start_sh_idx) {

      curr_stmt_sh_idx = false_start_sh_idx;

      else_idx = gen_ir(OPND_FLD((*cond_opnd)), OPND_IDX((*cond_opnd)),
                 Else_Opr, type_idx, line, col,
                      NO_Tbl_Idx, NULL_IDX);

      gen_sh(Before, Else_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = else_idx;
      parent_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      curr_stmt_sh_idx = false_end_sh_idx;

   }

   endif_idx = gen_ir(SH_Tbl_Idx, if_sh_idx,
                 Endif_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                      NO_Tbl_Idx, NULL_IDX);

   gen_sh(After, End_If_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   SH_IR_IDX(curr_stmt_sh_idx) = endif_idx;
   SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = parent_sh_idx;

   IR_FLD_R(if_idx) = SH_Tbl_Idx;
   IR_IDX_R(if_idx) = curr_stmt_sh_idx;
   IR_LINE_NUM_R(if_idx) = line;
   IR_COL_NUM_R(if_idx) = col;

# else

   label1_idx = gen_internal_lbl(line);

   if_idx = gen_ir(IR_Tbl_Idx,
                  gen_ir(OPND_FLD((*cond_opnd)),OPND_IDX((*cond_opnd)),
                     Not_Opr, type_idx, line, col,
                         NO_Tbl_Idx, NULL_IDX),
              Br_True_Opr, type_idx, line, col,
                   AT_Tbl_Idx, label1_idx);

   gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   curr_stmt_sh_idx = true_end_sh_idx;

   endif_idx = gen_ir(AT_Tbl_Idx, label1_idx,
              Label_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   gen_sh(After, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(curr_stmt_sh_idx) = endif_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   AT_DEFINED(label1_idx) = TRUE;
   ATL_DEF_STMT_IDX(label1_idx) = curr_stmt_sh_idx;

   if (false_start_sh_idx) {
      curr_stmt_sh_idx = true_end_sh_idx;

      label2_idx = gen_internal_lbl(line);

      else_idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
                 Br_Uncond_Opr, type_idx, line, col,
                      AT_Tbl_Idx, label2_idx);

      gen_sh(After, Goto_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = else_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      curr_stmt_sh_idx = false_end_sh_idx;

      endif_idx = gen_ir(AT_Tbl_Idx, label2_idx,
                 Label_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                      NO_Tbl_Idx, NULL_IDX);

      gen_sh(After, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = endif_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      AT_DEFINED(label2_idx) = TRUE;
      ATL_DEF_STMT_IDX(label2_idx) = curr_stmt_sh_idx;
   }


# endif


   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_if_stmt", NULL);

   return;

}  /* gen_if_stmt */

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

boolean	needs_bounds_check(int	sub_idx)

{
   int		base_attr;
   int		bd_idx;
   boolean	bound_chk;
   int		col;
   int		left_attr;
   int		line;

   TRACE (Func_Entry, "needs_bounds_check", NULL);

# ifdef _DEBUG
   if (IR_OPR(sub_idx) != Whole_Subscript_Opr &&
       IR_OPR(sub_idx) != Section_Subscript_Opr &&
       IR_OPR(sub_idx) != Subscript_Opr) {

      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "Subscript_Opr", "needs_bounds_check");
   }
# endif

   base_attr = find_base_attr(&IR_OPND_L(sub_idx), &line, &col);
   left_attr = find_left_attr(&IR_OPND_L(sub_idx));
   bd_idx = ATD_ARRAY_IDX(base_attr);

   bound_chk = (cdir_switches.bounds ||
                ATD_BOUNDS_CHECK(left_attr)) &&
               !ATD_NOBOUNDS_CHECK(left_attr);

   bound_chk &= ! (IR_WHOLE_ARRAY(sub_idx));

   if (IR_BOUNDS_DONE(sub_idx) ||
       IR_OPR(sub_idx) == Whole_Subscript_Opr ||
       ATD_CLASS(base_attr) == Compiler_Tmp) {
      bound_chk = FALSE;
   }

   if (BD_RANK(bd_idx) == 1 &&
       BD_ARRAY_CLASS(bd_idx) == Explicit_Shape &&
       BD_LB_FLD(bd_idx,1) == CN_Tbl_Idx &&
       compare_cn_and_value(BD_LB_IDX(bd_idx,1), 1, Eq_Opr) &&
       BD_UB_FLD(bd_idx,1) == CN_Tbl_Idx &&
       compare_cn_and_value(BD_UB_IDX(bd_idx,1), 1, Eq_Opr)) {

      bound_chk = FALSE;
   }
   

   TRACE (Func_Exit, "needs_bounds_check", NULL);

   return(bound_chk);

}  /* needs_bounds_check */

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

void gen_rbounds_condition(opnd_type	*cond_opnd,
                           opnd_type	*start_opnd,
                           opnd_type	*end_opnd,
                           opnd_type	*inc_opnd,
                           opnd_type	*lb_opnd,
                           opnd_type	*ub_opnd,
                           int		line,
                           int		col)

{
   int			and_idx;
   int                  div_idx;
   expr_arg_type        exp_desc;
   int                  gt_idx;
   int                  lt_idx;
   int                  minus_idx;
   int                  mult_idx;
   int                  or_idx1;
   int                  or_idx2;
   int                  or_idx3;
   opnd_type            opnd;
   int                  plus_idx;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   opnd_type		xt_opnd;


   TRACE (Func_Entry, "gen_rbounds_condition", NULL);

   /* cond_opnd = ((start < lb .or. start > ub) .or.                 */
   /* (start + (((end - start + inc) / inc) - 1) * inc < lb) .or.    */
   /* (start + (((end - start + inc) / inc) - 1) * inc > ub)) .and.  */
   /* (((end - start + inc) / inc) > 0)                              */

   /* start_opnd < lb */

   lt_idx = gen_ir(OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)),
               Lt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   OPND_FLD((*lb_opnd)), OPND_IDX((*lb_opnd)));

   /* start_opnd > ub */

   gt_idx = gen_ir(OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)),
               Gt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   OPND_FLD((*ub_opnd)), OPND_IDX((*ub_opnd)));



   or_idx1 = gen_ir(IR_Tbl_Idx, lt_idx,
                Or_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                    IR_Tbl_Idx, gt_idx);


   /* start + (((end - start + inc) / inc) - 1) * inc */

   minus_idx = gen_ir(OPND_FLD((*end_opnd)), OPND_IDX((*end_opnd)),
                  Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                      OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)));

   plus_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Plus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                     OPND_FLD((*inc_opnd)), OPND_IDX((*inc_opnd)));

   div_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                Div_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                    OPND_FLD((*inc_opnd)), OPND_IDX((*inc_opnd)));

   gen_opnd(&xt_opnd, div_idx, IR_Tbl_Idx, line, col);
   copy_subtree(&xt_opnd, &xt_opnd);

   minus_idx = gen_ir(IR_Tbl_Idx, div_idx,
                  Minus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                      CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

   mult_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                 Mult_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                     OPND_FLD((*inc_opnd)), OPND_IDX((*inc_opnd)));

   plus_idx = gen_ir(OPND_FLD((*start_opnd)), OPND_IDX((*start_opnd)),
                 Plus_Opr, CG_INTEGER_DEFAULT_TYPE, line, col,
                     IR_Tbl_Idx, mult_idx);

   lt_idx = gen_ir(IR_Tbl_Idx, plus_idx,
               Lt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   OPND_FLD((*lb_opnd)), OPND_IDX((*lb_opnd)));

   gen_opnd(&opnd, plus_idx, IR_Tbl_Idx, line, col);

   copy_subtree(&opnd, &opnd);

   gt_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
               Gt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   OPND_FLD((*ub_opnd)), OPND_IDX((*ub_opnd)));



   or_idx2 = gen_ir(IR_Tbl_Idx, lt_idx,
                Or_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                    IR_Tbl_Idx, gt_idx);


   or_idx3 = gen_ir(IR_Tbl_Idx, or_idx1,
                Or_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                    IR_Tbl_Idx, or_idx2);

   gt_idx = gen_ir(OPND_FLD(xt_opnd), OPND_IDX(xt_opnd),
               Gt_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

   and_idx = gen_ir(IR_Tbl_Idx, or_idx3,
                And_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                    IR_Tbl_Idx, gt_idx);

   gen_opnd(cond_opnd, and_idx, IR_Tbl_Idx, line, col);

   save_xref_state = xref_state;
   xref_state      = CIF_No_Usage_Rec;
   save_expr_mode  = expr_mode;
   expr_mode       = Regular_Expr;

   exp_desc        = init_exp_desc;
                      expr_semantics(cond_opnd, &exp_desc);
   xref_state = save_xref_state;
   expr_mode  = save_expr_mode;

   TRACE (Func_Exit, "gen_rbounds_condition", NULL);

   return;

}  /* gen_rbounds_condition */

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

void scan_for_ptr_chk(opnd_type	*top_opnd)

{
   opnd_type	dv_opnd;
   int		ir_idx;
   int		list_idx;
   opnd_type	opnd;

   TRACE (Func_Entry, "scan_for_ptr_chk", NULL);

   switch (OPND_FLD((*top_opnd))) {
   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*top_opnd));

      if (IR_OPR(ir_idx) == Dv_Deref_Opr) {
         COPY_OPND(dv_opnd, IR_OPND_L(ir_idx));
         gen_runtime_ptr_chk(&dv_opnd);
      }

      COPY_OPND(opnd, IR_OPND_L(ir_idx));
      scan_for_ptr_chk(&opnd);

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      scan_for_ptr_chk(&opnd);
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*top_opnd));

      while (list_idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         scan_for_ptr_chk(&opnd);
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;
   }

   TRACE (Func_Exit, "scan_for_ptr_chk", NULL);

   return;

}  /* scan_for_ptr_chk */

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

void runtime_ptr_chk_driver(void)

{
   opnd_type	opnd;
   int		save_curr_stmt_sh_idx;

   TRACE (Func_Entry, "runtime_ptr_chk_driver", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   curr_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);

   while (curr_stmt_sh_idx != NULL_IDX) {

      if (SH_IR_IDX(curr_stmt_sh_idx) != NULL_IDX) {
         gen_opnd(&opnd, SH_IR_IDX(curr_stmt_sh_idx), IR_Tbl_Idx,
                  SH_GLB_LINE(curr_stmt_sh_idx), SH_COL_NUM(curr_stmt_sh_idx));
         scan_for_ptr_chk(&opnd);
      }

      curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   }

   PRINT_IR_TBL4;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "runtime_ptr_chk_driver", NULL);

   return;

}  /* runtime_ptr_chk_driver */

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

void gen_copyin_bounds_stmt(int	attr_idx)

{
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int		col;
   int		ir_idx;
   int		line;

   TRACE (Func_Entry, "gen_copyin_bounds_stmt", NULL);

   line = AT_DEF_LINE(attr_idx);
   col = AT_DEF_COLUMN(attr_idx);

   ir_idx = gen_ir(AT_Tbl_Idx, attr_idx,
               Copyin_Bound_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   gen_sh(Before, Directive_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "gen_copyin_bounds_stmt", NULL);

# endif
   return;

}  /* gen_copyin_bounds_stmt */

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

void gen_dv_access_low_bound(opnd_type	*result_opnd,
                             opnd_type	*dv_opnd,
                             int	 dim)

{
   int			attr_idx;
   int			bd_idx;
   int			col;
   expr_arg_type	exp_desc;
   int			ir_idx;
   int			line;
   cif_usage_code_type	save_xref_state;


   TRACE (Func_Entry, "gen_dv_access_low_bound", NULL);

   attr_idx = find_base_attr(dv_opnd, &line, &col);

# ifdef _DEBUG
   if (! ATD_IM_A_DOPE(attr_idx)) {
      PRINTMSG(line, 626, Internal, col,
               "dope vector" , "gen_dv_low_bound");
   }
# endif

   bd_idx = ATD_ARRAY_IDX(attr_idx);

   if (bd_idx &&
       BD_ARRAY_CLASS(bd_idx) == Assumed_Shape) {

      gen_opnd(result_opnd, BD_LB_IDX(bd_idx,dim), BD_LB_FLD(bd_idx,dim),
               line, col);

      if (variable_size_func_expr &&
          OPND_FLD((*result_opnd)) == AT_Tbl_Idx &&
          ATD_CLASS(OPND_IDX((*result_opnd))) == Compiler_Tmp &&
          ATD_FLD(OPND_IDX((*result_opnd))) == IR_Tbl_Idx &&
          IR_OPR(ATD_TMP_IDX(OPND_IDX((*result_opnd)))) == Asg_Opr) {

         while (OPND_FLD((*result_opnd)) == AT_Tbl_Idx &&
                ATD_CLASS(OPND_IDX((*result_opnd))) == Compiler_Tmp &&
                ATD_FLD(OPND_IDX((*result_opnd))) == IR_Tbl_Idx &&
                IR_OPR(ATD_TMP_IDX(OPND_IDX((*result_opnd)))) == Asg_Opr) {

            COPY_OPND((*result_opnd), 
                      IR_OPND_R(ATD_TMP_IDX(OPND_IDX((*result_opnd)))));
         }

         exp_desc.rank = 0;
         
         save_xref_state = xref_state;
         xref_state      = CIF_No_Usage_Rec;
                           expr_semantics(result_opnd, &exp_desc);
         xref_state      = save_xref_state;
      }
   }
   else {
      ir_idx = gen_ir(OPND_FLD((*dv_opnd)), OPND_IDX((*dv_opnd)),
                  Dv_Access_Low_Bound, SA_INTEGER_DEFAULT_TYPE, line, col,
                      NO_Tbl_Idx, NULL_IDX);
      IR_DV_DIM(ir_idx) = dim;

      gen_opnd(result_opnd, ir_idx, IR_Tbl_Idx, line, col);
   }

   TRACE (Func_Exit, "gen_dv_access_low_bound", NULL);

   return;

}  /* gen_dv_access_low_bound */

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

long64	sm_unit_in_bits(int	type_idx)

{
   long64	bits;


   TRACE (Func_Entry, "sm_unit_in_bits", NULL);

# if defined(_SM_UNIT_IS_ELEMENT)

   switch (TYP_TYPE(type_idx)) {
   case Typeless:
      bits = TYP_BIT_LEN(type_idx);
      break;

   case Integer:
   case Logical:
   case CRI_Ptr:
   case CRI_Ch_Ptr:
   case Real:
   case Complex:
      bits = storage_bit_size_tbl[TYP_LINEAR(type_idx)];
      break;

   case Character:

# ifdef _DEBUG
      if (TYP_FLD(type_idx) != CN_Tbl_Idx) {
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "constant length character", "sm_unit_in_bits");
      }
# endif
      bits = CN_INT_TO_C(TYP_IDX(type_idx)) * 8;
      break;

   case Structure:
# ifdef _DEBUG
      if (ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx)) != CN_Tbl_Idx) {
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "constant length structure", "sm_unit_in_bits");
      }
# endif
      bits = CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx)));
      break;
   }

# else

   bits = stride_mult_unit_in_bits[TYP_LINEAR(type_idx)];

# endif

   TRACE (Func_Exit, "sm_unit_in_bits", NULL);

   return(bits);

}  /* sm_unit_in_bits */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Generate a data init or assignment stmt to initialize a temp to a     *|
|*      constant value. If it is an assignment, generate it a every entry.    *|
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

void gen_temp_init(int  attr_idx,
                   int  cn_idx)

{
   int		col;
   int		entry_attr_idx;
   int		entry_list_idx;
   int		entry_sh_idx;
   int		ir_idx;
   int		line;
   opnd_type	opnd;
   int		sh_idx;
   int		type_idx;

   TRACE (Func_Entry, "gen_temp_init", NULL);

   type_idx = ATD_TYPE_IDX(attr_idx);
   line = AT_DEF_LINE(attr_idx);
   col = AT_DEF_COLUMN(attr_idx);

   if (SB_RUNTIME_INIT(ATD_STOR_BLK_IDX(attr_idx))) {

      /* The var is on the stack, or is automatic, a darg or a func  */
      /* result.  Generate runtime code for the initialization.      */

      ir_idx = gen_ir(AT_Tbl_Idx, attr_idx,
                  Asg_Opr, type_idx, line, col,
                      CN_Tbl_Idx, cn_idx);

      gen_opnd(&opnd, ir_idx, IR_Tbl_Idx, line, col);

      sh_idx                       = ntr_sh_tbl();
      SH_STMT_TYPE(sh_idx)         = Assignment_Stmt;
      SH_GLB_LINE(sh_idx)          = line;
      SH_COL_NUM(sh_idx)           = col;
      SH_COMPILER_GEN(sh_idx)      = TRUE;
      SH_P2_SKIP_ME(sh_idx)        = TRUE;

      SH_IR_IDX(sh_idx) = ir_idx;

      insert_sh_chain_after_entries(sh_idx, sh_idx);
   }
   else {
      ir_idx = gen_ir(AT_Tbl_Idx, attr_idx,
                  Init_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                      IL_Tbl_Idx, gen_il(3,
                                         FALSE,
                                         line,
                                         col,
                                         CN_Tbl_Idx,
                                         cn_idx,
                                         CN_Tbl_Idx,
                                         CN_INTEGER_ONE_IDX,
                                         CN_Tbl_Idx,
                                         CN_INTEGER_ZERO_IDX));

      gen_sh(After,
             Type_Init_Stmt,
             line,
             col,
             FALSE,
             FALSE,
             TRUE);

      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
      SH_IR_IDX(curr_stmt_sh_idx)     = ir_idx;

   }


   TRACE (Func_Exit, "gen_temp_init", NULL);

   return;

}  /* gen_temp_init */
