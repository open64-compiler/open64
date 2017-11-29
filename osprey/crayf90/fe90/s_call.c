/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
/*
 * Copyright (C) 2007, 2008, 2009. PathScale, LLC. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_call.c	5.15	10/19/99 17:14:30\n";

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
# include "s_call.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"
# include "s_call.h"

# ifdef KEY
extern boolean LANG_Read_Write_Const;
extern boolean LANG_Copy_Inout;
extern unsigned int LANG_Copy_Inout_Level;
# endif
#ifdef KEY /* Bug 7424 */
extern boolean LANG_Ignore_Target_Attribute;
#endif /* KEY Bug 7424 */
boolean	variable_size_func_expr = FALSE;
#ifdef KEY
#define MAX_DIMENSION 14
#endif

#ifdef KEY /* Bug 8117 */
/* Clumsy way to prevent final_arg_work() from going into infinite recursion
 * when we generate calls to library functions _Copyin and _Copyout during
 * the processing of a user-coded call. We'd add a boolean argument to
 * final_arg_work, but it's used in so many places.  */
static int stop_recursion = FALSE;

#ifdef _DEBUG
/* Count the number of times that each variety of argument association occurs */
static int check_contig_flag_count = 0;
static int copy_in_make_dv_count = 0;
static int make_dv_count = 0;
static int copy_in_copy_out_count = 0;
static int copy_in_count = 0;
static int pass_dv_copy_count = 0;
static int pass_dv_count = 0;
static int pass_address_from_dv_count = 0;
static int pass_section_address_count = 0;
static int pass_address_count = 0;
static int copy_inout_make_dv_count = 0;
/* Count whenever COPY_INOUT_MAKE_DV can move its alloc code to the prolog */
static int move_copyinout_alloc_count = 0;
/* Count whenever CHECK_CONTIG_FLAG uses runtime instead of inline code */
static int runtime_copyinout_count = 0;

void print_arg_passing(FILE *fd) {
  if (0 == fd) {
    fd = stderr;
  }
  fprintf(fd, "check_contig_flag:\t%d\n", check_contig_flag_count);
  fprintf(fd, "copy_in_make_dv:\t%d\n", copy_in_make_dv_count);
  fprintf(fd, "make_dv:\t%d\n", make_dv_count);
  fprintf(fd, "copy_in_copy_out:\t%d\n", copy_in_copy_out_count);
  fprintf(fd, "copy_in:\t%d\n", copy_in_count);
  fprintf(fd, "pass_dv_copy:\t%d\n", pass_dv_copy_count);
  fprintf(fd, "pass_dv:\t%d\n", pass_dv_count);
  fprintf(fd, "pass_address_from_dv:\t%d\n", pass_address_from_dv_count);
  fprintf(fd, "pass_section_address:\t%d\n", pass_section_address_count);
  fprintf(fd, "pass_address:\t%d\n", pass_address_count);
  fprintf(fd, "copy_inout_make_dv:\t%d\n", copy_inout_make_dv_count);
  fprintf(fd, "move_copyinout_alloc:\t%d\n", move_copyinout_alloc_count);
  fprintf(fd, "runtime_copyinout:\t%d\n", runtime_copyinout_count);
}
#endif /* _DEBUG */
#endif /* KEY Bug 8117 */

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static	boolean		compare_darg_to_actual_arg(int, int, int, opnd_type,
                                                   int, int);
static	boolean		compare_func_result(int,int,int,int,int,int,int,int);
static	int		copy_and_replace_tmps (int);
static	int		create_tmp_DV_asg (int, int);
static	int		create_tmp_from_stmt_func (int);
static	void		determine_num_elements (opnd_type *, expr_arg_type *,
                                                int, int);
static	int		expand_user_bound (int);
static	void		find_attrs_in_il(int);
static	void		find_attrs_in_ir(int);
static	void		find_attrs_used_in_bound(int);
static	dummy_arg_type	get_dummy_arg_type (int);
static	void		make_new_bd_entries (int, int *, int *);
static	void		save_array_syntax_subscripts (int);
static	void		set_at_actual_arg (int);
static	void		process_variable_size_func (int, int, int, 
                                                    int, int *, int *);
static	void		check_bd_typ_for_dargs(int, int, opnd_type *);
static	void		search_expr_for_dargs(opnd_type *, opnd_type *);
static	int		gen_arg_type_descriptor(int, boolean, int, int);
static	int		gen_call_type_descriptor(int, int, boolean, int, int);
static  long_type       get_arg_type(int, boolean);
static	int		create_struct_argchck_tbl(int);
static	int		determine_struct_tbl_size(int);
static	void		fill_in_struct_argchck_const(int, int, int *);
static	void		make_base_assumed_size(opnd_type *, opnd_type *);
static	void		check_call_for_global_def(int, int, int);
static	void		ntr_ref_in_global_tbl(int, int, int, int *, int);

	void		gen_dbg_write_stmt(opnd_type *, sh_position_type);
#ifdef KEY /* Bug 11046 */
static	boolean		check_elemental_conformance(int, expr_arg_type *, int);
#else /* KEY Bug 11046 */
static	boolean		check_elemental_conformance(int, expr_arg_type *);
#endif /* KEY Bug 11046 */
static	void		check_for_constructors(opnd_type *, expr_arg_type *);
static	void		check_for_elementals(int);
static	void		check_expr_for_elementals(opnd_type *);
static	boolean		check_arg_for_co_array(opnd_type *);
static	void		update_components(opnd_type *);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
static	void		set_inline_state(int, int);
# endif
#ifdef KEY
static void Type_Converstion_to_Real(int ir_idx, int list_idx, int *prev_list_idx){

  int tmp_idx = IL_NEXT_LIST_IDX(list_idx);
  int list1_idx, idx, type_idx;
  int info_idx = IL_ARG_DESC_IDX(list_idx);

  NTR_IR_LIST_TBL(list1_idx);
  IL_FLD(list1_idx) = IR_Tbl_Idx;
  IL_ARG_DESC_VARIANT(list1_idx) = TRUE;
  IL_LINE_NUM(list1_idx) = IL_LINE_NUM(list_idx);
  IL_COL_NUM(list1_idx)  = IL_COL_NUM(list_idx);
  if (IR_IDX_R(ir_idx) == list_idx)
    IR_IDX_R(ir_idx) = list1_idx;
  IL_NEXT_LIST_IDX(list1_idx) = IL_NEXT_LIST_IDX(list_idx);
  if (*prev_list_idx != NULL_IDX)
    IL_NEXT_LIST_IDX(*prev_list_idx) = list1_idx;
  NTR_IR_TBL(idx);
  IR_TYPE_IDX(idx) = REAL_DEFAULT_TYPE;
  IR_RANK(idx) = IR_RANK(ir_idx);
  IR_LINE_NUM(idx) = IR_LINE_NUM(ir_idx);
  IR_COL_NUM(idx)  = IR_COL_NUM(ir_idx);
  IR_OPR(idx) = Real_Opr;
  IR_FLD_L(idx) = IL_Tbl_Idx;
  IR_IDX_L(idx) = list_idx;
  IR_OPND_R(idx) = null_opnd;
  IR_LIST_CNT_L(idx) = 1;
  IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
  IL_ARG_DESC_IDX(list1_idx) = info_idx;

  expr_arg_type exp_desc;
  opnd_type     opnd;
  COPY_OPND(opnd, IL_OPND(list1_idx));
  exp_desc.rank = 0;
  expr_semantics(&opnd, &exp_desc);
  COPY_OPND(IL_OPND(list1_idx), opnd);
  type_idx = REAL_DEFAULT_TYPE;
  exp_desc.type = TYP_TYPE(type_idx);
  exp_desc.linear_type = TYP_LINEAR(type_idx);
  exp_desc.type_idx = type_idx;
  arg_info_list[info_idx].ed = exp_desc;
  IL_IDX(list1_idx) = idx;
  *prev_list_idx = list1_idx;

}
#endif

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Process function and subroutine calls, both generic and specific.     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      result_opnd - opnd pointing to call opr.                              *|
|*      is_function - call is a function call.                                *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      result_opnd - output tree (usually a tmp reference)                   *|
|*	res_exp_desc - expression descriptor that descibes the call.          *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if everything ok.                                                *|
|*                                                                            *|
\******************************************************************************/

boolean call_list_semantics(opnd_type     *result_opnd,
			    expr_arg_type *res_exp_desc,
                            boolean        is_function)

{

   int                 arg_attr;
   int		       alt_return_tmp;
   int                 arg_idx;
   int		       asg_idx;
   int                 attr_idx;
   int		       br_idx_idx;
   int                 col;
   expr_arg_type       exp_desc;
   boolean             found			= FALSE;
   int		       gen_idx;
   boolean	       has_symbolic		= FALSE;
   boolean	       host_associated		= FALSE;
   int                 i;
   int		       idx;
   int		       info_idx;
   int		       ir_idx;
   int                 label_cnt		= 0;
   int		       label_list_head		= NULL_IDX;
   int		       label_list_tail		= NULL_IDX;
   long_type           length;
   int                 line;
   int                 list_cnt;
   int                 list_idx;
   int                 list_idx2;
   int		       loc_idx;
   int		       name_idx;
   int		       new_sn_idx;
   int		       num_registers;
   boolean             locked_in;
   boolean             reset_expr_mode;
   int		       loc_info_idx;
#ifdef KEY /* Bug 10177 */
   int                 msg_num = 0;
#else /* KEY Bug 10177 */
   int                 msg_num;
#endif /* KEY Bug 10177 */
   int                 new_attr_idx;
   int                 num_args;
   boolean             ok			= TRUE;
   opnd_type           opnd;
   int		       opnd_column;
   int		       opnd_line;
   int		       rslt_idx;
#ifdef KEY /* Bug 10177 */
   opnd_type	       save_char_len = INIT_OPND_TYPE;
#else /* KEY Bug 10177 */
   opnd_type	       save_char_len;
#endif /* KEY Bug 10177 */
   int		       save_curr_stmt_sh_idx;
   boolean	       save_defer_stmt_expansion;
#ifdef KEY /* Bug 10177 */
   expr_mode_type      save_expr_mode = 0;
#else /* KEY Bug 10177 */
   expr_mode_type      save_expr_mode;
#endif /* KEY Bug 10177 */
   boolean	       save_foldable;
   boolean             save_in_call_list;
   boolean	       save_io_item_must_flatten;
   opnd_type	       save_result_opnd;
   int		       save_where_ir_idx;
   boolean	       save_will_fold_later;
   cif_usage_code_type save_xref_state;
#ifdef KEY /* Bug 10177 */
   int		       save_rank = 0;
#else /* KEY Bug 10177 */
   int		       save_rank;
#endif /* KEY Bug 10177 */
   opnd_type	       save_shape[7];
   boolean	       save_shape_known;
   int                 sn_idx;
   int                 spec_sn_idx		= NULL_IDX;
   int                 spec_count;
   int                 spec_idx;
   int		       type_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int		       false_list_idx 		= NULL_IDX;
# endif
#ifdef KEY /* Bug 572 */
   /* Keep track of whether the argument list contains a reference to a
    * constant pointer; if the call is intrinsic and implies that the pointer
    * should be dereferenced, we will issue an error message. */
   boolean saw_constant_ptr = FALSE;
#endif /* KEY Bug 572 */

   TRACE (Func_Entry, "call_list_semantics", NULL);

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;
   save_result_opnd = null_opnd;

   ir_idx = OPND_IDX((*result_opnd));

   /* do memory management stuff to make sure the tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }
   
   loc_info_idx = arg_info_list_base;

   attr_idx               = IR_IDX_L(ir_idx);
   locked_in              = AT_LOCKED_IN(attr_idx);
   AT_LOCKED_IN(attr_idx) = TRUE;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   IR_INLINE_STATE(ir_idx) = Not_Specified_Sgi;
   set_inline_state(ir_idx, attr_idx);
# endif

   if (AT_ATTR_LINK(attr_idx)) {
      host_associated = TRUE;
   }

   while (AT_ATTR_LINK(attr_idx)           &&
          ! AT_IGNORE_ATTR_LINK(attr_idx)) {

      attr_idx               = AT_ATTR_LINK(attr_idx);
      locked_in              = locked_in || AT_LOCKED_IN(attr_idx);
      AT_LOCKED_IN(attr_idx) = TRUE;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      set_inline_state(ir_idx, attr_idx);
# endif

   }

   if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
       ATP_PROC(attr_idx) == Dummy_Proc &&
       ATP_DUMMY_PROC_LINK(attr_idx) != NULL_IDX) {

      attr_idx = ATP_DUMMY_PROC_LINK(attr_idx);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      set_inline_state(ir_idx, attr_idx);
# endif

   }

   IR_IDX_L(ir_idx) = attr_idx;
   spec_idx         = attr_idx;
   gen_idx          = attr_idx;
   line             = IR_LINE_NUM_L(ir_idx);
   col              = IR_COL_NUM_L(ir_idx);

   if ((cif_flags & XREF_RECS) != 0  &&  xref_state != CIF_No_Usage_Rec) {
      cif_usage_rec(gen_idx, AT_Tbl_Idx, line, col, CIF_Symbol_Reference);
   }

#if 0
   /* Workaround for bug 563 (bugs.open64.net).  Note that other
    * compilers don't consider dummy procedures to be variables that
    * can be put into the shared or private lists.
    */
   if (cdir_switches.parallel_region       &&
       AT_OBJ_CLASS(gen_idx) == Pgm_Unit   &&
       ATP_PROC(gen_idx)     == Dummy_Proc &&
       ! cdir_switches.autoscope           && 
#ifdef KEY /* Bug 10441 - fine in a single dir and not in shared list*/
       ! cdir_switches.single              &&
#endif /* KEY Bug 10441 */
       ! ATP_TASK_SHARED(gen_idx))         {

      PRINTMSG(line, 1041, Error, col, AT_OBJ_NAME_PTR(gen_idx));
      ok = FALSE;
   }
#endif

   if (expr_mode == Restricted_Imp_Do_Expr) {
      PRINTMSG(line, 658, Error, col, AT_OBJ_NAME_PTR(gen_idx));
      ok = FALSE;
      goto DONE;
   }
       
   save_where_ir_idx = where_ir_idx;
   save_io_item_must_flatten = io_item_must_flatten;

   if (! AT_ELEMENTAL_INTRIN(gen_idx)) {
      where_ir_idx = NULL_IDX;
   }

   if (AT_OBJ_CLASS(gen_idx) == Pgm_Unit) {

      if (expr_mode == Initialization_Expr) {
         fnd_semantic_err(Obj_Use_Init_Expr,
                          line,
                          col,
                          gen_idx,
                          TRUE);
         ok = FALSE;
         goto EXIT;
      }

      if (expr_mode == Specification_Expr) {

         if (fnd_semantic_err(Obj_Use_Spec_Expr,
                              line,
                              col,
                              gen_idx,
                              TRUE)) {
            ok = FALSE;
            goto EXIT;
         }

         if (ATD_IM_A_DOPE(ATP_RSLT_IDX(gen_idx)) ||
             ATD_ARRAY_IDX(ATP_RSLT_IDX(gen_idx)) != NULL_IDX) {

            PRINTMSG(line, 240, Error, col, AT_OBJ_NAME_PTR(gen_idx));
            ok = FALSE;
         }
         else {

            /* Issue ANSI - Cray allows external function calls here. */
            /* F95 allows some of the functions here now.  Check      */

            if (
#ifdef KEY /* Bug 7726 */
	    /* Fortran 95 says every elemental procedure is pure */
	    !(ATP_PURE(gen_idx) || ATP_ELEMENTAL(gen_idx)) ||
#else /* KEY Bug 7726 */
	    !ATP_PURE(gen_idx) ||
#endif /* KEY Bug 7726 */
		(ATP_PROC(gen_idx) == Intern_Proc) ||
                ATP_RECURSIVE(gen_idx)) {
               PRINTMSG(line, 520, Ansi, col, AT_OBJ_NAME_PTR(gen_idx));
            }
         }

         if (ATP_PROC(gen_idx) == Dummy_Proc && AT_ALT_DARG(gen_idx)) {

            /* This darg is not at all entry points.  Add to a list for   */
            /* this specification expression.  This only happens if there */
            /* are alternate entry points and bounds expressions.         */

            list_idx = SCP_TMP_LIST(curr_scp_idx);

            while (list_idx != NULL_IDX && gen_idx != AL_ATTR_IDX(list_idx)) {
               list_idx = AL_NEXT_IDX(list_idx);
            }

            if (list_idx == NULL_IDX) {
               NTR_ATTR_LIST_TBL(list_idx);
               AL_NEXT_IDX(list_idx)		= SCP_TMP_LIST(curr_scp_idx);
               AL_ATTR_IDX(list_idx)		= gen_idx;
               SCP_TMP_LIST(curr_scp_idx)	= list_idx;
            }
         }
      }

      spec_count = 0;
      found      = TRUE;

      if (is_function) {

         if (ATP_PGM_UNIT(gen_idx) != Function &&
             ATP_PGM_UNIT(gen_idx) != Pgm_Unknown) {
            /* error .. should be function but isn't */
            switch (ATP_PGM_UNIT(gen_idx)) {
               case Subroutine :
                  msg_num = 452;
                  break;
               case Program :
                  msg_num = 453;
                  break;
               case Module :
                  msg_num = 455;
                  break;
               case Blockdata :
                  msg_num = 454;
                  break;
            }
            PRINTMSG(line, msg_num, Error, col, 
                     AT_OBJ_NAME_PTR(gen_idx));
            ok = FALSE;
            goto EXIT;
         }
         else if (ATP_PGM_UNIT(gen_idx) == Pgm_Unknown) {

            if (ATP_SCP_IDX(gen_idx) == NULL_IDX &&
                ATP_RSLT_IDX(gen_idx) == NULL_IDX) {
               PRINTMSG(line, 970, Internal, col);
            }

            ATP_PGM_UNIT(gen_idx) = Function;

            if (ATP_RSLT_IDX(gen_idx) == NULL_IDX) {

               CREATE_FUNC_RSLT(gen_idx, new_attr_idx);

               AT_REFERENCED(new_attr_idx) = Referenced;

               SET_IMPL_TYPE_IN_SCP(new_attr_idx, ATP_SCP_IDX(gen_idx));
            }
         }
      }
      else {
         if (ATP_PGM_UNIT(gen_idx) != Subroutine &&
             ATP_PGM_UNIT(gen_idx) != Pgm_Unknown) {
            /* error .. should be Subroutine but isn't */
            switch (ATP_PGM_UNIT(gen_idx)) {
               case Function :
                  msg_num = 335;
                  break;
               case Program :
                  msg_num = 456;
                  break; 
               case Module :
                  msg_num = 458;
                  break;
               case Blockdata :
                  msg_num = 457;
                  break;
            }
            PRINTMSG(line, msg_num, Error, col,
                     AT_OBJ_NAME_PTR(gen_idx));
            ok = FALSE;
            goto EXIT;
         }
         else if (ATP_PGM_UNIT(gen_idx) == Pgm_Unknown) {
            ATP_PGM_UNIT(gen_idx) = Subroutine;
         }
      }
   }
   else if (AT_OBJ_CLASS(gen_idx) == Interface) {
      spec_count = ATI_NUM_SPECIFICS(gen_idx);

      if (spec_count == 0) {
         goto EXIT;
      }

      if (is_function) {

         if (ATI_INTERFACE_CLASS(gen_idx) != Generic_Unknown_Interface &&
             ATI_INTERFACE_CLASS(gen_idx) != Generic_Function_Interface) {

            PRINTMSG(line, 398, Error, col, AT_OBJ_NAME_PTR(gen_idx));
            ok    = FALSE;
            found = TRUE;
            goto EXIT;
         }
      }
      else {

         if (ATI_INTERFACE_CLASS(gen_idx) != Generic_Unknown_Interface &&
             ATI_INTERFACE_CLASS(gen_idx) != Generic_Subroutine_Interface) {

            PRINTMSG(line, 397, Error, col, AT_OBJ_NAME_PTR(gen_idx));
            ok    = FALSE;
            found = TRUE;
            goto EXIT;
         }
      }
   }
   else {

      spec_count = 0;
      found      = TRUE;

      AT_REFERENCED(gen_idx) = Not_Referenced;

      if (fnd_semantic_err((is_function ? Obj_Use_Extern_Func :
                                          Obj_Use_Extern_Subr), 
                           line, col, gen_idx, TRUE)) {
         ok = FALSE;
         goto EXIT;
      }
      else if (is_function && AT_OBJ_CLASS(gen_idx) == Pgm_Unit &&
               ATP_PROC(gen_idx) != Dummy_Proc &&
               ATP_RSLT_IDX(gen_idx) != NULL_IDX &&
               TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(gen_idx))) == Character &&
               TYP_CHAR_CLASS(ATD_TYPE_IDX(ATP_RSLT_IDX(gen_idx))) ==
                                                            Assumed_Size_Char) {
         PRINTMSG(line, 939, Error, col, AT_OBJ_NAME_PTR(gen_idx));
         ok = FALSE;
         goto EXIT;
      }

      AT_REFERENCED(gen_idx)  = Referenced;

      if (locked_in) {
         /* error .. can't have this call */

         if (is_function) {
            PRINTMSG(line, 511, Error, col, AT_OBJ_NAME_PTR(gen_idx));
         }
         else {
            PRINTMSG(line, 568, Error, col, AT_OBJ_NAME_PTR(gen_idx));
         }
         ok = FALSE;
         goto EXIT;
      }

      if (AT_USE_ASSOCIATED(gen_idx)) {
         PRINTMSG(line, 898, Error, col, AT_OBJ_NAME_PTR(gen_idx));
         ok = FALSE;
         goto EXIT;
      }
      else {
         PRINTMSG(line, 971, Internal, col);
      }
   }

   if (cdir_switches.no_internal_calls &&
       AT_OBJ_CLASS(gen_idx) == Pgm_Unit &&
       ATP_PROC(gen_idx) == Intern_Proc) {
      PRINTMSG(line, 821, Error, col);
   }

   /* process all the arguments thru expr_semantics */
   /* hang on to type etc. Catch errors.            */

   list_idx = IR_IDX_R(ir_idx);

   save_in_call_list = in_call_list;

   reset_expr_mode = FALSE;

   /* so we don't issue errors that should not be issued */

   if ((strcmp(AT_OBJ_NAME_PTR(gen_idx), "LBOUND") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "UBOUND") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "SIZE") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "SHAPE") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "KIND") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "BIT_SIZE") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "DIGITS") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "PRECISION") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "EPSILON") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "HUGE") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "MAXEXPONENT") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "MINEXPONENT") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "RADIX") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "RANGE") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "LEN") == 0) ||
       (strcmp(AT_OBJ_NAME_PTR(gen_idx), "TINY") == 0)) {
      save_expr_mode = expr_mode;
      if (expr_mode == Initialization_Expr ||
          expr_mode == Specification_Expr) {
         need_pure_function = TRUE;
      }
      expr_mode = Regular_Expr;
      reset_expr_mode = TRUE;
   }

   for (i = loc_info_idx + 1; 
        i <= loc_info_idx + IR_LIST_CNT_R(ir_idx); 
        i++) {

      arg_info_list[i] = init_arg_info;

      if (IL_FLD(list_idx) == IR_Tbl_Idx &&
          IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {
         /* error for call list */
         PRINTMSG(IR_LINE_NUM(IL_IDX(list_idx)), 197, Error,
                  IR_COL_NUM(IL_IDX(list_idx)),", or )", ":");
         ok       = FALSE;
         goto EXIT;
      }

      if (IL_FLD(list_idx) == IR_Tbl_Idx &&
          IR_OPR(IL_IDX(list_idx)) == Kwd_Opr) {

         if (spec_count == 0 &&
             ! ATP_EXPL_ITRFC(gen_idx)) {
            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &opnd_line,
                                      &opnd_column);
            PRINTMSG(opnd_line, 333, Error, opnd_column);
            ok = FALSE;
         }
         arg_info_list[i].kwd = IR_IDX_L(IL_IDX(list_idx));
         COPY_OPND(IL_OPND(list_idx),IR_OPND_R(IL_IDX(list_idx)));
      }
      else {
         arg_info_list[i].kwd = NULL_IDX;
      }

      COPY_OPND(opnd, IL_OPND(list_idx));

# ifdef _F_MINUS_MINUS
      if (cmd_line_flags.co_array_fortran) {
         ok &= check_arg_for_co_array(&opnd);
      }
# endif

      in_call_list = TRUE;
      exp_desc.rank = 0;
      save_xref_state = xref_state;

      label_allowed = TRUE;

      if (xref_state != CIF_No_Usage_Rec) {
         xref_state = CIF_Symbol_Reference;
      }

#ifdef KEY /* Bug 572 */
      /* Whether a pointer in an actual argument list is dereferenced depends
       * on whether the dummy argument has the "pointer" attribute, and we
       * can't know that till we identify which procedure the argument list
       * matches, so for now we must be lenient and assume that a constant
       * pointer whose value is null will not be dereferenced. Once we know
       * which function we're calling, we will issue an error message if
       * appropriate. */
      int save_constant_ptr_ok = constant_ptr_ok;
      constant_ptr_ok = TRUE;
#endif /* KEY Bug 572 */
      ok = expr_semantics(&opnd, &exp_desc) && ok;
#ifdef KEY /* Bug 572 */
      constant_ptr_ok = save_constant_ptr_ok;
#endif /* KEY Bug 572 */

# ifdef KEY
      {
      int o_idx = OPND_IDX(opnd); // 3507
      if (OPND_FLD(opnd) == AT_Tbl_Idx && 
	  // 3507: Original change erroneously called ATD_F2C_ABI_VAR on
	  // certain operands (e.g. label) for which it's not defined
          (AT_OBJ_CLASS(o_idx) == Data_Obj ||
	    AT_OBJ_CLASS(o_idx) == Pgm_Unit) &&
          ATD_F2C_ABI_VAR(OPND_IDX(opnd)) == TRUE &&
          TYP_LINEAR(ATD_TYPE_IDX(OPND_IDX(opnd))) != exp_desc.linear_type) {
          exp_desc.type_idx	= ATD_TYPE_IDX(OPND_IDX(opnd));
          exp_desc.linear_type	= TYP_LINEAR(ATD_TYPE_IDX(OPND_IDX(opnd)));
      }
      }
# endif

      label_allowed = FALSE;

      has_symbolic |= exp_desc.has_symbolic;

      find_opnd_line_and_column(&opnd, &opnd_line, &opnd_column);
      arg_info_list[i].line           = opnd_line;
      arg_info_list[i].col            = opnd_column;

#ifdef KEY
//Bug 242
      if (OPND_FLD(IL_OPND(list_idx)) == IR_Tbl_Idx && OPND_FLD(opnd) == AT_Tbl_Idx && 
          (IR_OPR(IL_IDX(list_idx)) == Mult_Opr || 
           IR_OPR(IL_IDX(list_idx)) == Div_Opr  ||
           IR_OPR(IL_IDX(list_idx)) == Uplus_Opr ||
           IR_OPR(IL_IDX(list_idx)) == Uminus_Opr ||
           IR_OPR(IL_IDX(list_idx)) == Plus_Opr ||
           IR_OPR(IL_IDX(list_idx)) == Minus_Opr ))
        ;
      else
#endif
        COPY_OPND(IL_OPND(list_idx), opnd);

      xref_state = save_xref_state;
      
      arg_info_list[i].ed             = exp_desc;
      arg_info_list[i].maybe_modified = TRUE;

      IL_ARG_DESC_VARIANT(list_idx)   = TRUE;
      IL_ARG_DESC_IDX(list_idx)       = i;

      if (AT_IS_INTRIN(gen_idx)) {  /* All args must be integer */

         switch (expr_mode) {
         case Regular_Expr:
         case Stmt_Func_Expr:
            break;

         case Initialization_Expr:
            if (exp_desc.type != Integer &&
                exp_desc.type != Character &&
                AT_ELEMENTAL_INTRIN(gen_idx)) {

               if (strcmp(AT_OBJ_NAME_PTR(gen_idx), "NINT") == 0 ||
                   strcmp(AT_OBJ_NAME_PTR(gen_idx), "INT") == 0) {
                  PRINTMSG(arg_info_list[i].line,
                           274,
                           Ansi,
                           arg_info_list[i].col);
               }
# if defined(_USE_FOLD_DOT_f)
               else if (strcmp(AT_OBJ_NAME_PTR(gen_idx), "SQRT") == 0) {
                  /* intentionally blank - will fold certain other intrinsics */
               }
# endif
               else {
                  PRINTMSG(arg_info_list[i].line,
                           1456,
                           Error,
                           arg_info_list[i].col);
                  ok = FALSE;
               }
            }
            break;

         case Specification_Expr:
         default:
            if (exp_desc.type != Integer &&  exp_desc.type != Character) {
               PRINTMSG(arg_info_list[i].line, 274, Ansi, arg_info_list[i].col);
            }
            break;
         }  /* End switch */
      }

      if (IL_FLD(list_idx)             == AT_Tbl_Idx &&
          AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit)  {

         /* check for external requirement */

         if (ATP_DCL_EXTERNAL(IL_IDX(list_idx)) ||
             ATP_IN_INTERFACE_BLK(IL_IDX(list_idx)) ||
             AT_IS_INTRIN(IL_IDX(list_idx)) ||
             ATP_SCP_ALIVE(IL_IDX(list_idx)) ||
             ATP_PROC(IL_IDX(list_idx)) == Module_Proc) {

            arg_info_list[i].pgm_unit = TRUE;

            if (ATP_PGM_UNIT(IL_IDX(list_idx)) == Pgm_Unknown) {
               type_idx = IM_TYPE_IDX(ATP_SCP_IDX(IL_IDX(list_idx)), 
                                      IMPL_IDX(AT_OBJ_NAME(IL_IDX(list_idx))));

               /* set up what the implicit type would be if function */

               arg_info_list[i].ed.type		= TYP_TYPE(type_idx); 
               arg_info_list[i].ed.type_idx	= type_idx;
               arg_info_list[i].ed.linear_type	= TYP_LINEAR(type_idx);
               arg_info_list[i].ed.rank		= 0;
            }
         }
         else {

            /* error .. procedure must be EXTERNAL, in an interface block */
            /* or be a module procedure to be used as an actual argument. */

            PRINTMSG(IL_LINE_NUM(list_idx), 379, Error, 
                     IL_COL_NUM(list_idx),
                     AT_OBJ_NAME_PTR(IL_IDX(list_idx)));
            ok = FALSE;
            goto EXIT;
         }
      }
      else {
         arg_info_list[i].pgm_unit = FALSE;
      }

      if (exp_desc.label) {
 
         label_cnt++;

         if (label_list_head == NULL_IDX) {
            NTR_IR_LIST_TBL(label_list_head);
            label_list_tail = label_list_head;
         }
         else {
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(label_list_tail));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(label_list_tail)) = 
                                                            label_list_tail;
            label_list_tail = IL_NEXT_LIST_IDX(label_list_tail);
         }

         COPY_OPND(IL_OPND(label_list_tail), IL_OPND(list_idx));

         if (spec_count == 0             &&
             ! ATP_EXPL_ITRFC(gen_idx)) {

            ATP_HAS_ALT_RETURN(gen_idx) = TRUE;
         }
      }

#ifdef KEY /* Bug 572 */
      if (arg_info_list[i].ed.pointer && arg_info_list[i].ed.constant) {
	 saw_constant_ptr = TRUE;
      }
#endif /* KEY Bug 572 */
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   need_pure_function = FALSE;

   if (reset_expr_mode) {
      expr_mode = save_expr_mode;
   }

#ifdef KEY

   if (IR_OPR(ir_idx) == Call_Opr &&
       strcmp(AT_OBJ_NAME_PTR(IR_OPND_L(ir_idx).idx),"FLUSH")==0 ){
     int list_idx = IR_IDX_R(ir_idx);
     if (IL_FLD(list_idx) == CN_Tbl_Idx){
       int op_idx = IL_IDX(list_idx);
       if ( TYP_LINEAR(CN_TYPE_IDX(op_idx)) == Integer_4 )
         IL_IDX(list_idx) = C_INT_TO_CN(double_linear_type[0], CN_CONST(op_idx));
     }
   }

   if (IR_OPR(ir_idx) == Call_Opr &&
       (strcmp(AT_OBJ_NAME_PTR(IR_OPND_L(ir_idx).idx),"MAX")==0 ||
        strcmp(AT_OBJ_NAME_PTR(IR_OPND_L(ir_idx).idx),"MIN")==0 )){

     int list_idx = IR_IDX_R(ir_idx);
     int info_idx = IL_ARG_DESC_IDX(list_idx);
     int type_idx = arg_info_list[info_idx].ed.type;
     boolean conversion = FALSE;
     list_idx = IL_NEXT_LIST_IDX(list_idx);
     while (list_idx != NULL_IDX) {
       if (arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type != type_idx){
         conversion = TRUE;
         break;
       }
       list_idx = IL_NEXT_LIST_IDX(list_idx);
     }

     list_idx = IR_IDX_R(ir_idx);
     int old_idx = NULL_IDX;
     while (conversion == TRUE && list_idx != NULL_IDX) {
       int tmp_idx = IL_NEXT_LIST_IDX(list_idx);
       info_idx = IL_ARG_DESC_IDX(list_idx);
       if (arg_info_list[info_idx].ed.type == Integer)
         Type_Converstion_to_Real(ir_idx, list_idx, &old_idx);
       else
         old_idx = list_idx;
       list_idx = tmp_idx;
     }
   }
#endif

   /* the io_item_must_flatten flag should not be passed back for*/
   /* actual arguments. Restore the flag to it's previous value. */
   /* oh, unless this is an intrinsic.                           */

   if (! AT_IS_INTRIN(gen_idx)) {
      io_item_must_flatten = save_io_item_must_flatten;
   }

   in_call_list = save_in_call_list;

   if (!ok) {
      goto EXIT;
   }


   if (spec_count > 0 || ATP_EXPL_ITRFC(gen_idx)) {

      do {

         if (spec_count > 0) {
      
            if (spec_sn_idx == NULL_IDX) {
               spec_sn_idx = ATI_FIRST_SPECIFIC_IDX(gen_idx);
            }
            else {
               spec_sn_idx = SN_SIBLING_LINK(spec_sn_idx);
            }
            spec_idx = SN_ATTR_IDX(spec_sn_idx);
         }
         else {
            spec_idx = gen_idx;
         }

         num_args = ATP_NUM_DARGS(spec_idx);

         if (ATP_EXTRA_DARG(spec_idx)) {
            num_args--;
         }

# ifdef _DEBUG
         if (num_args >= arg_list_size) {
            PRINTMSG(1,245,Internal,0);
         }

# endif
         for (arg_idx = 0; arg_idx <= num_args + 1; arg_idx++) {
            arg_list[arg_idx] = 0;
         }

         list_idx = IR_IDX_R(ir_idx);
      
         info_idx = loc_info_idx;

         for (list_cnt = 1; list_cnt <= IR_LIST_CNT_R(ir_idx); list_cnt++) {
            info_idx++;
            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &opnd_line,
                                      &opnd_column);

            if (arg_info_list[info_idx].kwd != NULL_IDX) {
               length = (long_type) CN_INT_TO_C(
                           TYP_IDX(CN_TYPE_IDX(arg_info_list[info_idx].kwd)));
               arg_attr = srch_kwd_name(
                                (char *)&CN_CONST(arg_info_list[info_idx].kwd),
                                         length,
                                         spec_idx,
                                        &sn_idx);
           
               if (arg_attr) {

                  if (ATP_EXTRA_DARG(spec_idx)) {
                     arg_idx = sn_idx - ATP_FIRST_IDX(spec_idx);
                  }
                  else {
                     arg_idx = sn_idx - ATP_FIRST_IDX(spec_idx) + 1;
                  }
   
                  if (arg_list[arg_idx]) {

                     if (spec_count == 0) {

                        /* error .. already have this argument */

                        PRINTMSG(opnd_line, 330, Error, opnd_column);
                        ok = FALSE;
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                        continue;
                     }
                     else {
                        goto CYCLE;
                     }
                  }
               }
               else {

                  if (spec_count == 0) {

                     /* error .. not an argument to this interface */

                     PRINTMSG(opnd_line, 277, Error, opnd_column,
                              (char *)&CN_CONST(arg_info_list[info_idx].kwd),
                              AT_OBJ_NAME_PTR(spec_idx));
                     ok = FALSE;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                     continue;  
                  }
                  else {
                     goto CYCLE;
                  }
               }
            }
            else { /* put in same place */

               if (ATP_EXTRA_DARG(spec_idx)) {
                  arg_attr = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + list_cnt);
               }
               else {
                  arg_attr = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + list_cnt-1);
               }
               arg_idx           = list_cnt;

   
               if (arg_list[arg_idx]) {
         
                  if (spec_count == 0) {

                     /* error .. already have this argument */

                     PRINTMSG(opnd_line, 330, Error, opnd_column);
                     ok = FALSE;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                     continue;
                  }
                  else {
                     goto CYCLE;
                  }
               }
               else if (arg_idx > num_args) {

                  if (ATP_PROC(spec_idx) == Intrin_Proc) {

                     if ((ATP_INTRIN_ENUM(spec_idx) == Ranf_Intrinsic) ||
                         (ATP_INTRIN_ENUM(spec_idx) == Date_Intrinsic) ||
#ifdef KEY
                         (ATP_INTRIN_ENUM(spec_idx) == Fdate_Intrinsic) ||
#endif
                         (ATP_INTRIN_ENUM(spec_idx) == Jdate_Intrinsic) ||
                         (ATP_INTRIN_ENUM(spec_idx) == Rtc_Intrinsic) ||
                         (ATP_INTRIN_ENUM(spec_idx) == Irtc_Intrinsic) ||
                         (ATP_INTRIN_ENUM(spec_idx) == Clock_Intrinsic) ||
#ifdef KEY
                         (ATP_INTRIN_ENUM(spec_idx) == Numarg_Intrinsic) ||
                         (ATP_INTRIN_ENUM(spec_idx) == Time4_Intrinsic) ||
                         (ATP_INTRIN_ENUM(spec_idx) == Time8_Intrinsic)) {
#else
                         (ATP_INTRIN_ENUM(spec_idx) == Numarg_Intrinsic)) {
#endif
                        PRINTMSG(opnd_line, 739, Warning, opnd_column,
                                 AT_OBJ_NAME_PTR(gen_idx));
                        break;  /* arguments are not allowed */
                     }
                  }

                  if (spec_count == 0) {
                     PRINTMSG(opnd_line, 331, Error, opnd_column,
                              list_cnt);
                     ok = FALSE;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                     continue;
                  }
                  else {
                     goto CYCLE;
                  }
               }
            }


            /* if everything was alright, (otherwise I wouldn't be here) */
            /* put argument in proper location and hang on to info idx   */

            arg_list[arg_idx] = list_idx;
   
            /* check for label/alternate return match */

            if (AT_COMPILER_GEND(arg_attr) && AT_IS_DARG(arg_attr)) {

               /* Set flag on the attr of the subroutine being called */

               ATP_HAS_ALT_RETURN(spec_idx) = TRUE;

               if (IL_FLD(list_idx) != AT_Tbl_Idx             ||
                   AT_OBJ_CLASS(IL_IDX(list_idx)) != Label) {
   
                  if (spec_count == 0) { /* error .. expected a label */
                     PRINTMSG(opnd_line, 338, Error, opnd_column);
                     ok = FALSE;
                  }
                  else {
                     goto CYCLE;
                  }
               }
   
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               continue;
            }
            else if (IL_FLD(list_idx) == AT_Tbl_Idx           &&
                     AT_OBJ_CLASS(IL_IDX(list_idx)) == Label) {

               if (spec_count == 0) { /* error .. actual is label */
                  PRINTMSG(opnd_line, 504, Error, opnd_column,
                           AT_OBJ_NAME_PTR(IL_IDX(list_idx)),
                           AT_OBJ_NAME_PTR(arg_attr));
                  ok = FALSE;
               }
               else {
                  goto CYCLE;
               }
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               continue;
            }

            /* check type and rank */

            if (!compare_darg_to_actual_arg(gen_idx, 
                                            spec_idx,
                                            arg_attr,
                                            IL_OPND(list_idx),
                                            info_idx,
                                            spec_count)) {

               if (spec_count == 0) {  /* Comparing darg and actual arg */
                  ok = FALSE;          /* An error was issued.          */
               }
               else {                  /* Looking for a specific.       */
                  goto CYCLE;          /* This one does not match.      */
               }
            }
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
   
         for (arg_idx = 1; arg_idx <= num_args; arg_idx++) {

            if (arg_list[arg_idx] == NULL_IDX) {

               /* check to see if it's optional */

               if (ATP_EXTRA_DARG(spec_idx)) {
                  arg_attr = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + arg_idx);
               }
               else {
                  arg_attr = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + arg_idx - 1);
               }

               if (!AT_OPTIONAL(arg_attr)) {
            
                  if (spec_count == 0) {

                     /* error .. no argument for this dummy arg */

                     if (AT_COMPILER_GEND(arg_attr)) {
                        PRINTMSG(line, 209, Error, col, arg_idx);
                     }
                     else {
                        PRINTMSG(line, 332, Error, col, 
                                 AT_OBJ_NAME_PTR(arg_attr));
                     }
                     ok = FALSE;
                     goto EXIT;
                  }
                  else {
                     goto CYCLE;
                  }
               }
            }
         }

         /* if I'm here, then I've found the right one */

   
         for (arg_idx = 1; arg_idx <= num_args; arg_idx++) {
   
            if (arg_list[arg_idx] == NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);
               IL_ARG_DESC_VARIANT(list_idx) = TRUE;
               arg_list[arg_idx] = list_idx;
            }

            if (arg_idx > 1){
               IL_NEXT_LIST_IDX(arg_list[arg_idx - 1]) = arg_list[arg_idx];
            }
         }

         IL_NEXT_LIST_IDX(arg_list[num_args]) = NULL_IDX;

         IR_LIST_CNT_R(ir_idx) = num_args;
         IR_IDX_R(ir_idx)      = arg_list[1];

         /* change call attr to right one */

         IR_IDX_L(ir_idx)      = spec_idx;

         found                 = TRUE;
         goto EXIT;

CYCLE:

         spec_count--;
      }
      while (spec_count > 0);

   }
   else {
      /* don't have explicit interface */
      spec_idx = gen_idx;
   }

EXIT:

   if (found) {

      if (ok) {

         /* Keep the CIF Call Site record first.			      */
         /* See the explanation in s_utils.c at the call to cif_call_site_rec */
         /* for the reasoning behind checking xref_state.		      */

         if ((cif_flags & MISC_RECS) != 0  &&  xref_state != CIF_No_Usage_Rec) {
            cif_call_site_rec(ir_idx, gen_idx);
         }

         if (AT_OBJ_CLASS(spec_idx)  == Pgm_Unit   &&
             ATP_SCP_ALIVE(spec_idx))              {

            if (ATP_PGM_UNIT(spec_idx)  == Function && 
                !ATP_RSLT_NAME(spec_idx)) {
               PRINTMSG(line, 344, Ansi, col);
            }

            if (!ATP_RECURSIVE(spec_idx) && !AT_DCL_ERR(spec_idx) &&
                !on_off_flags.recursive) {
               PRINTMSG(line, 343, Error, col);
               ok = FALSE;
            }
         }

         if (AT_NOT_VISIBLE(gen_idx)) {
            PRINTMSG(line, 486, Error, col,
                     AT_OBJ_NAME_PTR(gen_idx),
                     AT_OBJ_NAME_PTR(AT_MODULE_IDX((gen_idx))));
            ok = FALSE;
            goto DONE;
         }
         else if (AT_NOT_VISIBLE(spec_idx)) {
            PRINTMSG(line, 486, Error, col,
                     AT_OBJ_NAME_PTR(spec_idx),
                     AT_OBJ_NAME_PTR(AT_MODULE_IDX((spec_idx))));
            ok = FALSE;
            goto DONE;
         }

         if (AT_DCL_ERR(spec_idx)) {
            /* don't do any further processing on this bad boy */
   
            ok = FALSE;
            goto DONE;
         }

         if (ok && found &&
#ifdef KEY /* Bug 7726 */
	     /* Fortran 95 says every elemental procedure is pure */
	     (ATP_PURE(spec_idx) || ATP_ELEMENTAL(spec_idx)) &&
#else /* KEY Bug 7726 */
	     ATP_PURE(spec_idx) &&
#endif /* KEY Bug 7726 */
             ATP_PROC(spec_idx) != Intrin_Proc) {

            /* Check to make sure all actual args that are procedures are PURE*/

            list_idx	= IR_IDX_R(ir_idx);

            if (ATP_EXTRA_DARG(spec_idx)) {
               arg_idx	= ATP_FIRST_IDX(spec_idx) + 1;
               num_args	= ATP_NUM_DARGS(spec_idx) - 1;
            }
            else {
               arg_idx	= ATP_FIRST_IDX(spec_idx);
               num_args	= ATP_NUM_DARGS(spec_idx);
            }

            for (;num_args > 0; num_args--) {
      
               if (OPND_FLD(IL_OPND(list_idx)) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(OPND_IDX(IL_OPND(list_idx))) == Pgm_Unit &&
#ifdef KEY /* Bug 7726 */
                   /* Fortran 95 says every elemental procedure is pure (and
		    * the original code only accidentally worked because
		    * Pgm_Unit was 1) */
                   !(ATP_PURE(OPND_IDX(IL_OPND(list_idx))) || ATP_ELEMENTAL(OPND_IDX(IL_OPND(list_idx))))
#else /* KEY Bug 7726 */
                   !ATP_PURE(OPND_IDX(IL_OPND(list_idx))) == Pgm_Unit
#endif /* KEY Bug 7726 */
	       ) {
                  PRINTMSG(arg_info_list[IL_ARG_DESC_IDX(list_idx)].line, 
                           1642, Error,
                           arg_info_list[IL_ARG_DESC_IDX(list_idx)].col,
                           AT_OBJ_NAME_PTR(SN_ATTR_IDX(arg_idx)),
                           AT_OBJ_NAME_PTR(OPND_IDX(IL_OPND(list_idx))));
                  break;
               }
               arg_idx++;
               list_idx	= IL_NEXT_LIST_IDX(list_idx);
            }
         }


         if (ATP_PROC(spec_idx) == Intrin_Proc) {

            if (ATP_INTRIN_ENUM(spec_idx) != Unknown_Intrinsic) {
               ATP_INTERFACE_IDX(spec_idx) = gen_idx;

#ifdef KEY /* Bug 572 */
	       /* If an intrinsic implies that a constant pointer (which is
		* always null) in its arglist should be dereferenced, we need
		* to issue an error. There's no good machinery for deciding
		* this, so test for the two intrinsics that don't dereference
		* their pointer args. */
	       intrinsic_type intype = ATP_INTRIN_ENUM(spec_idx);
	       if (saw_constant_ptr && intype != Associated_Intrinsic &&
		  intype != Null_Intrinsic) {
		  PRINTMSG(opnd_line, 1677, Error, opnd_column);
	       }
#endif /* KEY Bug 572 */
               (*(void (*)())intrinsic_semantics[ATP_INTRIN_ENUM(spec_idx)]) 
                                                 (result_opnd, 
                                                  res_exp_desc,
                                                  &spec_idx);
               if (ATP_EXTERNAL_INTRIN(spec_idx)) {
                  goto CONTINUE;
               }
               else {

                  if (res_exp_desc->rank &&
                      OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
                      IR_OPR(OPND_IDX((*result_opnd))) != Subscript_Opr && 
                      IR_OPR(OPND_IDX((*result_opnd))) != Whole_Subscript_Opr&& 
                      IR_OPR(OPND_IDX((*result_opnd)))!=Section_Subscript_Opr&& 
                      IR_OPR(OPND_IDX((*result_opnd))) != Substring_Opr && 
                      IR_OPR(OPND_IDX((*result_opnd))) != Whole_Substring_Opr&& 
                      IR_OPR(OPND_IDX((*result_opnd))) != Struct_Opr) {
                     IR_ARRAY_SYNTAX(OPND_IDX((*result_opnd))) = TRUE;
                     io_item_must_flatten = TRUE;
                  }
                  else if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx) {
                     IR_ARRAY_SYNTAX(OPND_IDX((*result_opnd))) = FALSE;
                  }

                  /* Now that the types for the function result, etc. have    */
                  /* been resolved, if the procedure is a function, the       */
                  /* Object record that represents the function result can    */
                  /* now be output.			                      */
                  /* An intrinsic procedure that is NOT specified as the      */
                  /* generic name on an interface block is "special" and is   */
                  /* the only case where cif_call_site_rec outputs the Entry  */
                  /* record immediately.  In all other cases the CIF symbol id*/
                  /* is just set and the Entry Point and Object (for result)  */
                  /* records are output with all other records as we go       */
                  /* through the Attrs in cif_send_sytb.	              */
                  /* Found with SPR 94823,  The reason such an intrinsic is   */
                  /* "special" is that some intrinsic functions have result   */
                  /* types that vary from invocation to invocation.  Since    */
                  /* there is only one result Attr entry for all invocations, */
                  /* the Object record must be output right now to get the    */
                  /* result type correct.  Tools such as cflist are especially*/
                  /* anxious to have the result type for each invocation.     */
                  /* All other functions (particularly user functions) can,   */
                  /* of course, only have one result type so these result     */
                  /* Object records can be output later as a part of	      */
                  /* processing the Entry Point definition record for the     */
                  /* function.						      */

                  if (is_function &&
                      (cif_flags & MISC_RECS) != 0       &&
                      xref_state != CIF_No_Usage_Rec &&
                      ! ATI_USER_SPECIFIED(gen_idx)) {
                     cif_object_rec_for_func_result(spec_idx);
                     ATI_CIF_SEEN_IN_CALL(gen_idx) = TRUE;
                  }

                  goto DONE;
               }
            }
            else {

CONTINUE:

               /* Now that the types for the function result, etc. have been  */
               /* resolved, if the procedure is a function, the Object        */
               /* record that represents the function result can now be       */
               /* output.				                      */

               if (is_function                        &&
                   ! ATI_USER_SPECIFIED(gen_idx)      &&
                   (cif_flags & MISC_RECS) != 0       &&
                   xref_state != CIF_No_Usage_Rec) {
                  cif_object_rec_for_func_result(spec_idx);
               }

               /* put the specific attr in the local scope */

               NTR_ATTR_TBL(new_attr_idx);
               COPY_ATTR_NTRY(new_attr_idx, spec_idx);
               AT_CIF_SYMBOL_ID(new_attr_idx)	= 0;
               ADD_ATTR_TO_LOCAL_LIST(new_attr_idx);
               AT_REFERENCED(new_attr_idx)	= Referenced;
               AT_DEF_IN_CHILD(new_attr_idx)	= FALSE;
               AT_REF_IN_CHILD(new_attr_idx)	= FALSE;
               AT_HOST_ASSOCIATED(new_attr_idx)	= FALSE;
               ATP_INTERFACE_IDX(new_attr_idx)	= gen_idx;
               spec_idx				= new_attr_idx;

               if (ATP_PGM_UNIT(spec_idx) == Function) {

                  /* Create the result attr.  It's in the local scope  */
                  /* thru the function name.                           */

                  NTR_ATTR_TBL(new_attr_idx);
                  COPY_ATTR_NTRY(new_attr_idx, ATP_RSLT_IDX(spec_idx));
                  AT_CIF_SYMBOL_ID(new_attr_idx)	= 0;
                  AT_REFERENCED(new_attr_idx)		= Referenced;
                  AT_COMPILER_GEND(new_attr_idx)	= TRUE;
                  AT_DEF_IN_CHILD(new_attr_idx)		= FALSE;
                  AT_REF_IN_CHILD(new_attr_idx)		= FALSE;
                  AT_HOST_ASSOCIATED(new_attr_idx)	= FALSE;
                  ATD_STOR_BLK_IDX(new_attr_idx)	= SCP_SB_STACK_IDX(
                                                                  curr_scp_idx);
                  ATP_RSLT_IDX(spec_idx)		= new_attr_idx;
                  ATD_FUNC_IDX(new_attr_idx)		= spec_idx;
               }

               IR_IDX_L(ir_idx)	= spec_idx;
               rslt_idx		= ATP_RSLT_IDX(spec_idx);

               if (rslt_idx != NULL_IDX &&
#ifdef KEY /* Bug 5089 */
                   FUNCTION_MUST_BE_SUBROUTINE(spec_idx, rslt_idx)
#else /* KEY Bug 5089 */
                   FUNCTION_MUST_BE_SUBROUTINE(rslt_idx)
#endif /* KEY Bug 5089 */
		   ) {

                  ATD_STOR_BLK_IDX(rslt_idx) = SCP_SB_DARG_IDX(curr_scp_idx);

                  /* Insert the function result as the zero'th darg */

                  if (!ATP_EXTRA_DARG(spec_idx)) {
                     ATP_EXTRA_DARG(spec_idx)	= TRUE;

                     sn_idx = ATP_FIRST_IDX(spec_idx);

                     NTR_SN_TBL(new_sn_idx);
                     ATP_FIRST_IDX(spec_idx) = new_sn_idx;
                     SN_NAME_LEN(new_sn_idx) = AT_NAME_LEN(rslt_idx);
                     SN_NAME_IDX(new_sn_idx) = AT_NAME_IDX(rslt_idx);
                     SN_ATTR_IDX(new_sn_idx) = rslt_idx;

                     for (i = 0; i < ATP_NUM_DARGS(spec_idx); i++) {
                        NTR_SN_TBL(new_sn_idx);
                        COPY_TBL_NTRY(sec_name_tbl, new_sn_idx, sn_idx);
                        sn_idx++;
                     }

                     ATP_NUM_DARGS(spec_idx)   += 1;
                  }
               }
            }
         }
         else if (spec_idx != gen_idx) {

            if (ATP_SCP_IDX(spec_idx) != curr_scp_idx || 
                AT_NOT_VISIBLE(spec_idx)) {

               /* Not visible is checked, because a not visible procedure */
               /* may be referenced via its interface name, even though   */
               /* it cannot be referenced via its own name.               */

               new_attr_idx = srch_sym_tbl(AT_OBJ_NAME_PTR(spec_idx),
                                           AT_NAME_LEN(spec_idx),
                                           &name_idx);

               if (new_attr_idx != spec_idx) {

                  /* This attr is not in this scope.  It is either host    */
                  /* associated here, via the interface block, or it is    */
                  /* USE_ASSOCIATED, but is not in the local symbol table. */

                  ADD_ATTR_TO_LOCAL_LIST(spec_idx);

               }
            }

            if (host_associated) {
               host_associated_attr_semantics(spec_idx, FALSE);
            }
            AT_REFERENCED(spec_idx) = Referenced;
         }

         if (save_where_ir_idx != NULL_IDX && ATP_VFUNCTION(spec_idx))      {
            PRINTMSG(line, 897, Error, col,
                     AT_OBJ_NAME_PTR(spec_idx));
            ok = FALSE;
            goto DONE;
         }

         SCP_HAS_CALLS(curr_scp_idx) = TRUE;

         save_curr_stmt_sh_idx = curr_stmt_sh_idx;
         num_args = IR_LIST_CNT_R(ir_idx);

         if (is_function) {
            rslt_idx                  = ATP_RSLT_IDX(spec_idx);
            IR_TYPE_IDX(ir_idx)       = ATD_TYPE_IDX(rslt_idx);

            save_shape_known = res_exp_desc->shape_known;

            if (save_shape_known) {
               save_rank = res_exp_desc->rank;

               COPY_SHAPE(save_shape, (res_exp_desc->shape), save_rank);
            }

            if (! AT_ELEMENTAL_INTRIN(spec_idx)) {
               save_foldable = res_exp_desc->foldable;
               save_will_fold_later = res_exp_desc->will_fold_later;
               COPY_OPND(save_char_len, (res_exp_desc->char_len));

               (*res_exp_desc) = init_exp_desc;

               res_exp_desc->type_idx    = IR_TYPE_IDX(ir_idx);
               res_exp_desc->type	 = TYP_TYPE(res_exp_desc->type_idx);
               res_exp_desc->linear_type = TYP_LINEAR(res_exp_desc->type_idx);
               res_exp_desc->pointer     = ATD_POINTER(rslt_idx);
               res_exp_desc->target      = ATD_TARGET(rslt_idx);
               res_exp_desc->allocatable = ATD_ALLOCATABLE(rslt_idx);
               res_exp_desc->dope_vector = ATD_IM_A_DOPE(rslt_idx);

               res_exp_desc->will_fold_later = save_will_fold_later;
               res_exp_desc->foldable        = save_foldable;

               if (ATD_ARRAY_IDX(rslt_idx) != NULL_IDX) {
                  res_exp_desc->assumed_shape =
                   (BD_ARRAY_CLASS(ATD_ARRAY_IDX(rslt_idx)) == Assumed_Shape);
                  res_exp_desc->assumed_size  =
                    (BD_ARRAY_CLASS(ATD_ARRAY_IDX(rslt_idx)) == Assumed_Size);
   
                  IR_RANK(ir_idx)	= BD_RANK(ATD_ARRAY_IDX(rslt_idx));
                  res_exp_desc->rank	= IR_RANK(ir_idx);
                  res_exp_desc->contig_array = TRUE;
               }
               else {
                  IR_RANK(ir_idx) = 0;
               }
            }

            res_exp_desc->tmp_reference = TRUE;

            if (! no_func_expansion) {

               if (AT_IS_INTRIN(spec_idx) &&
                   res_exp_desc->will_fold_later) {

                  copy_subtree(result_opnd, &save_result_opnd);

                  list_idx = IR_IDX_R(ir_idx);

                  while (list_idx) {
                     arg_info_list_base      = arg_info_list_top;
                     arg_info_list_top       = arg_info_list_base + 1;

                     if (arg_info_list_top >= arg_info_list_size) {
                        enlarge_info_list_table();
                     }

                     arg_info_list[arg_info_list_top] = 
                                     arg_info_list[IL_ARG_DESC_IDX(list_idx)];

                     IL_ARG_DESC_IDX(list_idx) = arg_info_list_top;

                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                  }
               }

               flatten_function_call(result_opnd);

               if (ATP_ELEMENTAL(spec_idx) &&
                   ATP_PROC(spec_idx) != Intrin_Proc) {

                  attr_idx = find_base_attr(result_opnd, &line, &col);
                  res_exp_desc->rank = BD_RANK(ATD_ARRAY_IDX(attr_idx));
               }

               /* Now that the types for the function result, etc. have been  */
               /* resolved, if the procedure is a function, the Object        */
               /* record that represents the function result can now be       */
               /* output.				                      */

               if (ATP_PROC(spec_idx) == Intrin_Proc  &&
                   ! ATI_USER_SPECIFIED(gen_idx)      &&
                   (cif_flags & MISC_RECS) != 0       &&
                   xref_state != CIF_No_Usage_Rec) {
                  cif_object_rec_for_func_result(spec_idx);
               }

               if (res_exp_desc->type == Character || res_exp_desc->rank) {
   
                  attr_idx = find_base_attr(result_opnd, &line, &col);
   
                  if (res_exp_desc->type == Character) {
                     IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(attr_idx); 
                     res_exp_desc->type_idx = ATD_TYPE_IDX(attr_idx);
                     res_exp_desc->type	= TYP_TYPE(res_exp_desc->type_idx);
                     res_exp_desc->linear_type = 
                                          TYP_LINEAR(res_exp_desc->type_idx);
   
                     if (ATP_PROC(spec_idx) == Intrin_Proc) {
                        COPY_OPND(res_exp_desc->char_len, save_char_len);
                     }
                     else {
                        get_char_len(result_opnd, &(res_exp_desc->char_len));
                     }
                  }

                  if (res_exp_desc->rank && !AT_ELEMENTAL_INTRIN(spec_idx)) {
                     get_shape_from_attr(res_exp_desc,
                                         attr_idx,
                                         res_exp_desc->rank,
                                         line, 
                                         col);
                  }
               }
            }
            else if (!AT_ELEMENTAL_INTRIN(spec_idx)) {
               set_shape_for_deferred_funcs(res_exp_desc, ir_idx);

               if (AT_IS_INTRIN(spec_idx) &&
                   (ATP_INTRIN_ENUM(spec_idx) == Transfer_Intrinsic ||
                    ATP_INTRIN_ENUM(spec_idx) == Trim_Intrinsic)) {
                  COPY_OPND((res_exp_desc->char_len), save_char_len);
               }
            }

            if (save_shape_known) {
               res_exp_desc->shape_known = save_shape_known;
               res_exp_desc->rank = save_rank;
               COPY_SHAPE((res_exp_desc->shape), save_shape, save_rank);
            }
         }
         else if (label_cnt != 0 || ATP_HAS_ALT_RETURN(spec_idx)) { 
            /* do the alternate return thing */
            NTR_IR_TBL(br_idx_idx);
            IR_OPR(br_idx_idx) = Br_Index_Opr;
            IR_TYPE_IDX(br_idx_idx) = CG_INTEGER_DEFAULT_TYPE;

            alt_return_tmp = gen_compiler_tmp(1, 0, Priv, TRUE);
            ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
            ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
            AT_REFERENCED(alt_return_tmp)     = Referenced;
            AT_SEMANTICS_DONE(alt_return_tmp) = TRUE;
            AT_DEFINED(alt_return_tmp)        = TRUE;

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx)        = Alt_Return_Opr;
            IR_TYPE_IDX(asg_idx)   = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(asg_idx)   = line;
            IR_COL_NUM(asg_idx)    = col;
            IR_LINE_NUM_L(asg_idx) = line;
            IR_COL_NUM_L(asg_idx)  = col;
            IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
            IR_IDX_L(asg_idx)      = alt_return_tmp;
            
            COPY_OPND(IR_OPND_R(asg_idx), (*result_opnd));

            if (label_cnt != 0) {
               gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

               curr_stmt_sh_idx                = SH_PREV_IDX(curr_stmt_sh_idx);
               SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

               IR_FLD_L(br_idx_idx)            = AT_Tbl_Idx;
               IR_IDX_L(br_idx_idx)            = alt_return_tmp;
               IR_LINE_NUM(br_idx_idx)         = line;
               IR_COL_NUM(br_idx_idx)          = col;
               IR_LINE_NUM_L(br_idx_idx)       = line;
               IR_COL_NUM_L(br_idx_idx)        = col;
               IR_FLD_R(br_idx_idx)            = IL_Tbl_Idx;
               IR_IDX_R(br_idx_idx)            = label_list_head;
               IR_LIST_CNT_R(br_idx_idx)       = label_cnt;

               OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
               OPND_IDX((*result_opnd)) = br_idx_idx;
            
               list_idx2 = label_list_head;
               while (list_idx2) {
                  AT_REFERENCED( IL_IDX(list_idx2)) = Referenced;
                  list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
               }
            }
            else {
               OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
               OPND_IDX((*result_opnd)) = asg_idx;
            }
         }

         if (! no_func_expansion) {

            if (! is_function) {
               /* this was done for functions under flatten_func_call */
               COPY_OPND(opnd, IR_OPND_R(ir_idx));
               ok = final_arg_work(&opnd, spec_idx, num_args, NULL) && ok;
               COPY_OPND(IR_OPND_R(ir_idx), opnd);
            }

            if (ATP_PROC(spec_idx) != Dummy_Proc &&
                ATP_PROC(spec_idx) != Intrin_Proc &&
                ! ATP_VFUNCTION(spec_idx) &&
                (cmd_line_flags.runtime_argument ||
                 cmd_line_flags.runtime_arg_call)) {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
               list_idx = IR_IDX_R(ir_idx);
               list_idx2 = NULL_IDX;

               idx = 0;

               while (list_idx) {
                  if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                      IR_OPR(IL_IDX(list_idx)) == False_Parm_Opr) {
   
                     false_list_idx = list_idx;
   
                     IL_NEXT_LIST_IDX(list_idx2) = NULL_IDX;
                     break;
                  }

                  list_idx2 = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
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

               OPND_FLD(opnd) = IR_Tbl_Idx;
               OPND_IDX(opnd) = ir_idx;
               idx = create_argchck_descriptor(&opnd);
               IR_IDX_L(loc_idx) = idx;
               IR_LINE_NUM_L(loc_idx) = line;
               IR_COL_NUM_L(loc_idx) = col;
   
               NTR_IR_LIST_TBL(list_idx2);
               IL_ARG_DESC_VARIANT(list_idx2) = TRUE;
               IL_FLD(list_idx2) = IR_Tbl_Idx;
               IL_IDX(list_idx2) = loc_idx;

               if (IR_LIST_CNT_R(ir_idx) == 0) {
                  IR_FLD_R(ir_idx) = IL_Tbl_Idx;
                  IR_IDX_R(ir_idx) = list_idx2;
                  IR_LIST_CNT_R(ir_idx) = 1;
               }
               else {
                  list_idx = IR_IDX_R(ir_idx);
                  while (IL_NEXT_LIST_IDX(list_idx)) {
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                  }
   
                  IL_NEXT_LIST_IDX(list_idx) = list_idx2;
                  (IR_LIST_CNT_R(ir_idx))++;
               }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
               if (false_list_idx) {
                  IL_NEXT_LIST_IDX(list_idx2) = false_list_idx;
                  list_idx = false_list_idx;
                  while (list_idx) {
                     (IR_LIST_CNT_R(ir_idx))++;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                  }
               }
# endif
            }
         }

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;

         if (AT_OBJ_CLASS(gen_idx) == Interface &&
             ATP_PROC(spec_idx) != Intrin_Proc) {

            PRINTMSG(line, 399, Comment, col, AT_OBJ_NAME_PTR(gen_idx),
                     AT_OBJ_NAME_PTR(spec_idx));
         }
      }
   }
   else if (ok) {

      if (AT_IS_INTRIN(gen_idx)) {
         PRINTMSG(line, 
                  700, 
                  Error, 
                  col, 
                  AT_OBJ_NAME_PTR(gen_idx));
      }
      else {
         PRINTMSG(line, 
                  389, 
                  Error, 
                  col, 
                  AT_OBJ_NAME_PTR(gen_idx));
      }
      ok = FALSE;
   }

DONE:

   if (ok && found && (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
                       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx)))) {

      if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx))) {

         if (!ATP_PURE(spec_idx) && !ATP_ELEMENTAL(spec_idx)) {
            PRINTMSG(IR_LINE_NUM_L(ir_idx), 1274, Error, IR_COL_NUM_L(ir_idx),
                     AT_OBJ_NAME_PTR(spec_idx),
                     "pure or elemental",
                     "pure");
          
         }
      }
      else if (ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {

         if (!ATP_PURE(spec_idx) && !ATP_ELEMENTAL(spec_idx)) {
            PRINTMSG(IR_LINE_NUM_L(ir_idx), 1274, Error, IR_COL_NUM_L(ir_idx),
                     AT_OBJ_NAME_PTR(spec_idx),
                     "pure or elemental",
                     "elemental");
          
         }
      }

      /* Check to make sure that actual arguments are definable if */
      /* the dummy arg has INTENT(out), INTENT(inout) or POINTER.  */

      list_idx	= IR_IDX_R(ir_idx);

      if (ATP_EXTRA_DARG(spec_idx)) {
         arg_idx	= ATP_FIRST_IDX(spec_idx) + 1;
         num_args	= ATP_NUM_DARGS(spec_idx) - 1;
      }
      else {
         arg_idx	= ATP_FIRST_IDX(spec_idx);
         num_args	= ATP_NUM_DARGS(spec_idx);
      }
      for (;num_args > 0; num_args--) {

         if (AT_OBJ_CLASS(SN_ATTR_IDX(arg_idx)) == Data_Obj &&
             (ATD_POINTER(SN_ATTR_IDX(arg_idx)) ||
              ATD_INTENT(SN_ATTR_IDX(arg_idx)) == Intent_Inout ||
              ATD_INTENT(SN_ATTR_IDX(arg_idx)) == Intent_Out)) {
            COPY_OPND(opnd, IL_OPND(list_idx));

            if (! check_for_legal_define(&opnd)) {
               ok = FALSE;
            }
         }
         arg_idx++;
         list_idx	= IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (ok && found && ATP_VFUNCTION(spec_idx)) {
      num_registers	= 0;
      list_idx		= IR_IDX_R(ir_idx);

      while (list_idx) {

         if (IL_ARG_DESC_IDX(list_idx) == NULL_IDX) {
            list_idx = IL_NEXT_LIST_IDX(list_idx);
            continue;
         }

         if (arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type == Character ||
             arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type == Structure) {

            PRINTMSG(arg_info_list[IL_ARG_DESC_IDX(list_idx)].line, 
                     910, Error,
                     arg_info_list[IL_ARG_DESC_IDX(list_idx)].col);
            ok = FALSE;
            break;
         }
         else {
            num_registers += TARGET_BITS_TO_WORDS(storage_bit_size_tbl[
                     arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.linear_type]);
         }

         if (num_registers > 7) {
            PRINTMSG(arg_info_list[IL_ARG_DESC_IDX(list_idx)].line, 
                     909, Error,
                     arg_info_list[IL_ARG_DESC_IDX(list_idx)].col);
            ok = FALSE;
            break;
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (ok && found && (ATP_PROC(spec_idx) == Intrin_Proc)) {

      if (expr_mode == Specification_Expr) {

         if (!AT_ELEMENTAL_INTRIN(spec_idx)) {

            switch (ATP_INTRIN_ENUM(spec_idx)) {

# if defined(_F_MINUS_MINUS)
            case Num_Images_Intrinsic:
            case Log2_Images_Intrinsic:
            case Rem_Images_Intrinsic:

               if (!cmd_line_flags.co_array_fortran) {
                  PRINTMSG(line, 870, Error, col, AT_OBJ_NAME_PTR(gen_idx));
                  ok = FALSE;
                  break;
               }
               else if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
                        ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
                  PRINTMSG(line, 1580, Error, col, 
                           AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)),
                           AT_OBJ_NAME_PTR(gen_idx));
                  ok = FALSE;
                  break;
               }

               /* Intentional fall through */
# endif

            case SIK_Intrinsic:
            case SRK_Intrinsic:
            case Lbound_Intrinsic:
            case Ubound_Intrinsic:
            case Size_Intrinsic:
            case Bit_Size_Intrinsic:
            case Len_Intrinsic:
            case Kind_Intrinsic:
            case Digits_Intrinsic:
            case Huge_Intrinsic:
            case Maxexponent_Intrinsic:
            case Minexponent_Intrinsic:
            case Precision_Intrinsic:
            case Radix_Intrinsic:
            case Range_Intrinsic:
            case Transfer_Intrinsic:
            case Merge_Intrinsic:


               if ((TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) != Integer &&
                 TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) != Character) ||
                   res_exp_desc->rank > 1) {
                  PRINTMSG(line, 1070, Error, col, AT_OBJ_NAME_PTR(gen_idx));
                  ok = FALSE;
               }
               break;

            default:  /* Intrinsic not allowed in specification expression. */
               PRINTMSG(line, 870, Error, col, AT_OBJ_NAME_PTR(gen_idx));
               ok = FALSE;
               break;
            }
         }
         else {   /* elemental */
            if ((TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) != Integer &&
                 TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) != Character &&
                 TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) != Real &&
                 TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) != Typeless) ||
                res_exp_desc->rank > 1) {
               PRINTMSG(line, 1070, Error, col, AT_OBJ_NAME_PTR(gen_idx));
               ok = FALSE;
            }
            else {
               if (TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) == Real ||
                   TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(spec_idx))) == Typeless) {
                  PRINTMSG(line, 1392, Ansi, col);
               }
            }
         }
      }
      else if (expr_mode == Initialization_Expr) {

         if (!AT_ELEMENTAL_INTRIN(spec_idx) &&
             (ATP_INTRIN_ENUM(spec_idx) != SIK_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != SRK_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Repeat_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Reshape_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Lbound_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Ubound_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Size_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Shape_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Transfer_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Null_Intrinsic) &&
             (ATP_INTRIN_ENUM(spec_idx) != Trim_Intrinsic)) {
            PRINTMSG(line, 329, Error, col, AT_OBJ_NAME_PTR(gen_idx));
            ok = FALSE;
         }
      }
   }

   if (ok &&
       ATP_PROC(spec_idx) != Intrin_Proc) {

#ifdef KEY /* Bug 7726 */
      /* Fortran 95 says every elemental procedure is pure */
      if (! (ATP_PURE(spec_idx) || ATP_ELEMENTAL(spec_idx)))
#else /* KEY Bug 7726 */
      if (! ATP_PURE(spec_idx))
#endif /* KEY Bug 7726 */
      {
         if (within_forall_mask_expr) {
            PRINTMSG(line, 1611, Error, col, AT_OBJ_NAME_PTR(spec_idx),
                     "forall scalar-mask-expr");
            ok = FALSE;
         }
         else if (within_forall_construct) {
            PRINTMSG(line, 1611, Error, col, AT_OBJ_NAME_PTR(spec_idx),
                     "forall-body-construct");
            ok = FALSE;
         }
      }
   }

   res_exp_desc->has_symbolic = has_symbolic;

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(result_opnd);

   if (OPND_FLD(save_result_opnd) != NO_Tbl_Idx &&
       OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX((*result_opnd))) == Stmt_Expansion_Opr) {

      list_idx = IR_IDX_R(OPND_IDX((*result_opnd)));

      while (IL_NEXT_LIST_IDX(list_idx)) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx), save_result_opnd);

      IR_LIST_CNT_R(OPND_IDX((*result_opnd))) += 1;
   }

   if (save_where_ir_idx != NULL_IDX &&
       where_ir_idx == NULL_IDX) {

      where_ir_idx = save_where_ir_idx;

      if (ATP_ELEMENTAL(spec_idx)) {
         check_for_elementals(find_left_attr(result_opnd));
      }
   }
   else {
      where_ir_idx = save_where_ir_idx;
   }

   TRACE (Func_Exit, "call_list_semantics", NULL);

   return(ok);

}  /* call_list_semantics */

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

static void check_for_elementals(int	attr_idx)

{
   int			asg_idx;
   int			call_idx;
   int			col;
   int			line;
   int			list_idx;
   opnd_type		opnd;
   int			tmp_idx;

   TRACE (Func_Entry, "check_for_elementals", NULL);

   line = AT_DEF_LINE(attr_idx);
   col = AT_DEF_COLUMN(attr_idx);

# ifdef _DEBUG
   if (ATD_FLD(attr_idx) != IR_Tbl_Idx) {
      PRINTMSG(line, 626, Internal, col,
               "ATD_FLD(attr_idx) == IR_Tbl_Idx", "check_for_elementals");
   }
# endif

   /* this had better be the function call. */
   asg_idx = ATD_TMP_IDX(attr_idx);
      
# ifdef _DEBUG
   if (IR_FLD_R(asg_idx) != IR_Tbl_Idx ||
       IR_OPR(IR_IDX_R(asg_idx)) != Call_Opr ||
       ! ATP_ELEMENTAL(IR_IDX_L(IR_IDX_R(asg_idx)))) {

      PRINTMSG(line, 626, Internal, col,
               "elemental function", "check_for_elementals");
   }
# endif

   call_idx = IR_IDX_R(asg_idx);

   change_asg_to_where(asg_idx);

   /* check the arguments for array temps. */
   /* they are either array expressions to be masked, */
   /* or they are call temps. */

   list_idx = IR_IDX_R(call_idx);

   while (list_idx) {

      tmp_idx = find_left_attr(&IL_OPND(list_idx));
      line = AT_DEF_LINE(tmp_idx);
      col = AT_DEF_COLUMN(tmp_idx);

      if (AT_OBJ_CLASS(tmp_idx) == Data_Obj &&
          ATD_CLASS(tmp_idx) == Compiler_Tmp &&
          ATD_FLD(tmp_idx) == IR_Tbl_Idx &&
          ATD_ARRAY_IDX(tmp_idx) != NULL_IDX &&
          (ATD_ELEMENTAL_CALL_TMP(tmp_idx) ||
           ATD_ASG_TMP(tmp_idx))) {

         asg_idx = ATD_TMP_IDX(tmp_idx);

# ifdef _DEBUG
         if (IR_OPR(asg_idx) != Asg_Opr) {
            PRINTMSG(line, 626, Internal, col,
                     "Asg_Opr", "check_for_elementals");
         }
# endif

         if (ATD_ELEMENTAL_CALL_TMP(tmp_idx)) {
            check_for_elementals(tmp_idx);
         }
         else if (ATD_ASG_TMP(tmp_idx)) {
            COPY_OPND(opnd, IR_OPND_R(asg_idx));
            check_expr_for_elementals(&opnd);

            change_asg_to_where(asg_idx);
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "check_for_elementals", NULL);

   return;

}  /* check_for_elementals */

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

static void check_expr_for_elementals(opnd_type	*top_opnd)

{
   int			attr_idx;
   int			ir_idx;
   int			list_idx;
   opnd_type		opnd;

   TRACE (Func_Entry, "check_expr_for_elementals", NULL);

   switch (OPND_FLD((*top_opnd))) {
   case AT_Tbl_Idx:
      attr_idx = OPND_IDX((*top_opnd));
 
      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          ATD_CLASS(attr_idx) == Compiler_Tmp &&
          ATD_ELEMENTAL_CALL_TMP(attr_idx)) {

          check_for_elementals(attr_idx);
      }
      break;

   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*top_opnd));
      COPY_OPND(opnd, IR_OPND_L(ir_idx));
      check_expr_for_elementals(&opnd);
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      check_expr_for_elementals(&opnd);
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*top_opnd));
      while (list_idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         check_expr_for_elementals(&opnd);
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;

   case CN_Tbl_Idx:
   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;
   }

   TRACE (Func_Exit, "check_expr_for_elementals", NULL);

   return;

}  /* check_expr_for_elementals */

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

void change_asg_to_where(int	asg_idx)

{
   int		list_idx;

   TRACE (Func_Entry, "change_asg_to_where", NULL);

   if (IR_OPR(asg_idx) == Call_Opr) {
      /* list 1 = routine attr */

      NTR_IR_LIST_TBL(list_idx);
      COPY_OPND(IL_OPND(list_idx), IR_OPND_L(asg_idx));
      IR_FLD_L(asg_idx) = IL_Tbl_Idx;
      IR_IDX_L(asg_idx) = list_idx;
      IR_LIST_CNT_L(asg_idx) = 4;

      /* list 2 = lhs (first arg) */

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx),
                IL_OPND(IR_IDX_R(asg_idx)));

      /* list 3 = mask */

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = where_ir_idx;

      /* list 4 = rhs (second arg) */

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx), 
                IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_R(asg_idx))));
   }
   else {
      /* list 1 = lhs */

      NTR_IR_LIST_TBL(list_idx);
      COPY_OPND(IL_OPND(list_idx), IR_OPND_L(asg_idx));
      IR_FLD_L(asg_idx) = IL_Tbl_Idx;
      IR_IDX_L(asg_idx) = list_idx;
      IR_LIST_CNT_L(asg_idx) = 3;

      /* list 2 = mask */

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = where_ir_idx;

      /* list 3 = rhs */

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx), IR_OPND_R(asg_idx));
   }

   IR_OPR(asg_idx) = Where_Opr;
   IR_OPND_R(asg_idx) = null_opnd;

   TRACE (Func_Exit, "change_asg_to_where", NULL);

   return;

}  /* change_asg_to_where */

#ifdef KEY
static boolean inside_loop(int stmt_sh_idx)
{
  while (stmt_sh_idx != NULL_IDX){
    if ( SH_STMT_TYPE(stmt_sh_idx) == Do_Iterative_Stmt ||
         SH_STMT_TYPE(stmt_sh_idx) == Do_While_Stmt)
      return TRUE;
    else if (IR_OPR(SH_IR_IDX(stmt_sh_idx)) == Label_Opr ||
             SH_STMT_TYPE(stmt_sh_idx) == End_Do_Stmt || 
             SH_STMT_TYPE(stmt_sh_idx) == End_Forall_Stmt)
      return FALSE;
    stmt_sh_idx = SH_PREV_IDX(stmt_sh_idx);
  }
  return FALSE;
}
static boolean stride_access_greater_than_1(opnd_type          * opnd, char *dim)
{
  int ir_idx = OPND_IDX((*opnd));

  if (ir_idx == NULL_IDX) return FALSE;

  if (OPND_FLD((*opnd)) != IR_Tbl_Idx || IR_RANK(ir_idx) < 1)
    return FALSE;

  if (IR_FLD_R(ir_idx) != IL_Tbl_Idx)
    return FALSE;

  int list_idx = IR_IDX_R(ir_idx);
  int counter = 1;
  boolean flag = FALSE;

  while (list_idx != NULL_IDX) {

    if (IL_FLD(list_idx) == IR_Tbl_Idx && 
        IR_RANK(IL_IDX(list_idx)) > 0){
      (*dim) ++;
      if (counter > 1)
        flag = TRUE;
    }

    list_idx = IL_NEXT_LIST_IDX(list_idx);
    dim ++;
    counter ++;
  }
  return flag;
}
#ifndef KEY /* Bug 4955 */
static void generate_max_bound(opnd_type *opnd, int ir_idx, int attr_idx, int dim)
{
   int plus_idx, sub_idx, div_idx, plus_idx1, minus_idx, attr_bd_idx,
       max_idx, list_idx, dv_idx, dv_low_idx, deref_idx, deref_fld;
   int line, col;
                                                                                                                                                             
   max_idx                      = IR_IDX_R(ir_idx);
   list_idx                     = IR_IDX_L(max_idx);
   line                         = IR_LINE_NUM(max_idx);
   col                          = IR_COL_NUM(max_idx);
                                                                                                                                                             
   NTR_IR_TBL(plus_idx);
   IR_OPR(plus_idx)             = Plus_Opr;
   IR_TYPE_IDX(plus_idx)        = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(plus_idx)        = line;
   IR_COL_NUM(plus_idx)         = col;
                                                                                                                                                             
   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx)              = Minus_Opr;
   IR_TYPE_IDX(sub_idx)         = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(sub_idx)         = line;
   IR_COL_NUM(sub_idx)          = col;
                                                                                                                                                             
   NTR_IR_TBL(div_idx);
   IR_OPR(div_idx)              = Div_Opr;
   IR_TYPE_IDX(div_idx)         = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(div_idx)         = line;
   IR_COL_NUM(div_idx)          = col;
                                                                                                                                                             
   IL_IDX(list_idx)             = div_idx;
   IR_FLD_L(div_idx)            = IR_Tbl_Idx;
   IR_IDX_L(div_idx)            = plus_idx;
                                                                                                                                                             
   IR_FLD_L(plus_idx)           = IR_Tbl_Idx;
   IR_IDX_L(plus_idx)           = sub_idx;
                                                                                                                                                             
   IR_FLD_R(plus_idx)           = CN_Tbl_Idx;
   IR_IDX_R(plus_idx)           = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(plus_idx)      = line;
   IR_COL_NUM_R(plus_idx)       = col;
                                                                                                                                                             
   if (ATD_CLASS(attr_idx) == Variable)
     attr_bd_idx =  ATD_ARRAY_IDX(attr_idx);
                                                                                                                                                             
   if (ATD_CLASS(attr_idx) == Dummy_Argument ||
#ifdef KEY /* Bug 5431 */
       ATD_CLASS(attr_idx) == Function_Result ||
#endif /* KEY Bug 5431 */
       ATD_CLASS(attr_idx) == Variable){
     attr_bd_idx =  ATD_ARRAY_IDX(attr_idx);
     if (attr_bd_idx && 
         (BD_ARRAY_CLASS(attr_bd_idx) == Assumed_Shape ||
          BD_ARRAY_CLASS(attr_bd_idx) == Explicit_Shape)){
       IR_FLD_R(sub_idx)            = BD_LB_FLD(attr_bd_idx,dim);
       IR_IDX_R(sub_idx)            = BD_LB_IDX(attr_bd_idx,dim);
     }
     else{
       deref_idx = IR_IDX_L(IR_IDX_L(OPND_IDX((*opnd))));
       deref_fld = IR_FLD_L(IR_IDX_L(OPND_IDX((*opnd))));
       dv_low_idx = gen_ir(deref_fld, deref_idx, 
                  Dv_Access_Low_Bound, SA_INTEGER_DEFAULT_TYPE, line, col,
                      NO_Tbl_Idx, NULL_IDX);
       IR_DV_DIM(dv_low_idx) = dim;
       IR_FLD_R(sub_idx)            = IR_Tbl_Idx;
       IR_IDX_R(sub_idx)            = dv_low_idx;
     }
     IR_LINE_NUM_R(sub_idx)       = line;
     IR_COL_NUM_R(sub_idx)        = col;
   }
                                                                                                                                                             
   IR_FLD_R(div_idx)            = CN_Tbl_Idx;
   IR_IDX_R(div_idx)            = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(div_idx)       = line;
   IR_COL_NUM_R(div_idx)        = col;
                                                                                                                                                             
   if (ATD_CLASS(attr_idx) == Dummy_Argument ||
#ifdef KEY /* Bug 5431 */
       ATD_CLASS(attr_idx) == Function_Result ||
#endif /* KEY Bug 5431 */
       ATD_CLASS(attr_idx) == Variable){
     attr_bd_idx =  ATD_ARRAY_IDX(attr_idx);
     if (BD_ARRAY_CLASS(attr_bd_idx) == Explicit_Shape){
       IR_FLD_L(sub_idx)            = BD_UB_FLD(attr_bd_idx,dim);
       IR_IDX_L(sub_idx)            = BD_UB_IDX(attr_bd_idx,dim);
     }
     else{
       deref_idx = IR_IDX_L(IR_IDX_L(OPND_IDX((*opnd))));
       deref_fld = IR_FLD_L(IR_IDX_L(OPND_IDX((*opnd))));
       dv_idx = gen_ir(deref_fld, deref_idx,
                   Dv_Access_Extent,SA_INTEGER_DEFAULT_TYPE,line,col,
                   NO_Tbl_Idx, NULL_IDX);
                                                                                                                                                             
       IR_DV_DIM(dv_idx)           = dim;

       if (attr_bd_idx &&
         BD_ARRAY_CLASS(attr_bd_idx) == Assumed_Shape) 
         plus_idx1 = gen_ir(BD_LB_FLD(attr_bd_idx,dim), BD_LB_IDX(attr_bd_idx,dim), 
                     Plus_Opr,SA_INTEGER_DEFAULT_TYPE,line,col,
                     IR_Tbl_Idx, dv_idx);
       else{
         deref_idx = IR_IDX_L(IR_IDX_L(OPND_IDX((*opnd))));
         deref_fld = IR_FLD_L(IR_IDX_L(OPND_IDX((*opnd))));
         dv_low_idx = gen_ir(deref_fld, deref_idx,
                      Dv_Access_Low_Bound, SA_INTEGER_DEFAULT_TYPE, line, col,
                      NO_Tbl_Idx, NULL_IDX);
         IR_DV_DIM(dv_low_idx) = dim;
         plus_idx1 = gen_ir(IR_Tbl_Idx, dv_low_idx,
                     Plus_Opr,SA_INTEGER_DEFAULT_TYPE,line,col,
                     IR_Tbl_Idx, dv_idx);
       }
       minus_idx = gen_ir(IR_Tbl_Idx, plus_idx1,
                      Minus_Opr,SA_INTEGER_DEFAULT_TYPE,line,col,
                      CN_Tbl_Idx, CN_INTEGER_ONE_IDX);
       IR_FLD_L(sub_idx)            = IR_Tbl_Idx;
       IR_IDX_L(sub_idx)            = minus_idx;
     }
   }

   IR_LINE_NUM_L(sub_idx)       = line;
   IR_COL_NUM_L(sub_idx)        = col;
                                                                                                                                                             
}
#endif /* KEY Bug 4955 */

#ifdef KEY /* Bug 3607 */
/*
 * Check that a subtree involved in the COPY_INOUT_MAKE_DV optimization does
 * not depend on variables other than the array whose section we are taking.
 *
 * fld and idx represent an IR or IL node
 * array_idx is the AT index for the array itself
 * returns false if any leaf of the tree is an AT index other than array_idx
 */
static boolean check_leaves(fld_type fld, int idx, int array_idx) {
  switch(fld) {
    case AT_Tbl_Idx:
      return idx == array_idx;
    case IR_Tbl_Idx:
      return check_leaves(IR_FLD_L(idx), IR_IDX_L(idx), array_idx) &&
	check_leaves(IR_FLD_R(idx), IR_IDX_R(idx), array_idx);
    case IL_Tbl_Idx:
      for (; NULL_IDX != idx; idx = IL_NEXT_LIST_IDX(idx)) {
        if (!check_leaves(IL_FLD(idx), IL_IDX(idx), array_idx)) {
	  return FALSE;
	}
      }
  }
  return TRUE;
}

/*
 * Given opnd, which is an array section for which we will generate
 * copyinout code, return true if it is safe to move the alloc and related
 * computations from the copyinout site to the prolog. Our pattern-matching
 * is conservative, since we don't have use/def info.
 */
static boolean safe_to_move_copyinout_alloc(opnd_type *opnd)
{
  /* Must be reference to simple array (ruling out "t(n)%a(1,:)", for example,
   * since n might vary) */
  int sso_idx = OPND_IDX((*opnd));
  int array_idx;
  if (Section_Subscript_Opr != IR_OPR(sso_idx)) return FALSE;
  if (AT_Tbl_Idx == IR_FLD_L(sso_idx)) {
    array_idx = IR_IDX_L(sso_idx);
  }
  else {
    if (IR_Tbl_Idx != IR_FLD_L(sso_idx)) return FALSE;
    int dvdro_idx = IR_IDX_L(sso_idx);
    if (Dv_Deref_Opr != IR_OPR(dvdro_idx)) return FALSE;
    if (AT_Tbl_Idx != IR_FLD_L(dvdro_idx)) return FALSE;
    array_idx = IR_IDX_L(dvdro_idx);
  }

  /* Each subscript must be either:
   * (a) expression (an entire rank: size of section is independent of
   *     which element is chosen, so variables are ok)
   * (b) constant:constant
   * (c) constant:
   * (d) :constant
   * (e) :
   * Cases b-e are all represented by Triplet_Opr. The empty bounds will
   * generate a reference to the dope vector for the array, so we disallow
   * any variable except the array itself in cases b-e.
   */
  if (IL_Tbl_Idx != IR_FLD_R(sso_idx)) return FALSE;
  for (int list_idx = IR_IDX_R(sso_idx); NULL_IDX != list_idx;
    list_idx = IL_NEXT_LIST_IDX(list_idx)) {
    int subscr_idx = IL_IDX(list_idx);
    if (IR_Tbl_Idx != IL_FLD(list_idx) || Triplet_Opr != IR_OPR(subscr_idx)) {
      continue;
    }
    if (IL_Tbl_Idx != IR_FLD_L(subscr_idx)) return FALSE;
    for (int tlist_idx = IR_IDX_L(subscr_idx); NULL_IDX != tlist_idx;
      tlist_idx = IL_NEXT_LIST_IDX(tlist_idx)) {
      if (!check_leaves(IL_FLD(tlist_idx), IL_IDX(tlist_idx), array_idx)) {
        return FALSE;
      }
    }
  }

  return TRUE;
}
#endif /* KEY Bug 3607 */

/*
 * Move the alloc-and-assign portion of the copyinout code to the entry of
 * the enclosing function, so that if the call is inside a loop, this code
 * occurs once per function instead of once per loop. Caller must ensure
 * that it is safe to do so.
 *
 * old_stmt_sh_idx - first stmt of copyinout code
 * attr_idx - array variable being sliced
 *
 */
static void move_tmp_alloc_assignment(int old_stmt_sh_idx, int attr_idx)
{
  int entry_stmt_sh_idx, next_stmt_sh_idx,
      first_stmt_sh_idx, last_stmt_sh_idx, entry_list_idx,
      new_start_sh_idx, new_end_sh_idx, entry_attr_idx;

#ifdef _DEBUG
  move_copyinout_alloc_count += 1;
#endif /* _DEBUG */

  // Set entry_stmt_sh_idx to point to first Entry_Opr in scope
  // KEY Bug 6935: Searching backward from the curr_stmt_sh_idx doesn't work
  // because if save_defer_stmt_expansion is true, the machinery in
  // stmt_expansion_control_start() detaches the current statement from
  // the surrounding function. Searching forward from SCP_FIRST_SH_IDX()
  // fixes this and probably executes faster.
  for (entry_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);
     entry_stmt_sh_idx != NULL_IDX;
     entry_stmt_sh_idx = SH_NEXT_IDX(entry_stmt_sh_idx)){
     if (IR_OPR(SH_IR_IDX(entry_stmt_sh_idx)) == Entry_Opr) {
	break;
     }
  }

  if (entry_stmt_sh_idx == NULL_IDX)
    PRINTMSG (stmt_start_line, 1044, Internal, SH_COL_NUM(old_stmt_sh_idx),
      "No Entry_Opr");

  // Search forward and set entry_stmt_sh_idx to User_Code_Start_Opr
  while (IR_OPR(SH_IR_IDX(SH_NEXT_IDX(entry_stmt_sh_idx)))
    != User_Code_Start_Opr)
    entry_stmt_sh_idx = SH_NEXT_IDX(entry_stmt_sh_idx);

#ifndef KEY /* Bug 4955 */
. for (dim_counter = 0; dim_counter < MAX_DIMENSION; dim_counter ++)
.   if (dim[dim_counter] > 0)
.     break;
#endif /* Bug 4955 */

  // Set first_stmt_sh_idx to the first statement we wish to move, and
  // search forward for the "alloc" which is the last statement we wish to
  // move
  last_stmt_sh_idx = NULL_IDX;
  for (int stmt_sh_idx = first_stmt_sh_idx = SH_NEXT_IDX(old_stmt_sh_idx);
    stmt_sh_idx != curr_stmt_sh_idx;
    stmt_sh_idx = SH_NEXT_IDX(stmt_sh_idx)) {
#ifndef KEY /* Bug 4955 */
.   if (IR_OPR(SH_IR_IDX(stmt_sh_idx)) == Asg_Opr &&
.       IR_OPR(IR_IDX_R(SH_IR_IDX(stmt_sh_idx))) == Max_Opr && dim_counter < MAX_DIMENSION){
.     /* This wrongly overwrites a valid computation of the section size with
.      * an invalid computation. The computation is used both in "alloc"
.      * (where it would be overly generous, but harmless) and in the dope
.      * vector and the copying of the section, where it causes accesses
.      * beyond the end of the array.
.      */
.     generate_max_bound(opnd, SH_IR_IDX(stmt_sh_idx), attr_idx, dim_counter+1);
.     dim_counter ++;
.     for (;dim_counter < MAX_DIMENSION; dim_counter ++)
.       if (dim[dim_counter] > 0)
.         break;
.   }
#endif /* KEY Bug 4955 */
    if (IR_OPR(SH_IR_IDX(stmt_sh_idx)) == Asg_Opr &&
        IR_OPR(IR_IDX_R(SH_IR_IDX(stmt_sh_idx))) == Alloc_Opr){
      last_stmt_sh_idx = stmt_sh_idx;
      break;
    }
  }

  if (last_stmt_sh_idx == NULL_IDX)
    return;

  SH_NEXT_IDX(SH_PREV_IDX(first_stmt_sh_idx)) = SH_NEXT_IDX(last_stmt_sh_idx);
  SH_PREV_IDX(SH_NEXT_IDX(last_stmt_sh_idx))  = SH_PREV_IDX(first_stmt_sh_idx);

  next_stmt_sh_idx               = SH_NEXT_IDX(entry_stmt_sh_idx);
  SH_NEXT_IDX(entry_stmt_sh_idx) = first_stmt_sh_idx;
  SH_PREV_IDX(first_stmt_sh_idx) = entry_stmt_sh_idx;
  SH_NEXT_IDX(last_stmt_sh_idx)  = next_stmt_sh_idx;
  SH_PREV_IDX(next_stmt_sh_idx)  = last_stmt_sh_idx;

#ifdef KEY /* Bug 4955 */
  /* If the variable being sliced/sectioned is an optional formal arg, we
   * must check that it's present at execution time, since we're moving this
   * code outside any user-written construct that might be conditional on its
   * presence.
   */
  if (AT_OPTIONAL(attr_idx)) {
    gen_present_ir(attr_idx, first_stmt_sh_idx, last_stmt_sh_idx);
  }
#endif /* KEY Bug 4955 */

  entry_list_idx       = SCP_ENTRY_IDX(curr_scp_idx);
  while (entry_list_idx != NULL_IDX) {
    entry_attr_idx                        = AL_ATTR_IDX(entry_list_idx);
    entry_stmt_sh_idx                     = ATP_FIRST_SH_IDX(entry_attr_idx);
    next_stmt_sh_idx                      = SH_NEXT_IDX(entry_stmt_sh_idx);
    copy_entry_exit_sh_list(first_stmt_sh_idx,
                            last_stmt_sh_idx,
                            &new_start_sh_idx,
                            &new_end_sh_idx);

    if (new_start_sh_idx != NULL_IDX) {
      SH_NEXT_IDX(entry_stmt_sh_idx)      = new_start_sh_idx;
      SH_PREV_IDX(new_start_sh_idx)       = entry_stmt_sh_idx;
                                                                                                                                                             
      entry_stmt_sh_idx                   = new_end_sh_idx;
                                                                                                                                                             
      SH_PREV_IDX(next_stmt_sh_idx)       = entry_stmt_sh_idx;
      SH_NEXT_IDX(entry_stmt_sh_idx)      = next_stmt_sh_idx;
      ATP_FIRST_SH_IDX(entry_attr_idx)    = entry_stmt_sh_idx;
      entry_list_idx                      = AL_NEXT_IDX(entry_list_idx);
    }
  }

  return;
} /* move_tmp_alloc_assignment */
#endif
#ifdef KEY /* Bug 7424 */
/*
 * Fortran 90 and later standards say that you can't assign the address of a
 * dummy argument to a pointer unless it has the 'target' attribute, and that
 * if a dummy argument has that attribute, the interface to the procedure must
 * be explicitly visible to the caller. Thus we don't need to clear the
 * 'pointer to unique memory' WHIRL attr unless we see an explicit dummy
 * argument having the target attribute. However, for performance, one can set
 * -LANG:ignore_target_attribute=on which will never clear the WHIRL attr.
 */
static int clear_pt_unique_mem(int dummy) {
  if (LANG_Ignore_Target_Attribute) {
    return 0;
    }
  return dummy && (AT_OBJ_CLASS(dummy) == Data_Obj) && ATD_TARGET(dummy);
  }
#endif /* KEY Bug 7424 */
#ifdef KEY /* Bug 8117 */
extern int create_tmp_asg_or_call(opnd_type *r_opnd, expr_arg_type *exp_desc,
  opnd_type *left_opnd, int intent, boolean stmt_tmp,
  boolean save_where_dealloc_stmt, int info_idx, dummy_arg_type a_type,
  dummy_arg_type d_type);
static int common_create_tmp_asg(opnd_type *r_opnd, expr_arg_type *exp_desc,
  opnd_type *left_opnd, int intent, boolean stmt_tmp,
  boolean save_where_dealloc_stmt, boolean call);

/*
 * Wrap an Aloc_Opr around the specified operand, so as to pass it by
 * reference
 */
int
pass_by_ref(fld_type fld, int idx, int line, int col) {
  int ir_idx;
  NTR_IR_TBL(ir_idx);
  IR_OPR(ir_idx)	= Aloc_Opr;
  IR_TYPE_IDX(ir_idx)	= CRI_Ptr_8;
  IR_LINE_NUM(ir_idx) = line;
  IR_COL_NUM(ir_idx)  = col;
  IR_FLD_L(ir_idx) = fld;
  IR_IDX_L(ir_idx) = idx;
  IR_LINE_NUM_L(ir_idx) = line;
  IR_COL_NUM_L(ir_idx)  = col;
  return ir_idx;
}

/*
 * Walk the tree belonging to "opnd" looking for a variable reference which
 * we can pass to _Copyin or _Copyout. This is normally an attribute, but if
 * we encounter a Struct_Opr first, we return that.
 * opnd		Tree in which the variable reference appears
 * fld		AT_Tbl_Idx or IR_Tbl_Idx which applies to the reference
 * return	index into attribute or IR table, or NULL_IDX if the tree
 *		doesn't conform to expectations.
 */
static int
get_variable_reference(opnd_type *opnd, fld_type *fld) {
  fld_type ft = opnd->fld;
  Uint idx = opnd->idx;
  for (;;) {
    switch (ft) {
      case AT_Tbl_Idx:
	*fld = AT_Tbl_Idx;
	return idx;
        break;
      case IR_Tbl_Idx:
	if (IR_OPR(idx) == Struct_Opr) {
	  *fld = IR_Tbl_Idx;
	  return idx;
	}
	ft = IR_FLD_L(idx);
	idx = IR_IDX_L(idx);
        break;
      default:
        return NULL_IDX;
    }
  }
}

/*
 * Build a call to the copyin or copyout runtime library procedure.
 * "which_call" and "name" identify the procedure, "dest_type" and "dest_idx"
 * identify the destination argument, "src_type" and "src_idx" identify the
 * source argument, "line" and "column" identify the position in the
 * source program.
 */
static int
build_copyinout_call(glb_tbl_idx_type which_call, char *name,
  fld_type dest_fld, int dest_idx, fld_type src_fld, int src_idx, int line,
  int column)
{
  int arg_list_idx = gen_il(2, TRUE, line, column,
    IR_Tbl_Idx, pass_by_ref(dest_fld, dest_idx, line, column),
    IR_Tbl_Idx, pass_by_ref(src_fld, src_idx, line, column));

  return build_call(which_call, name, arg_list_idx, line, column);
}
#endif /* KEY Bug 8117 */

#ifdef KEY /* Bug 5089 */
/*
 * Generate IR for a call to a runtime procedure, passing a list of arguments.
 * The procedure is marked as "PURE".
 *
 * which_call	Identifies the runtime procedure
 * name		Linker symbol for the runtime procedure
 * arg_list_idx	IL_Tbl_Idx for the argument list
 * line		line number
 * column	column number
 * returns	ir_idx for the call_opr
 */
int
build_call(glb_tbl_idx_type which_call, char *name, int arg_list_idx, int line,
  int column) {

  /* If we haven't yet created an attr for the runtime library function we need
   * to call, do so */
  if (glb_tbl_idx[which_call] == NULL_IDX) {
    glb_tbl_idx[which_call] = create_lib_entry_attr(name, strlen(name), line,
      column);
    ATP_PURE(glb_tbl_idx[which_call]) = TRUE;
  }
  ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[which_call]);

  /* Create call */
  int call_idx = gen_ir(AT_Tbl_Idx, glb_tbl_idx[which_call], Call_Opr,
    TYPELESS_DEFAULT_TYPE, line, column, IL_Tbl_Idx, arg_list_idx);

  /* Magic copied from gen_ptr_chk_call() */
  opnd_type opnd;
  gen_opnd(&opnd, call_idx, IR_Tbl_Idx, line, column);
  cif_usage_code_type save_xref_state = xref_state;
  xref_state      = CIF_No_Usage_Rec;
  expr_mode_type save_expr_mode  = expr_mode;
  expr_mode       = Regular_Expr;
  expr_arg_type exp_desc        = init_exp_desc;
  stop_recursion = TRUE;
  call_list_semantics(&opnd, &exp_desc, FALSE);
  stop_recursion = FALSE;
  xref_state = save_xref_state;
  expr_mode  = save_expr_mode;

  return call_idx;
}

/*
 * Generate a statement containing a call to a runtime procedure, passing
 * a single argument by reference
 *
 * which_call	Identifies the runtime procedure
 * name		Linker symbol for the runtime procedure
 * arg_idx	AT_Tbl_Idx for the argument to pass
 * line		line number
 * column	column number
 * returns	curr_stmt_sh_idx, which points to the statement
 */
static int
gen_ieee_save_or_restore(glb_tbl_idx_type which_call, char *name,
  int arg_idx, int line, int column)
{
  int curr_stmt_sh_idx = ntr_sh_tbl();
  SH_STMT_TYPE(curr_stmt_sh_idx) = Call_Stmt;
  SH_GLB_LINE(curr_stmt_sh_idx) = line;
  SH_COL_NUM(curr_stmt_sh_idx) = column;
  SH_COMPILER_GEN(curr_stmt_sh_idx) = TRUE;
  SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

  int arg_list_idx = gen_il(1, TRUE, line, column,
    IR_Tbl_Idx, pass_by_ref(AT_Tbl_Idx, arg_idx, line, column));

  SH_IR_IDX(curr_stmt_sh_idx) = build_call(which_call, name, arg_list_idx, line,
    column);

  return curr_stmt_sh_idx;
}

/*
 * Generate a statement containing a call to a runtime procedure to save the
 * FPU state, and a statement containing a call to a runtime procedure to
 * restore the FPU state. Append the former to the statement containing the
 * entry_opr; append the latter to the SCP_EXIT_IR_SH_IDX for curr_scp_idx.
 *
 * curr_scp_idx	Identifies the scope of interest
 * line		line number
 * column	column number
 */
int
gen_ieee_save_and_restore(int curr_scp_idx, int line, int column) {

  /* Create temporary in which we can save the FPU state */
  int tmp_idx = gen_compiler_tmp(line, column, Priv, TRUE);
  CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
  TYP_TYPE(TYP_WORK_IDX) = Typeless;
  int ieee_save_size = WORD_ALIGNED_BIT_LENGTH(CHAR_BIT * IEEE_SAVE_SIZE);
  TYP_LINEAR(TYP_WORK_IDX) = (ieee_save_size <= MAX_SHORT_TYPELESS_BITS) ?
    Short_Typeless_Const :
    Long_Typeless ;
  TYP_BIT_LEN(TYP_WORK_IDX) = ieee_save_size;
  ATD_TYPE_IDX(tmp_idx) = ntr_type_tbl();
  ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
  AT_SEMANTICS_DONE(tmp_idx) = TRUE;
  AT_DEFINED(tmp_idx) = TRUE;

  /* Save-FPU call goes after the entry_opr */
  int curr_stmt_sh_idx = gen_ieee_save_or_restore(Ieee_Save_Attr_Idx,
    IEEE_SAVE_ENTRY, tmp_idx, line, column);
  int entry_stmt_sh_idx = entry_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);
  for (; entry_stmt_sh_idx != NULL_IDX &&
        IR_OPR(SH_IR_IDX(entry_stmt_sh_idx)) != Entry_Opr;
     entry_stmt_sh_idx = SH_NEXT_IDX(entry_stmt_sh_idx))
     ;
  if (entry_stmt_sh_idx == NULL_IDX) {
    PRINTMSG (line, 1044, Internal, column, "No Entry_Opr");
  }
  SH_PREV_IDX(SH_NEXT_IDX(entry_stmt_sh_idx)) = curr_stmt_sh_idx;
  SH_NEXT_IDX(curr_stmt_sh_idx) = SH_NEXT_IDX(entry_stmt_sh_idx);
  SH_NEXT_IDX(entry_stmt_sh_idx) = curr_stmt_sh_idx;
  SH_PREV_IDX(curr_stmt_sh_idx) = entry_stmt_sh_idx;

  /* Restore-FPU call goes on the exit list */
  curr_stmt_sh_idx = gen_ieee_save_or_restore(Ieee_Restore_Attr_Idx,
    IEEE_RESTORE_ENTRY, tmp_idx, line, column);
  SH_NEXT_IDX(curr_stmt_sh_idx) = SCP_EXIT_IR_SH_IDX(curr_scp_idx);
  SCP_EXIT_IR_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;
}
#endif /* KEY Bug 5089 */

#ifdef KEY /* Bug 10157 */
/*
 * dummy_idx	AT_Tbl_Idx of a dummy argument
 * return TRUE if the dummy argument is an assumed-shape array and the lower
 * 		bound of each dimension is unspecified
 */
static boolean
lower_bounds_match(int actual_il_idx, int dummy_idx) {

  if (IR_Tbl_Idx != IL_FLD(actual_il_idx)) {
    return FALSE;
    }
  int actual_ir_idx = IL_IDX(actual_il_idx);
  if (IR_OPR(actual_ir_idx) != Whole_Subscript_Opr ||
    IR_FLD_L(actual_ir_idx) != IR_Tbl_Idx) {
    return FALSE;
  }
  int dv_deref_idx = IR_IDX_L(actual_ir_idx);
  if (IR_OPR(dv_deref_idx) != Dv_Deref_Opr ||
    IR_FLD_L(dv_deref_idx) != AT_Tbl_Idx) {
    return FALSE;
  }
  int at_idx = IR_IDX_L(dv_deref_idx);
  int actual_array_idx = ATD_ARRAY_IDX(at_idx);

  int dummy_array_idx = ATD_ARRAY_IDX(dummy_idx);
  if (NULL_IDX == dummy_array_idx ||
    Assumed_Shape != BD_ARRAY_CLASS(dummy_array_idx)) {
    return FALSE;
  }
  for (int i = 1; i <= BD_RANK(dummy_array_idx); i++) {
    /* Assume unique CN_Tbl_Idx value for each integer constant */
    if (BD_LB_FLD(actual_array_idx, i) != BD_LB_FLD(dummy_array_idx, i) ||
      BD_LB_IDX(actual_array_idx, i) != BD_LB_IDX(dummy_array_idx, i)) {
      return FALSE;
    }
  }
  return TRUE;
}
#endif /* KEY Bug 10157 */
#ifdef KEY /* Bug 14150 */
/*
 * spec_idx	AT_Tbl_Idx for specific function
 * info_idx	index into arg_info_list[] for actual argument
 * dummy_idx	AT_Tbl_Idx for dummy argument
 * return	true if we should pass the actual argument by value
 */
static int
pass_by_value(int spec_idx, int info_idx, int dummy_idx) {
  return ATP_VFUNCTION(spec_idx) ||
    arg_info_list[info_idx].ed.percent_val_arg ||
    (dummy_idx != NULL_IDX && ATD_VALUE_ATTR(dummy_idx));
}
#endif /* KEY Bug 14150 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do all the call site processing for the call list. This includes      *|
|*      copy in/copy out for array arguments, copy in for expressions and     *|
|*      passing the correct actual arg(target, address, dope vector).         *|
|*									      *|
|* Input parameters:							      *|
|*	list_opnd	- call list.                                          *|
|*      spec_idx	- attr idx for callee.                                *|
|*      num_args 	- num args.                                           *|
|*      elemental_exp_desc -                                                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
boolean final_arg_work(opnd_type	*list_opnd,
                       int		spec_idx,
                       int		num_args,
		       expr_arg_type	*elemental_exp_desc)

{
   act_arg_type        a_type;
   int		       addr_tmp_idx;
   int		       asg_idx;
   int		       association;
#ifdef KEY /* Bug 10177 */
   int		       attr_idx = 0;
#else /* KEY Bug 10177 */
   int		       attr_idx;
#endif /* KEY Bug 10177 */
   opnd_type	       base_opnd;
   long64	       char_len;
   char		      *char_ptr1;
   char		      *char_ptr2;
   int		       col;
   dummy_arg_type      d_type;
   int                 debug_count;
   int		       dummy;
   int		       dummy_idx;
   opnd_type	       dv_opnd;
   expr_arg_type       exp_desc;
   boolean	       explicit;
   int		       false_list_idx = NULL_IDX;
   int		       false_parm_idx;
   int		       fcd_idx;
   int		       i;	/* this is the main loop index */
   int		       info_idx;
   int		       intent;
   boolean	       io_call = FALSE;
   int		       ir_idx;
   long		       k; 	/* available for small loop index */
   opnd_type	       len_opnd;
   int		       line;
   int		       list_idx;
   int		       list_idx2;
   opnd_type           l_opnd;
   int		       mult_idx;
   boolean             ok = TRUE;
   int		       old_cn_idx;
   opnd_type           opnd;
   int		       opnd_column;
   int		       opnd_line;
   int		       present_idx;
   opnd_type           r_opnd;
   int		       save_defer_stmt_expansion;
   expr_mode_type      save_expr_mode;
   cif_usage_code_type save_xref_state;
   long_type	       the_constant;
   int		       tmp_idx;
   int		       tmp_dv_idx;
   int		       tmp_loc_idx;
   int		       unused1;
   int		       unused2;
   int		       zero_constant_idx;
   int                 false_start_sh_idx;
   int                 false_end_sh_idx;
   int                 true_start_sh_idx;
   int                 true_end_sh_idx;
   int                 true_start_sh_idx2;
   int                 true_end_sh_idx2;
   opnd_type           cond_opnd;
# ifdef KEY
   int                 dim;
# endif


   TRACE (Func_Entry, "final_arg_work", NULL);

   exp_desc = init_exp_desc;

   save_expr_mode = expr_mode;

   expr_mode = Regular_Expr;

   explicit = ATP_EXPL_ITRFC(spec_idx);
   dummy = NULL_IDX;

   zero_constant_idx = (SA_INTEGER_DEFAULT_TYPE == CG_INTEGER_DEFAULT_TYPE) ?
                                   CN_INTEGER_ZERO_IDX : 
                                   C_INT_TO_CN(SA_INTEGER_DEFAULT_TYPE, 0);

   if (ATP_ELEMENTAL(spec_idx)) {
      list_idx = OPND_IDX((*list_opnd));

      if (ATP_EXTRA_DARG(spec_idx)) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
#ifdef KEY /* Bug 11046 */
      ok = check_elemental_conformance(list_idx, &exp_desc, spec_idx);
#else /* KEY Bug 11046 */
      ok = check_elemental_conformance(list_idx, &exp_desc);
#endif /* KEY Bug 11046 */
      explicit = FALSE;
      
      if (elemental_exp_desc != NULL) {
         elemental_exp_desc->rank = exp_desc.rank;
         COPY_SHAPE(elemental_exp_desc->shape, exp_desc.shape, exp_desc.rank);
      }
   }

# ifdef _DEBUG
   if (explicit) {
      /* check that number of args matches number of dargs */

      debug_count = 0;
      list_idx = OPND_IDX((*list_opnd));

      while (list_idx != NULL_IDX) {
         if (! IL_INTRIN_PLACE_HOLDER(list_idx)) {
            debug_count++;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      if (debug_count != ATP_NUM_DARGS(spec_idx)) {
         PRINTMSG(stmt_start_line, 1119, Internal, stmt_start_col,
                  AT_OBJ_NAME_PTR(spec_idx));
      }
   }
# endif


   if (spec_idx == glb_tbl_idx[Buffer_In_Attr_Idx] ||
       spec_idx == glb_tbl_idx[Buffer_Out_Attr_Idx] ||
       spec_idx == glb_tbl_idx[Close_Attr_Idx] ||
       spec_idx == glb_tbl_idx[Backspace_Attr_Idx] ||
       spec_idx == glb_tbl_idx[Rewind_Attr_Idx] ||
       spec_idx == glb_tbl_idx[Endfile_Attr_Idx] ||
       spec_idx == glb_tbl_idx[Inquire_Attr_Idx] ||
       spec_idx == glb_tbl_idx[Open_Attr_Idx]) {

      io_call = TRUE;
   }

   list_idx = OPND_IDX((*list_opnd));

   if (!ATP_EXPL_ITRFC(spec_idx) &&         /* Global semantics */
       !io_call && 
       !AT_COMPILER_GEND(spec_idx)) {

      /* Make sure this is not defined in this program unit and that */
      /* it is not an intrinsic.  Also get rid of stuff like _END    */

      check_call_for_global_def(list_idx, spec_idx, num_args);
   }

   if (ATP_EXTRA_DARG(spec_idx)) {
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   dummy_idx = 1;

   for (i = 1; i <= num_args; i++) {

      stmt_expansion_control_start();
      save_defer_stmt_expansion = defer_stmt_expansion;
      defer_stmt_expansion = FALSE;

      if (IL_FLD(list_idx) == NO_Tbl_Idx &&
          ATP_PROC(spec_idx) == Intrin_Proc &&
          IL_INTRIN_PLACE_HOLDER(list_idx)) {

         /* replace with zero */

         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = zero_constant_idx;
         IL_LINE_NUM(list_idx) = stmt_start_line;
         IL_COL_NUM(list_idx)  = stmt_start_col;

         /* don't advance to the next dummy argument */
         /* this is for intrinsic processing         */

         goto EXIT;
      }

      if (explicit) {
         if (ATP_EXTRA_DARG(spec_idx)) {
            dummy    = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + dummy_idx);
         }
         else {
            dummy    = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + dummy_idx - 1);
         }
      }

      if (IL_FLD(list_idx) == NO_Tbl_Idx) {

         /* replace with zero */
       
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (explicit &&
             ! io_call &&
             (AT_OBJ_CLASS(dummy) == Data_Obj &&
               ! ATD_IM_A_DOPE(dummy)          &&
               (ATP_PROC(spec_idx) == Intrin_Proc ?
                 (((1 << Character_1) & ATD_INTRIN_DARG_TYPE(dummy)) != 0) :
                 TYP_TYPE(ATD_TYPE_IDX(dummy)) == Character))) 
# else
         if (explicit &&
             ! io_call &&
             ((AT_OBJ_CLASS(dummy) == Data_Obj &&
               ! ATD_IM_A_DOPE(dummy)          &&
               (ATP_PROC(spec_idx) == Intrin_Proc ?
                 (((1 << Character_1) & ATD_INTRIN_DARG_TYPE(dummy)) != 0) :
                 TYP_TYPE(ATD_TYPE_IDX(dummy)) == Character)) ||
              (AT_OBJ_CLASS(dummy) == Pgm_Unit &&
               ATP_PGM_UNIT(dummy) == Function &&
               TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(dummy))) == Character))) 
# endif
									  {

            /* create a zero fcd */

            NTR_IR_TBL(fcd_idx);
            IR_OPR(fcd_idx) = Fcd_Opr;
            IR_TYPE_IDX(fcd_idx) = CRI_Ch_Ptr_8;
            IR_LINE_NUM(fcd_idx) = stmt_start_line;
            IR_COL_NUM(fcd_idx)  = stmt_start_col;

            NTR_IR_LIST_TBL(list_idx2);
            IR_FLD_L(fcd_idx) = IL_Tbl_Idx;
            IR_IDX_L(fcd_idx) = list_idx2;
            IR_LIST_CNT_L(fcd_idx) = 2;


            IL_FLD(list_idx2) = CN_Tbl_Idx;
            IL_IDX(list_idx2) = zero_constant_idx;
            IL_LINE_NUM(list_idx2) = stmt_start_line;
            IL_COL_NUM(list_idx2)  = stmt_start_col;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
            list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

            IL_FLD(list_idx2) = CN_Tbl_Idx;
            IL_IDX(list_idx2) = zero_constant_idx;
            IL_LINE_NUM(list_idx2) = stmt_start_line;
            IL_COL_NUM(list_idx2)  = stmt_start_col;

            IL_FLD(list_idx) = IR_Tbl_Idx;
            IL_IDX(list_idx) = fcd_idx;

         }
         else {
#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
           NTR_IR_TBL(fcd_idx);
           IR_OPR(fcd_idx) = Aloc_Opr;
           IR_TYPE_IDX(fcd_idx) = CRI_Ptr_8;
           IR_LINE_NUM(fcd_idx) = stmt_start_line;
           IR_COL_NUM(fcd_idx)  = stmt_start_col;

           IR_FLD_L(fcd_idx) = CN_Tbl_Idx;
           IR_IDX_L(fcd_idx) = zero_constant_idx;
           IR_LINE_NUM_L(fcd_idx) = stmt_start_line;
           IR_COL_NUM_L(fcd_idx)  = stmt_start_col;
           
           IL_FLD(list_idx) = IR_Tbl_Idx;
           IL_IDX(list_idx) = fcd_idx;
#else
           IL_FLD(list_idx) = CN_Tbl_Idx;
           IL_IDX(list_idx) = zero_constant_idx;
           IL_LINE_NUM(list_idx) = stmt_start_line;
           IL_COL_NUM(list_idx)  = stmt_start_col;
#endif
         }

         dummy_idx++;
         goto EXIT;
      }

      info_idx = IL_ARG_DESC_IDX(list_idx);

# ifdef _DEBUG
      if (info_idx == NULL_IDX) {
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "valid info_idx", "final_arg_work");
      }
# endif

      COPY_OPND(opnd, IL_OPND(list_idx));
      check_for_constructors(&opnd, 
                  (info_idx != NULL_IDX ? &(arg_info_list[info_idx].ed):
                                               NULL));
      COPY_OPND(IL_OPND(list_idx), opnd);

      if (OPND_FLD(opnd) == AT_Tbl_Idx ||
          (OPND_FLD(opnd) == IR_Tbl_Idx &&
           (IR_OPR(OPND_IDX(opnd)) == Subscript_Opr         ||
            IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr   ||
            IR_OPR(OPND_IDX(opnd)) == Section_Subscript_Opr ||
            IR_OPR(OPND_IDX(opnd)) == Substring_Opr         ||
            IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr   ||
            IR_OPR(OPND_IDX(opnd)) == Struct_Opr            ||
            IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr))) {

         attr_idx = find_left_attr(&opnd);

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             ATD_CLASS(attr_idx) == Compiler_Tmp) {
            arg_info_list[info_idx].ed.tmp_reference = TRUE;
            arg_info_list[info_idx].ed.reference = FALSE;
         }
         else if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            arg_info_list[info_idx].ed.reference = TRUE;
            arg_info_list[info_idx].ed.tmp_reference = FALSE;
         }
      }

      if (arg_info_list[info_idx].ed.label)   {

         if (stmt_type == Call_Stmt) {
            /* change label ref to zero constant */

            IL_FLD(list_idx) = CN_Tbl_Idx;
            IL_IDX(list_idx) = zero_constant_idx;
            IL_LINE_NUM(list_idx) = stmt_start_line;
            IL_COL_NUM(list_idx)  = stmt_start_col;
         }
         else {
            /* this is io alternate return */
            /* they expect a -1 address, not address of -1 */
            IL_FLD(list_idx) = CN_Tbl_Idx;
            IL_IDX(list_idx) = CN_INTEGER_NEG_ONE_IDX;
            IL_LINE_NUM(list_idx) = stmt_start_line;
            IL_COL_NUM(list_idx)  = stmt_start_col;
         }

         dummy_idx++;
         goto EXIT;
      }

      if (arg_info_list[info_idx].pgm_unit) {
         set_at_actual_arg(list_idx);

         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_column);

         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             !AT_IS_INTRIN(IL_IDX(list_idx)) &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit &&
             ATP_ELEMENTAL(IL_IDX(list_idx))) {
            PRINTMSG(opnd_line, 1639, Error,
                     opnd_column,
                     AT_OBJ_NAME_PTR(IL_IDX(list_idx)));

            ok = FALSE;
         }


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)      = Aloc_Opr;
         IR_TYPE_IDX(ir_idx) = CRI_Parcel_Ptr_8;
         IR_LINE_NUM(ir_idx) = opnd_line;
         IR_COL_NUM(ir_idx)  = opnd_column;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx));
         IL_FLD(list_idx)    = IR_Tbl_Idx;
         IL_IDX(list_idx)    = ir_idx;
# else
         if (arg_info_list[info_idx].ed.type == Character) {

            NTR_IR_TBL(fcd_idx);
            IR_OPR(fcd_idx) = Fcd_Opr;
            IR_TYPE_IDX(fcd_idx) = CRI_Ch_Ptr_8;
            IR_LINE_NUM(fcd_idx) = opnd_line;
            IR_COL_NUM(fcd_idx)  = opnd_column;

            NTR_IR_LIST_TBL(list_idx2);
            IR_FLD_L(fcd_idx) = IL_Tbl_Idx;
            IR_IDX_L(fcd_idx) = list_idx2;
            IR_LIST_CNT_L(fcd_idx) = 2;

            
            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)      = Aloc_Opr;
            IR_TYPE_IDX(ir_idx) = CRI_Parcel_Ptr_8;
            COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx));
            IR_LINE_NUM(ir_idx) = opnd_line;
            IR_COL_NUM(ir_idx)  = opnd_column;
            IL_FLD(list_idx2)   = IR_Tbl_Idx;
            IL_IDX(list_idx2)   = ir_idx;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
            list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

            IL_FLD(list_idx2) = TYP_FLD(arg_info_list[info_idx].ed.type_idx);
            IL_IDX(list_idx2) = TYP_IDX(arg_info_list[info_idx].ed.type_idx);
            IL_LINE_NUM(list_idx2) = opnd_line;
            IL_COL_NUM(list_idx2)  = opnd_column;

            IL_FLD(list_idx) = IR_Tbl_Idx;
            IL_IDX(list_idx) = fcd_idx;
         }
         else {
            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)		= Aloc_Opr;
            IR_TYPE_IDX(ir_idx)	= CRI_Parcel_Ptr_8;
            IR_LINE_NUM(ir_idx) = opnd_line;
            IR_COL_NUM(ir_idx)  = opnd_column;
            COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx));
            IL_FLD(list_idx)	= IR_Tbl_Idx;
            IL_IDX(list_idx)	= ir_idx;
         }
# endif

         dummy_idx++;
         goto EXIT;
      }

      if (explicit) {
         /* place checks here that need to be done after generic resolution */

         if (
#ifdef KEY /* Bug 6845 */
	    /* allocatable dummy requires allocatable actual */
	    (ATD_ALLOCATABLE(dummy) &&
	     ! arg_info_list[info_idx].ed.allocatable) ||
#endif /* KEY Bug 6845 */
	    (ATD_POINTER(dummy)                    &&
             ! arg_info_list[info_idx].ed.pointer)) {

            /* pointer dummy requires pointer actual */

            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &opnd_line,
                                      &opnd_column);
            PRINTMSG(opnd_line, 256, Error,
                     opnd_column,
                     AT_OBJ_NAME_PTR(dummy));

            ok = FALSE;
         }



         if (arg_info_list[info_idx].ed.assumed_size             &&
             ATD_ARRAY_IDX(dummy)                                &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(dummy)) == Assumed_Shape) {

            /* assumed size array to an assumed shape dummy array */

            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &opnd_line,
                                      &opnd_column);

            PRINTMSG(opnd_line, 258, Error, opnd_column);

            ok = FALSE;
         }

         /* if INTENT(out) or INTENT(inout), actual must be */
         /* defineable.                                     */

         if (ATD_INTENT(dummy) == Intent_Out ||
              ATD_INTENT(dummy) == Intent_Inout) {

            if (arg_info_list[info_idx].ed.vector_subscript ||
                ! arg_info_list[info_idx].ed.reference) {
               find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                         &opnd_line,
                                         &opnd_column);

               PRINTMSG(opnd_line, 786, Error, opnd_column);
               ok = FALSE;
            }
            else {
               COPY_OPND(opnd, IL_OPND(list_idx));
               attr_idx = find_left_attr(&opnd);

               if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                   ATD_CLASS(attr_idx) == Dummy_Argument &&
                   ATD_INTENT(attr_idx) == Intent_In) {

                  find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                            &opnd_line,
                                            &opnd_column);
                  PRINTMSG(opnd_line, 786, Error, opnd_column);

                  ok = FALSE;
               }
            }
         }

#ifdef KEY /* Bug 14110 */
         if (ATD_VOLATILE(dummy)) {
            boolean constraint_error = FALSE;
            /* Mentioned in 12.1.4.2 */
            if (arg_info_list[info_idx].ed.vector_subscript) {
              constraint_error = TRUE;
            }
            /* Constraint C1233 */
            else if (arg_info_list[info_idx].ed.pointer) {
              if (!(BD_ARRAY_CLASS(ATD_ARRAY_IDX(dummy)) == Assumed_Shape ||
                ATD_POINTER(dummy))) {
                constraint_error = TRUE;
              }
            }
            /* Constraint C1232 */
            else if (arg_info_list[info_idx].ed.section  ||
              arg_info_list[info_idx].ed.assumed_shape) {
              if (!(ATD_ARRAY_IDX(dummy) &&
                BD_ARRAY_CLASS(ATD_ARRAY_IDX(dummy)) == Assumed_Shape)) {
                constraint_error = TRUE;
              }
            }
            if (constraint_error) {
               find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                 &opnd_line, &opnd_column);
               PRINTMSG(opnd_line, 1688, Error, opnd_column,
                 AT_OBJ_NAME_PTR(dummy));
            }
         }
#endif /* KEY Bug 14110 */

         if (arg_info_list[info_idx].ed.type == Character &&
             ATP_PROC(spec_idx) != Intrin_Proc            &&
             AT_OBJ_CLASS(dummy) == Data_Obj              &&
             ATD_ARRAY_IDX(dummy) == NULL_IDX             &&
             TYP_TYPE(ATD_TYPE_IDX(dummy)) == Character) {

            if (TYP_FLD(arg_info_list[info_idx].ed.type_idx) == CN_Tbl_Idx &&
                TYP_FLD(ATD_TYPE_IDX(dummy)) == CN_Tbl_Idx &&
                fold_relationals(TYP_IDX(arg_info_list[info_idx].ed.type_idx),
                                 TYP_IDX(ATD_TYPE_IDX(dummy)),
                                 Lt_Opr)) {

               find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                         &opnd_line,
                                         &opnd_column);

               PRINTMSG(opnd_line, 1205, Error, opnd_column,
                        AT_OBJ_NAME_PTR(dummy));
               ok = FALSE;
            }
         }

         /* check for array/character size overlap */

         if (TYP_TYPE(ATD_TYPE_IDX(dummy)) == Character &&
             TYP_FLD(ATD_TYPE_IDX(dummy)) == CN_Tbl_Idx &&
             ATD_ARRAY_IDX(dummy) != NULL_IDX &&
             BD_LEN_FLD(ATD_ARRAY_IDX(dummy)) == CN_Tbl_Idx &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(dummy)) == Explicit_Shape &&
             arg_info_list[info_idx].ed.rank != 0) {

            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &opnd_line,
                                      &opnd_column);

            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc = arg_info_list[info_idx].ed;
            ok &= validate_char_len(&opnd, &exp_desc);
            arg_info_list[info_idx].ed = exp_desc;

            OPND_FLD(len_opnd) = TYP_FLD(exp_desc.type_idx);
            OPND_IDX(len_opnd) = TYP_IDX(exp_desc.type_idx);

            for (k = 0; k < arg_info_list[info_idx].ed.rank; k++) {
               NTR_IR_TBL(mult_idx);
               IR_OPR(mult_idx) = Mult_Opr;
               IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(mult_idx) = opnd_line;
               IR_COL_NUM(mult_idx)  = opnd_column;

               COPY_OPND(IR_OPND_L(mult_idx), len_opnd);
  
               COPY_OPND(IR_OPND_R(mult_idx),
                         arg_info_list[info_idx].ed.shape[k]);
  
               OPND_FLD(len_opnd) = IR_Tbl_Idx;
               OPND_IDX(len_opnd) = mult_idx;
            }

            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx) = Mult_Opr;
            IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx) = opnd_line;
            IR_COL_NUM(mult_idx)  = opnd_column;

            IR_FLD_L(mult_idx) = BD_LEN_FLD(ATD_ARRAY_IDX(dummy));
            IR_IDX_L(mult_idx) = BD_LEN_IDX(ATD_ARRAY_IDX(dummy));
            IR_LINE_NUM_L(mult_idx) = opnd_line;
            IR_COL_NUM_L(mult_idx)  = opnd_column;

            IR_FLD_R(mult_idx) = TYP_FLD(ATD_TYPE_IDX(dummy));
            IR_IDX_R(mult_idx) = TYP_IDX(ATD_TYPE_IDX(dummy));
            IR_LINE_NUM_R(mult_idx) = opnd_line;
            IR_COL_NUM_R(mult_idx)  = opnd_column;

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx) = Gt_Opr;
            IR_TYPE_IDX(ir_idx) = CG_LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx) = opnd_line;
            IR_COL_NUM(ir_idx)  = opnd_column;

            IR_FLD_L(ir_idx) = IR_Tbl_Idx;
            IR_IDX_L(ir_idx) = mult_idx;
            IR_LINE_NUM_L(ir_idx) = opnd_line;
            IR_COL_NUM_L(ir_idx)  = opnd_column;

            COPY_OPND(IR_OPND_R(ir_idx), len_opnd);

            OPND_FLD(len_opnd) = IR_Tbl_Idx;
            OPND_IDX(len_opnd) = ir_idx;

            save_xref_state = xref_state;
            xref_state      = CIF_No_Usage_Rec;
            save_expr_mode  = expr_mode;
            expr_mode       = Regular_Expr;

            exp_desc.rank   = 0;
            ok = expr_semantics(&len_opnd, &exp_desc);
            xref_state = save_xref_state;
            expr_mode  = save_expr_mode;

            if (OPND_FLD(len_opnd) == CN_Tbl_Idx &&
                THIS_IS_TRUE((&CN_CONST(OPND_IDX(len_opnd))), 
                              exp_desc.type_idx)) {

               PRINTMSG(opnd_line, 1500, Error, opnd_column);
               ok = FALSE;
            }
         }
         else if (ATD_ARRAY_IDX(dummy) != NULL_IDX &&
                  BD_LEN_FLD(ATD_ARRAY_IDX(dummy)) == CN_Tbl_Idx &&
                  BD_ARRAY_CLASS(ATD_ARRAY_IDX(dummy)) == Explicit_Shape &&
                  arg_info_list[info_idx].ed.rank != 0) {

            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &opnd_line,
                                      &opnd_column);

            COPY_OPND(len_opnd, arg_info_list[info_idx].ed.shape[0]);

            for (k = 1; k < arg_info_list[info_idx].ed.rank; k++) {
               NTR_IR_TBL(mult_idx);
               IR_OPR(mult_idx) = Mult_Opr;
               IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(mult_idx) = opnd_line;
               IR_COL_NUM(mult_idx)  = opnd_column;

               COPY_OPND(IR_OPND_L(mult_idx), len_opnd);
   
               COPY_OPND(IR_OPND_R(mult_idx),
                         arg_info_list[info_idx].ed.shape[k]);
   
               OPND_FLD(len_opnd) = IR_Tbl_Idx;
               OPND_IDX(len_opnd) = mult_idx;
            }

            save_xref_state = xref_state;
            xref_state      = CIF_No_Usage_Rec;
            save_expr_mode  = expr_mode;
            expr_mode       = Regular_Expr;

            exp_desc.rank   = 0;
            ok = expr_semantics(&len_opnd, &exp_desc);
            xref_state = save_xref_state;
            expr_mode  = save_expr_mode;

            if (OPND_FLD(len_opnd) == CN_Tbl_Idx &&
                fold_relationals(BD_LEN_IDX(ATD_ARRAY_IDX(dummy)),
                                 OPND_IDX(len_opnd),
                                 Gt_Opr)) {
               PRINTMSG(opnd_line, 1500, Error, opnd_column);
               ok = FALSE;
            }
         }
      }

      if (explicit &&
          arg_info_list[info_idx].ed.linear_type == Short_Typeless_Const &&
          (TYP_TYPE(ATD_TYPE_IDX(dummy)) == Integer ||
           TYP_TYPE(ATD_TYPE_IDX(dummy)) == Real ||
           TYP_TYPE(ATD_TYPE_IDX(dummy)) == Complex)) {

         IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                ATD_TYPE_IDX(dummy),
                                arg_info_list[info_idx].line,
                                arg_info_list[info_idx].col);

         arg_info_list[info_idx].ed.type_idx = ATD_TYPE_IDX(dummy);
         arg_info_list[info_idx].ed.type = TYP_TYPE(ATD_TYPE_IDX(dummy));
         arg_info_list[info_idx].ed.linear_type = 
                                         TYP_LINEAR(ATD_TYPE_IDX(dummy));
      }
      else if (explicit &&
               arg_info_list[info_idx].ed.linear_type ==
                                          Short_Typeless_Const &&
               (CN_HOLLERITH_TYPE(IL_IDX(list_idx)) == H_Hollerith ||
                CN_HOLLERITH_TYPE(IL_IDX(list_idx)) == L_Hollerith) &&
               TYP_TYPE(ATD_TYPE_IDX(dummy)) == Character) {

         old_cn_idx = IL_IDX(list_idx);

         char_len = strlen((char *)&CN_CONST(IL_IDX(list_idx)));

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

         TYP_TYPE(TYP_WORK_IDX)       = Character;
         TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)        = CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)	      = C_INT_TO_CN(NULL_IDX,
                                                    char_len);
         arg_info_list[info_idx].ed.type_idx = ntr_type_tbl();
         arg_info_list[info_idx].ed.type = Character;
         arg_info_list[info_idx].ed.linear_type = Short_Char_Const;
         arg_info_list[info_idx].ed.char_len.fld = 
                   TYP_FLD(arg_info_list[info_idx].ed.type_idx);
         arg_info_list[info_idx].ed.char_len.idx = 
                   TYP_IDX(arg_info_list[info_idx].ed.type_idx);

         /* Set up the new const table entry.  Pass ntr_const_tbl */
         /* a null pointer so the caller can move the constant.   */

         IL_IDX(list_idx) = ntr_const_tbl(arg_info_list[info_idx].ed.type_idx,
                                          TRUE,
                                          NULL);

         for (k = 0; k < TARGET_BYTES_TO_WORDS(char_len); k++) {
            CP_CONSTANT(CN_POOL_IDX(IL_IDX(list_idx)) + k) = 
                  CP_CONSTANT(CN_POOL_IDX(old_cn_idx) + k);
         }
      }
      else if (! explicit &&
              arg_info_list[info_idx].ed.linear_type == Short_Typeless_Const &&
               CN_HOLLERITH_TYPE(IL_IDX(list_idx)) == Not_Hollerith) {

         IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                INTEGER_DEFAULT_TYPE,
                                arg_info_list[info_idx].line,
                                arg_info_list[info_idx].col);

         arg_info_list[info_idx].ed.type_idx = INTEGER_DEFAULT_TYPE;
         arg_info_list[info_idx].ed.type = TYP_TYPE(INTEGER_DEFAULT_TYPE);
         arg_info_list[info_idx].ed.linear_type =
                                         TYP_LINEAR(INTEGER_DEFAULT_TYPE);
      }


      if (arg_info_list[info_idx].ed.reference          &&
          ! arg_info_list[info_idx].ed.vector_subscript &&
          arg_info_list[info_idx].maybe_modified)       {

         set_at_actual_arg(list_idx);
      }


      d_type = get_dummy_arg_type(dummy);

      if (dummy != NULL_IDX &&
          AT_OBJ_CLASS(dummy) == Data_Obj &&
#ifdef KEY /* Bug 14150 */
	  /* Prior to 14150, front end didn't allow ignore_tkr on a pointer,
	   * so we can't break any existing code by passing the descriptor,
	   * which is what we need for ISO_C_BINDING c_f_pointer. */
          (!ATD_POINTER(dummy)) &&
#endif /* KEY Bug 14150 */
          ATD_IGNORE_TKR(dummy)) {

          d_type = Unknown_Dummy;
      }

      if (IL_FLD(list_idx) == IR_Tbl_Idx &&
          IR_OPR(IL_IDX(list_idx)) == Null_Intrinsic_Opr) {

         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                   &opnd_line,
                                   &opnd_column);

         if (dummy != NULL_IDX &&
             AT_OBJ_CLASS(dummy) == Data_Obj) {
            tmp_dv_idx = gen_compiler_tmp(opnd_line, 
                                          opnd_column, 
                                          Priv, 
                                          TRUE);
            ATD_TYPE_IDX(tmp_dv_idx) = ATD_TYPE_IDX(dummy);
            ATD_STOR_BLK_IDX(tmp_dv_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
            AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
            ATD_ARRAY_IDX(tmp_dv_idx) = ATD_ARRAY_IDX(dummy);
            ATD_POINTER(tmp_dv_idx) = TRUE;
            ATD_IM_A_DOPE(tmp_dv_idx) = TRUE;

            gen_opnd(&dv_opnd, 
                     tmp_dv_idx, 
                     AT_Tbl_Idx, 
                     opnd_line, 
                     opnd_column);
            gen_dv_whole_def_init(&dv_opnd,
                                  tmp_dv_idx,
                                  Before);

            if (ATD_ARRAY_IDX(dummy) == NULL_IDX) {
               arg_info_list[info_idx].ed.rank = 0;
            }
            else {
               arg_info_list[info_idx].ed.rank = BD_RANK(ATD_ARRAY_IDX(dummy));
            }
            arg_info_list[info_idx].ed.type_idx = ATD_TYPE_IDX(dummy);
            arg_info_list[info_idx].ed.type = TYP_TYPE(ATD_TYPE_IDX(dummy));
            arg_info_list[info_idx].ed.linear_type = 
                 TYP_LINEAR(ATD_TYPE_IDX(dummy));
            arg_info_list[info_idx].ed.pointer = TRUE;
            arg_info_list[info_idx].ed.tmp_reference = TRUE;

            gen_opnd(&dv_opnd,
                     gen_ir(AT_Tbl_Idx,
                            tmp_dv_idx,
                            Dv_Deref_Opr,
                            arg_info_list[info_idx].ed.type_idx,
                            opnd_line,
                            opnd_column,
                            NO_Tbl_Idx,
                            NULL_IDX),
                     IR_Tbl_Idx,
                     opnd_line,
                     opnd_column);

            if (arg_info_list[info_idx].ed.rank > 0) {
               ok = gen_whole_subscript(&dv_opnd, &arg_info_list[info_idx].ed);
            }

            IL_IDX(list_idx) = OPND_IDX(dv_opnd);
            IL_FLD(list_idx) = OPND_FLD(dv_opnd);
         }
         else {
            tmp_dv_idx = gen_compiler_tmp(opnd_line, 
                                          opnd_column, 
                                          Priv, 
                                          TRUE);
            ATD_TYPE_IDX(tmp_dv_idx) = TYPELESS_DEFAULT_TYPE;
            ATD_STOR_BLK_IDX(tmp_dv_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
            AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;
            ATD_ARRAY_IDX(tmp_dv_idx) = NULL_IDX;
            ATD_POINTER(tmp_dv_idx) = TRUE;
            ATD_IM_A_DOPE(tmp_dv_idx) = TRUE;

            gen_opnd(&dv_opnd, 
                     tmp_dv_idx, 
                     AT_Tbl_Idx, 
                     opnd_line, 
                     opnd_column);
            gen_dv_whole_def_init(&dv_opnd,
                                  tmp_dv_idx,
                                  Before);

            arg_info_list[info_idx].ed.rank = 0;
            arg_info_list[info_idx].ed.type_idx = TYPELESS_DEFAULT_TYPE;
            arg_info_list[info_idx].ed.linear_type =  
                     TYP_LINEAR(TYPELESS_DEFAULT_TYPE);
            arg_info_list[info_idx].ed.pointer = TRUE;
            arg_info_list[info_idx].ed.tmp_reference = TRUE;

            gen_opnd(&dv_opnd,
                     gen_ir(AT_Tbl_Idx,
                            tmp_dv_idx,
                            Dv_Deref_Opr,
                            arg_info_list[info_idx].ed.type_idx,
                            opnd_line,
                            opnd_column,
                            NO_Tbl_Idx,
                            NULL_IDX),
                     IR_Tbl_Idx,
                     opnd_line,
                     opnd_column);

            if (arg_info_list[info_idx].ed.rank > 0) {
               ok = gen_whole_subscript(&dv_opnd, &arg_info_list[info_idx].ed);
            }

            IL_IDX(list_idx) = OPND_IDX(dv_opnd);
            IL_FLD(list_idx) = OPND_FLD(dv_opnd);
         }
      }

      a_type = get_act_arg_type(&arg_info_list[info_idx].ed);

      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &line,
                                &col);


      if (! AT_IS_INTRIN(spec_idx)         &&
          dummy != NULL_IDX                &&
          AT_OBJ_CLASS(dummy) == Data_Obj)  {

         /* check for auxiliary match */

         if (ATD_AUXILIARY(dummy)) {

            if (arg_assoc_tbl[a_type][d_type] != PASS_ADDRESS) {
               PRINTMSG(line, 627, Warning, col, 
                        AT_OBJ_NAME_PTR(dummy));
            }
            else {
           
               COPY_OPND(opnd, IL_OPND(list_idx));
               attr_idx = find_left_attr(&opnd);

               if (! ATD_AUXILIARY(attr_idx)) {
                  PRINTMSG(line, 627, Warning, col, 
                           AT_OBJ_NAME_PTR(dummy));
               }
            }
         }
         else if (arg_assoc_tbl[a_type][d_type] == PASS_ADDRESS) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            attr_idx = find_left_attr(&opnd);

            if (ATD_AUXILIARY(attr_idx)) {
               PRINTMSG(line, 620, Warning, col,
                        AT_OBJ_NAME_PTR(dummy));
            }
         }
      }

      association = arg_assoc_tbl[a_type][d_type];

      if (association == PASS_ADDRESS_FROM_DV           &&
# ifdef _TRANSFORM_CHAR_SEQUENCE
          (arg_info_list[info_idx].ed.type == Character  ||
           (arg_info_list[info_idx].ed.type == Structure &&
            ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx)))))
# else
          arg_info_list[info_idx].ed.type == Character)
# endif
                                                         {

         association = PASS_ADDRESS;
      }
      else if (association == PASS_ADDRESS &&
               IL_FLD(list_idx) == AT_Tbl_Idx &&
               AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj &&
               ATD_LIVE_DO_VAR(IL_IDX(list_idx))) {

         /* you cannot modify a live do loop lcv, so make it a copy in */
         association = COPY_IN;
      }
#ifdef KEY /* Bug 6845 */
      /* If dummy is allocatable, actual must be allocatable, and we must pass
       * the dope vector itself, not a copy, so that intent(out) and
       * intent(inout) can change it. It's harmless to do this for intent(in)
       * as well (the Fortran standard requires the user not to modify such
       * an argument, but does not require the compiler to pass it by value)
       * and the optimizer works better without the copy. Note that we need
       * to set ERROR_ASSOC in case of error, because arg_assoc_tbl[][] (see
       * above) doesn't take care of that.
       */
      else if (explicit && NULL_IDX != dummy && ATD_ALLOCATABLE(dummy)) {  
	association = arg_info_list[info_idx].ed.allocatable ?
	  PASS_DV :
	  ERROR_ASSOC;
      }
#endif /* KEY Bug 6845 */
#ifdef KEY /* Bug 10157 */
      /* Optimizer works better if we don't copy the dope vector, but we
       * can't omit the copy if the dummy-argument dope vector requires a
       * different lower bound than the actual-argument dope vector. */
      else if (association == PASS_DV_COPY &&
        lower_bounds_match(list_idx, dummy)) {
	/* If we're going to call process_variable_size_func() either
	 * because the function result needs to be passed as the initial
	 * subroutine argument or because the function is elemental, then
	 * we can't omit the copy of the first argument. Not clear to me
	 * why process_variable_size_func() demands compiler temp for args
	 * besides the first (see PRINTMSG of 626), but it does. */
	if (! (FUNCTION_MUST_BE_SUBROUTINE(spec_idx, ATP_RSLT_IDX(spec_idx)) ||
	    ATP_ELEMENTAL(spec_idx))) {
	  association = PASS_DV;
	}
      }
#endif /* KEY Bug 10157 */

      if (arg_info_list[info_idx].ed.rank > 0 &&
          ATP_ELEMENTAL(spec_idx)) {

         if (association == PASS_ADDRESS_FROM_DV ||
             association == CHECK_CONTIG_FLAG ||
             association == COPY_IN_COPY_OUT ||
             association == PASS_SECTION_ADDRESS) {

            association = PASS_ADDRESS;
         }
      }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (association == CHECK_CONTIG_FLAG &&
          arg_info_list[info_idx].ed.type == Character) {

         association = COPY_IN_COPY_OUT;
      }
# endif

# if defined(_F_MINUS_MINUS)
      if (dummy != NULL_IDX &&
          AT_OBJ_CLASS(dummy) == Data_Obj &&
          ATD_PE_ARRAY_IDX(dummy) != NULL_IDX) {

         if (arg_info_list[info_idx].ed.reference) {
            attr_idx = find_left_attr(&IL_OPND(list_idx));

            if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                ATD_PE_ARRAY_IDX(attr_idx) == NULL_IDX) {

               PRINTMSG(line, 1584, Error, col);
            }
         }
         else {
            PRINTMSG(line, 1584, Error, col);
         }
      }
# endif
//Bug 3230
# if  defined(KEY)
      char dim[MAX_DIMENSION];
      bzero(dim, sizeof(dim));
      /* Bug 5186: If LANG_Copy_Inout set and LANG_Copy_Inout_Level > 0, then
       * don't require the call to be inside a loop. */
      if (LANG_Copy_Inout &&
          (a_type == Dv_Array_Section ||
           a_type == Sequence_Array_Section) &&
          d_type == Assumed_Shape_Dummy &&
          !ATD_ALLOCATABLE(attr_idx) &&
          !ATD_POINTER(attr_idx) &&  
          arg_info_list[info_idx].ed.rank > 0 &&
          stride_access_greater_than_1(&opnd, dim) &&
	  ((LANG_Copy_Inout_Level > 0) ||
          inside_loop(SH_PREV_IDX(curr_stmt_sh_idx)))){
        association = COPY_INOUT_MAKE_DV;
      }
# endif
      arg_info_list[info_idx].association = association;

#ifdef KEY /* Bug 8117 */
      if (stop_recursion) {
	association = ERROR_ASSOC;
      }
#endif /* KEY Bug 8117 */
#ifdef KEY /* Bug 572 */
      /* constant pointer must be null, so user may not dereference it */
      if (arg_info_list[info_idx].ed.constant &&
        arg_info_list[info_idx].ed.pointer) {
	switch (association) {
	  case ERROR_ASSOC:
	  case PASS_DV:
	  case PASS_DV_COPY:
	     break;
	  default: {
	    int line_tmp, col_tmp;
	     find_opnd_line_and_column(&IL_OPND(list_idx), &line_tmp, &col_tmp);
	     PRINTMSG(line_tmp, 1677, Error, col_tmp);
	   }
	}
      }
#endif /* KEY Bug 572 */
      switch (association) {
         case ERROR_ASSOC          :
            break;

         case PASS_ADDRESS         :
#ifdef _DEBUG
	    pass_address_count += 1;
#endif /* _DEBUG */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 7424 */
            if (clear_pt_unique_mem(dummy)) {
#endif /* KEY Bug 7424 */
            ATD_NOT_PT_UNIQUE_MEM(
                   (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
#ifdef KEY /* Bug 7424 */
            }
#endif /* KEY Bug 7424 */
# endif

            if (!pass_by_value(spec_idx, info_idx, dummy)) {

               /* JBL - this is for array element change */

               if (arg_info_list[info_idx].ed.rank != 0 &&
                   ! ATP_ELEMENTAL(spec_idx)) {

# ifndef _FRONTEND_INLINER
                  if (IL_FLD(list_idx) == IR_Tbl_Idx) {

                     ir_idx = IL_IDX(list_idx);

                     if (IR_OPR(ir_idx) == Whole_Subscript_Opr) {
                        COPY_OPND(IL_OPND(list_idx), IR_OPND_L(ir_idx));
                     }
                  }
# endif

                  if ((IL_FLD(list_idx) == AT_Tbl_Idx &&
                       BD_ARRAY_CLASS(ATD_ARRAY_IDX(IL_IDX(list_idx))) == 
                                                               Assumed_Size) ||
                      (arg_info_list[info_idx].ed.type == Character &&
                       IR_FLD_L(IL_IDX(list_idx)) == AT_Tbl_Idx &&
                       BD_ARRAY_CLASS(ATD_ARRAY_IDX(IR_IDX_L( 
                                   IL_IDX(list_idx)))) == Assumed_Size)) {

                     if (arg_info_list[info_idx].ed.type == Character) {
                        COPY_OPND(opnd, IR_OPND_L(IL_IDX(list_idx)));
                        make_base_assumed_size(&opnd, &base_opnd);
                        COPY_OPND(IR_OPND_L(IL_IDX(list_idx)), base_opnd);
                     }
                     else {
                        COPY_OPND(opnd, IL_OPND(list_idx));
                        make_base_assumed_size(&opnd, &base_opnd);
                        COPY_OPND(IL_OPND(list_idx), base_opnd);
                     }

                     if (a_type == Whole_Sequence ||
                         a_type == Whole_Allocatable) {

                        if (OPND_FLD(base_opnd) == IR_Tbl_Idx &&
                            IR_OPR(OPND_IDX(base_opnd)) == Subscript_Opr) {
                           IR_WHOLE_ARRAY(OPND_IDX(base_opnd)) = TRUE;
                        }
                     }
                  }
                  else {

                     if (a_type == Whole_Sequence ||
                         a_type == Whole_Allocatable) {

                        COPY_OPND(opnd, IL_OPND(list_idx));

                        while (OPND_FLD(opnd) == IR_Tbl_Idx) {
                           
                           if (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr) {
                              IR_WHOLE_ARRAY(OPND_IDX(opnd)) = TRUE;
                              break;
                           }

                           COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
                        }
                     }

                     COPY_OPND(opnd, IL_OPND(list_idx));
                     unused1 = NULL_IDX;
                     unused2 = NULL_IDX;
                     make_base_subtree(&opnd, &base_opnd, &unused1, &unused2);
                     COPY_OPND(IL_OPND(list_idx), base_opnd);
                  }
               }

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)      = Aloc_Opr;
               IR_TYPE_IDX(ir_idx) = (arg_info_list[info_idx].ed.type == 
                                                                     Character)
                                                     ? CRI_Ch_Ptr_8 : CRI_Ptr_8;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;

               COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx));
               IL_FLD(list_idx) = IR_Tbl_Idx;
               IL_IDX(list_idx) = ir_idx;

# ifdef _TRANSFORM_CHAR_SEQUENCE
               if (arg_info_list[info_idx].ed.type == Structure &&
                   ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx))) {

                  COPY_OPND(opnd, IR_OPND_L(ir_idx));
                  transform_char_sequence_ref(&opnd,
                                  arg_info_list[info_idx].ed.type_idx);
                  COPY_OPND(IR_OPND_L(ir_idx), opnd);

                  IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
               }
# endif
            }

            break;

         case PASS_SECTION_ADDRESS :
#ifdef _DEBUG
            pass_section_address_count += 1;
#endif /* _DEBUG */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 7424 */
            if (clear_pt_unique_mem(dummy)) {
#endif /* KEY Bug 7424 */
            ATD_NOT_PT_UNIQUE_MEM(
                   (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
#ifdef KEY /* Bug 7424 */
            }
#endif /* KEY Bug 7424 */
# endif

            /* these are always arrays, no need for %val() or vfunction chks */

            COPY_OPND(opnd, IL_OPND(list_idx));
            unused1 = NULL_IDX;
            unused2 = NULL_IDX;
            make_base_subtree(&opnd, &base_opnd, &unused1, &unused2);
            COPY_OPND(opnd, base_opnd);

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)      = Aloc_Opr;
            IR_TYPE_IDX(ir_idx) = (arg_info_list[info_idx].ed.type ==
                                                                  Character)
                                                  ? CRI_Ch_Ptr_8 : CRI_Ptr_8;
            IR_LINE_NUM(ir_idx) = line;
            IR_COL_NUM(ir_idx)  = col;

            COPY_OPND(IR_OPND_L(ir_idx), opnd);
            IL_FLD(list_idx) = IR_Tbl_Idx;
            IL_IDX(list_idx) = ir_idx;

# ifdef _TRANSFORM_CHAR_SEQUENCE
            if (arg_info_list[info_idx].ed.type == Structure &&
                ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx))) {

               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               transform_char_sequence_ref(&opnd,
                               arg_info_list[info_idx].ed.type_idx);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);

               IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
            }
# endif

            break;

            

         case PASS_ADDRESS_FROM_DV :
#ifdef _DEBUG
            pass_address_from_dv_count += 1;
#endif /* _DEBUG */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 7424 */
            if (clear_pt_unique_mem(dummy)) {
#endif /* KEY Bug 7424 */
            ATD_NOT_PT_UNIQUE_MEM(
                   (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
#ifdef KEY /* Bug 7424 */
            }
#endif /* KEY Bug 7424 */
# endif


            if (!pass_by_value(spec_idx, info_idx, dummy)) {

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)   = Dv_Access_Base_Addr;
               IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;

               if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx)) == Whole_Substring_Opr) {
                  COPY_OPND(IL_OPND(list_idx), IR_OPND_L(IL_IDX(list_idx)));
               }

               if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(list_idx)) == Whole_Subscript_Opr) {
                  COPY_OPND(IL_OPND(list_idx), IR_OPND_L(IL_IDX(list_idx)));
               }

               if (IL_FLD(list_idx)         != IR_Tbl_Idx    ||
                   IR_OPR(IL_IDX(list_idx)) != Dv_Deref_Opr) {
   
                  PRINTMSG(line, 861, Internal, col,
                           "final_arg_work");
               }

               COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IL_IDX(list_idx)));

               if (cmd_line_flags.runtime_ptr_chk) {
                  COPY_OPND(opnd, IR_OPND_L(ir_idx));
                  gen_runtime_ptr_chk(&opnd);
               }
   
               IL_FLD(list_idx) = IR_Tbl_Idx;
               IL_IDX(list_idx) = ir_idx;
            }

            arg_info_list[info_idx].ed.dope_vector = FALSE;
            arg_info_list[info_idx].ed.pointer     = FALSE;
            
            break;

         case PASS_DV              :
#ifdef _DEBUG
            pass_dv_count += 1;
#endif /* _DEBUG */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 7424 */
            if (clear_pt_unique_mem(dummy)) {
#endif /* KEY Bug 7424 */
            ATD_NOT_PT_UNIQUE_MEM(
                   (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
#ifdef KEY /* Bug 7424 */
            }
#endif /* KEY Bug 7424 */
# endif

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)	= Aloc_Opr;
            IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
            IR_LINE_NUM(ir_idx) = line;
            IR_COL_NUM(ir_idx)  = col;

            if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                IR_OPR(IL_IDX(list_idx)) == Whole_Substring_Opr) {
               COPY_OPND(IL_OPND(list_idx), IR_OPND_L(IL_IDX(list_idx)));
            }

            if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                IR_OPR(IL_IDX(list_idx)) == Whole_Subscript_Opr) {
               COPY_OPND(IL_OPND(list_idx), IR_OPND_L(IL_IDX(list_idx)));
            }

            if (IL_FLD(list_idx)         != IR_Tbl_Idx    ||
                IR_OPR(IL_IDX(list_idx)) != Dv_Deref_Opr) {

               PRINTMSG(line, 861, Internal, col,
                        "final_arg_work");
            }

            COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_L(IL_IDX(list_idx)));

            IL_FLD(list_idx) = IR_Tbl_Idx;
            IL_IDX(list_idx) = ir_idx;

            break;

         case PASS_DV_COPY         :
#ifdef _DEBUG
            pass_dv_copy_count += 1;
#endif /* _DEBUG */

            if (AT_OPTIONAL(dummy) &&
                arg_info_list[info_idx].ed.optional_darg) {

               /* generate if stmt for present(arg) */

               COPY_OPND(opnd, IL_OPND(list_idx));
               present_idx = gen_ir(AT_Tbl_Idx, find_left_attr(&opnd),
                                Present_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                                    NO_Tbl_Idx, NULL_IDX);

               gen_opnd(&cond_opnd, 
                        present_idx, 
                        IR_Tbl_Idx, 
                        line, 
                        col);

               /* save the bounding stmts. these are moved in later. */
               true_start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               true_end_sh_idx = curr_stmt_sh_idx;

               /* put work here */

               tmp_dv_idx = create_tmp_DV_asg(list_idx, info_idx);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 10157 */
 /* Original dope vector is used only in caller, and copy of dope vector is
  * used only in called procedure, so leave "pointer to unique memory"
  * indicator set so that optimizer is free to work. */
   if ( ATD_NOT_PT_UNIQUE_MEM((find_left_attr(&IL_OPND(list_idx)))))
#endif /* KEY Bug 10157 */
              {
               ATD_NOT_PT_UNIQUE_MEM(tmp_dv_idx) = TRUE;
               ATD_NOT_PT_UNIQUE_MEM(
                     (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
              }
#endif

               /* gen loc tmp = loc(tmp_dv_idx) */

               GEN_COMPILER_TMP_ASG(asg_idx,
                                    tmp_loc_idx,
                                    TRUE,   /* Semantics done */
                                    line,
                                    col,
                                    SA_INTEGER_DEFAULT_TYPE,
                                    Priv);
              
               ir_idx = gen_ir(AT_Tbl_Idx, tmp_dv_idx,
                           Loc_Opr, CRI_Ptr_8, line, col,
                               NO_Tbl_Idx, NULL_IDX);

               IR_FLD_R(asg_idx)            = IR_Tbl_Idx;
               IR_IDX_R(asg_idx)            = ir_idx;
               IR_LINE_NUM_R(asg_idx)       = line;
               IR_COL_NUM_R(asg_idx)        = col;

               gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

               true_start_sh_idx = SH_NEXT_IDX(true_start_sh_idx);
               true_end_sh_idx = SH_PREV_IDX(true_end_sh_idx);

               /* gen the loc tmp = 0 stmt. */

               asg_idx = gen_ir(AT_Tbl_Idx, tmp_loc_idx,
                            Asg_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

               false_start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               false_end_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

               gen_if_stmt(&cond_opnd, 
                           true_start_sh_idx,
                           true_end_sh_idx,
                           false_start_sh_idx,
                           false_end_sh_idx,
                           line,
                           col);

               /* put loc tmp into arg */

               IL_FLD(list_idx) = AT_Tbl_Idx;
               IL_IDX(list_idx) = tmp_loc_idx;
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx)  = col;
            }
            else {

               tmp_dv_idx = create_tmp_DV_asg(list_idx, info_idx);


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 10157 */
 /* Original dope vector is used only in caller, and copy of dope vector is
  * used only in called procedure, so leave "pointer to unique memory"
  * indicator set so that optimizer is free to work. */
   if ( ATD_NOT_PT_UNIQUE_MEM((find_left_attr(&IL_OPND(list_idx)))))
#endif /* KEY Bug 10157 */
              {
               ATD_NOT_PT_UNIQUE_MEM(tmp_dv_idx) = TRUE;
               ATD_NOT_PT_UNIQUE_MEM(
                     (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
              }
#endif

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)	= Aloc_Opr;
               IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;
               IL_FLD(list_idx)	= IR_Tbl_Idx;
               IL_IDX(list_idx)	= ir_idx;
               IR_FLD_L(ir_idx)	= AT_Tbl_Idx;
               IR_IDX_L(ir_idx)	= tmp_dv_idx;
               IR_LINE_NUM_L(ir_idx) = line;
               IR_COL_NUM_L(ir_idx)  = col;
            }
            break;

         case COPY_IN              :
#ifdef _DEBUG
	    copy_in_count += 1;
#endif /* _DEBUG */

            if (IL_FLD(list_idx) == CN_Tbl_Idx &&
                ! io_call                      &&
// Bug 2182
# ifdef KEY
                ! LANG_Read_Write_Const        &&
# endif
# ifdef _TARGET_OS_MAX
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Integer_1 &&
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Integer_2 &&
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Integer_4 &&
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Logical_1 &&
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Logical_2 &&
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Logical_4 &&
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Real_4    &&
                TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx))) != Complex_4 &&
                
# endif
                (TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Integer ||
                 TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Logical ||
                 TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Real    ||
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
                 (TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Character &&
                  ! on_off_flags.pad_char_literals)                 ||
# endif
                 TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Complex)) {

               /* just use Const_Tmp_Loc_Opr */
               /* if %val(), just leave the constant as is */

	       if (!pass_by_value(spec_idx, info_idx, dummy)) {
                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx) = Const_Tmp_Loc_Opr;
                  IR_TYPE_IDX(ir_idx) = CN_TYPE_IDX(IL_IDX(list_idx));
                  IR_LINE_NUM(ir_idx) = line;
                  IR_COL_NUM(ir_idx) = col;
                  COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx));
                  IL_FLD(list_idx) = IR_Tbl_Idx;
                  IL_IDX(list_idx) = ir_idx;
               }

               break;
            }

            if (IL_FLD(list_idx) == CN_Tbl_Idx               &&
                arg_info_list[info_idx].ed.type == Character &&
                dummy == NULL_IDX                            &&
             compare_cn_and_value(TYP_IDX(arg_info_list[info_idx].ed.type_idx),
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
                                  storage_bit_size_tbl[INTEGER_DEFAULT_TYPE]/8,
# else
                                  TARGET_CHARS_PER_WORD,
# endif
                                  Lt_Opr)) {

               exp_desc = arg_info_list[info_idx].ed;
               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

               TYP_TYPE(TYP_WORK_IDX)       = Character;
               TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
               TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
               TYP_FLD(TYP_WORK_IDX)        = CN_Tbl_Idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
               the_constant = storage_bit_size_tbl[INTEGER_DEFAULT_TYPE]/8;
# else
               the_constant = TARGET_CHARS_PER_WORD;
# endif
               TYP_IDX(TYP_WORK_IDX)= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                  the_constant);

               exp_desc.type_idx          = ntr_type_tbl();
               exp_desc.char_len.fld      = CN_Tbl_Idx;
               exp_desc.char_len.idx      = TYP_IDX(exp_desc.type_idx);

               OPND_FLD(opnd) = CN_Tbl_Idx;
               OPND_IDX(opnd) = ntr_const_tbl(exp_desc.type_idx,
                                              TRUE,
                                              NULL);

               OPND_LINE_NUM(opnd) = line;
               OPND_COL_NUM(opnd)  = col;

               char_ptr1 = (char *)&CN_CONST(IL_IDX(list_idx));
               char_ptr2 = (char *)&CN_CONST(OPND_IDX(opnd));

               for (k = 0; 
                    k<CN_INT_TO_C(TYP_IDX(arg_info_list[info_idx].ed.type_idx));
                    k++) {
                  char_ptr2[k] = char_ptr1[k];
               }

               for ( ; k < CN_INT_TO_C(TYP_IDX(exp_desc.type_idx)); k++) {
                  char_ptr2[k] = ' ';
               }

#ifdef KEY /* Bug 8117 */
               tmp_idx = create_tmp_asg_or_call(&opnd,
                                        &exp_desc,
                                        &l_opnd, 
			       		Intent_In,
                                        TRUE, 
                                        FALSE,
					info_idx, a_type, d_type);
#else /* KEY Bug 8117 */
               tmp_idx = create_tmp_asg(&opnd,
                                        &exp_desc,
                                        &l_opnd, 
			       		Intent_In,
                                        TRUE, 
                                        FALSE);
#endif /* KEY Bug 8117 */

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)    = Aloc_Opr;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;

               IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;

               IL_FLD(list_idx)  = IR_Tbl_Idx;
               IL_IDX(list_idx)  = ir_idx;

               NTR_IR_TBL(unused1);
               IR_OPR(unused1) = Substring_Opr;
               IR_TYPE_IDX(unused1) = arg_info_list[info_idx].ed.type_idx;
               IR_FLD_L(unused1) = AT_Tbl_Idx;
               IR_IDX_L(unused1) = tmp_idx;
               IR_LINE_NUM_L(unused1) = line;
               IR_COL_NUM_L(unused1) = col;
               IR_LINE_NUM(unused1) = line;
               IR_COL_NUM(unused1) = col;

               NTR_IR_LIST_TBL(list_idx2);
               IR_FLD_R(unused1) = IL_Tbl_Idx;
               IR_IDX_R(unused1) = list_idx2;
               IR_LIST_CNT_R(unused1) = 3;

               IL_FLD(list_idx2) = CN_Tbl_Idx;
               IL_IDX(list_idx2) = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(list_idx2) = line;
               IL_COL_NUM(list_idx2)  = col;

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

               IL_FLD(list_idx2) = CN_Tbl_Idx;
               IL_IDX(list_idx2) = TYP_IDX(arg_info_list[info_idx].ed.type_idx);
               IL_LINE_NUM(list_idx2) = line;
               IL_COL_NUM(list_idx2)  = col;

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

               IL_FLD(list_idx2) = CN_Tbl_Idx;
               IL_IDX(list_idx2) = TYP_IDX(arg_info_list[info_idx].ed.type_idx);
               IL_LINE_NUM(list_idx2) = line;
               IL_COL_NUM(list_idx2)  = col;


               IR_FLD_L(ir_idx) = IR_Tbl_Idx;
               IR_IDX_L(ir_idx) = unused1;
            }
#ifdef KEY /* Bug 10410 */
            else if (IL_FLD(list_idx) == IR_Tbl_Idx &&
	      IR_OPR(IL_IDX(list_idx)) == Percent_Val_Opr) {
	       /* Ordinarily Percent_Val_Opr has been removed from the tree
	        * long ago and arg_info_list.ed.percent_val_arg has been set
		* in its place, so the code here doesn't expect to see that
		* operator in the tree. But size_intrinsic() sometimes needs
		* to indicate that it has already generated Aloc_Opr, so this
		* special case lets it do so by putting the operator into the
		* tree at the last minute. */
	      int pvo = IL_IDX(list_idx);
	      IL_FLD(list_idx) = IR_FLD_L(pvo);
	      IL_IDX(list_idx) = IR_IDX_L(pvo);
	    }
#endif /* KEY Bug 10410 */

            else {

               if (! io_call &&
                   arg_info_list[info_idx].ed.rank != 0) {

                  /* Caution about a COPY IN situation!! */

                  PRINTMSG(line, 1438, Caution, col, "copy in");
               }

               COPY_OPND(opnd, IL_OPND(list_idx));
               tmp_idx = create_tmp_asg(&opnd,
                                 (expr_arg_type *)&(arg_info_list[info_idx].ed),
                                 &l_opnd,
                                 Intent_In,
                                 TRUE, 
                                 FALSE);
              

	       if (pass_by_value(spec_idx, info_idx, dummy) ||
                   ATP_ELEMENTAL(spec_idx)) {

                  COPY_OPND(IL_OPND(list_idx), l_opnd);

                  if (ATP_ELEMENTAL(spec_idx)) {
                     NTR_IR_TBL(ir_idx);
                     IR_OPR(ir_idx)    = Aloc_Opr;
                     IR_LINE_NUM(ir_idx) = line;
                     IR_COL_NUM(ir_idx)  = col;

                     if (arg_info_list[info_idx].ed.type == Character) {
                        IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
                     }
                     else {
                        IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
                     }

                     COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(list_idx));
                     IL_FLD(list_idx)  = IR_Tbl_Idx;
                     IL_IDX(list_idx)  = ir_idx;
                  }
               }
               else {
                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx)    = Aloc_Opr;
                  IR_LINE_NUM(ir_idx) = line;
                  IR_COL_NUM(ir_idx)  = col;

                  if (arg_info_list[info_idx].ed.type == Character) {
                     IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
                  }
                  else {
                     IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
                  }

                  IL_FLD(list_idx)  = IR_Tbl_Idx;
                  IL_IDX(list_idx)  = ir_idx;

                  /* JBL - this is for array element change */

# ifndef _FRONTEND_INLINER
                  if (OPND_FLD(l_opnd) == IR_Tbl_Idx &&
                      ! ATP_ELEMENTAL(spec_idx)) {

                     if (IR_OPR(OPND_IDX(l_opnd)) == Whole_Subscript_Opr) {
                        COPY_OPND(l_opnd, IR_OPND_L(OPND_IDX(l_opnd)));
                     }
                  }
# endif
                  if (arg_info_list[info_idx].ed.rank != 0) {
                     unused1 = NULL_IDX;
                     unused2 = NULL_IDX;
                     make_base_subtree(&l_opnd,&base_opnd,&unused1,&unused2);
                     COPY_OPND(l_opnd, base_opnd);
                  }


                  COPY_OPND(IR_OPND_L(ir_idx), l_opnd);

# ifdef _TRANSFORM_CHAR_SEQUENCE
                  if (arg_info_list[info_idx].ed.type == Structure &&
                   ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx))) {

                     COPY_OPND(opnd, IR_OPND_L(ir_idx));
                     transform_char_sequence_ref(&opnd,
                                     arg_info_list[info_idx].ed.type_idx);
                     COPY_OPND(IR_OPND_L(ir_idx), opnd);

                     IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
                  }
# endif
               }
            }
            break;

         case COPY_IN_COPY_OUT     :
#ifdef _DEBUG
            copy_in_copy_out_count += 1;
#endif /* _DEBUG */


            if (arg_info_list[info_idx].ed.section) {
               save_array_syntax_subscripts(list_idx);
            }

            intent = Intent_Inout;

            if (dummy             != NULL_IDX    &&
                ATD_INTENT(dummy) == Intent_Out) {

               intent = Intent_Out;

               if (! io_call &&
                   arg_info_list[info_idx].ed.rank != 0) {
                  /* Caution about a COPY OUT situation!! */

                  PRINTMSG(line, 1438, Caution, col, "copy out");
               }
            }
            else if (! io_call &&
                     arg_info_list[info_idx].ed.rank != 0 &&
                     (dummy             == NULL_IDX   ||
                      ATD_INTENT(dummy) != Intent_In)) {
               /* Caution about a COPY IN/OUT situation!! */

               PRINTMSG(line, 1438, Caution, col, "copy in and copy out");
            }
            else if (! io_call &&
                     arg_info_list[info_idx].ed.rank != 0) {
               /* Caution about a COPY IN situation!! */

               intent = Intent_In;

               PRINTMSG(line, 1438, Caution, col, "copy in");
            }

            COPY_OPND(opnd, IL_OPND(list_idx));
            tmp_idx = create_tmp_asg(&opnd,
                              (expr_arg_type *)&(arg_info_list[info_idx].ed),
                              &l_opnd, 
                              intent,
                              TRUE, 
                              FALSE);

	    if (pass_by_value(spec_idx, info_idx, dummy)) {

               COPY_OPND(IL_OPND(list_idx), l_opnd);
            }
            else {
               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)    = Aloc_Opr;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;

               if (arg_info_list[info_idx].ed.type == Character) {
                  IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
               }
               else {
                  IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
               }

               IL_FLD(list_idx)  = IR_Tbl_Idx;
               IL_IDX(list_idx)  = ir_idx;

               /* JBL - this is for array element change */

# ifndef _FRONTEND_INLINER
               if (OPND_FLD(l_opnd) == IR_Tbl_Idx &&
                   ! ATP_ELEMENTAL(spec_idx)) {

                  if (IR_OPR(OPND_IDX(l_opnd)) == Whole_Subscript_Opr) {
                     COPY_OPND(l_opnd, IR_OPND_L(OPND_IDX(l_opnd)));
                  }
               }
# endif
               if (arg_info_list[info_idx].ed.rank != 0) {
                  unused1 = NULL_IDX;
                  unused2 = NULL_IDX;
                  make_base_subtree(&l_opnd, &base_opnd, &unused1, &unused2);
                  COPY_OPND(l_opnd, base_opnd);
               }

               COPY_OPND(IR_OPND_L(ir_idx), l_opnd);

# ifdef _TRANSFORM_CHAR_SEQUENCE
               if (arg_info_list[info_idx].ed.type == Structure &&
                   ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx))) {

                  COPY_OPND(opnd, IR_OPND_L(ir_idx));
                  transform_char_sequence_ref(&opnd,
                                  arg_info_list[info_idx].ed.type_idx);
                  COPY_OPND(IR_OPND_L(ir_idx), opnd);

                  IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
               }
# endif
            }

            
            break;

         case MAKE_DV              :
#ifdef _DEBUG
            make_dv_count += 1;
#endif /* _DEBUG */

            if (AT_OPTIONAL(dummy) &&
                arg_info_list[info_idx].ed.optional_darg) {

               /* generate if stmt for present(arg) */
             
               COPY_OPND(opnd, IL_OPND(list_idx));
               present_idx = gen_ir(AT_Tbl_Idx, find_left_attr(&opnd),
                                Present_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                                    NO_Tbl_Idx, NULL_IDX);

               gen_opnd(&cond_opnd, 
                        present_idx, 
                        IR_Tbl_Idx, 
                        line, 
                        col);

               /* save the bounding stmts. these are moved in later. */
               true_start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               true_end_sh_idx = curr_stmt_sh_idx;

               /* put work here */

               tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
               ATD_NOT_PT_UNIQUE_MEM(tmp_idx) = TRUE;
               ATD_NOT_PT_UNIQUE_MEM(
                     (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
# endif

               ATD_TYPE_IDX(tmp_idx) = arg_info_list[info_idx].ed.type_idx;
               ATD_STOR_BLK_IDX(tmp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
               AT_SEMANTICS_DONE(tmp_idx) = TRUE;

               if (arg_info_list[info_idx].ed.rank) {

               /* Positions 1-7 are deferred shape entries in bd table. */

                  ATD_ARRAY_IDX(tmp_idx) = arg_info_list[info_idx].ed.rank;
               }
               ATD_IM_A_DOPE(tmp_idx)    = TRUE;

               COPY_OPND(r_opnd, IL_OPND(list_idx));
               exp_desc = arg_info_list[info_idx].ed;
               OPND_FLD(l_opnd) = AT_Tbl_Idx;
               OPND_IDX(l_opnd) = tmp_idx;
               OPND_LINE_NUM(l_opnd) = line;
               OPND_COL_NUM(l_opnd)  = col;

               gen_dv_whole_def(&l_opnd, &r_opnd, &exp_desc);


               /* gen loc tmp = loc(tmp_idx) */

               GEN_COMPILER_TMP_ASG(asg_idx,
                                    tmp_loc_idx,
                                    TRUE,   /* Semantics done */
                                    line,
                                    col,
                                    SA_INTEGER_DEFAULT_TYPE,
                                    Priv);

               ir_idx = gen_ir(AT_Tbl_Idx, tmp_idx,
                           Loc_Opr, CRI_Ptr_8, line, col,
                               NO_Tbl_Idx, NULL_IDX);

               IR_FLD_R(asg_idx)            = IR_Tbl_Idx;
               IR_IDX_R(asg_idx)            = ir_idx;
               IR_LINE_NUM_R(asg_idx)       = line;
               IR_COL_NUM_R(asg_idx)        = col;

               gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

               true_start_sh_idx = SH_NEXT_IDX(true_start_sh_idx);
               true_end_sh_idx = SH_PREV_IDX(true_end_sh_idx);

               /* gen the loc tmp = 0 stmt. */

               asg_idx = gen_ir(AT_Tbl_Idx, tmp_loc_idx,
                            Asg_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

               gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

               false_start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               false_end_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

               gen_if_stmt(&cond_opnd,
                           true_start_sh_idx,
                           true_end_sh_idx,
                           false_start_sh_idx,
                           false_end_sh_idx,
                           line,
                           col);

               /* put loc tmp into arg */

               IL_FLD(list_idx) = AT_Tbl_Idx;
               IL_IDX(list_idx) = tmp_loc_idx;
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx)  = col;
            }
            else {

               tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
               ATD_NOT_PT_UNIQUE_MEM(tmp_idx) = TRUE;
               ATD_NOT_PT_UNIQUE_MEM(
                        (find_left_attr(&IL_OPND(list_idx)))) = TRUE;
# endif

               ATD_TYPE_IDX(tmp_idx) = arg_info_list[info_idx].ed.type_idx;
               ATD_STOR_BLK_IDX(tmp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
               AT_SEMANTICS_DONE(tmp_idx) = TRUE;

               if (arg_info_list[info_idx].ed.rank) {

               /* Positions 1-7 are deferred shape entries in bd table. */

                  ATD_ARRAY_IDX(tmp_idx) = arg_info_list[info_idx].ed.rank;
               }
               ATD_IM_A_DOPE(tmp_idx)    = TRUE;

               COPY_OPND(r_opnd, IL_OPND(list_idx));
               exp_desc = arg_info_list[info_idx].ed;
               OPND_FLD(l_opnd) = AT_Tbl_Idx;
               OPND_IDX(l_opnd) = tmp_idx;
               OPND_LINE_NUM(l_opnd) = line;
               OPND_COL_NUM(l_opnd)  = col;
   
               gen_dv_whole_def(&l_opnd, &r_opnd, &exp_desc);

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)	= Aloc_Opr;
               IR_TYPE_IDX(ir_idx)	= CRI_Ptr_8;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;
               IL_FLD(list_idx)	= IR_Tbl_Idx;
               IL_IDX(list_idx)	= ir_idx;
               IR_FLD_L(ir_idx)	= AT_Tbl_Idx;
               IR_IDX_L(ir_idx)	= tmp_idx;
               IR_LINE_NUM_L(ir_idx) = line;
               IR_COL_NUM_L(ir_idx)  = col;
            }

            arg_info_list[info_idx].ed.dope_vector = TRUE;

            break;

         case COPY_IN_MAKE_DV      :
# if defined(KEY) /* Bug 3230 */
         case COPY_INOUT_MAKE_DV      :
	    {
#ifdef _DEBUG
            copy_in_make_dv_count += (association == COPY_IN_MAKE_DV);
            copy_inout_make_dv_count += (association == COPY_INOUT_MAKE_DV);
#endif /* _DEBUG */
	      int old_curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
# endif /* KEY Bug 3230 */

	      /* tmp_idx is the copy in tmp */
	      COPY_OPND(opnd, IL_OPND(list_idx));
# if defined(KEY) /* Bug 3230 */
#ifdef KEY /* Bug 3607 */
	      boolean copyin = (association == COPY_IN_MAKE_DV);
	      /* The front end has always performed copyin and copyinout in
	       * cases where an array section must be passed to a procedure
	       * which expects its dummy argument to be a contiguous array (for
	       * example, an array reference whose subscript is itself an array;
	       * or an array section passed to a dummy argument which doesn't
	       * expect a dope vector.) The COPY_INOUT_MAKE_DV optimization
	       * relies on the same code in a situation where the dummy argument
	       * does expect a dope vector. Because we do this for performance,
	       * we would like to move the alloc-related statements to the
	       * prolog, if we know they don't depend on variables which might
	       * change during execution. */
	      boolean move = (association == COPY_INOUT_MAKE_DV) &&
		safe_to_move_copyinout_alloc(&opnd);
	      tmp_idx = create_tmp_asg(&opnd,
				(expr_arg_type *)&(arg_info_list[info_idx].ed), 
				&l_opnd, 
				copyin ? Intent_In : Intent_Inout,
				copyin,
				/* Dealloc unneeded if alloc occurs in prolog */
				move);
	       if (move) {
		 move_tmp_alloc_assignment(old_curr_stmt_sh_idx, attr_idx);
	       }
             }
#else /* KEY Bug 3607 */
            tmp_idx = create_tmp_asg(&opnd,
                              (expr_arg_type *)&(arg_info_list[info_idx].ed), 
                              &l_opnd, 
                              association == COPY_IN_MAKE_DV ? Intent_In : Intent_Inout,
                              association == COPY_IN_MAKE_DV ? TRUE : FALSE,
                              association == COPY_IN_MAKE_DV ? FALSE : TRUE);
             if (association == COPY_INOUT_MAKE_DV)
               move_tmp_alloc_assignment(&opnd, old_curr_stmt_sh_idx, attr_idx, dim);
#endif /* KEY Bug 3607 */
# else /* KEY Bug 3230 */
            tmp_idx = create_tmp_asg(&opnd,
                              (expr_arg_type *)&(arg_info_list[info_idx].ed), 
                              &l_opnd, 
                              Intent_In,
                              TRUE, 
                              FALSE);
# endif /* KEY Bug 3230 */

            if (! io_call &&
                arg_info_list[info_idx].ed.rank != 0) {

               /* Caution about a COPY IN situation!! */

               PRINTMSG(line, 1438, Caution, col, "copy in");
            }

            /* tmp_dv_idx is the dope vector tmp */

            tmp_dv_idx = gen_compiler_tmp(line, col, Priv, TRUE);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 7424 */
            if (clear_pt_unique_mem(dummy)) {
#endif /* KEY Bug 7424 */
            ATD_NOT_PT_UNIQUE_MEM(tmp_idx) = TRUE;
#ifdef KEY /* Bug 7424 */
            }
#endif /* KEY Bug 7424 */

            if (ATD_AUTOMATIC(tmp_idx) && 
                ATD_AUTO_BASE_IDX(tmp_idx) != NULL_IDX) {
               ATD_NOT_PT_UNIQUE_MEM(ATD_AUTO_BASE_IDX(tmp_idx)) = TRUE;
            }

#ifdef KEY /* Bug 7424 */
            if (clear_pt_unique_mem(dummy)) {
#endif /* KEY Bug 7424 */
            ATD_NOT_PT_UNIQUE_MEM(tmp_dv_idx) = TRUE;
#ifdef KEY /* Bug 7424 */
            }
#endif /* KEY Bug 7424 */
# endif

            ATD_TYPE_IDX(tmp_dv_idx) = arg_info_list[info_idx].ed.type_idx;
            ATD_STOR_BLK_IDX(tmp_dv_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
            AT_SEMANTICS_DONE(tmp_dv_idx) = TRUE;

            if (arg_info_list[info_idx].ed.rank) {

            /* Positions 1-7 are deferred shape entries in the bd table. */

               ATD_ARRAY_IDX(tmp_dv_idx) = arg_info_list[info_idx].ed.rank;
            }
            ATD_IM_A_DOPE(tmp_dv_idx)    = TRUE;

            OPND_FLD(r_opnd) = AT_Tbl_Idx;
            OPND_IDX(r_opnd) = tmp_idx;
            OPND_LINE_NUM(r_opnd) = line;
            OPND_COL_NUM(r_opnd)  = col;
   
            exp_desc = arg_info_list[info_idx].ed;
   
            if (TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) == Character) {
               ok = gen_whole_substring(&r_opnd, exp_desc.rank) && ok;
            }

            OPND_FLD(l_opnd) = AT_Tbl_Idx;
            OPND_IDX(l_opnd) = tmp_dv_idx;
            OPND_LINE_NUM(l_opnd) = line;
            OPND_COL_NUM(l_opnd)  = col;

            gen_dv_whole_def(&l_opnd, &r_opnd, &exp_desc);

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)	= Aloc_Opr;
            IR_TYPE_IDX(ir_idx)	= CRI_Ptr_8;
            IR_LINE_NUM(ir_idx) = line;
            IR_COL_NUM(ir_idx)  = col;
            IL_FLD(list_idx)	= IR_Tbl_Idx;
            IL_IDX(list_idx)	= ir_idx;
            IR_FLD_L(ir_idx)	= AT_Tbl_Idx;
            IR_IDX_L(ir_idx)	= tmp_dv_idx;
            IR_LINE_NUM_L(ir_idx) = line;
            IR_COL_NUM_L(ir_idx)  = col;

            arg_info_list[info_idx].ed.dope_vector = TRUE;

            break;

         case CHECK_CONTIG_FLAG :
#ifdef _DEBUG
            check_contig_flag_count += 1;
#endif /* _DEBUG */

            if (! io_call &&
                arg_info_list[info_idx].ed.rank != 0) {
               /* Caution about a potential COPY IN situation!! */

               PRINTMSG(line, 1438, Caution, col, "possible copy in and out");
            }

            if (arg_info_list[info_idx].ed.section) {
               save_array_syntax_subscripts(list_idx);
            }

            /* generate address temp */

            addr_tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

# ifdef _TRANSFORM_CHAR_SEQUENCE
            if (arg_info_list[info_idx].ed.type == Character ||
                (arg_info_list[info_idx].ed.type == Structure &&
                 ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx))))
# else
            if (arg_info_list[info_idx].ed.type == Character)
# endif
            {
               ATD_TYPE_IDX(addr_tmp_idx) = CRI_Ch_Ptr_8;
            }
            else {
               ATD_TYPE_IDX(addr_tmp_idx) = SA_INTEGER_DEFAULT_TYPE;
            }
            ATD_STOR_BLK_IDX(addr_tmp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
            AT_SEMANTICS_DONE(addr_tmp_idx) = TRUE;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            ATD_NOT_PT_UNIQUE_MEM(addr_tmp_idx) = TRUE;
            ATD_NOT_PT_UNIQUE_MEM(find_left_attr(&IL_OPND(list_idx))) = TRUE;
# endif

            /* find the dope vector opnd */

            COPY_OPND(dv_opnd, IL_OPND(list_idx));

            while (OPND_FLD(dv_opnd) == IR_Tbl_Idx &&
                   IR_OPR(OPND_IDX(dv_opnd)) != Dv_Deref_Opr) {
               COPY_OPND(dv_opnd, IR_OPND_L(OPND_IDX(dv_opnd)));
            }

            COPY_OPND(dv_opnd, IR_OPND_L(OPND_IDX(dv_opnd)));

            /* save the original dope vector in ATD_TMP_IDX */

            ATD_FLD(addr_tmp_idx) = OPND_FLD(dv_opnd);
            ATD_TMP_IDX(addr_tmp_idx) = OPND_IDX(dv_opnd);

            /* generate if (contig)  Before */

            ir_idx = gen_ir(OPND_FLD(dv_opnd), OPND_IDX(dv_opnd),
                        Dv_Access_A_Contig, CG_INTEGER_DEFAULT_TYPE, line, col,
                            NO_Tbl_Idx, NULL_IDX);

            present_idx = gen_ir(IR_Tbl_Idx, ir_idx,
                             Eq_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                                 CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

            gen_opnd(&cond_opnd,
                     present_idx,
                     IR_Tbl_Idx,
                     line,
                     col);


            contig_test_ir_idx    = present_idx;


            /* set address temp = address from dope vector or */
            /* loc (base subtree).         Before             */

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(addr_tmp_idx);
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;
            IR_LINE_NUM_L(asg_idx) = line;
            IR_COL_NUM_L(asg_idx)  = col;
            IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
            IR_IDX_L(asg_idx)      = addr_tmp_idx;

            COPY_OPND(opnd, IL_OPND(list_idx));
            unused1 = NULL_IDX;
            unused2 = NULL_IDX;
            make_base_subtree(&opnd, &base_opnd, &unused1, &unused2);

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)      = Loc_Opr;
            IR_TYPE_IDX(ir_idx) = (arg_info_list[info_idx].ed.type ==
                                                               Character)
                                               ? CRI_Ch_Ptr_8 : CRI_Ptr_8;
            IR_LINE_NUM(ir_idx) = line;
            IR_COL_NUM(ir_idx)  = col;

            COPY_OPND(IR_OPND_L(ir_idx), base_opnd);

# ifdef _TRANSFORM_CHAR_SEQUENCE
            if (arg_info_list[info_idx].ed.type == Structure &&
                ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx))) {

               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               transform_char_sequence_ref(&opnd,
                               arg_info_list[info_idx].ed.type_idx);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);

               IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
            }
# endif

/* Can't use this because of pdgcs problems 
               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)   = Dv_Access_Base_Addr;
               IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;

               COPY_OPND(IR_OPND_L(ir_idx), dv_opnd);
*/

            IR_FLD_R(asg_idx) = IR_Tbl_Idx;
            IR_IDX_R(asg_idx) = ir_idx;

            gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            true_start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
            true_end_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

            /* capture bounding stmts after curr_stmt_sh_idx */
            true_start_sh_idx2 = curr_stmt_sh_idx;
            true_end_sh_idx2 = SH_NEXT_IDX(curr_stmt_sh_idx);


            /* capture bounding stmts before curr_stmt_sh_idx */
            false_start_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
            false_end_sh_idx = curr_stmt_sh_idx;

            /* do copy in Before */

            intent = Intent_In;

            if (dummy             != NULL_IDX    &&
                ATD_INTENT(dummy) == Intent_Out) {
               intent = Intent_Out;
            }
            else if (a_type != Array_Tmp_Ptr &&
                     (dummy             == NULL_IDX   ||
                      ATD_INTENT(dummy) != Intent_In)) {
               intent = Intent_Inout;
            }

            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc = arg_info_list[info_idx].ed;
#ifdef KEY /* Bug 8117 */
            tmp_idx = create_tmp_asg_or_call(&opnd, 
                                     &exp_desc, 
                                     &r_opnd, 
                                     intent,
                                     TRUE, 
                                     FALSE,
				     info_idx, a_type, d_type);
#else /* KEY Bug 8117 */
            tmp_idx = create_tmp_asg(&opnd, 
                                     &exp_desc, 
                                     &r_opnd, 
                                     intent,
                                     TRUE, 
                                     FALSE);
#endif /* KEY Bug 8117 */
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            ATD_NOT_PT_UNIQUE_MEM(tmp_idx) = TRUE;

            if (ATD_AUTOMATIC(tmp_idx) &&
                ATD_AUTO_BASE_IDX(tmp_idx) != NULL_IDX) {

               ATD_NOT_PT_UNIQUE_MEM(ATD_AUTO_BASE_IDX(tmp_idx)) = TRUE;
            }
# endif

            contig_test_ir_idx = NULL_IDX;

            /* set address temp = loc(copy in temp) Before */

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(addr_tmp_idx);
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;
            IR_LINE_NUM_L(asg_idx) = line;
            IR_COL_NUM_L(asg_idx)  = col;
            IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
            IR_IDX_L(asg_idx)      = addr_tmp_idx;

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)    = Loc_Opr;
            IR_LINE_NUM(ir_idx) = line;
            IR_COL_NUM(ir_idx)  = col;

            if (arg_info_list[info_idx].ed.type == Character) {
               IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
            }
            else {
               IR_TYPE_IDX(ir_idx) = CRI_Ptr_8;
            }

            OPND_FLD(l_opnd)  = AT_Tbl_Idx;
            OPND_IDX(l_opnd)  = tmp_idx;
            OPND_LINE_NUM(l_opnd) = line;
            OPND_COL_NUM(l_opnd)  = col;

            if (TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) == Character) {
               ok = gen_whole_substring(&l_opnd,
                                     arg_info_list[info_idx].ed.rank) && ok;
            }

            COPY_OPND(IR_OPND_L(ir_idx), l_opnd);

# ifdef _TRANSFORM_CHAR_SEQUENCE
            if (arg_info_list[info_idx].ed.type == Structure &&
                ATT_CHAR_SEQ(TYP_IDX(arg_info_list[info_idx].ed.type_idx))) {

               COPY_OPND(opnd, IR_OPND_L(ir_idx));
               transform_char_sequence_ref(&opnd,
                               arg_info_list[info_idx].ed.type_idx);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);

               IR_TYPE_IDX(ir_idx) = CRI_Ch_Ptr_8;
            }
# endif

            IR_FLD_R(asg_idx) = IR_Tbl_Idx;
            IR_IDX_R(asg_idx) = ir_idx;

            gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            /* generate "if (contiguous) { ... } else { ... }" before call */
            gen_if_stmt(&cond_opnd,
                        true_start_sh_idx,
                        true_end_sh_idx,
                        SH_NEXT_IDX(false_start_sh_idx),
                        SH_PREV_IDX(false_end_sh_idx),
                        line,
                        col);
                        


            /* generate if (!contig) test After */
            if (SH_NEXT_IDX(true_start_sh_idx2) != true_end_sh_idx2) {
               ir_idx = gen_ir(OPND_FLD(dv_opnd), OPND_IDX(dv_opnd),
                        Dv_Access_A_Contig, CG_INTEGER_DEFAULT_TYPE, line, col,
                               NO_Tbl_Idx, NULL_IDX);

               present_idx = gen_ir(IR_Tbl_Idx, ir_idx,
                                Ne_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                                    CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

               gen_opnd(&cond_opnd,
                        present_idx,
                        IR_Tbl_Idx,
                        line,
                        col);
   
               gen_if_stmt(&cond_opnd,
                           SH_NEXT_IDX(true_start_sh_idx2),
                           SH_PREV_IDX(true_end_sh_idx2),
                           NULL_IDX,
                           NULL_IDX,
                           line,
                           col);
            }
          
            /* set argument to address temp */

            IL_FLD(list_idx) = AT_Tbl_Idx;
            IL_IDX(list_idx) = addr_tmp_idx;
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            break;
      }

      dummy_idx++;

EXIT:

      defer_stmt_expansion = save_defer_stmt_expansion;
      COPY_OPND(opnd, IL_OPND(list_idx));
      stmt_expansion_control_end(&opnd);
      COPY_OPND(IL_OPND(list_idx), opnd);

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   expr_mode = save_expr_mode;

   TRACE (Func_Exit, "final_arg_work", NULL);

   return(ok);

}  /* final_arg_work */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Linearizes the attributes of a dummy arg for table lookup in          *|
|*      final_arg_work.                                                       *|
|*									      *|
|* Input parameters:							      *|
|*	darg_idx - dummy arg.                                                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	dummy_arg_type.                                                       *|
|*									      *|
\******************************************************************************/

static dummy_arg_type get_dummy_arg_type(int	darg_idx)

{
   dummy_arg_type	d_type = Unknown_Dummy;


   TRACE (Func_Entry, "get_dummy_arg_type", NULL);

   if (darg_idx != NULL_IDX) {
# ifdef _DEBUG
      if (AT_OBJ_CLASS(darg_idx) != Data_Obj) {
         PRINTMSG(stmt_start_line, 325, Internal, stmt_start_col);
      }
# endif
      if (ATD_ARRAY_IDX(darg_idx) == NULL_IDX) {

         if (ATD_POINTER(darg_idx)) {
            d_type = Scalar_Ptr_Dummy;
         }
         else if (ATD_INTRIN_DARG(darg_idx) &&
                  ATD_IM_A_DOPE(darg_idx))  {
            d_type = Intrin_Dope_Dummy;
         }
         else {
            d_type = Scalar_Dummy;
         }
      }
      else {

         if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(darg_idx)) == Assumed_Shape) {
            d_type = Assumed_Shape_Dummy;
         }
#ifdef KEY /* Bug 10670 */
	 /* This is a band-aid to deal with a problem that might call for
	  * surgery: the declaration of a dummy argument of one module
	  * procedure may depend on a call to a second module procedure
	  * whose arguments have not been subjected to attr_semantics(), so
	  * that the second procedure's array arguments have not yet been
	  * transformed from deferred-shape to assumed-shape. The result is
	  * that the call to the second procedure in the prolog of the first
	  * procedure doesn't emit the correct argument-passing code.
	  *
	  * Example: character(len=second_proc(proc1_arg1)) :: proc1_arg2
	  *
	  * So far, the band-aid is adequate: if we eventually find a test
	  * case that proves otherwise, it may be necessary to call
	  * attr_semantics() early for the dummy arguments of the second
	  * module procedure, and to make sure that no problems result
	  * from calling attr_semantics() a second time in the normal fashion
	  * on the same dummy arguments, and to make sure that the compiler
	  * doesn't suffer infinite recursion if an erroneous user program
	  * contains two declarations which depend on one another.
	  */
         else if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(darg_idx)) == Deferred_Shape &&
	   ATD_CLASS(darg_idx) == Dummy_Argument &&
	   !(ATD_INTRIN_DARG(darg_idx) || ATD_POINTER(darg_idx) ||
	     ATD_ALLOCATABLE(darg_idx))) {
            d_type = Assumed_Shape_Dummy;
	 }
#endif /* KEY Bug 10670 */
         else if (ATD_POINTER(darg_idx)) {
            d_type = Array_Ptr_Dummy;
         }
         else if (ATD_INTRIN_DARG(darg_idx) &&
                  ATD_IM_A_DOPE(darg_idx))  {
            d_type = Intrin_Dope_Dummy;
         }
         else {
            d_type = Sequence_Array_Dummy;
         }
      }
   }

   TRACE (Func_Exit, "get_dummy_arg_type", NULL);

   return(d_type);

}  /* get_dummy_arg_type */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Linearizes actual argument attributes for table lookup in             *|
|*      final_arg_work.                                                       *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx - IL idx for actual arg.                                     *|
|*      info_idx - arg_info_list idx for description of actual arg            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	act_arg_type.                                                         *|
|*									      *|
\******************************************************************************/

act_arg_type	get_act_arg_type(expr_arg_type	*exp_desc)

{
   act_arg_type		a_type;

   TRACE (Func_Entry, "get_act_arg_type", NULL);

   if (exp_desc->rank == 0) {
  
#ifdef KEY /* Bug 572 */
      /* Test first for pointer-ness, then for constant-ness (opposite of
       * original code order) because a constant pointer is folded by
       * fold_aggragate_expression() into a Scalar_Tmp_Ptr
       */
#endif /* KEY Bug 572 */
      if (exp_desc->pointer) {
         if (exp_desc->tmp_reference) {
            a_type = Scalar_Tmp_Ptr;
         }
         else {
            a_type = Scalar_Ptr;
         }
      }
      else if (exp_desc->constant) {
         if (exp_desc->tmp_reference) {
            a_type = Scalar_Tmp_Var;
         }
         else {
            a_type = Scalar_Constant;
         }
      }
      else if (exp_desc->reference) {
         
         if (exp_desc->array_elt) {
            if (exp_desc->tmp_reference) {
               a_type = Array_Tmp_Elt;
            }
            else {
               a_type = Array_Elt;
            }
         }
         else if (exp_desc->has_symbolic) {
            a_type = Scalar_Expression;
         }
         else {
            if (exp_desc->tmp_reference) {
               a_type = Scalar_Tmp_Var;
            }
            else {
               a_type = Scalar_Var;
            }
         }
      }
      else if (exp_desc->tmp_reference) {
         if (exp_desc->dope_vector) {
            a_type = Scalar_Tmp_Ptr;
         }
         else {
            a_type = Scalar_Tmp_Var;
         }
      }
      else {
         a_type = Scalar_Expression;
      }
   }
   else {

      if (exp_desc->constant) {
         if (exp_desc->section) {
            a_type = Constant_Array_Section;
         }
         else if (exp_desc->tmp_reference) {
            a_type = Whole_Tmp_Sequence;
         }
         else {
            a_type = Whole_Array_Constant;
         }
      }
      else if (exp_desc->pointer) {
         if (exp_desc->tmp_reference) {
            a_type = Array_Tmp_Ptr;
         }
         else {
            a_type = Array_Ptr;
         }
      }
      else if (exp_desc->reference) {

         if (exp_desc->vector_subscript) {
            a_type = Vector_Subscript_Section;
         }
         else if (exp_desc->section) {

            if (exp_desc->contig_array) {
               if (exp_desc->dope_vector) {
                  a_type = Dv_Contig_Section;
               }
               else {
                  a_type = Contig_Section;
               }
            }
            else if (exp_desc->dope_vector) {
               a_type = Dv_Array_Section;
            }
            else {
               a_type = Sequence_Array_Section;
            }
         }
         else if (exp_desc->allocatable) {
            if (exp_desc->tmp_reference) {
               a_type = Whole_Tmp_Allocatable;
            }
            else {
               a_type = Whole_Allocatable;
            }
         }
         else if (exp_desc->assumed_shape) {
            a_type = Whole_Ass_Shape;
         }
         else {
            if (exp_desc->tmp_reference) {
               a_type = Whole_Tmp_Sequence;
            }
            else {
               a_type = Whole_Sequence;
            }
         }
      }
      else if (exp_desc->tmp_reference) {
         if (exp_desc->allocatable) {
            a_type = Whole_Tmp_Allocatable;
         }
         else if (exp_desc->dope_vector) {

            /* I'm using Array_Tmp_Ptr here because don't know for sure */
            /* sure if the tmp is contiguous.                           */

            a_type = Array_Tmp_Ptr;
         }
         else {
            a_type = Whole_Tmp_Sequence;
         }
      }
      else {
         a_type = Array_Expr;
      }
   }

   TRACE (Func_Exit, "get_act_arg_type", NULL);

   return(a_type);

}  /* get_act_arg_type */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Creates a temp assignment for copy in situations. Creates both stack  *|
|*      heap temps.                                                           *|
|*									      *|
|* Input parameters:							      *|
|*	r_opnd - right hand side expression.                                  *|
|*	exp_desc - expression descriptor for right hand side expression.      *|
|*	intent   - Intent_In, Intent_Inout, Intent_Out. A flag to specify if  *|
|*                 Assignments are needed before, after, or both.             *|
|*	stmt_tmp  - TRUE if ok to reuse stmt tmp.                             *|
|*	save_where_dealloc_stmt - if this is for where mask, don't insert the *|
|*                                dealloc stmt.                               *|
|*									      *|
|* Output parameters:							      *|
|*	left_opnd - the reference tree used in the assignment stmt. This is   *|
|*                  a complete tree include whole_subscripts or substrings.   *|
|*									      *|
|* Returns:								      *|
|*	attr idx for temp.                                                    *|
|*									      *|
\******************************************************************************/

int	create_tmp_asg(opnd_type     *r_opnd,
		       expr_arg_type *exp_desc,
                       opnd_type     *left_opnd,
		       int	      intent,
                       boolean	      stmt_tmp,
		       boolean        save_where_dealloc_stmt)
#if KEY /* Bug 8117 */
{
  return common_create_tmp_asg(r_opnd, exp_desc, left_opnd, intent,
    stmt_tmp, save_where_dealloc_stmt, FALSE);
}

/* Like create_tmp_asg(), but (usually) generates a call to a library function
 * instead of an inline assignment.
 */
int
create_tmp_asg_or_call(opnd_type *r_opnd, expr_arg_type *exp_desc,
  opnd_type *left_opnd, int intent, boolean stmt_tmp,
  boolean save_where_dealloc_stmt, int info_idx, dummy_arg_type a_type,
  dummy_arg_type d_type) {
  /*
   * To reduce code size, call a runtime function to copy a
   * noncontiguous array to a contiguous array temporary, instead of
   * performing the operation in line.
   *
   * For safety, this test limits the change to the case where an
   * entire assumed-shape array is passed to an F77-style dummy,
   * because I am not certain that the runtime function satisfies all
   * the other cases which lead to CHECK_CONTIG_FLAG (see global
   * arg_assoc_tbl.) In the future, for additional reduction in code
   * size, one could relax this test, assuming the runtime function
   * works correctly for those cases.
   */
  boolean call = arg_info_list[info_idx].ed.rank &&
    (!arg_info_list[info_idx].ed.section) &&
    arg_info_list[info_idx].ed.type != Structure &&
    (a_type == Whole_Ass_Shape || a_type == Array_Ptr) &&
    (d_type == Unknown_Dummy || d_type == Sequence_Array_Dummy);

#ifdef _DEBUG
  runtime_copyinout_count += !!call;
#endif /* _DEBUG */

  return common_create_tmp_asg(r_opnd, exp_desc, left_opnd, intent,
    stmt_tmp, save_where_dealloc_stmt, call);
}

/*
 * Perform the work common to create_tmp_asg() and create_tmp_asg_or_call()
 */
static int
common_create_tmp_asg(opnd_type *r_opnd, expr_arg_type *exp_desc,
 opnd_type *left_opnd, int intent, boolean stmt_tmp,
 boolean save_where_dealloc_stmt, boolean call)
#endif /* KEY Bug 8117 */
{
   int                  alloc_idx;
   int                  asg_idx;
   int                  base_asg_idx;
   int                  base_tmp_idx;
   int                  bd_idx;
   int                  col;
   boolean              constant_shape = TRUE;
   int                  dealloc_idx    = NULL_IDX;
   int                  ir_idx;
   int                  line;
   int                  list_idx;
   int                  max_idx;
   boolean              ok = TRUE;
   opnd_type            opnd;
   int                  save_curr_stmt_sh_idx;
   boolean		save_defer_stmt_expansion;
   opnd_type            size_opnd;
   int                  tmp_idx;
   int			true_start_sh_idx;
   int			true_end_sh_idx;


   TRACE (Func_Entry, "common_create_tmp_asg", NULL);

   find_opnd_line_and_column(r_opnd, &line, &col);

# ifdef _DEBUG
   if (defer_stmt_expansion &&
       alloc_block_start_idx &&
       save_where_dealloc_stmt) {
      PRINTMSG(line, 626, Internal, col,
               "no defer_stmt_expansion", "create_tmp_asg");
   }

   if (orig_sh_idx != NULL_IDX &&
       alloc_block_start_idx &&
       save_where_dealloc_stmt) {
      PRINTMSG(line, 626, Internal, col,
               "no orig_sh_idx", "create_tmp_asg");
   }
#  endif

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   if (alloc_block_start_idx &&
       save_where_dealloc_stmt) {
      curr_stmt_sh_idx = alloc_block_start_idx;
   }

   if (exp_desc->rank) {
      constant_shape = gen_bd_entry(r_opnd, exp_desc, &bd_idx, line, col);
   }
   else if (exp_desc->type == Character) {
      ok = validate_char_len(r_opnd, exp_desc);

      if (TYP_FLD(exp_desc->type_idx) != CN_Tbl_Idx) {
         constant_shape = FALSE;
      }
   }

   tmp_idx = NULL_IDX;

   if (stmt_tmp &&
       constant_shape) {
      tmp_idx = get_stmt_tmp(exp_desc->type_idx,
                             FALSE,
                             exp_desc->rank);
   }

   /* sjc: generate "contig_array = noncontig_array" before call;
    * tmp_idx is the contig_array */
#ifdef KEY /* Bug 8117 */
   int copyin_dest = NULL_IDX;
   fld_type copyin_fld;
   int copyin_src = get_variable_reference(r_opnd, &copyin_fld);
   if (NULL_IDX == copyin_src) {
     call = FALSE;
   }
   if (call) {
     /* Here we replace either the NTR_IR_TBL(asg_idx) sequence or
      * GEN_COMPILER_TMP_ASG with code to emit a call on a library procedure.
      * The (NULL_IDX == tmp_idx) code imitates part of GEN_COMPILER_TMP_ASG */
     if (NULL_IDX == tmp_idx) {
       tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);
       AT_SEMANTICS_DONE(tmp_idx)    = TRUE;
       ATD_TYPE_IDX(tmp_idx)         = exp_desc->type_idx;
       /*ATD_TMP_IDX(tmp_idx)          = asg_idx;*/
       ATD_FLD(tmp_idx)              = IR_Tbl_Idx;
       ATD_STOR_BLK_IDX(tmp_idx)     = SCP_SB_STACK_IDX(curr_scp_idx);
       AT_DEFINED(tmp_idx)           = TRUE;
     }
     copyin_dest = tmp_idx;
     asg_idx = build_copyinout_call(Copyin_Attr_Idx, COPYIN_ENTRY,
       AT_Tbl_Idx, copyin_dest, copyin_fld, copyin_src, line, col);
   }
   else {
#endif /* KEY Bug 8117 */
     if (tmp_idx) {
	NTR_IR_TBL(asg_idx);
	IR_OPR(asg_idx)           = Asg_Opr;

	IR_TYPE_IDX(asg_idx)	= exp_desc->type_idx;
	IR_FLD_L(asg_idx)         = AT_Tbl_Idx;
	IR_IDX_L(asg_idx)         = tmp_idx;
	IR_LINE_NUM_L(asg_idx)    = line;
	IR_LINE_NUM(asg_idx)      = line;
	IR_COL_NUM_L(asg_idx)     = col;
	IR_COL_NUM(asg_idx)       = col;
	ATD_TMP_IDX(tmp_idx)      = asg_idx;
	ATD_FLD(tmp_idx)          = IR_Tbl_Idx;
	AT_DEFINED(tmp_idx)       = TRUE;
     } 
     else {
	GEN_COMPILER_TMP_ASG(asg_idx, 
			     tmp_idx,
			     TRUE,	/* Semantics done */
			     line,
			     col,
			     exp_desc->type_idx,
			     Priv);
     }
#ifdef KEY /* Bug 8117 */
   }
#endif /* KEY Bug 8117 */

   ATD_ASG_TMP(tmp_idx) = TRUE;

   if (!constant_shape) {
      ATD_STOR_BLK_IDX(tmp_idx) =  SCP_SB_BASED_IDX(curr_scp_idx);
   }

   if (exp_desc->rank) {

      if (! constant_shape) {
         /* initialize size_opnd to the number of elements for */
         /* determine_tmp_size.                                */

         OPND_FLD(size_opnd)	= BD_LEN_FLD(bd_idx);
         OPND_IDX(size_opnd)	= BD_LEN_IDX(bd_idx);
         OPND_LINE_NUM(size_opnd) = line;
         OPND_COL_NUM(size_opnd)  = col;
      }

      ATD_ARRAY_IDX(tmp_idx) = bd_idx;
   }
   else if (! constant_shape) {
      /* initialize size_opnd to constant one for scalar for */
      /* determine_tmp_size.                                 */

      OPND_FLD(size_opnd) = CN_Tbl_Idx;
      OPND_IDX(size_opnd) = CN_INTEGER_ONE_IDX;
      OPND_LINE_NUM(size_opnd) = line;
      OPND_COL_NUM(size_opnd)  = col;
   }

   /* gen whole subscript or whole substring for tmp_idx */

#ifdef KEY /* Bug 8117 */
   if (!call) {
#endif /* KEY Bug 8117 */
     COPY_OPND((*left_opnd), IR_OPND_L(asg_idx));

   if (exp_desc->rank) {
      ok = gen_whole_subscript(left_opnd, exp_desc);
   }
   else if (exp_desc->type == Character) {
      ok = gen_whole_substring(left_opnd, 0);
   }

   COPY_OPND(IR_OPND_L(asg_idx), (*left_opnd));


   IR_RANK(asg_idx)          = exp_desc->rank;
#ifdef KEY /* Bug 8117 */
   }
#endif /* KEY Bug 8117 */

   if (! constant_shape) {
      /* now for the alloc and dealloc stmts */

      ATD_AUTOMATIC(tmp_idx)      = TRUE;

      GEN_COMPILER_TMP_ASG(base_asg_idx,
                           base_tmp_idx,
                           TRUE,	/* Semantics done */
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      ATD_AUTO_BASE_IDX(tmp_idx)	= base_tmp_idx;

      determine_tmp_size(&size_opnd, exp_desc->type_idx);

      NTR_IR_TBL(max_idx);
      IR_OPR(max_idx) = Max_Opr;
      IR_TYPE_IDX(max_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(max_idx) = line;
      IR_COL_NUM(max_idx)  = col;
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

      /* generate "alloc(contig_array)" before call */
      NTR_IR_TBL(alloc_idx);
      IR_OPR(alloc_idx) = Alloc_Opr;
      IR_TYPE_IDX(alloc_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(alloc_idx) = line;
      IR_COL_NUM(alloc_idx)  = col;
      COPY_OPND(IR_OPND_L(alloc_idx), size_opnd);
      IR_FLD_R(base_asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(base_asg_idx) = alloc_idx;

      gen_sh(Before, Assignment_Stmt, line,
                      col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = base_asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* sjc: generate dealloc(contig_array) */
      NTR_IR_TBL(dealloc_idx);
      IR_OPR(dealloc_idx) = Dealloc_Opr;
      IR_TYPE_IDX(dealloc_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(dealloc_idx) = line;
      IR_COL_NUM(dealloc_idx)  = col;
      COPY_OPND(IR_OPND_L(dealloc_idx), IR_OPND_L(base_asg_idx));

      if (save_where_dealloc_stmt) {

         if (alloc_block_start_idx) {
            curr_stmt_sh_idx = alloc_block_end_idx;
            gen_sh(After, Assignment_Stmt, line,
                         col, FALSE, FALSE, TRUE);

            SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         }
         else {
            where_dealloc_stmt_idx			 = ntr_sh_tbl();
            SH_STMT_TYPE(where_dealloc_stmt_idx)    = Assignment_Stmt;
            SH_GLB_LINE(where_dealloc_stmt_idx)     = line;
            SH_COL_NUM(where_dealloc_stmt_idx)      = col;
            SH_COMPILER_GEN(where_dealloc_stmt_idx) = TRUE;
            SH_P2_SKIP_ME(where_dealloc_stmt_idx)   = TRUE;

            SH_IR_IDX(where_dealloc_stmt_idx)       = dealloc_idx;
         }
      }
      else if (orig_sh_idx != NULL_IDX) {
         save_curr_stmt_sh_idx = curr_stmt_sh_idx;
         curr_stmt_sh_idx = orig_sh_idx;

         if (contig_test_ir_idx != NULL_IDX) {

            true_start_sh_idx = curr_stmt_sh_idx;
            true_end_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

            /* gen the branch around test */

            gen_opnd(&opnd, contig_test_ir_idx, IR_Tbl_Idx, line, col);
            copy_subtree(&opnd, &opnd);
            ir_idx = OPND_IDX(opnd);
            IR_OPR(ir_idx) = Ne_Opr;

            /* insert the dealloc stmt */

            gen_sh(After, Assignment_Stmt, line,
                         col, FALSE, FALSE, TRUE);

            SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

 
            gen_if_stmt(&opnd,
                        SH_NEXT_IDX(true_start_sh_idx),
                        SH_PREV_IDX(true_end_sh_idx),
                        NULL_IDX,
                        NULL_IDX,
                        line,
                        col);

         }
         else {
            gen_sh(After, Assignment_Stmt, line,
                         col, FALSE, FALSE, TRUE);

            SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         }

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }
      else {
         gen_sh(After, Assignment_Stmt, line,
                         col, FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      }
   }

   if (alloc_block_start_idx &&
       save_where_dealloc_stmt) {
      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }

#ifdef KEY /* Bug 8117 */
   if (!call) {
#endif /* KEY Bug 8117 */
     COPY_OPND(IR_OPND_R(asg_idx), (*r_opnd));

# ifdef _TRANSFORM_CHAR_SEQUENCE
   if (exp_desc->type == Structure &&
       ATT_CHAR_SEQ(TYP_IDX(exp_desc->type_idx))) {

      COPY_OPND(opnd, IR_OPND_L(asg_idx));
      transform_char_sequence_ref(&opnd, exp_desc->type_idx);
      COPY_OPND(IR_OPND_L(asg_idx), opnd);

      COPY_OPND(opnd, IR_OPND_R(asg_idx));
      transform_char_sequence_ref(&opnd, exp_desc->type_idx);
      COPY_OPND(IR_OPND_R(asg_idx), opnd);
   }
# endif
#ifdef KEY /* Bug 8117 */
   }
#endif /* KEY Bug 8117 */

   if (intent == Intent_In || intent == Intent_Inout) {
#ifdef KEY /* Bug 8117 */
      gen_sh(Before, call ? Call_Stmt : Assignment_Stmt, line, col, FALSE,
        FALSE, TRUE);
#else /* KEY Bug 8117 */
      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
#endif /* KEY Bug 8117 */

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }

   if (intent == Intent_Out || intent == Intent_Inout) {
      /* sjc: Generate noncontig_array = contig_array */
#ifdef KEY /* Bug 8117 */
      if (call) {
        asg_idx = build_copyinout_call(Copyout_Attr_Idx, COPYOUT_ENTRY,
	  copyin_fld, copyin_src, /* copyout reverses src and dest */
	  AT_Tbl_Idx, copyin_dest, line, col);
	gen_sh(After, Call_Stmt, stmt_start_line,
	       stmt_start_col, FALSE, FALSE, TRUE);
      }
      else {
#endif /* KEY Bug 8117 */
	NTR_IR_TBL(asg_idx);
	IR_OPR(asg_idx)   = Asg_Opr;
	IR_TYPE_IDX(asg_idx) = exp_desc->type_idx;
	IR_LINE_NUM(asg_idx) = line;
	IR_COL_NUM(asg_idx)  = col;
	IR_RANK(asg_idx)  = exp_desc->rank;
	COPY_OPND(IR_OPND_R(asg_idx), (*left_opnd));
	COPY_OPND(IR_OPND_L(asg_idx), (*r_opnd));

	gen_sh(After, Assignment_Stmt, stmt_start_line,
	       stmt_start_col, FALSE, FALSE, TRUE);
#ifdef KEY /* Bug 8117 */
      }
#endif /* KEY Bug 8117 */

      SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   }

   /* change the flags in the expression descriptor to show it's now a tmp */
 
   exp_desc->constant = FALSE;
   exp_desc->foldable = FALSE;
   exp_desc->will_fold_later = FALSE;

   exp_desc->tmp_reference = TRUE;
   exp_desc->section = FALSE;

   if (exp_desc->rank > 0) {
      exp_desc->contig_array = TRUE;
   }

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(left_opnd);

   TRACE (Func_Exit, "common_create_tmp_asg", NULL);

   return(tmp_idx);

}  /* create_tmp_asg */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      generates a new bounds table entry from information in an exp_desc.   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      r_opnd - right hand side expression. (might be NULL)                  *|
|*      exp_desc - expression descriptor for right hand side expression.      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if constant shape.                                               *|
|*                                                                            *|
\******************************************************************************/

boolean gen_bd_entry(opnd_type		*r_opnd,
                     expr_arg_type	*exp_desc,
		     int		*res_bd_idx,
		     int		line,
		     int		col)

{
   int                  attr_idx;
   int                  bd_idx;
   int                  br_idx;
   int                  ch_asg_idx;
   boolean              constant_shape = TRUE;
   int                  ir_idx;
#ifdef KEY /* Bug 10177 */
   int                  label_idx = 0;
#else /* KEY Bug 10177 */
   int                  label_idx;
#endif /* KEY Bug 10177 */
   expr_arg_type        loc_exp_desc;
   int                  i;
   int                  minus_idx;
   int                  mult_idx;
   opnd_type            num_el_opnd;
   boolean              ok = TRUE;
   opnd_type            opnd;
   int                  plus_idx;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   opnd_type            sm_opnd;
   size_offset_type     stride;
   int                  tmp_idx;


   TRACE (Func_Entry, "gen_bd_entry", NULL);

   bd_idx = reserve_array_ntry(exp_desc->rank);
   BD_RESOLVED(bd_idx) = TRUE;

   for (i = 1; i <= exp_desc->rank; i++) {

      if (shared_bd_idx != NULL_IDX) {
         /* This is a copy for the COPY_ASSUMED_SHAPE directive. */
         /* if shared_bd_idx < 0, this is the first one, else if */
         /* shared_bd_idx > 0 it is the bd_idx where we get XT.  */

         constant_shape = FALSE;

# ifdef _DEBUG
         if (r_opnd == NULL) {
            PRINTMSG(line, 626, Internal, col, "r_opnd", "gen_bd_entry");
         }
# endif
         attr_idx = find_left_attr(r_opnd);

         BD_LB_FLD(bd_idx,i) = BD_LB_FLD(ATD_ARRAY_IDX(attr_idx),i);
         BD_LB_IDX(bd_idx,i) = BD_LB_IDX(ATD_ARRAY_IDX(attr_idx),i);

         /* XT is from the shape operand */

         if (shared_bd_idx < 0) {

            if (OPND_FLD(exp_desc->shape[i-1]) == AT_Tbl_Idx &&
                ATD_CLASS(OPND_IDX(exp_desc->shape[i-1])) == Compiler_Tmp) {

               BD_XT_FLD(bd_idx,i) = OPND_FLD(exp_desc->shape[i-1]);
               BD_XT_IDX(bd_idx,i) = OPND_IDX(exp_desc->shape[i-1]);
            }
            else { /* must do tmp assignments */

               GEN_COMPILER_TMP_ASG(ch_asg_idx,
                                    tmp_idx,
                                    TRUE,    /* Semantics done */
                                    line,
                                    col,
                                    CG_INTEGER_DEFAULT_TYPE,
                                    Priv);

               IR_FLD_R(ch_asg_idx) = OPND_FLD(exp_desc->shape[i-1]);
               IR_IDX_R(ch_asg_idx) = OPND_IDX(exp_desc->shape[i-1]);
               IR_LINE_NUM_R(ch_asg_idx) = line;
               IR_COL_NUM_R(ch_asg_idx)  = col;

               if (reassign_XT_temps) {
                  /* gen the branch around test */

                  label_idx = gen_internal_lbl(line);

                  NTR_IR_TBL(br_idx);
                  IR_OPR(br_idx) = Br_True_Opr;
                  IR_TYPE_IDX(br_idx) = LOGICAL_DEFAULT_TYPE;
                  IR_LINE_NUM(br_idx) = line;
                  IR_COL_NUM(br_idx)  = col;

                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx) = Ge_Opr;
                  IR_TYPE_IDX(ir_idx) = LOGICAL_DEFAULT_TYPE;
                  IR_LINE_NUM(ir_idx) = line;
                  IR_COL_NUM(ir_idx)  = col;

                  IR_FLD_L(ir_idx) = AT_Tbl_Idx;
                  IR_IDX_L(ir_idx) = tmp_idx;
                  IR_LINE_NUM_L(ir_idx) = line;
                  IR_COL_NUM_L(ir_idx)  = col;

                  IR_FLD_R(ir_idx) = CN_Tbl_Idx;
                  IR_IDX_R(ir_idx) = CN_INTEGER_ZERO_IDX;
                  IR_LINE_NUM_R(ir_idx) = line;
                  IR_COL_NUM_R(ir_idx)  = col;

                  IR_FLD_L(br_idx) = IR_Tbl_Idx;
                  IR_IDX_L(br_idx) = ir_idx;
                  IR_FLD_R(br_idx) = AT_Tbl_Idx;
                  IR_IDX_R(br_idx) = label_idx;
                  IR_LINE_NUM_R(br_idx) = line;
                  IR_COL_NUM_R(br_idx)  = col;

                  gen_sh(Before, If_Stmt, line,
                            col, FALSE, FALSE, TRUE);

                  SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = br_idx;
                  SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
               }

               /* insert the assignment */

               gen_sh(Before, Assignment_Stmt, line,
                               col , FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))      = ch_asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))  = TRUE;

               gen_copyin_bounds_stmt(tmp_idx);

               if (reassign_XT_temps) {
                  /* gen the label stmt */

                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx)              = Label_Opr;
                  IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
                  IR_LINE_NUM(ir_idx)         = line;
                  IR_COL_NUM(ir_idx)          = col;
                  IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
                  IR_IDX_L(ir_idx)            = label_idx;
                  IR_COL_NUM_L(ir_idx)        = col;
                  IR_LINE_NUM_L(ir_idx)       = line;

                  AT_DEFINED(label_idx)      = TRUE;

                  gen_sh(Before, Continue_Stmt, line, col,
                         FALSE, FALSE, TRUE);
                  SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
                  SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

                  ATL_DEF_STMT_IDX(label_idx)=SH_PREV_IDX(curr_stmt_sh_idx);
               }

               BD_XT_FLD(bd_idx, i)          = AT_Tbl_Idx;
               BD_XT_IDX(bd_idx, i)          = tmp_idx;
               OPND_FLD(exp_desc->shape[i-1])        = AT_Tbl_Idx;
               OPND_IDX(exp_desc->shape[i-1])        = tmp_idx;
               SHAPE_FOLDABLE(exp_desc->shape[i-1])  = FALSE;
               SHAPE_WILL_FOLD_LATER(exp_desc->shape[i-1]) = FALSE;
            }
         }
         else {
            /* get the shared XT */
            BD_XT_FLD(bd_idx,i) = BD_XT_FLD(shared_bd_idx,i);
            BD_XT_IDX(bd_idx,i) = BD_XT_IDX(shared_bd_idx,i);

            if (reassign_XT_temps) {
               /* generate an assignment to the shared xt temp */

               /* gen the branch around test */

               label_idx = gen_internal_lbl(line);

               NTR_IR_TBL(br_idx);
               IR_OPR(br_idx) = Br_True_Opr;
               IR_TYPE_IDX(br_idx) = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(br_idx) = line;
               IR_COL_NUM(br_idx)  = col;

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx) = Ge_Opr;
               IR_TYPE_IDX(ir_idx) = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx) = line;
               IR_COL_NUM(ir_idx)  = col;

               IR_FLD_L(ir_idx) = AT_Tbl_Idx;
               IR_IDX_L(ir_idx) = BD_XT_IDX(bd_idx,i);
               IR_LINE_NUM_L(ir_idx) = line;
               IR_COL_NUM_L(ir_idx)  = col;

               IR_FLD_R(ir_idx) = CN_Tbl_Idx;
               IR_IDX_R(ir_idx) = CN_INTEGER_ZERO_IDX;
               IR_LINE_NUM_R(ir_idx) = line;
               IR_COL_NUM_R(ir_idx)  = col;

               IR_FLD_L(br_idx) = IR_Tbl_Idx;
               IR_IDX_L(br_idx) = ir_idx;
               IR_FLD_R(br_idx) = AT_Tbl_Idx;
               IR_IDX_R(br_idx) = label_idx;
               IR_LINE_NUM_R(br_idx) = line;
               IR_COL_NUM_R(br_idx)  = col;

               gen_sh(Before, If_Stmt, line,
                            col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = br_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

               /* insert the assignment */

               NTR_IR_TBL(ch_asg_idx);
               IR_OPR(ch_asg_idx) = Asg_Opr;
               IR_TYPE_IDX(ch_asg_idx) = ATD_TYPE_IDX(BD_XT_IDX(bd_idx,i));
               IR_LINE_NUM(ch_asg_idx) = line;
               IR_COL_NUM(ch_asg_idx)  = col;
               IR_LINE_NUM_L(ch_asg_idx) = line;
               IR_COL_NUM_L(ch_asg_idx)  = col;

               IR_FLD_L(ch_asg_idx) = AT_Tbl_Idx;
               IR_IDX_L(ch_asg_idx) = BD_XT_IDX(bd_idx,i);

               IR_FLD_R(ch_asg_idx) = OPND_FLD(exp_desc->shape[i-1]);
               IR_IDX_R(ch_asg_idx) = OPND_IDX(exp_desc->shape[i-1]);
               IR_LINE_NUM_R(ch_asg_idx) = line;
               IR_COL_NUM_R(ch_asg_idx)  = col;

               gen_sh(Before, Assignment_Stmt, line,
                               col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))      = ch_asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))  = TRUE;

               gen_copyin_bounds_stmt(BD_XT_IDX(bd_idx,i));

               /* gen the label stmt */

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)              = Label_Opr;
               IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)         = line;
               IR_COL_NUM(ir_idx)          = col;
               IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
               IR_IDX_L(ir_idx)            = label_idx;
               IR_COL_NUM_L(ir_idx)        = col;
               IR_LINE_NUM_L(ir_idx)       = line;

               AT_DEFINED(label_idx)      = TRUE;

               gen_sh(Before, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

               ATL_DEF_STMT_IDX(label_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
            }
         }

         /* calculate UB from UB = LB - 1 + XT */

         if (BD_LB_FLD(bd_idx, i) == CN_Tbl_Idx &&
             compare_cn_and_value(BD_LB_IDX(bd_idx, i), 1L, Eq_Opr)) {

            BD_UB_FLD(bd_idx,i) = BD_XT_FLD(bd_idx,i);
            BD_UB_IDX(bd_idx,i) = BD_XT_IDX(bd_idx,i);
         }
         else {
            NTR_IR_TBL(plus_idx);
            IR_OPR(plus_idx) = Plus_Opr;
            IR_LINE_NUM(plus_idx) = line;
            IR_COL_NUM(plus_idx) = col;
            IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_FLD_R(plus_idx) = BD_XT_FLD(bd_idx,i);
            IR_IDX_R(plus_idx) = BD_XT_IDX(bd_idx,i);
            IR_LINE_NUM_R(plus_idx) = line;
            IR_COL_NUM_R(plus_idx) = col;

            NTR_IR_TBL(minus_idx);
            IR_OPR(minus_idx) = Minus_Opr;
            IR_LINE_NUM(minus_idx) = line;
            IR_COL_NUM(minus_idx) = col;
            IR_TYPE_IDX(minus_idx) = SA_INTEGER_DEFAULT_TYPE;
            IR_FLD_L(minus_idx) = BD_LB_FLD(bd_idx,i);
            IR_IDX_L(minus_idx) = BD_LB_IDX(bd_idx,i);
            IR_LINE_NUM_L(minus_idx) = line;
            IR_COL_NUM_L(minus_idx) = col;
            IR_FLD_R(minus_idx) = CN_Tbl_Idx;
            IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
            IR_LINE_NUM_R(minus_idx) = line;
            IR_COL_NUM_R(minus_idx) = col;

            if (BD_LB_FLD(bd_idx, i) == CN_Tbl_Idx) {
               OPND_FLD(opnd) = IR_Tbl_Idx;
               OPND_IDX(opnd) = minus_idx;

               save_xref_state = xref_state;
               xref_state      = CIF_No_Usage_Rec;
               save_expr_mode  = expr_mode;
               expr_mode       = Regular_Expr;

               loc_exp_desc.rank   = 0;
               ok = expr_semantics(&opnd, &loc_exp_desc);
               xref_state = save_xref_state;
               expr_mode  = save_expr_mode;

               COPY_OPND(IR_OPND_L(plus_idx), opnd);
            }
            else {
               IR_FLD_L(plus_idx) = IR_Tbl_Idx;
               IR_IDX_L(plus_idx) = minus_idx;
            }

            GEN_COMPILER_TMP_ASG(ch_asg_idx,
                                 tmp_idx,
                                 TRUE,    /* Semantics done */
                                 line,
                                 col,
                                 SA_INTEGER_DEFAULT_TYPE,
                                 Priv);

            IR_FLD_R(ch_asg_idx) = IR_Tbl_Idx;
            IR_IDX_R(ch_asg_idx) = plus_idx;
            IR_LINE_NUM_R(ch_asg_idx) = line;
            IR_COL_NUM_R(ch_asg_idx)  = col;

            gen_sh(Before, Assignment_Stmt, line,
                            col, FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))      = ch_asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))  = TRUE;

            gen_copyin_bounds_stmt(tmp_idx);

            BD_UB_FLD(bd_idx, i) = AT_Tbl_Idx;
            BD_UB_IDX(bd_idx, i) = tmp_idx;
         }
      }
      else {

         BD_LB_FLD(bd_idx,i) = CN_Tbl_Idx;
         BD_LB_IDX(bd_idx,i) = CN_INTEGER_ONE_IDX;

         if (OPND_FLD(exp_desc->shape[i-1]) == CN_Tbl_Idx) {
            BD_UB_FLD(bd_idx,i) = OPND_FLD(exp_desc->shape[i-1]);
            BD_UB_IDX(bd_idx,i) = OPND_IDX(exp_desc->shape[i-1]);
         }
         else {
            constant_shape = FALSE;

            if (OPND_FLD(exp_desc->shape[i-1]) == AT_Tbl_Idx &&
                ATD_CLASS(OPND_IDX(exp_desc->shape[i-1])) == Compiler_Tmp) {

               BD_UB_FLD(bd_idx,i) = OPND_FLD(exp_desc->shape[i-1]);
               BD_UB_IDX(bd_idx,i) = OPND_IDX(exp_desc->shape[i-1]);
            }
            else { /* must do tmp assignments */
               GEN_COMPILER_TMP_ASG(ch_asg_idx,
                                    tmp_idx,
                                    TRUE,    /* Semantics done */
                                    line,
                                    col,
                                    SA_INTEGER_DEFAULT_TYPE,
                                    Priv);

               IR_FLD_R(ch_asg_idx) = OPND_FLD(exp_desc->shape[i-1]);
               IR_IDX_R(ch_asg_idx) = OPND_IDX(exp_desc->shape[i-1]);
               IR_LINE_NUM_R(ch_asg_idx) = line;
               IR_COL_NUM_R(ch_asg_idx)  = col;

               gen_sh(Before, Assignment_Stmt, line,
                               col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))      = ch_asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))  = TRUE;

               gen_copyin_bounds_stmt(tmp_idx);

               BD_UB_FLD(bd_idx, i)          = AT_Tbl_Idx;
               BD_UB_IDX(bd_idx, i)          = tmp_idx;
               OPND_FLD(exp_desc->shape[i-1])        = AT_Tbl_Idx;
               OPND_IDX(exp_desc->shape[i-1])        = tmp_idx;
               SHAPE_FOLDABLE(exp_desc->shape[i-1])  = FALSE;
               SHAPE_WILL_FOLD_LATER(exp_desc->shape[i-1]) = FALSE;
            }
         }

         BD_XT_FLD(bd_idx,i) = BD_UB_FLD(bd_idx,i);
         BD_XT_IDX(bd_idx,i) = BD_UB_IDX(bd_idx,i);
      }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX))
      if (BD_UB_FLD(bd_idx,i) == AT_Tbl_Idx &&
          AT_OBJ_CLASS(BD_UB_IDX(bd_idx,i)) == Data_Obj &&
          ATD_CLASS(BD_UB_IDX(bd_idx,i)) == Compiler_Tmp &&
          ATD_TASK_PRIVATE(BD_UB_IDX(bd_idx,i))) {

         /* need to initialize this to one to hack away an mp */
         /* lowerer problem. The lowerer will try to allocate */
         /* the array as a vla on entry to the parallel region */
         /* Setting the bound to one prevents seg faults.      */

         gen_temp_init(BD_UB_IDX(bd_idx,i),
                       CN_INTEGER_ONE_IDX);
      }
# endif
   }

   if (exp_desc->type == Character &&
       r_opnd != NULL) {
      ok = validate_char_len(r_opnd, exp_desc);
   }

   if (exp_desc->type == Character &&
       TYP_FLD(exp_desc->type_idx) != CN_Tbl_Idx) {
      constant_shape = FALSE;
   }

   BD_RANK(bd_idx)           = exp_desc->rank;
   BD_LINE_NUM(bd_idx)       = line;
   BD_COLUMN_NUM(bd_idx)     = col;

   determine_num_elements(&num_el_opnd, exp_desc, line, col);

   loc_exp_desc.rank = 0;
   save_xref_state   = xref_state;
   xref_state        = CIF_No_Usage_Rec;

   ok = expr_semantics(&num_el_opnd, &loc_exp_desc);

   xref_state        = save_xref_state;

   if (loc_exp_desc.constant) {
      BD_LEN_FLD(bd_idx)     = CN_Tbl_Idx;
      BD_LEN_IDX(bd_idx)     = OPND_IDX(num_el_opnd);
   }
   else if (OPND_FLD(num_el_opnd)            == AT_Tbl_Idx    &&
            ATD_CLASS(OPND_IDX(num_el_opnd)) == Compiler_Tmp) {
      BD_LEN_FLD(bd_idx)     = AT_Tbl_Idx;
      BD_LEN_IDX(bd_idx)     = OPND_IDX(num_el_opnd);
   }
   else { /* tmp assign the num_elements */

      GEN_COMPILER_TMP_ASG(ch_asg_idx,
                           tmp_idx,
                           TRUE,     /* Semantics done */
                           line,
                           col,
                           loc_exp_desc.type_idx,
                           Priv);

      COPY_OPND(IR_OPND_R(ch_asg_idx), num_el_opnd);
      gen_sh(Before, Assignment_Stmt, line,
                      col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))       = ch_asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))   = TRUE;

      BD_LEN_FLD(bd_idx) = AT_Tbl_Idx;
      BD_LEN_IDX(bd_idx) = tmp_idx;
   }
   BD_ARRAY_CLASS(bd_idx)    = Explicit_Shape;

   if (constant_shape) {
      BD_ARRAY_SIZE(bd_idx)  = Constant_Size;
   }
   else {
      BD_ARRAY_SIZE(bd_idx)  = Var_Len_Array;
   }

   set_stride_for_first_dim(exp_desc->type_idx, &stride);

   BD_SM_FLD(bd_idx, 1) = stride.fld;
   BD_SM_IDX(bd_idx, 1) = stride.idx;

   for (i = 2; i <= BD_RANK(bd_idx); i++) {
      NTR_IR_TBL(mult_idx);      /* Create Stride * Extent */
      IR_OPR(mult_idx)           = Mult_Opr;
      IR_TYPE_IDX(mult_idx)      = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx)      = line;
      IR_COL_NUM(mult_idx)       = col;
      IR_FLD_L(mult_idx)         = BD_SM_FLD(bd_idx, i - 1);
      IR_IDX_L(mult_idx)         = BD_SM_IDX(bd_idx, i - 1);
      IR_LINE_NUM_L(mult_idx)    = line;
      IR_COL_NUM_L(mult_idx)     = col;
      IR_FLD_R(mult_idx)         = BD_XT_FLD(bd_idx, i - 1);
      IR_IDX_R(mult_idx)         = BD_XT_IDX(bd_idx, i - 1);
      IR_LINE_NUM_R(mult_idx)    = line;
      IR_COL_NUM_R(mult_idx)     = col;
      OPND_FLD(sm_opnd)          = IR_Tbl_Idx;
      OPND_IDX(sm_opnd)          = mult_idx;

      loc_exp_desc.rank = 0;
      save_xref_state   = xref_state;
      xref_state        = CIF_No_Usage_Rec;

      ok = expr_semantics(&sm_opnd, &loc_exp_desc);

      xref_state        = save_xref_state;

      if (loc_exp_desc.constant) {
         BD_SM_FLD(bd_idx, i) = CN_Tbl_Idx;
         BD_SM_IDX(bd_idx, i) = OPND_IDX(sm_opnd);
      }
      else if (OPND_FLD(sm_opnd)            == AT_Tbl_Idx    &&
               ATD_CLASS(OPND_IDX(sm_opnd)) == Compiler_Tmp) {
         BD_SM_FLD(bd_idx, i) = AT_Tbl_Idx;
         BD_SM_IDX(bd_idx, i) = OPND_IDX(sm_opnd);
      }
      else {

         GEN_COMPILER_TMP_ASG(ch_asg_idx,
                              tmp_idx,
                              TRUE,  /* Semantics done */
                              line,
                              col,
                              loc_exp_desc.type_idx,
                              Priv);

         COPY_OPND(IR_OPND_R(ch_asg_idx), sm_opnd);
         gen_sh(Before, Assignment_Stmt, line,
                         col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = ch_asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         BD_SM_FLD(bd_idx, i) = AT_Tbl_Idx;
         BD_SM_IDX(bd_idx, i) = tmp_idx;
      }
   }

   BD_FLOW_DEPENDENT(bd_idx) = TRUE;

   *res_bd_idx = ntr_array_in_bd_tbl(bd_idx);

   TRACE (Func_Exit, "gen_bd_entry", NULL);

   return(constant_shape);

}  /* gen_bd_entry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Creates a temp dope vector assignment stmt.                           *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx - IL idx for actual arg.                                     *|
|*      info_idx - arg_info_list idx.                                         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	attr idx for temp dope vector.                                        *|
|*									      *|
\******************************************************************************/

static int	create_tmp_DV_asg(int	list_idx,
				  int	info_idx)

{
   int		asg_idx;
   int		column;
   int		line;
   int		tmp_idx;


   TRACE (Func_Entry, "create_tmp_DV_asg", NULL);

   find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                             &line,
                             &column);

# ifdef _DEBUG
      if (info_idx == NULL_IDX) {
         PRINTMSG(line, 626, Internal, column,
                  "valid info_idx", "create_tmp_DV_asg");
      }
# endif

   GEN_COMPILER_TMP_ASG(asg_idx,
                        tmp_idx,
                        TRUE,	/* Semantics done */
                        line,
                        column,
                        arg_info_list[info_idx].ed.type_idx,
                        Priv);

   IR_OPR(asg_idx)           = Dv_Whole_Copy_Opr;
   IR_DV_DIM(asg_idx)        = arg_info_list[info_idx].ed.rank;
   ATD_IM_A_DOPE(tmp_idx)    = TRUE;
   ATD_ARRAY_IDX(tmp_idx)    = ATD_ARRAY_IDX(find_base_attr(&IL_OPND(list_idx),
                                                            &line, &column));
   

   if (IL_FLD(list_idx)         == IR_Tbl_Idx &&
       IR_OPR(IL_IDX(list_idx)) == Whole_Substring_Opr) {

      COPY_OPND(IL_OPND(list_idx), IR_OPND_L(IL_IDX(list_idx)));
   }

   if (IL_FLD(list_idx)         == IR_Tbl_Idx &&
       IR_OPR(IL_IDX(list_idx)) == Whole_Subscript_Opr) {

      COPY_OPND(IL_OPND(list_idx), IR_OPND_L(IL_IDX(list_idx)));
   }

   /* had better be a deref_opr */

   if (IL_FLD(list_idx)         != IR_Tbl_Idx ||
       IR_OPR(IL_IDX(list_idx)) != Dv_Deref_Opr) {

      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                &line,
                                &column);
      PRINTMSG(line, 861, Internal, column,
               "create_tmp_DV_asg");
   }

   COPY_OPND(IR_OPND_R(asg_idx), IR_OPND_L(IL_IDX(list_idx)));

   gen_sh(Before, Assignment_Stmt, stmt_start_line,
          stmt_start_col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "create_tmp_DV_asg", NULL);

   return(tmp_idx);

}  /* create_tmp_DV_asg */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Find the correct attr to set AT_ACTUAL_ARG and set it.                *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx - IL idx for actual arg.                                     *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void set_at_actual_arg(int	list_idx)

{
   opnd_type		opnd;

   TRACE (Func_Entry, "set_at_actual_arg", NULL);

   COPY_OPND(opnd, IL_OPND(list_idx));

   while (TRUE) {
      switch (OPND_FLD(opnd)) {
         case AT_Tbl_Idx :
            AT_ACTUAL_ARG(OPND_IDX(opnd)) = TRUE;
            goto EXIT;
   
         case IR_Tbl_Idx :
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            break;

         default :
            goto EXIT;
      }
   }

EXIT:

   TRACE (Func_Exit, "set_at_actual_arg", NULL);

   return;

}  /* set_at_actual_arg */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Creates an operand that holds the size needed for a heap array tmp.   *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - holds number of elements on entry.                             *|
|*      type_idx - type tbl idx for tmp.                                      *|
|*									      *|
|* Output parameters:							      *|
|*	opnd - holds resultant expression for tmp size.                       *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void determine_tmp_size(opnd_type     	*opnd,
			int		 type_idx)

{
   int			col;
   size_offset_type	length;
   int			line;
   expr_arg_type	loc_exp_desc;
   int          	mult_idx;
   size_offset_type	result;
   cif_usage_code_type  save_xref_state;

# if !defined(_HEAP_REQUEST_IN_BYTES)
   long_type		constant;
   int			div_idx;
   int			plus_idx;
# endif


   TRACE (Func_Entry, "determine_tmp_size", NULL);

   find_opnd_line_and_column(opnd, &line, &col);

# ifdef _HEAP_REQUEST_IN_BYTES

   /* solaris compiler expects heap request in bytes */

   /* opnd HOLDS THE NUMBER OF ELEMENTS ON ENTRY. */

   if (TYP_TYPE(type_idx) == Character) {

      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx)  = col;
      COPY_OPND(IR_OPND_L(mult_idx), (*opnd));
      IR_FLD_R(mult_idx) = TYP_FLD(type_idx);
      IR_IDX_R(mult_idx) = TYP_IDX(type_idx);
      IR_LINE_NUM_R(mult_idx) = line;
      IR_COL_NUM_R(mult_idx)  = col;


      OPND_FLD((*opnd)) = IR_Tbl_Idx;
      OPND_IDX((*opnd)) = mult_idx;
   }
   else if (TYP_TYPE(type_idx) == Structure &&
            ATT_CHAR_SEQ(TYP_IDX(type_idx))) {
      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx)  = col;
      COPY_OPND(IR_OPND_L(mult_idx), (*opnd));

# if defined(_DEBUG)

      if (ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx)) != CN_Tbl_Idx) {
         PRINTMSG(line, 1201, Internal, col,
                  AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
      }

# endif

      result.idx	= CN_INTEGER_CHAR_BIT_IDX;
      result.fld	= CN_Tbl_Idx;
      length.fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
      length.idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

      size_offset_binary_calc(&length,
                              &result,
                               Div_Opr,
                              &result);

      if (result.fld == NO_Tbl_Idx) {
          IR_FLD_R(mult_idx) = CN_Tbl_Idx;
          IR_IDX_R(mult_idx) = ntr_const_tbl(result.type_idx,
                                             FALSE,
                                             result.constant);
      }
      else {
         IR_FLD_R(mult_idx)	= result.fld;
         IR_IDX_R(mult_idx)	= result.idx;
      }

      IR_LINE_NUM_R(mult_idx)	= line;
      IR_COL_NUM_R(mult_idx)	= col;

      OPND_FLD((*opnd)) = IR_Tbl_Idx;
      OPND_IDX((*opnd)) = mult_idx;
   }
   else {
      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx)  = col;
      COPY_OPND(IR_OPND_L(mult_idx), (*opnd));

      if (TYP_TYPE(type_idx) == Structure) {
         result.idx	= CN_INTEGER_CHAR_BIT_IDX;
         result.fld	= CN_Tbl_Idx;
         length.fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
         length.idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

# if defined(_DEBUG)

         if (length.fld != CN_Tbl_Idx) {
            PRINTMSG(line, 1201, Internal, col,
                     AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
         }
# endif

         size_offset_binary_calc(&length,
                                 &result,
                                  Div_Opr,
                                 &result);
      }
      else {
         C_TO_F_INT(result.constant,
                    (storage_bit_size_tbl[TYP_LINEAR(type_idx)] / 8),
                    CG_INTEGER_DEFAULT_TYPE);
         result.type_idx    = CG_INTEGER_DEFAULT_TYPE;
         result.fld	    = NO_Tbl_Idx;
      }

      if (result.fld == NO_Tbl_Idx) {
         IR_FLD_R(mult_idx) = CN_Tbl_Idx;
         IR_IDX_R(mult_idx) = ntr_const_tbl(result.type_idx,
                                            FALSE,
                                            result.constant);
      }
      else {
         IR_FLD_R(mult_idx) = result.fld;
         IR_IDX_R(mult_idx) = result.idx;
      }

      IR_LINE_NUM_R(mult_idx)	= line;
      IR_COL_NUM_R(mult_idx)	= col;

      OPND_FLD((*opnd)) = IR_Tbl_Idx;
      OPND_IDX((*opnd)) = mult_idx;
   }

# else

   /* cray compiler expects heap request in words */

   /* opnd HOLDS THE NUMBER OF ELEMENTS ON ENTRY. */

   if (TYP_TYPE(type_idx) == Character) {

      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx)  = col;
      COPY_OPND(IR_OPND_L(mult_idx), (*opnd));
      IR_FLD_R(mult_idx) = TYP_FLD(type_idx);
      IR_IDX_R(mult_idx) = TYP_IDX(type_idx);
      IR_LINE_NUM_R(mult_idx) = line;
      IR_COL_NUM_R(mult_idx)  = col;

      /* now calculate word size from byte size */
      NTR_IR_TBL(div_idx);
      IR_OPR(div_idx)   = Div_Opr;
      IR_TYPE_IDX(div_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(div_idx) = line;
      IR_COL_NUM(div_idx)  = col;
      IR_FLD_R(div_idx) = CN_Tbl_Idx;
      constant          = TARGET_CHARS_PER_WORD;
      IR_IDX_R(div_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                      TARGET_CHARS_PER_WORD);
      IR_LINE_NUM_R(div_idx) = line;
      IR_COL_NUM_R(div_idx)  = col;

      NTR_IR_TBL(plus_idx);
      IR_OPR(plus_idx)  = Plus_Opr;
      IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(plus_idx) = line;
      IR_COL_NUM(plus_idx)  = col;
      IR_FLD_L(div_idx) = IR_Tbl_Idx;
      IR_IDX_L(div_idx) = plus_idx;
 
      constant--;
      IR_FLD_R(plus_idx) = CN_Tbl_Idx;
      IR_IDX_R(plus_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                       constant);
      IR_LINE_NUM_R(plus_idx) = line;
      IR_COL_NUM_R(plus_idx)  = col;

      IR_FLD_L(plus_idx) = IR_Tbl_Idx;
      IR_IDX_L(plus_idx) = mult_idx;

      OPND_FLD((*opnd)) = IR_Tbl_Idx;
      OPND_IDX((*opnd)) = div_idx;
   }
   else if (TYP_TYPE(type_idx) == Structure &&
            ATT_CHAR_SEQ(TYP_IDX(type_idx))) {

      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx)  = col;
      COPY_OPND(IR_OPND_L(mult_idx), (*opnd));

# if defined(_DEBUG)

      if (ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx)) != CN_Tbl_Idx) {
         PRINTMSG(line, 1201, Internal, col,
                  AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
      }

# endif

      result.idx	= CN_INTEGER_CHAR_BIT_IDX;
      result.fld	= CN_Tbl_Idx;
      length.fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
      length.idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));

      size_offset_binary_calc(&length,
                              &result,
                               Div_Opr,
                              &result);

      if (result.fld == NO_Tbl_Idx) {
         IR_FLD_R(mult_idx)	= CN_Tbl_Idx;
         IR_IDX_R(mult_idx)	= ntr_const_tbl(result.type_idx,
                                                FALSE,
                                                result.constant);
      }
      else {
         IR_FLD_R(mult_idx)	= result.fld;
         IR_IDX_R(mult_idx)	= result.idx;
      }

      IR_LINE_NUM_R(mult_idx)	= line;
      IR_COL_NUM_R(mult_idx)	= col;

      /* now calculate word size from byte size */

      NTR_IR_TBL(div_idx);
      IR_OPR(div_idx)   = Div_Opr;
      IR_TYPE_IDX(div_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(div_idx) = line;
      IR_COL_NUM(div_idx)  = col;
      IR_FLD_R(div_idx) = CN_Tbl_Idx;
      constant          = TARGET_CHARS_PER_WORD;
      IR_IDX_R(div_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                      TARGET_CHARS_PER_WORD);
      IR_LINE_NUM_R(div_idx) = line;
      IR_COL_NUM_R(div_idx)  = col;

      NTR_IR_TBL(plus_idx);
      IR_OPR(plus_idx)  = Plus_Opr;
      IR_TYPE_IDX(plus_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(plus_idx) = line;
      IR_COL_NUM(plus_idx)  = col;
      IR_FLD_L(div_idx) = IR_Tbl_Idx;
      IR_IDX_L(div_idx) = plus_idx;

      constant--;
      IR_FLD_R(plus_idx) = CN_Tbl_Idx;
      IR_IDX_R(plus_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                       constant);
      IR_LINE_NUM_R(plus_idx) = line;
      IR_COL_NUM_R(plus_idx)  = col;

      IR_FLD_L(plus_idx) = IR_Tbl_Idx;
      IR_IDX_L(plus_idx) = mult_idx;

      OPND_FLD((*opnd)) = IR_Tbl_Idx;
      OPND_IDX((*opnd)) = div_idx;
   }
   else {

      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx) = Mult_Opr;
      IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(mult_idx) = line;
      IR_COL_NUM(mult_idx)  = col;
      COPY_OPND(IR_OPND_L(mult_idx), (*opnd));

      if (TYP_TYPE(type_idx) == Structure) {
         result.fld	= ATT_STRUCT_BIT_LEN_FLD(TYP_IDX(type_idx));
         result.idx	= ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx));
      }
      else {
         C_TO_F_INT(result.constant, 
                    storage_bit_size_tbl[TYP_LINEAR(type_idx)],
                    CG_INTEGER_DEFAULT_TYPE);
         result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
         result.fld		= NO_Tbl_Idx;
      }

      BITS_TO_WORDS(result, TARGET_BITS_PER_WORD);

      IR_IDX_R(mult_idx) = ntr_const_tbl(result.type_idx,
                                         FALSE,
                                         result.constant);

      IR_FLD_R(mult_idx)	= CN_Tbl_Idx;
      IR_LINE_NUM_R(mult_idx)	= line;
      IR_COL_NUM_R(mult_idx)	= col;
   
      OPND_FLD((*opnd)) = IR_Tbl_Idx;
      OPND_IDX((*opnd)) = mult_idx;
   }

# endif

   loc_exp_desc.rank = 0;
   save_xref_state   = xref_state;
   xref_state        = CIF_No_Usage_Rec;

   expr_semantics(opnd, &loc_exp_desc);

   xref_state        = save_xref_state;

   TRACE (Func_Exit, "determine_tmp_size", NULL);

   return;

}  /* determine_tmp_size */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	When we have a copy in, copy out situation, and have a section ref,   *|
|*      we must save all variable subscript, substring, section values into   *|
|*      tmps. This is because they may be modified by the callee and we have  *|
|*      to copy the array syntax ref back out after the call. This routine    *|
|*      replaces all these variable things with tmps.                         *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx - index to list text for actual arg.                         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void save_array_syntax_subscripts(int list_idx)
				    

{
   int		asg_idx;
   int		column;
   int		i;
   int		ir_idx;
   int		k;
   int		l_idx;
   int		line;
   int		t_idx;
   int		tmp_idx;


   TRACE (Func_Entry, "save_array_syntax_subscripts", NULL);

   ir_idx = IL_IDX(list_idx);

   while (TRUE) {

      if (IR_OPR(ir_idx) == Subscript_Opr         ||
          IR_OPR(ir_idx) == Section_Subscript_Opr ||
          IR_OPR(ir_idx) == Substring_Opr)        {

         l_idx = IR_IDX_R(ir_idx);

         for (k = 0; k < IR_LIST_CNT_R(ir_idx); k++) {

            if (!IL_CONSTANT_SUBSCRIPT(l_idx)) {

               if (IL_FLD(l_idx) == IR_Tbl_Idx &&
                   IR_OPR(IL_IDX(l_idx)) == Triplet_Opr) {

                  t_idx = IR_IDX_L(IL_IDX(l_idx));
                  for (i = 0; i < 3; i++) {

                     if (! IL_CONSTANT_SUBSCRIPT(t_idx)) { /* create tmp */
                        find_opnd_line_and_column((opnd_type *) &IL_OPND(t_idx),
                                                  &line,
                                                  &column);

                        GEN_COMPILER_TMP_ASG(asg_idx, 
                                             tmp_idx,
                                             TRUE,	/* Semantics done */
                                             line,
                                             column,
                                             CG_INTEGER_DEFAULT_TYPE,
                                             Priv);

                        COPY_OPND(IR_OPND_R(asg_idx), IL_OPND(t_idx));

                        gen_sh(Before, Assignment_Stmt, stmt_start_line,
                               stmt_start_col, FALSE, FALSE, TRUE);

                        SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                        SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      
                        IL_FLD(t_idx) = AT_Tbl_Idx;
                        IL_IDX(t_idx) = tmp_idx;
                        IL_LINE_NUM(t_idx) = line;
                        IL_COL_NUM(t_idx)  = column;
                     }
       
                     t_idx = IL_NEXT_LIST_IDX(t_idx);
                  }
               }
               else { /* create tmp */
                  find_opnd_line_and_column((opnd_type *) &IL_OPND(l_idx),
                                            &line,
                                            &column);
                  GEN_COMPILER_TMP_ASG(asg_idx,
                                       tmp_idx,
                                       TRUE,	/* Semantics done */
                                       line,
                                       column,
                                       CG_INTEGER_DEFAULT_TYPE,
                                       Priv);

                  COPY_OPND(IR_OPND_R(asg_idx), IL_OPND(l_idx));

                  gen_sh(Before, Assignment_Stmt, stmt_start_line,
                         stmt_start_col, FALSE, FALSE, TRUE);

                  SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                  SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

                  IL_FLD(l_idx) = AT_Tbl_Idx;
                  IL_IDX(l_idx) = tmp_idx;
                  IL_LINE_NUM(l_idx) = line;
                  IL_COL_NUM(l_idx)  = column;
               }
            }

            l_idx = IL_NEXT_LIST_IDX(l_idx);
         }
      }

      if (IR_FLD_L(ir_idx) != IR_Tbl_Idx) {
         break;
      }
      else {
         ir_idx = IR_IDX_L(ir_idx);
      }
   }

   TRACE (Func_Exit, "save_array_syntax_subscripts", NULL);

   return;

}  /* save_array_syntax_subscripts */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Determine the number of elements in an array expression.              *|
|*									      *|
|* Input parameters:							      *|
|*      exp_desc - expression descriptor for array expression.                *|
|*									      *|
|* Output parameters:							      *|
|*	opnd - number of elements expression.                                 *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void determine_num_elements(opnd_type     *opnd,
				   expr_arg_type *exp_desc,
                                   int		  line,
     			           int		  col)

{
   int			i;
   int			list_idx;
   int			max_idx;
   int			mult_idx;


   TRACE (Func_Entry, "determine_num_elements", NULL);

   *opnd = null_opnd;

   for (i = 0; i < exp_desc->rank; i++) {
      NTR_IR_TBL(max_idx);
      IR_OPR(max_idx)		= Max_Opr;
      IR_TYPE_IDX(max_idx)	= SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(max_idx)	= line;
      IR_COL_NUM(max_idx)	= col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_L(max_idx)      = IL_Tbl_Idx;
      IR_IDX_L(max_idx)      = list_idx;
      IR_LIST_CNT_L(max_idx) = 2;

      IL_FLD(list_idx)       = CN_Tbl_Idx;
      IL_IDX(list_idx)       = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx)  = IR_LINE_NUM(max_idx);
      IL_COL_NUM(list_idx)   = IR_COL_NUM(max_idx);

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;

      COPY_OPND(IL_OPND(IL_NEXT_LIST_IDX(list_idx)),
                exp_desc->shape[i]);
      IL_LINE_NUM(IL_NEXT_LIST_IDX(list_idx)) = line;
      IL_COL_NUM(IL_NEXT_LIST_IDX(list_idx)) = col;

      if (OPND_FLD((*opnd)) == NO_Tbl_Idx) {
         OPND_FLD((*opnd))  = IR_Tbl_Idx;
         OPND_IDX((*opnd))  = max_idx;
      }
      else {
         NTR_IR_TBL(mult_idx);
         IR_OPR(mult_idx) = Mult_Opr;
         IR_TYPE_IDX(mult_idx) = SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(mult_idx) = line;
         IR_COL_NUM(mult_idx)  = col;

         COPY_OPND(IR_OPND_L(mult_idx), (*opnd));
         IR_FLD_R(mult_idx) = IR_Tbl_Idx;
         IR_IDX_R(mult_idx) = max_idx;

         OPND_IDX((*opnd)) = mult_idx;
      }
   }


   TRACE (Func_Exit, "determine_num_elements", NULL);

   return;

}  /* determine_num_elements */
#ifdef KEY /* Bug 5089 */
/*
 * Unfortunate special case: ieee_value() needs to return a signaling NaN,
 * but the code generated on X86 for -m32 involves a fp "mov" which transforms
 * it into a quiet NaN. So we need to transform a function into a subroutine
 * which stores the value into a temp passed as the first arg by reference,
 * just as we do for functions returning structures and arrays. See macro
 * FUNCTION_MUST_BE_SUBROUTINE.
 */
boolean special_case_fcn_to_sub(int spec_idx) {
  char *name = ATP_EXT_NAME_PTR(spec_idx);
# define IEEE_VALUE_PREFIX "IEEE_VALUE_"
# define EXT_IEEE_VALUE_PREFIX "_Ieee_value_"
  return !(on_off_flags.intrinsic_module_gen ?
    strncmp(AT_OBJ_NAME_PTR(spec_idx), IEEE_VALUE_PREFIX,
      (sizeof IEEE_VALUE_PREFIX) - 1) :
    strncmp(ATP_EXT_NAME_PTR(spec_idx), EXT_IEEE_VALUE_PREFIX,
      (sizeof EXT_IEEE_VALUE_PREFIX) - 1));
}
#endif /* KEY Bug 5089 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine handles function calls in expressions by deciding        *|
|*      whether to pull the function call out of the expression and replace   *|
|*      it with a temp and also decide whether the result requires the        *|
|*      function call to be replaced with a subroutine call where the result  *|
|*      is the first argument.                                                *|
|*									      *|
|* Input parameters:							      *|
|*	result - the tree with the call opr.                                  *|
|*									      *|
|* Output parameters:							      *|
|*	result - opnd pointing to the tmp from the flattened function.        *|
|*									      *|
|* Returns:								      *|
|*	NOTHING                                                               *|
|*									      *|
\******************************************************************************/

void flatten_function_call(opnd_type     *result)
{
   int		       alloc_idx;
   int                 asg_idx;
   int		       attr_idx;
   int		       base_asg_idx;
   opnd_type	       base_opnd;
   int		       base_tmp_idx;
   int		       bd_idx;
   int		       cn_idx;
   int                 col;
   int		       dealloc_idx;
   int		       dv_idx;
   expr_arg_type       elemental_exp_desc;
   expr_arg_type       exp_desc;
   int                 ir_idx;
   int                 line;
   int		       list_idx;
   int		       loc_idx;
   int		       new_stmt_idx;
   int		       num_args;
   boolean             ok;
   opnd_type           opnd;
#ifdef KEY /* Bug 10177 */
   int		       res_list_idx = 0;
#else /* KEY Bug 10177 */
   int		       res_list_idx;
#endif /* KEY Bug 10177 */
   boolean	       save_keep_orig_sh;
   int		       save_orig_sh_idx;
   opnd_type	       size_opnd;
   int		       spec_idx;
   int		       unused1;
   int		       unused2;
   boolean	       save_defer_stmt_expansion;
   int                 tmp_idx		= NULL_IDX;
   int                 type_idx;
   boolean	       variable_size	= FALSE;

# ifdef _ALLOCATE_IS_CALL
   int		       call_idx;
   int		       sub_idx;
   long_type	       the_constant;
   int		       tmp_array_idx;
# endif


   TRACE (Func_Entry, "flatten_function_call", NULL);

   if (no_func_expansion) {
      goto EXIT;
   }

   ir_idx   = OPND_IDX((*result));

   spec_idx = IR_IDX_L(ir_idx);
   attr_idx = ATP_RSLT_IDX(spec_idx);
   type_idx = ATD_TYPE_IDX(attr_idx);

   line     = IR_LINE_NUM(ir_idx);
   col      = IR_COL_NUM(ir_idx);

   save_orig_sh_idx = orig_sh_idx;

   stmt_expansion_control_start();
   save_defer_stmt_expansion = defer_stmt_expansion;
   defer_stmt_expansion = FALSE;

   if (! keep_orig_sh) {
      orig_sh_idx = curr_stmt_sh_idx;
   }

#ifdef KEY /* Bug 4811 */
   /* If the preceding statement is an OMP "atomic" pragma, add the temp
    * assignment before the pragma, so that the pragma still applies to
    * the original assignment statement containing this call.
    */
   int sh_prev = SH_PREV_IDX(curr_stmt_sh_idx);
   int insertion_point = 
     (curr_stmt_sh_idx != SCP_FIRST_SH_IDX(curr_scp_idx) &&
       IR_OPR(SH_IR_IDX(sh_prev)) == Atomic_Open_Mp_Opr) ?
       sh_prev :
       curr_stmt_sh_idx;
     gen_sh_at(Before, Call_Stmt, stmt_start_line, stmt_start_col,
	    FALSE, FALSE, TRUE, insertion_point);
     curr_stmt_sh_idx = SH_PREV_IDX(insertion_point);
#else
   gen_sh(Before, Call_Stmt, stmt_start_line, stmt_start_col,
          FALSE, FALSE, TRUE);

   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
#endif /* KEY Bug 4811 */
   new_stmt_idx = curr_stmt_sh_idx;

   num_args = IR_LIST_CNT_R(ir_idx);

#ifdef KEY /* Bug 5089 */
   boolean fcn_to_sub = FUNCTION_MUST_BE_SUBROUTINE(spec_idx, attr_idx);
   if (fcn_to_sub)
#else /* KEY Bug 5089 */
   if (FUNCTION_MUST_BE_SUBROUTINE(attr_idx))
#endif /* KEY Bug 5089 */
   {

      NTR_IR_LIST_TBL(res_list_idx);
      IL_ARG_DESC_VARIANT(res_list_idx)  = TRUE;
      IL_NEXT_LIST_IDX(res_list_idx)     = IR_IDX_R(ir_idx);
      IR_IDX_R(ir_idx)                   = res_list_idx;
      IR_LIST_CNT_R(ir_idx)++;
   }

   elemental_exp_desc             = init_exp_desc;
   elemental_exp_desc.type_idx    = type_idx;
   elemental_exp_desc.type        = TYP_TYPE(type_idx);
   elemental_exp_desc.linear_type = TYP_LINEAR(type_idx);

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   ok = final_arg_work(&opnd, IR_IDX_L(ir_idx), num_args, &elemental_exp_desc);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   curr_stmt_sh_idx = new_stmt_idx;

#ifdef KEY /* Bug 5089 */
   if (fcn_to_sub || ATP_ELEMENTAL(spec_idx))
#else /* KEY Bug 5089 */
   if (FUNCTION_MUST_BE_SUBROUTINE(attr_idx) || ATP_ELEMENTAL(spec_idx))
#endif /* KEY Bug 5089 */
     {

      bd_idx = ATD_ARRAY_IDX(attr_idx);

      if (TYP_TYPE(type_idx) == Character &&
          TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char &&
          ATP_PROC(IR_IDX_L(ir_idx)) != Dummy_Proc &&
          attr_idx != SCP_ATTR_IDX(curr_scp_idx) &&
          !AT_IS_INTRIN(attr_idx)) {

         PRINTMSG(line, 939, Error, col, AT_OBJ_NAME_PTR(attr_idx));
      }

      if (ATP_PROC(IR_IDX_L(ir_idx)) != Intrin_Proc &&
          (TYP_TYPE(type_idx) == Character &&
           TYP_CHAR_CLASS(type_idx) == Var_Len_Char ||
           bd_idx != NULL_IDX && 
           BD_ARRAY_SIZE(bd_idx) == Var_Len_Array)) {

         save_keep_orig_sh = keep_orig_sh;
         keep_orig_sh = TRUE;

         process_variable_size_func(attr_idx,          /* The result  */
                             IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)),
			     (ATP_EXTRA_DARG(IR_IDX_L(ir_idx)) ?
                              ATP_FIRST_IDX(IR_IDX_L(ir_idx)) + 1 :
                              ATP_FIRST_IDX(IR_IDX_L(ir_idx))),
			     (ATP_EXTRA_DARG(IR_IDX_L(ir_idx)) ?
                              ATP_NUM_DARGS(IR_IDX_L(ir_idx)) - 1 :
                              ATP_NUM_DARGS(IR_IDX_L(ir_idx))),
                             &type_idx,         /* Gets new type idx - if one */
                             &bd_idx);          /* Gets new bd idx - if one   */


         if (! ATD_IM_A_DOPE(attr_idx) &&
             ((TYP_TYPE(type_idx) == Character &&
               TYP_CHAR_CLASS(type_idx) == Var_Len_Char) ||
              (bd_idx != NULL_IDX &&
               BD_ARRAY_SIZE(bd_idx) == Var_Len_Array))) {

            variable_size	= TRUE;
         }

         keep_orig_sh = save_keep_orig_sh;
      }

      if (ATP_PROC(IR_IDX_L(ir_idx)) != Intrin_Proc &&
          TYP_TYPE(type_idx) == Character &&
          TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char &&
          ! ATD_IM_A_DOPE(attr_idx)) {

         variable_size       = TRUE;
      }

      if (ATP_ELEMENTAL(spec_idx) && 
          elemental_exp_desc.rank > 0) {
         /* create new bd entry */
         elemental_exp_desc.type_idx = type_idx;
         variable_size |= ! gen_bd_entry(NULL, &elemental_exp_desc, &bd_idx,
                                        line, col);
      }

      /* need to make it a subroutine */

      tmp_idx = get_stmt_tmp(type_idx,
                             ATD_IM_A_DOPE(attr_idx),
                             (bd_idx == NULL_IDX ? 0 : BD_RANK(bd_idx)));

      if (tmp_idx == NULL_IDX) {

         if (variable_size) {
            tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);
            ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_BASED_IDX(curr_scp_idx);
         }
         else {
            tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);
            ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
         }

         ATD_TYPE_IDX(tmp_idx)	   = type_idx;
         ATD_ARRAY_IDX(tmp_idx)    = bd_idx;
         ATD_IM_A_DOPE(tmp_idx)    = ATD_IM_A_DOPE(attr_idx);
         AT_ACTUAL_ARG(tmp_idx)    = TRUE;
         ATD_POINTER(tmp_idx)      = ATD_POINTER(attr_idx);
         AT_SEMANTICS_DONE(tmp_idx)= TRUE;
      }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      ATD_NOT_PT_UNIQUE_MEM(tmp_idx) = TRUE;
# endif

      ATD_CHAR_LEN_IN_DV(tmp_idx) = ATD_CHAR_LEN_IN_DV(attr_idx);

      if (variable_size) {

         /* now for the alloc and dealloc stmts */

         ATD_AUTOMATIC(tmp_idx)      = TRUE;

         GEN_COMPILER_TMP_ASG(base_asg_idx,
                              base_tmp_idx,
                              TRUE,	/* Semantics done */
                              line,
                              col,
                              SA_INTEGER_DEFAULT_TYPE,
                              Priv);

         ATD_AUTO_BASE_IDX(tmp_idx)	= base_tmp_idx;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         ATD_NOT_PT_UNIQUE_MEM(base_tmp_idx) = TRUE;
# endif

         if (bd_idx) {
            OPND_FLD(size_opnd) = BD_LEN_FLD(bd_idx);
            OPND_IDX(size_opnd) = BD_LEN_IDX(bd_idx);
            OPND_LINE_NUM(size_opnd) = line;
            OPND_COL_NUM(size_opnd)  = col;
         }
         else {
            OPND_FLD(size_opnd) = CN_Tbl_Idx;
            OPND_IDX(size_opnd) = CN_INTEGER_ONE_IDX;
            OPND_LINE_NUM(size_opnd) = line;
            OPND_COL_NUM(size_opnd)  = col;
         }
     
         determine_tmp_size(&size_opnd, 
                            ATD_TYPE_IDX(tmp_idx));

         NTR_IR_TBL(alloc_idx);
         IR_OPR(alloc_idx) = Alloc_Opr;
         IR_TYPE_IDX(alloc_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(alloc_idx) = line;
         IR_COL_NUM(alloc_idx) = col;
         COPY_OPND(IR_OPND_L(alloc_idx), size_opnd);
         IR_FLD_R(base_asg_idx) = IR_Tbl_Idx;
         IR_IDX_R(base_asg_idx) = alloc_idx;

         /* put this before new_stmt_idx (which should be curr_stmt_sh_idx) */

         gen_sh(Before, Assignment_Stmt, stmt_start_line,
                      stmt_start_col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = base_asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         NTR_IR_TBL(dealloc_idx);
         IR_OPR(dealloc_idx) = Dealloc_Opr;
         IR_TYPE_IDX(dealloc_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(dealloc_idx) = line;
         IR_COL_NUM(dealloc_idx) = col;
         COPY_OPND(IR_OPND_L(dealloc_idx), IR_OPND_L(base_asg_idx));

         /* put this after orig_sh_idx */

         curr_stmt_sh_idx = orig_sh_idx;

         gen_sh(After, Assignment_Stmt, stmt_start_line,
                stmt_start_col, FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         curr_stmt_sh_idx = new_stmt_idx;
      }
      else if (ATD_IM_A_DOPE(tmp_idx)) {

         if (AT_IS_INTRIN(IR_IDX_L(ir_idx))) {

            /* need to dealloc the dope vector pointee */

            if (glb_tbl_idx[Dealloc_Attr_Idx] == NULL_IDX) {
               glb_tbl_idx[Dealloc_Attr_Idx] = create_lib_entry_attr(
                                                          DEALLOC_LIB_ENTRY,
                                                          DEALLOC_NAME_LEN,
                                                          line,
                                                          col);
            }

            ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Dealloc_Attr_Idx]);

            /* create array to send to DEALLOC */

            /* these go before "next" stmt which is orig_sh_idx */

            curr_stmt_sh_idx	  = SH_NEXT_IDX(orig_sh_idx);

# ifdef _ALLOCATE_IS_CALL
            tmp_array_idx  = create_alloc_descriptor(1, line, col, FALSE);

            /* put loc of dope vector, tmp_idx, into tmp_array_idx(2) */

            NTR_IR_TBL(sub_idx);
            IR_OPR(sub_idx)		= Subscript_Opr;
            IR_TYPE_IDX(sub_idx)        = ATD_TYPE_IDX(tmp_array_idx);
            IR_LINE_NUM(sub_idx)	= line;
            IR_COL_NUM(sub_idx)		= col;
            IR_FLD_L(sub_idx)		= AT_Tbl_Idx;
            IR_IDX_L(sub_idx)		= tmp_array_idx;
            IR_LINE_NUM_L(sub_idx)	= line;
            IR_COL_NUM_L(sub_idx)	= col;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(sub_idx)		= IL_Tbl_Idx;
            IR_IDX_R(sub_idx)		= list_idx;
            IR_LIST_CNT_R(sub_idx)	= 1;

            IL_FLD(list_idx) = CN_Tbl_Idx;
            the_constant     = 2L;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            if (TYP_LINEAR(ATD_TYPE_IDX(tmp_array_idx)) == Integer_4) {
               the_constant++;
            }
# endif

            IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                           the_constant);
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;

            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;
            IR_FLD_L(asg_idx)    = IR_Tbl_Idx;
            IR_IDX_L(asg_idx)    = sub_idx;
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

            gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

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
            IR_OPR(loc_idx)		= Aloc_Opr;
            IR_TYPE_IDX(loc_idx)	= CRI_Ptr_8;
            IR_FLD_L(loc_idx)		= AT_Tbl_Idx;
            IR_IDX_L(loc_idx)		= tmp_array_idx;
            IR_LINE_NUM(loc_idx)	= line;
            IR_COL_NUM(loc_idx)		= col;
            IR_LINE_NUM_L(loc_idx)	= line;
            IR_COL_NUM_L(loc_idx)	= col;
            IL_FLD(list_idx)		= IR_Tbl_Idx;
            IL_IDX(list_idx)		= loc_idx;

            gen_sh(Before, Call_Stmt, line, col, FALSE, FALSE, TRUE);
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = call_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
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

            gen_sh(Before, Call_Stmt, line, col, FALSE, FALSE, TRUE);
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
# endif
            
            curr_stmt_sh_idx = new_stmt_idx;
         }
      }

      if (ATD_IM_A_DOPE(tmp_idx)) {
         /* fill in constant fields in temp dope vector */

         OPND_FLD(opnd) = AT_Tbl_Idx;
         OPND_IDX(opnd) = tmp_idx;
         OPND_LINE_NUM(opnd) = line;
         OPND_COL_NUM(opnd)  = col;

         gen_dv_whole_def_init(&opnd, tmp_idx, Before);
      }
      else if (TYP_TYPE(type_idx) == Structure &&
               (ATT_POINTER_CPNT(TYP_IDX(type_idx)) ||
#ifdef KEY /* Bug 6845 */
		ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx)) ||
#endif /* KEY Bug 6845 */
                ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx)))) {

         OPND_FLD(opnd) = AT_Tbl_Idx;
         OPND_IDX(opnd) = tmp_idx;
         OPND_LINE_NUM(opnd) = line;
         OPND_COL_NUM(opnd)  = col;

         if (ATD_ARRAY_IDX(tmp_idx) != NULL_IDX) {
            ok = gen_whole_subscript(&opnd, &exp_desc);
         }

         process_cpnt_inits(&opnd,
                            TYP_IDX(type_idx),
                            gen_dv_whole_def_init,
                            Asg_Opr,
                            Before);
      }

#ifdef KEY /* Bug 5089 */
      if (fcn_to_sub)
#else /* KEY Bug 5089 */
      if (FUNCTION_MUST_BE_SUBROUTINE(attr_idx))
#endif /* KEY Bug 5089 */
      {
         /* insert tmp as first argument to call */

         OPND_FLD(opnd) = AT_Tbl_Idx;
         OPND_IDX(opnd) = tmp_idx;
         OPND_LINE_NUM(opnd) = line;
         OPND_COL_NUM(opnd) = col;

         NTR_IR_TBL(loc_idx);

         IR_OPR(loc_idx) = Aloc_Opr;

         /* JBL - this is for array element change */

         if (TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) == Character &&
             ! ATD_IM_A_DOPE(tmp_idx)) {
            IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
         }
         else {
            IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
         }

         if (ATD_ARRAY_IDX(tmp_idx) != NULL_IDX &&
             ! ATD_IM_A_DOPE(tmp_idx)) {

            ok &= gen_whole_subscript(&opnd, &exp_desc);

# ifndef _FRONTEND_INLINER
            if (OPND_FLD(opnd) == IR_Tbl_Idx &&
                ! ATP_ELEMENTAL(spec_idx)) {

               if (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr) {
                  COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
               }
            }

# endif
            if (! ATP_ELEMENTAL(spec_idx)) {
               unused1 = NULL_IDX;
               unused2 = NULL_IDX;
               make_base_subtree(&opnd, &base_opnd, &unused1, &unused2);
               COPY_OPND(opnd, base_opnd);
            }
         }
         else if (TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) == Character &&
                  ! ATD_IM_A_DOPE(tmp_idx)) {
            ok = gen_whole_substring(&opnd, 
                                     (ATD_ARRAY_IDX(tmp_idx) ? 
                                BD_RANK(ATD_ARRAY_IDX(tmp_idx)) : 0));
         }

         IR_LINE_NUM(loc_idx) = line;
         IR_COL_NUM(loc_idx) = col;
   
         COPY_OPND(IR_OPND_L(loc_idx), opnd);

# ifdef _TRANSFORM_CHAR_SEQUENCE
         if (! ATD_IM_A_DOPE(tmp_idx) &&
             TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) == Structure &&
             ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(tmp_idx)))) {

            transform_char_sequence_ref(&opnd,
                                        ATD_TYPE_IDX(tmp_idx));
            COPY_OPND(IR_OPND_L(loc_idx), opnd);
   
            IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
         }
# endif


         IL_FLD(res_list_idx)               = IR_Tbl_Idx;
         IL_IDX(res_list_idx)               = loc_idx;

         IR_RANK(ir_idx) = 0;

         SH_IR_IDX(new_stmt_idx)     = ir_idx;
         SH_P2_SKIP_ME(new_stmt_idx) = TRUE;

         OPND_FLD((*result))          = AT_Tbl_Idx;
         OPND_IDX((*result))          = tmp_idx;
         OPND_LINE_NUM((*result))     = line;
         OPND_COL_NUM((*result))      = col;

         if (ATD_IM_A_DOPE(tmp_idx)) {
            NTR_IR_TBL(dv_idx);
            IR_OPR(dv_idx) = Dv_Deref_Opr;
            IR_TYPE_IDX(dv_idx) = ATD_TYPE_IDX(tmp_idx);
            IR_LINE_NUM(dv_idx) = line;
            IR_COL_NUM(dv_idx)  = col;

            COPY_OPND(IR_OPND_L(dv_idx), (*result));
            OPND_FLD((*result)) = IR_Tbl_Idx;
            OPND_IDX((*result)) = dv_idx;
         }

         if (ATD_ARRAY_IDX(tmp_idx)) {
            ok = gen_whole_subscript(result, &exp_desc);
         }
         else if (TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) == Character) {
            ok = gen_whole_substring(result, 0);
         }

         if (ATD_IM_A_DOPE(tmp_idx)                        &&
             ATD_ARRAY_IDX(tmp_idx)                        &&
             AT_IS_INTRIN(IR_IDX_L(ir_idx))                &&
             ATP_INTRIN_ENUM(IR_IDX_L(ir_idx)) != Spread_Intrinsic &&
             TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) != Character  &&
             (TYP_TYPE(ATD_TYPE_IDX(tmp_idx)) != Structure ||
              ! ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(tmp_idx))))) {
   
            /* find Whole_Subscript_Opr and mark it for use */
            /* as a fei_as_ref (contiguous array ref)       */
         
            if (OPND_FLD((*result)) == IR_Tbl_Idx &&
                IR_OPR(OPND_IDX((*result))) == Whole_Subscript_Opr) {
   
               IR_CONTIG_ARRAY(OPND_IDX((*result))) = TRUE;
            }
         }
#ifdef KEY /* Bug 11986, 6845 */
	 /* If we are passing a temp by reference because the function
	  * result is allocatable, we need to deallocate it after the
	  * reference to it. */
         if (ATD_ALLOCATABLE(attr_idx)) {
	   int save_curr_stmt_sh_idx = curr_stmt_sh_idx;
	   /* This is the statment containing the reference to the temp */
	   curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
	   help_dealloc(line, col, AT_Tbl_Idx, tmp_idx, FALSE,
	     TRUE, FALSE);
	   curr_stmt_sh_idx = save_curr_stmt_sh_idx;
	 }
#endif /* KEY Bug 11986, 6845 */
      }
      else {

         /* this is for ELEMENTAL calls only */
         /* leave as temp = call             */

         ATD_ELEMENTAL_CALL_TMP(tmp_idx) = TRUE;

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx)           = Asg_Opr;
         IR_TYPE_IDX(asg_idx)      = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(asg_idx)      = IR_LINE_NUM(ir_idx);
         IR_COL_NUM(asg_idx)       = IR_COL_NUM(ir_idx);
         ATD_TMP_IDX(tmp_idx)      = asg_idx;
         ATD_FLD(tmp_idx)          = IR_Tbl_Idx;
         AT_DEFINED(tmp_idx)       = TRUE;

         IR_FLD_R(asg_idx) = IR_Tbl_Idx;
         IR_IDX_R(asg_idx) = ir_idx;

         OPND_FLD(opnd) = AT_Tbl_Idx;
         OPND_IDX(opnd) = tmp_idx;
         OPND_LINE_NUM(opnd) = IR_LINE_NUM(ir_idx);
         OPND_COL_NUM(opnd)  = IR_COL_NUM(ir_idx);

         if (ATD_ARRAY_IDX(tmp_idx)) {
            ok = gen_whole_subscript(&opnd, &exp_desc);
         }

         COPY_OPND(IR_OPND_L(asg_idx), opnd);

         SH_IR_IDX(new_stmt_idx)     = asg_idx;
         SH_P2_SKIP_ME(new_stmt_idx) = TRUE;

         if (where_ir_idx) {
            change_asg_to_where(asg_idx);
         }

         COPY_OPND((*result), opnd);

      }
   }
   else {
      tmp_idx = get_stmt_tmp(ATD_TYPE_IDX(attr_idx),
                             FALSE,
                             0);

      if (tmp_idx) {
         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx)           = Asg_Opr;
         IR_TYPE_IDX(asg_idx)      = ATD_TYPE_IDX(attr_idx);
         IR_FLD_L(asg_idx)         = AT_Tbl_Idx;
         IR_IDX_L(asg_idx)         = tmp_idx;
         IR_LINE_NUM_L(asg_idx)    = IR_LINE_NUM(ir_idx);
         IR_LINE_NUM(asg_idx)      = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_L(asg_idx)     = IR_COL_NUM(ir_idx);
         IR_COL_NUM(asg_idx)       = IR_COL_NUM(ir_idx);
         ATD_TMP_IDX(tmp_idx)      = asg_idx;
         ATD_FLD(tmp_idx)          = IR_Tbl_Idx;
         AT_DEFINED(tmp_idx)       = TRUE;
#ifdef KEY
// Bug 2164
         if ( ATD_F2C_ABI_VAR(attr_idx) ) {
           ATD_F2C_ABI_VAR(tmp_idx)  = TRUE;
           ATD_TYPE_IDX(tmp_idx) = Real_4;
         }
#endif
      }
      else {
         GEN_COMPILER_TMP_ASG(asg_idx,
                              tmp_idx,
                              TRUE,	/* Semantics done */
                              IR_LINE_NUM(ir_idx),
                              IR_COL_NUM(ir_idx),
                              ATD_TYPE_IDX(attr_idx),
                              Priv);
      }

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = ir_idx;

      SH_IR_IDX(new_stmt_idx)     = asg_idx;
      SH_P2_SKIP_ME(new_stmt_idx) = TRUE;

      if (where_ir_idx) {
         change_asg_to_where(asg_idx);
      }

      OPND_FLD((*result))      = AT_Tbl_Idx;
      OPND_IDX((*result))      = tmp_idx;
      OPND_LINE_NUM((*result)) = line;
      OPND_COL_NUM((*result))  = col;

      /* tmp_idx is not character, array or dope vector, so .. */
      /* no extra ir generation is needed.                     */
   }

   orig_sh_idx = save_orig_sh_idx;

   defer_stmt_expansion = save_defer_stmt_expansion;
   stmt_expansion_control_end(result);

EXIT:

   TRACE (Func_Exit, "flatten_function_call", NULL);

   return;

}  /* flatten_function_call */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Evaluate a bounds stmt function and assign it to a tmp.               *|
|*									      *|
|* Input parameters:							      *|
|*	sf_idx - stmt function attr idx.                                      *|
|*	list_idx - start list node for actual args.                           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	tmp attr idx.                                                         *|
|*									      *|
\******************************************************************************/
static int create_tmp_from_stmt_func(int	sf_idx)

{
   int			asg_idx;
   expr_arg_type	exp_desc;
   opnd_type		opnd;
   cif_usage_code_type  save_xref_state;
   int			tmp_idx;


   TRACE (Func_Entry, "create_tmp_from_stmt_func", NULL);

   gen_opnd(&opnd, ATS_SF_IDX(sf_idx), (fld_type) ATS_SF_FLD(sf_idx), 
            stmt_start_line, stmt_start_col);
   copy_subtree(&opnd, &opnd);

   /* Look for struct_opr to see if any components will */
   /* need updating to the correct component attr.      */

   exp_desc.rank	= 0;
   save_xref_state	= xref_state;
   xref_state		= CIF_No_Usage_Rec;
   expr_semantics(&opnd, &exp_desc);
   xref_state		= save_xref_state;

   GEN_COMPILER_TMP_ASG(asg_idx,
                        tmp_idx,
                        TRUE,	/* Semantics done */
                        stmt_start_line,
                        stmt_start_col,
                        exp_desc.type_idx,
                        Priv);

   /* Look for struct_opr to see if any components will */
   /* need updating to the correct component attr.      */

   update_components(&opnd);

   COPY_OPND(IR_OPND_R(asg_idx), opnd);

   gen_sh(Before, Assignment_Stmt, stmt_start_line,
          stmt_start_col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "create_tmp_from_stmt_func", NULL);

   return(tmp_idx);

}  /* create_tmp_from_stmt_func */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Take a rslt_idx describing a character function result and create the *|
|*	tmp needed to find the character length.                              *|
|*									      *|
|* Input parameters:							      *|
|*	rslt_idx - character function result attr idx.                        *|
|*									      *|
|* Output parameters:							      *|
|*	new_type_idx - return type idx.                                       *|
|*      new_bd_idx   - new bounds table idx.                                  *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void make_new_bd_entries(int	 rslt_idx,
				int	*new_type_idx,
				int	*new_bd_idx)

{
   long		attr_link_list[38];	/* (Max dimensions * 5) + 3 */
   int		attr_link_idx		= NULL_IDX;
   int		bd_idx;
   boolean	constant_size_array;
   int		dim;
   boolean	had_variable_len_char = FALSE;
   int		i;
   int		ir_idx;
   int		new_tmp_idx;
   boolean	symbolic_constant_size_array;  /* BRIANJ KAYKAY - not used */
   int		tmp_idx;
   int		type_idx;
   int		ub_idx;


   TRACE (Func_Entry, "make_new_bd_entries", NULL);

   type_idx	= ATD_TYPE_IDX(rslt_idx);
   bd_idx	= ATD_ARRAY_IDX(rslt_idx);

   if (TYP_TYPE(type_idx) == Character  &&
       (TYP_CHAR_CLASS(type_idx) == Var_Len_Char ||
        TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char)) {

      /* TYP_ORIG_LEN_IDX is the bounds tmp used to create the character   */
      /* length.  TYP_IDX is the character length, which is MAX(length,0). */

      if (TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {
         /* TYP_ORIG_LEN_IDX not set for Assumed_Size_Char */
         tmp_idx = TYP_IDX(type_idx);
      }
      else {
         tmp_idx = TYP_ORIG_LEN_IDX(type_idx);
      }

      had_variable_len_char	= TRUE;

      new_tmp_idx			 = expand_user_bound(tmp_idx);
      attr_link_list[attr_link_idx++]    = tmp_idx;
      tmp_idx				 = TYP_IDX(type_idx);
      new_tmp_idx			 = copy_and_replace_tmps(tmp_idx);
      attr_link_list[attr_link_idx++]	 = tmp_idx;
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

      if (ATD_CLASS(new_tmp_idx) == Constant) {
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)		= ATD_CONST_IDX(new_tmp_idx);
      }
      else {
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Var_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= AT_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)		= new_tmp_idx;
         TYP_ORIG_LEN_IDX(TYP_WORK_IDX) = new_tmp_idx;
      }

      TYP_TYPE(TYP_WORK_IDX)		= Character;
      TYP_LINEAR(TYP_WORK_IDX)		= CHARACTER_DEFAULT_TYPE;
      *new_type_idx			= ntr_type_tbl();
   }

   if (bd_idx != NULL_IDX && 
       ! ATD_IM_A_DOPE(rslt_idx) &&
       (BD_ARRAY_SIZE(bd_idx) == Var_Len_Array  ||
        had_variable_len_char)) {

      *new_bd_idx		= reserve_array_ntry(BD_RANK(bd_idx));

      COPY_BD_NTRY(*new_bd_idx, bd_idx);

      BD_LINE_NUM(*new_bd_idx)		= stmt_start_line;
      BD_COLUMN_NUM(*new_bd_idx)	= stmt_start_col;
      BD_RESOLVED(*new_bd_idx)          = TRUE;
      constant_size_array		= TRUE;
      symbolic_constant_size_array	= FALSE;
   
      for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

         if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             !ATD_SYMBOLIC_CONSTANT(BD_LB_IDX(bd_idx,dim))) {

            tmp_idx	= BD_LB_IDX(bd_idx, dim);
            new_tmp_idx	= expand_user_bound(tmp_idx);
            attr_link_list[attr_link_idx++]	= tmp_idx;

            if (ATD_CLASS(new_tmp_idx) == Constant) {
               BD_LB_IDX(*new_bd_idx, dim)	= ATD_CONST_IDX(new_tmp_idx);
               BD_LB_FLD(*new_bd_idx, dim)	= CN_Tbl_Idx;
            }
            else {

# if defined(_TARGET_OS_MAX)

               if (ATD_SYMBOLIC_CONSTANT(new_tmp_idx)) {
                  symbolic_constant_size_array	= TRUE;
               }
               else {
                  constant_size_array		= FALSE;
               }
# else
               constant_size_array		= FALSE;
# endif
               gen_copyin_bounds_stmt(new_tmp_idx);

               BD_LB_IDX(*new_bd_idx, dim)	= new_tmp_idx;
               BD_LB_FLD(*new_bd_idx, dim)	= AT_Tbl_Idx;
            }
         }
   
         if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             !ATD_SYMBOLIC_CONSTANT(BD_UB_IDX(bd_idx,dim))) {
            tmp_idx	= BD_UB_IDX(bd_idx, dim);
            ub_idx	= tmp_idx;
            new_tmp_idx	= expand_user_bound(tmp_idx);

            attr_link_list[attr_link_idx++]	= tmp_idx;

            if (ATD_CLASS(new_tmp_idx) == Constant) {
               BD_UB_IDX(*new_bd_idx, dim)	= ATD_CONST_IDX(new_tmp_idx);
               BD_UB_FLD(*new_bd_idx, dim)	= CN_Tbl_Idx;
            }
            else {

# if defined(_TARGET_OS_MAX)

               if (ATD_SYMBOLIC_CONSTANT(new_tmp_idx)) {
                  symbolic_constant_size_array	= TRUE;
               }
               else {
                  constant_size_array		= FALSE;
               }
# else
               constant_size_array		= FALSE;
# endif
               gen_copyin_bounds_stmt(new_tmp_idx);

               BD_UB_IDX(*new_bd_idx, dim)	= new_tmp_idx;
               BD_UB_FLD(*new_bd_idx, dim)	= AT_Tbl_Idx;
            }
         }
         else {
            ub_idx = NULL_IDX;
         }
   
         if (BD_XT_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             !ATD_SYMBOLIC_CONSTANT(BD_XT_IDX(bd_idx,dim))) {

            /* If the lower bound is a constant one, we take a shortcut  */
            /* in array_dcl_semantics that makes the upper bound and the */
            /* extent be the same tmp.  We have to check to see if the   */
            /* upper bound and the extent are the same tmp here.  If     */
            /* they are not the same temp, we need to copy and replace   */
            /* the tmp hidden in the max(tmp,0) stuff.                   */
            /* The extent should always look like t1 = max(t2,0).        */
            /* To check whether we need to handle the second tmp or not  */
            /* compare t2 to the upper bound tmp.  If it is the same, we */

            /*   t$1 = i (lb);  t$2 = j  (ub);                           */     
            /*   t$3 = t$2 - t$1 + 1             Extra tmp               */
            /*   t$4 = max(t$3,0)                extent tmp              */

            tmp_idx = BD_XT_IDX(bd_idx, dim);

            if (ATD_FLD(tmp_idx) == IR_Tbl_Idx) {
               ir_idx = IR_IDX_R(ATD_TMP_IDX(tmp_idx));

# ifdef _DEBUG
               if (IR_OPR(ir_idx) != Max_Opr ||
                   IL_FLD(IR_IDX_L(ir_idx)) != AT_Tbl_Idx) {
                  PRINTMSG(BD_LINE_NUM(bd_idx), 775, Internal, 
                           BD_COLUMN_NUM(bd_idx), 
                           AT_OBJ_NAME_PTR(tmp_idx), 
                           tmp_idx);
               }
# endif

               tmp_idx			       = IL_IDX(IR_IDX_L(ir_idx));

               if (tmp_idx != ub_idx) {
                  new_tmp_idx = copy_and_replace_tmps(tmp_idx);
                  attr_link_list[attr_link_idx++] = tmp_idx;
               }
            }

            tmp_idx			    = BD_XT_IDX(bd_idx, dim);
            new_tmp_idx			    = copy_and_replace_tmps(tmp_idx);
            attr_link_list[attr_link_idx++] = tmp_idx;

            if (ATD_CLASS(new_tmp_idx) == Constant) {
               BD_XT_IDX(*new_bd_idx, dim) = ATD_CONST_IDX(new_tmp_idx);
               BD_XT_FLD(*new_bd_idx, dim) = CN_Tbl_Idx;
            }
            else {

# if defined(_TARGET_OS_MAX)

               if (ATD_SYMBOLIC_CONSTANT(new_tmp_idx)) {
                  symbolic_constant_size_array	= TRUE;
               }
               else {
                  constant_size_array		= FALSE;
               }
# else
               constant_size_array		= FALSE;
# endif
               gen_copyin_bounds_stmt(new_tmp_idx);

               BD_XT_IDX(*new_bd_idx, dim) = new_tmp_idx;
               BD_XT_FLD(*new_bd_idx, dim) = AT_Tbl_Idx;
            }
         }
   
         if (BD_SM_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             !ATD_SYMBOLIC_CONSTANT(BD_SM_IDX(bd_idx, dim))) {
            tmp_idx			    = BD_SM_IDX(bd_idx, dim);
            new_tmp_idx			    = copy_and_replace_tmps(tmp_idx);
            attr_link_list[attr_link_idx++] = tmp_idx;

            if (ATD_CLASS(new_tmp_idx) == Constant) {
               BD_SM_IDX(*new_bd_idx, dim) = ATD_CONST_IDX(new_tmp_idx);
               BD_SM_FLD(*new_bd_idx, dim) = CN_Tbl_Idx;
            }
            else {

# if defined(_TARGET_OS_MAX)

               if (ATD_SYMBOLIC_CONSTANT(new_tmp_idx)) {
                  symbolic_constant_size_array	= TRUE;
               }
               else {
                  constant_size_array		= FALSE;
               }
# else
               constant_size_array		= FALSE;
# endif
               BD_SM_IDX(*new_bd_idx, dim) = new_tmp_idx;
               BD_SM_FLD(*new_bd_idx, dim) = AT_Tbl_Idx;
            }
         }
      }
   
      if (BD_LEN_FLD(bd_idx) == AT_Tbl_Idx &&
          !ATD_SYMBOLIC_CONSTANT(BD_LEN_IDX(bd_idx))) {
         tmp_idx			= BD_LEN_IDX(bd_idx);
         new_tmp_idx			= copy_and_replace_tmps(tmp_idx);
         attr_link_list[attr_link_idx++]= tmp_idx;

         if (ATD_CLASS(new_tmp_idx) == Constant) {
            BD_LEN_IDX(*new_bd_idx) = ATD_CONST_IDX(new_tmp_idx);
            BD_LEN_FLD(*new_bd_idx) = CN_Tbl_Idx;
         }
         else {

# if defined(_TARGET_OS_MAX)

            if (ATD_SYMBOLIC_CONSTANT(new_tmp_idx)) {
               symbolic_constant_size_array	= TRUE;
            }
            else {
               constant_size_array		= FALSE;
            }
# else
            constant_size_array		= FALSE;
# endif
            BD_LEN_IDX(*new_bd_idx) = new_tmp_idx;
            BD_LEN_FLD(*new_bd_idx) = AT_Tbl_Idx;
         }
      }

      if (constant_size_array) {
         BD_ARRAY_SIZE(*new_bd_idx)  = Constant_Size;
      }

      BD_FLOW_DEPENDENT(*new_bd_idx) = TRUE;
      *new_bd_idx = ntr_array_in_bd_tbl(*new_bd_idx);
   }
   
   for (i = 0; i < attr_link_idx; i++) {
      AT_ATTR_LINK(attr_link_list[i]) = NULL_IDX;
   }
      
   TRACE (Func_Exit, "make_new_bd_entries", NULL);

   return;

}  /* make_new_bd_entries */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine copies the IR for the bound tmp being passed in.  It     *|
|*	then calls expr_semantics to try to fold the bounds IR.  expr_semantic*|
|*	will replace the original (placeholder tmps) in the IR with the new   *|
|*	tmps created by this routines callee.  (When bounds are generated for *|
|*	functions, they are just placeholder tmps.  Each place that a call is *|
|*	made, the placeholder tmps need to be replaced by tmps generated for  *|
|*	the current scope.  The tmps get replaced via the attr_link mechanism.*|
|*	New tmps are generated and the old tmps are attr linked to the new    *|
|*	ones, then expr_semantics replaces the old tmps with the new ones in  *|
|*	the IR stream.  After expr_semantics is called, the old attr_link is  *|
|*	broken.)  If the IR folds, a new constant attr is created and         *|
|*	returned, otherwise a tmp is generated to hold the IR and the IR is   *|
|*	put into the current IR stream.  The tmp is returned.                 *|
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
static int copy_and_replace_tmps(int	attr_idx)

{
   expr_arg_type	exp_desc;
   int			ir_idx;
   opnd_type		opnd;
   cif_usage_code_type  save_xref_state;
   int			tmp_attr_idx;


   TRACE (Func_Entry, "copy_and_replace_tmps", NULL);

   COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(attr_idx)));
   copy_subtree(&opnd, &opnd);

   exp_desc.rank	= 0;
   save_xref_state	= xref_state;
   xref_state		= CIF_No_Usage_Rec;

   expr_semantics(&opnd, &exp_desc);

   xref_state		= save_xref_state;

   if (OPND_FLD(opnd) == CN_Tbl_Idx) {  /* Folded to a constant */
      tmp_attr_idx			= gen_compiler_tmp(stmt_start_line, 
                                                           stmt_start_col,
                                                           Priv, TRUE);
      ATD_CLASS(tmp_attr_idx)		= Constant;
      AT_TYPED(tmp_attr_idx)		= TRUE;
      ATD_TYPE_IDX(tmp_attr_idx)	= exp_desc.type_idx;
      AT_DEFINED(tmp_attr_idx)		= FALSE;
      AT_REFERENCED(tmp_attr_idx)	= Not_Referenced;
      AT_SEMANTICS_DONE(tmp_attr_idx)	= TRUE;
      ATD_CONST_IDX(tmp_attr_idx)	= OPND_IDX(opnd);
      ATD_FLD(tmp_attr_idx)		= CN_Tbl_Idx;
   }
   else {
      GEN_COMPILER_TMP_ASG(ir_idx,
                           tmp_attr_idx,
                           TRUE,	/* Semantics done */
                           stmt_start_line,
                           stmt_start_col,
                           exp_desc.type_idx,
                           Priv);

      COPY_OPND(IR_OPND_R(ATD_TMP_IDX(tmp_attr_idx)), opnd);

      gen_sh(Before, Assignment_Stmt, stmt_start_line,
             stmt_start_col, FALSE, FALSE, TRUE);

      AT_DEFINED(tmp_attr_idx)				= TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))		= ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))	= TRUE;
   }

   AT_ATTR_LINK(attr_idx)	= tmp_attr_idx;

   TRACE (Func_Exit, "copy_and_replace_tmps", NULL);

   return(tmp_attr_idx);

}  /* copy_and_replace_tmps */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Takes as input, a tmp for a user specified bound.  (Char length,      *|
|*	lower bound or upper bound).  It expands the dummy args with the      *|
|*	actual args specified in list_idx.  If it folds to a constant, it     *|
|*	changes the tmp into a constant tmp.                                  *|
|*									      *|
|* Input parameters:							      *|
|*	tmp_idx  - A tmp for the user specified bound.                        *|
|*	list_idx - A list of actual args to replace the dummy args.           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The newly expanded tmp.  It may be a compiler tmp or a compiler       *|
|*	constant.                                                             *|
|*									      *|
\******************************************************************************/
static int expand_user_bound(int	tmp_idx)

{
   int		const_idx;
   int		ir_idx;
   int		new_tmp_idx;
   opnd_type	opnd;
   int		sf_attr_idx;
   int		sh_idx;


   TRACE (Func_Exit, "expand_user_bound", NULL);

   /* Create a stmt function for the length tmp, so it     */
   /* can be expanded with the actual arguments.           */

   NTR_ATTR_TBL(sf_attr_idx);
   AT_OBJ_CLASS(sf_attr_idx)	= Stmt_Func;
   AT_COMPILER_GEND(sf_attr_idx)= TRUE;
   ATD_TYPE_IDX(sf_attr_idx)	= CG_INTEGER_DEFAULT_TYPE;
   ATS_SF_FLD(sf_attr_idx)	= IR_FLD_R(ATD_TMP_IDX(tmp_idx));
   ATS_SF_IDX(sf_attr_idx)	= IR_IDX_R(ATD_TMP_IDX(tmp_idx));

   new_tmp_idx			= create_tmp_from_stmt_func(sf_attr_idx);
   AT_ATTR_LINK(tmp_idx)	= new_tmp_idx;
   OPND_FLD(opnd)		= IR_FLD_R(ATD_TMP_IDX(new_tmp_idx));
   OPND_IDX(opnd)		= IR_IDX_R(ATD_TMP_IDX(new_tmp_idx));

   if (OPND_FLD(opnd) == CN_Tbl_Idx) {
      const_idx				= OPND_IDX(opnd);
      CLEAR_VARIANT_ATTR_INFO(new_tmp_idx, Data_Obj);
      ATD_CLASS(new_tmp_idx)		= Constant;
      AT_TYPED(new_tmp_idx)		= TRUE;
      ATD_TYPE_IDX(new_tmp_idx)		= CN_TYPE_IDX(const_idx);
      AT_DEFINED(new_tmp_idx)		= FALSE;
      AT_REFERENCED(new_tmp_idx)	= Not_Referenced;
      ATD_CONST_IDX(new_tmp_idx)	= const_idx;
      ATD_FLD(new_tmp_idx)		= CN_Tbl_Idx;

      /* Free the SH created for this IR. */

      sh_idx				= SH_PREV_IDX(curr_stmt_sh_idx);
      remove_sh(sh_idx);
      FREE_IR_NODE(SH_IR_IDX(sh_idx));
      FREE_SH_NODE(sh_idx);
   }

# if defined(_TARGET_OS_MAX)

   else if (expr_is_symbolic_constant(&opnd)) {
      ATD_SYMBOLIC_CONSTANT(new_tmp_idx)	= TRUE;
      AT_DEFINED(new_tmp_idx)			= FALSE;
      AT_REFERENCED(new_tmp_idx)		= Not_Referenced;
      ATD_FLD(new_tmp_idx)			= OPND_FLD(opnd);
      ATD_TMP_IDX(new_tmp_idx)			= OPND_IDX(opnd);

      /* Free the SH created for this IR. */

      sh_idx				= SH_PREV_IDX(curr_stmt_sh_idx);
      remove_sh(sh_idx);
      FREE_IR_NODE(SH_IR_IDX(sh_idx));
      FREE_SH_NODE(sh_idx);
   }

# endif

   else {
      AT_DEFINED(new_tmp_idx)		= TRUE;
      AT_REFERENCED(new_tmp_idx)	= Referenced;

      if (ATD_FLD(new_tmp_idx) == AT_Tbl_Idx) {
         find_attrs_used_in_bound(ATD_TMP_IDX(new_tmp_idx));
      }
      else if (ATD_FLD(new_tmp_idx) == IR_Tbl_Idx) {
         ir_idx = ATD_TMP_IDX(new_tmp_idx);

         switch (IR_FLD_R(ir_idx)) {
            case AT_Tbl_Idx:
               find_attrs_used_in_bound(IR_IDX_R(ir_idx));
               break;

            case IR_Tbl_Idx:
               find_attrs_in_ir(IR_IDX_R(ir_idx));
               break;

            case IL_Tbl_Idx:
               find_attrs_in_il(IR_IDX_R(ir_idx));
               break;
         }
      }
   }

   TRACE (Func_Exit, "expand_user_bound", NULL);

   return(new_tmp_idx);

}  /* expand_user_bound */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void find_attrs_used_in_bound(int	attr_idx)

{
   int		asg_idx;
   int		ir_idx;
   int		list_idx;
   int		name_idx;
   int		old_attr_idx;


   TRACE (Func_Entry, "find_attrs_used_in_bound", NULL);

   if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

      if (ATD_CLASS(attr_idx) == Struct_Component) {
         return;
      }

      if (ATD_CLASS(attr_idx) == Function_Result) {
         attr_idx = ATD_FUNC_IDX(attr_idx);
      }
   }

   old_attr_idx = srch_sym_tbl(AT_OBJ_NAME_PTR(attr_idx),
                               AT_NAME_LEN(attr_idx),
                               &name_idx);

   /* If the object is in this scope - just return */

   if (old_attr_idx == attr_idx) {
      return;
   }
   else if (old_attr_idx != NULL_IDX &&
            AT_OBJ_CLASS(old_attr_idx) == Interface &&
            ATI_PROC_IDX(old_attr_idx) != NULL_IDX &&
            attr_idx == ATI_PROC_IDX(old_attr_idx)) {
      return;
   }

   if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
       ATD_CLASS(attr_idx) == Compiler_Tmp) {

      if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
         find_attrs_used_in_bound(ATD_TMP_IDX(attr_idx));
      }
      else if (ATD_FLD(attr_idx) == IR_Tbl_Idx) {
         ir_idx = ATD_TMP_IDX(attr_idx);

         switch (IR_FLD_R(ir_idx)) {
         case AT_Tbl_Idx:
            find_attrs_used_in_bound(IR_IDX_R(ir_idx));
            break;

         case IR_Tbl_Idx:
            find_attrs_in_ir(IR_IDX_R(ir_idx));
            break;

         case IL_Tbl_Idx:
            find_attrs_in_il(IR_IDX_R(ir_idx));
            break;
         }
      }
      else if (ATD_FLD(attr_idx) == CN_Tbl_Idx) {

         if (SB_ORIG_SCP_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {

            /* Tmp for an aggregate constant that is defined in an     */
            /* interface block.  This tmp is in the static block of    */
            /* the interface which doesn't exist.  It needs to be in   */
            /* a static block in the caller.  It also needs to get its */
            /* initialization data generated in the callers scope.     */
            /* The interface's static block is added to the caller and */
            /* initialization code is generated.  host_assocociated_   */
            /* attr_semantics handles the generation of the storage    */
            /* block.  Following is generation of the IR.              */

            /* create data init stmt */

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx)		= Init_Opr;
            IR_TYPE_IDX(asg_idx)        = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(asg_idx)	= AT_DEF_LINE(attr_idx);
            IR_COL_NUM(asg_idx)		= AT_DEF_COLUMN(attr_idx);
            IR_LINE_NUM_L(asg_idx)	= AT_DEF_LINE(attr_idx);
            IR_COL_NUM_L(asg_idx)	= AT_DEF_COLUMN(attr_idx);
            IR_FLD_L(asg_idx)		= AT_Tbl_Idx;
            IR_IDX_L(asg_idx)		= attr_idx;
            AT_DEFINED(attr_idx)	= TRUE;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(asg_idx)		= IL_Tbl_Idx;
            IR_IDX_R(asg_idx)		= list_idx;
            IR_LIST_CNT_R(asg_idx)	= 3;

            IL_FLD(list_idx)		= CN_Tbl_Idx;
            IL_IDX(list_idx)		= ATD_TMP_IDX(attr_idx);
            IL_LINE_NUM(list_idx)       = AT_DEF_LINE(attr_idx);
            IL_COL_NUM(list_idx)        = AT_DEF_COLUMN(attr_idx);

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx))	= list_idx;

            list_idx			= IL_NEXT_LIST_IDX(list_idx);
            IL_FLD(list_idx)		= CN_Tbl_Idx;
            IL_IDX(list_idx)		= CN_INTEGER_ONE_IDX;
            IL_LINE_NUM(list_idx)       = AT_DEF_LINE(attr_idx);
            IL_COL_NUM(list_idx)        = AT_DEF_COLUMN(attr_idx);

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx))	= list_idx;

            list_idx		= IL_NEXT_LIST_IDX(list_idx);
            IL_FLD(list_idx)	= CN_Tbl_Idx;
            IL_IDX(list_idx)	= CN_INTEGER_ZERO_IDX;
            IL_LINE_NUM(list_idx)       = AT_DEF_LINE(attr_idx);
            IL_COL_NUM(list_idx)        = AT_DEF_COLUMN(attr_idx);

            gen_sh(Before, 
                   Assignment_Stmt,
                   AT_DEF_LINE(attr_idx),
                   AT_DEF_COLUMN(attr_idx),
                   FALSE, 
                   FALSE,
                   TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))		= asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))	= TRUE;
         }
      }
   }

   AT_REFERENCED(attr_idx)	= Referenced;

   host_associated_attr_semantics(attr_idx, TRUE);

   TRACE (Func_Exit, "find_attrs_used_in_bound", NULL);

   return;

}  /* find_attrs_used_in_bound */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*       ir_idx => ir to check                                                *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*       NONE                                                                 *|
|*                                                                            *|
|* Returns:                                                                   *|
|*       NONE                                                                 *|
|*                                                                            *|
\******************************************************************************/
static  void    find_attrs_in_ir(int   ir_idx)
{

   TRACE (Func_Entry, "find_attrs_in_ir", NULL);

   switch (IR_FLD_L(ir_idx)) {
      case AT_Tbl_Idx:
         find_attrs_used_in_bound(IR_IDX_L(ir_idx));
         break;

      case IR_Tbl_Idx:
         find_attrs_in_ir(IR_IDX_L(ir_idx));
         break;

      case IL_Tbl_Idx:
         find_attrs_in_il(IR_IDX_L(ir_idx));
         break;
   }

   switch (IR_FLD_R(ir_idx)) {
      case AT_Tbl_Idx:
         find_attrs_used_in_bound(IR_IDX_R(ir_idx));
         break;

      case IR_Tbl_Idx:
         find_attrs_in_ir(IR_IDX_R(ir_idx));
         break;

      case IL_Tbl_Idx:
         find_attrs_in_il(IR_IDX_R(ir_idx));
         break;
   }

   TRACE (Func_Exit, "find_attrs_in_ir", NULL);

   return;

}  /* find_attrs_in_ir */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*       list_idx => il to check                                              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*       NONE                                                                 *|
|*                                                                            *|
|* Returns:                                                                   *|
|*       NONE                                                                 *|
|*                                                                            *|
\******************************************************************************/
static  void    find_attrs_in_il(int   list_idx)
{

   TRACE (Func_Entry, "find_attrs_in_il", NULL);

   while (list_idx != NULL_IDX) {

      switch (IL_FLD(list_idx)) {

         case AT_Tbl_Idx:
            find_attrs_used_in_bound(IL_IDX(list_idx));
            break;

         case IR_Tbl_Idx:
            find_attrs_in_ir(IL_IDX(list_idx));
            break;

         case IL_Tbl_Idx:
            find_attrs_in_il(IL_IDX(list_idx));
            break;

      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "find_attrs_in_il", NULL);

   return;

}  /* find_attrs_in_il */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine will return a scalar or dope vector tmp that has only    *|
|*      a statements duration. It will try to reuse tmps if possible by       *|
|*      checking the stmt_tmp_tbl. If the global variables stmt_start_line    *|
|*      and stmt_start_col are different from the line and col in the IL      *|
|*      entry in the tmp list, then we can use that tmp.                      *|
|*									      *|
|* Input parameters:							      *|
|*	type_idx - type tbl idx needed.                                       *|
|*	dope_vector - TRUE if this is a dope vector.                          *|
|*	rank - rank of tmp.                                                   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	tmp attr idx.                                                         *|
|*									      *|
\******************************************************************************/

int	get_stmt_tmp(int	type_idx,
		     boolean	dope_vector,
		     int	rank)

{

   linear_type_type	linear_type;
   int			list_idx;
   int			tmp_idx = NULL_IDX;



   TRACE (Func_Entry, "get_stmt_tmp", NULL);

   if (comp_phase != Pass2_Semantics) {
      goto EXIT;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (dump_flags.mp) {
      goto EXIT;
   }
# endif

   if (cdir_switches.parallel_region) {
      goto EXIT;
   }

   linear_type = TYP_LINEAR(type_idx);

   if (stmt_tmp_tbl[linear_type].scalar_tmps_head < 0) {
      goto EXIT;
   }

   if (! dope_vector && rank != 0) {
      goto EXIT;
   }

   if (dope_vector) {

      if (stmt_tmp_tbl[linear_type].dope_vector_tmps_head[rank] == NULL_IDX) {

         goto EXIT;
      }
      else {
 
         list_idx = stmt_tmp_tbl[linear_type].dope_vector_tmps_head[rank];

         if (IL_LINE_NUM(list_idx) == stmt_start_line &&
             IL_COL_NUM(list_idx) == stmt_start_col)  {

            /* this tmp in use, and all others in list */
            /* create new tmp, put it on list */

            goto EXIT;

         }
         else {
            tmp_idx = IL_IDX(list_idx);
            IL_LINE_NUM(list_idx) = stmt_start_line;
            IL_COL_NUM(list_idx) = stmt_start_col;

            /* move tmp to end of list */

            if (stmt_tmp_tbl[linear_type].dope_vector_tmps_head[rank] ==
                stmt_tmp_tbl[linear_type].dope_vector_tmps_tail[rank]) {
               /* just one tmp on list */
               /* intentionally blank */
            }
            else {
               stmt_tmp_tbl[linear_type].dope_vector_tmps_head[rank] =
                                             IL_NEXT_LIST_IDX(list_idx);
               IL_NEXT_LIST_IDX(stmt_tmp_tbl[linear_type].
                                 dope_vector_tmps_tail[rank]) = list_idx;
               stmt_tmp_tbl[linear_type].dope_vector_tmps_tail[rank] = list_idx;
               IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
            }
         }
      }
   }
   else {

      if (stmt_tmp_tbl[linear_type].scalar_tmps_head == NULL_IDX) {

         /* create new tmp, put it on list */

         tmp_idx			= gen_compiler_tmp(stmt_start_line,
                                                           stmt_start_col,
                                                           Priv, TRUE);
         ATD_TYPE_IDX(tmp_idx)		= type_idx;
         ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_STACK_IDX(curr_scp_idx);
         AT_SEMANTICS_DONE(tmp_idx)	= TRUE;

         NTR_IR_LIST_TBL(list_idx);
         IL_LINE_NUM(list_idx) = stmt_start_line;
         IL_COL_NUM(list_idx)  = stmt_start_col;
         IL_IDX(list_idx)      = tmp_idx;
         IL_FLD(list_idx)      = AT_Tbl_Idx;
         stmt_tmp_tbl[linear_type].scalar_tmps_head = list_idx;
         stmt_tmp_tbl[linear_type].scalar_tmps_tail = list_idx;
      }
      else {
 
         list_idx = stmt_tmp_tbl[linear_type].scalar_tmps_head;

         if (IL_LINE_NUM(list_idx) == stmt_start_line &&
             IL_COL_NUM(list_idx) == stmt_start_col)  {

            /* this tmp in use, and all others in list */
            /* create new tmp, put it on list */

            tmp_idx			= gen_compiler_tmp(stmt_start_line, 
                                                           stmt_start_col,
                                                           Priv, TRUE);
            ATD_TYPE_IDX(tmp_idx)	= type_idx;
            ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_STACK_IDX(curr_scp_idx);
            AT_SEMANTICS_DONE(tmp_idx)	= TRUE;

            NTR_IR_LIST_TBL(list_idx);
            IL_LINE_NUM(list_idx) = stmt_start_line;
            IL_COL_NUM(list_idx)  = stmt_start_col;
            IL_IDX(list_idx)      = tmp_idx;
            IL_FLD(list_idx)      = AT_Tbl_Idx;

            IL_NEXT_LIST_IDX(stmt_tmp_tbl[linear_type].scalar_tmps_tail) =
                                                  list_idx;
            stmt_tmp_tbl[linear_type].scalar_tmps_tail = list_idx;

         }
         else {
            tmp_idx = IL_IDX(list_idx);
            IL_LINE_NUM(list_idx) = stmt_start_line;
            IL_COL_NUM(list_idx) = stmt_start_col;

            /* move tmp to end of list */

            if (stmt_tmp_tbl[linear_type].scalar_tmps_head ==
                stmt_tmp_tbl[linear_type].scalar_tmps_tail) {
               /* just one tmp on list */
               /* intentionally blank */
            }
            else {
               stmt_tmp_tbl[linear_type].scalar_tmps_head = 
                                             IL_NEXT_LIST_IDX(list_idx);
               IL_NEXT_LIST_IDX(stmt_tmp_tbl[linear_type].scalar_tmps_tail) =
                                             list_idx;
               stmt_tmp_tbl[linear_type].scalar_tmps_tail = list_idx;
               IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
            }
         }
      }
   }

EXIT:

   if (tmp_idx) {
      ATD_ASG_TMP(tmp_idx) = FALSE;
      ATD_ELEMENTAL_CALL_TMP(tmp_idx) = FALSE;
   }

   TRACE (Func_Exit, "get_stmt_tmp", NULL);

   return(tmp_idx);

}  /* get_stmt_tmp */

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

static void process_variable_size_func(int      rslt_idx,
                                       int      arg_list_idx,
                                       int      sf_darg_idx,
                                       int      sf_num_dargs,
                                       int     *new_type_idx,
                                       int     *new_bd_idx)

{
   int			a_list_idx;
   int			association;
   int			bd_idx;
   int			column;
   int			dummy_idx;
   int			d_bd_idx;
   int			d_type_idx;
   expr_arg_type	exp_desc;
   int			i;
   int			ir_idx;
   int			k;
   int			line;
   int			list_idx;
   opnd_type		list_opnd;
   opnd_type    	opnd;
   boolean		save_variable_size_func_expr;
   int			sn_idx;
   int			tmp_dv_idx;
   int			type_idx;


   TRACE (Func_Entry, "process_variable_size_func", NULL);

   sn_idx       = sf_darg_idx;

   a_list_idx = arg_list_idx;

   for (i = sf_num_dargs; i > 0; i--) {
      if (AT_OBJ_CLASS(SN_ATTR_IDX(sn_idx)) == Data_Obj) {
         ATD_SF_DARG(SN_ATTR_IDX(sn_idx)) = TRUE;
      }
      else if (AT_OBJ_CLASS(SN_ATTR_IDX(sn_idx)) == Pgm_Unit &&
               ATP_PROC(SN_ATTR_IDX(sn_idx)) == Dummy_Proc) {

         ATP_DUMMY_PROC_LINK(SN_ATTR_IDX(sn_idx)) = IL_IDX(a_list_idx);
      }
      
      sn_idx++;
      a_list_idx = IL_NEXT_LIST_IDX(a_list_idx);
   }

   type_idx     = ATD_TYPE_IDX(rslt_idx);
   bd_idx       = ATD_ARRAY_IDX(rslt_idx);

   list_opnd = null_opnd;

   check_bd_typ_for_dargs(bd_idx,
                          type_idx,
                          &list_opnd);

   list_idx = OPND_IDX(list_opnd);

   /* go through list of needed dummy args and hook up the actual args */

   for (i = 0; i < OPND_LIST_CNT(list_opnd); i++) {
      sn_idx = sf_darg_idx;
      a_list_idx = arg_list_idx;
      dummy_idx = IL_IDX(list_idx);

      
      /* find the position of the dummy/actual in question */

      for (k = 0; k < sf_num_dargs; k++) {
         if (SN_ATTR_IDX(sn_idx) == dummy_idx) {
            goto FOUND;
         }
         sn_idx++;
         a_list_idx = IL_NEXT_LIST_IDX(a_list_idx);
      }

# ifdef _DEBUG
      print_at(dummy_idx);
# endif
      PRINTMSG(stmt_start_line, 1020, Internal, stmt_start_col);

FOUND:

      if (AT_OBJ_CLASS(dummy_idx) == Data_Obj) {

         ATD_SF_LINK(dummy_idx) = IL_ARG_DESC_IDX(a_list_idx);

         association = arg_info_list[IL_ARG_DESC_IDX(a_list_idx)].association;

         COPY_OPND(opnd, IL_OPND(a_list_idx));

         switch (association) {
            case PASS_ADDRESS:
            case PASS_SECTION_ADDRESS :
            case PASS_DV :
            case PASS_DV_COPY :
            case COPY_IN :
            case COPY_IN_COPY_OUT :
            case MAKE_DV :
            case COPY_IN_MAKE_DV :
#ifdef KEY /* Bug 6939 */
            case COPY_INOUT_MAKE_DV :
#endif /* KEY Bug 6939 */
               if (OPND_FLD(opnd) == IR_Tbl_Idx &&
                   (IR_OPR(OPND_IDX(opnd)) == Loc_Opr ||
                    IR_OPR(OPND_IDX(opnd)) == Aloc_Opr ||
                    IR_OPR(OPND_IDX(opnd)) == Const_Tmp_Loc_Opr)) {

                  COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
               }
               break;

            case CHECK_CONTIG_FLAG :
               /* intentionally blank */
               break;

            case PASS_ADDRESS_FROM_DV:
               if (arg_info_list[IL_ARG_DESC_IDX(a_list_idx)].ed.rank == 0) {
                  COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
               }
               break;
         }

         if (ATD_ARRAY_IDX(dummy_idx) != NULL_IDX &&
             ! ATD_IM_A_DOPE(dummy_idx))          {

            /* Need to set the actual arg up as a dope vector with */
            /* the base address of the actual arg and the dimension*/
            /* information of the dummy arg. This is because of    */
            /* array reshaping of sequence association.            */

            d_bd_idx = ATD_ARRAY_IDX(dummy_idx);
            d_type_idx = ATD_TYPE_IDX(dummy_idx);

            if (TYP_TYPE(d_type_idx) == Character &&
                 TYP_CHAR_CLASS(d_type_idx) == Assumed_Size_Char) {

               /* temporarily hook up the original actual arg */

               if (association == CHECK_CONTIG_FLAG) {
                  ATD_FLD(dummy_idx)        = ATD_FLD(OPND_IDX(opnd));
                  ATD_SF_ARG_IDX(dummy_idx) = ATD_TMP_IDX(OPND_IDX(opnd));
               }
               else {
                  ATD_FLD(dummy_idx)        = OPND_FLD(opnd);
                  ATD_SF_ARG_IDX(dummy_idx) = OPND_IDX(opnd);
               }
            }

            if ((TYP_TYPE(d_type_idx) == Character &&
                 (TYP_CHAR_CLASS(d_type_idx) == Var_Len_Char ||
                  TYP_CHAR_CLASS(d_type_idx) == Assumed_Size_Char)) ||
                BD_ARRAY_SIZE(d_bd_idx) == Var_Len_Array) {
   
               make_new_bd_entries(dummy_idx,
                                   &d_type_idx,
                                   &d_bd_idx);
            }

            /* put in the new rank and shape in the expression descriptor */
            exp_desc = arg_info_list[IL_ARG_DESC_IDX(a_list_idx)].ed;

            exp_desc.rank = BD_RANK(d_bd_idx);
            for (k = 0; k < BD_RANK(d_bd_idx); k++) {
               exp_desc.shape[k].fld = BD_XT_FLD(d_bd_idx, k + 1);
               exp_desc.shape[k].idx = BD_XT_IDX(d_bd_idx, k + 1);
            }
            exp_desc.dope_vector = TRUE;

            arg_info_list_base      = arg_info_list_top;
            arg_info_list_top       = arg_info_list_base + 1;

            if (arg_info_list_top >= arg_info_list_size) {
               enlarge_info_list_table();
            }

            ATD_SF_LINK(dummy_idx)              = arg_info_list_top;
            arg_info_list[arg_info_list_top]    = init_arg_info;
            arg_info_list[arg_info_list_top].ed = exp_desc;
   
            tmp_dv_idx = gen_sf_dv_whole_def(&opnd, d_type_idx, d_bd_idx);

            find_opnd_line_and_column(&opnd, &line, &column);

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)		= Dv_Deref_Opr;
            IR_TYPE_IDX(ir_idx)	= type_idx;
            IR_LINE_NUM(ir_idx)	= line;
            IR_COL_NUM(ir_idx)	= column;
            IR_FLD_L(ir_idx)	= AT_Tbl_Idx;
            IR_IDX_L(ir_idx)	= tmp_dv_idx;
            IR_LINE_NUM_L(ir_idx)	= line;
            IR_COL_NUM_L(ir_idx)	= column;

            OPND_FLD(opnd)		= IR_Tbl_Idx;
            OPND_IDX(opnd)		= ir_idx;
         }

         ATD_FLD(dummy_idx)        = OPND_FLD(opnd);
         ATD_SF_ARG_IDX(dummy_idx) = OPND_IDX(opnd);

         if (ATD_ARRAY_IDX(dummy_idx) &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(dummy_idx)) == Assumed_Shape) {

# ifdef _DEBUG
            if (OPND_FLD(opnd) != AT_Tbl_Idx ||
                AT_OBJ_CLASS(OPND_IDX(opnd)) != Data_Obj ||
                ATD_CLASS(OPND_IDX(opnd)) != Compiler_Tmp) {
               PRINTMSG(stmt_start_line, 626, Internal,
                        stmt_start_col,
                        "Compiler_Tmp", "process_variable_size_func");
            }
# endif

            ATD_RESHAPE_ARRAY_IDX(OPND_IDX(opnd)) = 
                                   ATD_ARRAY_IDX(OPND_IDX(opnd));
            ATD_ARRAY_IDX(OPND_IDX(opnd)) = ATD_ARRAY_IDX(dummy_idx);
         }
      }


      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }


   save_variable_size_func_expr = variable_size_func_expr;
   variable_size_func_expr = TRUE;

   /* now call make_new_bd_entries for the function result */

   make_new_bd_entries(rslt_idx,
                       new_type_idx,
                       new_bd_idx);
   
   variable_size_func_expr = save_variable_size_func_expr;

   sn_idx = sf_darg_idx;

   for (i = sf_num_dargs; i > 0; i--) {
      if (AT_OBJ_CLASS(SN_ATTR_IDX(sn_idx)) == Data_Obj) {
         ATD_SF_DARG(SN_ATTR_IDX(sn_idx)) = FALSE;

         if (ATD_ARRAY_IDX(SN_ATTR_IDX(sn_idx)) &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(SN_ATTR_IDX(sn_idx))) == 
                                                       Assumed_Shape &&
             ATD_FLD(SN_ATTR_IDX(sn_idx)) == AT_Tbl_Idx &&
             ATD_SF_ARG_IDX(SN_ATTR_IDX(sn_idx)) != NULL_IDX) {
 
            ATD_ARRAY_IDX(ATD_SF_ARG_IDX(SN_ATTR_IDX(sn_idx))) = 
                  ATD_RESHAPE_ARRAY_IDX(ATD_SF_ARG_IDX(SN_ATTR_IDX(sn_idx)));
            ATD_RESHAPE_ARRAY_IDX(ATD_SF_ARG_IDX(SN_ATTR_IDX(sn_idx))) = 
                                                             NULL_IDX;
         }
      }
      else if (AT_OBJ_CLASS(SN_ATTR_IDX(sn_idx)) == Pgm_Unit &&
               ATP_PROC(SN_ATTR_IDX(sn_idx)) == Dummy_Proc) {
         ATP_DUMMY_PROC_LINK(SN_ATTR_IDX(sn_idx)) = NULL_IDX;
      }

      sn_idx++;
   }

   TRACE (Func_Exit, "process_variable_size_func", NULL);

   return;

}  /* process_variable_size_func */

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

static void check_bd_typ_for_dargs(int		 bd_idx,
				   int		 type_idx,
				   opnd_type	*list_opnd)

{
   int		dim;
   opnd_type	opnd;

   TRACE (Func_Entry, "check_bd_typ_for_dargs", NULL);

   if (TYP_TYPE(type_idx) == Character &&
       TYP_CHAR_CLASS(type_idx) == Var_Len_Char) {
      COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(TYP_ORIG_LEN_IDX(type_idx))));
      search_expr_for_dargs(&opnd, list_opnd);
   }

   if (bd_idx != NULL_IDX) {
      if (BD_ARRAY_SIZE(bd_idx) == Var_Len_Array) {

         for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

            if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {

               COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(BD_LB_IDX(bd_idx, dim))));
               search_expr_for_dargs(&opnd, list_opnd);
            }

            if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {

               COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(BD_UB_IDX(bd_idx, dim))));
               search_expr_for_dargs(&opnd, list_opnd);
            }
         }
      }
      else if (BD_ARRAY_CLASS(bd_idx) == Assumed_Shape) {
         for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

            if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {

               COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(BD_LB_IDX(bd_idx, dim))));
               search_expr_for_dargs(&opnd, list_opnd);
            }
         }
      }
   }

   TRACE (Func_Exit, "check_bd_typ_for_dargs", NULL);

   return;

}  /* check_bd_typ_for_dargs */

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

static void search_expr_for_dargs(opnd_type	*opnd,
				  opnd_type	*list_opnd)

{
   int		list_idx;
   opnd_type	loc_opnd;

   TRACE (Func_Entry, "search_expr_for_dargs", NULL);

   switch(OPND_FLD((*opnd))) {
      case AT_Tbl_Idx:
         if (AT_OBJ_CLASS(OPND_IDX((*opnd))) == Data_Obj &&
             ATD_CLASS(OPND_IDX((*opnd))) == Dummy_Argument &&
             ATD_SF_DARG(OPND_IDX((*opnd)))) {

            if (OPND_FLD((*list_opnd)) != NO_Tbl_Idx) {
               list_idx = OPND_IDX((*list_opnd));

               while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
                  if (IL_IDX(list_idx) == OPND_IDX((*opnd))) {
                     /* already on list */
                     /* this could be a flag on the dummy attr */
                     goto OUT;
                  }
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
            }

            if (ATD_ARRAY_IDX(OPND_IDX((*opnd))) != NULL_IDX ||
                TYP_TYPE(ATD_TYPE_IDX(OPND_IDX((*opnd)))) == Character) {

               check_bd_typ_for_dargs(ATD_ARRAY_IDX(OPND_IDX((*opnd))),
                                      ATD_TYPE_IDX(OPND_IDX((*opnd))),
                                      list_opnd);
            }

            if (OPND_FLD((*list_opnd)) == NO_Tbl_Idx) {
               NTR_IR_LIST_TBL(list_idx);
               OPND_FLD((*list_opnd)) = IL_Tbl_Idx;
               OPND_IDX((*list_opnd)) = list_idx;
               OPND_LIST_CNT((*list_opnd)) = 1;
            }
            else {

               list_idx = OPND_IDX((*list_opnd));

               while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
               OPND_LIST_CNT((*list_opnd)) += 1;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         
            IL_FLD(list_idx) = AT_Tbl_Idx;
            IL_IDX(list_idx) = OPND_IDX((*opnd));
            IL_LINE_NUM(list_idx) = stmt_start_line;
            IL_COL_NUM(list_idx)  = stmt_start_col;
         }
         break;

      case IR_Tbl_Idx:
         COPY_OPND(loc_opnd, IR_OPND_L(OPND_IDX((*opnd))));
         search_expr_for_dargs(&loc_opnd, list_opnd);
         
         COPY_OPND(loc_opnd, IR_OPND_R(OPND_IDX((*opnd))));
         search_expr_for_dargs(&loc_opnd, list_opnd);

         break;
         
      case IL_Tbl_Idx:

         list_idx = OPND_IDX((*opnd));

         while (list_idx) {
            COPY_OPND(loc_opnd, IL_OPND(list_idx));
            search_expr_for_dargs(&loc_opnd, list_opnd);
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         break;

      default :
         break;

   }

OUT:

   TRACE (Func_Exit, "search_expr_for_dargs", NULL);

   return;

}  /* search_expr_for_dargs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the descriptor tables used in runtime argument checking.       *|
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

int create_argchck_descriptor(opnd_type		*call_opnd)

{
   int			arg_idx;
   int			asg_idx;
   boolean		this_is_call;
   int			col;
   int			count;
   int			desc_idx;
   int			entry_attr_idx;
   expr_arg_type        exp_desc;
   int			i;
   int			ir_idx;
   int			line;
   int			list_idx;
   int			loc_idx;
   boolean		ok;
   opnd_type		opnd;
   int			rslt_idx;
   int			shift_idx;
   int			sub_idx;
   long_type		the_constant;
   long_type		the_constant2;
   int			static_tmp_idx;
   int			stack_tmp_idx;


   TRACE (Func_Entry, "create_argchck_descriptor", NULL);

   if (OPND_FLD((*call_opnd)) == IR_Tbl_Idx) {
      /* this is a call descriptor */
      this_is_call = TRUE;
      count = IR_LIST_CNT_R(OPND_IDX((*call_opnd)));
      entry_attr_idx = IR_IDX_L(OPND_IDX((*call_opnd)));
      line  = IR_LINE_NUM(OPND_IDX((*call_opnd)));
      col   = IR_COL_NUM(OPND_IDX((*call_opnd)));
      arg_idx = IR_IDX_R(OPND_IDX((*call_opnd)));

      if (ATP_EXTRA_DARG(entry_attr_idx)) {
         count--;
         arg_idx = IL_NEXT_LIST_IDX(arg_idx);
      }
   }
   else {
      /* this is an entry descriptor */
      this_is_call = FALSE;
      entry_attr_idx = OPND_IDX((*call_opnd));
      count = ATP_NUM_DARGS(entry_attr_idx);
      line = AT_DEF_LINE(entry_attr_idx);
      col  = AT_DEF_COLUMN(entry_attr_idx);
      arg_idx = ATP_FIRST_IDX(entry_attr_idx);

      if (ATP_EXTRA_DARG(entry_attr_idx)) {
         count--;
         arg_idx++;
      }
   }

   /***************************\
   |* create the header table *|
   \***************************/

   if (num_argchck_suppress_msg > 0) {
      static_tmp_idx = 
            gen_static_integer_array_tmp(3 + count + num_argchck_suppress_msg, 
                                           line, 
                                           col);
   }
   else {
      static_tmp_idx = gen_static_integer_array_tmp(2 + count, line, col);
   }

   stack_tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

   ATD_TYPE_IDX(stack_tmp_idx)     = ATD_TYPE_IDX(static_tmp_idx);
   ATD_STOR_BLK_IDX(stack_tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

   /* This new tmp is fully created, so does not need decl_semantics */

   AT_SEMANTICS_DONE(stack_tmp_idx)      = TRUE;

   ATD_ARRAY_IDX(stack_tmp_idx) = ATD_ARRAY_IDX(static_tmp_idx);


   /***********************************\
   |* copy static attr to stack attr. *|
   \***********************************/

   OPND_FLD(opnd) = AT_Tbl_Idx;
   OPND_IDX(opnd) = stack_tmp_idx;
   OPND_LINE_NUM(opnd) = line;
   OPND_COL_NUM(opnd)  = col;

   ok = gen_whole_subscript(&opnd, &exp_desc);

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Asg_Opr;
   IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   COPY_OPND(IR_OPND_L(asg_idx), opnd);

   OPND_FLD(opnd) = AT_Tbl_Idx;
   OPND_IDX(opnd) = static_tmp_idx;
   OPND_LINE_NUM(opnd) = line;
   OPND_COL_NUM(opnd)  = col;

   ok = gen_whole_subscript(&opnd, &exp_desc);

   COPY_OPND(IR_OPND_R(asg_idx), opnd);

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   if (this_is_call) {
      /**********************************************\
      |* set the seen_this flag on the static table *|
      \**********************************************/

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;

      IR_FLD_L(sub_idx) = AT_Tbl_Idx;
      IR_IDX_L(sub_idx) = static_tmp_idx;
      IR_LINE_NUM_L(sub_idx) = line;
      IR_COL_NUM_L(sub_idx)  = col;
      IR_FLD_L(asg_idx) = IR_Tbl_Idx;
      IR_IDX_L(asg_idx) = sub_idx;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_TBL(shift_idx);
      IR_OPR(shift_idx) = Shiftl_Opr;
      IR_TYPE_IDX(shift_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(shift_idx) = line;
      IR_COL_NUM(shift_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_L(shift_idx) = IL_Tbl_Idx;
      IR_IDX_L(shift_idx) = list_idx;
      IR_LIST_CNT_L(shift_idx) = 2;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      the_constant = TARGET_BITS_PER_WORD - 1;

      IL_FLD(list_idx)  = CN_Tbl_Idx;
      IL_IDX(list_idx)  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                      the_constant);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Bor_Opr;
      IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = line;
      IR_COL_NUM(ir_idx)  = col;

      IR_FLD_R(ir_idx) = IR_Tbl_Idx;
      IR_IDX_R(ir_idx) = shift_idx;

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;

      IR_FLD_L(sub_idx) = AT_Tbl_Idx;
      IR_IDX_L(sub_idx) = static_tmp_idx;
      IR_LINE_NUM_L(sub_idx) = line;
      IR_COL_NUM_L(sub_idx)  = col;
      IR_FLD_L(ir_idx) = IR_Tbl_Idx;
      IR_IDX_L(ir_idx) = sub_idx;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = ir_idx;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }

   /*****************************************\
   |* initialize the first word with header *|
   \*****************************************/

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Init_Opr;
   IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(sub_idx) = line;
   IR_COL_NUM(sub_idx)  = col;

   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_IDX_L(sub_idx) = static_tmp_idx;
   IR_LINE_NUM_L(sub_idx) = line;
   IR_COL_NUM_L(sub_idx)  = col;
   IR_FLD_L(asg_idx) = IR_Tbl_Idx;
   IR_IDX_L(asg_idx) = sub_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_IDX_R(sub_idx) = list_idx;
   IR_LIST_CNT_R(sub_idx) = 1;

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_IDX_R(asg_idx) = list_idx;
   IR_LIST_CNT_R(asg_idx) = 3;

   the_constant = 0;

   ((arg_desc_header_type *)&the_constant)->seen_this   = 0;
   ((arg_desc_header_type *)&the_constant)->f90_flag    = 1;
   ((arg_desc_header_type *)&the_constant)->num_ck_only = 
                                    cmd_line_flags.runtime_arg_count_only;
   ((arg_desc_header_type *)&the_constant)->arg_count   = count;

   if (num_argchck_suppress_msg > 0) {
      ((arg_desc_header_type *)&the_constant)->suppress_msg = 1;
   }

   IL_FLD(list_idx)  = CN_Tbl_Idx;
   IL_IDX(list_idx)  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                   the_constant);
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

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


# if defined(_INIT_RELOC_BASE_OFFSET) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
   /* Create a temp as an overlay for the first object if   */
   /* there is no FIRST attr.                               */

   if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(static_tmp_idx)) == NULL_IDX) {
      set_sb_first_attr_idx(static_tmp_idx);
   }
# endif

   /*******************************\
   |* create call type descriptor *|
   \*******************************/

   if (ATP_PGM_UNIT(entry_attr_idx) == Function) {
      if (this_is_call &&
          ATP_EXTRA_DARG(entry_attr_idx)) {

         COPY_OPND(opnd, IL_OPND(IR_IDX_R(OPND_IDX((*call_opnd)))));
         rslt_idx = find_left_attr(&opnd);
      }
      else {
         rslt_idx = ATP_RSLT_IDX(entry_attr_idx);
      }
   }
   else {
      rslt_idx = NULL_IDX;
   }

   the_constant = 2;

   desc_idx = gen_call_type_descriptor(entry_attr_idx, 
                                       rslt_idx,
                                       this_is_call, 
                                       line, 
                                       col);

   if (ATD_STOR_BLK_IDX(desc_idx) == SCP_SB_STACK_IDX(curr_scp_idx)) {
      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;
      IR_FLD_L(sub_idx) = AT_Tbl_Idx;
      IR_IDX_L(sub_idx) = stack_tmp_idx;
      IR_LINE_NUM_L(sub_idx) = line;
      IR_COL_NUM_L(sub_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_TWO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      IR_FLD_L(asg_idx) = IR_Tbl_Idx;
      IR_IDX_L(asg_idx) = sub_idx;

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)      = Loc_Opr;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;

      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;

      IR_FLD_L(loc_idx) = AT_Tbl_Idx;
      IR_IDX_L(loc_idx) = desc_idx;
      IR_LINE_NUM_L(loc_idx) = line;
      IR_COL_NUM_L(loc_idx)  = col;

      IR_FLD_R(asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(asg_idx) = loc_idx;
   }
   else {
      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;
      IR_FLD_L(sub_idx) = AT_Tbl_Idx;
      IR_IDX_L(sub_idx) = static_tmp_idx;
      IR_LINE_NUM_L(sub_idx) = line;
      IR_COL_NUM_L(sub_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_TWO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Init_Reloc_Opr;
      IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      IR_FLD_L(asg_idx) = IR_Tbl_Idx;
      IR_IDX_L(asg_idx) = sub_idx;

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)      = Loc_Opr;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;

      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;

      IR_FLD_L(loc_idx) = AT_Tbl_Idx;
      IR_IDX_L(loc_idx) = desc_idx;
      IR_LINE_NUM_L(loc_idx) = line;
      IR_COL_NUM_L(loc_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(asg_idx) = IL_Tbl_Idx;
      IR_IDX_R(asg_idx) = list_idx;
      IR_LIST_CNT_R(asg_idx) = 2;
      IL_FLD(list_idx)  = IR_Tbl_Idx;
      IL_IDX(list_idx)  = loc_idx;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }


   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


   /*************************************************\
   |* create argument descriptors for all arguments *|
   \*************************************************/

   for (i = 0; i < count; i++) {

      the_constant++;

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;
      IR_FLD_L(sub_idx) = AT_Tbl_Idx;
      IR_IDX_L(sub_idx) = static_tmp_idx;
      IR_LINE_NUM_L(sub_idx) = line;
      IR_COL_NUM_L(sub_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, the_constant);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      if (this_is_call &&
          IL_ARG_DESC_IDX(arg_idx) == NULL_IDX) {

         /* argument not present, initialize to zero */

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Init_Opr;
         IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;

         IR_FLD_L(asg_idx) = IR_Tbl_Idx;
         IR_IDX_L(asg_idx) = sub_idx;

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(asg_idx) = IL_Tbl_Idx;
         IR_IDX_R(asg_idx) = list_idx;
         IR_LIST_CNT_R(asg_idx) = 3;

         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
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
      else {

         desc_idx = gen_arg_type_descriptor(arg_idx, 
                                            this_is_call, 
                                            line, 
                                            col);

         if (ATD_STOR_BLK_IDX(desc_idx) == SCP_SB_STACK_IDX(curr_scp_idx)) {

            IR_IDX_L(sub_idx) = stack_tmp_idx;

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;
      
            IR_FLD_L(asg_idx) = IR_Tbl_Idx;
            IR_IDX_L(asg_idx) = sub_idx;

            NTR_IR_TBL(loc_idx);
            IR_OPR(loc_idx)      = Loc_Opr;
            IR_LINE_NUM(loc_idx) = line;
            IR_COL_NUM(loc_idx)  = col;

            IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;

            IR_FLD_L(loc_idx) = AT_Tbl_Idx;
            IR_IDX_L(loc_idx) = desc_idx;
            IR_LINE_NUM_L(loc_idx) = line;
            IR_COL_NUM_L(loc_idx)  = col;

            IR_FLD_R(asg_idx) = IR_Tbl_Idx;
            IR_IDX_R(asg_idx) = loc_idx;
         }
         else {

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Init_Reloc_Opr;
            IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;

            IR_FLD_L(asg_idx) = IR_Tbl_Idx;
            IR_IDX_L(asg_idx) = sub_idx;

            NTR_IR_TBL(loc_idx);
            IR_OPR(loc_idx)      = Loc_Opr;
            IR_LINE_NUM(loc_idx) = line;
            IR_COL_NUM(loc_idx)  = col;

            IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;

            IR_FLD_L(loc_idx) = AT_Tbl_Idx;
            IR_IDX_L(loc_idx) = desc_idx;
            IR_LINE_NUM_L(loc_idx) = line;
            IR_COL_NUM_L(loc_idx)  = col;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(asg_idx) = IL_Tbl_Idx;
            IR_IDX_R(asg_idx) = list_idx;
            IR_LIST_CNT_R(asg_idx) = 2;
            IL_FLD(list_idx)  = IR_Tbl_Idx;
            IL_IDX(list_idx)  = loc_idx;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            IL_FLD(list_idx) = CN_Tbl_Idx;
            IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;
         }
      }


      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
   
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


      if (this_is_call) {
         arg_idx = IL_NEXT_LIST_IDX(arg_idx);
      }
      else {
         arg_idx++;
      }
   }

   if (num_argchck_suppress_msg > 0) {
      for (i = 1; i <= num_argchck_suppress_msg+1; i++) {
         the_constant++;

         NTR_IR_TBL(sub_idx);
         IR_OPR(sub_idx) = Subscript_Opr;
         IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(sub_idx) = line;
         IR_COL_NUM(sub_idx)  = col;
         IR_FLD_L(sub_idx) = AT_Tbl_Idx;
         IR_IDX_L(sub_idx) = static_tmp_idx;
         IR_LINE_NUM_L(sub_idx) = line;
         IR_COL_NUM_L(sub_idx)  = col;
   
         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_IDX_R(sub_idx) = list_idx;
         IR_LIST_CNT_R(sub_idx) = 1;
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, the_constant);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
   
         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Init_Opr;
         IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;
   
         IR_FLD_L(asg_idx) = IR_Tbl_Idx;
         IR_IDX_L(asg_idx) = sub_idx;
   
         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(asg_idx) = IL_Tbl_Idx;
         IR_IDX_R(asg_idx) = list_idx;
         IR_LIST_CNT_R(asg_idx) = 3;
   
         the_constant2 = argchck_suppress_msg[i];

         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, the_constant2);
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

         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }
   }

   TRACE (Func_Exit, "create_argchck_descriptor", NULL);

   return(stack_tmp_idx);

}  /* create_argchck_descriptor */

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

static int gen_call_type_descriptor(int		attr_idx,
				    int		rslt_idx,
				    boolean	this_is_call,
				    int		line,
				    int		col)

{
   int			act_file_line;
   int			asg_idx;
   int			const_idx;
   expr_arg_type	exp_desc;
   int			glb_idx;
   int			idx;
   int			list_idx;
   int			loc_idx;
   long_type		num[MAX_WORDS_FOR_INTEGER];
   boolean		ok;
   opnd_type		opnd;
   int			sub_idx;
   int			static_tmp_idx;
   int			stack_tmp_idx = NULL_IDX;
   long_type		the_constant;
   int			type_idx;


   TRACE (Func_Entry, "gen_call_type_descriptor", NULL);

   /*******************************\
   |* create the descriptor table *|
   \*******************************/

   static_tmp_idx = gen_static_integer_array_tmp(NUM_TARGET_ARGCHCK_DESC_WORDS, 
                                                 line, 
                                                 col);


   if (ATP_PGM_UNIT(attr_idx) == Function &&
       ((ATD_ARRAY_IDX(rslt_idx) != NULL_IDX &&
         BD_LEN_FLD(ATD_ARRAY_IDX(rslt_idx)) != CN_Tbl_Idx &&
         BD_LEN_IDX(ATD_ARRAY_IDX(rslt_idx)) != NULL_IDX) ||
        (TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) == Character &&
         TYP_FLD(ATD_TYPE_IDX(rslt_idx)) != CN_Tbl_Idx))) {

      stack_tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

      ATD_TYPE_IDX(stack_tmp_idx)     = ATD_TYPE_IDX(static_tmp_idx);
      ATD_STOR_BLK_IDX(stack_tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      /* This new tmp is fully created, so does not need decl_semantics */

      AT_SEMANTICS_DONE(stack_tmp_idx)      = TRUE;

      ATD_ARRAY_IDX(stack_tmp_idx) = ATD_ARRAY_IDX(static_tmp_idx);


      /***********************************\
      |* copy static attr to stack attr. *|
      \***********************************/

      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_IDX(opnd) = stack_tmp_idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      ok = gen_whole_subscript(&opnd, &exp_desc);

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      COPY_OPND(IR_OPND_L(asg_idx), opnd);

      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_IDX(opnd) = static_tmp_idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      ok = gen_whole_subscript(&opnd, &exp_desc);

      COPY_OPND(IR_OPND_R(asg_idx), opnd);

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   }

   /* get a typeless constant that is one less than the size of the temp  */
   /* This is to allow a reloc init on the last word. Some codegens won't */
   /* allow multiple inits of the same address.                           */

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
   TYP_BIT_LEN(TYP_WORK_IDX)	= (NUM_TARGET_ARGCHCK_DESC_WORDS - 1) * 
                                              TARGET_BITS_PER_WORD;
   type_idx			= ntr_type_tbl();

   const_idx    = ntr_const_tbl(type_idx, FALSE, NULL);

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(sub_idx) = line;
   IR_COL_NUM(sub_idx)  = col;
   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_IDX_L(sub_idx) = static_tmp_idx;
   IR_LINE_NUM_L(sub_idx) = line;
   IR_COL_NUM_L(sub_idx)  = col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_IDX_R(sub_idx) = list_idx;
   IR_LIST_CNT_R(sub_idx) = 1;
   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Init_Opr;
   IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   IR_FLD_L(asg_idx) = IR_Tbl_Idx;
   IR_IDX_L(asg_idx) = sub_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_IDX_R(asg_idx) = list_idx;
   IR_LIST_CNT_R(asg_idx) = 3;

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

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /****************************\
   |* Now fill in the constant *|
   \****************************/

   /* BRIANJ  - arg desc fields. */

   /* fill in name and file line num for traceback */

   strcpy(((arg_desc_node_type *)&(CN_CONST(const_idx)))->name, 
          AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));

   if (this_is_call) {  /* BRIANJ */
      GLOBAL_LINE_TO_FILE_LINE(line, glb_idx, act_file_line);
      C_TO_F_INT(num, act_file_line, CG_INTEGER_DEFAULT_TYPE);

      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->line = num[0];
   }

   if (ATP_PGM_UNIT(attr_idx) == Function) {
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->arg_type = 
                          get_arg_type(ATD_TYPE_IDX(rslt_idx),
                                       TRUE);
      C_TO_F_INT(num, 
                 linear_to_kind_type[TYP_LINEAR(ATD_TYPE_IDX(rslt_idx))],
                 CG_INTEGER_DEFAULT_TYPE);
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->kind = num[0];

      if (ATD_ARRAY_IDX(rslt_idx) != NULL_IDX) {

         C_TO_F_INT(num,
                    BD_RANK(ATD_ARRAY_IDX(rslt_idx)),
                    CG_INTEGER_DEFAULT_TYPE);

         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->rank = num[0];

         if (BD_LEN_FLD(ATD_ARRAY_IDX(rslt_idx)) == CN_Tbl_Idx &&
             BD_LEN_IDX(ATD_ARRAY_IDX(rslt_idx)) != NULL_IDX) {

            ((arg_desc_node_type *)&(CN_CONST(const_idx)))->size = 
                          CN_CONST(BD_LEN_IDX(ATD_ARRAY_IDX(rslt_idx)));
         }
         else if (BD_LEN_IDX(ATD_ARRAY_IDX(rslt_idx)) != NULL_IDX) {
            /* runtime code to set the size */

            NTR_IR_TBL(sub_idx);
            IR_OPR(sub_idx) = Subscript_Opr;
            IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(sub_idx) = line;
            IR_COL_NUM(sub_idx)  = col;
            IR_FLD_L(sub_idx) = AT_Tbl_Idx;
            IR_IDX_L(sub_idx) = stack_tmp_idx;
            IR_LINE_NUM_L(sub_idx) = line;
            IR_COL_NUM_L(sub_idx)  = col;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(sub_idx) = IL_Tbl_Idx;
            IR_IDX_R(sub_idx) = list_idx;
            IR_LIST_CNT_R(sub_idx) = 1;
            IL_FLD(list_idx) = CN_Tbl_Idx;

            the_constant = ARGCHCK_SIZE_IDX;
            IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                           the_constant);
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;

            IR_FLD_L(asg_idx) = IR_Tbl_Idx;
            IR_IDX_L(asg_idx) = sub_idx;

            IR_FLD_R(asg_idx) = BD_LEN_FLD(ATD_ARRAY_IDX(rslt_idx));
            IR_IDX_R(asg_idx) = BD_LEN_IDX(ATD_ARRAY_IDX(rslt_idx));
            IR_LINE_NUM_R(asg_idx) = line;
            IR_COL_NUM_R(asg_idx)  = col;

            gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
         
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         }
      }
      else {
         C_TO_F_INT(num, 1, CG_INTEGER_DEFAULT_TYPE);
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->size = num[0];

         C_TO_F_INT(num, 0, CG_INTEGER_DEFAULT_TYPE);
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->rank = num[0];
      }

      if (TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) == Character) {

         if (TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) == Character &&
             TYP_CHAR_CLASS(ATD_TYPE_IDX(rslt_idx)) == Assumed_Size_Char) {
            ((arg_desc_node_type *)&(CN_CONST(const_idx)))->assumed_size_char = 
                                                  TRUE;
         }
         else if (TYP_FLD(ATD_TYPE_IDX(rslt_idx)) == CN_Tbl_Idx) {
            ((arg_desc_node_type *)&(CN_CONST(const_idx)))->char_len = 
                          CN_CONST(TYP_IDX(ATD_TYPE_IDX(rslt_idx)));
         }
         else {
            /* generate runtime code to insert the char length */

            NTR_IR_TBL(sub_idx);
            IR_OPR(sub_idx) = Subscript_Opr;
            IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(sub_idx) = line;
            IR_COL_NUM(sub_idx)  = col;
            IR_FLD_L(sub_idx) = AT_Tbl_Idx;
            IR_IDX_L(sub_idx) = stack_tmp_idx;
            IR_LINE_NUM_L(sub_idx) = line;
            IR_COL_NUM_L(sub_idx)  = col;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(sub_idx) = IL_Tbl_Idx;
            IR_IDX_R(sub_idx) = list_idx;
            IR_LIST_CNT_R(sub_idx) = 1;
            IL_FLD(list_idx) = CN_Tbl_Idx;
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = col;

            the_constant = ARGCHCK_CHAR_LEN_IDX;
            IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                           the_constant);

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;

            IR_FLD_L(asg_idx) = IR_Tbl_Idx;
            IR_IDX_L(asg_idx) = sub_idx;

            IR_FLD_R(asg_idx) = TYP_FLD(ATD_TYPE_IDX(rslt_idx));
            IR_IDX_R(asg_idx) = TYP_IDX(ATD_TYPE_IDX(rslt_idx));
            IR_LINE_NUM_R(asg_idx) = line;
            IR_COL_NUM_R(asg_idx)  = col;

            gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         }
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) == Structure) {
         /* generate derived type table, always static */

         NTR_IR_TBL(sub_idx);
         IR_OPR(sub_idx) = Subscript_Opr;
         IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(sub_idx) = line;
         IR_COL_NUM(sub_idx)  = col;
         IR_FLD_L(sub_idx) = AT_Tbl_Idx;
         IR_IDX_L(sub_idx) = static_tmp_idx;
         IR_LINE_NUM_L(sub_idx) = line;
         IR_COL_NUM_L(sub_idx)  = col;

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_IDX_R(sub_idx) = list_idx;
         IR_LIST_CNT_R(sub_idx) = 1;
         IL_FLD(list_idx) = CN_Tbl_Idx;
         the_constant = ARGCHCK_STRUCT_TBL_IDX;
         IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, the_constant);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Init_Reloc_Opr;
         IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;

         IR_FLD_L(asg_idx) = IR_Tbl_Idx;
         IR_IDX_L(asg_idx) = sub_idx;

         NTR_IR_TBL(loc_idx);
         IR_OPR(loc_idx)      = Loc_Opr;
         IR_LINE_NUM(loc_idx) = line;
         IR_COL_NUM(loc_idx)  = col;
         IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;

         IR_FLD_L(loc_idx) = AT_Tbl_Idx;
         idx = create_struct_argchck_tbl(TYP_IDX(ATD_TYPE_IDX(rslt_idx)));
         IR_IDX_L(loc_idx) = idx;
         IR_LINE_NUM_L(loc_idx) = line;
         IR_COL_NUM_L(loc_idx)  = col;

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(asg_idx) = IL_Tbl_Idx;
         IR_IDX_R(asg_idx) = list_idx;
         IR_LIST_CNT_R(asg_idx) = 2;
         IL_FLD(list_idx)  = IR_Tbl_Idx;
         IL_IDX(list_idx)  = loc_idx;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      
         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }

      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->pointer     = 
                          ATD_POINTER(rslt_idx);
   }
   else if (ATP_PGM_UNIT(attr_idx) == Subroutine) {
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->arg_type = 
                          Subroutine_Arg;
   }
   else {
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->arg_type = 
                          Subprogram_Arg;
   }

   TRACE (Func_Exit, "gen_call_type_descriptor", NULL);

   return((stack_tmp_idx ? stack_tmp_idx : static_tmp_idx));

}  /* gen_call_type_descriptor */

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

static int gen_arg_type_descriptor(int         idx,
                                   boolean     this_is_call,
                                   int         line,
                                   int         col)

{
   int                  asg_idx;
   int			attr_idx = NULL_IDX;
   int			bd_idx = NULL_IDX;
   opnd_type		char_len_opnd;  /*  BRIANJ - set but not used.*/
   int                  const_idx;
   expr_arg_type	exp_desc;
   long_type		folded_const[MAX_WORDS_FOR_INTEGER];
   int			i;
#ifdef KEY /* Bug 10177 */
   int			info_idx = 0;
#else /* KEY Bug 10177 */
   int			info_idx;
#endif /* KEY Bug 10177 */
   opnd_type		len_opnd;
   int                  list_idx;
   int                  loc_idx;
   int                  mult_idx;
   long_type		num[MAX_WORDS_FOR_INTEGER];
   boolean		ok;
   opnd_type		opnd;
   boolean		pgm_unit = FALSE;
   int			rank = 0;
   expr_mode_type       save_expr_mode;
   cif_usage_code_type  save_xref_state;
   int                  sub_idx;
   int                  static_tmp_idx;
   int                  stack_tmp_idx = NULL_IDX;
   int                  type_idx = NULL_IDX;
   int                  type_idx2;
   int			unused1;
   int			unused2;


   TRACE (Func_Entry, "gen_arg_type_descriptor", NULL);

   len_opnd = null_opnd;
   OPND_LINE_NUM(len_opnd) = line;
   OPND_COL_NUM(len_opnd)  = col;

   if (this_is_call) {
      /* then idx is a IL_TBl_Idx */

      info_idx = IL_ARG_DESC_IDX(idx);

# ifdef _DEBUG
      if (info_idx == NULL_IDX) {
         PRINTMSG(line, 626, Internal, col,
                  "valid info_idx", "gen_arg_type_descriptor");
      }
# endif

      pgm_unit = arg_info_list[info_idx].pgm_unit;

      COPY_OPND(opnd, IL_OPND(idx));

      if (OPND_FLD(opnd) == IR_Tbl_Idx &&
          (IR_OPR(OPND_IDX(opnd)) == Loc_Opr ||
           IR_OPR(OPND_IDX(opnd)) == Aloc_Opr ||
           IR_OPR(OPND_IDX(opnd)) == Const_Tmp_Loc_Opr)) {

         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (OPND_FLD(opnd) == IR_Tbl_Idx &&
          (IR_OPR(OPND_IDX(opnd)) == Substring_Opr ||
           IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr)) {

         COPY_OPND(char_len_opnd, 
                   IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                              IR_IDX_R(OPND_IDX(opnd))))));
      }

      type_idx = arg_info_list[info_idx].ed.type_idx;
      rank = arg_info_list[info_idx].ed.rank;

      if (arg_info_list[info_idx].association == PASS_SECTION_ADDRESS ||
          arg_info_list[info_idx].association == PASS_ADDRESS_FROM_DV ||
          arg_info_list[info_idx].association == CHECK_CONTIG_FLAG) {
         /* must get the array length from shape */

         COPY_OPND(len_opnd, arg_info_list[info_idx].ed.shape[0]);

         for (i = 1; i < rank; i++) {
            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx) = Mult_Opr;
            IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx) = line;
            IR_COL_NUM(mult_idx)  = col;

            COPY_OPND(IR_OPND_L(mult_idx), len_opnd);

            COPY_OPND(IR_OPND_R(mult_idx), 
                      arg_info_list[info_idx].ed.shape[i]);
            
            OPND_FLD(len_opnd) = IR_Tbl_Idx;
            OPND_IDX(len_opnd) = mult_idx;
         }

         save_xref_state = xref_state;
         xref_state      = CIF_No_Usage_Rec;
         save_expr_mode  = expr_mode;
         expr_mode       = Regular_Expr;

         exp_desc.rank   = 0;
         ok = expr_semantics(&len_opnd, &exp_desc);
         xref_state = save_xref_state;
         expr_mode  = save_expr_mode;
      }
      else if (IL_FLD(idx) != CN_Tbl_Idx) {

         COPY_OPND(opnd, IL_OPND(idx));
         attr_idx = find_base_attr(&opnd, &unused1, &unused2);

         if (attr_idx != NULL_IDX &&
             AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

            bd_idx = ATD_ARRAY_IDX(attr_idx);
 
            if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape ||
                BD_ARRAY_CLASS(bd_idx) == Assumed_Shape) {
               /* must get the array length from shape */

               COPY_OPND(len_opnd, arg_info_list[info_idx].ed.shape[0]);

               for (i = 1; i < rank; i++) {
                  NTR_IR_TBL(mult_idx);
                  IR_OPR(mult_idx) = Mult_Opr;
                  IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
                  IR_LINE_NUM(mult_idx) = line;
                  IR_COL_NUM(mult_idx)  = col;

                  COPY_OPND(IR_OPND_L(mult_idx), len_opnd);

                  COPY_OPND(IR_OPND_R(mult_idx),
                            arg_info_list[info_idx].ed.shape[i]);

                  OPND_FLD(len_opnd) = IR_Tbl_Idx;
                  OPND_IDX(len_opnd) = mult_idx;
               }

               save_xref_state = xref_state;
               xref_state      = CIF_No_Usage_Rec;
               save_expr_mode  = expr_mode;
               expr_mode       = Regular_Expr;

               exp_desc.rank   = 0;
               ok = expr_semantics(&len_opnd, &exp_desc);
               xref_state = save_xref_state;
               expr_mode  = save_expr_mode;
            }
            else {
               OPND_FLD(len_opnd) = BD_LEN_FLD(bd_idx);
               OPND_IDX(len_opnd) = BD_LEN_IDX(bd_idx);
            }
         }
      }
   }
   else {
      /* idx is an secondary name table idx */
      attr_idx = SN_ATTR_IDX(idx);

      pgm_unit = (AT_OBJ_CLASS(attr_idx) == Pgm_Unit);

      if (pgm_unit) {
         if (ATP_PGM_UNIT(attr_idx) == Function) {

            if (ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx)) != NULL_IDX) {
               bd_idx = ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx));

               rank = BD_RANK(bd_idx);
      
               OPND_FLD(len_opnd) = BD_LEN_FLD(bd_idx);
               OPND_IDX(len_opnd) = BD_LEN_IDX(bd_idx);
            }

            type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));

            if (TYP_TYPE(type_idx) == Character) {
               OPND_FLD(char_len_opnd) = TYP_FLD(type_idx);
               OPND_IDX(char_len_opnd) = TYP_IDX(type_idx);
            }
         }
      }
      else {
         if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
            bd_idx = ATD_ARRAY_IDX(attr_idx);
   
            rank = BD_RANK(bd_idx);
 
            OPND_FLD(len_opnd) = BD_LEN_FLD(bd_idx);
            OPND_IDX(len_opnd) = BD_LEN_IDX(bd_idx);
         }
   
         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            type_idx = ATD_TYPE_IDX(attr_idx);

            if (TYP_TYPE(type_idx) == Character) {
               OPND_FLD(char_len_opnd) = TYP_FLD(type_idx);
               OPND_IDX(char_len_opnd) = TYP_IDX(type_idx);
            }
         }
      }
   }

   /*******************************\
   |* create the descriptor table *|
   \*******************************/

   static_tmp_idx = gen_static_integer_array_tmp(NUM_TARGET_ARGCHCK_DESC_WORDS,
                                                 line,
                                                 col);

   if (type_idx   &&
       ((rank != 0 &&
        OPND_FLD(len_opnd) != CN_Tbl_Idx &&
        OPND_IDX(len_opnd) != NULL_IDX) ||
       (TYP_TYPE(type_idx) == Character &&
        TYP_FLD(type_idx) != CN_Tbl_Idx))) {

      stack_tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

      ATD_TYPE_IDX(stack_tmp_idx)     = ATD_TYPE_IDX(static_tmp_idx);
      ATD_STOR_BLK_IDX(stack_tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      /* This new tmp is fully created, so does not need decl_semantics */

      AT_SEMANTICS_DONE(stack_tmp_idx)      = TRUE;

      ATD_ARRAY_IDX(stack_tmp_idx) = ATD_ARRAY_IDX(static_tmp_idx);


      /***********************************\
      |* copy static attr to stack attr. *|
      \***********************************/

      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_IDX(opnd) = stack_tmp_idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      ok = gen_whole_subscript(&opnd, &exp_desc);

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      COPY_OPND(IR_OPND_L(asg_idx), opnd);

      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_IDX(opnd) = static_tmp_idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      ok = gen_whole_subscript(&opnd, &exp_desc);

      COPY_OPND(IR_OPND_R(asg_idx), opnd);

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   }


   /* get a typeless constant that is one less than the size of the temp  */
   /* This is to allow a reloc init on the last word. Some codegens won't */
   /* allow multiple inits of the same address.                           */

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
   TYP_BIT_LEN(TYP_WORK_IDX)	= (NUM_TARGET_ARGCHCK_DESC_WORDS - 1) * 
                                              TARGET_BITS_PER_WORD;
   type_idx2			= ntr_type_tbl();

   const_idx    = ntr_const_tbl(type_idx2, FALSE, NULL);

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(sub_idx) = line;
   IR_COL_NUM(sub_idx)  = col;
   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_IDX_L(sub_idx) = static_tmp_idx;
   IR_LINE_NUM_L(sub_idx) = line;
   IR_COL_NUM_L(sub_idx)  = col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_IDX_R(sub_idx) = list_idx;
   IR_LIST_CNT_R(sub_idx) = 1;
   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Init_Opr;
   IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   IR_FLD_L(asg_idx) = IR_Tbl_Idx;
   IR_IDX_L(asg_idx) = sub_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_IDX_R(asg_idx) = list_idx;
   IR_LIST_CNT_R(asg_idx) = 3;

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

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /****************************\
   |* Now fill in the constant *|
   \****************************/

   ((arg_desc_node_type *)&(CN_CONST(const_idx)))->pgm_unit = pgm_unit;

   if (attr_idx &&
       AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
       ATP_PGM_UNIT(attr_idx) == Subroutine) {

      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->arg_type = 
                                         Subroutine_Arg;
   }
   else if (type_idx) {
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->arg_type =
                       get_arg_type(type_idx, pgm_unit);

      C_TO_F_INT(num, 
                 linear_to_kind_type[TYP_LINEAR(type_idx)],
                 CG_INTEGER_DEFAULT_TYPE);

      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->kind = num[0];
      C_TO_F_INT(num, rank, CG_INTEGER_DEFAULT_TYPE);

      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->rank = num[0];

      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->default_kind = 
                              (TYP_DESC(type_idx) == Default_Typed);
   }

   if (attr_idx &&
       AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
       ATP_PGM_UNIT(attr_idx) == Pgm_Unknown) {  /* BRIANJ - target const??? */
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->pgm_unknown = 1;
   }

   if (this_is_call &&
       arg_info_list[info_idx].ed.array_elt) {

      /* calculate size to end of array. maybe */
   }
   else if (rank != 0) {

      if (OPND_FLD(len_opnd) == CN_Tbl_Idx &&
          OPND_IDX(len_opnd) != NULL_IDX   &&
          TYP_TYPE(type_idx) != Character) {


          /* BRIANJ - What if these are integer_8 on a 32 bit platform? */

         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->size =
                       CN_CONST(OPND_IDX(len_opnd));
      }
      else if (OPND_FLD(len_opnd) == CN_Tbl_Idx &&
               OPND_IDX(len_opnd) != NULL_IDX   &&
               TYP_TYPE(type_idx) == Character     &&
               TYP_FLD(type_idx) == CN_Tbl_Idx) {

         type_idx2 = (TYP_LINEAR(CN_TYPE_IDX(OPND_IDX(len_opnd))) > 
                      TYP_LINEAR(CN_TYPE_IDX(TYP_IDX(type_idx)))) ?
                                 CN_TYPE_IDX(OPND_IDX(len_opnd)) :
                                 CN_TYPE_IDX(TYP_IDX(type_idx));

         ok = folder_driver((char *)&CN_CONST(OPND_IDX(len_opnd)),
                                     CN_TYPE_IDX(OPND_IDX(len_opnd)),
                            (char *)&CN_CONST(TYP_IDX(type_idx)),
                                     CN_TYPE_IDX(TYP_IDX(type_idx)),
                                     folded_const,
                                    &type_idx2,
                                     stmt_start_line,
                                     stmt_start_col,
                                     2,
                                     Mult_Opr);

         /* This could loose precision if type_idx2 is Integer_8.  BRIANJ */

# if defined(_TARGET32)
         if (TYP_LINEAR(type_idx2) == Integer_8) {
            ((arg_desc_node_type *)&(CN_CONST(const_idx)))->size =
                                     folded_const[1];
         }
         else {
            ((arg_desc_node_type *)&(CN_CONST(const_idx)))->size =
                                     folded_const[0];
         }
# else
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->size = folded_const[0];
# endif
      }
      else if (OPND_IDX(len_opnd) != NULL_IDX) {
         /* runtime code to set the size */

         NTR_IR_TBL(sub_idx);
         IR_OPR(sub_idx) = Subscript_Opr;
         IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(sub_idx) = line;
         IR_COL_NUM(sub_idx)  = col;
         IR_FLD_L(sub_idx) = AT_Tbl_Idx;
         IR_IDX_L(sub_idx) = stack_tmp_idx;
         IR_LINE_NUM_L(sub_idx) = line;
         IR_COL_NUM_L(sub_idx)  = col;

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_IDX_R(sub_idx) = list_idx;
         IR_LIST_CNT_R(sub_idx) = 1;
         IL_FLD(list_idx) = CN_Tbl_Idx;

         IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                        ARGCHCK_SIZE_IDX);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;

         IR_FLD_L(asg_idx) = IR_Tbl_Idx;
         IR_IDX_L(asg_idx) = sub_idx;

         if (TYP_TYPE(type_idx) == Character) {
            NTR_IR_TBL(mult_idx);
            IR_OPR(mult_idx) = Mult_Opr;
            IR_TYPE_IDX(mult_idx) = CG_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(mult_idx) = line;
            IR_COL_NUM(mult_idx)  = col;

            COPY_OPND(IR_OPND_L(mult_idx), len_opnd);
            IR_FLD_R(mult_idx) = TYP_FLD(type_idx);
            IR_IDX_R(mult_idx) = TYP_IDX(type_idx);
            IR_LINE_NUM_R(mult_idx) = line;
            IR_COL_NUM_R(mult_idx)  = col;

            IR_FLD_R(asg_idx) = IR_Tbl_Idx;
            IR_IDX_R(asg_idx) = mult_idx;
         }
         else {
            COPY_OPND(IR_OPND_R(asg_idx), len_opnd);
         }

         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }
   }
   else {
      C_TO_F_INT(num, 1, CG_INTEGER_DEFAULT_TYPE);
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->size = num[0];
      C_TO_F_INT(num, 0, CG_INTEGER_DEFAULT_TYPE);
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->rank = num[0];
   }

   if (type_idx &&
       TYP_TYPE(type_idx) == Character) {

      if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->char_len =
                       CN_CONST(TYP_IDX(type_idx));
      }
      else {
         /* generate runtime code to insert the char length */

         NTR_IR_TBL(sub_idx);
         IR_OPR(sub_idx) = Subscript_Opr;
         IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(sub_idx) = line;
         IR_COL_NUM(sub_idx)  = col;
         IR_FLD_L(sub_idx) = AT_Tbl_Idx;
         IR_IDX_L(sub_idx) = stack_tmp_idx;
         IR_LINE_NUM_L(sub_idx) = line;
         IR_COL_NUM_L(sub_idx)  = col;

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_IDX_R(sub_idx) = list_idx;
         IR_LIST_CNT_R(sub_idx) = 1;
         IL_FLD(list_idx) = CN_Tbl_Idx;

         IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                        ARGCHCK_CHAR_LEN_IDX);
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;

         IR_FLD_L(asg_idx) = IR_Tbl_Idx;
         IR_IDX_L(asg_idx) = sub_idx;

         IR_FLD_R(asg_idx) = TYP_FLD(type_idx);
         IR_IDX_R(asg_idx) = TYP_IDX(type_idx);
         IR_LINE_NUM_R(asg_idx) = line;
         IR_COL_NUM_R(asg_idx)  = col;

         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      }
   }
   else if (type_idx &&
            TYP_TYPE(type_idx) == Structure) {
      /* generate derived type table, always static */

      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;
      IR_FLD_L(sub_idx) = AT_Tbl_Idx;
      IR_IDX_L(sub_idx) = static_tmp_idx;
      IR_LINE_NUM_L(sub_idx) = line;
      IR_COL_NUM_L(sub_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                     ARGCHCK_STRUCT_TBL_IDX);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Init_Reloc_Opr;
      IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      IR_FLD_L(asg_idx) = IR_Tbl_Idx;
      IR_IDX_L(asg_idx) = sub_idx;

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)      = Loc_Opr;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;

      IR_FLD_L(loc_idx) = AT_Tbl_Idx;
      i = create_struct_argchck_tbl(TYP_IDX(type_idx));
      IR_IDX_L(loc_idx) = i;
      IR_LINE_NUM_L(loc_idx) = line;
      IR_COL_NUM_L(loc_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(asg_idx) = IL_Tbl_Idx;
      IR_IDX_R(asg_idx) = list_idx;
      IR_LIST_CNT_R(asg_idx) = 2;
      IL_FLD(list_idx)  = IR_Tbl_Idx;
      IL_IDX(list_idx)  = loc_idx;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }

   if (this_is_call) {
      /* this is an actual arg */
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->defineable = 
                                   arg_info_list[info_idx].ed.reference;
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->array_element = 
				   arg_info_list[info_idx].ed.array_elt;
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->pointer     =
				   arg_info_list[info_idx].ed.pointer;

      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->dope_vector =
				   arg_info_list[info_idx].ed.dope_vector;
   }
   else {
      /* this is a dummy arg */
      ((arg_desc_node_type *)&(CN_CONST(const_idx)))->optional = 
                                               AT_OPTIONAL(attr_idx);
      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          (ATD_INTENT(attr_idx) == Intent_Out ||
           ATD_INTENT(attr_idx) == Intent_Inout)) {

         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->intent_out = TRUE;
      }

      if (bd_idx) {
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->assumed_shape = 
                           (BD_ARRAY_CLASS(bd_idx) == Assumed_Shape);
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->assumed_size_array = 
                           (BD_ARRAY_CLASS(bd_idx) == Assumed_Size);
      }

      if (type_idx &&
          TYP_TYPE(type_idx) == Character &&
          TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->assumed_size_char = 
                                                  TRUE;
      }

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->pointer     =
                          ATD_POINTER(attr_idx);

         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->dope_vector =
                          ATD_IM_A_DOPE(attr_idx);

         ((arg_desc_node_type *)&(CN_CONST(const_idx)))->ignore_tkr = 
                          ATD_IGNORE_TKR(attr_idx);
      }
   }

   TRACE (Func_Exit, "gen_arg_type_descriptor", NULL);

   return((stack_tmp_idx ? stack_tmp_idx : static_tmp_idx));

}  /* gen_arg_type_descriptor */

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

static long_type get_arg_type(int	type_idx,
			      boolean	is_pgm_unit)

{
#ifdef KEY /* Bug 10177 */
   long_type		arg_type = 0;
#else /* KEY Bug 10177 */
   long_type		arg_type;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "get_arg_type", NULL);

   switch (TYP_LINEAR(type_idx)) {

   case Typeless_4:
   case Typeless_8:
   case Short_Typeless_Const:
   case Long_Typeless:
      arg_type = (is_pgm_unit ?
                  Typeless_Function_Arg : Typeless_Arg);
      break;

   case Integer_1:
   case Integer_2:
   case Integer_4:
      arg_type = (is_pgm_unit ? 
                  Short_Integer_Function_Arg : Short_Integer_Arg);
      break;

   case Integer_8:
      arg_type = (is_pgm_unit ? 
                  Long_Integer_Function_Arg : Long_Integer_Arg);
      break;

   case Real_4:
# ifdef _TARGET64
   case Real_8:
# endif
      arg_type = (is_pgm_unit ? 
                  Real_Function_Arg : Real_Arg);
      break;

# ifdef _TARGET32
   case Real_8:
# endif
   case Real_16:
      arg_type = (is_pgm_unit ? 
                  Double_Function_Arg : Double_Arg);
      break;

   case Complex_4:
   case Complex_8:
   case Complex_16:
      arg_type = (is_pgm_unit ? 
                  Complex_Function_Arg : Complex_Arg);
      break;

   case CRI_Ptr_8:
      arg_type = (is_pgm_unit ? 
                  Pointer_Function_Arg : Pointer_Arg);
      break;

   case Logical_1:
   case Logical_2:
   case Logical_4:
   case Logical_8:
      arg_type = (is_pgm_unit ? 
                  Logical_Function_Arg : Logical_Arg);
      break;

   case Character_1:
   case Character_2:
   case Character_4:
   case Short_Char_Const:
      arg_type = (is_pgm_unit ? 
                  Character_Function_Arg : Character_Arg);
      break;

   case CRI_Ch_Ptr_8:
      arg_type = (is_pgm_unit ? 
                  Character_Pointer_Function_Arg : Character_Pointer_Arg);
      break;

   case Structure_Type:
      arg_type = (is_pgm_unit ? 
                  Derived_Type_Function_Arg : Derived_Type_Arg);
      break;

   case CRI_Parcel_Ptr_8:
      arg_type = (is_pgm_unit ? 
                  Pointer_Function_Arg : Pointer_Arg);
      break;

   default :
      PRINTMSG(stmt_start_line, 626, Internal, 1,
               "valid type",
               "get_arg_type");
      break;
   }


   TRACE (Func_Exit, "get_arg_type", NULL);

   return(arg_type);

}  /* get_arg_type */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	create the table that describes a derived type for runtime argument   *|
|*      checking.                                                             *|
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

static int create_struct_argchck_tbl(int		dt_idx)

{
   int		asg_idx;
   int		cn_idx;
   int		i;
   int		list_idx;
   int		sub_idx;
   int		tmp_idx;
   int		total_word_cnt;
   int		type_idx;


   TRACE (Func_Entry, "create_struct_argchck_tbl", NULL);

   /* first we must calculate the size of the table */

   total_word_cnt = 2 + determine_struct_tbl_size(dt_idx);

   /* get the temp */

   tmp_idx = gen_static_integer_array_tmp(total_word_cnt,
					  stmt_start_line,
                                          stmt_start_col);

   /* get the constant table entry */

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
   TYP_BIT_LEN(TYP_WORK_IDX)	= total_word_cnt * TARGET_BITS_PER_WORD;
   type_idx			= ntr_type_tbl();

   cn_idx = ntr_const_tbl(type_idx, FALSE, NULL);

   /* set up the the data initialization */

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(sub_idx) = stmt_start_line;
   IR_COL_NUM(sub_idx)  = stmt_start_col;
   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_IDX_L(sub_idx) = tmp_idx;
   IR_LINE_NUM_L(sub_idx) = stmt_start_line;
   IR_COL_NUM_L(sub_idx)  = stmt_start_col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_IDX_R(sub_idx) = list_idx;
   IR_LIST_CNT_R(sub_idx) = 1;
   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx) = stmt_start_col;


   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Init_Opr;
   IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = stmt_start_line;
   IR_COL_NUM(asg_idx)  = stmt_start_col;

   IR_FLD_L(asg_idx) = IR_Tbl_Idx;
   IR_IDX_L(asg_idx) = sub_idx;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_IDX_R(asg_idx) = list_idx;
   IR_LIST_CNT_R(asg_idx) = 3;

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = cn_idx;
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx) = stmt_start_col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx) = stmt_start_col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx) = stmt_start_col;

   gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col, 
          FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /****************************\
   |* now fill in the constant *|
   \****************************/

   i = 0;

   /* BRIANJ - Do we need to convert total_word_cnt to target? */

   CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) = total_word_cnt;
   i++;

   if (cmd_line_flags.dalign) {
      CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) = TRUE;
   }
   i++;

   fill_in_struct_argchck_const(dt_idx, cn_idx, &i);

# ifdef _DEBUG
   if (i != total_word_cnt) {
      printf("i = %d, total_word_cnt = %d \n", i, total_word_cnt);
      PRINTMSG(stmt_start_line, 1117, Internal, stmt_start_col);
   }
# endif

   TRACE (Func_Exit, "create_struct_argchck_tbl", NULL);

   return(tmp_idx);

}  /* create_struct_argchck_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine determines the number of words needed for a particular   *|
|*      derived type in an argchck derived type table.                        *|
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

static int determine_struct_tbl_size(int	dt_idx)

{
   int		i;
   int		size = 0;
   int		sn_idx;

   TRACE (Func_Entry, "determine_struct_tbl_size", NULL);

   /* add size of derived type name */

   size += TARGET_BYTES_TO_WORDS(1 + AT_NAME_LEN(dt_idx));

   /* add word for number of cpnts */

   size += 1;

   sn_idx = ATT_FIRST_CPNT_IDX(dt_idx);

   for (i = 0; i < ATT_NUM_CPNTS(dt_idx); i++) {

      /* add size of component name */

      size += TARGET_BYTES_TO_WORDS(1 + AT_NAME_LEN(SN_ATTR_IDX(sn_idx)));

      /* add a word for arg_type, kind type, rank, and pointer flags */

      size += 1;

      /* add two words for each dim */

      if (ATD_ARRAY_IDX(SN_ATTR_IDX(sn_idx)) != NULL_IDX &&
          BD_ARRAY_CLASS(ATD_ARRAY_IDX(SN_ATTR_IDX(sn_idx))) == 
                                                           Explicit_Shape) {

         size += 2 * BD_RANK(ATD_ARRAY_IDX(SN_ATTR_IDX(sn_idx)));
      }

      if (TYP_TYPE(ATD_TYPE_IDX(SN_ATTR_IDX(sn_idx))) == Character) {
         /* add one word for character length */
         size += 1;
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(SN_ATTR_IDX(sn_idx))) == Structure &&
               !ATD_POINTER(SN_ATTR_IDX(sn_idx))) {
         size += determine_struct_tbl_size(TYP_IDX(ATD_TYPE_IDX(
                                                    SN_ATTR_IDX(sn_idx))));
      }
     
      sn_idx = SN_SIBLING_LINK(sn_idx);
   }

   TRACE (Func_Exit, "determine_struct_tbl_size", NULL);

   return(size);

}  /* determine_struct_tbl_size */

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

static void fill_in_struct_argchck_const(int		dt_idx,
					 int		cn_idx,
					 int		*idx)

{
   int		attr_idx;
   char		*char_ptr;
   int		i;
   int		k;
   int		l;
   int		sn_idx;
   long_type	the_constant[MAX_WORDS_FOR_INTEGER];


   TRACE (Func_Entry, "fill_in_struct_argchck_const", NULL);

   i = *idx;

   /* fill in name length and name */

   CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) =   /* BRIANJ */
      ((long_type)(AT_NAME_LEN(dt_idx)) << (TARGET_BITS_PER_WORD - 8));

   char_ptr = (char *)&(CP_CONSTANT(CN_POOL_IDX(cn_idx) + i));

   char_ptr++;

   strncpy(char_ptr, AT_OBJ_NAME_PTR(dt_idx), AT_NAME_LEN(dt_idx));

   i += TARGET_BYTES_TO_WORDS(1 + AT_NAME_LEN(dt_idx));

   C_TO_F_INT(the_constant, ATT_NUM_CPNTS(dt_idx), CG_INTEGER_DEFAULT_TYPE);

   CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) = the_constant[0];

   i++;

   sn_idx = ATT_FIRST_CPNT_IDX(dt_idx);

   for (k = 0; k < ATT_NUM_CPNTS(dt_idx); k++) {
      attr_idx = SN_ATTR_IDX(sn_idx);

      /* BRIANJ */

      CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) = 
         ((long_type)(AT_NAME_LEN(attr_idx)) << (TARGET_BITS_PER_WORD - 8));

      char_ptr = (char *)&(CP_CONSTANT(CN_POOL_IDX(cn_idx) + i));
      char_ptr++;
      strncpy(char_ptr, AT_OBJ_NAME_PTR(attr_idx), AT_NAME_LEN(attr_idx));
      i += TARGET_BYTES_TO_WORDS(1 + AT_NAME_LEN(attr_idx));

      CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) |= 
             (get_arg_type(ATD_TYPE_IDX(attr_idx), FALSE) << 24);

      CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) |=
             (linear_to_kind_type[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))] << 16);

      if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
         CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) |=
             (BD_RANK(ATD_ARRAY_IDX(attr_idx)) << 8);
      }

      CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) |= ATD_POINTER(attr_idx);
         
      i++;

      /* BRIANJ */

      if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
          BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Explicit_Shape) {

         for (l = 1; l <= BD_RANK(ATD_ARRAY_IDX(attr_idx)); l++) {
            CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) = 
                                CN_CONST(BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), l));
            i++;
            CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) = 
                                CN_CONST(BD_XT_IDX(ATD_ARRAY_IDX(attr_idx), l));
            i++;
         }
      }

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
         /* fill in character length */
         CP_CONSTANT(CN_POOL_IDX(cn_idx) + i) = 
                                CN_CONST(TYP_IDX(ATD_TYPE_IDX(attr_idx)));
         i++;  
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
               !ATD_POINTER(attr_idx)) {
         fill_in_struct_argchck_const(TYP_IDX(ATD_TYPE_IDX(attr_idx)), 
                                      cn_idx, &i);
      }

      sn_idx = SN_SIBLING_LINK(sn_idx);
   }
   
   *idx = i;

   TRACE (Func_Exit, "fill_in_struct_argchck_const", NULL);

   return;

}  /* fill_in_struct_argchck_const */

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

void gen_dbg_write_stmt(opnd_type	*opnd,
			sh_position_type position)

{
   int			i;
   int			ir_idx;
   int			list_idx;


   TRACE (Func_Entry, "gen_dbg_write_stmt", NULL);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Write_Formatted_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = stmt_start_line;
   IR_COL_NUM(ir_idx)  = stmt_start_col;
   

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_L(ir_idx) = 10;
   IR_IDX_L(ir_idx) = list_idx;

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx)  = stmt_start_col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx)  = stmt_start_col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = CN_INTEGER_THREE_IDX;
   IL_LINE_NUM(list_idx) = stmt_start_line;
   IL_COL_NUM(list_idx)  = stmt_start_col;

   for (i = 0; i < 7; i++) {
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_LINE_NUM(list_idx) = stmt_start_line;
      IL_COL_NUM(list_idx)  = stmt_start_col;
   }

   if (OPND_FLD((*opnd)) == IL_Tbl_Idx) {
      COPY_OPND(IR_OPND_R(ir_idx), (*opnd));
   }
   else {
      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(ir_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_R(ir_idx) = 1;
      IR_IDX_R(ir_idx) = list_idx;

      COPY_OPND(IL_OPND(list_idx), (*opnd));
   }

   gen_sh(position, Assignment_Stmt, stmt_start_line, stmt_start_col, 
          FALSE, FALSE, TRUE);

   if (position == After) {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }
   else {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }

   TRACE (Func_Exit, "gen_dbg_write_stmt", NULL);

   return;

}  /* gen_dbg_write_stmt */

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

static void make_base_assumed_size(opnd_type	*old_opnd,
				   opnd_type	*new_opnd)

{
   int		attr_idx;
   int		bd_idx;
   int		col;
   int		i;
   int		line;
   int		list_idx;
   int		sub_idx;

   TRACE (Func_Entry, "make_base_assumed_size", NULL);

   find_opnd_line_and_column(old_opnd, &line, &col);

# ifdef _DEBUG
   if (OPND_FLD((*old_opnd)) != AT_Tbl_Idx) {
      PRINTMSG(line, 626, Internal, col, 
               "AT_Tbl_Idx", "make_base_assumed_size");
   }
# endif

   attr_idx = OPND_IDX((*old_opnd));
   bd_idx   = ATD_ARRAY_IDX(attr_idx);

# ifdef _DEBUG
   if (bd_idx == NULL_IDX ||
       BD_ARRAY_CLASS(bd_idx) != Assumed_Size) {

      PRINTMSG(line, 626, Internal, col, 
               "Assumed Size array", "make_base_assumed_size");
   }
# endif

   NTR_IR_TBL(sub_idx);
   IR_OPR(sub_idx) = Subscript_Opr;
   IR_TYPE_IDX(sub_idx) = ATD_TYPE_IDX(attr_idx);
   IR_LINE_NUM(sub_idx) = line;
   IR_COL_NUM(sub_idx) = col;
   IR_IDX_L(sub_idx) = attr_idx;
   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_LINE_NUM_L(sub_idx) = line;
   IR_COL_NUM_L(sub_idx) = col;

   NTR_IR_LIST_TBL(list_idx);
   IR_IDX_R(sub_idx) = list_idx;
   IR_FLD_R(sub_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_R(sub_idx) = BD_RANK(bd_idx);

   IL_FLD(list_idx) = BD_LB_FLD(bd_idx, 1);
   IL_IDX(list_idx) = BD_LB_IDX(bd_idx, 1);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx) = col;

   for (i = 2; i <= BD_RANK(bd_idx); i++) {
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_IDX(list_idx) = BD_LB_IDX(bd_idx, i);
      IL_FLD(list_idx) = BD_LB_FLD(bd_idx, i);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = col;
   }

   OPND_FLD((*new_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*new_opnd)) = sub_idx;

   TRACE (Func_Exit, "make_base_assumed_size", NULL);

   return;

}  /* make_base_assumed_size */

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

static void check_call_for_global_def(int	list_idx,
				      int	spec_idx,
				      int	num_dargs)

{
   uint			act_file_line;
   int			ga_idx;
   int			gl_idx;
   char			line_name[256];
   int			name_idx;
   int			new_ga_idx;


   TRACE (Func_Entry, "check_call_for_global_def", NULL);

   if (ATP_PROC(spec_idx) == Dummy_Proc || ATP_NAME_IN_STONE(spec_idx)) {

      /* Exit - this is a dummy procedure which makes it a local name or */
      /*        this name is specified in the !DIR$ NAME directive which */
      /*        also makes it a local name for the external name.        */

      return;
   }

   ga_idx = NULL_IDX;

   /* ATP_EXPL_ITRFC should be FALSE to be here. */

   /* There is no explicit interface for this program unit.  Check */
   /* the global name table to see if an explicit interface was    */
   /* found in another program unit.  If so, do semantics checks,  */
   /* if not, add this to the global name table as a reference in  */
   /* the hope that it can be checked later.                       */
      
   if (srch_global_name_tbl(AT_OBJ_NAME_PTR(spec_idx),
                            AT_NAME_LEN(spec_idx),
                            &name_idx)) {

      if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
         ga_idx	= GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx));
      }
      else {

         if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Pgm_Unit) {
            ga_idx = GN_ATTR_IDX(name_idx);
         }

# if defined(_DEBUG)

         /* ga_idx should always be valid in this else clause. */

         if (ga_idx == NULL_IDX) { 
            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                    "valid attr_idx",
                    "check_call_for_global_def");
         }
# endif
      }

      if (ga_idx == NULL_IDX) { 
         ga_idx = ntr_global_attr_tbl(spec_idx, name_idx);

         if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
            GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx)) = ga_idx;
         }
         else {

            /* Only get here if non-debug and assumption  */
            /* fails.  Fill in GN_ATTR_IDX for recovery.  */

            GN_ATTR_IDX(name_idx) = ga_idx;
         }

         ntr_ref_in_global_tbl(list_idx, 
                               spec_idx, 
                               num_dargs,
                              &ga_idx,
                               name_idx);
      }
      else if (GAP_NEEDS_EXPL_ITRFC(ga_idx)) {
         line_name[0]   = '\0';
         GLOBAL_LINE_TO_FILE_LINE(GA_DEF_LINE(ga_idx), gl_idx, act_file_line);

         sprintf(line_name, "%d (%s)", act_file_line, GL_FILE_NAME_PTR(gl_idx));
         PRINTMSG(stmt_start_line, 1277, Error, stmt_start_col,
                  AT_OBJ_NAME_PTR(spec_idx),
                  "defined",
                  line_name);
      }
      else if (!GA_DEFINED(ga_idx)) {

         /* The program unit being called has not been compiled yet,  */
         /* and no interface block has been found.  Call global_name_ */
         /* semantics to do a few ref to ref checks, and then add the */
         /* reference to the list in case a definition is found.      */

         /* In this case, ntr_ref just hangs the new ref off the older one. */

         global_name_semantics(ga_idx, NULL_IDX, list_idx, spec_idx, NULL_IDX);

         new_ga_idx = ntr_global_attr_tbl(spec_idx, name_idx);
         GAP_NEXT_PGM_UNIT_IDX(new_ga_idx) = ga_idx;

         if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
            GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx)) = new_ga_idx;
         }
         else {
            GN_ATTR_IDX(name_idx) = new_ga_idx;
         }
         ga_idx = new_ga_idx;
         
         ntr_ref_in_global_tbl(list_idx, 
                               spec_idx, 
                               num_dargs,
                              &ga_idx,
                               name_idx);
      }
      else {  /* True DEF to REF call */
         global_name_semantics(ga_idx, NULL_IDX, list_idx, spec_idx, NULL_IDX);
      }
   }
   else {
      ga_idx	= NULL_IDX;
      ntr_ref_in_global_tbl(list_idx, 
                            spec_idx, 
                            num_dargs,
                           &ga_idx,
                            name_idx);
   }


   TRACE (Func_Exit, "check_call_for_global_def", NULL);

   return;

}  /* check_call_for_global_def */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Enters a procedure reference into the global attr table.              *|
|*	This makes use of the utility routines ntr_global_name_table          *|
|*	and fill_in_global_attr_ntry.  The actual arguments must be entered   *|
|*	differently than the dargs are, so it is done in this routine.        *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx  -> IL index to list of actual arguments.                    *|
|*	spec_idx  -> Attr index to name of procedure being called.            *|
|*	num_dargs -> The number of dummy arguments.                           *|
|*	ga_idx    -> The global attr to be filled in.  If NULL,               *|
|*                   ntr_global_name_tbl is called to create a new entry.     *|
|*	name_idx  -> Valid name index in the global name table.               *|
|*	             If ga_idx is NULL_IDX, this should be the value returned *|
|*	             from the last call to srch_global_name_tbl.              *|
|*									      *|
|* Output parameters:							      *|
|*	ga_idx    -> The global attr updated or created by this call.         *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static	void	ntr_ref_in_global_tbl(int	 list_idx,
				      int	 spec_idx,
				      int	 num_dargs,
				      int	*ga_idx,
				      int	 name_idx)

{
   static	int		 arg_name_idx	= NULL_IDX;

		int		 arg_idx;
		id_str_type	 darg_name;
		int		 i;
		int		 il_idx;
		int		 info_idx;
		int		 kwd_idx;
		long_type	*name_id;
		int		 next_il_idx;
		int		 rslt_idx;
   		int	 	 str_idx;
   		int	 	 type_idx;
		int		 word_len;



   TRACE (Func_Entry, "ntr_ref_in_global_tbl", NULL);

   if (*ga_idx == NULL_IDX) {

      /* If *ga_idx is NULL we do not have an attr.  Create one.  */
      /* Once we have an attr.  Then it needs to be filled in.    */
      /* If *ga_idx is not NULL, the caller must have created the */
      /* attr and linked it to its name.                          */

      ntr_global_name_tbl(spec_idx, NULL_IDX, name_idx);
      *ga_idx = GN_ATTR_IDX(name_idx);
   }

   fill_in_global_attr_ntry(*ga_idx, spec_idx, NULL_IDX);

   GA_REFERENCED(*ga_idx) = TRUE;

   if (num_dargs == 0) {
      goto DONE;
   }

   /* Add actual arguments to reference. */

   GAP_FIRST_IDX(*ga_idx) = global_attr_tbl_idx + 1;
   GAP_NUM_DARGS(*ga_idx) = num_dargs;

   /* Reserve space for the dummy arguments so they are in */
   /* consecutive order.  Then return and fill them in.    */

   next_il_idx	= list_idx;
   rslt_idx	= ATP_RSLT_IDX(spec_idx);

#ifdef KEY /* Bug 5089 */
   boolean fcn_to_sub = (rslt_idx != NULL_IDX &&
     (FUNCTION_MUST_BE_SUBROUTINE(spec_idx, rslt_idx)));
   if (fcn_to_sub)
#else /* KEY Bug 5089 */
   if (rslt_idx != NULL_IDX && FUNCTION_MUST_BE_SUBROUTINE(rslt_idx))
#endif /* KEY Bug 5089 */
   {
      next_il_idx	= IL_NEXT_LIST_IDX(next_il_idx);
   }

   for (i = 0; i < num_dargs; i++ ) {
      il_idx			= next_il_idx;
      info_idx			= IL_ARG_DESC_IDX(il_idx);
# ifdef _DEBUG
      if (info_idx == NULL_IDX) {
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "valid info_idx", "ntr_ref_in_global_tbl");
      }
# endif

      next_il_idx		= IL_NEXT_LIST_IDX(il_idx);

      TBL_REALLOC_CK(global_attr_tbl, 1);
      CLEAR_TBL_NTRY(global_attr_tbl, global_attr_tbl_idx);

      arg_idx			= global_attr_tbl_idx;
      GA_DEF_LINE(arg_idx)	= AT_DEF_LINE(spec_idx);
      GA_DEF_COLUMN(arg_idx)	= AT_DEF_COLUMN(spec_idx);

      if (info_idx != NULL_IDX && arg_info_list[info_idx].kwd != NULL_IDX) {

         /* Have a keyword.  Save.  Right now it is a CN_Tbl_Idx */

         kwd_idx		= arg_info_list[info_idx].kwd;
         GA_NAME_IDX(arg_idx)	= str_pool_idx + 1;
         GA_NAME_LEN(arg_idx)	= (uint) CN_INT_TO_C(TYP_IDX(
                                                     CN_TYPE_IDX(kwd_idx))); 
         name_id		= &(CN_CONST(kwd_idx));
         word_len		= WORD_LEN(GN_NAME_LEN(arg_idx)) + EXTRA_WORD;

         TBL_REALLOC_CK(str_pool, word_len);

         for (str_idx = 0; str_idx < word_len; str_idx++) {
            str_pool[GA_NAME_IDX(arg_idx)+str_idx].name_long = name_id[str_idx];
         }
      }
      else { /* No keyword, so use _darg as a placeholder name. */

         if (arg_name_idx == NULL_IDX) {
            CREATE_ID(darg_name, "_darg", 5);
            arg_name_idx	= str_pool_idx + 1;
            name_id		= (long_type *) &(darg_name.words[0]); 
            word_len		= WORD_LEN(5) + EXTRA_WORD;

            TBL_REALLOC_CK(str_pool, word_len);

            for (str_idx = 0; str_idx < word_len; str_idx++) {
               str_pool[arg_name_idx + str_idx].name_long = name_id[str_idx];
            }
         }
         GA_NAME_IDX(arg_idx)	= arg_name_idx;
         GA_NAME_LEN(arg_idx)	= 5;
      }
      GA_ORIG_NAME_IDX(arg_idx)	= GA_NAME_IDX(arg_idx);
      GA_ORIG_NAME_LEN(arg_idx)	= GA_NAME_LEN(arg_idx);
   }

   next_il_idx	= list_idx;

#ifdef KEY /* Bug 5089 */
   if (rslt_idx != NULL_IDX && fcn_to_sub)
#else /* KEY Bug 5089 */
   if (rslt_idx != NULL_IDX && FUNCTION_MUST_BE_SUBROUTINE(rslt_idx))
#endif /* KEY Bug 5089 */
   {
      next_il_idx	= IL_NEXT_LIST_IDX(next_il_idx);
   }

   arg_idx	= GAP_FIRST_IDX(*ga_idx);

   arg_idx--;   /* Preset so we can add one at beginning of loop. */

   while (next_il_idx != NULL_IDX) {
      il_idx			= next_il_idx;
      info_idx			= IL_ARG_DESC_IDX(il_idx);
      next_il_idx		= IL_NEXT_LIST_IDX(il_idx);

      arg_idx++;
   
      if (arg_info_list[info_idx].pgm_unit) { /* Actual is a procedure. */

         /* Enhance note - Should we look for this name in the global table? */

         GA_OBJ_CLASS(arg_idx)	= Pgm_Unit;

         /* Should we pick up info from IL_IDX(il_idx) */
      }
      else {  /* KAY */

      /* Play with this in the debugger.  What if this is an expression? */
      /* What if it is a parameter?  What about intent checking?         */

         if (IL_FLD(il_idx) == CN_Tbl_Idx) {  /* Constant actual arg */
            GA_OBJ_CLASS(arg_idx)	= Data_Obj;
            GAD_CLASS(arg_idx)		= Constant;
            /* 28Dec00[sos]:  Wrong variable being referenced! (PV 810455)        */
            /* was: GAD_HOLLERITH(arg_idx) = CN_HOLLERITH_TYPE(IL_IDX(list_idx)); */
            GAD_HOLLERITH(arg_idx) = CN_HOLLERITH_TYPE(IL_IDX(il_idx));
         }
         else if (IL_FLD(il_idx) == AT_Tbl_Idx) {
            GA_OBJ_CLASS(arg_idx)	= AT_OBJ_CLASS(IL_IDX(il_idx));

            if (AT_OBJ_CLASS(IL_IDX(il_idx)) == Data_Obj) {
               GAD_CLASS(arg_idx)		= ATD_CLASS(IL_IDX(il_idx));
            }
         }
         else {  /* Will this happen */
            GA_OBJ_CLASS(arg_idx)	= Data_Obj;
            GAD_CLASS(arg_idx)		= Dummy_Argument;
         }

         if (GA_OBJ_CLASS(arg_idx) == Data_Obj) {
            GAD_RANK(arg_idx)		  = arg_info_list[info_idx].ed.rank;
            GAD_ARRAY_ELEMENT_REF(arg_idx)=arg_info_list[info_idx].ed.array_elt;

            /* We do not need to carry anymore array info for a reference. */

            if (arg_info_list[info_idx].ed.type_idx != NULL_IDX) {

               /* Enter the argument's type. */

               type_idx = 
                       ntr_global_type_tbl(arg_info_list[info_idx].ed.type_idx);
               GAD_TYPE_IDX(arg_idx)	= type_idx;
            }
            else { /* Create a type from the info in the arg_info_list */
               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);	
               TYP_TYPE(TYP_WORK_IDX)   =arg_info_list[info_idx].ed.type;
               TYP_LINEAR(TYP_WORK_IDX) =arg_info_list[info_idx].ed.linear_type;

               if (TYP_TYPE(TYP_WORK_IDX) == Character) {
                  TYP_FLD(TYP_WORK_IDX) = OPND_FLD(
                                          arg_info_list[info_idx].ed.char_len);
                  TYP_IDX(TYP_WORK_IDX) = OPND_IDX(
                                          arg_info_list[info_idx].ed.char_len);
               }
            }
         }
      }
   }  /* End actual argument while */

DONE:

   TRACE (Func_Exit, "ntr_ref_in_global_tbl", NULL);

   return;

}  /* ntr_ref_in_global_tbl */

#ifdef KEY /* Bug 11046 */
/*
 * start_il_idx		IL_Tbl_Idx for actual argument list
 * returns TRUE if any actual argument is an array
 */
static boolean has_array_arg(int start_il_idx)
{
   boolean result = FALSE;
   for (int list_idx = start_il_idx; list_idx && !result;
      list_idx = IL_NEXT_LIST_IDX(list_idx)) {
      int info_idx = IL_ARG_DESC_IDX(list_idx);
      expr_arg_type exp_desc = arg_info_list[info_idx].ed;
      result = (exp_desc.rank > 0);
   }
   return result;
}
#endif /* KEY Bug 11046 */
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

static boolean check_elemental_conformance(int			start_il_idx,
					   expr_arg_type	*res_exp_desc,
#ifdef KEY /* Bug 11046 */
					   int			spec_idx
#endif /* KEY Bug 11046 */
					   )

{
   int			array_info_idx = NULL_IDX;
   int			col;
   expr_arg_type	exp_desc;
   int			i;
   int			info_idx;
   int			line;
   int			list_idx;
   boolean		ok = TRUE;

   TRACE (Func_Entry, "check_elemental_conformance", NULL);

#ifdef KEY /* Bug 11046 */
   boolean subroutine_and_array_args = (AT_OBJ_CLASS(spec_idx) == Pgm_Unit &&
      ATP_PGM_UNIT(spec_idx) == Subroutine && has_array_arg(start_il_idx));
   int dummy_cnt = !!ATP_EXTRA_DARG(spec_idx);
#endif /* KEY Bug 11046 */

   list_idx = start_il_idx;

   while (list_idx && ok) {
      info_idx = IL_ARG_DESC_IDX(list_idx);

      if (info_idx == NULL_IDX) {
#ifdef KEY /* Bug 8435 */
         /* Perfectly correct to see info_idx == NULL_IDX when an optional
	  * argument has been omitted. Erroneous omission of a non-optional
	  * argument is detected elsewhere. */
	 list_idx = IL_NEXT_LIST_IDX(list_idx);
	 continue;
#else /* KEY Bug 8435 */
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "valid info_idx", "check_elemental_conformance");
#endif /* KEY Bug 8435 */
      }

      exp_desc = arg_info_list[info_idx].ed;

#ifdef KEY /* Bug 11046 */
      /*
       * Rule for subroutine is stricter than for function: if any arg is
       * an array, then all intent(out) and intent(inout) args must be arrays.
       *
       * Beyond that, rule is same as for function: all args must merely be
       * conformable (either scalars, or arrays having the same shape.)
       */
      if (subroutine_and_array_args && exp_desc.rank == 0) {
	 int dummy_idx = SN_ATTR_IDX(ATP_FIRST_IDX(spec_idx) + dummy_cnt);
	 intent_type intent = ATD_INTENT(dummy_idx);
	 if (intent == Intent_Inout || intent == Intent_Out) {
	    find_opnd_line_and_column(&IL_OPND(list_idx), &line, &col);
	    PRINTMSG(line, 1311, Error, col);
	    ok = FALSE;
	 }
      }
      dummy_cnt += 1;
#endif /* KEY Bug 11046 */

      if (exp_desc.rank > 0) {
         if (array_info_idx == NULL_IDX) {
            array_info_idx = info_idx;
            res_exp_desc->rank = exp_desc.rank;
            COPY_SHAPE(res_exp_desc->shape, exp_desc.shape, exp_desc.rank);
         }
         else {
            if (exp_desc.rank != res_exp_desc->rank) {
               /* error, rank does not conform */
               find_opnd_line_and_column(&IL_OPND(list_idx),
                                         &line,
                                         &col);
               PRINTMSG(line, 1311, Error, col);
               ok = FALSE;
            }
            else {
               for (i = 0; i < exp_desc.rank; i++) {

                  if (OPND_FLD(res_exp_desc->shape[i]) == CN_Tbl_Idx &&
                      OPND_FLD(exp_desc.shape[i]) == CN_Tbl_Idx) {

                     if (fold_relationals(OPND_IDX(res_exp_desc->shape[i]),
                                          OPND_IDX(exp_desc.shape[i]),
                                          Ne_Opr)) {

                        find_opnd_line_and_column(&IL_OPND(list_idx),
                                                  &line,
                                                  &col);
                        PRINTMSG(line, 1311, Error, col);
                        ok = FALSE;
                        break;
                     }
                  }
                  else if (SHAPE_FOLDABLE(exp_desc.shape[i])) {
                     COPY_OPND(res_exp_desc->shape[i], exp_desc.shape[i]);
                  }
               }
            }
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "check_elemental_conformance", NULL);

   return(ok);

}  /* check_elemental_conformance */

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

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
static void set_inline_state(int	ir_idx,
			     int	attr_idx)

{
   int		ga_idx = NULL_IDX;
   int		name_idx;

   TRACE (Func_Entry, "set_inline_state", NULL);

   if (srch_global_name_tbl(AT_OBJ_NAME_PTR(attr_idx),
                            AT_NAME_LEN(attr_idx),
                            &name_idx)) {

      ga_idx = GN_ATTR_IDX(name_idx);
   }

   if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit ||
       IR_INLINE_STATE(ir_idx) != Not_Specified_Sgi) {
      /* intentionally blank */
   }
   else if (ATP_SGI_LOCAL_INLINE(attr_idx)) {
      IR_INLINE_STATE(ir_idx) = Inline_Sgi;
   }
   else if (ATP_SGI_LOCAL_NOINLINE(attr_idx)) {
      IR_INLINE_STATE(ir_idx) = Noinline_Sgi;
   }
   else if (cdir_switches.inline_here_sgi) {
      IR_INLINE_STATE(ir_idx) = Inline_Sgi;
   }
   else if (cdir_switches.noinline_here_sgi) {
      IR_INLINE_STATE(ir_idx) = Noinline_Sgi;
   }
   else if (ATP_SGI_ROUTINE_INLINE(attr_idx)) {
      IR_INLINE_STATE(ir_idx) = Inline_Sgi;
   }
   else if (ATP_SGI_ROUTINE_NOINLINE(attr_idx)) {
      IR_INLINE_STATE(ir_idx) = Noinline_Sgi;
   }
   else if (SCP_INLINE_SGI(curr_scp_idx)) {
      IR_INLINE_STATE(ir_idx) = Inline_Sgi;
   }
   else if (SCP_NOINLINE_SGI(curr_scp_idx)) {
      IR_INLINE_STATE(ir_idx) = Noinline_Sgi;
   }
   else if (ATP_SGI_GLOBAL_INLINE(attr_idx)) {
      IR_INLINE_STATE(ir_idx) = Inline_Sgi;
   }
   else if (ATP_SGI_GLOBAL_NOINLINE(attr_idx)) {
      IR_INLINE_STATE(ir_idx) = Noinline_Sgi;
   }
   else if (ga_idx != NULL_IDX &&
            GA_OBJ_CLASS(ga_idx) == Pgm_Unit &&
            GAP_INLINE_STATE(ga_idx) == Inline_Sgi) {
      IR_INLINE_STATE(ir_idx) = Inline_Sgi;
   }
   else if (ga_idx != NULL_IDX &&
            GA_OBJ_CLASS(ga_idx) == Pgm_Unit &&
            GAP_INLINE_STATE(ga_idx) == Noinline_Sgi) {
      IR_INLINE_STATE(ir_idx) = Noinline_Sgi;
   }
   else if (inline_global_sgi) {
      IR_INLINE_STATE(ir_idx) = Inline_Sgi;
   }
   else if (noinline_global_sgi) {
      IR_INLINE_STATE(ir_idx) = Noinline_Sgi;
   }


   TRACE (Func_Exit, "set_inline_state", NULL);

   return;

}  /* set_inline_state */
# endif

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

static void check_for_constructors(opnd_type		*top_opnd,
				   expr_arg_type	*exp_desc)

{
   int			ir_idx;
   int			list_idx;
   expr_arg_type	loc_exp_desc;
   opnd_type		opnd;

   TRACE (Func_Entry, "check_for_constructors", NULL);

   switch (OPND_FLD((*top_opnd))) {
   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*top_opnd));

      switch (IR_OPR(ir_idx)) {
      case Stmt_Expansion_Opr:
      case Call_Opr:
         break;

      case Array_Construct_Opr :

         loc_exp_desc = arg_info_list[IR_IDX_L(ir_idx)].ed;
         create_runtime_array_constructor(top_opnd, &loc_exp_desc);
         if (exp_desc != NULL) {
            COPY_SHAPE((exp_desc->shape),
                       loc_exp_desc.shape, loc_exp_desc.rank);
         }

         break;

      default:
         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         check_for_constructors(&opnd, exp_desc);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         COPY_OPND(opnd, IR_OPND_R(ir_idx));
         check_for_constructors(&opnd, exp_desc);
         COPY_OPND(IR_OPND_R(ir_idx), opnd);
         break;
      }
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*top_opnd));
      while (list_idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         check_for_constructors(&opnd, exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;
   }


   TRACE (Func_Exit, "check_for_constructors", NULL);

   return;

}  /* check_for_constructors */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compare an actual arg with its corresponding darg.                    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      arg_attr    -                                                         *|
|*      info_idx    -                                                         *|
|*      spec_count  -                                                         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*	NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if they are the same.                                            *|
|*                                                                            *|
\******************************************************************************/
static boolean compare_darg_to_actual_arg(int		gen_idx,
					  int		spec_idx,
					  int		arg_attr,
					  opnd_type	list_opnd,
					  int		info_idx,
					  int		spec_count)
/* arg_attr is the darg */

{
   int			a_linear_type;
   int			a_type;
   int			a_type_idx;
   int			aa_rank;
   int			attr_idx;
   int			da_rank;
   expr_arg_type	exp_desc;
   int			idx;
   opnd_type		opnd;
   int			opnd_line;
   int			opnd_column;
   boolean		pgm_unit;
   boolean		pointer;
   boolean		same		= TRUE;
   int			type_idx;
   char			type_word1[40];
   char			type_word2[40];


   TRACE (Func_Entry, "compare_darg_to_actual_arg", NULL);

# if defined(_DEBUG)

   if (info_idx == NULL_IDX && OPND_FLD(list_opnd) != AT_Tbl_Idx) {
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "IL_FLD to be AT_Tbl_Idx",
               "compare_darg_to_actual_arg");
   }
# endif

   find_opnd_line_and_column((opnd_type *) &list_opnd, &opnd_line,
                             &opnd_column);

   attr_idx	= OPND_IDX(list_opnd);  /* Won't always be an attr */

   if (info_idx == NULL_IDX) {
      pgm_unit	= (AT_OBJ_CLASS(attr_idx) == Pgm_Unit);
   }
   else {
      pgm_unit	= arg_info_list[info_idx].pgm_unit;
   }

   switch (AT_OBJ_CLASS(arg_attr)) {
   case Data_Obj:
#ifdef KEY
      if (pgm_unit && strcmp(AT_OBJ_NAME_PTR(gen_idx), "SIGNAL") != 0)
#else
      if (pgm_unit)
#endif
      {
         same = FALSE;
         
         if (spec_count == 0) { /* error .. expecting data obj */
            PRINTMSG(opnd_line, 503, Error, opnd_column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     "PROGRAM UNIT",
                     AT_OBJ_NAME_PTR(arg_attr),
                     "DATA OBJECT");
         }
         break;
      }

      if (ATD_IGNORE_TKR(arg_attr)) {

         /* This dummy arg will match any rank, so skip the rank */
         /* type checking below.  -  intentionally blank         */

      }
      else if (OPND_FLD(list_opnd) == IR_Tbl_Idx &&
               IR_OPR(OPND_IDX(list_opnd)) == Null_Intrinsic_Opr) {

         /* intentionally blank */
         /* Don't know type or rank yet, they come from darg */
      }
#ifdef KEY
      else if (!((strcmp(AT_OBJ_NAME_PTR(gen_idx), "EOSHIFT") == 0) &&
               (strcmp(AT_OBJ_NAME_PTR(arg_attr), "BOUNDARY") == 0)) && 
                strcmp(AT_OBJ_NAME_PTR(gen_idx), "SIGNAL") != 0)
#else
      else if (!((strcmp(AT_OBJ_NAME_PTR(gen_idx), "EOSHIFT") == 0) &&
               (strcmp(AT_OBJ_NAME_PTR(arg_attr), "BOUNDARY") == 0)))
#endif
      {

         if (!(strcmp(AT_OBJ_NAME_PTR(gen_idx), "RESHAPE") == 0) ||
               ((strcmp(AT_OBJ_NAME_PTR(arg_attr), "PAD") != 0) &&
                (strcmp(AT_OBJ_NAME_PTR(arg_attr), "ORDER") != 0))) {

            aa_rank = 0;
            da_rank = (ATD_ARRAY_IDX(arg_attr) ?
                       BD_RANK(ATD_ARRAY_IDX(arg_attr)) : 0);

            if (info_idx == NULL_IDX) {
               pointer = ATD_POINTER(attr_idx);

               if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
                  aa_rank = BD_RANK(ATD_ARRAY_IDX(attr_idx));
               }
            }
            else {
               aa_rank = arg_info_list[info_idx].ed.rank;
               pointer = arg_info_list[info_idx].ed.pointer;
            }

            if (!AT_ELEMENTAL_INTRIN(spec_idx) && 
                !ATP_ELEMENTAL(spec_idx) &&
                 aa_rank != da_rank) {            /* ranks are different */

               if (spec_count > 0) {

                  /* rank must match for generic resolution */

                  same = FALSE;
                  break;
               }

               if (pointer && ATD_POINTER(arg_attr)) {
                  PRINTMSG(opnd_line, 1091, Error, opnd_column,
                           AT_OBJ_NAME_PTR(arg_attr));
                  same = FALSE;
               }
               else if (aa_rank == 0) {     /* scalar to array */
#ifdef KEY /* Bug 14150 */
		  linear_type_type actual_linear_type = (info_idx == NULL_IDX) ?
		    Err_Res :
		    arg_info_list[info_idx].ed.linear_type;
		  int atd_array_idx = ATD_ARRAY_IDX(arg_attr);
                  bd_array_type dummy_array_class = atd_array_idx ?
		    BD_ARRAY_CLASS(atd_array_idx) :
		    Unknown_Array;
		  linear_type_type dummy_linear_type = 
		    TYP_TYPE(ATD_TYPE_IDX(arg_attr));
#endif /* KEY Bug 14150 */

                  if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(arg_attr)) == Assumed_Shape){
                     PRINTMSG(opnd_line, 434, Error, opnd_column,
                              AT_OBJ_NAME_PTR(arg_attr));
                     same = FALSE;
                  }
                  else if (info_idx != NULL_IDX &&
                           arg_info_list[info_idx].ed.array_elt) {
   
                     /* this is ok, intentionally blank */
                     /* If info_idx is NULL, we do not  */
                     /* have an expression.             */

                  }
#ifdef KEY /* Bug 14150 */
                  else if ((actual_linear_type == Character_1 ||
		    actual_linear_type == Short_Char_Const) &&
		    (dummy_array_class == Explicit_Shape ||
		    dummy_array_class == Assumed_Size) &&
		    dummy_linear_type == Character) {
		    /* F2003 section 12.4.1.5 rules of sequence association plus
		     * note 15.19 on passing strings to bind(c) say that a
		     * scalar character(n) actual is compatible with an
		     * explicit- or assumed-size array of type character. */
		  }
#endif /* KEY Bug 14150 */
                  else {
                     PRINTMSG(opnd_line, 435, Error, opnd_column,
                              AT_OBJ_NAME_PTR(arg_attr));
                     same = FALSE;
                  }
               }
               else if (da_rank == 0) { /* array to scalar */
                  same = FALSE;
                  PRINTMSG(opnd_line, 436, Error, opnd_column,
                           AT_OBJ_NAME_PTR(arg_attr));
               }
               else { /* array to array, ranks don't match */

                  if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(arg_attr)) == Assumed_Shape){
                     PRINTMSG(opnd_line, 772, Error, opnd_column,
                              AT_OBJ_NAME_PTR(arg_attr));
                     break;
                  }
               }
            }
         }
      }

      if (info_idx == NULL_IDX) {
         a_type_idx	= ATD_TYPE_IDX(attr_idx);
         a_linear_type	= TYP_LINEAR(a_type_idx);
         a_type		= TYP_TYPE(a_type_idx);
      }
      else {
         a_type_idx	= arg_info_list[info_idx].ed.type_idx;
         a_linear_type	= arg_info_list[info_idx].ed.linear_type;
         a_type		= arg_info_list[info_idx].ed.type;
      }

#ifdef KEY /* Bug 5089 */
      /* Must not assume that Intrin_Proc uses ATD_INTRIN_DARG_TYPE (a
       * procedure from an intrinsic module does not.) */
      if (ATP_PROC(spec_idx) == Intrin_Proc && ATD_INTRIN_DARG(arg_attr))
#else /* KEY Bug 5089 */
      if (ATP_PROC(spec_idx) == Intrin_Proc)
#endif /* KEY Bug 5089 */
      {

         if (((1 << a_linear_type) & ATD_INTRIN_DARG_TYPE(arg_attr)) == 0) {
            same = FALSE;

            if (spec_count == 0) {
               PRINTMSG(opnd_line, 334, Error, opnd_column);
            }
            break;
         }
      }
      else if (ATD_IGNORE_TKR(arg_attr)) {

         /* intentionally blank */
         /* This dummy arg will match any type, so skip */
         /* the type and kind type checking below.      */
      }
      else if (OPND_FLD(list_opnd) == IR_Tbl_Idx &&
               IR_OPR(OPND_IDX(list_opnd)) == Null_Intrinsic_Opr) {

         /* intentionally blank */
         /* Don't know type or rank yet, they come from dummy */
      }
      else {
         type_idx = ATD_TYPE_IDX(arg_attr);

         if (a_linear_type == Short_Typeless_Const &&
             (TYP_TYPE(type_idx) == Integer ||
              TYP_TYPE(type_idx) == Real ||
              TYP_TYPE(type_idx) == Complex)) {

            /* intentionally blank */
            /* the constant will be converted to dummy type later */
         }
         else if (a_type == Typeless &&
                  (TYP_TYPE(type_idx) == Integer ||
                   TYP_TYPE(type_idx) == Real) &&
                      num_host_wds[a_linear_type] ==
                      num_host_wds[TYP_LINEAR(type_idx)]) {

            /* intentionally blank */
            /* no conversion will be done */
         }
         else if (a_linear_type == Short_Typeless_Const &&
                  (CN_HOLLERITH_TYPE(OPND_IDX(list_opnd)) == H_Hollerith ||
                   CN_HOLLERITH_TYPE(OPND_IDX(list_opnd)) == L_Hollerith) &&
                  TYP_TYPE(type_idx) == Character) {

            /* intentionall blank */
            /* this will be turned into a character constant later */
         }
         else if (a_type != TYP_TYPE(type_idx)) {
            same = FALSE;

            if (spec_count == 0) {
               type_word1[0] = '\0';
               type_word2[0] = '\0';
               strcat(type_word1,get_basic_type_str(type_idx));
               strcat(type_word2,get_basic_type_str(a_type_idx));
               PRINTMSG(opnd_line, 1108, Error, opnd_column,
                        type_word2,
                        type_word1);
            }
         }
         else if (TYP_TYPE(type_idx) == Structure &&
                  !compare_derived_types(type_idx, a_type_idx)) {
            same = FALSE;

            if (spec_count == 0) {
               type_word1[0] = '\0';
               type_word2[0] = '\0';
               strcat(type_word1,get_basic_type_str(type_idx));
               strcat(type_word2,get_basic_type_str(a_type_idx));
               PRINTMSG(opnd_line, 1108, Error, opnd_column,
                        type_word2,
                        type_word1);
            }
         }
         else if (TYP_TYPE(type_idx) == Character) {

            if (info_idx != NULL_IDX && TYP_FLD(type_idx) == CN_Tbl_Idx) {
               COPY_OPND(opnd, list_opnd);
               exp_desc			  = arg_info_list[info_idx].ed;
               same			 &= validate_char_len(&opnd, &exp_desc);
               arg_info_list[info_idx].ed = exp_desc;

               if (TYP_FLD(exp_desc.type_idx) == CN_Tbl_Idx &&
                   fold_relationals(TYP_IDX(exp_desc.type_idx),
                                    TYP_IDX(type_idx),
                                    Lt_Opr)) {

                  if (OPND_FLD(list_opnd) == CN_Tbl_Idx) {
                     PRINTMSG(opnd_line, 1305, Caution, opnd_column);
                     PRINTMSG(opnd_line, 1306, Ansi, opnd_column);
                     cast_to_type_idx(&opnd,
                                      &exp_desc,
                                      type_idx);
                     arg_info_list[info_idx].ed = exp_desc;
                     COPY_OPND(list_opnd, opnd);
                  }
               }
            }
         }
         else if (TYP_TYPE(type_idx) != Structure &&
                  a_linear_type != TYP_LINEAR(type_idx)) {
            same = FALSE;
     
            if (spec_count == 0) { /* error .. kinds don't match */
               PRINTMSG(opnd_line, 1307, Error, opnd_column,
                        storage_bit_kind_tbl[a_linear_type],
                        storage_bit_kind_tbl[TYP_LINEAR(type_idx)]);
            }
         }
      }
      break;

   
   case Pgm_Unit  :
   
      if (!pgm_unit) {
         same = FALSE;
         
         if (spec_count == 0) { /* error .. expecting pgm unit */
            PRINTMSG(opnd_line, 502, Error,
                     opnd_column,
                     AT_OBJ_NAME_PTR(arg_attr));
         }
         break;
      }
#ifdef KEY /* Bug 14150 */
      else if (ATD_IGNORE_TKR(arg_attr)) {
        /* As long as both actual and dummy are procedures, accept this */
      }
#endif /* KEY Bug 14150 */
      else if (ATP_EXPL_ITRFC(attr_idx) && ATP_EXPL_ITRFC(arg_attr)) {

         /* If a darg and an actual arg are both procedures and they */
         /* both have explicit interfaces then they must match to be */
         /* the same.  This routine will recursively check them.     */

         if (ATP_NUM_DARGS(attr_idx) == ATP_NUM_DARGS(arg_attr) &&
             ATP_PGM_UNIT(attr_idx) == ATP_PGM_UNIT(arg_attr)) {

            if (ATP_PGM_UNIT(attr_idx) == Function) {

               if (info_idx != NULL_IDX) {

                  same = compare_func_result(arg_attr, 
                                         arg_info_list[info_idx].ed.type_idx,
                                         arg_info_list[info_idx].ed.type,
                                         arg_info_list[info_idx].ed.linear_type,
                                         arg_info_list[info_idx].ed.rank,
                                         spec_count,
                                         opnd_line,
                                         opnd_column);
               }
               else {
                  a_type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));
                  aa_rank = (ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx)) == NULL_IDX)?
                            0 : BD_RANK(ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx)));

                  same = compare_func_result(arg_attr, 
                                            a_type_idx,
                                            TYP_TYPE(a_type_idx),
                                            TYP_LINEAR(a_type_idx),
                                            aa_rank,
                                            spec_count,
                                            opnd_line,
                                            opnd_column);
               }

               if (!same) {
                  break;
               }
            }

            /* Compare dummy arguments */

            for (idx = (ATP_EXTRA_DARG(arg_attr) ? 1:0);
                 idx < ATP_NUM_DARGS(arg_attr); idx++) {

               COPY_OPND(opnd, list_opnd);
               OPND_FLD(opnd)	= AT_Tbl_Idx;
               OPND_IDX(opnd)	= SN_ATTR_IDX(ATP_FIRST_IDX(attr_idx)+idx);

               if (!compare_darg_to_actual_arg(
                                       gen_idx,
                                       spec_idx,
                                       SN_ATTR_IDX(ATP_FIRST_IDX(arg_attr)+idx),
                                       opnd,
                                       NULL_IDX,
                                       spec_count)) {

                  /* If messages are necessary, they will be */
                  /* issued from compare_darg_to_actual_arg. */

                  same = FALSE;
                  break;
               }
            }
         }
         else {

            if (spec_count == 0) {
               PRINTMSG(opnd_line, 1635, Error,
                        opnd_column,
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(arg_attr));
            }
            same = FALSE;
         }
      }
      else if (ATP_EXPL_ITRFC(arg_attr) && spec_count != 0) {

         /* Interp 51 - We are looking for a specific with a dummy    */
         /* procedure argument.  Interp 51, states that we cannot use */
         /* whether it is a subroutine or a function to differentiate */
         /* a specific if there is no explciit interface declared.    */

         same = FALSE;
      }
      else if (ATP_PGM_UNIT(attr_idx) == Function) {

         /* matches same type function, or             */
         /* a Pgm_Unknown with same impl type.         */

         if (ATP_PGM_UNIT(arg_attr) == Function     ||
             ATP_PGM_UNIT(arg_attr) == Pgm_Unknown) {

            if (info_idx != NULL_IDX) {
               same = compare_func_result(arg_attr, 
                                         arg_info_list[info_idx].ed.type_idx,
                                         arg_info_list[info_idx].ed.type,
                                         arg_info_list[info_idx].ed.linear_type,
                                         arg_info_list[info_idx].ed.rank,
                                         spec_count,
                                         opnd_line,
                                         opnd_column);
            }
            else {
               a_type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));
               aa_rank    = (ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx)) == NULL_IDX)?
                             0 : BD_RANK(ATD_ARRAY_IDX(ATP_RSLT_IDX(attr_idx)));

               same       = compare_func_result(arg_attr, 
                                                a_type_idx,
                                                TYP_TYPE(a_type_idx),
                                                TYP_LINEAR(a_type_idx),
                                                aa_rank,
                                                spec_count,
                                                opnd_line,
                                                opnd_column);
            }

            if (!same) {

               if (spec_count == 0) {

                  /* ok for specific check */

               }
               else {
                  break;
               }
            }
         }
         else { /* error .. expected function got Subroutine */
            same = FALSE;

            if (spec_count == 0) {
               PRINTMSG(opnd_line, 503, Error, opnd_column,
                        AT_OBJ_NAME_PTR(attr_idx),
                        "FUNCTION",
                        AT_OBJ_NAME_PTR(arg_attr),
                        "SUBROUTINE");
            }
            break;
         }
      }
      else if (ATP_PGM_UNIT(attr_idx) == Subroutine) {

         /* matches Subroutine, or Pgm_Unknown         */

         if (ATP_PGM_UNIT(arg_attr) != Subroutine   &&
             ATP_PGM_UNIT(arg_attr) != Pgm_Unknown) {
            same = FALSE;

            if (spec_count == 0) {
               PRINTMSG(opnd_line, 503, Error,
                        opnd_column,
                        AT_OBJ_NAME_PTR(attr_idx),
                        "SUBROUTINE",
                        AT_OBJ_NAME_PTR(arg_attr),
                        "FUNCTION");
            }
            break;
         }
      }
      else if (ATP_PGM_UNIT(attr_idx) == Pgm_Unknown) {

         /* matches Subroutine, or Pgm_Unknown or      */
         /* a function                                 */

         if (ATP_PGM_UNIT(arg_attr) != Subroutine   &&
             ATP_PGM_UNIT(arg_attr) != Function     &&
             ATP_PGM_UNIT(arg_attr) != Pgm_Unknown) {

            /* shouldn't be here, dummy arg should only be */
            /* Function, Subroutine, or Pgm_Unknown        */

# ifdef _DEBUG
            print_at_all(arg_attr);
# endif
            PRINTMSG(opnd_line, 972, Internal, opnd_column);
         }
      }


      break;

   default :
# ifdef _DEBUG
      print_at_all(arg_attr);
# endif
      PRINTMSG(opnd_line, 972, Internal, opnd_column);
      break;

   }  /* end switch */

   TRACE (Func_Exit, "compare_darg_to_actual_arg", NULL);

   return(same);

}  /* compare_darg_to_actual_arg */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compare an actual function result with its corresponding darg.        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      arg_attr    -                                                         *|
|*      info_idx    -                                                         *|
|*      spec_count  -                                                         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*	NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if they are the same.                                            *|
|*                                                                            *|
\******************************************************************************/
static boolean compare_func_result(int          darg_attr,
                                   int          a_type_idx,
                                   int          a_type,
                                   int          a_linear_type,
                                   int          a_rank,
                                   int          spec_count,
                                   int		line,
                                   int		column)

{
   boolean              same            = TRUE;
   int                  type_idx;
   char                 type_word1[40];
   char                 type_word2[40];


   TRACE (Func_Entry, "compare_func_result", NULL);

   darg_attr = ATP_RSLT_IDX(darg_attr);

   if (darg_attr == NULL_IDX) { /* have an INTRINSIC darg */
      type_idx = a_type_idx;
   }
   else {
      type_idx = ATD_TYPE_IDX(darg_attr);
   }

   if (a_type != TYP_TYPE(type_idx)){
      same = FALSE;

      if (spec_count == 0) {
         type_word1[0] = '\0';
         type_word2[0] = '\0';
         strcat(type_word1,get_basic_type_str(type_idx));
         strcat(type_word2,get_basic_type_str(a_type_idx));
         PRINTMSG(line, 1108, Error, column, type_word2, type_word1);
      }
   }
   else if (TYP_TYPE(type_idx) == Structure &&
            !compare_derived_types(a_type_idx, type_idx)) {
      same = FALSE;

      if (spec_count == 0) {
         type_word1[0] = '\0';
         type_word2[0] = '\0';
         strcat(type_word1, get_basic_type_str(type_idx));
         strcat(type_word2, get_basic_type_str(a_type_idx));
         PRINTMSG(line, 1108, Error, column, type_word2, type_word1);
      }
   }
   else if (TYP_TYPE(type_idx) != Structure &&
            TYP_TYPE(type_idx) != Character &&
            a_linear_type != TYP_LINEAR(type_idx)) {
      same = FALSE;
      
      if (spec_count == 0) { /* error .. kinds don't match */
         PRINTMSG(line, 1307, Error, column,
                       storage_bit_kind_tbl[a_linear_type],
                       storage_bit_kind_tbl[TYP_LINEAR(type_idx)]);
      }
   }
   else if (darg_attr != NULL_IDX && (a_rank != (ATD_ARRAY_IDX(darg_attr) ? 
                                      BD_RANK(ATD_ARRAY_IDX(darg_attr)) : 0))) {

      if (spec_count != 0) { /* ok for specific check */
         same = FALSE;
      }
   }

   TRACE (Func_Exit, "compare_func_result", NULL);

   return(same);

}  /* compare_func_result */

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

static boolean check_arg_for_co_array(opnd_type	*top_opnd)

{
   int		col;
   int		ir_idx;
   int		line;
   int		list_idx;
   boolean	ok = TRUE;
   opnd_type	opnd;

   TRACE (Func_Entry, "check_arg_for_co_array", NULL);

   if (OPND_FLD((*top_opnd)) == IR_Tbl_Idx) {
      ir_idx = OPND_IDX((*top_opnd));

      switch(IR_OPR(ir_idx)) {
      case Struct_Opr:
      case Dv_Deref_Opr:
         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         ok &= check_arg_for_co_array(&opnd);
         break;

      case Subscript_Opr:
      case Whole_Subscript_Opr:
      case Section_Subscript_Opr:
         list_idx = IR_IDX_R(ir_idx);
         while (list_idx) {
            if (IL_PE_SUBSCRIPT(list_idx)) {
               find_opnd_line_and_column(&IL_OPND(list_idx), &line, &col);
               PRINTMSG(line, 1366, Error, col);
               ok = FALSE;
               break;
            }
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         /* intentionally falls through */


      case Substring_Opr:
      case Whole_Substring_Opr:

         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         ok &= check_arg_for_co_array(&opnd);

         list_idx = IR_IDX_R(ir_idx);
         while (list_idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok &= check_arg_for_co_array(&opnd);
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      case Triplet_Opr:
         list_idx = IR_IDX_L(ir_idx);
         while (list_idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            ok &= check_arg_for_co_array(&opnd);
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      }
   }

   TRACE (Func_Exit, "check_arg_for_co_array", NULL);

   return(ok);

}  /* check_arg_for_co_array */

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

static void update_components(opnd_type	*opnd)

{
   int		attr_idx;
   int		il_idx;
   int		ir_idx;
   int		sn_idx;


   TRACE (Func_Entry, "update_components", NULL);

   switch (OPND_FLD((*opnd))) {
   case IR_Tbl_Idx:

      ir_idx = OPND_IDX((*opnd));

      if (IR_OPR(ir_idx) == Struct_Opr) {

         /* Right is the component */

         if (IR_FLD_L(ir_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Derived_Type) {

            if (ATD_DERIVED_TYPE_IDX(IR_IDX_R(ir_idx)) != IR_IDX_L(ir_idx)) {

               /* The derived type is out of sync with the component. */
               /* Update the component attr index.                    */

               sn_idx	= ATT_FIRST_CPNT_IDX(IR_IDX_L(ir_idx));
               attr_idx	= srch_linked_sn(AT_OBJ_NAME_PTR(IR_IDX_R(ir_idx)),
                                         AT_NAME_LEN(IR_IDX_R(ir_idx)),
                                        &sn_idx);

# ifdef _DEBUG
               if (attr_idx == NULL_IDX) {
                   PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, 
                            IR_COL_NUM(ir_idx),
                            "component to be found",
                            "update_components");
               }
# endif
               IR_IDX_R(ir_idx)	= attr_idx;
            }  /* Same - so don't need to update */
         }
         else {

            if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {

               /* Nested structure reference.  Derived */
               /* type should be on this ir.           */

               if (TYP_TYPE(IR_TYPE_IDX(IR_IDX_L(ir_idx))) == Structure &&
                   ATD_DERIVED_TYPE_IDX(IR_IDX_R(ir_idx)) !=
                   TYP_IDX(IR_TYPE_IDX(IR_IDX_L(ir_idx)))) {

                  /* The derived type is out of sync with the component. */
                  /* Update the component attr index.                    */

                  sn_idx = ATT_FIRST_CPNT_IDX(TYP_IDX(
                                              IR_TYPE_IDX(IR_IDX_L(ir_idx))));
                  attr_idx = srch_linked_sn(AT_OBJ_NAME_PTR(IR_IDX_R(ir_idx)),
                                            AT_NAME_LEN(IR_IDX_R(ir_idx)),
                                           &sn_idx);

# ifdef _DEBUG
                  if (attr_idx == NULL_IDX) {
                      PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, 
                               IR_COL_NUM(ir_idx),
                               "component to be found",
                               "update_components");
                  }
# endif
                  IR_IDX_R(ir_idx)	= attr_idx;
               }  /* Same - so don't need to update */
            }
            update_components(&IR_OPND_L(ir_idx));
         }
      }
      else {
         update_components(&IR_OPND_L(ir_idx));
         update_components(&IR_OPND_R(ir_idx));
      }

      break;

   case IL_Tbl_Idx:
      il_idx = OPND_IDX((*opnd));

      do {
         update_components(&IL_OPND(il_idx));
         il_idx = IL_NEXT_LIST_IDX(il_idx);
      }
      while (il_idx != NULL_IDX);
      break;

   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
   case AT_Tbl_Idx:
   case CN_Tbl_Idx:
   case SB_Tbl_Idx:    /* done - quit looking here */
      break;

   default:            /* done - quit looking here */
      break;
   }

   TRACE (Func_Exit, "update_components", NULL);

   return;

}  /* update_components */
