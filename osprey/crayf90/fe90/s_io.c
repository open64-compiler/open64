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



static char USMID[] = "\n@(#)5.0_pl/sources/s_io.c	5.8	10/04/99 17:44:33\n";

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
# include "p_io.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"
# include "s_io.h"


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static boolean   io_ctl_list_semantics(opnd_type *, io_stmt_type, boolean);
static boolean   io_list_semantics(opnd_type *, io_stmt_type);
static void      namelist_static_dv_whole_def(opnd_type *, opnd_type *);
static void      put_string_in_tmp(char	*, int, opnd_type *);

# ifdef _INIT_RELOC_BASE_OFFSET
static int	 change_to_base_and_offset(opnd_type *, opnd_type *);
# endif
static int       create_scalar_type_tbl(opnd_type *, boolean);
static int       create_strct_tbl(opnd_type *, boolean);
static boolean   do_read_namelist_semantics(opnd_type	*);
static void      do_write_namelist_semantics(opnd_type	*);
static int       discombobulate_structure_ref(opnd_type	*, int, int *);
static int       change_section_to_do(int *);
static void      process_deferred_io_list(void);
static void      expand_io_list(void);
static void      expand_imp_do(int, int);
static int       copy_text_for_expansion(int);
static void	 create_io_call_descriptor(int, io_descriptor_type);
# ifdef _NO_IO_ALTERNATE_RETURN
static void	 add_alt_return_lbl(int, int);
# endif
static boolean	 item_has_bounds_chk(opnd_type *);
static void gen_array_element_init(int, long_type *, opnd_type *, int, int);

static int	err_attr_idx;


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void backspace_stmt_semantics (void)

{
# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int			asg_idx;
   int			br_true_idx;
   int			col;
   int			eq_idx;
   int			line;
   int			save_next_sh_idx;
# endif

   int                  ir_idx;
   opnd_type            opnd;
   int			save_arg_info_list_base;
   int			save_curr_stmt_sh_idx;
   boolean              semantically_correct;


   TRACE (Func_Entry, "backspace_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

# ifndef _NO_IO_ALTERNATE_RETURN
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
# endif

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Backspace, TRUE);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

# ifndef _NO_IO_ALTERNATE_RETURN
   if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {

      line = IR_LINE_NUM(ir_idx);
      col  = IR_COL_NUM(ir_idx);

      alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
      ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_REFERENCED(alt_return_tmp)     = Referenced;
      AT_DEFINED(alt_return_tmp)        = TRUE;
      AT_SEMANTICS_DONE(alt_return_tmp)	= TRUE;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Alt_Return_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = alt_return_tmp;
      IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)      = ir_idx;

      SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;

      if (err_list_idx) {
         NTR_IR_TBL(br_true_idx);
         IR_OPR(br_true_idx)      = Br_True_Opr;
         IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(br_true_idx) = line;
         IR_COL_NUM(br_true_idx)  = col;

         NTR_IR_TBL(eq_idx);
         IR_OPR(eq_idx)           = Eq_Opr;
         IR_TYPE_IDX(eq_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(eq_idx)      = line;
         IR_COL_NUM(eq_idx)       = col;
         IR_FLD_L(eq_idx)         = AT_Tbl_Idx;
         IR_IDX_L(eq_idx)         = alt_return_tmp;
         IR_LINE_NUM_L(eq_idx)    = line;
         IR_COL_NUM_L(eq_idx)     = col;

         IR_FLD_R(eq_idx)         = CN_Tbl_Idx;
         IR_IDX_R(eq_idx)         = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_R(eq_idx)    = line;
         IR_COL_NUM_R(eq_idx)     = col;

         IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
         IR_IDX_L(br_true_idx)    = eq_idx;

         COPY_OPND(IR_OPND_R(br_true_idx), IL_OPND(err_list_idx));

         curr_stmt_sh_idx = save_next_sh_idx;

         gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = br_true_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }
   }
# endif
      
   if (semantically_correct) {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = final_arg_work(&opnd,
                                            IR_IDX_L(ir_idx), 
                                            IR_LIST_CNT_R(ir_idx),
                                            NULL);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

# if defined(_FILE_IO_OPRS)
      IR_OPR(ir_idx) = Backspace_Opr;
# endif
   }

# ifdef _NO_IO_ALTERNATE_RETURN
   add_alt_return_lbl(ir_idx, err_attr_idx);
# endif

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "backspace_stmt_semantics", NULL);

   return;

}  /* backspace_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	do semantic stuff for buffer in and buffer out stmts.                 *|
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

void buffer_stmt_semantics (void)

{
   int		 base_attr;
   boolean	 buffer_in;
   int		 col;
   expr_arg_type exp_desc;
   expr_arg_type exp_desc2;
   int		 info_idx;
   int 		 ir_idx;
   int		 line;
   int		 list_idx;
   opnd_type	 opnd;
   int		 save_arg_info_list_base;
   boolean       semantically_correct;
   long_type	 the_constant[2];
   int		 type_idx;


   TRACE (Func_Entry, "buffer_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + 5;

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

   info_idx = arg_info_list_base;

   if (IR_IDX_L(ir_idx) == glb_tbl_idx[Buffer_In_Attr_Idx]) {
      buffer_in = TRUE;
   }
   else {
      buffer_in = FALSE;
   }

   /* process unit number or file name */

   list_idx = IR_IDX_R(ir_idx);
   COPY_OPND(opnd, IL_OPND(list_idx));
   exp_desc.rank = 0;
   xref_state = CIF_Symbol_Reference;
   semantically_correct = expr_semantics(&opnd, &exp_desc);
   COPY_OPND(IL_OPND(list_idx), opnd);
   find_opnd_line_and_column(&opnd, &line, &col);

   if (exp_desc.linear_type == Long_Typeless) {
      PRINTMSG(line, 1133, Error, col);
      semantically_correct = FALSE;
   }
   else if (exp_desc.type != Integer    &&
            exp_desc.type != Typeless   &&
            exp_desc.type != Character) {

      /* error .. bad type for unit */

      PRINTMSG(line, 229, Error, col);
      semantically_correct = FALSE;
   }
   else if (exp_desc.type != Character &&
            exp_desc.rank != 0)        {

      /* error .. not scalar */

      PRINTMSG(line, 229, Error, col);
      semantically_correct = FALSE;
   }
   else if (exp_desc.type == Character &&
            exp_desc.constant) {

      /* change to Typeless if length right */

      if (compare_cn_and_value(TYP_IDX(CN_TYPE_IDX(OPND_IDX(opnd))),
                               TARGET_BYTES_PER_WORD,
                               Lt_Opr)) {
         CN_TYPE_IDX(OPND_IDX(opnd)) = TYPELESS_DEFAULT_TYPE;
      }
      else {
         PRINTMSG(line, 231, Error, col,
                  TARGET_BYTES_PER_WORD - 1);
         semantically_correct = FALSE;
      }
   }
   else if (exp_desc.linear_type == Short_Typeless_Const) {
      IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
						CG_INTEGER_DEFAULT_TYPE,
						line,
						col);
      exp_desc.linear_type = INTEGER_DEFAULT_TYPE;
      exp_desc.type_idx    = INTEGER_DEFAULT_TYPE;
      exp_desc.type        = Integer;
   }

   COPY_OPND(opnd, IL_OPND(list_idx));
   cast_to_cg_default(&opnd, &exp_desc);
   COPY_OPND(IL_OPND(list_idx), opnd);

   info_idx++;
   arg_info_list[info_idx]    = init_arg_info;
   arg_info_list[info_idx].ed = exp_desc;
   arg_info_list[info_idx].maybe_modified   = TRUE;
   IL_ARG_DESC_IDX(list_idx)                = info_idx;

   /* process mode */

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(opnd, IL_OPND(list_idx));
   exp_desc.rank = 0;
   xref_state = CIF_Symbol_Reference;
   semantically_correct = expr_semantics(&opnd, &exp_desc) &&
                          semantically_correct;
   COPY_OPND(IL_OPND(list_idx), opnd);

   if (exp_desc.type != Integer) {

      /* error .. mode must be integer expression */

      find_opnd_line_and_column(&opnd, &line, &col);
      PRINTMSG(line, 228, Error, col);
      semantically_correct = FALSE;
   }
   
   if (exp_desc.rank != 0) {      /* error .. must be scalar */
      find_opnd_line_and_column(&opnd, &line, &col);
      PRINTMSG(line, 230, Error, col);
      semantically_correct = FALSE;
   }

   COPY_OPND(opnd, IL_OPND(list_idx));
   cast_to_cg_default(&opnd, &exp_desc);
   COPY_OPND(IL_OPND(list_idx), opnd);

   info_idx++;
   arg_info_list[info_idx]    = init_arg_info;
   arg_info_list[info_idx].ed = exp_desc; 
   arg_info_list[info_idx].maybe_modified   = TRUE;
   IL_ARG_DESC_IDX(list_idx)                = info_idx;
   
   /* process bloc */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   COPY_OPND(opnd, IL_OPND(list_idx));
   exp_desc.rank = 0;

   if (buffer_in) {
      xref_state = CIF_Symbol_Modification;
   }
   else {
      xref_state = CIF_Symbol_Reference;
   }
   semantically_correct = expr_semantics(&opnd, &exp_desc) &&
                          semantically_correct;
   COPY_OPND(IL_OPND(list_idx), opnd);

   base_attr = find_base_attr(&opnd, &line, &col);

   info_idx++;
   arg_info_list[info_idx]    = init_arg_info;
   arg_info_list[info_idx].ed = exp_desc;
   arg_info_list[info_idx].maybe_modified   = TRUE;
   IL_ARG_DESC_IDX(list_idx)                = info_idx;

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /* process eloc */

   COPY_OPND(opnd, IL_OPND(list_idx));
   exp_desc.rank = 0;

   if (buffer_in) {
      xref_state = CIF_Symbol_Modification;
   }
   else {
      xref_state = CIF_Symbol_Reference;
   }
   semantically_correct = expr_semantics(&opnd, &exp_desc2) &&
                          semantically_correct;
   COPY_OPND(IL_OPND(list_idx), opnd);

   if (exp_desc.type == Structure) {
      find_opnd_line_and_column(&opnd, &line, &col);
      PRINTMSG(line, 879, Error, col);
      semantically_correct = FALSE;
   }
   else if ((exp_desc.type == Character &&
             exp_desc2.type != Character) ||
            (exp_desc2.type == Character &&
             exp_desc.type != Character)) {

      find_opnd_line_and_column(&opnd, &line, &col);
      PRINTMSG(line, 896, Error, col);
      semantically_correct = FALSE;
   }
   else if (exp_desc.type != Character &&
            exp_desc2.type != Character &&
            storage_bit_size_tbl[exp_desc.linear_type] !=
            storage_bit_size_tbl[exp_desc2.linear_type]) {

      find_opnd_line_and_column(&opnd, &line, &col);
      PRINTMSG(line, 896, Error, col);
      semantically_correct = FALSE;
   }

   info_idx++;
   arg_info_list[info_idx]    = init_arg_info;
   arg_info_list[info_idx].ed = exp_desc2;
   arg_info_list[info_idx].maybe_modified   = TRUE;
   IL_ARG_DESC_IDX(list_idx)                = info_idx;

   /* set up type code arg */

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   IL_ARG_DESC_VARIANT(list_idx) = TRUE;
   IR_LIST_CNT_R(ir_idx) = 5;

   make_io_type_code(ATD_TYPE_IDX(base_attr), the_constant); /* BRIANJ */
   IL_FLD(list_idx) = CN_Tbl_Idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) && ! defined(_TYPE_CODE_64_BIT)
   /* the type information goes in a 64 bit thing for mongoose */
   the_constant[1] = the_constant[0];
   the_constant[0] = 0;
   type_idx = Integer_8;
# else
   type_idx = IO_TYPE_CODE_TYPE;
# endif

  /* BRIANJ - KAYKAY */

   IL_IDX(list_idx) = ntr_const_tbl(type_idx,
                                    FALSE,
                                    the_constant);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   exp_desc = init_exp_desc;
   exp_desc.type = TYP_TYPE(type_idx);
   exp_desc.linear_type = TYP_LINEAR(type_idx);
   exp_desc.type_idx = type_idx;
   exp_desc.constant = TRUE;
   exp_desc.foldable = TRUE;

   info_idx++;
   arg_info_list[info_idx]    = init_arg_info;
   arg_info_list[info_idx].ed = exp_desc;
   arg_info_list[info_idx].maybe_modified   = TRUE;
   IL_ARG_DESC_IDX(list_idx)                = info_idx;

   if (semantically_correct) {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = final_arg_work(&opnd,
                                            IR_IDX_L(ir_idx),
                                            IR_LIST_CNT_R(ir_idx),
                                            NULL);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);
      create_io_call_descriptor(ir_idx, Buffer_Desc);
   }

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   TRACE (Func_Exit, "buffer_stmt_semantics", NULL);

   return;

}  /* buffer_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void close_stmt_semantics (void)

{

   int			ir_idx;
   opnd_type		opnd;
   int                  save_arg_info_list_base;
   int                  save_curr_stmt_sh_idx;
   boolean		semantically_correct;

# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int                  asg_idx;
   int                  br_true_idx;
   int                  col;
   int                  eq_idx;
   int                  line;
   int                  save_next_sh_idx;
# endif


   TRACE (Func_Entry, "close_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

# ifndef _NO_IO_ALTERNATE_RETURN
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
# endif

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Close, TRUE);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

# ifndef _NO_IO_ALTERNATE_RETURN
   if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {

      line = IR_LINE_NUM(ir_idx);
      col  = IR_COL_NUM(ir_idx);

      alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
      ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_REFERENCED(alt_return_tmp)     = Referenced;
      AT_DEFINED(alt_return_tmp)        = TRUE;
      AT_SEMANTICS_DONE(alt_return_tmp)	= TRUE;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Alt_Return_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = alt_return_tmp;
      IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)      = ir_idx;

      SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;

      if (err_list_idx) {
         NTR_IR_TBL(br_true_idx);
         IR_OPR(br_true_idx)      = Br_True_Opr;
         IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(br_true_idx) = line;
         IR_COL_NUM(br_true_idx)  = col;

         NTR_IR_TBL(eq_idx);
         IR_OPR(eq_idx)           = Eq_Opr;
         IR_TYPE_IDX(eq_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(eq_idx)      = line;
         IR_COL_NUM(eq_idx)       = col;
         IR_FLD_L(eq_idx)         = AT_Tbl_Idx;
         IR_IDX_L(eq_idx)         = alt_return_tmp;
         IR_LINE_NUM_L(eq_idx)    = line;
         IR_COL_NUM_L(eq_idx)     = col;

         IR_FLD_R(eq_idx)         = CN_Tbl_Idx;
         IR_IDX_R(eq_idx)         = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_R(eq_idx)    = line;
         IR_COL_NUM_R(eq_idx)     = col;

         IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
         IR_IDX_L(br_true_idx)    = eq_idx;

         COPY_OPND(IR_OPND_R(br_true_idx), IL_OPND(err_list_idx));

         curr_stmt_sh_idx = save_next_sh_idx;

         gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = br_true_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }
   }
# endif

   if (semantically_correct) {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = final_arg_work(&opnd,
                                            IR_IDX_L(ir_idx),
                                            IR_LIST_CNT_R(ir_idx),
                                            NULL);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);
      create_io_call_descriptor(ir_idx, Close_Desc);
# if defined(_FILE_IO_OPRS)
      IR_OPR(ir_idx) = Close_Opr;
# endif
   }

# ifdef _NO_IO_ALTERNATE_RETURN
   add_alt_return_lbl(ir_idx, err_attr_idx);
# endif

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "close_stmt_semantics", NULL);

   return;

}  /* close_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process the decode stmt. It is transformed into an internal Read.     *|
|*	Process the encode stmt. It is transformed into an internal Write.    *|
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

void encode_decode_stmt_semantics (void)

{
   int		 attr_idx;
   int		 cnt_list_idx;
   int		 col;
   expr_arg_type exp_desc;
   int	         free_list_idx;
   int		 fmt_list_idx;
   int		 intern_list_idx;
   int		 ir_idx;
   opnd_type	 left_opnd;
   int		 line;
   int		 list_idx;
   boolean       ok;
   opnd_type     opnd;
   int		 pp_tmp = NULL_IDX;
   int		 tmp_idx;


   TRACE (Func_Entry, "encode_decode_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   cnt_list_idx = IR_IDX_L(ir_idx);

   COPY_OPND(opnd, IL_OPND(cnt_list_idx));
   exp_desc.rank = 0;
   xref_state    = CIF_Symbol_Reference;
   ok = expr_semantics(&opnd, &exp_desc);
   COPY_OPND(IL_OPND(cnt_list_idx), opnd);
   find_opnd_line_and_column(&opnd, &line, &col);

   if (exp_desc.type != Integer) {
      PRINTMSG(line, 681, Error, col, stmt_type_str[stmt_type]);
   }
   else if (exp_desc.constant &&
            (CN_INT_TO_C(OPND_IDX(opnd)) <= 0 ||
             CN_INT_TO_C(OPND_IDX(opnd)) > 152)) {

      PRINTMSG(line, 682, Error, col, stmt_type_str[stmt_type]);
   }
   else if (exp_desc.rank > 0) {
      PRINTMSG(line, 683, Error, col, stmt_type_str[stmt_type]);
   }

   /* do I need the count ? */

   fmt_list_idx = IL_NEXT_LIST_IDX(cnt_list_idx);

   if (IL_FLD(fmt_list_idx) == IL_Tbl_Idx) {

      /* this was format character constant inline */
      /* do not send through expr_semantics.       */
      /* first item is format tmp, second is       */
      /* preparsed format tmp.                     */

      pp_tmp = IL_IDX(IL_NEXT_LIST_IDX(IL_IDX(fmt_list_idx)));
      FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(IL_IDX(fmt_list_idx)));
      free_list_idx = IL_IDX(fmt_list_idx);
      COPY_OPND(IL_OPND(fmt_list_idx), IL_OPND(IL_IDX(fmt_list_idx)));
      FREE_IR_LIST_NODE(free_list_idx);

      ADD_TMP_TO_SHARED_LIST(IL_IDX(fmt_list_idx));
      ADD_TMP_TO_SHARED_LIST(pp_tmp);
   }
   else if (IL_FLD(fmt_list_idx)               == AT_Tbl_Idx &&
            AT_OBJ_CLASS(IL_IDX(fmt_list_idx)) == Label)     {

      if (ATL_CLASS(IL_IDX(fmt_list_idx)) == Lbl_Format) {

         pp_tmp = ATL_PP_FORMAT_TMP(IL_IDX(fmt_list_idx));
         /* replace label reference with format constant idx */
         IL_IDX(fmt_list_idx) = ATL_FORMAT_TMP(IL_IDX(fmt_list_idx));
         IL_FLD(fmt_list_idx) = AT_Tbl_Idx;
         IL_LINE_NUM(fmt_list_idx) = line;
         IL_COL_NUM(fmt_list_idx)  = col;

         ADD_TMP_TO_SHARED_LIST(ATL_FORMAT_TMP(IL_IDX(fmt_list_idx)));

         ADD_TMP_TO_SHARED_LIST(ATL_PP_FORMAT_TMP(IL_IDX(fmt_list_idx)));

      }

      /* if not a format label LRR will have already caught it */
   }
   else {

      COPY_OPND(opnd, IL_OPND(fmt_list_idx));
      exp_desc.rank = 0;
      xref_state    = CIF_Symbol_Reference;
      io_item_must_flatten = FALSE;
      ok = expr_semantics(&opnd, &exp_desc);
      COPY_OPND(IL_OPND(fmt_list_idx), opnd);

      /* do format checks */
   
      find_opnd_line_and_column(&opnd, &line, &col);

      if (exp_desc.type == Character) {

         if (io_item_must_flatten ||
             exp_desc.dist_reshape_ref ||
             exp_desc.vector_subscript) {

            tmp_idx = create_tmp_asg(&opnd, &exp_desc, &left_opnd, 
                                     Intent_In, TRUE, FALSE);
            COPY_OPND(IL_OPND(fmt_list_idx), left_opnd);
         }
      }
      else if (exp_desc.rank > 0                                &&
               (OPND_FLD(opnd)         != IR_Tbl_Idx ||
                exp_desc.dope_vector                 ||
                IR_OPR(OPND_IDX(opnd)) != Whole_Subscript_Opr)) {
  
         /* these are noncontiguous arrays, sections, dope vectors */
         /* error .. format error */

         PRINTMSG(line, 447, Error, col);
      }
      else if (exp_desc.type == Integer &&
               exp_desc.reference)      {
 
         if (exp_desc.rank == 0) {    /* check for ASSIGN */

            if (!exp_desc.reference) { /* error .. must be variable */
               PRINTMSG(line, 447, Error, col);
            }
            else if (exp_desc.linear_type != INTEGER_DEFAULT_TYPE) {

               /* must be default kind */

               PRINTMSG(line, 462, Error, col);
            }
            else {
 
               attr_idx = find_base_attr(&opnd, &line, &col);

               if (! ATD_IN_ASSIGN(attr_idx)) {
                  PRINTMSG(line, 1099, Error, col);
               }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
               if (ATD_ASSIGN_TMP_IDX(attr_idx) != NULL_IDX) {
                  OPND_FLD(opnd) = AT_Tbl_Idx;
                  OPND_IDX(opnd) = ATD_ASSIGN_TMP_IDX(attr_idx);
                  COPY_OPND(IL_OPND(fmt_list_idx), opnd);
                  ADD_TMP_TO_SHARED_LIST(ATD_ASSIGN_TMP_IDX(attr_idx));
               }
# endif
            }
         }
         else { /* integer array is nonstandard */
            PRINTMSG(line, 778, Ansi, col);
         }
      }
      else if ((exp_desc.linear_type == REAL_DEFAULT_TYPE ||
                exp_desc.type == Logical)                &&
               exp_desc.reference                        &&
               exp_desc.rank > 0)                        {
         PRINTMSG(line, 778, Ansi, col);
      }
      else if (exp_desc.type == Typeless &&
               exp_desc.rank == 0)       {

         /* intentionally blank */
         /* ansi msg already issued by lex */
      }
      else { /* error .. format error */
         PRINTMSG(line, 447, Error, col);
      }
   }

   intern_list_idx = IL_NEXT_LIST_IDX(fmt_list_idx);

   COPY_OPND(opnd, IL_OPND(intern_list_idx));
   exp_desc.rank = 0;

   if (stmt_type == Encode_Stmt) {
      xref_state = CIF_Symbol_Modification;
   }
   else {
      xref_state = CIF_Symbol_Reference;
   }
   ok = expr_semantics(&opnd, &exp_desc);
   COPY_OPND(IL_OPND(intern_list_idx), opnd);

   /* do internal unit semantics */

   if (stmt_type == Encode_Stmt) {

      /* check for live do loop variable definition for encode */

      if (! check_for_legal_define(&opnd)) {
         ok = FALSE;
      }
   }

   /* internal unit must be variable, array element or contiguous array */

   COPY_OPND(opnd, IL_OPND(intern_list_idx));
   find_opnd_line_and_column(&opnd, &line, &col);

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr) {

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       (IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr ||
        IR_OPR(OPND_IDX(opnd)) == Subscript_Opr)) {

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   if (OPND_FLD(opnd) != AT_Tbl_Idx) {
      /* bad internal unit reference */

      PRINTMSG(line, 1112, Error, col, 
               (stmt_type == Encode_Stmt ? "destination" : "source"),
               (stmt_type == Encode_Stmt ? "ENCODE" : "DECODE"));

      ok = FALSE;
   }
   else if (AT_OBJ_CLASS(OPND_IDX(opnd)) != Data_Obj ||
            (ATD_CLASS(OPND_IDX(opnd)) != Variable &&
             ATD_CLASS(OPND_IDX(opnd)) != Dummy_Argument &&
             ATD_CLASS(OPND_IDX(opnd)) != Function_Result) ||
            ATD_POINTER(OPND_IDX(opnd)) ||
            ATD_ALLOCATABLE(OPND_IDX(opnd))) {

      /* bad internal unit */
      PRINTMSG(line, 1112, Error, col, 
               (stmt_type == Encode_Stmt ? "destination" : "source"),
               (stmt_type == Encode_Stmt ? "ENCODE" : "DECODE"));
      ok = FALSE;
   }
   

   /* put in correct order for pgdcs */

   /******************\
   |* start new list *|
   \******************/

   IR_IDX_L(ir_idx) = cnt_list_idx;
# ifdef _NO_IO_ALTERNATE_RETURN
   IR_LIST_CNT_L(ir_idx) = NUM_PDG_CONTROL_LIST_ITEMS + 3;
# else
   IR_LIST_CNT_L(ir_idx) = NUM_PDG_CONTROL_LIST_ITEMS;
# endif

   /**************************\
   |* 1 - encode/decode flag *|
   \**************************/

   /* this is the cnt opnd */
   IL_PREV_LIST_IDX(cnt_list_idx) = NULL_IDX;

   /*********************\
   |* 2 - eeeflag value *|
   \*********************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(cnt_list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(cnt_list_idx)) = cnt_list_idx;
   list_idx = IL_NEXT_LIST_IDX(cnt_list_idx);

   IL_FLD(list_idx)     = CN_Tbl_Idx;
   IL_IDX(list_idx)     = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   /**********************\
   |* 3 - flflag value   *|
   \**********************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /* This is the flag for split io */
   /* set to FL_IO_SINGLE for now   */

   IL_FLD(list_idx)     = CN_Tbl_Idx;
   IL_IDX(list_idx)     = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, FL_IO_SINGLE);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;


   /**********************\
   |* 4 - UNIT specifier *|
   \**********************/

   IL_NEXT_LIST_IDX(list_idx) = intern_list_idx;
   IL_PREV_LIST_IDX(intern_list_idx) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /***********************\
   |* 5 - IOSTAT variable *|
   \***********************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /**********************\
   |* 6 - REC expression *|
   \**********************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /*************************\
   |* 7 - pre-parsed format *|
   \*************************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /* get pre-parsed from somewhere */

   if (pp_tmp) {
      IL_FLD(list_idx) = AT_Tbl_Idx;
      IL_IDX(list_idx) = pp_tmp;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
   }

   /*********************\
   |* 8 - format source *|
   \*********************/

   IL_NEXT_LIST_IDX(list_idx) = fmt_list_idx;
   IL_PREV_LIST_IDX(fmt_list_idx) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /**************************\
   |* 9 - ADVANCE expression *|
   \**************************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /************************\
   |* 10 - SIZE expression *|
   \************************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;

# ifdef _NO_IO_ALTERNATE_RETURN
   /************************\
   |* 11 - ERR label       *|
   \************************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /************************\
   |* 12 - END label       *|
   \************************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /************************\
   |* 13 - EOR label       *|
   \************************/

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
# endif

   
   /* now do io list */

  
   COPY_OPND(opnd, IR_OPND_R(ir_idx));

   defer_stmt_expansion = TRUE;
   number_of_functions  = 0;
   io_stmt_must_be_split = FALSE;

   if (stmt_type == Decode_Stmt) {
      ok = io_list_semantics(&opnd, Decode);
   }
   else {
      ok = io_list_semantics(&opnd, Encode);
   }

   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   defer_stmt_expansion = FALSE;

   if (ok                       &&
       (number_of_functions > 0 ||
        tree_has_constructor    ||
        io_stmt_must_be_split   ||
        io_item_must_flatten))       {
      process_deferred_io_list();
   }
   else if (ok) {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      gen_runtime_checks(&opnd);
   }

   TRACE (Func_Exit, "encode_decode_stmt_semantics", NULL);

   return;

}  /* encode_decode_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      ADD A DESCRIPTION HERE, Brian.					      *|
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

void endfile_stmt_semantics (void)

{
   int                  ir_idx;
   opnd_type            opnd;
   int                  save_arg_info_list_base;
   int                  save_curr_stmt_sh_idx;
   boolean              semantically_correct;

# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int                  asg_idx;
   int                  br_true_idx;
   int                  col;
   int                  eq_idx;
   int                  line;
   int                  save_next_sh_idx;
# endif


   TRACE (Func_Entry, "endfile_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

# ifndef _NO_IO_ALTERNATE_RETURN
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
# endif

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Endfile, TRUE);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

# ifndef _NO_IO_ALTERNATE_RETURN
   if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {

      line = IR_LINE_NUM(ir_idx);
      col  = IR_COL_NUM(ir_idx);

      alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
      ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_REFERENCED(alt_return_tmp)     = Referenced;
      AT_DEFINED(alt_return_tmp)        = TRUE;
      AT_SEMANTICS_DONE(alt_return_tmp)	= TRUE;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Alt_Return_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = alt_return_tmp;
      IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)      = ir_idx;

      SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;

      if (err_list_idx) {
         NTR_IR_TBL(br_true_idx);
         IR_OPR(br_true_idx)      = Br_True_Opr;
         IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(br_true_idx) = line;
         IR_COL_NUM(br_true_idx)  = col;

         NTR_IR_TBL(eq_idx);
         IR_OPR(eq_idx)           = Eq_Opr;
         IR_TYPE_IDX(eq_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(eq_idx)      = line;
         IR_COL_NUM(eq_idx)       = col;
         IR_FLD_L(eq_idx)         = AT_Tbl_Idx;
         IR_IDX_L(eq_idx)         = alt_return_tmp;
         IR_LINE_NUM_L(eq_idx)    = line;
         IR_COL_NUM_L(eq_idx)     = col;

         IR_FLD_R(eq_idx)         = CN_Tbl_Idx;
         IR_IDX_R(eq_idx)         = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_R(eq_idx)    = line;
         IR_COL_NUM_R(eq_idx)     = col;

         IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
         IR_IDX_L(br_true_idx)    = eq_idx;

         COPY_OPND(IR_OPND_R(br_true_idx), IL_OPND(err_list_idx));

         curr_stmt_sh_idx = save_next_sh_idx;

         gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = br_true_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }
   }
# endif

   if (semantically_correct) {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = final_arg_work(&opnd,
                                            IR_IDX_L(ir_idx),
                                            IR_LIST_CNT_R(ir_idx),
                                            NULL);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);
# if defined(_FILE_IO_OPRS)
      IR_OPR(ir_idx) = Endfile_Opr;
# endif
   }

# ifdef _NO_IO_ALTERNATE_RETURN
   add_alt_return_lbl(ir_idx, err_attr_idx);
# endif

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "endfile_stmt_semantics", NULL);

   return;

}  /* endfile_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void inquire_stmt_semantics (void)

{ 
   int                  asg_idx;
   int			attr_idx;
   int                  col;
   expr_arg_type        exp_desc;
   int			ir_idx;
   int			line;
   int                  list_idx;
   opnd_type		opnd;
   int                  save_arg_info_list_base;
   int                  save_curr_stmt_sh_idx;
   boolean		semantically_correct;
   int			tmp_idx;

# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int                  br_true_idx;
   int                  eq_idx;
   int			save_next_sh_idx;
# endif


   TRACE (Func_Entry, "inquire_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx	= SH_IR_IDX(curr_stmt_sh_idx);
   line		= IR_LINE_NUM(ir_idx);
   col		= IR_COL_NUM(ir_idx);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

   if (IR_OPR(ir_idx) == Call_Opr) {

# ifndef _NO_IO_ALTERNATE_RETURN
      save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
# endif

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = io_ctl_list_semantics(&opnd, Inquire, TRUE);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;

# ifndef _NO_IO_ALTERNATE_RETURN
      if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {


         alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
         ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
         ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
         AT_REFERENCED(alt_return_tmp)     = Referenced;
         AT_DEFINED(alt_return_tmp)        = TRUE;
         AT_SEMANTICS_DONE(alt_return_tmp) = TRUE;

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx)        = Alt_Return_Opr;
         IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx)   = line;
         IR_COL_NUM(asg_idx)    = col;
         IR_LINE_NUM_L(asg_idx) = line;
         IR_COL_NUM_L(asg_idx)  = col;
         IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
         IR_IDX_L(asg_idx)      = alt_return_tmp;
         IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
         IR_IDX_R(asg_idx)      = ir_idx;

         SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;

         if (err_list_idx) {
            NTR_IR_TBL(br_true_idx);
            IR_OPR(br_true_idx)      = Br_True_Opr;
            IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(br_true_idx) = line;
            IR_COL_NUM(br_true_idx)  = col;
   
            NTR_IR_TBL(eq_idx);
            IR_OPR(eq_idx)           = Eq_Opr;
            IR_TYPE_IDX(eq_idx)      = LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(eq_idx)      = line;
            IR_COL_NUM(eq_idx)       = col;
            IR_FLD_L(eq_idx)         = AT_Tbl_Idx;
            IR_IDX_L(eq_idx)         = alt_return_tmp;
            IR_LINE_NUM_L(eq_idx)    = line;
            IR_COL_NUM_L(eq_idx)     = col;

            IR_FLD_R(eq_idx)         = CN_Tbl_Idx;
            IR_IDX_R(eq_idx)         = CN_INTEGER_ONE_IDX;
            IR_LINE_NUM_R(eq_idx)    = line;
            IR_COL_NUM_R(eq_idx)     = col;

            IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
            IR_IDX_L(br_true_idx)    = eq_idx;

            COPY_OPND(IR_OPND_R(br_true_idx), IL_OPND(err_list_idx));

            curr_stmt_sh_idx = save_next_sh_idx;

            gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = br_true_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            curr_stmt_sh_idx = save_curr_stmt_sh_idx;
         }
      }
# endif

      if (semantically_correct) {
         COPY_OPND(opnd, IR_OPND_R(ir_idx));
         semantically_correct = final_arg_work(&opnd,
                                               IR_IDX_L(ir_idx),
                                               IR_LIST_CNT_R(ir_idx),
                                               NULL);
         COPY_OPND(IR_OPND_R(ir_idx), opnd);
         create_io_call_descriptor(ir_idx, Inquire_Desc);
# if defined(_FILE_IO_OPRS)
         IR_OPR(ir_idx) = Inquire_Opr;
# endif
      }

# ifdef _NO_IO_ALTERNATE_RETURN
      add_alt_return_lbl(ir_idx, err_attr_idx);
# endif

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
   else {

      /* have IOLENGTH */

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)             = Asg_Opr;
      IR_TYPE_IDX(asg_idx)        = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)        = IR_LINE_NUM(ir_idx);
      IR_COL_NUM(asg_idx)         = IR_COL_NUM(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      IR_FLD_R(asg_idx)           = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)           = ir_idx;

      /* put in correct order for pgdcs */

      list_idx = IR_IDX_L(ir_idx);

      /**********************\
      |* iolength variable  *|
      \**********************/

      
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state    = CIF_Symbol_Modification;
      semantically_correct = expr_semantics(&opnd, &exp_desc);
      COPY_OPND(IR_OPND_L(asg_idx), opnd);


      if (exp_desc.rank != 0                       ||
          !exp_desc.reference                      ||
          exp_desc.type != Integer                 ||
          exp_desc.linear_type != INTEGER_DEFAULT_TYPE) {

         find_opnd_line_and_column(&opnd, &line, &col);
         PRINTMSG(line, 483, Error, col);
         semantically_correct = FALSE;
      }
      else if (! check_for_legal_define(&opnd)) {
         semantically_correct = FALSE;
      }
      else {

         attr_idx = find_base_attr(&opnd, &line, &col);

         /* create tmp for iolength */

         tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);
         ATD_TYPE_IDX(tmp_idx) = ATD_TYPE_IDX(attr_idx);
         ATD_STOR_BLK_IDX(tmp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
         AT_SEMANTICS_DONE(tmp_idx) = TRUE;

         IR_FLD_L(asg_idx) = AT_Tbl_Idx;
         IR_IDX_L(asg_idx) = tmp_idx;
         IR_LINE_NUM_L(asg_idx) = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_L(asg_idx)  = IR_COL_NUM(ir_idx);


         /* gen the assignment to the iolength variable */

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;

         COPY_OPND(IR_OPND_L(asg_idx), opnd);
         IR_FLD_R(asg_idx) = AT_Tbl_Idx;
         IR_IDX_R(asg_idx) = tmp_idx;
         IR_LINE_NUM_R(asg_idx) = line;
         IR_COL_NUM_R(asg_idx)  = col;

         gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      }


      /******************\
      |* flflag value   *|
      \******************/

      /* This is the flag for split io */
      /* set to FL_IO_SINGLE for now   */


      IL_FLD(list_idx)     = CN_Tbl_Idx;
      IL_IDX(list_idx)     = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, FL_IO_SINGLE);
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      /*******************\
      |* io list is next *|
      \*******************/

      defer_stmt_expansion = TRUE;
      number_of_functions  = 0;
      io_stmt_must_be_split = FALSE;

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = io_list_semantics(&opnd, Inquire) &&
                             semantically_correct;
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      defer_stmt_expansion = FALSE;

      if (semantically_correct     &&
          (number_of_functions > 0 ||
           tree_has_constructor    ||
           io_stmt_must_be_split   ||
           io_item_must_flatten))       {
         process_deferred_io_list();
      }
      else if (semantically_correct) {
         COPY_OPND(opnd, IR_OPND_R(ir_idx));
         gen_runtime_checks(&opnd);
      }
   }

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   TRACE (Func_Exit, "inquire_stmt_semantics", NULL);

   return;

}  /* inquire_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void open_stmt_semantics (void)

{
   int			ir_idx;
   opnd_type		opnd;
   int                  save_arg_info_list_base;
   int                  save_curr_stmt_sh_idx;
   boolean              semantically_correct;

# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int                  asg_idx;
   int                  br_true_idx;
   int                  col;
   int                  eq_idx;
   int			line;
   int                  save_next_sh_idx;
# endif

   
   TRACE (Func_Entry, "open_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

# ifndef _NO_IO_ALTERNATE_RETURN
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
# endif

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Open, TRUE);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

# ifndef _NO_IO_ALTERNATE_RETURN
   if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {

      line = IR_LINE_NUM(ir_idx);
      col  = IR_COL_NUM(ir_idx);

      alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
      ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_REFERENCED(alt_return_tmp)     = Referenced;
      AT_DEFINED(alt_return_tmp)        = TRUE;
      AT_SEMANTICS_DONE(alt_return_tmp) = TRUE;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Alt_Return_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = alt_return_tmp;
      IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)      = ir_idx;

      SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;

      if (err_list_idx) {
         NTR_IR_TBL(br_true_idx);
         IR_OPR(br_true_idx)      = Br_True_Opr;
         IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(br_true_idx) = line;
         IR_COL_NUM(br_true_idx)  = col;

         NTR_IR_TBL(eq_idx);
         IR_OPR(eq_idx)           = Eq_Opr;
         IR_TYPE_IDX(eq_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(eq_idx)      = line;
         IR_COL_NUM(eq_idx)       = col;
         IR_FLD_L(eq_idx)         = AT_Tbl_Idx;
         IR_IDX_L(eq_idx)         = alt_return_tmp;
         IR_LINE_NUM_L(eq_idx)    = line;
         IR_COL_NUM_L(eq_idx)     = col;

         IR_FLD_R(eq_idx)         = CN_Tbl_Idx;
         IR_IDX_R(eq_idx)         = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_R(eq_idx)    = line;
         IR_COL_NUM_R(eq_idx)     = col;

         IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
         IR_IDX_L(br_true_idx)    = eq_idx;

         COPY_OPND(IR_OPND_R(br_true_idx), IL_OPND(err_list_idx));

         curr_stmt_sh_idx = save_next_sh_idx;

         gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = br_true_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }
   }
# endif

   if (semantically_correct) {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = final_arg_work(&opnd,
                                            IR_IDX_L(ir_idx),
                                            IR_LIST_CNT_R(ir_idx),
                                            NULL);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);
      create_io_call_descriptor(ir_idx, Open_Desc);
# if defined(_FILE_IO_OPRS)
      IR_OPR(ir_idx) = Open_Opr;
# endif
   }

# ifdef _NO_IO_ALTERNATE_RETURN
   add_alt_return_lbl(ir_idx, err_attr_idx);
# endif

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "open_stmt_semantics", NULL);

   return;

}  /* open_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void print_stmt_semantics (void)

{
   int			col;
   int                  ir_idx;
   int			line;
   int			list_idx;
   int			loc_idx;
   opnd_type            opnd;
   boolean              semantically_correct;


   TRACE (Func_Entry, "print_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Print, FALSE);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   if (is_namelist) {

      if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(IR_IDX_R(ir_idx)),
                                   &line,
                                   &col);
         PRINTMSG(line, 444, Error, col);
      }

      if (namelist_descriptor_attr) {

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(ir_idx)		= IL_Tbl_Idx;
         IR_LIST_CNT_R(ir_idx)		= 1;
         IR_IDX_R(ir_idx)		= list_idx;
         NTR_IR_TBL(loc_idx);
         IR_OPR(loc_idx)		= Loc_Opr;
         IR_TYPE_IDX(loc_idx)		= CRI_Ptr_8;
         IR_LINE_NUM(loc_idx)           = stmt_start_line;
         IR_COL_NUM(loc_idx)            = stmt_start_col;
         IR_FLD_L(loc_idx)		= AT_Tbl_Idx;
         IR_IDX_L(loc_idx)		= namelist_descriptor_attr;
         IR_LINE_NUM_L(loc_idx)         = stmt_start_line;
         IR_COL_NUM_L(loc_idx)          = stmt_start_col;
         IL_FLD(list_idx)		= IR_Tbl_Idx;
         IL_IDX(list_idx)		= loc_idx;
      }
   }
   else {
      defer_stmt_expansion = TRUE;
      number_of_functions  = 0;
      io_stmt_must_be_split = FALSE;

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = io_list_semantics(&opnd, Print) &&
                             semantically_correct;
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      defer_stmt_expansion = FALSE;

      if (semantically_correct     &&
          (number_of_functions > 0 ||
           tree_has_constructor    ||
           io_stmt_must_be_split   ||
           io_item_must_flatten))       {
         process_deferred_io_list();
      }
      else if (semantically_correct) {
         COPY_OPND(opnd, IR_OPND_R(ir_idx));
         gen_runtime_checks(&opnd);
      }
   }

   TRACE (Func_Exit, "print_stmt_semantics", NULL);

   return;

}  /* print_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void read_stmt_semantics (void)

{
   int                  col;
   int                  ir_idx;
   int                  line;
   int                  list_idx;
   int			loc_idx;
   opnd_type            opnd;
   boolean              semantically_correct;

# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int                  asg_idx;
   int                  br_idx_idx = NULL_IDX;
   int                  br_true_idx;
   int			drop_thru_label_idx;
   int                  jump_out_label;
   int			lab_idx;
   int                  ne_idx;
   int			save_next_sh_idx;
   int			save_curr_stmt_sh_idx;
# endif

   
   TRACE (Func_Entry, "read_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

# ifndef _NO_IO_ALTERNATE_RETURN
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
# endif

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Read, FALSE);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

# ifndef _NO_IO_ALTERNATE_RETURN
   if (have_iostat              ||
       end_list_idx != NULL_IDX ||
       err_list_idx != NULL_IDX ||
       eor_list_idx != NULL_IDX) {

      if (end_list_idx == NULL_IDX ||
          err_list_idx == NULL_IDX ||
          eor_list_idx == NULL_IDX) {

         /* generate a label for drop through branch */

         drop_thru_label_idx = gen_internal_lbl(stmt_start_line);

         curr_stmt_sh_idx = save_next_sh_idx;

         gen_sh(Before, Continue_Stmt, line, col, FALSE, TRUE, TRUE);

         NTR_IR_TBL(lab_idx);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = lab_idx;
         IR_OPR(lab_idx)                          = Label_Opr;
         IR_TYPE_IDX(lab_idx)                     = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(lab_idx)                     = line;
         IR_COL_NUM(lab_idx)                      = col;
         IR_FLD_L(lab_idx)                        = AT_Tbl_Idx;
         IR_IDX_L(lab_idx)                        = drop_thru_label_idx;
         IR_COL_NUM_L(lab_idx)                    = col;
         IR_LINE_NUM_L(lab_idx)                   = line;

         AT_DEFINED(drop_thru_label_idx)       = TRUE;
         ATL_DEF_STMT_IDX(drop_thru_label_idx) = SH_PREV_IDX(curr_stmt_sh_idx);

         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         save_next_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
        
         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }

      alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
      ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_REFERENCED(alt_return_tmp)     = Referenced;
      AT_DEFINED(alt_return_tmp)        = TRUE;
      AT_SEMANTICS_DONE(alt_return_tmp) = TRUE;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Alt_Return_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = alt_return_tmp;

      IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)      = ir_idx;

      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;

      NTR_IR_TBL(br_idx_idx);
      IR_OPR(br_idx_idx) = Br_Index_Opr;
      IR_TYPE_IDX(br_idx_idx) = CG_INTEGER_DEFAULT_TYPE;

      IR_FLD_L(br_idx_idx)            = AT_Tbl_Idx;
      IR_IDX_L(br_idx_idx)            = alt_return_tmp;
      IR_LINE_NUM(br_idx_idx)         = line;
      IR_COL_NUM(br_idx_idx)          = col;
      IR_LINE_NUM_L(br_idx_idx)       = line;
      IR_COL_NUM_L(br_idx_idx)        = col;
      IR_FLD_R(br_idx_idx)            = IL_Tbl_Idx;
      IR_LIST_CNT_R(br_idx_idx)       = 3;

      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_R(br_idx_idx)            = list_idx;

      if (err_list_idx) {
         COPY_OPND(IL_OPND(list_idx), IL_OPND(err_list_idx));
      }
      else {
         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = drop_thru_label_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (end_list_idx) {
         COPY_OPND(IL_OPND(list_idx), IL_OPND(end_list_idx));
      }
      else {
         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = drop_thru_label_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (eor_list_idx) {
         COPY_OPND(IL_OPND(list_idx), IL_OPND(eor_list_idx));
      }
      else {
         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = drop_thru_label_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }

      curr_stmt_sh_idx = save_next_sh_idx;

      gen_sh(Before, If_Stmt, line, col, FALSE, TRUE, TRUE);
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = br_idx_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   }
# endif

   if (is_namelist) {

      if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(IR_IDX_R(ir_idx)),
                                   &line,
                                   &col);
         PRINTMSG(line, 444, Error, col);
      }

      if (namelist_descriptor_attr) {

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(ir_idx)		= IL_Tbl_Idx;
         IR_LIST_CNT_R(ir_idx)		= 1;
         IR_IDX_R(ir_idx)		= list_idx;
         NTR_IR_TBL(loc_idx);
         IR_OPR(loc_idx)		= Loc_Opr;
         IR_TYPE_IDX(loc_idx)		= CRI_Ptr_8;
         IR_LINE_NUM(loc_idx)           = stmt_start_line;
         IR_COL_NUM(loc_idx)            = stmt_start_col;
         IR_FLD_L(loc_idx)		= AT_Tbl_Idx;
         IR_IDX_L(loc_idx)		= namelist_descriptor_attr;
         IR_LINE_NUM_L(loc_idx)         = stmt_start_line;
         IR_COL_NUM_L(loc_idx)          = stmt_start_col;
         IL_FLD(list_idx)		= IR_Tbl_Idx;
         IL_IDX(list_idx)		= loc_idx;
      }
   }
   else {
      defer_stmt_expansion = TRUE;
      number_of_functions  = 0;
      io_stmt_must_be_split = FALSE;

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = io_list_semantics(&opnd, Read) &&
                             semantically_correct;
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      defer_stmt_expansion = FALSE;

# ifndef _NO_IO_ALTERNATE_RETURN
      if (semantically_correct &&
          io_stmt_must_be_split &&
          br_idx_idx != NULL_IDX) {

         /* we have to split the io and we had an alternate return */
         /* so generate the jump out label and the branch true     */

         jump_out_label = gen_internal_lbl(stmt_start_line);

         gen_sh(After, Continue_Stmt, line, col, FALSE, FALSE, TRUE);

         NTR_IR_TBL(lab_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = lab_idx;
         IR_OPR(lab_idx)             = Label_Opr;
         IR_TYPE_IDX(lab_idx)        = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(lab_idx)        = line;
         IR_COL_NUM(lab_idx)         = col;
         IR_FLD_L(lab_idx)           = AT_Tbl_Idx;
         IR_IDX_L(lab_idx)           = jump_out_label;
         IR_COL_NUM_L(lab_idx)       = col;
         IR_LINE_NUM_L(lab_idx)      = line;
         AT_DEFINED(jump_out_label)      = TRUE;
         SH_IR_IDX(curr_stmt_sh_idx)     = lab_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         ATL_DEF_STMT_IDX(jump_out_label) = curr_stmt_sh_idx;

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         NTR_IR_TBL(br_true_idx);
         IR_OPR(br_true_idx)      = Br_True_Opr;
         IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(br_true_idx) = line;
         IR_COL_NUM(br_true_idx)  = col;

         NTR_IR_TBL(ne_idx);
         IR_OPR(ne_idx)           = Ne_Opr;
         IR_TYPE_IDX(ne_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(ne_idx)      = line;
         IR_COL_NUM(ne_idx)       = col;
         IR_FLD_L(ne_idx)         = AT_Tbl_Idx;
         IR_IDX_L(ne_idx)         = alt_return_tmp;
         IR_LINE_NUM_L(ne_idx)    = line;
         IR_COL_NUM_L(ne_idx)     = col;

         IR_FLD_R(ne_idx)         = CN_Tbl_Idx;
         IR_IDX_R(ne_idx)         = CN_INTEGER_ZERO_IDX;
         IR_LINE_NUM_R(ne_idx)    = line;
         IR_COL_NUM_R(ne_idx)     = col;

         IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
         IR_IDX_L(br_true_idx)    = ne_idx;

         IR_FLD_R(br_true_idx) = AT_Tbl_Idx;
         IR_IDX_R(br_true_idx) = jump_out_label;
         IR_LINE_NUM_R(br_true_idx) = line;
         IR_COL_NUM_R(br_true_idx)  = col;

         gen_sh(After, If_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx)     = br_true_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      }
# endif

      if (semantically_correct     &&
          (number_of_functions > 0 ||
           tree_has_constructor    ||
           io_stmt_must_be_split   ||
           io_item_must_flatten))       {
         process_deferred_io_list();
      }
      else if (semantically_correct) {
         COPY_OPND(opnd, IR_OPND_R(ir_idx));
         gen_runtime_checks(&opnd);
      }
   }
   
   TRACE (Func_Exit, "read_stmt_semantics", NULL);

   return;

}  /* read_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void rewind_stmt_semantics (void)

{
   int                  ir_idx;
   opnd_type            opnd;
   int                  save_arg_info_list_base;
   int                  save_curr_stmt_sh_idx;
   boolean              semantically_correct;

# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int                  asg_idx;
   int                  br_true_idx;
   int                  col;
   int                  eq_idx;
   int                  line;
   int			save_next_sh_idx;
# endif


   TRACE (Func_Entry, "rewind_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;

   arg_info_list_base      = arg_info_list_top;

   arg_info_list_top       = arg_info_list_base + IR_LIST_CNT_R(ir_idx);

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

# ifndef _NO_IO_ALTERNATE_RETURN
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
# endif

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Rewind, TRUE);
   COPY_OPND(IR_OPND_R(ir_idx), opnd);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

# ifndef _NO_IO_ALTERNATE_RETURN

   if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {

      line = IR_LINE_NUM(ir_idx);
      col  = IR_COL_NUM(ir_idx);

      alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
      ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_REFERENCED(alt_return_tmp)     = Referenced;
      AT_DEFINED(alt_return_tmp)        = TRUE;
      AT_SEMANTICS_DONE(alt_return_tmp) = TRUE;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Alt_Return_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = alt_return_tmp;
      IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)      = ir_idx;

      SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;

      if (err_list_idx) {
         NTR_IR_TBL(br_true_idx);
         IR_OPR(br_true_idx)      = Br_True_Opr;
         IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(br_true_idx) = line;
         IR_COL_NUM(br_true_idx)  = col;

         NTR_IR_TBL(eq_idx);
         IR_OPR(eq_idx)           = Eq_Opr;
         IR_TYPE_IDX(eq_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(eq_idx)      = line;
         IR_COL_NUM(eq_idx)       = col;
         IR_FLD_L(eq_idx)         = AT_Tbl_Idx;
         IR_IDX_L(eq_idx)         = alt_return_tmp;
         IR_LINE_NUM_L(eq_idx)    = line;
         IR_COL_NUM_L(eq_idx)     = col;

         IR_FLD_R(eq_idx)         = CN_Tbl_Idx;
         IR_IDX_R(eq_idx)         = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_R(eq_idx)    = line;
         IR_COL_NUM_R(eq_idx)     = col;

         IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
         IR_IDX_L(br_true_idx)    = eq_idx;

         COPY_OPND(IR_OPND_R(br_true_idx), IL_OPND(err_list_idx));

         curr_stmt_sh_idx = save_next_sh_idx;

         gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = br_true_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }
   }
# endif

   if (semantically_correct) {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = final_arg_work(&opnd,
                                            IR_IDX_L(ir_idx),
                                            IR_LIST_CNT_R(ir_idx),
                                            NULL);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);
# if defined(_FILE_IO_OPRS)
      IR_OPR(ir_idx) = Rewind_Opr;
# endif
   }

# ifdef _NO_IO_ALTERNATE_RETURN
   add_alt_return_lbl(ir_idx, err_attr_idx);
# endif

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "rewind_stmt_semantics", NULL);

   return;

}  /* rewind_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void write_stmt_semantics (void)

{
   int                  col;
   int                  ir_idx;
   int                  line;
   int                  list_idx;
   int			loc_idx;
   opnd_type            opnd;
   boolean              semantically_correct;

# ifndef _NO_IO_ALTERNATE_RETURN
   int			alt_return_tmp;
   int                  asg_idx;
   int                  br_idx_idx = NULL_IDX;
   int			br_true_idx;
   int                  drop_thru_label_idx;
   int			jump_out_label;
   int                  lab_idx;
   int			ne_idx;
   int                  save_curr_stmt_sh_idx;
   int                  save_next_sh_idx;
# endif

   
   TRACE (Func_Entry, "write_stmt_semantics", NULL);

   SCP_DOES_IO(curr_scp_idx) = TRUE;

# ifndef _NO_IO_ALTERNATE_RETURN
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
# endif

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   semantically_correct = io_ctl_list_semantics(&opnd, Write, FALSE);
   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   line = IR_LINE_NUM(ir_idx);
   col  = IR_COL_NUM(ir_idx);

# ifndef _NO_IO_ALTERNATE_RETURN
   if (have_iostat              ||
       end_list_idx != NULL_IDX ||
       err_list_idx != NULL_IDX ||
       eor_list_idx != NULL_IDX) {

      if (end_list_idx == NULL_IDX ||
          err_list_idx == NULL_IDX ||
          eor_list_idx == NULL_IDX) {

         /* generate a label for drop through branch */

         drop_thru_label_idx = gen_internal_lbl(stmt_start_line);

         curr_stmt_sh_idx = save_next_sh_idx;

         gen_sh(Before, Continue_Stmt, line, col, FALSE, TRUE, TRUE);

         NTR_IR_TBL(lab_idx);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = lab_idx;
         IR_OPR(lab_idx)                          = Label_Opr;
         IR_TYPE_IDX(lab_idx)                     = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(lab_idx)                     = line;
         IR_COL_NUM(lab_idx)                      = col;
         IR_FLD_L(lab_idx)                        = AT_Tbl_Idx;
         IR_IDX_L(lab_idx)                        = drop_thru_label_idx;
         IR_COL_NUM_L(lab_idx)                    = col;
         IR_LINE_NUM_L(lab_idx)                   = line;

         AT_DEFINED(drop_thru_label_idx)       = TRUE;
         ATL_DEF_STMT_IDX(drop_thru_label_idx) = SH_PREV_IDX(curr_stmt_sh_idx);

         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         save_next_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }

      alt_return_tmp                    = gen_compiler_tmp(1, 0, Priv, TRUE);
      ATD_TYPE_IDX(alt_return_tmp)      = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(alt_return_tmp)  = SCP_SB_STACK_IDX(curr_scp_idx);
      AT_REFERENCED(alt_return_tmp)     = Referenced;
      AT_DEFINED(alt_return_tmp)        = TRUE;
      AT_SEMANTICS_DONE(alt_return_tmp) = TRUE;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Alt_Return_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = alt_return_tmp;

      IR_FLD_R(asg_idx)      = IR_Tbl_Idx;
      IR_IDX_R(asg_idx)      = ir_idx;

      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;

      NTR_IR_TBL(br_idx_idx);
      IR_OPR(br_idx_idx) = Br_Index_Opr;
      IR_TYPE_IDX(br_idx_idx) = CG_INTEGER_DEFAULT_TYPE;

      IR_FLD_L(br_idx_idx)            = AT_Tbl_Idx;
      IR_IDX_L(br_idx_idx)            = alt_return_tmp;
      IR_LINE_NUM(br_idx_idx)         = line;
      IR_COL_NUM(br_idx_idx)          = col;
      IR_LINE_NUM_L(br_idx_idx)       = line;
      IR_COL_NUM_L(br_idx_idx)        = col;
      IR_FLD_R(br_idx_idx)            = IL_Tbl_Idx;
      IR_LIST_CNT_R(br_idx_idx)       = 3;

      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_R(br_idx_idx)            = list_idx;

      if (err_list_idx) {
         COPY_OPND(IL_OPND(list_idx), IL_OPND(err_list_idx));
      }
      else {
         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = drop_thru_label_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (end_list_idx) {
         COPY_OPND(IL_OPND(list_idx), IL_OPND(end_list_idx));
      }
      else {
         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = drop_thru_label_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      if (eor_list_idx) {
         COPY_OPND(IL_OPND(list_idx), IL_OPND(eor_list_idx));
      }
      else {
         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = drop_thru_label_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;
      }

      curr_stmt_sh_idx = save_next_sh_idx;

      gen_sh(Before, If_Stmt, line, col, FALSE, TRUE, TRUE);
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = br_idx_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   }
# endif

   if (is_namelist) {

      if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(IR_IDX_R(ir_idx)),
                                   &line,
                                   &col);
         PRINTMSG(line, 444, Error, col);
      }

      if (namelist_descriptor_attr) {

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(ir_idx)		= IL_Tbl_Idx;
         IR_LIST_CNT_R(ir_idx)		= 1;
         IR_IDX_R(ir_idx)		= list_idx;
         NTR_IR_TBL(loc_idx);
         IR_OPR(loc_idx)		= Loc_Opr;
         IR_TYPE_IDX(loc_idx)		= CRI_Ptr_8;
         IR_LINE_NUM(loc_idx)           = stmt_start_line;
         IR_COL_NUM(loc_idx)            = stmt_start_col;
         IR_FLD_L(loc_idx)		= AT_Tbl_Idx;
         IR_IDX_L(loc_idx)		= namelist_descriptor_attr;
         IR_LINE_NUM_L(loc_idx)         = stmt_start_line;
         IR_COL_NUM_L(loc_idx)          = stmt_start_col;
         IL_FLD(list_idx)		= IR_Tbl_Idx;
         IL_IDX(list_idx)		= loc_idx;
      }
   }
   else {
      defer_stmt_expansion = TRUE;
      number_of_functions  = 0;
      io_stmt_must_be_split = FALSE;

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = io_list_semantics(&opnd, Write) &&
                             semantically_correct;
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      defer_stmt_expansion = FALSE;

# ifndef _NO_IO_ALTERNATE_RETURN
      if (semantically_correct &&
          io_stmt_must_be_split &&
          br_idx_idx != NULL_IDX) {

         /* we have to split the io and we had an alternate return */
         /* so generate the jump out label and the branch true     */

         jump_out_label = gen_internal_lbl(stmt_start_line);

         gen_sh(After, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
         
         NTR_IR_TBL(lab_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = lab_idx;
         IR_OPR(lab_idx)             = Label_Opr;
         IR_TYPE_IDX(lab_idx)        = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(lab_idx)        = line;
         IR_COL_NUM(lab_idx)         = col;
         IR_FLD_L(lab_idx)           = AT_Tbl_Idx;
         IR_IDX_L(lab_idx)           = jump_out_label;
         IR_COL_NUM_L(lab_idx)       = col;
         IR_LINE_NUM_L(lab_idx)      = line;
         AT_DEFINED(jump_out_label)      = TRUE;
         SH_IR_IDX(curr_stmt_sh_idx)     = lab_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         ATL_DEF_STMT_IDX(jump_out_label) = curr_stmt_sh_idx;

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         NTR_IR_TBL(br_true_idx);
         IR_OPR(br_true_idx)      = Br_True_Opr;
         IR_TYPE_IDX(br_true_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(br_true_idx) = line;
         IR_COL_NUM(br_true_idx)  = col;

         NTR_IR_TBL(ne_idx);
         IR_OPR(ne_idx)           = Ne_Opr;
         IR_TYPE_IDX(ne_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(ne_idx)      = line;
         IR_COL_NUM(ne_idx)       = col;
         IR_FLD_L(ne_idx)         = AT_Tbl_Idx;
         IR_IDX_L(ne_idx)         = alt_return_tmp;
         IR_LINE_NUM_L(ne_idx)    = line;
         IR_COL_NUM_L(ne_idx)     = col;

         IR_FLD_R(ne_idx)         = CN_Tbl_Idx;
         IR_IDX_R(ne_idx)         = CN_INTEGER_ZERO_IDX;
         IR_LINE_NUM_R(ne_idx)    = line;
         IR_COL_NUM_R(ne_idx)     = col;

         IR_FLD_L(br_true_idx)    = IR_Tbl_Idx;
         IR_IDX_L(br_true_idx)    = ne_idx;

         IR_FLD_R(br_true_idx) = AT_Tbl_Idx;
         IR_IDX_R(br_true_idx) = jump_out_label;
         IR_LINE_NUM_R(br_true_idx) = line;
         IR_COL_NUM_R(br_true_idx)  = col;

         gen_sh(After, If_Stmt, line, col, FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx)     = br_true_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      }
# endif

      if (semantically_correct     &&
          (number_of_functions > 0 ||
           tree_has_constructor    ||
           io_stmt_must_be_split   ||
           io_item_must_flatten))       {
         process_deferred_io_list();
      }
      else if (semantically_correct) {
         COPY_OPND(opnd, IR_OPND_R(ir_idx));
         gen_runtime_checks(&opnd);
      }
   }

   TRACE (Func_Exit, "write_stmt_semantics", NULL);
   
   return;

}  /* write_stmt_semantics */


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

static boolean io_ctl_list_semantics(opnd_type     *list_opnd, 
                                     io_stmt_type   io_type,
                                     boolean        is_call)

{
   int		 attr_idx;
   int		 ciitem_idx;
   int		 col;
   long_type     constant_value;
   int		 err_idx;
   expr_arg_type exp_desc;
   boolean       default_kind;
   boolean       format_expected;
   int		 free_list_idx;
   int		 i;
   int		 info_idx;
   boolean       internal_file = FALSE;
   char		 io_type_string[16];
   int		 k;
   opnd_type     left_opnd;
   int		 line;
   int		 list_array[MAX_NUM_CIITEM + 1];
   int		 list_idx;
   boolean       match;
   boolean       namelist_expected;
   opnd_type	 opnd;
   int		 pp_tmp = NULL_IDX;
   boolean	 semantically_correct = TRUE;
   int		 tmp_idx;

# if ! (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int		 ir_idx;
# endif


   TRACE (Func_Entry, "io_ctl_list_semantics", NULL);

   list_directed = FALSE;

   io_type_string[0] = '\0';
   strcat(io_type_string, io_stmt_str[io_type]);

   end_list_idx = NULL_IDX;
   err_list_idx = NULL_IDX;
   err_attr_idx = NULL_IDX;
   eor_list_idx = NULL_IDX;
   have_iostat  = FALSE;

   if (io_type == Print) {
      io_type = Write;
   }

   if (io_type == Inquire) {
      err_idx = INQ_ERR_IDX;
   }
   else {
      err_idx = ERR_IDX;
   }

   is_namelist = FALSE;

   list_idx = OPND_IDX((*list_opnd));

   info_idx = arg_info_list_base;

   for (i = 1; i <= OPND_LIST_CNT((*list_opnd)); i++) {

      info_idx++;

      list_array[i] = list_idx;

      format_expected   = IL_FORMAT_EXPECTED(list_idx);
      namelist_expected = IL_NAMELIST_EXPECTED(list_idx);

      if (IL_FLD(list_idx) == NO_Tbl_Idx) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         continue;
      }
      else if (IL_FLD(list_idx) == CN_Tbl_Idx &&
               IL_IDX(list_idx) == NULL_IDX) {
         /* had a * for format or unit */

         if (i == 1) {
            /* default unit */

            /* change to NO_Tbl_Idx */
            IL_FLD(list_idx) = NO_Tbl_Idx;
         }
         else if (i == 2) {
           /* list directed io */
            list_directed = TRUE;

         }
# ifdef _DEBUG
         else {
            find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                      &line,
                                      &col);
            PRINTMSG(line, 762, Internal, col);
         }
# endif
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         continue;
      }
      else if (i                == FMT_IDX &&
               IL_FLD(list_idx) == IL_Tbl_Idx) {
         /* this was format character constant inline */
         /* do not send through expr_semantics.       */
         /* first item is format tmp, second is       */
         /* preparsed format tmp.                     */

         pp_tmp = IL_IDX(IL_NEXT_LIST_IDX(IL_IDX(list_idx)));
         FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(IL_IDX(list_idx)));
         free_list_idx = IL_IDX(list_idx);
         COPY_OPND(IL_OPND(list_idx), IL_OPND(IL_IDX(list_idx)));
         FREE_IR_LIST_NODE(free_list_idx);

         ADD_TMP_TO_SHARED_LIST(IL_IDX(list_idx));
         ADD_TMP_TO_SHARED_LIST(pp_tmp);

         list_idx = IL_NEXT_LIST_IDX(list_idx);
         continue;
      }

      ciitem_idx = arg_idx_tbl[io_type][i];

      exp_desc.rank = 0;
      COPY_OPND(opnd, IL_OPND(list_idx));

      if (i == NML_IDX) {
         namelist_illegal = FALSE;
      }

      io_item_must_flatten = FALSE;

      if (ciitem_tbl[io_type].ciitem_list[ciitem_idx].allowed_form
                                                    == Var_Only_Form) {
         xref_state = CIF_Symbol_Modification;
      }
      else if (i                              == FMT_IDX    &&
               IL_FLD(list_idx)               == AT_Tbl_Idx &&
               AT_OBJ_CLASS(IL_IDX(list_idx)) == Label)     {

         xref_state = CIF_No_Usage_Rec;
      }
      else {
         xref_state = CIF_Symbol_Reference;
      }

      if (i == UNIT_IDX) {
         in_call_list = TRUE;
      }

      if (ciitem_tbl[io_type].ciitem_list[ciitem_idx].allowed_form
                                                    == Label_Form ||
          ciitem_tbl[io_type].ciitem_list[ciitem_idx].allowed_form
                                                    == Format_Form) {

         label_allowed = TRUE;
      }

      if (!expr_semantics(&opnd, &exp_desc)) {
         namelist_illegal     = TRUE;
         label_allowed        = FALSE;
         semantically_correct = FALSE;
         list_idx             = IL_NEXT_LIST_IDX(list_idx);
         in_call_list = FALSE;
         continue;
      }

      in_call_list = FALSE;
      label_allowed = FALSE;

      COPY_OPND(IL_OPND(list_idx), opnd);

      namelist_illegal = TRUE;

      if (! is_call) {

         if (io_item_must_flatten ||
             exp_desc.dist_reshape_ref ||
             exp_desc.vector_subscript) {

            tmp_idx = create_tmp_asg(&opnd, &exp_desc, &left_opnd, 
                                     Intent_In, TRUE, FALSE);
            COPY_OPND(IL_OPND(list_idx), left_opnd);
         }
      }
      else  {
         arg_info_list[info_idx]    = init_arg_info;
         arg_info_list[info_idx].ed = exp_desc;
         arg_info_list[info_idx].maybe_modified   = TRUE;
         IL_ARG_DESC_IDX(list_idx)                = info_idx;
      }

      if (exp_desc.rank != 0          &&
          i             != FMT_IDX    &&
          exp_desc.type != Character) {

         find_opnd_line_and_column(&opnd, &line, &col);
         PRINTMSG(line, 449, Error, col,
                  exp_desc.rank,
                  ciitem_tbl[io_type].ciitem_list[ciitem_idx].name);
         semantically_correct = FALSE;
      }

      if (ciitem_tbl[io_type].ciitem_list[ciitem_idx].scalar &&
          exp_desc.rank > 0) {

         find_opnd_line_and_column(&opnd, &line, &col);
         PRINTMSG(line, 1113, Error, col,
                  ciitem_tbl[io_type].ciitem_list[ciitem_idx].name);
         semantically_correct = FALSE;
      }

      switch (ciitem_tbl[io_type].ciitem_list[ciitem_idx].allowed_form) {
         case Exp_Form      :

            match = FALSE;

            for (k = 0; 
                 k < ciitem_tbl[io_type].ciitem_list[ciitem_idx].num_types; 
                 k++){

               if (exp_desc.type == 
                   ciitem_tbl[io_type].ciitem_list[ciitem_idx].
                                                  allowed_types[k]) {
                  match = TRUE;
                  break;
               }
            }

            find_opnd_line_and_column(&opnd, &line, &col);

            if (!match) {
               PRINTMSG(line, 441, Error, col,
                        get_basic_type_str(exp_desc.type_idx),
                        ciitem_tbl[io_type].ciitem_list[ciitem_idx].name,
                        io_type_string);            

               semantically_correct = FALSE;
            }
            else if (exp_desc.type == Typeless) {
           
               if (exp_desc.linear_type == Long_Typeless) {
                  find_opnd_line_and_column(&opnd, &line, &col);
                  PRINTMSG(line, 1133, Error, col);
                  semantically_correct = FALSE;
               }
               else if (exp_desc.linear_type == Short_Typeless_Const) {
                  IL_IDX(list_idx) = cast_typeless_constant(IL_IDX(list_idx),
                                                           INTEGER_DEFAULT_TYPE,
                                                           line,
                                                           col);
                  exp_desc.linear_type = INTEGER_DEFAULT_TYPE;
                  exp_desc.type_idx    = INTEGER_DEFAULT_TYPE;
                  exp_desc.type        = Integer;
                  COPY_OPND(opnd, IL_OPND(list_idx));
               } 
            }
            else if (exp_desc.type                       == Character &&
                     ! exp_desc.constant                              &&
                     ciitem_tbl[io_type].ciitem_list[ciitem_idx].
                                        allowed_types[0] == Integer   &&
                     (io_type == Rewind ||
                      io_type == Backspace ||
                      io_type == Endfile)) {

               /* It is an error to have a character variable UNIT for */
               /* endfile, backspace or rewind.                        */

               PRINTMSG(line, 441, Error, col,
                        get_basic_type_str(exp_desc.type_idx),
                        ciitem_tbl[io_type].ciitem_list[ciitem_idx].name,
                        io_type_string);

               semantically_correct = FALSE;

            }
            else if (exp_desc.type                       == Character &&
                     exp_desc.constant                                &&
                     ciitem_tbl[io_type].ciitem_list[ciitem_idx].
                                        allowed_types[0] == Integer)  { 

               /* This combination of factors assumes we are  */
               /* talking about a character constant UNIT,    */
               /* this is a file name, not internal write.    */
               /* change to Typeless here. check length first */

# ifdef _DEBUG
               if (strcmp(ciitem_tbl[io_type].ciitem_list[ciitem_idx].name,
                           "UNIT") != 0) {
                  PRINTMSG(line, 606, Internal, col);
               }
# endif

              if (compare_cn_and_value(TYP_IDX(exp_desc.type_idx),
                                       TARGET_BYTES_PER_WORD,
                                       Lt_Opr)) {

                  /* BRIANJ - Why convert since it is going back to const */
                  /*          table.  Can we use it directly?             */

                  constant_value = (long_type) CN_INT_TO_C(OPND_IDX(opnd));
                  IL_IDX(list_idx) = ntr_const_tbl(TYPELESS_DEFAULT_TYPE,
                                                   FALSE,
                                                  &constant_value);

                  exp_desc.type = Typeless;
                  exp_desc.linear_type = TYPELESS_DEFAULT_TYPE;
                  exp_desc.type_idx = TYPELESS_DEFAULT_TYPE;

                  PRINTMSG(line, 485, Ansi, col);

                  if (is_call) {
                     arg_info_list[info_idx].ed = exp_desc;
                  }
               }
               else {
                  PRINTMSG(line, 216, Error, col,
                           TARGET_BYTES_PER_WORD);
                  semantically_correct = FALSE;
               }
            }

            if (semantically_correct) {
               
               COPY_OPND(opnd, IL_OPND(list_idx));
               cast_to_cg_default(&opnd, &exp_desc);
               COPY_OPND(IL_OPND(list_idx), opnd);

               if (is_call) {
                  arg_info_list[info_idx].ed = exp_desc;
               }
            }

            if (is_call) {
               arg_info_list[info_idx].maybe_modified = FALSE;
            }
            break;

         case Label_Form    :

            if (i == err_idx) {
               err_list_idx = list_idx;
               err_attr_idx = IL_IDX(list_idx);
            }
            else if (i == END_IDX) {
               end_list_idx = list_idx;
            }
            else if (i == EOR_IDX) {
               eor_list_idx = list_idx;
            }

            if (OPND_FLD(opnd) == AT_Tbl_Idx &&
                AT_OBJ_CLASS(OPND_IDX(opnd)) == Label) {
            }
            else {
               find_opnd_line_and_column(&opnd, &line, &col);
               PRINTMSG(line, 448, Error, col);
               semantically_correct = FALSE;
            }

            break;

         case Namelist_Form :

            /* never be here */
            break;

         case Var_Only_Form :

            find_opnd_line_and_column(&opnd, &line, &col);

            if (exp_desc.reference) {

               default_kind = TRUE;
               switch (exp_desc.type) {
               case Integer   :
                  default_kind = (exp_desc.linear_type == INTEGER_DEFAULT_TYPE);
                  break;

               case Logical   :
                  default_kind = (exp_desc.linear_type == LOGICAL_DEFAULT_TYPE);
                  break;
               case Real      :
                  default_kind = (exp_desc.linear_type == REAL_DEFAULT_TYPE);
                  break;
               case Complex   :
                  default_kind = (exp_desc.linear_type == COMPLEX_DEFAULT_TYPE);
                  break;
               case Character :
                  default_kind = TRUE;
                  break;
               }

               if (exp_desc.type != ciitem_tbl[io_type].ciitem_list[ciitem_idx].
                                                            allowed_types[0]) {
                  PRINTMSG(line, 459, Error, col,
                           get_basic_type_str(exp_desc.type_idx),
                           ciitem_tbl[io_type].ciitem_list[ciitem_idx].name,
                           io_type_string);

                  semantically_correct = FALSE;
               }
               else if (!default_kind) {
                  PRINTMSG(line, 461, Error, col,
                           ciitem_tbl[io_type].ciitem_list[ciitem_idx].name,
                           io_type_string);
                  semantically_correct = FALSE;
               }
               else if (! check_for_legal_define(&opnd)) {
                  semantically_correct = FALSE;
               }
               else {

                  attr_idx = find_left_attr(&opnd);

# if ! (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
                  if ((exp_desc.type == Integer &&
                       storage_bit_size_tbl[exp_desc.linear_type] !=
                       storage_bit_size_tbl[
                                   TYP_LINEAR(CG_INTEGER_DEFAULT_TYPE)]) ||
                      (exp_desc.type == Logical &&
                       storage_bit_size_tbl[exp_desc.linear_type] !=
                       storage_bit_size_tbl[
                                   TYP_LINEAR(CG_LOGICAL_DEFAULT_TYPE)])) {


                     /* must be word size int/logical, else copy out */

                     tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

                     if (exp_desc.type == Integer) {
                        ATD_TYPE_IDX(tmp_idx)     = CG_INTEGER_DEFAULT_TYPE;
                     }
                     else {
                        ATD_TYPE_IDX(tmp_idx)     = CG_LOGICAL_DEFAULT_TYPE;
                     }

                     ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
                     AT_SEMANTICS_DONE(tmp_idx)= TRUE;


                     NTR_IR_TBL(ir_idx);
                     IR_OPR(ir_idx) = Asg_Opr;
                     IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(tmp_idx);
                     IR_LINE_NUM(ir_idx) = line;
                     IR_COL_NUM(ir_idx)  = col;

                     IR_FLD_R(ir_idx) = AT_Tbl_Idx;
                     IR_IDX_R(ir_idx) = tmp_idx;
                     IR_LINE_NUM_R(ir_idx) = line;
                     IR_COL_NUM_R(ir_idx)  = col;

                     COPY_OPND(IR_OPND_L(ir_idx), opnd);
                     gen_sh(After, Assignment_Stmt, line,
                            col, FALSE, FALSE, TRUE);
                     SH_IR_IDX(curr_stmt_sh_idx)     = ir_idx;
                     SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
                     curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

                     IL_FLD(list_idx) = AT_Tbl_Idx;
                     IL_IDX(list_idx) = tmp_idx;
                     IL_LINE_NUM(list_idx) = line;
                     IL_COL_NUM(list_idx)  = col;

                     COPY_OPND(opnd, IL_OPND(list_idx));

                     exp_desc.tmp_reference = TRUE;
                     exp_desc.type_idx = ATD_TYPE_IDX(tmp_idx);
                     exp_desc.linear_type = TYP_LINEAR(ATD_TYPE_IDX(tmp_idx));

                     if (is_call) {
                        arg_info_list[info_idx].ed = exp_desc;
                     }

                  }
# endif
               }
            }
            else { /* must be variable */
               PRINTMSG(line, 460, Error, col,
                        ciitem_tbl[io_type].ciitem_list[ciitem_idx].name,
                        basic_type_str[ciitem_tbl[io_type].
                                      ciitem_list[ciitem_idx].allowed_types[0]],
                        io_type_string);
               semantically_correct = FALSE;
            }
            
            break;

         case Format_Form   :

            /* assume format for now */

            find_opnd_line_and_column(&opnd, &line, &col);

            if (!format_expected                              &&
                OPND_FLD(opnd)               == AT_Tbl_Idx    &&
                AT_OBJ_CLASS(OPND_IDX(opnd)) == Namelist_Grp) {
               is_namelist = TRUE;

               if (io_type == Read) {
                  semantically_correct = do_read_namelist_semantics(&opnd)
                                         && semantically_correct;
               }
               else {
                  do_write_namelist_semantics(&opnd);
               }

               if (ATN_NAMELIST_DESC(OPND_IDX(opnd)) == NULL_IDX) {
                  create_namelist_descriptor(OPND_IDX(opnd));
               }

               namelist_descriptor_attr = ATN_NAMELIST_DESC(OPND_IDX(opnd));

               ADD_TMP_TO_SHARED_LIST(namelist_descriptor_attr);

            }
            else if (namelist_expected) {

               /* must be namelist group, had NML = */

               PRINTMSG(line, 446, Error, col);
               semantically_correct = FALSE;

               is_namelist = TRUE;
            }
            else if (OPND_FLD(opnd)               == AT_Tbl_Idx &&
                     AT_OBJ_CLASS(OPND_IDX(opnd)) == Label)     {

               if (ATL_CLASS(OPND_IDX(opnd)) == Lbl_Format) {
                  /* replace label reference with format constant idx */
                  IL_IDX(list_idx) = ATL_FORMAT_TMP(OPND_IDX(opnd));
                  IL_FLD(list_idx) = AT_Tbl_Idx;
                  IL_LINE_NUM(list_idx) = line;
                  IL_COL_NUM(list_idx)  = col;

                  pp_tmp = ATL_PP_FORMAT_TMP(OPND_IDX(opnd));

                  ADD_TMP_TO_SHARED_LIST(ATL_FORMAT_TMP(OPND_IDX(opnd)));
                  ADD_TMP_TO_SHARED_LIST(ATL_PP_FORMAT_TMP(OPND_IDX(opnd)));
               }

               /* if not a format label larry will have already caught it */
               
            }
            else if (exp_desc.type == Character) {

            }
            else if (exp_desc.rank > 0                                &&
                     (OPND_FLD(opnd)         != IR_Tbl_Idx ||
                      exp_desc.dope_vector                 ||
                      IR_OPR(OPND_IDX(opnd)) != Whole_Subscript_Opr)) {

               /* these are noncontiguous arrays, sections, dope vectors */
               /* error .. format error */

               PRINTMSG(line, 447, Error, col);
               semantically_correct = FALSE;
            }
            else if (exp_desc.type == Integer &&
                     exp_desc.reference)      {

               if (exp_desc.rank == 0) { /* check for ASSIGN */

                  if (!exp_desc.reference) { /* error .. must be variable */
                     PRINTMSG(line, 447, Error, col);
                     semantically_correct = FALSE;
                  }
                  else if (exp_desc.linear_type != INTEGER_DEFAULT_TYPE) {

                     /* must be default kind */

                     PRINTMSG(line, 462, Error, col);
                     semantically_correct = FALSE;
                  }
                  else {

                     attr_idx = find_base_attr(&opnd, &line, &col);

                     if (! ATD_IN_ASSIGN(attr_idx)) {
                        PRINTMSG(line, 1099, Error, col);
                     }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
                     if (ATD_ASSIGN_TMP_IDX(attr_idx) != NULL_IDX) {
                        OPND_FLD(opnd) = AT_Tbl_Idx;
                        OPND_IDX(opnd) = ATD_ASSIGN_TMP_IDX(attr_idx);
                        COPY_OPND(IL_OPND(list_idx), opnd);
                        ADD_TMP_TO_SHARED_LIST(ATD_ASSIGN_TMP_IDX(attr_idx));
                     }
# endif
                  }
               }
               else { /* integer array is nonstandard */
                  PRINTMSG(line, 778, Ansi, col);
               }
            }
            else if ((exp_desc.linear_type == REAL_DEFAULT_TYPE ||
                      exp_desc.type == Logical)                &&
                     exp_desc.reference                        &&
                     exp_desc.rank > 0)                        {
               PRINTMSG(line, 778, Ansi, col);
            }
            else if (exp_desc.type == Typeless &&
                     exp_desc.rank == 0)       {
            
               /* intentionally blank */
               /* ansi msg already issued by lex */
            }
            else { /* error .. format error */
               PRINTMSG(line, 447, Error, col);
               semantically_correct = FALSE;
            }

            break;
      } /* switch */

      /* put checks here that require that the exp_desc be valid */
      /* these checks are done for each list item.               */

      if (io_type == Read || io_type == Write) {

         if (i == UNIT_IDX) {

            if (exp_desc.type == Character &&
                !exp_desc.constant         &&
                exp_desc.reference)        {

               internal_file = TRUE;

               if (io_type == Write) {
                  mark_attr_defined(&opnd);

                  if (! check_for_legal_define(&opnd)) {
                     semantically_correct = FALSE;
                  }
               }

               if (exp_desc.vector_subscript) {
                  find_opnd_line_and_column(&opnd, &line, &col);
                  PRINTMSG(line, 467, Error, col);
                  semantically_correct = FALSE;
               }
               else if (OPND_FLD(opnd) == AT_Tbl_Idx &&
                        ATD_ARRAY_IDX(OPND_IDX(opnd)) != NULL_IDX &&
                        BD_ARRAY_CLASS(ATD_ARRAY_IDX(OPND_IDX(opnd))) ==
                                                     Assumed_Size) {

                  PRINTMSG(line, 1302, Ansi, col);
               }
            }
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   } /* end of for loop for list items */

   /* put checks here that can be done after all list items */
   /* are processed. They are only done once.               */

   if (internal_file && is_namelist) {
      find_opnd_line_and_column((opnd_type *) &IL_OPND(list_array[FMT_IDX]),
                                &line, &col);
      PRINTMSG(line, 472, Error, col);
      semantically_correct = FALSE;
   }

   if (is_call) {
      /* copy list_array to arg_list */
      for (k = 1; k <= OPND_LIST_CNT((*list_opnd)); k++) {
         arg_list[k]          = list_array[k];
      }
   }
   else {
      /* read, write, print */

      /* If PURE/ELEMENTAL subprogram, can only read/write to internal file. */

      if (!internal_file && 
           (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
            ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) &&
          (io_type == Read || io_type == Write)) {
#ifdef KEY
         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_array[FMT_IDX]),
                                   &line, &col);
#endif /* KEY */
         PRINTMSG(line, 1263, Error, col, 
                  io_type == Read ? "READ" : "WRITE",
                  ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))?"elemental":"pure",
                  AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
         semantically_correct = FALSE;
      }


      if (internal_file) {
      }

      /* if had advance specifier => check this stuff */

      if (IL_FLD(list_array[ADVANCE_IDX]) != NO_Tbl_Idx) {

         if (IL_FLD(list_array[FMT_IDX]) == NO_Tbl_Idx || is_namelist) {
            find_opnd_line_and_column((opnd_type *) 
                                      &IL_OPND(list_array[ADVANCE_IDX]),
                                      &line, &col);
            PRINTMSG(line, 468, Error, col);
            semantically_correct = FALSE;
         }
         else if (list_directed) {
            find_opnd_line_and_column((opnd_type *) 
                                      &IL_OPND(list_array[FMT_IDX]),
                                      &line, &col);
            PRINTMSG(line, 469, Error, col);
            semantically_correct = FALSE;
         }

         if (internal_file) {
            find_opnd_line_and_column((opnd_type *) 
                                      &IL_OPND(list_array[UNIT_IDX]),
                                      &line, &col);
            PRINTMSG(line, 470, Error, col);
            semantically_correct = FALSE;
         }
      }

      /* if REC specifier is present => check this stuff */

      if (IL_FLD(list_array[REC_IDX]) != NO_Tbl_Idx) {

         if (internal_file) {
            find_opnd_line_and_column((opnd_type *) 
                                      &IL_OPND(list_array[REC_IDX]),
                                      &line, &col);
            PRINTMSG(line, 471, Error, col);
            semantically_correct = FALSE;
         }
         else if (is_namelist) {
            find_opnd_line_and_column((opnd_type *) 
                                      &IL_OPND(list_array[FMT_IDX]),
                                      &line, &col);
            PRINTMSG(line, 466, Error, col,
                     io_type_string);
            semantically_correct = FALSE;
         }
      }


      if (is_namelist) {
         /* change opr */
         IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) = (operator_type)
                                      (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) + 2);
      }
      else if (IL_FLD(list_array[FMT_IDX]) == NO_Tbl_Idx) {
         /* unformatted */
         IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) = (operator_type)
                                      (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) + 1);
      }

      if (IL_FLD(list_array[IOSTAT_IDX]) != NO_Tbl_Idx) {
         have_iostat = TRUE;
      }

      /******************\
      |* start new list *|
      \******************/

      NTR_IR_LIST_TBL(list_idx);
      OPND_IDX((*list_opnd))      = list_idx;

# ifdef _NO_IO_ALTERNATE_RETURN
      OPND_LIST_CNT((*list_opnd)) = NUM_PDG_CONTROL_LIST_ITEMS + 3;
# else
      OPND_LIST_CNT((*list_opnd)) = NUM_PDG_CONTROL_LIST_ITEMS;
# endif

      /**************************\
      |* 1 - encode/decode flag *|
      \**************************/

      IL_FLD(list_idx)     = CN_Tbl_Idx;
      IL_IDX(list_idx)     = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = stmt_start_line;
      IL_COL_NUM(list_idx)  = stmt_start_col;

      /*********************\
      |* 2 - eeeflag value *|
      \*********************/

      constant_value = 0;
      constant_value |= (IL_FLD(list_array[err_idx]) != NO_Tbl_Idx) ?
                                 ERR_IS_PRESENT : 0;
      constant_value |= (IL_FLD(list_array[END_IDX]) != NO_Tbl_Idx) ?
                                 END_IS_PRESENT : 0;
      constant_value |= (IL_FLD(list_array[EOR_IDX]) != NO_Tbl_Idx) ?
                                 EOR_IS_PRESENT : 0;
      constant_value |= (IL_FLD(list_array[IOSTAT_IDX]) != NO_Tbl_Idx) ?
                                 IOSTAT_IS_PRESENT : 0;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, constant_value);
      IL_LINE_NUM(list_idx) = stmt_start_line;
      IL_COL_NUM(list_idx)  = stmt_start_col;

      /**********************\
      |* 3 - flflag value   *|
      \**********************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      /* This is the flag for split io */
      /* set to FL_IO_SINGLE for now   */

      constant_value       = FL_IO_SINGLE;

      IL_FLD(list_idx)     = CN_Tbl_Idx;
      IL_IDX(list_idx)     = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, FL_IO_SINGLE);
      IL_LINE_NUM(list_idx) = stmt_start_line;
      IL_COL_NUM(list_idx)  = stmt_start_col;


      /**********************\
      |* 4 - UNIT specifier *|
      \**********************/

      IL_NEXT_LIST_IDX(list_idx) = list_array[UNIT_IDX];
      IL_PREV_LIST_IDX(list_array[UNIT_IDX]) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      /***********************\
      |* 5 - IOSTAT variable *|
      \***********************/

      IL_NEXT_LIST_IDX(list_idx) = list_array[IOSTAT_IDX];
      IL_PREV_LIST_IDX(list_array[IOSTAT_IDX]) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      /**********************\
      |* 6 - REC expression *|
      \**********************/

      IL_NEXT_LIST_IDX(list_idx) = list_array[REC_IDX];
      IL_PREV_LIST_IDX(list_array[REC_IDX]) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      
      /*************************\
      |* 7 - pre-parsed format *|
      \*************************/

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      /* get pre-parsed from somewhere */

      if (! is_namelist    && 
          ! list_directed) {

         if (pp_tmp) {
            IL_FLD(list_idx) = AT_Tbl_Idx;
            IL_IDX(list_idx) = pp_tmp;
            IL_LINE_NUM(list_idx) = stmt_start_line;
            IL_COL_NUM(list_idx)  = stmt_start_col;
         }
      }

      /*********************\
      |* 8 - format source *|
      \*********************/

      IL_NEXT_LIST_IDX(list_idx) = list_array[FMT_IDX];
      IL_PREV_LIST_IDX(list_array[FMT_IDX]) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      
      if (list_directed)
      {
         IL_OPND(list_idx) = null_opnd;
      }

      /**************************\
      |* 9 - ADVANCE expression *|
      \**************************/

      IL_NEXT_LIST_IDX(list_idx) = list_array[ADVANCE_IDX];
      IL_PREV_LIST_IDX(list_array[ADVANCE_IDX]) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      /************************\
      |* 10 - SIZE expression *|
      \************************/

      IL_NEXT_LIST_IDX(list_idx) = list_array[SIZE_IDX];
      IL_PREV_LIST_IDX(list_array[SIZE_IDX]) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;

# ifdef _NO_IO_ALTERNATE_RETURN
      /************************\
      |* 11 - ERR label       *|
      \************************/

      if (err_list_idx == NULL_IDX) {
         NTR_IR_LIST_TBL(err_list_idx);
      }

      IL_NEXT_LIST_IDX(list_idx) = err_list_idx;
      IL_PREV_LIST_IDX(err_list_idx) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      /************************\
      |* 12 - END label       *|
      \************************/

      if (end_list_idx == NULL_IDX) {
         NTR_IR_LIST_TBL(end_list_idx);
      }

      IL_NEXT_LIST_IDX(list_idx) = end_list_idx;
      IL_PREV_LIST_IDX(end_list_idx) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      /************************\
      |* 13 - EOR label       *|
      \************************/

      if (eor_list_idx == NULL_IDX) {
         NTR_IR_LIST_TBL(eor_list_idx);
      }

      IL_NEXT_LIST_IDX(list_idx) = eor_list_idx;
      IL_PREV_LIST_IDX(eor_list_idx) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
# endif

   }

   TRACE (Func_Exit, "io_ctl_list_semantics", NULL);

   return(semantically_correct);

}  /* io_ctl_list_semantics */
#ifdef KEY /* Bug 2611 */

/*
 * Whe constructing an Io_Item_Type_Code_Opr, the Cray FE ordinarily uses the
 * type which appears in the tree of IR_Tbl_Idx and AT_Tbl_Idx nodes. For a
 * handful of character-related intrinsics returning integers, when -i8 is in
 * effect, the type seen by the Cray FE is integer*8 but the WHIRL op emitted
 * by the SGI portion of the FE is integer*4. When the intrinsic is called as
 * an operand, or as the RHS of a variable, or as the actual value of a dummy
 * argument of type integer, this isn't a problem, because the SGI code emits
 * conversions as a matter of course, and that code ignores the type in the
 * Cray tree, but pays attention to the type of the WHIRL intrinsic op.
 *
 * But there are no conversions when generating WHIRL for an I/O list item,
 * because a dope constant is generated to describe the type of the item to
 * the runtime system.
 *
 * If we generate the wrong type in the Io_Item_Type_Code_Opr based on the
 * type in the Cray tree, the Cray FE generates the wrong dope constant. So,
 * although it's horrible to hard-wire a list of special cases here, there's
 * no good alternative: the SGI world doesn't know how to map an
 * Io_Item_Type_Code_Opr onto a dope constant, and the Cray world doesn't know
 * what WHIRL intrinsic op will be generated (nor does it know how to discover
 * the type of a WHIRL node.)
 *
 * See the call to WNRTY(wn) in cwh_convert_to_ty() for a demonstration of
 * how type conversions gets generated in expressions or assignments.
 *
 * opnd		operand which is an IO list item
 * exp_desc	description of that operand
 * returns	type to use in constructing Io_Item_Type_Code_Opr
 */
static Uint
intrinsic_special_case(opnd_type *opnd, expr_arg_type *exp_desc) {
  if (opnd->fld == IR_Tbl_Idx) {
    operator_type op = IR_OPR(opnd->idx);
    if (op == Len_Trim_Opr || op == Index_Opr || op == Scan_Opr ||
      op == Verify_Opr) {
      return CG_INTEGER_DEFAULT_TYPE;
      }
    }
  return exp_desc->type_idx;
  }
#endif /* KEY Bug 2611 */

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

static boolean io_list_semantics(opnd_type     *top_opnd, 
                                 io_stmt_type   io_type)

{
   int			asg_idx;
   int			attr_idx;
   int			cnt;
   int			col;
   boolean              do_var_ok;
   expr_arg_type        exp_desc;
   expr_arg_type        lcv_exp_desc;
   expr_arg_type        start_exp_desc;
   expr_arg_type        end_exp_desc;
   expr_arg_type        inc_exp_desc;
   boolean              have_seen_must_flatten = FALSE;
   boolean		have_seen_constructor = FALSE;
   int			imp_idx;
   int			line;
#ifdef KEY /* Bug 10177 */
   int                  list_idx = 0;
#else /* KEY Bug 10177 */
   int                  list_idx;
#endif /* KEY Bug 10177 */
   int                  list2_idx;
   boolean              needs_expansion = FALSE;
   int			new_do_var_idx;
   opnd_type            opnd;
   boolean		save_in_implied_do;
   boolean		semantically_correct = TRUE;
   int			struct_list_idx;
   long_type		the_constant;
   int			type_idx;


   TRACE (Func_Entry, "io_list_semantics", NULL);

   if (OPND_FLD((*top_opnd)) == NO_Tbl_Idx) {
      goto EXIT;
   }
   if (OPND_FLD((*top_opnd)) == IL_Tbl_Idx) {
      list_idx = OPND_IDX((*top_opnd));
   }
   else {
      find_opnd_line_and_column(top_opnd, &line, &col);
      PRINTMSG(line, 637, Internal, col);
   }

   io_item_must_flatten = FALSE;
   tree_has_constructor = FALSE;

# ifdef _THREE_CALL_IO
   io_stmt_must_be_split = TRUE;
   three_call_model = TRUE;
# endif

   while (list_idx != NULL_IDX) {

      IL_HAS_FUNCTIONS(list_idx) = FALSE;
      IL_MUST_BE_LOOP(list_idx)  = FALSE;

      if (IL_FLD(list_idx)         == IR_Tbl_Idx      &&
          IR_OPR(IL_IDX(list_idx)) == Implied_Do_Opr) {

# ifdef _THREE_CALL_IO
         IL_MUST_BE_LOOP(list_idx) = TRUE;
# endif

         /* skip do variable processing until the control values are done. */

         /***********************\
         |* do do initial value *|
         \***********************/

         list2_idx = IL_NEXT_LIST_IDX(IR_IDX_R(IL_IDX(list_idx)));

         COPY_OPND(opnd, IL_OPND(list2_idx));
         start_exp_desc.rank = 0;
         number_of_functions = 0;
         xref_state          = CIF_Symbol_Reference;
         semantically_correct = expr_semantics(&opnd, &start_exp_desc) &&
                                semantically_correct;
         COPY_OPND(IL_OPND(list2_idx), opnd);

         if (item_has_bounds_chk(&opnd)) {
            number_of_functions++;
         }

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list2_idx) = TRUE;
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
            needs_expansion             = TRUE;

            if (io_type == Read &&
                list_idx != OPND_IDX((*top_opnd))) {
               io_stmt_must_be_split       = TRUE;
            }
         }
         else {
            IL_HAS_FUNCTIONS(list2_idx) = FALSE;
         }

         find_opnd_line_and_column(&opnd, &line, &col);

         if (start_exp_desc.rank != 0) {
            PRINTMSG(line, 476, Error, col);
            semantically_correct = FALSE;
         }

         if (start_exp_desc.linear_type == Long_Typeless) {
            PRINTMSG(line, 1133, Error, col);
            semantically_correct = FALSE;
         }
         else if (start_exp_desc.type != Integer                      &&
                  start_exp_desc.type != Typeless                     &&
                  (start_exp_desc.type != Real || 
                   (start_exp_desc.linear_type != REAL_DEFAULT_TYPE    &&
                    start_exp_desc.linear_type != DOUBLE_DEFAULT_TYPE))) {

            PRINTMSG(line, 477, Error, col);
            semantically_correct = FALSE;
         }
         else if (start_exp_desc.type == Real) {
            PRINTMSG(line, 943,
#ifdef KEY /* Bug 318, 321 */
	      Ansi,
#else /* KEY Bug 318, 321 */
	      Comment,
#endif /* KEY Bug 318, 321 */
	      col);
         }

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
         COPY_OPND(opnd, IL_OPND(list2_idx));
         cast_to_cg_default(&opnd, &start_exp_desc);
         COPY_OPND(IL_OPND(list2_idx), opnd);
#endif /* KEY Bug 4709 */

         /************************\
         |* do do terminal value *|
         \************************/

         list2_idx = IL_NEXT_LIST_IDX(list2_idx);

         COPY_OPND(opnd, IL_OPND(list2_idx));
         end_exp_desc.rank = 0;
         number_of_functions = 0;
         xref_state          = CIF_Symbol_Reference;
         semantically_correct = expr_semantics(&opnd, &end_exp_desc) &&
                                semantically_correct;
         COPY_OPND(IL_OPND(list2_idx), opnd);

         if (item_has_bounds_chk(&opnd)) {
            number_of_functions++;
         }

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list2_idx) = TRUE;
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
            needs_expansion             = TRUE;

            if (io_type == Read &&
                list_idx != OPND_IDX((*top_opnd))) {
               io_stmt_must_be_split       = TRUE;
            }
         }
         else {
            IL_HAS_FUNCTIONS(list2_idx) = FALSE;
         }

         find_opnd_line_and_column(&opnd, &line, &col);

         if (end_exp_desc.rank != 0) {
            PRINTMSG(line, 476, Error, col);
            semantically_correct = FALSE;
         }

         if (end_exp_desc.linear_type == Long_Typeless) {
            PRINTMSG(line, 1133, Error, col);
            semantically_correct = FALSE;
         }
         else if (end_exp_desc.type != Integer                      &&
                  end_exp_desc.type != Typeless                     &&
                  (end_exp_desc.type != Real ||
                   (end_exp_desc.linear_type != REAL_DEFAULT_TYPE    &&
                    end_exp_desc.linear_type != DOUBLE_DEFAULT_TYPE))) {

            PRINTMSG(line, 477, Error, col);
            semantically_correct = FALSE;
         }
         else if (end_exp_desc.type == Real) {
            PRINTMSG(line, 943,
#ifdef KEY /* Bug 318, 321 */
	      Ansi,
#else /* KEY Bug 318, 321 */
	      Comment,
#endif /* KEY Bug 318, 321 */
	      col);
         }

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
         COPY_OPND(opnd, IL_OPND(list2_idx));
         cast_to_cg_default(&opnd, &end_exp_desc);
         COPY_OPND(IL_OPND(list2_idx), opnd);
#endif /* KEY */


         /********************************\
         |* do do stride if there is one *|
         \********************************/

         if (IL_NEXT_LIST_IDX(list2_idx) != NULL_IDX) {
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            COPY_OPND(opnd, IL_OPND(list2_idx));
            inc_exp_desc.rank = 0;
            number_of_functions = 0;
            xref_state          = CIF_Symbol_Reference;
            semantically_correct = expr_semantics(&opnd, &inc_exp_desc) &&
                                   semantically_correct;
            COPY_OPND(IL_OPND(list2_idx), opnd);

            if (item_has_bounds_chk(&opnd)) {
               number_of_functions++;
            }

            if (number_of_functions > 0) {
               IL_HAS_FUNCTIONS(list2_idx) = TRUE;
               IL_HAS_FUNCTIONS(list_idx)  = TRUE;
               needs_expansion             = TRUE;

               if (io_type == Read &&
                   list_idx != OPND_IDX((*top_opnd))) {
                  io_stmt_must_be_split       = TRUE;
               }
            }
            else {
               IL_HAS_FUNCTIONS(list2_idx) = FALSE;
            }

            find_opnd_line_and_column(&opnd, &line, &col);

            if (inc_exp_desc.rank != 0) {
               PRINTMSG(line, 476, Error, col);
               semantically_correct = FALSE;
            }

            if (inc_exp_desc.linear_type == Long_Typeless) {
               PRINTMSG(line, 1133, Error, col);
               semantically_correct = FALSE;
            }
            else if (inc_exp_desc.type != Integer                      &&
                     inc_exp_desc.type != Typeless                     &&
                     (inc_exp_desc.type != Real ||
                      (inc_exp_desc.linear_type != REAL_DEFAULT_TYPE    &&
                       inc_exp_desc.linear_type != DOUBLE_DEFAULT_TYPE))) {

               PRINTMSG(line, 477, Error, col);
               semantically_correct = FALSE;
            }
            else if (inc_exp_desc.type == Real) {
               PRINTMSG(line, 943,
#ifdef KEY /* Bug 318, 321 */
	         Ansi,
#else /* KEY Bug 318, 321 */
	         Comment,
#endif /* KEY Bug 318, 321 */
		 col);
            }

            if (semantically_correct &&
                OPND_FLD(opnd) == CN_Tbl_Idx) {

               type_idx = CG_LOGICAL_DEFAULT_TYPE;

               semantically_correct &= 
                   folder_driver((char *)&CN_CONST(OPND_IDX(opnd)),
                                 inc_exp_desc.type_idx,
                                 (char *)&CN_CONST(CN_INTEGER_ZERO_IDX),
                                 CG_INTEGER_DEFAULT_TYPE,
                                 &the_constant,
                                 &type_idx,
                                 line,
                                 col,
                                 2,
                                 Eq_Opr);

               if (THIS_IS_TRUE(&the_constant, type_idx)) {
                  PRINTMSG(line, 1084, Error, col);
                  semantically_correct = FALSE;
               }
            }

#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
            COPY_OPND(opnd, IL_OPND(list2_idx));
            cast_to_cg_default(&opnd, &inc_exp_desc);
            COPY_OPND(IL_OPND(list2_idx), opnd);
#endif /* KEY */

         }
         else {
            /* fill in default stride here */
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            IR_LIST_CNT_R(IL_IDX(list_idx))++;
            IL_FLD(list2_idx) = CN_Tbl_Idx;
            IL_IDX(list2_idx) = CN_INTEGER_ONE_IDX;
            IL_LINE_NUM(list2_idx) = stmt_start_line;
            IL_COL_NUM(list2_idx)  = stmt_start_col;

            inc_exp_desc = init_exp_desc;

            inc_exp_desc.type_idx    = CN_TYPE_IDX(CN_INTEGER_ONE_IDX);
            inc_exp_desc.linear_type = TYP_LINEAR(inc_exp_desc.type_idx);
            inc_exp_desc.type        = TYP_TYPE(inc_exp_desc.type_idx);
         }

         /**************************\
         |* do do control variable *|
         \**************************/


         list2_idx = IR_IDX_R(IL_IDX(list_idx));

         do_var_ok = TRUE;

         COPY_OPND(opnd, IL_OPND(list2_idx));
         lcv_exp_desc.rank = 0;
         number_of_functions = 0;
         xref_state          = CIF_Symbol_Modification;
         save_in_implied_do = in_implied_do;
         in_implied_do      = FALSE;
         do_var_ok = expr_semantics(&opnd, &lcv_exp_desc);
         COPY_OPND(IL_OPND(list2_idx), opnd);
         in_implied_do = save_in_implied_do;


         /* For CIF purposes, mark the LCV Attr as being used as an I/O       */
         /* implied-DO so that if it appears nowhere else, CIF will still     */
         /* generate an Object record for it.				      */

         attr_idx = find_left_attr(&opnd);

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_AS_IO_LCV(attr_idx) = TRUE;
         }


         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list2_idx) = TRUE;
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
            needs_expansion             = TRUE;
         }
         else {
            IL_HAS_FUNCTIONS(list2_idx) = FALSE;
         }

/* BHJ JLS LRR ... need interpretation for this one. imp do var must be */
/* "named" scalar variable, not sub-object.                             */

         find_opnd_line_and_column(&opnd, &line, &col);

         if (!lcv_exp_desc.reference) {
            PRINTMSG(line, 481, Error, col);
            do_var_ok = FALSE;
         }
         else { 

            if (lcv_exp_desc.rank != 0) {
               PRINTMSG(line, 482, Error, col);
               do_var_ok = FALSE;
            }

            if (lcv_exp_desc.type != Integer &&
                (lcv_exp_desc.type != Real || 
                 (lcv_exp_desc.linear_type != REAL_DEFAULT_TYPE &&
                  lcv_exp_desc.linear_type != DOUBLE_DEFAULT_TYPE))) {

               PRINTMSG(line, 474, Error, col);
               do_var_ok = FALSE;
            }
            else if (lcv_exp_desc.type == Real) {
               IL_MUST_BE_LOOP(list_idx)   = TRUE;
               io_stmt_must_be_split       = TRUE;
               PRINTMSG(line, 1569, Ansi, col);
            }

            if (do_var_ok                                    &&
                OPND_FLD(opnd) != AT_Tbl_Idx                 &&
                (OPND_FLD(opnd) != IR_Tbl_Idx           ||
                 IR_OPR(OPND_IDX(opnd)) != Dv_Deref_Opr ||
                 IR_FLD_L(OPND_IDX(opnd)) != AT_Tbl_Idx))    {

               PRINTMSG(line, 530, Comment, col);
               do_var_ok = FALSE;
            }

            if (do_var_ok) {

               if (! check_for_legal_define(&opnd)) {
                  do_var_ok = FALSE;
               }
            }
         }

         NTR_IR_LIST_TBL(imp_idx);
         IL_NEXT_LIST_IDX(imp_idx) = imp_do_var_list;
         imp_do_var_list           = imp_idx;

         if (do_var_ok) {
            imp_idx = IL_NEXT_LIST_IDX(imp_idx);

            while (imp_idx) {

               if (OPND_IDX(opnd) == IL_IDX(imp_idx)) {
                  PRINTMSG(line, 533, Error, col,
                           AT_OBJ_NAME_PTR(OPND_IDX(opnd)));
                  do_var_ok = FALSE;
                  break;
               }

               imp_idx = IL_NEXT_LIST_IDX(imp_idx);
            }
         
            if (do_var_ok) {
               COPY_OPND(IL_OPND(imp_do_var_list), opnd);
            }
         }


         semantically_correct = semantically_correct && do_var_ok;


         /***********************\
         |* do list of io items *|
         \***********************/

         in_implied_do = TRUE;
         COPY_OPND(opnd, IR_OPND_L(IL_IDX(list_idx)));
         number_of_functions = 0;
         semantically_correct = io_list_semantics(&opnd, io_type) &&
                                semantically_correct;
         COPY_OPND(IR_OPND_L(IL_IDX(list_idx)), opnd);

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
            IL_MUST_BE_LOOP(list_idx)   = TRUE;
            io_stmt_must_be_split       = TRUE;
            needs_expansion             = TRUE;
         }
         
         if (io_item_must_flatten) {
            IL_MUST_BE_LOOP(list_idx)   = TRUE;
            io_stmt_must_be_split       = TRUE;
            have_seen_must_flatten      = TRUE;
         }

         if (tree_has_constructor) {
            IL_MUST_BE_LOOP(list_idx)   = TRUE;
            io_stmt_must_be_split       = TRUE;
            have_seen_constructor       = TRUE;
         }

         /* take imp_do var of list */
         imp_idx         = imp_do_var_list;
         imp_do_var_list = IL_NEXT_LIST_IDX(imp_idx);
         FREE_IR_LIST_NODE(imp_idx);
//Bug# 2521: the compiler should not generate a temp whose size is smaller than that of the loop index. 
# ifdef KEY
         if (do_var_ok &&
             storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))] < 
                storage_bit_size_tbl[TYP_LINEAR(CG_INTEGER_DEFAULT_TYPE)] &&
             ! IL_MUST_BE_LOOP(list_idx)) {
# else
         if (do_var_ok &&
             storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))] !=
                storage_bit_size_tbl[TYP_LINEAR(CG_INTEGER_DEFAULT_TYPE)] &&
             ! IL_MUST_BE_LOOP(list_idx)) {
# endif

            new_do_var_idx = gen_compiler_tmp(stmt_start_line, stmt_start_col,
                                              Priv, TRUE);

            AT_SEMANTICS_DONE(new_do_var_idx)= TRUE;
            ATD_TYPE_IDX(new_do_var_idx)     = CG_INTEGER_DEFAULT_TYPE;
            ATD_STOR_BLK_IDX(new_do_var_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
            AT_CIF_SYMBOL_ID(new_do_var_idx) = AT_CIF_SYMBOL_ID(attr_idx);

            lcv_exp_desc.type_idx    = ATD_TYPE_IDX(new_do_var_idx);
            lcv_exp_desc.type        = TYP_TYPE(lcv_exp_desc.type_idx);
            lcv_exp_desc.linear_type = TYP_LINEAR(lcv_exp_desc.type_idx);

            AT_ATTR_LINK(attr_idx)           = new_do_var_idx;
            AT_IGNORE_ATTR_LINK(attr_idx)    = TRUE;

            ATD_IMP_DO_LCV(new_do_var_idx)   = TRUE;

            IL_NONDEFAULT_IMP_DO_LCV(list_idx) = TRUE;
            io_stmt_must_be_split              = TRUE;
            needs_expansion                    = TRUE;

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(attr_idx);
            IR_LINE_NUM(asg_idx) = stmt_start_line;
            IR_COL_NUM(asg_idx)  = stmt_start_col;
            IR_FLD_L(asg_idx) = AT_Tbl_Idx;
            IR_IDX_L(asg_idx) = attr_idx;
            IR_LINE_NUM_L(asg_idx) = stmt_start_line;
            IR_COL_NUM_L(asg_idx)  = stmt_start_col;
            IR_FLD_R(asg_idx) = AT_Tbl_Idx;
            IR_IDX_R(asg_idx) = new_do_var_idx;
            IR_LINE_NUM_R(asg_idx) = stmt_start_line;
            IR_COL_NUM_R(asg_idx)  = stmt_start_col;

            IL_FLD(IR_IDX_R(IL_IDX(list_idx))) = IR_Tbl_Idx;
            IL_IDX(IR_IDX_R(IL_IDX(list_idx))) = asg_idx;

            in_implied_do = TRUE;
            COPY_OPND(opnd, IR_OPND_L(IL_IDX(list_idx)));
            semantically_correct = io_list_semantics(&opnd, io_type) &&
                                   semantically_correct;
            COPY_OPND(IR_OPND_L(IL_IDX(list_idx)), opnd);
         }

         if (semantically_correct &&
             lcv_exp_desc.type == Integer) {

            /* Check start, end, and increment to make */
            /* sure they are the same type as lcv.     */

            list2_idx = IL_NEXT_LIST_IDX(IR_IDX_R(IL_IDX(list_idx)));

            COPY_OPND(opnd, IL_OPND(list2_idx));
            cast_to_type_idx(&opnd, &start_exp_desc, lcv_exp_desc.type_idx);
            COPY_OPND(IL_OPND(list2_idx), opnd);

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);

            COPY_OPND(opnd, IL_OPND(list2_idx));
            cast_to_type_idx(&opnd, &end_exp_desc, lcv_exp_desc.type_idx);
            COPY_OPND(IL_OPND(list2_idx), opnd);

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);

            COPY_OPND(opnd, IL_OPND(list2_idx));
            cast_to_type_idx(&opnd, &inc_exp_desc, lcv_exp_desc.type_idx);
            COPY_OPND(IL_OPND(list2_idx), opnd);
         }

         if (do_var_ok) {
            /* clear the AT_ATTR_LINK field of the old do var attr */
            AT_ATTR_LINK(attr_idx) = NULL_IDX;
            AT_IGNORE_ATTR_LINK(attr_idx) = FALSE;
         }

         in_implied_do = save_in_implied_do;
      }
      else {

         if (IL_FLD(list_idx) == IR_Tbl_Idx &&
             IR_OPR(IL_IDX(list_idx)) == Io_Item_Type_Code_Opr) {

            /* this is the second time here for this one. */
            /* remove the Io_Item_Type_Code_Opr.          */

            COPY_OPND(IL_OPND(list_idx), IR_OPND_L(IL_IDX(list_idx)));
         }

         exp_desc.rank = 0;
         COPY_OPND(opnd, IL_OPND(list_idx));
         number_of_functions = 0;
         io_item_must_flatten = FALSE;
         tree_has_constructor = FALSE;

         if (io_type == Read || io_type == Decode) {
            xref_state = CIF_Symbol_Modification;
         }
         else {
            xref_state = CIF_Symbol_Reference;
         }

         if (list_directed                                       &&
             OPND_FLD(opnd)                        == CN_Tbl_Idx &&
             TYP_TYPE(CN_TYPE_IDX(OPND_IDX(opnd))) == Typeless) {

            find_opnd_line_and_column(&opnd, &line, &col);
            PRINTMSG(line, 316, Error, col);
            semantically_correct = FALSE;
         }

         in_io_list = TRUE;
         semantically_correct = expr_semantics(&opnd, &exp_desc) &&
                                semantically_correct;
         COPY_OPND(IL_OPND(list_idx), opnd);
         in_io_list = FALSE;

         if (exp_desc.reference) {
            attr_idx = find_left_attr(&opnd);
           
            if (ATD_AUXILIARY(attr_idx)) {
               semantically_correct = FALSE;
               find_opnd_line_and_column(&opnd, &line, &col);
               PRINTMSG(line, 945, Error, col);
            }
         }

         if (item_has_bounds_chk(&opnd)) {
            number_of_functions++;
         }

         if (number_of_functions > 0) {
            IL_HAS_FUNCTIONS(list_idx)  = TRUE;
            needs_expansion             = TRUE;

            if (io_type == Read &&
                list_idx != OPND_IDX((*top_opnd))) {
               io_stmt_must_be_split       = TRUE;
            }
         }

         if (io_item_must_flatten         ||
             exp_desc.dist_reshape_ref    ||
             (IL_FLD(list_idx) == IR_Tbl_Idx &&
              IR_ARRAY_SYNTAX(IL_IDX(list_idx)))  ||
             exp_desc.vector_subscript)           {

            IL_MUST_FLATTEN(list_idx) = TRUE;
            have_seen_must_flatten  = TRUE;

            if ((io_type == Read || io_type == Decode)  &&
                (exp_desc.vector_subscript || exp_desc.dist_reshape_ref) &&
                IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {

               io_stmt_must_be_split       = TRUE;
            }

            IL_ARG_DESC_VARIANT(list_idx) = TRUE;

            /* save exp_desc */
            arg_info_list_base      = arg_info_list_top;
            arg_info_list_top       = arg_info_list_base + 1;

            if (arg_info_list_top >= arg_info_list_size) {
               enlarge_info_list_table();
            }

            IL_ARG_DESC_IDX(list_idx) = arg_info_list_top;
            arg_info_list[arg_info_list_top]    = init_arg_info;
            arg_info_list[arg_info_list_top].ed = exp_desc;
         }
         else if (tree_has_constructor) {
            IL_HAS_CONSTRUCTOR(list_idx) = TRUE;
            have_seen_constructor = TRUE;
         }

         if (io_type == Read || io_type == Decode) {

            if (!exp_desc.reference) {
               find_opnd_line_and_column(&opnd, &line, &col);

               if (exp_desc.constant) {
                  PRINTMSG(line, 479, Error, col,
                           io_stmt_str[io_type]);
               }
               else { /* expression */
                  PRINTMSG(line, 478, Error, col,
                           io_stmt_str[io_type]);
               }

               semantically_correct = FALSE;
            }
            else if (OPND_FLD(opnd) == AT_Tbl_Idx &&
                     imp_do_var_list != NULL_IDX) {

               imp_idx = imp_do_var_list;

               while (imp_idx) {

                  if (OPND_IDX(opnd) == IL_IDX(imp_idx)) {

                     /* error .. input item must not be imp do var */

                     find_opnd_line_and_column(&opnd, &line, &col);
                     PRINTMSG(line, 532, Error, col);
                     semantically_correct = FALSE;
                     break;
                  }
                  imp_idx = IL_NEXT_LIST_IDX(imp_idx);
               }
            }

            if (semantically_correct) {

               if (! check_for_legal_define(&opnd)) {
                  semantically_correct = FALSE;
               }
            }
         } /* io_type == Read */

         if (exp_desc.type == Structure) {

#ifdef KEY /* Bug 6845 */
            if (ATT_POINTER_CPNT(TYP_IDX(exp_desc.type_idx)) ||
	      ATT_ALLOCATABLE_CPNT(TYP_IDX(exp_desc.type_idx)))
#else /* KEY Bug 6845 */
            if (ATT_POINTER_CPNT(TYP_IDX(exp_desc.type_idx)))
#endif /* KEY Bug 6845 */
	    {
               find_opnd_line_and_column(&opnd, &line, &col);
               PRINTMSG(line, 235, Error, col);
               semantically_correct = FALSE;
            }
            else if (AT_USE_ASSOCIATED(TYP_IDX(exp_desc.type_idx)) &&
                     ATT_PRIVATE_CPNT(TYP_IDX(exp_desc.type_idx))) {
               find_opnd_line_and_column(&opnd, &line, &col);
               PRINTMSG(line, 1100, Error, col);
               semantically_correct = FALSE;
            }
            else if (IL_MUST_FLATTEN(list_idx)) {
               /* This is either a concat, array */
               /* syntax, or a vector valued subscript. All of which  */
               /* need to be flattened to a temp before the structure */
               /* is split up. The expr_desc has already been saved.  */

               IL_STRUCT_REF(list_idx) = TRUE;

            }
            else /* 11/07/00[sos]: else clause changed for PV 799401 */
            {

               IL_STRUCT_REF(list_idx) = TRUE;
               IL_ARG_DESC_VARIANT(list_idx) = TRUE;
               IL_ARG_DESC_VARIANT(list_idx) = TRUE;
               number_of_functions++;
               needs_expansion = TRUE;

               /* save exp_desc */
               arg_info_list_base      = arg_info_list_top;
               arg_info_list_top       = arg_info_list_base + 1;
   
               if (arg_info_list_top >= arg_info_list_size) {
                  enlarge_info_list_table();
               }
   
               IL_ARG_DESC_IDX(list_idx) = arg_info_list_top;
               arg_info_list[arg_info_list_top]    = init_arg_info;
               arg_info_list[arg_info_list_top].ed = exp_desc;
            }
         }
         else {
            /* insert the Io_Item_Type_Code_Opr */

            COPY_OPND(opnd, IL_OPND(list_idx));
            find_opnd_line_and_column(&opnd, &line, &col);

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Io_Item_Type_Code_Opr;
#ifdef KEY /* Bug 2611 */
            IR_TYPE_IDX(asg_idx) = intrinsic_special_case(&opnd, &exp_desc);
#else /* KEY Bug 2611 */
            IR_TYPE_IDX(asg_idx) = exp_desc.type_idx;
#endif /* KEY Bug 2611 */
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx) = col;

            COPY_OPND(IR_OPND_L(asg_idx), opnd);
            IL_FLD(list_idx) = IR_Tbl_Idx;
            IL_IDX(list_idx) = asg_idx;
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

EXIT:

   if (needs_expansion) {
      number_of_functions = 1;
   }

   if (have_seen_must_flatten) {
      io_item_must_flatten = TRUE;
   }

   if (have_seen_constructor) {
      tree_has_constructor = TRUE;
   }

   TRACE (Func_Exit, "io_list_semantics", NULL);

   return(semantically_correct);

}  /* io_list_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create namelist descriptor. This is the description of the tables     *|
|*      from the library people.                                              *|
|*                                                                            *|
|* The CFT90 namelist I/O READ and WRITE statements will generate             *|
|* namelist list table for single-call I/O interface:                         *|
|*                                                                            *|
|*         ---------------------------------------------------                *|
|*        | reserved |      pointer  to  namelist             |               *|
|*        |63      58|57                                     0|               *|
|*         ---------------------------------------------------                *|
|*                                                                            *|
|* A namelist for single-call namelist I/O statements may contain the         *|
|* following items in the namelist:                                           *|
|*                                                                            *|
|*     1.  scalar variables of type:                                          *|
|*          a. integer                                                        *|
|*          b. logical                                                        *|
|*          c. real (single and double)                                       *|
|*         d. complex (single and double)                                     *|
|*          e. character                                                      *|
|*          f. typeless or Boolean                                            *|
|*          g. derived type character (all character or mixed word and        *|
|*             character)                                                     *|
|*          h. derived type word (all word-oriented)                          *|
|*     2.  array variables of type:                                           *|
|*          a. integer                                                        *|
|*          b. logical                                                        *|
|*          c. real (single and double)                                       *|
|*          d. complex (single and double)                                    *|
|*          e. character                                                      *|
|*          f. typeless or Boolean                                            *|
|*          g. derived type character (all character or mixed word and        *|
|*             character)                                                     *|
|*          h. derived type word (all word-oriented)                          *|
|*                                                                            *|
|* Pointers are not allowed in I/O lists or namelists.                        *|
|*                                                                            *|
|* NAMELIST TABLE CONTENTS:                                                   *|
|*                                                                            *|
|* The namelist group information will contain the following 2-word entry:    *|
|*    ----------------------------------------------------------------------  *|
|* 0 |version|     reserved                                  |    icount    | *|
|*   |63   61|60                                           16|15           0| *|
|*    ----------------------------------------------------------------------  *|
|* 1 |                      fcd of namelist group name                      | *|
|*    ----------------------------------------------------------------------  *|
|*                                                                            *|
|* The namelist group information will be followed by one or more namelist    *|
|* group_object_list items:                                                   *|
|*    ----------------------------------------------------------------------  *|
|* 2 | valtype |               reserved                                     | *|
|*   |63     56|55                                                         0| *|
|*    ----------------------------------------------------------------------  *|
|* 3 |                fcd of namelist group_object_list_item name           | *|
|*    ----------------------------------------------------------------------  *|
|* 4 | address of 1)scalar info or 2)array dopevector or 3)structure table  | *|
|*    ----------------------------------------------------------------------  *|
|*                                                                            *|
|* The dope vector is described in dopevec.h.  The namelist scalar entry      *|
|* contains:                                                                  *|
|*    ----------------------------------------------------------------------  *|
|* 0 |     reserved                   |  type  |dp| decde| intlen | decllen | *|
|*   |63                            32|31    24|23|22  20|19     8|7       0| *|
|*    ----------------------------------------------------------------------  *|
|* 1 |   fortran character descriptor or address of noncharacter variable   | *|
|*    ----------------------------------------------------------------------  *|
|*                                                                            *|
|* Structures point to another namelist table which contains header word with *|
|* a count of the number of entries in the structure and one or more namelist *|
|* group_object_list entries for scalars, arrays, and other structures within *|
|* the structure.                                                             *|
|*    ----------------------------------------------------------------------  *|
|* 0 |             reserved                                  | structlen    | *|
|*   |63                                                   16|15           0| *|
|*    ----------------------------------------------------------------------  *|
|* 1 |  address of dopevector if structure is an array, else addr of strct  | *|
|*    ----------------------------------------------------------------------  *|
|* 2 | valtype |               reserved                                     | *|
|*   |63     56|55                                                         0| *|
|*    ----------------------------------------------------------------------  *|
|* 3 |                fcd of namelist group_object_list_item name           | *|
|*    ----------------------------------------------------------------------  *|
|* 4 | address of 1)scalar info or 2)array dopevector or 3)structure entries| *|
|*    ----------------------------------------------------------------------  *|
|*                                                                            *|
|* where:                                                                     *|
|*                                                                            *|
|* Namelist Group Information:                                                *|
|*                                                                            *|
|* WORD 0:                                                                    *|
|*        version:                                                            *|
|*        word 0, bits 61-63                                                  *|
|*                1 = current version                                         *|
|*                                                                            *|
|*        reserved for future development:                                    *|
|*        word 0, bits 16-60 = 0                                              *|
|*                                                                            *|
|*        icount is the number of namelist group_object_list items in the     *|
|*        namelist table.                                                     *|
|*        word 0, bits 0-15                                                   *|
|*                                                                            *|
|* WORD 1:                                                                    *|
|*        fcd of namelist group name:                                         *|
|*        word 1, bits 0-63                                                   *|
|*                                                                            *|
|* Namelist group_object_list_item information:                               *|
|*                                                                            *|
|* WORD 0:                                                                    *|
|*        valtype indicates type of iolist entry:                             *|
|*        word 0, bits 56-63                                                  *|
|*                0 = unused                                                  *|
|*                1 = scalar, no pointers                                     *|
|*                2 = dope vector for array, no pointers                      *|
|*                3 = io loop (NOT USED FOR NAMELIST)                         *|
|*                4 = structure as scalar, no pointers                        *|
|*                5 = structure as array, no pointers                         *|
|*                                                                            *|
|*        reserved for future development:                                    *|
|*        word 0, bits 0-63 = 0                                               *|
|*                                                                            *|
|* WORD 1:                                                                    *|
|*        fcd of namelist group_object_list_item name:                        *|
|*        word 1, bits 0-63                                                   *|
|*                                                                            *|
|* WORD 2:                                                                    *|
|*        address of namelist 1) scalar information, 2) dopevector,           *|
|*                            3) structure                                    *|
|*        table                                                               *|
|*        word 2, bits 0-63                                                   *|
|*                                                                            *|
|* Namelist scalar information contains:                                      *|
|*                                                                            *|
|* WORD 0:                                                                    *|
|*        Fortran 90 type word                                                *|
|*                                                                            *|
|* WORD 1:                                                                    *|
|*        fcd of scalar character item or addr of noncharacter scalar item:   *|
|*        word 1, bits 0-63                                                   *|
|*                                                                            *|
|* Namelist structure information contains:                                   *|
|*                                                                            *|
|* WORD 0:                                                                    *|
|*        reserved for future development:                                    *|
|*        word 0, bits 17-63 = 0                                              *|
|*                                                                            *|
|*        structlen is number of structure components in this structure:      *|
|*        word 0, bits 0-16                                                   *|
|*                                                                            *|
|* WORD 1:                                                                    *|
|*      address of dopevector when structure is an array;                     *|
|*      else address of scalar structure.                                     *|
|*      word 1, bits 0-63                                                     *|
|*                                                                            *|
|* WORD n*(1-3) where n is the number of structure components in structure:   *|
|*                                                                            *|
|*        Namelist group_object_list_item[n]                                  *|
|*                                                                            *|
|*									      *|
|* Input parameters:							      *|
|*	namelist_attr - idx to namelist group attr                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void create_namelist_descriptor(int	namelist_attr)

{
   int			asg_idx;
   int			col;
   expr_arg_type	exp_desc;
   int			head_idx;
   long_type		idx_constant;
   boolean		in_module = FALSE;
   int			item_attr;
   opnd_type		l_opnd;
   int			line;
   int			list_idx;
   int			loc_idx;
   long			num;
   int			offset;
   boolean		ok;
   opnd_type		opnd;
   opnd_type		opnd2;
   int			save_curr_stmt_sh_idx;
   int			sh_idx;
   int			size;
   int			sn_idx;
   int			sub_idx;
#ifdef KEY /* Bug 10177 */
   int			stack_grp_tbl_idx = 0;
#else /* KEY Bug 10177 */
   int			stack_grp_tbl_idx;
#endif /* KEY Bug 10177 */
   int			static_grp_tbl_idx;
   int			tail_idx;
   long_type		the_constant[2];
   int			tmp_idx;
   int			type_idx;
   int			val_type;

   nmlist_group_hdr	*group_hdr_ptr;
   nmlist_goli_t	*goli_ptr;

# ifdef _INIT_RELOC_BASE_OFFSET
   int			attr_idx;
# endif


   TRACE (Func_Entry, "create_namelist_descriptor", NULL);

   line       = AT_DEF_LINE(namelist_attr);
   col        = AT_DEF_COLUMN(namelist_attr);
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
      in_module = TRUE;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   type_idx = SA_INTEGER_DEFAULT_TYPE;
# else
   type_idx = CG_INTEGER_DEFAULT_TYPE;
# endif

   /*****************************************\
   |* create static namelist group tbl attr *|
   \*****************************************/

   if (two_word_fcd) {
      size = NML_GRP_HDR_SIZE_FCD2 +
                   (NML_GRP_ITEM_SIZE_FCD2 * ATN_NUM_NAMELIST(namelist_attr));
   }
   else {
      size = NML_GRP_HDR_SIZE + 
                  (NML_GRP_ITEM_SIZE * ATN_NUM_NAMELIST(namelist_attr));
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   /* the version item is always 64 bits */
   /* add one for the header version, and one for each item entry */
   if (TYP_LINEAR(type_idx) == Integer_4) {
      size += 1 + ATN_NUM_NAMELIST(namelist_attr);
   }
# endif

   static_grp_tbl_idx  = gen_static_integer_array_tmp(size, line, col);

   if (! in_module) {
      /****************************************\
      |* create stack namelist group tbl attr *|
      \****************************************/

      stack_grp_tbl_idx			  = gen_compiler_tmp(line,col,
                                                             Priv, TRUE);
      ATD_TYPE_IDX(stack_grp_tbl_idx)	  = ATD_TYPE_IDX(static_grp_tbl_idx);
      ATD_STOR_BLK_IDX(stack_grp_tbl_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      /* This new tmp is fully created, so does not need decl_semantics */

      AT_SEMANTICS_DONE(stack_grp_tbl_idx)	= TRUE;

      ATD_ARRAY_IDX(stack_grp_tbl_idx) = ATD_ARRAY_IDX(static_grp_tbl_idx);
   }


   sh_idx			= ntr_sh_tbl();
   SH_STMT_TYPE(sh_idx)		= Assignment_Stmt;
   SH_GLB_LINE(sh_idx)		= line;
   SH_COL_NUM(sh_idx)		= col;
   SH_COMPILER_GEN(sh_idx)	= TRUE;
   SH_P2_SKIP_ME(sh_idx)	= TRUE;
   head_idx			= sh_idx;
   tail_idx			= sh_idx;
   curr_stmt_sh_idx		= sh_idx;

   if (! in_module) {
      /***********************************\
      |* copy static attr to stack attr. *|
      \***********************************/

   
      gen_opnd(&opnd, stack_grp_tbl_idx, AT_Tbl_Idx, line, col);
      ok = gen_whole_subscript(&opnd, &exp_desc);

      gen_opnd(&opnd2, static_grp_tbl_idx, AT_Tbl_Idx, line, col);
      ok = gen_whole_subscript(&opnd2, &exp_desc);

      asg_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                   Asg_Opr, type_idx, line, col,
                       OPND_FLD(opnd2), OPND_IDX(opnd2));

      gen_sh(After, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }

   /* set first word to version and count */
   /* this can be two words on IRIX with -n32 */

   idx_constant = 1;

   the_constant[0] = 0;
   the_constant[1] = 0;

   group_hdr_ptr = (nmlist_group_hdr *)the_constant;

# if defined(_BITFIELD_RIGHT_TO_LEFT)		/* Most x86 platforms */
   the_constant[0] = 1  | (ATN_NUM_NAMELIST(namelist_attr) << 16) ;
# else
   group_hdr_ptr->version = 1;
   group_hdr_ptr->icount = ATN_NUM_NAMELIST(namelist_attr);
# endif

   gen_opnd(&opnd,
            ntr_const_tbl((sizeof(nmlist_group_hdr) == 8) ? Integer_8 :
                                                            Integer_4,
                          FALSE,
                          the_constant),
            CN_Tbl_Idx,
            line,
            col);

   gen_array_element_init(static_grp_tbl_idx,
                          &idx_constant,
                          &opnd,
                          Init_Opr,
                          NULL_IDX);


   /***********************************************\
   |* set next word to fcd to namelist group name *|
   \***********************************************/

   put_string_in_tmp(AT_OBJ_NAME_PTR(namelist_attr),
                     AT_NAME_LEN(namelist_attr),
                    &opnd);

# ifdef _INIT_RELOC_BASE_OFFSET
   if (in_module) {

      /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
      /* Create a temp as an overlay for the first object if   */
      /* there is no FIRST attr.                               */

      attr_idx = find_left_attr(&opnd);

      if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
         set_sb_first_attr_idx(attr_idx);
      }
   }
# endif
   
   /* tmp is character */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                Loc_Opr, CRI_Ch_Ptr_8, line, col,
                    NO_Tbl_Idx, NULL_IDX);
# else
   loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                Aloc_Opr, CRI_Ch_Ptr_8, line, col,
                    NO_Tbl_Idx, NULL_IDX);
# endif


   gen_opnd(&opnd, loc_idx, IR_Tbl_Idx, line, col);

   gen_array_element_init(in_module ? static_grp_tbl_idx : stack_grp_tbl_idx,
                          &idx_constant,
                          &opnd,
                          in_module ? Init_Reloc_Opr : Asg_Opr,
                          NULL_IDX);


   if (two_word_fcd) {
      /* fill in the length explicitly */

      if (char_len_in_bytes) {
         /* length is in bytes on solaris */
         num = (long) AT_NAME_LEN(namelist_attr);
      }
      else {
         /* length is in bits on mpp. */
         num = (long) AT_NAME_LEN(namelist_attr) * CHAR_BIT;
      }

      gen_opnd(&opnd,
               C_INT_TO_CN(type_idx, num),
               CN_Tbl_Idx,
               line,
               col);

      gen_array_element_init(static_grp_tbl_idx,
                             &idx_constant,
                             &opnd,
                             Init_Opr,
                             NULL_IDX);

   }

   sn_idx = ATN_FIRST_NAMELIST_IDX(namelist_attr);

   while (sn_idx != NULL_IDX) {

      item_attr = SN_ATTR_IDX(sn_idx);

      while (AT_ATTR_LINK(item_attr) &&
             ! AT_IGNORE_ATTR_LINK(item_attr)) {
         item_attr = AT_ATTR_LINK(item_attr);
      }


      /***************************************************\
      |* set the valtype in the first word of item entry *|
      \***************************************************/

      if (TYP_TYPE(ATD_TYPE_IDX(item_attr)) == Structure) {

         if (ATD_ARRAY_IDX(item_attr)) {
            val_type = NML_VALTYPE_STRCT_ARRAY;
         }
         else {
            val_type = NML_VALTYPE_STRCT;
         }
      }
      else if (ATD_ARRAY_IDX(item_attr)) {
         val_type = NML_VALTYPE_ARRAY;
      }
      else {
         val_type = NML_VALTYPE_SCALAR;
      }


# if defined(_BITFIELD_RIGHT_TO_LEFT)		/* Most x86 platforms */
      the_constant[0] = val_type;
# else

      the_constant[0] = 0;
      the_constant[1] = 0;

      goli_ptr = (nmlist_goli_t *)the_constant;

      goli_ptr->valtype = val_type;
# endif

      gen_opnd(&opnd,
               ntr_const_tbl((sizeof(nmlist_goli_t) == 8) ? Integer_8 :
                                                            Integer_4,
                             FALSE,
                             the_constant),
               CN_Tbl_Idx,
               line,
               col);

      gen_array_element_init(static_grp_tbl_idx,
                             &idx_constant,
                             &opnd,
                             Init_Opr,
                             NULL_IDX);


      /***************************************\
      |* set the fcd for the group item name *|
      \***************************************/

      put_string_in_tmp(AT_OBJ_NAME_PTR(item_attr),
                        AT_NAME_LEN(item_attr),
                       &opnd);

# ifdef _INIT_RELOC_BASE_OFFSET
      if (in_module) {

         /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
         /* Create a temp as an overlay for the first object if   */
         /* there is no FIRST attr.                               */

         attr_idx = find_left_attr(&opnd);

         if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
            set_sb_first_attr_idx(attr_idx);
         }
      }
# endif

      /* tmp is character */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                   Loc_Opr, CRI_Ch_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# else
      loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                   Aloc_Opr, CRI_Ch_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# endif


      gen_opnd(&opnd, loc_idx, IR_Tbl_Idx, line, col);

      gen_array_element_init(in_module ? static_grp_tbl_idx:stack_grp_tbl_idx,
                             &idx_constant,
                             &opnd,
                             in_module ? Init_Reloc_Opr : Asg_Opr,
                             NULL_IDX);


      if (two_word_fcd) {
         /* fill in the length explicitly */

         if (char_len_in_bytes) {
            /* length is in bytes on solaris */
            num = (long) AT_NAME_LEN(item_attr);
         }
         else {
            /* length is in bits on mpp. */
            num = (long) AT_NAME_LEN(item_attr) * CHAR_BIT;
         }

         gen_opnd(&opnd,
                  C_INT_TO_CN(type_idx, num),
                  CN_Tbl_Idx,
                  line,
                  col);

         gen_array_element_init(static_grp_tbl_idx,
                                &idx_constant,
                                &opnd,
                                Init_Opr,
                                NULL_IDX);
   
      }


      /*******************************************\
      |* Now for the varieties of the third word *|
      \*******************************************/

      gen_opnd(&opnd, item_attr, AT_Tbl_Idx, line, col);

      switch (val_type) {
         case NML_VALTYPE_SCALAR :
            /* get scalar type tbl */
            loc_idx = gen_ir(AT_Tbl_Idx, create_scalar_type_tbl(&opnd, 
                                                                in_module),
                         Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);

            break;

         case NML_VALTYPE_ARRAY :
            /* get dope vector */

            exp_desc = init_exp_desc;
            ok = gen_whole_subscript(&opnd, &exp_desc);

            if (in_module) {
               namelist_static_dv_whole_def(&l_opnd, &opnd);
            }
            else {
               tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);
               ATD_TYPE_IDX(tmp_idx)	= ATD_TYPE_IDX(item_attr);
               ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
               AT_SEMANTICS_DONE(tmp_idx)	= TRUE;

               /* Positions 1-7 are deferred shape entries in the bd table. */

               ATD_ARRAY_IDX(tmp_idx) = exp_desc.rank;
               ATD_IM_A_DOPE(tmp_idx)    = TRUE;
               OPND_FLD(l_opnd) = AT_Tbl_Idx;
               OPND_IDX(l_opnd) = tmp_idx;
               OPND_LINE_NUM(l_opnd) = line;
               OPND_COL_NUM(l_opnd) = col;

               exp_desc.type_idx = ATD_TYPE_IDX(item_attr);
               exp_desc.type     = TYP_TYPE(exp_desc.type_idx);
               exp_desc.linear_type = TYP_LINEAR(exp_desc.type_idx);

               if (exp_desc.type == Character) {
                  exp_desc.char_len.fld = TYP_FLD(exp_desc.type_idx);
                  exp_desc.char_len.idx = TYP_IDX(exp_desc.type_idx);
               }
               gen_dv_whole_def(&l_opnd, &opnd, &exp_desc);
            }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            loc_idx = gen_ir(OPND_FLD(l_opnd), OPND_IDX(l_opnd),
                         Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);
# else
            loc_idx = gen_ir(OPND_FLD(l_opnd), OPND_IDX(l_opnd),
                         in_module ? Aloc_Opr : Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);
# endif

            break;

         case NML_VALTYPE_STRCT :
         case NML_VALTYPE_STRCT_ARRAY :
            /* get struct tbl */
            loc_idx = gen_ir(AT_Tbl_Idx, create_strct_tbl(&opnd, in_module),
                         Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);

            break;
      }

# ifdef _INIT_RELOC_BASE_OFFSET
      if (in_module) {

         /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
         /* Create a temp as an overlay for the first object if   */
         /* there is no FIRST attr.                               */

         attr_idx = find_left_attr(&(IR_OPND_L(loc_idx)));

         if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
            set_sb_first_attr_idx(attr_idx);
         }
      }
# endif

      gen_opnd(&opnd, loc_idx, IR_Tbl_Idx, line, col);

      gen_array_element_init(in_module ? static_grp_tbl_idx:stack_grp_tbl_idx,
                             &idx_constant,
                             &opnd,
                             in_module ? Init_Reloc_Opr : Asg_Opr,
                             NULL_IDX);


      sn_idx = SN_SIBLING_LINK(sn_idx);
   }

   ATN_NAMELIST_DESC(namelist_attr) = in_module ? static_grp_tbl_idx :
                                                  stack_grp_tbl_idx;

   tail_idx = curr_stmt_sh_idx;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

# ifdef _DEBUG
   if (SH_IR_IDX(head_idx) != NULL_IDX) {
      PRINTMSG(line, 626, Internal, col,
               "empty head_idx", "create_namelist_descriptor");
   }
# endif
   head_idx = SH_NEXT_IDX(head_idx);
   FREE_SH_NODE(SH_PREV_IDX(head_idx));
   SH_PREV_IDX(head_idx) = NULL_IDX;

   insert_sh_chain_after_entries(head_idx, tail_idx);

   TRACE (Func_Exit, "create_namelist_descriptor", NULL);

   return;

}  /* create_namelist_descriptor */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Create a whole def of a dope vector that is in a module block.        *|
|*      This is for namelist tables. It is a complete definition using        *|
|*      Init_Opr and Init_Reloc_Opr. There can be no runtime assignments.     *|
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

static void namelist_static_dv_whole_def(opnd_type         *l_opnd,
                                         opnd_type         *r_opnd)

{
   int                  asg_idx;
   int			attr_idx;
   int                  col;
   int                  const_idx;
   long_type		constant[2];
   int			dope_idx = NULL_IDX;
   ext_dope_type        *dv_ptr;
   int			i;
   long_type		idx_constant;
   int                  line;
   int                  list_idx;
   int			loc_idx;
   int			num_elements;
   int                  num_words;
   int			offset;
   opnd_type            opnd;
   long_type            rank;
   int			rank_idx = NULL_IDX;
   int			sub_idx;
   long_type		the_constant[2];
   int			tmp_idx;
   int                  type_idx;
   int                  type_idx2;
   int			words_in_address = 1;

# ifdef _INIT_RELOC_BASE_OFFSET
   int			attr_idx2;
# endif


   TRACE (Func_Entry, "namelist_static_dv_whole_def", NULL);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   type_idx2 = SA_INTEGER_DEFAULT_TYPE;
 
   if (type_idx2 == Integer_8) {
      words_in_address = 2;
   }
# else
   type_idx2 = CG_INTEGER_DEFAULT_TYPE;
# endif

   attr_idx = find_base_attr(r_opnd, &line, &col);

   rank = (long_type) ((ATD_ARRAY_IDX(attr_idx) ? 
                        BD_RANK(ATD_ARRAY_IDX(attr_idx)) :0));

   num_words    = DV_HD_WORD_SIZE + (rank * DV_DIM_WORD_SIZE);
   num_elements = num_words;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (TYP_LINEAR(type_idx2) == Integer_8) {
      num_elements = num_elements / 2;
   }
# endif

   tmp_idx = gen_static_integer_array_tmp(num_elements, line, col);

   gen_opnd(l_opnd, tmp_idx, AT_Tbl_Idx, line, col);

   /* Start the initialization of the dope vector at the second element */

   idx_constant = 2;

   /* We don't want to initialize the Base address in this constant */
   /* It gets a Init_Reloc_Opr and ccg doesn't allow multiple inits */
   /* when one is a reloc init. So ask for (num_words - 1).         */

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
#if defined (TARG_X8664) && defined (_HOST64)
   // OSP_TODO, need to be re-checked
   TYP_BIT_LEN(TYP_WORK_IDX)    = (num_words - words_in_address) *
	                          ((SET_POINTER_SIZE) ? 64 : 32);
#else
   TYP_BIT_LEN(TYP_WORK_IDX)	= (num_words - words_in_address) * 
                                                    TARGET_BITS_PER_WORD;
#endif
   type_idx			= ntr_type_tbl();

   const_idx    = ntr_const_tbl(type_idx, FALSE, NULL);

   gen_opnd(&opnd, const_idx, CN_Tbl_Idx, line, col);

   gen_array_element_init(tmp_idx,
                          &idx_constant,
                          &opnd,
                          Init_Opr,
                          NULL_IDX);

   /********************\
   |* set BASE address *|
   \********************/

# ifdef _INIT_RELOC_BASE_OFFSET
   offset = change_to_base_and_offset(r_opnd, &opnd);
# else
   make_base_subtree(r_opnd, &opnd, &rank_idx, &dope_idx);
   offset = NULL_IDX;
# endif


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                Loc_Opr, TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character ?
                               CRI_Ch_Ptr_8 : CRI_Ptr_8, line, col,
                    NO_Tbl_Idx, NULL_IDX);
# else
   loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                Aloc_Opr, TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character ?
                               CRI_Ch_Ptr_8 : CRI_Ptr_8, line, col,
                    NO_Tbl_Idx, NULL_IDX);
# endif


# ifdef _INIT_RELOC_BASE_OFFSET
   /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
   /* Create a temp as an overlay for the first object if   */
   /* there is no FIRST attr.                               */

   attr_idx2 = find_left_attr(&opnd);

   if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx2)) == NULL_IDX) {
      set_sb_first_attr_idx(attr_idx2);
   }
# endif


# ifdef _TRANSFORM_CHAR_SEQUENCE
# ifdef _TARGET_OS_UNICOS
   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
       ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {

      IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
      COPY_OPND(opnd, IR_OPND_L(loc_idx));
      transform_char_sequence_ref(&opnd, ATD_TYPE_IDX(attr_idx));
      COPY_OPND(IR_OPND_L(loc_idx), opnd);
   }
# endif
# endif

   gen_opnd(&opnd, loc_idx, IR_Tbl_Idx, line, col);

   /* reset idx_constant to 1 for the base address */
   idx_constant = 1;
   gen_array_element_init(tmp_idx,
                          &idx_constant,
                          &opnd,
                          Init_Reloc_Opr,
                          offset);
   

   /* We must set the dv_ptr to the word before the actual constant    */
   /* since it has a "Base address" component and the constant doesn't */

   dv_ptr = (ext_dope_type *)&(CP_CONSTANT(
                              CN_POOL_IDX(const_idx) - words_in_address));
   type_idx = ATD_TYPE_IDX(attr_idx);

   /* the entire constant is initialized to 0's */
   /* so just fill in the non zero parts.       */

   /******************\
   |* set ASSOC flag *|
   \******************/

   DV_SET_ASSOC(*dv_ptr, 1);


   /*************\
   |* EL_LEN    *|
   \*************/

   if (TYP_TYPE(type_idx) == Structure) {

      if (compare_cn_and_value(ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx)),
                               MAX_DV_EL_LEN,
                               Ge_Opr)) {
         PRINTMSG(line, 1174, Error, col, 
                  ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx)),
                  MAX_DV_EL_LEN);
         DV_SET_EL_LEN(*dv_ptr, MAX_DV_EL_LEN);
      }
      else {

         gen_opnd(&opnd, 
                  ATT_STRUCT_BIT_LEN_IDX(TYP_IDX(type_idx)), 
                  CN_Tbl_Idx,
                  line, 
                  col);

         cast_opnd_to_type_idx(&opnd, type_idx2);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (TYP_LINEAR(type_idx2) == Integer_8) {
            DV_SET_EL_LEN(*dv_ptr, *(long long *)&(CN_CONST(OPND_IDX(opnd))));
         }
         else {
            DV_SET_EL_LEN(*dv_ptr, CN_CONST(OPND_IDX(opnd)));
         }
# else
         DV_SET_EL_LEN(*dv_ptr, CN_CONST(OPND_IDX(opnd)));
# endif
      }
   }
   else if (TYP_TYPE(type_idx) == Character) {

      if (TYP_FLD(type_idx) == CN_Tbl_Idx) {

         gen_opnd(&opnd,
                  TYP_IDX(type_idx),
                  CN_Tbl_Idx,
                  line,
                  col);

         cast_opnd_to_type_idx(&opnd, type_idx2);


         if (! char_len_in_bytes) {

            /* length must be in bits for every platform BUT solaris */
            if (folder_driver((char *)&CN_CONST(OPND_IDX(opnd)),
                              type_idx2,
                              (char *)&CN_CONST(CN_INTEGER_CHAR_BIT_IDX),
                              CN_TYPE_IDX(CN_INTEGER_CHAR_BIT_IDX),
                              the_constant,
                             &type_idx2,
                              line,
                              col,
                              2,
                              Mult_Opr)) {
            }

            gen_opnd(&opnd,
                     ntr_const_tbl(type_idx2,
                                   FALSE,
                                   the_constant),
                     CN_Tbl_Idx,
                     line,
                     col);
         }


         if (char_len_in_bytes) {

            if (compare_cn_and_value(TYP_IDX(type_idx),
                                     MAX_DV_EL_LEN,
                                     Ge_Opr)) {
               PRINTMSG(line, 1174, Error, col,
                        TYP_IDX(type_idx), MAX_DV_EL_LEN);
            }
         }
         else {

            if (compare_cn_and_value(TYP_IDX(type_idx),
                                     MAX_DV_EL_LEN/8,
                                     Ge_Opr)) {
               PRINTMSG(line, 1174, Error, col,
                        TYP_IDX(type_idx),
                        MAX_DV_EL_LEN/8);
            }
         }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (TYP_LINEAR(type_idx2) == Integer_8) {
            DV_SET_EL_LEN(*dv_ptr, *(long long *)&(CN_CONST(OPND_IDX(opnd))));
         }
         else {
            DV_SET_EL_LEN(*dv_ptr, CN_CONST(OPND_IDX(opnd)));
         }
# else
         DV_SET_EL_LEN(*dv_ptr, CN_CONST(OPND_IDX(opnd)));
# endif
      }
      else {
         PRINTMSG(line, 630, Internal, col);
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

   for (i = 0; i < rank; i++) {

      /************************************\
      |* set LOW_BOUND for each dimension *|
      \************************************/

      gen_opnd(&opnd,
               BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), i + 1),
               CN_Tbl_Idx,
               line,
               col);

      cast_opnd_to_type_idx(&opnd, type_idx2);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (TYP_LINEAR(type_idx2) == Integer_8) {
         DV_SET_LOW_BOUND(*dv_ptr,i, 
                    *(long long *)&(CN_CONST(OPND_IDX(opnd))));
      }
      else {
         DV_SET_LOW_BOUND(*dv_ptr,i, CN_CONST(OPND_IDX(opnd)));
      }
# else
      DV_SET_LOW_BOUND(*dv_ptr,i, CN_CONST(OPND_IDX(opnd)));
# endif

      /*********************************\
      |* set EXTENT for each dimension *|
      \*********************************/

      gen_opnd(&opnd,
               BD_XT_IDX(ATD_ARRAY_IDX(attr_idx), i + 1),
               CN_Tbl_Idx,
               line,
               col);

      cast_opnd_to_type_idx(&opnd, type_idx2);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (TYP_LINEAR(type_idx2) == Integer_8) {
         DV_SET_EXTENT(*dv_ptr,i,
                    *(long long *)&(CN_CONST(OPND_IDX(opnd))));
      }
      else {
         DV_SET_EXTENT(*dv_ptr,i, CN_CONST(OPND_IDX(opnd)));
      }
# else
      DV_SET_EXTENT(*dv_ptr,i, CN_CONST(OPND_IDX(opnd)));
# endif

      /**************************************\
      |* set STRIDE_MULT for each dimension *|
      \**************************************/

      gen_opnd(&opnd,
               BD_SM_IDX(ATD_ARRAY_IDX(attr_idx), i + 1),
               CN_Tbl_Idx,
               line,
               col);

      cast_opnd_to_type_idx(&opnd, type_idx2);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (TYP_LINEAR(type_idx2) == Integer_8) {
         DV_SET_STRIDE_MULT(*dv_ptr,i,
                    *(long long *)&(CN_CONST(OPND_IDX(opnd))));
      }
      else {
         DV_SET_STRIDE_MULT(*dv_ptr,i, CN_CONST(OPND_IDX(opnd)));
      }
# else
      DV_SET_STRIDE_MULT(*dv_ptr,i, CN_CONST(OPND_IDX(opnd)));
# endif

   }

   TRACE (Func_Exit, "namelist_static_dv_whole_def", NULL);

   return;

}  /* namelist_static_dv_whole_def */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	create the two word scalar type tbl entry. It is described in the     *|
|*      description for create_namelist_descriptor.                           *|
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

static int create_scalar_type_tbl(opnd_type	*opnd,
                                  boolean	 in_module)

{
   int			attr_idx;
   int			asg_idx;
   int			base_attr;
   int			col;
   expr_arg_type	exp_desc;
   long_type		idx_constant;
   int			line;
   int			list_idx;
   int			loc_idx;
   int			offset;
   boolean		ok;
   opnd_type		opnd2;
   int			sub_idx;
   long_type		the_constant[2];
   long64		num;
   int			tmp_idx;
   int			type_idx;

   TRACE (Func_Entry, "create_scalar_type_tbl", NULL);

   base_attr = find_base_attr(opnd, &line, &col);

   /**********************************\
   |* create scalar type tbl attr    *|
   \**********************************/

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   type_idx = SA_INTEGER_DEFAULT_TYPE;
# else
   type_idx = CG_INTEGER_DEFAULT_TYPE;
# endif

   if (in_module) {
      tmp_idx                   = gen_compiler_tmp(line,col, Shared, TRUE);
      ATD_TYPE_IDX(tmp_idx)     = type_idx;
      AT_SEMANTICS_DONE(tmp_idx)        = TRUE;

      ATD_SAVED(tmp_idx)        = TRUE;
      ATD_DATA_INIT(tmp_idx)    = TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
   }
   else {
      tmp_idx                   = gen_compiler_tmp(line,col, Priv, TRUE);
      ATD_TYPE_IDX(tmp_idx)     = type_idx;
      AT_SEMANTICS_DONE(tmp_idx)        = TRUE;

      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
   }

   exp_desc.type	= Integer;
   exp_desc.type_idx	= type_idx;
   exp_desc.linear_type	= TYP_LINEAR(type_idx);
   exp_desc.rank	= 1;
   exp_desc.shape[0].fld = CN_Tbl_Idx;

   if (two_word_fcd) {
      num = NML_SCALAR_ENTRY_SIZE_FCD2;
   }
   else {
      num = NML_SCALAR_ENTRY_SIZE;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (TYP_LINEAR(type_idx) == Integer_4) {
      num++;
   }
# endif

   exp_desc.shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num);

   ATD_ARRAY_IDX(tmp_idx) = create_bd_ntry_for_const(&exp_desc,
                                                     line,
                                                     col);


   /*********************\
   |* fill in type code *|
   \*********************/

   idx_constant = 1;

   make_io_type_code(ATD_TYPE_IDX(base_attr), the_constant);

   gen_opnd(&opnd2,
            ntr_const_tbl(IO_TYPE_CODE_TYPE,
                          FALSE,
                          the_constant),
            CN_Tbl_Idx,
            line,
            col);

   gen_array_element_init(tmp_idx,
                          &idx_constant,
                          &opnd2,
                          in_module ? Init_Opr : Asg_Opr,
                          NULL_IDX);

   /***********************\
   |* fill in loc of opnd *|
   \***********************/

   if (TYP_TYPE(ATD_TYPE_IDX(base_attr)) == Character) {
      ok = gen_whole_substring(opnd, 0);
   }

   offset = NULL_IDX;

   opnd2 = *opnd;

# ifdef _INIT_RELOC_BASE_OFFSET
   if (in_module) {
      offset  = change_to_base_and_offset(opnd, &opnd2);

      /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
      /* Create a temp as an overlay for the first object if   */
      /* there is no FIRST attr.                               */

      attr_idx = find_left_attr(&opnd2);

      if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
         set_sb_first_attr_idx(attr_idx);
      }
   }
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   loc_idx = gen_ir(OPND_FLD(opnd2), OPND_IDX(opnd2),
                Loc_Opr, TYP_TYPE(ATD_TYPE_IDX(base_attr)) == Character ?
                               CRI_Ch_Ptr_8 : CRI_Ptr_8, line, col,
                    NO_Tbl_Idx, NULL_IDX);
# else
   loc_idx = gen_ir(OPND_FLD(opnd2), OPND_IDX(opnd2),
                Aloc_Opr, TYP_TYPE(ATD_TYPE_IDX(base_attr)) == Character ?
                               CRI_Ch_Ptr_8 : CRI_Ptr_8, line, col,
                    NO_Tbl_Idx, NULL_IDX);
# endif


# ifdef _TRANSFORM_CHAR_SEQUENCE
# ifdef _TARGET_OS_UNICOS
   if (TYP_TYPE(ATD_TYPE_IDX(base_attr)) == Structure &&
       ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(base_attr)))) {

      IR_TYPE_IDX(loc_idx)      = CRI_Ch_Ptr_8;
      COPY_OPND(opnd2, IR_OPND_L(loc_idx));
      transform_char_sequence_ref(&opnd2, ATD_TYPE_IDX(base_attr));
      COPY_OPND(IR_OPND_L(loc_idx), opnd2);
   }
# endif
# endif

   gen_opnd(&opnd2, loc_idx, IR_Tbl_Idx, line, col);

   gen_array_element_init(tmp_idx,
                          &idx_constant,
                          &opnd2,
                          in_module ? Init_Reloc_Opr : Asg_Opr,
                          offset);


   if (TYP_TYPE(ATD_TYPE_IDX(base_attr)) == Character &&
       two_word_fcd) {

      /* fill in the length explicitly */
   
      /* This must be a constant length character variable */
# ifdef _DEBUG
      if (TYP_FLD(ATD_TYPE_IDX(base_attr)) != CN_Tbl_Idx) {
         PRINTMSG(line, 1025, Internal, col);
      }
# endif

      gen_opnd(&opnd2,
               TYP_IDX(ATD_TYPE_IDX(base_attr)),
               CN_Tbl_Idx,
               line,
               col);

      cast_opnd_to_type_idx(&opnd2, type_idx);


      if (! char_len_in_bytes) {

         /* length must be in bits for every platform BUT solaris */
         if (folder_driver((char *)&CN_CONST(OPND_IDX(opnd2)),
                           type_idx,
                           (char *)&CN_CONST(CN_INTEGER_CHAR_BIT_IDX),
                           CN_TYPE_IDX(CN_INTEGER_CHAR_BIT_IDX),
                           the_constant,
                          &type_idx,
                           line,
                           col,
                           2,
                           Mult_Opr)) {
         }

         gen_opnd(&opnd2,
                  ntr_const_tbl(type_idx,
                                FALSE,
                                the_constant),
                  CN_Tbl_Idx,
                  line,
                  col);

      }


      gen_array_element_init(tmp_idx,
                             &idx_constant,
                             &opnd2,
                             in_module ? Init_Opr : Asg_Opr,
                             NULL_IDX);
   }


   TRACE (Func_Exit, "create_scalar_type_tbl", NULL);

   return(tmp_idx);

}  /* create_scalar_type_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	create the struct tbl for namelist descriptors.                       *|
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

static int create_strct_tbl(opnd_type	*base_opnd,
                            boolean	 in_module)

{
   int			asg_idx;
   int			base_attr;
   int			col;
   int			comp_attr;
   int			dope_idx = NULL_IDX;
   int			dv_tmp_idx;
   expr_arg_type	exp_desc;
   long_type		idx_constant;
   opnd_type		l_opnd;
   int			line;
   int			list_idx;
   int			loc_idx;
   long64		num;
   int			offset;
   boolean		ok;
   opnd_type		opnd;
   opnd_type		opnd2;
   int			rank_idx = NULL_IDX;
   int			size;
   int			sn_idx;
   int			static_tmp_idx;
   int			sub_idx;
   int			struct_idx;
   long_type		the_constant[2];
#ifdef KEY /* Bug 10177 */
   int			tmp_idx = 0;
#else /* KEY Bug 10177 */
   int			tmp_idx;
#endif /* KEY Bug 10177 */
   int			type_idx;
   int			type_idx2;
   int			val_type;

# ifdef _INIT_RELOC_BASE_OFFSET
   int			attr_idx;
# endif

   nmlist_struclist_t	*struct_hdr;
   nmlist_goli_t	*goli_ptr;

   TRACE (Func_Entry, "create_strct_tbl", NULL);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   type_idx2 = SA_INTEGER_DEFAULT_TYPE;
# else
   type_idx2 = CG_INTEGER_DEFAULT_TYPE;
# endif

   base_attr = find_base_attr(base_opnd, &line, &col);
   type_idx  = TYP_IDX(ATD_TYPE_IDX(base_attr)); /* Structure index */

   /**********************************\
   |* create static struct tbl attr. *|
   \**********************************/

   if (two_word_fcd) {
      size = NML_STRCT_HDR_SIZE_FCD2 +
                   (NML_STRCT_ITEM_SIZE_FCD2 * ATT_NUM_CPNTS(type_idx));
   }
   else {
      size = NML_STRCT_HDR_SIZE +
                   (NML_STRCT_ITEM_SIZE * ATT_NUM_CPNTS(type_idx));
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   /* the version item is always 64 bits */
   /* add one for the header version, and one for each cpnt entry */
   if (TYP_LINEAR(type_idx2) == Integer_4) {
      size += 1 + ATT_NUM_CPNTS(type_idx);
   }
# endif

   static_tmp_idx = gen_static_integer_array_tmp(size,line,col);

   if (! in_module) {

      /***************************\
      |* create struct tbl attr. *|
      \***************************/

      tmp_idx = gen_compiler_tmp(line,col, Priv, TRUE);
      ATD_TYPE_IDX(tmp_idx) = type_idx2;
      AT_SEMANTICS_DONE(tmp_idx)	= TRUE;

      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

      ATD_ARRAY_IDX(tmp_idx) = ATD_ARRAY_IDX(static_tmp_idx);

      /***********************************\
      |* copy static attr to stack attr. *|
      \***********************************/


      gen_opnd(&opnd, tmp_idx, AT_Tbl_Idx, line, col);
      ok = gen_whole_subscript(&opnd, &exp_desc);

      gen_opnd(&opnd2, static_tmp_idx, AT_Tbl_Idx, line, col);
      ok = gen_whole_subscript(&opnd2, &exp_desc);

      asg_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                   Asg_Opr, type_idx2, line, col,
                       OPND_FLD(opnd2), OPND_IDX(opnd2));

      gen_sh(After, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }


   /*************************************************\
   |* set first word with number of components.     *|
   \*************************************************/

   idx_constant = 1;

# if defined(_BITFIELD_RIGHT_TO_LEFT)		/* Most x86 platforms */
   the_constant[0] = (long_type) ATT_NUM_CPNTS(type_idx) << 16;
# else
   the_constant[0] = 0;
   the_constant[1] = 0;

   struct_hdr = (nmlist_struclist_t *)the_constant;

   struct_hdr->structlen = ATT_NUM_CPNTS(type_idx);
# endif

   gen_opnd(&opnd,
            ntr_const_tbl((sizeof(nmlist_struclist_t) == 8) ? Integer_8 :
                                                              Integer_4,
                          FALSE,
                          the_constant),
            CN_Tbl_Idx,
            line,
            col);

   gen_array_element_init(static_tmp_idx,
                          &idx_constant,
                          &opnd,
                          Init_Opr,
                          NULL_IDX);


   /*************************************************************\
   |* set next word with loc of dope vector or address of strct *|
   \*************************************************************/

   if (ATD_ARRAY_IDX(base_attr)) {
      /* get dope vector */

      COPY_OPND(opnd, (*base_opnd));
      exp_desc = init_exp_desc;
      ok = gen_whole_subscript(&opnd, &exp_desc);
      COPY_OPND((*base_opnd), opnd);

      if (in_module) {
         namelist_static_dv_whole_def(&l_opnd, &opnd);
      }
      else {
         dv_tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

         ATD_TYPE_IDX(dv_tmp_idx)	= ATD_TYPE_IDX(base_attr);
         ATD_STOR_BLK_IDX(dv_tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
         AT_SEMANTICS_DONE(dv_tmp_idx)= TRUE;

         /* Positions 1-7 are deferred shape entries in the bd table. */

         ATD_ARRAY_IDX(dv_tmp_idx) = exp_desc.rank;
         ATD_IM_A_DOPE(dv_tmp_idx)    = TRUE;

         gen_opnd(&l_opnd, dv_tmp_idx, AT_Tbl_Idx, line, col);

         exp_desc.type_idx = ATD_TYPE_IDX(base_attr);
         exp_desc.type     = TYP_TYPE(exp_desc.type_idx);
         exp_desc.linear_type = TYP_LINEAR(exp_desc.type_idx);
   
         if (exp_desc.type == Character) {
            exp_desc.char_len.fld = TYP_FLD(exp_desc.type_idx);
            exp_desc.char_len.idx = TYP_IDX(exp_desc.type_idx);
         }
         gen_dv_whole_def(&l_opnd, &opnd, &exp_desc);
      }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      loc_idx = gen_ir(OPND_FLD(l_opnd), OPND_IDX(l_opnd),
                   Loc_Opr, CRI_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# else
      loc_idx = gen_ir(OPND_FLD(l_opnd), OPND_IDX(l_opnd),
                   in_module ? Aloc_Opr : Loc_Opr, CRI_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# endif

      rank_idx = 0;
      dope_idx = 0;
      make_base_subtree(base_opnd, &l_opnd, &rank_idx, &dope_idx);
      COPY_OPND((*base_opnd), l_opnd);

   }
   else {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      loc_idx = gen_ir(OPND_FLD((*base_opnd)), OPND_IDX((*base_opnd)),
                   Loc_Opr, CRI_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# else
      loc_idx = gen_ir(OPND_FLD((*base_opnd)), OPND_IDX((*base_opnd)),
                   in_module ? Aloc_Opr : Loc_Opr, CRI_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# endif

# ifdef _TRANSFORM_CHAR_SEQUENCE
# ifdef _TARGET_OS_UNICOS
      if (TYP_TYPE(ATD_TYPE_IDX(base_attr)) == Structure &&
          ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(base_attr)))) {

         IR_TYPE_IDX(loc_idx) = CRI_Ch_Ptr_8;
         COPY_OPND(opnd, IR_OPND_L(loc_idx));
         transform_char_sequence_ref(&opnd, ATD_TYPE_IDX(base_attr));
         COPY_OPND(IR_OPND_L(loc_idx), opnd);
      }
# endif
# endif

   }

   offset = NULL_IDX;

# ifdef _INIT_RELOC_BASE_OFFSET
   if (in_module) {

      COPY_OPND(opnd, IR_OPND_L(loc_idx));
      offset = change_to_base_and_offset(&opnd, &opnd2);
      COPY_OPND(IR_OPND_L(loc_idx), opnd2);

      /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
      /* Create a temp as an overlay for the first object if   */
      /* there is no FIRST attr.                               */

      attr_idx = find_left_attr(&(IR_OPND_L(loc_idx)));

      if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
         set_sb_first_attr_idx(attr_idx);
      }
   }
# endif

   gen_opnd(&opnd, loc_idx, IR_Tbl_Idx, line, col);

   gen_array_element_init(in_module ? static_tmp_idx : tmp_idx,
                          &idx_constant,
                          &opnd,
                          in_module ? Init_Reloc_Opr : Asg_Opr,
                          offset);

   sn_idx = ATT_FIRST_CPNT_IDX(type_idx);

   while (sn_idx != NULL_IDX) {

      comp_attr = SN_ATTR_IDX(sn_idx);

      /***************************************************\
      |* set the valtype in the first word of item entry *|
      \***************************************************/

      if (TYP_TYPE(ATD_TYPE_IDX(comp_attr)) == Structure) {

         if (ATD_ARRAY_IDX(comp_attr)) {
            val_type = NML_VALTYPE_STRCT_ARRAY;
         }
         else {
            val_type = NML_VALTYPE_STRCT;
         }
      }
      else if (ATD_ARRAY_IDX(comp_attr)) {
         val_type = NML_VALTYPE_ARRAY;
      }
      else {
         val_type = NML_VALTYPE_SCALAR;
      }

# if defined(_BITFIELD_RIGHT_TO_LEFT)           /* Most x86 platforms */
      the_constant[0] = val_type;
# else

      the_constant[0] = 0;
      the_constant[1] = 0;

      goli_ptr = (nmlist_goli_t *)the_constant;

      goli_ptr->valtype = val_type;
# endif

      gen_opnd(&opnd,
               ntr_const_tbl((sizeof(nmlist_goli_t) == 8) ? Integer_8 :
                                                            Integer_4,
                             FALSE,
                             the_constant),
               CN_Tbl_Idx,
               line,
               col);

      gen_array_element_init(static_tmp_idx,
                             &idx_constant,
                             &opnd,
                             Init_Opr,
                             NULL_IDX);


      /***************************************\
      |* set the fcd for the group item name *|
      \***************************************/

      put_string_in_tmp(AT_OBJ_NAME_PTR(comp_attr),
                        AT_NAME_LEN(comp_attr),
                       &opnd);


# ifdef _INIT_RELOC_BASE_OFFSET
      if (in_module) {
         /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
         /* Create a temp as an overlay for the first object if   */
         /* there is no FIRST attr.                               */

         attr_idx = find_left_attr(&opnd);

         if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
            set_sb_first_attr_idx(attr_idx);
         }
      }
# endif

      /* tmp is character */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                   Loc_Opr, CRI_Ch_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# else
      loc_idx = gen_ir(OPND_FLD(opnd), OPND_IDX(opnd),
                   Aloc_Opr, CRI_Ch_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);
# endif

      gen_opnd(&opnd, loc_idx, IR_Tbl_Idx, line, col);

      gen_array_element_init(in_module ? static_tmp_idx : tmp_idx,
                             &idx_constant,
                             &opnd,
                             in_module ? Init_Reloc_Opr : Asg_Opr,
                             NULL_IDX);

      if (two_word_fcd) {
         /* fill in the length explicitly */

         if (char_len_in_bytes) {
            /* length is in bytes on solaris */
            num = AT_NAME_LEN(comp_attr);
         }
         else {
            /* length is in bits on mpp. */
            num = AT_NAME_LEN(comp_attr) * CHAR_BIT;
         }

         gen_opnd(&opnd,
                  C_INT_TO_CN(type_idx2, num),
                  CN_Tbl_Idx,
                  line,
                  col);

         gen_array_element_init(static_tmp_idx,
                                &idx_constant,
                                &opnd,
                                Init_Opr,
                                NULL_IDX);

      }
      
      /*******************************************\
      |* Now for the varieties of the third word *|
      \*******************************************/

      NTR_IR_TBL(struct_idx);
      IR_OPR(struct_idx) = Struct_Opr;
      IR_TYPE_IDX(struct_idx) = ATD_TYPE_IDX(comp_attr);
      IR_LINE_NUM(struct_idx) = line;
      IR_COL_NUM(struct_idx) = col;

      COPY_OPND(IR_OPND_L(struct_idx), (*base_opnd));
      IR_FLD_R(struct_idx) = AT_Tbl_Idx;
      IR_IDX_R(struct_idx) = comp_attr;
      IR_LINE_NUM_R(struct_idx) = line;
      IR_COL_NUM_R(struct_idx) = col;

      gen_opnd(&opnd, struct_idx, IR_Tbl_Idx, line, col);

      switch (val_type) {
         case NML_VALTYPE_SCALAR :
            /* get scalar type tbl */

            loc_idx = gen_ir(AT_Tbl_Idx, create_scalar_type_tbl(&opnd,
                                                                in_module),
                         Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);

            break;

         case NML_VALTYPE_ARRAY :
            /* get dope vector */

            exp_desc = init_exp_desc;
            ok = gen_whole_subscript(&opnd, &exp_desc);

            if (in_module) {
               namelist_static_dv_whole_def(&l_opnd, &opnd);
            }
            else {
               dv_tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

               ATD_TYPE_IDX(dv_tmp_idx)	  = ATD_TYPE_IDX(comp_attr);
               ATD_STOR_BLK_IDX(dv_tmp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
               AT_SEMANTICS_DONE(dv_tmp_idx) = TRUE;

               /* Positions 1-7 are deferred shape entries in the bd table. */

               ATD_ARRAY_IDX(dv_tmp_idx) = exp_desc.rank;
               ATD_IM_A_DOPE(dv_tmp_idx)    = TRUE;

               gen_opnd(&l_opnd, dv_tmp_idx, AT_Tbl_Idx, line, col);

               exp_desc.type_idx = ATD_TYPE_IDX(comp_attr);
               exp_desc.type     = TYP_TYPE(exp_desc.type_idx);
               exp_desc.linear_type = TYP_LINEAR(exp_desc.type_idx);

               if (exp_desc.type == Character) {
                  exp_desc.char_len.fld = TYP_FLD(exp_desc.type_idx);
                  exp_desc.char_len.idx = TYP_IDX(exp_desc.type_idx);
               }
               gen_dv_whole_def(&l_opnd, &opnd, &exp_desc);
            }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            loc_idx = gen_ir(OPND_FLD(l_opnd), OPND_IDX(l_opnd),
                         Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);
# else
            loc_idx = gen_ir(OPND_FLD(l_opnd), OPND_IDX(l_opnd),
                         in_module ? Aloc_Opr : Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);
# endif
            break;

         case NML_VALTYPE_STRCT :
         case NML_VALTYPE_STRCT_ARRAY :
            /* get struct tbl */

            loc_idx = gen_ir(AT_Tbl_Idx, create_strct_tbl(&opnd, in_module),
                         Loc_Opr, CRI_Ptr_8, line, col,
                             NO_Tbl_Idx, NULL_IDX);

            break;
      }

# ifdef _INIT_RELOC_BASE_OFFSET
      if (in_module) {

         /* Check to make sure that SB_FIRST_ATTR_IDX has a value.*/
         /* Create a temp as an overlay for the first object if   */
         /* there is no FIRST attr.                               */

         attr_idx = find_left_attr(&(IR_OPND_L(loc_idx)));

         if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
            set_sb_first_attr_idx(attr_idx);
         }
      }
# endif

      gen_opnd(&opnd, loc_idx, IR_Tbl_Idx, line, col);

      gen_array_element_init(in_module ? static_tmp_idx : tmp_idx,
                             &idx_constant,
                             &opnd,
                             in_module ? Init_Reloc_Opr : Asg_Opr,
                             NULL_IDX);

      sn_idx	= SN_SIBLING_LINK(sn_idx);
   }


   TRACE (Func_Exit, "create_strct_tbl", NULL);

   return(in_module ? static_tmp_idx : tmp_idx);

}  /* create_strct_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Put a string into the constant table, initialize a static tmp with    *|
|*      the constant, return the tmp idx.                                     *|
|*									      *|
|* Input parameters:							      *|
|*	str - address of string.                                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	tmp_idx                                                               *|
|*									      *|
\******************************************************************************/

static void put_string_in_tmp(char	 *str,
                              int         len,
                              opnd_type  *opnd)

{
   int		col;
   int		const_idx;
   int		init_idx;
   int		line;
   int		list_idx;
   boolean	ok;
   int		save_curr_stmt_sh_idx;
   int		tmp_idx;
   int		type_idx;


   /* NOTE - If this is ever called with a str that is not on a word boundary */
   /*        the call to ntr_const_tbl will not work correctly.  ntr_const_tbl*/
   /*        will have to be called with a NULL, and the string hand copied.  */

   TRACE (Func_Entry, "put_string_in_tmp", NULL);

   line				= SH_GLB_LINE(curr_stmt_sh_idx);
   col				= SH_COL_NUM(curr_stmt_sh_idx);
   save_curr_stmt_sh_idx	= curr_stmt_sh_idx;
   curr_stmt_sh_idx		= SCP_FIRST_SH_IDX(curr_scp_idx);

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Character;
   TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
   TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
   TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
   TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, len);
   type_idx			= ntr_type_tbl();
   const_idx			= ntr_const_tbl(type_idx, 
                                                TRUE, 
                                                (long_type *) str);

   tmp_idx			= gen_compiler_tmp(line, col, Shared, TRUE);

   ATD_TYPE_IDX(tmp_idx)	= type_idx;
   ATD_SAVED(tmp_idx)		= TRUE;
   ATD_DATA_INIT(tmp_idx)	= TRUE;
   ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
   AT_SEMANTICS_DONE(tmp_idx)	= TRUE;

   OPND_FLD((*opnd))		= AT_Tbl_Idx;
   OPND_IDX((*opnd))		= tmp_idx;
   OPND_LINE_NUM((*opnd))	= line;
   OPND_COL_NUM((*opnd))	= col;

   ok				= gen_whole_substring(opnd, 0);

   NTR_IR_TBL(init_idx);
   IR_OPR(init_idx)		= Init_Opr;
   IR_TYPE_IDX(init_idx)        = TYPELESS_DEFAULT_TYPE;

   IR_LINE_NUM(init_idx)	= line;
   IR_COL_NUM(init_idx)		= col;
   IR_LINE_NUM_R(init_idx)	= line;
   IR_COL_NUM_R(init_idx)	= col;

   COPY_OPND(IR_OPND_L(init_idx), (*opnd));

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(init_idx)		= IL_Tbl_Idx;
   IR_IDX_R(init_idx)		= list_idx;
   IR_LIST_CNT_R(init_idx)	= 3;

   IL_FLD(list_idx)		= CN_Tbl_Idx;
   IL_IDX(list_idx)		= const_idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));

   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx))	= list_idx;
   list_idx					= IL_NEXT_LIST_IDX(list_idx);
   IL_FLD(list_idx)				= CN_Tbl_Idx;
   IL_IDX(list_idx)				= CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));

   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx					= IL_NEXT_LIST_IDX(list_idx);
   IL_FLD(list_idx)				= CN_Tbl_Idx;
   IL_IDX(list_idx)				= CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   gen_sh(After,
          Assignment_Stmt,
          line,
          col,
          FALSE,
          FALSE,
          TRUE);

   SH_IR_IDX(curr_stmt_sh_idx)		= init_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx)	= TRUE;
   curr_stmt_sh_idx			= save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "put_string_in_tmp", NULL);

   return;

}  /* put_string_in_tmp */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Take a reference and return the base attr and the offset to the       *|
|*      subobject (if any). This is used for Init_Reloc_Opr stuff because     *|
|*      rcg doesn't handle this very well.                                    *|
|*									      *|
|* Input parameters:							      *|
|*	ref_opnd - address of opnd pointing to reference tree.                *|
|*									      *|
|* Output parameters:							      *|
|*	base_opnd - address of opnd to point to base attr.                    *|
|*									      *|
|* Returns:								      *|
|*	the offset in bits.                                                   *|
|*									      *|
\******************************************************************************/

# ifdef _INIT_RELOC_BASE_OFFSET
static int	change_to_base_and_offset(opnd_type *ref_opnd,
					  opnd_type *base_opnd)
					

{
   int			col;
   size_offset_type	cpnt_offset;
   int			line;
   int			offset_idx;
   size_offset_type	offset;
   opnd_type		opnd;
   boolean		unused;


   TRACE (Func_Entry, "change_to_base_and_offset", NULL);

   if (OPND_FLD((*ref_opnd)) == AT_Tbl_Idx) {
      offset_idx = CN_INTEGER_ZERO_IDX;
      COPY_OPND((*base_opnd), (*ref_opnd));
   }
   else {
      COPY_OPND(opnd, (*ref_opnd));

      offset.idx		= CN_INTEGER_ZERO_IDX;
      offset.fld		= CN_Tbl_Idx;

      while (OPND_FLD(opnd) != AT_Tbl_Idx) {

         switch (IR_OPR(OPND_IDX(opnd))) {
            case Whole_Subscript_Opr :
            case Section_Subscript_Opr :
            case Subscript_Opr :
            case Whole_Substring_Opr :
            case Substring_Opr :
               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
               break;

            case Struct_Opr :

               cpnt_offset.idx	= ATD_CPNT_OFFSET_IDX(IR_IDX_R(OPND_IDX(opnd)));
               cpnt_offset.fld	= ATD_OFFSET_FLD(IR_IDX_R(OPND_IDX(opnd)));

               size_offset_binary_calc(&offset,
                                       &cpnt_offset,
                                        Plus_Opr,
                                       &offset);

               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
               break;

            default :
               find_opnd_line_and_column(&opnd, &line, &col);
               PRINTMSG(line, 1048, Internal, col);
               break;
         }
      }

      if (offset.fld == NO_Tbl_Idx) {
         offset_idx = ntr_const_tbl(offset.type_idx, FALSE, offset.constant);
      }
      else if (offset.fld == CN_Tbl_Idx) {
         offset_idx = offset.idx;
      }
      else {
         PRINTMSG(OPND_LINE_NUM(opnd), 1201, Internal, OPND_COL_NUM(opnd),
                  AT_OBJ_NAME_PTR(IR_IDX_R(OPND_IDX(opnd))));
      }

      COPY_OPND((*base_opnd), opnd);

      if (TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(opnd))) == Character) {
         unused = gen_whole_substring(base_opnd, 0);
      } 
   }

   TRACE (Func_Exit, "change_to_base_and_offset", NULL);

   return(offset_idx);

}  /* change_to_base_and_offset */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set the defined flag on all the namelist group attrs. Check for       *|
|*      live do control variables and intent in dummy args.                   *|
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

static boolean do_read_namelist_semantics(opnd_type	*namelist_opnd)

{
   int			attr_idx;
   int			col;
   int			line;
   int			namelist_attr;
   opnd_type		opnd;
   boolean		semantically_correct = TRUE;
   int			sn_idx;


   TRACE (Func_Entry, "do_read_namelist_semantics", NULL);

   namelist_attr = OPND_IDX((*namelist_opnd));
   line          = OPND_LINE_NUM((*namelist_opnd));
   col           = OPND_COL_NUM((*namelist_opnd));

   sn_idx = ATN_FIRST_NAMELIST_IDX(namelist_attr);

   while (sn_idx != NULL_IDX) {
      attr_idx             = SN_ATTR_IDX(sn_idx);
      AT_DEFINED(attr_idx) = TRUE;

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

         gen_opnd(&opnd, attr_idx, AT_Tbl_Idx, line, col);

         if (! check_for_legal_define(&opnd)) {
            semantically_correct	= FALSE;
            sn_idx			= SN_SIBLING_LINK(sn_idx);
            continue;
         }
      }

      if (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
         AT_DEF_IN_CHILD(attr_idx) = TRUE;

         do {
            attr_idx			= AT_ATTR_LINK(attr_idx);
            AT_DEF_IN_CHILD(attr_idx)	= TRUE;
         }
         while (AT_ATTR_LINK(attr_idx) != NULL_IDX);
      }

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          ATD_CLASS(attr_idx) == Dummy_Argument   &&
          ATD_INTENT(attr_idx) == Intent_In) {
         PRINTMSG(line, 890, Error, col, AT_OBJ_NAME_PTR(attr_idx));
         semantically_correct = FALSE;
      }

      sn_idx               = SN_SIBLING_LINK(sn_idx);
   }

   TRACE (Func_Exit, "do_read_namelist_semantics", NULL);

   return(semantically_correct);

}  /* do_read_namelist_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Set the referenced field on all the namelist group attrs.             *|
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

static void do_write_namelist_semantics(opnd_type     *namelist_opnd)

{
   int                  attr_idx;
   int                  namelist_attr;
   int                  sn_idx;

   TRACE (Func_Entry, "do_write_namelist_semantics", NULL);

   namelist_attr = OPND_IDX((*namelist_opnd));

   sn_idx = ATN_FIRST_NAMELIST_IDX(namelist_attr);

   while (sn_idx != NULL_IDX) {
      attr_idx                = SN_ATTR_IDX(sn_idx);
      AT_REFERENCED(attr_idx) = Referenced;

      if (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
         AT_REF_IN_CHILD(attr_idx) = TRUE;

         do {
            attr_idx			= AT_ATTR_LINK(attr_idx);
            AT_REF_IN_CHILD(attr_idx)	= TRUE;
         }
         while (AT_ATTR_LINK(attr_idx) != NULL_IDX);
      }

      sn_idx                  = SN_SIBLING_LINK(sn_idx);
   }

   TRACE (Func_Exit, "do_write_namelist_semantics", NULL);

   return;

}  /* do_write_namelist_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine breaks up structure objects in io lists into component   *|
|*      references.                                                           *|
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

static int discombobulate_structure_ref(opnd_type	*base_opnd,
					int		 type_idx,
					int		*list_idx)

{
   int		 attr_idx;
   int		 cnt;
   int		 col;
   int		 deref_idx;
   expr_arg_type exp_desc;
   boolean       first_item = TRUE;
   int           imp_idx;
   int		 ir_idx;
   int		 line;
   opnd_type	 loc_base_opnd;
   int		 new_list_idx;
   int		 next_list_idx;
   int           num_items = 0;
   boolean       ok;
   opnd_type     opnd;
   int		 sn_idx;
   int		 struct_idx;
   int		 tmp_list_idx;

 
   TRACE (Func_Entry, "discombobulate_structure_ref", NULL);

   new_list_idx = *list_idx;
   next_list_idx = IL_NEXT_LIST_IDX(new_list_idx);

   attr_idx = find_base_attr(base_opnd, &line, &col);

   sn_idx = ATT_FIRST_CPNT_IDX(type_idx);

   COPY_OPND(loc_base_opnd, (*base_opnd));

   while (sn_idx != NULL_IDX) {
      attr_idx = SN_ATTR_IDX(sn_idx);

      NTR_IR_TBL(struct_idx);
      IR_OPR(struct_idx) = Struct_Opr;
      IR_TYPE_IDX(struct_idx) = ATD_TYPE_IDX(attr_idx);
      IR_LINE_NUM(struct_idx) = line;
      IR_COL_NUM(struct_idx)  = col;
      COPY_OPND(IR_OPND_L(struct_idx), loc_base_opnd);

      if (SN_SIBLING_LINK(sn_idx) != NULL_IDX) {
         copy_subtree(&loc_base_opnd, &loc_base_opnd);
      }

      IR_FLD_R(struct_idx)      = AT_Tbl_Idx;
      IR_IDX_R(struct_idx)      = attr_idx;
      IR_LINE_NUM_R(struct_idx) = line;
      IR_COL_NUM_R(struct_idx)  = col;

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = struct_idx;

      if (ATD_POINTER(attr_idx)) {
         NTR_IR_TBL(deref_idx);
         IR_OPR(deref_idx)      = Dv_Deref_Opr;
         IR_TYPE_IDX(deref_idx) = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(deref_idx) = line;
         IR_COL_NUM(deref_idx)  = col;
         COPY_OPND(IR_OPND_L(deref_idx), opnd);
         OPND_FLD(opnd)         = IR_Tbl_Idx;
         OPND_IDX(opnd)         = deref_idx;
      }

      if (ATD_ARRAY_IDX(attr_idx)) {
         ok = gen_whole_subscript(&opnd, &exp_desc);
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
         ok = gen_whole_substring(&opnd, 0);
      }

      if (first_item) {
         first_item = FALSE;
      }
      else {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(new_list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(new_list_idx)) = new_list_idx;
         new_list_idx = IL_NEXT_LIST_IDX(new_list_idx);
         num_items++;
      }

      COPY_OPND(IL_OPND(new_list_idx), opnd);

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {

         if (ATD_ARRAY_IDX(attr_idx)) {
            tmp_list_idx = new_list_idx;
            imp_idx = change_section_to_do(&tmp_list_idx);
            COPY_OPND(opnd, IL_OPND(tmp_list_idx));
            cnt = discombobulate_structure_ref( &opnd,
                                      TYP_IDX(ATD_TYPE_IDX(attr_idx)),
                                      &tmp_list_idx);
            IR_LIST_CNT_L(imp_idx) += cnt;
         }
         else {
            num_items += discombobulate_structure_ref(
                                        &opnd, 
                                        TYP_IDX(ATD_TYPE_IDX(attr_idx)),
                                         &new_list_idx);
         }
      }
      else {
         /* insert the Io_Item_Type_Code_Opr */

         COPY_OPND(opnd, IL_OPND(new_list_idx));

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Io_Item_Type_Code_Opr;
         IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx) = col;

         COPY_OPND(IR_OPND_L(ir_idx), opnd);
         IL_FLD(new_list_idx) = IR_Tbl_Idx;
         IL_IDX(new_list_idx) = ir_idx;
      }

      sn_idx	= SN_SIBLING_LINK(sn_idx);
   }

   IL_NEXT_LIST_IDX(new_list_idx) = next_list_idx;
   *list_idx = new_list_idx;

   TRACE (Func_Exit, "discombobulate_structure_ref", NULL);

   return(num_items);

}  /* discombobulate_structure_ref */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine checks the existing subtree that is type structure and   *|
|*      creates implied do loops for any sections.                            *|
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

static int change_section_to_do(int	*list_idx)

{
   int		col;
   expr_arg_type	exp_desc;
   int		imp_idx;
   int		ir_idx;
   int          i;
   int		k;
   int		line;
   opnd_type	opnd;
   int          rank = 1;
   int		return_imp_idx;
#ifdef KEY /* Bug 10177 */
   int          return_list_idx = 0;
#else /* KEY Bug 10177 */
   int          return_list_idx;
#endif /* KEY Bug 10177 */
   int          sub_list_idx;
   int		tmp_idx;
   int          tmp_list_idx;
   opnd_type    tmp_opnd;
   int		trip_list_idx;


   TRACE (Func_Entry, "change_section_to_do", NULL);

   COPY_OPND(opnd, IL_OPND((*list_idx)));
   find_opnd_line_and_column(&opnd, &line, &col);

   while (OPND_FLD(opnd) == IR_Tbl_Idx) {

      ir_idx = OPND_IDX(opnd);

      if (IR_OPR(ir_idx) == Whole_Subscript_Opr    ||
          IR_OPR(ir_idx) == Section_Subscript_Opr) {

         IR_OPR(ir_idx) = Subscript_Opr;

         /* create first implied do */
         NTR_IR_TBL(imp_idx);
         return_imp_idx         = imp_idx;
         IR_OPR(imp_idx)        = Implied_Do_Opr;
         IR_TYPE_IDX(imp_idx)   = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(imp_idx)   = line;
         IR_COL_NUM(imp_idx)    = col;

         NTR_IR_LIST_TBL(return_list_idx);
         IR_FLD_L(imp_idx)      = IL_Tbl_Idx;
         IR_LIST_CNT_L(imp_idx) = 1;
         IR_IDX_L(imp_idx)      = return_list_idx;

         COPY_OPND(IL_OPND(return_list_idx), IL_OPND((*list_idx)));
         IL_FLD((*list_idx))    = IR_Tbl_Idx;
         IL_IDX((*list_idx))    = imp_idx;

         sub_list_idx = IR_IDX_R(ir_idx);
         for (i = 0; i < IR_LIST_CNT_R(ir_idx); i++) {
            
            if (IL_VECTOR_SUBSCRIPT(sub_list_idx)) {
               /* ultimately we must have a triplet opr.   */
               /* this section just finds that triplet opr */

               COPY_OPND(tmp_opnd, IL_OPND(sub_list_idx));

               trip_list_idx = NULL_IDX;
               while (trip_list_idx == NULL_IDX) {

                  while (OPND_FLD(tmp_opnd) == IR_Tbl_Idx) {

                     if (IR_OPR(OPND_IDX(tmp_opnd)) == Whole_Subscript_Opr  ||
                         IR_OPR(OPND_IDX(tmp_opnd)) == Section_Subscript_Opr) {

                        tmp_list_idx = IR_IDX_R(OPND_IDX(tmp_opnd));

                        for (k = 0; k < IR_LIST_CNT_R(OPND_IDX(tmp_opnd)); 
                                                                        k++) {
                           if (IL_VECTOR_SUBSCRIPT(tmp_list_idx)) {
                              COPY_OPND(tmp_opnd, IL_OPND(tmp_list_idx));
                              break;
                           }
                           else if (IL_FLD(tmp_list_idx) == IR_Tbl_Idx &&
                                  IR_OPR(IL_IDX(tmp_list_idx)) == Triplet_Opr) {

                              trip_list_idx = tmp_list_idx;
                              break;
                           }

                           tmp_list_idx = IL_NEXT_LIST_IDX(tmp_list_idx);
                        }
                        break;
                     }

                     COPY_OPND(tmp_opnd, IR_OPND_L(OPND_IDX(tmp_opnd)));
                  }
               }
            }
            else {
               trip_list_idx = sub_list_idx;
            }

            if (IL_FLD(trip_list_idx)         == IR_Tbl_Idx   &&
                IR_OPR(IL_IDX(trip_list_idx)) == Triplet_Opr) {

               if (rank > 1) {
                  /* generate new implied do */
                  NTR_IR_LIST_TBL(tmp_list_idx);
                  IL_FLD(tmp_list_idx)   = IR_Tbl_Idx;
                  IL_IDX(tmp_list_idx)   = imp_idx;

                  NTR_IR_TBL(imp_idx);
                  IR_OPR(imp_idx)        = Implied_Do_Opr;
                  IR_TYPE_IDX(imp_idx)   = TYPELESS_DEFAULT_TYPE;
                  IR_LINE_NUM(imp_idx)   = line;
                  IR_COL_NUM(imp_idx)    = col;
                  IR_FLD_L(imp_idx)      = IL_Tbl_Idx;
                  IR_LIST_CNT_L(imp_idx) = 1;
                  IR_IDX_L(imp_idx)      = tmp_list_idx;
                  IL_IDX((*list_idx))    = imp_idx;
               }

               /* create the tmp implied do control variable. */

               tmp_idx              = gen_compiler_tmp(line, col, Priv, TRUE);
               ATD_TYPE_IDX(tmp_idx)	 = CG_INTEGER_DEFAULT_TYPE;
               ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
               AT_SEMANTICS_DONE(tmp_idx)= TRUE;

               /* hook in control var. */

               NTR_IR_LIST_TBL(tmp_list_idx);
               IR_FLD_R(imp_idx)      = IL_Tbl_Idx;
               IR_LIST_CNT_R(imp_idx) = 4;
               IR_IDX_R(imp_idx)      = tmp_list_idx;

               IL_FLD(tmp_list_idx)   = AT_Tbl_Idx;
               IL_IDX(tmp_list_idx)   = tmp_idx;
               IL_LINE_NUM(tmp_list_idx) = line;
               IL_COL_NUM(tmp_list_idx)  = col;

               /* second is start opnd */

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(tmp_list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(tmp_list_idx)) = tmp_list_idx;
               tmp_list_idx = IL_NEXT_LIST_IDX(tmp_list_idx);

               COPY_OPND(IL_OPND(tmp_list_idx), 
                         IL_OPND(IR_IDX_L(IL_IDX(trip_list_idx))));

               COPY_OPND(tmp_opnd, IL_OPND(tmp_list_idx));
               set_up_exp_desc(&tmp_opnd, &exp_desc);
#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
               cast_to_cg_default(&tmp_opnd, &exp_desc);
#endif /* KEY */
               COPY_OPND(IL_OPND(tmp_list_idx), tmp_opnd);

               /* third is end opnd */

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(tmp_list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(tmp_list_idx)) = tmp_list_idx;
               tmp_list_idx = IL_NEXT_LIST_IDX(tmp_list_idx);

               COPY_OPND(IL_OPND(tmp_list_idx),
                         IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_L(
                                  IL_IDX(trip_list_idx)))));

               COPY_OPND(tmp_opnd, IL_OPND(tmp_list_idx));
               set_up_exp_desc(&tmp_opnd, &exp_desc);
#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
               cast_to_cg_default(&tmp_opnd, &exp_desc);
#endif /* KEY */
               COPY_OPND(IL_OPND(tmp_list_idx), tmp_opnd);

               /* fourth is stride opnd */

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(tmp_list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(tmp_list_idx)) = tmp_list_idx;
               tmp_list_idx = IL_NEXT_LIST_IDX(tmp_list_idx);

               COPY_OPND(IL_OPND(tmp_list_idx),
                         IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                                       IR_IDX_L(IL_IDX(trip_list_idx))))));
                  
               COPY_OPND(tmp_opnd, IL_OPND(tmp_list_idx));
               set_up_exp_desc(&tmp_opnd, &exp_desc);
#ifdef KEY /* Bug 4709 */
         /* Converting array bounds from Integer_8 to Integer_4 breaks
	  * customer code which uses large array bounds, and isn't
	  * correct for our 64-bit-oriented runtime.
	  */
#else
               cast_to_cg_default(&tmp_opnd, &exp_desc);
#endif /* KEY */
               COPY_OPND(IL_OPND(tmp_list_idx), tmp_opnd);

               /* replace triplet with tmp control variable */

               IL_FLD(trip_list_idx) = AT_Tbl_Idx;
               IL_IDX(trip_list_idx) = tmp_idx;
               IL_LINE_NUM(trip_list_idx) = line;
               IL_COL_NUM(trip_list_idx)  = col;

               rank++;
            }
          
            

            sub_list_idx = IL_NEXT_LIST_IDX(sub_list_idx);
         }
         break;
      }

      COPY_OPND(opnd, IR_OPND_L(ir_idx));
   }

   (*list_idx) = return_list_idx;

   TRACE (Func_Exit, "change_section_to_do", NULL);

   return(return_imp_idx);

}  /* change_section_to_do */

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

static void process_deferred_io_list(void)

{
   int			ir_idx;
   int                  new_root;
   int			next_stmt_idx;
   int			save_curr_stmt_sh_idx;


   TRACE (Func_Entry, "process_deferred_io_list", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   alt_return_branch_idx = NULL_IDX;

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   if (io_stmt_must_be_split) {

      if (IR_OPR(ir_idx) == Alt_Return_Opr) {

# ifdef _DEBUG
         if (IR_OPR(SH_IR_IDX(SH_NEXT_IDX(curr_stmt_sh_idx))) != Br_True_Opr) {
            PRINTMSG(stmt_start_line, 737, Internal, stmt_start_col);
         }
# endif
         alt_return_branch_idx = SH_IR_IDX(SH_NEXT_IDX(curr_stmt_sh_idx));
         ir_idx = IR_IDX_R(ir_idx);
      }
      else if (IR_OPR(ir_idx) == Asg_Opr) {
         ir_idx = IR_IDX_R(ir_idx);
      }

      new_root = copy_text_for_expansion(FL_IO_FIRST);

      gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = new_root;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      if (alt_return_branch_idx) {
         gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = alt_return_branch_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      }

      if (stmt_type == Inquire_Stmt) {
         IL_IDX(IR_IDX_L(ir_idx)) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                FL_IO_MIDDLE);
      }
      else {
         IL_IDX(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx)))) =
                                          C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      FL_IO_MIDDLE);
      }

      new_root = copy_text_for_expansion(FL_IO_LAST);

      if (alt_return_branch_idx) {
         gen_sh(After, If_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      }

      gen_sh(After, stmt_type, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(curr_stmt_sh_idx)     = new_root;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      next_stmt_idx = curr_stmt_sh_idx;

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

      expand_io_list();

      curr_stmt_sh_idx = next_stmt_idx;
   }
   else {
      next_stmt_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

      expand_io_list();

      curr_stmt_sh_idx = SH_PREV_IDX(next_stmt_idx);
   }

   TRACE (Func_Exit, "process_deferred_io_list", NULL);

   return;

}  /* process_deferred_io_list */

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

static void expand_io_list(void)

{
   int			cnt = 0;
   expr_arg_type	exp_desc;
#ifndef KEY /* Bug 10008 */
   int			i;
#endif /* KEY Bug 10008 */
   int			imp_idx;
   int			io_idx;
   int			ir_idx;
   opnd_type		left_opnd;
   int			list_idx;
   int			new_root;
   int			next_stmt_idx;
   opnd_type		opnd;
   int			prev_list_idx;
   int			save_curr_stmt_sh_idx;
   int			struct_list_idx;
   int			tmp_asg_sh_idx;
   int			tmp_idx;


   TRACE (Func_Entry, "expand_io_list", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   next_stmt_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

   if (IR_OPR(ir_idx) == Alt_Return_Opr) {
      ir_idx = IR_IDX_R(ir_idx);
      next_stmt_idx = SH_NEXT_IDX(next_stmt_idx);
   }
   else if (IR_OPR(ir_idx) == Asg_Opr) {
      ir_idx = IR_IDX_R(ir_idx);
   }

   list_idx = IR_IDX_R(ir_idx);
   prev_list_idx = NULL_IDX;

   while (list_idx != NULL_IDX) {
      cnt++;
      new_root = NULL_IDX;

      if (IL_NONDEFAULT_IMP_DO_LCV(list_idx)) {
         /* put the assignment of the original lcv in place */

         if (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
            new_root = copy_text_for_expansion(FL_IO_MIDDLE);

            if (IR_OPR(new_root) == Alt_Return_Opr ||
                IR_OPR(new_root) == Asg_Opr)       {

               io_idx = IR_IDX_R(new_root);
            }
            else {
               io_idx = new_root;
            }

            IR_FLD_R(io_idx) = IL_Tbl_Idx;
            IR_IDX_R(io_idx) = IL_NEXT_LIST_IDX(list_idx);
            IR_LIST_CNT_R(io_idx) = IR_LIST_CNT_R(ir_idx) - cnt;
         }

         gen_sh(After, stmt_type, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx)     = IL_IDX(IR_IDX_R(IL_IDX(list_idx)));
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

# ifdef _DEBUG
         if (IL_FLD(IR_IDX_R(IL_IDX(list_idx))) != IR_Tbl_Idx ||
             IR_OPR(IL_IDX(IR_IDX_R(IL_IDX(list_idx)))) != Asg_Opr ||
             IR_FLD_R(IL_IDX(IR_IDX_R(IL_IDX(list_idx)))) != AT_Tbl_Idx) {

            PRINTMSG(stmt_start_line, 1050, Internal, stmt_start_col);
         }
# endif
         IL_FLD(IR_IDX_R(IL_IDX(list_idx))) = AT_Tbl_Idx;
         IL_IDX(IR_IDX_R(IL_IDX(list_idx))) = 
                                 IR_IDX_R(IL_IDX(IR_IDX_R(IL_IDX(list_idx))));
         IL_LINE_NUM(IR_IDX_R(IL_IDX(list_idx))) = stmt_start_line;
         IL_COL_NUM(IR_IDX_R(IL_IDX(list_idx))) = stmt_start_col;

         IL_NONDEFAULT_IMP_DO_LCV(list_idx) = FALSE;


         if (new_root) {
            
            save_curr_stmt_sh_idx = curr_stmt_sh_idx;

            IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
            IR_LIST_CNT_R(ir_idx) = cnt;

            curr_stmt_sh_idx = next_stmt_idx;
            gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_root;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            if (alt_return_branch_idx) {
               gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

               curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            }

            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

            expand_io_list();

            curr_stmt_sh_idx = save_curr_stmt_sh_idx;
         }
      }


      if (IL_MUST_BE_LOOP(list_idx)) {
          
         if (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
            new_root = copy_text_for_expansion(FL_IO_MIDDLE);

            if (IR_OPR(new_root) == Alt_Return_Opr ||
                IR_OPR(new_root) == Asg_Opr)       {

               io_idx = IR_IDX_R(new_root);
            }
            else {
               io_idx = new_root;
            }

            IR_FLD_R(io_idx) = IL_Tbl_Idx;
            IR_IDX_R(io_idx) = IL_NEXT_LIST_IDX(list_idx);
            IR_LIST_CNT_R(io_idx) = IR_LIST_CNT_R(ir_idx) - cnt;
         }

         if (cnt == 1) {
            expand_imp_do(list_idx, 0);
         }
         else {
            IL_NEXT_LIST_IDX(prev_list_idx) = NULL_IDX;
            IR_LIST_CNT_R(ir_idx) = cnt - 1;

            expand_imp_do(list_idx, next_stmt_idx);
         }

         if (new_root) {
            curr_stmt_sh_idx = next_stmt_idx;
            gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_root;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            if (alt_return_branch_idx) {
               gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

               curr_stmt_sh_idx                = SH_PREV_IDX(curr_stmt_sh_idx);
               SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            }

            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

            expand_io_list();
         }

         break;
      }
      else if (stmt_type == Read_Stmt &&
               IL_ARG_DESC_VARIANT(list_idx) &&
               (arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.vector_subscript ||
                arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.dist_reshape_ref)) {

         COPY_OPND(opnd, IL_OPND(list_idx));

         if (OPND_FLD(opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(opnd)) == Io_Item_Type_Code_Opr) {
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         if (IL_HAS_FUNCTIONS(list_idx) ||
             three_call_model) {

            if (cnt > 1) {

               new_root = copy_text_for_expansion(FL_IO_MIDDLE);

               if (IR_OPR(new_root) == Alt_Return_Opr ||
                   IR_OPR(new_root) == Asg_Opr)       {
                  io_idx = IR_IDX_R(new_root);
               }
               else {
                  io_idx = new_root;
               }

               IR_FLD_R(io_idx) = IL_Tbl_Idx;
               IR_LIST_CNT_R(io_idx) = IR_LIST_CNT_R(ir_idx) - cnt + 1;
               IR_IDX_R(io_idx) = list_idx;

               IL_NEXT_LIST_IDX(prev_list_idx) = NULL_IDX;
               IR_LIST_CNT_R(ir_idx) = cnt - 1;

               curr_stmt_sh_idx = next_stmt_idx;
               gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
                         FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_root;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

               if (alt_return_branch_idx) {
                  gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                         FALSE, FALSE, TRUE);

                  curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
                  SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
                  SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
               }

               curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               ir_idx = io_idx;
               cnt = 1;
               new_root = NULL_IDX;
            }
         }

         exp_desc = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed;
         process_deferred_functions(&opnd);

         gen_runtime_checks(&opnd);

         if (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
            new_root = copy_text_for_expansion(FL_IO_MIDDLE);

            if (IR_OPR(new_root) == Alt_Return_Opr ||
                IR_OPR(new_root) == Asg_Opr)       {

               io_idx = IR_IDX_R(new_root);
            }
            else {
               io_idx = new_root;
            }

            IR_FLD_R(io_idx) = IL_Tbl_Idx;
            IR_IDX_R(io_idx) = IL_NEXT_LIST_IDX(list_idx);
            IR_LIST_CNT_R(io_idx) = IR_LIST_CNT_R(ir_idx) - cnt;
         }


         /* Create a temp assignment (Before) and then move just the    */
         /* tmp asg stmt from before to after. All the other generated  */
         /* stmts will be in the right place. Go to tmp asg and switch  */
         /* the left and right sides. Then place the temp in the io     */
         /* list. If there were more io list items following, split the */
         /* list and start with the new io stmt.                        */

         tmp_idx = create_tmp_asg(&opnd,
                                  &arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed,
                                  &left_opnd,
                                  Intent_In,
                                  TRUE,
                                  FALSE);

         /* move the tmp assign from before the curr stmt to after */
# ifdef _DEBUG
         if (OPND_FLD(left_opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(left_opnd)) == Stmt_Expansion_Opr) {
            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "no Stmt_Expansion_Opr", "expand_io_list");
         }
# endif

         tmp_asg_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         SH_PREV_IDX(curr_stmt_sh_idx) = SH_PREV_IDX(tmp_asg_sh_idx);

         if (SH_PREV_IDX(curr_stmt_sh_idx)) {
            SH_NEXT_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = curr_stmt_sh_idx;
         }

         SH_NEXT_IDX(tmp_asg_sh_idx) = SH_NEXT_IDX(curr_stmt_sh_idx);
         if (SH_NEXT_IDX(tmp_asg_sh_idx)) {
            SH_PREV_IDX(SH_NEXT_IDX(tmp_asg_sh_idx)) = tmp_asg_sh_idx;
         }
         SH_NEXT_IDX(curr_stmt_sh_idx) = tmp_asg_sh_idx;
         SH_PREV_IDX(tmp_asg_sh_idx) = curr_stmt_sh_idx;


         curr_stmt_sh_idx = tmp_asg_sh_idx;
         COPY_OPND(IR_OPND_L(SH_IR_IDX(curr_stmt_sh_idx)), 
                   IR_OPND_R(SH_IR_IDX(curr_stmt_sh_idx)));
         COPY_OPND(IR_OPND_R(SH_IR_IDX(curr_stmt_sh_idx)),
                   left_opnd);

         copy_subtree(&left_opnd,  &left_opnd);

         if (IL_FLD(list_idx) == IR_Tbl_Idx &&
             IR_OPR(IL_IDX(list_idx)) == Io_Item_Type_Code_Opr) {
            
            COPY_OPND(IR_OPND_L(IL_IDX(list_idx)), left_opnd);
            IR_TYPE_IDX(IL_IDX(list_idx)) = 
                         arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type_idx;
         }
         else {
            COPY_OPND(IL_OPND(list_idx), left_opnd);
         }

         if (new_root) {

            IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
            IR_LIST_CNT_R(ir_idx) = cnt;

            curr_stmt_sh_idx = next_stmt_idx;
            gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_root;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            if (alt_return_branch_idx) {
               gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

               curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            }

            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

            expand_io_list();
         }

         if (IL_STRUCT_REF(list_idx)) {

            exp_desc = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed;

            if (exp_desc.rank > 0) {
               struct_list_idx = list_idx;
               imp_idx = change_section_to_do(&struct_list_idx);
               COPY_OPND(opnd, IL_OPND(struct_list_idx));
#ifdef KEY /* Bug 10008 */
               int i = discombobulate_structure_ref(&opnd,
		  TYP_IDX(exp_desc.type_idx), &struct_list_idx);
               cnt += i;
#else /* KEY Bug 10008 */
               i = discombobulate_structure_ref(&opnd,
                                                TYP_IDX(exp_desc.type_idx),
                                                &struct_list_idx);
#endif /* KEY Bug 10008 */
               IR_LIST_CNT_L(imp_idx) += i;
            }
            else {
               COPY_OPND(opnd, IL_OPND(list_idx));
#ifdef KEY /* Bug 10008 */
               int i = discombobulate_structure_ref(&opnd,
		  TYP_IDX(exp_desc.type_idx), &list_idx);
               cnt += i;
#else /* KEY Bug 10008 */
               i = discombobulate_structure_ref(&opnd,
                                                TYP_IDX(exp_desc.type_idx),
                                                &list_idx);
#endif /* KEY Bug 10008 */
               IR_LIST_CNT_R(ir_idx) += i;
            }
         }

         break;
      }
      else if (IL_MUST_FLATTEN(list_idx)) {

         COPY_OPND(opnd, IL_OPND(list_idx));

         if (OPND_FLD(opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(opnd)) == Io_Item_Type_Code_Opr) {
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         if (IL_HAS_FUNCTIONS(list_idx) ||
             three_call_model) {

            if ((stmt_type == Read_Stmt ||
                 three_call_model)  &&
                cnt > 1)               {

               new_root = copy_text_for_expansion(FL_IO_MIDDLE);

               if (IR_OPR(new_root) == Alt_Return_Opr ||
                   IR_OPR(new_root) == Asg_Opr)       {
                  io_idx = IR_IDX_R(new_root);
               }
               else {
                  io_idx = new_root;
               }

               IR_FLD_R(io_idx) = IL_Tbl_Idx;
               IR_LIST_CNT_R(io_idx) = IR_LIST_CNT_R(ir_idx) - cnt + 1;
               IR_IDX_R(io_idx) = list_idx;

               IL_NEXT_LIST_IDX(prev_list_idx) = NULL_IDX;
               IR_LIST_CNT_R(ir_idx) = cnt - 1;

               curr_stmt_sh_idx = next_stmt_idx;
               gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
                         FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_root;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

               if (alt_return_branch_idx) {
                  gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                         FALSE, FALSE, TRUE);

                  curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
                  SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
                  SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
               }

               curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
               ir_idx = io_idx;
               cnt = 1;
            }

         }

         exp_desc = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed;
         process_deferred_functions(&opnd);

         gen_runtime_checks(&opnd);

         tmp_idx = create_tmp_asg(&opnd, 
                               &arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed,
                                  &left_opnd,
                                  (stmt_type == Read_Stmt ? 
                                      Intent_Out : Intent_In),
                                  TRUE,
                                  FALSE);

         copy_subtree(&left_opnd, &left_opnd);

         if (IL_FLD(list_idx) == IR_Tbl_Idx &&
             IR_OPR(IL_IDX(list_idx)) == Io_Item_Type_Code_Opr) {

            COPY_OPND(IR_OPND_L(IL_IDX(list_idx)), left_opnd);
            IR_TYPE_IDX(IL_IDX(list_idx)) =
                         arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed.type_idx;
         }
         else {
            COPY_OPND(IL_OPND(list_idx), left_opnd);
         }
      }
      else if (IL_HAS_FUNCTIONS(list_idx) ||
               three_call_model) {

         if ((stmt_type == Read_Stmt ||
              three_call_model) &&
             cnt > 1)               {

            new_root = copy_text_for_expansion(FL_IO_MIDDLE);

            if (IR_OPR(new_root) == Alt_Return_Opr ||
                IR_OPR(new_root) == Asg_Opr)       {

               io_idx = IR_IDX_R(new_root);
            }
            else {
               io_idx = new_root;
            }

            IR_FLD_R(io_idx) = IL_Tbl_Idx;
            IR_LIST_CNT_R(io_idx) = IR_LIST_CNT_R(ir_idx) - cnt + 1;
            IR_IDX_R(io_idx) = list_idx;

            IL_NEXT_LIST_IDX(prev_list_idx) = NULL_IDX;
            IR_LIST_CNT_R(ir_idx) = cnt - 1;

            curr_stmt_sh_idx = next_stmt_idx;
            gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_root;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

            if (alt_return_branch_idx) {
               gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

               curr_stmt_sh_idx                = SH_PREV_IDX(curr_stmt_sh_idx);
               SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            }

            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
            ir_idx = io_idx;
            cnt = 1;
         }

         COPY_OPND(opnd, IL_OPND(list_idx));
         process_deferred_functions(&opnd);
         COPY_OPND(IL_OPND(list_idx), opnd);

         gen_runtime_checks(&opnd);
      }
      else if (IL_HAS_CONSTRUCTOR(list_idx)) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         process_deferred_functions(&opnd);
         COPY_OPND(IL_OPND(list_idx), opnd);

         gen_runtime_checks(&opnd);
      }

      if (IL_STRUCT_REF(list_idx)) {

         exp_desc = arg_info_list[IL_ARG_DESC_IDX(list_idx)].ed;

         if (exp_desc.rank > 0) {
            struct_list_idx = list_idx;
            imp_idx = change_section_to_do(&struct_list_idx);
            COPY_OPND(opnd, IL_OPND(struct_list_idx));
#ifdef KEY /* Bug 10008 */
	    int i = discombobulate_structure_ref(&opnd,
	       TYP_IDX(exp_desc.type_idx), &struct_list_idx);
            cnt += i;
#else /* KEY Bug 10008 */
            i = discombobulate_structure_ref(&opnd,
                                             TYP_IDX(exp_desc.type_idx),
                                             &struct_list_idx);
#endif /* KEY Bug 10008 */
            IR_LIST_CNT_L(imp_idx) += i;
         }
         else {
            COPY_OPND(opnd, IL_OPND(list_idx));
#ifdef KEY /* Bug 10008 */
            int i = discombobulate_structure_ref(&opnd,
	       TYP_IDX(exp_desc.type_idx), &list_idx);
            cnt += i;
#else /* KEY Bug 10008 */
            i = discombobulate_structure_ref(&opnd,
                                             TYP_IDX(exp_desc.type_idx),
                                             &list_idx);
#endif /* KEY Bug 10008 */
            IR_LIST_CNT_R(ir_idx) += i;
         }
      }

      prev_list_idx = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "expand_io_list", NULL);

   return;

}  /* expand_io_list */

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

static void expand_imp_do(int	top_list_idx,
                          int  next_stmt_idx)

{
   opnd_type		end_opnd;
   int			imp_do_idx;
   opnd_type		inc_opnd;
   int			io_idx;
   int			ir_idx;
   int			lcv_attr;
   int			list_idx;
   int			new_root;
   opnd_type		start_opnd;
   

   TRACE (Func_Entry, "expand_imp_do", NULL);

# ifdef _DEBUG
   if (! io_stmt_must_be_split) {
      PRINTMSG(stmt_start_line, 433, Internal, stmt_start_col);
   }
# endif

   imp_do_idx = IL_IDX(top_list_idx);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
   
   if (IR_OPR(ir_idx) == Alt_Return_Opr ||
       IR_OPR(ir_idx) == Asg_Opr)       {

      ir_idx = IR_IDX_R(ir_idx);
   }

   if (next_stmt_idx) {

      new_root = copy_text_for_expansion(FL_IO_MIDDLE);

      if (IR_OPR(new_root) == Alt_Return_Opr ||
          IR_OPR(new_root) == Asg_Opr)       {

         io_idx = IR_IDX_R(new_root);
      }
      else {
         io_idx = new_root;
      }

      COPY_OPND(IR_OPND_R(io_idx), IR_OPND_L(imp_do_idx));

      curr_stmt_sh_idx = next_stmt_idx;
      gen_sh(Before, stmt_type, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_root;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      if (alt_return_branch_idx) {
         gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         curr_stmt_sh_idx                = SH_PREV_IDX(curr_stmt_sh_idx);
         SH_IR_IDX(curr_stmt_sh_idx)     = alt_return_branch_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
      }

      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   }
   else {
      /* this is the first item */
      /* so no new stmt.        */

      COPY_OPND(IR_OPND_R(ir_idx), IR_OPND_L(imp_do_idx));
   }

   /* process right side first */

   list_idx = IR_IDX_R(imp_do_idx);
   lcv_attr = IL_IDX(list_idx);
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(start_opnd, IL_OPND(list_idx));

   if (IL_HAS_FUNCTIONS(list_idx)) {
      process_deferred_functions(&start_opnd);
      COPY_OPND(IL_OPND(list_idx), start_opnd);
   }

   gen_runtime_checks(&start_opnd);

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(end_opnd, IL_OPND(list_idx));

   if (IL_HAS_FUNCTIONS(list_idx)) {
      process_deferred_functions(&end_opnd);
      COPY_OPND(IL_OPND(list_idx), end_opnd);
   }

   gen_runtime_checks(&end_opnd);

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   COPY_OPND(inc_opnd, IL_OPND(list_idx));

   if (IL_HAS_FUNCTIONS(list_idx)) {
      process_deferred_functions(&inc_opnd);
      COPY_OPND(IL_OPND(list_idx), inc_opnd);
   }

   gen_runtime_checks(&inc_opnd);

   create_loop_stmts(lcv_attr, &start_opnd, &end_opnd, &inc_opnd,
                     curr_stmt_sh_idx, 
                     (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) == Alt_Return_Opr ?
                        SH_NEXT_IDX(curr_stmt_sh_idx) : curr_stmt_sh_idx));

   expand_io_list();

   TRACE (Func_Exit, "expand_imp_do", NULL);

   return;

}  /* expand_imp_do */

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

static int copy_text_for_expansion(int	flflag)

{
   int			new_alt_idx;
   int			new_io_idx;
   int			new_root;
   int			old_alt_idx;
   int			old_io_idx;
   opnd_type		opnd;


   TRACE (Func_Entry, "copy_text_for_expansion", NULL);

# ifdef _DEBUG
   if (! io_stmt_must_be_split) {
      PRINTMSG(stmt_start_line, 433, Internal, stmt_start_col);
   }
# endif

   if (alt_return_branch_idx      ||
       stmt_type == Inquire_Stmt) {

      NTR_IR_TBL(new_alt_idx);
      old_alt_idx = SH_IR_IDX(curr_stmt_sh_idx);
      COPY_TBL_NTRY(ir_tbl, new_alt_idx, old_alt_idx);
      NTR_IR_TBL(new_io_idx);
      old_io_idx = IR_IDX_R(old_alt_idx);
      COPY_TBL_NTRY(ir_tbl, new_io_idx, old_io_idx);
      IR_OPND_R(new_io_idx) = null_opnd;
      IR_IDX_R(new_alt_idx) = new_io_idx;
      new_root = new_alt_idx;
   }
   else {
      NTR_IR_TBL(new_io_idx);
      old_io_idx = SH_IR_IDX(curr_stmt_sh_idx);
      COPY_TBL_NTRY(ir_tbl, new_io_idx, old_io_idx);
      IR_OPND_R(new_io_idx) = null_opnd;
      new_root = new_io_idx;
   }

   copy_subtree(&IR_OPND_L(old_io_idx), &opnd);
   COPY_OPND(IR_OPND_L(new_io_idx), opnd);

   if (stmt_type == Inquire_Stmt) {
      IL_IDX(IR_IDX_L(new_io_idx)) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                 flflag);
   }
   else {
      IL_IDX(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_L(new_io_idx)))) =
                                        C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                    flflag);
   }

   TRACE (Func_Exit, "copy_text_for_expansion", NULL);

   return(new_root);

}  /* copy_text_for_expansion */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the argument descriptor for calls to open, close, inquire,     *|
|*      buffer in, and buffer out.                                            *|
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

static void create_io_call_descriptor(int			call_idx,
				      io_descriptor_type	call_type)

{
   int			asg_idx;
   int			bd_idx;
   int			col;
   boolean		gen_descriptor;
   int			item_cnt;
   int			line;
   int			list_idx;
   int			list2_idx;
   int			loc_idx;
   int			offset;
   int			shift_idx;
   int			subscript_idx;
   long64		the_constant;
   int			tmp_idx;
   int			type_idx;
   int			version_cn_idx;

# define IO_CALL_VERSION	0


   TRACE (Func_Entry, "create_io_call_descriptor", NULL);

   col = IR_COL_NUM(call_idx);
   line = IR_LINE_NUM(call_idx);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   type_idx = SA_INTEGER_DEFAULT_TYPE;
# else
   type_idx = CG_INTEGER_DEFAULT_TYPE;
# endif

   version_cn_idx = C_INT_TO_CN(type_idx, IO_CALL_VERSION);  /* BRIANJ */

# if defined(_FILE_IO_OPRS)
   if (call_type == Buffer_Desc) {
      gen_descriptor = TRUE;
   }
   else {
      gen_descriptor = FALSE;
   }
# else
   gen_descriptor = TRUE;
# endif

   if (! gen_descriptor) {
      /* place version constant as first list item */
      NTR_IR_LIST_TBL(list_idx);
      IL_NEXT_LIST_IDX(list_idx) = IR_IDX_R(call_idx);
      IR_IDX_R(call_idx) = list_idx;
      IR_LIST_CNT_R(call_idx) += 1;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = version_cn_idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

   }
   else {
      /* create integer array for descriptor */

      tmp_idx                      = gen_compiler_tmp(line, col, Priv, TRUE);
      AT_SEMANTICS_DONE(tmp_idx)   = TRUE;
      ATD_TYPE_IDX(tmp_idx)        = type_idx;
      ATD_STOR_BLK_IDX(tmp_idx)    = SCP_SB_STACK_IDX(curr_scp_idx);

      bd_idx                       = reserve_array_ntry(1);
      BD_RANK(bd_idx)              = 1;
      BD_ARRAY_CLASS(bd_idx)       = Explicit_Shape;
      BD_ARRAY_SIZE(bd_idx)        = Constant_Size;
      BD_LINE_NUM(bd_idx)          = line;
      BD_COLUMN_NUM(bd_idx)        = col;
      BD_RESOLVED(bd_idx)          = TRUE;

      if (two_word_fcd) {
         the_constant		= descriptor_size_tbl[call_type];
      }
      else {
         the_constant		= 1 + IR_LIST_CNT_R(call_idx);
      }
   
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      /* the version item is always 64 bits */
      if (TYP_LINEAR(type_idx) == Integer_4) {
         the_constant++;
      }
# endif

      BD_LEN_FLD(bd_idx)           = CN_Tbl_Idx;
      BD_LEN_IDX(bd_idx)           = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                 the_constant);

      BD_LB_FLD(bd_idx, 1)         = CN_Tbl_Idx;
      BD_LB_IDX(bd_idx, 1)         = CN_INTEGER_ONE_IDX;

      BD_UB_FLD(bd_idx, 1)         = CN_Tbl_Idx;
      BD_UB_IDX(bd_idx, 1)         = BD_LEN_IDX(bd_idx);
   
      BD_XT_FLD(bd_idx, 1)         = CN_Tbl_Idx;
      BD_XT_IDX(bd_idx, 1)         = BD_LEN_IDX(bd_idx);
   
      BD_SM_FLD(bd_idx, 1)         = CN_Tbl_Idx;
      BD_SM_IDX(bd_idx, 1)         = CN_INTEGER_ONE_IDX;
   
      ATD_ARRAY_IDX(tmp_idx)       = ntr_array_in_bd_tbl(bd_idx);
   
   
      /* fill in the descriptor fields */
   
      item_cnt = 0;
   
      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = type_idx;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      NTR_IR_TBL(subscript_idx);
      IR_OPR(subscript_idx) = Subscript_Opr;
      IR_TYPE_IDX(subscript_idx) = type_idx;
      IR_LINE_NUM(subscript_idx) = line;
      IR_COL_NUM(subscript_idx)  = col;
      IR_FLD_L(subscript_idx)    = AT_Tbl_Idx;
      IR_IDX_L(subscript_idx)    = tmp_idx;
      IR_LINE_NUM_L(subscript_idx) = line;
      IR_COL_NUM_L(subscript_idx)  = col;
   
      NTR_IR_LIST_TBL(list2_idx);
      IR_FLD_R(subscript_idx)    = IL_Tbl_Idx;
      IR_LIST_CNT_R(subscript_idx) = 1;
      IR_IDX_R(subscript_idx)      = list2_idx;
      IL_FLD(list2_idx)             = CN_Tbl_Idx;
      IL_IDX(list2_idx)             = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list2_idx)        = line;
      IL_COL_NUM(list2_idx)         = col;

      IR_FLD_L(asg_idx)            = IR_Tbl_Idx;
      IR_IDX_L(asg_idx)            = subscript_idx;

      IR_FLD_R(asg_idx)    = CN_Tbl_Idx;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (TYP_LINEAR(type_idx) == Integer_4) {
         IR_IDX_R(asg_idx)    = CN_INTEGER_ZERO_IDX;
      }
      else {
         IR_IDX_R(asg_idx)    = version_cn_idx;
      }
# else 
      IR_IDX_R(asg_idx)    = version_cn_idx;
# endif
      IR_LINE_NUM_R(asg_idx) = line;
      IR_COL_NUM_R(asg_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   
      item_cnt++;
      the_constant = 2;
   
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (TYP_LINEAR(type_idx) == Integer_4) {
         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = type_idx;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;
         NTR_IR_TBL(subscript_idx);
         IR_OPR(subscript_idx) = Subscript_Opr;
         IR_TYPE_IDX(subscript_idx) = type_idx;
         IR_LINE_NUM(subscript_idx) = line;
         IR_COL_NUM(subscript_idx)  = col;
         IR_FLD_L(subscript_idx)    = AT_Tbl_Idx;
         IR_IDX_L(subscript_idx)    = tmp_idx;
         IR_LINE_NUM_L(subscript_idx) = line;
         IR_COL_NUM_L(subscript_idx)  = col;
   
         NTR_IR_LIST_TBL(list2_idx);
         IR_FLD_R(subscript_idx)    = IL_Tbl_Idx;
         IR_LIST_CNT_R(subscript_idx) = 1;
         IR_IDX_R(subscript_idx)      = list2_idx;
         IL_FLD(list2_idx)             = CN_Tbl_Idx;
         IL_IDX(list2_idx)             = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                     the_constant);
         IL_LINE_NUM(list2_idx)        = line;
         IL_COL_NUM(list2_idx)         = col;

         IR_FLD_L(asg_idx)            = IR_Tbl_Idx;
         IR_IDX_L(asg_idx)            = subscript_idx;

         IR_FLD_R(asg_idx)    = CN_Tbl_Idx;
         IR_IDX_R(asg_idx)    = version_cn_idx;
         IR_LINE_NUM_R(asg_idx) = line;
         IR_COL_NUM_R(asg_idx)  = col;

         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   
         the_constant++;
      }
# endif

      list_idx = IR_IDX_R(call_idx);

      while (list_idx) {

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = type_idx;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx)  = col;
         NTR_IR_TBL(subscript_idx);
         IR_OPR(subscript_idx) = Subscript_Opr;
         IR_TYPE_IDX(subscript_idx) = type_idx;
         IR_LINE_NUM(subscript_idx) = line;
         IR_COL_NUM(subscript_idx)  = col;
         IR_FLD_L(subscript_idx)    = AT_Tbl_Idx;
         IR_IDX_L(subscript_idx)    = tmp_idx;
         IR_LINE_NUM_L(subscript_idx) = line;
         IR_COL_NUM_L(subscript_idx)  = col;
   
         NTR_IR_LIST_TBL(list2_idx);
         IR_FLD_R(subscript_idx)    = IL_Tbl_Idx;
         IR_LIST_CNT_R(subscript_idx) = 1;
         IR_IDX_R(subscript_idx)      = list2_idx;
         IL_FLD(list2_idx)             = CN_Tbl_Idx;
         IL_IDX(list2_idx)             = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                     the_constant);
         IL_LINE_NUM(list2_idx)        = line;
         IL_COL_NUM(list2_idx)         = col;
   
         IR_FLD_L(asg_idx)            = IR_Tbl_Idx;
         IR_IDX_L(asg_idx)            = subscript_idx;

         if (IL_FLD(list_idx) == IR_Tbl_Idx &&
             IR_OPR(IL_IDX(list_idx)) == Aloc_Opr) {
   
            IR_OPR(IL_IDX(list_idx)) = Loc_Opr;
         }
   
         COPY_OPND(IR_OPND_R(asg_idx), IL_OPND(list_idx));
      
         gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         offset = offset_tbl[call_type][item_cnt];

         if (two_word_fcd) {

            if (IL_FLD(list_idx) != IR_Tbl_Idx ||
                IR_OPR(IL_IDX(list_idx)) != Loc_Opr ||
                TYP_LINEAR(IR_TYPE_IDX(IL_IDX(list_idx))) != CRI_Ch_Ptr_8) {
               /* no character argument */
               the_constant += offset;
            }
            else {
               the_constant++;
             
               if (offset == 2) {
                  /* fill in the length */
                  NTR_IR_TBL(asg_idx);
                  IR_OPR(asg_idx) = Asg_Opr;
                  IR_TYPE_IDX(asg_idx) = type_idx;
                  IR_LINE_NUM(asg_idx) = line;
                  IR_COL_NUM(asg_idx)  = col;
                  NTR_IR_TBL(subscript_idx);
                  IR_OPR(subscript_idx) = Subscript_Opr;
                  IR_TYPE_IDX(subscript_idx) = type_idx;
                  IR_LINE_NUM(subscript_idx) = line;
                  IR_COL_NUM(subscript_idx)  = col;
                  IR_FLD_L(subscript_idx)    = AT_Tbl_Idx;
                  IR_IDX_L(subscript_idx)    = tmp_idx;
                  IR_LINE_NUM_L(subscript_idx) = line;
                  IR_COL_NUM_L(subscript_idx)  = col;
      
                  NTR_IR_LIST_TBL(list2_idx);
                  IR_FLD_R(subscript_idx)    = IL_Tbl_Idx;
                  IR_LIST_CNT_R(subscript_idx) = 1;
                  IR_IDX_R(subscript_idx)      = list2_idx;
                  IL_FLD(list2_idx) = CN_Tbl_Idx;
                  IL_IDX(list2_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                  the_constant);
                  IL_LINE_NUM(list2_idx) = line;
                  IL_COL_NUM(list2_idx)  = col;
      
                  IR_FLD_L(asg_idx)      = IR_Tbl_Idx;
                  IR_IDX_L(asg_idx)      = subscript_idx;
      
# ifdef _DEBUG
                  if (IL_FLD(list_idx) != IR_Tbl_Idx ||
                      IR_OPR(IL_IDX(list_idx)) != Loc_Opr ||
                      IR_FLD_L(IL_IDX(list_idx)) != IR_Tbl_Idx ||
                      (IR_OPR(IR_IDX_L(IL_IDX(list_idx))) != Substring_Opr &&
                       IR_OPR(IR_IDX_L(IL_IDX(list_idx))) != 
                                                     Whole_Substring_Opr)) {
   
                     PRINTMSG(line, 1022, Internal, col);
                  }
# endif
   
                  if (char_len_in_bytes) {
                     /* length is in bytes on solaris */
                     COPY_OPND(IR_OPND_R(asg_idx),
                            IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(
                                      IR_IDX_L(IL_IDX(list_idx)))))));
                  }
                  else {
                     /* length is in bits on mpp. shift left 3 */
                     NTR_IR_TBL(shift_idx);
                     IR_OPR(shift_idx) = Shiftl_Opr;
                     IR_TYPE_IDX(shift_idx) = type_idx;
                     IR_LINE_NUM(shift_idx) = line;
                     IR_COL_NUM(shift_idx) = col;
   
                     NTR_IR_LIST_TBL(list2_idx);
                     IR_FLD_L(shift_idx) = IL_Tbl_Idx;
                     IR_IDX_L(shift_idx) = list2_idx;
                     IR_LIST_CNT_L(shift_idx) = 2;
   
                     COPY_OPND(IL_OPND(list2_idx),
                            IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(
                                      IR_IDX_L(IL_IDX(list_idx)))))));
   
                     NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
                     list2_idx = IL_NEXT_LIST_IDX(list2_idx);
   
                     IL_LINE_NUM(list2_idx) = line;
                     IL_COL_NUM(list2_idx)  = col;
                     IL_FLD(list2_idx) = CN_Tbl_Idx;
                     IL_IDX(list2_idx) = CN_INTEGER_THREE_IDX;
                     IR_FLD_R(asg_idx) = IR_Tbl_Idx;
                     IR_IDX_R(asg_idx) = shift_idx;
                  }

                  gen_sh(Before,Assignment_Stmt,line,col,FALSE,FALSE,TRUE);
                  SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                  SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      
                  the_constant++;
               }
            }
         }
         else {
            the_constant++;
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
         item_cnt++;
      }

      /* replace the call list with the descriptor */


      IR_LIST_CNT_R(call_idx) = 1;
      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_R(call_idx) = list_idx;


      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx) = Aloc_Opr;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;

      IR_FLD_L(loc_idx) = AT_Tbl_Idx;
      IR_IDX_L(loc_idx) = tmp_idx;
      IR_LINE_NUM_L(loc_idx) = line;
      IR_COL_NUM_L(loc_idx)  = col;

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = loc_idx;
   
   }

   TRACE (Func_Exit, "create_io_call_descriptor", NULL);

   return;

}  /* create_io_call_descriptor */

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

void set_sb_first_attr_idx(int		attr_idx)

{
   int		tmp_idx;
   int		type_idx;

   TRACE (Func_Entry, "set_sb_first_attr_idx", NULL);

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
   TYP_LINEAR(TYP_WORK_IDX)	= Long_Typeless;
   TYP_BIT_LEN(TYP_WORK_IDX)	= CHAR_BIT;
   type_idx			= ntr_type_tbl();

   tmp_idx = gen_compiler_tmp(stmt_start_line, stmt_start_col, Priv, TRUE);

   ATD_TYPE_IDX(tmp_idx) = type_idx;
   AT_SEMANTICS_DONE(tmp_idx) = TRUE;
   ATD_OFFSET_ASSIGNED(tmp_idx) = TRUE;
   ATD_OFFSET_IDX(tmp_idx) = CN_INTEGER_ZERO_IDX;
   ATD_OFFSET_FLD(tmp_idx) = CN_Tbl_Idx;

   ATD_STOR_BLK_IDX(tmp_idx) = ATD_STOR_BLK_IDX(attr_idx);

   SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) = tmp_idx;

   TRACE (Func_Exit, "set_sb_first_attr_idx", NULL);

   return;

}  /* set_sb_first_attr_idx */

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

# ifdef _NO_IO_ALTERNATE_RETURN
static void add_alt_return_lbl(int	ir_idx,
			       int	lbl_attr_idx)

{

   int		list_idx;

   TRACE (Func_Entry, "add_alt_return_lbl", NULL);

   list_idx = IR_IDX_R(ir_idx);
   while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_ARG_DESC_VARIANT(IL_NEXT_LIST_IDX(list_idx)) = TRUE;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (lbl_attr_idx != NULL_IDX) {
      IL_FLD(list_idx) = AT_Tbl_Idx;
      IL_IDX(list_idx) = lbl_attr_idx;
   }
   IL_LINE_NUM(list_idx) = IR_LINE_NUM(ir_idx);
   IL_COL_NUM(list_idx)  = IR_COL_NUM(ir_idx);

   IR_LIST_CNT_R(ir_idx) += 1;

   TRACE (Func_Exit, "add_alt_return_lbl", NULL);

   return;

}  /* add_alt_return_lbl */
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

static boolean item_has_bounds_chk(opnd_type	*top_opnd)

{

   boolean		bounds_chk = FALSE;
   int			ir_idx;
   int			list_idx;
   opnd_type		opnd;

   TRACE (Func_Entry, "item_has_bounds_chk", NULL);

   switch (OPND_FLD((*top_opnd))) {
   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*top_opnd));

      if (IR_OPR(ir_idx) == Substring_Opr &&
          cmd_line_flags.runtime_substring &&
          ATD_CLASS(find_left_attr(&IR_OPND_L(ir_idx))) != Compiler_Tmp) {

         bounds_chk = TRUE;
      }
      else if ((IR_OPR(ir_idx) == Subscript_Opr ||
                IR_OPR(ir_idx) == Section_Subscript_Opr) &&
               needs_bounds_check(ir_idx)) {

         bounds_chk = TRUE;
      }
      else if (IR_OPR(ir_idx) == Dv_Deref_Opr &&
               cmd_line_flags.runtime_ptr_chk &&
               ATD_CLASS(find_left_attr(&IR_OPND_L(ir_idx))) != Compiler_Tmp) {

         bounds_chk = TRUE;
      }
      else {
         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         bounds_chk = item_has_bounds_chk(&opnd);

         if (! bounds_chk) {
            COPY_OPND(opnd, IR_OPND_R(ir_idx));
            bounds_chk = item_has_bounds_chk(&opnd);
         }
      }
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*top_opnd));

      while (list_idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));

         if (item_has_bounds_chk(&opnd)) {
            bounds_chk = TRUE;
            break;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;
   }

   TRACE (Func_Exit, "item_has_bounds_chk", NULL);

   return(bounds_chk);

}  /* item_has_bounds_chk */

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

static void gen_array_element_init(int		attr_idx,
                                   long_type	*idx_constant,
                                   opnd_type	*rhs_opnd,
                                   int		opr,
                                   int		offset)

{
#ifdef KEY /* Bug 10177 */
   int		asg_idx = 0;
#else /* KEY Bug 10177 */
   int		asg_idx;
#endif /* KEY Bug 10177 */
   opnd_type	opnd[2];
   int		col;
   int		i;
   int		line;
   int		list_idx;
   int		num_values = 1;
   int		sub_idx;
   int		type_idx;

   TRACE (Func_Entry, "gen_array_element_init", NULL);

   find_opnd_line_and_column(rhs_opnd, &line, &col);

   type_idx = ATD_TYPE_IDX(attr_idx);

   if (OPND_FLD((*rhs_opnd)) == CN_Tbl_Idx) {

      if (TYP_LINEAR(type_idx) == Integer_4 &&
          (sizeof(long_type) == 4 || Is_Target_32bit()) &&  /* OSP_456 */
          TYP_LINEAR(CN_TYPE_IDX(OPND_IDX((*rhs_opnd)))) == Integer_8) {
 
         // OSP_456
         // in 32-bit compiler, the_constant[0] = CP_CONSTANT[idx]
         //                     the_constant[1] = CP_CONSTANT[idx+1]
         // in 64-bit compiler, the_constant[0] = CP_CONSTANT[idx] & 0xffffffff
         //                     the_constant[1] = CP_CONSTANT[idx] >> 32
         // TODO: the byte order
         int* the_constant = (int *) &CP_CONSTANT(CN_POOL_IDX(
                                                 OPND_IDX((*rhs_opnd)))); 
         gen_opnd(&(opnd[0]), 
                  ntr_const_tbl(Integer_4,
                                FALSE,
                                the_constant),
                  CN_Tbl_Idx,
                  line,
                  col);
         gen_opnd(&(opnd[1]), 
                  ntr_const_tbl(Integer_4,
                                FALSE,
                                the_constant + 1),
                  CN_Tbl_Idx,
                  line,
                  col);
         num_values = 2;
      }
      else {
         opnd[0] = *rhs_opnd;
      }
   }
   else {
      opnd[0] = *rhs_opnd;
   }

# ifdef _DEBUG
# ifndef _INIT_RELOC_BASE_OFFSET
   if (offset) {
      PRINTMSG(line, 626, Internal, col,
               "offset == 0", "gen_array_element_init");
   }
# endif
# endif

   for (i = 0; i < num_values; i++) {
      NTR_IR_TBL(sub_idx);
      IR_OPR(sub_idx) = Subscript_Opr;
      IR_TYPE_IDX(sub_idx) = type_idx;
      IR_LINE_NUM(sub_idx) = line;
      IR_COL_NUM(sub_idx)  = col;

      IR_FLD_L(sub_idx) = AT_Tbl_Idx;
      IR_IDX_L(sub_idx) = attr_idx;
      IR_LINE_NUM_L(sub_idx) = line;
      IR_COL_NUM_L(sub_idx)  = col;

      list_idx = gen_il(1,
                        FALSE,
                        line,
                        col,
                        CN_Tbl_Idx,
                        ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                                      FALSE,
                                      idx_constant));

      IR_FLD_R(sub_idx) = IL_Tbl_Idx;
      IR_IDX_R(sub_idx) = list_idx;
      IR_LIST_CNT_R(sub_idx) = 1;


      if (opr == Asg_Opr) {
         asg_idx = gen_ir(IR_Tbl_Idx, sub_idx,
                      Asg_Opr, type_idx, line, col,
                          OPND_FLD(opnd[i]), OPND_IDX(opnd[i]));             
      }
      else if (opr == Init_Opr) {
         asg_idx = gen_ir(IR_Tbl_Idx, sub_idx,
                      Init_Opr, type_idx, line, col,
                          IL_Tbl_Idx, gen_il(3,
                                             FALSE,
                                             line,
                                             col,
                                             OPND_FLD(opnd[i]), 
                                             OPND_IDX(opnd[i]),
                                             CN_Tbl_Idx,
                                             CN_INTEGER_ONE_IDX,
                                             CN_Tbl_Idx,
                                             CN_INTEGER_ZERO_IDX));
      }
      else if (opr == Init_Reloc_Opr) {
         asg_idx = gen_ir(IR_Tbl_Idx, sub_idx,
                      Init_Reloc_Opr, type_idx, line, col,
                          IL_Tbl_Idx, gen_il(2,
                                             FALSE,
                                             line,
                                             col,
                                             OPND_FLD(opnd[i]), 
                                             OPND_IDX(opnd[i]),
                                             CN_Tbl_Idx,
                                             offset ? offset 
                                                    : CN_INTEGER_ZERO_IDX));
      }
      else {
         PRINTMSG(line, 626, Internal, col,
                  "Asg_Opr or Init_Opr", "gen_array_element_init");
      }

      gen_sh(After, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      *idx_constant += 1;
   }

   TRACE (Func_Exit, "gen_array_element_init", NULL);

   return;

}  /* gen_array_element_init */
