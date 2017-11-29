/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_typ_init.c	5.3	06/16/99 10:02:23\n";

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


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static	boolean	attr_init_semantics(opnd_type *, int, int, expr_arg_type *);
static	boolean	const_init_semantics(opnd_type *, int, int);
static	void	process_all_initialized_cpnts(opnd_type *, int, operator_type);


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do semantics for type declaration initializations.                    *|
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

void type_init_semantics (void)

{
   int			attr_idx;
   int			col;
   expr_arg_type	expr_desc;
   opnd_type		init_opnd;
   int			ir_idx;
   int			line;
   int			list_idx;
   int			opnd_column;
   int			opnd_line;
   int			sh_idx;
   int			type_idx;


   TRACE (Func_Entry, "type_init_semantics", NULL);

   /* set comp_gen_expr to TRUE. This forces the fold of REAL   */
   /* constant expressions. When -Oieeeconform is specified,    */
   /* the folding of Real and Complex expressions is prevented. */

   comp_gen_expr = TRUE;

   ir_idx	= SH_IR_IDX(curr_stmt_sh_idx);
   attr_idx	= IR_IDX_L(ir_idx);

   COPY_OPND(init_opnd, IR_OPND_R(ir_idx));

   line	= IR_LINE_NUM_L(ir_idx);
   col  = IR_COL_NUM_L(ir_idx);

   /* Constraint checks:                                                   */
   /* * A variable that is a member of blank common should not be          */
   /*   initialized.                                                       */
   /* * A variable that is a member of a named common block should only be */
   /*   initialized in a block data program unit.                          */
   /* * A variable that is a member of a task common block must not be     */
   /*   initialized.                                                       */
   /* * From a CF77 SPR:  If an object in a Block Data program unit is NOT */
   /*   in a common block (and is not equivalenced to an object in common) */
   /*   but IS initialized, issue a warning.                               */

   if (ATD_IN_COMMON(attr_idx)) {

      if (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Common) {

         if (SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx))) {
            PRINTMSG(line, 1109, Ansi, col);
         }

         else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Blockdata) {

# if defined(_ALLOW_DATA_INIT_OF_COMMON)
            PRINTMSG(line, 692, Ansi, col);
# else
            PRINTMSG(line, 1542, Warning, col);
# endif
         }
      }
      else if (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Task_Common) {
         PRINTMSG(line, 851, Error, col);
         goto EXIT;
      }
   }
   else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Blockdata  &&
            ! (ATD_EQUIV(attr_idx)  &&
               SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx)))) {
      PRINTMSG(line, 825, Warning, col);
   }

   /* There is no way to initialize a CRI character pointer.               */

   type_idx = ATD_TYPE_IDX(attr_idx);

   if (TYP_TYPE(type_idx) == CRI_Ch_Ptr) {
      PRINTMSG(line, 695, Error, col);
      goto EXIT;
   }

   if (AT_DCL_ERR(attr_idx)) {
      /* don't do anything else */
      goto EXIT;
   }


   OPND_FLD(init_target_opnd) = AT_Tbl_Idx;
   OPND_IDX(init_target_opnd) = attr_idx;
   OPND_LINE_NUM(init_target_opnd) = line;
   OPND_COL_NUM(init_target_opnd) = col;

   target_array_idx		= ATD_ARRAY_IDX(attr_idx);

   if (TYP_TYPE(type_idx) == Integer ||
       TYP_TYPE(type_idx) == Real    ||
       TYP_TYPE(type_idx) == Complex) {

      check_type_conversion = TRUE;
      target_type_idx       = type_idx;
   }
   else if (TYP_TYPE(type_idx) == Character) {

      if (TYP_CHAR_CLASS(type_idx) == Const_Len_Char) {

         check_type_conversion = TRUE;
         target_type_idx       = Character_1;
         target_char_len_idx   = TYP_IDX(type_idx);
      }
   }

   expr_mode  = Initialization_Expr;
   xref_state = CIF_Symbol_Reference;

   if (expr_semantics(&init_opnd, &expr_desc)) {

      if (ATD_POINTER(attr_idx) &&
          (OPND_FLD(init_opnd) == AT_Tbl_Idx || 
           OPND_FLD(init_opnd) == CN_Tbl_Idx ||
           (OPND_FLD(init_opnd) == IR_Tbl_Idx &&
            IR_OPR(OPND_IDX(init_opnd)) != Null_Intrinsic_Opr))) {
         PRINTMSG(line, 1559, Error, col,
                  AT_OBJ_NAME_PTR(attr_idx));
         goto EXIT;
      }

      if (! expr_desc.foldable) {

         /* The initialization expression must be a constant. */

         if (ATD_POINTER(attr_idx) &&
             OPND_FLD(init_opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(init_opnd)) == Null_Intrinsic_Opr) {
             goto EXIT;
         }
#ifdef KEY /* Bug 6845 */
	else if ((AT_OBJ_CLASS(TYP_IDX(ATD_TYPE_IDX(attr_idx)))
	     == Derived_Type) &&
	     ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {
	     find_opnd_line_and_column(&init_opnd, &opnd_line, &opnd_column);
	     PRINTMSG(opnd_line, 1680, Error, opnd_column,
	       AT_OBJ_NAME_PTR(attr_idx));
	     goto EXIT;
	}
#endif /* KEY Bug 6845 */
         else {
            find_opnd_line_and_column(&init_opnd, &opnd_line, &opnd_column);
            PRINTMSG(opnd_line, 842, Error, opnd_column);
            goto EXIT;
         }
      }

      while (OPND_FLD(init_opnd) == IR_Tbl_Idx) {
        COPY_OPND(init_opnd, IR_OPND_L(OPND_IDX(init_opnd)));
      }
   }
   else {
      goto EXIT;
   }

   if (OPND_FLD(init_opnd) == AT_Tbl_Idx) {

      if (attr_init_semantics(&init_opnd, attr_idx, ir_idx, &expr_desc)) {

         /* pull this init out of stmts. don't need it any more */

         sh_idx					= curr_stmt_sh_idx;
         SH_NEXT_IDX(SH_PREV_IDX(sh_idx))	= SH_NEXT_IDX(sh_idx);
         SH_PREV_IDX(SH_NEXT_IDX(sh_idx))	= SH_PREV_IDX(sh_idx);
         curr_stmt_sh_idx			= SH_PREV_IDX(sh_idx);
         FREE_IR_NODE(ir_idx);
         FREE_SH_NODE(sh_idx);
      }
   }
   else {

      if (const_init_semantics(&init_opnd, attr_idx, ir_idx)) {
         find_opnd_line_and_column(&init_opnd, &opnd_line, &opnd_column);
         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(ir_idx)	= IL_Tbl_Idx;
         IR_IDX_R(ir_idx)	= list_idx;
         IR_LIST_CNT_R(ir_idx)	= 3;
 
         COPY_OPND(IL_OPND(list_idx), init_opnd);
       
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx		= IL_NEXT_LIST_IDX(list_idx);

         IL_FLD(list_idx)	= CN_Tbl_Idx;
         IL_IDX(list_idx)	= CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(list_idx)	= opnd_line;
         IL_COL_NUM(list_idx)	= opnd_column;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx		= IL_NEXT_LIST_IDX(list_idx);

         IL_FLD(list_idx)	= CN_Tbl_Idx;
         IL_IDX(list_idx)	= CN_INTEGER_ZERO_IDX;
         IL_LINE_NUM(list_idx)	= opnd_line;
         IL_COL_NUM(list_idx)	= opnd_column;
      }
   }

EXIT:

   expr_mode			= Regular_Expr;
   check_type_conversion	= FALSE;
   target_array_idx		= NULL_IDX;
   init_target_opnd		= null_opnd;

   /* reset comp_gen_expr to FALSE. end of compiler generated expression */

   comp_gen_expr = FALSE;

   TRACE (Func_Exit, "type_init_semantics", NULL);

   return;

}  /* type_init_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do semantics for type declaration initializations.                    *|
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
void default_init_semantics(int	attr_idx)
{

   int			column;
   expr_arg_type	expr_desc;
   opnd_type		init_opnd;
   int			line;
   int			next_sh_idx;
   boolean		null_init;
   int			old_curr_stmt_sh_idx;
   opnd_type		opnd;
   int			sh_idx;
   int			type_idx;
   int			type_init_sh_idx;


   TRACE (Func_Entry, "default_init_semantics", NULL);

# ifdef _DEBUG
   if (ATD_CPNT_INIT_IDX(attr_idx) == NULL_IDX ||
       ATD_FLD(attr_idx) != IR_Tbl_Idx ||
       (IR_OPR(ATD_CPNT_INIT_IDX(attr_idx)) != Init_Opr &&
        IR_OPR(ATD_CPNT_INIT_IDX(attr_idx)) != Null_Opr)) {

      PRINTMSG(AT_DEF_LINE(attr_idx), 626, Internal,
               AT_DEF_COLUMN(attr_idx),
               "Init_Opr or Null_Opr", "default_init_semantics");
   }
# endif

   /* Generate a type init statement so that expression semantics gets */
   /* anything it generates in the correct order.  This statement will */
   /* be removed.                                                      */

   old_curr_stmt_sh_idx	= curr_stmt_sh_idx;

   gen_sh(After,
          Type_Init_Stmt,
          AT_DEF_LINE(attr_idx),
          AT_DEF_COLUMN(attr_idx),
          FALSE,
          FALSE,
          TRUE);

   type_init_sh_idx	= curr_stmt_sh_idx;
   target_array_idx	= ATD_ARRAY_IDX(attr_idx);
   type_idx		= ATD_TYPE_IDX(attr_idx);
   null_init		= FALSE;

   if (TYP_TYPE(type_idx) == Integer ||
       TYP_TYPE(type_idx) == Real    ||
       TYP_TYPE(type_idx) == Complex) {
      check_type_conversion = TRUE;
      target_type_idx       = type_idx;
   }
   else if (TYP_TYPE(type_idx) == Character) {

      if (TYP_CHAR_CLASS(type_idx) == Const_Len_Char) {
         check_type_conversion = TRUE;
         target_type_idx       = Character_1;
         target_char_len_idx   = TYP_IDX(type_idx);
      }
   }

   expr_mode		= Initialization_Expr;
   xref_state		= CIF_Symbol_Reference;
   expr_desc.rank	= 0;

   COPY_OPND(init_opnd, IR_OPND_R(ATD_CPNT_INIT_IDX(attr_idx)));

   if (expr_semantics(&init_opnd, &expr_desc)) {

      if (ATD_POINTER(attr_idx) &&
          (OPND_FLD(init_opnd) == AT_Tbl_Idx || 
           OPND_FLD(init_opnd) == CN_Tbl_Idx ||
           (OPND_FLD(init_opnd) == IR_Tbl_Idx &&
            IR_OPR(OPND_IDX(init_opnd)) != Null_Intrinsic_Opr))) {
         find_opnd_line_and_column(&init_opnd, &line, &column);
         PRINTMSG(line, 1559, Error, column, AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)		= TRUE;
         goto EXIT;
      }

      if (!expr_desc.foldable) {

         /* The initialization expression must be a constant. */

         if (ATD_POINTER(attr_idx) &&
             OPND_FLD(init_opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(init_opnd)) == Null_Intrinsic_Opr) {

            /* Pointer components are null'd by default, so we  */
            /* do not need to keep the null information around. */

            null_init	= TRUE;
            goto EXIT;
         }
#ifdef KEY /* Bug 6845 */
	 else if (AT_OBJ_CLASS(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ==
	    Derived_Type &&
	    ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {
	    find_opnd_line_and_column(&init_opnd, &line, &column);
	    PRINTMSG(line, 1680, Error, column, AT_OBJ_NAME_PTR(attr_idx));
	    AT_DCL_ERR(attr_idx)	= TRUE;
	    goto EXIT;
	 }
#endif /* KEY Bug 6845 */

         find_opnd_line_and_column(&init_opnd, &line, &column);
         PRINTMSG(line, 842, Error, column);
         AT_DCL_ERR(attr_idx)	= TRUE;
      }

      /* The assumption is that if this is IR, we will    */
      /* never end up with a CN_Tbl_Idx on the left side. */

      if (OPND_FLD(init_opnd) == CN_Tbl_Idx) {

         if (!const_init_semantics(&init_opnd,
                                    attr_idx,
                                    ATD_CPNT_INIT_IDX(attr_idx))) {
            AT_DCL_ERR(attr_idx) = TRUE;
         }
      }
      else {
         COPY_OPND(opnd, init_opnd);

         while (OPND_FLD(opnd) == IR_Tbl_Idx && OPND_IDX(opnd) != NULL_IDX) {
           COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         if (OPND_FLD(opnd) == AT_Tbl_Idx) {

            if (!attr_init_semantics(&opnd,
                                     attr_idx, 
                                     ATD_CPNT_INIT_IDX(attr_idx),
                                    &expr_desc)) {
               AT_DCL_ERR(attr_idx) = TRUE;
            }
         }
         else {
            PRINTMSG(AT_DEF_LINE(attr_idx), 626, Internal,
                     AT_DEF_COLUMN(attr_idx), 
                     "AT_Tbl_Idx",
                     "default_init_semantics");
         }
      }
   }
   else {  /* The initialization expression has an error */
      AT_DCL_ERR(attr_idx) = TRUE;
   }

EXIT:

   expr_mode			= Regular_Expr;
   check_type_conversion	= FALSE;
   target_array_idx		= NULL_IDX;
   sh_idx			= SH_NEXT_IDX(old_curr_stmt_sh_idx);

   if (old_curr_stmt_sh_idx != NULL_IDX) {
      SH_NEXT_IDX(old_curr_stmt_sh_idx) = SH_NEXT_IDX(type_init_sh_idx);
   }

   if (SH_NEXT_IDX(type_init_sh_idx) != NULL_IDX) {
      SH_PREV_IDX(SH_NEXT_IDX(type_init_sh_idx)) = old_curr_stmt_sh_idx;
   }

   curr_stmt_sh_idx = old_curr_stmt_sh_idx;

   while (sh_idx != type_init_sh_idx) {
      next_sh_idx	= SH_NEXT_IDX(sh_idx);
      FREE_SH_NODE(sh_idx);
      sh_idx		= next_sh_idx;
      
   }

   FREE_SH_NODE(type_init_sh_idx);

   if (AT_DCL_ERR(attr_idx) || null_init) {
      ATD_CPNT_INIT_IDX(attr_idx) = NULL_IDX;
      ATD_FLD(attr_idx)		  = NO_Tbl_Idx;
   }
   else {
      ATD_CPNT_INIT_IDX(attr_idx) = OPND_IDX(init_opnd);
      ATD_FLD(attr_idx)		  = OPND_FLD(init_opnd);
   }

   TRACE (Func_Exit, "default_init_semantics", NULL);

   return;

}  /* default_init_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do semantics for type declaration initializations.                    *|
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

static	boolean	attr_init_semantics(opnd_type		*init_opnd,
				    int			 attr_idx,
				    int			 ir_idx,
				    expr_arg_type	*expr_desc)

{
   int			c_type_idx;
   int			column;
   int			i;
   int			line;
   boolean		ok		= TRUE;
   int			opnd_column;
   int			opnd_line;
   char			type_str[40];


   TRACE (Func_Entry, "attr_init_semantics", NULL);

   line		= IR_LINE_NUM_L(ir_idx);
   column	= IR_COL_NUM_L(ir_idx);
   c_type_idx	= expr_desc->type_idx;

   find_opnd_line_and_column(init_opnd, &opnd_line, &opnd_column);

   if (TYP_LINEAR(c_type_idx) == Long_Typeless) {
      PRINTMSG(opnd_line, 1133, Error, opnd_column);
      ok = FALSE;
   }
   else if (!check_asg_semantics(ATD_TYPE_IDX(attr_idx),
                                 c_type_idx,
                                 opnd_line,
                                 opnd_column)) {
      type_str[0] = '\0';
      strcat(type_str, get_basic_type_str(ATD_TYPE_IDX(attr_idx)));

      PRINTMSG(line, 843, Error, column, AT_OBJ_NAME_PTR(attr_idx),
               type_str,
               get_basic_type_str(c_type_idx));
      ok = FALSE;
   }
   else if (expr_desc->rank > 0) {         /* check array conformance */

      if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
         PRINTMSG(line, 844, Error, column, AT_OBJ_NAME_PTR(attr_idx));
         ok = FALSE;
      }
      else if (expr_desc->rank == BD_RANK(ATD_ARRAY_IDX(attr_idx))) {

         for (i = 1; i <= expr_desc->rank; i++) {

            if (fold_relationals(expr_desc->shape[i-1].idx,
                                 BD_XT_IDX(ATD_ARRAY_IDX(attr_idx),i),
                                 Ne_Opr)) {

               PRINTMSG(line, 845, Error, column, AT_OBJ_NAME_PTR(attr_idx));
               ok = FALSE;
               break;
            }
         }
      }
      else {
         PRINTMSG(line, 845, Error, column, AT_OBJ_NAME_PTR(attr_idx));
         ok = FALSE;
      }
   }

   TRACE (Func_Exit, "attr_init_semantics", NULL);

   return(ok);

}  /* attr_init_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do semantics for type declaration initializations.                    *|
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

static boolean const_init_semantics(opnd_type		*init_opnd,
				    int			 attr_idx,
				    int			 ir_idx)

{
   int			a_type_idx;
   long_type	        another_constant[MAX_WORDS_FOR_NUMERIC];
   int			c_type_idx;
   char			*char_ptr;
   char			*c_char_ptr;
   int			column;
   int			const_idx;
   long64		i;
   int			line;
   boolean		ok		= TRUE;
   int			opnd_column;
   int			opnd_line;
   opnd_type		tar_opnd;
   char			type_str[40];


   TRACE (Func_Entry, "const_init_semantics", NULL);

   line		= IR_LINE_NUM_L(ir_idx);
   column	= IR_COL_NUM_L(ir_idx);
   a_type_idx	= ATD_TYPE_IDX(attr_idx);
   c_type_idx	= CN_TYPE_IDX(OPND_IDX((*init_opnd)));

   find_opnd_line_and_column(init_opnd, &opnd_line, &opnd_column);

   if (TYP_LINEAR(c_type_idx) == Long_Typeless) {
      PRINTMSG(opnd_line, 1133, Error, opnd_column);
      ok = FALSE;
      goto EXIT;
   }
   else if (!check_asg_semantics(a_type_idx,
                                 c_type_idx,
                                 opnd_line,
                                 opnd_column)) {
      type_str[0] = '\0';
      strcat(type_str, get_basic_type_str(a_type_idx));

      PRINTMSG(line, 843, Error, column, AT_OBJ_NAME_PTR(attr_idx),
               type_str,
               get_basic_type_str(c_type_idx));
      ok = FALSE;
      goto EXIT;
   }

   if (TYP_TYPE(a_type_idx) == Character) {

      if (fold_relationals(TYP_IDX(a_type_idx),
                           TYP_IDX(c_type_idx),
                           Ne_Opr)) {

         /* assumes that these are both CN_Tbl_Idx */

         /* create new constant for the right length and put the */
         /* original string in it. Truncate or blank pad to fit. */

         const_idx	= ntr_const_tbl(a_type_idx, TRUE, NULL);
         char_ptr	= (char *)&CN_CONST(const_idx);
         c_char_ptr	= (char *)&CN_CONST(OPND_IDX((*init_opnd)));

         for (i = 0; i < CN_INT_TO_C(TYP_IDX(a_type_idx)); i++) {
            char_ptr[i] = (i >= CN_INT_TO_C(TYP_IDX(c_type_idx))) ?
                          ' ' : c_char_ptr[i];
         }

         while (i % TARGET_CHARS_PER_WORD != 0) {
            char_ptr[i] = ' ';
            i++;
         }

         OPND_IDX((*init_opnd)) = const_idx;
      }

      /* If this is default initialization, the substring reference will */
      /* need to be generated when something is actually initialized.    */

      if (ATD_CLASS(attr_idx) != Struct_Component) {
         COPY_OPND(tar_opnd, IR_OPND_L(ir_idx));

         if (gen_whole_substring(&tar_opnd, 0)) {
            COPY_OPND(IR_OPND_L(ir_idx), tar_opnd);
         }
      }
   }
   else if (TYP_TYPE(c_type_idx) == Character ||
            TYP_TYPE(c_type_idx) == Typeless) {

      /* cast the character or typeless constant to the target type */

      OPND_IDX((*init_opnd)) = cast_typeless_constant(OPND_IDX((*init_opnd)),
                                                      a_type_idx,
                                                      opnd_line,
                                                      opnd_column);
   }
   else if (TYP_TYPE(c_type_idx) != Character  &&
            TYP_TYPE(c_type_idx) != Typeless   &&
            TYP_LINEAR(c_type_idx) != TYP_LINEAR(a_type_idx)) {

      /* PDGCS does not like it if the value is not the same size as the   */
      /* target; for example, the value is a double precision constant and */
      /* the target is a single precision variable.  So explicitly convert */
      /* the value to the type and kind type parameter of the target for   */
      /* all combinations to be consistent.                                */

      if (folder_driver( (char *) &CN_CONST(OPND_IDX((*init_opnd))),
                         c_type_idx,
                         NULL,
                         NULL_IDX,
                         another_constant,
                        &a_type_idx,
                         opnd_line,
                         opnd_column,
                         1,
                         Cvrt_Opr)) {

         OPND_IDX((*init_opnd)) = ntr_const_tbl(ATD_TYPE_IDX(attr_idx),
                                                FALSE,
                                                another_constant);
      }
   }

EXIT:

   TRACE (Func_Exit, "const_init_semantics", NULL);

   return(ok);

}  /* const_init_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine creates a chain of stmts to handle default initialization*|
|*      of a component or components.                                         *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx - idx of variable to process.                                *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void gen_default_init_code(int	 attr_idx)

{
   expr_arg_type	expr_desc;
   operator_type	operator;
   opnd_type    	opnd;


   TRACE (Func_Entry, "gen_default_init_code", NULL);

   if (AT_DCL_ERR(attr_idx)) {
      goto EXIT;
   }

   if (SB_RUNTIME_INIT(ATD_STOR_BLK_IDX(attr_idx))) {

      /* The var is on the stack, or is automatic, a darg or a func  */
      /* result.  Generate runtime code for the initialization.      */

      operator = Asg_Opr;
   }
   else if (ATD_IN_COMMON(attr_idx)) {
      operator = Init_Opr;

   }
   else {
      operator = Init_Opr;
   }

   if (!ATD_IM_A_DOPE(attr_idx) &&
       TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure           &&
       ATT_DEFAULT_INITIALIZED(TYP_IDX(ATD_TYPE_IDX(attr_idx))) &&
       !AT_DCL_ERR(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {

      OPND_FLD(opnd)      = AT_Tbl_Idx;
      OPND_IDX(opnd)      = attr_idx;
      OPND_LINE_NUM(opnd) = AT_DEF_LINE(attr_idx);
      OPND_COL_NUM(opnd)  = AT_DEF_COLUMN(attr_idx);

# if defined(_F_MINUS_MINUS)
      if (ATD_ARRAY_IDX(attr_idx) || ATD_PE_ARRAY_IDX(attr_idx)) {
# else
      if (ATD_ARRAY_IDX(attr_idx)) {
# endif
         gen_whole_subscript(&opnd, &expr_desc);
      }

      process_all_initialized_cpnts(&opnd, 
                                    TYP_IDX(ATD_TYPE_IDX(attr_idx)),
                                    operator);
   }

EXIT:

   TRACE (Func_Exit, "gen_default_init_code", NULL);

   return;

}  /* gen_default_init_code */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	recursively go through all components of a structure to look for      *|
|*      default initialization. Then call the supplied routine func for       *|
|*	processing.							      *|
|*									      *|
|* Input parameters:							      *|
|*	left_opnd - current base of sub-object reference.                     *|
|*      type_idx  - defined type attr.                                        *|
|*      operator  - Whether to use Init_Opr or Asg_Opr.                       *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_all_initialized_cpnts(opnd_type    *left_opnd,
					  int		type_idx,
					  operator_type	operator)

{
   int			attr_idx;
   expr_arg_type	expr_desc;
   opnd_type		expr_opnd;
   int			init_idx;
   int			ir_idx;
   int			list_idx;
   opnd_type		opnd;
   int			sn_idx;


   TRACE (Func_Entry, "process_all_initialized_cpnts", NULL);

   sn_idx = ATT_FIRST_CPNT_IDX(type_idx);

   while (sn_idx != NULL_IDX) {
      attr_idx = SN_ATTR_IDX(sn_idx);  /* A component */

      if (ATD_CPNT_INIT_IDX(attr_idx) != NULL_IDX) {
         NTR_IR_TBL(ir_idx);

         IR_OPR(ir_idx)		= Struct_Opr;
         IR_TYPE_IDX(ir_idx)	= ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(ir_idx)	= AT_DEF_LINE(attr_idx);
         IR_COL_NUM(ir_idx)	= AT_DEF_COLUMN(attr_idx);

         COPY_OPND(IR_OPND_L(ir_idx), (*left_opnd));

         IR_FLD_R(ir_idx)	= AT_Tbl_Idx;
         IR_IDX_R(ir_idx)	= attr_idx;
         IR_LINE_NUM_R(ir_idx)	= AT_DEF_LINE(attr_idx);
         IR_COL_NUM_R(ir_idx)	= AT_DEF_COLUMN(attr_idx);

         if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
             IR_RANK(ir_idx)	= IR_RANK(IR_IDX_L(ir_idx));
         }

         NTR_IR_TBL(init_idx);

         IR_OPR(init_idx)	= operator;
         IR_LINE_NUM(init_idx)	= AT_DEF_LINE(attr_idx);
         IR_COL_NUM(init_idx)	= AT_DEF_COLUMN(attr_idx);
         IR_TYPE_IDX(init_idx)	= TYPELESS_DEFAULT_TYPE;
         IR_FLD_L(init_idx)	= IR_Tbl_Idx;
         IR_IDX_L(init_idx)	= ir_idx;
         IR_LINE_NUM_L(init_idx)= AT_DEF_LINE(attr_idx);
         IR_COL_NUM_L(init_idx)	= AT_DEF_COLUMN(attr_idx);

         if (operator == Asg_Opr) {

            if (ATD_FLD(attr_idx) == IR_Tbl_Idx) {

               /* This should be an Init_Opr */

               if (IR_OPR(ATD_CPNT_INIT_IDX(attr_idx)) != Init_Opr) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 626, Internal,
                           AT_DEF_COLUMN(attr_idx),
                           "An Init Opr",
                           "process_all_initialized_cpnts");
               }

               COPY_OPND(IR_OPND_R(init_idx),
                         IL_OPND(IR_IDX_R(ATD_CPNT_INIT_IDX(attr_idx))));
            }
            else {
               IR_IDX_R(init_idx)	= ATD_CPNT_INIT_IDX(attr_idx);
               IR_FLD_R(init_idx)	= (fld_type) ATD_FLD(attr_idx);
               IR_LINE_NUM_R(init_idx)	= AT_DEF_LINE(attr_idx);
               IR_COL_NUM_R(init_idx)	= AT_DEF_COLUMN(attr_idx);
            }

            if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX ||
                TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {   
               xref_state		= CIF_No_Usage_Rec;
               expr_desc.rank		= 0;
               OPND_FLD(expr_opnd)	= IR_Tbl_Idx;
               OPND_IDX(expr_opnd)	= ir_idx;;

               if (expr_semantics(&expr_opnd, &expr_desc)) {
                  COPY_OPND(IR_OPND_L(init_idx), expr_opnd);
               }
            }

            gen_sh(After,
                   Assignment_Stmt,
                   AT_DEF_LINE(attr_idx),
                   AT_DEF_COLUMN(attr_idx),
                   FALSE,
                   FALSE,
                   TRUE);
         }
         else {  /* Init_Opr */

            if (ATD_FLD(attr_idx) == IR_Tbl_Idx) {

               /* This should be an Init_Opr */

               if (IR_OPR(ATD_CPNT_INIT_IDX(attr_idx)) != Init_Opr) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 626, Internal,
                           AT_DEF_COLUMN(attr_idx),
                           "An Init Opr",
                           "process_all_initialized_cpnts");
               }

               IR_FLD_R(init_idx)	= IL_Tbl_Idx;
               IR_IDX_R(init_idx)	= IR_IDX_R(ATD_CPNT_INIT_IDX(attr_idx));
               IR_LIST_CNT_R(init_idx)	= 3;
            }
            else {
               NTR_IR_LIST_TBL(list_idx);
               IR_FLD_R(init_idx)	= IL_Tbl_Idx;
               IR_IDX_R(init_idx)	= list_idx;
               IR_LIST_CNT_R(init_idx)	= 3;
               IL_IDX(list_idx)		= ATD_CPNT_INIT_IDX(attr_idx);
               IL_FLD(list_idx)		= (fld_type) ATD_FLD(attr_idx);
               IL_LINE_NUM(list_idx)	= AT_DEF_LINE(attr_idx);
               IL_COL_NUM(list_idx)	= AT_DEF_COLUMN(attr_idx);
       
               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));

               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;

               list_idx			= IL_NEXT_LIST_IDX(list_idx);
               IL_FLD(list_idx)		= CN_Tbl_Idx;
               IL_IDX(list_idx)		= CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(list_idx)	= AT_DEF_LINE(attr_idx);
               IL_COL_NUM(list_idx)	= AT_DEF_COLUMN(attr_idx);

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));

               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;

               list_idx			= IL_NEXT_LIST_IDX(list_idx);
               IL_FLD(list_idx)		= CN_Tbl_Idx;
               IL_IDX(list_idx)		= CN_INTEGER_ZERO_IDX;
               IL_LINE_NUM(list_idx)	= AT_DEF_LINE(attr_idx);
               IL_COL_NUM(list_idx)	= AT_DEF_COLUMN(attr_idx);
            }

            gen_sh(After,
                   Type_Init_Stmt,
                   AT_DEF_LINE(attr_idx),
                   AT_DEF_COLUMN(attr_idx),
                   FALSE,
                   FALSE,
                   TRUE);
         }

         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         SH_IR_IDX(curr_stmt_sh_idx)	 = init_idx;
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure           &&
               ATT_DEFAULT_INITIALIZED(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)		= Struct_Opr;
         IR_TYPE_IDX(ir_idx)	= ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(ir_idx)	= AT_DEF_LINE(attr_idx);
         IR_COL_NUM(ir_idx)	= AT_DEF_COLUMN(attr_idx);

         COPY_OPND(IR_OPND_L(ir_idx), (*left_opnd));

         IR_FLD_R(ir_idx)	= AT_Tbl_Idx;
         IR_IDX_R(ir_idx)	= attr_idx;
         IR_LINE_NUM_R(ir_idx)	= AT_DEF_LINE(attr_idx);
         IR_COL_NUM_R(ir_idx)	= AT_DEF_COLUMN(attr_idx);
         OPND_FLD(opnd)		= IR_Tbl_Idx;
         OPND_IDX(opnd)		= ir_idx;

         if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
             IR_RANK(ir_idx)	= IR_RANK(IR_IDX_L(ir_idx));
         }

         if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
            gen_whole_subscript(&opnd, &expr_desc);
         }

         process_all_initialized_cpnts(&opnd, 
                                       TYP_IDX(ATD_TYPE_IDX(attr_idx)),
                                       operator);

      }

      sn_idx = SN_SIBLING_LINK(sn_idx);
   }

   TRACE (Func_Exit, "process_all_initialized_cpnts", NULL);

   return;

}  /* process_all_initialized_cpnts */
