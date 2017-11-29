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



static char USMID[] = "\n@(#)5.0_pl/sources/p_dcls.c	5.10	10/08/99 08:26:21\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "p_dcls.h"

#ifdef KEY
#include "i_cvrt.h"
#endif

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static	void	issue_attr_blk_err(char *);
static	void	issue_attr_err(attr_type, long);
static	void	merge_parameter(boolean, int, int, int, opnd_type *,
				expr_arg_type *, int, int);
static	void	merge_type(int, int, int, int);
static	void	parse_cpnt_dcl_stmt(void);
static	long	parse_attr_spec(int *, boolean *);
static	boolean	parse_data_imp_do(opnd_type *);
static	void	parse_derived_type_stmt(void);
static	boolean	parse_initializer(int);
static	void	parse_only_spec(int);
static	void	retype_attr(int);


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	COMMON [/[common-block-name]/] common-block-object-list [[,]          *|
|*	       /[common-block-name]/ common-block-object-list]...             *|
|*	common-block-object IS variable-name [(explicit-shape-spec-list)]     *|
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

void parse_common_stmt (void)

{
   int		array_idx;
   int		attr_idx;
   boolean	blank_common	= FALSE;
   boolean	blk_err		= FALSE;
   int		column;
   int		line;
   int		name_idx;
   int		new_sb_idx;
#ifdef KEY /* Bug 10177 */
   int		last_attr_idx = 0;
#else /* KEY Bug 10177 */
   int		last_attr_idx;
#endif /* KEY Bug 10177 */
   boolean	parse_err	= FALSE;
   token_type	save_token;
   int		sb_idx		= NULL_IDX;


   TRACE (Func_Entry, "parse_common_stmt", NULL);

   if (stmt_type == Task_Common_Stmt) {

      if (!matched_specific_token(Tok_Kwd_Common, Tok_Class_Keyword)) {
         parse_err_flush(Find_Comma_Slash, "COMMON");
         blk_err = TRUE;
      }

# if !defined(_TASK_COMMON_EXTENSION)
      PRINTMSG(stmt_start_line, 1118, Error, stmt_start_col);
# else

      /* ANSI message, Task common statements are extensions */

      PRINTMSG(stmt_start_line, 46, Ansi, stmt_start_col);
# endif
   }

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, stmt_type) ||
        STMT_CANT_BE_IN_BLK(stmt_type, CURR_BLK)) && iss_blk_stk_err()) {
      blk_err = TRUE;		/* Block error issued */
   }
   else {
      curr_stmt_category = Declaration_Stmt_Cat;
   }

   do {
      if (sb_idx == NULL_IDX || LA_CH_VALUE == SLASH) {
         parse_err	= blk_err;	/* New common block list */
         blank_common	= FALSE;
         last_attr_idx	= NULL_IDX;

         if (LA_CH_VALUE != SLASH) {
            CREATE_ID(TOKEN_ID(token), 
                      BLANK_COMMON_NAME,
                      BLANK_COMMON_NAME_LEN);
            TOKEN_LEN(token)		= BLANK_COMMON_NAME_LEN;
            TOKEN_VALUE(token)		= Tok_Id;
            TOKEN_LINE(token)		= LA_CH_LINE; 
            TOKEN_COLUMN(token)		= LA_CH_COLUMN;
            blank_common		= TRUE;

            if (stmt_type == Task_Common_Stmt) {       /* Task can't be blank */
               PRINTMSG(LA_CH_LINE, 109, Error, LA_CH_COLUMN);
            }
         }
         else {
            NEXT_LA_CH;

            if (LA_CH_VALUE == SLASH) {
               CREATE_ID(TOKEN_ID(token), 
                         BLANK_COMMON_NAME,
                         BLANK_COMMON_NAME_LEN);
               TOKEN_LEN(token)		= BLANK_COMMON_NAME_LEN;
               TOKEN_VALUE(token)	= Tok_Id;
               TOKEN_LINE(token)	= LA_CH_LINE; 
               TOKEN_COLUMN(token)	= LA_CH_COLUMN;
               blank_common		= TRUE;

               if (stmt_type == Task_Common_Stmt) {   /* Task can't be blank */
                  PRINTMSG(LA_CH_LINE, 109, Error, LA_CH_COLUMN);
               }
               NEXT_LA_CH;
            }
            else if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

               if (LA_CH_VALUE == SLASH) {
                  NEXT_LA_CH;
               }
               else {
                  parse_err	= TRUE;
                  save_token	= token;  /* parse_err_flush destroys token */

                  if (parse_err_flush(Find_Comma_Slash, "/") &&
                      LA_CH_VALUE == SLASH) {
                      NEXT_LA_CH;
                  }
                  token		= save_token;  /* Restore common block name */
               }
            }
            else {
               line	= LA_CH_LINE; 
               column	= LA_CH_COLUMN;

               if (parse_err_flush(Find_Comma_Slash, "common-block-name or /")&&
                   LA_CH_VALUE == SLASH) {
                  NEXT_LA_CH;
               }

               CREATE_ID(TOKEN_ID(token), "//", 2);
               TOKEN_LEN(token)		= 2;
               TOKEN_VALUE(token)	= Tok_Id;
               TOKEN_LINE(token)	= line;
               TOKEN_COLUMN(token)	= column;
               parse_err		= TRUE;

            }
         }

         sb_idx = srch_stor_blk_tbl(TOKEN_STR(token),
                                    TOKEN_LEN(token),
                                    curr_scp_idx);

         if (sb_idx == NULL_IDX) {
            sb_idx			= ntr_stor_blk_tbl(TOKEN_STR(token),
							   TOKEN_LEN(token),
                                                           TOKEN_LINE(token),
                                                           TOKEN_COLUMN(token),
                                                           Common);
            SB_BLANK_COMMON(sb_idx)		= blank_common;
            SB_COMMON_NEEDS_OFFSET(sb_idx)	= TRUE;
         }
         else if (SB_USE_ASSOCIATED(sb_idx) || SB_HOST_ASSOCIATED(sb_idx)) {

            /* Common block has been use or host associated into this scope. */
            /* Make an entry for this block and hide the associated block    */
            /* storage_blk_resolution will resolve the blocks.               */

            new_sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
	   	                          TOKEN_LEN(token),
                                          TOKEN_LINE(token),
                                          TOKEN_COLUMN(token),
                                          Common);
            SB_BLANK_COMMON(new_sb_idx)		= blank_common;
            SB_COMMON_NEEDS_OFFSET(new_sb_idx)	= TRUE;
            SB_HIDDEN(sb_idx)			= TRUE;
            SB_DEF_MULT_SCPS(sb_idx)		= TRUE;
            SB_MERGED_BLK_IDX(sb_idx)		= new_sb_idx;
            sb_idx				= new_sb_idx;
         }
         else if (SB_FIRST_ATTR_IDX(sb_idx) != NULL_IDX) {
            last_attr_idx	= SB_FIRST_ATTR_IDX(sb_idx);

            while (ATD_NEXT_MEMBER_IDX(last_attr_idx) != NULL_IDX) {
               last_attr_idx	= ATD_NEXT_MEMBER_IDX(last_attr_idx);
            }
         }

         if ((cif_flags & XREF_RECS) != 0) {
            cif_sb_usage_rec(sb_idx,
                             TOKEN_LINE(token),
                             TOKEN_COLUMN(token),
                             CIF_Symbol_Declaration);
         }

         if (stmt_type == Task_Common_Stmt) {

            /* A common block may be specified, multiple times.  If any       */
            /* specifications are task common, then they all are.             */

            SB_BLK_TYPE(sb_idx)		= Task_Common;
            SB_RUNTIME_INIT(sb_idx)	= FALSE;
            SB_IS_COMMON(sb_idx)	= TRUE;
         }

         if (parse_err) {
            SB_DCL_ERR(sb_idx)	= TRUE;
         }

         if (LA_CH_CLASS == Ch_Class_Letter) {
            continue;		/* Get first object in common list */
         }
         else {
            /* There must be a common object name following.  If LA_CH is EOS */
            /* this will just fall out of the while.  If it's slash, it will  */
            /* pick up another common block name.  And a comma is usually     */
            /* expected.                                                      */

            if (!parse_err) {
               parse_err_flush(Find_Comma_Slash, "common-block-object");
               parse_err	= TRUE;
            }
            SB_DCL_ERR(sb_idx)	= TRUE;
         }
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         line		= TOKEN_LINE(token);
         column		= TOKEN_COLUMN(token);
         attr_idx	= srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                       &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_DCL_ERR(attr_idx)	= parse_err;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj; 
            ATD_CLASS(attr_idx)		= Variable;
            ATD_IN_COMMON(attr_idx)	= TRUE;
            ATD_STOR_BLK_IDX(attr_idx)	= sb_idx;
            SET_IMPL_TYPE(attr_idx);
         }
#ifdef KEY /* Bug 14150 */
         else {
	  int save_stor_blk_idx = 0;
	  /* We need to set this so fnd_semantic_err() can report the
	   * special-case error of "equivalence" involving a common block
	   * object when the common block (rather than the object) has the
	   * "bind" attribute. Set it only if safe, and revert in case of
	   * error. */
	  if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
	   save_stor_blk_idx = ATD_STOR_BLK_IDX(attr_idx);
	   ATD_STOR_BLK_IDX(attr_idx) = sb_idx;
	  }
	  if (!fnd_semantic_err(Obj_Common_Obj,line,column,attr_idx,TRUE))
#else /* KEY Bug 14150 */
         else if (!fnd_semantic_err(Obj_Common_Obj,line,column,attr_idx,TRUE))
#endif /* KEY Bug 14150 */
	  {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            if (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
               AT_ATTR_LINK(attr_idx)		= NULL_IDX;
               AT_HOST_ASSOCIATED(attr_idx)	= FALSE;
               LN_DEF_LOC(name_idx)		= TRUE;
               SET_IMPL_TYPE(attr_idx);
            }

            ATD_IN_COMMON(attr_idx)	= TRUE;
            ATD_STOR_BLK_IDX(attr_idx)	= sb_idx;
            ATD_CLASS(attr_idx)		= Variable;
            AT_DCL_ERR(attr_idx)	= parse_err || AT_DCL_ERR(attr_idx);

            if (ATD_AUXILIARY(attr_idx)) {
               SB_AUXILIARY(sb_idx)	= TRUE;
            }
         }
#ifdef KEY /* Bug 14150 */
	  /* fnd_semantic_err() found error, so revert */
	  else if (save_stor_blk_idx) {
	    ATD_STOR_BLK_IDX(attr_idx) = save_stor_blk_idx;
	  }
	 }
#endif /* KEY Bug 14150 */

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
         }

         if ((cif_flags & XREF_RECS) != 0) {
            cif_usage_rec(attr_idx, 
                          AT_Tbl_Idx,
                          line,
                          column,
                          CIF_Symbol_Declaration);
         }

         if (!AT_DCL_ERR(attr_idx)) {

            if (last_attr_idx == NULL_IDX) {
               SB_FIRST_ATTR_IDX(sb_idx)		= attr_idx;
            }
            else {
               ATD_NEXT_MEMBER_IDX(last_attr_idx)	= attr_idx;
            }

            last_attr_idx				= attr_idx;
         }
         else {
            SB_DCL_ERR(sb_idx)		= TRUE;
         }

         if (LA_CH_VALUE == LPAREN) {  /* Array specifier follows */
            array_idx = parse_array_spec(attr_idx);

            if (BD_ARRAY_CLASS(array_idx) == Deferred_Shape) {

               /* Arrays specified on a COMMON list must be    */
               /* explicit-shape-specs.  Common can have a     */
               /* deferred-shape-spec specified, but it has to */
               /* be on a seperate DIMENSION statement.        */

               PRINTMSG(BD_LINE_NUM(array_idx), 372, Error, 
                        BD_COLUMN_NUM(array_idx));
               AT_DCL_ERR(attr_idx)	= TRUE;
            }
            merge_dimension(attr_idx, line, column, array_idx);
         }

# ifdef _F_MINUS_MINUS
         if (LA_CH_VALUE == LBRKT &&
             cmd_line_flags.co_array_fortran) {
            ATD_PE_ARRAY_IDX(attr_idx) = parse_pe_array_spec(attr_idx);
         }
# endif
      }
      else { /* Problem with common block name.  Default to blank common name */
         line	= LA_CH_LINE;
         column	= LA_CH_COLUMN;

         parse_err_flush(Find_Comma_Slash, "common-block-object or /");

         if (sb_idx == NULL_IDX) {
            CREATE_ID(TOKEN_ID(token), "//", 2);
            TOKEN_LEN(token)		= 2;
            TOKEN_VALUE(token)		= Tok_Id;
            TOKEN_LINE(token)		= line; 
            TOKEN_COLUMN(token)		= column;
            parse_err			= TRUE;

            sb_idx = srch_stor_blk_tbl(TOKEN_STR(token),
                                       TOKEN_LEN(token),
                                       curr_scp_idx);

            if (sb_idx == NULL_IDX) {
               sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                         TOKEN_LEN(token),
                                         TOKEN_LINE(token),
                                         TOKEN_COLUMN(token),
                                         Common);
               SB_COMMON_NEEDS_OFFSET(sb_idx)	= TRUE;
            }
            else if (SB_USE_ASSOCIATED(sb_idx) || SB_HOST_ASSOCIATED(sb_idx)) {

               /* Common block has been use or host associated into this scp. */
               /* Make an entry for this block and hide the associated block. */
               /* storage_blk_resolution will resolve the blocks.             */

               new_sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                             TOKEN_LEN(token),
                                             TOKEN_LINE(token),
                                             TOKEN_COLUMN(token),
                                             Common);
               SB_COMMON_NEEDS_OFFSET(new_sb_idx)	= TRUE;
               SB_HIDDEN(sb_idx)			= TRUE;
               SB_DEF_MULT_SCPS(sb_idx)			= TRUE;
               sb_idx					= new_sb_idx;
            }
         }
         SB_DCL_ERR(sb_idx)		= TRUE;
      }

      if (LA_CH_VALUE != COMMA && LA_CH_VALUE != SLASH && LA_CH_VALUE != EOS) {
         parse_err_flush(Find_Comma_Slash, "/ or, or " EOS_STR);
         parse_err	= TRUE;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;

         if (LA_CH_VALUE == EOS) {	/* ,, case */
            parse_err_flush(Find_None, "common-block-object or /");
         }
      }
   }
   while (LA_CH_VALUE != EOS);

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_common_stmt", NULL);

   return;

}  /* parse_common_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BNF is CONTAINS							      *|
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

void parse_contains_stmt (void)

{
   boolean	have_blk_err	= FALSE;


   TRACE (Func_Entry, "parse_contains_stmt", NULL);

#ifdef KEY /* Bug 14110 */
   revisit_volatile();
#endif /* KEY Bug 14110 */

   do_cmic_blk_checks();

   if (LA_CH_VALUE == EOS) {

      if (STMT_CANT_BE_IN_BLK(Contains_Stmt, CURR_BLK) && iss_blk_stk_err()) {
         have_blk_err	= TRUE;
      }
      else {
         curr_stmt_category	= Sub_Func_Stmt_Cat;
      }

      if (CURR_BLK != Interface_Blk) {
 
         /* If this were an Interface_Blk, this is an error situation.  */
         /* We don't want to push the contains blk, because it creates  */
         /* havoc with interface block compressing.                     */

         PUSH_BLK_STK(Contains_Blk);
         CURR_BLK_NO_EXEC = TRUE;
         CURR_BLK_ERR     = have_blk_err;

         if (cif_flags) {
            cif_module_proc_start_line    = LA_CH_LINE;
            cif_internal_proc_start_line  = LA_CH_LINE;
            BLK_CIF_SCOPE_ID(blk_stk_idx) = BLK_CIF_SCOPE_ID(blk_stk_idx - 1);
         }
      }
      else {
         CURR_BLK_ERR     = TRUE;
      }
   }
   else {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   NEXT_LA_CH;			/* Skip EOS */

   TRACE (Func_Exit, "parse_contains_stmt", NULL);

   return;

}  /* parse_contains_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*   BNF   component-def-stmt                                                 *|
|*           type-spec [[,component-attr-spec-list]::] component-decl-list    *|
|*   Notes     - This routine is only entered if in a derived type statement. *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE                                                                  *|
|*									      *|
\******************************************************************************/

static void parse_cpnt_dcl_stmt()

{
   int		alignment;
#ifdef KEY /* Bug 10177 */
   int		array_column = 0;
   int		array_line = 0;
#else /* KEY Bug 10177 */
   int		array_column;
   int		array_line;
#endif /* KEY Bug 10177 */
   int		attr_idx;
   int		bd_idx;
   int		dt_idx;
   boolean	found_colon;
   boolean	GT_encountered;
   boolean	have_attr_list		= FALSE;
   int		idx;
   int		init_ir_idx;
   opnd_type	init_opnd;
   boolean	junk;
   int		np_idx;
   int		old_bd_idx;
#ifdef KEY /* Bug 10177 */
   int		save_column = 0;
   int		save_line = 0;
#else /* KEY Bug 10177 */
   int		save_column;
   int		save_line;
#endif /* KEY Bug 10177 */
   int		sn_idx;
   int		stmt_number;
   boolean	type_err;
   int		type_idx;


   TRACE (Func_Entry, "parse_cpnt_dcl_stmt", NULL);

   found_colon			= FALSE;
   colon_recovery		= TRUE;		   /* Can recover at ::  */
   type_err			= !parse_type_spec(TRUE);  /* Get KIND   */
   type_idx			= ATD_TYPE_IDX(AT_WORK_IDX);
   AT_DCL_ERR(AT_WORK_IDX)	= type_err;
   stmt_number			= statement_number;

   if (TYP_TYPE(type_idx) == Character) { /* Must be const len char */
      ATT_CHAR_CPNT(CURR_BLK_NAME) = TRUE;

      if (fold_relationals(TYP_IDX(type_idx), CN_INTEGER_ZERO_IDX, Lt_Opr)) {

         /* Zero Length character */

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Character;
         TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
         TYP_DESC(TYP_WORK_IDX)		= TYP_DESC(type_idx);
         TYP_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(type_idx);
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)		= CN_INTEGER_ZERO_IDX;
         type_idx			= ntr_type_tbl();
         ATD_TYPE_IDX(AT_WORK_IDX)	= type_idx;
      }
   }
   else if (TYP_TYPE(type_idx) != Structure) {
      ATT_NUMERIC_CPNT(CURR_BLK_NAME) = TRUE;
   }

   if (TYP_DESC(type_idx) == Default_Typed ||
       TYP_LINEAR(type_idx) == INTEGER_DEFAULT_TYPE ||
       TYP_LINEAR(type_idx) == LOGICAL_DEFAULT_TYPE ||
       TYP_LINEAR(type_idx) == REAL_DEFAULT_TYPE ||
       TYP_LINEAR(type_idx) == DOUBLE_DEFAULT_TYPE ||
       TYP_LINEAR(type_idx) == COMPLEX_DEFAULT_TYPE) {

       /* Intentionally blank */
   }
   else {
      ATT_NON_DEFAULT_CPNT(CURR_BLK_NAME) = TRUE;
   } 

   /* Assume all type errors issued - la_ch is comma, ::, or id */

   while (LA_CH_VALUE == COMMA) {
      NEXT_LA_CH;			/* Skip Comma */

      if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {

         switch (TOKEN_VALUE(token)) {

#ifdef KEY /* Bug 6845 */
	    case Tok_Kwd_Allocatable:

               if (ATD_ALLOCATABLE(AT_WORK_IDX)) { /* duplicate error msg */
                  PRINTMSG (TOKEN_LINE(token), 273, Error, TOKEN_COLUMN(token),
		    "ALLOCATABLE");
               }

               have_attr_list			= TRUE;
	       ATD_ALLOCATABLE(AT_WORK_IDX)	= TRUE;
	       ATD_IM_A_DOPE(AT_WORK_IDX)       = TRUE;
	       save_line = array_line = TOKEN_LINE(token);
	       save_column = array_column = TOKEN_COLUMN(token);
	       ATT_ALLOCATABLE_CPNT(CURR_BLK_NAME) = TRUE;
	       ATT_NUMERIC_CPNT(CURR_BLK_NAME)	= TRUE;
	       break;
#endif /* KEY Bug 6845 */

            case Tok_Kwd_Pointer:

               if (ATD_POINTER(AT_WORK_IDX)) { /* duplicate error msg */
                  PRINTMSG (TOKEN_LINE(token), 273, Error, 
                            TOKEN_COLUMN(token), "POINTER");
               }

               have_attr_list			= TRUE;
               ATD_POINTER(AT_WORK_IDX)		= TRUE;
               ATD_IM_A_DOPE(AT_WORK_IDX)	= TRUE;
               ATT_POINTER_CPNT(CURR_BLK_NAME)	= TRUE;
               ATT_NUMERIC_CPNT(CURR_BLK_NAME)	= TRUE;
               break;

            case Tok_Kwd_Dimension:

               if (ATD_ARRAY_IDX(AT_WORK_IDX) != NULL_IDX) { /* Duplicate err */
                  PRINTMSG (TOKEN_LINE(token), 273, Error, 
                            TOKEN_COLUMN(token), "DIMENSION");
               }

               have_attr_list	= TRUE;

               if (LA_CH_VALUE == LPAREN) {
                  array_line			= TOKEN_LINE(token);
                  array_column			= TOKEN_COLUMN(token);
                  idx				= parse_array_spec(AT_WORK_IDX);
                  ATD_ARRAY_IDX(AT_WORK_IDX)	= idx;
               }
# ifdef _F_MINUS_MINUS
               else if (!cmd_line_flags.co_array_fortran ||
                        LA_CH_VALUE != LBRKT) 
# else
               else 
# endif
                          {    /* DIMENSION attribute must have an array spec */

                  parse_err_flush(Find_Comma, "("); 
                  AT_DCL_ERR(AT_WORK_IDX) = TRUE;
               }

# ifdef _F_MINUS_MINUS
               if (LA_CH_VALUE == LBRKT && cmd_line_flags.co_array_fortran) {
                  ATD_PE_ARRAY_IDX(AT_WORK_IDX) = 
                                           parse_pe_array_spec(AT_WORK_IDX);
               }
# endif

               break;
                          
            default: /* POINTER and/or DIMENSION must follow the first comma */
               PRINTMSG(TOKEN_LINE(token), 197, Error, TOKEN_COLUMN(token),
                        "POINTER or DIMENSION", TOKEN_STR(token));
               parse_err_flush(Find_Comma, NULL);
               AT_DCL_ERR(AT_WORK_IDX) = TRUE;
               break;

         }   /* switch */
      }
      else {
         parse_err_flush(Find_Comma, "POINTER or DIMENSION");
      }
   }  /* end while */

#ifdef KEY /* Bug 6845 */
   if (ATD_ALLOCATABLE(AT_WORK_IDX)) {
     if (ATD_POINTER(AT_WORK_IDX)) {
	PRINTMSG(save_line, 425, Error, save_column, "POINTER", "ALLOCATABLE");
     }
   }
#endif /* KEY Bug 6845 */

   found_colon = matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct);

   if (!found_colon && have_attr_list) {
      PRINTMSG (LA_CH_LINE, 187, Error, LA_CH_COLUMN);
   }

   colon_recovery = FALSE;					   /* Past :: */

   if (TYP_TYPE(type_idx) == Structure) {

      if (!ATD_POINTER(AT_WORK_IDX)) {
         dt_idx	= TYP_IDX(type_idx);

         if (CURR_BLK_NAME == dt_idx) {        /* Points to itself */
            PRINTMSG(TOKEN_LINE(token), 33, Error, TOKEN_COLUMN(token));
            ATT_NUMERIC_CPNT(CURR_BLK_NAME)	= TRUE;
            AT_DCL_ERR(AT_WORK_IDX)		= TRUE;
            AT_DCL_ERR(CURR_BLK_NAME)		= TRUE;
         }
         else if (!AT_DEFINED(dt_idx)) {
            ATT_NUMERIC_CPNT(CURR_BLK_NAME)	= TRUE;

            if (!AT_DCL_ERR(AT_WORK_IDX)) {
               AT_DCL_ERR(AT_WORK_IDX)		= TRUE;

               /* Must have components declared before they are used. */

               if (!AT_DCL_ERR(dt_idx)) {
                  issue_undefined_type_msg(dt_idx, 
                                           TOKEN_LINE(token),
                                           TOKEN_COLUMN(token));
               }
            }
         }
         else {  /* This type must be defined by this point */
            ATT_CHAR_CPNT(CURR_BLK_NAME)        |= ATT_CHAR_CPNT(dt_idx);
            ATT_NUMERIC_CPNT(CURR_BLK_NAME)     |= ATT_NUMERIC_CPNT(dt_idx);
            ATT_POINTER_CPNT(CURR_BLK_NAME)     |= ATT_POINTER_CPNT(dt_idx);
#ifdef KEY /* Bug6845 */
	    ATT_ALLOCATABLE_CPNT(CURR_BLK_NAME) |= ATT_ALLOCATABLE_CPNT(dt_idx);
#endif /* KEY Bug6845 */
            ATT_NON_DEFAULT_CPNT(CURR_BLK_NAME) |= ATT_NON_DEFAULT_CPNT(dt_idx);
            ATT_DEFAULT_INITIALIZED(CURR_BLK_NAME) |= 
                                                ATT_DEFAULT_INITIALIZED(dt_idx);
         }
      }
   }

   alignment = WORD_ALIGN;

   if (ATD_POINTER(AT_WORK_IDX)) {

      if (cmd_line_flags.s_pointer8) {
         alignment = Align_64;
      }
      else {
         alignment = WORD_ALIGN;
      }
   }
   else if (TYP_TYPE(type_idx) == Structure) {
         alignment = ATT_ALIGNMENT(TYP_IDX(type_idx));
   }
   else if (TYP_TYPE(type_idx) == Character) {

# if defined(_CHAR_IS_ALIGN_8)
      alignment = Align_8;
# else
      alignment = Align_Bit;
# endif
   }

# if defined(_ALIGN_REAL16_TO_16_BYTES)

   else if (TYP_LINEAR(type_idx) == Complex_16 ||
            TYP_LINEAR(type_idx) == Real_16) {
      alignment = Align_128;
   }
# endif

# if defined(_TARGET_PACK_HALF_WORD_TYPES)

   else if (dump_flags.pack_half_word && 
            PACK_HALF_WORD_TEST_CONDITION(type_idx)) {
      alignment	= Align_32;
   }
# endif

# if defined(_HOST32) 
 
   else if (DALIGN_TEST_CONDITION(type_idx)) {
      alignment = Align_64;
   }
# endif

# if defined(_INTEGER_1_AND_2)

   else if (on_off_flags.integer_1_and_2 &&
            PACK_8_BIT_TEST_CONDITION(type_idx)) {
      alignment	= Align_8;
   }
   else if (on_off_flags.integer_1_and_2 &&
            PACK_16_BIT_TEST_CONDITION(type_idx)){
      alignment	= Align_16;
   }

# endif

   if (ATT_ALIGNMENT(CURR_BLK_NAME) < alignment) {
      ATT_ALIGNMENT(CURR_BLK_NAME) = alignment;
   }

   do {
      if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_err_flush(Find_Comma, "component-name");
         continue;
      }

      sn_idx	= ATT_FIRST_CPNT_IDX(CURR_BLK_NAME);
      attr_idx	= srch_linked_sn(TOKEN_STR(token),
                                 TOKEN_LEN(token),
                                 &sn_idx);

      if (attr_idx == NULL_IDX) {
         NTR_SN_TBL(sn_idx);
         NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
         NTR_ATTR_TBL(attr_idx);
         AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
         AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
         AT_NAME_LEN(attr_idx)		= TOKEN_LEN(token);
         AT_NAME_IDX(attr_idx)		= np_idx;
         SN_NAME_LEN(sn_idx)		= TOKEN_LEN(token);
         SN_NAME_IDX(sn_idx)		= np_idx;
         SN_ATTR_IDX(sn_idx)		= attr_idx;

         if (BLK_LAST_CPNT_IDX(blk_stk_idx) == NULL_IDX) {
            ATT_FIRST_CPNT_IDX(CURR_BLK_NAME)	= sn_idx;
            ATT_NUM_CPNTS(CURR_BLK_NAME)	= 1;
         }
         else {
            ATT_NUM_CPNTS(CURR_BLK_NAME)		       += 1;
            SN_SIBLING_LINK(BLK_LAST_CPNT_IDX(blk_stk_idx))	= sn_idx;
         }
         BLK_LAST_CPNT_IDX(blk_stk_idx) = sn_idx;
      }
      else {   /* Error - Duplicate component names for this derived type */
         PRINTMSG (TOKEN_LINE(token), 188, Error, TOKEN_COLUMN(token),
                   AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)	= TRUE;
      }

      /* Mark semantics done, so it doesn't go thru declaration semantics */

      AT_SEMANTICS_DONE(attr_idx)	= TRUE;
      ATD_CLASS(attr_idx)		= Struct_Component;
      ATD_DERIVED_TYPE_IDX(attr_idx)	= CURR_BLK_NAME;
      ATD_ARRAY_IDX(attr_idx)		= ATD_ARRAY_IDX(AT_WORK_IDX);
      ATD_PE_ARRAY_IDX(attr_idx)	= ATD_PE_ARRAY_IDX(AT_WORK_IDX);
      ATD_POINTER(attr_idx)		= ATD_POINTER(AT_WORK_IDX);
#ifdef KEY /* Bug 6845 */
      ATD_ALLOCATABLE(attr_idx)	= ATD_ALLOCATABLE(AT_WORK_IDX);
#endif /* KEY Bug 6845 */
      ATD_IM_A_DOPE(attr_idx)		= ATD_IM_A_DOPE(AT_WORK_IDX);
      save_line				= array_line;
      save_column			= array_column;
      AT_TYPED(attr_idx)		= AT_TYPED(AT_WORK_IDX);
      AT_DCL_ERR(attr_idx)		= AT_DCL_ERR(AT_WORK_IDX);

      if (type_err) {
         SET_IMPL_TYPE(attr_idx);
      }
      else {
         ATD_TYPE_IDX(attr_idx)	= type_idx;
      }

      if ((cif_flags & XREF_RECS) != 0) {
         cif_usage_rec(attr_idx,
                       AT_Tbl_Idx,
                       TOKEN_LINE(token),
                       TOKEN_COLUMN(token),
                       CIF_Symbol_Declaration);
      }

      if (LA_CH_VALUE == LPAREN) {
         save_line			= TOKEN_LINE(token);
         save_column			= TOKEN_COLUMN(token);
         idx				= parse_array_spec(attr_idx);
         ATD_ARRAY_IDX(attr_idx)	= idx;
      }

# ifdef _F_MINUS_MINUS
      if (LA_CH_VALUE == LBRKT && cmd_line_flags.co_array_fortran) {
         ATD_PE_ARRAY_IDX(attr_idx) = parse_pe_array_spec(attr_idx);
      }
# endif

      bd_idx = ATD_ARRAY_IDX(attr_idx);

#ifdef KEY /* Bug 6845 */
      if (ATD_ALLOCATABLE(attr_idx)) {
	if (bd_idx == NULL_IDX || BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {
	   PRINTMSG(save_line, 570, Error, save_column,
	      AT_OBJ_NAME_PTR(attr_idx));
	   AT_DCL_ERR(attr_idx)	= TRUE;
	}
      }
      else
#endif /* KEY Bug 6845 */
      if (bd_idx != NULL_IDX) {  /* Array declared */
         AT_DCL_ERR(attr_idx) = BD_DCL_ERR(bd_idx) | AT_DCL_ERR(attr_idx);

         if (ATD_POINTER(attr_idx)) {

            if (BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {
               PRINTMSG(save_line, 189, Error, save_column,
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(attr_idx)	 = TRUE;
            }
         }
	 else if (BD_ARRAY_CLASS(bd_idx) != Explicit_Shape ||
		  BD_ARRAY_SIZE(bd_idx) != Constant_Size) {
	    PRINTMSG(save_line, 190, Error, save_column,
		     AT_OBJ_NAME_PTR(attr_idx));
	    AT_DCL_ERR(attr_idx)	= TRUE;
	 }
      }

      if (LA_CH_VALUE == STAR) {
         save_line			= LA_CH_LINE;
         save_column			= LA_CH_COLUMN;

         /* Pick up character length.  LEN = is not allowed here (FALSE)   */
         /* We are not parsing the character* part of the line, so this    */
         /* is not the length_selector.  It is the char-length on the name */

         parse_length_selector(attr_idx, FALSE, FALSE);

         TYP_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(type_idx);
         TYP_DESC(TYP_WORK_IDX)		= TYP_DESC(type_idx);

         if (TYP_TYPE(type_idx) != Character) {
            PRINTMSG(save_line, 192, Error, save_column);
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
         else if (TYP_CHAR_CLASS(TYP_WORK_IDX) == Const_Len_Char) { 

            if (fold_relationals(TYP_IDX(TYP_WORK_IDX), 
                                 CN_INTEGER_ZERO_IDX, 
                                 Le_Opr)) {
               TYP_IDX(TYP_WORK_IDX)	= CN_INTEGER_ZERO_IDX;
            }

            ATD_TYPE_IDX(attr_idx)	= ntr_type_tbl();
         }
         else if (!AT_DCL_ERR(attr_idx)) {

            /* Must be a constant length char */

            PRINTMSG(save_line, 191, Error, save_column);

            ATD_TYPE_IDX(attr_idx)	= CHARACTER_DEFAULT_TYPE;
            AT_DCL_ERR(attr_idx)	= TRUE;
         }

         /* Have a different character length than the one specified on the   */
         /* CHARACTER component statement.  (ie:  CHARACTER*(2) :: A*(10),B)  */
         /* If this is an array, it may need a seperate bounds table entry if */
         /* this is a shared array entry.  The stride multiplier is kept in   */
         /* the bounds table and is dependent on type.  Therefore, if two     */
         /* items have seperate types, they must have seperate bounds entries.*/
         /* Ex:  CHARACTER*(2), DIMENSION(100) :: A*(10), B   ! A and B need  */
         /*                     seperate bounds entries.                      */
         /*      CHARACTER*(2), DIMENSION(100) :: A(20)*(10), B  ! They       */
         /*                     already have seperate bounds entries, because */
         /*                     they have seperate dimensions.                */
         /*      CHARACTER*(2), DIMENSION(100) :: A,B  ! They have the same   */
         /*                     type, so they can share a bound entry.        */

         old_bd_idx	= ATD_ARRAY_IDX(attr_idx);

         if (old_bd_idx != NULL_IDX && 
             old_bd_idx == ATD_ARRAY_IDX(AT_WORK_IDX) &&
             BD_ARRAY_CLASS(old_bd_idx) != Deferred_Shape) {
            bd_idx = reserve_array_ntry(BD_RANK(old_bd_idx));
            COPY_BD_NTRY(bd_idx, old_bd_idx);
            ATD_ARRAY_IDX(attr_idx) = ntr_array_in_bd_tbl(bd_idx);
         }
      }

      if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
         bd_idx	= ATD_ARRAY_IDX(attr_idx);
      
         if (BD_RESOLVED(bd_idx) ||
             BD_ARRAY_CLASS(bd_idx) == Deferred_Shape ||
             BD_ARRAY_CLASS(bd_idx) == Assumed_Shape) {
         }
         else {

            /* All the array bounds must be constants.  parse_array_spec */
            /* calls parse_int_spec_expr, which guarantees these to be   */
            /* constants if CURR_BLK == Derived_Type_Blk.                */

            if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size) {

               /* A component cannot be assumed size.  So copy the array     */
               /* entry and make the last dimension have a upper bound equal */
               /* to the lower bound. Can't just make the upper bound be 1,  */
               /* because it could end up being a zero-sized array.          */

               old_bd_idx	= bd_idx;
               bd_idx		= reserve_array_ntry(BD_RANK(old_bd_idx));
               COPY_BD_NTRY(bd_idx, old_bd_idx);
               BD_UB_IDX(bd_idx, BD_RANK(bd_idx)) = BD_LB_IDX(bd_idx,
                                                              BD_RANK(bd_idx));
               BD_UB_FLD(bd_idx, BD_RANK(bd_idx)) = BD_LB_FLD(bd_idx,
                                                              BD_RANK(bd_idx));
               BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
               BD_DCL_ERR(bd_idx)	= TRUE;
               ATD_ARRAY_IDX(attr_idx)	= ntr_array_in_bd_tbl(bd_idx);
            }
            array_bounds_resolution(attr_idx, &junk);
         }
      }

      if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
         PRINTMSG(BD_LINE_NUM(ATD_PE_ARRAY_IDX(attr_idx)), 1579, Error,
                  BD_COLUMN_NUM(ATD_PE_ARRAY_IDX(attr_idx)),
                  AT_OBJ_NAME_PTR(attr_idx),
                  AT_OBJ_NAME_PTR(CURR_BLK_NAME));
         AT_DCL_ERR(attr_idx)		= TRUE;
         ATD_PE_ARRAY_IDX(attr_idx)	= NULL_IDX;
      }

      if (LA_CH_VALUE == EQUAL) {
         NEXT_LA_CH;
         save_line	= LA_CH_LINE;
         save_column	= LA_CH_COLUMN;

         if (LA_CH_VALUE == GT) {
            NEXT_LA_CH;
            save_line		= LA_CH_LINE;
            save_column		= LA_CH_COLUMN;
            GT_encountered	= TRUE;
         }
         else {
            GT_encountered	= FALSE;
         }
  
         if (parse_expr(&init_opnd)) {

            if (!found_colon) {
               PRINTMSG(save_line, 121, Error, save_column);
               AT_DCL_ERR(attr_idx) = TRUE;
            }

            NTR_IR_TBL(init_ir_idx);
            ATD_CPNT_INIT_IDX(attr_idx)			= init_ir_idx;
            ATD_FLD(attr_idx)				= IR_Tbl_Idx;
            ATT_DEFAULT_INITIALIZED(CURR_BLK_NAME)	= TRUE;

            if (OPND_FLD(init_opnd) == IR_Tbl_Idx &&
                IR_OPR(OPND_IDX(init_opnd)) == Call_Opr &&
                AT_IS_INTRIN(IR_IDX_L(OPND_IDX(init_opnd))) &&
                strcmp(AT_OBJ_NAME_PTR(IR_IDX_L(OPND_IDX(init_opnd))),
                                                                 "NULL") == 0) {

               if (IR_IDX_R(OPND_IDX(init_opnd)) != NULL_IDX) {
                  PRINTMSG(IR_LINE_NUM(OPND_IDX(init_opnd)), 1573, Error, 
                           IR_COL_NUM(OPND_IDX(init_opnd))); 
                  AT_DCL_ERR(attr_idx)		= TRUE;
                  ATD_CPNT_INIT_IDX(attr_idx)	= NULL_IDX;
                  ATD_FLD(attr_idx)		= NO_Tbl_Idx;
               }

               IR_OPR(init_ir_idx) = Null_Opr;

               if (!GT_encountered) {
                  PRINTMSG(TOKEN_LINE(token), 1562, Error, TOKEN_COLUMN(token));
                  AT_DCL_ERR(attr_idx)		= TRUE;
                  ATD_CPNT_INIT_IDX(attr_idx)	= NULL_IDX;
                  ATD_FLD(attr_idx)		= NO_Tbl_Idx;
               }
            }
            else {
               IR_OPR(init_ir_idx) = Init_Opr;

               if (GT_encountered) {
                  PRINTMSG(TOKEN_LINE(token), 1562, Error, TOKEN_COLUMN(token));
                  AT_DCL_ERR(attr_idx)		= TRUE;
                  ATD_CPNT_INIT_IDX(attr_idx)	= NULL_IDX;
                  ATD_FLD(attr_idx)		= NO_Tbl_Idx;
               }
            }

            if (ATD_CPNT_INIT_IDX(attr_idx) != NULL_IDX) {
               IR_TYPE_IDX(init_ir_idx)    = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(init_ir_idx)    = AT_DEF_LINE(attr_idx);
               IR_COL_NUM(init_ir_idx)     = AT_DEF_COLUMN(attr_idx);
               IR_LINE_NUM_L(init_ir_idx)  = AT_DEF_LINE(attr_idx);
               IR_COL_NUM_L(init_ir_idx)   = AT_DEF_COLUMN(attr_idx);
               IR_FLD_L(init_ir_idx)       = AT_Tbl_Idx;
               IR_IDX_L(init_ir_idx)       = attr_idx;

               COPY_OPND(IR_OPND_R(init_ir_idx), init_opnd);
            }
         }
         else { /* error from parse_expr */
            AT_DCL_ERR(attr_idx) = TRUE;
         }
      }

      if (!AT_DCL_ERR(attr_idx)) {
#ifdef KEY /* Bug 14150 */
         assign_bind_c_offset(attr_idx,
	   AT_OBJ_CLASS(CURR_BLK_NAME) == Derived_Type &&
	     AT_BIND_ATTR(CURR_BLK_NAME)); /* Assign offsets to components */
#else /* KEY Bug 14150 */
         assign_offset(attr_idx);	/* Assign offsets to components */
#endif /* KEY Bug 14150 */
      }
      else {
         ATD_CPNT_OFFSET_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;
         ATD_OFFSET_FLD(attr_idx)	= CN_Tbl_Idx;
      }

      ATD_OFFSET_ASSIGNED(attr_idx)	= TRUE;
      AT_DCL_ERR(CURR_BLK_NAME)		= AT_DCL_ERR(CURR_BLK_NAME) ||
                                          AT_DCL_ERR(attr_idx);

   }   /* Do while */
   while (LA_CH_VALUE == COMMA &&
          matched_specific_token(Tok_Punct_Comma, Tok_Class_Punct));

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, ", or " EOS_STR);
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_Type_Declaration_Stmt, stmt_number);
   }

   NEXT_LA_CH;				/* Skip EOS */

   TRACE (Func_Exit, "parse_cpnt_dcl_stmt", NULL);

   return;

}  /* parse_cpnt_dcl_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse the DATA statement.					      *|
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

void parse_data_stmt (void)

{
   int		attr_idx;
   boolean	found_attr;
   boolean	found_comma	= FALSE;
   int		il_idx;
   int		init_ir_idx;
   int 		name_column;
   int		name_idx;
   int		name_line;
   int		obj_chain_end;
   opnd_type	opnd;


   TRACE (Func_Entry, "parse_data_stmt", NULL);

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Data_Stmt)  ||
        STMT_CANT_BE_IN_BLK(Data_Stmt, CURR_BLK))             &&
       iss_blk_stk_err()) {

      /* Issued block error - intentionally blank.			      */

   }
   else if (curr_stmt_category < Declaration_Stmt_Cat) {
      curr_stmt_category = Declaration_Stmt_Cat;
   }
   else if (curr_stmt_category > Declaration_Stmt_Cat) {
      PRINTMSG(TOKEN_LINE(token), 1571,
#ifdef KEY /* Bug 318, 321 */
               Ansi,            /* Obsolescent */
#else /* KEY Bug 318, 321 */
               Comment,            /* Obsolescent */
#endif /* KEY Bug 318, 321 */
               TOKEN_COLUMN(token));
   }

DATA_STMT_SET:

   obj_chain_end      = NULL_IDX;
   TOKEN_VALUE(token) = Tok_Const_False;

   NTR_IR_TBL(init_ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = init_ir_idx;
   IR_OPR(init_ir_idx)         = Init_Opr;
   IR_TYPE_IDX(init_ir_idx)    = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(init_ir_idx)    = LA_CH_LINE;
   IR_COL_NUM(init_ir_idx)     = LA_CH_COLUMN;

   while (MATCHED_TOKEN_CLASS(Tok_Class_Id)  ||  LA_CH_VALUE == LPAREN) {

      found_comma = FALSE;

      if (TOKEN_VALUE(token) != Tok_Const_False) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

         if (attr_idx == NULL_IDX) {
            found_attr           = FALSE;
            attr_idx		 = ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx) = TRUE;
            SET_IMPL_TYPE(attr_idx);
         }
         else {
            found_attr = TRUE;

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
#ifdef KEY /* Bug 6845 */
	    /* Would wait till semantics to detect this error, but we're about
	     * to blunder into calling constant_value_semantics(), which will
	     * print a nonsensical error, so we want to emit this one first. */
	    int typ_idx = TYP_IDX(ATD_TYPE_IDX(attr_idx));
	    if (AT_OBJ_CLASS(typ_idx) == Derived_Type &&
	      ATT_ALLOCATABLE_CPNT(typ_idx)) {
	      PRINTMSG(TOKEN_LINE(token), 1680, Error, TOKEN_COLUMN(token),
	        AT_OBJ_NAME_PTR(attr_idx));
	    }
#endif /* KEY Bug 6845 */
         }


         name_line   = TOKEN_LINE(token);
         name_column = TOKEN_COLUMN(token);

         /* Note: If a cross reference was requested, the Usage record for the*/
         /* target will be produced by the Semantics Pass so that expr_       */
         /* semantics can produce a "modification" record for the target and  */
         /* "reference" records for any subscripts, substring expressions,    */
         /* etc.							      */

         if (LA_CH_VALUE == LPAREN  ||  LA_CH_VALUE == PERCENT) {

            if (parse_deref(&opnd, NULL_IDX)) {

               if (OPND_FLD(opnd) == IR_Tbl_Idx  &&
                   IR_OPR(OPND_IDX(opnd)) == Call_Opr) {
                  PRINTMSG(name_line, 699, Error, name_column);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }
            }
            else {
               parse_err_flush(Find_EOS, NULL);
               goto EXIT;
            }
         }
         else {
            OPND_LINE_NUM(opnd) = TOKEN_LINE(token);
            OPND_COL_NUM(opnd)  = TOKEN_COLUMN(token);
            OPND_FLD(opnd)      = AT_Tbl_Idx;
            OPND_IDX(opnd)      = attr_idx;
         }

         if (! merge_data(found_attr, name_line, name_column, attr_idx)) {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }
      }
      else {

         if (! parse_data_imp_do(&opnd)) {
              parse_err_flush(Find_EOS, NULL);
              goto EXIT;
         }
      }

      NTR_IR_LIST_TBL(il_idx);
      COPY_OPND(IL_OPND(il_idx), opnd);

      switch (IL_FLD(il_idx)) {

         case AT_Tbl_Idx:
            IL_LINE_NUM(il_idx) = TOKEN_LINE(token);
            IL_COL_NUM(il_idx)  = TOKEN_COLUMN(token);
            break;
      
         case IR_Tbl_Idx: 
            IL_LINE_NUM(il_idx) = IR_LINE_NUM(IL_IDX(il_idx));
            IL_COL_NUM(il_idx)  = IR_COL_NUM(IL_IDX(il_idx));
            break;

         default:
            PRINTMSG(stmt_start_line, 179, Internal, stmt_start_col,
                     "parse_data_stmt");
      }

      if (obj_chain_end == NULL_IDX) {
         IR_FLD_L(init_ir_idx) = IL_Tbl_Idx;
         IR_IDX_L(init_ir_idx) = il_idx;
      }
      else {
         IL_NEXT_LIST_IDX(obj_chain_end) = il_idx;
         IL_PREV_LIST_IDX(il_idx)        = obj_chain_end;
      }

      obj_chain_end = il_idx;
      ++IR_LIST_CNT_L(init_ir_idx);

      TOKEN_VALUE(token) = Tok_Const_False;

      if (LA_CH_VALUE == COMMA) {
         found_comma = TRUE;
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE != SLASH) {
         parse_err_flush(Find_EOS, "comma or /");
         goto EXIT;
      } 

   }  /* End while */


   /* We have just found a token that does not belong in the data-stmt-object */
   /* list.  At this point, we could be processing either the first target    */
   /* list or trying to process a target list following a value list.         */
   /* Have we actually seen any targets in the current list we're trying to   */
   /* parse?								      */
   /*   Y: Was the last token a comma?					      */
   /*        Y: Error.  The comma must be followed by an id or implied-DO.    */
   /*        N: OK.  Go see if the next token is a '/'.			      */
   /*   N: Are we trying to parse the first target list?		      */
   /*        Y: Error.  The first thing must be an id or an implied-DO.       */
   /*        N: Was the last token a comma?				      */
   /*             Y: Error.  The comma must be followed by an id or implied-  */
   /*                DO.						      */
   /*             N: Error.  The next token must be a target, comma, or EOS.  */

   if (IR_IDX_L(init_ir_idx) != NULL_IDX) { 

      if (found_comma) {
         parse_err_flush(Find_EOS, "data-stmt-object");
         goto EXIT;
      }
   }
   else {

      if (! SH_COMPILER_GEN(curr_stmt_sh_idx)) {
         parse_err_flush(Find_EOS, "data-stmt-object");
         goto EXIT;
      }
      else {

         if (found_comma) {
            parse_err_flush(Find_EOS, "data-stmt-object");
            goto EXIT;
         }
         else {
            parse_err_flush(Find_EOS, "comma, data-stmt-object, or EOS");
            goto EXIT;
         }
      }
   }

   if (LA_CH_VALUE == SLASH) {
      NEXT_LA_CH;

      if (!parse_initializer(init_ir_idx)) {
         goto EXIT;
      }

      if (LA_CH_VALUE == COMMA) {
         found_comma = TRUE;
         NEXT_LA_CH;
      }
      else {
         found_comma = FALSE;
      }
      
      if (LA_CH_VALUE != EOS) {
         gen_sh(After, Data_Stmt, LA_CH_LINE, LA_CH_COLUMN, FALSE, FALSE, TRUE);
         goto DATA_STMT_SET;
      }
      else if (found_comma) {
         parse_err_flush(Find_EOS, "data-stmt-object");
      }
   }
   else {
      parse_err_flush(Find_EOS, "/");
   }
            
EXIT:

   NEXT_LA_CH;
   strcpy(parse_operand_insert, "operand");
   
   TRACE (Func_Exit, "parse_data_stmt", NULL);

   return;

}  /* parse_data_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF       - TYPE [[,access_spec]::type-name                           *|
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

static void parse_derived_type_stmt()

{
   access_type	 access;
   boolean	 access_set	= FALSE;
   int		 dt_idx		= NULL_IDX;
   boolean	 err;
   int		 name_idx;
   char		*str;
#ifdef KEY /* Bug 14150 */
   int		 found_bind = 0;
#endif /* KEY Bug 14150 */


   TRACE (Func_Entry, "parse_derived_type_stmt", NULL);

   access = (access_type) AT_PRIVATE(SCP_ATTR_IDX(curr_scp_idx));

   if (LA_CH_VALUE == COMMA) {
      colon_recovery	= TRUE;				 /* Can recover at :: */
      NEXT_LA_CH;					 /* Skip COMMA        */

      if (matched_specific_token(Tok_Kwd_Private, Tok_Class_Keyword)  ||
          matched_specific_token(Tok_Kwd_Public, Tok_Class_Keyword)) {
         access     = TOKEN_VALUE(token) == Tok_Kwd_Private ? Private : Public;
         access_set = TRUE;

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {
            str	    = access == Private ? "PRIVATE" : "PUBLIC";
            PRINTMSG(TOKEN_LINE(token), 596, Error, TOKEN_COLUMN(token), str);
            access_set = FALSE;
         }

         if (!matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct)) {
            parse_err_flush(Find_None, "::");
         }
      }
#ifdef KEY /* Bug 14150 */
      else if (matched_specific_token(Tok_Kwd_Bind, Tok_Class_Keyword)) {
	 parse_language_binding_spec(0);
	 found_bind = 1;
         if (!matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct)) {
            parse_err_flush(Find_None, "::");
         }
      }
#endif /* KEY Bug 14150 */
      else {
         parse_err_flush(Find_None, 
#ifdef KEY /* Bug 14150 */
            "BIND, PUBLIC, or PRIVATE"
#else /* KEY Bug 14150 */
	    "PUBLIC or PRIVATE"
#endif /* KEY Bug 14150 */
	    );
         /* Bypass ::, just in case it's there */
         matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct);
      }
      colon_recovery = FALSE;
   }
   else {  /* Colon Colon is optional here */
      matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct);
   }


   if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
      
      if (LA_CH_VALUE != EOS) {
         parse_err_flush(Find_EOS, EOS_STR);
      }

      err = FALSE;

      switch (TOKEN_STR(token)[0]) {
         case 'C':
            err = (strcmp(TOKEN_STR(token), "CHARACTER") == 0) ||
                  (strcmp(TOKEN_STR(token), "COMPLEX") == 0);
            break;
         case 'D':
            err = (strcmp(TOKEN_STR(token), "DOUBLEPRECISION") == 0);
            break;
         case 'I':
            err = (strcmp(TOKEN_STR(token), "INTEGER") == 0);
            break;
         case 'L':
            err = (strcmp(TOKEN_STR(token), "LOGICAL") == 0);
            break;
         case 'R':
            err = (strcmp(TOKEN_STR(token), "REAL") == 0);
            break;
      }  /* end switch */

      if (err) {	/* Issue msg - but allow name to be used */
         PRINTMSG (TOKEN_LINE(token), 286, Error, TOKEN_COLUMN(token),
                   TOKEN_STR(token));
      }

      dt_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

      if (dt_idx == NULL_IDX) {
         dt_idx				= ntr_sym_tbl(&token, name_idx);
         AT_OBJ_CLASS(dt_idx)		= Derived_Type;
         ATT_STRUCT_BIT_LEN_FLD(dt_idx)	= CN_Tbl_Idx;
         ATT_STRUCT_BIT_LEN_IDX(dt_idx)	= CN_INTEGER_ZERO_IDX;
         ATT_SCP_IDX(dt_idx)		= curr_scp_idx;
      }
      else if (AT_NOT_VISIBLE(dt_idx)) {
         PRINTMSG(TOKEN_LINE(token), 486, Error,
                  TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(dt_idx),
                  AT_OBJ_NAME_PTR(AT_MODULE_IDX(dt_idx)));
         CREATE_ERR_ATTR(dt_idx,
                         TOKEN_LINE(token), 
                         TOKEN_COLUMN(token),
                         Derived_Type);
         ATT_STRUCT_BIT_LEN_FLD(dt_idx)	= CN_Tbl_Idx;
         ATT_STRUCT_BIT_LEN_IDX(dt_idx)	= CN_INTEGER_ZERO_IDX;
         ATT_SCP_IDX(dt_idx)		= curr_scp_idx;
      }
      else if (AT_ATTR_LINK(dt_idx) != NULL_IDX) {
         AT_DEF_LINE(dt_idx)		= TOKEN_LINE(token);
         AT_DEF_COLUMN(dt_idx)		= TOKEN_COLUMN(token);
         AT_ATTR_LINK(dt_idx)		= NULL_IDX;
         CLEAR_VARIANT_ATTR_INFO(dt_idx, Derived_Type);
         ATT_STRUCT_BIT_LEN_FLD(dt_idx)	= CN_Tbl_Idx;
         ATT_STRUCT_BIT_LEN_IDX(dt_idx)	= CN_INTEGER_ZERO_IDX;
         ATT_SCP_IDX(dt_idx)		= curr_scp_idx;

         if (AT_LOCKED_IN(dt_idx)) {
            PRINTMSG(TOKEN_LINE(token), 390, Error, TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(dt_idx));
            AT_DCL_ERR(dt_idx)		= TRUE;
         }
      }
      else if (AT_OBJ_CLASS(dt_idx) == Derived_Type) {
         ATT_SCP_IDX(dt_idx)		= curr_scp_idx;

         if (AT_DEFINED(dt_idx)) {
            AT_DCL_ERR(dt_idx)	 = TRUE;
            PRINTMSG(TOKEN_LINE(token), 123, Error, TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(dt_idx));
         }
      }
      else if (fnd_semantic_err(Obj_Derived_Type,
                                TOKEN_LINE(token),
                                TOKEN_COLUMN(token),
                                dt_idx,
                                TRUE)) {

         /* Create an error attr - but leave LN pointing to the original one. */

         CREATE_ERR_ATTR(dt_idx,
                         TOKEN_LINE(token), 
                         TOKEN_COLUMN(token),
                         Derived_Type);
         ATT_STRUCT_BIT_LEN_FLD(dt_idx)	= CN_Tbl_Idx;
         ATT_STRUCT_BIT_LEN_IDX(dt_idx)	= CN_INTEGER_ZERO_IDX;
         ATT_SCP_IDX(dt_idx)		= curr_scp_idx;
      }
      else {  /* Can only have been specified in an access statement */

         CLEAR_VARIANT_ATTR_INFO(dt_idx, Derived_Type);
         ATT_STRUCT_BIT_LEN_FLD(dt_idx)	= CN_Tbl_Idx;
         ATT_STRUCT_BIT_LEN_IDX(dt_idx)	= CN_INTEGER_ZERO_IDX;
         ATT_SCP_IDX(dt_idx)		= curr_scp_idx;
      }

#ifdef KEY /* Bug 14150 */
      if (found_bind) {
        AT_BIND_ATTR(dt_idx) = 1;
      }
#endif /* KEY Bug 14150 */

      if (CURR_BLK != Interface_Body_Blk) {

         /* Interface_Body_Blk stuff is counted during interface collapse. */

         num_of_derived_types++;
      }

      if ((cif_flags & XREF_RECS) != 0) {
         cif_usage_rec(dt_idx, 
                       AT_Tbl_Idx,
                       TOKEN_LINE(token),
                       TOKEN_COLUMN(token),
                       CIF_Derived_Type_Name_Definition);
      }

      LN_DEF_LOC(name_idx)	= TRUE;
      AT_DEFINED(dt_idx)	= TRUE;
      AT_LOCKED_IN(dt_idx)	= TRUE;

      if (AT_ACCESS_SET(dt_idx)) {

         if (access_set) {
            AT_DCL_ERR(dt_idx)	= TRUE;
            PRINTMSG (TOKEN_LINE(token), 275, Error, TOKEN_COLUMN(token),
                      AT_OBJ_NAME_PTR(dt_idx));
         }
      }
      else {
         AT_PRIVATE(dt_idx)	= access;
         AT_ACCESS_SET(dt_idx)	= access_set;
      }
   }
   else {
      parse_err_flush(Find_EOS, "type-name");
   }

   stmt_type				= Derived_Type_Stmt;
   SH_STMT_TYPE(curr_stmt_sh_idx)	= Derived_Type_Stmt;

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Derived_Type_Stmt) ||
        STMT_CANT_BE_IN_BLK(Derived_Type_Stmt, CURR_BLK)) &&
        iss_blk_stk_err()) {
      PUSH_BLK_STK(Derived_Type_Blk);
      CURR_BLK_ERR		= TRUE;
   }
   else {
      PUSH_BLK_STK(Derived_Type_Blk);
      curr_stmt_category	= Declaration_Stmt_Cat;
   }
#ifdef KEY /* Bug 14150 */
   /* Even before the 14150 enhancement, a source error (like "type ::" with no
    * id) could make us arrive here with dt_idx == NULL_IDX, which would cause
    * subsequent references to ATT_* in parse_cpnt_dcl_stmt() to die in
    * _DEBUG mode. */
   if (dt_idx == NULL_IDX) {
#define NO_ID "<NO NAME>"
     TOKEN_LEN(token) = (sizeof NO_ID) - 1;
     CREATE_ID(TOKEN_ID(token), NO_ID, TOKEN_LEN(token));
     dt_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);
     if (dt_idx == NULL_IDX) {
       dt_idx				= ntr_sym_tbl(&token, name_idx);
       AT_OBJ_CLASS(dt_idx)		= Derived_Type;
       ATT_STRUCT_BIT_LEN_FLD(dt_idx)	= CN_Tbl_Idx;
       ATT_STRUCT_BIT_LEN_IDX(dt_idx)	= CN_INTEGER_ZERO_IDX;
       ATT_SCP_IDX(dt_idx)		= curr_scp_idx;
     }
   }
#endif /* KEY Bug 14150 */

   CURR_BLK_NO_EXEC		= TRUE;
   CURR_BLK_NAME		= dt_idx;

   NEXT_LA_CH;			/* Skip EOS */

   TRACE (Func_Exit, "parse_derived_type_stmt", NULL);

   return;

}  /* parse_derived_type_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse the EQUIVALENCE statement.                                      *|
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

void parse_equivalence_stmt (void)

{
   int		al_idx;
   int		attr_idx;
   int		column;
   int		eq_idx;
   boolean	fnd_attr;
   int		group;
   boolean	have_array;
   int		items_in_list;
   int		line;
   int		list_idx;
   int		list2_idx;
   int		name_idx;
   opnd_type	opnd;
   boolean	parsed_ok	= TRUE;
   int		rank;
   opnd_type	result_opnd;
   int		subs_idx	= NULL_IDX;
   boolean	substring;
   int		substring_idx;


   TRACE (Func_Entry, "parse_equivalence_stmt", NULL);

   if (LA_CH_VALUE == LPAREN) {

      NTR_EQ_TBL(eq_idx);

      while (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;  /* eat the ( */

         EQ_NEXT_EQUIV_GRP(eq_idx)	   = SCP_FIRST_EQUIV_GRP(curr_scp_idx);
         SCP_FIRST_EQUIV_GRP(curr_scp_idx) = eq_idx;
         group				   = eq_idx;
         items_in_list			   = 0;

         do {
            if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               attr_idx			= srch_sym_tbl(TOKEN_STR(token),  
                                                       TOKEN_LEN(token), 
                                                       &name_idx);
               fnd_attr			= attr_idx;
               line			= TOKEN_LINE(token);
               column			= TOKEN_COLUMN(token);
               EQ_LINE_NUM(eq_idx)	= line;
               EQ_COLUMN_NUM(eq_idx)	= column;
               items_in_list		= items_in_list + 1;

               if (attr_idx == NULL_IDX) {
                  attr_idx			= ntr_sym_tbl(&token, name_idx);
                  LN_DEF_LOC(name_idx)		= TRUE;
                  SET_IMPL_TYPE(attr_idx);
                  AT_OBJ_CLASS(attr_idx)	= Data_Obj;
                  ATD_CLASS(attr_idx)		= Variable;
               }
               else if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
                  AT_ATTR_LINK(attr_idx)	= NULL_IDX;
                  LN_DEF_LOC(name_idx)		= TRUE;
               }

               if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
                  ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
               }

               if ((cif_flags & XREF_RECS) != 0) { 
                  cif_usage_rec(attr_idx,
                                AT_Tbl_Idx,
                                line,
                                column,
                                CIF_Symbol_Declaration);
               }

               if (group != eq_idx) {
                  EQ_NEXT_EQUIV_OBJ(EQ_GRP_END_IDX(group))	= eq_idx;
               }

               if (!fnd_attr || !fnd_semantic_err(Obj_Equiv,
                                                  line,
                                                  column,
                                                  attr_idx,
                                                  TRUE)) {

                  NTR_ATTR_LIST_TBL(al_idx);

                  AL_IDX_IS_EQ(al_idx)		= TRUE;
                  AL_NEXT_IDX(al_idx)		= ATD_EQUIV_LIST(attr_idx);
                  AL_EQ_IDX(al_idx)		= eq_idx;
                  ATD_CLASS(attr_idx)		= Variable;
                  ATD_EQUIV(attr_idx)		= TRUE;
                  ATD_EQUIV_LIST(attr_idx)	= al_idx;
                  ATD_DCL_EQUIV(attr_idx)	= TRUE;
               }
               EQ_ATTR_IDX(eq_idx)		= attr_idx;
               EQ_GRP_IDX(eq_idx)		= group;
               EQ_GRP_END_IDX(group)		= eq_idx;

               if (LA_CH_VALUE == LPAREN) {  /* Array and/or substring */
                  expr_mode			= Initialization_Expr;
                  OPND_FLD(result_opnd)		= AT_Tbl_Idx;
                  OPND_IDX(result_opnd)		= attr_idx;
                  OPND_LINE_NUM(result_opnd)	= TOKEN_LINE(token);
                  OPND_COL_NUM(result_opnd)	= TOKEN_COLUMN(token);
                  substring			= is_substring_ref();
                  have_array	= (ATD_ARRAY_IDX(attr_idx) != NULL_IDX);

                  if (have_array && substring) {
                     PRINTMSG(TOKEN_LINE(token), 250,Error,TOKEN_COLUMN(token));
                  }

                  if (!substring) {
                     rank = 0;
                     NTR_IR_TBL(subs_idx);

                     /* copy the attr_idx */

                     COPY_OPND(IR_OPND_L(subs_idx), result_opnd);

                     /* put subs_idx into result opnd for now */

                     OPND_FLD(result_opnd)	= IR_Tbl_Idx;
                     OPND_IDX(result_opnd)	= subs_idx;

                     /* LA_CH is '(' */
                     IR_LINE_NUM(subs_idx)	= LA_CH_LINE;
                     IR_COL_NUM(subs_idx)	= LA_CH_COLUMN;
                     IR_OPR(subs_idx)		= Subscript_Opr;
                     IR_FLD_R(subs_idx)		= IL_Tbl_Idx;
                     list_idx			= NULL_IDX;

                     do {
                        NEXT_LA_CH;

                        if (list_idx == NULL_IDX) {
                           NTR_IR_LIST_TBL(list_idx);
                           IR_IDX_R(subs_idx) = list_idx;
                        }
                        else {
                           NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                           IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) =
                                                                  list_idx;
                           list_idx = IL_NEXT_LIST_IDX(list_idx);
                        }

                        if (LA_CH_VALUE != COLON) {
                           parsed_ok = parse_expr(&opnd) && parsed_ok;
                           COPY_OPND(IL_OPND(list_idx), opnd);
                        }
                        rank++;
                     }
                     while (LA_CH_VALUE == COMMA);

                     if (! matched_specific_token(Tok_Punct_Rparen,
                                                  Tok_Class_Punct)) {
                        parse_err_flush(Find_EOS, ")");
                        parsed_ok = FALSE;
                        expr_mode = Regular_Expr;
                        goto EXIT;
                     }
         
                     IR_LIST_CNT_R(subs_idx) = rank;
   
                  } /* if (array) */

                  /* now check for possible substring reference */

                  if (LA_CH_VALUE == LPAREN && is_substring_ref()) {
                     EQ_SUBSTRINGED(eq_idx)	= TRUE;
                     NTR_IR_TBL(substring_idx);
                     IR_OPR(substring_idx)	= Substring_Opr;
                     IR_LINE_NUM(substring_idx)	= LA_CH_LINE;
                     IR_COL_NUM(substring_idx)	= LA_CH_COLUMN;
    
                     COPY_OPND(IR_OPND_L(substring_idx), result_opnd);

                     /* put substring idx into result_opnd */

                     OPND_FLD(result_opnd)		= IR_Tbl_Idx;
                     OPND_IDX(result_opnd)		= substring_idx;
                     IR_FLD_R(substring_idx)		= IL_Tbl_Idx;
                     IR_LIST_CNT_R(substring_idx)	= 2;
                     NTR_IR_LIST_TBL(list_idx);
                     NTR_IR_LIST_TBL(list2_idx);
                     IR_IDX_R(substring_idx)		= list_idx;
                     IL_NEXT_LIST_IDX(list_idx)		= list2_idx;
                     IL_PREV_LIST_IDX(list2_idx)	= list_idx;

                     NEXT_LA_CH;	/* consume ( */

                     if (LA_CH_VALUE != COLON) {
                        parsed_ok = parse_expr(&opnd) && parsed_ok;
                        COPY_OPND(IL_OPND(list_idx), opnd);
                     }

                     if (LA_CH_VALUE != COLON) {

                        if (parse_err_flush(Find_EOS, ":")) {
                           NEXT_LA_CH;
                        }

                        parsed_ok = FALSE;
                        expr_mode = Regular_Expr;
                        goto EXIT;
                     }

                     NEXT_LA_CH;  /* consume : */

                     if (LA_CH_VALUE != RPAREN) {
                        parsed_ok = parse_expr(&opnd) && parsed_ok;
                        COPY_OPND(IL_OPND(list2_idx), opnd);
                     }

                     if (LA_CH_VALUE != RPAREN) {

                        if (parse_err_flush(Find_EOS, ")")) {
                           NEXT_LA_CH;
                        }
                        parsed_ok = FALSE;
                        expr_mode = Regular_Expr;
                        goto EXIT;
                     }
                     NEXT_LA_CH;     /* Consume rparen */
                  }  /* substring reference */

                  expr_mode			= Regular_Expr;
                  EQ_OPND_FLD(eq_idx)		= OPND_FLD(result_opnd);
                  EQ_OPND_IDX(eq_idx)		= OPND_IDX(result_opnd);
               }
               NTR_EQ_TBL(eq_idx);

# ifdef _F_MINUS_MINUS

               if (LA_CH_VALUE == LBRKT && cmd_line_flags.co_array_fortran) {
                  PRINTMSG(LA_CH_LINE, 1578, Error, LA_CH_COLUMN,
                           AT_OBJ_NAME_PTR(attr_idx), "EQUIVALENCE");

                  /* Disregard the list_idx.  It's just a place holder */
                  /* so that we can parse correctly.                   */

                  list2_idx = parse_pe_array_spec(attr_idx);
               }
# endif
            }
            else {
               parse_err_flush(Find_Comma_Rparen, "equivalence-object");
            }

            if (LA_CH_VALUE != COMMA && LA_CH_VALUE != RPAREN) {
               parse_err_flush(Find_EOS, ", or )");
               goto EXIT;
            }
   
            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;  /* eat the , */
            }
            else {
               break;
            }

         }  /* End while */
         while (TRUE);

         if (items_in_list < 2) {
            PRINTMSG(LA_CH_LINE, 137, Error, LA_CH_COLUMN);
         }

         if (LA_CH_VALUE != RPAREN) {
            parse_err_flush(Find_EOS, ")");
            goto EXIT; 
         }
         NEXT_LA_CH;  /* eat the ) */

         if (LA_CH_VALUE != COMMA && LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, ", or " EOS_STR);
            goto EXIT;
         }

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;  /* eat the , */
         }
      }  /* End while */

      if ((STMT_OUT_OF_ORDER(curr_stmt_category, Equivalence_Stmt) ||
           STMT_CANT_BE_IN_BLK(Equivalence_Stmt, CURR_BLK)) &&
          iss_blk_stk_err()) {
         /* Issued block stack error - intentionally left blank */
      }
      else {
         curr_stmt_category = Declaration_Stmt_Cat;
      }
   }
   else {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

EXIT:

   NEXT_LA_CH;  /* eat the EOS */
 
   TRACE (Func_Exit, "parse_equivalence_stmt", NULL);
   
   return;

}  /* parse_equivalence_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function parses the implicit statement.  If the statement is an  *|
|*	IMPLICIT NONE statement, stmt_type is changed to reflect the fact.    *|
|*	The syntax that is parsed is a follows:				      *|
|*									      *|
|*	   implicit-stmt  =>  IMPLICIT implicit-spec-list  |  IMPLICIT NONE   *|
|*	   implicit-spec  =>  type-spec ( letter-spec-list )		      *|
|*	   letter-spec    =>  letter [- letter]				      *|
|*									      *|
|*	This routine also parses and extension -> IMPLICIT UNDEFINED          *|
|*	This is the same as IMPLICIT NONE.                                    *|
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

void parse_implicit_stmt (void)

{
   int			al_idx;
   int			attr_idx;
   boolean		end_found 	= FALSE;
   int			end_idx;
   int			err_idx;
   char			err_str[80];
   boolean		found_type;
   boolean		have_kind;
   int			idx;
   boolean		implicit_undefined;
   int			name_idx;
   char			start_char;
   int			start_idx;
   int			stmt_number;
   int			storage;
   boolean		type_err;
#ifdef KEY /* Bug 10177 */
   int			type_idx = 0;
#else /* KEY Bug 10177 */
   int			type_idx;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "parse_implicit_stmt", NULL);

   stmt_number		= statement_number;
   implicit_undefined	= FALSE;

   if (LA_CH_VALUE == 'U' &&
       matched_specific_token(Tok_Kwd_Undefined, Tok_Class_Keyword)) {
      implicit_undefined = TRUE;
      PRINTMSG(stmt_start_line, 1253, Ansi, stmt_start_col,
               "IMPLICIT UNDEFINED");
   }

   if (implicit_undefined ||
       (LA_CH_VALUE == 'N' &&
        matched_specific_token(Tok_Kwd_None, Tok_Class_Keyword))) {

      if (LA_CH_VALUE == EOS) {
         stmt_type			= Implicit_None_Stmt;
         SH_STMT_TYPE(curr_stmt_sh_idx)	= Implicit_None_Stmt;

         if (cif_flags & MISC_RECS) {
            cif_stmt_type_rec(TRUE, CIF_Implicit_None_Stmt, stmt_number);
         }

         if ((STMT_OUT_OF_ORDER(curr_stmt_category, Implicit_None_Stmt) ||
              STMT_CANT_BE_IN_BLK(Implicit_None_Stmt, CURR_BLK)) &&
             iss_blk_stk_err()) {
            /* Intentionally left blank */
         }
	 else {
            curr_stmt_category = Implicit_None_Stmt_Cat;
	 }

         if (SCP_IMPL_NONE(curr_scp_idx)) { /* IMPLICIT NONE already in scope */
            PRINTMSG(stmt_start_line, 298, Error, stmt_start_col);
         }

         SCP_IMPL_NONE(curr_scp_idx)	= TRUE;
      } 
      else {
	 parse_err_flush(Find_EOS, EOS_STR);
      }

      goto EXIT;
   }  

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_Implicit_Stmt, stmt_number);
   }

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Implicit_Stmt) ||
        STMT_CANT_BE_IN_BLK(Implicit_Stmt, CURR_BLK)) &&
       iss_blk_stk_err()) {
      /* Issued block stack error - intentionally left blank */
   }
   else {
      curr_stmt_category = Implicit_Stmt_Cat;
   }

   found_type	= FALSE;

   do {

      if (!MATCHED_TOKEN_CLASS (Tok_Class_Keyword)) {

         /* We could also have AUTOMATIC or STATIC but they are not */
         /* included in the list because this is an old MIPS        */
         /* extension and we do not want to encourage this use.     */

         if (!parse_err_flush(Find_Comma, "INTEGER, REAL, DOUBLE, COMPLEX,"
                             " LOGICAL, CHARACTER or TYPE")) {
             goto EXIT;  /* Didn't find a comma */
         }
         NEXT_LA_CH;
         continue;
      }

      if (TOKEN_VALUE(token) == Tok_Kwd_Automatic) {
         storage = Impl_Automatic_Storage;
      }
      else if (TOKEN_VALUE(token) == Tok_Kwd_Static) {
         storage = Impl_Static_Storage;
      }
      else {
         storage	= Impl_Default_Storage;

         found_type	= TRUE;

         /* Set have_kind if there is more than one paren group following the */
         /* implicit type keyword.  If there is only one paren group, that    */
         /* means, that the paren group is the letter(s) for the implict type */

         have_kind = (LA_CH_VALUE == LPAREN && 
                      TOKEN_VALUE(token) != Tok_Kwd_Type &&
                      ch_after_paren_grp() == LPAREN);

         type_err	= !parse_type_spec(have_kind);
         type_idx	= ATD_TYPE_IDX(AT_WORK_IDX);

         if (type_err) { /* No valid type keyword */

            if (!parse_err_flush(Find_Comma, NULL)) {
                goto EXIT;  /* Didn't find a comma */
            }
            NEXT_LA_CH;
            continue;
         }

         if (TYP_TYPE(type_idx) == Character &&
             TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {

            /* implicit character *(*) is not allowed */

            PRINTMSG(TOKEN_LINE(token), 32, Error, TOKEN_COLUMN(token));

            if (!parse_err_flush(Find_Comma, NULL)) {
               goto EXIT;  /* Didn't find a comma */
            }
            NEXT_LA_CH;
            continue;
         }
      }

      if (LA_CH_VALUE != LPAREN) {

         if (!parse_err_flush(Find_Comma, "(")) {
            goto EXIT;
         }
         NEXT_LA_CH;
         continue;
      }

      do {
         NEXT_LA_CH;

         if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            parse_err_flush(Find_Comma_Rparen,
                        "A,B,C,D,E,F,G,H,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y or Z");
            continue;
         }

         if (TOKEN_LEN(token) > 1) {
             PRINTMSG(TOKEN_LINE(token), 197, Error, TOKEN_COLUMN(token),
                      "A,B,C,D,E,F,G,H,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y or Z",
                      TOKEN_STR(token));
             parse_err_flush(Find_Comma_Rparen, NULL);
             continue;
         }

         start_char	= TOKEN_STR(token)[0];
         start_idx	= IMPL_IDX(start_char);
         end_idx	= start_idx;

         if (LA_CH_VALUE == DASH) {
            NEXT_LA_CH;

            if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               parse_err_flush(Find_Comma_Rparen,
                        "A,B,C,D,E,F,G,H,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y or Z");
               continue;
            }

            if (TOKEN_LEN(token) > 1) {
               PRINTMSG(TOKEN_LINE(token), 197, Error,TOKEN_COLUMN(token),
                        "B,C,D,E,F,G,H,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y or Z",
                        TOKEN_STR(token));
               parse_err_flush(Find_Comma_Rparen, NULL);
               continue;
            }
            end_idx	= IMPL_IDX(TOKEN_STR(token)[0]);

            if (start_idx > end_idx) {  /* start range exceeds end */
               PRINTMSG(TOKEN_LINE(token), 175, Error,TOKEN_COLUMN(token),
                        start_char, TOKEN_STR(token)[0]);
            }
         }

         err_idx = NULL_IDX;

         if (storage == Impl_Default_Storage) {  /* Implicit type statement */

            for (idx = start_idx; idx <= end_idx; idx++) {

               if (IM_SET(curr_scp_idx, idx)) {
                  err_str[err_idx++]	= COMMA;
                  err_str[err_idx++]	= ' ';
                  err_str[err_idx++]	= idx + 'A';
               }
               else {
                  IM_SET(curr_scp_idx, idx)	= TRUE;
                  IM_TYPE_IDX(curr_scp_idx, idx)	= type_idx;
               }
            }

            if (err_idx != NULL_IDX) {
               err_str[err_idx]		= EOS;
               PRINTMSG(TOKEN_LINE(token), 1629, Error, TOKEN_COLUMN(token),
                        "type",
                        &err_str[2]);  /* Skip first , blank in string */
            }
         }
         else {
            for (idx = start_idx; idx <= end_idx; idx++) {

               if (IM_STORAGE(curr_scp_idx, idx) != Impl_Default_Storage) {
                  err_str[err_idx++]	= COMMA;
                  err_str[err_idx++]	= ' ';
                  err_str[err_idx++]	= idx + 'A';
               }
               else {
                  IM_STORAGE(curr_scp_idx, idx)	= storage;
               }
            }

            if (err_idx != NULL_IDX) {
               err_str[err_idx]		= EOS;
               PRINTMSG(TOKEN_LINE(token), 1629, Error, TOKEN_COLUMN(token),
                        "storage",
                        &err_str[2]);  /* Skip first , blank in string */
            }
         }

         if (LA_CH_VALUE != COMMA && LA_CH_VALUE != RPAREN) {
            parse_err_flush(Find_Comma_Rparen, ", or )");
         }

      }  /* End while */
      while (LA_CH_VALUE == COMMA);

      if (LA_CH_VALUE == RPAREN) {
         NEXT_LA_CH;
      }

      if (LA_CH_VALUE == EOS || (LA_CH_VALUE != COMMA &&
                                !parse_err_flush(Find_Comma, ", or " EOS_STR))){
         end_found = TRUE;
      }
      else {
	 NEXT_LA_CH;
      }
   }  /* while */
   while (!end_found);

   if (SCP_IMPL_NONE(curr_scp_idx) && found_type) {

      /* IMPLICIT NONE already set in scope */

      PRINTMSG (stmt_start_line, 176, Error, stmt_start_col);
      parse_err_flush(Find_EOS, NULL);
      goto EXIT;
   }

   for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1;
        name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {

      attr_idx = LN_ATTR_IDX(name_idx);

      if (AT_ATTR_LINK(attr_idx) == NULL_IDX && !AT_USE_ASSOCIATED(attr_idx)) {
         retype_attr(attr_idx);
      }
   }

   al_idx = SCP_ATTR_LIST(curr_scp_idx);

   while (al_idx != NULL_IDX) {

      if (AT_ATTR_LINK(AL_ATTR_IDX(al_idx)) == NULL_IDX &&
          !AT_USE_ASSOCIATED(AL_ATTR_IDX(al_idx))) {
         retype_attr(AL_ATTR_IDX(al_idx));
      }
      al_idx = AL_NEXT_IDX(al_idx);
   }

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_implicit_stmt", NULL);

   return;

}  /* parse_implicit_stmt */

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

static void retype_attr(int	attr_idx)

{
   int	old_type_idx;


   TRACE (Func_Entry, "retype_attr", NULL);

   /* Retype possible function name, dummy args, and any thing used in */
   /* a bounds expression for character.  Special case for N$PES.      */

   switch (AT_OBJ_CLASS(attr_idx)) {

   case Data_Obj:

      if (!AT_TYPED(attr_idx) && !ATD_SYMBOLIC_CONSTANT(attr_idx)) {

         if (ATD_CLASS(attr_idx) == Constant) {
            old_type_idx = ATD_TYPE_IDX(attr_idx);
            SET_IMPL_TYPE(attr_idx);

            if (old_type_idx != ATD_TYPE_IDX(attr_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 238, Error, 
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        get_basic_type_str(old_type_idx));
               ATD_TYPE_IDX(attr_idx) = old_type_idx;
            }
         }
         else if (ATD_CLASS(attr_idx) != Compiler_Tmp) {

            if (AT_REFERENCED(attr_idx) > Not_Referenced) {
               old_type_idx = ATD_TYPE_IDX(attr_idx);
               SET_IMPL_TYPE(attr_idx);

               if (old_type_idx != ATD_TYPE_IDX(attr_idx)) {
                  ATD_TYPE_IDX(attr_idx)	= old_type_idx;
                  AT_DCL_ERR(attr_idx)		= TRUE;
                  PRINTMSG(AT_DEF_LINE(attr_idx), 827, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           get_basic_type_str(old_type_idx));
               }
            }
            else {
               SET_IMPL_TYPE(attr_idx);
            }
         }
      }
      break;

   case Pgm_Unit:

      if (ATP_PGM_UNIT(attr_idx) == Function &&
          !ATP_RSLT_NAME(attr_idx) &&            /* Will catch with own name */
          !AT_TYPED(ATP_RSLT_IDX(attr_idx))) { 
         SET_IMPL_TYPE(ATP_RSLT_IDX(attr_idx));
      }
      break;

   default: /* Any stmt_functions would be host associated */
      break;

   }  /* End switch */

   TRACE (Func_Exit, "retype_attr", NULL);

   return;

}  /* retype_attr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BNF is    INTERFACE [ generic spec ]          			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|* Notes:								      *|
|*									      *|
|*      From interp 99:                                                       *|
|*      If two or more generic interfaces that are accessible in a scoping    *|
|*	unit have the same name, ..., they are interpreted as a single        *|
|*	generic interface.                                                    *|
|*									      *|
|*									      *|
\******************************************************************************/

void parse_interface_stmt (void)

{
   int		attr_idx	= NULL_IDX;
   id_str_type	name;
   int		stmt_number;


   TRACE (Func_Entry, "parse_interface_stmt", NULL);

   stmt_number = statement_number;

   if (LA_CH_VALUE != EOS) {

      if (parse_generic_spec()) {
         attr_idx = generic_spec_semantics();

         /* Even if this interface came from a module, it is being extended */
         /* in this program unit, so it is not the exact same as the one    */
         /* from the module.                                                */

         AT_MODULE_IDX(attr_idx) = NULL_IDX;

         /* CIF usage record is generated by generic_spec_semantics */

         if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, EOS_STR);
         }
         else {

            if ((cif_flags & MISC_RECS)  &&  attr_idx != NULL_IDX) {
               
               if (TOKEN_VALUE(token) == Tok_Id) {
                  cif_stmt_type_rec(TRUE, 
                                    CIF_Interface_Generic_Stmt, 
                                    stmt_number);
               }
               else if (TOKEN_VALUE(token) == Tok_Op_Assign) {
                  cif_stmt_type_rec(TRUE, 
                                    CIF_Interface_Assignment_Stmt, 
                                    stmt_number);
               }
               else {
                  cif_stmt_type_rec(TRUE, 
                                    CIF_Interface_Operator_Stmt, 
                                    stmt_number);
               }
            }
         }
      }
      else {
         CREATE_ID(name, "unnamed interface", 17);
         attr_idx = ntr_local_attr_list(name.string,
                                        17,
                                        TOKEN_LINE(token),
                                        TOKEN_COLUMN(token));
         AT_OBJ_CLASS(attr_idx)                 = Interface;
         ATI_UNNAMED_INTERFACE(attr_idx)        = TRUE;
         AT_DCL_ERR(attr_idx)                   = TRUE;
         parse_err_flush(Find_EOS, NULL);
      }
   }
   else {

      /* Generate an unnamed attr entry for this interface.  It is used */
      /* for collapsing the individual interface bodies at one time.    */

      CREATE_ID(name, "unnamed interface", 17);
      attr_idx = ntr_local_attr_list(name.string,
                                     17,
                                     TOKEN_LINE(token),
                                     TOKEN_COLUMN(token));
      AT_OBJ_CLASS(attr_idx)		= Interface;
      ATI_UNNAMED_INTERFACE(attr_idx)	= TRUE;

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Interface_Explicit_Stmt, stmt_number);
      }
   }

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Interface_Stmt) ||
        STMT_CANT_BE_IN_BLK(Interface_Stmt, CURR_BLK)) &&
        iss_blk_stk_err()) {
      PUSH_BLK_STK(Interface_Blk);
      CURR_BLK_ERR 		= TRUE;
   }
   else {
      PUSH_BLK_STK(Interface_Blk);
      curr_stmt_category	= Sub_Func_Stmt_Cat;
   }

   CURR_BLK_NO_EXEC		= TRUE;

   /* Save the unnamed interface attr in the blk stack, but not in   */
   /* CURR_BLK_NAME.  If it is in CURR_BLK_NAME, there are too many  */
   /* ways the block stack can get messed up.                        */
 
   if (attr_idx != NULL_IDX && ATI_UNNAMED_INTERFACE(attr_idx)) {
      BLK_UNNAMED_INTERFACE(blk_stk_idx) = attr_idx;
      attr_idx = NULL_IDX;
   }
   
   CURR_BLK_NAME = attr_idx;
   NEXT_LA_CH;				/* Pick up EOS */
         
   if (cif_flags & BASIC_RECS) {
      cif_begin_scope_rec();

      if (attr_idx != NULL_IDX) {
         ATI_CIF_SCOPE_ID(attr_idx) = BLK_CIF_SCOPE_ID(blk_stk_idx);
      }
      else if (BLK_UNNAMED_INTERFACE(blk_stk_idx) != NULL_IDX) {
         ATI_CIF_SCOPE_ID(BLK_UNNAMED_INTERFACE(blk_stk_idx)) =
            BLK_CIF_SCOPE_ID(blk_stk_idx);
      }
   }

   TRACE (Func_Exit, "parse_interface_stmt", NULL);

   return;

}  /* parse_interface_stmt */


#ifdef KEY /* Bug 10572 */

/*
 *	BNF is    ENUM, BIND(C)
 */
void
parse_enum_stmt() {
   TRACE (Func_Entry, "parse_enum_stmt", NULL);

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Enum_Stmt) ||
      STMT_CANT_BE_IN_BLK(Enum_Stmt, CURR_BLK)) &&
      iss_blk_stk_err()) {
      PUSH_BLK_STK(Enum_Blk);
      CURR_BLK_ERR = TRUE;
   }
   else {
      PUSH_BLK_STK(Enum_Blk);
   }
   curr_stmt_category = Declaration_Stmt_Cat;

   CURR_BLK_NO_EXEC = TRUE;

   BLK_ENUM_EMPTY(blk_stk_idx) = TRUE;
   BLK_ENUM_COUNTER(blk_stk_idx) = 0;

   if (LA_CH_VALUE != COMMA) {
     parse_err_flush(Find_EOS, ",");
     return;
   }
   NEXT_LA_CH; /* Consume comma */

   if (!matched_specific_token(Tok_Kwd_Bind, Tok_Class_Keyword)) {
     parse_err_flush(Find_EOS, "BIND");
     return;
   }
   parse_language_binding_spec(0);

   if (LA_CH_VALUE != EOS) {
     parse_err_flush(Find_EOS, EOS_STR);
   }
   NEXT_LA_CH;				/* Pick up EOS */
         
   TRACE (Func_Exit, "parse_enum_stmt", NULL);

}  /* parse_enum_stmt */

/*
 *	BNF is    	ENUMERATOR [ :: ] enumerator-list
 *      enumerator is	id [ = scalar-int-initialization-expr ]
 */
void
parse_enumerator_stmt() {
   TRACE (Func_Entry, "parse_enumerator_stmt", NULL);

   PRINTMSG(TOKEN_LINE(token), 1685, Ansi, TOKEN_COLUMN(token), "ENUM");

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Enumerator_Stmt) ||
      STMT_CANT_BE_IN_BLK(Enumerator_Stmt, CURR_BLK)) &&
      iss_blk_stk_err()) {
      PUSH_BLK_STK(Enum_Blk);
      CURR_BLK_ERR = TRUE;
      CURR_BLK_NO_EXEC = TRUE;
   }

   BLK_ENUM_EMPTY(blk_stk_idx) = FALSE;

   /* Optional :: */
   matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct);

   for (;;) {
     if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
	parse_err_flush(Find_Comma_Rparen, "named-constant");
	if (LA_CH_VALUE == EOS) {
	  break;
	}
	continue;
     }
     int line = TOKEN_LINE(token);
     int column = TOKEN_COLUMN(token);
     int name_idx;
     int attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);
     if (attr_idx != NULL_IDX) {
	char *ptr2 = get_basic_type_str(ATD_TYPE_IDX(attr_idx));
	PRINTMSG(line, 550, Error, column, AT_OBJ_NAME_PTR(attr_idx), ptr2,
	  "ENUMERATOR", AT_DEF_LINE(attr_idx));
     }
     attr_idx = ntr_sym_tbl(&token, name_idx);
     LN_DEF_LOC(name_idx) = TRUE;
     ATD_TYPE_IDX(attr_idx) = INTEGER_DEFAULT_TYPE;
     AT_TYPED(attr_idx) = TRUE;
     ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
     boolean have_value = TRUE;

     opnd_type opnd;
     int const_line = line;
     int const_column = column;

     if (LA_CH_VALUE == EQUAL) {
       have_value = FALSE;
       NEXT_LA_CH;		/* Skip = */

       const_line = LA_CH_LINE;
       const_column = LA_CH_COLUMN;

       long kind_idx;
       fld_type field_type;
       if (parse_int_spec_expr(&kind_idx, &field_type, TRUE, FALSE)) {
	  OPND_FLD(opnd)		= field_type;
	  OPND_IDX(opnd)		= kind_idx;

	  BLK_ENUM_COUNTER(blk_stk_idx) =
	    * (long *) &CN_CONST(OPND_IDX(opnd));
       }
       else {
	  /* error from parse_expr */
	  AT_DCL_ERR(attr_idx) = TRUE;
       }
     }
     else {
       /* Use the value in the enum counter */
       long temp = BLK_ENUM_COUNTER(blk_stk_idx);
       OPND_FLD(opnd) = CN_Tbl_Idx;
       OPND_IDX(opnd) = ntr_const_tbl(ATD_TYPE_IDX(attr_idx), FALSE, &temp);
     }
     BLK_ENUM_COUNTER(blk_stk_idx) += 1;
     OPND_LINE_NUM(opnd) = const_line;
     OPND_COL_NUM(opnd)  = const_column;

     expr_arg_type exp_desc;
     exp_desc.rank = 0;
     if ((!AT_DCL_ERR(attr_idx)) && expr_semantics(&opnd, &exp_desc)) {
       merge_parameter(FALSE, attr_idx, line, column, &opnd, &exp_desc,
	 const_line, const_column);
       if ((cif_flags & XREF_RECS) != 0) {
	  cif_usage_rec(attr_idx, AT_Tbl_Idx, line, column,
	    CIF_Symbol_Declaration);
       }
     }

     if (LA_CH_VALUE != COMMA) {
       break;
     }
     NEXT_LA_CH; /* Consume comma */
   }

   if (LA_CH_VALUE != EOS) {
     parse_err_flush(Find_EOS, EOS_STR);
   }

   NEXT_LA_CH;				/* Pick up EOS */
         
   TRACE (Func_Exit, "parse_enumerator_stmt", NULL);

}  /* parse_enum_stmt */
#endif /* KEY Bug 10572 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse the NAMELIST statement.  BNF is:				      *|
|*									      *|
|*        NAMELIST /namelist-group-name/ namelist-group-object-list	      *|
|*                 [[,] /namelist-group-name/ namelist-group-object-list]...  *|
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

void parse_namelist_stmt (void)

{
   int		attr_idx;
   boolean	end_grp_list	=FALSE;
   int          grp_attr;
   int          host_attr_idx;
   int          host_name_idx;
   int		name_idx;
   int		sn_idx;


   TRACE (Func_Entry, "parse_namelist_stmt", NULL);

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Namelist_Stmt) ||
      STMT_CANT_BE_IN_BLK(Namelist_Stmt, CURR_BLK)) && iss_blk_stk_err()) {
      /* Issued block stack error - intentionally left blank */
   }
   else if (curr_stmt_category < Declaration_Stmt_Cat) {
      curr_stmt_category = Declaration_Stmt_Cat;
   }
   else if (curr_stmt_category == Executable_Stmt_Cat) {
      PRINTMSG(stmt_start_line, 265, Ansi, stmt_start_col);
   }

   if (LA_CH_VALUE != SLASH) {
      parse_err_flush (Find_EOS,"/");
   }

   /* Will always have a Slash or an EOS at this point. */
   while (LA_CH_VALUE == SLASH) {
      NEXT_LA_CH;	/* Consume the slash */

      if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
	 parse_err_flush (Find_EOS, "namelist-group-name");
         goto EXIT;
      }

      /* At this point have a namelist group name.  Enter it into the */
      /* symbol table.						      */

      grp_attr = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

      if (grp_attr == NULL_IDX) {
         grp_attr		= ntr_sym_tbl(&token, name_idx);
         LN_DEF_LOC(name_idx)	= TRUE;
         AT_OBJ_CLASS(grp_attr)	= Namelist_Grp;
      }
      else if (!fnd_semantic_err(Obj_Namelist_Grp,
                                 TOKEN_LINE(token),
                                 TOKEN_COLUMN(token),
                                 grp_attr,
                                 TRUE)) {

         if (AT_REFERENCED(grp_attr) == Referenced) {
            PRINTMSG(TOKEN_LINE(token), 39, Error, TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(grp_attr));
         }

         AT_OBJ_CLASS(grp_attr)	= Namelist_Grp;
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      if ((cif_flags & XREF_RECS) != 0) { 
         cif_usage_rec(grp_attr,
                       AT_Tbl_Idx,
                       TOKEN_LINE(token),
                       TOKEN_COLUMN(token),
                       CIF_Symbol_Declaration);
      }

      if (LA_CH_VALUE != SLASH) {
	 parse_err_flush (Find_EOS, "/");
	 goto EXIT;
      }

      /* Have a matching set of slashes, now parse group object list */
      NEXT_LA_CH;		/* Consume slash */

      while (!end_grp_list) {

         if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            parse_err_flush(Find_EOS, "namelist-group-object");
            AT_DCL_ERR(grp_attr) = TRUE;
            goto EXIT;
         }

         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {    /* search host sym tab */
            host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                              TOKEN_LEN(token), 
                                              &host_name_idx,
                                              FALSE);  /* Don't srch INTRINSIC*/
 
            /* Because of forward referencing - NOT_VISIBLE gets checked when */
            /* the rest of the namelist objects semantics are done.           */

            if (host_attr_idx != NULL_IDX) {
               attr_idx = ntr_host_in_sym_tbl(&token, name_idx,
                                              host_attr_idx, host_name_idx, 
                                              TRUE);
            }
            else {
               attr_idx	= ntr_sym_tbl(&token, name_idx);
               SET_IMPL_TYPE(attr_idx);
            }
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
         }


         if ((cif_flags & XREF_RECS) != 0) { 
            cif_usage_rec(attr_idx,
                          AT_Tbl_Idx,
                          TOKEN_LINE(token),
                          TOKEN_COLUMN(token),
                          CIF_Symbol_Declaration);
         }

         AT_NAMELIST_OBJ(attr_idx)	= TRUE;

         NTR_SN_TBL(sn_idx);

         SN_ATTR_IDX(sn_idx)	= attr_idx;
         SN_NAME_LEN(sn_idx)	= AT_NAME_LEN(attr_idx);
         SN_NAME_IDX(sn_idx)	= AT_NAME_IDX(attr_idx);
         SN_LINE_NUM(sn_idx)	= TOKEN_LINE(token);
         SN_COLUMN_NUM(sn_idx)	= TOKEN_COLUMN(token);

         if (ATN_FIRST_NAMELIST_IDX(grp_attr) == NULL_IDX) {
            ATN_FIRST_NAMELIST_IDX(grp_attr) = sn_idx;
         }
         else {
            SN_SIBLING_LINK(ATN_LAST_NAMELIST_IDX(grp_attr)) = sn_idx;
         }

         ATN_LAST_NAMELIST_IDX(grp_attr)	= sn_idx;
         ATN_NUM_NAMELIST(grp_attr)	       += 1;

         if (LA_CH_VALUE != COMMA && 
	     LA_CH_VALUE != SLASH &&
	     LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, "/ or, or " EOS_STR);
            AT_DCL_ERR(grp_attr) = TRUE;
            goto EXIT;
         }

         /* At this point la will be comma, slash or eos. */

         if (LA_CH_VALUE == COMMA) {
	    NEXT_LA_CH;

	    if (LA_CH_VALUE == SLASH) {
	       /* have start of new group */
	       end_grp_list = TRUE;
	    }
         }
         else {
	    end_grp_list = TRUE;
         }
      } /* while */

      end_grp_list = FALSE;
   }  /* end while groups*/

EXIT:

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   NEXT_LA_CH;
   
   TRACE (Func_Exit, "parse_namelist_stmt", NULL);

   return;

}  /* parse_namelist_stmt */

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

void parse_parameter_stmt (void)

{
   int			attr_idx;
   int			column;
   int			const_column;
   int			const_line;
   expr_arg_type        exp_desc;
   boolean		fnd_attr;
   opnd_type		init_opnd;
   int			line;
   int			name_idx;


   TRACE (Func_Entry, "parse_parameter_stmt", NULL);

   /* NOTE:  CFT77 does allow a PARAMETER stmt to preceed an IMPLICIT   */
   /*        NONE stmt but there isn't a way to do it without getting   */
   /*        an error later down the line...the IMPLICIT NONE is        */
   /*        imposed on the PARAMETER stmt and is therefore typeless    */
   /*        and an is generated.                                       */

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Parameter_Stmt) ||
        STMT_CANT_BE_IN_BLK(Parameter_Stmt,CURR_BLK)) && iss_blk_stk_err()) {
      /* Block error - intentionally blank */
   }
   else if (curr_stmt_category <= Implicit_Stmt_Cat) {
      curr_stmt_category = Implicit_Stmt_Cat;
   }
   else {
      curr_stmt_category = Declaration_Stmt_Cat;
   }

   do {
      NEXT_LA_CH;   /* Skip first Lparen, and then skips the commas */

      if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_err_flush(Find_Comma_Rparen, "named-constant");
         continue;
      }

      attr_idx	= srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);
      fnd_attr	= attr_idx;
      line	= TOKEN_LINE(token);
      column	= TOKEN_COLUMN(token);

      if (attr_idx == NULL_IDX) {
         attr_idx		= ntr_sym_tbl(&token, name_idx);
         LN_DEF_LOC(name_idx)	= TRUE;
         SET_IMPL_TYPE(attr_idx);
      }
      else if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
         AT_ATTR_LINK(attr_idx)	= NULL_IDX;
         LN_DEF_LOC(name_idx)	= TRUE;
      }

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
      }

      if (LA_CH_VALUE != EQUAL) {
          parse_err_flush(Find_Comma_Rparen, "=");
          continue;
      }

      NEXT_LA_CH;		/* Skip = */
      const_line	= LA_CH_LINE;
      const_column	= LA_CH_COLUMN;

      if (parse_expr(&init_opnd)) {
         exp_desc.rank	= 0;
         expr_mode	= Initialization_Expr;
         xref_state     = CIF_Symbol_Reference;

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
             TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == Unknown_Char) {

            char_bounds_resolution(attr_idx,
                                   &fnd_attr);
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_ARRAY_IDX(attr_idx)) {
            array_bounds_resolution(attr_idx, &fnd_attr);
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

            if (ATD_ARRAY_IDX(attr_idx)) {
               target_array_idx = ATD_ARRAY_IDX(attr_idx);
            }

            switch (TYP_TYPE(ATD_TYPE_IDX(attr_idx))) {
            case Integer:
            case Real:
            case Complex:
               check_type_conversion = TRUE;
               target_type_idx       = ATD_TYPE_IDX(attr_idx);
               break;

            case Character:

               if (TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == Const_Len_Char) {
                  check_type_conversion	= TRUE;
                  target_type_idx = Character_1;
                  target_char_len_idx = TYP_IDX(ATD_TYPE_IDX(attr_idx));
               }
               break;
            }
         }

         /* set comp_gen_expr to TRUE. This forces the fold of REAL   */
         /* constant expressions. When -Oieeeconform is specified,    */
         /* the folding of Real and Complex expressions is prevented. */

         comp_gen_expr = TRUE;

         if (expr_semantics(&init_opnd, &exp_desc)) {
            check_type_conversion = FALSE;
            target_array_idx      = NULL_IDX;
            expr_mode	          = Regular_Expr;
            merge_parameter(fnd_attr,
                            attr_idx,
                            line,
                            column,
                            &init_opnd,
			    &exp_desc,
                            const_line,
                            const_column);

            if ((cif_flags & XREF_RECS) != 0) {
               cif_usage_rec(attr_idx,
                             AT_Tbl_Idx,
                             line,
                             column,
                             CIF_Symbol_Declaration);
            }
         }
         else {
            check_type_conversion = FALSE;
            target_array_idx      = NULL_IDX;
            expr_mode	          = Regular_Expr;
            AT_DCL_ERR(attr_idx) = TRUE;
         }

         /* reset comp_gen_expr to FALSE. end of compiler generated expr */
         comp_gen_expr = FALSE;
      }
      else {
         /* error from parse_expr */
         AT_DCL_ERR(attr_idx) = TRUE;
      }

      if (LA_CH_VALUE != COMMA && LA_CH_VALUE != RPAREN) {
         parse_err_flush(Find_Comma_Rparen, ", or )");
      }
   }
   while (LA_CH_VALUE == COMMA);

   if (LA_CH_VALUE == RPAREN) {
      NEXT_LA_CH;
   }

EXIT:

   NEXT_LA_CH;       /* Pick up EOS */
   
   TRACE (Func_Exit, "parse_parameter_stmt", NULL);

   return;

}  /* parse_parameter_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BNF   SEQUENCE                              			      *|
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

void parse_sequence_stmt (void)

{
   TRACE (Func_Entry, "parse_sequence_stmt", NULL);

   if (CURR_BLK == Derived_Type_Blk) {

      if (LA_CH_VALUE == EOS) {

         if (ATT_SEQUENCE_SET(CURR_BLK_NAME)) {
            PRINTMSG (TOKEN_LINE(token), 41, Error,
                      TOKEN_COLUMN(token), "SEQUENCE",
                      AT_OBJ_NAME_PTR(CURR_BLK_NAME));
         }

         if (ATT_FIRST_CPNT_IDX(CURR_BLK_NAME) != NULL_IDX) {
	    PRINTMSG(TOKEN_LINE(token), 8, Error, TOKEN_COLUMN(token),
		     "SEQUENCE", AT_OBJ_NAME_PTR(CURR_BLK_NAME));
         }

         ATT_SEQUENCE_SET(CURR_BLK_NAME) = TRUE;
      }
      else {
         parse_err_flush(Find_EOS, EOS_STR);
      }
   }
   else {
      parse_err_flush(Find_EOS, NULL);
      iss_blk_stk_err();	/* Not assignment statement */
   }

   NEXT_LA_CH;			/* Skip EOS */

   TRACE (Func_Exit, "parse_sequence_stmt", NULL);

   return;

}  /* parse_sequence_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This parses a statement function.  Unlike other stmt parsers, this    *|
|*	routine is called from parse_assignment_stmt, not from the stmt table.*|
|*	At entry the name of the statement function has been entered into the.*|
|*	attr table.                                                           *|
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

void parse_stmt_func_stmt(int	sf_attr_idx,
			  int	sf_name_idx)

{
   int		attr_idx;
   int		count;
   int		first_idx;
   boolean	found_end		= FALSE;
   int		i;
   int		name_idx;
   int		new_attr_idx;
   opnd_type	opnd;
   int		sn_idx;
   int		sn_attr_idx;
   int		stmt_number;


   TRACE (Func_Entry, "parse_stmt_func_stmt", NULL);

   stmt_type = Stmt_Func_Stmt;
   stmt_number = statement_number;

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Stmt_Func_Stmt) ||
        STMT_CANT_BE_IN_BLK(Stmt_Func_Stmt, CURR_BLK)) && iss_blk_stk_err()) {
      /* Issued block error - intentionally left blank */
   }
   else {
      curr_stmt_category = Declaration_Stmt_Cat;
   }

   if (!fnd_semantic_err(Obj_Stmt_Func, 
                         TOKEN_LINE(token),
                         TOKEN_COLUMN(token),
                         sf_attr_idx,
                         TRUE)) {

      if (AT_REFERENCED(sf_attr_idx) == Char_Rslt_Bound_Ref) {
         AT_ATTR_LINK(sf_attr_idx)   = NULL_IDX;
         LN_DEF_LOC(sf_name_idx)     = TRUE;
      }

      /* MUST be a data object - has been implicitly typed already */

      AT_OBJ_CLASS(sf_attr_idx)	= Stmt_Func;
      LN_DEF_LOC(sf_name_idx)	= TRUE;
   }
   else {
      CREATE_ERR_ATTR(sf_attr_idx, 
                      TOKEN_LINE(token),
                      TOKEN_COLUMN(token),
                      Stmt_Func);
   }

   if ((cif_flags & XREF_RECS) != 0) {
      cif_usage_rec(sf_attr_idx,
                    AT_Tbl_Idx,
                    TOKEN_LINE(token),
                    TOKEN_COLUMN(token),
                    CIF_Symbol_Declaration);
   }

   NEXT_LA_CH;   /* Must be Lparen to be here - Consume Lparen */

   if (LA_CH_VALUE == RPAREN) {
      goto DONE;
   }

   do {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj;
            ATD_CLASS(attr_idx)		= Dummy_Argument;
            SET_IMPL_TYPE(attr_idx);
            AT_IS_DARG(attr_idx)	= TRUE;
            ATD_SF_DARG(attr_idx)	= TRUE;
         }
         else {

            if (fnd_semantic_err(Obj_Sf_Darg, 
                                 TOKEN_LINE(token),
                                 TOKEN_COLUMN(token),
                                 attr_idx,
                                 TRUE)) {

               AT_DCL_ERR(sf_attr_idx) = TRUE;
            }

            NTR_ATTR_TBL(new_attr_idx);
            COPY_COMMON_ATTR_INFO(attr_idx, new_attr_idx, Data_Obj);
            AT_OBJ_CLASS(new_attr_idx)		= Data_Obj;
            ATD_CLASS(new_attr_idx)		= Dummy_Argument;
            AT_IS_DARG(new_attr_idx)		= TRUE;
            AT_IS_INTRIN(new_attr_idx)		= FALSE;
            AT_ELEMENTAL_INTRIN(new_attr_idx)	= FALSE;
            ATD_SF_DARG(new_attr_idx)		= TRUE;

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
                AT_TYPED(new_attr_idx)		= AT_TYPED(attr_idx);
                ATD_TYPE_IDX(new_attr_idx)	= ATD_TYPE_IDX(attr_idx);
            }
            else {
               SET_IMPL_TYPE(new_attr_idx);
            }
            ATD_SF_LINK(new_attr_idx)		= attr_idx;
            LN_ATTR_IDX(name_idx)		= new_attr_idx;
            attr_idx				= new_attr_idx;
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
         }


         if ((cif_flags & XREF_RECS) != 0) {
            cif_usage_rec(attr_idx,
                          AT_Tbl_Idx,
                          TOKEN_LINE(token),
                          TOKEN_COLUMN(token),
                          CIF_Symbol_Is_Dummy_Arg);
         }

	 /* Enter into secondary name table */

         sn_attr_idx = srch_kwd_name(TOKEN_STR(token),
                                     TOKEN_LEN(token),
                                     sf_attr_idx,
                                     &sn_idx);

	 if (sn_attr_idx != NULL_IDX) { /* Have duplicate dummy arg */
	    PRINTMSG(TOKEN_LINE(token), 10, Error, TOKEN_COLUMN(token),
                     TOKEN_STR(token));
            AT_DCL_ERR(sf_attr_idx) = TRUE;
	 }
         else { 
            NTR_SN_TBL(sn_idx);
            SN_ATTR_IDX(sn_idx)		= attr_idx;
            SN_NAME_LEN(sn_idx)		= AT_NAME_LEN(attr_idx);
            SN_NAME_IDX(sn_idx)		= AT_NAME_IDX(attr_idx);
            SN_LINE_NUM(sn_idx)		= TOKEN_LINE(token);
            SN_COLUMN_NUM(sn_idx)	= TOKEN_COLUMN(token);

            if (ATP_FIRST_IDX(sf_attr_idx) == NULL_IDX) {
               ATP_FIRST_IDX(sf_attr_idx) = sn_idx;
            }
            ATP_NUM_DARGS(sf_attr_idx) += 1;
         }
      }
      else  {

         AT_DCL_ERR(sf_attr_idx) = TRUE;

         if (!parse_err_flush(Find_Comma_Rparen, "dummy-arg-name")) {
            goto EXIT;
         }
      }

      if (LA_CH_VALUE != RPAREN && LA_CH_VALUE != COMMA) {

         AT_DCL_ERR(sf_attr_idx) = TRUE;

         if (!parse_err_flush(Find_Comma_Rparen, ", or )")) {
            goto EXIT;
         }
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         found_end = TRUE;
      }

   }  /* end do while */
   while (!found_end);

DONE:

   NEXT_LA_CH;	/* Consume RPAREN */

   if (matched_specific_token(Tok_Punct_Eq, Tok_Class_Punct)) {
      expr_mode			= Stmt_Func_Expr;

      if (parse_expr(&opnd)) {
         ATS_SF_FLD(sf_attr_idx) 	= OPND_FLD(opnd);
         ATS_SF_IDX(sf_attr_idx) 	= OPND_IDX(opnd);
      }
      else {
         AT_DCL_ERR(sf_attr_idx) = TRUE;
      }

      expr_mode			= Regular_Expr;

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Statement_Function_Stmt, stmt_number);
      }
   }
   else {
      AT_DCL_ERR(sf_attr_idx) = TRUE;
      parse_err_flush(Find_EOS, "=");
   }

   first_idx = ATP_FIRST_IDX(sf_attr_idx);
   count     = ATP_NUM_DARGS(sf_attr_idx);

   /* Remove the dargs from the local name table */

   for (i = first_idx; i < (first_idx + count); i++) {
      attr_idx = SN_ATTR_IDX(i);
      srch_sym_tbl(AT_OBJ_NAME_PTR(attr_idx), AT_NAME_LEN(attr_idx), &name_idx);

      if (ATD_SF_LINK(attr_idx) != NULL_IDX) {
         LN_ATTR_IDX(name_idx) = ATD_SF_LINK(attr_idx);
      }
      else {
         remove_ln_ntry(name_idx);
      }
   }

   if (LA_CH_VALUE != EOS) {
      AT_DCL_ERR(sf_attr_idx) = TRUE;
      parse_err_flush(Find_EOS, EOS_STR);
   }

EXIT:
#ifdef KEY /* Bug 318 */
   PRINTMSG(stmt_start_line, 1682, Ansi, stmt_start_col);
#endif /* KEY Bug 318 */

   TRACE (Func_Exit, "parse_stmt_func_stmt", NULL);

   return;

}  /* parse_stmt_func_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    Parses the type declaration statement                                   *|
|*									      *|
|*      BNF   type_spec[[,attr_spec]...::]entity-decl-list                    *|
|*              type-spec is  INTEGER [kind-selector]                         *|
|*                            REAL [kind-selector]                            *|
|*                            DOUBLE PRECISION                                *|
|*                            COMPLEX [kind-selector]                         *|
|*                            CHARACTER [char-selector]                       *|
|*                            LOGICAL [kind-selector]                         *|
|*                            TYPE (type-name)                                *|
|*                            BYTE                                            *|
|*            entity_dcl_list is                                              *|
|*              object-name[(array-spec)][*char-length][=initialization-expr] *|
|*            attr_spec is    done in parse_attr_spec                         *|
|*                                                                            *|
\******************************************************************************/

void parse_type_dcl_stmt (void)

{
   int			array_idx;
   int			attr_idx;
   long			attr_list		= 0;
   int			buf_idx;
   boolean		check_char_comma;
   boolean		GT_encountered		= FALSE;
   boolean		chk_semantics;
   expr_arg_type	exp_desc;
   boolean		found_colon;
   boolean		found_end;
   boolean		has_parameter		= FALSE;
   int			id_column;
   int			id_line;
   int			il_idx;
   int			init_ir_idx;
   opnd_type		init_opnd;
   int			name_idx;
   boolean		need_new_array;
   int			new_array_idx;
   int			new_pe_array_idx	= NULL_IDX;
   boolean		new_attr;
   int			old_array_idx;
   int			pe_array_idx		= NULL_IDX;
   boolean		possible_func;
   int			save_column;
   int			save_line;
   int			stmt_number;
   int			stmt_num;
   boolean		type_err;
   int			type_idx;
   int			usage_code;


   TRACE (Func_Entry, "parse_type_dcl_stmt", NULL);

   colon_recovery = TRUE;		/* Can recover to :: */
   stmt_number = statement_number;

   if (TOKEN_VALUE(token) == Tok_Kwd_Type  &&  LA_CH_VALUE != LPAREN) {

      if (LA_CH_VALUE == EOS) {

         /* Expecting either a TYPE statement or a derived type statement. */

         parse_err_flush(Find_EOS, "( or , or :: or type-name");
         NEXT_LA_CH;    /* Skip EOS */
         goto EXIT;
      }

      /* Allows for nested  derived types.  The block stack will allow for    */
      /* this.  Context/block checking catches and issues the error.          */

      parse_derived_type_stmt();

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Type_Stmt, stmt_number);
      }

      goto EXIT;
   }

   if (CURR_BLK == Derived_Type_Blk) {
      stmt_type	= Cpnt_Decl_Stmt;
      parse_cpnt_dcl_stmt();
      goto EXIT;
   }

   if (curr_stmt_category == Sub_Func_Stmt_Cat) {

      /* In contains or Interface block - must be function statement.         */
      /* DO NOT end curr_stmt_category == Init_Stmt_Cat thru here, because it */
      /* may not be a FUNCTION statement.  The following is a legal type dcl  */
      /* stmt:   INTEGER FUNCTION A(10) in fixed format.                      */

      CLEAR_ATTR_NTRY(AT_WORK_IDX);		/* Used for AT_TYPED */
      parse_typed_function_stmt();
      goto EXIT;
   }

   check_char_comma		= (TOKEN_VALUE(token) == Tok_Kwd_Character &&
                                   LA_CH_VALUE == STAR);
   found_colon			= FALSE;
   found_end			= FALSE;
   type_err			= !parse_type_spec(TRUE);
   AT_DCL_ERR(AT_WORK_IDX)	= type_err;
   type_idx			= ATD_TYPE_IDX(AT_WORK_IDX);
   array_idx			= NULL_IDX;


   if (LA_CH_VALUE == COMMA && (!check_char_comma || stmt_has_double_colon())) {

      if ((STMT_OUT_OF_ORDER(curr_stmt_category, Type_Decl_Stmt) ||
           STMT_CANT_BE_IN_BLK(Type_Decl_Stmt, CURR_BLK)) && iss_blk_stk_err()){
         /* Block error - intentionally left blank */
      }
      else {
         curr_stmt_category = Declaration_Stmt_Cat;
      }

      /* Check that type is defined before it is used. */

      if (TYP_TYPE(type_idx) == Structure &&
          !AT_DEFINED(TYP_IDX(type_idx)) && !AT_DCL_ERR(TYP_IDX(type_idx))) {
         issue_undefined_type_msg(TYP_IDX(type_idx), 
                                  TOKEN_LINE(token),
                                  TOKEN_COLUMN(token));
      }

      /* Attr_list contains a bit vector of which attrs are specified.  */
      /* array_idx contains the index of the array spec, if DIMESION is */
      /* specified.  AT_WORK_IDX does not get updated with it, because  */
      /* it has to be updated later in case the variable is followed    */
      /* by its own dimension.  ie:  REAL,DIMENSION(5),POINTER :: B(:)  */
      /* is legal.  Dimension cannot be merged until B is processed.    */

      new_intent	= Intent_Unseen;
      attr_list		= parse_attr_spec(&array_idx, &has_parameter);

# ifdef _F_MINUS_MINUS
      if (AT_OBJ_CLASS(AT_WORK_IDX) == Data_Obj) {
         pe_array_idx  = ATD_PE_ARRAY_IDX(AT_WORK_IDX);
      }
# endif
      found_colon	= TRUE;
      colon_recovery	= FALSE;	/* Past ::    */
   }
   else {          /* Not followed by a COMMA  or CHARACTER*8,             */
      colon_recovery	= FALSE;  /* No error recovery attempted before :: */

      if (curr_stmt_category == Init_Stmt_Cat) {

         /* Check to see if this is a FUNCTION statement.  Have to go as   */
         /* far as the dummy arg list, because the following is a legal    */
         /* type dcl statement:  INTEGER FUNCTION A(10)                    */

         save_line		= LA_CH_LINE;
         save_column		= LA_CH_COLUMN;
         buf_idx		= LA_CH_BUF_IDX;
         stmt_num		= LA_CH_STMT_NUM;
         possible_func		= TRUE;

         while (MATCHED_TOKEN_CLASS(Tok_Class_Keyword) &&  possible_func) {

            switch(TOKEN_VALUE(token)) {
            case Tok_Kwd_Recursive:
            case Tok_Kwd_Elemental:
            case Tok_Kwd_Pure:
               break;

            case Tok_Kwd_Function:

#ifdef KEY /* Bug 8261 */
               if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
		 /* Allow optional "*ddd" or "*(*)" extension after function
		  * name */
		 if (!on_off_flags.issue_ansi_messages && LA_CH_VALUE == STAR) {
		   possible_func = FALSE;
		   NEXT_LA_CH;
		   if (LA_CH_VALUE == LPAREN) {
		     NEXT_LA_CH;
		     if (LA_CH_VALUE == STAR) {
		       NEXT_LA_CH;
		       if (LA_CH_VALUE == RPAREN) {
		         NEXT_LA_CH;
			 possible_func = TRUE;
		       }
		     }
		   }
		   else if (MATCHED_TOKEN_CLASS(Tok_Class_Int_Spec)) {
		     possible_func = TRUE;
		   }
		   if (!possible_func) {
		     break;
		   }
		 }
	         if (LA_CH_VALUE == LPAREN) {
		  NEXT_LA_CH;

                  if (LA_CH_VALUE == RPAREN || LA_CH_CLASS == Ch_Class_Letter) {

                     /* TRUE = type-spec is parsed - type is in AT_WORK_IDX */
                     /* Reset to pick up recursive, pure and elemental and  */
                     /* the function name.  This will isolated semantics.   */

                     reset_lex(buf_idx, stmt_num);
                     AT_DCL_ERR(AT_WORK_IDX) = SH_ERR_FLG(curr_stmt_sh_idx);
                     parse_typed_function_stmt();
                     goto EXIT;
		  }
		 }
               }
#else /* KEY Bug 8261 */
               if (MATCHED_TOKEN_CLASS(Tok_Class_Id) && LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;

                  if (LA_CH_VALUE == RPAREN || LA_CH_CLASS == Ch_Class_Letter) {

                     /* TRUE = type-spec is parsed - type is in AT_WORK_IDX */
                     /* Reset to pick up recursive, pure and elemental and  */
                     /* the function name.  This will isolated semantics.   */

                     reset_lex(buf_idx, stmt_num);
                     AT_DCL_ERR(AT_WORK_IDX) = SH_ERR_FLG(curr_stmt_sh_idx);
                     parse_typed_function_stmt();
                     goto EXIT;
                  }
               }
#endif /* KEY Bug 8261 */
               possible_func	= FALSE;
               break;

            default:   /* Tok_Kwd_Id */
               possible_func	= FALSE;
               break;
            }
         }

         /* Actually had a match and need to reset and clear attr */
         /* INTEGER FUNCTION A(10) in fixed form would get here.  */

         if (LA_CH_LINE != save_line || LA_CH_COLUMN != save_column) {
            reset_lex(buf_idx, stmt_num);
         }
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }

      found_colon = matched_specific_token(Tok_Punct_Colon_Colon,
                                           Tok_Class_Punct);

      if ((STMT_OUT_OF_ORDER(curr_stmt_category, Type_Decl_Stmt) ||
           STMT_CANT_BE_IN_BLK(Type_Decl_Stmt, CURR_BLK)) && iss_blk_stk_err()){
            /* Block error - intentionally left blank */
      }
      else {
         curr_stmt_category = Declaration_Stmt_Cat;
      }

      if (TYP_TYPE(type_idx) == Structure && !AT_DEFINED(TYP_IDX(type_idx)) && 
          !AT_DCL_ERR(TYP_IDX(type_idx))) {
         issue_undefined_type_msg(TYP_IDX(type_idx), 
                                  AT_DEF_LINE(TYP_IDX(type_idx)),
                                  AT_DEF_COLUMN(TYP_IDX(type_idx)));
      }
   }

   AT_DCL_ERR(AT_WORK_IDX) = SH_ERR_FLG(curr_stmt_sh_idx);

#ifdef KEY /* Bug 14150 */
   int count_entities = 0;
#endif /* KEY Bug 14150 */
   do {
      if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         found_end = !parse_err_flush(Find_Comma, "object-name");
         NEXT_LA_CH;
         continue;
      }

      type_idx		= ATD_TYPE_IDX(AT_WORK_IDX);
      attr_idx		= srch_sym_tbl(TOKEN_STR(token),
                       		       TOKEN_LEN(token), &name_idx);
      id_line		= TOKEN_LINE(token);
      id_column		= TOKEN_COLUMN(token);
      new_attr		= FALSE;
      new_array_idx	= array_idx;
      new_pe_array_idx	= pe_array_idx;

      /* If the type is assumed size character, we cannot share array bounds */
      /* because each object may assume a different size upon entry.         */

      need_new_array	= (TYP_TYPE(type_idx) == Character && 
                           TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char);

      if (attr_idx == NULL_IDX) {
         attr_idx			= ntr_sym_tbl(&token, name_idx);
         LN_DEF_LOC(name_idx)		= TRUE;
         new_attr			= TRUE;
         AT_NAME_LEN(AT_WORK_IDX)	= AT_NAME_LEN(attr_idx);
         AT_NAME_IDX(AT_WORK_IDX)	= AT_NAME_IDX(attr_idx);
         AT_DEF_LINE(AT_WORK_IDX)	= AT_DEF_LINE(attr_idx);
         AT_DEF_COLUMN(AT_WORK_IDX)	= AT_DEF_COLUMN(attr_idx);
         COPY_ATTR_NTRY(attr_idx, AT_WORK_IDX);
#ifdef KEY
         if (AT_OBJ_CLASS(attr_idx) == Data_Obj && !AT_IS_INTRIN(attr_idx) &&
             TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == Real_4 &&
             Check_FF2C_Script(AT_OBJ_NAME_PTR(attr_idx), 0) )
         {
           ATD_TYPE_IDX(attr_idx) = Real_8; 
           ATD_F2C_ABI_VAR(attr_idx) = TRUE;
         }
#endif
         AT_CIF_SYMBOL_ID(attr_idx)	= 0;

         if (type_err) {
            SET_IMPL_TYPE(attr_idx);
         }
      }
      else if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
         AT_ATTR_LINK(attr_idx)		= NULL_IDX;
         LN_DEF_LOC(name_idx)		= TRUE;
      }

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
      }

      /* Have to merge the intrinsic now, because the INTRINSIC scope has to */
      /* be searched and the INTRINSIC attr copied down.  Then all the other */
      /* things declared on this line, are added to this the attr.  Check    */
      /* semantics if this isn't a new attr.  If this is an INTRINSIC subrtn */
      /* the error will be issued from merge_intrinsic whether other         */
      /* semantics checking is done or not.                                  */

      if (attr_list & (1 << Intrinsic_Attr)) {
         merge_intrinsic(!new_attr, id_line, id_column, attr_idx);
      }

      /* Always have to merge_external, because it has to be switched to a   */
      /* program unit.  Do semantic checking if this isn't a new attr.       */

      if (attr_list & (1 << External_Attr)) {
         merge_external(!new_attr, id_line, id_column, attr_idx);
      }

      /* Merge in binding label for each entity, because in the general case
       * it will omit "name=x" and depend on the name of the entity  */
      if (attr_list & (1 << Bind_Attr)) {
	merge_bind(TRUE, id_line, id_column, attr_idx);
	count_entities += 1;
	/* Nonempty name= precludes multiple entities in one statement */
	if ((BIND_SPECIFIES_NAME(new_binding_label)) && count_entities == 2) {
	  PRINTMSG(id_line, 1689, Error, id_column);
	}
      }

#ifdef KEY /* Bug 8260 */
      boolean have_seen_bounds = FALSE;
#endif /* KEY Bug 8260 */

      if (LA_CH_VALUE == LPAREN) {

         /* If LA_CH is left paren, then a dimension spec is specified on    */
         /* the variable name.  This overrides the specification on the      */
         /* dimension attribute.                                             */

         new_array_idx	= parse_array_spec(attr_idx);
         need_new_array	= FALSE;
#ifdef KEY /* Bug 8260 */
	 have_seen_bounds = TRUE;
#endif /* KEY Bug 8260 */
      }

# ifdef _F_MINUS_MINUS

      if (LA_CH_VALUE == LBRKT && cmd_line_flags.co_array_fortran) {
         new_pe_array_idx  = parse_pe_array_spec(attr_idx);
      }
# endif

      if (LA_CH_VALUE == STAR) { /* Pick up char len.  LEN = not allowed here */

         /* We are not parsing the character* part of the line, so this    */
         /* is not the length_selector.  It is the char-length on the name */

#ifdef KEY /* Bug 8422 */
	 /* Handle new extension like "integer i*2" (vs old extension
	  * "integer*2 i" or standard "character*80 c, d*40".) If -ansi,
	  * skip this and let character-specific code below issue an error
	  * message */
	 if (Character != TYP_TYPE(type_idx) &&
	    !on_off_flags.issue_ansi_messages) {
	    NEXT_LA_CH;
	    type_idx = parse_non_char_kind_selector(FALSE);
	    need_new_array = FALSE;
            if (new_attr) {
               switch (AT_OBJ_CLASS(attr_idx)) {
               case Data_Obj:
               case Interface:
                  ATD_TYPE_IDX(attr_idx)		= type_idx;
                  break;

               case Pgm_Unit:
                  ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx))	= type_idx;
                  break;
               }
            }
	 }
	 else
#endif /* KEY Bug 8422 */
	   parse_length_selector(attr_idx, FALSE, FALSE);

#ifdef KEY /* Bug 8260 */
	 /* Extension: array bounds follow the "*" instead of preceding it. */
	 if (LA_CH_VALUE == LPAREN &&
	    !(have_seen_bounds || on_off_flags.issue_ansi_messages)) { 
	    new_array_idx	= parse_array_spec(attr_idx);
	    need_new_array	= FALSE;
	    have_seen_bounds = TRUE;
	 }
#endif /* KEY Bug 8260 */

         if (TYP_TYPE(type_idx) == Character) {
            TYP_DESC(TYP_WORK_IDX)	= TYP_DESC(type_idx);
            TYP_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(type_idx);
            type_idx			= ntr_type_tbl();

            if (TYP_CHAR_CLASS(type_idx) != Assumed_Size_Char)
	    {
               need_new_array = FALSE;
            }

            if (new_attr) {
               switch (AT_OBJ_CLASS(attr_idx)) {
               case Data_Obj:
               case Interface:
                  ATD_TYPE_IDX(attr_idx)		= type_idx;
                  break;

               case Pgm_Unit:
                  ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx))	= type_idx;
                  break;
               }
            }
         }
#ifdef KEY /* Bug 8422 */
         else if (!on_off_flags.issue_ansi_messages) { }
#endif /* KEY Bug 8422 */
         else {		/* This must be a CHARACTER stmt to have * length.  */
            PRINTMSG(TOKEN_LINE(token), 192, Error, TOKEN_COLUMN(token));
            AT_DCL_ERR(attr_idx) = TRUE;
         }

         /* Have a different character length than the one specified on the   */
         /* CHARACTER component statement.  (ie:  CHARACTER*(2) :: A*(10),B)  */
         /* If this is an array, it may need a seperate bounds table entry if */
         /* this is a shared array entry.  The stride multiplier is kept in   */
         /* the bounds table and is dependent on type.  Therefore, if two     */
         /* items have seperate types, they must have seperate bounds entries.*/
         /* Ex:  CHARACTER*(2), DIMENSION(100) :: A*(10), B   ! A and B need  */
         /*                     seperate bounds entries.                      */
         /*      CHARACTER*(2), DIMENSION(100) :: A(20)*(10), B  ! They       */
         /*                     already have seperate bounds entries, because */
         /*                     they have seperate dimensions.                */
         /*      CHARACTER*(2), DIMENSION(100) :: A,B  ! They have the same   */
         /*                     type, so they can share a bound entry.        */

         if (new_array_idx != NULL_IDX && new_array_idx == array_idx &&
             BD_ARRAY_CLASS(new_array_idx) != Deferred_Shape) {
            old_array_idx = new_array_idx;
            new_array_idx = reserve_array_ntry(BD_RANK(old_array_idx));
            COPY_BD_NTRY(new_array_idx, old_array_idx);
            new_array_idx = ntr_array_in_bd_tbl(new_array_idx);
         }
      }

      /* Always have to merge in the type if it is character, because the     */
      /* length may have referenced, the thing being declared.                */

      if (!new_attr || TYP_TYPE(type_idx) == Character) {

         if (new_attr) {

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj ||
                AT_OBJ_CLASS(attr_idx) == Interface) {
               AT_TYPED(attr_idx) = FALSE;
            }
            else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
               AT_TYPED(ATP_RSLT_IDX(attr_idx)) = FALSE;
            }
         }

         merge_type(attr_idx,
                    type_idx,
                    id_line,
                    id_column);
      }


      /* Always have to merge in dimension, even if it's a new attribute,  */
      /* because the dimension may not semantically agree with the type.   */

      if (new_array_idx != NULL_IDX) {

         if (need_new_array && BD_ARRAY_CLASS(array_idx) != Deferred_Shape) {

            /* This cannot share a bounds entry, because the type is *(*),  */
            /* which means that at execution time, each object may have a   */
            /* different type, so create a new bd idx to be used.           */
            /* Deferred shape array entries are allowed to share.           */

            new_array_idx = reserve_array_ntry(BD_RANK(array_idx));
            COPY_BD_NTRY(new_array_idx, array_idx);
            new_array_idx = ntr_array_in_bd_tbl(new_array_idx);
         }

         merge_dimension(attr_idx, id_line, id_column, new_array_idx);
      }

      if (attr_list && !new_attr) {

         if (attr_list & (1 << Allocatable_Attr)) {
            merge_allocatable(TRUE, id_line, id_column, attr_idx);
         }

         if (attr_list & (1 << Automatic_Attr)) {
            merge_automatic(TRUE, id_line, id_column, attr_idx);
         }
         if (attr_list & (1 << Value_Attr)) {
            merge_value(TRUE, id_line, id_column, attr_idx);
         }

         if (attr_list & (1 << Public_Attr)) {
            merge_access(attr_idx, id_line, id_column, Public);
         }
         else if (attr_list & (1 << Private_Attr)) {
            merge_access(attr_idx, id_line, id_column, Private);
         }

         if (attr_list & (1 << Optional_Attr)) {
            merge_optional(TRUE, id_line, id_column, attr_idx);
         }

         if (attr_list & (1 << Pointer_Attr)) {
            merge_pointer(TRUE, id_line, id_column, attr_idx);
         }

         if (attr_list & (1 << Save_Attr)) {
            merge_save(TRUE, id_line, id_column, attr_idx);
         }

         if (attr_list & (1 << Target_Attr)) {
            merge_target(TRUE, id_line, id_column, attr_idx);
         }

         if (attr_list & (1 << Volatile_Attr)) {
            merge_volatile(TRUE, id_line, id_column, attr_idx);
         }

         if (attr_list & (1 << Intent_Attr)) {
            merge_intent(TRUE, id_line, id_column, attr_idx);
         }
      } 

      if ((new_pe_array_idx != NULL_IDX) &&
          (!new_attr || (!(attr_list & (1 << Co_Array_Attr))))) {
         merge_co_array(TRUE, id_line, id_column, attr_idx,new_pe_array_idx);
      }

      usage_code = CIF_Symbol_Declaration;

      if (LA_CH_VALUE == SLASH) {
         PRINTMSG(LA_CH_LINE, 1662, Ansi, LA_CH_COLUMN);

         if (has_parameter) {
            PRINTMSG(LA_CH_LINE, 1663, Error, LA_CH_COLUMN);
         }
         NEXT_LA_CH;

         if (merge_data(TRUE, id_line, id_column, attr_idx)) {

            if (SH_STMT_TYPE(curr_stmt_sh_idx) == Type_Decl_Stmt) {
               SH_STMT_TYPE(curr_stmt_sh_idx)    = Data_Stmt;
               SH_COMPILER_GEN(curr_stmt_sh_idx) = TRUE;
               SH_GLB_LINE(curr_stmt_sh_idx)     = id_line;
               SH_COL_NUM(curr_stmt_sh_idx)      = id_column;
            }
            else {
               gen_sh(After, Data_Stmt, id_line, id_column, 
                      FALSE, FALSE, TRUE);
            }

            stmt_type = Data_Stmt;

            NTR_IR_TBL(init_ir_idx);
            SH_IR_IDX(curr_stmt_sh_idx) = init_ir_idx;

            IR_OPR(init_ir_idx) = Init_Opr;

            IR_TYPE_IDX(init_ir_idx)    = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(init_ir_idx)    = id_line;
            IR_COL_NUM(init_ir_idx)     = id_column;
            NTR_IR_LIST_TBL(il_idx);
            IR_FLD_L(init_ir_idx)       = IL_Tbl_Idx;
            IR_IDX_L(init_ir_idx)       = il_idx;
            IR_LIST_CNT_L(init_ir_idx)	= 1;
            IL_FLD(il_idx)		= AT_Tbl_Idx;
            IL_IDX(il_idx)		= attr_idx;
            IL_LINE_NUM(il_idx)		= id_line;
            IL_COL_NUM(il_idx)		= id_column;

            parse_initializer(init_ir_idx);

            /* The item is being initialized, so flag this by adding  */
            /* 200 to the CIF "modification" value.		      */

            usage_code = CIF_Symbol_Modification + 200;
         }
      }
      else if (LA_CH_VALUE == EQUAL) {
         NEXT_LA_CH;
         save_line	= LA_CH_LINE;
         save_column	= LA_CH_COLUMN;

         if (LA_CH_VALUE == GT) {
            NEXT_LA_CH;
            save_line	= LA_CH_LINE;
            save_column	= LA_CH_COLUMN;
            GT_encountered = TRUE;
         }
  
         if (!found_colon) {
            PRINTMSG(save_line, 121, Error, save_column);
            AT_DCL_ERR(attr_idx) = TRUE;
         }

         /* (Re)set stmt_type to Type_Decl_Stmt in case this is the second or */
         /* later initialization for this stmt.  On the first pass through    */
         /* here, stmt_type is set to (CG) Data_Stmt so that the SH won't be  */
         /* thrown away.  stmt_type needs to be Type_Decl_Stmt at this point  */
         /* so parse_expr will issue an Ansi message if the initialization    */
         /* value is a BOZ constant.					      */
       
         stmt_type = Type_Decl_Stmt;

         if (parse_expr(&init_opnd)) {

            if (has_parameter) {

               /* Only check semantics if this is not a new attribute         */
               /* ATD_CLASS does not get set to Constant until here.  If      */
               /* this is a new attribute,  merge_dimension actually gets     */
               /* called before ATD_CLASS is set.  This will  work, because   */
               /* all the kinds of arrays that cannot be parameters are       */
               /* caught by related attributes of these arrays, so we do not  */
               /* have to check the parameter attribute against the kind of   */
               /* dimension.  All other attributes are checked against        */
               /* PARAMETER by parse_attr_spec.  If this isn't a new          */
               /* attribute, then merge_parameter does semantic checking.     */
               /* All this is done to prevent an ordering problem with arrays */
               /* and parameters.  To be correct the array attribute must be  */
               /* added before the dimension attribute.  (See parse_attr_spec */
               /* under Parameter for more details.)                          */

               chk_semantics = !new_attr;

# if defined(_F_MINUS_MINUS)

               if (pe_array_idx == NULL_IDX && new_pe_array_idx != NULL_IDX) {

                  /* A co-array was specified with the variable, */
                  /* but not with the DIMENSION attribute word.  */

                  chk_semantics = TRUE;
               }
# endif


               if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
                  type_idx = ATD_TYPE_IDX(attr_idx);

                  if (TYP_TYPE(type_idx) == Character &&
                      TYP_CHAR_CLASS(type_idx) == Unknown_Char) {

                     char_bounds_resolution(attr_idx,
                                            &chk_semantics);
                  }

                  if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
                     array_bounds_resolution(attr_idx,
                                             &chk_semantics);

                     target_array_idx = ATD_ARRAY_IDX(attr_idx);
                  }

                  type_idx		= ATD_TYPE_IDX(attr_idx);

                  switch (TYP_TYPE(type_idx)) {
                  case Integer:
                  case Real:
                  case Complex:
                     check_type_conversion = TRUE;
                     target_type_idx	 = type_idx;
                     break;

                  case Character:

                     if (TYP_CHAR_CLASS(type_idx) == Const_Len_Char) {
                        check_type_conversion = TRUE;
                        target_type_idx = Character_1;
                        target_char_len_idx = TYP_IDX(type_idx);
                     }
                     break;
                  }
               }

               exp_desc.rank	= 0;
               expr_mode	= Initialization_Expr;
               xref_state       = CIF_Symbol_Reference;


               /* set comp_gen_expr to TRUE. This forces the fold of REAL   */
               /* constant expressions. When -Oieeeconform is specified,    */
               /* the folding of Real and Complex expressions is prevented. */
      
               comp_gen_expr = TRUE;

               if (expr_semantics(&init_opnd, &exp_desc)) {
                  check_type_conversion	= FALSE;
                  target_array_idx	= NULL_IDX;
                  expr_mode		= Regular_Expr;

                  /* There is an error with the PARAMETER attribute if    */
                  /* Parameter_Attr is not set, but has_parameter is.  If */
                  /* there is an error, do not try to merge_parameter.    */

                  if (attr_list & (1 << Parameter_Attr)) {
                     merge_parameter(chk_semantics,
                                     attr_idx,
                                     id_line,
                                     id_column,
                                     &init_opnd,
                                     &exp_desc,
                                     save_line,
                                     save_column);
                  }
               }
               else {
                  check_type_conversion = FALSE;
                  target_array_idx      = NULL_IDX;
                  expr_mode	        = Regular_Expr;
                  AT_DCL_ERR(attr_idx) = TRUE;
               }

               /* reset comp_gen_expr to FALSE. end of compiler gen'ed expr */
               comp_gen_expr = FALSE;
            }
            else {

               if (merge_data(TRUE, id_line, id_column, attr_idx)) {

                  if (SH_STMT_TYPE(curr_stmt_sh_idx) == Type_Decl_Stmt) {
                     SH_STMT_TYPE(curr_stmt_sh_idx)    = Type_Init_Stmt;
                     SH_COMPILER_GEN(curr_stmt_sh_idx) = TRUE;
                     SH_GLB_LINE(curr_stmt_sh_idx)     = id_line;
                     SH_COL_NUM(curr_stmt_sh_idx)      = id_column;
                  }
                  else {
                     gen_sh(After, Type_Init_Stmt, id_line, id_column, 
                            FALSE, FALSE, TRUE);
                  }

         	  stmt_type = Type_Init_Stmt;

                  NTR_IR_TBL(init_ir_idx);
                  SH_IR_IDX(curr_stmt_sh_idx) = init_ir_idx;

                  if (OPND_FLD(init_opnd) == IR_Tbl_Idx &&
                      IR_OPR(OPND_IDX(init_opnd)) == Call_Opr &&
                      AT_IS_INTRIN(IR_IDX_L(OPND_IDX(init_opnd))) &&
                      strcmp(AT_OBJ_NAME_PTR(IR_IDX_L(OPND_IDX(init_opnd))),
                                "NULL") == 0) {
                     if (IR_IDX_R(OPND_IDX(init_opnd)) != NULL_IDX) {
                        PRINTMSG(IR_LINE_NUM(OPND_IDX(init_opnd)), 1573, Error, 
                                 IR_COL_NUM(OPND_IDX(init_opnd))); 
                     }
                     IR_OPR(init_ir_idx) = Null_Opr;
                     if (!GT_encountered) {
                        PRINTMSG(TOKEN_LINE(token), 1562, Error, 
			 	 TOKEN_COLUMN(token));
                     }
                  }
                  else {
                     IR_OPR(init_ir_idx) = Init_Opr;
                     if (GT_encountered) {
                        PRINTMSG(TOKEN_LINE(token), 1562, Error, 
			 	 TOKEN_COLUMN(token));
                     }
                  }
                  IR_TYPE_IDX(init_ir_idx)    = TYPELESS_DEFAULT_TYPE;
                  IR_LINE_NUM(init_ir_idx)    = id_line;
                  IR_COL_NUM(init_ir_idx)     = id_column;
                  IR_LINE_NUM_L(init_ir_idx)  = id_line;
                  IR_COL_NUM_L(init_ir_idx)   = id_column;
                  IR_FLD_L(init_ir_idx)       = AT_Tbl_Idx;
                  IR_IDX_L(init_ir_idx)       = attr_idx;

                  COPY_OPND(IR_OPND_R(init_ir_idx), init_opnd);

                  /* The item is being initialized, so flag this by adding    */
                  /* 200 to the CIF "modification" value.		      */

                  usage_code = CIF_Symbol_Modification + 200;
               }
            }
         }
         else {
            /* error from parse_expr */
            AT_DCL_ERR(attr_idx) = TRUE;
         }

      }
      else if (has_parameter) {
         AT_DCL_ERR(attr_idx) = TRUE;
         PRINTMSG(LA_CH_LINE, 111, Error, LA_CH_COLUMN,
                  AT_OBJ_NAME_PTR(attr_idx));
      }

      AT_DCL_ERR(attr_idx) = AT_DCL_ERR(AT_WORK_IDX) || AT_DCL_ERR(attr_idx);

      if ((cif_flags & XREF_RECS) != 0) {
         cif_usage_rec(attr_idx,
                       AT_Tbl_Idx,
                       id_line,
                       id_column,
                       usage_code);
      }

      if (LA_CH_VALUE == COMMA ||
          (LA_CH_VALUE != EOS &&
           parse_err_flush(Find_Comma, ", or " EOS_STR))) {

         /* Intentionally left blank.  */

      }
      else {
         found_end = TRUE;
      }
      NEXT_LA_CH;
   }
   while (!found_end);

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_Type_Declaration_Stmt, stmt_number);
   }

EXIT: 

   TRACE (Func_Exit, "parse_type_dcl_stmt", NULL);

   return;

}  /* parse_type_dcl_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parses the use statement                                              *|
|*	BNF    USE module-name [,rename-list]                                 *|
|*	    or USE module-name, ONLY:[only-list]			      *|
|*	       rename  is  local-name => use-name                             *|
|*	       only    is  access-id					      *|
|*                     or  [local-name =>] use-name			      *|
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
void parse_use_stmt (void)

{
#ifdef KEY /* Bug 10177 */
   int			attr_idx = 0;
#else /* KEY Bug 10177 */
   int			attr_idx;
#endif /* KEY Bug 10177 */
   boolean		found_end		= TRUE;
   int			list_idx;
   int			name_idx;
   int			new_name_idx;
   use_type_type	prev_use		= Use_Not;
   int			ro_idx;
   int			use_ir_idx;
#ifdef KEY /* Bug 5089 */
   boolean		intrinsic		= FALSE;
   boolean		non_intrinsic		= FALSE;
#endif /* KEY Bug 5089 */


   TRACE (Func_Entry, "parse_use_stmt", NULL);

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Use_Stmt) ||
        STMT_CANT_BE_IN_BLK(Use_Stmt, CURR_BLK)) && iss_blk_stk_err()) {
      /* Block error - intentionally left blank */
   }
   else {
      curr_stmt_category = Use_Stmt_Cat;
   }

#ifdef KEY /* Bug 5089 */
   /* Optional ", module-nature ::" */
   if (LA_CH_VALUE == COMMA) {
      NEXT_LA_CH;  /* Consume comma */
      token_values_type module_nature;
      if ((!MATCHED_TOKEN_CLASS(Tok_Class_Keyword))
        || ((Tok_Kwd_Intrinsic != (module_nature = TOKEN_VALUE(token))) &&
        (Tok_Kwd_Nonintrinsic != module_nature))) { 
         parse_err_flush(Find_EOS, "INTRINSIC/NON_INTRINSIC");
      }
      /* Required :: */
      else if (!matched_specific_token(Tok_Punct_Colon_Colon,
        Tok_Class_Punct)) {
	  parse_err_flush(Find_EOS, "::");
      }
      else if (Tok_Kwd_Intrinsic == module_nature) {
        intrinsic = TRUE;
      }
      else if (Tok_Kwd_Nonintrinsic == module_nature) {
        non_intrinsic = TRUE;
      }
   }
   /* Consume optional :: */
   else {
     (void) matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct);
   }
#endif /* KEY Bug 5089 */

   if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) { 
      parse_err_flush(Find_EOS, "module-name");
      goto EXIT;
   }

   attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

   if (attr_idx != NULL_IDX) {  /* Name exists in symbol table already */

      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
          ATP_PGM_UNIT(attr_idx) == Module) {

         /* The only way this could be here, is if it */
         /* is specified in a previous USE statement. */

         prev_use = (use_type_type) ATP_USE_TYPE(attr_idx);
         list_idx = SCP_USED_MODULE_LIST(curr_scp_idx);

#ifdef KEY /* Bug 5089 */
	 /*
	  * Each "use" statement for a particular module in the current scope
	  * adds to the markings which indicate the module-nature, without
	  * removing earlier markings. During semantics, we need to know
	  * whether there were conflicting markings (e.g. a "use, intrinsic"
	  * along with a "use, non_intrinsic"; or even a "use, intrinsic"
	  * along with an unadorned "use" which finds a nonintrinsic.)
	  */
	 if (intrinsic) {
	   AT_IS_INTRIN(attr_idx) = TRUE;
	 } else if (non_intrinsic) {
	   ATT_NON_INTRIN(attr_idx) = TRUE;
	 } else {
	   ATT_NO_MODULE_NATURE(attr_idx) = TRUE;
	 }
#endif /* KEY Bug 5089 */

         while (list_idx != NULL_IDX) {

            if (AL_ATTR_IDX(list_idx) == attr_idx) {
               break;
            }
            list_idx = AL_NEXT_IDX(list_idx);
         }

         if (list_idx == NULL_IDX) {

            /* Found end of module list.  The attr is not */
            /* in the list.  Add the attr to the list.    */

            NTR_ATTR_LIST_TBL(list_idx);
            AL_ATTR_IDX(list_idx)				   = attr_idx;
            AL_PREV_MODULE_IDX(SCP_USED_MODULE_LIST(curr_scp_idx)) = list_idx;
            AL_NEXT_IDX(list_idx)	= SCP_USED_MODULE_LIST(curr_scp_idx);
            SCP_USED_MODULE_LIST(curr_scp_idx)	= list_idx;
            AT_USE_ASSOCIATED(attr_idx)		= TRUE;
            AT_MODULE_IDX(attr_idx)		= attr_idx;
            prev_use				= Use_Not;
         }
      }
      else { /* This is already something else in this scope.  */
         PRINTMSG(TOKEN_LINE(token), 791, Error,
                  TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(attr_idx));

         CREATE_ERR_ATTR(attr_idx, 
                         TOKEN_LINE(token),
                         TOKEN_COLUMN(token),
                         Pgm_Unit);
         ATP_PGM_UNIT(attr_idx)		   = Module;
         ATP_SCP_IDX(attr_idx)		   = curr_scp_idx;
         NTR_ATTR_LIST_TBL(list_idx);
         AL_ATTR_IDX(list_idx)					= attr_idx;
         AL_PREV_MODULE_IDX(SCP_USED_MODULE_LIST(curr_scp_idx))	= list_idx;
         AL_NEXT_IDX(list_idx)		   = SCP_USED_MODULE_LIST(curr_scp_idx);
         SCP_USED_MODULE_LIST(curr_scp_idx)= list_idx;
         AT_USE_ASSOCIATED(attr_idx)	   = TRUE;
         AT_MODULE_IDX(attr_idx)	   = attr_idx;
         MAKE_EXTERNAL_NAME(attr_idx,
                            AT_NAME_IDX(attr_idx), 
                            AT_NAME_LEN(attr_idx));
      }
   }
   else {
      attr_idx				= ntr_sym_tbl(&token, name_idx);
      AT_OBJ_CLASS(attr_idx)		= Pgm_Unit;
      ATP_PGM_UNIT(attr_idx)		= Module;
      ATP_SCP_IDX(attr_idx)		= curr_scp_idx;
      MAKE_EXTERNAL_NAME(attr_idx,
                         AT_NAME_IDX(attr_idx), 
                         AT_NAME_LEN(attr_idx));
      NTR_ATTR_LIST_TBL(list_idx);
      AL_ATTR_IDX(list_idx)					= attr_idx;
      AL_PREV_MODULE_IDX(SCP_USED_MODULE_LIST(curr_scp_idx))	= list_idx;
      AL_NEXT_IDX(list_idx)		= SCP_USED_MODULE_LIST(curr_scp_idx);
      SCP_USED_MODULE_LIST(curr_scp_idx)= list_idx;
      AT_USE_ASSOCIATED(attr_idx)	= TRUE;
      AT_MODULE_IDX(attr_idx)		= attr_idx;
      LN_DEF_LOC(name_idx)		= TRUE;
#ifdef KEY /* Bug 5089 */
      AT_IS_INTRIN(attr_idx)		= intrinsic;
      ATT_NON_INTRIN(attr_idx)		= non_intrinsic;
      ATT_NO_MODULE_NATURE(attr_idx)	= !(intrinsic || non_intrinsic);
#endif /* KEY Bug 5089 */
   }

   if (AT_ORIG_NAME_IDX(attr_idx) == NULL_IDX) {
      AT_ORIG_NAME_IDX(attr_idx)	= AT_NAME_IDX(attr_idx);
      AT_ORIG_NAME_LEN(attr_idx)	= AT_NAME_LEN(attr_idx);
   }

   if (ATP_GLOBAL_ATTR_IDX(attr_idx) == NULL_IDX) {

      /* This searches to see if there are other references/defines to this  */
      /* global name.  It issues an error if this name has been used as a    */
      /* common block or non-module name.  This returns the global name tbl  */
      /* index.  If this module has been referenced previously, we will have */
      /* the file path table index with the file name and an index to the    */
      /* start of this module information table in that file.  If GAP_FP_IDX */
      /* is blank, we will have to search for the file in use_stmt_semantics.*/

      AT_REFERENCED(attr_idx)		= Referenced;
      name_idx				= check_global_pgm_unit(attr_idx);
      ATP_MODULE_STR_IDX(attr_idx)	= GN_NAME_IDX(name_idx);
   }

   if ((cif_flags & XREF_RECS) != 0) {
      cif_usage_rec(attr_idx,
                    AT_Tbl_Idx,
                    TOKEN_LINE(token),
                    TOKEN_COLUMN(token),
                    CIF_Symbol_Reference);
   }

   if (LA_CH_VALUE == COMMA) {
      NEXT_LA_CH;  /* Consume comma */

      if (!MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) { 
         parse_err_flush(Find_EOS, "ONLY or use-name");
      }
      else if (TOKEN_VALUE(token) == Tok_Kwd_Only && LA_CH_VALUE == COLON) {
         NEXT_LA_CH;  /* Colon */

         if (LA_CH_VALUE != EOS) {
            parse_only_spec(attr_idx);
         }

         /* Check for error here - if interpretation makes it.  This must */
         /* always be used as ONLY:                                       */

         if (prev_use == Use_Not || prev_use == Use_Only) {
            ATP_USE_TYPE(attr_idx) = Use_Only;	 
         }

         goto EXIT;
      }
      else {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
         found_end = FALSE;
      }
   }
   else if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, ", or " EOS_STR);
   }

   while (!found_end) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         new_name_idx	= make_ro_entry(attr_idx, 
                                        NULL_IDX,
                                        TRUE); /* New name - do not order */

         if (matched_specific_token(Tok_Punct_Rename, Tok_Class_Punct)) {

            if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               ro_idx			= make_ro_entry(attr_idx, 
                                                        NULL_IDX,
                                                        FALSE);     /* order */
               RO_RENAME_IDX(ro_idx)	= new_name_idx;
               check_for_duplicate_renames(new_name_idx);
            }
            else {
               parse_err_flush(Find_Comma, NULL);
            }
         }
         else {
            parse_err_flush(Find_Comma, "=>");
         }
      }
      else {
         parse_err_flush(Find_Comma, "use-name");
      }

      if (LA_CH_VALUE != COMMA && LA_CH_VALUE != EOS) {
         parse_err_flush(Find_Comma, ", or " EOS_STR);
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;                            /* Pick up comma */
      }
      else if (LA_CH_VALUE == EOS) {
         found_end = TRUE;
      }
   }  /* End while */

   ATP_USE_TYPE(attr_idx) = (ATP_USE_LIST(attr_idx) == NULL_IDX) ? Use_All :
                                                                   Use_Renamed;

EXIT:

   /* Generate IR for this USE statement.  Need to keep the attr so that it  */
   /* can be passed thru the PDGCS interface during IR conversion.  Do not   */
   /* need pass2 semantics for this statement.                               */

   SH_P2_SKIP_ME(curr_stmt_sh_idx)	= TRUE;
   NTR_IR_TBL(use_ir_idx);
   IR_OPR(use_ir_idx)			= Use_Opr;
   IR_TYPE_IDX(use_ir_idx)              = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(use_ir_idx)		= stmt_start_line;
   IR_COL_NUM(use_ir_idx)		= stmt_start_col;
   IR_IDX_L(use_ir_idx)			= attr_idx;
   IR_FLD_L(use_ir_idx)			= AT_Tbl_Idx;
   IR_LINE_NUM_L(use_ir_idx)		= stmt_start_line;
   IR_COL_NUM_L(use_ir_idx)		= stmt_start_col;
   SH_IR_IDX(curr_stmt_sh_idx)		= use_ir_idx;

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_use_stmt", NULL);
   
   return;

}  /* parse_use_stmt */
#ifdef KEY /* Bug 11741 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parses the "import" statement                                         *|
|*	BNF    IMPORT [ :: [ import-name-list ]                               *|
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
void parse_import_stmt(void)

{
   boolean found_list = FALSE;

   TRACE (Func_Entry, "parse_import_stmt", NULL);

   PRINTMSG(TOKEN_LINE(token), 1685, Ansi, TOKEN_COLUMN(token), "IMPORT");

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Import_Stmt) ||
        STMT_CANT_BE_IN_BLK(Import_Stmt, CURR_BLK)) && iss_blk_stk_err()) {
      /* Block error - intentionally left blank */
   }
   else {
      curr_stmt_category = Import_Stmt_Cat;
   }

   /* Consume optional :: */
   if (matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct) &&
     LA_CH_VALUE == EOS) {
       parse_err_flush(Find_EOS, "identifier");
   }

   while (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
     found_list = TRUE;
     int missing_in_host = FALSE;
     int name_idx;
     int attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

     /* Name exists in symbol table already */
     if (attr_idx != NULL_IDX) {
       /* For "type(t) function()" we have already seen the type name, but we
        * haven't defined it, so we want to import it */
       if (AT_OBJ_CLASS(attr_idx) == Derived_Type && !AT_DEFINED(attr_idx)) {
	 if (import_from_host(TOKEN_STR(token), TOKEN_LEN(token), 0, attr_idx)
	   == NULL_IDX) {
	   missing_in_host = TRUE;
	 }
       }
       else {
	 /* If a duplicate import, rather than a conflict between import and
	  * local, then issue a mere warning */
	 int severity = AT_ATTR_LINK(attr_idx) ? Warning : Error;
	 PRINTMSG(TOKEN_LINE(token), 1683, severity, TOKEN_COLUMN(token),
	   TOKEN_STR(token));
       }
     }

     /* Make local name pointing to host's attribute */
     else {
       int host_name_idx;
       int host_attr_idx = srch_host_sym_tbl_for_import(TOKEN_STR(token),
         TOKEN_LEN(token), &host_name_idx);
       if (NULL_IDX == host_attr_idx) {
         missing_in_host = TRUE;
       }
       else {
	 attr_idx = ntr_host_in_sym_tbl(&token, name_idx, host_attr_idx,
	   host_name_idx, TRUE);
	 AT_DEFINED(attr_idx) = AT_DEFINED(host_attr_idx);
	 AT_LOCKED_IN(attr_idx) = TRUE;
       }
     }

     if (missing_in_host) {
       PRINTMSG(TOKEN_LINE(token), 1684, Error, TOKEN_COLUMN(token),
	 TOKEN_STR(token));
     }

     if (LA_CH_VALUE == COMMA) {
	NEXT_LA_CH;                            /* Pick up comma */
     }
     else {
	break;
     }
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, ", or " EOS_STR);
   }

   NEXT_LA_CH; /* Consume EOS */

   if (!found_list) {
     SCP_IMPORT(curr_scp_idx) = TRUE;
   }

   TRACE (Func_Exit, "parse_import_stmt", NULL);
}  /* parse_import_stmt */
#endif /* KEY Bug 11741 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parses the ONLY portion of the USE statement.			      *|
|*	BNF    only    is  access-id					      *|
|*                     or  [local-name =>] use-name			      *|
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
static	void	parse_only_spec(int	module_attr_idx)
{
   int		 first_name_idx;
   boolean	 found_end		= FALSE;
   int		 ro_idx;


   TRACE (Func_Entry, "parse_only_spec", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         first_name_idx	= make_ro_entry(module_attr_idx, 
                                        NULL_IDX,
                                        TRUE); /* New name - do not order */

         if (LA_CH_VALUE == EQUAL) {  /* Rename */

            if (!matched_specific_token(Tok_Punct_Rename, Tok_Class_Punct)) {
               parse_err_flush(Find_Comma, "=>");
               goto ERR_EXIT;
            }

            if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               parse_err_flush(Find_Comma, NULL);
               goto ERR_EXIT;
            }
            ro_idx = make_ro_entry(module_attr_idx, 
                                   NULL_IDX,
                                   FALSE);
            RO_RENAME_IDX(ro_idx)    = first_name_idx;
            check_for_duplicate_renames(first_name_idx);

         }
         else if (LA_CH_VALUE == LPAREN) {  /* Possible generic spec */
            reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));

            if (!parse_generic_spec()) {
               parse_err_flush(Find_Comma, NULL);
               goto ERR_EXIT;
            }

            rename_only_tbl_idx--;   /* Reuse the entry just made. */

            ro_idx = make_ro_entry(module_attr_idx, 
                                   NULL_IDX,    /* Get a new ro entry */
                                   FALSE);      /* Order it */
         }
         else {

            /* If this is not renamed - the ro entry is in first_name_idx.    */
            /* This is not a linked entry yet, because when we created it, we */
            /* didn't know if it was a local name or the original name.  The  */
            /* original names are linked together in alphabetical order.      */
            /* Pass in first_name_idx and make_ro_entry will insert it into   */
            /* the list in correct order.                                     */

            ro_idx = make_ro_entry(module_attr_idx, 
                                   first_name_idx,
                                   FALSE);
         }
      }
      else {
         parse_err_flush(Find_Comma, "use-name");
      }

ERR_EXIT:

      if (LA_CH_VALUE != COMMA && LA_CH_VALUE != EOS) {
         parse_err_flush(Find_Comma, ", or " EOS_STR);
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;                            /* Pick up comma */
      }
      else if (LA_CH_VALUE == EOS) {
         found_end = TRUE;
      }
   }  /* End while */
   while (!found_end);

   TRACE (Func_Exit, "parse_only_spec", NULL);
   
   return;

}  /* parse_only_spec */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    Parses the attr_spec in the type declaration statement                  *|
|*      BNF   type_spec[[,attr_spec]...::]entity-decl-list                    *|
|*            attr_spec is    PARAMETER                                       *|
|*                         or access_spec  is  PUBLIC or PRIVATE              *|
|*                         or ALLOCATABLE                                     *|
|*                         or DIMENSION(array-spec)                           *|
|*                         or EXTERNAL					      *|
|*                         or INTENT(intent-spec)			      *|
|*                         or INTRINSIC					      *|
|*                         or OPTIONAL					      *|
|*                         or POINTER					      *|
|*                         or SAVE					      *|
|*                         or TARGET					      *|
|*									      *|
|* Input parameters:							      *|
|*	attr_list --> A bit vector specifying which attrs have been           *|
|*	              specified already.                                      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
static long parse_attr_spec(int		*array_idx,
			    boolean	*has_parameter)

{
   long		attr_list	= 0;
   long		err_in_list;
   long		err_list	= 0;
   int		pe_array_idx;


   TRACE (Func_Entry, "parse_attr_spec", NULL);

   /* At entry, LA_CH_VALUE must be comma. */

   *has_parameter = FALSE;

   do {
      if (LA_CH_VALUE == EOS) {		/* Missing id list */
         break;
      }

      if (LA_CH_VALUE != COMMA) {
         parse_err_flush(Find_Comma, ", or ::");
         continue;
      }

      NEXT_LA_CH;
               
      if (!MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {
         parse_err_flush(Find_Comma, "ALLOCATABLE, "
#ifdef KEY /* Bug 14150 */
	                 "BIND, "
#endif /* KEY Bug 14150 */
	                 "DIMENSION, EXTERNAL, "
                         "INTENT, INTRINSIC, OPTIONAL, PARAMETER, POINTER, "
                         "PRIVATE, PUBLIC, SAVE or TARGET");
         continue;
      }

      switch (TOKEN_VALUE(token)) {

         case Tok_Kwd_Parameter:		

            /* merge_parameter will actually set this to Constant    */
            /* Do not set it here, because if this is an array, it   */
            /* must be set as an array first.  It is okay to set     */
            /* the dimension first and then add the parameter to     */
            /* it.  Wrong arrays will get caught as follows:         */
            /* If the array is adjustable or automatic, this is      */
            /* determined at end_pass1.  The attribute is already    */
            /* a parameter, so it's caught there.  Assumed_Size      */
            /* and Assumed_Shape arrays will get caught because      */
            /* by definition they must be dummy arguments and a      */
            /* parameter is not allowed to be a dummy argument.      */
            /* Deferred_Shape arrays are caught, because they must   */
            /* be ALLOCATABLE or POINTER.  The parameter is caught   */
            /* because a parameter can't be allocatable or a pointer.*/
            /* Parameter is returned as set, whether there is an     */
            /* error or not.                                         */

            err_in_list		= err_attrs[Parameter_Attr] & attr_list;
            attr_list		= attr_list | (1 << Parameter_Attr);
            *has_parameter	= TRUE;

            if (err_in_list) {
               issue_attr_err(Parameter_Attr, err_in_list);
               err_list	= err_list | (1 << Parameter_Attr);
            }
            break;


         case Tok_Kwd_Public:

            if (CURR_BLK != Module_Blk) {
               issue_attr_blk_err("PUBLIC");
            }
            else {
               err_in_list	= err_attrs[Public_Attr] & attr_list;
               attr_list	= attr_list | (1 << Public_Attr);

               if (err_in_list) {
                  issue_attr_err(Public_Attr, err_in_list);
                  err_list	= err_list | (1 << Public_Attr);
               }
               else {
                  AT_ACCESS_SET(AT_WORK_IDX)	= TRUE;
                  AT_PRIVATE(AT_WORK_IDX)	= FALSE;
               }
            }
            break;


         case Tok_Kwd_Private:

            if (CURR_BLK != Module_Blk) {
               issue_attr_blk_err("PRIVATE");
            }
            else {
               err_in_list	= err_attrs[Private_Attr] & attr_list;
               attr_list	= attr_list | (1 << Private_Attr);

               if (err_in_list) {
                  issue_attr_err(Private_Attr, err_in_list);
                  err_list	= err_list | (1 << Private_Attr);
               }
               else {
                  AT_ACCESS_SET(AT_WORK_IDX)	= TRUE;
                  AT_PRIVATE(AT_WORK_IDX)	= TRUE;
               }
            }
            break;


         case Tok_Kwd_Allocatable:

            if (STMT_CANT_BE_IN_BLK(Allocatable_Stmt, CURR_BLK)) {
               issue_attr_blk_err("ALLOCATABLE");
            }
            else {
               err_in_list	= err_attrs[Allocatable_Attr] & attr_list;
               attr_list	= attr_list | (1 << Allocatable_Attr);

               if (err_in_list) {
                  issue_attr_err(Allocatable_Attr, err_in_list);
                  err_list	= err_list | (1 << Allocatable_Attr);
               }
               else {
                  ATD_ALLOCATABLE(AT_WORK_IDX)	= TRUE;
                  ATD_IM_A_DOPE(AT_WORK_IDX)    = TRUE;
               }
            }
            break;


         case Tok_Kwd_Automatic:

            if (STMT_CANT_BE_IN_BLK(Automatic_Stmt, CURR_BLK)) {
               issue_attr_blk_err("AUTOMATIC");
            }
            else {
               PRINTMSG(TOKEN_LINE(token), 1254, Ansi, 
                        TOKEN_COLUMN(token),
                        "AUTOMATIC");
               err_in_list	= err_attrs[Automatic_Attr] & attr_list;
               attr_list	= attr_list | (1 << Automatic_Attr);

               if (err_in_list) {
                  issue_attr_err(Automatic_Attr, err_in_list);
                  err_list	= err_list | (1 << Automatic_Attr);
               }
               else {
                  ATD_STACK(AT_WORK_IDX)	= TRUE;
               }
            }
            break;


         /* External and intrinsic will get switched to program units */
         /* when the names are processed.                             */

         case Tok_Kwd_External:

            if (STMT_CANT_BE_IN_BLK(External_Stmt, CURR_BLK)) {
               issue_attr_blk_err("EXTERNAL");
            }
            else {
               err_in_list	= err_attrs[External_Attr] & attr_list;
               attr_list	= attr_list | (1 << External_Attr);

               if (err_in_list) {
                  issue_attr_err(External_Attr, err_in_list);
                  err_list = err_list | (1 << External_Attr);
               }
            }
            break;


         case Tok_Kwd_Intrinsic:

            err_in_list	= err_attrs[Intrinsic_Attr] & attr_list;
            attr_list	= attr_list | (1 << Intrinsic_Attr);

            if (err_in_list) {
               issue_attr_err(Intrinsic_Attr, err_in_list);
               err_list = err_list | (1 << Intrinsic_Attr);
            }
            break;


         case Tok_Kwd_Optional:

            if (STMT_CANT_BE_IN_BLK(Optional_Stmt, CURR_BLK)) {
               issue_attr_blk_err("OPTIONAL");
            }
            else {
               err_in_list	= err_attrs[Optional_Attr] & attr_list;
               attr_list	= attr_list | (1 << Optional_Attr);

               if (err_in_list) {
                  issue_attr_err(Optional_Attr, err_in_list);
                  err_list = err_list | (1 << Optional_Attr);
               }
               else {
                  if (AT_OBJ_CLASS(AT_WORK_IDX) == Data_Obj) {
                     ATD_CLASS(AT_WORK_IDX)	= Dummy_Argument;
                  }

                  AT_OPTIONAL(AT_WORK_IDX)	= TRUE;
               }
            }
            break;


         case Tok_Kwd_Pointer:

            err_in_list	= err_attrs[Pointer_Attr] & attr_list;
            attr_list	= attr_list | (1 << Pointer_Attr);

            if (err_in_list) {
               issue_attr_err(Pointer_Attr, err_in_list);
               err_list = err_list | (1 << Pointer_Attr);
            }
            else { /* EXTERNAL, INTRINSIC are illegal, so can't be pgm_unit   */
               ATD_POINTER(AT_WORK_IDX)   = TRUE;
               ATD_IM_A_DOPE(AT_WORK_IDX) = TRUE;
            }
            break;


         case Tok_Kwd_Save:

            if (ATP_SAVE_ALL(SCP_ATTR_IDX(curr_scp_idx))) {
               PRINTMSG(TOKEN_LINE(token), 133, Ansi, TOKEN_COLUMN(token));
            }

            err_in_list	= err_attrs[Save_Attr] & attr_list;
            attr_list	= attr_list | (1 << Save_Attr);

            if (err_in_list) {
               issue_attr_err(Save_Attr, err_in_list);
               err_list = err_list | (1 << Save_Attr);
            }
            else {
               ATD_SAVED(AT_WORK_IDX)	= TRUE;
               ATD_CLASS(AT_WORK_IDX)	= Variable;
            }
            break;


         case Tok_Kwd_Target:

            err_in_list	= err_attrs[Target_Attr] & attr_list;
            attr_list	= attr_list | (1 << Target_Attr);

            if (err_in_list) {
               issue_attr_err(Target_Attr, err_in_list);
               err_list = err_list | (1 << Target_Attr);
            }
            else {
               ATD_TARGET(AT_WORK_IDX) = TRUE;
            }
            break;


         case Tok_Kwd_Volatile:

            if (STMT_CANT_BE_IN_BLK(Volatile_Stmt, CURR_BLK)) {
               issue_attr_blk_err("VOLATILE");
            }
            else {
               PRINTMSG(TOKEN_LINE(token),
#ifdef KEY /* Bug 14110 */
	         1685,
#else /* KEY Bug 14110 */
	         1254,
#endif /* KEY Bug 14110 */
		 Ansi, TOKEN_COLUMN(token), "VOLATILE");
               err_in_list	= err_attrs[Volatile_Attr] & attr_list;
               attr_list	= attr_list | (1 << Volatile_Attr);

               if (err_in_list) {
                  issue_attr_err(Volatile_Attr, err_in_list);
                  err_list	= err_list | (1 << Volatile_Attr);
               }
               else {
                  ATD_VOLATILE(AT_WORK_IDX)	= TRUE;
               }
            }
            break;


         case Tok_Kwd_Intent:

            if (STMT_CANT_BE_IN_BLK(Intent_Stmt, CURR_BLK)) {
               issue_attr_blk_err("INTENT");
               parse_err_flush(Find_Comma, NULL);
               continue;
            }
            err_in_list	= err_attrs[Intent_Attr] & attr_list;
            attr_list	= attr_list | (1 << Intent_Attr);

            if (err_in_list) {
               issue_attr_err(Intent_Attr, err_in_list);
            }

            new_intent			= parse_intent_spec();
            ATD_CLASS(AT_WORK_IDX)	= Dummy_Argument;
            ATD_INTENT(AT_WORK_IDX)	= new_intent;
            break;

#ifdef KEY /* Bug 14150 */
	 case Tok_Kwd_Bind:
            if (STMT_CANT_BE_IN_BLK(Bind_Stmt, CURR_BLK)) {
               issue_attr_blk_err("BIND");
               parse_err_flush(Find_Comma, NULL);
               continue;
            }
            err_in_list	= err_attrs[Bind_Attr] & attr_list;
            attr_list	= attr_list | (1 << Bind_Attr);

            if (err_in_list) {
               issue_attr_err(Bind_Attr, err_in_list);
            }

	    if (parse_language_binding_spec(&new_binding_label)) {
              /* parse_attrs() will merge in the binding label */
	    }
	    break;

         case Tok_Kwd_Value:

            if (STMT_CANT_BE_IN_BLK(Value_Stmt, CURR_BLK)) {
               issue_attr_blk_err("VALUE");
               parse_err_flush(Find_Comma, NULL);
               continue;
            }
            err_in_list	= err_attrs[Value_Attr] & attr_list;
            attr_list	= attr_list | (1 << Value_Attr);

            if (err_in_list) {
               issue_attr_err(Value_Attr, err_in_list);
            }

	    else {
	       /* Set OBJ_CLASS just to avoid sytb_var_error() */
	       AT_OBJ_CLASS(AT_WORK_IDX) = Data_Obj;
	       ATD_CLASS(AT_WORK_IDX) = Dummy_Argument;
	       ATD_VALUE_ATTR(AT_WORK_IDX)	= TRUE;
	    }
            break;
#endif /* KEY Bug 14150 */

         case Tok_Kwd_Dimension:
            err_in_list	= err_attrs[Dimension_Attr] & attr_list;
            attr_list	= attr_list | (1 << Dimension_Attr);

            if (err_in_list) {
               issue_attr_err(Dimension_Attr, err_in_list);
               err_list = err_list | (1 << Dimension_Attr);
            }

            if (LA_CH_VALUE == LPAREN) {
               *array_idx	= parse_array_spec(AT_WORK_IDX);
            }
# ifdef _F_MINUS_MINUS
            else if (!cmd_line_flags.co_array_fortran || LA_CH_VALUE != LBRKT) 
# else
            else 
# endif
                      { /* Looking for array specifier */
               parse_err_flush(Find_Comma, "( dimension-spec )");
            }

# ifdef _F_MINUS_MINUS

            if (LA_CH_VALUE == LBRKT && cmd_line_flags.co_array_fortran) {
               err_in_list	= err_attrs[Co_Array_Attr] & attr_list;
               attr_list	= attr_list | (1 << Co_Array_Attr);

               if (err_in_list) {
                  issue_attr_err(Co_Array_Attr, err_in_list);
                  err_list = err_list | (1 << Co_Array_Attr);
               }

               pe_array_idx = parse_pe_array_spec(AT_WORK_IDX);

               if (!err_in_list) {
                  ATD_PE_ARRAY_IDX(AT_WORK_IDX) = pe_array_idx;
               }
            }
# endif
            break;
                       

         default:
            parse_err_flush(Find_Comma, "attr-spec");
            break;

      }   /* end switch */

   } /* end while */
   while (LA_CH_VALUE != COLON || 
          !matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct));

   /* Turn off any error bits */

   attr_list = attr_list^err_list;

   TRACE (Func_Exit, "parse_attr_spec", NULL);

   return(attr_list);

}  /* parse_attr_spec */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Issues error messages for illegal combinations of attributes on       *|
|*	the type declaration statement.                                       *|
|*									      *|
|* Input parameters:							      *|
|*	new_attr    -> The attribute being added.          		      *|
|*	err_in_list -> The error list in bit vector form.		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void issue_attr_err(attr_type	new_attr,
			   long		err_in_list)
{
   long		idx;


   TRACE (Func_Entry, "issue_attr_err", NULL);

   for (idx = 0; idx <= End_Attr; idx++) {

      if ((1 & err_in_list) != 0) {

         if (idx == new_attr) {

            /* More than one instance of this attribute in the attribute list */

            PRINTMSG(TOKEN_LINE(token), 424, Error, TOKEN_COLUMN(token),
                     attr_str[new_attr]);
         }
         else { /* Invalid combination of attributes in the list.             */

            PRINTMSG(TOKEN_LINE(token), 425, Error, TOKEN_COLUMN(token),
                     attr_str[new_attr], attr_str[idx]);
         }
      }
      err_in_list = err_in_list >> 1;
   }

   AT_DCL_ERR(AT_WORK_IDX)	= TRUE;

   TRACE (Func_Exit, "issue_attr_err", NULL);

   return;

}  /* issue_attr_err */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Issues error messages for illegal combinations of attributes on       *|
|*	the type declaration statement.                                       *|
|*	NOTE:  If errors are added here for illegal combinations between      *|
|*	       the type and the function name, they must also be added to     *|
|*	       parse_typed_function.  parse_typed_function does not call      *|
|*	       this routine.                                                  *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx  -> This is the attribute that gets the new type.            *|
|*	type_idx  -> This is the new type to add to the attribute.	      *|
|*	id_line   -> This is line where the item is being typed.      	      *|
|*	id_column -> This is column where the item is being typed.    	      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void	merge_type(int		attr_idx,
			   int		type_idx,
			   int		id_line,
			   int		id_column)

{
   boolean		error			= FALSE;
   int			func_idx;
   int			msg_num;
   opnd_type		opnd;
   char		       *ptr;
   char		       *ptr2;
   boolean		referenced_itrfc	= FALSE;
   int			rslt_idx;
   obj_type		sem_type		= Obj_Typed;
   boolean		set_type		= FALSE;


   TRACE (Func_Entry, "merge_type", NULL);

   if (AT_OBJ_CLASS(attr_idx) == Interface &&
       !AT_IS_INTRIN(attr_idx) &&
       ATI_PROC_IDX(attr_idx) != NULL_IDX) {
      referenced_itrfc	= AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref;
      attr_idx		= ATI_PROC_IDX(attr_idx);
   }

   if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit && ATP_RSLT_NAME(attr_idx)) {

      /* Use the result name to type */

      PRINTMSG(id_line, 185, Error, id_column,
               AT_OBJ_NAME_PTR(attr_idx),
               AT_OBJ_NAME_PTR(ATP_RSLT_IDX(attr_idx)));
      AT_DCL_ERR(attr_idx) = TRUE;
      AT_DCL_ERR(ATP_RSLT_IDX(attr_idx)) = TRUE;
      goto EXIT;
   }

   if (TYP_TYPE(type_idx) == Character && 
       TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {
      sem_type = Obj_Assum_Type_Ch;
      error    = fnd_semantic_err(sem_type,
                                  id_line,
                                  id_column,
                                  attr_idx,
                                  TRUE);
   }

# if ! defined(_EXTENDED_CRI_CHAR_POINTER)
   else if (TYP_TYPE(type_idx) == Character &&
            AT_OBJ_CLASS(attr_idx) == Data_Obj &&
            ATD_CLASS(attr_idx) == CRI__Pointee) {
            PRINTMSG(id_line,625,Error,id_column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     "Cray pointee","CHARACTER*(*)");
            AT_DCL_ERR(attr_idx) = TRUE;
            error = TRUE;
   }
# endif
   else if (AT_ATTR_LINK(attr_idx) != NULL_IDX ||
            AT_USE_ASSOCIATED(attr_idx) ||
            AT_OBJ_CLASS(attr_idx) != Data_Obj ||
            ATD_SYMBOLIC_CONSTANT(attr_idx) ||
            AT_TYPED(attr_idx) ) {
 
      /* Replace the message output string, with the type that is being */
      /* defined for this object.  Gives a more meaningful message.     */

/*       strcpy(obj_str[Obj_Typed][0], get_basic_type_str(type_idx));  */

      ptr = get_basic_type_str(type_idx);
      (obj_str[Obj_Typed]) = ptr;
    

      error = fnd_semantic_err(Obj_Typed,
                               id_line,
                               id_column,
                               attr_idx,
                               TRUE);
   }

   /* If this thing has been referenced or defined already, the error is */
   /* caught later in this routine so that a better message can be issued*/

# ifdef _DEBUG

   /* Check to make sure that this routine is catching everything in the  */
   /* semantic tables, because it only calls fnd_semantic_err if it finds */
   /* an error.                                                           */

   if (!error && fnd_semantic_err(Obj_Typed, 
                                  id_line,
                                  id_column,
                                  attr_idx,
                                  TRUE)) {
      PRINTMSG(id_line, 655, Internal, id_column, "merge_type");
   }

# endif

   if (AT_ARG_TO_KIND(attr_idx)) {
      PRINTMSG(id_line, 1522, Error, id_column, AT_OBJ_NAME_PTR(attr_idx));
      error = TRUE;
   }

   if (error) {
      AT_DCL_ERR(attr_idx) = TRUE;
      goto EXIT;
   }

   switch (AT_OBJ_CLASS(attr_idx))  {
   case Data_Obj:

      if (ATD_CLASS(attr_idx) == CRI__Pointee) {

         if (TYP_TYPE(type_idx) == Structure) {
            PRINTMSG(id_line, 650, Error,
                     id_column,
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx) = TRUE;
         }
      }

      if (AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref ||
          ATD_CLASS(attr_idx) == Constant ||
          AT_NAMELIST_OBJ(attr_idx) ||
          ATD_DATA_INIT(attr_idx)) {

         if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != TYP_LINEAR(type_idx)) {

            /* If this is character, make sure the variable is not referenced */
            /* in its own character length.  This is obscure, but we're in an */
            /* error situation anyway - so we might as well do it right.      */

            if (TYP_TYPE(type_idx) == Character &&
                TYP_FLD(type_idx) == AT_Tbl_Idx &&
                find_attr_in_ir(attr_idx, 
                                ATD_TMP_IDX(TYP_IDX(type_idx)),
                                &opnd)) {
               AT_DCL_ERR(attr_idx)	= TRUE;
               PRINTMSG(OPND_LINE_NUM(opnd), 1035, Error,
                        OPND_COL_NUM(opnd),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else if (SCP_IMPL_NONE(curr_scp_idx)) { /* IMPLICIT NONE in scope */
               PRINTMSG(id_line, 1424, Error, id_column,
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else {

               if (ATD_CLASS(attr_idx) == Constant) {
                  msg_num = 238;
               }
               else if (ATD_DATA_INIT(attr_idx)) {
                  msg_num = 239;
               }
               else if (AT_NAMELIST_OBJ(attr_idx)) {
                  msg_num = 1002;
               }
               else {  /* Ref'd in a spec expression */
                  msg_num = 827;
               }

               if (!AT_DCL_ERR(attr_idx)) {
                  PRINTMSG(id_line, msg_num, Error,
                           id_column,
                           AT_OBJ_NAME_PTR(attr_idx),
                           get_basic_type_str(ATD_TYPE_IDX(attr_idx)));
               }
            }

            type_idx = ATD_TYPE_IDX(attr_idx);
         }
         else if (SCP_IMPL_NONE(curr_scp_idx)) { /* IMPLICIT NONE in scope */
            PRINTMSG(id_line, 1423, Ansi, id_column,
                     AT_OBJ_NAME_PTR(attr_idx));
         }
      }
      else if (sem_type == Obj_Assum_Type_Ch &&
               ATD_CLASS(attr_idx) == Function_Result) {
         func_idx = ATD_FUNC_IDX(attr_idx);

         PRINTMSG(id_line, 1565,
#ifdef KEY /* Bug 318, 321 */
	   Ansi,
#else /* KEY Bug 318, 321 */
	   Comment,
#endif /* KEY Bug 318, 321 */
	   id_column); /* Obsolescent */

         /* fnd_semantic_err catches everything but the current function */

         if (ATP_PROC(func_idx) == Intern_Proc ||
            ATP_PROC(func_idx) == Module_Proc) {

            /* An internal or module procedure cannot be assumed size char */
            /* Allow it to be set for error recovery.                      */

            PRINTMSG(id_line, 367, Error, id_column,
                     AT_OBJ_NAME_PTR(func_idx));

            AT_DCL_ERR(attr_idx) = TRUE;
            AT_DCL_ERR(func_idx) = TRUE;
         }
         else if (ATP_IN_INTERFACE_BLK(func_idx)) {

            /* An interface block may be typed as assumed size character, */
            /* but it must not be invoked.                                   */

            PRINTMSG(id_line, 1566, Warning, id_column,
                     AT_OBJ_NAME_PTR(func_idx));
         }
         else if (ATP_RECURSIVE(func_idx)) {

            /* Recursive is not allowed to be assumed size character */
            /* Allow it to be set for error recovery.                */

            PRINTMSG(id_line, 506, Error, id_column,
                     AT_OBJ_NAME_PTR(func_idx));

            AT_DCL_ERR(attr_idx) = TRUE;
            AT_DCL_ERR(func_idx) = TRUE;
         }
      }

      set_type	= TRUE;
      break;


   case Pgm_Unit:

      if (ATP_PGM_UNIT(attr_idx) != Function) {
         CREATE_FUNC_RSLT(attr_idx, rslt_idx);
         ATP_PGM_UNIT(attr_idx) = Function;
      }
      else {
         rslt_idx = ATP_RSLT_IDX(attr_idx);

         if (attr_idx != SCP_ATTR_IDX(curr_scp_idx) &&
             !ATP_ALT_ENTRY(attr_idx) &&
              (AT_REFERENCED(rslt_idx) >= Dcl_Bound_Ref || referenced_itrfc)) {

            /* This has been used already */

            if (ATD_TYPE_IDX(rslt_idx) != type_idx) {

               /* If this is character, make sure the function is not       */
               /* referenced in its own character length.  This is obscure, */
               /* but we're in an error situation anyway - so we might as   */
               /* well do it right.                                         */

               if (TYP_TYPE(type_idx) == Character &&
                   TYP_FLD(type_idx) == AT_Tbl_Idx &&
                   find_attr_in_ir(attr_idx, 
                                   ATD_TMP_IDX(TYP_IDX(type_idx)),
                                   &opnd)) {
                  AT_DCL_ERR(attr_idx)	= TRUE;
                  PRINTMSG(OPND_LINE_NUM(opnd), 1035, Error,
                           OPND_COL_NUM(opnd),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else {
                  PRINTMSG(id_line, 
                           118, 
                           Error, 
                           id_column,
                           AT_OBJ_NAME_PTR(attr_idx),
                           get_basic_type_str(ATD_TYPE_IDX(rslt_idx)));
               }

               type_idx	= ATD_TYPE_IDX(rslt_idx);
            }
         }
         else if (sem_type == Obj_Assum_Type_Ch) {

            /* fnd_semantic_err catches everything but current function */

            PRINTMSG(id_line, 1565,
#ifdef KEY /* Bug 318, 321 */
	      Ansi,
#else /* KEY Bug 318, 321 */
	      Comment,
#endif /* KEY Bug 318, 321 */
	      id_column); /* Obsolescent */

            if (ATP_PROC(attr_idx) == Intern_Proc ||
                ATP_PROC(attr_idx) == Module_Proc) {

               /* An internal or module procedure cannot be assumed size */
               /* char.  Allow it to be set for error recovery.          */

               AT_DCL_ERR(attr_idx) = TRUE;
               AT_DCL_ERR(rslt_idx) = TRUE;
               PRINTMSG(id_line, 367, Error,
                        id_column,
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else if (ATP_IN_INTERFACE_BLK(attr_idx)) {

               /* An interface block may typed as assumed size  */
               /* character, but it cannot be invoked.          */

               PRINTMSG(id_line, 1566, Warning, id_column,
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else if (ATP_RECURSIVE(attr_idx)) {

               /* Recursive is not allowed to be assumed size character */
               /* Allow it to be set for error recovery.                */

               AT_DCL_ERR(attr_idx) = TRUE;
               AT_DCL_ERR(rslt_idx) = TRUE;
               PRINTMSG(id_line, 
                        506, 
                        Error, 
                        id_column,
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
      }

      set_type	= TRUE;
      attr_idx	= rslt_idx;
      break;

   case Interface:

      if (AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref) {  /* Used already */

         /* Do not use the ATD_TYPE_IDX on the interface.  This is only set */
         /* if the interface block has been explicitly typed via a type     */
         /* declaration statement.  Find any implicit type given to the     */
         /* intrinsic, by finding the intrinsic with the same name, via     */
         /* ATI_PROC_IDX.                                                   */

         if (ATP_RSLT_IDX(SN_ATTR_IDX(ATI_FIRST_SPECIFIC_IDX(attr_idx))) !=
                           NULL_IDX &&
             TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(
                 SN_ATTR_IDX(ATI_FIRST_SPECIFIC_IDX(attr_idx))))) != 
                                                     TYP_TYPE(type_idx)) {
            PRINTMSG(id_line, 950, Error, id_column,
                     AT_OBJ_NAME_PTR(attr_idx));
         }
      }

      set_type	= TRUE;
      break;

   case Stmt_Func:

      if (AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref &&
          TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != TYP_LINEAR(type_idx)) {
         AT_TYPED(attr_idx) = TRUE;

         /* If this is character, make sure the function is not       */
         /* referenced in its own character length.  This is obscure, */
         /* but we're in an error situation anyway - so we might as   */
         /* well do it right.                                         */

         if (TYP_TYPE(type_idx) == Character &&
             TYP_FLD(type_idx) == AT_Tbl_Idx &&
             find_attr_in_ir(attr_idx, 
                             ATD_TMP_IDX(TYP_IDX(type_idx)),
                             &opnd)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
            PRINTMSG(OPND_LINE_NUM(opnd), 1035, Error,
                     OPND_COL_NUM(opnd),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         else if (SCP_IMPL_NONE(curr_scp_idx)) { /* IMPLICIT NONE in scope */
            PRINTMSG(id_line, 1424, Error, id_column,
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         else {
            PRINTMSG(id_line, 827, Error,
                     id_column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     get_basic_type_str(ATD_TYPE_IDX(attr_idx)));
         }
      }
      else {

         if (SCP_IMPL_NONE(curr_scp_idx)) { /* IMPLICIT NONE in scope */
            PRINTMSG(id_line, 1423, Ansi, id_column,
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         set_type	= TRUE;
      }
      break;

   default:
      break;

   }  /* End switch */

   if (set_type) {

      if (AT_TYPED(attr_idx)) {
         ptr = get_basic_type_str(type_idx);

#ifdef KEY /* Bug 5040 */
	 ptr2 = get_basic_type_str(ATD_TYPE_IDX(attr_idx));
	 /* Type mismatch might still look like identical type within error
	  * message (e.g. "real" and "real*8" both print "real(kind=8)") so
	  * treat them as equal if they compare equal. */
         if (type_idx == ATD_TYPE_IDX(attr_idx) ||
	   0 == strcmp(ptr, ptr2)) {
            PRINTMSG(id_line, 1259, ansi_or_warning(), id_column,
                     AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), ptr);
         }
         else {
            PRINTMSG(id_line, 550, Error, id_column, AT_OBJ_NAME_PTR(attr_idx),
	             ptr2, ptr, AT_DEF_LINE(attr_idx));
         }
#else /* KEY Bug 5040 */
         if (type_idx == ATD_TYPE_IDX(attr_idx)) {
            PRINTMSG(id_line, 1259, Ansi, id_column,
                     AT_OBJ_NAME_PTR(attr_idx), ptr);
         }
         else {
            ptr2 = get_basic_type_str(ATD_TYPE_IDX(attr_idx));
            PRINTMSG(id_line, 550, Error, id_column,
                     AT_OBJ_NAME_PTR(attr_idx), ptr2, ptr);
         }
#endif /* KEY Bug 5040 */
      }
      else {
         AT_TYPED(attr_idx)	= TRUE;
         ATD_TYPE_IDX(attr_idx)	= type_idx;
      }
   }

EXIT:

   TRACE (Func_Exit, "merge_type", NULL);

   return;

} /* merge_type */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Issues error 206 when an attribute is used in the wrong context.      *|
|*	Uses the TOKEN to get the line and column number.                     *|
|*									      *|
|* Input parameters:							      *|
|*	attr_str  -> String to go in message, with name of attribute.         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
static	void	issue_attr_blk_err(char		*attr_str)

{
   boolean	 issue_msg	= TRUE;
#ifdef KEY /* Bug 10177 */
   char		*msg_str = 0;
#else /* KEY Bug 10177 */
   char		*msg_str;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "issue_attr_blk_err", NULL);

   switch (CURR_BLK) {

      case Unknown_Blk:
         PRINTMSG(TOKEN_LINE(token), 160, Internal, TOKEN_COLUMN(token));
         break;

      case Program_Blk:
         msg_str = "PROGRAM";
         break;

      case Function_Blk:
         msg_str = "FUNCTION";
         break;

      case Subroutine_Blk:
         msg_str = "SUBROUTINE";
         break;

      case Module_Blk:
         msg_str = "MODULE";
         break;

      case Blockdata_Blk:
         msg_str = "BLOCKDATA";
         break;

      case Interface_Body_Blk:
      case Internal_Blk:
      case Module_Proc_Blk:
         msg_str = (ATP_PGM_UNIT(CURR_BLK_NAME) == Function) ? "FUNCTION" :
                                                               "SUBROUTINE";
         break;

      case Where_Then_Blk:
      case Where_Else_Blk:
      case Where_Else_Mask_Blk:
      case Select_Blk:
      case Case_Blk:
      case Do_Blk:
      case If_Blk:
      case If_Then_Blk:
      case If_Else_If_Blk:
      case If_Else_Blk:
      case Contains_Blk:
      case Derived_Type_Blk:
      case Interface_Blk:
#ifdef KEY /* Bug 10572 */
      case Enum_Blk:
#endif /* KEY Bug 10572 */

         /* These things are caught earlier.  The type declaration statement */
         /* is not allowed in any of the executable blocks, so don't print   */
         /* another out of context msg.  If the type declaration statement   */
         /* is found in a contains or an interface block, a parse error is   */
         /* issued, because the compiler thinks this is a FUNCTION stmt.     */
         /* If the type declaration statement is in a Derived_Type_Blk it    */
         /* won't be here, because it's parsed as a component decl stmt.     */

         issue_msg = FALSE;
         break;


   }  /* End switch */

   if (issue_msg) {
      AT_DCL_ERR(AT_WORK_IDX)	= TRUE;
      PRINTMSG(TOKEN_LINE(token), 595, Error, 
               TOKEN_COLUMN(token),
               attr_str,
               msg_str);
   }

   TRACE (Func_Exit, "issue_attr_blk_err", NULL);

   return;

} /* issue_attr_blk_err */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parse DATA implied-DO loops.                                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      result_opnd - opnd_type, points to root of tree returned.             *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if parsed ok                                                     *|
|*                                                                            *|
|* Algorithm notes:							      *|
|*      This procedure is recursive.					      *|
|*      This procedure duplicates the IR generation of parse_imp_do (which is *|
|*      used to parse I/O implied-DOs).					      *|
|*                                                                            *|
\******************************************************************************/

static	boolean  parse_data_imp_do(opnd_type	*result_opnd)

{
   int          attr_idx;
   int		column;
   int 		expr_start_col;
   int		expr_start_line;
   boolean	found_attr;
   boolean      had_equal 	= FALSE;
   int		imp_do_start_col;
   int		imp_do_start_line;
   int          ir_idx;
   int		line;
   int          list_idx;
   int          list2_idx 	= NULL_IDX;
   int          name_column;
   int          name_idx;
   int          name_line;
   opnd_type    opnd;
   boolean      parsed_ok 	= TRUE;
   boolean	save_in_implied_do;


   TRACE (Func_Entry, "parse_data_imp_do", NULL);

   /* Generate the Implied_Do IR and set the result opnd to point at it.      */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)           = Implied_Do_Opr;
   IR_TYPE_IDX(ir_idx)      = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)      = LA_CH_LINE;
   IR_COL_NUM(ir_idx)       = LA_CH_COLUMN;
   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = ir_idx;

   imp_do_start_line = LA_CH_LINE;
   imp_do_start_col  = LA_CH_COLUMN;
   save_in_implied_do = in_implied_do;
   in_implied_do = TRUE;

   /* Parse the targets.  A target can be another implied-DO or a (hopefully  */
   /* subscripted) variable name.  Recurse if the target is an implied-DO.    */
   /* If not an implied-DO keep going as long as we keep hitting commas.  The */
   /* last (hopefully UNsubscripted) item in the list should be the implied-DO*/
   /* variable.								      */

   do {

      /* Eat the left paren (if entering this loop) or comma (if continuing   */
      /* this loop).							      */

      NEXT_LA_CH;

      if (LA_CH_VALUE == LPAREN) {

         if (parsed_ok = parse_data_imp_do(&opnd)) {
  
            if (LA_CH_VALUE != COMMA) {
               parsed_ok = FALSE;
               parse_err_flush(Find_Rparen, ",");
               continue;
            }
         }
         else {

            if (LA_CH_VALUE != EOS) {
               parse_err_flush(Find_Rparen, NULL);
               NEXT_LA_CH;
            }

            goto EXIT;
         }
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

         if (LA_CH_VALUE == EQUAL) {
            had_equal = TRUE;

            parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;

            if (parsed_ok) {
               mark_attr_defined(&opnd);
            }

            if (OPND_FLD(opnd) == AT_Tbl_Idx &&
                AT_OBJ_CLASS(OPND_IDX(opnd)) == Data_Obj) {

               ATD_SEEN_AS_LCV(OPND_IDX(opnd)) = TRUE;

               if (ATD_SEEN_IN_IMP_DO(OPND_IDX(opnd)) &&
                   (AT_DEF_LINE(OPND_IDX(opnd)) > imp_do_start_line ||
                    (AT_DEF_LINE(OPND_IDX(opnd)) == imp_do_start_line &&
                     AT_DEF_COLUMN(OPND_IDX(opnd)) > imp_do_start_col))) {

                  /* clear ATD_SEEN_IN_IMP_DO */

                  ATD_SEEN_IN_IMP_DO(OPND_IDX(opnd)) = FALSE;
               }
            }

            /* Set up right operand (the implied-DO variable) of the          */
            /* Implied_Do IR.  The implied-DO variable must be a named        */
            /* variable; if it's not, the error (199) will be caught down     */
            /* below in target processing.				      */

            if (OPND_FLD(opnd) == AT_Tbl_Idx  &&
                AT_OBJ_CLASS(OPND_IDX(opnd)) == Data_Obj) {
               attr_idx = OPND_IDX(opnd);
            }

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(ir_idx) = IL_Tbl_Idx;
            IR_IDX_R(ir_idx) = list_idx;
            COPY_OPND(IL_OPND(list_idx), opnd);
            
            /* Eat the equal sign.					      */
            /* Generate an IL entry to hold the loop start value and attach   */
            /* it to the LCV IL entry.  Parse the loop start value expression.*/

            NEXT_LA_CH;

            NTR_IR_LIST_TBL(list2_idx);
            IL_NEXT_LIST_IDX(list_idx)  = list2_idx;
            IL_PREV_LIST_IDX(list2_idx) = list_idx;
            expr_start_line             = LA_CH_LINE;
            expr_start_col              = LA_CH_COLUMN;
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list2_idx), opnd);
            IL_LINE_NUM(list2_idx)      = expr_start_line;
            IL_COL_NUM(list2_idx)       = expr_start_col;

            if (LA_CH_VALUE != COMMA) {
               parsed_ok = FALSE;
               parse_err_flush(Find_Rparen, ",");
               continue;
            }

            /* Eat the comma following the start value expression.	      */
            /* Generate an IL entry to hold the loop end value and attach it  */
            /* to the start value IL.  Parse the end value expression.	      */
            
            NEXT_LA_CH;

            NTR_IR_LIST_TBL(list_idx);
            IL_NEXT_LIST_IDX(list2_idx) = list_idx;
            IL_PREV_LIST_IDX(list_idx)  = list2_idx;
            expr_start_line             = LA_CH_LINE;
            expr_start_col              = LA_CH_COLUMN;
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list_idx), opnd);
            IL_LINE_NUM(list_idx)       = expr_start_line;
            IL_COL_NUM(list_idx)        = expr_start_col;

            /* If a loop increment expression exists, generate an IL entry    */
            /* and attach it to the loop end value IL.  Parse the increment   */
            /* value expression.					      */

            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;
               NTR_IR_LIST_TBL(list2_idx);
               IL_NEXT_LIST_IDX(list_idx)  = list2_idx;
               IL_PREV_LIST_IDX(list2_idx) = list_idx;
               expr_start_line             = LA_CH_LINE;
               expr_start_col              = LA_CH_COLUMN;
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list2_idx), opnd);
               IL_LINE_NUM(list2_idx)      = expr_start_line;
               IL_COL_NUM(list2_idx)       = expr_start_col;
               IR_LIST_CNT_R(ir_idx) = 4;
            }
            else {
               IR_LIST_CNT_R(ir_idx) = 3;
            }

            break;
         }
         else {

            /* Search for the target's Attr.  If no Attr exists in the        */
            /* current scope, enter one (the target is being implicitly       */
            /* declared by its presence in the DATA stmt).                    */

            attr_idx =
               srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

            if (attr_idx == NULL_IDX) {
               found_attr           = FALSE;
               attr_idx             = ntr_sym_tbl(&token, name_idx);
               LN_DEF_LOC(name_idx) = TRUE;
               SET_IMPL_TYPE(attr_idx);
            }
            else {
               found_attr = TRUE;

               if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
                  AT_ATTR_LINK(attr_idx) = NULL_IDX;
                  LN_DEF_LOC(name_idx)	 = TRUE;
               }
            }

            name_line   = TOKEN_LINE(token);
            name_column = TOKEN_COLUMN(token);


            /* The target name is followed by a left paren (which normally    */
            /* means it's subscripted) or a percent (component to come):      */
            /*   Parse the full reference.				      */
            /*   If the name the form of an array element reference, but was  */
            /*     not declared locally to be an array, it's an error.	      */
            /* Otherwise, fake up an opnd.				      */
            /* Use merge_data to set AT_DEFINED, ATD_DATA_INIT, and ATD_CLASS.*/

            if (LA_CH_VALUE == LPAREN  ||  LA_CH_VALUE == PERCENT) {

               if (parse_deref(&opnd, NULL_IDX)) {

                  if (OPND_FLD(opnd) == IR_Tbl_Idx  &&
                      IR_OPR(OPND_IDX(opnd)) == Call_Opr) {
                     PRINTMSG(name_line, 699, Error, name_column);
                     parsed_ok = FALSE;
                  }

                  if (LA_CH_VALUE == EQUAL) {
                     find_opnd_line_and_column(&opnd, &line, &column);
                     PRINTMSG(line, 199, Error, column);
                     parse_err_flush(Find_Rparen, NULL_IDX);
                     parsed_ok = FALSE;
                  }
    
               }
               else {
                  parse_err_flush(Find_Rparen, NULL);
                  parsed_ok = FALSE;
               }

            }
            else {
               OPND_LINE_NUM(opnd) = TOKEN_LINE(token);
               OPND_COL_NUM(opnd)  = TOKEN_COLUMN(token);
               OPND_FLD(opnd)      = AT_Tbl_Idx;
               OPND_IDX(opnd)      = attr_idx;
            }

            if (parsed_ok) {

               if (! merge_data(found_attr, name_line, name_column, attr_idx)) {
                  parsed_ok = FALSE;
               }
            }
         }
      }
      else {     /* Not an implied-DO and not an identifier. */
         parsed_ok = FALSE;
         parse_err_flush(Find_Rparen, 
                         (list2_idx == NULL_IDX) ?
                            "data-i-do-object" :
                            "data-i-do-object or data-i-do-variable");
      }

      /* Generate an IL entry for the target and attach the IL to the left    */
      /* operand chain of the Implied_Do IR.				      */
    
      NTR_IR_LIST_TBL(list_idx);
      COPY_OPND(IL_OPND(list_idx), opnd);

      if (IR_IDX_L(ir_idx) == NULL_IDX) {
         IR_LIST_CNT_L(ir_idx) = 1;
         IR_FLD_L(ir_idx)      = IL_Tbl_Idx;
         IR_IDX_L(ir_idx)      = list_idx;
      }
      else {
         IL_NEXT_LIST_IDX(list2_idx) = list_idx;
         IL_PREV_LIST_IDX(list_idx)  = list2_idx;
         ++IR_LIST_CNT_L(ir_idx);
      }

      list2_idx = list_idx;
   }
   while (LA_CH_VALUE == COMMA);

   in_implied_do = save_in_implied_do;

   if (LA_CH_VALUE == RPAREN) {
     
      if (! SH_ERR_FLG(curr_stmt_sh_idx)  &&  ! had_equal) {
         parsed_ok = FALSE;
         parse_err_flush(Find_Rparen, ",");
      }
   }
   else {

      if (had_equal) {
         parse_err_flush(Find_EOS,
                         (IR_LIST_CNT_R(ir_idx) == 3) ? ", or )" : ")");
      }
      else {

         if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
            parse_err_flush(Find_EOS, "=, comma, or '(subscript-list)'");
         }
         else {
            parse_err_flush(Find_EOS, ",");
         }

      }

      parsed_ok = FALSE;
      goto EXIT;
   }

   NEXT_LA_CH;                                  /* Eat the right paren. */

EXIT:

   TRACE (Func_Exit, "parse_data_imp_do", NULL);

   return(parsed_ok);

}  /* parse_data_imp_do */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This calls bound_semantics to resolve the character to a constant     *|
|*	length character for parameter initialization.                        *|
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

void char_bounds_resolution(int		attr_idx,
         		    boolean     *chk_semantics)

{
   int		tmp_idx;


   TRACE (Func_Entry, "char_bounds_resolution", NULL);

   if (TYP_FLD(ATD_TYPE_IDX(attr_idx)) == CN_Tbl_Idx) {
      return;
   }

   tmp_idx		= TYP_IDX(ATD_TYPE_IDX(attr_idx));
   xref_state		= CIF_Symbol_Reference;
   no_func_expansion	= TRUE;

   /* Call bound_semantics.  No IR will be generated in a valid  */
   /* case, so pass FALSE.                                       */

   if (ATD_CLASS(tmp_idx) != Constant) {
      bound_semantics(tmp_idx, FALSE);
   }

   char_len_resolution(attr_idx, TRUE);  /* Needs to be a constant */

   if (TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) != Const_Len_Char) {

      /* The variable length character cannot be a parameter error will   */
      /* issue in fnd_semantic_err.  This will go through                 */
      /* char_len_resolution during decl_semantics for error recovery.    */

      *chk_semantics	= TRUE;
   }

   no_func_expansion	= FALSE;

   TRACE (Func_Exit, "char_bounds_resolution", NULL);

   return;

}  /* char_bounds_resolution */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This calls bound_semantics to resolve an array to a constant size     *|
|*	array for parameter initialization.                                   *|
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

void array_bounds_resolution(int	attr_idx,
		             boolean    *chk_semantics)

{
   int		bd_idx;
   int		dim;


   TRACE (Func_Entry, "array_bounds_resolution", NULL);

   bd_idx = ATD_ARRAY_IDX(attr_idx);

   if (BD_RESOLVED(bd_idx)) {
      return;
   }

   if (BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {
      xref_state	= CIF_Symbol_Reference;
      no_func_expansion	= TRUE;

      /* Call bound_semantics for each bound.  No IR will be generated      */
      /* in a valid case, so pass FALSE.                                    */

      for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

         if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             ATD_CLASS(BD_LB_IDX(bd_idx, dim)) != Constant) {
            bound_semantics(BD_LB_IDX(bd_idx, dim), FALSE);

            if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
                ATD_CLASS(BD_LB_IDX(bd_idx, dim)) != Constant) {

               /* This did not resolve to a constant. - May be okay */

               AT_REFERENCED(BD_LB_IDX(bd_idx, dim))  = Referenced;
            }
         }

         if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             ATD_CLASS(BD_UB_IDX(bd_idx, dim)) != Constant) {
            bound_semantics(BD_UB_IDX(bd_idx, dim), FALSE);

            if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
                ATD_CLASS(BD_UB_IDX(bd_idx, dim)) != Constant) {

               /* This did not resolve to a constant. - May be okay */

               AT_REFERENCED(BD_UB_IDX(bd_idx, dim))  = Referenced;
            }
         }
      }

      no_func_expansion	= FALSE;
   }


   /* TRUE means this must be a constant array.  If it is not,  */
   /* array_dim_resolution will not set BD_RESOLVED and this    */
   /* array should get resolved during decl_semantics.          */

   array_dim_resolution(attr_idx, TRUE);

   /* Need to use ATD_ARRAY_IDX, because bd_idx may change in resolution */

   if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) !=  Explicit_Shape ||
       BD_ARRAY_SIZE(ATD_ARRAY_IDX(attr_idx)) != Constant_Size) { 
      *chk_semantics = TRUE;
   }

# ifdef _F_MINUS_MINUS
   bd_idx = ATD_PE_ARRAY_IDX(attr_idx);

   if (bd_idx == NULL_IDX ||
       BD_RESOLVED(bd_idx)) {
      return;
   }

   if (BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {
      xref_state        = CIF_Symbol_Reference;
      no_func_expansion = TRUE;

      /* Call bound_semantics for each bound.  No IR will be generated      */
      /* in a valid case, so pass FALSE.                                    */

      for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

         if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             ATD_CLASS(BD_LB_IDX(bd_idx, dim)) != Constant) {
            bound_semantics(BD_LB_IDX(bd_idx, dim), FALSE);

            if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
                ATD_CLASS(BD_LB_IDX(bd_idx, dim)) != Constant) {

               /* This did not resolve to a constant. - May be okay */

               AT_REFERENCED(BD_LB_IDX(bd_idx, dim))  = Referenced;
            }
         }

         if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
             ATD_CLASS(BD_UB_IDX(bd_idx, dim)) != Constant) {
            bound_semantics(BD_UB_IDX(bd_idx, dim), FALSE);

            if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx &&
                ATD_CLASS(BD_UB_IDX(bd_idx, dim)) != Constant) {

               /* This did not resolve to a constant. - May be okay */

               AT_REFERENCED(BD_UB_IDX(bd_idx, dim))  = Referenced;
            }
         }
      }

      no_func_expansion = FALSE;
   }

   pe_array_dim_resolution(attr_idx);  /* It must be a constant array */

# endif
   TRACE (Func_Exit, "array_bounds_resolution", NULL);

   return;

}  /* array_bounds_resolution */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the PARAMETER attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	attr_idx -> Attr index to add the EXTERNAL attribute to.              *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The column number of the object to add the attribute to   *|
|*	opnd     -> A pointer to an operand that holds the parsed constant.   *|
|*	            This routine does the semantic checking and folding of    *|
|*	            the constant.                                             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static	void	merge_parameter(boolean		 chk_semantics,
				int		 attr_idx,
		                int		 line,
			        int		 column,
			        opnd_type	*opnd,
				expr_arg_type   *const_exp_desc,
				int		 const_line,
				int		 const_column)

{
   int			 a_type_idx;
   int			 c_type_idx;
   char			*c_char_ptr;
   char			*char_ptr;
   long_type		 constant[MAX_WORDS_FOR_NUMERIC];
   int			 const_idx;
   long64		 i;
   char			 msg_str[45];
   int			 o_column;
   int			 o_line;
   long_type		 the_constant;


   TRACE (Func_Entry, "merge_parameter", NULL);

   if (chk_semantics) {

      if (fnd_semantic_err(Obj_Constant, line, column, attr_idx, TRUE)) {
         goto EXIT;
      }

      if ((AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || AT_DEFINED(attr_idx))) {
         AT_DCL_ERR(attr_idx) = TRUE;

         if (ATD_CLASS(attr_idx) == Atd_Unknown) {

            /* This was most likely referenced as a constant earlier.   */
            /* Issue a meaningful message and let it become a constant. */

            PRINTMSG(line, 1426, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         else { /* This was referenced earlier and not as a constant. */
            PRINTMSG(line, 559, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     "PARAMETER");
            goto EXIT;
         }
      }
   }

   a_type_idx	= ATD_TYPE_IDX(attr_idx);

   if (TYP_TYPE(a_type_idx) == Structure && 
#ifdef KEY /* Bug 572, 6845 */
       /* The prohibition against constant structures having pointer components
	* was removed from the end of section 4.4.4, "Construction of
	* derived-type values", between Fortan 90 and Fortran 95. On the other
	* hand, starting with TR15581, we need to prohibit constant structures
	* having allocatable components. */
       ATT_ALLOCATABLE_CPNT(TYP_IDX(a_type_idx))
#else /* KEY Bug 572, 6845 */
       ATT_POINTER_CPNT(TYP_IDX(a_type_idx))
#endif /* KEY Bug 572, 6845 */
      ) {
      PRINTMSG(line, 691, Error, column,
               AT_OBJ_NAME_PTR(attr_idx));
      AT_DCL_ERR(attr_idx)	= TRUE;
      goto EXIT;
   }

   /* AT_DEFINED is set, so that parameter constants can be differentiated */
   /* from compiler tmp constants.  Compiler tmp constants are created by  */
   /* bounds resolution.  They will not have AT_DEFINED set.  CIF wants    */
   /* all parameters, whether they were referenced or not.  Compiler tmps  */
   /* have AT_REFERENCED = Not_Referenced, but they still are Constants so */
   /* they were going thru to CIF anyway.  Now they don't if they don't    */
   /* have their AT_DEFINED flag set.                                      */

   AT_DEFINED(attr_idx)	= TRUE;
   ATD_CLASS(attr_idx)	= Constant;

   if (opnd == NULL_IDX || ! const_exp_desc->foldable) {

      /* The initialization expression must be a constant. */

      find_opnd_line_and_column(opnd, &o_line, &o_column);
      PRINTMSG(o_line, 587, Error, o_column,
               AT_OBJ_NAME_PTR(attr_idx));
      AT_DCL_ERR(attr_idx)	= TRUE;
      ATD_CONST_IDX(attr_idx)	= NULL_IDX;
      ATD_FLD(attr_idx)		= NO_Tbl_Idx;
      goto EXIT;
   }


   while (OPND_FLD((*opnd)) == IR_Tbl_Idx) {
      COPY_OPND((*opnd), IR_OPND_L(OPND_IDX((*opnd))));
   }

   ATD_FLD(attr_idx)		= OPND_FLD((*opnd));
   ATD_CONST_IDX(attr_idx)	= OPND_IDX((*opnd));

   if (OPND_FLD((*opnd)) == AT_Tbl_Idx) {

      /* since this has a data init'd tmp it must be */
      /* marked as referenced,                       */
      AT_REFERENCED(attr_idx) = Referenced;

      /* change the tmps name to the constants name */
      AT_NAME_IDX(OPND_IDX((*opnd))) = AT_NAME_IDX(attr_idx);
      AT_NAME_LEN(OPND_IDX((*opnd))) = AT_NAME_LEN(attr_idx);
      
      c_type_idx = const_exp_desc->type_idx;
      find_opnd_line_and_column(opnd, &o_line, &o_column);

      if (TYP_LINEAR(c_type_idx) == Long_Typeless) {
         PRINTMSG(o_line, 1133, Error, o_column);
         AT_DCL_ERR(attr_idx)   = TRUE;
         goto EXIT;
      }

      if (!check_asg_semantics(a_type_idx, c_type_idx, o_line, o_column)) {
         msg_str[0] = '\0';
         strcpy(msg_str, get_basic_type_str(a_type_idx));

         PRINTMSG(line, 580, Error, column,
                  AT_OBJ_NAME_PTR(attr_idx),
                  msg_str,
                  get_basic_type_str(c_type_idx));

         AT_DCL_ERR(attr_idx) = TRUE;
         goto EXIT;
      }

      /* check array conformance */

      if (const_exp_desc->rank > 0) {

         if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
            PRINTMSG(line, 835, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx) = TRUE;
            goto EXIT;
         }

         if (const_exp_desc->rank == BD_RANK(ATD_ARRAY_IDX(attr_idx))) {

            for (i = 1; i <= const_exp_desc->rank; i++) {

               if (fold_relationals(const_exp_desc->shape[i-1].idx,
                                    BD_XT_IDX(ATD_ARRAY_IDX(attr_idx),i),
                                    Ne_Opr)) {

                  PRINTMSG(line, 834, Error, column, AT_OBJ_NAME_PTR(attr_idx));
                  AT_DCL_ERR(attr_idx) = TRUE;
                  goto EXIT;
               }
            }
         }
         else {
            PRINTMSG(line, 834, Error, column, AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)   = TRUE;
            goto EXIT;
         }
      }


      if (TYP_TYPE(a_type_idx) == Character &&
          TYP_CHAR_CLASS(a_type_idx) == Assumed_Size_Char) {

         /* attr gets length from constant */

         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)        = Character;
         TYP_LINEAR(TYP_WORK_IDX)      = TYP_LINEAR(a_type_idx);
         TYP_DESC(TYP_WORK_IDX)        = TYP_DESC(a_type_idx);
         TYP_DCL_VALUE(TYP_WORK_IDX)   = TYP_DCL_VALUE(a_type_idx);
         TYP_CHAR_CLASS(TYP_WORK_IDX)  = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)         = CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)         = TYP_IDX(c_type_idx);
         ATD_TYPE_IDX(attr_idx)        = ntr_type_tbl();

         if (ATD_ARRAY_IDX(attr_idx)) {
            /* stride multipliers are not done since it was assumed size */
            BD_RESOLVED(ATD_ARRAY_IDX(attr_idx)) = FALSE;
            array_dim_resolution(attr_idx, TRUE);
            ATD_ARRAY_IDX(ATD_CONST_IDX(attr_idx)) = ATD_ARRAY_IDX(attr_idx);
         }
      }
   }
   else if (a_type_idx != CN_TYPE_IDX(OPND_IDX((*opnd)))) {
       c_type_idx = CN_TYPE_IDX(OPND_IDX((*opnd)));
       find_opnd_line_and_column(opnd, &o_line, &o_column);

      if (TYP_LINEAR(c_type_idx) == Long_Typeless) {
         PRINTMSG(o_line, 1133, Error, o_column);
         AT_DCL_ERR(attr_idx)   = TRUE;
         goto EXIT;
      }

      if (!check_asg_semantics(a_type_idx, c_type_idx, o_line, o_column)) {
         msg_str[0] = '\0';
         strcpy(msg_str, get_basic_type_str(a_type_idx));

         PRINTMSG(line, 580, Error, column,
                  AT_OBJ_NAME_PTR(attr_idx), 
                  msg_str,
                  get_basic_type_str(c_type_idx));

         AT_DCL_ERR(attr_idx) = TRUE;
         goto EXIT;
      }

      switch (TYP_TYPE(a_type_idx)) {
      case Integer:
      case Real:
      case Complex:
      case Logical:

         if (TYP_TYPE(c_type_idx) == Character) {
            /* change to typeless constant */
            /* BRIANJ - Should we use cvrt to do this? */
            the_constant     = CN_CONST(OPND_IDX((*opnd)));

            /* TYPELESS_DEFAULT_TYPE is default index for a Typeless of */
            /* length equal to the number of bits in a word.     */

            OPND_IDX((*opnd)) = ntr_const_tbl(TYPELESS_DEFAULT_TYPE,
                                              FALSE,
                                              &the_constant);
            c_type_idx = TYPELESS_DEFAULT_TYPE;
         }

         if (TYP_LINEAR(a_type_idx) == TYP_LINEAR(c_type_idx)) {
            /* intentionally blank */
         }
         else {
            find_opnd_line_and_column(opnd, &o_line, &o_column);

            if (folder_driver((char *)&CN_CONST(OPND_IDX((*opnd))),
                              c_type_idx,
                              NULL,
                              NULL_IDX,
                              constant,
                             &a_type_idx,
                              o_line,
                              o_column,
                              1,
                              Cvrt_Opr)) {

            /* Enter with the attr's type - but make it Default_Typed */

            ATD_FLD(attr_idx)		= CN_Tbl_Idx;
            ATD_CONST_IDX(attr_idx)	= ntr_const_tbl(TYP_LINEAR(a_type_idx),
                                                        FALSE,
                                                        constant);
            }
         }
         break;


      case Character:

         if (TYP_TYPE(c_type_idx) != Character && 
             TYP_TYPE(c_type_idx) != Typeless) {

            /* should flag error here? */
         }
         else if (TYP_CHAR_CLASS(a_type_idx) == Assumed_Size_Char) {

            /* attr gets length from constant */

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
            TYP_TYPE(TYP_WORK_IDX)	= Character;
            TYP_LINEAR(TYP_WORK_IDX)	= TYP_LINEAR(a_type_idx);
            TYP_DESC(TYP_WORK_IDX)	= TYP_DESC(a_type_idx);
            TYP_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(a_type_idx);
            TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= TYP_IDX(c_type_idx);
            ATD_TYPE_IDX(attr_idx)	= ntr_type_tbl();
         }
         else if (TYP_IDX(a_type_idx) != TYP_IDX(c_type_idx)) {

            /* Assume that these are both CN_Tbl_Idx.  Create a new constant */
            /* for the right length and put the original string in it.       */
            /* Truncate or blank pad to fit.  NULL_IDX to ntr_const_tbl      */
            /* that the caller will add the constant to the constant pool.   */

            const_idx = ntr_const_tbl(a_type_idx, TRUE, NULL_IDX);

            char_ptr	= (char *)&CN_CONST(const_idx);
            c_char_ptr	= (char *)&CN_CONST(OPND_IDX((*opnd)));

            for (i = 0; i < CN_INT_TO_C(TYP_IDX(a_type_idx)); i++) {
               char_ptr[i] = (i >= CN_INT_TO_C(TYP_IDX(c_type_idx))) ?
                             ' ' : c_char_ptr[i];
            }

            /* blank pad the new constant to a word boundary */

            while ((++i) % TARGET_CHARS_PER_WORD != 0) {
               char_ptr[i] = ' ';
            }

            ATD_FLD(attr_idx)		= CN_Tbl_Idx;
            ATD_CONST_IDX(attr_idx)	= const_idx;
         }
         break;
      }          
   }

   if (cif_flags & INFO_RECS) {
      cif_named_constant_rec(attr_idx, const_line, const_column);
   }

EXIT:

   TRACE (Func_Exit, "merge_parameter", NULL);

   return;

}  /* merge_parameter */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	If we're going to issue a type not defined message, try to give       *|
|*	more info to the user, by checking if this is an interface block.     *|
|*	If it is, then check if the type is defined there.  If it is, then    *|
|*	issue a message about not being able to host associate from the       *|
|*	parent of the interface block.                                        *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx -> Attr index of the type.                                   *|
|*	line     -> Line number to issue message for                          *|
|*	column   -> Column number to issue message for.                       *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void	issue_undefined_type_msg(int		 attr_idx,
		                 int		 line,
			         int		 column)

{
   int		host_attr_idx;
   int		host_name_idx;
   int		msg_num		= 126;
   int		save_scp_idx;


   TRACE (Func_Entry, "issue_undefined_type_msg", NULL);

   if (SCP_IS_INTERFACE(curr_scp_idx)) {
      save_scp_idx	= curr_scp_idx;
      curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);

      host_attr_idx	= srch_sym_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                       AT_NAME_LEN(attr_idx),
                                       &host_name_idx);

      curr_scp_idx	= save_scp_idx;

      if (host_attr_idx != NULL_IDX &&
          (AT_OBJ_CLASS(host_attr_idx) == Derived_Type &&
           (AT_USE_ASSOCIATED(host_attr_idx) && 
             !AT_NOT_VISIBLE(host_attr_idx)) ||
           LN_DEF_LOC(host_name_idx))) {

         /* Attempt to give more info to the user, by assuming they are  */
         /* trying to host associate into an interface body.  Alert them */
         /* that this is not allowed in Fortran 95.                      */

         msg_num = 1420;
      }
   }

   PRINTMSG(line, msg_num, Error, column, AT_OBJ_NAME_PTR(attr_idx));
   AT_DCL_ERR(attr_idx)	= TRUE;

   TRACE (Func_Exit, "issue_undefined_type_msg", NULL);

   return;

}  /* issue_undefined_type_msg */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	parse_initializer parse the initializers on a DATA statement or in    *|
|*	the slash format (extension) on the type declaration statement.       *|
|*									      *|
|* Input parameters:							      *|
|*	init_ir_idx -> IR to attach the initializer to                        *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static	boolean	parse_initializer(int	init_ir_idx)

{
   int		column;
   boolean	found_star;
   boolean	get_init_value;
   int		il_idx;
   int		ir_idx;
   int		line;
#ifdef KEY /* Bug 10177 */
   boolean	ok = FALSE;
#else /* KEY Bug 10177 */
   boolean	ok;
#endif /* KEY Bug 10177 */
   opnd_type	opnd;
   int		uopr_ir_idx		= NULL_IDX;
   int		value_chain_end;


   TRACE (Func_Entry, "parse_initializer", NULL);

   get_init_value		= TRUE;
   value_chain_end		= FALSE;
   found_star			= FALSE;
   IR_LIST_CNT_R(init_ir_idx)	= 0;

   while (get_init_value  &&  LA_CH_VALUE != EOS) {
      NTR_IR_LIST_TBL(il_idx);

      if (value_chain_end == NULL_IDX) {
         IR_FLD_R(init_ir_idx) = IL_Tbl_Idx;
         IR_IDX_R(init_ir_idx) = il_idx;
      }
      else {
         IL_NEXT_LIST_IDX(value_chain_end) = il_idx;
         IL_PREV_LIST_IDX(il_idx)          = value_chain_end;
      }

      value_chain_end = il_idx;
      ++IR_LIST_CNT_R(init_ir_idx);

      strcpy(parse_operand_insert, "data-stmt-repeat or data-stmt-constant");
        
      if (LA_CH_VALUE == MINUS  ||  LA_CH_VALUE == PLUS) {
         NTR_IR_TBL(uopr_ir_idx);
         IR_OPR(uopr_ir_idx)      = (LA_CH_VALUE == MINUS) ? Uminus_Opr :
                                                             Uplus_Opr;
         IR_LINE_NUM(uopr_ir_idx) = LA_CH_LINE;
         IR_COL_NUM(uopr_ir_idx)  = LA_CH_COLUMN;
         NEXT_LA_CH;
      }

      if (!parse_operand(&opnd)) {
         parse_err_flush(Find_EOS, NULL);
         ok = FALSE;
         goto EXIT;
      }

      if (OPND_FLD(opnd) == IR_Tbl_Idx && IR_OPR(OPND_IDX(opnd)) == Paren_Opr) {
         PRINTMSG(IR_LINE_NUM(OPND_IDX(opnd)), 197, Error,
                  IR_COL_NUM(OPND_IDX(opnd)),
                  "data-stmt-repeat or data-stmt-constant", "(");
      }

      if (LA_CH_VALUE == STAR) {

         /* The first was the repeat value.  This should be the constant */

         found_star = TRUE;

         if (uopr_ir_idx != NULL_IDX) {

            /* Illegal to have a signed constant here. */

            uopr_ir_idx = NULL_IDX;
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 542, Error, column);
         }

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)      = Rep_Count_Opr;
         IR_LINE_NUM(ir_idx) = LA_CH_LINE;
         IR_COL_NUM(ir_idx)  = LA_CH_COLUMN;

         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         data_repeat_semantics(ir_idx);

         NEXT_LA_CH;			/* Eat the asterisk */
               
         if (LA_CH_VALUE == MINUS  ||  LA_CH_VALUE == PLUS) {
            NTR_IR_TBL(uopr_ir_idx);
            IR_OPR(uopr_ir_idx) = (LA_CH_VALUE == MINUS) ? Uminus_Opr :
                                                           Uplus_Opr;
            IR_LINE_NUM(uopr_ir_idx) = LA_CH_LINE;
            IR_COL_NUM(uopr_ir_idx)  = LA_CH_COLUMN;
            NEXT_LA_CH;
         }

         strcpy(parse_operand_insert, "data-stmt-constant");

         if (!parse_operand(&opnd)) {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         if (OPND_FLD(opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(opnd)) == Paren_Opr) {
            PRINTMSG(IR_LINE_NUM(OPND_IDX(opnd)), 197, Error,
                     IR_COL_NUM(OPND_IDX(opnd)),
                     "data-stmt-constant", "(");
         }

         constant_value_semantics(&opnd, uopr_ir_idx);

         COPY_OPND(IR_OPND_R(ir_idx), opnd);
      
         OPND_FLD(opnd) = IR_Tbl_Idx;
         OPND_IDX(opnd) = ir_idx;
      }
      else {  /* Do some necessary pass 1 semantic checks for constant */ 
         constant_value_semantics(&opnd, uopr_ir_idx);
      }

      uopr_ir_idx = NULL_IDX;

      COPY_OPND(IL_OPND(il_idx), opnd);

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
         found_star  = FALSE;
      }
      else {
         get_init_value = FALSE;
      }
   }  /* End while */

   if (LA_CH_VALUE == SLASH) {
      NEXT_LA_CH;
      ok = TRUE;
   }
   else {
      parse_err_flush(Find_EOS,
                      (found_star) ? "comma or /" : "comma, *, or /");
      ok = FALSE;
   }

EXIT:

   TRACE (Func_Exit, "parse_initializer", NULL);

   return(ok);

}  /* parse_initializer */

#ifdef KEY /* Bug 14110 */
static char **surprise_ids;
static unsigned surprise_ids_cnt;
static unsigned surprise_ids_limit;

/*
 * id		Identifier which appears in "volatile" statement without
 *		having previously created an attribute in the current scope
 */
void surprise_volatile(char *id) {
  if (surprise_ids_cnt >= surprise_ids_limit) {
    surprise_ids = surprise_ids_limit ?
      realloc(surprise_ids, surprise_ids_limit *= 2) :
      malloc(10 * sizeof(char *));
  }
  surprise_ids[surprise_ids_cnt++] = strdup(id);
}

/*
 * For each identifier which appeared in a "volatile" statement without
 * having previously created an attribute in the current scope, if there's
 * now a local or host-associated attribute, set the "volatile" bit on that
 * attribute. Otherwise, create an attribute in the current scope.
 */
void
revisit_volatile() {
  for (int i = 0; i < surprise_ids_cnt; i += 1) {
    int name_idx;
    token_type tmp = initial_token;
    memcpy(TOKEN_STR(tmp), surprise_ids[i], strlen(surprise_ids[i]));
    int attr_idx = srch_sym_tbl(TOKEN_STR(tmp), TOKEN_LEN(tmp), &name_idx);
    if (!attr_idx) {
      attr_idx = srch_host_sym_tbl(TOKEN_STR(tmp), TOKEN_LEN(tmp), &name_idx,
        0);
    }
    if (!attr_idx) {
      attr_idx = ntr_sym_tbl(&tmp, name_idx);
      SET_IMPL_TYPE(attr_idx);
    }
    merge_volatile(TRUE, 1, 1, attr_idx);
    free(surprise_ids[i]);
  }
  surprise_ids_cnt = 0;
}
#endif /* KEY Bug 14110 */
