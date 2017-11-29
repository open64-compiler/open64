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



static char USMID[] = "\n@(#)5.0_pl/sources/p_asg_expr.c	5.3	06/17/99 09:28:10\n";

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


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

boolean parse_level_1 (opnd_type *);
boolean parse_mult_opnd (opnd_type *);
boolean parse_add_opnd (opnd_type *);
boolean parse_level_2 (opnd_type *);
boolean parse_level_3 (opnd_type *);
boolean parse_level_4 (opnd_type *);
boolean parse_and_opnd (opnd_type *);
boolean parse_or_opnd (opnd_type *);
boolean parse_equiv_opnd (opnd_type *);
boolean parse_level_5 (opnd_type *);
boolean parse_lhs (opnd_type *, int);


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

void parse_assignment_stmt (void)

{
   int			attr_idx;
   int			buf_idx;
   int			col;
   int          	host_attr_idx;
   int          	host_name_idx;
   int			ir_idx;
   int          	line;
   int          	name_idx;
   opnd_type    	opnd = INIT_OPND_TYPE;
   stmt_category_type	save_curr_stmt_category;
   char			str[2];
   int			stmt_num;


   TRACE (Func_Entry, "parse_assignment_stmt", NULL);


   attr_idx = srch_sym_tbl (TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

   if (attr_idx == NULL_IDX) {			 /* search host sym tab */
      host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token), 
                                        TOKEN_LEN(token),
                                        &host_name_idx,
                                        FALSE);

      if (host_attr_idx != NULL_IDX && IS_STMT_ENTITY(host_attr_idx)) {

         /* do not hook up to host stmt entities */
         host_attr_idx = NULL_IDX;
      }

      if (host_attr_idx != NULL_IDX) {

         /* Make entry in local name table for this item.  Make a new attr    */
         /* and attr_link them together.                                      */

         attr_idx = ntr_host_in_sym_tbl(&token, name_idx, host_attr_idx,
                                        host_name_idx, TRUE);
      }
      else {		 /* enter attr in local symbol table */
         attr_idx = ntr_sym_tbl(&token, name_idx);
         SET_IMPL_TYPE(attr_idx);
      }
   }

   if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
      ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
   }

   if (curr_stmt_category < Executable_Stmt_Cat                  && 
       LA_CH_VALUE             == LPAREN                         &&
       AT_ATTR_LINK(attr_idx)  == NULL_IDX                       && 
       AT_OBJ_CLASS(attr_idx)  == Data_Obj                       &&
       ATD_ARRAY_IDX(attr_idx) == NULL_IDX                       &&
       (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Character ||
        ! is_substring_ref())) {

      parse_stmt_func_stmt(attr_idx, name_idx);
      goto EXIT;
   }

   /* The WHERE block is marked as not allowing executables, because the only */
   /* statement allowed in a WHERE block is the assignment statement.  So if  */
   /* CURR_BLK_NO_EXEC is set, have to check that this isn't a WHERE block.   */


   /* Likewise, the FORALL block is marked as not allowing executables,       */
   /* because the only statements allowed in a FORALL construct are:          */
   /*   * assignment statement (including pointer assignment statement)       */
   /*   * WHERE statement or WHERE construct				      */
   /*   * another FORALL construct or FORALL statement			      */
   /* So if CURR_BLK_NO_EXEC is set, also have to check that this isn't a     */
   /* FORALL block.  							      */

   if (CURR_BLK_NO_EXEC &&
       CURR_BLK != Where_Else_Blk && 
       CURR_BLK != Where_Then_Blk &&
       CURR_BLK != Where_Else_Mask_Blk && 
       CURR_BLK != Forall_Blk) {

      if (iss_blk_stk_err()) {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }
   }

   save_curr_stmt_category = curr_stmt_category;
   curr_stmt_category = Executable_Stmt_Cat;
   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx)	= ir_idx;

   if (!parse_lhs(&opnd, attr_idx)) {
      parse_err_flush(Find_EOS, NULL);
      goto EXIT;
   }

   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   IR_LINE_NUM(ir_idx)		= LA_CH_LINE;
   IR_COL_NUM(ir_idx)		= LA_CH_COLUMN;

   line = LA_CH_LINE;
   col  = LA_CH_COLUMN;
   buf_idx = LA_CH_BUF_IDX;
   stmt_num = LA_CH_STMT_NUM;

   if (LA_CH_VALUE == EOS) {
      PRINTMSG(line, 724, Error, col, EOS_STR);
      curr_stmt_category = save_curr_stmt_category;
   }
   else if (MATCHED_TOKEN_CLASS(Tok_Class_Punct) &&
            (TOKEN_VALUE(token) == Tok_Punct_Eq ||
             TOKEN_VALUE(token) == Tok_Punct_Rename)) {
      IR_OPR(ir_idx) = (TOKEN_VALUE(token) == Tok_Punct_Eq) ? Asg_Opr :
                                                              Ptr_Asg_Opr;
      parse_expr(&opnd);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);
   }
   else {
      reset_lex(buf_idx, stmt_num);
      str[0] = LA_CH_VALUE;
      str[1] = '\0';
      PRINTMSG(line, 724, Error, col, str);
      parse_err_flush(Find_EOS, NULL);
      curr_stmt_category = save_curr_stmt_category;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, "operator or " EOS_STR);
   }

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_assignment_stmt", NULL);

   return;

}  /* parse_assignment_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BNF	level-5-expr { defined-binary-op level-5-expr }               *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE                      					      *|
|*									      *|
|* Returns:								      *|
|*									      *|
\******************************************************************************/

boolean parse_expr (opnd_type	*result)

{
   int       attr_idx;
   int	     host_attr_idx;
   int	     host_name_idx;
   int       ir_idx;
   int       list1_idx;
   int       list2_idx;
   int       name_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_expr", NULL);


   parsed_ok = parse_level_5(&opnd);

   while (TOKEN_VALUE(token) == Tok_Op_Defined) {

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Defined_Bin_Opr;

      attr_idx = srch_sym_tbl (TOKEN_STR(token), TOKEN_LEN(token), &name_idx);
      host_attr_idx = attr_idx;
      
      if (attr_idx == NULL_IDX) {
         host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token), 
                                           TOKEN_LEN(token),
                                           &host_name_idx,
                                           TRUE);

         if (host_attr_idx) {
            attr_idx = ntr_host_in_sym_tbl(&token,
                                           name_idx,
                                           host_attr_idx,
                                           host_name_idx,
                                           TRUE);

            attr_idx = host_attr_idx;
         }
#ifdef KEY /* Bug 12636 */
         while (AT_ATTR_LINK(host_attr_idx)) {
            host_attr_idx = AT_ATTR_LINK(attr_idx);
         }
#endif /* KEY Bug 12636 */
      }
      else if (AT_ATTR_LINK(attr_idx)) {
         host_attr_idx = AT_ATTR_LINK(attr_idx);
         while (AT_ATTR_LINK(host_attr_idx)) {
            host_attr_idx = AT_ATTR_LINK(attr_idx);
         }
      }

      if (attr_idx == NULL_IDX || AT_OBJ_CLASS(host_attr_idx) != Interface) {

         /* error .. no defined opr */

         PRINTMSG(TOKEN_LINE(token), 318, Error, TOKEN_COLUMN(token),
                  TOKEN_STR(token));
         parsed_ok = FALSE;
      }
      else if (AT_NOT_VISIBLE(host_attr_idx)) {
         PRINTMSG(TOKEN_LINE(token), 486, Error,
                  TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(host_attr_idx),
                  AT_OBJ_NAME_PTR(AT_MODULE_IDX((host_attr_idx))));
         parsed_ok = FALSE;
      }
     
      IR_FLD_L(ir_idx)   = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)   = attr_idx;
         
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
      IR_LINE_NUM(ir_idx)   = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)    = TOKEN_COLUMN(token);


      NTR_IR_LIST_TBL(list1_idx);
      NTR_IR_LIST_TBL(list2_idx);
      IR_FLD_R(ir_idx)            = IL_Tbl_Idx;
      IR_LIST_CNT_R(ir_idx)       = 2;
      IR_IDX_R(ir_idx)            = list1_idx;
      IL_NEXT_LIST_IDX(list1_idx) = list2_idx;
      IL_PREV_LIST_IDX(list2_idx) = list1_idx;

      COPY_OPND(IL_OPND(list1_idx), opnd);
   
      parsed_ok = parse_level_5(&opnd) && parsed_ok; 

      COPY_OPND(IL_OPND(list2_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_expr", NULL);

   return(parsed_ok);
} /* parse_expr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	[defined_unary_op] primary                                    *|
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

boolean parse_level_1(opnd_type *result)

{
   int       attr_idx;
   int       def_idx = NULL_IDX;
   int       host_attr_idx;
   int       host_name_idx;
   int       name_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_level_1", NULL);

   if (LA_CH_VALUE == DOT && matched_specific_token(Tok_Op_Defined, 
                                                    Tok_Class_Op)) {
      /* have defined_unary_op */

      NTR_IR_TBL(def_idx);
      IR_OPR(def_idx) = Defined_Un_Opr;
      attr_idx = srch_sym_tbl (TOKEN_STR(token), TOKEN_LEN(token), &name_idx);
      host_attr_idx = attr_idx;

      if (attr_idx == NULL_IDX) {
         host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token), 
                                           TOKEN_LEN(token),
                                           &host_name_idx,
                                           TRUE);

         if (host_attr_idx) {
            attr_idx = ntr_host_in_sym_tbl(&token,
                                           name_idx,
                                           host_attr_idx,
                                           host_name_idx,
                                           TRUE);

            attr_idx = host_attr_idx;
         }
      }
      else if (AT_ATTR_LINK(attr_idx)) {
         host_attr_idx = AT_ATTR_LINK(attr_idx);
         while (AT_ATTR_LINK(host_attr_idx)) {
            host_attr_idx = AT_ATTR_LINK(attr_idx);
         }
      }

      if (attr_idx == NULL_IDX || AT_OBJ_CLASS(host_attr_idx) != Interface) {

         /* error .. no defined opr */

         PRINTMSG(TOKEN_LINE(token), 318, Error, TOKEN_COLUMN(token),
                  TOKEN_STR(token));
         parsed_ok = FALSE;
      }
      else if (AT_NOT_VISIBLE(host_attr_idx)) {
         PRINTMSG(TOKEN_LINE(token), 486, Error,
                  TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(host_attr_idx),
                  AT_OBJ_NAME_PTR(AT_MODULE_IDX((host_attr_idx))));
         parsed_ok = FALSE;
      }
      else {

         IR_FLD_L(def_idx)   = AT_Tbl_Idx;
         IR_IDX_L(def_idx)   = attr_idx;

         IR_LINE_NUM_L(def_idx) = TOKEN_LINE(token);
         IR_COL_NUM_L(def_idx)  = TOKEN_COLUMN(token);
         IR_LINE_NUM(def_idx)   = TOKEN_LINE(token);
         IR_COL_NUM(def_idx)    = TOKEN_COLUMN(token);
      }
   }

   parsed_ok = parse_operand(&opnd) && parsed_ok;

   if (def_idx) {
      COPY_OPND(IR_OPND_R(def_idx), opnd);
      OPND_FLD((*result)) = IR_Tbl_Idx;
      OPND_IDX((*result)) = def_idx;
   }
   else {
      COPY_OPND((*result), opnd);
   }
   TRACE (Func_Exit, "parse_level_1", NULL);

   return(parsed_ok);
} /* parse_level_1 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	level-1-expr [** mult_opnd]                                   *|
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

boolean parse_mult_opnd(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;


   TRACE (Func_Entry, "parse_mult_opnd", NULL);

   parsed_ok = parse_level_1(&opnd);

   if (MATCHED_TOKEN_CLASS(Tok_Class_Op)) {

      if (TOKEN_VALUE(token) == Tok_Op_Power) {

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)      = Power_Opr;
         IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         parsed_ok = parse_mult_opnd(&opnd) && parsed_ok;

         COPY_OPND(IR_OPND_R(ir_idx), opnd);

         OPND_FLD((*result)) = IR_Tbl_Idx;
         OPND_IDX((*result)) = ir_idx;
      }
      else if (TOKEN_VALUE(token) == Tok_Const_True ||
               TOKEN_VALUE(token) == Tok_Const_False) {
      
         PRINTMSG(TOKEN_LINE(token), 197, Error, TOKEN_COLUMN(token),
                  "operator", "logical literal constant");
         parse_err_flush(Find_Expr_End, NULL);
         parsed_ok = FALSE;
      }
      else if (TOKEN_VALUE(token) == Tok_Op_Assign      ||
               TOKEN_VALUE(token) == Tok_Op_Deref       ||
               TOKEN_VALUE(token) == Tok_Op_Ptr_Assign  ||
               TOKEN_VALUE(token) == Tok_Op_Not)        {

         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
         COPY_OPND((*result),opnd);
      }
      else {
         COPY_OPND((*result),opnd);
      }
   }
   else {
      COPY_OPND((*result),opnd);
   }

   TRACE (Func_Exit, "parse_mult_opnd", NULL);

   return(parsed_ok);
} /* parse_mult_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	mult_opnd {mult-op mult_opnd}                                 *|
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

boolean parse_add_opnd(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_add_opnd", NULL);

   parsed_ok = parse_mult_opnd(&opnd);

   while (TOKEN_VALUE(token) == Tok_Op_Mult || 
          TOKEN_VALUE(token) == Tok_Op_Div) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(token)) {
         case Tok_Op_Mult :
            IR_OPR(ir_idx) = Mult_Opr;
            break;
         case Tok_Op_Div  :
            IR_OPR(ir_idx) = Div_Opr;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);
      
      parsed_ok = parse_mult_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_add_opnd", NULL);

   return(parsed_ok);
} /* parse_add_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	[add-op] add-opnd {add-op add-opnd}                           *|
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

boolean parse_level_2(opnd_type *result)

{
   int       ir_idx = NULL_IDX;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_level_2", NULL);

   if (LA_CH_VALUE == PLUS || LA_CH_VALUE == MINUS) {
      NTR_IR_TBL(ir_idx);
      switch (LA_CH_VALUE) {
         case PLUS  :
            IR_OPR(ir_idx) = Uplus_Opr;
            break;
         case MINUS :
            IR_OPR(ir_idx) = Uminus_Opr;
      }
      IR_LINE_NUM(ir_idx) = LA_CH_LINE;
      IR_COL_NUM(ir_idx)  = LA_CH_COLUMN;
      NEXT_LA_CH;
   }

   parsed_ok = parse_add_opnd(&opnd);
   
   if (ir_idx) {
      COPY_OPND(IR_OPND_L(ir_idx), opnd);
      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   while (TOKEN_VALUE(token) == Tok_Op_Add ||
          TOKEN_VALUE(token) == Tok_Op_Sub) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(token)) {
         case Tok_Op_Add :
            IR_OPR(ir_idx) = Plus_Opr;
            break;
         case Tok_Op_Sub :
            IR_OPR(ir_idx) = Minus_Opr;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_add_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_level_2", NULL);

   return(parsed_ok);
} /* parse_level_2 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	level-2-expr { // level-2-expr }                              *|
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

boolean parse_level_3(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_level_3", NULL);

   parsed_ok = parse_level_2(&opnd);

   while (TOKEN_VALUE(token) == Tok_Op_Concat) {
   
      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Concat_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);
      
      parsed_ok = parse_level_2(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_level_3", NULL);

   return(parsed_ok);
} /* parse_level_3 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	[level-3-expr rel-op] level-3-expr                            *|
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

boolean parse_level_4(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_level_4", NULL);

   parsed_ok = parse_level_3(&opnd);

   if (TOKEN_VALUE(token) == Tok_Op_Eq ||
       TOKEN_VALUE(token) == Tok_Op_Ne ||
       TOKEN_VALUE(token) == Tok_Op_Ge ||
       TOKEN_VALUE(token) == Tok_Op_Gt ||
       TOKEN_VALUE(token) == Tok_Op_Le ||
       TOKEN_VALUE(token) == Tok_Op_Lt ||
       TOKEN_VALUE(token) == Tok_Op_Lg) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(token)) {
         case Tok_Op_Eq :
            IR_OPR(ir_idx) = Eq_Opr;
            break;
         case Tok_Op_Ne :
            IR_OPR(ir_idx) = Ne_Opr;
            break;
         case Tok_Op_Ge :
            IR_OPR(ir_idx) = Ge_Opr;
            break;
         case Tok_Op_Gt :
            IR_OPR(ir_idx) = Gt_Opr;
            break;
         case Tok_Op_Le :
            IR_OPR(ir_idx) = Le_Opr;
            break;
         case Tok_Op_Lt :
            IR_OPR(ir_idx) = Lt_Opr;
            break;
         case Tok_Op_Lg :
            IR_OPR(ir_idx) = Lg_Opr;
            PRINTMSG(TOKEN_LINE(token), 1243, Ansi, TOKEN_COLUMN(token));
            break;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_level_3(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_level_4", NULL);

   return(parsed_ok);
} /* parse_level_4 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	[not-op] level-4-expr                                         *|
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

boolean parse_and_opnd(opnd_type *result)

{
   int	     buf_idx;
   int       i;
   int       ir_idx = NULL_IDX;
   char      op[8];
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;
   int       stmt_num;


   TRACE (Func_Entry, "parse_and_opnd", NULL);

   if (LA_CH_VALUE == DOT) {
      buf_idx = LA_CH_BUF_IDX;
      stmt_num = LA_CH_STMT_NUM;

      NEXT_LA_CH;

      for (i = 0; i < 4; i++) {
         op[i] = LA_CH_VALUE;

         if (LA_CH_VALUE == DOT ||
             LA_CH_VALUE == EOS) {
            break;
         }
         NEXT_LA_CH;
      }

      reset_lex(buf_idx, stmt_num);

      if (((i == 1 && strncmp(op, "N.", 2) == 0)    ||
           (i == 3 && strncmp(op, "NOT.", 4) == 0))    &&
          matched_specific_token(Tok_Op_Not, Tok_Class_Op)) {

         NTR_IR_TBL(ir_idx);
         OPND_FLD((*result)) = IR_Tbl_Idx;
         OPND_IDX((*result)) = ir_idx;
         IR_OPR(ir_idx)      = Not_Opr;
         IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);
      }
   }

   parsed_ok = parse_level_4(&opnd);

   if (ir_idx) {
      COPY_OPND(IR_OPND_L(ir_idx), opnd);
   }
   else {
      COPY_OPND((*result), opnd);
   }

   TRACE (Func_Exit, "parse_and_opnd", NULL);

   return(parsed_ok);
} /* parse_and_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	and_opnd { and_op and_opnd }                                  *|
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

boolean parse_or_opnd(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_or_opnd", NULL);

   parsed_ok = parse_and_opnd(&opnd);

   while (TOKEN_VALUE(token) == Tok_Op_And) {

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = And_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);
  
      parsed_ok = parse_and_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_or_opnd", NULL);

   return(parsed_ok);
} /* parse_or_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	or_opnd { or_op or_opnd }                                     *|
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

boolean parse_equiv_opnd(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_equiv_opnd", NULL);

   parsed_ok = parse_or_opnd(&opnd);

   while (TOKEN_VALUE(token) == Tok_Op_Or) {
      
      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Or_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_or_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_equiv_opnd", NULL);

   return(parsed_ok);
} /* parse_equiv_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF	equiv-opnd { equiv-op equiv-opnd }                            *|
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

boolean parse_level_5(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd = INIT_OPND_TYPE;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_level_5", NULL);

   parsed_ok = parse_equiv_opnd(&opnd);

   while (TOKEN_VALUE(token) == Tok_Op_Eqv   ||
          TOKEN_VALUE(token) == Tok_Op_Neqv) {
      
      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(token)) {
         case Tok_Op_Eqv  :
            IR_OPR(ir_idx) = Eqv_Opr;
            break;
         case Tok_Op_Neqv :
            IR_OPR(ir_idx) = Neqv_Opr;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_equiv_opnd(&opnd) && parsed_ok; 

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_level_5", NULL);

   return(parsed_ok);
} /* parse_level_5 */


#ifdef KEY /* Bug 8004 */
/*
 * Do the parsing of an array constructor.
 * parsed_ok	Indicates whether we have encountered an error so far; we
 *		update this.
 * the_opnd	Operand to be created by parsing
 */
static void do_array_constructor(boolean *parsed_ok, opnd_type *the_opnd) {
  int ir_idx = NULL_IDX;
  opnd_type opnd = INIT_OPND_TYPE;

  NTR_IR_TBL(ir_idx);
  IR_OPR(ir_idx) = Array_Construct_Opr;
  IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
  IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);
  OPND_FLD((*the_opnd)) = IR_Tbl_Idx;
  OPND_IDX((*the_opnd)) = ir_idx;

  boolean save_in_constructor = in_constructor;
  in_constructor = TRUE;
  *parsed_ok = parse_io_list(&opnd) && *parsed_ok;
  in_constructor = save_in_constructor;

  COPY_OPND(IR_OPND_R(ir_idx), opnd);
}

#endif /* KEY Bug 8004 */
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
boolean parse_operand (opnd_type *the_opnd)

{
   opnd_type		cmplx_opnd 	= INIT_OPND_TYPE;
   int			cmplx_lin_type;
   int			cmplx_dcl_val;
   int			cmplx_desc;
   int			col;
   int			cx_l		= NULL_IDX;
   int			cx_r		= NULL_IDX;
   long_type		constant[MAX_WORDS_FOR_NUMERIC];
   expr_arg_type	exp_desc;
   int			ir_idx;
   int			line;
   int			list_idx;
   int			list2_idx;
   opnd_type		opnd 		= INIT_OPND_TYPE;
   boolean		parsed_ok	= TRUE;
   boolean		save_in_constructor;
   int			type_idx;
   int			type_l;
   int			type_r;


   TRACE (Func_Entry, "parse_operand", NULL);

   if (LA_CH_VALUE == LPAREN && matched_specific_token(Tok_Punct_Lparen,
                                                       Tok_Class_Punct)) {

      line = TOKEN_LINE(token);
      col  = TOKEN_COLUMN(token);

      if (!parse_expr(the_opnd)) {
         parsed_ok = FALSE;
      }
      else if (LA_CH_VALUE == RPAREN) {
         /* insert paren_opr */
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Paren_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), (*the_opnd));
         OPND_FLD((*the_opnd)) = IR_Tbl_Idx;
         OPND_IDX((*the_opnd)) = ir_idx;
         IR_LINE_NUM(ir_idx)   = line;
         IR_COL_NUM(ir_idx)    = col;
         
         NEXT_LA_CH;
         goto EXIT;
      }
      /* Assume complex constant - Try to fold */
      else if (OPND_FLD((*the_opnd)) == CN_Tbl_Idx) {
         cx_l = OPND_IDX((*the_opnd));
      }
      else if (OPND_FLD((*the_opnd))               == AT_Tbl_Idx &&
               AT_OBJ_CLASS(OPND_IDX((*the_opnd))) == Data_Obj   &&
               ATD_CLASS(OPND_IDX((*the_opnd)))    == Constant   &&
               ATD_FLD(OPND_IDX((*the_opnd)))      == CN_Tbl_Idx) {

         cx_l = ATD_CONST_IDX(OPND_IDX((*the_opnd)));
      }
      else if (OPND_FLD((*the_opnd))               == IR_Tbl_Idx  &&
               (IR_OPR(OPND_IDX((*the_opnd)))      == Uplus_Opr ||
                IR_OPR(OPND_IDX((*the_opnd)))      == Uminus_Opr) &&
               (IR_FLD_L(OPND_IDX((*the_opnd)))    == CN_Tbl_Idx ||
                (IR_FLD_L(OPND_IDX((*the_opnd)))    == AT_Tbl_Idx &&
                 AT_OBJ_CLASS(IR_IDX_L(OPND_IDX((*the_opnd)))) == Data_Obj &&
                 ATD_CLASS(IR_IDX_L(OPND_IDX((*the_opnd))))    == Constant &&
                 ATD_FLD(IR_IDX_L(OPND_IDX((*the_opnd)))) == CN_Tbl_Idx))) {

         exp_desc.rank = 0;
         xref_state    = CIF_No_Usage_Rec;
         comp_gen_expr = TRUE;
         parsed_ok = expr_semantics(the_opnd, &exp_desc);
         comp_gen_expr = FALSE;

         if (OPND_FLD((*the_opnd)) == CN_Tbl_Idx) {
            cx_l = OPND_IDX((*the_opnd));
         }
      }

      if (cx_l                            &&
          (TYP_TYPE(CN_TYPE_IDX(cx_l)) == Real ||
           TYP_TYPE(CN_TYPE_IDX(cx_l)) == Integer) &&
          LA_CH_VALUE == COMMA                             ) {  
         /* Have complex const */
         NEXT_LA_CH;
            
         if (!parse_expr(&cmplx_opnd)) {
            parsed_ok = FALSE;
         }
         else {

            if (OPND_FLD(cmplx_opnd) == CN_Tbl_Idx) {
               cx_r = OPND_IDX(cmplx_opnd);
            }
            else if (OPND_FLD(cmplx_opnd)               == AT_Tbl_Idx &&
                     AT_OBJ_CLASS(OPND_IDX(cmplx_opnd)) == Data_Obj   &&
                     ATD_CLASS(OPND_IDX(cmplx_opnd))    == Constant   &&
                     ATD_FLD(OPND_IDX(cmplx_opnd))      == CN_Tbl_Idx) {

               cx_r = ATD_CONST_IDX(OPND_IDX(cmplx_opnd));
            }
            else if (OPND_FLD(cmplx_opnd)               == IR_Tbl_Idx  &&
                     (IR_OPR(OPND_IDX(cmplx_opnd))      == Uplus_Opr ||
                      IR_OPR(OPND_IDX(cmplx_opnd))      == Uminus_Opr) &&
                     (IR_FLD_L(OPND_IDX(cmplx_opnd))    == CN_Tbl_Idx ||
                      (IR_FLD_L(OPND_IDX(cmplx_opnd))    == AT_Tbl_Idx &&
                       AT_OBJ_CLASS(IR_IDX_L(OPND_IDX(cmplx_opnd)))==Data_Obj &&
                       ATD_CLASS(IR_IDX_L(OPND_IDX(cmplx_opnd))) == Constant &&
                       ATD_FLD(IR_IDX_L(OPND_IDX(cmplx_opnd))) == CN_Tbl_Idx))){

               exp_desc.rank = 0;
               xref_state    = CIF_No_Usage_Rec;
               comp_gen_expr = TRUE;
               parsed_ok = expr_semantics(&cmplx_opnd, &exp_desc);
               comp_gen_expr = FALSE;
      
               if (OPND_FLD(cmplx_opnd) == CN_Tbl_Idx) {
                  cx_r = OPND_IDX(cmplx_opnd);
               }
            }


            if (cx_r                               &&
                (TYP_TYPE(CN_TYPE_IDX(cx_r)) == Real ||
                 TYP_TYPE(CN_TYPE_IDX(cx_r)) == Integer)) {
               type_r = CN_TYPE_IDX(cx_r);
               type_l = CN_TYPE_IDX(cx_l);
           
               if (TYP_TYPE(type_l) == Real && 
                   TYP_TYPE(type_r) == Real) {

                  if (TYP_LINEAR(type_l) > TYP_LINEAR(type_r)) {
                     cmplx_lin_type = TYP_LINEAR(type_l);
                     cmplx_dcl_val  = TYP_DCL_VALUE(type_l);
                     cmplx_desc     = TYP_DESC(type_l);
                  }
                  else {
                     cmplx_lin_type = TYP_LINEAR(type_r);
                     cmplx_dcl_val  = TYP_DCL_VALUE(type_r);
                     cmplx_desc     = TYP_DESC(type_r);
                  }
               }
               else if (TYP_TYPE(type_l) == Real     && 
                        TYP_TYPE(type_r) == Integer) {
                  cmplx_lin_type = TYP_LINEAR(type_l);
                  cmplx_dcl_val  = TYP_DCL_VALUE(type_l);
                  cmplx_desc     = TYP_DESC(type_l);

               }
               else if (TYP_TYPE(type_l) == Integer && 
                        TYP_TYPE(type_r) == Real)   {
                  cmplx_lin_type = TYP_LINEAR(type_r);
                  cmplx_dcl_val  = TYP_DCL_VALUE(type_r);
                  cmplx_desc     = TYP_DESC(type_r);

               } 
               else { /* both integer */
                  cmplx_lin_type = REAL_DEFAULT_TYPE;
                  cmplx_dcl_val  = 0;
                  cmplx_desc     = 0;
               }

               type_idx = cmplx_lin_type;
               parsed_ok = folder_driver((char *)&CN_CONST(cx_l),
                                         type_l,
                                         NULL,
                                         NULL_IDX,
                                         constant,
                                         &type_idx,
                                          line,
                                          col,
                                          1,
                                          Cvrt_Opr) && parsed_ok;

               type_idx = cmplx_lin_type;
               parsed_ok = folder_driver((char *)&CN_CONST(cx_r),
                                         type_r,
                                         NULL,
                                         NULL_IDX,
                                    &(constant[num_host_wds[cmplx_lin_type]]),
                                         &type_idx,
                                          line,
                                          col,
                                          1,
                                          Cvrt_Opr) && parsed_ok;

               switch(cmplx_lin_type) {
                  case Real_4 :
                     cmplx_lin_type = Complex_4;
# if defined(_WHIRL_HOST64_TARGET64)
                     {
                       float *p = (float *)&constant;
                       p[1] = p[2];
                     }
# endif
                     break;

                  case Real_8 :
                     cmplx_lin_type = Complex_8;
                     break;

                  case Real_16 :
                     cmplx_lin_type = Complex_16;
                     break;

               }

               OPND_FLD((*the_opnd)) = CN_Tbl_Idx;

               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
               TYP_TYPE(TYP_WORK_IDX)	= Complex;
               TYP_LINEAR(TYP_WORK_IDX)	= (linear_type_type) cmplx_lin_type;
               TYP_DCL_VALUE(TYP_WORK_IDX)	= cmplx_dcl_val;
               TYP_DESC(TYP_WORK_IDX)		= (type_desc_type) cmplx_desc;
               type_idx				= ntr_type_tbl();

               OPND_IDX((*the_opnd)) = ntr_const_tbl(type_idx, 
                                                     FALSE,
                                                     constant);
            }
            else {
               parse_err_flush(Find_Rparen, "CONSTANT");
               parsed_ok = FALSE;
            }
         }
      }

      if (LA_CH_VALUE == RPAREN) {
         NEXT_LA_CH;
      }
      else if (parse_err_flush(Find_Rparen, ")")) {
         NEXT_LA_CH;
         parsed_ok = FALSE;
      }
      else {
         parsed_ok = FALSE;
      }
      goto EXIT;
   }
   else if (LA_CH_CLASS == Ch_Class_Digit  ||
            LA_CH_CLASS == Ch_Class_Letter ||
            LA_CH_VALUE == DOT             ||
            LA_CH_VALUE == QUOTE           ||
            LA_CH_VALUE == DBL_QUOTE)      {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Opnd)) {
         OPND_LINE_NUM((*the_opnd)) = TOKEN_LINE(token);
         OPND_COL_NUM((*the_opnd))  = TOKEN_COLUMN(token);
         OPND_FLD((*the_opnd))	    = CN_Tbl_Idx;

         switch (TOKEN_VALUE(token)) {

            case Tok_Id :

            if (! parse_deref(the_opnd, NULL_IDX)) {
               parsed_ok = FALSE;
            }
            break;

            case Tok_Const_Char :
            
               if (LA_CH_VALUE == LPAREN && is_substring_ref ()) {
                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx)              = Substring_Opr;
                  IR_LINE_NUM(ir_idx)         = LA_CH_LINE;
                  IR_COL_NUM(ir_idx)          = LA_CH_COLUMN;
                  OPND_FLD((*the_opnd))       = IR_Tbl_Idx;
                  OPND_IDX((*the_opnd))       = ir_idx;
                  IR_FLD_L(ir_idx)            = CN_Tbl_Idx;
                  IR_IDX_L(ir_idx)            = TOKEN_CONST_TBL_IDX(token);
                  IR_LINE_NUM_L(ir_idx)       = TOKEN_LINE(token);
                  IR_COL_NUM_L(ir_idx)        = TOKEN_COLUMN(token);
   
                  IR_FLD_R(ir_idx)            = IL_Tbl_Idx;
                  IR_LIST_CNT_R(ir_idx)       = 2;
                  NTR_IR_LIST_TBL(list_idx);
                  NTR_IR_LIST_TBL(list2_idx);
                  IR_IDX_R(ir_idx)            = list_idx;
                  IL_NEXT_LIST_IDX(list_idx)  = list2_idx;
                  IL_PREV_LIST_IDX(list2_idx) = list_idx;
   
                  /* consume ( */
                  NEXT_LA_CH;
      
                  if (LA_CH_VALUE != COLON) {
                     parsed_ok = parse_expr(&opnd) && parsed_ok;
                     COPY_OPND(IL_OPND(list_idx), opnd);
                  }
      
                  if (LA_CH_VALUE != COLON) {
                     if (parse_err_flush(Find_Rparen, ":")) {
                        NEXT_LA_CH;
                     }
                     parsed_ok = FALSE;
                     goto EXIT;
                  }
                  else {
                     NEXT_LA_CH;
                  }
                  if (LA_CH_VALUE != RPAREN) {
                     parsed_ok = parse_expr(&opnd) && parsed_ok;
                     COPY_OPND(IL_OPND(list2_idx), opnd);
                  }
      
                  if (LA_CH_VALUE != RPAREN) {
      
                     if (parse_err_flush(Find_Rparen, ")")) {
                        NEXT_LA_CH;
                     }
                     parsed_ok = FALSE;
                  }
                  else {
                     NEXT_LA_CH;
                  }
               }
               else {
                  OPND_IDX((*the_opnd))	= TOKEN_CONST_TBL_IDX(token);
               }
               break;
   
            case Tok_Const_Hollerith :
            case Tok_Const_Boolean   :
            case Tok_Const_Boz :
            case Tok_Const_Int :
            case Tok_Const_Real :
            case Tok_Const_Dbl :
            case Tok_Const_Quad :
            case Tok_Const_False :
            case Tok_Const_True :
   
               OPND_IDX((*the_opnd)) = TOKEN_CONST_TBL_IDX(token);
               break;
         }
      }
      else if (TOKEN_VALUE(token) == Tok_Unknown) {
         parsed_ok = FALSE;
         parse_err_flush(Find_Expr_End, parse_operand_insert);
      }
      else {
         parsed_ok = FALSE;
         parse_err_flush(Find_Expr_End, NULL);
      }
   }
#ifdef KEY /* Bug 8004 */
   else if (LA_CH_VALUE == LBRKT) {
      NEXT_LA_CH;

      do_array_constructor(&parsed_ok, the_opnd);

      if (LA_CH_VALUE == RBRKT) {
         NEXT_LA_CH;
      }
      else {
         parse_err_flush(Find_EOS, "]");
         parsed_ok = FALSE;
      }
   }
#endif /* KEY Bug 8004 */
   else if (LA_CH_VALUE == LPAREN && matched_specific_token(Tok_Punct_Lbrkt,
                                                            Tok_Class_Punct)) {

      do_array_constructor(&parsed_ok, the_opnd);

      if (LA_CH_VALUE == SLASH && matched_specific_token(Tok_Punct_Rbrkt,
                                                         Tok_Class_Punct)) {

         /* intentionally blank */
      }
      else {
         parse_err_flush(Find_EOS, "/)");
         parsed_ok = FALSE;
      }
   }
   else {
      parsed_ok = FALSE;
      parse_err_flush(Find_Expr_End, parse_operand_insert);

      if (LA_CH_VALUE == EOS) {
         TOKEN_STR_WD(token, 0)   = 0;
         TOKEN_VALUE(token)       = Tok_EOS;
         TOKEN_KIND_STR(token)[0] = EOS;
         TOKEN_KIND_LEN(token)    = 0;
         TOKEN_LEN(token)         = 0;
         TOKEN_LINE(token)        = LA_CH_LINE;
         TOKEN_COLUMN(token)      = LA_CH_COLUMN;
      }
   }

EXIT:
   TRACE (Func_Exit, "parse_operand", NULL);

   return(parsed_ok);

}  /* parse_operand */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parse structure/array dereference on lhs of assignment.               *|
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
\******************************************************************************/

boolean parse_lhs (opnd_type *result_opnd,
                   int        attr_idx)

{

   int           array_idx;
   int           amb_attr_idx;
   token_type    attr_name;
   int           col;
   int           ir_idx;
   int           line;
   int           list_idx;
   int           list2_idx;
   int           list3_idx;
   opnd_type     opnd = INIT_OPND_TYPE;
   boolean       parsed_ok = TRUE;
   int           rank;
   int           subs_idx = NULL_IDX;
   int           substring_idx;
   int           trip_idx;


   TRACE (Func_Entry, "parse_lhs", NULL);

   attr_name = token;

   amb_attr_idx = attr_idx;

   while (AT_ATTR_LINK(amb_attr_idx)) {
      amb_attr_idx = AT_ATTR_LINK(amb_attr_idx);
   }

   /* if local attr has problem, quit */

   if (AT_DCL_ERR(attr_idx)) {
      SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;

      parse_err_flush(Find_Ref_End, NULL);
      parsed_ok = FALSE;
      goto EXIT;
   }

   /* Now lets see what this attr is */

   switch (AT_OBJ_CLASS(amb_attr_idx)) {
      case Data_Obj :

         break;

      case Pgm_Unit :
         /* must be result var of local or host function, not interface or */
         /* other external                                                 */

         if (ATP_PGM_UNIT(amb_attr_idx) == Function  &&
             ATP_SCP_ALIVE(amb_attr_idx))            {

            if (ATP_RSLT_NAME(amb_attr_idx)) {

               /* error .. assigned function name not result name */

               PRINTMSG(TOKEN_LINE(token), 299, Error, 
                        TOKEN_COLUMN(token));
               parse_err_flush(Find_Ref_End, NULL);
               parsed_ok = FALSE;
               goto EXIT;
            }
            else {
               attr_idx = ATP_RSLT_IDX(amb_attr_idx);
               amb_attr_idx = attr_idx;
            }
         }
         else {

            if (AT_NOT_VISIBLE(amb_attr_idx)) {
               PRINTMSG(TOKEN_LINE(token), 486, Error, 
                        TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(amb_attr_idx),
                        AT_OBJ_NAME_PTR(AT_MODULE_IDX((amb_attr_idx))));
            }
            else {  /* can't assign to pgm unit other than current function */
               PRINTMSG(TOKEN_LINE(token), 281, Error, 
                        TOKEN_COLUMN(token));
            }
            parsed_ok = FALSE;
            parse_err_flush(Find_Ref_End, NULL);
            goto EXIT;
         }

         break;

      default       :

         if (AT_NOT_VISIBLE(amb_attr_idx)) {
            PRINTMSG(TOKEN_LINE(token), 486, Error, 
                     TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(amb_attr_idx),
                     AT_OBJ_NAME_PTR(AT_MODULE_IDX((amb_attr_idx))));
         }
         else {  /* can't assign this to a program unit */
            PRINTMSG(TOKEN_LINE(token), 281, Error, 
                     TOKEN_COLUMN(token));
         }

         parsed_ok = FALSE;
         parse_err_flush(Find_Ref_End, NULL);
         goto EXIT;
   }

   OPND_FLD((*result_opnd))      = AT_Tbl_Idx;
   OPND_IDX((*result_opnd))      = attr_idx;
   OPND_LINE_NUM((*result_opnd)) = TOKEN_LINE(token);
   OPND_COL_NUM((*result_opnd))  = TOKEN_COLUMN(token);

# ifdef _F_MINUS_MINUS
   if (LA_CH_VALUE != PERCENT && LA_CH_VALUE != LPAREN && 
       ((! cmd_line_flags.co_array_fortran) || LA_CH_VALUE != LBRKT))
# else
   if (LA_CH_VALUE != PERCENT && LA_CH_VALUE != LPAREN)
# endif
                                                       {
      goto EXIT;
   }


   if (LA_CH_VALUE == LPAREN) {
      /* do that array stuff */
      array_idx = ATD_ARRAY_IDX(amb_attr_idx);

      if (array_idx) {

         rank = 0;
         NTR_IR_TBL(subs_idx);
         IR_FLD_L(subs_idx)            = AT_Tbl_Idx;
         IR_IDX_L(subs_idx)            = attr_idx;
         IR_LINE_NUM_L(subs_idx)       = TOKEN_LINE(token);
         IR_COL_NUM_L(subs_idx)        = TOKEN_COLUMN(token);

         /* LA_CH is '(' */
         IR_LINE_NUM(subs_idx)         = LA_CH_LINE;
         IR_COL_NUM(subs_idx)          = LA_CH_COLUMN;

         IR_OPR(subs_idx)              = Subscript_Opr;
         IR_FLD_R(subs_idx)            = IL_Tbl_Idx;

         list_idx = NULL_IDX;

         do {
            NEXT_LA_CH;

            if (list_idx == NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);
               IR_IDX_R(subs_idx) = list_idx;
            }
            else {
               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }

            if (LA_CH_VALUE != COLON) {
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list_idx), opnd);
            }

            /* do some text stuff here */

            if (LA_CH_VALUE == COLON) {
               NTR_IR_TBL(trip_idx);
               IR_LINE_NUM(trip_idx)       = LA_CH_LINE;
               IR_COL_NUM(trip_idx)        = LA_CH_COLUMN;

               NEXT_LA_CH;

               IR_OPR(trip_idx)            = Triplet_Opr;
               IR_FLD_L(trip_idx)          = IL_Tbl_Idx;
               IR_LIST_CNT_L(trip_idx)     = 3;
               NTR_IR_LIST_TBL(list2_idx);
               IR_IDX_L(trip_idx)          = list2_idx;
               IL_OPND(list2_idx)          = IL_OPND(list_idx);
               IL_FLD(list_idx)            = IR_Tbl_Idx;
               IL_IDX(list_idx)            = trip_idx;
               NTR_IR_LIST_TBL(list3_idx);
               IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
               IL_PREV_LIST_IDX(list3_idx) = list2_idx;

               if (LA_CH_VALUE != COLON && 
                   LA_CH_VALUE != COMMA &&
                   LA_CH_VALUE != RPAREN) {
                  parsed_ok = parse_expr(&opnd) && parsed_ok;
                  COPY_OPND(IL_OPND(list3_idx), opnd);
               }

               NTR_IR_LIST_TBL(list2_idx);
               IL_NEXT_LIST_IDX(list3_idx) = list2_idx;
               IL_PREV_LIST_IDX(list2_idx) = list3_idx;

               if (LA_CH_VALUE == COLON) {
                  NEXT_LA_CH;
                  parsed_ok = parse_expr(&opnd) && parsed_ok;
                  COPY_OPND(IL_OPND(list2_idx), opnd);
               }
            }
            rank++;
         }
         while (LA_CH_VALUE == COMMA);

         if (! matched_specific_token(Tok_Punct_Rparen, Tok_Class_Punct)) {
            if (parse_err_flush(Find_Rparen, ")")) {
               NEXT_LA_CH;
            }
            parsed_ok = FALSE;
            goto EXIT;
         }

         IR_LIST_CNT_R(subs_idx) = rank;

      } /* if (array_idx) */

      /* now check for possible substring reference */

      if (LA_CH_VALUE == LPAREN) {

         if (is_substring_ref ()) {

            if (TYP_TYPE(ATD_TYPE_IDX(amb_attr_idx)) != Character) {

               PRINTMSG(TOKEN_LINE(token), 508, Error, TOKEN_COLUMN(token));
               parsed_ok = FALSE;
               parse_err_flush(Find_Ref_End, NULL);
               goto EXIT;
            }

            NTR_IR_TBL(substring_idx);
            IR_OPR(substring_idx)             = Substring_Opr;
            IR_LINE_NUM(substring_idx)        = LA_CH_LINE;
            IR_COL_NUM(substring_idx)         = LA_CH_COLUMN;
            OPND_FLD((*result_opnd))          = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))          = substring_idx;

            if (subs_idx) {
               IR_FLD_L(substring_idx)        = IR_Tbl_Idx;
               IR_IDX_L(substring_idx)        = subs_idx;

            }
            else {
               IR_FLD_L(substring_idx)        = AT_Tbl_Idx;
               IR_IDX_L(substring_idx)        = attr_idx;
               IR_LINE_NUM_L(substring_idx)   = TOKEN_LINE(token);
               IR_COL_NUM_L(substring_idx)    = TOKEN_COLUMN(token);
            }

            IR_FLD_R(substring_idx)           = IL_Tbl_Idx;
            IR_LIST_CNT_R(substring_idx)      = 2;
            NTR_IR_LIST_TBL(list_idx);
            NTR_IR_LIST_TBL(list2_idx);
            IR_IDX_R(substring_idx)           = list_idx;
            IL_NEXT_LIST_IDX(list_idx)        = list2_idx;
            IL_PREV_LIST_IDX(list2_idx)       = list_idx;

            /* consume ( */
            NEXT_LA_CH;

            if (LA_CH_VALUE != COLON) {
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list_idx), opnd); 
            }

            if (LA_CH_VALUE != COLON) {
               if (parse_err_flush(Find_Rparen, ":")) {
                  NEXT_LA_CH;
               }
               parsed_ok = FALSE;
               goto EXIT;
            }
            else {
               NEXT_LA_CH;
            }

            if (LA_CH_VALUE != RPAREN) {
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list2_idx), opnd); 
            }

            if (LA_CH_VALUE != RPAREN) {

               if (parse_err_flush(Find_Rparen, ")")) {
                  NEXT_LA_CH;
               }
               parsed_ok = FALSE;
               goto EXIT;
            }
            else {
               NEXT_LA_CH;
            }
            goto EXIT;
         }
      }

      if (LA_CH_VALUE != PERCENT) {

         if (subs_idx) {
            OPND_FLD((*result_opnd))       = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))       = subs_idx;
         }
         else {

            OPND_FLD((*result_opnd))      = AT_Tbl_Idx;
            OPND_IDX((*result_opnd))      = attr_idx;
            OPND_LINE_NUM((*result_opnd)) = TOKEN_LINE(attr_name);
            OPND_COL_NUM((*result_opnd))  = TOKEN_COLUMN(attr_name);

         }
      }
   } /* if (LA_CH_VALUE == LPAREN) */

# ifdef _F_MINUS_MINUS
   if (cmd_line_flags.co_array_fortran &&
       LA_CH_VALUE == LBRKT &&
       AT_OBJ_CLASS(amb_attr_idx) == Data_Obj) {

      if (ATD_PE_ARRAY_IDX(amb_attr_idx) == NULL_IDX) {
         /* not declared with pe dimensions */
         PRINTMSG(LA_CH_LINE, 1245, Error, LA_CH_COLUMN,
                  AT_OBJ_NAME_PTR(amb_attr_idx));
         parsed_ok = FALSE;
         parse_err_flush(Find_Ref_End, NULL);
         goto EXIT;
      }

      if (subs_idx == NULL_IDX) {
         NTR_IR_TBL(subs_idx);

         /* LA_CH is '[' */
         IR_LINE_NUM(subs_idx)         = LA_CH_LINE;
         IR_COL_NUM(subs_idx)          = LA_CH_COLUMN;

         IR_OPR(subs_idx)              = Subscript_Opr;
         IR_FLD_R(subs_idx)            = IL_Tbl_Idx;
         IR_LIST_CNT_R(subs_idx)       = 0;

         if (OPND_FLD((*result_opnd)) == AT_Tbl_Idx) {
            COPY_OPND(IR_OPND_L(subs_idx), (*result_opnd));

            /* put subs_idx into result opnd for now */
            OPND_FLD((*result_opnd))      = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))      = subs_idx;
         }
         else if (OPND_FLD((*result_opnd)) == IR_Tbl_Idx &&
                  IR_OPR(OPND_IDX((*result_opnd))) == Substring_Opr) {

            COPY_OPND(IR_OPND_L(subs_idx), IR_OPND_L(OPND_IDX((*result_opnd))));

            IR_FLD_L(OPND_IDX((*result_opnd))) = IR_Tbl_Idx;
            IR_IDX_L(OPND_IDX((*result_opnd))) = subs_idx;
         }
# ifdef _DEBUG
         else {
            PRINTMSG(LA_CH_LINE, 626, Internal, LA_CH_COLUMN,
                     "AT_Tbl_Idx", "parse_deref");
         }
# endif

         list_idx = NULL_IDX;
      }
      else {

         list_idx = IR_IDX_R(subs_idx);

         while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }

      do {
         NEXT_LA_CH;

         if (list_idx == NULL_IDX) {
            NTR_IR_LIST_TBL(list_idx);
            IR_IDX_R(subs_idx) = list_idx;
         }
         else {
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         IL_PE_SUBSCRIPT(list_idx) = TRUE;

         if (LA_CH_VALUE != COLON) {
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list_idx), opnd);
         }

         /* do some text stuff here */

         if (LA_CH_VALUE == COLON) {

            NTR_IR_TBL(trip_idx);
            IR_LINE_NUM(trip_idx)       = LA_CH_LINE;
            IR_COL_NUM(trip_idx)        = LA_CH_COLUMN;

            NEXT_LA_CH;

            IR_OPR(trip_idx)            = Triplet_Opr;
            IR_FLD_L(trip_idx)          = IL_Tbl_Idx;
            IR_LIST_CNT_L(trip_idx)     = 3;
            NTR_IR_LIST_TBL(list2_idx);
            IR_IDX_L(trip_idx)          = list2_idx;
            IL_OPND(list2_idx)          = IL_OPND(list_idx);
            IL_FLD(list_idx)            = IR_Tbl_Idx;
            IL_IDX(list_idx)            = trip_idx;
            NTR_IR_LIST_TBL(list3_idx);
            IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
            IL_PREV_LIST_IDX(list3_idx) = list2_idx;

            if (LA_CH_VALUE != COLON &&
                LA_CH_VALUE != COMMA &&
                LA_CH_VALUE != RBRKT) {
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list3_idx), opnd);
            }

            NTR_IR_LIST_TBL(list2_idx);
            IL_NEXT_LIST_IDX(list3_idx) = list2_idx;
            IL_PREV_LIST_IDX(list2_idx) = list3_idx;

            if (LA_CH_VALUE == COLON) {
               NEXT_LA_CH;
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list2_idx), opnd);
            }
         }
         (IR_LIST_CNT_R(subs_idx))++;
      }
      while (LA_CH_VALUE == COMMA);

      if (LA_CH_VALUE != RBRKT) {
         parse_err_flush(Find_EOS, "]");
         parsed_ok = FALSE;
         goto EXIT;
      }
      else {
         /* swallow ] */
         NEXT_LA_CH;
      }
   }
# endif

   if (LA_CH_VALUE == PERCENT) {

      /* first see if attr_idx is a structure */

      if (TYP_TYPE(ATD_TYPE_IDX(amb_attr_idx)) != Structure) {

         if (SCP_IMPL_NONE(curr_scp_idx) && !AT_TYPED(amb_attr_idx) &&
             !AT_DCL_ERR(amb_attr_idx)) {
            AT_DCL_ERR(amb_attr_idx)	= TRUE;
            PRINTMSG(TOKEN_LINE(attr_name), 113, Error,
                     TOKEN_COLUMN(attr_name),
                     TOKEN_STR(attr_name));
         }
         else {
            PRINTMSG(TOKEN_LINE(attr_name), 212, Error,
                     TOKEN_COLUMN(attr_name), 
                     TOKEN_STR(attr_name),
                     get_basic_type_str(ATD_TYPE_IDX(amb_attr_idx)));
         }

         parse_err_flush(Find_Ref_End, NULL);
         parsed_ok = FALSE;
         goto EXIT;
      }
      line = LA_CH_LINE;
      col = LA_CH_COLUMN;
      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)      = Struct_Opr;
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx)  = col;

         if (subs_idx) {
            IR_FLD_L(ir_idx)            = IR_Tbl_Idx;
            IR_IDX_L(ir_idx)            = subs_idx;
         }
         else {
            IR_FLD_L(ir_idx) = AT_Tbl_Idx;
            IR_IDX_L(ir_idx) = attr_idx;

            IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(attr_name);
            IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(attr_name);
         }

         OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
         OPND_IDX((*result_opnd)) = ir_idx;

         parsed_ok = parse_deref(result_opnd, 
                                 TYP_IDX(ATD_TYPE_IDX(amb_attr_idx)));

      }
      else {
         /* no ID after %, must be an error */
         parse_err_flush(Find_Ref_End, "IDENTIFIER");
         parsed_ok = FALSE;
      }
   }

EXIT:

   if (parsed_ok) {

      if (ATD_CLASS(amb_attr_idx) == Function_Result) {
         AT_DEFINED(ATD_FUNC_IDX(amb_attr_idx)) = TRUE;
      }
      else if (ATD_CLASS(amb_attr_idx) == Atd_Unknown) {
         ATD_CLASS(amb_attr_idx)	= Variable;
      }

      AT_DEFINED(attr_idx) = TRUE;
   }

   TRACE (Func_Exit, "parse_lhs", NULL);

   return(parsed_ok);

} /* parse_lhs */
