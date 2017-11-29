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



static char USMID[] = "\n@(#)5.0_pl/sources/p_dcl_util.c	5.7	10/28/99 10:03:56\n";

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

static	int	ntr_bnds_tmp_list(opnd_type *);
#ifndef KEY /* Bug 10572 */
static	boolean	parse_int_spec_expr(long *, fld_type *, boolean, boolean);
#endif /* KEY Bug 10572 */
static	void	parse_kind_selector(void);
static	boolean	is_attr_referenced_in_bound(int, int);


static	boolean	kind0seen;
static	boolean	kind0E0seen;
static	boolean	kind0D0seen;
static	boolean	kindconstseen;

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Parses the array_spec for declarations                                *|
|*             array_spec is explicit-shape-spec-list                         *|
|*                            is [lower-bound :]upper-bound                   *|
|*                               [specification-expr :] specification-expr    *|
|*                       is assumed-shape-spec-list                           *|
|*                          is [lower-bound] :                                *|
|*                       is deferred-shape-spec-list                          *|
|*                          is :                                              *|
|*                       is assumed-size-spec                                 *|
|*                          is [explicit-shape-spec-list,] [lower-bound:]*    *|
|*                                                                            *|
|*       Position  - entry - token is open paren                              *|
|*                   exit  - token is verified close paren                    *|
|*                           if close paren is missing.  LA_CH is set to      *|
|*                           colon-colon, or EOS                              *|
|*                                                                            *|
|* Returns:								      *|
|*	NONE                                                                  *|
|*									      *|
\******************************************************************************/
int	parse_array_spec(int	attr_idx)

{
   int			bd_idx;
   int			column;
   boolean		fold_it;
   boolean		found_end		= FALSE;
   boolean		found_error		= FALSE;
   fld_type		lb_fld;
   long			lb_len_idx;
   int			line;
   boolean		lower_bound_found;
   boolean		non_constant_size	= FALSE;
   boolean		possible_assumed_shape	= FALSE;
   int			rank			= 1;
   reference_type	referenced;
   fld_type		ub_fld;
   long			ub_len_idx;

  
   TRACE (Func_Entry, "parse_array_spec", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != LPAREN) {
      PRINTMSG(LA_CH_LINE, 295, Internal, LA_CH_COLUMN,
               "parse_array_spec", "LPAREN");
   }
# endif

   NEXT_LA_CH;				                     /* Skip Lparen   */
   bd_idx			= reserve_array_ntry(7);
   referenced			= (reference_type) AT_REFERENCED(attr_idx);
   AT_REFERENCED(attr_idx)	= Not_Referenced;
   BD_LINE_NUM(bd_idx)		= LA_CH_LINE;
   BD_COLUMN_NUM(bd_idx)	= LA_CH_COLUMN;

   /* If LA_CH is RPAREN, there is no dimension, so default to a rank 1       */
   /* constant sized array of length 1 and return.                            */

   if (LA_CH_VALUE == RPAREN) {
      parse_err_flush(Find_None, "dimension-spec");
      BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
      BD_ARRAY_SIZE(bd_idx)	= Constant_Size;
      BD_DCL_ERR(bd_idx)	= TRUE;
      BD_RANK(bd_idx)		= 1;
      BD_LB_FLD(bd_idx, 1)	= CN_Tbl_Idx;
      BD_LB_IDX(bd_idx, 1)	= CN_INTEGER_ONE_IDX;
      BD_UB_FLD(bd_idx, 1)	= CN_Tbl_Idx;
      BD_UB_IDX(bd_idx, 1)	= CN_INTEGER_ONE_IDX;
      NEXT_LA_CH;
      goto EXIT;
   }

   /* Set fold_it flag.  Will continue on and do pass2 style semantic     */
   /* checking and constant folding, if this is a component declaration.  */

   fold_it = (CURR_BLK == Derived_Type_Blk);

   do {  /* Process each dimension of the array */
      lower_bound_found 	= FALSE;
      lb_len_idx		= CN_INTEGER_ONE_IDX;
      lb_fld			= CN_Tbl_Idx;
      ub_len_idx		= NULL_IDX;
      ub_fld			= NO_Tbl_Idx;

      if (LA_CH_VALUE != COLON && LA_CH_VALUE != STAR) {
         line		= LA_CH_LINE;
         column		= LA_CH_COLUMN;

         /* If LA_CH isn't a COLON or a STAR, then this must be an expression.*/
         /* Get the expression and determine if it is a lower or upper bound. */
         /* If there is a parse error, a constant one is returned.            */

         if (!parse_int_spec_expr(&ub_len_idx, &ub_fld, fold_it, FALSE)) {
            ub_len_idx		= CN_INTEGER_ONE_IDX;
            ub_fld		= CN_Tbl_Idx;
            BD_DCL_ERR(bd_idx)	= TRUE;
         }

         if (ub_fld != CN_Tbl_Idx) {
            non_constant_size	= TRUE;
         }

         if (LA_CH_VALUE == COLON) {	               /* This is lower bound */
            lower_bound_found		= TRUE;
            possible_assumed_shape	= TRUE;
            lb_len_idx			= ub_len_idx;
            lb_fld			= ub_fld;
            ub_len_idx			= NULL_IDX;
            ub_fld			= NO_Tbl_Idx;
         }

         /* If LA_CH isn't a COLON this must be an upper bound.  If it is an  */
         /* upper bound and the array has already been classified as deferred */
         /* shape, issue an error, because a deferred shape array can never   */
         /* have an upper bound.  Otherwise set as an Explicit_Shape array.   */

         else if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape) {

            /* DIMENSION A(10,:)  --> Illegal - Set Upper bound to NULL_IDX.  */

            ub_len_idx		= NULL_IDX;
            ub_fld		= NO_Tbl_Idx;
            BD_DCL_ERR(bd_idx)	= TRUE;
            PRINTMSG(line, 114, Error, column);
         }
         else {
            BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
         }
      }
         
      /* Now the parser is in one of 3 states.  1) Lower bound found, upper   */
      /* bound = NULL, LA_CH must be COLON.  2) Lower bound not found, so it  */
      /* is set to a default of 1, upper bound is found and LA_CH is COMMA or */
      /* RPAREN.   (LA_CH must be COLON.)  3) Neither lower bound or upper    */
      /* bound have been seen, they are set to defaults of lb=1, ub=NULL.     */
      /* LA_CH is COLON or STAR.  If the LA_CH is COLON, this is either a     */
      /* Deferred-Shape spec or it is followed by the upper bound for an      */
      /* Explicit-Shape spec.  NOTE: LA_CH may be EOS - this is parse error.  */

      if (LA_CH_VALUE == COLON) {
         line		= LA_CH_LINE;
         column		= LA_CH_COLUMN;
         NEXT_LA_CH;				/* Skip COLON */

         if (LA_CH_VALUE == COMMA || LA_CH_VALUE == RPAREN) {

            /* Have one of two cases  1)  ARRAY(1:)  - This is an assumed     */
            /* shape spec which is classed as a Deferred-Shape, or 2) ARRAY(:)*/
            /* which is a deferred-Shape spec.   Issue an error if this array */
            /* has already been classified as an Explicit_Shape array.        */

            if (BD_ARRAY_CLASS(bd_idx) == Explicit_Shape) {
               PRINTMSG(line, 115, Error, column);
               BD_DCL_ERR(bd_idx)	= TRUE;
            }
            else {  /* Must be Deferred-Shape spec */
               BD_ARRAY_CLASS(bd_idx)	= Deferred_Shape;
            }
         }
         else {

            /* Have one of two cases  1)  ARRAY (1:10) - legal - pick up upper*/
            /* bound expression.  Err if array is already set to Deferred-    */
            /* Shape spec.  2) ARRAY (:10) - illegal - issue error.           */
            /* If the upper bound is a STAR, pick it up in the next section.  */

            if (!lower_bound_found) {			/* A(:10) - illegal   */
               PRINTMSG(LA_CH_LINE, 119, Error, LA_CH_COLUMN, &LA_CH_VALUE);
               BD_DCL_ERR(bd_idx)	= TRUE;
            }

            if (LA_CH_VALUE != STAR) {
               line	= LA_CH_LINE;
               column	= LA_CH_COLUMN;

               if (!parse_int_spec_expr(&ub_len_idx, &ub_fld, fold_it, FALSE)) {

                  /* Expression parser recovers LA_CH to : ) , or EOS */

                  BD_DCL_ERR(bd_idx)	= TRUE;
                  ub_len_idx		= CN_INTEGER_ONE_IDX;
                  ub_fld		= CN_Tbl_Idx;
               }

               if (ub_fld != CN_Tbl_Idx) {
                  non_constant_size	= TRUE;
               }

               if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape) { /*A(:,1:2)*/
                  PRINTMSG(line, 114, Error, column);
                  BD_DCL_ERR(bd_idx)	= TRUE;
                  ub_len_idx		= NULL_IDX;
                  ub_fld		= NO_Tbl_Idx;
               }
               else {
                  BD_ARRAY_CLASS(bd_idx)= Explicit_Shape;
               }
            }
         }
      }

      /* The parser may be:  1)  ARRAY(*) - lb=1, ub=NULL_IDX.  2) ARR(10:*)  */
      /* lb is set, and ub=NULL_IDX.  3)  ARRAY(:*) - illegal -error already  */
      /* issued.  You could not have picked up a lower bound and/or an upper  */
      /* bound and got to this position, because a * is part of an expression.*/
      /* The expression parser stops at COLON, COMMA, RPAREN or EOS.          */

      if (LA_CH_VALUE == STAR) {
         line	= LA_CH_LINE;
         column	= LA_CH_COLUMN;
         NEXT_LA_CH;				/* Skip STAR */

         if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape) {

            /* can't have asterisk with a deferred shape spec */

            PRINTMSG(line, 114, Error, column);
            parse_err_flush(Find_Rparen, NULL);
            BD_DCL_ERR(bd_idx)	= TRUE;
         }
         else {
            BD_ARRAY_CLASS(bd_idx)	= Assumed_Size;
            ub_len_idx			= lb_len_idx;
            ub_fld			= lb_fld;

            if (LA_CH_VALUE != RPAREN) {
 
               /* The assumed-size specifier * must be in the last dimension. */

               BD_DCL_ERR(bd_idx)	= TRUE;
               PRINTMSG(line, 116, Error, column);
               parse_err_flush(Find_Rparen, NULL);
            }
         }
      }

      BD_LB_IDX(bd_idx, rank)	= lb_len_idx;
      BD_LB_FLD(bd_idx, rank)	= lb_fld;
      BD_UB_IDX(bd_idx, rank)	= ub_len_idx;
      BD_UB_FLD(bd_idx, rank)	= ub_fld;

      if (LA_CH_VALUE == COMMA) {

         if (rank++ == 7) {          /* issue error - too many ranks */
            found_end		= TRUE;
            BD_DCL_ERR(bd_idx)	= TRUE;
            PRINTMSG(LA_CH_LINE, 117, Error, LA_CH_COLUMN);
            parse_err_flush(Find_Rparen, NULL);
         }
         else {
            NEXT_LA_CH;
         }
      }
      else {
         found_end = TRUE;
      }

      found_error = BD_DCL_ERR(bd_idx) | found_error;
   }
   while (!found_end);

   if (LA_CH_VALUE == RPAREN || 
       parse_err_flush(Find_Rparen, (found_error) ? NULL : ", or )")) {

      NEXT_LA_CH;		/* Skip RPAREN */
   }

   if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape) {

      if (possible_assumed_shape) {
         BD_ARRAY_CLASS(bd_idx) = Assumed_Shape;
      }
   }
   else if (!non_constant_size) {
      BD_ARRAY_SIZE(bd_idx) = Constant_Size;
   }

   BD_RANK(bd_idx) = rank;

# ifdef _DEBUG
   if (BD_ARRAY_CLASS(bd_idx) == Unknown_Array) {

      /* There is a parsing problem here.  This must never be Unknown_Array */

      PRINTMSG(LA_CH_LINE, 178, Internal, LA_CH_COLUMN);
   }
# endif

EXIT:

   if (AT_REFERENCED(attr_idx) > Not_Referenced) {
      is_attr_referenced_in_bound(bd_idx, attr_idx);
   }

   if (AT_REFERENCED(attr_idx) < referenced) {
      AT_REFERENCED(attr_idx)	= referenced;
   }

   bd_idx = ntr_array_in_bd_tbl(bd_idx);

   TRACE (Func_Exit, "parse_array_spec", NULL);

   return(bd_idx);

}  /* parse_array_spec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse a POSSIBLE generic spec.                                        *|
|*									      *|
|*	Input position  : The identifier, OPERATOR, or ASSIGNMENT token.      *|
|*	Output position : The comma or EOS after the interface or id.         *|
|*	Called By       : parse_access_stmt, parse_interface_stmt,            *|
|*	                  parse_use_stmt                                      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if parsed spec.  FALSE if found a fatal error.		      *|
|*									      *|
\******************************************************************************/

boolean	parse_generic_spec(void)

{
   boolean	parse_ok;


   TRACE (Func_Entry, "parse_generic_spec", NULL);

   if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {
      parse_ok	= TRUE;

      if (TOKEN_VALUE(token) == Tok_Id) {
         /* Intentionally left blank */
      }
      else if (TOKEN_VALUE(token) == Tok_Kwd_Assignment &&
               LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;				/* Pick up LPAREN */
 
         if (LA_CH_VALUE == EQUAL) {

            MATCHED_TOKEN_CLASS(Tok_Class_Op);

            if (TOKEN_VALUE(token) == Tok_Op_Assign) {

               if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ")")) {
                  NEXT_LA_CH;			/* Pick up RPAREN */
               }
            }
            else {
               PRINTMSG(TOKEN_LINE(token), 197, Error, TOKEN_COLUMN(token),
                        "=", TOKEN_STR(token));
               parse_ok = FALSE;

               if (parse_err_flush(Find_Rparen, NULL)) {
                  NEXT_LA_CH;                         /* Pick up RPAREN */
               }
            }
         }
         else if (parse_err_flush(Find_Rparen, "=")) {
            parse_ok = FALSE;
            NEXT_LA_CH;				/* Pick up RPAREN */
         }
      }
      else if (TOKEN_VALUE(token) == Tok_Kwd_Operator && 
               LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;				/* Pick up LPAREN */

         if (MATCHED_TOKEN_CLASS(Tok_Class_Op)) {

            switch (TOKEN_VALUE(token)) {
               case Tok_Const_True:
               case Tok_Const_False:
                  parse_ok = FALSE;
                  PRINTMSG(TOKEN_LINE(token), 499, Error, TOKEN_COLUMN(token));
                  break;

               case Tok_Op_Deref:
               case Tok_Op_Ptr_Assign:
               case Tok_Op_Assign:
                  parse_ok = FALSE;
                  PRINTMSG(TOKEN_LINE(token), 300, Error, TOKEN_COLUMN(token));
                  break;

               case Tok_Op_Eq :
                  TOKEN_STR(token)[0] = 'e';
                  TOKEN_STR(token)[1] = 'q';
                  break;

               case Tok_Op_Ge :
                  TOKEN_STR(token)[0] = 'g';
                  TOKEN_STR(token)[1] = 'e';
                  break;

               case Tok_Op_Gt :
                  TOKEN_STR(token)[0] = 'g';
                  TOKEN_STR(token)[1] = 't';
                  break;

               case Tok_Op_Le :
                  TOKEN_STR(token)[0] = 'l';
                  TOKEN_STR(token)[1] = 'e';
                  break;

               case Tok_Op_Lt :
                  TOKEN_STR(token)[0] = 'l';
                  TOKEN_STR(token)[1] = 't';
                  break;

               case Tok_Op_Ne :
                  TOKEN_STR(token)[0] = 'n';
                  TOKEN_STR(token)[1] = 'e';
                  break;

               case Tok_Op_Lg :
                  TOKEN_STR(token)[0] = 'l';
                  TOKEN_STR(token)[1] = 'g';
                  break;

               default:
                  break;
            }

            if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ")")) {
                  NEXT_LA_CH;				/* Pick up RPAREN */
            }
         }
         else if (LA_CH_VALUE == SLASH) {

            /* this clause is needed  because lex thinks that "/)" is an */
            /* array constructor punctuator.                             */

            TOKEN_STR(token)[0] = LA_CH_VALUE;
            TOKEN_VALUE(token)  = Tok_Op_Div;
            TOKEN_LEN(token)    = 1;
            NEXT_LA_CH;

            if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ")")) {
                  NEXT_LA_CH;                           /* Pick up RPAREN */
            }
         }
         else if (parse_err_flush(Find_Rparen, "defined-operator")) {
            parse_ok = FALSE;
            NEXT_LA_CH;		/* Pick up RPAREN */
         }
      }
      else {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
         MATCHED_TOKEN_CLASS(Tok_Class_Id);
      }
   }
   else {
      parse_err_flush(Find_Comma, "OPERATOR or ASSIGNMENT or generic-name");
      parse_ok = FALSE;
   }

   TRACE (Func_Exit, "parse_generic_spec", NULL);
   return(parse_ok);

}  /* parse_generic_spec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF       - intent-spec   is  IN  or  OUT  or INOUT                   *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	intent_type - Returns the INTENT value.  Default is Intent_Inout if   *|
|*                    there is an error.  Otherwise it's whatever was parsed. *|
|*									      *|
\******************************************************************************/

intent_type	parse_intent_spec()

{
   char		*err_str	= NULL;
   intent_type	 intent		= Intent_Inout;


   TRACE (Func_Entry, "parse_intent_spec", NULL);

   if (LA_CH_VALUE != LPAREN) {
      err_str = "(";
   }
   else {
      NEXT_LA_CH;			/* Skip Lparen */

      if (matched_specific_token(Tok_Kwd_In, Tok_Class_Keyword)) {

         if (!matched_specific_token(Tok_Kwd_Out, Tok_Class_Keyword)) {
            intent = Intent_In;
         }
      }
      else if (matched_specific_token(Tok_Kwd_Out, Tok_Class_Keyword)) {
         intent = Intent_Out;
      }
      else {
         parse_err_flush(Find_Rparen, "IN or OUT or INOUT");
         intent = Intent_Unseen;	/* Signal parse error */
      }

      if (LA_CH_VALUE == RPAREN) {
         NEXT_LA_CH;			/* Skip Rparen */
      }
      else {
         err_str = ")";
      }
   }

   if (err_str != NULL) {
      parse_err_flush(Find_Rparen, err_str);
      matched_specific_token(Tok_Punct_Rparen, Tok_Class_Punct);
      intent = Intent_Unseen;
   }

   TRACE (Func_Exit, "parse_intent_spec", NULL);

   return(intent);

}  /* parse_intent_spec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF       - ([KIND =] scalar-int-initialization-expr)                 *|
|*      Position  - entry - Token before start of expression                  *|
|*                  exit  - Token at end of expression                        *|
|*                                                                            *|
|*      QUESTIONS:  Need to know what the expression parser is going to do,   *|
|*                  if it hits an error?   Need to advance to the end of the  *|
|*                  specification expression.                                 *|
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
 
static	void	parse_kind_selector(void)

{
   int		al_idx;
   fld_type	field_type;
   long		kind_idx;
   opnd_type	opnd;


   TRACE (Func_Entry, "parse_kind_selector", NULL);

   if (matched_specific_token(Tok_Kwd_Kind, Tok_Class_Keyword) &&
       !matched_specific_token(Tok_Punct_Eq, Tok_Class_Punct)) {
      reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
   }

   OPND_LINE_NUM(opnd)	= LA_CH_LINE;
   OPND_COL_NUM(opnd)	= LA_CH_COLUMN;

   /* Always FOLD - These should be constants.  If not - it's fatal error. */

   parsing_kind_selector = TRUE;
   kind0seen = FALSE;
   kind0E0seen = FALSE;
   kind0D0seen = FALSE;
   kindconstseen = FALSE;

   if (parse_int_spec_expr(&kind_idx, &field_type, TRUE, FALSE)) {
      OPND_FLD(opnd)		= field_type;
      OPND_IDX(opnd)		= kind_idx;

      if (!kind_to_linear_type(&opnd,
                               AT_WORK_IDX,
                               kind0seen,
                               kind0E0seen,
                               kind0D0seen,
                               kindconstseen)) {
         AT_DCL_ERR(AT_WORK_IDX)	= TRUE;
      }

# if !defined(_TARGET_OS_MAX)

      if (!on_off_flags.enable_double_precision &&
          (TYP_TYPE(ATD_TYPE_IDX(AT_WORK_IDX)) == Complex ||
           TYP_TYPE(ATD_TYPE_IDX(AT_WORK_IDX)) == Real) &&
          TYP_DCL_VALUE(ATD_TYPE_IDX(AT_WORK_IDX)) == 16) {
         PRINTMSG(OPND_LINE_NUM(opnd), 586, Warning, OPND_COL_NUM(opnd));
      }
# endif

# if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)
      if ((TYP_TYPE(ATD_TYPE_IDX(AT_WORK_IDX)) == Complex ||
           TYP_TYPE(ATD_TYPE_IDX(AT_WORK_IDX)) == Real) &&
          TYP_DCL_VALUE(ATD_TYPE_IDX(AT_WORK_IDX)) == 16) {
         PRINTMSG(OPND_LINE_NUM(opnd), 541, Error, OPND_COL_NUM(opnd));
      }
# endif

      if (field_type == AT_Tbl_Idx) {

         /* Mark tmp not referenced and remove it from the tmp list.  This */
         /* tmp was not shared when it was entered into the bounds list.   */

         AT_REFERENCED(kind_idx)	= Not_Referenced;
         al_idx				= SCP_TMP_FW_IDX(curr_scp_idx);
         SCP_TMP_FW_IDX(curr_scp_idx)	= AL_NEXT_IDX(al_idx);
      }
   }

   parsing_kind_selector = FALSE;

   TRACE (Func_Exit, "parse_kind_selector", NULL);

   return;

}  /* parse_kind_selector */


/******************************************************************************\
|*                                                                            *|
|* Description:								      *|
|*   If i_can_have_len_equal                                                  *|
|*      BNF - [LEN =] type-param-value                                        *|
|*   If !(i_can_have_len_equal)                                               *|
|*      BNF - *(type-param-value)  OR *scalar-int-literal-constant            *|
|*   type-param-value IS   specification-expr OR *                            *|
|*                                                                            *|
|*   Position  - entry - token is token following (                           *|
|*                       or following LEN=                                    *|
|*                       or *                                                 *|
|*               exit  - LA is comma or right paren or EOS                    *|
|*									      *|
|* Input parameters:							      *|
|*	i_can_have_len_equal - LEN = is allowed.  See description.            *|
|*	parsing_length_selector - We are parsing the length-selector on the   *|
|*	                        CHARACTER statement, as opposed to parsing    *|
|*	                        the char-length on a variable name in a list. *|
|*	                        This is needed for the obsolescence message.  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if there are no errors, else FALSE				      *|
|*                                                                            *|
\******************************************************************************/
void	parse_length_selector(int	attr_idx,
			      boolean	i_can_have_len_equal,
			      boolean	parsing_length_selector)

{
   type_char_type	char_class	= Unknown_Char;
   int			column;
   fld_type		field_type;
   boolean		fold_it;
   long			len_idx;
   int			line;
   opnd_type		opnd;
   reference_type	referenced;


   TRACE (Func_Entry, "parse_length_selector", NULL);

   /* Set fold_it flag.  Will continue on and do pass2 style semantic     */
   /* checking and constant folding, if this is a component declaration.  */
  
   fold_it			= (CURR_BLK == Derived_Type_Blk);
   referenced			= (reference_type) AT_REFERENCED(attr_idx);
   AT_REFERENCED(attr_idx)	= Not_Referenced;

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

   if (i_can_have_len_equal) {

      if (matched_specific_token(Tok_Kwd_Len, Tok_Class_Keyword) &&
          !matched_specific_token(Tok_Punct_Eq, Tok_Class_Punct)) {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
      }
      line		= LA_CH_LINE;
      column		= LA_CH_COLUMN;

      if (LA_CH_VALUE == STAR) {
         NEXT_LA_CH;
         len_idx	= 0;
         field_type	= NO_Tbl_Idx;
         char_class	= Assumed_Size_Char;
      }
      else {

         if (!parse_int_spec_expr(&len_idx, &field_type, fold_it, TRUE)) {
            len_idx	= CN_INTEGER_ONE_IDX;
            field_type	= CN_Tbl_Idx;
         }

         if (field_type != AT_Tbl_Idx) {
            char_class	= Const_Len_Char;
         }
      }
   }
   else {
#ifdef KEY /* Bug 318, 321 */
      line		= TOKEN_LINE(token);
      column		= TOKEN_COLUMN(token);
      PRINTMSG(line, 1563, Ansi, Comment, column); /* Obsolescent */
#endif /* KEY Bug 318, 321 */
      NEXT_LA_CH;                    /* Skip Star */

      if (LA_CH_VALUE == LPAREN) {   /*    *(*)  or *(length)   */
         NEXT_LA_CH;  /* Skip Lparen */
         line		= LA_CH_LINE;
         column		= LA_CH_COLUMN;

         if (LA_CH_VALUE == STAR) {
            NEXT_LA_CH;
            len_idx	= 0;
            field_type	= NO_Tbl_Idx;
            char_class	= Assumed_Size_Char;
         }
         else {

            if (!parse_int_spec_expr(&len_idx, &field_type, fold_it, TRUE)) {
               len_idx		= CN_INTEGER_ONE_IDX;
               field_type	= CN_Tbl_Idx;
            }

            if (field_type != AT_Tbl_Idx) {
               char_class	= Const_Len_Char;
            }
         }

         if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ")")) {
            NEXT_LA_CH;                 /* Skip Rparen */
         }
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_Int_Spec)) {
         len_idx	= TOKEN_CONST_TBL_IDX(token);
         field_type	= CN_Tbl_Idx;
         char_class	= Const_Len_Char;
         line		= TOKEN_LINE(token);
         column		= TOKEN_COLUMN(token);

#ifndef KEY /* Bug 318, 321 */
         if (parsing_length_selector) {
            PRINTMSG(line, 1563, Comment, column); /* Obsolescent */
         }
#endif /* KEY Bug 318, 321 */
      }
      else {
         line		= LA_CH_LINE;
         column		= LA_CH_COLUMN;
         len_idx	= CN_INTEGER_ONE_IDX;
         field_type	= CN_Tbl_Idx;
         char_class	= Const_Len_Char;
         parse_err_flush(Find_None, "scalar-int-literal-constant or (");
      }
   }

   if (char_class == Assumed_Size_Char && CURR_BLK == Derived_Type_Blk) {

      /* Components cannot be assumed size character. */
      
      PRINTMSG(line, 191, Error, column);
      char_class	= Const_Len_Char;
      len_idx		= CN_INTEGER_ONE_IDX;
      field_type	= CN_Tbl_Idx;
   }
  
   if (AT_REFERENCED(attr_idx) > Not_Referenced) {

      /* The attr was referenced in the dimension bound describing the attr. */
      /* Find the reference to the attr, so that we can issue a really good  */
      /* error message.  It's a little compile time, but it's a fatal error. */

      AT_DCL_ERR(attr_idx)	= TRUE;

      if (field_type == AT_Tbl_Idx &&
          ATD_FLD(len_idx) == IR_Tbl_Idx &&
          find_attr_in_ir(attr_idx, ATD_TMP_IDX(len_idx), &opnd)) {
         PRINTMSG(OPND_LINE_NUM(opnd), 1035, Error,
                  OPND_COL_NUM(opnd),
                  AT_OBJ_NAME_PTR(attr_idx));
         len_idx	= CN_INTEGER_ONE_IDX;
         field_type	= CN_Tbl_Idx;
      }
   }

   if (AT_REFERENCED(attr_idx) < referenced) {
      AT_REFERENCED(attr_idx)	= referenced;
   }

   TYP_TYPE(TYP_WORK_IDX)		= Character;
   TYP_LINEAR(TYP_WORK_IDX)		= CHARACTER_DEFAULT_TYPE;
   TYP_CHAR_CLASS(TYP_WORK_IDX)		= char_class;
   TYP_FLD(TYP_WORK_IDX)		= field_type;
   TYP_IDX(TYP_WORK_IDX)		= len_idx;

   TRACE (Func_Exit, "parse_length_selector", NULL);

   return;

}  /* parse_length_selector */
#ifdef KEY /* Bug 8422 */

/*
 * This is the part of parse_type_spec() which processed the "ddd" in
 * something like "integer*ddd"; I have separated it so that it can be used
 * for the extension which allows something like "integer i*ddd" as well
 * (but it is not used for "character c*ddd", which was already handled by
 * parse_length_selector().)
 */
int
parse_non_char_kind_selector(boolean double_precision) {
   if (MATCHED_TOKEN_CLASS(Tok_Class_Int_Spec)) {
      long num	= (long) CN_INT_TO_C(TOKEN_CONST_TBL_IDX(token));
      linear_type_type linear_type	= Err_Res;
      int type_idx	= ATD_TYPE_IDX(AT_WORK_IDX);
      char *type_str	= basic_type_str[TYP_TYPE(type_idx)];

      switch (TYP_TYPE(type_idx)) {

      case Integer:
	    
	 switch (num) {

	 case 1:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  INTEGER_DEFAULT_TYPE : Integer_1;
	    break;

	 case 2:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  INTEGER_DEFAULT_TYPE : Integer_2;
	    break;

	 case 4:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  INTEGER_DEFAULT_TYPE : Integer_4;
	    break;

	 case 8:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  INTEGER_DEFAULT_TYPE : Integer_8;
	    break;

	 };

      break;


      case Real:

	 if (double_precision) {
	    type_str	= "DOUBLE PRECISION";

	    if (num == 16) {

# ifdef _TARGET_OS_MAX /* Msg was issued when DOUBLE PRECISION was parsed.*/
	       linear_type = Real_8;
# elif defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)
	       PRINTMSG(TOKEN_LINE(token), 541, Error, 
			TOKEN_COLUMN(token));
# else
	       linear_type = Real_16;

	       if (!on_off_flags.enable_double_precision) {
		  PRINTMSG(TOKEN_LINE(token), 710, Warning, 
			   TOKEN_COLUMN(token),
			   type_str,
			   num);
	       }
# endif
	    }
	 }
	 else {
	    switch (num) {

	    case 4:
	       linear_type = (cmd_line_flags.s_cf77types) ?
			     REAL_DEFAULT_TYPE : Real_4;
	       break;

	    case 8:
	       linear_type = (cmd_line_flags.s_cf77types) ?
			     REAL_DEFAULT_TYPE : Real_8;
	       break;

	    case 16:

# ifdef _TARGET_OS_MAX
	       PRINTMSG(TOKEN_LINE(token), 391, Warning,
			TOKEN_COLUMN(token),
			type_str, num, type_str, 8);
	       linear_type = Real_8;
# elif defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)
	       PRINTMSG(TOKEN_LINE(token), 541, Error, 
			TOKEN_COLUMN(token));
# else
	       linear_type = Real_16;

	       if (!on_off_flags.enable_double_precision) {
		  PRINTMSG(TOKEN_LINE(token), 710, Warning, 
			   TOKEN_COLUMN(token),
			   type_str,
			   num);
	       }
# endif
	       break;
	    };
	 }

	 break;


      case Complex:

	 switch (num) {

	 case 8:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  COMPLEX_DEFAULT_TYPE : Complex_4;
	    break;

	 case 16:
	    linear_type = Complex_8;
	    break;

	 case 32:

# ifdef _TARGET_OS_MAX
	    PRINTMSG(TOKEN_LINE(token), 391, Warning, 
		     TOKEN_COLUMN(token),
		     type_str, num, type_str, 16);
	    linear_type = Complex_8;
# elif defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)
	       PRINTMSG(TOKEN_LINE(token), 541, Error, 
			TOKEN_COLUMN(token));
# else
	    linear_type = Complex_16;

	    if (!on_off_flags.enable_double_precision) {
	       PRINTMSG(TOKEN_LINE(token), 710, Warning, 
			TOKEN_COLUMN(token),
			type_str,
			num);
	    }
# endif
	    break;
	 };

	 break;


      case Logical:

	 switch (num) {

	 case 1:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  LOGICAL_DEFAULT_TYPE : Logical_1;
	    break;

	 case 2:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  LOGICAL_DEFAULT_TYPE : Logical_2;
	    break;

	 case 4:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  LOGICAL_DEFAULT_TYPE : Logical_4;
	    break;

	 case 8:
	    linear_type = (cmd_line_flags.s_cf77types) ?
			  LOGICAL_DEFAULT_TYPE : Logical_8;
	    break;

	 };  /* end switch */

	 break;

      }  /* end switch */


      if (linear_type == Err_Res) {
	 PRINTMSG(TOKEN_LINE(token), 125, Error,
		  TOKEN_COLUMN(token),
		  num,
		  type_str);
      }
      else {
	 CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
	 TYP_TYPE(TYP_WORK_IDX)		= TYP_TYPE(type_idx);
	 TYP_LINEAR(TYP_WORK_IDX)		= linear_type;
	 TYP_DCL_VALUE(TYP_WORK_IDX)	= num;
	 TYP_DESC(TYP_WORK_IDX)		= Star_Typed;
	 int result = ntr_type_tbl();

	 PRINTMSG(TOKEN_LINE(token), 124, Ansi, 
		  TOKEN_COLUMN(token),
		  type_str,
		  num);
	 return result;

      }
   }
   else { /* Cannot search - because of IMPLICIT calls */
      parse_err_flush(Find_None, "scalar-int-literal-constant");
   }
   /* 0th entry of type_tbl is guaranteed to be initialized (see p_driver.c)
    * so error doesn't cause crash later on (SiCortex bug 4840) */ 
   return NULL_IDX;
}
#endif /* KEY Bug 8422 */

/******************************************************************************\
|*                                                                            *|
|* Description:								      *|
|*                                                                            *|
|*      BNF    - is INTEGER [kind-selector]                                   *|
|*               or REAL    [kind-selector]                                   *|
|*               or COMPLEX [kind-selector]                                   *|
|*               or LOGICAL [kind-selector]                                   *|
|*               or TYPE(type-name)                                           *|
|*               or DOUBLE PRECISION                                          *|
|*               or DOUBLE COMPLEX                                            *|
|*               CHARACTER [char-selector]                                    *|
|*                         where char_selector is                             *|
|*               * (type-param-value)                                         *|
|*               * scalar-int-literal-constant                                *|
|*               ([LEN=] type-param-value)                                    *|
|*               (LEN=type-param-value,KIND=scalar-int-initialization-exp)    *|
|*               (type-param-value, [KIND=] scalar-int-initialization-exp)    *|
|*               (KIND=scalar-int-initialization-exp [,LEN=type-param-value]) *|
|*                                                                            *|
|* Input parameters:							      *|
|*	chk_kind - TRUE if check for kind (or kind/len for character) on type *|
|*			spec				                      *|
|*                                                                            *|
|* Output parameters:							      *|
|*	NONE								      *|
|*                                                                            *|
|* Returns:								      *|
|*	TRUE is statement parsed okay.  (There may be fatal errors. )         *|
|*	FALSE if first token is not INTEGER, REAL, COMPLEX, LOGICAL, TYPE     *|
|*	      DOUBLE or CHARACTER.  ATD_TYPE_IDX will be NULL??               *|
|*	FALSE if this is found to be an assignment statement.                 *|
|*	Note: In case of error and stmt_type is known, the only searching     *|
|*	      will be for a right paren, if a left one has been found.        *|
|*                                                                            *|
\******************************************************************************/

boolean parse_type_spec(boolean		chk_kind)

{
   int			 al_idx;
   int			 attr_idx;
   int			 column;
   boolean		 do_kind_first;
   boolean		 double_precision	= FALSE;
   int			 host_attr_idx;
   int			 host_name_idx;
   int			 line;
   int			 name_idx;
   boolean		 parse_err		= FALSE;
   boolean		 save_err		= FALSE;
   boolean		 type_done		= FALSE;
#ifndef KEY /* Bug 8422 */
   long			 num;
   linear_type_type	 linear_type;
   int			 type_idx;
   char			*type_str;
#endif /* KEY Bug 8422 */


   TRACE (Func_Entry, "parse_type_spec", NULL);
   
   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      SH_ERR_FLG(curr_stmt_sh_idx)	= FALSE;
      save_err				= TRUE;
   }

   CLEAR_ATTR_NTRY(AT_WORK_IDX);

   switch (TOKEN_VALUE(token)) {
   case Tok_Kwd_Byte:
      PRINTMSG(TOKEN_LINE(token), 1253, Ansi, TOKEN_COLUMN(token), "BYTE");
      ATD_TYPE_IDX(AT_WORK_IDX) = Integer_1;
      break;

   case Tok_Kwd_Integer:
      ATD_TYPE_IDX(AT_WORK_IDX) = INTEGER_DEFAULT_TYPE;
      break;

   case Tok_Kwd_Real:
      ATD_TYPE_IDX(AT_WORK_IDX) = REAL_DEFAULT_TYPE;
      break;

   case Tok_Kwd_Complex:
      ATD_TYPE_IDX(AT_WORK_IDX) = COMPLEX_DEFAULT_TYPE;
      break;

   case Tok_Kwd_Logical:
      ATD_TYPE_IDX(AT_WORK_IDX) = LOGICAL_DEFAULT_TYPE;
      break;

   case Tok_Kwd_Character:
      line			= TOKEN_LINE(token);
      column			= TOKEN_COLUMN(token);
      ATD_TYPE_IDX(AT_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;

      if (LA_CH_VALUE == LPAREN) {

         if (chk_kind) {
            NEXT_LA_CH;				/* Skip LPAREN */
            do_kind_first = FALSE;

            if (LA_CH_VALUE == 'K' &&
                matched_specific_token(Tok_Kwd_Kind, Tok_Class_Keyword)) {

               if (LA_CH_VALUE == EQUAL) {
                  do_kind_first = TRUE;
               }
               reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
            }

            if (do_kind_first) {
               parse_kind_selector();

               if (LA_CH_VALUE == COMMA) {
                  NEXT_LA_CH;			/* Skip comma */

                  /* We can have length equal and we are parsing the */
                  /* length selector.  Hence  TRUE, TRUE.            */

                  parse_length_selector(AT_WORK_IDX, TRUE, TRUE);
                  TYP_DCL_VALUE(TYP_WORK_IDX)	= TYP_DCL_VALUE(ATD_TYPE_IDX(
                                                                AT_WORK_IDX));
                  TYP_DESC(TYP_WORK_IDX) = TYP_DESC(ATD_TYPE_IDX(AT_WORK_IDX));
                  ATD_TYPE_IDX(AT_WORK_IDX) = ntr_type_tbl();
               }
            }
            else {

               /* We can have length equal and we are parsing the */
               /* length selector.  Hence  TRUE, TRUE.            */

               parse_length_selector(AT_WORK_IDX, TRUE, TRUE);
               ATD_TYPE_IDX(AT_WORK_IDX)	= ntr_type_tbl();

               if (LA_CH_VALUE == COMMA) {
                  NEXT_LA_CH;			/* Skip comma */
                  parse_kind_selector();
               }
            }

            if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ")")) {
               NEXT_LA_CH;			/* Skip Rparen */
            }
         }
      }
      else if (LA_CH_VALUE == STAR) {

         /* This can't have LEN =, so pass FALSE */
         /* TRUE means this is a length-selector */

         parse_length_selector(AT_WORK_IDX, FALSE, TRUE);
         ATD_TYPE_IDX(AT_WORK_IDX)	= ntr_type_tbl();
      }

      type_done = TRUE;
      break;


   case Tok_Kwd_Double:
      line	= TOKEN_LINE(token);
      column	= TOKEN_COLUMN(token);

      if (LA_CH_VALUE == 'C' &&
          matched_specific_token(Tok_Kwd_Complex, Tok_Class_Keyword)) {

# if defined(_TARGET_OS_MAX)

         if (!on_off_flags.enable_double_precision) {
            PRINTMSG(line, 20, Ansi, column);
         }
         else if (cmd_line_flags.s_default32) {

            /* Use DOUBLE COMPLEX because -sdefault32 is built into the macro.*/

            PRINTMSG(line, 20, Ansi, column);
         }
         else {
            PRINTMSG(line, 702, Error, column);
         }
# else
         PRINTMSG(line, 20, Ansi, column);
# endif

         ATD_TYPE_IDX(AT_WORK_IDX)	= DOUBLE_COMPLEX_TYPE_IDX;
         type_done			= TRUE;
      }
      else if (LA_CH_VALUE == 'P' &&
               matched_specific_token(Tok_Kwd_Precision, Tok_Class_Keyword)) {

         ATD_TYPE_IDX(AT_WORK_IDX)      = DOUBLE_PRECISION_TYPE_IDX;

# ifdef _TARGET_OS_MAX

         if (! cmd_line_flags.s_default32 &&
             on_off_flags.enable_double_precision) {
            PRINTMSG(line, 1110, Warning, column);
            ATD_TYPE_IDX(AT_WORK_IDX)	= REAL_DEFAULT_TYPE;
         }
# endif

         double_precision		= TRUE;

         if (LA_CH_VALUE != STAR) {     /* Kind is not allowed */
            type_done			= TRUE;
         }
      }
      else {
         type_done			= TRUE;
         ATD_TYPE_IDX(AT_WORK_IDX)	= DOUBLE_PRECISION_TYPE_IDX;
         parse_err_flush(Find_None, "COMPLEX or PRECISION");
      }
      break;


   case Tok_Kwd_Type:

      if (LA_CH_VALUE != LPAREN) {
         parse_err_flush(Find_None, "(");
         ATD_TYPE_IDX(AT_WORK_IDX) = TYPELESS_DEFAULT_TYPE;
      }
      else {
         NEXT_LA_CH;				/* Skip Lparen */

         if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            parse_err_flush(Find_Rparen, "type-name");
         }
         else if (LA_CH_VALUE == RPAREN) {
            attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                    &name_idx);

            if (attr_idx == NULL_IDX) {       /* search host sym table */
               host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                                 TOKEN_LEN(token),
                                                 &host_name_idx,
                                                 FALSE);

               if (host_attr_idx == NULL_IDX) {
                  attr_idx			= ntr_sym_tbl(&token, name_idx);
                  AT_OBJ_CLASS(attr_idx)	= Derived_Type;
                  AT_LOCKED_IN(attr_idx)	= TRUE;
                  ATT_STRUCT_BIT_LEN_FLD(attr_idx)	= CN_Tbl_Idx;
                  ATT_STRUCT_BIT_LEN_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;
               }
               else if (stmt_type == Implicit_Stmt ||
                        stmt_type == Function_Stmt) {

                  /* Enter into the local scope, and link to the host's    */
                  /* attr.  Cannot lock in on this, because it may be      */
                  /* defined later in this scope.  Cannot issue error if   */
                  /* it's not a derived type for the same reason.          */
                  /* Catch AT_NOT_VISIBLE stuff when this is resolved.     */

                  /* ntr_host_in_sym_tbl just makes a new empty attr and   */
                  /* attr links it to the host_attr_idx.                   */

                  attr_idx = ntr_host_in_sym_tbl(&token,
                                                 name_idx,
                                                 host_attr_idx,
                                                 host_name_idx,
                                                 TRUE);

                  if (AT_OBJ_CLASS(host_attr_idx) == Derived_Type) {
                     COPY_ATTR_NTRY(attr_idx, host_attr_idx);
                     AT_CIF_SYMBOL_ID(attr_idx)	= 0;
                     AT_DEF_LINE(attr_idx)	= TOKEN_LINE(token);
                     AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
                     AT_LOCKED_IN(attr_idx)	= FALSE;
                     AT_ATTR_LINK(attr_idx)	= host_attr_idx;
                  }
                  else {
                     AT_OBJ_CLASS(attr_idx)		= Derived_Type;
                     ATT_STRUCT_BIT_LEN_FLD(attr_idx)	= CN_Tbl_Idx;
                     ATT_STRUCT_BIT_LEN_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;
                  }
               }
               else if (AT_OBJ_CLASS(host_attr_idx) == Derived_Type &&
                        !AT_NOT_VISIBLE(host_attr_idx)) {

                  /* Lock into using the host definition */

                  attr_idx = ntr_host_in_sym_tbl(&token,
                                                 name_idx,
                                                 host_attr_idx,
                                                 host_name_idx,
                                                 TRUE);

                  COPY_ATTR_NTRY(attr_idx, host_attr_idx);
                  AT_CIF_SYMBOL_ID(attr_idx)	= 0;
                  AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
                  AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
                  AT_ATTR_LINK(attr_idx)	= host_attr_idx;
                  AT_LOCKED_IN(attr_idx)	= TRUE;
               }
               else if (!fnd_semantic_err(Obj_Use_Derived_Type, 
                                          TOKEN_LINE(token),
                                          TOKEN_COLUMN(token),
                                          host_attr_idx,
                                          TRUE)) {

                  /* Has just PUBLIC or PRIVATE set.  Lock into using the  */
                  /* host definition.                                      */

                  attr_idx = ntr_host_in_sym_tbl(&token,
                                                 name_idx,
                                                 host_attr_idx,
                                                 host_name_idx,
                                                 TRUE);

                  AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
                  AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
                  AT_LOCKED_IN(attr_idx)	= TRUE;
               }
               else {  /* Lock into this use.  Issue error - create new    */
                       /* local attr to hopefully prevent error escalation.*/
                       /* Need to make this a better msg, because it comes */
                       /* from the host.                                   */

                  attr_idx			= ntr_sym_tbl(&token, name_idx);
                  AT_OBJ_CLASS(attr_idx)	= Derived_Type;
                  AT_DCL_ERR(attr_idx)		= TRUE;
                  AT_LOCKED_IN(attr_idx)	= TRUE;
                  ATT_STRUCT_BIT_LEN_FLD(attr_idx)	= CN_Tbl_Idx;
                  ATT_STRUCT_BIT_LEN_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;
               }
            }
            else if (AT_OBJ_CLASS(attr_idx) == Derived_Type &&
                     !AT_NOT_VISIBLE(attr_idx)) {
               AT_LOCKED_IN(attr_idx)	= TRUE;
            }
            else if (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
               host_attr_idx = AT_ATTR_LINK(attr_idx);

               while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX) {
                  host_attr_idx = AT_ATTR_LINK(host_attr_idx);
               }

               if (AT_OBJ_CLASS(host_attr_idx) == Derived_Type) {
                  CLEAR_VARIANT_ATTR_INFO(attr_idx, Derived_Type);
                  ATT_STRUCT_BIT_LEN_FLD(attr_idx)	= CN_Tbl_Idx;
                  ATT_STRUCT_BIT_LEN_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;
                  AT_LOCKED_IN(attr_idx)	= TRUE;
               }
               else {
                  PRINTMSG(TOKEN_LINE(token), 956, Error, 
                           TOKEN_COLUMN(token),
                           AT_OBJ_NAME_PTR(attr_idx));
                  CREATE_ERR_ATTR(attr_idx, TOKEN_LINE(token),
                                            TOKEN_COLUMN(token),
                                            Derived_Type);
                  ATT_STRUCT_BIT_LEN_FLD(attr_idx)	= CN_Tbl_Idx;
                  ATT_STRUCT_BIT_LEN_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;

                  /* Switch the name table to use the new attr, to limit      */
                  /* cascading errors for derived type definitions.           */

                  LN_ATTR_IDX(name_idx)		= attr_idx;
                  LN_NAME_IDX(name_idx)		= AT_NAME_IDX(attr_idx);
                  AT_LOCKED_IN(attr_idx)	= TRUE;
               }
            }
            else if (!fnd_semantic_err(Obj_Use_Derived_Type,
                                       TOKEN_LINE(token),
                                       TOKEN_COLUMN(token),
                                       attr_idx,
                                       TRUE)) {

               /* Has just PUBLIC or PRIVATE set */

               CLEAR_VARIANT_ATTR_INFO(attr_idx, Derived_Type);
               ATT_STRUCT_BIT_LEN_FLD(attr_idx)	= CN_Tbl_Idx;
               ATT_STRUCT_BIT_LEN_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;
               AT_LOCKED_IN(attr_idx)	= TRUE;
            }
            else {
               CREATE_ERR_ATTR(attr_idx, TOKEN_LINE(token),
                                         TOKEN_COLUMN(token), 
                                         Derived_Type);
               ATT_STRUCT_BIT_LEN_FLD(attr_idx)	= CN_Tbl_Idx;
               ATT_STRUCT_BIT_LEN_IDX(attr_idx)	= CN_INTEGER_ZERO_IDX;

               /* Add replaced attr to cif error list, so it gets a cif rec */
               /* Switch the name table to use the new attr, to limit       */
               /* cascading errors for derived type definitions.            */

               NTR_ATTR_LIST_TBL(al_idx);
               AL_ATTR_IDX(al_idx)	= LN_ATTR_IDX(name_idx);
               AL_NEXT_IDX(al_idx)	= SCP_CIF_ERR_LIST(curr_scp_idx);
               SCP_CIF_ERR_LIST(curr_scp_idx)	= al_idx;

               LN_ATTR_IDX(name_idx)	= attr_idx;
               LN_NAME_IDX(name_idx)	= AT_NAME_IDX(attr_idx);
               AT_LOCKED_IN(attr_idx)	= TRUE;

            }

            if ((cif_flags & XREF_RECS) != 0) {
      
               if (AT_ATTR_LINK(attr_idx) == NULL_IDX) {
                  host_attr_idx = attr_idx;
               }
               else {
                  host_attr_idx = AT_ATTR_LINK(attr_idx);

                  while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX) {
                     host_attr_idx = AT_ATTR_LINK(host_attr_idx);
                  }
               }

               cif_usage_rec(host_attr_idx, 
                             AT_Tbl_Idx,
                             TOKEN_LINE(token),
                             TOKEN_COLUMN(token),
                             CIF_Derived_Type_Name_Reference);
            }
            
            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
            TYP_TYPE(TYP_WORK_IDX)	= Structure;
            TYP_LINEAR(TYP_WORK_IDX)	= Structure_Type;
            TYP_IDX(TYP_WORK_IDX)	= attr_idx;
            ATD_TYPE_IDX(AT_WORK_IDX)	= ntr_type_tbl();

            NEXT_LA_CH;			/* Skip Rparen */
         }
         else {
            ATD_TYPE_IDX(AT_WORK_IDX) = TYPELESS_DEFAULT_TYPE;
            parse_err_flush(Find_Rparen, ")");
         }
      }

      type_done = TRUE;
      break;


   default:
      ATD_TYPE_IDX(AT_WORK_IDX)	= TYPELESS_DEFAULT_TYPE;
      type_done			= TRUE;
      PRINTMSG(TOKEN_LINE(token), 197, Error, TOKEN_COLUMN(token),
               "INTEGER, REAL, DOUBLE, COMPLEX, LOGICAL, CHARACTER or TYPE",
               TOKEN_STR(token));
      break;

   }  /* end switch */
            
   AT_TYPED(AT_WORK_IDX) = TRUE;

   if (!type_done) {

      if (chk_kind && LA_CH_VALUE == LPAREN) {
           
         NEXT_LA_CH;			/* Skip Lparen */
         parse_kind_selector();

         if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen,  ")")) {
            NEXT_LA_CH;			/* Skip Rparen */
         }
      }
      else if (LA_CH_VALUE == STAR) {
         NEXT_LA_CH;			/* Skip Star */

#ifdef KEY /* Bug 8422 */
      ATD_TYPE_IDX(AT_WORK_IDX) =
        parse_non_char_kind_selector(double_precision);
#endif /* KEY Bug 8422 */
      }
   }




   parse_err			= SH_ERR_FLG(curr_stmt_sh_idx);
   SH_ERR_FLG(curr_stmt_sh_idx) = save_err || parse_err;

   TRACE (Func_Exit, "parse_type_spec", NULL);

   return (!parse_err);

}  /* parse_type_spec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the PUBLIC or PRIVATE attribute to an attr.			      *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx -> Attr index to add the PUBLIC or PRIVATE access to.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	access   -> Public or Private                                         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_access(int		attr_idx,
	             int		line,
		     int		column,
		     access_type	access)

{
   boolean 	err_found;
   int		sn_idx;


   TRACE (Func_Entry, "merge_access", NULL);

   /* Error if access is already set, or if it is host associated */

   err_found = ((AT_ACCESS_SET(attr_idx) && access != AT_PRIVATE(attr_idx)) ||
                 AT_NOT_VISIBLE(attr_idx) ||
                (AT_ATTR_LINK(attr_idx) != NULL_IDX));

   switch (AT_OBJ_CLASS(attr_idx)) {
      case Data_Obj:

         if (ATD_SYMBOLIC_CONSTANT(attr_idx)) {
            err_found = TRUE;
         }
         break;

      case Pgm_Unit:
         if (ATP_PROC(attr_idx) == Intrin_Proc ||
             ATP_PGM_UNIT(attr_idx) == Program ||
             ATP_PGM_UNIT(attr_idx) == Module ||
             ATP_PGM_UNIT(attr_idx) == Blockdata) {
            err_found = TRUE;
         }
         break;

      case Interface:
         break;

      case Stmt_Func:
         err_found = TRUE;
         break;

      case Label:
         err_found = TRUE;
         break;

      default:
         break;

   }  /* end switch */


# ifdef _DEBUG

   /* Check to make sure that this routine is catching everything in the  */
   /* semantic tables, because it only calls fnd_semantic_err if it finds */
   /* an error.                                                           */

   if (!err_found &&
       fnd_semantic_err(((access == Public) ? Obj_Public : Obj_Private),
                        line, 
                        column,
                        attr_idx,
                        TRUE)) {
      PRINTMSG(line, 655, Internal, column, "merge_access");
   }
# endif

   if (err_found) {
      fnd_semantic_err(((access == Public) ? Obj_Public : Obj_Private),
                       line,
                       column,
                       attr_idx,
                       TRUE);
   }
   else {

      if (AT_ACCESS_SET(attr_idx)) {  /* Duplicate declaration */
#ifdef KEY /* Bug 5040 */
         PRINTMSG(line, 1259, ansi_or_warning(), column,
                  AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx),
                  (access == Public) ? "PUBLIC":"PRIVATE");
#else /* KEY Bug 5040 */
         PRINTMSG(line, 1259, Ansi, column,
                  AT_OBJ_NAME_PTR(attr_idx),
                  (access == Public) ? "PUBLIC":"PRIVATE");
#endif /* KEY Bug 5040 */
      }

      AT_PRIVATE(attr_idx)	= access;
      AT_ACCESS_SET(attr_idx)	= TRUE;

      if (AT_OBJ_CLASS(attr_idx) == Interface) { 

         if (AT_IS_INTRIN(attr_idx)) {
            sn_idx	= ATI_FIRST_SPECIFIC_IDX(attr_idx);

            while (sn_idx != NULL_IDX) {

               if (AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                  AT_PRIVATE(SN_ATTR_IDX(sn_idx))	= access;
                  AT_ACCESS_SET(SN_ATTR_IDX(sn_idx))	= TRUE;
               }
               sn_idx = SN_SIBLING_LINK(sn_idx);
            }
         }
         else if (ATI_PROC_IDX(attr_idx) != NULL_IDX) {
            AT_PRIVATE(ATI_PROC_IDX(attr_idx))		= access;
            AT_ACCESS_SET(ATI_PROC_IDX(attr_idx))	= TRUE;
         }
      }
   }

   TRACE (Func_Exit, "merge_access", NULL);

   return(!err_found);

}  /* merge_access */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the ALLOCATABLE attribute to an attr.			      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the ALLOCATABLE attribute to.           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_allocatable(boolean chk_semantics,
	                  int	  line,
		          int     column,
			  int	  attr_idx)

{
   boolean	fnd_err		= FALSE;


   TRACE (Func_Entry, "merge_allocatable", NULL);

#ifdef KEY /* Bug 6845 */
   /* Changes for bug 6845 imitate function merge_pointer(); we need to
    * deal with the situation where the "allocatable" statement is the first
    * clue that the current procedure is a function, etc. */
   if (AT_OBJ_CLASS(attr_idx) == Interface && 
       ATI_PROC_IDX(attr_idx) != NULL_IDX) {
       attr_idx = ATI_PROC_IDX(attr_idx);
   }
#endif /* KEY Bug 6845 */

   if (chk_semantics) {
#ifdef KEY /* Bug 6845 */
      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit && ATP_RSLT_NAME(attr_idx)) {
         PRINTMSG(line, 36, Error, column, AT_OBJ_NAME_PTR(attr_idx),
                  AT_OBJ_NAME_PTR(ATP_RSLT_IDX(attr_idx)));
         fnd_err                = TRUE;
         AT_DCL_ERR(attr_idx)   = TRUE;
      }
      else {
	fnd_err = fnd_semantic_err(Obj_Allocatable, line, column, attr_idx,
				   TRUE);
      }

      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit ||
	ATD_CLASS(attr_idx) == Function_Result) {
	PRINTMSG(line, 1679, Ansi, column);
      }

      if (!fnd_err && AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

         if (ATP_RSLT_IDX(attr_idx) == NULL_IDX) {
	    int rslt_idx = NULL_IDX;
            CREATE_FUNC_RSLT(attr_idx, rslt_idx);
            ATP_PGM_UNIT(attr_idx)      = Function;
            SET_IMPL_TYPE(rslt_idx);
            attr_idx                    = rslt_idx;
         }
         else {
            attr_idx = ATP_RSLT_IDX(attr_idx);
            fnd_err  = fnd_semantic_err(Obj_Pointer, line, column, attr_idx,
                                        TRUE);
         }
      }
#else /* KEY Bug 6845 */
	fnd_err = fnd_semantic_err(Obj_Allocatable,
				   line,
				   column,
				   attr_idx,
				   TRUE);
#endif /* KEY Bug 6845 */

      if (!fnd_err) {

         if (ATD_ALLOCATABLE(attr_idx)) {
#ifdef KEY /* Bug 5040 */
            PRINTMSG(line, 1259, ansi_or_warning(), column,
                     AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx),
                     "ALLOCATABLE");
#else /* KEY Bug 5040 */
            PRINTMSG(line, 1259, Ansi, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     "ALLOCATABLE");
#endif /* KEY Bug 5040 */
         }
         ATD_ALLOCATABLE(attr_idx) = TRUE;
         ATD_IM_A_DOPE(attr_idx)   = TRUE;
      }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
      ATD_ALLOCATABLE(attr_idx) = TRUE;
      ATD_IM_A_DOPE(attr_idx)   = TRUE;
   }


   TRACE (Func_Exit, "merge_allocatable", NULL);

   return(!fnd_err);

}  /* merge_allocatable */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the AUTOMATIC attribute to an attr.				      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the AUTOMATIC attribute to.             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_automatic(boolean		chk_semantics,
	                int		line,
		        int		column,
			int		attr_idx)

{
   boolean	fnd_err		= FALSE;
   int		rslt_idx;


   TRACE (Func_Entry, "merge_automatic", NULL);

   if (chk_semantics) {
      fnd_err = fnd_semantic_err(Obj_Automatic,
                                 line,
                                 column,
                                 attr_idx,
                                 TRUE);

      if (!fnd_err && AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

         if (ATP_RSLT_IDX(attr_idx) == NULL_IDX) {
            CREATE_FUNC_RSLT(attr_idx, rslt_idx);
            ATP_PGM_UNIT(attr_idx)	= Function;
            SET_IMPL_TYPE(rslt_idx);
            attr_idx			= rslt_idx;
         }
         else {
            attr_idx = ATP_RSLT_IDX(attr_idx);
            fnd_err  = fnd_semantic_err(Obj_Automatic, 
                                        line,
                                        column,
                                        attr_idx,
                                        TRUE);
         }
      }

      if (!fnd_err && ATD_CLASS(attr_idx) == Function_Result &&
          (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character ||
           TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure ||
           ATD_ARRAY_IDX(attr_idx) != NULL_IDX ||
           ATD_POINTER(attr_idx))) {
         AT_DCL_ERR(attr_idx)	= TRUE;
         fnd_err			= TRUE;
         PRINTMSG(line, 1255, Error, column, AT_OBJ_NAME_PTR(attr_idx));
      }

      if (!fnd_err) {

         if (ATD_STACK(attr_idx)) {
#ifdef KEY /* Bug 5040 */
            PRINTMSG(line, 1259, ansi_or_warning(), column,
                     AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx),
                     "AUTOMATIC");
#else /* KEY Bug 5040 */
            PRINTMSG(line, 1259, Ansi, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     "AUTOMATIC");
#endif /* KEY Bug 5040 */
         }
         ATD_STACK(attr_idx) = TRUE;
      }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
      ATD_STACK(attr_idx) = TRUE;
   }

   TRACE (Func_Exit, "merge_automatic", NULL);

   return(!fnd_err);

}  /* merge_automatic */

#ifdef KEY /* Bug 14150 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the BIND attribute to an attr. Imitating the (bad) example of     *|
|*      merge_intent(), the binding label comes from a global variable.       *|
|*	Note that common (sb_idx) is handled elsewhere, not here.             *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the BIND attribute to.                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_bind(boolean		chk_semantics,
	                int		line,
		        int		column,
			int		attr_idx)

{
   boolean fnd_err  = FALSE;
   TRACE (Func_Entry, "merge_bind", NULL);

   if (chk_semantics) {
     fnd_err = fnd_semantic_err(Obj_Bind, line, column, attr_idx, TRUE);
   }
   else if (!fnd_err && AT_BIND_ATTR(attr_idx)) {
     /* Make this an error, not warning, because it's not a legacy feature, and
      * binding labels might not match if we allow redundant "bind" */
     PRINTMSG(line, 1259, Error, column, AT_OBJ_NAME_PTR(attr_idx),
       AT_DEF_LINE(attr_idx), "BIND");
  }

  if (!fnd_err) {
    if (ATD_TYPE_IDX(attr_idx) == NULL_IDX) {
      SET_IMPL_TYPE(attr_idx);
    }
    set_binding_label(AT_Tbl_Idx, attr_idx, &new_binding_label);
  }

  TRACE (Func_Exit, "merge_bind", NULL);
  return !fnd_err;
}  /* merge_bind */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the VALUE attribute to an attr                                    *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the VALUE attribute to.                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_value(boolean		chk_semantics,
	                int		line,
		        int		column,
			int		attr_idx)

{
   boolean fnd_err  = FALSE;
   TRACE (Func_Entry, "merge_value", NULL);

   if (chk_semantics) {
     fnd_err = fnd_semantic_err(Obj_Value, line, column, attr_idx, TRUE);
     if (!fnd_err) {
       if (ATD_CLASS(attr_idx) == Dummy_Argument && ATD_VALUE_ATTR(attr_idx)) {
	 /* Make this error, not warning, because it's not a legacy feature */
	 PRINTMSG(line, 1259, Error, column, AT_OBJ_NAME_PTR(attr_idx),
	   AT_DEF_LINE(attr_idx), "VALUE");
	 fnd_err = TRUE;
       }
       else if (ATD_INTENT(attr_idx) == Intent_Out ||
         ATD_INTENT(attr_idx) == Intent_Inout) {
	 PRINTMSG(line, 550, Error, column, AT_OBJ_NAME_PTR(attr_idx),
	   "VALUE", "INTENT(OUT)", AT_DEF_LINE(attr_idx));
	 fnd_err = TRUE;
       }
     }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
   }

  if (!fnd_err) {
     /* Imitate merge_intent(). attr_semantics() detects use of "intent" or
      * "value" on non-dummy */
     ATD_CLASS(attr_idx) = Dummy_Argument;
     ATD_VALUE_ATTR(attr_idx) = TRUE;
  }

  TRACE (Func_Exit, "merge_value", NULL);
  return !fnd_err;
}  /* merge_value */
#endif /* KEY Bug 14150 */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the DIMENSION attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx -> Attr index to add the ALLOCATABLE attribute to.           *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	array_idx-> The index of the bounds table index to add to the attr.   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_dimension(int	attr_idx,
	                int	line,
		        int	column,
                        int	array_idx)

{
#ifdef KEY /* Bug 10177 */
   obj_type		dcl_type = Obj_Done;
   boolean		err_fnd = FALSE;
#else /* KEY Bug 10177 */
   obj_type		dcl_type;
   boolean		err_fnd;
#endif /* KEY Bug 10177 */
   int			i;
   int			old_bd_idx;
   int			rslt_idx;
   boolean		same;


   TRACE (Func_Entry, "merge_dimension", NULL);

   if (BD_DCL_ERR(array_idx)) {  /* Don't try if bad array declaration */
      AT_DCL_ERR(attr_idx)	= TRUE;
      err_fnd			= TRUE;
      goto EXIT;
   }
       
   switch (BD_ARRAY_CLASS(array_idx)) {

      case Explicit_Shape:
         dcl_type = Obj_Expl_Shp_Arr;
         break;

      case Deferred_Shape:
         dcl_type = Obj_Defrd_Shp_Arr;
         break;

      case Assumed_Size:
         dcl_type = Obj_Assum_Size_Arr;
         break;

      case Assumed_Shape:
         dcl_type = Obj_Assum_Shp_Arr;
         break;

   }  /* End switch */

   if (AT_OBJ_CLASS(attr_idx) == Interface && 
       ATI_PROC_IDX(attr_idx) != NULL_IDX) {
       attr_idx = ATI_PROC_IDX(attr_idx);
   }

   if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit && ATP_PGM_UNIT(attr_idx) != Module) {
      rslt_idx = ATP_RSLT_IDX(attr_idx);

      if (rslt_idx != NULL_IDX) {	/* Has a function result already */

         if (ATP_RSLT_NAME(attr_idx) && !AT_NOT_VISIBLE(attr_idx)) {
            PRINTMSG(line, 27, Error, column, AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(rslt_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
            AT_DCL_ERR(rslt_idx)	= TRUE;
         }
         else {

            if (AT_REFERENCED(attr_idx) > Not_Referenced &&
                is_attr_referenced_in_bound(array_idx, attr_idx)) {
               err_fnd	= TRUE;
            }
            else {
               err_fnd = fnd_semantic_err(dcl_type,
                                          line,
                                          column,
                                          attr_idx,
                                          TRUE);
            }

            if (!err_fnd) {

               if (ATD_ARRAY_IDX(rslt_idx) != NULL_IDX) {

                  /* This has an array declaration already.  Is it the same? */

                  old_bd_idx	= ATD_ARRAY_IDX(rslt_idx);
                  same		= (old_bd_idx == array_idx);

                  if (!same && 
                      BD_ARRAY_CLASS(old_bd_idx)==BD_ARRAY_CLASS(array_idx)&&
                      BD_RANK(old_bd_idx) == BD_RANK(array_idx) &&
                      BD_ARRAY_SIZE(old_bd_idx) == BD_ARRAY_SIZE(array_idx)){

                     if (BD_ARRAY_CLASS(array_idx) != Deferred_Shape) {
                        same	= TRUE;

                        for (i = 1; i <= BD_RANK(array_idx); i++) {

                           if (BD_UB_FLD(old_bd_idx,i)!=BD_UB_FLD(array_idx,i)||
                               (BD_UB_FLD(old_bd_idx,i) == AT_Tbl_Idx &&
                              BD_UB_IDX(old_bd_idx,i)!=BD_UB_IDX(array_idx,i))||
                               (BD_UB_FLD(old_bd_idx,i) == CN_Tbl_Idx &&
                                fold_relationals(BD_UB_IDX(old_bd_idx,i),
                                                 BD_UB_IDX(array_idx,i),
                                                 Ne_Opr)) ||
                               BD_LB_FLD(old_bd_idx,i)!=BD_LB_FLD(array_idx,i)||
                               (BD_LB_FLD(old_bd_idx,i) == AT_Tbl_Idx &&
                              BD_LB_IDX(old_bd_idx,i)!=BD_LB_IDX(array_idx,i))||
                               (BD_LB_FLD(old_bd_idx,i) == CN_Tbl_Idx &&
                                fold_relationals(BD_LB_IDX(old_bd_idx,i),
                                                 BD_LB_IDX(array_idx,i),
                                                 Ne_Opr))) {
                              same	= FALSE;
                              break;
                           }
                        }
                     }
                  }

                  if (same) {
#ifdef KEY /* Bug 5040 */
                     PRINTMSG(line, 1259, ansi_or_warning(), column, 
                              AT_OBJ_NAME_PTR(rslt_idx), AT_DEF_LINE(attr_idx),
			      "DIMENSION");
                  }
                  else {
                     PRINTMSG(line, 554, Error, column, 
                              AT_OBJ_NAME_PTR(rslt_idx), "DIMENSION",
                              "DIMENSION", AT_DEF_LINE(rslt_idx));
#else /* KEY Bug 5040 */
                     PRINTMSG(line, 1259, Ansi, column, 
                              AT_OBJ_NAME_PTR(rslt_idx), "DIMENSION");
                  }
                  else {
                     PRINTMSG(line, 554, Error, column, 
                              AT_OBJ_NAME_PTR(rslt_idx), "DIMENSION",
                              "DIMENSION");
#endif /* KEY Bug 5040 */
                  }
               }
               else {
                  ATD_ARRAY_IDX(rslt_idx) = array_idx;
               }
            }

            if (ATP_RECURSIVE(attr_idx) && !on_off_flags.recursive) {
               PRINTMSG(line, 184, Caution, column, AT_OBJ_NAME_PTR(attr_idx));
            }
         }
      }
      else {

         if (AT_REFERENCED(attr_idx) > Not_Referenced &&
             is_attr_referenced_in_bound(array_idx, attr_idx)) {
            err_fnd	= TRUE;
         }
         else {
            err_fnd = fnd_semantic_err(dcl_type,
                                       line,
                                       column,
                                       attr_idx,
                                       TRUE);
         }

         /* This must be a Function if it's legal */

         if (!err_fnd) {
            CREATE_FUNC_RSLT(attr_idx, rslt_idx);
            ATP_PGM_UNIT(attr_idx)	= Function;
            ATD_ARRAY_IDX(rslt_idx)	= array_idx;
            SET_IMPL_TYPE(rslt_idx);
         }
      }
   }
   else {
      if (AT_REFERENCED(attr_idx) > Not_Referenced &&
          is_attr_referenced_in_bound(array_idx, attr_idx)) {
         err_fnd	= TRUE;
      }
      else {
         err_fnd = fnd_semantic_err(dcl_type,
                                    line,
                                    column,
                                    attr_idx,
                                    TRUE);
      }
         
      if (!err_fnd) {

         if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

            /* This has an array declaration already.  Is it the same? */

            old_bd_idx	= ATD_ARRAY_IDX(attr_idx);
            same	= (old_bd_idx == array_idx);

            if (!same && 
                BD_ARRAY_CLASS(old_bd_idx) == BD_ARRAY_CLASS(array_idx) &&
                BD_RANK(old_bd_idx) == BD_RANK(array_idx) &&
                BD_ARRAY_SIZE(old_bd_idx) == BD_ARRAY_SIZE(array_idx)) {

               if (BD_ARRAY_CLASS(array_idx) != Deferred_Shape) {
                  same	= TRUE;

                  for (i = 1; i <= BD_RANK(array_idx); i++) {

                     if (BD_UB_FLD(old_bd_idx,i) != BD_UB_FLD(array_idx,i)||
                         (BD_UB_FLD(old_bd_idx,i) == AT_Tbl_Idx &&
                          BD_UB_IDX(old_bd_idx,i) != BD_UB_IDX(array_idx,i))||
                         (BD_UB_FLD(old_bd_idx,i) == CN_Tbl_Idx &&
                          fold_relationals(BD_UB_IDX(old_bd_idx,i),
                                           BD_UB_IDX(array_idx,i),
                                           Ne_Opr)) ||
                         BD_LB_FLD(old_bd_idx,i) != BD_LB_FLD(array_idx,i)||
                         (BD_LB_FLD(old_bd_idx,i) == AT_Tbl_Idx &&
                          BD_LB_IDX(old_bd_idx,i) != BD_LB_IDX(array_idx,i))||
                         (BD_LB_FLD(old_bd_idx,i) == CN_Tbl_Idx &&
                          fold_relationals(BD_LB_IDX(old_bd_idx,i),
                                           BD_LB_IDX(array_idx,i),
                                           Ne_Opr))) {

                        same	= FALSE;
                        break;
                     }
                  }
               }
            }

            if (same) {
#ifdef KEY /* Bug 5040 */
               PRINTMSG(line, 1259, ansi_or_warning(), column, 
                        AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx),
			"DIMENSION");
            }
            else {
               PRINTMSG(line, 554, Error, column, 
                        AT_OBJ_NAME_PTR(attr_idx), "DIMENSION", "DIMENSION",
			AT_DEF_LINE(attr_idx));
#else /* KEY Bug 5040 */
               PRINTMSG(line, 1259, Ansi, column, 
                        AT_OBJ_NAME_PTR(attr_idx), "DIMENSION");
            }
            else {
               PRINTMSG(line, 554, Error, column, 
                        AT_OBJ_NAME_PTR(attr_idx), "DIMENSION", "DIMENSION");
#endif /* KEY Bug 5040 */
            }
         }
         else {
            ATD_ARRAY_IDX(attr_idx)	= array_idx;

            if (BD_ARRAY_CLASS(array_idx) == Assumed_Shape ||
                BD_ARRAY_CLASS(array_idx) == Deferred_Shape) {

               ATD_IM_A_DOPE(attr_idx) = TRUE;
            }
         }
      }
   }

EXIT:

   TRACE (Func_Exit, "merge_dimension", NULL);

   return(!err_fnd);

}  /* merge_dimension */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the DATA or initialization attribute to an Attr.		      *|
|*      As long as we're here, also mark the Attr as being defined.	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the Attr.        *|
|*	line     -> The line number of the object to add the attribute to.    *|
|*	column   -> The line number of the object to add the attribute to.    *|
|*	attr_idx -> Attr index to add the DATA attribute to.                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_data(boolean	chk_semantics,
	           int		line,
		   int		column,
		   int		attr_idx)

{
   boolean	fnd_err 	= FALSE;


   TRACE (Func_Entry, "merge_data", NULL);
   
   if (chk_semantics) {
      fnd_err = fnd_semantic_err(Obj_Data_Init,
                                 line,
                                 column,
                                 attr_idx,
                                 TRUE);
   }

   if (!fnd_err) {
      AT_DEFINED(attr_idx)    = TRUE;
      ATD_DATA_INIT(attr_idx) = TRUE;
      ATD_CLASS(attr_idx)     = Variable;
   }

   TRACE (Func_Exit, "merge_data", NULL);

   return(!fnd_err);

}  /* merge_data */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the EXTERNAL attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the EXTERNAL attribute to.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_external(boolean	chk_semantics,
		       int	line,
		       int	column,
		       int	attr_idx)

{
   long		chk_err		= FALSE;


   TRACE (Func_Entry, "merge_external", NULL);

   if (AT_OBJ_CLASS(attr_idx) == Interface && 
       !AT_IS_INTRIN(attr_idx) &&		/* JBL  - this is a kludge */
       ATI_PROC_IDX(attr_idx) != NULL_IDX) {
       attr_idx = ATI_PROC_IDX(attr_idx);
   }

   if (chk_semantics && fnd_semantic_err(Obj_Dcl_Extern,
                                         line,
                                         column,
                                         attr_idx,
                                         TRUE)) {
      chk_err = TRUE;
   }
   else {

#ifdef KEY /* Bug 14150 */
      /* fnd_semantic_err() detects addition of bind to external, not vice
       * versa */
      if ((!(AT_OBJ_CLASS(attr_idx) == Data_Obj && 
	ATD_CLASS(attr_idx) == Dummy_Argument)) &&
	AT_BIND_ATTR(attr_idx)) {
	PRINTMSG(line, 550, Error, column, AT_OBJ_NAME_PTR(attr_idx), "BIND",
	  "EXTERNAL", AT_DEF_LINE(attr_idx));
	chk_err = TRUE;
      }
  #endif /* KEY Bug 14150 */
      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

         /* By passing Pgm_Unknown, chg_data_obj_to_pgm_unit will decide */
         /* if this attr, should become a Function or a Pgm_Unknown.     */

         chg_data_obj_to_pgm_unit(attr_idx, 
                                  Pgm_Unknown,
                                  Extern_Proc);
      }
      else {

         if (ATP_DCL_EXTERNAL(attr_idx)) {
#ifdef KEY /* Bug 5040 */
            PRINTMSG(line, 1259, ansi_or_warning(), column, 
                     AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx),
                     "EXTERNAL");
#else /* KEY Bug 5040 */
            PRINTMSG(line, 1259, Ansi, column, 
                     AT_OBJ_NAME_PTR(attr_idx),
                     "EXTERNAL");
#endif /* KEY Bug 5040 */
         }

         if (ATP_PROC(attr_idx) == Unknown_Proc) {
            ATP_PROC(attr_idx) = Extern_Proc;
         }

         if (attr_idx == SCP_ATTR_IDX(curr_scp_idx)) {

            /* SUBROUTINE JOE(); EXTERNAL JOE   is non-standard. */
   
            PRINTMSG(line, 279, Ansi, column, AT_OBJ_NAME_PTR(attr_idx));
         }
      }

      ATP_DCL_EXTERNAL(attr_idx)	= TRUE;
      ATP_SCP_IDX(attr_idx)		= curr_scp_idx;
   }

   TRACE (Func_Exit, "merge_external", NULL);

   return(!chk_err);

}  /* merge_external */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the INTENT attribute to an attr.			      	      *|
|*      NOTE:  The intent to add is picked up from a global variable, called  *|
|*             new_intent.                                                    *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the EXTERNAL attribute to.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_intent(boolean		chk_semantics,
	             int		line,
		     int		column,
		     int		attr_idx)

{
   boolean	fnd_err		= FALSE;


   TRACE (Func_Entry, "merge_intent", NULL);

   if (AT_OBJ_CLASS(attr_idx) == Interface && 
       ATI_PROC_IDX(attr_idx) != NULL_IDX) {
       attr_idx = ATI_PROC_IDX(attr_idx);
   }
   
   if (chk_semantics) {
      fnd_err = fnd_semantic_err(Obj_Intent,
                                 line,
                                 column,
                                 attr_idx,
                                 TRUE);

      if (!fnd_err) {

#ifdef KEY /* Bug 14110, 14150 */
         if (new_intent == Intent_In) {
	   if (ATD_VOLATILE(attr_idx)) {
	     PRINTMSG(line, 550, Error, column, AT_OBJ_NAME_PTR(attr_idx),
	       "VOLATILE", "INTENT(IN)", AT_DEF_LINE(attr_idx));
	   }
	 }
	 else {
	   if (ATD_VALUE_ATTR(attr_idx)) {
	     PRINTMSG(line, 550, Error, column, AT_OBJ_NAME_PTR(attr_idx),
	       "VALUE", "INTENT(OUT)", AT_DEF_LINE(attr_idx));
	   }
	 }
#endif /* KEY Bug 14110, 14150 */
         if (ATD_INTENT(attr_idx) != Intent_Unseen) {

            if (ATD_INTENT(attr_idx) == new_intent) {
#ifdef KEY /* Bug 5040 */
               PRINTMSG(line, 1259, ansi_or_warning(), column,
	         AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), "INTENT");
            }
            else {  /* The intent is different */
               PRINTMSG(line, 554, Error, column, AT_OBJ_NAME_PTR(attr_idx),
                        "INTENT", "INTENT", AT_DEF_LINE(attr_idx));
#else /* KEY Bug 5040 */
               PRINTMSG(line, 1259, Ansi, column, AT_OBJ_NAME_PTR(attr_idx),
                        "INTENT");
            }
            else {  /* The intent is different */
               PRINTMSG(line, 554, Error, column, AT_OBJ_NAME_PTR(attr_idx),
                        "INTENT", "INTENT");
#endif /* KEY Bug 5040 */
            }
         }
      }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
   }

   if (!fnd_err) {
      ATD_CLASS(attr_idx)	= Dummy_Argument;
      ATD_INTENT(attr_idx)	= new_intent;
   }

   TRACE (Func_Exit, "merge_intent", NULL);

   return(!fnd_err);

}  /* merge_intent */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the INTRINSIC attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the EXTERNAL attribute to.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_intrinsic(boolean	chk_semantics,
	                int	line,
		        int	column,
			int	attr_idx)

{
   boolean	found_error	= FALSE;
   int		save_curr_scp_idx;
   int		host_name_idx;
   int		host_attr_idx;
   int		sn_idx;
   int		type_idx;


   TRACE (Func_Entry, "merge_intrinsic", NULL);

   if (chk_semantics && fnd_semantic_err(Obj_Dcl_Intrin, 
                                         line, 
                                         column, 
                                         attr_idx,
                                         TRUE)) {
      found_error = TRUE;
   }
   else if (AT_IS_INTRIN(attr_idx) && AT_OBJ_CLASS(attr_idx) == Interface) {

      if (ATI_DCL_INTRINSIC(attr_idx)) {
#ifdef KEY /* Bug 5040 */
         PRINTMSG(line, 1259, ansi_or_warning(), column,
	   AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), "INTRINSIC");
#else /* KEY Bug 5040 */
         PRINTMSG(line, 1259, Ansi, column, AT_OBJ_NAME_PTR(attr_idx),
                  "INTRINSIC");
#endif /* KEY Bug 5040 */
      }
      ATI_DCL_INTRINSIC(attr_idx) = TRUE;
   }
   else {

      /* The INTRINSIC has not been copied down yet. - So copy it. */

      host_attr_idx = srch_host_sym_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                        AT_NAME_LEN(attr_idx),
                                        &host_name_idx,
                                        TRUE);

      if (host_attr_idx != NULL_IDX) {

         /* go directly to the INTRINSIC scope if not interface */
         /* or if it is not the name of a specific intrinsic.   */

         if (AT_OBJ_CLASS(host_attr_idx) != Interface ||
             !ATI_GENERIC_INTRINSIC(host_attr_idx)) {
            save_curr_scp_idx = curr_scp_idx;
            curr_scp_idx = INTRINSIC_SCP_IDX;
            host_attr_idx = srch_sym_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                         AT_NAME_LEN(attr_idx),
                                         &host_name_idx);
            curr_scp_idx = save_curr_scp_idx;
         }

         if (host_attr_idx != NULL_IDX &&
             AT_IS_INTRIN(host_attr_idx) &&
             ATI_FIRST_SPECIFIC_IDX(host_attr_idx) == NULL_IDX) {
            complete_intrinsic_definition(host_attr_idx);
         }
      }

      if (host_attr_idx == NULL_IDX) {

         /* Error - Couldn't find the intrinsic.  Set implicit type.  */

         PRINTMSG(line, 701, Error, column, AT_OBJ_NAME_PTR(attr_idx));
         found_error = TRUE;

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj && !AT_TYPED(attr_idx)) {
            SET_IMPL_TYPE(attr_idx);
         }
         else {

            if (AT_OBJ_CLASS(attr_idx) == Interface && 
                ATI_PROC_IDX(attr_idx) != NULL_IDX) {
               attr_idx = ATI_PROC_IDX(attr_idx);
            }

            if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit && 
                ATP_PGM_UNIT(attr_idx) == Function &&
               !AT_TYPED(ATP_RSLT_IDX(attr_idx))) {
               SET_IMPL_TYPE(ATP_RSLT_IDX(attr_idx));
            }
         }
      }
      else if (AT_OBJ_CLASS(attr_idx) == Interface) {

         /* Both are interfaces.   Add these specific intrinsics behind     */
         /* any user declared specifics that have previously been declared. */

         AT_IS_INTRIN(attr_idx)		= TRUE;
         ATI_DCL_INTRINSIC(attr_idx)	= TRUE;
         ATI_NUM_SPECIFICS(attr_idx)   += ATI_NUM_SPECIFICS(host_attr_idx);

         sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

         if (sn_idx == NULL_IDX) {
            ATI_FIRST_SPECIFIC_IDX(attr_idx) = 
                                          ATI_FIRST_SPECIFIC_IDX(host_attr_idx);
         }
         else {
            while (SN_SIBLING_LINK(sn_idx) != NULL_IDX) {
               sn_idx = SN_SIBLING_LINK(sn_idx);
            }
            SN_SIBLING_LINK(sn_idx) = ATI_FIRST_SPECIFIC_IDX(host_attr_idx);
         }
      }
      else {

         if (ATI_INTERFACE_CLASS(host_attr_idx) == 
                                                Generic_Subroutine_Interface) {

            if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

               if (ATP_RSLT_IDX(attr_idx) != NULL_IDX && 
                   AT_TYPED(ATP_RSLT_IDX(attr_idx))) {
                  PRINTMSG(line, 869, Error, column,
                           AT_OBJ_NAME_PTR(attr_idx));
                  found_error = TRUE;
               }
   
               ATP_RSLT_IDX(attr_idx) = NULL_IDX;
            }
            else if (AT_OBJ_CLASS(attr_idx) == Data_Obj && AT_TYPED(attr_idx)){
               PRINTMSG(line, 869, Error, column, AT_OBJ_NAME_PTR(attr_idx));
               found_error = TRUE;
            }
         }

         type_idx = NULL_IDX;

         if (AT_TYPED(attr_idx)) {

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               type_idx = ATD_TYPE_IDX(attr_idx);
            }
            else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                     ATP_RSLT_IDX(attr_idx) != NULL_IDX) {
               type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));
            }
         }

         COPY_VARIANT_ATTR_INFO(host_attr_idx,
                                attr_idx, 
                                Interface);

         AT_ELEMENTAL_INTRIN(attr_idx)	= AT_ELEMENTAL_INTRIN(host_attr_idx);
         AT_IS_INTRIN(attr_idx)		= TRUE;
         ATD_TYPE_IDX(attr_idx)		= type_idx;
         ATI_DCL_INTRINSIC(attr_idx)	= TRUE;
      }
   }

   TRACE (Func_Exit, "merge_intrinsic", NULL);

   return(!found_error);

}  /* merge_intrinsic */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      NOTES: The object must be a dummy argument.             (p5-2,17-18)  *|
|*             The object must be in a subpgm or interface blk  (5.2.2)       *|
|*             The stmt must not be declared in a blockdata pgm (p11-6,31-33) *|
|*             If in an ASSIGNMENT or an OPERATOR interface     (p12-6,1-4)   *|
|*                cannot specify optional                                     *|
|*             Must not be specified in a main program          (p11-1,33-36) *|
|*                                                                            *|
|*  END of declaration:                                                       *|
|*         If allocatable - it must be a deferred-shape array.                *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the EXTERNAL attribute to.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge					      *|
|*									      *|
\******************************************************************************/

boolean merge_optional (boolean	chk_semantics,
		        int	line,
		        int	column,
			int	attr_idx)

{
   boolean	chk_err		= FALSE;


   TRACE (Func_Entry, "merge_optional", NULL);

   if (chk_semantics) {
      chk_err = fnd_semantic_err(Obj_Optional,
                                 line,
                                 column,
                                 attr_idx,
                                 TRUE);

      if (!chk_err && AT_OPTIONAL(attr_idx)) {
#ifdef KEY /* Bug 5040 */
         PRINTMSG(line, 1259, ansi_or_warning(), column,
	   AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), "OPTIONAL");
#else /* KEY Bug 5040 */
         PRINTMSG(line, 1259, Ansi, column, AT_OBJ_NAME_PTR(attr_idx),
                  "OPTIONAL");
#endif /* KEY Bug 5040 */
      }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
   }

   if (!chk_err) {

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ATD_CLASS(attr_idx)	= Dummy_Argument;
      }
      else {
         ATP_PROC(attr_idx)	= Dummy_Proc;
      }
      AT_OPTIONAL(attr_idx)	= TRUE;
   }

   TRACE (Func_Exit, "merge_optional", NULL);

   return(!chk_err);

}  /* merge_optional */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the POINTER attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the POINTER attribute to.               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_pointer(boolean	chk_semantics,
	              int	line,
		      int	column,
		      int	attr_idx)

{
   boolean	fnd_err		= FALSE;
   int		rslt_idx;


   TRACE (Func_Entry, "merge_pointer", NULL);

   if (AT_OBJ_CLASS(attr_idx) == Interface && 
       ATI_PROC_IDX(attr_idx) != NULL_IDX) {
       attr_idx = ATI_PROC_IDX(attr_idx);
   }

   if (chk_semantics) {

      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit && ATP_RSLT_NAME(attr_idx)) {
         PRINTMSG(line, 36, Error, column, AT_OBJ_NAME_PTR(attr_idx),
                  AT_OBJ_NAME_PTR(ATP_RSLT_IDX(attr_idx)));
         fnd_err		= TRUE;
         AT_DCL_ERR(attr_idx)	= TRUE;
      }
      else {
         fnd_err = fnd_semantic_err(Obj_Pointer,
                                    line,
                                    column,
                                    attr_idx,
                                    TRUE);
      }

      if (!fnd_err && AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

         if (ATP_RSLT_IDX(attr_idx) == NULL_IDX) {
            CREATE_FUNC_RSLT(attr_idx, rslt_idx);
            ATP_PGM_UNIT(attr_idx)	= Function;
            SET_IMPL_TYPE(rslt_idx);
            attr_idx			= rslt_idx;
         }
         else {
            attr_idx = ATP_RSLT_IDX(attr_idx);
            fnd_err  = fnd_semantic_err(Obj_Pointer, 
                                        line,
                                        column,
                                        attr_idx,
                                        TRUE);
         }
      }

      if (!fnd_err) {

         if (ATD_POINTER(attr_idx)) {
#ifdef KEY /* Bug 5040 */
            PRINTMSG(line, 1259, ansi_or_warning(), column,
	      AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), "POINTER");
#else /* KEY Bug 5040 */
            PRINTMSG(line, 1259, Ansi, column, AT_OBJ_NAME_PTR(attr_idx),
                     "POINTER");
#endif /* KEY Bug 5040 */
         }
         ATD_POINTER(attr_idx)   = TRUE;
         ATD_IM_A_DOPE(attr_idx) = TRUE;
      }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
      ATD_POINTER(attr_idx)   = TRUE;
      ATD_IM_A_DOPE(attr_idx) = TRUE;
   }

   TRACE (Func_Exit, "merge_pointer", NULL);

   return(!fnd_err);

}  /* merge_pointer */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the SAVE attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the EXTERNAL attribute to.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_save(boolean	chk_semantics,
	           int		line,
		   int		column,
		   int		attr_idx)

{
   boolean	fnd_err		= FALSE;


   TRACE (Func_Entry, "merge_save", NULL);

   if (chk_semantics) {
      fnd_err = fnd_semantic_err(Obj_Saved, line, column, attr_idx, TRUE);

      if (!fnd_err && ATD_SAVED(attr_idx)) {
#ifdef KEY /* Bug 5040 */
         PRINTMSG(line, 1259, ansi_or_warning(), column,
	   AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), "SAVE");
#else /* KEY Bug 5040 */
         PRINTMSG(line, 1259, Ansi, column, AT_OBJ_NAME_PTR(attr_idx), "SAVE");
#endif /* KEY Bug 5040 */
      }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
   }

   if (!fnd_err) {
      ATD_SAVED(attr_idx) = TRUE;
      ATD_CLASS(attr_idx) = Variable;
   }

   TRACE (Func_Exit, "merge_save", NULL);

   return(!fnd_err);

}  /* merge_save */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the TARGET attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the TARGET attribute to.                *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_target(boolean	chk_semantics,
	             int	line,
		     int	column,
		     int	attr_idx)

{
   boolean	fnd_err		= FALSE;
   int		rslt_idx;


   TRACE (Func_Entry, "merge_target", NULL);

   if (AT_OBJ_CLASS(attr_idx) == Interface && 
       ATI_PROC_IDX(attr_idx) != NULL_IDX) {
       attr_idx = ATI_PROC_IDX(attr_idx);
   }

   if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit && ATP_RSLT_NAME(attr_idx)) {
      PRINTMSG(line, 132, Error, column, AT_OBJ_NAME_PTR(attr_idx),
               AT_OBJ_NAME_PTR(ATP_RSLT_IDX(attr_idx)));
      fnd_err			= TRUE;
      AT_DCL_ERR(attr_idx)	= TRUE;
   }
   else if (chk_semantics) {
      fnd_err = fnd_semantic_err(Obj_Target, line, column, attr_idx, TRUE);
   }
   else {
      SET_IMPL_TYPE(attr_idx);
   }

   if (!fnd_err && AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

      if (ATP_RSLT_IDX(attr_idx) == NULL_IDX) {
         CREATE_FUNC_RSLT(attr_idx, rslt_idx);
         ATP_PGM_UNIT(attr_idx)	= Function;
         SET_IMPL_TYPE(rslt_idx);
         attr_idx = rslt_idx;
      } 
      else {
         attr_idx = ATP_RSLT_IDX(attr_idx);
         fnd_err  = fnd_semantic_err(Obj_Target,
                                     line,
                                     column,
                                     attr_idx,
                                     TRUE);
      }
   }

   if (!fnd_err) {

      if (!fnd_err && ATD_TARGET(attr_idx)) {
#ifdef KEY /* Bug 5040 */
         PRINTMSG(line, 1259, ansi_or_warning(), column,
	   AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), "TARGET");
#else /* KEY Bug 5040 */
         PRINTMSG(line, 1259, Ansi, column, AT_OBJ_NAME_PTR(attr_idx),"TARGET");
#endif /* KEY Bug 5040 */
      }
      ATD_TARGET(attr_idx) = TRUE;
   }

   TRACE (Func_Exit, "merge_target", NULL);

   return(!fnd_err);

}  /* merge_target */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses integer specification expressions.  It is used by *|
|*      the parsing routines for array boundaries, character lengths, and     *|
|*      kind type parameters.  It takes a shortcut if there is just a literal *|
|*      integer constant like B(10).  It would pick this up itself without    *|
|*      calling the expression parser.  Expression semantics is called only   *|
|*      if input fold_it tells it do it.  This is because kind type parameters*|
|*      can be folded right away, but array and character bounds must wait    *|
|*      until the end of pass1.  If this doesn't come out to be a constant    *|
|*      value, tmp = is generated and the tmp is added to the bounds list.    *|
|*									      *|
|* Input parameters:							      *|
|*	fold_it          -> A flag that tells this routine whether to try to  *|
|*                          fold this value or not.                           *|
|*	kind_type        -> A flag that tells this routine is being called    *|
|*                          to process kind type or not.                      *|
|*									      *|
|* Output parameters:							      *|
|*	len_idx	    -> This is a ptr to a table index.  field_type tells what *|
|*                     kind of index this is.                                 *|
|*	field_type  -> This is a ptr to what kind of table index this is.  It *|
|*		       contains the fld_type enum. (AT_Tbl_Idx, IR_Tbl_Idx ect*|
|*									      *|
|* Returns:								      *|
|*	Returns TRUE if it parsed okay.					      *|
|*									      *|
\******************************************************************************/
#ifndef KEY /* Bug 10572 */
static
#endif /* KEY Bug 10572 */
boolean	parse_int_spec_expr(long		*len_idx,
				    fld_type		*field_type,
				    boolean		 fold_it,
				    boolean		 char_len)

{
   int			column;
   expr_arg_type	expr_desc;
   opnd_type		len_opnd;
   int			line;
   boolean		parse_ok;
   expr_mode_type	save_expr_mode	= expr_mode;
   int			type_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
   int			cvrt_idx;
   int			new_type;
# endif


   TRACE (Func_Entry, "parse_int_spec_expr", NULL);

   xref_state		= CIF_Symbol_Reference;
   *field_type		= CN_Tbl_Idx;
   *len_idx		= CN_INTEGER_ONE_IDX;
   expr_mode		= fold_it ? Initialization_Expr : Specification_Expr;
   line			= LA_CH_LINE;
   column		= LA_CH_COLUMN;
   expr_desc		= init_exp_desc;

   if (!parse_expr(&len_opnd)) {
      parse_ok	= FALSE;
      goto EXIT;
   }


   if (fold_it) {

      expr_desc.rank = 0;

      if (!expr_semantics(&len_opnd, &expr_desc)) {
         parse_ok	= FALSE;
         goto EXIT;
      }

      if (expr_desc.rank != 0) {
         PRINTMSG(line, 907, Error, column);
         parse_ok	= FALSE;
         goto EXIT;
      }

      if (OPND_FLD(len_opnd) != CN_Tbl_Idx) {
         PRINTMSG(line, 1531, Error, column);
         parse_ok	= FALSE;
         goto EXIT;
      }

      if (parsing_kind_selector) {
         if (expr_desc.kind0seen) {
            kind0seen = TRUE;
         }
         else if (expr_desc.kind0E0seen) {
            kind0E0seen = TRUE;
         }
         else if (expr_desc.kind0D0seen) {
            kind0D0seen = TRUE;
         }
         else if (! expr_desc.kindnotconst) {
            kindconstseen = TRUE;
         }
      }
   }

   parse_ok = TRUE;

   if (OPND_FLD(len_opnd) == CN_Tbl_Idx) {
      type_idx	= CN_TYPE_IDX(OPND_IDX(len_opnd));

      if (TYP_TYPE(type_idx) != Integer) {
    
         if (TYP_TYPE(type_idx) == Typeless) {

            if (TYP_LINEAR(type_idx) == Short_Typeless_Const) {
               PRINTMSG(line, 221, Ansi, column);

               OPND_IDX(len_opnd) = cast_typeless_constant(OPND_IDX(len_opnd),
                                                           INTEGER_DEFAULT_TYPE,
                                                           line, 
                                                           column);
               type_idx = INTEGER_DEFAULT_TYPE;
            }
            else { /* hollerith too long */
               PRINTMSG(line, 1133, Error, column);
               parse_ok = FALSE;
            }
         }
         else {
            PRINTMSG(line, 488, Error, column,
                     get_basic_type_str(type_idx));
            parse_ok	= FALSE;
         }
      }

      *len_idx          = (long) OPND_IDX(len_opnd);
      *field_type       = CN_Tbl_Idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)

      if (!parsing_kind_selector) {
         new_type	= NULL_IDX;

         if (char_len) {     /* All char lens must be integer_4 */

            if (TYP_LINEAR(CN_TYPE_IDX(OPND_IDX(len_opnd))) != Integer_4) {
               new_type	= Integer_4;
            }
         }
         else if (cmd_line_flags.s_pointer8 &&
                  TYP_LINEAR(CN_TYPE_IDX(OPND_IDX(len_opnd))) !=
                             SA_INTEGER_DEFAULT_TYPE) {
               new_type	= SA_INTEGER_DEFAULT_TYPE;
         }
         else if (TYP_LINEAR(CN_TYPE_IDX(OPND_IDX(len_opnd))) <
                             SA_INTEGER_DEFAULT_TYPE) {

            /* Bump up integer*1 and integer*2 to avoid overflows KAY  */

            new_type	= SA_INTEGER_DEFAULT_TYPE;
         }

         if (new_type != NULL_IDX) {
            NTR_IR_TBL(cvrt_idx);
            IR_OPR(cvrt_idx)		= Cvrt_Opr;
            IR_TYPE_IDX(cvrt_idx)	= new_type;
            IR_LINE_NUM(cvrt_idx)	= line;
            IR_COL_NUM(cvrt_idx)	= column;

            COPY_OPND(IR_OPND_L(cvrt_idx), len_opnd);

            OPND_IDX(len_opnd)		= cvrt_idx;
            OPND_FLD(len_opnd)		= IR_Tbl_Idx;

            if (fold_it) {
               expr_desc.rank = 0;

               if (!expr_semantics(&len_opnd, &expr_desc)) {
                  parse_ok	= FALSE;
                  goto EXIT;
               }

               *len_idx		= (long) OPND_IDX(len_opnd); 
               *field_type	= CN_Tbl_Idx;
            }
            else {
               *field_type      		= AT_Tbl_Idx;
               *len_idx				= ntr_bnds_tmp_list(&len_opnd);
               ATD_TMP_HAS_CVRT_OPR(*len_idx)	= TRUE;
            }
         }
         else {
            *len_idx	= (long) OPND_IDX(len_opnd); 
            *field_type	= CN_Tbl_Idx;
         }
      }

# endif

   }
   else {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
      new_type	= NULL_IDX;

      if (!parsing_kind_selector) { 

         if (char_len) {     /* All char lens must be integer_4 */
            new_type	= Integer_4;
         }
         else if (cmd_line_flags.s_pointer8) { 
            new_type	= SA_INTEGER_DEFAULT_TYPE;
         }

         if (new_type != NULL_IDX) {
            NTR_IR_TBL(cvrt_idx);
            IR_OPR(cvrt_idx)		= Cvrt_Opr;
            IR_TYPE_IDX(cvrt_idx)	= new_type;
            IR_LINE_NUM(cvrt_idx)	= line;
            IR_COL_NUM(cvrt_idx)	= column;

            COPY_OPND(IR_OPND_L(cvrt_idx), len_opnd);

            OPND_IDX(len_opnd)		= cvrt_idx;
            OPND_FLD(len_opnd)		= IR_Tbl_Idx;

            if (fold_it) {
               expr_desc.rank = 0;

               if (!expr_semantics(&len_opnd, &expr_desc)) {
                  parse_ok	= FALSE;
                  goto EXIT;
               }
            }
         }
      }

      *field_type       = AT_Tbl_Idx;
      *len_idx		= ntr_bnds_tmp_list(&len_opnd);

      ATD_TMP_SEMANTICS_DONE(*len_idx) = fold_it;

      if (new_type != NULL_IDX) {
         ATD_TMP_HAS_CVRT_OPR(*len_idx) = TRUE;
      }
# else
      *field_type       = AT_Tbl_Idx;
      *len_idx		= ntr_bnds_tmp_list(&len_opnd);

      ATD_TMP_SEMANTICS_DONE(*len_idx) = fold_it;
# endif

   }

EXIT:

   expr_mode = save_expr_mode;

   TRACE (Func_Exit, "parse_int_spec_expr", NULL);

   return(parse_ok);

}  /* parse_int_spec_expr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine takes a bounds ir stream, and searches the bounds tmp    *|
|*      list for a match.  If a match is found, it returns the attr_idx of    *|
|*      the matched bound.  If there is no match, a compiler temp is          *|
|*      generated and added to the end of the bounds tmp list.   This assumes *|
|*      that the ir pointed to by ATD_TMP_IDX is always of the form           *|
|*      TMP = ir_stream, so it passes to compare_ir the right operand of      *|
|*      the compiler temp.  And then if a new temp is needed, this routine    *|
|*      generates the TMP =.                                                  *|
|*									      *|
|* Input parameters:							      *|
|*	opnd   A pointer to an operand pointing to the attribute or ir stream *|
|*	       that needs a temp.  This should NOT have TMP = generated yet.  *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	attr_idx  Index to attr table for this temp.                          *|
|*									      *|
\******************************************************************************/
static int ntr_bnds_tmp_list (opnd_type	*opnd)

{
   int		al_idx;
   int		attr_idx;
   int		cif_attr	= NULL_IDX;
   int		column;
   int		ir_idx;
   int		line;
   int		prev_al		= NULL_IDX;


   TRACE (Func_Entry, "ntr_bnds_tmp_list", NULL);

   al_idx	= SCP_TMP_FW_IDX(curr_scp_idx);
   attr_idx	= NULL_IDX;

   while (al_idx != NULL_IDX) {
      attr_idx	= AL_ATTR_IDX(al_idx);

      if (ATD_CLASS(attr_idx) == Constant) {
         
         /* This tmp has resolved to a constant, because it has gone   */
         /* thru expr_semantics already.  Remove it from the tmp list. */
         /* A tmp may go through expr_semantics early, if the item     */
         /* needs to be folded because it is referenced in a bound of  */
         /* a component declaration.  prev_al stays the same.          */

         al_idx = AL_NEXT_IDX(al_idx);

         if (prev_al == NULL_IDX) {
            SCP_TMP_FW_IDX(curr_scp_idx) = al_idx;
         }
         else {
            AL_NEXT_IDX(prev_al)	 = al_idx;
         }
         continue;
      }

      /* Okay to pass a pointer to the operand here, because it should */
      /* not move.  This is only a call to compare operands.           */

      if (compare_opnds(opnd, &(IR_OPND_R(ATD_TMP_IDX(attr_idx)))) ) {

         /* If this is CIF - retain the IR, even though the tmp gets shared */

         if ((cif_flags & XREF_RECS) != 0) {

            if (cif_attr == NULL_IDX) {
               cif_attr	= attr_idx;
            }
         }
         else {

            if (OPND_FLD((*opnd)) == IR_Tbl_Idx) { 
               free_ir_stream(OPND_IDX((*opnd)));
            }
            goto EXIT;
         }
      }

      prev_al	= al_idx;
      al_idx	= AL_NEXT_IDX(al_idx);
   }

   /* Each new bound has to be added at the end of the list. */

   NTR_ATTR_LIST_TBL(al_idx);

   if (prev_al == NULL_IDX) {
      SCP_TMP_FW_IDX(curr_scp_idx)	= al_idx;
   }
   else {
      AL_NEXT_IDX(prev_al)		= al_idx;
   }
   find_opnd_line_and_column(opnd, &line, &column);

   GEN_COMPILER_TMP_ASG(ir_idx, 
                        attr_idx,
                        FALSE,		/* Tmp should go through semantics */
                        line,
                        column,
                        INTEGER_DEFAULT_TYPE,
                        Priv);

   AL_ATTR_IDX(al_idx)		= attr_idx;

   COPY_OPND(IR_OPND_R(ir_idx), (*opnd));	/* IR_OPND_R = *opnd  */

   if (cif_attr != NULL_IDX) {

      /* This tmp is only being kept for CIF - Return the tmp to be shared. */

      AT_REFERENCED(attr_idx)	= Not_Referenced;
      attr_idx			= cif_attr;
   }

EXIT:

   TRACE (Func_Exit, "ntr_bnds_tmp_list", NULL);

   return (attr_idx);

}  /* ntr_bnds_tmp_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse a POSSIBLE generic spec.  If OPERATOR(operator) or ASSIGNMENT   *|
|*	  (=) is found, search for (create if necessary)  an attr entry.      *|
|*	  If identifier found, return attr index if found, else 0.     	      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	attr index of new attribute.                       	 	      *|
|*									      *|
\******************************************************************************/

int	generic_spec_semantics(void) 

{
   int		attr_idx;
   boolean	generic_name;
   int		host_attr_idx;
   int		host_name_idx;
   int		name_idx;
   boolean	new_attr	= FALSE;
   int		new_attr_idx;
   int		scp_idx;
   int		type_idx;


   TRACE (Func_Entry, "generic_spec_semantics", NULL);

   generic_name = TOKEN_VALUE(token) == Tok_Id;
   attr_idx	= srch_sym_tbl(TOKEN_STR(token),
                               TOKEN_LEN(token),
                               &name_idx);

   if (stmt_type == Interface_Stmt) {

      if (attr_idx == NULL_IDX) {  /* Didn't find entry in local scope */
         host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token), 
                                           TOKEN_LEN(token),
                                           &host_name_idx,
                                           TRUE);

         if (host_attr_idx == NULL_IDX ||
            AT_OBJ_CLASS(host_attr_idx) != Interface) {

            /* The only host association in this situation, is if the   */
            /* host is an INTERFACE.  If it is not, make a LN_DEF_LOC   */
            /* interface entry in the local scope.                      */

            attr_idx			= ntr_sym_tbl(&token, name_idx);
            AT_OBJ_CLASS(attr_idx)	= Interface;
            LN_DEF_LOC(name_idx)	= TRUE;
            new_attr			= TRUE;

            if (generic_name) {
               ATI_INTERFACE_CLASS(attr_idx) = Generic_Unknown_Interface;
            }
         }
         else if (AT_NOT_VISIBLE(host_attr_idx)) {
            PRINTMSG(TOKEN_LINE(token), 486, Error,
                     TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(host_attr_idx),
                     AT_OBJ_NAME_PTR(AT_MODULE_IDX((host_attr_idx))));
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            AT_OBJ_CLASS(attr_idx)	= Interface;
            LN_DEF_LOC(name_idx)	= TRUE;
            new_attr			= TRUE;

            if (generic_name) {
               ATI_INTERFACE_CLASS(attr_idx) = Generic_Unknown_Interface;
            }
         }
         else { /* Found interface in host.  Make a new entry in the local   */
                /* scp and copy the attr.  LN_DEF_LOC gets set in local scp. */

            if (AT_IS_INTRIN(host_attr_idx) &&
                ATI_FIRST_SPECIFIC_IDX(host_attr_idx) == NULL_IDX) {
               complete_intrinsic_definition(host_attr_idx);
               attr_idx	= srch_sym_tbl(TOKEN_STR(token), 
                                       TOKEN_LEN(token), 
                                       &name_idx);
            }

            attr_idx = ntr_host_in_sym_tbl(&token, 
                                           name_idx,
                                           host_attr_idx,
                                           host_name_idx,
                                           TRUE);

            type_idx = (AT_TYPED(host_attr_idx)) ? ATD_TYPE_IDX(host_attr_idx) :
                                                   NULL_IDX;

            COPY_VARIANT_ATTR_INFO(host_attr_idx, attr_idx, Interface);

            LN_DEF_LOC(name_idx)	  = TRUE;
            AT_ATTR_LINK(attr_idx)	  = NULL_IDX;
            AT_IS_INTRIN(attr_idx)	  = AT_IS_INTRIN(host_attr_idx);
            AT_ELEMENTAL_INTRIN(attr_idx) = AT_ELEMENTAL_INTRIN(host_attr_idx);
            ATD_TYPE_IDX(attr_idx)	  = type_idx;
            AT_DEF_LINE(attr_idx)	  = TOKEN_LINE(token);
            AT_DEF_COLUMN(attr_idx)	  = TOKEN_COLUMN(token);
         }
      }
      else if ((!AT_USE_ASSOCIATED(attr_idx) ||
                 AT_OBJ_CLASS(attr_idx) != Pgm_Unit ||
                 ATP_PROC(attr_idx) != Module_Proc) &&
               fnd_semantic_err(Obj_Generic_Interface,
                                TOKEN_LINE(token),
                                TOKEN_COLUMN(token),
                                attr_idx,
                                TRUE)) {
         CREATE_ERR_ATTR(attr_idx, TOKEN_LINE(token),
                                   TOKEN_COLUMN(token), Interface);
         AT_OBJ_CLASS(attr_idx)	= Interface;
      }
      else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

         /* Must be a function or subroutine */

         NTR_ATTR_TBL(new_attr_idx);
         COPY_COMMON_ATTR_INFO(attr_idx, new_attr_idx, Interface);
         AT_DEF_LINE(new_attr_idx)	= TOKEN_LINE(token);
         AT_DEF_COLUMN(new_attr_idx)	= TOKEN_COLUMN(token);
         ATI_PROC_IDX(new_attr_idx)	= attr_idx;
         LN_ATTR_IDX(name_idx)		= new_attr_idx;
         LN_NAME_IDX(name_idx)		= AT_NAME_IDX(new_attr_idx);

         if (ATP_RSLT_IDX(attr_idx) != NULL_IDX && 
             AT_TYPED(ATP_RSLT_IDX(attr_idx))) {
            ATD_TYPE_IDX(new_attr_idx) = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));
         }

         attr_idx			= new_attr_idx;

         if (generic_name) {
            ATI_INTERFACE_CLASS(attr_idx) = Generic_Unknown_Interface;
         }
      }
      else if (AT_OBJ_CLASS(attr_idx) != Interface) {
         scp_idx		= curr_scp_idx;
         curr_scp_idx		= INTRINSIC_SCP_IDX;
         host_attr_idx		= srch_sym_tbl(TOKEN_STR(token), 
                                               TOKEN_LEN(token),
                                               &host_name_idx);
         curr_scp_idx		= scp_idx;

         if (host_attr_idx == NULL_IDX) {
            CLEAR_VARIANT_ATTR_INFO(attr_idx, Interface);
            type_idx		= NULL_IDX;
         }
         else {  /* It is an intrinsic */
            complete_intrinsic_definition(host_attr_idx);
            COPY_VARIANT_ATTR_INFO(host_attr_idx, attr_idx, Interface);
            AT_ATTR_LINK(attr_idx)	  = NULL_IDX;
            AT_IS_INTRIN(attr_idx)	  = AT_IS_INTRIN(host_attr_idx);
            AT_ELEMENTAL_INTRIN(attr_idx) = AT_ELEMENTAL_INTRIN(host_attr_idx);
            type_idx			  = ATD_TYPE_IDX(host_attr_idx);
         }

         ATD_TYPE_IDX(attr_idx)		  = type_idx;
         AT_DEF_LINE(attr_idx)		  = TOKEN_LINE(token);
         AT_DEF_COLUMN(attr_idx)	  = TOKEN_COLUMN(token);
      }
   }
   else if (CURR_BLK == Module_Blk) {		/* Public/Private statement */

      if (attr_idx == NULL_IDX) {
         attr_idx		= ntr_sym_tbl(&token, name_idx);
         LN_DEF_LOC(name_idx)	= TRUE;
         new_attr		= TRUE;

         if (generic_name) {
            SET_IMPL_TYPE(attr_idx);
         }
         else {
            AT_OBJ_CLASS(attr_idx)	= Interface;
         }
      }  /* NOT_VISIBLE and other semantics are done by the caller */
   }
   else if (attr_idx == NULL_IDX) {
      attr_idx			= ntr_sym_tbl(&token, name_idx);
      LN_DEF_LOC(name_idx)	= TRUE;
      new_attr			= TRUE;

      if (generic_name) {
         SET_IMPL_TYPE(attr_idx);
      }
      else {
         AT_OBJ_CLASS(attr_idx)	= Interface;
      }
   }
   else if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
      AT_ATTR_LINK(attr_idx)   = NULL_IDX;
      LN_DEF_LOC(name_idx)     = TRUE;
   }


   /* CIF processing uses ATI_USER_SPECIFIED and AT_IS_INTRINSIC to determine */
   /* that an intrinsic procedure name is being overloaded.  Must check that  */
   /* the current statement is an interface block statement because this      */
   /* routine is also called to parse the generic name in a PUBLIC/PRIVATE    */
   /* statement.							      */

   if (stmt_type == Interface_Stmt  && 
       AT_OBJ_CLASS(attr_idx) == Interface  &&  generic_name) {
      ATI_USER_SPECIFIED(attr_idx) = TRUE;
   }


   if (new_attr && !generic_name) {

      switch (TOKEN_VALUE(token)) {
      case Tok_Op_Add :
         ATI_DEFINED_OPR(attr_idx)     = Plus_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Unary_Or_Binary_Interface;
         break;

      case Tok_Op_Div :
         ATI_DEFINED_OPR(attr_idx)     = Div_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Mult :
         ATI_DEFINED_OPR(attr_idx)     = Mult_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Power :
         ATI_DEFINED_OPR(attr_idx)     = Power_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Sub :
         ATI_DEFINED_OPR(attr_idx)     = Minus_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Unary_Or_Binary_Interface;
         break;

      case Tok_Op_Concat :
         ATI_DEFINED_OPR(attr_idx)     = Concat_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Eq :
         ATI_DEFINED_OPR(attr_idx)     = Eq_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Ge :
         ATI_DEFINED_OPR(attr_idx)     = Ge_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Gt :
         ATI_DEFINED_OPR(attr_idx)     = Gt_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Le :
         ATI_DEFINED_OPR(attr_idx)     = Le_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Lt :
         ATI_DEFINED_OPR(attr_idx)     = Lt_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Ne :
         ATI_DEFINED_OPR(attr_idx)     = Ne_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Lg :
         ATI_DEFINED_OPR(attr_idx)     = Lg_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_And :
         ATI_DEFINED_OPR(attr_idx)     = And_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Eqv :
         ATI_DEFINED_OPR(attr_idx)     = Eqv_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Neqv :
         ATI_DEFINED_OPR(attr_idx)     = Neqv_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Not :
         ATI_DEFINED_OPR(attr_idx)     = Not_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Unary_Interface;
         break;

      case Tok_Op_Or :
         ATI_DEFINED_OPR(attr_idx)     = Or_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Binary_Interface;
         break;

      case Tok_Op_Assign :
         ATI_DEFINED_OPR(attr_idx)     = Asg_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Assign_Interface;
         break;

      case Tok_Op_Defined :
         ATI_DEFINED_OPR(attr_idx)     = Null_Opr;
         ATI_INTERFACE_CLASS(attr_idx) = Defined_Unary_Or_Binary_Interface;
         break;
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

   TRACE (Func_Exit, "generic_spec_semantics", NULL);

   return(attr_idx);

}  /* generic_spec_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check to see if attr was used to declare its own bounds.   An object  *|
|*	cannot be dimensioned after it is referenced, so an error situation   *|
|*	definitely exists.  This is a bit of compile time to check, but we    *|
|*	are in an error situation anyway.  This searches the bounds looking   *|
|*	for a reference to the attr in the bounds for the array.              *|
|*									      *|
|* Input parameters:							      *|
|*	attr index to look for                            	 	      *|
|*	bd index of dimension to searh 					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if an error is issued.					      *|
|*									      *|
\******************************************************************************/

static boolean	is_attr_referenced_in_bound(int		bd_idx,
					    int		attr_idx) 

{
   boolean	error		= FALSE;
   opnd_type	opnd;
   int		rank;


   TRACE (Func_Entry, "is_attr_referenced_in_bound", NULL);

   if (BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {

      for (rank = BD_RANK(bd_idx); rank >0; rank--) {

         if (BD_LB_FLD(bd_idx, rank) == AT_Tbl_Idx &&
             ATD_FLD(BD_LB_IDX(bd_idx, rank)) == IR_Tbl_Idx &&
             find_attr_in_ir(attr_idx, 
                             ATD_TMP_IDX(BD_LB_IDX(bd_idx, rank)),
                             &opnd)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
            BD_DCL_ERR(bd_idx)		= TRUE;
            error			= TRUE;
            PRINTMSG(OPND_LINE_NUM(opnd), 1036, Error,
                     OPND_COL_NUM(opnd),
                     AT_OBJ_NAME_PTR(attr_idx));
            break;
         }

         if (BD_UB_FLD(bd_idx, rank) == AT_Tbl_Idx &&
             ATD_FLD(BD_UB_IDX(bd_idx, rank)) == IR_Tbl_Idx &&
             find_attr_in_ir(attr_idx, 
                             ATD_TMP_IDX(BD_UB_IDX(bd_idx, rank)),
                             &opnd)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
            BD_DCL_ERR(bd_idx)		= TRUE;
            error			= TRUE;
            PRINTMSG(OPND_LINE_NUM(opnd), 1036, Error,
                     OPND_COL_NUM(opnd),
                     AT_OBJ_NAME_PTR(attr_idx));
            break;
         }
      }
   }

   TRACE (Func_Exit, "is_attr_referenced_in_bound", NULL);

   return(error);

}  /* is_attr_referenced_in_bound */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parses the pe array_spec for declarations (F--)                       *|
|*             array_spec is explicit-shape-spec-list                         *|
|*                            is [lower-bound :]upper-bound                   *|
|*                               [specification-expr :] specification-expr    *|
|*                       is assumed-size-spec                                 *|
|*                          is [explicit-shape-spec-list,] [lower-bound:]*    *|
|*                                                                            *|
|*       Position  - entry - token is open brkt                               *|
|*                   exit  - token is verified close brkt                     *|
|*                           if close brkt is missing.  LA_CH is set to       *|
|*                           colon-colon, or EOS                              *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
int     parse_pe_array_spec(int    attr_idx)

{
   int                  bd_idx;
   int                  column;
   boolean              fold_it;
   boolean              found_end               = FALSE;
   boolean              found_error             = FALSE;
   fld_type             lb_fld;
   long                 lb_len_idx;
   int                  line;
   boolean              lower_bound_found;
   boolean              non_constant_size       = FALSE;
   int                  rank                    = 1;
   reference_type       referenced;
   fld_type             ub_fld;
   long                 ub_len_idx;


   TRACE (Func_Entry, "parse_pe_array_spec", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != LBRKT) {
      PRINTMSG(LA_CH_LINE, 295, Internal, LA_CH_COLUMN,
               "parse_pe_array_spec", "LBRKT");
   }
# endif

   NEXT_LA_CH;                                               /* Skip Lbrkt   */
   bd_idx                       = reserve_array_ntry(7);
   referenced                   = (reference_type) AT_REFERENCED(attr_idx);
   AT_REFERENCED(attr_idx)      = Not_Referenced;
   BD_LINE_NUM(bd_idx)          = LA_CH_LINE;
   BD_COLUMN_NUM(bd_idx)        = LA_CH_COLUMN;

   /* If LA_CH is RBRKT, there is no dimension, so default to a rank 1       */
   /* constant sized array of length 1 and return.                            */

   if (LA_CH_VALUE == RBRKT) {
      parse_err_flush(Find_None, "dimension-spec");
      BD_ARRAY_CLASS(bd_idx)    = Explicit_Shape;
      BD_ARRAY_SIZE(bd_idx)     = Constant_Size;
      BD_DCL_ERR(bd_idx)        = TRUE;
      BD_RANK(bd_idx)           = 1;
      BD_LB_FLD(bd_idx, 1)      = CN_Tbl_Idx;
      BD_LB_IDX(bd_idx, 1)      = CN_INTEGER_ONE_IDX;
      BD_UB_FLD(bd_idx, 1)      = CN_Tbl_Idx;
      BD_UB_IDX(bd_idx, 1)      = CN_INTEGER_ONE_IDX;
      NEXT_LA_CH;
      goto EXIT;
   }

   /* Set fold_it flag.  Will continue on and do pass2 style semantic     */
   /* checking and constant folding, if this is a component declaration.  */

   fold_it = (CURR_BLK == Derived_Type_Blk);

   do {  /* Process each dimension of the array */
      lower_bound_found         = FALSE;
      lb_len_idx                = CN_INTEGER_ONE_IDX;
      lb_fld                    = CN_Tbl_Idx;
      ub_len_idx                = NULL_IDX;
      ub_fld                    = NO_Tbl_Idx;

      if (LA_CH_VALUE != COLON && LA_CH_VALUE != STAR) {
         line           = LA_CH_LINE;
         column         = LA_CH_COLUMN;

         /* If LA_CH isn't a COLON or a STAR, then this must be an expression.*/
         /* Get the expression and determine if it is a lower or upper bound. */
         /* If there is a parse error, a constant one is returned.            */

         if (!parse_int_spec_expr(&ub_len_idx, &ub_fld, fold_it, FALSE)) {
            ub_len_idx          = CN_INTEGER_ONE_IDX;
            ub_fld              = CN_Tbl_Idx;
            BD_DCL_ERR(bd_idx)  = TRUE;
         }

         if (ub_fld != CN_Tbl_Idx) {
            non_constant_size   = TRUE;
         }

         if (LA_CH_VALUE == COLON) {                   /* This is lower bound */
            lower_bound_found           = TRUE;
            lb_len_idx                  = ub_len_idx;
            lb_fld                      = ub_fld;
            ub_len_idx                  = NULL_IDX;
            ub_fld                      = NO_Tbl_Idx;
         }
         else {
            BD_ARRAY_CLASS(bd_idx)      = Explicit_Shape;
         }
      }

      /* Now the parser is in one of 3 states. (1) Lower bound found, upper   */
      /* bound = NULL, LA_CH must be COLON. (2) Lower bound not found, so it  */
      /* is set to a default of 1, upper bound is found and LA_CH is COMMA or */
      /* RBRKT.   (LA_CH must be COLON.) (3) Neither lower bound or upper    */
      /* bound have been seen, they are set to defaults of lb=1, ub=NULL.     */
      /* LA_CH is COLON or STAR.  If the LA_CH is COLON, this is either a     */
      /* Deferred-Shape spec or it is followed by the upper bound for an      */
      /* Explicit-Shape spec.  NOTE: LA_CH may be EOS - this is parse error.  */

      if (LA_CH_VALUE == COLON) {
         line           = LA_CH_LINE;
         column         = LA_CH_COLUMN;
         NEXT_LA_CH;                            /* Skip COLON */

         if (LA_CH_VALUE == COMMA || LA_CH_VALUE == RBRKT) {

            /* Have one of two cases  1)  ARRAY(1:)  - This is an assumed     */
            /* shape spec which is classed as a Deferred-Shape, or 2) ARRAY(:)*/
            /* which is a deferred-Shape spec.   Issue an error if this array */
            /* has already been classified as an Explicit_Shape array.        */

            if (BD_ARRAY_CLASS(bd_idx) == Explicit_Shape) {
               PRINTMSG(line, 115, Error, column);
               BD_DCL_ERR(bd_idx)       = TRUE;
            }
            else {  /* Must be Deferred-Shape spec */
               BD_ARRAY_CLASS(bd_idx)   = Deferred_Shape;
            }
         }
         else {

            /* Have one of two cases  1)  ARRAY (1:10) - legal - pick up upper*/
            /* bound expression.  Err if array is already set to Deferred-    */
            /* Shape spec.  2) ARRAY (:10) - illegal - issue error.           */
            /* If the upper bound is a STAR, pick it up in the next section.  */

            if (!lower_bound_found) {                   /* A(:10) - illegal   */
               PRINTMSG(LA_CH_LINE, 119, Error, LA_CH_COLUMN, &LA_CH_VALUE);
               BD_DCL_ERR(bd_idx)       = TRUE;
            }

            if (LA_CH_VALUE != STAR) {
               line     = LA_CH_LINE;
               column   = LA_CH_COLUMN;

               if (!parse_int_spec_expr(&ub_len_idx, &ub_fld, fold_it, FALSE)) {

                  /* Expression parser recovers LA_CH to : ) , or EOS */

                  BD_DCL_ERR(bd_idx)    = TRUE;
                  ub_len_idx            = CN_INTEGER_ONE_IDX;
                  ub_fld                = CN_Tbl_Idx;
               }

               if (ub_fld != CN_Tbl_Idx) {
                  non_constant_size     = TRUE;
               }

               BD_ARRAY_CLASS(bd_idx)= Explicit_Shape;
            }
         }
      }

      /* The parser may be:  1)  ARRAY(*) - lb=1, ub=NULL_IDX.  2) ARR(10:*)  */
      /* lb is set, and ub=NULL_IDX.  3)  ARRAY(:*) - illegal -error already  */
      /* issued.  You could not have picked up a lower bound and/or an upper  */
      /* bound and got to this position, because a * is part of an expression.*/
      /* The expression parser stops at COLON, COMMA, RBRKT or EOS.          */

      if (LA_CH_VALUE == STAR) {
         line   = LA_CH_LINE;
         column = LA_CH_COLUMN;
         NEXT_LA_CH;                            /* Skip STAR */

         BD_ARRAY_CLASS(bd_idx)      = Assumed_Size;
         ub_len_idx                  = lb_len_idx;
         ub_fld                      = lb_fld;

         if (LA_CH_VALUE != RBRKT) {

            /* The assumed-size specifier * must be in the last dimension. */

            BD_DCL_ERR(bd_idx)       = TRUE;
            PRINTMSG(line, 116, Error, column);
            parse_err_flush(Find_Rparen, NULL);
         }
      }

      BD_LB_IDX(bd_idx, rank)   = lb_len_idx;
      BD_LB_FLD(bd_idx, rank)   = lb_fld;
      BD_UB_IDX(bd_idx, rank)   = ub_len_idx;
      BD_UB_FLD(bd_idx, rank)   = ub_fld;

      if (LA_CH_VALUE == COMMA) {

         if (rank++ == 7) {          /* issue error - too many ranks */
            found_end           = TRUE;
            BD_DCL_ERR(bd_idx)  = TRUE;
            PRINTMSG(LA_CH_LINE, 117, Error, LA_CH_COLUMN);
            parse_err_flush(Find_Rparen, NULL);
         }
         else {
            NEXT_LA_CH;
         }
      }
      else {
         found_end = TRUE;
      }

      found_error = BD_DCL_ERR(bd_idx) | found_error;
   }
   while (!found_end);

   if (LA_CH_VALUE == RBRKT ||
       parse_err_flush(Find_EOS, (found_error) ? NULL : ", or )")) {

      NEXT_LA_CH;               /* Skip RBRKT */
   }

   if (!non_constant_size) {
      BD_ARRAY_SIZE(bd_idx) = Constant_Size;
   }

   BD_RANK(bd_idx) = rank;

# ifdef _DEBUG
   if (BD_ARRAY_CLASS(bd_idx) == Unknown_Array) {

      /* There is a parsing problem here.  This must never be Unknown_Array */

      PRINTMSG(LA_CH_LINE, 178, Internal, LA_CH_COLUMN);
   }
# endif

EXIT:

   if (AT_REFERENCED(attr_idx) > Not_Referenced) {
      is_attr_referenced_in_bound(bd_idx, attr_idx);
   }

   if (AT_REFERENCED(attr_idx) < referenced) {
      AT_REFERENCED(attr_idx)   = referenced;
   }

   bd_idx = ntr_array_in_bd_tbl(bd_idx);

   TRACE (Func_Exit, "parse_pe_array_spec", NULL);

   return(bd_idx);

}  /* parse_pe_array_spec */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the co-array attribute to the attr.             		      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the Attr.        *|
|*	line          -> The line number of the object.                       *|
|*	column        -> The column number of the object.                     *|
|*	attr_idx      -> Attr index to add the co-array attribute to.         *|
|*	pe_array_idx  -> bounds table index describing the co-array.          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_co_array(boolean	chk_semantics,
	           int		line,
		   int		column,
		   int		attr_idx,
		   int		pe_array_idx)
{
   boolean	fnd_err;


   TRACE (Func_Entry, "merge_co_array", NULL);
   
   if (!chk_semantics || !fnd_semantic_err(Obj_Co_Array,
                                           line,
                                           column,
                                           attr_idx,
                                           TRUE)) {
      ATD_PE_ARRAY_IDX(attr_idx) = pe_array_idx;
      fnd_err	= FALSE;
   }
   else {
      fnd_err	= TRUE;
   }

   TRACE (Func_Exit, "merge_co_array", NULL);

   return(!fnd_err);

}  /* merge_co_array */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add the SAVE attribute to an attr.			      	      *|
|*									      *|
|* Input parameters:							      *|
|*	chk_semantics -> TRUE if semantic checking needs to be done.  If this *|
|*	                 is FALSE, just add the attribute to the attr.        *|
|*	line     -> The line number of the object to add the attribute to     *|
|*	column   -> The line number of the object to add the attribute to     *|
|*	attr_idx -> Attr index to add the EXTERNAL attribute to.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if successful merge.					      *|
|*									      *|
\******************************************************************************/

boolean merge_volatile(boolean	chk_semantics,
	               int		line,
		       int		column,
		       int		attr_idx)

{
   boolean	fnd_err		= FALSE;


   TRACE (Func_Entry, "merge_volatile", NULL);

   if (chk_semantics) {
      fnd_err = fnd_semantic_err(Obj_Volatile, line, column, attr_idx, TRUE);

#ifdef KEY /* Bug 14110 */
      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
	 ATD_INTENT(attr_idx) == Intent_In) {
	 PRINTMSG(line, 550, Error, column, AT_OBJ_NAME_PTR(attr_idx),
	    "INTENT(IN)", "VOLATILE", AT_DEF_LINE(attr_idx));
      }
#endif /* KEY Bug 14110 */
      if (!fnd_err && ATD_VOLATILE(attr_idx)) {
#ifdef KEY /* Bug 5040 */
         PRINTMSG(line, 1259, ansi_or_warning(), column,
	   AT_OBJ_NAME_PTR(attr_idx), AT_DEF_LINE(attr_idx), "VOLATILE");
#else /* KEY Bug 5040 */
         PRINTMSG(line, 1259, Ansi, column, AT_OBJ_NAME_PTR(attr_idx), 
                  "VOLATILE");
#endif /* KEY Bug 5040 */
      }
   }
   else {
      SET_IMPL_TYPE(attr_idx);
   }

   if (!fnd_err) {
      ATD_VOLATILE(attr_idx)	= TRUE;
   }

   TRACE (Func_Exit, "merge_volatile", NULL);

   return(!fnd_err);

}  /* merge_volatile */
#ifdef KEY /* Bug 14150 */
/*
 *	BNF is
 *		( C )
 *	or
 *	      	( C [, NAME = scalar-char-initialization-expr ] )
 *
 *  result	If null on input, then we don't allow the optional clause.
 *		Otherwise, we set TOKEN_LINE(*result) and TOKEN_COLUMN(*result).
 *		We set BIND_SPECIFIES_NAME(*result) to indicate whether the
 *		optional clause is present, and if so, we set TOKEN_STR(*result)
 *		and TOKEN_LEN(*result). Note that the presence of the optional
 *		clause is semantically different than its absence, even if the
 *		string is empty.
 *
 *  returns	1 if parsed ok, 0 if erroneous
 */
int
parse_language_binding_spec(token_type *result) {
  int ok = 1;
  if (result) {
    TOKEN_LEN(*result) = 0;
    TOKEN_LINE(*result) = LA_CH_LINE;
    TOKEN_COLUMN(*result) = LA_CH_COLUMN;
    SET_BIND_SPECIFIES_NAME(*result, FALSE);
  }

  if (LA_CH_VALUE != LPAREN) {
    parse_err_flush(Find_EOS, "(");
    return 0;
  }
  NEXT_LA_CH; /* Consume left paren */
  if (LA_CH_VALUE != 'C' && LA_CH_VALUE != 'c') {
    parse_err_flush(Find_EOS, "C");
    return 0;
  }
  NEXT_LA_CH; /* Consume 'c' */
  if (result) {
    if (LA_CH_VALUE == COMMA) {
      NEXT_LA_CH; /* Consume comma */
      if (!matched_specific_token(Tok_Kwd_Name, Tok_Class_Keyword)) {
	parse_err_flush(Find_Rparen, "NAME");
	if (LA_CH_VALUE == RPAREN) {
	  NEXT_LA_CH;
	}
	return 0;
      }
      if (LA_CH_VALUE != EQUAL) {
	parse_err_flush(Find_Rparen, "=");
	if (LA_CH_VALUE == RPAREN) {
	  NEXT_LA_CH;
	}
	return 0;
      }
      NEXT_LA_CH; /* Consume equal */
      opnd_type opnd;
      check_type_conversion = FALSE; /* TRUE causes exp_desc to be wrong */
      target_type_idx = 0;
      target_char_len_idx = 0;
      expr_arg_type exp_desc = init_exp_desc;
      exp_desc.rank = 0;
      expr_mode_type save_expr_mode = expr_mode;
      expr_mode = Initialization_Expr;
      xref_state = CIF_Symbol_Reference;
      comp_gen_expr = TRUE;
      if (parse_expr(&opnd) && expr_semantics(&opnd, &exp_desc)) {
	TOKEN_LINE(*result) = opnd.line_num;
	TOKEN_COLUMN(*result) = opnd.col_num;
        if (exp_desc.type != Character ||
	  exp_desc.linear_type != Short_Char_Const ||
	  !exp_desc.constant || exp_desc.rank) {
	  PRINTMSG(opnd.line_num, 1690, Error, opnd.col_num);
	  ok = 0;
	  }
	else {
	  int len = CN_INT_TO_C(exp_desc.char_len.idx);
	  /* Sigh. CN table doesn't null terminate. */
	  memcpy(TOKEN_STR(*result), (char *) &CN_CONST(opnd.idx), len);
	  TOKEN_LEN(*result) = len;
	  SET_BIND_SPECIFIES_NAME(*result, TRUE);
	}
      }
    expr_mode = save_expr_mode;
    }
  }
  if (LA_CH_VALUE != RPAREN) {
    parse_err_flush(Find_Rparen, ")");
    if (LA_CH_VALUE == RPAREN) {
      NEXT_LA_CH; /* Consume right paren */
      return 0;
    }
  }
  NEXT_LA_CH; /* Consume right paren */
  return ok;
}
#endif /* KEY Bug 14150 */
