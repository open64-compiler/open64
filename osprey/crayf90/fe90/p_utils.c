/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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



static char USMID[] = "\n@(#)5.0_pl/sources/p_utils.c	5.5	09/09/99 12:47:48\n";

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

static boolean  create_kwd_text(opnd_type *, boolean);
static void check_cmic_blk_branches(int, int, int, int);
static void block_err_string(operator_type, char *, int *);

extern boolean star_expected;


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine calls get token to try and retrieve the specified token. *|
|*									      *|
|* Input parameters:							      *|
|*	specific_token	- The token to get.				      *|
|*	token_class	- The class of the token to get.		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if the requested token is found.				      *|
|*									      *|
\******************************************************************************/

boolean matched_specific_token (token_values_type	specific_token,
				token_class_type	token_class)
{
   boolean		match			= FALSE;
   la_type		save_la;
   token_type		save_token;
   boolean		valid_token;


   TRACE (Func_Entry, "matched_specific_token", NULL);

   if (LA_CH_CLASS == Ch_Class_EOS && specific_token != Tok_EOS) { 

      /* this check is here because we can't consume the  */
      /* EOS and then back up, so it's special cased here */

      match = FALSE;
   }
   else {
      save_token	= token;
      save_la		= la_ch;
      valid_token	= get_token (token_class);

      if (valid_token && TOKEN_VALUE(token) == specific_token) {
            match = TRUE;
      }
      else {	/* restore things to the way they looked upon entry */
         token = save_token;
         la_ch = save_la;
         reset_src_input(LA_CH_BUF_IDX, LA_CH_STMT_NUM);
      }
   }

   TRACE (Func_Exit, "matched_specific_token",
					(match ? TOKEN_STR(token) : NULL));
   return  (match);

}  /* matched_specific_token */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      First this routine, determines if a reparse is necessary.  This is    *|
|*      done for ALL calls.  Msg printing and flushing are controlled by      *|
|*      input parameters.   This routine prints a parser error message, and   *|
|*      can flush the LA char to try to recover parsing.   Input parameters   *|
|*      control exactly what is done.  (Whether to print a msg, how many args *|
|*      the msg is called with, whether the bad char is in token form, or     *|
|*      only LA form, and what to flush to.  Set error flag on IR statement   *|
|*      header, if one exists and an error message is being printed.          *|
|*                                                                            *|
|*	NOTE:  If a flush is done, TOKEN is UNDEFINED.  LA_CH is set to       *|
|*	       the item being searched for, or to EOS.                        *|
|*									      *|
|* Input parameters:							      *|
|*	rule    -> If the parser is in recovery mode, this is what to flush   *|
|*                 the LA char to.  The rules are:		              *|
|*	           Find_None        -> Leave LA char as is.                   *|
|*	           Find_EOS         -> Search so LA is EOS.                   *|
|*	           Find_Rparen      -> Search so LA is rparen or EOS.         *|
|*	           Find_Lparen      -> Search so LA is lparen or EOS.         *|
|*	           Find_Comma       -> Search so LA is comma or EOS.          *|
|*	           Find_Comma_Rparen-> Search so LA is comma, an unmatched    *|
|*	                               right paren, or EOS                    *|
|*	           Find_Comma_Slash -> Search so LA is comma, slash or EOS.   *|
|*	           Find_Expr_End    -> Search for end of expr.  ) , : or EOS. *|
|*	str     -> Pointer to string to pass as 1st arg to PRINTMSG.          *|
|*	           NULL if NO message should be printed.                      *|
|*									      *|
|* Note:  LA_CH is used for the description, the following is printed         *|
|*	           LA_CH_CLASS         description sent to PRINTMSG           *|
|*	           Ch_Class_Letter     Keyword or Identifier                  *|
|*	           Ch_Class_Symbol                                            *|
|*	           Ch_Class_Digit      The lookahead character                *|
|*	           Ch_Class_EOS        End of Statement                       *|
|*	           Ch_Class_Dir and Ch_Class_EOF should not be seen here.     *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if the item being searched for is FOUND.  ie - if rule =         *|
|*	     Find_Comma, then TRUE is returned if Comma is found, else FALSE. *|
|*	     If Colon_Recovery is on, and :: is found, will return FALSE.     *|
|*									      *|
\******************************************************************************/

boolean parse_err_flush (search_type	 rule,
			 char		*str)

{
#ifdef KEY /* Bug 10177 */
   boolean	 found_end = FALSE;
#else /* KEY Bug 10177 */
   boolean	 found_end;
#endif /* KEY Bug 10177 */
   char		*new_str;
   int		 paren_level;
   boolean	 found;


   TRACE (Func_Entry, "parse_err_flush", search_str[rule]);

   if (str != NULL) {
      LA_CH_TO_ERR_STR(new_str, la_ch);
      PRINTMSG(LA_CH_LINE, 197, Error, LA_CH_COLUMN, str, new_str);
   }

   if (rule == Find_EOS) {
      flush_LA_to_EOS();
      found_end = TRUE;
   }
   else if (rule != Find_None) {

      /* If LA_CH is open paren - set to find matching closed paren */
      paren_level	=  0;
      found		= FALSE;
      found_end		= FALSE;

      if (rule == Find_Ref_End) {
         
         if (LA_CH_CLASS != Ch_Class_Symbol &&
             LA_CH_VALUE != EOS)            {
            flush_LA_to_symbol();
         }
         paren_level = 0;
      }

      do {

         if (rule == Find_Ref_End && paren_level == 0) {
            found = TRUE;
         }

         switch (LA_CH_VALUE) {
            case RPAREN:
               if (paren_level == 0) {

                  /* Matching rparen if looking for paren,         */
                  /*   or closing paren if looking for comma_paren */

                  if (rule == Find_Rparen || rule == Find_Comma_Rparen ||
                      rule == Find_Expr_End) {
                     found = TRUE;
                  }
               }
               else {
                  paren_level--;

                  if (paren_level == 0 && rule == Find_Matching_Rparen) {
                     found = TRUE;
                  }
                  else if (rule == Find_Ref_End) {
                     found = FALSE;
                  }
               }
               break;

            case LPAREN:
	       if (rule == Find_Lparen) {
		  found = TRUE;
	       }
	       else {
                  paren_level++;

                  if (rule == Find_Ref_End) {
                     found = FALSE;
                  }
	       }
               break;

            case COMMA:
               if (paren_level == 0 && rule >= Find_Comma) {
                  found = TRUE;
               }
               break;

            case SLASH:

               /* Check paren level to prevent picking up (/ or /) slashes */

               if (paren_level == 0 && rule == Find_Comma_Slash) {
                  found = TRUE;
               }
               else if (rule == Find_Expr_End &&
                        paren_level == 0      &&
                        matched_specific_token(Tok_Punct_Rbrkt,
                                               Tok_Class_Punct)) {
                  found = TRUE;
                  reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
               }
               break;

            case COLON:
               if (rule == Find_Expr_End &&
                           matched_specific_token(Tok_Punct_Colon,
                                                  Tok_Class_Punct)) {
                  found = TRUE;
                  reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
               }
               else if (colon_recovery &&
                  matched_specific_token(Tok_Punct_Colon_Colon,
                                         Tok_Class_Punct)) {
                  found		= TRUE;
                  found_end	= TRUE;
                  reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
               }
               break;

            case EOS:
               found		= TRUE;
               found_end	= TRUE;
               break;

            case PERCENT:
            case USCORE:
            case DOLLAR:
            case AT_SIGN:
            
               if (rule == Find_Ref_End) {
                  found = FALSE;
               } 
               break;

          } /* End switch */

          if (!found) {
             flush_LA_to_symbol();  /* This skips char constants & hollerith */
          }
       }
       while (!found);
   }

   TRACE (Func_Exit, "parse_err_flush", &LA_CH_VALUE);

   return(!found_end);

}  /* parse_err_flush */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Creates kwd_opr subtrees.                                             *|
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

static boolean  create_kwd_text(opnd_type *result_opnd,
                                boolean    function_call)

{
   int		attr_idx;
   int		ir_idx;
   int		kwd_idx;
   opnd_type	opnd;
   boolean	parsed_ok	= TRUE;
   la_type	save_la;
   int		type_idx;


   TRACE (Func_Entry, "create_kwd_text", NULL);

   /* the kwd identifier should already be tokenized and LA should be '=' */

# ifdef _DEBUG
   if (LA_CH_VALUE != EQUAL) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "create_kwd_text", "EQUAL");
   }
# endif

   NTR_IR_TBL(kwd_idx);
   OPND_FLD((*result_opnd))	= IR_Tbl_Idx;
   OPND_IDX((*result_opnd))	= kwd_idx;
   IR_FLD_L(kwd_idx)		= CN_Tbl_Idx;
   IR_OPR(kwd_idx)		= Kwd_Opr;
   IR_TYPE_IDX(kwd_idx)         = TYPELESS_DEFAULT_TYPE;
   
   IR_LINE_NUM(kwd_idx)		= LA_CH_LINE;
   IR_COL_NUM(kwd_idx)		= LA_CH_COLUMN;

   IR_LINE_NUM_L(kwd_idx)	= TOKEN_LINE(token);
   IR_COL_NUM_L(kwd_idx)	= TOKEN_COLUMN(token);

   /* put kwd identifier in constant table */

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Character;
   TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
   TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
   TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
   TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                              TOKEN_LEN(token));
   type_idx			= ntr_type_tbl();
   IR_IDX_L(kwd_idx)		= ntr_const_tbl(type_idx,
                                                TRUE,
                                   (long_type *)&(TOKEN_STR_WD(token,0)));

   NEXT_LA_CH;			/* swallow = */

   /* get expression for right opnd */
   /* float scalarness and ambiguity */
   
   if (LA_CH_VALUE == STAR && !function_call) {
      NEXT_LA_CH;

      if (LA_CH_CLASS == Ch_Class_Digit &&
          MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
          ! TOKEN_ERR(token)) {

         attr_idx = check_label_ref();

         IR_FLD_R(kwd_idx)	= AT_Tbl_Idx;
         IR_IDX_R(kwd_idx)	= attr_idx;
         IR_LINE_NUM_R(kwd_idx)	= TOKEN_LINE(token);
         IR_COL_NUM_R(kwd_idx)	= TOKEN_COLUMN(token);
      }
      else if (TOKEN_ERR(token)) {
         parse_err_flush(Find_Comma_Rparen, NULL);
         parsed_ok = FALSE;
      }
      else {
         parse_err_flush(Find_Comma_Rparen, "LABEL");
         parsed_ok = FALSE;
      }
   }
   else {

      if (LA_CH_VALUE == PERCENT) {
         save_la = la_ch;
         NEXT_LA_CH;

         MATCHED_TOKEN_CLASS(Tok_Class_Id);

         if (TOKEN_LEN(token) == 3 &&
             strncmp(TOKEN_STR(token), "VAL", 3) == 0 &&
             LA_CH_VALUE == LPAREN) {

            NEXT_LA_CH;   /* swallow ( */

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx) = Percent_Val_Opr;
            IR_LINE_NUM(ir_idx) = save_la.line;
            IR_COL_NUM(ir_idx) = save_la.column;
            IR_FLD_R(kwd_idx) = IR_Tbl_Idx;
            IR_IDX_R(kwd_idx) = ir_idx;

            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (LA_CH_VALUE != RPAREN) {
               parse_err_flush(Find_EOS,")");
               parsed_ok = FALSE;
            }
            else {
               NEXT_LA_CH;
            }
         }
         else {
            reset_lex(save_la.stmt_buf_idx, save_la.stmt_num);
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IR_OPND_R(kwd_idx), opnd);
         }
      }
      else {
         parsed_ok = parse_expr(&opnd) && parsed_ok;
         COPY_OPND(IR_OPND_R(kwd_idx), opnd);
      }
   }

   TRACE (Func_Exit, "create_kwd_text", NULL);

   return(parsed_ok);

} /* create_kwd_text */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      parse_actual_arg_spec will create a call text from an actual arg      *|
|*      list. It processes keyword arguments.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      index of call text                                                    *|
|*                                                                            *|
\******************************************************************************/

boolean parse_actual_arg_spec (opnd_type *result_opnd,
                               boolean    function_call,
                               int        pgm_attr_idx)

{
   int          arg_cnt = 0;
   int          attr_idx;
   boolean      had_keyword = FALSE;
   int		ir_idx;
   boolean      issued_msg_128 = FALSE;
   int          list_idx;
   int          list2_idx;
   opnd_type    opnd;
   boolean      parsed_ok = TRUE;
   la_type      save_la;


   TRACE (Func_Entry, "parse_actual_arg_spec", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != LPAREN) {
      /* shouldn't be here */
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "parse_actual_arg_spec", "LPAREN");
   } 
# endif

   OPND_FLD((*result_opnd)) = IL_Tbl_Idx;
   OPND_IDX((*result_opnd)) = NULL_IDX;
   list2_idx = NULL_IDX;

   do {
      NEXT_LA_CH;

      if (LA_CH_VALUE == RPAREN && arg_cnt == 0) {
         break;
      }

      NTR_IR_LIST_TBL(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;

      if (list2_idx == NULL_IDX) {
         OPND_IDX((*result_opnd)) = list_idx;
      }
      else {
         IL_NEXT_LIST_IDX(list2_idx) = list_idx;
      }
      list2_idx = list_idx;

      if (LA_CH_VALUE == STAR && !function_call) {

         NEXT_LA_CH;

         if (LA_CH_CLASS == Ch_Class_Digit && 
             MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
             ! TOKEN_ERR(token)) {

            attr_idx = check_label_ref();
            if (AT_OBJ_CLASS(pgm_attr_idx) == Pgm_Unit) {
               ATP_HAS_ALT_RETURN(pgm_attr_idx) = TRUE;
            }

            IL_FLD(list_idx)      = AT_Tbl_Idx;
            IL_IDX(list_idx)      = attr_idx;
            IL_LINE_NUM(list_idx) = TOKEN_LINE(token);
            IL_COL_NUM(list_idx)  = TOKEN_COLUMN(token);
         }
         else if (TOKEN_ERR(token)) {
            parse_err_flush(Find_Comma_Rparen, NULL);
            parsed_ok = FALSE;
         }
         else {
            parse_err_flush(Find_Comma_Rparen, "LABEL");
            parsed_ok = FALSE;
         }
      }
      else if (next_arg_is_kwd_equal()) {
         MATCHED_TOKEN_CLASS(Tok_Class_Id);

            /* have keyword */
            had_keyword = TRUE;
            /* put keyword on constant tbl */
            parsed_ok = create_kwd_text(&opnd, function_call) && parsed_ok;
            COPY_OPND(IL_OPND(list_idx), opnd);
      } 
      else { /* had id but not kwd, must reparse as expression */

         if (had_keyword) {
            /* error */

            if (! issued_msg_128) {
               PRINTMSG(LA_CH_LINE, 128, Error,
                        LA_CH_COLUMN,NULL);
               issued_msg_128 = TRUE;
               parsed_ok = FALSE;
            }
         }

         if (LA_CH_VALUE == PERCENT) {
            save_la = la_ch;
            NEXT_LA_CH;

            MATCHED_TOKEN_CLASS(Tok_Class_Id);

            if (TOKEN_LEN(token) == 3 &&
                strncmp(TOKEN_STR(token), "VAL", 3) == 0 &&
                LA_CH_VALUE == LPAREN) {

               NEXT_LA_CH;   /* swallow ( */

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx) = Percent_Val_Opr;
               IR_LINE_NUM(ir_idx) = save_la.line;
               IR_COL_NUM(ir_idx) = save_la.column;
               IL_FLD(list_idx) = IR_Tbl_Idx;
               IL_IDX(list_idx) = ir_idx;

               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
               
               if (LA_CH_VALUE != RPAREN) {
                  parse_err_flush(Find_EOS,")");
                  parsed_ok = FALSE;
               }
               else {
                  NEXT_LA_CH;
               }
            }
            else {
               reset_lex(save_la.stmt_buf_idx, save_la.stmt_num);
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list_idx), opnd);
            }
         }
         else {
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list_idx), opnd);
         }
      }

      arg_cnt++;
   }
   while (LA_CH_VALUE == COMMA);

   OPND_LIST_CNT((*result_opnd)) = arg_cnt;

   /* this global variable is used to set the max size for arrays in */
   /* call_list_semantics.                                           */
   if (arg_cnt > max_call_list_size) {
      max_call_list_size = arg_cnt;
   }

   if (LA_CH_VALUE != RPAREN) {
      parse_err_flush(Find_EOS,", or )");
      parsed_ok = FALSE;
   }
   else {
      NEXT_LA_CH;
   }

   TRACE (Func_Exit, "parse_actual_arg_spec", NULL);

   return(parsed_ok);

} /* parse_actual_arg_spec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parse structure/array dereference                                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      struct_type_idx - idx for derived type of previous id, NULL_IDX       *|
|*                        if no previous id.                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      result_opnd - opnd_type, points to root of tree returned.             *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if parsed ok                                                     *|
|*                                                                            *|
\******************************************************************************/

boolean parse_deref (opnd_type *result_opnd, 
                     int        struct_type_idx)

{

   boolean       ambiguous_ref = FALSE;
   int           amb_attr_idx;
   int           array_idx;
   int           attr_idx;
   token_type    attr_name;
   int           check_attr;
   int           col;
   int           host_attr_idx;
   int           host_name_idx;
   int           i;
   int           j;
   int           ir_idx;
   int           line;
   int           list_idx;
   int           list2_idx;
   int           list3_idx;
   int           name_idx;
   int           new_attr_idx;
   int		 num_dims;
   opnd_type     opnd;
   boolean       parsed_ok = TRUE;
   int           rank;
   int           rslt_idx;
   int           save_curr_scp_idx;
   int           sn_idx;
   int		 struct_idx = NULL_IDX;
   int           subs_idx = NULL_IDX;
   int           substring_idx;
   token_type    tmp_token;
   int           trip_idx;
   int           type_idx;


   TRACE (Func_Entry, "parse_deref", NULL);

   attr_name = token;
   
   if (struct_type_idx) {		/* test for valid structure component */
      sn_idx	= ATT_FIRST_CPNT_IDX(struct_type_idx);
      attr_idx	= srch_linked_sn(TOKEN_STR(token),
                                 TOKEN_LEN(token),
                                 &sn_idx);

      if (attr_idx == NULL_IDX) {

         if (!AT_DCL_ERR(struct_type_idx)) {
            PRINTMSG(TOKEN_LINE(token), 213, Error,
                     TOKEN_COLUMN(token), TOKEN_STR(token),
                     AT_OBJ_NAME_PTR(struct_type_idx));
         }
         else {
            SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         }

         parse_err_flush(Find_Ref_End, NULL);
         parsed_ok = FALSE;
         goto EXIT;
      }
      
      if (AT_USE_ASSOCIATED(struct_type_idx) && 
          ATT_PRIVATE_CPNT(struct_type_idx)) {

         if (!AT_DCL_ERR(struct_type_idx)) {
            PRINTMSG(TOKEN_LINE(token), 882, Error,
                     TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(struct_type_idx),
                     TOKEN_STR(token));
         }
         else {
            SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         }

         parse_err_flush(Find_Ref_End, NULL);
         parsed_ok = FALSE;
         goto EXIT;
      }

      /* Reference to something of a derived type causes the derived   */
      /* type to be locked in.  In other words, it cannot be redefined */
      /* if it is host associated.                                     */

      AT_LOCKED_IN(struct_type_idx) = TRUE;
      amb_attr_idx = attr_idx;

      struct_idx                = OPND_IDX((*result_opnd));
      IR_FLD_R(struct_idx)      = AT_Tbl_Idx;
      IR_IDX_R(struct_idx)      = attr_idx;
      IR_LINE_NUM_R(struct_idx) = TOKEN_LINE(token);
      IR_COL_NUM_R(struct_idx)  = TOKEN_COLUMN(token);
   }
   else {
      attr_idx = srch_sym_tbl(TOKEN_STR(attr_name), 
                              TOKEN_LEN(attr_name),
                              &name_idx);

      if (attr_idx != NULL_IDX) {  /* the name was found locally */

         /* copy intrinsic attr to the local scope from the 0th scope */
#ifdef KEY
         if (LA_CH_VALUE == LPAREN &&
             AT_REFERENCED(attr_idx) == Not_Referenced &&
             !AT_NAMELIST_OBJ(attr_idx) &&
             ((AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
               strncasecmp(TOKEN_STR(attr_name), "omp_",4) == 0) ||
             (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
              ATD_CLASS(attr_idx) == Atd_Unknown &&
             !ATD_ALLOCATABLE(attr_idx) &&
             !ATD_TARGET(attr_idx) &&
             !ATD_POINTER(attr_idx) &&
              ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
              (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Character ||
               ! is_substring_ref())))) {
#else
         if (LA_CH_VALUE == LPAREN &&
             AT_REFERENCED(attr_idx) == Not_Referenced &&
             !AT_NAMELIST_OBJ(attr_idx) &&
              AT_OBJ_CLASS(attr_idx) == Data_Obj &&
              ATD_CLASS(attr_idx) == Atd_Unknown &&
             !ATD_ALLOCATABLE(attr_idx) &&
             !ATD_TARGET(attr_idx) &&
             !ATD_POINTER(attr_idx) &&
              ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
              (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Character ||
               ! is_substring_ref())) {
#endif

            /* search INTRINSIC host for the INTRINSIC in question */
            save_curr_scp_idx = curr_scp_idx;
            curr_scp_idx = INTRINSIC_SCP_IDX;
            host_attr_idx = srch_sym_tbl(TOKEN_STR(attr_name),
                                         TOKEN_LEN(attr_name), 
                                         &host_name_idx);
            curr_scp_idx = save_curr_scp_idx;

            if (host_attr_idx != NULL_IDX) {

               if (AT_IS_INTRIN(host_attr_idx) &&
                   ATI_FIRST_SPECIFIC_IDX(host_attr_idx) == NULL_IDX) {
                  complete_intrinsic_definition(host_attr_idx);

                  attr_idx = srch_sym_tbl(TOKEN_STR(attr_name), 
                                          TOKEN_LEN(attr_name),
                                          &name_idx);
               }

               type_idx = (AT_TYPED(attr_idx)) ? ATD_TYPE_IDX(attr_idx) : 
                                                 NULL_IDX;

               COPY_VARIANT_ATTR_INFO(host_attr_idx, 
                                      attr_idx, 
                                      AT_OBJ_CLASS(host_attr_idx));

               ATD_TYPE_IDX(attr_idx)	= type_idx;
               AT_IS_INTRIN(attr_idx)	= AT_IS_INTRIN(host_attr_idx);
               AT_ELEMENTAL_INTRIN(attr_idx)=AT_ELEMENTAL_INTRIN(host_attr_idx);
               host_attr_idx		= NULL_IDX;
            }
         }

         amb_attr_idx = attr_idx;

         if (!LN_DEF_LOC(name_idx)) {
            ambiguous_ref = TRUE;

            while (AT_ATTR_LINK(amb_attr_idx) != NULL_IDX) {
               amb_attr_idx = AT_ATTR_LINK(amb_attr_idx);
            }
         }
      }
      else {
         /* any other reference is ambiguous */
         ambiguous_ref = TRUE;

         /* search host sym tab */
#ifdef KEY
         if (strncasecmp(TOKEN_STR(attr_name), "omp_",4) == 0){
           save_curr_scp_idx = curr_scp_idx;
           curr_scp_idx = INTRINSIC_SCP_IDX;
           host_attr_idx = srch_sym_tbl(TOKEN_STR(attr_name),
                                        TOKEN_LEN(attr_name), 
                                        &host_name_idx);
           curr_scp_idx = save_curr_scp_idx;
           if (!host_attr_idx)
             host_attr_idx = srch_host_sym_tbl(TOKEN_STR(attr_name),
                                               TOKEN_LEN(attr_name), 
                                               &host_name_idx,
                                               TRUE);
         }
         else
#endif
           host_attr_idx = srch_host_sym_tbl(TOKEN_STR(attr_name),
                                             TOKEN_LEN(attr_name), 
                                             &host_name_idx,
                                             TRUE);


         if (host_attr_idx != NULL_IDX && IS_STMT_ENTITY(host_attr_idx)) {

            /* do not hook up to host stmt entities */

            host_attr_idx = NULL_IDX;
         }
             
         /* if we are copying info down from the host scope */

         if (host_attr_idx != NULL_IDX) {
            if (LA_CH_VALUE != LPAREN &&
                AT_IS_INTRIN(host_attr_idx) &&
                AT_OBJ_CLASS(host_attr_idx) == Interface) {
               host_attr_idx = NULL_IDX;
            }
         }

         if (host_attr_idx != NULL_IDX) {

            /* TRUE means make a new attr and link it to the old one. */

            attr_idx = ntr_host_in_sym_tbl(&attr_name, 
                                           name_idx,
                                           host_attr_idx, 
                                           host_name_idx, 
                                           TRUE);

            amb_attr_idx = host_attr_idx;

            while (AT_ATTR_LINK(amb_attr_idx) != NULL_IDX) {
               amb_attr_idx = AT_ATTR_LINK(amb_attr_idx);
            }

            if (LA_CH_VALUE == LPAREN &&
                AT_IS_INTRIN(amb_attr_idx) && 
                AT_OBJ_CLASS(amb_attr_idx) == Interface) {

               /* copy intrinsic attr to the local scope from the 0th scope */
               /* and break the link to the host scope.                     */

               if (AT_IS_INTRIN(host_attr_idx) &&
                   ATI_FIRST_SPECIFIC_IDX(host_attr_idx) == NULL_IDX) {
                  complete_intrinsic_definition(host_attr_idx);
               }
               COPY_ATTR_NTRY(attr_idx, amb_attr_idx);
               AT_CIF_SYMBOL_ID(attr_idx)	= 0;
               AT_ATTR_LINK(attr_idx)		= NULL_IDX;
               host_attr_idx			= NULL_IDX;
               amb_attr_idx			= attr_idx;
               AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
               AT_DEF_COLUMN(attr_idx)		= TOKEN_COLUMN(token);
            }
         }
         else {
            attr_idx		= ntr_sym_tbl(&attr_name, name_idx);
            amb_attr_idx	= attr_idx;

            if (LA_CH_VALUE == LPAREN && ! is_substring_ref()) {

               /* set function on attr */

               AT_OBJ_CLASS(attr_idx)		= Pgm_Unit;
               ATP_PROC(attr_idx)		= Unknown_Proc;
               ATP_PGM_UNIT(attr_idx)		= Function;
               ATP_SCP_IDX(attr_idx)		= curr_scp_idx;
               MAKE_EXTERNAL_NAME(attr_idx, 
                                  AT_NAME_IDX(attr_idx),
                                  AT_NAME_LEN(attr_idx));

               CREATE_FUNC_RSLT(attr_idx, new_attr_idx);

               if (expr_mode == Specification_Expr ||
                   expr_mode == Initialization_Expr ||
                   expr_mode == Stmt_Func_Expr) {
                  AT_REFERENCED(new_attr_idx) = Dcl_Bound_Ref;
               }
               else {
                  AT_REFERENCED(new_attr_idx) = Referenced;
               }
               SET_IMPL_TYPE(new_attr_idx);
            }
            else {
               SET_IMPL_TYPE(attr_idx);
            }
         }
      }

      if (AT_OBJ_CLASS(amb_attr_idx) == Interface) {

         if (ATI_FIRST_SPECIFIC_IDX(amb_attr_idx) == NULL_IDX) {
            check_attr = NULL_IDX;
         }
         else {
            check_attr = SN_ATTR_IDX(ATI_FIRST_SPECIFIC_IDX(amb_attr_idx));
         }
      }
      else {
         check_attr = amb_attr_idx;
      }

      if (check_attr != NULL_IDX && 
          AT_OBJ_CLASS(check_attr) == Pgm_Unit &&
          ATP_NON_ANSI_INTRIN(check_attr)) {
         PRINTMSG(TOKEN_LINE(attr_name), 
                  787, 
                  Ansi,   
                  TOKEN_COLUMN(attr_name), 
                  TOKEN_STR(attr_name));
      }

      /* put AT in result opnd for now */

      OPND_FLD((*result_opnd))      = AT_Tbl_Idx;
      OPND_IDX((*result_opnd))      = attr_idx;
      OPND_LINE_NUM((*result_opnd)) = TOKEN_LINE(token);
      OPND_COL_NUM((*result_opnd))  = TOKEN_COLUMN(token);

      if (in_implied_do) {

         if (IS_STMT_ENTITY(attr_idx) &&
             ATD_FIRST_SEEN_IL_IDX(attr_idx) == NULL_IDX) {

            /* need to keep track of line/col to determine if stmt entity */

            NTR_IR_LIST_TBL(ATD_FIRST_SEEN_IL_IDX(attr_idx));
            IL_LINE_NUM(ATD_FIRST_SEEN_IL_IDX(attr_idx)) = TOKEN_LINE(token);
            IL_COL_NUM(ATD_FIRST_SEEN_IL_IDX(attr_idx)) = TOKEN_COLUMN(token);
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_IN_IMP_DO(attr_idx) = TRUE;
         }
      }
      else if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
      }
   }

   /* if there was a problem with the local attr, quit */

   if (AT_DCL_ERR(attr_idx)) {
      SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;

      parse_err_flush(Find_Ref_End, NULL);
      parsed_ok = FALSE;
      goto EXIT;
   }

   /* if this is not ambiguous, and it is not visible, error. */

   if (! ambiguous_ref           &&
       AT_NOT_VISIBLE(attr_idx)) {

      PRINTMSG(TOKEN_LINE(token), 486, Error, TOKEN_COLUMN(token),
               AT_OBJ_NAME_PTR(attr_idx),
               AT_OBJ_NAME_PTR(AT_MODULE_IDX((attr_idx))));
      parse_err_flush(Find_Ref_End, NULL);
      parsed_ok = FALSE;
      goto EXIT;
   }

   /* Now lets see what this attr is */

   switch (AT_OBJ_CLASS(amb_attr_idx)) {
      case Data_Obj :

         if (ATD_SYMBOLIC_CONSTANT(amb_attr_idx)) {

            if (AT_DEF_LINE(amb_attr_idx) == 0) {
               AT_DEF_LINE(amb_attr_idx)	= TOKEN_LINE(token);
               AT_DEF_COLUMN(amb_attr_idx)	= TOKEN_LINE(token);
            }
         }
         break;

      case Pgm_Unit :

         if (ATP_SCP_ALIVE(amb_attr_idx) && 
             ATP_PGM_UNIT(amb_attr_idx) == Function) {
             rslt_idx = ATP_RSLT_IDX(amb_attr_idx);

            if (ATP_RSLT_NAME(amb_attr_idx) ||
                (LA_CH_VALUE == LPAREN &&
                 TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) != Structure &&
                 ATD_ARRAY_IDX(rslt_idx) == NULL_IDX &&
                 (TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) != Character ||
                  ! is_substring_ref()))) {

               /* must be a pgm unit */
           
               if (LA_CH_VALUE != LPAREN &&
                   LA_CH_VALUE != PERCENT) {

                  /* possibly a proc actual arg */
                  goto EXIT;
               }
               else if (LA_CH_VALUE != LPAREN) {
                  /* could have better message if we want. */

                  PRINTMSG(TOKEN_LINE(token), 722, Error, TOKEN_COLUMN(token),
                           AT_OBJ_NAME_PTR(attr_idx));
                  parse_err_flush(Find_Ref_End, NULL);
                  parsed_ok = FALSE;
                  goto EXIT;
               }
               else {
                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx)             = Call_Opr;
                  IR_FLD_L(ir_idx)           = AT_Tbl_Idx;
                  IR_IDX_L(ir_idx)           = attr_idx;
                  IR_LINE_NUM(ir_idx)        = TOKEN_LINE(token);
                  IR_COL_NUM(ir_idx)         = TOKEN_COLUMN(token);
                  IR_LINE_NUM_L(ir_idx)      = TOKEN_LINE(token);
                  IR_COL_NUM_L(ir_idx)       = TOKEN_COLUMN(token);
                  OPND_FLD((*result_opnd))   = IR_Tbl_Idx;
                  OPND_IDX((*result_opnd))   = ir_idx;

                  parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
                  COPY_OPND(IR_OPND_R(ir_idx), opnd);
                  goto EXIT;
               }
            }
            else {
               /* have data obj ref */
               attr_idx                 = rslt_idx;
               amb_attr_idx             = attr_idx;
               OPND_IDX((*result_opnd)) = attr_idx;
 
               /* continue on with further processing */
            }
         }
         else if (LA_CH_VALUE == LPAREN) {
         
            if (! ambiguous_ref                       &&
                ATP_PGM_UNIT(attr_idx) == Pgm_Unknown &&
                ATP_DCL_EXTERNAL(attr_idx))            {

               /* my assumption is that this has only been seen in EXTERNAL */
               /* so make into implicitly typed function.                   */

               ATP_PGM_UNIT(attr_idx)         = Function;
               CREATE_FUNC_RSLT(attr_idx, new_attr_idx);

               SET_IMPL_TYPE(new_attr_idx);
            }

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)             = Call_Opr;
            IR_FLD_L(ir_idx)           = AT_Tbl_Idx;
            IR_IDX_L(ir_idx)           = attr_idx;
            IR_LINE_NUM(ir_idx)        = TOKEN_LINE(token);
            IR_COL_NUM(ir_idx)         = TOKEN_COLUMN(token);
            IR_LINE_NUM_L(ir_idx)      = TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)       = TOKEN_COLUMN(token);
            OPND_FLD((*result_opnd))   = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))   = ir_idx;

            parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
            goto EXIT;
         }
         else {
            goto EXIT;
         }
            
         break;

      case Label    :
         /* shouldn't be here */
         parsed_ok = FALSE;
         goto EXIT;

      case Derived_Type :

         if (LA_CH_VALUE == LPAREN) {
            /* This is treated as a structure constructor */
            /* until further notice. If it turns out to   */
            /* be a function call, all the needed info is */
            /* retained.                                  */

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)                 = Struct_Construct_Opr;
            IR_FLD_L(ir_idx)               = AT_Tbl_Idx;
            IR_IDX_L(ir_idx)               = attr_idx;
            IR_LINE_NUM(ir_idx)            = TOKEN_LINE(token);
            IR_COL_NUM(ir_idx)             = TOKEN_COLUMN(token);
            IR_LINE_NUM_L(ir_idx)          = TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)           = TOKEN_COLUMN(token);
            OPND_FLD((*result_opnd))       = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))       = ir_idx;

            parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         else if (ambiguous_ref) {
            /* error */
            /* must be either structure constructor or function */
            PRINTMSG(TOKEN_LINE(token), 322, Error, TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(attr_idx));
            parse_err_flush(Find_Ref_End, NULL);
            parsed_ok = FALSE;
         }
         else {
            /* error */
            /* must be structure constructor */
            PRINTMSG(TOKEN_LINE(token), 151, Error, TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(attr_idx));
            parse_err_flush(Find_Ref_End, NULL);
            parsed_ok = FALSE;
         }
     
         goto EXIT;

      case Interface   :

         if (LA_CH_VALUE != LPAREN && AT_IS_INTRIN(amb_attr_idx)) {

            if (!ATI_INTRIN_PASSABLE(amb_attr_idx)) {
               PRINTMSG(TOKEN_LINE(token), 
                        860, 
                        Error,  
                        TOKEN_COLUMN(token), 
                        AT_OBJ_NAME_PTR(amb_attr_idx));
               AT_DCL_ERR(amb_attr_idx) = TRUE;
               goto EXIT;
            }

            /* generic intrinsic interface call */

            tmp_token = initial_token;
            TOKEN_COLUMN(tmp_token) = 1;
            TOKEN_LINE(tmp_token) = 1;

            for (i = 0; i < MAX_INTRIN_MAP_SIZE; i++) {
               if ((strcmp(AT_OBJ_NAME_PTR(attr_idx), 
                   (char *)&intrin_map[i].id_str) == 0)) {

#ifdef KEY /* Bug 3869 */
	       /* NINT is a special case: though it begins with "N" and its
	        * result type is integer, its argument type is real */
               if (0 == strcmp(AT_OBJ_NAME_PTR(attr_idx), "NINT")) {
		 tmp_token = initial_token;
		 TOKEN_COLUMN(tmp_token) = 1;
		 TOKEN_LINE(tmp_token) = 1;
		 strcat(
		   strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
		     (char *) ((INTEGER_DEFAULT_TYPE == Integer_4) ?
		       &intrin_map[i].mapped_4 :
		       &intrin_map[i].mapped_8)),
		   ((REAL_DEFAULT_TYPE == Real_4) ?
		     "" :
		     "_d")
		   );
		 break;
	       }
#endif /* KEY Bug 3869 */

                  if (INTEGER_DEFAULT_TYPE == Integer_1 ||
                      INTEGER_DEFAULT_TYPE == Integer_2 ||
                      INTEGER_DEFAULT_TYPE == Integer_4) {
                     if (intrin_map[i].id_str.string[0] == 'I' ||  
                         intrin_map[i].id_str.string[0] == 'N' ||  
                         intrin_map[i].id_str.string[0] == 'M' ||  
                         intrin_map[i].id_str.string[0] == 'L') {  
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_4);
                     }
                  }

                  if (INTEGER_DEFAULT_TYPE == Integer_8) {
                     if (intrin_map[i].id_str.string[0] == 'I' ||  
                         intrin_map[i].id_str.string[0] == 'N' ||  
                         intrin_map[i].id_str.string[0] == 'M' ||  
                         intrin_map[i].id_str.string[0] == 'L') {  
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_8);
                     }
                  }

                  if (REAL_DEFAULT_TYPE == Real_4) {
                     if (strcmp(AT_OBJ_NAME_PTR(attr_idx), "DIM") == 0 ||
                         strcmp(AT_OBJ_NAME_PTR(attr_idx), "DPROD") == 0) {
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_4);
                     }
                     else if (intrin_map[i].id_str.string[0] != 'I' &&
                              intrin_map[i].id_str.string[0] != 'N' &&
                              intrin_map[i].id_str.string[0] != 'M' &&
                              intrin_map[i].id_str.string[0] != 'D' &&
                              intrin_map[i].id_str.string[0] != 'L') {
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_4);
                     }
                  }

                  if (REAL_DEFAULT_TYPE == Real_8) {
                     if (strcmp(AT_OBJ_NAME_PTR(attr_idx), "DIM") == 0 ||
                         strcmp(AT_OBJ_NAME_PTR(attr_idx), "DPROD") == 0) {
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_8);
                     }
                     else if (intrin_map[i].id_str.string[0] != 'I' &&
                              intrin_map[i].id_str.string[0] != 'N' &&
                              intrin_map[i].id_str.string[0] != 'M' &&
                              intrin_map[i].id_str.string[0] != 'D' &&
                              intrin_map[i].id_str.string[0] != 'L') {
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_8);
                     }
                  }

                  if (DOUBLE_DEFAULT_TYPE == Real_8) {
                     if (intrin_map[i].id_str.string[0] == 'D' &&  
                         strcmp(AT_OBJ_NAME_PTR(attr_idx), "DPROD") != 0 &&
                         strcmp(AT_OBJ_NAME_PTR(attr_idx), "DIM") != 0) {
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_4);
                     }
                  }

                  if (DOUBLE_DEFAULT_TYPE == Real_16) {
                     if (intrin_map[i].id_str.string[0] == 'D' &&  
                         strcmp(AT_OBJ_NAME_PTR(attr_idx), "DPROD") != 0 &&
                         strcmp(AT_OBJ_NAME_PTR(attr_idx), "DIM") != 0) {
                        tmp_token = initial_token;
                        TOKEN_COLUMN(tmp_token) = 1;
                        TOKEN_LINE(tmp_token) = 1;
                        strcpy((char *)&(TOKEN_STR(tmp_token)[0]),
                               (char *)&intrin_map[i].mapped_8);
                     }
                  }

                  break;
               }
            }

            TOKEN_LEN(tmp_token) = strlen((char *)&(TOKEN_STR(tmp_token)[0]));
            TOKEN_VALUE(tmp_token) = Tok_Id;

            attr_idx = srch_sym_tbl(TOKEN_STR(tmp_token),
                                    TOKEN_LEN(tmp_token),
                                    &name_idx);

            if (attr_idx == NULL_IDX) {
               attr_idx			= ntr_sym_tbl(&tmp_token, name_idx);
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
            ATP_PROC(attr_idx)		= Intrin_Proc;   
            ATP_PGM_UNIT(attr_idx)	= Function;
            ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
            AT_IS_INTRIN(attr_idx)	= TRUE;
            MAKE_EXTERNAL_NAME(attr_idx, 
                               AT_NAME_IDX(attr_idx),
                               AT_NAME_LEN(attr_idx));
            ATP_INTERFACE_IDX(attr_idx) = amb_attr_idx;

            CREATE_FUNC_RSLT(attr_idx, rslt_idx);

            if (AT_TYPED(amb_attr_idx)) {
               ATD_TYPE_IDX(rslt_idx) = ATD_TYPE_IDX(amb_attr_idx);
            }
            else {
               j = ATI_INTRIN_TBL_IDX(amb_attr_idx)+1;

               if (intrin_tbl[j].data_type == Real_16 ||
                   intrin_tbl[j].data_type == Complex_16) {
                  if (cmd_line_flags.s_default64 ||
                      cmd_line_flags.s_float64) {
                     /* intentionally blank */
                  }
                  else {
                     j = j + 1;
                     while (intrin_tbl[j].intrin_enum == 0 &&
                            intrin_tbl[j].external == 0) {
                        j = j + 1;  /* skip over the dummy arguments */
                     }
                  }
               }

               ATD_TYPE_IDX(rslt_idx) = intrin_tbl[j].data_type;

# ifdef _TARGET64
               /* 
               The intrinsic table is designed for a 32 bit machine.
               Sizes must be doubled.
               */
               switch (intrin_tbl[j].data_type) {
                      case Real_4    : 
                                     ATD_TYPE_IDX(rslt_idx) =
                                     REAL_DEFAULT_TYPE;
                                     break;
                      case Real_8    : 
                                     ATD_TYPE_IDX(rslt_idx) =
                                     DOUBLE_DEFAULT_TYPE;
                                     break;
                      case Complex_4 : 
                                     ATD_TYPE_IDX(rslt_idx) =
                                     COMPLEX_DEFAULT_TYPE;
                                     break;
                      case Complex_8 : 
                                     ATD_TYPE_IDX(rslt_idx) =
                                     DOUBLE_COMPLEX_DEFAULT_TYPE;
                                     break;
                      case Integer_4 : 
                                     ATD_TYPE_IDX(rslt_idx) =
                                     INTEGER_DEFAULT_TYPE;
                                     break;
               }
# endif


# ifdef _TARGET32
               /* If in Cray compatability mode, we must double the sizes. */
               switch (intrin_tbl[j].data_type) {
                   case Real_4    :
                                     if (REAL_DEFAULT_TYPE == Real_8) {
                                        ATD_TYPE_IDX(rslt_idx) =
                                        REAL_DEFAULT_TYPE;
                                     }
                                     break;
                   case Real_8    :
                                     if (DOUBLE_DEFAULT_TYPE == Real_16) {
                                        ATD_TYPE_IDX(rslt_idx) =
                                        DOUBLE_DEFAULT_TYPE;
                                     }
                                     break;
                   case Complex_4 :
                                     if (COMPLEX_DEFAULT_TYPE == Complex_8) {
                                        ATD_TYPE_IDX(rslt_idx) =
                                        COMPLEX_DEFAULT_TYPE;
                                     }
                                     break;
                   case Complex_8 :
                                     if (COMPLEX_DEFAULT_TYPE == Complex_16) {
                                        ATD_TYPE_IDX(rslt_idx) =
                                        DOUBLE_COMPLEX_DEFAULT_TYPE;
                                     }
                                     break;
                   case Integer_4 :
                                     if (INTEGER_DEFAULT_TYPE == Integer_8) {
                                        ATD_TYPE_IDX(rslt_idx) =
                                        INTEGER_DEFAULT_TYPE;
                                     }
                                     break;
               }


               /* If double precision is disabled, half the size. */
               if ((ATD_TYPE_IDX(rslt_idx) == Real_8 ||
                    ATD_TYPE_IDX(rslt_idx) == Complex_8 ||
                    ATD_TYPE_IDX(rslt_idx) == Real_16 ||
                    ATD_TYPE_IDX(rslt_idx) == Complex_16) &&
                   !on_off_flags.enable_double_precision) {
                  j = j + 1;
                  while (intrin_tbl[j].intrin_enum == 0 &&
                         intrin_tbl[j].external == 0) {
                    j = j + 1;  /* skip over the dummy arguments */
                  }
                  ATD_TYPE_IDX(rslt_idx) = intrin_tbl[j].data_type;
               }
# endif

            }

            OPND_FLD((*result_opnd)) = AT_Tbl_Idx;
            OPND_IDX((*result_opnd)) = attr_idx;
         }
         else if (LA_CH_VALUE == LPAREN) {
            /* generic interface call or forward ref to function */
            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)             = Call_Opr;
            IR_FLD_L(ir_idx)           = AT_Tbl_Idx;
            IR_IDX_L(ir_idx)           = attr_idx;
            IR_LINE_NUM(ir_idx)        = TOKEN_LINE(token);
            IR_COL_NUM(ir_idx)         = TOKEN_COLUMN(token);
            IR_LINE_NUM_L(ir_idx)      = TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)       = TOKEN_COLUMN(token);
            OPND_FLD((*result_opnd))   = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))   = ir_idx;

            parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);

         }
         goto EXIT;

      case Namelist_Grp :

         if (ambiguous_ref && LA_CH_VALUE == LPAREN) {
            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)             = Call_Opr;
            IR_FLD_L(ir_idx)           = AT_Tbl_Idx;
            IR_IDX_L(ir_idx)           = attr_idx;
            IR_LINE_NUM(ir_idx)        = TOKEN_LINE(token);
            IR_COL_NUM(ir_idx)         = TOKEN_COLUMN(token);
            IR_LINE_NUM_L(ir_idx)      = TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)       = TOKEN_COLUMN(token);
            OPND_FLD((*result_opnd))   = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))   = ir_idx;

            parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         else {
            OPND_FLD((*result_opnd))       = AT_Tbl_Idx;
            OPND_IDX((*result_opnd))       = attr_idx;
            OPND_LINE_NUM((*result_opnd))  = TOKEN_LINE(token);
            OPND_COL_NUM((*result_opnd))   = TOKEN_COLUMN(token);
         }
         goto EXIT;

      case Stmt_Func    :

         if (LA_CH_VALUE == LPAREN) {
            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)             = Stmt_Func_Call_Opr;
            IR_FLD_L(ir_idx)           = AT_Tbl_Idx;
            IR_IDX_L(ir_idx)           = attr_idx;
            IR_LINE_NUM(ir_idx)        = TOKEN_LINE(token);
            IR_COL_NUM(ir_idx)         = TOKEN_COLUMN(token);
            IR_LINE_NUM_L(ir_idx)      = TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)       = TOKEN_COLUMN(token);
            OPND_FLD((*result_opnd))   = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))   = ir_idx;

            parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         else {
            parse_err_flush(Find_Ref_End, "(");
         }

         goto EXIT;
   }
            
# ifdef _F_MINUS_MINUS
   if (LA_CH_VALUE != PERCENT && LA_CH_VALUE != LPAREN && 
       ((!cmd_line_flags.co_array_fortran) || LA_CH_VALUE != LBRKT))
# else
   if (LA_CH_VALUE != PERCENT && LA_CH_VALUE != LPAREN)
# endif
                                                       {

      /* return the operand that came in */
      /* or the attr_idx.                */
      /* intentionally blank             */

      goto EXIT;
   }


   if (LA_CH_VALUE == LPAREN) {
      /* do that array stuff */
      array_idx = ATD_ARRAY_IDX(amb_attr_idx);

      if (array_idx) {

         rank = 0;
         NTR_IR_TBL(subs_idx);

         /* copy either the struct subtree that was passed in, */
         /* or the attr_idx                                    */
         COPY_OPND(IR_OPND_L(subs_idx), (*result_opnd));

         /* put subs_idx into result opnd for now */
         OPND_FLD((*result_opnd))      = IR_Tbl_Idx;
         OPND_IDX((*result_opnd))      = subs_idx;

         /* LA_CH is '(' */
         IR_LINE_NUM(subs_idx)         = LA_CH_LINE;
         IR_COL_NUM(subs_idx)          = LA_CH_COLUMN;

         IR_OPR(subs_idx)              = Subscript_Opr;
         IR_FLD_R(subs_idx)            = IL_Tbl_Idx;
      
         list_idx = NULL_IDX;

         do {
            NEXT_LA_CH;

            if (ambiguous_ref) {

               if (LA_CH_VALUE == RPAREN) {
                  /* could be function with no args */
                  break;
               }
               else if (next_arg_is_kwd_equal ()) {
                  MATCHED_TOKEN_CLASS(Tok_Class_Id);
                  /* could be kwd arg so lets make text for now. */
                  parsed_ok = create_kwd_text(&opnd, TRUE) && parsed_ok;

                  if (list_idx == NULL_IDX) {
                     NTR_IR_LIST_TBL(list_idx);
                     IR_IDX_R(subs_idx) = list_idx;
                  }
                  else {
                     NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                  }
                  
                  COPY_OPND(IL_OPND(list_idx), opnd);
                  rank++;
                  continue;
               }
            }

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
            parse_err_flush(Find_Comma_Rparen, ")");
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
    
            COPY_OPND(IR_OPND_L(substring_idx), (*result_opnd));

            /* put substring idx into result_opnd */
            OPND_FLD((*result_opnd))          = IR_Tbl_Idx;
            OPND_IDX((*result_opnd))          = substring_idx;

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

         if (subs_idx         ||
             struct_type_idx) {

            /* intentionally blank */
         }
         else {

            /* By the way, LA_CH_VALUE should be LPAREN           */

            if (ambiguous_ref) {
               /* host reference is scalar so might be function call */
               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)                 = Call_Opr;
               IR_FLD_L(ir_idx)               = AT_Tbl_Idx;
               IR_IDX_L(ir_idx)               = attr_idx;
               IR_LINE_NUM(ir_idx)            = TOKEN_LINE(token);
               IR_COL_NUM(ir_idx)             = TOKEN_COLUMN(token);
               IR_LINE_NUM_L(ir_idx)          = TOKEN_LINE(token);
               IR_COL_NUM_L(ir_idx)           = TOKEN_COLUMN(token);
               OPND_FLD((*result_opnd))       = IR_Tbl_Idx;
               OPND_IDX((*result_opnd))       = ir_idx;

               parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
               COPY_OPND(IR_OPND_R(ir_idx), opnd);

               goto EXIT;

            }
            else if (AT_USE_ASSOCIATED(attr_idx)) {

               PRINTMSG(TOKEN_LINE(token), 898, Error, TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(attr_idx));
               parse_err_flush(Find_Ref_End, NULL);
               parsed_ok = FALSE;
               goto EXIT;
            }
            else if (expr_mode == Stmt_Func_Expr &&
                     AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                     ATD_CLASS(attr_idx) == Dummy_Argument &&
                     ATD_SF_DARG(attr_idx)) {

               PRINTMSG(TOKEN_LINE(token), 1094, Error, TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(attr_idx));
               parse_err_flush(Find_Ref_End, NULL);
               parsed_ok = FALSE;
               goto EXIT;
            }
            else if (AT_REFERENCED(attr_idx) == Not_Referenced) {

               if (!fnd_semantic_err(Obj_Use_Extern_Func,
                                     TOKEN_LINE(token), 
                                     TOKEN_COLUMN(token),
                                     attr_idx,
                                     TRUE)) {

                  if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 914, Error,
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx));
                     AT_DCL_ERR(attr_idx)     = TRUE;
                  }
                  else if (ATD_POINTER(attr_idx)) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 915, Error,
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx));
                     AT_DCL_ERR(attr_idx)        = TRUE;
                  }
                  else if (ATD_CLASS(attr_idx) != Dummy_Argument &&
                           TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
                           TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == 
                                                            Assumed_Size_Char) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 939, Error,
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx));
                     AT_DCL_ERR(attr_idx)        = TRUE;
                  }

                  /* If this is a dummy arg, the Proc will be switched to */
                  /* Dummy_Proc by this routine.                          */

                  chg_data_obj_to_pgm_unit(attr_idx, Function, Extern_Proc);

                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx)                 = Call_Opr;
                  IR_FLD_L(ir_idx)               = AT_Tbl_Idx;
                  IR_IDX_L(ir_idx)               = attr_idx;
                  IR_LINE_NUM(ir_idx)            = TOKEN_LINE(token);
                  IR_COL_NUM(ir_idx)             = TOKEN_COLUMN(token);
                  IR_LINE_NUM_L(ir_idx)          = TOKEN_LINE(token);
                  IR_COL_NUM_L(ir_idx)           = TOKEN_COLUMN(token);
                  OPND_FLD((*result_opnd))       = IR_Tbl_Idx;
                  OPND_IDX((*result_opnd))       = ir_idx;

                  parsed_ok = parse_actual_arg_spec(&opnd, TRUE, attr_idx);
                  COPY_OPND(IR_OPND_R(ir_idx), opnd);
   
                  goto EXIT;
               }
               else { /* found error with attr */
                  parse_err_flush(Find_Ref_End, NULL);
                  parsed_ok = FALSE;
                  goto EXIT;
               }
            }
            else {
               /* pass back whatever is in result_opnd */
               goto EXIT;
            }
         }
      }
   } /* if (LA_CH_VALUE == LPAREN) */

# ifdef _F_MINUS_MINUS
   if (LA_CH_VALUE == LBRKT &&
       cmd_line_flags.co_array_fortran &&
       struct_type_idx == NULL_IDX &&
       AT_OBJ_CLASS(amb_attr_idx) == Data_Obj) {

      if (ATD_PE_ARRAY_IDX(amb_attr_idx) == NULL_IDX) {
         /* not declared with pe dimensions */
         PRINTMSG(LA_CH_LINE, 1245, Error, LA_CH_COLUMN, 
                  AT_OBJ_NAME_PTR(amb_attr_idx));
         parsed_ok = FALSE;
         parse_err_flush(Find_Ref_End, NULL);
         goto EXIT;
      }

      if (stmt_type == Data_Stmt) {
         PRINTMSG(LA_CH_LINE, 1578, Error, LA_CH_COLUMN,
                  AT_OBJ_NAME_PTR(amb_attr_idx), "DATA");
         parsed_ok = FALSE;

         /* Let it continue to pick up the co-array subobject */  
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

      num_dims = 0;

      do {
         NEXT_LA_CH;
         num_dims++;

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

         if (LA_CH_VALUE != COLON &&
             (! star_expected || LA_CH_VALUE != STAR)) {
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

            if (star_expected &&
                num_dims == BD_RANK(ATD_PE_ARRAY_IDX(amb_attr_idx)) &&
                LA_CH_VALUE != STAR) {

               PRINTMSG(LA_CH_LINE, 1594, Error, LA_CH_COLUMN);
               parsed_ok = FALSE;
            }

            if (star_expected && LA_CH_VALUE == STAR) {
               /* leave list3_idx as NO_Tbl_Idx */

               if (num_dims != BD_RANK(ATD_PE_ARRAY_IDX(amb_attr_idx))) {
                  PRINTMSG(LA_CH_LINE, 116, Error, LA_CH_COLUMN);
                  parsed_ok = FALSE;
               }
               NEXT_LA_CH;
            }
            else if (LA_CH_VALUE != COLON &&
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
         else if (star_expected &&
                  num_dims == BD_RANK(ATD_PE_ARRAY_IDX(amb_attr_idx)) &&
                  IL_FLD(list_idx) != NO_Tbl_Idx) {

            find_opnd_line_and_column(&(IL_OPND(list_idx)), &line, &col);
            PRINTMSG(line, 1594, Error, col);
            parsed_ok = FALSE;
         }
         else if (star_expected && LA_CH_VALUE == STAR) {
            /* leave list_idx as NO_Tbl_Idx */

            if (num_dims != BD_RANK(ATD_PE_ARRAY_IDX(amb_attr_idx))) {
               PRINTMSG(LA_CH_LINE, 116, Error, LA_CH_COLUMN);
               parsed_ok = FALSE;
            } 
            NEXT_LA_CH;
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

         COPY_OPND(IR_OPND_L(ir_idx), (*result_opnd));

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

      if (ambiguous_ref                                 &&
          AT_REFERENCED(attr_idx) == Not_Referenced     &&
          AT_OBJ_CLASS(attr_idx) == Data_Obj            &&
          OPND_FLD((*result_opnd)) == IR_Tbl_Idx        &&
          IR_OPR(OPND_IDX((*result_opnd))) == Call_Opr) {

         /* change local attr to function  */
         chg_data_obj_to_pgm_unit(attr_idx, Function, Extern_Proc);
      }

      if (stmt_type != Data_Stmt) {

         if (expr_mode == Specification_Expr ||
             expr_mode == Initialization_Expr ||
             expr_mode == Stmt_Func_Expr) {
            AT_REFERENCED(attr_idx) = Dcl_Bound_Ref;
         }
         else {
            AT_REFERENCED(attr_idx) = Referenced;
         }

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
             ATP_PGM_UNIT(attr_idx) != Module &&
             ATP_RSLT_IDX(attr_idx) != NULL_IDX) {
            AT_REFERENCED(ATP_RSLT_IDX(attr_idx)) = AT_REFERENCED(attr_idx);
         }
      }
   }

   TRACE (Func_Exit, "parse_deref", NULL);

   return(parsed_ok);

} /* parse_deref */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parse I/O implied-DO loops.  DATA implied-DOs are parsed by           *|
|*      parse_data_imp_do in p_dcls.c. 				              *|
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

boolean  parse_imp_do (opnd_type *result_opnd)

{
   int		buf_idx;
   int          col;
   boolean	had_equal = FALSE;
   int		imp_do_start_line;
   int		imp_do_start_col;
   int		ir_idx;
   int          line;
   int		list_idx;
   int		list2_idx = NULL_IDX;
   char         next_char;
   opnd_type	opnd;
   int  	paren_level = 0;
   boolean	parsed_ok = TRUE;
   boolean	save_in_implied_do;
   int		stmt_num;


   TRACE (Func_Entry, "parse_imp_do", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != LPAREN) {
      /* shouldn't be here */
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "parse_imp_do", "LPAREN");
   }
# endif

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Implied_Do_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = LA_CH_LINE;
   IR_COL_NUM(ir_idx) = LA_CH_COLUMN;
   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = ir_idx;
   

   imp_do_start_line = LA_CH_LINE;
   imp_do_start_col  = LA_CH_COLUMN;
   save_in_implied_do = in_implied_do;
   in_implied_do = TRUE;

   do {
      NEXT_LA_CH;

START:

      if (LA_CH_VALUE == LPAREN) {

         if (next_tok_is_paren_slash ()) {

            parsed_ok = parse_expr(&opnd) && parsed_ok;

         }
         else if (is_implied_do ()) {

            if (! (parsed_ok = parse_imp_do(&opnd))) {
    
               if (LA_CH_VALUE != EOS) {
                  parse_err_flush(Find_Rparen, NULL);
                  NEXT_LA_CH;
               }

               goto EXIT;
            }
         }
         else {
            next_char = scan_thru_close_paren(0,0,1);

            if (next_char == COMMA ||
                next_char == EOS   ||
                next_char == RPAREN) {

               line = LA_CH_LINE;
               col  = LA_CH_COLUMN;
               buf_idx = LA_CH_BUF_IDX;
               stmt_num = LA_CH_STMT_NUM;

               NEXT_LA_CH;

               if (LA_CH_VALUE == LPAREN ||
                   LA_CH_VALUE == RPAREN ||
                   LA_CH_VALUE == EOS)   {

                  paren_level++;
                  goto START;
               }
               else if (paren_grp_is_cplx_const()) {
                  /* this is a complex constant */
                  reset_lex(buf_idx,stmt_num);
                  parsed_ok = parse_expr(&opnd) && parsed_ok;
               }
               else {
                  /* go back and swallow beginning ( */
                  reset_lex(buf_idx,stmt_num);
                  NEXT_LA_CH;
                  paren_level++;
                  goto START;
               }
            }
            else {

               if (list2_idx == NULL_IDX) {
                  strcpy(parse_operand_insert, "implied-do-object");
               }
               else {
                  strcpy(parse_operand_insert, 
                         "implied-do-object or do-variable");
               }

               parsed_ok = parse_expr(&opnd) && parsed_ok;

               if (stmt_type == Read_Stmt    ||
                   stmt_type == Decode_Stmt  ||
                   stmt_type == Data_Stmt) {

                  mark_attr_defined(&opnd);
               }
            }
         }
      }
      else {

         if (list2_idx == NULL_IDX) {
            strcpy(parse_operand_insert, "implied-do-object");
         }
         else {
            strcpy(parse_operand_insert, "implied-do-object or do-variable");
         }

         parsed_ok = parse_expr(&opnd) && parsed_ok;

         if (stmt_type == Read_Stmt    ||
             stmt_type == Decode_Stmt  ||
             stmt_type == Data_Stmt) {
            mark_attr_defined(&opnd);
         }

         if (LA_CH_VALUE == EQUAL) {

            if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
               find_opnd_line_and_column(&opnd, &line, &col);

               /* no list before lcv */

               PRINTMSG(line, 872, Error, col);
               parsed_ok = FALSE;
            }

            had_equal = TRUE;

            /* Set up right child (loop control variable) of the Implied_Do   */
            /* IR.					                      */

            if (OPND_FLD(opnd) == IR_Tbl_Idx) {
               find_opnd_line_and_column(&opnd, &line, &col);
               PRINTMSG(line, 199, Error, col);
               parsed_ok = FALSE;
            }

            IR_FLD_R(ir_idx) = IL_Tbl_Idx;
            NTR_IR_LIST_TBL(list_idx);
            IR_IDX_R(ir_idx) = list_idx;
            COPY_OPND(IL_OPND(list_idx), opnd);
            mark_attr_defined(&opnd);


            if (OPND_FLD(opnd) == AT_Tbl_Idx &&
                AT_OBJ_CLASS(OPND_IDX(opnd)) == Data_Obj) {

               ATD_SEEN_AS_LCV(OPND_IDX(opnd)) = TRUE;

               if (ATD_FIRST_SEEN_IL_IDX(OPND_IDX(opnd)) != NULL_IDX) {

                  if (ATD_SEEN_IN_IMP_DO(OPND_IDX(opnd)) &&
                      (IL_LINE_NUM(ATD_FIRST_SEEN_IL_IDX(OPND_IDX(opnd)))
                                                        > imp_do_start_line ||
                       (IL_LINE_NUM(ATD_FIRST_SEEN_IL_IDX(OPND_IDX(opnd))) == 
                                                         imp_do_start_line &&
                        IL_COL_NUM(ATD_FIRST_SEEN_IL_IDX(OPND_IDX(opnd))) 
                                                       > imp_do_start_col))) {
   
                     /* clear ATD_SEEN_IN_IMP_DO */
   
                     ATD_SEEN_IN_IMP_DO(OPND_IDX(opnd)) = FALSE;
                  }

                  FREE_IR_LIST_NODE(ATD_FIRST_SEEN_IL_IDX(OPND_IDX(opnd)));
                  ATD_FIRST_SEEN_IL_IDX(OPND_IDX(opnd)) = NULL_IDX;
               }
               else if (ATD_SEEN_IN_IMP_DO(OPND_IDX(opnd)) &&
                   (AT_DEF_LINE(OPND_IDX(opnd)) > imp_do_start_line ||
                    (AT_DEF_LINE(OPND_IDX(opnd)) == imp_do_start_line &&
                     AT_DEF_COLUMN(OPND_IDX(opnd)) > imp_do_start_col))) {

                  /* clear ATD_SEEN_IN_IMP_DO */

                  ATD_SEEN_IN_IMP_DO(OPND_IDX(opnd)) = FALSE;
               }
            }

            /* Create an IL to hold the start value and attach it to the LCV  */
            /* IL.  Parse the loop start expression.			      */

            NTR_IR_LIST_TBL(list2_idx);
            IL_NEXT_LIST_IDX(list_idx) = list2_idx;
            IL_PREV_LIST_IDX(list2_idx) = list_idx;
            NEXT_LA_CH;
            strcpy(parse_operand_insert, "operand");
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list2_idx), opnd);
            
            if (LA_CH_VALUE != COMMA) {
               parsed_ok = FALSE;
               parse_err_flush(Find_Rparen, ",");
               continue;
            }

            /* Eat the comma following the loop start expression.	      */
            /* Create an IL to hold the end value and attach it to the start  */
            /* value IL.  Parse the loop end expression.		      */

            NEXT_LA_CH;

            NTR_IR_LIST_TBL(list_idx);
            IL_NEXT_LIST_IDX(list2_idx) = list_idx;
            IL_PREV_LIST_IDX(list_idx) = list2_idx;
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list_idx), opnd);
            
            /* If the increment expression exists, create an IL to hold it    */
            /* and attach it to the end value IL.  Parse the inc expression.  */

            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;
               NTR_IR_LIST_TBL(list2_idx);
               IL_NEXT_LIST_IDX(list_idx) = list2_idx;
               IL_PREV_LIST_IDX(list2_idx) = list_idx;
               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(list2_idx), opnd);
               IR_LIST_CNT_R(ir_idx) = 4;
            }
            else {
               IR_LIST_CNT_R(ir_idx) = 3;
            }
   
            break;
         }
      }

      if (IR_IDX_L(ir_idx) == NULL_IDX) {
         NTR_IR_LIST_TBL(list_idx);
         COPY_OPND(IL_OPND(list_idx), opnd);
         IR_FLD_L(ir_idx) = IL_Tbl_Idx;
         IR_IDX_L(ir_idx) = list_idx;
         IR_LIST_CNT_L(ir_idx) = 1;
         list2_idx = list_idx;
      }
      else {
         NTR_IR_LIST_TBL(list_idx);
         IL_NEXT_LIST_IDX(list2_idx) = list_idx;
         IL_PREV_LIST_IDX(list_idx) = list2_idx;
         COPY_OPND(IL_OPND(list_idx), opnd);
         ++IR_LIST_CNT_L(ir_idx);
         list2_idx = list_idx;
      }

      while (LA_CH_VALUE == RPAREN && paren_level) {
         NEXT_LA_CH;
         paren_level--;
      }
   }
   while (LA_CH_VALUE == COMMA);

   in_implied_do = save_in_implied_do;

   if (paren_level) {
      parse_err_flush(Find_EOS, ")");
      goto EXIT;
   }
   else if (LA_CH_VALUE != RPAREN) {

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

   NEXT_LA_CH;					/* swallow ) */
   
EXIT:

   strcpy(parse_operand_insert, "operand");

   TRACE (Func_Exit, "parse_imp_do", NULL);

   return(parsed_ok);

} /* parse_imp_do */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*     This procedure should be called to check a label reference in a branch *|
|*     context (GO TO, arithmetic IF, actual arg that is a label, etc.).      *|
|*									      *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Global data used:						  	      *|
|*      The data structure "token" is assumed to be the label token.          *|
|*      The Statement Header for the statement containing the label reference *|
|*        is assumed to exist.     					      *|
|*									      *|
|* Returns:                                                                   *|
|*      The index to the label's Attribute entry.  			      *|
|*                                                                            *|
|* Algorithm notes:							      *|
|*									      *|
|*     Processing of the label reference is dependent on the state of the     *|
|*     label's Attribute entry:				                      *|
|*									      *|
|*       Case 1:  It doesn't exist.  					      *|
|*         This is the first reference to the label and it's a forward        *|
|*         reference.  Enter it into the symbol table and generate a Forward  *|
|*         Ref entry.	 						      *|
|*									      *|
|*       Case 2:  It exists in the symbol table and is defined.               *|
|*         This is a backward reference.  Check the reference to the label.   *|
|*									      *|
|*       Case 3:  It exists in the symbol table but is still undefined.       *|
|*         This is another forward reference to the label.  Generate a        *|
|*         Forward Ref entry.						      *|
|*									      *|
|*     Each new Forward Ref entry is attached to the head of the chain of     *|
|*     Forward Ref entries attached to the label's Attribute entry via        *|
|*     ATL_FWD_REF_IDX (when the label is encountered, the Forward Ref chain  *|
|*     is processed, the entries are freed, and the field is set to the index *|
|*     of the Statement Header for the label's defining statement; the field  *|
|*     is then referenced using ATL_DEF_STMT_IDX).			      *|
|*                                                                            *|
|*     Note:  If the statement containing the label reference has been marked *|
|*            in error, the label reference semantics will not be checked.    *|
|*            This should prevent meaningless messages from being issued for  *|
|*            oddball cases like a GO TO existing in an interface block.      *|
|*									      *|
\******************************************************************************/

int  check_label_ref(void)

{
   int		blk_idx;
   int		cmic_blk_sh_idx = NULL_IDX;
   int		lbl_attr_idx;
   int		name_idx;


   TRACE (Func_Entry, "check_label_ref", NULL);

   lbl_attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);
      
   if (lbl_attr_idx == NULL_IDX) {
      lbl_attr_idx			= ntr_sym_tbl(&token, name_idx);
      AT_REFERENCED(lbl_attr_idx)	= Referenced;
      AT_OBJ_CLASS(lbl_attr_idx)	= Label;
      LN_DEF_LOC(name_idx)		= TRUE;
   }

   if (AT_DEFINED(lbl_attr_idx)) {

      /* If the stmt contains a reference to its own label, set               */
      /* ATL_EXECUTABLE now so label_ref_semantics will work correctly.       */
      /* (Normally, ATL_EXECUTABLE is set AFTER the stmt is parsed.)          */

      if (stmt_label_idx != NULL_IDX  &&
          (ATL_DEF_STMT_IDX(lbl_attr_idx) == curr_stmt_sh_idx  ||
           if_stmt_lbl_idx != NULL_IDX)) {
         ATL_EXECUTABLE(lbl_attr_idx) = TRUE;
      }

      if ( ! SH_ERR_FLG(curr_stmt_sh_idx) ) {

         blk_idx = blk_stk_idx;

         while (blk_idx > 0) {
            if (BLK_IS_PARALLEL_REGION(blk_idx)) {
               cmic_blk_sh_idx = BLK_FIRST_SH_IDX(blk_idx);
               break;
            }

            blk_idx--;
         }

         check_cmic_blk_branches(cmic_blk_sh_idx,
                                 lbl_attr_idx,
                                 TOKEN_LINE(token), 
                                 TOKEN_COLUMN(token));

         blk_idx = blk_stk_idx;

         while (BLK_IS_PARALLEL_REGION(blk_idx) ||
                BLK_TYPE(blk_idx) == Do_Parallel_Blk    ||
                BLK_TYPE(blk_idx) == Wait_Blk  ||
                BLK_TYPE(blk_idx) == SGI_Region_Blk) {

            blk_idx--;
         }

         label_ref_semantics(lbl_attr_idx, Branch_Context,
                             (BLK_TYPE(blk_idx) > Interface_Body_Blk) ?
                                BLK_FIRST_SH_IDX(blk_idx) : NULL_IDX,
                             TOKEN_LINE(token), TOKEN_COLUMN(token));
      }
   }
   else {
      build_fwd_ref_entry(lbl_attr_idx, Branch_Context);
   }
              
   if (cif_flags & XREF_RECS) {
      cif_usage_rec(lbl_attr_idx, AT_Tbl_Idx,
                    TOKEN_LINE(token), TOKEN_COLUMN(token),
		    CIF_Label_Referenced_As_Branch_Target);
   }

   TRACE (Func_Exit, "check_label_ref", NULL);

   return(lbl_attr_idx);

}  /* check_label_ref */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Given location information about the reference to a label and the     *|
|*      statement defining the label, this procedure verifies that the        *|
|*      reference to the label is valid.  The essential rule for a label      *|
|*      reference being valid is that the reference and label must be within  *|
|*      the same block or the label must be in an outer block.  It doesn't    *|
|*      matter that the label occurs before or after the reference.  Thus,    *|
|*      this procedure may be used to check both forward and backward         *|
|*      references (see the design paper for details).			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx     : the index to the label's Attribute entry	              *|
|*      ref_blk_idx  : the index to the Statement Header of the statement     *|
|*                       that begins the block containing the label           *|
|*                       reference					      *|
|*      ref_line_num : the line number of the label reference                 *|
|*      ref_col_num  : the column number of the label reference               *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void  label_ref_semantics(int		attr_idx,
			  lbl_ref_type	context,
		          int		ref_blk_idx,
    	   	          int		ref_line_num,
		          int		ref_col_num)
{
   stmt_type_type       check_stmt_type;
   int			lbl_blk_idx;
   stmt_type_type	lbl_stmt_type;
   int			line_num;
   char                 stmt_str[12];
   boolean		valid_branch_target = TRUE;


   TRACE (Func_Entry, "label_ref_semantics", NULL);

   /* If something is wrong with the label (possibly because there was       */
   /* something wrong with its defining statement), don't do any checking.   */

   if (AT_DCL_ERR(attr_idx)) {
      goto EXIT;
   }


   /* If this is a backward reference, AT_DEFINED is TRUE and                */
   /* ATL_DEF_STMT_IDX points at the label's defining statement.  Otherwise, */
   /* we are processing a forward reference so ATL_DEF_STMT_IDX (aka         */
   /* ATL_FWD_REF_IDX) is used to point at the Forward Ref chain.  But       */
   /* processing a forward reference means we are now at the defining        */
   /* statement so the statement type is available from a global variable.   */ 

   if (AT_DEFINED(attr_idx)) {
      lbl_stmt_type = SH_STMT_TYPE(ATL_DEF_STMT_IDX(attr_idx));
   }
   else {
      lbl_stmt_type = stmt_type;
   }
         

   /* If the label is defined on a nonexecutable statement or on an          */
   /* executable statement that can not be a branch target, issue a message  */
   /* and quit.  Note that if the label reference is from an ASSIGN stmt,    */
   /* the reference can be to a FORMAT statement.			     */

   if ( ! ATL_EXECUTABLE(attr_idx) ) {

      if (context == Branch_Context) {
         PRINTMSG(ref_line_num, 144, Error, ref_col_num, AT_DEF_LINE(attr_idx));
      }
      else if (lbl_stmt_type != Format_Stmt) {
              PRINTMSG(ref_line_num, 345, Error, ref_col_num, 
		       AT_OBJ_NAME_PTR(attr_idx));
      }

      goto EXIT;
   }

   stmt_str[0] = '\0';

   switch (lbl_stmt_type) {
       case Case_Stmt:
          valid_branch_target = FALSE;
	  strcpy(stmt_str, "CASE");
          break; 
  
       case Else_Stmt:
          valid_branch_target = FALSE;
	  strcpy(stmt_str, "ELSE");
	  break;

       case Else_If_Stmt:
          valid_branch_target = FALSE;
	  strcpy(stmt_str, "ELSE IF");
	  break;

       case Else_Where_Stmt:
          valid_branch_target = FALSE;
	  strcpy(stmt_str, "ELSEWHERE");
	  break;

       case End_Where_Stmt:  
	  valid_branch_target = FALSE;
	  strcpy(stmt_str, "END WHERE");
	  break;

       case End_Forall_Stmt:  
	  valid_branch_target = FALSE;
	  strcpy(stmt_str, "END FORALL");
	  break;

       case Then_Stmt:       
          valid_branch_target = FALSE;
	  strcpy(stmt_str, "THEN");
	  break;
   }

   if ( ! valid_branch_target ) {
      PRINTMSG(ref_line_num,
               (context == Branch_Context) ? 145 : 346,
               Error, ref_col_num, stmt_str,
               AT_DEF_LINE(attr_idx));
      goto EXIT;
   }


   /* A jump into a WHERE construct or a CASE block is not allowed.          */

   if (SH_STMT_TYPE(ATL_BLK_STMT_IDX(attr_idx)) == Where_Cstrct_Stmt  ||
       SH_STMT_TYPE(ATL_BLK_STMT_IDX(attr_idx)) == Else_Where_Stmt) {

      if (context == Branch_Context) {
         PRINTMSG(ref_line_num, 147, Error, ref_col_num,
               SH_GLB_LINE(ATL_BLK_STMT_IDX(attr_idx)));
      }
      else {
         PRINTMSG(ref_line_num, 347, Warning, ref_col_num,
                  AT_OBJ_NAME_PTR(attr_idx),
                  SH_GLB_LINE(ATL_BLK_STMT_IDX(attr_idx)));
      }
 
      goto EXIT;
   }

   /* (or FORALL construct) */

   if (SH_STMT_TYPE(ATL_BLK_STMT_IDX(attr_idx)) == Forall_Cstrct_Stmt) {

      if (context == Branch_Context) {
         PRINTMSG(ref_line_num, 1595, Error, ref_col_num,
               SH_GLB_LINE(ATL_BLK_STMT_IDX(attr_idx)));
      }
      else {
         PRINTMSG(ref_line_num, 1596, Warning, ref_col_num,
                  AT_OBJ_NAME_PTR(attr_idx),
                  SH_GLB_LINE(ATL_BLK_STMT_IDX(attr_idx)));
      }

      goto EXIT;
   }



   /* Nothing more we can do if the reference is from an ASSIGN statement.   */

   if (context != Branch_Context) {
      goto EXIT;
   }
     

   /* We have ascertained that the label is defined on a valid branch target */
   /* statement.  Now check that the branch is into a valid block.           */
   /* If the label is defined at the procedure level, the jump can't         */
   /* possibly be into a block.  Also, if the label and reference both exist */
   /* in the same block, the jump is also OK.			             */

   if (ATL_BLK_STMT_IDX(attr_idx) == NULL_IDX  ||
       ATL_BLK_STMT_IDX(attr_idx) == ref_blk_idx) {
      goto EXIT;
   }


   /* If control reaches this point, the label and reference are in          */
   /* different blocks.  We now need to see if the label is defined in a     */
   /* block that is a containing block of the block containing the reference.*/
   /* This is done by searching a "long-lived" block stack that is formed by */
   /* parent links in Statement Headers for executable blocking statements.  */
   /* However, the stack need not be searched if the reference is at the     */
   /* procedure level because we know we have a jump into a block.           */
   
   lbl_blk_idx = NULL_IDX;

   if (ref_blk_idx != NULL_IDX) {
      lbl_blk_idx = SH_PARENT_BLK_IDX(ref_blk_idx);

      while (lbl_blk_idx != NULL_IDX) {

         if (lbl_blk_idx == ATL_BLK_STMT_IDX(attr_idx)) {
            break;
         }
         else {
            lbl_blk_idx = SH_PARENT_BLK_IDX(lbl_blk_idx);
         }
      }
   }

   if (lbl_blk_idx != NULL_IDX) {
      goto EXIT;
   }


   /* The jump is into a block.  					     */
   /* A jump to an END SELECT or END DO from outside the construct (or from  */
   /* outside the innermost block DO) is an error.                           */
   /* A jump to an END IF from outside the construct is obsolescent.         */
   /* A jump to any statement in a block or nonblock DO or into an IF        */
   /* construct subblock (possibly from another IF subblock) is unsafe (to   */
   /* be compatible with CF77) and is not standard-conforming.               */

   if (lbl_stmt_type == End_Do_Stmt) {
      PRINTMSG(ref_line_num, 150, Error, ref_col_num);
      goto EXIT;
   }

   if (lbl_stmt_type == End_Select_Stmt) {
      PRINTMSG(ref_line_num, 153, Error, ref_col_num);
      goto EXIT;
   }

   if (lbl_stmt_type == End_If_Stmt) {
      PRINTMSG(ref_line_num, 1567, Ansi, ref_col_num);

      if (SH_PARENT_BLK_IDX(ATL_BLK_STMT_IDX(attr_idx)) != NULL_IDX) {

         if (SH_PARENT_BLK_IDX(ATL_BLK_STMT_IDX(attr_idx)) != ref_blk_idx) {
            check_stmt_type =
               SH_STMT_TYPE(SH_PARENT_BLK_IDX(ATL_BLK_STMT_IDX(attr_idx)));
            line_num =
               SH_GLB_LINE(SH_PARENT_BLK_IDX(ATL_BLK_STMT_IDX(attr_idx)));
         }
         else {
            goto EXIT;
         }
      }
      else {
         goto EXIT;
      }
   }
   else {
      check_stmt_type = SH_STMT_TYPE(ATL_BLK_STMT_IDX(attr_idx));
      line_num        = SH_GLB_LINE(ATL_BLK_STMT_IDX(attr_idx));
   }

   switch (check_stmt_type) {

      case Case_Stmt:
         PRINTMSG(ref_line_num, 148, Error, ref_col_num, line_num);
         goto EXIT;

      case Do_Iterative_Stmt:
      case Do_While_Stmt: 
      case Do_Infinite_Stmt:
         PRINTMSG(ref_line_num, 154, Warning, ref_col_num, line_num);
         PRINTMSG(ref_line_num, 155, Ansi, ref_col_num, line_num);
         goto EXIT;

      case Else_Stmt:
         strcpy(stmt_str, "ELSE");
         break;
     
      case Else_If_Stmt:
         strcpy(stmt_str, "ELSE IF");
         break;

      case Then_Stmt:
         strcpy(stmt_str, "THEN");
         break;

      case Directive_Stmt:
      case Parallel_Case_Stmt:
         /* do nothing in here */
         goto EXIT;
   }

   PRINTMSG(ref_line_num, 156, Warning, ref_col_num, stmt_str, line_num);
   PRINTMSG(ref_line_num, 157, Ansi, ref_col_num, stmt_str, line_num);

EXIT:

   TRACE (Func_Entry, "label_ref_semantics", NULL);

   return;

}  /* label_ref_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*     This procedure builds a Forward Ref entry for a branch context (GO TO, *|
|*     arithmetic IF, actual argument for procedure call), a reference to a   *|
|*     FORMAT statement, or a reference to a label in an ASSIGN or DO         *|
|*     statement.							      *|
|*                                                                            *|
|*     Each new Forward Ref entry is attached to the head of the chain of     *|
|*     Forward Ref entries attached to the label's Attribute entry via        *|
|*     ATL_FWD_REF_IDX (when the label is encountered, the Forward Ref chain  *|
|*     is processed, the entries are freed, and the field is set to the index *|
|*     of the Statement Header for the label's defining statement; the field  *|
|*     is then referenced using ATL_DEF_STMT_IDX).                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      lbl_attr_idx  : The index of the label's Attribute entry.             *|
|*      fwd_ref_cntxt : An enumerated list describing the label reference.    *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Global data used:                                                          *|
|*      The data structure "token" is assumed to be the label token.          *|
|*      The Attribute entry for the label is assumed to exist.                *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Algorithm notes:                                                           *|
|*   - The first Forward Ref entry does NOT point back at the label's Attr    *|
|*     entry.                                                                 *|
|*                                                                            *|
|*   - The IL_FLD field is used as the indicator of the context of the        *|
|*     Forward Ref entry.  The values in IL_FLD have the following meanings:  *|
|*									      *|
|*           SH_Tbl_Idx : branch context (GO TO, arithmetic IF, etc.); IL_IDX *|
|*                          points to the first stmt of the block containing  *|
|*                          the label definition or is NULL_IDX to indicate   *|
|*                          that the label is defined at the procedure level  *|
|*           NO_Tbl_Idx : must check the "rank" field (accessed by macro      *|
|*                           IL_FORWARD_REF) to see if the ref is from an     *|
|*                           ASSIGN or DO stmt or to a FORMAT stmt    	      *|
|*                                                                            *|
|*     DO statement entries really only exist in the chain to diagnose        *|
|*     references to a label if the label is never defined.  (See the         *|
|*     undefined label checks in the end-pass-1-checks routine.)              *|
|*     ASSIGN statement entries exist so that a diagnostic can be issued if   *|
|*     the label is not defined on an executable or FORMAT statement.         *|
|*     FORMAT statement entries exist so that if the label turns out to be    *|
|*     defined on an executable stmt, references to it as a FORMAT stmt label *|
|*     can be diagnosed.  (And if the label turns out to be on a FORMAT stmt  *|
|*     and is referenced in a branch context, the misuse can be diagnosed.)   *|
|*                                                                            *|
\******************************************************************************/

void  build_fwd_ref_entry(int           lbl_attr_idx,
                          lbl_ref_type  fwd_ref_cntxt)

{
   int		blk_idx;
   int		cmic_sh_idx = NULL_IDX;
   int          curr_fwd_ref_idx;
   int		fwd_ref_idx1;
   int		fwd_ref_idx2;
   int          new_fwd_ref_idx;


   TRACE (Func_Entry, "build_fwd_ref_entry", NULL);

   curr_fwd_ref_idx = ATL_FWD_REF_IDX(lbl_attr_idx);

   NTR_IR_LIST_TBL(new_fwd_ref_idx);

   ATL_FWD_REF_IDX(lbl_attr_idx) = new_fwd_ref_idx;
   IL_NEXT_LIST_IDX(new_fwd_ref_idx) = curr_fwd_ref_idx;

   if (curr_fwd_ref_idx != NULL_IDX) {
      IL_PREV_LIST_IDX(curr_fwd_ref_idx) = new_fwd_ref_idx;
   }

   IL_LINE_NUM(new_fwd_ref_idx) = TOKEN_LINE(token);
   IL_COL_NUM(new_fwd_ref_idx)  = TOKEN_COLUMN(token);


   switch (fwd_ref_cntxt) {

      case Branch_Context:
         IL_FLD(new_fwd_ref_idx) = SH_Tbl_Idx;
  
         blk_idx = blk_stk_idx;

         while (BLK_IS_PARALLEL_REGION(blk_idx) ||
                BLK_TYPE(blk_idx) == Do_Parallel_Blk    ||
                BLK_TYPE(blk_idx) == Wait_Blk  ||
                BLK_TYPE(blk_idx) == SGI_Region_Blk) {

            blk_idx--;
         }

         if (BLK_TYPE(blk_idx) > Interface_Body_Blk) {
            IL_IDX(new_fwd_ref_idx) = BLK_FIRST_SH_IDX(blk_idx);
         }

         /* Check to see if this label ref  is within a parallel region and */
         /* save the statement header that begins the region if it is.      */

         blk_idx = blk_stk_idx;

         while (blk_idx > 0) {

            if (BLK_IS_PARALLEL_REGION(blk_idx)) {
               cmic_sh_idx = BLK_FIRST_SH_IDX(blk_idx);
               break;
            }

            blk_idx--;
         }

         if (cmic_sh_idx != NULL_IDX) {
            NTR_IR_LIST_TBL(fwd_ref_idx1);
            NTR_IR_LIST_TBL(fwd_ref_idx2);
            IL_NEXT_LIST_IDX(fwd_ref_idx1) = fwd_ref_idx2;
            IL_PREV_LIST_IDX(fwd_ref_idx2) = fwd_ref_idx1;
            IL_LINE_NUM(fwd_ref_idx1)      = TOKEN_LINE(token);
            IL_COL_NUM(fwd_ref_idx1)       = TOKEN_COLUMN(token);
            IL_LINE_NUM(fwd_ref_idx2)      = TOKEN_LINE(token);
            IL_COL_NUM(fwd_ref_idx2)       = TOKEN_COLUMN(token);

            IL_FLD(fwd_ref_idx1)           = SH_Tbl_Idx;
            IL_FLD(fwd_ref_idx2)           = SH_Tbl_Idx;

            IL_IDX(fwd_ref_idx1)           = IL_IDX(new_fwd_ref_idx);
            IL_IDX(fwd_ref_idx2)           = cmic_sh_idx;

            IL_FLD(new_fwd_ref_idx)        = IL_Tbl_Idx;
            IL_LIST_CNT(new_fwd_ref_idx)   = 2;
            IL_IDX(new_fwd_ref_idx)        = fwd_ref_idx1;
         }

         break;

      case Assign_Ref:
         IL_FORWARD_REF(new_fwd_ref_idx) = From_Assign_Stmt;
         break;
    
      case Do_Ref:
         IL_FORWARD_REF(new_fwd_ref_idx) = From_Do_Stmt;
         break;

      case Format_Ref:
         IL_FORWARD_REF(new_fwd_ref_idx) = To_Format_Stmt;
         break;

      default:
         PRINTMSG(stmt_start_line, 179, Internal, stmt_start_col,
                  "build_fwd_ref_entry");
   }

   TRACE (Func_Exit, "build_fwd_ref_entry", NULL);

}  /* build_fwd_ref_entry */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine resolves forward references to labels that are either    *|
|*      FORMAT labels or branch target labels.				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
|* Algorithm notes:							      *|
|*      The type of the forward reference is determined by IL_FLD and values  *|
|*      in the IL operand "rank" field.  If IL_FLD is SH_Tbl_Idx, the         *|
|*      reference is due to a branch context.  If IL_FLD is NO_Tbl_Idx, then  *|
|*      the "rank" field value contains the reason for the forward reference  *|
|*      (the "rank" field is accessed via macro IL_FORWARD_REF).              *|
|*									      *|
\******************************************************************************/

void  resolve_fwd_lbl_refs (void)
  
{
   int	fwd_ref_idx;
   int  next_fwd_ref_idx;


   TRACE (Func_Entry, "resolve_fwd_lbl_refs", NULL);

   fwd_ref_idx = ATL_FWD_REF_IDX(stmt_label_idx);

   if ( ! AT_DCL_ERR(stmt_label_idx) ) {

      /* The label is OK.  If it's defined on a FORMAT stmt, just ensure that */
      /* all references are format refs.  Otherwise, the label is a branch    */
      /* target so make sure the branch is allowed.                           */

      if (stmt_type == Format_Stmt) {

         while (fwd_ref_idx != NULL_IDX) {

            if (IL_FLD(fwd_ref_idx) == SH_Tbl_Idx) {
               PRINTMSG(IL_LINE_NUM(fwd_ref_idx), 144, Error,
                        IL_COL_NUM(fwd_ref_idx), stmt_start_line);
            }
            else if (IL_FLD(fwd_ref_idx) == IL_Tbl_Idx) {
               PRINTMSG(IL_LINE_NUM(IL_IDX(fwd_ref_idx)), 144, Error,
                        IL_COL_NUM(IL_IDX(fwd_ref_idx)), stmt_start_line);
            }

            next_fwd_ref_idx = IL_NEXT_LIST_IDX(fwd_ref_idx);

            if (IL_FLD(fwd_ref_idx) == IL_Tbl_Idx) {
               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(IL_IDX(fwd_ref_idx)));
               FREE_IR_LIST_NODE(IL_IDX(fwd_ref_idx));
            }
            FREE_IR_LIST_NODE(fwd_ref_idx);
            fwd_ref_idx = next_fwd_ref_idx;
         }

      }
      else {

         while (fwd_ref_idx != NULL_IDX) {

            if (IL_FLD(fwd_ref_idx) == NO_Tbl_Idx) {

               if (IL_FORWARD_REF(fwd_ref_idx) == To_Format_Stmt) {
                  PRINTMSG(IL_LINE_NUM(fwd_ref_idx), 328, Error,
                           IL_COL_NUM(fwd_ref_idx),
                           AT_OBJ_NAME_PTR(stmt_label_idx));
               }
               else if (IL_FORWARD_REF(fwd_ref_idx) == From_Assign_Stmt) {
                  label_ref_semantics(stmt_label_idx, Assign_Ref,
                                      IL_IDX(fwd_ref_idx),
                                      IL_LINE_NUM(fwd_ref_idx),
                                      IL_COL_NUM(fwd_ref_idx));
               }
            }
            else if (IL_FLD(fwd_ref_idx) == IL_Tbl_Idx) {

               check_cmic_blk_branches(IL_IDX(IL_NEXT_LIST_IDX(
                                                IL_IDX(fwd_ref_idx))),
                                       stmt_label_idx,
                                       IL_LINE_NUM(IL_IDX(fwd_ref_idx)),
                                       IL_COL_NUM(IL_IDX(fwd_ref_idx)));

               label_ref_semantics(stmt_label_idx, Branch_Context,
                                   IL_IDX(IL_IDX(fwd_ref_idx)),
                                   IL_LINE_NUM(IL_IDX(fwd_ref_idx)),
                                   IL_COL_NUM(IL_IDX(fwd_ref_idx)));
            }
            else {

               check_cmic_blk_branches(NULL_IDX,
                                       stmt_label_idx,
                                       IL_LINE_NUM(fwd_ref_idx),
                                       IL_COL_NUM(fwd_ref_idx));

               label_ref_semantics(stmt_label_idx, Branch_Context,
                                   IL_IDX(fwd_ref_idx),
                                   IL_LINE_NUM(fwd_ref_idx),
                                   IL_COL_NUM(fwd_ref_idx));
            }

            next_fwd_ref_idx = IL_NEXT_LIST_IDX(fwd_ref_idx);

            if (IL_FLD(fwd_ref_idx) == IL_Tbl_Idx) {
               FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(IL_IDX(fwd_ref_idx)));
               FREE_IR_LIST_NODE(IL_IDX(fwd_ref_idx));
            }
            FREE_IR_LIST_NODE(fwd_ref_idx);
            fwd_ref_idx = next_fwd_ref_idx;
         }

      }

      AT_DEFINED(stmt_label_idx) = TRUE;
      ATL_DEF_STMT_IDX(stmt_label_idx) =
         (SH_STMT_TYPE(curr_stmt_sh_idx) != Then_Stmt) ? curr_stmt_sh_idx :
            SH_PARENT_BLK_IDX(curr_stmt_sh_idx);
   }
   else {

      /* The label is bad.  Abandon the forward references.                   */

      while (fwd_ref_idx != NULL_IDX) {
         next_fwd_ref_idx = IL_NEXT_LIST_IDX(fwd_ref_idx);

         if (IL_FLD(fwd_ref_idx) == IL_Tbl_Idx) {
            FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(IL_IDX(fwd_ref_idx)));
            FREE_IR_LIST_NODE(IL_IDX(fwd_ref_idx));
         }
         FREE_IR_LIST_NODE(fwd_ref_idx);
         fwd_ref_idx = next_fwd_ref_idx;
      }

      ATL_FWD_REF_IDX(stmt_label_idx) = NULL_IDX;
      AT_DEFINED(stmt_label_idx) = TRUE;
   }

   TRACE (Func_Exit, "resolve_fwd_lbl_refs", NULL);

   return;

}  /* resolve_fwd_lbl_refs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine checks for branches into or out of parallel, doall,      *|
|*	guard or case autotasking regions.                                    *|
|*									      *|
|* Input parameters:							      *|
|*	ref_blk_sh_idx	-if == NULL_IDX => the ref was not in parallel region *|
|*			 else its the first sh in the ref's region.           *|
|*	label_attr	- label attr in question.                             *|
|*	line, col	- line and column of the goto branch.                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void check_cmic_blk_branches(int		ref_blk_sh_idx,
				    int		label_attr,
				    int		line,
				    int		col)

{

   char		str1[32];
   char		str2[8];
   int		msg_num;

   TRACE (Func_Entry, "check_cmic_blk_branches", NULL);

   if (ATL_CLASS(label_attr) == Lbl_User &&
       ref_blk_sh_idx != ATL_CMIC_BLK_STMT_IDX(label_attr)) {

      if (ATL_CMIC_BLK_STMT_IDX(label_attr) != NULL_IDX) {

         block_err_string(IR_OPR(SH_IR_IDX(ATL_CMIC_BLK_STMT_IDX(label_attr))),
                          str1,
                         &msg_num);

         strcpy(str2, "into");
      }
      else {
         block_err_string(IR_OPR(SH_IR_IDX(ref_blk_sh_idx)),
                          str1,
                         &msg_num);

         strcpy(str2, "out of");
      }

      PRINTMSG(line, msg_num, Error, col, str2, str1);
   }

   TRACE (Func_Exit, "check_cmic_blk_branches", NULL);

   return;

}  /* check_cmic_blk_branches */

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

static void block_err_string(operator_type    opr,
                             char	     *str,
                             int	     *msg_num)

{


   TRACE (Func_Entry, "block_err_string", NULL);
   switch (opr) {
      case Parallel_Cmic_Opr:
         strcpy(str, "PARALLEL");
         *msg_num = 1220;
         break;

      case Doall_Cmic_Opr:
         strcpy(str, "DOALL");
         *msg_num = 1220;
         break;

      case Guard_Cmic_Opr:
         strcpy(str, "GUARD");
         *msg_num = 1220;
         break;

      case Case_Cmic_Opr:
         strcpy(str, "CASE");
         *msg_num = 1220;
         break;

      case Parallel_Open_Mp_Opr:
         strcpy(str, "!$OMP PARALLEL");
         *msg_num = 1503;
         break;

      case Do_Open_Mp_Opr:
         strcpy(str, "!$OMP DO");
         *msg_num = 1503;
         break;

      case Parallelsections_Open_Mp_Opr:
      case Sections_Open_Mp_Opr:
      case Section_Open_Mp_Opr:
         strcpy(str, "!$OMP SECTION");
         *msg_num = 1503;
         break;

      case Single_Open_Mp_Opr:
         strcpy(str, "!$OMP SINGLE");
         *msg_num = 1503;
         break;

      case Paralleldo_Open_Mp_Opr:
         strcpy(str, "!$OMP PARALLEL DO");
         *msg_num = 1503;
         break;

      case Master_Open_Mp_Opr:
         strcpy(str, "!$OMP MASTER");
         *msg_num = 1503;
         break;

      case Critical_Open_Mp_Opr:
         strcpy(str, "!$OMP CRITICAL");
         *msg_num = 1503;
         break;

      case Ordered_Open_Mp_Opr:
         strcpy(str, "!$OMP ORDERED");
         *msg_num = 1503;
         break;

      case Doacross_Dollar_Opr:
         strcpy(str, "!$ DOACROSS");
         *msg_num = 1504;
         break;

      case Psection_Par_Opr:
         strcpy(str, "!$PAR PSECTION");
         *msg_num = 1504;
         break;

      case Section_Par_Opr:
         strcpy(str, "!$PAR SECTION");
         *msg_num = 1504;
         break;

      case Pdo_Par_Opr:
         strcpy(str, "!$PAR PDO");
         *msg_num = 1504;
         break;

      case Parallel_Do_Par_Opr:
         strcpy(str, "!$PAR PARALLEL DO");
         *msg_num = 1504;
         break;

      case Parallel_Par_Opr:
         strcpy(str, "!$PAR PARALLEL");
         *msg_num = 1504;
         break;

      case Critical_Section_Par_Opr:
         strcpy(str, "!$PAR CRITICAL SECTION");
         *msg_num = 1504;
         break;

      case Singleprocess_Par_Opr:
         strcpy(str, "!$PAR SINGLE PROCESS");
         *msg_num = 1504;
         break;

      default:
# ifdef _DEBUG
         PRINTMSG(1, 626, Internal, 1, 
                  "directive operator", "block_err_string");
# endif
         break;
   }


   TRACE (Func_Exit, "block_err_string", NULL);

   return;

}  /* block_err_string */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Find the base attr (left-most) and set it's defined flag.             *|
|*      If not a data object nothing is done.                                 *|
|*      If data object is a function result, mark the pgm unit attr also.     *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - address of root opnd.                                          *|
|*									      *|
|* Output parameters:							      *|
|*	opnd - address of root opnd.                                          *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void	mark_attr_defined(opnd_type *opnd)

{
   opnd_type	l_opnd;

   TRACE (Func_Entry, "mark_attr_defined", NULL);

   COPY_OPND(l_opnd, (*opnd));

   while (OPND_FLD(l_opnd) == IR_Tbl_Idx) {
      COPY_OPND(l_opnd, IR_OPND_L(OPND_IDX(l_opnd)));
   }

   if (OPND_FLD(l_opnd)               == AT_Tbl_Idx &&
       AT_OBJ_CLASS(OPND_IDX(l_opnd)) == Data_Obj)  {
      
      AT_DEFINED(OPND_IDX(l_opnd)) = TRUE;

      if (ATD_CLASS(OPND_IDX(l_opnd)) == Function_Result) {
         AT_DEFINED(ATD_FUNC_IDX(OPND_IDX(l_opnd))) = TRUE;
      }

   }
   

   TRACE (Func_Exit, "mark_attr_defined", NULL);

   return;

}  /* mark_attr_defined */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine will parse ahead to see if a io list item in a paren     *|
|*      group is a complex constant or not. This is needed because SOMEBODY   *|
|*      thought cft77 should allow extra paren groups in io lists for         *|
|*      clarification.                                                        *|
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

boolean	paren_grp_is_cplx_const(void)

{
   int			cx_l = NULL_IDX;
   int			cx_r = NULL_IDX;
   expr_arg_type	exp_desc;
   boolean		is_constant = FALSE;
   boolean		parsed_ok;
   opnd_type		the_opnd;


   TRACE (Func_Entry, "paren_grp_is_cplx_const", NULL);

   /* LA_CH is char after ( */

   if (LA_CH_VALUE == SLASH) {
      /* not a complex constant, maybe a constructor */
      goto EXIT;
   }
   else if (!parse_expr(&the_opnd)) {
      goto EXIT;
   }
   else if (LA_CH_VALUE != COMMA) {
      goto EXIT;
   }

   /* Assume complex constant - Try to fold */
   if (OPND_FLD(the_opnd) == CN_Tbl_Idx) {
      cx_l = OPND_IDX(the_opnd);
   }
   else if (OPND_FLD(the_opnd)               == AT_Tbl_Idx &&
            AT_OBJ_CLASS(OPND_IDX(the_opnd)) == Data_Obj   &&
            ATD_CLASS(OPND_IDX(the_opnd))    == Constant   &&
            ATD_FLD(OPND_IDX(the_opnd))      == CN_Tbl_Idx) {

      cx_l = ATD_CONST_IDX(OPND_IDX(the_opnd));
   }
   else if (OPND_FLD(the_opnd)               == IR_Tbl_Idx  &&
            (IR_OPR(OPND_IDX(the_opnd))      == Uplus_Opr ||
             IR_OPR(OPND_IDX(the_opnd))      == Uminus_Opr) &&
            (IR_FLD_L(OPND_IDX(the_opnd))    == CN_Tbl_Idx ||
             (IR_FLD_L(OPND_IDX(the_opnd))    == AT_Tbl_Idx &&
              AT_OBJ_CLASS(IR_IDX_L(OPND_IDX(the_opnd))) == Data_Obj &&
              ATD_CLASS(IR_IDX_L(OPND_IDX(the_opnd)))    == Constant &&
              ATD_FLD(IR_IDX_L(OPND_IDX(the_opnd))) == CN_Tbl_Idx))) {

      exp_desc.rank = 0;
      xref_state    = CIF_No_Usage_Rec;
      comp_gen_expr = TRUE;
      parsed_ok = expr_semantics(&the_opnd, &exp_desc);
      comp_gen_expr = FALSE;

      if (parsed_ok                         &&
          OPND_FLD(the_opnd) == CN_Tbl_Idx) {
         cx_l = OPND_IDX(the_opnd);
      }
   }

   if (cx_l                            &&
       (TYP_TYPE(CN_TYPE_IDX(cx_l)) == Real ||
        TYP_TYPE(CN_TYPE_IDX(cx_l)) == Integer)) {

      /* swallow comma */
      NEXT_LA_CH;

      if (!parse_expr(&the_opnd)) {
         goto EXIT;
      }
      else if (LA_CH_VALUE != RPAREN) {
         goto EXIT;
      }
      else {

         if (OPND_FLD(the_opnd) == CN_Tbl_Idx) {
            cx_r = OPND_IDX(the_opnd);
         }
         else if (OPND_FLD(the_opnd)               == AT_Tbl_Idx &&
                  AT_OBJ_CLASS(OPND_IDX(the_opnd)) == Data_Obj   &&
                  ATD_CLASS(OPND_IDX(the_opnd))    == Constant   &&
                  ATD_FLD(OPND_IDX(the_opnd))      == CN_Tbl_Idx) {

            cx_r = ATD_CONST_IDX(OPND_IDX(the_opnd));
         }
         else if (OPND_FLD(the_opnd)               == IR_Tbl_Idx  &&
                  (IR_OPR(OPND_IDX(the_opnd))      == Uplus_Opr ||
                   IR_OPR(OPND_IDX(the_opnd))      == Uminus_Opr) &&
                  (IR_FLD_L(OPND_IDX(the_opnd))    == CN_Tbl_Idx ||
                   (IR_FLD_L(OPND_IDX(the_opnd))    == AT_Tbl_Idx &&
                    AT_OBJ_CLASS(IR_IDX_L(OPND_IDX(the_opnd))) == Data_Obj &&
                    ATD_CLASS(IR_IDX_L(OPND_IDX(the_opnd)))    == Constant &&
                    ATD_FLD(IR_IDX_L(OPND_IDX(the_opnd))) == CN_Tbl_Idx))) {

            exp_desc.rank = 0;
            xref_state    = CIF_No_Usage_Rec;
            comp_gen_expr = TRUE;
            parsed_ok = expr_semantics(&the_opnd, &exp_desc);
            comp_gen_expr = FALSE;

            if (parsed_ok                         &&
                OPND_FLD(the_opnd) == CN_Tbl_Idx) {
               cx_r = OPND_IDX(the_opnd);
            }
         }


         if (cx_r                               &&
             (TYP_TYPE(CN_TYPE_IDX(cx_r)) == Real ||
              TYP_TYPE(CN_TYPE_IDX(cx_r)) == Integer)) {
   
            is_constant = TRUE;
         }
      }
   }


EXIT:

   TRACE (Func_Exit, "paren_grp_is_cplx_const", NULL);

   return(is_constant);

}  /* "paren_grp_is_cplx_const" */

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

void check_for_vestigial_task_blks(void)

{

   TRACE (Func_Entry, "check_for_vestigial_task_blks", NULL);

   while (blk_stk_idx > 1  &&
          (BLK_TYPE(blk_stk_idx) == Do_Parallel_Blk ||
           BLK_TYPE(blk_stk_idx) == SGI_Pdo_Blk ||
           BLK_TYPE(blk_stk_idx) == Open_Mp_Do_Blk ||
           BLK_TYPE(blk_stk_idx) == Open_Mp_Parallel_Do_Blk)) {

      POP_BLK_STK;

      switch (CURR_BLK) {
      case Do_Parallel_Blk:
         CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
         break;

      case SGI_Pdo_Blk:
         CLEAR_DIRECTIVE_STATE(Pdo_Region);
         break;

      case Open_Mp_Do_Blk:
         CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
         break;

      case Open_Mp_Parallel_Do_Blk:
         CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
         break;

      }
   }

   TRACE (Func_Exit, "check_for_vestigial_task_blks", NULL);

   return;

}  /* check_for_vestigial_task_blks */

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

void set_up_fake_dt_blk(int	dt_idx)

{


   TRACE (Func_Entry, "set_up_fake_dt_blk", NULL);

   if (dt_idx == NULL_IDX) {
      if (blk_stk_idx > 0) {
         POP_BLK_STK;
      }
   }
   else {
      PUSH_BLK_STK(Derived_Type_Blk);
      CURR_BLK_NAME                = dt_idx;
   }

   TRACE (Func_Exit, "set_up_fake_dt_blk", NULL);

   return;

}  /* set_up_fake_dt_blk */
