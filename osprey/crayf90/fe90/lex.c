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



static char USMID[] = "\n@(#)5.0_pl/sources/lex.c	5.8	08/23/99 17:26:51\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "lex.m"
# include "debug.m"

# ifdef _ARITH_H
# include "arith.h"
# endif

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "lex.h"
# include <errno.h>

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static boolean  convert_const(void);
static boolean	fixed_get_keyword (void);
static boolean	free_get_keyword (void);
static boolean	get_directive (void);
static boolean	get_format_str (void);
static boolean	get_label (void);
static boolean	get_micro_directive (void);
static boolean get_open_mp_directive (void);
static boolean	get_sgi_directive (void);
static boolean	get_operand_digit (void);
static boolean	get_operand_dot (void);
static boolean	get_operand_letter (void);
static boolean	get_operand_quote (void);
static boolean	get_operator (void);
static boolean	get_operator_dot (void);
static boolean	get_program_str (void);
static boolean	get_punctuator (void);
static void     convert_octal_literal (boolean);
static void 	convert_hex_literal(boolean);
static void 	convert_binary_literal(boolean);
static void	set_up_letter_idx_table(int *,kwd_type *, int);

# ifdef _DEBUG
static boolean	get_debug_directive (void);
# endif


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Init_lex is called by the main compiler driver to initialize the look *|
|*	ahead character to the first charcter of the first source input line. *|
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

void init_lex (void)

{
   int word;


   TRACE (Func_Entry, "init_lex", NULL);

   /* set function pointers for getting next char based on source form */

   if (source_form == Fixed_Form) {
      get_char	       = fixed_get_char;
      get_char_literal = fixed_get_char_literal;
   }
   else {
      get_char	       = free_get_char;
      get_char_literal = free_get_char_literal;
   }


   /* get first look ahead char of source */

   NEXT_LA_CH;


   /* Set N$PES sighting flag.						      */

   havent_issued_ndollarpes_ansi = TRUE;


   /* initialize initial_token used to initialize token */

   for (word = 0;  word < NUM_ID_WDS;  word++) {
      TOKEN_STR_WD(initial_token, word) = 0;
   }

   TOKEN_LEN(initial_token)		= 0;
   TOKEN_VALUE(initial_token)		= Tok_Unknown;
   TOKEN_ERR(initial_token)		= FALSE;
   TOKEN_KIND_STR(initial_token)[0]	= EOS;
   TOKEN_KIND_LEN(initial_token)	= 0;
   TOKEN_COLUMN(initial_token)		= 0;
   TOKEN_LINE(initial_token)		= 0;
   TOKEN_BUF_IDX(initial_token)		= 0;
   TOKEN_STMT_NUM(initial_token)	= 0;

   TRACE (Func_Exit, "init_lex", NULL);

   return;

}  /* init_lex */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Used for error recovery.  This keeps getting the LA_CH value until    *|
|*      LA_CH is EOS.   The token is UNDEFINED after this is called.          *|
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

void flush_LA_to_EOS (void)

{
   TRACE (Func_Entry, "flush_LA_to_EOS", NULL);

   la_ch = stmt_EOS_la_ch;

   TRACE (Func_Exit, "flush_LA_to_EOS", NULL);

   return;

}  /* flush_LA_to_EOS */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine skips to the next punctuator, discarding the intervening *|
|*	characters.   LA_CH is set to the punctuator.  Token is UNDEFINED     *|
|*	upon exit.   It skips over quoted strings, hollerith, boz constants,  *|
|*	and other operands.  Used for error recovery.                         *|
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

void flush_LA_to_symbol (void)

{

   TRACE (Func_Entry, "flush_LA_to_symbol", NULL);
   
   do {

      if (LA_CH_CLASS == Ch_Class_Letter) {
         NEXT_LA_CH;
         while (VALID_LA_CH) {
            NEXT_LA_CH;
         }
      }
      else if (LA_CH_CLASS == Ch_Class_Digit || LA_CH_VALUE == DBL_QUOTE ||
          LA_CH_VALUE == QUOTE) {

         /* Skip quoted strings, hollerith constants, and other strange stuff */

         get_token(Tok_Class_Opnd);
      }
      else {
         NEXT_LA_CH;
      }
   }
   while (LA_CH_CLASS != Ch_Class_EOS  &&  LA_CH_CLASS != Ch_Class_Symbol);

   TRACE (Func_Exit, "flush_LA_to_symbol", NULL);

   return;

}  /* flush_LA_to_symbol */
#ifdef KEY /* Bug 3635 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check whether token exceeds our internal limit, and whether it        *|
|*	exceeds the ANSI standard limit, issuing a warning and, if need be,   *|
|*	adjusting tok_len to truncate the identifier to meet the internal     *|
|*      limit.                                                                *|
|*									      *|
|* Input parameters:							      *|
|*	tok_len			length of token             	 	      *|
|*									      *|
|* Output parameters:							      *|
|*	tok_len			new length of token            	 	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates that we needed to truncate the token                   *|
|*	FALSE indicates that we did not need to truncate the token	      *|
|*									      *|
\******************************************************************************/
static int id_too_long(int *tok_len)
{
   if (*tok_len > ANSI90_ID_LEN) {
      if (*tok_len > MAX_ID_LEN) {
	 /* Id len exceeds our maximum, so truncate */
	 PRINTMSG (TOKEN_LINE(token), 67, Warning, TOKEN_COLUMN(token));
	 *tok_len = MAX_ID_LEN;
	 return 1;
      }
      /* Exceeds constraint imposed by F90 std, which requires
	 us to issue a message */
      PRINTMSG (TOKEN_LINE(token), 1671, Ansi, TOKEN_COLUMN(token));
   }
   return 0;
}

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Like id_too_long, but slightly different message.                     *|
|*									      *|
\******************************************************************************/
static int kind_too_long(int *tok_len) {
  if (*tok_len > ANSI90_ID_LEN) {
    if (*tok_len > MAX_ID_LEN) {
      /* Id len exceeds our maximum, so truncate */
      *tok_len = MAX_ID_LEN;
      PRINTMSG(LA_CH_LINE, 67, Warning, LA_CH_COLUMN);
      return 1;
    }
  /* Exceeds constraint imposed by F90 std, which requires
     us to issue a message */
  PRINTMSG(LA_CH_LINE, 101, Ansi, LA_CH_COLUMN);
  }
  return 0;
}

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Like id_too_long and kind_too_long, but slightly different message.   *|
|*									      *|
\******************************************************************************/
static int defined_op_too_long(int *tok_len) {
  if (*tok_len > ANSI90_ID_LEN) {
    if (*tok_len > MAX_ID_LEN) {
      /* Id len exceeds our maximum, so truncate */
      *tok_len = MAX_ID_LEN;
      PRINTMSG(LA_CH_LINE, 67, Warning, LA_CH_COLUMN);
      return 1;
    }
  /* Exceeds constraint imposed by F90 std, which requires
     us to issue a message */
  PRINTMSG(LA_CH_LINE, 65, Ansi, LA_CH_COLUMN);
  }
  return 0;
}
#endif /* KEY Bug 3635 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_token is called by the parser to construct a token out of the     *|
|*	current look ahead character and additional characters from the input *|
|*	source.	 The parser supplies context information describing the class *|
|*	of token needed in the input parameter.	 Get_token uses the requested *|
|*	class to call the appropriate get_... routine to create a token.      *|
|*									      *|
|* Input parameters:							      *|
|*	class			parser requested token class		      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token produced by scanner		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates that a token of the requested class was produced.      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

boolean get_token (token_class_type class)

{
   boolean	result		= FALSE;
   int		tok_len		= 0;


   TRACE (Func_Entry, "get_token", NULL);

   comp_phase	= Lex_Parsing;

   switch (class) {
      case Tok_Class_Id :

         if (LA_CH_CLASS == Ch_Class_Letter         ||
             (on_off_flags.allow_leading_uscore &&
              LA_CH_VALUE == USCORE))               {

            sig_blank		= FALSE;
            result		= TRUE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            TOKEN_VALUE(token)	= Tok_Id;

            while (VALID_LA_CH) {
               ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
               NEXT_LA_CH;
            }
            TOKEN_LEN(token)	= tok_len;

#ifdef KEY /* Bug 3635 */
            if (id_too_long(&tok_len)) {
               TOKEN_LEN(token) = MAX_ID_LEN;
	    }
#else
            if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 chars */
               PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
               TOKEN_LEN(token) = MAX_ID_LEN;
            }
#endif /* KEY Bug 3635 */
            else if (tok_len == 5  &&
                     strcmp(TOKEN_STR(token), "N$PES") == 0  &&
                     havent_issued_ndollarpes_ansi) {
               PRINTMSG (TOKEN_LINE(token), 1414, Ansi, TOKEN_COLUMN(token));
               havent_issued_ndollarpes_ansi = FALSE;
            }
         }
	 break;

      case Tok_Class_Keyword :

         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
	    result		= (source_form == Fixed_Form)  ?
                                   fixed_get_keyword () : free_get_keyword ();
         }
         else if (LA_CH_CLASS == Ch_Class_Dir1) {

            /* Only set to Ch_Class_Dir1 by src_input if the next letters */
            /* are known to be $ (for sgi C$ directives )                 */

            sig_blank           = FALSE;
            token               = initial_token;
            TOKEN_LINE(token)   = LA_CH_LINE;
            TOKEN_COLUMN(token) = LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            TOKEN_VALUE(token)  = Tok_Kwd_Dir;
            TOKEN_LEN(token)    = 1;

            for (tok_len = 0; tok_len < TOKEN_LEN(token); tok_len++) {
               TOKEN_STR(token)[tok_len] = LA_CH_VALUE;
               NEXT_LA_CH;
            }
            result = TRUE;
         }
         else if (LA_CH_CLASS == Ch_Class_Dir2) {

            
            /* Only set to Ch_Class_Dir2 by src_input if the prefix is known */
            /* to be 2 chars. There are none at this time.                   */

            sig_blank           = FALSE;
            token               = initial_token;
            TOKEN_LINE(token)   = LA_CH_LINE;
            TOKEN_COLUMN(token) = LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            TOKEN_VALUE(token)  = Tok_Kwd_Dir;
            TOKEN_LEN(token)    = 2;

            for (tok_len = 0; tok_len < TOKEN_LEN(token); tok_len++) {
               TOKEN_STR(token)[tok_len] = LA_CH_VALUE;
               NEXT_LA_CH;
            }
            result = TRUE;
         }
         else if (LA_CH_CLASS == Ch_Class_Dir3) {

            /* Only set to Ch_Class_Dir3 by src_input if the next letters */
            /* are known to be the start of a C*$* directive              */

            sig_blank           = FALSE;
            token               = initial_token;
            TOKEN_LINE(token)   = LA_CH_LINE;
            TOKEN_COLUMN(token) = LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            TOKEN_VALUE(token)  = Tok_Kwd_Dir;
            TOKEN_LEN(token)    = 3;

            for (tok_len = 0; tok_len < TOKEN_LEN(token); tok_len++) {
               TOKEN_STR(token)[tok_len] = LA_CH_VALUE;
               NEXT_LA_CH;
            }
            result = TRUE;
         }
         else if (LA_CH_CLASS == Ch_Class_Dir4) {

            /* Only set to Ch_Class_Dir4 by src_input if the next letters */
            /* are known to be the start of a CMIC, CDIR, or C$PAR        */

            sig_blank           = FALSE;
            token               = initial_token;
            TOKEN_LINE(token)   = LA_CH_LINE;
            TOKEN_COLUMN(token) = LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            TOKEN_VALUE(token)  = Tok_Kwd_Dir;
            TOKEN_LEN(token)    = 4;

            for (tok_len = 0; tok_len < TOKEN_LEN(token); tok_len++) {
               TOKEN_STR(token)[tok_len] = LA_CH_VALUE;
               NEXT_LA_CH;
            }
            result = TRUE;
         }
         else if (LA_CH_VALUE == USCORE &&
                  on_off_flags.allow_leading_uscore) {
         
            sig_blank           = FALSE;
            result              = TRUE;
            token               = initial_token;
            TOKEN_LINE(token)   = LA_CH_LINE;
            TOKEN_COLUMN(token) = LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            TOKEN_VALUE(token)  = Tok_Id;

            while (VALID_LA_CH) {
               ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
               NEXT_LA_CH;
            }
            TOKEN_LEN(token)    = tok_len;

#ifdef KEY /* Bug 3635 */
            if (id_too_long(&tok_len)) {
               TOKEN_LEN(token) = MAX_ID_LEN;
	    }
#else
            if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 chars */
               PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
               TOKEN_LEN(token) = MAX_ID_LEN;
            }
#endif /* KEY Bug 3635 */
         }

	 break;

      case Tok_Class_Punct :
         if (LA_CH_CLASS == Ch_Class_Symbol || LA_CH_CLASS == Ch_Class_EOS) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
	    result		= get_punctuator ();
         }
	 break;

      case Tok_Class_Op :
         if (LA_CH_CLASS == Ch_Class_Symbol) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
	    result		= get_operator ();
         }
	 break;

      case Tok_Class_Opnd :
         switch (LA_CH_CLASS) {
            case Ch_Class_Digit:
               sig_blank		= FALSE;
               token			= initial_token;
               TOKEN_LINE(token)	= LA_CH_LINE;
               TOKEN_COLUMN(token)	= LA_CH_COLUMN;
               TOKEN_BUF_IDX(token)	= LA_CH_BUF_IDX;
               TOKEN_STMT_NUM(token)	= LA_CH_STMT_NUM;
	       result			= get_operand_digit ();
               break;

     	    case Ch_Class_Letter:
               sig_blank		= FALSE;
               token			= initial_token;
               TOKEN_LINE(token)	= LA_CH_LINE;
               TOKEN_BUF_IDX(token)	= LA_CH_BUF_IDX;
               TOKEN_STMT_NUM(token)	= LA_CH_STMT_NUM;
               TOKEN_COLUMN(token)	= LA_CH_COLUMN;
	       result			= get_operand_letter ();

               if (TOKEN_LEN(token) == 5  &&
                   strcmp(TOKEN_STR(token), "N$PES") == 0  &&
                   havent_issued_ndollarpes_ansi) {
                  PRINTMSG (TOKEN_LINE(token), 1414, Ansi, TOKEN_COLUMN(token));
                  havent_issued_ndollarpes_ansi = FALSE;

               }

               break;

            case Ch_Class_EOS:
	       result = FALSE;
               break;

            case Ch_Class_Symbol:
	       if (LA_CH_VALUE == DOT) {
                  sig_blank		= FALSE;
                  token			= initial_token;
                  TOKEN_LINE(token)	= LA_CH_LINE;
                  TOKEN_COLUMN(token)	= LA_CH_COLUMN;
                  TOKEN_BUF_IDX(token)	= LA_CH_BUF_IDX;
                  TOKEN_STMT_NUM(token)	= LA_CH_STMT_NUM;
                  result		= get_operand_dot ();
               }
               else if (LA_CH_VALUE == QUOTE  ||  LA_CH_VALUE == DBL_QUOTE) {
                  sig_blank		= FALSE;
                  token			= initial_token;
                  TOKEN_LINE(token)	= LA_CH_LINE;
                  TOKEN_COLUMN(token)	= LA_CH_COLUMN;
                  TOKEN_BUF_IDX(token)	= LA_CH_BUF_IDX;
                  TOKEN_STMT_NUM(token)	= LA_CH_STMT_NUM;
                  result		= get_operand_quote ();
              }
              break;
         } /* End switch */
     	 break;

      case Tok_Class_Int_Spec :
         if (LA_CH_CLASS == Ch_Class_Digit) {
            sig_blank		= FALSE;
            result		= TRUE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            TOKEN_VALUE(token)	= Tok_Const_Int;

            while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
               ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
               NEXT_LA_CH;
            }

            CHECK_FOR_FREE_BLANK;

            const_buf[tok_len] = '\0';

            CONVERT_INT_CONST(INTEGER_DEFAULT_TYPE, tok_len, result);
         }
	 break;

      case Tok_Class_Label :
         if (LA_CH_CLASS == Ch_Class_Digit) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result		= get_label ();
         }
	 break;

      case Tok_Class_Construct_Def :

         /* This could really be written as a buffer search.  */
         /* done at the start of every statement.   JLS/jls   */
         /* TRUE returned if construct name of < 32 chars     */
         /* found and is followed by a single colon.          */

         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;

            while (VALID_LA_CH) {
               ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
               NEXT_LA_CH;
            }

            /* Check to see if this is really a construct name */

            if (LA_CH_VALUE != COLON) {
               reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
            }
            else {
               NEXT_LA_CH;

               if (LA_CH_VALUE != COLON) { /* id followed by :: */
#ifdef KEY /* Bug 3635 */
		  id_too_long(&tok_len);
#else
                  if (tok_len > MAX_ID_LEN) { 
                     /* Id len exceeds maximum of 31 chars */
                     PRINTMSG (TOKEN_LINE(token), 67, Error, 
                               TOKEN_COLUMN(token));
                     tok_len = MAX_ID_LEN;
                  }
#endif /* KEY Bug 3635 */
                  result		= TRUE;
                  TOKEN_LEN(token)	= tok_len;
                  TOKEN_VALUE(token)	= Tok_Id;
               }
               else {
                  reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
               }
            }
         }
         break;

      case Tok_Class_DO :
         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;

            if (source_form == Fixed_Form) {

               /* This is a kludge to get around the fixed source form problem*/
               /* of DOUBLE possibly being a DO stmt (e.g. DO uble = ...).    */
               /* It is assumed the DOUBLE was previously parsed as type      */
               /* specification statement and failed making the check a DO    */
               /* stmt necessary.  Since DOUBLE has already been seen, this   */
               /* routine is rather trivial but must be done in order to get  */
               /* the line and column information in the loop variable token  */
               /* (UBLE) correct if this is indeed a DO stmt.	              */

               TOKEN_VALUE(token)	= Tok_Kwd_Do;
               result			= TRUE;

               if (LA_CH_VALUE != 'D') {  /* Abort compilation */

                  /* Invalid character encountered;  expected keyword         */
		  /* DO following failed parse of type spec DOUBLE            */

                  /* Following is a detailed description of when message 38   */
                  /* issues.  The parser encountered a DOUBLE keyword at the  */
                  /* beginning of a statement and attempted to parse the      */
                  /* statement as a type specifier and failed.  The parser    */
                  /* has to special case this situation by reseting the       */
                  /* scanner input and asking for the DO keyword using        */
                  /* Tok_Class_Do in get_token, because DOUBLE could possibly */
                  /* be a DO statement in fixed source form                   */
                  /* (e.g. DO uble = ...).  The scanner assumes DOUBLE had    */
                  /* been previously fetched as a token and therefore the     */
                  /* routine that gets the DO keyword (fixed_get_DO_keyword)  */
                  /* makes assumptions by expecting DO.                       */

            	  PRINTMSG (LA_CH_LINE, 38, Internal, LA_CH_COLUMN);
               }
               NEXT_LA_CH;

               if (LA_CH_VALUE != 'O') {  /* Abort compilation - see above */
            	  PRINTMSG (LA_CH_LINE, 38, Internal, LA_CH_COLUMN);
               }

               TOKEN_STR(token)[0]	= 'D';
               TOKEN_STR(token)[1]	= 'O';
               TOKEN_LEN(token)		= 2;
               NEXT_LA_CH;
            }
            else { 
               result = free_get_keyword ();
            }

         }
	 break;

      case Tok_Class_Dir_Kwd :
         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result		= get_directive ();
         }
	 break;

      case Tok_Class_Mic_Kwd :
         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result		= get_micro_directive ();
         }
	 break;

      case Tok_Class_Open_Mp_Dir_Kwd :
         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank           = FALSE;
            token               = initial_token;
            TOKEN_LINE(token)   = LA_CH_LINE;
            TOKEN_COLUMN(token) = LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result              = get_open_mp_directive ();
         }
         break;

      case Tok_Class_SGI_Dir_Kwd :
         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank           = FALSE;
            token               = initial_token;
            TOKEN_LINE(token)   = LA_CH_LINE;
            TOKEN_COLUMN(token) = LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result              = get_sgi_directive ();
         }
         break;

# ifdef _DEBUG
      case Tok_Class_Dbg_Kwd :
         if (LA_CH_CLASS == Ch_Class_Letter) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result		= get_debug_directive ();
         }
	 break;
# endif

      case Tok_Class_Format_Str :
         if (LA_CH_VALUE == LPAREN) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result		= get_format_str ();
         }
	 break;

      case Tok_Class_Program_Str :
         if (LA_CH_VALUE == LPAREN) {
            sig_blank		= FALSE;
            token		= initial_token;
            TOKEN_LINE(token)	= LA_CH_LINE;
            TOKEN_COLUMN(token)	= LA_CH_COLUMN;
            TOKEN_BUF_IDX(token) = LA_CH_BUF_IDX;
            TOKEN_STMT_NUM(token) = LA_CH_STMT_NUM;
            result		= get_program_str ();
         }
	 break;
   }  /* switch */

   comp_phase	= Pass1_Parsing;
      
   TRACE (Func_Exit, "get_token", NULL);

   return (result);

}  /* get_token */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Reset_lex is called by the parser to backup the token stream for a    *|
|*	rescan.	 The reset_src_input routine is called to reposition to the   *|
|*	requested source input character and reset the global la_ch.	      *|
|*	Be careful not to call this routine, if token is already EOS.         *|
|*	Strange and not so wonderful things can happen.                       *|
|*									      *|
|* Input parameters:							      *|
|*	buf_idx			stmt_buf_idx to reset to.           	      *|
|*	stmt_num		gets passed on to reset_src_input.            *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void reset_lex (int buf_idx,
		int stmt_num)

{
   TRACE (Func_Entry, "reset_lex", NULL);

   reset_src_input (--buf_idx, stmt_num);

   NEXT_LA_CH;

   TRACE (Func_Exit, "reset_lex", NULL);

   return;

}  /* reset_lex */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_directive is called by the get_token routine to attempt	      *|
|*	recognition of a directive keyword by matching the look ahead char    *|
|*	and following characters of class Ch_Class_Letter with entries in the *|
|*	kwd_dir table.	If a keyword is not found, an id token is created.    *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of directive kwd token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_directive		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a keyword or id token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_directive (void)

{
   int		beg_idx;
   la_type	la_queue[MAX_KWD_LEN + 1];
   int		letter_idx;
   int		lim_idx;
   int		tok_len		= 0;
      

   TRACE (Func_Entry, "get_directive", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_directive", "letter");
   }
# endif

   TOKEN_VALUE(token) = Tok_Id;

   /* check for any directive keywords starting with look ahead char */

   letter_idx	= LA_CH_VALUE - 'A';
   beg_idx	= kwd_dir_idx[letter_idx];
   lim_idx	= kwd_dir_idx[letter_idx+1];
   
   if (beg_idx != lim_idx) {

#ifdef _DEBUG
      if (kwd_dir_len[beg_idx] > MAX_ID_LEN) {
         PRINTMSG(TOKEN_LINE(token), 384, Internal, TOKEN_COLUMN(token),
                  beg_idx, kwd_dir_len[beg_idx]);
      }
# endif

      while ((LA_CH_CLASS == Ch_Class_Letter || LA_CH_VALUE == USCORE) && 
             tok_len < kwd_dir_len[beg_idx]) {

         /* Internal error if kwd_dir len ever exceeds MAX_ID_LEN        */
         /* There is no check.  Assume keywords may never be > 31 chars. */

	 la_queue[tok_len]		= la_ch;
         TOKEN_STR(token)[tok_len]	= LA_CH_VALUE;
         tok_len++;
	 NEXT_LA_CH;
      }
      
      TOKEN_LEN(token) = tok_len;

      if (tok_len >= kwd_dir_len[lim_idx-1]) {

	 /* compare token string to directive keyword entries */

	 while (beg_idx < lim_idx) {

	    if (kwd_dir_len[beg_idx] <= tok_len) {

	       if (strncmp(TOKEN_STR(token),
			   kwd_dir[beg_idx].name,
			   kwd_dir_len[beg_idx]) == IDENTICAL) {

		  /* the following chars and preceding letter can't be */
		  /* part of a keyword on full length match of string. */

		  if (tok_len == kwd_dir_len[beg_idx]  &&
		      (LA_CH_VALUE == USCORE  ||
		       LA_CH_VALUE == DOLLAR  ||
		       LA_CH_VALUE == AT_SIGN)) {
		  }
		  else {
		     TOKEN_VALUE(token) = kwd_dir[beg_idx].value;

		     /* adjust la_ch to be char following keyword */

		     if (tok_len > kwd_dir_len[beg_idx]) {
			tok_len			= kwd_dir_len[beg_idx];
			la_ch			= la_queue[tok_len];
			TOKEN_LEN(token)	= tok_len;
			reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
		     }
		     break;
		  }
	       }
	    }

	    beg_idx++;

	 }  /* while */
      }	 /* if */
   }  /* if */

   if (TOKEN_VALUE(token) == Tok_Id) {			/* keyword not found  */

      while (VALID_LA_CH) {
	 ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
	 NEXT_LA_CH;
      }
    
#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 characters. */
         PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
	 tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */

      TOKEN_LEN(token) = tok_len;
   }

   TRACE (Func_Exit, "get_directive", NULL);

   return (TRUE);
   
}  /* get_directive */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_format_str is called by the get_token routine to attempt	      *|
|*	recognition of a format string by using the look ahead character and  *|
|*	following characters of the input source.  Blanks and embedded	      *|
|*	comments are removed from the string.				      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			opening paren of format string		      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_format_str		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a format string token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_format_str (void)

{

   TRACE (Func_Entry, "get_format_str", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != LPAREN) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_format_str", "(");
   }
# endif

   TOKEN_VALUE(token)		= Tok_Const_Char;	
   TOKEN_CONST_TBL_IDX(token)	= put_format_in_tbl();

   TRACE (Func_Exit, "get_format_str", NULL);

   return (TRUE);

}  /* get_format_str */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Fixed_get_keyword is called in fixed input source form by the	      *|
|*	get_token routine to attempt recognition of a keyword by matching the *|
|*	look ahead char and following characters of class Ch_Class_Letter     *|
|*	with entries in the kwd table.	If a keyword is not found, an id      *|
|*	token is created.						      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of kwd token		      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_keyword		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a keyword or id token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean fixed_get_keyword (void)

{
   int		beg_idx;
   la_type	la_queue[MAX_KWD_LEN + 1];
   int		letter_idx;
   int		lim_idx;
   int		tok_len		= 0;
      

   TRACE (Func_Entry, "fixed_get_keyword", NULL);


# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "fixed_get_keyword", "letter");
   }
# endif

   TOKEN_VALUE(token) = Tok_Id;
   
   /* check for any keywords starting with look ahead char */
   letter_idx = LA_CH_VALUE - 'A';

   beg_idx = kwd_idx[letter_idx];
   lim_idx = kwd_idx[letter_idx+1];
   
   if (beg_idx != lim_idx) {

#ifdef _DEBUG
      if (kwd_len[beg_idx] > MAX_ID_LEN) {
         PRINTMSG(TOKEN_LINE(token), 384, Internal, TOKEN_COLUMN(token),
                  beg_idx, kwd_len[beg_idx]);
      }
# endif

      while (LA_CH_CLASS == Ch_Class_Letter && tok_len < kwd_len[beg_idx]) {
         la_queue[tok_len] = la_ch;
         TOKEN_STR(token)[tok_len] = LA_CH_VALUE;
         tok_len++;
	 NEXT_LA_CH;
      }

      TOKEN_LEN(token) = tok_len;

      if (tok_len >= kwd_len[lim_idx-1]) {

         /* compare token string to keyword entries */

	 while (beg_idx < lim_idx) {
	    if (tok_len >= kwd_len[beg_idx]) {
	       if (strncmp(TOKEN_STR(token),
	       		   kwd[beg_idx].name,
			   kwd_len[beg_idx]) == IDENTICAL) {

                  /* the following chars and preceding letter can't be */
		  /* part of a keyword on full length match of string. */

		  if (tok_len == kwd_len[beg_idx]  &&
                      ! on_off_flags.allow_leading_uscore  &&
		      (LA_CH_VALUE == USCORE	 ||
		       LA_CH_VALUE == DOLLAR	 ||
		       LA_CH_VALUE == AT_SIGN)) {
                  }
	          else {
	             TOKEN_VALUE(token) = kwd[beg_idx].value;

		     /* adjust la_ch to be char following keyword */

		     if (tok_len > kwd_len[beg_idx]) {
		        tok_len		= kwd_len[beg_idx];
		        la_ch		= la_queue[tok_len];
                        TOKEN_LEN(token)	= tok_len;
		        reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
		     }
		     break;
                  }
	       }
	    }

	    beg_idx++;

         }  /* while */
      }  /* if */
   }	 /* if */

   if (TOKEN_VALUE(token) == Tok_Id) {		/* keyword not found  */

      while (VALID_LA_CH) {
         ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
         NEXT_LA_CH;
      }
   
#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds max of 31 characters */
         PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
         tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */
      TOKEN_LEN(token) = tok_len;
   }

   TRACE (Func_Exit, "fixed_get_keyword", NULL);

   return (TRUE);
   
}  /* fixed_get_keyword */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Free_get_keyword is called in free input source form by the	      *|
|*	get_token routine to attempt recognition of a keyword by matching the *|
|*	look ahead char and following characters of class Ch_Class_Letter     *|
|*	with entries in the kwd table.	If a keyword is not found, an id      *|
|*	token is created.						      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of kwd token		      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_keyword		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a keyword or id token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean free_get_keyword (void)

{
   boolean	all_letters	= TRUE;
   int		beg_idx;
   la_type	la_queue[MAX_KWD_LEN + 1];
   int		letter_idx;
   int		lim_idx;
   int		tok_len		= 0;
      

   TRACE (Func_Entry, "free_get_keyword", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "free_get_keyword", "letter");
   }
# endif

   TOKEN_VALUE(token) = Tok_Id;

   while (VALID_LA_CH) {
      if (LA_CH_CLASS != Ch_Class_Letter) {		/* non letter in kwd  */
#ifdef KEY /* Bug 5089 */
         /* F2003 allows "_" in keyword "NON_INTRINSIC" */
         all_letters = all_letters && ('_' == LA_CH_VALUE);
#else /* KEY Bug 5089 */
         all_letters = FALSE;
#endif /* KEY Bug 5089 */
      }

      if (tok_len < MAX_KWD_LEN) {
         la_queue[tok_len] = la_ch;
      }
      ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
      NEXT_LA_CH;
   }

   TOKEN_LEN(token) = tok_len;
   
   if (all_letters  &&  tok_len > 1  &&  tok_len <= MAX_KWD_LEN) {

      /* check for any keywords starting with first character of token */

      letter_idx	= TOKEN_STR(token)[0] - 'A';
      beg_idx		= kwd_idx[letter_idx];
      lim_idx		= kwd_idx[letter_idx+1];

      /* compare token string to keyword entries of matching length */
      while (beg_idx < lim_idx) {
         if (kwd_len[beg_idx] == tok_len) {
            if (EQUAL_STRS(TOKEN_STR(token), kwd[beg_idx].name)) {
     	       TOKEN_VALUE(token) = kwd[beg_idx].value;
               break;
            }
         }	
         beg_idx++;
      }

      if (beg_idx == lim_idx) {

         /* check for any alternate form keywords starting with first char */

         beg_idx = alt_kwd_idx[letter_idx];
         lim_idx = alt_kwd_idx[letter_idx+1];

         /* compare token string to alternate form keyword entries */

         while (beg_idx < lim_idx) {
            if (alt_kwd[beg_idx].len == tok_len) {
               if (EQUAL_STRS(TOKEN_STR(token), alt_kwd[beg_idx].name)) {
 	          TOKEN_VALUE(token)	= alt_kwd[beg_idx].value;
 	          tok_len		= alt_kwd[beg_idx].val_len;
                  TOKEN_LEN(token)	= tok_len;

                  /* adjust la_ch to be char following first keyword */

	          la_ch = la_queue[tok_len];

                  /* would'nt be here if we just passed a sig blank so .. */

                  sig_blank = FALSE;

                  /* reset src input buffer and column index to la_ch pos */

	 	  reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
	 	  break;
               }
	    }   

	    beg_idx++;

	 } /* while */
      }  /* if */
   }	 /* if */

   if (TOKEN_VALUE(token) == Tok_Id) {

#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds max of 31 characters */
         PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
         tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */
      TOKEN_LEN(token) = tok_len;
   }

   TRACE (Func_Exit, "free_get_keyword", NULL);

   return (TRUE);

}  /* free_get_keyword */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_label is called by the get_token routine to attempt recognition   *|
|*	of a label token using the look ahead character and following	      *|
|*	characters of the input source.					      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of label token		      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_label		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a label token was produced.			      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_label (void)

{
   int		tok_cnt		= 0;
   int		tok_len		= 0;
   

   TRACE (Func_Entry, "get_label", NULL);

# ifdef _DEBUG
      if (LA_CH_CLASS != Ch_Class_Digit) {
         PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
                  "get_label", "digit");
      }
# endif

   TOKEN_VALUE(token) = Tok_Label;

   while (LA_CH_VALUE == ZERO && !sig_blank) {
      NEXT_LA_CH;
      tok_cnt++;
   }

   while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
      ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
      NEXT_LA_CH;
      tok_cnt++;
   }

   CHECK_FOR_FREE_BLANK;

   TOKEN_LEN(token) = tok_len;

   if (tok_cnt == 0  ||  tok_cnt > 5) { 
      /* Label len exceeds max of 5 digits */
      PRINTMSG (TOKEN_LINE(token), 68, Error, TOKEN_COLUMN(token));

      /* truncate the long label so that error won't cascade */
      TOKEN_STR(token)[5] = '\0';
      TOKEN_LEN(token)    = 5;
      TOKEN_ERR(token)    = TRUE;
   }
   else if (tok_len == 0) { /* Label must have at least one non-zero digit */
      PRINTMSG (TOKEN_LINE(token), 69, Error, TOKEN_COLUMN(token));
      ADD_TO_TOKEN_STR ('0', TOKEN_LEN(token));
      TOKEN_ERR(token)    = TRUE;
   }

   TRACE (Func_Exit, "get_label", NULL);

   return (TRUE);

}  /* get_label */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_micro_directive is called by the get_token routine to attempt     *|
|*	recognition of a microtasking keyword by matching the look ahead char *|
|*	and following characters of class Ch_Class_Letter with entries in the *|
|*	kwd_mic table.	If a keyword is not found, an id token is created.    *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of microtasking kwd token     *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_micro_directive	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a keyword or id token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_micro_directive (void)

{
   int		beg_idx;
   la_type	la_queue[MAX_KWD_LEN + 1];
   int		letter_idx;
   int		lim_idx;
   int		tok_len		= 0;
      

   TRACE (Func_Entry, "get_micro_directive", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_micro_directive", "letter");
   }
# endif

   TOKEN_VALUE(token) = Tok_Id;

   /* check for any microtasking keywords starting with look ahead char */
   letter_idx = LA_CH_VALUE - 'A';

   beg_idx = kwd_mic_idx[letter_idx];
   lim_idx = kwd_mic_idx[letter_idx+1];
   
   if (beg_idx != lim_idx) {

#ifdef _DEBUG
      if (kwd_mic_len[beg_idx] > MAX_ID_LEN) {
         PRINTMSG(TOKEN_LINE(token), 384, Internal, TOKEN_COLUMN(token),
                  beg_idx, kwd_mic_len[beg_idx]);
      }
# endif

      while (LA_CH_CLASS == Ch_Class_Letter && tok_len < kwd_mic_len[beg_idx]) {
	 la_queue[tok_len]		= la_ch;
         TOKEN_STR(token)[tok_len]	= LA_CH_VALUE;
         tok_len++;
	 NEXT_LA_CH;
      }

      TOKEN_LEN(token) = tok_len;

      if (tok_len >= kwd_mic_len[lim_idx-1]) {

	 /* compare token string to microtasking keyword entries */

	 while (beg_idx < lim_idx) {

	    if (kwd_mic_len[beg_idx] <= tok_len) {

	       if (strncmp(TOKEN_STR(token),
			   kwd_mic[beg_idx].name,
			   kwd_mic_len[beg_idx]) == IDENTICAL) {

		  /* the following chars and preceding letter can't be */
		  /* part of a keyword on full length match of string. */

		  if (tok_len == kwd_mic_len[beg_idx]  &&
		      (LA_CH_VALUE == USCORE  ||
		       LA_CH_VALUE == DOLLAR  ||
		       LA_CH_VALUE == AT_SIGN)) {
		  }
		  else {
		     TOKEN_VALUE(token) = kwd_mic[beg_idx].value;

		     /* adjust la_ch to be char following keyword */

		     if (tok_len > kwd_mic_len[beg_idx]) {
			tok_len = kwd_mic_len[beg_idx];
			la_ch	= la_queue[tok_len];
                        TOKEN_LEN(token) = tok_len;

			/* reset src input buffer and col index to la_ch pos */
			reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
		     }
		     break;
		  }
	       }
	    }  /* if */

	    beg_idx++;

	 }  /* while */
      }	 /* if */
   }  /* if */

   if (TOKEN_VALUE(token) == Tok_Id) {			/* keyword not found  */

      while (VALID_LA_CH) {
	 ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
	 NEXT_LA_CH;
      }
    
#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 characters. */
         PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
	 tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */
      TOKEN_LEN(token) = tok_len;
   }

   TRACE (Func_Exit, "get_micro_directive", NULL);

   return (TRUE);
   
}  /* get_micro_directive */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      is_par_directive is called by the classify_line routines to decide if *|
|*      a !$par prefix is followed by a directive or a comment. It must       *|
|*      remain similar in design to get_par_directive.                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE indicates a directive.                                           *|
|*      FALSE indicates that the line is a comment.                           *|
|*                                                                            *|
\******************************************************************************/

boolean is_par_directive (int	start_idx)

{
   int          beg_idx;
   int		blank = ' ';
   int		idx;
   boolean	is_directive = FALSE;
   int          letter_idx;
   int          lim_idx;
   int		newline = '\n';
   int		tab = '\t';
   char         upper_str[MAX_KWD_LEN + 1];
   int          str_len         = 0;


   TRACE (Func_Entry, "is_par_directive", NULL);

   idx = start_idx;

   while (nxt_line[idx] == blank || nxt_line[idx] == tab) {
      idx++;
   }

   if (ch_class[nxt_line[idx]] != Ch_Class_Letter) {
      goto EXIT;
   }
   else if (islower(nxt_line[idx])) {
      upper_str[str_len] = TOUPPER(nxt_line[idx]);
   }
   else {
      upper_str[str_len] = nxt_line[idx];
   }
   str_len++;
   idx++;

   letter_idx = upper_str[0] - 'A';

   beg_idx = kwd_sgi_dir_idx[letter_idx];
   lim_idx = kwd_sgi_dir_idx[letter_idx+1];

   if (beg_idx == lim_idx) {
      goto EXIT;
   }

   while (nxt_line[idx] != newline && str_len <= kwd_sgi_dir_len[beg_idx]) {
      if (nxt_line[idx] == blank || nxt_line[idx] == tab) {
         idx++;
      }
      else {
         if (ch_class[nxt_line[idx]] != Ch_Class_Letter &&
             nxt_line[idx] != USCORE) {
            break;
         }
         else if (islower(nxt_line[idx])) {
            upper_str[str_len] = TOUPPER(nxt_line[idx]);
         }
         else {
            upper_str[str_len] = nxt_line[idx];
         }
         str_len++;
         idx++;
      }
   }

   upper_str[str_len] = '\0';


   if (str_len >= kwd_sgi_dir_len[lim_idx-1]) {

      while (beg_idx < lim_idx) {

         if (kwd_sgi_dir_len[beg_idx] <= str_len) {

            if (strncmp(upper_str,
                        kwd_sgi_dir[beg_idx].name,
                        kwd_sgi_dir_len[beg_idx]) == IDENTICAL) {

               switch (kwd_sgi_dir[beg_idx].value) {
                  case Tok_SGI_Dir_Barrier:
                  case Tok_SGI_Dir_Criticalsection:
                  case Tok_SGI_Dir_Endcriticalsection:
                  case Tok_SGI_Dir_Endparallel:
                  case Tok_SGI_Dir_Endpdo:
                  case Tok_SGI_Dir_Endpsection:
                  case Tok_SGI_Dir_Endpsections:
                  case Tok_SGI_Dir_Endsingleprocess:
                  case Tok_SGI_Dir_Parallel:
                  case Tok_SGI_Dir_Paralleldo:
                  case Tok_SGI_Dir_Pdo:
                  case Tok_SGI_Dir_Psection:
                  case Tok_SGI_Dir_Psections:
                  case Tok_SGI_Dir_Section:
                  case Tok_SGI_Dir_Singleprocess:

                     is_directive = TRUE;
                     break;

                  default:
                     break;
               }
               break;
            }
         }  /* if */

         beg_idx++;

      }  /* while */
   }  /* if */

EXIT:

   TRACE (Func_Exit, "is_par_directive", NULL);

   return (is_directive);

}  /* is_par_directive */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      get_open_mp_directive is called by the get_token routine to attempt   *|
|*      recognition of a !$OMP        keyword by matching the look ahead char *|
|*      and following characters of class Ch_Class_Letter with entries in the *|
|*      kwd_mic table.  If a keyword is not found, an id token is created.    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      la_ch                   first character of !$  kwd token              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      la_ch                   next character of input source statement      *|
|*      token                   token created by get_open_mp_directive        *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE indicates a keyword or id token was produced.                    *|
|*      FALSE indicates that an error was encountered.                        *|
|*                                                                            *|
\******************************************************************************/

static boolean get_open_mp_directive (void)

{
   int          beg_idx;
   la_type      la_queue[MAX_KWD_LEN + 1];
   int          letter_idx;
   int          lim_idx;
   int          tok_len         = 0;


   TRACE (Func_Entry, "get_open_mp_directive", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_open_mp_directive", "letter");
   }
# endif

   TOKEN_VALUE(token) = Tok_Id;

   /* check for any keywords starting with look ahead char */
   letter_idx = LA_CH_VALUE - 'A';

   beg_idx = kwd_open_mp_dir_idx[letter_idx];
   lim_idx = kwd_open_mp_dir_idx[letter_idx+1];

   if (beg_idx != lim_idx) {

#ifdef _DEBUG
      if (kwd_open_mp_dir_len[beg_idx] > MAX_ID_LEN) {
         PRINTMSG(TOKEN_LINE(token), 384, Internal, TOKEN_COLUMN(token),
                  beg_idx, kwd_open_mp_dir_len[beg_idx]);
      }
# endif

      while ((LA_CH_CLASS == Ch_Class_Letter ||
              LA_CH_CLASS == Ch_Class_Digit  ||
              LA_CH_VALUE == USCORE) &&
             tok_len < kwd_open_mp_dir_len[beg_idx]) {
         la_queue[tok_len]              = la_ch;
         TOKEN_STR(token)[tok_len]      = LA_CH_VALUE;
         tok_len++;
         NEXT_LA_CH;
      }

      TOKEN_LEN(token) = tok_len;

      if (tok_len >= kwd_open_mp_dir_len[lim_idx-1]) {

         /* compare token string to keyword entries */

         while (beg_idx < lim_idx) {

            if (kwd_open_mp_dir_len[beg_idx] <= tok_len) {

               if (strncmp(TOKEN_STR(token),
                           kwd_open_mp_dir[beg_idx].name,
                           kwd_open_mp_dir_len[beg_idx]) == IDENTICAL) {

                  /* the following chars and preceding letter can't be */
                  /* part of a keyword on full length match of string. */

                  if (tok_len == kwd_open_mp_dir_len[beg_idx]  &&
                      (LA_CH_VALUE == USCORE  ||
                       LA_CH_VALUE == DOLLAR  ||
                       LA_CH_VALUE == AT_SIGN)) {
                  }
                  else {
                     TOKEN_VALUE(token) = kwd_open_mp_dir[beg_idx].value;

                     /* adjust la_ch to be char following keyword */

                     if (tok_len > kwd_open_mp_dir_len[beg_idx]) {
                        tok_len = kwd_open_mp_dir_len[beg_idx];
                        la_ch   = la_queue[tok_len];
                        TOKEN_LEN(token) = tok_len;

                        /* reset src input buffer and col index to la_ch pos */
                        reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
                     }
                     break;
                  }
               }
            }  /* if */

            beg_idx++;

         }  /* while */
      }  /* if */
   }  /* if */

   if (TOKEN_VALUE(token) == Tok_Id) {                  /* keyword not found  */

      while (VALID_LA_CH) {
         ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
         NEXT_LA_CH;
      }

#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 characters. */
         PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
         tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */
      TOKEN_LEN(token) = tok_len;
   }

   TRACE (Func_Exit, "get_open_mp_directive", NULL);

   return (TRUE);

}  /* get_open_mp_directive */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      get_sgi_directive is called by the get_token routine to attempt       *|
|*      recognition of a !$ (sgi)     keyword by matching the look ahead char *|
|*      and following characters of class Ch_Class_Letter with entries in the *|
|*      kwd_mic table.  If a keyword is not found, an id token is created.    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      la_ch                   first character of !$  kwd token              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      la_ch                   next character of input source statement      *|
|*      token                   token created by get_sgi_directive            *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE indicates a keyword or id token was produced.                    *|
|*      FALSE indicates that an error was encountered.                        *|
|*                                                                            *|
\******************************************************************************/

static boolean get_sgi_directive (void)

{
   int          beg_idx;
   la_type      la_queue[MAX_KWD_LEN + 1];
   int          letter_idx;
   int          lim_idx;
   int          tok_len         = 0;


   TRACE (Func_Entry, "get_sgi_directive", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_sgi_directive", "letter");
   }
# endif

   TOKEN_VALUE(token) = Tok_Id;

   /* check for any keywords starting with look ahead char */
   letter_idx = LA_CH_VALUE - 'A';

   beg_idx = kwd_sgi_dir_idx[letter_idx];
   lim_idx = kwd_sgi_dir_idx[letter_idx+1];

   if (beg_idx != lim_idx) {

#ifdef _DEBUG
      if (kwd_sgi_dir_len[beg_idx] > MAX_ID_LEN) {
         PRINTMSG(TOKEN_LINE(token), 384, Internal, TOKEN_COLUMN(token),
                  beg_idx, kwd_sgi_dir_len[beg_idx]);
      }
# endif

      while ((LA_CH_CLASS == Ch_Class_Letter || 
              LA_CH_CLASS == Ch_Class_Digit  ||
              LA_CH_VALUE == USCORE) &&
             tok_len < kwd_sgi_dir_len[beg_idx]) {
         la_queue[tok_len]              = la_ch;
         TOKEN_STR(token)[tok_len]      = LA_CH_VALUE;
         tok_len++;
         NEXT_LA_CH;
      }

      TOKEN_LEN(token) = tok_len;

      if (tok_len >= kwd_sgi_dir_len[lim_idx-1]) {

         /* compare token string to keyword entries */

         while (beg_idx < lim_idx) {

            if (kwd_sgi_dir_len[beg_idx] <= tok_len) {

               if (strncmp(TOKEN_STR(token),
                           kwd_sgi_dir[beg_idx].name,
                           kwd_sgi_dir_len[beg_idx]) == IDENTICAL) {

                  /* the following chars and preceding letter can't be */
                  /* part of a keyword on full length match of string. */

                  if (tok_len == kwd_sgi_dir_len[beg_idx]  &&
                      (LA_CH_VALUE == USCORE  ||
                       LA_CH_VALUE == DOLLAR  ||
                       LA_CH_VALUE == AT_SIGN)) {
                  }
                  else {
                     TOKEN_VALUE(token) = kwd_sgi_dir[beg_idx].value;

                     /* adjust la_ch to be char following keyword */

                     if (tok_len > kwd_sgi_dir_len[beg_idx]) {
                        tok_len = kwd_sgi_dir_len[beg_idx];
                        la_ch   = la_queue[tok_len];
                        TOKEN_LEN(token) = tok_len;

                        /* reset src input buffer and col index to la_ch pos */
                        reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
                     }
                     break;
                  }
               }
            }  /* if */

            beg_idx++;

         }  /* while */
      }  /* if */
   }  /* if */

   if (TOKEN_VALUE(token) == Tok_Id) {                  /* keyword not found  */

      while (VALID_LA_CH) {
         ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
         NEXT_LA_CH;
      }

#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 characters. */
         PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
         tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */
      TOKEN_LEN(token) = tok_len;
   }

   TRACE (Func_Exit, "get_sgi_directive", NULL);

   return (TRUE);

}  /* get_sgi_directive */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      is_dollar_directive is called by the classify_line routines to decide *|
|*      if a !$ prefix is followed by a directive or a comment. It must       *|
|*      remain similar in design to get_dollar_directive.                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE indicates a directive.                                           *|
|*      FALSE indicates that the line is a comment.                           *|
|*                                                                            *|
\******************************************************************************/

boolean is_dollar_directive (int	start_idx)

{
   int          beg_idx;
   int		blank = ' ';
   int		idx;
   boolean	is_directive = FALSE;
   int          letter_idx;
   int          lim_idx;
   int		newline = '\n';
   int		tab = '\t';
   char         upper_str[MAX_KWD_LEN + 1];
   int          str_len         = 0;


   TRACE (Func_Entry, "is_dollar_directive", NULL);

   idx = start_idx;

   while (nxt_line[idx] == blank || nxt_line[idx] == tab) {
      idx++;
   }

   if (ch_class[nxt_line[idx]] != Ch_Class_Letter) {
      goto EXIT;
   }
   else if (islower(nxt_line[idx])) {
      upper_str[str_len] = TOUPPER(nxt_line[idx]);
   }
   else {
      upper_str[str_len] = nxt_line[idx];
   }
   str_len++;
   idx++;

   letter_idx = upper_str[0] - 'A';

   beg_idx = kwd_sgi_dir_idx[letter_idx];
   lim_idx = kwd_sgi_dir_idx[letter_idx+1];

   if (beg_idx == lim_idx) {
      goto EXIT;
   }

   while (nxt_line[idx] != newline && str_len <= kwd_sgi_dir_len[beg_idx]) {
      if (nxt_line[idx] == blank || nxt_line[idx] == tab) {
         idx++;
      }
      else {
         if (ch_class[nxt_line[idx]] != Ch_Class_Letter &&
             nxt_line[idx] != USCORE) {
            break;
         }
         else if (islower(nxt_line[idx])) {
            upper_str[str_len] = TOUPPER(nxt_line[idx]);
         }
         else {
            upper_str[str_len] = nxt_line[idx];
         }
         str_len++;
         idx++;
      }
   }

   upper_str[str_len] = '\0';


   if (str_len >= kwd_sgi_dir_len[lim_idx-1]) {

      while (beg_idx < lim_idx) {

         if (kwd_sgi_dir_len[beg_idx] <= str_len) {

            if (strncmp(upper_str,
                        kwd_sgi_dir[beg_idx].name,
                        kwd_sgi_dir_len[beg_idx]) == IDENTICAL) {

               switch (kwd_sgi_dir[beg_idx].value) {
                  case Tok_SGI_Dir_Distribute:
                  case Tok_SGI_Dir_Distribute_Reshape:
                  case Tok_SGI_Dir_Doacross:
                  case Tok_SGI_Dir_Dynamic:
                  case Tok_SGI_Dir_Chunk:
                  case Tok_SGI_Dir_Mp_Schedtype:
                  case Tok_SGI_Dir_Page_Place:
                  case Tok_SGI_Dir_Redistribute:
                  case Tok_SGI_Dir_Copyin:

                     is_directive = TRUE;
                     break;

                  default:
                     break;
               }
               break;
            }
         }  /* if */

         beg_idx++;

      }  /* while */
   }  /* if */

EXIT:

   TRACE (Func_Exit, "is_dollar_directive", NULL);

   return (is_directive);

}  /* is_dollar_directive */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      is_star_directive is called by the classify_line routines to decide if*|
|*      a !*$* prefix is followed by a directive or a comment. It must        *|
|*      remain similar in design to get_star_directive.                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE indicates a directive.                                           *|
|*      FALSE indicates that the line is a comment.                           *|
|*                                                                            *|
\******************************************************************************/

boolean is_star_directive (int	start_idx)

{
   int          beg_idx;
   int		blank = ' ';
   int		idx;
   boolean	is_directive = FALSE;
   int          letter_idx;
   int          lim_idx;
   int		newline = '\n';
   int		tab = '\t';
   char         upper_str[MAX_KWD_LEN + 1];
   int          str_len         = 0;


   TRACE (Func_Entry, "is_star_directive", NULL);

   idx = start_idx;

   while (nxt_line[idx] == blank || nxt_line[idx] == tab) {
      idx++;
   }

   if (ch_class[nxt_line[idx]] != Ch_Class_Letter) {
      goto EXIT;
   }
   else if (islower(nxt_line[idx])) {
      upper_str[str_len] = TOUPPER(nxt_line[idx]);
   }
   else {
      upper_str[str_len] = nxt_line[idx];
   }
   str_len++;
   idx++;

   letter_idx = upper_str[0] - 'A';

   beg_idx = kwd_sgi_dir_idx[letter_idx];
   lim_idx = kwd_sgi_dir_idx[letter_idx+1];

   if (beg_idx == lim_idx) {
      goto EXIT;
   }

   while (nxt_line[idx] != newline && str_len <= kwd_sgi_dir_len[beg_idx]) {
      if (nxt_line[idx] == blank || nxt_line[idx] == tab) {
         idx++;
      }
      else {
         if (ch_class[nxt_line[idx]] != Ch_Class_Letter &&
             nxt_line[idx] != USCORE) {
            break;
         }
         else if (islower(nxt_line[idx])) {
            upper_str[str_len] = TOUPPER(nxt_line[idx]);
         }
         else {
            upper_str[str_len] = nxt_line[idx];
         }
         str_len++;
         idx++;
      }
   }

   upper_str[str_len] = '\0';


   if (str_len >= kwd_sgi_dir_len[lim_idx-1]) {

      while (beg_idx < lim_idx) {

         if (kwd_sgi_dir_len[beg_idx] <= str_len) {

            if (strncmp(upper_str,
                        kwd_sgi_dir[beg_idx].name,
                        kwd_sgi_dir_len[beg_idx]) == IDENTICAL) {

               switch (kwd_sgi_dir[beg_idx].value) {
                  case Tok_SGI_Dir_Align_Symbol:
                  case Tok_SGI_Dir_Aggressiveinner:
                  case Tok_SGI_Dir_Assert:
                  case Tok_SGI_Dir_Blockable:
                  case Tok_SGI_Dir_Blockingsize:
                  case Tok_SGI_Dir_Concurrentize:
                  case Tok_SGI_Dir_Fill_Symbol:
                  case Tok_SGI_Dir_Fission:
                  case Tok_SGI_Dir_Fissionable:
                  case Tok_SGI_Dir_Fusable:
                  case Tok_SGI_Dir_Flush:
                  case Tok_SGI_Dir_Fuse:
                  case Tok_SGI_Dir_Inline:
                  case Tok_SGI_Dir_Interchange:
                  case Tok_SGI_Dir_Ipa:
                  case Tok_SGI_Dir_Limit:
                  case Tok_SGI_Dir_Minconcurrent:
                  case Tok_SGI_Dir_Noblocking:
                  case Tok_SGI_Dir_Noconcurrentize:
                  case Tok_SGI_Dir_Nofission:
                  case Tok_SGI_Dir_Nofusion:
                  case Tok_SGI_Dir_Noinline:
                  case Tok_SGI_Dir_Nointerchange:
                  case Tok_SGI_Dir_Noipa:
                  case Tok_SGI_Dir_Opaque:
                  case Tok_SGI_Dir_Optional:
#ifdef KEY /* Bug 2660 */
                  case Tok_SGI_Dir_Options:
#endif /* KEY Bug 2660 */
                  case Tok_SGI_Dir_Prefetch:
                  case Tok_SGI_Dir_Prefetch_Manual:
                  case Tok_SGI_Dir_Prefetch_Ref:
                  case Tok_SGI_Dir_Prefetch_Ref_Disable:
                  case Tok_SGI_Dir_Regionbegin:
                  case Tok_SGI_Dir_Regionend:
                  case Tok_SGI_Dir_Section_Gp:
                  case Tok_SGI_Dir_Section_Non_Gp:
                  case Tok_SGI_Dir_Unroll:

                     is_directive = TRUE;
                     break;

                  default:
                     break;
               }
               break;
            }
         }  /* if */

         beg_idx++;

      }  /* while */
   }  /* if */

EXIT:

   TRACE (Func_Exit, "is_star_directive", NULL);

   return (is_directive);

}  /* is_star_directive */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_operand_digit is called by the get_token routine to attempt	      *|
|*	recognition of integer constants, real constants, boolean hollerith   *|
|*	constants of the form nH..., nL..., or nR..., boolean octal constants *|
|*	of the form ...B, and character literal constants of non-default kind *|
|*	by examining the look ahead character and following characters of the *|
|*	input source.							      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of operand token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token produced by get_operand_digit	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates an operand token was produced.			      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_operand_digit (void)

{
   char		delim;
   char		exponent	= BLANK;
   boolean      had_letter	= FALSE;
   boolean      had_zero	= FALSE;
   int		hollerith_len	= 0;
   int          i;
   boolean	result		= TRUE;
   la_type	save_ch;
   int		tok_len		= 0;

   TRACE (Func_Entry, "get_operand_digit", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Digit) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_operand_digit", "digit");
   }
# endif

   TOKEN_VALUE(token) = Tok_Const_Int;

   /* skip leading zeros */
   while (LA_CH_CLASS == Ch_Class_Digit &&
          !sig_blank                    &&
          LA_CH_VALUE == ZERO) {

      had_zero = TRUE;
      NEXT_LA_CH;
   }

   while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
      ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
      NEXT_LA_CH;
   }

   if (tok_len == 0 && had_zero) {
      ADD_TO_CONST_BUF (ZERO, tok_len);
   }

   /* check for optional decimal point or operator */

   if (LA_CH_VALUE == DOT && !sig_blank) {

      /* check for dot-op form operator */
      save_ch = la_ch;
	 
      NEXT_LA_CH;

      while (LA_CH_CLASS == Ch_Class_Letter) {
         had_letter = TRUE;
	 NEXT_LA_CH;
      }

      if (LA_CH_VALUE == DOT && had_letter) {
	
	 /* reset src input buffer and column index to la_ch pos */
         /* Have digits followed by a dot operator.              */

	 la_ch = save_ch;
	 reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
      }
      else {		/* have decimal point in real constant */
	 TOKEN_VALUE(token) = Tok_Const_Real;	     

	 /* reset src input buffer and column index to la_ch pos */
	 la_ch = save_ch;
	 reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
         sig_blank = FALSE;

	 /* add decimal point to constant */
	 ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
	 NEXT_LA_CH;

	 /* check for optional fractional digits of real constant */
	 while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
	    ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
	    NEXT_LA_CH;
	 }
      }
   }  /* if */

   /* check for optional exponent letter in real constant */

   if ((LA_CH_VALUE == 'D'  ||  LA_CH_VALUE == 'E' || LA_CH_VALUE == 'Q') && 
        !sig_blank) {

      switch (LA_CH_VALUE) {
      case 'D':
         TOKEN_VALUE(token) = Tok_Const_Dbl;
         break;

      case 'E':
         TOKEN_VALUE(token) = Tok_Const_Real;
         break;

      case 'Q':

# if defined(_QUAD_PRECISION)
         TOKEN_VALUE(token) = Tok_Const_Quad;
# else
         TOKEN_VALUE(token) = Tok_Const_Dbl;
         PRINTMSG(TOKEN_LINE(token), 1348, Caution,
                  TOKEN_COLUMN(token));
# endif
         break;

      }

      exponent = LA_CH_VALUE;

      ADD_TO_CONST_BUF ('E', tok_len);

      NEXT_LA_CH;

      /* check for optional sign in exponent */
      if ((LA_CH_VALUE == PLUS  ||  LA_CH_VALUE == MINUS) && !sig_blank) {
         ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
         NEXT_LA_CH;
      }

      /* check for required digits of exponent */

      if (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
         do {
            ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
            NEXT_LA_CH;
         }
         while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank);
      }
      else {            /* D, E or Q must be followed by an exponent */
         PRINTMSG (LA_CH_LINE, 1308, Error, LA_CH_COLUMN);
         result = FALSE;
      }

      /* check that kind-param doesn't follow real const with exponent 'D' */

      if (LA_CH_VALUE == USCORE  &&  
          (exponent == 'D' ||
           exponent == 'Q') && 
          !sig_blank) {

         /* Can't have a kind-param if it's a D or Q exponent */

         PRINTMSG (TOKEN_LINE(token), 1309, Error, TOKEN_COLUMN(token));
         result = FALSE;
      }
   }  /* if */

   TOKEN_LEN(token) = tok_len;
   const_buf[tok_len] = '\0';
 
   /* check for optional kind-param suffix on integer or real constant, or    */
   /* integer form of kind-param prefix on character literal.		      */

   if (LA_CH_VALUE == USCORE && !sig_blank) {
      NEXT_LA_CH;

      tok_len = 0;

      if (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
	 do {
	    ADD_TO_TOKEN_KIND_STR (LA_CH_VALUE, tok_len);
	    NEXT_LA_CH;
	 }
	 while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank);

#ifdef KEY /* Bug 3635 */
         if (kind_too_long(&tok_len)) {
	    result = FALSE;
	 }
#else
         if (tok_len > MAX_ID_LEN) {
            tok_len = MAX_ID_LEN;
            PRINTMSG(LA_CH_LINE, 101, Error, LA_CH_COLUMN);
            result = FALSE;
         }
#endif /* KEY Bug 3635 */
         TOKEN_KIND_LEN(token) = tok_len;
         TOKEN_KIND_STR(token)[tok_len] = EOS;
      }
      else if (LA_CH_CLASS == Ch_Class_Letter && !sig_blank) {
	 do {
	    ADD_TO_TOKEN_KIND_STR (LA_CH_VALUE, tok_len);
	    NEXT_LA_CH;
	 }
	 while (VALID_LA_CH); 

#ifdef KEY /* Bug 3635 */
         if (kind_too_long(&tok_len)) {
	    result = FALSE;
	 }
#else
         if (tok_len > MAX_ID_LEN) {
            tok_len = MAX_ID_LEN;
            PRINTMSG(LA_CH_LINE, 101, Error, LA_CH_COLUMN);
            result = FALSE;
         }
#endif /* KEY Bug 3635 */
         TOKEN_KIND_LEN(token) = tok_len;
         TOKEN_KIND_STR(token)[tok_len] = EOS;
      }
      else if ((LA_CH_VALUE == QUOTE  ||  LA_CH_VALUE == DBL_QUOTE) && 
               !sig_blank) {

	 /* check that kind-param is integer value */

	 if (TOKEN_VALUE(token) == Tok_Const_Real) {

	    /* Kind param on literal const must be integer or symbolic name */

            PRINTMSG (TOKEN_LINE(token), 89, Error, TOKEN_COLUMN(token));
            result = FALSE;
	 }
         else {
            for (i = 0; i <= TOKEN_LEN(token); i++) {
               TOKEN_KIND_STR(token)[i] = const_buf[i];
            }
            TOKEN_KIND_LEN(token) = TOKEN_LEN(token);
	 }

	 TOKEN_VALUE(token) = Tok_Const_Char;

         delim = LA_CH_VALUE;
         NEXT_LA_CH;

         result = convert_const() && result;

         TOKEN_CONST_TBL_IDX(token) = put_char_const_in_tbl ('\0', &tok_len);

         if (LA_CH_VALUE != delim){
	    PRINTMSG (TOKEN_LINE(token), 83, Error, TOKEN_COLUMN(token), delim);
            result = FALSE;
	 }   
         else {
            NEXT_LA_CH;
         }

         TOKEN_LEN(token) = tok_len;

         goto EXIT;
      }
      else { /* Kind param on literal const must be integer or symbolic name */
         PRINTMSG (LA_CH_LINE, 89, Error, LA_CH_COLUMN);
         result = FALSE;
      }	    
   }

   /* check for boolean octal suffix */
   else if (LA_CH_VALUE == 'B' && !sig_blank) {
      TOKEN_VALUE(token)       = Tok_Const_Boolean;
      TOKEN_KIND_STR(token)[0] = LA_CH_VALUE;
      TOKEN_KIND_STR(token)[1] = EOS;
      TOKEN_KIND_LEN(token)    = 1;

      /* Boolean constants are non-standard */

      PRINTMSG (TOKEN_LINE(token), 90, Ansi, TOKEN_COLUMN(token));

      if (tok_len > MAX_OCT_CONST_LEN) {
	 PRINTMSG(TOKEN_LINE(token), 91, Error, TOKEN_COLUMN(token),
                  tok_len, MAX_OCT_CONST_LEN);
      }
      else if (tok_len == MAX_OCT_CONST_LEN) {

	 if (const_buf[0] < '0'  ||  const_buf[0] > '1') {
            /* The value exceeds the range. */

	    PRINTMSG (TOKEN_LINE(token), 92, Error, TOKEN_COLUMN(token));
            result = FALSE;
	 }
      }	  

      /* validate that all digits of token are octal digits */

      tok_len = 0;

      while (IS_OCT_DIGIT(const_buf[tok_len])) {
	 tok_len++;
      }

      if (const_buf[tok_len] != EOS) { /* Invalid digit in octal const */
	 PRINTMSG(TOKEN_LINE(token), 93, Error, TOKEN_COLUMN(token),
                  const_buf[tok_len]);
         result = FALSE;
      }
      NEXT_LA_CH;

      if (result) {	/* convert const boolean */
         convert_octal_literal(FALSE);
      }
      else {
         TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
      }
   }

   /* check for hollerith prefix */

   else if ((LA_CH_VALUE == 'H'  ||  
             LA_CH_VALUE == 'L'  ||  
             LA_CH_VALUE == 'R')    && !sig_blank) {

      /* digit string is length of hollerith string */

      for (i = 0; i <= TOKEN_LEN(token); i++) {
         TOKEN_STR(token)[i] = const_buf[i];
      }

      hollerith_len	= atoi (TOKEN_STR(token));
      TOKEN_VALUE(token)= Tok_Const_Hollerith;

      /* Hollerith constant form is non-standard */

      PRINTMSG (TOKEN_LINE(token), 96, Ansi, TOKEN_COLUMN(token));

      if (hollerith_len > TARGET_CHARS_PER_WORD && LA_CH_VALUE == 'R') {

         /* Hollerith constant must be 8 (4 on sparc) */
         /* characters or less in "R" form            */

	 PRINTMSG (TOKEN_LINE(token), 94, Error, TOKEN_COLUMN(token),
                   TARGET_CHARS_PER_WORD);
         result = FALSE;
      }

      TOKEN_KIND_STR(token)[0] = LA_CH_VALUE;
      TOKEN_KIND_STR(token)[1] = EOS;
      TOKEN_KIND_LEN(token)    = 1;
 
      if (hollerith_len) {
	 NEXT_LA_CH_LITERAL;

         TOKEN_CONST_TBL_IDX(token) = 
                  put_char_const_in_tbl (TOKEN_KIND_STR(token)[0], &tok_len);

         TOKEN_LEN(token) = tok_len;
	  
	 if (tok_len < hollerith_len) {

	    /* Hollerith constant contains fewer characters than specified */

	    PRINTMSG(TOKEN_LINE(token), 84, Error, TOKEN_COLUMN(token),
                     hollerith_len, tok_len);
            result = FALSE;
	 }

# ifdef _TARGET_LITTLE_ENDIAN
         if (TOKEN_KIND_STR(token)[0] != 'R') {
            CN_HOLLERITH_ENDIAN(TOKEN_CONST_TBL_IDX(token)) = TRUE;
         }
# endif

         switch(TOKEN_KIND_STR(token)[0]) {
         case 'H':
            CN_HOLLERITH_TYPE(TOKEN_CONST_TBL_IDX(token)) = H_Hollerith;
            break;

         case 'L':
            CN_HOLLERITH_TYPE(TOKEN_CONST_TBL_IDX(token)) = L_Hollerith;
            break;

         case 'R':
            CN_HOLLERITH_TYPE(TOKEN_CONST_TBL_IDX(token)) = R_Hollerith;
            break;

         }

      }
      else {
	 TOKEN_STR(token)[0] = EOS;
	 TOKEN_LEN(token)    = 0;

	 /* Number of characters in hollerith specifier must be non-zero */

	 PRINTMSG (TOKEN_LINE(token), 85, Error, TOKEN_COLUMN(token));
         result = FALSE;
      }

      goto EXIT;
   }  /* else if */

   CHECK_FOR_FREE_BLANK;

   if (result) {
      switch (TOKEN_VALUE(token)) {
         case Tok_Const_Int  :
         case Tok_Const_Real :
            result = convert_const();
            break;
         case Tok_Const_Dbl  :

# ifdef _TARGET_OS_MAX
            if (! cmd_line_flags.s_default32 &&
                on_off_flags.enable_double_precision) {
               PRINTMSG(TOKEN_LINE(token), 1110, Warning, TOKEN_COLUMN(token));
               TOKEN_VALUE(token) = Tok_Const_Real;
               result = convert_const();
            }
            else {
               CONVERT_DBL_CONST(DOUBLE_PRECISION_TYPE_IDX, 
                                 TOKEN_LEN(token), result);
            }
# else
            CONVERT_DBL_CONST(DOUBLE_PRECISION_TYPE_IDX, 
                              TOKEN_LEN(token), result);
# endif
            break;
         case Tok_Const_Quad :
            CONVERT_REAL_CONST(Real_16,
                               TOKEN_LEN(token), result);
            break;
      }
   }
   else if (TOKEN_VALUE(token) == Tok_Const_Int  ||
            TOKEN_VALUE(token) == Tok_Const_Real ||
            TOKEN_VALUE(token) == Tok_Const_Quad ||
            TOKEN_VALUE(token) == Tok_Const_Dbl) {
     TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
   }

EXIT:

   TRACE (Func_Exit, "get_operand_digit", NULL);

   return (result);

}  /* get_operand_digit */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_operand_dot is called by the get_token routine to attempt	      *|
|*	recognition of real constants of the form .nnn, and logical constants *|
|*	that begin with "." by examining the look ahead                       *|
|*	character and following characters of the input source.		      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of operand token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token produced by get_operand_dot	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates an operand token was produced.			      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_operand_dot (void)

{
   int          attr_idx;
   char		exponent	= BLANK;
   int          name_idx;
   boolean	result		= TRUE;
   la_type	save_ch;
   int		tok_len		= 0;

 
   TRACE (Func_Entry, "get_operand_dot", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != DOT) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_operand_dot", ".");
   }
# endif

   /* check for logical constant or dot-op form operator */
   save_ch = la_ch;
       
   NEXT_LA_CH;

   while (LA_CH_CLASS == Ch_Class_Letter && !sig_blank) {
      ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
      NEXT_LA_CH;
   }

#ifdef KEY /* Bug 3635 */
   id_too_long(&tok_len);
#else
   if (tok_len > MAX_ID_LEN) {
      PRINTMSG(TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
      tok_len = MAX_ID_LEN;
   }
#endif /* KEY Bug 3635 */
   TOKEN_LEN(token) = tok_len;
 
   if (LA_CH_VALUE == DOT && !sig_blank) {

      if (EQUAL_STRS(TOKEN_STR(token), "TRUE")	 ||
	  EQUAL_STRS(TOKEN_STR(token), "FALSE")	 ||
	  EQUAL_STRS(TOKEN_STR(token), "T")	 ||
	  EQUAL_STRS(TOKEN_STR(token), "F")) {

	 TOKEN_VALUE(token) = (TOKEN_STR(token)[0] == 'T')  ?  Tok_Const_True  :
							       Tok_Const_False;
	 NEXT_LA_CH;

         if (tok_len == 1) {
            attr_idx = srch_sym_tbl(TOKEN_STR(token),
                                    TOKEN_LEN(token),
                                    &name_idx);

            if (attr_idx == NULL_IDX) {
               attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                            TOKEN_LEN(token), 
                                            &name_idx,
                                            TRUE);
            }

            if (attr_idx != NULL_IDX) {

               while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
                  attr_idx = AT_ATTR_LINK(attr_idx);
               }
            }

            /* Let NOT_VISIBLE items take the standard path */

            if (attr_idx != NULL_IDX && AT_OBJ_CLASS(attr_idx) == Interface) {

               /* .T. of .F. is a defined operator so the extension */
               /* is disallowed.                                    */

               reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
               TOKEN_VALUE(token) = Tok_Unknown;
               result = FALSE;
               goto EXIT;
            }
            else {
               PRINTMSG(TOKEN_LINE(token), 510, Ansi, TOKEN_COLUMN(token),
                        TOKEN_STR(token));
            }
         }

	 /* check for optional kind-param suffix on logical constant */
	 if (LA_CH_VALUE == USCORE) {
	    NEXT_LA_CH;
 
	    tok_len = 0;

	    if (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
	       do {				 
		  ADD_TO_TOKEN_KIND_STR (LA_CH_VALUE, tok_len);
		  NEXT_LA_CH;
	       }
	       while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank);
	    }	
	    else if (LA_CH_CLASS == Ch_Class_Letter && !sig_blank) {
	       do {
		  ADD_TO_TOKEN_KIND_STR (LA_CH_VALUE, tok_len);
		  NEXT_LA_CH;
	       }
	       while (VALID_LA_CH);
	    }
	    else {
	       /* Kind param on literal const must be integer or symbolic name*/
	       PRINTMSG (LA_CH_LINE, 89, Error, LA_CH_COLUMN);
               result = FALSE;
	    }
	    TOKEN_KIND_LEN(token) = tok_len;
	 }  /* if */

         result = convert_const() && result;

      }	 /* if */
      else { /* reset src input buffer and column index to la_ch pos */
         /* this is not an opnd .. let someone else deal with it */
	 la_ch = save_ch;
	 reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
         result = FALSE;
         sig_blank = FALSE;
         TOKEN_VALUE(token) = Tok_Unknown;
      }
   }  /* if */
   else {
      /* have decimal point in real constant */
      TOKEN_VALUE(token) = Tok_Const_Real;	  

      /* reset src input buffer and column index to la_ch pos */
      la_ch = save_ch;
      reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
      sig_blank = FALSE;
       
      /* add decimal point to constant */
      tok_len = 0;
      ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
      NEXT_LA_CH;

      /* check for required fractional digits of real constant */
      if (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
	 do {
	    ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
	    NEXT_LA_CH;
	 }
	 while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank);

         /* check for optional exponent letter in real constant */
         if ((LA_CH_VALUE == 'D'  ||  
              LA_CH_VALUE == 'E' || 
              LA_CH_VALUE == 'Q') &&
             !sig_blank) {

            exponent = LA_CH_VALUE;

            switch (LA_CH_VALUE) {
            case 'D':
               TOKEN_VALUE(token) = Tok_Const_Dbl;
               break;

            case 'E':
               TOKEN_VALUE(token) = Tok_Const_Real;
               break;

            case 'Q':
# if defined(_QUAD_PRECISION)
               TOKEN_VALUE(token) = Tok_Const_Quad;
# else
               TOKEN_VALUE(token) = Tok_Const_Dbl;
               PRINTMSG(TOKEN_LINE(token), 1348, Caution,
                        TOKEN_COLUMN(token));
# endif
               break;
            }

            ADD_TO_CONST_BUF ('E', tok_len);

            NEXT_LA_CH;

            /* check for optional sign in exponent */
            if ((LA_CH_VALUE == PLUS  ||  LA_CH_VALUE == MINUS) && !sig_blank) {
               ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
               NEXT_LA_CH;
            }

            /* check for required digits of exponent */
            if (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
               do {
                  ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
                  NEXT_LA_CH;
               }
               while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank);
            }
            else { /* Invalid exponent in real constant */
               PRINTMSG (LA_CH_LINE, 1308, Error, LA_CH_COLUMN);
               result = FALSE;
            }

      /* check that kind-param doesn't follow real const with exponent 'D' */

            if (LA_CH_VALUE == USCORE  &&  
                (exponent == 'D' ||
                 exponent == 'Q') && 
                !sig_blank) {

               /* Kind param is invalid on double precision real const form */
               PRINTMSG (TOKEN_LINE(token), 1309, Error, TOKEN_COLUMN(token));
               result = FALSE;
            }
         }

         TOKEN_LEN(token) = tok_len;
         const_buf[tok_len] = '\0';

	 /* check for optional kind-param suffix on real constant */

	 if (LA_CH_VALUE == USCORE && !sig_blank) {
	    NEXT_LA_CH;

	    tok_len = 0;

	    if (LA_CH_CLASS == Ch_Class_Digit && !sig_blank) {
	       do {
		  ADD_TO_TOKEN_KIND_STR (LA_CH_VALUE, tok_len);
		  NEXT_LA_CH;
	       }
	       while (LA_CH_CLASS == Ch_Class_Digit && !sig_blank); 
	    }
	    else if (LA_CH_CLASS == Ch_Class_Letter && !sig_blank) {
	       do {
		  ADD_TO_TOKEN_KIND_STR (LA_CH_VALUE, tok_len);
		  NEXT_LA_CH;
	       }
	       while (VALID_LA_CH);
	    }
	    else { /* Kind param on literal const must be int or symbolic name*/
	       PRINTMSG (LA_CH_LINE, 89, Error, LA_CH_COLUMN);
               result = FALSE;
	    }

#ifdef KEY /* Bug 3635 */
            kind_too_long(&tok_len);
#else
            if (tok_len > MAX_ID_LEN) {
               tok_len = MAX_ID_LEN;
               PRINTMSG(LA_CH_LINE, 101, Error, LA_CH_COLUMN);
            }
#endif /* KEY Bug 3635 */
            TOKEN_KIND_LEN(token) = tok_len;
            TOKEN_KIND_STR(token)[tok_len] = EOS;
	 }  /* if */
      }	 /* if */
      else {
	 /* Real constant must contain digits in whole or fractional part */
	 PRINTMSG (TOKEN_LINE(token), 95, Error, TOKEN_COLUMN(token));
         result = FALSE;

#ifdef KEY /* Bug 3635 */
         id_too_long(&tok_len);
#else
         if (tok_len > MAX_ID_LEN) {
            PRINTMSG(TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
            tok_len = MAX_ID_LEN;
         }
#endif /* KEY Bug 3635 */
         TOKEN_LEN(token) = tok_len;
      }
   }

   if (TOKEN_VALUE(token) == Tok_Const_Real ||
       TOKEN_VALUE(token) == Tok_Const_Dbl  ||
       TOKEN_VALUE(token) == Tok_Const_Quad) {

      CHECK_FOR_FREE_BLANK;

      if (result) {
         switch (TOKEN_VALUE(token)) {
            case Tok_Const_Real :
               result = convert_const();
               break;
            case Tok_Const_Dbl  :

# ifdef _TARGET_OS_MAX
               if (! cmd_line_flags.s_default32 &&
                   on_off_flags.enable_double_precision) {
                  PRINTMSG(TOKEN_LINE(token), 1110, Warning, 
                           TOKEN_COLUMN(token));
                  TOKEN_VALUE(token) = Tok_Const_Real;
                  result = convert_const();
               }
               else {
                  CONVERT_DBL_CONST(DOUBLE_PRECISION_TYPE_IDX,
                                    TOKEN_LEN(token), result);
               }
# else 
               CONVERT_DBL_CONST(DOUBLE_PRECISION_TYPE_IDX,
                                 TOKEN_LEN(token), result);
# endif
               break;
            case Tok_Const_Quad :
               CONVERT_REAL_CONST(Real_16,
                                  TOKEN_LEN(token), result);
               break;
         }
      }
      else {
          TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
      }
   }

EXIT:

   TRACE (Func_Exit, "get_operand_dot", NULL);

   return (result);

}  /* get_operand_dot */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_operand_letter is called by the get_token routine to attempt      *|
|*	recognition of identifiers, boolean octal, boolean hex, boolean	      *|
|*	hollerith, and non-default kind character literal constant forms by   *|
|*	examining the look ahead character and following characters of the    *|
|*	input source.							      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of operand token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token produced by get_operand_letter	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates an operand token was produced.			      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_operand_letter (void)

{
   char		delim;
   boolean      had_zero	= FALSE;
   char		prefix;
   boolean	result		= TRUE;
   int		tok_len		= 0;


   TRACE (Func_Entry, "get_operand_letter", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_operand_letter", "letter");
   }
# endif

   prefix = LA_CH_VALUE;				    /* remember 1st   */

   do {
      ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
      NEXT_LA_CH;
   }
   while (VALID_LA_CH); 

#ifdef KEY /* Bug 3635 */
   id_too_long(&tok_len);
#else
   if (tok_len > MAX_ID_LEN) {
      PRINTMSG(TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
      tok_len = MAX_ID_LEN;
   }
#endif /* KEY Bug 3635 */
   TOKEN_LEN(token) = tok_len;
 
   if ((LA_CH_VALUE == QUOTE  ||  LA_CH_VALUE == DBL_QUOTE) && !sig_blank) {
      if (tok_len == 1) {

	 if (prefix == 'B' || prefix == 'O' || prefix == 'Z' || prefix == 'X') {

	    if (prefix == 'X') { /* Boolean constants are non-standard */
	       PRINTMSG (TOKEN_LINE(token), 90, Ansi, TOKEN_COLUMN(token));
	       TOKEN_VALUE(token) = Tok_Const_Boolean;
	    }
	    else {
	       TOKEN_VALUE(token) = Tok_Const_Boz;

               if (stmt_type != Data_Stmt) {
                  PRINTMSG (TOKEN_LINE(token), 771, Ansi, TOKEN_COLUMN(token));
               }
	    }

	    strcpy (TOKEN_KIND_STR(token), TOKEN_STR(token));
	    TOKEN_KIND_LEN(token) = TOKEN_LEN(token);

	    delim = LA_CH_VALUE;

	    NEXT_LA_CH;
	    tok_len = 0;

            /* skip leading zeros */
            while (LA_CH_VALUE != delim && 
                   LA_CH_VALUE != EOS  &&
                   (LA_CH_VALUE == ZERO || 
                    LA_CH_VALUE == BLANK ||
                    LA_CH_VALUE == TAB)) {

               if (LA_CH_VALUE == ZERO) {
                  had_zero = TRUE;
               }
               NEXT_LA_CH;
            }

	    while (LA_CH_VALUE != delim	 &&  LA_CH_VALUE != EOS) {
               if (LA_CH_VALUE != BLANK && LA_CH_VALUE != TAB) {
	          ADD_TO_CONST_BUF (LA_CH_VALUE, tok_len);
               }
	       NEXT_LA_CH;
	    }

            if (tok_len == 0 && had_zero) {
               ADD_TO_CONST_BUF (ZERO, tok_len);
            }

            const_buf[tok_len] = '\0';
            TOKEN_LEN(token) = tok_len;

	    if (LA_CH_VALUE == EOS) { /* Token is missing trailing delimiter */
	       PRINTMSG(TOKEN_LINE(token), 83, Error,TOKEN_COLUMN(token),delim);
               result = FALSE;
	    }
	    else {
	       if (prefix == 'B') {     /* validate length of binary constant */

		  if (tok_len > MAX_BIN_CONST_LEN || tok_len == 0) { 

		     /* Binary constant length is invalid */

		     PRINTMSG(TOKEN_LINE(token), 91, Error,
                              TOKEN_COLUMN(token), tok_len, MAX_BIN_CONST_LEN);
                     result = FALSE;
		  } 

		  /* validate that all digits of token are binary digits */
		  tok_len = 0;

		  while (IS_BIN_DIGIT(const_buf[tok_len])) {
		     tok_len++;
		  }   

	          if (const_buf[tok_len] != EOS) { 
		     PRINTMSG (TOKEN_LINE(token), 422, Error, 
                               TOKEN_COLUMN(token), const_buf[tok_len]);
                     result = FALSE;
	          }
	 
                  if (result) {
                     convert_binary_literal(TRUE);
                  }
                  else {
                     TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
                  }
	       }
	       else if (prefix == 'O') {

		  /* validate length of octal constant */

		  if (tok_len > MAX_OCT_CONST_LEN || tok_len == 0) {

		     /* Octal constant length is invalid */

		     PRINTMSG(TOKEN_LINE(token), 91, Error,
                              TOKEN_COLUMN(token), tok_len, MAX_OCT_CONST_LEN);
                     result = FALSE;
		  }
		  else if (tok_len == MAX_OCT_CONST_LEN) {

		     if (const_buf[0] < '0'  || const_buf[0] > '1') {
			/* Octal constant value is out of range */

			PRINTMSG(TOKEN_LINE(token), 92, Error,
                                 TOKEN_COLUMN(token));
                        result = FALSE;
		     }
		  }	       

		  /* validate that all digits of token are octal digits */
		  tok_len = 0;

		  while (IS_OCT_DIGIT(const_buf[tok_len])) {
		     tok_len++;
		  }

	          if (const_buf[tok_len] != EOS) { 
	             PRINTMSG(TOKEN_LINE(token), 93, Error, TOKEN_COLUMN(token),
                              const_buf[tok_len]);
                     result = FALSE;
	          }

                  if (result) {
                     convert_octal_literal(TRUE);
                  }
                  else {
                     TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
                  }
	       }
	       else if (prefix == 'Z') {

		  /* validate length of hex constant */ 

		  if (tok_len > MAX_HEX_CONST_LEN || tok_len == 0) {	
		     PRINTMSG(TOKEN_LINE(token), 91, Error,
                              TOKEN_COLUMN(token), tok_len, MAX_HEX_CONST_LEN);
                     result = FALSE;
		  } 

		  /* validate that all digits of token are hex digits */ 
		  tok_len = 0; 

		  while (isxdigit(const_buf[tok_len])) { 
		     tok_len++; 
		  } 

	          if (const_buf[tok_len] != EOS) { 
		     PRINTMSG (TOKEN_LINE(token), 423, Error, 
                               TOKEN_COLUMN(token), const_buf[tok_len]);
                     result = FALSE;
	          }
 
                  if (result) {
                     convert_hex_literal(TRUE);
                  }
                  else {
                     TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
                  }
	       }
	       else {						/* prefix = X */

		  if (const_buf[0] == PLUS  || const_buf[0] == MINUS) {

		     /* validate length of sign plus hex constant */

		     if (--tok_len > MAX_HEX_CONST_LEN || tok_len == 0) {

			PRINTMSG(TOKEN_LINE(token), 91, Error,
                                 TOKEN_COLUMN(token), tok_len,
                                 MAX_HEX_CONST_LEN);
                        result = FALSE;
		     }
		     tok_len = 1;
		  }
		  else {

		     if (tok_len > MAX_HEX_CONST_LEN || tok_len == 0) {
			PRINTMSG(TOKEN_LINE(token), 91, Error,
                                 TOKEN_COLUMN(token), tok_len,
                                 MAX_HEX_CONST_LEN);
                        result = FALSE;
		     }
		     tok_len = 0;
		  }

		  /* validate that all digits of token are hex digits */

		  while (isxdigit(const_buf[tok_len])) {
		     tok_len++;
		  }

	          if (const_buf[tok_len] != EOS) { 
		     PRINTMSG (TOKEN_LINE(token), 423, Error, 
                               TOKEN_COLUMN(token), const_buf[tok_len]);
                     result = FALSE;
	          }

                  if (result) {
                     convert_hex_literal(FALSE);
                  }
                  else {
                     TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
                  }
	       }

	       NEXT_LA_CH;
	    }
	 }
	 else {
	    TOKEN_VALUE(token) = Tok_Id;
	 }
      }	 /* if */
      else if (TOKEN_STR(token)[tok_len-1] == USCORE) {		/* kind param */

	 TOKEN_VALUE(token) = Tok_Const_Char;
	 
#ifdef KEY /* Bug 3635 */
         tok_len -= 1;
	 if (id_too_long(&tok_len)) {
            result	= FALSE;
	 }
#else
	 if (--tok_len > MAX_ID_LEN) {
	    PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
            result	= FALSE;
	    tok_len	= MAX_ID_LEN;
	 }
#endif /* KEY Bug 3635 */
	 TOKEN_STR(token)[tok_len] = EOS;			/* remove '_' */

	 strcpy (TOKEN_KIND_STR(token), TOKEN_STR(token));
	 TOKEN_KIND_LEN(token) = TOKEN_LEN(token) - 1;

	 delim	 = LA_CH_VALUE;
         NEXT_LA_CH;

         result = convert_const() && result;

         TOKEN_CONST_TBL_IDX(token) = put_char_const_in_tbl ('\0', &tok_len);

         TOKEN_LEN(token) = tok_len;

         if (LA_CH_VALUE != delim) {
	    PRINTMSG (TOKEN_LINE(token), 83, Error, TOKEN_COLUMN(token), delim);
            result = FALSE;
         }
         else {
            NEXT_LA_CH;
         }
      }	 /* else if */
      else {
	 TOKEN_VALUE(token) = Tok_Id;

#ifdef KEY /* Bug 3635 */
         id_too_long(&tok_len);
#else
	 if (tok_len > MAX_ID_LEN) { /* Id len exceeds max of 31 characters */
	    PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
	    tok_len = MAX_ID_LEN;
	 }
#endif /* KEY Bug 3635 */
      }
   }  /* if */
   else {
      TOKEN_VALUE(token) = Tok_Id;

#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 characters */
	 PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
	 tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */
   }

   TRACE (Func_Exit, "get_operand_letter", NULL);

   return (result);

}  /* get_operand_letter */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_operand_quote is called by the get_token routine to attempt	      *|
|*	recognition of character literal constants of the form '...' or "...",*|
|*	and boolean hollerith constants of the form '...'H, "..."H, '...'L,   *|
|*	"..."L, '...'R, or "..."R by examining the look ahead character and   *|
|*	following characters of the input source.			      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of operand token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token produced by get_operand_quote	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates an operand token was produced.			      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_operand_quote (void)

{
   int		char_len;
   char        *chptr;
   char		delim;
   boolean      had_zero	= FALSE;
   int		i;
   boolean	result		= TRUE;
   int          shift;
   int		tok_len		= 0;


   TRACE (Func_Entry, "get_operand_quote", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != QUOTE && LA_CH_VALUE != DBL_QUOTE) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_operand_quote", "quote or double quote");
   }
# endif

   delim			= LA_CH_VALUE;
   NEXT_LA_CH;
   TOKEN_VALUE(token)		= Tok_Const_Char;

   /* assume 'H' hollerith for now */

   TOKEN_CONST_TBL_IDX(token)	= put_char_const_in_tbl ('H', &tok_len);

   if (LA_CH_VALUE != delim) {
      PRINTMSG (TOKEN_LINE(token), 83, Error, TOKEN_COLUMN(token), delim);
      result = FALSE;
   }
   else {
      NEXT_LA_CH;
   }

   TOKEN_LEN(token) = tok_len;

   /* check for hollerith suffix letters */
   if ((LA_CH_VALUE == 'H'  ||  LA_CH_VALUE == 'L'  ||  LA_CH_VALUE == 'R') &&
       !sig_blank)                                                          {

      if (LA_CH_VALUE == 'L') { /* replace trailing blanks with nulls */
         chptr = (char *)&CN_CONST(TOKEN_CONST_TBL_IDX(token));

         while (tok_len % TARGET_CHARS_PER_WORD != 0) {
            chptr[tok_len] = '\0';
            tok_len++;
         }
      }
      else if (LA_CH_VALUE == 'R') { /* shift const to be right justified */
         chptr = (char *)&CN_CONST(TOKEN_CONST_TBL_IDX(token));
         shift = (TARGET_CHARS_PER_WORD - (tok_len % TARGET_CHARS_PER_WORD)) % 
                                           TARGET_CHARS_PER_WORD;

         if (shift) {
            while (--tok_len >= 0) {
               chptr[tok_len + shift] = chptr[tok_len];
            }

            tok_len = shift;
            while (--tok_len >= 0) {
               chptr[tok_len] = '\0';
            }
         }
      }

      TOKEN_VALUE(token) = Tok_Const_Hollerith;

      /* Hollerith constant form is non-standard */

      PRINTMSG(TOKEN_LINE(token), 96, Ansi, TOKEN_COLUMN(token));

      if (TOKEN_LEN(token) > TARGET_CHARS_PER_WORD && LA_CH_VALUE == 'R') {

         /* Hollerith constant must be 8 (4 on sparc) */
         /* characters or less in "R" form            */

	 PRINTMSG(TOKEN_LINE(token), 94, Error, TOKEN_COLUMN(token),
                  TARGET_CHARS_PER_WORD);
         result = FALSE;
      }

      TOKEN_KIND_STR(token)[0] = LA_CH_VALUE;
      TOKEN_KIND_STR(token)[1] = EOS;
      TOKEN_KIND_LEN(token)    = 1;

# ifdef _TARGET_LITTLE_ENDIAN
      if (TOKEN_KIND_STR(token)[0] != 'R') {
         CN_HOLLERITH_ENDIAN(TOKEN_CONST_TBL_IDX(token)) = TRUE;
      }
# endif

      switch(TOKEN_KIND_STR(token)[0]) {
      case 'H':
         CN_HOLLERITH_TYPE(TOKEN_CONST_TBL_IDX(token)) = H_Hollerith;
         break;

      case 'L':
         CN_HOLLERITH_TYPE(TOKEN_CONST_TBL_IDX(token)) = L_Hollerith;
         break;

      case 'R':
         CN_HOLLERITH_TYPE(TOKEN_CONST_TBL_IDX(token)) = R_Hollerith;
         break;

      }

      NEXT_LA_CH;
   }
   else if (LA_CH_VALUE == 'X' && ! sig_blank) {

      NEXT_LA_CH;

      /* this is a hex constant */

      PRINTMSG (TOKEN_LINE(token), 90, Ansi, TOKEN_COLUMN(token));
      TOKEN_VALUE(token) = Tok_Const_Boolean;

      /* put the character constant into the const_buf */

      chptr = (char *)&CN_CONST(TOKEN_CONST_TBL_IDX(token));

      char_len = tok_len;

      i = 0;
      while (i < char_len &&
             (chptr[i] == ZERO ||
              chptr[i] == BLANK ||
              chptr[i] == TAB)) {

         if (chptr[i] == ZERO) {
            had_zero = TRUE;
         }

         i++;
      }

      tok_len = 0;

      while (i < char_len) {
         if (chptr[i] != BLANK &&
             chptr[i] != TAB) {
            ADD_TO_CONST_BUF (chptr[i], tok_len);
         }
         i++;
      }

      if (tok_len == 0 && had_zero) {
         ADD_TO_CONST_BUF (ZERO, tok_len);
      }

      const_buf[tok_len] = '\0';
      TOKEN_LEN(token) = tok_len;

      if (const_buf[0] == PLUS  || const_buf[0] == MINUS) {

         /* validate length of sign plus hex constant */

         if (--tok_len > MAX_HEX_CONST_LEN || tok_len == 0) {

            PRINTMSG(TOKEN_LINE(token), 91, Error,
                     TOKEN_COLUMN(token), tok_len,
                     MAX_HEX_CONST_LEN);
            result = FALSE;
         }
         tok_len = 1;
      }
      else {

         if (tok_len > MAX_HEX_CONST_LEN || tok_len == 0) {
            PRINTMSG(TOKEN_LINE(token), 91, Error,
                     TOKEN_COLUMN(token), tok_len,
                     MAX_HEX_CONST_LEN);
            result = FALSE;
         }
         tok_len = 0;
      }

      /* validate that all digits of token are hex digits */

      while (isxdigit(const_buf[tok_len])) {
         tok_len++;
      }

      if (const_buf[tok_len] != EOS) {
         PRINTMSG (TOKEN_LINE(token), 423, Error,
                   TOKEN_COLUMN(token), const_buf[tok_len]);
         result = FALSE;
      }

      if (result) {
         convert_hex_literal(FALSE);
      }
      else {
         TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
      }
   }
   else if (LA_CH_VALUE == 'O' && ! sig_blank) {

      NEXT_LA_CH;

      /* this is an octal constant */

      PRINTMSG (TOKEN_LINE(token), 90, Ansi, TOKEN_COLUMN(token));
      TOKEN_VALUE(token) = Tok_Const_Boolean;

      /* put the character constant into the const_buf */

      chptr = (char *)&CN_CONST(TOKEN_CONST_TBL_IDX(token));

      char_len = tok_len;

      i = 0;
      while (i < char_len &&
             (chptr[i] == ZERO ||
              chptr[i] == BLANK ||
              chptr[i] == TAB)) {

         if (chptr[i] == ZERO) {
            had_zero = TRUE;
         }

         i++;
      }

      tok_len = 0;

      while (i < char_len) {
         if (chptr[i] != BLANK &&
             chptr[i] != TAB) {
            ADD_TO_CONST_BUF (chptr[i], tok_len);
         }
         i++;
      }

      if (tok_len == 0 && had_zero) {
         ADD_TO_CONST_BUF (ZERO, tok_len);
      }

      const_buf[tok_len] = '\0';
      TOKEN_LEN(token) = tok_len;

      if (tok_len > MAX_OCT_CONST_LEN) {
         PRINTMSG(TOKEN_LINE(token), 91, Error, TOKEN_COLUMN(token),
                  tok_len, MAX_OCT_CONST_LEN);
      }
      else if (tok_len == MAX_OCT_CONST_LEN) {

         if (const_buf[0] < '0'  ||  const_buf[0] > '1') {
            /* The value exceeds the range. */

            PRINTMSG (TOKEN_LINE(token), 92, Error, TOKEN_COLUMN(token));
            result = FALSE;
         }
      }

      tok_len = 0;

      /* validate that all digits of token are octal digits */

      while (IS_OCT_DIGIT(const_buf[tok_len])) {
         tok_len++;
      }

      if (const_buf[tok_len] != EOS) {
         PRINTMSG (TOKEN_LINE(token), 93, Error,
                   TOKEN_COLUMN(token), const_buf[tok_len]);
         result = FALSE;
      }

      if (result) {
         convert_octal_literal(FALSE);
      }
      else {
         TOKEN_CONST_TBL_IDX(token) = NULL_IDX;
      }
   }
   else {
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)		= Character;
      TYP_LINEAR(TYP_WORK_IDX)		= CHARACTER_DEFAULT_TYPE;
      TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, tok_len);

      CN_TYPE_IDX(TOKEN_CONST_TBL_IDX(token))	= ntr_type_tbl();
   }

   TRACE (Func_Exit, "get_operand_quote", NULL);

   return (result);

}  /* get_operand_quote */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_operator is called by the get_token routine to attempt recognition*|
|*	of an operator token using the look ahead character and following     *|
|*	characters of the input source.					      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of operator token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_operator		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates an operator token was produced.			      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_operator (void)

{
   int		buf_idx;
   char		op_ch;
   boolean	result	= TRUE;
   int		stmt_num;
   

   TRACE (Func_Entry, "get_operator", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Symbol) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_operator", "symbol");
   }
# endif

   op_ch		= LA_CH_VALUE;
   buf_idx		= LA_CH_BUF_IDX;
   stmt_num		= LA_CH_STMT_NUM;
   TOKEN_STR(token)[0]	= op_ch;

   if (op_ch != PLUS     &&
       op_ch != MINUS    &&
       op_ch != STAR     &&
       op_ch != SLASH    &&
       op_ch != EQUAL    &&
       op_ch != GT       &&
       op_ch != LT       &&
       op_ch != DOT      &&
       op_ch != PERCENT) {

      result = FALSE;
      TOKEN_VALUE(token) = Tok_Unknown;
   }
   else {

      NEXT_LA_CH;				 /* op may be multi-character */

      switch (op_ch) {
         case PLUS :
	    TOKEN_VALUE(token) = Tok_Op_Add;
            TOKEN_LEN(token)   = 1;
	    break;
    
         case MINUS :
	    TOKEN_VALUE(token) = Tok_Op_Sub;
            TOKEN_LEN(token)   = 1;
	    break;
    
         case STAR :
	    if (LA_CH_VALUE == STAR && !sig_blank) {
	       TOKEN_VALUE(token)	= Tok_Op_Power;
               TOKEN_STR(token)[1]	= STAR;
               TOKEN_LEN(token)         = 2;
	       NEXT_LA_CH;
	    }
	    else {
	       TOKEN_VALUE(token) = Tok_Op_Mult;
               TOKEN_LEN(token)   = 1;
	    }
	    break;

         case SLASH :
	    if (LA_CH_VALUE == EQUAL && !sig_blank) {
	       TOKEN_VALUE(token)	= Tok_Op_Ne;
               TOKEN_STR(token)[1]	= EQUAL;
               TOKEN_LEN(token)         = 2;
	       NEXT_LA_CH;
	    }
	    else if (LA_CH_VALUE == SLASH && !sig_blank) {
	       TOKEN_VALUE(token)  = Tok_Op_Concat;
               TOKEN_STR(token)[1] = SLASH;
               TOKEN_LEN(token)    = 2;
	       NEXT_LA_CH;
	    }
            else if (LA_CH_VALUE == RPAREN && !sig_blank) {
               result = FALSE;
               reset_lex(buf_idx,stmt_num);
               TOKEN_VALUE(token) = Tok_Unknown;
            }
	    else {
	       TOKEN_VALUE(token) = Tok_Op_Div;
               TOKEN_LEN(token)   = 1;
	    }
	    break;
       
         case EQUAL :
	    if (LA_CH_VALUE == EQUAL && !sig_blank) {
	       TOKEN_VALUE(token)	= Tok_Op_Eq;
               TOKEN_STR(token)[1]	= EQUAL;
               TOKEN_LEN(token)         = 2;
	       NEXT_LA_CH;
	    }
	    else if (LA_CH_VALUE == GT && !sig_blank) {
	       TOKEN_VALUE(token)       = Tok_Op_Ptr_Assign;
               TOKEN_STR(token)[1]	= GT;
               TOKEN_LEN(token)         = 2;
	       NEXT_LA_CH;
	    }
	    else {
	       TOKEN_VALUE(token) = Tok_Op_Assign;
               TOKEN_LEN(token)   = 1;
	    }
	    break;
	       
         case GT :
	    if (LA_CH_VALUE == EQUAL && !sig_blank) {
	       TOKEN_VALUE(token)	= Tok_Op_Ge;
               TOKEN_STR(token)[1]	= EQUAL;
               TOKEN_LEN(token)         = 2;
	       NEXT_LA_CH;
	    }
	    else {
	       TOKEN_VALUE(token) = Tok_Op_Gt;
               TOKEN_LEN(token)   = 1;
	    }
	    break;
	       
         case LT :
	    if (LA_CH_VALUE == EQUAL && !sig_blank) {
	       TOKEN_VALUE(token)	= Tok_Op_Le;
               TOKEN_STR(token)[1]	= EQUAL;
               TOKEN_LEN(token)         = 2;
	       NEXT_LA_CH;
	    }
            else if (LA_CH_VALUE == GT &&
                     !sig_blank) {

               TOKEN_VALUE(token)       = Tok_Op_Lg;
               TOKEN_STR(token)[1]      = GT;
               TOKEN_LEN(token)         = 2;
               NEXT_LA_CH;
            }
	    else {
	       TOKEN_VALUE(token) = Tok_Op_Lt;
               TOKEN_LEN(token)   = 1;
	    }
	    break;
	       
         case DOT :
            
/*
            if (LA_CH_CLASS == Ch_Class_Digit) 
*/
            if (LA_CH_CLASS != Ch_Class_Letter ||
                sig_blank)                     {
               result = FALSE;
               reset_lex(buf_idx, stmt_num);
            }
            else {
	       result = get_operator_dot ();
            }
	    break;
    
         case PERCENT :
	    TOKEN_VALUE(token) = Tok_Op_Deref;
            TOKEN_LEN(token)   = 1;
	    break;
       
         default :
	    TOKEN_VALUE(token) = Tok_Unknown;
            result             = FALSE;
            TOKEN_LEN(token)   = 1;
	    break;
      }  /* switch */
   }

   TRACE (Func_Exit, "get_operator", NULL);

   return (result);

}  /* get_operator */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_operator_dot is called by the get_operator routine to recognize   *|
|*	relational operators, logical operators, and defined operators by     *|
|*	matching the look ahead character and following characters of class   *|
|*	Ch_Class_Letter with entries in the dot_op table.  The operator must  *|
|*	be delimited with a trailing "."				      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of operator token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token produced by get_operator_dot	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a dot operator token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_operator_dot (void)

{
   int          attr_idx;
   int		beg_idx;
   int		i;
   int		letter_idx;
   int		lim_idx;
   int          name_idx;
   boolean	result		= TRUE;
   int		tok_len		= 0;


   TRACE (Func_Entry, "get_operator_dot", NULL);

   /* This is only called if LA_CH_CLASS is Ch_Class_Letter */

   while (LA_CH_CLASS == Ch_Class_Letter && !sig_blank) {
      ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
      NEXT_LA_CH;
   }

#ifdef KEY /* Bug 3635 */
   if (defined_op_too_long(&tok_len))
#else
   if (tok_len > MAX_ID_LEN)
#endif /* KEY Bug 3635 */
   {
#ifdef KEY /* Bug 3635 */
#else
      /* Defined operator exceeds maximum length of 31 characters */
      PRINTMSG (LA_CH_LINE, 65, Error, LA_CH_COLUMN);
      tok_len	 	 = MAX_ID_LEN;
#endif /* KEY Bug 3635 */
      TOKEN_LEN(token)	 = tok_len;
      /* call this a user defined opr */
      TOKEN_VALUE(token) = Tok_Op_Defined;

      if (LA_CH_VALUE == DOT && !sig_blank) {
         NEXT_LA_CH;
      }
      else {
         PRINTMSG (LA_CH_LINE, 66, Error, LA_CH_COLUMN);
      }
   }
   else if (LA_CH_VALUE == DOT && !sig_blank) {

      /* check for any dot ops starting with first char of token */
      letter_idx = TOKEN_STR(token)[0] - 'A';

      beg_idx = dot_op_idx[letter_idx];
      lim_idx = dot_op_idx[letter_idx+1];
   
      /* compare token string to dot_op entries */
      while (beg_idx < lim_idx) {
	 if (dot_op_len[beg_idx] == tok_len) {
	    if (strncmp(TOKEN_STR(token),
			dot_op[beg_idx].name,
			tok_len) == IDENTICAL) {

	       TOKEN_VALUE(token) = dot_op[beg_idx].value;

	       break;
	    }
	 }
	 beg_idx++;
      }

      /* operator must be user defined */
      if (beg_idx == lim_idx) {
	 TOKEN_VALUE(token) = Tok_Op_Defined;
      }

      for (i = 0; i < tok_len; i++) {
         TOKEN_STR(token)[i] = tolower(TOKEN_STR(token)[i]);
      }

      switch (TOKEN_VALUE(token)) {
         case Tok_Op_Neqv     :

            if (tok_len == 3 || tok_len == 1) {
            
               attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                       &name_idx);

               if (attr_idx == NULL_IDX) {
                  attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                               TOKEN_LEN(token), 
                                               &name_idx,
                                               TRUE);
               }

               if (attr_idx != NULL_IDX) {

                  while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
                     attr_idx = AT_ATTR_LINK(attr_idx);
                  }
               }

               if (attr_idx == NULL_IDX && 
                   SH_STMT_TYPE(curr_stmt_sh_idx) == Interface_Stmt) {
                  TOKEN_VALUE(token) = Tok_Op_Defined;
               }
 
               else if (attr_idx != NULL_IDX && 
                        AT_OBJ_CLASS(attr_idx) == Interface) {
                  TOKEN_VALUE(token) = Tok_Op_Defined;
               }
               else {
                  PRINTMSG(TOKEN_LINE(token), 317, Ansi, TOKEN_COLUMN(token),
                           TOKEN_STR(token));
               }
            }
            break;
               
         case Tok_Const_True  :
         case Tok_Const_False :

            if (tok_len == 1) {

               attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                       &name_idx);

               if (attr_idx == NULL_IDX) {
                  attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                               TOKEN_LEN(token), 
                                               &name_idx,
                                               TRUE);
               }

               if (attr_idx != NULL_IDX) {

                  while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
                     attr_idx = AT_ATTR_LINK(attr_idx);
                  }
               }

               if (attr_idx                       == NULL_IDX && 
                   SH_STMT_TYPE(curr_stmt_sh_idx) == Interface_Stmt) {
                  TOKEN_VALUE(token) = Tok_Op_Defined;
               } 
               else if (attr_idx != NULL_IDX && 
                        AT_OBJ_CLASS(attr_idx) == Interface) {
                  TOKEN_VALUE(token) = Tok_Op_Defined;
               }
            }

            break;

         case Tok_Op_And      :
         case Tok_Op_Not      :
         case Tok_Op_Or       :

            if (tok_len == 1) {
            
               attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                       &name_idx);

               if (attr_idx == NULL_IDX) {
                  attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                               TOKEN_LEN(token), 
                                               &name_idx,
                                               TRUE);
               }

               if (attr_idx != NULL_IDX) {

                  while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
                     attr_idx = AT_ATTR_LINK(attr_idx);
                  }
               }

               if (attr_idx == NULL_IDX && 
                   SH_STMT_TYPE(curr_stmt_sh_idx) == Interface_Stmt) {
                  TOKEN_VALUE(token) = Tok_Op_Defined;
               }
               else if (attr_idx != NULL_IDX && 
                        AT_OBJ_CLASS(attr_idx) == Interface) {
                  TOKEN_VALUE(token) = Tok_Op_Defined;
               }
               else {
                  PRINTMSG(TOKEN_LINE(token), 317, Ansi, TOKEN_COLUMN(token),
                           TOKEN_STR(token));
               }
            }
            
            break;

      }
      NEXT_LA_CH;
   }
   else { /* Defined operator is missing "." delimiter */
      PRINTMSG (LA_CH_LINE, 66, Error, LA_CH_COLUMN);
      result = FALSE;
   }
   TOKEN_LEN(token) = tok_len;

   TRACE (Func_Exit, "get_operator_dot", NULL);

   return (result);

}  /* get_operator_dot */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_program_str is called by the get_token routine to attempt	      *|
|*	recognition of the optional string in a program statement by using    *|
|*	the look ahead character and following characters of the input source.*|
|*	This is a special case, the only thing cft90 does is to check to make *|
|*	sure there is only one (....) group.  It then gets to EOS.  Cft90     *|
|*	will do no more with this string, because it was used for CTSS.       *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			opening paren of program string		      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a program string token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_program_str (void)

{
   int		paren_lvl	= 0;
   boolean	result		= TRUE;


   TRACE (Func_Entry, "get_program_str", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != LPAREN) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_program_str", "(");
   }
# endif

   /* Scan thru the program string, checking if the paren groups are matched. */
   /* Do not save anything in the TOKEN.  Cray treats this as a comment.      */

   do {
      if (LA_CH_VALUE == LPAREN) {
	 paren_lvl++;
      }
      else if (LA_CH_VALUE == RPAREN) {
	 paren_lvl--;
      }
      NEXT_LA_CH;
   }
   while (paren_lvl > 0 && LA_CH_VALUE != EOS);

   if (paren_lvl > 0) {		/* Trailing ")" is missing in program string. */
      PRINTMSG (TOKEN_LINE(token), 28, Error, TOKEN_COLUMN(token));
      result = FALSE;
   }

   TRACE (Func_Exit, "get_program_str", NULL);

   return (result);

}  /* get_program_str */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_punctuator is called by the get_token routine to attempt	      *|
|*	recognition of a punctuator token using the look ahead character and  *|
|*	following characters of the input source.			      *|
|*									      *|
|*	EOS is considered a punctuator token.  Get_punctuator is responsible  *|
|*	for skipping redundant EOS punctuators between statements.	      *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of punctuator token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_punctuator		      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a punctuator token was produced.			      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_punctuator (void)

{
   char		punct_ch;
   int		tok_len         = 0; 

   TRACE (Func_Entry, "get_punctuator", NULL);

   punct_ch		= LA_CH_VALUE;
   TOKEN_STR(token)[0]	= punct_ch;
   tok_len		= 1;

   NEXT_LA_CH;			      /* punct may be multi-character */

   switch (punct_ch) {
      case COLON :
	 if (LA_CH_VALUE == COLON && !sig_blank) {
	    TOKEN_VALUE(token) = Tok_Punct_Colon_Colon;
            TOKEN_STR(token)[1] = COLON;
            tok_len = 2;
	    NEXT_LA_CH;
	 }
	 else {
	    TOKEN_VALUE(token) = Tok_Punct_Colon;
	 }
	 break;

      case COMMA :
	 TOKEN_VALUE(token) = Tok_Punct_Comma;
	 break;
    
      case DASH :
	 TOKEN_VALUE(token) = Tok_Punct_Dash;
	 break;
    
      case EOS :
	 TOKEN_VALUE(token) = Tok_EOS;
	 break;
    
      case EQUAL :
	 if (LA_CH_VALUE == GT && !sig_blank) {
	    TOKEN_VALUE(token)	= Tok_Punct_Rename;
            TOKEN_STR(token)[1]	= GT;
            tok_len		= 2;
	    NEXT_LA_CH;
	 }
	 else {
	    TOKEN_VALUE(token) = Tok_Punct_Eq;
	 }
	 break;

      case LPAREN :
	 if (LA_CH_VALUE == SLASH && !sig_blank) {
	    TOKEN_VALUE(token)	= Tok_Punct_Lbrkt;
            TOKEN_STR(token)[1]	= SLASH;
            tok_len		= 2;
	    NEXT_LA_CH;
	 }
	 else {
	    TOKEN_VALUE(token) = Tok_Punct_Lparen;
	 }
	 break;

      case RPAREN :
	 TOKEN_VALUE(token) = Tok_Punct_Rparen;
	 break;
    
      case SLASH :
	 if (LA_CH_VALUE == RPAREN && !sig_blank) {
	    TOKEN_VALUE(token)	= Tok_Punct_Rbrkt;
            TOKEN_STR(token)[1]	= RPAREN;
            tok_len		= 2;
	    NEXT_LA_CH;
	 }
	 else {
	    TOKEN_VALUE(token) = Tok_Punct_Slash;
	 }
	 break;

      case STAR :
	 TOKEN_VALUE(token) = Tok_Punct_Star;
	 break;
    
      default :
	 TOKEN_VALUE(token) = Tok_Unknown;
	 break;
   }  /* switch */

   TOKEN_LEN(token) = tok_len;

   TRACE (Func_Exit, "get_punctuator", NULL);

   return (TRUE);

}  /* get_punctuator */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	ch_after_paren_grp						      *|
|*	 initial implementation.  Does not handle hollerith.                  *|
|*	 Needs to be rewritten before it gets to the real world.              *|
|* 	 Parse needs to be positioned with LA_CH_VALUE = to LPAREN.           *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	If the lookahead is a left paren, it searches for the character       *|
|*      following the right paren and returns it.  If there is no right paren,*|
|*      it returns EOS char.  If the lookahead is not a left paren, it returns*|
|*      the lookahead.                                                        *|
|*									      *|
\******************************************************************************/

char	ch_after_paren_grp(void)

{
   char			return_char;


   TRACE (Func_Entry, "ch_after_paren_grp", &LA_CH_VALUE);

   return_char = scan_thru_close_paren(0,0,1);

   TRACE (Func_Exit, "ch_after_paren_grp", &return_char);

   return(return_char);

}  /* ch_after_paren_grp */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      convert numeric constants that possibly have kind parameters.         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if no problems.                                                  *|
|*                                                                            *|
\******************************************************************************/

static boolean convert_const(void)

{
   int			attr_idx;
   long     		bytes			= 0;
   long_type		constant[MAX_WORDS_FOR_NUMERIC];
   linear_type_type	linear_type;
   id_str_type  	name;
   int			name_idx;
   boolean		result_ok		= TRUE;
   int			type_idx;
   type_desc_type	type_kind		= Default_Typed;


   TRACE (Func_Entry, "convert_const", NULL);

   TOKEN_CONST_TBL_IDX(token) = NULL_IDX;

   if (TOKEN_KIND_LEN(token) != 0) {

      if (TOKEN_KIND_STR(token)[0] >= '0' && TOKEN_KIND_STR(token)[0] <= '9') {
         errno	= 0;
         bytes	= LEX_STRTOL(TOKEN_KIND_STR(token), (char **) NULL, 10);

         if (errno != 0) {
            result_ok	= FALSE;
            PRINTMSG(TOKEN_LINE(token), 621, Error, 
                     TOKEN_COLUMN(token),
                     TOKEN_KIND_STR(token));
         }
         else {
            type_kind	= Kind_Typed;
         }
      }
      else {
         CREATE_ID(name, TOKEN_KIND_STR(token), TOKEN_KIND_LEN(token));
         attr_idx	= srch_sym_tbl(name.string,
                                       TOKEN_KIND_LEN(token),
                                       &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx = srch_host_sym_tbl(name.string,
                                         TOKEN_KIND_LEN(token), 
                                         &name_idx,
                                         TRUE);
         }

         if (attr_idx == NULL_IDX) { /* error .. no parameter */
            PRINTMSG(TOKEN_LINE(token), 129, Error, TOKEN_COLUMN(token));
            result_ok	= FALSE;
         }
         else {

            while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
               attr_idx = AT_ATTR_LINK(attr_idx);
            }

            if (AT_NOT_VISIBLE(attr_idx)) {
               PRINTMSG(TOKEN_LINE(token), 486, Error,
                        TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(AT_MODULE_IDX((attr_idx))));
               result_ok	= FALSE;
            }
            else if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                     ATD_CLASS(attr_idx)    == Constant &&
                     TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Integer &&
                     ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
               bytes		= (long)CN_INT_TO_C(ATD_CONST_IDX(attr_idx));
               type_kind	= Kind_Typed;
            }
            else { /* error .. kind must be integer or integer parameter */
               PRINTMSG(TOKEN_LINE(token), 129, Error, TOKEN_COLUMN(token));
               result_ok	= FALSE;
            }
         }
      }
   }

   switch (TOKEN_VALUE(token)) {
   case Tok_Const_Int  :

      if (type_kind == Default_Typed) {
         type_idx	= INTEGER_DEFAULT_TYPE;
      }
      else if (!validate_kind(Integer,
                              TOKEN_LINE(token),
                              TOKEN_COLUMN(token),
                              &bytes,
	                      &linear_type)) {

         type_idx	= INTEGER_DEFAULT_TYPE;
         result_ok	= FALSE;
      }
      else {
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Integer;
         TYP_LINEAR(TYP_WORK_IDX)	= linear_type;
         TYP_DCL_VALUE(TYP_WORK_IDX)	= bytes;
         TYP_DESC(TYP_WORK_IDX)		= Kind_Typed;
         type_idx			= ntr_type_tbl();
      }

#ifdef KEY /* Bug 5554 */
      int new_type_idx;
      CONVERT_INT_CONST_AND_PROMOTE(type_idx, TOKEN_LEN(token), result_ok);
#else /* KEY Bug 5554 */
      CONVERT_INT_CONST(type_idx, TOKEN_LEN(token), result_ok);
#endif /* KEY Bug 5554 */
      break;

   case Tok_Const_Real :

      if (type_kind == Default_Typed) {
         type_idx	= REAL_DEFAULT_TYPE;
         CONVERT_REAL_CONST(type_idx, TOKEN_LEN(token), result_ok);
      }
      else if (!validate_kind(Real,
                              TOKEN_LINE(token),
                              TOKEN_COLUMN(token),
                              &bytes,
                              &linear_type)) {
         type_idx	= REAL_DEFAULT_TYPE;
         result_ok	= FALSE;
         CONVERT_REAL_CONST(type_idx, TOKEN_LEN(token), result_ok);
      }
      else {
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Real;
         TYP_LINEAR(TYP_WORK_IDX)	= linear_type;
         TYP_DCL_VALUE(TYP_WORK_IDX)	= bytes;
         TYP_DESC(TYP_WORK_IDX)		= Kind_Typed;
         type_idx			= ntr_type_tbl();

# ifdef _TARGET64
         if (linear_type > Real_8)
# else
         if (linear_type > Real_4)
# endif
         {
            CONVERT_DBL_CONST(type_idx, TOKEN_LEN(token), result_ok);
         }
         else {
            CONVERT_REAL_CONST(type_idx, TOKEN_LEN(token), result_ok);
         }
      }
      break;

   case Tok_Const_True  :
   case Tok_Const_False :

      if (type_kind == Default_Typed) {
         type_idx	= LOGICAL_DEFAULT_TYPE;
      }
      else if (!validate_kind(Logical,
                              TOKEN_LINE(token),
                              TOKEN_COLUMN(token),
                              &bytes,
                              &linear_type)) {
         type_idx	= LOGICAL_DEFAULT_TYPE;
         result_ok	= FALSE;
      }
      else {
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Logical;
         TYP_LINEAR(TYP_WORK_IDX)	= linear_type;
         TYP_DCL_VALUE(TYP_WORK_IDX)	= bytes;
         TYP_DESC(TYP_WORK_IDX)		= Kind_Typed;
         type_idx			= ntr_type_tbl();
      }

      TOKEN_CONST_TBL_IDX(token) = set_up_logical_constant(constant, 
                                                           type_idx, 
                   (TOKEN_VALUE(token) == Tok_Const_True ? TRUE_VALUE :
                                                           FALSE_VALUE),
                                                           TRUE);
      break;

   case Tok_Const_Char :

      if (type_kind != Default_Typed && !validate_kind(Character,
                                                       TOKEN_LINE(token),
                                                       TOKEN_COLUMN(token),
                                                       &bytes,
                                                       &linear_type)) {
         result_ok = FALSE;
      }
      break;
   }

   TRACE (Func_Exit, "convert_const", NULL);

   return(result_ok);

} /* convert_const */

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

token_values_type get_dir_token_from_str(char *str)

{
   int               beg_idx;
   int		     i;
   int               letter_idx;
   int               lim_idx;
   char              upper_str[MAX_KWD_LEN + 1];
   int               str_len         = 0;
   token_values_type value           = Tok_Unknown;


   TRACE (Func_Entry, "get_dir_token_from_str", NULL);

   str_len = 0;
   i = 0;

   while (str[i] != '\0') {
      if (str[i] == ' ' || str[i] == '\t') {
         i++;
      }
      else {
         if (ch_class[str[i]] != Ch_Class_Letter &&
             str[i] != USCORE) {
            goto EXIT;
         }
         else if (islower(str[i])) {
            upper_str[str_len] = TOUPPER(str[i]);
         }
         else {
            upper_str[str_len] = str[i];
         }
         str_len++;
         i++;
      }
   }

   if (ch_class[upper_str[0]] != Ch_Class_Letter) {
      goto EXIT;
   }

   letter_idx   = upper_str[0] - 'A';
   beg_idx      = kwd_dir_idx[letter_idx];
   lim_idx      = kwd_dir_idx[letter_idx+1];

   if (beg_idx != lim_idx) {

      if (str_len >= kwd_dir_len[lim_idx-1]) {

         while (beg_idx < lim_idx) {

            if (kwd_dir_len[beg_idx] == str_len &&
                strncmp(upper_str, 
                        kwd_dir[beg_idx].name,
                        kwd_dir_len[beg_idx]) == IDENTICAL) {

               value = kwd_dir[beg_idx].value;

               break;
            }

            beg_idx++;

         }  /* while */
      }  /* if */
   }  /* if */

   if (value                        == Tok_Unknown &&
       ((strncmp("ALL", upper_str, 3) == IDENTICAL) ||
        (strncmp("DIR", upper_str, 3) == IDENTICAL) ||
        (strncmp("MIC", upper_str, 3) == IDENTICAL) ||
        (strncmp("MIPSPRO", upper_str, 7) == IDENTICAL) ||
        (strncmp("OMP", upper_str, 3) == IDENTICAL) ||
        (strncmp("CONDITIONAL_OMP", upper_str, 15) == IDENTICAL) ||
        (strncmp("MPP", upper_str, 3) == IDENTICAL))) {

      /* Tok_Id is a signal to cmd_line that "all" or "mpp" was specified */

      value = Tok_Id;
   }
   else if (value == Tok_Unknown) {  /* See if it is a MIC keyword */
      letter_idx   = upper_str[0] - 'A';
      beg_idx      = kwd_mic_idx[letter_idx];
      lim_idx      = kwd_mic_idx[letter_idx+1];

      if (beg_idx != lim_idx) {

         if (str_len >= kwd_mic_len[lim_idx-1]) {

            while (beg_idx < lim_idx) {

               if (kwd_mic_len[beg_idx] == str_len &&
                   strncmp(upper_str, 
                           kwd_mic[beg_idx].name,
                           kwd_mic_len[beg_idx]) == IDENTICAL) {

                  value = kwd_mic[beg_idx].value;

                  break;
               }

               beg_idx++;

            }  /* while */
         }
      }
   }

   if (value == Tok_Unknown) {  /* See if it is an OpenMp keyword */
      letter_idx   = upper_str[0] - 'A';
      beg_idx      = kwd_open_mp_dir_idx[letter_idx];
      lim_idx      = kwd_open_mp_dir_idx[letter_idx+1];

      if (beg_idx != lim_idx) {

         if (str_len >= kwd_open_mp_dir_len[lim_idx-1]) {

            while (beg_idx < lim_idx) {

               if (kwd_open_mp_dir_len[beg_idx] == str_len &&
                   strncmp(upper_str, 
                           kwd_open_mp_dir[beg_idx].name,
                           kwd_open_mp_dir_len[beg_idx]) == IDENTICAL) {

                  value = kwd_open_mp_dir[beg_idx].value;

                  break;
               }

               beg_idx++;

            }  /* while */
         }
      }
   }

   if (value == Tok_Unknown) {  /* See if it is an Mips keyword */
      letter_idx   = upper_str[0] - 'A';
      beg_idx      = kwd_sgi_dir_idx[letter_idx];
      lim_idx      = kwd_sgi_dir_idx[letter_idx+1];

      if (beg_idx != lim_idx) {

         if (str_len >= kwd_sgi_dir_len[lim_idx-1]) {

            while (beg_idx < lim_idx) {

               if (kwd_sgi_dir_len[beg_idx] == str_len &&
                   strncmp(upper_str, 
                           kwd_sgi_dir[beg_idx].name,
                           kwd_sgi_dir_len[beg_idx]) == IDENTICAL) {

                  value = kwd_sgi_dir[beg_idx].value;

                  break;
               }

               beg_idx++;

            }  /* while */
         }
      }
   }

EXIT:

   TRACE (Func_Exit, "get_dir_token_from_str", NULL);

   return(value);

}  /* get_dir_token_from_str */

# ifdef _DEBUG
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_debug_directive is called by the get_token routine to attempt     *|
|*	recognition of a debug keyword by matching the look ahead char        *|
|*	and following characters of class Ch_Class_Letter with entries in the *|
|*	kwd_dbg table.	If a keyword is not found, an id token is created.    *|
|*									      *|
|* Input parameters:							      *|
|*	la_ch			first character of debug kwd token	      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next character of input source statement      *|
|*	token			token created by get_debug_directive	      *|
|*									      *|
|* Returns:								      *|
|*	TRUE indicates a keyword or id token was produced.		      *|
|*	FALSE indicates that an error was encountered.			      *|
|*									      *|
\******************************************************************************/

static boolean get_debug_directive (void)

{
   int		beg_idx;
   la_type	la_queue[MAX_KWD_LEN + 1];
   int		letter_idx;
   int		lim_idx;
   int		tok_len		= 0;
      

   TRACE (Func_Entry, "get_debug_directive", NULL);

# ifdef _DEBUG
   if (LA_CH_CLASS != Ch_Class_Letter) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "get_debug_directive", "letter");
   }
# endif

   TOKEN_VALUE(token) = Tok_Id;

   /* check for any microtasking keywords starting with look ahead char */
   letter_idx = LA_CH_VALUE - 'A';

   beg_idx = kwd_dbg_idx[letter_idx];
   lim_idx = kwd_dbg_idx[letter_idx+1];
   
   if (beg_idx != lim_idx) {

#ifdef _DEBUG
      if (kwd_dbg_len[beg_idx] > MAX_ID_LEN) {
         PRINTMSG(TOKEN_LINE(token), 384, Internal, TOKEN_COLUMN(token),
                  beg_idx, kwd_dbg_len[beg_idx]);
      }
# endif

      while (LA_CH_CLASS == Ch_Class_Letter && tok_len < kwd_dbg_len[beg_idx]) {
	 la_queue[tok_len]		= la_ch;
         TOKEN_STR(token)[tok_len]	= LA_CH_VALUE;
         tok_len++;
	 NEXT_LA_CH;
      }

      TOKEN_LEN(token) = tok_len;

      if (tok_len >= kwd_dbg_len[lim_idx-1]) {

	 /* compare token string to debug keyword entries */

	 while (beg_idx < lim_idx) {

	    if (kwd_dbg_len[beg_idx] <= tok_len) {

               if (strncmp(TOKEN_STR(token),
                           kwd_dbg[beg_idx].name,
                           kwd_dbg_len[beg_idx]) == IDENTICAL) {

		  /* the following chars and preceding letter can't be */
		  /* part of a keyword on full length match of string. */

		  if (tok_len == kwd_dbg_len[beg_idx]  &&
		      (LA_CH_VALUE == USCORE  ||
		       LA_CH_VALUE == DOLLAR  ||
		       LA_CH_VALUE == AT_SIGN)) {
		  }
		  else {
		     TOKEN_VALUE(token) = kwd_dbg[beg_idx].value;

		     /* adjust la_ch to be char following keyword */

		     if (tok_len > kwd_dbg_len[beg_idx]) {
			tok_len = kwd_dbg_len[beg_idx];
			la_ch	= la_queue[tok_len];
                        TOKEN_LEN(token) = tok_len;

			/* reset src input buffer and col index to la_ch pos */
			reset_src_input (LA_CH_BUF_IDX, LA_CH_STMT_NUM);
		     }
		     break;
		  }
	       }
	    }  /* if */

	    beg_idx++;

	 }  /* while */
      }	 /* if */
   }  /* if */

   if (TOKEN_VALUE(token) == Tok_Id) {			/* keyword not found  */

      while (VALID_LA_CH) {
	 ADD_TO_TOKEN_STR (LA_CH_VALUE, tok_len);
	 NEXT_LA_CH;
      }
    
#ifdef KEY /* Bug 3635 */
      id_too_long(&tok_len);
#else
      if (tok_len > MAX_ID_LEN) { /* Id len exceeds maximum of 31 characters. */
         PRINTMSG (TOKEN_LINE(token), 67, Error, TOKEN_COLUMN(token));
	 tok_len = MAX_ID_LEN;
      }
#endif /* KEY Bug 3635 */
      TOKEN_LEN(token) = tok_len;
   }

   TRACE (Func_Exit, "get_debug_directive", NULL);

   return (TRUE);
   
}  /* get_debug_directive */
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

static void convert_octal_literal(boolean	is_boz)

{
   int		i;
   int		idx;
   int		num_bits;
   int		num_words;
   long_type	result[MAX_WORDS_FOR_NUMERIC];
   int		shift;
   int		temp;
   int		type_idx;
   int		word;

   TRACE (Func_Entry, "convert_octal_literal", NULL);


   num_bits = ((TOKEN_LEN(token) - 1) * 3);
   temp = const_buf[0] - '0';

   num_bits +=
    ((temp & 4) != 0 ? 3 : ((temp & 2) != 0 ? 2 : ((temp & 1) != 0 ? 1 : 0)));

   num_words = (num_bits + TARGET_BITS_PER_WORD - 1) / TARGET_BITS_PER_WORD;

   if (num_words == 0) {
      num_words = 1;
   }

   /* clear result array */
   for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
      result[i] = 0;
   }

   word = num_words - 1;

   idx = TOKEN_LEN(token) - 1;
   shift = 0;

   while (idx >= 0) {

      if (shift > (TARGET_BITS_PER_WORD - 1)) {
         shift = 0;
         word--;
      }

      temp = const_buf[idx] - '0';
      idx--;

      result[word] |= ((temp & 1) << shift);
      shift++;

      if (shift > (TARGET_BITS_PER_WORD - 1)) {
         shift = 0;
         word--;
      }

      result[word] |= (((temp >> 1) & 1) << shift);
      shift++;

      if (shift > (TARGET_BITS_PER_WORD - 1)) {
         shift = 0;
         word--;
      }

      result[word] |= (((temp >> 2) & 1) << shift);  /* BRIANJ */
      shift++;
   }

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
   TYP_LINEAR(TYP_WORK_IDX)	= Short_Typeless_Const;
   TYP_BIT_LEN(TYP_WORK_IDX)	= (num_words * TARGET_BITS_PER_WORD);
   type_idx			= ntr_type_tbl();

   if (is_boz) {
      TOKEN_CONST_TBL_IDX(token) = ntr_boz_const_tbl(type_idx,
                                                     result);
   }
   else {
      TOKEN_CONST_TBL_IDX(token) = ntr_boolean_const_tbl(type_idx,
                                                         result);
   }


   TRACE (Func_Exit, "convert_octal_literal", NULL);

   return;

}  /* convert_octal_literal */

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

static void convert_hex_literal(boolean       is_boz)

{
   int		i;
   int          idx;
   int		base;
   int		bits;
   char		*char_ptr;
   long_type	constant[MAX_WORDS_FOR_NUMERIC];
   int		const_idx;
   int		count;
   int		digits_per_word;
   int		num_digits;
   int          num_words;
   boolean	negate = FALSE;
   long_type    result[MAX_WORDS_FOR_NUMERIC];
   char		tmpstr[80];
   int          type_idx;
   int          word;


   TRACE (Func_Entry, "convert_hex_literal", NULL);

   if (const_buf[0] == PLUS) {
      num_digits = TOKEN_LEN(token) - 1;
      char_ptr   = &(const_buf[1]);
   }
   else if (const_buf[0] == MINUS) {
      num_digits = TOKEN_LEN(token) - 1;
      char_ptr   = &(const_buf[1]);
      negate     = TRUE;
   }
   else {
      num_digits = TOKEN_LEN(token);
      char_ptr = const_buf;
   }

   digits_per_word = TARGET_BITS_PER_WORD / 4;

   num_words = (num_digits + digits_per_word - 1) / digits_per_word;

   /* clear result array */
   for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
      result[i] = 0;  /*  BRIANJ - SHould we set this to target value? */
   }

   word = num_words - 1;
   idx  = num_digits - digits_per_word;

   while (word >= 0) {

      if (idx < 0) {
         count = digits_per_word + idx;
         idx = 0;
      }
      else {
         count = digits_per_word;
      }

      strncpy(tmpstr, &(char_ptr[idx]), count);
      tmpstr[count] = '\0';

# ifdef _ARITH_INPUT_CONV
      base = 16;

      i    = AR_convert_str_to_int((AR_DATA *)constant,
                   (const AR_TYPE *)&input_arith_type[CG_INTEGER_DEFAULT_TYPE],
                                    &bits,
                      (const char *)tmpstr,
                       (const int *)&base);
      SHIFT_ARITH_RESULT(constant, CG_INTEGER_DEFAULT_TYPE);
      result[word] = constant[0];

# else
# if defined(_HOST32) && defined(_TARGET64)

      result[word] = (long_type) strtoull(tmpstr, (char **) NULL, 16);

# else 

      result[word] = strtoul(tmpstr, (char **) NULL, 16);

# endif
# endif

      idx -= digits_per_word;
      word--;
   }

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
   TYP_LINEAR(TYP_WORK_IDX)	= Short_Typeless_Const;
   TYP_BIT_LEN(TYP_WORK_IDX)	= (num_words * TARGET_BITS_PER_WORD);
   type_idx			= ntr_type_tbl();

   if (is_boz) {
      const_idx = ntr_boz_const_tbl(type_idx, result);
   }
   else {
      const_idx  = ntr_boolean_const_tbl(type_idx, result);
   }

   
   /* BHJ - need to check for '-' and do the fold. The constant is */
   /* truncated to default integer size.                           */

   if (negate) {
      const_idx = cast_typeless_constant(const_idx,
					 TYPELESS_DEFAULT_TYPE,
					 TOKEN_LINE(token),
					 TOKEN_COLUMN(token));

      type_idx = INTEGER_DEFAULT_TYPE;
      if (folder_driver((char *)&CN_CONST(const_idx),
			INTEGER_DEFAULT_TYPE,
                        NULL,
                        NULL_IDX,
                        constant,
                       &type_idx,
                        TOKEN_LINE(token),
                        TOKEN_COLUMN(token),
                        1,
                        Uminus_Opr)) {

         if (is_boz) {
            const_idx = ntr_boz_const_tbl(TYPELESS_DEFAULT_TYPE, constant);
         }
         else {
            const_idx  = ntr_boolean_const_tbl(TYPELESS_DEFAULT_TYPE, constant);
         }
      }
   }

   TOKEN_CONST_TBL_IDX(token) = const_idx;

   TRACE (Func_Exit, "convert_hex_literal", NULL);

   return;

}  /* convert_hex_literal */

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

static void convert_binary_literal(boolean       is_boz)

{
   int		i;
   int          idx;
   int          base;
   int          bits;
   long_type    constant[MAX_WORDS_FOR_NUMERIC];
   int		count;
   int          digits_per_word;
   int          num_digits;
   int          num_words;
   long_type    result[MAX_WORDS_FOR_NUMERIC];
   char         tmpstr[80];
   int          type_idx;
   int          word;


   TRACE (Func_Entry, "convert_binary_literal", NULL);

   num_digits = TOKEN_LEN(token);

   digits_per_word = TARGET_BITS_PER_WORD;

   num_words = (num_digits + digits_per_word - 1) / digits_per_word;

   /* clear result array */
   for (i = 0; i < MAX_WORDS_FOR_NUMERIC; i++) {
      result[i] = 0; /* BRIANJ - Do we need a target value */
   }

   word = num_words - 1;
   idx  = num_digits - digits_per_word;

   while (word >= 0) {

      if (idx < 0) {
         count = digits_per_word + idx;
         idx = 0;
      }
      else {
         count = digits_per_word;
      }

      strncpy(tmpstr, &(const_buf[idx]), count);
      tmpstr[count] = '\0';

# ifdef _ARITH_INPUT_CONV
      base = 2;

      i    = AR_convert_str_to_int((AR_DATA *)constant,
                   (const AR_TYPE *)&input_arith_type[CG_INTEGER_DEFAULT_TYPE],
                                    &bits,
                      (const char *)tmpstr,
                       (const int *)&base);
      SHIFT_ARITH_RESULT(constant, CG_INTEGER_DEFAULT_TYPE);
      result[word] = constant[0];

# else
# if defined(_HOST32) && defined(_TARGET64)

      result[word] = (long_type) strtoull(tmpstr, (char **) NULL, 2);

# else

      result[word] = strtoul(tmpstr, (char **) NULL, 2);

# endif
# endif

      idx -= digits_per_word;
      word--;
   }

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)	= Typeless;
   TYP_LINEAR(TYP_WORK_IDX)	= Short_Typeless_Const;
   TYP_BIT_LEN(TYP_WORK_IDX)	= (num_words * TARGET_BITS_PER_WORD);
   type_idx			= ntr_type_tbl();

   if (is_boz) {
      TOKEN_CONST_TBL_IDX(token) = ntr_boz_const_tbl(type_idx,
                                                     result);
   }
   else {
      TOKEN_CONST_TBL_IDX(token) = ntr_boolean_const_tbl(type_idx,
                                                         result);
   }


   TRACE (Func_Exit, "convert_binary_literal", NULL);

   return;

}  /* convert_binary_literal */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine takes a character string as input and, using the proper  *|
|*      input conversion routines, creates a fortran constant (a CN_Tbl_Idx)  *|
|*      It exists so that the frontend can create constants for values such   *|
|*      as HUGE without worrying about cross compile problems. The strings    *|
|*      must be proper 'c' numbers (like "1.7976931348623158e+308") and must  *|
|*      valid for the machine being targeted. Otherwise, an internal compiler *|
|*      error will be issued. At this time, ONLY INTEGER and REAL types are   *|
|*      supported.                                                            *|
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

int	cvrt_str_to_cn(char	*str,
		       int	type_idx)

{
   int		cn_idx;
   int		len;
   boolean	ok = TRUE;
   token_type	save_token;

   TRACE (Func_Entry, "cvrt_str_to_cn", NULL);

   save_token = token;

   strcpy(const_buf, str);
   len = strlen(str);

   switch (TYP_LINEAR(type_idx)) {
      case Integer_1 :
      case Integer_2 :
      case Integer_4 :
      case Integer_8 :
         CONVERT_INT_CONST(type_idx, len, ok);
         break;

      case Real_4 :
         CONVERT_REAL_CONST(type_idx, len, ok);
         break;

      case Real_8 :
# ifdef _TARGET64
         CONVERT_REAL_CONST(type_idx, len, ok);
# else
         CONVERT_DBL_CONST(type_idx, len, ok);
# endif
         break;

      case Real_16 :
         CONVERT_DBL_CONST(type_idx, len, ok);
         break;

      default :
         PRINTMSG(stmt_start_line, 1190, Internal, 0);
         break;
   }

   if (! ok) {
      PRINTMSG(stmt_start_line, 1190, Internal, 0);
   }

   cn_idx = TOKEN_CONST_TBL_IDX(token);

   token = save_token;

   TRACE (Func_Exit, "cvrt_str_to_cn", NULL);

   return(cn_idx);

}  /* cvrt_str_to_cn */

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

void set_up_token_tables(void)

{
   int		i;
   int		len;

   TRACE (Func_Entry, "set_up_token_tables", NULL);

   /****************\
   |* dot_op table *|
   \****************/

   len = 0;

   while (dot_op[len].value != Tok_LAST) {
      len++;
   }

   len++;

   dot_op_len = malloc(sizeof(int) * len);

   for (i = 0; i < len; i++) {
      dot_op_len[i] = strlen(dot_op[i].name);
   }

   set_up_letter_idx_table(dot_op_idx, dot_op, len);
   
   /*************\
   |* kwd table *|
   \*************/

   len = 0;

   while (kwd[len].value != Tok_LAST) {
      len++;
   }

   len++;

   kwd_len = malloc(sizeof(int) * len);

   for (i = 0; i < len; i++) {
      kwd_len[i] = strlen(kwd[i].name);
   }

   set_up_letter_idx_table(kwd_idx, kwd, len);

   /* we do not do the alt_kwd here */

   /*****************\
   |* kwd_dir table *|
   \*****************/

   len = 0;

   while (kwd_dir[len].value != Tok_LAST) {
      len++;
   }

   len++;

   kwd_dir_len = malloc(sizeof(int) * len);

   for (i = 0; i < len; i++) {
      kwd_dir_len[i] = strlen(kwd_dir[i].name);
   }

   set_up_letter_idx_table(kwd_dir_idx, kwd_dir, len);

   /*****************\
   |* kwd_mic table *|
   \*****************/

   len = 0;

   while (kwd_mic[len].value != Tok_LAST) {
      len++;
   }

   len++;

   kwd_mic_len = malloc(sizeof(int) * len);

   for (i = 0; i < len; i++) {
      kwd_mic_len[i] = strlen(kwd_mic[i].name);
   }

   set_up_letter_idx_table(kwd_mic_idx, kwd_mic, len);

   /*********************\
   |* kwd_sgi_dir table *|
   \*********************/

   len = 0;

   while (kwd_sgi_dir[len].value != Tok_LAST) {
      len++;
   }

   len++;

   kwd_sgi_dir_len = malloc(sizeof(int) * len);

   for (i = 0; i < len; i++) {
      kwd_sgi_dir_len[i] = strlen(kwd_sgi_dir[i].name);
   }

   set_up_letter_idx_table(kwd_sgi_dir_idx, kwd_sgi_dir, len);

   /*************************\
   |* kwd_open_mp_dir table *|
   \*************************/

   len = 0;

   while (kwd_open_mp_dir[len].value != Tok_LAST) {
      len++;
   }

   len++;

   kwd_open_mp_dir_len = malloc(sizeof(int) * len);

   for (i = 0; i < len; i++) {
      kwd_open_mp_dir_len[i] = strlen(kwd_open_mp_dir[i].name);
   }

   set_up_letter_idx_table(kwd_open_mp_dir_idx, kwd_open_mp_dir, len);

# ifdef _DEBUG
   /*****************\
   |* kwd_dbg table *|
   \*****************/

   len = 0;

   while (kwd_dbg[len].value != Tok_LAST) {
      len++;
   }

   len++;

   kwd_dbg_len = malloc(sizeof(int) * len);

   for (i = 0; i < len; i++) {
      kwd_dbg_len[i] = strlen(kwd_dbg[i].name);
   }

   set_up_letter_idx_table(kwd_dbg_idx, kwd_dbg, len);


# endif

   TRACE (Func_Exit, "set_up_token_tables", NULL);

   return;

}  /* set_up_token_tables */

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

static void set_up_letter_idx_table(int		*idx_tbl,
				    kwd_type	*kwd_tbl,
				    int		len)

{

   int	i;
   int	idx;
   int	k;

   TRACE (Func_Entry, "set_up_letter_idx_table", NULL);

   for (i = 0; i < 27; i++) {
      idx_tbl[i] = len - 1;
   }

   idx = -1;
   for (i = 0; i < len; i++) {
      if (kwd_tbl[i].name[0] - 'A' != idx) {
         for (k = idx+1; k <= kwd_tbl[i].name[0] - 'A'; k++) {
            idx_tbl[k] = i;
         }
         idx = kwd_tbl[i].name[0] - 'A';
      }
   }



   TRACE (Func_Exit, "set_up_letter_idx_table", NULL);

   return;

}  /* set_up_letter_idx_table */
