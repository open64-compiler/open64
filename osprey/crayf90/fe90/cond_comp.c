/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/cond_comp.c	5.8	09/21/99 00:21:13\n";

#ifndef KEY
# include <sys/time.h>
#else
# include <time.h>
#endif

# include "defines.h"           /* Machine dependent ifdefs */

# include "host.m"              /* Host machine dependent macros.*/
# include "host.h"              /* Host machine dependent header.*/
# include "target.m"            /* Target machine dependent macros.*/
# include "target.h"            /* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "src_input.m"
# include "cond_comp.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "cond_comp.h"


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static	void	cc_get_token(boolean);
static	void	cc_get_next_char(void);
static	void	cc_get_stmt(void);
static	boolean parse_cc_add_opnd (opnd_type *);
static	boolean parse_cc_level_2 (opnd_type *);
static	boolean parse_cc_level_3 (opnd_type *);
static	boolean parse_cc_level_4 (opnd_type *);
static	boolean parse_cc_level_5 (opnd_type *);
static	boolean parse_cc_level_6 (opnd_type *);
static	boolean parse_cc_level_7 (opnd_type *);
static	boolean parse_cc_level_8 (opnd_type *);
static	boolean parse_cc_mult_opnd (opnd_type *);
static	boolean parse_cc_or_opnd (opnd_type *);
static	boolean parse_cc_equiv_opnd (opnd_type *);
static	int	srch_cc_sym_tbl (char *,int, int *);
static	int	ntr_cc_sym_tbl(cc_token_type *, int);
static	boolean parse_cc_expr (opnd_type *);
static	boolean parse_cc_operand (opnd_type *);
static	void	flush_cc_line(void);
static	int	fold_cc_expr(opnd_type *);
static	void	parse_define_str(int);
static	void	copy_define_str(int, int);
static	void	scan_cc_line(void);
static	void	find_line_and_col(int, int *, int *);
static	void	shift_cc_stmt_buf(int, int);
static	void	shift_nxt_line(int, int);
static	void	scan_cc_macro(int, int);
static	boolean	scan_fortran_macro(int, int, int);
static	int	expanded_macro_len(int, cc_arg_type *);
static	void	insert_macro(int, cc_arg_type *, int, int, int);
static	void	cc_advance_idx(void);
static	void	adjust_continued_macro(int, int *);
static	char	*get_dynamic_predef_str(int, int);
static	void	free_arg_entry(cc_arg_type *);
static	cc_arg_type	*alloc_arg_entry(int);


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse conditional compilation statements.                             *|
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

boolean parse_cc_line(void)

{
   int			attr_idx;
   char			ch;
   int			col;
   char			delim;
   boolean		include_found = FALSE;
   int			line;
   int			name_idx;
   opnd_type		opnd;
   int			str_idx;
   char			str[MAX_SRC_LINE_SIZE];


   TRACE (Func_Entry, "parse_cc_line", NULL);

   cc_get_stmt();

   CC_NEXT_LA_CH;

   if (CC_LA_CH_VALUE != pound) { 
      find_line_and_col(cc_stmt_buf_idx, &line, &col);
      PRINTMSG(line, 1164, Internal, col, "parse_cc_line");
      flush_cc_line();
   }

   CC_NEXT_LA_CH;

   cc_get_token(TRUE);

   switch (TOKEN_VALUE(cc_token)) {

      case Cc_Tok_Kwd_Define :

         if (! ignore_source_line) {

            cc_get_token(FALSE);

            if (TOKEN_VALUE(cc_token) == Cc_Tok_Id) {

               attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                          TOKEN_LEN(cc_token),
                                          &name_idx);

               if (attr_idx != NULL_IDX) {

                  if (CC_AT_DEFINED(attr_idx)) {
                     /* issue message about redefinition */
   
			ntr_next_msg_queue(TOKEN_LINE(cc_token), 1670, Warning,
      				TOKEN_COLUMN(cc_token),
      				TOKEN_STR(cc_token),
      				0,
      				STR_ARG);
                  }
               }
               else {
                  attr_idx = ntr_cc_sym_tbl(&cc_token, name_idx);
               }

               parse_define_str(attr_idx);
            }
            else {
               ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                                  TOKEN_COLUMN(cc_token),
                                  "IDENTIFIER",
                                  0,
                                  STR_ARG);
               flush_cc_line();
            }
         }
         else {
            flush_cc_line();
         }
         break;

      case Cc_Tok_Kwd_Elif :

         if (CC_CURR_BLK_TYPE == Cc_If_Blk ||
             CC_CURR_BLK_TYPE == Cc_Ifdef_Blk ||
             CC_CURR_BLK_TYPE == Cc_Ifndef_Blk ||
             CC_CURR_BLK_TYPE == Cc_Elif_Blk) {

            /* blk ok, change curr block to else block */


            CC_CURR_BLK_TYPE = Cc_Elif_Blk;

            if (CC_CURR_BLK_IS_ACTIVE) {

               if (! CC_CURR_BLK_DONE) {
               
                  scan_cc_line();


                  if (CC_LA_CH_VALUE == EOS) {
                     ntr_next_msg_queue(CC_LA_CH_LINE, 1165, Error,
                                        CC_LA_CH_COLUMN,
                                        "expression",
                                        0,
                                        STR_ARG);
                  }
                  else if (parse_cc_expr(&opnd)) {

                     if (fold_cc_expr(&opnd)) {

                        ignore_source_line = FALSE;
                     }
                     else {
                        ignore_source_line = TRUE;
                     }

                     CC_CURR_BLK_DONE = (ignore_source_line == FALSE);
                  }
                  else {
                     flush_cc_line();
                  }
               }
               else {
                  ignore_source_line = TRUE;
                  flush_cc_line();
               }
            }
            else {
               flush_cc_line();
            }
         }
         else {
            /* blk_stk error */
            ntr_next_msg_queue(TOKEN_LINE(cc_token), 1166, Error,
                               TOKEN_COLUMN(cc_token),
                               "No matching IF, IFDEF, or IFNDEF directive",
                               0,
                               STR_ARG);
            flush_cc_line();
            CC_CURR_BLK_IN_ERROR = TRUE;
         }
   
         break;

      case Cc_Tok_Kwd_Else :

         if (CC_CURR_BLK_TYPE == Cc_If_Blk ||
             CC_CURR_BLK_TYPE == Cc_Ifdef_Blk ||
             CC_CURR_BLK_TYPE == Cc_Ifndef_Blk ||
             CC_CURR_BLK_TYPE == Cc_Elif_Blk) {

            /* blk ok, change curr block to else block */


            CC_CURR_BLK_TYPE = Cc_Else_Blk;

            if (CC_CURR_BLK_IS_ACTIVE) {

               if (! CC_CURR_BLK_DONE) {
                  ignore_source_line = FALSE;
                  CC_CURR_BLK_DONE = TRUE;
               }
               else {
                  ignore_source_line = TRUE;
               }
            }
         }
         else {
            /* blk error */
            ntr_next_msg_queue(TOKEN_LINE(cc_token), 1166, Error,
                               TOKEN_COLUMN(cc_token),
                               "No matching IF, IFDEF, or IFNDEF directive",
                               0,
                               STR_ARG);
            CC_CURR_BLK_IN_ERROR = TRUE;
         }

         flush_cc_line();
         break;

      case Cc_Tok_Kwd_Endif :

         if (CC_CURR_BLK_TYPE == Cc_If_Blk ||
             CC_CURR_BLK_TYPE == Cc_Ifdef_Blk ||
             CC_CURR_BLK_TYPE == Cc_Ifndef_Blk ||
             CC_CURR_BLK_TYPE == Cc_Elif_Blk   ||
             CC_CURR_BLK_TYPE == Cc_Else_Blk) {

            if (CC_CURR_BLK_IS_ACTIVE) {
               ignore_source_line = FALSE;
            }

            /* blk ok, pop off the block */

            POP_CC_BLK_STK;

         }
         else {
            /* blk error */
            ntr_next_msg_queue(TOKEN_LINE(cc_token), 1166, Error,
                               TOKEN_COLUMN(cc_token),
                               "No matching IF, IFDEF, or IFNDEF directive",
                               0,
                               STR_ARG);
            CC_CURR_BLK_IN_ERROR = TRUE;
         }

         flush_cc_line();
         break;

      case Cc_Tok_Kwd_If :

         PUSH_CC_BLK_STK(Cc_If_Blk);

         if (! ignore_source_line) {

            CC_CURR_BLK_IS_ACTIVE = TRUE;

            scan_cc_line();

            if (CC_LA_CH_VALUE == EOS) {
               ntr_next_msg_queue(CC_LA_CH_LINE, 1165, Error,
                                  CC_LA_CH_COLUMN,
                                  "expression",
                                  0,
                                  STR_ARG);
            }
            else if (parse_cc_expr(&opnd)) {

               if (fold_cc_expr(&opnd)) {

                  ignore_source_line = FALSE;
               }
               else {
                  ignore_source_line = TRUE;
               }

               CC_CURR_BLK_DONE = (ignore_source_line == FALSE);
            }
            else {
               flush_cc_line();
            }
         }
         else {
            flush_cc_line();
         }
         break;

      case Cc_Tok_Kwd_Ifdef :

         PUSH_CC_BLK_STK(Cc_Ifdef_Blk);

         if (! ignore_source_line) {

            CC_CURR_BLK_IS_ACTIVE = TRUE;

            cc_get_token(FALSE);
   
            if (TOKEN_VALUE(cc_token) == Cc_Tok_Id) {

               attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                          TOKEN_LEN(cc_token),
                                          &name_idx);

               if (attr_idx != NULL_IDX) {

                  if (CC_AT_DEFINED(attr_idx)) {
                     ignore_source_line = FALSE;
                  }
                  else {
                     ignore_source_line = TRUE;
                  }
               }
               else {
                  ignore_source_line = TRUE;
               }

               CC_CURR_BLK_DONE = (ignore_source_line == FALSE);
            }
            else {
               ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                                  TOKEN_COLUMN(cc_token),
                                  "IDENTIFIER",
                                  0,
                                  STR_ARG);
            }

            flush_cc_line();
         }
         else {
            flush_cc_line();
         }
         break;

      case Cc_Tok_Kwd_Ifndef :

         PUSH_CC_BLK_STK(Cc_Ifndef_Blk);

         if (! ignore_source_line) {

            CC_CURR_BLK_IS_ACTIVE = TRUE;

            cc_get_token(FALSE);

            if (TOKEN_VALUE(cc_token) == Cc_Tok_Id) {

               attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                          TOKEN_LEN(cc_token),
                                          &name_idx);

               if (attr_idx != NULL_IDX) {

                  if (CC_AT_DEFINED(attr_idx)) {
                     ignore_source_line = TRUE;
                  }
                  else {
                     ignore_source_line = FALSE;
                  }
               }
               else {
                  ignore_source_line = FALSE;
               }

               CC_CURR_BLK_DONE = (ignore_source_line == FALSE);
            }
            else {
               ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                                  TOKEN_COLUMN(cc_token),
                                  "IDENTIFIER",
                                  0,
                                  STR_ARG);
            }

            flush_cc_line();
         }
         else {
            flush_cc_line();
         }
         break;

      case Cc_Tok_Kwd_Include :

         if (! ignore_source_line) {

            if (CC_LA_CH_VALUE == quote ||
                CC_LA_CH_VALUE == db_quote ||
                CC_LA_CH_VALUE == '<') {

               if (CC_LA_CH_VALUE == '<') {
                  delim = '>';
                  angle_brkt_include = TRUE;
               }
               else {
                  delim = CC_LA_CH_VALUE;
               }

               str_idx = 0;

               while ((ch = cc_stmt_buf[++cc_stmt_buf_idx]) != delim &&
                      ch != eos) {

                  if (str_idx < MAX_FILE_NAME_SIZE) {
                     include_file[str_idx++] = ch;
                  }
                  else if (str_idx == MAX_FILE_NAME_SIZE) {

                     /* Include file name length exceeds maximum. */

                     find_line_and_col(cc_stmt_buf_idx, &line, &col);
                     ntr_next_msg_queue(line, 57, Error,
                                        col,
                                        (char *)NULL,
                                        (MAX_FILE_NAME_SIZE - 1),
                                        ARG_ARG);
                     flush_cc_line();
                     break;
                  }
               }

               include_file[str_idx] = '\0';

               if (str_idx == NULL_IDX) { /* Include file name missing. */
                  find_line_and_col(cc_stmt_buf_idx, &line, &col);
                  ntr_next_msg_queue(line, 58, Error,
                                     col,
                                     (char *)NULL,
                                     0,
                                     NO_ARG);
               }
               else if (ch == eos) { /* Missing delimiter on include file name*/
                  find_line_and_col(cc_stmt_buf_idx, &line, &col);
                  ntr_next_msg_queue(line, 59, Error,
                                     col,
                                     (char *)NULL,
                                     0,
                                     NO_ARG);
               }
               else {                           /* check for comments */
                  include_found = TRUE;
               }
            }
            else {
               find_line_and_col(cc_stmt_buf_idx, &line, &col);
               ntr_next_msg_queue(line, 1165, Error,
                                  col,
                                  "INCLUDE file name",
                                  0,
                                  STR_ARG);
            }
         }

         flush_cc_line();
         break;

      case Cc_Tok_Kwd_Undef :

         if (! ignore_source_line) {

            cc_get_token(FALSE);

            if (TOKEN_VALUE(cc_token) == Cc_Tok_Id) {

               attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                          TOKEN_LEN(cc_token),
                                          &name_idx);

               if (attr_idx != NULL_IDX) {
                  CC_AT_DEFINED(attr_idx) = FALSE;
               }
               else {
                  /* enter it in the symbol table for now BHJ */
                  attr_idx = ntr_cc_sym_tbl(&cc_token, name_idx);
               }
            }
            else {
               ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                                  TOKEN_COLUMN(cc_token),
                                  "IDENTIFIER",
                                  0,
                                  STR_ARG);
            }

            flush_cc_line();
         }
         else {
            flush_cc_line();
         }
         break;

      case Cc_Tok_Kwd_Error :

         if (! ignore_source_line) {

            str_idx = 0;

            while (cc_stmt_buf_idx < MAX_SRC_LINE_SIZE &&
                   (ch = cc_stmt_buf[cc_stmt_buf_idx]) != eos &&
                   ch != newline) {

               str[str_idx] = ch;
               str_idx++;
               cc_stmt_buf_idx++;
            }

            str[str_idx] = '\0';

            if (cc_line_continued) {
               /* error, can't continue error line */
               ntr_next_msg_queue(TOKEN_LINE(cc_token), 1178, Error,
                                  TOKEN_COLUMN(cc_token),
                                  (char *)NULL,
                                  0,
                                  NO_ARG);
            }

            ntr_next_msg_queue(TOKEN_LINE(cc_token), 1177, Error,
                               TOKEN_COLUMN(cc_token),
                               str,
                               0,
                               STR_ARG);

            flush_cc_line();
         }
         else {
            flush_cc_line();
         }
         break;

      case Cc_Tok_EOS :
         break;

      case Cc_Tok_Constant:
         /* this is a pound line directive coming in. Ftpp just ignored */
         /* these, so I will too.                                       */
         flush_cc_line();
         break;

      default:
         ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                            TOKEN_COLUMN(cc_token),
                            "conditional compilation directive",
                            0,
                            STR_ARG);
         flush_cc_line();
         break;
   }

   if (CC_LA_CH_VALUE != EOS) {
      ntr_next_msg_queue(CC_LA_CH_LINE, 1165, Error,
                         CC_LA_CH_COLUMN,
                         EOS_STR,
                         0,
                         STR_ARG);
      flush_cc_line();
   }

   TRACE (Func_Exit, "parse_cc_line", NULL);

   return(include_found);

}  /* parse_cc_line */

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

static void parse_define_str(int attr_idx)

{
   int		arg_num;
   int		id_start_idx;
   int		i;
   int		start_idx;
   cc_darg_type	*arg_head = NULL;
   cc_darg_type	*arg_tail = NULL;


   TRACE (Func_Entry, "parse_define_str", NULL);

   if (CC_LA_CH_VALUE == LPAREN &&
       cc_stmt_buf[cc_stmt_buf_idx-1] != blank &&
       cc_stmt_buf[cc_stmt_buf_idx-1] != tab &&
       cc_stmt_buf[cc_stmt_buf_idx-1] != '/') {
      /* macro with arguments. */

      CC_NEXT_LA_CH;
      CC_AT_NUM_ARGS(attr_idx) = 0;

      while (CC_LA_CH_VALUE != RPAREN) {
         cc_get_token(FALSE);

         if (TOKEN_VALUE(cc_token) == Cc_Tok_Id) {
            if (arg_head == NULL) {
               arg_head = (cc_darg_type *)malloc(sizeof(cc_darg_type));
               arg_head->next = NULL;
               arg_tail = arg_head;
            }
            else {
               arg_tail->next = (cc_darg_type *)malloc(sizeof(cc_darg_type));
               arg_tail = arg_tail->next;
               arg_tail->next = NULL;
            }

            for (i = 0; i < CC_NUM_ID_WDS; i++) {
               arg_tail->name.words[i] = TOKEN_STR_WD(cc_token, i);
            }
            arg_tail->name_len = TOKEN_LEN(cc_token);
            CC_AT_NUM_ARGS(attr_idx) += 1;
         }
         else {
            ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                               TOKEN_COLUMN(cc_token),
                               "IDENTIFIER",
                               0,
                               STR_ARG);
         }

         if (CC_LA_CH_VALUE == COMMA) {
            CC_NEXT_LA_CH;
         }
         else if (CC_LA_CH_VALUE != RPAREN) {
            ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                               TOKEN_COLUMN(cc_token),
                               ")",
                               0,
                               STR_ARG);
            flush_cc_line();
            goto EXIT;
         }
      }

      /* swallow RPAREN */
      CC_NEXT_LA_CH;

      start_idx = cc_stmt_buf_idx;

      if (CC_LA_CH_VALUE == EOS) {
         CC_AT_STR_LEN(attr_idx) = 0;
         CC_AT_DEFINED(attr_idx) = TRUE;
         goto EXIT;
      }

      while (CC_LA_CH_VALUE != EOS) {
         if (CC_LA_CH_CLASS == Ch_Class_Letter ||
             CC_LA_CH_VALUE == underscore) {

            id_start_idx = cc_stmt_buf_idx;
            cc_get_token(FALSE);

            if (TOKEN_VALUE(cc_token) == Cc_Tok_Id) {

               if (strcmp(TOKEN_STR(cc_token), CC_AT_NAME_PTR(attr_idx))
                        == 0) {
                  /* recursive macro definition */
                  ntr_next_msg_queue(TOKEN_LINE(cc_token), 1549, Warning,
                                     TOKEN_COLUMN(cc_token),
                                     TOKEN_STR(cc_token),
                                     0,
                                     STR_ARG);
                  flush_cc_line();
                  goto EXIT;
               }
               else {
                  arg_num = 1;
                  arg_tail = arg_head;
                  while (arg_tail != NULL) {

                     if (strcmp(TOKEN_STR(cc_token), arg_tail->name.string)
                                == 0) {

                        shift_cc_stmt_buf(id_start_idx+TOKEN_LEN(cc_token),
                                          3 - TOKEN_LEN(cc_token));

                        cc_stmt_buf[id_start_idx] = '\001';
                        cc_stmt_buf[id_start_idx+1] = TOKEN_LEN(cc_token);
                        cc_stmt_buf[id_start_idx+2] = arg_num;

                        cc_stmt_buf_idx = id_start_idx+2;
                        CC_NEXT_LA_CH;
                        break;
                     }
                     arg_tail = arg_tail->next;
                     arg_num++;
                  }
               }
            }
         }
         else {
            CC_NEXT_LA_CH;
         }
      }

      copy_define_str(attr_idx, start_idx);

      flush_cc_line();

   }
   else if (CC_LA_CH_VALUE == EOS) {
      CC_AT_NUM_ARGS(attr_idx) = 0;
      CC_AT_STR_LEN(attr_idx) = 0;

   }
   else {
      start_idx = cc_stmt_buf_idx;

      while (CC_LA_CH_VALUE != EOS) {
         if (CC_LA_CH_CLASS == Ch_Class_Letter ||
             CC_LA_CH_VALUE == underscore) {

            id_start_idx = cc_stmt_buf_idx;
            cc_get_token(FALSE);

            if (TOKEN_VALUE(cc_token) == Cc_Tok_Id) {

               if (strcmp(TOKEN_STR(cc_token), CC_AT_NAME_PTR(attr_idx))
                        == 0) {
                  /* recursive macro definition */
                  ntr_next_msg_queue(TOKEN_LINE(cc_token), 1549, Warning,
                                     TOKEN_COLUMN(cc_token),
                                     TOKEN_STR(cc_token),
                                     0,
                                     STR_ARG);
                  flush_cc_line();
                  goto EXIT;
               }
            }
         }
         else {
            CC_NEXT_LA_CH;
         }
      }

      CC_AT_NUM_ARGS(attr_idx) = 0;

      copy_define_str(attr_idx, start_idx);

      flush_cc_line();

   }

   CC_AT_DEFINED(attr_idx) = TRUE;

EXIT:

   while (arg_head != NULL) {
      arg_tail = arg_head->next;
      free(arg_head);
      arg_head = arg_tail;
   }

   TRACE (Func_Exit, "parse_define_str", NULL);

   return;

}  /* parse_define_str */

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

static void copy_define_str(int	attr_idx,
			    int	start_idx)

{
   char		*char_ptr;
   int		col;
   int		idx;
   int		len;
   int		line;
   int		wd_len;

   TRACE (Func_Entry, "copy_define_str", NULL);

   idx = start_idx;

   len = 0;

   while (cc_stmt_buf[idx] != newline) {
      if (cc_stmt_buf[idx] == '/' &&
          cc_stmt_buf[idx+1] == '*') {
         /* don't count comments */
         idx += 2;
         while (! (cc_stmt_buf[idx] == '*' &&
                   cc_stmt_buf[idx + 1] == '/') &&
                cc_stmt_buf[idx] != newline) {
            idx++;
         }

         if (cc_stmt_buf[idx] != newline) {
            idx++;
         }
      }
      else if (cc_stmt_buf[idx] == '\001') {
         /* macro argument insertion point */
         /* skip ahead 3 chars */
         idx += 2;
         len += 3;
      }
      else {
         len++;
      }
      idx++;
   }

   idx--;

   while (cc_stmt_buf[idx] == blank ||
          cc_stmt_buf[idx] == tab || 
          (cc_stmt_buf[idx] == '/' &&
           cc_stmt_buf[idx-1] == '*')) {

      if (cc_stmt_buf[idx] == '/' &&
          cc_stmt_buf[idx-1] == '*') {

         idx -= 2;
         while (! (cc_stmt_buf[idx] == '*' &&
                   cc_stmt_buf[idx - 1] == '/')) {
            idx--;
         }
         idx--;
      }
      else {
         len--;
      }

      idx--;
   }

   find_line_and_col(start_idx, &line, &col);
   CC_AT_START_LINE(attr_idx) = line;
   CC_AT_START_COL(attr_idx) = col;

   if (len > 512) {
      ntr_next_msg_queue(line, 1630, Error, col,
                         CC_AT_NAME_PTR(attr_idx),
                         512,
                         STR_ARG_ARG);
   }

   CC_AT_STR_LEN(attr_idx) = len;

   wd_len = WORD_LEN(len);
   CC_AT_STR_IDX(attr_idx) = str_pool_idx + 1;
   TBL_REALLOC_CK(str_pool,wd_len);

   str_pool[CC_AT_STR_IDX(attr_idx) + wd_len - 1].name_long = 0;

   char_ptr = CC_AT_STR_PTR(attr_idx);

   while (len > 0) {
      if (cc_stmt_buf[start_idx] == '/' &&
          cc_stmt_buf[start_idx+1] == '*') {
         /* comment */

         start_idx += 2;
         while (! (cc_stmt_buf[start_idx] == '*' &&
                   cc_stmt_buf[start_idx + 1] == '/') &&
                cc_stmt_buf[start_idx] != newline) {
            start_idx++;
         }

         if (cc_stmt_buf[start_idx] != newline) {
            start_idx++;
         }
      }
      else {
         *char_ptr = cc_stmt_buf[start_idx];
         char_ptr++;
         len--;
      }
      start_idx++;
   }

   TRACE (Func_Exit, "copy_define_str", NULL);

   return;

}  /* copy_define_str */

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

static void scan_cc_line(void)

{
   int		attr_idx;
   char		*char_ptr;
   int		col;
   int		i;
   int		id_start_idx;
   int		len;
   int		line;
   int		name_idx;
   int		paren_cnt = 0;
   int		save_cc_stmt_buf_idx;

   TRACE (Func_Entry, "scan_cc_line", NULL);

   save_cc_stmt_buf_idx = cc_stmt_buf_idx;

   while (CC_LA_CH_VALUE != EOS) {
      if (CC_LA_CH_CLASS == Ch_Class_Letter ||
          CC_LA_CH_VALUE == underscore) {

         id_start_idx = cc_stmt_buf_idx;
         cc_get_token(TRUE);

         switch (TOKEN_VALUE(cc_token)) {

         case Cc_Tok_Id :
         case Cc_Tok_Kwd_Define :
         case Cc_Tok_Kwd_Elif :
         case Cc_Tok_Kwd_Else :
         case Cc_Tok_Kwd_Endif :
         case Cc_Tok_Kwd_If :
         case Cc_Tok_Kwd_Ifdef :
         case Cc_Tok_Kwd_Ifndef :
         case Cc_Tok_Kwd_Include :
         case Cc_Tok_Kwd_Undef :

            attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                       TOKEN_LEN(cc_token),
                                       &name_idx);

            if (attr_idx != NULL_IDX &&
                CC_AT_DEFINED(attr_idx)) {

               if (CC_AT_NUM_ARGS(attr_idx) == 0) {

                  if (CC_AT_DYNAMIC_PREDEF(attr_idx)) {
                     find_line_and_col(id_start_idx, &line, &col);
                     char_ptr = get_dynamic_predef_str(attr_idx, line);
                     len = strlen(char_ptr);
                  }
                  else {
                     char_ptr = CC_AT_STR_PTR(attr_idx);
                     len = CC_AT_STR_LEN(attr_idx);
                  }

                  shift_cc_stmt_buf(id_start_idx+TOKEN_LEN(cc_token),
                                    len - TOKEN_LEN(cc_token));


                  for (i = 0; i < len; i++) {
                     cc_stmt_buf[i+id_start_idx] = char_ptr[i];
                  }

                  cc_stmt_buf_idx = id_start_idx - 1;
                  CC_NEXT_LA_CH;
               }
               else {

                  scan_cc_macro(attr_idx, id_start_idx);
               }
            }
            break;

         case Cc_Tok_Kwd_Defined :
            /* just parse through this, it is evaluated later */

            while (CC_LA_CH_VALUE == '(') {
               paren_cnt++;
               CC_NEXT_LA_CH;
            }

            cc_get_token(FALSE);

            while (paren_cnt) {
               if (CC_LA_CH_VALUE != ')') {
                  flush_cc_line();
                  break;
               }
               else {
                  CC_NEXT_LA_CH;
                  paren_cnt--;
               }
            }
            break;
         }
      }
      else {
         CC_NEXT_LA_CH;
      }
   }

   cc_stmt_buf_idx = save_cc_stmt_buf_idx - 1;
   CC_NEXT_LA_CH;

   TRACE (Func_Exit, "scan_cc_line", NULL);

   return;

}  /* scan_cc_line */

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

static void cc_advance_idx(void)

{
   int		i;
   int		save_pp_line_idx;

   TRACE (Func_Entry, "cc_advance_idx", NULL);

   save_pp_line_idx = pp_line_idx;
   prev_idx = cc_stmt_buf_idx;

   cc_stmt_buf_idx++;

   while (PP_LINE_TYPE == Comment_Line ||
          cc_stmt_buf_idx == PP_EOL) {

      /* advance to next line */

      pp_line_idx++;

      if (pp_line_idx > end_stmt_line_idx ||
          (PP_LINE_TYPE != Continuation_Line &&
           PP_LINE_TYPE != Dir_Continuation_Line)) {

         cc_stmt_buf_idx = nxt_line_end_idx[save_pp_line_idx];
         break;
      }

      if (source_form == Fixed_Form &&
          PP_LINE_TYPE == Continuation_Line) {
         cc_stmt_buf_idx = NULL_IDX;

         for (i = NXT_COL(1); i < PP_IDX; i++) {
            if (VALID_CC_ID_CHAR(nxt_line[i])) {
               cc_stmt_buf_idx = i;
               break;
            }
         }

         if (cc_stmt_buf_idx == NULL_IDX) {
            cc_stmt_buf_idx = PP_IDX + 1;
         }
      }
      else {
         cc_stmt_buf_idx = PP_IDX + 1;
      }
   }

   TRACE (Func_Exit, "cc_advance_idx", NULL);

   return;

}  /* cc_advance_idx */

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

boolean scan_fixed_comment(void)

{
   int		attr_idx;
   char		*char_ptr;
   int		col;
   boolean	found_macro = FALSE;
   int		i;
   int		id_start_idx;
   int		len;
   int		macro_len;
   int		name_idx;
   int		save_pp_line_idx;

   TRACE (Func_Entry, "scan_fixed_comment", NULL);

   save_pp_line_idx = pp_line_idx;

   end_stmt_line_idx = pp_line_idx;

   cc_stmt_buf_idx = NXT_COL(0);

   /* reset line type for now */

   PP_LINE_TYPE = Regular_Line;
   cc_advance_idx();

   id_start_idx = NXT_COL(1);
   col = nxt_line_col[NXT_COL(1)];

   CC_GET_ID_TOKEN;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx != NULL_IDX &&
       CC_AT_DEFINED(attr_idx)) {

      found_macro = TRUE;

      if (CC_AT_NUM_ARGS(attr_idx) == 0) {

         if (CC_AT_DYNAMIC_PREDEF(attr_idx)) {
            char_ptr = get_dynamic_predef_str(attr_idx, 
                                              TOKEN_LINE(cc_token));
            len = strlen(char_ptr);
         }
         else {
            char_ptr = CC_AT_STR_PTR(attr_idx);
            len = CC_AT_STR_LEN(attr_idx);
         }

         macro_len = (prev_idx - id_start_idx) + 1;
         shift_nxt_line(id_start_idx + macro_len,
                        len - macro_len);

         for (i = 0; i < len; i++) {
            nxt_line[i+id_start_idx] = char_ptr[i];
            nxt_line_col[i+id_start_idx] = col;
         }
      }
      else {
         if (! scan_fortran_macro(attr_idx, id_start_idx, pp_line_idx)) {
            found_macro = FALSE;
         }
      }
   }

   pp_line_idx = save_pp_line_idx;

   if (found_macro) {
      for (i = NXT_COL(1); i <= nxt_line_end_idx[end_stmt_line_idx]; i++) {
         if (nxt_line[i] < 0) {
            nxt_line[i] = nxt_line[i] & 0xFF;
         }
      }
   }

   PP_LINE_TYPE = Comment_Line;


   TRACE (Func_Exit, "scan_fixed_comment", NULL);

   return(found_macro);

}  /* scan_fixed_comment */

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

boolean scan_fortran_stmt(void)

{
   int          attr_idx;
   int		col;
   char         *char_ptr;
   int          i;
   int          id_start_idx;
   int		len;
   int		macro_len;
   int          name_idx;
   boolean	found_macro = FALSE;
   int		save_pp_line_idx;
   int		end_idx;

   TRACE (Func_Entry, "scan_fortran_stmt", NULL);

   end_stmt_line_idx = nxt_line_num_lines;
   pp_line_idx = 1;

   cc_stmt_buf_idx = NXT_COL(0);
   cc_advance_idx();

   while (cc_stmt_buf_idx < pp_nxt_line_EOL[end_stmt_line_idx]) {

      if (nxt_line[cc_stmt_buf_idx] > 0 &&
          (ch_class[nxt_line[cc_stmt_buf_idx]] == Ch_Class_Letter ||
           nxt_line[cc_stmt_buf_idx] == underscore)) {

         id_start_idx = cc_stmt_buf_idx;
         col = nxt_line_col[cc_stmt_buf_idx];
         save_pp_line_idx = pp_line_idx;

         CC_GET_ID_TOKEN;

         attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                    TOKEN_LEN(cc_token),
                                    &name_idx);

         if (attr_idx != NULL_IDX &&
             CC_AT_DEFINED(attr_idx)) {

            found_macro = TRUE;

            if (CC_AT_NUM_ARGS(attr_idx) == 0) {

               if (CC_AT_DYNAMIC_PREDEF(attr_idx)) {
                  char_ptr = get_dynamic_predef_str(attr_idx, 
                                                    TOKEN_LINE(cc_token));
                  len = strlen(char_ptr);
               }
               else {
                  char_ptr = CC_AT_STR_PTR(attr_idx);
                  len = CC_AT_STR_LEN(attr_idx);
               }

               end_idx = prev_idx + 1;

               adjust_continued_macro(id_start_idx, &end_idx);

               macro_len = end_idx - id_start_idx;

               shift_nxt_line(end_idx,
                              len - macro_len);

               for (i = 0; i < len; i++) {
                  nxt_line[i+id_start_idx] = char_ptr[i];
                  nxt_line_col[i+id_start_idx] = col;
               }

               cc_stmt_buf_idx = id_start_idx + len - 1;
               pp_line_idx = save_pp_line_idx;
               cc_advance_idx();
            }
            else {
               if (! scan_fortran_macro(attr_idx, id_start_idx, 
                                        save_pp_line_idx)) {
                  found_macro = FALSE;
                  goto EXIT;
               }
            }
         }
      }
      else {
         cc_advance_idx();
      }
   }

EXIT:

   if (found_macro) {
      for (i = 1; i <= nxt_line_end_idx[end_stmt_line_idx]; i++) {
         if (nxt_line[i] < 0) {
            nxt_line[i] = nxt_line[i] & 0xFF;
         }
      }
   }

   TRACE (Func_Exit, "scan_fortran_stmt", NULL);

   return(found_macro);

}  /* scan_fortran_stmt */

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

static cc_arg_type *alloc_arg_entry(int	num_chars)

{
   cc_arg_type	*arg_ptr;
   int		i;


   TRACE (Func_Entry, "alloc_arg_entry", NULL);

   arg_ptr = (cc_arg_type *)malloc(sizeof(cc_arg_type));

   /* 18Jan01[sos]: compute num_chars accounting for ending '\0' */
   num_chars = ((num_chars+TARGET_CHARS_PER_WORD)/
                           TARGET_CHARS_PER_WORD) * TARGET_CHARS_PER_WORD;

   arg_ptr->name = (char *)malloc(num_chars);

   for (i = 0; i < num_chars; i++) {
      arg_ptr->name[i] = '\0';
   }

   TRACE (Func_Exit, "alloc_arg_entry", NULL);

   return(arg_ptr);

}  /* alloc_arg_entry */

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

static void free_arg_entry(cc_arg_type *arg_entry)

{


   TRACE (Func_Entry, "free_arg_entry", NULL);

   free(arg_entry->name);
   free(arg_entry);

   TRACE (Func_Exit, "free_arg_entry", NULL);

   return;

}  /* free_arg_entry */

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

static void scan_cc_macro(int	attr_idx,
                          int	id_start_idx)

{
   int          arg_num;
   char         *char_ptr;
   int		col;
   int          i;
   int          j;
   int          k;
   int          len;
   int		line;
   int          paren_cnt = 0;
   cc_arg_type  *arg_head = NULL;
   cc_arg_type  *arg_tail = NULL;

   TRACE (Func_Entry, "scan_cc_macro", NULL);

   if (CC_LA_CH_VALUE == LPAREN) {
      /* macro with arguments. */

      cc_stmt_buf_idx++;

      arg_num = 1;
      while (arg_num <= CC_AT_NUM_ARGS(attr_idx)) {

         i = 0;
         k = cc_stmt_buf_idx;
         paren_cnt = 0;

         while ((cc_stmt_buf[k] != RPAREN ||
                 paren_cnt > 0)                       &&
                (cc_stmt_buf[k] != COMMA ||
                 paren_cnt > 0)                       &&
                cc_stmt_buf[k] != EOS) {

            if (cc_stmt_buf[k] == LPAREN ||
                cc_stmt_buf[k] == '[') {
               paren_cnt++;
            }
            else if (cc_stmt_buf[k] == RPAREN ||
                     cc_stmt_buf[k] == ']') {
               paren_cnt--;
            }

            k++;
            i++;
         }

         if (arg_head == NULL) {
            arg_head = alloc_arg_entry(i);
            arg_head->next = NULL;
            arg_tail = arg_head;
         }
         else {
            arg_tail->next = alloc_arg_entry(i);
            arg_tail = arg_tail->next;
            arg_tail->next = NULL;
         }

         i = 0;
         paren_cnt = 0;

         while ((cc_stmt_buf[cc_stmt_buf_idx] != RPAREN ||
                 paren_cnt > 0)                       &&
                (cc_stmt_buf[cc_stmt_buf_idx] != COMMA ||
                 paren_cnt > 0)                       &&
                cc_stmt_buf[cc_stmt_buf_idx] != EOS) {

            if (cc_stmt_buf[cc_stmt_buf_idx] == LPAREN ||
                cc_stmt_buf[cc_stmt_buf_idx] == '[') {
               paren_cnt++;
            }
            else if (cc_stmt_buf[cc_stmt_buf_idx] == RPAREN ||
                     cc_stmt_buf[cc_stmt_buf_idx] == ']') {
               paren_cnt--;
            }

            arg_tail->name[i] = cc_stmt_buf[cc_stmt_buf_idx];
            cc_stmt_buf_idx++;
            i++;
         }

         arg_tail->name_len = i;

         if (cc_stmt_buf[cc_stmt_buf_idx] == COMMA &&
             arg_num < CC_AT_NUM_ARGS(attr_idx)) {
            CC_NEXT_LA_CH;
         }
         else if (cc_stmt_buf[cc_stmt_buf_idx] == RPAREN &&
                  arg_num < CC_AT_NUM_ARGS(attr_idx)) {
            /* not enough args */
            find_line_and_col(cc_stmt_buf_idx, &line, &col);
            ntr_next_msg_queue(line, 1550, Warning, col,
                               CC_AT_NAME_PTR(attr_idx),
                               0,
                               STR_ARG);
            flush_cc_line();
            goto EXIT;
         }

         arg_num++;
      }

      if (cc_stmt_buf[cc_stmt_buf_idx] != RPAREN) {
         /* too many args */
         find_line_and_col(cc_stmt_buf_idx, &line, &col);
         ntr_next_msg_queue(line, 1551, Warning, col,
                            CC_AT_NAME_PTR(attr_idx),
                            0,
                            STR_ARG);
         flush_cc_line();
         goto EXIT;
      }

      /* determine macro length */

      len = CC_AT_STR_LEN(attr_idx);

      for (i = 0; i < CC_AT_STR_LEN(attr_idx); i++) {
         if ((CC_AT_STR_PTR(attr_idx))[i] == '\001') {
            i += 2;
            arg_num = 1;
            arg_tail = arg_head;
            while (arg_num != (CC_AT_STR_PTR(attr_idx))[i]) {
               arg_tail = arg_tail->next;
               arg_num++;
            }

           len += (arg_tail->name_len - 3);
         }
      }

      /* cc_stmt_buf_idx is the end of replacement */
      /* id_start_idx is the start of replacement */

      shift_cc_stmt_buf(cc_stmt_buf_idx,
                        len-((cc_stmt_buf_idx - id_start_idx) + 1));

      char_ptr = CC_AT_STR_PTR(attr_idx);

      j = 0;
      i = 0;
      while (i < len) {
         if (char_ptr[j] == '\001') {
            j += 2;
            arg_num = 1;
            arg_tail = arg_head;
            while (arg_num != char_ptr[j]) {
               arg_tail = arg_tail->next;
               arg_num++;
            }

            for (k = 0; k < arg_tail->name_len; k++) {
               cc_stmt_buf[i+id_start_idx] = arg_tail->name[k];
               i++;
            }
         }
         else {
            cc_stmt_buf[i+id_start_idx] = char_ptr[j];
            i++;
         }

         j++;
      }

      cc_stmt_buf_idx = id_start_idx - 1;
      CC_NEXT_LA_CH;
   }

EXIT:

   while (arg_head != NULL) {
      arg_tail = arg_head->next;
      free_arg_entry(arg_head);
      arg_head = arg_tail;
   }

   TRACE (Func_Exit, "scan_cc_macro", NULL);

   return;

}  /* scan_cc_macro */

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

static boolean scan_fortran_macro(int	attr_idx,
                                  int	id_start_idx,
				  int	save_pp_line_idx)

{
   int          arg_num;
   int		col;
   int          i;
   int          j;
   boolean	ok = TRUE;
   int          paren_cnt = 0;
   cc_arg_type  *arg_head = NULL;
   cc_arg_type  *arg_tail = NULL;
   int		save_line_idx;
   int		save_cc_stmt_buf_idx;


   TRACE (Func_Entry, "scan_fortran_macro", NULL);

   if (nxt_line[cc_stmt_buf_idx] == lparen) {
      /* macro with arguments. */

      col = nxt_line_col[id_start_idx];

      cc_advance_idx();

      arg_num = 1;
      while (arg_num <= CC_AT_NUM_ARGS(attr_idx)) {

         while (nxt_line[cc_stmt_buf_idx] == blank ||
                nxt_line[cc_stmt_buf_idx] == tab) {
            cc_advance_idx();
         }
                
         save_line_idx = pp_line_idx;
         save_cc_stmt_buf_idx = cc_stmt_buf_idx;

         i = 0;
         paren_cnt = 0;

         while ((nxt_line[cc_stmt_buf_idx] != rparen ||
                 paren_cnt > 0)                       &&
                (nxt_line[cc_stmt_buf_idx] != comma ||
                 paren_cnt > 0)                       &&
                nxt_line[cc_stmt_buf_idx] != eos) {

            if (nxt_line[cc_stmt_buf_idx] == lparen ||
                nxt_line[cc_stmt_buf_idx] == lbrkt) {
               paren_cnt++;
            }
            else if (nxt_line[cc_stmt_buf_idx] == rparen ||
                     nxt_line[cc_stmt_buf_idx] == rbrkt) {
               paren_cnt--;
            }

            cc_advance_idx();
            i++;
         }

         if (arg_head == NULL) {
            arg_head = alloc_arg_entry(i);
            arg_head->next = NULL;
            arg_tail = arg_head;
         }
         else {
            arg_tail->next = alloc_arg_entry(i);
            arg_tail = arg_tail->next;
            arg_tail->next = NULL;
         }

         cc_stmt_buf_idx = save_cc_stmt_buf_idx;
         pp_line_idx = save_line_idx;

         i = 0;
         paren_cnt = 0;

         while ((nxt_line[cc_stmt_buf_idx] != rparen ||
                 paren_cnt > 0)                       &&
                (nxt_line[cc_stmt_buf_idx] != comma ||
                 paren_cnt > 0)                       &&
                nxt_line[cc_stmt_buf_idx] != eos) {

            if (nxt_line[cc_stmt_buf_idx] == lparen ||
                nxt_line[cc_stmt_buf_idx] == lbrkt) {
               paren_cnt++;
            }
            else if (nxt_line[cc_stmt_buf_idx] == rparen ||
                     nxt_line[cc_stmt_buf_idx] == rbrkt) {
               paren_cnt--;
            }

            arg_tail->name[i] = nxt_line[cc_stmt_buf_idx];
            cc_advance_idx();
            i++;
         }

         j = i - 1;
         while (arg_tail->name[j] == blank ||
                arg_tail->name[j] == tab) {
            j--;
         }

         arg_tail->name[++j] = '\0';
         arg_tail->name_len = j;
         

         if (nxt_line[cc_stmt_buf_idx] == comma &&
             arg_num < CC_AT_NUM_ARGS(attr_idx)) {
            cc_advance_idx();
         }
         else if (nxt_line[cc_stmt_buf_idx] == rparen &&
                  arg_num < CC_AT_NUM_ARGS(attr_idx)) {
            /* not enough args */
            ntr_next_msg_queue(PP_LINE_NUM, 1550, Warning, 
                               nxt_line_col[cc_stmt_buf_idx],
                               CC_AT_NAME_PTR(attr_idx),
                               0,
                               STR_ARG);
            ok = FALSE;
            goto EXIT;
         }

         arg_num++;
      }

      if (nxt_line[cc_stmt_buf_idx] != rparen) {
         /* too many args */
         ntr_next_msg_queue(PP_LINE_NUM, 1551, Warning,
                            nxt_line_col[cc_stmt_buf_idx],
                            CC_AT_NAME_PTR(attr_idx),
                            0,
                            STR_ARG);
         ok = FALSE;
         goto EXIT;
      }

      insert_macro(attr_idx,
                   arg_head,
                   id_start_idx,
                   col,
                   save_pp_line_idx);

   }
   else {
      ok = FALSE;
   }

EXIT:

   while (arg_head != NULL) {
      arg_tail = arg_head->next;
      free_arg_entry(arg_head);
      arg_head = arg_tail;
   }

   TRACE (Func_Exit, "scan_fortran_macro", NULL);

   return(ok);

}  /* scan_fortran_macro */

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

void init_cond_comp(void)

{
   int                  ln_idx;
   int			word;

   TRACE (Func_Entry, "init_cond_comp", NULL);

   /* initialize cc_initial_token used to initialize token */

   for (word = 0;  word < CC_NUM_ID_WDS;  word++) {
      TOKEN_STR_WD(cc_initial_token, word) = 0;
   }

   TOKEN_LEN(cc_initial_token)             = 0;
   TOKEN_VALUE(cc_initial_token)           = Cc_Tok_Unknown;
   TOKEN_ERR(cc_initial_token)             = FALSE;
   TOKEN_KIND_STR(cc_initial_token)[0]     = EOS;
   TOKEN_KIND_LEN(cc_initial_token)        = 0;
   TOKEN_COLUMN(cc_initial_token)          = 0;
   TOKEN_LINE(cc_initial_token)            = 0;


   CHECK_INITIAL_ALLOC (cc_attr_tbl, 	NULL_IDX);
   CHECK_INITIAL_ALLOC (cc_ln_tbl, 	NULL_IDX);
   CHECK_INITIAL_ALLOC (cc_blk_stk_tbl, NULL_IDX);

   ln_idx                               = cc_ln_tbl_idx + 1;

   TBL_REALLOC_CK(cc_ln_tbl, 2);
   CLEAR_TBL_NTRY(cc_ln_tbl, ln_idx);
   CC_LN_NAME_IDX(ln_idx)               = NAME_POOL_ZERO_IDX; /* Zero word */
   CC_LN_NAME_LEN(ln_idx)               = HOST_BYTES_PER_WORD;
   cc_ln_fw_idx                         = ln_idx;

   CLEAR_TBL_NTRY(cc_ln_tbl, cc_ln_tbl_idx);
   CC_LN_NAME_IDX(cc_ln_tbl_idx)       	= NAME_POOL_ONES_IDX; /* Ones word   */
   CC_LN_NAME_LEN(cc_ln_tbl_idx)       	= HOST_BYTES_PER_WORD;
   cc_ln_lw_idx                         = cc_ln_tbl_idx;

   TRACE (Func_Exit, "init_cond_comp", NULL);

   return;

}  /* init_cond_comp */

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

void enter_predefined_macros(void)

{
   int                  attr_idx;
   int                  name_idx;


# if defined(_TARGET_OS_UNICOS)  ||  defined(_TARGET_OS_MAX)

   union        {long   int_form;
                 char   char_form[9];
                } cpu_type;

# endif


   TRACE (Func_Entry, "enter_predefined_macros", NULL);

   /****************************************\
   |* These are dynamic predefined macros. *|
   |* Enter them without a string and save *|
   |* the index in global variables.       *|
   \****************************************/

   /************\
   |* __line__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__line__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      line_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }

   /************\
   |* __LINE__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__LINE__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      LINE_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }

   /************\
   |* __file__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__file__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      file_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }

   /************\
   |* __FILE__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__FILE__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      FILE_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }

   /************\
   |* __date__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__date__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      date_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }

   /************\
   |* __DATE__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__DATE__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      DATE_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }

   /************\
   |* __time__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__time__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      time_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }

   /************\
   |* __TIME__ *|
   \************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__TIME__");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
      CC_AT_DYNAMIC_PREDEF(attr_idx) = TRUE;
      TIME_macro_idx = attr_idx;
   }
   else {
      /* error, cannot be redefined */
   }


# ifdef _UNICOS

   /*********************\
   |* Predefine _UNICOS *|
   \*********************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_UNICOS");
   TOKEN_LEN(cc_token) = 7;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, _UNICOS);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


# endif


#if defined(_HOST_OS_UNICOS)  ||  defined(_HOST_OS_SOLARIS)  ||  \
    defined(_HOST_OS_MAX) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))

   /******************\
   |* Predefine unix *|
   \******************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "unix");
   TOKEN_LEN(cc_token) = 4;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


   /********************\
   |* Predefine __unix *|
   \********************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__unix");
   TOKEN_LEN(cc_token) = 6;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }

# endif


#if defined(_TARGET_OS_UNICOS)  ||  defined(_TARGET_OS_MAX)

   /* Define cray, _CRAY, and CRAY if the target is a PVP or MPP. */

   /******************\
   |* Predefine cray *|
   \******************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "cray");
   TOKEN_LEN(cc_token) = 4;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


   /******************\
   |* Predefine CRAY *|
   \******************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "CRAY");
   TOKEN_LEN(cc_token) = 4;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);


   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


   /*******************\
   |* Predefine _CRAY *|
   \*******************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_CRAY");
   TOKEN_LEN(cc_token) = 5;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


   /**********************\
   |* Predefine _MEMSIZE *|
   \**********************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_MEMSIZE");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, target_machine.fld.mcmsz);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }

#endif


# ifdef _TARGET_OS_UNICOS

# if defined(_GETPMC_AVAILABLE)
   cpu_type.int_form     = target_machine.fld.mcpmt;
   cpu_type.char_form[8] = '\0';
# else
   strcpy(cpu_type.char_form, target_machine.fld.mcpmt);
# endif

   if (strcmp("CRAY-C90", cpu_type.char_form) == 0) {

      /**********************\
      |* Predefine _CRAYC90 *|
      \**********************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "_CRAYC90");
      TOKEN_LEN(cc_token) = 8;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 128);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }


      /***************************\
      |* Predefine _MAXVL to 128 *|
      \***************************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "_MAXVL");
      TOKEN_LEN(cc_token) = 6;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 128);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }
   }
   else if (strcmp("CRAY-TS", cpu_type.char_form) == 0) {

      /**********************\
      |* Predefine _CRAYT90 *|
      \**********************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "_CRAYT90");
      TOKEN_LEN(cc_token) = 8;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 128);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }

      /***************************\
      |* Predefine _MAXVL to 128 *|
      \***************************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "_MAXVL");
      TOKEN_LEN(cc_token) = 6;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 128);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }


      /*********************\
      |* Predefine _ADDR64 *|
      \*********************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "_ADDR64");
      TOKEN_LEN(cc_token) = 7;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 1);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }
   }
   else if (strcmp("CRAY-YMP", cpu_type.char_form) == 0) {

      /**************************\
      |* Predefine _MAXVL to 64 *|
      \**************************/

      cc_token = cc_initial_token;
      strcpy(TOKEN_STR(cc_token), "_MAXVL");
      TOKEN_LEN(cc_token) = 6;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);


      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 64);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }
   }

   if (target_machine.fld.mcaddr32) {

      /*****************\
      |* Predefine YMP *|
      \*****************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "YMP");
      TOKEN_LEN(cc_token) = 3;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 1);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }


      /*********************\
      |* Predefine _ADDR32 *|
      \*********************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "_ADDR32");
      TOKEN_LEN(cc_token) = 7;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 1);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }
   }


   if (target_triton  &&  target_ieee) {

      /***********************\
      |* Predefine _CRAYIEEE *|
      \***********************/

      cc_token = cc_initial_token;

      strcpy(TOKEN_STR(cc_token), "_CRAYIEEE");
      TOKEN_LEN(cc_token) = 9;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx = ntr_cc_sym_tbl(&cc_token, name_idx);
         PUT_VALUE_IN_AT_STR(attr_idx, 1);
         CC_AT_DEFINED(attr_idx) = TRUE;
      }

   }

# endif


# ifdef _TARGET_OS_MAX

   /**********************\
   |* Predefine _CRAYMPP *|
   \**********************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_CRAYMPP");
   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


   /**********************************\
   |* Predefine CRAY-T3D or CRAY-T3E *|
   \**********************************/

# if defined(_GETPMC_AVAILABLE)
   cpu_type.int_form     = target_machine.fld.mcpmt;
   cpu_type.char_form[8] = '\0';
# else
   strcpy(cpu_type.char_form, target_machine.fld.mcpmt);
# endif

   cc_token = cc_initial_token;

   if (strcmp("CRAY-T3D", cpu_type.char_form) == 0) {
      strcpy(TOKEN_STR(cc_token), "_CRAYT3D");
   }
   else {
      strcpy(TOKEN_STR(cc_token), "_CRAYT3E");
   }

   TOKEN_LEN(cc_token) = 8;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


   /***********************\
   |* Predefine _CRAYIEEE *|
   \***********************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_CRAYIEEE");
   TOKEN_LEN(cc_token) = 9;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }

   /*********************\
   |* Predefine _ADDR64 *|
   \*********************/

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_ADDR64");
   TOKEN_LEN(cc_token) = 7;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


# endif


#ifdef _TARGET_OS_SOLARIS

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "__sparc");
   TOKEN_LEN(cc_token) = 7;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "sun");
   TOKEN_LEN(cc_token) = 3;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_SUN");
   TOKEN_LEN(cc_token) = 4;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "sparc");
   TOKEN_LEN(cc_token) = 5;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }

   cc_token = cc_initial_token;

   strcpy(TOKEN_STR(cc_token), "_SPARC");
   TOKEN_LEN(cc_token) = 6;

   attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                              TOKEN_LEN(cc_token),
                              &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                = ntr_cc_sym_tbl(&cc_token, name_idx);
      PUT_VALUE_IN_AT_STR(attr_idx, 1);
      CC_AT_DEFINED(attr_idx) = TRUE;
   }


#endif


   TRACE (Func_Exit, "enter_predefined_macros", NULL);

   return;

}  /* enter_predefined_macros */


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

static void cc_get_token(boolean	keyword_expected)

{
   int		i;
   int		idx;
   boolean	its_unsigned = FALSE;
   char		upper_str[CC_MAX_ID_LEN+1];
   char		str[80];
   long64	the_constant;


   TRACE (Func_Entry, "cc_get_token", NULL);

   cc_token = cc_initial_token;

   TOKEN_LINE(cc_token)   = CC_LA_CH_LINE;
   TOKEN_COLUMN(cc_token) = CC_LA_CH_COLUMN;

   if (CC_LA_CH_VALUE == eos) {
      TOKEN_VALUE(cc_token)  = Cc_Tok_EOS;
   }
   else if (CC_LA_CH_CLASS == Ch_Class_Letter ||
            CC_LA_CH_VALUE == underscore) {


      TOKEN_LEN(cc_token) = 0;

      while (cc_stmt_buf[cc_stmt_buf_idx] != newline &&
             cc_stmt_buf[cc_stmt_buf_idx] != blank &&
             cc_stmt_buf[cc_stmt_buf_idx] != tab &&
             VALID_CC_ID_CHAR(cc_stmt_buf[cc_stmt_buf_idx])) {

         if (TOKEN_LEN(cc_token) < CC_MAX_ID_LEN) {
            ADD_TO_CC_TOKEN_STR(cc_stmt_buf[cc_stmt_buf_idx], 
                                TOKEN_LEN(cc_token));
         }

         cc_stmt_buf_idx++;
      }

      cc_stmt_buf_idx--;
      CC_NEXT_LA_CH;

      TOKEN_VALUE(cc_token)  = Cc_Tok_Id;

      /* see if it is a keyword */

      if (keyword_expected) {

         for (i = 0; i < TOKEN_LEN(cc_token); i++) {
            if (islower((TOKEN_STR(cc_token))[i])) {
               upper_str[i] = TOUPPER((TOKEN_STR(cc_token))[i]);
            }
            else {
               upper_str[i] = (TOKEN_STR(cc_token))[i];
            }
         }

         upper_str[i] ='\0';

         switch(upper_str[0]) {
            case 'D':
               if (TOKEN_LEN(cc_token) == 6 &&
                   EQUAL_STRS(upper_str, "DEFINE")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Define;
               }
               else if (TOKEN_LEN(cc_token) == 7 &&
                   EQUAL_STRS(upper_str, "DEFINED")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Defined;
               }
               break;
        
            case 'E':
               if (TOKEN_LEN(cc_token) == 4 &&
                   EQUAL_STRS(upper_str, "ELIF")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Elif;
               }
               else if (TOKEN_LEN(cc_token) == 4 &&
                   EQUAL_STRS(upper_str, "ELSE")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Else;
               }
               else if (TOKEN_LEN(cc_token) == 5 &&
                   EQUAL_STRS(upper_str, "ENDIF")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Endif;
               }
               else if (TOKEN_LEN(cc_token) == 5 &&
                   EQUAL_STRS(upper_str, "ERROR")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Error;
               }
               break;

            case 'I':
               if (TOKEN_LEN(cc_token) == 2 &&
                   EQUAL_STRS(upper_str, "IF")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_If;
               }
               else if (TOKEN_LEN(cc_token) == 5 &&
                   EQUAL_STRS(upper_str, "IFDEF")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Ifdef;
               }
               else if (TOKEN_LEN(cc_token) == 6 &&
                   EQUAL_STRS(upper_str, "IFNDEF")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Ifndef;
               }
               else if (TOKEN_LEN(cc_token) == 7 &&
                   EQUAL_STRS(upper_str, "INCLUDE")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Include;
               }

               break;

            case 'U':
               if (TOKEN_LEN(cc_token) == 5 &&
                   EQUAL_STRS(upper_str, "UNDEF")) {
                  TOKEN_VALUE(cc_token) = Cc_Tok_Kwd_Undef;
               }

               break;

         }
      }
   }
   else if (CC_LA_CH_CLASS == Ch_Class_Digit) {

      idx = 0;
      if (CC_LA_CH_VALUE == ZERO) {
         /* possibly either oct or hex */

         str[idx] = CC_LA_CH_VALUE;
         idx++;
         CC_NEXT_LA_CH;

         if (CC_LA_CH_VALUE == 'X' ||
             CC_LA_CH_VALUE == 'x') {
            str[idx] = CC_LA_CH_VALUE;
            idx++;
            CC_NEXT_LA_CH;
         }
      }

      while (CC_LA_CH_CLASS == Ch_Class_Digit &&
             idx < 79) {
         str[idx] = CC_LA_CH_VALUE;
         CC_NEXT_LA_CH;
         idx++;
      }

      str[idx] = '\0';

      if (CC_LA_CH_VALUE == 'L' ||
          CC_LA_CH_VALUE == 'l') {
         /* just swallow it for now */
         CC_NEXT_LA_CH;

         if (CC_LA_CH_VALUE == 'U' ||
             CC_LA_CH_VALUE == 'u') {
            /* just swallow it for now */
            CC_NEXT_LA_CH;
            its_unsigned = TRUE;
         }
      }

      if (CC_LA_CH_VALUE == 'U' ||
          CC_LA_CH_VALUE == 'u') {
         /* just swallow it for now */
         CC_NEXT_LA_CH;
         its_unsigned = TRUE;

         if (CC_LA_CH_VALUE == 'L' ||
             CC_LA_CH_VALUE == 'l') {
            /* just swallow it for now */
            CC_NEXT_LA_CH;
         }
      }

      if (its_unsigned) {
         the_constant = (long64) strtoul(str, NULL, 0);
      }
      else {
         the_constant = (long64) strtol(str, NULL, 0);
      }

      TOKEN_CONST_TBL_IDX(cc_token) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                  the_constant);

      TOKEN_VALUE(cc_token) = Cc_Tok_Constant;

   }
   else {

      switch(CC_LA_CH_VALUE) {

         case '#':

            CC_NEXT_LA_CH;
            break;

         case '+':
            TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Add;

            CC_NEXT_LA_CH;
            break;

         case '/':
            TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Div;

            CC_NEXT_LA_CH;
            break;

         case '%':
            TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Mod;

            CC_NEXT_LA_CH;
            break;

         case '*':
            TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Mult;

            CC_NEXT_LA_CH;
            break;

         case '-':
            TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Sub;

            CC_NEXT_LA_CH;
            break;

         case '!':
            if (cc_stmt_buf[cc_stmt_buf_idx + 1] == '=') {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Ne;
               CC_NEXT_LA_CH;
            }
            else {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Not;
            }

            CC_NEXT_LA_CH;
            break;

         case '~':
            TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Bnot;

            CC_NEXT_LA_CH;
            break;

         case '^':
            TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Neqv;

            CC_NEXT_LA_CH;
            break;

         case '=':
            if (cc_stmt_buf[cc_stmt_buf_idx+1] == '=') {
               TOKEN_VALUE(cc_token)  = Cc_Tok_Op_Eq;
               CC_NEXT_LA_CH;
            }

            CC_NEXT_LA_CH;
            break;

         case '>':

            if (cc_stmt_buf[cc_stmt_buf_idx + 1] == '=') {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Ge;
               CC_NEXT_LA_CH;
            }
            else if (cc_stmt_buf[cc_stmt_buf_idx + 1] == '>') {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Rshift;
               CC_NEXT_LA_CH;
            }
            else {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Gt;
            }

            CC_NEXT_LA_CH;
            break;

         case '<':

            if (cc_stmt_buf[cc_stmt_buf_idx + 1] == '=') {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Le;
               CC_NEXT_LA_CH;
            }
            else if (cc_stmt_buf[cc_stmt_buf_idx + 1] == '<') {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Lshift;
               CC_NEXT_LA_CH;
            }
            else {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Lt;
            }

            CC_NEXT_LA_CH;
            break;

         case '&':

            if (cc_stmt_buf[cc_stmt_buf_idx + 1] == '&') {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_And;
               CC_NEXT_LA_CH;
            }
            else {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Band;
            }

            CC_NEXT_LA_CH;
            break;

         case '|':

            if (cc_stmt_buf[cc_stmt_buf_idx + 1] == '|') {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Or;
               CC_NEXT_LA_CH;
            }
            else {
               TOKEN_VALUE(cc_token) = Cc_Tok_Op_Bor;
            }

            CC_NEXT_LA_CH;
            break;

      } /* end switch */

   }

   TRACE (Func_Exit, "cc_get_token", NULL);

   return;

}  /* cc_get_token */

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

static void cc_get_next_char(void)

{

   int		col;
   int		line;


   TRACE (Func_Entry, "cc_get_next_char", NULL);

   cc_stmt_buf_idx++;

BACK:

   /* skip any leading white space */
   while (cc_stmt_buf[cc_stmt_buf_idx] == blank ||
          cc_stmt_buf[cc_stmt_buf_idx] == tab) {

      cc_stmt_buf_idx++;
   }

   if (cc_stmt_buf[cc_stmt_buf_idx] == '/' &&
       cc_stmt_buf[cc_stmt_buf_idx + 1] == '*') {
      /* this is a comment */
      cc_stmt_buf_idx += 2;

      while (! (cc_stmt_buf[cc_stmt_buf_idx] == '*' &&
                cc_stmt_buf[cc_stmt_buf_idx + 1] == '/') &&
             cc_stmt_buf[cc_stmt_buf_idx] != newline) {
         cc_stmt_buf_idx++;
      }

      if (cc_stmt_buf[cc_stmt_buf_idx] != newline) {
         cc_stmt_buf_idx += 2;
         goto BACK;
      }
   }

   if (cc_stmt_buf[cc_stmt_buf_idx] == newline) {
      CC_LA_CH_VALUE = eos;
   }
   else {
      CC_LA_CH_VALUE  = cc_stmt_buf[cc_stmt_buf_idx];
   }

   find_line_and_col(cc_stmt_buf_idx, &line, &col);
   CC_LA_CH_LINE   = line;
   CC_LA_CH_COLUMN = col;
   CC_LA_CH_CLASS  = (CC_LA_CH_VALUE == (char) EOF) ? 
                       Ch_Class_EOF : ch_class[CC_LA_CH_VALUE];


   TRACE (Func_Exit, "cc_get_next_char", NULL);

   return;

}  /* cc_get_next_char */

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

static void cc_get_stmt(void)

{
#ifdef KEY /* Bug 10177 */
   int		char_start_idx = 0;
   int		comment_start_idx;
   char		delim = 0;
#else /* KEY Bug 10177 */
   int		char_start_idx;
   int		comment_start_idx;
   char		delim;
#endif /* KEY Bug 10177 */
   int		idx;
   boolean	in_char_context = FALSE;
   boolean	is_define_dir = FALSE;
   int		save_idx;
   int		start_col;
   int		start_line;

   TRACE (Func_Entry, "cc_get_stmt", NULL);

   cc_line_continued = FALSE;

   cc_stmt_buf_len = cc_stmt_buf_idx;
   cc_stmt_buf_idx = NULL_IDX;

   CC_NEXT_LA_CH; /* # */

   CC_NEXT_LA_CH;

   cc_get_token(TRUE);

   if (TOKEN_VALUE(cc_token) == Cc_Tok_Kwd_Define) {
      is_define_dir = TRUE;
   }

   cc_stmt_buf_idx = NULL_IDX;

   while (cc_stmt_buf[++cc_stmt_buf_idx] != eos) {

      if (cc_stmt_buf[cc_stmt_buf_idx] == QUOTE ||
          cc_stmt_buf[cc_stmt_buf_idx] == DBL_QUOTE) {

         if (in_char_context) {

            if (delim == cc_stmt_buf[cc_stmt_buf_idx] &&
                delim != cc_stmt_buf[cc_stmt_buf_idx+1]) {
               in_char_context = FALSE;
            }
            else if (delim == cc_stmt_buf[cc_stmt_buf_idx] &&
                     delim == cc_stmt_buf[cc_stmt_buf_idx+1]) {
               cc_stmt_buf_idx++;
            }
         }
         else {
            /* not in character context. */

            in_char_context = TRUE;
            char_start_idx = cc_stmt_buf_idx;
            delim = cc_stmt_buf[cc_stmt_buf_idx];
         }
      }

      if (! in_char_context &&
          cc_stmt_buf[cc_stmt_buf_idx] == '/' &&
          cc_stmt_buf[cc_stmt_buf_idx + 1] == '*') {
         /* this is a comment */
         comment_start_idx = cc_stmt_buf_idx;
         start_line = cc_stmt_buf_line[cc_stmt_buf_num_lines].line;
         start_col = comment_start_idx - 
                          cc_stmt_buf_line[cc_stmt_buf_num_lines].start_idx;
         cc_stmt_buf_idx += 2;

         while (! (cc_stmt_buf[cc_stmt_buf_idx] == '*' &&
                   cc_stmt_buf[cc_stmt_buf_idx + 1] == '/')) {


            if (cc_stmt_buf[cc_stmt_buf_idx] == newline) {
               cc_stmt_buf_idx--;
               save_idx = cc_stmt_buf_idx;
               read_line(TRUE);
               cc_stmt_buf_len = cc_stmt_buf_idx;
               cc_line_continued = TRUE;
               cc_stmt_buf_idx = save_idx;

               if (on_off_flags.preprocess_only || on_off_flags.save_dot_i) {
                  /* print blank line */
                  fprintf(dot_i_fptr, "\n");
               }
            }
            cc_stmt_buf_idx++;
         }

         cc_stmt_buf_idx += 2;
      }


      if (cc_stmt_buf[cc_stmt_buf_idx] == newline &&
          cc_stmt_buf[cc_stmt_buf_idx - 1] == '\\') {
         cc_stmt_buf_idx -= 2;

         save_idx = cc_stmt_buf_idx;
         read_line(TRUE);
         cc_stmt_buf_len = cc_stmt_buf_idx;
         cc_line_continued = TRUE;
         cc_stmt_buf_idx = save_idx;

         if (on_off_flags.preprocess_only || on_off_flags.save_dot_i) {
            /* print blank line */
            fprintf(dot_i_fptr, "\n");
         }

      }
      else if (is_define_dir &&
               cc_stmt_buf[cc_stmt_buf_idx] == amp &&
               source_form == Free_Form) {

         idx = cc_stmt_buf_idx + 1;

         while (cc_stmt_buf[idx] == blank ||
                cc_stmt_buf[idx] == tab) {
            idx++;
         }

         if (cc_stmt_buf[idx] == '!' ||
             cc_stmt_buf[idx] == newline) {

            cc_stmt_buf_idx--;

            save_idx = cc_stmt_buf_idx;
            read_line(TRUE);
            cc_stmt_buf_len = cc_stmt_buf_idx;
            cc_line_continued = TRUE;
            cc_stmt_buf_idx = save_idx;

            idx = cc_stmt_buf_idx + 1;

            while (cc_stmt_buf[idx] == blank ||
                   cc_stmt_buf[idx] == tab) {
               idx++;
            }

            if (cc_stmt_buf[idx] == amp) {
               shift_cc_stmt_buf(idx+1,
                                 cc_stmt_buf_idx - idx);
            }

            if (on_off_flags.preprocess_only || on_off_flags.save_dot_i) {
               /* print blank line */
               fprintf(dot_i_fptr, "\n");
            }
         }
      }
      else if (! in_char_context &&
               is_define_dir &&
               cc_stmt_buf[cc_stmt_buf_idx] == '!') {

         idx = cc_stmt_buf_idx;
         while (cc_stmt_buf[idx] != newline) {
            idx++;
         }

         if (cc_stmt_buf[idx-1] == '\\') {
            idx--;
         }

         shift_cc_stmt_buf(idx,
                           cc_stmt_buf_idx - idx);
         cc_stmt_buf_idx--;

      }
   }

   if (in_char_context) {
      find_line_and_col(char_start_idx, &start_line, &start_col);
      ntr_next_msg_queue(start_line, 1631, Warning,
                         start_col,
                         (char *)NULL,
                         0,
                         NO_ARG);
   }


   cc_stmt_buf_line[cc_stmt_buf_num_lines+1].line = 
             cc_stmt_buf_line[cc_stmt_buf_num_lines].line;
   cc_stmt_buf_line[cc_stmt_buf_num_lines+1].start_idx = cc_stmt_buf_idx;
   cc_stmt_buf_len = cc_stmt_buf_idx;
   cc_stmt_buf[cc_stmt_buf_idx+1] = '\0';
   cc_stmt_buf_idx = NULL_IDX;

   TRACE (Func_Exit, "cc_get_stmt", NULL);

   return;

}  /* cc_get_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     level-5-expr { defined-binary-op level-5-expr }               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*                                                                            *|
\******************************************************************************/

static boolean parse_cc_expr (opnd_type   *result)

{
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_expr", NULL);

   parsed_ok = parse_cc_level_8(&opnd);

   COPY_OPND((*result), opnd);


   TRACE (Func_Exit, "parse_cc_expr", NULL);

   return(parsed_ok);
} /* parse_cc_expr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     [not-op] level-5-expr                                         *|
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

static boolean parse_cc_mult_opnd(opnd_type *result)

{
   int		ir_idx		= NULL_IDX;
   opnd_type	opnd;
   boolean   	parsed_ok		= TRUE;


   TRACE (Func_Entry, "parse_cc_mult_opnd", NULL);

   if ((cc_stmt_buf[cc_stmt_buf_idx] == '!' &&
        cc_stmt_buf[cc_stmt_buf_idx+1] != '=') ||
       cc_stmt_buf[cc_stmt_buf_idx] == '~'     ||
       cc_stmt_buf[cc_stmt_buf_idx] == '+'     ||
       cc_stmt_buf[cc_stmt_buf_idx] == '-') {

      cc_get_token(FALSE);

      NTR_IR_TBL(ir_idx);
      OPND_FLD((*result)) = IR_Tbl_Idx;
      OPND_IDX((*result)) = ir_idx;
      switch (TOKEN_VALUE(cc_token)) {
         case Cc_Tok_Op_Add  :
            IR_OPR(ir_idx) = Uplus_Opr;
            break;
         case Cc_Tok_Op_Sub :
            IR_OPR(ir_idx) = Uminus_Opr;
            break;
         case Cc_Tok_Op_Bnot :
            IR_OPR(ir_idx) = Bnot_Opr;
            break;
         case Cc_Tok_Op_Not :
            IR_OPR(ir_idx) = Not_Opr;
            break;
      }

      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);
   }

   parsed_ok = parse_cc_operand(&opnd);

   if (ir_idx) {
      COPY_OPND(IR_OPND_L(ir_idx), opnd);
   }
   else {
      COPY_OPND((*result), opnd);
   }

   TRACE (Func_Exit, "parse_cc_mult_opnd", NULL);

   return(parsed_ok);
} /* parse_cc_mult_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     opnd {mult-op opnd}                                           *|
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

static boolean parse_cc_add_opnd(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_add_opnd", NULL);

   parsed_ok = parse_cc_mult_opnd(&opnd) && parsed_ok;

   if (CC_LA_CH_VALUE == '+' ||
       CC_LA_CH_VALUE == '/' ||
       CC_LA_CH_VALUE == '%' ||
       CC_LA_CH_VALUE == '*' ||
       CC_LA_CH_VALUE == '-' ||
       CC_LA_CH_VALUE == '!' ||
       CC_LA_CH_VALUE == '~' ||
       CC_LA_CH_VALUE == '^' ||
       CC_LA_CH_VALUE == '=' ||
       CC_LA_CH_VALUE == '>' ||
       CC_LA_CH_VALUE == '<' ||
       CC_LA_CH_VALUE == '&' ||
       CC_LA_CH_VALUE == '|') {

      cc_get_token(FALSE);
   }

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Mult ||
          TOKEN_VALUE(cc_token) == Cc_Tok_Op_Div  ||
          TOKEN_VALUE(cc_token) == Cc_Tok_Op_Mod) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(cc_token)) {
         case Cc_Tok_Op_Mult :
            IR_OPR(ir_idx) = Mult_Opr;
            break;
         case Cc_Tok_Op_Div  :
            IR_OPR(ir_idx) = Div_Opr;
            break;
         case Cc_Tok_Op_Mod  :
            IR_OPR(ir_idx) = Mod_Opr;
            break;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_mult_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;

      if (CC_LA_CH_VALUE == '+' ||
          CC_LA_CH_VALUE == '/' ||
          CC_LA_CH_VALUE == '%' ||
          CC_LA_CH_VALUE == '*' ||
          CC_LA_CH_VALUE == '-' ||
          CC_LA_CH_VALUE == '!' ||
          CC_LA_CH_VALUE == '~' ||
          CC_LA_CH_VALUE == '^' ||
          CC_LA_CH_VALUE == '=' ||
          CC_LA_CH_VALUE == '>' ||
          CC_LA_CH_VALUE == '<' ||
          CC_LA_CH_VALUE == '&' ||
          CC_LA_CH_VALUE == '|') {

         cc_get_token(FALSE);
      }
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_add_opnd", NULL);

   return(parsed_ok);
} /* parse_cc_add_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     [add-op] add-opnd {add-op add-opnd}                           *|
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

static boolean parse_cc_level_2(opnd_type *result)

{
   int       ir_idx = NULL_IDX;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_level_2", NULL);

   parsed_ok = parse_cc_add_opnd(&opnd);

   if (ir_idx) {
      COPY_OPND(IR_OPND_L(ir_idx), opnd);
      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Add ||
          TOKEN_VALUE(cc_token) == Cc_Tok_Op_Sub) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(cc_token)) {
         case Cc_Tok_Op_Add :
            IR_OPR(ir_idx) = Plus_Opr;
            break;
         case Cc_Tok_Op_Sub :
            IR_OPR(ir_idx) = Minus_Opr;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_add_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_level_2", NULL);

   return(parsed_ok);
} /* parse_cc_level_2 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     shift-opnd {shift-op shift-opnd}                              *|
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

static boolean parse_cc_level_3(opnd_type *result)

{
   int       ir_idx = NULL_IDX;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_level_3", NULL);

   parsed_ok = parse_cc_level_2(&opnd);

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Lshift ||
          TOKEN_VALUE(cc_token) == Cc_Tok_Op_Rshift) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(cc_token)) {
         case Cc_Tok_Op_Lshift :
            IR_OPR(ir_idx) = Shiftl_Opr;
            break;
         case Cc_Tok_Op_Rshift :
            IR_OPR(ir_idx) = Shiftr_Opr;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_level_2(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_level_3", NULL);

   return(parsed_ok);
} /* parse_cc_level_3 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     [level-2-expr rel-op] level-2-expr                            *|
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

static boolean parse_cc_level_4(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_level_4", NULL);

   parsed_ok = parse_cc_level_3(&opnd);

   if (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Ge ||
       TOKEN_VALUE(cc_token) == Cc_Tok_Op_Gt ||
       TOKEN_VALUE(cc_token) == Cc_Tok_Op_Le ||
       TOKEN_VALUE(cc_token) == Cc_Tok_Op_Lt) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(cc_token)) {
         case Cc_Tok_Op_Ge :
            IR_OPR(ir_idx) = Ge_Opr;
            break;
         case Cc_Tok_Op_Gt :
            IR_OPR(ir_idx) = Gt_Opr;
            break;
         case Cc_Tok_Op_Le :
            IR_OPR(ir_idx) = Le_Opr;
            break;
         case Cc_Tok_Op_Lt :
            IR_OPR(ir_idx) = Lt_Opr;
            break;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_level_3(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_level_4", NULL);

   return(parsed_ok);
} /* parse_cc_level_4 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     [level-4-expr rel-op] level-4-expr                            *|
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

static boolean parse_cc_level_5(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_level_5", NULL);

   parsed_ok = parse_cc_level_4(&opnd);

   if (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Eq ||
       TOKEN_VALUE(cc_token) == Cc_Tok_Op_Ne) {

      NTR_IR_TBL(ir_idx);
      switch (TOKEN_VALUE(cc_token)) {
         case Cc_Tok_Op_Eq :
            IR_OPR(ir_idx) = Eq_Opr;
            break;
         case Cc_Tok_Op_Ne :
            IR_OPR(ir_idx) = Ne_Opr;
            break;
      }
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_level_4(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_level_5", NULL);

   return(parsed_ok);
} /* parse_cc_level_5 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     and_opnd { and_op and_opnd }                                  *|
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

static boolean parse_cc_or_opnd(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_or_opnd", NULL);

   parsed_ok = parse_cc_level_5(&opnd);

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Band) {

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Band_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_level_5(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_or_opnd", NULL);

   return(parsed_ok);
} /* parse_cc_or_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     equiv-opnd { equiv-op equiv-opnd }                            *|
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

static boolean parse_cc_level_6(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_level_6", NULL);

   parsed_ok = parse_cc_or_opnd(&opnd);

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Neqv) {

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Neqv_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_equiv_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_level_6", NULL);

   return(parsed_ok);
} /* parse_cc_level_6 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     or_opnd { or_op or_opnd }                                     *|
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

static boolean parse_cc_equiv_opnd(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_equiv_opnd", NULL);

   parsed_ok = parse_cc_level_6(&opnd);

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Bor) {

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Bor_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_level_6(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_equiv_opnd", NULL);

   return(parsed_ok);
} /* parse_cc_equiv_opnd */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     and_opnd { and_op and_opnd }                                  *|
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

static boolean parse_cc_level_7(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_level_7", NULL);

   parsed_ok = parse_cc_equiv_opnd(&opnd);

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_And) {

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = And_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_equiv_opnd(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_level_7", NULL);

   return(parsed_ok);
} /* parse_cc_level_7 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BNF     or_opnd { or_op or_opnd }                                     *|
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

static boolean parse_cc_level_8(opnd_type *result)

{
   int       ir_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_cc_level_8", NULL);

   parsed_ok = parse_cc_level_7(&opnd);

   while (TOKEN_VALUE(cc_token) == Cc_Tok_Op_Or) {

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Or_Opr;
      IR_LINE_NUM(ir_idx) = TOKEN_LINE(cc_token);
      IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(cc_token);

      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      parsed_ok = parse_cc_level_7(&opnd) && parsed_ok;

      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      OPND_FLD(opnd) = IR_Tbl_Idx;
      OPND_IDX(opnd) = ir_idx;
   }

   COPY_OPND((*result), opnd);

   TRACE (Func_Exit, "parse_cc_level_8", NULL);

   return(parsed_ok);
} /* parse_cc_level_8 */

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
static boolean parse_cc_operand (opnd_type *the_opnd)

{
   int			attr_idx;
   int                  col;
   int                  ir_idx;
   int                  line;
   int			name_idx;
   int			paren_cnt = 0;
   boolean              parsed_ok       = TRUE;


   TRACE (Func_Entry, "parse_cc_operand", NULL);

   if (CC_LA_CH_VALUE == '(' ) {

      line = CC_LA_CH_LINE;
      col  = CC_LA_CH_COLUMN;

      CC_NEXT_LA_CH;  /* swallow ( */

      if (!parse_cc_expr(the_opnd)) {
         parsed_ok = FALSE;
      }
      else if (CC_LA_CH_VALUE == ')') {
         /* insert paren_opr */
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Paren_Opr;
         COPY_OPND(IR_OPND_L(ir_idx), (*the_opnd));
         OPND_FLD((*the_opnd)) = IR_Tbl_Idx;
         OPND_IDX((*the_opnd)) = ir_idx;
         IR_LINE_NUM(ir_idx)   = line;
         IR_COL_NUM(ir_idx)    = col;

         CC_NEXT_LA_CH;		/* swallow ) */
         goto EXIT;
      }
      else {
         /* unmatched paranthesis */
         ntr_next_msg_queue(CC_LA_CH_LINE, 1165, Error,
                            CC_LA_CH_COLUMN,
                            ")",
                            0,
                            STR_ARG);
         parsed_ok = FALSE;
         flush_cc_line();
      }
      goto EXIT;
   }
   else if (CC_LA_CH_CLASS == Ch_Class_Digit  ||
            CC_LA_CH_CLASS == Ch_Class_Letter ||
            CC_LA_CH_VALUE == underscore) {

      cc_get_token(TRUE);

      OPND_LINE_NUM((*the_opnd)) = TOKEN_LINE(cc_token);
      OPND_COL_NUM((*the_opnd))  = TOKEN_COLUMN(cc_token);
      OPND_FLD((*the_opnd))      = CN_Tbl_Idx;

      switch (TOKEN_VALUE(cc_token)) {

         case Cc_Tok_Id :
         case Cc_Tok_Kwd_Define :
         case Cc_Tok_Kwd_Elif :
         case Cc_Tok_Kwd_Else :
         case Cc_Tok_Kwd_Endif :
         case Cc_Tok_Kwd_If :
         case Cc_Tok_Kwd_Ifdef :
         case Cc_Tok_Kwd_Ifndef :
         case Cc_Tok_Kwd_Include :
         case Cc_Tok_Kwd_Undef :

            /* all of these are identifiers. They can't be defined or */
            /* the scan would have replaced them already. Replace them */
            /* with a 0. */

            attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                       TOKEN_LEN(cc_token),
                                       &name_idx);

            if (attr_idx != NULL_IDX &&
                CC_AT_DEFINED(attr_idx)) {

               PRINTMSG(TOKEN_LINE(cc_token), 626, Internal, 
                        TOKEN_COLUMN(cc_token),
                        "defined macro to be replaced", "parse_cc_operand");

            }

            OPND_IDX((*the_opnd)) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 0);
            break;

         case Cc_Tok_Constant :

            OPND_IDX((*the_opnd)) = TOKEN_CONST_TBL_IDX(cc_token);
            break;

         case Cc_Tok_Kwd_Defined :

            while (CC_LA_CH_VALUE == '(') {
               paren_cnt++;
               CC_NEXT_LA_CH;
            }

            cc_get_token(FALSE);

            if (TOKEN_VALUE(cc_token) != Tok_Id) {
               ntr_next_msg_queue(TOKEN_LINE(cc_token), 1165, Error,
                                  TOKEN_COLUMN(cc_token),
                                  "IDENTIFIER",
                                  0,
                                  STR_ARG);
               parsed_ok = FALSE;
               flush_cc_line();
            }
            else {

               attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                          TOKEN_LEN(cc_token),
                                          &name_idx);

 
               if (attr_idx != NULL_IDX &&
                   CC_AT_DEFINED(attr_idx)) {

                  /* defined, replace with 1 */
                  OPND_IDX((*the_opnd)) = CN_INTEGER_ONE_IDX;
               }
               else {
                  /* else, replace with 0 */
                  OPND_IDX((*the_opnd)) = CN_INTEGER_ZERO_IDX;
               }
            }

            while (paren_cnt) {
               if (CC_LA_CH_VALUE != ')') {
                  ntr_next_msg_queue(CC_LA_CH_LINE, 1165, Error,
                                     CC_LA_CH_COLUMN,
                                     ")",
                                     0,
                                     STR_ARG);
                  parsed_ok = FALSE;
                  flush_cc_line();
                  break;
               }
               else {
                  CC_NEXT_LA_CH;
                  paren_cnt--;
               }
            }

            break;
      }
   }
   else {
      ntr_next_msg_queue(CC_LA_CH_LINE, 1165, Error,
                         CC_LA_CH_COLUMN,
                         "identifier or integer constant",
                         0,
                         STR_ARG);
      parsed_ok = FALSE;
      flush_cc_line();
   }

EXIT:
   TRACE (Func_Exit, "parse_cc_operand", NULL);

   return(parsed_ok);

}  /* parse_cc_operand */

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

static int srch_cc_sym_tbl (char  *name_str,
                            int    name_len,
                            int   *name_idx)

{
   int       idx;
   long      tst_val;


   TRACE (Func_Entry, "srch_cc_sym_tbl", name_str);

   tst_val = srch_name_tbl(name_str,
                           name_len,
                           &idx,
                           cc_ln_tbl,
                           str_pool,
                           cc_ln_fw_idx,
                           cc_ln_lw_idx);

   *name_idx = idx;

   if (tst_val != 0) {
      idx = NULL_IDX;
      TRACE (Func_Exit, "srch_cc_sym_tbl", NULL);
   }
   else {
      TRACE (Func_Exit, "srch_cc_sym_tbl", CC_LN_NAME_PTR(idx));
      idx = CC_LN_ATTR_IDX(idx);
   }
   return (idx);

}  /* srch_cc_sym_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ntr_cc_sym_tbl adds the token name to the the name pool, links it     *|
|*      to an attribute table entry through the local name table, and         *|
|*      reserves an attribute table entry for the identifier or label.        *|
|*      The attribute table entry field name_idx is linked to the name in     *|
|*      the name pool.                                                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      token                   token containing identifier or label and      *|
|*                              length of name to be added to symbol table    *|
|*                                                                            *|
|*      name_idx                local name table index where entry is to      *|
|*                              be inserted                                   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      attribute table index of reserved entry                               *|
|*                                                                            *|
\******************************************************************************/

static int ntr_cc_sym_tbl(cc_token_type *token,
                          int            name_idx)

{
   register int          attr_idx;
   register int          i;
   register int          np_idx;

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   register long        *name_tbl_base; /* name table base address */
# endif


   TRACE (Func_Entry, "ntr_cc_sym_tbl", TOKEN_STR(*token));

   TBL_REALLOC_CK(cc_ln_tbl, 1);             /* add local name table entry */

   NTR_CC_NAME_POOL((long *) TOKEN_STR(*token), TOKEN_LEN(*token), np_idx);

   /* reserve attribute table entry and fill in common definition fields */

   NTR_CC_ATTR_TBL(attr_idx);
   CC_AT_NAME_LEN(attr_idx)        = TOKEN_LEN(*token);
   CC_AT_NAME_IDX(attr_idx)        = np_idx;

   cc_ln_lw_idx = cc_ln_tbl_idx;

   /* Enter name in correct position.  Link name pool and attribute table */


# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)

   name_tbl_base = (long *) cc_ln_tbl;

#  pragma _CRI ivdep

   for (i = cc_ln_tbl_idx; i >= name_idx; i--) {
      name_tbl_base [i] = name_tbl_base [i-1];
   }
# else

#  pragma _CRI ivdep

   for (i = cc_ln_tbl_idx; i >= name_idx; i--) {
      cc_ln_tbl [i]  = cc_ln_tbl [i-1];
   }
# endif

   CLEAR_TBL_NTRY(cc_ln_tbl, name_idx);
   CC_LN_ATTR_IDX(name_idx)        = attr_idx;
   CC_LN_NAME_IDX(name_idx)        = np_idx;
   CC_LN_NAME_LEN(name_idx)        = TOKEN_LEN(*token);

   TRACE (Func_Exit, "ntr_cc_sym_tbl", TOKEN_STR(*token));

   return (attr_idx);

}  /* ntr_cc_sym_tbl */

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

static void flush_cc_line(void)

{
   int		col;
   int		line;

   TRACE (Func_Entry, "flush_cc_line", NULL);

   cc_stmt_buf_idx = cc_stmt_buf_len;

   find_line_and_col(cc_stmt_buf_idx, &line, &col);
   CC_LA_CH_VALUE  = eos;
   CC_LA_CH_LINE   = line;
   CC_LA_CH_COLUMN = col;
   CC_LA_CH_CLASS  = ch_class[CC_LA_CH_VALUE];

   cc_get_token(FALSE);

   TRACE (Func_Exit, "flush_cc_line", NULL);

   return;

}  /* flush_cc_line */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Handle the command line -D option by putting the define in the table. *|
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

boolean	enter_cmd_line_cc_define(char		*str,
				 char		*value,
				 boolean	is_define)

{
   int		attr_idx;
   int		idx;
   int		name_idx;
   boolean	ok = TRUE;
   int		tidx;

   TRACE (Func_Entry, "enter_cmd_line_cc_define", NULL);

   idx = 0;
   while (str[idx] == ' ' || str[idx] == '\t') {
      idx++;
   }

   tidx = 0;

   if (ch_class[str[idx]] == Ch_Class_Letter ||
       str[idx] == underscore) {

      cc_token = cc_initial_token;

      while (ch_class[str[idx]] == Ch_Class_Letter ||
             ch_class[str[idx]] == Ch_Class_Digit ||
             str[idx] == underscore ||
             str[idx] == dollar ||
             str[idx] == at_sign) {

         (TOKEN_STR(cc_token))[tidx] = str[idx];
         tidx++;
         idx++;
      }

      TOKEN_LEN(cc_token) = tidx;

      attr_idx = srch_cc_sym_tbl(TOKEN_STR(cc_token),
                                 TOKEN_LEN(cc_token),
                                 &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx = ntr_cc_sym_tbl(&cc_token, name_idx);
      }

      if (is_define) {
         CC_AT_DEFINED(attr_idx) = TRUE;

         if (value != NULL) {
            strcpy(temp_id_str.string, value);
            CC_AT_STR_LEN(attr_idx) = strlen(temp_id_str.string);
            NTR_CC_NAME_POOL(temp_id_str.words, CC_AT_STR_LEN(attr_idx), 
                             CC_AT_STR_IDX(attr_idx));
         }
         else {
            PUT_VALUE_IN_AT_STR(attr_idx, 1);
         }
      }
      else {
         CC_AT_DEFINED(attr_idx) = FALSE;
      }
   }
   else {
      ok = FALSE;
   }
   

   TRACE (Func_Exit, "enter_cmd_line_cc_define", NULL);

   return(ok);

}  /* enter_cmd_line_cc_define */

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

static int fold_cc_expr(opnd_type	*opnd)

{
   int		l_value;
   int		ir_idx;
   opnd_type	loc_opnd;
#ifdef KEY /* Bug 10177 */
   int		r_value = 0;
   long		result = 0;
#else /* KEY Bug 10177 */
   int		r_value;
   long		result;
#endif /* KEY Bug 10177 */

   TRACE (Func_Entry, "fold_cc_expr", NULL);

   switch (OPND_FLD((*opnd))) {
      case CN_Tbl_Idx :
         result = (long) CN_INT_TO_C(OPND_IDX((*opnd)));
         break;

      case IR_Tbl_Idx :
         ir_idx = OPND_IDX((*opnd));

         COPY_OPND(loc_opnd, IR_OPND_L(ir_idx));

         l_value = fold_cc_expr(&loc_opnd);

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(loc_opnd, IR_OPND_R(ir_idx));

            r_value = fold_cc_expr(&loc_opnd);
         }

         switch (IR_OPR(ir_idx)) {
            case Uplus_Opr :
               result = l_value;
               break;

            case Uminus_Opr :
               result = -l_value;
               break;

            case Plus_Opr :
               result = l_value + r_value;
               break;

            case Minus_Opr :
               result = l_value - r_value;
               break;

            case Div_Opr :
               result = l_value / r_value;
               break;

            case Mod_Opr :
               result = l_value % r_value;
               break;

            case Mult_Opr :
               result = l_value * r_value;
               break;

            case Not_Opr :
               result = (l_value != 0 ? 0 : 1);
               break;

            case Bnot_Opr :
               result = ~l_value;
               break;

            case Ne_Opr :
               result = (l_value != r_value ? 1 : 0);
               break;

            case Eq_Opr : 
               result = (l_value == r_value ? 1 : 0);
               break;

            case Lt_Opr :
               result = (l_value < r_value ? 1 : 0);
               break;

            case Le_Opr :
               result = (l_value <= r_value ? 1 : 0);
               break;

            case Gt_Opr :
               result = (l_value > r_value ? 1 : 0);
               break;

            case Ge_Opr :
               result = (l_value >= r_value ? 1 : 0);
               break;

            case Neqv_Opr :
               result = l_value ^ r_value;
               break;

            case And_Opr :
               result = (l_value && r_value ? 1 : 0);
               break;

            case Band_Opr :
               result = l_value & r_value;
               break;

            case Or_Opr :
               result = (l_value || r_value ? 1 : 0);
               break;

            case Bor_Opr :
               result = l_value | r_value;
               break;

            case Shiftl_Opr :
               result = l_value << r_value;
               break;

            case Shiftr_Opr :
               result = l_value >> r_value;
               break;

            case Paren_Opr :
               result = l_value;
               break;

            default:
               PRINTMSG(IR_LINE_NUM(ir_idx), 1164, Internal, 
                        IR_COL_NUM(ir_idx), "parse_cc_line");
               break;
         }
         break;

      default:
         PRINTMSG(OPND_LINE_NUM((*opnd)), 1164, Internal, 
                  OPND_COL_NUM((*opnd)), "parse_cc_line");        
         break;
   }

   TRACE (Func_Exit, "fold_cc_expr", NULL);

   return(result);

}  /* fold_cc_expr */

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

static void find_line_and_col(int	idx,
			      int	*line,
			      int	*col)

{
   int		i = 1;

   TRACE (Func_Entry,"find_line_and_col" , NULL);

   if (cc_stmt_buf_num_lines == 1) {
      *line = cc_stmt_buf_line[1].line;
      *col = idx;
   }
   else {
      for (i = 2; i <= cc_stmt_buf_num_lines+1; i++) {
         if (cc_stmt_buf_line[i].start_idx >= idx) {
            *line = cc_stmt_buf_line[i-1].line;
            *col = idx - cc_stmt_buf_line[i-1].start_idx;
            break;
         }
      }
# ifdef _DEBUG
      if (i > cc_stmt_buf_num_lines + 1) {
         PRINTMSG(PP_LINE_NUM, 626, Internal, 1,
                  "valid line num", "find_line_and_col");
      }
# endif
   }


   TRACE (Func_Exit, "find_line_and_col", NULL);

   return;

}  /* find_line_and_col */

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

static void shift_cc_stmt_buf(int	start_idx,
                              int	shift)

{
   int		i;

   TRACE (Func_Entry, "shift_cc_stmt_buf", NULL);

   if (shift > 0) {
      for (i = cc_stmt_buf_len+1; i >= start_idx; i--) {
         cc_stmt_buf[i+shift] = cc_stmt_buf[i];
      }
   }
   else if (shift < 0) {
      for (i = start_idx; i <= cc_stmt_buf_len+1; i++) {
         cc_stmt_buf[i+shift] = cc_stmt_buf[i];
      }
   }

   for (i = 1; i <= cc_stmt_buf_num_lines + 1; i++) {
      if (cc_stmt_buf_line[i].start_idx >= start_idx) {
         cc_stmt_buf_line[i].start_idx += shift;
      }
   }

   cc_stmt_buf_len += shift;

   TRACE (Func_Exit, "shift_cc_stmt_buf", NULL);

   return;

}  /* shift_cc_stmt_buf */

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

static void shift_nxt_line(int       start_idx,
                           int       shift)

{
   int		end_line;
   int          i;
   int		curr_line = NULL_IDX;

   TRACE (Func_Entry, "shift_nxt_line", NULL);

   for (i = 1; i <= nxt_line_num_lines; i++) {
      if (start_idx >= nxt_line_start_idx[i] &&
          start_idx <= nxt_line_end_idx[i]) {
         curr_line = i;
         break;
      }
   }

# ifdef _DEBUG
   if (start_idx == NULL_IDX) {
      PRINTMSG(pp_nxt_line_num[1], 626, Internal, 1,
               "valid start_idx", "shift_nxt_line");
   }
# endif

   if (extra_nxt_line) {
      end_line = extra_nxt_line;
   }
   else {
      end_line = nxt_line_num_lines;
   }

   if (nxt_line_end_idx[end_line]+shift >= MAX_STMT_CHAR_SIZE) {
      PRINTMSG(pp_nxt_line_num[end_line], 1593, Limit, 0);
   }

   if (shift > 0) {
      for (i = nxt_line_end_idx[end_line]; i >= start_idx; i--) {
         nxt_line[i+shift] = nxt_line[i];
         nxt_line_col[i+shift] = nxt_line_col[i];
      }
   }
   else if (shift < 0) {
      for (i = start_idx; i <= nxt_line_end_idx[end_line]; i++) {
         nxt_line[i+shift] = nxt_line[i];
         nxt_line_col[i+shift] = nxt_line_col[i];
      }
   }

   nxt_line_end_idx[curr_line] += shift;
   pp_nxt_line_length[curr_line] += shift;
   pp_nxt_line_EOL[curr_line] += shift;
   pp_orig_line_size[curr_line] += shift;

   for (i = curr_line + 1; i <= end_line; i++) {
      nxt_line_start_idx[i] += shift;
      nxt_line_end_idx[i] += shift;
      pp_nxt_line_EOL[i] += shift;
   }

   TRACE (Func_Exit, "shift_nxt_line", NULL);

   return;

}  /* shift_nxt_line */

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

static int expanded_macro_len(int		attr_idx,
                              cc_arg_type	*arg_head)

{
   int		arg_num;
   cc_arg_type	*arg_tail;
   int		i;
   int		len;

   TRACE (Func_Entry, "expanded_macro_len", NULL);

   len = CC_AT_STR_LEN(attr_idx);

   for (i = 0; i < CC_AT_STR_LEN(attr_idx); i++) {
      if ((CC_AT_STR_PTR(attr_idx))[i] == '\001') {
         i += 2;
         arg_num = 1;
         arg_tail = arg_head;
         while (arg_num != (CC_AT_STR_PTR(attr_idx))[i]) {
            arg_tail = arg_tail->next;
            arg_num++;
         }

        len += (arg_tail->name_len - 3);
      }
   }

   TRACE (Func_Exit, "expanded_macro_len", NULL);

   return(len);

}  /* expanded_macro_len */

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

static void insert_macro(int		attr_idx,
                         cc_arg_type	*arg_head,
                         int		id_start_idx,
                         int		col,
			 int		save_pp_line_idx)

{
   int		arg_num;
   cc_arg_type  *arg_tail;
   char		*char_ptr;
   int		i;
   int		j;
   int		k;
   int		len;
   int		end_idx;

   TRACE (Func_Entry, "insert_macro", NULL);

   /* cc_stmt_buf_idx is the end of replacement ')' */
   /* id_start_idx is the start of replacement */

   end_idx = cc_stmt_buf_idx + 1;

   adjust_continued_macro(id_start_idx, &end_idx);

   len = expanded_macro_len(attr_idx, arg_head);

   shift_nxt_line(end_idx,
                  len-(end_idx - id_start_idx));

   char_ptr = CC_AT_STR_PTR(attr_idx);

   j = 0;
   i = 0;
   while (i < len) {
   if (char_ptr[j] == '\001') {
         j += 2;
         arg_num = 1;
         arg_tail = arg_head;
         while (arg_num != char_ptr[j]) {
            arg_tail = arg_tail->next;
            arg_num++;
         }

         for (k = 0; k < arg_tail->name_len; k++) {
            nxt_line[i+id_start_idx] = arg_tail->name[k];
            nxt_line_col[i+id_start_idx] = col;
            i++;
         }
      }
      else {
         nxt_line[i+id_start_idx] = char_ptr[j];
         nxt_line_col[i+id_start_idx] = col;
         i++;
      }

      j++;
   }

   cc_stmt_buf_idx = id_start_idx + len - 1;
   pp_line_idx = save_pp_line_idx;
   cc_advance_idx();


   TRACE (Func_Exit, "insert_macro", NULL);

   return;

}  /* insert_macro */

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

static void adjust_continued_macro(int	start_idx,
				   int	*end_idx)

{
   int		start_line = NULL_IDX;
   int		end_line = NULL_IDX;
   int		i;
   boolean	keep_end_line = FALSE;

   TRACE (Func_Entry, "adjust_continued_macro", NULL);

   for (i = 1; i <= nxt_line_num_lines; i++) {
      if (start_idx >= nxt_line_start_idx[i] &&
          start_idx <= nxt_line_end_idx[i]) {

         start_line = i;
      }

      if (*end_idx >= nxt_line_start_idx[i] &&
          *end_idx <= nxt_line_end_idx[i]) {

         end_line = i;
      }

      if (start_line != NULL_IDX && end_line != NULL_IDX) {
         break;
      }
   }

   if (end_line > start_line) {
      /* have continuation */

      for (i = *end_idx; i < pp_nxt_line_EOL[end_line]; i++) {
         if (nxt_line[i] != blank &&
             nxt_line[i] != tab) {
            keep_end_line = TRUE;
            break;
         }
      }
             
      if (keep_end_line) {
         /* blank out the end_line from nxt_line_idx to end_idx - 1 */

         for (i = pp_nxt_line_idx[end_line]+1; i < *end_idx; i++) {
            nxt_line[i] = blank;
         }

         /* change end_line to the previous line */

         end_line--;
      }

      *end_idx = pp_nxt_line_EOL[end_line];

      /* see if new end_line is still greater than start_line */

      if (end_line > start_line) {
         pp_orig_line_size[start_line] += nxt_line_end_idx[end_line] -
                               nxt_line_end_idx[start_line];
         nxt_line_end_idx[start_line] = nxt_line_end_idx[end_line];
         pp_nxt_line_EOL[start_line] = pp_nxt_line_EOL[end_line];
         pp_nxt_line_length[start_line] = nxt_line_end_idx[start_line] - 
                                 nxt_line_start_idx[start_line];

         for (i = end_line + 1; 
              i <= (extra_nxt_line ? extra_nxt_line : end_stmt_line_idx); 
              i++) {
            nxt_line_start_idx[i-(end_line-start_line)] = nxt_line_start_idx[i];
            nxt_line_end_idx[i-(end_line-start_line)] = nxt_line_end_idx[i];
            pp_orig_line_size[i-(end_line-start_line)] = pp_orig_line_size[i];
            pp_nxt_line_EOL[i-(end_line-start_line)] = pp_nxt_line_EOL[i];
            pp_nxt_line_length[i-(end_line-start_line)] = pp_nxt_line_length[i];
            pp_nxt_line_idx[i-(end_line-start_line)] = pp_nxt_line_idx[i];
            pp_nxt_line_type[i-(end_line-start_line)] = pp_nxt_line_type[i];
            pp_nxt_line_num[i-(end_line-start_line)] = pp_nxt_line_num[i];
            pp_change_source_form[i-(end_line-start_line)] = 
                                      pp_change_source_form[i];
            pp_expected_line[i-(end_line-start_line)] = 
                                      pp_expected_line[i];
         }

         nxt_line_num_lines -= (end_line - start_line);
         end_stmt_line_idx -= (end_line - start_line);

         if (extra_nxt_line != NULL_IDX) {
            extra_nxt_line -= (end_line - start_line);
         }
      }
   }

   TRACE (Func_Exit, "adjust_continued_macro", NULL);

   return;

}  /* adjust_continued_macro */

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

static char *get_dynamic_predef_str(int	attr_idx,
                                    int	line)

{
   time_t	clock;
   int		file_line;
   int		g_idx;
   struct tm	*tmptr;

   static char		str[MAX_PATH_NAME_SIZE + 1];

   TRACE (Func_Entry, "get_dynamic_predef_str", NULL);

   str[0] = '\0';

   if (attr_idx == line_macro_idx || attr_idx == LINE_macro_idx) {
      GLOBAL_LINE_TO_FILE_LINE(line, g_idx, file_line);
      sprintf(str, "%d", file_line);
   }
   else if (attr_idx == file_macro_idx || attr_idx == FILE_macro_idx) {
      get_curr_file_name(str);
   }
   else if (attr_idx == date_macro_idx || attr_idx == DATE_macro_idx) {
      clock = time(NULL);
      tmptr = localtime(&clock);
      sprintf(str, "\"%02d/%02d/%02d\"",
              tmptr->tm_mon+1, tmptr->tm_mday,
              (tmptr->tm_year % 100));

   }
   else if (attr_idx == time_macro_idx || attr_idx == TIME_macro_idx) {
      clock = time(NULL);
      tmptr = localtime(&clock);
      sprintf(str, "\"%02d:%02d:%02d\"",
              tmptr->tm_hour, tmptr->tm_min, tmptr->tm_sec);
   }

   TRACE (Func_Exit, "get_dynamic_predef_str", NULL);

   return(str);

}  /* get_dynamic_predef_str */
