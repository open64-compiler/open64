/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/src_input.c	5.5	10/20/99 17:17:46\n";


#ifdef KEY /* Bug 4719 */
#include <errno.h>
#endif /* KEY Bug 4719 */

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "src_input.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "src_input.h"

#ifdef KEY /* Bug 4719 */
/* Sigh. Including ../sgi/sgi_cmd_line.h is hopeless. */
extern char *preprocessor_output_file;
#endif /* KEY /* Bug 4719 */


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static boolean	is_pound_line_dir(void);
static void	fixed_classify_line (void);
static void	fixed_get_stmt (void);
boolean		read_line (boolean);
static void	free_classify_line (void);
static void	free_get_stmt (void);
void		ntr_next_msg_queue(int,int,msg_severities_type,
                                   int,char *, long,int);
static void     move_up_next_msg_queue(void);
#ifdef KEY /* Bug 10151 */
static boolean	open_include_file (boolean, boolean);
#else /* KEY Bug 10151 */
static boolean	open_include_file (boolean);
#endif /* KEY Bug 10151 */
static void	update_global_line (void);
static int      whats_after_paren_group(int *, int *, int);
static int      whats_after_brkt_group(int *, int *, int);
static void	print_nxt_line(void);
static void	classify_line(void);
static boolean	get_nxt_line(void);
static void	pp_get_stmt (void);
static void	shift_to_line_size(int);


# pragma _CRI align fixed_get_char
# pragma _CRI inline scan_thru_close_paren

static boolean	issue_classify_msg = TRUE;

# ifdef _DEBUG

static void	print_stmt (void);
static void	print_src (void);



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Print_stmt prints the source input to stderr.                         *|
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
|* Note:  This routine has been superceded by the print routine in miflib     *|
|*        called CIF_postprocessor (for debugging the front-end, we still     *|
|*        use print_buffered_messages).  print_stmt has been left here in     *|
|*        case we need to go back to this method for any reason.  It only     *|
|*        exists in debug compilers anyway.				      *|
|*     									      *|
\******************************************************************************/

static void print_stmt()

{
   int   idx;
   int   line;
   int   ich2;


   for (line = 1; line <= lines_in_buf; line++) {

      for (idx = 0;
           (((ich2 = stmt_buf[idx + stmt_line_start_idx[line]]) != newline)
                   && (ich2 != eos));
           idx++) {
         fprintf(stderr, "%c", ich2);
      }
      fprintf(stderr, "\n");
   }

   return;

}  /* print_stmt */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Print_src is an expanded version of print_stmt that dumps the source  *|
|*	input.  This prints the source form and marks character constants.    *|
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
static void print_src()
{
   int   idx;
   int   line;

   if (source_form == Fixed_Form) {
      fprintf(stderr, "Fixed_Form\n");
   }
   else {
      fprintf(stderr, "Free_Form\n");
   }

   for (line = 1; line <= lines_in_buf; line++) {

       fprintf(stderr, "global line num = %d\n", stmt_line_num[line]);
       for (idx = stmt_line_start_idx[line]; idx < stmt_line_end_idx[line];
            idx++) {

          if (stmt_buf[idx] < 0) {
             fprintf(stderr, "^%c", stmt_buf[idx]);
          }
          else {
             fprintf(stderr, "%c", stmt_buf[idx]);
          }
       }
       fprintf(stderr, "\n");
   }
   fprintf(stderr, "*******************************************************\n");

   return;

}  /* print_src */

# endif


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Init_src is called by compiler initialization in main to open the     *|
|*	source input file and initialize the data structures used by this     *|
|*	module.								      *|
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

void init_src_input (void)

{
   TRACE (Func_Entry, "init_src_input", NULL);

   dot_i_fptr = stdout; /* default is stdout */

#ifdef KEY /* Bug 4719 */
   char *filename = 0;
#endif /* KEY Bug 4719 */
   if (on_off_flags.save_dot_i) {
      filename = dot_i_file;
   }
#ifdef KEY /* Bug 4719 */
   else if (preprocessor_output_file) {
      filename = preprocessor_output_file;
   }
   if (filename) {
     dot_i_fptr = fopen(filename, "w");
     if (!dot_i_fptr) {
       PRINTMSG(0, 1676, Log_Error, 0, filename, strerror(errno));
       exit_compiler (RC_USER_ERROR);
     }
   }
#endif /* KEY Bug 4719 */

   previous_global_line = 0;

   /* allocate memory for src_stk and set src_stk_idx to 1 */

   CHECK_INITIAL_ALLOC(src_stk, 1);

   /* This field will get updated by update_global_line. */

   SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)	= NULL_IDX;
   SRC_STK_PREV_SRC_FORM(src_stk_idx)	= source_form;  /* Init to current */

   /* initialize base entry in src_stk with source file info */

   if (src_file[0] == EOS) {				/* src file is stdin  */
      SRC_STK_FILE_LINE(SRC_STK_BASE_IDX) = 0;
      SRC_STK_FILE_TYPE(SRC_STK_BASE_IDX) = Stdin_Src;
      SRC_STK_FILE_PTR(SRC_STK_BASE_IDX)  = stdin;
      strcpy (src_file, "'stdin'");			/* use 'stdin' as name*/
   }
   else {
      SRC_STK_FILE_LINE(SRC_STK_BASE_IDX) = 0;
      SRC_STK_FILE_TYPE(SRC_STK_BASE_IDX) = Input_Src;

      SRC_STK_FILE_PTR(SRC_STK_BASE_IDX) = fopen (src_file, "r");

      if (SRC_STK_FILE_PTR(SRC_STK_BASE_IDX) == NULL) {
	 PRINTMSG (0, 49, Log_Error, 0, src_file);	/* Can't open src file*/
	 exit_compiler (RC_USER_ERROR);
      }
   }

   if (on_off_flags.output_pound_lines &&
       (on_off_flags.preprocess_only || on_off_flags.save_dot_i)) {
        fprintf(dot_i_fptr, "# 1 \"%s\"\n", src_file);
   }

   /* Make relative path for source file absolute.                            */

   if (src_file[0] != SLASH)
   {
      getcwd (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), MAX_FILE_NAME_SIZE);
      /* Mac getcwd supplies separator */
      strcat (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), "/");
      SRC_STK_FILE_IDX(SRC_STK_BASE_IDX) =
                               strlen(SRC_STK_PATH_NAME(SRC_STK_BASE_IDX));
      strcat (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), src_file);
   }
   else {
      SRC_STK_FILE_IDX(SRC_STK_BASE_IDX) = 0;
      strcpy (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), src_file);
   }

   /* Initialize base entry of global_line_tbl with first line of source file.*/

   update_global_line();
   
   /* Set the line numbers in this entry correctly.			      */
   /* Always set GL_CIF_FILE_ID; it's needed for buffered message output.     */

   GL_GLOBAL_LINE(global_line_tbl_idx)	= 1;
   GL_FILE_LINE(global_line_tbl_idx)	= 1;
   
   /* Initialize idx to start of next stmt, after ;                           */

   starting_pt = NULL_IDX;

   /* Flag controls Ansi msg for line over 72 chars so it is issued once.     */

   have_issued_msg_37		= FALSE;

   havent_issued_tab_ansi	= TRUE;
   havent_issued_at_ansi	= TRUE;
   havent_issued_dollar_ansi	= TRUE;

   nxt_line_start_idx[0] = 0;
   nxt_line_end_idx[0] = 0;
   nxt_line_col[0] = 0;
   nxt_line_num_lines = 1;
   pp_line_idx = 1;

   pp_nxt_line_type[0] = Regular_Line;

   /* Manufacture dummy 0th line of fixed source file in nxt_line buffer.     */

   nxt_line_start_idx[1] = 1;
   nxt_line_end_idx[1] = 2;
   nxt_line[0]     = blank;
   nxt_line[1]     = newline;
   nxt_line[2]     = eos;
   nxt_line_idx    = 1;
   nxt_line_num    = 0;
   nxt_line_type   = Comment_Line;
   PP_LINE_TYPE    = Comment_Line;
   PP_EOL          = 1;

   if (source_form == Fixed_Form) {
      /* set source line size from command line */
      if (cmd_line_flags.line_size_80) {
         line_size = FIXED_SRC_LINE_SIZE_80;
      }
      else if (cmd_line_flags.line_size_132) {
         line_size = FIXED_SRC_LINE_SIZE_132;
      }
      else {
         line_size = FIXED_SRC_LINE_SIZE_72;
      }

      if (! on_off_flags.preprocess_only) {
         fixed_get_stmt ();
      }
   }
   else {
      /* set source line size to standard free form length */
      line_size = FREE_SRC_LINE_SIZE;
      if (! on_off_flags.preprocess_only) {
         expected_line = Regular_Line;
         free_get_stmt ();
      }
   }

   if (change_orig_src_file) {
      /* reset the file name in the src stk and update global line again. */

      if (pound_file[0] != SLASH) {
         getcwd (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), MAX_FILE_NAME_SIZE);
         strcat (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), "/");
         SRC_STK_FILE_IDX(SRC_STK_BASE_IDX) =
                                  strlen(SRC_STK_PATH_NAME(SRC_STK_BASE_IDX));
         strcat (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), pound_file);
      }
      else {
         SRC_STK_FILE_IDX(SRC_STK_BASE_IDX) = 0;
         strcpy (SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), pound_file);
      }

      /* If CIF records have been requested, output the Source File record.  */
      /* Always output a File Name record for the source file.               */

      c_i_f = cif_actual_file;
      SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX) =
         cif_file_name_rec(SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), pound_file);

      if (cif_flags) {
         cif_source_file_rec(SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX),
                             cmd_line_flags.src_form);
      }

      c_i_f = cif_tmp_file;

      cif_file_rec_issued = TRUE;

      global_line_tbl_idx = 0;
      SRC_STK_FILE_LINE(SRC_STK_BASE_IDX) = 0;
      SRC_STK_GLOBAL_LINE_IDX(src_stk_idx) = NULL_IDX;
      update_global_line();

      /* Set the line numbers in this entry correctly.                       */
      /* Always set GL_CIF_FILE_ID; it's needed for buffered message output. */

      GL_CIF_FILE_ID(global_line_tbl_idx)  = 
                                     SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX);
      GL_GLOBAL_LINE(global_line_tbl_idx)  = 1;
      GL_FILE_LINE(global_line_tbl_idx)    = 1;

      if (source_form == Fixed_Form) {
         if (! on_off_flags.preprocess_only) {
            fixed_get_stmt ();
         }
      }
      else {
         if (! on_off_flags.preprocess_only) {
            expected_line = Regular_Line;
            free_get_stmt ();
         }
      }
   }
   else if (! cif_file_rec_issued ) {
      /* If CIF records have been requested, output the Source File record.  */
      /* Always output a File Name record for the source file.               */

      c_i_f = cif_actual_file;
      SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX) =
         cif_file_name_rec(SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), src_file);

      if (cif_flags) {
         cif_source_file_rec(SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX),
                             cmd_line_flags.src_form);
      }

      c_i_f = cif_tmp_file;
      cif_file_rec_issued = TRUE;

      /* Set the line numbers in this entry correctly.                       */
      /* Always set GL_CIF_FILE_ID; it's needed for buffered message output. */

      GL_CIF_FILE_ID(global_line_tbl_idx)  = 
                                SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX);
   }

   TRACE (Func_Exit, "init_src_input", SRC_STK_FILE_NAME(SRC_STK_BASE_IDX));

   return;

}  /* init_src_input */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Reset_src_input is called by lex routines to back up the src input    *|
|*	character stream when splitting tokens or when a reparse of the input *|
|*	is requested by the parser.					      *|
|*									      *|
|*      Reset can only occur within the range of a single statement.          *|
|*									      *|
|* Input parameters:							      *|
|*	buf_idx			stmt_buf_idx will be reset to this.           *|
|*	stmt_num		compared to statement_number to ensure that   *|
|*				we are resetting within the same statement.   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void reset_src_input (int buf_idx,
		      int stmt_num)

{
   char  ch;
   int	 i;

   TRACE (Func_Entry, "reset_src_input", NULL);

   /* These checks can be placed under _DEBUG later. For   */
   /* now, however, I want them executed in both compilers */

   /* Attempt to set src to line not in buffer */

   if (stmt_num != statement_number && !in_action_stmt_of_if_where_or_forall) {
      PRINTMSG (stmt_line_num[stmt_line_idx], 207, Internal, 1);
   }

   if (buf_idx < 0) {
      PRINTMSG (stmt_line_num[stmt_line_idx], 626, Internal, 1, 
                "valid buf_idx",
                "reset_src_input");
   }

   stmt_line_idx = 0;

   if (buf_idx == 0) {
      stmt_line_idx = 1;
   }
   else {
      for (i = 1; i <= lines_in_buf; i++) {
         if (buf_idx >= stmt_line_start_idx[i] &&
             buf_idx <= stmt_line_end_idx[i]) {
            stmt_line_idx = i;
            break;
         }
      }
   }

# ifdef _DEBUG
   if (stmt_line_idx == 0) {
      PRINTMSG(1,626,Internal,1,
               "valid stmt_line_idx",
               "reset_src_input");
   }
# endif

   stmt_buf_idx = buf_idx;

   if (stmt_buf_idx == 0 ||
       stmt_buf[stmt_buf_idx] == '\0' ||
       stmt_buf[stmt_buf_idx] == '\n') {
      ch = ' ';
   }
   else {
      ch = stmt_buf[stmt_buf_idx];
   }

   if (islower(ch)) {                               /* lowercase char     */
      ch = TOUPPER(ch);                             /* cnvrt lwr to upr   */
   }

   LA_CH_VALUE	= ch;
   LA_CH_LINE   = stmt_line_num[stmt_line_idx];
   LA_CH_COLUMN = stmt_buf_col[stmt_buf_idx];
   LA_CH_BUF_IDX = stmt_buf_idx;
   LA_CH_STMT_NUM = statement_number;
   LA_CH_CLASS	= (ch == (char) EOF) ? Ch_Class_EOF : ch_class[LA_CH_VALUE];

   TRACE (Func_Exit, "reset_src_input", NULL);

   return;

}  /* reset_src_input */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Fixed_get_char is called to obtain the next non-space character from  *|
|*	the source line buffer when outside a character literal context.      *|
|*									      *|
|*	Lowercase letters are converted to uppercase.  End of statement	      *|
|*	causes an EOS to be returned.					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next look ahead character from source line    *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void fixed_get_char (void)

{
   char ch;
   int  ich;


   TRACE (Func_Entry, "fixed_get_char", NULL);

   /* if previous la_ch was end of stmt, then get next stmt */

   if (LA_CH_VALUE == EOS) {
      stmt_end_line = LA_CH_LINE;
      stmt_end_col  = LA_CH_COLUMN - 1;

      if (change_source_form) {
         change_source_form = FALSE;
         line_size = FREE_SRC_LINE_SIZE;
         expected_line = Regular_Line;
         get_char = free_get_char;
         get_char_literal = free_get_char_literal;
         source_form = Free_Form;
      
         free_get_char ();

         TRACE (Func_Exit, "fixed_get_char",
                           ch_str[(unsigned char)LA_CH_VALUE]);

         return;
      }

      fixed_get_stmt ();				/* get next src line  */

      /* the only time that stmt_buf_type is comment is EOF of include file */
      while (stmt_buf_type == Comment_Line) {
         if (change_source_form) {
            change_source_form = FALSE;
            line_size = FREE_SRC_LINE_SIZE;
            expected_line = Regular_Line;
            get_char = free_get_char;
            get_char_literal = free_get_char_literal;
            source_form = Free_Form;

            free_get_char ();
   
            TRACE (Func_Exit, "fixed_get_char",
                               ch_str[(unsigned char)LA_CH_VALUE]);

            return;
         }
         else {
            fixed_get_stmt ();
         }
      }


      ich = stmt_buf[stmt_buf_idx + 1];

      while (((ich == newline)               |
               (ich == semi_colon)           |
               (ich == bang))                                 &&
             (stmt_line_idx >= lines_in_buf))                 {

         fixed_get_stmt ();

         while (stmt_buf_type == Comment_Line) {
            if (change_source_form) {
               change_source_form = FALSE;
               line_size = FREE_SRC_LINE_SIZE;
               expected_line = Regular_Line;
               get_char = free_get_char;
               get_char_literal = free_get_char_literal;
               source_form = Free_Form;

               free_get_char ();
  
               TRACE (Func_Exit, "fixed_get_char",
                                  ch_str[(unsigned char)LA_CH_VALUE]);

               return;
            }
            else {
               fixed_get_stmt ();
            }
         }

         ich = stmt_buf[stmt_buf_idx + 1];
      }

      if (stmt_buf_type == Dir_Line) {			/* directive line     */
         ch = stmt_buf[stmt_buf_idx];         /* next src character */

         if (islower(ch)) {
            ch = TOUPPER(ch);
         }

         LA_CH_VALUE  = ch;
	 LA_CH_LINE   = stmt_line_num[stmt_line_idx];	/* global line num    */
	 LA_CH_COLUMN = stmt_buf_col[stmt_buf_idx];
         LA_CH_BUF_IDX = stmt_buf_idx;
         LA_CH_STMT_NUM = statement_number;

         switch (stmt_prefix_len) {
         case 1 :
	    LA_CH_CLASS  = Ch_Class_Dir1;		/* directive class    */
            break;
         case 2 :
	    LA_CH_CLASS  = Ch_Class_Dir2;		/* directive class    */
            break;
         case 3 :
	    LA_CH_CLASS  = Ch_Class_Dir3;		/* directive class    */
            break;
         case 4 :
	    LA_CH_CLASS  = Ch_Class_Dir4;		/* directive class    */
            break;
         }

	 TRACE (Func_Exit, "fixed_get_char", 
                           ch_str[(unsigned char) LA_CH_VALUE]);

	 return;
      }
   }

   /* find next significant char if there is one */

   do {
      ich = stmt_buf[++stmt_buf_idx];			/* next src character */

      if (stmt_buf_idx == stmt_buf_EOS_idx) {
         ich = eos;
      }
      else if (stmt_buf_idx == stmt_line_end_idx[stmt_line_idx]) {

         if (stmt_line_idx < lines_in_buf) {	        /* stmt continues     */
            stmt_buf_idx = stmt_line_start_idx[++stmt_line_idx] 
                           + stmt_line_offset[stmt_line_idx];

	    ich = blank;				/* loop again	      */
	 }
	 else {						/* end of statement   */
	    ich = eos;					/* cnvrt \n,'!' to EOS*/
	 }
      }
   }
   while ((ich == blank) | (ich == tab));		/* skip white space   */

   LA_CH_LINE	= stmt_line_num[stmt_line_idx];        /* global line num    */
   LA_CH_COLUMN = stmt_buf_col[stmt_buf_idx];
   LA_CH_BUF_IDX = stmt_buf_idx;
   LA_CH_STMT_NUM = statement_number;


   if (havent_issued_at_ansi && ich == at_sign) {
      havent_issued_at_ansi = FALSE;
      ntr_msg_queue(LA_CH_LINE, 900, Ansi,
                    LA_CH_COLUMN,
                    (char *)NULL,
                    0,
                    NO_ARG);
   }
   
   if (havent_issued_dollar_ansi && ich == dollar) {
      havent_issued_dollar_ansi = FALSE;
      ntr_msg_queue(LA_CH_LINE, 901, Ansi,
                    LA_CH_COLUMN,
                    (char *)NULL,
                    0,
                    NO_ARG);
   }

   ch = ich;

   if (islower(ch)) {					/* lowercase char     */
      ch = TOUPPER(ch);	        			/* cnvrt lwr to upr   */
   }

   LA_CH_VALUE = ch;					/* next look ahead ch */
   LA_CH_CLASS = (ch == (char) EOF) ? Ch_Class_EOF : ch_class[LA_CH_VALUE];

   TRACE (Func_Exit, "fixed_get_char", ch_str[(unsigned char) LA_CH_VALUE]);

   return;

}  /* fixed_get_char */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Fixed_get_char_literal is called to obtain the next character from    *|
|*	the source line buffer when inside a character literal context.	      *|
|*	End of statement causes an EOS to be returned.			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next look ahead character from source line    *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void fixed_get_char_literal (void)

{
   char ch;
   int  ich;


   TRACE (Func_Entry, "fixed_get_char_literal", NULL);

   ich = stmt_buf[++stmt_buf_idx];			/* next src character */

   if (stmt_buf_idx == stmt_line_end_idx[stmt_line_idx]) {

      if (stmt_line_idx < lines_in_buf) {               /* check for cont line*/
         stmt_buf_idx = stmt_line_start_idx[++stmt_line_idx]
                        + stmt_line_offset[stmt_line_idx];

	 ich = stmt_buf[++stmt_buf_idx];		/* next src character */
      }
      else {						/* end of statement   */
	 ich = eos;					/* cnvrt \n to EOS    */
      }
   }

   ch = ich;
   LA_CH_VALUE	= ch;					/* next look ahead ch */
   LA_CH_LINE	= stmt_line_num[stmt_line_idx];
   LA_CH_COLUMN = stmt_buf_col[stmt_buf_idx];
   LA_CH_BUF_IDX = stmt_buf_idx;
   LA_CH_STMT_NUM = statement_number;
   LA_CH_CLASS	= (ch == (char) EOF) ? Ch_Class_EOF : ch_class[LA_CH_VALUE];

   TRACE (Func_Exit, "fixed_get_char_literal", 
                     ch_str[(unsigned char) LA_CH_VALUE]);

   return;

}  /* fixed_get_char_literal */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Free_get_char is called to obtain the next character from the source  *|
|*	line buffer when outside a character literal context.  Space chars    *|
|*	are returned here since they are significant in free source form.     *|
|*									      *|
|*	Lowercase letters are converted to uppercase.  End of statement	      *|
|*	causes an EOS to be returned.					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next look ahead character from source line    *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void free_get_char (void)

{
   char ch;
   int  ich;

   
   TRACE (Func_Entry, "free_get_char", NULL);

   sig_blank = FALSE;

   /* If previous la_ch was end of stmt, then get next stmt.		      */

   if (LA_CH_VALUE == EOS) {
      stmt_end_line = LA_CH_LINE;
      stmt_end_col  = LA_CH_COLUMN - 1;

      if (change_source_form) {
         change_source_form = FALSE;

         if (cmd_line_flags.line_size_80) {
            line_size = FIXED_SRC_LINE_SIZE_80;
         }
         else if (cmd_line_flags.line_size_132) {
            line_size = FIXED_SRC_LINE_SIZE_132;
         }
         else {
            line_size = FIXED_SRC_LINE_SIZE_72;
         }
         get_char = fixed_get_char;
         get_char_literal = fixed_get_char_literal;
         source_form = Fixed_Form;

         fixed_get_char ();

         TRACE (Func_Exit, "free_get_char", 
                           ch_str[(unsigned char) LA_CH_VALUE]);

         return;
      }

      free_get_stmt ();

      /* the only time that stmt_buf_type is comment is EOF of include file */
      while (stmt_buf_type == Comment_Line) {
         if (change_source_form) {
            change_source_form = FALSE;

            if (cmd_line_flags.line_size_80) {
               line_size = FIXED_SRC_LINE_SIZE_80;
            }
            else if (cmd_line_flags.line_size_132) {
               line_size = FIXED_SRC_LINE_SIZE_132;
            }
            else {
               line_size = FIXED_SRC_LINE_SIZE_72;
            }

            get_char = fixed_get_char;
            get_char_literal = fixed_get_char_literal;
            source_form = Fixed_Form;

            fixed_get_char ();

            TRACE (Func_Exit, "free_get_char", 
                              ch_str[(unsigned char) LA_CH_VALUE]);

            return;
         }
         else {
            free_get_stmt ();
         }
      }

      ich = stmt_buf[stmt_buf_idx + 1];

      while (((ich == newline)     |
              (ich == semi_colon)  |
               (ich == bang))                                 &&
             (stmt_line_idx >= lines_in_buf))                 {

         free_get_stmt ();

         while (stmt_buf_type == Comment_Line) {
            if (change_source_form) {
               change_source_form = FALSE;

               if (cmd_line_flags.line_size_80) {
                  line_size = FIXED_SRC_LINE_SIZE_80;
               }
               else if (cmd_line_flags.line_size_132) {
                  line_size = FIXED_SRC_LINE_SIZE_132;
               }
               else {
                  line_size = FIXED_SRC_LINE_SIZE_72;
               }

               get_char = fixed_get_char;
               get_char_literal = fixed_get_char_literal;
               source_form = Fixed_Form;

               fixed_get_char ();

               TRACE (Func_Exit, "free_get_char",
                                 ch_str[(unsigned char) LA_CH_VALUE]);

               return;
            }
            else {
               free_get_stmt ();
            }
         }

         ich = stmt_buf[stmt_buf_idx + 1];
      }


      if (stmt_buf_type == Dir_Line) {			/* directive line     */
         ch = stmt_buf[stmt_buf_idx];         /* next src character */

         if (islower(ch)) {
            ch = TOUPPER(ch);
         }

         LA_CH_VALUE  = ch;
	 LA_CH_LINE   = stmt_line_num[stmt_line_idx];
	 LA_CH_COLUMN = stmt_buf_col[stmt_buf_idx];
         LA_CH_BUF_IDX = stmt_buf_idx;
         LA_CH_STMT_NUM = statement_number;

         switch (stmt_prefix_len) {
         case 1 :
            LA_CH_CLASS  = Ch_Class_Dir1;               /* directive class    */
            break;
         case 2 :
            LA_CH_CLASS  = Ch_Class_Dir2;               /* directive class    */
            break;
         case 3 :
            LA_CH_CLASS  = Ch_Class_Dir3;               /* directive class    */
            break;
         case 4 :
            LA_CH_CLASS  = Ch_Class_Dir4;               /* directive class    */
            break;
         }

	 TRACE (Func_Exit, "free_get_char",
                           ch_str[(unsigned char) LA_CH_VALUE]);

	 return;
      }
   }

   /* handling of continued lines must be in a loop to process special cases. */
   /* beginning of line cases, '& \n' and '& !' have been detected and	      */
   /* reported by free_classify_line.  end of line case, 'text & text' is     */
   /* detected here.  The '&' is considered part of the token string.	      */
   /* a last special case, '&&' is handled here.  It is treated as both a     */
   /* continuation and continued line with zero characters of the token.      */

   ich = stmt_buf[++stmt_buf_idx];			/* next src character */

   if (ich == blank  ||  ich == tab) {                  /* skip no-op blanks  */

      do {
         ich = stmt_buf[++stmt_buf_idx];                /* next src character */
      }
      while (ich == blank  ||  ich == tab);             /* skip white space   */

      sig_blank = TRUE;
   }

   if (stmt_buf_idx == stmt_buf_EOS_idx) {
      ich = eos;
   }
   else if (stmt_buf_idx == stmt_line_end_idx[stmt_line_idx]) {

      if (stmt_line_idx < lines_in_buf) {         /* stmt continues     */
         stmt_buf_idx = stmt_line_start_idx[++stmt_line_idx]
                        + stmt_line_offset[stmt_line_idx];
         ich = stmt_buf[++stmt_buf_idx];
      }
      else {
         ich = eos;
      }

      if (ich == blank  ||  ich == tab) {               /* skip no-op blanks  */

         do {
            ich = stmt_buf[++stmt_buf_idx];             /* next src character */
         }
         while (ich == blank  ||  ich == tab);          /* skip white space   */

         sig_blank = TRUE;
      }
      
      if (stmt_buf_idx == stmt_buf_EOS_idx) {
         ich = eos;
      }
   }

   LA_CH_LINE   = stmt_line_num[stmt_line_idx];
   LA_CH_COLUMN = stmt_buf_col[stmt_buf_idx];
   LA_CH_BUF_IDX = stmt_buf_idx;
   LA_CH_STMT_NUM = statement_number;

   if (havent_issued_at_ansi && ich == at_sign) {
      havent_issued_at_ansi = FALSE;
      ntr_msg_queue(LA_CH_LINE, 900, Ansi,
                    LA_CH_COLUMN,
                    (char *)NULL,
                    0,
                    NO_ARG);
   }

   if (havent_issued_dollar_ansi && ich == dollar) {
      havent_issued_dollar_ansi = FALSE;
      ntr_msg_queue(LA_CH_LINE, 901, Ansi,
                    LA_CH_COLUMN,
                    (char *)NULL,
                    0,
                    NO_ARG);
   }

   ch = ich;

   if (islower(ch)) {					/* lowercase char     */
      ch = TOUPPER(ch);		        		/* cnvrt lwr to upr   */
   }

   LA_CH_VALUE = ch;					/* next look ahead ch */
   LA_CH_CLASS = (ch == (char) EOF) ? Ch_Class_EOF : ch_class[LA_CH_VALUE];

   TRACE (Func_Exit, "free_get_char",
                     ch_str[(unsigned char) LA_CH_VALUE]);

   return;

}  /* free_get_char */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Free_get_char_literal is called to obtain the next character from     *|
|*	the source line buffer when inside a character literal context.	      *|
|*	End of statement causes an EOS to be returned.			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	la_ch			next look ahead character from source line    *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void free_get_char_literal (void)

{
   char ch;
   int  ich;


   TRACE (Func_Entry, "free_get_char_literal", NULL);

   /*  ? */
   /* handling of continued lines must be in a loop to process special cases. */
   /* beginning of line cases, '& \n' and '& !' have been detected and	      */
   /* reported by free_classify_line.  end of line case, 'text & text' is     */
   /* detected here.  The '&' is considered part of the character literal.    */
   /* a last special case, '&&' is handled here.  It is treated as both a     */
   /* continuation and continued line with zero characters of the literal.    */
   /*  ? */

   ich = stmt_buf[++stmt_buf_idx];			/* next src character */


   if (stmt_buf_idx == stmt_line_end_idx[stmt_line_idx]) {

      if (stmt_line_idx < lines_in_buf) {
         stmt_buf_idx = stmt_line_start_idx[++stmt_line_idx]
                        + stmt_line_offset[stmt_line_idx];

         ich = stmt_buf[++stmt_buf_idx];
      }
      else {
         ich = eos;
      }
   }
	
   ch = ich;

   LA_CH_VALUE	= ch;					/* next look ahead ch */
   LA_CH_LINE	= stmt_line_num[stmt_line_idx];
   LA_CH_COLUMN = stmt_buf_col[stmt_buf_idx];
   LA_CH_BUF_IDX = stmt_buf_idx;
   LA_CH_STMT_NUM = statement_number;
   LA_CH_CLASS	= (ch == (char) EOF) ? Ch_Class_EOF : ch_class[LA_CH_VALUE];

   TRACE (Func_Exit, "free_get_char_literal",
                     ch_str[(unsigned char) LA_CH_VALUE]);

   return;

}  /* free_get_char_literal */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Fixed_get_stmt obtains the next src input stmt.	 It will fill         *|
|*      stmt_buf array with a full statement using repeated calls to          *|
|*	read_line.       						      *|
|*									      *|
|*	The line is classified as a comment, include, dir, regular, or	      *|
|*	continuation line by calling fixed_classify_line.  Checks involving   *|
|*	relationships between lines (eg. continued followed by continuation)  *|
|*	are performed here.  Comment and include lines are not returned.      *|
|*      Character constants are marked.                                       *|
|*									      *|
|*	An EOF line is returned when end of the input source file is	      *|
|*	encountered.  Include lines cause a file switch.  End of include      *|
|*	files are treated as comments.					      *|
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

static void fixed_get_stmt (void)
{
   int idx;
   int line_counter = 1;
   int loc_stmt_num;
   int save_idx;
   int stmt_buf_EOS;
   

   TRACE (Func_Entry, "fixed_get_stmt", NULL);

   /* Issue any deferred src_input messages.                                  */

   issue_deferred_msgs();


   if (stmt_buf_type == EOF_Line) {
      /* Attempt to read past end of file */
      PRINTMSG (0, 50, Internal, 1);			/* all done now	      */
   }

   stmt_line_idx = NULL_IDX;				/* line array index   */
   stmt_buf_idx = NULL_IDX;                             /* global stmt buf idx*/
   lines_in_buf = 0;                                    /* total lines in buf */

   label_ok = FALSE;

   /* loop while stmt continues */
# pragma _CRI align
   do {

      save_idx = 0;

      /* add nxt_line to stmt_buf */

      stmt_line_num[++stmt_line_idx] = nxt_line_num;

      /* stmt_line_offset holds offset to first significant char of line */

      if (starting_pt) {
         stmt_line_offset[stmt_line_idx] = starting_pt - 2;
         label_ok = FALSE;
      }
      else {
         stmt_line_offset[stmt_line_idx] = nxt_line_idx - 1;
         starting_pt = nxt_line_idx;

         if (nxt_line_label) {
            label_ok = TRUE;
         }
      }
   
      /* stmt_line_start_idx points to where line starts in stmt_buf */

      stmt_line_start_idx[stmt_line_idx] = line_counter;

      /* stmt_line_end_idx points to newline or bang */

      stmt_line_end_idx[stmt_line_idx] = nxt_line_EOL + line_counter - 1;

      /* record the statement type                                 */

      if (nxt_line_type != Continuation_Line      &&
          nxt_line_type != Dir_Continuation_Line) {
         stmt_buf_type = nxt_line_type;

         if (nxt_line_type == Dir_Line) {
            stmt_prefix_len = nxt_line_prefix_len;
            stmt_buf_dir_prefix = nxt_line_dir_prefix;
         }

         if (stmt_buf_type != Comment_Line &&
             stmt_buf_type != Pound_Src_Line &&
             stmt_buf_type != Pound_Include_Exit_Line) {
            INCREMENT_STATEMENT_NUMBER;
         }
      }

      if (stmt_buf_type == Dir_Line) {
         line_dir_prefix[stmt_line_idx] = nxt_line_actual_dir_prefix;
      }

      move_up_next_msg_queue();

      /* copy nxt_line to proper part of stmt_buf, backwards       */

      stmt_buf_idx = line_counter + nxt_line_EOL - 1;
      line_counter += nxt_line_EOL;
      
      for (idx = nxt_line_EOL; idx > 0; idx --) {
         stmt_buf[stmt_buf_idx] = nxt_line[NXT_COL(idx)];
#ifdef KEY /* Bug 3449 */
         if (stmt_buf_idx >= MAX_STMT_CHAR_SIZE) {
           PRINTMSG(curr_glb_line, 1593, Limit, 0);
	   }
#endif
         stmt_buf_col[stmt_buf_idx] = nxt_line_col[NXT_COL(idx)];
         stmt_buf_idx--;

         if (havent_issued_tab_ansi && 
             idx < nxt_line_EOL     &&
             nxt_line[NXT_COL(idx)] == tab) {
            havent_issued_tab_ansi = FALSE;
            ntr_msg_queue(nxt_line_num, 899, Ansi,
                          idx,
                          (char *)NULL,
                          0,
                          NO_ARG);
         }

         if (nxt_line[NXT_COL(idx)] == semi_colon &&
             stmt_buf_type != Dir_Line) {
            if ((idx > starting_pt) && (idx < nxt_line_EOL)) {
               save_idx = idx;
            }
         }
      }

      idx = 0;
      lines_in_buf++;

      /* save_idx is the point in nxt_line where the next ';' is */
      /* If there is a semi_colon then I don't replace nxt_line  */
      /* or classify it again. I simply recopy nxt_line into the */
      /* stmt_buf and start processing after the ;.              */
      /* Must check if nxt_line_type is a comment line because   */
      /* when an include file ends, comment lines come through.  */

      if (save_idx &&
          nxt_line_type != Comment_Line) {

         stmt_buf_EOS = save_idx;
         idx = save_idx;
         while ((nxt_line[NXT_COL(idx)] == semi_colon) |
                (nxt_line[NXT_COL(idx)] == blank)      |
                (nxt_line[NXT_COL(idx)] == tab))       {
            idx++;
         }

         /* starting_pt is the idx I start processing the next time around */

         starting_pt        = idx;
         nxt_line_type      = Regular_Line;
         continuation_count = 0;		/* clear cont counter */
         include_found	    = FALSE;	/* and include flags  */
         include_complete   = FALSE;
      }
      else {
         stmt_buf_EOS = nxt_line_EOL;
         starting_pt = NULL_IDX;
      }

      /* don't read past end of source file            */
      /* and don't get new nxt_line if had semi-colon. */

      if ((stmt_buf_type != EOF_Line) &&
          (starting_pt == NULL_IDX))       {

	 /* get next line from src input file */
	 do {
            nxt_line_type = Regular_Line;

	    if (get_nxt_line ()) {		/* read next src line */

	       if (include_switch) {
		  update_global_line();		/* enter global_line_tbl  */
		  include_switch = FALSE;
	       }

               if (issue_pound_exit_line) {
                  OUTPUT_POUND_INCLUDE_EXIT_LINE(curr_glb_line);
                  issue_pound_exit_line = FALSE;
               }

               nxt_line_mp_line = FALSE;

               if (nxt_line_type != Cond_Comp_Line) {
                  PP_ORIG_SIZE = line_size;
                  classify_line();
               }

               if (on_off_flags.save_dot_i) {

                  if (ignore_source_line ||
                      nxt_line_type == Cond_Comp_Line ||
                      nxt_line_type == Include_Line) {

                     /* print blank line */
                     fprintf(dot_i_fptr, "\n");
                     previous_global_line++;
                  }
                  else {
                     print_nxt_line();
                  }
               }

	       switch (nxt_line_type) {
		  case Comment_Line:			/* ignore comments    */
		     break;

                  case Cond_Comp_Line:
                     if (parse_cc_line()) {

                        /* if result is true, then it was an include line */

                        nxt_line_type = Include_Line;
                        include_stmt_file_line = SRC_STK_FILE_LINE(src_stk_idx);

#ifdef KEY /* Bug 10151 */
                        if (open_include_file (TRUE, FALSE))
#else /* KEY Bug 10151 */
                        if (open_include_file (TRUE))
#endif /* KEY Bug 10151 */
			{
                           include_found  = TRUE;      /* flag begin of file */
                           include_switch = TRUE;      /* flag file switch   */
                        }
                     }
                     else {
                        nxt_line_type = Comment_Line;
                     }
                     angle_brkt_include = FALSE;
                     break;

		  case Dir_Line:
		  case Regular_Line:
		     continuation_count = 0;		/* clear cont counter */
		     include_found	= FALSE;	/* and include flags  */
		     include_complete	= FALSE;
		     break;

		  case Continuation_Line:
		  case Dir_Continuation_Line:

		     if (++continuation_count == MAX_ANSI_FIXED_LINES) {

			/* Too many continuation lines is non-standard. */

                        ntr_msg_queue(nxt_line_num, 52, Ansi, 
                                      CONTINUE_COLUMN,
                                      "fixed",
                                      (MAX_ANSI_FIXED_LINES - 1), 
                                      ARG_STR_ARG);
		     }

                     if (continuation_count == MAX_FIXED_LINES) {

                        /* this is it. I cn give ya na more powr cap'n */

                        ntr_msg_queue(nxt_line_num, 524, Error,
                                      CONTINUE_COLUMN,
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
                     }

                     if (continuation_count >= MAX_FIXED_LINES) {
                        nxt_line_type = Comment_Line;
                        break;
                     }

                     if (stmt_buf_type == Dir_Line           &&
                         nxt_line_type == Continuation_Line) {

                        /* Invalid continuation of comment or directive */

                        ntr_msg_queue(nxt_line_num, 51, Error,
                                      CONTINUE_COLUMN,
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
                     }

		     if (include_found) {
			include_found = FALSE;

			/* First line of included file must not be a cont line*/

			ntr_msg_queue(nxt_line_num, 53, Error,
                                      CONTINUE_COLUMN,
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
		     }

		     if (include_complete) {
			include_complete = FALSE;

			/* Next line of file after include must not be a cont */

			ntr_msg_queue(nxt_line_num, 54, Error,
                                      CONTINUE_COLUMN,
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
		     }

                     if (cif_flags & MISC_RECS) {
                        cif_cont_line_rec(
                           (nxt_line_type == Continuation_Line) ? 0 : 1,
                           nxt_line_num);
                     }
 
		     break;

                  case Pound_Src_Line:
                     break;

                  case Pound_Include_Exit_Line:
                     include_complete = TRUE;
		     nxt_line_type	   = Comment_Line;
                     curr_glb_line--;
                     SRC_STK_FILE_LINE(src_stk_idx)--;
                     GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                               SRC_STK_FILE_LINE(src_stk_idx);
                     set_related_gl_source_lines(
                                     SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));

                     if (source_form != SRC_STK_PREV_SRC_FORM(src_stk_idx)) {
                        change_source_form = TRUE;
                     }
                     POP_SRC;
                     include_switch = TRUE;
                     break;

                  case Pound_Include_Enter_Line:
		  case Include_Line:

                     include_stmt_file_line = SRC_STK_FILE_LINE(src_stk_idx);

#ifdef KEY /* Bug 10151 */
		     if (open_include_file (FALSE,
		       Pound_Include_Enter_Line == nxt_line_type))
#else /* KEY Bug 10151 */
		     if (open_include_file (FALSE))
#endif /* KEY Bug 10151 */
		     {
			include_found  = TRUE;		/* flag begin of file */
			include_switch = TRUE;		/* flag file switch   */
		     }

		     break;
	       }  /* switch */
	    }
	    else {					/* EOF on source file */

               /* need to bump pp_line_idx since classify_line was not called */

               if (cmd_line_flags.pp_macro_expansion) {
                  pp_line_idx++;
               }

	       /* check for termination of include file */

	       if (src_stk_idx > SRC_STK_BASE_IDX) {	/* curr src is include*/
		  include_complete = TRUE;		/* flag end of file   */
		  nxt_line_type	   = Comment_Line;	/* make EOF a comment */
                  nxt_line_EOL = 2;
                  curr_glb_line--;            /* don't count this line */

                  GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                            SRC_STK_FILE_LINE(src_stk_idx);
                  set_related_gl_source_lines(
                                     SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));

                  if (source_form != SRC_STK_PREV_SRC_FORM(src_stk_idx)) {
                     change_source_form = TRUE;
                  }
		  POP_SRC;
		  include_switch = TRUE;		/* flag file switch   */
                  issue_pound_exit_line = TRUE;
                  break;
	       }
	       else {					/* curr src is input  */
                  GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                            SRC_STK_FILE_LINE(src_stk_idx);
                  set_related_gl_source_lines(
                                     SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));
		  nxt_line_type = EOF_Line;		/* end of compilation */
                  nxt_line_EOL = 2;
	       }
	    }
	 }
	 while (nxt_line_type == Comment_Line | 
                nxt_line_type == Include_Line |
                nxt_line_type == Pound_Include_Enter_Line);

      }
   }
   while (nxt_line_type == Continuation_Line || 
          nxt_line_type == Dir_Continuation_Line);

   if (prev_statement_number != statement_number) {
      loc_stmt_num = statement_number;
      statement_number = prev_statement_number;
      prev_statement_number = loc_stmt_num;
   }

   stmt_buf_EOS_idx = stmt_line_start_idx[lines_in_buf] + stmt_buf_EOS - 1;
   stmt_EOS_la_ch.line = stmt_line_num[lines_in_buf];
   stmt_EOS_la_ch.column = stmt_buf_col[stmt_buf_EOS_idx];
   stmt_EOS_la_ch.stmt_buf_idx = stmt_buf_EOS_idx;
   stmt_EOS_la_ch.stmt_num = statement_number;

   stmt_buf_idx = stmt_line_offset[1] + 1;
   stmt_line_idx = SRC_STK_BASE_IDX;

   PRINT_STMT_SRC();	/* If DEBUG and -u src or -u stmt set print source */

   TRACE (Func_Exit, "fixed_get_stmt", NULL);

   return;

}  /* fixed_get_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Reads next line of source characters from source file into source     *|
|*	buffer.	 A line is delimited by a new line character followed by EOS. *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if source input line was read, FALSE on EOF or error.	      *|
|*									      *|
\******************************************************************************/

boolean read_line (boolean	cc_continuation_line)

{
   int		ch;
   int		i;
   int		k;
   int		limit;
   boolean	result		= TRUE;			/* assume success     */


   TRACE (Func_Entry, "read_line", NULL);

   if (extra_nxt_line != 0 &&
       nxt_line_num_lines == 0) {

      /* just move the extra line into the first line spot */

      nxt_line_num_lines++;

      if (pp_nxt_line_type[extra_nxt_line] == EOF_Line) {
         nxt_line_start_idx[1] = 1;
         nxt_line_end_idx[1] = 2;
         nxt_line[1]  = EOF;
         nxt_line[2]  = eos;
         pp_nxt_line_length[1] = 1;
         pp_nxt_line_type[1] = EOF_Line;
         pp_nxt_line_idx[1] = NULL_IDX;
         result       = FALSE;                         /* return EOF result  */
      }
      else {
         k = 0;
         for (i = nxt_line_start_idx[extra_nxt_line]; 
              i <= nxt_line_end_idx[extra_nxt_line]; 
              i++) {

            nxt_line[++k] = (nxt_line[i] & 0xFF);
            nxt_line_col[k] = nxt_line_col[i];
         }
         nxt_line_start_idx[1] = 1;
         nxt_line_end_idx[1] = k;
         pp_nxt_line_length[1] = k-1;
      }

      pp_nxt_line_num[1] = pp_nxt_line_num[extra_nxt_line];
      pp_nxt_line_mp_line[1] = pp_nxt_line_mp_line[extra_nxt_line];

      if (pp_nxt_line_type[extra_nxt_line] == Include_Line) {
         nxt_line_type = Include_Line;
      }

      extra_nxt_line = NULL_IDX;

      goto EXIT;
   }

   /* copy max number characters in the next line to the source input buffer  */
#ifdef KEY
   if (src_stk_idx == NULL_IDX) { /* src_stk_idx underflow */     
     PRINTMSG (curr_glb_line, 1, Internal, 0, "src_stk_idx");
   }                                                              
#endif
   ch = getc(SRC_STK_FILE_PTR(src_stk_idx));

   if (on_off_flags.preprocess &&
       ch != newline &&
       ch != EOF     &&
       (ch == '#' || cc_continuation_line)) {

      limit = MAX_STMT_CHAR_SIZE - 4;

      if (! cc_continuation_line) {
         cc_stmt_buf_idx = NULL_IDX;
         cc_stmt_buf_num_lines = 0;
      }

      cc_stmt_buf_line[++cc_stmt_buf_num_lines].line = ++curr_glb_line;
      cc_stmt_buf_line[cc_stmt_buf_num_lines].start_idx = cc_stmt_buf_idx;
      SRC_STK_FILE_LINE(src_stk_idx)++;

      cc_stmt_buf[++cc_stmt_buf_idx] = ch;
      while ((ch = getc(SRC_STK_FILE_PTR(src_stk_idx))) != newline && 
             ch != EOF)
      {

         if (limit > 0) {
            cc_stmt_buf[++cc_stmt_buf_idx] = ch;
            limit--;
         }
#ifdef KEY /* Bug 3632 */
	 else {
           PRINTMSG(curr_glb_line, 1674, Limit, 0, FREE_SRC_LINE_SIZE);
	 }
#endif /* KEY Bug 3632 */
      }
#ifdef KEY
      // Bug 3605: Don't treat DOS 'carriage return plus linefeed' as two
      // distinct line separators; just suppress the carriage return
      if (ch == newline && cc_stmt_buf_idx >= 0 &&
         cc_stmt_buf[cc_stmt_buf_idx] == dosnewline) {
         cc_stmt_buf_idx  -= 1;
      }
#endif /* KEY */

      cc_stmt_buf[++cc_stmt_buf_idx] = newline;
      cc_stmt_buf[++cc_stmt_buf_idx] = eos;

      nxt_line_type = Cond_Comp_Line;

      goto EXIT;
   }

   nxt_line_start_idx[nxt_line_num_lines + 1] =
                              nxt_line_end_idx[nxt_line_num_lines] + 1;

   nxt_line_num_lines++;

   for (i = nxt_line_start_idx[nxt_line_num_lines] - 1;
        i <= nxt_line_start_idx[nxt_line_num_lines] + line_size;
        i++) {
      nxt_line_col[i] = (i - nxt_line_start_idx[nxt_line_num_lines]) + 1;
   }

   nxt_line_idx = nxt_line_start_idx[nxt_line_num_lines] - 1;
   if (ch != newline && ch != EOF)
   {
      limit = nxt_line_idx + FREE_SRC_LINE_SIZE;

      nxt_line[++nxt_line_idx] = ch;
      while ((ch = getc(SRC_STK_FILE_PTR(src_stk_idx))) != newline && 
             ch != EOF)
      {

         if (nxt_line_idx < limit) { 
            nxt_line[++nxt_line_idx] = ch;
         }
#ifdef KEY /* Bug 3632 */
	 else {
           PRINTMSG(curr_glb_line, 1674, Limit, 0, FREE_SRC_LINE_SIZE);
	 }
#endif /* KEY Bug 3632 */
      }
#ifdef KEY
      // Bug 3605: Don't treat DOS 'carriage return plus linefeed' as two
      // distinct line separators; just suppress the carriage return
      if (ch == newline && nxt_line_idx >= 0 &&
         nxt_line[nxt_line_idx] == dosnewline) {
         nxt_line_idx  -= 1;
      }
#endif /* KEY */
   }


   if (nxt_line_idx > 
                   (nxt_line_start_idx[nxt_line_num_lines] - 1) + line_size) {

#ifdef KEY /* Bug 6839 */
     /* This statement truncates the line at column 72 (or 132, or whatever is
      * appropriate) in fixed format. But a '# lineno "filename"' line provided
      * by cpp should never be truncated, because a lengthy filename will
      * exceed the limit.
      */
     if (1 != nxt_line_num_lines || 1 < nxt_line_start_idx[1] ||
       '#' != nxt_line[nxt_line_start_idx[1]]) {
       nxt_line_idx = (nxt_line_start_idx[nxt_line_num_lines] - 1) + line_size;
       }
#else /* KEY Bug 6839 */
      nxt_line_idx = (nxt_line_start_idx[nxt_line_num_lines] - 1) + line_size;
#endif /* KEY Bug 6839 */
   }

   if (nxt_line_idx == nxt_line_start_idx[nxt_line_num_lines] - 1 &&
       ch == EOF) {
      /* file ends with '\n'*/
      nxt_line[nxt_line_start_idx[nxt_line_num_lines]]  = EOF;
      nxt_line[nxt_line_start_idx[nxt_line_num_lines] + 1]  = eos;
      pp_nxt_line_length[nxt_line_num_lines] = 1;
      nxt_line_end_idx[nxt_line_num_lines] = 
                            nxt_line_start_idx[nxt_line_num_lines] + 1;
      pp_nxt_line_num[nxt_line_num_lines] = ++curr_glb_line;
      pp_nxt_line_type[nxt_line_num_lines] = EOF_Line;
      pp_nxt_line_idx[nxt_line_num_lines] = NULL_IDX;
      result       = FALSE;				/* return EOF result  */
   }
   else {						/* return next line   */
      if (source_form == Fixed_Form) {
         /* pad to continue col*/
/* # pragma _CRI shortloop  KAY This causes problems on the PVP */
         while (nxt_line_idx < 
              (nxt_line_start_idx[nxt_line_num_lines] - 1) + CONTINUE_COLUMN) {	
	    nxt_line[++nxt_line_idx] = blank; 
         }
      }

      pp_nxt_line_num[nxt_line_num_lines] = ++curr_glb_line;
      SRC_STK_FILE_LINE(src_stk_idx)++;			/* upd file line num  */

      nxt_line[++nxt_line_idx] = newline;		/* end line w/ \n/EOS */
      pp_nxt_line_length[nxt_line_num_lines] = nxt_line_idx - 
                                   (nxt_line_start_idx[nxt_line_num_lines] - 1);
      nxt_line[++nxt_line_idx] = eos;
      nxt_line_end_idx[nxt_line_num_lines] = nxt_line_idx;

      if (pp_nxt_line_length[nxt_line_num_lines] > 73 && 
          source_form == Fixed_Form &&
          ! have_issued_msg_37 &&
          issue_classify_msg) {
         have_issued_msg_37 = TRUE;
         ntr_next_msg_queue(curr_glb_line, 37, Ansi,
                       0,
                       (char *)NULL,
                       0,
                       NO_ARG);
      }




   }

EXIT:

   TRACE (Func_Exit, "read_line", NULL);

   return (result);

}  /* read_line */

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

static boolean is_pound_line_dir(void)

{
   int		ch;
   boolean	dir = FALSE;
   int		idx;

   TRACE (Func_Entry, "is_pound_line_dir", NULL);

   ch = nxt_line[NXT_COL(1)];                          /* column 1 char      */

   if (ch == pound) {
      idx = NXT_COL(2);
      ch = nxt_line[idx];

      while (ch == blank | ch == tab) {
         idx++;
         ch = nxt_line[idx];
      }

      if (isdigit(ch)) {
         dir = TRUE;
      }
   }

   TRACE (Func_Exit, "is_pound_line_dir", NULL);

   return(dir);

}  /* is_pound_line_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	The line in the input buffer is classified as a comment, include,     *|
|*	dir, continuation, or regular line.  Fixed source form rules apply.   *|
|*      Character constants are marked with a set sign bit in each character. *|
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

static void fixed_classify_line (void)

{
   int     ch;
   char    form[4] = "    ";
   char    ch_tmp;
   int	   cont_col;
   char    delim;
   char    err_str[2];
   int     i;
   int     ich;
   int     idx;
   int	   incl_idx;
   boolean label;
   int     line_num;
   char    line_num_str[24];
   int	   nxt_idx;
   int	   save_curr_glb_line;

   TRACE (Func_Entry, "fixed_classify_line", NULL);

   PP_EOL = nxt_line_end_idx[pp_line_idx] - 1;

START:

   cont_col = NXT_COL(CONTINUE_COLUMN);

   PP_IDX = NXT_COL(0);
   PP_LABEL = FALSE;

   ch = nxt_line[NXT_COL(1)];

# ifdef _FRONTEND_CONDITIONAL_COMP
   if (ch == pound &&
       on_off_flags.preprocess) {

      PP_LINE_TYPE = Cond_Comp_Line;
   } else
# endif
   if (is_pound_line_dir()) {
      PP_IDX = NXT_COL(2);
      ch = nxt_line[PP_IDX];

      while (ch == blank | ch == tab) {
         PP_IDX++;
         ch = nxt_line[PP_IDX];
      }

      if (isdigit(ch)) {
         idx = 0;
         line_num_str[idx++] = ch;
         ch = nxt_line[++PP_IDX];
         while (isdigit(ch)) {
            line_num_str[idx++] = ch;
            ch =  nxt_line[++PP_IDX];
         }

         line_num_str[idx] = '\0';
         line_num = atoi(line_num_str);

         while (ch == blank | ch == tab) {
            PP_IDX++;
            ch = nxt_line[PP_IDX];
         }

         if (ch == quote | ch == db_quote) {
            char_delim = ch;
            ch =  nxt_line[++PP_IDX];

            idx = 0;
            while (ch != char_delim) {
               include_file[idx++] = ch;
               ch =  nxt_line[++PP_IDX];
            }
            include_file[idx] = '\0';

            ch =  nxt_line[++PP_IDX];
            while (ch == blank | ch == tab) {
               PP_IDX++;
               ch = nxt_line[PP_IDX];
            }

            if (ch == '1') {
               PP_LINE_TYPE = Pound_Include_Enter_Line;
            }
            else if (ch == '2') {
               PP_LINE_TYPE = Pound_Include_Exit_Line;
            }
            else {
               PP_LINE_TYPE = Comment_Line;

               /* reset the curr_glb_line */
               /* line_num is a file line, not a global line */

               save_curr_glb_line = curr_glb_line;
               curr_glb_line = line_num + GL_GLOBAL_LINE(global_line_tbl_idx) -
                             GL_FILE_LINE(global_line_tbl_idx);
               curr_glb_line--;

               if (first_pound_line) {
                  PP_LINE_TYPE = Pound_Src_Line;
                  change_orig_src_file = TRUE;
                  strcpy(pound_file, include_file);
               }
               else {
                  SRC_STK_FILE_LINE(src_stk_idx) +=
                                 curr_glb_line - save_curr_glb_line;
               }
            }

            first_pound_line = FALSE;

         }
         else if (ch == newline) {

            PP_LINE_TYPE = Comment_Line;

            /* reset the curr_glb_line */
            /* line_num is a file line, not a global line */

            save_curr_glb_line = curr_glb_line;
            curr_glb_line = line_num + GL_GLOBAL_LINE(global_line_tbl_idx) -
                          GL_FILE_LINE(global_line_tbl_idx);
            curr_glb_line--;

            SRC_STK_FILE_LINE(src_stk_idx) +=
                           curr_glb_line - save_curr_glb_line;

            first_pound_line = FALSE;
         }
         else {
            PP_LINE_TYPE = Comment_Line;
         }
      }
      else {
         PP_LINE_TYPE = Comment_Line;
      }
   }
   else if (ignore_source_line) {
      PP_LINE_TYPE = Comment_Line;
   }
# ifdef _D_LINES_SUPPORTED
   else if (ch == uc_d | ch == lc_d) {
      /* this is a debug line. */

      if (on_off_flags.d_lines) {
         nxt_line[NXT_COL(1)] = ' ';
         goto START;
      }
      else {
         PP_LINE_TYPE = Comment_Line;
      }
   }
# endif
   else if (ch == uc_c |  ch == lc_c | ch == bang | ch == star) {

      if (((ch = nxt_line[NXT_COL(2)]) == uc_d   || ch == lc_d) &&
	  ((ch = nxt_line[NXT_COL(3)]) == uc_i   || ch == lc_i) &&
	  ((ch = nxt_line[NXT_COL(4)]) == uc_r   || ch == lc_r) &&
	  ((ch = nxt_line[NXT_COL(5)]) == dollar || ch == at_sign)) {

         PP_PREFIX_LEN = 4;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(5)]);


         if (nxt_line[NXT_COL(6)] == zero) {
            nxt_line[NXT_COL(6)] = blank;
         }

	 if ((ch = nxt_line[NXT_COL(6)]) != blank  && ch != tab) {
            
            if (IS_DIR_CONTINUATION(Cdir_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cdir_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(6);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {
	    PP_IDX = NXT_COL(2);			/* skip 'C' char      */
	    PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cdir_Dir;
            PP_ACTUAL_DIR_PREFIX = Cdir_Dir;
            in_format = FALSE;
            idx = NXT_COL(6);
            do {
               ch = nxt_line[++idx];
            }
            while (ch == blank | ch == tab);

            if (ch == lc_f | ch == uc_f) {
               i = 0;
               while (i < 4) {
                  ch = nxt_line[++idx];
   
                  if (ch == newline) {
                     break;
                  }

                  if (! (ch == blank | ch == tab)) {
                     if (islower(ch)) {
                        form[i] = TOUPPER(ch);
                     }
                     else {
                        form[i] = ch;
                     }
                     i++;
                  }
               }

               if (strncmp(form, "REE ", 4) == 0 &&
                   !disregard_directive[Tok_Dir_Free-Tok_Dir_Start]) {
                  PP_CHANGE_SOURCE_FORM = TRUE;
               }
            }           
         }
      }
      else if (((ch = nxt_line[NXT_COL(2)]) == uc_m   || ch == lc_m) &&
	       ((ch = nxt_line[NXT_COL(3)]) == uc_i   || ch == lc_i) &&
	       ((ch = nxt_line[NXT_COL(4)]) == uc_c   || ch == lc_c) &&
	       ((ch = nxt_line[NXT_COL(5)]) == dollar || ch == at_sign)) {

         PP_PREFIX_LEN = 4;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(5)]);


         if ((ch = nxt_line[NXT_COL(6)]) != blank  && ch != tab) {
  
            if (IS_DIR_CONTINUATION(Cmic_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cmic_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(6);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

   	    PP_IDX = NXT_COL(2);			/* skip 'C' char      */
            in_format = FALSE;
	    PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cmic_Dir;
            PP_ACTUAL_DIR_PREFIX = Cmic_Dir;

            idx = NXT_COL(6);
         }
      }
      else if (((ch = nxt_line[NXT_COL(2)]) == dollar) &&
               ((ch = nxt_line[NXT_COL(3)]) == uc_o   || ch == lc_o) &&
               ((ch = nxt_line[NXT_COL(4)]) == uc_m   || ch == lc_m) &&
               ((ch = nxt_line[NXT_COL(5)]) == uc_p   || ch == lc_p)) {

         /* C$omp */

         PP_PREFIX_LEN = 4;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(2)]);

         if (nxt_line[NXT_COL(6)] == zero) {
            nxt_line[NXT_COL(6)] = blank;
         }

         if ((ch = nxt_line[NXT_COL(6)]) != blank && ch != tab) {

            if (IS_DIR_CONTINUATION(Comp_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Comp_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(6);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

            PP_IDX = NXT_COL(2);                   /* skip 'C' char      */
            in_format = FALSE;
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Comp_Dir;
            PP_ACTUAL_DIR_PREFIX = Comp_Dir;

            idx = NXT_COL(6);

# if ! defined(_TARGET_OS_MAX)
            if (! dump_flags.open_mp &&
                ! on_off_flags.preprocess_only) {
               PP_LINE_TYPE = Comment_Line;
            }
# endif
         }
      }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      else if (((ch = nxt_line[NXT_COL(2)]) == dollar) &&
               ((ch = nxt_line[NXT_COL(3)]) == uc_s   || ch == lc_s) &&
               ((ch = nxt_line[NXT_COL(4)]) == uc_g   || ch == lc_g) &&
               ((ch = nxt_line[NXT_COL(5)]) == uc_i   || ch == lc_i)) {

         /* C$sgi */

         PP_PREFIX_LEN = 4;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(2)]);

         if (nxt_line[NXT_COL(6)] == zero) {
            nxt_line[NXT_COL(6)] = blank;
         }

         if ((ch = nxt_line[NXT_COL(6)]) != blank && ch != tab) {

            if (IS_DIR_CONTINUATION(Comp_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Csgi_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(6);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

            PP_IDX = NXT_COL(2);                   /* skip 'C' char      */
            in_format = FALSE;
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Comp_Dir;
            PP_ACTUAL_DIR_PREFIX = Csgi_Dir;

            idx = NXT_COL(6);

            if (! dump_flags.open_mp &&
                ! on_off_flags.preprocess_only) {
               PP_LINE_TYPE = Comment_Line;
            }
         }
      }
# endif
      else if (((ch = nxt_line[NXT_COL(2)]) == dollar) &&
               ((ch = nxt_line[NXT_COL(3)]) == uc_p   || ch == lc_p) &&
               ((ch = nxt_line[NXT_COL(4)]) == uc_a   || ch == lc_a) &&
               ((ch = nxt_line[NXT_COL(5)]) == uc_r   || ch == lc_r)) {

         /* C$par */

         PP_PREFIX_LEN = 4;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(2)]);

         if ((ch = nxt_line[NXT_COL(6)]) == amp) {

            if (IS_DIR_CONTINUATION(Cpar_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cpar_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(6);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

            PP_IDX = NXT_COL(2);                   /* skip 'C' char      */
            in_format = FALSE;
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cpar_Dir;
            PP_ACTUAL_DIR_PREFIX = Cpar_Dir;

            idx = NXT_COL(6);

            if ((! dump_flags.mp  &&
                 ! on_off_flags.preprocess_only) ||
                ! is_par_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
         }
      }
      else if (((ch = nxt_line[NXT_COL(2)]) == star) &&
               ((ch = nxt_line[NXT_COL(3)]) == dollar) &&
               ((ch = nxt_line[NXT_COL(4)]) == star)) {

         /* C*$* */

         PP_PREFIX_LEN = 3;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(3)]);


         if ((ch = nxt_line[NXT_COL(5)]) == amp) {

            if (IS_DIR_CONTINUATION(Cstar_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cstar_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(5);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

            PP_IDX = NXT_COL(2);                   /* skip 'C' char      */
            in_format = FALSE;
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cstar_Dir;
            PP_ACTUAL_DIR_PREFIX = Cstar_Dir;

            idx = NXT_COL(5);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            if (! is_star_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
# else
            if ((! dump_flags.mp &&
                 ! on_off_flags.preprocess_only) ||
                ! is_star_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
# endif
         }
      }
      else if ((ch = nxt_line[NXT_COL(2)]) == dollar) {

         /* C$ */

         PP_PREFIX_LEN = 1;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(2)]);


         if ((ch = nxt_line[NXT_COL(3)]) == amp &&
             dump_flags.mp) {

            if (IS_DIR_CONTINUATION(Cdollar_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cdollar_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(3);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

            PP_IDX = NXT_COL(2);                   /* skip 'C' char      */
            in_format = FALSE;
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cdollar_Dir;
            PP_ACTUAL_DIR_PREFIX = Cdollar_Dir;

            idx = NXT_COL(3);

            if (dump_flags.mp &&
                is_dollar_directive(idx)) {
               /* intentionally blank */
            }
            else {
                
               ch = nxt_line[NXT_COL(3)];

               if (ch == ' '  ||
                   ch == '\t' ||
                   isdigit(ch)) {

                  PP_MP_LINE = TRUE;
               }

               if ((dump_flags.mp ||
                    dump_flags.open_mp ||
                    on_off_flags.preprocess_only) &&
                   ! cmd_line_flags.disregard_conditional_omp &&
                   PP_MP_LINE) {

                  nxt_line[NXT_COL(1)] = ' ';
                  nxt_line[NXT_COL(2)] = ' ';
                  goto START;
               }
               else {
                  PP_LINE_TYPE = Comment_Line;
                  PP_MP_LINE = FALSE;
               }
            }
         }
      }
# ifdef _DEBUG
      else if (((ch = nxt_line[NXT_COL(2)]) == uc_d   || ch == lc_d) &&
	       ((ch = nxt_line[NXT_COL(3)]) == uc_b   || ch == lc_b) &&
	       ((ch = nxt_line[NXT_COL(4)]) == uc_g   || ch == lc_g) &&
	       ((ch = nxt_line[NXT_COL(5)]) == dollar)) {

         PP_PREFIX_LEN = 4;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */

         MARK_CHAR_CONST(nxt_line[NXT_COL(5)]);

         if ((ch = nxt_line[NXT_COL(6)]) != blank  && ch != tab) {
  
            if (IS_DIR_CONTINUATION(Cdbg_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cdbg_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(6);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

   	    PP_IDX = NXT_COL(2);			/* skip 'C' char      */
            in_format = FALSE;
	    PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cdbg_Dir;
            PP_ACTUAL_DIR_PREFIX = Cdbg_Dir;

            idx = NXT_COL(6);
         }
      }
# endif
      else {
	 PP_LINE_TYPE = Comment_Line;
      }
   }
   else if (ch == star) {
      if ((ch = nxt_line[NXT_COL(2)]) == dollar &&
          (ch = nxt_line[NXT_COL(3)]) == star) {

         /* *$* */

         PP_PREFIX_LEN = 3;

         first_line = FALSE;

         /* mark the sign bit of the dollar or at_sign */
         MARK_CHAR_CONST(nxt_line[NXT_COL(2)]);


         if ((ch = nxt_line[NXT_COL(4)]) == amp) {

            if (IS_DIR_CONTINUATION(Cstar_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cstar_Dir;
               in_format = FALSE;
               PP_IDX  = NXT_COL(4);
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {

            PP_IDX = NXT_COL(1);
            in_format = FALSE;
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cstar_Dir;
            PP_ACTUAL_DIR_PREFIX = Cstar_Dir;

            idx = NXT_COL(4);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            if (! is_star_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
# else
            if ((! dump_flags.mp &&
                 ! on_off_flags.preprocess_only) ||
                ! is_star_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
# endif
         }
      }
      else {
         PP_LINE_TYPE = Comment_Line;
      }
   }
   else if (ch == bang  |  ch == star) {		/* column 1 '!' | '*' */
      PP_LINE_TYPE = Comment_Line;
   }
   else {						/* columns 2-72|80    */
      /* check for tab expansion character in label or continuation field */
      do {
	 if (nxt_line[++PP_IDX] == tab) {

	    /* DEC rules for tab expansion apply in label field */
	    /*		C2345678     ->	 C2345678		*/
	    /*		t1 + 27	     ->	      1 + 27		*/
	    /*		 tI = 3	     ->	       I = 3		*/
	    /*		10tJ=5	     ->	 10    J=5		*/
	    /* where 't' is a tab character expanded to blanks	*/

	    if (IS_1TO9(nxt_line[PP_IDX+1])) { /* digits '1'..'9'   */
	       cont_col = PP_IDX+1;
	    }
	    else {					/* stmt in next col   */
	       cont_col = PP_IDX;
	    }
	 }
      }
      while (PP_IDX < cont_col);

      if (nxt_line[cont_col] == zero) {		/* '0' NOT a continue */
	 nxt_line[cont_col] = blank;		/* convert to a blank */
      }

      PP_IDX = NXT_COL(NULL_IDX);			/* beginning of line  */

      do {						/* find 1st non-blank */
	 ch = nxt_line[++PP_IDX];
      }
      while (ch == blank  |  ch == tab);		/* skip white space   */
      
      if (ch == newline) {				/* blank line	      */
         PP_EOL = PP_IDX;
	 PP_LINE_TYPE = Comment_Line;
      }
      else if (ch == bang) {				/* comment ?	      */
	 if (PP_IDX == cont_col) {		/* continuation col   */
	    PP_LINE_TYPE = Continuation_Line;
	 }
	 else {
	    PP_LINE_TYPE = Comment_Line;
	 }
      }
      else if (PP_IDX > cont_col) {		/* statement columns  */
	 PP_IDX--;				/* restore position   */
	 PP_LINE_TYPE = Regular_Line;
         format_idx = -1;
         in_format = FALSE;
      }
      else if (PP_IDX < cont_col) {		/* label columns      */

         label = TRUE;

         /* verify that we've got numbers here */
         i = PP_IDX;
         while (i < cont_col) {

            if (nxt_line[i] == blank || 
                nxt_line[i] == tab   ||
                (nxt_line[i] >= zero && nxt_line[i] <= nine)) {
            
               i++;
            }
            else {
               label = FALSE;
               break;
            }
         }  

	 if (label) {

            if (nxt_line[cont_col] != blank  &&	/* continuation too ? */
	        nxt_line[cont_col] != tab)   {

	       /* Continuation line must not contain a label. */

               if (issue_classify_msg) {
	          ntr_next_msg_queue(PP_LINE_NUM, 56, Error,
                                nxt_line_col[PP_IDX], 
                                (char *)NULL,
                                0,
                                NO_ARG);
               }

	       PP_IDX  = cont_col;		/* set position	      */
	       PP_LINE_TYPE = Continuation_Line;
            }
            else {
               PP_LABEL = TRUE;
               PP_IDX--;                          /* restore position   */
               PP_LINE_TYPE = Regular_Line;
               format_idx    = 0;                       /* format possible    */
               in_format     = FALSE;
            }
	 }
         else {

            /* Invalid characters found in label field. */

            if (issue_classify_msg) {
               ntr_next_msg_queue(PP_LINE_NUM, 400, Error,
                             nxt_line_col[PP_IDX],
                             (char *)NULL,
                             0,
                             NO_ARG);
            }

            if (nxt_line[cont_col] != blank  && /* continuation too ? */
                nxt_line[cont_col] != tab)   {
               PP_IDX  = cont_col;                /* set position       */
               PP_LINE_TYPE = Continuation_Line;
            }
            else {
               PP_IDX  = cont_col;                /* set position       */
               PP_LINE_TYPE = Regular_Line;
               format_idx    = -1;                      /* no label, no format*/
               in_format     = FALSE;
            }
	 }
      }
      else {						/* continuation col   */
	 PP_LINE_TYPE = Continuation_Line;

         if (! valid_f90_char[nxt_line[cont_col]] &&
             issue_classify_msg) {
            err_str[0] = nxt_line[cont_col];
            err_str[1] = '\0';
            ntr_next_msg_queue(PP_LINE_NUM, 799, Ansi, 
                          nxt_line_col[cont_col],
                          err_str,
                          0,
                          STR_ARG);
         }
      }

      if (first_line && PP_LINE_TYPE != Comment_Line) {

         if (PP_LINE_TYPE == Continuation_Line && issue_classify_msg) {
            ntr_next_msg_queue(PP_LINE_NUM, 211, Error,
                          nxt_line_col[cont_col],
                          (char *)NULL,
                          0,
                          NO_ARG);
         }

         first_line = FALSE;
      }
   }

   if (PP_LINE_TYPE == Regular_Line          |
       PP_LINE_TYPE == Dir_Line              |
       PP_LINE_TYPE == Dir_Continuation_Line |
       PP_LINE_TYPE == Continuation_Line)    {
      idx = NXT_COL(0);

      if (PP_LINE_TYPE != Continuation_Line      &&
          PP_LINE_TYPE != Dir_Continuation_Line) {
         previous_char = 0;
      }

      /* check for possible format stmt first.         */
      /* Format_idx is both a flag and an array index. */
      /* It is set to -1 when not expecting possible   */
      /* format stmt, set to 0 after encountering a    */
      /* label, and then is incremented up to 7 to test*/
      /* each letter in the string.                    */

      if (format_idx >= 0) {

         if (PP_IDX > cont_col) {
            idx = PP_IDX;
         }
         else {
            idx = cont_col;
         }

         ich = nxt_line[idx];
         while ((format_idx < 7) && (ich)) {

            do {
               ich = nxt_line[++idx];
            }
            while (ich == blank  |  ich == tab);

            if ((ich == newline) | (ich == bang)) {
               PP_EOL = idx;
               break;
            }
            if (! ((ich == format_str[format_idx][0]) |
                   (ich == format_str[format_idx][1]))) {
               format_idx = -1;
               idx--;
               break;
            }
            format_idx++;

         }

         if (format_idx == 7) {
            /* must be format stmt */
            in_format = TRUE;
            format_idx = -1;
            previous_char = ich;
         }
      } /* check for format line */

   /* mark all characters in character constant */
      if (format_idx < 0) {

         if (PP_LINE_TYPE != Continuation_Line      &&
             PP_LINE_TYPE != Dir_Continuation_Line) {
            char_delim = 0;
            digit_start = 0;
            seen_lp_eq_slash = FALSE;
         }

         if (idx == NXT_COL(0)) {
            idx = PP_IDX;
         }

         if (PP_LINE_TYPE == Continuation_Line &&
             prev_char_delim != 0               &&
             idx == cont_col                    &&
             nxt_line[idx + 1] == prev_char_delim) {

            /* set the character context flag on both this char */
            /* and the last one on the previous line.           */

            if (nxt_line_num_lines > 1) {
               MARK_CHAR_CONST(nxt_line[prev_char_delim_idx]);
            }
            else {
               MARK_CHAR_CONST(stmt_buf[stmt_line_start_idx[lines_in_buf] + 
                                     prev_char_delim_idx - 1]);
            }
            idx++;
            MARK_CHAR_CONST(nxt_line[idx]);
            nxt_line[idx] |= (1 << 9);

            /* reset char_delim to the previous char_delim */
            /* we are still in character context.          */

            char_delim = prev_char_delim;
         }

         prev_char_delim = 0;

         /* Char_delim will hold either the character that     */
         /* is the string delimiter, or negative the hollerith */
         /* count.                                             */

# pragma _CRI align
         do {
            while (char_delim == 0) {
               ich = nxt_line[++idx];

               /* skip thru blanks  */

               while ((ich == blank) |
                      (ich == tab))  {
                  ich = nxt_line[++idx];
               }

               /* Make tests on significant character.*/
               /* These tests are order sensitive!!   */

               if ((ich == newline) |
                   (ich == bang))   {		/* done for now. */
                  PP_EOL = idx;
                  idx = 0;
                  break;
               } 

               else if (ich == semi_colon) {		/* end of stmt   */
                  digit_start = 0;
                  in_format = FALSE;
                  seen_lp_eq_slash = FALSE;
               }

               else if ((ich == quote)     |
                        (ich == db_quote)) {		/* begin of ch const */
                  char_delim = ich;
                  digit_start = 0;
               }

               else if ((in_format) && (ich == star)) { /* also ch const */
                  char_delim = ich;
                  digit_start = 0;
               }

               else if (ich != EOF && 
                        ch_class[(char)ich] == Ch_Class_Digit) { /*possible holl*/

                  if (digit_start == 0) {

                     if (PP_LINE_TYPE != Continuation_Line      &&
                         PP_LINE_TYPE != Dir_Continuation_Line) {
                        digit_start = idx;
                     } 
                     else if (nxt_line_num_lines > 1) {
                        digit_start = idx;
                     }
                     else {
                        digit_start = stmt_line_end_idx[lines_in_buf] + idx;
                     }
                     num_idx = 0;
                  }
                  else {
                     num_idx++;
                  }
                  if (num_idx <= 4) {
                     num_str[num_idx] = ich;
                     num_str[num_idx + 1] = '\0';
                  }
                  /* if num_idx > 4 we've got some sort of error */
                  
                  continue;
               }

               else if ((ich == lparen) |      /* record weve seen these */
                        (ich == equal)  |
                        (ich == slash)) {
                  seen_lp_eq_slash = TRUE;
                  digit_start = 0;
               }

               /* check for hollerith, already seen number, (not label) */

               else if ((digit_start) && (previous_char)) {
                  if (in_format) {
                     if ((ich == lc_h) | (ich == uc_h)) {
                        /* have hollerith */
                        sscanf(num_str, "%d",&char_delim);
                        char_delim = - char_delim;
                     }
                  }
                  else if ((ich == lc_h)  |
                           (ich == lc_r)  |
                           (ich == lc_l)  |
                           (ich == uc_h)  |
                           (ich == uc_r)  |
                           (ich == uc_l)) {

                     if (previous_char == star) {

                        if (seen_lp_eq_slash) {
                           sscanf(num_str,"%d",&char_delim);
                           char_delim = - char_delim;
                        }
                     }
                     else if (previous_char == EOF) {

                        /* definately have hollerith */

                        sscanf(num_str,"%d",&char_delim);
                        char_delim = - char_delim;  
                     }
                     else if (ch_class[previous_char] != Ch_Class_Letter &&
                              previous_char != dollar &&
                              previous_char != at_sign &&
                              previous_char != underscore) {
                           sscanf(num_str,"%d",&char_delim);
                           char_delim = - char_delim;  
                     }
                  }
                  digit_start = 0;

               } /* if digit_start */

               /* end of tests, record this sig char     */

               previous_char = ich;

            } /* while (char_delim == 0) */
            

            /* found char constant so mark the characters */

            if (char_delim) {
               previous_char = ' ';

               /* char_delim < 0 means hollerith, - (length) is stored */

               if (char_delim < 0) {
                  /* I've got hollerith */
                  /* idx => h, r, or l */
                  for (; char_delim < 0; char_delim++) {
                     if (nxt_line[++idx] == newline) {
                        if (idx <= NXT_COL(PP_ORIG_SIZE)) {
                           shift_to_line_size((NXT_COL(PP_ORIG_SIZE)-idx)+1);
                           nxt_line[idx] = marked_blank;
                           for (i = idx + 1; i <= NXT_COL(PP_ORIG_SIZE); i++) {
                              nxt_line[i] = blank;
                           }
                           nxt_line[i] = newline;
                           PP_EOL = NXT_COL(PP_ORIG_SIZE) + 1;
                        }
                        else {
                           PP_EOL = idx;
                           idx = 0;
                           break;
                        }
                     }
                     else {
                        MARK_CHAR_CONST(nxt_line[idx]);
                     }
                  } /* for */
               }
               else {
                  
                  /* char_delim holds delimiter  */

                  while (TRUE) {
                     if (nxt_line[++idx] == newline) {
                        if (idx <= NXT_COL(PP_ORIG_SIZE)) {
                           shift_to_line_size((NXT_COL(PP_ORIG_SIZE)-idx)+1);
                           /* pad with blanks */
                           for (; idx <= NXT_COL(PP_ORIG_SIZE); idx++) {
                              nxt_line[idx] = marked_blank;
                           }
                           nxt_line[idx] = newline;
                        }
                        PP_EOL = NXT_COL(PP_ORIG_SIZE) + 1;
                        idx = 0;
                        break;
                     }
                     else if (nxt_line[idx] == char_delim) {
                        if (nxt_line[idx + 1] == char_delim) {
                           MARK_CHAR_CONST(nxt_line[idx]);
                           ++idx;
                           MARK_CHAR_CONST(nxt_line[idx]);
                           nxt_line[idx] |= (1 << 9);
                        }
                        else if (nxt_line[idx + 1] == newline &&
                                 idx == NXT_COL(PP_ORIG_SIZE)) {

                           prev_char_delim = char_delim;
                           prev_char_delim_idx = idx;
                           char_delim = 0;
                           break;
                        }
                        else {
                           /* end of char constant */
                           char_delim = 0;
                           break;
                        }
                     } else {
                        MARK_CHAR_CONST(nxt_line[idx]);
                     }
                  } /* while (1) */
               }

            } /* if char_delim != 0 */
         }
         while (idx);

      } /* if format_idx < 0 .... mark character constants */
   }  /* if reg or continue line */

   /* check regular lines for INCLUDE "filename" line */
   if (PP_LINE_TYPE == Regular_Line) {
      nxt_idx = cont_col;				/* beginning of stmt  */

      do {						/* get 1st non-blank  */
         ch = nxt_line[++nxt_idx];
      }
      while (ch == blank  |  ch == tab);		/* skip white space   */

      if (ch == uc_i |  ch == lc_i) {		/* possible include   */
	 incl_idx = NULL_IDX;

         do {
            ch_tmp = ch;
            if (islower(ch_tmp)) {			/* lowercase char     */
               ch = TOUPPER(ch_tmp);			/* cnvrt lwr to upr   */
	    }
	    include_file[incl_idx++] = ch;

	    do {					/* get next non-blank */
	       ch = nxt_line[++nxt_idx];
	    }
	    while (ch == blank  |  ch == tab);	/* skip white space   */
	 }
	 while (incl_idx < 7	 &&  ch != eos);	/* get 7 chars or EOS */

	 include_file[incl_idx] = EOS;		/* terminate text str */
 
	 if (EQUAL_STRS(include_file, "INCLUDE")) {	/* match INCLUDE str  */
	    if (ch == quote  |  ch == db_quote) {	/* have include line  */
	       PP_LINE_TYPE = Comment_Line;

	       /* get requested file name */
	       delim	   = ch;			/* save delimiter     */
	       incl_idx = NULL_IDX;

	       while ((ch = nxt_line[++nxt_idx]) != delim && ch != eos) {
		  if (incl_idx < MAX_FILE_NAME_SIZE) {
		     include_file[incl_idx++] = ch;
		  }
		  else if (incl_idx == MAX_FILE_NAME_SIZE) {

		     /* Include file name length exceeds maximum. */

	             ntr_next_msg_queue(PP_LINE_NUM, 57, Error,
                                   nxt_line_col[nxt_idx],
			           (char *)NULL,
                                   (MAX_FILE_NAME_SIZE - 1),
                                   ARG_ARG);
	          }
	       }
	       include_file[incl_idx] = eos;		/* terminate file name*/

	       if (incl_idx == NULL_IDX) { /* Include file name missing. */
	          ntr_next_msg_queue(PP_LINE_NUM, 58, Error,
                                nxt_line_col[nxt_idx], 
                                (char *)NULL,
                                0,
                                NO_ARG);
	       }
	       else if (ch == eos) { /* Missing delimiter on include file name*/
	          ntr_next_msg_queue(PP_LINE_NUM, 59, Error,
                                nxt_line_col[NXT_COL(PP_ORIG_SIZE)],
                                (char *)NULL,
                                0,
                                NO_ARG);
	       }
	       else {				/* check for comments */

		  do {
		     ch = nxt_line[++nxt_idx];
		  }
		  while (ch == blank	 |  ch == tab);/* skip white space   */

		  if (ch != newline  &&  ch != bang) {  /* end of line     */

		     /* Text following include file name is not a comment. */

		     ntr_next_msg_queue(PP_LINE_NUM, 60, Error,
                                   nxt_line_col[nxt_idx],
                                   (char *)NULL,
                                   0,
                                   NO_ARG);
		  }
		  else if (PP_IDX < cont_col) { /* check for label   */

		     /* Include line must not contain a statement label. */

		     ntr_next_msg_queue(PP_LINE_NUM, 61, Error,
                                   nxt_line_col[PP_IDX],
                                   (char *)NULL,
                                   0,
                                   NO_ARG);
		  }
		  else {				/* valid include line */
		     PP_LINE_TYPE = Include_Line;
	          }
	       }  /* else */
            }  /* if */
         }  /* if */
      }  /* if */
   }	 /* if */

   if (!issue_obsolete_src_form_msg && !PP_CHANGE_SOURCE_FORM &&
       issue_classify_msg) {
      issue_obsolete_src_form_msg = TRUE;
      ntr_next_msg_queue(PP_LINE_NUM, 1582,
#ifdef KEY /* Bug 318, 321 */
      			 Ansi,
#else /* KEY Bug 318, 321 */
      			 Comment,
#endif /* KEY Bug 318, 321 */
			 0,
                         (char *)NULL,
                         0,
                         NO_ARG);
   }

   TRACE (Func_Exit, "fixed_classify_line", line_type_str[PP_LINE_TYPE]);

   return;

}  /* fixed_classify_line */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Calls read_line to get the next line image from the currently         *|
|*	active source file.  An EOF line is returned when end of the input    *|
|*	source file is encountered.  Include lines cause a file switch.	      *|
|*	End of include files are treated as comments.			      *|
|*									      *|
|*	The line is classified as a comment, include, dir, regular, or	      *|
|*	continuation line by calling free_classify_line.  Checks involving    *|
|*	relationships between lines (eg. continued followed by continuation)  *|
|*	are performed here.  Comment and include lines are not returned.      *|
|*									      *|
|* Input parameters:							      *|
|*	expected_line		Regular_Line or Continuation_Line expected.   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void free_get_stmt (void)

{
   int idx;
   int line_counter = 1;
   int loc_stmt_num;
   int save_idx;
   int stmt_buf_EOS;

   TRACE (Func_Entry, "free_get_stmt", NULL);

   /* Issue any deferred src_input messages.                                  */

   issue_deferred_msgs();


   if (stmt_buf_type == EOF_Line) {
      /* Attempt to read past end of file */
      PRINTMSG (0, 50, Internal, 0);			/* all done now	      */
   }

   stmt_line_idx = NULL_IDX;
   stmt_buf_idx = NULL_IDX;
   lines_in_buf = 0;
   label_ok = TRUE;

   /* loop while stmt continues */
# pragma _CRI align
   do {

      save_idx = 0;

      /* add nxt_line to stmt_buf */

      stmt_line_num[++stmt_line_idx] = nxt_line_num;

      /* stmt_line_offset holds offset to first significant char of line */

      if (starting_pt) {
         stmt_line_offset[stmt_line_idx] = starting_pt - 2;
      }
      else {
         stmt_line_offset[stmt_line_idx] = nxt_line_idx - 1;
      }

      /* stmt_line_start_idx points to where line starts in stmt_buf */

      stmt_line_start_idx[stmt_line_idx] = line_counter;

      /* stmt_line_end_idx points to newline, bang or ampersand */

      stmt_line_end_idx[stmt_line_idx] = nxt_line_EOL + line_counter - 1;

      /* record the statement type                                 */

      if (nxt_line_type != Continuation_Line      &&
          nxt_line_type != Dir_Continuation_Line) {
         stmt_buf_type = nxt_line_type;

         if (nxt_line_type == Dir_Line) {
            stmt_prefix_len = nxt_line_prefix_len;
            stmt_buf_dir_prefix = nxt_line_dir_prefix;
         }

         if (stmt_buf_type != Comment_Line &&
             stmt_buf_type != Pound_Src_Line &&
             stmt_buf_type != Pound_Include_Exit_Line) {
            INCREMENT_STATEMENT_NUMBER;
         }
      }

      if (stmt_buf_type == Dir_Line) {
         line_dir_prefix[stmt_line_idx] = nxt_line_actual_dir_prefix;
      }

      move_up_next_msg_queue();

      /* copy nxt_line to proper part of stmt_buf, backwards       */

      stmt_buf_idx = line_counter + nxt_line_EOL - 1;
      line_counter += nxt_line_EOL;

      for (idx = nxt_line_EOL; idx > 0; idx --) {
         stmt_buf[stmt_buf_idx] = nxt_line[NXT_COL(idx)];
         stmt_buf_col[stmt_buf_idx] = nxt_line_col[NXT_COL(idx)];
         stmt_buf_idx--;

         if (havent_issued_tab_ansi &&
             idx < nxt_line_EOL     &&
             nxt_line[NXT_COL(idx)] == tab) {
            havent_issued_tab_ansi = FALSE;
            ntr_msg_queue(nxt_line_num, 899, Ansi,
                          nxt_line_col[NXT_COL(idx)],
                          (char *)NULL,
                          0,
                          NO_ARG);
         }

         if (nxt_line[NXT_COL(idx)] == semi_colon &&
             stmt_buf_type != Dir_Line) {
            if ((idx > starting_pt) && (idx < nxt_line_EOL)) {
               save_idx = idx;
            }
         }
      }
 
      lines_in_buf++;

      /* save_idx is the point in nxt_line where the next ';' is */
      /* If there is a semi_colon then I don't replace nxt_line  */
      /* or classify it again. I simply recopy nxt_line into the */
      /* stmt_buf and start processing after the ;.              */
      /* Must check if nxt_line_type is a comment line because   */
      /* when an include file ends, comment lines come through.  */

      if (save_idx &&
          nxt_line_type != Comment_Line) {

         stmt_buf_EOS = save_idx;
         idx = save_idx;
         while ((nxt_line[NXT_COL(idx)] == semi_colon) |
                (nxt_line[NXT_COL(idx)] == blank)      |
                (nxt_line[NXT_COL(idx)] == tab))       {
            idx++;
         }

         /* starting_pt is the idx I start processing the next time around */

         starting_pt        = idx;
         nxt_line_type      = Regular_Line;
         continuation_count = 0;		/* clear cont counter */
         include_found	    = FALSE;	/* and include flags  */
         include_complete   = FALSE;
      }
      else {
         stmt_buf_EOS = nxt_line_EOL;
         starting_pt = NULL_IDX;
      }


      /* don't read past end of source file */
      /* and don't get new nxt_line if had semi-colon. */

      if ((stmt_buf_type != EOF_Line) &&
          (starting_pt == NULL_IDX))       {

         /* get next line from src input file */
         do {

            nxt_line_type = Regular_Line;

	    if (get_nxt_line ()) {		/* read next src line */

	       if (include_switch) {
	          update_global_line ();	/* enter global_line_tbl */
	          include_switch = FALSE;
	       }

               if (issue_pound_exit_line) {
                  OUTPUT_POUND_INCLUDE_EXIT_LINE(curr_glb_line);
                  issue_pound_exit_line = FALSE;
               }
   
               nxt_line_mp_line = FALSE;

               if (nxt_line_type != Cond_Comp_Line) {
                  PP_ORIG_SIZE = line_size;
                  classify_line();
               }

               if (on_off_flags.save_dot_i) {

                  if (ignore_source_line ||
                      nxt_line_type == Cond_Comp_Line ||
                      nxt_line_type == Include_Line) {

                     /* print blank line */
                     fprintf(dot_i_fptr, "\n");
                     previous_global_line++;
                  }
                  else {
                     print_nxt_line();
                  }
               }

	       switch (nxt_line_type) {
	          case Comment_Line:			/* ignore comments    */
		     break;
   
                  case Cond_Comp_Line:
                     if (parse_cc_line()) {

                        /* if result is true, then it was an include line */

                        nxt_line_type = Include_Line;
                        include_stmt_file_line = SRC_STK_FILE_LINE(src_stk_idx);

#ifdef KEY /* Bug 10151 */
                        if (open_include_file (TRUE, FALSE))
#else /* KEY Bug 10151 */
                        if (open_include_file (TRUE))
#endif /* KEY Bug 10151 */
			{
                           include_found  = TRUE;      /* flag begin of file */
                           include_switch = TRUE;      /* flag file switch   */
                        }
                     }
                     else {
                        nxt_line_type = Comment_Line;
                     }
                     angle_brkt_include = FALSE;
                     break;

	          case Dir_Line:
	          case Regular_Line:
		     continuation_count = 0;		/* clear cont counter */
		     include_found	     = FALSE;	/* and include flags  */
		     include_complete   = FALSE;
		     break;
   
	          case Continuation_Line:
	          case Dir_Continuation_Line:

		     if (++continuation_count == MAX_ANSI_FREE_LINES) {

		      /* Too many continuation lines is non-standard in form. */

		        ntr_msg_queue(nxt_line_num, 52, Ansi,
                                      nxt_line_idx,
                                      "free",
                                      (MAX_ANSI_FREE_LINES - 1),
                                      ARG_STR_ARG);
		     }

                     if (continuation_count == MAX_FREE_LINES) {

                        /* this is it. I cn give ya na more powr cap'n */

                        ntr_msg_queue(nxt_line_num, 525, Error,
                                      CONTINUE_COLUMN,
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
                     }

                     if (continuation_count >= MAX_FREE_LINES) {
                        nxt_line_type = Comment_Line;
                        break;
                     }
   
		     if (include_found) {

		       /* First line of included file must not be a cont line */
   
		        ntr_msg_queue(nxt_line_num, 53, Error,
                                      nxt_line_idx,
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
   
		        include_found = FALSE;
		     }
       
		     if (include_complete) {

		     /* Nxt line of file after include must not be a cont line*/
   
		        ntr_msg_queue(nxt_line_num, 54, Error,
                                      nxt_line_idx,
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
   
		        include_complete = FALSE;
		     }

                     if (cif_flags & MISC_RECS) {
                        cif_cont_line_rec(
                           (nxt_line_type == Continuation_Line) ? 0 : 1,
                           nxt_line_num);
                     }

		     break;
       
                  case Pound_Src_Line:
                     break;

                  case Pound_Include_Exit_Line:
                     nxt_line_type         = Comment_Line;
                     curr_glb_line--;
                     SRC_STK_FILE_LINE(src_stk_idx)--;
                     GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                               SRC_STK_FILE_LINE(src_stk_idx);
                     set_related_gl_source_lines(
                                     SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));

                     if (source_form != SRC_STK_PREV_SRC_FORM(src_stk_idx)) {
                        change_source_form = TRUE;
                     }
                     POP_SRC;
                     include_switch = TRUE;
                     break;

                  case Pound_Include_Enter_Line:
	          case Include_Line:

                     include_stmt_file_line = SRC_STK_FILE_LINE(src_stk_idx);

#ifdef KEY /* Bug 10151 */
		     if (open_include_file (FALSE,
		       Pound_Include_Enter_Line == nxt_line_type))
#else /* KEY Bug 10151 */
		     if (open_include_file (FALSE))
#endif /* KEY Bug 10151 */
		     {
                        if( nxt_line_type == Include_Line ) {
		          include_found  = TRUE;		/* flag begin of file */
                        }
		        include_switch = TRUE;		/* flag file switch   */
		     }
		     break;
	       }  /* switch */
	    }
	    else {					/* EOF on source file */

               /* need to bump pp_line_idx since classify_line was not called */

               if (cmd_line_flags.pp_macro_expansion) {
                  pp_line_idx++;
               }

               if (expected_line == Continuation_Line) {
                  ntr_msg_queue(stmt_line_num[lines_in_buf], 539, Error, 
                                stmt_line_end_idx[lines_in_buf] - 
                                          stmt_line_start_idx[lines_in_buf] + 1,
                                (char *)NULL,
                                0,
                                NO_ARG);
               }

	       /* check for termination of include file */

	       if (src_stk_idx > SRC_STK_BASE_IDX) {	/* curr src is include*/
	          include_complete 	= TRUE;		/* flag end of file   */
	          nxt_line_type		= Comment_Line;	/* make EOF a comment */
                  nxt_line_EOL = 2;
                  curr_glb_line--;            /* don't count this line */
   
                  GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                            SRC_STK_FILE_LINE(src_stk_idx);
                  set_related_gl_source_lines(
                                     SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));

                  if (source_form != SRC_STK_PREV_SRC_FORM(src_stk_idx)) {
                     change_source_form = TRUE;
                  }
	          POP_SRC;
	          include_switch = TRUE;		/* flag file switch   */
                  issue_pound_exit_line = TRUE;
                  break;
	       }
	       else {					/* curr src is input  */
                  GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                            SRC_STK_FILE_LINE(src_stk_idx);
                  set_related_gl_source_lines(
                                     SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));
	          nxt_line_type = EOF_Line;		/* end of compilation */
                  nxt_line_EOL = 2;
	       }
	    }
         }
         while (nxt_line_type == Comment_Line | 
                nxt_line_type == Include_Line |
                nxt_line_type == Pound_Include_Enter_Line);
      }
   }
   while (nxt_line_type == Continuation_Line ||
          nxt_line_type == Dir_Continuation_Line);

   if (prev_statement_number != statement_number) {
      loc_stmt_num = statement_number;
      statement_number = prev_statement_number;
      prev_statement_number = loc_stmt_num;
   }

   stmt_buf_EOS_idx = stmt_line_start_idx[lines_in_buf] + stmt_buf_EOS - 1;
   stmt_EOS_la_ch.line = stmt_line_num[lines_in_buf];
   stmt_EOS_la_ch.column = stmt_buf_col[stmt_buf_EOS_idx];
   stmt_EOS_la_ch.stmt_buf_idx = stmt_buf_EOS_idx;
   stmt_EOS_la_ch.stmt_num = statement_number;

   stmt_buf_idx = stmt_line_offset[1] + 1;
   stmt_line_idx = SRC_STK_BASE_IDX;

   PRINT_STMT_SRC();	/* If DEBUG and -u src or -u stmt set print source */

   TRACE (Func_Exit, "free_get_stmt", NULL);

   return;

}  /* free_get_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	The line in the input buffer is classified as a comment, include,     *|
|*	dir, continuation or regular line.  Free source form rules apply.     *|
|*									      *|
|* Input parameters:							      *|
|*	expected_line		Regular_Line or Continuation_Line expected.   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void free_classify_line (void)

{
   int		ch;
   char		delim;
   char         form[5] = "     ";
   boolean	had_amp = FALSE;
   int          i;
   int          ich;
   int          idx;
   int		incl_idx;
   boolean	label_found	= FALSE;
   int          line_num;
   char         line_num_str[24];
   int		save_curr_glb_line;
   int		src_idx;
   int          sv_amp;



   TRACE (Func_Entry, "free_classify_line", NULL);

   PP_EOL = nxt_line_end_idx[pp_line_idx] - 1;
   PP_EXPECTED_LINE = expected_line;

START:

   ch = nxt_line[NXT_COL(1)];

# ifdef _FRONTEND_CONDITIONAL_COMP
   if (ch == pound  &&
       on_off_flags.preprocess) {

      PP_LINE_TYPE = Cond_Comp_Line;
   } else
# endif
   if (is_pound_line_dir()) {
      PP_IDX = NXT_COL(2);
      ch = nxt_line[PP_IDX];

      while (ch == blank | ch == tab) {
         PP_IDX++;
         ch = nxt_line[PP_IDX];
      }

      if (isdigit(ch)) {
         idx = 0;
         line_num_str[idx++] = ch;
         ch = nxt_line[++PP_IDX];
         while (isdigit(ch)) {
            line_num_str[idx++] = ch;
            ch =  nxt_line[++PP_IDX];
         }

         line_num_str[idx] = '\0';
         line_num = atoi(line_num_str);

         while (ch == blank | ch == tab) {
            PP_IDX++;
            ch = nxt_line[PP_IDX];
         }

         if (ch == quote | ch == db_quote) {
            char_delim = ch;
            ch =  nxt_line[++PP_IDX];

            idx = 0;
            while (ch != char_delim) {
               include_file[idx++] = ch;
               ch =  nxt_line[++PP_IDX];
            }
            include_file[idx] = '\0';
            char_delim = 0;

            ch =  nxt_line[++PP_IDX];
            while (ch == blank | ch == tab) {
               PP_IDX++;
               ch = nxt_line[PP_IDX];
            }

            if (ch == '1') {
               PP_LINE_TYPE = Pound_Include_Enter_Line;
            }
            else if (ch == '2') {
               PP_LINE_TYPE = Pound_Include_Exit_Line;
            }
            else {
               PP_LINE_TYPE = Comment_Line;

               /* reset the curr_glb_line */
               /* line_num is a file line, not a global line */

               save_curr_glb_line = curr_glb_line;
               curr_glb_line = line_num + GL_GLOBAL_LINE(global_line_tbl_idx) -
                             GL_FILE_LINE(global_line_tbl_idx);
               curr_glb_line--;

               if (first_pound_line) {
                  PP_LINE_TYPE = Pound_Src_Line;
                  change_orig_src_file = TRUE;
                  strcpy(pound_file, include_file);
               }
               else {
                  SRC_STK_FILE_LINE(src_stk_idx) +=
                                 curr_glb_line - save_curr_glb_line;
               }
            }

            first_pound_line = FALSE;

         }
         else if (ch == newline) {

            PP_LINE_TYPE = Comment_Line;

            /* reset the curr_glb_line */
            /* line_num is a file line, not a global line */

            save_curr_glb_line = curr_glb_line;
            curr_glb_line = line_num + GL_GLOBAL_LINE(global_line_tbl_idx) -
                          GL_FILE_LINE(global_line_tbl_idx);
            curr_glb_line--;

            SRC_STK_FILE_LINE(src_stk_idx) +=
                           curr_glb_line - save_curr_glb_line;

            first_pound_line = FALSE;
         }
         else {
            PP_LINE_TYPE = Comment_Line;
         }
      }
      else {
         PP_LINE_TYPE = Comment_Line;
      }
   }
   else if (ignore_source_line) {
      PP_LINE_TYPE = Comment_Line;
   }
   else {

   PP_IDX = NXT_COL(NULL_IDX);				/* beginning of line  */

   do {							/* find 1st non-blank */
      ch = nxt_line[++PP_IDX];
   }
   while (ch == blank  ||  ch == tab);			/* skip white space   */

   switch (ch) {					/* handle special ch  */
   case BANG:						/* directive ?	      */
      if (((ch = nxt_line[PP_IDX+1]) == uc_d      || ch == lc_d) &&
	  ((ch = nxt_line[PP_IDX+2]) == uc_i      || ch == lc_i) &&
	  ((ch = nxt_line[PP_IDX+3]) == uc_r      || ch == lc_r) &&
	  ((ch = nxt_line[PP_IDX+4]) == dollar    || ch == at_sign)) {

         PP_PREFIX_LEN = 4;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+4]);

         if (nxt_line[PP_IDX+5] == zero) {
            nxt_line[PP_IDX+5] = blank;
         }

         if (issue_classify_msg &&
             expected_line == Continuation_Line) {

            ntr_next_msg_queue(PP_LINE_NUM, 1656, Error, nxt_line_col[PP_IDX],
                               (char *)NULL,
                               0,
                               NO_ARG);
            expected_line = Regular_Line;
            PP_EXPECTED_LINE = expected_line;
         }

         if ((ch = nxt_line[PP_IDX+5]) != blank && ch != tab) {
         
            if (IS_DIR_CONTINUATION(Cdir_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cdir_Dir;
               in_format = FALSE;
               PP_IDX  = PP_IDX+5;
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {
	    PP_IDX++;				/* skip '!' char      */
	    PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cdir_Dir;
            PP_ACTUAL_DIR_PREFIX = Cdir_Dir;
            in_format = FALSE;

            idx = PP_IDX + 4;
            do {
               ch = nxt_line[++idx];
            }
            while (ch == blank | ch == tab);

            if (ch == lc_f | ch == uc_f) {
               for (i = 0; i < 5; i++) {
                  ch = nxt_line[++idx];
   
                  if (ch == newline) {
                     break;
                  }
   
                  if (islower(ch)) {  
                     form[i] = TOUPPER(ch);
                  }
                  else {
                     form[i] = ch;
                  }
               }
   
               if (strncmp(form, "IXED ", 5) == 0 &&
                   !disregard_directive[Tok_Dir_Fixed-Tok_Dir_Start]) {
                  PP_CHANGE_SOURCE_FORM = TRUE;

                  if (!issue_obsolete_src_form_msg && issue_classify_msg) {
                     ntr_next_msg_queue(PP_LINE_NUM, 1582,
#ifdef KEY /* Bug 318, 321 */
		                        Ansi,
#else /* KEY Bug 318, 321 */
		                        Comment,
#endif /* KEY Bug 318, 321 */
					0,
                                        (char *)NULL,
                                        0,
                                        NO_ARG);
                     issue_obsolete_src_form_msg	= TRUE;
               }
            }
            }
         }
      }
      else if (((ch = nxt_line[PP_IDX+1]) == uc_m   || ch == lc_m) &&
	       ((ch = nxt_line[PP_IDX+2]) == uc_i   || ch == lc_i) &&
	       ((ch = nxt_line[PP_IDX+3]) == uc_c   || ch == lc_c) &&
	       ((ch = nxt_line[PP_IDX+4]) == dollar || ch == at_sign)) {
      
         PP_PREFIX_LEN = 4;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+4]);

         if (issue_classify_msg &&
             expected_line == Continuation_Line) {

            ntr_next_msg_queue(PP_LINE_NUM, 1656, Error, nxt_line_col[PP_IDX],
                               (char *)NULL,
                               0,
                               NO_ARG);
            expected_line = Regular_Line;
            PP_EXPECTED_LINE = expected_line;
         }

         if ((ch = nxt_line[PP_IDX+5]) != blank && ch != tab) {
          
            if (IS_DIR_CONTINUATION(Cmic_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cmic_Dir;
               in_format = FALSE;
               PP_IDX  = PP_IDX+5;
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {
	    PP_IDX++;				/* skip '!' char      */
	    PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cmic_Dir;
            PP_ACTUAL_DIR_PREFIX = Cmic_Dir;
            in_format = FALSE;

            idx = NXT_COL(6);
         }
      }
      else if (((ch = nxt_line[PP_IDX+1]) == dollar) &&
               ((ch = nxt_line[PP_IDX+2]) == uc_o   || ch == lc_o) &&
               ((ch = nxt_line[PP_IDX+3]) == uc_m   || ch == lc_m) &&
               ((ch = nxt_line[PP_IDX+4]) == uc_p   || ch == lc_p)) {

         /* !$omp */

         PP_PREFIX_LEN = 4;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+1]);

         if (issue_classify_msg &&
             dump_flags.open_mp &&
             expected_line == Continuation_Line) {

            ntr_next_msg_queue(PP_LINE_NUM, 1656, Error, nxt_line_col[PP_IDX],
                               (char *)NULL,
                               0,
                               NO_ARG);
            expected_line = Regular_Line;
            PP_EXPECTED_LINE = expected_line;
         }

         if (expected_line == Dir_Continuation_Line) {

            PP_EXPECTED_LINE = Regular_Line;

            if (IS_DIR_CONTINUATION(Comp_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Comp_Dir;
               in_format = FALSE;
               PP_IDX  = PP_IDX+5;

               src_idx = PP_IDX;

               do {
                  ch = nxt_line[++src_idx];           /* get next src char  */
               }
               while (ch == blank  ||  ch == tab);

               if (ch == AMP) {

                  PP_IDX = src_idx;

                  do {
                     ch = nxt_line[++src_idx];
                  }
                  while (ch == blank  ||  ch == tab);
   
                  if (ch == newline  ||  ch == bang) {
                     PP_EOL = src_idx;
   
                     /* Cont lines must contain text following */
                     /* the & in free src form. */
   
                     if (issue_classify_msg) {
                        ntr_next_msg_queue(PP_LINE_NUM, 71, Ansi,
                                      nxt_line_col[src_idx],
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
                     }
         
                     PP_LINE_TYPE = Comment_Line;
                  }

                  had_amp = TRUE;
               }
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else if ((ch = nxt_line[PP_IDX+5]) != blank && ch != tab) {
            PP_LINE_TYPE = Comment_Line;
         }
         else {
            idx = PP_IDX+5;
            PP_IDX++;                             /* skip '!' char      */
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Comp_Dir;
            PP_ACTUAL_DIR_PREFIX = Comp_Dir;
            in_format = FALSE;

# if ! defined(_TARGET_OS_MAX)
            if (! dump_flags.open_mp &&
                ! on_off_flags.preprocess_only) {
               PP_LINE_TYPE = Comment_Line;
            }
# endif
         }
      }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      else if (((ch = nxt_line[PP_IDX+1]) == dollar) &&
               ((ch = nxt_line[PP_IDX+2]) == uc_s   || ch == lc_s) &&
               ((ch = nxt_line[PP_IDX+3]) == uc_g   || ch == lc_g) &&
               ((ch = nxt_line[PP_IDX+4]) == uc_i   || ch == lc_i)) {

         /* !$sgi */

         PP_PREFIX_LEN = 4;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+1]);

         if (issue_classify_msg &&
             dump_flags.open_mp &&
             expected_line == Continuation_Line) {

            ntr_next_msg_queue(PP_LINE_NUM, 1656, Error, nxt_line_col[PP_IDX],
                               (char *)NULL,
                               0,
                               NO_ARG);
            expected_line = Regular_Line;
            PP_EXPECTED_LINE = expected_line;
         }

         if (expected_line == Dir_Continuation_Line) {

            PP_EXPECTED_LINE = Regular_Line;

            if (IS_DIR_CONTINUATION(Comp_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Csgi_Dir;
               in_format = FALSE;
               PP_IDX  = PP_IDX+5;

               src_idx = PP_IDX;

               do {
                  ch = nxt_line[++src_idx];           /* get next src char  */
               }
               while (ch == blank  ||  ch == tab);

               if (ch == AMP) {

                  PP_IDX = src_idx;

                  do {
                     ch = nxt_line[++src_idx];
                  }
                  while (ch == blank  ||  ch == tab);

                  if (ch == newline  ||  ch == bang) {
                     PP_EOL = src_idx;

                     /* Cont lines must contain text following */
                     /* the & in free src form. */

                     if (issue_classify_msg) {
                        ntr_next_msg_queue(PP_LINE_NUM, 71, Ansi,
                                      nxt_line_col[src_idx],
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
                     }

                     PP_LINE_TYPE = Comment_Line;
                  }

                  had_amp = TRUE;
               }
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else if ((ch = nxt_line[PP_IDX+5]) != blank && ch != tab) {
            PP_LINE_TYPE = Comment_Line;
         }
         else {
            idx = PP_IDX+5;
            PP_IDX++;                             /* skip '!' char      */
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Comp_Dir;
            PP_ACTUAL_DIR_PREFIX = Csgi_Dir;
            in_format = FALSE;

            if (! dump_flags.open_mp &&
                ! on_off_flags.preprocess_only) {
               PP_LINE_TYPE = Comment_Line;
            }
         }
      }
# endif
      else if (((ch = nxt_line[PP_IDX+1]) == dollar) &&
               ((ch = nxt_line[PP_IDX+2]) == uc_p   || ch == lc_p) &&
               ((ch = nxt_line[PP_IDX+3]) == uc_a   || ch == lc_a) &&
               ((ch = nxt_line[PP_IDX+4]) == uc_r   || ch == lc_r)) {
  
         /* !$par */

         PP_PREFIX_LEN = 4;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+1]);

         if (issue_classify_msg &&
             expected_line == Continuation_Line &&
             dump_flags.mp  &&
             is_par_directive(PP_IDX+5)) {

            ntr_next_msg_queue(PP_LINE_NUM, 1656, Error, nxt_line_col[PP_IDX],
                               (char *)NULL,
                               0,
                               NO_ARG);
            expected_line = Regular_Line;
            PP_EXPECTED_LINE = expected_line;
         }

         if ((ch = nxt_line[PP_IDX+5]) == amp) {

            if (IS_DIR_CONTINUATION(Cpar_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cpar_Dir;
               in_format = FALSE;
               PP_IDX  = PP_IDX+5;
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {
            idx = PP_IDX+5;
            PP_IDX++;                             /* skip '!' char      */
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cpar_Dir;
            PP_ACTUAL_DIR_PREFIX = Cpar_Dir;
            in_format = FALSE;

            if ((! dump_flags.mp  &&
                 ! on_off_flags.preprocess_only) ||
                ! is_par_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
         }
      }
      else if (((ch = nxt_line[PP_IDX+1]) == star) &&
               ((ch = nxt_line[PP_IDX+2]) == dollar) &&
               ((ch = nxt_line[PP_IDX+3]) == star)) {
  
         /* !*$* */

         PP_PREFIX_LEN = 3;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+2]);

         if (issue_classify_msg &&
             expected_line == Continuation_Line &&
             dump_flags.mp &&
             is_star_directive(PP_IDX+4)) {

            ntr_next_msg_queue(PP_LINE_NUM, 1656, Error, nxt_line_col[PP_IDX],
                               (char *)NULL,
                               0,
                               NO_ARG);
            expected_line = Regular_Line;
            PP_EXPECTED_LINE = expected_line;
         }

         if ((ch = nxt_line[PP_IDX+4]) == amp) {

            if (IS_DIR_CONTINUATION(Cstar_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cstar_Dir;
               in_format = FALSE;
               PP_IDX  = PP_IDX+4;
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {
            idx = PP_IDX+4;
            PP_IDX++;                             /* skip '!' char      */
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cstar_Dir;
            PP_ACTUAL_DIR_PREFIX = Cstar_Dir;
            in_format = FALSE;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            if (! is_star_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
# else
            if ((! dump_flags.mp &&
                 ! on_off_flags.preprocess_only) ||
                ! is_star_directive(idx)) {
               PP_LINE_TYPE = Comment_Line;
            }
# endif
         }
      }
      else if ((ch = nxt_line[PP_IDX+1]) == dollar) {
  
         /* !$ */

         PP_PREFIX_LEN = 1;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+1]);

         if ((ch = nxt_line[PP_IDX+2]) == amp &&
             dump_flags.mp                          &&
             IS_DIR_CONTINUATION(Cdollar_Dir)) {

            PP_LINE_TYPE = Dir_Continuation_Line;
            PP_ACTUAL_DIR_PREFIX = Cdollar_Dir;
            in_format = FALSE;
            PP_IDX  = PP_IDX+2;
         }
         else {
            idx = PP_IDX+2;
            PP_IDX++;                             /* skip '!' char      */
            PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cdollar_Dir;
            PP_ACTUAL_DIR_PREFIX = Cdollar_Dir;
            in_format = FALSE;

            if (dump_flags.mp &&
                is_dollar_directive(idx)) {

               if (issue_classify_msg &&
                   expected_line == Continuation_Line) {

                  ntr_next_msg_queue(PP_LINE_NUM, 1656, Error, 
                                     nxt_line_col[PP_IDX],
                                     (char *)NULL,
                                     0,
                                     NO_ARG);
                  expected_line = Regular_Line;
                  PP_EXPECTED_LINE = expected_line;
               }
            }
            else {

               ch = nxt_line[idx];

               if (ch == ' '  ||
                   ch == '\t' ||
                   ch == amp  ||
                   isdigit(ch)) {

                  PP_MP_LINE = TRUE;
               }

               if ((dump_flags.mp ||
                    dump_flags.open_mp || 
                    on_off_flags.preprocess_only) &&
                   ! cmd_line_flags.disregard_conditional_omp &&
                   PP_MP_LINE) {

                  nxt_line[PP_IDX - 1] = ' ';
                  nxt_line[PP_IDX]     = ' ';
                  goto START;
               }
               else {
                  PP_LINE_TYPE = Comment_Line;
                  PP_MP_LINE = FALSE;
               }
            }
         }
      }
# ifdef _DEBUG
      else if (((ch = nxt_line[PP_IDX+1]) == uc_d   || ch == lc_d) &&
	       ((ch = nxt_line[PP_IDX+2]) == uc_b   || ch == lc_b) &&
	       ((ch = nxt_line[PP_IDX+3]) == uc_g   || ch == lc_g) &&
	       ((ch = nxt_line[PP_IDX+4]) == dollar)) {
      
         PP_PREFIX_LEN = 4;

         /* mark the sign bit of the dollar and at_sign */
         MARK_CHAR_CONST(nxt_line[PP_IDX+4]);

         if ((ch = nxt_line[PP_IDX+5]) != blank && ch != tab) {
          
            if (IS_DIR_CONTINUATION(Cdbg_Dir)) {

               PP_LINE_TYPE = Dir_Continuation_Line;
               PP_ACTUAL_DIR_PREFIX = Cdbg_Dir;
               in_format = FALSE;
               PP_IDX  = PP_IDX+5;
            }
            else {
               PP_LINE_TYPE = Comment_Line;
            }
         }
         else {
	    PP_IDX++;				/* skip '!' char      */
	    PP_LINE_TYPE = Dir_Line;
            PP_DIR_PREFIX = Cdbg_Dir;
            PP_ACTUAL_DIR_PREFIX = Cdbg_Dir;
            in_format = FALSE;

            idx = NXT_COL(6);
         }
      }
# endif
      else {
	 PP_LINE_TYPE = Comment_Line;
      }
      break;

   case NEWLINE:					/* blank line	      */
      PP_LINE_TYPE = Comment_Line;
      break;

   case AMP:						/* continuation ?     */
      if (expected_line == Continuation_Line) {		/* expected	      */

	 /* make sure line doesn't begin with '&   \n' or '&   !' */
	 src_idx = PP_IDX;

	 do {
	    ch = nxt_line[++src_idx];		/* get next src char  */
	 }
	 while (ch == blank  ||  ch == tab);		/* skip white space   */
	
	 if (ch == newline  ||  ch == bang) {
            PP_EOL = src_idx;

	    /* Cont lines must contain text following the & in free src form. */

            if (issue_classify_msg) {
	       ntr_next_msg_queue(PP_LINE_NUM, 71, Ansi,
                             nxt_line_col[src_idx],
                             (char *)NULL,
                             0,
                             NO_ARG);
            }

	    PP_LINE_TYPE = Comment_Line;
	 }
	 else {
	    PP_LINE_TYPE = Continuation_Line;
            /* PP_IDX++;  */                      /* skip past &        */
	 }

         had_amp = TRUE;
      }
      else {						/* unexpected	      */
	 /* Continuation line may only follow a line continued with the &. */
         if (issue_classify_msg) {
	    ntr_next_msg_queue(PP_LINE_NUM, 703, Error,
                          nxt_line_col[PP_IDX],
                          (char *)NULL,
                          0,
                          NO_ARG);
         }

	 PP_LINE_TYPE = Comment_Line;
      }
      break;

   default:
      if (expected_line == Continuation_Line) {		/* continue expected  */

         if (char_delim != 0 && issue_classify_msg) {
            /* message about starting & missing */
            ntr_next_msg_queue(PP_LINE_NUM, 505, Error,
                          nxt_line_col[PP_IDX],
                          (char *)NULL,
                          0,
                          NO_ARG);
         }
	 PP_IDX = NXT_COL(NULL_IDX);			/* col 1 is 1st char  */
	 PP_LINE_TYPE = Continuation_Line;
      }
      else {
	 PP_IDX--;				/* reset position     */
	 PP_LINE_TYPE = Regular_Line;

	 /* check regular lines for INCLUDE "filename" line */
	 src_idx = PP_IDX;			/* beginning of stmt  */

	 do {						/* get 1st non-blank  */
	    ch = nxt_line[++src_idx];

	    if (isdigit(ch)) {				/* labeled stmt	      */
	       label_found = TRUE;			/* save for later     */
	       ch = blank;				/* keep looking	      */
	    }
	 }
	 while (ch == blank  ||  ch == tab);		/* skip label/spaces  */

	 if (ch == 'I'  ||  ch == 'i') {		/* possible include   */
	    incl_idx = NULL_IDX;

	    do {
	       if (islower(ch)) {			/* lowercase char     */
		  ch = TOUPPER(ch);			/* cnvrt lwr to upr   */
	       }
	       include_file[incl_idx++] = ch;
	       ch = nxt_line[++src_idx];		/* get next src char  */
	    }
	    while (incl_idx < 7	 &&  ch != eos);	/* get 7 chars or EOS */

	    include_file[incl_idx] = EOS;		/* terminate text str */
 
	    if (EQUAL_STRS(include_file, "INCLUDE")) {	/* match INCLUDE str  */
	       do {					/* get next non-blank */
		  ch = nxt_line[++src_idx];
	       }
	       while (ch == blank  ||  ch == tab);	/* skip white space   */

	       if (ch == quote  ||  ch == db_quote) {	/* have include line  */
		  PP_LINE_TYPE = Comment_Line;

		  /* get requested file name */
		  delim	   = ch;			/* save delimiter     */
		  incl_idx = NULL_IDX;

		  while ((ch = nxt_line[++src_idx]) != delim && ch != eos) {
		     if (incl_idx < MAX_FILE_NAME_SIZE) {
			include_file[incl_idx++] = ch;
		     }
		     else if (incl_idx == MAX_FILE_NAME_SIZE) {

			/* Include file name length exceeds maximum */

			ntr_next_msg_queue(PP_LINE_NUM, 57, Error,
                                      nxt_line_col[src_idx],
			              (char *)NULL,
                                      (MAX_FILE_NAME_SIZE - 1),
                                      ARG_ARG);
		     }
		  }
		  include_file[incl_idx] = EOS;		/* terminate file name*/

		  if (incl_idx == NULL_IDX) {		/* missing file name  */

		     /* Include file name missing. */

		     ntr_next_msg_queue(PP_LINE_NUM, 58, Error,
                                   nxt_line_col[src_idx],
                                   (char *)NULL,
                                   0,
                                   NO_ARG);
		  }
		  else if (ch == eos) {			/* missing delimiter  */

		     /* Missing delimiter on include file name. */

		     ntr_next_msg_queue(PP_LINE_NUM, 59, Error,
                                   nxt_line_col[NXT_COL(PP_ORIG_SIZE)],
                                   (char *)NULL,
                                   0,
                                   NO_ARG);
		  }
		  else {				/* check for comments */

		     do {
			ch = nxt_line[++src_idx];
		     }
		     while (ch == blank	 ||  ch == tab);/* skip white space   */

		     if (ch != newline  &&  ch != bang) {  /* end of line     */

			/* Text following include file name is not a comment. */

			ntr_next_msg_queue(PP_LINE_NUM, 60, Error,
                                      nxt_line_col[src_idx],
                                      (char *)NULL, 
                                      0,
                                      NO_ARG);
		     }
		     else if (label_found) {		/* check for label    */

			/* Include line must not contain a statement label. */

			ntr_next_msg_queue(PP_LINE_NUM, 61, Error,
                                      nxt_line_col[PP_IDX],
                                      (char *)NULL,
                                      0,
                                      NO_ARG);
		     }
		     else {				/* valid include line */
			PP_LINE_TYPE = Include_Line;
		     }
		  }  /* else */
	       }  /* if */
	    }  /* if */
	 }  /* if */
      }	 /* else */
      break;
   }  /* switch */
   }  /* else */

   if (PP_LINE_TYPE != Comment_Line && 
       PP_LINE_TYPE != Pound_Include_Enter_Line &&
       PP_LINE_TYPE != Pound_Include_Exit_Line &&
       PP_LINE_TYPE != Cond_Comp_Line &&
       PP_LINE_TYPE != Dir_Line) {
      PP_EXPECTED_LINE = Regular_Line;
   }

   /* mark character constants */
   /* must check for format stmt after every ; also */

   /* check for possible format stmt first.         */
   /* Format_idx is both a flag and an array index. */
   /* It is set to -1 when not expecting possible   */
   /* format stmt, set to 0 after encountering a    */
   /* label, and then is incremented up to 7 to test*/
   /* each letter in the string.                    */

   /* Char_delim will hold either the character that     */
   /* is the string delimiter, or negative the hollerith */
   /* count.                                             */


   if (PP_LINE_TYPE == Regular_Line          || 
       PP_LINE_TYPE == Dir_Line              ||
       PP_LINE_TYPE == Dir_Continuation_Line ||
       PP_LINE_TYPE == Pound_Include_Enter_Line || 
       PP_LINE_TYPE == Pound_Include_Exit_Line || 
       PP_LINE_TYPE == Continuation_Line)    {

      if (PP_LINE_TYPE != Continuation_Line      &&
          PP_LINE_TYPE != Dir_Continuation_Line) {
         seen_lp_eq_slash = FALSE;
         char_delim = 0;
         digit_start = 0;
         previous_char = 0;
         format_idx = -1;
         in_format = FALSE;
      }

      idx = PP_IDX;

      if (PP_LINE_TYPE == Continuation_Line &&
          prev_char_delim != 0               &&
          had_amp                            &&
          nxt_line[idx + 1] == prev_char_delim) {

         /* set the character context flag on both this char */
         /* and the last one on the previous line.           */

         if (nxt_line_num_lines > 1) {
            MARK_CHAR_CONST(nxt_line[prev_char_delim_idx]);
         }
         else {
            MARK_CHAR_CONST(stmt_buf[stmt_line_start_idx[lines_in_buf] +
                                     prev_char_delim_idx - 1]);
         }
         idx++;
         MARK_CHAR_CONST(nxt_line[idx]);
         nxt_line[idx] |= (1 << 9);

         /* reset char_delim to the previous char_delim */
         /* we are still in character context.          */

         char_delim = prev_char_delim;
      }

      prev_char_delim = 0;


# pragma _CRI align
      do {
         while (char_delim == 0) {
            ich = nxt_line[++idx];

            /* skip thru blanks, noting that a blank kills possible */
            /* format match in free form.                           */

            if (ich == blank | ich == tab) {
               if ((format_idx > 0) && (format_idx < 6)) {
                  format_idx = -1;
               }
               ich = nxt_line[++idx];

               while ((ich == blank) |
                      (ich == tab))  {
                  ich = nxt_line[++idx];
               }
            }

            /* make tests on significant character */

            if ((ich == newline) |
                (ich == bang))   {		/* done for now. */
               PP_EOL = idx;
               idx = 0;
               break;
            }

            else if (ich == semi_colon) {	/* end of stmt   */
               in_format = FALSE;
               format_idx = -1;
               seen_lp_eq_slash = FALSE;
               digit_start = 0;
            }

            else if (ich == amp) {		/* test for continue */
               PP_EOL = idx;
               ich = nxt_line[++idx];

               while ((ich == blank) |
                      (ich == tab))  {
                  ich = nxt_line[++idx];
               }
               if (ich == newline | ich == bang) {

                  if (PP_LINE_TYPE == Dir_Line ||
                      PP_LINE_TYPE == Dir_Continuation_Line)     {

                     PP_EXPECTED_LINE = Dir_Continuation_Line;
                  }
                  else {
                     PP_EXPECTED_LINE = Continuation_Line;
                  }
                  idx = 0;
                  break;
               }
               else {
                  format_idx = -1;
                  idx--;
               }
            }

            else if ((ich == quote)     |
                     (ich == db_quote)) {	/* begin of ch const */
               char_delim = ich;
               digit_start = 0;
               format_idx = -1;
            }

            else if ((in_format) && (ich == star)) { /* also ch const */
               char_delim = ich;
               digit_start = 0;
               format_idx = -1;
            }

            else if (ich != (char) EOF && ch_class[(char) ich] == Ch_Class_Digit) {

               if ((previous_char == 0) | (previous_char == semi_colon)) {

                  /* then this is a label */

                  format_idx = 0;
                  digit_start = -1;
               }
               else if (digit_start >= 0) {
                  format_idx = -1;
                  if (digit_start == 0) {
                     if (PP_LINE_TYPE != Continuation_Line      &&
                         PP_LINE_TYPE != Dir_Continuation_Line) {
                        digit_start = idx;
                     }
                     else if (nxt_line_num_lines > 1) {
                        digit_start = idx;
                     }
                     else {
                        digit_start = stmt_line_end_idx[lines_in_buf] + idx;
                     }
                     num_idx = 0;
                  }
                  else {
                     num_idx++;
                  }
                  if (num_idx <= 4) {
                     num_str[num_idx] = ich;
                     num_str[num_idx + 1] = '\0';
                  }
                  /* if num_idx > 4 we've got some sort of error */

                  continue;
               }
            }

            else if (format_idx >= 0) { 		/* match format str */
               digit_start = 0;
               if (! ((ich == format_str[format_idx][0]) |
                      (ich == format_str[format_idx][1]))) {
                  format_idx = -1;
                  idx--;
                  continue;
               }
               else if (format_idx == 6) {
                  in_format = TRUE;
                  format_idx = -1;
                  seen_lp_eq_slash = TRUE;
               }
               else {
                  format_idx++;
               }
            }

            else if ((ich == lparen) |
                     (ich == equal)  |
                     (ich == slash)) { 		/* record weve seen these */
               seen_lp_eq_slash = TRUE;
               format_idx = -1;
               digit_start = 0;
            }

            /* check for hollerith, already seen number, (not label) */

            else if ((digit_start > 0) && (previous_char)) {
               format_idx = -1;
               if (in_format) {
                  if ((ich == lc_h) | (ich == uc_h)) {
                     /* have hollerith */
                     sscanf(num_str, "%d",&char_delim);
                     char_delim = - char_delim;
                  }
               }
               else if ((ich == lc_h)  |
                        (ich == lc_r)  |
                        (ich == lc_l)  |
                        (ich == uc_h)  |
                        (ich == uc_r)  |
                        (ich == uc_l)) {

                  if (previous_char == star) {

                     if (seen_lp_eq_slash) {
                        sscanf(num_str,"%d",&char_delim);
                        char_delim = - char_delim;
                     }
                  }
                  else if (previous_char == EOF) {
                     sscanf(num_str,"%d",&char_delim);
                     char_delim = - char_delim;
                  }
                  else if (ch_class[previous_char] != Ch_Class_Letter &&
                           previous_char != dollar &&
                           previous_char != at_sign &&
                           previous_char != underscore) {
                     sscanf(num_str,"%d",&char_delim);
                     char_delim = - char_delim;
                  }
               }
               digit_start = 0;

            } /* if digit_start */

            /* end of tests, record this sig char     */

            previous_char = ich;

         } /* while (char_delim == 0) */

         /* found char constant so mark the characters */

         if (char_delim) {
            previous_char = ' ';

            /* char_delim < 0 means hollerith, - (length) is stored */

            if (char_delim < 0) {
               /* I've got hollerith */
               /* idx => h, r, or l */
               for (; char_delim < 0; char_delim++) {
                  if (nxt_line[++idx] == newline) {
                     if (idx <= NXT_COL(PP_ORIG_SIZE)) {
                        shift_to_line_size((NXT_COL(PP_ORIG_SIZE)-idx)+1);
                        nxt_line[idx] = marked_blank;
                        for (i = idx + 1; i <= NXT_COL(PP_ORIG_SIZE); i++) {
                           nxt_line[i] = blank;
                        }
                        nxt_line[i]  = newline;
                        PP_EOL = NXT_COL(PP_ORIG_SIZE) + 1;
                     }
                     else {
                        PP_EOL = idx;
                        idx = 0;
                        break;
                     }
                  }
                  else if (nxt_line[idx] == amp) {
                     PP_EOL = idx;
                     sv_amp = idx;
                     ich = nxt_line[++idx];

                     while ((ich == blank) |
                            (ich == tab))  {
                        ich = nxt_line[++idx];
                     }

                     if (ich == newline) {

                        if (PP_LINE_TYPE == Dir_Line ||
                            PP_LINE_TYPE == Dir_Continuation_Line)     {

                           PP_EXPECTED_LINE = Dir_Continuation_Line;
                        }
                        else {
                           PP_EXPECTED_LINE = Continuation_Line;
                        }
                        idx = 0;
                        break;
                     }
                     else {
                        idx = sv_amp;
                        MARK_CHAR_CONST(nxt_line[idx]);
                     }
                  }
                  else {
                     MARK_CHAR_CONST(nxt_line[idx]);
                  }
               } /* for */
            }
            else {

               /* char_delim holds delimiter  */

               while (1) {
                  if (nxt_line[++idx] == newline) {
                     if (idx <= NXT_COL(PP_ORIG_SIZE)) {
                        shift_to_line_size((NXT_COL(PP_ORIG_SIZE)-idx)+1);
                        /* pad with blanks */
                        for (; idx <= NXT_COL(PP_ORIG_SIZE); idx++) {
                           nxt_line[idx] = marked_blank;
                        }
                        nxt_line[idx] = newline;
                     }
                     PP_EOL = NXT_COL(PP_ORIG_SIZE) + 1;
                     idx = 0;
                     break;
                  }
                  else if (nxt_line[idx] == char_delim) {
                     if (nxt_line[idx + 1] == char_delim) {
                        MARK_CHAR_CONST(nxt_line[idx]);
                        ++idx;
                        MARK_CHAR_CONST(nxt_line[idx]);
                        nxt_line[idx] |= (1 << 9);
                     }
                     else if (nxt_line[idx + 1] == amp) {
                        prev_char_delim = char_delim;
                        prev_char_delim_idx = idx;
                        char_delim = 0;
                        break;
                     }
                     else {
                        /* end of char constant */
                        char_delim = 0;
                        break;
                     }
                  }
                  else if (nxt_line[idx] == amp) {
                     PP_EOL = idx;
                     sv_amp = idx;
                     ich = nxt_line[++idx];

                     while ((ich == blank) |
                            (ich == tab))  {
                        ich = nxt_line[++idx];
                     }

                     if (ich == newline) {

                        if (PP_LINE_TYPE == Dir_Line ||
                            PP_LINE_TYPE == Dir_Continuation_Line) {

                           PP_EXPECTED_LINE = Dir_Continuation_Line;
                        }
                        else {
                           PP_EXPECTED_LINE = Continuation_Line;
                        }
                        idx = 0;
                        break;
                     }
                     else {
                        idx = sv_amp;
                        MARK_CHAR_CONST(nxt_line[idx]);
                     }
                  } 
                  else {
                     MARK_CHAR_CONST(nxt_line[idx]);
                  }
               } /* while (1) */
            }

         } /* if char_delim != 0 */
      }
      while (idx);

   } /* if reg or continue line */

   TRACE (Func_Exit, "free_classify_line", line_type_str[PP_LINE_TYPE]);

   return;

}  /* free_classify_line */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Open_include_file attempts to locate and open the requested include   *|
|*	file.  The src_stk is checked to prevent recursive use of the file.   *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if include file is found and opened successfully, else FALSE     *|
|*									      *|
\******************************************************************************/

#ifdef KEY /* Bug 10151 */
static boolean open_include_file (boolean pound_include_line,
  boolean allow_recursion)
#else /* KEY Bug 10151 */
static boolean open_include_file (boolean pound_include_line)
#endif /* KEY Bug 10151 */

{
   char	       *char_ptr;
   int		cif_file_id;
   int		i;
   int		include_idx;
   int		include_file_len		= 0;
   FILE	       *include_file_ptr		= NULL;
   char		include_path[MAX_PATH_NAME_SIZE];
   int		include_path_len		= 0;
   boolean	more				= TRUE;
   boolean	recursive_use			= FALSE;
   boolean	result				= FALSE;
   int		save_stmt_start_line;
   int		save_stmt_start_col;
   int		src_stk_i;
   char		str[MAX_PATH_NAME_SIZE+10];



   TRACE (Func_Entry, "open_include_file", NULL);

   statement_number++;

   if (! cif_file_rec_issued ) {
      /* If CIF records have been requested, output the Source File record.  */
      /* Always output a File Name record for the source file.               */

      c_i_f = cif_actual_file;
      SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX) =
         cif_file_name_rec(SRC_STK_PATH_NAME(SRC_STK_BASE_IDX), src_file);

      if (cif_flags) {
         cif_source_file_rec(SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX),
                             cmd_line_flags.src_form);
      }

      c_i_f = cif_tmp_file;
      cif_file_rec_issued = TRUE;

      /* Set the line numbers in this entry correctly.                       */
      /* Always set GL_CIF_FILE_ID; it's needed for buffered message output. */

      GL_CIF_FILE_ID(global_line_tbl_idx)  =
                                SRC_STK_CIF_FILE_ID(SRC_STK_BASE_IDX);
   }

   if (nxt_line_type == Pound_Include_Enter_Line) {
      if (include_file[0] != SLASH) {
         getcwd (include_path, MAX_FILE_NAME_SIZE);
         strcat (include_path, "/");
         include_path_len = strlen(include_path);
         strcat (include_path, include_file);
      }
      else {
         include_path_len = 0;
         strcpy (include_path, include_file);
      }

      /* use previous file ptr */
      include_file_ptr = SRC_STK_FILE_PTR(src_stk_idx);
   }
   else if (include_file[0] == SLASH) 			/* absolute path name */
   {
      include_path_len = 0;				/* path prefix len    */
      include_file_len = strlen(include_file);
      strcpy (include_path, include_file);

      /* attempt to open include file for input */

      include_file_ptr = fopen (include_path, "r");
   }
   else {						/* find directory     */

      include_file_len	= strlen (include_file);	/* file name length   */
      include_idx	= include_path_idx;		/* glb idx to start   */
	
      if (angle_brkt_include) {
         if (include_idx != NULL_IDX) {
            strcpy (include_path, FP_NAME_PTR(include_idx));
            include_idx    = FP_NEXT_FILE_IDX(include_idx);
         }
         else {
            /* nothing to search */
            more = FALSE;
         }
      }
      else {
         strcpy(include_path, SRC_STK_PATH_NAME(src_stk_idx));

         /* Find out if there is a directory separator in the current name */

         char_ptr = strrchr(include_path, SLASH);

         if (char_ptr == NULL) {  /* No path precedes the name */
            char_ptr = include_path;
            *char_ptr++ = '.';
         }
         *char_ptr = EOS;

      }

      /* Append file_name to each directory name in include search list.      */

      while (more) {
	 include_path_len = strlen(include_path) + 1;

	 /* don't bother checking path names greater than max length */

	 if ((include_path_len + include_file_len) < MAX_PATH_NAME_SIZE) {
	    strcat (include_path, "/");
	    strcat (include_path, include_file);

	    /* attempt to open include file for input */
	    include_file_ptr = fopen (include_path, "r");
	 }

         if (include_file_ptr != NULL || include_idx == NULL_IDX) {
            break;
         }

         strcpy (include_path, FP_NAME_PTR(include_idx));
         include_idx	= FP_NEXT_FILE_IDX(include_idx);
      }
   }

   if (on_off_flags.output_pound_lines &&
       (on_off_flags.preprocess_only || on_off_flags.save_dot_i)) {

      if (cmd_line_flags.pp_macro_expansion) {
         nxt_line_start_idx[nxt_line_num_lines+1] = 
                         nxt_line_end_idx[nxt_line_num_lines]+1;
         nxt_line_num_lines++;
         pp_line_idx++;
         PP_LINE_TYPE = Comment_Line;
         PP_LINE_NUM = curr_glb_line;
         PP_EXPECTED_LINE = Regular_Line;

         sprintf(str, "# 1 \"%s\" 1\n", include_path);

         i = 0;
         while (str[i] != '\0') {
            nxt_line[nxt_line_start_idx[nxt_line_num_lines] + i] = str[i];
            i++;
         }

         nxt_line[nxt_line_start_idx[nxt_line_num_lines] + i] = '\0';
         nxt_line_end_idx[nxt_line_num_lines] = 
                               nxt_line_start_idx[nxt_line_num_lines] + i;
      }
      else {
         fprintf(dot_i_fptr, "# 1 \"%s\" 1\n", include_path);
      }
   }

   /* Make sure records associated with the INCLUDE line are buffered because */
   /* the INCLUDE line is always processed in "lookahead" mode which means    */
   /* the records belong to a following program unit in cases like this:      */
   /*                  ...						      */
   /*                  END 						      */
   /*                  SUBROUTINE sub					      */
   /*                  INCLUDE '...'					      */
   /* The INCLUDE line is processed when the EOS at the end of the END stmt   */
   /* is eaten.								      */
   /* If cif_need_unit_rec is TRUE, the current CIF is the temp CIF so don't  */
   /* change it.  c_i_f is switched back the actual file a ways down.  	      */

   if (! cif_need_unit_rec) {
      c_i_f = cif_tmp_file;
   }

   save_stmt_start_line = stmt_start_line;
   save_stmt_start_col  = stmt_start_col;
   stmt_start_line = nxt_line_num;
   stmt_start_col  = nxt_line_idx + 1;
   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_Include_Stmt, statement_number);
   }
   stmt_start_line = save_stmt_start_line;
   stmt_start_col  = save_stmt_start_col;



   /* Delete the following line when the above code is reinstated.            */

   cif_file_id = cif_file_name_rec(include_path, include_file);


   if (nxt_line_type != Pound_Include_Enter_Line &&
       ! pound_include_line) {
      for (i = nxt_line_idx + 8;           /* The char following INCLUDE.     */
           nxt_line[NXT_COL(i)] != QUOTE  &&  
           nxt_line[NXT_COL(i)] != DBL_QUOTE &&
           i < nxt_line_idx + line_size;
           ++i) {
      }
 
      include_stmt_file_col = ++i;
   }
   else {
      /* since this is from an "# lineno 'file.f' " line, */
      /* we'll just guess at column 10 here.              */

      include_stmt_file_col = 10;
   }

   cif_include_rec(nxt_line_num,
                   include_stmt_file_col,
                   cif_file_id);


   /* OK, switch the CIF back now.					      */

   if (! cif_need_unit_rec) {
      c_i_f = cif_actual_file;
   }


   if (include_file_ptr == NULL) {

      /* Can't open INCLUDE file.   */
      ntr_msg_queue(curr_glb_line, 63, Error, 
                    0,
                    include_file,
                    0,
                    STR_ARG);
   }
   else {               /* Check for recursive use of INCLUDE file name.      */

#ifdef KEY /* Bug 10151 */
    /*
     * Fortran standard 'include' forbids recursive use, and originally this
     * front end didn't allow it for any kind of inclusion (evidently -ftpp
     * can't limit the depth because it doesn't implement macros, and who
     * knows what Cc_Tok_Kwd_Include inclusion can or can't do.) But the cpp
     * preprocessor can tolerate recursive inclusion, can use macros to
     * limit it to a finite depth, and will issue a nice message if the
     * recursion threatens to be infinite, so we don't want to forbid
     * recursion solely on the basis of a "# filename lineno" directive.
     */
    if (!allow_recursion) {
#endif /* KEY Bug 10151 */
      for (src_stk_i = src_stk_idx; src_stk_i > NULL_IDX; src_stk_i--) {

	 if (EQUAL_STRS(include_path, SRC_STK_PATH_NAME(src_stk_i))) {
	    recursive_use = TRUE;
	    break;
	 }
      }
#ifdef KEY /* Bug 10151 */
    }
#endif /* KEY Bug 10151 */

      if (recursive_use) { /* Recursive use of include file */
	 ntr_msg_queue(curr_glb_line, 64, Error,
                       0,
                       include_file,
                       0,
                       STR_ARG);
      }
      else { /* update source stack with open file info */
         TBL_REALLOC_CK (src_stk, 1);
         SRC_STK_FILE_LINE(src_stk_idx)		= 0;

         /* This field will get set by update_global_line */

         SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)	= NULL_IDX;
         SRC_STK_FILE_TYPE(src_stk_idx)		= Include_Src;
         SRC_STK_PREV_SRC_FORM(src_stk_idx)	= source_form;
         SRC_STK_FILE_PTR(src_stk_idx)		= include_file_ptr;
         SRC_STK_FILE_IDX(src_stk_idx)		= include_path_len;
         strcpy (SRC_STK_PATH_NAME(src_stk_idx), include_path);

         if (nxt_line_type == Pound_Include_Enter_Line) {
            SRC_STK_DO_NOT_FCLOSE(src_stk_idx) = TRUE;
         }
         else {
            SRC_STK_DO_NOT_FCLOSE(src_stk_idx) = FALSE;
         }

         /* Always set the CIF File Id.  It's needed for the buffered         */
         /* message file.						      */

         SRC_STK_CIF_FILE_ID(src_stk_idx) = cif_file_id;

	 result = TRUE;
      }
   }

   TRACE (Func_Exit, "open_include_file", include_path);

   return (result);

}  /* open_include_file */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Update_global_line adds a new entry to the global_line_tbl each time  *|
|*	an include file is opened, or when an EOF is encountered reading an   *|
|*	include file.							      *|
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

static void update_global_line (void)

{
   int		idx;
   int		length;
   int		lengthp;


   TRACE (Func_Entry, "update_global_line", NULL);

   TBL_REALLOC_CK (global_line_tbl, 1);
   GL_GLOBAL_LINE(global_line_tbl_idx)       = curr_glb_line;
   GL_FILE_LINE(global_line_tbl_idx)         = SRC_STK_FILE_LINE(src_stk_idx);
   GL_CIF_FILE_ID(global_line_tbl_idx)       = SRC_STK_CIF_FILE_ID(src_stk_idx);

   /* Clear the field.  It gets set at EOF of each file. */
   /* It also holds a running total of file lines at each end statement */
   /* because of mif inflexibility.                                     */

   GL_SOURCE_LINES(global_line_tbl_idx)      = 0;
   GL_INCLUDE_FILE_LINE(global_line_tbl_idx) = include_stmt_file_line;
   GL_INCLUDE_FILE_COL(global_line_tbl_idx)  = include_stmt_file_col;
   include_stmt_file_line		     = 0;
   include_stmt_file_col 		     = 0;

   /* Need to keep track of the first entry in  the global  */
   /* line table, that describes the current source file.   */

   if (SRC_STK_GLOBAL_LINE_IDX(src_stk_idx) == NULL_IDX) {
      SRC_STK_GLOBAL_LINE_IDX(src_stk_idx) = global_line_tbl_idx;
   }

   /* prevent duplication of file name strings in string pool */

   GL_FILE_NAME_IDX(global_line_tbl_idx) = NULL_IDX;
   GL_PATH_NAME_IDX(global_line_tbl_idx) = NULL_IDX;
   length = strlen(SRC_STK_FILE_NAME(src_stk_idx));
   lengthp = strlen(SRC_STK_PATH_NAME(src_stk_idx));

   for (idx = global_line_tbl_idx - 1; idx > NULL_IDX; idx--) {

      if (GL_FILE_NAME_LEN(idx) == length &&
          EQUAL_STRS(SRC_STK_FILE_NAME(src_stk_idx), GL_FILE_NAME_PTR(idx))) {
	 GL_FILE_NAME_IDX(global_line_tbl_idx) = GL_FILE_NAME_IDX(idx);
	 GL_FILE_NAME_LEN(global_line_tbl_idx) = length;
	 break;
      }
   }

   for (idx = global_line_tbl_idx - 1; idx > NULL_IDX; idx--) {

      if (GL_PATH_NAME_LEN(idx) == lengthp &&
          EQUAL_STRS(SRC_STK_PATH_NAME(src_stk_idx), GL_PATH_NAME_PTR(idx))) {
	 GL_PATH_NAME_IDX(global_line_tbl_idx) = GL_PATH_NAME_IDX(idx);
	 GL_PATH_NAME_LEN(global_line_tbl_idx) = lengthp;
	 break;
      }
   }

   /* check for file name already in the string pool */

   if (GL_FILE_NAME_IDX(global_line_tbl_idx) == NULL_IDX) {
      GL_FILE_NAME_LEN(global_line_tbl_idx)	= length;
      GL_FILE_NAME_IDX(global_line_tbl_idx)	= str_pool_idx+1;
      TBL_REALLOC_CK(str_pool, WORD_LEN(length));

      for (idx = GL_FILE_NAME_IDX(global_line_tbl_idx); 
           idx <= str_pool_idx; idx++) {
          str_pool[idx].name_long = 0;
      }

      strcpy(GL_FILE_NAME_PTR(global_line_tbl_idx), 
             SRC_STK_FILE_NAME(src_stk_idx));
   }

   if (GL_PATH_NAME_IDX(global_line_tbl_idx) == NULL_IDX) {
      GL_PATH_NAME_LEN(global_line_tbl_idx)	= lengthp;
      GL_PATH_NAME_IDX(global_line_tbl_idx)	= str_pool_idx+1;
      TBL_REALLOC_CK(str_pool, WORD_LEN(lengthp));

      for (idx = GL_PATH_NAME_IDX(global_line_tbl_idx); 
           idx <= str_pool_idx; idx++) {
          str_pool[idx].name_long = 0;
      }

      strcpy(GL_PATH_NAME_PTR(global_line_tbl_idx), 
             SRC_STK_PATH_NAME(src_stk_idx));
   }

   TRACE (Func_Exit, "update_global_line", NULL);

   return;
		  
}  /* update_global_line */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints the source line to stderr.			              *|
|*									      *|
|* Input parameters:							      *|
|*	line - source line to be printed.				      *|
|*	column - column position for pointer.			              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void print_err_line(int line, int column)
{
   char		buf[MAX_SRC_LINE_SIZE];
   char		buf2[MAX_SRC_LINE_SIZE];
   int		col_idx = 0;
   int		i;
   int          line_idx;


   if (line != stmt_line_num[stmt_line_idx]) {

      for (line_idx = 1; line_idx <= lines_in_buf; line_idx++) {

          if (line == stmt_line_num[line_idx]) {
             break;
          }
      }
   } 
   else {
      line_idx = stmt_line_idx;
   }

   if (line == stmt_line_num[line_idx]) {

      for (i = 0;  
           i <= stmt_line_end_idx[line_idx]-stmt_line_start_idx[line_idx];
           i++) {

          if (i >= MAX_SRC_LINE_SIZE) {
             break;
          }

          buf2[i] = stmt_buf[stmt_line_start_idx[line_idx] + i];

          if (stmt_buf_col[stmt_line_start_idx[line_idx] + i] == column) {
             col_idx = i;
          }

          if (buf2[i] == '\0') {
             buf[i] = '\0';
             break;
          }
          else if (buf2[i] == (char)EOF) {
             buf[i] = '\0';
             buf2[i] = '\0';
             break;
          }
          else if (buf2[i] == '\n') {
             buf[i] = ' ';
             break;
          }
          else if (buf2[i] == '\t') {
             buf[i] = '\t';
          }
          else {
             buf[i] = ' ';
          }
      }

      buf[++i] = '\0';
      buf2[i] = '\0';
      fprintf(stderr, "%s", buf2);

      if (column != 0) {
         buf[col_idx] = '^';
         fprintf(stderr, "%s\n", buf);
      }
   }
   else if (line == nxt_line_num) {

      for (i = 0;  i <= nxt_line_EOL;  i++) {
          if (i >= MAX_SRC_LINE_SIZE) {
             break;
          }

          buf2[i] = nxt_line[NXT_COL(i)];

          if (nxt_line_col[NXT_COL(i)] == column) {
             col_idx = i;
          }

          if (buf2[i] == '\0') {
             buf[i] = '\0';
             break;
          }
          else if (buf2[i] == (char)EOF) {
             buf[i] = '\0';
             buf2[i] = '\0';
             break;
          }
          else if (buf2[i] == '\n') {
             buf[i] = ' ';
             break;
          }
          else if (buf2[i] == '\t') {
             buf[i] = '\t';
          }
          else {
             buf[i] = ' ';
          }
      }
      
      buf[++i] = '\0';
      buf2[i] = '\0';
      fprintf(stderr, "%s", buf2);

      if (column != 0) {
         buf[col_idx] = '^';
         fprintf(stderr, "%s\n", buf);
      }
   }

   return;

}  /* print_err_line */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      looks ahead in stmt_buf to find what follows a paren group.           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      cnt - number of paren groups to look for (1 or 2).                    *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      char following parens.                                                *|
|*                                                                            *|
\******************************************************************************/

char	scan_thru_close_paren(int idx, int line_idx, int cnt)

{
   char         ch;
   int          ich;
   int          paren_level     = 1;

   TRACE (Func_Entry, "scan_thru_close_paren", &LA_CH_VALUE);

   if (line_idx == 0) {
      idx = stmt_buf_idx;
      line_idx = stmt_line_idx;
   }
   ich = stmt_buf[idx];
   while ((ich == lparen) && (cnt > 0)) {
      while (idx < stmt_buf_EOS_idx) {
         ich = stmt_buf[++idx];
         if (ich == rparen) {
            paren_level--;
         
            if (paren_level == 0) {
               do {
                  ich = stmt_buf[++idx];

                  if (idx == stmt_line_end_idx[line_idx]) {

                     if (line_idx < lines_in_buf) {
                        idx = stmt_line_start_idx[++line_idx] +
                              stmt_line_offset[line_idx];
                        ich = blank;
                     }
                     else {
                        ich = eos;
                     }
                  }
               }
               while ((ich == blank) | (ich == tab));
               paren_level = 1;
               break;
            }
         }
         else if (ich == lparen) {
            paren_level++;
         }
         else if (idx == stmt_line_end_idx[line_idx]) {

            if (line_idx < lines_in_buf) {
               idx = stmt_line_start_idx[++line_idx] + 
                        stmt_line_offset[line_idx];
            }
            else {
               ich = eos;
               break;
            }
         }
      } /* while */

      cnt--;

   } /* while lparen */
 
   ch = ich;

   if (islower(ch)) {                                   /* lowercase char     */
      ch = TOUPPER(ch);                                 /* cnvrt lwr to upr   */
   }

   TRACE (Func_Exit, "scan_thru_close_paren", NULL);

   return (ch);

}  /* scan_thru_close_paren */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Checks if character following id and possible paren groups implies    *|
|*      that the stmt is an assignment statement.                             *|
|*                                                                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if not assignment.                                               *|
|*                                                                            *|
\******************************************************************************/

boolean set_stmt_type_known(void)

{

   int		ich;
   int		idx;
   int		lblank		= ' ';
   int		line_idx;
   boolean	lsig_blank;
   int		ltab		= '\t';
   boolean	stmt_type_known	= FALSE;


   TRACE (Func_Entry, "set_stmt_type_known", NULL);

   idx		= stmt_buf_idx;
   line_idx	= stmt_line_idx;
   ich		= stmt_buf[idx];
   lsig_blank   = sig_blank;

   if (source_form != Fixed_Form) {
      lblank	= -1;
      ltab	= -1;
   }

BACK:

# pragma _CRI align
   while ((ich != EOF) &&
          ((ch_class[ich] == Ch_Class_Letter) |
           (ch_class[ich] == Ch_Class_Digit)  |
           (ich == underscore)                |
           (ich == dollar)                    |
           (ich == at_sign)                   |
           (ich == lblank)                    |
           (ich == ltab)                      |
           (ich == newline)                   |
           (ich == bang))                     &&
           (! lsig_blank))                     {

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            goto DONE;
         }
      }
      ich = stmt_buf[++idx];
   } /* while */

   while ((ich == blank)   |
          (ich == tab)     |
          (ich == newline) |
          (ich == bang))   {

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            goto DONE;
         }
      }
      ich = stmt_buf[++idx];
   }

   if (ich == lparen) {
      ich = whats_after_paren_group(&idx, &line_idx, 2);
   }

# ifdef _F_MINUS_MINUS
   if (ich == lbrkt && cmd_line_flags.co_array_fortran) {
      ich = whats_after_brkt_group(&idx, &line_idx, 2);
   }
# endif

   if (ich == percent) {

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            goto DONE;
         }
      }
      ich = stmt_buf[++idx];

      while ((ich == blank)   |
             (ich == tab)     |
             (ich == newline) |
             (ich == bang))   {

         if (idx == stmt_line_end_idx[line_idx]) {

            if (line_idx < lines_in_buf) {
               idx = stmt_line_start_idx[++line_idx]
                     + stmt_line_offset[line_idx];
            }
            else {
               goto DONE;
            }
         }
         ich = stmt_buf[++idx];
      }

      lsig_blank = FALSE;
      goto BACK;
   }

DONE:

   if (ich != equal) {
      stmt_type_known = TRUE;
   }

   TRACE (Func_Exit, "set_stmt_type_known", NULL);

   return(stmt_type_known);

}  /* set_stmt_type_known */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      looks ahead in stmt_buf to find what follows a paren group.           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      cnt - number of paren groups to look for (1 or 2).                    *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      char following parens.                                                *|
|*                                                                            *|
\******************************************************************************/

static int    whats_after_paren_group(int *idx, int *line_idx, int cnt)

{
   int          ich;
   int          paren_level     = 1;

   TRACE (Func_Entry, "whats_after_paren_group", &LA_CH_VALUE);

   ich = stmt_buf[*idx];

   while ((ich == lparen) && (cnt > 0)) {
      while (*idx < stmt_buf_EOS_idx) {
         ich = stmt_buf[++(*idx)];
         if (ich == rparen) {
            paren_level--;

            if (paren_level == 0) {
               do {
                  ich = stmt_buf[++(*idx)];

                  if (*idx == stmt_line_end_idx[*line_idx]) {

                     if (*line_idx < lines_in_buf) {
                        *idx = stmt_line_start_idx[++(*line_idx)] +
                              stmt_line_offset[*line_idx];
                        ich = blank;
                     }
                     else {
                        ich = eos;
                     }
                  }
               }
               while ((ich == blank) | (ich == tab));
               paren_level = 1;
               break;
            }
         }
         else if (ich == lparen) {
            paren_level++;
         }
         else if (*idx == stmt_line_end_idx[*line_idx]) {

            if (*line_idx < lines_in_buf) {
               *idx = stmt_line_start_idx[++(*line_idx)] + 
                              stmt_line_offset[*line_idx];
            }
            else {
               ich = eos;
               break;
            }
         }
      } /* while */

      cnt--;

   } /* while lparen */

   TRACE (Func_Exit, "whats_after_paren_group", NULL);

   return (ich);

}  /* whats_after_paren_group */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      looks ahead in stmt_buf to find what follows a brkt group.            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      cnt - number of brkt groups to look for (1 or 2).                     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      char following parens.                                                *|
|*                                                                            *|
\******************************************************************************/

static int    whats_after_brkt_group(int *idx, int *line_idx, int cnt)

{
   int          ich;
   int          brkt_level     = 1;

   TRACE (Func_Entry, "whats_after_brkt_group", &LA_CH_VALUE);

   ich = stmt_buf[*idx];

   while ((ich == lbrkt) && (cnt > 0)) {
      while (*idx < stmt_buf_EOS_idx) {
         ich = stmt_buf[++(*idx)];
         if (ich == rbrkt) {
            brkt_level--;

            if (brkt_level == 0) {
               do {
                  ich = stmt_buf[++(*idx)];

                  if (*idx == stmt_line_end_idx[*line_idx]) {

                     if (*line_idx < lines_in_buf) {
                        *idx = stmt_line_start_idx[++(*line_idx)] +
                              stmt_line_offset[*line_idx];
                        ich = blank;
                     }
                     else {
                        ich = eos;
                     }
                  }
               }
               while ((ich == blank) | (ich == tab));
               brkt_level = 1;
               break;
            }
         }
         else if (ich == lbrkt) {
            brkt_level++;
         }
         else if (*idx == stmt_line_end_idx[*line_idx]) {
            if (*line_idx < lines_in_buf) {
               *idx = stmt_line_start_idx[++(*line_idx)] +
                              stmt_line_offset[*line_idx];
            }
            else {
               ich = eos;
               break;
            }
         }
      } /* while */

      cnt--;

   } /* while lbrkt */

   TRACE (Func_Exit, "whats_after_brkt_group", NULL);

   return (ich);

}  /* whats_after_brkt_group */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Checks if character after id is =.                                    *|
|**     This means keyword.                                                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if keyword                                                       *|
|*                                                                            *|
\******************************************************************************/

boolean next_arg_is_kwd_equal (void)

{

   int  ich;
   int  idx;
   int  lamp   = '&';
   int  lblank = ' ';
   int  line_idx;
   int  ltab = '\t';
   int  kwd  = FALSE;

   TRACE (Func_Entry, "next_arg_is_kwd_equal", NULL);

   idx = stmt_buf_idx;

   line_idx = stmt_line_idx;

   ich = stmt_buf[idx];

   if (source_form != Fixed_Form) {
      lblank = -1;
      ltab = -1;
   }
   else {
      lamp = -1;
   }

# pragma _CRI align
   while ((ich != EOF) &&
          (ch_class[(char)ich] == Ch_Class_Letter) |
          (ch_class[(char)ich] == Ch_Class_Digit)  |
          (ich == underscore)                |
          (ich == dollar)                    |
          (ich == at_sign)                   |
          (ich == lblank)                    |
          (ich == ltab)                      |
          (ich == lamp)                      |
          (ich == newline)                   |
          (ich == bang))                     {

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            break;
         }
      }
      else if (ich == lamp) {
         break;
      }
      ich = stmt_buf[++idx];
   } /* while */

   while ((ich == blank)   |
          (ich == tab)     |
          (ich == lamp)    |
          (ich == newline) |
          (ich == bang))   {

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            break;
         }
      }
      else if (ich == lamp) {
         break;
      }
         
      ich = stmt_buf[++idx];
   }

   kwd = (ich == equal);

   if (!kwd) {
      goto EXIT;
   }

   ich = stmt_buf[++idx];

   while ((ich == lblank)  |
          (ich == ltab)    |
          (ich == lamp)    |
          (ich == newline) |
          (ich == bang))   {

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            break;
         }
      }
      else if (ich == lamp) {
         break;
      }

      ich = stmt_buf[++idx];
   }

   kwd = (ich != equal);

EXIT:

   TRACE (Func_Exit, "next_arg_is_kwd_equal", NULL);

   return(kwd);

}  /* next_arg_is_kwd_equal */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Checks if possible data stmt is an assignment.                        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if stmt is DATA stmt.                                            *|
|*                                                                            *|
\******************************************************************************/

boolean stmt_is_DATA_stmt (void)

{
   int     ich;
   int     idx;
   int     line_idx;
   int     paren_level = 0;
   boolean result = TRUE;
 
   TRACE (Func_Entry, "stmt_is_DATA_stmt", NULL);

   ich = stmt_buf[stmt_buf_idx];


   /* In free form, a blank cannot be inside an identifier. */

   if (sig_blank      &&
       ich != equal   &&
       ich != percent &&
       ich != lparen) {
      goto EXIT;
   }

   idx = stmt_buf_idx - 1;

   line_idx = stmt_line_idx;

   do {
      ich = stmt_buf[++idx];

      if (ich == lparen) {
         paren_level++;
         continue;
      }
      else if (ich == rparen) {
         paren_level--;
         continue;
      }
 
      if (ich == equal && paren_level == 0) {
         result = FALSE;
         break;
      }

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            break;
         }
      }
   }
   while (ich != slash | paren_level);

EXIT:

   TRACE (Func_Exit, "stmt_is_DATA_stmt", NULL);

   return(result);

} /* stmt_is_DATA_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Checks if stmt has double colon at zero paren level.                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if double paren found.                                           *|
|*                                                                            *|
\******************************************************************************/

boolean stmt_has_double_colon (void)

{
   boolean colon_found = FALSE;
   boolean found = FALSE;
   int     ich;
   int     idx;
   int     lblank = ' ';
   int     ltab   = '\t';
   int     line_idx;
   int     paren_level = 0;

   TRACE (Func_Entry, "stmt_has_double_colon", NULL);

   idx = stmt_buf_idx - 1;

   line_idx = stmt_line_idx;

   if (source_form != Fixed_Form) {
      lblank = -1;
      ltab   = -1;
   }
  

   do {
      ich = stmt_buf[++idx];

      if (ich == lparen) {
         paren_level++;
         colon_found = FALSE;
         continue;
      }
      else if (ich == rparen) {
         paren_level--;
         colon_found = FALSE;
         continue;
      }

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
            continue;
         }
         else {
            break;
         }
      }

      if (ich == colon && paren_level == 0) {
      
         if (colon_found) {
            found = TRUE;
            break;
         }
         else {
            colon_found = TRUE;
            continue;
         }
      }

      if (ich == lblank | ich == ltab) {
         continue;
      }

      colon_found = FALSE;
   }
   while (TRUE);

   TRACE (Func_Exit, "stmt_has_double_colon", NULL);

   return(found);

} /* stmt_has_double_colon */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Checks if stmt has comma after equal. Called for Do token.            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if stmt is DO stmt.                                              *|
|*                                                                            *|
\******************************************************************************/

boolean  stmt_is_DO_stmt (void) 

{
   boolean found_eq = FALSE;
   int     ich;
   int     idx;
   int     line_idx;
   int     paren_level = 0;
   boolean result = FALSE;

   TRACE (Func_Entry, "stmt_is_DO_stmt", NULL);

   idx = stmt_buf_idx - 1;

   line_idx = stmt_line_idx;

   do {
      ich = stmt_buf[++idx];

      if (ich == lparen) {
         paren_level++;
         continue;
      }
      else if (ich == rparen) {
         paren_level--;
         continue;
      }

      if (ich == equal && paren_level == 0) {
         found_eq = TRUE;
         continue;
      }

      if (ich == comma && paren_level == 0 && found_eq) {
         result = TRUE;
         break;
      }

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
         }
         else {
            break;
         }
      }
   }
   while (TRUE);

   TRACE (Func_Exit, "stmt_is_DO_stmt", NULL);

   return (result);

} /* stmt_is_DO_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Translates an index in the format string to line and column number.   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      format_col - index in the format string.                              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      line       - line number in source.                                   *|
|*      col        - column number in source.                                 *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void  format_line_n_col (int *line,
                         int *col,
                         int  format_col)

{
   int   dbl_delim_mask         = (1 << 9);
   int	 i;
   int	 ich;
   int   idx;
   int   line_idx = NULL_IDX;

   TRACE (Func_Entry, "format_line_n_col", NULL);

   idx = format_start_idx - 1;

   for (i = 1; i <= lines_in_buf; i++) {
      if (idx + 1 >= stmt_line_start_idx[i] && 
          idx + 1 <= stmt_line_end_idx[i]) {

         line_idx = i;
         break;
      }
   }

# ifdef _DEBUG
   if (line_idx == NULL_IDX) {
      PRINTMSG(1, 626, Internal, 1,
               "valid line_idx",
               "format_line_n_col");
   }
# endif

   if (stmt_type == Format_Stmt) {

      while (format_col > 0) {
         if (idx + format_col < stmt_line_end_idx[line_idx]) {
            idx += format_col;
            format_col = 0;
         }
         else {
            format_col -= stmt_line_end_idx[line_idx] - idx;
            if (line_idx < lines_in_buf) {
               idx = stmt_line_start_idx[++line_idx]
                                 + stmt_line_offset[line_idx] + 1;
            }
            else {
               idx = stmt_line_end_idx[line_idx] - 1;
               break;
            }
         }
      }
   }
   else {

      while (format_col > 0) {

         ich = stmt_buf[++idx];

         if (idx == stmt_line_end_idx[line_idx]) {

            if (line_idx < lines_in_buf) {
               idx = stmt_line_start_idx[++line_idx]
                     + stmt_line_offset[line_idx];
               ich = stmt_buf[++idx];
            }
            else {
               break;
            }
         }

         if ((ich & dbl_delim_mask) == 0) {
            format_col--;
         }
      }
   }

   *line = stmt_line_num[line_idx];

   *col = stmt_buf_col[idx];

   TRACE (Func_Exit, "format_line_n_col", NULL);

   return;

} /* format_line_n_col */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine sets the global variable format_start_idx to the         *|
|*      stmt buffer idx that matches the input line and column value.         *|
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

void set_format_start_idx(int	buf_idx)

{

   TRACE (Func_Entry, "set_format_start_idx", NULL);

   /* get past the quote */
   buf_idx++;

   format_start_idx = buf_idx;

   TRACE (Func_Exit, "set_format_start_idx", NULL);

   return;

}  /* set_format_start_idx */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Place the next character constant into the constant table.            *|
|*      Resets LA_CH                                                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      index into constant table.                                            *|
|*                                                                            *|
\******************************************************************************/

int  put_char_const_in_tbl (char holler,	/* In  */
                            int  *len)		/* Out */

{
   int		 	char_idx		= NULL_IDX;
   char		       *char_ptr;
   int		 	dbl_delim_mask		= (1 << 9);
   int		 	ich;
   int		 	idx;
   long64	 	length			= 0;
   linear_type_type	linear_type;
   int		 	line_idx;
   int		 	type_idx;


   TRACE (Func_Entry, "put_char_const_in_tbl", NULL);

   ich		= stmt_buf[stmt_buf_idx];
   idx		= stmt_buf_idx;
   line_idx	= stmt_line_idx;

   while (ich < 0) {

      if ((ich & dbl_delim_mask) == 0) {
         length++;
      }
      ich = stmt_buf[++idx];

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
            ich = stmt_buf[++idx];
         }
         else {
            break;
         }
      }
   } /* while */

   *len = length;

   if (holler != '\0') {

      if (WORD_ALIGNED_BIT_LENGTH(CHAR_BIT * length) <= 
                                               MAX_SHORT_TYPELESS_BITS) {
         linear_type = Short_Typeless_Const;
      }
      else {
         linear_type = Long_Typeless;
      }

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Typeless;
      TYP_LINEAR(TYP_WORK_IDX)	= linear_type;
      TYP_BIT_LEN(TYP_WORK_IDX)	= WORD_ALIGNED_BIT_LENGTH(CHAR_BIT * length);
      type_idx			= ntr_type_tbl();
   }
   else {
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Character;
      TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
      TYP_DESC(TYP_WORK_IDX)	= Default_Typed;
      TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, length),
      type_idx			= ntr_type_tbl();
   }

   char_idx	= ntr_const_tbl(type_idx, TRUE, NULL);
   char_ptr	= (char *) &CN_CONST(char_idx);
   idx		= 0;

   if (holler == 'R') {

      while (idx < (TARGET_CHARS_PER_WORD - (length % TARGET_CHARS_PER_WORD)) %
                                             TARGET_CHARS_PER_WORD){
         idx++;
      }
   }

   idx--;
   ich = stmt_buf[stmt_buf_idx];

   while (ich < 0) {

      if ((ich & dbl_delim_mask) == 0) {
         char_ptr[++idx] = ich;
      }
      ich = stmt_buf[++stmt_buf_idx];
      
      if (stmt_buf_idx == stmt_line_end_idx[stmt_line_idx]) {

         if (stmt_line_idx < lines_in_buf) {
            stmt_buf_idx = stmt_line_start_idx[++stmt_line_idx]
                  + stmt_line_offset[stmt_line_idx];
            ich = stmt_buf[++stmt_buf_idx];
         }
         else {
            break;
         }
      }
   }

   if (holler == 'H' || holler == '\0') {

      while ((++idx) % TARGET_CHARS_PER_WORD != 0) {
         char_ptr[idx] = ' ';
      }
   }

   stmt_buf_idx--;

   NEXT_LA_CH;

   TRACE (Func_Exit, "put_char_const_in_tbl", NULL);

   return(char_idx);

} /* put_char_const_in_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Places a format string into the constant table.                       *|
|*      Resets LA_CH                                                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      index into constant table.                                            *|
|*                                                                            *|
\******************************************************************************/

int  put_format_in_tbl (void)

{
   int		char_idx	= NULL_IDX;
   char	       *char_ptr;
   char	       *char_ptr2;
   int		ich;
   int		idx;
   long64	length		= 0;
   int		line_idx;
   long64	new_length;
   int		paren_lvl	= 0;
   int		type_idx;


   TRACE (Func_Entry, "put_format_in_tbl", NULL);

   idx = stmt_buf_idx - 1;
   line_idx = stmt_line_idx;

   ich = stmt_buf[stmt_buf_idx];

   char_idx = (MASK_CHAR_CONST_IDX & ich) >> 8;

   if (char_idx != NULL_IDX) {
      length = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(char_idx)));

      while (length > 0) {

         if ((stmt_buf_idx + length) < stmt_line_end_idx[stmt_line_idx]) {
            stmt_buf_idx += length;
            length = 0;
         }
         else {
            length -= stmt_line_end_idx[stmt_line_idx] - stmt_buf_idx;
            stmt_buf_idx = stmt_line_start_idx[++stmt_line_idx]
                           + stmt_line_offset[stmt_line_idx] + 1;
         }
      }
      goto EXIT;
   }


   /* figure the length */
   do {
      ++idx;

      if (idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx];
            continue;
         }
         else {
            break;
         }
      }
      ich = stmt_buf[idx];
      length++;

      if (ich == lparen) {
         paren_lvl++;
      }
      else if (ich == rparen) {
         paren_lvl--;
      }
   }
   while (paren_lvl);

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   new_length	  		  = length + AT_NAME_LEN(stmt_label_idx);
   TYP_TYPE(TYP_WORK_IDX)         = Character;
   TYP_LINEAR(TYP_WORK_IDX)       = CHARACTER_DEFAULT_TYPE;
   TYP_DESC(TYP_WORK_IDX)         = Default_Typed;
   TYP_CHAR_CLASS(TYP_WORK_IDX)   = Const_Len_Char;
   TYP_FLD(TYP_WORK_IDX)          = CN_Tbl_Idx;
   TYP_IDX(TYP_WORK_IDX)          = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                                                new_length),
   type_idx			= ntr_type_tbl();
   char_idx			= ntr_const_tbl(type_idx, TRUE, NULL);

   stmt_buf[stmt_buf_idx] |= (char_idx << 8);

   char_ptr = (char *) &CN_CONST(char_idx);
   char_ptr2 = (char *)AT_OBJ_NAME_PTR(stmt_label_idx);

   for (idx = 0; idx < AT_NAME_LEN(stmt_label_idx); idx++) {
      char_ptr[idx] = char_ptr2[idx];
   }

   stmt_buf_idx--;

   while (length > 0) {
      
      stmt_buf_idx++;

      if (stmt_buf_idx == stmt_line_end_idx[stmt_line_idx]) {

         if (stmt_line_idx < lines_in_buf) {
            stmt_buf_idx = stmt_line_start_idx[++stmt_line_idx]
                  + stmt_line_offset[stmt_line_idx];
            continue;
         }
         else {
            break;
         }
      }

      char_ptr[idx] = stmt_buf[stmt_buf_idx];
      idx++;
      length--;
   } /* while */

   stmt_buf_idx++;

EXIT:

   stmt_buf_idx--;
 
   NEXT_LA_CH;


   TRACE (Func_Exit, "put_format_in_tbl", NULL);

   return(char_idx);

} /* put_format_in_tbl */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Places a format string into the constant table.                       *|
|*      Resets LA_CH                                                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      index into constant table.                                            *|
|*                                                                            *|
\******************************************************************************/

boolean  is_implied_do (void)

{
   boolean had_equal = FALSE;
   int     ich;
   int     idx;
   boolean imp_do = FALSE;
   int     lblank = ' ';
   int     ltab   = '\t';
   int     line_idx;
   int     paren_lvl = 0;
   int     prev_ich;

   TRACE (Func_Entry, "is_implied_do", NULL);

   idx = stmt_buf_idx; 
   line_idx = stmt_line_idx;
   ich = stmt_buf[idx];
   prev_ich = ich;

   if (source_form != Fixed_Form) {
      lblank = -1;
      ltab   = -1;
   }

   do {

      if (had_equal) {

         if (ich == equal) {
            had_equal = FALSE;
         }
         else if (ich != lblank && ich != ltab) {
            imp_do = TRUE;
            break;
         }
      }
      else if (ich == lparen) {
         paren_lvl++;
      }
      else if (ich == rparen) {
         paren_lvl--;
      }
      else if (ich == equal && paren_lvl == 1 &&
               prev_ich != slash &&
               prev_ich != greater &&
               prev_ich != less) {
         had_equal = TRUE;
      }

      if (++idx == stmt_line_end_idx[line_idx]) {
         
         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx] + 1;
         }
         else {
            break;
         }
      }

      if (ich != lblank && ich != ltab) {
         prev_ich = ich;
      }

      ich = stmt_buf[idx];
   } 
   while (paren_lvl > 0);

   TRACE (Func_Exit, "is_implied_do", NULL);

   return(imp_do);

} /* is_implied_do */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Looks for a colon in a paren group, to tell the difference between    *|
|*      a character function and a substring reference.                       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if colon found.                                                  *|
|*                                                                            *|
\******************************************************************************/

boolean  is_substring_ref (void)

{
   int     ich;
   int     idx;
   int     line_idx;
   int     paren_lvl = 0;
   boolean substring = FALSE;

   TRACE (Func_Entry, "is_substring_ref", NULL);

   idx = stmt_buf_idx;
   line_idx = stmt_line_idx;

   ich = stmt_buf[idx];
   do {
      if (ich == lparen) {
         paren_lvl++;
      }
      else if (ich == rparen) {
         paren_lvl--;
      }
      else if (ich == colon && paren_lvl == 1) {

         if (substring) {
            substring = FALSE;
            break;
         }
         else {
            substring = TRUE;
         }
      }
      else if (ich == comma && paren_lvl == 1) {
         substring = FALSE;
         break;
      }

      if (++idx == stmt_line_end_idx[line_idx]) {
         
         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx] + 1;
         }
         else {
            break;
         }
      }
      ich = stmt_buf[idx];
   }
   while (paren_lvl);

   TRACE (Func_Exit, "is_substring_ref", NULL);

   return(substring);
} /* is_substring_ref */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Looks for an equal sign following the next identifier. Looks past     *|
|*      the entire reference.                                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if this identifier is the implied do control variable.           *|
|*                                                                            *|
\******************************************************************************/

boolean next_id_is_imp_control(void)
{
   boolean cont_var = FALSE;
   int     ich;
   int     idx;
   int     line_idx;
   int     paren_lvl = 0;
   
   TRACE (Func_Entry, "next_id_is_imp_control", NULL);

   idx      = stmt_buf_idx;
   line_idx = stmt_line_idx;
   ich      = stmt_buf[idx];

   do {
      if (ich == lparen) {
         paren_lvl++;
      }
      else if (ich == rparen) {
         paren_lvl--;
      }
      else if (paren_lvl != 0) {
      }
      else if (ich == equal) {
         cont_var = TRUE;
         break;
      }
      else if (ich == EOF) {
         break;
      }
      else if (ich == blank | 
               ich == tab | 
               ich == percent |
               ich == underscore | 
               ich == dollar | 
               ich == at_sign |
               ch_class[ich] == Ch_Class_Digit | 
               ch_class[ich] == Ch_Class_Letter) {
      }
      else {
         break;
      }

      if (++idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx] + 1;
         }
         else {
            break;
         }
      }
      ich = stmt_buf[idx];
   }
   while (paren_lvl >= 0);

   TRACE (Func_Exit, "next_id_is_imp_control", NULL);

   return(cont_var);

} /* next_id_is_imp_control */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Issue all deferred msgs.				       	      *|
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

void  issue_deferred_msgs (void)

{
   char *arg1;
   char *arg2;
   char *arg3;
   char *arg4;
   int	 i;
   char *ptr;


   TRACE (Func_Entry, "issue_deferred_msgs", NULL);

   i = 1;

   while (i <= msg_queue_idx) {

      switch (msg_queue[i].order) {

         case NO_ARG      :
            PRINTMSG(msg_queue[i].line_num, msg_queue[i].msg_num,
                     msg_queue[i].sever, msg_queue[i].col_num);
            break;

         case STR_ARG     :
            PRINTMSG(msg_queue[i].line_num, msg_queue[i].msg_num,
                     msg_queue[i].sever, msg_queue[i].col_num, 
                     (char *) (&msg_queue[i+1]));
            break;

         case ARG_ARG     :
            PRINTMSG(msg_queue[i].line_num, msg_queue[i].msg_num,
                     msg_queue[i].sever, msg_queue[i].col_num,
                     msg_queue[i].arg);
            break;

         case STR_ARG_ARG :
            PRINTMSG(msg_queue[i].line_num, msg_queue[i].msg_num,
                     msg_queue[i].sever, msg_queue[i].col_num,
                     (char *) (&msg_queue[i+1]),
                     msg_queue[i].arg);
            break;

         case ARG_STR_ARG :
            PRINTMSG(msg_queue[i].line_num, msg_queue[i].msg_num,
                     msg_queue[i].sever, msg_queue[i].col_num,
                     msg_queue[i].arg,
                     (char *) (&msg_queue[i+1]));
            break;

         case MULT_STR_ARG :

            /* Find out how many arguments there are.  There are at least 2 */

            ptr  = (char *) (&msg_queue[i+1]);
            arg1 = strtok(ptr, "\n");
            arg2 = strtok(NULL, "\n");
            arg3 = strtok(NULL, "\n");
            arg4 = NULL;

            if (arg3 != NULL) {
               arg4 = strtok(NULL, "\n");
            }

            PRINTMSG(msg_queue[i].line_num, msg_queue[i].msg_num,
                     msg_queue[i].sever, msg_queue[i].col_num,
                     arg1, arg2, arg3, arg4);
            break;
      }

      i = msg_queue[i].next_msg;
   }

   msg_queue_idx = NULL_IDX;

   TRACE (Func_Exit, "issue_deferred_msgs", NULL);

   return;

}  /* <func name> */


# ifdef _DEBUG

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Print a single src_stk entry to stderr.  This routine exist in this   *|
|*      file because the definition of src_stk and the macros that access it  *|
|*      are defined in src_input.h and src_input.m.			      *|
|*									      *|
|* Input parameters:							      *|
|*	The index of the src_stk entry to display.                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void  print_src_stk_entry(int	ss_idx)

{
   char		file_type[12];


   TRACE (Func_Entry, "print_src_stk_entry", NULL);

   switch (SRC_STK_FILE_TYPE(ss_idx)) {

      case Stdin_Src: 
         strcpy(file_type, "Stdin_Src");
         break;

      case Input_Src:
         strcpy(file_type, "Input_Src");
         break;

      case Include_Src:
         strcpy(file_type, "Include_Src");
   }

   fprintf(stderr, "Entry #%3d   File line = %-5d%12sFile type = %s\n"
                   "%13sPrev src form = %10s   File name start = %d\n"
                   "%13sFull path name = %s",
                   ss_idx, SRC_STK_FILE_LINE(ss_idx), " ", file_type,
                   " ", (SRC_STK_PREV_SRC_FORM(ss_idx) == Fixed_Form) ?
                           "Fixed Form" : "Free Form",
                   SRC_STK_FILE_IDX(ss_idx),
                   " ", SRC_STK_PATH_NAME(ss_idx));

   fprintf(stderr, "\n%13sCIF file id = %d",
                   " ", SRC_STK_CIF_FILE_ID(ss_idx));

   putc('\n', stderr);

   TRACE (Func_Exit, "print_src_stk_entry", NULL);

   return;

}  /* print_src_stk_entry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Used by mem_report to print starting info about the src tables.       *|
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

void  print_src_input_tbls(void)

{
   fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                        "msg_queue",
                        "init size", msg_queue_init_size,
                        "increment", msg_queue_inc,
                        "num words", msg_queue_num_wds);
   fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                        "src_stk",
                        "init size", src_stk_init_size,
                        "increment", src_stk_inc,
                        "num words", src_stk_num_wds);

   return;

}  /* print_src_input_tbls */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This scan routine makes the guess about whether a digit string is     *|
|*      a format label or the start of a something else.                      *|
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

boolean	digit_is_format_label(void)

{
   int			ich;
   int			idx;
   boolean		is_label = TRUE;
   int			lblank   = ' ';
   int			line_idx;
   int			ltab	 = '\t';

   TRACE (Func_Entry, "digit_is_format_label", NULL);

   idx 		= stmt_buf_idx;
   line_idx	= stmt_line_idx;
   ich          = stmt_buf[idx];

   if (source_form == Free_Form) {
      lblank    = -1;
      ltab      = -1;
   }

   while (ich != EOF && (ch_class[ich] == Ch_Class_Digit ||
          ich == lblank || ich == ltab))   {

      if (++idx == stmt_line_end_idx[line_idx]) {

         if (line_idx < lines_in_buf) {
            idx = stmt_line_start_idx[++line_idx]
                  + stmt_line_offset[line_idx] + 1;
         }
         else {
            break;
         }
      }
      ich = stmt_buf[idx];
   }

   if (ich == dot                 || 
       ich == underscore          ||
       ich == lc_d || ich == uc_d ||
       ich == lc_e || ich == uc_e ||
       ich == lc_b || ich == uc_b ||
       ich == lc_h || ich == uc_h ||
       ich == lc_l || ich == uc_l ||
       ich == lc_r || ich == uc_r) {

      is_label = FALSE;
   }

   TRACE (Func_Exit, "digit_is_format_label", NULL);

   return(is_label);

}  /* digit_is_format_label */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine returns the base path name for the file.  It exists      *|
|*      because src_stk is only defined in terms of this module and it        *|
|*      would be quite an implementation to move all this out to the          *|
|*      global level.  This is only used by the PDGCS_debug_init call.        *|
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

char *get_src_path_name(void)
{

   return(SRC_STK_PATH_NAME(SRC_STK_BASE_IDX));

}  /* get_src_path_name */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      This routine checks to see if the next two characters are (/.         *|
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

boolean next_tok_is_paren_slash(void)

{
   int			ich;
   int			idx;
   int			line_idx;
   int			lblank = ' ';
   int			ltab   = '\t';
   boolean		paren_slash = FALSE;

   TRACE (Func_Entry, "next_tok_is_paren_slash", NULL);

   idx = stmt_buf_idx;
   line_idx = stmt_line_idx;

   if (source_form != Fixed_Form) {
      lblank = -1;
      ltab   = -1;
   }

   ich = stmt_buf[idx];
   
   if (ich == lparen) {
      
      do {
         idx++;

         if (idx == stmt_line_end_idx[line_idx]) {

            if (line_idx < lines_in_buf) {
               idx = stmt_line_start_idx[++line_idx]
                     + stmt_line_offset[line_idx] + 1;
            }
            else {
               break;
            }
         }

         ich = stmt_buf[idx];
      }
      while (ich == lblank | ich == ltab);

      if (ich == slash) {
         paren_slash = TRUE;
      }
   }


   TRACE (Func_Exit, "next_tok_is_paren_slash", NULL);

   return(paren_slash);

}  /* next_tok_is_paren_slash */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Search back from the LA_CH to find the column and line of the previous*|
|*      significant character.                                                *|
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

void prev_char_line_and_col(int		*line,
			    int		*col)

{
   int		ich;
   int		idx;
   int		line_idx;

   TRACE (Func_Entry, "prev_char_line_and_col", NULL);

   idx = stmt_buf_idx;
   line_idx = stmt_line_idx;

   do {

      idx--;
       
      if (idx <= stmt_line_start_idx[line_idx] + stmt_line_offset[line_idx]) {
         
         if (line_idx > 1) {
            line_idx--;
            idx = stmt_line_end_idx[line_idx];
            ich = blank;
            continue;
         }
         else {
            break;
         }
      }

      ich = stmt_buf[idx];
   }
   while (ich == blank | ich == tab);

   *line = stmt_line_num[line_idx];
   *col  = stmt_buf_col[idx];

   TRACE (Func_Exit, "prev_char_line_and_col", NULL);

   return;

}  /* prev_char_line_and_col */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine is called when the initial token of a stmt is Savelast.  *|
|*      If in free form, just return false. If in fixed form, this becomes    *|
|*      a SAVE stmt. LA_CH must be reset to be the "L" in LAST.               *|
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

boolean	stmt_is_save_stmt(int buf_idx, int stmt_num)

{
   boolean	is_save_stmt = FALSE;

   TRACE (Func_Entry, "stmt_is_save_stmt", NULL);

   if (source_form == Free_Form) {
      goto EXIT;
   }
   else {
      is_save_stmt = TRUE;
   }

   reset_src_input(buf_idx, stmt_num);

   while (LA_CH_VALUE != 'L') {
      NEXT_LA_CH;
   }

EXIT:

   TRACE (Func_Exit, "stmt_is_save_stmt", NULL);

   return(is_save_stmt);

}  /* stmt_is_save_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine adds a message to the message queue.                     *|
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

void	ntr_msg_queue(int			 line,
		      int			 msg_num,
		      msg_severities_type	 severity,
		      int			 column,
		      char			*str,
		      long			 arg,
		      int			 order)

{
   char		*char_idx;
   int		 idx;
   int		 length;
   int		 next_msg_idx;
   int		 num_entries;


   TRACE (Func_Entry, "ntr_msg_queue", NULL);

   if (msg_queue_size == 0) {
      CHECK_INITIAL_ALLOC(msg_queue, 1);
   }
   else {
      TBL_REALLOC_CK(msg_queue, 1);
   }

   CLEAR_TBL_NTRY(msg_queue, msg_queue_idx);

   msg_queue[msg_queue_idx].line_num	= line;
   msg_queue[msg_queue_idx].col_num	= column;
   msg_queue[msg_queue_idx].sever	= severity;
   msg_queue[msg_queue_idx].msg_num	= msg_num;
   msg_queue[msg_queue_idx].order	= order;
   msg_queue[msg_queue_idx].arg		= arg;

   if (str == NULL) {
      msg_queue[msg_queue_idx].str_len	= 0;
      msg_queue[msg_queue_idx].next_msg	= msg_queue_idx + 1;
   }
   else {
      next_msg_idx	= msg_queue_idx + 1;
      length		= strlen(str);
      num_entries	= (WORD_LEN(length) + NUM_MQ_WDS-1) / NUM_MQ_WDS;
      msg_queue_idx    += num_entries;

      msg_queue[next_msg_idx - 1].next_msg	= msg_queue_idx + 1;
      msg_queue[next_msg_idx - 1].str_len	= length;

      CHECK_TBL_ALLOC_SIZE(msg_queue, msg_queue_idx);

      char_idx	= ((char *) (&msg_queue[next_msg_idx]));

      for (idx = 0; idx < num_entries; idx++) {
         CLEAR_TBL_NTRY(msg_queue, next_msg_idx + idx);
      }

      strcpy(char_idx, str);
   }

   TRACE (Func_Exit, "ntr_msg_queue", NULL);

   return;

}  /* ntr_msg_queue */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine adds a message to the message queue for the next_line.   *|
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

void     ntr_next_msg_queue(int                        line,
                            int                        msg_num,
                            msg_severities_type        severity,
                            int                        column,
                            char                      *str,
                            long                       arg,
                            int                        order)

{
   char         *char_idx;
   int           idx;
   int           length;
   int           next_msg_idx;
   int           num_entries;


   TRACE (Func_Entry, "ntr_next_msg_queue", NULL);

   if (next_msg_queue_size == 0) {
      CHECK_INITIAL_ALLOC(next_msg_queue, 1);
   }
   else {
      TBL_REALLOC_CK(next_msg_queue, 1);
   }

   CLEAR_TBL_NTRY(next_msg_queue, next_msg_queue_idx);

   next_msg_queue[next_msg_queue_idx].line_num    = line;
   next_msg_queue[next_msg_queue_idx].col_num     = column;
   next_msg_queue[next_msg_queue_idx].sever       = severity;
   next_msg_queue[next_msg_queue_idx].msg_num     = msg_num;
   next_msg_queue[next_msg_queue_idx].order       = order;
   next_msg_queue[next_msg_queue_idx].arg         = arg;

   if (str == NULL) {
      next_msg_queue[next_msg_queue_idx].str_len  = 0;
      next_msg_queue[next_msg_queue_idx].next_msg = next_msg_queue_idx + 1;
   }
   else {
      next_msg_idx      = next_msg_queue_idx + 1;
      length            = strlen(str);
      num_entries       = (WORD_LEN(length) + NUM_MQ_WDS-1) / NUM_MQ_WDS;
      next_msg_queue_idx    += num_entries;

      next_msg_queue[next_msg_idx - 1].next_msg      = next_msg_queue_idx + 1;
      next_msg_queue[next_msg_idx - 1].str_len       = length;

      CHECK_TBL_ALLOC_SIZE(next_msg_queue, next_msg_queue_idx);

      char_idx  = ((char *) (&next_msg_queue[next_msg_idx]));

      for (idx = 0; idx < num_entries; idx++) {
         CLEAR_TBL_NTRY(next_msg_queue, next_msg_idx + idx);
      }

      strcpy(char_idx, str);
   }

   TRACE (Func_Exit, "ntr_next_msg_queue", NULL);

   return;

}  /* ntr_next_msg_queue */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Move the contents of next_msg_queue into msg_queue.                   *|
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

static void move_up_next_msg_queue(void)

{
#ifdef KEY /* Bug 10177 */
   char 	*chptr = 0;
#else /* KEY Bug 10177 */
   char 	*chptr;
#endif /* KEY Bug 10177 */
   int		i;
   int		k;
   boolean	duplicate;

   TRACE (Func_Entry, "move_up_next_msg_queue", NULL);

   i = 1;
   while (i <= next_msg_queue_idx) {

      duplicate = FALSE;

      switch(next_msg_queue[i].order) {
         case NO_ARG      :
         case ARG_ARG     :
            chptr = NULL;
            break;

         case STR_ARG     :
         case STR_ARG_ARG :
         case ARG_STR_ARG :
            chptr = (char *) (&next_msg_queue[i+1]);
            break;
      }

      k = 1;

      while (k < i) {
         if (next_msg_queue[k].msg_num == next_msg_queue[i].msg_num &&
             next_msg_queue[k].line_num == next_msg_queue[i].line_num &&
             next_msg_queue[k].col_num == next_msg_queue[i].col_num) {

            duplicate = TRUE;
            break;
         }

         k = next_msg_queue[k].next_msg;
      }

      if (extra_nxt_line != NULL_IDX &&
          next_msg_queue[i].line_num == pp_nxt_line_num[extra_nxt_line]) {

         /* this will be classified again, so skip this message */
         duplicate = TRUE;
      }

      if (! duplicate ) {
         ntr_msg_queue(next_msg_queue[i].line_num,
                       next_msg_queue[i].msg_num,
                       next_msg_queue[i].sever,
                       next_msg_queue[i].col_num,
                       chptr,
                       next_msg_queue[i].arg,
                       next_msg_queue[i].order);
      }

      i = next_msg_queue[i].next_msg;
   }
                    
   next_msg_queue_idx = NULL_IDX;

   TRACE (Func_Exit, "move_up_next_msg_queue", NULL);

   return;

}  /* move_up_next_msg_queue */

# ifdef _DEBUG

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine is called after compilation to gather memory usage       *|
|*      statistics.                                                           *|
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

void	final_src_input(void)

{
   TRACE (Func_Entry, "final_src_input", NULL);

   MEM_REPORT(src_stk);
   MEM_REPORT(msg_queue);

   TRACE (Func_Exit, "final_src_input", NULL);

   return;

}  /* final_src_input */
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

void preprocess_only_driver(void)

{
   TRACE (Func_Entry, "preprocess_only_driver", NULL);

   nxt_line_type = Comment_Line;
   include_found = FALSE;
   include_switch = FALSE;
   include_complete = FALSE;

   issue_classify_msg = FALSE;

   while (nxt_line_type != EOF_Line) {

      nxt_line_type = Regular_Line;

      if (get_nxt_line()) {

         if (include_switch) {
            update_global_line();         /* enter global_line_tbl  */
            include_switch = FALSE;
         }

         if (issue_pound_exit_line) {
            OUTPUT_POUND_INCLUDE_EXIT_LINE(curr_glb_line);
            issue_pound_exit_line = FALSE;
         }

         nxt_line_mp_line = FALSE;

         if (nxt_line_type != Cond_Comp_Line) {
            PP_ORIG_SIZE = line_size;
            classify_line();
         }

         if (change_source_form) {
            if (source_form == Fixed_Form) {
               source_form = Free_Form;
               line_size = FREE_SRC_LINE_SIZE;
               expected_line = Regular_Line;
            }
            else {
               source_form = Fixed_Form;
               if (cmd_line_flags.line_size_80) {
                  line_size = FIXED_SRC_LINE_SIZE_80;
               }
               else if (cmd_line_flags.line_size_132) {
                  line_size = FIXED_SRC_LINE_SIZE_132;
               }
               else {
                  line_size = FIXED_SRC_LINE_SIZE_72;
               }
            }

            change_source_form = FALSE;
         }

         if (nxt_line_type == Cond_Comp_Line) {

            fprintf(dot_i_fptr, "\n");
            previous_global_line++;

            if (parse_cc_line()) {

               /* if result is true, then it was an include line */

               include_stmt_file_line = SRC_STK_FILE_LINE(src_stk_idx);

#ifdef KEY /* Bug 10151 */
               if (open_include_file (TRUE, FALSE))
#else /* KEY Bug 10151 */
               if (open_include_file (TRUE))
#endif /* KEY Bug 10151 */
	       {
                  include_found  = TRUE;      /* flag begin of file */
                  include_switch = TRUE;      /* flag file switch   */
               }
            }

         }
         else {
            if (ignore_source_line) {

               fprintf(dot_i_fptr, "\n");
               previous_global_line++;
            }
            else {
               print_nxt_line();
            }  
         }
      }
      else {
         /* need to bump up pp_line_idx since classify_line was not called */

         if (cmd_line_flags.pp_macro_expansion) {
            pp_line_idx++;
         }

         /* check for termination of include file */

         if (src_stk_idx > SRC_STK_BASE_IDX) {    /* curr src is include*/
            include_complete = TRUE;              /* flag end of file   */
            nxt_line_type    = Comment_Line;      /* make EOF a comment */
            curr_glb_line--;            /* don't count this line */

            GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                      SRC_STK_FILE_LINE(src_stk_idx);
            set_related_gl_source_lines(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));

            if (source_form != SRC_STK_PREV_SRC_FORM(src_stk_idx)) {
               change_source_form = TRUE;
            }
            POP_SRC;
            include_switch = TRUE;                /* flag file switch   */
            issue_pound_exit_line = TRUE;
         }
         else {                                   /* curr src is input  */
            GL_SOURCE_LINES(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx)) =
                      SRC_STK_FILE_LINE(src_stk_idx);
            set_related_gl_source_lines(SRC_STK_GLOBAL_LINE_IDX(src_stk_idx));
            nxt_line_type = EOF_Line;             /* end of compilation */
            nxt_line_EOL = 0;
         }
      }

      move_up_next_msg_queue();
   }

   issue_classify_msg = TRUE;

   TRACE (Func_Exit, "preprocess_only_driver", NULL);

   return;

}  /* preprocess_only_driver */

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

void set_related_gl_source_lines(int	gl_idx)

{
   int			i;
   int			id;

   TRACE (Func_Entry, "set_related_gl_source_lines", NULL);

   id = GL_CIF_FILE_ID(gl_idx);

   for (i = gl_idx - 1; i > 0; i--) {
      if (GL_CIF_FILE_ID(i) == id) {
         GL_SOURCE_LINES(i) = GL_SOURCE_LINES(gl_idx);
      }
   }

   TRACE (Func_Exit, "set_related_gl_source_lines", NULL);

   return;

}  /* set_related_gl_source_lines */

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

int ntr_io_string_constant(void)

{
   char		       *char_ptr;
   int			cn_idx = NULL_IDX;
   long			count = 0;
   int			idx;
   int			ich;
   int			line_idx;
   int			type_idx;

   TRACE (Func_Entry, "ntr_io_string_constant", NULL);

   /* first, count the significant characters */
 
   for (line_idx = 1; line_idx <= lines_in_buf; line_idx++) {
      idx = stmt_line_start_idx[line_idx] + stmt_line_offset[line_idx] + 1;

      while (idx < stmt_line_end_idx[line_idx]) {

         ich = stmt_buf[idx];

         if (ich == blank | ich == tab | ich == newline) {
            /* intentionally blank */
         }
         else {
            count++;
         }
         idx++;
      }
   }
   
   /* get an empty character constant */

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)         = Character;
   TYP_LINEAR(TYP_WORK_IDX)       = CHARACTER_DEFAULT_TYPE;
   TYP_DESC(TYP_WORK_IDX)         = Default_Typed;
   TYP_CHAR_CLASS(TYP_WORK_IDX)   = Const_Len_Char;
   TYP_FLD(TYP_WORK_IDX)          = CN_Tbl_Idx;
   TYP_IDX(TYP_WORK_IDX)          = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, count),
   type_idx                       = ntr_type_tbl();
   cn_idx                         = ntr_const_tbl(type_idx, TRUE, NULL);

   /* fill in the character constant */

   char_ptr = (char *) &CN_CONST(cn_idx);

   count = 0;

   for (line_idx = 1; line_idx <= lines_in_buf; line_idx++) {
      idx = stmt_line_start_idx[line_idx] + stmt_line_offset[line_idx] + 1;

      while (idx < stmt_line_end_idx[line_idx]) {

         ich = stmt_buf[idx];

         if (ich == blank | ich == tab | ich == newline) {
            /* intentionally blank */
         }
         else {
            char_ptr[count] = ich;
            count++;
         }
         idx++;
      }
   }


   TRACE (Func_Exit, "ntr_io_string_constant", NULL);

   return(cn_idx);

}  /* ntr_io_string_constant */

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

boolean omp_extension_prefix(int	line)

{
   boolean	is_extension = FALSE;
   int		line_idx;

   TRACE (Func_Entry, "omp_extension_prefix", NULL);

   for (line_idx = 1; line_idx <= lines_in_buf; line_idx++) {
      if (line == stmt_line_num[line_idx]) {
         break;
      }
   }

   if (line_idx <= lines_in_buf &&
       line_dir_prefix[line_idx] == Csgi_Dir) {

      is_extension = TRUE;
   }


   TRACE (Func_Exit, "omp_extension_prefix", NULL);

   return(is_extension);

}  /* omp_extension_prefix */

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

static void print_nxt_line(void)

{
   int		break_point;
   char		ch;
   int		i;
   int		remaining_room;
   int		whats_left;
   boolean	continued_line = FALSE;

   TRACE (Func_Entry, "print_nxt_line", NULL);

   if (PP_LINE_TYPE == Comment_Line &&
       nxt_line[NXT_COL(1)] == '#') {

      for (i = NXT_COL(1); i < nxt_line_end_idx[pp_line_idx]; i++) {
         fprintf(dot_i_fptr, "%c", (char)nxt_line[i]);
      }
      goto EXIT;
   }

   i = 1;
   whats_left = PP_IDX_TO_COL(PP_EOL) - 1;
   remaining_room = line_size;

   while (whats_left > 0) {

      if (whats_left > remaining_room &&
          PP_LINE_TYPE != Comment_Line) {
         /* find break_point */
         break_point = i + remaining_room - 2;

         while (ch_class[nxt_line[NXT_COL(break_point)]] == Ch_Class_Letter  ||
                ch_class[nxt_line[NXT_COL(break_point)]] == Ch_Class_Digit   ||
                nxt_line[NXT_COL(break_point)] == USCORE           ||
                nxt_line[NXT_COL(break_point)] == DOLLAR           ||
                nxt_line[NXT_COL(break_point)] == AT_SIGN) {

            break_point--;
         }
      }
      else {
         break_point = -1;
      }

      if (PP_LINE_TYPE == Comment_Line) {
         previous_global_line++;
      }
      else {
         if (PP_LINE_NUM != previous_global_line + 1 &&
             ! continued_line) {
            OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
         }

         previous_global_line = PP_LINE_NUM;
      }

      while (i != break_point &&
             (ch = nxt_line[NXT_COL(i)]) != newline && ch != eos) {

         if (i == 1 &&
             PP_MP_LINE) {

            /* replace blanks with !$ */

            fprintf(dot_i_fptr, "!$");
            i += 2;
            whats_left -= 2;
         }
         else {
            fprintf(dot_i_fptr, "%c", ch);
            i++;
            whats_left--;
         }
      }

      if (i == break_point) {
         /* continue the line */
         remaining_room = line_size;
         continued_line = TRUE;

         if (source_form == Fixed_Form) {
            fprintf(dot_i_fptr, "\n");
            OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);

            if (PP_LINE_TYPE == Dir_Line) {
               switch (PP_ACTUAL_DIR_PREFIX) {
               case Cdir_Dir:
                  fprintf(dot_i_fptr, "!DIR$& ");
                  remaining_room -= 7;
                  break;

               case Cmic_Dir:
                  fprintf(dot_i_fptr, "!MIC$& ");
                  remaining_room -= 7;
                  break;

               case Cpar_Dir:
                  fprintf(dot_i_fptr, "!$PAR& ");
                  remaining_room -= 7;
                  break;

               case Cstar_Dir:
                  fprintf(dot_i_fptr, "!*$*& ");
                  remaining_room -= 6;
                  break;

               case Cdollar_Dir:
                  fprintf(dot_i_fptr, "!$& ");
                  remaining_room -= 4;
                  break;

               case Comp_Dir:
                  fprintf(dot_i_fptr, "!$OMP& ");
                  remaining_room -= 7;
                  break;

               case Cdbg_Dir:
                  fprintf(dot_i_fptr, "!DBG$& ");
                  remaining_room -= 7;
                  break;

               case Csgi_Dir:
                  fprintf(dot_i_fptr, "!$SGI& ");
                  remaining_room -= 7;
                  break;

               }
            }
            else if (PP_MP_LINE) {
               fprintf(dot_i_fptr, "!$   .");
               remaining_room -= 7;
            }
            else {
               fprintf(dot_i_fptr, "     .");
               remaining_room -= 7;
            }
         }
         else {
            /* source == Free_Form */

            if (PP_LINE_TYPE == Dir_Line) {
               switch (PP_ACTUAL_DIR_PREFIX) {
               case Cdir_Dir:
                  fprintf(dot_i_fptr, "\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!DIR$& ");
                  remaining_room -= 7;
                  break;

               case Cmic_Dir:
                  fprintf(dot_i_fptr, "\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!MIC$& ");
                  remaining_room -= 7;
                  break;

               case Cpar_Dir:
                  fprintf(dot_i_fptr, "\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!$PAR& ");
                  remaining_room -= 7;
                  break;

               case Cstar_Dir:
                  fprintf(dot_i_fptr, "\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!*$*& ");
                  remaining_room -= 6;
                  break;

               case Cdollar_Dir:
                  fprintf(dot_i_fptr, "\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!$& ");
                  remaining_room -= 4;
                  break;

               case Comp_Dir:
                  fprintf(dot_i_fptr, "&\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!$OMP& ");
                  remaining_room -= 7;
                  break;

               case Cdbg_Dir:
                  fprintf(dot_i_fptr, "\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!DBG$& ");
                  remaining_room -= 7;
                  break;

               case Csgi_Dir:
                  fprintf(dot_i_fptr, "&\n");
                  OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
                  fprintf(dot_i_fptr, "!$SGI& ");
                  remaining_room -= 7;
                  break;

               }
            }
            else if (PP_MP_LINE) {
               fprintf(dot_i_fptr, "&\n");
               OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
               fprintf(dot_i_fptr, "!$ & ");
               remaining_room -= 5;
            }
            else {
               fprintf(dot_i_fptr, "&\n");
               OUTPUT_POUND_LINE_NUM(PP_LINE_NUM);
               fprintf(dot_i_fptr, "& ");
               remaining_room -= 2;
            }
         }
      }
   }
   fprintf(dot_i_fptr, "\n");

EXIT:

   TRACE (Func_Exit, "print_nxt_line", NULL);

   return;

}  /* print_nxt_line */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Wrapper routine for fixed_classify_line and free_classify_line.       *|
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

static void classify_line(void)

{


   TRACE (Func_Entry, "classify_line", NULL);

   if (cmd_line_flags.pp_macro_expansion) {
      pp_line_idx++;

# ifdef _DEBUG
      if (pp_line_idx > nxt_line_num_lines) {
         PRINTMSG(pp_nxt_line_num[pp_line_idx-1], 626, Internal, 1,
                  "valid pp_line_idx", "classify_line");
      }
# endif
   }
   else {

      PP_MP_LINE = FALSE;
      PP_CHANGE_SOURCE_FORM = FALSE;

      if (source_form == Fixed_Form) {
         fixed_classify_line();
      }
      else {
         free_classify_line();
      }
   }

   if (source_form == Free_Form) {
      expected_line = PP_EXPECTED_LINE;
   }

   nxt_line_idx               = PP_IDX_TO_COL(PP_IDX);
   nxt_line_label             = PP_LABEL;
   nxt_line_length            = pp_nxt_line_length[pp_line_idx];
   nxt_line_num               = PP_LINE_NUM;
   nxt_line_type              = PP_LINE_TYPE;
   nxt_line_EOL               = PP_IDX_TO_COL(PP_EOL);
   nxt_line_prefix_len        = PP_PREFIX_LEN;
   nxt_line_dir_prefix        = PP_DIR_PREFIX;
   nxt_line_actual_dir_prefix = PP_ACTUAL_DIR_PREFIX;
   nxt_line_mp_line           = PP_MP_LINE;
   change_source_form	      = PP_CHANGE_SOURCE_FORM;

   TRACE (Func_Exit, "classify_line", NULL);

   return;

}  /* classify_line */

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

static boolean get_nxt_line(void)

{
   boolean	not_EOF = TRUE;

   TRACE (Func_Entry, "get_nxt_line", NULL);

   if (! cmd_line_flags.pp_macro_expansion) {
      nxt_line_num_lines = 0;

      not_EOF = read_line(FALSE);
   }
   else {
      if (pp_line_idx >= nxt_line_num_lines) {
         pp_get_stmt();
      }

      /* reset nxt_line_type so that Cond_Comp_Lines aren't done twice */
      nxt_line_type = Regular_Line; 

      if (pp_nxt_line_type[pp_line_idx + 1] == EOF_Line) {
         not_EOF = FALSE;
         nxt_line_idx = pp_nxt_line_idx[pp_line_idx + 1];
      }
   }

   TRACE (Func_Exit, "get_nxt_line", NULL);

   return(not_EOF);

}  /* get_nxt_line */

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

static void pp_get_stmt (void)

{
   boolean		cc_include_line;

   TRACE (Func_Entry, "pp_get_stmt", NULL);

   SAVE_GLOBAL_VARIABLES

   pp_line_idx = 0;
   nxt_line_num_lines = 0;

   /* loop while stmt continues */
   do {

      /* get next line from src input file */

      nxt_line_type = Regular_Line;
      cc_include_line = FALSE;

      if (read_line (FALSE)) {              /* read next src line */

         if (include_switch) {
            update_global_line();         /* enter global_line_tbl  */
            include_switch = FALSE;
         }

         if (issue_pound_exit_line) {
            OUTPUT_POUND_INCLUDE_EXIT_LINE(curr_glb_line);
            issue_pound_exit_line = FALSE;
         }

         if (nxt_line_type != Cond_Comp_Line) {
            pp_line_idx++;
            PP_MP_LINE = FALSE;
            PP_CHANGE_SOURCE_FORM = FALSE;
            PP_ORIG_SIZE = line_size;

            if (source_form == Fixed_Form) {
               fixed_classify_line();

               while (PP_LINE_TYPE == Comment_Line &&
                      nxt_line[NXT_COL(1)] > 0 &&
                      (nxt_line[NXT_COL(1)] == uc_c ||
                       nxt_line[NXT_COL(1)] == lc_c) &&
                      scan_fixed_comment()) {

                  PP_MP_LINE = FALSE;
                  PP_CHANGE_SOURCE_FORM = FALSE;
                  fixed_classify_line();
               }
            }
            else {
               free_classify_line();
               expected_line = PP_EXPECTED_LINE;
            }

            switch (PP_LINE_TYPE) {
            case Comment_Line:

               if (on_off_flags.preprocess_only || on_off_flags.save_dot_i) {

                  if (ignore_source_line) {
                     /* print blank line */
#ifdef KEY /* Bug 4724 */
		     /* See comment for bug 4724 below */
#else
                     fprintf(dot_i_fptr, "\n");
#endif /* KEY Bug 4724 */
                     previous_global_line++;
                  }
                  else {
#ifdef KEY /* Bug 4724 */
		     /* When a series of more than one regular line (and
		      * their continuation lines, if any) ends with a comment
		      * line, this causes the comment line to appear ahead of
		      * the last regular line, since this function saves up
		      * the regular and continuation lines, and returns to the
		      * caller, who prints them later on. That messes up the
		      * numbering of the regular line. Suppressing the comment
		      * ensures that the preceding regular line has the
		      * correct number, and because each series of regular
		      * lines is preceded by a "# lineno" directive whose
		      * value is independent of whether we print the comment,
		      * any subsequent regular line will also have the correct
		      * number. Preprocessor output (e.g. from cpp) normally
		      * suppresses the content of comments anyway.
		      */
#else
                     print_nxt_line();
#endif /* KEY Bug 4724 */
                  }
               }

               pp_line_idx--;
               nxt_line_num_lines--;
               break;

            case Dir_Line:
            case Regular_Line:
               include_found      = FALSE;        /* and include flags  */
               include_complete   = FALSE;
               break;

            case Continuation_Line:
            case Dir_Continuation_Line:
               if (include_found) {
                  include_found = FALSE;

                  /* First line of included file must not be a cont line*/

                  ntr_msg_queue(PP_LINE_NUM, 53, Error,
                                (source_form == Fixed_Form ? CONTINUE_COLUMN :
                                                   nxt_line_col[PP_IDX]),
                                (char *)NULL,
                                0,
                                NO_ARG);
               }

               if (include_complete) {
                  include_complete = FALSE;

                  /* Next line of file after include must not be a cont */

                  ntr_msg_queue(PP_LINE_NUM, 54, Error,
                                (source_form == Fixed_Form ? CONTINUE_COLUMN :
                                                   nxt_line_col[PP_IDX]),
                                (char *)NULL,
                                0,
                                NO_ARG);
               }
               break;

            case Include_Line:

               cc_include_line = TRUE;
               pp_line_idx--;
               nxt_line_num_lines--;

               include_stmt_file_line = SRC_STK_FILE_LINE(src_stk_idx);

#ifdef KEY /* Bug 10151 */
               if (open_include_file (FALSE, FALSE))
#else /* KEY Bug 10151 */
               if (open_include_file (FALSE))
#endif /* KEY Bug 10151 */
	       {
                  include_found  = TRUE;          /* flag begin of file */
                  include_switch = TRUE;          /* flag file switch   */
               }

               break;
            }  /* switch */
         }
         else if (parse_cc_line()) {

            /* if result is true, then it was an include line */

            cc_include_line = TRUE;
            include_stmt_file_line = SRC_STK_FILE_LINE(src_stk_idx);

#ifdef KEY /* Bug 10151 */
            if (open_include_file (TRUE, FALSE))
#else /* KEY Bug 10151 */
            if (open_include_file (TRUE))
#endif /* KEY Bug 10151 */
	    {
               include_found  = TRUE;      /* flag begin of file */
               include_switch = TRUE;      /* flag file switch   */
            }
         }
      }
      else {                                      /* EOF on source file */

         /* need to bump pp_line_idx since classify_line was not called */

         if (cmd_line_flags.pp_macro_expansion) {
            pp_line_idx++;
            PP_MP_LINE = FALSE;
            PP_CHANGE_SOURCE_FORM = FALSE;
         }
      }
   }
   while (PP_LINE_TYPE != EOF_Line &&
          nxt_line_num_lines < MAX_FIXED_LINES &&
          ! PP_CHANGE_SOURCE_FORM &&
          (nxt_line_num_lines <= 1 ||
           cc_include_line ||
           PP_LINE_TYPE == Comment_Line ||
           PP_LINE_TYPE == Continuation_Line ||
           PP_LINE_TYPE == Dir_Continuation_Line));


   if (nxt_line_num_lines > 1 &&
       pp_nxt_line_type[nxt_line_num_lines] != Comment_Line &&
       pp_nxt_line_type[nxt_line_num_lines] != Continuation_Line &&
       pp_nxt_line_type[nxt_line_num_lines] != Dir_Continuation_Line) {

      extra_nxt_line = nxt_line_num_lines;
      nxt_line_num_lines--;
   }

   if (pp_nxt_line_type[1] != EOF_Line) {

      while (scan_fortran_stmt()) {
         pp_line_idx = 0;

         RESTORE_GLOBAL_VARIABLES

         while (++pp_line_idx <= nxt_line_num_lines) {

            if (PP_LINE_TYPE == Comment_Line &&
                nxt_line[NXT_COL(1)] == '#') {
               continue;
            }

            PP_MP_LINE = FALSE;
            PP_CHANGE_SOURCE_FORM = FALSE;

            if (source_form == Fixed_Form) {
               fixed_classify_line();
            }
            else {
               free_classify_line();
               expected_line = PP_EXPECTED_LINE;
            }
         }
      }
   }

   expected_line = save_expected_line;

   /* reset pp_line_idx to 0 on exit */
   pp_line_idx = 0;

   TRACE (Func_Exit, "pp_get_stmt", NULL);

   return;

}  /* pp_get_stmt */

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

static void shift_to_line_size(int	shift)

{
   int		end_line;
   int		i;
   int		start_idx;

   TRACE (Func_Entry, "shift_to_line_size", NULL);

   if (extra_nxt_line) {
      end_line = extra_nxt_line;
   }
   else {
      end_line = nxt_line_num_lines;
   }

   start_idx = nxt_line_end_idx[pp_line_idx];

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

   nxt_line_end_idx[pp_line_idx] += shift;
   pp_nxt_line_length[pp_line_idx] += shift;
   pp_nxt_line_EOL[pp_line_idx] += shift;

   for (i = pp_line_idx + 1; i <= end_line; i++) {
      nxt_line_start_idx[i] += shift;
      nxt_line_end_idx[i] += shift;
      pp_nxt_line_EOL[i] += shift;
   }

   TRACE (Func_Exit, "shift_to_line_size", NULL);

   return;

}  /* shift_to_line_size */

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

void get_curr_file_name(char *str)

{


   TRACE (Func_Entry, "get_curr_file_name", NULL);

   sprintf(str, "\"%s\"", SRC_STK_FILE_NAME(src_stk_idx));

   TRACE (Func_Exit, "get_curr_file_name", NULL);

   return;

}  /* get_curr_file_name */
