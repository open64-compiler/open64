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


/* USMID:  "\n@(#)5.0_pl/macros/src_input.m	5.2	07/19/99 12:03:43\n" */
 

/*****************\
|* MISCELLANEOUS *|
\*****************/

# define CONTINUE_COLUMN	6		/* fixed src continuation col */

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
#    define SIGN_BIT 		63
#    define NUM_MQ_WDS 		3
# else
#    define SIGN_BIT 		31
# ifdef _WHIRL_HOST64_TARGET64
#    define NUM_MQ_WDS 		3
# else
#    define NUM_MQ_WDS 		5
# endif
# endif

# define MASK_CHAR_CONST_IDX	017777777400

# define NO_ARG 		0
# define STR_ARG		1
# define ARG_ARG		2
# define STR_ARG_ARG		3
# define ARG_STR_ARG		4
# define MULT_STR_ARG		5

/********************\
|* SIZES AND LIMITS *|
\********************/

# define MAX_ASCII_CHARS	256		/* max ascii characters	      */

/* source input line sizes */

# define FIXED_SRC_LINE_SIZE_72 72		/* std fixed src line size    */
# define FIXED_SRC_LINE_SIZE_80 80		/* alt fixed src line size    */
# define FIXED_SRC_LINE_SIZE_132	132	/* alt fixed src line size    */
#ifdef KEY /* Bug 3632 */
# define FREE_SRC_LINE_SIZE	2048		/* std free src line size     */
# define MAX_SRC_LINE_SIZE	(FREE_SRC_LINE_SIZE+3)	/* maximum source line size   */
#else
# define FREE_SRC_LINE_SIZE	132		/* std free src line size     */
# define MAX_SRC_LINE_SIZE	135		/* maximum source line size   */
#endif /* KEY Bug 3632 */
						/* it is 1 based, not 0 */
#ifdef KEY /* Bug 3449 */
# define MAX_STMT_CHAR_SIZE     33750           /* maximum statement size     */
#else
# define MAX_STMT_CHAR_SIZE     13500           /* maximum statement size     */
#endif /* KEY */

/* max number of source lines per statement */

# define MAX_ANSI_FIXED_LINES	20
# define MAX_ANSI_FREE_LINES	40
# define MAX_FIXED_LINES        500
# define MAX_FREE_LINES         500


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

# define SRC_STK_BASE_IDX	1

# define SRC_STK_CIF_FILE_ID(IDX)	src_stk[IDX].cif_file_id
# define SRC_STK_FILE_IDX(IDX)		src_stk[IDX].file_idx
# define SRC_STK_FILE_LINE(IDX)		src_stk[IDX].file_line
# define SRC_STK_FILE_NAME(IDX)	  &src_stk[IDX].path_name[src_stk[IDX].file_idx]
# define SRC_STK_DO_NOT_FCLOSE(IDX)	src_stk[IDX].do_not_fclose
# define SRC_STK_FILE_PTR(IDX)		src_stk[IDX].file_ptr
# define SRC_STK_FILE_TYPE(IDX)		src_stk[IDX].file_type
# define SRC_STK_GLOBAL_LINE_IDX(IDX)	src_stk[IDX].global_line_first_idx
# define SRC_STK_PREV_SRC_FORM(IDX)     src_stk[IDX].prev_src_form
# define SRC_STK_PATH_NAME(IDX)		src_stk[IDX].path_name

/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/

# define IS_1TO9(CH)		((CH) >= one && (CH) <= nine)

/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

/* pop a source stack entry */
# define POP_SRC							       \
	{	if (! SRC_STK_DO_NOT_FCLOSE(src_stk_idx))                      \
		   fclose(SRC_STK_FILE_PTR(src_stk_idx));		       \
		if (src_stk_idx == NULL_IDX) { /* src_stk_idx underflow */     \
		   PRINTMSG (curr_glb_line, 12, Internal, 0, "src_stk_idx");   \
		}							       \
		src_stk_idx--;						       \
	}

# define OUTPUT_POUND_INCLUDE_EXIT_LINE(GLB_LINE)                              \
        if (on_off_flags.output_pound_lines &&                                              \
            (on_off_flags.preprocess_only || on_off_flags.save_dot_i)) {       \
           int _g_idx; int _file_line;                                         \
           GLOBAL_LINE_TO_FILE_LINE(GLB_LINE, _g_idx, _file_line);             \
           fprintf(dot_i_fptr,"# %d \"%s\" 2\n", _file_line,                   \
                   SRC_STK_FILE_NAME(src_stk_idx));                            \
           fprintf(dot_i_fptr,"# %d \"%s\"\n", _file_line,                     \
                   SRC_STK_FILE_NAME(src_stk_idx));                            \
        }

# define OUTPUT_POUND_LINE_NUM(GLB_LINE)                                       \
        if (on_off_flags.output_pound_lines &&                                              \
            (on_off_flags.preprocess_only || on_off_flags.save_dot_i)) {       \
           int _g_idx; int _file_line;                                         \
           GLOBAL_LINE_TO_FILE_LINE(GLB_LINE, _g_idx, _file_line);             \
           fprintf(dot_i_fptr,"# %d\n", _file_line);                           \
        }
	
/* Set sign bit of word holding a char in a char constant context */
# define MARK_CHAR_CONST(CH)    ((CH) = ((1 << SIGN_BIT) | (CH)))

# ifdef _DEBUG
# 	define PRINT_STMT_SRC()						       \
		if (dump_flags.src_dmp) {				       \
		   print_src();						       \
		}							        
/*              Lines are now output by print_buffered_messages.              */
/*              else if (dump_flags.stmt_dmp) {				       \
		   print_stmt();					       \
		}                                                             */
# else
# 	define PRINT_STMT_SRC()
# endif

# define SAVE_GLOBAL_VARIABLES                          \
   save_prev_char_delim = prev_char_delim;              \
   save_prev_char_delim_idx = prev_char_delim_idx;      \
   save_previous_char = previous_char;                  \
   save_seen_lp_eq_slash = seen_lp_eq_slash;            \
   save_char_delim = char_delim;                        \
   save_digit_start = digit_start;                      \
   save_format_idx = format_idx;                        \
   save_in_format = in_format;                          \
   save_expected_line = expected_line;                  \
   save_first_line = first_line;

# define RESTORE_GLOBAL_VARIABLES                    \
   prev_char_delim = save_prev_char_delim;           \
   prev_char_delim_idx = save_prev_char_delim_idx;   \
   previous_char = save_previous_char;               \
   seen_lp_eq_slash = save_seen_lp_eq_slash;         \
   char_delim = save_char_delim;                     \
   digit_start = save_digit_start;                   \
   format_idx = save_format_idx;                     \
   in_format = save_in_format;                       \
   expected_line = save_expected_line;               \
   first_line = save_first_line;

# define NXT_COL(COL)            (nxt_line_start_idx[pp_line_idx]+(COL)-1)
# define PP_IDX_TO_COL(IDX)	 ((IDX - nxt_line_start_idx[pp_line_idx]) + 1)

# define PP_IDX                  pp_nxt_line_idx[pp_line_idx]
# define PP_LINE_TYPE            pp_nxt_line_type[pp_line_idx]
# define PP_DIR_PREFIX           pp_nxt_line_dir_prefix[pp_line_idx]
# define PP_ACTUAL_DIR_PREFIX    pp_nxt_line_actual_dir_prefix[pp_line_idx]
# define PP_PREFIX_LEN           pp_nxt_line_prefix_len[pp_line_idx]
# define PP_LINE_NUM             pp_nxt_line_num[pp_line_idx]
# define PP_EOL                  pp_nxt_line_EOL[pp_line_idx]
# define PP_LABEL		 pp_nxt_line_label[pp_line_idx]
# define PP_MP_LINE		 pp_nxt_line_mp_line[pp_line_idx]
# define PP_EXPECTED_LINE	 pp_expected_line[pp_line_idx]
# define PP_ORIG_SIZE		 pp_orig_line_size[pp_line_idx]
# define PP_CHANGE_SOURCE_FORM	 pp_change_source_form[pp_line_idx]

# define IS_DIR_CONTINUATION(DIR)				\
   ((nxt_line_num_lines > 1 && 					\
     pp_nxt_line_type[1] == Dir_Line &&				\
     pp_nxt_line_dir_prefix[1] == DIR) ||			\
    (nxt_line_num_lines <= 1 &&                                 \
     stmt_buf_type == Dir_Line &&				\
     stmt_buf_dir_prefix == DIR))
