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



/* USMID:  "\n@(#)5.0_pl/headers/src_input.h	5.3	09/01/99 12:24:50\n" */

 
/********************************************\
|* type specifiers used within this module. *|
\********************************************/

enum	src_file_values	       {Stdin_Src,
				Input_Src,
				Include_Src };

typedef enum			src_file_values		src_file_type;

struct	src_stk_entry	       {Uint		file_line		: 32;
				Uint		file_idx		: 32;
				src_file_type	file_type		: 4;
                                src_form_type   prev_src_form		: 4;
     				Uint		cif_file_id		: 24;
				Uint		global_line_first_idx	: 32;
				boolean		do_not_fclose;
				FILE	       *file_ptr;
				char		path_name[MAX_PATH_NAME_SIZE];
	};

enum	dir_prefix_values	{Cdir_Dir,
				 Cmic_Dir,
				 Cpar_Dir,
				 Cstar_Dir,
				 Cdollar_Dir,
				 Comp_Dir,
				 Cdbg_Dir,
				 Csgi_Dir};

typedef enum dir_prefix_values	dir_prefix_type;


/******************************************************************************\
|*	In the following struct for msg queueing, the value of order          *|
|*	determines the order of possible str and arg arguments.               *|
|*									      *|
|*			order		implies				      *|
|*			-----		-------				      *|
|*			  0		 no args			      *|
|*			  1		 str				      *|
|*			  2		 arg				      *|
|*			  3		 str, arg			      *|
|*			  4		 arg, str			      *|
|*									      *|
\******************************************************************************/

struct  msg_queue_entry	{Uint			line_num	: 24;
			 Uint			col_num		:  8;
			 Uint			msg_num		: 24;
			 msg_severities_type	sever		:  8;
			 Uint			order		:  8;
			 Uint			str_len		: 24;
			 Uint			next_msg	: 32;
			 long			arg;
			};


typedef	struct		msg_queue_entry		msg_queue_type;
typedef	struct		msg_queue_entry		next_msg_queue_type;
typedef struct		src_stk_entry		src_stk_type;


next_msg_queue_type *next_msg_queue;
long		next_msg_queue_idx	 = NULL_IDX;
int		next_msg_queue_inc	 = 5;
int		next_msg_queue_init_size = 5;   /* Same as increment */
long		next_msg_queue_limit	 = OUR_LONG_MAX;
int		next_msg_queue_num_wds	 = NUM_MQ_WDS;
long		next_msg_queue_size	 = 0;
long		next_msg_queue_largest_idx	= NULL_IDX;

msg_queue_type *msg_queue;
long		msg_queue_idx		= NULL_IDX;
int		msg_queue_inc		= 5;
int		msg_queue_init_size	= 5;   /* Same as increment */
long		msg_queue_limit		= OUR_LONG_MAX;
int		msg_queue_num_wds	= NUM_MQ_WDS;
long		msg_queue_size		= 0;
long		msg_queue_largest_idx	= NULL_IDX;


src_stk_type   * RESTRICT src_stk;
long		src_stk_idx		= NULL_IDX;
int		src_stk_inc		= 2;
int		src_stk_init_size	= 3;
long		src_stk_limit		= OUR_LONG_MAX;
int		src_stk_num_wds		= HOST_BYTES_TO_WORDS(
                                                        sizeof(src_stk_type));
long		src_stk_size		= 0;
long		src_stk_largest_idx	= NULL_IDX;


/****************************************\
|* static data used within this module. *|
\****************************************/

boolean                 change_source_form 	= FALSE;
int                     char_delim;
int			continuation_count;
int                     digit_start;
line_type               expected_line;
boolean			first_line 		= TRUE;
boolean			first_pound_line	= TRUE;
boolean			cif_file_rec_issued	= FALSE;
int			full_include_name_idx	= NULL_IDX;
boolean			ignore_source_line	= FALSE;

char			include_file[MAX_FILE_NAME_SIZE];
int			include_stmt_file_line	= 0;
int			include_stmt_file_col	= 0;
boolean			include_switch;
boolean			include_complete;
boolean			include_found;

boolean			issue_obsolete_src_form_msg	= FALSE;

char			pound_file[MAX_FILE_NAME_SIZE];
boolean			change_orig_src_file 	= FALSE;

int			line_size;
int                     lines_in_buf;
char                    num_str[5];
int                     num_idx;

# ifdef _FRONTEND_CONDITIONAL_COMP
int                     nxt_line[MAX_STMT_CHAR_SIZE];
int                     nxt_line_col[MAX_STMT_CHAR_SIZE];
# else
int                     nxt_line[MAX_SRC_LINE_SIZE];
int                     nxt_line_col[MAX_SRC_LINE_SIZE];
# endif
int                     nxt_line_idx;
boolean			nxt_line_label;
int                     nxt_line_length;
int                     nxt_line_num;
line_type               nxt_line_type;
int                     nxt_line_EOL;
int			nxt_line_prefix_len;
boolean			nxt_line_mp_line;

int                     nxt_line_start_idx[MAX_FIXED_LINES+1];
int                     nxt_line_end_idx[MAX_FIXED_LINES+1];
boolean                 pp_nxt_line_label[MAX_FIXED_LINES+1];
int                     pp_nxt_line_length[MAX_FIXED_LINES+1];
int                     pp_nxt_line_num[MAX_FIXED_LINES+1];
line_type               pp_nxt_line_type[MAX_FIXED_LINES+1];
int                     pp_nxt_line_EOL[MAX_FIXED_LINES+1];
int                     pp_nxt_line_prefix_len[MAX_FIXED_LINES+1];
int                     pp_nxt_line_idx[MAX_FIXED_LINES+1];
boolean                 pp_nxt_line_mp_line[MAX_FIXED_LINES+1];
boolean                 pp_change_source_form[MAX_FIXED_LINES+1];
dir_prefix_type		pp_nxt_line_actual_dir_prefix[MAX_FIXED_LINES+1];
dir_prefix_type 	pp_nxt_line_dir_prefix[MAX_FIXED_LINES+1];
line_type               pp_expected_line[MAX_FIXED_LINES+1];
int			pp_orig_line_size[MAX_FIXED_LINES+1];

int			nxt_line_num_lines;
int			pp_line_idx;

int			extra_nxt_line = NULL_IDX;

int			prev_char_delim = 0;
int			prev_char_delim_idx;
int                     previous_char;
boolean                 seen_lp_eq_slash;
int                     starting_pt;

int                     stmt_buf[MAX_STMT_CHAR_SIZE];
int                     stmt_buf_col[MAX_STMT_CHAR_SIZE];
int                     stmt_buf_idx;
line_type               stmt_buf_type;
int                     stmt_buf_EOS_idx;
int                     stmt_line_start_idx[MAX_FIXED_LINES+1];
int                     stmt_line_end_idx[MAX_FIXED_LINES+1];
int                     stmt_line_idx;
int                     stmt_line_num[MAX_FIXED_LINES+1];
int                     stmt_line_offset[MAX_FIXED_LINES+1];
int			stmt_prefix_len;

dir_prefix_type 	stmt_buf_dir_prefix;
dir_prefix_type 	nxt_line_dir_prefix;
				 
dir_prefix_type		line_dir_prefix[MAX_FIXED_LINES+1];
dir_prefix_type		nxt_line_actual_dir_prefix;

/* chars predefined as ints */

int      lc_a         = 'a';
int      uc_a         = 'A';
int	lc_b	     = 'b';
int	uc_b	     = 'B';
int      lc_c         = 'c';
int      uc_c         = 'C';
int      lc_d         = 'd';
int      uc_d         = 'D';
int	lc_e	     = 'e';
int	uc_e	     = 'E';
int      lc_f         = 'f';
int      uc_f         = 'F';
int      lc_g         = 'g';
int      uc_g         = 'G';
int	lc_h         = 'h';
int	uc_h         = 'H';
int      lc_i         = 'i';
int      uc_i         = 'I';
int	lc_l         = 'l';
int      uc_l         = 'L';
int      lc_m         = 'm';
int      uc_m         = 'M';
int      lc_o         = 'o';
int      uc_o         = 'O';
int      lc_p         = 'p';
int      uc_p         = 'P';
int      lc_r         = 'r';
int	uc_r         = 'R';
int      lc_s         = 's';
int      uc_s         = 'S';
int      lc_t         = 't';
int      uc_t         = 'T';
int      dot          = '.';
int	star         = '*';
int	quote        = '\'';
int      db_quote     = '"';
int      blank        = ' ';
int      marked_blank = ((1 << SIGN_BIT) | ' ');
int      tab          = '\t';
int      newline      = '\n';
int      dosnewline   = '\r';
int      bang         = '!';
int      lparen       = '(';
int      rparen       = ')';
int      lbrkt        = '[';
int      rbrkt        = ']';
int      equal        = '=';
int      slash        = '/';
int      underscore   = '_';
int      dollar       = '$';
int      amp          = '&';
int      percent      = '%';
int      at_sign      = '@';
int      semi_colon   = ';';
int      colon        = ':';
int      eos          = 0;
int      one          = '1';
int      nine         = '9';
int      zero         = '0';
int      comma        = ',';
int	pound        = '#';
int	greater      = '>';
int	less         = '<';

int      format_str[7][2] = {	{'F', 'f'},
					{'O', 'o'},
					{'R', 'r'},
					{'M', 'm'},
					{'A', 'a'},
					{'T', 't'},
					{'(', '('}};

int      format_idx = -1;
boolean  in_format = FALSE;
int      format_start_idx;

/* The following flags allow certain messages to be issued only once. */
static boolean	have_issued_msg_37;

/* The following table is used to identify the class of look ahead characters */
ch_class_type		ch_class[MAX_ASCII_CHARS] = {
				Ch_Class_EOS,			       /* NULL*/
				Ch_Class_Symbol,		       /* SOH */
				Ch_Class_Symbol,		       /* STX */
				Ch_Class_Symbol,		       /* ETX */
				Ch_Class_Symbol,		       /* EOT */
				Ch_Class_Symbol,		       /* ENQ */
				Ch_Class_Symbol,		       /* ACK */
				Ch_Class_Symbol,		       /* BEL */
				Ch_Class_Symbol,		       /* BS  */
				Ch_Class_Symbol,		       /* TAB */
				Ch_Class_Symbol,		       /* LF  */
				Ch_Class_Symbol,		       /* VT  */
				Ch_Class_Symbol,		       /* FF  */
				Ch_Class_Symbol,		       /* CR  */
				Ch_Class_Symbol,		       /* SO  */
				Ch_Class_Symbol,		       /* SI  */
				Ch_Class_Symbol,		       /* DLE */
				Ch_Class_Symbol,		       /* DC1 */
				Ch_Class_Symbol,		       /* DC2 */
				Ch_Class_Symbol,		       /* DC3 */
				Ch_Class_Symbol,		       /* DC4 */
				Ch_Class_Symbol,		       /* NAK */
				Ch_Class_Symbol,		       /* SYN */
				Ch_Class_Symbol,		       /* ETB */
				Ch_Class_Symbol,		       /* CAN */
				Ch_Class_Symbol,		       /* EM  */
				Ch_Class_Symbol,		       /* SUB */
				Ch_Class_Symbol,		       /* ESC */
				Ch_Class_Symbol,		       /* FS  */
				Ch_Class_Symbol,		       /* GS  */
				Ch_Class_Symbol,		       /* RS  */
				Ch_Class_Symbol,		       /* US  */
				Ch_Class_Symbol,		       /* SP  */
				Ch_Class_Symbol,		       /* !   */
				Ch_Class_Symbol,		       /* "   */
				Ch_Class_Symbol,		       /* #   */
				Ch_Class_Symbol,		       /* $   */
				Ch_Class_Symbol,		       /* %   */
				Ch_Class_Symbol,		       /* &   */
				Ch_Class_Symbol,		       /* '   */
				Ch_Class_Symbol,		       /* (   */
				Ch_Class_Symbol,		       /* )   */
				Ch_Class_Symbol,		       /* *   */
				Ch_Class_Symbol,		       /* +   */
				Ch_Class_Symbol,		       /* ,   */
				Ch_Class_Symbol,		       /* -   */
				Ch_Class_Symbol,		       /* .   */
				Ch_Class_Symbol,		       /* /   */
				Ch_Class_Digit,			       /* 0   */
				Ch_Class_Digit,			       /* 1   */
				Ch_Class_Digit,			       /* 2   */
				Ch_Class_Digit,			       /* 3   */
				Ch_Class_Digit,			       /* 4   */
				Ch_Class_Digit,			       /* 5   */
				Ch_Class_Digit,			       /* 6   */
				Ch_Class_Digit,			       /* 7   */
				Ch_Class_Digit,			       /* 8   */
				Ch_Class_Digit,			       /* 9   */
				Ch_Class_Symbol,		       /* :   */
				Ch_Class_Symbol,		       /* ;   */
				Ch_Class_Symbol,		       /* <   */
				Ch_Class_Symbol,		       /* =   */
				Ch_Class_Symbol,		       /* >   */
				Ch_Class_Symbol,		       /* ?   */
				Ch_Class_Symbol,		       /* @   */
				Ch_Class_Letter,		       /* A   */
				Ch_Class_Letter,		       /* B   */
				Ch_Class_Letter,		       /* C   */
				Ch_Class_Letter,		       /* D   */
				Ch_Class_Letter,		       /* E   */
				Ch_Class_Letter,		       /* F   */
				Ch_Class_Letter,		       /* G   */
				Ch_Class_Letter,		       /* H   */
				Ch_Class_Letter,		       /* I   */
				Ch_Class_Letter,		       /* J   */
				Ch_Class_Letter,		       /* K   */
				Ch_Class_Letter,		       /* L   */
				Ch_Class_Letter,		       /* M   */
				Ch_Class_Letter,		       /* N   */
				Ch_Class_Letter,		       /* O   */
				Ch_Class_Letter,		       /* P   */
				Ch_Class_Letter,		       /* Q   */
				Ch_Class_Letter,		       /* R   */
				Ch_Class_Letter,		       /* S   */
				Ch_Class_Letter,		       /* T   */
				Ch_Class_Letter,		       /* U   */
				Ch_Class_Letter,		       /* V   */
				Ch_Class_Letter,		       /* W   */
				Ch_Class_Letter,		       /* X   */
				Ch_Class_Letter,		       /* Y   */
				Ch_Class_Letter,		       /* Z   */
				Ch_Class_Symbol,		       /* [   */
				Ch_Class_Symbol,		       /* \   */
				Ch_Class_Symbol,		       /* ]   */
				Ch_Class_Symbol,		       /* ^   */
				Ch_Class_Symbol,		       /* _   */
				Ch_Class_Symbol,		       /* `   */
				Ch_Class_Letter,		       /* a   */
				Ch_Class_Letter,		       /* b   */
				Ch_Class_Letter,		       /* c   */
				Ch_Class_Letter,		       /* d   */
				Ch_Class_Letter,		       /* e   */
				Ch_Class_Letter,		       /* f   */
				Ch_Class_Letter,		       /* g   */
				Ch_Class_Letter,		       /* h   */
				Ch_Class_Letter,		       /* i   */
				Ch_Class_Letter,		       /* j   */
				Ch_Class_Letter,		       /* k   */
				Ch_Class_Letter,		       /* l   */
				Ch_Class_Letter,		       /* m   */
				Ch_Class_Letter,		       /* n   */
				Ch_Class_Letter,		       /* o   */
				Ch_Class_Letter,		       /* p   */
				Ch_Class_Letter,		       /* q   */
				Ch_Class_Letter,		       /* r   */
				Ch_Class_Letter,		       /* s   */
				Ch_Class_Letter,		       /* t   */
				Ch_Class_Letter,		       /* u   */
				Ch_Class_Letter,		       /* v   */
				Ch_Class_Letter,		       /* w   */
				Ch_Class_Letter,		       /* x   */
				Ch_Class_Letter,		       /* y   */
				Ch_Class_Letter,		       /* z   */
				Ch_Class_Symbol,		       /* {   */
				Ch_Class_Symbol,		       /* |   */
				Ch_Class_Symbol,		       /* }   */
				Ch_Class_Symbol,		       /* ~   */
				Ch_Class_Symbol,		       /* DEL */
				Ch_Class_Symbol,		       /* 128 */
				Ch_Class_Symbol,		       /* 129 */
				Ch_Class_Symbol,		       /* 130 */
				Ch_Class_Symbol,		       /* 131 */
				Ch_Class_Symbol,		       /* 132 */
				Ch_Class_Symbol,		       /* 133 */
				Ch_Class_Symbol,		       /* 134 */
				Ch_Class_Symbol,		       /* 135 */
				Ch_Class_Symbol,		       /* 136 */
				Ch_Class_Symbol,		       /* 137 */
				Ch_Class_Symbol,		       /* 138 */
				Ch_Class_Symbol,		       /* 139 */
				Ch_Class_Symbol,		       /* 140 */
				Ch_Class_Symbol,		       /* 141 */
				Ch_Class_Symbol,		       /* 142 */
				Ch_Class_Symbol,		       /* 143 */
				Ch_Class_Symbol,		       /* 144 */
				Ch_Class_Symbol,		       /* 145 */
				Ch_Class_Symbol,		       /* 146 */
				Ch_Class_Symbol,		       /* 147 */
				Ch_Class_Symbol,		       /* 148 */
				Ch_Class_Symbol,		       /* 149 */
				Ch_Class_Symbol,		       /* 150 */
				Ch_Class_Symbol,		       /* 151 */
				Ch_Class_Symbol,		       /* 152 */
				Ch_Class_Symbol,		       /* 153 */
				Ch_Class_Symbol,		       /* 154 */
				Ch_Class_Symbol,		       /* 155 */
				Ch_Class_Symbol,		       /* 156 */
				Ch_Class_Symbol,		       /* 157 */
				Ch_Class_Symbol,		       /* 158 */
				Ch_Class_Symbol,		       /* 159 */
				Ch_Class_Symbol,		       /* 160 */
				Ch_Class_Symbol,		       /* 161 */
				Ch_Class_Symbol,		       /* 162 */
				Ch_Class_Symbol,		       /* 163 */
				Ch_Class_Symbol,		       /* 164 */
				Ch_Class_Symbol,		       /* 165 */
				Ch_Class_Symbol,		       /* 166 */
				Ch_Class_Symbol,		       /* 167 */
				Ch_Class_Symbol,		       /* 168 */
				Ch_Class_Symbol,		       /* 169 */
				Ch_Class_Symbol,		       /* 170 */
				Ch_Class_Symbol,		       /* 171 */
				Ch_Class_Symbol,		       /* 172 */
				Ch_Class_Symbol,		       /* 173 */
				Ch_Class_Symbol,		       /* 174 */
				Ch_Class_Symbol,		       /* 175 */
				Ch_Class_Symbol,		       /* 176 */
				Ch_Class_Symbol,		       /* 177 */
				Ch_Class_Symbol,		       /* 178 */
				Ch_Class_Symbol,		       /* 179 */
				Ch_Class_Symbol,		       /* 180 */
				Ch_Class_Symbol,		       /* 181 */
				Ch_Class_Symbol,		       /* 182 */
				Ch_Class_Symbol,		       /* 183 */
				Ch_Class_Symbol,		       /* 184 */
				Ch_Class_Symbol,		       /* 185 */
				Ch_Class_Symbol,		       /* 186 */
				Ch_Class_Symbol,		       /* 187 */
				Ch_Class_Symbol,		       /* 188 */
				Ch_Class_Symbol,		       /* 189 */
				Ch_Class_Symbol,		       /* 190 */
				Ch_Class_Symbol,		       /* 191 */
				Ch_Class_Symbol,		       /* 192 */
				Ch_Class_Symbol,		       /* 193 */
				Ch_Class_Symbol,		       /* 194 */
				Ch_Class_Symbol,		       /* 195 */
				Ch_Class_Symbol,		       /* 196 */
				Ch_Class_Symbol,		       /* 197 */
				Ch_Class_Symbol,		       /* 198 */
				Ch_Class_Symbol,		       /* 199 */
				Ch_Class_Symbol,		       /* 200 */
				Ch_Class_Symbol,		       /* 201 */
				Ch_Class_Symbol,		       /* 202 */
				Ch_Class_Symbol,		       /* 203 */
				Ch_Class_Symbol,		       /* 204 */
				Ch_Class_Symbol,		       /* 205 */
				Ch_Class_Symbol,		       /* 206 */
				Ch_Class_Symbol,		       /* 207 */
				Ch_Class_Symbol,		       /* 208 */
				Ch_Class_Symbol,		       /* 209 */
				Ch_Class_Symbol,		       /* 210 */
				Ch_Class_Symbol,		       /* 211 */
				Ch_Class_Symbol,		       /* 212 */
				Ch_Class_Symbol,		       /* 213 */
				Ch_Class_Symbol,		       /* 214 */
				Ch_Class_Symbol,		       /* 215 */
				Ch_Class_Symbol,		       /* 216 */
				Ch_Class_Symbol,		       /* 217 */
				Ch_Class_Symbol,		       /* 218 */
				Ch_Class_Symbol,		       /* 219 */
				Ch_Class_Symbol,		       /* 220 */
				Ch_Class_Symbol,		       /* 221 */
				Ch_Class_Symbol,		       /* 222 */
				Ch_Class_Symbol,		       /* 223 */
				Ch_Class_Symbol,		       /* 224 */
				Ch_Class_Symbol,		       /* 225 */
				Ch_Class_Symbol,		       /* 226 */
				Ch_Class_Symbol,		       /* 227 */
				Ch_Class_Symbol,		       /* 228 */
				Ch_Class_Symbol,		       /* 229 */
				Ch_Class_Symbol,		       /* 230 */
				Ch_Class_Symbol,		       /* 231 */
				Ch_Class_Symbol,		       /* 232 */
				Ch_Class_Symbol,		       /* 233 */
				Ch_Class_Symbol,		       /* 234 */
				Ch_Class_Symbol,		       /* 235 */
				Ch_Class_Symbol,		       /* 236 */
				Ch_Class_Symbol,		       /* 237 */
				Ch_Class_Symbol,		       /* 238 */
				Ch_Class_Symbol,		       /* 239 */
				Ch_Class_Symbol,		       /* 240 */
				Ch_Class_Symbol,		       /* 241 */
				Ch_Class_Symbol,		       /* 242 */
				Ch_Class_Symbol,		       /* 243 */
				Ch_Class_Symbol,		       /* 244 */
				Ch_Class_Symbol,		       /* 245 */
				Ch_Class_Symbol,		       /* 246 */
				Ch_Class_Symbol,		       /* 247 */
				Ch_Class_Symbol,		       /* 248 */
				Ch_Class_Symbol,		       /* 249 */
				Ch_Class_Symbol,		       /* 250 */
				Ch_Class_Symbol,		       /* 251 */
				Ch_Class_Symbol,		       /* 252 */
				Ch_Class_Symbol,		       /* 253 */
				Ch_Class_Symbol,		       /* 254 */
				Ch_Class_EOF };			       /* 255 */

/* The following table is just to see if a character is in the "valid" */
/* Fortran 90 character set.                                           */

static	boolean		valid_f90_char[MAX_ASCII_CHARS] = {
                                FALSE,                          /* NULL*/
                                FALSE,                       /* SOH */
                                FALSE,                       /* STX */
                                FALSE,                       /* ETX */
                                FALSE,                       /* EOT */
                                FALSE,                       /* ENQ */
                                FALSE,                       /* ACK */
                                FALSE,                       /* BEL */
                                FALSE,                       /* BS  */
                                TRUE ,                       /* TAB */
                                FALSE,                       /* LF  */
                                FALSE,                       /* VT  */
                                FALSE,                       /* FF  */
                                FALSE,                       /* CR  */
                                FALSE,                       /* SO  */
                                FALSE,                       /* SI  */
                                FALSE,                       /* DLE */
                                FALSE,                       /* DC1 */
                                FALSE,                       /* DC2 */
                                FALSE,                       /* DC3 */
                                FALSE,                       /* DC4 */
                                FALSE,                       /* NAK */
                                FALSE,                       /* SYN */
                                FALSE,                       /* ETB */
                                FALSE,                       /* CAN */
                                FALSE,                       /* EM  */
                                FALSE,                       /* SUB */
                                FALSE,                       /* ESC */
                                FALSE,                       /* FS  */
                                FALSE,                       /* GS  */
                                FALSE,                       /* RS  */
                                FALSE,                       /* US  */
                                TRUE ,                       /* SP  */
                                TRUE ,                       /* !   */
                                TRUE ,                       /* "   */
                                FALSE,                       /* #   */
                                TRUE ,                       /* $   */
                                TRUE ,                       /* %   */
                                TRUE ,                       /* &   */
                                TRUE ,                       /* '   */
                                TRUE ,                       /* (   */
                                TRUE ,                       /* )   */
                                TRUE ,                       /* *   */
                                TRUE ,                       /* +   */
                                TRUE ,                       /* ,   */
                                TRUE ,                       /* -   */
                                TRUE ,                       /* .   */
                                TRUE ,                       /* /   */
                                TRUE ,                        /* 0   */
                                TRUE ,                        /* 1   */
                                TRUE ,                        /* 2   */
                                TRUE ,                        /* 3   */
                                TRUE ,                        /* 4   */
                                TRUE ,                        /* 5   */
                                TRUE ,                        /* 6   */
                                TRUE ,                        /* 7   */
                                TRUE ,                        /* 8   */
                                TRUE ,                        /* 9   */
                                TRUE ,                       /* :   */
                                TRUE ,                       /* ;   */
                                TRUE ,                       /* <   */
                                TRUE ,                       /* =   */
                                TRUE ,                       /* >   */
                                TRUE ,                       /* ?   */
                                FALSE,                       /* @   */
                                TRUE ,                       /* A   */
                                TRUE ,                       /* B   */
                                TRUE ,                       /* C   */
                                TRUE ,                       /* D   */
                                TRUE ,                       /* E   */
                                TRUE ,                       /* F   */
                                TRUE ,                       /* G   */
                                TRUE ,                       /* H   */
                                TRUE ,                       /* I   */
                                TRUE ,                       /* J   */
                                TRUE ,                       /* K   */
                                TRUE ,                       /* L   */
                                TRUE ,                       /* M   */
                                TRUE ,                       /* N   */
                                TRUE ,                       /* O   */
                                TRUE ,                       /* P   */
                                TRUE ,                       /* Q   */
                                TRUE ,                       /* R   */
                                TRUE ,                       /* S   */
                                TRUE ,                       /* T   */
                                TRUE ,                       /* U   */
                                TRUE ,                       /* V   */
                                TRUE ,                       /* W   */
                                TRUE ,                       /* X   */
                                TRUE ,                       /* Y   */
                                TRUE ,                       /* Z   */
                                FALSE,                       /* [   */
                                FALSE,                       /* \   */
                                FALSE,                       /* ]   */
                                FALSE,                       /* ^   */
                                TRUE ,                       /* _   */
                                FALSE,                       /* `   */
                                TRUE ,                       /* a   */
                                TRUE ,                       /* b   */
                                TRUE ,                       /* c   */
                                TRUE ,                       /* d   */
                                TRUE ,                       /* e   */
                                TRUE ,                       /* f   */
                                TRUE ,                       /* g   */
                                TRUE ,                       /* h   */
                                TRUE ,                       /* i   */
                                TRUE ,                       /* j   */
                                TRUE ,                       /* k   */
                                TRUE ,                       /* l   */
                                TRUE ,                       /* m   */
                                TRUE ,                       /* n   */
                                TRUE ,                       /* o   */
                                TRUE ,                       /* p   */
                                TRUE ,                       /* q   */
                                TRUE ,                       /* r   */
                                TRUE ,                       /* s   */
                                TRUE ,                       /* t   */
                                TRUE ,                       /* u   */
                                TRUE ,                       /* v   */
                                TRUE ,                       /* w   */
                                TRUE ,                       /* x   */
                                TRUE ,                       /* y   */
                                TRUE ,                       /* z   */
                                FALSE,                       /* {   */
                                FALSE,                       /* |   */
                                FALSE,                       /* }   */
                                FALSE,                       /* ~   */
                                FALSE,                       /* DEL */
                                FALSE,                       /* 128 */
                                FALSE,                       /* 129 */
                                FALSE,                       /* 130 */
                                FALSE,                       /* 131 */
                                FALSE,                       /* 132 */
                                FALSE,                       /* 133 */
                                FALSE,                       /* 134 */
                                FALSE,                       /* 135 */
                                FALSE,                       /* 136 */
                                FALSE,                       /* 137 */
                                FALSE,                       /* 138 */
                                FALSE,                       /* 139 */
                                FALSE,                       /* 140 */
                                FALSE,                       /* 141 */
                                FALSE,                       /* 142 */
                                FALSE,                       /* 143 */
                                FALSE,                       /* 144 */
                                FALSE,                       /* 145 */
                                FALSE,                       /* 146 */
                                FALSE,                       /* 147 */
                                FALSE,                       /* 148 */
                                FALSE,                       /* 149 */
                                FALSE,                       /* 150 */
                                FALSE,                       /* 151 */
                                FALSE,                       /* 152 */
                                FALSE,                       /* 153 */
                                FALSE,                       /* 154 */
                                FALSE,                       /* 155 */
                                FALSE,                       /* 156 */
                                FALSE,                       /* 157 */
                                FALSE,                       /* 158 */
                                FALSE,                       /* 159 */
                                FALSE,                       /* 160 */
                                FALSE,                       /* 161 */
                                FALSE,                       /* 162 */
                                FALSE,                       /* 163 */
                                FALSE,                       /* 164 */
                                FALSE,                       /* 165 */
                                FALSE,                       /* 166 */
                                FALSE,                       /* 167 */
                                FALSE,                       /* 168 */
                                FALSE,                       /* 169 */
                                FALSE,                       /* 170 */
                                FALSE,                       /* 171 */
                                FALSE,                       /* 172 */
                                FALSE,                       /* 173 */
                                FALSE,                       /* 174 */
                                FALSE,                       /* 175 */
                                FALSE,                       /* 176 */
                                FALSE,                       /* 177 */
                                FALSE,                       /* 178 */
                                FALSE,                       /* 179 */
                                FALSE,                       /* 180 */
                                FALSE,                       /* 181 */
                                FALSE,                       /* 182 */
                                FALSE,                       /* 183 */
                                FALSE,                       /* 184 */
                                FALSE,                       /* 185 */
                                FALSE,                       /* 186 */
                                FALSE,                       /* 187 */
                                FALSE,                       /* 188 */
                                FALSE,                       /* 189 */
                                FALSE,                       /* 190 */
                                FALSE,                       /* 191 */
                                FALSE,                       /* 192 */
                                FALSE,                       /* 193 */
                                FALSE,                       /* 194 */
                                FALSE,                       /* 195 */
                                FALSE,                       /* 196 */
                                FALSE,                       /* 197 */
                                FALSE,                       /* 198 */
                                FALSE,                       /* 199 */
                                FALSE,                       /* 200 */
                                FALSE,                       /* 201 */
                                FALSE,                       /* 202 */
                                FALSE,                       /* 203 */
                                FALSE,                       /* 204 */
                                FALSE,                       /* 205 */
                                FALSE,                       /* 206 */
                                FALSE,                       /* 207 */
                                FALSE,                       /* 208 */
                                FALSE,                       /* 209 */
                                FALSE,                       /* 210 */
                                FALSE,                       /* 211 */
                                FALSE,                       /* 212 */
                                FALSE,                       /* 213 */
                                FALSE,                       /* 214 */
                                FALSE,                       /* 215 */
                                FALSE,                       /* 216 */
                                FALSE,                       /* 217 */
                                FALSE,                       /* 218 */
                                FALSE,                       /* 219 */
                                FALSE,                       /* 220 */
                                FALSE,                       /* 221 */
                                FALSE,                       /* 222 */
                                FALSE,                       /* 223 */
                                FALSE,                       /* 224 */
                                FALSE,                       /* 225 */
                                FALSE,                       /* 226 */
                                FALSE,                       /* 227 */
                                FALSE,                       /* 228 */
                                FALSE,                       /* 229 */
                                FALSE,                       /* 230 */
                                FALSE,                       /* 231 */
                                FALSE,                       /* 232 */
                                FALSE,                       /* 233 */
                                FALSE,                       /* 234 */
                                FALSE,                       /* 235 */
                                FALSE,                       /* 236 */
                                FALSE,                       /* 237 */
                                FALSE,                       /* 238 */
                                FALSE,                       /* 239 */
                                FALSE,                       /* 240 */
                                FALSE,                       /* 241 */
                                FALSE,                       /* 242 */
                                FALSE,                       /* 243 */
                                FALSE,                       /* 244 */
                                FALSE,                       /* 245 */
                                FALSE,                       /* 246 */
                                FALSE,                       /* 247 */
                                FALSE,                       /* 248 */
                                FALSE,                       /* 249 */
                                FALSE,                       /* 250 */
                                FALSE,                       /* 251 */
                                FALSE,                       /* 252 */
                                FALSE,                       /* 253 */
                                FALSE,                       /* 254 */
                                FALSE };                        /* 255 */


# ifdef _DEBUG
   static char *line_type_str[] = {
			"Comment_Line",			/* Comment_Line	      */
			"Include_Line",			/* Include_Line	      */
			"Cond_Comp_Line",               /* Cond_Comp_Line     */
			"Dir_Line",			/* Dir_Line	      */
			"Regular_Line",			/* Regular_Line	      */
			"Continuation_Line",		/* Continuation_Line  */
			"EOF_Line"			/* EOF_Line	      */
		};

   static char *ch_str[] = {
			"EOS", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
			"BS",  "TAB", "LF",  "VT",  "FF",  "CR",  "SO",	 "SI",
			"DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
			"CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",	 "US",
			"SP",  "!",   "\"",  "#",   "$",   "%",	  "&",	 "'",
			"(",   ")",   "*",   "+",   ",",   "-",	  ".",	 "/",
			"0",   "1",   "2",   "3",   "4",   "5",	  "6",	 "7",
			"8",   "9",   ":",   ";",   "<",   "=",	  ">",	 "?",
			"@",   "A",   "B",   "C",   "D",   "E",	  "F",	 "G",
			"H",   "I",   "J",   "K",   "L",   "M",	  "N",	 "O",
			"P",   "Q",   "R",   "S",   "T",   "U",	  "V",	 "W",
			"X",   "Y",   "Z",   "[",   "\\",  "]",	  "^",	 "_",	
			"`",   "a",   "b",   "c",   "d",   "e",	  "f",	 "g",	
			"h",   "i",   "j",   "k",   "l",   "m",	  "n",	 "o",	
			"p",   "q",   "r",   "s",   "t",   "u",	  "v",	 "w",
			"x",   "y",   "z",   "{",   "|",   "}",	  "~",	 "DEL",
			"128", "129", "130", "131", "132", "133", "134", "135",
			"136", "137", "138", "139", "140", "141", "142", "143",
			"144", "145", "146", "147", "148", "149", "150", "151",
			"152", "153", "154", "155", "156", "157", "158", "159",
			"160", "161", "162", "163", "164", "165", "166", "167",
			"168", "169", "170", "171", "172", "173", "174", "175",
			"176", "177", "178", "179", "180", "181", "182", "183",
			"184", "185", "186", "187", "188", "189", "190", "191",
			"192", "193", "194", "195", "196", "197", "198", "199",
			"200", "201", "202", "203", "204", "205", "206", "207",
			"208", "209", "210", "211", "212", "213", "214", "215",
			"216", "217", "218", "219", "220", "221", "222", "223",
			"224", "225", "226", "227", "228", "229", "230", "231",
			"232", "233", "234", "235", "236", "237", "238", "239",
			"240", "241", "242", "243", "244", "245", "246", "247",
			"248", "249", "250", "251", "252", "253", "254", "EOF"
		};
# endif

/*************************************\
|* objects referenced by this module *|
\*************************************/
extern boolean	sig_blank;
extern la_type  stmt_EOS_la_ch;

static boolean havent_issued_tab_ansi;
static boolean havent_issued_at_ansi;
static boolean havent_issued_dollar_ansi;

extern boolean parse_cc_line(void);
extern boolean is_par_directive (int);
extern boolean is_dollar_directive (int);
extern boolean is_star_directive (int);

extern boolean scan_fortran_stmt(void);
extern boolean scan_fixed_comment(void);

extern void	check_for_continued_macro(void);

FILE	*dot_i_fptr;

boolean	angle_brkt_include = FALSE;

/* save variables */
int	save_prev_char_delim;
int	save_prev_char_delim_idx;
int	save_previous_char;
boolean	save_seen_lp_eq_slash;
int	save_char_delim;
int	save_digit_start;
int	save_format_idx;
boolean	save_in_format;
line_type	save_expected_line;
boolean	save_first_line;

boolean	issue_pound_exit_line = FALSE;

extern	char                    cc_stmt_buf[MAX_STMT_CHAR_SIZE];
extern	int			cc_stmt_buf_idx;
extern	cc_stmt_buf_line_type   cc_stmt_buf_line[200];
extern	long                    cc_stmt_buf_num_lines;

long	previous_global_line;
