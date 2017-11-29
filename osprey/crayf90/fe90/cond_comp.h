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



/* USMID:  "\n@(#)5.0_pl/headers/cond_comp.h	5.2	04/30/99 09:41:17\n" */

enum	cc_token_values {
			Cc_Tok_Unknown,

			Cc_Tok_Id,
			Cc_Tok_Constant,

			Cc_Tok_Kwd_Define,
			Cc_Tok_Kwd_Defined,
			Cc_Tok_Kwd_Elif,
			Cc_Tok_Kwd_Else,
			Cc_Tok_Kwd_Endif,
			Cc_Tok_Kwd_Error,
			Cc_Tok_Kwd_If,
			Cc_Tok_Kwd_Ifdef,
			Cc_Tok_Kwd_Ifndef,
			Cc_Tok_Kwd_Include,
			Cc_Tok_Kwd_Undef,

			Cc_Tok_Op_Add,		/* + */
			Cc_Tok_Op_Div,		/* / */
			Cc_Tok_Op_Mult,		/* * */
			Cc_Tok_Op_Sub,		/* - */
			Cc_Tok_Op_Eq,		/* == */
			Cc_Tok_Op_Ge,		/* >= */
			Cc_Tok_Op_Gt,		/* > */
			Cc_Tok_Op_Le,		/* <= */
			Cc_Tok_Op_Lt,		/* < */
			Cc_Tok_Op_Ne,		/* != */
			Cc_Tok_Op_And,		/* && */
			Cc_Tok_Op_Band,		/* &  */
			Cc_Tok_Op_Neqv,		/* ^ */
			Cc_Tok_Op_Not,		/* ! */
			Cc_Tok_Op_Or,		/* || */
			Cc_Tok_Op_Bor,		/* | */
			Cc_Tok_Op_Mod,		/* % */
			Cc_Tok_Op_Lshift,	/* << */
			Cc_Tok_Op_Rshift,	/* >> */
			Cc_Tok_Op_Bnot,		/* ~ */

			Cc_Tok_EOS
			};

typedef enum    cc_token_values cc_token_values_type;

			
typedef union   cc_id_str_entry                    cc_id_str_type;

union   cc_id_str_entry            {char           string[CC_MAX_ID_LEN+1];
                                    long           words[CC_NUM_ID_WDS];
                                };

cc_id_str_type	temp_id_str;

typedef struct  cc_darg_entry	cc_darg_type;

struct	cc_darg_entry	{
			cc_id_str_type		name;
			int			name_len;
			cc_darg_type		*next;
			};

typedef struct  cc_arg_entry   cc_arg_type;

struct  cc_arg_entry   {
                        char          	       *name;
                        int                     name_len;
                        cc_arg_type            *next;
                        };



cc_stmt_buf_line_type		cc_stmt_buf_line[200];
long				cc_stmt_buf_num_lines;

struct  cc_token_entry  {cc_id_str_type         token_str;
                         int                    token_len;
                         boolean                token_err;
                         cc_token_values_type   value;
                         char                   kind_str[MAX_ID_LEN+1];
                         int                    kind_len;
                         int                    line;
                         int                    column;
                        };

typedef struct cc_token_entry cc_token_type;

struct	cc_attr_entry  {Uint			name_idx	: 24;
			Uint			name_len	:  7;
			boolean			defined		:  1;
                        Uint			str_idx		: 24;
			Uint			num_args	:  8;
			Uint			str_len		: 24;
			Uint			unused		:  7;
                        boolean			dynamic_predef	:  1;
			Uint			start_line	: 24;
			Uint			start_col	:  8;
		       };

typedef	struct	cc_attr_entry	cc_attr_tbl_type;

cc_attr_tbl_type       *cc_attr_tbl;
int                     cc_attr_tbl_idx                    = NULL_IDX;
int                     cc_attr_tbl_inc                    = 100;
int                     cc_attr_tbl_init_size              = 100;
int                     cc_attr_tbl_limit                  = (1 << 20) - 1;
int                     cc_attr_tbl_num_wds                = CC_NUM_AT_WDS;
int                     cc_attr_tbl_size                   = 0;
int                     cc_attr_tbl_largest_idx            = NULL_IDX;


typedef	struct	name_tbl_entry		cc_ln_tbl_type;

cc_ln_tbl_type         *cc_ln_tbl;
int                     cc_ln_tbl_idx                = NULL_IDX;
int                     cc_ln_tbl_inc                = 100;
int                     cc_ln_tbl_init_size          = 100;
int                     cc_ln_tbl_limit              = (1 << 20) - 1;
# ifdef _HOST32
int                     cc_ln_tbl_num_wds            = 2;
# else
int                     cc_ln_tbl_num_wds            = 1;
# endif
int                     cc_ln_tbl_size               = 0;
int                     cc_ln_tbl_largest_idx        = NULL_IDX;

int			cc_ln_fw_idx;
int			cc_ln_lw_idx;

enum	cc_blk_values	       {Cc_No_Blk,
				Cc_Elif_Blk,
				Cc_Else_Blk,
				Cc_If_Blk,
				Cc_Ifdef_Blk,
				Cc_Ifndef_Blk};

typedef enum cc_blk_values	cc_blk_type;
				
struct	cc_blk_stk_entry	{
# ifdef _HOST64
				Uint		unused2		: 32;
# endif
                                Uint		unused		: 21;
				boolean		is_active	:  1;
				boolean		already_done	:  1;
				boolean		in_error	:  1;
				Uint		blk_type	:  8;
				};

typedef	struct	cc_blk_stk_entry	cc_blk_stk_tbl_type;

cc_blk_stk_tbl_type    *cc_blk_stk_tbl;
int                     cc_blk_stk_tbl_idx                = NULL_IDX;
int                     cc_blk_stk_tbl_inc                = 10;
int                     cc_blk_stk_tbl_init_size          = 10;
int                     cc_blk_stk_tbl_limit              = (1 << 20) - 1;
int                     cc_blk_stk_tbl_num_wds            = 1;
int                     cc_blk_stk_tbl_size               = 0;
int                     cc_blk_stk_tbl_largest_idx        = NULL_IDX;
int			cc_blk_stk_top			  = NULL_IDX;

static	cc_token_type		cc_token;
static	cc_token_type		cc_initial_token;
static	la_type			cc_la_ch;

char			cc_stmt_buf[MAX_STMT_CHAR_SIZE];
int			cc_stmt_buf_idx;
int			cc_stmt_buf_len;
int			prev_idx;

# ifdef _FRONTEND_CONDITIONAL_COMP
extern	int                     nxt_line[MAX_STMT_CHAR_SIZE];
extern	int                     nxt_line_col[MAX_STMT_CHAR_SIZE];
# else
extern	int                     nxt_line[MAX_SRC_LINE_SIZE];
extern	int                     nxt_line_col[MAX_SRC_LINE_SIZE];
# endif

extern	int                     nxt_line_start_idx[MAX_FIXED_LINES+1];
extern	int                     nxt_line_end_idx[MAX_FIXED_LINES+1];
extern	int                     pp_nxt_line_length[MAX_FIXED_LINES+1];
extern	int                     pp_nxt_line_idx[MAX_FIXED_LINES+1];
extern	int                     pp_nxt_line_num[MAX_FIXED_LINES+1];
extern	line_type               pp_nxt_line_type[MAX_FIXED_LINES+1];
extern	int                     pp_nxt_line_EOL[MAX_FIXED_LINES+1];
extern	boolean                 pp_nxt_line_mp_line[MAX_FIXED_LINES+1];
extern	int                     pp_orig_line_size[MAX_FIXED_LINES+1];
extern	boolean			pp_change_source_form[MAX_FIXED_LINES+1];
extern	line_type		pp_expected_line[MAX_FIXED_LINES+1];

extern	int			nxt_line_num_lines;
extern	int			pp_line_idx;
extern	int			extra_nxt_line;

extern	char                    include_file[MAX_FILE_NAME_SIZE];

extern  boolean			ignore_source_line;
extern	int                     line_size;
extern	int                     orig_line_size;

extern	ch_class_type           ch_class[];


/* chars predefined as ints */

extern	int	lc_a;
extern	int	uc_a;
extern	int	lc_b;
extern	int	uc_b;
extern	int	lc_c;
extern	int	uc_c;
extern	int	lc_d;
extern	int	uc_d;
extern	int	lc_e;
extern	int	uc_e;
extern	int	lc_f;
extern	int	uc_f;
extern	int	lc_g;
extern	int	uc_g;
extern	int	lc_h;
extern	int	uc_h;
extern	int	lc_i;
extern	int	uc_i;
extern	int	lc_l;
extern	int	uc_l;
extern	int	lc_m;
extern	int	uc_m;
extern	int	lc_o;
extern	int	uc_o;
extern	int	lc_r;
extern	int	uc_r;
extern	int	lc_t;
extern	int	uc_t;
extern	int	dot;
extern	int	star;
extern	int	quote;
extern	int	db_quote;
extern	int	blank;
extern	int	marked_blank;
extern	int	tab;
extern	int	newline;
extern	int	bang;
extern	int	lparen;
extern	int	rparen;
extern	int	equal;
extern	int	slash;
extern	int	underscore;
extern	int	dollar;
extern	int	amp;
extern	int	percent;
extern	int	at_sign;
extern	int	semi_colon;
extern	int	colon;
extern	int	eos;
extern	int	one;
extern	int	nine;
extern	int	zero;
extern	int	comma;
extern	int	pound;
extern	int	greater;
extern	int	less;

extern	int	lbrkt;
extern	int	rbrkt;

extern void read_line (boolean);
extern void ntr_next_msg_queue(int,int,msg_severities_type,
                                   int,char *, long,int);

extern	void get_curr_file_name(char *);

extern FILE	*dot_i_fptr;

extern boolean	angle_brkt_include;

static boolean	cc_line_continued;

int	end_stmt_line_idx;

int	cc_curr_glb_line;


/****************************************\
|* these are dynamic predefined macros. *|
\****************************************/

int	line_macro_idx;
int	LINE_macro_idx;
int	file_macro_idx;
int	FILE_macro_idx;
int	date_macro_idx;
int	DATE_macro_idx;
int	time_macro_idx;
int	TIME_macro_idx;
