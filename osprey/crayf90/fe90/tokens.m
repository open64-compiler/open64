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


/* USMID:  "\n@(#)5.0_pl/macros/tokens.m	5.1	04/29/99 21:22:31\n" */
 

/*****************\
|* MISCELLANEOUS *|
\*****************/


/********************\
|* SIZES AND LIMITS *|
\********************/


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

# define  LA_CH_CLASS			la_ch.ch_class
# define  LA_CH_COLUMN			la_ch.column
# define  LA_CH_LINE			la_ch.line
# define  LA_CH_VALUE			la_ch.value
# define  LA_CH_BUF_IDX			la_ch.stmt_buf_idx
# define  LA_CH_STMT_NUM		la_ch.stmt_num

# define  TOKEN_COLUMN(TOK)		(TOK).column
# define  TOKEN_CONST_TBL_IDX(TOK)	TOKEN_STR_WD(TOK, 0)
# define  TOKEN_KIND_LEN(TOK)		(TOK).kind_len
# define  TOKEN_KIND_STR(TOK)		(TOK).kind_str
# define  TOKEN_LEN(TOK)		(TOK).token_len
# define  TOKEN_LINE(TOK)		(TOK).line
# define  TOKEN_STR(TOK)		(TOK).token_str.string
# define  TOKEN_STR_WD(TOK, WD)		(TOK).token_str.words[WD]
# define  TOKEN_VALUE(TOK)		(TOK).value
# define  TOKEN_ID(TOK)			(TOK).token_str
# define  TOKEN_ERR(TOK)		(TOK).token_err
# define  TOKEN_BUF_IDX(TOK)		(TOK).stmt_buf_idx
# define  TOKEN_STMT_NUM(TOK)		(TOK).stmt_num


/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/


/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/
# define LA_CH_TO_ERR_STR(STR,THE_LA)                                          \
         STR =  (THE_LA.ch_class == Ch_Class_EOS) ? "EOS" : &THE_LA.value
