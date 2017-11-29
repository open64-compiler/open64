/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/p_io.h	5.1	04/29/99 21:22:31\n" */
 

/***********************************************\
|* Static lookup table for format msg numbers. *|
\***********************************************/

struct	msg_num_entry	{Uint			msg_num		: 24;
			 Uint			num_args	:  8;	
			 msg_severities_type	msg_severity	: 32;
			};

typedef	struct	msg_num_entry	msg_num_type;

msg_num_type	msg_num_tbl[43] = {
		{0,	0,	Error},	  /* END_OF_MESSAGES		 0 */
		{166,	0,	Error},	  /* TRAILING_CHARS		 1 */
		{1159,	0,	Internal},/*                             2 */
		{1159,	0,	Internal},/*                             3 */
		{1159,	0,	Internal},/*                             4 */
		{1159,	0,	Internal},/*                             5 */
		{1159,	0,	Internal},/*                             6 */
		{1159,	0,	Internal},/*                             7 */
		{1159,	0,	Internal},/*                             8 */
		{1159,	0,	Internal},/*                             9 */
		{168,	0,	Ansi},	  /* ANSI_EMPTY_PAREN_MSG  	10 */
		{169,	0,	Ansi},	  /* ANSI_COMMA_REQ		11 */
		{170,	0,	Ansi},	  /* COMMA_NON_ANSI		12 */
		{0,	0,	Ansi},	  /* REP_SLASH_NON_ANSI		13 */
		{200,	1,	Ansi},	  /* NON_ANSI_EDIT_DESCRIPTOR	14 */
		{266,	0,	Ansi},	  /* MINUS_X_NON_ANSI		15 */
		{1194,	0,	Ansi},	  /* E_WITH_D_NON_ANSI          16 */
		{892,	0,	Ansi},	  /* H_IS_OBSOLETE_IN_F90	17 */
		{1160,	0,	Ansi},	  /* NON_ANSI_NULL_DESCRIPTOR	18 */
		{1628,	1,	Ansi},    /* ZERO_WIDTH_NON_ANSI        19 */
		{1626,	1,	Ansi},    /* MISSING_WIDTH_NON_ANSI     20 */
		{1159,	0,	Internal},/*                            21 */
		{1159,	0,	Internal},/*                            22 */
		{1159,	0,	Internal},/*                            23 */
		{1159,	0,	Internal},/*                            24 */
		{1159,	0,	Internal},/*                            25 */
		{1159,	0,	Internal},/*                            26 */
		{1159,	0,	Internal},/*                            27 */
		{1159,	0,	Internal},/*                            28 */
		{1159,	0,	Internal},/*                            29 */
		{304,	0,	Error},	  /* EXPECTING_LEFT_PAREN	30 */
		{305,	0,	Error},	  /* EXPECTING_RIGHT_PAREN	31 */
		{306,	1,	Error},	  /* EXPECTING_INTEGER		32 */
		{307,	1,	Error},	  /* EXPECTING_PERIOD		33 */
		{308,	1,	Error},	  /* EXPECTING_P_OR_X		34 */
		{309,	1,	Error},	  /* INVALID_REP_COUNT		35 */
		{310,	0,	Error},	  /* ZERO_REP_COUNT		36 */
		{311,	0,	Error},	  /* FIELD_WIDTH_ZERO		37 */
		{312,	0,	Error},	  /* FIELD_TO_LARGE		38 */
		{313,	0,	Error},	  /* ZERO_OR_NO_HOLLERITH_CNT	39 */
		{314,	1,	Error},	  /* UNKNOWN_EDIT_DESCRIPTOR	40 */
		{315,	0,	Error},	  /* NONTERMINATED_LITERAL	41 */
		{14,	0,	Limit}    /* UNABLE_TO_MALLOC_MEMORY	42 */
                };


static int	format_cn_idx;
static boolean  ignore_trailing_chars = FALSE;


/****************************************************************\
|* Interface table for IO stmt control list arguments.          *|
\****************************************************************/

char io_stmt_str[11][16] = {	{"BACKSPACE"},
					{"CLOSE"},
					{"ENDFILE"},
					{"INQUIRE"},
					{"OPEN"},
					{"READ"},
					{"REWIND"},
					{"WRITE"},
                                        {"PRINT"},
					{"DECODE"},
					{"ENCODE"}
				};

int arg_idx_tbl[8][26] = {
                /*BACKSPACE   */        {  0,  2,  1,  0},
                /*CLOSE       */        {  0,  3,  1,  0,  2},
                /*ENDFILE     */        {  0,  2,  1,  0},
                /*INQUIRE     */        {  0, 23,  7, 10,  5,  
                                           6, 15, 14, 12, 11,  
                                           0, 21,  4,  8,  9, 
                                          22, 20, 13,  2,  17,
                                           1, 18, 24, 19,  3,
                                          16},
                /*OPEN        */        {  0, 12,  7,  4,  5,
                                          11,  0,  6, 10,  2,
                                           9,  1,  3,  8},
                /*READ        */        {  0,  9,  4,  3,  1,
                                           5,  7,  0,  8,  2},
                /*REWIND      */        {  0,  2,  1,  0},
                /*WRITE       */        {  0,  9,  4,  3,  1,
                                           5,  7,  0,  8,  2}
                                };


ciitem_tbl_type	ciitem_tbl[NUM_IO_STMT_TYPES] = 

				{
/* Backspace */
        /* num_ciitems */		{	3,

	/* num_diff_ciitems */			3,

        /* num_without_kwd  */			1,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 3,
		/* allowed_types  */			{Integer,
							 Typeless,
							 Character},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					},

/* Close */
        /* num_ciitems */		{	4,

	/* num_diff_ciitems */			4,

        /* num_without_kwd  */			1,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */		 	{Typeless},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"STATUS",
		/* name_length    */		 6,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 4,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 2,
		/* const_opts     */			{"DELETE",
							 "KEEP"
							}
						},

	/* ciitem_entry # 3 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 2,
		/* allowed_types  */			{Integer,
							 Typeless},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					},

/* Endfile */
        /* num_ciitems */		{	3,

	/* num_diff_ciitems */			3,

        /* num_without_kwd  */			1,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 3,
		/* allowed_types  */			{Integer,
							 Typeless,
							 Character},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					},

/* Inquire */
        /* num_ciitems */		{	25,

	/* num_diff_ciitems */			25,

        /* num_without_kwd  */			1,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ACCESS",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 10,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"ACTION",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 20,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"BLANK",
		/* name_length    */		 5,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 18,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 3 */
		/* name           */		{"DELIM",
		/* name_length    */		 5,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 24,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 4 */
		/* name           */		{"DIRECT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 12,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 5 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 4,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 6 */
		/* name           */		{"EXIST",
		/* name_length    */		 5,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Logical},
		/* arg_position   */		 5,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 7 */
		/* name           */		{"FILE",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 8 */
		/* name           */		{"FORM",
		/* name_length    */		 4,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 13,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 9 */
		/* name           */		{"FORMATTED",
		/* name_length    */		 9,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 14,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #10 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #11 */
		/* name           */		{"NAME",
		/* name_length    */		 4,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 9,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #12 */
		/* name           */		{"NAMED",
		/* name_length    */		 5,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Logical},
		/* arg_position   */		 8,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #13 */
		/* name           */		{"NEXTREC",
		/* name_length    */		 7,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 17,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #14 */
		/* name           */		{"NUMBER",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 7,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #15 */
		/* name           */		{"OPENED",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Logical},
		/* arg_position   */		 6,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #16 */
		/* name           */		{"PAD",
		/* name_length    */		 3,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 25,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #17 */
		/* name           */		{"POSITION",
		/* name_length    */		 8,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 19,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #18 */
		/* name           */		{"READ",
		/* name_length    */		 4,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 21,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #19 */
		/* name           */		{"READWRITE",
		/* name_length    */		 9,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 23,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #20 */
		/* name           */		{"RECL",
		/* name_length    */		 4,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 16,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #21 */
		/* name           */		{"SEQUENTIAL",
		/* name_length    */		 10,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 11,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #22 */
		/* name           */		{"UNFORMATTED",
		/* name_length    */		 11,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 15,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #23 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 2,
		/* allowed_types  */			{Integer,
							 Typeless},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #24 */
		/* name           */		{"WRITE",
		/* name_length    */		 5,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 22,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					},

/* Open */
        /* num_ciitems */		{	13,

	/* num_diff_ciitems */			13,

        /* num_without_kwd  */			1,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ACCESS",
		/* name_length    */		 6,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 6,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 3,
		/* const_opts     */			{"SEQUENTIAL",
							 "DIRECT",
							 "APPEND"}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"ACTION",
		/* name_length    */		 6,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 11,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 3,
		/* const_opts     */			{"READ",
							 "READWRITE",
							 "WRITE"}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"BLANK",
		/* name_length    */		 5,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 9,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 2,
		/* const_opts     */			{"NULL",
							 "ZERO"}
						},

	/* ciitem_entry # 3 */
		/* name           */		{"DELIM",
		/* name_length    */		 5,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 12,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 3,
		/* const_opts     */			{"APOSTROPHE",
							 "QUOTE",
							 "NONE"}
						},

	/* ciitem_entry # 4 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 5 */
		/* name           */		{"FILE",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 4,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 6 */
		/* name           */		{"FORM",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 7,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 4,
		/* const_opts     */			{"FORMATTED",
							 "UNFORMATTED",
                                                         "SYSTEM",
                                                         "BINARY"}
						},

	/* ciitem_entry # 7 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 8 */
		/* name           */		{"PAD",
		/* name_length    */		 3,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 13,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 2,
		/* const_opts     */			{"YES",
							 "NO"}
						},

	/* ciitem_entry # 9 */
		/* name           */		{"POSITION",
		/* name_length    */		 8,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 10,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 3,
		/* const_opts     */			{"ASIS",
							 "REWIND",
							 "APPEND"}
						},

	/* ciitem_entry #10 */
		/* name           */		{"RECL",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 2,
		/* allowed_types  */			{Integer,
                                                         Typeless},
		/* arg_position   */		 8,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry #11 */
		/* name           */		{"STATUS",
		/* name_length    */		 6,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 5,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 5,
		/* const_opts     */			{"OLD",
							 "NEW",
							 "SCRATCH",
							 "REPLACE",
							 "UNKNOWN"}
						},

	/* ciitem_entry #12 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 2,
		/* allowed_types  */			{Integer,
							 Typeless},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					},

/* Read */
        /* num_ciitems */		{	9,

	/* num_diff_ciitems */			10,

        /* num_without_kwd  */			2,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ADVANCE",
		/* name_length    */		 7,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 7,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 2,
		/* const_opts     */			{"YES",
							 "NO"}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"END",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 4,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"EOR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 9,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 3 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 4 */
		/* name           */		{"FMT",
		/* name_length    */		 3,
		/* allowed_forms  */		 Format_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 5 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 5,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 6 */
		/* name           */		{"NML",
		/* name_length    */		 3,
		/* allowed_forms  */		 Namelist_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 7 */
		/* name           */		{"REC",
		/* name_length    */		 3,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 2,
		/* allowed_types  */			{Integer,
							 Typeless},
		/* arg_position   */		 6,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 8 */
		/* name           */		{"SIZE",
		/* name_length    */		 4,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 8,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 9 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 3,
		/* allowed_types  */			{Integer,
							 Typeless,
							 Character},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					},

/* Rewind */
        /* num_ciitems */		{	3,

	/* num_diff_ciitems */			3,

        /* num_without_kwd  */			1,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 3,
		/* allowed_types  */			{Integer,
							 Typeless,
							 Character},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					},

/* Write */
        /* num_ciitems */		{	9,

	/* num_diff_ciitems */			10,

        /* num_without_kwd  */			2,

					    {
	/* ciitem_entry # 0 */
		/* name           */		{"ADVANCE",
		/* name_length    */		 7,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 7,
		/* has_const_opts */		 TRUE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 2,
		/* const_opts     */			{"YES",
							 "NO"}
						},

	/* ciitem_entry # 1 */
		/* name           */		{"END",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 4,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 2 */
		/* name           */		{"EOR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 9,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 3 */
		/* name           */		{"ERR",
		/* name_length    */		 3,
		/* allowed_forms  */		 Label_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 3,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 4 */
		/* name           */		{"FMT",
		/* name_length    */		 3,
		/* allowed_forms  */		 Format_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Character},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 5 */
		/* name           */		{"IOSTAT",
		/* name_length    */		 6,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 5,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 6 */
		/* name           */		{"NML",
		/* name_length    */		 3,
		/* allowed_forms  */		 Namelist_Form,
		/* num_types      */		 0,
		/* allowed_types  */			{Typeless},
		/* arg_position   */		 2,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 7 */
		/* name           */		{"REC",
		/* name_length    */		 3,
		/* allowed_forms  */		 Exp_Form,
		/* num_types      */		 2,
		/* allowed_types  */			{Integer,
							 Typeless},
		/* arg_position   */		 6,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 8 */
		/* name           */		{"SIZE",
		/* name_length    */		 4,
		/* allowed_forms  */		 Var_Only_Form,
		/* num_types      */		 1,
		/* allowed_types  */			{Integer},
		/* arg_position   */		 8,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 TRUE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						},

	/* ciitem_entry # 9 */
		/* name           */		{"UNIT",
		/* name_length    */		 4,
                /* allowed_forms  */		 Exp_Form,
		/* num_types      */		 3,
		/* allowed_types  */			{Integer,
							 Typeless,
							 Character},
		/* arg_position   */		 1,
		/* has_const_opts */		 FALSE,
		/* scalar         */		 FALSE,
		/* num_const_opts */		 0,
		/* const_opts     */			{""}
						}
					    }
					}
				};

/**************************************************************************\
|* extern to expr_semantics flags.                                        *|
\**************************************************************************/

extern boolean	namelist_illegal;

/**************************************************************************\
|* Variables global to s_io.c and p_io.c.                                 *|
\**************************************************************************/

extern boolean  is_namelist;
extern int	imp_do_var_list;
