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


/* USMID:  "\n@(#)5.0_pl/macros/s_cnstrct.m	5.3	06/11/99 08:54:22\n" */


/*****************\
|* MISCELLANEOUS *|
\*****************/


/********************\
|* SIZES AND LIMITS *|
\********************/

# define CONSTRUCTOR_GUESS_SIZE		1000


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/


# define	LCV_CONST(ATTR, IDX)	attr_tbl[ATTR].wd[IDX]


/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

# define SAVE_ENV							\
	save.words_in_constructor	= words_in_constructor;		\
	save.bits_in_constructor	= bits_in_constructor;		\
	save.char_result_len		= char_result_len;		\
	save.char_result_offset		= char_result_offset;		\
	save.the_cn_bit_offset		= the_cn_bit_offset;		\
	save.the_cn_idx			= the_cn_idx;			\
	save.bcast_cn_bit_offset	= bcast_cn_bit_offset;		\
	save.no_result_value		= no_result_value;		\
	save.unequal_char_lens		= unequal_char_lens;		\
	save.do_constructor_init	= do_constructor_init;		\
	save.check_type_conversion	= check_type_conversion;	\
	save.target_type_idx   		= target_type_idx;   		\
	save.target_char_len_idx	= target_char_len_idx;		\
	save.single_value_array		= single_value_array;		\
	save.single_value_opnd		= single_value_opnd;		\
        COPY_OPND(save.init_target_opnd, init_target_opnd);

# define RESTORE_ENV							\
	words_in_constructor		= save.words_in_constructor;	\
	bits_in_constructor		= save.bits_in_constructor;	\
	char_result_len			= save.char_result_len;		\
	char_result_offset		= save.char_result_offset;	\
	the_cn_bit_offset		= save.the_cn_bit_offset;	\
	the_cn_idx			= save.the_cn_idx;		\
	bcast_cn_bit_offset		= save.bcast_cn_bit_offset;	\
	no_result_value			= save.no_result_value;		\
	unequal_char_lens		= save.unequal_char_lens;	\
	do_constructor_init		= save.do_constructor_init;	\
	check_type_conversion		= save.check_type_conversion;	\
	target_type_idx			= save.target_type_idx;		\
	target_char_len_idx		= save.target_char_len_idx;	\
	single_value_array		= save.single_value_array;	\
	single_value_opnd		= save.single_value_opnd;	\
        COPY_OPND(init_target_opnd, save.init_target_opnd);
