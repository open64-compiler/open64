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



/* USMID:  "\n@(#)5.0_pl/headers/s_cnstrct.h	5.3	06/11/99 08:54:22\n" */

long64			words_in_constructor;
long64			bits_in_constructor;
long_type		result_value[MAX_WORDS_FOR_NUMERIC];
char			*char_result_buffer = NULL;
long64			char_result_len = 0;
long64			char_result_buffer_len = 0;
long64			char_result_offset = 0;
long64			the_cn_bit_offset;
int			the_cn_idx;
long64			bcast_cn_bit_offset;
boolean			in_array_constructor = FALSE;
boolean			no_result_value = FALSE;
boolean			unequal_char_lens;
boolean			do_constructor_init = TRUE;
boolean			single_value_array;
opnd_type		single_value_opnd;

struct	save_env_entry	{
			long64               	words_in_constructor;
			long64               	bits_in_constructor;
			long64               	char_result_len;
			long64                  char_result_offset;
			long64 	              	the_cn_bit_offset;
			int                     the_cn_idx;
			long64               	bcast_cn_bit_offset;
			boolean                 no_result_value;
			boolean                 unequal_char_lens;
			boolean                 do_constructor_init;
			boolean			check_type_conversion;
			int			target_type_idx;
			int   	                target_char_len_idx;
			opnd_type		init_target_opnd;
			boolean			single_value_array;
			opnd_type		single_value_opnd;
			};

typedef struct	save_env_entry save_env_type;


extern exp_tbl_type    bin_add_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    bin_sub_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    mult_div_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    power_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    eq_ne_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    gt_lt_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    and_or_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    asg_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    un_plus_tbl[Num_Linear_Types];
extern exp_tbl_type    not_tbl[Num_Linear_Types];
