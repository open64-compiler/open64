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



/* USMID:  "\n@(#)5.0_pl/headers/s_data.h	5.3	05/27/99 10:30:26\n" */

/* The following variables are shared by recursive references to the DATA     */
/* implied-DO routines.			                                      */

int             curr_parent_idx;
int             last_lt_idx;
int             lt_idx;
long64          obj_count;
long64          rep_factor;
expr_arg_type   value_desc;
int             value_il_idx;
opnd_type       value_opnd;


/* The following variables are used by multiple routines.		      */

int             obj_il_idx;


/* loop_tbl is shared by recursive references to the DATA implied-DO routines.*/
/*									      */
/* CAUTION:  parent_idx, sibling_idx, and offspring_idx must be large enough  */
/*           to hold the value of LOOP_TBL_SIZE.                              */
/*
  -------------------------------------------------------------------------
  | lcv_idx                  |        | target_list             | num_   8|
  |                       24 |      8 |                      24 | targets |
  |-----------------------------------------------------------------------|
  | start_idx                | start_ | start_expr_desc_idx     | parent_ | 
  |                       24 | fld  8 |                         | idx    8|
  |-----------------------------------------------------------------------|
  | end_idx                  | end_   | end_expr_desc_idx       |sibling_ |
  |                       24 | fld  8 |                         |idx     8|
  |-----------------------------------------------------------------------|
  | inc_idx                  | inc_   | inc_expr_desc_idx       |offspring|
  |                       24 | fld  8 |                         |_idx    8|
  |-----------------------------------------------------------------------|
  |                                start_value                            |
  |-----------------------------------------------------------------------|
  |                                 end_value                             |
  |-----------------------------------------------------------------------|
  |                                 inc_value                             |
  |-----------------------------------------------------------------------|
  |                                 curr_value                            |
  -------------------------------------------------------------------------

*/

struct  loop_tbl_entry  { Uint          lcv_idx                 : 24;
                          Uint          UNUSED_1                :  8;

                          Uint          target_list             : 24;
                          Uint          num_targets             :  8;

                          Uint          start_idx               : 24;
                          fld_type      start_fld               :  8;

                          Uint		start_expr_desc_idx	: 24;
                          Uint          parent_idx              :  8;

                          Uint          end_idx                 : 24;
                          fld_type      end_fld                 :  8;

                          Uint		end_expr_desc_idx	: 24;
                          Uint          sibling_idx             :  8;

                          Uint          inc_idx                 : 24;
                          fld_type      inc_fld                 :  8;

                          Uint		inc_expr_desc_idx	: 24;
                          Uint          offspring_idx           :  8;

                          long64        start_value                 ;
                          long64        end_value                   ;
                          long64        inc_value                   ;
                          long64        curr_value                  ;
                        };

typedef struct  loop_tbl_entry          loop_tbl_type;


/* Define the size of the loop_tbl for DATA implied-DO processing.            */

# define LOOP_TBL_SIZE  21


/* The first entry is not used so that NULL_IDX can be used to mean "no       */
/* table entry".                                                              */

loop_tbl_type   loop_tbl[LOOP_TBL_SIZE];


/* The following variable is used globally in s_data.c.		      */

long64		ls_word_len;
