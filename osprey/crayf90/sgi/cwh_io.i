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

typedef struct impdo_info IMPDO_INFO;
typedef struct marked_set MARKED_SET;

struct impdo_info {
   ST *index;
   struct impdo_info *next;
};

struct marked_set {
   ST *st;
   struct marked_set *next;
};

typedef struct nlist {
   WN *wn;
   struct nlist *next;
} NLIST;

#define Impdo_index(x) ((x)->index)
#define Impdo_next(x) ((x)->next)

#define Nlist_wn(x) ((x)->wn)
#define Nlist_next(x) ((x)->next)

#define Marked_st(x) ((x)->st)
#define Marked_next(x) ((x)->next)

static WN * cwh_io_create_new_label(void);
static ST * cwh_io_ST_base(ST *st);
static BOOL cwh_io_analyse_io_statement(WN *tree, int mode);
static BOOL cwh_io_analyse_io_item(WN *tree, IMPDO_INFO *impdo_set, int mode);
static BOOL cwh_io_analyse_expr(WN *tree, IMPDO_INFO *impdo_set, int mode);
static BOOL cwh_io_analyse_arr(WN *tree, IMPDO_INFO *impdo_set, int mode);
static INT32 cwh_io_analyse_index_expr(WN *tree, IMPDO_INFO *impdo_set, int mode);
static INT32 member(ST *st, IMPDO_INFO *impdo_set);
static void cwh_io_create_dopes(WN *tree);
static void cwh_io_create_dope_from_item(WN *parent, int kid_num,
                        IMPDO_INFO *impdo_set);
static WN * cwh_io_conv_array_to_dope(WN *arr, IMPDO_INFO *impdo_set,
                                      WN *old_item, WN *char_len, TY_IDX ty,
				      WN *craytype);
static WN * cwh_io_conv_arrsection_to_dope(WN *arr, IMPDO_INFO *impdo_set,
                                      WN *old_item, WN *char_len, TY_IDX ty,
				      WN *craytype);
static INT32 cwh_io_search_implied_do_index(WN *tree, IMPDO_INFO *impdo_set);
static void cwh_io_add_st_to_marked_set(ST *st);
static void cwh_io_unmark(void);
static WN * Substitute_1_For_Impdo_Index_Val(WN *tree, IMPDO_INFO *impdo);
static void cwh_io_split_io_statement(WN *tree);
static void cwh_io_split_io_items(IOSTATEMENT ios, WN **cilist,
                                  INT32 num_cilist_items, WN *item);
static BOOL OPCODE_has_aux(const OPCODE opc);
static BOOL is_f90_pointer(WN *tree);

INT cwh_io_in_ioblock = 0; /* are we in an I/O statement? */

static WN * cwh_io_cvt_tos_label_to_wn(BOOL flag);
