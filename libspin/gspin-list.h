/*
  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.

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
 */

#ifndef __GSPIN_LIST_H__
#define __GSPIN_LIST_H__

static inline gs_t gs_cons (gs_t e, gs_t es)
{
  gs_t node = __gs(CONS);
  gs_set_operand(node, 0, e);
  gs_set_operand(node, 1, es);
  return node;
}

gs_count_t gs_length (gs_t list);
gs_t gs_index (gs_t list, gs_count_t index);
#ifdef FE_GNU_4_2_0
void gs_set_index (gs_t list, gs_count_t index, gs_t value);
#endif

#endif /* __GSPIN_LIST_H__ */
