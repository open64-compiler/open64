/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/* Type information for cp/call.c.
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* This file is machine generated.  Do not edit.  */

void
gt_ggc_mx_z_candidate (x_p)
      void *x_p;
{
  struct z_candidate * const x = (struct z_candidate *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).fn);
      gt_ggc_m_9tree_node ((*x).convs);
      gt_ggc_m_9tree_node ((*x).second_conv);
      gt_ggc_m_9tree_node ((*x).access_path);
      gt_ggc_m_9tree_node ((*x).conversion_path);
      gt_ggc_m_9tree_node ((*x).template);
      gt_ggc_m_9tree_node ((*x).warnings);
      gt_ggc_m_11z_candidate ((*x).next);
  }
}

/* GC roots.  */

const struct ggc_root_tab gt_ggc_r_gt_cp_call_h[] = {
  {
    &java_iface_lookup_fn,
    1,
    sizeof (java_iface_lookup_fn),
    &gt_ggc_mx_tree_node

  },
  LAST_GGC_ROOT_TAB
};

