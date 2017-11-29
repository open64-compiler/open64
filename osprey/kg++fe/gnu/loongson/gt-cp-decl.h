/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/* Type information for cp/decl.c.
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
gt_ggc_mx_named_label_list (x_p)
      void *x_p;
{
  struct named_label_list * const x = (struct named_label_list *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_16cp_binding_level ((*x).binding_level);
      gt_ggc_m_9tree_node ((*x).names_in_scope);
      gt_ggc_m_9tree_node ((*x).old_value);
      gt_ggc_m_9tree_node ((*x).label_decl);
      gt_ggc_m_9tree_node ((*x).bad_decls);
      gt_ggc_m_16named_label_list ((*x).next);
  }
}

void
gt_ggc_mx_named_label_use_list (x_p)
      void *x_p;
{
  struct named_label_use_list * const x = (struct named_label_use_list *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_16cp_binding_level ((*x).binding_level);
      gt_ggc_m_9tree_node ((*x).names_in_scope);
      gt_ggc_m_9tree_node ((*x).label_decl);
      gt_ggc_m_20named_label_use_list ((*x).next);
  }
}

void
gt_ggc_mx_binding_table_s (x_p)
      void *x_p;
{
  struct binding_table_s * const x = (struct binding_table_s *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      if ((*x).chain != NULL) {
        size_t i1;
        ggc_set_mark ((*x).chain);
        for (i1 = 0; i1 < (size_t)(((*x)).chain_count); i1++) {
          gt_ggc_m_15binding_entry_s ((*x).chain[i1]);
        }
      }
  }
}

void
gt_ggc_mx_cp_binding_level (x_p)
      void *x_p;
{
  struct cp_binding_level * const x = (struct cp_binding_level *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).names);
      gt_ggc_m_9tree_node ((*x).namespaces);
      gt_ggc_m_15varray_head_tag ((*x).static_decls);
      gt_ggc_m_9tree_node ((*x).vtables);
      gt_ggc_m_15binding_table_s ((*x).type_decls);
      gt_ggc_m_9tree_node ((*x).usings);
      gt_ggc_m_9tree_node ((*x).using_directives);
      gt_ggc_m_9tree_node ((*x).class_shadowed);
      gt_ggc_m_9tree_node ((*x).type_shadowed);
      gt_ggc_m_9tree_node ((*x).shadowed_labels);
      gt_ggc_m_9tree_node ((*x).blocks);
      gt_ggc_m_9tree_node ((*x).this_class);
      gt_ggc_m_16cp_binding_level ((*x).level_chain);
      gt_ggc_m_9tree_node ((*x).dead_vars_from_for);
  }
}

void
gt_ggc_mx_cxx_saved_binding (x_p)
      void *x_p;
{
  struct cxx_saved_binding * const x = (struct cxx_saved_binding *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_17cxx_saved_binding ((*x).previous);
      gt_ggc_m_9tree_node ((*x).identifier);
      gt_ggc_m_11cxx_binding ((*x).binding);
      gt_ggc_m_9tree_node ((*x).class_value);
      gt_ggc_m_9tree_node ((*x).real_type_value);
  }
}

/* GC roots.  */

const struct ggc_root_tab gt_ggc_r_gt_cp_decl_h[] = {
  {
    &typename_htab,
    1,
    sizeof (typename_htab),
    &gt_ggc_m_P9tree_node4htab
  },
  {
    &incomplete_vars,
    1,
    sizeof (incomplete_vars),
    &gt_ggc_mx_tree_node

  },
  {
    &global_type_node,
    1,
    sizeof (global_type_node),
    &gt_ggc_mx_tree_node

  },
  LAST_GGC_ROOT_TAB
};

const struct ggc_root_tab gt_ggc_rd_gt_cp_decl_h[] = {
  { &free_bindings, 1, sizeof (free_bindings), NULL },
  { &free_binding_level, 1, sizeof (free_binding_level), NULL },
  { &free_binding_entry, 1, sizeof (free_binding_entry), NULL },
  LAST_GGC_ROOT_TAB
};

