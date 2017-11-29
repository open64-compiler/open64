/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

/* Type information for cp.
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
gt_ggc_mx_saved_scope (x_p)
      void *x_p;
{
  struct saved_scope * const x = (struct saved_scope *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_17cxx_saved_binding ((*x).old_bindings);
      gt_ggc_m_9tree_node ((*x).old_namespace);
      gt_ggc_m_9tree_node ((*x).decl_ns_list);
      gt_ggc_m_9tree_node ((*x).class_name);
      gt_ggc_m_9tree_node ((*x).class_type);
      gt_ggc_m_9tree_node ((*x).access_specifier);
      gt_ggc_m_9tree_node ((*x).function_decl);
      gt_ggc_m_15varray_head_tag ((*x).lang_base);
      gt_ggc_m_9tree_node ((*x).lang_name);
      gt_ggc_m_9tree_node ((*x).template_parms);
      gt_ggc_m_9tree_node ((*x).x_previous_class_type);
      gt_ggc_m_9tree_node ((*x).x_previous_class_values);
      gt_ggc_m_9tree_node ((*x).x_saved_tree);
      gt_ggc_m_9tree_node ((*x).lookups);
      gt_ggc_m_9tree_node ((*x).last_parms);
      gt_ggc_m_9tree_node ((*x).x_stmt_tree.x_last_stmt);
      gt_ggc_m_9tree_node ((*x).x_stmt_tree.x_last_expr_type);
      gt_ggc_m_16cp_binding_level ((*x).class_bindings);
      gt_ggc_m_16cp_binding_level ((*x).bindings);
      gt_ggc_m_11saved_scope ((*x).prev);
  }
}

void
gt_ggc_mx_lang_id2 (x_p)
      void *x_p;
{
  struct lang_id2 * const x = (struct lang_id2 *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).label_value);
      gt_ggc_m_9tree_node ((*x).implicit_decl);
      gt_ggc_m_9tree_node ((*x).error_locus);
  }
}

void
gt_ggc_mx_binding_entry_s (x_p)
      void *x_p;
{
  struct binding_entry_s * const x = (struct binding_entry_s *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_15binding_entry_s ((*x).chain);
      gt_ggc_m_9tree_node ((*x).name);
      gt_ggc_m_9tree_node ((*x).type);
  }
}

void
gt_ggc_mx_cxx_binding (x_p)
      void *x_p;
{
  struct cxx_binding * const x = (struct cxx_binding *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_11cxx_binding ((*x).previous);
      gt_ggc_m_9tree_node ((*x).value);
      gt_ggc_m_9tree_node ((*x).type);
      switch ((*x).has_level)
        {
        case 0:
          gt_ggc_m_9tree_node ((*x).scope.scope);
          break;
        case 1:
          gt_ggc_m_16cp_binding_level ((*x).scope.level);
          break;
        default:
          break;
        }
  }
}

#ifdef KEY
/* Tell the GNU garbage collector about 'tree' nodes used for EKO OpenMP
 * processing.
 */
void
gt_ggc_m_9tree_node_for_mp (tree x)
{
  /* Two-level switch statement */
  switch (x->omp.choice)
  {
    case parallel_dir_b:
      {
        struct parallel_clause_list * clause =
          (struct parallel_clause_list *) (x->omp.omp_clause_list);

        if (!clause) break;
        switch (clause->type)
        {
          case p_if:
          case p_num_threads:
            gt_ggc_m_9tree_node (clause->node.expr_no_commas);
            break;
          case p_private:
          case p_firstprivate:
          case p_shared:
          case p_copyin:
            gt_ggc_m_9tree_node (clause->node.var_list);
            break;
          case p_reduction:
            gt_ggc_m_9tree_node (clause->node.reduction_node.var_list);
            break;
          default:
            break;
        }
      }
      break;

    case for_dir_b:
      {
        struct for_clause_list * clause = 
          (struct for_clause_list *) (x->omp.omp_clause_list);
        if (!clause) break;
        switch (clause->type)
        {
          case f_private:
          case f_firstprivate:
          case f_lastprivate:
            gt_ggc_m_9tree_node (clause->node.var_list);
            break;
          case f_schedule_2:
            gt_ggc_m_9tree_node (clause->node.schedule_node.chunk_size);
            break;
          case f_reduction:
            gt_ggc_m_9tree_node (clause->node.reduction_node.var_list);
            break;
          default:
            break;
        }
      }
      break;

    case sections_cons_b:
      {
        struct sections_clause_list * clause =
          (struct sections_clause_list *) (x->omp.omp_clause_list);
        if (!clause) break;
        switch (clause->type)
        {
          case sections_private:
          case sections_firstprivate:
          case sections_lastprivate:
            gt_ggc_m_9tree_node (clause->node.var_list);
            break;
          case sections_reduction:
            gt_ggc_m_9tree_node (clause->node.reduction_node.var_list);
            break;
          default:
            break;
        }
      }
      break;

    case single_cons_b:
      {
        struct single_clause_list * clause =
          (struct single_clause_list *) (x->omp.omp_clause_list);
        if (!clause) break;
        switch (clause->type)
        {
          case single_private:
          case single_firstprivate:
          case single_copyprivate:
            gt_ggc_m_9tree_node (clause->node.var_list);
            break;
          default:
            break;
        }
      }
      break;

    case par_for_cons_b:
      {
        struct parallel_for_clause_list * clause =
          (struct parallel_for_clause_list *) (x->omp.omp_clause_list);
        if (!clause) break;

        switch (clause->type)
        {
          case p_for_if:
          case p_for_num_threads:
            gt_ggc_m_9tree_node (clause->node.expr_no_commas);
            break;
          case p_for_copyprivate:
          case p_for_shared:
          case p_for_copyin:
          case p_for_firstprivate:
          case p_for_lastprivate:
          case p_for_private:
	    gt_ggc_m_9tree_node (clause->node.var_list);
	    break;
          case p_for_schedule_2:
            gt_ggc_m_9tree_node (clause->node.schedule_node.chunk_size);
            break;
          case p_for_reduction:
            gt_ggc_m_9tree_node (clause->node.reduction_node.var_list);
            break;
          default:
	    break;
        }
      }
      break;

    case par_sctn_cons_b:
      {
        struct parallel_sections_clause_list * clause =
          (struct parallel_sections_clause_list *) (x->omp.omp_clause_list);
        if (!clause) break;

        switch (clause->type)
          {
            case p_sections_if:
            case p_sections_num_threads:
              gt_ggc_m_9tree_node (clause->node.expr_no_commas);
              break;
            case p_sections_private:
            case p_sections_copyprivate:
            case p_sections_firstprivate:
            case p_sections_lastprivate:
            case p_sections_shared:
            case p_sections_copyin:
              gt_ggc_m_9tree_node (clause->node.var_list);
              break;
            case p_sections_reduction:
              gt_ggc_m_9tree_node (clause->node.reduction_node.var_list);
              break;
            default:
              break;
          }
      }
      break;

    case critical_cons_b:
    case flush_dir:
    case thdprv_dir:
    case options_dir:
    case exec_freq_dir:
      gt_ggc_m_9tree_node ((tree)x->omp.omp_clause_list);
      break;

    default:
      break;
  }
}
#endif /* KEY */

void
gt_ggc_mx_lang_tree_node (x_p)
      void *x_p;
{
  union lang_tree_node * x = (union lang_tree_node *)x_p;
  union lang_tree_node * xlimit = x;
  while (ggc_test_and_set_mark (xlimit))
   xlimit = ((union lang_tree_node *)TREE_CHAIN (&(*xlimit).generic));
  while (x != xlimit)
    {
      switch (cp_tree_node_structure (&((*x))))
        {
        case TS_CP_COMMON:
          gt_ggc_m_9tree_node ((*x).common.chain);
          gt_ggc_m_9tree_node ((*x).common.type);
          break;
        case TS_CP_GENERIC:
          switch (tree_node_structure (&((*x).generic)))
            {
            case TS_COMMON:
              gt_ggc_m_9tree_node ((*x).generic.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.common.type);
              break;
            case TS_INT_CST:
              gt_ggc_m_9tree_node ((*x).generic.int_cst.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.int_cst.common.type);
              gt_ggc_m_7rtx_def ((*x).generic.int_cst.rtl);
              break;
            case TS_REAL_CST:
              gt_ggc_m_9tree_node ((*x).generic.real_cst.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.real_cst.common.type);
              gt_ggc_m_7rtx_def ((*x).generic.real_cst.rtl);
              gt_ggc_m_10real_value ((*x).generic.real_cst.real_cst_ptr);
              break;
            case TS_VECTOR:
              gt_ggc_m_9tree_node ((*x).generic.vector.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.vector.common.type);
              gt_ggc_m_7rtx_def ((*x).generic.vector.rtl);
              gt_ggc_m_9tree_node ((*x).generic.vector.elements);
              break;
            case TS_STRING:
              gt_ggc_m_9tree_node ((*x).generic.string.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.string.common.type);
              gt_ggc_m_7rtx_def ((*x).generic.string.rtl);
              break;
            case TS_COMPLEX:
              gt_ggc_m_9tree_node ((*x).generic.complex.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.complex.common.type);
              gt_ggc_m_7rtx_def ((*x).generic.complex.rtl);
              gt_ggc_m_9tree_node ((*x).generic.complex.real);
              gt_ggc_m_9tree_node ((*x).generic.complex.imag);
              break;
            case TS_IDENTIFIER:
              gt_ggc_m_9tree_node ((*x).generic.identifier.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.identifier.common.type);
              break;
            case TS_DECL:
              gt_ggc_m_9tree_node ((*x).generic.decl.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.decl.common.type);
              gt_ggc_m_9tree_node ((*x).generic.decl.size);
              gt_ggc_m_9tree_node ((*x).generic.decl.size_unit);
              gt_ggc_m_9tree_node ((*x).generic.decl.name);
              gt_ggc_m_9tree_node ((*x).generic.decl.context);
              gt_ggc_m_9tree_node ((*x).generic.decl.arguments);
              gt_ggc_m_9tree_node ((*x).generic.decl.result);
              gt_ggc_m_9tree_node ((*x).generic.decl.initial);
              gt_ggc_m_9tree_node ((*x).generic.decl.initial_2);
              gt_ggc_m_9tree_node ((*x).generic.decl.abstract_origin);
              gt_ggc_m_9tree_node ((*x).generic.decl.assembler_name);
              gt_ggc_m_9tree_node ((*x).generic.decl.section_name);
              gt_ggc_m_9tree_node ((*x).generic.decl.attributes);
              gt_ggc_m_7rtx_def ((*x).generic.decl.rtl);
              gt_ggc_m_7rtx_def ((*x).generic.decl.live_range_rtl);
              switch (TREE_CODE((tree) &((*x))))
                {
                case FUNCTION_DECL:
                  gt_ggc_m_8function ((*x).generic.decl.u2.f);
                  break;
                case PARM_DECL:
                  gt_ggc_m_7rtx_def ((*x).generic.decl.u2.r);
                  break;
                case FIELD_DECL:
                  gt_ggc_m_9tree_node ((*x).generic.decl.u2.t);
                  break;
                default:
                  break;
                }
              gt_ggc_m_9tree_node ((*x).generic.decl.saved_tree);
              gt_ggc_m_9tree_node ((*x).generic.decl.inlined_fns);
              gt_ggc_m_9tree_node ((*x).generic.decl.vindex);
              gt_ggc_m_9lang_decl ((*x).generic.decl.lang_specific);
              break;
            case TS_TYPE:
              gt_ggc_m_9tree_node ((*x).generic.type.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.type.common.type);
              gt_ggc_m_9tree_node ((*x).generic.type.values);
              gt_ggc_m_9tree_node ((*x).generic.type.size);
              gt_ggc_m_9tree_node ((*x).generic.type.size_unit);
              gt_ggc_m_9tree_node ((*x).generic.type.attributes);
              gt_ggc_m_9tree_node ((*x).generic.type.pointer_to);
              gt_ggc_m_9tree_node ((*x).generic.type.reference_to);
              switch (debug_hooks == &sdb_debug_hooks ? 1 : debug_hooks == &dwarf2_debug_hooks ? 2 : 0)
                {
                case 1:
                  break;
                default:
                  break;
                }
              gt_ggc_m_9tree_node ((*x).generic.type.name);
              gt_ggc_m_9tree_node ((*x).generic.type.minval);
              gt_ggc_m_9tree_node ((*x).generic.type.maxval);
              gt_ggc_m_9tree_node ((*x).generic.type.next_variant);
              gt_ggc_m_9tree_node ((*x).generic.type.main_variant);
              gt_ggc_m_9tree_node ((*x).generic.type.binfo);
              gt_ggc_m_9tree_node ((*x).generic.type.context);
              gt_ggc_m_9lang_type ((*x).generic.type.lang_specific);
              gt_ggc_m_9tree_node ((*x).generic.type.extra_methods);
              break;
            case TS_LIST:
              gt_ggc_m_9tree_node ((*x).generic.list.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.list.common.type);
              gt_ggc_m_9tree_node ((*x).generic.list.purpose);
              gt_ggc_m_9tree_node ((*x).generic.list.value);
              break;
            case TS_VEC:
              gt_ggc_m_9tree_node ((*x).generic.vec.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.vec.common.type);
              {
                size_t i1_0;
                const size_t ilimit1_0 = (TREE_VEC_LENGTH ((tree)&((*x).generic.vec)));
                for (i1_0 = 0; i1_0 < ilimit1_0; i1_0++) {
                  gt_ggc_m_9tree_node ((*x).generic.vec.a[i1_0]);
                }
              }
              break;
            case TS_EXP:
              gt_ggc_m_9tree_node ((*x).generic.exp.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.exp.common.type);
              switch (TREE_CODE ((tree) &(*x)))
                {
                case METHOD_CALL_EXPR:
                  gt_ggc_m_7rtx_def ((*x).generic.exp.operands[3]);
                  gt_ggc_m_9tree_node ((*x).generic.exp.operands[2]);
                  gt_ggc_m_9tree_node ((*x).generic.exp.operands[1]);
                  gt_ggc_m_9tree_node ((*x).generic.exp.operands[0]);
                  break;
                case WITH_CLEANUP_EXPR:
                  gt_ggc_m_7rtx_def ((*x).generic.exp.operands[2]);
                  gt_ggc_m_9tree_node ((*x).generic.exp.operands[1]);
                  gt_ggc_m_9tree_node ((*x).generic.exp.operands[0]);
                  break;
                case RTL_EXPR:
                  gt_ggc_m_7rtx_def ((*x).generic.exp.operands[1]);
                  gt_ggc_m_7rtx_def ((*x).generic.exp.operands[0]);
                  break;
                case GOTO_SUBROUTINE_EXPR:
                  gt_ggc_m_7rtx_def ((*x).generic.exp.operands[1]);
                  gt_ggc_m_7rtx_def ((*x).generic.exp.operands[0]);
                  break;
                case SAVE_EXPR:
                  gt_ggc_m_7rtx_def ((*x).generic.exp.operands[2]);
                  gt_ggc_m_9tree_node ((*x).generic.exp.operands[1]);
                  gt_ggc_m_9tree_node ((*x).generic.exp.operands[0]);
                  break;
                default:
                  {
                    size_t i2_0;
                    const size_t ilimit2_0 = (TREE_CODE_LENGTH (TREE_CODE ((tree) &(*x))));
                    for (i2_0 = 0; i2_0 < ilimit2_0; i2_0++) {
                      gt_ggc_m_9tree_node ((*x).generic.exp.operands[i2_0]);
                    }
                  }
                  break;
                }
              break;
            case TS_BLOCK:
              gt_ggc_m_9tree_node ((*x).generic.block.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.block.common.type);
              gt_ggc_m_9tree_node ((*x).generic.block.vars);
              gt_ggc_m_9tree_node ((*x).generic.block.subblocks);
              gt_ggc_m_9tree_node ((*x).generic.block.supercontext);
              gt_ggc_m_9tree_node ((*x).generic.block.abstract_origin);
              gt_ggc_m_9tree_node ((*x).generic.block.fragment_origin);
              gt_ggc_m_9tree_node ((*x).generic.block.fragment_chain);
              break;
#ifdef KEY
            case TS_OMP:
              gt_ggc_m_9tree_node ((*x).generic.omp.common.chain);
              gt_ggc_m_9tree_node ((*x).generic.omp.common.type);

	      gt_ggc_m_9tree_node_for_mp ((tree) &(*x));
              break;
#endif /* KEY */
            default:
              break;
            }
          break;
        case TS_CP_TPI:
          gt_ggc_m_9tree_node ((*x).tpi.common.chain);
          gt_ggc_m_9tree_node ((*x).tpi.common.type);
          gt_ggc_m_9tree_node ((*x).tpi.decl);
          break;
        case TS_CP_PTRMEM:
          gt_ggc_m_9tree_node ((*x).ptrmem.common.chain);
          gt_ggc_m_9tree_node ((*x).ptrmem.common.type);
          gt_ggc_m_7rtx_def ((*x).ptrmem.rtl);
          gt_ggc_m_9tree_node ((*x).ptrmem.member);
          break;
        case TS_CP_OVERLOAD:
          gt_ggc_m_9tree_node ((*x).overload.common.chain);
          gt_ggc_m_9tree_node ((*x).overload.common.type);
          gt_ggc_m_9tree_node ((*x).overload.function);
          break;
        case TS_CP_WRAPPER:
          gt_ggc_m_9tree_node ((*x).wrapper.common.chain);
          gt_ggc_m_9tree_node ((*x).wrapper.common.type);
          gt_ggc_m_11z_candidate ((*x).wrapper.z_c);
          break;
        case TS_CP_SRCLOC:
          gt_ggc_m_9tree_node ((*x).srcloc.common.chain);
          gt_ggc_m_9tree_node ((*x).srcloc.common.type);
          break;
        case TS_CP_IDENTIFIER:
          gt_ggc_m_9tree_node ((*x).identifier.c_common.common.chain);
          gt_ggc_m_9tree_node ((*x).identifier.c_common.common.type);
          gt_ggc_m_11cxx_binding ((*x).identifier.namespace_bindings);
          gt_ggc_m_11cxx_binding ((*x).identifier.bindings);
          gt_ggc_m_9tree_node ((*x).identifier.class_value);
          gt_ggc_m_9tree_node ((*x).identifier.class_template_info);
          gt_ggc_m_8lang_id2 ((*x).identifier.x);
          break;
        default:
          break;
        }
      x = ((union lang_tree_node *)TREE_CHAIN (&(*x).generic));
  }
}

void
gt_ggc_mx_lang_decl (x_p)
      void *x_p;
{
  struct lang_decl * const x = (struct lang_decl *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      switch (((*x).decl_flags).u1sel)
        {
        case 0:
          gt_ggc_m_9tree_node ((*x).decl_flags.u.template_info);
          break;
        case 1:
          gt_ggc_m_16cp_binding_level ((*x).decl_flags.u.level);
          break;
        default:
          break;
        }
      switch (((*x).decl_flags).u2sel)
        {
        case 0:
          gt_ggc_m_9tree_node ((*x).decl_flags.u2.access);
          break;
        case 2:
          gt_ggc_m_9tree_node ((*x).decl_flags.u2.vcall_offset);
          break;
        default:
          break;
        }
      switch (((*x)).decl_flags.can_be_full)
        {
        case 1:
          gt_ggc_m_9tree_node ((*x).u.f.befriending_classes);
          gt_ggc_m_9tree_node ((*x).u.f.context);
          gt_ggc_m_9tree_node ((*x).u.f.cloned_function);
          gt_ggc_m_9tree_node ((*x).u.f.named_return_object);
          switch (((*x).u.f).u3sel + ((*x).u.f).pending_inline_p)
            {
            case 0:
              gt_ggc_m_9tree_node ((*x).u.f.u.sorted_fields);
              break;
            case 2:
              gt_ggc_m_13unparsed_text ((*x).u.f.u.pending_inline_info);
              break;
            case 1:
              gt_ggc_m_17language_function ((*x).u.f.u.saved_language_function);
              break;
            default:
              break;
            }
          break;
        default:
          break;
        }
  }
}

void
gt_ggc_mx_lang_type (x_p)
      void *x_p;
{
  struct lang_type * const x = (struct lang_type *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      switch (((*x).u).h.is_lang_type_class)
        {
        case 2:
          break;
        case 1:
          gt_ggc_m_9tree_node ((*x).u.c.primary_base);
          gt_ggc_m_9tree_node ((*x).u.c.vfields);
          gt_ggc_m_9tree_node ((*x).u.c.vcall_indices);
          gt_ggc_m_9tree_node ((*x).u.c.vtables);
          gt_ggc_m_9tree_node ((*x).u.c.typeinfo_var);
          gt_ggc_m_9tree_node ((*x).u.c.vbases);
          gt_ggc_m_15binding_table_s ((*x).u.c.nested_udts);
          gt_ggc_m_9tree_node ((*x).u.c.as_base);
          gt_ggc_m_9tree_node ((*x).u.c.pure_virtuals);
          gt_ggc_m_9tree_node ((*x).u.c.friend_classes);
          gt_ggc_m_9tree_node ((*x).u.c.methods);
          gt_ggc_m_9tree_node ((*x).u.c.key_method);
          gt_ggc_m_9tree_node ((*x).u.c.decl_list);
          gt_ggc_m_9tree_node ((*x).u.c.template_info);
          gt_ggc_m_9tree_node ((*x).u.c.befriending_classes);
          gt_ggc_m_9tree_node ((*x).u.c.copy_constructor);
          break;
        case 0:
          gt_ggc_m_9tree_node ((*x).u.ptrmem.record);
          break;
        default:
          break;
        }
  }
}

void
gt_ggc_mx_language_function (x_p)
      void *x_p;
{
  struct language_function * const x = (struct language_function *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).base.x_stmt_tree.x_last_stmt);
      gt_ggc_m_9tree_node ((*x).base.x_stmt_tree.x_last_expr_type);
      gt_ggc_m_9tree_node ((*x).base.x_scope_stmt_stack);
      gt_ggc_m_9tree_node ((*x).x_dtor_label);
      gt_ggc_m_9tree_node ((*x).x_current_class_ptr);
      gt_ggc_m_9tree_node ((*x).x_current_class_ref);
      gt_ggc_m_9tree_node ((*x).x_eh_spec_block);
      gt_ggc_m_9tree_node ((*x).x_in_charge_parm);
      gt_ggc_m_9tree_node ((*x).x_vtt_parm);
      gt_ggc_m_9tree_node ((*x).x_return_value);
      gt_ggc_m_20named_label_use_list ((*x).x_named_label_uses);
      gt_ggc_m_16named_label_list ((*x).x_named_labels);
      gt_ggc_m_16cp_binding_level ((*x).bindings);
      gt_ggc_m_15varray_head_tag ((*x).x_local_names);
      gt_ggc_m_13unparsed_text ((*x).unparsed_inlines);
  }
}

/* GC roots.  */

const struct ggc_root_tab gt_ggc_r_gtype_cp_h[] = {
  {
    &got_object,
    1,
    sizeof (got_object),
    &gt_ggc_mx_tree_node

  },
  {
    &got_scope,
    1,
    sizeof (got_scope),
    &gt_ggc_mx_tree_node

  },
  {
    &lastiddecl,
    1,
    sizeof (lastiddecl),
    &gt_ggc_mx_tree_node

  },
  {
    &last_function_parms,
    1,
    sizeof (last_function_parms),
    &gt_ggc_mx_tree_node

  },
  {
    &static_dtors,
    1,
    sizeof (static_dtors),
    &gt_ggc_mx_tree_node

  },
  {
    &static_ctors,
    1,
    sizeof (static_ctors),
    &gt_ggc_mx_tree_node

  },
  {
    &static_aggregates,
    1,
    sizeof (static_aggregates),
    &gt_ggc_mx_tree_node

  },
  {
    &local_classes,
    1,
    sizeof (local_classes),
    &gt_ggc_mx_varray_head_tag

  },
  {
    &anonymous_namespace_name,
    1,
    sizeof (anonymous_namespace_name),
    &gt_ggc_mx_tree_node

  },
  {
    &integer_three_node,
    1,
    sizeof (integer_three_node),
    &gt_ggc_mx_tree_node

  },
  {
    &integer_two_node,
    1,
    sizeof (integer_two_node),
    &gt_ggc_mx_tree_node

  },
  {
    &error_mark_list,
    1,
    sizeof (error_mark_list),
    &gt_ggc_mx_tree_node

  },
  {
    &global_namespace,
    1,
    sizeof (global_namespace),
    &gt_ggc_mx_tree_node

  },
  {
    &scope_chain,
    1,
    sizeof (scope_chain),
    &gt_ggc_mx_saved_scope

  },
  {
    &cp_global_trees[0],
    1 * (CPTI_MAX),
    sizeof (cp_global_trees[0]),
    &gt_ggc_mx_tree_node

  },
  LAST_GGC_ROOT_TAB
};

extern const struct ggc_root_tab gt_ggc_r_gtype_desc_c[];
extern const struct ggc_root_tab gt_ggc_r_gt_alias_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cselib_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_dwarf2out_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_emit_rtl_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_except_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_explow_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_expr_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_fold_const_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_function_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_gcse_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_integrate_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_optabs_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_profile_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ra_build_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_regclass_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_reg_stack_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_sdbout_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_stor_layout_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_varasm_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_mips_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_mangle_h[];
extern const struct ggc_root_tab gt_ggc_r_gtype_cp_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_call_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_decl_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_decl2_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_parse_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_pt_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_repo_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_spew_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_tree_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cp_method_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_c_common_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_c_pragma_h[];
const struct ggc_root_tab * const gt_ggc_rtab[] = {
  gt_ggc_r_gtype_desc_c,
  gt_ggc_r_gt_alias_h,
  gt_ggc_r_gt_cselib_h,
  gt_ggc_r_gt_dwarf2out_h,
  gt_ggc_r_gt_emit_rtl_h,
  gt_ggc_r_gt_except_h,
  gt_ggc_r_gt_explow_h,
  gt_ggc_r_gt_expr_h,
  gt_ggc_r_gt_fold_const_h,
  gt_ggc_r_gt_function_h,
  gt_ggc_r_gt_gcse_h,
  gt_ggc_r_gt_integrate_h,
  gt_ggc_r_gt_optabs_h,
  gt_ggc_r_gt_profile_h,
  gt_ggc_r_gt_ra_build_h,
  gt_ggc_r_gt_regclass_h,
  gt_ggc_r_gt_reg_stack_h,
  gt_ggc_r_gt_sdbout_h,
  gt_ggc_r_gt_stor_layout_h,
  gt_ggc_r_gt_varasm_h,
  gt_ggc_r_gt_mips_h,
  gt_ggc_r_gt_cp_mangle_h,
  gt_ggc_r_gtype_cp_h,
  gt_ggc_r_gt_cp_call_h,
  gt_ggc_r_gt_cp_decl_h,
  gt_ggc_r_gt_cp_decl2_h,
  gt_ggc_r_gt_cp_parse_h,
  gt_ggc_r_gt_cp_pt_h,
  gt_ggc_r_gt_cp_repo_h,
  gt_ggc_r_gt_cp_spew_h,
  gt_ggc_r_gt_cp_tree_h,
  gt_ggc_r_gt_cp_method_h,
  gt_ggc_r_gt_c_common_h,
  gt_ggc_r_gt_c_pragma_h,
  NULL
};
extern const struct ggc_root_tab gt_ggc_rd_gt_bitmap_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_cselib_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_emit_rtl_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_lists_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_cp_decl_h[];
const struct ggc_root_tab * const gt_ggc_deletable_rtab[] = {
  gt_ggc_rd_gt_bitmap_h,
  gt_ggc_rd_gt_cselib_h,
  gt_ggc_rd_gt_emit_rtl_h,
  gt_ggc_rd_gt_lists_h,
  gt_ggc_rd_gt_cp_decl_h,
  NULL
};
extern const struct ggc_cache_tab gt_ggc_rc_gt_emit_rtl_h[];
extern const struct ggc_cache_tab gt_ggc_rc_gt_fold_const_h[];
extern const struct ggc_cache_tab gt_ggc_rc_gt_tree_h[];
const struct ggc_cache_tab * const gt_ggc_cache_rtab[] = {
  gt_ggc_rc_gt_emit_rtl_h,
  gt_ggc_rc_gt_fold_const_h,
  gt_ggc_rc_gt_tree_h,
  NULL
};
