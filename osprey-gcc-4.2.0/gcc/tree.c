/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. PathScale, LLC.  All rights reserved.
 */
/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */
/* Language-independent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* This file contains the low level primitives for operating on tree nodes,
   including allocation, list operations, interning of identifiers,
   construction of data type nodes and statement nodes,
   and construction of type conversion nodes.  It also contains
   tables index by tree code that describe how to take apart
   nodes of that code.

   It is intended to be language-independent, but occasionally
   calls language-dependent routines defined (for C) in typecheck.c.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "tree.h"
#include "real.h"
#include "tm_p.h"
#include "function.h"
#include "obstack.h"
#include "toplev.h"
#include "ggc.h"
#include "hashtab.h"
#include "output.h"
#include "target.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "params.h"
#include "pointer-set.h"

#ifdef KEY
#include "dwarf2.h"

extern tree cplus_expand_constant (tree);
#endif

/* Each tree code class has an associated string representation.
   These must correspond to the tree_code_class entries.  */

const char *const tree_code_class_strings[] =
{
  "exceptional",
  "constant",
  "type",
  "declaration",
  "reference",
  "comparison",
  "unary",
  "binary",
  "statement",
  "expression",
};

/* obstack.[ch] explicitly declined to prototype this.  */
extern int _obstack_allocated_p (struct obstack *h, void *obj);

#ifdef GATHER_STATISTICS
/* Statistics-gathering stuff.  */

int tree_node_counts[(int) all_kinds];
int tree_node_sizes[(int) all_kinds];

/* Keep in sync with tree.h:enum tree_node_kind.  */
static const char * const tree_node_kind_names[] = {
  "decls",
  "types",
  "blocks",
  "stmts",
  "refs",
  "exprs",
  "constants",
  "identifiers",
  "perm_tree_lists",
  "temp_tree_lists",
  "vecs",
  "binfos",
  "phi_nodes",
  "ssa names",
  "constructors",
  "random kinds",
  "lang_decl kinds",
  "lang_type kinds",
  "omp clauses"
};
#endif /* GATHER_STATISTICS */

/* Unique id for next decl created.  */
static GTY(()) int next_decl_uid;
/* Unique id for next type created.  */
static GTY(()) int next_type_uid = 1;

/* Since we cannot rehash a type after it is in the table, we have to
   keep the hash code.  */

struct type_hash GTY(())
{
  unsigned long hash;
  tree type;
};

/* Initial size of the hash table (rounded to next prime).  */
#define TYPE_HASH_INITIAL_SIZE 1000

/* Now here is the hash table.  When recording a type, it is added to
   the slot whose index is the hash code.  Note that the hash table is
   used for several kinds of types (function types, array types and
   array index range types, for now).  While all these live in the
   same table, they are completely independent, and the hash code is
   computed differently for each of these.  */

static GTY ((if_marked ("type_hash_marked_p"), param_is (struct type_hash)))
     htab_t type_hash_table;

/* Hash table and temporary node for larger integer const values.  */
static GTY (()) tree int_cst_node;
static GTY ((if_marked ("ggc_marked_p"), param_is (union tree_node)))
     htab_t int_cst_hash_table;

/* General tree->tree mapping  structure for use in hash tables.  */


static GTY ((if_marked ("tree_map_marked_p"), param_is (struct tree_map))) 
     htab_t debug_expr_for_decl;

static GTY ((if_marked ("tree_map_marked_p"), param_is (struct tree_map))) 
     htab_t value_expr_for_decl;

static GTY ((if_marked ("tree_int_map_marked_p"), param_is (struct tree_int_map)))
  htab_t init_priority_for_decl;

static GTY ((if_marked ("tree_map_marked_p"), param_is (struct tree_map)))
  htab_t restrict_base_for_decl;

struct tree_int_map GTY(())
{
  tree from;
  unsigned short to;
};
static unsigned int tree_int_map_hash (const void *);
static int tree_int_map_eq (const void *, const void *);
static int tree_int_map_marked_p (const void *);
static void set_type_quals (tree, int);
static int type_hash_eq (const void *, const void *);
static hashval_t type_hash_hash (const void *);
static hashval_t int_cst_hash_hash (const void *);
static int int_cst_hash_eq (const void *, const void *);
static void print_type_hash_statistics (void);
static void print_debug_expr_statistics (void);
static void print_value_expr_statistics (void);
static int type_hash_marked_p (const void *);
static unsigned int type_hash_list (tree, hashval_t);
static unsigned int attribute_hash_list (tree, hashval_t);

tree global_trees[TI_MAX];
tree integer_types[itk_none];

unsigned char tree_contains_struct[256][64];

/* Number of operands for each OpenMP clause.  */
unsigned const char omp_clause_num_ops[] =
{
  0, /* OMP_CLAUSE_ERROR  */
  1, /* OMP_CLAUSE_PRIVATE  */
  1, /* OMP_CLAUSE_SHARED  */
  1, /* OMP_CLAUSE_FIRSTPRIVATE  */
  1, /* OMP_CLAUSE_LASTPRIVATE  */
  4, /* OMP_CLAUSE_REDUCTION  */
  1, /* OMP_CLAUSE_COPYIN  */
  1, /* OMP_CLAUSE_COPYPRIVATE  */
  1, /* OMP_CLAUSE_IF  */
  1, /* OMP_CLAUSE_NUM_THREADS  */
  1, /* OMP_CLAUSE_SCHEDULE  */
  0, /* OMP_CLAUSE_NOWAIT  */
  0, /* OMP_CLAUSE_ORDERED  */
  0  /* OMP_CLAUSE_DEFAULT  */
};

const char * const omp_clause_code_name[] =
{
  "error_clause",
  "private",
  "shared",
  "firstprivate",
  "lastprivate",
  "reduction",
  "copyin",
  "copyprivate",
  "if",
  "num_threads",
  "schedule",
  "nowait",
  "ordered",
  "default"
};

/* Init tree.c.  */

void
init_ttree (void)
{
  /* Initialize the hash table of types.  */
  type_hash_table = htab_create_ggc (TYPE_HASH_INITIAL_SIZE, type_hash_hash,
				     type_hash_eq, 0);

  debug_expr_for_decl = htab_create_ggc (512, tree_map_hash,
					 tree_map_eq, 0);

  value_expr_for_decl = htab_create_ggc (512, tree_map_hash,
					 tree_map_eq, 0);
  init_priority_for_decl = htab_create_ggc (512, tree_int_map_hash,
					    tree_int_map_eq, 0);
  restrict_base_for_decl = htab_create_ggc (256, tree_map_hash,
					    tree_map_eq, 0);

  int_cst_hash_table = htab_create_ggc (1024, int_cst_hash_hash,
					int_cst_hash_eq, NULL);
  
  int_cst_node = make_node (INTEGER_CST);

  tree_contains_struct[FUNCTION_DECL][TS_DECL_NON_COMMON] = 1;
  tree_contains_struct[TRANSLATION_UNIT_DECL][TS_DECL_NON_COMMON] = 1;
  tree_contains_struct[TYPE_DECL][TS_DECL_NON_COMMON] = 1;
  

  tree_contains_struct[CONST_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[VAR_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[PARM_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[RESULT_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[FUNCTION_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[TYPE_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[TRANSLATION_UNIT_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[LABEL_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[FIELD_DECL][TS_DECL_COMMON] = 1;


  tree_contains_struct[CONST_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[VAR_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[PARM_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[RESULT_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[FUNCTION_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[LABEL_DECL][TS_DECL_WRTL] = 1; 

  tree_contains_struct[CONST_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[VAR_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[PARM_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[RESULT_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[FUNCTION_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[TYPE_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[TRANSLATION_UNIT_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[LABEL_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[FIELD_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[STRUCT_FIELD_TAG][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[NAME_MEMORY_TAG][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[SYMBOL_MEMORY_TAG][TS_DECL_MINIMAL] = 1;

  tree_contains_struct[STRUCT_FIELD_TAG][TS_MEMORY_TAG] = 1;
  tree_contains_struct[NAME_MEMORY_TAG][TS_MEMORY_TAG] = 1;
  tree_contains_struct[SYMBOL_MEMORY_TAG][TS_MEMORY_TAG] = 1;

  tree_contains_struct[STRUCT_FIELD_TAG][TS_STRUCT_FIELD_TAG] = 1;

  tree_contains_struct[VAR_DECL][TS_DECL_WITH_VIS] = 1;
  tree_contains_struct[FUNCTION_DECL][TS_DECL_WITH_VIS] = 1;
  tree_contains_struct[TYPE_DECL][TS_DECL_WITH_VIS] = 1;
  tree_contains_struct[TRANSLATION_UNIT_DECL][TS_DECL_WITH_VIS] = 1;
  
  tree_contains_struct[VAR_DECL][TS_VAR_DECL] = 1;
  tree_contains_struct[FIELD_DECL][TS_FIELD_DECL] = 1;
  tree_contains_struct[PARM_DECL][TS_PARM_DECL] = 1;
  tree_contains_struct[LABEL_DECL][TS_LABEL_DECL] = 1;
  tree_contains_struct[RESULT_DECL][TS_RESULT_DECL] = 1;
  tree_contains_struct[CONST_DECL][TS_CONST_DECL] = 1;
  tree_contains_struct[TYPE_DECL][TS_TYPE_DECL] = 1;
  tree_contains_struct[FUNCTION_DECL][TS_FUNCTION_DECL] = 1;

  lang_hooks.init_ts ();
}


/* The name of the object as the assembler will see it (but before any
   translations made by ASM_OUTPUT_LABELREF).  Often this is the same
   as DECL_NAME.  It is an IDENTIFIER_NODE.  */
tree
decl_assembler_name (tree decl)
{
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    lang_hooks.set_decl_assembler_name (decl);
  return DECL_WITH_VIS_CHECK (decl)->decl_with_vis.assembler_name;
}

/* Compute the number of bytes occupied by a tree with code CODE.
   This function cannot be used for TREE_VEC, PHI_NODE, or STRING_CST
   codes, which are of variable length.  */
size_t
tree_code_size (enum tree_code code)
{
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:  /* A decl node */
      {
	switch (code)
	  {
	  case FIELD_DECL:
	    return sizeof (struct tree_field_decl);
	  case PARM_DECL:
	    return sizeof (struct tree_parm_decl);
	  case VAR_DECL:
	    return sizeof (struct tree_var_decl);
	  case LABEL_DECL:
	    return sizeof (struct tree_label_decl);
	  case RESULT_DECL:
	    return sizeof (struct tree_result_decl);
	  case CONST_DECL:
	    return sizeof (struct tree_const_decl);
	  case TYPE_DECL:
	    return sizeof (struct tree_type_decl);
	  case FUNCTION_DECL:
	    return sizeof (struct tree_function_decl);
	  case NAME_MEMORY_TAG:
	  case SYMBOL_MEMORY_TAG:
	    return sizeof (struct tree_memory_tag);
	  case STRUCT_FIELD_TAG:
	    return sizeof (struct tree_struct_field_tag);
	  default:
	    return sizeof (struct tree_decl_non_common);
	  }
      }

    case tcc_type:  /* a type node */
      return sizeof (struct tree_type);

    case tcc_reference:   /* a reference */
    case tcc_expression:  /* an expression */
    case tcc_statement:   /* an expression with side effects */
    case tcc_comparison:  /* a comparison expression */
    case tcc_unary:       /* a unary arithmetic expression */
    case tcc_binary:      /* a binary arithmetic expression */
      return (sizeof (struct tree_exp)
	      + (TREE_CODE_LENGTH (code) - 1) * sizeof (char *));

    case tcc_constant:  /* a constant */
      switch (code)
	{
	case INTEGER_CST:	return sizeof (struct tree_int_cst);
	case REAL_CST:		return sizeof (struct tree_real_cst);
	case COMPLEX_CST:	return sizeof (struct tree_complex);
	case VECTOR_CST:	return sizeof (struct tree_vector);
	case STRING_CST:	gcc_unreachable ();
	default:
	  return lang_hooks.tree_size (code);
	}

    case tcc_exceptional:  /* something random, like an identifier.  */
      switch (code)
	{
	case IDENTIFIER_NODE:	return lang_hooks.identifier_size;
	case TREE_LIST:		return sizeof (struct tree_list);

	case ERROR_MARK:
	case PLACEHOLDER_EXPR:	return sizeof (struct tree_common);

	case TREE_VEC:
	case OMP_CLAUSE:
	case PHI_NODE:		gcc_unreachable ();

	case SSA_NAME:		return sizeof (struct tree_ssa_name);

	case STATEMENT_LIST:	return sizeof (struct tree_statement_list);
	case BLOCK:		return sizeof (struct tree_block);
	case VALUE_HANDLE:	return sizeof (struct tree_value_handle);
	case CONSTRUCTOR:	return sizeof (struct tree_constructor);

	default:
	  return lang_hooks.tree_size (code);
	}

    default:
      gcc_unreachable ();
    }
}

/* Compute the number of bytes occupied by NODE.  This routine only
   looks at TREE_CODE, except for PHI_NODE and TREE_VEC nodes.  */
size_t
tree_size (tree node)
{
  enum tree_code code = TREE_CODE (node);
  switch (code)
    {
    case PHI_NODE:
      return (sizeof (struct tree_phi_node)
	      + (PHI_ARG_CAPACITY (node) - 1) * sizeof (struct phi_arg_d));

    case TREE_BINFO:
      return (offsetof (struct tree_binfo, base_binfos)
	      + VEC_embedded_size (tree, BINFO_N_BASE_BINFOS (node)));

    case TREE_VEC:
      return (sizeof (struct tree_vec)
	      + (TREE_VEC_LENGTH (node) - 1) * sizeof(char *));

    case STRING_CST:
      return TREE_STRING_LENGTH (node) + offsetof (struct tree_string, str) + 1;

    case OMP_CLAUSE:
      return (sizeof (struct tree_omp_clause)
	      + (omp_clause_num_ops[OMP_CLAUSE_CODE (node)] - 1)
	        * sizeof (tree));

    default:
      return tree_code_size (code);
    }
}

/* Return a newly allocated node of code CODE.  For decl and type
   nodes, some other fields are initialized.  The rest of the node is
   initialized to zero.  This function cannot be used for PHI_NODE,
   TREE_VEC or OMP_CLAUSE nodes, which is enforced by asserts in
   tree_code_size.

   Achoo!  I got a code in the node.  */

tree
make_node_stat (enum tree_code code MEM_STAT_DECL)
{
  tree t;
  enum tree_code_class type = TREE_CODE_CLASS (code);
  size_t length = tree_code_size (code);
#ifdef GATHER_STATISTICS
  tree_node_kind kind;

  switch (type)
    {
    case tcc_declaration:  /* A decl node */
      kind = d_kind;
      break;

    case tcc_type:  /* a type node */
      kind = t_kind;
      break;

    case tcc_statement:  /* an expression with side effects */
      kind = s_kind;
      break;

    case tcc_reference:  /* a reference */
      kind = r_kind;
      break;

    case tcc_expression:  /* an expression */
    case tcc_comparison:  /* a comparison expression */
    case tcc_unary:  /* a unary arithmetic expression */
    case tcc_binary:  /* a binary arithmetic expression */
      kind = e_kind;
      break;

    case tcc_constant:  /* a constant */
      kind = c_kind;
      break;

    case tcc_exceptional:  /* something random, like an identifier.  */
      switch (code)
	{
	case IDENTIFIER_NODE:
	  kind = id_kind;
	  break;

	case TREE_VEC:
	  kind = vec_kind;
	  break;

	case TREE_BINFO:
	  kind = binfo_kind;
	  break;

	case PHI_NODE:
	  kind = phi_kind;
	  break;

	case SSA_NAME:
	  kind = ssa_name_kind;
	  break;

	case BLOCK:
	  kind = b_kind;
	  break;

	case CONSTRUCTOR:
	  kind = constr_kind;
	  break;

	default:
	  kind = x_kind;
	  break;
	}
      break;
      
    default:
      gcc_unreachable ();
    }

  tree_node_counts[(int) kind]++;
  tree_node_sizes[(int) kind] += length;
#endif

  if (code == IDENTIFIER_NODE)
    t = ggc_alloc_zone_pass_stat (length, &tree_id_zone);
  else
    t = ggc_alloc_zone_pass_stat (length, &tree_zone);

  memset (t, 0, length);

  TREE_SET_CODE (t, code);

  switch (type)
    {
    case tcc_statement:
      TREE_SIDE_EFFECTS (t) = 1;
      break;

    case tcc_declaration:
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
	DECL_IN_SYSTEM_HEADER (t) = in_system_header;
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  if (code != FUNCTION_DECL)
	    DECL_ALIGN (t) = 1;
	  DECL_USER_ALIGN (t) = 0;	  
	  /* We have not yet computed the alias set for this declaration.  */
	  DECL_POINTER_ALIAS_SET (t) = -1;
	}
      DECL_SOURCE_LOCATION (t) = input_location;
      DECL_UID (t) = next_decl_uid++;

      break;

    case tcc_type:
      TYPE_UID (t) = next_type_uid++;
      TYPE_ALIGN (t) = BITS_PER_UNIT;
      TYPE_USER_ALIGN (t) = 0;
      TYPE_MAIN_VARIANT (t) = t;

      /* Default to no attributes for type, but let target change that.  */
      TYPE_ATTRIBUTES (t) = NULL_TREE;
      targetm.set_default_type_attributes (t);

      /* We have not yet computed the alias set for this type.  */
      TYPE_ALIAS_SET (t) = -1;
      break;

    case tcc_constant:
      TREE_CONSTANT (t) = 1;
      TREE_INVARIANT (t) = 1;
      break;

    case tcc_expression:
      switch (code)
	{
	case INIT_EXPR:
	case MODIFY_EXPR:
	case VA_ARG_EXPR:
	case PREDECREMENT_EXPR:
	case PREINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	case POSTINCREMENT_EXPR:
	  /* All of these have side-effects, no matter what their
	     operands are.  */
	  TREE_SIDE_EFFECTS (t) = 1;
	  break;

	default:
	  break;
	}
      break;

    default:
      /* Other classes need no special treatment.  */
      break;
    }

  return t;
}

/* Return a new node with the same contents as NODE except that its
   TREE_CHAIN is zero and it has a fresh uid.  */

/* KEY: Also gs_node translation related fields are 0, so that this new
   node is again translated to gs_t. */

tree
copy_node_stat (tree node MEM_STAT_DECL)
{
  tree t;
  enum tree_code code = TREE_CODE (node);
  size_t length;

  gcc_assert (code != STATEMENT_LIST);

  length = tree_size (node);
  t = ggc_alloc_zone_pass_stat (length, &tree_zone);
  memcpy (t, node, length);

  TREE_CHAIN (t) = 0;
  TREE_ASM_WRITTEN (t) = 0;
  TREE_VISITED (t) = 0;
  t->common.ann = 0;
#ifdef KEY
  if (flag_spin_file)
    TREE_TO_TRANSLATED_GS (t) = 0;
#endif

  if (TREE_CODE_CLASS (code) == tcc_declaration)
    {
      DECL_UID (t) = next_decl_uid++;
      if ((TREE_CODE (node) == PARM_DECL || TREE_CODE (node) == VAR_DECL)
	  && DECL_HAS_VALUE_EXPR_P (node))
	{
	  SET_DECL_VALUE_EXPR (t, DECL_VALUE_EXPR (node));
	  DECL_HAS_VALUE_EXPR_P (t) = 1;
	}
      if (TREE_CODE (node) == VAR_DECL && DECL_HAS_INIT_PRIORITY_P (node))
	{
	  SET_DECL_INIT_PRIORITY (t, DECL_INIT_PRIORITY (node));
	  DECL_HAS_INIT_PRIORITY_P (t) = 1;
	}
      if (TREE_CODE (node) == VAR_DECL && DECL_BASED_ON_RESTRICT_P (node))
	{
	  SET_DECL_RESTRICT_BASE (t, DECL_GET_RESTRICT_BASE (node));
	  DECL_BASED_ON_RESTRICT_P (t) = 1;
	}
    }
  else if (TREE_CODE_CLASS (code) == tcc_type)
    {
      TYPE_UID (t) = next_type_uid++;
      /* The following is so that the debug code for
	 the copy is different from the original type.
	 The two statements usually duplicate each other
	 (because they clear fields of the same union),
	 but the optimizer should catch that.  */
      TYPE_SYMTAB_POINTER (t) = 0;
      TYPE_SYMTAB_ADDRESS (t) = 0;
      
      /* Do not copy the values cache.  */
      if (TYPE_CACHED_VALUES_P(t))
	{
	  TYPE_CACHED_VALUES_P (t) = 0;
	  TYPE_CACHED_VALUES (t) = NULL_TREE;
	}
    }

  return t;
}

/* Return a copy of a chain of nodes, chained through the TREE_CHAIN field.
   For example, this can copy a list made of TREE_LIST nodes.  */

tree
copy_list (tree list)
{
  tree head;
  tree prev, next;

  if (list == 0)
    return 0;

  head = prev = copy_node (list);
  next = TREE_CHAIN (list);
  while (next)
    {
      TREE_CHAIN (prev) = copy_node (next);
      prev = TREE_CHAIN (prev);
      next = TREE_CHAIN (next);
    }
  return head;
}


/* Create an INT_CST node with a LOW value sign extended.  */

tree
build_int_cst (tree type, HOST_WIDE_INT low)
{
  return build_int_cst_wide (type, low, low < 0 ? -1 : 0);
}

/* Create an INT_CST node with a LOW value zero extended.  */

tree
build_int_cstu (tree type, unsigned HOST_WIDE_INT low)
{
  return build_int_cst_wide (type, low, 0);
}

/* Create an INT_CST node with a LOW value in TYPE.  The value is sign extended
   if it is negative.  This function is similar to build_int_cst, but
   the extra bits outside of the type precision are cleared.  Constants
   with these extra bits may confuse the fold so that it detects overflows
   even in cases when they do not occur, and in general should be avoided.
   We cannot however make this a default behavior of build_int_cst without
   more intrusive changes, since there are parts of gcc that rely on the extra
   precision of the integer constants.  */

tree
build_int_cst_type (tree type, HOST_WIDE_INT low)
{
  unsigned HOST_WIDE_INT val = (unsigned HOST_WIDE_INT) low;
  unsigned HOST_WIDE_INT hi, mask;
  unsigned bits;
  bool signed_p;
  bool negative;

  if (!type)
    type = integer_type_node;

  bits = TYPE_PRECISION (type);
  signed_p = !TYPE_UNSIGNED (type);

  if (bits >= HOST_BITS_PER_WIDE_INT)
    negative = (low < 0);
  else
    {
      /* If the sign bit is inside precision of LOW, use it to determine
	 the sign of the constant.  */
      negative = ((val >> (bits - 1)) & 1) != 0;

      /* Mask out the bits outside of the precision of the constant.  */
      mask = (((unsigned HOST_WIDE_INT) 2) << (bits - 1)) - 1;

      if (signed_p && negative)
	val |= ~mask;
      else
	val &= mask;
    }

  /* Determine the high bits.  */
  hi = (negative ? ~(unsigned HOST_WIDE_INT) 0 : 0);

  /* For unsigned type we need to mask out the bits outside of the type
     precision.  */
  if (!signed_p)
    {
      if (bits <= HOST_BITS_PER_WIDE_INT)
	hi = 0;
      else
	{
	  bits -= HOST_BITS_PER_WIDE_INT;
	  mask = (((unsigned HOST_WIDE_INT) 2) << (bits - 1)) - 1;
	  hi &= mask;
	}
    }

  return build_int_cst_wide (type, val, hi);
}

/* These are the hash table functions for the hash table of INTEGER_CST
   nodes of a sizetype.  */

/* Return the hash code code X, an INTEGER_CST.  */

static hashval_t
int_cst_hash_hash (const void *x)
{
  tree t = (tree) x;

  return (TREE_INT_CST_HIGH (t) ^ TREE_INT_CST_LOW (t)
	  ^ htab_hash_pointer (TREE_TYPE (t)));
}

/* Return nonzero if the value represented by *X (an INTEGER_CST tree node)
   is the same as that given by *Y, which is the same.  */

static int
int_cst_hash_eq (const void *x, const void *y)
{
  tree xt = (tree) x;
  tree yt = (tree) y;

  return (TREE_TYPE (xt) == TREE_TYPE (yt)
	  && TREE_INT_CST_HIGH (xt) == TREE_INT_CST_HIGH (yt)
	  && TREE_INT_CST_LOW (xt) == TREE_INT_CST_LOW (yt));
}

/* Create an INT_CST node of TYPE and value HI:LOW.  If TYPE is NULL,
   integer_type_node is used.  The returned node is always shared.
   For small integers we use a per-type vector cache, for larger ones
   we use a single hash table.  */

tree
build_int_cst_wide (tree type, unsigned HOST_WIDE_INT low, HOST_WIDE_INT hi)
{
  tree t;
  int ix = -1;
  int limit = 0;

  if (!type)
    type = integer_type_node;

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* Cache NULL pointer.  */
      if (!hi && !low)
	{
	  limit = 1;
	  ix = 0;
	}
      break;

    case BOOLEAN_TYPE:
      /* Cache false or true.  */
      limit = 2;
      if (!hi && low < 2)
	ix = low;
      break;

    case INTEGER_TYPE:
    case OFFSET_TYPE:
      if (TYPE_UNSIGNED (type))
	{
	  /* Cache 0..N */
	  limit = INTEGER_SHARE_LIMIT;
	  if (!hi && low < (unsigned HOST_WIDE_INT)INTEGER_SHARE_LIMIT)
	    ix = low;
	}
      else
	{
	  /* Cache -1..N */
	  limit = INTEGER_SHARE_LIMIT + 1;
	  if (!hi && low < (unsigned HOST_WIDE_INT)INTEGER_SHARE_LIMIT)
	    ix = low + 1;
	  else if (hi == -1 && low == -(unsigned HOST_WIDE_INT)1)
	    ix = 0;
	}
      break;
    default:
      break;
    }

  if (ix >= 0)
    {
      /* Look for it in the type's vector of small shared ints.  */
      if (!TYPE_CACHED_VALUES_P (type))
	{
	  TYPE_CACHED_VALUES_P (type) = 1;
	  TYPE_CACHED_VALUES (type) = make_tree_vec (limit);
	}

      t = TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix);
      if (t)
	{
	  /* Make sure no one is clobbering the shared constant.  */
	  gcc_assert (TREE_TYPE (t) == type);
	  gcc_assert (TREE_INT_CST_LOW (t) == low);
	  gcc_assert (TREE_INT_CST_HIGH (t) == hi);
	}
      else
	{
	  /* Create a new shared int.  */
	  t = make_node (INTEGER_CST);

	  TREE_INT_CST_LOW (t) = low;
	  TREE_INT_CST_HIGH (t) = hi;
	  TREE_TYPE (t) = type;
	  
	  TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix) = t;
	}
    }
  else
    {
      /* Use the cache of larger shared ints.  */
      void **slot;

      TREE_INT_CST_LOW (int_cst_node) = low;
      TREE_INT_CST_HIGH (int_cst_node) = hi;
      TREE_TYPE (int_cst_node) = type;

      slot = htab_find_slot (int_cst_hash_table, int_cst_node, INSERT);
      t = *slot;
      if (!t)
	{
	  /* Insert this one into the hash table.  */
	  t = int_cst_node;
	  *slot = t;
	  /* Make a new node for next time round.  */
	  int_cst_node = make_node (INTEGER_CST);
	}
    }

  return t;
}

/* Builds an integer constant in TYPE such that lowest BITS bits are ones
   and the rest are zeros.  */

tree
build_low_bits_mask (tree type, unsigned bits)
{
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT high;
  unsigned HOST_WIDE_INT all_ones = ~(unsigned HOST_WIDE_INT) 0;

  gcc_assert (bits <= TYPE_PRECISION (type));

  if (bits == TYPE_PRECISION (type)
      && !TYPE_UNSIGNED (type))
    {
      /* Sign extended all-ones mask.  */
      low = all_ones;
      high = -1;
    }
  else if (bits <= HOST_BITS_PER_WIDE_INT)
    {
      low = all_ones >> (HOST_BITS_PER_WIDE_INT - bits);
      high = 0;
    }
  else
    {
      bits -= HOST_BITS_PER_WIDE_INT;
      low = all_ones;
      high = all_ones >> (HOST_BITS_PER_WIDE_INT - bits);
    }

  return build_int_cst_wide (type, low, high);
}

/* Checks that X is integer constant that can be expressed in (unsigned)
   HOST_WIDE_INT without loss of precision.  */

bool
cst_and_fits_in_hwi (tree x)
{
  if (TREE_CODE (x) != INTEGER_CST)
    return false;

  if (TYPE_PRECISION (TREE_TYPE (x)) > HOST_BITS_PER_WIDE_INT)
    return false;

  return (TREE_INT_CST_HIGH (x) == 0
	  || TREE_INT_CST_HIGH (x) == -1);
}

/* Return a new VECTOR_CST node whose type is TYPE and whose values
   are in a list pointed to by VALS.  */

tree
build_vector (tree type, tree vals)
{
  tree v = make_node (VECTOR_CST);
  int over1 = 0, over2 = 0;
  tree link;

  TREE_VECTOR_CST_ELTS (v) = vals;
  TREE_TYPE (v) = type;

  /* Iterate through elements and check for overflow.  */
  for (link = vals; link; link = TREE_CHAIN (link))
    {
      tree value = TREE_VALUE (link);

      /* Don't crash if we get an address constant.  */
      if (!CONSTANT_CLASS_P (value))
	continue;

      over1 |= TREE_OVERFLOW (value);
      over2 |= TREE_CONSTANT_OVERFLOW (value);
    }

  TREE_OVERFLOW (v) = over1;
  TREE_CONSTANT_OVERFLOW (v) = over2;

  return v;
}

/* Return a new VECTOR_CST node whose type is TYPE and whose values
   are extracted from V, a vector of CONSTRUCTOR_ELT.  */

tree
build_vector_from_ctor (tree type, VEC(constructor_elt,gc) *v)
{
  tree list = NULL_TREE;
  unsigned HOST_WIDE_INT idx;
  tree value;

  FOR_EACH_CONSTRUCTOR_VALUE (v, idx, value)
    list = tree_cons (NULL_TREE, value, list);
  return build_vector (type, nreverse (list));
}

/* Return a new CONSTRUCTOR node whose type is TYPE and whose values
   are in the VEC pointed to by VALS.  */
tree
build_constructor (tree type, VEC(constructor_elt,gc) *vals)
{
  tree c = make_node (CONSTRUCTOR);
  TREE_TYPE (c) = type;
  CONSTRUCTOR_ELTS (c) = vals;
  return c;
}

/* Build a CONSTRUCTOR node made of a single initializer, with the specified
   INDEX and VALUE.  */
tree
build_constructor_single (tree type, tree index, tree value)
{
  VEC(constructor_elt,gc) *v;
  constructor_elt *elt;
  tree t;

  v = VEC_alloc (constructor_elt, gc, 1);
  elt = VEC_quick_push (constructor_elt, v, NULL);
  elt->index = index;
  elt->value = value;

  t = build_constructor (type, v);
  TREE_CONSTANT (t) = TREE_CONSTANT (value);
  return t;
}


/* Return a new CONSTRUCTOR node whose type is TYPE and whose values
   are in a list pointed to by VALS.  */
tree
build_constructor_from_list (tree type, tree vals)
{
  tree t, val;
  VEC(constructor_elt,gc) *v = NULL;
  bool constant_p = true;

  if (vals)
    {
      v = VEC_alloc (constructor_elt, gc, list_length (vals));
      for (t = vals; t; t = TREE_CHAIN (t))
	{
	  constructor_elt *elt = VEC_quick_push (constructor_elt, v, NULL);
	  val = TREE_VALUE (t);
	  elt->index = TREE_PURPOSE (t);
	  elt->value = val;
	  if (!TREE_CONSTANT (val))
	    constant_p = false;
	}
    }

  t = build_constructor (type, v);
  TREE_CONSTANT (t) = constant_p;
  return t;
}


/* Return a new REAL_CST node whose type is TYPE and value is D.  */

tree
build_real (tree type, REAL_VALUE_TYPE d)
{
  tree v;
  REAL_VALUE_TYPE *dp;
  int overflow = 0;

  /* ??? Used to check for overflow here via CHECK_FLOAT_TYPE.
     Consider doing it via real_convert now.  */

  v = make_node (REAL_CST);
  dp = ggc_alloc (sizeof (REAL_VALUE_TYPE));
  memcpy (dp, &d, sizeof (REAL_VALUE_TYPE));

  TREE_TYPE (v) = type;
  TREE_REAL_CST_PTR (v) = dp;
  TREE_OVERFLOW (v) = TREE_CONSTANT_OVERFLOW (v) = overflow;
  return v;
}

/* Return a new REAL_CST node whose type is TYPE
   and whose value is the integer value of the INTEGER_CST node I.  */

REAL_VALUE_TYPE
real_value_from_int_cst (tree type, tree i)
{
  REAL_VALUE_TYPE d;

  /* Clear all bits of the real value type so that we can later do
     bitwise comparisons to see if two values are the same.  */
  memset (&d, 0, sizeof d);

  real_from_integer (&d, type ? TYPE_MODE (type) : VOIDmode,
		     TREE_INT_CST_LOW (i), TREE_INT_CST_HIGH (i),
		     TYPE_UNSIGNED (TREE_TYPE (i)));
  return d;
}

/* Given a tree representing an integer constant I, return a tree
   representing the same value as a floating-point constant of type TYPE.  */

tree
build_real_from_int_cst (tree type, tree i)
{
  tree v;
  int overflow = TREE_OVERFLOW (i);

  v = build_real (type, real_value_from_int_cst (type, i));

  TREE_OVERFLOW (v) |= overflow;
  TREE_CONSTANT_OVERFLOW (v) |= overflow;
  return v;
}

/* Return a newly constructed STRING_CST node whose value is
   the LEN characters at STR.
   The TREE_TYPE is not initialized.  */

tree
build_string (int len, const char *str)
{
  tree s;
  size_t length;

  /* Do not waste bytes provided by padding of struct tree_string.  */
  length = len + offsetof (struct tree_string, str) + 1;

#ifdef GATHER_STATISTICS
  tree_node_counts[(int) c_kind]++;
  tree_node_sizes[(int) c_kind] += length;
#endif  

  s = ggc_alloc_tree (length);

  memset (s, 0, sizeof (struct tree_common));
  TREE_SET_CODE (s, STRING_CST);
  TREE_CONSTANT (s) = 1;
  TREE_INVARIANT (s) = 1;
  TREE_STRING_LENGTH (s) = len;
  memcpy ((char *) TREE_STRING_POINTER (s), str, len);
  ((char *) TREE_STRING_POINTER (s))[len] = '\0';

  return s;
}

/* Return a newly constructed COMPLEX_CST node whose value is
   specified by the real and imaginary parts REAL and IMAG.
   Both REAL and IMAG should be constant nodes.  TYPE, if specified,
   will be the type of the COMPLEX_CST; otherwise a new type will be made.  */

tree
build_complex (tree type, tree real, tree imag)
{
  tree t = make_node (COMPLEX_CST);

  TREE_REALPART (t) = real;
  TREE_IMAGPART (t) = imag;
  TREE_TYPE (t) = type ? type : build_complex_type (TREE_TYPE (real));
  TREE_OVERFLOW (t) = TREE_OVERFLOW (real) | TREE_OVERFLOW (imag);
  TREE_CONSTANT_OVERFLOW (t)
    = TREE_CONSTANT_OVERFLOW (real) | TREE_CONSTANT_OVERFLOW (imag);
  return t;
}

/* Return a constant of arithmetic type TYPE which is the
   multiplicative identity of the set TYPE.  */

tree
build_one_cst (tree type)
{
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE: case ENUMERAL_TYPE: case BOOLEAN_TYPE:
    case POINTER_TYPE: case REFERENCE_TYPE:
    case OFFSET_TYPE:
      return build_int_cst (type, 1);

    case REAL_TYPE:
      return build_real (type, dconst1);

    case VECTOR_TYPE:
      {
	tree scalar, cst;
	int i;

	scalar = build_one_cst (TREE_TYPE (type));

	/* Create 'vect_cst_ = {cst,cst,...,cst}'  */
	cst = NULL_TREE;
	for (i = TYPE_VECTOR_SUBPARTS (type); --i >= 0; )
	  cst = tree_cons (NULL_TREE, scalar, cst);

	return build_vector (type, cst);
      }

    case COMPLEX_TYPE:
      return build_complex (type,
			    build_one_cst (TREE_TYPE (type)),
			    fold_convert (TREE_TYPE (type), integer_zero_node));

    default:
      gcc_unreachable ();
    }
}

/* Build a BINFO with LEN language slots.  */

tree
make_tree_binfo_stat (unsigned base_binfos MEM_STAT_DECL)
{
  tree t;
  size_t length = (offsetof (struct tree_binfo, base_binfos)
		   + VEC_embedded_size (tree, base_binfos));

#ifdef GATHER_STATISTICS
  tree_node_counts[(int) binfo_kind]++;
  tree_node_sizes[(int) binfo_kind] += length;
#endif

  t = ggc_alloc_zone_pass_stat (length, &tree_zone);

  memset (t, 0, offsetof (struct tree_binfo, base_binfos));

  TREE_SET_CODE (t, TREE_BINFO);

  VEC_embedded_init (tree, BINFO_BASE_BINFOS (t), base_binfos);

  return t;
}


/* Build a newly constructed TREE_VEC node of length LEN.  */

tree
make_tree_vec_stat (int len MEM_STAT_DECL)
{
  tree t;
  int length = (len - 1) * sizeof (tree) + sizeof (struct tree_vec);

#ifdef GATHER_STATISTICS
  tree_node_counts[(int) vec_kind]++;
  tree_node_sizes[(int) vec_kind] += length;
#endif

  t = ggc_alloc_zone_pass_stat (length, &tree_zone);

  memset (t, 0, length);

  TREE_SET_CODE (t, TREE_VEC);
  TREE_VEC_LENGTH (t) = len;

  return t;
}

/* Return 1 if EXPR is the integer constant zero or a complex constant
   of zero.  */

int
integer_zerop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && TREE_INT_CST_LOW (expr) == 0
	   && TREE_INT_CST_HIGH (expr) == 0)
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && integer_zerop (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the integer constant one or the corresponding
   complex constant.  */

int
integer_onep (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && TREE_INT_CST_LOW (expr) == 1
	   && TREE_INT_CST_HIGH (expr) == 0)
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && integer_onep (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is an integer containing all 1's in as much precision as
   it contains.  Likewise for the corresponding complex constant.  */

int
integer_all_onesp (tree expr)
{
  int prec;
  int uns;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_all_onesp (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return 1;

  else if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  uns = TYPE_UNSIGNED (TREE_TYPE (expr));
  if (TREE_INT_CST_LOW (expr) == ~(unsigned HOST_WIDE_INT) 0
      && TREE_INT_CST_HIGH (expr) == -1)
    return 1;
  if (!uns)
    return 0;

  /* Note that using TYPE_PRECISION here is wrong.  We care about the
     actual bits, not the (arbitrary) range of the type.  */
  prec = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (expr)));
  if (prec >= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT high_value;
      int shift_amount;

      shift_amount = prec - HOST_BITS_PER_WIDE_INT;

      /* Can not handle precisions greater than twice the host int size.  */
      gcc_assert (shift_amount <= HOST_BITS_PER_WIDE_INT);
      if (shift_amount == HOST_BITS_PER_WIDE_INT)
	/* Shifting by the host word size is undefined according to the ANSI
	   standard, so we must handle this as a special case.  */
	high_value = -1;
      else
	high_value = ((HOST_WIDE_INT) 1 << shift_amount) - 1;

      return (TREE_INT_CST_LOW (expr) == ~(unsigned HOST_WIDE_INT) 0
	      && TREE_INT_CST_HIGH (expr) == high_value);
    }
  else
    return TREE_INT_CST_LOW (expr) == ((unsigned HOST_WIDE_INT) 1 << prec) - 1;
}

/* Return 1 if EXPR is an integer constant that is a power of 2 (i.e., has only
   one bit on).  */

int
integer_pow2p (tree expr)
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_pow2p (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return 1;

  if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));
  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  if (high == 0 && low == 0)
    return 0;

  return ((high == 0 && (low & (low - 1)) == 0)
	  || (low == 0 && (high & (high - 1)) == 0));
}

/* Return 1 if EXPR is an integer constant other than zero or a
   complex constant other than zero.  */

int
integer_nonzerop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && (TREE_INT_CST_LOW (expr) != 0
	       || TREE_INT_CST_HIGH (expr) != 0))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && (integer_nonzerop (TREE_REALPART (expr))
		  || integer_nonzerop (TREE_IMAGPART (expr)))));
}

/* Return the power of two represented by a tree node known to be a
   power of two.  */

int
tree_log2 (tree expr)
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));

  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  return (high != 0 ? HOST_BITS_PER_WIDE_INT + exact_log2 (high)
	  : exact_log2 (low));
}

/* Similar, but return the largest integer Y such that 2 ** Y is less
   than or equal to EXPR.  */

int
tree_floor_log2 (tree expr)
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));

  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  Ignore if type's precision hasn't been set
     since what we are doing is setting it.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT || prec == 0)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  return (high != 0 ? HOST_BITS_PER_WIDE_INT + floor_log2 (high)
	  : floor_log2 (low));
}

/* Return 1 if EXPR is the real constant zero.  */

int
real_zerop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst0))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_zerop (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant one in real or complex form.  */

int
real_onep (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst1))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_onep (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant two.  */

int
real_twop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst2))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_twop (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant minus one.  */

int
real_minus_onep (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconstm1))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_minus_onep (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Nonzero if EXP is a constant or a cast of a constant.  */

int
really_constant_p (tree exp)
{
  /* This is not quite the same as STRIP_NOPS.  It does more.  */
  while (TREE_CODE (exp) == NOP_EXPR
	 || TREE_CODE (exp) == CONVERT_EXPR
	 || TREE_CODE (exp) == NON_LVALUE_EXPR)
    exp = TREE_OPERAND (exp, 0);
  return TREE_CONSTANT (exp);
}

/* Return first list element whose TREE_VALUE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
value_member (tree elem, tree list)
{
  while (list)
    {
      if (elem == TREE_VALUE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return first list element whose TREE_PURPOSE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
purpose_member (tree elem, tree list)
{
  while (list)
    {
      if (elem == TREE_PURPOSE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return nonzero if ELEM is part of the chain CHAIN.  */

int
chain_member (tree elem, tree chain)
{
  while (chain)
    {
      if (elem == chain)
	return 1;
      chain = TREE_CHAIN (chain);
    }

  return 0;
}

/* Return the length of a chain of nodes chained through TREE_CHAIN.
   We expect a null pointer to mark the end of the chain.
   This is the Lisp primitive `length'.  */

int
list_length (tree t)
{
  tree p = t;
#ifdef ENABLE_TREE_CHECKING
  tree q = t;
#endif
  int len = 0;

  while (p)
    {
      p = TREE_CHAIN (p);
#ifdef ENABLE_TREE_CHECKING
      if (len % 2)
	q = TREE_CHAIN (q);
      gcc_assert (p != q);
#endif
      len++;
    }

  return len;
}

/* Returns the number of FIELD_DECLs in TYPE.  */

int
fields_length (tree type)
{
  tree t = TYPE_FIELDS (type);
  int count = 0;

  for (; t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) == FIELD_DECL)
      ++count;

  return count;
}

/* Concatenate two chains of nodes (chained through TREE_CHAIN)
   by modifying the last node in chain 1 to point to chain 2.
   This is the Lisp primitive `nconc'.  */

tree
chainon (tree op1, tree op2)
{
  tree t1;

  if (!op1)
    return op2;
  if (!op2)
    return op1;

  for (t1 = op1; TREE_CHAIN (t1); t1 = TREE_CHAIN (t1))
    continue;
  TREE_CHAIN (t1) = op2;

#ifdef ENABLE_TREE_CHECKING
  {
    tree t2;
    for (t2 = op2; t2; t2 = TREE_CHAIN (t2))
      gcc_assert (t2 != t1);
  }
#endif

  return op1;
}

/* Return the last node in a chain of nodes (chained through TREE_CHAIN).  */

tree
tree_last (tree chain)
{
  tree next;
  if (chain)
    while ((next = TREE_CHAIN (chain)))
      chain = next;
  return chain;
}

/* Reverse the order of elements in the chain T,
   and return the new head of the chain (old last element).  */

tree
nreverse (tree t)
{
  tree prev = 0, decl, next;
  for (decl = t; decl; decl = next)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE.  */

tree
build_tree_list_stat (tree parm, tree value MEM_STAT_DECL)
{
  tree t = make_node_stat (TREE_LIST PASS_MEM_STAT);
  TREE_PURPOSE (t) = parm;
  TREE_VALUE (t) = value;
  return t;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PURPOSE and VALUE
   and whose TREE_CHAIN is CHAIN.  */

tree
tree_cons_stat (tree purpose, tree value, tree chain MEM_STAT_DECL)
{
  tree node;

  node = ggc_alloc_zone_pass_stat (sizeof (struct tree_list), &tree_zone);

  memset (node, 0, sizeof (struct tree_common));

#ifdef GATHER_STATISTICS
  tree_node_counts[(int) x_kind]++;
  tree_node_sizes[(int) x_kind] += sizeof (struct tree_list);
#endif

  TREE_SET_CODE (node, TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;
  return node;
}


/* Return the size nominally occupied by an object of type TYPE
   when it resides in memory.  The value is measured in units of bytes,
   and its data type is that normally used for type sizes
   (which is the first type created by make_signed_type or
   make_unsigned_type).  */

tree
size_in_bytes (tree type)
{
  tree t;

  if (type == error_mark_node)
    return integer_zero_node;

  type = TYPE_MAIN_VARIANT (type);
  t = TYPE_SIZE_UNIT (type);

  if (t == 0)
    {
      lang_hooks.types.incomplete_type_error (NULL_TREE, type);
      return size_zero_node;
    }

  if (TREE_CODE (t) == INTEGER_CST)
    t = force_fit_type (t, 0, false, false);

  return t;
}

/* Return the size of TYPE (in bytes) as a wide integer
   or return -1 if the size can vary or is larger than an integer.  */

HOST_WIDE_INT
int_size_in_bytes (tree type)
{
  tree t;

  if (type == error_mark_node)
    return 0;

  type = TYPE_MAIN_VARIANT (type);
  t = TYPE_SIZE_UNIT (type);
  if (t == 0
      || TREE_CODE (t) != INTEGER_CST
      || TREE_INT_CST_HIGH (t) != 0
      /* If the result would appear negative, it's too big to represent.  */
      || (HOST_WIDE_INT) TREE_INT_CST_LOW (t) < 0)
    return -1;

  return TREE_INT_CST_LOW (t);
}

/* Return the maximum size of TYPE (in bytes) as a wide integer
   or return -1 if the size can vary or is larger than an integer.  */

HOST_WIDE_INT
max_int_size_in_bytes (tree type)
{
  HOST_WIDE_INT size = -1;
  tree size_tree;

  /* If this is an array type, check for a possible MAX_SIZE attached.  */

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      size_tree = TYPE_ARRAY_MAX_SIZE (type);

      if (size_tree && host_integerp (size_tree, 1))
	size = tree_low_cst (size_tree, 1);
    }

  /* If we still haven't been able to get a size, see if the language
     can compute a maximum size.  */

  if (size == -1)
    {
      size_tree = lang_hooks.types.max_size (type);

      if (size_tree && host_integerp (size_tree, 1))
	size = tree_low_cst (size_tree, 1);
    }

  return size;
}

/* Return the bit position of FIELD, in bits from the start of the record.
   This is a tree of type bitsizetype.  */

tree
bit_position (tree field)
{
  return bit_from_pos (DECL_FIELD_OFFSET (field),
		       DECL_FIELD_BIT_OFFSET (field));
}

/* Likewise, but return as an integer.  It must be representable in
   that way (since it could be a signed value, we don't have the
   option of returning -1 like int_size_in_byte can.  */

HOST_WIDE_INT
int_bit_position (tree field)
{
  return tree_low_cst (bit_position (field), 0);
}

/* Return the byte position of FIELD, in bytes from the start of the record.
   This is a tree of type sizetype.  */

tree
byte_position (tree field)
{
  return byte_from_pos (DECL_FIELD_OFFSET (field),
			DECL_FIELD_BIT_OFFSET (field));
}

/* Likewise, but return as an integer.  It must be representable in
   that way (since it could be a signed value, we don't have the
   option of returning -1 like int_size_in_byte can.  */

HOST_WIDE_INT
int_byte_position (tree field)
{
  return tree_low_cst (byte_position (field), 0);
}

/* Return the strictest alignment, in bits, that T is known to have.  */

unsigned int
expr_align (tree t)
{
  unsigned int align0, align1;

  switch (TREE_CODE (t))
    {
    case NOP_EXPR:  case CONVERT_EXPR:  case NON_LVALUE_EXPR:
      /* If we have conversions, we know that the alignment of the
	 object must meet each of the alignments of the types.  */
      align0 = expr_align (TREE_OPERAND (t, 0));
      align1 = TYPE_ALIGN (TREE_TYPE (t));
      return MAX (align0, align1);

    case SAVE_EXPR:         case COMPOUND_EXPR:       case MODIFY_EXPR:
    case INIT_EXPR:         case TARGET_EXPR:         case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
      /* These don't change the alignment of an object.  */
      return expr_align (TREE_OPERAND (t, 0));

    case COND_EXPR:
      /* The best we can do is say that the alignment is the least aligned
	 of the two arms.  */
      align0 = expr_align (TREE_OPERAND (t, 1));
      align1 = expr_align (TREE_OPERAND (t, 2));
      return MIN (align0, align1);

    case LABEL_DECL:     case CONST_DECL:
    case VAR_DECL:       case PARM_DECL:   case RESULT_DECL:
      if (DECL_ALIGN (t) != 0)
	return DECL_ALIGN (t);
      break;

    case FUNCTION_DECL:
      return FUNCTION_BOUNDARY;

    default:
      break;
    }

  /* Otherwise take the alignment from that of the type.  */
  return TYPE_ALIGN (TREE_TYPE (t));
}

/* Return, as a tree node, the number of elements for TYPE (which is an
   ARRAY_TYPE) minus one. This counts only elements of the top array.  */

tree
array_type_nelts (tree type)
{
  tree index_type, min, max;

  /* If they did it with unspecified bounds, then we should have already
     given an error about it before we got here.  */
  if (! TYPE_DOMAIN (type))
    return error_mark_node;

  index_type = TYPE_DOMAIN (type);
  min = TYPE_MIN_VALUE (index_type);
  max = TYPE_MAX_VALUE (index_type);

  return (integer_zerop (min)
	  ? max
	  : fold_build2 (MINUS_EXPR, TREE_TYPE (max), max, min));
}

/* If arg is static -- a reference to an object in static storage -- then
   return the object.  This is not the same as the C meaning of `static'.
   If arg isn't static, return NULL.  */

tree
staticp (tree arg)
{
  switch (TREE_CODE (arg))
    {
    case FUNCTION_DECL:
      /* Nested functions are static, even though taking their address will
	 involve a trampoline as we unnest the nested function and create
	 the trampoline on the tree level.  */
      return arg;

    case VAR_DECL:
      return ((TREE_STATIC (arg) || DECL_EXTERNAL (arg))
	      && ! DECL_THREAD_LOCAL_P (arg)
	      && ! DECL_DLLIMPORT_P (arg)
	      ? arg : NULL);

    case CONST_DECL:
      return ((TREE_STATIC (arg) || DECL_EXTERNAL (arg))
	      ? arg : NULL);

    case CONSTRUCTOR:
      return TREE_STATIC (arg) ? arg : NULL;

    case LABEL_DECL:
    case STRING_CST:
      return arg;

    case COMPONENT_REF:
      /* If the thing being referenced is not a field, then it is
	 something language specific.  */
      if (TREE_CODE (TREE_OPERAND (arg, 1)) != FIELD_DECL)
	return (*lang_hooks.staticp) (arg);

      /* If we are referencing a bitfield, we can't evaluate an
	 ADDR_EXPR at compile time and so it isn't a constant.  */
      if (DECL_BIT_FIELD (TREE_OPERAND (arg, 1)))
	return NULL;

      return staticp (TREE_OPERAND (arg, 0));

    case BIT_FIELD_REF:
      return NULL;

    case MISALIGNED_INDIRECT_REF:
    case ALIGN_INDIRECT_REF:
    case INDIRECT_REF:
      return TREE_CONSTANT (TREE_OPERAND (arg, 0)) ? arg : NULL;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (arg))) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg, 1)) == INTEGER_CST)
	return staticp (TREE_OPERAND (arg, 0));
      else
	return false;

    default:
      if ((unsigned int) TREE_CODE (arg)
	  >= (unsigned int) LAST_AND_UNUSED_TREE_CODE)
	return lang_hooks.staticp (arg);
      else
	return NULL;
    }
}

/* Wrap a SAVE_EXPR around EXPR, if appropriate.
   Do this to any expression which may be used in more than one place,
   but must be evaluated only once.

   Normally, expand_expr would reevaluate the expression each time.
   Calling save_expr produces something that is evaluated and recorded
   the first time expand_expr is called on it.  Subsequent calls to
   expand_expr just reuse the recorded value.

   The call to expand_expr that generates code that actually computes
   the value is the first call *at compile time*.  Subsequent calls
   *at compile time* generate code to use the saved value.
   This produces correct result provided that *at run time* control
   always flows through the insns made by the first expand_expr
   before reaching the other places where the save_expr was evaluated.
   You, the caller of save_expr, must make sure this is so.

   Constants, and certain read-only nodes, are returned with no
   SAVE_EXPR because that is safe.  Expressions containing placeholders
   are not touched; see tree.def for an explanation of what these
   are used for.  */

tree
save_expr (tree expr)
{
  tree t = fold (expr);
  tree inner;

  /* If the tree evaluates to a constant, then we don't want to hide that
     fact (i.e. this allows further folding, and direct checks for constants).
     However, a read-only object that has side effects cannot be bypassed.
     Since it is no problem to reevaluate literals, we just return the
     literal node.  */
  inner = skip_simple_arithmetic (t);

  if (TREE_INVARIANT (inner)
      || (TREE_READONLY (inner) && ! TREE_SIDE_EFFECTS (inner))
      || TREE_CODE (inner) == SAVE_EXPR
      || TREE_CODE (inner) == ERROR_MARK)
    return t;

  /* If INNER contains a PLACEHOLDER_EXPR, we must evaluate it each time, since
     it means that the size or offset of some field of an object depends on
     the value within another field.

     Note that it must not be the case that T contains both a PLACEHOLDER_EXPR
     and some variable since it would then need to be both evaluated once and
     evaluated more than once.  Front-ends must assure this case cannot
     happen by surrounding any such subexpressions in their own SAVE_EXPR
     and forcing evaluation at the proper time.  */
  if (contains_placeholder_p (inner))
    return t;

  t = build1 (SAVE_EXPR, TREE_TYPE (expr), t);

  /* This expression might be placed ahead of a jump to ensure that the
     value was computed on both sides of the jump.  So make sure it isn't
     eliminated as dead.  */
  TREE_SIDE_EFFECTS (t) = 1;
  TREE_INVARIANT (t) = 1;
  return t;
}

/* Look inside EXPR and into any simple arithmetic operations.  Return
   the innermost non-arithmetic node.  */

tree
skip_simple_arithmetic (tree expr)
{
  tree inner;

  /* We don't care about whether this can be used as an lvalue in this
     context.  */
  while (TREE_CODE (expr) == NON_LVALUE_EXPR)
    expr = TREE_OPERAND (expr, 0);

  /* If we have simple operations applied to a SAVE_EXPR or to a SAVE_EXPR and
     a constant, it will be more efficient to not make another SAVE_EXPR since
     it will allow better simplification and GCSE will be able to merge the
     computations if they actually occur.  */
  inner = expr;
  while (1)
    {
      if (UNARY_CLASS_P (inner))
	inner = TREE_OPERAND (inner, 0);
      else if (BINARY_CLASS_P (inner))
	{
	  if (TREE_INVARIANT (TREE_OPERAND (inner, 1)))
	    inner = TREE_OPERAND (inner, 0);
	  else if (TREE_INVARIANT (TREE_OPERAND (inner, 0)))
	    inner = TREE_OPERAND (inner, 1);
	  else
	    break;
	}
      else
	break;
    }

  return inner;
}

/* Return which tree structure is used by T.  */

enum tree_node_structure_enum
tree_node_structure (tree t)
{
  enum tree_code code = TREE_CODE (t);

  switch (TREE_CODE_CLASS (code))
    {      
    case tcc_declaration:
      {
	switch (code)
	  {
	  case FIELD_DECL:
	    return TS_FIELD_DECL;
	  case PARM_DECL:
	    return TS_PARM_DECL;
	  case VAR_DECL:
	    return TS_VAR_DECL;
	  case LABEL_DECL:
	    return TS_LABEL_DECL;
	  case RESULT_DECL:
	    return TS_RESULT_DECL;
	  case CONST_DECL:
	    return TS_CONST_DECL;
	  case TYPE_DECL:
	    return TS_TYPE_DECL;
	  case FUNCTION_DECL:
	    return TS_FUNCTION_DECL;
	  case SYMBOL_MEMORY_TAG:
	  case NAME_MEMORY_TAG:
	  case STRUCT_FIELD_TAG:
	    return TS_MEMORY_TAG;
	  default:
	    return TS_DECL_NON_COMMON;
	  }
      }
    case tcc_type:
      return TS_TYPE;
    case tcc_reference:
    case tcc_comparison:
    case tcc_unary:
    case tcc_binary:
    case tcc_expression:
    case tcc_statement:
      return TS_EXP;
    default:  /* tcc_constant and tcc_exceptional */
      break;
    }
  switch (code)
    {
      /* tcc_constant cases.  */
    case INTEGER_CST:		return TS_INT_CST;
    case REAL_CST:		return TS_REAL_CST;
    case COMPLEX_CST:		return TS_COMPLEX;
    case VECTOR_CST:		return TS_VECTOR;
    case STRING_CST:		return TS_STRING;
      /* tcc_exceptional cases.  */
    case ERROR_MARK:		return TS_COMMON;
    case IDENTIFIER_NODE:	return TS_IDENTIFIER;
    case TREE_LIST:		return TS_LIST;
    case TREE_VEC:		return TS_VEC;
    case PHI_NODE:		return TS_PHI_NODE;
    case SSA_NAME:		return TS_SSA_NAME;
    case PLACEHOLDER_EXPR:	return TS_COMMON;
    case STATEMENT_LIST:	return TS_STATEMENT_LIST;
    case BLOCK:			return TS_BLOCK;
    case CONSTRUCTOR:		return TS_CONSTRUCTOR;
    case TREE_BINFO:		return TS_BINFO;
    case VALUE_HANDLE:		return TS_VALUE_HANDLE;
    case OMP_CLAUSE:		return TS_OMP_CLAUSE;

    default:
      gcc_unreachable ();
    }
}

/* Return 1 if EXP contains a PLACEHOLDER_EXPR; i.e., if it represents a size
   or offset that depends on a field within a record.  */

bool
contains_placeholder_p (tree exp)
{
  enum tree_code code;

  if (!exp)
    return 0;

  code = TREE_CODE (exp);
  if (code == PLACEHOLDER_EXPR)
    return 1;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_reference:
      /* Don't look at any PLACEHOLDER_EXPRs that might be in index or bit
	 position computations since they will be converted into a
	 WITH_RECORD_EXPR involving the reference, which will assume
	 here will be valid.  */
      return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0));

    case tcc_exceptional:
      if (code == TREE_LIST)
	return (CONTAINS_PLACEHOLDER_P (TREE_VALUE (exp))
		|| CONTAINS_PLACEHOLDER_P (TREE_CHAIN (exp)));
      break;

    case tcc_unary:
    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
      switch (code)
	{
	case COMPOUND_EXPR:
	  /* Ignoring the first operand isn't quite right, but works best.  */
	  return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1));

	case COND_EXPR:
	  return (CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 2)));

	case CALL_EXPR:
	  return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1));

	default:
	  break;
	}

      switch (TREE_CODE_LENGTH (code))
	{
	case 1:
	  return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0));
	case 2:
	  return (CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1)));
	default:
	  return 0;
	}

    default:
      return 0;
    }
  return 0;
}

/* Return true if any part of the computation of TYPE involves a
   PLACEHOLDER_EXPR.  This includes size, bounds, qualifiers
   (for QUAL_UNION_TYPE) and field positions.  */

static bool
type_contains_placeholder_1 (tree type)
{
  /* If the size contains a placeholder or the parent type (component type in
     the case of arrays) type involves a placeholder, this type does.  */
  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (type))
      || CONTAINS_PLACEHOLDER_P (TYPE_SIZE_UNIT (type))
      || (TREE_TYPE (type) != 0
	  && type_contains_placeholder_p (TREE_TYPE (type))))
    return true;

  /* Now do type-specific checks.  Note that the last part of the check above
     greatly limits what we have to do below.  */
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case COMPLEX_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_TYPE:
    case OFFSET_TYPE:
    case REFERENCE_TYPE:
    case METHOD_TYPE:
    case FUNCTION_TYPE:
    case VECTOR_TYPE:
      return false;

    case INTEGER_TYPE:
    case REAL_TYPE:
      /* Here we just check the bounds.  */
      return (CONTAINS_PLACEHOLDER_P (TYPE_MIN_VALUE (type))
	      || CONTAINS_PLACEHOLDER_P (TYPE_MAX_VALUE (type)));

    case ARRAY_TYPE:
      /* We're already checked the component type (TREE_TYPE), so just check
	 the index type.  */
      return type_contains_placeholder_p (TYPE_DOMAIN (type));

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree field;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  if (TREE_CODE (field) == FIELD_DECL
	      && (CONTAINS_PLACEHOLDER_P (DECL_FIELD_OFFSET (field))
		  || (TREE_CODE (type) == QUAL_UNION_TYPE
		      && CONTAINS_PLACEHOLDER_P (DECL_QUALIFIER (field)))
		  || type_contains_placeholder_p (TREE_TYPE (field))))
	    return true;

	return false;
      }

    default:
      gcc_unreachable ();
    }
}

bool
type_contains_placeholder_p (tree type)
{
  bool result;

  /* If the contains_placeholder_bits field has been initialized,
     then we know the answer.  */
  if (TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) > 0)
    return TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) - 1;

  /* Indicate that we've seen this type node, and the answer is false.
     This is what we want to return if we run into recursion via fields.  */
  TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) = 1;

  /* Compute the real value.  */
  result = type_contains_placeholder_1 (type);

  /* Store the real value.  */
  TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) = result + 1;

  return result;
}

/* Given a tree EXP, a FIELD_DECL F, and a replacement value R,
   return a tree with all occurrences of references to F in a
   PLACEHOLDER_EXPR replaced by R.   Note that we assume here that EXP
   contains only arithmetic expressions or a CALL_EXPR with a
   PLACEHOLDER_EXPR occurring only in its arglist.  */

tree
substitute_in_expr (tree exp, tree f, tree r)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2, op3;
  tree new;
  tree inner;

  /* We handle TREE_LIST and COMPONENT_REF separately.  */
  if (code == TREE_LIST)
    {
      op0 = SUBSTITUTE_IN_EXPR (TREE_CHAIN (exp), f, r);
      op1 = SUBSTITUTE_IN_EXPR (TREE_VALUE (exp), f, r);
      if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	return exp;

      return tree_cons (TREE_PURPOSE (exp), op1, op0);
    }
  else if (code == COMPONENT_REF)
   {
     /* If this expression is getting a value from a PLACEHOLDER_EXPR
	and it is the right field, replace it with R.  */
     for (inner = TREE_OPERAND (exp, 0);
	  REFERENCE_CLASS_P (inner);
	  inner = TREE_OPERAND (inner, 0))
       ;
     if (TREE_CODE (inner) == PLACEHOLDER_EXPR
	 && TREE_OPERAND (exp, 1) == f)
       return r;

     /* If this expression hasn't been completed let, leave it alone.  */
     if (TREE_CODE (inner) == PLACEHOLDER_EXPR && TREE_TYPE (inner) == 0)
       return exp;

     op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
     if (op0 == TREE_OPERAND (exp, 0))
       return exp;

     new = fold_build3 (COMPONENT_REF, TREE_TYPE (exp),
			op0, TREE_OPERAND (exp, 1), NULL_TREE);
   }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
      case tcc_declaration:
	return exp;

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_expression:
      case tcc_reference:
	switch (TREE_CODE_LENGTH (code))
	  {
	  case 0:
	    return exp;

	  case 1:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    if (op0 == TREE_OPERAND (exp, 0))
	      return exp;

	    new = fold_build1 (code, TREE_TYPE (exp), op0);
	    break;

	  case 2:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    op1 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 1), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	      return exp;

	    new = fold_build2 (code, TREE_TYPE (exp), op0, op1);
	    break;

	  case 3:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    op1 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 1), f, r);
	    op2 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 2), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2))
	      return exp;

	    new = fold_build3 (code, TREE_TYPE (exp), op0, op1, op2);
	    break;

	  case 4:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    op1 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 1), f, r);
	    op2 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 2), f, r);
	    op3 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 3), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2)
		&& op3 == TREE_OPERAND (exp, 3))
	      return exp;

	    new = fold (build4 (code, TREE_TYPE (exp), op0, op1, op2, op3));
	    break;

	  default:
	    gcc_unreachable ();
	  }
	break;

      default:
	gcc_unreachable ();
      }

  TREE_READONLY (new) = TREE_READONLY (exp);
  return new;
}

/* Similar, but look for a PLACEHOLDER_EXPR in EXP and find a replacement
   for it within OBJ, a tree that is an object or a chain of references.  */

tree
substitute_placeholder_in_expr (tree exp, tree obj)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2, op3;

  /* If this is a PLACEHOLDER_EXPR, see if we find a corresponding type
     in the chain of OBJ.  */
  if (code == PLACEHOLDER_EXPR)
    {
      tree need_type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));
      tree elt;

      for (elt = obj; elt != 0;
	   elt = ((TREE_CODE (elt) == COMPOUND_EXPR
		   || TREE_CODE (elt) == COND_EXPR)
		  ? TREE_OPERAND (elt, 1)
		  : (REFERENCE_CLASS_P (elt)
		     || UNARY_CLASS_P (elt)
		     || BINARY_CLASS_P (elt)
		     || EXPRESSION_CLASS_P (elt))
		  ? TREE_OPERAND (elt, 0) : 0))
	if (TYPE_MAIN_VARIANT (TREE_TYPE (elt)) == need_type)
	  return elt;

      for (elt = obj; elt != 0;
	   elt = ((TREE_CODE (elt) == COMPOUND_EXPR
		   || TREE_CODE (elt) == COND_EXPR)
		  ? TREE_OPERAND (elt, 1)
		  : (REFERENCE_CLASS_P (elt)
		     || UNARY_CLASS_P (elt)
		     || BINARY_CLASS_P (elt)
		     || EXPRESSION_CLASS_P (elt))
		  ? TREE_OPERAND (elt, 0) : 0))
	if (POINTER_TYPE_P (TREE_TYPE (elt))
	    && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (elt)))
		== need_type))
	  return fold_build1 (INDIRECT_REF, need_type, elt);

      /* If we didn't find it, return the original PLACEHOLDER_EXPR.  If it
	 survives until RTL generation, there will be an error.  */
      return exp;
    }

  /* TREE_LIST is special because we need to look at TREE_VALUE
     and TREE_CHAIN, not TREE_OPERANDS.  */
  else if (code == TREE_LIST)
    {
      op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_CHAIN (exp), obj);
      op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_VALUE (exp), obj);
      if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	return exp;

      return tree_cons (TREE_PURPOSE (exp), op1, op0);
    }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
      case tcc_declaration:
	return exp;

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_expression:
      case tcc_reference:
      case tcc_statement:
	switch (TREE_CODE_LENGTH (code))
	  {
	  case 0:
	    return exp;

	  case 1:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    if (op0 == TREE_OPERAND (exp, 0))
	      return exp;
	    else
	      return fold_build1 (code, TREE_TYPE (exp), op0);

	  case 2:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 1), obj);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	      return exp;
	    else
	      return fold_build2 (code, TREE_TYPE (exp), op0, op1);

	  case 3:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 1), obj);
	    op2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 2), obj);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2))
	      return exp;
	    else
	      return fold_build3 (code, TREE_TYPE (exp), op0, op1, op2);

	  case 4:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 1), obj);
	    op2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 2), obj);
	    op3 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 3), obj);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2)
		&& op3 == TREE_OPERAND (exp, 3))
	      return exp;
	    else
	      return fold (build4 (code, TREE_TYPE (exp), op0, op1, op2, op3));

	  default:
	    gcc_unreachable ();
	  }
	break;

      default:
	gcc_unreachable ();
      }
}

/* Stabilize a reference so that we can use it any number of times
   without causing its operands to be evaluated more than once.
   Returns the stabilized reference.  This works by means of save_expr,
   so see the caveats in the comments about save_expr.

   Also allows conversion expressions whose operands are references.
   Any other kind of expression is returned unchanged.  */

tree
stabilize_reference (tree ref)
{
  tree result;
  enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* No action is needed in this case.  */
      return ref;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      result = build_nt (code, stabilize_reference (TREE_OPERAND (ref, 0)));
      break;

    case INDIRECT_REF:
      result = build_nt (INDIRECT_REF,
			 stabilize_reference_1 (TREE_OPERAND (ref, 0)));
      break;

    case COMPONENT_REF:
      result = build_nt (COMPONENT_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 TREE_OPERAND (ref, 1), NULL_TREE);
      break;

    case BIT_FIELD_REF:
      result = build_nt (BIT_FIELD_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 2)));
      break;

    case ARRAY_REF:
      result = build_nt (ARRAY_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 TREE_OPERAND (ref, 2), TREE_OPERAND (ref, 3));
      break;

    case ARRAY_RANGE_REF:
      result = build_nt (ARRAY_RANGE_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 TREE_OPERAND (ref, 2), TREE_OPERAND (ref, 3));
      break;

    case COMPOUND_EXPR:
      /* We cannot wrap the first expression in a SAVE_EXPR, as then
	 it wouldn't be ignored.  This matters when dealing with
	 volatiles.  */
      return stabilize_reference_1 (ref);

      /* If arg isn't a kind of lvalue we recognize, make no change.
	 Caller should recognize the error for an invalid lvalue.  */
    default:
      return ref;

    case ERROR_MARK:
      return error_mark_node;
    }

  TREE_TYPE (result) = TREE_TYPE (ref);
  TREE_READONLY (result) = TREE_READONLY (ref);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (ref);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (ref);

  return result;
}

/* Subroutine of stabilize_reference; this is called for subtrees of
   references.  Any expression with side-effects must be put in a SAVE_EXPR
   to ensure that it is only evaluated once.

   We don't put SAVE_EXPR nodes around everything, because assigning very
   simple expressions to temporaries causes us to miss good opportunities
   for optimizations.  Among other things, the opportunity to fold in the
   addition of a constant into an addressing mode often gets lost, e.g.
   "y[i+1] += x;".  In general, we take the approach that we should not make
   an assignment unless we are forced into it - i.e., that any non-side effect
   operator should be allowed, and that cse should take care of coalescing
   multiple utterances of the same expression should that prove fruitful.  */

tree
stabilize_reference_1 (tree e)
{
  tree result;
  enum tree_code code = TREE_CODE (e);

  /* We cannot ignore const expressions because it might be a reference
     to a const array but whose index contains side-effects.  But we can
     ignore things that are actual constant or that already have been
     handled by this function.  */

  if (TREE_INVARIANT (e))
    return e;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_exceptional:
    case tcc_type:
    case tcc_declaration:
    case tcc_comparison:
    case tcc_statement:
    case tcc_expression:
    case tcc_reference:
      /* If the expression has side-effects, then encase it in a SAVE_EXPR
	 so that it will only be evaluated once.  */
      /* The reference (r) and comparison (<) classes could be handled as
	 below, but it is generally faster to only evaluate them once.  */
      if (TREE_SIDE_EFFECTS (e))
	return save_expr (e);
      return e;

    case tcc_constant:
      /* Constants need no processing.  In fact, we should never reach
	 here.  */
      return e;

    case tcc_binary:
      /* Division is slow and tends to be compiled with jumps,
	 especially the division by powers of 2 that is often
	 found inside of an array reference.  So do it just once.  */
      if (code == TRUNC_DIV_EXPR || code == TRUNC_MOD_EXPR
	  || code == FLOOR_DIV_EXPR || code == FLOOR_MOD_EXPR
	  || code == CEIL_DIV_EXPR || code == CEIL_MOD_EXPR
	  || code == ROUND_DIV_EXPR || code == ROUND_MOD_EXPR)
	return save_expr (e);
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)),
			 stabilize_reference_1 (TREE_OPERAND (e, 1)));
      break;

    case tcc_unary:
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)));
      break;

    default:
      gcc_unreachable ();
    }

  TREE_TYPE (result) = TREE_TYPE (e);
  TREE_READONLY (result) = TREE_READONLY (e);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (e);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (e);
  TREE_INVARIANT (result) = 1;

  return result;
}

/* Low-level constructors for expressions.  */

/* A helper function for build1 and constant folders.  Set TREE_CONSTANT,
   TREE_INVARIANT, and TREE_SIDE_EFFECTS for an ADDR_EXPR.  */

void
recompute_tree_invariant_for_addr_expr (tree t)
{
  tree node;
  bool tc = true, ti = true, se = false;

  /* We started out assuming this address is both invariant and constant, but
     does not have side effects.  Now go down any handled components and see if
     any of them involve offsets that are either non-constant or non-invariant.
     Also check for side-effects.

     ??? Note that this code makes no attempt to deal with the case where
     taking the address of something causes a copy due to misalignment.  */

#define UPDATE_TITCSE(NODE)  \
do { tree _node = (NODE); \
     if (_node && !TREE_INVARIANT (_node)) ti = false; \
     if (_node && !TREE_CONSTANT (_node)) tc = false; \
     if (_node && TREE_SIDE_EFFECTS (_node)) se = true; } while (0)

  for (node = TREE_OPERAND (t, 0); handled_component_p (node);
       node = TREE_OPERAND (node, 0))
    {
      /* If the first operand doesn't have an ARRAY_TYPE, this is a bogus
	 array reference (probably made temporarily by the G++ front end),
	 so ignore all the operands.  */
      if ((TREE_CODE (node) == ARRAY_REF
	   || TREE_CODE (node) == ARRAY_RANGE_REF)
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (node, 0))) == ARRAY_TYPE)
	{
	  UPDATE_TITCSE (TREE_OPERAND (node, 1));
	  if (TREE_OPERAND (node, 2))
	    UPDATE_TITCSE (TREE_OPERAND (node, 2));
	  if (TREE_OPERAND (node, 3))
	    UPDATE_TITCSE (TREE_OPERAND (node, 3));
	}
      /* Likewise, just because this is a COMPONENT_REF doesn't mean we have a
	 FIELD_DECL, apparently.  The G++ front end can put something else
	 there, at least temporarily.  */
      else if (TREE_CODE (node) == COMPONENT_REF
	       && TREE_CODE (TREE_OPERAND (node, 1)) == FIELD_DECL)
	{
	  if (TREE_OPERAND (node, 2))
	    UPDATE_TITCSE (TREE_OPERAND (node, 2));
	}
      else if (TREE_CODE (node) == BIT_FIELD_REF)
	UPDATE_TITCSE (TREE_OPERAND (node, 2));
    }

  node = lang_hooks.expr_to_decl (node, &tc, &ti, &se);

  /* Now see what's inside.  If it's an INDIRECT_REF, copy our properties from
     the address, since &(*a)->b is a form of addition.  If it's a decl, it's
     invariant and constant if the decl is static.  It's also invariant if it's
     a decl in the current function.  Taking the address of a volatile variable
     is not volatile.  If it's a constant, the address is both invariant and
     constant.  Otherwise it's neither.  */
  if (TREE_CODE (node) == INDIRECT_REF)
    UPDATE_TITCSE (TREE_OPERAND (node, 0));
  else if (DECL_P (node))
    {
      if (staticp (node))
	;
      else if (decl_function_context (node) == current_function_decl
	       /* Addresses of thread-local variables are invariant.  */
	       || (TREE_CODE (node) == VAR_DECL
		   && DECL_THREAD_LOCAL_P (node)))
	tc = false;
      else
	ti = tc = false;
    }
  else if (CONSTANT_CLASS_P (node))
    ;
  else
    {
      ti = tc = false;
      se |= TREE_SIDE_EFFECTS (node);
    }

  TREE_CONSTANT (t) = tc;
  TREE_INVARIANT (t) = ti;
  TREE_SIDE_EFFECTS (t) = se;
#undef UPDATE_TITCSE
}

/* Build an expression of code CODE, data type TYPE, and operands as
   specified.  Expressions and reference nodes can be created this way.
   Constants, decls, types and misc nodes cannot be.

   We define 5 non-variadic functions, from 0 to 4 arguments.  This is
   enough for all extant tree codes.  */

tree
build0_stat (enum tree_code code, tree tt MEM_STAT_DECL)
{
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 0);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  return t;
}

tree
build1_stat (enum tree_code code, tree type, tree node MEM_STAT_DECL)
{
  int length = sizeof (struct tree_exp);
#ifdef GATHER_STATISTICS
  tree_node_kind kind;
#endif
  tree t;

#ifdef GATHER_STATISTICS
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_statement:  /* an expression with side effects */
      kind = s_kind;
      break;
    case tcc_reference:  /* a reference */
      kind = r_kind;
      break;
    default:
      kind = e_kind;
      break;
    }

  tree_node_counts[(int) kind]++;
  tree_node_sizes[(int) kind] += length;
#endif

  gcc_assert (TREE_CODE_LENGTH (code) == 1);

  t = ggc_alloc_zone_pass_stat (length, &tree_zone);

  memset (t, 0, sizeof (struct tree_common));

  TREE_SET_CODE (t, code);

  TREE_TYPE (t) = type;
#ifdef USE_MAPPED_LOCATION
  SET_EXPR_LOCATION (t, UNKNOWN_LOCATION);
#else
  SET_EXPR_LOCUS (t, NULL);
#endif
  TREE_COMPLEXITY (t) = 0;
  TREE_OPERAND (t, 0) = node;
  TREE_BLOCK (t) = NULL_TREE;
  if (node && !TYPE_P (node))
    {
      TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (node);
      TREE_READONLY (t) = TREE_READONLY (node);
    }

  if (TREE_CODE_CLASS (code) == tcc_statement)
    TREE_SIDE_EFFECTS (t) = 1;
  else switch (code)
    {
    case VA_ARG_EXPR:
      /* All of these have side-effects, no matter what their
	 operands are.  */
      TREE_SIDE_EFFECTS (t) = 1;
      TREE_READONLY (t) = 0;
      break;

    case MISALIGNED_INDIRECT_REF:
    case ALIGN_INDIRECT_REF:
    case INDIRECT_REF:
      /* Whether a dereference is readonly has nothing to do with whether
	 its operand is readonly.  */
      TREE_READONLY (t) = 0;
      break;

    case ADDR_EXPR:
      if (node)
	recompute_tree_invariant_for_addr_expr (t);
      break;

    default:
      if ((TREE_CODE_CLASS (code) == tcc_unary || code == VIEW_CONVERT_EXPR)
	  && node && !TYPE_P (node)
	  && TREE_CONSTANT (node))
	TREE_CONSTANT (t) = 1;
      if ((TREE_CODE_CLASS (code) == tcc_unary || code == VIEW_CONVERT_EXPR)
	  && node && TREE_INVARIANT (node))
	TREE_INVARIANT (t) = 1;
      if (TREE_CODE_CLASS (code) == tcc_reference
	  && node && TREE_THIS_VOLATILE (node))
	TREE_THIS_VOLATILE (t) = 1;
      break;
    }

  return t;
}

#define PROCESS_ARG(N)			\
  do {					\
    TREE_OPERAND (t, N) = arg##N;	\
    if (arg##N &&!TYPE_P (arg##N))	\
      {					\
        if (TREE_SIDE_EFFECTS (arg##N))	\
	  side_effects = 1;		\
        if (!TREE_READONLY (arg##N))	\
	  read_only = 0;		\
        if (!TREE_CONSTANT (arg##N))	\
	  constant = 0;			\
	if (!TREE_INVARIANT (arg##N))	\
	  invariant = 0;		\
      }					\
  } while (0)

tree
build2_stat (enum tree_code code, tree tt, tree arg0, tree arg1 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects, invariant;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 2);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  /* Below, we automatically set TREE_SIDE_EFFECTS and TREE_READONLY for the
     result based on those same flags for the arguments.  But if the
     arguments aren't really even `tree' expressions, we shouldn't be trying
     to do this.  */

  /* Expressions without side effects may be constant if their
     arguments are as well.  */
  constant = (TREE_CODE_CLASS (code) == tcc_comparison
	      || TREE_CODE_CLASS (code) == tcc_binary);
  read_only = 1;
  side_effects = TREE_SIDE_EFFECTS (t);
  invariant = constant;

  PROCESS_ARG(0);
  PROCESS_ARG(1);

  TREE_READONLY (t) = read_only;
  TREE_CONSTANT (t) = constant;
  TREE_INVARIANT (t) = invariant;
  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_THIS_VOLATILE (t)
    = (TREE_CODE_CLASS (code) == tcc_reference
       && arg0 && TREE_THIS_VOLATILE (arg0));

  return t;
}

tree
build3_stat (enum tree_code code, tree tt, tree arg0, tree arg1,
	     tree arg2 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects, invariant;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 3);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG(0);
  PROCESS_ARG(1);
  PROCESS_ARG(2);

  if (code == CALL_EXPR && !side_effects)
    {
      tree node;
      int i;

      /* Calls have side-effects, except those to const or
	 pure functions.  */
      i = call_expr_flags (t);
      if (!(i & (ECF_CONST | ECF_PURE)))
	side_effects = 1;

      /* And even those have side-effects if their arguments do.  */
      else for (node = arg1; node; node = TREE_CHAIN (node))
	if (TREE_SIDE_EFFECTS (TREE_VALUE (node)))
	  {
	    side_effects = 1;
	    break;
	  }
    }

  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_THIS_VOLATILE (t)
    = (TREE_CODE_CLASS (code) == tcc_reference
       && arg0 && TREE_THIS_VOLATILE (arg0));

  return t;
}

tree
build4_stat (enum tree_code code, tree tt, tree arg0, tree arg1,
	     tree arg2, tree arg3 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects, invariant;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 4);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG(0);
  PROCESS_ARG(1);
  PROCESS_ARG(2);
  PROCESS_ARG(3);

  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_THIS_VOLATILE (t)
    = (TREE_CODE_CLASS (code) == tcc_reference
       && arg0 && TREE_THIS_VOLATILE (arg0));

  return t;
}

tree
build5_stat (enum tree_code code, tree tt, tree arg0, tree arg1,
	     tree arg2, tree arg3, tree arg4 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects, invariant;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 5);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG(0);
  PROCESS_ARG(1);
  PROCESS_ARG(2);
  PROCESS_ARG(3);
  PROCESS_ARG(4);

  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_THIS_VOLATILE (t)
    = (TREE_CODE_CLASS (code) == tcc_reference
       && arg0 && TREE_THIS_VOLATILE (arg0));

  return t;
}

tree
build7_stat (enum tree_code code, tree tt, tree arg0, tree arg1,
	     tree arg2, tree arg3, tree arg4, tree arg5,
	     tree arg6 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects, invariant;
  tree t;

  gcc_assert (code == TARGET_MEM_REF);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG(0);
  PROCESS_ARG(1);
  PROCESS_ARG(2);
  PROCESS_ARG(3);
  PROCESS_ARG(4);
  PROCESS_ARG(5);
  PROCESS_ARG(6);

  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_THIS_VOLATILE (t) = 0;

  return t;
}

/* Similar except don't specify the TREE_TYPE
   and leave the TREE_SIDE_EFFECTS as 0.
   It is permissible for arguments to be null,
   or even garbage if their values do not matter.  */

tree
build_nt (enum tree_code code, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  va_start (p, code);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  return t;
}

/* Create a DECL_... node of code CODE, name NAME and data type TYPE.
   We do NOT enter this node in any sort of symbol table.

   layout_decl is used to set up the decl's storage layout.
   Other slots are initialized to 0 or null pointers.  */

tree
build_decl_stat (enum tree_code code, tree name, tree type MEM_STAT_DECL)
{
  tree t;

  t = make_node_stat (code PASS_MEM_STAT);

/*  if (type == error_mark_node)
    type = integer_type_node; */
/* That is not done, deliberately, so that having error_mark_node
   as the type can suppress useless errors in the use of this variable.  */

  DECL_NAME (t) = name;
  TREE_TYPE (t) = type;

  if (code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
    layout_decl (t, 0);
  else if (code == FUNCTION_DECL)
    DECL_MODE (t) = FUNCTION_MODE;

  return t;
}

/* Builds and returns function declaration with NAME and TYPE.  */

tree
build_fn_decl (const char *name, tree type)
{
  tree id = get_identifier (name);
  tree decl = build_decl (FUNCTION_DECL, id, type);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  TREE_NOTHROW (decl) = 1;

  return decl;
}


/* BLOCK nodes are used to represent the structure of binding contours
   and declarations, once those contours have been exited and their contents
   compiled.  This information is used for outputting debugging info.  */

tree
build_block (tree vars, tree subblocks, tree supercontext, tree chain)
{
  tree block = make_node (BLOCK);

  BLOCK_VARS (block) = vars;
  BLOCK_SUBBLOCKS (block) = subblocks;
  BLOCK_SUPERCONTEXT (block) = supercontext;
  BLOCK_CHAIN (block) = chain;
  return block;
}

#if 1 /* ! defined(USE_MAPPED_LOCATION) */
/* ??? gengtype doesn't handle conditionals */
static GTY(()) source_locus last_annotated_node;
#endif

#ifdef USE_MAPPED_LOCATION

expanded_location
expand_location (source_location loc)
{
  expanded_location xloc;
  if (loc == 0) { xloc.file = NULL; xloc.line = 0;  xloc.column = 0; }
  else
    {
      const struct line_map *map = linemap_lookup (&line_table, loc);
      xloc.file = map->to_file;
      xloc.line = SOURCE_LINE (map, loc);
      xloc.column = SOURCE_COLUMN (map, loc);
    };
  return xloc;
}

#else

/* Record the exact location where an expression or an identifier were
   encountered.  */

void
annotate_with_file_line (tree node, const char *file, int line)
{
  /* Roughly one percent of the calls to this function are to annotate
     a node with the same information already attached to that node!
     Just return instead of wasting memory.  */
  if (EXPR_LOCUS (node)
      && EXPR_LINENO (node) == line
      && (EXPR_FILENAME (node) == file
	  || !strcmp (EXPR_FILENAME (node), file)))
    {
      last_annotated_node = EXPR_LOCUS (node);
      return;
    }

  /* In heavily macroized code (such as GCC itself) this single
     entry cache can reduce the number of allocations by more
     than half.  */
  if (last_annotated_node
      && last_annotated_node->line == line
      && (last_annotated_node->file == file
	  || !strcmp (last_annotated_node->file, file)))
    {
      SET_EXPR_LOCUS (node, last_annotated_node);
      return;
    }

  SET_EXPR_LOCUS (node, ggc_alloc (sizeof (location_t)));
  EXPR_LINENO (node) = line;
  EXPR_FILENAME (node) = file;
  last_annotated_node = EXPR_LOCUS (node);
}

void
annotate_with_locus (tree node, location_t locus)
{
  annotate_with_file_line (node, locus.file, locus.line);
}
#endif

/* Return a declaration like DDECL except that its DECL_ATTRIBUTES
   is ATTRIBUTE.  */

tree
build_decl_attribute_variant (tree ddecl, tree attribute)
{
  DECL_ATTRIBUTES (ddecl) = attribute;
  return ddecl;
}

/* Borrowed from hashtab.c iterative_hash implementation.  */
#define mix(a,b,c) \
{ \
  a -= b; a -= c; a ^= (c>>13); \
  b -= c; b -= a; b ^= (a<< 8); \
  c -= a; c -= b; c ^= ((b&0xffffffff)>>13); \
  a -= b; a -= c; a ^= ((c&0xffffffff)>>12); \
  b -= c; b -= a; b = (b ^ (a<<16)) & 0xffffffff; \
  c -= a; c -= b; c = (c ^ (b>> 5)) & 0xffffffff; \
  a -= b; a -= c; a = (a ^ (c>> 3)) & 0xffffffff; \
  b -= c; b -= a; b = (b ^ (a<<10)) & 0xffffffff; \
  c -= a; c -= b; c = (c ^ (b>>15)) & 0xffffffff; \
}


/* Produce good hash value combining VAL and VAL2.  */
static inline hashval_t
iterative_hash_hashval_t (hashval_t val, hashval_t val2)
{
  /* the golden ratio; an arbitrary value.  */
  hashval_t a = 0x9e3779b9;

  mix (a, val, val2);
  return val2;
}

/* Produce good hash value combining PTR and VAL2.  */
static inline hashval_t
iterative_hash_pointer (void *ptr, hashval_t val2)
{
  if (sizeof (ptr) == sizeof (hashval_t))
    return iterative_hash_hashval_t ((size_t) ptr, val2);
  else
    {
      hashval_t a = (hashval_t) (size_t) ptr;
      /* Avoid warnings about shifting of more than the width of the type on
         hosts that won't execute this path.  */
      int zero = 0;
      hashval_t b = (hashval_t) ((size_t) ptr >> (sizeof (hashval_t) * 8 + zero));
      mix (a, b, val2);
      return val2;
    }
}

/* Produce good hash value combining VAL and VAL2.  */
static inline hashval_t
iterative_hash_host_wide_int (HOST_WIDE_INT val, hashval_t val2)
{
  if (sizeof (HOST_WIDE_INT) == sizeof (hashval_t))
    return iterative_hash_hashval_t (val, val2);
  else
    {
      hashval_t a = (hashval_t) val;
      /* Avoid warnings about shifting of more than the width of the type on
         hosts that won't execute this path.  */
      int zero = 0;
      hashval_t b = (hashval_t) (val >> (sizeof (hashval_t) * 8 + zero));
      mix (a, b, val2);
      if (sizeof (HOST_WIDE_INT) > 2 * sizeof (hashval_t))
	{
	  hashval_t a = (hashval_t) (val >> (sizeof (hashval_t) * 16 + zero));
	  hashval_t b = (hashval_t) (val >> (sizeof (hashval_t) * 24 + zero));
	  mix (a, b, val2);
	}
      return val2;
    }
}

/* Return a type like TTYPE except that its TYPE_ATTRIBUTE
   is ATTRIBUTE and its qualifiers are QUALS.

   Record such modified types already made so we don't make duplicates.  */

static tree
build_type_attribute_qual_variant (tree ttype, tree attribute, int quals)
{
  if (! attribute_list_equal (TYPE_ATTRIBUTES (ttype), attribute))
    {
      hashval_t hashcode = 0;
      tree ntype;
      enum tree_code code = TREE_CODE (ttype);

      ntype = copy_node (ttype);

      TYPE_POINTER_TO (ntype) = 0;
      TYPE_REFERENCE_TO (ntype) = 0;
      TYPE_ATTRIBUTES (ntype) = attribute;

      /* Create a new main variant of TYPE.  */
      TYPE_MAIN_VARIANT (ntype) = ntype;
      TYPE_NEXT_VARIANT (ntype) = 0;
      set_type_quals (ntype, TYPE_UNQUALIFIED);

      hashcode = iterative_hash_object (code, hashcode);
      if (TREE_TYPE (ntype))
	hashcode = iterative_hash_object (TYPE_HASH (TREE_TYPE (ntype)),
					  hashcode);
      hashcode = attribute_hash_list (attribute, hashcode);

      switch (TREE_CODE (ntype))
	{
	case FUNCTION_TYPE:
	  hashcode = type_hash_list (TYPE_ARG_TYPES (ntype), hashcode);
	  break;
	case ARRAY_TYPE:
	  hashcode = iterative_hash_object (TYPE_HASH (TYPE_DOMAIN (ntype)),
					    hashcode);
	  break;
	case INTEGER_TYPE:
	  hashcode = iterative_hash_object
	    (TREE_INT_CST_LOW (TYPE_MAX_VALUE (ntype)), hashcode);
	  hashcode = iterative_hash_object
	    (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (ntype)), hashcode);
	  break;
	case REAL_TYPE:
	  {
	    unsigned int precision = TYPE_PRECISION (ntype);
	    hashcode = iterative_hash_object (precision, hashcode);
	  }
	  break;
	default:
	  break;
	}

      ntype = type_hash_canon (hashcode, ntype);
      ttype = build_qualified_type (ntype, quals);
    }

  return ttype;
}


/* Return a type like TTYPE except that its TYPE_ATTRIBUTE
   is ATTRIBUTE.

   Record such modified types already made so we don't make duplicates.  */

tree
build_type_attribute_variant (tree ttype, tree attribute)
{
  return build_type_attribute_qual_variant (ttype, attribute,
					    TYPE_QUALS (ttype));
}

/* Return nonzero if IDENT is a valid name for attribute ATTR,
   or zero if not.

   We try both `text' and `__text__', ATTR may be either one.  */
/* ??? It might be a reasonable simplification to require ATTR to be only
   `text'.  One might then also require attribute lists to be stored in
   their canonicalized form.  */

static int
is_attribute_with_length_p (const char *attr, int attr_len, tree ident)
{
  int ident_len;
  const char *p;

  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    return 0;
  
  p = IDENTIFIER_POINTER (ident);
  ident_len = IDENTIFIER_LENGTH (ident);
  
  if (ident_len == attr_len
      && strcmp (attr, p) == 0)
    return 1;

  /* If ATTR is `__text__', IDENT must be `text'; and vice versa.  */
  if (attr[0] == '_')
    {
      gcc_assert (attr[1] == '_');
      gcc_assert (attr[attr_len - 2] == '_');
      gcc_assert (attr[attr_len - 1] == '_');
      if (ident_len == attr_len - 4
	  && strncmp (attr + 2, p, attr_len - 4) == 0)
	return 1;
    }
  else
    {
      if (ident_len == attr_len + 4
	  && p[0] == '_' && p[1] == '_'
	  && p[ident_len - 2] == '_' && p[ident_len - 1] == '_'
	  && strncmp (attr, p + 2, attr_len) == 0)
	return 1;
    }

  return 0;
}

/* Return nonzero if IDENT is a valid name for attribute ATTR,
   or zero if not.

   We try both `text' and `__text__', ATTR may be either one.  */

int
is_attribute_p (const char *attr, tree ident)
{
  return is_attribute_with_length_p (attr, strlen (attr), ident);
}

/* Given an attribute name and a list of attributes, return a pointer to the
   attribute's list element if the attribute is part of the list, or NULL_TREE
   if not found.  If the attribute appears more than once, this only
   returns the first occurrence; the TREE_CHAIN of the return value should
   be passed back in if further occurrences are wanted.  */

tree
lookup_attribute (const char *attr_name, tree list)
{
  tree l;
  size_t attr_len = strlen (attr_name);

  for (l = list; l; l = TREE_CHAIN (l))
    {
      gcc_assert (TREE_CODE (TREE_PURPOSE (l)) == IDENTIFIER_NODE);
      if (is_attribute_with_length_p (attr_name, attr_len, TREE_PURPOSE (l)))
	return l;
    }

  return NULL_TREE;
}

/* Remove any instances of attribute ATTR_NAME in LIST and return the
   modified list.  */

tree
remove_attribute (const char *attr_name, tree list)
{
  tree *p;
  size_t attr_len = strlen (attr_name);

  for (p = &list; *p; )
    {
      tree l = *p;
      gcc_assert (TREE_CODE (TREE_PURPOSE (l)) == IDENTIFIER_NODE);
      if (is_attribute_with_length_p (attr_name, attr_len, TREE_PURPOSE (l)))
	*p = TREE_CHAIN (l);
      else
	p = &TREE_CHAIN (l);
    }

  return list;
}

/* Return an attribute list that is the union of a1 and a2.  */

tree
merge_attributes (tree a1, tree a2)
{
  tree attributes;

  /* Either one unset?  Take the set one.  */

  if ((attributes = a1) == 0)
    attributes = a2;

  /* One that completely contains the other?  Take it.  */

  else if (a2 != 0 && ! attribute_list_contained (a1, a2))
    {
      if (attribute_list_contained (a2, a1))
	attributes = a2;
      else
	{
	  /* Pick the longest list, and hang on the other list.  */

	  if (list_length (a1) < list_length (a2))
	    attributes = a2, a2 = a1;

	  for (; a2 != 0; a2 = TREE_CHAIN (a2))
	    {
	      tree a;
	      for (a = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (a2)),
					 attributes);
		   a != NULL_TREE;
		   a = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (a2)),
					 TREE_CHAIN (a)))
		{
		  if (TREE_VALUE (a) != NULL
		      && TREE_CODE (TREE_VALUE (a)) == TREE_LIST
		      && TREE_VALUE (a2) != NULL
		      && TREE_CODE (TREE_VALUE (a2)) == TREE_LIST)
		    {
		      if (simple_cst_list_equal (TREE_VALUE (a),
						 TREE_VALUE (a2)) == 1)
			break;
		    }
		  else if (simple_cst_equal (TREE_VALUE (a),
					     TREE_VALUE (a2)) == 1)
		    break;
		}
	      if (a == NULL_TREE)
		{
		  a1 = copy_node (a2);
		  TREE_CHAIN (a1) = attributes;
		  attributes = a1;
		}
	    }
	}
    }
  return attributes;
}

/* Given types T1 and T2, merge their attributes and return
  the result.  */

tree
merge_type_attributes (tree t1, tree t2)
{
  return merge_attributes (TYPE_ATTRIBUTES (t1),
			   TYPE_ATTRIBUTES (t2));
}

/* Given decls OLDDECL and NEWDECL, merge their attributes and return
   the result.  */

tree
merge_decl_attributes (tree olddecl, tree newdecl)
{
  return merge_attributes (DECL_ATTRIBUTES (olddecl),
			   DECL_ATTRIBUTES (newdecl));
}

#if TARGET_DLLIMPORT_DECL_ATTRIBUTES

/* Specialization of merge_decl_attributes for various Windows targets.

   This handles the following situation:

     __declspec (dllimport) int foo;
     int foo;

   The second instance of `foo' nullifies the dllimport.  */

tree
merge_dllimport_decl_attributes (tree old, tree new)
{
  tree a;
  int delete_dllimport_p = 1;

  /* What we need to do here is remove from `old' dllimport if it doesn't
     appear in `new'.  dllimport behaves like extern: if a declaration is
     marked dllimport and a definition appears later, then the object
     is not dllimport'd.  We also remove a `new' dllimport if the old list
     contains dllexport:  dllexport always overrides dllimport, regardless
     of the order of declaration.  */     
  if (!VAR_OR_FUNCTION_DECL_P (new))
    delete_dllimport_p = 0;
  else if (DECL_DLLIMPORT_P (new)
     	   && lookup_attribute ("dllexport", DECL_ATTRIBUTES (old)))
    { 
      DECL_DLLIMPORT_P (new) = 0;
      warning (OPT_Wattributes, "%q+D already declared with dllexport attribute: "
	      "dllimport ignored", new);
    }
  else if (DECL_DLLIMPORT_P (old) && !DECL_DLLIMPORT_P (new))
    {
      /* Warn about overriding a symbol that has already been used. eg:
           extern int __attribute__ ((dllimport)) foo;
	   int* bar () {return &foo;}
	   int foo;
      */
      if (TREE_USED (old))
	{
	  warning (0, "%q+D redeclared without dllimport attribute "
		   "after being referenced with dll linkage", new);
	  /* If we have used a variable's address with dllimport linkage,
	      keep the old DECL_DLLIMPORT_P flag: the ADDR_EXPR using the
	      decl may already have had TREE_INVARIANT and TREE_CONSTANT
	      computed.
	      We still remove the attribute so that assembler code refers
	      to '&foo rather than '_imp__foo'.  */
	  if (TREE_CODE (old) == VAR_DECL && TREE_ADDRESSABLE (old))
	    DECL_DLLIMPORT_P (new) = 1;
	}

      /* Let an inline definition silently override the external reference,
	 but otherwise warn about attribute inconsistency.  */ 
      else if (TREE_CODE (new) == VAR_DECL
	       || !DECL_DECLARED_INLINE_P (new))
	warning (OPT_Wattributes, "%q+D redeclared without dllimport attribute: "
		  "previous dllimport ignored", new);
    }
  else
    delete_dllimport_p = 0;

  a = merge_attributes (DECL_ATTRIBUTES (old), DECL_ATTRIBUTES (new));

  if (delete_dllimport_p) 
    {
      tree prev, t;
      const size_t attr_len = strlen ("dllimport"); 
     
      /* Scan the list for dllimport and delete it.  */
      for (prev = NULL_TREE, t = a; t; prev = t, t = TREE_CHAIN (t))
	{
	  if (is_attribute_with_length_p ("dllimport", attr_len,
					  TREE_PURPOSE (t)))
	    {
	      if (prev == NULL_TREE)
		a = TREE_CHAIN (a);
	      else
		TREE_CHAIN (prev) = TREE_CHAIN (t);
	      break;
	    }
	}
    }

  return a;
}

/* Handle a "dllimport" or "dllexport" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_dll_attribute (tree * pnode, tree name, tree args, int flags,
		      bool *no_add_attrs)
{
  tree node = *pnode;

  /* These attributes may apply to structure and union types being created,
     but otherwise should pass to the declaration involved.  */
  if (!DECL_P (node))
    {
      if (flags & ((int) ATTR_FLAG_DECL_NEXT | (int) ATTR_FLAG_FUNCTION_NEXT
		   | (int) ATTR_FLAG_ARRAY_NEXT))
	{
	  *no_add_attrs = true;
	  return tree_cons (name, args, NULL_TREE);
	}
      if (TREE_CODE (node) != RECORD_TYPE && TREE_CODE (node) != UNION_TYPE)
	{
	  warning (OPT_Wattributes, "%qs attribute ignored",
		   IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}

      return NULL_TREE;
    }

  if (TREE_CODE (node) != FUNCTION_DECL
      && TREE_CODE (node) != VAR_DECL)
    {
      *no_add_attrs = true;
      warning (OPT_Wattributes, "%qs attribute ignored",
	       IDENTIFIER_POINTER (name));
      return NULL_TREE;
    }

  /* Report error on dllimport ambiguities seen now before they cause
     any damage.  */
  else if (is_attribute_p ("dllimport", name))
    {
      /* Honor any target-specific overrides. */ 
      if (!targetm.valid_dllimport_attribute_p (node))
	*no_add_attrs = true;

     else if (TREE_CODE (node) == FUNCTION_DECL
	        && DECL_DECLARED_INLINE_P (node))
	{
	  warning (OPT_Wattributes, "inline function %q+D declared as "
		  " dllimport: attribute ignored", node); 
	  *no_add_attrs = true;
	}
      /* Like MS, treat definition of dllimported variables and
	 non-inlined functions on declaration as syntax errors. */
     else if (TREE_CODE (node) == FUNCTION_DECL && DECL_INITIAL (node))
	{
	  error ("function %q+D definition is marked dllimport", node);
	  *no_add_attrs = true;
	}

     else if (TREE_CODE (node) == VAR_DECL)
	{
	  if (DECL_INITIAL (node))
	    {
	      error ("variable %q+D definition is marked dllimport",
		     node);
	      *no_add_attrs = true;
	    }

	  /* `extern' needn't be specified with dllimport.
	     Specify `extern' now and hope for the best.  Sigh.  */
	  DECL_EXTERNAL (node) = 1;
	  /* Also, implicitly give dllimport'd variables declared within
	     a function global scope, unless declared static.  */
	  if (current_function_decl != NULL_TREE && !TREE_STATIC (node))
	    TREE_PUBLIC (node) = 1;
	}

      if (*no_add_attrs == false)
        DECL_DLLIMPORT_P (node) = 1;
    }

  /*  Report error if symbol is not accessible at global scope.  */
  if (!TREE_PUBLIC (node)
      && (TREE_CODE (node) == VAR_DECL
	  || TREE_CODE (node) == FUNCTION_DECL))
    {
      error ("external linkage required for symbol %q+D because of "
	     "%qs attribute", node, IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

#endif /* TARGET_DLLIMPORT_DECL_ATTRIBUTES  */

/* Set the type qualifiers for TYPE to TYPE_QUALS, which is a bitmask
   of the various TYPE_QUAL values.  */

static void
set_type_quals (tree type, int type_quals)
{
  TYPE_READONLY (type) = (type_quals & TYPE_QUAL_CONST) != 0;
  TYPE_VOLATILE (type) = (type_quals & TYPE_QUAL_VOLATILE) != 0;
  TYPE_RESTRICT (type) = (type_quals & TYPE_QUAL_RESTRICT) != 0;
}

/* Returns true iff cand is equivalent to base with type_quals.  */

bool
check_qualified_type (tree cand, tree base, int type_quals)
{
  return (TYPE_QUALS (cand) == type_quals
	  && TYPE_NAME (cand) == TYPE_NAME (base)
	  /* Apparently this is needed for Objective-C.  */
	  && TYPE_CONTEXT (cand) == TYPE_CONTEXT (base)
	  && attribute_list_equal (TYPE_ATTRIBUTES (cand),
				   TYPE_ATTRIBUTES (base)));
}

/* Return a version of the TYPE, qualified as indicated by the
   TYPE_QUALS, if one exists.  If no qualified version exists yet,
   return NULL_TREE.  */

tree
get_qualified_type (tree type, int type_quals)
{
  tree t;

  if (TYPE_QUALS (type) == type_quals)
    return type;

  /* Search the chain of variants to see if there is already one there just
     like the one we need to have.  If so, use that existing one.  We must
     preserve the TYPE_NAME, since there is code that depends on this.  */
  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    if (check_qualified_type (t, type, type_quals))
      return t;

  return NULL_TREE;
}

/* Like get_qualified_type, but creates the type if it does not
   exist.  This function never returns NULL_TREE.  */

tree
build_qualified_type (tree type, int type_quals)
{
  tree t;

  /* See if we already have the appropriate qualified variant.  */
  t = get_qualified_type (type, type_quals);

  /* If not, build it.  */
  if (!t)
    {
      t = build_variant_type_copy (type);
      set_type_quals (t, type_quals);
    }

  return t;
}

/* Create a new distinct copy of TYPE.  The new type is made its own
   MAIN_VARIANT.  */

tree
build_distinct_type_copy (tree type)
{
  tree t = copy_node (type);
  
  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;

  /* Make it its own variant.  */
  TYPE_MAIN_VARIANT (t) = t;
  TYPE_NEXT_VARIANT (t) = 0;

  /* Note that it is now possible for TYPE_MIN_VALUE to be a value
     whose TREE_TYPE is not t.  This can also happen in the Ada
     frontend when using subtypes.  */

  return t;
}

/* Create a new variant of TYPE, equivalent but distinct.
   This is so the caller can modify it.  */

tree
build_variant_type_copy (tree type)
{
  tree t, m = TYPE_MAIN_VARIANT (type);

  t = build_distinct_type_copy (type);
  
  /* Add the new type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;
  TYPE_MAIN_VARIANT (t) = m;

  return t;
}

/* Return true if the from tree in both tree maps are equal.  */

int
tree_map_eq (const void *va, const void *vb)
{
  const struct tree_map  *a = va, *b = vb;
  return (a->from == b->from);
}

/* Hash a from tree in a tree_map.  */

unsigned int
tree_map_hash (const void *item)
{
  return (((const struct tree_map *) item)->hash);
}

/* Return true if this tree map structure is marked for garbage collection
   purposes.  We simply return true if the from tree is marked, so that this
   structure goes away when the from tree goes away.  */

int
tree_map_marked_p (const void *p)
{
  tree from = ((struct tree_map *) p)->from;

  return ggc_marked_p (from);
}

/* Return true if the trees in the tree_int_map *'s VA and VB are equal.  */

static int
tree_int_map_eq (const void *va, const void *vb)
{
  const struct tree_int_map  *a = va, *b = vb;
  return (a->from == b->from);
}

/* Hash a from tree in the tree_int_map * ITEM.  */

static unsigned int
tree_int_map_hash (const void *item)
{
  return htab_hash_pointer (((const struct tree_int_map *)item)->from);
}

/* Return true if this tree int map structure is marked for garbage collection
   purposes.  We simply return true if the from tree_int_map *P's from tree is marked, so that this
   structure goes away when the from tree goes away.  */

static int
tree_int_map_marked_p (const void *p)
{
  tree from = ((struct tree_int_map *) p)->from;

  return ggc_marked_p (from);
}
/* Lookup an init priority for FROM, and return it if we find one.  */

unsigned short
decl_init_priority_lookup (tree from)
{
  struct tree_int_map *h, in;
  in.from = from;

  h = htab_find_with_hash (init_priority_for_decl, 
			   &in, htab_hash_pointer (from));
  if (h)
    return h->to;
  return 0;
}

/* Insert a mapping FROM->TO in the init priority hashtable.  */

void
decl_init_priority_insert (tree from, unsigned short to)
{
  struct tree_int_map *h;
  void **loc;

  h = ggc_alloc (sizeof (struct tree_int_map));
  h->from = from;
  h->to = to;
  loc = htab_find_slot_with_hash (init_priority_for_decl, h, 
				  htab_hash_pointer (from), INSERT);
  *(struct tree_int_map **) loc = h;
}  

/* Look up a restrict qualified base decl for FROM.  */

tree
decl_restrict_base_lookup (tree from)
{
  struct tree_map *h;
  struct tree_map in;

  in.from = from;
  h = htab_find_with_hash (restrict_base_for_decl, &in,
			   htab_hash_pointer (from));
  return h ? h->to : NULL_TREE;
}

/* Record the restrict qualified base TO for FROM.  */

void
decl_restrict_base_insert (tree from, tree to)
{
  struct tree_map *h;
  void **loc;

  h = ggc_alloc (sizeof (struct tree_map));
  h->hash = htab_hash_pointer (from);
  h->from = from;
  h->to = to;
  loc = htab_find_slot_with_hash (restrict_base_for_decl, h, h->hash, INSERT);
  *(struct tree_map **) loc = h;
}

/* Print out the statistics for the DECL_DEBUG_EXPR hash table.  */

static void
print_debug_expr_statistics (void)
{
  fprintf (stderr, "DECL_DEBUG_EXPR  hash: size %ld, %ld elements, %f collisions\n",
	   (long) htab_size (debug_expr_for_decl),
	   (long) htab_elements (debug_expr_for_decl),
	   htab_collisions (debug_expr_for_decl));
}

/* Print out the statistics for the DECL_VALUE_EXPR hash table.  */

static void
print_value_expr_statistics (void)
{
  fprintf (stderr, "DECL_VALUE_EXPR  hash: size %ld, %ld elements, %f collisions\n",
	   (long) htab_size (value_expr_for_decl),
	   (long) htab_elements (value_expr_for_decl),
	   htab_collisions (value_expr_for_decl));
}

/* Print out statistics for the RESTRICT_BASE_FOR_DECL hash table, but
   don't print anything if the table is empty.  */

static void
print_restrict_base_statistics (void)
{
  if (htab_elements (restrict_base_for_decl) != 0)
    fprintf (stderr,
	     "RESTRICT_BASE    hash: size %ld, %ld elements, %f collisions\n",
	     (long) htab_size (restrict_base_for_decl),
	     (long) htab_elements (restrict_base_for_decl),
	     htab_collisions (restrict_base_for_decl));
}

/* Lookup a debug expression for FROM, and return it if we find one.  */

tree 
decl_debug_expr_lookup (tree from)
{
  struct tree_map *h, in;
  in.from = from;

  h = htab_find_with_hash (debug_expr_for_decl, &in, htab_hash_pointer (from));
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping FROM->TO in the debug expression hashtable.  */

void
decl_debug_expr_insert (tree from, tree to)
{
  struct tree_map *h;
  void **loc;

  h = ggc_alloc (sizeof (struct tree_map));
  h->hash = htab_hash_pointer (from);
  h->from = from;
  h->to = to;
  loc = htab_find_slot_with_hash (debug_expr_for_decl, h, h->hash, INSERT);
  *(struct tree_map **) loc = h;
}  

/* Lookup a value expression for FROM, and return it if we find one.  */

tree 
decl_value_expr_lookup (tree from)
{
  struct tree_map *h, in;
  in.from = from;

  h = htab_find_with_hash (value_expr_for_decl, &in, htab_hash_pointer (from));
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping FROM->TO in the value expression hashtable.  */

void
decl_value_expr_insert (tree from, tree to)
{
  struct tree_map *h;
  void **loc;

  h = ggc_alloc (sizeof (struct tree_map));
  h->hash = htab_hash_pointer (from);
  h->from = from;
  h->to = to;
  loc = htab_find_slot_with_hash (value_expr_for_decl, h, h->hash, INSERT);
  *(struct tree_map **) loc = h;
}

/* Hashing of types so that we don't make duplicates.
   The entry point is `type_hash_canon'.  */

/* Compute a hash code for a list of types (chain of TREE_LIST nodes
   with types in the TREE_VALUE slots), by adding the hash codes
   of the individual types.  */

unsigned int
type_hash_list (tree list, hashval_t hashcode)
{
  tree tail;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
    if (TREE_VALUE (tail) != error_mark_node)
      hashcode = iterative_hash_object (TYPE_HASH (TREE_VALUE (tail)),
					hashcode);

  return hashcode;
}

/* These are the Hashtable callback functions.  */

/* Returns true iff the types are equivalent.  */

static int
type_hash_eq (const void *va, const void *vb)
{
  const struct type_hash *a = va, *b = vb;

  /* First test the things that are the same for all types.  */
  if (a->hash != b->hash
      || TREE_CODE (a->type) != TREE_CODE (b->type)
      || TREE_TYPE (a->type) != TREE_TYPE (b->type)
      || !attribute_list_equal (TYPE_ATTRIBUTES (a->type),
				 TYPE_ATTRIBUTES (b->type))
      || TYPE_ALIGN (a->type) != TYPE_ALIGN (b->type)
      || TYPE_MODE (a->type) != TYPE_MODE (b->type))
    return 0;

  switch (TREE_CODE (a->type))
    {
    case VOID_TYPE:
    case COMPLEX_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      return 1;

    case VECTOR_TYPE:
      return TYPE_VECTOR_SUBPARTS (a->type) == TYPE_VECTOR_SUBPARTS (b->type);

    case ENUMERAL_TYPE:
      if (TYPE_VALUES (a->type) != TYPE_VALUES (b->type)
	  && !(TYPE_VALUES (a->type)
	       && TREE_CODE (TYPE_VALUES (a->type)) == TREE_LIST
	       && TYPE_VALUES (b->type)
	       && TREE_CODE (TYPE_VALUES (b->type)) == TREE_LIST
	       && type_list_equal (TYPE_VALUES (a->type),
				   TYPE_VALUES (b->type))))
	return 0;

      /* ... fall through ... */

    case INTEGER_TYPE:
    case REAL_TYPE:
    case BOOLEAN_TYPE:
      return ((TYPE_MAX_VALUE (a->type) == TYPE_MAX_VALUE (b->type)
	       || tree_int_cst_equal (TYPE_MAX_VALUE (a->type),
				      TYPE_MAX_VALUE (b->type)))
	      && (TYPE_MIN_VALUE (a->type) == TYPE_MIN_VALUE (b->type)
		  || tree_int_cst_equal (TYPE_MIN_VALUE (a->type),
					 TYPE_MIN_VALUE (b->type))));

    case OFFSET_TYPE:
      return TYPE_OFFSET_BASETYPE (a->type) == TYPE_OFFSET_BASETYPE (b->type);

    case METHOD_TYPE:
      return (TYPE_METHOD_BASETYPE (a->type) == TYPE_METHOD_BASETYPE (b->type)
	      && (TYPE_ARG_TYPES (a->type) == TYPE_ARG_TYPES (b->type)
		  || (TYPE_ARG_TYPES (a->type)
		      && TREE_CODE (TYPE_ARG_TYPES (a->type)) == TREE_LIST
		      && TYPE_ARG_TYPES (b->type)
		      && TREE_CODE (TYPE_ARG_TYPES (b->type)) == TREE_LIST
		      && type_list_equal (TYPE_ARG_TYPES (a->type),
					  TYPE_ARG_TYPES (b->type)))));

    case ARRAY_TYPE:
      return TYPE_DOMAIN (a->type) == TYPE_DOMAIN (b->type);

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      return (TYPE_FIELDS (a->type) == TYPE_FIELDS (b->type)
	      || (TYPE_FIELDS (a->type)
		  && TREE_CODE (TYPE_FIELDS (a->type)) == TREE_LIST
		  && TYPE_FIELDS (b->type)
		  && TREE_CODE (TYPE_FIELDS (b->type)) == TREE_LIST
		  && type_list_equal (TYPE_FIELDS (a->type),
				      TYPE_FIELDS (b->type))));

    case FUNCTION_TYPE:
      return (TYPE_ARG_TYPES (a->type) == TYPE_ARG_TYPES (b->type)
	      || (TYPE_ARG_TYPES (a->type)
		  && TREE_CODE (TYPE_ARG_TYPES (a->type)) == TREE_LIST
		  && TYPE_ARG_TYPES (b->type)
		  && TREE_CODE (TYPE_ARG_TYPES (b->type)) == TREE_LIST
		  && type_list_equal (TYPE_ARG_TYPES (a->type),
				      TYPE_ARG_TYPES (b->type))));

    default:
      return 0;
    }
}

/* Return the cached hash value.  */

static hashval_t
type_hash_hash (const void *item)
{
  return ((const struct type_hash *) item)->hash;
}

/* Look in the type hash table for a type isomorphic to TYPE.
   If one is found, return it.  Otherwise return 0.  */

tree
type_hash_lookup (hashval_t hashcode, tree type)
{
  struct type_hash *h, in;

  /* The TYPE_ALIGN field of a type is set by layout_type(), so we
     must call that routine before comparing TYPE_ALIGNs.  */
  layout_type (type);

  in.hash = hashcode;
  in.type = type;

  h = htab_find_with_hash (type_hash_table, &in, hashcode);
  if (h)
    return h->type;
  return NULL_TREE;
}

/* Add an entry to the type-hash-table
   for a type TYPE whose hash code is HASHCODE.  */

void
type_hash_add (hashval_t hashcode, tree type)
{
  struct type_hash *h;
  void **loc;

  h = ggc_alloc (sizeof (struct type_hash));
  h->hash = hashcode;
  h->type = type;
  loc = htab_find_slot_with_hash (type_hash_table, h, hashcode, INSERT);
  *(struct type_hash **) loc = h;
}

/* Given TYPE, and HASHCODE its hash code, return the canonical
   object for an identical type if one already exists.
   Otherwise, return TYPE, and record it as the canonical object.

   To use this function, first create a type of the sort you want.
   Then compute its hash code from the fields of the type that
   make it different from other similar types.
   Then call this function and use the value.  */

tree
type_hash_canon (unsigned int hashcode, tree type)
{
  tree t1;

  /* The hash table only contains main variants, so ensure that's what we're
     being passed.  */
  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  if (!lang_hooks.types.hash_types)
    return type;

  /* See if the type is in the hash table already.  If so, return it.
     Otherwise, add the type.  */
  t1 = type_hash_lookup (hashcode, type);
  if (t1 != 0)
    {
#ifdef GATHER_STATISTICS
      tree_node_counts[(int) t_kind]--;
      tree_node_sizes[(int) t_kind] -= sizeof (struct tree_type);
#endif
      return t1;
    }
  else
    {
      type_hash_add (hashcode, type);
      return type;
    }
}

/* See if the data pointed to by the type hash table is marked.  We consider
   it marked if the type is marked or if a debug type number or symbol
   table entry has been made for the type.  This reduces the amount of
   debugging output and eliminates that dependency of the debug output on
   the number of garbage collections.  */

static int
type_hash_marked_p (const void *p)
{
  tree type = ((struct type_hash *) p)->type;

  return ggc_marked_p (type) || TYPE_SYMTAB_POINTER (type);
}

static void
print_type_hash_statistics (void)
{
  fprintf (stderr, "Type hash: size %ld, %ld elements, %f collisions\n",
	   (long) htab_size (type_hash_table),
	   (long) htab_elements (type_hash_table),
	   htab_collisions (type_hash_table));
}

/* Compute a hash code for a list of attributes (chain of TREE_LIST nodes
   with names in the TREE_PURPOSE slots and args in the TREE_VALUE slots),
   by adding the hash codes of the individual attributes.  */

unsigned int
attribute_hash_list (tree list, hashval_t hashcode)
{
  tree tail;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
    /* ??? Do we want to add in TREE_VALUE too? */
    hashcode = iterative_hash_object
      (IDENTIFIER_HASH_VALUE (TREE_PURPOSE (tail)), hashcode);
  return hashcode;
}

/* Given two lists of attributes, return true if list l2 is
   equivalent to l1.  */

int
attribute_list_equal (tree l1, tree l2)
{
  return attribute_list_contained (l1, l2)
	 && attribute_list_contained (l2, l1);
}

/* Given two lists of attributes, return true if list L2 is
   completely contained within L1.  */
/* ??? This would be faster if attribute names were stored in a canonicalized
   form.  Otherwise, if L1 uses `foo' and L2 uses `__foo__', the long method
   must be used to show these elements are equivalent (which they are).  */
/* ??? It's not clear that attributes with arguments will always be handled
   correctly.  */

int
attribute_list_contained (tree l1, tree l2)
{
  tree t1, t2;

  /* First check the obvious, maybe the lists are identical.  */
  if (l1 == l2)
    return 1;

  /* Maybe the lists are similar.  */
  for (t1 = l1, t2 = l2;
       t1 != 0 && t2 != 0
        && TREE_PURPOSE (t1) == TREE_PURPOSE (t2)
        && TREE_VALUE (t1) == TREE_VALUE (t2);
       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2));

  /* Maybe the lists are equal.  */
  if (t1 == 0 && t2 == 0)
    return 1;

  for (; t2 != 0; t2 = TREE_CHAIN (t2))
    {
      tree attr;
      for (attr = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (t2)), l1);
	   attr != NULL_TREE;
	   attr = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (t2)),
				    TREE_CHAIN (attr)))
	{
	  if (TREE_VALUE (t2) != NULL
	      && TREE_CODE (TREE_VALUE (t2)) == TREE_LIST
	      && TREE_VALUE (attr) != NULL
	      && TREE_CODE (TREE_VALUE (attr)) == TREE_LIST)
	    {
	      if (simple_cst_list_equal (TREE_VALUE (t2),
					 TREE_VALUE (attr)) == 1)
		break;
	    }
	  else if (simple_cst_equal (TREE_VALUE (t2), TREE_VALUE (attr)) == 1)
	    break;
	}

      if (attr == 0)
	return 0;
    }

  return 1;
}

/* Given two lists of types
   (chains of TREE_LIST nodes with types in the TREE_VALUE slots)
   return 1 if the lists contain the same types in the same order.
   Also, the TREE_PURPOSEs must match.  */

int
type_list_equal (tree l1, tree l2)
{
  tree t1, t2;

  for (t1 = l1, t2 = l2; t1 && t2; t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    if (TREE_VALUE (t1) != TREE_VALUE (t2)
	|| (TREE_PURPOSE (t1) != TREE_PURPOSE (t2)
	    && ! (1 == simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2))
		  && (TREE_TYPE (TREE_PURPOSE (t1))
		      == TREE_TYPE (TREE_PURPOSE (t2))))))
      return 0;

  return t1 == t2;
}

/* Returns the number of arguments to the FUNCTION_TYPE or METHOD_TYPE
   given by TYPE.  If the argument list accepts variable arguments,
   then this function counts only the ordinary arguments.  */

int
type_num_arguments (tree type)
{
  int i = 0;
  tree t;

  for (t = TYPE_ARG_TYPES (type); t; t = TREE_CHAIN (t))
    /* If the function does not take a variable number of arguments,
       the last element in the list will have type `void'.  */
    if (VOID_TYPE_P (TREE_VALUE (t)))
      break;
    else
      ++i;

  return i;
}

/* Nonzero if integer constants T1 and T2
   represent the same constant value.  */

int
tree_int_cst_equal (tree t1, tree t2)
{
  if (t1 == t2)
    return 1;

  if (t1 == 0 || t2 == 0)
    return 0;

  if (TREE_CODE (t1) == INTEGER_CST
      && TREE_CODE (t2) == INTEGER_CST
      && TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
      && TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2))
    return 1;

  return 0;
}

/* Nonzero if integer constants T1 and T2 represent values that satisfy <.
   The precise way of comparison depends on their data type.  */

int
tree_int_cst_lt (tree t1, tree t2)
{
  if (t1 == t2)
    return 0;

  if (TYPE_UNSIGNED (TREE_TYPE (t1)) != TYPE_UNSIGNED (TREE_TYPE (t2)))
    {
      int t1_sgn = tree_int_cst_sgn (t1);
      int t2_sgn = tree_int_cst_sgn (t2);

      if (t1_sgn < t2_sgn)
	return 1;
      else if (t1_sgn > t2_sgn)
	return 0;
      /* Otherwise, both are non-negative, so we compare them as
	 unsigned just in case one of them would overflow a signed
	 type.  */
    }
  else if (!TYPE_UNSIGNED (TREE_TYPE (t1)))
    return INT_CST_LT (t1, t2);

  return INT_CST_LT_UNSIGNED (t1, t2);
}

/* Returns -1 if T1 < T2, 0 if T1 == T2, and 1 if T1 > T2.  */

int
tree_int_cst_compare (tree t1, tree t2)
{
  if (tree_int_cst_lt (t1, t2))
    return -1;
  else if (tree_int_cst_lt (t2, t1))
    return 1;
  else
    return 0;
}

/* Return 1 if T is an INTEGER_CST that can be manipulated efficiently on
   the host.  If POS is zero, the value can be represented in a single
   HOST_WIDE_INT.  If POS is nonzero, the value must be non-negative and can
   be represented in a single unsigned HOST_WIDE_INT.  */

int
host_integerp (tree t, int pos)
{
  return (TREE_CODE (t) == INTEGER_CST
	  && ((TREE_INT_CST_HIGH (t) == 0
	       && (HOST_WIDE_INT) TREE_INT_CST_LOW (t) >= 0)
	      || (! pos && TREE_INT_CST_HIGH (t) == -1
		  && (HOST_WIDE_INT) TREE_INT_CST_LOW (t) < 0
		  && !TYPE_UNSIGNED (TREE_TYPE (t)))
	      || (pos && TREE_INT_CST_HIGH (t) == 0)));
}

/* Return the HOST_WIDE_INT least significant bits of T if it is an
   INTEGER_CST and there is no overflow.  POS is nonzero if the result must
   be non-negative.  We must be able to satisfy the above conditions.  */

HOST_WIDE_INT
tree_low_cst (tree t, int pos)
{
  gcc_assert (host_integerp (t, pos));
  return TREE_INT_CST_LOW (t);
}

/* Return the most significant bit of the integer constant T.  */

int
tree_int_cst_msb (tree t)
{
  int prec;
  HOST_WIDE_INT h;
  unsigned HOST_WIDE_INT l;

  /* Note that using TYPE_PRECISION here is wrong.  We care about the
     actual bits, not the (arbitrary) range of the type.  */
  prec = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (t))) - 1;
  rshift_double (TREE_INT_CST_LOW (t), TREE_INT_CST_HIGH (t), prec,
		 2 * HOST_BITS_PER_WIDE_INT, &l, &h, 0);
  return (l & 1) == 1;
}

/* Return an indication of the sign of the integer constant T.
   The return value is -1 if T < 0, 0 if T == 0, and 1 if T > 0.
   Note that -1 will never be returned if T's type is unsigned.  */

int
tree_int_cst_sgn (tree t)
{
  if (TREE_INT_CST_LOW (t) == 0 && TREE_INT_CST_HIGH (t) == 0)
    return 0;
  else if (TYPE_UNSIGNED (TREE_TYPE (t)))
    return 1;
  else if (TREE_INT_CST_HIGH (t) < 0)
    return -1;
  else
    return 1;
}

/* Compare two constructor-element-type constants.  Return 1 if the lists
   are known to be equal; otherwise return 0.  */

int
simple_cst_list_equal (tree l1, tree l2)
{
  while (l1 != NULL_TREE && l2 != NULL_TREE)
    {
      if (simple_cst_equal (TREE_VALUE (l1), TREE_VALUE (l2)) != 1)
	return 0;

      l1 = TREE_CHAIN (l1);
      l2 = TREE_CHAIN (l2);
    }

  return l1 == l2;
}

/* Return truthvalue of whether T1 is the same tree structure as T2.
   Return 1 if they are the same.
   Return 0 if they are understandably different.
   Return -1 if either contains tree structure not understood by
   this function.  */

int
simple_cst_equal (tree t1, tree t2)
{
  enum tree_code code1, code2;
  int cmp;
  int i;

  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  if (code1 == NOP_EXPR || code1 == CONVERT_EXPR || code1 == NON_LVALUE_EXPR)
    {
      if (code2 == NOP_EXPR || code2 == CONVERT_EXPR
	  || code2 == NON_LVALUE_EXPR)
	return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      else
	return simple_cst_equal (TREE_OPERAND (t1, 0), t2);
    }

  else if (code2 == NOP_EXPR || code2 == CONVERT_EXPR
	   || code2 == NON_LVALUE_EXPR)
    return simple_cst_equal (t1, TREE_OPERAND (t2, 0));

  if (code1 != code2)
    return 0;

  switch (code1)
    {
    case INTEGER_CST:
      return (TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
	      && TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2));

    case REAL_CST:
      return REAL_VALUES_IDENTICAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

    case STRING_CST:
      return (TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	      && ! memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
			 TREE_STRING_LENGTH (t1)));

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	VEC(constructor_elt, gc) *v1 = CONSTRUCTOR_ELTS (t1);
	VEC(constructor_elt, gc) *v2 = CONSTRUCTOR_ELTS (t2);

	if (VEC_length (constructor_elt, v1) != VEC_length (constructor_elt, v2))
	  return false;

        for (idx = 0; idx < VEC_length (constructor_elt, v1); ++idx)
	  /* ??? Should we handle also fields here? */
	  if (!simple_cst_equal (VEC_index (constructor_elt, v1, idx)->value,
				 VEC_index (constructor_elt, v2, idx)->value))
	    return false;
	return true;
      }

    case SAVE_EXPR:
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case CALL_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return
	simple_cst_list_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case TARGET_EXPR:
      /* Special case: if either target is an unallocated VAR_DECL,
	 it means that it's going to be unified with whatever the
	 TARGET_EXPR is really supposed to initialize, so treat it
	 as being equivalent to anything.  */
      if ((TREE_CODE (TREE_OPERAND (t1, 0)) == VAR_DECL
	   && DECL_NAME (TREE_OPERAND (t1, 0)) == NULL_TREE
	   && !DECL_RTL_SET_P (TREE_OPERAND (t1, 0)))
	  || (TREE_CODE (TREE_OPERAND (t2, 0)) == VAR_DECL
	      && DECL_NAME (TREE_OPERAND (t2, 0)) == NULL_TREE
	      && !DECL_RTL_SET_P (TREE_OPERAND (t2, 0))))
	cmp = 1;
      else
	cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

      if (cmp <= 0)
	return cmp;

      return simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case WITH_CLEANUP_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;

      return simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t1, 1));

    case COMPONENT_REF:
      if (TREE_OPERAND (t1, 1) == TREE_OPERAND (t2, 1))
	return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

      return 0;

    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
      return 0;

    default:
      break;
    }

  /* This general rule works for most tree codes.  All exceptions should be
     handled above.  If this is a language-specific tree code, we can't
     trust what might be in the operand, so say we don't know
     the situation.  */
  if ((int) code1 >= (int) LAST_AND_UNUSED_TREE_CODE)
    return -1;

  switch (TREE_CODE_CLASS (code1))
    {
    case tcc_unary:
    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
    case tcc_reference:
    case tcc_statement:
      cmp = 1;
      for (i = 0; i < TREE_CODE_LENGTH (code1); i++)
	{
	  cmp = simple_cst_equal (TREE_OPERAND (t1, i), TREE_OPERAND (t2, i));
	  if (cmp <= 0)
	    return cmp;
	}

      return cmp;

    default:
      return -1;
    }
}

/* Compare the value of T, an INTEGER_CST, with U, an unsigned integer value.
   Return -1, 0, or 1 if the value of T is less than, equal to, or greater
   than U, respectively.  */

int
compare_tree_int (tree t, unsigned HOST_WIDE_INT u)
{
  if (tree_int_cst_sgn (t) < 0)
    return -1;
  else if (TREE_INT_CST_HIGH (t) != 0)
    return 1;
  else if (TREE_INT_CST_LOW (t) == u)
    return 0;
  else if (TREE_INT_CST_LOW (t) < u)
    return -1;
  else
    return 1;
}

/* Return true if CODE represents an associative tree code.  Otherwise
   return false.  */
bool
associative_tree_code (enum tree_code code)
{
  switch (code)
    {
    case BIT_IOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_XOR_EXPR:
    case PLUS_EXPR:
    case MULT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
      return true;

    default:
      break;
    }
  return false;
}

/* Return true if CODE represents a commutative tree code.  Otherwise
   return false.  */
bool
commutative_tree_code (enum tree_code code)
{
  switch (code)
    {
    case PLUS_EXPR:
    case MULT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case NE_EXPR:
    case EQ_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_OR_EXPR:
      return true;

    default:
      break;
    }
  return false;
}

/* Generate a hash value for an expression.  This can be used iteratively
   by passing a previous result as the "val" argument.

   This function is intended to produce the same hash for expressions which
   would compare equal using operand_equal_p.  */

hashval_t
iterative_hash_expr (tree t, hashval_t val)
{
  int i;
  enum tree_code code;
  char class;

  if (t == NULL_TREE)
    return iterative_hash_pointer (t, val);

  code = TREE_CODE (t);

  switch (code)
    {
    /* Alas, constants aren't shared, so we can't rely on pointer
       identity.  */
    case INTEGER_CST:
      val = iterative_hash_host_wide_int (TREE_INT_CST_LOW (t), val);
      return iterative_hash_host_wide_int (TREE_INT_CST_HIGH (t), val);
    case REAL_CST:
      {
	unsigned int val2 = real_hash (TREE_REAL_CST_PTR (t));

	return iterative_hash_hashval_t (val2, val);
      }
    case STRING_CST:
      return iterative_hash (TREE_STRING_POINTER (t),
			     TREE_STRING_LENGTH (t), val);
    case COMPLEX_CST:
      val = iterative_hash_expr (TREE_REALPART (t), val);
      return iterative_hash_expr (TREE_IMAGPART (t), val);
    case VECTOR_CST:
      return iterative_hash_expr (TREE_VECTOR_CST_ELTS (t), val);

    case SSA_NAME:
    case VALUE_HANDLE:
      /* we can just compare by pointer.  */
      return iterative_hash_pointer (t, val);

    case TREE_LIST:
      /* A list of expressions, for a CALL_EXPR or as the elements of a
	 VECTOR_CST.  */
      for (; t; t = TREE_CHAIN (t))
	val = iterative_hash_expr (TREE_VALUE (t), val);
      return val;
    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	tree field, value;
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), idx, field, value)
	  {
	    val = iterative_hash_expr (field, val);
	    val = iterative_hash_expr (value, val);
	  }
	return val;
      }
    case FUNCTION_DECL:
      /* When referring to a built-in FUNCTION_DECL, use the
	 __builtin__ form.  Otherwise nodes that compare equal
	 according to operand_equal_p might get different
	 hash codes.  */
      if (DECL_BUILT_IN (t))
	{
	  val = iterative_hash_pointer (built_in_decls[DECL_FUNCTION_CODE (t)], 
				      val);
	  return val;
	}
      /* else FALL THROUGH */
    default:
      class = TREE_CODE_CLASS (code);

      if (class == tcc_declaration)
	{
	  /* DECL's have a unique ID */
	  val = iterative_hash_host_wide_int (DECL_UID (t), val);
	}
      else
	{
	  gcc_assert (IS_EXPR_CODE_CLASS (class));
	  
	  val = iterative_hash_object (code, val);

	  /* Don't hash the type, that can lead to having nodes which
	     compare equal according to operand_equal_p, but which
	     have different hash codes.  */
	  if (code == NOP_EXPR
	      || code == CONVERT_EXPR
	      || code == NON_LVALUE_EXPR)
	    {
	      /* Make sure to include signness in the hash computation.  */
	      val += TYPE_UNSIGNED (TREE_TYPE (t));
	      val = iterative_hash_expr (TREE_OPERAND (t, 0), val);
	    }

	  else if (commutative_tree_code (code))
	    {
	      /* It's a commutative expression.  We want to hash it the same
		 however it appears.  We do this by first hashing both operands
		 and then rehashing based on the order of their independent
		 hashes.  */
	      hashval_t one = iterative_hash_expr (TREE_OPERAND (t, 0), 0);
	      hashval_t two = iterative_hash_expr (TREE_OPERAND (t, 1), 0);
	      hashval_t t;

	      if (one > two)
		t = one, one = two, two = t;

	      val = iterative_hash_hashval_t (one, val);
	      val = iterative_hash_hashval_t (two, val);
	    }
	  else
	    for (i = TREE_CODE_LENGTH (code) - 1; i >= 0; --i)
	      val = iterative_hash_expr (TREE_OPERAND (t, i), val);
	}
      return val;
      break;
    }
}

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

/* Construct, lay out and return the type of pointers to TO_TYPE with
   mode MODE.  If CAN_ALIAS_ALL is TRUE, indicate this type can
   reference all of memory. If such a type has already been
   constructed, reuse it.  */

tree
build_pointer_type_for_mode (tree to_type, enum machine_mode mode,
			     bool can_alias_all)
{
  tree t;

  if (to_type == error_mark_node)
    return error_mark_node;

  /* In some cases, languages will have things that aren't a POINTER_TYPE
     (such as a RECORD_TYPE for fat pointers in Ada) as TYPE_POINTER_TO.
     In that case, return that type without regard to the rest of our
     operands.

     ??? This is a kludge, but consistent with the way this function has
     always operated and there doesn't seem to be a good way to avoid this
     at the moment.  */
  if (TYPE_POINTER_TO (to_type) != 0
      && TREE_CODE (TYPE_POINTER_TO (to_type)) != POINTER_TYPE)
    return TYPE_POINTER_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE and it's
     the proper mode, use it.  */
  for (t = TYPE_POINTER_TO (to_type); t; t = TYPE_NEXT_PTR_TO (t))
    if (TYPE_MODE (t) == mode && TYPE_REF_CAN_ALIAS_ALL (t) == can_alias_all)
      return t;

  t = make_node (POINTER_TYPE);

  TREE_TYPE (t) = to_type;
  TYPE_MODE (t) = mode;
  TYPE_REF_CAN_ALIAS_ALL (t) = can_alias_all;
  TYPE_NEXT_PTR_TO (t) = TYPE_POINTER_TO (to_type);
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.  */
  layout_type (t);

  return t;
}

/* By default build pointers in ptr_mode.  */

tree
build_pointer_type (tree to_type)
{
  return build_pointer_type_for_mode (to_type, ptr_mode, false);
}

/* Same as build_pointer_type_for_mode, but for REFERENCE_TYPE.  */

tree
build_reference_type_for_mode (tree to_type, enum machine_mode mode,
			       bool can_alias_all)
{
  tree t;

  /* In some cases, languages will have things that aren't a REFERENCE_TYPE
     (such as a RECORD_TYPE for fat pointers in Ada) as TYPE_REFERENCE_TO.
     In that case, return that type without regard to the rest of our
     operands.

     ??? This is a kludge, but consistent with the way this function has
     always operated and there doesn't seem to be a good way to avoid this
     at the moment.  */
  if (TYPE_REFERENCE_TO (to_type) != 0
      && TREE_CODE (TYPE_REFERENCE_TO (to_type)) != REFERENCE_TYPE)
    return TYPE_REFERENCE_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE and it's
     the proper mode, use it.  */
  for (t = TYPE_REFERENCE_TO (to_type); t; t = TYPE_NEXT_REF_TO (t))
    if (TYPE_MODE (t) == mode && TYPE_REF_CAN_ALIAS_ALL (t) == can_alias_all)
      return t;

  t = make_node (REFERENCE_TYPE);

  TREE_TYPE (t) = to_type;
  TYPE_MODE (t) = mode;
  TYPE_REF_CAN_ALIAS_ALL (t) = can_alias_all;
  TYPE_NEXT_REF_TO (t) = TYPE_REFERENCE_TO (to_type);
  TYPE_REFERENCE_TO (to_type) = t;

  layout_type (t);

  return t;
}


/* Build the node for the type of references-to-TO_TYPE by default
   in ptr_mode.  */

tree
build_reference_type (tree to_type)
{
  return build_reference_type_for_mode (to_type, ptr_mode, false);
}

/* Build a type that is compatible with t but has no cv quals anywhere
   in its type, thus

   const char *const *const *  ->  char ***.  */

tree
build_type_no_quals (tree t)
{
  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      return build_pointer_type_for_mode (build_type_no_quals (TREE_TYPE (t)),
					  TYPE_MODE (t),
					  TYPE_REF_CAN_ALIAS_ALL (t));
    case REFERENCE_TYPE:
      return
	build_reference_type_for_mode (build_type_no_quals (TREE_TYPE (t)),
				       TYPE_MODE (t),
				       TYPE_REF_CAN_ALIAS_ALL (t));
    default:
      return TYPE_MAIN_VARIANT (t);
    }
}

/* Create a type of integers to be the TYPE_DOMAIN of an ARRAY_TYPE.
   MAXVAL should be the maximum value in the domain
   (one less than the length of the array).

   The maximum value that MAXVAL can have is INT_MAX for a HOST_WIDE_INT.
   We don't enforce this limit, that is up to caller (e.g. language front end).
   The limit exists because the result is a signed type and we don't handle
   sizes that use more than one HOST_WIDE_INT.  */

tree
build_index_type (tree maxval)
{
  tree itype = make_node (INTEGER_TYPE);

  TREE_TYPE (itype) = sizetype;
  TYPE_PRECISION (itype) = TYPE_PRECISION (sizetype);
  TYPE_MIN_VALUE (itype) = size_zero_node;
  TYPE_MAX_VALUE (itype) = fold_convert (sizetype, maxval);
  TYPE_MODE (itype) = TYPE_MODE (sizetype);
  TYPE_SIZE (itype) = TYPE_SIZE (sizetype);
  TYPE_SIZE_UNIT (itype) = TYPE_SIZE_UNIT (sizetype);
  TYPE_ALIGN (itype) = TYPE_ALIGN (sizetype);
  TYPE_USER_ALIGN (itype) = TYPE_USER_ALIGN (sizetype);

  if (host_integerp (maxval, 1))
    return type_hash_canon (tree_low_cst (maxval, 1), itype);
  else
    return itype;
}

/* Builds a signed or unsigned integer type of precision PRECISION.
   Used for C bitfields whose precision does not match that of
   built-in target types.  */
tree
build_nonstandard_integer_type (unsigned HOST_WIDE_INT precision,
				int unsignedp)
{
  tree itype = make_node (INTEGER_TYPE);

  TYPE_PRECISION (itype) = precision;

  if (unsignedp)
    fixup_unsigned_type (itype);
  else
    fixup_signed_type (itype);

  if (host_integerp (TYPE_MAX_VALUE (itype), 1))
    return type_hash_canon (tree_low_cst (TYPE_MAX_VALUE (itype), 1), itype);

  return itype;
}

/* Create a range of some discrete type TYPE (an INTEGER_TYPE,
   ENUMERAL_TYPE or BOOLEAN_TYPE), with low bound LOWVAL and
   high bound HIGHVAL.  If TYPE is NULL, sizetype is used.  */

tree
build_range_type (tree type, tree lowval, tree highval)
{
  tree itype = make_node (INTEGER_TYPE);

  TREE_TYPE (itype) = type;
  if (type == NULL_TREE)
    type = sizetype;

  TYPE_MIN_VALUE (itype) = fold_convert (type, lowval);
  TYPE_MAX_VALUE (itype) = highval ? fold_convert (type, highval) : NULL;

  TYPE_PRECISION (itype) = TYPE_PRECISION (type);
  TYPE_MODE (itype) = TYPE_MODE (type);
  TYPE_SIZE (itype) = TYPE_SIZE (type);
  TYPE_SIZE_UNIT (itype) = TYPE_SIZE_UNIT (type);
  TYPE_ALIGN (itype) = TYPE_ALIGN (type);
  TYPE_USER_ALIGN (itype) = TYPE_USER_ALIGN (type);

  if (host_integerp (lowval, 0) && highval != 0 && host_integerp (highval, 0))
    return type_hash_canon (tree_low_cst (highval, 0)
			    - tree_low_cst (lowval, 0),
			    itype);
  else
    return itype;
}

/* Just like build_index_type, but takes lowval and highval instead
   of just highval (maxval).  */

tree
build_index_2_type (tree lowval, tree highval)
{
  return build_range_type (sizetype, lowval, highval);
}

/* Construct, lay out and return the type of arrays of elements with ELT_TYPE
   and number of elements specified by the range of values of INDEX_TYPE.
   If such a type has already been constructed, reuse it.  */

tree
build_array_type (tree elt_type, tree index_type)
{
  tree t;
  hashval_t hashcode = 0;

  if (TREE_CODE (elt_type) == FUNCTION_TYPE)
    {
      error ("arrays of functions are not meaningful");
      elt_type = integer_type_node;
    }

  t = make_node (ARRAY_TYPE);
  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;
  
  if (index_type == 0)
    {
      tree save = t;
      hashcode = iterative_hash_object (TYPE_HASH (elt_type), hashcode);
      t = type_hash_canon (hashcode, t);
      if (save == t)
	layout_type (t);
      return t;
    }

  hashcode = iterative_hash_object (TYPE_HASH (elt_type), hashcode);
  hashcode = iterative_hash_object (TYPE_HASH (index_type), hashcode);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);
  return t;
}

/* Return the TYPE of the elements comprising
   the innermost dimension of ARRAY.  */

tree
get_inner_array_type (tree array)
{
  tree type = TREE_TYPE (array);

  while (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  return type;
}

/* Construct, lay out and return
   the type of functions returning type VALUE_TYPE
   given arguments of types ARG_TYPES.
   ARG_TYPES is a chain of TREE_LIST nodes whose TREE_VALUEs
   are data type nodes for the arguments of the function.
   If such a type has already been constructed, reuse it.  */

tree
build_function_type (tree value_type, tree arg_types)
{
  tree t;
  hashval_t hashcode = 0;

  if (TREE_CODE (value_type) == FUNCTION_TYPE)
    {
      error ("function return type cannot be function");
      value_type = integer_type_node;
    }

  /* Make a node of the sort we want.  */
  t = make_node (FUNCTION_TYPE);
  TREE_TYPE (t) = value_type;
  TYPE_ARG_TYPES (t) = arg_types;

  /* If we already have such a type, use the old one.  */
  hashcode = iterative_hash_object (TYPE_HASH (value_type), hashcode);
  hashcode = type_hash_list (arg_types, hashcode);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);
  return t;
}

/* Build a function type.  The RETURN_TYPE is the type returned by the
   function.  If additional arguments are provided, they are
   additional argument types.  The list of argument types must always
   be terminated by NULL_TREE.  */

tree
build_function_type_list (tree return_type, ...)
{
  tree t, args, last;
  va_list p;

  va_start (p, return_type);

  t = va_arg (p, tree);
  for (args = NULL_TREE; t != NULL_TREE; t = va_arg (p, tree))
    args = tree_cons (NULL_TREE, t, args);

  if (args == NULL_TREE)
    args = void_list_node;
  else
    {
      last = args;
      args = nreverse (args);
      TREE_CHAIN (last) = void_list_node;
    }
  args = build_function_type (return_type, args);

  va_end (p);
  return args;
}

/* Build a METHOD_TYPE for a member of BASETYPE.  The RETTYPE (a TYPE)
   and ARGTYPES (a TREE_LIST) are the return type and arguments types
   for the method.  An implicit additional parameter (of type
   pointer-to-BASETYPE) is added to the ARGTYPES.  */

tree
build_method_type_directly (tree basetype,
			    tree rettype,
			    tree argtypes)
{
  tree t;
  tree ptype;
  int hashcode = 0;

  /* Make a node of the sort we want.  */
  t = make_node (METHOD_TYPE);

  TYPE_METHOD_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = rettype;
  ptype = build_pointer_type (basetype);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  */
  argtypes = tree_cons (NULL_TREE, ptype, argtypes);
  TYPE_ARG_TYPES (t) = argtypes;

  /* If we already have such a type, use the old one.  */
  hashcode = iterative_hash_object (TYPE_HASH (basetype), hashcode);
  hashcode = iterative_hash_object (TYPE_HASH (rettype), hashcode);
  hashcode = type_hash_list (argtypes, hashcode);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  return t;
}

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments and values are described by TYPE.
   If that type exists already, reuse it.
   TYPE must be a FUNCTION_TYPE node.  */

tree
build_method_type (tree basetype, tree type)
{
  gcc_assert (TREE_CODE (type) == FUNCTION_TYPE);

  return build_method_type_directly (basetype,
				     TREE_TYPE (type),
				     TYPE_ARG_TYPES (type));
}

/* Construct, lay out and return the type of offsets to a value
   of type TYPE, within an object of type BASETYPE.
   If a suitable offset type exists already, reuse it.  */

tree
build_offset_type (tree basetype, tree type)
{
  tree t;
  hashval_t hashcode = 0;

  /* Make a node of the sort we want.  */
  t = make_node (OFFSET_TYPE);

  TYPE_OFFSET_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = type;

  /* If we already have such a type, use the old one.  */
  hashcode = iterative_hash_object (TYPE_HASH (basetype), hashcode);
  hashcode = iterative_hash_object (TYPE_HASH (type), hashcode);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  return t;
}

/* Create a complex type whose components are COMPONENT_TYPE.  */

tree
build_complex_type (tree component_type)
{
  tree t;
  hashval_t hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (COMPLEX_TYPE);

  TREE_TYPE (t) = TYPE_MAIN_VARIANT (component_type);

  /* If we already have such a type, use the old one.  */
  hashcode = iterative_hash_object (TYPE_HASH (component_type), 0);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  /* If we are writing Dwarf2 output we need to create a name,
     since complex is a fundamental type.  */
  if ((write_symbols == DWARF2_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
      && ! TYPE_NAME (t))
    {
      const char *name;
      if (component_type == char_type_node)
	name = "complex char";
      else if (component_type == signed_char_type_node)
	name = "complex signed char";
      else if (component_type == unsigned_char_type_node)
	name = "complex unsigned char";
      else if (component_type == short_integer_type_node)
	name = "complex short int";
      else if (component_type == short_unsigned_type_node)
	name = "complex short unsigned int";
      else if (component_type == integer_type_node)
	name = "complex int";
      else if (component_type == unsigned_type_node)
	name = "complex unsigned int";
      else if (component_type == long_integer_type_node)
	name = "complex long int";
      else if (component_type == long_unsigned_type_node)
	name = "complex long unsigned int";
      else if (component_type == long_long_integer_type_node)
	name = "complex long long int";
      else if (component_type == long_long_unsigned_type_node)
	name = "complex long long unsigned int";
      else
	name = 0;

      if (name != 0)
	TYPE_NAME (t) = get_identifier (name);
    }

  return build_qualified_type (t, TYPE_QUALS (component_type));
}

/* Return OP, stripped of any conversions to wider types as much as is safe.
   Converting the value back to OP's type makes a value equivalent to OP.

   If FOR_TYPE is nonzero, we return a value which, if converted to
   type FOR_TYPE, would be equivalent to converting OP to type FOR_TYPE.

   If FOR_TYPE is nonzero, unaligned bit-field references may be changed to the
   narrowest type that can hold the value, even if they don't exactly fit.
   Otherwise, bit-field references are changed to a narrower type
   only if they can be fetched directly from memory in that type.

   OP must have integer, real or enumeral type.  Pointers are not allowed!

   There are some cases where the obvious value we could return
   would regenerate to OP if converted to OP's type,
   but would not extend like OP to wider types.
   If FOR_TYPE indicates such extension is contemplated, we eschew such values.
   For example, if OP is (unsigned short)(signed char)-1,
   we avoid returning (signed char)-1 if FOR_TYPE is int,
   even though extending that to an unsigned short would regenerate OP,
   since the result of extending (signed char)-1 to (int)
   is different from (int) OP.  */

tree
get_unwidened (tree op, tree for_type)
{
  /* Set UNS initially if converting OP to FOR_TYPE is a zero-extension.  */
  tree type = TREE_TYPE (op);
  unsigned final_prec
    = TYPE_PRECISION (for_type != 0 ? for_type : type);
  int uns
    = (for_type != 0 && for_type != type
       && final_prec > TYPE_PRECISION (type)
       && TYPE_UNSIGNED (type));
  tree win = op;

  while (TREE_CODE (op) == NOP_EXPR
	 || TREE_CODE (op) == CONVERT_EXPR)
    {
      int bitschange;

      /* TYPE_PRECISION on vector types has different meaning
	 (TYPE_VECTOR_SUBPARTS) and casts from vectors are view conversions,
	 so avoid them here.  */
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (op, 0))) == VECTOR_TYPE)
	break;

      bitschange = TYPE_PRECISION (TREE_TYPE (op))
		   - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.
	 Unless we are later going to truncate down even farther.  */
      if (bitschange < 0
	  && final_prec > TYPE_PRECISION (TREE_TYPE (op)))
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      /* If we have not stripped any zero-extensions (uns is 0),
	 we can strip any kind of extension.
	 If we have previously stripped a zero-extension,
	 only zero-extensions can safely be stripped.
	 Any extension can be stripped if the bits it would produce
	 are all going to be discarded later by truncating to FOR_TYPE.  */

      if (bitschange > 0)
	{
	  if (! uns || final_prec <= TYPE_PRECISION (TREE_TYPE (op)))
	    win = op;
	  /* TYPE_UNSIGNED says whether this is a zero-extension.
	     Let's avoid computing it if it does not affect WIN
	     and if UNS will not be needed again.  */
	  if ((uns
	       || TREE_CODE (op) == NOP_EXPR
	       || TREE_CODE (op) == CONVERT_EXPR)
	      && TYPE_UNSIGNED (TREE_TYPE (op)))
	    {
	      uns = 1;
	      win = op;
	    }
	}
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (type) != REAL_TYPE
      /* Don't crash if field not laid out yet.  */
      && DECL_SIZE (TREE_OPERAND (op, 1)) != 0
      && host_integerp (DECL_SIZE (TREE_OPERAND (op, 1)), 1))
    {
      unsigned int innerprec
	= tree_low_cst (DECL_SIZE (TREE_OPERAND (op, 1)), 1);
      int unsignedp = (DECL_UNSIGNED (TREE_OPERAND (op, 1))
		       || TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op, 1))));
      type = lang_hooks.types.type_for_size (innerprec, unsignedp);

      /* We can get this structure field in the narrowest type it fits in.
	 If FOR_TYPE is 0, do this only for a field that matches the
	 narrower type exactly and is aligned for it
	 The resulting extension to its nominal type (a fullword type)
	 must fit the same conditions as for other extensions.  */

      if (type != 0
	  && INT_CST_LT_UNSIGNED (TYPE_SIZE (type), TYPE_SIZE (TREE_TYPE (op)))
	  && (for_type || ! DECL_BIT_FIELD (TREE_OPERAND (op, 1)))
	  && (! uns || final_prec <= innerprec || unsignedp))
	{
	  win = build3 (COMPONENT_REF, type, TREE_OPERAND (op, 0),
			TREE_OPERAND (op, 1), NULL_TREE);
	  TREE_SIDE_EFFECTS (win) = TREE_SIDE_EFFECTS (op);
	  TREE_THIS_VOLATILE (win) = TREE_THIS_VOLATILE (op);
	}
    }

  return win;
}

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

tree
get_narrower (tree op, int *unsignedp_ptr)
{
  int uns = 0;
  int first = 1;
  tree win = op;
  bool integral_p = INTEGRAL_TYPE_P (TREE_TYPE (op));

  while (TREE_CODE (op) == NOP_EXPR)
    {
      int bitschange
	= (TYPE_PRECISION (TREE_TYPE (op))
	   - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0))));

      /* Truncations are many-one so cannot be removed.  */
      if (bitschange < 0)
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */

      if (bitschange > 0)
	{
	  op = TREE_OPERAND (op, 0);
	  /* An extension: the outermost one can be stripped,
	     but remember whether it is zero or sign extension.  */
	  if (first)
	    uns = TYPE_UNSIGNED (TREE_TYPE (op));
	  /* Otherwise, if a sign extension has been stripped,
	     only sign extensions can now be stripped;
	     if a zero extension has been stripped, only zero-extensions.  */
	  else if (uns != TYPE_UNSIGNED (TREE_TYPE (op)))
	    break;
	  first = 0;
	}
      else /* bitschange == 0 */
	{
	  /* A change in nominal type can always be stripped, but we must
	     preserve the unsignedness.  */
	  if (first)
	    uns = TYPE_UNSIGNED (TREE_TYPE (op));
	  first = 0;
	  op = TREE_OPERAND (op, 0);
	  /* Keep trying to narrow, but don't assign op to win if it
	     would turn an integral type into something else.  */
	  if (INTEGRAL_TYPE_P (TREE_TYPE (op)) != integral_p)
	    continue;
	}

      win = op;
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (TREE_TYPE (op)) != REAL_TYPE
      /* Ensure field is laid out already.  */
      && DECL_SIZE (TREE_OPERAND (op, 1)) != 0
      && host_integerp (DECL_SIZE (TREE_OPERAND (op, 1)), 1))
    {
      unsigned HOST_WIDE_INT innerprec
	= tree_low_cst (DECL_SIZE (TREE_OPERAND (op, 1)), 1);
      int unsignedp = (DECL_UNSIGNED (TREE_OPERAND (op, 1))
		       || TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op, 1))));
      tree type = lang_hooks.types.type_for_size (innerprec, unsignedp);

      /* We can get this structure field in a narrower type that fits it,
	 but the resulting extension to its nominal type (a fullword type)
	 must satisfy the same conditions as for other extensions.

	 Do this only for fields that are aligned (not bit-fields),
	 because when bit-field insns will be used there is no
	 advantage in doing this.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && ! DECL_BIT_FIELD (TREE_OPERAND (op, 1))
	  && (first || uns == DECL_UNSIGNED (TREE_OPERAND (op, 1)))
	  && type != 0)
	{
	  if (first)
	    uns = DECL_UNSIGNED (TREE_OPERAND (op, 1));
	  win = fold_convert (type, op);
	}
    }

  *unsignedp_ptr = uns;
  return win;
}

/* Nonzero if integer constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

int
int_fits_type_p (tree c, tree type)
{
  tree type_low_bound = TYPE_MIN_VALUE (type);
  tree type_high_bound = TYPE_MAX_VALUE (type);
  bool ok_for_low_bound, ok_for_high_bound;
  tree tmp;

  /* If at least one bound of the type is a constant integer, we can check
     ourselves and maybe make a decision. If no such decision is possible, but
     this type is a subtype, try checking against that.  Otherwise, use
     force_fit_type, which checks against the precision.

     Compute the status for each possibly constant bound, and return if we see
     one does not match. Use ok_for_xxx_bound for this purpose, assigning -1
     for "unknown if constant fits", 0 for "constant known *not* to fit" and 1
     for "constant known to fit".  */

  /* Check if C >= type_low_bound.  */
  if (type_low_bound && TREE_CODE (type_low_bound) == INTEGER_CST)
    {
      if (tree_int_cst_lt (c, type_low_bound))
	return 0;
      ok_for_low_bound = true;
    }
  else
    ok_for_low_bound = false;

  /* Check if c <= type_high_bound.  */
  if (type_high_bound && TREE_CODE (type_high_bound) == INTEGER_CST)
    {
      if (tree_int_cst_lt (type_high_bound, c))
	return 0;
      ok_for_high_bound = true;
    }
  else
    ok_for_high_bound = false;

  /* If the constant fits both bounds, the result is known.  */
  if (ok_for_low_bound && ok_for_high_bound)
    return 1;

  /* Perform some generic filtering which may allow making a decision
     even if the bounds are not constant.  First, negative integers
     never fit in unsigned types, */
  if (TYPE_UNSIGNED (type) && tree_int_cst_sgn (c) < 0)
    return 0;

  /* Second, narrower types always fit in wider ones.  */
  if (TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (c)))
    return 1;

  /* Third, unsigned integers with top bit set never fit signed types.  */
  if (! TYPE_UNSIGNED (type)
      && TYPE_UNSIGNED (TREE_TYPE (c))
      && tree_int_cst_msb (c))
    return 0;

  /* If we haven't been able to decide at this point, there nothing more we
     can check ourselves here.  Look at the base type if we have one and it
     has the same precision.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && TREE_TYPE (type) != 0
      && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (type)))
    return int_fits_type_p (c, TREE_TYPE (type));

  /* Or to force_fit_type, if nothing else.  */
  tmp = copy_node (c);
  TREE_TYPE (tmp) = type;
  tmp = force_fit_type (tmp, -1, false, false);
  return TREE_INT_CST_HIGH (tmp) == TREE_INT_CST_HIGH (c)
         && TREE_INT_CST_LOW (tmp) == TREE_INT_CST_LOW (c);
}

/* Subprogram of following function.  Called by walk_tree.

   Return *TP if it is an automatic variable or parameter of the
   function passed in as DATA.  */

static tree
find_var_from_fn (tree *tp, int *walk_subtrees, void *data)
{
  tree fn = (tree) data;

  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  else if (DECL_P (*tp)
	   && lang_hooks.tree_inlining.auto_var_in_fn_p (*tp, fn))
    return *tp;

  return NULL_TREE;
}

/* Returns true if T is, contains, or refers to a type with variable
   size.  For METHOD_TYPEs and FUNCTION_TYPEs we exclude the
   arguments, but not the return type.  If FN is nonzero, only return
   true if a modifier of the type or position of FN is a variable or
   parameter inside FN.

   This concept is more general than that of C99 'variably modified types':
   in C99, a struct type is never variably modified because a VLA may not
   appear as a structure member.  However, in GNU C code like:

     struct S { int i[f()]; };

   is valid, and other languages may define similar constructs.  */

bool
variably_modified_type_p (tree type, tree fn)
{
  tree t;

/* Test if T is either variable (if FN is zero) or an expression containing
   a variable in FN.  */
#define RETURN_TRUE_IF_VAR(T)						\
  do { tree _t = (T);							\
    if (_t && _t != error_mark_node && TREE_CODE (_t) != INTEGER_CST	\
        && (!fn || walk_tree (&_t, find_var_from_fn, fn, NULL)))	\
      return true;  } while (0)

  if (type == error_mark_node)
    return false;

  /* If TYPE itself has variable size, it is variably modified.  */
  RETURN_TRUE_IF_VAR (TYPE_SIZE (type));
  RETURN_TRUE_IF_VAR (TYPE_SIZE_UNIT (type));

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case VECTOR_TYPE:
      if (variably_modified_type_p (TREE_TYPE (type), fn))
	return true;
      break;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      /* If TYPE is a function type, it is variably modified if the
	 return type is variably modified.  */
      if (variably_modified_type_p (TREE_TYPE (type), fn))
	  return true;
      break;

    case INTEGER_TYPE:
    case REAL_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      /* Scalar types are variably modified if their end points
	 aren't constant.  */
      RETURN_TRUE_IF_VAR (TYPE_MIN_VALUE (type));
      RETURN_TRUE_IF_VAR (TYPE_MAX_VALUE (type));
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      /* We can't see if any of the fields are variably-modified by the
	 definition we normally use, since that would produce infinite
	 recursion via pointers.  */
      /* This is variably modified if some field's type is.  */
      for (t = TYPE_FIELDS (type); t; t = TREE_CHAIN (t))
	if (TREE_CODE (t) == FIELD_DECL)
	  {
	    RETURN_TRUE_IF_VAR (DECL_FIELD_OFFSET (t));
	    RETURN_TRUE_IF_VAR (DECL_SIZE (t));
	    RETURN_TRUE_IF_VAR (DECL_SIZE_UNIT (t));

	    if (TREE_CODE (type) == QUAL_UNION_TYPE)
	      RETURN_TRUE_IF_VAR (DECL_QUALIFIER (t));
	  }
	break;

    case ARRAY_TYPE:
      /* Do not call ourselves to avoid infinite recursion.  This is
	 variably modified if the element type is.  */
      RETURN_TRUE_IF_VAR (TYPE_SIZE (TREE_TYPE (type)));
      RETURN_TRUE_IF_VAR (TYPE_SIZE_UNIT (TREE_TYPE (type)));
      break;

    default:
      break;
    }

  /* The current language may have other cases to check, but in general,
     all other types are not variably modified.  */
  return lang_hooks.tree_inlining.var_mod_type_p (type, fn);

#undef RETURN_TRUE_IF_VAR
}

/* Given a DECL or TYPE, return the scope in which it was declared, or
   NULL_TREE if there is no containing scope.  */

tree
get_containing_scope (tree t)
{
  return (TYPE_P (t) ? TYPE_CONTEXT (t) : DECL_CONTEXT (t));
}

/* Return the innermost context enclosing DECL that is
   a FUNCTION_DECL, or zero if none.  */

tree
decl_function_context (tree decl)
{
  tree context;

  if (TREE_CODE (decl) == ERROR_MARK)
    return 0;

  /* C++ virtual functions use DECL_CONTEXT for the class of the vtable
     where we look up the function at runtime.  Such functions always take
     a first argument of type 'pointer to real context'.

     C++ should really be fixed to use DECL_CONTEXT for the real context,
     and use something else for the "virtual context".  */
  else if (TREE_CODE (decl) == FUNCTION_DECL && DECL_VINDEX (decl))
    context
      = TYPE_MAIN_VARIANT
	(TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (decl)))));
  else
    context = DECL_CONTEXT (decl);

  while (context && TREE_CODE (context) != FUNCTION_DECL)
    {
      if (TREE_CODE (context) == BLOCK)
	context = BLOCK_SUPERCONTEXT (context);
      else
	context = get_containing_scope (context);
    }

  return context;
}

/* Return the innermost context enclosing DECL that is
   a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE, or zero if none.
   TYPE_DECLs and FUNCTION_DECLs are transparent to this function.  */

tree
decl_type_context (tree decl)
{
  tree context = DECL_CONTEXT (decl);

  while (context)
    switch (TREE_CODE (context))
      {
      case NAMESPACE_DECL:
      case TRANSLATION_UNIT_DECL:
	return NULL_TREE;

      case RECORD_TYPE:
      case UNION_TYPE:
      case QUAL_UNION_TYPE:
	return context;

      case TYPE_DECL:
      case FUNCTION_DECL:
	context = DECL_CONTEXT (context);
	break;

      case BLOCK:
	context = BLOCK_SUPERCONTEXT (context);
	break;

      default:
	gcc_unreachable ();
      }

  return NULL_TREE;
}

/* CALL is a CALL_EXPR.  Return the declaration for the function
   called, or NULL_TREE if the called function cannot be
   determined.  */

tree
get_callee_fndecl (tree call)
{
  tree addr;

  if (call == error_mark_node)
    return call;

  /* It's invalid to call this function with anything but a
     CALL_EXPR.  */
  gcc_assert (TREE_CODE (call) == CALL_EXPR);

  /* The first operand to the CALL is the address of the function
     called.  */
  addr = TREE_OPERAND (call, 0);

  STRIP_NOPS (addr);

  /* If this is a readonly function pointer, extract its initial value.  */
  if (DECL_P (addr) && TREE_CODE (addr) != FUNCTION_DECL
      && TREE_READONLY (addr) && ! TREE_THIS_VOLATILE (addr)
      && DECL_INITIAL (addr))
    addr = DECL_INITIAL (addr);

  /* If the address is just `&f' for some function `f', then we know
     that `f' is being called.  */
  if (TREE_CODE (addr) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (addr, 0)) == FUNCTION_DECL)
    return TREE_OPERAND (addr, 0);

  /* We couldn't figure out what was being called.  Maybe the front
     end has some idea.  */
  return lang_hooks.lang_get_callee_fndecl (call);
}

/* Print debugging information about tree nodes generated during the compile,
   and any language-specific information.  */

void
dump_tree_statistics (void)
{
#ifdef GATHER_STATISTICS
  int i;
  int total_nodes, total_bytes;
#endif

  fprintf (stderr, "\n??? tree nodes created\n\n");
#ifdef GATHER_STATISTICS
  fprintf (stderr, "Kind                   Nodes      Bytes\n");
  fprintf (stderr, "---------------------------------------\n");
  total_nodes = total_bytes = 0;
  for (i = 0; i < (int) all_kinds; i++)
    {
      fprintf (stderr, "%-20s %7d %10d\n", tree_node_kind_names[i],
	       tree_node_counts[i], tree_node_sizes[i]);
      total_nodes += tree_node_counts[i];
      total_bytes += tree_node_sizes[i];
    }
  fprintf (stderr, "---------------------------------------\n");
  fprintf (stderr, "%-20s %7d %10d\n", "Total", total_nodes, total_bytes);
  fprintf (stderr, "---------------------------------------\n");
  ssanames_print_statistics ();
  phinodes_print_statistics ();
#else
  fprintf (stderr, "(No per-node statistics)\n");
#endif
  print_type_hash_statistics ();
  print_debug_expr_statistics ();
  print_value_expr_statistics ();
  print_restrict_base_statistics ();
  lang_hooks.print_statistics ();
}

#define FILE_FUNCTION_FORMAT "_GLOBAL__%s_%s"

/* Generate a crc32 of a string.  */

unsigned
crc32_string (unsigned chksum, const char *string)
{
  do
    {
      unsigned value = *string << 24;
      unsigned ix;

      for (ix = 8; ix--; value <<= 1)
  	{
  	  unsigned feedback;

  	  feedback = (value ^ chksum) & 0x80000000 ? 0x04c11db7 : 0;
 	  chksum <<= 1;
 	  chksum ^= feedback;
  	}
    }
  while (*string++);
  return chksum;
}

/* P is a string that will be used in a symbol.  Mask out any characters
   that are not valid in that context.  */

void
clean_symbol_name (char *p)
{
  for (; *p; p++)
    if (! (ISALNUM (*p)
#ifndef NO_DOLLAR_IN_LABEL	/* this for `$'; unlikely, but... -- kr */
	    || *p == '$'
#endif
#ifndef NO_DOT_IN_LABEL		/* this for `.'; unlikely, but...  */
	    || *p == '.'
#endif
	   ))
      *p = '_';
}

/* Generate a name for a function unique to this translation unit.
   TYPE is some string to identify the purpose of this function to the
   linker or collect2.  */

tree
get_file_function_name_long (const char *type)
{
  char *buf;
  const char *p;
  char *q;

  if (first_global_object_name)
    {
      p = first_global_object_name;

      /* For type 'F', the generated name must be unique not only to this
	 translation unit but also to any given link.  Since global names
	 can be overloaded, we concatenate the first global object name
	 with a string derived from the file name of this object.  */
      if (!strcmp (type, "F"))
	{
	  const char *file = main_input_filename;

	  if (! file)
	    file = input_filename;

	  q = alloca (strlen (p) + 10);
	  sprintf (q, "%s_%08X", p, crc32_string (0, file));

	  p = q;
	}
    }
  else
    {
      /* We don't have anything that we know to be unique to this translation
	 unit, so use what we do have and throw in some randomness.  */
      unsigned len;
      const char *name = weak_global_object_name;
      const char *file = main_input_filename;

      if (! name)
	name = "";
      if (! file)
	file = input_filename;

      len = strlen (file);
      q = alloca (9 * 2 + len + 1);
      memcpy (q, file, len + 1);
      clean_symbol_name (q);

      sprintf (q + len, "_%08X_%08X", crc32_string (0, name),
	       crc32_string (0, flag_random_seed));

      p = q;
    }

  buf = alloca (sizeof (FILE_FUNCTION_FORMAT) + strlen (p) + strlen (type));

  /* Set up the name of the file-level functions we may need.
     Use a global object (which is already required to be unique over
     the program) rather than the file name (which imposes extra
     constraints).  */
  sprintf (buf, FILE_FUNCTION_FORMAT, type, p);

  return get_identifier (buf);
}

/* If KIND=='I', return a suitable global initializer (constructor) name.
   If KIND=='D', return a suitable global clean-up (destructor) name.  */

tree
get_file_function_name (int kind)
{
  char p[2];

  p[0] = kind;
  p[1] = 0;

  return get_file_function_name_long (p);
}

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)

/* Complain that the tree code of NODE does not match the expected 0
   terminated list of trailing codes. The trailing code list can be
   empty, for a more vague error message.  FILE, LINE, and FUNCTION
   are of the caller.  */

void
tree_check_failed (const tree node, const char *file,
		   int line, const char *function, ...)
{
  va_list args;
  char *buffer;
  unsigned length = 0;
  int code;

  va_start (args, function);
  while ((code = va_arg (args, int)))
    length += 4 + strlen (tree_code_name[code]);
  va_end (args);
  if (length)
    {
      va_start (args, function);
      length += strlen ("expected ");
      buffer = alloca (length);
      length = 0;
      while ((code = va_arg (args, int)))
	{
	  const char *prefix = length ? " or " : "expected ";
	  
	  strcpy (buffer + length, prefix);
	  length += strlen (prefix);
	  strcpy (buffer + length, tree_code_name[code]);
	  length += strlen (tree_code_name[code]);
	}
      va_end (args);
    }
  else
    buffer = (char *)"unexpected node";

  internal_error ("tree check: %s, have %s in %s, at %s:%d",
		  buffer, tree_code_name[TREE_CODE (node)],
		  function, trim_filename (file), line);
}

/* Complain that the tree code of NODE does match the expected 0
   terminated list of trailing codes. FILE, LINE, and FUNCTION are of
   the caller.  */

void
tree_not_check_failed (const tree node, const char *file,
		       int line, const char *function, ...)
{
  va_list args;
  char *buffer;
  unsigned length = 0;
  int code;

  va_start (args, function);
  while ((code = va_arg (args, int)))
    length += 4 + strlen (tree_code_name[code]);
  va_end (args);
  va_start (args, function);
  buffer = alloca (length);
  length = 0;
  while ((code = va_arg (args, int)))
    {
      if (length)
	{
	  strcpy (buffer + length, " or ");
	  length += 4;
	}
      strcpy (buffer + length, tree_code_name[code]);
      length += strlen (tree_code_name[code]);
    }
  va_end (args);

  internal_error ("tree check: expected none of %s, have %s in %s, at %s:%d",
		  buffer, tree_code_name[TREE_CODE (node)],
		  function, trim_filename (file), line);
}

/* Similar to tree_check_failed, except that we check for a class of tree
   code, given in CL.  */

void
tree_class_check_failed (const tree node, const enum tree_code_class cl,
			 const char *file, int line, const char *function)
{
  internal_error
    ("tree check: expected class %qs, have %qs (%s) in %s, at %s:%d",
     TREE_CODE_CLASS_STRING (cl),
     TREE_CODE_CLASS_STRING (TREE_CODE_CLASS (TREE_CODE (node))),
     tree_code_name[TREE_CODE (node)], function, trim_filename (file), line);
}

/* Similar to tree_check_failed, except that instead of specifying a
   dozen codes, use the knowledge that they're all sequential.  */

void
tree_range_check_failed (const tree node, const char *file, int line,
			 const char *function, enum tree_code c1,
			 enum tree_code c2)
{
  char *buffer;
  unsigned length = 0;
  enum tree_code c;

  for (c = c1; c <= c2; ++c)
    length += 4 + strlen (tree_code_name[c]);

  length += strlen ("expected ");
  buffer = alloca (length);
  length = 0;

  for (c = c1; c <= c2; ++c)
    {
      const char *prefix = length ? " or " : "expected ";

      strcpy (buffer + length, prefix);
      length += strlen (prefix);
      strcpy (buffer + length, tree_code_name[c]);
      length += strlen (tree_code_name[c]);
    }

  internal_error ("tree check: %s, have %s in %s, at %s:%d",
		  buffer, tree_code_name[TREE_CODE (node)],
		  function, trim_filename (file), line);
}


/* Similar to tree_check_failed, except that we check that a tree does
   not have the specified code, given in CL.  */

void
tree_not_class_check_failed (const tree node, const enum tree_code_class cl,
			     const char *file, int line, const char *function)
{
  internal_error
    ("tree check: did not expect class %qs, have %qs (%s) in %s, at %s:%d",
     TREE_CODE_CLASS_STRING (cl),
     TREE_CODE_CLASS_STRING (TREE_CODE_CLASS (TREE_CODE (node))),
     tree_code_name[TREE_CODE (node)], function, trim_filename (file), line);
}


/* Similar to tree_check_failed but applied to OMP_CLAUSE codes.  */

void
omp_clause_check_failed (const tree node, const char *file, int line,
                         const char *function, enum omp_clause_code code)
{
  internal_error ("tree check: expected omp_clause %s, have %s in %s, at %s:%d",
		  omp_clause_code_name[code], tree_code_name[TREE_CODE (node)],
		  function, trim_filename (file), line);
}


/* Similar to tree_range_check_failed but applied to OMP_CLAUSE codes.  */

void
omp_clause_range_check_failed (const tree node, const char *file, int line,
			       const char *function, enum omp_clause_code c1,
			       enum omp_clause_code c2)
{
  char *buffer;
  unsigned length = 0;
  enum omp_clause_code c;

  for (c = c1; c <= c2; ++c)
    length += 4 + strlen (omp_clause_code_name[c]);

  length += strlen ("expected ");
  buffer = alloca (length);
  length = 0;

  for (c = c1; c <= c2; ++c)
    {
      const char *prefix = length ? " or " : "expected ";

      strcpy (buffer + length, prefix);
      length += strlen (prefix);
      strcpy (buffer + length, omp_clause_code_name[c]);
      length += strlen (omp_clause_code_name[c]);
    }

  internal_error ("tree check: %s, have %s in %s, at %s:%d",
		  buffer, omp_clause_code_name[TREE_CODE (node)],
		  function, trim_filename (file), line);
}


#undef DEFTREESTRUCT
#define DEFTREESTRUCT(VAL, NAME) NAME,

static const char *ts_enum_names[] = {
#include "treestruct.def"
};
#undef DEFTREESTRUCT

#define TS_ENUM_NAME(EN) (ts_enum_names[(EN)])

/* Similar to tree_class_check_failed, except that we check for
   whether CODE contains the tree structure identified by EN.  */

void
tree_contains_struct_check_failed (const tree node, 
				   const enum tree_node_structure_enum en,
				   const char *file, int line, 
				   const char *function)
{
  internal_error
    ("tree check: expected tree that contains %qs structure, have %qs  in %s, at %s:%d",
     TS_ENUM_NAME(en),
     tree_code_name[TREE_CODE (node)], function, trim_filename (file), line);
}


/* Similar to above, except that the check is for the bounds of a TREE_VEC's
   (dynamically sized) vector.  */

void
tree_vec_elt_check_failed (int idx, int len, const char *file, int line,
			   const char *function)
{
  internal_error
    ("tree check: accessed elt %d of tree_vec with %d elts in %s, at %s:%d",
     idx + 1, len, function, trim_filename (file), line);
}

/* Similar to above, except that the check is for the bounds of a PHI_NODE's
   (dynamically sized) vector.  */

void
phi_node_elt_check_failed (int idx, int len, const char *file, int line,
			    const char *function)
{
  internal_error
    ("tree check: accessed elt %d of phi_node with %d elts in %s, at %s:%d",
     idx + 1, len, function, trim_filename (file), line);
}

/* Similar to above, except that the check is for the bounds of the operand
   vector of an expression node.  */

void
tree_operand_check_failed (int idx, enum tree_code code, const char *file,
			   int line, const char *function)
{
  internal_error
    ("tree check: accessed operand %d of %s with %d operands in %s, at %s:%d",
     idx + 1, tree_code_name[code], TREE_CODE_LENGTH (code),
     function, trim_filename (file), line);
}

/* Similar to above, except that the check is for the number of
   operands of an OMP_CLAUSE node.  */

void
omp_clause_operand_check_failed (int idx, tree t, const char *file,
			         int line, const char *function)
{
  internal_error
    ("tree check: accessed operand %d of omp_clause %s with %d operands "
     "in %s, at %s:%d", idx + 1, omp_clause_code_name[OMP_CLAUSE_CODE (t)],
     omp_clause_num_ops [OMP_CLAUSE_CODE (t)], function,
     trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

/* Create a new vector type node holding SUBPARTS units of type INNERTYPE,
   and mapped to the machine mode MODE.  Initialize its fields and build
   the information necessary for debugging output.  */

static tree
make_vector_type (tree innertype, int nunits, enum machine_mode mode)
{
  tree t;
  hashval_t hashcode = 0;

  /* Build a main variant, based on the main variant of the inner type, then
     use it to build the variant we return.  */
  if ((TYPE_ATTRIBUTES (innertype) || TYPE_QUALS (innertype))
      && TYPE_MAIN_VARIANT (innertype) != innertype)
    return build_type_attribute_qual_variant (
	    make_vector_type (TYPE_MAIN_VARIANT (innertype), nunits, mode),
	    TYPE_ATTRIBUTES (innertype),
	    TYPE_QUALS (innertype));

  t = make_node (VECTOR_TYPE);
  TREE_TYPE (t) = TYPE_MAIN_VARIANT (innertype);
  SET_TYPE_VECTOR_SUBPARTS (t, nunits);
  TYPE_MODE (t) = mode;
  TYPE_READONLY (t) = TYPE_READONLY (innertype);
  TYPE_VOLATILE (t) = TYPE_VOLATILE (innertype);

  layout_type (t);

  {
    tree index = build_int_cst (NULL_TREE, nunits - 1);
    tree array = build_array_type (innertype, build_index_type (index));
    tree rt = make_node (RECORD_TYPE);

    TYPE_FIELDS (rt) = build_decl (FIELD_DECL, get_identifier ("f"), array);
    DECL_CONTEXT (TYPE_FIELDS (rt)) = rt;
    layout_type (rt);
    TYPE_DEBUG_REPRESENTATION_TYPE (t) = rt;
    /* In dwarfout.c, type lookup uses TYPE_UID numbers.  We want to output
       the representation type, and we want to find that die when looking up
       the vector type.  This is most easily achieved by making the TYPE_UID
       numbers equal.  */
    TYPE_UID (rt) = TYPE_UID (t);
  }

  hashcode = iterative_hash_host_wide_int (VECTOR_TYPE, hashcode);
  hashcode = iterative_hash_host_wide_int (mode, hashcode);
  hashcode = iterative_hash_object (TYPE_HASH (innertype), hashcode);
  return type_hash_canon (hashcode, t);
}

static tree
make_or_reuse_type (unsigned size, int unsignedp)
{
  if (size == INT_TYPE_SIZE)
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (size == CHAR_TYPE_SIZE)
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (size == SHORT_TYPE_SIZE)
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (size == LONG_TYPE_SIZE)
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (size == LONG_LONG_TYPE_SIZE)
    return (unsignedp ? long_long_unsigned_type_node
            : long_long_integer_type_node);

  if (unsignedp)
    return make_unsigned_type (size);
  else
    return make_signed_type (size);
}

/* Create nodes for all integer types (and error_mark_node) using the sizes
   of C datatypes.  The caller should call set_sizetype soon after calling
   this function to select one of the types as sizetype.  */

void
build_common_tree_nodes (bool signed_char, bool signed_sizetype)
{
  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  initialize_sizetypes (signed_sizetype);

  /* Define both `signed char' and `unsigned char'.  */
  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
  TYPE_STRING_FLAG (signed_char_type_node) = 1;
  unsigned_char_type_node = make_unsigned_type (CHAR_TYPE_SIZE);
  TYPE_STRING_FLAG (unsigned_char_type_node) = 1;

  /* Define `char', which is like either `signed char' or `unsigned char'
     but not the same as either.  */
  char_type_node
    = (signed_char
       ? make_signed_type (CHAR_TYPE_SIZE)
       : make_unsigned_type (CHAR_TYPE_SIZE));
  TYPE_STRING_FLAG (char_type_node) = 1;

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
  integer_type_node = make_signed_type (INT_TYPE_SIZE);
  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);
  long_unsigned_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);

  /* Define a boolean type.  This type only represents boolean values but
     may be larger than char depending on the value of BOOL_TYPE_SIZE.
     Front ends which want to override this size (i.e. Java) can redefine
     boolean_type_node before calling build_common_tree_nodes_2.  */
  boolean_type_node = make_unsigned_type (BOOL_TYPE_SIZE);
  TREE_SET_CODE (boolean_type_node, BOOLEAN_TYPE);
  TYPE_MAX_VALUE (boolean_type_node) = build_int_cst (boolean_type_node, 1);
  TYPE_PRECISION (boolean_type_node) = 1;

  /* Fill in the rest of the sized types.  Reuse existing type nodes
     when possible.  */
  intQI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (QImode), 0);
  intHI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (HImode), 0);
  intSI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (SImode), 0);
  intDI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (DImode), 0);
  intTI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (TImode), 0);

  unsigned_intQI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (QImode), 1);
  unsigned_intHI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (HImode), 1);
  unsigned_intSI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (SImode), 1);
  unsigned_intDI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (DImode), 1);
  unsigned_intTI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (TImode), 1);

  access_public_node = get_identifier ("public");
  access_protected_node = get_identifier ("protected");
  access_private_node = get_identifier ("private");
}

/* Call this function after calling build_common_tree_nodes and set_sizetype.
   It will create several other common tree nodes.  */

void
build_common_tree_nodes_2 (int short_double)
{
  /* Define these next since types below may used them.  */
  integer_zero_node = build_int_cst (NULL_TREE, 0);
  integer_one_node = build_int_cst (NULL_TREE, 1);
  integer_minus_one_node = build_int_cst (NULL_TREE, -1);

  size_zero_node = size_int (0);
  size_one_node = size_int (1);
  bitsize_zero_node = bitsize_int (0);
  bitsize_one_node = bitsize_int (1);
  bitsize_unit_node = bitsize_int (BITS_PER_UNIT);

  boolean_false_node = TYPE_MIN_VALUE (boolean_type_node);
  boolean_true_node = TYPE_MAX_VALUE (boolean_type_node);

  void_type_node = make_node (VOID_TYPE);
  layout_type (void_type_node);

  /* We are not going to have real types in C with less than byte alignment,
     so we might as well not have any types that claim to have it.  */
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;
  TYPE_USER_ALIGN (void_type_node) = 0;

  null_pointer_node = build_int_cst (build_pointer_type (void_type_node), 0);
  layout_type (TREE_TYPE (null_pointer_node));

  ptr_type_node = build_pointer_type (void_type_node);
  const_ptr_type_node
    = build_pointer_type (build_type_variant (void_type_node, 1, 0));
  fileptr_type_node = ptr_type_node;

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  layout_type (float_type_node);

  double_type_node = make_node (REAL_TYPE);
  if (short_double)
    TYPE_PRECISION (double_type_node) = FLOAT_TYPE_SIZE;
  else
    TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  layout_type (double_type_node);

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = LONG_DOUBLE_TYPE_SIZE;
  layout_type (long_double_type_node);

  float_ptr_type_node = build_pointer_type (float_type_node);
  double_ptr_type_node = build_pointer_type (double_type_node);
  long_double_ptr_type_node = build_pointer_type (long_double_type_node);
  integer_ptr_type_node = build_pointer_type (integer_type_node);

  /* Decimal float types. */
  dfloat32_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (dfloat32_type_node) = DECIMAL32_TYPE_SIZE; 
  layout_type (dfloat32_type_node);
  TYPE_MODE (dfloat32_type_node) = SDmode;
  dfloat32_ptr_type_node = build_pointer_type (dfloat32_type_node);

  dfloat64_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (dfloat64_type_node) = DECIMAL64_TYPE_SIZE;
  layout_type (dfloat64_type_node);
  TYPE_MODE (dfloat64_type_node) = DDmode;
  dfloat64_ptr_type_node = build_pointer_type (dfloat64_type_node);

  dfloat128_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (dfloat128_type_node) = DECIMAL128_TYPE_SIZE; 
  layout_type (dfloat128_type_node);
  TYPE_MODE (dfloat128_type_node) = TDmode;
  dfloat128_ptr_type_node = build_pointer_type (dfloat128_type_node);

  complex_integer_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_integer_type_node) = integer_type_node;
  layout_type (complex_integer_type_node);

  complex_float_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_float_type_node) = float_type_node;
  layout_type (complex_float_type_node);

  complex_double_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_double_type_node) = double_type_node;
  layout_type (complex_double_type_node);

  complex_long_double_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_long_double_type_node) = long_double_type_node;
  layout_type (complex_long_double_type_node);

  {
    tree t = targetm.build_builtin_va_list ();

    /* Many back-ends define record types without setting TYPE_NAME.
       If we copied the record type here, we'd keep the original
       record type without a name.  This breaks name mangling.  So,
       don't copy record types and let c_common_nodes_and_builtins()
       declare the type to be __builtin_va_list.  */
    if (TREE_CODE (t) != RECORD_TYPE)
      t = build_variant_type_copy (t);

    va_list_type_node = t;
  }
}

/* A subroutine of build_common_builtin_nodes.  Define a builtin function.  */

static void
local_define_builtin (const char *name, tree type, enum built_in_function code,
                      const char *library_name, int ecf_flags)
{
  tree decl;

  decl = lang_hooks.builtin_function (name, type, code, BUILT_IN_NORMAL,
				      library_name, NULL_TREE);
  if (ecf_flags & ECF_CONST)
    TREE_READONLY (decl) = 1;
  if (ecf_flags & ECF_PURE)
    DECL_IS_PURE (decl) = 1;
  if (ecf_flags & ECF_NORETURN)
    TREE_THIS_VOLATILE (decl) = 1;
  if (ecf_flags & ECF_NOTHROW)
    TREE_NOTHROW (decl) = 1;
  if (ecf_flags & ECF_MALLOC)
    DECL_IS_MALLOC (decl) = 1;

  built_in_decls[code] = decl;
  implicit_built_in_decls[code] = decl;
}

/* Call this function after instantiating all builtins that the language
   front end cares about.  This will build the rest of the builtins that
   are relied upon by the tree optimizers and the middle-end.  */

void
build_common_builtin_nodes (void)
{
  tree tmp, ftype;

  if (built_in_decls[BUILT_IN_MEMCPY] == NULL
      || built_in_decls[BUILT_IN_MEMMOVE] == NULL)
    {
      tmp = tree_cons (NULL_TREE, size_type_node, void_list_node);
      tmp = tree_cons (NULL_TREE, const_ptr_type_node, tmp);
      tmp = tree_cons (NULL_TREE, ptr_type_node, tmp);
      ftype = build_function_type (ptr_type_node, tmp);

      if (built_in_decls[BUILT_IN_MEMCPY] == NULL)
	local_define_builtin ("__builtin_memcpy", ftype, BUILT_IN_MEMCPY,
			      "memcpy", ECF_NOTHROW);
      if (built_in_decls[BUILT_IN_MEMMOVE] == NULL)
	local_define_builtin ("__builtin_memmove", ftype, BUILT_IN_MEMMOVE,
			      "memmove", ECF_NOTHROW);
    }

  if (built_in_decls[BUILT_IN_MEMCMP] == NULL)
    {
      tmp = tree_cons (NULL_TREE, size_type_node, void_list_node);
      tmp = tree_cons (NULL_TREE, const_ptr_type_node, tmp);
      tmp = tree_cons (NULL_TREE, const_ptr_type_node, tmp);
      ftype = build_function_type (integer_type_node, tmp);
      local_define_builtin ("__builtin_memcmp", ftype, BUILT_IN_MEMCMP,
			    "memcmp", ECF_PURE | ECF_NOTHROW);
    }

  if (built_in_decls[BUILT_IN_MEMSET] == NULL)
    {
      tmp = tree_cons (NULL_TREE, size_type_node, void_list_node);
      tmp = tree_cons (NULL_TREE, integer_type_node, tmp);
      tmp = tree_cons (NULL_TREE, ptr_type_node, tmp);
      ftype = build_function_type (ptr_type_node, tmp);
      local_define_builtin ("__builtin_memset", ftype, BUILT_IN_MEMSET,
			    "memset", ECF_NOTHROW);
    }

  if (built_in_decls[BUILT_IN_ALLOCA] == NULL)
    {
      tmp = tree_cons (NULL_TREE, size_type_node, void_list_node);
      ftype = build_function_type (ptr_type_node, tmp);
      local_define_builtin ("__builtin_alloca", ftype, BUILT_IN_ALLOCA,
			    "alloca", ECF_NOTHROW | ECF_MALLOC);
    }

  tmp = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, ptr_type_node, tmp);
  tmp = tree_cons (NULL_TREE, ptr_type_node, tmp);
  ftype = build_function_type (void_type_node, tmp);
  local_define_builtin ("__builtin_init_trampoline", ftype,
			BUILT_IN_INIT_TRAMPOLINE,
			"__builtin_init_trampoline", ECF_NOTHROW);

  tmp = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
  ftype = build_function_type (ptr_type_node, tmp);
  local_define_builtin ("__builtin_adjust_trampoline", ftype,
			BUILT_IN_ADJUST_TRAMPOLINE,
			"__builtin_adjust_trampoline",
			ECF_CONST | ECF_NOTHROW);

  tmp = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, ptr_type_node, tmp);
  ftype = build_function_type (void_type_node, tmp);
  local_define_builtin ("__builtin_nonlocal_goto", ftype,
			BUILT_IN_NONLOCAL_GOTO,
			"__builtin_nonlocal_goto",
			ECF_NORETURN | ECF_NOTHROW);

  tmp = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
  tmp = tree_cons (NULL_TREE, ptr_type_node, tmp);
  ftype = build_function_type (void_type_node, tmp);
  local_define_builtin ("__builtin_setjmp_setup", ftype,
			BUILT_IN_SETJMP_SETUP,
			"__builtin_setjmp_setup", ECF_NOTHROW);

  tmp = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
  ftype = build_function_type (ptr_type_node, tmp);
  local_define_builtin ("__builtin_setjmp_dispatcher", ftype,
			BUILT_IN_SETJMP_DISPATCHER,
			"__builtin_setjmp_dispatcher",
			ECF_PURE | ECF_NOTHROW);

  tmp = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
  ftype = build_function_type (void_type_node, tmp);
  local_define_builtin ("__builtin_setjmp_receiver", ftype,
			BUILT_IN_SETJMP_RECEIVER,
			"__builtin_setjmp_receiver", ECF_NOTHROW);

  ftype = build_function_type (ptr_type_node, void_list_node);
  local_define_builtin ("__builtin_stack_save", ftype, BUILT_IN_STACK_SAVE,
			"__builtin_stack_save", ECF_NOTHROW);

  tmp = tree_cons (NULL_TREE, ptr_type_node, void_list_node);
  ftype = build_function_type (void_type_node, tmp);
  local_define_builtin ("__builtin_stack_restore", ftype,
			BUILT_IN_STACK_RESTORE,
			"__builtin_stack_restore", ECF_NOTHROW);

  ftype = build_function_type (void_type_node, void_list_node);
  local_define_builtin ("__builtin_profile_func_enter", ftype,
			BUILT_IN_PROFILE_FUNC_ENTER, "profile_func_enter", 0);
  local_define_builtin ("__builtin_profile_func_exit", ftype,
			BUILT_IN_PROFILE_FUNC_EXIT, "profile_func_exit", 0);

  /* Complex multiplication and division.  These are handled as builtins
     rather than optabs because emit_library_call_value doesn't support
     complex.  Further, we can do slightly better with folding these 
     beasties if the real and complex parts of the arguments are separate.  */
  {
    enum machine_mode mode;

    for (mode = MIN_MODE_COMPLEX_FLOAT; mode <= MAX_MODE_COMPLEX_FLOAT; ++mode)
      {
	char mode_name_buf[4], *q;
	const char *p;
	enum built_in_function mcode, dcode;
	tree type, inner_type;

	type = lang_hooks.types.type_for_mode (mode, 0);
	if (type == NULL)
	  continue;
	inner_type = TREE_TYPE (type);

	tmp = tree_cons (NULL_TREE, inner_type, void_list_node);
	tmp = tree_cons (NULL_TREE, inner_type, tmp);
	tmp = tree_cons (NULL_TREE, inner_type, tmp);
	tmp = tree_cons (NULL_TREE, inner_type, tmp);
	ftype = build_function_type (type, tmp);

        mcode = BUILT_IN_COMPLEX_MUL_MIN + mode - MIN_MODE_COMPLEX_FLOAT;
        dcode = BUILT_IN_COMPLEX_DIV_MIN + mode - MIN_MODE_COMPLEX_FLOAT;

        for (p = GET_MODE_NAME (mode), q = mode_name_buf; *p; p++, q++)
	  *q = TOLOWER (*p);
	*q = '\0';

	built_in_names[mcode] = concat ("__mul", mode_name_buf, "3", NULL);
        local_define_builtin (built_in_names[mcode], ftype, mcode,
			      built_in_names[mcode], ECF_CONST | ECF_NOTHROW);

	built_in_names[dcode] = concat ("__div", mode_name_buf, "3", NULL);
        local_define_builtin (built_in_names[dcode], ftype, dcode,
			      built_in_names[dcode], ECF_CONST | ECF_NOTHROW);
      }
  }
}

/* HACK.  GROSS.  This is absolutely disgusting.  I wish there was a
   better way.

   If we requested a pointer to a vector, build up the pointers that
   we stripped off while looking for the inner type.  Similarly for
   return values from functions.

   The argument TYPE is the top of the chain, and BOTTOM is the
   new type which we will point to.  */

tree
reconstruct_complex_type (tree type, tree bottom)
{
  tree inner, outer;

  if (POINTER_TYPE_P (type))
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_pointer_type (inner);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_array_type (inner, TYPE_DOMAIN (type));
    }
  else if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_function_type (inner, TYPE_ARG_TYPES (type));
    }
  else if (TREE_CODE (type) == METHOD_TYPE)
    {
      tree argtypes;
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      /* The build_method_type_directly() routine prepends 'this' to argument list,
         so we must compensate by getting rid of it.  */
      argtypes = TYPE_ARG_TYPES (type);
      outer = build_method_type_directly (TYPE_METHOD_BASETYPE (type),
					  inner,
					  TYPE_ARG_TYPES (type));
      TYPE_ARG_TYPES (outer) = argtypes;
    }
  else
    return bottom;

  TYPE_READONLY (outer) = TYPE_READONLY (type);
  TYPE_VOLATILE (outer) = TYPE_VOLATILE (type);

  return outer;
}

/* Returns a vector tree node given a mode (integer, vector, or BLKmode) and
   the inner type.  */
tree
build_vector_type_for_mode (tree innertype, enum machine_mode mode)
{
  int nunits;

  switch (GET_MODE_CLASS (mode))
    {
    case MODE_VECTOR_INT:
    case MODE_VECTOR_FLOAT:
      nunits = GET_MODE_NUNITS (mode);
      break;

    case MODE_INT:
      /* Check that there are no leftover bits.  */
      gcc_assert (GET_MODE_BITSIZE (mode)
		  % TREE_INT_CST_LOW (TYPE_SIZE (innertype)) == 0);

      nunits = GET_MODE_BITSIZE (mode)
	       / TREE_INT_CST_LOW (TYPE_SIZE (innertype));
      break;

    default:
      gcc_unreachable ();
    }

  return make_vector_type (innertype, nunits, mode);
}

/* Similarly, but takes the inner type and number of units, which must be
   a power of two.  */

tree
build_vector_type (tree innertype, int nunits)
{
  return make_vector_type (innertype, nunits, VOIDmode);
}


/* Build RESX_EXPR with given REGION_NUMBER.  */
tree
build_resx (int region_number)
{
  tree t;
  t = build1 (RESX_EXPR, void_type_node,
	      build_int_cst (NULL_TREE, region_number));
  return t;
}

/* Given an initializer INIT, return TRUE if INIT is zero or some
   aggregate of zeros.  Otherwise return FALSE.  */
bool
initializer_zerop (tree init)
{
  tree elt;

  STRIP_NOPS (init);

  switch (TREE_CODE (init))
    {
    case INTEGER_CST:
      return integer_zerop (init);

    case REAL_CST:
      /* ??? Note that this is not correct for C4X float formats.  There,
	 a bit pattern of all zeros is 1.0; 0.0 is encoded with the most
	 negative exponent.  */
      return real_zerop (init)
	&& ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (init));

    case COMPLEX_CST:
      return integer_zerop (init)
	|| (real_zerop (init)
	    && ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_REALPART (init)))
	    && ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_IMAGPART (init))));

    case VECTOR_CST:
      for (elt = TREE_VECTOR_CST_ELTS (init); elt; elt = TREE_CHAIN (elt))
	if (!initializer_zerop (TREE_VALUE (elt)))
	  return false;
      return true;

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;

	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (init), idx, elt)
	  if (!initializer_zerop (elt))
	    return false;
	return true;
      }

    default:
      return false;
    }
}

/* Build an empty statement.  */

tree
build_empty_stmt (void)
{
  return build1 (NOP_EXPR, void_type_node, size_zero_node);
}


/* Build an OpenMP clause with code CODE.  */

tree
build_omp_clause (enum omp_clause_code code)
{
  tree t;
  int size, length;

  length = omp_clause_num_ops[code];
  size = (sizeof (struct tree_omp_clause) + (length - 1) * sizeof (tree));

  t = ggc_alloc (size);
  memset (t, 0, size);
  TREE_SET_CODE (t, OMP_CLAUSE);
  OMP_CLAUSE_SET_CODE (t, code);

#ifdef GATHER_STATISTICS
  tree_node_counts[(int) omp_clause_kind]++;
  tree_node_sizes[(int) omp_clause_kind] += size;
#endif
  
  return t;
}


/* Returns true if it is possible to prove that the index of
   an array access REF (an ARRAY_REF expression) falls into the
   array bounds.  */

bool
in_array_bounds_p (tree ref)
{
  tree idx = TREE_OPERAND (ref, 1);
  tree min, max;

  if (TREE_CODE (idx) != INTEGER_CST)
    return false;

  min = array_ref_low_bound (ref);
  max = array_ref_up_bound (ref);
  if (!min
      || !max
      || TREE_CODE (min) != INTEGER_CST
      || TREE_CODE (max) != INTEGER_CST)
    return false;

  if (tree_int_cst_lt (idx, min)
      || tree_int_cst_lt (max, idx))
    return false;

  return true;
}

/* Returns true if it is possible to prove that the range of
   an array access REF (an ARRAY_RANGE_REF expression) falls
   into the array bounds.  */

bool
range_in_array_bounds_p (tree ref)
{
  tree domain_type = TYPE_DOMAIN (TREE_TYPE (ref));
  tree range_min, range_max, min, max;

  range_min = TYPE_MIN_VALUE (domain_type);
  range_max = TYPE_MAX_VALUE (domain_type);
  if (!range_min
      || !range_max
      || TREE_CODE (range_min) != INTEGER_CST
      || TREE_CODE (range_max) != INTEGER_CST)
    return false;

  min = array_ref_low_bound (ref);
  max = array_ref_up_bound (ref);
  if (!min
      || !max
      || TREE_CODE (min) != INTEGER_CST
      || TREE_CODE (max) != INTEGER_CST)
    return false;

  if (tree_int_cst_lt (range_min, min)
      || tree_int_cst_lt (max, range_max))
    return false;

  return true;
}

/* Return true if T (assumed to be a DECL) is a global variable.  */

bool
is_global_var (tree t)
{
  if (MTAG_P (t))
    return (TREE_STATIC (t) || MTAG_GLOBAL (t));
  else
    return (TREE_STATIC (t) || DECL_EXTERNAL (t));
}

/* Return true if T (assumed to be a DECL) must be assigned a memory
   location.  */

bool
needs_to_live_in_memory (tree t)
{
  return (TREE_ADDRESSABLE (t)
	  || is_global_var (t)
	  || (TREE_CODE (t) == RESULT_DECL
	      && aggregate_value_p (t, current_function_decl)));
}

/* There are situations in which a language considers record types
   compatible which have different field lists.  Decide if two fields
   are compatible.  It is assumed that the parent records are compatible.  */

bool
fields_compatible_p (tree f1, tree f2)
{
  if (!operand_equal_p (DECL_FIELD_BIT_OFFSET (f1),
			DECL_FIELD_BIT_OFFSET (f2), OEP_ONLY_CONST))
    return false;

  if (!operand_equal_p (DECL_FIELD_OFFSET (f1),
                        DECL_FIELD_OFFSET (f2), OEP_ONLY_CONST))
    return false;

  if (!lang_hooks.types_compatible_p (TREE_TYPE (f1), TREE_TYPE (f2)))
    return false;

  return true;
}

/* Locate within RECORD a field that is compatible with ORIG_FIELD.  */

tree
find_compatible_field (tree record, tree orig_field)
{
  tree f;

  for (f = TYPE_FIELDS (record); f ; f = TREE_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL
	&& fields_compatible_p (f, orig_field))
      return f;

  /* ??? Why isn't this on the main fields list?  */
  f = TYPE_VFIELD (record);
  if (f && TREE_CODE (f) == FIELD_DECL
      && fields_compatible_p (f, orig_field))
    return f;

  /* ??? We should abort here, but Java appears to do Bad Things
     with inherited fields.  */
  return orig_field;
}

/* Return value of a constant X.  */

HOST_WIDE_INT
int_cst_value (tree x)
{
  unsigned bits = TYPE_PRECISION (TREE_TYPE (x));
  unsigned HOST_WIDE_INT val = TREE_INT_CST_LOW (x);
  bool negative = ((val >> (bits - 1)) & 1) != 0;

  gcc_assert (bits <= HOST_BITS_PER_WIDE_INT);

  if (negative)
    val |= (~(unsigned HOST_WIDE_INT) 0) << (bits - 1) << 1;
  else
    val &= ~((~(unsigned HOST_WIDE_INT) 0) << (bits - 1) << 1);

  return val;
}

/* Returns the greatest common divisor of A and B, which must be
   INTEGER_CSTs.  */

tree
tree_fold_gcd (tree a, tree b)
{
  tree a_mod_b;
  tree type = TREE_TYPE (a);

  gcc_assert (TREE_CODE (a) == INTEGER_CST);
  gcc_assert (TREE_CODE (b) == INTEGER_CST);

  if (integer_zerop (a))
    return b;

  if (integer_zerop (b))
    return a;

  if (tree_int_cst_sgn (a) == -1)
    a = fold_build2 (MULT_EXPR, type, a,
		     build_int_cst (type, -1));

  if (tree_int_cst_sgn (b) == -1)
    b = fold_build2 (MULT_EXPR, type, b,
		     build_int_cst (type, -1));

  while (1)
    {
      a_mod_b = fold_build2 (FLOOR_MOD_EXPR, type, a, b);

      if (!TREE_INT_CST_LOW (a_mod_b)
	  && !TREE_INT_CST_HIGH (a_mod_b))
	return b;

      a = b;
      b = a_mod_b;
    }
}

/* Returns unsigned variant of TYPE.  */

tree
unsigned_type_for (tree type)
{
  if (POINTER_TYPE_P (type))
    return lang_hooks.types.unsigned_type (size_type_node);
  return lang_hooks.types.unsigned_type (type);
}

/* Returns signed variant of TYPE.  */

tree
signed_type_for (tree type)
{
  if (POINTER_TYPE_P (type))
    return lang_hooks.types.signed_type (size_type_node);
  return lang_hooks.types.signed_type (type);
}

/* Returns the largest value obtainable by casting something in INNER type to
   OUTER type.  */

tree
upper_bound_in_type (tree outer, tree inner)
{
  unsigned HOST_WIDE_INT lo, hi;
  unsigned int det = 0;
  unsigned oprec = TYPE_PRECISION (outer);
  unsigned iprec = TYPE_PRECISION (inner);
  unsigned prec;

  /* Compute a unique number for every combination.  */
  det |= (oprec > iprec) ? 4 : 0;
  det |= TYPE_UNSIGNED (outer) ? 2 : 0;
  det |= TYPE_UNSIGNED (inner) ? 1 : 0;

  /* Determine the exponent to use.  */
  switch (det)
    {
    case 0:
    case 1:
      /* oprec <= iprec, outer: signed, inner: don't care.  */
      prec = oprec - 1;
      break;
    case 2:
    case 3:
      /* oprec <= iprec, outer: unsigned, inner: don't care.  */
      prec = oprec;
      break;
    case 4:
      /* oprec > iprec, outer: signed, inner: signed.  */
      prec = iprec - 1;
      break;
    case 5:
      /* oprec > iprec, outer: signed, inner: unsigned.  */
      prec = iprec;
      break;
    case 6:
      /* oprec > iprec, outer: unsigned, inner: signed.  */
      prec = oprec;
      break;
    case 7:
      /* oprec > iprec, outer: unsigned, inner: unsigned.  */
      prec = iprec;
      break;
    default:
      gcc_unreachable ();
    }

  /* Compute 2^^prec - 1.  */
  if (prec <= HOST_BITS_PER_WIDE_INT)
    {
      hi = 0;
      lo = ((~(unsigned HOST_WIDE_INT) 0)
	    >> (HOST_BITS_PER_WIDE_INT - prec));
    }
  else
    {
      hi = ((~(unsigned HOST_WIDE_INT) 0)
	    >> (2 * HOST_BITS_PER_WIDE_INT - prec));
      lo = ~(unsigned HOST_WIDE_INT) 0;
    }

  return build_int_cst_wide (outer, lo, hi);
}

/* Returns the smallest value obtainable by casting something in INNER type to
   OUTER type.  */

tree
lower_bound_in_type (tree outer, tree inner)
{
  unsigned HOST_WIDE_INT lo, hi;
  unsigned oprec = TYPE_PRECISION (outer);
  unsigned iprec = TYPE_PRECISION (inner);

  /* If OUTER type is unsigned, we can definitely cast 0 to OUTER type
     and obtain 0.  */
  if (TYPE_UNSIGNED (outer)
      /* If we are widening something of an unsigned type, OUTER type
	 contains all values of INNER type.  In particular, both INNER
	 and OUTER types have zero in common.  */
      || (oprec > iprec && TYPE_UNSIGNED (inner)))
    lo = hi = 0;
  else
    {
      /* If we are widening a signed type to another signed type, we
	 want to obtain -2^^(iprec-1).  If we are keeping the
	 precision or narrowing to a signed type, we want to obtain
	 -2^(oprec-1).  */
      unsigned prec = oprec > iprec ? iprec : oprec;

      if (prec <= HOST_BITS_PER_WIDE_INT)
	{
	  hi = ~(unsigned HOST_WIDE_INT) 0;
	  lo = (~(unsigned HOST_WIDE_INT) 0) << (prec - 1);
	}
      else
	{
	  hi = ((~(unsigned HOST_WIDE_INT) 0)
		<< (prec - HOST_BITS_PER_WIDE_INT - 1));
	  lo = 0;
	}
    }

  return build_int_cst_wide (outer, lo, hi);
}

/* Return nonzero if two operands that are suitable for PHI nodes are
   necessarily equal.  Specifically, both ARG0 and ARG1 must be either
   SSA_NAME or invariant.  Note that this is strictly an optimization.
   That is, callers of this function can directly call operand_equal_p
   and get the same result, only slower.  */

int
operand_equal_for_phi_arg_p (tree arg0, tree arg1)
{
  if (arg0 == arg1)
    return 1;
  if (TREE_CODE (arg0) == SSA_NAME || TREE_CODE (arg1) == SSA_NAME)
    return 0;
  return operand_equal_p (arg0, arg1, 0);
}

/* Returns number of zeros at the end of binary representation of X.
   
   ??? Use ffs if available?  */

tree
num_ending_zeros (tree x)
{
  unsigned HOST_WIDE_INT fr, nfr;
  unsigned num, abits;
  tree type = TREE_TYPE (x);

  if (TREE_INT_CST_LOW (x) == 0)
    {
      num = HOST_BITS_PER_WIDE_INT;
      fr = TREE_INT_CST_HIGH (x);
    }
  else
    {
      num = 0;
      fr = TREE_INT_CST_LOW (x);
    }

  for (abits = HOST_BITS_PER_WIDE_INT / 2; abits; abits /= 2)
    {
      nfr = fr >> abits;
      if (nfr << abits == fr)
	{
	  num += abits;
	  fr = nfr;
	}
    }

  if (num > TYPE_PRECISION (type))
    num = TYPE_PRECISION (type);

  return build_int_cst_type (type, num);
}


#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_tree (&(NODE), func, data, pset);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

/* This is a subroutine of walk_tree that walks field of TYPE that are to
   be walked whenever a type is seen in the tree.  Rest of operands and return
   value are as for walk_tree.  */

static tree
walk_type_fields (tree type, walk_tree_fn func, void *data,
		  struct pointer_set_t *pset)
{
  tree result = NULL_TREE;

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* We have to worry about mutually recursive pointers.  These can't
	 be written in C.  They can in Ada.  It's pathological, but
	 there's an ACATS test (c38102a) that checks it.  Deal with this
	 by checking if we're pointing to another pointer, that one
	 points to another pointer, that one does too, and we have no htab.
	 If so, get a hash table.  We check three levels deep to avoid
	 the cost of the hash table if we don't need one.  */
      if (POINTER_TYPE_P (TREE_TYPE (type))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (type)))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (TREE_TYPE (type))))
	  && !pset)
	{
	  result = walk_tree_without_duplicates (&TREE_TYPE (type),
						 func, data);
	  if (result)
	    return result;

	  break;
	}

      /* ... fall through ... */

    case COMPLEX_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      break;

    case METHOD_TYPE:
      WALK_SUBTREE (TYPE_METHOD_BASETYPE (type));

      /* Fall through.  */

    case FUNCTION_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      {
	tree arg;

	/* We never want to walk into default arguments.  */
	for (arg = TYPE_ARG_TYPES (type); arg; arg = TREE_CHAIN (arg))
	  WALK_SUBTREE (TREE_VALUE (arg));
      }
      break;

    case ARRAY_TYPE:
      /* Don't follow this nodes's type if a pointer for fear that
	 we'll have infinite recursion.  If we have a PSET, then we
	 need not fear.  */
      if (pset
	  || (!POINTER_TYPE_P (TREE_TYPE (type))
	      && TREE_CODE (TREE_TYPE (type)) != OFFSET_TYPE))
	WALK_SUBTREE (TREE_TYPE (type));
      WALK_SUBTREE (TYPE_DOMAIN (type));
      break;

    case BOOLEAN_TYPE:
    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
      WALK_SUBTREE (TYPE_MIN_VALUE (type));
      WALK_SUBTREE (TYPE_MAX_VALUE (type));
      break;

    case OFFSET_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      WALK_SUBTREE (TYPE_OFFSET_BASETYPE (type));
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Apply FUNC to all the sub-trees of TP in a pre-order traversal.  FUNC is
   called with the DATA and the address of each sub-tree.  If FUNC returns a
   non-NULL value, the traversal is stopped, and the value returned by FUNC
   is returned.  If PSET is non-NULL it is used to record the nodes visited,
   and to avoid visiting a node more than once.  */

tree
walk_tree (tree *tp, walk_tree_fn func, void *data, struct pointer_set_t *pset)
{
  enum tree_code code;
  int walk_subtrees;
  tree result;

#define WALK_SUBTREE_TAIL(NODE)				\
  do							\
    {							\
       tp = & (NODE);					\
       goto tail_recurse;				\
    }							\
  while (0)

 tail_recurse:
  /* Skip empty subtrees.  */
  if (!*tp)
    return NULL_TREE;

  /* Don't walk the same tree twice, if the user has requested
     that we avoid doing so.  */
  if (pset && pointer_set_insert (pset, *tp))
    return NULL_TREE;

  /* Call the function.  */
  walk_subtrees = 1;
  result = (*func) (tp, &walk_subtrees, data);

  /* If we found something, return it.  */
  if (result)
    return result;

  code = TREE_CODE (*tp);

  /* Even if we didn't, FUNC may have decided that there was nothing
     interesting below this point in the tree.  */
  if (!walk_subtrees)
    {
      /* But we still need to check our siblings.  */
      if (code == TREE_LIST)
	WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
      else if (code == OMP_CLAUSE)
	WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));
      else
	return NULL_TREE;
    }

  result = lang_hooks.tree_inlining.walk_subtrees (tp, &walk_subtrees, func,
						   data, pset);
  if (result || ! walk_subtrees)
    return result;

  switch (code)
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case VECTOR_CST:
    case STRING_CST:
    case BLOCK:
    case PLACEHOLDER_EXPR:
    case SSA_NAME:
    case FIELD_DECL:
    case RESULT_DECL:
      /* None of these have subtrees other than those already walked
	 above.  */
      break;

    case TREE_LIST:
      WALK_SUBTREE (TREE_VALUE (*tp));
      WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
      break;

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (*tp);

	if (len == 0)
	  break;

	/* Walk all elements but the first.  */
	while (--len)
	  WALK_SUBTREE (TREE_VEC_ELT (*tp, len));

	/* Now walk the first one as a tail call.  */
	WALK_SUBTREE_TAIL (TREE_VEC_ELT (*tp, 0));
      }

    case COMPLEX_CST:
      WALK_SUBTREE (TREE_REALPART (*tp));
      WALK_SUBTREE_TAIL (TREE_IMAGPART (*tp));

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	constructor_elt *ce;

	for (idx = 0;
	     VEC_iterate(constructor_elt, CONSTRUCTOR_ELTS (*tp), idx, ce);
	     idx++)
	  WALK_SUBTREE (ce->value);
      }
      break;

    case SAVE_EXPR:
      WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, 0));

    case BIND_EXPR:
      {
	tree decl;
	for (decl = BIND_EXPR_VARS (*tp); decl; decl = TREE_CHAIN (decl))
	  {
	    /* Walk the DECL_INITIAL and DECL_SIZE.  We don't want to walk
	       into declarations that are just mentioned, rather than
	       declared; they don't really belong to this part of the tree.
	       And, we can see cycles: the initializer for a declaration
	       can refer to the declaration itself.  */
	    WALK_SUBTREE (DECL_INITIAL (decl));
	    WALK_SUBTREE (DECL_SIZE (decl));
	    WALK_SUBTREE (DECL_SIZE_UNIT (decl));
	  }
	WALK_SUBTREE_TAIL (BIND_EXPR_BODY (*tp));
      }

    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (*tp); !tsi_end_p (i); tsi_next (&i))
	  WALK_SUBTREE (*tsi_stmt_ptr (i));
      }
      break;

    case OMP_CLAUSE:
      switch (OMP_CLAUSE_CODE (*tp))
	{
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LASTPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_SCHEDULE:
	  WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, 0));
	  /* FALLTHRU */

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	  WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));

	case OMP_CLAUSE_REDUCTION:
	  {
	    int i;
	    for (i = 0; i < 4; i++)
	      WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, i));
	    WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));
	  }

	default:
	  gcc_unreachable ();
	}
      break;

    case TARGET_EXPR:
      {
	int i, len;

	/* TARGET_EXPRs are peculiar: operands 1 and 3 can be the same.
	   But, we only want to walk once.  */
	len = (TREE_OPERAND (*tp, 3) == TREE_OPERAND (*tp, 1)) ? 2 : 3;
	for (i = 0; i < len; ++i)
	  WALK_SUBTREE (TREE_OPERAND (*tp, i));
	WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, len));
      }

    case DECL_EXPR:
      /* Walk into various fields of the type that it's defining.  We only
	 want to walk into these fields of a type in this case.  Note that
	 decls get walked as part of the processing of a BIND_EXPR.

	 ??? Precisely which fields of types that we are supposed to walk in
	 this case vs. the normal case aren't well defined.  */
      if (TREE_CODE (DECL_EXPR_DECL (*tp)) == TYPE_DECL
	  && TREE_CODE (TREE_TYPE (DECL_EXPR_DECL (*tp))) != ERROR_MARK)
	{
	  tree *type_p = &TREE_TYPE (DECL_EXPR_DECL (*tp));

	  /* Call the function for the type.  See if it returns anything or
	     doesn't want us to continue.  If we are to continue, walk both
	     the normal fields and those for the declaration case.  */
	  result = (*func) (type_p, &walk_subtrees, data);
	  if (result || !walk_subtrees)
	    return NULL_TREE;

	  result = walk_type_fields (*type_p, func, data, pset);
	  if (result)
	    return result;

	  /* If this is a record type, also walk the fields.  */
	  if (TREE_CODE (*type_p) == RECORD_TYPE
	      || TREE_CODE (*type_p) == UNION_TYPE
	      || TREE_CODE (*type_p) == QUAL_UNION_TYPE)
	    {
	      tree field;

	      for (field = TYPE_FIELDS (*type_p); field;
		   field = TREE_CHAIN (field))
		{
		  /* We'd like to look at the type of the field, but we can
		     easily get infinite recursion.  So assume it's pointed
		     to elsewhere in the tree.  Also, ignore things that
		     aren't fields.  */
		  if (TREE_CODE (field) != FIELD_DECL)
		    continue;

		  WALK_SUBTREE (DECL_FIELD_OFFSET (field));
		  WALK_SUBTREE (DECL_SIZE (field));
		  WALK_SUBTREE (DECL_SIZE_UNIT (field));
		  if (TREE_CODE (*type_p) == QUAL_UNION_TYPE)
		    WALK_SUBTREE (DECL_QUALIFIER (field));
		}
	    }

	  WALK_SUBTREE (TYPE_SIZE (*type_p));
	  WALK_SUBTREE_TAIL (TYPE_SIZE_UNIT (*type_p));
	}
      /* FALLTHRU */

    default:
      if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
	{
	  int i, len;

	  /* Walk over all the sub-trees of this operand.  */
	  len = TREE_CODE_LENGTH (code);

	  /* Go through the subtrees.  We need to do this in forward order so
	     that the scope of a FOR_EXPR is handled properly.  */
	  if (len)
	    {
	      for (i = 0; i < len - 1; ++i)
		WALK_SUBTREE (TREE_OPERAND (*tp, i));
	      WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, len - 1));
	    }
	}

      /* If this is a type, walk the needed fields in the type.  */
      else if (TYPE_P (*tp))
	return walk_type_fields (*tp, func, data, pset);
      break;
    }

  /* We didn't find what we were looking for.  */
  return NULL_TREE;

#undef WALK_SUBTREE_TAIL
}
#undef WALK_SUBTREE

/* Like walk_tree, but does not walk duplicate nodes more than once.  */

tree
walk_tree_without_duplicates (tree *tp, walk_tree_fn func, void *data)
{
  tree result;
  struct pointer_set_t *pset;

  pset = pointer_set_create ();
  result = walk_tree (tp, func, data, pset);
  pointer_set_destroy (pset);
  return result;
}


/* Return true if STMT is an empty statement or contains nothing but
   empty statements.  */

bool
empty_body_p (tree stmt)
{
  tree_stmt_iterator i;
  tree body;

  if (IS_EMPTY_STMT (stmt))
    return true;
  else if (TREE_CODE (stmt) == BIND_EXPR)
    body = BIND_EXPR_BODY (stmt);
  else if (TREE_CODE (stmt) == STATEMENT_LIST)
    body = stmt;
  else
    return false;

  for (i = tsi_start (body); !tsi_end_p (i); tsi_next (&i))
    if (!empty_body_p (tsi_stmt (i)))
      return false;

  return true;
}

#include "gt-tree.h"
#ifdef KEY
/**
  The following are no longer present in GNU 4.2.0, and so should be
  obsoleted out of libspin and wgen.
    // DECL_ARG_TYPE_AS_WRITTEN
    // DECL_TRANSPARENT_UNION
    // IX86_BUILTIN_CMPNEPS
    // IX86_BUILTIN_CMPNESS
 **/

#include "cp/cp-tree.h"
#include "diagnostic.h"

#include "gspin-gcc-interface.h"

/* Bug 1392 */
#include <stdlib.h>
#include <ctype.h>
#include "c-common.h" 

enum language { C, CPP };
enum language language = C;
#define CPR() (language == CPP)
#define CR()  (language == C)

/* C++ Dummy Variables Section Begins. */
tree global_namespace; /* CP_DECL_CONTEXT () references this variable in cp/name-lookup.c */
int (*p_uses_template_parms) (tree);
tree (*p_most_general_template) (tree);
int (*p_copy_fn_p) (tree);
int (*p_is_empty_class) (tree);
tree (*p_namespace_binding) (tree, tree);
/* tree p_complete_ctor_identifier; */
tree cp_global_trees[CPTI_MAX];
tree (*p_get_tinfo_decl) (tree);
/* C++ Dummy Variables Section Ends. */

/* GSPIN stuff follows: */

static inline gs_code_t
gcc2gs (int code)
{
 switch (code) {
 
 /* GCC Specific tree codes: return ; */

   case ABS_EXPR: return GS_ABS_EXPR;
   case ADDR_EXPR: return GS_ADDR_EXPR;
   case AGGR_INIT_EXPR: return GS_AGGR_INIT_EXPR;
   case ALIGNOF_EXPR: return GS_ALIGNOF_EXPR;
   case ALIGN_INDIRECT_REF: return GS_ALIGN_INDIRECT_REF;
   case ARRAY_RANGE_REF: return GS_ARRAY_RANGE_REF;
   case ARRAY_REF: return GS_ARRAY_REF;
   case ARRAY_TYPE: return GS_ARRAY_TYPE;
   case ARROW_EXPR: return GS_ARROW_EXPR;
   case ASM_EXPR: return GS_ASM_EXPR;
   case BASELINK: return GS_BASELINK;
   case BIND_EXPR: return GS_BIND_EXPR;
   case BIT_AND_EXPR: return GS_BIT_AND_EXPR;
   case BIT_FIELD_REF: return GS_BIT_FIELD_REF;
   case BIT_IOR_EXPR: return GS_BIT_IOR_EXPR;
   case BIT_NOT_EXPR: return GS_BIT_NOT_EXPR;
   case BIT_XOR_EXPR: return GS_BIT_XOR_EXPR;
   case BLOCK: return GS_BLOCK;
   case BOOLEAN_TYPE: return GS_BOOLEAN_TYPE;
   case BOUND_TEMPLATE_TEMPLATE_PARM: 
     return GS_BOUND_TEMPLATE_TEMPLATE_PARM;
   case BREAK_STMT: return GS_BREAK_STMT;
   case CALL_EXPR: return GS_CALL_EXPR;
   case CAST_EXPR: return GS_CAST_EXPR;
   case CONST_CAST_EXPR: return GS_CONST_CAST_EXPR;
   case CASE_LABEL_EXPR: return GS_CASE_LABEL_EXPR;
   case CATCH_EXPR: return GS_CATCH_EXPR;
   case CEIL_DIV_EXPR: return GS_CEIL_DIV_EXPR;
   case CEIL_MOD_EXPR: return GS_CEIL_MOD_EXPR;
   case CLEANUP_STMT: return GS_CLEANUP_STMT;
   case CLEANUP_POINT_EXPR: return GS_CLEANUP_POINT_EXPR;
   case COMPLEX_CST: return GS_COMPLEX_CST;
   case COMPLEX_EXPR: return GS_COMPLEX_EXPR;
   case COMPLEX_TYPE: return GS_COMPLEX_TYPE;
   case COMPONENT_REF: return GS_COMPONENT_REF;
   case COMPOUND_EXPR: return GS_COMPOUND_EXPR;
   case COMPOUND_LITERAL_EXPR: return GS_COMPOUND_LITERAL_EXPR;
   case COND_EXPR: return GS_COND_EXPR;
   case CONJ_EXPR: return GS_CONJ_EXPR;
   case CONSTRUCTOR: return GS_CONSTRUCTOR;
   case CONST_DECL: return GS_CONST_DECL;
   case CONTINUE_STMT: return GS_CONTINUE_STMT;
   case CONVERT_EXPR: return GS_CONVERT_EXPR;
   case CTOR_INITIALIZER: return GS_CTOR_INITIALIZER;
   case DECL_EXPR: return GS_DECL_EXPR;
   case DELETE_EXPR: return GS_DELETE_EXPR;
   case DEFAULT_ARG: return GS_DEFAULT_ARG;
   case DYNAMIC_CAST_EXPR: return GS_DYNAMIC_CAST_EXPR;
   case DO_STMT: return GS_DO_STMT;
   case DOTSTAR_EXPR: return GS_DOTSTAR_EXPR;
   case EH_FILTER_EXPR: return GS_EH_FILTER_EXPR;
   case EMPTY_CLASS_EXPR: return GS_EMPTY_CLASS_EXPR;
   case ENUMERAL_TYPE: return GS_ENUMERAL_TYPE;
   case EQ_EXPR: return GS_EQ_EXPR;
   case ERROR_MARK: return GS_ERROR_MARK;
   case EXACT_DIV_EXPR: return GS_EXACT_DIV_EXPR;
   case EXC_PTR_EXPR: return GS_EXC_PTR_EXPR;
   case EXIT_EXPR: return GS_EXIT_EXPR;
   case EXPR_STMT: return GS_EXPR_STMT;
   case EH_SPEC_BLOCK: return GS_EH_SPEC_BLOCK;
   case FDESC_EXPR: return GS_FDESC_EXPR;
   case FIELD_DECL: return GS_FIELD_DECL;
   case FILTER_EXPR: return GS_FILTER_EXPR;
   case FIX_CEIL_EXPR: return GS_FIX_CEIL_EXPR;
   case FIX_FLOOR_EXPR: return GS_FIX_FLOOR_EXPR;
   case FIX_ROUND_EXPR: return GS_FIX_ROUND_EXPR;
   case FIX_TRUNC_EXPR: return GS_FIX_TRUNC_EXPR;
   case FLOAT_EXPR: return GS_FLOAT_EXPR;
   case FLOOR_DIV_EXPR: return GS_FLOOR_DIV_EXPR;
   case FLOOR_MOD_EXPR: return GS_FLOOR_MOD_EXPR;
   case FOR_STMT: return GS_FOR_STMT;
   case FUNCTION_DECL: return GS_FUNCTION_DECL;
   case FUNCTION_TYPE: return GS_FUNCTION_TYPE;
   case GE_EXPR: return GS_GE_EXPR;
   case GOTO_EXPR: return GS_GOTO_EXPR;
   case GT_EXPR: return GS_GT_EXPR;
   case HANDLER: return GS_HANDLER;
   case IDENTIFIER_NODE: return GS_IDENTIFIER_NODE;
   case IF_STMT: return GS_IF_STMT;
   case IMAGPART_EXPR: return GS_IMAGPART_EXPR;
   case INDIRECT_REF: return GS_INDIRECT_REF;
   case INIT_EXPR: return GS_INIT_EXPR;
   case INTEGER_CST: return GS_INTEGER_CST;
   case INTEGER_TYPE: return GS_INTEGER_TYPE;
   case LABEL_DECL: return GS_LABEL_DECL;
   case LABEL_EXPR: return GS_LABEL_EXPR;
   case LANG_TYPE: return GS_LANG_TYPE;
   case LE_EXPR: return GS_LE_EXPR;
   case LOOP_EXPR: return GS_LOOP_EXPR;
   case LROTATE_EXPR: return GS_LROTATE_EXPR;
   case LSHIFT_EXPR: return GS_LSHIFT_EXPR;
   case LTGT_EXPR: return GS_LTGT_EXPR;
   case LT_EXPR: return GS_LT_EXPR;
   case MAX_EXPR: return GS_MAX_EXPR;
   case MEMBER_REF: return GS_MEMBER_REF;
   case METHOD_TYPE: return GS_METHOD_TYPE;
   case MINUS_EXPR: return GS_MINUS_EXPR;
   case MIN_EXPR: return GS_MIN_EXPR;
   case MISALIGNED_INDIRECT_REF: return GS_MISALIGNED_INDIRECT_REF;
   case MODOP_EXPR: return GS_MODOP_EXPR;
   case MODIFY_EXPR: return GS_MODIFY_EXPR;
   case MULT_EXPR: return GS_MULT_EXPR;
   case MUST_NOT_THROW_EXPR: return GS_MUST_NOT_THROW_EXPR;
   case NAMESPACE_DECL: return GS_NAMESPACE_DECL;
   case NE_EXPR: return GS_NE_EXPR;
   case NEGATE_EXPR: return GS_NEGATE_EXPR;
   case NEW_EXPR: return GS_NEW_EXPR;
   case NON_LVALUE_EXPR: return GS_NON_LVALUE_EXPR;
   case NOP_EXPR: return GS_NOP_EXPR;
   case NON_DEPENDENT_EXPR: return GS_NON_DEPENDENT_EXPR;
   case OBJ_TYPE_REF: return GS_OBJ_TYPE_REF;
   case OFFSET_REF: return GS_OFFSET_REF;
   case OFFSET_TYPE: return GS_OFFSET_TYPE;
   case OFFSETOF_EXPR: return GS_OFFSETOF_EXPR;
   case OMP_ATOMIC: return GS_OMP_ATOMIC;
   case OMP_CLAUSE: return GS_OMP_CLAUSE;
   case OMP_CRITICAL: return GS_OMP_CRITICAL;
   case OMP_FOR: return GS_OMP_FOR;
   case OMP_MASTER: return GS_OMP_MASTER;
   case OMP_ORDERED: return GS_OMP_ORDERED;
   case OMP_PARALLEL: return GS_OMP_PARALLEL;
   case OMP_SECTION: return GS_OMP_SECTION;
   case OMP_SECTIONS: return GS_OMP_SECTIONS;
   case OMP_SINGLE: return GS_OMP_SINGLE;
   case ORDERED_EXPR: return GS_ORDERED_EXPR;
   case OVERLOAD: return GS_OVERLOAD;
   case PARM_DECL: return GS_PARM_DECL;
   case PHI_NODE: return GS_PHI_NODE;
   case PLACEHOLDER_EXPR: return GS_PLACEHOLDER_EXPR;
   case PLUS_EXPR: return GS_PLUS_EXPR;
   case POINTER_TYPE: return GS_POINTER_TYPE;
   case POLYNOMIAL_CHREC: return GS_POLYNOMIAL_CHREC;
   case POSTDECREMENT_EXPR: return GS_POSTDECREMENT_EXPR;
   case POSTINCREMENT_EXPR: return GS_POSTINCREMENT_EXPR;
   case PREDECREMENT_EXPR: return GS_PREDECREMENT_EXPR;
   case PREINCREMENT_EXPR: return GS_PREINCREMENT_EXPR;
   case PTRMEM_CST: return GS_PTRMEM_CST;
   case PSEUDO_DTOR_EXPR: return GS_PSEUDO_DTOR_EXPR;
   case QUAL_UNION_TYPE: return GS_QUAL_UNION_TYPE;
   case RANGE_EXPR: return GS_RANGE_EXPR;
   case RDIV_EXPR: return GS_RDIV_EXPR;
   case REALIGN_LOAD_EXPR: return GS_REALIGN_LOAD_EXPR;
   case REALPART_EXPR: return GS_REALPART_EXPR;
   case REAL_CST: return GS_REAL_CST;
   case REAL_TYPE: return GS_REAL_TYPE;
   case RECORD_TYPE: return GS_RECORD_TYPE;
   case REINTERPRET_CAST_EXPR: return GS_REINTERPRET_CAST_EXPR;
   case REFERENCE_TYPE: return GS_REFERENCE_TYPE;
   case RESULT_DECL: return GS_RESULT_DECL;
   case RESX_EXPR: return GS_RESX_EXPR;
   case RETURN_EXPR: return GS_RETURN_EXPR;
   case ROUND_DIV_EXPR: return GS_ROUND_DIV_EXPR;
   case ROUND_MOD_EXPR: return GS_ROUND_MOD_EXPR;
   case RROTATE_EXPR: return GS_RROTATE_EXPR;
   case RSHIFT_EXPR: return GS_RSHIFT_EXPR;
   case SAVE_EXPR: return GS_SAVE_EXPR;
   case SCEV_KNOWN: return GS_SCEV_KNOWN;
   case SCEV_NOT_KNOWN: return GS_SCEV_NOT_KNOWN;
   case SCOPE_REF: return GS_SCOPE_REF;
   case SIZEOF_EXPR: return GS_SIZEOF_EXPR;
   case SSA_NAME: return GS_SSA_NAME;
   case STATEMENT_LIST: return GS_STATEMENT_LIST;
   case STATIC_CAST_EXPR: return GS_STATIC_CAST_EXPR;
   case STMT_EXPR: return GS_STMT_EXPR;
   case STRING_CST: return GS_STRING_CST;
   case SWITCH_EXPR: return GS_SWITCH_EXPR;
   case SWITCH_STMT: return GS_SWITCH_STMT;
   case TARGET_EXPR: return GS_TARGET_EXPR;
   case TAG_DEFN: return GS_TAG_DEFN;
   case TEMPLATE_DECL:  return GS_TEMPLATE_DECL;
   case TEMPLATE_ID_EXPR:  return GS_TEMPLATE_ID_EXPR;
   case TEMPLATE_PARM_INDEX:  return GS_TEMPLATE_PARM_INDEX;
   case TEMPLATE_TYPE_PARM:  return GS_TEMPLATE_TYPE_PARM;
   case THROW_EXPR: return GS_THROW_EXPR;
   case TINST_LEVEL: return GS_TINST_LEVEL;
   case TRANSLATION_UNIT_DECL: return GS_TRANSLATION_UNIT_DECL;
   case TREE_BINFO: return GS_TREE_BINFO;
   case TREE_LIST: return GS_TREE_LIST;
   case TREE_VEC: return GS_TREE_VEC;
   case TRUNC_DIV_EXPR: return GS_TRUNC_DIV_EXPR;
   case TRUNC_MOD_EXPR: return GS_TRUNC_MOD_EXPR;
   case TRUTH_ANDIF_EXPR: return GS_TRUTH_ANDIF_EXPR;
   case TRUTH_AND_EXPR: return GS_TRUTH_AND_EXPR;
   case TRUTH_NOT_EXPR: return GS_TRUTH_NOT_EXPR;
   case TRUTH_ORIF_EXPR: return GS_TRUTH_ORIF_EXPR;
   case TRUTH_OR_EXPR: return GS_TRUTH_OR_EXPR;
   case TRUTH_XOR_EXPR: return GS_TRUTH_XOR_EXPR;
   case TRY_BLOCK: return GS_TRY_BLOCK;
   case TRY_CATCH_EXPR: return GS_TRY_CATCH_EXPR;
   case TRY_FINALLY_EXPR: return GS_TRY_FINALLY_EXPR;
   case TYPEOF_TYPE: return GS_TYPEOF_TYPE;
   case TYPENAME_TYPE: return GS_TYPENAME_TYPE;
   case TYPE_DECL: return GS_TYPE_DECL;
   case TYPE_EXPR: return GS_TYPE_EXPR;
   case TYPEID_EXPR: return GS_TYPEID_EXPR;
   case USING_DECL: return GS_USING_DECL;
   case USING_STMT: return GS_USING_STMT;
   case UNBOUND_CLASS_TEMPLATE: return GS_UNBOUND_CLASS_TEMPLATE;
   case UNEQ_EXPR: return GS_UNEQ_EXPR;
   case UNGE_EXPR: return GS_UNGE_EXPR;
   case UNGT_EXPR: return GS_UNGT_EXPR;
   case UNION_TYPE: return GS_UNION_TYPE;
   case UNLE_EXPR: return GS_UNLE_EXPR;
   case UNLT_EXPR: return GS_UNLT_EXPR;
   case UNORDERED_EXPR: return GS_UNORDERED_EXPR;
   case VALUE_HANDLE: return GS_VALUE_HANDLE;
   case VAR_DECL: return GS_VAR_DECL;
   case VA_ARG_EXPR: return GS_VA_ARG_EXPR;
   case VECTOR_CST: return GS_VECTOR_CST;
   case VECTOR_TYPE: return GS_VECTOR_TYPE;
   case VEC_COND_EXPR: return GS_VEC_COND_EXPR;
   case VEC_DELETE_EXPR: return GS_VEC_DELETE_EXPR;
   case VEC_NEW_EXPR: return GS_VEC_NEW_EXPR;
   case VIEW_CONVERT_EXPR: return GS_VIEW_CONVERT_EXPR;
   case VOID_TYPE: return GS_VOID_TYPE;
   case WHILE_STMT: return GS_WHILE_STMT;
   case WITH_CLEANUP_EXPR: return GS_WITH_CLEANUP_EXPR;
   case WITH_SIZE_EXPR: return GS_WITH_SIZE_EXPR;
   case TEMPLATE_TEMPLATE_PARM: return GS_TEMPLATE_TEMPLATE_PARM;
   case FREQ_HINT_STMT: return GS_FREQ_HINT_STMT;
   case ZDL_STMT: return GS_ZDL_STMT;
 }
 gcc_assert(0);
 return (gs_code_t) 0;
}

static inline gs_tree_code_class_t
gcc_class2gs_class (int class) 
{
  switch (class) {
    case tcc_exceptional: return GS_TCC_EXCEPTIONAL;
    case tcc_constant: return GS_TCC_CONSTANT;
    case tcc_type: return GS_TCC_TYPE;     
    case tcc_declaration: return GS_TCC_DECLARATION;
    case tcc_reference: return GS_TCC_REFERENCE; 
    case tcc_comparison: return GS_TCC_COMPARISON;
    case tcc_unary: return GS_TCC_UNARY;
    case tcc_binary: return GS_TCC_BINARY;  
    case tcc_statement: return GS_TCC_STATEMENT;
    case tcc_expression: return GS_TCC_EXPRESSION;
  }
  gcc_assert (0);
  return (gs_tree_code_class_t) 0;
}

static inline gsbi_t
gcc_built_in2gsbi (enum built_in_function code)
{
  switch (code) {

    case BUILT_IN_ACOS: return GSBI_BUILT_IN_ACOS;
    case BUILT_IN_ACOSF: return GSBI_BUILT_IN_ACOSF;
    case BUILT_IN_ACOSH: return GSBI_BUILT_IN_ACOSH;
    case BUILT_IN_ACOSHF: return GSBI_BUILT_IN_ACOSHF;
    case BUILT_IN_ACOSHL: return GSBI_BUILT_IN_ACOSHL;
    case BUILT_IN_ACOSL: return GSBI_BUILT_IN_ACOSL;
    case BUILT_IN_ASIN: return GSBI_BUILT_IN_ASIN;
    case BUILT_IN_ASINF: return GSBI_BUILT_IN_ASINF;
    case BUILT_IN_ASINH: return GSBI_BUILT_IN_ASINH;
    case BUILT_IN_ASINHF: return GSBI_BUILT_IN_ASINHF;
    case BUILT_IN_ASINHL: return GSBI_BUILT_IN_ASINHL;
    case BUILT_IN_ASINL: return GSBI_BUILT_IN_ASINL;
    case BUILT_IN_ATAN: return GSBI_BUILT_IN_ATAN;
    case BUILT_IN_ATAN2: return GSBI_BUILT_IN_ATAN2;
    case BUILT_IN_ATAN2F: return GSBI_BUILT_IN_ATAN2F;
    case BUILT_IN_ATAN2L: return GSBI_BUILT_IN_ATAN2L;
    case BUILT_IN_ATANF: return GSBI_BUILT_IN_ATANF;
    case BUILT_IN_ATANH: return GSBI_BUILT_IN_ATANH;
    case BUILT_IN_ATANHF: return GSBI_BUILT_IN_ATANHF;
    case BUILT_IN_ATANHL: return GSBI_BUILT_IN_ATANHL;
    case BUILT_IN_ATANL: return GSBI_BUILT_IN_ATANL;
    case BUILT_IN_CBRT: return GSBI_BUILT_IN_CBRT;
    case BUILT_IN_CBRTF: return GSBI_BUILT_IN_CBRTF;
    case BUILT_IN_CBRTL: return GSBI_BUILT_IN_CBRTL;
    case BUILT_IN_CEIL: return GSBI_BUILT_IN_CEIL;
    case BUILT_IN_CEILF: return GSBI_BUILT_IN_CEILF;
    case BUILT_IN_CEILL: return GSBI_BUILT_IN_CEILL;
    case BUILT_IN_COPYSIGN: return GSBI_BUILT_IN_COPYSIGN;
    case BUILT_IN_COPYSIGNF: return GSBI_BUILT_IN_COPYSIGNF;
    case BUILT_IN_COPYSIGNL: return GSBI_BUILT_IN_COPYSIGNL;
    case BUILT_IN_COS: return GSBI_BUILT_IN_COS;
    case BUILT_IN_COSF: return GSBI_BUILT_IN_COSF;
    case BUILT_IN_COSH: return GSBI_BUILT_IN_COSH;
    case BUILT_IN_COSHF: return GSBI_BUILT_IN_COSHF;
    case BUILT_IN_COSHL: return GSBI_BUILT_IN_COSHL;
    case BUILT_IN_COSL: return GSBI_BUILT_IN_COSL;
    case BUILT_IN_DREM: return GSBI_BUILT_IN_DREM;
    case BUILT_IN_DREMF: return GSBI_BUILT_IN_DREMF;
    case BUILT_IN_DREML: return GSBI_BUILT_IN_DREML;
    case BUILT_IN_ERF: return GSBI_BUILT_IN_ERF;
    case BUILT_IN_ERFC: return GSBI_BUILT_IN_ERFC;
    case BUILT_IN_ERFCF: return GSBI_BUILT_IN_ERFCF;
    case BUILT_IN_ERFCL: return GSBI_BUILT_IN_ERFCL;
    case BUILT_IN_ERFF: return GSBI_BUILT_IN_ERFF;
    case BUILT_IN_ERFL: return GSBI_BUILT_IN_ERFL;
    case BUILT_IN_EXP: return GSBI_BUILT_IN_EXP;
    case BUILT_IN_EXP10: return GSBI_BUILT_IN_EXP10;
    case BUILT_IN_EXP10F: return GSBI_BUILT_IN_EXP10F;
    case BUILT_IN_EXP10L: return GSBI_BUILT_IN_EXP10L;
    case BUILT_IN_EXP2: return GSBI_BUILT_IN_EXP2;
    case BUILT_IN_EXP2F: return GSBI_BUILT_IN_EXP2F;
    case BUILT_IN_EXP2L: return GSBI_BUILT_IN_EXP2L;
    case BUILT_IN_EXPF: return GSBI_BUILT_IN_EXPF;
    case BUILT_IN_EXPL: return GSBI_BUILT_IN_EXPL;
    case BUILT_IN_EXPM1: return GSBI_BUILT_IN_EXPM1;
    case BUILT_IN_EXPM1F: return GSBI_BUILT_IN_EXPM1F;
    case BUILT_IN_EXPM1L: return GSBI_BUILT_IN_EXPM1L;
    case BUILT_IN_FABS: return GSBI_BUILT_IN_FABS;
    case BUILT_IN_FABSF: return GSBI_BUILT_IN_FABSF;
    case BUILT_IN_FABSL: return GSBI_BUILT_IN_FABSL;
    case BUILT_IN_FDIM: return GSBI_BUILT_IN_FDIM;
    case BUILT_IN_FDIMF: return GSBI_BUILT_IN_FDIMF;
    case BUILT_IN_FDIML: return GSBI_BUILT_IN_FDIML;
    case BUILT_IN_FLOOR: return GSBI_BUILT_IN_FLOOR;
    case BUILT_IN_FLOORF: return GSBI_BUILT_IN_FLOORF;
    case BUILT_IN_FLOORL: return GSBI_BUILT_IN_FLOORL;
    case BUILT_IN_FMA: return GSBI_BUILT_IN_FMA;
    case BUILT_IN_FMAF: return GSBI_BUILT_IN_FMAF;
    case BUILT_IN_FMAL: return GSBI_BUILT_IN_FMAL;
    case BUILT_IN_FMAX: return GSBI_BUILT_IN_FMAX;
    case BUILT_IN_FMAXF: return GSBI_BUILT_IN_FMAXF;
    case BUILT_IN_FMAXL: return GSBI_BUILT_IN_FMAXL;
    case BUILT_IN_FMIN: return GSBI_BUILT_IN_FMIN;
    case BUILT_IN_FMINF: return GSBI_BUILT_IN_FMINF;
    case BUILT_IN_FMINL: return GSBI_BUILT_IN_FMINL;
    case BUILT_IN_FMOD: return GSBI_BUILT_IN_FMOD;
    case BUILT_IN_FMODF: return GSBI_BUILT_IN_FMODF;
    case BUILT_IN_FMODL: return GSBI_BUILT_IN_FMODL;
    case BUILT_IN_FREXP: return GSBI_BUILT_IN_FREXP;
    case BUILT_IN_FREXPF: return GSBI_BUILT_IN_FREXPF;
    case BUILT_IN_FREXPL: return GSBI_BUILT_IN_FREXPL;
    case BUILT_IN_GAMMA: return GSBI_BUILT_IN_GAMMA;
    case BUILT_IN_GAMMAF: return GSBI_BUILT_IN_GAMMAF;
    case BUILT_IN_GAMMAL: return GSBI_BUILT_IN_GAMMAL;
    case BUILT_IN_HUGE_VAL: return GSBI_BUILT_IN_HUGE_VAL;
    case BUILT_IN_HUGE_VALF: return GSBI_BUILT_IN_HUGE_VALF;
    case BUILT_IN_HUGE_VALL: return GSBI_BUILT_IN_HUGE_VALL;
    case BUILT_IN_HYPOT: return GSBI_BUILT_IN_HYPOT;
    case BUILT_IN_HYPOTF: return GSBI_BUILT_IN_HYPOTF;
    case BUILT_IN_HYPOTL: return GSBI_BUILT_IN_HYPOTL;
    case BUILT_IN_ILOGB: return GSBI_BUILT_IN_ILOGB;
    case BUILT_IN_ILOGBF: return GSBI_BUILT_IN_ILOGBF;
    case BUILT_IN_ILOGBL: return GSBI_BUILT_IN_ILOGBL;
    case BUILT_IN_INF: return GSBI_BUILT_IN_INF;
    case BUILT_IN_INFF: return GSBI_BUILT_IN_INFF;
    case BUILT_IN_INFL: return GSBI_BUILT_IN_INFL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_INFD32: return GSBI_BUILT_IN_INFD32;
    case BUILT_IN_INFD64: return GSBI_BUILT_IN_INFD64;
    case BUILT_IN_INFD128: return GSBI_BUILT_IN_INFD128;
#endif
    case BUILT_IN_J0: return GSBI_BUILT_IN_J0;
    case BUILT_IN_J0F: return GSBI_BUILT_IN_J0F;
    case BUILT_IN_J0L: return GSBI_BUILT_IN_J0L;
    case BUILT_IN_J1: return GSBI_BUILT_IN_J1;
    case BUILT_IN_J1F: return GSBI_BUILT_IN_J1F;
    case BUILT_IN_J1L: return GSBI_BUILT_IN_J1L;
    case BUILT_IN_JN: return GSBI_BUILT_IN_JN;
    case BUILT_IN_JNF: return GSBI_BUILT_IN_JNF;
    case BUILT_IN_JNL: return GSBI_BUILT_IN_JNL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_LCEIL: return GSBI_BUILT_IN_LCEIL;
    case BUILT_IN_LCEILF: return GSBI_BUILT_IN_LCEILF;
    case BUILT_IN_LCEILL: return GSBI_BUILT_IN_LCEILL;
#endif
    case BUILT_IN_LDEXP: return GSBI_BUILT_IN_LDEXP;
    case BUILT_IN_LDEXPF: return GSBI_BUILT_IN_LDEXPF;
    case BUILT_IN_LDEXPL: return GSBI_BUILT_IN_LDEXPL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_LFLOOR: return GSBI_BUILT_IN_LFLOOR;
    case BUILT_IN_LFLOORF: return GSBI_BUILT_IN_LFLOORF;
    case BUILT_IN_LFLOORL: return GSBI_BUILT_IN_LFLOORL;
#endif
    case BUILT_IN_LGAMMA: return GSBI_BUILT_IN_LGAMMA;
    case BUILT_IN_LGAMMAF: return GSBI_BUILT_IN_LGAMMAF;
    case BUILT_IN_LGAMMAL: return GSBI_BUILT_IN_LGAMMAL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_LLCEIL: return GSBI_BUILT_IN_LLCEIL;
    case BUILT_IN_LLCEILF: return GSBI_BUILT_IN_LLCEILF;
    case BUILT_IN_LLCEILL: return GSBI_BUILT_IN_LLCEILL;
    case BUILT_IN_LLFLOOR: return GSBI_BUILT_IN_LLFLOOR;
    case BUILT_IN_LLFLOORF: return GSBI_BUILT_IN_LLFLOORF;
    case BUILT_IN_LLFLOORL: return GSBI_BUILT_IN_LLFLOORL;
#endif
    case BUILT_IN_LLRINT: return GSBI_BUILT_IN_LLRINT;
    case BUILT_IN_LLRINTF: return GSBI_BUILT_IN_LLRINTF;
    case BUILT_IN_LLRINTL: return GSBI_BUILT_IN_LLRINTL;
    case BUILT_IN_LLROUND: return GSBI_BUILT_IN_LLROUND;
    case BUILT_IN_LLROUNDF: return GSBI_BUILT_IN_LLROUNDF;
    case BUILT_IN_LLROUNDL: return GSBI_BUILT_IN_LLROUNDL;
    case BUILT_IN_LOG: return GSBI_BUILT_IN_LOG;
    case BUILT_IN_LOG10: return GSBI_BUILT_IN_LOG10;
    case BUILT_IN_LOG10F: return GSBI_BUILT_IN_LOG10F;
    case BUILT_IN_LOG10L: return GSBI_BUILT_IN_LOG10L;
    case BUILT_IN_LOG1P: return GSBI_BUILT_IN_LOG1P;
    case BUILT_IN_LOG1PF: return GSBI_BUILT_IN_LOG1PF;
    case BUILT_IN_LOG1PL: return GSBI_BUILT_IN_LOG1PL;
    case BUILT_IN_LOG2: return GSBI_BUILT_IN_LOG2;
    case BUILT_IN_LOG2F: return GSBI_BUILT_IN_LOG2F;
    case BUILT_IN_LOG2L: return GSBI_BUILT_IN_LOG2L;
    case BUILT_IN_LOGB: return GSBI_BUILT_IN_LOGB;
    case BUILT_IN_LOGBF: return GSBI_BUILT_IN_LOGBF;
    case BUILT_IN_LOGBL: return GSBI_BUILT_IN_LOGBL;
    case BUILT_IN_LOGF: return GSBI_BUILT_IN_LOGF;
    case BUILT_IN_LOGL: return GSBI_BUILT_IN_LOGL;
    case BUILT_IN_LRINT: return GSBI_BUILT_IN_LRINT;
    case BUILT_IN_LRINTF: return GSBI_BUILT_IN_LRINTF;
    case BUILT_IN_LRINTL: return GSBI_BUILT_IN_LRINTL;
    case BUILT_IN_LROUND: return GSBI_BUILT_IN_LROUND;
    case BUILT_IN_LROUNDF: return GSBI_BUILT_IN_LROUNDF;
    case BUILT_IN_LROUNDL: return GSBI_BUILT_IN_LROUNDL;
    case BUILT_IN_MODF: return GSBI_BUILT_IN_MODF;
    case BUILT_IN_MODFF: return GSBI_BUILT_IN_MODFF;
    case BUILT_IN_MODFL: return GSBI_BUILT_IN_MODFL;
    case BUILT_IN_NAN: return GSBI_BUILT_IN_NAN;
    case BUILT_IN_NANF: return GSBI_BUILT_IN_NANF;
    case BUILT_IN_NANL: return GSBI_BUILT_IN_NANL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_NAND32: return GSBI_BUILT_IN_NAND32;
    case BUILT_IN_NAND64: return GSBI_BUILT_IN_NAND64;
    case BUILT_IN_NAND128: return GSBI_BUILT_IN_NAND128;
#endif
    case BUILT_IN_NANS: return GSBI_BUILT_IN_NANS;
    case BUILT_IN_NANSF: return GSBI_BUILT_IN_NANSF;
    case BUILT_IN_NANSL: return GSBI_BUILT_IN_NANSL;
    case BUILT_IN_NEARBYINT: return GSBI_BUILT_IN_NEARBYINT;
    case BUILT_IN_NEARBYINTF: return GSBI_BUILT_IN_NEARBYINTF;
    case BUILT_IN_NEARBYINTL: return GSBI_BUILT_IN_NEARBYINTL;
    case BUILT_IN_NEXTAFTER: return GSBI_BUILT_IN_NEXTAFTER;
    case BUILT_IN_NEXTAFTERF: return GSBI_BUILT_IN_NEXTAFTERF;
    case BUILT_IN_NEXTAFTERL: return GSBI_BUILT_IN_NEXTAFTERL;
    case BUILT_IN_NEXTTOWARD: return GSBI_BUILT_IN_NEXTTOWARD;
    case BUILT_IN_NEXTTOWARDF: return GSBI_BUILT_IN_NEXTTOWARDF;
    case BUILT_IN_NEXTTOWARDL: return GSBI_BUILT_IN_NEXTTOWARDL;
    case BUILT_IN_POW: return GSBI_BUILT_IN_POW;
    case BUILT_IN_POW10: return GSBI_BUILT_IN_POW10;
    case BUILT_IN_POW10F: return GSBI_BUILT_IN_POW10F;
    case BUILT_IN_POW10L: return GSBI_BUILT_IN_POW10L;
    case BUILT_IN_POWF: return GSBI_BUILT_IN_POWF;
    case BUILT_IN_POWI: return GSBI_BUILT_IN_POWI;
    case BUILT_IN_POWIF: return GSBI_BUILT_IN_POWIF;
    case BUILT_IN_POWIL: return GSBI_BUILT_IN_POWIL;
    case BUILT_IN_POWL: return GSBI_BUILT_IN_POWL;
    case BUILT_IN_REMAINDER: return GSBI_BUILT_IN_REMAINDER;
    case BUILT_IN_REMAINDERF: return GSBI_BUILT_IN_REMAINDERF;
    case BUILT_IN_REMAINDERL: return GSBI_BUILT_IN_REMAINDERL;
    case BUILT_IN_REMQUO: return GSBI_BUILT_IN_REMQUO;
    case BUILT_IN_REMQUOF: return GSBI_BUILT_IN_REMQUOF;
    case BUILT_IN_REMQUOL: return GSBI_BUILT_IN_REMQUOL;
    case BUILT_IN_RINT: return GSBI_BUILT_IN_RINT;
    case BUILT_IN_RINTF: return GSBI_BUILT_IN_RINTF;
    case BUILT_IN_RINTL: return GSBI_BUILT_IN_RINTL;
    case BUILT_IN_ROUND: return GSBI_BUILT_IN_ROUND;
    case BUILT_IN_ROUNDF: return GSBI_BUILT_IN_ROUNDF;
    case BUILT_IN_ROUNDL: return GSBI_BUILT_IN_ROUNDL;
    case BUILT_IN_SCALB: return GSBI_BUILT_IN_SCALB;
    case BUILT_IN_SCALBF: return GSBI_BUILT_IN_SCALBF;
    case BUILT_IN_SCALBL: return GSBI_BUILT_IN_SCALBL;
    case BUILT_IN_SCALBLN: return GSBI_BUILT_IN_SCALBLN;
    case BUILT_IN_SCALBLNF: return GSBI_BUILT_IN_SCALBLNF;
    case BUILT_IN_SCALBLNL: return GSBI_BUILT_IN_SCALBLNL;
    case BUILT_IN_SCALBN: return GSBI_BUILT_IN_SCALBN;
    case BUILT_IN_SCALBNF: return GSBI_BUILT_IN_SCALBNF;
    case BUILT_IN_SCALBNL: return GSBI_BUILT_IN_SCALBNL;
    case BUILT_IN_SIGNBIT: return GSBI_BUILT_IN_SIGNBIT;
    case BUILT_IN_SIGNBITF: return GSBI_BUILT_IN_SIGNBITF;
    case BUILT_IN_SIGNBITL: return GSBI_BUILT_IN_SIGNBITL;
    case BUILT_IN_SIGNIFICAND: return GSBI_BUILT_IN_SIGNIFICAND;
    case BUILT_IN_SIGNIFICANDF: return GSBI_BUILT_IN_SIGNIFICANDF;
    case BUILT_IN_SIGNIFICANDL: return GSBI_BUILT_IN_SIGNIFICANDL;
    case BUILT_IN_SIN: return GSBI_BUILT_IN_SIN;
    case BUILT_IN_SINCOS: return GSBI_BUILT_IN_SINCOS;
    case BUILT_IN_SINCOSF: return GSBI_BUILT_IN_SINCOSF;
    case BUILT_IN_SINCOSL: return GSBI_BUILT_IN_SINCOSL;
    case BUILT_IN_SINF: return GSBI_BUILT_IN_SINF;
    case BUILT_IN_SINH: return GSBI_BUILT_IN_SINH;
    case BUILT_IN_SINHF: return GSBI_BUILT_IN_SINHF;
    case BUILT_IN_SINHL: return GSBI_BUILT_IN_SINHL;
    case BUILT_IN_SINL: return GSBI_BUILT_IN_SINL;
    case BUILT_IN_SQRT: return GSBI_BUILT_IN_SQRT;
    case BUILT_IN_SQRTF: return GSBI_BUILT_IN_SQRTF;
    case BUILT_IN_SQRTL: return GSBI_BUILT_IN_SQRTL;
    case BUILT_IN_TAN: return GSBI_BUILT_IN_TAN;
    case BUILT_IN_TANF: return GSBI_BUILT_IN_TANF;
    case BUILT_IN_TANH: return GSBI_BUILT_IN_TANH;
    case BUILT_IN_TANHF: return GSBI_BUILT_IN_TANHF;
    case BUILT_IN_TANHL: return GSBI_BUILT_IN_TANHL;
    case BUILT_IN_TANL: return GSBI_BUILT_IN_TANL;
    case BUILT_IN_TGAMMA: return GSBI_BUILT_IN_TGAMMA;
    case BUILT_IN_TGAMMAF: return GSBI_BUILT_IN_TGAMMAF;
    case BUILT_IN_TGAMMAL: return GSBI_BUILT_IN_TGAMMAL;
    case BUILT_IN_TRUNC: return GSBI_BUILT_IN_TRUNC;
    case BUILT_IN_TRUNCF: return GSBI_BUILT_IN_TRUNCF;
    case BUILT_IN_TRUNCL: return GSBI_BUILT_IN_TRUNCL;
    case BUILT_IN_Y0: return GSBI_BUILT_IN_Y0;
    case BUILT_IN_Y0F: return GSBI_BUILT_IN_Y0F;
    case BUILT_IN_Y0L: return GSBI_BUILT_IN_Y0L;
    case BUILT_IN_Y1: return GSBI_BUILT_IN_Y1;
    case BUILT_IN_Y1F: return GSBI_BUILT_IN_Y1F;
    case BUILT_IN_Y1L: return GSBI_BUILT_IN_Y1L;
    case BUILT_IN_YN: return GSBI_BUILT_IN_YN;
    case BUILT_IN_YNF: return GSBI_BUILT_IN_YNF;
    case BUILT_IN_YNL: return GSBI_BUILT_IN_YNL;
    case BUILT_IN_CABS: return GSBI_BUILT_IN_CABS;
    case BUILT_IN_CABSF: return GSBI_BUILT_IN_CABSF;
    case BUILT_IN_CABSL: return GSBI_BUILT_IN_CABSL;
    case BUILT_IN_CACOS: return GSBI_BUILT_IN_CACOS;
    case BUILT_IN_CACOSF: return GSBI_BUILT_IN_CACOSF;
    case BUILT_IN_CACOSH: return GSBI_BUILT_IN_CACOSH;
    case BUILT_IN_CACOSHF: return GSBI_BUILT_IN_CACOSHF;
    case BUILT_IN_CACOSHL: return GSBI_BUILT_IN_CACOSHL;
    case BUILT_IN_CACOSL: return GSBI_BUILT_IN_CACOSL;
    case BUILT_IN_CARG: return GSBI_BUILT_IN_CARG;
    case BUILT_IN_CARGF: return GSBI_BUILT_IN_CARGF;
    case BUILT_IN_CARGL: return GSBI_BUILT_IN_CARGL;
    case BUILT_IN_CASIN: return GSBI_BUILT_IN_CASIN;
    case BUILT_IN_CASINF: return GSBI_BUILT_IN_CASINF;
    case BUILT_IN_CASINH: return GSBI_BUILT_IN_CASINH;
    case BUILT_IN_CASINHF: return GSBI_BUILT_IN_CASINHF;
    case BUILT_IN_CASINHL: return GSBI_BUILT_IN_CASINHL;
    case BUILT_IN_CASINL: return GSBI_BUILT_IN_CASINL;
    case BUILT_IN_CATAN: return GSBI_BUILT_IN_CATAN;
    case BUILT_IN_CATANF: return GSBI_BUILT_IN_CATANF;
    case BUILT_IN_CATANH: return GSBI_BUILT_IN_CATANH;
    case BUILT_IN_CATANHF: return GSBI_BUILT_IN_CATANHF;
    case BUILT_IN_CATANHL: return GSBI_BUILT_IN_CATANHL;
    case BUILT_IN_CATANL: return GSBI_BUILT_IN_CATANL;
    case BUILT_IN_CCOS: return GSBI_BUILT_IN_CCOS;
    case BUILT_IN_CCOSF: return GSBI_BUILT_IN_CCOSF;
    case BUILT_IN_CCOSH: return GSBI_BUILT_IN_CCOSH;
    case BUILT_IN_CCOSHF: return GSBI_BUILT_IN_CCOSHF;
    case BUILT_IN_CCOSHL: return GSBI_BUILT_IN_CCOSHL;
    case BUILT_IN_CCOSL: return GSBI_BUILT_IN_CCOSL;
    case BUILT_IN_CEXP: return GSBI_BUILT_IN_CEXP;
    case BUILT_IN_CEXPF: return GSBI_BUILT_IN_CEXPF;
    case BUILT_IN_CEXPL: return GSBI_BUILT_IN_CEXPL;
    case BUILT_IN_CIMAG: return GSBI_BUILT_IN_CIMAG;
    case BUILT_IN_CIMAGF: return GSBI_BUILT_IN_CIMAGF;
    case BUILT_IN_CIMAGL: return GSBI_BUILT_IN_CIMAGL;
    case BUILT_IN_CLOG: return GSBI_BUILT_IN_CLOG;
    case BUILT_IN_CLOGF: return GSBI_BUILT_IN_CLOGF;
    case BUILT_IN_CLOGL: return GSBI_BUILT_IN_CLOGL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_CLOG10: return GSBI_BUILT_IN_CLOG10;
    case BUILT_IN_CLOG10F: return GSBI_BUILT_IN_CLOG10F;
    case BUILT_IN_CLOG10L: return GSBI_BUILT_IN_CLOG10L;
#endif
    case BUILT_IN_CONJ: return GSBI_BUILT_IN_CONJ;
    case BUILT_IN_CONJF: return GSBI_BUILT_IN_CONJF;
    case BUILT_IN_CONJL: return GSBI_BUILT_IN_CONJL;
    case BUILT_IN_CPOW: return GSBI_BUILT_IN_CPOW;
    case BUILT_IN_CPOWF: return GSBI_BUILT_IN_CPOWF;
    case BUILT_IN_CPOWL: return GSBI_BUILT_IN_CPOWL;
    case BUILT_IN_CPROJ: return GSBI_BUILT_IN_CPROJ;
    case BUILT_IN_CPROJF: return GSBI_BUILT_IN_CPROJF;
    case BUILT_IN_CPROJL: return GSBI_BUILT_IN_CPROJL;
    case BUILT_IN_CREAL: return GSBI_BUILT_IN_CREAL;
    case BUILT_IN_CREALF: return GSBI_BUILT_IN_CREALF;
    case BUILT_IN_CREALL: return GSBI_BUILT_IN_CREALL;
    case BUILT_IN_CSIN: return GSBI_BUILT_IN_CSIN;
    case BUILT_IN_CSINF: return GSBI_BUILT_IN_CSINF;
    case BUILT_IN_CSINH: return GSBI_BUILT_IN_CSINH;
    case BUILT_IN_CSINHF: return GSBI_BUILT_IN_CSINHF;
    case BUILT_IN_CSINHL: return GSBI_BUILT_IN_CSINHL;
    case BUILT_IN_CSINL: return GSBI_BUILT_IN_CSINL;
    case BUILT_IN_CSQRT: return GSBI_BUILT_IN_CSQRT;
    case BUILT_IN_CSQRTF: return GSBI_BUILT_IN_CSQRTF;
    case BUILT_IN_CSQRTL: return GSBI_BUILT_IN_CSQRTL;
    case BUILT_IN_CTAN: return GSBI_BUILT_IN_CTAN;
    case BUILT_IN_CTANF: return GSBI_BUILT_IN_CTANF;
    case BUILT_IN_CTANH: return GSBI_BUILT_IN_CTANH;
    case BUILT_IN_CTANHF: return GSBI_BUILT_IN_CTANHF;
    case BUILT_IN_CTANHL: return GSBI_BUILT_IN_CTANHL;
    case BUILT_IN_CTANL: return GSBI_BUILT_IN_CTANL;
    case BUILT_IN_BCMP: return GSBI_BUILT_IN_BCMP;
    case BUILT_IN_BCOPY: return GSBI_BUILT_IN_BCOPY;
    case BUILT_IN_BZERO: return GSBI_BUILT_IN_BZERO;
    case BUILT_IN_INDEX: return GSBI_BUILT_IN_INDEX;
    case BUILT_IN_MEMCMP: return GSBI_BUILT_IN_MEMCMP;
    case BUILT_IN_MEMCPY: return GSBI_BUILT_IN_MEMCPY;
    case BUILT_IN_MEMMOVE: return GSBI_BUILT_IN_MEMMOVE;
    case BUILT_IN_MEMPCPY: return GSBI_BUILT_IN_MEMPCPY;
    case BUILT_IN_MEMSET: return GSBI_BUILT_IN_MEMSET;
    case BUILT_IN_RINDEX: return GSBI_BUILT_IN_RINDEX;
    case BUILT_IN_STPCPY: return GSBI_BUILT_IN_STPCPY;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_STPNCPY: return GSBI_BUILT_IN_STPNCPY;
    case BUILT_IN_STRCASECMP: return GSBI_BUILT_IN_STRCASECMP;
#endif
    case BUILT_IN_STRCAT: return GSBI_BUILT_IN_STRCAT;
    case BUILT_IN_STRCHR: return GSBI_BUILT_IN_STRCHR;
    case BUILT_IN_STRCMP: return GSBI_BUILT_IN_STRCMP;
    case BUILT_IN_STRCPY: return GSBI_BUILT_IN_STRCPY;
    case BUILT_IN_STRCSPN: return GSBI_BUILT_IN_STRCSPN;
    case BUILT_IN_STRDUP: return GSBI_BUILT_IN_STRDUP;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_STRNDUP: return GSBI_BUILT_IN_STRNDUP;
#endif
    case BUILT_IN_STRLEN: return GSBI_BUILT_IN_STRLEN;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_STRNCASECMP: return GSBI_BUILT_IN_STRNCASECMP;
#endif
    case BUILT_IN_STRNCAT: return GSBI_BUILT_IN_STRNCAT;
    case BUILT_IN_STRNCMP: return GSBI_BUILT_IN_STRNCMP;
    case BUILT_IN_STRNCPY: return GSBI_BUILT_IN_STRNCPY;
    case BUILT_IN_STRPBRK: return GSBI_BUILT_IN_STRPBRK;
    case BUILT_IN_STRRCHR: return GSBI_BUILT_IN_STRRCHR;
    case BUILT_IN_STRSPN: return GSBI_BUILT_IN_STRSPN;
    case BUILT_IN_STRSTR: return GSBI_BUILT_IN_STRSTR;
    case BUILT_IN_FPRINTF: return GSBI_BUILT_IN_FPRINTF;
    case BUILT_IN_FPRINTF_UNLOCKED: return GSBI_BUILT_IN_FPRINTF_UNLOCKED;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_PUTC: return GSBI_BUILT_IN_PUTC;
    case BUILT_IN_PUTC_UNLOCKED: return GSBI_BUILT_IN_PUTC_UNLOCKED;
#endif
    case BUILT_IN_FPUTC: return GSBI_BUILT_IN_FPUTC;
    case BUILT_IN_FPUTC_UNLOCKED: return GSBI_BUILT_IN_FPUTC_UNLOCKED;
    case BUILT_IN_FPUTS: return GSBI_BUILT_IN_FPUTS;
    case BUILT_IN_FPUTS_UNLOCKED: return GSBI_BUILT_IN_FPUTS_UNLOCKED;
    case BUILT_IN_FSCANF: return GSBI_BUILT_IN_FSCANF;
    case BUILT_IN_FWRITE: return GSBI_BUILT_IN_FWRITE;
    case BUILT_IN_FWRITE_UNLOCKED: return GSBI_BUILT_IN_FWRITE_UNLOCKED;
    case BUILT_IN_PRINTF: return GSBI_BUILT_IN_PRINTF;
    case BUILT_IN_PRINTF_UNLOCKED: return GSBI_BUILT_IN_PRINTF_UNLOCKED;
    case BUILT_IN_PUTCHAR: return GSBI_BUILT_IN_PUTCHAR;
    case BUILT_IN_PUTCHAR_UNLOCKED: return GSBI_BUILT_IN_PUTCHAR_UNLOCKED;
    case BUILT_IN_PUTS: return GSBI_BUILT_IN_PUTS;
    case BUILT_IN_PUTS_UNLOCKED: return GSBI_BUILT_IN_PUTS_UNLOCKED;
    case BUILT_IN_SCANF: return GSBI_BUILT_IN_SCANF;
    case BUILT_IN_SNPRINTF: return GSBI_BUILT_IN_SNPRINTF;
    case BUILT_IN_SPRINTF: return GSBI_BUILT_IN_SPRINTF;
    case BUILT_IN_SSCANF: return GSBI_BUILT_IN_SSCANF;
    case BUILT_IN_VFPRINTF: return GSBI_BUILT_IN_VFPRINTF;
    case BUILT_IN_VFSCANF: return GSBI_BUILT_IN_VFSCANF;
    case BUILT_IN_VPRINTF: return GSBI_BUILT_IN_VPRINTF;
    case BUILT_IN_VSCANF: return GSBI_BUILT_IN_VSCANF;
    case BUILT_IN_VSNPRINTF: return GSBI_BUILT_IN_VSNPRINTF;
    case BUILT_IN_VSPRINTF: return GSBI_BUILT_IN_VSPRINTF;
    case BUILT_IN_VSSCANF: return GSBI_BUILT_IN_VSSCANF;
    case BUILT_IN_ISALNUM: return GSBI_BUILT_IN_ISALNUM;
    case BUILT_IN_ISALPHA: return GSBI_BUILT_IN_ISALPHA;
    case BUILT_IN_ISASCII: return GSBI_BUILT_IN_ISASCII;
    case BUILT_IN_ISBLANK: return GSBI_BUILT_IN_ISBLANK;
    case BUILT_IN_ISCNTRL: return GSBI_BUILT_IN_ISCNTRL;
    case BUILT_IN_ISDIGIT: return GSBI_BUILT_IN_ISDIGIT;
    case BUILT_IN_ISGRAPH: return GSBI_BUILT_IN_ISGRAPH;
    case BUILT_IN_ISLOWER: return GSBI_BUILT_IN_ISLOWER;
    case BUILT_IN_ISPRINT: return GSBI_BUILT_IN_ISPRINT;
    case BUILT_IN_ISPUNCT: return GSBI_BUILT_IN_ISPUNCT;
    case BUILT_IN_ISSPACE: return GSBI_BUILT_IN_ISSPACE;
    case BUILT_IN_ISUPPER: return GSBI_BUILT_IN_ISUPPER;
    case BUILT_IN_ISXDIGIT: return GSBI_BUILT_IN_ISXDIGIT;
    case BUILT_IN_TOASCII: return GSBI_BUILT_IN_TOASCII;
    case BUILT_IN_TOLOWER: return GSBI_BUILT_IN_TOLOWER;
    case BUILT_IN_TOUPPER: return GSBI_BUILT_IN_TOUPPER;
    case BUILT_IN_ISWALNUM: return GSBI_BUILT_IN_ISWALNUM;
    case BUILT_IN_ISWALPHA: return GSBI_BUILT_IN_ISWALPHA;
    case BUILT_IN_ISWBLANK: return GSBI_BUILT_IN_ISWBLANK;
    case BUILT_IN_ISWCNTRL: return GSBI_BUILT_IN_ISWCNTRL;
    case BUILT_IN_ISWDIGIT: return GSBI_BUILT_IN_ISWDIGIT;
    case BUILT_IN_ISWGRAPH: return GSBI_BUILT_IN_ISWGRAPH;
    case BUILT_IN_ISWLOWER: return GSBI_BUILT_IN_ISWLOWER;
    case BUILT_IN_ISWPRINT: return GSBI_BUILT_IN_ISWPRINT;
    case BUILT_IN_ISWPUNCT: return GSBI_BUILT_IN_ISWPUNCT;
    case BUILT_IN_ISWSPACE: return GSBI_BUILT_IN_ISWSPACE;
    case BUILT_IN_ISWUPPER: return GSBI_BUILT_IN_ISWUPPER;
    case BUILT_IN_ISWXDIGIT: return GSBI_BUILT_IN_ISWXDIGIT;
    case BUILT_IN_TOWLOWER: return GSBI_BUILT_IN_TOWLOWER;
    case BUILT_IN_TOWUPPER: return GSBI_BUILT_IN_TOWUPPER;
    case BUILT_IN_CTYPE_B_LOC: return GSBI_BUILT_IN_CTYPE_B_LOC;
    case BUILT_IN_CTYPE_TOUPPER_LOC: return GSBI_BUILT_IN_CTYPE_TOUPPER_LOC;
    case BUILT_IN_CTYPE_TOLOWER_LOC: return GSBI_BUILT_IN_CTYPE_TOLOWER_LOC;
    case BUILT_IN_ABORT: return GSBI_BUILT_IN_ABORT;
    case BUILT_IN_ABS: return GSBI_BUILT_IN_ABS;
    case BUILT_IN_AGGREGATE_INCOMING_ADDRESS: return GSBI_BUILT_IN_AGGREGATE_INCOMING_ADDRESS;
    case BUILT_IN_ALLOCA: return GSBI_BUILT_IN_ALLOCA;
    case BUILT_IN_APPLY: return GSBI_BUILT_IN_APPLY;
    case BUILT_IN_APPLY_ARGS: return GSBI_BUILT_IN_APPLY_ARGS;
    case BUILT_IN_ARGS_INFO: return GSBI_BUILT_IN_ARGS_INFO;
    case BUILT_IN_CALLOC: return GSBI_BUILT_IN_CALLOC;
    case BUILT_IN_CLASSIFY_TYPE: return GSBI_BUILT_IN_CLASSIFY_TYPE;
    case BUILT_IN_CLZ: return GSBI_BUILT_IN_CLZ;
    case BUILT_IN_CLZIMAX: return GSBI_BUILT_IN_CLZIMAX;
    case BUILT_IN_CLZL: return GSBI_BUILT_IN_CLZL;
    case BUILT_IN_CLZLL: return GSBI_BUILT_IN_CLZLL;
    case BUILT_IN_CONSTANT_P: return GSBI_BUILT_IN_CONSTANT_P;
    case BUILT_IN_CTZ: return GSBI_BUILT_IN_CTZ;
    case BUILT_IN_CTZIMAX: return GSBI_BUILT_IN_CTZIMAX;
    case BUILT_IN_CTZL: return GSBI_BUILT_IN_CTZL;
    case BUILT_IN_CTZLL: return GSBI_BUILT_IN_CTZLL;
    case BUILT_IN_DCGETTEXT: return GSBI_BUILT_IN_DCGETTEXT;
    case BUILT_IN_DGETTEXT: return GSBI_BUILT_IN_DGETTEXT;
    case BUILT_IN_DWARF_CFA: return GSBI_BUILT_IN_DWARF_CFA;
    case BUILT_IN_DWARF_SP_COLUMN: return GSBI_BUILT_IN_DWARF_SP_COLUMN;
    case BUILT_IN_EH_RETURN: return GSBI_BUILT_IN_EH_RETURN;
    case BUILT_IN_EH_RETURN_DATA_REGNO: return GSBI_BUILT_IN_EH_RETURN_DATA_REGNO;
    case BUILT_IN_EXECL: return GSBI_BUILT_IN_EXECL;
    case BUILT_IN_EXECLP: return GSBI_BUILT_IN_EXECLP;
    case BUILT_IN_EXECLE: return GSBI_BUILT_IN_EXECLE;
    case BUILT_IN_EXECV: return GSBI_BUILT_IN_EXECV;
    case BUILT_IN_EXECVP: return GSBI_BUILT_IN_EXECVP;
    case BUILT_IN_EXECVE: return GSBI_BUILT_IN_EXECVE;
    case BUILT_IN_EXIT: return GSBI_BUILT_IN_EXIT;
    case BUILT_IN_EXPECT: return GSBI_BUILT_IN_EXPECT;
    case BUILT_IN_EXTEND_POINTER: return GSBI_BUILT_IN_EXTEND_POINTER;
    case BUILT_IN_EXTRACT_RETURN_ADDR: return GSBI_BUILT_IN_EXTRACT_RETURN_ADDR;
    case BUILT_IN_FFS: return GSBI_BUILT_IN_FFS;
    case BUILT_IN_FFSIMAX: return GSBI_BUILT_IN_FFSIMAX;
    case BUILT_IN_FFSL: return GSBI_BUILT_IN_FFSL;
    case BUILT_IN_FFSLL: return GSBI_BUILT_IN_FFSLL;
    case BUILT_IN_FORK: return GSBI_BUILT_IN_FORK;
    case BUILT_IN_FRAME_ADDRESS: return GSBI_BUILT_IN_FRAME_ADDRESS;
    case BUILT_IN_FROB_RETURN_ADDR: return GSBI_BUILT_IN_FROB_RETURN_ADDR;
    case BUILT_IN_GETTEXT: return GSBI_BUILT_IN_GETTEXT;
    case BUILT_IN_IMAXABS: return GSBI_BUILT_IN_IMAXABS;
    case BUILT_IN_INIT_DWARF_REG_SIZES: return GSBI_BUILT_IN_INIT_DWARF_REG_SIZES;
    case BUILT_IN_FINITE: return GSBI_BUILT_IN_FINITE;
    case BUILT_IN_FINITEF: return GSBI_BUILT_IN_FINITEF;
    case BUILT_IN_FINITEL: return GSBI_BUILT_IN_FINITEL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_FINITED32: return GSBI_BUILT_IN_FINITED32;
    case BUILT_IN_FINITED64: return GSBI_BUILT_IN_FINITED64;
    case BUILT_IN_FINITED128: return GSBI_BUILT_IN_FINITED128;
#endif
    case BUILT_IN_ISINF: return GSBI_BUILT_IN_ISINF;
    case BUILT_IN_ISINFF: return GSBI_BUILT_IN_ISINFF;
    case BUILT_IN_ISINFL: return GSBI_BUILT_IN_ISINFL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_ISINFD32: return GSBI_BUILT_IN_ISINFD32;
    case BUILT_IN_ISINFD64: return GSBI_BUILT_IN_ISINFD64;
    case BUILT_IN_ISINFD128: return GSBI_BUILT_IN_ISINFD128;
#endif
    case BUILT_IN_ISNAN: return GSBI_BUILT_IN_ISNAN;
    case BUILT_IN_ISNANF: return GSBI_BUILT_IN_ISNANF;
    case BUILT_IN_ISNANL: return GSBI_BUILT_IN_ISNANL;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_ISNAND32: return GSBI_BUILT_IN_ISNAND32;
    case BUILT_IN_ISNAND64: return GSBI_BUILT_IN_ISNAND64;
    case BUILT_IN_ISNAND128: return GSBI_BUILT_IN_ISNAND128;
#endif
    case BUILT_IN_ISGREATER: return GSBI_BUILT_IN_ISGREATER;
    case BUILT_IN_ISGREATEREQUAL: return GSBI_BUILT_IN_ISGREATEREQUAL;
    case BUILT_IN_ISLESS: return GSBI_BUILT_IN_ISLESS;
    case BUILT_IN_ISLESSEQUAL: return GSBI_BUILT_IN_ISLESSEQUAL;
    case BUILT_IN_ISLESSGREATER: return GSBI_BUILT_IN_ISLESSGREATER;
    case BUILT_IN_ISUNORDERED: return GSBI_BUILT_IN_ISUNORDERED;
    case BUILT_IN_LABS: return GSBI_BUILT_IN_LABS;
    case BUILT_IN_LLABS: return GSBI_BUILT_IN_LLABS;
    case BUILT_IN_LONGJMP: return GSBI_BUILT_IN_LONGJMP;
    case BUILT_IN_MALLOC: return GSBI_BUILT_IN_MALLOC;
    case BUILT_IN_NEXT_ARG: return GSBI_BUILT_IN_NEXT_ARG;
    case BUILT_IN_PARITY: return GSBI_BUILT_IN_PARITY;
    case BUILT_IN_PARITYIMAX: return GSBI_BUILT_IN_PARITYIMAX;
    case BUILT_IN_PARITYL: return GSBI_BUILT_IN_PARITYL;
    case BUILT_IN_PARITYLL: return GSBI_BUILT_IN_PARITYLL;
    case BUILT_IN_POPCOUNT: return GSBI_BUILT_IN_POPCOUNT;
    case BUILT_IN_POPCOUNTIMAX: return GSBI_BUILT_IN_POPCOUNTIMAX;
    case BUILT_IN_POPCOUNTL: return GSBI_BUILT_IN_POPCOUNTL;
    case BUILT_IN_POPCOUNTLL: return GSBI_BUILT_IN_POPCOUNTLL;
    case BUILT_IN_PREFETCH: return GSBI_BUILT_IN_PREFETCH;
    case BUILT_IN_RETURN: return GSBI_BUILT_IN_RETURN;
    case BUILT_IN_RETURN_ADDRESS: return GSBI_BUILT_IN_RETURN_ADDRESS;
    case BUILT_IN_SAVEREGS: return GSBI_BUILT_IN_SAVEREGS;
    case BUILT_IN_SETJMP: return GSBI_BUILT_IN_SETJMP;
    case BUILT_IN_STDARG_START: return GSBI_BUILT_IN_STDARG_START;
    case BUILT_IN_STRFMON: return GSBI_BUILT_IN_STRFMON;
    case BUILT_IN_STRFTIME: return GSBI_BUILT_IN_STRFTIME;
    case BUILT_IN_TRAP: return GSBI_BUILT_IN_TRAP;
    case BUILT_IN_UNWIND_INIT: return GSBI_BUILT_IN_UNWIND_INIT;
    case BUILT_IN_UPDATE_SETJMP_BUF: return GSBI_BUILT_IN_UPDATE_SETJMP_BUF;
    case BUILT_IN_VA_COPY: return GSBI_BUILT_IN_VA_COPY;
    case BUILT_IN_VA_END: return GSBI_BUILT_IN_VA_END;
    case BUILT_IN_VA_START: return GSBI_BUILT_IN_VA_START;
    case BUILT_IN__EXIT: return GSBI_BUILT_IN__EXIT;
    case BUILT_IN__EXIT2: return GSBI_BUILT_IN__EXIT2;
    case BUILT_IN_INIT_TRAMPOLINE: return GSBI_BUILT_IN_INIT_TRAMPOLINE;
    case BUILT_IN_ADJUST_TRAMPOLINE: return GSBI_BUILT_IN_ADJUST_TRAMPOLINE;
    case BUILT_IN_NONLOCAL_GOTO: return GSBI_BUILT_IN_NONLOCAL_GOTO;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_SETJMP_SETUP: return GSBI_BUILT_IN_SETJMP_SETUP;
    case BUILT_IN_SETJMP_DISPATCHER: return GSBI_BUILT_IN_SETJMP_DISPATCHER;
    case BUILT_IN_SETJMP_RECEIVER: return GSBI_BUILT_IN_SETJMP_RECEIVER;
#endif
    case BUILT_IN_STACK_SAVE: return GSBI_BUILT_IN_STACK_SAVE;
    case BUILT_IN_STACK_RESTORE: return GSBI_BUILT_IN_STACK_RESTORE;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_OBJECT_SIZE: return GSBI_BUILT_IN_OBJECT_SIZE;
    case BUILT_IN_MEMCPY_CHK: return GSBI_BUILT_IN_MEMCPY_CHK;
    case BUILT_IN_MEMMOVE_CHK: return GSBI_BUILT_IN_MEMMOVE_CHK;
    case BUILT_IN_MEMPCPY_CHK: return GSBI_BUILT_IN_MEMPCPY_CHK;
    case BUILT_IN_MEMSET_CHK: return GSBI_BUILT_IN_MEMSET_CHK;
    case BUILT_IN_STPCPY_CHK: return GSBI_BUILT_IN_STPCPY_CHK;
    case BUILT_IN_STRCAT_CHK: return GSBI_BUILT_IN_STRCAT_CHK;
    case BUILT_IN_STRCPY_CHK: return GSBI_BUILT_IN_STRCPY_CHK;
    case BUILT_IN_STRNCAT_CHK: return GSBI_BUILT_IN_STRNCAT_CHK;
    case BUILT_IN_STRNCPY_CHK: return GSBI_BUILT_IN_STRNCPY_CHK;
    case BUILT_IN_SNPRINTF_CHK: return GSBI_BUILT_IN_SNPRINTF_CHK;
    case BUILT_IN_SPRINTF_CHK: return GSBI_BUILT_IN_SPRINTF_CHK;
    case BUILT_IN_VSNPRINTF_CHK: return GSBI_BUILT_IN_VSNPRINTF_CHK;
    case BUILT_IN_VSPRINTF_CHK: return GSBI_BUILT_IN_VSPRINTF_CHK;
    case BUILT_IN_FPRINTF_CHK: return GSBI_BUILT_IN_FPRINTF_CHK;
    case BUILT_IN_PRINTF_CHK: return GSBI_BUILT_IN_PRINTF_CHK;
    case BUILT_IN_VFPRINTF_CHK: return GSBI_BUILT_IN_VFPRINTF_CHK;
    case BUILT_IN_VPRINTF_CHK: return GSBI_BUILT_IN_VPRINTF_CHK;
#endif
    case BUILT_IN_PROFILE_FUNC_ENTER: return GSBI_BUILT_IN_PROFILE_FUNC_ENTER;
    case BUILT_IN_PROFILE_FUNC_EXIT: return GSBI_BUILT_IN_PROFILE_FUNC_EXIT;
#ifdef FE_GNU_4_2_0
    case BUILT_IN_FETCH_AND_ADD_N: return GSBI_BUILT_IN_FETCH_AND_ADD_N;
    case BUILT_IN_FETCH_AND_ADD_1: return GSBI_BUILT_IN_FETCH_AND_ADD_1;
    case BUILT_IN_FETCH_AND_ADD_2: return GSBI_BUILT_IN_FETCH_AND_ADD_2;
    case BUILT_IN_FETCH_AND_ADD_4: return GSBI_BUILT_IN_FETCH_AND_ADD_4;
    case BUILT_IN_FETCH_AND_ADD_8: return GSBI_BUILT_IN_FETCH_AND_ADD_8;
    case BUILT_IN_FETCH_AND_ADD_16: return GSBI_BUILT_IN_FETCH_AND_ADD_16;
    case BUILT_IN_FETCH_AND_SUB_N: return GSBI_BUILT_IN_FETCH_AND_SUB_N;
    case BUILT_IN_FETCH_AND_SUB_1: return GSBI_BUILT_IN_FETCH_AND_SUB_1;
    case BUILT_IN_FETCH_AND_SUB_2: return GSBI_BUILT_IN_FETCH_AND_SUB_2;
    case BUILT_IN_FETCH_AND_SUB_4: return GSBI_BUILT_IN_FETCH_AND_SUB_4;
    case BUILT_IN_FETCH_AND_SUB_8: return GSBI_BUILT_IN_FETCH_AND_SUB_8;
    case BUILT_IN_FETCH_AND_SUB_16: return GSBI_BUILT_IN_FETCH_AND_SUB_16;
    case BUILT_IN_FETCH_AND_OR_N: return GSBI_BUILT_IN_FETCH_AND_OR_N;
    case BUILT_IN_FETCH_AND_OR_1: return GSBI_BUILT_IN_FETCH_AND_OR_1;
    case BUILT_IN_FETCH_AND_OR_2: return GSBI_BUILT_IN_FETCH_AND_OR_2;
    case BUILT_IN_FETCH_AND_OR_4: return GSBI_BUILT_IN_FETCH_AND_OR_4;
    case BUILT_IN_FETCH_AND_OR_8: return GSBI_BUILT_IN_FETCH_AND_OR_8;
    case BUILT_IN_FETCH_AND_OR_16: return GSBI_BUILT_IN_FETCH_AND_OR_16;
    case BUILT_IN_FETCH_AND_AND_N: return GSBI_BUILT_IN_FETCH_AND_AND_N;
    case BUILT_IN_FETCH_AND_AND_1: return GSBI_BUILT_IN_FETCH_AND_AND_1;
    case BUILT_IN_FETCH_AND_AND_2: return GSBI_BUILT_IN_FETCH_AND_AND_2;
    case BUILT_IN_FETCH_AND_AND_4: return GSBI_BUILT_IN_FETCH_AND_AND_4;
    case BUILT_IN_FETCH_AND_AND_8: return GSBI_BUILT_IN_FETCH_AND_AND_8;
    case BUILT_IN_FETCH_AND_AND_16: return GSBI_BUILT_IN_FETCH_AND_AND_16;
    case BUILT_IN_FETCH_AND_XOR_N: return GSBI_BUILT_IN_FETCH_AND_XOR_N;
    case BUILT_IN_FETCH_AND_XOR_1: return GSBI_BUILT_IN_FETCH_AND_XOR_1;
    case BUILT_IN_FETCH_AND_XOR_2: return GSBI_BUILT_IN_FETCH_AND_XOR_2;
    case BUILT_IN_FETCH_AND_XOR_4: return GSBI_BUILT_IN_FETCH_AND_XOR_4;
    case BUILT_IN_FETCH_AND_XOR_8: return GSBI_BUILT_IN_FETCH_AND_XOR_8;
    case BUILT_IN_FETCH_AND_XOR_16: return GSBI_BUILT_IN_FETCH_AND_XOR_16;
    case BUILT_IN_FETCH_AND_NAND_N: return GSBI_BUILT_IN_FETCH_AND_NAND_N;
    case BUILT_IN_FETCH_AND_NAND_1: return GSBI_BUILT_IN_FETCH_AND_NAND_1;
    case BUILT_IN_FETCH_AND_NAND_2: return GSBI_BUILT_IN_FETCH_AND_NAND_2;
    case BUILT_IN_FETCH_AND_NAND_4: return GSBI_BUILT_IN_FETCH_AND_NAND_4;
    case BUILT_IN_FETCH_AND_NAND_8: return GSBI_BUILT_IN_FETCH_AND_NAND_8;
    case BUILT_IN_FETCH_AND_NAND_16: return GSBI_BUILT_IN_FETCH_AND_NAND_16;
    case BUILT_IN_ADD_AND_FETCH_N: return GSBI_BUILT_IN_ADD_AND_FETCH_N;
    case BUILT_IN_ADD_AND_FETCH_1: return GSBI_BUILT_IN_ADD_AND_FETCH_1;
    case BUILT_IN_ADD_AND_FETCH_2: return GSBI_BUILT_IN_ADD_AND_FETCH_2;
    case BUILT_IN_ADD_AND_FETCH_4: return GSBI_BUILT_IN_ADD_AND_FETCH_4;
    case BUILT_IN_ADD_AND_FETCH_8: return GSBI_BUILT_IN_ADD_AND_FETCH_8;
    case BUILT_IN_ADD_AND_FETCH_16: return GSBI_BUILT_IN_ADD_AND_FETCH_16;
    case BUILT_IN_SUB_AND_FETCH_N: return GSBI_BUILT_IN_SUB_AND_FETCH_N;
    case BUILT_IN_SUB_AND_FETCH_1: return GSBI_BUILT_IN_SUB_AND_FETCH_1;
    case BUILT_IN_SUB_AND_FETCH_2: return GSBI_BUILT_IN_SUB_AND_FETCH_2;
    case BUILT_IN_SUB_AND_FETCH_4: return GSBI_BUILT_IN_SUB_AND_FETCH_4;
    case BUILT_IN_SUB_AND_FETCH_8: return GSBI_BUILT_IN_SUB_AND_FETCH_8;
    case BUILT_IN_SUB_AND_FETCH_16: return GSBI_BUILT_IN_SUB_AND_FETCH_16;
    case BUILT_IN_OR_AND_FETCH_N: return GSBI_BUILT_IN_OR_AND_FETCH_N;
    case BUILT_IN_OR_AND_FETCH_1: return GSBI_BUILT_IN_OR_AND_FETCH_1;
    case BUILT_IN_OR_AND_FETCH_2: return GSBI_BUILT_IN_OR_AND_FETCH_2;
    case BUILT_IN_OR_AND_FETCH_4: return GSBI_BUILT_IN_OR_AND_FETCH_4;
    case BUILT_IN_OR_AND_FETCH_8: return GSBI_BUILT_IN_OR_AND_FETCH_8;
    case BUILT_IN_OR_AND_FETCH_16: return GSBI_BUILT_IN_OR_AND_FETCH_16;
    case BUILT_IN_AND_AND_FETCH_N: return GSBI_BUILT_IN_AND_AND_FETCH_N;
    case BUILT_IN_AND_AND_FETCH_1: return GSBI_BUILT_IN_AND_AND_FETCH_1;
    case BUILT_IN_AND_AND_FETCH_2: return GSBI_BUILT_IN_AND_AND_FETCH_2;
    case BUILT_IN_AND_AND_FETCH_4: return GSBI_BUILT_IN_AND_AND_FETCH_4;
    case BUILT_IN_AND_AND_FETCH_8: return GSBI_BUILT_IN_AND_AND_FETCH_8;
    case BUILT_IN_AND_AND_FETCH_16: return GSBI_BUILT_IN_AND_AND_FETCH_16;
    case BUILT_IN_XOR_AND_FETCH_N: return GSBI_BUILT_IN_XOR_AND_FETCH_N;
    case BUILT_IN_XOR_AND_FETCH_1: return GSBI_BUILT_IN_XOR_AND_FETCH_1;
    case BUILT_IN_XOR_AND_FETCH_2: return GSBI_BUILT_IN_XOR_AND_FETCH_2;
    case BUILT_IN_XOR_AND_FETCH_4: return GSBI_BUILT_IN_XOR_AND_FETCH_4;
    case BUILT_IN_XOR_AND_FETCH_8: return GSBI_BUILT_IN_XOR_AND_FETCH_8;
    case BUILT_IN_XOR_AND_FETCH_16: return GSBI_BUILT_IN_XOR_AND_FETCH_16;
    case BUILT_IN_NAND_AND_FETCH_N: return GSBI_BUILT_IN_NAND_AND_FETCH_N;
    case BUILT_IN_NAND_AND_FETCH_1: return GSBI_BUILT_IN_NAND_AND_FETCH_1;
    case BUILT_IN_NAND_AND_FETCH_2: return GSBI_BUILT_IN_NAND_AND_FETCH_2;
    case BUILT_IN_NAND_AND_FETCH_4: return GSBI_BUILT_IN_NAND_AND_FETCH_4;
    case BUILT_IN_NAND_AND_FETCH_8: return GSBI_BUILT_IN_NAND_AND_FETCH_8;
    case BUILT_IN_NAND_AND_FETCH_16: return GSBI_BUILT_IN_NAND_AND_FETCH_16;
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_N: return GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_N;
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_1: return GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_1;
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_2: return GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_2;
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_4: return GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_4;
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_8: return GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_8;
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_16: return GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_16;
    case BUILT_IN_VAL_COMPARE_AND_SWAP_N: return GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_N;
    case BUILT_IN_VAL_COMPARE_AND_SWAP_1: return GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_1;
    case BUILT_IN_VAL_COMPARE_AND_SWAP_2: return GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_2;
    case BUILT_IN_VAL_COMPARE_AND_SWAP_4: return GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_4;
    case BUILT_IN_VAL_COMPARE_AND_SWAP_8: return GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_8;
    case BUILT_IN_VAL_COMPARE_AND_SWAP_16: return GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_16;
    case BUILT_IN_LOCK_TEST_AND_SET_N: return GSBI_BUILT_IN_LOCK_TEST_AND_SET_N;
    case BUILT_IN_LOCK_TEST_AND_SET_1: return GSBI_BUILT_IN_LOCK_TEST_AND_SET_1;
    case BUILT_IN_LOCK_TEST_AND_SET_2: return GSBI_BUILT_IN_LOCK_TEST_AND_SET_2;
    case BUILT_IN_LOCK_TEST_AND_SET_4: return GSBI_BUILT_IN_LOCK_TEST_AND_SET_4;
    case BUILT_IN_LOCK_TEST_AND_SET_8: return GSBI_BUILT_IN_LOCK_TEST_AND_SET_8;
    case BUILT_IN_LOCK_TEST_AND_SET_16: return GSBI_BUILT_IN_LOCK_TEST_AND_SET_16;
    case BUILT_IN_LOCK_RELEASE_N: return GSBI_BUILT_IN_LOCK_RELEASE_N;
    case BUILT_IN_LOCK_RELEASE_1: return GSBI_BUILT_IN_LOCK_RELEASE_1;
    case BUILT_IN_LOCK_RELEASE_2: return GSBI_BUILT_IN_LOCK_RELEASE_2;
    case BUILT_IN_LOCK_RELEASE_4: return GSBI_BUILT_IN_LOCK_RELEASE_4;
    case BUILT_IN_LOCK_RELEASE_8: return GSBI_BUILT_IN_LOCK_RELEASE_8;
    case BUILT_IN_LOCK_RELEASE_16: return GSBI_BUILT_IN_LOCK_RELEASE_16;
    case BUILT_IN_SYNCHRONIZE: return GSBI_BUILT_IN_SYNCHRONIZE;
    case BUILT_IN_OMP_GET_THREAD_NUM: return GSBI_BUILT_IN_OMP_GET_THREAD_NUM;
    case BUILT_IN_OMP_GET_NUM_THREADS: return GSBI_BUILT_IN_OMP_GET_NUM_THREADS;
    case BUILT_IN_GOMP_ATOMIC_START: return GSBI_BUILT_IN_GOMP_ATOMIC_START;
    case BUILT_IN_GOMP_ATOMIC_END: return GSBI_BUILT_IN_GOMP_ATOMIC_END;
    case BUILT_IN_GOMP_BARRIER: return GSBI_BUILT_IN_GOMP_BARRIER;
    case BUILT_IN_GOMP_CRITICAL_START: return GSBI_BUILT_IN_GOMP_CRITICAL_START;
    case BUILT_IN_GOMP_CRITICAL_END: return GSBI_BUILT_IN_GOMP_CRITICAL_END;
    case BUILT_IN_GOMP_CRITICAL_NAME_START: return GSBI_BUILT_IN_GOMP_CRITICAL_NAME_START;
    case BUILT_IN_GOMP_CRITICAL_NAME_END: return GSBI_BUILT_IN_GOMP_CRITICAL_NAME_END;
    case BUILT_IN_GOMP_LOOP_STATIC_START: return GSBI_BUILT_IN_GOMP_LOOP_STATIC_START;
    case BUILT_IN_GOMP_LOOP_DYNAMIC_START: return GSBI_BUILT_IN_GOMP_LOOP_DYNAMIC_START;
    case BUILT_IN_GOMP_LOOP_GUIDED_START: return GSBI_BUILT_IN_GOMP_LOOP_GUIDED_START;
    case BUILT_IN_GOMP_LOOP_RUNTIME_START: return GSBI_BUILT_IN_GOMP_LOOP_RUNTIME_START;
    case BUILT_IN_GOMP_LOOP_ORDERED_STATIC_START: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_STATIC_START;
    case BUILT_IN_GOMP_LOOP_ORDERED_DYNAMIC_START: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_DYNAMIC_START;
    case BUILT_IN_GOMP_LOOP_ORDERED_GUIDED_START: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_GUIDED_START;
    case BUILT_IN_GOMP_LOOP_ORDERED_RUNTIME_START: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_RUNTIME_START;
    case BUILT_IN_GOMP_LOOP_STATIC_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_STATIC_NEXT;
    case BUILT_IN_GOMP_LOOP_DYNAMIC_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_DYNAMIC_NEXT;
    case BUILT_IN_GOMP_LOOP_GUIDED_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_GUIDED_NEXT;
    case BUILT_IN_GOMP_LOOP_RUNTIME_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_RUNTIME_NEXT;
    case BUILT_IN_GOMP_LOOP_ORDERED_STATIC_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_STATIC_NEXT;
    case BUILT_IN_GOMP_LOOP_ORDERED_DYNAMIC_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_DYNAMIC_NEXT;
    case BUILT_IN_GOMP_LOOP_ORDERED_GUIDED_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_GUIDED_NEXT;
    case BUILT_IN_GOMP_LOOP_ORDERED_RUNTIME_NEXT: return GSBI_BUILT_IN_GOMP_LOOP_ORDERED_RUNTIME_NEXT;
    case BUILT_IN_GOMP_PARALLEL_LOOP_STATIC_START: return GSBI_BUILT_IN_GOMP_PARALLEL_LOOP_STATIC_START;
    case BUILT_IN_GOMP_PARALLEL_LOOP_DYNAMIC_START: return GSBI_BUILT_IN_GOMP_PARALLEL_LOOP_DYNAMIC_START;
    case BUILT_IN_GOMP_PARALLEL_LOOP_GUIDED_START: return GSBI_BUILT_IN_GOMP_PARALLEL_LOOP_GUIDED_START;
    case BUILT_IN_GOMP_PARALLEL_LOOP_RUNTIME_START: return GSBI_BUILT_IN_GOMP_PARALLEL_LOOP_RUNTIME_START;
    case BUILT_IN_GOMP_LOOP_END: return GSBI_BUILT_IN_GOMP_LOOP_END;
    case BUILT_IN_GOMP_LOOP_END_NOWAIT: return GSBI_BUILT_IN_GOMP_LOOP_END_NOWAIT;
    case BUILT_IN_GOMP_ORDERED_START: return GSBI_BUILT_IN_GOMP_ORDERED_START;
    case BUILT_IN_GOMP_ORDERED_END: return GSBI_BUILT_IN_GOMP_ORDERED_END;
    case BUILT_IN_GOMP_PARALLEL_START: return GSBI_BUILT_IN_GOMP_PARALLEL_START;
    case BUILT_IN_GOMP_PARALLEL_END: return GSBI_BUILT_IN_GOMP_PARALLEL_END;
    case BUILT_IN_GOMP_SECTIONS_START: return GSBI_BUILT_IN_GOMP_SECTIONS_START;
    case BUILT_IN_GOMP_SECTIONS_NEXT: return GSBI_BUILT_IN_GOMP_SECTIONS_NEXT;
    case BUILT_IN_GOMP_PARALLEL_SECTIONS_START: return GSBI_BUILT_IN_GOMP_PARALLEL_SECTIONS_START;
    case BUILT_IN_GOMP_SECTIONS_END: return GSBI_BUILT_IN_GOMP_SECTIONS_END;
    case BUILT_IN_GOMP_SECTIONS_END_NOWAIT: return GSBI_BUILT_IN_GOMP_SECTIONS_END_NOWAIT;
    case BUILT_IN_GOMP_SINGLE_START: return GSBI_BUILT_IN_GOMP_SINGLE_START;
    case BUILT_IN_GOMP_SINGLE_COPY_START: return GSBI_BUILT_IN_GOMP_SINGLE_COPY_START;
    case BUILT_IN_GOMP_SINGLE_COPY_END: return GSBI_BUILT_IN_GOMP_SINGLE_COPY_END;
#endif

    case BUILT_IN_COMPLEX_MUL_MIN: return GSBI_BUILT_IN_COMPLEX_MUL_MIN;
    case BUILT_IN_COMPLEX_MUL_MAX: return GSBI_BUILT_IN_COMPLEX_MUL_MAX;
    case BUILT_IN_COMPLEX_DIV_MIN: return GSBI_BUILT_IN_COMPLEX_DIV_MIN;
    case BUILT_IN_COMPLEX_DIV_MAX: return GSBI_BUILT_IN_COMPLEX_DIV_MAX;
#ifdef TARG_SL
    case BUILT_IN_CVT64_HIGH:          return GSBI_BUILT_IN_CVT64_HIGH;
    case BUILT_IN_CVT64_LOW:           return GSBI_BUILT_IN_CVT64_LOW;
    case BUILT_IN_CVT32:               return GSBI_BUILT_IN_CVT32;
    case BUILT_IN_LONGLONG_CVT64_HIGH: return GSBI_BUILT_IN_LONGLONG_CVT64_HIGH;
    case BUILT_IN_LONGLONG_CVT64_LOW:  return GSBI_BUILT_IN_LONGLONG_CVT64_LOW;
    case BUILT_IN_C3AADDA:  return GSBI_BUILT_IN_C3AADDA;
    case BUILT_IN_C3NEGA:   return GSBI_BUILT_IN_C3NEGA;
    case BUILT_IN_C3BITR:   return GSBI_BUILT_IN_C3BITR;
    case BUILT_IN_C3CS:     return GSBI_BUILT_IN_C3CS;
    case BUILT_IN_C3DADD:   return GSBI_BUILT_IN_C3DADD;
    case BUILT_IN_C3DMAC:   return GSBI_BUILT_IN_C3DMAC;
    case BUILT_IN_C3DMACA:  return GSBI_BUILT_IN_C3DMACA;
    case BUILT_IN_C3DMULA:  return GSBI_BUILT_IN_C3DMULA;
    case BUILT_IN_C3DMULAA: return GSBI_BUILT_IN_C3DMULAA;
    case BUILT_IN_C3DSHLLI: return GSBI_BUILT_IN_C3DSHLLI;
    case BUILT_IN_C3FFE:    return GSBI_BUILT_IN_C3FFE;
    case BUILT_IN_C3LD:     return GSBI_BUILT_IN_C3LD;
    case BUILT_IN_C3ST:     return GSBI_BUILT_IN_C3ST;
    case BUILT_IN_C3LEAD:   return GSBI_BUILT_IN_C3LEAD;
    case BUILT_IN_C3MAC:    return GSBI_BUILT_IN_C3MAC;
    case BUILT_IN_C3MACA:   return GSBI_BUILT_IN_C3MACA;
    case BUILT_IN_C3MACAR:  return GSBI_BUILT_IN_C3MACAR;
    case BUILT_IN_C3MACI:   return GSBI_BUILT_IN_C3MACI;
    case BUILT_IN_C3MULA:   return GSBI_BUILT_IN_C3MULA;
    case BUILT_IN_C3MULAA:  return GSBI_BUILT_IN_C3MULAA;
    case BUILT_IN_C3MULAAR: return GSBI_BUILT_IN_C3MULAAR;
    case BUILT_IN_C3MULAI:  return GSBI_BUILT_IN_C3MULAI;
    case BUILT_IN_C3MULS:   return GSBI_BUILT_IN_C3MULS;
    case BUILT_IN_C3MULUS:  return GSBI_BUILT_IN_C3MULUS;
    case BUILT_IN_C3REVB:   return GSBI_BUILT_IN_C3REVB;
    case BUILT_IN_C3ROUND:  return GSBI_BUILT_IN_C3ROUND;
    case BUILT_IN_C3SAADDA: return GSBI_BUILT_IN_C3SAADDA;
    case BUILT_IN_C3SAADDHA: return GSBI_BUILT_IN_C3SAADDHA;
    case BUILT_IN_C3SAADDS:  return GSBI_BUILT_IN_C3SAADDS;
    case BUILT_IN_C3SAADDSH: return GSBI_BUILT_IN_C3SAADDSH;
    case BUILT_IN_C3SADDA:   return GSBI_BUILT_IN_C3SADDA;
    case BUILT_IN_C3SADDAA:  return GSBI_BUILT_IN_C3SADDAA;
    case BUILT_IN_C3SAMULHA: return GSBI_BUILT_IN_C3SAMULHA;
    case BUILT_IN_C3SAMULSH: return GSBI_BUILT_IN_C3SAMULSH;
    case BUILT_IN_C3SHAV:    return GSBI_BUILT_IN_C3SHAV;
    case BUILT_IN_C3SHLAFAI: return GSBI_BUILT_IN_C3SHLAFAI;
    case BUILT_IN_C3SHLATAI: return GSBI_BUILT_IN_C3SHLATAI;
    case BUILT_IN_C3SHLAI:   return GSBI_BUILT_IN_C3SHLAI;
    case BUILT_IN_C3SUBC:    return GSBI_BUILT_IN_C3SUBC;
    case BUILT_IN_INIT_HI:   return GSBI_BUILT_IN_INIT_HI;
    case BUILT_IN_COPY_HI:   return GSBI_BUILT_IN_COPY_HI;
    case BUILT_IN_C3_INIT_ACC: return GSBI_BUILT_IN_C3_INIT_ACC;
    case BUILT_IN_C3_SAVE_ACC: return GSBI_BUILT_IN_C3_SAVE_ACC;
    case BUILT_IN_C3_INIT_DACC: return GSBI_BUILT_IN_C3_INIT_DACC;
    case BUILT_IN_C3_SAVE_DACC: return GSBI_BUILT_IN_C3_SAVE_DACC;
    case BUILT_IN_C3_INIT_ADDR: return GSBI_BUILT_IN_C3_INIT_ADDR;
    case BUILT_IN_C3_SAVE_ADDR: return GSBI_BUILT_IN_C3_SAVE_ADDR;
    case BUILT_IN_C3_MVFS:      return GSBI_BUILT_IN_C3_MVFS;
    case BUILT_IN_SET_ADDR:     return GSBI_BUILT_IN_SET_ADDR;
    case BUILT_IN_SET_CIRCBUF:  return GSBI_BUILT_IN_SET_CIRCBUF;
#endif
    case END_BUILTINS: return GSBI_END_BUILTINS;
  }
  gcc_assert (0);
  return (gsbi_t) 0;
}

static inline gsbi_class_t
gcc_built_in_class2gsbi_class (unsigned char class) 
{
 switch (class) {
   case 0: return GSBI_CLASS_NOT_BUILT_IN;
   case 1: return GSBI_CLASS_BUILT_IN_FRONTEND;
   case 2: return GSBI_CLASS_BUILT_IN_MD;
   case 3: return GSBI_CLASS_BUILT_IN_NORMAL;
 }
 gcc_assert (0);
 return (gsbi_class_t) 0;
}

#ifdef TARG_X8664
static inline gsbi_ts_t
ix86_builtins2gsbi_ts (enum ix86_builtins code) 
{
  switch (code) {
    case IX86_BUILTIN_ADDPS: return GSBI_IX86_BUILTIN_ADDPS;
    case IX86_BUILTIN_ADDSS: return GSBI_IX86_BUILTIN_ADDSS;
    case IX86_BUILTIN_DIVPS: return GSBI_IX86_BUILTIN_DIVPS;
    case IX86_BUILTIN_DIVSS: return GSBI_IX86_BUILTIN_DIVSS;
    case IX86_BUILTIN_MULPS: return GSBI_IX86_BUILTIN_MULPS;
    case IX86_BUILTIN_MULSS: return GSBI_IX86_BUILTIN_MULSS;
    case IX86_BUILTIN_SUBPS: return GSBI_IX86_BUILTIN_SUBPS;
    case IX86_BUILTIN_SUBSS: return GSBI_IX86_BUILTIN_SUBSS;
    case IX86_BUILTIN_CMPEQPS: return GSBI_IX86_BUILTIN_CMPEQPS;
    case IX86_BUILTIN_CMPLTPS: return GSBI_IX86_BUILTIN_CMPLTPS;
    case IX86_BUILTIN_CMPLEPS: return GSBI_IX86_BUILTIN_CMPLEPS;
    case IX86_BUILTIN_CMPGTPS: return GSBI_IX86_BUILTIN_CMPGTPS;
    case IX86_BUILTIN_CMPGEPS: return GSBI_IX86_BUILTIN_CMPGEPS;
    case IX86_BUILTIN_CMPNEQPS: return GSBI_IX86_BUILTIN_CMPNEQPS;
    case IX86_BUILTIN_CMPNLTPS: return GSBI_IX86_BUILTIN_CMPNLTPS;
    case IX86_BUILTIN_CMPNLEPS: return GSBI_IX86_BUILTIN_CMPNLEPS;
    case IX86_BUILTIN_CMPNGTPS: return GSBI_IX86_BUILTIN_CMPNGTPS;
    case IX86_BUILTIN_CMPNGEPS: return GSBI_IX86_BUILTIN_CMPNGEPS;
    case IX86_BUILTIN_CMPORDPS: return GSBI_IX86_BUILTIN_CMPORDPS;
    case IX86_BUILTIN_CMPUNORDPS: return GSBI_IX86_BUILTIN_CMPUNORDPS;
    case IX86_BUILTIN_CMPEQSS: return GSBI_IX86_BUILTIN_CMPEQSS;
    case IX86_BUILTIN_CMPLTSS: return GSBI_IX86_BUILTIN_CMPLTSS;
    case IX86_BUILTIN_CMPLESS: return GSBI_IX86_BUILTIN_CMPLESS;
    case IX86_BUILTIN_CMPNEQSS: return GSBI_IX86_BUILTIN_CMPNEQSS;
    case IX86_BUILTIN_CMPNLTSS: return GSBI_IX86_BUILTIN_CMPNLTSS;
    case IX86_BUILTIN_CMPNLESS: return GSBI_IX86_BUILTIN_CMPNLESS;
    case IX86_BUILTIN_CMPNGTSS: return GSBI_IX86_BUILTIN_CMPNGTSS;
    case IX86_BUILTIN_CMPNGESS: return GSBI_IX86_BUILTIN_CMPNGESS;
    case IX86_BUILTIN_CMPORDSS: return GSBI_IX86_BUILTIN_CMPORDSS;
    case IX86_BUILTIN_CMPUNORDSS: return GSBI_IX86_BUILTIN_CMPUNORDSS;
    case IX86_BUILTIN_COMIEQSS: return GSBI_IX86_BUILTIN_COMIEQSS;
    case IX86_BUILTIN_COMILTSS: return GSBI_IX86_BUILTIN_COMILTSS;
    case IX86_BUILTIN_COMILESS: return GSBI_IX86_BUILTIN_COMILESS;
    case IX86_BUILTIN_COMIGTSS: return GSBI_IX86_BUILTIN_COMIGTSS;
    case IX86_BUILTIN_COMIGESS: return GSBI_IX86_BUILTIN_COMIGESS;
    case IX86_BUILTIN_COMINEQSS: return GSBI_IX86_BUILTIN_COMINEQSS;
    case IX86_BUILTIN_UCOMIEQSS: return GSBI_IX86_BUILTIN_UCOMIEQSS;
    case IX86_BUILTIN_UCOMILTSS: return GSBI_IX86_BUILTIN_UCOMILTSS;
    case IX86_BUILTIN_UCOMILESS: return GSBI_IX86_BUILTIN_UCOMILESS;
    case IX86_BUILTIN_UCOMIGTSS: return GSBI_IX86_BUILTIN_UCOMIGTSS;
    case IX86_BUILTIN_UCOMIGESS: return GSBI_IX86_BUILTIN_UCOMIGESS;
    case IX86_BUILTIN_UCOMINEQSS: return GSBI_IX86_BUILTIN_UCOMINEQSS;
    case IX86_BUILTIN_CVTPI2PS: return GSBI_IX86_BUILTIN_CVTPI2PS;
    case IX86_BUILTIN_CVTPS2PI: return GSBI_IX86_BUILTIN_CVTPS2PI;
    case IX86_BUILTIN_CVTSI2SS: return GSBI_IX86_BUILTIN_CVTSI2SS;
    case IX86_BUILTIN_CVTSI642SS: return GSBI_IX86_BUILTIN_CVTSI642SS;
    case IX86_BUILTIN_CVTSS2SI: return GSBI_IX86_BUILTIN_CVTSS2SI;
    case IX86_BUILTIN_CVTSS2SI64: return GSBI_IX86_BUILTIN_CVTSS2SI64;
    case IX86_BUILTIN_CVTTPS2PI: return GSBI_IX86_BUILTIN_CVTTPS2PI;
    case IX86_BUILTIN_CVTTSS2SI: return GSBI_IX86_BUILTIN_CVTTSS2SI;
    case IX86_BUILTIN_CVTTSS2SI64: return GSBI_IX86_BUILTIN_CVTTSS2SI64;
    case IX86_BUILTIN_MAXPS: return GSBI_IX86_BUILTIN_MAXPS;
    case IX86_BUILTIN_MAXSS: return GSBI_IX86_BUILTIN_MAXSS;
    case IX86_BUILTIN_MINPS: return GSBI_IX86_BUILTIN_MINPS;
    case IX86_BUILTIN_MINSS: return GSBI_IX86_BUILTIN_MINSS;
    case IX86_BUILTIN_LOADUPS: return GSBI_IX86_BUILTIN_LOADUPS;
    case IX86_BUILTIN_STOREUPS: return GSBI_IX86_BUILTIN_STOREUPS;
    case IX86_BUILTIN_MOVSS: return GSBI_IX86_BUILTIN_MOVSS;
    case IX86_BUILTIN_MOVHLPS: return GSBI_IX86_BUILTIN_MOVHLPS;
    case IX86_BUILTIN_MOVLHPS: return GSBI_IX86_BUILTIN_MOVLHPS;
    case IX86_BUILTIN_LOADHPS: return GSBI_IX86_BUILTIN_LOADHPS;
    case IX86_BUILTIN_LOADLPS: return GSBI_IX86_BUILTIN_LOADLPS;
    case IX86_BUILTIN_STOREHPS: return GSBI_IX86_BUILTIN_STOREHPS;
    case IX86_BUILTIN_STORELPS: return GSBI_IX86_BUILTIN_STORELPS;
    case IX86_BUILTIN_MASKMOVQ: return GSBI_IX86_BUILTIN_MASKMOVQ;
    case IX86_BUILTIN_MOVMSKPS: return GSBI_IX86_BUILTIN_MOVMSKPS;
    case IX86_BUILTIN_PMOVMSKB: return GSBI_IX86_BUILTIN_PMOVMSKB;
    case IX86_BUILTIN_MOVNTPS: return GSBI_IX86_BUILTIN_MOVNTPS;
    case IX86_BUILTIN_MOVNTQ: return GSBI_IX86_BUILTIN_MOVNTQ;
    case IX86_BUILTIN_LOADDQU: return GSBI_IX86_BUILTIN_LOADDQU;
    case IX86_BUILTIN_STOREDQU: return GSBI_IX86_BUILTIN_STOREDQU;
    case IX86_BUILTIN_PACKSSWB: return GSBI_IX86_BUILTIN_PACKSSWB;
    case IX86_BUILTIN_PACKSSDW: return GSBI_IX86_BUILTIN_PACKSSDW;
    case IX86_BUILTIN_PACKUSWB: return GSBI_IX86_BUILTIN_PACKUSWB;
    case IX86_BUILTIN_PADDB: return GSBI_IX86_BUILTIN_PADDB;
    case IX86_BUILTIN_PADDW: return GSBI_IX86_BUILTIN_PADDW;
    case IX86_BUILTIN_PADDD: return GSBI_IX86_BUILTIN_PADDD;
    case IX86_BUILTIN_PADDQ: return GSBI_IX86_BUILTIN_PADDQ;
    case IX86_BUILTIN_PADDSB: return GSBI_IX86_BUILTIN_PADDSB;
    case IX86_BUILTIN_PADDSW: return GSBI_IX86_BUILTIN_PADDSW;
    case IX86_BUILTIN_PADDUSB: return GSBI_IX86_BUILTIN_PADDUSB;
    case IX86_BUILTIN_PADDUSW: return GSBI_IX86_BUILTIN_PADDUSW;
    case IX86_BUILTIN_PSUBB: return GSBI_IX86_BUILTIN_PSUBB;
    case IX86_BUILTIN_PSUBW: return GSBI_IX86_BUILTIN_PSUBW;
    case IX86_BUILTIN_PSUBD: return GSBI_IX86_BUILTIN_PSUBD;
    case IX86_BUILTIN_PSUBQ: return GSBI_IX86_BUILTIN_PSUBQ;
    case IX86_BUILTIN_PSUBSB: return GSBI_IX86_BUILTIN_PSUBSB;
    case IX86_BUILTIN_PSUBSW: return GSBI_IX86_BUILTIN_PSUBSW;
    case IX86_BUILTIN_PSUBUSB: return GSBI_IX86_BUILTIN_PSUBUSB;
    case IX86_BUILTIN_PSUBUSW: return GSBI_IX86_BUILTIN_PSUBUSW;
    case IX86_BUILTIN_PAND: return GSBI_IX86_BUILTIN_PAND;
    case IX86_BUILTIN_PANDN: return GSBI_IX86_BUILTIN_PANDN;
    case IX86_BUILTIN_POR: return GSBI_IX86_BUILTIN_POR;
    case IX86_BUILTIN_PXOR: return GSBI_IX86_BUILTIN_PXOR;
    case IX86_BUILTIN_PAVGB: return GSBI_IX86_BUILTIN_PAVGB;
    case IX86_BUILTIN_PAVGW: return GSBI_IX86_BUILTIN_PAVGW;
    case IX86_BUILTIN_PCMPEQB: return GSBI_IX86_BUILTIN_PCMPEQB;
    case IX86_BUILTIN_PCMPEQW: return GSBI_IX86_BUILTIN_PCMPEQW;
    case IX86_BUILTIN_PCMPEQD: return GSBI_IX86_BUILTIN_PCMPEQD;
    case IX86_BUILTIN_PCMPGTB: return GSBI_IX86_BUILTIN_PCMPGTB;
    case IX86_BUILTIN_PCMPGTW: return GSBI_IX86_BUILTIN_PCMPGTW;
    case IX86_BUILTIN_PCMPGTD: return GSBI_IX86_BUILTIN_PCMPGTD;
    case IX86_BUILTIN_PMADDWD: return GSBI_IX86_BUILTIN_PMADDWD;
    case IX86_BUILTIN_PMAXSW: return GSBI_IX86_BUILTIN_PMAXSW;
    case IX86_BUILTIN_PMAXUB: return GSBI_IX86_BUILTIN_PMAXUB;
    case IX86_BUILTIN_PMINSW: return GSBI_IX86_BUILTIN_PMINSW;
    case IX86_BUILTIN_PMINUB: return GSBI_IX86_BUILTIN_PMINUB;
    case IX86_BUILTIN_PMULHUW: return GSBI_IX86_BUILTIN_PMULHUW;
    case IX86_BUILTIN_PMULHW: return GSBI_IX86_BUILTIN_PMULHW;
    case IX86_BUILTIN_PMULLW: return GSBI_IX86_BUILTIN_PMULLW;
    case IX86_BUILTIN_PSADBW: return GSBI_IX86_BUILTIN_PSADBW;
    case IX86_BUILTIN_PSHUFW: return GSBI_IX86_BUILTIN_PSHUFW;
    case IX86_BUILTIN_PSLLW: return GSBI_IX86_BUILTIN_PSLLW;
    case IX86_BUILTIN_PSLLD: return GSBI_IX86_BUILTIN_PSLLD;
    case IX86_BUILTIN_PSLLQ: return GSBI_IX86_BUILTIN_PSLLQ;
    case IX86_BUILTIN_PSRAW: return GSBI_IX86_BUILTIN_PSRAW;
    case IX86_BUILTIN_PSRAD: return GSBI_IX86_BUILTIN_PSRAD;
    case IX86_BUILTIN_PSRLW: return GSBI_IX86_BUILTIN_PSRLW;
    case IX86_BUILTIN_PSRLD: return GSBI_IX86_BUILTIN_PSRLD;
    case IX86_BUILTIN_PSRLQ: return GSBI_IX86_BUILTIN_PSRLQ;
    case IX86_BUILTIN_PSLLWI: return GSBI_IX86_BUILTIN_PSLLWI;
    case IX86_BUILTIN_PSLLDI: return GSBI_IX86_BUILTIN_PSLLDI;
    case IX86_BUILTIN_PSLLQI: return GSBI_IX86_BUILTIN_PSLLQI;
    case IX86_BUILTIN_PSRAWI: return GSBI_IX86_BUILTIN_PSRAWI;
    case IX86_BUILTIN_PSRADI: return GSBI_IX86_BUILTIN_PSRADI;
    case IX86_BUILTIN_PSRLWI: return GSBI_IX86_BUILTIN_PSRLWI;
    case IX86_BUILTIN_PSRLDI: return GSBI_IX86_BUILTIN_PSRLDI;
    case IX86_BUILTIN_PSRLQI: return GSBI_IX86_BUILTIN_PSRLQI;
    case IX86_BUILTIN_PUNPCKHBW: return GSBI_IX86_BUILTIN_PUNPCKHBW;
    case IX86_BUILTIN_PUNPCKHWD: return GSBI_IX86_BUILTIN_PUNPCKHWD;
    case IX86_BUILTIN_PUNPCKHDQ: return GSBI_IX86_BUILTIN_PUNPCKHDQ;
    case IX86_BUILTIN_PUNPCKLBW: return GSBI_IX86_BUILTIN_PUNPCKLBW;
    case IX86_BUILTIN_PUNPCKLWD: return GSBI_IX86_BUILTIN_PUNPCKLWD;
    case IX86_BUILTIN_PUNPCKLDQ: return GSBI_IX86_BUILTIN_PUNPCKLDQ;
    case IX86_BUILTIN_SHUFPS: return GSBI_IX86_BUILTIN_SHUFPS;
    case IX86_BUILTIN_RCPPS: return GSBI_IX86_BUILTIN_RCPPS;
    case IX86_BUILTIN_RCPSS: return GSBI_IX86_BUILTIN_RCPSS;
    case IX86_BUILTIN_RSQRTPS: return GSBI_IX86_BUILTIN_RSQRTPS;
    case IX86_BUILTIN_RSQRTSS: return GSBI_IX86_BUILTIN_RSQRTSS;
    case IX86_BUILTIN_SQRTPS: return GSBI_IX86_BUILTIN_SQRTPS;
    case IX86_BUILTIN_SQRTSS: return GSBI_IX86_BUILTIN_SQRTSS;
    case IX86_BUILTIN_UNPCKHPS: return GSBI_IX86_BUILTIN_UNPCKHPS;
    case IX86_BUILTIN_UNPCKLPS: return GSBI_IX86_BUILTIN_UNPCKLPS;
    case IX86_BUILTIN_ANDPS: return GSBI_IX86_BUILTIN_ANDPS;
    case IX86_BUILTIN_ANDNPS: return GSBI_IX86_BUILTIN_ANDNPS;
    case IX86_BUILTIN_ORPS: return GSBI_IX86_BUILTIN_ORPS;
    case IX86_BUILTIN_XORPS: return GSBI_IX86_BUILTIN_XORPS;
    case IX86_BUILTIN_EMMS: return GSBI_IX86_BUILTIN_EMMS;
    case IX86_BUILTIN_LDMXCSR: return GSBI_IX86_BUILTIN_LDMXCSR;
    case IX86_BUILTIN_STMXCSR: return GSBI_IX86_BUILTIN_STMXCSR;
    case IX86_BUILTIN_SFENCE: return GSBI_IX86_BUILTIN_SFENCE;
    case IX86_BUILTIN_FEMMS: return GSBI_IX86_BUILTIN_FEMMS;
    case IX86_BUILTIN_PAVGUSB: return GSBI_IX86_BUILTIN_PAVGUSB;
    case IX86_BUILTIN_PF2ID: return GSBI_IX86_BUILTIN_PF2ID;
    case IX86_BUILTIN_PFACC: return GSBI_IX86_BUILTIN_PFACC;
    case IX86_BUILTIN_PFADD: return GSBI_IX86_BUILTIN_PFADD;
    case IX86_BUILTIN_PFCMPEQ: return GSBI_IX86_BUILTIN_PFCMPEQ;
    case IX86_BUILTIN_PFCMPGE: return GSBI_IX86_BUILTIN_PFCMPGE;
    case IX86_BUILTIN_PFCMPGT: return GSBI_IX86_BUILTIN_PFCMPGT;
    case IX86_BUILTIN_PFMAX: return GSBI_IX86_BUILTIN_PFMAX;
    case IX86_BUILTIN_PFMIN: return GSBI_IX86_BUILTIN_PFMIN;
    case IX86_BUILTIN_PFMUL: return GSBI_IX86_BUILTIN_PFMUL;
    case IX86_BUILTIN_PFRCP: return GSBI_IX86_BUILTIN_PFRCP;
    case IX86_BUILTIN_PFRCPIT1: return GSBI_IX86_BUILTIN_PFRCPIT1;
    case IX86_BUILTIN_PFRCPIT2: return GSBI_IX86_BUILTIN_PFRCPIT2;
    case IX86_BUILTIN_PFRSQIT1: return GSBI_IX86_BUILTIN_PFRSQIT1;
    case IX86_BUILTIN_PFRSQRT: return GSBI_IX86_BUILTIN_PFRSQRT;
    case IX86_BUILTIN_PFSUB: return GSBI_IX86_BUILTIN_PFSUB;
    case IX86_BUILTIN_PFSUBR: return GSBI_IX86_BUILTIN_PFSUBR;
    case IX86_BUILTIN_PI2FD: return GSBI_IX86_BUILTIN_PI2FD;
    case IX86_BUILTIN_PMULHRW: return GSBI_IX86_BUILTIN_PMULHRW;
    case IX86_BUILTIN_PF2IW: return GSBI_IX86_BUILTIN_PF2IW;
    case IX86_BUILTIN_PFNACC: return GSBI_IX86_BUILTIN_PFNACC;
    case IX86_BUILTIN_PFPNACC: return GSBI_IX86_BUILTIN_PFPNACC;
    case IX86_BUILTIN_PI2FW: return GSBI_IX86_BUILTIN_PI2FW;
    case IX86_BUILTIN_PSWAPDSI: return GSBI_IX86_BUILTIN_PSWAPDSI;
    case IX86_BUILTIN_PSWAPDSF: return GSBI_IX86_BUILTIN_PSWAPDSF;
    case IX86_BUILTIN_ADDPD: return GSBI_IX86_BUILTIN_ADDPD;
    case IX86_BUILTIN_ADDSD: return GSBI_IX86_BUILTIN_ADDSD;
    case IX86_BUILTIN_DIVPD: return GSBI_IX86_BUILTIN_DIVPD;
    case IX86_BUILTIN_DIVSD: return GSBI_IX86_BUILTIN_DIVSD;
    case IX86_BUILTIN_MULPD: return GSBI_IX86_BUILTIN_MULPD;
    case IX86_BUILTIN_MULSD: return GSBI_IX86_BUILTIN_MULSD;
    case IX86_BUILTIN_SUBPD: return GSBI_IX86_BUILTIN_SUBPD;
    case IX86_BUILTIN_SUBSD: return GSBI_IX86_BUILTIN_SUBSD;
    case IX86_BUILTIN_CMPEQPD: return GSBI_IX86_BUILTIN_CMPEQPD;
    case IX86_BUILTIN_CMPLTPD: return GSBI_IX86_BUILTIN_CMPLTPD;
    case IX86_BUILTIN_CMPLEPD: return GSBI_IX86_BUILTIN_CMPLEPD;
    case IX86_BUILTIN_CMPGTPD: return GSBI_IX86_BUILTIN_CMPGTPD;
    case IX86_BUILTIN_CMPGEPD: return GSBI_IX86_BUILTIN_CMPGEPD;
    case IX86_BUILTIN_CMPNEQPD: return GSBI_IX86_BUILTIN_CMPNEQPD;
    case IX86_BUILTIN_CMPNLTPD: return GSBI_IX86_BUILTIN_CMPNLTPD;
    case IX86_BUILTIN_CMPNLEPD: return GSBI_IX86_BUILTIN_CMPNLEPD;
    case IX86_BUILTIN_CMPNGTPD: return GSBI_IX86_BUILTIN_CMPNGTPD;
    case IX86_BUILTIN_CMPNGEPD: return GSBI_IX86_BUILTIN_CMPNGEPD;
    case IX86_BUILTIN_CMPORDPD: return GSBI_IX86_BUILTIN_CMPORDPD;
    case IX86_BUILTIN_CMPUNORDPD: return GSBI_IX86_BUILTIN_CMPUNORDPD;
    case IX86_BUILTIN_CMPNEPD: return GSBI_IX86_BUILTIN_CMPNEPD;
    case IX86_BUILTIN_CMPEQSD: return GSBI_IX86_BUILTIN_CMPEQSD;
    case IX86_BUILTIN_CMPLTSD: return GSBI_IX86_BUILTIN_CMPLTSD;
    case IX86_BUILTIN_CMPLESD: return GSBI_IX86_BUILTIN_CMPLESD;
    case IX86_BUILTIN_CMPNEQSD: return GSBI_IX86_BUILTIN_CMPNEQSD;
    case IX86_BUILTIN_CMPNLTSD: return GSBI_IX86_BUILTIN_CMPNLTSD;
    case IX86_BUILTIN_CMPNLESD: return GSBI_IX86_BUILTIN_CMPNLESD;
    case IX86_BUILTIN_CMPORDSD: return GSBI_IX86_BUILTIN_CMPORDSD;
    case IX86_BUILTIN_CMPUNORDSD: return GSBI_IX86_BUILTIN_CMPUNORDSD;
    case IX86_BUILTIN_CMPNESD: return GSBI_IX86_BUILTIN_CMPNESD;
    case IX86_BUILTIN_COMIEQSD: return GSBI_IX86_BUILTIN_COMIEQSD;
    case IX86_BUILTIN_COMILTSD: return GSBI_IX86_BUILTIN_COMILTSD;
    case IX86_BUILTIN_COMILESD: return GSBI_IX86_BUILTIN_COMILESD;
    case IX86_BUILTIN_COMIGTSD: return GSBI_IX86_BUILTIN_COMIGTSD;
    case IX86_BUILTIN_COMIGESD: return GSBI_IX86_BUILTIN_COMIGESD;
    case IX86_BUILTIN_COMINEQSD: return GSBI_IX86_BUILTIN_COMINEQSD;
    case IX86_BUILTIN_UCOMIEQSD: return GSBI_IX86_BUILTIN_UCOMIEQSD;
    case IX86_BUILTIN_UCOMILTSD: return GSBI_IX86_BUILTIN_UCOMILTSD;
    case IX86_BUILTIN_UCOMILESD: return GSBI_IX86_BUILTIN_UCOMILESD;
    case IX86_BUILTIN_UCOMIGTSD: return GSBI_IX86_BUILTIN_UCOMIGTSD;
    case IX86_BUILTIN_UCOMIGESD: return GSBI_IX86_BUILTIN_UCOMIGESD;
    case IX86_BUILTIN_UCOMINEQSD: return GSBI_IX86_BUILTIN_UCOMINEQSD;
    case IX86_BUILTIN_MAXPD: return GSBI_IX86_BUILTIN_MAXPD;
    case IX86_BUILTIN_MAXSD: return GSBI_IX86_BUILTIN_MAXSD;
    case IX86_BUILTIN_MINPD: return GSBI_IX86_BUILTIN_MINPD;
    case IX86_BUILTIN_MINSD: return GSBI_IX86_BUILTIN_MINSD;
    case IX86_BUILTIN_ANDPD: return GSBI_IX86_BUILTIN_ANDPD;
    case IX86_BUILTIN_ANDNPD: return GSBI_IX86_BUILTIN_ANDNPD;
    case IX86_BUILTIN_ORPD: return GSBI_IX86_BUILTIN_ORPD;
    case IX86_BUILTIN_XORPD: return GSBI_IX86_BUILTIN_XORPD;
    case IX86_BUILTIN_SQRTPD: return GSBI_IX86_BUILTIN_SQRTPD;
    case IX86_BUILTIN_SQRTSD: return GSBI_IX86_BUILTIN_SQRTSD;
    case IX86_BUILTIN_UNPCKHPD: return GSBI_IX86_BUILTIN_UNPCKHPD;
    case IX86_BUILTIN_UNPCKLPD: return GSBI_IX86_BUILTIN_UNPCKLPD;
    case IX86_BUILTIN_SHUFPD: return GSBI_IX86_BUILTIN_SHUFPD;
    case IX86_BUILTIN_LOADUPD: return GSBI_IX86_BUILTIN_LOADUPD;
    case IX86_BUILTIN_STOREUPD: return GSBI_IX86_BUILTIN_STOREUPD;
    case IX86_BUILTIN_MOVSD: return GSBI_IX86_BUILTIN_MOVSD;
    case IX86_BUILTIN_LOADHPD: return GSBI_IX86_BUILTIN_LOADHPD;
    case IX86_BUILTIN_LOADLPD: return GSBI_IX86_BUILTIN_LOADLPD;
    case IX86_BUILTIN_CVTDQ2PD: return GSBI_IX86_BUILTIN_CVTDQ2PD;
    case IX86_BUILTIN_CVTDQ2PS: return GSBI_IX86_BUILTIN_CVTDQ2PS;
    case IX86_BUILTIN_CVTPD2DQ: return GSBI_IX86_BUILTIN_CVTPD2DQ;
    case IX86_BUILTIN_CVTPD2PI: return GSBI_IX86_BUILTIN_CVTPD2PI;
    case IX86_BUILTIN_CVTPD2PS: return GSBI_IX86_BUILTIN_CVTPD2PS;
    case IX86_BUILTIN_CVTTPD2DQ: return GSBI_IX86_BUILTIN_CVTTPD2DQ;
    case IX86_BUILTIN_CVTTPD2PI: return GSBI_IX86_BUILTIN_CVTTPD2PI;
    case IX86_BUILTIN_CVTPI2PD: return GSBI_IX86_BUILTIN_CVTPI2PD;
    case IX86_BUILTIN_CVTSI2SD: return GSBI_IX86_BUILTIN_CVTSI2SD;
    case IX86_BUILTIN_CVTSI642SD: return GSBI_IX86_BUILTIN_CVTSI642SD;
    case IX86_BUILTIN_CVTSD2SI: return GSBI_IX86_BUILTIN_CVTSD2SI;
    case IX86_BUILTIN_CVTSD2SI64: return GSBI_IX86_BUILTIN_CVTSD2SI64;
    case IX86_BUILTIN_CVTSD2SS: return GSBI_IX86_BUILTIN_CVTSD2SS;
    case IX86_BUILTIN_CVTSS2SD: return GSBI_IX86_BUILTIN_CVTSS2SD;
    case IX86_BUILTIN_CVTTSD2SI: return GSBI_IX86_BUILTIN_CVTTSD2SI;
    case IX86_BUILTIN_CVTTSD2SI64: return GSBI_IX86_BUILTIN_CVTTSD2SI64;
    case IX86_BUILTIN_CVTPS2DQ: return GSBI_IX86_BUILTIN_CVTPS2DQ;
    case IX86_BUILTIN_CVTPS2PD: return GSBI_IX86_BUILTIN_CVTPS2PD;
    case IX86_BUILTIN_CVTTPS2DQ: return GSBI_IX86_BUILTIN_CVTTPS2DQ;
    case IX86_BUILTIN_MOVNTI: return GSBI_IX86_BUILTIN_MOVNTI;
    case IX86_BUILTIN_MOVNTPD: return GSBI_IX86_BUILTIN_MOVNTPD;
    case IX86_BUILTIN_MOVNTDQ: return GSBI_IX86_BUILTIN_MOVNTDQ;
    case IX86_BUILTIN_MASKMOVDQU: return GSBI_IX86_BUILTIN_MASKMOVDQU;
    case IX86_BUILTIN_MOVMSKPD: return GSBI_IX86_BUILTIN_MOVMSKPD;
    case IX86_BUILTIN_PMOVMSKB128: return GSBI_IX86_BUILTIN_PMOVMSKB128;
    case IX86_BUILTIN_PACKSSWB128: return GSBI_IX86_BUILTIN_PACKSSWB128;
    case IX86_BUILTIN_PACKSSDW128: return GSBI_IX86_BUILTIN_PACKSSDW128;
    case IX86_BUILTIN_PACKUSWB128: return GSBI_IX86_BUILTIN_PACKUSWB128;
    case IX86_BUILTIN_PADDB128: return GSBI_IX86_BUILTIN_PADDB128;
    case IX86_BUILTIN_PADDW128: return GSBI_IX86_BUILTIN_PADDW128;
    case IX86_BUILTIN_PADDD128: return GSBI_IX86_BUILTIN_PADDD128;
    case IX86_BUILTIN_PADDQ128: return GSBI_IX86_BUILTIN_PADDQ128;
    case IX86_BUILTIN_PADDSB128: return GSBI_IX86_BUILTIN_PADDSB128;
    case IX86_BUILTIN_PADDSW128: return GSBI_IX86_BUILTIN_PADDSW128;
    case IX86_BUILTIN_PADDUSB128: return GSBI_IX86_BUILTIN_PADDUSB128;
    case IX86_BUILTIN_PADDUSW128: return GSBI_IX86_BUILTIN_PADDUSW128;
    case IX86_BUILTIN_PSUBB128: return GSBI_IX86_BUILTIN_PSUBB128;
    case IX86_BUILTIN_PSUBW128: return GSBI_IX86_BUILTIN_PSUBW128;
    case IX86_BUILTIN_PSUBD128: return GSBI_IX86_BUILTIN_PSUBD128;
    case IX86_BUILTIN_PSUBQ128: return GSBI_IX86_BUILTIN_PSUBQ128;
    case IX86_BUILTIN_PSUBSB128: return GSBI_IX86_BUILTIN_PSUBSB128;
    case IX86_BUILTIN_PSUBSW128: return GSBI_IX86_BUILTIN_PSUBSW128;
    case IX86_BUILTIN_PSUBUSB128: return GSBI_IX86_BUILTIN_PSUBUSB128;
    case IX86_BUILTIN_PSUBUSW128: return GSBI_IX86_BUILTIN_PSUBUSW128;
    case IX86_BUILTIN_PAND128: return GSBI_IX86_BUILTIN_PAND128;
    case IX86_BUILTIN_PANDN128: return GSBI_IX86_BUILTIN_PANDN128;
    case IX86_BUILTIN_POR128: return GSBI_IX86_BUILTIN_POR128;
    case IX86_BUILTIN_PXOR128: return GSBI_IX86_BUILTIN_PXOR128;
    case IX86_BUILTIN_PAVGB128: return GSBI_IX86_BUILTIN_PAVGB128;
    case IX86_BUILTIN_PAVGW128: return GSBI_IX86_BUILTIN_PAVGW128;
    case IX86_BUILTIN_PCMPEQB128: return GSBI_IX86_BUILTIN_PCMPEQB128;
    case IX86_BUILTIN_PCMPEQW128: return GSBI_IX86_BUILTIN_PCMPEQW128;
    case IX86_BUILTIN_PCMPEQD128: return GSBI_IX86_BUILTIN_PCMPEQD128;
    case IX86_BUILTIN_PCMPGTB128: return GSBI_IX86_BUILTIN_PCMPGTB128;
    case IX86_BUILTIN_PCMPGTW128: return GSBI_IX86_BUILTIN_PCMPGTW128;
    case IX86_BUILTIN_PCMPGTD128: return GSBI_IX86_BUILTIN_PCMPGTD128;
    case IX86_BUILTIN_PMADDWD128: return GSBI_IX86_BUILTIN_PMADDWD128;
    case IX86_BUILTIN_PMAXSW128: return GSBI_IX86_BUILTIN_PMAXSW128;
    case IX86_BUILTIN_PMAXUB128: return GSBI_IX86_BUILTIN_PMAXUB128;
    case IX86_BUILTIN_PMINSW128: return GSBI_IX86_BUILTIN_PMINSW128;
    case IX86_BUILTIN_PMINUB128: return GSBI_IX86_BUILTIN_PMINUB128;
    case IX86_BUILTIN_PMULUDQ: return GSBI_IX86_BUILTIN_PMULUDQ;
    case IX86_BUILTIN_PMULUDQ128: return GSBI_IX86_BUILTIN_PMULUDQ128;
    case IX86_BUILTIN_PMULHUW128: return GSBI_IX86_BUILTIN_PMULHUW128;
    case IX86_BUILTIN_PMULHW128: return GSBI_IX86_BUILTIN_PMULHW128;
    case IX86_BUILTIN_PMULLW128: return GSBI_IX86_BUILTIN_PMULLW128;
    case IX86_BUILTIN_PSADBW128: return GSBI_IX86_BUILTIN_PSADBW128;
    case IX86_BUILTIN_PSHUFHW: return GSBI_IX86_BUILTIN_PSHUFHW;
    case IX86_BUILTIN_PSHUFLW: return GSBI_IX86_BUILTIN_PSHUFLW;
    case IX86_BUILTIN_PSHUFD: return GSBI_IX86_BUILTIN_PSHUFD;
    case IX86_BUILTIN_PSLLW128: return GSBI_IX86_BUILTIN_PSLLW128;
    case IX86_BUILTIN_PSLLD128: return GSBI_IX86_BUILTIN_PSLLD128;
    case IX86_BUILTIN_PSLLQ128: return GSBI_IX86_BUILTIN_PSLLQ128;
    case IX86_BUILTIN_PSRAW128: return GSBI_IX86_BUILTIN_PSRAW128;
    case IX86_BUILTIN_PSRAD128: return GSBI_IX86_BUILTIN_PSRAD128;
    case IX86_BUILTIN_PSRLW128: return GSBI_IX86_BUILTIN_PSRLW128;
    case IX86_BUILTIN_PSRLD128: return GSBI_IX86_BUILTIN_PSRLD128;
    case IX86_BUILTIN_PSRLQ128: return GSBI_IX86_BUILTIN_PSRLQ128;
    case IX86_BUILTIN_PSLLDQI128: return GSBI_IX86_BUILTIN_PSLLDQI128;
    case IX86_BUILTIN_PSLLWI128: return GSBI_IX86_BUILTIN_PSLLWI128;
    case IX86_BUILTIN_PSLLDI128: return GSBI_IX86_BUILTIN_PSLLDI128;
    case IX86_BUILTIN_PSLLQI128: return GSBI_IX86_BUILTIN_PSLLQI128;
    case IX86_BUILTIN_PSRAWI128: return GSBI_IX86_BUILTIN_PSRAWI128;
    case IX86_BUILTIN_PSRADI128: return GSBI_IX86_BUILTIN_PSRADI128;
    case IX86_BUILTIN_PSRLDQI128: return GSBI_IX86_BUILTIN_PSRLDQI128;
    case IX86_BUILTIN_PSRLWI128: return GSBI_IX86_BUILTIN_PSRLWI128;
    case IX86_BUILTIN_PSRLDI128: return GSBI_IX86_BUILTIN_PSRLDI128;
    case IX86_BUILTIN_PSRLQI128: return GSBI_IX86_BUILTIN_PSRLQI128;
    case IX86_BUILTIN_PUNPCKHBW128: return GSBI_IX86_BUILTIN_PUNPCKHBW128;
    case IX86_BUILTIN_PUNPCKHWD128: return GSBI_IX86_BUILTIN_PUNPCKHWD128;
    case IX86_BUILTIN_PUNPCKHDQ128: return GSBI_IX86_BUILTIN_PUNPCKHDQ128;
    case IX86_BUILTIN_PUNPCKHQDQ128: return GSBI_IX86_BUILTIN_PUNPCKHQDQ128;
    case IX86_BUILTIN_PUNPCKLBW128: return GSBI_IX86_BUILTIN_PUNPCKLBW128;
    case IX86_BUILTIN_PUNPCKLWD128: return GSBI_IX86_BUILTIN_PUNPCKLWD128;
    case IX86_BUILTIN_PUNPCKLDQ128: return GSBI_IX86_BUILTIN_PUNPCKLDQ128;
    case IX86_BUILTIN_PUNPCKLQDQ128: return GSBI_IX86_BUILTIN_PUNPCKLQDQ128;
    case IX86_BUILTIN_CLFLUSH: return GSBI_IX86_BUILTIN_CLFLUSH;
    case IX86_BUILTIN_MFENCE: return GSBI_IX86_BUILTIN_MFENCE;
    case IX86_BUILTIN_LFENCE: return GSBI_IX86_BUILTIN_LFENCE;
    case IX86_BUILTIN_ADDSUBPS: return GSBI_IX86_BUILTIN_ADDSUBPS;
    case IX86_BUILTIN_HADDPS: return GSBI_IX86_BUILTIN_HADDPS;
    case IX86_BUILTIN_HSUBPS: return GSBI_IX86_BUILTIN_HSUBPS;
    case IX86_BUILTIN_MOVSHDUP: return GSBI_IX86_BUILTIN_MOVSHDUP;
    case IX86_BUILTIN_MOVSLDUP: return GSBI_IX86_BUILTIN_MOVSLDUP;
    case IX86_BUILTIN_ADDSUBPD: return GSBI_IX86_BUILTIN_ADDSUBPD;
    case IX86_BUILTIN_HADDPD: return GSBI_IX86_BUILTIN_HADDPD;
    case IX86_BUILTIN_HSUBPD: return GSBI_IX86_BUILTIN_HSUBPD;
    case IX86_BUILTIN_LDDQU: return GSBI_IX86_BUILTIN_LDDQU;
    case IX86_BUILTIN_MONITOR: return GSBI_IX86_BUILTIN_MONITOR;
    case IX86_BUILTIN_MWAIT: return GSBI_IX86_BUILTIN_MWAIT;
    case IX86_BUILTIN_VEC_INIT_V2SI: return GSBI_IX86_BUILTIN_VEC_INIT_V2SI;
    case IX86_BUILTIN_VEC_INIT_V4HI: return GSBI_IX86_BUILTIN_VEC_INIT_V4HI;
    case IX86_BUILTIN_VEC_INIT_V8QI: return GSBI_IX86_BUILTIN_VEC_INIT_V8QI;
    case IX86_BUILTIN_VEC_EXT_V2DF: return GSBI_IX86_BUILTIN_VEC_EXT_V2DF;
    case IX86_BUILTIN_VEC_EXT_V2DI: return GSBI_IX86_BUILTIN_VEC_EXT_V2DI;
    case IX86_BUILTIN_VEC_EXT_V4SF: return GSBI_IX86_BUILTIN_VEC_EXT_V4SF;
    case IX86_BUILTIN_VEC_EXT_V4SI: return GSBI_IX86_BUILTIN_VEC_EXT_V4SI;
    case IX86_BUILTIN_VEC_EXT_V8HI: return GSBI_IX86_BUILTIN_VEC_EXT_V8HI;
    case IX86_BUILTIN_VEC_EXT_V2SI: return GSBI_IX86_BUILTIN_VEC_EXT_V2SI;
    case IX86_BUILTIN_VEC_EXT_V4HI: return GSBI_IX86_BUILTIN_VEC_EXT_V4HI;
    case IX86_BUILTIN_VEC_SET_V8HI: return GSBI_IX86_BUILTIN_VEC_SET_V8HI;
    case IX86_BUILTIN_VEC_SET_V4HI: return GSBI_IX86_BUILTIN_VEC_SET_V4HI;
    case IX86_BUILTIN_MOVNTSS: return GSBI_IX86_BUILTIN_MOVNTSS;
    case IX86_BUILTIN_MOVNTSD: return GSBI_IX86_BUILTIN_MOVNTSD;
    case IX86_BUILTIN_EXTRQI: return GSBI_IX86_BUILTIN_EXTRQI;
    case IX86_BUILTIN_EXTRQ: return GSBI_IX86_BUILTIN_EXTRQ;
    case IX86_BUILTIN_INSERTQI: return GSBI_IX86_BUILTIN_INSERTQI;
    case IX86_BUILTIN_INSERTQ: return GSBI_IX86_BUILTIN_INSERTQ;

    /* SSSE3 intrinsics */
    case IX86_BUILTIN_PABSB:    return GSBI_IX86_BUILTIN_PABSB;
    case IX86_BUILTIN_PABSB128: return GSBI_IX86_BUILTIN_PABSB128;
    case IX86_BUILTIN_PABSD:    return GSBI_IX86_BUILTIN_PABSD;
    case IX86_BUILTIN_PABSD128: return GSBI_IX86_BUILTIN_PABSD128;
    case IX86_BUILTIN_PABSW:    return GSBI_IX86_BUILTIN_PABSW;
    case IX86_BUILTIN_PABSW128: return GSBI_IX86_BUILTIN_PABSW128;
    case IX86_BUILTIN_PALIGNR:  return GSBI_IX86_BUILTIN_PALIGNR;
    case IX86_BUILTIN_PALIGNR128:       return GSBI_IX86_BUILTIN_PALIGNR128;
    case IX86_BUILTIN_PHADDD:   return GSBI_IX86_BUILTIN_PHADDD;
    case IX86_BUILTIN_PHADDD128:        return GSBI_IX86_BUILTIN_PHADDD128;
    case IX86_BUILTIN_PHADDSW:  return GSBI_IX86_BUILTIN_PHADDSW;
    case IX86_BUILTIN_PHADDSW128:       return GSBI_IX86_BUILTIN_PHADDSW128;
    case IX86_BUILTIN_PHADDW:   return GSBI_IX86_BUILTIN_PHADDW;
    case IX86_BUILTIN_PHADDW128:        return GSBI_IX86_BUILTIN_PHADDW128;
    case IX86_BUILTIN_PHSUBD:   return GSBI_IX86_BUILTIN_PHSUBD;
    case IX86_BUILTIN_PHSUBD128:        return GSBI_IX86_BUILTIN_PHSUBD128;
    case IX86_BUILTIN_PHSUBSW:  return GSBI_IX86_BUILTIN_PHSUBSW;
    case IX86_BUILTIN_PHSUBSW128:       return GSBI_IX86_BUILTIN_PHSUBSW128;
    case IX86_BUILTIN_PHSUBW:   return GSBI_IX86_BUILTIN_PHSUBW;
    case IX86_BUILTIN_PHSUBW128:        return GSBI_IX86_BUILTIN_PHSUBW128;
    case IX86_BUILTIN_PMADDUBSW:        return GSBI_IX86_BUILTIN_PMADDUBSW;
    case IX86_BUILTIN_PMADDUBSW128:     return GSBI_IX86_BUILTIN_PMADDUBSW128;
    case IX86_BUILTIN_PMULHRSW: return GSBI_IX86_BUILTIN_PMULHRSW;
    case IX86_BUILTIN_PMULHRSW128:      return GSBI_IX86_BUILTIN_PMULHRSW128;
    case IX86_BUILTIN_PSHUFB:   return GSBI_IX86_BUILTIN_PSHUFB;
    case IX86_BUILTIN_PSHUFB128:        return GSBI_IX86_BUILTIN_PSHUFB128;
    case IX86_BUILTIN_PSIGNB:   return GSBI_IX86_BUILTIN_PSIGNB;
    case IX86_BUILTIN_PSIGNB128:        return GSBI_IX86_BUILTIN_PSIGNB128;
    case IX86_BUILTIN_PSIGND:   return GSBI_IX86_BUILTIN_PSIGND;
    case IX86_BUILTIN_PSIGND128:        return GSBI_IX86_BUILTIN_PSIGND128;
    case IX86_BUILTIN_PSIGNW:   return GSBI_IX86_BUILTIN_PSIGNW;
    case IX86_BUILTIN_PSIGNW128:        return GSBI_IX86_BUILTIN_PSIGNW128;

    /* SSE4.1 intrinsics */
    case IX86_BUILTIN_BLENDPD:  return GSBI_IX86_BUILTIN_BLENDPD;
    case IX86_BUILTIN_BLENDPS:  return GSBI_IX86_BUILTIN_BLENDPS;
    case IX86_BUILTIN_BLENDVPD: return GSBI_IX86_BUILTIN_BLENDVPD;
    case IX86_BUILTIN_BLENDVPS: return GSBI_IX86_BUILTIN_BLENDVPS;
    case IX86_BUILTIN_DPPD:     return GSBI_IX86_BUILTIN_DPPD;
    case IX86_BUILTIN_DPPS:     return GSBI_IX86_BUILTIN_DPPS;
    case IX86_BUILTIN_INSERTPS128:      return GSBI_IX86_BUILTIN_INSERTPS128;
    case IX86_BUILTIN_MOVNTDQA: return GSBI_IX86_BUILTIN_MOVNTDQA;
    case IX86_BUILTIN_MPSADBW128:       return GSBI_IX86_BUILTIN_MPSADBW128;
    case IX86_BUILTIN_PACKUSDW128:      return GSBI_IX86_BUILTIN_PACKUSDW128;
    case IX86_BUILTIN_PBLENDVB128:      return GSBI_IX86_BUILTIN_PBLENDVB128;
    case IX86_BUILTIN_PBLENDW128:       return GSBI_IX86_BUILTIN_PBLENDW128;
    case IX86_BUILTIN_PCMPEQQ:  return GSBI_IX86_BUILTIN_PCMPEQQ;
    case IX86_BUILTIN_PHMINPOSUW128:    return GSBI_IX86_BUILTIN_PHMINPOSUW128;
    case IX86_BUILTIN_PMAXSB128:        return GSBI_IX86_BUILTIN_PMAXSB128;
    case IX86_BUILTIN_PMAXSD128:        return GSBI_IX86_BUILTIN_PMAXSD128;
    case IX86_BUILTIN_PMAXUD128:        return GSBI_IX86_BUILTIN_PMAXUD128;
    case IX86_BUILTIN_PMAXUW128:        return GSBI_IX86_BUILTIN_PMAXUW128;
    case IX86_BUILTIN_PMINSB128:        return GSBI_IX86_BUILTIN_PMINSB128;
    case IX86_BUILTIN_PMINSD128:        return GSBI_IX86_BUILTIN_PMINSD128;
    case IX86_BUILTIN_PMINUD128:        return GSBI_IX86_BUILTIN_PMINUD128;
    case IX86_BUILTIN_PMINUW128:        return GSBI_IX86_BUILTIN_PMINUW128;
    case IX86_BUILTIN_PMOVSXBD128:      return GSBI_IX86_BUILTIN_PMOVSXBD128;
    case IX86_BUILTIN_PMOVSXBQ128:      return GSBI_IX86_BUILTIN_PMOVSXBQ128;
    case IX86_BUILTIN_PMOVSXBW128:      return GSBI_IX86_BUILTIN_PMOVSXBW128;
    case IX86_BUILTIN_PMOVSXDQ128:      return GSBI_IX86_BUILTIN_PMOVSXDQ128;
    case IX86_BUILTIN_PMOVSXWD128:      return GSBI_IX86_BUILTIN_PMOVSXWD128;
    case IX86_BUILTIN_PMOVSXWQ128:      return GSBI_IX86_BUILTIN_PMOVSXWQ128;
    case IX86_BUILTIN_PMOVZXBD128:      return GSBI_IX86_BUILTIN_PMOVZXBD128;
    case IX86_BUILTIN_PMOVZXBQ128:      return GSBI_IX86_BUILTIN_PMOVZXBQ128;
    case IX86_BUILTIN_PMOVZXBW128:      return GSBI_IX86_BUILTIN_PMOVZXBW128;
    case IX86_BUILTIN_PMOVZXDQ128:      return GSBI_IX86_BUILTIN_PMOVZXDQ128;
    case IX86_BUILTIN_PMOVZXWD128:      return GSBI_IX86_BUILTIN_PMOVZXWD128;
    case IX86_BUILTIN_PMOVZXWQ128:      return GSBI_IX86_BUILTIN_PMOVZXWQ128;
    case IX86_BUILTIN_PMULDQ128:        return GSBI_IX86_BUILTIN_PMULDQ128;
    case IX86_BUILTIN_PMULLD128:        return GSBI_IX86_BUILTIN_PMULLD128;
    case IX86_BUILTIN_VEC_SET_V16QI:    return GSBI_IX86_BUILTIN_VEC_SET_V16QI;
    case IX86_BUILTIN_VEC_SET_V2DI:     return GSBI_IX86_BUILTIN_VEC_SET_V2DI;
    case IX86_BUILTIN_VEC_SET_V4SF:     return GSBI_IX86_BUILTIN_VEC_SET_V4SF;
    case IX86_BUILTIN_VEC_SET_V4SI:     return GSBI_IX86_BUILTIN_VEC_SET_V4SI;

    /* SSE4.2 intrinsics */
    case IX86_BUILTIN_CRC32DI:  return GSBI_IX86_BUILTIN_CRC32DI;
    case IX86_BUILTIN_CRC32HI:  return GSBI_IX86_BUILTIN_CRC32HI;
    case IX86_BUILTIN_CRC32QI:  return GSBI_IX86_BUILTIN_CRC32QI;
    case IX86_BUILTIN_CRC32SI:  return GSBI_IX86_BUILTIN_CRC32SI;
    case IX86_BUILTIN_PCMPESTRI128:     return GSBI_IX86_BUILTIN_PCMPESTRI128;
    case IX86_BUILTIN_PCMPESTRA128:     return GSBI_IX86_BUILTIN_PCMPESTRA128;
    case IX86_BUILTIN_PCMPESTRC128:     return GSBI_IX86_BUILTIN_PCMPESTRC128;
    case IX86_BUILTIN_PCMPESTRO128:     return GSBI_IX86_BUILTIN_PCMPESTRO128;
    case IX86_BUILTIN_PCMPESTRS128:     return GSBI_IX86_BUILTIN_PCMPESTRS128;
    case IX86_BUILTIN_PCMPESTRZ128:     return GSBI_IX86_BUILTIN_PCMPESTRZ128;
    case IX86_BUILTIN_PCMPESTRM128:     return GSBI_IX86_BUILTIN_PCMPESTRM128;
    case IX86_BUILTIN_PCMPGTQ:  return GSBI_IX86_BUILTIN_PCMPGTQ;
    case IX86_BUILTIN_PCMPISTRI128:     return GSBI_IX86_BUILTIN_PCMPISTRI128;
    case IX86_BUILTIN_PCMPISTRA128:     return GSBI_IX86_BUILTIN_PCMPISTRA128;
    case IX86_BUILTIN_PCMPISTRC128:     return GSBI_IX86_BUILTIN_PCMPISTRC128;
    case IX86_BUILTIN_PCMPISTRO128:     return GSBI_IX86_BUILTIN_PCMPISTRO128;
    case IX86_BUILTIN_PCMPISTRS128:     return GSBI_IX86_BUILTIN_PCMPISTRS128;
    case IX86_BUILTIN_PCMPISTRZ128:     return GSBI_IX86_BUILTIN_PCMPISTRZ128;
    case IX86_BUILTIN_PCMPISTRM128:     return GSBI_IX86_BUILTIN_PCMPISTRM128;

    /* AES intrinsics */
    case IX86_BUILTIN_AESDEC128:        return GSBI_IX86_BUILTIN_AESDEC128;
    case IX86_BUILTIN_AESDECLAST128:    return GSBI_IX86_BUILTIN_AESDECLAST128;
    case IX86_BUILTIN_AESENC128:        return GSBI_IX86_BUILTIN_AESENC128;
    case IX86_BUILTIN_AESENCLAST128:    return GSBI_IX86_BUILTIN_AESENCLAST128;
    case IX86_BUILTIN_AESIMC128:        return GSBI_IX86_BUILTIN_AESIMC128;
    case IX86_BUILTIN_AESKEYGENASSIST128:       return GSBI_IX86_BUILTIN_AESKEYGENASSIST128;

    /* PCLMUL intrinsics */
    case IX86_BUILTIN_PCLMULQDQ128:     return GSBI_IX86_BUILTIN_PCLMULQDQ128;

    /* AVX intrinsics */
    case IX86_BUILTIN_ADDPD256: return GSBI_IX86_BUILTIN_ADDPD256;
    case IX86_BUILTIN_ADDPS256: return GSBI_IX86_BUILTIN_ADDPS256;
    case IX86_BUILTIN_ADDSUBPD256:      return GSBI_IX86_BUILTIN_ADDSUBPD256;
    case IX86_BUILTIN_ADDSUBPS256:      return GSBI_IX86_BUILTIN_ADDSUBPS256;
    case IX86_BUILTIN_ANDNPD256:        return GSBI_IX86_BUILTIN_ANDNPD256;
    case IX86_BUILTIN_ANDNPS256:        return GSBI_IX86_BUILTIN_ANDNPS256;
    case IX86_BUILTIN_ANDPD256: return GSBI_IX86_BUILTIN_ANDPD256;
    case IX86_BUILTIN_ANDPS256: return GSBI_IX86_BUILTIN_ANDPS256;
    case IX86_BUILTIN_BLENDPD256:       return GSBI_IX86_BUILTIN_BLENDPD256;
    case IX86_BUILTIN_BLENDPS256:       return GSBI_IX86_BUILTIN_BLENDPS256;
    case IX86_BUILTIN_BLENDVPD256:      return GSBI_IX86_BUILTIN_BLENDVPD256;
    case IX86_BUILTIN_BLENDVPS256:      return GSBI_IX86_BUILTIN_BLENDVPS256;
    case IX86_BUILTIN_CMPPD:    return GSBI_IX86_BUILTIN_CMPPD;
    case IX86_BUILTIN_CMPPD256: return GSBI_IX86_BUILTIN_CMPPD256;
    case IX86_BUILTIN_CMPPS:    return GSBI_IX86_BUILTIN_CMPPS;
    case IX86_BUILTIN_CMPPS256: return GSBI_IX86_BUILTIN_CMPPS256;
    case IX86_BUILTIN_CMPSD:    return GSBI_IX86_BUILTIN_CMPSD;
    case IX86_BUILTIN_CMPSS:    return GSBI_IX86_BUILTIN_CMPSS;
    case IX86_BUILTIN_CVTDQ2PD256:      return GSBI_IX86_BUILTIN_CVTDQ2PD256;
    case IX86_BUILTIN_CVTDQ2PS256:      return GSBI_IX86_BUILTIN_CVTDQ2PS256;
    case IX86_BUILTIN_CVTPD2DQ256:      return GSBI_IX86_BUILTIN_CVTPD2DQ256;
    case IX86_BUILTIN_CVTPD2PS256:      return GSBI_IX86_BUILTIN_CVTPD2PS256;
    case IX86_BUILTIN_CVTPS2DQ256:      return GSBI_IX86_BUILTIN_CVTPS2DQ256;
    case IX86_BUILTIN_CVTPS2PD256:      return GSBI_IX86_BUILTIN_CVTPS2PD256;
    case IX86_BUILTIN_CVTTPD2DQ256:     return GSBI_IX86_BUILTIN_CVTTPD2DQ256;
    case IX86_BUILTIN_CVTTPS2DQ256:     return GSBI_IX86_BUILTIN_CVTTPS2DQ256;
    case IX86_BUILTIN_DIVPD256: return GSBI_IX86_BUILTIN_DIVPD256;
    case IX86_BUILTIN_DIVPS256: return GSBI_IX86_BUILTIN_DIVPS256;
    case IX86_BUILTIN_DPPS256:  return GSBI_IX86_BUILTIN_DPPS256;
    case IX86_BUILTIN_HADDPD256:        return GSBI_IX86_BUILTIN_HADDPD256;
    case IX86_BUILTIN_HADDPS256:        return GSBI_IX86_BUILTIN_HADDPS256;
    case IX86_BUILTIN_HSUBPD256:        return GSBI_IX86_BUILTIN_HSUBPD256;
    case IX86_BUILTIN_HSUBPS256:        return GSBI_IX86_BUILTIN_HSUBPS256;
    case IX86_BUILTIN_LDDQU256: return GSBI_IX86_BUILTIN_LDDQU256;
    case IX86_BUILTIN_LOADDQU256:       return GSBI_IX86_BUILTIN_LOADDQU256;
    case IX86_BUILTIN_LOADUPD256:       return GSBI_IX86_BUILTIN_LOADUPD256;
    case IX86_BUILTIN_LOADUPS256:       return GSBI_IX86_BUILTIN_LOADUPS256;
    case IX86_BUILTIN_MASKLOADPD:       return GSBI_IX86_BUILTIN_MASKLOADPD;
    case IX86_BUILTIN_MASKLOADPD256:    return GSBI_IX86_BUILTIN_MASKLOADPD256;
    case IX86_BUILTIN_MASKLOADPS:       return GSBI_IX86_BUILTIN_MASKLOADPS;
    case IX86_BUILTIN_MASKLOADPS256:    return GSBI_IX86_BUILTIN_MASKLOADPS256;
    case IX86_BUILTIN_MASKSTOREPD:      return GSBI_IX86_BUILTIN_MASKSTOREPD;
    case IX86_BUILTIN_MASKSTOREPD256:   return GSBI_IX86_BUILTIN_MASKSTOREPD256;
    case IX86_BUILTIN_MASKSTOREPS:      return GSBI_IX86_BUILTIN_MASKSTOREPS;
    case IX86_BUILTIN_MASKSTOREPS256:   return GSBI_IX86_BUILTIN_MASKSTOREPS256;
    case IX86_BUILTIN_MAXPD256: return GSBI_IX86_BUILTIN_MAXPD256;
    case IX86_BUILTIN_MAXPS256: return GSBI_IX86_BUILTIN_MAXPS256;
    case IX86_BUILTIN_MINPD256: return GSBI_IX86_BUILTIN_MINPD256;
    case IX86_BUILTIN_MINPS256: return GSBI_IX86_BUILTIN_MINPS256;
    case IX86_BUILTIN_MOVDDUP256:       return GSBI_IX86_BUILTIN_MOVDDUP256;
    case IX86_BUILTIN_MOVMSKPD256:      return GSBI_IX86_BUILTIN_MOVMSKPD256;
    case IX86_BUILTIN_MOVMSKPS256:      return GSBI_IX86_BUILTIN_MOVMSKPS256;
    case IX86_BUILTIN_MOVNTDQ256:       return GSBI_IX86_BUILTIN_MOVNTDQ256;
    case IX86_BUILTIN_MOVNTPD256:       return GSBI_IX86_BUILTIN_MOVNTPD256;
    case IX86_BUILTIN_MOVNTPS256:       return GSBI_IX86_BUILTIN_MOVNTPS256;
    case IX86_BUILTIN_MOVSHDUP256:      return GSBI_IX86_BUILTIN_MOVSHDUP256;
    case IX86_BUILTIN_MOVSLDUP256:      return GSBI_IX86_BUILTIN_MOVSLDUP256;
    case IX86_BUILTIN_MULPD256: return GSBI_IX86_BUILTIN_MULPD256;
    case IX86_BUILTIN_MULPS256: return GSBI_IX86_BUILTIN_MULPS256;
    case IX86_BUILTIN_ORPD256:  return GSBI_IX86_BUILTIN_ORPD256;
    case IX86_BUILTIN_ORPS256:  return GSBI_IX86_BUILTIN_ORPS256;
    case IX86_BUILTIN_PD256_PD: return GSBI_IX86_BUILTIN_PD256_PD;
    case IX86_BUILTIN_PD_PD256: return GSBI_IX86_BUILTIN_PD_PD256;
    case IX86_BUILTIN_PS256_PS: return GSBI_IX86_BUILTIN_PS256_PS;
    case IX86_BUILTIN_PS_PS256: return GSBI_IX86_BUILTIN_PS_PS256;
    case IX86_BUILTIN_PTESTC256:        return GSBI_IX86_BUILTIN_PTESTC256;
    case IX86_BUILTIN_PTESTNZC256:      return GSBI_IX86_BUILTIN_PTESTNZC256;
    case IX86_BUILTIN_PTESTZ256:        return GSBI_IX86_BUILTIN_PTESTZ256;
    case IX86_BUILTIN_RCPPS256: return GSBI_IX86_BUILTIN_RCPPS256;
    case IX86_BUILTIN_ROUNDPD256:       return GSBI_IX86_BUILTIN_ROUNDPD256;
    case IX86_BUILTIN_ROUNDPS256:       return GSBI_IX86_BUILTIN_ROUNDPS256;
    case IX86_BUILTIN_RSQRTPS256:       return GSBI_IX86_BUILTIN_RSQRTPS256;
    case IX86_BUILTIN_RSQRTPS_NR256:    return GSBI_IX86_BUILTIN_RSQRTPS_NR256;
    case IX86_BUILTIN_SHUFPD256:        return GSBI_IX86_BUILTIN_SHUFPD256;
    case IX86_BUILTIN_SHUFPS256:        return GSBI_IX86_BUILTIN_SHUFPS256;
    case IX86_BUILTIN_SI256_SI: return GSBI_IX86_BUILTIN_SI256_SI;
    case IX86_BUILTIN_SI_SI256: return GSBI_IX86_BUILTIN_SI_SI256;
    case IX86_BUILTIN_SQRTPD256:        return GSBI_IX86_BUILTIN_SQRTPD256;
    case IX86_BUILTIN_SQRTPS256:        return GSBI_IX86_BUILTIN_SQRTPS256;
    case IX86_BUILTIN_SQRTPS_NR256:     return GSBI_IX86_BUILTIN_SQRTPS_NR256;
    case IX86_BUILTIN_STOREDQU256:      return GSBI_IX86_BUILTIN_STOREDQU256;
    case IX86_BUILTIN_STOREUPD256:      return GSBI_IX86_BUILTIN_STOREUPD256;
    case IX86_BUILTIN_STOREUPS256:      return GSBI_IX86_BUILTIN_STOREUPS256;
    case IX86_BUILTIN_SUBPD256: return GSBI_IX86_BUILTIN_SUBPD256;
    case IX86_BUILTIN_SUBPS256: return GSBI_IX86_BUILTIN_SUBPS256;
    case IX86_BUILTIN_UNPCKHPD256:      return GSBI_IX86_BUILTIN_UNPCKHPD256;
    case IX86_BUILTIN_UNPCKHPS256:      return GSBI_IX86_BUILTIN_UNPCKHPS256;
    case IX86_BUILTIN_UNPCKLPD256:      return GSBI_IX86_BUILTIN_UNPCKLPD256;
    case IX86_BUILTIN_UNPCKLPS256:      return GSBI_IX86_BUILTIN_UNPCKLPS256;
    case IX86_BUILTIN_VBROADCASTPD256:  return GSBI_IX86_BUILTIN_VBROADCASTPD256;
    case IX86_BUILTIN_VBROADCASTPS256:  return GSBI_IX86_BUILTIN_VBROADCASTPS256;
    case IX86_BUILTIN_VBROADCASTSD256:  return GSBI_IX86_BUILTIN_VBROADCASTSD256;
    case IX86_BUILTIN_VBROADCASTSS:     return GSBI_IX86_BUILTIN_VBROADCASTSS;
    case IX86_BUILTIN_VBROADCASTSS256:  return GSBI_IX86_BUILTIN_VBROADCASTSS256;
    case IX86_BUILTIN_EXTRACTF128PD256: return GSBI_IX86_BUILTIN_EXTRACTF128PD256;
    case IX86_BUILTIN_EXTRACTF128PS256: return GSBI_IX86_BUILTIN_EXTRACTF128PS256;
    case IX86_BUILTIN_EXTRACTF128SI256: return GSBI_IX86_BUILTIN_EXTRACTF128SI256;
    case IX86_BUILTIN_VINSERTF128PD256: return GSBI_IX86_BUILTIN_VINSERTF128PD256;
    case IX86_BUILTIN_VINSERTF128PS256: return GSBI_IX86_BUILTIN_VINSERTF128PS256;
    case IX86_BUILTIN_VINSERTF128SI256: return GSBI_IX86_BUILTIN_VINSERTF128SI256;
    case IX86_BUILTIN_VPERM2F128PD256:  return GSBI_IX86_BUILTIN_VPERM2F128PD256;
    case IX86_BUILTIN_VPERM2F128PS256:  return GSBI_IX86_BUILTIN_VPERM2F128PS256;
    case IX86_BUILTIN_VPERM2F128SI256:  return GSBI_IX86_BUILTIN_VPERM2F128SI256;
    case IX86_BUILTIN_VPERMILPD:        return GSBI_IX86_BUILTIN_VPERMILPD;
    case IX86_BUILTIN_VPERMILPD256:     return GSBI_IX86_BUILTIN_VPERMILPD256;
    case IX86_BUILTIN_VPERMILPS:        return GSBI_IX86_BUILTIN_VPERMILPS;
    case IX86_BUILTIN_VPERMILPS256:     return GSBI_IX86_BUILTIN_VPERMILPS256;
    case IX86_BUILTIN_VPERMILVARPD:     return GSBI_IX86_BUILTIN_VPERMILVARPD;
    case IX86_BUILTIN_VPERMILVARPD256:  return GSBI_IX86_BUILTIN_VPERMILVARPD256;
    case IX86_BUILTIN_VPERMILVARPS:     return GSBI_IX86_BUILTIN_VPERMILVARPS;
    case IX86_BUILTIN_VPERMILVARPS256:  return GSBI_IX86_BUILTIN_VPERMILVARPS256;
    case IX86_BUILTIN_VTESTCPD: return GSBI_IX86_BUILTIN_VTESTCPD;
    case IX86_BUILTIN_VTESTCPD256:      return GSBI_IX86_BUILTIN_VTESTCPD256;
    case IX86_BUILTIN_VTESTCPS: return GSBI_IX86_BUILTIN_VTESTCPS;
    case IX86_BUILTIN_VTESTCPS256:      return GSBI_IX86_BUILTIN_VTESTCPS256;
    case IX86_BUILTIN_VTESTNZCPD:       return GSBI_IX86_BUILTIN_VTESTNZCPD;
    case IX86_BUILTIN_VTESTNZCPD256:    return GSBI_IX86_BUILTIN_VTESTNZCPD256;
    case IX86_BUILTIN_VTESTNZCPS:       return GSBI_IX86_BUILTIN_VTESTNZCPS;
    case IX86_BUILTIN_VTESTNZCPS256:    return GSBI_IX86_BUILTIN_VTESTNZCPS256;
    case IX86_BUILTIN_VTESTZPD: return GSBI_IX86_BUILTIN_VTESTZPD;
    case IX86_BUILTIN_VTESTZPD256:      return GSBI_IX86_BUILTIN_VTESTZPD256;
    case IX86_BUILTIN_VTESTZPS: return GSBI_IX86_BUILTIN_VTESTZPS;
    case IX86_BUILTIN_VTESTZPS256:      return GSBI_IX86_BUILTIN_VTESTZPS256;
    case IX86_BUILTIN_VZEROALL: return GSBI_IX86_BUILTIN_VZEROALL;
    case IX86_BUILTIN_VZEROUPPER:       return GSBI_IX86_BUILTIN_VZEROUPPER;
    case IX86_BUILTIN_XORPD256: return GSBI_IX86_BUILTIN_XORPD256;
    case IX86_BUILTIN_XORPS256: return GSBI_IX86_BUILTIN_XORPS256;

    /* FMA4 intrinsics */
    case IX86_BUILTIN_VFMADDPD: return GSBI_IX86_BUILTIN_VFMADDPD;
    case IX86_BUILTIN_VFMADDPD256:      return GSBI_IX86_BUILTIN_VFMADDPD256;
    case IX86_BUILTIN_VFMADDPS: return GSBI_IX86_BUILTIN_VFMADDPS;
    case IX86_BUILTIN_VFMADDPS256:      return GSBI_IX86_BUILTIN_VFMADDPS256;
    case IX86_BUILTIN_VFMADDSD: return GSBI_IX86_BUILTIN_VFMADDSD;
    case IX86_BUILTIN_VFMADDSS: return GSBI_IX86_BUILTIN_VFMADDSS;
    case IX86_BUILTIN_VFMADDSUBPD:      return GSBI_IX86_BUILTIN_VFMADDSUBPD;
    case IX86_BUILTIN_VFMADDSUBPD256:   return GSBI_IX86_BUILTIN_VFMADDSUBPD256;
    case IX86_BUILTIN_VFMADDSUBPS:      return GSBI_IX86_BUILTIN_VFMADDSUBPS;
    case IX86_BUILTIN_VFMADDSUBPS256:   return GSBI_IX86_BUILTIN_VFMADDSUBPS256;
    case IX86_BUILTIN_VFMSUBADDPD:      return GSBI_IX86_BUILTIN_VFMSUBADDPD;
    case IX86_BUILTIN_VFMSUBADDPD256:   return GSBI_IX86_BUILTIN_VFMSUBADDPD256;
    case IX86_BUILTIN_VFMSUBADDPS:      return GSBI_IX86_BUILTIN_VFMSUBADDPS;
    case IX86_BUILTIN_VFMSUBADDPS256:   return GSBI_IX86_BUILTIN_VFMSUBADDPS256;
    case IX86_BUILTIN_VFMSUBPD: return GSBI_IX86_BUILTIN_VFMSUBPD;
    case IX86_BUILTIN_VFMSUBPD256:      return GSBI_IX86_BUILTIN_VFMSUBPD256;
    case IX86_BUILTIN_VFMSUBPS: return GSBI_IX86_BUILTIN_VFMSUBPS;
    case IX86_BUILTIN_VFMSUBPS256:      return GSBI_IX86_BUILTIN_VFMSUBPS256;
    case IX86_BUILTIN_VFMSUBSD: return GSBI_IX86_BUILTIN_VFMSUBSD;
    case IX86_BUILTIN_VFMSUBSS: return GSBI_IX86_BUILTIN_VFMSUBSS;
    case IX86_BUILTIN_VFNMADDPD:        return GSBI_IX86_BUILTIN_VFNMADDPD;
    case IX86_BUILTIN_VFNMADDPD256:     return GSBI_IX86_BUILTIN_VFNMADDPD256;
    case IX86_BUILTIN_VFNMADDPS:        return GSBI_IX86_BUILTIN_VFNMADDPS;
    case IX86_BUILTIN_VFNMADDPS256:     return GSBI_IX86_BUILTIN_VFNMADDPS256;
    case IX86_BUILTIN_VFNMADDSD:        return GSBI_IX86_BUILTIN_VFNMADDSD;
    case IX86_BUILTIN_VFNMADDSS:        return GSBI_IX86_BUILTIN_VFNMADDSS;
    case IX86_BUILTIN_VFNMSUBPD:        return GSBI_IX86_BUILTIN_VFNMSUBPD;
    case IX86_BUILTIN_VFNMSUBPD256:     return GSBI_IX86_BUILTIN_VFNMSUBPD256;
    case IX86_BUILTIN_VFNMSUBPS:        return GSBI_IX86_BUILTIN_VFNMSUBPS;
    case IX86_BUILTIN_VFNMSUBPS256:     return GSBI_IX86_BUILTIN_VFNMSUBPS256;
    case IX86_BUILTIN_VFNMSUBSD:        return GSBI_IX86_BUILTIN_VFNMSUBSD;
    case IX86_BUILTIN_VFNMSUBSS:        return GSBI_IX86_BUILTIN_VFNMSUBSS;

    /* XOP intrinsics */
    case IX86_BUILTIN_VFRCZPD:  return GSBI_IX86_BUILTIN_VFRCZPD;
    case IX86_BUILTIN_VFRCZPD256:       return GSBI_IX86_BUILTIN_VFRCZPD256;
    case IX86_BUILTIN_VFRCZPS:  return GSBI_IX86_BUILTIN_VFRCZPS;
    case IX86_BUILTIN_VFRCZPS256:       return GSBI_IX86_BUILTIN_VFRCZPS256;
    case IX86_BUILTIN_VFRCZSD:  return GSBI_IX86_BUILTIN_VFRCZSD;
    case IX86_BUILTIN_VFRCZSS:  return GSBI_IX86_BUILTIN_VFRCZSS;
    case IX86_BUILTIN_VPCMOV:   return GSBI_IX86_BUILTIN_VPCMOV;
    case IX86_BUILTIN_VPCMOV256:        return GSBI_IX86_BUILTIN_VPCMOV256;
    case IX86_BUILTIN_VPCMOV_V16HI256:  return GSBI_IX86_BUILTIN_VPCMOV_V16HI256;
    case IX86_BUILTIN_VPCMOV_V16QI:     return GSBI_IX86_BUILTIN_VPCMOV_V16QI;
    case IX86_BUILTIN_VPCMOV_V2DF:      return GSBI_IX86_BUILTIN_VPCMOV_V2DF;
    case IX86_BUILTIN_VPCMOV_V2DI:      return GSBI_IX86_BUILTIN_VPCMOV_V2DI;
    case IX86_BUILTIN_VPCMOV_V32QI256:  return GSBI_IX86_BUILTIN_VPCMOV_V32QI256;
    case IX86_BUILTIN_VPCMOV_V4DF256:   return GSBI_IX86_BUILTIN_VPCMOV_V4DF256;
    case IX86_BUILTIN_VPCMOV_V4DI256:   return GSBI_IX86_BUILTIN_VPCMOV_V4DI256;
    case IX86_BUILTIN_VPCMOV_V4SF:      return GSBI_IX86_BUILTIN_VPCMOV_V4SF;
    case IX86_BUILTIN_VPCMOV_V4SI:      return GSBI_IX86_BUILTIN_VPCMOV_V4SI;
    case IX86_BUILTIN_VPCMOV_V8HI:      return GSBI_IX86_BUILTIN_VPCMOV_V8HI;
    case IX86_BUILTIN_VPCMOV_V8SF256:   return GSBI_IX86_BUILTIN_VPCMOV_V8SF256;
    case IX86_BUILTIN_VPCMOV_V8SI256:   return GSBI_IX86_BUILTIN_VPCMOV_V8SI256;
    case IX86_BUILTIN_VPCOMEQB: return GSBI_IX86_BUILTIN_VPCOMEQB;
    case IX86_BUILTIN_VPCOMEQD: return GSBI_IX86_BUILTIN_VPCOMEQD;
    case IX86_BUILTIN_VPCOMEQQ: return GSBI_IX86_BUILTIN_VPCOMEQQ;
    case IX86_BUILTIN_VPCOMEQUB:        return GSBI_IX86_BUILTIN_VPCOMEQUB;
    case IX86_BUILTIN_VPCOMEQUD:        return GSBI_IX86_BUILTIN_VPCOMEQUD;
    case IX86_BUILTIN_VPCOMEQUQ:        return GSBI_IX86_BUILTIN_VPCOMEQUQ;
    case IX86_BUILTIN_VPCOMEQUW:        return GSBI_IX86_BUILTIN_VPCOMEQUW;
    case IX86_BUILTIN_VPCOMEQW: return GSBI_IX86_BUILTIN_VPCOMEQW;
    case IX86_BUILTIN_VPCOMFALSEB:      return GSBI_IX86_BUILTIN_VPCOMFALSEB;
    case IX86_BUILTIN_VPCOMFALSED:      return GSBI_IX86_BUILTIN_VPCOMFALSED;
    case IX86_BUILTIN_VPCOMFALSEQ:      return GSBI_IX86_BUILTIN_VPCOMFALSEQ;
    case IX86_BUILTIN_VPCOMFALSEUB:     return GSBI_IX86_BUILTIN_VPCOMFALSEUB;
    case IX86_BUILTIN_VPCOMFALSEUD:     return GSBI_IX86_BUILTIN_VPCOMFALSEUD;
    case IX86_BUILTIN_VPCOMFALSEUQ:     return GSBI_IX86_BUILTIN_VPCOMFALSEUQ;
    case IX86_BUILTIN_VPCOMFALSEUW:     return GSBI_IX86_BUILTIN_VPCOMFALSEUW;
    case IX86_BUILTIN_VPCOMFALSEW:      return GSBI_IX86_BUILTIN_VPCOMFALSEW;
    case IX86_BUILTIN_VPCOMGEB: return GSBI_IX86_BUILTIN_VPCOMGEB;
    case IX86_BUILTIN_VPCOMGED: return GSBI_IX86_BUILTIN_VPCOMGED;
    case IX86_BUILTIN_VPCOMGEQ: return GSBI_IX86_BUILTIN_VPCOMGEQ;
    case IX86_BUILTIN_VPCOMGEUB:        return GSBI_IX86_BUILTIN_VPCOMGEUB;
    case IX86_BUILTIN_VPCOMGEUD:        return GSBI_IX86_BUILTIN_VPCOMGEUD;
    case IX86_BUILTIN_VPCOMGEUQ:        return GSBI_IX86_BUILTIN_VPCOMGEUQ;
    case IX86_BUILTIN_VPCOMGEUW:        return GSBI_IX86_BUILTIN_VPCOMGEUW;
    case IX86_BUILTIN_VPCOMGEW: return GSBI_IX86_BUILTIN_VPCOMGEW;
    case IX86_BUILTIN_VPCOMGTB: return GSBI_IX86_BUILTIN_VPCOMGTB;
    case IX86_BUILTIN_VPCOMGTD: return GSBI_IX86_BUILTIN_VPCOMGTD;
    case IX86_BUILTIN_VPCOMGTQ: return GSBI_IX86_BUILTIN_VPCOMGTQ;
    case IX86_BUILTIN_VPCOMGTUB:        return GSBI_IX86_BUILTIN_VPCOMGTUB;
    case IX86_BUILTIN_VPCOMGTUD:        return GSBI_IX86_BUILTIN_VPCOMGTUD;
    case IX86_BUILTIN_VPCOMGTUQ:        return GSBI_IX86_BUILTIN_VPCOMGTUQ;
    case IX86_BUILTIN_VPCOMGTUW:        return GSBI_IX86_BUILTIN_VPCOMGTUW;
    case IX86_BUILTIN_VPCOMGTW: return GSBI_IX86_BUILTIN_VPCOMGTW;
    case IX86_BUILTIN_VPCOMLEB: return GSBI_IX86_BUILTIN_VPCOMLEB;
    case IX86_BUILTIN_VPCOMLED: return GSBI_IX86_BUILTIN_VPCOMLED;
    case IX86_BUILTIN_VPCOMLEQ: return GSBI_IX86_BUILTIN_VPCOMLEQ;
    case IX86_BUILTIN_VPCOMLEUB:        return GSBI_IX86_BUILTIN_VPCOMLEUB;
    case IX86_BUILTIN_VPCOMLEUD:        return GSBI_IX86_BUILTIN_VPCOMLEUD;
    case IX86_BUILTIN_VPCOMLEUQ:        return GSBI_IX86_BUILTIN_VPCOMLEUQ;
    case IX86_BUILTIN_VPCOMLEUW:        return GSBI_IX86_BUILTIN_VPCOMLEUW;
    case IX86_BUILTIN_VPCOMLEW: return GSBI_IX86_BUILTIN_VPCOMLEW;
    case IX86_BUILTIN_VPCOMLTB: return GSBI_IX86_BUILTIN_VPCOMLTB;
    case IX86_BUILTIN_VPCOMLTD: return GSBI_IX86_BUILTIN_VPCOMLTD;
    case IX86_BUILTIN_VPCOMLTQ: return GSBI_IX86_BUILTIN_VPCOMLTQ;
    case IX86_BUILTIN_VPCOMLTUB:        return GSBI_IX86_BUILTIN_VPCOMLTUB;
    case IX86_BUILTIN_VPCOMLTUD:        return GSBI_IX86_BUILTIN_VPCOMLTUD;
    case IX86_BUILTIN_VPCOMLTUQ:        return GSBI_IX86_BUILTIN_VPCOMLTUQ;
    case IX86_BUILTIN_VPCOMLTUW:        return GSBI_IX86_BUILTIN_VPCOMLTUW;
    case IX86_BUILTIN_VPCOMLTW: return GSBI_IX86_BUILTIN_VPCOMLTW;
    case IX86_BUILTIN_VPCOMNEB: return GSBI_IX86_BUILTIN_VPCOMNEB;
    case IX86_BUILTIN_VPCOMNED: return GSBI_IX86_BUILTIN_VPCOMNED;
    case IX86_BUILTIN_VPCOMNEQ: return GSBI_IX86_BUILTIN_VPCOMNEQ;
    case IX86_BUILTIN_VPCOMNEUB:        return GSBI_IX86_BUILTIN_VPCOMNEUB;
    case IX86_BUILTIN_VPCOMNEUD:        return GSBI_IX86_BUILTIN_VPCOMNEUD;
    case IX86_BUILTIN_VPCOMNEUQ:        return GSBI_IX86_BUILTIN_VPCOMNEUQ;
    case IX86_BUILTIN_VPCOMNEUW:        return GSBI_IX86_BUILTIN_VPCOMNEUW;
    case IX86_BUILTIN_VPCOMNEW: return GSBI_IX86_BUILTIN_VPCOMNEW;
    case IX86_BUILTIN_VPCOMTRUEB:       return GSBI_IX86_BUILTIN_VPCOMTRUEB;
    case IX86_BUILTIN_VPCOMTRUED:       return GSBI_IX86_BUILTIN_VPCOMTRUED;
    case IX86_BUILTIN_VPCOMTRUEQ:       return GSBI_IX86_BUILTIN_VPCOMTRUEQ;
    case IX86_BUILTIN_VPCOMTRUEUB:      return GSBI_IX86_BUILTIN_VPCOMTRUEUB;
    case IX86_BUILTIN_VPCOMTRUEUD:      return GSBI_IX86_BUILTIN_VPCOMTRUEUD;
    case IX86_BUILTIN_VPCOMTRUEUQ:      return GSBI_IX86_BUILTIN_VPCOMTRUEUQ;
    case IX86_BUILTIN_VPCOMTRUEUW:      return GSBI_IX86_BUILTIN_VPCOMTRUEUW;
    case IX86_BUILTIN_VPCOMTRUEW:       return GSBI_IX86_BUILTIN_VPCOMTRUEW;
    case IX86_BUILTIN_VPHADDBD: return GSBI_IX86_BUILTIN_VPHADDBD;
    case IX86_BUILTIN_VPHADDBQ: return GSBI_IX86_BUILTIN_VPHADDBQ;
    case IX86_BUILTIN_VPHADDBW: return GSBI_IX86_BUILTIN_VPHADDBW;
    case IX86_BUILTIN_VPHADDDQ: return GSBI_IX86_BUILTIN_VPHADDDQ;
    case IX86_BUILTIN_VPHADDUBD:        return GSBI_IX86_BUILTIN_VPHADDUBD;
    case IX86_BUILTIN_VPHADDUBQ:        return GSBI_IX86_BUILTIN_VPHADDUBQ;
    case IX86_BUILTIN_VPHADDUBW:        return GSBI_IX86_BUILTIN_VPHADDUBW;
    case IX86_BUILTIN_VPHADDUDQ:        return GSBI_IX86_BUILTIN_VPHADDUDQ;
    case IX86_BUILTIN_VPHADDUWD:        return GSBI_IX86_BUILTIN_VPHADDUWD;
    case IX86_BUILTIN_VPHADDUWQ:        return GSBI_IX86_BUILTIN_VPHADDUWQ;
    case IX86_BUILTIN_VPHADDWD: return GSBI_IX86_BUILTIN_VPHADDWD;
    case IX86_BUILTIN_VPHADDWQ: return GSBI_IX86_BUILTIN_VPHADDWQ;
    case IX86_BUILTIN_VPHSUBBW: return GSBI_IX86_BUILTIN_VPHSUBBW;
    case IX86_BUILTIN_VPHSUBDQ: return GSBI_IX86_BUILTIN_VPHSUBDQ;
    case IX86_BUILTIN_VPHSUBWD: return GSBI_IX86_BUILTIN_VPHSUBWD;
    case IX86_BUILTIN_VPMACSDD: return GSBI_IX86_BUILTIN_VPMACSDD;
    case IX86_BUILTIN_VPMACSDQH:        return GSBI_IX86_BUILTIN_VPMACSDQH;
    case IX86_BUILTIN_VPMACSDQL:        return GSBI_IX86_BUILTIN_VPMACSDQL;
    case IX86_BUILTIN_VPMACSSDD:        return GSBI_IX86_BUILTIN_VPMACSSDD;
    case IX86_BUILTIN_VPMACSSDQH:       return GSBI_IX86_BUILTIN_VPMACSSDQH;
    case IX86_BUILTIN_VPMACSSDQL:       return GSBI_IX86_BUILTIN_VPMACSSDQL;
    case IX86_BUILTIN_VPMACSSWD:        return GSBI_IX86_BUILTIN_VPMACSSWD;
    case IX86_BUILTIN_VPMACSSWW:        return GSBI_IX86_BUILTIN_VPMACSSWW;
    case IX86_BUILTIN_VPMACSWD: return GSBI_IX86_BUILTIN_VPMACSWD;
    case IX86_BUILTIN_VPMACSWW: return GSBI_IX86_BUILTIN_VPMACSWW;
    case IX86_BUILTIN_VPMADCSSWD:       return GSBI_IX86_BUILTIN_VPMADCSSWD;
    case IX86_BUILTIN_VPMADCSWD:        return GSBI_IX86_BUILTIN_VPMADCSWD;
    case IX86_BUILTIN_VPPERM:   return GSBI_IX86_BUILTIN_VPPERM;
    case IX86_BUILTIN_VPROTB:   return GSBI_IX86_BUILTIN_VPROTB;
    case IX86_BUILTIN_VPROTB_IMM:       return GSBI_IX86_BUILTIN_VPROTB_IMM;
    case IX86_BUILTIN_VPROTD:   return GSBI_IX86_BUILTIN_VPROTD;
    case IX86_BUILTIN_VPROTD_IMM:       return GSBI_IX86_BUILTIN_VPROTD_IMM;
    case IX86_BUILTIN_VPROTQ:   return GSBI_IX86_BUILTIN_VPROTQ;
    case IX86_BUILTIN_VPROTQ_IMM:       return GSBI_IX86_BUILTIN_VPROTQ_IMM;
    case IX86_BUILTIN_VPROTW:   return GSBI_IX86_BUILTIN_VPROTW;
    case IX86_BUILTIN_VPROTW_IMM:       return GSBI_IX86_BUILTIN_VPROTW_IMM;
    case IX86_BUILTIN_VPSHAB:   return GSBI_IX86_BUILTIN_VPSHAB;
    case IX86_BUILTIN_VPSHAD:   return GSBI_IX86_BUILTIN_VPSHAD;
    case IX86_BUILTIN_VPSHAQ:   return GSBI_IX86_BUILTIN_VPSHAQ;
    case IX86_BUILTIN_VPSHAW:   return GSBI_IX86_BUILTIN_VPSHAW;
    case IX86_BUILTIN_VPSHLB:   return GSBI_IX86_BUILTIN_VPSHLB;
    case IX86_BUILTIN_VPSHLD:   return GSBI_IX86_BUILTIN_VPSHLD;
    case IX86_BUILTIN_VPSHLQ:   return GSBI_IX86_BUILTIN_VPSHLQ;
    case IX86_BUILTIN_VPSHLW:   return GSBI_IX86_BUILTIN_VPSHLW;

    case IX86_BUILTIN_MAX: return GSBI_IX86_BUILTIN_MAX;
  }
  gcc_assert (0);
  return (gsbi_ts_t) 0;
}
#endif

static inline gs_omp_clause_code_t
gcc_omp_clause_code2gs_occ (enum omp_clause_code c)
{
  switch (c)
  {
    case OMP_CLAUSE_ERROR: return GS_OMP_CLAUSE_ERROR;
    case OMP_CLAUSE_PRIVATE: return GS_OMP_CLAUSE_PRIVATE;
    case OMP_CLAUSE_SHARED: return GS_OMP_CLAUSE_SHARED;
    case OMP_CLAUSE_FIRSTPRIVATE: return GS_OMP_CLAUSE_FIRSTPRIVATE;
    case OMP_CLAUSE_LASTPRIVATE: return GS_OMP_CLAUSE_LASTPRIVATE;
    case OMP_CLAUSE_REDUCTION: return GS_OMP_CLAUSE_REDUCTION;
    case OMP_CLAUSE_COPYIN: return GS_OMP_CLAUSE_COPYIN;
    case OMP_CLAUSE_COPYPRIVATE: return GS_OMP_CLAUSE_COPYPRIVATE;
    case OMP_CLAUSE_IF: return GS_OMP_CLAUSE_IF;
    case OMP_CLAUSE_NUM_THREADS: return GS_OMP_CLAUSE_NUM_THREADS;
    case OMP_CLAUSE_SCHEDULE: return GS_OMP_CLAUSE_SCHEDULE;
    case OMP_CLAUSE_NOWAIT: return GS_OMP_CLAUSE_NOWAIT;
    case OMP_CLAUSE_ORDERED: return GS_OMP_CLAUSE_ORDERED;
    case OMP_CLAUSE_DEFAULT: return GS_OMP_CLAUSE_DEFAULT;
  }
  gcc_assert (0);
  return (gsbi_ts_t) 0;
}

static inline gs_omp_clause_default_kind_t
gcc_omp_clause_default_kind2gs_ocdk (enum omp_clause_default_kind k)
{
  switch (k)
  {
    case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
         return GS_OMP_CLAUSE_DEFAULT_UNSPECIFIED;
    case OMP_CLAUSE_DEFAULT_SHARED: return GS_OMP_CLAUSE_DEFAULT_SHARED;
    case OMP_CLAUSE_DEFAULT_NONE: return GS_OMP_CLAUSE_DEFAULT_NONE;
    case OMP_CLAUSE_DEFAULT_PRIVATE: return GS_OMP_CLAUSE_DEFAULT_PRIVATE;
  }
  gcc_assert (0);
  return (gsbi_ts_t) 0;
}

static inline gs_omp_clause_schedule_kind_t
gcc_omp_clause_schedule_kind2gs_ocsk (enum omp_clause_schedule_kind k)
{
  switch (k)
  {
    case OMP_CLAUSE_SCHEDULE_STATIC: return GS_OMP_CLAUSE_SCHEDULE_STATIC;
    case OMP_CLAUSE_SCHEDULE_DYNAMIC: return GS_OMP_CLAUSE_SCHEDULE_DYNAMIC;
    case OMP_CLAUSE_SCHEDULE_GUIDED: return GS_OMP_CLAUSE_SCHEDULE_GUIDED;
    case OMP_CLAUSE_SCHEDULE_RUNTIME: return GS_OMP_CLAUSE_SCHEDULE_RUNTIME;
  }
  gcc_assert (0);
  return (gsbi_ts_t) 0;
}

/******************************************************************************/

unsigned int gspin_label_count = 0;

/* C++: Are we processing the final global namespace? */
int processing_global_namespace = 0;
/* Function prototypes */
gs_t gs_x (tree node);
gs_t gs_x_func_decl (tree node);
void gspin_gxx_emits_decl (tree t);
void gspin_gxx_emits_asm (tree str);
void gspin_write (void);
void gspin_init (void);
void gspin_init_global_trees_list (void);
int gspin_invoked (tree t);
void gs_set_flag_value (tree t, unsigned int flag, bool value);
void dump_statement_list (tree t);
static void gspin_add_weak (tree decl, gs_t decl_node);

int    gs_argc;
char **gs_argv;

static gs_t program   = (gs_t) NULL, 
            arg_list  = (gs_t) NULL,
            global_trees_list  = (gs_t) NULL,
            integer_types_list  = (gs_t) NULL,
	    gxx_emitted_decls_dot = (gs_t) NULL,
	    gxx_emitted_asms_dot = (gs_t) NULL,
	    weak_decls_dot = (gs_t) NULL,
	    gs_program_flags = (gs_t) NULL;

/* dot to insert new declaration tree */
static gs_t program_decls_dot = (gs_t) NULL;

void gspin_write (void) 
{
  /* Bug 10172: If errors have occurred, do not write out the .spin output file. */
  if (!errorcount)
    gs_write ((gs_string_t) spin_file_name);
}

/* Return 1 if gspin has been invoked already and this node has been
 * processed, else return 0.
 */
int gspin_invoked (tree t)
{
  return program != NULL && ((gs_t)GS_NODE(t) != NULL);
}

/* Set FLAG in gs_t of T with VALUE.
 * It is the caller's responsibility to verify that T has already been
 * translated into gs_t.
 * The caller should also ensure FLAG is a flag in the GS_FLAGS section.
 * The flag is set if the gs_t node already contains flag information.
 * If there is no flag information, the node must be in the early
 * stages of creation, and it is no use setting the flag here.
 */
void gs_set_flag_value (tree t, unsigned int flag, bool value)
{
    gs_t gs_flags = gs_operand((gs_t)GS_NODE(t), GS_FLAGS);
    if (gs_flags == NULL) {
      gs_flags = __gs (IB_BIT_VECTOR);
      gs_set_operand((gs_t) GS_NODE (t), GS_FLAGS, gs_flags);
    }
    if (value)
      _gs_bv (gs_flags, flag, 1);
    else
      _gs_bv_reset (gs_flags, flag);
}

void gs_set_program_flag_value (unsigned int flag, bool value)
{
  gs_t program_flags = gs_operand((gs_t)program, GS_PROGRAM_FLAGS);
  GS_ASSERT(program_flags != NULL,
   ("gs_set_program_flag_value should be called after gspin initialization"));
  if (value)
    _gs_bv (program_flags, flag, 1);
  else
    _gs_bv_reset (program_flags, flag);
}

/* Initialize the gspin dot universe. */
void
gspin_init(void)
{
  gs_t arg, decl_list;
  gs_int_t i;
  gs_t gxx_emitted_decls, gxx_emitted_asms, weak_decls;
  
  GS_ASSERT(program == NULL,
	    ("gspin_init: dot universe already initialized"));

  /* First find out and flag the kind of translation run. */
  if (strcmp ("GNU C++", lang_hooks.name) == 0) {
    language = CPP;
  }
  else if (strcmp ("GNU C", lang_hooks.name) == 0) {
    language = C;
  }

  /*
     Create the dot structure for GS_PROGRAM.
    
                           root dot
			       / \
			      .   weak decls		GS_PROGRAM arg7
                             / \
                            .   program flags		GS_PROGRAM arg6
                           / \
                          .   gxx-emitted asms		GS_PROGRAM arg5
                         / \
                        .   gxx-emitted decls		GS_PROGRAM arg4
                       / \
                      .   program declarations		GS_PROGRAM arg3
                     / \
                    .   integer types list		GS_PROGRAM arg2
                   / \
                  .   global trees list			GS_PROGRAM arg1
                 / \
       GS_PROGRAM   cc1 command line args		GS_PROGRAM arg0
  */

  program = __gs (GS_PROGRAM);

  /* cc1 command line args */
  arg_list  = __gs (EMPTY);
  for (i = gs_argc - 1; i >= 0; i--) {
    arg = __gs (IB_STRING);
    _gs_s (arg, (gs_string_t)gs_argv[i], 1 + strlen (gs_argv[i]));
    arg_list = gs_cons (arg, arg_list);
  }
  gs_set_operand(program, GS_CC1_COMMAND_LINE_ARGS, arg_list);

  /* global trees list will be initialized by gspin_init_global_trees_list */

  /* integer types list will be initialized by gspin_init_global_trees_list */

  /* program declarations */
  decl_list = __gs (EMPTY);
  gs_set_operand(program, GS_PROGRAM_DECLARATIONS, decl_list);
  program_decls_dot = decl_list;

  /* gxx-emitted decls */
  gxx_emitted_decls = __gs (EMPTY);
  gs_set_operand(program, GS_GXX_EMITTED_DECLS, gxx_emitted_decls);
  gxx_emitted_decls_dot = gxx_emitted_decls;

  /* gxx-emitted asms */
  gxx_emitted_asms = __gs (EMPTY);
  gs_set_operand(program, GS_GXX_EMITTED_ASMS, gxx_emitted_asms);
  gxx_emitted_asms_dot = gxx_emitted_asms;

  /* program flags */
  gs_program_flags = __gs(IB_BIT_VECTOR);
  gs_set_operand(program, GS_PROGRAM_FLAGS, gs_program_flags);
  _gs_bv(gs_program_flags, GS_FLAG_ERRNO_MATH, flag_errno_math);

  /* weak decls */
  weak_decls = __gs (EMPTY);
  gs_set_operand(program, GS_WEAK_DECLS, weak_decls);
  weak_decls_dot = weak_decls;

  if ((atexit (gspin_write)) != 0) 
    fprintf (stderr, "gspin_write registration with atexit (3) failed.\n");
}

/* Add the global trees containing common types. */
void
gspin_init_global_trees_list(void)
{
  int i;

  global_trees_list = __gs (EMPTY);
  for (i = TI_MAX - 1; i >= TI_ERROR_MARK; i--) {
#ifdef TARG_X8664
    GS_ASSERT((global_trees[i] != NULL) ||
              (!TARGET_64BIT &&
               (i == TI_VA_LIST_GPR_COUNTER_FIELD ||
                i == TI_VA_LIST_FPR_COUNTER_FIELD)),
              ("gspin_init_global_trees_list: global_tree not initialized"));
#else
    GS_ASSERT((global_trees[i] != NULL) ||
              ((i == TI_VA_LIST_GPR_COUNTER_FIELD ||
                i == TI_VA_LIST_FPR_COUNTER_FIELD)),
              ("gspin_init_global_trees_list: global_tree not initialized"));
#endif
    global_trees_list = gs_cons (gs_x (global_trees [i]), global_trees_list);
  }
  gs_set_operand(program, GS_GLOBAL_TREES_LIST, global_trees_list);

  integer_types_list = __gs (EMPTY);
  for (i = itk_none - 1; i >= itk_char; i--) {
    integer_types_list = gs_cons (gs_x (integer_types [i]), integer_types_list);
  }
  gs_set_operand(program, GS_INTEGER_TYPES_LIST, integer_types_list);
}

/* Add tree T to the program's decls list. */
void
gspin (tree t) 
{
  gs_t decl, decl_list;

  /* C++ should call gs_x_func_decl to translate FUNCTION_DECL. */
  if (CPR()) {
    GS_ASSERT(t == NULL ||
	      TREE_CODE(t) != FUNCTION_DECL,
	      ("gspin: C++ should not call gspin for FUNCTION_DECL"));
  }
  
  GS_ASSERT(program != NULL, ("gspin: gspin not initialized"));

  decl = gs_x_func_decl(t);

  if (gs_code(program_decls_dot) == EMPTY) {
    _gs_code(program_decls_dot, CONS);
    gs_set_operand(program_decls_dot, 0, decl);
    gs_set_operand(program_decls_dot, 1, __gs(EMPTY));
    return;
  }
  decl_list = gs_cons(decl, gs_operand(program_decls_dot, 1));
  gs_set_operand(program_decls_dot, 1, decl_list);
  program_decls_dot = decl_list;
}

/* Set to 1 by the name-mangling code if the decl cannot be reliably mangled.
 * This happens when some of the info needed for mangling is missing. */
int cannot_mangle_name;

/* Tell gs_x_1 whether or not to fully translate FUNCTION_DECL. */
static int translate_func_decl = 0;

/* The sequence number identifies the tree nodes that are being translated in
 * the current pass.  Prevents infinite recursion when tree nodes point to each
 * other. */
static HOST_WIDE_INT sequence_num = 0;

/* SEQ_NUM is the sequence number for this translation pass. */
static gs_t
gs_x_1 (tree t, HOST_WIDE_INT seq_num)
{
  enum machine_mode mode;
  enum tree_code_class class;
  enum tree_code tcode;
  int i;
  int translate_this_func_decl = translate_func_decl;
  gs_t flags, code_class;

  /* Don't translate FUNCTION_DECLs fully when recursing. */
  translate_func_decl = 0;
  
  if (t == (tree) NULL)
    return (gs_t) NULL;
 
  /* Do not process the ERROR_MARK code any further. */
  if (TREE_CODE (t) == ERROR_MARK)
    return __gs (GS_ERROR_MARK);

  tcode = TREE_CODE(t);

  if ((gs_t) GS_NODE (t) != (gs_t) NULL) {

    /* A caller was in the process of translating this tree node.  Don't
     * translate it again here, in order to prevent infinite recursion. */
    if (GS_SEQUENCE_NUM(t) == seq_num) {
      GS_ASSERT(GS_NODE(t) != NULL, ("gs_x_1: GS_NODE null"));
      return (gs_t) GS_NODE(t);
    }

    /* Handle FUNCTION_DECL with extreme care.  It is possible that GCC has not
     * finished generating the function body (DECL_SAVED_TREE).  This happens,
     * for example, when we are called to translate the FUNCTION_DECL from a
     * CALL_EXPR, or when we are chasing down a TREE_CHAIN containing the
     * FUNCTION_DECL.  The solution is to translate the FUNCTION_DECL in two
     * steps:
     *   1)  Body not yet generated by GCC:
     *	     Partially construct the gs node for the FUNCTION_DECL so that the
     *	     node ID can be returned to the caller.  The partially-constructed
     *	     node has minimal info to keep gspin from breaking.
     *   2)  Body fully generated by GCC:
     *	     Finish constructing the rest of the gs node.  This step should be
     *	     performed *once* from a single GCC call point.  Once the node is
     *	     fully translated, don't translate it again because GCC may later
     *	     delete info from the FUNCTION_DECL, such as the DECL_SAVED_TREE. */

    if (TREE_CODE(t) == FUNCTION_DECL) {
      /* bug 11311: For C++, it is not always possible to determine if a
       * function will need a body or not. So previously we may have fully
       * translated a function without its body. If we have
       * "translate_this_func_decl" set, it means this is the proper time
       * to expand this function, and it may now have a function body --
       * whatever is the state of the function_decl, we want to get it now. */
      if (CPR() && translate_this_func_decl)
        goto REVISIT;
      else if (CR()) {
        /* Don't re-translate the function if it was fully translated before. */
        if (FULLY_TRANSLATED_TO_GS(t)) {
          /* This could be an inline function that we have already output
           * but for which we now see a non-inline definition.  In this case,
           * we want to translate again to override the previous definition. */
          if (TRANSLATED_TO_GS_AS_INLINE(t)
              && ! DECL_INLINE (t) && translate_this_func_decl)
            TRANSLATED_TO_GS_AS_INLINE(t) = 0;
          else
            return GS_NODE(t);
	}

        /* Node is partially-translated. */
        if (translate_this_func_decl)
          goto REVISIT;
      }

      return GS_NODE(t);
    }

    if (tcode == VAR_DECL &&
	DECL_INITIAL (t) != NULL &&
	DECL_INITIAL (t) != t &&
        gs_operand((gs_t) GS_NODE(t), GS_DECL_INITIAL) == NULL) {
      goto REVISIT;
    }

    if (tcode == VAR_DECL) {
      gs_t gs_flags = gs_operand((gs_t)GS_NODE(t), GS_FLAGS);
      if (gs_bv(gs_flags, GS_DECL_EXTERNAL) && !DECL_EXTERNAL(t))
	goto REVISIT;		/* bug 10324 */
      if (!gs_bv(gs_flags, GS_TREE_STATIC) && TREE_STATIC(t))
	goto REVISIT;		/* bug 10324 */
    }

    if (TREE_CODE_CLASS(tcode) == tcc_type &&
	(tcode == BOOLEAN_TYPE ||
	 tcode == COMPLEX_TYPE || tcode == INTEGER_TYPE ||
	 tcode == REAL_TYPE)) { /* bug 10352 */
      flags = gs_operand((gs_t)GS_NODE(t), GS_FLAGS);
      goto REVISIT2;
    }

    if (tcode == NAMESPACE_DECL && processing_global_namespace) {
      processing_global_namespace = 0;
      goto REVISIT;
    }

    if ((tcode == ARRAY_TYPE || tcode == RECORD_TYPE ||
         tcode == UNION_TYPE || tcode == QUAL_UNION_TYPE) && 
	COMPLETE_TYPE_P(t) &&
        gs_operand((gs_t)GS_NODE(t), GS_TYPE_SIZE) == NULL)
      goto REVISIT;

    /* bug 14420 */
    if (tcode == RECORD_TYPE &&
        TYPE_FIELDS(t) &&
        gs_operand((gs_t)GS_NODE(t), GS_TYPE_FIELDS) == NULL)
      goto REVISIT;

    if (tcode == RECORD_TYPE &&
        CLASS_TYPE_P(t) &&
        CLASSTYPE_TYPEINFO_VAR(t) &&
        gs_operand((gs_t) GS_NODE(t), GS_CLASSTYPE_TYPEINFO_VAR) == NULL)
      goto REVISIT;

    return (gs_t) GS_NODE (t);
  }

  /* Create the node that will represent this tree node. 
   * Open64, we use long to represent the pointer */
  TREE_TO_TRANSLATED_GS (t) = (unsigned long)__gs(gcc2gs (TREE_CODE (t)));

  /* Argument 0 is the name of the class: */
  class = TREE_CODE_CLASS (TREE_CODE (t));
  code_class = __gs (GS_TCC);
  _gs_b (code_class, gcc_class2gs_class (class));
  gs_set_operand(GS_NODE(t), 0, code_class);

  flags = __gs (IB_BIT_VECTOR);
  gs_set_operand((gs_t) GS_NODE (t), GS_FLAGS, flags);

  /* Always translate the type because, if it is a FUNCTION_DECL, the front-end
   * may need the type info even if the rest of the FUNCTION_DECL is never
   * translated.  (This occurs for a C++ function that is called but does not
   * appear in the source.  Bug 11123.) */
  gs_set_operand((gs_t)GS_NODE(t), GS_TREE_TYPE, gs_x_1(TREE_TYPE(t), seq_num));

  /* For the same reason, translate the assembler name.  Create the assembler
   * name now if it doesn't exist.  Example is __comp_ctor in bug 11123. */
  if (CPR() &&
      TREE_CODE(t) == FUNCTION_DECL) {
    if (!DECL_ASSEMBLER_NAME_SET_P(t)) {
      cannot_mangle_name = 0;
      lang_hooks.mangle_decl(t);
      if (cannot_mangle_name) {
	/* The decl cannot be reliably mangled.  Delete the possibly wrong
	 * result. */
	SET_DECL_ASSEMBLER_NAME(t, NULL);
      }
    }
    gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ASSEMBLER_NAME,
		   gs_x_1(DECL_ASSEMBLER_NAME(t), seq_num));
#ifdef FE_GNU_4_2_0
    /* Bug 5737: For C++ (triggered by C++ OpenMP bug), also traverse the
     * chain node. Otherwise, if TYPE_METHODS initially has a function
     * without a function body (like a maybe_in_charge_constructor), its
     * TREE_CHAIN would be empty, thus causing other methods to go missing. */
    gs_set_operand((gs_t) GS_NODE(t), GS_TREE_CHAIN,
	           gs_x_1(TREE_CHAIN(t), seq_num));
#endif
    _gs_bv(flags, GS_DECL_ASSEMBLER_NAME_SET_P, DECL_ASSEMBLER_NAME_SET_P(t));
    /* Bug 11224: Also update some flags. Even if the decl is not defined
     * in this translation unit, it needs to have the proper flags so that
     * its scope is correctly set. Updating all possible flags here is
     * difficult. If we don't see this definition, wgen seems to require
     * two flags. */
    _gs_bv (flags, GS_TREE_PUBLIC, TREE_PUBLIC (t));
    _gs_bv (flags, GS_DECL_WEAK, DECL_WEAK (t));
  }


  /* See if the FUNCTION_DECL should be fully or partially translated.  For
   * bodyless functions such as printf, always translate them fully.  For C++,
   * a bodyless function will always have a null DECL_SAVED_TREE, even for the
   * first time we see them here.  (For functions with bodies, DECL_SAVED_TREE
   * will be non-null up until we are called to translate them, then GCC may
   * change the DECL_SAVED_TREE to null.)
   *
   * For C, a null DECL_SAVED_TREE may mean that the function body has not been
   * generated by GCC, since we translate C functions one at a time as soon as
   * each function is finished. */

  if (TREE_CODE(t) == FUNCTION_DECL &&
      !translate_this_func_decl) {
    if (CR()) {				/* C */
      if (!DECL_BUILT_IN(t))
	return GS_NODE(t);
    } else {				/* C++ */
      if ( /* not a bodyless function */
	  DECL_SAVED_TREE(t) ||
	  (DECL_LANG_SPECIFIC(t) &&
	   /* some form of template, whose body GCC will generate later, or */
	   (DECL_USE_TEMPLATE(t)||
	   /* a thunk function which will be expanded later */
	    DECL_THUNK_P(t)))) {
	return GS_NODE(t);
      }
    }
  }

  REVISIT:

  /* Update sequence number before calling gs_x_1. */
  GS_SEQUENCE_NUM(t) = seq_num;

  GS_ASSERT(!(TREE_CODE(t) == FUNCTION_DECL &&
	      FULLY_TRANSLATED_TO_GS(t)) || translate_this_func_decl,
	    ("gs_x_1: cannot re-translate a fully translated FUNCTION_DECL"));
  if (!CR() || TREE_CODE(t) != FUNCTION_DECL || 
      !DECL_BUILT_IN(t) || DECL_SAVED_TREE(t)) { /* bug 14254 */
    FULLY_TRANSLATED_TO_GS(t) = 1;
    if (TREE_CODE(t) == FUNCTION_DECL && DECL_INLINE (t))
      TRANSLATED_TO_GS_AS_INLINE(t) = 1;
    }

  /* Add common tree fields: TREE_TYPE, TREE_CHAIN, common flags.  Note that
   * some flags are defined/valid only for certain conditions. */

  gs_set_operand((gs_t) GS_NODE(t), GS_TREE_TYPE, gs_x_1(TREE_TYPE(t), seq_num));
  gs_set_operand((gs_t) GS_NODE(t), GS_TREE_CHAIN, gs_x_1(TREE_CHAIN(t), seq_num));

  flags = gs_operand((gs_t) GS_NODE(t), GS_FLAGS);
  GS_ASSERT(flags != NULL, ("gs_x_1: GS_FLAGS NULL"));

  REVISIT2:

  /* Update sequence number before calling gs_x_1. */
  GS_SEQUENCE_NUM(t) = seq_num;

  if (!TYPE_P (t))
    _gs_bv (flags, GS_TREE_SIDE_EFFECTS, TREE_SIDE_EFFECTS (t));

  if (TYPE_P (t))
    _gs_bv (flags, GS_TYPE_READONLY, TYPE_READONLY (t));
  else
    _gs_bv (flags, GS_TREE_READONLY, TREE_READONLY (t));

  if (!TYPE_P (t)) 
    _gs_bv (flags, GS_TREE_CONSTANT, TREE_CONSTANT (t));
  else if (TYPE_P (t))
    _gs_bv (flags, GS_TYPE_SIZES_GIMPLIFIED, TYPE_SIZES_GIMPLIFIED (t));

  _gs_bv (flags, GS_TREE_INVARIANT, TREE_INVARIANT (t));
  _gs_bv (flags, GS_TREE_ADDRESSABLE, TREE_ADDRESSABLE (t));
  _gs_bv (flags, GS_TREE_THIS_VOLATILE, TREE_THIS_VOLATILE (t));
  _gs_bv (flags, GS_TREE_ASM_WRITTEN, TREE_ASM_WRITTEN (t));
  _gs_bv (flags, GS_TREE_USED, TREE_USED (t));

  /* TYPE_P (t) ? " align-ok" : " nothrow" */
  _gs_bv (flags, GS_TREE_NOTHROW, TREE_NOTHROW (t));

  /* Don't need to set GS_ASM_VOLATILE_P because it shares the same bit field
   * as GS_TREE_PUBLIC, and GCC's ASM_VOLATILE_P accesses the same flag as
   * TREE_PUBLIC. */
  GS_ASSERT(TREE_PUBLIC(t) == ASM_VOLATILE_P(t),
	    ("gs_x_1: TREE_PUBLIC differs from ASM_VOLATILE"));

  _gs_bv (flags, GS_TREE_PUBLIC, TREE_PUBLIC (t));
  if (!TREE_PUBLIC (t) && gs_bv(flags, GS_TREE_PUBLIC)) /* bug 13126 */
    _gs_bv_reset (flags, GS_TREE_PUBLIC);
  _gs_bv (flags, GS_TREE_PRIVATE, TREE_PRIVATE (t));
  _gs_bv (flags, GS_TREE_PROTECTED, TREE_PROTECTED (t));
  _gs_bv (flags, GS_TREE_STATIC, TREE_STATIC (t));
  _gs_bv (flags, GS_TREE_LANG_FLAG_0, TREE_LANG_FLAG_0 (t));
  _gs_bv (flags, GS_TREE_LANG_FLAG_1, TREE_LANG_FLAG_1 (t));
  _gs_bv (flags, GS_TREE_LANG_FLAG_2, TREE_LANG_FLAG_2 (t));
  _gs_bv (flags, GS_TREE_LANG_FLAG_3, TREE_LANG_FLAG_3 (t));
  _gs_bv (flags, GS_TREE_LANG_FLAG_4, TREE_LANG_FLAG_4 (t));
  _gs_bv (flags, GS_TREE_LANG_FLAG_5, TREE_LANG_FLAG_5 (t));
  _gs_bv (flags, GS_TREE_LANG_FLAG_6, TREE_LANG_FLAG_6 (t));

  _gs_bv (flags, GS_TREE_NOT_EMITTED_BY_GXX, TREE_NOT_EMITTED_BY_GXX(t));

  /* "enum dwarf_access_attribute" is either 1, 2, or 3.  Use 2 flags to encode
   * it. */
  switch (DWARF_ACCESS(t)) {
    case DW_ACCESS_public:	/* 1 */
      _gs_bv(flags, GS_DWARF_ACCESS_FLAG_0, 1);
      break;
    case DW_ACCESS_protected:	/* 2 */
      _gs_bv(flags, GS_DWARF_ACCESS_FLAG_1, 1);
      break;
    case DW_ACCESS_private:	/* 3 */
      _gs_bv(flags, GS_DWARF_ACCESS_FLAG_0, 1);
      _gs_bv(flags, GS_DWARF_ACCESS_FLAG_1, 1);
      break;
  }

  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_declaration:

      _gs_bv (flags, GS_DECL_UNSIGNED, DECL_UNSIGNED (t));
      _gs_bv (flags, GS_DECL_IGNORED_P, DECL_IGNORED_P (t));
      _gs_bv (flags, GS_DECL_ABSTRACT, DECL_ABSTRACT (t));
      _gs_bv (flags, GS_DECL_IN_SYSTEM_HEADER, DECL_IN_SYSTEM_HEADER (t));
      _gs_bv (flags, GS_DECL_COMMON, DECL_COMMON (t));
      if (CPR()) {
	_gs_bv (flags, GS_DECL_EXTERNAL, DECL_EXTERNAL (t));
	if (! DECL_EXTERNAL(t) && gs_bv(flags, GS_DECL_EXTERNAL))
	  _gs_bv_reset (flags, GS_DECL_EXTERNAL);
      } else {
        if (gs_bv(flags, GS_DECL_COMMON) == 0) /* bug 10324 */
          _gs_bv (flags, GS_DECL_EXTERNAL, DECL_EXTERNAL (t));
	/* bugs 10324, 14446 */
        if (! DECL_EXTERNAL(t) && gs_bv(flags, GS_DECL_EXTERNAL))
          _gs_bv_reset (flags, GS_DECL_EXTERNAL);
      }
      if (DECL_WEAK(t) && 
	  (TREE_CODE(t) == VAR_DECL ||
           TREE_CODE(t) == FUNCTION_DECL)) {
	_gs_bv (flags, GS_DECL_WEAK, DECL_WEAK (t));
	gspin_add_weak(t, GS_NODE(t));
      }

      if (TREE_CODE (t) != FIELD_DECL && 
          TREE_CODE (t) != FUNCTION_DECL && 
          TREE_CODE (t) != LABEL_DECL)
        _gs_bv (flags, GS_DECL_REGISTER, DECL_REGISTER (t));

      _gs_bv (flags, GS_DECL_NONLOCAL, DECL_NONLOCAL (t));

      switch (TREE_CODE(t)) {
	case TYPE_DECL:
	  gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ORIGINAL_TYPE,
		    gs_x_1(DECL_ORIGINAL_TYPE(t), seq_num));
	  _gs_bv (flags, GS_TYPE_DECL_SUPPRESS_DEBUG,
		  TYPE_DECL_SUPPRESS_DEBUG (t));
	  break;

	case FUNCTION_DECL:
	  {
	    struct cgraph_node * node = cgraph_node (t);
	    gs_set_operand((gs_t) GS_NODE(t), GS_DECL_SAVED_TREE,
		       gs_x_1(DECL_SAVED_TREE(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_DECL_RESULT,
		       gs_x_1(DECL_RESULT(t), seq_num));
            gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ARGUMENTS,
		       gs_x_1(DECL_ARGUMENTS(t), seq_num));
            gs_set_operand((gs_t) GS_NODE(t), GS_DECL_VINDEX,
		       gs_x_1(DECL_VINDEX(t), seq_num));
	    _gs_bv (flags, GS_DECL_INLINE, DECL_INLINE (t));
	    _gs_bv (flags, GS_DECL_DECLARED_INLINE_P, DECL_DECLARED_INLINE_P (t));
	    _gs_bv (flags, GS_DECL_BUILT_IN, DECL_BUILT_IN (t));
	    _gs_bv (flags, GS_DECL_NO_STATIC_CHAIN, DECL_NO_STATIC_CHAIN (t));
	    /* Set this flag only for C++, because for C it may not always
	     * be initialized to 0. This flag may also be moved to
	     * cp_decl_flags. */
	    if (CPR())
	      _gs_bv (flags, GS_DECL_THUNK_P, DECL_THUNK_P (t));

	    /* KEY: By default for C++, each function is not "needed" and not
	     * "reachable" in GNU call graph terminology. That means wgen
	     * will not emit such functions. These flags are updated during
	     * GNU call graph analysis.
	     */
	    _gs_bv (flags, GS_DECL_NEEDED, node->needed);
	    _gs_bv (flags, GS_DECL_REACHABLE, node->reachable);
	  }

	  break;

	case FIELD_DECL:
	  gs_set_operand((gs_t) GS_NODE(t), GS_DECL_FIELD_OFFSET,
		     gs_x_1(DECL_FIELD_OFFSET(t), seq_num));
	  gs_set_operand((gs_t) GS_NODE(t), GS_DECL_FIELD_BIT_OFFSET,
		     gs_x_1(DECL_FIELD_BIT_OFFSET(t), seq_num));
          _gs_bv (flags, GS_DECL_OFFSET_ALIGN, DECL_OFFSET_ALIGN (t));
	  _gs_bv (flags, GS_DECL_PACKED, DECL_PACKED (t));
	  _gs_bv (flags, GS_DECL_BIT_FIELD, DECL_BIT_FIELD (t));
	  _gs_bv (flags, GS_DECL_NONADDRESSABLE_P, DECL_NONADDRESSABLE_P (t));
	  break;

	case LABEL_DECL:
	  {
	    gs_t label_decl_uid = __gs(IB_INT);
	    _gs_n(label_decl_uid, gspin_label_count);
	    gspin_label_count++;
	    gs_set_operand((gs_t) GS_NODE(t), GS_LABEL_DECL_UID, label_decl_uid);
	  }
	  break;

	case VAR_DECL:
	  gs_set_operand((gs_t) GS_NODE(t), GS_DECL_VALUE_EXPR,
		     gs_x_1(DECL_VALUE_EXPR(t), seq_num));
	  _gs_bv (flags, GS_DECL_IN_TEXT_SECTION, DECL_IN_TEXT_SECTION (t));
	  _gs_bv (flags, GS_DECL_THREAD_LOCAL, DECL_THREAD_LOCAL_P (t));
	  if (TREE_STATIC(t)) {
	    struct cgraph_varpool_node * var_node = cgraph_varpool_node (t);
	    _gs_bv (flags, GS_DECL_NEEDED, var_node->needed);
	  }

	  if (DECL_REGISTER(t) && 
	      (DECL_HARD_REGISTER(t) || DECL_ASSEMBLER_NAME_SET_P(t))) {
	    const char *reg_name = IDENTIFIER_POINTER(DECL_ASSEMBLER_NAME(t));
	    int reg_number = decode_reg_name(&reg_name[1]);
	    gs_t asmreg = __gs(IB_INT);
	    _gs_n(asmreg, reg_number);
	    gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ASMREG, asmreg);
	  }
	  break;

	case PARM_DECL:
	  gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ARG_TYPE,
		     gs_x_1(DECL_ARG_TYPE(t), seq_num));
	  gs_set_operand((gs_t) GS_NODE(t), GS_DECL_VALUE_EXPR,
		     gs_x_1(DECL_VALUE_EXPR(t), seq_num));
	  break;

	default:
	  break;
      }

      _gs_bv (flags, GS_DECL_VIRTUAL_P, DECL_VIRTUAL_P (t));

      _gs_bv (flags, GS_DECL_DEFER_OUTPUT, DECL_DEFER_OUTPUT (t));

      _gs_bv (flags, GS_DECL_PRESERVE_P, DECL_PRESERVE_P (t));

      _gs_bv (flags, GS_DECL_EMITTED_BY_GXX, DECL_EMITTED_BY_GXX (t));

      _gs_bv (flags, GS_DECL_LANG_FLAG_0, DECL_LANG_FLAG_0 (t));
      _gs_bv (flags, GS_DECL_LANG_FLAG_1, DECL_LANG_FLAG_1 (t));
      _gs_bv (flags, GS_DECL_LANG_FLAG_2, DECL_LANG_FLAG_2 (t));
      _gs_bv (flags, GS_DECL_LANG_FLAG_3, DECL_LANG_FLAG_3 (t));
      _gs_bv (flags, GS_DECL_LANG_FLAG_4, DECL_LANG_FLAG_4 (t));
      _gs_bv (flags, GS_DECL_LANG_FLAG_5, DECL_LANG_FLAG_5 (t));
      _gs_bv (flags, GS_DECL_LANG_FLAG_6, DECL_LANG_FLAG_6 (t));
      _gs_bv (flags, GS_DECL_LANG_FLAG_7, DECL_LANG_FLAG_7 (t));

      /* DECL_NAME */
      gs_set_operand((gs_t)GS_NODE(t), GS_DECL_NAME, gs_x_1(DECL_NAME(t), seq_num));

      if (gs_operand((gs_t)GS_NODE (t), GS_DECL_UID) == NULL)
      {
        /* DECL_UID */
        gs_t decl_uid = __gs (IB_INT); 
        _gs_n (decl_uid, DECL_UID (t));
        gs_set_operand ((gs_t) GS_NODE (t), GS_DECL_UID, decl_uid);
      }

      /* MODE */
      mode = DECL_MODE (t);
      if (gs_operand ((gs_t) GS_NODE (t), GS_DECL_MODE) == NULL)
      {
        gs_t mode_node;
        mode_node = __gs (IB_STRING);
        _gs_s (mode_node, (gs_string_t) GET_MODE_NAME (mode),
	       1 + strlen (GET_MODE_NAME (mode)));
        gs_set_operand ((gs_t) GS_NODE (t), GS_DECL_MODE, mode_node);
      }

      /* file */
      if (gs_operand((gs_t) GS_NODE(t), GS_DECL_SOURCE_FILE) == NULL)
      {
        gs_t file = __gs (IB_STRING);
        _gs_s (file, (gs_string_t) DECL_SOURCE_FILE (t),
	       1 + strlen (DECL_SOURCE_FILE (t)));
        gs_set_operand ((gs_t) GS_NODE (t), GS_DECL_SOURCE_FILE, file);
      }

      /* line */
      if (gs_operand((gs_t) GS_NODE(t), GS_DECL_SOURCE_LINE) == NULL)
      {
        gs_t line = __gs (IB_INT);
        _gs_n (line, DECL_SOURCE_LINE (t));
        gs_set_operand ((gs_t) GS_NODE (t), GS_DECL_SOURCE_LINE, line);
      }

      /* DECL_SIZE */
      /* DECL_SIZE_UNIT */
      gs_set_operand((gs_t) GS_NODE(t), GS_DECL_SIZE,
		 gs_x_1(DECL_SIZE(t), seq_num));
      gs_set_operand((gs_t) GS_NODE(t), GS_DECL_SIZE_UNIT,
		 gs_x_1(DECL_SIZE_UNIT(t), seq_num));

      if (TREE_CODE (t) != FUNCTION_DECL) {
        _gs_bv (flags, GS_DECL_USER_ALIGN, DECL_USER_ALIGN (t));
      }
      else if (DECL_BUILT_IN (t)) {
        gs_t decl_built_in_class, decl_function_code;
        _gs_bv (flags, GS_DECL_BUILT_IN, DECL_BUILT_IN (t));
        decl_built_in_class = __gs (GSBI_CLASS);
        decl_function_code  = __gs (GSBI);
        _gs_b(decl_built_in_class, gcc_built_in_class2gsbi_class ((int) DECL_BUILT_IN_CLASS (t)));
        switch (DECL_BUILT_IN_CLASS (t)) {
          case BUILT_IN_NORMAL:
            _gs_hword(decl_function_code, gcc_built_in2gsbi ((int) DECL_FUNCTION_CODE (t)));
            break;
#ifdef TARG_X8664
          case BUILT_IN_MD:
            _gs_hword(decl_function_code, ix86_builtins2gsbi_ts ((int) DECL_FUNCTION_CODE (t)));
            break;
#endif
          default: gcc_assert (0); break;
        }
        gs_set_operand ((gs_t) GS_NODE (t), GS_DECL_BUILT_IN_CLASS, decl_built_in_class);
        gs_set_operand ((gs_t) GS_NODE (t), GS_DECL_FUNCTION_CODE, decl_function_code);
        /* printf ("BUILT_IN_COMPLEX_MUL_MIN: %d\n", BUILT_IN_COMPLEX_MUL_MIN); */
        /* printf ("BUILT_IN_COMPLEX_MUL_MAX: %d\n", BUILT_IN_COMPLEX_MUL_MAX); */
        /* printf ("BUILT_IN_COMPLEX_DIV_MIN: %d\n", BUILT_IN_COMPLEX_DIV_MIN); */
        /* printf ("BUILT_IN_COMPLEX_DIV_MAX: %d\n", BUILT_IN_COMPLEX_DIV_MAX); */
        /* printf ("END_BUILTINS: %d\n", END_BUILTINS); */
      }

      if (DECL_POINTER_ALIAS_SET_KNOWN_P (t))
        _gs_bv (flags, GS_DECL_POINTER_ALIAS_SET, DECL_POINTER_ALIAS_SET (t));

      /* DECL_CONTEXT */
      /* DECL_ATTRIBUTES */
      /* DECL_ABSTRACT_ORIGIN */
      /* DECL_RESULT_FLD */
      /* DECL_INITIAL */
      gs_set_operand((gs_t) GS_NODE(t), GS_DECL_CONTEXT,
		 gs_x_1 (DECL_CONTEXT(t), seq_num));
      gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ATTRIBUTES,
		 gs_x_1 (DECL_ATTRIBUTES(t), seq_num));
      gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ABSTRACT_ORIGIN,
		 gs_x_1 (DECL_ABSTRACT_ORIGIN(t), seq_num));

      /* Don't care about the DECL_INITIAL In a TYPE_DECL.  (For C++, the
       * DECL_INITIAL is used as DECL_FRIENDLIST.)  Bug 11281. */
      if (TREE_CODE (t) != TYPE_DECL) {
	gs_set_operand((gs_t) GS_NODE(t), GS_DECL_INITIAL,
		       gs_x_1 (DECL_INITIAL(t), seq_num));
      }

      /* if (DECL_RTL_SET_P (t)) */

      if (gs_operand((gs_t) GS_NODE(t), GS_DECL_ALIGN_UNIT) == NULL)
      {
	gs_t decl_align_unit;
	decl_align_unit = __gs (IB_INT);
	_gs_n (decl_align_unit, DECL_ALIGN_UNIT (t)); 
	gs_set_operand ((gs_t) GS_NODE (t), GS_DECL_ALIGN_UNIT, decl_align_unit);
      }

      _gs_bv (flags, GS_DECL_ASSEMBLER_NAME_SET_P, DECL_ASSEMBLER_NAME_SET_P (t));
      _gs_bv (flags, GS_DECL_ARTIFICIAL, DECL_ARTIFICIAL(t));
      _gs_bv (flags, GS_DECL_LANG_SPECIFIC, DECL_LANG_SPECIFIC (t));

      if (TREE_CODE (t) == FUNCTION_DECL
         || (TREE_CODE (t) == VAR_DECL
            && (TREE_STATIC (t)
                || DECL_EXTERNAL (t)
                || TREE_PUBLIC (t)
		|| (DECL_REGISTER(t) && DECL_HARD_REGISTER(t))))) {
        gs_set_operand((gs_t) GS_NODE(t), GS_DECL_ASSEMBLER_NAME,
		   gs_x_1(DECL_ASSEMBLER_NAME(t), seq_num));
      }

      /* C++ */
      {
        gs_t cp_decl_flags;
	if (gs_operand((gs_t) GS_NODE(t), GS_CP_DECL_FLAGS) == NULL) {
	  cp_decl_flags = __gs (IB_BIT_VECTOR);
	  gs_set_operand ((gs_t) GS_NODE (t), GS_CP_DECL_FLAGS, cp_decl_flags);
	}
	else cp_decl_flags = gs_operand((gs_t) GS_NODE(t), GS_CP_DECL_FLAGS);

        if (TREE_CODE (t) == VAR_DECL)
          _gs_bv (cp_decl_flags, GS_DECL_COMDAT, DECL_COMDAT (t));

	if (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == FUNCTION_DECL)
          gs_set_operand((gs_t) GS_NODE(t), GS_DECL_SECTION_NAME,
                         gs_x_1(DECL_SECTION_NAME(t), seq_num));

        if (TREE_PUBLIC (t))
          _gs_bv (cp_decl_flags, GS_DECL_ONE_ONLY, DECL_ONE_ONLY (t));

        if (CPR()) {	/* C++ */
	  switch (TREE_CODE(t)) {
	    case FUNCTION_DECL:
	      
	      if (!DECL_LANG_SPECIFIC (t)) {
	        /* Following C++ snippet necessitates the need of condition 
	         * "if (!DECL_LANG_SPECIFIC(t))"
	         * 
	         *   #pragma weak bar1 = foo1
	         *   extern "C" void foo1 (void) { }
	         *
	         *  The front end creates a decl tree both for bar1() and foo1().
	         *  The decl.lang_specific field of the decl tree for bar1() is NULL,
	         *  as evidenced in maybe_apply_pending_pragma_weaks(). 
	         *  If the <t> being procesed is the decl tree for bar1(), calling 
	         *  macros like DECL_GLOBAL_CTOR_P will incur segamentation fault.
	         */
	        break;
	      }

	      _gs_bv(cp_decl_flags, GS_DECL_GLOBAL_CTOR_P,
		     DECL_GLOBAL_CTOR_P(t));
	      _gs_bv(cp_decl_flags, GS_DECL_GLOBAL_DTOR_P,
		     DECL_GLOBAL_DTOR_P(t));
	      _gs_bv(cp_decl_flags, GS_DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P,
		     DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P(t));
	      _gs_bv(cp_decl_flags, GS_DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P,
		     DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P(t));
	      _gs_bv(cp_decl_flags, GS_DECL_FUNCTION_MEMBER_P,
		     DECL_FUNCTION_MEMBER_P(t));
	      _gs_bv(cp_decl_flags, GS_DECL_USES_TEMPLATE_PARMS,
		     (*p_uses_template_parms)(t));
	      _gs_bv(cp_decl_flags,
		     GS_DECL_COPY_CONSTRUCTOR_P, /* DECL_COPY_CONSTRUCTOR_P */
		     DECL_CONSTRUCTOR_P (t) && (*p_copy_fn_p) (t) > 0);
	      _gs_bv(cp_decl_flags, GS_DECL_EXTERN_C_P,
		     DECL_EXTERN_C_P(t));
#ifdef FE_GNU_4_2_0
	      _gs_bv(cp_decl_flags, GS_DECL_CONSTRUCTOR_P,
		     DECL_CONSTRUCTOR_P(t));
	      _gs_bv(cp_decl_flags, GS_DECL_ASSIGNMENT_OPERATOR_P,
		     DECL_ASSIGNMENT_OPERATOR_P(t));
#endif

	      gs_set_operand((gs_t) GS_NODE(t), GS_DECL_NAMED_RETURN_OBJECT,
			   gs_x_1(DECL_NAMED_RETURN_OBJECT(t), seq_num));

	      if (DECL_THUNK_P(t)) {
		gs_t fixed = __gs(IB_LONG);
		_gs_n(fixed, THUNK_FIXED_OFFSET(t));

		gs_set_operand((gs_t) GS_NODE(t), GS_THUNK_FIXED_OFFSET, fixed);
		gs_set_operand((gs_t) GS_NODE(t), GS_THUNK_VIRTUAL_OFFSET,
			   gs_x_1(THUNK_VIRTUAL_OFFSET(t), seq_num));
		gs_set_operand((gs_t) GS_NODE(t), GS_THUNK_TARGET,
			   gs_x_1(THUNK_TARGET(t), seq_num));
		_gs_bv(cp_decl_flags, GS_DECL_THIS_THUNK_P,
			   DECL_THIS_THUNK_P(t));
	      }

	      break;

	    case NAMESPACE_DECL:
	      if (NAMESPACE_LEVEL(t)) {	/* bug 10855 */
		gs_set_operand((gs_t) GS_NODE(t), GS_CP_NAMESPACE_DECLS,
			       gs_x_1(NAMESPACE_LEVEL(t)->names, seq_num));
	      }
	      gs_set_operand((gs_t) GS_NODE(t), GS_DECL_NAMESPACE_ALIAS,
			 gs_x_1(DECL_NAMESPACE_ALIAS(t), seq_num));
	      break;

	    case TEMPLATE_DECL:
	      if (DECL_LANG_SPECIFIC (t))
	        gs_set_operand((gs_t) GS_NODE (t), GS_MOST_GENERAL_TEMPLATE,
			   gs_x_1((*p_most_general_template)(t), seq_num));
	      break;

	    default:
	      break;
	  }

	  if ((TREE_CODE(t) == VAR_DECL ||
	       TREE_CODE(t) == FUNCTION_DECL ||
	       TREE_CODE(t) == TYPE_DECL ||
	       TREE_CODE(t) == TEMPLATE_DECL) &&
	      DECL_LANG_SPECIFIC(t) != NULL &&
	      DECL_TEMPLATE_INFO(t) != NULL) {
	    gs_set_operand((gs_t) GS_NODE(t), GS_DECL_TEMPLATE_INFO,
		       gs_x_1(DECL_TEMPLATE_INFO(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_DECL_TI_TEMPLATE,
		       gs_x_1(DECL_TI_TEMPLATE(t), seq_num));
	  }

	  if (TREE_CODE(t) == VAR_DECL ||
	      TREE_CODE(t) == FUNCTION_DECL) {
	    _gs_bv(cp_decl_flags, GS_DECL_TEMPLATE_INSTANTIATED, 
		   DECL_TEMPLATE_INSTANTIATED(t));
	  }

	  if (DECL_LANG_SPECIFIC(t)) {
	    _gs_bv(cp_decl_flags, GS_DECL_IMPLICIT_INSTANTIATION,
		   DECL_IMPLICIT_INSTANTIATION(t));
	    _gs_bv(cp_decl_flags, GS_DECL_TEMPLATE_SPECIALIZATION,
		   DECL_TEMPLATE_SPECIALIZATION(t));
            _gs_bv(cp_decl_flags, GS_DECL_COMPLETE_CONSTRUCTOR_P,
                   DECL_COMPLETE_CONSTRUCTOR_P(t));
#ifdef FE_GNU_4_2_0
            _gs_bv(cp_decl_flags, GS_DECL_COMPLETE_DESTRUCTOR_P,
                   DECL_COMPLETE_DESTRUCTOR_P(t));
            _gs_bv(cp_decl_flags, GS_DECL_HAS_IN_CHARGE_PARM_P,
                   DECL_HAS_IN_CHARGE_PARM_P(t));
            _gs_bv(cp_decl_flags, GS_DECL_HAS_VTT_PARM_P,
                   DECL_HAS_VTT_PARM_P(t));
#endif
            _gs_bv(cp_decl_flags, GS_DECL_REALLY_EXTERN, DECL_REALLY_EXTERN(t));
            if (! DECL_REALLY_EXTERN(t) &&
                gs_bv(cp_decl_flags, GS_DECL_REALLY_EXTERN))
              _gs_bv_reset (cp_decl_flags, GS_DECL_REALLY_EXTERN);
            _gs_bv(cp_decl_flags, GS_DECL_USE_TEMPLATE, DECL_USE_TEMPLATE(t));
            if (TREE_CODE(t) == VAR_DECL)
              _gs_bv(cp_decl_flags, GS_CP_DECL_THREADPRIVATE_P,
                     CP_DECL_THREADPRIVATE_P(t));
	  }
          gs_set_operand((gs_t) GS_NODE (t), GS_CP_DECL_CONTEXT,
		     gs_x_1(CP_DECL_CONTEXT(t), seq_num));
          _gs_bv(cp_decl_flags, GS_DECL_NAMESPACE_SCOPE_P,
		 DECL_NAMESPACE_SCOPE_P(t));
        }
      }

      /* DECL_FLAG2 */
      gs_set_operand((gs_t) GS_NODE (t), GS_DECL_FLAG2, __gs (IB_BIT_VECTOR));
      gs_set_decl_tls_model((gs_t) GS_NODE (t), DECL_TLS_MODEL(t));
      GS_ASSERT(gs_decl_tls_model((gs_t) GS_NODE (t) ) == DECL_TLS_MODEL(t),
                ("tls_model setting failure!!!"));
      gs_set_decl_visibility_specified((gs_t) GS_NODE (t), DECL_VISIBILITY_SPECIFIED(t));
      gs_set_decl_visibility((gs_t) GS_NODE (t), DECL_VISIBILITY(t));

      break;

    case tcc_type:

      switch (TREE_CODE(t)) {
	case ENUMERAL_TYPE:
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_VALUES, gs_x_1(TYPE_VALUES(t),
		     seq_num));
	  break;

	case ARRAY_TYPE:
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_DOMAIN, gs_x_1(TYPE_DOMAIN(t),
		     seq_num));
	  if (TYPE_NONALIASED_COMPONENT(t))
	    _gs_bv(flags, GS_TYPE_NONALIASED_COMPONENT,
		   TYPE_NONALIASED_COMPONENT(t));
	  break;

	case VECTOR_TYPE:
	  {
	    gs_t parts_node;
	    parts_node = __gs (IB_INT);
	    _gs_n (parts_node, TYPE_VECTOR_SUBPARTS (t));
	    gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_VECTOR_SUBPARTS, parts_node);

	    gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_DEBUG_REPRESENTATION_TYPE,
		       gs_x_1(TYPE_DEBUG_REPRESENTATION_TYPE(t), seq_num));
	  }
	  break;

	case UNION_TYPE:
	  if (TYPE_TRANSPARENT_UNION(t))
	    _gs_bv(flags, GS_TYPE_TRANSPARENT_UNION, TYPE_TRANSPARENT_UNION(t));
	  /* fall through */

	case RECORD_TYPE:
	  if (TREE_CODE(t) == RECORD_TYPE) {
	    gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_BINFO, gs_x_1(TYPE_BINFO(t),
		       seq_num));
	  }
	  if (CPR()) {
	      /* Currently, wgen uses the following flag only for these types.
	       * It is ok not to set it for C. */
	      _gs_bv(flags, GS_AGGREGATE_VALUE_P, aggregate_value_p (t, NULL));
	  }
	  /* fall through */

	case QUAL_UNION_TYPE:
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_FIELDS, gs_x_1(TYPE_FIELDS(t),
		     seq_num));
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_VFIELD, gs_x_1(TYPE_VFIELD(t),
	  	     seq_num));
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_METHODS, gs_x_1(TYPE_METHODS(t),
		     seq_num));

	  if (TYPE_NO_FORCE_BLK(t))
	    _gs_bv(flags, GS_TYPE_NO_FORCE_BLK, TYPE_NO_FORCE_BLK(t));
	  break;

	case FUNCTION_TYPE:
	  if (TYPE_RETURNS_STACK_DEPRESSED(t))
	    _gs_bv(flags, GS_TYPE_RETURNS_STACK_DEPRESSED,
		   TYPE_RETURNS_STACK_DEPRESSED(t));
	  /* fall through */

	case METHOD_TYPE:
	  if (TYPE_METHOD_BASETYPE(t)) {
	    gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_METHOD_BASETYPE,
		       gs_x_1(TYPE_METHOD_BASETYPE(t), seq_num));
	  }
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_ARG_TYPES,
		     gs_x_1(TYPE_ARG_TYPES(t), seq_num));
	  break;

	case OFFSET_TYPE:
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_OFFSET_BASETYPE,
		     gs_x_1(TYPE_OFFSET_BASETYPE(t), seq_num));
	  break;

	case INTEGER_TYPE:
	  if (TYPE_IS_SIZETYPE(t))
	    _gs_bv(flags, GS_TYPE_IS_SIZETYPE, TYPE_IS_SIZETYPE(t));
	  break;

	default:
	  break;
      }

      _gs_bv (flags, GS_TYPE_UNSIGNED, TYPE_UNSIGNED (t));
      _gs_bv (flags, GS_TYPE_STRING_FLAG, TYPE_STRING_FLAG (t));
      _gs_bv (flags, GS_TYPE_NEEDS_CONSTRUCTING, TYPE_NEEDS_CONSTRUCTING (t));
      _gs_bv (flags, GS_TYPE_PACKED, TYPE_PACKED (t));
      _gs_bv (flags, GS_TYPE_RESTRICT, TYPE_RESTRICT (t));
      _gs_bv (flags, GS_TYPE_LANG_FLAG_0, TYPE_LANG_FLAG_0 (t));
      _gs_bv (flags, GS_TYPE_LANG_FLAG_1, TYPE_LANG_FLAG_1 (t));
      _gs_bv (flags, GS_TYPE_LANG_FLAG_2, TYPE_LANG_FLAG_2 (t));
      _gs_bv (flags, GS_TYPE_LANG_FLAG_3, TYPE_LANG_FLAG_3 (t));
      _gs_bv (flags, GS_TYPE_LANG_FLAG_4, TYPE_LANG_FLAG_4 (t));
      _gs_bv (flags, GS_TYPE_LANG_FLAG_5, TYPE_LANG_FLAG_5 (t));
      _gs_bv (flags, GS_TYPE_LANG_FLAG_6, TYPE_LANG_FLAG_6 (t));
      _gs_bv (flags, GS_TYPE_VOLATILE, TYPE_VOLATILE (t));

      if (TYPE_NAME(t)) {
	if (TREE_CODE(TYPE_NAME(t)) == IDENTIFIER_NODE)
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_NAME, gs_x_1(TYPE_NAME(t),
		     seq_num));
	else if (TREE_CODE(TYPE_NAME(t)) == TYPE_DECL)
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_NAME, gs_x_1(TYPE_NAME(t),
		     seq_num));
      }

      /* mode */
      mode = TYPE_MODE (t);
      if (gs_operand((gs_t) GS_NODE(t), GS_TYPE_MODE) == NULL)
      {
        gs_t mode_node;
        mode_node = __gs (IB_STRING);
        _gs_s (mode_node, (gs_string_t) GET_MODE_NAME (mode),
	       1 + strlen (GET_MODE_NAME (mode)));
        gs_set_operand ((gs_t) GS_NODE (t), GS_TYPE_MODE, mode_node);
      }

      /* TYPE_SIZE */
      /* TYPE_SIZE_UNIT */
      /* TYPE_USER_ALIGN */
      /* printf ("TYPE_SIZE: %p\n", TYPE_SIZE (t)); */
      gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_SIZE,
		 gs_x_1(TYPE_SIZE(t), seq_num));
      gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_SIZE_UNIT,
		 gs_x_1(TYPE_SIZE_UNIT(t), seq_num));
      /* TODO: Freeup this slot. */
      /* gs_set_operand ((gs_t) GS_NODE (t), GS_TYPE_USER_ALIGN, gs_x (TYPE_USER_ALIGN (t))); */
      gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_USER_ALIGN, gs_x_1(NULL, seq_num));

      /* align */
      if (gs_operand((gs_t) GS_NODE(t), GS_TYPE_ALIGN) == NULL) {
        gs_t align_node;
        align_node = __gs (IB_INT);
        _gs_n (align_node, TYPE_ALIGN (t));
        gs_set_operand ((gs_t) GS_NODE (t), GS_TYPE_ALIGN, align_node);
      }

      /* alias_set_node */
      if (gs_operand((gs_t) GS_NODE (t), GS_TYPE_ALIAS_SET) == NULL) {
	gs_t alias_set_node;
	alias_set_node = __gs (IB_INT);
	_gs_n (alias_set_node, TYPE_ALIAS_SET (t));
	gs_set_operand ((gs_t) GS_NODE (t), GS_TYPE_ALIAS_SET, alias_set_node);
      }

      gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_ATTRIBUTES,
		 gs_x_1(TYPE_ATTRIBUTES(t), seq_num));
      gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_CONTEXT, gs_x_1(TYPE_CONTEXT(t),
		 seq_num));
      gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_POINTER_TO,
		 gs_x_1(TYPE_POINTER_TO(t), seq_num));
      gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_REFERENCE_TO,
		 gs_x_1(TYPE_REFERENCE_TO(t), seq_num));

      _gs_bv (flags, GS_TYPE_LANG_SPECIFIC, TYPE_LANG_SPECIFIC(t));
      _gs_bv (flags, GS_POINTER_TYPE_P, POINTER_TYPE_P(t));

      /* TYPE_PRECISION, TYPE_MIN_VALUE, TYPE_MAX_VALUE */
      {
	if (gs_operand((gs_t) GS_NODE(t), GS_TYPE_PRECISION) == NULL) {
	  gs_t type_precision;
	  type_precision = __gs (IB_INT);
	  _gs_n (type_precision, TYPE_PRECISION (t));
	  gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_PRECISION, type_precision);
	}
	gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_MIN_VALUE,
		   gs_x_1(TYPE_MIN_VALUE (t), seq_num));
	gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_MAX_VALUE,
		   gs_x_1(TYPE_MAX_VALUE(t), seq_num));
      }

      /* C++ */
      {
        gs_t cp_type_flags;
	if (gs_operand((gs_t) GS_NODE(t), GS_CP_TYPE_FLAGS) == NULL) {
	  cp_type_flags = __gs (IB_BIT_VECTOR);
	  gs_set_operand ((gs_t) GS_NODE (t), GS_CP_TYPE_FLAGS, cp_type_flags);
	}
	else cp_type_flags = gs_operand((gs_t) GS_NODE (t), GS_CP_TYPE_FLAGS);

        if (CPR ()) {
          _gs_bv (cp_type_flags, GS_TYPE_PTRMEMFUNC_P, TYPE_PTRMEMFUNC_P(t));
          _gs_bv (cp_type_flags, GS_TYPE_PTRMEM_P, TYPE_PTRMEM_P(t));
          _gs_bv (cp_type_flags, GS_ANON_UNION_TYPE_P, ANON_UNION_TYPE_P(t));
          _gs_bv (cp_type_flags, GS_CLASS_TYPE_P, CLASS_TYPE_P(t));
          if (TYPE_LANG_SPECIFIC (t) &&
	      TYPE_LANG_SPECIFIC(t)->u.h.is_lang_type_class) {
	    gs_set_operand((gs_t) GS_NODE(t), GS_CLASSTYPE_AS_BASE,
		       gs_x_1(CLASSTYPE_AS_BASE(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_CLASSTYPE_TYPEINFO_VAR,
		       gs_x_1(CLASSTYPE_TYPEINFO_VAR(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_CLASSTYPE_COPY_CONSTRUCTOR,
		       gs_x_1(CLASSTYPE_COPY_CONSTRUCTOR(t), seq_num));

	    _gs_bv(cp_type_flags, GS_CLASSTYPE_INTERFACE_ONLY,
		   CLASSTYPE_INTERFACE_ONLY(t));
	    _gs_bv(cp_type_flags, GS_CLASSTYPE_TEMPLATE_SPECIALIZATION,
		   CLASSTYPE_TEMPLATE_SPECIALIZATION(t));
#ifdef FE_GNU_4_2_0
	    _gs_bv(cp_type_flags, GS_CLASSTYPE_NON_POD_P,
		   CLASSTYPE_NON_POD_P(t));
	    _gs_bv(cp_type_flags, GS_TYPE_HAS_DEFAULT_CONSTRUCTOR,
		   TYPE_HAS_DEFAULT_CONSTRUCTOR(t));
	    _gs_bv(cp_type_flags, GS_TYPE_HAS_IMPLICIT_COPY_CONSTRUCTOR,
	           TYPE_HAS_IMPLICIT_COPY_CONSTRUCTOR(t));
#endif
	  }

	  switch (TREE_CODE(t)) {
	    case RECORD_TYPE:
	      _gs_bv(cp_type_flags, GS_TYPE_USES_TEMPLATE_PARMS,
		     (*p_uses_template_parms)(t));
	      /* fall through */

	    case UNION_TYPE:
	      _gs_bv(cp_type_flags, GS_IS_EMPTY_CLASS, (*p_is_empty_class)(t));
	      break;

	    default:
	      break;
	  }
        }
      }

      if (TYPE_MAIN_VARIANT (t) != t)
        gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_MAIN_VARIANT,
		   gs_x_1(TYPE_MAIN_VARIANT(t), seq_num));
      else
        gs_set_operand((gs_t) GS_NODE(t), GS_TYPE_MAIN_VARIANT, (gs_t) GS_NODE(t));
      break;

    case tcc_expression:
    case tcc_comparison:
    case tcc_unary:
    case tcc_binary:
    case tcc_reference:
    case tcc_statement:

      {
        gs_t length;

	switch (TREE_CODE(t)) {
	  case BIT_FIELD_REF:
	    _gs_bv(flags, GS_BIT_FIELD_REF_UNSIGNED, BIT_FIELD_REF_UNSIGNED(t));
	    break;
	  case TARGET_EXPR:
	    _gs_bv(flags, GS_EMIT_TARGET_EXPR_CLEANUP,
		   EMIT_TARGET_EXPR_CLEANUP(t));
	    break;
	  case CALL_EXPR:
	    {
	      /* bug 12598: Try to fold OBJ_TYPE_REF if it is present
	         under the CALL_EXPR. Code adapted from fold_stmt() . */
	      tree callee = get_callee_fndecl (t);
              if (callee && TREE_CODE(callee) == FUNCTION_DECL)
              {
                 /* we need to emit the function be calleed, no matter 
                    if the call is removed later by gcc cfg cleanup, so 
                    the open64 backend wouldn't be surprised by missing
                    function definition. */
                 TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (callee)) = 1;
              }
	      if (!(callee && DECL_BUILT_IN(callee)))
	      {
		callee = TREE_OPERAND(t,0);
		if (TREE_CODE(callee) == OBJ_TYPE_REF &&
		    lang_hooks.fold_obj_type_ref
		    && TREE_CODE (OBJ_TYPE_REF_OBJECT (callee)) == ADDR_EXPR
		    && DECL_P (TREE_OPERAND(OBJ_TYPE_REF_OBJECT (callee), 0)))
		    {
		      tree t1 = TREE_TYPE (TREE_TYPE
		                           (OBJ_TYPE_REF_OBJECT (callee)));
		      t1 = lang_hooks.fold_obj_type_ref (callee, t1);
		      if (t1)
		        TREE_OPERAND (t, 0) = t1;
		    }
	      }
	    }
	    break;
	  case OMP_ATOMIC:
	    {
	      /* For the atomic operation

	          lhs = expr

	         where 'expr' may use the variable 'lhs', the first operand
	         of OMP_ATOMIC is &lhs, and the second operand is 'expr'. GCC
	         knows to always refer the first operand through an 
	         indirect_ref. So we build it here for wgen, but don't modify
	         the GNU tree.

	         The operands are already set here, so we should not redo it
	         below.
	      */
	      gs_set_operand((gs_t) GS_NODE(t), GS_TREE_OPERAND_ZERO,
	        gs_x_1(build_indirect_ref(TREE_OPERAND(t, 0), NULL), seq_num));
	      gs_set_operand((gs_t) GS_NODE(t), GS_TREE_OPERAND_ZERO + 1,
	        gs_x_1(TREE_OPERAND(t, 1), seq_num));
	    }
	    break;

	  default:
	    break;
	}

        /* length = TREE_CODE_LENGTH (TREE_CODE ()) */
        length = __gs (IB_INT);
        _gs_n (length, TREE_CODE_LENGTH (TREE_CODE (t)));
        gs_set_operand ((gs_t) GS_NODE (t), GS_ARITY, length);

        {
          gs_t file, line;
  
          if (EXPR_HAS_LOCATION (t)) {
            file = __gs (IB_STRING);
            _gs_s (file, (gs_string_t) EXPR_FILENAME (t),
		   1 + strlen (EXPR_FILENAME (t)));
            line = __gs (IB_INT);
            _gs_n (line, EXPR_LINENO (t));
 
	    gs_set_operand ((gs_t) GS_NODE (t), GS_EXPR_FILENAME, file);
	    gs_set_operand ((gs_t) GS_NODE (t), GS_EXPR_LINENO, line);
          }

          _gs_bv (flags, GS_EXPR_HAS_LOCATION, EXPR_HAS_LOCATION (t));
        }

	/* ATOMIC has already been processed above. */
	if (TREE_CODE(t) != OMP_ATOMIC)
          /* max value returnable by TREE_CODE_LENGTH () seems to be 4. */
          for (i = 0; i < TREE_CODE_LENGTH (TREE_CODE (t)); i++) {
            gs_set_operand((gs_t) GS_NODE(t), GS_TREE_OPERAND_ZERO + i,
		       gs_x_1(TREE_OPERAND(t, i), seq_num));
	  }

        /* C++ */
        {
          gs_t cp_expr_flags;
          cp_expr_flags = __gs (IB_BIT_VECTOR);
          gs_set_operand ((gs_t) GS_NODE (t), GS_CP_EXPR_FLAGS, cp_expr_flags);

          if (TREE_CODE (t) == CLEANUP_STMT && CPR ()) /* Will be NULL if this is a C run. */
            gs_set_operand((gs_t) GS_NODE(t), GS_CLEANUP_EXPR,
		       gs_x_1(CLEANUP_EXPR(t), seq_num));

          if (CPR())
          {
            /* We can have this kid for classes also, but that is not
             * currently required. */
            if (TREE_CODE(t) == HANDLER)
            {
              tree htype = HANDLER_TYPE (t);
              /* Note: NULL htype for catch-all clause */
              if (htype && !CLASS_TYPE_P (htype))
              {
                /* Make sure htype has been translated. */
                gs_x_1(htype, seq_num);
                gs_set_operand ((gs_t) GS_NODE (htype),
                            GS_TYPEINFO_DECL,
                            gs_x_1((*p_get_tinfo_decl)(htype), seq_num));
              }
            }
            else if (TREE_CODE(t) == EH_SPEC_BLOCK)
            {
              tree spec = EH_SPEC_RAISES (t);
              for (; spec; spec = TREE_CHAIN (spec))
              {
                tree htype = TREE_VALUE (spec);
                if (htype && !CLASS_TYPE_P (htype))
                {
                  /* Make sure htype has been translated. */
                  gs_x_1(htype, seq_num);
                  gs_set_operand ((gs_t) GS_NODE (htype),
                              GS_TYPEINFO_DECL,
                              gs_x_1((*p_get_tinfo_decl)(htype), seq_num));
                }
              }
            }
          }

          _gs_bv(cp_expr_flags, GS_STMT_IS_FULL_EXPR_P, STMT_IS_FULL_EXPR_P(t));

	  switch (TREE_CODE(t)) {
	    case AGGR_INIT_EXPR:
	      _gs_bv(cp_expr_flags, GS_AGGR_INIT_VIA_CTOR_P,
		     AGGR_INIT_VIA_CTOR_P(t));
	      break;
	    case TARGET_EXPR:
	    case WITH_CLEANUP_EXPR:
	    case CLEANUP_STMT:
	      _gs_bv(cp_expr_flags, GS_CLEANUP_EH_ONLY, CLEANUP_EH_ONLY(t));
	      break;
	    default:
	      break;
	  }
        }
      }
      break;

    case tcc_constant:
    case tcc_exceptional:

      switch (TREE_CODE (t)) {

	  case INTEGER_CST:
            {
              /* low, high */
              gs_t low, high; 

              _gs_bv (flags, GS_TREE_CONSTANT_OVERFLOW, TREE_CONSTANT_OVERFLOW (t));
  
              /* printf ("TREE_INT_CST_HIGH (t): %lld, TREE_INT_CST_LOW (t): %llu\n", TREE_INT_CST_HIGH (t), TREE_INT_CST_LOW (t)); */

              low  = __gs (IB_UNSIGNED_LONG_LONG); 
              high = __gs (IB_LONG_LONG);

              _gs_ull (low, TREE_INT_CST_LOW (t));
              _gs_ll (high, TREE_INT_CST_HIGH (t));

              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_INT_CST_LOW, low);
              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_INT_CST_HIGH, high);

              /* printf ("tree_int_cst_high (t): %lld, tree_int_cst_low (t): %llu\n", gs_ll (high), gs_ull (low)); */
            }
  	    break;
  
          case PTRMEM_CST:
	    {
	      /* The usual way to expand PTRMEM_CST is to call
	       * cplus_expand_constant, so do it now while we have access to
	       * cplus_expand_constant. */
	      tree t2;
	      GS_ASSERT(lang_hooks.cplus_expand_constant,
			("gs_x_1: lang hook NULL for cplus_expand_constant"));
	      t2 = lang_hooks.cplus_expand_constant(t);
	      gs_set_operand((gs_t) GS_NODE(t), GS_EXPANDED_PTRMEM_CST,
			 gs_x_1(t2, seq_num));
	    }
  	    break;

          case REAL_CST:
            {
  	      REAL_VALUE_TYPE d;
  	      char string[16*8];
              gs_t value_f;
              gs_t value_d;
              gs_t value_ld;
    
              _gs_bv (flags, GS_TREE_OVERFLOW, TREE_OVERFLOW (t));
    
              value_f =  __gs (IB_FLOAT);
              value_d =  __gs (IB_DOUBLE);
              value_ld = __gs (IB_LONG_DOUBLE);

  	      d = TREE_REAL_CST (t);
              _gs_bv (flags, GS_REAL_VALUE_ISINF, REAL_VALUE_ISINF (d));
              _gs_bv (flags, GS_REAL_VALUE_ISNAN, REAL_VALUE_ISNAN (d));
	      real_to_decimal (string, &d, sizeof (string), 0, 1);

              _gs_f (value_f, strtof ((string), NULL));
              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_REAL_CST_F, value_f);
              _gs_d (value_d, strtod ((string), NULL));
              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_REAL_CST_D, value_d);
              _gs_ld (value_ld, strtold ((string), NULL));
              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_REAL_CST_LD, value_ld);
            }
	    break;

	  case VECTOR_CST:
	    {
              /* Does this work? */
              /* TREE_VECTOR_CST_ELTS */
	      gs_set_operand((gs_t) GS_NODE(t), GS_TREE_VECTOR_CST_ELTS,
			 gs_x_1(TREE_VECTOR_CST_ELTS(t), seq_num));
	    }
	    break;

	  case COMPLEX_CST:
            /* TREE_REALPART */
            /* TREE_IMAGPART */
	    gs_set_operand((gs_t) GS_NODE(t), GS_TREE_REALPART,
		       gs_x_1(TREE_REALPART(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_TREE_IMAGPART,
		       gs_x_1(TREE_IMAGPART(t), seq_num));
	    break;

	  case STRING_CST:
            /* string_constant */
	    {
              gs_t string_pointer, string_length;

              string_pointer = __gs (IB_STRING);
              /* bug 11180: TREE_STRING_LENGTH does not include '\0'. */
              _gs_s (string_pointer, (gs_string_t) TREE_STRING_POINTER (t),
		     1 + TREE_STRING_LENGTH (t));
              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_STRING_POINTER, string_pointer);

              string_length = __gs (IB_INT);
              _gs_n (string_length, TREE_STRING_LENGTH (t));
              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_STRING_LENGTH, string_length);
            }
	    break;

	  case IDENTIFIER_NODE:

          /* gs_tree_symbol_referenced uses the GS_TREE_STATIC flag */
          _gs_bv (flags, GS_TREE_STATIC, TREE_SYMBOL_REFERENCED(t));

#define BUILTIN_PREFIXED(builtin)                           \
     (strcmp (builtin, 10 + IDENTIFIER_POINTER (t)) == 0) { \
    _gs_s (name, (gs_string_t) builtin, sizeof (builtin));                \
    goto ATTACH_ID_PTR;                                     \
  }

            /* name */
            {
               gs_t name;
               name = __gs (IB_STRING);
               if (strncmp ("__builtin_", IDENTIFIER_POINTER (t), 10) == 0) {

                      if BUILTIN_PREFIXED ("acos")
                 else if BUILTIN_PREFIXED ("acosf")
                 else if BUILTIN_PREFIXED ("acosh")
                 else if BUILTIN_PREFIXED ("acoshf")
                 else if BUILTIN_PREFIXED ("acoshl")
                 else if BUILTIN_PREFIXED ("acosl")
                 else if BUILTIN_PREFIXED ("asin")
                 else if BUILTIN_PREFIXED ("asinf")
                 else if BUILTIN_PREFIXED ("asinh")
                 else if BUILTIN_PREFIXED ("asinhf")
                 else if BUILTIN_PREFIXED ("asinhl")
                 else if BUILTIN_PREFIXED ("asinl")
                 else if BUILTIN_PREFIXED ("atan")
                 else if BUILTIN_PREFIXED ("atan2")
                 else if BUILTIN_PREFIXED ("atan2f")
                 else if BUILTIN_PREFIXED ("atan2l")
                 else if BUILTIN_PREFIXED ("atanf")
                 else if BUILTIN_PREFIXED ("atanh")
                 else if BUILTIN_PREFIXED ("atanhf")
                 else if BUILTIN_PREFIXED ("atanhl")
                 else if BUILTIN_PREFIXED ("atanl")
                 else if BUILTIN_PREFIXED ("cbrt")
                 else if BUILTIN_PREFIXED ("cbrtf")
                 else if BUILTIN_PREFIXED ("cbrtl")
                 else if BUILTIN_PREFIXED ("ceil")
                 else if BUILTIN_PREFIXED ("ceilf")
                 else if BUILTIN_PREFIXED ("ceill")
                 else if BUILTIN_PREFIXED ("copysign")
                 else if BUILTIN_PREFIXED ("copysignf")
                 else if BUILTIN_PREFIXED ("copysignl")
                 else if BUILTIN_PREFIXED ("cos")
                 else if BUILTIN_PREFIXED ("cosf")
                 else if BUILTIN_PREFIXED ("cosh")
                 else if BUILTIN_PREFIXED ("coshf")
                 else if BUILTIN_PREFIXED ("coshl")
                 else if BUILTIN_PREFIXED ("cosl")
                 else if BUILTIN_PREFIXED ("drem")
                 else if BUILTIN_PREFIXED ("dremf")
                 else if BUILTIN_PREFIXED ("dreml")
                 else if BUILTIN_PREFIXED ("erf")
                 else if BUILTIN_PREFIXED ("erfc")
                 else if BUILTIN_PREFIXED ("erfcf")
                 else if BUILTIN_PREFIXED ("erfcl")
                 else if BUILTIN_PREFIXED ("erff")
                 else if BUILTIN_PREFIXED ("erfl")
                 else if BUILTIN_PREFIXED ("exp")
                 else if BUILTIN_PREFIXED ("exp10")
                 else if BUILTIN_PREFIXED ("exp10f")
                 else if BUILTIN_PREFIXED ("exp10l")
                 else if BUILTIN_PREFIXED ("exp2")
                 else if BUILTIN_PREFIXED ("exp2f")
                 else if BUILTIN_PREFIXED ("exp2l")
                 else if BUILTIN_PREFIXED ("expf")
                 else if BUILTIN_PREFIXED ("expl")
                 else if BUILTIN_PREFIXED ("expm1")
                 else if BUILTIN_PREFIXED ("expm1f")
                 else if BUILTIN_PREFIXED ("expm1l")
                 else if BUILTIN_PREFIXED ("fabs")
                 else if BUILTIN_PREFIXED ("fabsf")
                 else if BUILTIN_PREFIXED ("fabsl")
                 else if BUILTIN_PREFIXED ("fdim")
                 else if BUILTIN_PREFIXED ("fdimf")
                 else if BUILTIN_PREFIXED ("fdiml")
                 else if BUILTIN_PREFIXED ("floor")
                 else if BUILTIN_PREFIXED ("floorf")
                 else if BUILTIN_PREFIXED ("floorl")
                 else if BUILTIN_PREFIXED ("fma")
                 else if BUILTIN_PREFIXED ("fmaf")
                 else if BUILTIN_PREFIXED ("fmal")
                 else if BUILTIN_PREFIXED ("fmax")
                 else if BUILTIN_PREFIXED ("fmaxf")
                 else if BUILTIN_PREFIXED ("fmaxl")
                 else if BUILTIN_PREFIXED ("fmin")
                 else if BUILTIN_PREFIXED ("fminf")
                 else if BUILTIN_PREFIXED ("fminl")
                 else if BUILTIN_PREFIXED ("fmod")
                 else if BUILTIN_PREFIXED ("fmodf")
                 else if BUILTIN_PREFIXED ("fmodl")
                 else if BUILTIN_PREFIXED ("frexp")
                 else if BUILTIN_PREFIXED ("frexpf")
                 else if BUILTIN_PREFIXED ("frexpl")
                 else if BUILTIN_PREFIXED ("gamma")
                 else if BUILTIN_PREFIXED ("gammaf")
                 else if BUILTIN_PREFIXED ("gammal")
                 else if BUILTIN_PREFIXED ("huge_val")
                 else if BUILTIN_PREFIXED ("huge_valf")
                 else if BUILTIN_PREFIXED ("huge_vall")
                 else if BUILTIN_PREFIXED ("hypot")
                 else if BUILTIN_PREFIXED ("hypotf")
                 else if BUILTIN_PREFIXED ("hypotl")
                 else if BUILTIN_PREFIXED ("ilogb")
                 else if BUILTIN_PREFIXED ("ilogbf")
                 else if BUILTIN_PREFIXED ("ilogbl")
                 else if BUILTIN_PREFIXED ("inf")
                 else if BUILTIN_PREFIXED ("inff")
                 else if BUILTIN_PREFIXED ("infl")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("infd32")
                 else if BUILTIN_PREFIXED ("infd64")
                 else if BUILTIN_PREFIXED ("infd128")
#endif
                 else if BUILTIN_PREFIXED ("j0")
                 else if BUILTIN_PREFIXED ("j0f")
                 else if BUILTIN_PREFIXED ("j0l")
                 else if BUILTIN_PREFIXED ("j1")
                 else if BUILTIN_PREFIXED ("j1f")
                 else if BUILTIN_PREFIXED ("j1l")
                 else if BUILTIN_PREFIXED ("jn")
                 else if BUILTIN_PREFIXED ("jnf")
                 else if BUILTIN_PREFIXED ("jnl")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("lceil")
                 else if BUILTIN_PREFIXED ("lceilf")
                 else if BUILTIN_PREFIXED ("lceill")
#endif
                 else if BUILTIN_PREFIXED ("ldexp")
                 else if BUILTIN_PREFIXED ("ldexpf")
                 else if BUILTIN_PREFIXED ("ldexpl")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("lfloor")
                 else if BUILTIN_PREFIXED ("lfloorf")
                 else if BUILTIN_PREFIXED ("lfloorl")
#endif
                 else if BUILTIN_PREFIXED ("lgamma")
                 else if BUILTIN_PREFIXED ("lgammaf")
                 else if BUILTIN_PREFIXED ("lgammal")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("llceil")
                 else if BUILTIN_PREFIXED ("llceilf")
                 else if BUILTIN_PREFIXED ("llceill")
                 else if BUILTIN_PREFIXED ("llfloor")
                 else if BUILTIN_PREFIXED ("llfloorf")
                 else if BUILTIN_PREFIXED ("llfloorl")
#endif
                 else if BUILTIN_PREFIXED ("llrint")
                 else if BUILTIN_PREFIXED ("llrintf")
                 else if BUILTIN_PREFIXED ("llrintl")
                 else if BUILTIN_PREFIXED ("llround")
                 else if BUILTIN_PREFIXED ("llroundf")
                 else if BUILTIN_PREFIXED ("llroundl")
                 else if BUILTIN_PREFIXED ("log")
                 else if BUILTIN_PREFIXED ("log10")
                 else if BUILTIN_PREFIXED ("log10f")
                 else if BUILTIN_PREFIXED ("log10l")
                 else if BUILTIN_PREFIXED ("log1p")
                 else if BUILTIN_PREFIXED ("log1pf")
                 else if BUILTIN_PREFIXED ("log1pl")
                 else if BUILTIN_PREFIXED ("log2")
                 else if BUILTIN_PREFIXED ("log2f")
                 else if BUILTIN_PREFIXED ("log2l")
                 else if BUILTIN_PREFIXED ("logb")
                 else if BUILTIN_PREFIXED ("logbf")
                 else if BUILTIN_PREFIXED ("logbl")
                 else if BUILTIN_PREFIXED ("logf")
                 else if BUILTIN_PREFIXED ("logl")
                 else if BUILTIN_PREFIXED ("lrint")
                 else if BUILTIN_PREFIXED ("lrintf")
                 else if BUILTIN_PREFIXED ("lrintl")
                 else if BUILTIN_PREFIXED ("lround")
                 else if BUILTIN_PREFIXED ("lroundf")
                 else if BUILTIN_PREFIXED ("lroundl")
                 else if BUILTIN_PREFIXED ("modf")
                 else if BUILTIN_PREFIXED ("modff")
                 else if BUILTIN_PREFIXED ("modfl")
                 else if BUILTIN_PREFIXED ("nan")
                 else if BUILTIN_PREFIXED ("nanf")
                 else if BUILTIN_PREFIXED ("nanl")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("nand32")
                 else if BUILTIN_PREFIXED ("nand64")
                 else if BUILTIN_PREFIXED ("nand128")
#endif
                 else if BUILTIN_PREFIXED ("nans")
                 else if BUILTIN_PREFIXED ("nansf")
                 else if BUILTIN_PREFIXED ("nansl")
                 else if BUILTIN_PREFIXED ("nearbyint")
                 else if BUILTIN_PREFIXED ("nearbyintf")
                 else if BUILTIN_PREFIXED ("nearbyintl")
                 else if BUILTIN_PREFIXED ("nextafter")
                 else if BUILTIN_PREFIXED ("nextafterf")
                 else if BUILTIN_PREFIXED ("nextafterl")
                 else if BUILTIN_PREFIXED ("nexttoward")
                 else if BUILTIN_PREFIXED ("nexttowardf")
                 else if BUILTIN_PREFIXED ("nexttowardl")
                 else if BUILTIN_PREFIXED ("pow")
                 else if BUILTIN_PREFIXED ("pow10")
                 else if BUILTIN_PREFIXED ("pow10f")
                 else if BUILTIN_PREFIXED ("pow10l")
                 else if BUILTIN_PREFIXED ("powf")
                 else if BUILTIN_PREFIXED ("powi")
                 else if BUILTIN_PREFIXED ("powif")
                 else if BUILTIN_PREFIXED ("powil")
                 else if BUILTIN_PREFIXED ("powl")
                 else if BUILTIN_PREFIXED ("remainder")
                 else if BUILTIN_PREFIXED ("remainderf")
                 else if BUILTIN_PREFIXED ("remainderl")
                 else if BUILTIN_PREFIXED ("remquo")
                 else if BUILTIN_PREFIXED ("remquof")
                 else if BUILTIN_PREFIXED ("remquol")
                 else if BUILTIN_PREFIXED ("rint")
                 else if BUILTIN_PREFIXED ("rintf")
                 else if BUILTIN_PREFIXED ("rintl")
                 else if BUILTIN_PREFIXED ("round")
                 else if BUILTIN_PREFIXED ("roundf")
                 else if BUILTIN_PREFIXED ("roundl")
                 else if BUILTIN_PREFIXED ("scalb")
                 else if BUILTIN_PREFIXED ("scalbf")
                 else if BUILTIN_PREFIXED ("scalbl")
                 else if BUILTIN_PREFIXED ("scalbln")
                 else if BUILTIN_PREFIXED ("scalblnf")
                 else if BUILTIN_PREFIXED ("scalblnl")
                 else if BUILTIN_PREFIXED ("scalbn")
                 else if BUILTIN_PREFIXED ("scalbnf")
                 else if BUILTIN_PREFIXED ("scalbnl")
                 else if BUILTIN_PREFIXED ("signbit")
                 else if BUILTIN_PREFIXED ("signbitf")
                 else if BUILTIN_PREFIXED ("signbitl")
                 else if BUILTIN_PREFIXED ("significand")
                 else if BUILTIN_PREFIXED ("significandf")
                 else if BUILTIN_PREFIXED ("significandl")
                 else if BUILTIN_PREFIXED ("sin")
                 else if BUILTIN_PREFIXED ("sincos")
                 else if BUILTIN_PREFIXED ("sincosf")
                 else if BUILTIN_PREFIXED ("sincosl")
                 else if BUILTIN_PREFIXED ("sinf")
                 else if BUILTIN_PREFIXED ("sinh")
                 else if BUILTIN_PREFIXED ("sinhf")
                 else if BUILTIN_PREFIXED ("sinhl")
                 else if BUILTIN_PREFIXED ("sinl")
                 else if BUILTIN_PREFIXED ("sqrt")
                 else if BUILTIN_PREFIXED ("sqrtf")
                 else if BUILTIN_PREFIXED ("sqrtl")
                 else if BUILTIN_PREFIXED ("tan")
                 else if BUILTIN_PREFIXED ("tanf")
                 else if BUILTIN_PREFIXED ("tanh")
                 else if BUILTIN_PREFIXED ("tanhf")
                 else if BUILTIN_PREFIXED ("tanhl")
                 else if BUILTIN_PREFIXED ("tanl")
                 else if BUILTIN_PREFIXED ("tgamma")
                 else if BUILTIN_PREFIXED ("tgammaf")
                 else if BUILTIN_PREFIXED ("tgammal")
                 else if BUILTIN_PREFIXED ("trunc")
                 else if BUILTIN_PREFIXED ("truncf")
                 else if BUILTIN_PREFIXED ("truncl")
                 else if BUILTIN_PREFIXED ("y0")
                 else if BUILTIN_PREFIXED ("y0f")
                 else if BUILTIN_PREFIXED ("y0l")
                 else if BUILTIN_PREFIXED ("y1")
                 else if BUILTIN_PREFIXED ("y1f")
                 else if BUILTIN_PREFIXED ("y1l")
                 else if BUILTIN_PREFIXED ("yn")
                 else if BUILTIN_PREFIXED ("ynf")
                 else if BUILTIN_PREFIXED ("ynl")
                 else if BUILTIN_PREFIXED ("cabs")
                 else if BUILTIN_PREFIXED ("cabsf")
                 else if BUILTIN_PREFIXED ("cabsl")
                 else if BUILTIN_PREFIXED ("cacos")
                 else if BUILTIN_PREFIXED ("cacosf")
                 else if BUILTIN_PREFIXED ("cacosh")
                 else if BUILTIN_PREFIXED ("cacoshf")
                 else if BUILTIN_PREFIXED ("cacoshl")
                 else if BUILTIN_PREFIXED ("cacosl")
                 else if BUILTIN_PREFIXED ("carg")
                 else if BUILTIN_PREFIXED ("cargf")
                 else if BUILTIN_PREFIXED ("cargl")
                 else if BUILTIN_PREFIXED ("casin")
                 else if BUILTIN_PREFIXED ("casinf")
                 else if BUILTIN_PREFIXED ("casinh")
                 else if BUILTIN_PREFIXED ("casinhf")
                 else if BUILTIN_PREFIXED ("casinhl")
                 else if BUILTIN_PREFIXED ("casinl")
                 else if BUILTIN_PREFIXED ("catan")
                 else if BUILTIN_PREFIXED ("catanf")
                 else if BUILTIN_PREFIXED ("catanh")
                 else if BUILTIN_PREFIXED ("catanhf")
                 else if BUILTIN_PREFIXED ("catanhl")
                 else if BUILTIN_PREFIXED ("catanl")
                 else if BUILTIN_PREFIXED ("ccos")
                 else if BUILTIN_PREFIXED ("ccosf")
                 else if BUILTIN_PREFIXED ("ccosh")
                 else if BUILTIN_PREFIXED ("ccoshf")
                 else if BUILTIN_PREFIXED ("ccoshl")
                 else if BUILTIN_PREFIXED ("ccosl")
                 else if BUILTIN_PREFIXED ("cexp")
                 else if BUILTIN_PREFIXED ("cexpf")
                 else if BUILTIN_PREFIXED ("cexpl")
                 else if BUILTIN_PREFIXED ("cimag")
                 else if BUILTIN_PREFIXED ("cimagf")
                 else if BUILTIN_PREFIXED ("cimagl")
                 else if BUILTIN_PREFIXED ("clog")
                 else if BUILTIN_PREFIXED ("clogf")
                 else if BUILTIN_PREFIXED ("clogl")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("clog10")
                 else if BUILTIN_PREFIXED ("clog10f")
                 else if BUILTIN_PREFIXED ("clog10l")
#endif
                 else if BUILTIN_PREFIXED ("conj")
                 else if BUILTIN_PREFIXED ("conjf")
                 else if BUILTIN_PREFIXED ("conjl")
                 else if BUILTIN_PREFIXED ("cpow")
                 else if BUILTIN_PREFIXED ("cpowf")
                 else if BUILTIN_PREFIXED ("cpowl")
                 else if BUILTIN_PREFIXED ("cproj")
                 else if BUILTIN_PREFIXED ("cprojf")
                 else if BUILTIN_PREFIXED ("cprojl")
                 else if BUILTIN_PREFIXED ("creal")
                 else if BUILTIN_PREFIXED ("crealf")
                 else if BUILTIN_PREFIXED ("creall")
                 else if BUILTIN_PREFIXED ("csin")
                 else if BUILTIN_PREFIXED ("csinf")
                 else if BUILTIN_PREFIXED ("csinh")
                 else if BUILTIN_PREFIXED ("csinhf")
                 else if BUILTIN_PREFIXED ("csinhl")
                 else if BUILTIN_PREFIXED ("csinl")
                 else if BUILTIN_PREFIXED ("csqrt")
                 else if BUILTIN_PREFIXED ("csqrtf")
                 else if BUILTIN_PREFIXED ("csqrtl")
                 else if BUILTIN_PREFIXED ("ctan")
                 else if BUILTIN_PREFIXED ("ctanf")
                 else if BUILTIN_PREFIXED ("ctanh")
                 else if BUILTIN_PREFIXED ("ctanhf")
                 else if BUILTIN_PREFIXED ("ctanhl")
                 else if BUILTIN_PREFIXED ("ctanl")
                 else if BUILTIN_PREFIXED ("bcmp")
                 else if BUILTIN_PREFIXED ("bcopy")
                 else if BUILTIN_PREFIXED ("bzero")
                 else if BUILTIN_PREFIXED ("index")
                 else if BUILTIN_PREFIXED ("memcmp")
                 else if BUILTIN_PREFIXED ("memcpy")
                 else if BUILTIN_PREFIXED ("memmove")
                 else if BUILTIN_PREFIXED ("mempcpy")
                 else if BUILTIN_PREFIXED ("memset")
                 else if BUILTIN_PREFIXED ("rindex")
                 else if BUILTIN_PREFIXED ("stpcpy")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("stpncpy")
                 else if BUILTIN_PREFIXED ("strcasecmp")
#endif
                 else if BUILTIN_PREFIXED ("strcat")
                 else if BUILTIN_PREFIXED ("strchr")
                 else if BUILTIN_PREFIXED ("strcmp")
                 else if BUILTIN_PREFIXED ("strcpy")
                 else if BUILTIN_PREFIXED ("strcspn")
                 else if BUILTIN_PREFIXED ("strdup")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("strndup")
#endif
                 else if BUILTIN_PREFIXED ("strlen")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("strncasecmp")
#endif
                 else if BUILTIN_PREFIXED ("strncat")
                 else if BUILTIN_PREFIXED ("strncmp")
                 else if BUILTIN_PREFIXED ("strncpy")
                 else if BUILTIN_PREFIXED ("strpbrk")
                 else if BUILTIN_PREFIXED ("strrchr")
                 else if BUILTIN_PREFIXED ("strspn")
                 else if BUILTIN_PREFIXED ("strstr")
                 else if BUILTIN_PREFIXED ("fprintf")
                 else if BUILTIN_PREFIXED ("fprintf_unlocked")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("putc")
                 else if BUILTIN_PREFIXED ("putc_unlocked")
#endif
                 else if BUILTIN_PREFIXED ("fputc")
                 else if BUILTIN_PREFIXED ("fputc_unlocked")
                 else if BUILTIN_PREFIXED ("fputs")
                 else if BUILTIN_PREFIXED ("fputs_unlocked")
                 else if BUILTIN_PREFIXED ("fscanf")
                 else if BUILTIN_PREFIXED ("fwrite")
                 else if BUILTIN_PREFIXED ("fwrite_unlocked")
                 else if BUILTIN_PREFIXED ("printf")
                 else if BUILTIN_PREFIXED ("printf_unlocked")
                 else if BUILTIN_PREFIXED ("putchar")
                 else if BUILTIN_PREFIXED ("putchar_unlocked")
                 else if BUILTIN_PREFIXED ("puts")
                 else if BUILTIN_PREFIXED ("puts_unlocked")
                 else if BUILTIN_PREFIXED ("scanf")
                 else if BUILTIN_PREFIXED ("snprintf")
                 else if BUILTIN_PREFIXED ("sprintf")
                 else if BUILTIN_PREFIXED ("sscanf")
                 else if BUILTIN_PREFIXED ("vfprintf")
                 else if BUILTIN_PREFIXED ("vfscanf")
                 else if BUILTIN_PREFIXED ("vprintf")
                 else if BUILTIN_PREFIXED ("vscanf")
                 else if BUILTIN_PREFIXED ("vsnprintf")
                 else if BUILTIN_PREFIXED ("vsprintf")
                 else if BUILTIN_PREFIXED ("vsscanf")
                 else if BUILTIN_PREFIXED ("isalnum")
                 else if BUILTIN_PREFIXED ("isalpha")
                 else if BUILTIN_PREFIXED ("isascii")
                 else if BUILTIN_PREFIXED ("isblank")
                 else if BUILTIN_PREFIXED ("iscntrl")
                 else if BUILTIN_PREFIXED ("isdigit")
                 else if BUILTIN_PREFIXED ("isgraph")
                 else if BUILTIN_PREFIXED ("islower")
                 else if BUILTIN_PREFIXED ("isprint")
                 else if BUILTIN_PREFIXED ("ispunct")
                 else if BUILTIN_PREFIXED ("isspace")
                 else if BUILTIN_PREFIXED ("isupper")
                 else if BUILTIN_PREFIXED ("isxdigit")
                 else if BUILTIN_PREFIXED ("toascii")
                 else if BUILTIN_PREFIXED ("tolower")
                 else if BUILTIN_PREFIXED ("toupper")
                 else if BUILTIN_PREFIXED ("iswalnum")
                 else if BUILTIN_PREFIXED ("iswalpha")
                 else if BUILTIN_PREFIXED ("iswblank")
                 else if BUILTIN_PREFIXED ("iswcntrl")
                 else if BUILTIN_PREFIXED ("iswdigit")
                 else if BUILTIN_PREFIXED ("iswgraph")
                 else if BUILTIN_PREFIXED ("iswlower")
                 else if BUILTIN_PREFIXED ("iswprint")
                 else if BUILTIN_PREFIXED ("iswpunct")
                 else if BUILTIN_PREFIXED ("iswspace")
                 else if BUILTIN_PREFIXED ("iswupper")
                 else if BUILTIN_PREFIXED ("iswxdigit")
                 else if BUILTIN_PREFIXED ("towlower")
                 else if BUILTIN_PREFIXED ("towupper")
                 else if BUILTIN_PREFIXED ("abort")
                 else if BUILTIN_PREFIXED ("abs")
                 else if BUILTIN_PREFIXED ("aggregate_incoming_address")
                 else if BUILTIN_PREFIXED ("alloca")
                 else if BUILTIN_PREFIXED ("apply")
                 else if BUILTIN_PREFIXED ("apply_args")
                 else if BUILTIN_PREFIXED ("args_info")
                 else if BUILTIN_PREFIXED ("calloc")
                 else if BUILTIN_PREFIXED ("classify_type")
                 else if BUILTIN_PREFIXED ("clz")
                 else if BUILTIN_PREFIXED ("clzimax")
                 else if BUILTIN_PREFIXED ("clzl")
                 else if BUILTIN_PREFIXED ("clzll")
                 else if BUILTIN_PREFIXED ("constant_p")
                 else if BUILTIN_PREFIXED ("ctz")
                 else if BUILTIN_PREFIXED ("ctzimax")
                 else if BUILTIN_PREFIXED ("ctzl")
                 else if BUILTIN_PREFIXED ("ctzll")
                 else if BUILTIN_PREFIXED ("dcgettext")
                 else if BUILTIN_PREFIXED ("dgettext")
                 else if BUILTIN_PREFIXED ("dwarf_cfa")
                 else if BUILTIN_PREFIXED ("dwarf_sp_column")
                 else if BUILTIN_PREFIXED ("eh_return")
                 else if BUILTIN_PREFIXED ("eh_return_data_regno")
                 else if BUILTIN_PREFIXED ("execl")
                 else if BUILTIN_PREFIXED ("execlp")
                 else if BUILTIN_PREFIXED ("execle")
                 else if BUILTIN_PREFIXED ("execv")
                 else if BUILTIN_PREFIXED ("execvp")
                 else if BUILTIN_PREFIXED ("execve")
                 else if BUILTIN_PREFIXED ("exit")
                 else if BUILTIN_PREFIXED ("expect")
                 else if BUILTIN_PREFIXED ("extend_pointer")
                 else if BUILTIN_PREFIXED ("extract_return_addr")
                 else if BUILTIN_PREFIXED ("ffs")
                 else if BUILTIN_PREFIXED ("ffsimax")
                 else if BUILTIN_PREFIXED ("ffsl")
                 else if BUILTIN_PREFIXED ("ffsll")
                 else if BUILTIN_PREFIXED ("fork")
                 else if BUILTIN_PREFIXED ("frame_address")
                 else if BUILTIN_PREFIXED ("frob_return_addr")
                 else if BUILTIN_PREFIXED ("gettext")
                 else if BUILTIN_PREFIXED ("imaxabs")
                 else if BUILTIN_PREFIXED ("init_dwarf_reg_size_table")
                 else if BUILTIN_PREFIXED ("finite")
                 else if BUILTIN_PREFIXED ("finitef")
                 else if BUILTIN_PREFIXED ("finitel")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("finited32")
                 else if BUILTIN_PREFIXED ("finited64")
                 else if BUILTIN_PREFIXED ("finited128")
#endif
                 else if BUILTIN_PREFIXED ("isinf")
                 else if BUILTIN_PREFIXED ("isinff")
                 else if BUILTIN_PREFIXED ("isinfl")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("isinfd32")
                 else if BUILTIN_PREFIXED ("isinfd64")
                 else if BUILTIN_PREFIXED ("isinfd128")
#endif
                 else if BUILTIN_PREFIXED ("isnan")
                 else if BUILTIN_PREFIXED ("isnanf")
                 else if BUILTIN_PREFIXED ("isnanl")
#ifdef FE_GNU_4_2_0
                 else if BUILTIN_PREFIXED ("isnand32")
                 else if BUILTIN_PREFIXED ("isnand64")
                 else if BUILTIN_PREFIXED ("isnand128")
#endif
                 else if BUILTIN_PREFIXED ("isgreater")
                 else if BUILTIN_PREFIXED ("isgreaterequal")
                 else if BUILTIN_PREFIXED ("isless")
                 else if BUILTIN_PREFIXED ("islessequal")
                 else if BUILTIN_PREFIXED ("islessgreater")
                 else if BUILTIN_PREFIXED ("isunordered")
                 else if BUILTIN_PREFIXED ("labs")
                 else if BUILTIN_PREFIXED ("llabs")
                 else if BUILTIN_PREFIXED ("longjmp")
                 else if BUILTIN_PREFIXED ("malloc")
                 else if BUILTIN_PREFIXED ("next_arg")
                 else if BUILTIN_PREFIXED ("parity")
                 else if BUILTIN_PREFIXED ("parityimax")
                 else if BUILTIN_PREFIXED ("parityl")
                 else if BUILTIN_PREFIXED ("parityll")
                 else if BUILTIN_PREFIXED ("popcount")
                 else if BUILTIN_PREFIXED ("popcountimax")
                 else if BUILTIN_PREFIXED ("popcountl")
                 else if BUILTIN_PREFIXED ("popcountll")
                 else if BUILTIN_PREFIXED ("prefetch")
                 else if BUILTIN_PREFIXED ("return")
                 else if BUILTIN_PREFIXED ("return_address")
                 else if BUILTIN_PREFIXED ("saveregs")
                 else if BUILTIN_PREFIXED ("setjmp")
                 else if BUILTIN_PREFIXED ("stdarg_start")
                 else if BUILTIN_PREFIXED ("strfmon")
                 else if BUILTIN_PREFIXED ("strftime")
                 else if BUILTIN_PREFIXED ("trap")
                 else if BUILTIN_PREFIXED ("unwind_init")
                 else if BUILTIN_PREFIXED ("update_setjmp_buf")
                 else if BUILTIN_PREFIXED ("va_copy")
                 else if BUILTIN_PREFIXED ("va_end")
                 else if BUILTIN_PREFIXED ("va_start")
                 else if BUILTIN_PREFIXED ("_exit")
                 else if BUILTIN_PREFIXED ("_Exit")

                 /* Else, it's a builtin-type or something that we do not care converting
                  * like the other builtin's. */

                 else goto SET_ID_PTR;

               }
               else {
                 SET_ID_PTR:
                 _gs_s (name, (gs_string_t) IDENTIFIER_POINTER (t),
			1 + strlen (IDENTIFIER_POINTER (t)));
               }
               ATTACH_ID_PTR:
	       gs_set_operand ((gs_t) GS_NODE (t), GS_IDENTIFIER_POINTER, name);
            }
	    break;

	  case TREE_LIST:
            /* TREE_PURPOSE */
            /* TREE_VALUE */
	    gs_set_operand((gs_t) GS_NODE(t), GS_TREE_PURPOSE,
		       gs_x_1(TREE_PURPOSE(t), seq_num));
	    /* bug 8346 */
	    if (TREE_VALUE(t))
	      STRIP_USELESS_TYPE_CONVERSION(TREE_VALUE(t));
	    gs_set_operand((gs_t) GS_NODE(t), GS_TREE_VALUE,
		       gs_x_1(TREE_VALUE(t), seq_num));
	    break;

	  case TREE_VEC:
            {
              /* TREE_VEC_LENGTH */
              /* TREE_VEC_ELT */
              int i, len;
              gs_t tree_vec_elt_list;
              {
                gs_t length;
                length = __gs (IB_INT);
                _gs_n (length, TREE_VEC_LENGTH (t));
                gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_VEC_LENGTH, length);
              }
              len = TREE_VEC_LENGTH (t);
              tree_vec_elt_list = __gs (EMPTY);
              for (i = 0; i < len; i++)
                tree_vec_elt_list = gs_cons(gs_x_1(TREE_VEC_ELT(t, i), seq_num),
					    tree_vec_elt_list);
              gs_set_operand ((gs_t) GS_NODE (t), GS_TREE_VEC_ELT, tree_vec_elt_list);
            }
	    break;

	  case BLOCK:
            /* BLOCK_VARS */
            /* BLOCK_SUPERCONTEXT */
            /* BLOCK_SUBBLOCKS */
            /* BLOCK_CHAIN */
            /* BLOCK_ABSTRACT_ORIGIN */
	    gs_set_operand((gs_t) GS_NODE(t), GS_BLOCK_VARS,
		       gs_x_1(BLOCK_VARS(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_BLOCK_SUPERCONTEXT,
		       gs_x_1(BLOCK_SUPERCONTEXT(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_BLOCK_SUBBLOCKS,
		       gs_x_1(BLOCK_SUBBLOCKS(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_BLOCK_CHAIN,
		       gs_x_1(BLOCK_CHAIN(t), seq_num));
	    gs_set_operand((gs_t) GS_NODE(t), GS_BLOCK_ABSTRACT_ORIGIN,
		       gs_x_1(BLOCK_ABSTRACT_ORIGIN(t), seq_num));
	    break;

	  case SSA_NAME:
	    break;

          case STATEMENT_LIST:
            {
              tree_stmt_iterator i;
              gs_t stmt_list = __gs (EMPTY);
              for (i = tsi_last (t); (i.ptr) != NULL; tsi_prev (&i)) {
                stmt_list = gs_cons(gs_x_1(tsi_stmt(i), seq_num), stmt_list);
              }
              gs_set_operand ((gs_t) GS_NODE (t), GS_STATEMENT_LIST_ELTS, stmt_list);
            }
	    break;

          case TREE_BINFO:
            gs_set_operand((gs_t) GS_NODE(t), GS_BINFO_TYPE,
		       gs_x_1(BINFO_TYPE(t), seq_num));
            gs_set_operand((gs_t) GS_NODE(t), GS_BINFO_VPTR_FIELD,
		       gs_x_1(BINFO_VPTR_FIELD(t), seq_num));
            _gs_bv(flags, GS_BINFO_VIRTUAL_P, BINFO_VIRTUAL_P(t));

	    /* Traverse the vector of base infos.  Put the base infos on a
	     * gspin list. */
	    {
	      gs_t list = __gs(EMPTY);
	      unsigned int i;
	      for (i = 0; i < BINFO_N_BASE_BINFOS(t); i++) {
		tree base_binfo = BINFO_BASE_BINFO(t, i);
		list = gs_cons(gs_x_1(base_binfo, seq_num), list);
	      }
	      gs_set_operand((gs_t) GS_NODE(t), GS_BINFO_BASE_BINFOS, list);
	    }
	    break;

          case TEMPLATE_PARM_INDEX:
            {
              gs_t tpi_idx, tpi_level, tpi_orig_level;

              tpi_idx = __gs (IB_INT);
              _gs_n (tpi_idx, TEMPLATE_PARM_IDX (t));

              tpi_level = __gs (IB_INT);
              _gs_n (tpi_level, TEMPLATE_PARM_LEVEL (t));

              tpi_orig_level = __gs (IB_INT);
              _gs_n (tpi_orig_level, TEMPLATE_PARM_ORIG_LEVEL (t));

              gs_set_operand((gs_t) GS_NODE(t), GS_TEMPLATE_PARM_IDX, tpi_idx);
              gs_set_operand((gs_t) GS_NODE(t), GS_TEMPLATE_PARM_LEVEL, tpi_level);
              gs_set_operand((gs_t) GS_NODE(t), GS_TEMPLATE_PARM_DESCENDANTS,
			 gs_x_1(TEMPLATE_PARM_DESCENDANTS(t), seq_num));
              gs_set_operand((gs_t) GS_NODE(t), GS_TEMPLATE_PARM_ORIG_LEVEL,
			 tpi_orig_level);
              gs_set_operand((gs_t) GS_NODE(t), GS_TEMPLATE_PARM_DECL,
			 gs_x_1(TEMPLATE_PARM_DECL(t), seq_num));
            }
	    break;

          case BASELINK:
            gs_set_operand((gs_t) GS_NODE(t), GS_BASELINK_BINFO,
		       gs_x_1(BASELINK_BINFO(t), seq_num));
            gs_set_operand((gs_t) GS_NODE(t), GS_BASELINK_FUNCTIONS,
		       gs_x_1(BASELINK_FUNCTIONS(t), seq_num));
            gs_set_operand((gs_t) GS_NODE(t), GS_BASELINK_ACCESS_BINFO,
		       gs_x_1(BASELINK_ACCESS_BINFO(t), seq_num));
            gs_set_operand((gs_t) GS_NODE(t), GS_BASELINK_OPTYPE,
		       gs_x_1(BASELINK_OPTYPE(t), seq_num));
	    break;

          case OVERLOAD:
	  /* OVERLOAD not yet used by wgen. OVL_FUNCTION and OVL_CURRENT
	   * returns tree_overload*, so we should check if the following
	   * disabled code are correct. */
#if 0
            gs_set_operand((gs_t) GS_NODE(t), GS_OVL_FUNCTION,
		       gs_x_1(OVL_FUNCTION(t), seq_num));
#endif
            gs_set_operand((gs_t) GS_NODE(t), GS_OVL_CHAIN,
		       gs_x_1(OVL_CHAIN(t), seq_num));
#if 0
            gs_set_operand((gs_t) GS_NODE(t), GS_OVL_CURRENT,
		       gs_x_1(OVL_CURRENT(t), seq_num));
#endif
            gs_set_operand((gs_t) GS_NODE(t), GS_OVL_NEXT,
		       gs_x_1(OVL_NEXT(t), seq_num));
	    break;

          case CONSTRUCTOR:
            {
              gs_t constructor_elts_index, constructor_elts_value;
              tree field, value;
              signed HOST_WIDE_INT idx;
              {
                gs_t length = __gs (IB_INT);
                _gs_n (length,
                       VEC_length (constructor_elt, CONSTRUCTOR_ELTS(t)));
                gs_set_operand ((gs_t) GS_NODE (t),
                                 GS_CONSTRUCTOR_LENGTH, length);
              }
              constructor_elts_index = __gs (EMPTY);
              constructor_elts_value = __gs (EMPTY);
              /* Traverse in reverse order to preserve the order of
               * initializations, because gs_cons reverses it again. */
              FOR_EACH_CONSTRUCTOR_ELT_R (CONSTRUCTOR_ELTS(t), idx, field, value)
              {
                constructor_elts_index = gs_cons(gs_x_1(field, seq_num),
                                                constructor_elts_index);
                constructor_elts_value = gs_cons(gs_x_1(value, seq_num),
                                                constructor_elts_value);
              }
              gs_set_operand ((gs_t) GS_NODE (t), GS_CONSTRUCTOR_ELTS_INDEX,
                              constructor_elts_index);
              gs_set_operand ((gs_t) GS_NODE (t), GS_CONSTRUCTOR_ELTS_VALUE,
                              constructor_elts_value);
            }
            break;

          case OMP_CLAUSE:
            {
              gs_t clause_code = __gs (IB_INT);
              _gs_n (clause_code,
                     gcc_omp_clause_code2gs_occ(OMP_CLAUSE_CODE(t)));
              gs_set_operand((gs_t) GS_NODE(t), GS_OMP_CLAUSE_CODE,
                             clause_code);
              /* OMP_CLAUSE_DECL, OMP_CLAUSE_NUM_THREADS_EXPR, OMP_CLAUSE_IF_EXPR */

              switch (OMP_CLAUSE_CODE(t))
              {
                case OMP_CLAUSE_REDUCTION:
                {
                  gs_t reduction_code = __gs (IB_INT);
                  _gs_n (reduction_code, gcc2gs(OMP_CLAUSE_REDUCTION_CODE(t)));
                  gs_set_operand((gs_t) GS_NODE(t),
                                 GS_OMP_CLAUSE_REDUCTION_CODE,
                                 reduction_code);
                  /* fall through */
                }
                case OMP_CLAUSE_COPYIN:
                case OMP_CLAUSE_PRIVATE:
                case OMP_CLAUSE_FIRSTPRIVATE:
                case OMP_CLAUSE_LASTPRIVATE:
                case OMP_CLAUSE_SHARED:
                case OMP_CLAUSE_COPYPRIVATE:
                  gs_set_operand((gs_t) GS_NODE(t), GS_OMP_CLAUSE_DECL,
                                 gs_x_1(OMP_CLAUSE_DECL(t), seq_num));
                  break;
                case OMP_CLAUSE_NUM_THREADS:
                  gs_set_operand((gs_t) GS_NODE(t),
                            GS_OMP_CLAUSE_NUM_THREADS_EXPR,
                            gs_x_1(OMP_CLAUSE_NUM_THREADS_EXPR(t), seq_num));
                  break;
                case OMP_CLAUSE_IF:
                  gs_set_operand((gs_t) GS_NODE(t), GS_OMP_CLAUSE_IF_EXPR,
                                 gs_x_1(OMP_CLAUSE_IF_EXPR(t), seq_num));
                  break;
                case OMP_CLAUSE_DEFAULT:
                {
                  gs_t default_kind = __gs (IB_INT);
                  _gs_n (default_kind,
                         gcc_omp_clause_default_kind2gs_ocdk(OMP_CLAUSE_DEFAULT_KIND(t)));
                  gs_set_operand((gs_t) GS_NODE(t), GS_OMP_CLAUSE_DEFAULT_KIND,
                                 default_kind);
                }
                break;

                case OMP_CLAUSE_NOWAIT:
                case OMP_CLAUSE_ORDERED:
                  /* Do nothing. */
                  break;

                case OMP_CLAUSE_SCHEDULE:
                {
                  gs_t schedule_kind = __gs (IB_INT);
                  _gs_n (schedule_kind,
          gcc_omp_clause_schedule_kind2gs_ocsk(OMP_CLAUSE_SCHEDULE_KIND(t)));
                  gs_set_operand((gs_t) GS_NODE(t), GS_OMP_CLAUSE_SCHEDULE_KIND,
                                 schedule_kind);

                  gs_set_operand((gs_t) GS_NODE(t),
                                 GS_OMP_CLAUSE_SCHEDULE_CHUNK_EXPR,
                           gs_x_1(OMP_CLAUSE_SCHEDULE_CHUNK_EXPR(t), seq_num));
                }
                break;

                default:
                  fprintf (stdout, "Unhandled OpenMP clause code\n");
                  gcc_assert (0);
              }
            }
            break;

	  default:
            fprintf (stdout,
	      "Unhandled case %s in gs_x_1. (tcc_constant/tcc_exceptional)\n",
	      tree_code_name[(int) TREE_CODE ((t))]);
            gcc_assert (0);
	    break;
      }
      break;
    }

  return (gs_t) GS_NODE (t);
}

gs_t
gs_x_func_decl (tree t)
{
  translate_func_decl = 1;	/* Translate FUNCTION_DECLs. */
  sequence_num++;
  return gs_x_1(t, sequence_num);
}

gs_t
gs_x (tree t)
{
  translate_func_decl = 0;	/* Don't translate FUNCTION_DECLs. */
  sequence_num++;
  return gs_x_1(t, sequence_num);
}

/* Like translate_func_decl, except for C++ thunk functions. Thunk
 * functions are fully translated only when this variable is set. */
static bool translate_thunk_decl = false;

/* Add T to list of decls emitted by g++. */
void
gspin_gxx_emits_decl (tree t)
{
  gs_t decl, decl_list;

  if (translate_thunk_decl) {
    /* Fully translate this thunk function. This is the only place other
     * than tree_rest_of_compilation() where we fully process a function
     * in C++. */
    translate_thunk_decl = false;
    decl = gs_x_func_decl(t);
  }
  else
    decl = gs_x(t);

  if (gs_code(gxx_emitted_decls_dot) == EMPTY) {
    _gs_code(gxx_emitted_decls_dot, CONS);
    gs_set_operand(gxx_emitted_decls_dot, 0, decl);
    gs_set_operand(gxx_emitted_decls_dot, 1, __gs(EMPTY));
    return;
  }
  decl_list = gs_cons(decl, gs_operand(gxx_emitted_decls_dot, 1));
  gs_set_operand(gxx_emitted_decls_dot, 1, decl_list);
  gxx_emitted_decls_dot = decl_list;
}

/* Called only from use_thunk() for C++. */
void
gspin_gxx_emits_thunk_decl (tree t)
{
  translate_thunk_decl = true;
  gspin_gxx_emits_decl(t);
}

/* Add T to list of asms emitted by g++. */
void
gspin_gxx_emits_asm (tree t)
{
  gs_t decl, decl_list;
  decl = gs_x(t);

  if (gs_code(program_decls_dot) == EMPTY) {
    _gs_code(program_decls_dot, CONS);
    gs_set_operand(program_decls_dot, 0, decl);
    gs_set_operand(program_decls_dot, 1, __gs(EMPTY));
    return;
  }
  decl_list = gs_cons(decl, gs_operand(program_decls_dot, 1));
  gs_set_operand(program_decls_dot, 1, decl_list);
  program_decls_dot = decl_list;
}

void
gspin_add_weak (tree decl, gs_t decl_node)
{
  gs_t list;

  GS_ASSERT(decl != NULL &&
	    (TREE_CODE(decl) == VAR_DECL || TREE_CODE(decl) == FUNCTION_DECL),
	    ("gspin_add_weak: bad decl"));

  if (DECL_ADDED_TO_WEAK_DECLS(decl))
    return;

  DECL_ADDED_TO_WEAK_DECLS(decl) = 1;

  if (gs_code(weak_decls_dot) == EMPTY) {
    _gs_code(weak_decls_dot, CONS);
    gs_set_operand(weak_decls_dot, 0, decl_node);
    gs_set_operand(weak_decls_dot, 1, __gs(EMPTY));
    return;
  }
  list = gs_cons(decl_node, gs_operand(weak_decls_dot, 1));
  gs_set_operand(weak_decls_dot, 1, list);
  weak_decls_dot = list;
}

/* Dump a STATEMENT_LIST for debugging. */
void
dump_statement_list (tree t)
{
  tree_stmt_iterator i;

  if (t &&
      TREE_CODE(t) == STATEMENT_LIST) {
    for (i = tsi_start(t); (i.ptr) != NULL; tsi_next(&i)) {
      tree item = tsi_stmt(i);
      if (item) {
	debug_tree(item);
      }
    }
  }
}

bool
lang_cplus (void)
{
  return CPR();
}
#endif
