/*
 * Copyright (C) 2007. PathScale, LLC.  All rights reserved.
 */
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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "gspin-tree.h"
#include "gspin-list.h"
#include "gspin-tel.h"
#include "gspin-mempool.h"

#define OMIT *omit = 1

// from gspin-tree.c
extern struct tree_code_property {
  gs_code_t tree_code;
  signed char arity;	// -1 means not yet known; assert when this value
  gs_tree_code_class_t tcc;
  char *name;
} tree_code_table[256];

gs_char_t operand_number[256];

gs_t gs_program = NULL;	// The root program node when translating from gspin.

gs_tree_code_class_t gs_tree_code_class(gs_t t) {
  GS_ASSERT (t != (gs_t) NULL, "Got null node");
  gs_tree_code_class_t tcc = (gs_tree_code_class_t) gs_b(gs_operand (t, GS_TREE_CODE_CLASS));
  GS_ASSERT(tcc == tree_code_table[gs_code(t)].tcc, 
  	    "Incorrect tree code class in tree_code_table");
#ifdef Is_True_On
  if (tcc != tree_code_table[gs_code(t)].tcc) {
    printf("%s should have tcc %s\n", gs_code_name(gs_code(t)),
	   gs_tree_code_class_name(tcc));
    abort();
  }
#endif
  return tcc;
}

// Return the name of the attribute.  Return NULL if no such attribute.
gs_string_t
gs_attribute_name (gs_int_t attribute, gs_tree_code_class_t tree_code_class,
		   gs_code_t constructor, gs_int_t * omit) {

  if (constructor == GS_PROGRAM) {
    switch (attribute) {
      case GS_CC1_COMMAND_LINE_ARGS:	return "GS_CC1_COMMAND_LINE_ARGS";
      case GS_PROGRAM_DECLARATIONS:	return "GS_PROGRAM_DECLARATIONS";
      case GS_GLOBAL_TREES_LIST:	return "GS_GLOBAL_TREES_LIST";
      case GS_INTEGER_TYPES_LIST:	return "GS_INTEGER_TYPES_LIST";
      case GS_GXX_EMITTED_DECLS:	return "GS_GXX_EMITTED_DECLS";
      case GS_GXX_EMITTED_ASMS:		return "GS_GXX_EMITTED_ASMS";
      case GS_PROGRAM_FLAGS:		return "GS_PROGRAM_FLAGS";
      case GS_WEAK_DECLS:		return "GS_WEAK_DECLS";
      default:
	GS_ASSERT(gs_false,("gs_attribute_name: unkown GS_PROGRAM attribute"));
	return (gs_string_t) "UNKNOWN!";
    }
  }

  switch (attribute) {
    case GS_TREE_CODE_CLASS:	OMIT; return "GS_TREE_CODE_CLASS";
    case GS_TREE_TYPE:		OMIT; return "GS_TREE_TYPE";
    case GS_FLAGS:		return "GS_FLAGS";
  }

  if (tree_code_class == GS_TCC_DECLARATION) {
    switch (attribute) {
      case GS_LABEL_DECL_UID:		OMIT; return "GS_LABEL_DECL_UID";
      case GS_DECL_UID:			OMIT; return "GS_DECL_UID";       
      case GS_DECL_NAME:		return "GS_DECL_NAME";     
      case GS_TREE_CHAIN:		return "GS_TREE_CHAIN";
      case GS_DECL_MODE:		OMIT; return "GS_DECL_MODE";
      case GS_DECL_SOURCE_FILE:		OMIT; return "GS_DECL_SOURCE_FILE";
      case GS_DECL_SOURCE_LINE:		OMIT; return "GS_DECL_SOURCE_LINE";
      case GS_DECL_SIZE:		return "GS_DECL_SIZE";
      case GS_DECL_SIZE_UNIT:		OMIT; return "GS_DECL_SIZE_UNIT";
      case GS_DECL_BUILT_IN_CLASS:	OMIT; return "GS_DECL_BUILT_IN_CLASS";
      case GS_DECL_FUNCTION_CODE:	OMIT; return "GS_DECL_FUNCTION_CODE";
      case GS_DECL_FIELD_OFFSET:	return "GS_DECL_FIELD_OFFSET";
      case GS_DECL_FIELD_BIT_OFFSET:	OMIT; return "GS_DECL_FIELD_BIT_OFFSET";
      case GS_DECL_CONTEXT:		return "GS_DECL_CONTEXT";
      case GS_DECL_ATTRIBUTES:		return "GS_DECL_ATTRIBUTES";
      case GS_DECL_ABSTRACT_ORIGIN:	return "GS_DECL_ABSTRACT_ORIGIN";
      case GS_DECL_ARGUMENTS:
	return (constructor == GS_VAR_DECL) ?
		 "GS_DECL_ANON_UNION_ELEMS" : "GS_DECL_ARGUMENTS";
      case GS_DECL_INITIAL:		OMIT; return "GS_DECL_INITIAL";
      case GS_DECL_ARG_TYPE:		return "GS_DECL_ARG_TYPE";
      case GS_DECL_ARG_TYPE_AS_WRITTEN:	OMIT; return "GS_DECL_ARG_TYPE_AS_WRITTEN";
      case GS_DECL_SAVED_TREE:
        return (constructor == GS_FUNCTION_DECL) ?
                "GS_DECL_SAVED_TREE" : "GS_DECL_VALUE_EXPR";
      case GS_DECL_RESULT:
	return (constructor == GS_TYPE_DECL) ?
		 "GS_DECL_ORIGINAL_TYPE" : "GS_DECL_RESULT";
      case GS_CP_DECL_FLAGS:		return "GS_CP_DECL_FLAGS";
      case GS_DECL_ALIGN_UNIT:		OMIT; return "GS_DECL_ALIGN_UNIT";
      case GS_DECL_ASSEMBLER_NAME:	return "GS_DECL_ASSEMBLER_NAME";
      case GS_DECL_TEMPLATE_INFO:	OMIT; return "GS_DECL_TEMPLATE_INFO";
      case GS_DECL_SECTION_NAME:	OMIT; return "GS_DECL_SECTION_NAME";
      case GS_CP_DECL_CONTEXT:		OMIT; return "GS_CP_DECL_CONTEXT"; 
      case GS_DECL_VINDEX:		OMIT; return "GS_DECL_VINDEX"; 
      case GS_MOST_GENERAL_TEMPLATE:	OMIT; return "GS_MOST_GENERAL_TEMPLATE"; 
      case GS_DECL_ALIAS_TARGET:	OMIT; return "GS_DECL_ALIAS_TARGET"; 
      case GS_DECL_ASMREG:		OMIT; return "GS_DECL_ASMREG";
      case GS_DECL_NAMESPACE_ALIAS:	OMIT; return "GS_DECL_NAMESPACE_ALIAS"; 
      case GS_THUNK_TARGET:		return "GS_THUNK_TARGET"; 
      case GS_DECL_TI_TEMPLATE:		OMIT; return "GS_DECL_TI_TEMPLATE";
      case GS_THUNK_FIXED_OFFSET:	OMIT; return "GS_THUNK_FIXED_OFFSET";
      case GS_THUNK_VIRTUAL_OFFSET:	OMIT; return "GS_THUNK_VIRTUAL_OFFSET";
      case GS_DECL_NAMED_RETURN_OBJECT:	OMIT; return "GS_DECL_NAME_RETURN_OBJECT";
    }

    if (attribute == GS_DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P &&
        constructor == GS_FUNCTION_DECL)
      return "GS_DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P"; 

    if (attribute == GS_DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P &&
        constructor == GS_FUNCTION_DECL)
      return "GS_DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P"; 

    if (attribute == GS_CP_NAMESPACE_DECLS &&
        constructor == GS_NAMESPACE_DECL)
      return "GS_CP_NAMESPACE_DECLS"; 

  } else if (tree_code_class == GS_TCC_TYPE) {

      if (constructor == GS_ENUMERAL_TYPE && 
          attribute == GS_TYPE_VALUES)
        return "GS_TYPE_VALUES";

      if (constructor == GS_ARRAY_TYPE && 
          attribute == GS_TYPE_DOMAIN) 
      	return "GS_TYPE_DOMAIN";

      if (constructor == GS_VECTOR_TYPE) {
	if (attribute == GS_TYPE_VECTOR_SUBPARTS)
	  return "GS_TYPE_VECTOR_SUBPARTS";
	else if (attribute == GS_TYPE_DEBUG_REPRESENTATION_TYPE)
	  return "GS_TYPE_DEBUG_REPRESENTATION_TYPE";
      }

      if ((constructor == GS_RECORD_TYPE || 
           constructor == GS_UNION_TYPE  || 
           constructor == GS_QUAL_UNION_TYPE) && 
           attribute == GS_TYPE_FIELDS)
        return "GS_TYPE_FIELDS";

      if ((constructor == GS_FUNCTION_TYPE || 
           constructor == GS_METHOD_TYPE) && 
           attribute == GS_TYPE_METHOD_BASETYPE)
        return "GS_TYPE_METHOD_BASETYPE";

      if (constructor == GS_OFFSET_TYPE && 
          attribute == GS_TYPE_OFFSET_BASETYPE)
        return "GS_TYPE_OFFSET_BASETYPE";

    switch (attribute) {
      case GS_TYPE_NAME:		return "GS_TYPE_NAME";
      case GS_TREE_CHAIN:		return "GS_TREE_CHAIN";
      case GS_TYPE_MODE:		OMIT; return "GS_TYPE_MODE";
      case GS_TYPE_SIZE:		return "GS_TYPE_SIZE";
      case GS_TYPE_SIZE_UNIT:		OMIT; return "GS_TYPE_SIZE_UNIT";
      case GS_TYPE_USER_ALIGN:		OMIT; return "GS_TYPE_USER_ALIGN";
      case GS_TYPE_ALIGN:		return "GS_TYPE_ALIGN";
      case GS_TYPE_ALIAS_SET:		OMIT; return "GS_TYPE_ALIAS_SET";
      case GS_TYPE_ATTRIBUTES:		return "GS_TYPE_ATTRIBUTES";
      case GS_TYPE_PRECISION:		OMIT; return "GS_TYPE_PRECISION";
      case GS_TYPE_MIN_VALUE:
	OMIT;
        return (constructor == GS_RECORD_TYPE || constructor == GS_UNION_TYPE) ?
		 "GS_TYPE_VFIELD" :
	       (constructor == GS_POINTER_TYPE) ? "GS_TYPE_NEXT_PTR_TO" :
	       "GS_TYPE_MIN_VALUE";
      case GS_TYPE_MAX_VALUE:
	OMIT;
	return (constructor == GS_RECORD_TYPE || constructor == GS_UNION_TYPE) ?
		 "GS_TYPE_METHODS" : "GS_TYPE_MAX_VALUE";
      case GS_TYPE_ARG_TYPES:		OMIT; return "GS_TYPE_ARG_TYPES";
      case GS_TYPE_CONTEXT:		return "GS_TYPE_CONTEXT";
      case GS_TYPE_POINTER_TO:		OMIT; return "GS_TYPE_POINTER_TO";
      case GS_TYPE_REFERENCE_TO:	return "GS_TYPE_REFERENCE_TO";
      case GS_CP_TYPE_FLAGS:		return "GS_CP_TYPE_FLAGS";
      case GS_TYPE_BINFO:		return "GS_TYPE_BINFO";
      case GS_TYPE_MAIN_VARIANT:	OMIT; return "GS_TYPE_MAIN_VARIANT";
      case GS_CLASSTYPE_AS_BASE:	return "GS_CLASSTYPE_AS_BASE";
      case GS_CLASSTYPE_TYPEINFO_VAR:	OMIT; return "GS_CLASSTYPE_TYPEINFO_VAR";
      case GS_TYPEINFO_DECL:		OMIT; return "GS_TYPEINFO_DECL";
      case GS_CLASSTYPE_COPY_CONSTRUCTOR: OMIT; return "GS_CLASSTYPE_COPY_CONSTRUCTOR";
    }
  } else if (tree_code_class == GS_TCC_EXPRESSION ||
           tree_code_class == GS_TCC_COMPARISON ||
           tree_code_class == GS_TCC_UNARY ||
           tree_code_class == GS_TCC_BINARY ||
           tree_code_class == GS_TCC_REFERENCE ||
           tree_code_class == GS_TCC_STATEMENT) {
    switch (attribute) {
      case GS_ARITY:			OMIT; return "GS_ARITY";
      case GS_TREE_CHAIN:		return "GS_TREE_CHAIN"; 
      case GS_EXPR_FILENAME:		OMIT; return "GS_EXPR_FILENAME";
      case GS_EXPR_LINENO:		OMIT; return "GS_EXPR_LINENO";
      case GS_CP_EXPR_FLAGS:		return "GS_CP_EXPR_FLAGS";
    }

    switch (constructor) {
      case GS_ASM_EXPR:
	switch (attribute) {
	  case GS_ASM_STRING:		return "GS_ASM_STRING";
	  case GS_ASM_OUTPUTS:		return "GS_ASM_OUTPUTS";
	  case GS_ASM_INPUTS:		return "GS_ASM_INPUTS";
	  case GS_ASM_CLOBBERS:		return "GS_ASM_CLOBBERS";
	}
	return (gs_string_t) NULL;

      case GS_BIND_EXPR:
	switch (attribute) {
	  case GS_BIND_EXPR_VARS:	OMIT; return "GS_BIND_EXPR_VARS";
	  case GS_BIND_EXPR_BODY:	return "GS_BIND_EXPR_BODY";
	  case GS_BIND_EXPR_BLOCK:	OMIT; return "GS_BIND_EXPR_BLOCK";
	}
	return (gs_string_t) NULL;

      case GS_CASE_LABEL_EXPR:
	switch (attribute) {
	  case GS_CASE_LOW:		return "GS_CASE_LOW";
	  case GS_CASE_HIGH:		return "GS_CASE_HIGH";
	  case GS_CASE_LABEL:		return "GS_CASE_LABEL";
	}
	return (gs_string_t) NULL;

      case GS_CLEANUP_STMT:
	switch (attribute) {
	  case GS_CLEANUP_EXPR:		OMIT; return "GS_CLEANUP_EXPR";
	  case GS_CLEANUP_BODY:		OMIT; return "GS_CLEANUP_BODY";
	}
	return (gs_string_t) NULL;

#ifndef FE_GNU_4_2_0
      case GS_CONSTRUCTOR:
	if (attribute == GS_CONSTRUCTOR_ELTS)
	  return "GS_CONSTRUCTOR_ELTS";
#endif
	return (gs_string_t) "NONAME";

      case GS_DECL_EXPR:
	if (attribute == GS_DECL_EXPR_DECL)
	  return "GS_DECL_EXPR_DECL";
	return (gs_string_t) "NONAME";

      case GS_DO_STMT:
	switch (attribute) {
	  case GS_DO_COND:		return "GS_DO_COND";
	  case GS_DO_BODY:		return "GS_DO_BODY";
	}
	return (gs_string_t) NULL;

      case GS_EH_SPEC_BLOCK:
	switch (attribute) {
	  case GS_EH_SPEC_STMTS:	return "GS_EH_SPEC_STMTS";
	  case GS_EH_SPEC_RAISES:	return "GS_EH_SPEC_RAISES";
	}
	return (gs_string_t) NULL;

      case GS_EXPR_STMT:
	if (attribute == GS_EXPR_STMT_EXPR)
	  return "GS_EXPR_STMT_EXPR";
	return (gs_string_t) NULL;

      case GS_FOR_STMT:
	switch (attribute) {
	  case GS_FOR_INIT_STMT:	return "GS_FOR_INIT_STMT";
	  case GS_FOR_COND:		return "GS_FOR_COND";
	  case GS_FOR_EXPR:		return "GS_FOR_EXPR";
	  case GS_FOR_BODY:		return "GS_FOR_BODY";
	}
	return (gs_string_t) NULL;

      case GS_HANDLER:
	switch (attribute) {
	  case GS_HANDLER_PARMS:	return "GS_HANDLER_PARMS";
	  case GS_HANDLER_BODY:		return "GS_HANDLER_BODY";
	}
	return (gs_string_t) NULL;

      case GS_IF_STMT:
	switch (attribute) {
	  case GS_IF_COND:		return "GS_IF_COND";
	  case GS_THEN_CLAUSE:		return "GS_THEN_CLAUSE";
	  case GS_ELSE_CLAUSE:		return "GS_ELSE_CLAUSE";
	}
	return (gs_string_t) NULL;

      case GS_LABEL_EXPR_LABEL:
	if (attribute == GS_LABEL_EXPR_LABEL)
	  return "GS_LABEL_EXPR_LABEL";
	return (gs_string_t) NULL;

      case GS_LOOP_EXPR:
	if (attribute == GS_LOOP_EXPR_BODY)
	  return "GS_LOOP_EXPR_BODY";
	return (gs_string_t) NULL;

      case GS_STMT_EXPR:
	if (attribute == GS_STMT_EXPR_STMT)
	  return "GS_STMT_EXPR_STMT";
	return (gs_string_t) NULL;

      case GS_TRY_BLOCK:
	switch (attribute) {
	  case GS_TRY_STMTS:		return "GS_TRY_STMTS";
	  case GS_TRY_HANDLERS:		return "GS_TRY_HANDLERS";
	}
	return (gs_string_t) NULL;

      case GS_WHILE_STMT:
	switch (attribute) {
	  case GS_WHILE_COND:		return "GS_WHILE_COND";
	  case GS_WHILE_BODY:		return "GS_WHILE_BODY";
	}
	return (gs_string_t) NULL;

      default:
	if (attribute >= GS_TREE_OPERAND_ZERO) {
	  sprintf(operand_number, "Operand %d",
		  attribute - GS_TREE_OPERAND_ZERO);
	  return operand_number;
	}
	return (gs_string_t) NULL;
    }

  } else if (tree_code_class == GS_TCC_CONSTANT || 
	     tree_code_class == GS_TCC_EXCEPTIONAL) {

    if (attribute == GS_TREE_CHAIN) {
      return "GS_TREE_CHAIN";
    }

    switch (constructor) {
      case GS_BASELINK:
	switch (attribute) {
	  case GS_BASELINK_BINFO:	        return "GS_BASELINK_BINFO";
	  case GS_BASELINK_FUNCTIONS:		return "GS_BASELINK_FUNCTIONS";
	  case GS_BASELINK_ACCESS_BINFO:      return "GS_BASELINK_ACCESS_BINFO";
	  case GS_BASELINK_OPTYPE:		return "GS_BASELINK_OPTYPE";
	}
	return (gs_string_t) NULL;

      case GS_BLOCK:
	switch (attribute) {
	  case GS_BLOCK_VARS:		OMIT; return "GS_BLOCK_VARS";
	  case GS_BLOCK_SUPERCONTEXT:	OMIT; return "GS_BLOCK_SUPERCONTEXT";
	  case GS_BLOCK_SUBBLOCKS:	OMIT; return "GS_BLOCK_SUBBLOCKS";
	  case GS_BLOCK_CHAIN:		OMIT; return "GS_BLOCK_CHAIN";
	  case GS_BLOCK_ABSTRACT_ORIGIN: OMIT; return "GS_BLOCK_ABSTRACT_ORIGIN";
	}
	return (gs_string_t) NULL;

      case GS_COMPLEX_CST:
	switch (attribute) {
	  case GS_TREE_REALPART:	return "GS_TREE_REALPART";
	  case GS_TREE_IMAGPART:	return "GS_TREE_IMAGPART";
	}
	return (gs_string_t) NULL;

      case GS_IDENTIFIER_NODE:
	if (attribute == GS_IDENTIFIER_POINTER)
	  return "GS_IDENTIFIER_POINTER";
	return (gs_string_t) NULL;

      case GS_INTEGER_CST:
	switch (attribute) {
	  case GS_TREE_INT_CST_LOW:	return "GS_TREE_INT_CST_LOW";
	  case GS_TREE_INT_CST_HIGH:	return "GS_TREE_INT_CST_HIGH";
	}
	return (gs_string_t) NULL;

      case GS_PTRMEM_CST:
	switch (attribute) {
	  case GS_EXPANDED_PTRMEM_CST:	return "GS_EXPANDED_PTRMEM_CST";
	}
	return (gs_string_t) NULL;

      case GS_REAL_CST:
	switch (attribute) {
	  case GS_TREE_REAL_CST_F:	return "GS_TREE_REAL_CST_F";
	  case GS_TREE_REAL_CST_D:	return "GS_TREE_REAL_CST_D";
	  case GS_TREE_REAL_CST_LD:	return "GS_TREE_REAL_CST_LD";
	}
	return (gs_string_t) NULL;

      case GS_STATEMENT_LIST:
	if (attribute == GS_STATEMENT_LIST_ELTS)
	  return "GS_STATEMENT_LIST_ELTS";
	return (gs_string_t) NULL;

      case GS_STRING_CST:
	switch (attribute) {
	  case GS_TREE_STRING_POINTER:	return "GS_TREE_STRING_POINTER";
	  case GS_TREE_STRING_LENGTH:	return "GS_TREE_STRING_LENGTH";
	}
	return (gs_string_t) NULL;

      case GS_TEMPLATE_PARM_INDEX:
	switch (attribute) {
	  case GS_TEMPLATE_PARM_IDX:	        OMIT; return "GS_TEMPLATE_PARM_IDX";
	  case GS_TEMPLATE_PARM_LEVEL:	        OMIT; return "GS_TEMPLATE_PARM_LEVEL";
	  case GS_TEMPLATE_PARM_DESCENDANTS:	OMIT; return "GS_TEMPLATE_PARM_DESCENDANTS";
	  case GS_TEMPLATE_PARM_ORIG_LEVEL:	OMIT; return "GS_TEMPLATE_PARM_ORIG_LEVEL";
	  case GS_TEMPLATE_PARM_DECL:	        OMIT; return "GS_TEMPLATE_PARM_DECL";
	}
	return (gs_string_t) NULL;

      case GS_TREE_BINFO:
	switch (attribute) {
	  case GS_BINFO_TYPE:		return "GS_BINFO_TYPE";
	  case GS_BINFO_BASE_BINFOS:	OMIT; return "GS_BINFO_BASE_BINFOS";
	  case GS_BINFO_VPTR_FIELD:	return "GS_BINFO_VPTR_FIELD";
	}
	return (gs_string_t) NULL;

      case GS_TREE_LIST:
	switch (attribute) {
	  case GS_TREE_PURPOSE:		return "GS_TREE_PURPOSE";
	  case GS_TREE_VALUE:		return "GS_TREE_VALUE";
	}
	return (gs_string_t) NULL;

      case GS_TREE_VEC:
	switch (attribute) {
	  case GS_TREE_VEC_LENGTH:	return "GS_TREE_VEC_LENGTH";
	  case GS_TREE_VEC_ELT:		return "GS_TREE_VEC_ELT";
	}
	return (gs_string_t) NULL;

      case GS_VECTOR_CST:
	if (attribute == GS_TREE_VECTOR_CST_ELTS)
	  return "GS_TREE_VECTOR_CST_ELTS";
	return (gs_string_t) NULL;

      case GS_OVERLOAD:
	switch (attribute) {
	  case GS_OVL_FUNCTION:	OMIT; return "GS_OVL_FUNCTION";
	  case GS_OVL_CHAIN:	OMIT; return "GS_OVL_CHAIN";
	  case GS_OVL_CURRENT:	OMIT; return "GS_OVL_CURRENT";
	  case GS_OVL_NEXT:	OMIT; return "GS_OVL_NEXT";
	}
	return (gs_string_t) NULL;

#ifdef FE_GNU_4_2_0
      case GS_CONSTRUCTOR:
	switch (attribute) {
	  case GS_CONSTRUCTOR_LENGTH:      return "GS_CONSTRUCTOR_LENGTH";
	  case GS_CONSTRUCTOR_ELTS_INDEX:  return "GS_CONSTRUCTOR_ELTS_INDEX";
	  case GS_CONSTRUCTOR_ELTS_VALUE:  return "GS_CONSTRUCTOR_ELTS_VALUE";
	}
	return (gs_string_t) NULL;

      case GS_OMP_CLAUSE:
        if (attribute == GS_OMP_CLAUSE_DECL)
	  return "GS_OMP_CLAUSE_DECL";
	return (gs_string_t) NULL;
#endif
      default:
        break;
    }
  }
  return (gs_string_t) "NONAME";
}

gs_string_t
gs_flag_name (gs_code_t constructor, gs_int_t attribute, gs_count_t flag,
	      gs_tree_code_class_t tree_code_class) {

  if (attribute == GS_FLAGS) {

    if (flag == GS_TYPE_READONLY || 
        flag == GS_TREE_READONLY) {
      return (tree_code_class == GS_TCC_TYPE) ?
	       "GS_TYPE_READONLY" : "GS_TREE_READONLY";
    }

    if (flag == GS_TREE_CONSTANT ||
        flag == GS_TYPE_SIZES_GIMPLIFIED) {
      return (tree_code_class == GS_TCC_TYPE) ?
	       "GS_TYPE_SIZES_GIMPLIFIED" : "GS_TREE_CONSTANT";
    } 

    switch (flag) {

      case GS_TREE_SIDE_EFFECTS:	return "GS_TREE_SIDE_EFFECTS";     
      case GS_TREE_INVARIANT:		return "GS_TREE_INVARIANT";
      case GS_TREE_ADDRESSABLE:		return "GS_TREE_ADDRESSABLE";
      case GS_TREE_THIS_VOLATILE:	return "GS_TREE_THIS_VOLATILE";
      case GS_TREE_ASM_WRITTEN:		return "GS_TREE_ASM_WRITTEN";
      case GS_TREE_USED:		return constructor == GS_OVERLOAD ? "GS_OVL_USED" : "GS_TREE_USED";
      case GS_TREE_NOTHROW:		return "GS_TREE_NOTHROW";
      case GS_TREE_PUBLIC:		return "GS_TREE_PUBLIC";
      case GS_TREE_PRIVATE:		return "GS_TREE_PRIVATE";
      case GS_TREE_PROTECTED:		return "GS_TREE_PROTECTED";
      case GS_TREE_STATIC:
	return (constructor == GS_IDENTIFIER_NODE)? "GS_TREE_SYMBOL_REFERENCED":
	       (constructor == GS_POINTER_TYPE ||
	        constructor == GS_REFERENCE_TYPE) ? "GS_TYPE_REF_CAN_ALIAS_ALL":
	       "GS_TREE_STATIC";
      case GS_TREE_LANG_FLAG_0:		return "GS_TREE_LANG_FLAG_0";
      case GS_TREE_LANG_FLAG_1:		return "GS_TREE_LANG_FLAG_1";
      case GS_TREE_LANG_FLAG_2:		return "GS_TREE_LANG_FLAG_2";
      case GS_TREE_LANG_FLAG_3:		return "GS_TREE_LANG_FLAG_3";
      case GS_TREE_LANG_FLAG_4:		return "GS_TREE_LANG_FLAG_4";
      case GS_TREE_LANG_FLAG_5:		return "GS_TREE_LANG_FLAG_5";
      case GS_TREE_LANG_FLAG_6:		return "GS_TREE_LANG_FLAG_6";      
      case GS_TREE_NOT_EMITTED_BY_GXX:	return "GS_TREE_NOT_EMITTED_BY_GXX";
      case GS_DWARF_ACCESS_FLAG_0:	return "GS_DWARF_ACCESS_FLAG_0";
      case GS_DWARF_ACCESS_FLAG_1:	return "GS_DWARF_ACCESS_FLAG_1";
      default:
        // FLAG is not in the common flags section.
        break;
    }

    if (tree_code_class == GS_TCC_DECLARATION) {
      switch (flag) {
	case GS_DECL_UNSIGNED:		return "GS_DECL_UNSIGNED";            
	case GS_DECL_IGNORED_P:		return "GS_DECL_IGNORED_P";
	case GS_DECL_ABSTRACT:		return "GS_DECL_ABSTRACT";
	case GS_DECL_IN_SYSTEM_HEADER:	return "GS_DECL_IN_SYSTEM_HEADER";
	case GS_DECL_COMMON:		return "GS_DECL_COMMON";
	case GS_DECL_EXTERNAL:		return "GS_DECL_EXTERNAL";
	case GS_DECL_WEAK:		return "GS_DECL_WEAK";
	case GS_DECL_REGISTER:		return "GS_DECL_REGISTER";
	case GS_DECL_NONLOCAL:		return "GS_DECL_NONLOCAL";
	case GS_TYPE_DECL_SUPPRESS_DEBUG: return constructor == GS_FUNCTION_DECL ? "GS_DECL_NEEDED" : "GS_TYPE_DECL_SUPPRESS_DEBUG";
	case GS_DECL_INLINE:		return "GS_DECL_INLINE";
	case GS_DECL_DECLARED_INLINE_P:	return "GS_DECL_DECLARED_INLINE_P";
	case GS_DECL_BUILT_IN:		return "GS_DECL_BUILT_IN";
	case GS_DECL_NO_STATIC_CHAIN:	return "GS_DECL_NO_STATIC_CHAIN";
	case GS_DECL_PACKED:		return constructor == GS_FUNCTION_DECL ? "GS_DECL_REACHABLE" : "GS_DECL_PACKED";
	case GS_DECL_BIT_FIELD:		return "GS_DECL_BIT_FIELD";
	case GS_DECL_NONADDRESSABLE_P:	return "GS_DECL_NONADDRESSABLE_P";
	case GS_DECL_EMITTED_BY_GXX:	return "GS_DECL_EMITTED_BY_GXX";
	case GS_DECL_IN_TEXT_SECTION:	return "GS_DECL_IN_TEXT_SECTION";
	case GS_DECL_THREAD_LOCAL:	return "GS_DECL_THREAD_LOCAL";
	case GS_DECL_TRANSPARENT_UNION:	return "GS_DECL_TRANSPARENT_UNION";
	case GS_DECL_VIRTUAL_P:		return "GS_DECL_VIRTUAL_P";
	case GS_DECL_DEFER_OUTPUT:	return "GS_DECL_DEFER_OUTPUT";
	case GS_DECL_PRESERVE_P:	return "GS_DECL_PRESERVE_P";
	case GS_DECL_LANG_FLAG_0:	return "GS_DECL_LANG_FLAG_0";
	case GS_DECL_LANG_FLAG_1:	return "GS_DECL_LANG_FLAG_1";
	case GS_DECL_LANG_FLAG_2:	return "GS_DECL_LANG_FLAG_2";
	case GS_DECL_LANG_FLAG_3:	return "GS_DECL_LANG_FLAG_3";
	case GS_DECL_LANG_FLAG_4:	return "GS_DECL_LANG_FLAG_4";
	case GS_DECL_LANG_FLAG_5:	return "GS_DECL_LANG_FLAG_5";
	case GS_DECL_LANG_FLAG_6:	return "GS_DECL_LANG_FLAG_6";
	case GS_DECL_LANG_FLAG_7:	return "GS_DECL_LANG_FLAG_7";
	case GS_DECL_USER_ALIGN:	return "GS_DECL_USER_ALIGN";
	case GS_DECL_OFFSET_ALIGN:	return "GS_DECL_OFFSET_ALIGN";
	case GS_DECL_POINTER_ALIAS_SET:	return "GS_DECL_POINTER_ALIAS_SET";
	case GS_DECL_THUNK_P:		return "GS_DECL_THUNK_P";
	case GS_DECL_ASSEMBLER_NAME_SET_P: return "GS_DECL_ASSEMBLER_NAME_SET_P";
	case GS_DECL_ARTIFICIAL:	return "GS_DECL_ARTIFICIAL";
	case GS_DECL_LANG_SPECIFIC:	return "GS_DECL_LANG_SPECIFIC";
	case GS_DECL_THREADPRIVATE:	return "GS_DECL_THREADPRIVATE";
	default:  GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
      }
    }

    if (tree_code_class == GS_TCC_TYPE) {
      switch (flag) {
	case GS_TYPE_UNSIGNED:		return "GS_TYPE_UNSIGNED";
	case GS_TYPE_STRING_FLAG:	return "GS_TYPE_STRING_FLAG";
	case GS_TYPE_NEEDS_CONSTRUCTING: return "GS_TYPE_NEEDS_CONSTRUCTING";
	case GS_TYPE_PACKED:		return "GS_TYPE_PACKED";
	case GS_TYPE_RESTRICT:		return "GS_TYPE_RESTRICT";
	case GS_TYPE_LANG_FLAG_0:	return "GS_TYPE_LANG_FLAG_0";
	case GS_TYPE_LANG_FLAG_1:	return "GS_TYPE_LANG_FLAG_1";
	case GS_TYPE_LANG_FLAG_2:	return "GS_TYPE_LANG_FLAG_2";
	case GS_TYPE_LANG_FLAG_3:	return "GS_TYPE_LANG_FLAG_3";
	case GS_TYPE_LANG_FLAG_4:	return "GS_TYPE_LANG_FLAG_4";
	case GS_TYPE_LANG_FLAG_5:	return "GS_TYPE_LANG_FLAG_5";
	case GS_TYPE_LANG_FLAG_6:	return "GS_TYPE_LANG_FLAG_6";
	case GS_TYPE_VOLATILE:		return "GS_TYPE_VOLATILE";
	case GS_TYPE_LANG_SPECIFIC:	return "GS_TYPE_LANG_SPECIFIC";
        case GS_POINTER_TYPE_P:	        return "GS_POINTER_TYPE_P";
        case GS_AGGREGATE_VALUE_P:      return "GS_AGGREGATE_VALUE_P";
        case GS_TYPE_BIG_ENDIAN:        return "GS_TYPE_BIG_ENDIAN";
        case GS_TYPE_LITTLE_ENDIAN:     return "GS_TYPE_LITTLE_ENDIAN";
        case GS_TYPE_EXPLICIT_ENDIAN:     return "GS_TYPE_EXPLICIT_ENDIAN";
      }

      // Differentiate the GS_...'s that share the same numerical ID.

      if (flag == GS_TYPE_NO_FORCE_BLK) {
        switch (constructor) {
	  case GS_RECORD_TYPE:
	  case GS_UNION_TYPE:
	  case GS_QUAL_UNION_TYPE:
	    return "GS_TYPE_NO_FORCE_BLK";
	  case GS_INTEGER_TYPE:
	    return "GS_TYPE_IS_SIZETYPE";
	  case GS_FUNCTION_TYPE:
	    return "GS_TYPE_RETURNS_STACK_DEPRESSED";
	  default:
	    GS_ASSERT(gs_false,
		("gs_flag_name: bad constructor for GS_TYPE_NO_FORCE_BLK"));
	    break;
	}
      }

      if (flag == GS_TYPE_TRANSPARENT_UNION) {
	if (constructor == GS_UNION_TYPE)
	  return "GS_TYPE_TRANSPARENT_UNION"; 
	else if (constructor == GS_ARRAY_TYPE)
	  return "GS_TYPE_NONALIASED_COMPONENT";
	else
	  GS_ASSERT(gs_false,
	    ("gs_flag_name: bad constructor for GS_TYPE_TRANSPARENT_UNION"));
      }

      GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
    }	// tree_code_class == GS_TCC_TYPE

    if (tree_code_class == GS_TCC_EXPRESSION ||
        tree_code_class == GS_TCC_COMPARISON ||
        tree_code_class == GS_TCC_UNARY ||
        tree_code_class == GS_TCC_BINARY ||
        tree_code_class == GS_TCC_REFERENCE ||
        tree_code_class == GS_TCC_STATEMENT) {
      switch (flag) {
	case GS_BIT_FIELD_REF_UNSIGNED:	return "GS_BIT_FIELD_REF_UNSIGNED";
	case GS_EXPR_HAS_LOCATION:	return "GS_EXPR_HAS_LOCATION";
	case GS_EMIT_TARGET_EXPR_CLEANUP:
	  GS_ASSERT(constructor == GS_TARGET_EXPR,
		 ("gs_flag_name: unexpected flag GS_EMIT_TARGET_EXPR_CLEANUP"));
	  return "GS_EMIT_TARGET_EXPR_CLEANUP";
	default:  GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
      }
    }

    if (tree_code_class == GS_TCC_CONSTANT || 
        tree_code_class == GS_TCC_EXCEPTIONAL) {

	if (constructor == GS_INTEGER_CST && 
            flag == GS_TREE_CONSTANT_OVERFLOW) 
          return "GS_TREE_CONSTANT_OVERFLOW";

	if (constructor == GS_REAL_CST && 
            flag == GS_TREE_OVERFLOW)		
          return "GS_TREE_OVERFLOW";

      switch (flag) {
	case GS_TREE_CONSTANT_OVERFLOW:
	  if (constructor == GS_INTEGER_CST) return "GS_TREE_CONSTANT_OVERFLOW";
	  if (constructor == GS_TREE_BINFO) return "GS_BINFO_VIRTUAL_P";
	  GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
	  return "NONAME";
	case GS_REAL_VALUE_ISINF:	return "GS_REAL_VALUE_ISINF";
	case GS_REAL_VALUE_ISNAN:	return "GS_REAL_VALUE_ISNAN";
	default:  GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
      }
    }

  } else if (attribute == GS_CP_DECL_FLAGS) {
    switch (flag) {
      case GS_DECL_COMDAT:			return "GS_DECL_COMDAT";
      case GS_DECL_GLOBAL_CTOR_P:		return "GS_DECL_GLOBAL_CTOR_P";
      case GS_DECL_GLOBAL_DTOR_P:		return "GS_DECL_GLOBAL_DTOR_P";
      case GS_DECL_ONE_ONLY:			return "GS_DECL_ONE_ONLY";
      case GS_DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P:
					return "GS_DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P";
      case GS_DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P:
					return "GS_DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P";
      case GS_DECL_FUNCTION_MEMBER_P:		return "GS_DECL_FUNCTION_MEMBER_P";
      case GS_DECL_USES_TEMPLATE_PARMS:		return "GS_DECL_USES_TEMPLATE_PARMS";
      case GS_DECL_COPY_CONSTRUCTOR_P:		return "GS_DECL_COPY_CONSTRUCTOR_P";
      case GS_DECL_IMPLICIT_INSTANTIATION:	return "GS_DECL_IMPLICIT_INSTANTIATION";
      case GS_DECL_NAMESPACE_SCOPE_P:		return "GS_DECL_NAMESPACE_SCOPE_P";
#ifdef FE_GNU_4_2_0
      case GS_CP_DECL_THREADPRIVATE_P:
        return constructor == GS_FUNCTION_DECL ? "GS_DECL_CONSTRUCTOR_P" :
                                                 "GS_CP_DECL_THREADPRIVATE_P";
      case GS_DECL_COMPLETE_DESTRUCTOR_P:       return "GS_DECL_COMPLETE_DESTRUCTOR_P";
      case GS_DECL_HAS_IN_CHARGE_PARM_P:        return "GS_DECL_HAS_IN_CHARGE_PARM_P";
      case GS_DECL_HAS_VTT_PARM_P:              return "GS_DECL_HAS_VTT_PARM_P";
      case GS_DECL_ASSIGNMENT_OPERATOR_P:       return "GS_DECL_ASSIGNMENT_OPERATOR_P";
#endif
      case GS_DECL_COMPLETE_CONSTRUCTOR_P:	return "GS_DECL_COMPLETE_CONSTRUCTOR_P";
      case GS_DECL_REALLY_EXTERN:		return "GS_DECL_REALLY_EXTERN";
      case GS_DECL_USE_TEMPLATE:		return "GS_DECL_USE_TEMPLATE";
      case GS_DECL_TEMPLATE_INSTANTIATED:	return "GS_DECL_TEMPLATE_INSTANTIATED";
      case GS_DECL_TEMPLATE_SPECIALIZATION:	return "GS_DECL_TEMPLATE_SPECIALIZATION";
      case GS_DECL_PURE_VIRTUAL_P:		return "GS_DECL_PURE_VIRTUAL_P";
      case GS_DECL_THIS_THUNK_P:		return "GS_DECL_THIS_THUNK_P";
      case GS_DECL_EXTERN_C_P:			return "GS_DECL_EXTERN_C_P";
      default:	GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
    }

  } else if (attribute == GS_CP_TYPE_FLAGS) {
    switch (flag) {
      case GS_TYPE_PTRMEMFUNC_P:	return "GS_TYPE_PTRMEMFUNC_P";
      case GS_TYPE_PTRMEM_P:		return "GS_TYPE_PTRMEM_P";
      case GS_CLASSTYPE_INTERFACE_ONLY:	return "GS_CLASSTYPE_INTERFACE_ONLY";
      case GS_TYPE_USES_TEMPLATE_PARMS:	return "GS_TYPE_USES_TEMPLATE_PARMS";
      case GS_IS_EMPTY_CLASS:	        return "GS_IS_EMPTY_CLASS";
      case GS_CLASS_TYPE_P:	        return "GS_CLASS_TYPE_P";
      case GS_ANON_UNION_TYPE_P:        return "GS_ANON_UNION_TYPE_P";
      case GS_CLASSTYPE_TEMPLATE_SPECIALIZATION:
				return "GS_CLASSTYPE_TEMPLATE_SPECIALIZATION";
#ifdef FE_GNU_4_2_0
      case GS_CLASSTYPE_NON_POD_P:      return "GS_CLASSTYPE_NON_POD_P";
      case GS_TYPE_HAS_DEFAULT_CONSTRUCTOR:
                                return "GS_TYPE_HAS_DEFAULT_CONSTRUCTOR";
      case GS_TYPE_HAS_IMPLICIT_COPY_CONSTRUCTOR:
                                return "GS_TYPE_HAS_IMPLICIT_COPY_CONSTRUCTOR";
#endif
      default:	GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
    }

  } else if (attribute == GS_CP_EXPR_FLAGS) {
    switch (flag) {
      case GS_STMT_IS_FULL_EXPR_P:	return "GS_STMT_IS_FULL_EXPR_P";
      case GS_AGGR_INIT_VIA_CTOR_P:	return "GS_AGGR_INIT_VIA_CTOR_P";
      case GS_CLEANUP_EH_ONLY:	        return "GS_CLEANUP_EH_ONLY";
      default:	GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
    }
  } else if (attribute == GS_PROGRAM_FLAGS) {
    switch (flag) {
      case GS_FLAG_ERRNO_MATH:          return "GS_FLAG_ERRNO_MATH";
      case GS_PRAGMA_IMPLEMENTATION:    return "GS_PRAGMA_IMPLEMENTATION";
      case GS_PRAGMA_INTERFACE:         return "GS_PRAGMA_INTERFACE";
      default: GS_ASSERT(gs_false, ("gs_flag_name: missing flag name"));
    }
  }

  return (gs_string_t) "FLAG_NONAME";
}

static inline gs_t
gs_build_0(gs_tree_code_class_t code_class, gs_code_t code)
{
  gs_t root = __gs(code);
  gs_t code_class_node = __gs(GS_TCC);
  _gs_b(code_class_node, code_class);
  gs_set_operand(root, 0, code_class_node);
  gs_set_operand(root, GS_FLAGS, __gs(IB_BIT_VECTOR));
  return root;
}

gs_t
gs_build_type(gs_code_t code)
{
  gs_t root = gs_build_0(GS_TCC_TYPE, code);
  return root;
}

gs_t
gs_build_if_stmt(gs_t k0, gs_t k1, gs_t k2)
{
  gs_t root = __gs(GS_IF_STMT);

  // Argument 0 is the name of the class:
  gs_t code_class_node = __gs(GS_TCC);
  _gs_b(code_class_node, GS_TCC_STATEMENT);
  gs_set_operand(root, 0, code_class_node);

  gs_set_operand(root, GS_TREE_OPERAND_ZERO, k0);
  gs_set_operand(root, GS_TREE_OPERAND_ZERO+1, k1);
  gs_set_operand(root, GS_TREE_OPERAND_ZERO+2, k2);
  return root;
}

gs_t
gs_build_target_expr(gs_t k0, gs_t k1, gs_t k2, gs_t k3)
{
  gs_t res = __gs(GS_TARGET_EXPR);

  // Argument 0 is the name of the class:
  gs_t code_class_node = __gs(GS_TCC);
  _gs_b(code_class_node, GS_TCC_EXPRESSION);
  gs_set_operand(res, 0, code_class_node);

  gs_set_operand(res, GS_FLAGS, __gs(IB_BIT_VECTOR));

  gs_set_operand(res, GS_TREE_OPERAND_ZERO, k0);
  gs_set_operand(res, GS_TREE_OPERAND_ZERO+1, k1);
  gs_set_operand(res, GS_TREE_OPERAND_ZERO+2, k2);
  gs_set_operand(res, GS_TREE_OPERAND_ZERO+3, k3);
  return res;
}

gs_t
gs_build_int_cst(gs_long_long_t n)
{
  gs_t cst = __gs(GS_INTEGER_CST);

  // Argument 0 is the name of the class:
  gs_t code_class_node = __gs(GS_TCC);
  _gs_b(code_class_node, GS_TCC_CONSTANT);
  gs_set_operand(cst, 0, code_class_node);

  // Add type.
  gs_t gs_integer_type = gs_integer_type_node();
  gs_set_operand(cst, GS_TREE_TYPE, gs_integer_type);

  gs_t low = __gs(IB_UNSIGNED_LONG_LONG);
  gs_t high = __gs(IB_LONG_LONG);
  _gs_ull(low, n);
  _gs_ll(high, n < 0 ? -1 : 0);

  gs_set_operand(cst, GS_TREE_INT_CST_LOW, low);
  gs_set_operand(cst, GS_TREE_INT_CST_HIGH, high);
  gs_set_operand(cst, GS_FLAGS, __gs(IB_BIT_VECTOR));
  return cst;
}

gs_t
gs_build_decl(gs_code_t code, gs_t node2)
{
  gs_t res = __gs (code);

  // Argument 0 is the name of the class:
  gs_t code_class = __gs (GS_TCC);
  _gs_b (code_class, GS_TCC_DECLARATION);
  gs_set_operand(res, 0, code_class);

  // TREE_TYPE
  gs_set_operand(res, GS_TREE_TYPE, node2);

  // DECL_NAME
  gs_set_operand(res, GS_DECL_NAME, NULL);
  gs_set_operand(res, GS_FLAGS, __gs (IB_BIT_VECTOR));
  return res;
}

#ifndef FE_GNU_4_2_0
static inline
#endif
gs_t
gs_build_2(gs_tree_code_class_t code_class, gs_code_t code, gs_t k0, gs_t k1)
{
  gs_t root = gs_build_0(code_class, code);
  gs_set_operand(root, GS_TREE_OPERAND_ZERO, k0);
  gs_set_operand(root, GS_TREE_OPERAND_ZERO + 1, k1);
  return root;
}

static inline gs_t
gs_build_comparison(gs_code_t code, gs_t k0, gs_t k1)
{
  gs_t gs_integer_type = gs_integer_type_node();
  gs_t expr = gs_build_2(GS_TCC_COMPARISON, GS_NE_EXPR, k0, k1);
  gs_set_operand(expr, GS_TREE_TYPE, gs_integer_type);
  return expr;
}

gs_t
gs_c_common_truthvalue_conversion(gs_t node)
{
  gs_t zero = gs_build_int_cst(0);
  gs_t expr = gs_build_comparison(GS_NE_EXPR, node, zero);
  return expr;
}

gs_t
gs_strip_nops(gs_t node)
{
  gs_t opnd0;
  gs_t error_mark_node = gs_error_mark_node();
  gs_code_t code = gs_tree_code(node);

  while ((code == GS_NOP_EXPR ||
	  code == GS_CONVERT_EXPR ||
	  code == GS_NON_LVALUE_EXPR) &&
	 (opnd0 = gs_tree_operand(node, 0)) != error_mark_node &&
	 !strcmp(gs_type_mode(gs_tree_type(node)),
		 gs_type_mode(gs_tree_type(opnd0)))) {
    node = opnd0;
    code = gs_tree_code(node);
  }
  return node;
}

// Perform the function of GCC tree.c's build_pointer_type.
gs_t gs_build_pointer_type(gs_t to_type)
{
  gs_t t, type_pointer_to;
  gs_t error_mark_node = gs_error_mark_node();
  gs_t ptr_type = gs_ptr_type_node();
  gs_string_t ptr_type_mode = gs_type_mode(ptr_type);

  if (to_type == error_mark_node)
    return error_mark_node;

  // GCC returns NODE if the pointer_to type is not POINTER_TYPE.  This case
  // does not occur in C/C++,
  type_pointer_to = gs_type_pointer_to(to_type);
  GS_ASSERT(!(type_pointer_to != NULL &&
	      gs_tree_code(type_pointer_to) != GS_POINTER_TYPE),
	    ("gs_build_pointer_type: TYPE_POINTER_TO is not POINTER_TYPE"));

  for (t = type_pointer_to; t != NULL; t = gs_type_next_ptr_to(t)) {
    gs_string_t mode = gs_type_mode(t);
    if (!strcmp(mode, ptr_type_mode) &&
	gs_type_ref_can_alias_all(t) == 0) {
      return t;
    }
  }

  t = gs_build_type(GS_POINTER_TYPE);
  gs_set_operand(t, GS_TREE_TYPE, to_type);
  {
    gs_t type_mode = __gs(IB_STRING);
    _gs_s (type_mode, ptr_type_mode, 1 + strlen((char *)ptr_type_mode));
    gs_set_operand(t, GS_TYPE_MODE, type_mode);
  }
  gs_set_type_ref_can_alias_all(t, 0);
  gs_set_operand(t, GS_TYPE_NEXT_PTR_TO, gs_type_pointer_to(to_type));
  gs_set_operand(to_type, GS_TYPE_POINTER_TO, t);

  // Copy pointer attributes from the generic pointer.
  {
    gs_t type_precision = __gs(IB_INT);

    gs_set_operand(t, GS_TYPE_SIZE, gs_operand(ptr_type, GS_TYPE_SIZE));
    gs_set_operand(t, GS_TYPE_SIZE_UNIT, gs_operand(ptr_type, GS_TYPE_SIZE_UNIT));
    gs_set_operand(t, GS_TYPE_ALIGN, gs_operand(ptr_type, GS_TYPE_ALIGN));
    gs_set_type_unsigned(t, 1);
    _gs_n(type_precision, gs_type_type_precision(ptr_type));
    gs_set_operand(t, GS_TYPE_PRECISION, type_precision);
  }
  return t;
}
