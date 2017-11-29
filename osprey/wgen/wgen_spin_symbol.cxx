/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 Pathscale, LLC.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified October 9, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.3.1 release.
 */

/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

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


/* translate gnu decl trees to symtab references */

extern "C"{
#include "gspin-wgen-interface.h"
}
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "pathscale_defs.h"
#include "defs.h"
#include "errors.h"

#include "symtab.h"
#include "strtab.h"
#include "wn.h"
#include "wgen_expr.h"
#include "wgen_decl.h"
#include "wgen_misc.h"
#include "wgen_dst.h"
#include "ir_reader.h"
#include "wgen_spin_symbol.h"
#include "wgen_stmt.h"
#include <map>
#include "erfe.h"
#ifdef TARG_X8664
#include <ctype.h>
#endif
//#include "tree_cmp.h"
#include <erglob.h>
#include <ext/hash_set>
using __gnu_cxx::hash_set;
typedef struct {
    size_t operator()(void* p) const { return reinterpret_cast<size_t>(p); }
} void_ptr_hash;

extern int pstatic_as_global;
extern BOOL flag_no_common;
extern gs_t decl_arguments;
extern BOOL gv_cond_expr;

extern void Push_Deferred_Function(gs_t);
extern char *WGEN_Tree_Node_Name(gs_t op);
#if defined(TARG_SL)
extern char *Orig_Src_File_Name, *Src_File_Name;
#endif
static enum ST_TLS_MODEL tls_stress_model = TLS_NONE;
extern "C" void Process_TLS_Stress_Model(const char* p)
{
  if ( !strcmp(p, "global-dynamic") )
    tls_stress_model = TLS_GLOBAL_DYNAMIC;
  else if ( !strcmp(p, "local-dynamic") )
    tls_stress_model = TLS_LOCAL_DYNAMIC;
  else if ( !strcmp(p, "initial-exec") )
    tls_stress_model = TLS_INITIAL_EXEC;
  else if ( !strcmp(p, "local-exec") )
    tls_stress_model = TLS_LOCAL_EXEC;
}

#ifdef KEY
// =====================================================================
// bug 8346: A function's VLA argument types should only be expanded
// when necessary, to prevent size-expression-whirl from landing in an
// unintended location. If we attempt to expand such a type while
// generating a function's argument types, but are not expanding that
// specific function body, then we mark the TY as incomplete, to expand
// it later when we actually expand that function body.
// "expanding_function_definition" denotes when it is safe to process a
// function's VLA argument types.
// "processing_function_prototype" indicates when we are expanding
// function arguments (as opposed to other VLA variable occurrences).
// =====================================================================
BOOL processing_function_prototype = FALSE;
#endif

// Map duplicate gcc nodes that refer to the same function.
std::multimap<gs_t, gs_t> duplicate_of;
void
add_duplicates (gs_t newdecl, gs_t olddecl)
{
	duplicate_of.insert (pair<gs_t, gs_t>(newdecl, olddecl));
	duplicate_of.insert (pair<gs_t, gs_t>(olddecl, newdecl));
}

// Remove all references to DECL from the map.
void
erase_duplicates (gs_t decl)
{
  int i, j;
  int count = duplicate_of.count (decl);

  for (i=0; i<count; i++) {
    std::multimap<gs_t, gs_t>::iterator iter = duplicate_of.find(decl);
    gs_t t = (*iter).second;

    // Erase entries with DECL as the data, i.e., <..., DECL>.
    int count2 = duplicate_of.count(t); 
    for (j=0; j<count2; j++) {
      std::multimap<gs_t, gs_t>::iterator iter2 = duplicate_of.find(t);
      gs_t t2 = (*iter2).second;
      if (t2 == decl) {
	duplicate_of.erase (iter2);
      }
    }

    // Erase entry with DECL as the key, i.e., <DECL, ...>.
    duplicate_of.erase (iter);
  }
}

static ST*
get_duplicate_st (gs_t decl)
{
  int count = duplicate_of.count (decl);

  for (int i=0; i<count; ++i) {
    std::multimap<gs_t, gs_t>::iterator iter = duplicate_of.find(decl);
    gs_t t = (*iter).second;
    // The node t could have been garbage-collected by gcc.  This is a crude
    // test to see if t is still valid.
    if (gs_tree_code(t) == GS_FUNCTION_DECL &&
	gs_decl_name(t) == gs_decl_name(decl) &&
	gs_decl_assembler_name_set_p(t) == gs_decl_assembler_name_set_p(decl) &&
	(!gs_decl_assembler_name_set_p(t) ||
	 gs_decl_assembler_name(t) == gs_decl_assembler_name(decl))) {
      // Return the ST previously allocated, if any.
      ST *st = DECL_ST(t);
      if (st != NULL)
        return st;
    }
    duplicate_of.erase (iter);
  }
  return NULL;
}

static char*
Get_Name (gs_t node)
{
	static UINT anon_num = 0;
	static char buf[64];

	if (node == NULL) {
		++anon_num;
		sprintf(buf, ".anonymous.%d", anon_num);
		return buf;
	}
	else if (gs_tree_code (node) == GS_IDENTIFIER_NODE)
		return ((char *) gs_identifier_pointer (node));
	else if (gs_tree_code (node) == GS_TYPE_DECL)
		// If type has a typedef-name, the TYPE_NAME is a TYPE_DECL.
#ifdef FE_GNU_4_2_0 // bug 14137
		if (gs_decl_name(node) == NULL) {
		  ++anon_num;
		  sprintf(buf, ".anonymous.%d", anon_num);
		  return buf;
		}
		else
#endif
		return ((char *) gs_identifier_pointer (gs_decl_name (node)));
	else
		FmtAssert(FALSE, ("Get_Name unexpected tree"));
		return NULL;
}

static void
dump_field(gs_t field)
{
  printf("%s:  ", Get_Name(gs_decl_name(field)));
  printf("%d\n", DECL_FIELD_ID(field));
}

// =================================================================
// KEY: If there is a vtable pointer, then number it as the first
// field in the record. GNU 4.x provides the same field for a vptr
// to a base class, and its inherited classes. So we consistenly
// number the vptr as field_id 1.
// =================================================================
gs_t
get_first_real_or_virtual_field (gs_t type_tree)
{
  // return vfield only if the type contains fields (bug 10787)
  // bug 11227: C_TYPE_INCOMPLETE_VARS for C is the same as TYPE_VFIELD,
  //            make sure we do not use it for C.
  if (lang_cplus && gs_type_fields(type_tree) && gs_type_vfield(type_tree))
    return gs_type_vfield(type_tree);

  return gs_type_fields(type_tree);
}

gs_t
get_virtual_field (gs_t type_tree)
{
  gs_t vfield;

  // return vfield only if the type contains fields (bug 10787)
  if (lang_cplus &&
      gs_type_fields(type_tree) &&
      (vfield = gs_type_vfield(type_tree)) != NULL)
    return vfield;
  return NULL;
}

gs_t
get_first_real_field (gs_t type_tree)
{
  gs_t field = gs_type_fields(type_tree);

  if (!field)
    return NULL;

  // If there is a pointer to the virtual function table, it is always at the
  // first field.
  if (field == gs_type_vfield(type_tree))
  {
    Is_True (lang_cplus, ("get_first_real_field: TYPE_VFIELD used for C"));
    return gs_tree_chain(field);
  }
  return field;
}

gs_t
next_real_field (gs_t type_tree, gs_t field)
{
  BOOL first_real_field = FALSE;

  if (field == gs_type_vfield(type_tree))
  {
    first_real_field = TRUE; // return first real field
  }

  // If vptr is not in the list of fields, then return the first field
  if (first_real_field && field != gs_type_fields (type_tree))
    return gs_type_fields (type_tree);

  // Else return the next field.
  return gs_tree_chain (field);
}

static void
Do_Base_Types (gs_t type_tree)
{
  gs_t binfo = gs_type_binfo(type_tree);
  gs_t basetypes = binfo ? gs_binfo_base_binfos(binfo) : 0;
  gs_t list;
  if (basetypes) {
    for (list = basetypes; gs_code(list) != EMPTY; list = gs_operand(list, 1))
      (void) Get_TY (gs_binfo_type(gs_operand(list, 0)));
  }
}

size_t 
Roundup (size_t offset, int alignment)
{
  return (offset % alignment) ? offset + alignment - offset % alignment
			      : offset;
}

size_t
Type_Size_Without_Vbases (gs_t type_tree)
{
  gs_t field;
  gs_t last_field_decl = 0;

  for (field = get_first_real_or_virtual_field(type_tree);
       field;
       field = next_real_field (type_tree, field)) {
    if (gs_tree_code(field) == GS_FIELD_DECL)
      last_field_decl = field;
  }

  if (last_field_decl == 0)
    return 0;

  return
    gs_get_integer_value (gs_decl_field_offset(last_field_decl)) +
    gs_get_integer_value (gs_decl_field_bit_offset(last_field_decl)) / BITSPERBYTE +
    gs_get_integer_value (gs_decl_size(last_field_decl)) / BITSPERBYTE;
} 

bool
is_empty_base_class (gs_t type_tree)
{
  gs_t field = gs_type_fields(type_tree);
  return gs_tree_code(field) == GS_TYPE_DECL && gs_tree_chain(field) == 0;
}

// look up the attribute given by attr_name in the attribute list
gs_t 
lookup_attribute(char *attr_name, gs_t attr_list)
{
  gs_t nd;
  for (nd = attr_list; nd; nd = gs_tree_chain(nd)) {
    Is_True(gs_tree_code(nd) == GS_TREE_LIST,
	    ("lookup_attributes: TREE_LIST node not found")); 
    gs_t attr = gs_tree_purpose(nd);
    if (is_attribute(attr_name, attr))
      return nd;
  }
  return NULL;
}

// idx is non-zero only for RECORD and UNION, when there is forward declaration
extern TY_IDX
Create_TY_For_Tree (gs_t type_tree, TY_IDX idx)
{

	if(gs_tree_code(type_tree) == GS_ERROR_MARK)
	   return idx;

	TY_IDX orig_idx = idx;
	if(gs_tree_code_class(type_tree) != GS_TCC_TYPE) {
	  DevWarn("Bad tree class passed to Create_TY_For_Tree %c",
		gs_tree_code_class(type_tree));
          return idx;
	}


#ifdef KEY
	UINT align = gs_type_align(type_tree) / BITSPERBYTE;
#endif
	// for typedefs get the information from the base type
	if (gs_type_name(type_tree) &&
	    idx == 0 &&
	    (gs_tree_code(type_tree) == GS_RECORD_TYPE ||
	     gs_tree_code(type_tree) == GS_UNION_TYPE) &&
	    gs_tree_code(gs_type_name(type_tree)) == GS_TYPE_DECL &&
	    gs_type_main_variant(type_tree) != type_tree) {
		idx = Get_TY (gs_type_main_variant(type_tree));
		if (gs_type_readonly(type_tree))
			Set_TY_is_const (idx);
		if (gs_type_volatile(type_tree))
			Set_TY_is_volatile (idx);
#ifdef KEY
		if (gs_type_restrict(type_tree))
			Set_TY_is_restrict (idx);
		Set_TY_align (idx, align); // bug 10533
#endif
		TYPE_TY_IDX(type_tree) = idx;
		if(Debug_Level >= 2) {
#ifdef KEY // bug 11782
		  defer_DST_type(type_tree, idx, orig_idx);
#else
		  DST_INFO_IDX dst = Create_DST_type_For_Tree(type_tree,
			idx,orig_idx);
		  TYPE_DST_IDX(type_tree) = dst;
#endif
	        }
		TYPE_FIELD_IDS_USED(type_tree) =
			TYPE_FIELD_IDS_USED(gs_type_main_variant(type_tree));
		return idx;
	}

	TYPE_ID mtype;
	INT64 tsize;
	BOOL variable_size = FALSE;
        
	gs_t type_size = gs_type_size(type_tree);
        gs_string_t type_mode  = gs_type_mode (type_tree);
#ifndef KEY
	UINT align = gs_type_align(type_tree) / BITSPERBYTE;
#endif
	if (type_size == NULL) {
		// incomplete structs have 0 size.  Similarly, 'void' is
                // an incomplete type that can never be completed.
		FmtAssert(gs_tree_code(type_tree) == GS_ARRAY_TYPE 
			|| gs_tree_code(type_tree) == GS_ENUMERAL_TYPE
			|| gs_tree_code(type_tree) == GS_UNION_TYPE
			|| gs_tree_code(type_tree) == GS_RECORD_TYPE
			|| gs_tree_code(type_tree) == GS_LANG_TYPE
			|| gs_tree_code(type_tree) == GS_FUNCTION_TYPE
			|| gs_tree_code(type_tree) == GS_VOID_TYPE,
			  ("Create_TY_For_Tree: type_size NULL for non ARRAY/RECORD/VOID, type is %d",
                           (int) gs_tree_code(type_tree)));
		tsize = 0;
	}
	else {
		if (gs_tree_code(type_size) != GS_INTEGER_CST) {
			if (gs_tree_code(type_tree) == GS_ARRAY_TYPE) {
				DevWarn ("Encountered VLA at line %d", lineno);
				tsize = 0;
			}
			else {
			// bugs 943, 11277, 10506
#if defined(TARG_SL)
				ErrMsg(EC_Unimplemented_Feature, "variable-length structure",
				  Orig_Src_File_Name?Orig_Src_File_Name:Src_File_Name, lineno);
#else
				DevWarn ("Encountered variable-length structure at line %d", lineno);
				tsize = -1;
#endif
			}
			variable_size = TRUE;
		}
		else
#ifdef KEY		// bug 3045
			tsize = (gs_get_integer_value(type_size) + BITSPERBYTE - 1)
				  / BITSPERBYTE;
#else
			tsize = gs_get_integer_value(type_size) / BITSPERBYTE;
#endif
	}
	switch (gs_tree_code(type_tree)) {
	case GS_VOID_TYPE:
	case GS_LANG_TYPE: // unknown type
		idx = MTYPE_To_TY (MTYPE_V);	// use predefined type
		break;
	case GS_BOOLEAN_TYPE:
	case GS_INTEGER_TYPE:
	case GS_OFFSET_TYPE:
		switch (tsize) {
		case 1:  mtype = MTYPE_I1;  break;
		case 2:  mtype = MTYPE_I2;  break;
		case 4:  mtype = MTYPE_I4;  break;
		case 8:  mtype = MTYPE_I8;  break;
#if !defined(TARG_X8664) && !defined(TARG_MIPS)  // Bug 12358
#ifdef _LP64
		case 16:  mtype = MTYPE_I8; break;
#endif /* _LP64 */
#else 
	        // needed for compiling variable length array
		// as in gcc.c-torture/execute/920929-1.c
		// we need to fix the rest of the compiler 
		// with _LP64 but seems to work fine without.	
		case 16:  mtype = MTYPE_I8; break;
#endif /* KEY */
		default:  FmtAssert(FALSE,
                                    ("Get_TY unexpected size %d", tsize));
		}
		if (gs_decl_unsigned(type_tree)) {
			mtype = MTYPE_complement(mtype);
		}
#ifdef KEY
		if (lookup_attribute("may_alias",gs_type_attributes(type_tree)))
		{
		  // bug 9975: Handle may_alias attribute, we need to create
		  // a new type to which we can attach the flag.
		  TY &ty = New_TY (idx);
		  TY_Init (ty, tsize, KIND_SCALAR, mtype, 
		           Save_Str(Get_Name(gs_type_name(type_tree))) );
		  Set_TY_no_ansi_alias (ty);
#if defined(TARG_SL)
		  // for -m32, it is not the predefined type, alignment shoule be set.
		  // Corresponding to following code about bug#2932.
		  if (!TARGET_64BIT)  
		    Set_TY_align (idx, align);
#endif
 		} else
#endif
		idx = MTYPE_To_TY (mtype);	// use predefined type
#if defined(TARG_X8664) || defined(TARG_SL)
		/* At least for -m32, the alignment is not the same as the data
		   type's natural size. (bug#2932)
		*/
		if( TARGET_64BIT )
#endif // TARG_X8664
		  Set_TY_align (idx, align);
		break;
	case GS_CHAR_TYPE:
		mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U1 : MTYPE_I1);
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_ENUMERAL_TYPE:
#ifdef KEY
		switch (tsize) {
		  case 1: // bug 14445
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U1 :
		                                               MTYPE_I1);
		        break;
		  case 2: // bug 14445
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U2 :
		                                               MTYPE_I2);
		        break;
		  case 8: // bug 500
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U8 :
		                                               MTYPE_I8);
		        break;
		  default:
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U4 :
		                                               MTYPE_I4);
		}
#else
		mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U4 : MTYPE_I4);
#endif
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_REAL_TYPE:
		switch (tsize) {
		case 4:  mtype = MTYPE_F4; break;
		case 8:  mtype = MTYPE_F8; break;
#if defined(TARG_IA64) || defined(TARG_X8664)
                // the correct way to get the type is from type mode
                // so it can support float_128
                case 12:
                  FmtAssert(!TARGET_64BIT, ("Get_TY unexpected size"));
                  // fall through
                case 16: 
                  {
#ifdef SUPPORT_FLOAT128
                     if (strcmp("XF", type_mode) == 0)
                       mtype = MTYPE_F10; 
                     else if (strcmp("TF",type_mode) == 0)
                       mtype = MTYPE_F16;
                     else 
                       FmtAssert(FALSE, ("Get_TY unexpected size"));
#else
                     mtype = MTYPE_F10;
#endif
                     break;
                  }
#elif defined(TARG_MIPS) || defined(TARG_IA32) 
		case 16: mtype = MTYPE_FQ; break;
#endif /* TARG_MIPS */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_COMPLEX_TYPE:
		switch (tsize) {
		case 2: 
		case 4: ErrMsg (EC_Unsupported_Type, "Complex integer");
		case  8:  mtype = MTYPE_C4; break;
		case 16:  mtype = MTYPE_C8; break;
#if defined(TARG_IA64) || defined(TARG_X8664)
                // the correct way to get the type is from type mode
                // so it can support float_128
                case 24:
                  FmtAssert(!TARGET_64BIT, ("Get_TY unexpected size"));
                  // fall through
                case 32: 
                  {
#ifdef SUPPORT_FLOAT128
                     if (strcmp("XC", type_mode) == 0)
                       mtype = MTYPE_C10; 
                     else if (strcmp("TC",type_mode) == 0)
                       mtype = MTYPE_C16;
                     else 
                       FmtAssert(FALSE, ("Get_TY unexpected size"));
#else
                     mtype = MTYPE_C10;
#endif
                     break;
                  }
#elif defined(TARG_MIPS) || defined(TARG_IA32) 
		case 32: mtype = MTYPE_CQ; break;
#endif /* TARG_MIPS */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_POINTER_TYPE:
		if (gs_type_ptrmem_p(type_tree)) {
			// pointer to member
			idx = Be_Type_Tbl(Pointer_Size == 8 ? MTYPE_I8 : MTYPE_I4);
			break;
		}
		/* FALLTHRU */
	case GS_REFERENCE_TYPE:
		idx = Make_Pointer_Type (Get_TY (gs_tree_type(type_tree)));
		Set_TY_align (idx, align);
		break;
	case GS_ARRAY_TYPE:
		{	// new scope for local vars
#ifdef KEY /* bug 8346 */
		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
		Clear_TY_is_incomplete (idx);
#else
		TY &ty = New_TY (idx);
#endif
		TY_Init (ty, tsize, KIND_ARRAY, MTYPE_M, 
			Save_Str(Get_Name(gs_type_name(type_tree))) );
                // for the anonymoust array
                if (gs_type_name(type_tree) == NULL)
                    Set_TY_anonymous(ty);
		Set_TY_etype (ty, Get_TY (gs_tree_type(type_tree)));
		Set_TY_align (idx, TY_align(TY_etype(ty)));

		// For GNU VLS (Variable length array in struct),
		// the size and upper boundary is expression.
		// If the TYPE_TY_IDX(type_tree) is not set, when
		// expanding the TY's size, it will fall into a infinite
		// recursion if the type_tree is referenced in the
		// size expression. So we set the TYPE_TY_IDX here.
		if (gs_type_readonly(type_tree))
                    Set_TY_is_const (idx);
		if (gs_type_volatile(type_tree))
                    Set_TY_is_volatile (idx);
		if (gs_type_restrict(type_tree))
                    Set_TY_is_restrict (idx);
	        TYPE_TY_IDX(type_tree) = idx;

		// assumes 1 dimension
		// nested arrays are treated as arrays of arrays
		ARB_HANDLE arb = New_ARB ();
		ARB_Init (arb, 0, 0, 0);
		Set_TY_arb (ty, arb);
		Set_ARB_first_dimen (arb);
		Set_ARB_last_dimen (arb);
		Set_ARB_dimension (arb, 1);
		if (gs_type_size(gs_tree_type(type_tree)) == 0)
			break; // anomaly:  type will never be needed

		// =================== Array stride ======================
		if (gs_tree_code(gs_type_size(gs_tree_type(type_tree))) == GS_INTEGER_CST) {
			Set_ARB_const_stride (arb);
			Set_ARB_stride_val (arb, 
				gs_get_integer_value (gs_type_size_unit(gs_tree_type(type_tree))));
		}
#ifdef KEY /* bug 8346 */
		else if (!expanding_function_definition &&
		         processing_function_prototype)
		{
			Set_ARB_const_stride (arb);
			// dummy stride val 4
			Set_ARB_stride_val (arb, 4);
			Set_TY_is_incomplete (idx);
		}
#endif
		else {
			WN *swn;
			swn = WGEN_Expand_Expr (gs_type_size_unit(gs_tree_type(type_tree)));
			if (WN_opcode (swn) == OPC_U4I4CVT ||
			    WN_opcode (swn) == OPC_U8I8CVT) {
				swn = WN_kid0 (swn);
			}
#ifdef KEY
			// In the event that swn operator is not 
			// OPR_LDID, save expr node swn 
			// and use LDID of that stored address as swn.
			// Copied from Wfe_Save_Expr in wfe_expr.cxx
			if (WN_operator (swn) != OPR_LDID) {

			  TYPE_ID   mtype   = WN_rtype(swn);
			  TY_IDX    ty_idx  = MTYPE_To_TY(mtype);
			  ST       *st;
			  st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef FE_GNU_4_2_0
			  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
			  WGEN_Set_ST_Addr_Saved (swn);
			  swn = WN_Stid (mtype, 0, st, ty_idx, swn);
			  WGEN_Stmt_Append (swn, Get_Srcpos());
			  swn = WN_Ldid (mtype, 0, st, ty_idx);
			}
#endif /* KEY */
			FmtAssert (WN_operator (swn) == OPR_LDID,
				("stride operator for VLA not LDID"));
			ST *st = WN_st (swn);
			TY_IDX ty_idx = ST_type (st);
			WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
						   (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WGEN_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_stride (arb);
			Set_ARB_stride_var (arb, (ST_IDX) ST_st_idx (st));
#ifdef KEY /* bug 8346 */
			Clear_TY_is_incomplete (idx);
#endif
		}

		// ================= Array lower bound =================
		Set_ARB_const_lbnd (arb);
		Set_ARB_lbnd_val (arb, 0);

		// ================= Array upper bound =================
		if (type_size) {
#ifdef KEY
		    // For Zero-length arrays, TYPE_MAX_VALUE tree is NULL
		    if (!gs_type_max_value (gs_type_domain (type_tree))) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0xffffffff);
		    } else
#endif /* KEY */
		    if (gs_tree_code(gs_type_max_value (gs_type_domain (type_tree))) ==
			GS_INTEGER_CST) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, gs_get_integer_value (
				gs_type_max_value (gs_type_domain (type_tree)) ));
		    }
#ifdef KEY /* bug 8346 */
		    else if (!expanding_function_definition &&
		             processing_function_prototype) {
			Set_ARB_const_ubnd (arb);
			// dummy upper bound 8
			Set_ARB_ubnd_val (arb, 8);
			Set_TY_is_incomplete (idx);
		    }
#endif
		    else {
			WN *uwn = WGEN_Expand_Expr (gs_type_max_value (gs_type_domain (type_tree)) );
			if (WN_opcode (uwn) == OPC_U4I4CVT ||
			    WN_opcode (uwn) == OPC_U8I8CVT) {
				uwn = WN_kid0 (uwn);
			}
			ST *st;
			TY_IDX ty_idx;
			WN *wn;
			if (WN_operator (uwn) != OPR_LDID) {
				ty_idx  = MTYPE_To_TY(WN_rtype(uwn));
				st = Gen_Temp_Symbol (ty_idx, "__vla_bound");
#ifdef FE_GNU_4_2_0
			  	WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
				wn = WN_Stid (TY_mtype (ty_idx), 0, st, ty_idx, uwn);
				WGEN_Stmt_Append (wn, Get_Srcpos());
			}
			else {
				st = WN_st (uwn);
				ty_idx = ST_type (st);
			}
			wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND, (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WGEN_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_var (arb, ST_st_idx (st));
#ifdef KEY /* bug 8346 */
			Clear_TY_is_incomplete (idx);
#endif
		    }
		}
		else {
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0);
		}

		// ==================== Array size ====================
		if (variable_size) {
#ifdef KEY /* bug 8346 */
		   if (!expanding_function_definition &&
		       processing_function_prototype) {
		     Set_TY_is_incomplete (idx);
		   }
		   else
#endif
		   {
			WN *swn, *wn;
			swn = WGEN_Expand_Expr (gs_type_size_unit(type_tree));
			if (TY_size(TY_etype(ty))) {
				if (WN_opcode (swn) == OPC_U4I4CVT ||
				    WN_opcode (swn) == OPC_U8I8CVT) {
					swn = WN_kid0 (swn);
				}
#ifdef KEY
				// In the event that swn operator is not 
				// OPR_LDID, save expr node swn 
				// and use LDID of that stored address as swn.
				// Copied from Wfe_Save_Expr in wfe_expr.cxx
				if (WN_operator (swn) != OPR_LDID) {
				  TYPE_ID   mtype   = WN_rtype(swn);
				  TY_IDX    ty_idx  = MTYPE_To_TY(mtype);
				  ST       *st;
				  st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef FE_GNU_4_2_0
			  	  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
				  WGEN_Set_ST_Addr_Saved (swn);
				  swn = WN_Stid (mtype, 0, st, ty_idx, swn);
				  WGEN_Stmt_Append (swn, Get_Srcpos());
				  swn = WN_Ldid (mtype, 0, st, ty_idx);
				}
#endif /* KEY */
				FmtAssert (WN_operator (swn) == OPR_LDID,
					("size operator for VLA not LDID"));
				ST *st = WN_st (swn);
				TY_IDX ty_idx = ST_type (st);
				TYPE_ID mtype = TY_mtype (ty_idx);

				wn = WN_Stid (mtype, 0, st, ty_idx, swn);
				WGEN_Stmt_Append (wn, Get_Srcpos());
			}
#ifdef KEY /* bug 8346 */
			Clear_TY_is_incomplete (idx);
#endif
		   }
		}
		} // end array scope
		break;
	case GS_RECORD_TYPE:
	case GS_UNION_TYPE:
		{	// new scope for local vars

		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
#ifdef KEY
		// Must create DSTs in the order that the records are declared,
		// in order to preserve their scope.  Bug 4168.
		if (Debug_Level >= 2)
		  defer_DST_type(type_tree, idx, orig_idx);

		// GCC 3.2 pads empty structures with a fake 1-byte field.
		// These structures should have tsize = 0.
		if (tsize != 0 &&
		    // is_empty_class assumes non-null CLASSTYPE_SIZE
		    // check if it has lang-specific data
		    gs_type_lang_specific(type_tree) &&
		    // check if it has its base version set
		    gs_classtype_as_base(type_tree) &&
		    gs_classtype_size(type_tree) &&
		    gs_is_empty_class(type_tree))
			tsize = 0;
#endif	// KEY
		TY_Init (ty, tsize, KIND_STRUCT, MTYPE_M, 
			Save_Str(Get_Name(gs_type_name(type_tree))) );

		if (gs_type_name(type_tree) == NULL || gs_type_anonymous_p(type_tree))
		    Set_TY_anonymous(ty);

		if (gs_tree_code(type_tree) == GS_UNION_TYPE) {
			Set_TY_is_union(idx);
		}
#ifdef KEY
                // gs_aggregate_value_p is only set for c++
		if (gs_aggregate_value_p(type_tree)) {
			Set_TY_return_in_mem(idx);
		}
#endif
		if (align == 0) align = 1;	// in case incomplete type
		Set_TY_align (idx, align);
		// set idx now in case recurse thru fields
		TYPE_TY_IDX(type_tree) = idx;
		Do_Base_Types (type_tree);

		// Process nested structs and static data members first

                for (gs_t field =  get_first_real_or_virtual_field (type_tree);
                          field;
                          field = next_real_field(type_tree, field)) {
		  	Set_TY_content_seen(idx); // bug 10851
                        if (gs_tree_code(field) == GS_TYPE_DECL ||
			    gs_tree_code(field) == GS_FIELD_DECL) {
                                gs_t field_type = gs_tree_type(field);
				if ((gs_tree_code(field_type) == GS_RECORD_TYPE ||
				     gs_tree_code(field_type) == GS_UNION_TYPE) &&
                                    field_type != type_tree) {
#ifdef KEY
					// Defer typedefs within class
					// declarations to avoid circular
					// declaration dependences.  See
					// example in bug 5134.
                                        if (gs_tree_code(field) == GS_TYPE_DECL)
					  defer_decl(field_type);
                                        else
#endif
                                        Get_TY(field_type);
				}
                        }
#ifdef KEY	// Defer expansion of static vars until all the fields in
		// _every_ struct are laid out.  Consider this code (see
		// bug 3044):
		//  struct A
		//    struct B *p
		//  struct B
		//    static struct A *q = ...	// static data member with
		//                              // initializer
		// We cannot expand static member vars while expanding the
		// enclosing stuct, for the following reason:  Expansion of
		// struct A leads to expansion of p, which leads to the
		// expansion of struct B, which leads to the expansion of q and
		// q's initializer.  The code that expands the initializer goes
		// through the fields of struct A, but these fields are not yet
		// completely defined, and this will cause kg++fe to die.
		//
		// The solution is the delay all static var expansions until
		// the very end.
			else if (gs_tree_code(field) == GS_VAR_DECL)
				defer_decl(field);
#else
			else if (gs_tree_code(field) == GS_VAR_DECL)
				WGEN_Expand_Decl(field, TRUE);
#endif
			else if (gs_tree_code(field) == GS_TEMPLATE_DECL)
				WGEN_Expand_Decl(field, TRUE);
	        }

  		Set_TY_fld (ty, FLD_HANDLE());
		FLD_IDX first_field_idx = Fld_Table.Size ();
		gs_t field;
		gs_t method = gs_type_methods(type_tree);
		FLD_HANDLE fld;
		INT32 next_field_id = 1;

#ifdef KEY
		// In GCC 4, the same tree node representing a vtable ptr field
		// can appear in different derived classes.  As a result,
		// DECL_FIELD_ID(field) can't be used to map its field ID.  As
		// a fix, always allocate field ID 1 to the vtable ptr field.
		// Do this before allocating IDs to any other field.
		gs_t vfield = get_virtual_field(type_tree); 
		if (vfield) {
		  Is_True(gs_tree_code(vfield) == GS_FIELD_DECL,
			  ("Create_TY_For_Tree: bad vfield code"));
		  Is_True(gs_decl_name(vfield) &&
			  !strncmp(Get_Name(gs_decl_name(vfield)),"_vptr", 5),
			  ("Create_TY_For_Tree: bad vfield name"));
		  // The vfield field ID is either not set, or was set to 1.
		  Is_True(DECL_FIELD_ID(vfield) <= 1,
			  ("Create_TY_For_Tree: invalid vfield field ID"));

		  DECL_FIELD_ID(vfield) = next_field_id;	// must be 1
		  next_field_id += TYPE_FIELD_IDS_USED(gs_tree_type(vfield)) +1;
		  fld = New_FLD ();
		  FLD_Init(fld, Save_Str(Get_Name(gs_decl_name(vfield))), 
			   0, // type
			   gs_get_integer_value(gs_decl_field_offset(vfield))
			    + gs_get_integer_value(gs_decl_field_bit_offset(vfield))
			    / BITSPERBYTE);
		}
#endif

		// Generate an anonymous field for every direct, nonempty,
		// nonvirtual base class.  

		INT32 offset = 0;
		INT32 anonymous_fields = 0;
#ifndef KEY	// g++'s class.c already laid out the base types.  Bug 11622.
		gs_t type_binfo, basetypes;
		if ((type_binfo = gs_type_binfo(type_tree)) != NULL &&
		    (basetypes = gs_binfo_base_binfos(type_binfo)) != NULL) {
		  gs_t list;
		  for (list = basetypes; gs_code(list) != EMPTY;
		       list = gs_operand(list, 1)) {
		    gs_t binfo = gs_operand(list, 0);
		    gs_t basetype = gs_binfo_type(binfo);
		    offset = Roundup (offset,
				    gs_type_align(basetype) / BITSPERBYTE);
		    if (!is_empty_base_class(basetype) || 
			!gs_binfo_virtual_p(binfo)) {
		      ++next_field_id;
		      ++anonymous_fields;
		      next_field_id += TYPE_FIELD_IDS_USED(basetype);
		      fld = New_FLD();
		      FLD_Init (fld, Save_Str(Get_Name(0)), 
				Get_TY(basetype), offset);
		      offset += Type_Size_Without_Vbases (basetype);
                      Set_FLD_is_anonymous(fld);
#ifdef KEY
// temporary hack for a bug in gcc
// Details: From layout_class_type(), it turns out that for this
// type, gcc is apparently sending wrong type info, they have 2 fields
// each 8 bytes in a 'record', with the type size == 8 bytes also!
// So we take care of it here...
		      if (offset > tsize)
			{
			    tsize = offset;
			    Set_TY_size (ty, tsize);
			}
#endif // KEY
		    }
		  }
		}
#endif // KEY

                hash_set <gs_t, void_ptr_hash> anonymous_base;
                hash_set <gs_t, void_ptr_hash> virtual_base;
                gs_t type_binfo, basetypes;

                // find all base classes
                if ((type_binfo = gs_type_binfo(type_tree)) != NULL &&
                    (basetypes = gs_binfo_base_binfos(type_binfo)) != NULL) {
                  gs_t list;
                  for (list = basetypes; gs_code(list) != EMPTY;
                       list = gs_operand(list, 1)) {
                    gs_t binfo = gs_operand(list, 0);
                    gs_t basetype = gs_binfo_type(binfo);
                    anonymous_base.insert(basetype);
                    if (gs_binfo_virtual_p(binfo))
                       virtual_base.insert(basetype);
                  } 
                } 

		// Assign IDs to real fields.  The vtable ptr field is already
		// assigned ID 1.
		for (field = get_first_real_field(type_tree); 
			field;
			field = next_real_field(type_tree, field) )
		{
			if (gs_tree_code(field) == GS_TYPE_DECL) {
				continue;
			}
			if (gs_tree_code(field) == GS_CONST_DECL) {
				DevWarn ("got CONST_DECL in field list");
				continue;
			}
			if (gs_tree_code(field) == GS_VAR_DECL) {
				continue;	
			}
			if (gs_tree_code(field) == GS_TEMPLATE_DECL) {
				continue;
			}

			// Either the DECL_FIELD_ID is not yet set, or is
			// already set to the same field ID.  The latter
			// happens when GCC 4 duplicates the type tree and the
			// same field node appears in both type nodes.
			Is_True(DECL_FIELD_ID(field) == 0 ||
				DECL_FIELD_ID(field) == next_field_id,
				("Create_TY_For_Tree: field ID already set"));

			DECL_FIELD_ID(field) = next_field_id;
			next_field_id += 
			  TYPE_FIELD_IDS_USED(gs_tree_type(field)) + 1;
			fld = New_FLD ();
			FLD_Init (fld, Save_Str(Get_Name(gs_decl_name(field))), 
				0, // type
				gs_get_integer_value(gs_decl_field_offset(field)) +
				gs_get_integer_value(gs_decl_field_bit_offset(field))
					/ BITSPERBYTE);
                        if (gs_decl_name(field) == NULL)
                            Set_FLD_is_anonymous(fld);
                        if (anonymous_base.find(gs_tree_type(field)) != anonymous_base.end())
                            Set_FLD_is_base_class(fld); 
                        if (virtual_base.find(gs_tree_type(field)) != virtual_base.end())
                            Set_FLD_is_virtual(fld); 
		}

		TYPE_FIELD_IDS_USED(type_tree) = next_field_id - 1;
  		FLD_IDX last_field_idx = Fld_Table.Size () - 1;
		if (last_field_idx >= first_field_idx) {
			Set_TY_fld (ty, FLD_HANDLE (first_field_idx));
			Set_FLD_last_field (FLD_HANDLE (last_field_idx));
		}

		// now set the fld types.
		fld = TY_fld(ty);
#ifdef KEY
		// Handle the vtable ptr field if it exists.
		if (vfield) {
		  Is_True(gs_tree_code(gs_tree_type(vfield)) == GS_POINTER_TYPE,
		  ("Create_TY_For_Tree: vtable ptr should be GS_POINTER_TYPE"));

		  // As mentioned below, don't expand pointer-type fields to
		  // avoid circular dependences.  Defer expanding the field
		  // type.
		  fld = TY_fld(ty);
		  TY_IDX p_idx = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8),FALSE);
		  Set_FLD_type(fld, p_idx);
		  defer_field(vfield, fld);
		  fld = FLD_next(fld);
		}
#endif
		// first skip the anonymous fields, whose types are already
		// set.
		while (anonymous_fields--)
		  fld = FLD_next(fld);

		for (field = get_first_real_field(type_tree);
		     /* ugly hack follows; traversing the fields isn't
                        the same from run-to-run. fwa? */
			field && fld.Entry();
			field = next_real_field(type_tree, field))
		{
#ifdef KEY
			const  int FLD_BIT_FIELD_SIZE   = 64;
#endif
			if (gs_tree_code(field) == GS_TYPE_DECL)
				continue;
			if (gs_tree_code(field) == GS_CONST_DECL)
				continue;
			if (gs_tree_code(field) == GS_VAR_DECL)
				continue;
			if (gs_tree_code(field) == GS_TEMPLATE_DECL)
				continue;
#ifdef KEY
			// Don't expand the field's type if it's a pointer
			// type, in order to avoid circular dependences
			// involving member object types and base types.  See
			// example in bug 4954.  
			if (gs_tree_code(gs_tree_type(field)) == GS_POINTER_TYPE) {
				// Defer expanding the field's type.  Put in a
				// generic pointer type for now.
				TY_IDX p_idx =
				  Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8),
						    FALSE);
				Set_FLD_type(fld, p_idx);
				defer_field(field, fld);
				fld = FLD_next(fld);
				continue;
			}
#endif
			TY_IDX fty_idx = Get_TY(gs_tree_type(field));

			if ((TY_align (fty_idx) > align) || (TY_is_packed (fty_idx)))
				Set_TY_is_packed (ty);
			if (! gs_tree_this_volatile(field))
			  Clear_TY_is_volatile (fty_idx);
			Set_FLD_type(fld, fty_idx);

			if ( ! gs_decl_bit_field(field)
			  	&& gs_tree_code(gs_tree_type(field)) != GS_RECORD_TYPE
			  	&& gs_tree_code(gs_tree_type(field)) != GS_UNION_TYPE
			  	&& gs_decl_size(field) // bug 10305
				&& gs_get_integer_value(gs_decl_size(field)) > 0
#ifdef KEY
// We don't handle bit-fields > 64 bits. For an INT field of 128 bits, we
// make it 64 bits. But then don't set it as FLD_IS_BIT_FIELD.
				&& gs_get_integer_value(gs_decl_size(field)) <= 
				   FLD_BIT_FIELD_SIZE
				// bug 2401
				&& TY_size(Get_TY(gs_tree_type(field))) != 0
#endif
				&& gs_get_integer_value(gs_decl_size(field))
				  != (TY_size(Get_TY(gs_tree_type(field))) 
					* BITSPERBYTE) )
			{
#ifdef KEY
			        FmtAssert( gs_get_integer_value(gs_decl_size(field)) <=
					   FLD_BIT_FIELD_SIZE,
					   ("field size too big") );
#endif
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because 
				// otherwise the field type is wrong.
				DevWarn("field size %lld doesn't match type size %lld", 
					gs_get_integer_value(gs_decl_size(field)),
					TY_size(Get_TY(gs_tree_type(field)))
						* BITSPERBYTE );
				gs_set_decl_bit_field(field, 1);
			}
			if (gs_decl_bit_field(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld, 
					gs_get_integer_value(
						gs_decl_field_bit_offset(field))
						% BITSPERBYTE);
				Set_FLD_bsize (fld, gs_get_integer_value(
                                                           gs_decl_size(field)));
			}
			fld = FLD_next(fld);
		}

#ifndef KEY	// Don't expand methods by going through TYPE_METHODS,
		// because:
		//   1) It is incorrect to translate all methods in
		//      TYPE_METHODS to WHIRL because some of the methods are
		//      never used, and generating the assembly code for them
		//      might lead to undefined symbol references.  Instead,
		//      consult the gxx_emitted_decls list, which has all the
		//      functions (including methods) that g++ has ever emitted
		//      to assembly.
		//   2) Expanding the methods here will cause error when the
		//      methods are for a class B that appears as a field in an
		//      enclosing class A.  When Get_TY is run for A, it will
		//      call Get_TY for B in order to calculate A's field ID's.
		//      (Need Get_TY to find B's TYPE_FIELD_IDS_USED.)  If
		//      Get_TY uses the code below to expand B's methods, it
		//      will lead to error because the expansion requires the
		//      field ID's of the enclosing record (A), and these field
		//      ID's are not yet defined.

		// process methods
		if (!Enable_WGEN_DFE) {
		if (cp_type_quals(type_tree) == TYPE_UNQUALIFIED) {
			while (method != NULL_TREE) {
				WGEN_Expand_Decl (method, TRUE);
				method = TREE_CHAIN(method);
			}
		}
		}
#endif	// KEY
		} //end record scope
		break;
	case GS_METHOD_TYPE:
		//DevWarn ("Encountered METHOD_TYPE at line %d", lineno);
	case GS_FUNCTION_TYPE:
		{	// new scope for local vars
		gs_t arg;
		INT32 num_args, i;
#ifdef KEY /* bug 8346 */
		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
		Clear_TY_is_incomplete (idx);
#else
		TY &ty = New_TY (idx);
#endif
		TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0); 
		Set_TY_align (idx, 1);
		TY_IDX ret_ty_idx;
		TY_IDX arg_ty_idx;
		TYLIST tylist_idx;

		// allocate TYs for return as well as parameters
		// this is needed to avoid mixing TYLISTs if one
		// of the parameters is a pointer to a function

		ret_ty_idx = Get_TY(gs_tree_type(type_tree));
		for (arg = gs_type_arg_types(type_tree);
		     arg;
		     arg = gs_tree_chain(arg))
		{
		  arg_ty_idx = Get_TY(gs_tree_value(arg));
#ifdef KEY /* bug 8346 */
		  if (TY_is_incomplete (arg_ty_idx) ||
		      (TY_kind(arg_ty_idx) == KIND_POINTER &&
		       TY_is_incomplete(TY_pointed(arg_ty_idx))))
		    Set_TY_is_incomplete (idx);
#endif
		}

		// if return type is pointer to a zero length struct
		// convert it to void
		if (!WGEN_Keep_Zero_Length_Structs    &&
		    TY_mtype (ret_ty_idx) == MTYPE_M &&
		    TY_size (ret_ty_idx) == 0) {
			// zero length struct being returned
		  	DevWarn ("function returning zero length struct at line %d", lineno);
			ret_ty_idx = Be_Type_Tbl (MTYPE_V);
		}

#ifdef KEY
		// If the front-end adds the fake first param, then convert the
		// function to return void.
		if (TY_return_in_mem(ret_ty_idx)) {
		  ret_ty_idx = Be_Type_Tbl (MTYPE_V);
		  Set_TY_return_to_param(idx);		// bugs 2423 2424
		}
#endif
		Set_TYLIST_type (New_TYLIST (tylist_idx), ret_ty_idx);
		Set_TY_tylist (ty, tylist_idx);
		for (num_args = 0, arg = gs_type_arg_types(type_tree);
		     arg;
		     num_args++, arg = gs_tree_chain(arg))
		{
			arg_ty_idx = Get_TY(gs_tree_value(arg));
			Is_True (!TY_is_incomplete (arg_ty_idx) ||
			          TY_is_incomplete (idx),
				  ("Create_TY_For_Tree: unexpected TY flag"));
			if (!WGEN_Keep_Zero_Length_Structs    &&
			    TY_mtype (arg_ty_idx) == MTYPE_M &&
			    TY_size (arg_ty_idx) == 0) {
				// zero length struct passed as parameter
				DevWarn ("zero length struct encountered in function prototype at line %d", lineno);
			}
			else
				Set_TYLIST_type (New_TYLIST (tylist_idx), arg_ty_idx);
		}
		if (num_args)
		{
			Set_TY_has_prototype(idx);
			if (arg_ty_idx != Be_Type_Tbl(MTYPE_V))
			{
				Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
				Set_TY_is_varargs(idx);
			}
			else
				Set_TYLIST_type (Tylist_Table [tylist_idx], 0);
		}
		else
			Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
#ifdef TARG_X8664
		if (!TARGET_64BIT && !TY_is_varargs(idx))
		{
		  // Ignore m{sse}regparm and corresponding attributes at -m64.
		  // Ignore stdcall/fastcall attributes at -m64 and varargs.
		  if (SSE_Reg_Parm ||
		      lookup_attribute("sseregparm",
		                       gs_type_attributes(type_tree)))
		    Set_TY_has_sseregister_parm (idx);
		  if (gs_t attr = lookup_attribute("regparm",
		      gs_type_attributes(type_tree)))
		  {
		    gs_t value = gs_tree_value (attr);
		    Is_True (gs_tree_code(value) == GS_TREE_LIST,
		             ("Expected TREE_LIST"));
		    value = gs_tree_value (value);
		    if (gs_tree_code(value) == GS_INTEGER_CST)
		      Set_TY_register_parm (idx, gs_get_integer_value (value));
		  }
		  else if (Reg_Parm_Count)
		    Set_TY_register_parm (idx, Reg_Parm_Count);

                  if (gs_t attr = lookup_attribute("stdcall",
		      gs_type_attributes(type_tree)))
		  {
		    gs_t value = gs_tree_value (attr);
		    Set_TY_has_stdcall (idx);
		  }
                  else if (gs_t attr = lookup_attribute("fastcall",
		            gs_type_attributes(type_tree)))
		  {
		    gs_t value = gs_tree_value (attr);
		    Set_TY_has_fastcall (idx);
		  }
		}
#endif
		} // end FUNCTION_TYPE scope
		break;
#ifdef TARG_X8664
        // x86 gcc vector types
        case GS_VECTOR_TYPE:
                {
		char *p = gs_type_mode(type_tree);
		idx = 0;
		if (strcmp(p, "BLK") == 0) {
		  TY_IDX elem_ty = Get_TY(gs_tree_type(type_tree));
		  TYPE_ID elem_mtype = TY_mtype(elem_ty);
		  switch (gs_n(gs_type_precision(type_tree))) {
		    case 1: if (elem_mtype == MTYPE_I8)
		    	      idx = MTYPE_To_TY(MTYPE_V8I8);
			    break;
		    case 2: if (elem_mtype == MTYPE_I4)
		    	      idx = MTYPE_To_TY(MTYPE_M8I4);
			    else if (elem_mtype == MTYPE_F4)
		    	      idx = MTYPE_To_TY(MTYPE_V8F4);
		    	    else if (elem_mtype == MTYPE_I8)
		    	      idx = MTYPE_To_TY(MTYPE_V16I8);
			    else if (elem_mtype == MTYPE_F8)
		    	      idx = MTYPE_To_TY(MTYPE_V16F8);
			    break;
		    case 4: if (elem_mtype == MTYPE_I2)
		    	      idx = MTYPE_To_TY(MTYPE_M8I2);
		    	    else if (elem_mtype == MTYPE_I4)
		    	      idx = MTYPE_To_TY(MTYPE_V16I4);
                            else if (elem_mtype == MTYPE_I8)
                              idx = MTYPE_To_TY(MTYPE_V32I8);
			    else if (elem_mtype == MTYPE_F4)
		    	      idx = MTYPE_To_TY(MTYPE_V16F4);
                            else if (elem_mtype == MTYPE_F8)
                              idx = MTYPE_To_TY(MTYPE_V32F8);
			    break;
		    case 8: if (elem_mtype == MTYPE_I1)
		    	      idx = MTYPE_To_TY(MTYPE_M8I1);
		    	    else if (elem_mtype == MTYPE_I2)
		    	      idx = MTYPE_To_TY(MTYPE_V16I2);
                            else if (elem_mtype == MTYPE_I4)
                              idx = MTYPE_To_TY(MTYPE_V32I4);
                            else if (elem_mtype == MTYPE_F4)
                              idx = MTYPE_To_TY(MTYPE_V32F4);
			    break;
		    case 16: if (elem_mtype == MTYPE_I1)
		    	       idx = MTYPE_To_TY(MTYPE_V16I1);
                             else if (elem_mtype == MTYPE_I2)
                               idx = MTYPE_To_TY(MTYPE_V32I2);
			     break;
                    case 32: if (elem_mtype == MTYPE_I1)
                               idx = MTYPE_To_TY(MTYPE_V32I1);
                             break;
		    default:
		      Fail_FmtAssertion ("Get_TY: unexpected vector type element count");
		  }
		}
		else { // use string emcoded in TYPE_MODE
		  if (toupper(*p++) != 'V') {
		    if (gs_type_name(type_tree)) {
		      p = gs_identifier_pointer(gs_decl_name(gs_type_name(type_tree)));
		      if (toupper(*p++) != 'V') 
			Fail_FmtAssertion("Get_TY: NYI");
		    }
		    else Fail_FmtAssertion("Get_TY: NYI");
		  }
		  int num_elems = strtol(p, &p, 10);
                  if (strncasecmp(p, "DI", 2) == 0) {
                    if (num_elems == 1)
                      idx = MTYPE_To_TY(MTYPE_V8I8);
                    else if (num_elems == 2)
                      idx = MTYPE_To_TY(MTYPE_V16I8);
                    else if (num_elems == 4)
                      idx = MTYPE_To_TY(MTYPE_V32I8);
                  }
                  else if (strncasecmp(p, "DF", 2) == 0) {
                    if (num_elems == 2)
                      idx = MTYPE_To_TY(MTYPE_V16F8);
                    else if (num_elems == 4)
                      idx = MTYPE_To_TY(MTYPE_V32F8);
                  }
		  else if (strncasecmp(p, "SI", 2) == 0) {
		    if (num_elems == 2)
		      idx = MTYPE_To_TY(MTYPE_M8I4);
		    else if (num_elems == 4)
		      idx = MTYPE_To_TY(MTYPE_V16I4);
		    else if (num_elems == 8)
		      idx = MTYPE_To_TY(MTYPE_V32I4);
		  }
		  else if (strncasecmp(p, "SF", 2) == 0) {
		    if (num_elems == 2)
		      idx = MTYPE_To_TY(MTYPE_V8F4);
		    else if (num_elems == 4)
		      idx = MTYPE_To_TY(MTYPE_V16F4);
                    else if (num_elems == 8)
                      idx = MTYPE_To_TY(MTYPE_V32F4);
		  }
		  else if (strncasecmp(p, "HI", 2) == 0) {
		    if (num_elems == 4)
		      idx = MTYPE_To_TY(MTYPE_M8I2);
		    else if (num_elems == 8)
		      idx = MTYPE_To_TY(MTYPE_V16I2);
                    else if (num_elems == 16)
                      idx = MTYPE_To_TY(MTYPE_V32I2);
		  }
		  else if (strncasecmp(p, "QI", 2) == 0) {
		    if (num_elems == 8)
		      idx = MTYPE_To_TY(MTYPE_M8I1);
		    else if (num_elems == 16)
		      idx = MTYPE_To_TY(MTYPE_V16I1);
                    else if (num_elems == 32)
                      idx = MTYPE_To_TY(MTYPE_V32I1);
		  }
		}
		if (idx == 0)
		  Fail_FmtAssertion ("Get_TY: unexpected vector type");
                }
                break;
#endif // TARG_X8664
	default:
		FmtAssert(FALSE, ("Get_TY unexpected tree_type"));
	}
	if (gs_type_readonly(type_tree))
		Set_TY_is_const (idx);
	if (gs_type_volatile(type_tree))
		Set_TY_is_volatile (idx);
#ifdef KEY
	if (gs_type_restrict(type_tree))
		Set_TY_is_restrict (idx);
#endif
	TYPE_TY_IDX(type_tree) = idx;
        if(Debug_Level >= 2) {
#ifdef KEY
	  // DSTs for records were entered into the defer list in the order
	  // that the records are declared, in order to preserve their scope.
	  // Bug 4168.
	  if (gs_tree_code(type_tree) != GS_RECORD_TYPE &&
	      gs_tree_code(type_tree) != GS_UNION_TYPE &&
	      // Bugs 8346, 11819: Insert a TY for DST processing only
	      // when the TY is complete to ensure that when the DST info
	      // are created, the TY will be valid.
	      !TY_is_incomplete(idx) &&
	      !(TY_kind(idx) == KIND_POINTER &&
	        TY_is_incomplete(TY_pointed(idx)))) {
	    // Defer creating DST info until there are no partially constructed
	    // types, in order to prevent Create_DST_type_For_Tree from calling
	    // Get_TY, which in turn may use field IDs from partially created
	    // structs.  Such fields IDs are wrong.  Bug 5658.
	    defer_DST_type(type_tree, idx, orig_idx);
	  }
#else
          DST_INFO_IDX dst =
            Create_DST_type_For_Tree(type_tree,
              idx,orig_idx);
          TYPE_DST_IDX(type_tree) = dst;
#endif
        }

	return idx;
}

void 
Create_DST_For_Tree (gs_t decl_node, ST* st)
{
  DST_INFO_IDX dst =
    Create_DST_decl_For_Tree(decl_node,st);
  DECL_DST_IDX(decl_node) = dst; 
  return;
}

// if there is a PARM_DECL with the same name (as opposed to same node), 
// use the ST created for it
ST *
Search_decl_arguments(char *name)
{
  gs_t p;
  if (name) {
    for (p = decl_arguments; p; p = gs_tree_chain(p)) {
      if (gs_decl_name(p) == NULL) // matches with any parm with null name (sanity32.C)
	return DECL_ST(p);
      if (strcmp(name, (char *) gs_identifier_pointer(gs_decl_name(p))) == 0)
	return DECL_ST(p);
    }
  }
  else { // search for an argument with no name
    for (p = decl_arguments; p; p = gs_tree_chain(p)) {
      if (gs_decl_name(p) == NULL)
	return DECL_ST(p);
    }
  }
  return NULL;
}

#ifdef KEY // bug 12668
static BOOL
Has_label_decl(gs_t init)
{
  if (gs_tree_code(init) == GS_LABEL_DECL)
    return TRUE;
  if (gs_tree_code(init) == GS_ADDR_EXPR)
    return Has_label_decl(gs_tree_operand(init,0));
#ifdef FE_GNU_4_2_0 // bug 12699
  if (gs_tree_code(init) == GS_NOP_EXPR)
    return Has_label_decl(gs_tree_operand(init,0));
#endif
  if (gs_tree_code(init) == GS_CONSTRUCTOR) {
#ifdef FE_GNU_4_2_0
    INT length = gs_constructor_length(init);
    gs_t element_value;
    for (INT idx = 0; idx < length; idx++) {
      element_value = gs_constructor_elts_value(init, idx);
      if (Has_label_decl(element_value))
	return TRUE;
    }
#else
    gs_t nd;
    for (nd = gs_constructor_elts(init); nd; nd = gs_tree_chain(nd)) {
      if (Has_label_decl(gs_tree_value(nd)))
	return TRUE;
    }
#endif
  }
  return FALSE;
}
#endif

ST*
Create_ST_For_Tree (gs_t decl_node)
{
  TY_IDX     ty_idx;
  ST*        st = NULL;
  char      *name;
  char	    tempname[32];
  ST_SCLASS  sclass;
  ST_EXPORT  eclass;
  SYMTAB_IDX level;
  static INT anon_count = 0;


  // If the decl is a function decl, and there are duplicate decls for the
  // function, then use a ST already allocated for the function, if such ST
  // exists.
  if (gs_tree_code(decl_node) == GS_FUNCTION_DECL) {
    st = get_duplicate_st (decl_node);
    if (st) {
      set_DECL_ST(decl_node, st);
      return st;
    }
  }

  // For variables with asm register assignments, don't use the assembler
  // names because they are of the form "%rbx".
  if (gs_tree_code(decl_node) == GS_RESULT_DECL) {
    sprintf(tempname, ".result_decl_%d", gs_decl_uid(decl_node));
    name = tempname;
  }
  else if ((gs_tree_code(decl_node) == GS_FUNCTION_DECL ||
            gs_tree_code(decl_node) == GS_PARM_DECL ||
            (gs_tree_code(decl_node) == GS_VAR_DECL &&
             gs_decl_asmreg(decl_node) >= 0)) &&
           gs_decl_name(decl_node) != 0)
    name = (char *) gs_identifier_pointer (gs_decl_name (decl_node));
  else if (gs_decl_assembler_name (decl_node) && gs_decl_name (decl_node))
    name = (char *) gs_identifier_pointer (gs_decl_assembler_name (decl_node));
  else if (gs_decl_name (decl_node))
    name = (char *) gs_identifier_pointer (gs_decl_name (decl_node));
  else {
    sprintf(tempname, TVAR_PREFIX "%d", ++anon_count);
    name = tempname;
  }

#ifdef KEY
  BOOL guard_var = FALSE;
  // See if variable is a guard variable.
  if (strncmp("_ZGV", name, 4) == 0) {
    guard_var = TRUE;
  }
#endif

  switch (gs_tree_code(decl_node)) {

    case GS_FUNCTION_DECL:
      {
        if (Enable_WFE_DFE) {
          gs_t body = gs_decl_saved_tree(decl_node);
          if (gs_decl_thunk_p(decl_node) &&
              gs_tree_code(gs_cp_decl_context(decl_node)) != GS_NAMESPACE_DECL)
            Push_Deferred_Function (decl_node);
/*
          else
          if (DECL_TINFO_FN_P(decl_node))
            Push_Deferred_Function (decl_node);
*/
          else
          if (body != NULL && !gs_decl_external(decl_node) &&
              (gs_decl_template_info(decl_node) == NULL              ||
               gs_decl_friend_pseudo_template_instantiation(decl_node) ||
               gs_decl_template_instantiated(decl_node)              ||
               gs_decl_template_specialization(decl_node))) {
            Push_Deferred_Function (decl_node);
          }
        }

#ifdef KEY /* bug 8346 */
        Is_True (!processing_function_prototype,
                 ("Create_ST_For_Tree: processing another function prototype?"));
        processing_function_prototype = TRUE;
        TY_IDX func_ty_idx = Get_TY(gs_tree_type(decl_node));
        processing_function_prototype = FALSE;
#else
        TY_IDX func_ty_idx = Get_TY(gs_tree_type(decl_node));
#endif

        sclass = SCLASS_EXTERN;
        eclass = gs_tree_public(decl_node) || gs_decl_weak(decl_node) ?
		   EXPORT_PREEMPTIBLE				:
		   EXPORT_LOCAL;
        level  = GLOBAL_SYMTAB+1;

        PU_IDX pu_idx;
        PU&    pu = New_PU (pu_idx);

        PU_Init (pu, func_ty_idx, level);

#ifdef KEY
        st = New_ST (level - 1);
#else
        st = New_ST (GLOBAL_SYMTAB);
#endif

        // Fix bug # 34, 3356
// gcc sometimes adds a '*' and itself handles it this way while outputing
	char *p;
	if (gs_decl_assembler_name(decl_node) == NULL)
	  p = name;
	else p  = gs_identifier_pointer (gs_decl_assembler_name (decl_node));
	if (*p == '*')
	  p++;
        ST_Init (st, Save_Str(p),
                 CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));

        // St is a constructor
        if (gs_decl_complete_constructor_p(decl_node) && !gs_decl_copy_constructor_p(decl_node))
            Set_PU_is_constructor(pu);
        // St is a pure virual function
        if (gs_decl_pure_virtual_p(decl_node) || strncmp(p, "__cxa_pure_virtual", 18) == 0)
            Set_ST_is_pure_vfunc(st);

        if (gs_tree_code(gs_tree_type(decl_node)) == GS_METHOD_TYPE) { 
            TY_IDX base = Get_TY(gs_type_method_basetype(gs_tree_type(decl_node))); 
            Set_PU_base_class(pu, base); 
        } 
  
	if (gs_decl_thunk_p(decl_node) &&
            gs_tree_code(gs_cp_decl_context(decl_node)) != GS_NAMESPACE_DECL &&
            eclass != EXPORT_LOCAL &&
            eclass != EXPORT_LOCAL_INTERNAL)
	  Set_ST_is_weak_symbol(st);

        // process attributes for FUNCTION_DECL
        gs_t attr_list = gs_decl_attributes(decl_node);
        for ( ; attr_list != NULL; attr_list = gs_tree_chain(attr_list) ) {
                Is_True(gs_tree_code(attr_list) == GS_TREE_LIST,
                                ("lookup_attributes: TREE_LIST node not found"));
                gs_t attr = gs_tree_purpose(attr_list);
                if ( is_attribute("noreturn", attr) ) // __attribute__((noreturn))
                        Set_PU_has_attr_noreturn (pu);
        }

        if (gs_tree_nothrow (decl_node)) {
          Set_PU_nothrow (pu);
        }
      }
      break;

    case GS_INDIRECT_REF:
        if (gs_tree_code(gs_tree_operand(decl_node, 0)) == GS_RESULT_DECL &&
            DECL_ST(gs_tree_operand(decl_node,0)))
            return DECL_ST(gs_tree_operand(decl_node,0));
            
        Fail_FmtAssertion("Create_ST_For_Tree: not expected GS_INDIRECT_REF");
        
#ifdef KEY
    case GS_RESULT_DECL: // bug 3878
#endif
    case GS_PARM_DECL:
    case GS_VAR_DECL:
      {
        if (gs_tree_code(decl_node) == GS_PARM_DECL) {
#ifdef KEY
	  // wgen fix for C++ and also for C, as in bug 8346.
	  if (decl_arguments) {
	    st = Search_decl_arguments(gs_decl_name(decl_node) ? name : NULL);
	    if (st) {
	      set_DECL_ST(decl_node, st); // created now
	      return st;
	    }
	  }
#endif
          sclass = SCLASS_FORMAL;
          eclass = EXPORT_LOCAL;
          level = CURRENT_SYMTAB;
        }
        else {
          if (gs_decl_context (decl_node) == 0 			     ||
	      gs_tree_code (gs_decl_context (decl_node)) == GS_NAMESPACE_DECL ||
 	      gs_tree_code (gs_decl_context (decl_node)) == GS_RECORD_TYPE ) {
            if (gs_tree_public (decl_node)) {
	      // GCC 3.2
	      if (gs_decl_external(decl_node) ||
		  (gs_decl_lang_specific(decl_node) &&
		   gs_decl_really_extern(decl_node)))
		sclass = SCLASS_EXTERN;
	      else
	      if (gs_decl_initial(decl_node))
		sclass = SCLASS_UGLOBAL;
	      else if (gs_tree_static(decl_node)) {
#ifdef KEY
// bugs 340, 3717
		if (flag_no_common || !gs_decl_common (decl_node) ||
		    (!lang_cplus /* bug 14187 */ &&
		     gs_decl_section_name (decl_node) /* bug 14181 */))
#else
		if (flag_no_common)
#endif
		  sclass = SCLASS_UGLOBAL;
		else
		  sclass = SCLASS_COMMON;
	      }
	      else
              	sclass = SCLASS_EXTERN;
              eclass = EXPORT_PREEMPTIBLE;
            }
            else {
              	sclass = SCLASS_FSTATIC;
		eclass = EXPORT_LOCAL;
            }
            level = GLOBAL_SYMTAB;
          }
          else {
#ifdef KEY
	    // .gnu.linkonce.b is .bss with DECL_ONE_ONLY set.  Bug 10876.
	    gs_t section_name = gs_decl_section_name(decl_node);
	    if (section_name &&
		!strncmp(gs_tree_string_pointer(section_name),
			 ".gnu.linkonce.", 14)) {
	      if (!strncmp(gs_tree_string_pointer(section_name),
			   ".gnu.linkonce.b.", 16)
	          // bug 13054
	          || !strncmp(gs_tree_string_pointer(section_name),
	                      ".gnu.linkonce.sb.", 17)) {
		sclass = SCLASS_UGLOBAL;
		level  = GLOBAL_SYMTAB;
		eclass = EXPORT_PREEMPTIBLE;
	      } else {
		// Add support as needed.
		Fail_FmtAssertion("Create_ST_For_Tree: %s section NYI",
				  gs_tree_string_pointer(section_name));
	      }
	    }
	    // bug 13090 and 13245
	    // Bug 13047 shows that the gnu42 front-end (specifically
	    // the gcc/g++ part) behaves differently when built on a gnu3
	    // system, than when built on a gnu4 system. If the compiler
	    // is built on a gnu4 system, default_unique_section() in
	    // varasm.c will never generate a linkonce section because
	    // starting GNU42, this also depends on whether the host
	    // compiling system has COMDAT groups.
	    else if (section_name &&
	             (!strncmp(gs_tree_string_pointer(section_name),
			       ".sbss.", 6) ||
		      !strncmp(gs_tree_string_pointer(section_name),
			       ".bss.", 5))) {
	      sclass = SCLASS_UGLOBAL;
	      level  = GLOBAL_SYMTAB;
	      eclass = EXPORT_PREEMPTIBLE;
	    }
	    else
#endif
            if (gs_decl_external(decl_node) || gs_decl_weak(decl_node)) {
	      // OSP_255
	      // Not all weak symbols are EXTERN: COMMON&WEAK, STATIC&WEAK
	      if (!flag_no_common && gs_decl_common (decl_node)) {
		// COMMON & WEAK: 
		//   static vars in exported inline/template functions(IA64)
		sclass = SCLASS_COMMON;
	      }
	      else if (gs_tree_static (decl_node)) {
		// STATIC & WEAK:
		//   static vars in exported inline/template function(X8664) 
		sclass = SCLASS_UGLOBAL;
	      }
	      else {
		// OTHERS:
		//   treat it EXTERN ( will not allocate space )
	        sclass = SCLASS_EXTERN;
	      }
	      level  = GLOBAL_SYMTAB;
              eclass = EXPORT_PREEMPTIBLE;
            }
#ifdef KEY
	    // Bug 8652: If GNU marks it as COMMON, we should the same.
	    else if (!flag_no_common &&
		     gs_tree_static (decl_node) &&
	             gs_decl_common (decl_node) &&
		     gs_tree_public (decl_node)) {
	      sclass = SCLASS_COMMON;
	      level = GLOBAL_SYMTAB;
	      eclass = EXPORT_PREEMPTIBLE;
	    }
#endif
            else {
	      if (gs_tree_static (decl_node)) {
		sclass = SCLASS_PSTATIC;
               if (pstatic_as_global
#ifdef KEY // bug 12668
                   && ! (gs_decl_initial(decl_node) &&
                         !gs_decl_external(decl_node) &&
                         Has_label_decl(gs_decl_initial(decl_node)))
#endif
                 )
			level = GLOBAL_SYMTAB;
		else
			level = CURRENT_SYMTAB;
              }
              else {
		sclass = SCLASS_AUTO;
	 	level = DECL_SYMTAB_IDX(decl_node) ?
			DECL_SYMTAB_IDX(decl_node) : CURRENT_SYMTAB;
              }
              eclass = EXPORT_LOCAL;
            }
          }
        }
	if (guard_var) {
          // This is a guard variable created by the g++ front-end to protect
          // against multiple initializations (and destruction) of symbols 
          // with static storage class. Make it local unless it's weak.
	  level = GLOBAL_SYMTAB;
          if ( gs_decl_weak(decl_node) ) {
	    sclass = SCLASS_UGLOBAL;
	    eclass = EXPORT_PREEMPTIBLE;
          }
          else {
            sclass = SCLASS_PSTATIC;
            eclass = EXPORT_LOCAL;
          }
	}
        else if (gv_cond_expr) {
          //Make guard variable for condtional expressions a local stack 
          //variable to avoid being over-written when evaluating nested 
          //conditional expressions.
          //See comments for WGEN_add_guard_var in wgen_expr.cxx 
          //for information on conditional expressions.
          level = DECL_SYMTAB_IDX(decl_node) ?
                  DECL_SYMTAB_IDX(decl_node) : CURRENT_SYMTAB;
          sclass = SCLASS_AUTO;
          eclass = EXPORT_LOCAL;
	} 

	// The tree under DECL_ARG_TYPE(decl_node) could reference decl_node.
	// If that's the case, the Get_TY would create the ST for decl_node.
	// As a result, call GET_TY first, then check if the ST is already
	// created, and create ST only if it isn't created.
        ty_idx = Get_TY (gs_tree_type(decl_node));
	st = DECL_ST(decl_node);
	if (st)
	  return st;
        st = New_ST (level);

        // Set line number where define sym in source file
        if (gs_operand (decl_node, GS_DECL_SOURCE_LINE))
          Set_ST_Line(*st, gs_decl_source_line(decl_node));
        else
          Set_ST_Line(*st, 0);

        if (TY_kind (ty_idx) == KIND_ARRAY &&
            gs_tree_static (decl_node) &&
            gs_decl_initial (decl_node) == FALSE &&
            TY_size (ty_idx) == 0) {
	  Set_TY_size (ty_idx, TY_size (Get_TY (gs_tree_type (gs_tree_type (decl_node)))));
        }
#ifndef KEY
// bug 3735: the compiler cannot arbitrarily change the alignment of
// individual structures
	if (TY_mtype (ty_idx) == MTYPE_M &&
	    Aggregate_Alignment > 0 &&
	    Aggregate_Alignment > TY_align (ty_idx))
	  Set_TY_align (ty_idx, Aggregate_Alignment);
#endif // !KEY
	// qualifiers are set on decl nodes
	if (gs_tree_readonly(decl_node))
		Set_TY_is_const (ty_idx);
	if (gs_tree_this_volatile(decl_node))
		Set_TY_is_volatile (ty_idx);
	else Clear_TY_is_volatile (ty_idx);
	if (gs_decl_user_align (decl_node))
		Set_TY_is_user_align(ty_idx);
	else Clear_TY_is_user_align(ty_idx);
#ifdef KEY
        // Handle aligned attribute (bug 7331)
        if (gs_decl_user_align (decl_node))
          Set_TY_align (ty_idx, gs_decl_align_unit(decl_node));
        // NOTE: we do not update the ty_idx value in the TYPE_TREE. So
        // if any of the above properties are set, the next time we get into
        // Get_ST, the ty_idx in the TYPE_TREE != ty_idx in st. The solution
        // is either to update TYPE_TREE now, or compare the ty_idx_index
        // in Get_ST (instead of ty_idx). Currently we do the latter
#endif // KEY
	// bug 34 3356 10892: gcc sometimes adds a '*' and itself handles it 
	//   this way while outputing
	char *p = name;
	if (*p == '*')
	  p++;
        ST_Init (st, Save_Str(p), CLASS_VAR, sclass, eclass, ty_idx);
		
      if (gs_decl_virtual_p(decl_node) && strncmp(name, "_ZTV", 4) == 0)
      {
          Set_ST_is_vtable(st);
          Set_ST_vtable_ty_idx(st, Get_TY(gs_cp_decl_context(decl_node)));
      }
	  
#ifdef KEY
#ifdef FE_GNU_4_2_0
	if (gs_tree_code (decl_node) == GS_VAR_DECL &&
	    // Bug 12968: just checking for threadprivate flag is not
	    // sufficient, because for C the flag is basically
	    // gs_decl_lang_flag_3.
	    gs_decl_thread_local (decl_node) &&
	    ((!lang_cplus && gs_c_decl_threadprivate_p (decl_node)) ||
	     (lang_cplus && gs_cp_decl_threadprivate_p (decl_node))))
	  Set_ST_is_thread_private (st);

	if (gs_tree_code (decl_node) == GS_VAR_DECL && sclass == SCLASS_AUTO)
	  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif

        if (gs_decl_size_unit (decl_node) &&
            gs_tree_code (gs_decl_size_unit (decl_node)) != GS_INTEGER_CST)
        {
            // if this is the first alloca, save sp.
            int idx;
            if (!Set_Current_Scope_Has_Alloca (idx))
            {
              ST * save_st = WGEN_Alloca_0 ();
              Set_Current_Scope_Alloca_St (save_st, idx);
            }
            WN * size = WGEN_Expand_Expr (gs_decl_size_unit (decl_node));
            // mimic WGEN_Alloca_ST
            ST * alloca_st = New_ST (CURRENT_SYMTAB);
            ST_Init (alloca_st, Save_Str (name),
                       CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
                                  Make_Pointer_Type (ty_idx, FALSE));
            Set_ST_is_temp_var (alloca_st);
            Set_ST_pt_to_unique_mem (alloca_st);
            Set_ST_base_idx (st, ST_st_idx (alloca_st));
            WN *wn  = WN_CreateAlloca (size);
            wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
            WGEN_Stmt_Append (wn, Get_Srcpos());
            Set_PU_has_alloca (Get_Current_PU());
	    // For kids 1..n of DEALLOCA
            Add_Current_Scope_Alloca_St (alloca_st, idx);
        }
#endif // KEY
        if (gs_tree_code(decl_node) == GS_PARM_DECL) {
		Set_ST_is_value_parm(st);
        }
      }
      break; 
    default:
      {
        Fail_FmtAssertion ("Create_ST_For_Tree: unexpected tree type %s",
			   WGEN_Tree_Node_Name(decl_node));
      }
      break;
  }

  set_DECL_ST(decl_node, st); // created now

  // If VAR_DECL has a non-zero DECL_ASMREG, then DECL_ASMREG-1 is the register
  // number assigned by an "asm".
  if (gs_tree_code(decl_node) == GS_VAR_DECL && gs_decl_register(decl_node) &&
      gs_decl_asmreg(decl_node) >= 0) {
    extern PREG_NUM Map_Reg_To_Preg []; // defined in common/com/arch/config_targ.cxx
    int reg = gs_decl_asmreg(decl_node);
    PREG_NUM preg = Map_Reg_To_Preg [reg];
#if defined(TARG_SL)
    if (preg < 0 || preg > 31)
      ErrMsg (EC_Unimplemented_Feature, "Variable in Special register",
        Orig_Src_File_Name?Orig_Src_File_Name:Src_File_Name, lineno);
#endif

    FmtAssert (preg >= 0,
               ("mapping register %d to preg failed\n", reg));
    TY_IDX ty_idx = ST_type (st);
    Set_TY_is_volatile (ty_idx);
    Set_ST_type (st, ty_idx);
    Set_ST_assigned_to_dedicated_preg (st);
    ST_ATTR_IDX st_attr_idx;
    // OSP_325, change CURRENT_SYMTAB to level to 
    //   make the level of the ST and ST_ATTR the same.
    ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_DEDICATED_REGISTER, preg);
  }

  if (gs_tree_code(decl_node) == GS_VAR_DECL) {
    if (gs_decl_context(decl_node) &&
	gs_tree_code(gs_decl_context(decl_node)) == GS_RECORD_TYPE) {
      Get_TY(gs_decl_context(decl_node));
    }
    if (gs_decl_thread_local(decl_node)
#ifdef FE_GNU_4_2_0
        // Bug 12891: threadprivate variables are also marked thread-local
        // by GNU, but we don't want to tell our backend such variables are
        // thread-local.
        &&  ((!lang_cplus && !gs_c_decl_threadprivate_p(decl_node)) ||
             (lang_cplus && !gs_cp_decl_threadprivate_p(decl_node)))
#endif
       ) {
      Set_ST_is_thread_local(st);
    }
  }

  if (Enable_WFE_DFE) {
    if (gs_tree_code(decl_node) == GS_VAR_DECL &&
        level == GLOBAL_SYMTAB &&
        !gs_decl_external (decl_node) &&
        gs_decl_initial (decl_node)) {
      Push_Deferred_Function (decl_node);
    }
  }

  if (gs_decl_weak      (decl_node) &&
      (!gs_decl_external (decl_node)
#ifdef KEY
       // Make weak symbols for:
       //   extern "C" int bar() __attribute__ ((weak, alias("foo")))
       // Bug 3841.
       || gs_decl_alias_target(decl_node))
#endif
      ) {
    Set_ST_is_weak_symbol (st);
  }

#ifdef KEY
  // Make all symbols referenced in cleanup code and try handler code weak.
  // This is to work around an implementation issue where kg++fe always emit
  // the code in a cleanup or try handler, regardless of whether such code is
  // emitted by g++.  If the code calls a function foo that isn't emitted by
  // g++ into the RTL, then foo won't be tagged as needed, and the WHIRL for
  // foo won't be genterated.  This leads to undefined symbol at link-time.
  //
  // The correct solution is to mimick g++ and generate the cleanup/handler
  // code only if the region can generate an exception.  g++ does this in
  // except.c by checking for "(flag_non_call_exceptions ||
  // region->may_contain_throw)".  This checking isn't done in kg++fe because
  // the equivalent of "region->may_contain_throw" isn't (yet) implemented.
  // For now, work around the problem by making all symbols refereced in
  // cleanups and try handlers as weak.
  if (make_symbols_weak) {
    if (eclass != EXPORT_LOCAL &&
	eclass != EXPORT_LOCAL_INTERNAL &&
	// Don't make symbol weak if it is defined in current file.  Workaround
	// for SLES 8 linker.  Bug 3758.
	WEAK_WORKAROUND(st) != WEAK_WORKAROUND_dont_make_weak &&
	// Don't make builtin functions weak.  Bug 9534.
	!(gs_tree_code(decl_node) == GS_FUNCTION_DECL &&
	  gs_decl_built_in(decl_node))) {
      Set_ST_is_weak_symbol (st);
      WEAK_WORKAROUND(st) = WEAK_WORKAROUND_made_weak;
    }
  }
  // See comment above about guard variables.
  else if (guard_var) {
    if ( gs_decl_weak(decl_node) ) {
      Set_ST_is_weak_symbol (st);
    }
    Set_ST_init_value_zero (st);
    Set_ST_is_initialized (st);
  }
#endif

  if (gs_decl_section_name (decl_node)) {
    if (strncmp(gs_tree_string_pointer (gs_decl_section_name (decl_node)), 
                ".gnu.linkonce.",
                14) != 0 ) {
      // OSP. only handle non-.gnu.linkonce.* section name
    DevWarn ("section %s specified for %s",
             gs_tree_string_pointer (gs_decl_section_name (decl_node)),
             ST_name (st));
    if (gs_tree_code (decl_node) == GS_FUNCTION_DECL)
      level = GLOBAL_SYMTAB;
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_SECTION_NAME,
                  Save_Str (gs_tree_string_pointer (gs_decl_section_name (decl_node))));
    if (!lang_cplus) // bug 14187
      Set_ST_has_named_section (st);
    }
    else {
      // OSP. Ignore .gnu.linkonce.* section name
      DevWarn ("Ignore section %s specified for %s",
               gs_tree_string_pointer (gs_decl_section_name (decl_node)),
               ST_name (st));
    }
  }

  // For external variable without an initializer, if it has
  // a "const" qualifier, we mark the symbol const var.
  if (gs_tree_code(decl_node) == GS_VAR_DECL && 
      gs_tree_readonly(decl_node) && !gs_tree_this_volatile(decl_node) &&
      gs_tree_public(decl_node) && gs_decl_external(decl_node) &&
      !ST_is_initialized(st)) 
  {
    Set_ST_is_const_var(st);
  }

#if defined(TARG_IA64)
  //lookup syscall_linkage attribute for FUNCTION_DECL
  if (gs_tree_code (decl_node) == GS_FUNCTION_DECL)
  {
    // Iterate the attributes of the type of the decl
    gs_t type_attr_list = gs_type_attributes(gs_tree_type(decl_node));
    gs_t type_attr;
    for ( type_attr = type_attr_list; type_attr; type_attr = gs_tree_chain(type_attr)) {

      if (gs_tree_purpose(type_attr) != NULL && 
          gs_code(gs_tree_purpose(type_attr)) == GS_IDENTIFIER_NODE ) {
        const char * attr_name = gs_tree_string_pointer(gs_tree_purpose(type_attr));
        if( strcmp("syscall_linkage", attr_name) == 0 ) {
          // this function has attribute "syscall_linkage"
          DevWarn("Encounter syscall_linkage attribute!!!");
          Set_PU_has_syscall_linkage (Pu_Table [ST_pu(st)]);
          // We only handle the "syscall_linkage" so far
          break;
        }
      }
    } 
  }
#endif

  if (gs_tree_code (decl_node) == GS_VAR_DECL)
  {
    // for function decls visibility, needs to delay the process to WGEN_Start_Function
    gs_symbol_visibility_kind_t vk = 
      (gs_symbol_visibility_kind_t) gs_decl_visibility(decl_node);
    if (gs_tree_public (decl_node) &&
        (gs_decl_visibility_specified(decl_node) ||
        GS_VISIBILITY_DEFAULT != vk) ) {
      ST_EXPORT export_class = EXPORT_PREEMPTIBLE;
      switch (vk) {
        case GS_VISIBILITY_DEFAULT:
          export_class = EXPORT_PREEMPTIBLE;
          break;
        case GS_VISIBILITY_PROTECTED:
          export_class = EXPORT_PROTECTED;
          break;
        case GS_VISIBILITY_HIDDEN:
          export_class = EXPORT_HIDDEN;
          break;
        case GS_VISIBILITY_INTERNAL:
          export_class = EXPORT_INTERNAL;
          break;
        default:
          GS_ASSERT(0, "unknown decl visibility");
          break;
      }
      Set_ST_export (*st, export_class);
    }

#ifndef TARG_NVISA
    gs_tls_model_kind_t tlsk = 
      (gs_tls_model_kind_t) gs_decl_tls_model(decl_node);
    enum ST_TLS_MODEL tls_model = TLS_NONE;
    switch (tlsk) {
      case GS_TLS_MODEL_GLOBAL_DYNAMIC:
        tls_model = TLS_GLOBAL_DYNAMIC;
        break;
      case GS_TLS_MODEL_LOCAL_DYNAMIC:
        tls_model = TLS_LOCAL_DYNAMIC;
        break;
      case GS_TLS_MODEL_INITIAL_EXEC:
        tls_model = TLS_INITIAL_EXEC;
        break;
      case GS_TLS_MODEL_LOCAL_EXEC:
        tls_model = TLS_LOCAL_EXEC;
        break;
    }
    Set_ST_tls_model(st, tls_model);

    if ( tls_stress_test ) {
      // once tls_stress_test is set, make PSTATIC/FSTATIC variables to be TLS
      if ( ST_sclass(st) == SCLASS_PSTATIC || ST_sclass(st) == SCLASS_FSTATIC ) {
        Set_ST_is_thread_local(st);
        if ( tls_stress_model == TLS_NONE ) {
          tls_stress_model = ( gen_pic_code ) ? TLS_GLOBAL_DYNAMIC : TLS_LOCAL_EXEC;
        }
        Set_ST_tls_model(st, tls_stress_model);
      }
    }
#endif // !TARG_NVISA
  }

  if(Debug_Level >= 2) {
    // Bug 559
    if (ST_sclass(st) != SCLASS_EXTERN) {
      // Add DSTs for all types seen so far.
      add_deferred_DST_types();

      DST_INFO_IDX dst = Create_DST_decl_For_Tree(decl_node,st);
      DECL_DST_IDX(decl_node) = dst;
    }
  }

#ifdef KEY
  // Bug 11352: For C++, expand any initializations decl_node may have,
  // after the ST is fully formed. The decl may be in gxx_emitted_decls
  // list, and WGEN_Process_Var_Decl may have decided not to expand it.
  // Now that we are here, we are sure to need any initialization it
  // may have.
  if (lang_cplus && gs_tree_code(decl_node) == GS_VAR_DECL &&
      !expanded_decl(decl_node))
    WGEN_Expand_Decl(decl_node, TRUE);
#endif

  return st;
}

#include <ext/hash_map>

namespace {

  using __gnu_cxx::hash_map;

  struct ptrhash {
    size_t operator()(void* p) const { return reinterpret_cast<size_t>(p); }
  };

  hash_map<gs_t, TY_IDX,     ptrhash>     ty_idx_map;
  hash_map<gs_t, ST*,        ptrhash>     st_map;
  hash_map<gs_t, SYMTAB_IDX, ptrhash>     symtab_idx_map;
  hash_map<gs_t, LABEL_IDX,  ptrhash>     label_idx_map;
  hash_map<gs_t, ST*,        ptrhash>     string_st_map;
  hash_map<gs_t, BOOL,       ptrhash>     bool_map;
  hash_map<gs_t, INT32,      ptrhash>     field_id_map;
  hash_map<gs_t, INT32,	  ptrhash>     type_field_ids_used_map;
  hash_map<gs_t, INT32,      ptrhash>     scope_number_map;
  hash_map<gs_t, gs_t,       ptrhash>     label_scope_map;
  hash_map<gs_t, DST_INFO_IDX,ptrhash>    decl_idx_map; 
  hash_map<gs_t, DST_INFO_IDX,ptrhash>    decl_field_idx_map; 
  hash_map<gs_t, DST_INFO_IDX,ptrhash>    decl_specification_idx_map; 
  hash_map<gs_t, DST_INFO_IDX,ptrhash>    type_idx_map;
  hash_map<gs_t, LABEL_IDX,  ptrhash>     handler_label_map;
  hash_map<gs_t, DST_INFO_IDX,ptrhash>    abstract_root_map;
#ifdef KEY
  // Map PU to the PU-specific st_map.
  hash_map<PU*, hash_map<gs_t, ST*, ptrhash>*, ptrhash>     pu_map;
  // TRUE if ST is a decl that is being/already been expanded.
  hash_map<gs_t, BOOL,        ptrhash>     expanded_decl_map;
  // TRUE if TREE is a DECL_FUNCTION whose PU should have PU_uplevel set.
  hash_map<gs_t, BOOL,        ptrhash>     func_PU_uplevel_map;
  hash_map<gs_t, gs_t,	      ptrhash>	   parent_scope_map;
  // Record whether a symbol referenced in a cleanup should be marked weak as a
  // workaround to the fact that kg++fe may emit cleanups that g++ won't emit
  // because g++ knows that are not needed.  The linker will complain if these
  // symbols are not defined.
  hash_map<ST*, INT32,        ptrhash>     weak_workaround_map;
#endif
  hash_map<gs_t, ST*,	      ptrhash>	   decl_st2_map;
}

TY_IDX& TYPE_TY_IDX(gs_t t)         { return ty_idx_map[t]; }

BOOL& expanded_decl(gs_t t) {
  FmtAssert (t, ("func_expanded: not a decl"));
  return expanded_decl_map[t];
}

// Put ST in a map based on the tree node T and the current PU.
void
set_DECL_ST(gs_t t, ST* st) {

  // Find the tree node to use as index into st_map.
  gs_t t_index;
  if (gs_tree_code(t) == GS_VAR_DECL &&
      (gs_decl_context(t) == 0 || 
       gs_tree_code(gs_decl_context(t)) == GS_NAMESPACE_DECL) &&
     gs_decl_name (t) && gs_decl_assembler_name(t))
    t_index = gs_decl_assembler_name(t);
  else
    t_index = t;

  // If ST is 1, then the caller only wants to pretend that there is a symbol
  // for T.  Later on, the caller will reset the ST to NULL and assign a real
  // symbol to T.
  if (st == (ST *) 1) {
    st_map[t_index] = st;
    return;
  }

  // If ST is a symbol that should be shared across functions, then put ST in
  // the st_map, which maps T directly to ST.  Otherwise, put ST in the
  // PU-specific st_map.
  //
  // It is observed that g++ uses the same tree for different functions, such
  // as inline functions.  As a result, we cannot attach PU-specific ST's 
  // directly to the tree nodes.
  //
  // If Current_scope is 0, then the symbol table has not been initialized, and
  // we are being called by WFE_Add_Weak to handle a weak symbol.  In that
  // case, use the non-PU-specific st_map.
  if (Current_scope != 0 &&
      (gs_tree_code(t) == GS_PARM_DECL ||
       (gs_tree_code(t) == GS_VAR_DECL &&
        (ST_sclass(st) == SCLASS_AUTO ||
         (! pstatic_as_global &&
	  ST_sclass(st) == SCLASS_PSTATIC))))) {
    // ST is PU-specific.  Use pu_map[pu] to get the PU-specific st_map, then
    // use st_map[t] to get the ST for the tree node t.
    //
    // We can access pu_map[pu] only if Scope_tab[Current_scope].st is valid
    // because we need to get the current PU, but Get_Current_PU requires a
    // valid Scope_tab[Current_scope].st.  If Scope_tab[Current_scope].st is
    // not set, then this means the caller is trying to create the ST for the
    // function symbol.
    if (Scope_tab[Current_scope].st != NULL) {
      // ok to call Get_Current_PU.
      PU *pu = &Get_Current_PU();
      hash_map<PU*, hash_map<gs_t, ST*, ptrhash>*, ptrhash>::iterator it =
	pu_map.find(pu);
      if (it == pu_map.end()) {
	// Create new PU-specific map.
	pu_map[pu] = new hash_map<gs_t, ST*, ptrhash>;
      }
      // Put the ST in the PU-specific st_map.
      (*(pu_map[pu]))[t_index] = st;
    }
  } else {
#ifdef Is_True_On
    if (st_map[t_index]) {
      // The st_map is already set.  This is ok only for weak ST.
      FmtAssert (ST_is_weak_symbol(st_map[t_index]),
		 ("set_DECL_ST: st_map already set"));
    }
#endif
    // Put the ST in the non-PU-specific st_map.
    st_map[t_index] = st;
  }
}

// Get ST associated with the tree node T.
ST*&
get_DECL_ST(gs_t t) {
  static ST *null_ST = (ST *) NULL;

  // Find the tree node to use as index into st_map.
  gs_t t_index;
  if (gs_tree_code(t) == GS_VAR_DECL &&
      (gs_decl_context(t) == 0 || 
       gs_tree_code(gs_decl_context(t)) == GS_NAMESPACE_DECL) &&
     gs_decl_name (t) && gs_decl_assembler_name(t))
    t_index = gs_decl_assembler_name(t);
  else
    t_index = t;

  // If Current_scope is 0, then the symbol table has not been initialized, and
  // we are being called by WFE_Add_Weak to handle a weak symbol.  Use the
  // non-PU-specific st_map.
  if (Current_scope == 0)
    return st_map[t_index];

  // See if the ST is in the non-PU-specific st_map.
  if (st_map[t_index]) {
    return st_map[t_index];
  }

  // The ST is not in the non-PU-specific map.  Look in the PU-specific maps.
  INT scope = Current_scope;
  do {
    // If Scope_tab[scope].st is NULL, then the function ST has not
    // been set yet, and there is no PU-specific map.
    if (Scope_tab[scope].st != NULL) {
      // See if there is a PU-specific map.
      PU *pu = &Get_Scope_PU(scope);
      hash_map<PU*, hash_map<gs_t, ST*, ptrhash>*, ptrhash>::iterator pu_map_it =
	pu_map.find(pu);
      if (pu_map_it != pu_map.end()) {
	// There is a PU-specific map.  Get the ST from the map.
	hash_map<gs_t, ST*, ptrhash> *st_map2 = pu_map[pu];
	if ((*st_map2)[t_index])
	  return (*st_map2)[t_index];
      }
    }
    scope--;
  } while (scope > 1);
  return null_ST;
}

BOOL&
func_PU_uplevel(gs_t t) {
  FmtAssert (gs_tree_code(t) == GS_FUNCTION_DECL,
	     ("func_PU_uplevel: not a FUNCTION_DECL tree node"));
  return func_PU_uplevel_map[t];
}

INT32& WEAK_WORKAROUND(ST *st)         { return weak_workaround_map[st]; }

SYMTAB_IDX& DECL_SYMTAB_IDX(gs_t t) { return symtab_idx_map[t]; }
LABEL_IDX& DECL_LABEL_IDX(gs_t t)   { return label_idx_map[t]; }
ST*& TREE_STRING_ST(gs_t t)         { return string_st_map[t]; }
BOOL& DECL_LABEL_DEFINED(gs_t t)    { return bool_map[t]; }
INT32& DECL_FIELD_ID(gs_t t)        { return field_id_map[t]; }
INT32 & TYPE_FIELD_IDS_USED(gs_t t) { return type_field_ids_used_map[t]; }
INT32 & SCOPE_NUMBER(gs_t t)        { return scope_number_map[t]; }
#ifdef KEY
gs_t & PARENT_SCOPE(gs_t t)	    { return parent_scope_map[t]; }
#endif
gs_t & LABEL_SCOPE(gs_t t)	    { return label_scope_map[t]; }
ST* & DECL_ST2(gs_t t)		    { return decl_st2_map[t]; }

// This is for normal declarations.

// We do not know if the DST entry is filled in.
// So check and ensure a real entry exists.

DST_INFO_IDX & DECL_DST_IDX(gs_t t) 
{ 
	hash_map<gs_t, DST_INFO_IDX,ptrhash>::iterator it =
		decl_idx_map.find(t);
	if(it == decl_idx_map.end()) {
		// substitute for lack of default constructor
		DST_INFO_IDX dsti = DST_INVALID_IDX;
		decl_idx_map[t] = dsti;
	}
	return decl_idx_map[t]; 
}
// This is for static class members and member functions.
// We need a distinct DST record for a single ST.
// Note that only the main record actually need be linked
// to ST as only that one gets an address/location.

// We do not know if the DST entry is filled in.
// So check and ensure a real entry exists.
DST_INFO_IDX & DECL_DST_SPECIFICATION_IDX(gs_t t) 
{ 
	hash_map<gs_t, DST_INFO_IDX,ptrhash>::iterator it =
		decl_specification_idx_map.find(t);
	if(it == decl_specification_idx_map.end()) {
		// substitute for lack of default constructor
		DST_INFO_IDX dsti = DST_INVALID_IDX;
		decl_specification_idx_map[t] = dsti;
	}
	return decl_specification_idx_map[t]; 
}

// This is for static class members and member functions.
// We need a distinct DST record for a single ST.
// Note that only the main record actually need be linked
// to ST as only that one gets an address/location.

// We do not know if the DST entry is filled in.
// So check and ensure a real entry exists.
DST_INFO_IDX & DECL_DST_FIELD_IDX(gs_t t) 
{ 
	hash_map<gs_t, DST_INFO_IDX,ptrhash>::iterator it =
		decl_field_idx_map.find(t);
	if(it == decl_idx_map.end()) {
		// substitute for lack of default constructor
		DST_INFO_IDX dsti = DST_INVALID_IDX;
		decl_field_idx_map[t] = dsti;
	}
	return decl_field_idx_map[t]; 
}

// We do not know if the DST entry is filled in.
// So check and ensure a real entry exists.
DST_INFO_IDX & TYPE_DST_IDX(gs_t t) 
{
	hash_map<gs_t, DST_INFO_IDX,ptrhash>::iterator it =
		type_idx_map.find(t);
	if(it == type_idx_map.end()) {
		// substitute for lack of default constructor
		DST_INFO_IDX dsti = DST_INVALID_IDX;
		type_idx_map[t] = dsti;
	}
	return type_idx_map[t]; 
}

// We do not know if the DST entry is filled in.
// So check and ensure a real entry exists.
DST_INFO_IDX & DECL_DST_ABSTRACT_ROOT_IDX(gs_t t) 
{
	hash_map<gs_t, DST_INFO_IDX,ptrhash>::iterator it =
		abstract_root_map.find(t);
	if(it == abstract_root_map.end()) {
		// substitute for lack of default constructor
		DST_INFO_IDX dsti = DST_INVALID_IDX;
		abstract_root_map[t] = dsti;
	}
	return abstract_root_map[t]; 
}


LABEL_IDX& HANDLER_LABEL(gs_t t)    { return handler_label_map[t]; }
