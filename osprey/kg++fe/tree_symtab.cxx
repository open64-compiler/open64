/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
* Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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

#include <values.h>
#include "defs.h"
#include "errors.h"
extern "C" {
#include "gnu_config.h"
}
#ifdef KEY	// get HW_WIDE_INT for flags.h
#include "gnu/hwint.h"
#endif	/* KEY */
extern "C" {
#include "gnu/flags.h"
#include "gnu/system.h"
#include "gnu/tree.h"
#include "cp-tree.h"
}
#undef TARGET_PENTIUM // hack around macro definition in gnu
#if defined(TARG_PPC32)
#undef TARGET_POWERPC
#endif /* TARG_PPC32 */
#include "symtab.h"
#include "strtab.h"
#include "wn.h"
#include "wfe_expr.h"
#include "wfe_decl.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "ir_reader.h"
#include "tree_symtab.h"
#ifdef KEY
#include "wfe_stmt.h"
#include <map>
#endif
#include "tree_cmp.h"

#include <ext/hash_map>
using __gnu_cxx::hash_map;
typedef struct {
    size_t operator()(void* p) const { return reinterpret_cast<size_t>(p); }
} void_ptr_hash;

extern INT pstatic_as_global;

extern FILE *tree_dump_file; /* for debugging */
extern void Push_Deferred_Function(tree);

#ifdef KEY
extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID, ST *);

// Map duplicate gcc nodes that refer to the same function.
std::multimap<tree, tree> duplicate_of;
void
add_duplicates (tree newdecl, tree olddecl)
{
	duplicate_of.insert (pair<tree, tree>(newdecl, olddecl));
	duplicate_of.insert (pair<tree, tree>(olddecl, newdecl));
}

// Remove all references to DECL from the map.
void
erase_duplicates (tree decl)
{
  int i, j;
  int count = duplicate_of.count (decl);

  for (i=0; i<count; i++) {
    std::multimap<tree, tree>::iterator iter = duplicate_of.find(decl);
    tree t = (*iter).second;

    // Erase entries with DECL as the data, i.e., <..., DECL>.
    int count2 = duplicate_of.count(t); 
    for (j=0; j<count2; j++) {
      std::multimap<tree, tree>::iterator iter2 = duplicate_of.find(t);
      tree t2 = (*iter2).second;
      if (t2 == decl) {
	duplicate_of.erase (iter2);
      }
    }

    // Erase entry with DECL as the key, i.e., <DECL, ...>.
    duplicate_of.erase (iter);
  }
}

static ST*
get_duplicate_st (tree decl)
{
  int count = duplicate_of.count (decl);

  for (int i=0; i<count; ++i) {
    std::multimap<tree, tree>::iterator iter = duplicate_of.find(decl);
    tree t = (*iter).second;
    // The node t could have been garbage-collected by gcc.  This is a crude
    // test to see if t is still valid.
    if (TREE_CODE(t) == FUNCTION_DECL &&
	DECL_NAME(t) == DECL_NAME(decl) &&
	DECL_ASSEMBLER_NAME_SET_P(t) == DECL_ASSEMBLER_NAME_SET_P(decl) &&
	(!DECL_ASSEMBLER_NAME_SET_P(t) ||
	 DECL_ASSEMBLER_NAME(t) == DECL_ASSEMBLER_NAME(decl))) {
      // Return the ST previously allocated, if any.
      ST *st = DECL_ST(t);
      if (st != NULL)
        return st;
    }
    duplicate_of.erase (iter);
  }
  return NULL;
}
#endif

static char*
Get_Name (tree node)
{
	static UINT anon_num = 0;
	static char buf[64];

	if (node == NULL) {
		++anon_num;
		sprintf(buf, ".anonymous.%d", anon_num);
		return buf;
	}
	else if (TREE_CODE (node) == IDENTIFIER_NODE)
		return ((char *) IDENTIFIER_POINTER (node));
	else if (TREE_CODE (node) == TYPE_DECL)
		// If type has a typedef-name, the TYPE_NAME is a TYPE_DECL.
		return ((char *) IDENTIFIER_POINTER (DECL_NAME (node)));
	else
		FmtAssert(FALSE, ("Get_Name unexpected tree"));
		return NULL;
}
#ifdef TARG_SL
/* this function only return signed mtype and the function MTYPE_complement will do 
 * conversion from signed type to unsigned type if current type is unsigned type */
TYPE_ID
Get_Mtype_For_Integer_Type(tree type_tree, INT64 tsize) 
{
     TYPE_ID mtype;
     switch(tsize) {
       case 1:
       /*
       	 if(TYPE_VBUF_P(type_tree)) {
       		if(TYPE_VBUF1(type_tree))  {
		      mtype = MTYPE_VBUF1;
            }
            else if(TYPE_VBUF2(type_tree))
	        {
		      mtype = MTYPE_VBUF2;
	        }
            else if(TYPE_VBUF4(type_tree))
	        {
		      mtype = MTYPE_VBUF4;
	        }
       	 }
	     else if(TYPE_SBUF(type_tree)) {
	          mtype = MTYPE_SB1;
	     }
	     else if(TYPE_SDRAM(type_tree)){
	          mtype = MTYPE_SD1;
	     }
	     else {
	         mtype = MTYPE_I1;
	     }
	     */
/* we will use above code when mtype is ready */
	     mtype = MTYPE_I1;
	     break;
	  case 2:
	/*
	    if(TYPE_SBUF(type_tree)) {
	          mtype = MTYPE_SB2;
	    }
	    else if(TYPE_SDRAM(type_tree)) {
	          mtype = MTYPE_SD2;
	    }
	    else {
	         mtype = MTYPE_I2;
	    }
	 */
	    mtype = MTYPE_I2;
	    break;
	  case 4: 
 /*
	     if(TYPE_SBUF(type_tree)) {
	          mtype = MTYPE_SB4;
	     }
	     else if(TYPE_SDRAM(type_tree)){
	          mtype = MTYPE_SD4;
	     }
	     else {
	         mtype = MTYPE_I4;
	     }
*/
         mtype = MTYPE_I4;
	     break;
	  case 8:
	     DevWarn("8 byte types being used");
	     mtype = MTYPE_I8;
	  /*
	     if(TYPE_SBUF(type_tree)) {
	          mtype = MTYPE_SB8;
	     }
	     else if(TYPE_SDRAM(type_tree)){
	          mtype = MTYPE_SD8;
	     }
	     else {
	         mtype = MTYPE_I8;
	     }
	  */ 
	     break;
    }
    return mtype;
}
#endif 

static void
dump_field(tree field)
{
  printf("%s:  ", Get_Name(DECL_NAME(field)));
  printf("%d\n", DECL_FIELD_ID(field));
}


tree
next_real_or_virtual_field (tree type_tree, tree field)
{
  static bool real_field = true;
#ifdef KEY
  static tree prev_field = NULL_TREE;

  // If FIELD is not the same as the previously returned field, then we are
  // being called to traverse a new list or to traverse the same list over
  // again.  In either case, begin with the real fields.
  if (field != prev_field)
    real_field = true;

  if (field == TYPE_VFIELD(type_tree))
    real_field = false;

  if (TREE_CHAIN(field))
    return (prev_field = TREE_CHAIN(field));

  if (real_field && TYPE_VFIELD(type_tree)) {
    real_field = false;
    return (prev_field = TYPE_VFIELD(type_tree));
  }

  real_field = true;
  return (prev_field = NULL_TREE);

#else

  if (field == TYPE_VFIELD(type_tree))
    real_field = false;

  if (TREE_CHAIN(field))
    return TREE_CHAIN(field);

  if (real_field && TYPE_VFIELD(type_tree)) {
    real_field = false;
    return TYPE_VFIELD(type_tree);
  }

  real_field = true;
  return NULL_TREE;
#endif	// KEY
}

static void
Do_Base_Types (tree type_tree)
{
  tree binfo = TYPE_BINFO(type_tree);
  tree basetypes = binfo ? BINFO_BASETYPES(binfo) : 0;
  INT32 i;
  if (basetypes)
    for (i = 0; i < TREE_VEC_LENGTH(basetypes); ++i)
      (void) Get_TY (BINFO_TYPE(TREE_VEC_ELT(basetypes, i)));
}

size_t 
Roundup (size_t offset, int alignment)
{
  return (offset % alignment) ? offset + alignment - offset % alignment
			      : offset;
}

size_t
Type_Size_Without_Vbases (tree type_tree)
{
  tree field;
  tree last_field_decl = 0;

  for (field = TYPE_FIELDS(type_tree);
       field;
       field = next_real_or_virtual_field (type_tree, field)) {
    if (TREE_CODE(field) == FIELD_DECL)
      last_field_decl = field;
  }

  if (last_field_decl == 0)
    return 0;

  return
    Get_Integer_Value (DECL_FIELD_OFFSET(last_field_decl))  		     +
    Get_Integer_Value (DECL_FIELD_BIT_OFFSET(last_field_decl)) / BITSPERBYTE +
    Get_Integer_Value (DECL_SIZE(last_field_decl)) / BITSPERBYTE;
} 

bool
is_empty_base_class (tree type_tree)
{
  tree field = TYPE_FIELDS(type_tree);
  return TREE_CODE(field) == TYPE_DECL && TREE_CHAIN(field) == 0;
}

// idx is non-zero only for RECORD and UNION, when there is forward declaration
extern TY_IDX
Create_TY_For_Tree (tree type_tree, TY_IDX idx)
{

	if(TREE_CODE(type_tree) == ERROR_MARK)
	   return idx;

	TY_IDX orig_idx = idx;
	if(TREE_CODE_CLASS(TREE_CODE(type_tree)) != 't') {
	  DevWarn("Bad tree class passed to Create_TY_For_Tree %c",
		TREE_CODE_CLASS(TREE_CODE(type_tree)));
          return idx;
	}

#ifdef KEY
        UINT align = TYPE_ALIGN(type_tree) / BITSPERBYTE;
#endif
	// for typedefs get the information from the base type
	if (TYPE_NAME(type_tree) &&
	    idx == 0 &&
	    (TREE_CODE(type_tree) == RECORD_TYPE ||
	     TREE_CODE(type_tree) == UNION_TYPE) &&
	    TREE_CODE(TYPE_NAME(type_tree)) == TYPE_DECL &&
	    TYPE_MAIN_VARIANT(type_tree) != type_tree) {
		idx = Get_TY (TYPE_MAIN_VARIANT(type_tree));
		if (TYPE_READONLY(type_tree))
			Set_TY_is_const (idx);
		if (TYPE_VOLATILE(type_tree))
			Set_TY_is_volatile (idx);
#ifdef KEY
		if (TYPE_RESTRICT(type_tree))
			Set_TY_is_restrict (idx);
                Set_TY_align (idx, align); // bug 10533
#endif
		TYPE_TY_IDX(type_tree) = idx;
		if(Debug_Level >= 2) {
		  DST_INFO_IDX dst = Create_DST_type_For_Tree(type_tree,
			idx,orig_idx);
		  TYPE_DST_IDX(type_tree) = dst;
	        }
		TYPE_FIELD_IDS_USED(type_tree) =
			TYPE_FIELD_IDS_USED(TYPE_MAIN_VARIANT(type_tree));
		return idx;
	}

	TYPE_ID mtype;
	INT64 tsize;
	BOOL variable_size = FALSE;
	tree type_size = TYPE_SIZE(type_tree);
#ifndef KEY
	UINT align = TYPE_ALIGN(type_tree) / BITSPERBYTE;
#endif
	if (type_size == NULL) {
		// incomplete structs have 0 size.  Similarly, 'void' is
                // an incomplete type that can never be completed.
		FmtAssert(TREE_CODE(type_tree) == ARRAY_TYPE 
			|| TREE_CODE(type_tree) == ENUMERAL_TYPE
			|| TREE_CODE(type_tree) == UNION_TYPE
			|| TREE_CODE(type_tree) == RECORD_TYPE
			|| TREE_CODE(type_tree) == LANG_TYPE
			|| TREE_CODE(type_tree) == VOID_TYPE,
			  ("Create_TY_For_Tree: type_size NULL for non ARRAY/RECORD/VOID, type is %d",
                           (int) TREE_CODE(type_tree)));
		tsize = 0;
	}
	else {
		if (TREE_CODE(type_size) != INTEGER_CST) {
			if (TREE_CODE(type_tree) == ARRAY_TYPE)
				DevWarn ("Encountered VLA at line %d", lineno);
			else
				Fail_FmtAssertion ("VLA at line %d not currently implemented", lineno);
			variable_size = TRUE;
			tsize = 0;
		}
		else
#ifdef KEY		// bug 3045
			tsize = (Get_Integer_Value(type_size) + BITSPERBYTE - 1)
				  / BITSPERBYTE;
#else
			tsize = Get_Integer_Value(type_size) / BITSPERBYTE;
#endif
	}
	switch (TREE_CODE(type_tree)) {
	case VOID_TYPE:
	case LANG_TYPE: // unknown type
		idx = MTYPE_To_TY (MTYPE_V);	// use predefined type
		break;
	case BOOLEAN_TYPE:
	case INTEGER_TYPE:
		switch (tsize) {
		case 1: 
#ifdef TARG_SL		
		  mtype = Get_Mtype_For_Integer_Type(type_tree, tsize); 
#else  
		  mtype = MTYPE_I1;
#endif 
		  break;
		case 2: 
#ifdef TARG_SL 
         mtype = Get_Mtype_For_Integer_Type(type_tree, tsize); 
#else 
		 mtype = MTYPE_I2; 
#endif 
		 break;

		case 4: 
#ifdef TARG_SL 
        mtype = Get_Mtype_For_Integer_Type(type_tree, tsize); 
#else 
        mtype = MTYPE_I4; 
#endif 
                 break;

		case 8: 
#ifdef TARG_SL 
		  mtype = Get_Mtype_For_Integer_Type(type_tree, tsize); 
#else 
		  mtype = MTYPE_I8; 
#endif 
		  break;

#if !defined(TARG_X8664) && !defined(TARG_MIPS) && !defined(TARG_IA64) && !defined(TARG_LOONGSON) || defined(TARG_SL) 
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
		if (TREE_UNSIGNED(type_tree)) {
			mtype = MTYPE_complement(mtype);
		}
#ifdef KEY
                if (lookup_attribute ("may_alias", TYPE_ATTRIBUTES (type_tree)))
                {
                  // bug 9975: Handle may_alias attribute, we need to cr        eate
                  // a new type to which we can attach the flag.
                  TY &ty = New_TY (idx);
                  TY_Init (ty, tsize, KIND_SCALAR, mtype,
                           Save_Str(Get_Name(TYPE_NAME(type_tree))) );
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
	case CHAR_TYPE:
		mtype = (TREE_UNSIGNED(type_tree) ? MTYPE_U1 : MTYPE_I1);
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case ENUMERAL_TYPE:
		mtype = (TREE_UNSIGNED(type_tree) ? MTYPE_U4 : MTYPE_I4);
#ifdef KEY
		/* bug#500 */
		if( tsize == 8 ){
		  mtype = (TREE_UNSIGNED(type_tree) ? MTYPE_U8 : MTYPE_I8);
		}
#endif
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case REAL_TYPE:
		switch (tsize) {
		case 4:  mtype = MTYPE_F4; break;
		case 8:  mtype = MTYPE_F8; break;
#if defined(TARG_IA64)
                case 12:
                case 16: mtype = MTYPE_F10; break;
#elif defined(TARG_MIPS) || defined(TARG_IA32) || defined(TARG_X8664) || defined(TARG_LOONGSON)
                case 12:
                case 16: mtype = MTYPE_FQ; break;
#else
                case 16: mtype = MTYPE_F16; break;
#endif
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case COMPLEX_TYPE:
		switch (tsize) {
		case  8:  mtype = MTYPE_C4; break;
		case 16:  mtype = MTYPE_C8; break;
#if defined(TARG_IA32) || defined(TARG_X8664)
		case 24:  mtype = MTYPE_CQ; break;
#endif
#if defined(TARG_IA64)
                case 32: mtype = MTYPE_C10; break;
#else
                case 32: mtype = MTYPE_CQ; break;
#endif
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case POINTER_TYPE:
		if (TYPE_PTRMEM_P(type_tree)) {
			// pointer to member
			idx = Be_Type_Tbl(Pointer_Size == 8 ? MTYPE_I8 : MTYPE_I4);
			break;
		}
		/* FALLTHRU */
	case REFERENCE_TYPE:
		idx = Make_Pointer_Type (Get_TY (TREE_TYPE(type_tree)));
		Set_TY_align (idx, align);
		break;
	case ARRAY_TYPE:
		{	// new scope for local vars
		TY &ty = New_TY (idx);
		TY_Init (ty, tsize, KIND_ARRAY, MTYPE_M, 
			Save_Str(Get_Name(TYPE_NAME(type_tree))) );
		Set_TY_etype (ty, Get_TY (TREE_TYPE(type_tree)));
		Set_TY_align (idx, TY_align(TY_etype(ty)));
                if (TYPE_ANONYMOUS_P(type_tree) || TYPE_NAME(type_tree) == NULL)
                    Set_TY_anonymous(ty);
		// assumes 1 dimension
		// nested arrays are treated as arrays of arrays
		ARB_HANDLE arb = New_ARB ();
		ARB_Init (arb, 0, 0, 0);
		Set_TY_arb (ty, arb);
		Set_ARB_first_dimen (arb);
		Set_ARB_last_dimen (arb);
		Set_ARB_dimension (arb, 1);
		if (TYPE_SIZE(TREE_TYPE(type_tree)) == 0)
			break; // anomaly:  type will never be needed
		if (TREE_CODE(TYPE_SIZE(TREE_TYPE(type_tree))) == INTEGER_CST) {
			Set_ARB_const_stride (arb);
			Set_ARB_stride_val (arb, 
				Get_Integer_Value (TYPE_SIZE_UNIT(TREE_TYPE(type_tree))));
		}
		else {
			WN *swn;
			swn = WFE_Expand_Expr (TYPE_SIZE_UNIT(TREE_TYPE(type_tree)));
			
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
			  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
			  WFE_Set_ST_Addr_Saved (swn);
			  swn = WN_Stid (mtype, 0, st, ty_idx, swn);
			  WFE_Stmt_Append (swn, Get_Srcpos());
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
			WFE_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_stride (arb);
			Set_ARB_stride_var (arb, (ST_IDX) ST_st_idx (st));
		}
		Set_ARB_const_lbnd (arb);
		Set_ARB_lbnd_val (arb, 0);
		if (type_size) {
#ifdef KEY
		    // For Zero-length arrays, TYPE_MAX_VALUE tree is NULL
		    if (!TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree))) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0xffffffff);
		    } else
#endif /* KEY */
		    if (TREE_CODE(TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree))) ==
			INTEGER_CST) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, Get_Integer_Value (
				TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree)) ));
		    }
		    else {
			WN *uwn = WFE_Expand_Expr (TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree)) );
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
#ifdef KEY
			  	WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
				wn = WN_Stid (TY_mtype (ty_idx), 0, st, ty_idx, uwn);
				WFE_Stmt_Append (wn, Get_Srcpos());
			}
			else {
				st = WN_st (uwn);
				ty_idx = ST_type (st);
			}
			wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND, (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WFE_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_var (arb, ST_st_idx (st));
		    }
		}
		else {
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0);
		}
		if (variable_size) {
			WN *swn, *wn;

			swn = WFE_Expand_Expr (TYPE_SIZE_UNIT(type_tree));

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
				  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
				  WFE_Set_ST_Addr_Saved (swn);
				  swn = WN_Stid (mtype, 0, st, ty_idx, swn);
				  WFE_Stmt_Append (swn, Get_Srcpos());
				  swn = WN_Ldid (mtype, 0, st, ty_idx);
				}
#endif /* KEY */
				FmtAssert (WN_operator (swn) == OPR_LDID,
					("size operator for VLA not LDID"));
				ST *st = WN_st (swn);
				TY_IDX ty_idx = ST_type (st);
				TYPE_ID mtype = TY_mtype (ty_idx);

				wn = WN_Stid (mtype, 0, st, ty_idx, swn);
				WFE_Stmt_Append (wn, Get_Srcpos());
			}
		}
		} // end array scope
		break;
	case RECORD_TYPE:
	case UNION_TYPE:
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
		    TYPE_LANG_SPECIFIC(type_tree) &&
		    // check if it has its base version set
		    CLASSTYPE_AS_BASE(type_tree) &&
		    CLASSTYPE_SIZE(type_tree) &&
		    is_empty_class(type_tree))
			tsize = 0;
#endif	// KEY

                // For typedef A B, tree A and tree B link to the same TY C
                // When parsing A, C's name is set to A
                // When parsing B, C's name is set to B
                // It makes C's name be random in different object files so that TY merge will fail
                // So the name of this TY must be fixed to the main variant name.
                if (TYPE_MAIN_VARIANT(type_tree) != type_tree)
                    TY_Init(ty, tsize, KIND_STRUCT, MTYPE_M,
                            Save_Str(Get_Name(TYPE_NAME(TYPE_MAIN_VARIANT(type_tree)))));
                else
                    TY_Init (ty, tsize, KIND_STRUCT, MTYPE_M,
                            Save_Str(Get_Name(TYPE_NAME(type_tree))) );
                if (TYPE_ANONYMOUS_P(type_tree) || TYPE_NAME(type_tree) == NULL)
                    Set_TY_anonymous(ty);
                if (TREE_CODE(type_tree) == UNION_TYPE) {
                        Set_TY_is_union(idx);
                }

#ifdef KEY
		if (aggregate_value_p(type_tree)) {
			Set_TY_return_in_mem(idx);
		}
#endif
		if (align == 0) align = 1;	// in case incomplete type
		Set_TY_align (idx, align);
		// set idx now in case recurse thru fields
		TYPE_TY_IDX(type_tree) = idx;
		Do_Base_Types (type_tree);

		// Process nested structs and static data members first

                for (tree field =  TYPE_FIELDS (type_tree);
                          field;
                          field = next_real_or_virtual_field(type_tree, field))
                        if (TREE_CODE(field) == TYPE_DECL ||
			    TREE_CODE(field) == FIELD_DECL) {
                                tree field_type = TREE_TYPE(field);
				if ((TREE_CODE(field_type) == RECORD_TYPE ||
				     TREE_CODE(field_type) == UNION_TYPE) &&
                                    field_type != type_tree) {
#ifdef KEY
					// Defer typedefs within class
					// declarations to avoid circular
					// declaration dependences.  See
					// example in bug 5134.
                                        if (TREE_CODE(field) == TYPE_DECL)
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
			else if (TREE_CODE(field) == VAR_DECL)
				defer_decl(field);
#else
			else if (TREE_CODE(field) == VAR_DECL)
				WFE_Expand_Decl(field);
#endif
			else if (TREE_CODE(field) == TEMPLATE_DECL)
				WFE_Expand_Decl(field);


  		Set_TY_fld (ty, FLD_HANDLE());
		FLD_IDX first_field_idx = Fld_Table.Size ();
		tree field;
		tree method = TYPE_METHODS(type_tree);
		FLD_HANDLE fld;
		INT32 next_field_id = 1;
		hash_map <tree, tree, void_ptr_hash> anonymous_base;

		// Generate an anonymous field for every direct, nonempty,
		// nonvirtual base class.  

		INT32 offset = 0;
		INT32 anonymous_fields = 0;
#ifndef KEY     // g++'s class.c already laid out the base types.  Bug 11622.
		if (TYPE_BINFO(type_tree) &&
		    BINFO_BASETYPES(TYPE_BINFO(type_tree))) {
		  tree basetypes = BINFO_BASETYPES(TYPE_BINFO(type_tree));
		  INT32 i;
		  for (i = 0; i < TREE_VEC_LENGTH(basetypes); ++i) {
		    tree binfo = TREE_VEC_ELT(basetypes, i);
		    tree basetype = BINFO_TYPE(binfo);
		    offset = Roundup (offset,
				    TYPE_ALIGN(basetype) / BITSPERBYTE);
		    if (!is_empty_base_class(basetype) || 
			!TREE_VIA_VIRTUAL(binfo)) {
		      ++next_field_id;
		      ++anonymous_fields;
		      next_field_id += TYPE_FIELD_IDS_USED(basetype);
		      fld = New_FLD();
		      FLD_Init (fld, Save_Str(Get_Name(0)), 
				Get_TY(basetype), offset);
		      offset += Type_Size_Without_Vbases (basetype);
                      // For the field with base class type,
                      // set it to anonymous and base class,
                      // and add it into anonymous_base set
                      // to avoid create the anonymous field twice in the TY
                      Set_FLD_is_anonymous(fld);
                      Set_FLD_is_base_class(fld);
                      anonymous_base.insert(CLASSTYPE_AS_BASE(basetype));
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
#endif

                // find all base classes
                if (TYPE_BINFO(type_tree) && BINFO_BASETYPES(TYPE_BINFO(type_tree))) {
                  tree basetypes = BINFO_BASETYPES(TYPE_BINFO(type_tree));
                  INT32 i;
                  for (i = 0; i < TREE_VEC_LENGTH(basetypes); ++i) {
                    tree basetype = BINFO_TYPE(TREE_VEC_ELT(basetypes, i));
                    anonymous_base[CLASSTYPE_AS_BASE(basetype)] = basetype;
                  }
                }

		for (field = TYPE_FIELDS(type_tree); 
			field;
			field = next_real_or_virtual_field(type_tree, field) )
		{
			if (TREE_CODE(field) == TYPE_DECL) {
				continue;
			}
			if (TREE_CODE(field) == CONST_DECL) {
                // Just for the time being, we need to investigate
                // whether this criteria is reasonable.
                static BOOL once_is_enough=FALSE;
                if (!once_is_enough) {
                                  DevWarn ("got CONST_DECL in field list");
                  once_is_enough=TRUE;
                }
				continue;
			}
			if (TREE_CODE(field) == VAR_DECL) {
				continue;	
			}
			if (TREE_CODE(field) == TEMPLATE_DECL) {
				continue;
			}
			DECL_FIELD_ID(field) = next_field_id;
			next_field_id += 
			  TYPE_FIELD_IDS_USED(TREE_TYPE(field)) + 1;
			fld = New_FLD ();
			FLD_Init (fld, Save_Str(Get_Name(DECL_NAME(field))), 
				0, // type
				Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
				Get_Integer_Value(DECL_FIELD_BIT_OFFSET(field))
					/ BITSPERBYTE);
                        if (DECL_NAME(field) == NULL)
                          Set_FLD_is_anonymous(fld);
                        if (anonymous_base.find(TREE_TYPE(field)) != anonymous_base.end()) {
                          Set_FLD_is_base_class(fld);
                          // set the base class type;
                          tree base_class = anonymous_base[TREE_TYPE(field)];
                          Set_FLD_type(fld, Get_TY(base_class));
                        }
		}

		TYPE_FIELD_IDS_USED(type_tree) = next_field_id - 1;
  		FLD_IDX last_field_idx = Fld_Table.Size () - 1;
		if (last_field_idx >= first_field_idx) {
			Set_TY_fld (ty, FLD_HANDLE (first_field_idx));
			Set_FLD_last_field (FLD_HANDLE (last_field_idx));
		}
		// now set the fld types.
		// first skip the anonymous fields, whose types are already
		// set.
		fld = TY_fld(ty);
		while (anonymous_fields--)
		  fld = FLD_next(fld);

		for (field = TYPE_FIELDS(type_tree);
		     /* ugly hack follows; traversing the fields isn't
                        the same from run-to-run. fwa? */
			field && fld.Entry();
			field = next_real_or_virtual_field(type_tree, field))
		{
#ifdef KEY
			const  int FLD_BIT_FIELD_SIZE   = 64;
#endif
			if (TREE_CODE(field) == TYPE_DECL)
				continue;
			if (TREE_CODE(field) == CONST_DECL)
				continue;
			if (TREE_CODE(field) == VAR_DECL)
				continue;
			if (TREE_CODE(field) == TEMPLATE_DECL)
				continue;
#ifdef KEY
			// Don't expand the field's type if it's a pointer
			// type, in order to avoid circular dependences
			// involving member object types and base types.  See
			// example in bug 4954.  
			if (TREE_CODE(TREE_TYPE(field)) == POINTER_TYPE) {
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
                        if (anonymous_base.find(TREE_TYPE(field)) == anonymous_base.end()) {
                          TY_IDX fty_idx = Get_TY(TREE_TYPE(field));

                          if ((TY_align (fty_idx) > align) || (TY_is_packed (fty_idx)))
                            Set_TY_is_packed (ty);
                          Set_FLD_type(fld, fty_idx);
                        }

			if ( ! DECL_BIT_FIELD(field)
				&& Get_Integer_Value(DECL_SIZE(field)) > 0
#ifdef KEY
// We don't handle bit-fields > 64 bits. For an INT field of 128 bits, we
// make it 64 bits. But then don't set it as FLD_IS_BIT_FIELD.
				&& Get_Integer_Value(DECL_SIZE(field)) <= 
				   FLD_BIT_FIELD_SIZE
				// bug 2401
				&& TY_size(Get_TY(TREE_TYPE(field))) != 0
#endif
				&& Get_Integer_Value(DECL_SIZE(field))
				  != (TY_size(Get_TY(TREE_TYPE(field))) 
					* BITSPERBYTE) )
			{
#ifdef KEY
			        FmtAssert( Get_Integer_Value(DECL_SIZE(field)) <=
					   FLD_BIT_FIELD_SIZE,
					   ("field size too big") );
#endif
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because 
				// otherwise the field type is wrong.
				DevWarn("field size %lld doesn't match type size %lld", 
					Get_Integer_Value(DECL_SIZE(field)),
					TY_size(Get_TY(TREE_TYPE(field)))
						* BITSPERBYTE );
				DECL_BIT_FIELD(field) = 1;
			}
			if (DECL_BIT_FIELD(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld, 
					Get_Integer_Value(
						DECL_FIELD_BIT_OFFSET(field))
						% BITSPERBYTE);
				Set_FLD_bsize (fld, Get_Integer_Value(
                                                           DECL_SIZE(field)));
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
		if (!Enable_WFE_DFE) {
		if (cp_type_quals(type_tree) == TYPE_UNQUALIFIED) {
			while (method != NULL_TREE) {
				WFE_Expand_Decl (method);
				method = TREE_CHAIN(method);
			}
		}
		}
#endif	// KEY
		} //end record scope
		break;
	case METHOD_TYPE:
		//DevWarn ("Encountered METHOD_TYPE at line %d", lineno);
	case FUNCTION_TYPE:
		{	// new scope for local vars
		tree arg;
		INT32 num_args;
		TY &ty = New_TY (idx);
		TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0); 
		Set_TY_align (idx, 1);
		TY_IDX ret_ty_idx;
		TY_IDX arg_ty_idx;
		TYLIST tylist_idx;

		// allocate TYs for return as well as parameters
		// this is needed to avoid mixing TYLISTs if one
		// of the parameters is a pointer to a function

		ret_ty_idx = Get_TY(TREE_TYPE(type_tree));
		for (arg = TYPE_ARG_TYPES(type_tree);
		     arg;
		     arg = TREE_CHAIN(arg))
			arg_ty_idx = Get_TY(TREE_VALUE(arg));

		// if return type is pointer to a zero length struct
		// convert it to void
		if (!WFE_Keep_Zero_Length_Structs    &&
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
		for (num_args = 0, arg = TYPE_ARG_TYPES(type_tree);
		     arg;
		     num_args++, arg = TREE_CHAIN(arg))
		{
			arg_ty_idx = Get_TY(TREE_VALUE(arg));
			if (!WFE_Keep_Zero_Length_Structs    &&
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
		} // end FUNCTION_TYPE scope
		break;
#ifdef TARG_X8664
        // x86 gcc vector types
        case VECTOR_TYPE:
                {
                  switch (GET_MODE_SIZE (TYPE_MODE (type_tree)))
                  {
                    case 8:
                      switch (GET_MODE_UNIT_SIZE (TYPE_MODE (type_tree)))
                      {
                        case 1:
                          idx = MTYPE_To_TY (MTYPE_M8I1);
                          break;
                        case 2:
                          idx = MTYPE_To_TY (MTYPE_M8I2);
                          break;
                        case 4:
                          if (TREE_CODE (TREE_TYPE (type_tree)) == INTEGER_TYPE)
                            idx = MTYPE_To_TY (MTYPE_V8I4);
                          else
                            idx = MTYPE_To_TY (MTYPE_M8F4);
                          break;
                        default: Fail_FmtAssertion ("Get_TY: NYI");
                      }
                      break;
                    case 16:
                      switch (GET_MODE_UNIT_SIZE (TYPE_MODE (type_tree)))
                      {
                        case 1:
                          idx = MTYPE_To_TY (MTYPE_V16I1);
                          break;
                        case 2:
                          idx = MTYPE_To_TY (MTYPE_V16I2);
                          break;
                        case 4:
                          if (TREE_CODE (TREE_TYPE (type_tree)) == INTEGER_TYPE)
                            idx = MTYPE_To_TY (MTYPE_V16I4);
                          else
                            idx = MTYPE_To_TY (MTYPE_V16F4);
                          break;
                        case 8:
                          if (TREE_CODE (TREE_TYPE (type_tree)) == INTEGER_TYPE)
                            idx = MTYPE_To_TY (MTYPE_V16I8);
                          else
                            idx = MTYPE_To_TY (MTYPE_V16F8);
                          break;
                        default: Fail_FmtAssertion ("Get_TY: NYI");
                      }
                      break;
                    default:
                      Fail_FmtAssertion ("Get_TY: Unexpected vector type");
                  }
                }
                break;
#endif // TARG_X8664
	default:
		FmtAssert(FALSE, ("Get_TY unexpected tree_type"));
	}
	if (TYPE_READONLY(type_tree))
		Set_TY_is_const (idx);
	if (TYPE_VOLATILE(type_tree))
		Set_TY_is_volatile (idx);
#ifdef KEY
	if (TYPE_RESTRICT(type_tree))
		Set_TY_is_restrict (idx);
#endif
	TYPE_TY_IDX(type_tree) = idx;
        if(Debug_Level >= 2) {
#ifdef KEY
	  // DSTs for records were entered into the defer list in the order
	  // that the records are declared, in order to preserve their scope.
	  // Bug 4168.
	  if (TREE_CODE(type_tree) != RECORD_TYPE &&
	      TREE_CODE(type_tree) != UNION_TYPE) {
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

ST*
Create_ST_For_Tree (tree decl_node)
{
  TY_IDX     ty_idx;
  ST*        st;
  char      *name;
  char	    tempname[32];
  ST_SCLASS  sclass;
  ST_EXPORT  eclass;
  SYMTAB_IDX level;
  static INT anon_count = 0;
#ifdef KEY
  BOOL anon_st = FALSE;
#endif

  if(TREE_CODE(decl_node) == ERROR_MARK) {
        Fail_FmtAssertion ("Unable to handle ERROR_MARK. internal error");
  }

#ifdef KEY
  // If the decl is a function decl, and there are duplicate decls for the
  // function, then use a ST already allocated for the function, if such ST
  // exists.
  if (TREE_CODE (decl_node) == FUNCTION_DECL) {
    st = get_duplicate_st (decl_node);
    if (st) {
      set_DECL_ST(decl_node, st);
      return st;
    }
  }
#endif

#ifdef KEY
  // For variables with asm register assignments, don't use the assembler
  // names because they are of the form "%rbx".
  if (TREE_CODE(decl_node) == VAR_DECL &&
      DECL_ASMREG(decl_node) != 0) {
    FmtAssert (DECL_NAME (decl_node),
	       ("Create_ST_For_Tree: DECL_NAME null"));
    name = (char *) IDENTIFIER_POINTER (DECL_NAME (decl_node));
  } else
#endif
  if (DECL_NAME (decl_node) && DECL_ASSEMBLER_NAME (decl_node))
    name = (char *) IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node));
  else {
    sprintf(tempname, "anon%d", ++anon_count);
    name = tempname;
#ifdef KEY
    anon_st = TRUE;
#endif
  }

#ifdef KEY
  BOOL guard_var = FALSE;
  // See if variable is a guard variable.
  if (strncmp("_ZGV", name, 4) == 0) {
    guard_var = TRUE;
  }
#endif

  switch (TREE_CODE(decl_node)) {

    case FUNCTION_DECL:
      {
        if (Enable_WFE_DFE) {
          tree body = DECL_SAVED_TREE(decl_node);
          if (DECL_THUNK_P(decl_node) &&
              TREE_CODE(CP_DECL_CONTEXT(decl_node)) != NAMESPACE_DECL)
            Push_Deferred_Function (decl_node);
/*
          else
          if (DECL_TINFO_FN_P(decl_node))
            Push_Deferred_Function (decl_node);
*/
          else
          if (body != NULL_TREE && !DECL_EXTERNAL(decl_node) &&
              (DECL_TEMPLATE_INFO(decl_node) == NULL              ||
               DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION(decl_node) ||
               DECL_TEMPLATE_INSTANTIATED(decl_node)              ||
               DECL_TEMPLATE_SPECIALIZATION(decl_node))) {
            Push_Deferred_Function (decl_node);
          }
        }

        TY_IDX func_ty_idx = Get_TY(TREE_TYPE(decl_node));

        sclass = SCLASS_EXTERN;
        eclass = TREE_PUBLIC(decl_node) || DECL_WEAK(decl_node) ?
		   EXPORT_PREEMPTIBLE				:
		   EXPORT_LOCAL;
        level  = GLOBAL_SYMTAB+1;

        PU_IDX pu_idx;
        PU&    pu = New_PU (pu_idx);

        PU_Init (pu, func_ty_idx, level);

        st = New_ST (GLOBAL_SYMTAB);

#ifdef KEY	// Fix bug # 34, 3356
	char *p = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node));
	if (*p == '*')
	  p++;
        ST_Init (st, Save_Str(p),
                 CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));

        // St is a constructor
        if (DECL_CONSTRUCTOR_P(decl_node) && !DECL_COPY_CONSTRUCTOR_P(decl_node))
            Set_PU_is_constructor(pu);
        // St is a pure virual function
        if (DECL_PURE_VIRTUAL_P(decl_node) || strncmp(p, "__cxa_pure_virtual", 18) == 0)
            Set_ST_is_pure_vfunc(st);

        p = IDENTIFIER_POINTER (DECL_NAME (decl_node));
        if (!strncmp(p,"operator",8))
                Set_PU_is_operator(pu);
#else
        ST_Init (st,
                 Save_Str ( IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node))),
                 CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));
#endif
        if (TREE_CODE(TREE_TYPE(decl_node)) == METHOD_TYPE) {
                Set_ST_is_method_func(st);
                TY_IDX base = Get_TY(TYPE_METHOD_BASETYPE(TREE_TYPE(decl_node)));
                Set_PU_base_class(pu, base);
        }

	if (DECL_THUNK_P(decl_node) &&
            TREE_CODE(CP_DECL_CONTEXT(decl_node)) != NAMESPACE_DECL)
	  Set_ST_is_weak_symbol(st);
      }
      break;

    case PARM_DECL:
    case VAR_DECL:
#ifdef KEY
    case RESULT_DECL:	// bug 3878
#endif
      {
        if (TREE_CODE(decl_node) == PARM_DECL) {
          sclass = SCLASS_FORMAL;
          eclass = EXPORT_LOCAL;
          level = CURRENT_SYMTAB;
        }
        else {
          if (DECL_CONTEXT (decl_node) == 0 			     ||
	      TREE_CODE (DECL_CONTEXT (decl_node)) == NAMESPACE_DECL ||
 	      TREE_CODE (DECL_CONTEXT (decl_node)) == RECORD_TYPE ) {
            if (TREE_PUBLIC (decl_node)) {
#ifdef KEY
	      // GCC 3.2
	      if (DECL_EXTERNAL(decl_node) ||
		  (DECL_LANG_SPECIFIC(decl_node) &&
		   DECL_REALLY_EXTERN(decl_node)))
#else
	      if (DECL_EXTERNAL(decl_node))
#endif /* KEY */
		sclass = SCLASS_EXTERN;
	      else
	      if (DECL_INITIAL(decl_node))
		sclass = SCLASS_UGLOBAL;
	      else if (TREE_STATIC(decl_node)) {
#ifdef KEY
// bugs 340, 3717
		if (flag_no_common || !DECL_COMMON (decl_node))
#else
		if (flag_no_common)
#endif
		  sclass = SCLASS_UGLOBAL;
		else
		  sclass = SCLASS_COMMON;
	      }
	      else
              	sclass = SCLASS_EXTERN;
#ifdef TARG_IA64
              // bug fix for OSP_89 && OSP_173 && OSP_169
              if (!flag_pic) {
                if (Use_Call_Shared_Link && Gp_Rel_Aggresive_Opt &&
                    sclass != SCLASS_EXTERN &&  sclass != SCLASS_COMMON)
                  eclass = EXPORT_PROTECTED;
                else
                  eclass = EXPORT_PREEMPTIBLE;
              }
              else {
                eclass = EXPORT_PREEMPTIBLE;
              }
            }
#else
              eclass = EXPORT_PREEMPTIBLE;
            }
#endif
            else {
              	sclass = SCLASS_FSTATIC;
		eclass = EXPORT_LOCAL;
            }
            level = GLOBAL_SYMTAB;
          }
          else {
            if (DECL_EXTERNAL(decl_node) || DECL_WEAK (decl_node)) {
	      sclass = SCLASS_EXTERN;
	      level  = GLOBAL_SYMTAB;
              eclass = EXPORT_PREEMPTIBLE;
            }
#ifdef KEY
	    // Bug 8652: If GNU marks it as COMMON, we should the same.
            else if (!flag_no_common && TREE_STATIC (decl_node) &&
                     DECL_COMMON (decl_node) &&
                     TREE_PUBLIC (decl_node)) {
	      sclass = SCLASS_COMMON;
	      level = GLOBAL_SYMTAB;
	      eclass = EXPORT_PREEMPTIBLE;
	    }
#endif
            else {
	      if (TREE_STATIC (decl_node)) {
		sclass = SCLASS_PSTATIC;
		if (pstatic_as_global)
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
#ifdef KEY
	// Make g++ guard variables global in order to make them weak.  Ideally
	// guard variables should be "common", but for some reason the back-end
	// currently can't handle C++ commons.  As a work around, make the
	// guard variables weak.  Since symtab_verify.cxx don't like weak
	// locals, make the guard variables global.
	if (guard_var) {
	  level = GLOBAL_SYMTAB;
	  sclass = SCLASS_UGLOBAL;
	  eclass = EXPORT_PREEMPTIBLE;
	}

	// The tree under TREE_TYPE(decl_node) could reference also decl_node.
	// If that's the case, the Get_TY would create the ST for decl_node.
	// As a result, call GET_TY first, then check if the ST is already
	// created, and create ST only if it isn't created.
        ty_idx = Get_TY (TREE_TYPE(decl_node));
	st = DECL_ST(decl_node);
	if (st)
	  return st;
        st = New_ST (level);
#else
        st = New_ST (level);
        ty_idx = Get_TY (TREE_TYPE(decl_node));
#endif
        if (TY_kind (ty_idx) == KIND_ARRAY &&
            TREE_STATIC (decl_node) &&
            DECL_INITIAL (decl_node) == FALSE &&
            TY_size (ty_idx) == 0) {
          Set_TY_size (ty_idx, TY_size (Get_TY (TREE_TYPE (TREE_TYPE (decl_node)))));
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
	if (TREE_READONLY(decl_node))
		Set_TY_is_const (ty_idx);
	if (TREE_THIS_VOLATILE(decl_node))
		Set_TY_is_volatile (ty_idx);
#ifdef KEY
        // Handle aligned attribute (bug 7331)
        if (DECL_USER_ALIGN (decl_node))
          Set_TY_align (ty_idx, DECL_ALIGN_UNIT (decl_node));
        // NOTE: we do not update the ty_idx value in the TYPE_TREE. So
        // if any of the above properties are set, the next time we get into
        // Get_ST, the ty_idx in the TYPE_TREE != ty_idx in st. The solution
        // is either to update TYPE_TREE now, or compare the ty_idx_index
        // in Get_ST (instead of ty_idx). Currently we do the latter
#endif // KEY
        ST_Init (st, Save_Str(name), CLASS_VAR, sclass, eclass, ty_idx);
#ifdef KEY
	if (TREE_CODE (decl_node) == VAR_DECL && DECL_THREADPRIVATE (decl_node))
	  Set_ST_is_thread_private (st);
	if (TREE_CODE (decl_node) == VAR_DECL && anon_st)
	  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
        if (TREE_CODE (decl_node) == VAR_DECL && DECL_THREAD_LOCAL (decl_node))
          Set_ST_is_thread_local (st);

        if (DECL_SIZE_UNIT (decl_node) &&
            TREE_CODE (DECL_SIZE_UNIT (decl_node)) != INTEGER_CST)
        {
            // if this is the first alloca, save sp.
            int idx;
            if (!Set_Current_Scope_Has_Alloca (idx))
            {
              ST * save_st = WFE_Alloca_0 ();
              Set_Current_Scope_Alloca_St (save_st, idx);
            }
            WN * size = WFE_Expand_Expr (DECL_SIZE_UNIT (decl_node));
            // mimic WFE_Alloca_ST
            ST * alloca_st = New_ST (CURRENT_SYMTAB);
            ST_Init (alloca_st, Save_Str (name),
                       CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
                                  Make_Pointer_Type (ty_idx, FALSE));
            Set_ST_is_temp_var (alloca_st);
            Set_ST_pt_to_unique_mem (alloca_st);
            Set_ST_base_idx (st, ST_st_idx (alloca_st));
            WN *wn  = WN_CreateAlloca (size);
            wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
            WFE_Stmt_Append (wn, Get_Srcpos());
            Set_PU_has_alloca (Get_Current_PU());
	    // For kids 1..n of DEALLOCA
            Add_Current_Scope_Alloca_St (alloca_st, idx);
        }
#endif // KEY
        if (TREE_CODE(decl_node) == PARM_DECL) {
		Set_ST_is_value_parm(st);
        }
      }
      break;

    default:
      {
        Fail_FmtAssertion ("Create_ST_For_Tree: unexpected tree type");
      }
      break;
  }

#ifdef KEY
  set_DECL_ST(decl_node, st);
#else
  DECL_ST(decl_node) = st;
#endif

#ifdef KEY
// For an anonymous union, all the members must be expanded together (cf.
// expand_anon_union_decl() in GNU), the members will actually just point 
// to the ST for the union itself, so that whenever we access a member, 
// we access the same variable.
//
// We may need to do the following for anonymous aggregates also
// (ANON_AGGR_TYPE_P), for the time being let us support anon unions.
  if (TREE_CODE (decl_node) == VAR_DECL &&
      TYPE_LANG_SPECIFIC (TREE_TYPE (decl_node)) &&
      ANON_UNION_TYPE_P (TREE_TYPE (decl_node)))
  {
      tree members = DECL_ANON_UNION_ELEMS (decl_node);
      for (tree t = members; t; t = TREE_CHAIN (t))
      {
      	tree var = TREE_VALUE (t);
	FmtAssert (TREE_CODE (var) == VAR_DECL, 
		   ("Unexpected member type in anonymous union"));
	set_DECL_ST (var, st);
      }
  }

  // If VAR_DECL has a non-zero DECL_ASMREG, then DECL_ASMREG-1 is the register
  // number assigned by an "asm".
  if (TREE_CODE(decl_node) == VAR_DECL &&
      DECL_ASMREG(decl_node) != 0) {
    extern PREG_NUM Map_Reg_To_Preg []; // defined in common/com/arch/config_targ.cxx
    int reg = DECL_ASMREG(decl_node) - 1;
    PREG_NUM preg = Map_Reg_To_Preg [reg];
    FmtAssert (preg >= 0,
               ("mapping register %d to preg failed\n", reg));
    TY_IDX ty_idx = ST_type (st);
    Set_TY_is_volatile (ty_idx);
    Set_ST_type (st, ty_idx);
    Set_ST_assigned_to_dedicated_preg (st);
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR&    st_attr = New_ST_ATTR (CURRENT_SYMTAB, st_attr_idx);
    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_DEDICATED_REGISTER, preg);
  }
#endif

  if (TREE_CODE(decl_node) == VAR_DECL &&
      DECL_CONTEXT(decl_node)	       &&
      TREE_CODE(DECL_CONTEXT(decl_node)) == RECORD_TYPE)
	Get_TY(DECL_CONTEXT(decl_node));

  if (Enable_WFE_DFE) {
    if (TREE_CODE(decl_node) == VAR_DECL &&
        level == GLOBAL_SYMTAB &&
        !DECL_EXTERNAL (decl_node) &&
        DECL_INITIAL (decl_node)) {
      Push_Deferred_Function (decl_node);
    }
  }

  if (DECL_WEAK      (decl_node) &&
      (!DECL_EXTERNAL (decl_node)
#ifdef KEY
       // Make weak symbols for:
       //   extern "C" int bar() __attribute__ ((weak, alias("foo")))
       // Bug 3841.
       || DECL_ALIAS_TARGET(decl_node))
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
        !(TREE_CODE(decl_node) == FUNCTION_DECL &&
          DECL_BUILT_IN(decl_node))) {
      Set_ST_is_weak_symbol (st);
      WEAK_WORKAROUND(st) = WEAK_WORKAROUND_made_weak;
    }
  }
  // See comment above about guard variables.
  else if (guard_var) {
    Set_ST_is_weak_symbol (st);
    Set_ST_init_value_zero (st);
    Set_ST_is_initialized (st);
  }
#endif

  if (DECL_SECTION_NAME (decl_node)) {
    if (strncmp(TREE_STRING_POINTER (DECL_SECTION_NAME (decl_node)),
                ".gnu.linkonce.",
                14) != 0 ) {
      // OSP, only handle non-.gnu.linkonce.* section name
      DevWarn ("section %s specified for %s",
               TREE_STRING_POINTER (DECL_SECTION_NAME (decl_node)),
               ST_name (st));
      if (TREE_CODE (decl_node) == FUNCTION_DECL)
        level = GLOBAL_SYMTAB;
      ST_ATTR_IDX st_attr_idx;
      ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
      ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_SECTION_NAME,
                    Save_Str (TREE_STRING_POINTER (DECL_SECTION_NAME (decl_node))));
      Set_ST_has_named_section (st);
    }
    else {
      // OSP. Ignore .gnu.linkonce.* section name
      DevWarn ("Ignore %s specified for %s",
               TREE_STRING_POINTER (DECL_SECTION_NAME (decl_node)),
               ST_name (st));
    }
  }

/*
  if (DECL_SYSCALL_LINKAGE (decl_node)) {
	Set_PU_has_syscall_linkage (Pu_Table [ST_pu(st)]);
  }
*/
#if defined(TARG_SL)
  if(DECL_SL_MODEL_NAME(decl_node)) {
    if(TREE_CODE(decl_node) == VAR_DECL && 
       TREE_CODE(DECL_SL_MODEL_NAME(decl_node)) == STRING_CST) 
    {
      if(!strcmp(TREE_STRING_POINTER(DECL_SL_MODEL_NAME(decl_node)), "small"))
        Set_ST_gprel(st); 
      else if(!strcmp(TREE_STRING_POINTER(DECL_SL_MODEL_NAME(decl_node)), "large"))
        Set_ST_not_gprel(st); 
      else 
        Fail_FmtAssertion("incorrect model type for sl data model"); 
    }
  }
#endif 
  if(Debug_Level >= 2) {
#ifdef KEY
    // Bug 559
    if (ST_sclass(st) != SCLASS_EXTERN) {
      // Add DSTs for all types seen so far.
      add_deferred_DST_types();

      DST_INFO_IDX dst = Create_DST_decl_For_Tree(decl_node,st);
      DECL_DST_IDX(decl_node) = dst;
    }
#else
    DST_INFO_IDX dst = Create_DST_decl_For_Tree(decl_node,st);
    DECL_DST_IDX(decl_node) = dst;
#endif
  }
  /* NOTES:
 * Following code is temporarily used since mtype isn't 
 * ready for now. After mtype handling has been finished we will
 * use new normal method to handle section assignment. */

/* Description:
 * Set ST Flags for variant internal buffer type 
 * VBUF is only to be file scope variable and gp-relative
 * SBUF need to decide if SBUF is explicitly declared. If 
 * declared the flag Set_ST_in_sbuf need to be set to indicate 
 * the variable will be processed by CP2. */
#ifdef TARG_SL 
	const char* section_name;
	int has_assigned_section = 0;
	if(DECL_VBUF(decl_node)) // || DECL_SBUF(decl_node))
	{
           if(DECL_V1BUF(decl_node) && TREE_CODE(decl_node) != FUNCTION_DECL 
            && !POINTER_TYPE_P(TREE_TYPE(decl_node))) 
          { 
             Set_ST_in_v1buf(st);
             Set_ST_gprel(st);
          }	 
          else if(DECL_V2BUF(decl_node) && TREE_CODE(decl_node) != FUNCTION_DECL 
            && !POINTER_TYPE_P(TREE_TYPE(decl_node)))
          {
             Set_ST_in_v2buf(st);
       	     Set_ST_gprel(st);
             TY_IDX st_ty_idx=ST_type(st);
             Set_TY_size (st_ty_idx, TY_size(st_ty_idx)*2);
          }
          else if(DECL_V4BUF(decl_node) && TREE_CODE(decl_node) != FUNCTION_DECL 
            && !POINTER_TYPE_P(TREE_TYPE(decl_node))) 
          {
             Set_ST_in_v4buf(st);       
             Set_ST_gprel(st);
             TY_IDX st_ty_idx=ST_type(st);
             Set_TY_size (st_ty_idx, TY_size(st_ty_idx)*4);
          }
	}
	else if(DECL_SBUF(decl_node) && TREE_CODE(decl_node) != FUNCTION_DECL 
		&& !POINTER_TYPE_P(TREE_TYPE(decl_node))) {
	  if(TREE_CODE(TREE_TYPE(decl_node)) == ARRAY_TYPE)
	  {
	     tree element_type = TREE_TYPE(decl_node); 
	     while(TREE_CODE(element_type) == ARRAY_TYPE)
	       element_type = TREE_TYPE(element_type); 

	     if(!POINTER_TYPE_P(element_type))
	     {
                Set_ST_in_sbuf(st);
 	        Set_ST_gprel(st); 
	     }
	  }
	  else 
	  {
	     Set_ST_in_sbuf(st);
 	     Set_ST_gprel(st); 
	  }
	}
	else if(DECL_SDRAM(decl_node) && TREE_CODE(decl_node) != FUNCTION_DECL 
          && !POINTER_TYPE_P(TREE_TYPE(decl_node))) {
              Set_ST_in_sdram(st);
	}
#endif  // TARG_SL

  return st;
}

#ifndef EXTRA_WORD_IN_TREE_NODES

#include <ext/hash_map>

namespace {

  using __gnu_cxx::hash_map;

  struct ptrhash {
    size_t operator()(void* p) const { return reinterpret_cast<size_t>(p); }
  };

  hash_map<tree, TY_IDX,     ptrhash>     ty_idx_map;
  hash_map<tree, ST*,        ptrhash>     st_map;
  hash_map<tree, SYMTAB_IDX, ptrhash>     symtab_idx_map;
  hash_map<tree, LABEL_IDX,  ptrhash>     label_idx_map;
  hash_map<tree, ST*,        ptrhash>     string_st_map;
  hash_map<tree, BOOL,       ptrhash>     bool_map;
  hash_map<tree, INT32,      ptrhash>     field_id_map;
  hash_map<tree, INT32,	  ptrhash>     type_field_ids_used_map;
  hash_map<tree, INT32,      ptrhash>     scope_number_map;
  hash_map<tree, tree,       ptrhash>     label_scope_map;
  hash_map<tree, DST_INFO_IDX,ptrhash>    decl_idx_map; 
  hash_map<tree, DST_INFO_IDX,ptrhash>    decl_field_idx_map; 
  hash_map<tree, DST_INFO_IDX,ptrhash>    decl_specification_idx_map; 
  hash_map<tree, DST_INFO_IDX,ptrhash>    type_idx_map;
  hash_map<tree, LABEL_IDX,  ptrhash>     handler_label_map;
  hash_map<tree, DST_INFO_IDX,ptrhash>    abstract_root_map;
#ifdef KEY
  // Map PU to the PU-specific st_map.
  hash_map<PU*, hash_map<tree, ST*, ptrhash>*, ptrhash>     pu_map;
  // TRUE if ST is a decl that is being/already been expanded.
  hash_map<tree, BOOL,        ptrhash>     expanded_decl_map;
  // TRUE if TREE is a DECL_FUNCTION whose PU should have PU_uplevel set.
  hash_map<tree, BOOL,        ptrhash>     func_PU_uplevel_map;
  hash_map<tree, tree,	      ptrhash>	   parent_scope_map;
  // Record whether a symbol referenced in a cleanup should be marked weak as a
  // workaround to the fact that kg++fe may emit cleanups that g++ won't emit
  // because g++ knows that are not needed.  The linker will complain if these
  // symbols are not defined.
  hash_map<ST*, INT32,        ptrhash>     weak_workaround_map;
#endif
}

TY_IDX& TYPE_TY_IDX(tree t)         { return ty_idx_map[t]; }

#ifdef KEY
BOOL& expanded_decl(tree t) {
  FmtAssert (DECL_CHECK(t), ("func_expanded: not a decl"));
  return expanded_decl_map[t];
}

// Put ST in a map based on the tree node T and the current PU.
void
set_DECL_ST(tree t, ST* st) {

  // Find the tree node to use as index into st_map.
  tree t_index;
  if (TREE_CODE(t) == VAR_DECL 			     &&
      (DECL_CONTEXT(t) == 0 || 
       TREE_CODE(DECL_CONTEXT(t)) == NAMESPACE_DECL) &&
     DECL_NAME (t) && DECL_ASSEMBLER_NAME(t))
    t_index = DECL_ASSEMBLER_NAME(t);
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
      (TREE_CODE(t) == PARM_DECL ||
       (TREE_CODE(t) == VAR_DECL &&
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
      hash_map<PU*, hash_map<tree, ST*, ptrhash>*, ptrhash>::iterator it =
	pu_map.find(pu);
      if (it == pu_map.end()) {
	// Create new PU-specific map.
	pu_map[pu] = new hash_map<tree, ST*, ptrhash>;
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
get_DECL_ST(tree t) {
  static ST *null_ST = (ST *) NULL;

  // Find the tree node to use as index into st_map.
  tree t_index;
  if (TREE_CODE(t) == VAR_DECL 			     &&
      (DECL_CONTEXT(t) == 0 || 
       TREE_CODE(DECL_CONTEXT(t)) == NAMESPACE_DECL) &&
     DECL_NAME (t) && DECL_ASSEMBLER_NAME(t))
    t_index = DECL_ASSEMBLER_NAME(t);
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

  // The ST is not in the non-PU-specific map.  Look in the PU-specific map.

  // If Scope_tab[Current_scope].st is NULL, then the function ST has not
  // been set yet, and there is no PU-specific map.
  if (Scope_tab[Current_scope].st == NULL)
    return null_ST;

  // See if there is a PU-specific map.
  PU *pu = &Get_Current_PU();	// needs Scope_tab[Current_scope].st
  hash_map<PU*, hash_map<tree, ST*, ptrhash>*, ptrhash>::iterator pu_map_it =
    pu_map.find(pu);
  if (pu_map_it == pu_map.end())
    return null_ST;

  // There is a PU-specific map.  Get the ST from the map.
  hash_map<tree, ST*, ptrhash> *st_map = pu_map[pu];
  return (*st_map)[t_index];
}

BOOL&
func_PU_uplevel(tree t) {
  FmtAssert (TREE_CODE(t) == FUNCTION_DECL,
	     ("func_PU_uplevel: not a FUNCTION_DECL tree node"));
  return func_PU_uplevel_map[t];
}

INT32& WEAK_WORKAROUND(ST *st)         { return weak_workaround_map[st]; }
#else

ST*& DECL_ST(tree t) {
  if (TREE_CODE(t) == VAR_DECL 			     &&
      (DECL_CONTEXT(t) == 0 || 
       TREE_CODE(DECL_CONTEXT(t)) == NAMESPACE_DECL) &&
     DECL_NAME (t) && DECL_ASSEMBLER_NAME(t))
    return st_map[DECL_ASSEMBLER_NAME(t)];
  else
    return st_map[t];
  }
#endif

SYMTAB_IDX& DECL_SYMTAB_IDX(tree t) { return symtab_idx_map[t]; }
LABEL_IDX& DECL_LABEL_IDX(tree t)   { return label_idx_map[t]; }
ST*& TREE_STRING_ST(tree t)         { return string_st_map[t]; }
BOOL& DECL_LABEL_DEFINED(tree t)    { return bool_map[t]; }
INT32& DECL_FIELD_ID(tree t)        { return field_id_map[t]; }
INT32 & TYPE_FIELD_IDS_USED(tree t) { return type_field_ids_used_map[t]; }
INT32 & SCOPE_NUMBER(tree t)        { return scope_number_map[t]; }
#ifdef KEY
tree & PARENT_SCOPE(tree t)	    { return parent_scope_map[t]; }
#endif
tree & LABEL_SCOPE(tree t)	    { return label_scope_map[t]; }

// This is for normal declarations.

// We do not know if the DST entry is filled in.
// So check and ensure a real entry exists.

DST_INFO_IDX & DECL_DST_IDX(tree t) 
{ 
	hash_map<tree, DST_INFO_IDX,ptrhash>::iterator it =
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
DST_INFO_IDX & DECL_DST_SPECIFICATION_IDX(tree t) 
{ 
	hash_map<tree, DST_INFO_IDX,ptrhash>::iterator it =
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
DST_INFO_IDX & DECL_DST_FIELD_IDX(tree t) 
{ 
	hash_map<tree, DST_INFO_IDX,ptrhash>::iterator it =
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
DST_INFO_IDX & TYPE_DST_IDX(tree t) 
{
	hash_map<tree, DST_INFO_IDX,ptrhash>::iterator it =
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
DST_INFO_IDX & DECL_DST_ABSTRACT_ROOT_IDX(tree t) 
{
	hash_map<tree, DST_INFO_IDX,ptrhash>::iterator it =
		abstract_root_map.find(t);
	if(it == abstract_root_map.end()) {
		// substitute for lack of default constructor
		DST_INFO_IDX dsti = DST_INVALID_IDX;
		abstract_root_map[t] = dsti;
	}
	return abstract_root_map[t]; 
}


LABEL_IDX& HANDLER_LABEL(tree t)    { return handler_label_map[t]; }


#endif
