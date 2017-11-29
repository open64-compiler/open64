/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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

#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "errors.h"
#include "gnu_config.h"
#ifdef KEY
// To get HW_WIDE_INT ifor flags.h */
#include "gnu/hwint.h"
#endif /* KEY */
#include "gnu/flags.h"
extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
#include "gnu/toplev.h"
}
#if defined(TARG_IA32) || defined(TARG_X8664)
// the definition in gnu/config/i386/i386.h causes problem
// with the enumeration in common/com/ia32/config_targ.h
#undef TARGET_PENTIUM
#endif /* TARG_IA32 */
#if defined(TARG_PPC32)
// the definition in gnu/config/ppc32/rs6000.h causes problem
// with the enumeration in common/com/ppc32/config_targ.h
#undef TARGET_POWERPC
#endif /* TARG_PPC32 */

#ifdef KEY 
#ifdef TARG_MIPS
// ABI_N32 is defined in config/MIPS/mips.h and conflicts with 
// common/com/MIPS/config_targ.h
#undef ABI_N32
#endif /* TARG_MIPS */
#endif /* KEY */

#include "symtab.h"
#include "strtab.h"
#include "tree_symtab.h"
#include "wn.h"
#include "wfe_expr.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "ir_reader.h"
#include <cmplrs/rcodes.h>
#ifdef KEY
#include "erfe.h"
#endif
#include "config_asm.h"

extern FILE *tree_dump_file; // For debugging only

extern INT pstatic_as_global;

static char*
Get_Name (tree node)
{
	static UINT anon_num = 0;
	static char buf[256];

	if (node != NULL) {
	    if (TREE_CODE (node) == IDENTIFIER_NODE)
		return IDENTIFIER_POINTER (node);
	    else if (TREE_CODE (node) == TYPE_DECL)
		// If type has a typedef-name, the TYPE_NAME is a TYPE_DECL.
		return IDENTIFIER_POINTER (DECL_NAME (node));
  	    else if (DECL_NAME (node)) {
		// e.g. var_decl or function_decl
		return IDENTIFIER_POINTER (DECL_NAME (node));
	    }
	}
	// null node or null decl_name,
	// e.g. from compiler temporaries
	++anon_num;
	sprintf(buf, "%sanonymous%s%d", Label_Name_Separator, Label_Name_Separator, anon_num);
	return buf;
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
         mtype = MTYPE_I1;
         break;
       case 2:
         mtype = MTYPE_I2;
         break;
       case 4: 
         mtype = MTYPE_I4;
         break;
       case 8:
         DevWarn("8 byte types being used");
         mtype = MTYPE_I8;
         break;
    }
    return mtype;
}
#endif 


#ifdef KEY
extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID, ST *);

// Called from the parser to find out if a local variable declared within
// a structured block in OMP context should be shared among threads
BOOL
Is_shared_mp_var (tree decl_node)
{
  ST * st = DECL_ST (decl_node);

  if (st && ST_sclass (st) == SCLASS_AUTO)
    return TRUE;

  return FALSE;
}
#endif // KEY

// idx is non-zero only for RECORD and UNION, when there is forward declaration
extern TY_IDX
Create_TY_For_Tree (tree type_tree, TY_IDX idx)
{
	if (TREE_CODE(type_tree) == ERROR_MARK)
		exit (RC_USER_ERROR);

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
#ifdef TARG_NVISA
		if (TREE_CODE(type_tree) == RECORD_TYPE) {
		   char *name = Get_Name(TYPE_NAME(type_tree));
		   if (strcmp(name, "char1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uchar1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "char2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uchar2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "char3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uchar3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "char4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uchar4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "short1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ushort1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "short2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ushort2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "short3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ushort3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "short4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ushort4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "int1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uint1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "int2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uint2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "int3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uint3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "int4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "uint4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "long1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ulong1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "long2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ulong2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "long3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ulong3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "long4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "ulong4") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "float1") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "float2") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "float3") == 0)
			   Set_TY_can_be_vector(idx);
		   else if (strcmp(name, "float4") == 0)
			   Set_TY_can_be_vector(idx);
		}
#endif
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
		  struct mongoose_gcc_DST_IDX dst = 
		    Create_DST_type_For_Tree(type_tree,idx,orig_idx);
		  TYPE_DST_IDX(type_tree) = dst;
		}
		return idx;
	}

	TYPE_ID mtype;
	INT64 tsize;
	BOOL variable_size = FALSE;
	tree type_size = TYPE_SIZE(type_tree);

#ifndef KEY
	UINT align = TYPE_ALIGN(type_tree) / BITSPERBYTE;
#endif
	if (TREE_CODE(type_tree) == VOID_TYPE)
		tsize = 0;
	else
	if (type_size == NULL) {
#ifndef KEY
		// incomplete structs have 0 size
		FmtAssert(TREE_CODE(type_tree) == ARRAY_TYPE 
			|| TREE_CODE(type_tree) == UNION_TYPE
			|| TREE_CODE(type_tree) == RECORD_TYPE,
			  ("Create_TY_For_Tree: type_size NULL for non ARRAY/RECORD"));
#endif
		tsize = 0;
	}
	else {
		if (TREE_CODE(type_size) != INTEGER_CST) {
			if (TREE_CODE(type_tree) == ARRAY_TYPE)
				DevWarn ("Encountered VLA at line %d", lineno);
			else
#ifndef KEY
				Fail_FmtAssertion ("VLA at line %d not currently implemented", lineno);
#else
			// Bug 943
			{
			#ifdef PSC_TO_OPEN64
			  printf("opencc: variable-length structure not yet implemented\n");
			#endif
			  exit(2);
			}
#endif
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

#if !defined(TARG_X8664) && !defined(TARG_IA64) && !defined(TARG_MIPS) && !defined(TARG_LOONGSON)
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
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		if (TREE_UNSIGNED(type_tree)) {
			mtype = MTYPE_complement(mtype);
		}
#ifdef KEY
		if (lookup_attribute ("may_alias", TYPE_ATTRIBUTES (type_tree)))
		{
		  // bug 9975: Handle may_alias attribute, we need to create
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
#ifdef TARG_NVISA
		// In C/C++, char is a separate type from signed char
		// or unsigned char.  WHIRL doesn't distinguish this
		// because what we emit needs a sign property, 
		// but whirl2c needs to be able to distinguish.
		// So override the TY_is_character flag (which was previously
		// only used for Fortran) to say that this is a plain char.
		// Gnu creates a "char_type_node" for this purpose.
		if (tsize == 1 && type_tree == char_type_node) {
		  // create a new type with this flag
		  TY &ty = New_TY (idx);
		  TY_Init (ty, tsize, KIND_SCALAR, mtype,
			Save_Str(Get_Name(TYPE_NAME(type_tree))) );
		  Set_TY_is_character(ty);
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
#elif defined(TARG_MIPS) || defined(TARG_IA32) || defined(TARG_X8664) || defined(TARG_NVISA) || defined(TARG_LOONGSON)
		case 12:
		case 16: mtype = MTYPE_FQ; break;
#else
		case 16: mtype = MTYPE_F16; break;
#endif /* TARG_MIPS */
		default: FmtAssert(FALSE, ("Get_TY unexpected REAL_TYPE size %d", tsize));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case COMPLEX_TYPE:
		switch (tsize) {
#ifdef KEY
		case 2: 
		case 4: ErrMsg (EC_Unsupported_Type, "Complex integer");
#endif
		case  8: mtype = MTYPE_C4; break;
		case 16: mtype = MTYPE_C8; break;
#if defined(TARG_MIPS) || defined(TARG_IA32) || defined(TARG_X8664) || defined(TARG_LOONGSON)
		case 32: mtype = MTYPE_CQ; break;
#endif /* TARG_MIPS */
#ifdef TARG_IA64
#ifdef PATHSCALE_MERGE
		case 32: mtype = MTYPE_C10; break; 
#endif
		case 24: mtype = MTYPE_C10; break;
#endif /* TARG_IA64 */
#if defined(TARG_IA32) || defined(TARG_X8664)
		case 24: mtype = MTYPE_CQ; break;
#endif /* TARG_IA32 */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case REFERENCE_TYPE:
	case POINTER_TYPE:
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
	        if (TYPE_NAME(type_tree) == NULL)
	            Set_TY_anonymous(ty);
		// assumes 1 dimension
		// nested arrays are treated as arrays of arrays
		ARB_HANDLE arb = New_ARB ();
		ARB_Init (arb, 0, 0, 0);
		Set_TY_arb (ty, arb);
		Set_ARB_first_dimen (arb);
		Set_ARB_last_dimen (arb);
		Set_ARB_dimension (arb, 1);
#ifdef KEY
		// Bug 660
		// Due to the way we handle extern variables, we may end up 
		// with a type whose base type is undefined upto this point.
		// For example, extern struct bar_t bar[]; 
		// (don't know size of struct bar_t)
		if (!TYPE_SIZE(TREE_TYPE(type_tree)))
		  break;
#endif		
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
#ifdef KEY
			  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
			                                       st);
#endif
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
#ifdef KEY
		    // bug 4086: see comments "throw away any variable
		    // type sizes ..." in finish_decl().
		    else if (!TYPE_DEFER_EXPANSION (type_tree))
#else
		    else
#endif
		    {
			WN *uwn = WFE_Expand_Expr (TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree)) );
			if (WN_opcode (uwn) == OPC_U4I4CVT ||
			    WN_opcode (uwn) == OPC_U8I8CVT) {
				uwn = WN_kid0 (uwn);
			}
#ifdef KEY
			// In the event that uwn operator is not 
			// OPR_LDID, save expr node uwn 
			// and use LDID of that stored address as uwn.
			// Copied from Wfe_Save_Expr in wfe_expr.cxx
			if (WN_operator (uwn) != OPR_LDID) {
        
			  TYPE_ID   mtype   = WN_rtype(uwn);
			  TY_IDX    ty_idx  = MTYPE_To_TY(mtype);
			  ST       *st;
			  st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef KEY
			  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
			                                      st);
#endif
			  WFE_Set_ST_Addr_Saved (uwn);
			  uwn = WN_Stid (mtype, 0, st, ty_idx, uwn);
			  WFE_Stmt_Append (uwn, Get_Srcpos());
			  uwn = WN_Ldid (mtype, 0, st, ty_idx);
			}
#endif /* KEY */
			FmtAssert (WN_operator (uwn) == OPR_LDID,
				("bounds operator for VLA not LDID"));
			ST *st = WN_st (uwn);
			TY_IDX ty_idx = ST_type (st);
			WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
						   (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WFE_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_var (arb, ST_st_idx (st));
		    }
#ifdef KEY
		    else
		    {
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0);
		    }
#endif
		}
		else {
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0);
		}
		if (variable_size
#ifdef KEY
		    // bug 4086
		    && !TYPE_DEFER_EXPANSION (type_tree)
#endif
		   ) {
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
#ifdef KEY
				  WFE_add_pragma_to_enclosing_regions
				                        (WN_PRAGMA_LOCAL, st);
#endif
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
	        if (TYPE_NAME(type_tree) == NULL)
	            Set_TY_anonymous(ty);
		if (TREE_CODE(type_tree) == UNION_TYPE) {
			Set_TY_is_union(idx);
		}
		if (align == 0) align = 1;	// in case incomplete type
		Set_TY_align (idx, align);
		// set idx now in case recurse thru fields
		TYPE_TY_IDX(type_tree) = idx;	

		// to handle nested structs and avoid entering flds
		// into wrong struct, make two passes over the fields.
		// first create the list of flds for the current struct,
		// but don't follow the nested types.  Then go back thru
		// the fields and set the fld_type, recursing down into
		// nested structs.
  		Set_TY_fld (ty, FLD_HANDLE());
		FLD_IDX first_field_idx = Fld_Table.Size ();
		tree field;
		FLD_HANDLE fld;
		for (field = TREE_PURPOSE(type_tree); 
			field;
			field = TREE_CHAIN(field) )
		{
			if (TREE_CODE(field) == TYPE_DECL) {
				DevWarn ("got TYPE_DECL in field list");
				continue;
			}
			if (TREE_CODE(field) == CONST_DECL) {
				DevWarn ("got CONST_DECL in field list");
				continue;
			}
			fld = New_FLD ();
			FLD_Init (fld, Save_Str(Get_Name(DECL_NAME(field))), 
				0, // type
				Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
				Get_Integer_Value(DECL_FIELD_BIT_OFFSET(field))
					/ BITSPERBYTE );
                        if (DECL_NAME(field) == NULL)
                                Set_FLD_is_anonymous(fld);
#ifdef OLDCODE
			if ( ! DECL_BIT_FIELD(field)
				&& Get_Integer_Value(DECL_SIZE(field)) > 0
				&& Get_Integer_Value(DECL_SIZE(field))
				  != (TY_size(Get_TY(TREE_TYPE(field))) 
					* BITSPERBYTE) )
			{
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because 
				// otherwise the field type is wrong.
				DevWarn("field size %d doesn't match type size %d", 
					Get_Integer_Value(DECL_SIZE(field)),
					TY_size(Get_TY(TREE_TYPE(field)))
						* BITSPERBYTE );
				DECL_BIT_FIELD(field) = 1;
			}
			if (DECL_BIT_FIELD(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld, 
//					Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
					Get_Integer_Value(
						DECL_FIELD_BIT_OFFSET(field))
					% BITSPERBYTE );
				Set_FLD_bsize (fld, Get_Integer_Value(DECL_SIZE(field)));
			}
#endif /* OLDCODE */
		}
  		FLD_IDX last_field_idx = Fld_Table.Size () - 1;
		if (last_field_idx >= first_field_idx) {
			Set_TY_fld (ty, FLD_HANDLE (first_field_idx));
			Set_FLD_last_field (FLD_HANDLE (last_field_idx));
		}
		// now set the fld types.
		fld = TY_fld(ty);
		for (field = TREE_PURPOSE(type_tree);
			field;
			field = TREE_CHAIN(field))
		{
#ifdef KEY
			const  int FLD_BIT_FIELD_SIZE   = 64;
#endif
			if (TREE_CODE(field) == TYPE_DECL)
				continue;
			if (TREE_CODE(field) == CONST_DECL)
				continue;
			if ( ! DECL_BIT_FIELD(field)
#ifdef KEY
			        && DECL_SIZE(field)
// We don't handle bit-fields > 64 bits. For an INT field of 128 bits, we
// make it 64 bits. But then don't set it as FLD_IS_BIT_FIELD.
				&& Get_Integer_Value(DECL_SIZE(field)) <=
				   FLD_BIT_FIELD_SIZE
#endif /* KEY */
				&& Get_Integer_Value(DECL_SIZE(field)) > 0
				&& Get_Integer_Value(DECL_SIZE(field))
				  != (TY_size(Get_TY(TREE_TYPE(field))) 
					* BITSPERBYTE) )
			{
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
//					Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
					Get_Integer_Value(
						DECL_FIELD_BIT_OFFSET(field))
					% BITSPERBYTE );
				Set_FLD_bsize (fld, Get_Integer_Value(DECL_SIZE(field)));
			}
			TY_IDX fty_idx = Get_TY(TREE_TYPE(field));
			if ((TY_align (fty_idx) > align) || (TY_is_packed (fty_idx)))
				Set_TY_is_packed (ty);
			Set_FLD_type(fld, fty_idx);
			fld = FLD_next(fld);
		}
		} // end record scope
		break;
	case METHOD_TYPE:
		DevWarn ("Encountered METHOD_TYPE at line %d", lineno);
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
			    idx = MTYPE_To_TY (MTYPE_M8I4);
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
	  struct mongoose_gcc_DST_IDX dst = 
	    Create_DST_type_For_Tree(type_tree,idx,orig_idx);
	  TYPE_DST_IDX(type_tree) = dst;
	}
	return idx;
}

#ifdef KEY
void 
Create_DST_For_Tree (tree decl_node, ST* st)
{
  struct mongoose_gcc_DST_IDX dst =
    Create_DST_decl_For_Tree(decl_node,st);
  DECL_DST_IDX(decl_node) = dst; 
  return;
}
#endif

ST*
Create_ST_For_Tree (tree decl_node)
{
  TY_IDX     ty_idx;
  ST*        st;
  char      *name;
  ST_SCLASS  sclass;
  ST_EXPORT  eclass;
  SYMTAB_IDX level;

  if (TREE_CODE(decl_node) == ERROR_MARK)
    exit (RC_USER_ERROR);

#ifdef PATHSCALE_MERGE
  //begin - fix for bug OSP 204
  if (TREE_CODE (decl_node) == VAR_DECL &&
      (TREE_STATIC(decl_node) || DECL_EXTERNAL(decl_node) || TREE_PUBLIC(decl_node) ) &&
      DECL_ASSEMBLER_NAME(decl_node) ) {
    name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME(decl_node));
    if (*name == '*' ) name++;
  }
  else if (DECL_NAME (decl_node)) {
#ifdef TARG_NVISA
   if (TREE_CODE(decl_node) == PARM_DECL) {
	static char buf[256];
	// have to mangle parameter names since referred to by name;
	// need to mangle such that params in different functions are unique.
	sprintf(buf, "__cudaparm_%s_%s",
		ST_name(Get_Current_PU_ST()),
		IDENTIFIER_POINTER (DECL_NAME (decl_node)));
	name = buf;
   }
   else
#endif
    name = IDENTIFIER_POINTER (DECL_NAME (decl_node));
  }
  //end - fix for bug OSP 204
#endif
  else {
    DevWarn ("no name for DECL_NODE");
    name = "__unknown__";
  }

  switch (TREE_CODE(decl_node)) {

    case FUNCTION_DECL:
      {
        TY_IDX func_ty_idx = Get_TY(TREE_TYPE(decl_node));

        if (DECL_WIDEN_RETVAL (decl_node)) {
/*
          extern tree long_long_integer_type_node;
          extern tree long_long_unsigned_type_node;
*/
          tree type_tree = TREE_TYPE(decl_node);
          tree ret_type_tree = TREE_TYPE (type_tree);
          TY_IDX ret_ty_idx = Get_TY(ret_type_tree);
	  if (MTYPE_signed (TY_mtype (ret_ty_idx)))
            TREE_TYPE (type_tree) = long_long_integer_type_node;
          else
            TREE_TYPE (type_tree) = long_long_unsigned_type_node;
          TY_IDX old_func_ty_idx = func_ty_idx;
          func_ty_idx = Create_TY_For_Tree (type_tree, TY_IDX_ZERO);
          TREE_TYPE (type_tree) = ret_type_tree;
          TYPE_TY_IDX(type_tree) = old_func_ty_idx;
        }

        sclass = SCLASS_EXTERN;
        eclass = TREE_PUBLIC(decl_node) ? EXPORT_PREEMPTIBLE : EXPORT_LOCAL;
        level  = GLOBAL_SYMTAB+1;

        PU_IDX pu_idx;
        PU&    pu = New_PU (pu_idx);

        PU_Init (pu, func_ty_idx, level);

        st = New_ST (GLOBAL_SYMTAB);

	if (DECL_CDECL(decl_node))
	  Set_PU_is_cdecl(pu_idx);

#ifdef KEY	// Fix bug # 34
// gcc sometimes adds a '*' and itself handles it this way while outputing
	char * check_for_star = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (
								decl_node));
	if (*check_for_star == '*')
	    check_for_star++;
        ST_Init (st, Save_Str (check_for_star),
                 CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));
#else
        ST_Init (st,
                 Save_Str ( IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node))),
                 CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));
#endif // KEY
/*
       if (TREE_CODE(TREE_TYPE(decl_node)) == METHOD_TYPE)
         fprintf (stderr, "Create_ST_For_Tree: METHOD_TYPE\n");
*/
      }
      break;

    case PARM_DECL:
    case VAR_DECL:
      {
        if (TREE_CODE(decl_node) == PARM_DECL) {
          sclass = SCLASS_FORMAL;
          eclass = EXPORT_LOCAL;
          level = CURRENT_SYMTAB;
        }
        else {
          if (DECL_CONTEXT (decl_node) == 0) {
            if (TREE_PUBLIC (decl_node)) {
	      if (DECL_INITIAL(decl_node))
		sclass = SCLASS_DGLOBAL;
	      else if (TREE_STATIC(decl_node)) {
		if (flag_no_common || DECL_SECTION_NAME(decl_node) ||
                    DECL_THREAD_LOCAL(decl_node) )
              	  sclass = SCLASS_UGLOBAL;
		else
              	  sclass = SCLASS_COMMON;
	      }
	      else
              	sclass = SCLASS_EXTERN;
#ifdef TARG_IA64 
          // bug fix for OSP_89 && OSP_173 && OSP_169
	  extern BOOL Use_Call_Shared_Link,Gp_Rel_Aggresive_Opt;
          if (!flag_pic) {
            if (Use_Call_Shared_Link && Gp_Rel_Aggresive_Opt &&
                sclass != SCLASS_EXTERN && sclass != SCLASS_COMMON)
              eclass = EXPORT_PROTECTED;
            else
              eclass = EXPORT_PREEMPTIBLE;
          }
          else
            eclass = EXPORT_PREEMPTIBLE;
        }
#else
            eclass = EXPORT_PREEMPTIBLE;
          }
#endif
            else {
              	sclass = SCLASS_FSTATIC;
		eclass = EXPORT_LOCAL;
#ifdef TARG_NVISA
		if (TREE_CODE(TREE_TYPE(decl_node)) == ARRAY_TYPE
		  && TYPE_SIZE(TREE_TYPE(decl_node)) == NULL)
		{
		    // HACK WARNING:
		    // cudafe is generating these as fake pointers,
		    // which it later allocates all to the same memory;
		    // because they are static wopt thinks they are already
		    // allocated and don't alias, but they do, 
		    // so change them to be common preemptible
		    // (ideally cudafe should probably do this itself).
		    DevWarn("static array of unknown size, change to common");
              	    sclass = SCLASS_COMMON;
		    eclass = EXPORT_PREEMPTIBLE;
		}
#endif
            }
            level = GLOBAL_SYMTAB;
          }
          else {
            if (DECL_EXTERNAL(decl_node)) {
	      sclass = SCLASS_EXTERN;
	      level  = GLOBAL_SYMTAB;
              eclass = EXPORT_PREEMPTIBLE;
            }
            else {
	      if (TREE_STATIC (decl_node)) {
		sclass = SCLASS_PSTATIC;
		if (pstatic_as_global
#ifdef KEY
// bugs 2647, 2681
// Promote it if we are sure the function containing this var has been
// inlined.
		    || DECL_PROMOTE_STATIC (decl_node)
#endif
		   )
			level = GLOBAL_SYMTAB;
		else
			level = CURRENT_SYMTAB;
              }
              else {
		sclass = SCLASS_AUTO;
		level = decl_node->decl.symtab_idx ?
                        decl_node->decl.symtab_idx : CURRENT_SYMTAB;
              }
              eclass = EXPORT_LOCAL;
            }
          }
        }
        st = New_ST (level);
        ty_idx = Get_TY (TREE_TYPE(decl_node));
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
	// in Get_ST (instead of ty_idx). Currently we do the latter.
#endif // KEY
        ST_Init (st, Save_Str(name), CLASS_VAR, sclass, eclass, ty_idx);
        if (TREE_CODE(decl_node) == PARM_DECL) {
		Set_ST_is_value_parm(st);
        }
        if (TREE_CODE(decl_node) == VAR_DECL && TREE_READONLY(decl_node) 
		// const_var doesn't apply to stack variables.
		// we are really doing this mainly for external const
		// (if initialized then just replaced with initial value).
		&& ST_sclass(st) != SCLASS_AUTO) 
	{
		Set_ST_is_const_var(st);
	}
	if (TREE_CODE(decl_node) == VAR_DECL && DECL_THREAD_LOCAL(decl_node)) {
		Set_ST_is_thread_local (st);
	}
      }
      break;

    default:
      {
        Fail_FmtAssertion ("Create_ST_For_Tree: unexpected tree type");
      }
      break;
  }

  DECL_ST(decl_node) = st;

  if ((DECL_WEAK (decl_node)) && (TREE_CODE (decl_node) != PARM_DECL)) {
    Set_ST_is_weak_symbol (st);
/*
    if (TREE_CODE (decl_node) == FUNCTION_DECL)
      Set_ST_sclass (st, SCLASS_TEXT);
*/
  }

#ifdef TARG_NVISA
  if (DECL_GLOBAL(decl_node)) {
    Set_ST_in_global_mem (st);
  }
  if (DECL_LOCAL(decl_node)) {
    Set_ST_in_local_mem (st);
  }
  if (DECL_SHARED(decl_node)) {
    Set_ST_in_shared_mem (st);
    if (ST_sclass(st) == SCLASS_FORMAL)
    	Set_ST_is_const_var(st);	/* param space is readonly */
  }
  if (DECL_CONSTANT(decl_node)) {
    Set_ST_in_constant_mem (st);
    Set_ST_is_const_var(st);
  }
  if (DECL_TEXTURE(decl_node)) {
    Set_ST_in_texture_mem (st);
  }
  if (DECL_THREAD_LIMIT (decl_node) != 0 &&
      DECL_BLOCK_LIMIT (decl_node) != 0 ) {
    Set_PU_thread_limit (Pu_Table [ST_pu(st)], DECL_THREAD_LIMIT (decl_node));
    Set_PU_block_limit (Pu_Table [ST_pu(st)], DECL_BLOCK_LIMIT (decl_node));
  }
#endif /* TARG_NVISA */

  if (DECL_SECTION_NAME (decl_node)) {
    if (TREE_CODE (decl_node) == FUNCTION_DECL)
      level = GLOBAL_SYMTAB;
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_SECTION_NAME,
                  Save_Str (TREE_STRING_POINTER (DECL_SECTION_NAME (decl_node))));
    Set_ST_has_named_section (st);
  }

  if (DECL_SYSCALL_LINKAGE (decl_node)) {
	Set_PU_has_syscall_linkage (Pu_Table [ST_pu(st)]);
  }

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
      DST_INFO_IDX dst_idx ;
      struct mongoose_gcc_DST_IDX tdst
	= DECL_DST_IDX(decl_node);
      cp_to_dst_from_tree(&dst_idx,&tdst);
      // Bug 6679 - when the variables inside the second definition of an 
      // "extern inline" function (with an attribute) are encountered, there
      // will already be an entry in the DST table. In that event, update the
      // ST (offset) field in the DST entry. The ST offset may change because
      // now we are expanding the function body.
      // Just deleting the DECL_DST_IDX entry in WFE_Null_ST_References
      // will not help because the DST entry was already appended to the 
      // DST tree.
      if(ST_class(st) == CLASS_VAR && !DST_IS_NULL(dst_idx)) {
	DST_INFO *info_ptr = DST_INFO_IDX_TO_PTR(dst_idx);
	DST_ATTR_IDX attr_idx = DST_INFO_attributes(info_ptr);
	DST_VARIABLE *attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);
	DST_ASSOC_INFO_st_idx(DST_VARIABLE_def_st(attr)) = ST_st_idx(st);
      } else {
	struct mongoose_gcc_DST_IDX dst =
	  Create_DST_decl_For_Tree(decl_node,st);
	DECL_DST_IDX(decl_node) = dst;
      }
    }
#else
     struct mongoose_gcc_DST_IDX dst =
       Create_DST_decl_For_Tree(decl_node,st);
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
