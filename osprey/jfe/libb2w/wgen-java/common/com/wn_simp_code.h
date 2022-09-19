/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#if defined(BACK_END) && defined(WN_SIMP_WORKING_ON_WHIRL)
#include "be_symtab.h"
#endif

/*===================================================================================

wn_simp_code.h

This file contains the routines which do the real work of the simplifier. It is 
written so that it can easily be made to work on data structures other than WHIRL.

How to use this:

Several functions and macros need to be created by the user. These are
listed below.  Although the data structure does not need to be a WHIRL
node, it must have the same information as a WHIRL node accessible to
it. Define the macros and functions, then include this file to get a
simplifier. See the files common/com/wn_simp.{cxx,h} for an example.

The example below shows all the definitions necessary to build the simplifier
for WHIRL trees. 


--------------------------------------------------------------------------------------


// define the type simpnode as the basic data type
typedef WN * simpnode;

// These are accessors which take a simpnode and return the various 
// attributes/children from it

#define SIMPNODE_opcode WN_opcode                   // get a WHIRL opcode from a simpnode
#define SIMPNODE_operator(x) WN_operator(x)  // get a WHIRL opcode from a simpnode
#define SIMPNODE_rtype(x) WN_rtype(x) // get the rtype from a node
#define SIMPNODE_desc(x) WN_desc(x)   // get the desc from a node
#define SIMPNODE_load_offset WN_load_offset         //   " "   "   load_offset
#define SIMPNODE_cvtl_bits WN_cvtl_bits             //   " "   "   cvtl_bits
#define SIMPNODE_st WN_st
#define SIMPNODE_kid0 WN_kid0
#define SIMPNODE_kid1 WN_kid1
#define SIMPNODE_element_size WN_element_size
#define SIMPNODE_idname_offset WN_idname_offset
#define SIMPNODE_lda_offset WN_lda_offset           // etc....
#define SIMPNODE_num_dim WN_num_dim
#define SIMPNODE_element_size WN_element_size
#define SIMPNODE_array_base WN_array_base
#define SIMPNODE_array_index WN_array_index
#define SIMPNODE_array_dim WN_array_dim
#define SIMPNODE_intrinsic WN_intrinsic
#define SIMPNODE_kid_count WN_kid_count
#define SIMPNODE_kid WN_kid
#define SIMPNODE_const_val WN_const_val             // get the 64 bit immediate integer constant
#ifdef KEY
#define SIMPNODE_st_idx WN_st_idx
#endif
                                                    // value from a simpnode
#define SIMPNODE_fconst_val Const_Val               // get a TCON from a simpnode for all types
                                                    // but integer
#define SIMPNODE_field_id WN_field_id		    // get the field id
#define SIMPNODE_i_field_id WN_field_id		    // field id for indirect
#define SIMPNODE_bit_offset WN_bit_offset	    // get the bit offset
#define SIMPNODE_i_bit_offset WN_bit_offset	    // bit offset for indirect

// functions required

// simpnode SIMPNODE_SimpCreateExp1(OPCODE, simpnode)  // create a 1 operand expression
// simpnode SIMPNODE_SimpCreateExp2(OPCODE, simpnode, simpnode)  // create a two operand expression
// simpnode SIMPNODE_SimpCreateExp3(OPCODE, simpnode, simpnode, simpnode)  // create a 3 operand expression
// void SIMPNODE_TREE_DELETE(simpnode)                 // delete a simpnode and all of its children
                                                    // This is called when the node is never used in
                                                    // the expression again. 
// void SIMPNODE_DELETE(simpnode)                   // delete only the argument
                                                    // leaving the children intact
// simpnode SIMPNODE_CopyNode(simpnode)             // create a copy of a node. It is guaranteed
                                                    // to be placed into the tree. 
// simpnode SIMPNODE_CreateIntconst(OPCODE, int64)  // create an integer constant simpnode from a
                                                    // WHIRL T1INTCONST opcode and a value
// simpnode SIMPNODE_CreateFloatconstFromTcon(TCON) // create a simpnode representing a floating
                                                    // point or complex constant from a TCON node


#define SIMPNODE_SimpCreateExp1 WN_SimpCreateExp1
#define SIMPNODE_SimpCreateExp2 WN_SimpCreateExp2
#define SIMPNODE_SimpCreateExp3 WN_SimpCreateExp3
#define SIMPNODE_SimpCreateExtract WN_SimpCreateExtract
#define SIMPNODE_SimpCreateDeposit WN_SimpCreateDeposit
#define SIMPNODE_TREE_DELETE WN_DELETE_Tree
#define SIMPNODE_DELETE WN_Delete
#define SIMPNODE_CopyNode WN_CopyNode
#define SIMPNODE_CreateIntconst WN_CreateIntconst
#define SIMPNODE_CreateFloatconstFromTcon Make_Const

// Externally visible routines. These three are defined in wn_simp_code.h. They need
// a name defined here and in whatever external interface file exists for this routine.
// The argument lists for these are fixed. Only the names need be defined.
// Each of these returns a simpnode if simplification was done, or NULL if
// no simplifications could be performed.
//
// simpnode SIMPNODE_SimplifyExp1(OPCODE opc, simpnode k0) 
// simpnode SIMPNODE_SimplifyExp2(OPCODE opc, simpnode k0, simpnode k1) 
// simpnode SIMPNODE_SimplifyExp3(OPCODE opc, simpnode k0, simpnode k1, simpnode k2) 
// simpnode SIMPNODE_SimplifyCvtl(OPCODE opc, INT16 cvtl_bits, simpnode k0) 

#define SIMPNODE_SimplifyExp1 WN_SimplifyExp1
#define SIMPNODE_SimplifyExp2 WN_SimplifyExp2
#define SIMPNODE_SimplifyExp3 WN_SimplifyExp3
#define SIMPNODE_SimplifyCvtl WN_SimplifyCvtl

================================================================================================
================================================================================================*/


/* Size in bits of each type */
#define SIMP_TYPE_SIZE(x) MTYPE_size_min(x)

/* Claasification macros */
   
#define SIMP_IS_TYPE_INTEGRAL(x) ((MTYPE_type_class(x)&MTYPE_CLASS_INTEGER)!=0)
#define SIMP_IS_TYPE_UNSIGNED(x) ((MTYPE_type_class(x)&MTYPE_CLASS_UNSIGNED)!=0)
#define SIMP_IS_TYPE_FLOATING(x) ((MTYPE_type_class(x)&MTYPE_CLASS_FLOAT)!=0)
#define SIMP_IS_TYPE_COMPLEX(x) ((MTYPE_type_class(x)&MTYPE_CLASS_COMPLEX)!=0)

/* saves a little typing */
#define OPC_FROM_OPR(opr,type) (OPCODE_make_op(opr,type,MTYPE_V))

/* has the package ever been initialized? */
static BOOL SIMPNODE_simp_initialized = FALSE;
static void SIMPNODE_Simplify_Initialize( void );

/* Tracing functions */
/* Dump a tree if a simplification occured */
static BOOL trace_trees;
#define SHOW_TREE(opc,k0,k1,result) if ((result!=NULL) && trace_trees) show_tree(opc,k0,k1,result)


/* Print the rule causing a simplification */
static BOOL trace_rules;

#define SHOW_RULE(x) show_rule(x)

static void show_rule(const char * rule) 
{
   if (trace_rules) {
      fprintf(TRACEFILE,"Rule: %s\n",rule);
   }
}


/* used before their definition */
static simpnode SIMPNODE_ConstantFold1(OPCODE opc, simpnode k0);
static simpnode SIMPNODE_ConstantFold2(OPCODE opc, simpnode k0, simpnode k1);
static simpnode SIMPNODE_SimpCreateExp3(OPCODE opc, simpnode k0, simpnode k1, simpnode k2);
static simpnode SIMPNODE_SimpCreateExp2(OPCODE opc, simpnode k0, simpnode k1);
static simpnode SIMPNODE_SimpCreateExp1(OPCODE opc, simpnode k0);
static simpnode SIMPNODE_SimpCreateExtract(OPCODE opc, INT16 boffset, INT16 bsize, simpnode k0);
static simpnode SIMPNODE_SimpCreateDeposit(OPCODE opc, INT16 boffset, INT16 bsize,
					   simpnode k0, simpnode k1);

#ifdef SIMPNODE_SimpCreateCvtl
static simpnode SIMPNODE_SimpCreateCvtl(OPCODE opc, INT16 bits, simpnode k0);
#endif

/* Utility macros */

/* Simple wrapper for SIMPNODE_CreateIntconst */
#define SIMP_INTCONST(ty,value) SIMPNODE_CreateIntconst(OPC_FROM_OPR(OPR_INTCONST,ty),(value))

/* Simple wrapper for Make_Const */
#define SIMP_FLOATCONST(ty,value) SIMPNODE_CreateFloatconstFromTcon(Host_To_Targ_Float(ty,(value)))

#ifdef TARG_X8664
#define SIMP_SIMDCONST(ty,value)					    \
  SIMPNODE_CreateSIMDconstFromTcon(					    \
    Create_Simd_Const(ty,						    \
		      Host_To_Targ_Float((ty) == MTYPE_V8F4 ? MTYPE_F4 :    \
					   (ty) == MTYPE_V16F4 ? MTYPE_F4 : \
					   (ty) == MTYPE_V16F8 ? MTYPE_F8 : \
					   (ty) == MTYPE_V16C4 ? MTYPE_C4 : \
					   (ty) == MTYPE_V16C8 ? MTYPE_C8 : \
					   MTYPE_UNKNOWN,		    \
					 (value))))
#endif

/* Delete a whirl tree. Done this way (for now) for debugging purposes */
static void simp_delete_tree(simpnode w)
{
   if (!trace_trees) SIMPNODE_TREE_DELETE(w);
}

static void simp_delete(simpnode w)
{
   if (!trace_trees) SIMPNODE_DELETE(w);
}

#define SIMP_DELETE(x) simp_delete(x)
#define SIMP_DELETE_TREE(x) simp_delete_tree(x)

#define SIMP_TYPE(x) SIMPNODE_rtype(x)

inline simpnode SIMPNODE_GetDefinition(simpnode x)
{
#ifdef BACK_END
#ifdef WN_SIMP_WORKING_ON_WHIRL
  if (SIMPNODE_operator(x) == OPR_LDID && ST_class(WN_st(x)) == CLASS_PREG) {
    WN *home = Preg_Home(WN_load_offset(x));
    if (home) x = home;
  }
#else
  // TODO: investigate how to do this with codereps in the optimizer
#endif
#endif
  return x;
}

#ifdef KEY
// Check if the types are real/complex, return FALSE otherwise
inline BOOL SIMP_Check (TYPE_ID rtype, simpnode x)
{
    if (!OPT_Enable_Simp_Fold) return FALSE;

    TYPE_ID tcon_type = TCON_ty (ST_tcon_val (SIMPNODE_st(x)));

    if (!MTYPE_is_float (tcon_type) && !MTYPE_is_complex (tcon_type))
      return FALSE;
    if (rtype == tcon_type ||
#ifdef TARG_IA64
        (rtype == MTYPE_F10 && tcon_type == MTYPE_C10) ||
#endif
        (rtype == MTYPE_F8 && tcon_type == MTYPE_C8) ||
	(rtype == MTYPE_F4 && tcon_type == MTYPE_C4))
      return TRUE;

    return FALSE;
}

// Return TRUE if it is a real or complex constant
inline BOOL SIMP_Check_Real_Complex (simpnode x)
{
  if (!OPT_Enable_Simp_Fold) return FALSE;

  if (SIMPNODE_operator(x) == OPR_LDID && 
      ST_class(SIMPNODE_st(x)) == CLASS_CONST)
  {
    return SIMP_Check (SIMPNODE_rtype (x), x);
  }
  else if (SIMPNODE_operator (x) == OPR_ILOAD &&
           SIMPNODE_operator (SIMPNODE_kid (x,0)) == OPR_ARRAY)
  {
    simpnode arr = SIMPNODE_kid (x,0);

    // 1-dimensional array with constant index
    if (SIMPNODE_num_dim (arr) != 1 ||
        SIMPNODE_operator (SIMPNODE_array_index (arr, 0)) !=
	OPR_INTCONST)
      return FALSE;

    // array base
    simpnode base = SIMPNODE_array_base (arr);
    if (SIMPNODE_operator (base) == OPR_LDA &&
        ST_class (SIMPNODE_st (base)) == CLASS_CONST)
    {
      return SIMP_Check (SIMPNODE_rtype(x), base);
    }
  }
  return FALSE;
}

// Return TRUE if 'x' has a string constant
inline BOOL SIMP_Is_Str_Constant (simpnode x)
{
  if (!OPT_Enable_Simp_Fold) return FALSE;

  simpnode base = SIMPNODE_kid (x,0);

  if (SIMPNODE_operator (x) == OPR_ILOAD &&
      (SIMPNODE_desc (x) == MTYPE_U1 || SIMPNODE_desc (x) == MTYPE_I1))
  {
    if (SIMPNODE_operator (base) == OPR_ARRAY)
    {
	if (SIMPNODE_num_dim (base) != 1 ||
	    SIMPNODE_operator (SIMPNODE_array_index (base, 0)) != 
	    OPR_INTCONST)
	  return FALSE;
        base = SIMPNODE_array_base (base);
    }

    // e.g. array of array
    if (!OPERATOR_has_sym (SIMPNODE_operator (base))) return FALSE;

    ST * s = SIMPNODE_st (base);
    if (SIMPNODE_operator (base) == OPR_LDA &&
        ST_class (s) == CLASS_CONST &&
	TCON_ty (ST_tcon_val (s)) == MTYPE_STR)
      return TRUE;
  }

  return FALSE;
}
#endif // KEY

/* Decide if a node is a constant or not */

inline BOOL SIMP_Is_Int_Constant(simpnode x)
{
#ifdef KEY // fix hidden bug
  x = SIMPNODE_GetDefinition(x);
#endif // KEY
  return SIMPNODE_operator(x)==OPR_INTCONST;
}

inline BOOL SIMP_Is_Flt_Constant(simpnode x)
{
  x = SIMPNODE_GetDefinition(x);
#ifdef KEY
  if (SIMP_Check_Real_Complex (x))
    return TRUE;
#endif // KEY
  return SIMPNODE_operator(x)==OPR_CONST;
}

inline BOOL SIMP_Is_Constant(simpnode x)
{
  x = SIMPNODE_GetDefinition(x);
#ifdef KEY
  // real/complex/string constants
  if (SIMP_Check_Real_Complex (x) || SIMP_Is_Str_Constant (x))
    return TRUE;
#endif // KEY
  return    SIMPNODE_operator(x)==OPR_INTCONST
	 || SIMPNODE_operator(x)==OPR_CONST;
}


/* Get the value of a constant node */

inline INT64 SIMP_Int_ConstVal(simpnode x)
{
  x = SIMPNODE_GetDefinition(x);
  return SIMPNODE_const_val(x);
}

// Return tcon for float constants, extract float from complex if required
inline TCON SIMP_Flt_ConstVal(simpnode x)
{
  x = SIMPNODE_GetDefinition(x);
#ifdef KEY
  if (SIMPNODE_operator(x) == OPR_LDID && 
      ST_class(SIMPNODE_st(x)) == CLASS_CONST)
  {
    TYPE_ID rtype = SIMPNODE_rtype(x);
    TYPE_ID tcon_type = TCON_ty (ST_tcon_val (SIMPNODE_st(x)));

    if (rtype != tcon_type)
    {
      if (rtype == MTYPE_F10 && tcon_type == MTYPE_C10)
      {
	TCON c;
	if (SIMPNODE_load_offset (x) == 0)
	  c = Extract_Complex_Real (ST_tcon_val (SIMPNODE_st(x)));
	else if (SIMPNODE_load_offset (x) == 16)
	  c = Extract_Complex_Imag (ST_tcon_val (SIMPNODE_st(x)));
	else Fail_FmtAssertion ("Loading real from outside of complex value");

	return c;
      }
      else if (rtype == MTYPE_F8 && tcon_type == MTYPE_C8)
      {
        TCON c;
        if (SIMPNODE_load_offset (x) == 0)
	  c = Extract_Complex_Real (ST_tcon_val (SIMPNODE_st(x)));
	else if (SIMPNODE_load_offset (x) == 8)
	  c = Extract_Complex_Imag (ST_tcon_val (SIMPNODE_st(x)));
	else Fail_FmtAssertion ("Loading real from outside of complex value");

	return c;
      }
      else if (rtype == MTYPE_F4 && tcon_type == MTYPE_C4)
      {
        TCON c;
        if (SIMPNODE_load_offset (x) == 0)
	  c = Extract_Complex_Real (ST_tcon_val (SIMPNODE_st(x)));
	else if (SIMPNODE_load_offset (x) == 4)
	  c = Extract_Complex_Imag (ST_tcon_val (SIMPNODE_st(x)));
	else Fail_FmtAssertion ("Loading real from outside of complex value");

	return c;
      }
    }
    else // rtype == tcon_type
      return ST_tcon_val (SIMPNODE_st (x));
  }
  else if (SIMPNODE_operator(x) == OPR_ILOAD /* must be ILOAD of array */)
  {
    TYPE_ID rtype = SIMPNODE_rtype (x);
    simpnode arr = SIMPNODE_kid (x,0);

    Is_True (SIMPNODE_operator (arr) == OPR_ARRAY, ("Expected array as kid of OPR_ILOAD"));
    simpnode base = SIMPNODE_kid (arr, 0);

    TYPE_ID tcon_type = TCON_ty (ST_tcon_val (SIMPNODE_st(base)));

    Is_True (SIMPNODE_operator (base) == OPR_LDA, ("Unexpected operator"));

    int ofst = SIMPNODE_const_val (SIMPNODE_array_index (arr, 0)) * 
               SIMPNODE_element_size (arr) + SIMPNODE_load_offset (base);

    if (rtype == MTYPE_F10 && tcon_type == MTYPE_C10)
    {
      TCON c;
      if (ofst == 0)
	c = Extract_Complex_Real (ST_tcon_val (SIMPNODE_st(base)));
      else if (ofst == 16)
	c = Extract_Complex_Imag (ST_tcon_val (SIMPNODE_st(base)));
      else Fail_FmtAssertion ("Loading real from outside of complex value");

      return c;
    }
    else if (rtype == MTYPE_F8 && tcon_type == MTYPE_C8)
    {
      TCON c;
      if (ofst == 0)
        c = Extract_Complex_Real (ST_tcon_val (SIMPNODE_st(base)));
      else if (ofst == 8)
        c = Extract_Complex_Imag (ST_tcon_val (SIMPNODE_st(base)));
      else Fail_FmtAssertion ("Loading real from outside of complex value");

      return c;
    }
    else if (rtype == MTYPE_F4 && tcon_type == MTYPE_C4)
    {
      TCON c;
      if (ofst == 0)
        c = Extract_Complex_Real (ST_tcon_val (SIMPNODE_st(base)));
      else if (ofst == 4)
        c = Extract_Complex_Imag (ST_tcon_val (SIMPNODE_st(base)));
      else Fail_FmtAssertion ("Loading real from outside of complex value");

      return c;
    }
    else if (rtype == tcon_type)
      return ST_tcon_val (SIMPNODE_st (base));
  }
#endif // KEY
  return SIMPNODE_fconst_val(x);
}

#ifdef KEY
inline TCON SIMP_Str_ConstVal (simpnode x)
{
  Is_True (SIMPNODE_operator (x) == OPR_ILOAD,
           ("Unexpected load operator for string constant"));

  simpnode base = SIMPNODE_kid (x,0);
  BOOL has_array = FALSE;
  simpnode arr = NULL;
  if (SIMPNODE_operator (base) == OPR_ARRAY)
  {
       has_array = TRUE;
       arr = base;
       base = SIMPNODE_kid (base, 0);
  }

  TCON c = ST_tcon_val (SIMPNODE_st (base));

  char * s = Index_to_char_array (TCON_str_idx (c));
  s += SIMPNODE_load_offset (base) + SIMPNODE_load_offset (x) +
       (has_array ? SIMPNODE_const_val (SIMPNODE_array_index (arr, 0)) : 0);

  return Host_To_Targ(SIMPNODE_rtype(x),(INT64) *s); 
}
#endif // KEY

#define IS_POWER_OF_2(x) (((x)!=0) && ((x) & ((x)-1))==0)

/* Utility routine to create a 64 bit number with from 0 to 64 bits in it.
 */
static INT64 create_bitmask(INT64 num_bits)
{
   if (num_bits == 0) return (0);
   if (num_bits == 64) return (-1LL);
   return ((1LL << num_bits) - 1);
}

/* Compute FLOOR(log2(x))
 * 
 */
static UINT64 log2(UINT64 x)
{
   UINT64 l;

   l = 0;
   while (x > 1) {
     x >>= 1;
     ++l;
   }
   return (l);
}

/***********************************************************/
/* 
 * Some floating point and other constant handling utilities
 */


/* Determine if an add overflows or not, and do the add in any case */
/* Return TRUE if the add is OK. */

static BOOL is_add_ok(INT64 *sum, INT64 i1, INT64 i2, TYPE_ID ty)
{
   *sum = i1 + i2;

   switch (ty) {
    case MTYPE_U4:
      if ((UINT64) *sum > UINT32_MAX) return(FALSE);
      break;

    case MTYPE_I4:
      if (*sum > INT32_MAX) return(FALSE);
      if (*sum < INT32_MIN) return(FALSE);
      break;

    case MTYPE_U8:
      if ((UINT64) *sum < (UINT64) i1) return (FALSE); 
      break;

    case MTYPE_I8:
      if ((i1^i2) < 0) return (TRUE); /* can't overflow if signs are different */
      /* Signs are the same */
      if ((i1 > 0 || i2 > 0) && *sum <= 0) return (FALSE);
      if ((i1 < 0 || i2 < 0) && *sum >= 0) return (FALSE); 
      break;
      
    default:
      /* Don't recognize the type, play it safe */
      return (FALSE);
   }

   return (TRUE);
}

static BOOL is_sub_ok(INT64 *sum, INT64 i1, INT64 i2, TYPE_ID ty)
{
   *sum = i1 - i2;

   switch (ty) {

    case MTYPE_I4:
      if (*sum > INT32_MAX) return(FALSE);
      if (*sum < INT32_MIN) return(FALSE);
      break;

    case MTYPE_U4:
    case MTYPE_U8:
      if ((UINT64) i2 > (UINT64) i1) return FALSE;
      break;

    case MTYPE_I8:
      if ((i1^i2) >= 0) return (TRUE); /* can't overflow if signs are same */
      /* Signs are different */
      if ((i1 > i2) && *sum <= 0) return (FALSE);
      if ((i1 < i2) && *sum >= 0) return (FALSE); 
      break;
      
    default:
      /* Don't recognize the type, play it safe */
      return (FALSE);
   }

   return (TRUE);
}



/*
 * This utility determines if a floating point constant is equal to 
 * a specified double-precision value. This gets us everything we need, since we don't
 * need to compare against more than a few values, all of which are exact as floats,
 * doubles or quads.
 */

static BOOL is_floating_equal(simpnode k, double d)
{
   TCON kval,dval;
   INT64 eqval;
   TYPE_ID ty;

   if (!SIMP_Is_Constant(k)) return (FALSE);
   ty = SIMPNODE_rtype(k);
   if (SIMP_IS_TYPE_INTEGRAL(ty) || SIMP_IS_TYPE_COMPLEX(ty)) return (FALSE);

#ifdef KEY
   if (MTYPE_is_str (ty)) return FALSE;
#endif
   
   kval = SIMP_Flt_ConstVal(k);
   
   switch (ty) {
    case MTYPE_F4:
    case MTYPE_F8:
    case MTYPE_F10:
      return (d == Targ_To_Host_Float(kval));

    case MTYPE_FQ:
      /* Tricky: we must convert d to a quad */
      dval = Host_To_Targ_Float(MTYPE_FQ, d);
      eqval = Targ_To_Host(Targ_WhirlOp(OPC_I4FQEQ,dval,kval,NULL));
      return (eqval != 0);
   }
   return (FALSE);
}

/*
 * This utility determines if a constant is equal to 
 * a specified double-precision value. This gets us everything we need, since we don't
 * need to compare against more than a few values, all of which are exact as floats,
 * doubles or quads. This routine also accept integers types, but the integer value must 
 * be less than 2**52. This is not checked. 
 */
static BOOL is_numeric_equal(simpnode k, double d)
{
   INT64 c;
   UINT64 uc;
   TYPE_ID ty;

   if (!SIMP_Is_Constant(k)) return (FALSE);
   ty = SIMPNODE_rtype(k);
   if (SIMP_IS_TYPE_COMPLEX(ty)) return (FALSE);
#ifdef KEY
   if (MTYPE_is_str (ty)) return FALSE;
#endif
   if (SIMP_IS_TYPE_FLOATING(ty)) return (is_floating_equal(k,d));
   if (SIMP_IS_TYPE_UNSIGNED(ty)) {
      uc = (UINT64) SIMP_Int_ConstVal(k);
      return (uc == d);
   } else if (SIMP_IS_TYPE_INTEGRAL(ty)) {
      c = SIMP_Int_ConstVal(k);
      return (c == d);
   }
   return (FALSE);
}

#ifdef KEY
// This utility determines if a constant is equal to a complex value.
// Should be called only for the values of 1, 0, -1.
static BOOL is_complex_equal (simpnode k, double d)
{
    Is_True (SIMPNODE_operator (k) == OPR_CONST, ("is_complex_equal: Invalid operator"));
    Is_True (d==1.0 || d==0.0 || d==-1.0, ("Unsupported complex constant value"));

    TCON val = ST_tcon_val (SIMPNODE_st(k));
    
    TCON real_part = Extract_Complex_Real (val);

    TYPE_ID type = TCON_ty (real_part);
    switch (type)
    {
      case MTYPE_F4:
      case MTYPE_F8:
        if (d != Targ_To_Host_Float (real_part)) return FALSE;
        break;
      default:
        return FALSE;
    }

    TCON imag_part = Extract_Complex_Imag (val);
    type = TCON_ty (imag_part);
    switch (type)
    {
      case MTYPE_F4:
      case MTYPE_F8:
        if (Targ_To_Host_Float (imag_part) == 0.0) return TRUE;
        break;
      default:
        return FALSE;
    }

    return FALSE;
}
#endif // KEY

/*================================================================
 * This utility routine takes an LDA node, sets up base and offset, and marks a couple of 
 * flags. See PV 619250 for more explanation of why the various return values are 
 * what they are. 
 */

#define LDA_CANNOT_BE_ZERO 1       // Address of symbol cannot be 0
#define LDA_EQUIV_POSSIBLE 2       // Might have same address as another symbol
#define LDA_UNTESTABLE 4           // Can't figure out anything about the symbol
typedef INT LDA_FLAGS;

static LDA_FLAGS get_lda_info(simpnode lda, INT64& offset, ST_IDX& base_sym_idx)
{
   ST_IDX base_idx=SIMPNODE_st_idx(lda);
   ST *base;
   LDA_FLAGS r = 0;
   
   base = ST_ptr(base_idx);
   offset = SIMPNODE_lda_offset(lda);

   while ( base && (ST_sclass(base) != SCLASS_TEXT) && (ST_base(base) != base)) { 
      offset += ST_ofst(base);
      base = ST_base(base); 
   }
   
   /* Exit if the base index isn't set properly. This could happen for
    * a number of legitimate reasons
    */
   if (!base) {
      return (LDA_UNTESTABLE | LDA_CANNOT_BE_ZERO | LDA_EQUIV_POSSIBLE);
   }
   
   offset += ST_ofst(base);
   base_sym_idx = ST_st_idx(*base);
   
   if ((ST_export(base) != EXPORT_PREEMPTIBLE) &&
       (ST_export(base) != EXPORT_OPTIONAL) &&
       (ST_sclass(base) != SCLASS_FORMAL_REF) &&
       (ST_sclass(base) != SCLASS_EXTERN) &&
       !ST_is_weak_symbol(base)) {
      r |= LDA_CANNOT_BE_ZERO;
   }
   
   if (ST_export(base) == EXPORT_PREEMPTIBLE || 
       ST_sclass(base) == SCLASS_EXTERN ||
       ST_sclass(base) == SCLASS_COMMON || 
       ST_is_weak_symbol(base)) {
      r |= LDA_EQUIV_POSSIBLE;
   }

   return (r);
}

typedef enum {
   NO,YES,MAYBE
} YESNOMAYBE;


static YESNOMAYBE LDA_Equal_Address (simpnode lda_x,simpnode lda_y) 
{
   const ST *x;
   const ST *y;
   LDA_FLAGS fx,fy;
   INT64 x_offset,y_offset;
   ST_IDX x_base_sym_idx,y_base_sym_idx;
   
   fx = get_lda_info(lda_x, x_offset, x_base_sym_idx);
   fy = get_lda_info(lda_y, y_offset, y_base_sym_idx);
   
   if (((fx|fy)&LDA_UNTESTABLE)!=0) return MAYBE;
   if (x_base_sym_idx == y_base_sym_idx) {
      if (x_offset == y_offset) return YES;
      return NO;
   }
   // If the offsets differ, it's really not safe to say anything
   if (x_offset != y_offset) return MAYBE;
      
   x = ST_ptr(x_base_sym_idx);
   y = ST_ptr(y_base_sym_idx);
   
   // find out if x is weak or may become weak after symbol resolution
   // likewise for y.
   BOOL x_maybe_weak = (ST_is_weak_symbol (x) ||
			(ST_sclass (x) == SCLASS_EXTERN && 
			 ST_export (x) == EXPORT_PREEMPTIBLE));
   BOOL y_maybe_weak = (ST_is_weak_symbol (y) ||
			(ST_sclass (y) == SCLASS_EXTERN && 
			 ST_export (y) == EXPORT_PREEMPTIBLE));
   
   // check if x and y maybe aliased:
   // the first condition checks if x points to y
   // the second condition checks if y points to x
   // the third condition checks, after symbol resolution, if x and y
   // points to each other, of if they both point to the same variable z
   BOOL maybe_aliased = 
      (ST_is_weak_symbol (x) && ST_strong_idx (*x) == ST_st_idx (y)) ||
      (ST_is_weak_symbol (y) && ST_strong_idx (*y) == ST_st_idx (x)) ||
      (ST_export (x) == EXPORT_OPTIONAL) ||
      (ST_export (y) == EXPORT_OPTIONAL) ||
      (x_maybe_weak && y_maybe_weak);
   
   if (!maybe_aliased) return NO;
   else return MAYBE;
}


/* ====================================================================
 *
 * INT32 SIMPNODE_Simp_Compare_Trees(simpnode t1, simpnode t2)
 *
 * Compares two WHIRL trees. This routine returns 0 if the trees are the
 * same. Otherwise it imposes a somewhat arbitrary, but consistent ordering.
 * If t1 is "less than" t2, this routine returns -1, otherwise it returns 1.
 *
 * The order is computed as follows:
 * 1) If the root opcodes are different, the numerical value of the opcode
 *    determines the order.
 * 2) Compare children. The result will be the result of the first non-zero
 *    comparison. 
 * 3) Special cases:
 *    a) INTCONST compares the signed value of SIMP_Int_ConstVal.
 *    b) ILOAD, ILOADX, LDA, MLOAD compare the offset first.
 *    c) IDNAME and LDID compare offsets and then symbol table entries
 *    d) CONST compares symbol table entries
 *    e) ARRAY compares num_dim, element_size and the children
 *    f) CVTL compares cvtl_bits first.
 * ====================================================================
 */

INT32 SIMPNODE_Simp_Compare_Trees(simpnode t1, simpnode t2)
{
   INT32  i;
   INT32  rv;
   
   /* Trivial comparison, shouldn't ever happen for WHIRL nodes, but
    * will happen for the WOPT IR.
    */
   if (t1 == t2) return (0);

   /* First compare opcodes */
   if (SIMPNODE_opcode(t1) < SIMPNODE_opcode(t2))
     return (-1);
   else if (SIMPNODE_opcode(t1) > SIMPNODE_opcode(t2))
     return (1);
   
   /* Opcodes are the same, switch on operator class */
   switch (SIMPNODE_operator(t1)) {
    case OPR_INTCONST:
      if (SIMPNODE_const_val(t1) < SIMPNODE_const_val(t2)) return(-1);
      if (SIMPNODE_const_val(t1) > SIMPNODE_const_val(t2)) return(1);
      return (0);

    case OPR_CVTL:
      if (SIMPNODE_cvtl_bits(t1) < SIMPNODE_cvtl_bits(t2)) return (-1);
      if (SIMPNODE_cvtl_bits(t1) > SIMPNODE_cvtl_bits(t2)) return (1);
      return (SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(t1),
					  SIMPNODE_kid0(t2)));
      

    case OPR_EXTRACT_BITS:
      if (SIMPNODE_op_bit_offset(t1) < SIMPNODE_op_bit_offset(t2)) return (-1);
      if (SIMPNODE_op_bit_offset(t1) > SIMPNODE_op_bit_offset(t2)) return (1);
      if (SIMPNODE_op_bit_size(t1) < SIMPNODE_op_bit_size(t2)) return (-1);
      if (SIMPNODE_op_bit_size(t1) > SIMPNODE_op_bit_size(t2)) return (1);
      return (SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(t1),
					  SIMPNODE_kid0(t2)));


    case OPR_COMPOSE_BITS:
      if (SIMPNODE_op_bit_offset(t1) < SIMPNODE_op_bit_offset(t2)) return (-1);
      if (SIMPNODE_op_bit_offset(t1) > SIMPNODE_op_bit_offset(t2)) return (1);
      if (SIMPNODE_op_bit_size(t1) < SIMPNODE_op_bit_size(t2)) return (-1);
      if (SIMPNODE_op_bit_size(t1) > SIMPNODE_op_bit_size(t2)) return (1);
      rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(t1), SIMPNODE_kid0(t2));
      if (rv == 0) {
	rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid1(t1), SIMPNODE_kid1(t2));
      }
      return rv;


    case OPR_CONST:
      return SIMPNODE_Compare_Symbols(t1,t2);
      
    case OPR_ILOAD:
      if (SIMPNODE_load_offset(t1) < SIMPNODE_load_offset(t2)) return(-1);
      if (SIMPNODE_load_offset(t1) > SIMPNODE_load_offset(t2)) return(1);
      if (SIMPNODE_desc(t1) == MTYPE_BS || SIMPNODE_desc(t2) == MTYPE_BS) {
	if (SIMPNODE_i_field_id(t1) < SIMPNODE_i_field_id(t2)) return(-1);
	if (SIMPNODE_i_field_id(t1) > SIMPNODE_i_field_id(t2)) return(1);
      }
      return (SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(t1),
					  SIMPNODE_kid0(t2)));

    case OPR_ILDBITS:
      if (SIMPNODE_load_offset(t1) < SIMPNODE_load_offset(t2)) return(-1);
      if (SIMPNODE_load_offset(t1) > SIMPNODE_load_offset(t2)) return(1);
      if (SIMPNODE_i_bit_offset(t1) < SIMPNODE_i_bit_offset(t2)) return(-1);
      if (SIMPNODE_i_bit_offset(t1) > SIMPNODE_i_bit_offset(t2)) return(1);
      return (SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(t1),
					  SIMPNODE_kid0(t2)));

    case OPR_MLOAD:  /* Same procedure as ILOADX, though the children */
    case OPR_ILOADX:  /* have very different meanings */    
      if (SIMPNODE_load_offset(t1) < SIMPNODE_load_offset(t2)) return(-1);
      if (SIMPNODE_load_offset(t1) > SIMPNODE_load_offset(t2)) return(1);
      rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(t1),SIMPNODE_kid0(t2));
      if (rv != 0) return (rv);
      return (SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid1(t1),
					  SIMPNODE_kid1(t2)));

    case OPR_LDID:
      if (SIMPNODE_load_offset(t1) < SIMPNODE_load_offset(t2)) return(-1);
      if (SIMPNODE_load_offset(t1) > SIMPNODE_load_offset(t2)) return(1);
      if (SIMPNODE_desc(t1) == MTYPE_BS || SIMPNODE_desc(t2) == MTYPE_BS) {
	if (SIMPNODE_field_id(t1) < SIMPNODE_field_id(t2)) return(-1);
	if (SIMPNODE_field_id(t1) > SIMPNODE_field_id(t2)) return(1);
      }
      return SIMPNODE_Compare_Symbols(t1,t2);

    case OPR_LDBITS:
      if (SIMPNODE_load_offset(t1) < SIMPNODE_load_offset(t2)) return(-1);
      if (SIMPNODE_load_offset(t1) > SIMPNODE_load_offset(t2)) return(1);
      if (SIMPNODE_bit_offset(t1) < SIMPNODE_bit_offset(t2)) return(-1);
      if (SIMPNODE_bit_offset(t1) > SIMPNODE_bit_offset(t2)) return(1);
      return SIMPNODE_Compare_Symbols(t1,t2);
      
    case OPR_IDNAME:
      if (SIMPNODE_idname_offset(t1) < SIMPNODE_idname_offset(t2)) return(-1);
      if (SIMPNODE_idname_offset(t1) > SIMPNODE_idname_offset(t2)) return(1);
      return SIMPNODE_Compare_Symbols(t1,t2);

    case OPR_LDA:
      if (SIMPNODE_lda_offset(t1) < SIMPNODE_lda_offset(t2)) return(-1);
      if (SIMPNODE_lda_offset(t1) > SIMPNODE_lda_offset(t2)) return(1);
      return SIMPNODE_Compare_Symbols(t1,t2);

    case OPR_ARRAY:
      if (SIMPNODE_num_dim(t1) < SIMPNODE_num_dim(t2)) return (-1);
      if (SIMPNODE_num_dim(t1) > SIMPNODE_num_dim(t2)) return (1);
      if (SIMPNODE_element_size(t1) < SIMPNODE_element_size(t2)) return (-1);
      if (SIMPNODE_element_size(t1) > SIMPNODE_element_size(t2)) return (1);
      /* Compare bases */
      rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_array_base(t1),
				       SIMPNODE_array_base(t2));
      if (rv != 0) return (rv);
      
      /* Compare array_index and array_dim */
      for (i=0; i < SIMPNODE_num_dim(t1); i++) {
	 rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_array_index(t1,i),
					  SIMPNODE_array_index(t2,i));
	 if (rv != 0) return (rv);
	 rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_array_dim(t1,i),
					  SIMPNODE_array_dim(t2,i));
	 if (rv != 0) return (rv);
      }
      /* everything compares */
      return (0);

    case OPR_INTRINSIC_OP:
      if (SIMPNODE_intrinsic(t1) < SIMPNODE_intrinsic(t2)) return (-1);
      if (SIMPNODE_intrinsic(t1) > SIMPNODE_intrinsic(t2)) return (1);
      if (SIMPNODE_kid_count(t1) < SIMPNODE_kid_count(t2)) return (-1);
      if (SIMPNODE_kid_count(t1) > SIMPNODE_kid_count(t2)) return (1);
      
      for (i=0; i<SIMPNODE_kid_count(t1); i++) {
	rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid(t1,i),
					 SIMPNODE_kid(t2,i));
	if (rv != 0) return (rv);
      }
      return (0);

    case OPR_COMMA:
    case OPR_RCOMMA:
    case OPR_CSELECT:

      return ((INTPS)t1 - (INTPS)t2);

#ifdef KEY
    case OPR_PURE_CALL_OP:
      if (SIMPNODE_st_idx(t1) < SIMPNODE_st_idx(t2)) return (-1);
      if (SIMPNODE_st_idx(t1) > SIMPNODE_st_idx(t2)) return (1);
      if (SIMPNODE_kid_count(t1) < SIMPNODE_kid_count(t2)) return (-1);
      if (SIMPNODE_kid_count(t1) > SIMPNODE_kid_count(t2)) return (1);

      for (i=0; i<SIMPNODE_kid_count(t1); i++) {
	rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid(t1,i),
					 SIMPNODE_kid(t2,i));
	if (rv != 0) return (rv);
      }
      return (0);
#endif

    default:
       if (OPCODE_is_expression(SIMPNODE_opcode(t1))) {
	  for (i=0; i<SIMPNODE_kid_count(t1); i++) {
	     rv = SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid(t1,i),
					      SIMPNODE_kid(t2,i));
	     if (rv != 0) return (rv);
	  }
	  return (0);
       } else {
	  /* Non-expression opcode. Return arbitrary */
	  return ((INTPS)t1 - (INTPS)t2);
       }
   }
}



/* ====================================================================
 *
 * static OPCODE is_logop(OPCODE opc)
 *
 * If opc is an operator logop (LIOR, LAND) return it. If it's a relop,
 * return the inverse. If it's not one of these, return OPCODE_UNKNOWN.
 * ====================================================================
 */

static OPCODE is_logop(OPCODE opc)
{
   OPERATOR op;
   
   op = OPCODE_operator(opc);
   if (op == OPR_LAND || op == OPR_LIOR
#ifdef KEY // bug 9878
       || op == OPR_LNOT
#endif
      ) return (opc);
   return OPCODE_UNKNOWN;
}


/* ====================================================================
 *
 * static BOOL is_ok_to_reassociate(OPCODE opc)
 *
 * If opc is an operator which can be reassociated under the current set 
 * of compile time flags, return TRUE. The current operators which
 * can be reassociated are 
 * &, &&, |, ||, ^, MAX, MIN under all circumstances, all types
 * +, * for floating point types if roundoff is 2 or greater
 * +, * for all integer types.
 * ====================================================================
 */

static BOOL is_ok_to_reassociate(OPCODE opc)
{
   switch (OPCODE_operator(opc)) {
    case OPR_MAX:
    case OPR_MIN:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_LAND:
    case OPR_LIOR:
      return (TRUE);

    case OPR_ADD:
    case OPR_MPY:
      if (SIMP_IS_TYPE_INTEGRAL(OPCODE_rtype(opc))) {
	 return (TRUE);
#ifdef __FP_REASSOCIATE__
      } else if (SIMP_IS_TYPE_FLOATING(OPCODE_rtype(opc))) {
	 return (Enable_Cfold_Reassociate);
#endif
      } else {
	 /* Don't know what to do */
	 return (FALSE);
      }
      
    default:
      return (FALSE);
   }
}


/* ====================================================================
 *
 * OPCODE get_inverse_relop( OPCODE opc )
 *
 * For a relational operator, return the inverse operator. Return 0
 * for non-relational operators.
 *
 * ====================================================================
 */

static OPCODE get_inverse_relop( OPCODE opc )
{
   OPCODE iopc;
   OPERATOR iopr;

   switch (OPCODE_operator(opc)) {
   case OPR_LT: iopr = OPR_GE; break;
   case OPR_LE: iopr = OPR_GT; break;
   case OPR_GT: iopr = OPR_LE; break;
   case OPR_GE: iopr = OPR_LT; break;
   case OPR_EQ: iopr = OPR_NE; break;
   case OPR_NE: iopr = OPR_EQ; break;
   default: return OPCODE_UNKNOWN;
   }

   iopc = OPCODE_make_op(iopr, OPCODE_rtype(opc), OPCODE_desc(opc));
   return iopc;
}

/*
 * This routine returns the "real" type of an operand. The "real" type
 * differs from the WHIRL type for loads of the short types.
 */

static TYPE_ID get_value_type(simpnode k0)
{
   TYPE_ID ty;
   OPERATOR op;
   op = SIMPNODE_operator(k0);
   if (op == OPR_ILOAD || op == OPR_LDID || op == OPR_LDBITS) {
      ty = OPCODE_desc(SIMPNODE_opcode(k0));
   } else {
      ty = SIMPNODE_rtype(k0);
   }
   return (ty);
}

/*
 * This utility routine returns NULL if the trees are not comparable, 
 * otherwise it returns an INTCONST representing the difference of the two trees
 */
static simpnode simp_diff_value(simpnode k0, simpnode k1, BOOL negate_result)
{
   INT64 resultval;
   simpnode r;
   TYPE_ID ty;
   
   r = NULL;
   /* Special case for difference of two LDA's */
   if (SIMPNODE_operator(k0) == OPR_LDA &&
       SIMPNODE_operator(k1) == OPR_LDA &&
       SIMPNODE_Compare_Symbols(k0,k1) == 0) {
      resultval = SIMPNODE_lda_offset(k0) - SIMPNODE_lda_offset(k1);
      if (negate_result) resultval = -resultval;
      if (SIMPNODE_rtype(k0) == MTYPE_U4) {
	 r = SIMP_INTCONST(MTYPE_I4,resultval);
      } else {
	 r = SIMP_INTCONST(MTYPE_I8,resultval);
      }
   } else if (SIMPNODE_Simp_Compare_Trees(k0,k1) == 0) {
      ty = SIMPNODE_rtype(k0);
      if (SIMP_IS_TYPE_FLOATING(ty)) {
         r = SIMP_FLOATCONST(ty,0.0);
      } else {
         r = SIMP_INTCONST(ty,0);
      }
   }
   return (r);
}

#define FACTOR_11 1
#define FACTOR_12 2
#define FACTOR_21 4
#define FACTOR_22 8
#define FACTOR_ALL (FACTOR_11 | FACTOR_12 | FACTOR_21 | FACTOR_22)
#define FACTOR_OK(flag,type) ((flag&type)!=0)

/*
 * Factor flag is a flag which tells the simp_factor routine which operands to 
 * check for equivalence. For example, FACTOR_12 says that it is OK to factor 
 * something like (i op j) op (k op i).
 */

static simpnode simp_factor (simpnode k0, simpnode k1, OPERATOR op1,
			     OPCODE opc2, TYPE_ID ty, INT32 flag)
     /*
	This routine handles the generalized factor transformation
	(i op1 j) op2 (i op2 k) -> i op1 (j op2 k).
	
	k0, k1 - The two operands
	op1 - the multiply-like operator
	opc2 - the add-like opcode (to save having to create it for the types
	ty - the type of the result
	
	returns either a simplified tree or NULL
	
	*/
{
   simpnode r = NULL;
   OPCODE opc1;
   
   if (!Enable_Cfold_Aggressive ||
       (!Enable_Cfold_Reassociate && SIMP_IS_TYPE_FLOATING(ty)))  return (r);
   opc1 = OPC_FROM_OPR(op1,ty);
   if (SIMPNODE_opcode(k0) == opc1 && 
       SIMPNODE_opcode(k1) == opc1) {
      
      /* Compare the trees, and factor */
      if (FACTOR_OK(flag,FACTOR_11) &&
	  SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),SIMPNODE_kid0(k1))==0) {
	 SHOW_RULE("z*x op z*y");
	 r = SIMPNODE_SimpCreateExp2(opc1,SIMPNODE_kid0(k0),
				     SIMPNODE_SimpCreateExp2(opc2,SIMPNODE_kid1(k0),
							     SIMPNODE_kid1(k1)));
	 SIMP_DELETE_TREE(SIMPNODE_kid0(k1)); 
	 SIMP_DELETE(k0);
	 SIMP_DELETE(k1);
	 
      } else if (FACTOR_OK(flag,FACTOR_22) &&
		 SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid1(k0),SIMPNODE_kid1(k1))==0) {
	 SHOW_RULE("x*z op y*z");
	 r = SIMPNODE_SimpCreateExp2(opc1, SIMPNODE_SimpCreateExp2(opc2,SIMPNODE_kid0(k0),
								   SIMPNODE_kid0(k1)),
				     SIMPNODE_kid1(k0));
	 SIMP_DELETE_TREE(SIMPNODE_kid1(k1)); 
	 SIMP_DELETE(k0);
	 SIMP_DELETE(k1);
	 
      } else if (FACTOR_OK(flag,FACTOR_21) &&
		 SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid1(k0),SIMPNODE_kid0(k1))==0) {
	 SHOW_RULE("x*z op z*y");
	 r = SIMPNODE_SimpCreateExp2(opc1,SIMPNODE_kid1(k0),
				     SIMPNODE_SimpCreateExp2(opc2,SIMPNODE_kid0(k0),
							     SIMPNODE_kid1(k1)));
	 SIMP_DELETE_TREE(SIMPNODE_kid0(k1)); 
	 SIMP_DELETE(k0);
	 SIMP_DELETE(k1);
      } else if (FACTOR_OK(flag,FACTOR_12) &&
		 SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),SIMPNODE_kid1(k1))==0) {
	 SHOW_RULE("z*x op y*z");
	 r = SIMPNODE_SimpCreateExp2(opc1,SIMPNODE_kid0(k0),
				     SIMPNODE_SimpCreateExp2(opc2,SIMPNODE_kid1(k0),
							     SIMPNODE_kid0(k1)));
         SIMP_DELETE_TREE(SIMPNODE_kid1(k1)); 
	 SIMP_DELETE(k0);
	 SIMP_DELETE(k1);
      }
   }
   return (r);

}


/* Make the identity operator for an operator/type pair */
static simpnode make_identity(OPERATOR opr,TYPE_ID ty)
{
   simpnode r = NULL;
   switch (opr) {
    case OPR_ADD:
    case OPR_SUB:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_LIOR:
      if (SIMP_IS_TYPE_FLOATING(ty)) {
         r = SIMP_FLOATCONST(ty,0.0);
      } else {
         r = SIMP_INTCONST(ty,0);
      }
      break;

    case OPR_MPY:
    case OPR_LAND:
      if (SIMP_IS_TYPE_FLOATING(ty)) {
         r = SIMP_FLOATCONST(ty,1.0);
      } else {
         r = SIMP_INTCONST(ty,1);
      }
      break;
    case OPR_BAND:
      r = SIMP_INTCONST(ty,-1);
      break;
    default:
      #pragma mips_frequency_hint NEVER
      FmtAssert(FALSE,
		("unknown identity value requested in simplifier"));
      /*NOTREACHED*/
   }
   return (r);
}

static simpnode simp_factor_idty (simpnode k0, simpnode k1, OPERATOR op1,
			     OPCODE opc2, TYPE_ID ty, INT32 const_only)
     /*
	This routine handles the generalized factor transformation
	(i op1 j) op2 i -> i op1 (j op2 k).
	
	k0, k1 - The two operands
	op1 - the multiply-like operator
	opc2 - the add-like opcode (to save having to create it for the types
	ty - the type of the result
	const_only - indicates factor only if the non-common term is a constant
	
	returns either a simplified tree or NULL
	
	*/
{
   simpnode r = NULL;
   OPCODE opc1;

   if (!Enable_Cfold_Aggressive ||
       (!Enable_Cfold_Reassociate && SIMP_IS_TYPE_FLOATING(ty)))  return (r);

   opc1 = OPC_FROM_OPR(op1,ty);
   if (SIMPNODE_opcode(k0) == opc1) {
      /* Compare the trees, and factor */
      if (SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),k1)==0) {
	 if (!const_only || SIMP_Is_Constant(SIMPNODE_kid1(k0))) {
	    SHOW_RULE("z*x op z");
	    r = SIMPNODE_SimpCreateExp2(opc1,SIMPNODE_kid0(k0),
					SIMPNODE_SimpCreateExp2(opc2,SIMPNODE_kid1(k0),
								make_identity(op1,ty)));
	    SIMP_DELETE(k0);
	    SIMP_DELETE_TREE(k1);
	 }
      } else if (SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid1(k0),k1)==0) {
	 if (!const_only || SIMP_Is_Constant(SIMPNODE_kid0(k0))) {
	    SHOW_RULE("x*z op z");
	    r = SIMPNODE_SimpCreateExp2(opc1, SIMPNODE_SimpCreateExp2(opc2,SIMPNODE_kid0(k0),
								      make_identity(op1,ty)),
					SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	    SIMP_DELETE_TREE(k1);
	 }
      }
   } else if (SIMPNODE_opcode(k1) == opc1) {
      if (SIMPNODE_Simp_Compare_Trees(k0,SIMPNODE_kid0(k1))==0) {
	 if (!const_only || SIMP_Is_Constant(SIMPNODE_kid1(k1))) {
	    SHOW_RULE("z op z*y");
	    r = SIMPNODE_SimpCreateExp2(opc1,SIMPNODE_kid0(k1),
					SIMPNODE_SimpCreateExp2(opc2,make_identity(op1,ty),
								SIMPNODE_kid1(k1)));
	    
	    SIMP_DELETE_TREE(k0);
	    SIMP_DELETE(k1);
	 }
      } else if (SIMPNODE_Simp_Compare_Trees(k0,SIMPNODE_kid1(k1))==0) {
	 if (!const_only || SIMP_Is_Constant(SIMPNODE_kid0(k1))) {
	    SHOW_RULE("z op y*z");
	    r = SIMPNODE_SimpCreateExp2(opc1,SIMPNODE_SimpCreateExp2(opc2,make_identity(op1,ty),
								     SIMPNODE_kid0(k1)),
					SIMPNODE_kid1(k1));
	    SIMP_DELETE_TREE(k0);
	    SIMP_DELETE(k1);
	 }
      }
   }
   return (r);
}


/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/*
 *  Here follow all the simplification routines for unary operations
 *  These are all of the form 
 *
 *  static simpnode  simp_thing( OPCODE opc, simpnode k0)
 *        opc - the opcode of the node we wish to build/simplify
 *        k0 - the child of the operation
 * The return value is either a simplified WHIRL node or NULL if no simplification
 * was done.
 */
/**************************************************************************/
/**************************************************************************/
/**************************************************************************/

/*------------------------------------------------ 
   Simplifications for ABS:
   ABS(ABS(x))     ABS(X)
   ABS(-x)         ABS(x)
   ABS(CVT)        CVT(ABS) 
-------------------------------------------------*/

/* opc, k1, k0const, k1const not used, compiler parses next comment */
/* ARGSUSED */
static simpnode  simp_abs(OPCODE opc, simpnode k0, simpnode k1,
			  BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   OPERATOR opr;
   TYPE_ID  sty,dty;
   OPCODE   cvt_op,abs_op;

   opr = SIMPNODE_operator(k0);
   if (opr == OPR_ABS) {
      SHOW_RULE("ABS(ABS(x)) -> ABS(X)");
      r = k0;
   } else if (opr == OPR_NEG) {
      SHOW_RULE("ABS(-x) -> ABS(x)");     
      r = SIMPNODE_SimpCreateExp1(opc,SIMPNODE_kid0(k0));
      SIMP_DELETE(k0);
   } else if (opr == OPR_CVT) {
      cvt_op = SIMPNODE_opcode(k0);
      sty = OPCODE_desc(cvt_op);
      dty = OPCODE_rtype(cvt_op);
      if (dty == OPCODE_rtype(opc) && 
	  SIMP_IS_TYPE_FLOATING(sty) && !SIMP_IS_TYPE_COMPLEX(sty)) {
	 SHOW_RULE("ABS(CVT) -> CVT(ABS)");
	 abs_op = OPC_FROM_OPR(OPR_ABS,sty);
	 r = SIMPNODE_SimpCreateExp1(abs_op,SIMPNODE_kid0(k0));
	 r = SIMPNODE_SimpCreateExp1(cvt_op,r);
	 SIMP_DELETE(k0);
      }
   }

   return (r);
}


/*------------------------------------------------ 
   Simplifications for ~ and !:
   
   ! ! j			j
   ~ ~ j			j
   ! <comp>                     inverse comp, IEEE_comparisons false for floating point
   ~(a | b)                     a nor b
   ~(a nor b)                   a or b

-------------------------------------------------*/
/* k1, k0const, k1const not used, compiler parses next comment */
/* ARGSUSED */
static simpnode  simp_not(OPCODE opc, simpnode k0, simpnode k1,
			  BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   OPCODE  inverse_relop;
   
   if (opc == SIMPNODE_opcode(k0)) {
      SHOW_RULE("~ ~ j -> j");
      r = SIMPNODE_kid0(k0);
      SIMP_DELETE(k0);
   } else if (SIMPNODE_operator(k0) == OPR_BIOR && OPCODE_operator(opc) == OPR_BNOT &&
	      ARCH_generate_nor) {
      SHOW_RULE("~(a | b) -> a nor b");
      r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BNOR,OPCODE_rtype(opc)),
				  SIMPNODE_kid0(k0),SIMPNODE_kid1(k0));
      SIMP_DELETE(k0);
   } else if (SIMPNODE_operator(k0) == OPR_BNOR && OPCODE_operator(opc) == OPR_BNOT) {
      SHOW_RULE("~(a nor b) -> a | b");
      r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BIOR,OPCODE_rtype(opc)),
				  SIMPNODE_kid0(k0),SIMPNODE_kid1(k0));
      SIMP_DELETE(k0);
   } else {
      inverse_relop = get_inverse_relop(SIMPNODE_opcode(k0));
// >> WHIRL 0.30: replaced OPC_LNOT by OPC_BLNOT or OPC_I4LNOT
// TODO WHIRL 0.30: get rid of OPC_I4LNOT
      if ((opc == OPC_I4LNOT || opc == OPC_BLNOT) && inverse_relop != 0 &&
// << WHIRL 0.30: replaced OPC_LNOT by OPC_BLNOT or OPC_I4LNOT
	  (! (Force_IEEE_Comparisons && 
	      SIMP_IS_TYPE_FLOATING(OPCODE_desc(inverse_relop)))))
	{
	   SHOW_RULE("! <relop>");
	   inverse_relop = OPCODE_make_op(OPCODE_operator(inverse_relop),
					  OPCODE_rtype(opc),
					  OPCODE_desc(inverse_relop));
	   r = SIMPNODE_SimpCreateExp2(inverse_relop,SIMPNODE_kid0(k0),SIMPNODE_kid1(k0));
	   SIMP_DELETE(k0);
	}
   }

   return (r);
}


/*------------------------------------------------ 
   Simplifications for unary - :

   -(-x)			x
   -(x-y)			y-x
   -(x*c1)                      x*-c
   -(x/c1)                      x/-c
   -(c1/x)                      -c/x
-------------------------------------------------*/
/* k1, k0const, k1const not used, compiler parses next comment */
/* ARGSUSED */
static simpnode  simp_neg(OPCODE opc, simpnode k0, simpnode k1,
			  BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   TYPE_ID ty;
   
   ty = SIMPNODE_rtype(k0);
   
   if (opc == SIMPNODE_opcode(k0)) {
      SHOW_RULE("-(-x)");
      r = SIMPNODE_kid0(k0);
      SIMP_DELETE(k0);

   } else if (SIMPNODE_operator(k0) == OPR_SUB) {
      SHOW_RULE("-(x-y)");
      r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),SIMPNODE_kid1(k0),
				  SIMPNODE_kid0(k0));
      SIMP_DELETE(k0);
   } else if (((SIMPNODE_operator(k0) == OPR_MPY) || (SIMPNODE_operator(k0) == OPR_DIV)) &&
	      SIMP_Is_Constant(SIMPNODE_kid1(k0)) && !SIMP_IS_TYPE_UNSIGNED(ty)) {
      SHOW_RULE(" - x*/c");
      ty = SIMPNODE_rtype(SIMPNODE_kid1(k0));
#ifdef KEY
      // Bug 1169
      if (SIMP_IS_TYPE_UNSIGNED(ty)) ty = MTYPE_complement(ty);
#endif
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),SIMPNODE_kid1(k0));
      r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),SIMPNODE_kid0(k0),r);
      SIMP_DELETE(k0);
   } else if (SIMPNODE_operator(k0) == OPR_DIV &&
	      SIMP_Is_Constant(SIMPNODE_kid0(k0)) && !SIMP_IS_TYPE_UNSIGNED(ty)) {
      SHOW_RULE(" - c/x");
      ty = SIMPNODE_rtype(SIMPNODE_kid0(k0));
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),SIMPNODE_kid0(k0));
      r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),r,SIMPNODE_kid1(k0));
      SIMP_DELETE(k0);
   }
				  
   
   return (r);
}

/*------------------------------------------------ 
   Simplifications for sqrt, recip and rsqrt:

   SQRT(RECIP(x))     RSQRT(x)   
   RECIP(SQRT(x))     RSQRT(x)   
   RECIP(RECIP(x))    x          Roundoff:ROUNDOFF_SIMPLE needed
   RECIP(RSQRT(x))    SQRT(x)
   RSQRT(RECIP(x))    SQRT(x)

   // TARG_X8664
   SQRT(x)            x * RSQRT(x)
   RECIP(x*RSQRT(x))  RSQRT(x)

All of these require Rsqrt_Allowed to generate RSQRT,

-------------------------------------------------*/
/* k1, k0const, k1const not used, compiler parses next comment */
/* ARGSUSED */
static simpnode  simp_recip(OPCODE opc, simpnode k0, simpnode k1,
			  BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   OPERATOR op,child_op;
   TYPE_ID ty;
   
   op = OPCODE_operator(opc);
   child_op = SIMPNODE_operator(k0);
   ty = OPCODE_rtype(opc);

   if (op == OPR_RECIP) {
      switch (child_op) {
#ifdef TARG_X8664
      case OPR_MPY:
	SHOW_RULE("RECIP(x*RSQRT(x)) RSQRT(x)");
	    if (OPCODE_is_load(SIMPNODE_opcode(SIMPNODE_kid0(k0))) &&
	    OPCODE_operator(SIMPNODE_opcode(SIMPNODE_kid1(k0))) == OPR_RSQRT &&
	    SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0), 
					SIMPNODE_kid0(SIMPNODE_kid1(k0)))==0) {
	  r = SIMPNODE_kid1(k0);
	  SIMP_DELETE(SIMPNODE_kid0(k0));
	}
	break;
#endif
       case OPR_RECIP:
	 SHOW_RULE("RECIP(RECIP(X))");
	 if (Roundoff_Level >= ROUNDOFF_SIMPLE) {
	    r = SIMPNODE_kid0(k0);
	    SIMP_DELETE(k0);
	 }
	 break;

       case OPR_SQRT:
	 SHOW_RULE(" RECIP(SQRT(x))     RSQRT(x) ");
#ifdef TARG_X8664
	 if (Rsqrt_Allowed >= 1 &&
	     // x86-64 rsqrt supports single-precision only.
	     (ty == MTYPE_F4 || ty == MTYPE_V16F4))
#else	    
	 if (Rsqrt_Allowed)
#endif
	 {
	    r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_RSQRT,ty),SIMPNODE_kid0(k0));
	    SIMP_DELETE(k0);
	 }
	 break;

       case OPR_RSQRT:
#ifndef TARG_X8664 
	 // RESQRT + RECIP is faster than SQRT on Opteron
	 // and we already generated RSQRT and RECIP (-OPT:rsqrt=on:recip=on) 
	 // then why delete it?
	 SHOW_RULE(" RECIP(RSQRT(x))    SQRT(x) ");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_SQRT,ty),SIMPNODE_kid0(k0));
	 SIMP_DELETE(k0);
#endif
	 break;

       default:
	 break;
      }
   } else if (op == OPR_SQRT && child_op == OPR_RECIP && 
#ifdef TARG_X8664
	      Rsqrt_Allowed == 2 &&
	      // x86-64 rsqrt supports single-precision only.
	      (ty == MTYPE_F4 || ty == MTYPE_V16F4)
#else
	      Rsqrt_Allowed
#endif
	      ) {
      SHOW_RULE(" SQRT(RECIP(x))     RSQRT(x)   ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_RSQRT,ty),SIMPNODE_kid0(k0));
      SIMP_DELETE(k0);
   } else if (op == OPR_RSQRT && child_op == OPR_RECIP) {
#ifndef TARG_X8664 
      // RSQRT + RECIP is faster than SQRT on Opteron
      // and we already generated RSQRT and RECIP (-OPT:rsqrt=on:recip=on) 
      // then why delete it?
      SHOW_RULE(" RSQRT(RECIP(x))    SQRT(x) ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_SQRT,ty),SIMPNODE_kid0(k0));
      SIMP_DELETE(k0);
#endif
   }

   return (r);
}


/*------------------------------------------------ 
   Simplifications for conversions:

   REALPART (COMPLEX(a,b)) a
   IMAGPART (COMPLEX(a,b)) b
   t1CVT(t2CVT(a)) 	x, if x is of type t1 and t2 is a more precise type than t1.
   t1CVT(t2CVT(x))      t1CVT(x) if t2CVT(x) can represent all possible values of x
   t1TRUNC(t2CVT(x))    t1TRUNC or t1CVT(x) if t2CVT(x) can represent all possible values of x
   
   t1TAS(a)    a if type of A is t1

-------------------------------------------------*/


/* Helper routine for precise conversions. Returns TRUE if the conversion
 *  from type1 to type 2 will preserve all values of type1.
 */

/* Define a bunch of bit constants. Note that if the definitions of any of the
 *  MTYPES results in a constant > 32, this might not work. Each bit constant
 *  is the list of types this type can be converted into without loss of information
 */
#define B(t) (1<<t)
#ifdef TARG_IA64
#define PRECISE_I1 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)|B(MTYPE_I1)|B(MTYPE_I2)|B(MTYPE_I4)|B(MTYPE_I8)
#define PRECISE_I2 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)|B(MTYPE_I2)|B(MTYPE_I4)|B(MTYPE_I8)
#define PRECISE_I4 B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)|B(MTYPE_I4)|B(MTYPE_I8)
#define PRECISE_I8 B(MTYPE_F10)|B(MTYPE_FQ)|B(MTYPE_I8)
#define PRECISE_U1 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)|B(MTYPE_U1)|B(MTYPE_U2)|B(MTYPE_U4)|B(MTYPE_U8)
#define PRECISE_U2 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)|B(MTYPE_U2)|B(MTYPE_U4)|B(MTYPE_U8)
#define PRECISE_U4 B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)|B(MTYPE_I8)
#define PRECISE_U8 B(MTYPE_F10)|B(MTYPE_FQ)
#define PRECISE_F4 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)
#define PRECISE_F8 B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ)
#define PRECISE_F10 B(MTYPE_F10)|B(MTYPE_FQ)
#else // TARG_IA64
#define PRECISE_I1 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_FQ)|B(MTYPE_I1)|B(MTYPE_I2)|B(MTYPE_I4)|B(MTYPE_I8)
#define PRECISE_I2 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_FQ)|B(MTYPE_I2)|B(MTYPE_I4)|B(MTYPE_I8)
#define PRECISE_I4 B(MTYPE_F8)|B(MTYPE_FQ)|B(MTYPE_I4)|B(MTYPE_I8)
#define PRECISE_I8 B(MTYPE_FQ)|B(MTYPE_I8)
#define PRECISE_U1 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_FQ)|B(MTYPE_U1)|B(MTYPE_U2)|B(MTYPE_U4)|B(MTYPE_U8)
#define PRECISE_U2 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_FQ)|B(MTYPE_U2)|B(MTYPE_U4)|B(MTYPE_U8)
#define PRECISE_U4 B(MTYPE_F8)|B(MTYPE_FQ)|B(MTYPE_I8)
#define PRECISE_U8 B(MTYPE_FQ)
#define PRECISE_F4 B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_FQ)
#define PRECISE_F8 B(MTYPE_F8)|B(MTYPE_FQ)
#endif // TARG_IA64
#define PRECISE_FQ B(MTYPE_FQ)
#define TESTABLE_TYPE (B(MTYPE_U1)|B(MTYPE_U2)|B(MTYPE_U4)|B(MTYPE_U8)|\
		       B(MTYPE_I1)|B(MTYPE_I2)|B(MTYPE_I4)|B(MTYPE_I8)|\
		       B(MTYPE_F4)|B(MTYPE_F8)|B(MTYPE_F10)|B(MTYPE_FQ))
#define TYPEISIN(t,b) ( ((1<<(t)) & (b)) !=0)

static BOOL convert_precise(TYPE_ID t1, TYPE_ID t2)
{
   BOOL r = FALSE;
   INT32 precise_bits;

   if (TYPEISIN(t1,TESTABLE_TYPE) && TYPEISIN(t2,TESTABLE_TYPE)) {
      switch (t1) {
       case MTYPE_U1:
	 precise_bits = PRECISE_U1;
	 break;
       case MTYPE_U2:
	 precise_bits = PRECISE_U2;
	 break;
       case MTYPE_U4:
	 precise_bits = PRECISE_U4;
	 break;
       case MTYPE_U8:
	 precise_bits = PRECISE_U8;
	 break;
       case MTYPE_I1:
	 precise_bits = PRECISE_I1;
	 break;
       case MTYPE_I2:
	 precise_bits = PRECISE_I2;
	 break;
       case MTYPE_I4:
	 precise_bits = PRECISE_I4;
	 break;
       case MTYPE_I8:
	 precise_bits = PRECISE_I8;
	 break;
       case MTYPE_F4:
	 precise_bits = PRECISE_F4;
	 break;
       case MTYPE_F8:
	 precise_bits = PRECISE_F8;
	 break;
#ifdef TARG_IA64
       case MTYPE_F10:
         precise_bits = PRECISE_F10;
         break;
#endif
       case MTYPE_FQ:
	 precise_bits = PRECISE_FQ;
	 break;
       default:
	 precise_bits = 0;
	 break;
      }
      r = TYPEISIN(t2,precise_bits);
   }
   return (r);
}


/* k1, k0const, k1const not used, the compiler parses the next comment */
/* ARGSUSED */
static simpnode  simp_cvt(OPCODE opc, simpnode k0, simpnode k1,
			  BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   simpnode k0k0;
   OPCODE   k0opc,newopc;
   OPERATOR op, k0op;
   TYPE_ID source_type, dest_type, inter_type,source_value_type;

   op = OPCODE_operator(opc);
   k0opc = SIMPNODE_opcode(k0);
   k0op = OPCODE_operator(k0opc);

   if (OPCODE_desc(k0opc) == MTYPE_BS)
     return (NULL);

#ifdef WN_SIMP_WORKING_ON_WHIRL
   /* Several converts are nops and can be removed right away */
   if (!WHIRL_Keep_Cvt_On) {
      if (opc == OPC_I8I4CVT || opc == OPC_U8I4CVT) {
	 SHOW_RULE("Removed CVT");
	 return (k0);
      }
   }

   /* Fix up converts of loads which just change the size of the loaded object */
   /* Note: this is only done for WHIRL simplification */ 
   if (op == OPR_CVT &&
       (k0op == OPR_LDID || k0op == OPR_ILOAD) &&
       !WN_Is_Volatile_Mem(k0)) {
      source_type = OPCODE_desc(k0opc);
      inter_type = OPCODE_rtype(k0opc);
      dest_type = OPCODE_rtype(opc);
      if (OPCODE_rtype(k0opc) == OPCODE_desc(opc) &&
#ifdef KEY /* bug 4803 */
          ((MTYPE_size_min(source_type) <= MTYPE_size_min(inter_type)) ==
           (MTYPE_size_min(inter_type) <= MTYPE_size_min(dest_type))) &&
#endif
	  (SIMP_IS_TYPE_UNSIGNED(source_type))==(SIMP_IS_TYPE_UNSIGNED(dest_type))) {
	 /* Replace the LDID or ILOAD opcode with one which does the conversion directly */
	 /* For example, U8U4CVT(U4U1ILOAD) becomes U8U1ILOAD */
	 if (Is_Valid_Opcode_Parts(k0op,dest_type,source_type)) {
	    newopc = OPCODE_make_op(k0op,dest_type,source_type);
	 } else {
	    newopc = OPCODE_UNKNOWN;
	 }
	 if (k0op == OPR_LDID && ST_class(WN_st(k0))==CLASS_PREG) {
	    newopc = OPCODE_UNKNOWN; /* Suppress for PREGs */
	 }

	 if (newopc != OPCODE_UNKNOWN) {
	    SHOW_RULE("CVT(LOAD)");
	    WN_set_opcode(k0,newopc);
	    return (k0);
	 }
      }
   }
#endif	 

#ifndef EMULATE_LONGLONG

// KEY: SIMP_Is_Constant changed to SIMP_Is_Int_Constant

   /* A couple of additional extraneous convert conditions */
   if (opc == OPC_U4I8CVT || opc == OPC_U4U8CVT) {
      if ((k0op == OPR_BAND && SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))
	   && (UINT64) SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) <= 0x7fffffff) ||
	  (k0op == OPR_LSHR && SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))
	   && SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) >= 33)) {
	 SHOW_RULE("Removed U4I8/U4U8CVT");
	 return (k0);
      }
   } else if (opc == OPC_I4I8CVT || opc == OPC_I4U8CVT) {
      if ((k0op == OPR_ASHR && SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))
	  && SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) >= 32) ||
	  (k0op == OPR_BAND && SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))
	   && (UINT64) SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) <= 0x7fffffff) ||
	  (k0op == OPR_LSHR && SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))
	  && SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) >= 33)) {
	 SHOW_RULE("Removed I4I8/I4U8CVT");
	 return (k0);
      }
   }

#endif /* EMULATE_LONGLONG */

   
   if (op == OPR_REALPART &&
       k0op == OPR_COMPLEX) {
      SHOW_RULE(" REALPART (COMPLEX(a,b))    a ");
      r = SIMPNODE_kid0(k0);
      SIMP_DELETE_TREE(SIMPNODE_kid1(k0));
#ifndef KEY
      // Fix bug 603
      // Return value r is a part of the tree k0 then why delete k0
      SIMP_DELETE(k0);
#endif
      return (r);
      
   } if (op == OPR_IMAGPART &&
	 k0op == OPR_COMPLEX) {
      SHOW_RULE(" IMAGPART (COMPLEX(a,b)) b ");
      r = SIMPNODE_kid1(k0);
      SIMP_DELETE_TREE(SIMPNODE_kid0(k0));
#ifndef KEY
      // Dual for bug 603 - see above
      // Return value r is a part of the tree k0 then why delete k0
      SIMP_DELETE(k0);
#endif
      return (r);
   }
   
   if (op == OPR_CVT &&
       k0op == OPR_CVT) {
      source_type = OPCODE_desc(k0opc);
      inter_type = OPCODE_rtype(k0opc);
      dest_type = OPCODE_rtype(opc);
      k0k0 = SIMPNODE_kid0(k0);
      source_value_type = get_value_type(k0k0);
      
      if (convert_precise(source_value_type,inter_type)) {
	 if (Is_Valid_Opcode_Parts(OPR_CVT,dest_type,source_type)) {
	    newopc = OPCODE_make_op(OPR_CVT,dest_type,source_type);
	 } else {
	    newopc = OPCODE_UNKNOWN;
	 }
	 if (newopc != OPCODE_UNKNOWN) {
	    SHOW_RULE("t1CVT(t2CVT(a)) -> t1CVT(a)");
	    r = SIMPNODE_SimpCreateExp1(newopc,k0k0);
	    SIMP_DELETE(k0);
	 } else if (source_type == dest_type) {
	    SHOW_RULE("t1CVT(t2CVT(a)) -> a");
	    r = k0k0;
	    SIMP_DELETE(k0);
	 }
      }
   }
   
   if (op == OPR_TRUNC &&
       k0op == OPR_CVT) {
      source_type = OPCODE_desc(k0opc);
      inter_type = OPCODE_rtype(k0opc);
      dest_type = OPCODE_rtype(opc);
      k0k0 = SIMPNODE_kid0(k0);
      source_value_type = get_value_type(k0k0);
      if (convert_precise(source_value_type,inter_type)) {
	 if (Is_Valid_Opcode_Parts(OPR_TRUNC,dest_type,source_type)) {
	    newopc = OPCODE_make_op(OPR_TRUNC,dest_type,source_type);
	 } else {
	    newopc = OPCODE_UNKNOWN;
	 }
	 if (newopc != OPCODE_UNKNOWN) {
	    SHOW_RULE("t1TRUNC(t2CVT(a)) -> t1TRUNC(a)");
	    r = SIMPNODE_SimpCreateExp1(newopc,k0k0);
	    SIMP_DELETE(k0);
	 } else if (source_type == dest_type) {
	    SHOW_RULE("t1TRUNC(t2CVT(a)) -> a");
	    r = k0k0;
	    SIMP_DELETE(k0);
	 } else {
	    if (Is_Valid_Opcode_Parts(OPR_CVT,dest_type,source_type)) {
	       newopc = OPCODE_make_op(OPR_CVT,dest_type,source_type);
	       SHOW_RULE("t1TRUNC(t2CVT(a)) -> t1CVT(a)");
	       r = SIMPNODE_SimpCreateExp1(newopc,k0k0);
	       SIMP_DELETE(k0);
	    }
	 }
      }
   }
   
   
   if (op == OPR_TAS && 
       OPCODE_rtype(opc) == OPCODE_rtype(k0opc)) {
      r = k0;
      return (r);
   }


   return (r);
}



/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/*
 *  Here follow all the simplification routines for binary operations
 *  These are all of the form 
 *
 *  static simpnode  simp_thing( OPCODE opc, simpnode k0, simpnode k1,
 *                          BOOL k0const, BOOL k1const)
 *        opc - the opcode of the node we wish to build/simplify
 *        k0,k1 - the children of the operation
 *        k0const, k1const - true if the appropriate child is a constant
 * The return value is either a simplified WHIRL node or NULL if no simplification
 * was done.
 */
/**************************************************************************/
/**************************************************************************/
/**************************************************************************/




/*------------------------------------------------ 
   Simplifications for +, -

x +- 0			x
0 - x			-x
-x + y			y - x
x - (-y)                x + y
x + (-y)                x - y
-x - y                  -(x+y)
x - c                   x + (-c)

Aggressive:
x - x                   0   R:2 for floating point (so that Inf-Inf=0 is acceptable)

The following are done for floating-point only if reassociation is on:

(x+c1) - c2  		x + (c1 - c2)		R:2 for floating types (comments on the 2?)
(x-c) + y  		(x+y) +- c		R:2 for floating types (comments on the 2?)
(x-c1) +- c2 		x + (-c1 +- c2)		R:2 for floating types (comments on the 2?)
(c1-x) +- c2 		(c1 +- c2) - x		R:2 for floating types (comments on the 2?)
(x+-c) - y		(x-y) +- c		R:2 for floating types (comments on the 2?)
(c-x) + y 		(y-x) + c		R:2 for floating types (comments on the 2?)
(c-x) - y 		c - (x+y)		R:2 for floating types (comments on the 2?)

x + (y - x)		y			R:2 for floating types (comments on the 2?)
x - (x + y)		-y			R:2 for floating types (comments on the 2?)
x - (y + x)		-y			R:2 for floating types (comments on the 2?)
x - (x - y)		y			R:2 for floating types (comments on the 2?)
also all 4 op cases of the form 
(x op y) op (w op z) are check for constant fols and cancellation.


z*x + z*(y - x)		y*z			R:2 for floating types (comments on the 2?)
z*x - z*(x + y)		-y*z			R:2 for floating types (comments on the 2?)
z*x - z*(y + x)		-y*z			R:2 for floating types (comments on the 2?)
z*x - z*(x - y)		y*z			R:2 for floating types (comments on the 2?)


-------------------------------------------------*/

/* Pick the subop if x is true */
#define SELECT_ADD_SUB(x) ((x)? subop : addop)

static simpnode  simp_add_sub(OPCODE opc,
			 simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   BOOL reassoc, issub;
   TYPE_ID  ty;
   OPCODE  subop,addop,negop;
   simpnode x[4], t, dt;
   BOOL s[4], bt, constant_moved;
   INT32 num_const,num_ops,i,j,k,ic1,ic2,d1,d2;
   
   ty = OPCODE_rtype(opc);
   issub = (OPCODE_operator(opc) == OPR_SUB);
   if (issub) {
      subop = opc;
      addop = OPC_FROM_OPR(OPR_ADD,ty);
   } else {
      addop = opc;
      subop = OPC_FROM_OPR(OPR_SUB,ty);
#ifdef TARG_X8664 // bug 11400: if any operand is signed type, make SUB signed
      if (MTYPE_signed(SIMPNODE_rtype(k0)) || MTYPE_signed(SIMPNODE_rtype(k1)))
        subop = OPC_FROM_OPR(OPR_SUB, Mtype_TransferSign(MTYPE_I4, ty));
#endif
   }
   negop = OPC_FROM_OPR(OPR_NEG,ty);

   /* Decide if we can reassasociate */ 
   reassoc = FALSE;
   if (SIMP_IS_TYPE_INTEGRAL(ty)) {
      reassoc = TRUE;
   } else if (SIMP_IS_TYPE_FLOATING(ty)) {
      reassoc = Enable_Cfold_Reassociate;
   }

   /* Try the simple ones first */

   if (k1const && 
       ((SIMP_IS_TYPE_INTEGRAL(ty) && !SIMP_Is_Str_Constant (k1) && SIMP_Int_ConstVal(k1)==0) ||
        (SIMP_IS_TYPE_FLOATING(ty) && is_floating_equal(k1,0.0)
#ifdef KEY // bug 2780: if x is -0.0, it is wrong to delete the + with 0.0
	 && IEEE_Arithmetic >= IEEE_INEXACT
#endif
	 ))) {
      SHOW_RULE(" x +- 0 ");
      r = k0;
      SIMP_DELETE(k1);
      return (r);
   }
   if (k1const && issub && !SIMP_IS_TYPE_UNSIGNED(ty)) {
      SHOW_RULE("x - c => x + -c");
      r = SIMPNODE_SimpCreateExp2(addop,k0,SIMPNODE_ConstantFold1(negop,k1));
      return (r);
   }

   if (k0const && issub &&
       ((SIMP_IS_TYPE_INTEGRAL(ty) && !SIMP_Is_Str_Constant (k1) && SIMP_Int_ConstVal(k0)==0) ||
        (SIMP_IS_TYPE_FLOATING(ty) && is_floating_equal(k0,0.0)))) {
      SHOW_RULE(" 0 - x ");
      r = SIMPNODE_SimpCreateExp1(negop,k1);
      SIMP_DELETE(k0);
      return (r);
   }			    
   
   if (issub) {
      if (SIMPNODE_operator(k1)==OPR_NEG) {
	 SHOW_RULE(" x - (-y) ");
	 r = SIMPNODE_SimpCreateExp2(addop,k0,SIMPNODE_kid0(k1));
	 SIMP_DELETE(k1);
      } else if (SIMPNODE_operator(k0)==OPR_NEG) {
         if (k1const && !SIMP_IS_TYPE_UNSIGNED(ty)) {
            SHOW_RULE("-x - c -> (-c)-x");
            r = SIMPNODE_ConstantFold1(negop,k1);
	    r = SIMPNODE_SimpCreateExp2(subop,r,SIMPNODE_kid0(k0));
	    SIMP_DELETE(k0);
	 } else {
	    SHOW_RULE(" -x - y ");
	    r = SIMPNODE_SimpCreateExp1(negop,
			      SIMPNODE_SimpCreateExp2(addop,SIMPNODE_kid0(k0),k1));
	    SIMP_DELETE(k0);
	 }
      }
   } else {
      if (SIMPNODE_operator(k0)==OPR_NEG) {
	 SHOW_RULE(" -x + y ");
	 r = SIMPNODE_SimpCreateExp2(subop,k1,SIMPNODE_kid0(k0));
	 SIMP_DELETE(k0);
      } else if (SIMPNODE_operator(k1)==OPR_NEG) {
	 SHOW_RULE(" x + (-y) ");
	 r = SIMPNODE_SimpCreateExp2(subop,k0,SIMPNODE_kid0(k1));
	 SIMP_DELETE(k1);
      }
   }
   if (r) return (r);

   /* This must be done after the previous opts */
   if (Enable_Cfold_Aggressive && reassoc) {
      if (issub && (r=simp_diff_value(k0,k1,FALSE))) {
	 SHOW_RULE("x-x");
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE_TREE(k1);
	 return (r);
      }
   }

   /* General reassociation */
   /* 4 op case:
    *  op is always the main op.
    *  (x0 op1 x1) op (x2 op3 x3)
    *
    * 3 op cases:
    *  (x0 op1 x1) op x2
    *  x0 op (x1 op2 x1)
    *
    * The prior simplifications guarantee no unary minuses to complicate things 
    */
   num_const = 0;
   num_ops = 0;
   if((SIMPNODE_operator(k0)==OPR_ADD || SIMPNODE_operator(k0)==OPR_SUB) &&
      (SIMPNODE_operator(k1)==OPR_ADD || SIMPNODE_operator(k1)==OPR_SUB)) {
      /* x[0] to x[3] represent the ops */
      /* s[0] to s[3] represent the sign of the 4 ops (TRUE is negative) */
      
      x[0] = SIMPNODE_kid0(k0);
      x[1] = SIMPNODE_kid1(k0);
      x[2] = SIMPNODE_kid0(k1);
      x[3] = SIMPNODE_kid1(k1);

      s[0] = FALSE;
      s[1] = (SIMPNODE_operator(k0) == OPR_SUB);
      s[2] = FALSE;
      s[3] = (SIMPNODE_operator(k1) == OPR_SUB);
      if (issub) {
	 s[2] = !s[2];
	 s[3] = !s[3];
      }
      num_ops = 4;

   } else if ((SIMPNODE_operator(k0)==OPR_ADD || SIMPNODE_operator(k0)==OPR_SUB) &&
	      !(SIMPNODE_operator(k1)==OPR_ADD || SIMPNODE_operator(k1)==OPR_SUB)) {
      x[0] = SIMPNODE_kid0(k0);
      x[1] = SIMPNODE_kid1(k0);
      x[2] = k1;
      
      s[0] = FALSE;
      s[1] = (SIMPNODE_operator(k0) == OPR_SUB);
      s[2] = issub;
      num_ops = 3;

   } else if (!(SIMPNODE_operator(k0)==OPR_ADD || SIMPNODE_operator(k0)==OPR_SUB) &&
	      (SIMPNODE_operator(k1)==OPR_ADD || SIMPNODE_operator(k1)==OPR_SUB)) {
      x[0] = k0;
      x[1] = SIMPNODE_kid0(k1);
      x[2] = SIMPNODE_kid1(k1);
      
      s[0] = FALSE;
      s[1] = FALSE;
      s[2] = (SIMPNODE_operator(k1) == OPR_SUB);
      if (issub) {
	 s[1] = !s[1];
	 s[2] = !s[2];
      }
      num_ops = 3;
      /* Need to guard against the case of c - x - y where neither x nor y is
       * constant.  This interacts with other things in the simplifier to 
       * produce an infinite loop.
       */
      if (SIMP_Is_Constant(x[0]) && !SIMP_Is_Constant(x[1]) && !SIMP_Is_Constant(x[2]) &&
	  s[1] && s[2]) {
	 num_ops = 0;
      }
   }
   
   /* Guard against reassociating when we shouldn't */
   if (!reassoc) num_ops = 0;
   
   /* check that all types are the same size */
   for (k=0; k < num_ops; k++) {
      if (MTYPE_size_reg(SIMPNODE_rtype(x[k])) != MTYPE_size_reg(ty)) {
	 /* disable the rest of the reassociation */
	 num_ops = 0;
      }
   }
   
   if (num_ops != 0) {
      /* There is some potential reassociation to be done */
            
      /* Step 1, collect all the constants to the right */
      constant_moved = FALSE;
      for (i=0, j=num_ops-1; i <= j; ) {
	 if (SIMP_Is_Constant(x[i])) {
	    /* Move the constant to the jth position,
	       shift down the rest (to preserve order),
	       decrement j */
	    if (i != j) {
	       bt = s[i];
               t = x[i];
	       for (k=i; k<j; k++) {
		  s[k] = s[k+1];
		  x[k] = x[k+1];
	       }
	       s[j] = bt;
	       x[j] = t;
	       constant_moved = TRUE;
	    }
	    --j;
	    ++num_const;
	    continue;
	 } else {
	    ++i;
	    continue;
	 }
      }
	 
      /* numconst can now be 0, 1 or 2. If it's 3 or 4 
	 we had a foul up earlier, but simplify now anyway */
      if (num_const == 4) {
	 SHOW_RULE("Questionable 4 const add fold");
	 /* This can only happen if the simplifier is called on unsimplified trees.
	  * Do the folding now
	  */
	 if (s[0]) {
	    r = SIMPNODE_ConstantFold1(negop,x[0]); 
	 } else {
	    r = x[0];
	 }
	 for (i = 1; i <= 3; i++) {
	    if (s[i]) {
	       r = SIMPNODE_ConstantFold2(subop,r,x[i]);
	    } else {
	       r = SIMPNODE_ConstantFold2(addop,r,x[i]);
	    }
	 }
	 return (r);
      } else  if (num_const == 3) {
	 SHOW_RULE("Questionable 3 const add fold");
	 /* This can only happen if the simplifier is called on unsimplified trees.
	  * Do the folding now
	  */
	 ic1 = num_ops-3;
	 ic2 = num_ops-1;
	 if (s[ic1]) {
	    r = SIMPNODE_ConstantFold1(negop,x[ic1]); 
	 } else {
	    r = x[ic1];
	 }
	 for (i = ic1+1; i <= ic2; i++) {
	    if (s[i]) {
	       r = SIMPNODE_ConstantFold2(subop,r,x[i]);
	    } else {
	       r = SIMPNODE_ConstantFold2(addop,r,x[i]);
	    }
	 }
	 if (ic1 == 0) return (r); /* 3 ops, all constant */
	 if (s[0]) {
	    r = SIMPNODE_SimpCreateExp2(subop,r,x[0]);
	 } else {
	    r = SIMPNODE_SimpCreateExp2(addop,x[0],r);
	 }
	 return (r);
      } else  if (num_const==2) {
	 /* Fold the constants */
	 ic1 = num_ops-2;
	 ic2 = num_ops-1;
	 /* Take care of the first op */
	 if (s[ic1]) x[ic1] = SIMPNODE_ConstantFold1(negop,x[ic1]);
	 /* Now fold the two together */
	 if (s[ic2]) {
	    x[ic1] = SIMPNODE_ConstantFold2(subop,x[ic1],x[ic2]);
	 } else {
	    x[ic1] = SIMPNODE_ConstantFold2(addop,x[ic1],x[ic2]);
	 }
	 s[ic1] = FALSE;
	 --num_ops;
	 num_const = 1;
      }

      switch (num_ops) {
       case 2:
	 if (num_const != 1) {
	    /* nothing to do, but this shouldn't happen */
	    SHOW_RULE("Reassociation goof?");
	    return(r);
	 } else {
	    /* Add back the constant */
	    if (s[0]) {
	       SHOW_RULE("-x0 + c1 + c2");
	       r = SIMPNODE_SimpCreateExp2(subop,x[1],x[0]);
	    } else {
	       SHOW_RULE("x0 + c1 + c2");
	       r = SIMPNODE_SimpCreateExp2(addop,x[0],x[1]);
	    }
	    return (r);
	 }
       case 3:
	 /* Three remaining ops */
	 /* either we have 
	    s0 x1 + s1 x1 + c1   or
	    s0 x1 + s1 x1 + s2 x2.
	    
	    */   
	   
	 if (num_const == 1 && constant_moved) {
	    if (s[0]) {
	       if (s[1]) {
		  SHOW_RULE(" -x0 - x1 + c => c - (x0 + x1) ");
		  r = SIMPNODE_SimpCreateExp2(addop,x[0],x[1]);
		  if (s[2]) {  /* This shouldn't happen, but just in case */
		     x[2] = SIMPNODE_SimpCreateExp1(negop,x[2]);
		  }
		  r = SIMPNODE_SimpCreateExp2(subop,x[2],r);
	       } else {
		  SHOW_RULE(" -x0 + x1 + c => (x1 - x0) + c");
		  r = SIMPNODE_SimpCreateExp2(subop,x[1],x[0]);
		  r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[2]),r,x[2]);
	       }
	    } else {
	       SHOW_RULE(" x0 + s1 x1 + c => (x0 + s1 x1) + c ");
	       r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[1]),x[0],x[1]);
	       r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[2]),r,x[2]);
	    }
	 } else if (Enable_Cfold_Aggressive) {
	    /* We want to look for potential cancellations */
	    if ((s[0] != s[1]) && (r = simp_diff_value(x[0],x[1],s[0]))) {
	       SHOW_RULE("x - x op y");
	       SIMP_DELETE_TREE(x[0]);
	       SIMP_DELETE_TREE(x[1]);
	       /* two cancel! */
	       r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[2]),r,x[2]);
	    } else if ((s[1] != s[2]) && (r = simp_diff_value(x[1],x[2],s[1]))) {
	       /* two cancel! */
	       SHOW_RULE("y op x - x");
	       SIMP_DELETE_TREE(x[1]);
	       SIMP_DELETE_TREE(x[2]);
	       r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[0]),r,x[0]);
	    } else if ((s[2] != s[0]) && (r = simp_diff_value(x[2],x[0],s[2]))) {
	       /* two cancel! */
	       SHOW_RULE("x op y - x");
	       SIMP_DELETE_TREE(x[0]);
	       SIMP_DELETE_TREE(x[2]);
	       r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[1]),r,x[1]);
	    } else {
	       /* Generic 3 op case, none of which are constant (or were 
		  constant). Do nothing.
		  */
	       r = NULL;
	    }
	 }
	 return (r);

       case 4:
	 if (!Enable_Cfold_Aggressive) return (r);
	 /* Four remaining ops */
	 /* either we have 
	    s0 x1 + s1 x1 + s2 x2 + c3   or
	    s0 x1 + s1 x1 + s2 x2 + s3 x3.
	    
	    we can't have two constants, since they will have already been folded out
	    
	    */   

	 /* Search for cancellations */
	 if (num_const==1) {
	    i = -1;
	    if ((s[0] != s[1]) && (dt = simp_diff_value(x[0],x[1],s[0]))) {
	       SHOW_RULE("4 op, 1 const, cancel 0,1");
	       i = 2;
	       d1 = 0; d2 = 1;
	    } else if ((s[1] != s[2]) && (dt = simp_diff_value(x[1],x[2],s[1]))) {
	       SHOW_RULE("4 op, 1 const cancel 1,2");
	       i = 0;
	       d1 = 1; d2 = 2;
	    } else if ((s[2] != s[0]) && (dt = simp_diff_value(x[2],x[0],s[2]))) {
	       SHOW_RULE("4 op, 1 const cancel 0,2");
	       i = 1;
	       d1 = 0; d2 = 2;
	    } else {
	       /* Just give up */
	       r = NULL;
	    }
	    if (i != -1) {
	       if (s[i]) {
		  if (s[3]) {
		     r = SIMPNODE_SimpCreateExp2(addop,x[i],x[3]);
		     r = SIMPNODE_SimpCreateExp1(negop,r);
		  } else {
		     r = SIMPNODE_SimpCreateExp2(subop,x[3],x[i]);
		  }
	       } else {
		  r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[3]),x[i],x[3]);
	       }
	       r = SIMPNODE_SimpCreateExp2(addop,r,dt);
	       SIMP_DELETE_TREE(x[d1]);
	       SIMP_DELETE_TREE(x[d2]);
	    }
	 } else {
	    i = -1; j = -1;
	    /* now we have 6 cases */
	    if ((s[0] != s[1]) && (dt = simp_diff_value(x[0],x[1],s[0]))) {
	       SHOW_RULE("4 op, cancel 0,1");
	       i = 2; j = 3;
	       d1 = 0; d2 = 1;
	    } else if ((s[0] != s[2]) && (dt = simp_diff_value(x[0],x[2],s[0]))) {
	       SHOW_RULE("4 op, cancel 0,2");
	       i = 1; j = 3;
	       d1 = 0; d2 = 2;
	    } else if ((s[0] != s[3]) && (dt = simp_diff_value(x[0],x[3],s[0]))) {
	       SHOW_RULE("4 op, cancel 0,3");
	       i = 1; j = 2;
	       d1 = 0; d2 = 3;
	    } else if ((s[1] != s[2]) && (dt = simp_diff_value(x[1],x[2],s[1]))) {
	       SHOW_RULE("4 op, cancel 1,2");
	       i = 0; j = 3;
	       d1 = 1; d2 = 2;
	    } else if ((s[1] != s[3]) && (dt = simp_diff_value(x[1],x[3],s[1]))) {
	       SHOW_RULE("4 op, cancel 1,3");
	       i = 0; j = 2;
	       d1 = 1; d2 = 3;
	    } else if ((s[2] != s[3]) && (dt = simp_diff_value(x[2],x[3],s[2]))) {
	       SHOW_RULE("4 op, cancel 2,3");
	       i = 0; j = 1;
	       d1 = 2; d2 = 3;
	    }
	    if (i == -1) {
	       /* we found nothing useful, give up */
	       r = NULL;
	    } else {
	       /* i and j point to two operands */
	       if (s[i]) {
		  if (s[j]) {
		     r = SIMPNODE_SimpCreateExp2(addop,x[i],x[j]);
		     r = SIMPNODE_SimpCreateExp1(negop,r);
		  } else {
		     r = SIMPNODE_SimpCreateExp2(subop,x[j],x[i]);
		  }
	       } else {
		  r = SIMPNODE_SimpCreateExp2(SELECT_ADD_SUB(s[j]),x[i],x[j]);
	       }
	       r = SIMPNODE_SimpCreateExp2(addop,r,dt);
	       SIMP_DELETE_TREE(x[d1]);
	       SIMP_DELETE_TREE(x[d2]);
	    }
	 }
	 return (r);
      }   /* switch (num_ops) */
   }
   
   if (r) return (r);
   /*------------------------------------------------------*/
   /* Try to distribute, if possible */
   /* z*x op z*y           z*(x op y)              r:2 for floating types */
   
   r = simp_factor(k0,k1,OPR_MPY,opc,ty,FACTOR_ALL);
   if (!r) r = simp_factor_idty(k0,k1,OPR_MPY,opc,ty,TRUE);
#ifdef WN_SIMP_WORKING_ON_WHIRL

   if (r) return (r);

   /* Simplify ADD(LDA[offset], CONSTANT) into LDA[offset+constant] */
   if (k1const && 
       SIMP_IS_TYPE_INTEGRAL(ty) &&
       SIMPNODE_operator(k0) == OPR_LDA) {
      INT64 offset;
      INT64 newoffset;
      INT64 k1val;
      offset = SIMPNODE_lda_offset(k0);
      k1val = SIMP_Int_ConstVal(k1);
      if (issub) k1val = -k1val;
      if (WN_Simp_Fold_LDA &&
	  is_add_ok(&newoffset,k1val,offset,MTYPE_I4)) {
	 SHOW_RULE("c1 + LDA");
	 SIMPNODE_lda_offset(k0) = newoffset;
	 r = k0;
	 SIMP_DELETE(k1);
      }
   }

#endif

   return (r);
}


/*------------------------------------------------ 
   Simplifications for *
 x * 1			x
 x * 0			0			I:1 (because of 0*Infinity)
 x * -1			-x
 -x * c			x * (-c)
 -x * -y		simplify(x*y)
 -x * y			-simplify(x*y)
 x * -y			-simplify(x*y)
 (a +- c1)*c2 		a*c2 +- c1*c2	
 (c1 - a)*c2		c1*c2 - c2*a	
 (c1 - c2*a)*c3		c1*c3 - c2*c3*a	
 (c2*a +- c1)*c3	c2*c3*a +- c1*c3

 Aggressive:
 a*rsqrt(a)             sqrt(a)    Round >= Roundoff_simple
 sqrt(a)*recip(a)       1/sqrt(a)  "

-------------------------------------------------*/

static simpnode  simp_times( OPCODE opc,
		       simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1;
   TYPE_ID  ty;
   OPERATOR first_op,second_op;
   simpnode  addend;
   

   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }
   ty = OPCODE_rtype(opc);
   first_op = SIMPNODE_operator(k0);
   second_op = SIMPNODE_operator(k1);

   /* Cases involving unary - */

   if (first_op==OPR_NEG && second_op==OPR_NEG) {
      SHOW_RULE(" -x*-y ");
      r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),SIMPNODE_kid0(k1));
      SIMP_DELETE(k0);
      SIMP_DELETE(k1);
      return (r);
   } else if (first_op==OPR_NEG && !k1const) {
      SHOW_RULE(" -x * y ");
      r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),k1);
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),r);
      SIMP_DELETE(k0);
      return (r);
   } else if (second_op==OPR_NEG) {
      SHOW_RULE(" x * -y ");
      r = SIMPNODE_SimpCreateExp2(opc,k0,SIMPNODE_kid0(k1));
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),r);
      SIMP_DELETE(k1);
      return (r);
   } else if (first_op==OPR_NEG && k1const
#ifdef KEY /* negate of a const will become a large unsigned const causing */
   	   /* wrong strength reduction (bug 2253) */
   		&& MTYPE_is_signed(ty)
#endif
   					   ) {
      SHOW_RULE(" -x * c ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),k1);
      r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),r);
      SIMP_DELETE(k0);
      return(r);
   }
      
   if (k1const) {
      if (SIMP_IS_TYPE_INTEGRAL(ty)) {
	 c1 = SIMP_Int_ConstVal(k1);
	 if (c1 == 1) {
	    SHOW_RULE(" j * 1 ");
	    r = k0;
	    SIMP_DELETE(k1);
	 } else if (c1 == -1 && !SIMP_IS_TYPE_UNSIGNED(ty)) {
	    SHOW_RULE(" j * -1 ");
	    r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),k0);
	    SIMP_DELETE(k1);
	 } else if (c1 == 0) {
	    SHOW_RULE(" j * 0 ");
	    r = SIMP_INTCONST(ty,0);
	    SIMP_DELETE_TREE(k0);
	    SIMP_DELETE(k1);
	 }
      } else if (SIMP_IS_TYPE_FLOATING(ty) && !SIMP_IS_TYPE_COMPLEX(ty)) {
	 if (is_floating_equal(k1,1.0)) {
	    SHOW_RULE(" a * 1 ");
	    r = k0;
	    SIMP_DELETE(k1);
	 } else if (is_floating_equal(k1,-1.0)) {
	    SHOW_RULE(" a * -1 ");
	    r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),k0);
	    SIMP_DELETE(k1);
	 } else if (is_floating_equal(k1,0.0)) {
	    if (IEEE_Arithmetic >= IEEE_INEXACT) {
	       SHOW_RULE(" j * 0 ");
	       r = SIMP_FLOATCONST(ty,0.0);
	       SIMP_DELETE_TREE(k0);
	       SIMP_DELETE(k1);
	    }
	 }
      }
#ifdef KEY
      else if (SIMP_IS_TYPE_COMPLEX (ty) && second_op == OPR_CONST) {
        if (is_complex_equal (k1,1.0)) {
	  SHOW_RULE (" a * 1 ");
	  r = k0;
	  SIMP_DELETE (k1);
	}
	else if (is_complex_equal (k1, -1.0)) {
	  SHOW_RULE (" a * -1 ");
	  r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),k0);
	  SIMP_DELETE (k1);
	}
	else if (is_complex_equal (k1, 0.0)) {
	  if (IEEE_Arithmetic >= IEEE_INEXACT) {
	    SHOW_RULE ("a * 0 ");
	    r = k1;
	    SIMP_DELETE_TREE (k0);
	  }
	}
      }
#endif
      if (r) {
	 return (r);
      }
      
      if ((first_op == OPR_ADD || first_op == OPR_SUB) &&
	  SIMPNODE_rtype(k0) == ty &&
	  SIMPNODE_rtype(k1) == ty && 
	  is_ok_to_reassociate(opc)) {
	 if (SIMP_Is_Constant(SIMPNODE_kid1(k0))) {
	    /*
	     * Distribute multiplies by constants over the first 
	     * child.
	     *
	     * (c2*a +- c1)*c3	c2*c3*a +- c1*c3
	     * (a +- c1)*c2 	a*c2 +- c1*c2	
	     * (c1 - a)*c2		c1*c2 - c2*a	
	     * (c1 - c2*a)*c3	c1*c3 - c2*c3*a
	     */
	    SHOW_RULE("(a op c1)*c2");
	    /* Make a copy of k1 */
	    r = SIMPNODE_CopyNode(k1);
	    addend = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid1(k0),k1);
	    r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),r);
	    r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),r,addend);
	    SIMP_DELETE(k0);
	 } else if (SIMP_Is_Constant(SIMPNODE_kid0(k0))) {
	    SHOW_RULE("(c1 op a )*c2");
	    /* Make a copy of k1 */
	    r = SIMPNODE_CopyNode(k1);
	    addend = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),k1);
	    r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid1(k0),r);
	    r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),addend,r);
	    SIMP_DELETE(k0);
	 }
      }
   }
   
   if (r) return(r);

   if (Enable_Cfold_Aggressive && Roundoff_Level >= ROUNDOFF_SIMPLE) {
      if (first_op==OPR_SQRT && second_op==OPR_RECIP
	  && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),SIMPNODE_kid0(k1))==0) {
	 SHOW_RULE("sqrt(a)*recip(a)");
	 r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_DIV,ty),
				     SIMP_FLOATCONST(ty,1.0) ,k0);
	 SIMP_DELETE_TREE(k1);
      } else if (second_op==OPR_SQRT && first_op==OPR_RECIP
	  && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),SIMPNODE_kid0(k1))==0) {
	 SHOW_RULE("recip(a)*sqrt(a)");
	 r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_DIV,ty),SIMP_FLOATCONST(ty,1.0) ,k1);
	 SIMP_DELETE_TREE(k0);
#ifndef TARG_X8664 // RSQRT+MULTIPLY is faster than doing a SQRT on x86-64.
      } else if (first_op == OPR_RSQRT && SIMPNODE_Simp_Compare_Trees(k1,SIMPNODE_kid0(k0))==0) {
	 SHOW_RULE("rsqrt(a)*a");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_SQRT,ty),k1);
	 SIMP_DELETE_TREE(k0);
      } else if (second_op == OPR_RSQRT && SIMPNODE_Simp_Compare_Trees(k0,SIMPNODE_kid0(k1))==0) {
	 SHOW_RULE("a*rsqrt(a)");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_SQRT,ty),k0);
	 SIMP_DELETE_TREE(k1);
#endif
      }
   }

   return (r);
}

#ifdef KEY
#if 0
// See comment in the caller.
static simpnode Can_Be_Divided(simpnode node, INT64 c1)
{
  INT64 val;
  INT i;
  simpnode new_k = NULL;
  simpnode k0 = NULL;
  simpnode k1 = NULL;
  simpnode new_k0 = NULL;
  simpnode new_k1 = NULL;
  OPERATOR opr = SIMPNODE_operator(node);
  TYPE_ID ty = SIMPNODE_rtype(node);
  INT kids_count = SIMPNODE_kid_count(node);

  if(!SIMP_IS_TYPE_INTEGRAL(ty))
    return NULL;

  switch(opr){

  //      (a +- b) / c = a/c +- b/c

  case OPR_ADD:
  case OPR_SUB:
    k0 = SIMPNODE_kid0(node);
    k1 = SIMPNODE_kid1(node);
    if (SIMP_Is_Constant(k0)){
      if (SIMPNODE_const_val(k0) % c1 == 0){
	val = SIMPNODE_const_val(k0);
	new_k0 = SIMP_INTCONST(ty,val/c1);
      }
      else return NULL;
    }
    else if (!(new_k0 = Can_Be_Divided(k0, c1)))
      return NULL;
    if (SIMP_Is_Constant(k1)){
      if (SIMPNODE_const_val(k1) % c1 == 0){
	val = SIMPNODE_const_val(k1);
	new_k1 = SIMP_INTCONST(ty,val/c1);
      }
      else return NULL;
    }
    else if (!(new_k1 = Can_Be_Divided(k1, c1)))
      return NULL;
    new_k = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(opr,ty),new_k0,new_k1);
    break;
      
     //      a * c / c = a
    
  case OPR_MPY:
    k0 = SIMPNODE_kid0(node);
    k1 = SIMPNODE_kid1(node);
    if (SIMP_Is_Constant(k1)){
      if (SIMPNODE_const_val(k1) % c1 == 0){
	val = SIMPNODE_const_val(k1);
	new_k1 = SIMP_INTCONST(ty,val/c1);
	new_k = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(opr,ty),k0,new_k1);
	break;
      }
      else return NULL;
    }
    else if (new_k1 = Can_Be_Divided(k1, c1)){
      new_k = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(opr,ty),k0,new_k1);
      break;
    }
    else if (new_k0 = Can_Be_Divided(k0, c1)){
      new_k = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(opr,ty),new_k0,k1);
    break;
    }
    else return NULL;

  case OPR_DIV:;
  default:
    return NULL;
  }
  return new_k;
}
#endif  
#endif  



/*------------------------------------------------ 
   Simplifications for /

-x/-y 			x/y
x / 1			x
x / (-1)		-x
1.0/a			RECIP(a)
-1.0/a			-RECIP(a)
j/(2**N)		j ASHR N (if J is unsigned)
j/N 			TRUNC(DBLE(j)*((1.0d0/DBLE(N)*(1+2**-40)))),
			if j is 32 bits. (Yes, this really works).
a / c			a * 1.0/c, if |c| is 2**k
a / b			a * RECIP(b) 		R:3,I:3 (this is a very bad thing to do)

Aggressive:
a / sqrt(a)             sqrt(a) R:2
sqrt(a) / a             1/sqrt(a) r:2 [simplifier will turn this into RSQRT if allowed]
a/a                     1         

0 / x                   0

-------------------------------------------------*/

static simpnode  simp_div( OPCODE opc,
		     simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{

   /* TODO: DIVISION by 0 checks */

   simpnode r = NULL;
   INT64  c1;
   TYPE_ID ty;
   TCON recip,dval;

   ty = OPCODE_rtype(opc);

   if (SIMPNODE_operator(k0)==OPR_NEG && SIMPNODE_operator(k1)==OPR_NEG) {
      SHOW_RULE(" -x/-y ");
      r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),SIMPNODE_kid0(k1));
      SIMP_DELETE(k0);
      SIMP_DELETE(k1);
      return (r);
   }

   if (k1const && SIMP_IS_TYPE_INTEGRAL(ty)) {
      c1 = SIMP_Int_ConstVal(k1);
      if (c1 == 1) {
	 SHOW_RULE(" x / 1	");
	 r = k0;
	 SIMP_DELETE(k1);
      } else if (c1==-1 && !SIMP_IS_TYPE_UNSIGNED(ty)) {
	 SHOW_RULE(" x / (-1) ");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),k0);
	 SIMP_DELETE(k1);
      }
   } /* Integral with constant divisor */
   else if (k1const && SIMP_IS_TYPE_FLOATING(ty)) {
      /* 
       * x / 1			x
       * x / (-1)		-x
       */
      /* bail out on division by 0 */
      if (is_floating_equal(k1,0.0)) {
	 /* Division by 0 */
	 return(r); 
      }
      if (is_floating_equal(k1,1.0)) {
	 SHOW_RULE("x/1");
	 r = k0;
	 SIMP_DELETE(k1);
      } else if (is_floating_equal(k1,-1.0)) {
	 SHOW_RULE("x/-1");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),k0);
	 SIMP_DELETE(k1);
      } else if (Targ_Is_Power_Of_Two(SIMP_Flt_ConstVal(k1))) {
	 SHOW_RULE(" a / c  a * 1.0/c, if |c| is 2**k ");

	 /* Take the reciprocal of a floating point-constant */
	 dval = SIMP_Flt_ConstVal(k1);
	 recip = Targ_WhirlOp (opc,
			       Host_To_Targ_Float (ty, 1.0 ),
			       dval, 0 );
	 r = SIMPNODE_CreateFloatconstFromTcon(recip);
	 
	 r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_MPY,ty),k0,r);
	 SIMP_DELETE(k1);
      }
   }
   /* Bail out for simplicity */
   if (r) return (r);
   if (k0const && SIMP_IS_TYPE_FLOATING(ty) && !SIMP_IS_TYPE_COMPLEX(ty)) {
      /*  1.0/a			RECIP(a)
       * -1.0/a			-RECIP(a)
       */
      if ((is_floating_equal(k0,1.0) ||
	   is_floating_equal(k0,-1.0)) && 
#ifdef TARG_X8664
	  (Rsqrt_Allowed >= 1 &&			// bug 6123
	   (SIMPNODE_operator(k1) == OPR_SQRT ||
	    SIMPNODE_operator(k1) == OPR_MPY))
#else
	  Recip_Allowed
#endif
	  ) {
	 SHOW_RULE("+-1.0 / a");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_RECIP,ty),k1);
	 if (is_floating_equal(k0,-1.0)) {
	    r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,ty),r);
	 }
	 SIMP_DELETE(k0);
      }
   } else if (SIMP_IS_TYPE_FLOATING(ty) && !SIMP_IS_TYPE_COMPLEX(ty)
#ifndef KEY
	      && Div_Split_Allowed && Recip_Allowed) {
#else
       	      && Div_Split_Allowed && Opt_Level > 1) {
#endif
      SHOW_RULE(" a / b	   a * RECIP(b)  ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_RECIP,ty),k1);
      r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_MPY,ty),k0,r);
   }

#ifdef KEY
#if 0
   // Yan Xie implemented this optimization but, 
   // there is a problem with Can_Be_Divided. 
   // Whenever the function fails (meaning it returns NULL), 
   // it would have modified the WHIRL tree properties 
   // (for example, the parent map), but it disables the following
   // optimization. Because the optimization does not take effect,
   // the WHIRL tree is left un-modified, but the new properties 
   // may not be correct.
   if( k0const && SIMP_IS_TYPE_INTEGRAL(ty) ){
     if( SIMP_Int_ConstVal(k0) == 0 ){
       SHOW_RULE(" 0 / x	");
       SIMP_DELETE(k0);
       SIMP_DELETE(k1);
       return SIMP_INTCONST(ty,0);
     }
   }
            
   if( k1const && SIMP_IS_TYPE_INTEGRAL(ty)
       && SIMP_Int_ConstVal(k1) != 0 ){
     simpnode new_k0 = NULL;
     c1 = SIMP_Int_ConstVal(k1); 
     if( new_k0 = Can_Be_Divided(k0,c1)){
       SIMP_DELETE(k1);
       return new_k0;
     }
   }
   
#endif
#endif

   if (!Enable_Cfold_Aggressive || r) return(r);

   if (Roundoff_Level >= ROUNDOFF_SIMPLE) {
      if (SIMPNODE_operator(k0)==OPR_SQRT 
	  && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),k1)==0) {
	 SHOW_RULE("sqrt(a)/a");
	 r = SIMPNODE_SimpCreateExp2(opc, SIMP_FLOATCONST(ty,1.0) ,k0);
	 SIMP_DELETE_TREE(k1);
      } else if (SIMPNODE_operator(k1)==OPR_SQRT
		 && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k1),k0)==0) {
	 SHOW_RULE("a/sqrt(a)");
	 r = k1;
	 SIMP_DELETE_TREE(k0);
      }
   }
   
   if (SIMPNODE_Simp_Compare_Trees(k0,k1) == 0) {
      SHOW_RULE("x/x");
      if (SIMP_IS_TYPE_INTEGRAL(ty)) {
	 r = SIMP_INTCONST(ty,1);
      } else if (SIMP_IS_TYPE_FLOATING(ty) && !Force_IEEE_Comparisons && 
		 IEEE_Arithmetic >= IEEE_INEXACT) {
	 r = SIMP_FLOATCONST(ty,1.0);
      }
   }

   return (r);
}


/*------------------------------------------------ 
   Simplifications for MOD and REM

j mod 1 		0
j mod -1		0
0 mod j			0
j rem 1 		0
j rem -1		0
0 rem j			0
j mod (2**N)		j & (2**N-1)	
j mod -(2**N)		(j & (2**N-1)) - 2**N	If j is signed
j rem (2**N) 		j & (2**N-1)		If j is unsigned

To be done:
j rem (2**N)		(j & (2**N-1)) -
			 ((-(j.lt.0) & (2**N))) If j is signed
j rem -(2**N)		(j & (2**N-1)) -
			 ((-(j.lt.0) & (2**N))) If j is signed

-------------------------------------------------*/

static simpnode  simp_mod_rem(OPCODE opc,
			 simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64 c0,c1;
   BOOL isrem,isunsigned;
   TYPE_ID ty;
   
   ty = OPCODE_rtype(opc);

   if (k0const) {
      SHOW_RULE(" 0 rem or mod j ");
      c0 = SIMP_Int_ConstVal(k0);
      if (c0 == 0) {
	 r = SIMP_INTCONST(ty,0);
	 SIMP_DELETE(k0);
	 SIMP_DELETE_TREE(k1);
      } 
      return(r);
   }

   /* Bail out if neither is constant */
   if (!(k0const || k1const)) return(r);

   /* at this point, we know k1 to be a constant */
   c1 = SIMP_Int_ConstVal(k1);
   isrem = (OPCODE_operator(opc) == OPR_REM);
   isunsigned = SIMP_IS_TYPE_UNSIGNED(ty);
   
   if (c1 == 1 || (c1 == -1 && !isunsigned)) {
      /* 
       * j mod 1 		0
       * j mod -1		0
       * j rem 1 		0
       * j rem -1 		0
       *
       */
      SHOW_RULE("j mod/rem +-1");
      r = SIMP_INTCONST(ty,0);
      SIMP_DELETE_TREE(k0);
      SIMP_DELETE(k1);
   } else if (!isrem && IS_POWER_OF_2(c1)) {
      if (isunsigned || (!isunsigned && (c1 > 0))) {
	 SHOW_RULE(" j mod (2**N)		j & (2**N-1) ");
	 r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BAND,ty),k0,
			   SIMP_INTCONST(ty,c1-1));
	 SIMP_DELETE(k1);
      }
   } else if (!isrem && IS_POWER_OF_2(-c1) && !isunsigned) {
      SHOW_RULE(" j mod -(2**N)	(j & (2**N-1)) - 2**N	If j is signed      ");
      /* Note that this works as well for the largest negative integer */
      c1 = -c1;
      r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BAND,ty),k0,
			SIMP_INTCONST(ty,c1-1));
      r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_SUB,ty),r,
			SIMP_INTCONST(ty,c1));
      SIMP_DELETE(k1);
   } else if (isrem && IS_POWER_OF_2(c1) && isunsigned) {
      SHOW_RULE(" j rem (2**N) ");
      r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BAND,ty),k0,
			SIMP_INTCONST(ty,c1-1));
      SIMP_DELETE(k1);
   }

   return (r);
}


/*------------------------------------------------ 
   Simplifications for **

1 ** x		        1
-1 ** N			1 - (N&1)<<1
x ** 0                  1
x ** 1                  x
a ** 0.5		SQRT(a)
a ** -0.5		1.0/SQRT(a) (RSQRT will be generated by simplifier)
a ** -1.0		1.0/a       (RECIP will be generated by the simplifier)

-------------------------------------------------*/

static simpnode  simp_power( OPCODE opc,
		       simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   TYPE_ID ty;

   if (!(k0const || k1const)) return (r);
   ty = OPCODE_rtype(opc);

   if (k0const) {
      if (is_numeric_equal(k0,1.0)) {
	 SHOW_RULE(" 1 ** x     1 ");
	 if (SIMP_IS_TYPE_FLOATING(ty)) {
	    r = SIMP_FLOATCONST(ty,1.0);
	    SIMP_DELETE(k0);
	    SIMP_DELETE_TREE(k1);
	 } else if (SIMP_IS_TYPE_INTEGRAL(ty)) {
	    r = SIMP_INTCONST(ty,1);
	    SIMP_DELETE(k0);
	    SIMP_DELETE_TREE(k1);
	 }
      } else if (SIMP_IS_TYPE_INTEGRAL(ty) && !SIMP_IS_TYPE_UNSIGNED(ty) &&
		 SIMP_Int_ConstVal(k0)==-1) {
	 SHOW_RULE(" -1 ** N    1-(N&1)<<1 ");
	 r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BAND,ty),k1,
			   SIMP_INTCONST(ty,1));
	 r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_SHL,ty),r,
			   SIMP_INTCONST(ty,1));
	 r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_SUB,ty),
			   SIMP_INTCONST(ty,1),r);
	 SIMP_DELETE(k0);
      }
   } else if (k1const) {
      if (is_numeric_equal(k1,1.0)) {
	 SHOW_RULE(" x ** 1     x ");
	 r = k0;
	 SIMP_DELETE(k1);
      } else if (is_numeric_equal(k1,0.0)) {
	 SHOW_RULE(" x ** 0     1 ");
	 /*** NOTE: this means that 0**0 will not signal an exception */
	 if (SIMP_IS_TYPE_FLOATING(ty)) {
	    r = SIMP_FLOATCONST(ty,1.0);
	    SIMP_DELETE_TREE(k0);
	    SIMP_DELETE(k1);
	 } else if (SIMP_IS_TYPE_INTEGRAL(ty)) {
	    r = SIMP_INTCONST(ty,1);
	    SIMP_DELETE_TREE(k0);
	    SIMP_DELETE(k1);
	 }
      } else if (SIMP_IS_TYPE_FLOATING(ty)) {
	 if (is_floating_equal(k1,-1.0)) {
	    SHOW_RULE(" a ** -1    1/a ");
	    r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_DIV,ty),
			      SIMP_FLOATCONST(ty,1.0),k0);
	    SIMP_DELETE(k1);
	 } else if (is_floating_equal(k1,0.5)) {
	    SHOW_RULE(" a ** 0.5		SQRT(a) ");
	    r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_SQRT,ty),k0);
	    SIMP_DELETE(k1);
	 } else if (is_floating_equal(k1,-0.5)) {
	    SHOW_RULE(" a ** -0.5	1/SQRT(a) ");
	    r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_SQRT,ty),k0);
	    r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_DIV,ty),
			      SIMP_FLOATCONST(ty,1.0),r);
	    SIMP_DELETE(k1);
	 }
      }
      
   }
   return (r);
}


/*------------------------------------------------ 
   Simplifications for MIN and MAX

MAX(x,c)		x if c is smallest possible value
MIN(x,c)		x if c is largest possible value
MAX(x,c)		c if c is largest possible value
MIN(x,c)		c if c is smallest possible value

The above are currently only done for integer types 

Aggressive:
MAX(x,x), MIN(x,x)      x
-------------------------------------------------*/

/* k0const not used, compiler parses next comment */
/* ARGSUSED */
static simpnode  simp_min_max(OPCODE opc,
			 simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   BOOL  ismax;
   INT64 c1;

   ismax = (OPCODE_operator(opc) == OPR_MAX);
   if (k1const && SIMP_Is_Int_Constant(k1)) {
      c1 = SIMP_Int_ConstVal(k1);
      switch (OPCODE_rtype(opc)) {
       case MTYPE_I4:
	 if ((ismax && c1==INT32_MIN) ||
	     (!ismax && c1==INT32_MAX)) {
	    r = k0;
	    SIMP_DELETE(k1);
	 } else if ((ismax && c1==INT32_MAX) ||
		    (!ismax && c1==INT32_MIN)){
	    r = k1;
	    SIMP_DELETE_TREE(k0);
	 }
	 break;

       case MTYPE_U4:
	 if ((ismax && c1==0) ||
	     (!ismax && c1==UINT32_MAX)) {
	    r = k0;
	    SIMP_DELETE(k1);
	 } else if ((ismax && c1==UINT32_MAX) ||
		    (!ismax && c1==0)){
	    r = k1;
	    SIMP_DELETE_TREE(k0);
	 }
	 break;

       case MTYPE_I8:
	 if ((ismax && c1==0x8000000000000000LL) ||
	     (!ismax && c1==0x7fffffffffffffffLL)) {
	    r = k0;
	    SIMP_DELETE(k1);
	 } else if ((ismax && c1==0x7fffffffffffffffLL) ||
		    (!ismax && c1==0x8000000000000000LL)){
	    r = k1;
	    SIMP_DELETE_TREE(k0);
	 }
	 break;

       case MTYPE_U8:
	 if ((ismax && c1==0) ||
	     (!ismax && c1==(UINT64) -1)) {
	    r = k0;
	    SIMP_DELETE(k1);
	 } else if ((ismax && c1==(UINT64) -1) ||
		    (!ismax && c1==0)){
	    r = k1;
	    SIMP_DELETE_TREE(k0);
	 }
	 break;
      }
   }

   if (r) {
      SHOW_RULE("MIN/MAX(x, largest/smallest)");
      return (r);
   }

   if (!Enable_Cfold_Aggressive) return (r);
   if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
      SHOW_RULE(" MAX(x,x), MIN(x,x) ");
      r = k0;
      SIMP_DELETE_TREE(k1);
   }
   return (r);
}


/*------------------------------------------------ 
   Simplifications for &
j & 0 			0
j & -1			j
(j==0) & (k==0)		(j|k)==0
~j & ~k			~(j|k)			Generates NOR
<comp> & 1		<comp>
(j | c1) & c2           j & c2    if (c1&c2 == 0)
(j >> shift) & c1       j >> shift if mask has ones in the shift_size - shift lower bits
 Aggressive:
~j & j			0
j & j 			j

(j >> c2) & mask        extract (if enabled)

-------------------------------------------------*/

static simpnode  simp_band( OPCODE opc,
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1,mask_bits;
   TYPE_ID  ty;

   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }
   ty = OPCODE_rtype(opc);

   if (k1const) {
      c1 = SIMP_Int_ConstVal(k1); 
      if (c1 == 0) {
	 SHOW_RULE("j&0");
	 r = SIMP_INTCONST(ty,0);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE(k1);
      } else if (c1 == -1) {
	 SHOW_RULE("j&-1");
	 r = k0;
	 SIMP_DELETE(k1);
      } else if ((c1 == 1) && (get_inverse_relop(SIMPNODE_opcode(k0))!=0)) {
	 SHOW_RULE("<comp> & 1");
	 r = k0;
	 SIMP_DELETE(k1);
      } else if ((SIMPNODE_operator(k0) == OPR_BIOR) && SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) && // KEY
		 ((SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) & c1) == 0)) {
	 SHOW_RULE("(j|c1) & c2, c1&c2=0");
	 r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),k1);
	 SIMP_DELETE(SIMPNODE_kid1(k0));
	 SIMP_DELETE(k0);
      } else if ((SIMPNODE_operator(k0) == OPR_LSHR) &&
		 SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) && // KEY
		 (MTYPE_bit_size(SIMPNODE_rtype(k0)) == MTYPE_bit_size(ty))) {
	 INT32 shift_count = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
#ifdef KEY
	 // When expanding into extract, need to take care of the endianness.
	 // See gcc.c-torture/execute/200201271-1.c 
	 if (Target_Byte_Sex != Host_Byte_Sex)
	   shift_count = MTYPE_bit_size(ty) - shift_count - log2((UINT64)c1+1);
#endif
         mask_bits = create_bitmask(MTYPE_bit_size(ty) - shift_count);
	 if ((mask_bits & c1) == mask_bits) {
	   SHOW_RULE("(j LSHR c2) & c1)");
	   r = k0;
	   SIMP_DELETE(k1);
         } else if (Enable_extract_bits && IS_POWER_OF_2(c1+1)) {
	   r = SIMPNODE_SimpCreateExtract(MTYPE_bit_size(ty) == 32 ? OPC_U4EXTRACT_BITS : OPC_U8EXTRACT_BITS,
					  shift_count,log2((UINT64)c1+1),
					  SIMPNODE_kid0(k0));
	   SIMP_DELETE(k1);
	   SIMP_DELETE(SIMPNODE_kid1(k0));
	 }
      }	  
   } else if ((SIMPNODE_operator(k0)==OPR_BNOT) && (SIMPNODE_operator(k1)==OPR_BNOT)) {
      SHOW_RULE(" ~j & ~k ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_BNOT,ty),
			SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BIOR,ty),
				      SIMPNODE_kid0(k0),SIMPNODE_kid0(k1)));
      SIMP_DELETE(k0);
      SIMP_DELETE(k1);
   } else if ((SIMPNODE_operator(k0)==OPR_EQ) && (SIMPNODE_operator(k1)==OPR_EQ) &&
	      SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) &&
	      SIMP_Is_Int_Constant(SIMPNODE_kid1(k1)) &&
	      SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) == 0 &&
	      SIMP_Int_ConstVal(SIMPNODE_kid1(k1)) == 0 &&
	      (SIMP_TYPE(SIMPNODE_kid0(k0)) == SIMP_TYPE(SIMPNODE_kid0(k1))) &&
	      (ty == SIMP_TYPE(SIMPNODE_kid0(k0)))) {
      SHOW_RULE(" (j==0) & (k==0) ");
      
      r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),
			SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BIOR,ty),
				      SIMPNODE_kid0(k0), SIMPNODE_kid0(k1)),
			SIMPNODE_kid1(k0));
      SIMP_DELETE(k0);
      SIMP_DELETE(SIMPNODE_kid1(k1));
      SIMP_DELETE(k1);
   }

   if (!Enable_Cfold_Aggressive || r) return (r);

   if ((SIMPNODE_operator(k0)==OPR_BNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),k1)==0) ||
       (SIMPNODE_operator(k1)==OPR_BNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k1),k0)==0)) {
      SHOW_RULE(" ~j & j ");
      r = SIMP_INTCONST(ty,0);
      SIMP_DELETE_TREE(k0);
      SIMP_DELETE_TREE(k1);
   } else if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
      SHOW_RULE(" j & j ");
      r = k0;
      SIMP_DELETE_TREE(k1);
   }

   if (r) return (r);

   r = simp_factor(k0,k1,OPR_BIOR,opc,ty,FACTOR_ALL);
   if (r) return (r);

   r = simp_factor_idty(k0,k1,OPR_BIOR,opc,ty,FALSE);
   if (r) return (r);

   /* catch the a & b & c & b type cases */
   r = simp_factor(k0,k1,OPR_BAND,opc,ty,FACTOR_ALL);
   if (r) return (r);

   r = simp_factor_idty(k0,k1,OPR_BAND,opc,ty,FALSE);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_SHL,opc,ty,FACTOR_22);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_ASHR,opc,ty,FACTOR_22);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_LSHR,opc,ty,FACTOR_22);
       
   return (r);
}


/*------------------------------------------------ 
   Simplifications for |
j | 0			j
j | -1			-1
(j & c2) | c1		(j | c1) & (c1|c2)	might simplify if c1|c2 = -1  
~j | ~k			~(j & k)
(j!=0) | (k!=0)		(j|k)!=0
<comp> | 1		1 

x & mask1 | compose (0,y) (if appropriate)
                         compose (x,y)

 Aggressive:
~j | j			-1
j | j 			j

-------------------------------------------------*/

static simpnode  simp_bior( OPCODE opc,
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1,c2;
   TYPE_ID  ty;

   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }
   ty = OPCODE_rtype(opc);

   if (k1const) {
#ifdef TARG_IA64
      if (SIMP_Is_Int_Constant (k1)) {
         c1 = SIMP_Int_ConstVal(k1); 
      } else if (SIMP_Is_Str_Constant (k1)) {
         c1 = Targ_To_Host (SIMP_Str_ConstVal (k1)); 
      } else {
         Fail_FmtAssertion ("Not a int/str constant");
      }
#else
       c1 = SIMP_Int_ConstVal(k1); 
#endif
      if (c1 == 0) {
	 SHOW_RULE("j|0");
	 r = k0;
	 SIMP_DELETE(k1);
      } else if (c1 == -1) {
	 SHOW_RULE("j|-1");
	 r = SIMP_INTCONST(ty,-1);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE(k1);
      } else if ((c1 == 1) && (get_inverse_relop(SIMPNODE_opcode(k0))!=0)) {
	 SHOW_RULE("<comp> | 1");
	 r = SIMP_INTCONST(ty,1);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE(k1);
      } else if ((SIMPNODE_operator(k0) == OPR_BAND) &&
		 (SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)))) /* KEY */ {
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	 if ((c2 | c1) == -1) {
	    SHOW_RULE("(j & c2) | c1");
	    r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),k1);
	    SIMP_DELETE(k0);
	    SIMP_DELETE(SIMPNODE_kid1(k0));
	 }
      }
	  
   } else if ((SIMPNODE_operator(k0)==OPR_BNOT) && (SIMPNODE_operator(k1)==OPR_BNOT)) {
      SHOW_RULE(" ~j | ~k ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_BNOT,ty),
			SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BAND,ty),
				      SIMPNODE_kid0(k0),SIMPNODE_kid0(k1)));
      SIMP_DELETE(k0);
      SIMP_DELETE(k1);

   } else if ((SIMPNODE_operator(k0)==OPR_NE) && (SIMPNODE_operator(k1)==OPR_NE) &&
	      SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) &&
	      SIMP_Is_Int_Constant(SIMPNODE_kid1(k1)) &&
	      SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) == 0 &&
	      SIMP_Int_ConstVal(SIMPNODE_kid1(k1)) == 0 &&
	      (SIMP_TYPE(SIMPNODE_kid0(k0)) == SIMP_TYPE(SIMPNODE_kid0(k1))) &&
	      (OPCODE_rtype(opc) == (SIMP_TYPE(SIMPNODE_kid0(k0))))) {
      SHOW_RULE(" (j!=0) | (k!=0) ");
      
      r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),
			SIMPNODE_SimpCreateExp2(opc, SIMPNODE_kid0(k0), SIMPNODE_kid0(k1)),
			SIMPNODE_kid1(k0));
      SIMP_DELETE(k0);
      SIMP_DELETE(SIMPNODE_kid1(k1));
      SIMP_DELETE(k1);
   }

   if (!Enable_Cfold_Aggressive || r) return (r);

   if ((SIMPNODE_operator(k0)==OPR_BNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),k1)==0) ||
       (SIMPNODE_operator(k1)==OPR_BNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k1),k0)==0)) {
      SHOW_RULE(" ~j | j ");
      r = SIMP_INTCONST(ty,-1);
      SIMP_DELETE_TREE(k0);
      SIMP_DELETE_TREE(k1);
   } else if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
      SHOW_RULE(" j | j ");
      r = k0;
      SIMP_DELETE_TREE(k1);
   }

   if (r) return (r);

   if (SIMPNODE_operator(k0) == OPR_BAND && SIMPNODE_operator(k1) == OPR_COMPOSE_BITS &&
       SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) && SIMP_Is_Int_Constant(SIMPNODE_kid0(k1)) && // KEY
       SIMP_Int_ConstVal(SIMPNODE_kid0(k1)) == 0)
   {
     
     UINT64 dep_mask = create_bitmask(SIMPNODE_op_bit_size(k1))<<SIMPNODE_op_bit_offset(k1);
#ifdef KEY
     // When expanding into deposit, need to take care of the endianness.
     if (Target_Byte_Sex != Host_Byte_Sex)
       dep_mask = create_bitmask(SIMPNODE_op_bit_size(k1)) <<
	 (MTYPE_bit_size(ty) - SIMPNODE_op_bit_offset(k1) - SIMPNODE_op_bit_size(k1));
#endif
     UINT64 type_mask = create_bitmask(MTYPE_bit_size(ty));
     c1 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
     if (((dep_mask & c1) == 0) && (((dep_mask | c1) & type_mask) == type_mask)) {
       SHOW_RULE("(j&mask)|compose(0,k)");
       r = SIMPNODE_SimpCreateDeposit(SIMPNODE_opcode(k1),SIMPNODE_op_bit_offset(k1),
				      SIMPNODE_op_bit_size(k1),SIMPNODE_kid0(k0),SIMPNODE_kid1(k1));
#ifdef KEY
       // When expanding into deposit, need to take care of the endianness.
       if (Target_Byte_Sex != Host_Byte_Sex)
	 r = SIMPNODE_SimpCreateDeposit(SIMPNODE_opcode(k1), 
					MTYPE_bit_size(ty) - 
					SIMPNODE_op_bit_offset(k1) - 
					SIMPNODE_op_bit_size(k1),
					SIMPNODE_op_bit_size(k1),SIMPNODE_kid0(k0),SIMPNODE_kid1(k1));
#endif       
       SIMP_DELETE(SIMPNODE_kid1(k0));
       SIMP_DELETE(SIMPNODE_kid0(k1));
       SIMP_DELETE(k0);
       SIMP_DELETE(k1);
     }
   }
   if (Enable_compose_bits &&
       SIMPNODE_operator(k0) == OPR_BAND &&
       SIMPNODE_operator(k1) == OPR_BAND &&
       SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) &&
       SIMP_Is_Int_Constant(SIMPNODE_kid1(k1)) /* KEY */)
   {
     c1 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
     c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k1));
     UINT64 type_mask = create_bitmask(MTYPE_bit_size(ty));
     
     if (IS_POWER_OF_2(c1+1) && ((c2 & c1) == 0) && (((c2 | c1) & type_mask) == type_mask)) {
       SHOW_RULE("(J&mask1) | (k & mask2)");
       r = SIMPNODE_SimpCreateDeposit(OPC_FROM_OPR(OPR_COMPOSE_BITS,ty),0,log2((UINT64)c1+1),
				      SIMPNODE_kid0(k1),SIMPNODE_kid0(k0));
#ifdef KEY
       // When expanding into deposit, need to take care of the endianness.
       if (Target_Byte_Sex != Host_Byte_Sex)
	 r = SIMPNODE_SimpCreateDeposit(OPC_FROM_OPR(OPR_COMPOSE_BITS,ty),
					MTYPE_bit_size(ty)-log2((UINT64)c1+1), 
					log2((UINT64)c1+1),
					SIMPNODE_kid0(k1),SIMPNODE_kid0(k0));
#endif
       SIMP_DELETE(SIMPNODE_kid1(k0));
       SIMP_DELETE(SIMPNODE_kid1(k1));
       SIMP_DELETE(k0);
       SIMP_DELETE(k1);
     } else if (IS_POWER_OF_2(c2+1) && ((c2 & c1) == 0) && (((c2 | c1) & type_mask) == type_mask)) {
       SHOW_RULE("(J&mask2) | (k & mask1)");
       r = SIMPNODE_SimpCreateDeposit(OPC_FROM_OPR(OPR_COMPOSE_BITS,ty),0,log2((UINT64)c2+1),
				      SIMPNODE_kid0(k0),SIMPNODE_kid0(k1));
#ifdef KEY
       // When expanding into deposit, need to take care of the endianness.
       if (Target_Byte_Sex != Host_Byte_Sex)
         r = SIMPNODE_SimpCreateDeposit(OPC_FROM_OPR(OPR_COMPOSE_BITS,ty),
					MTYPE_bit_size(ty)-log2((UINT64)c2+1), 
					log2((UINT64)c2+1),
					SIMPNODE_kid0(k0),SIMPNODE_kid0(k1));
#endif
       SIMP_DELETE(SIMPNODE_kid1(k0));
       SIMP_DELETE(SIMPNODE_kid1(k1));
       SIMP_DELETE(k0);
       SIMP_DELETE(k1);
     }
   }
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_BAND,opc,ty,FACTOR_ALL);
   if (r) return (r);

   r = simp_factor_idty(k0,k1,OPR_BAND,opc,ty,FALSE);
   if (r) return (r);

   /* catch the a | b | c | b type cases */
   r = simp_factor(k0,k1,OPR_BIOR,opc,ty,FACTOR_ALL);
   if (r) return (r);

   r = simp_factor_idty(k0,k1,OPR_BIOR,opc,ty,FALSE);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_SHL,opc,ty,FACTOR_22);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_ASHR,opc,ty,FACTOR_22);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_LSHR,opc,ty,FACTOR_22);

   return (r);
}

/*------------------------------------------------ 
   Simplifications for bitwise Nor

Just create NOT (a|b). The NOT simplifier will create BNOR if necessary

-------------------------------------------------*/
static simpnode  simp_bnor( OPCODE opc,
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   TYPE_ID   ty; 
   
   ty = OPCODE_rtype(opc);
   r = SIMPNODE_SimplifyExp2(OPC_FROM_OPR(OPR_BIOR,ty),k0,k1);
   if (r) {
      /* The OR simplified to something */
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_BNOT,ty),r);
   }
   if (r) {
      SHOW_RULE("BNOR simplified");
   }
   return (r);
}


/*------------------------------------------------ 
   Simplifications for ^

j ^ 0 			j
j ^ -1			~j
<comp> ^ 1		<inverse comp>		IEEE_comparisons off for floating
						compares
Aggressive:

j ^ j                   0
j ^ ~j                  -1
-------------------------------------------------*/

static simpnode  simp_bxor( OPCODE opc,
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1;
   OPCODE  inv_opc;
   TYPE_ID   ty; 
   
   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }

   ty = OPCODE_rtype(opc);

   if (k1const) {
      c1 = SIMP_Int_ConstVal(k1); 
      if (c1 == 0) {
	 SHOW_RULE("j^0");
	 r = k0;
	 SIMP_DELETE(k1);
      } else if (c1 == -1) {
	 SHOW_RULE("j^-1");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_BNOT,OPCODE_rtype(opc)),
			   k0);
	 SIMP_DELETE(k1);
      } else if ((c1 == 1) && ((inv_opc=get_inverse_relop(SIMPNODE_opcode(k0)))!=0)) {
	 if (! (Force_IEEE_Comparisons && 
		SIMP_IS_TYPE_FLOATING(OPCODE_desc(inv_opc)))) {
	    SHOW_RULE("<comp> ^ 1 ");
	    r = SIMPNODE_SimpCreateExp2(inv_opc,SIMPNODE_kid0(k0),SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	    SIMP_DELETE(k1);
	 }
      }
   }

   if (!Enable_Cfold_Aggressive || r) return (r);

   if ((SIMPNODE_operator(k0)==OPR_BNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),k1)==0) ||
       (SIMPNODE_operator(k1)==OPR_BNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k1),k0)==0)) {
      SHOW_RULE(" ~j ^ j ");
      r = SIMP_INTCONST(ty,-1);
      SIMP_DELETE_TREE(k0);
      SIMP_DELETE_TREE(k1);
   } else if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
      SHOW_RULE(" j ^ j ");
      r = SIMP_INTCONST(ty,0);
      SIMP_DELETE_TREE(k0);
      SIMP_DELETE_TREE(k1);
   }

   if (r) return (r);

   r = simp_factor(k0,k1,OPR_BAND,opc,ty,FACTOR_ALL);
   if (r) return (r);

   r = simp_factor_idty(k0,k1,OPR_BAND,opc,ty,FALSE);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_SHL,opc,ty,FACTOR_22);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_ASHR,opc,ty,FACTOR_22);
   if (r) return (r);

   r = simp_factor(k0,k1,OPR_LSHR,opc,ty,FACTOR_22);

   return (r);
}


/*------------------------------------------------ 
   Simplifications for &&

j && 0 			0
j && 1			j
!j && !k		!(j || k)		Generates NOR

Aggressive:

!j && j			0			A
j && j 			j			A

-------------------------------------------------*/

static simpnode  simp_land( OPCODE opc,
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1;
   TYPE_ID  ty;

   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }
   ty = OPCODE_rtype(opc);

   if (k1const) {
      c1 = SIMP_Int_ConstVal(k1); 
      if (c1 == 0) {
	 SHOW_RULE(" j&&0");
	 r = SIMP_INTCONST(ty,0);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE(k1);
      } else {
	 SHOW_RULE(" j&&1");
	 r = k0;
	 SIMP_DELETE(k1);
      }
	  
   } else if ((SIMPNODE_operator(k0)==OPR_LNOT) && (SIMPNODE_operator(k1)==OPR_LNOT)) {
      SHOW_RULE(" !j && !k ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_LNOT,ty),
			SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_LIOR,ty),
				      SIMPNODE_kid0(k0),SIMPNODE_kid0(k1)));
      SIMP_DELETE(k0);
      SIMP_DELETE(k1);
   }

   if (!Enable_Cfold_Aggressive || r) return (r);

   if ((SIMPNODE_operator(k0)==OPR_LNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),k1)==0) ||
       (SIMPNODE_operator(k1)==OPR_LNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k1),k0)==0)) {
      SHOW_RULE(" !j && j ");
      r = SIMP_INTCONST(ty,0);
      SIMP_DELETE_TREE(k0);
      SIMP_DELETE_TREE(k1);
   } else if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
      SHOW_RULE(" j && j ");
      r = k0;
      SIMP_DELETE_TREE(k1);
   }

   if (r) return (r);

   r = simp_factor(k0,k1,OPR_LIOR,opc,ty,FACTOR_ALL);
   if (!r) r = simp_factor_idty(k0,k1,OPR_LIOR,opc,ty,FALSE);

   return (r);
}


/*------------------------------------------------ 
   Simplifications for ||

j || 0			j
j || 1			1
!j || !k		!(j && k)

Aggressive:
!j || j			1			A
j || j 			j			A

-------------------------------------------------*/

static simpnode  simp_lior( OPCODE opc, 
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1;
   TYPE_ID  ty;

   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }
   ty = OPCODE_rtype(opc);

   if (k1const) {
      c1 = SIMP_Int_ConstVal(k1); 
      if (c1 == 0) {
	 SHOW_RULE("j||0");
	 r = k0;
	 SIMP_DELETE(k1);
      } else {
	 SHOW_RULE("j||1");
	 r = SIMP_INTCONST(ty,1);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE(k1);
      }
   } else if ((SIMPNODE_operator(k0)==OPR_LNOT) && (SIMPNODE_operator(k1)==OPR_LNOT)) {
      SHOW_RULE(" !j || !k ");
      r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_LNOT,ty),
			SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_LAND,ty),
				      SIMPNODE_kid0(k0),SIMPNODE_kid0(k1)));
      SIMP_DELETE(k0);
      SIMP_DELETE(k1);
   }

   if (!Enable_Cfold_Aggressive || r) return (r);

   if ((SIMPNODE_operator(k0)==OPR_LNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k0),k1)==0) ||
       (SIMPNODE_operator(k1)==OPR_LNOT && SIMPNODE_Simp_Compare_Trees(SIMPNODE_kid0(k1),k0)==0)) {
      SHOW_RULE(" !j || j ");
      r = SIMP_INTCONST(ty,1);
      SIMP_DELETE_TREE(k0);
      SIMP_DELETE_TREE(k1);
   } else if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
      SHOW_RULE(" j || j ");
      r = k0;
      SIMP_DELETE_TREE(k1);
   }

   if (r) return (r);

   r = simp_factor(k0,k1,OPR_LAND,opc,ty,FACTOR_ALL);
   if (!r) r = simp_factor_idty(k0,k1,OPR_LAND,opc,ty,TRUE);

   return (r);
}

/*------------------------------------------------ 
   Simplifications for && (with control flow)

j && 1			j
0 && j                  0
1 && j                  j

-------------------------------------------------*/

static simpnode  simp_cand( OPCODE opc,
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1;

   if (k0const) {
     c1 = SIMP_Int_ConstVal(k0); 
     if (c1 == 0) {
       SHOW_RULE(" 0 c&& j");
       r = SIMP_INTCONST(OPCODE_rtype(opc),0);
       SIMP_DELETE(k0);
       SIMP_DELETE_TREE(k1);
      } else {
	SHOW_RULE(" 1 c&& j");
	r = k1;
	SIMP_DELETE(k0);
      }
   } else if (k1const) {
     c1 = SIMP_Int_ConstVal(k1); 
     if (c1 != 0) {
       SHOW_RULE(" j c&& 1");
       r = k0;
       SIMP_DELETE(k1);
     }
     else 
#ifdef WN_SIMP_WORKING_ON_WHIRL
     if (! WN_has_side_effects(k0)) 
#endif
     {
       SHOW_RULE(" j c&& 0");
       r = SIMP_INTCONST(OPCODE_rtype(opc),0);
       SIMP_DELETE(k1);
       SIMP_DELETE_TREE(k0);
     }
   } 
   
   return (r);
}


/*------------------------------------------------ 
   Simplifications for || (with control flow)

j || 0			j
0 || j                  j
1 || j                  1

-------------------------------------------------*/

static simpnode  simp_cior( OPCODE opc, 
		      simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1;

#ifndef KEY // bug 9920
   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }
#endif

   if (k0const) {
     c1 = SIMP_Int_ConstVal(k0); 
     if (c1 == 0) {
       SHOW_RULE("0 c|| j");
       r = k1;
       SIMP_DELETE(k0);
     } else {
       SHOW_RULE("1 c|| j");
       r = SIMP_INTCONST(OPCODE_rtype(opc),1);
       SIMP_DELETE(k0);
       SIMP_DELETE_TREE(k1);
     }
   } else if (k1const) {
     c1 = SIMP_Int_ConstVal(k1); 
      if (c1 == 0) {
	SHOW_RULE("j c|| 0");
	r = k0;
	SIMP_DELETE(k1);
      } 
   }

   return (r);
}

/*------------------------------------------------ 
   Simplifications for <<, >>

j >> c1			j  if (c1 & shiftsize-1) == 0
j << c1			j  if (c1 & shiftsize-1) == 0
(j << c2) << c1		j << c1+c2, unless c1 + c2 >= max shift, in which case 0
(j >> c2) >> c1		j >> c1+c2, unless c1 + c2 >= max shift, in which case 0
(j >> c1) << c1		j & ~((1<<c1)-1)
(j << c1) lshr c1	j & ((1<<(wordsize-c1))-1)

(j<<c1) >> c2           appropriate EXTRACT
(j & mask) << c1        appropriate COMPOSE


(j >> c1)               0 if j is a load of an unsigned
                          short type and (c1 mod lengthofshift) > (lengthoftype)

(integerCVT(X) << c1)   X if c1 >= 32
(j << 32) ashr 32       I8U4CVT (j)


j shift (k & mask)      j << k  if mask & (shift_size-1) == shift_size-1

(j & mask) << k         j << k if lower (shift_length - k) bits are all 1's.

(j & c1) >> k           j >> k &
-------------------------------------------------*/

static simpnode  simp_shift( OPCODE opc,
		       simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64   c1,c2,mask;
   TYPE_ID  ty,rty;
   INT64  shift_size;
   BOOL   firstop_is_shift=FALSE;
   INT64  shift_size2;
   OPERATOR  firstop, op;
   OPCODE    first_opcode;
   simpnode tmp; 

   op = OPCODE_operator(opc);
   ty = OPCODE_rtype(opc);
   if (ty==MTYPE_U8 || ty==MTYPE_I8) {
      shift_size = 64;
   } else {
      shift_size = 32;
   }
   first_opcode = SIMPNODE_opcode(k0);
   firstop = OPCODE_operator(first_opcode);
   if (firstop == OPR_SHL || firstop == OPR_ASHR ||
       firstop == OPR_LSHR) {
      firstop_is_shift=TRUE;
      if (SIMPNODE_rtype(k0) == MTYPE_U8 ||
	  SIMPNODE_rtype(k0) == MTYPE_I8) {
	 shift_size2 = 64;
      } else {
	 shift_size2 = 32;
      }
   }

   if (k1const) {
      if (ARCH_mask_shift_counts) {
	 c1 = SIMP_Int_ConstVal(k1) & (shift_size-1);
      } else {
	 c1 = MIN((UINT64) SIMP_Int_ConstVal(k1),shift_size);
      }
     
      /* 
       * j >> c1, c1=0
       * j << c1, c1=0
       */
      if (c1 == 0) {
	 SHOW_RULE("j shift 0");
	 r = k0;
	 SIMP_DELETE(k1);
	 return (r);
      }
      /* 
       * j >> c1, c1 >= shift_size
       */
      if (c1 >= shift_size) {
	 if (op != OPR_ASHR) {
	    /* The two shifts shift off all the bits, so we get 0 */
	    SHOW_RULE("j shift big count -> 0");
	    r = SIMP_INTCONST(OPCODE_rtype(opc),0);
	    SIMP_DELETE_TREE(k0);
	    SIMP_DELETE(k1);
	 } else {  /* ASHR, just do one shift of size shift_size-1 */
	    SHOW_RULE("j ASHR bigcount");
	    r = SIMPNODE_SimpCreateExp2(opc,k0,SIMP_INTCONST(ty, shift_size-1));
	    SIMP_DELETE(k1);
	 }
	 return (r);
      }
      if (!WHIRL_Keep_Cvt_On &&
          op == OPR_SHL &&
	  firstop == OPR_CVT &&
	  SIMP_IS_TYPE_INTEGRAL(OPCODE_rtype(first_opcode)) &&
	  SIMP_IS_TYPE_INTEGRAL(OPCODE_desc(first_opcode)) &&
	  c1 >= 32) {
	 SHOW_RULE("Integer CVT << c1, c1>=32");
	 r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),k1);
	 SIMP_DELETE(k0);
      } else if (firstop_is_shift && shift_size==shift_size2 && 
		 SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
	 if (ARCH_mask_shift_counts) {
	    c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0)) & (shift_size-1);
	 } else {
	    c2 = MIN((UINT64) SIMP_Int_ConstVal(SIMPNODE_kid1(k0)),shift_size);
	 }
	 if (firstop == op ) {
	    /* 
	     * (j << c2) << c1		j << min(c1+c2,max possible shift)
	     * (j >> c2) >> c1		j >> min(c1+c2,max possible shift)
	     */
	    if ((c1 + c2) >= shift_size) {
	       if (op != OPR_ASHR) {
		  /* The two shifts shift off all the bits, so we get 0 */
		  SHOW_RULE("j shift c2 shift c1 -> 0");
		  r = SIMP_INTCONST(OPCODE_rtype(opc), 0);
		  SIMP_DELETE_TREE(k0);
		  SIMP_DELETE_TREE(k1);
	       } else {  /* ASHR, just do one shift of size shift_size-1 */
		  SHOW_RULE("j ASHR c2 ASHR c1");
		  r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),SIMP_INTCONST(ty, shift_size-1));
		  SIMP_DELETE(SIMPNODE_kid1(k0));
		  SIMP_DELETE(k0);
		  SIMP_DELETE(k1);
	       }
	    } else {
	       /* We can combine the two shifts */	
	       SHOW_RULE("j shift c2 shift c1");
	       r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),SIMP_INTCONST(ty, c1+c2));
	       SIMP_DELETE(SIMPNODE_kid1(k0));
	       SIMP_DELETE(k0);
	       SIMP_DELETE(k1);
	    }
	 } else if (firstop != OPR_SHL && op == OPR_SHL && c1==c2) {
	    SHOW_RULE("(j >> c1) << c1");
	    c2 = ~create_bitmask(c1);
	    r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BAND,ty),
			      SIMPNODE_kid0(k0),
			     SIMP_INTCONST(ty,c2));
	    SIMP_DELETE(SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	    SIMP_DELETE(k1);
	 } else if (firstop == OPR_SHL && op == OPR_LSHR && c1==c2) {
	    SHOW_RULE("(j << c1) LSHR c1");
	    c2 = create_bitmask(shift_size-c1);
	    r = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_BAND,ty),
			      SIMPNODE_kid0(k0),
			     SIMP_INTCONST(ty,c2));
	    SIMP_DELETE(SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	    SIMP_DELETE(k1);
         } else if (firstop == OPR_SHL && op == OPR_LSHR && Enable_extract_bits && c1 > c2) {
	   SHOW_RULE("(j << c1) LSHR c2");
	   INT16 boffset = c1 - c2;
	   INT16 bsize = shift_size - c1;
	   if (bsize < 1) bsize = 1;
#ifdef KEY
	   // When expanding into extract, need to take care of the endianness.
	   if (Target_Byte_Sex != Host_Byte_Sex)
	     boffset = MTYPE_bit_size(ty) - boffset - bsize;
#endif
	   r = SIMPNODE_SimpCreateExtract(shift_size == 32 ? OPC_U4EXTRACT_BITS : OPC_U8EXTRACT_BITS,
				       boffset, bsize,
				       SIMPNODE_kid0(k0));
	   SIMP_DELETE(SIMPNODE_kid1(k0));
	   SIMP_DELETE(k0);
	   SIMP_DELETE(k1);
	 } else if (firstop == OPR_SHL && op == OPR_ASHR && c1 == 32 && c2 == 32) {
	   rty = SIMPNODE_rtype(SIMPNODE_kid0(k0));
	   if (rty == MTYPE_I4 || rty == MTYPE_U4) {
	      SHOW_RULE("(j << 32) ASHR 32");
	      r = SIMPNODE_SimpCreateExp1(OPC_I8I4CVT,SIMPNODE_kid0(k0));
	      SIMP_DELETE(SIMPNODE_kid1(k0));
	      SIMP_DELETE(k0);
	      SIMP_DELETE(k1);
	   }
	   // This next one must follow this preceeding one, so that we preference
	   // the case c1 == c2 == 32
         } else if (firstop == OPR_SHL && op == OPR_ASHR && Enable_extract_bits && c1 >= c2) {
	   SHOW_RULE("(j << c1) ASHR c2");
	   INT16 boffset = c1 - c2;
	   INT16 bsize = shift_size - c1;
	   if (bsize < 1) bsize = 1;
#ifdef KEY
	   // When expanding into extract, need to take care of the endianness.
	   if (Target_Byte_Sex != Host_Byte_Sex)
	     boffset = MTYPE_bit_size(ty) - boffset - bsize;
#endif
	   r = SIMPNODE_SimpCreateExtract(shift_size == 32 ? OPC_I4EXTRACT_BITS : OPC_I8EXTRACT_BITS,
				       boffset, bsize,
				       SIMPNODE_kid0(k0));
	   SIMP_DELETE(SIMPNODE_kid1(k0));
	   SIMP_DELETE(k0);
	   SIMP_DELETE(k1);
	 }
      } else if (firstop == OPR_BAND && op == OPR_SHL &&
		 SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
#ifdef KEY
	 // When expanding into deposit, need to take care of the endianness.
	 if (Target_Byte_Sex != Host_Byte_Sex)
	   c1 = MTYPE_bit_size(ty) - c1 - log2((UINT64)c2+1);
#endif
	 /* See if the mask is all 1's in the right places */
	 mask = create_bitmask(shift_size-c1);
	 if ((c2 & mask) == mask) {
	    SHOW_RULE("(j & mask) << c1");
	    r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),k1);
	    SIMP_DELETE(SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	    return (r);
         } else if (Enable_compose_bits && IS_POWER_OF_2(c2+1)) {
	   SHOW_RULE("(j & mask) << c1 -> COMPOSE");
	   c2 = log2((UINT64)c2+1);
	   r = SIMPNODE_SimpCreateDeposit(OPC_FROM_OPR(OPR_COMPOSE_BITS,ty),c1,c2,
					  SIMP_INTCONST(ty,0),SIMPNODE_kid0(k0));
	   SIMP_DELETE(SIMPNODE_kid1(k0));
	   SIMP_DELETE(k0);
	   return (r);
	 }
      } else if (firstop == OPR_BAND && (op == OPR_ASHR || op == OPR_LSHR) &&
		 SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) && // KEY
		 shift_size == MTYPE_bit_size(OPCODE_rtype(first_opcode))) {
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	 SHOW_RULE("(j & mask) >> c1");
	 tmp = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid1(k0),SIMPNODE_CopyNode(k1));
	 r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),k1);
	 r = SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),r,tmp);
	 SIMP_DELETE(k0);
      } else {
	 rty = get_value_type(k0);
	 if (rty != ty && 
#ifdef KEY // SIMP_TYPE_SIZE(MTYPE_BS) is 0
	     rty != MTYPE_BS &&
#endif
	     		  ((op == OPR_LSHR)
#ifdef KEY // bug 2643
			    && SIMP_IS_TYPE_UNSIGNED(rty)
#endif
			   || (op == OPR_ASHR &&
			       SIMP_IS_TYPE_UNSIGNED(SIMP_TYPE(k0)) &&
			       SIMP_TYPE_SIZE(rty) < SIMP_TYPE_SIZE(ty)))) {
	    if (c1 >= SIMP_TYPE_SIZE(rty)) {
	       SHOW_RULE("short >> large c1 = 0");
	       r = SIMP_INTCONST(ty,0);
	    }
	 }
      }
   }
   if (r) return (r);

   /* Non-constant second child */
   if (SIMPNODE_operator(k1) == OPR_BAND &&
       SIMP_Is_Int_Constant(SIMPNODE_kid1(k1)) && ARCH_mask_shift_counts) /* KEY */ {
      c1 = SIMP_Int_ConstVal(SIMPNODE_kid1(k1));
      if  ((c1 & (shift_size-1)) == shift_size-1) {
	 SHOW_RULE ("j shift (X & mask)");
	 r = SIMPNODE_SimpCreateExp2(opc,k0,SIMPNODE_kid0(k1));
	 SIMPNODE_DELETE(SIMPNODE_kid1(k1));
	 SIMPNODE_DELETE(k1);
      }
   }
   return (r);
}



/*---------------------------------------------

Helper routine for simplifying expressions like
 x+y relop x+z  -> y relop z
 x+y relop x    -> y relop 0
 -x relop -y    -> y relop x

---------------------------------------------*/

static simpnode cancel_in_relop(OPCODE opc, TYPE_ID ty, simpnode k0, simpnode k1)
{
   simpnode r = NULL;
   simpnode t;
   simpnode dt;
   OPERATOR op0,op1,top,mainopr;
   char buf[64];  /* for the show message */
   OPCODE kidop;
   
   simpnode   x[4],lhs,rhs,del1,del2,del3;
   BOOL     s[4],s_lhs,s_rhs,pointer_seen;
   INT32    i,j,jmax;
   
   op0 = SIMPNODE_operator(k0);
   op1 = SIMPNODE_operator(k1);

   /* If we have a negate on both sides, remove it and flip the comparison */
   if (op0 == OPR_NEG && op1 == OPR_NEG) {
      SHOW_RULE("-x relop -y");
      r = SIMPNODE_SimpCreateExp2(OPCODE_commutative_op(opc),SIMPNODE_kid0(k0),SIMPNODE_kid0(k1));
      SIMPNODE_DELETE(k0);
      SIMPNODE_DELETE(k1);
      return (r);
   }

   /* Special for a pair of LDA's */
   if (op0 == OPR_LDA && op1 == OPR_LDA &&
       SIMPNODE_Compare_Symbols(k0,k1) == 0) {
      SHOW_RULE("LDA relop LDA");
      lhs = SIMP_INTCONST(MTYPE_I8,SIMPNODE_lda_offset(k0));
      rhs = SIMP_INTCONST(MTYPE_I8,SIMPNODE_lda_offset(k1));
      opc = OPCODE_make_op(OPCODE_operator(opc),MTYPE_I4,MTYPE_I8);
      SIMPNODE_DELETE(k0);
      SIMPNODE_DELETE(k1);
      r = SIMPNODE_SimpCreateExp2(opc,lhs,rhs);
   }

   if (!Enable_Cfold_Aggressive) return (r);
   if (SIMP_IS_TYPE_FLOATING(ty) &&
       (!Enable_Cfold_Reassociate || Force_IEEE_Comparisons
	|| Roundoff_Level < ROUNDOFF_ANY)) return(r);

   /* might we have nothing to do? */
   if (op0 != OPR_ADD && op1 != OPR_ADD && op0 != OPR_SUB && op1 != OPR_SUB) return (r);


   /* mini canonicalization, put +- on left */
   if (op0 != OPR_ADD && op0 != OPR_SUB) {
      /* Swap around */
      opc = OPCODE_commutative_op(opc);
      t = k0; k0 = k1; k1 = t;
      top = op0; op0 = op1; op1 = top;
   }

   /* mini canonicalization, put + on left */
   if (op0 != OPR_ADD && op1 == OPR_ADD) {
      /* Swap around */
      opc = OPCODE_commutative_op(opc);
      t = k0; k0 = k1; k1 = t;
      top = op0; op0 = op1; op1 = top;
   }

   mainopr = OPCODE_operator(opc);

   /* Pick up the 3 or 4 pieces */
   s[0] = 0;
   s[1] = (op0 == OPR_SUB);
   x[0] = SIMPNODE_kid0(k0);
   x[1] = SIMPNODE_kid1(k0);
   
   if (op1 == OPR_ADD || op1 == OPR_SUB) {
      s[2] = 0;
      s[3] = (op1 == OPR_SUB);
      x[2] = SIMPNODE_kid0(k1);
      x[3] = SIMPNODE_kid1(k1);
      jmax = 3;
   } else if (op1 == OPR_NEG) {
      s[2] = 1;
      x[2] = SIMPNODE_kid0(k1);
      x[3] = NULL;
      jmax = 2;
   } else {
      s[2] = 0;
      x[2] = k1;
      x[3] = NULL;
      jmax = 2;
   }

   /* Check for pointer arithmetic in any of the X's */
   pointer_seen = FALSE;
   if (mainopr != OPR_EQ && mainopr != OPR_NE) {  /* We only want to special case for 
						     <, <=, > ,>= */
      for (i=0; i <= jmax; i++) {
	 kidop = SIMPNODE_opcode(x[i]);
	 if (OPCODE_has_1ty(kidop) || OPCODE_has_2ty(kidop)) {
	    if (TY_kind (SIMPNODE_ty(x[i])) == KIND_POINTER) {
	       pointer_seen = TRUE;
	       break;
	    }
	 } else if (OPCODE_operator(kidop) == OPR_LDA) {
	    pointer_seen = TRUE;
	    break;
	 }
      }
   }
   
   /* Can't do this for unsigned's either */
   if (SIMP_IS_TYPE_UNSIGNED(ty) && !pointer_seen && !Simp_Fold_Unsigned_Relops &&
       (mainopr != OPR_EQ && mainopr != OPR_NE)) return(r);

   /* Don't do anything if we are trying to make sure that overflow doesn't change the answers */
   if ((mainopr != OPR_EQ && mainopr != OPR_NE) &&
       SIMP_IS_TYPE_INTEGRAL(ty) && !SIMP_IS_TYPE_UNSIGNED(ty) &&
       !Simp_Unsafe_Relops) return (r);


   /* Do all the pairwise comparisons */
   for (i=0; i <= 1; i++) {
      for (j=2; j <= jmax; j++) {
	 if (s[i]==s[j] && (dt = simp_diff_value(x[i],x[j],s[i]))) {
	    /* We've found a match, cancel and return */
	    /* get the remaining quantities */
	    lhs = x[1-i]; s_lhs = s[1-i];
	    rhs = x[5-j]; s_rhs = s[5-j];
	    if (!rhs) {
	       rhs = dt;
	       if (!s_lhs) {
		  rhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),rhs);
	       }
	       s_rhs = s_lhs; /* We always want to give RHS and LHS the same sign if possible */
	    } else {
	       /* We need to fix up the lhs with the offset */
	       if (s_lhs) {
		  dt = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),dt);
	       }
	       lhs = SIMPNODE_SimpCreateExp2(OPC_FROM_OPR(OPR_ADD,ty),lhs,dt);
	    }
	    
	    if (pointer_seen) {
	    /* Special treatment of pointer comparisons. After
	     *  cancellation, we need to do a SIGNED comparison on the two
	     *  remaining arguments cases.
	     */
	       sprintf(buf,"(pointers) x+y relop x+z %2d %2d\n",i,j);
	       SHOW_RULE(buf);
	       /* Convert to signed comparison */
	       if (ty == MTYPE_U8) {
		  opc = OPCODE_make_op(mainopr,OPCODE_rtype(opc),MTYPE_I8);
	       } else if (ty == MTYPE_U4) {
		  opc = OPCODE_make_op(mainopr,OPCODE_rtype(opc),MTYPE_I4);
	       }
	       if (s_lhs && s_rhs) {
		  r = SIMPNODE_SimpCreateExp2(opc,rhs,lhs);
	       } else {
		  if (s_lhs) {
		     lhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),lhs);
		  }
		  if (s_rhs) {
		     rhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),rhs);
		  }
		  r = SIMPNODE_SimpCreateExp2(opc,lhs,rhs);
	       }
	       SIMP_DELETE_TREE(x[i]);
	       SIMP_DELETE_TREE(x[j]);
	       SIMP_DELETE(k0);
	       if (k1 != x[j]) SIMP_DELETE(k1);
	       return (r);
	       
	    } else {  /* Non-pointer case */
	       sprintf(buf,"x+y relop x+z %2d %2d\n",i,j);
	       SHOW_RULE(buf);
	       
	       /* Build lhs op rhs with appropriate signs */
	       /* special case: left and right sides both have negates */
	       if (s_lhs && s_rhs) {
		  r = SIMPNODE_SimpCreateExp2(opc,rhs,lhs);
	       } else {
		  if (s_lhs) {
		     lhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),lhs);
		  }
		  if (s_rhs) {
		     rhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),rhs);
		  }
		  r = SIMPNODE_SimpCreateExp2(opc,lhs,rhs);
	       }
	       SIMP_DELETE_TREE(x[i]);
	       SIMP_DELETE_TREE(x[j]);
	       SIMP_DELETE(k0);
	       if (k1 != x[j]) SIMP_DELETE(k1);
	       return (r);
	    }
	 }
      }
   }
   if (r) return (r);

   /* Now we need to check each side against the pieces of the other side,
    We only need to do this if we had add/sub ops on both sides */
   if (jmax != 3) return (r); 

   /* compare LHS against pieces of RHS */
   lhs = NULL;
   rhs = NULL;
   
   for (i=0; i <= 1; i++) {
      if (s[i] == 0 && (dt = simp_diff_value(k1,x[i],TRUE))) {
	 lhs = x[1-i];
	 s_lhs = s[1-i];
	 del1 = x[i];
	 del2 = k1;
	 del3 = k0;
	 break;
      } else if (s[i+2] == 0 && (dt = simp_diff_value(k0,x[i+2],FALSE))) {
	 rhs = x[3-i];
	 s_rhs = s[3-i];
	 del1 = x[i+2];
	 del2 = k0;
	 del3 = k1;
	 break;
      }
   }
   if (lhs || rhs) { /* We found something */
      if (!rhs) {
	 rhs = dt;
	 s_rhs = 0;
      }
      if (!lhs) {
	 lhs = dt;
	 s_lhs = 0;
      }
	    
      if (pointer_seen) {
	 /* Special treatment of pointer comparisons. After
	  * cancellation, we need to do a SIGNED comparison on the two
	  * remaining arguments cases.
	  */
	 SHOW_RULE("(pointers) x+y relop x");
	 /* Convert to signed comparison */
	 if (ty == MTYPE_U8) {
	    opc = OPCODE_make_op(mainopr,OPCODE_rtype(opc),MTYPE_I8);
	 } else if (ty == MTYPE_U4) {
	    opc = OPCODE_make_op(mainopr,OPCODE_rtype(opc),MTYPE_I4);
	 }
	 if (s_lhs && s_rhs) {
	    r = SIMPNODE_SimpCreateExp2(opc,rhs,lhs);
	 } else {
	    if (s_lhs) {
	       lhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),lhs);
	    }
	    if (s_rhs) {
	       rhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),rhs);
	    }
	    r = SIMPNODE_SimpCreateExp2(opc,lhs,rhs);
	 }
	 /* Clean up the old trees */
	 SIMP_DELETE_TREE(del1);
	 SIMP_DELETE_TREE(del2);
	 SIMP_DELETE(del3);
	 return (r);
	 
      } else {  /* Non-pointer case */
	 SHOW_RULE(" x+y relop x");
	 /* Build lhs op rhs with appropriate signs */
	 /* special case: left and right sides both have negates */
	 if (s_lhs && s_rhs) {
	    r = SIMPNODE_SimpCreateExp2(opc,rhs,lhs);
	 } else {
	    if (s_lhs) {
	       lhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),lhs);
	    }
	    if (s_rhs) {
	       rhs = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_NEG,Mtype_TransferSign(MTYPE_I4,ty)),rhs);
	    }
	    r = SIMPNODE_SimpCreateExp2(opc,lhs,rhs);
	 }
	 /* Clean up the old trees */
	 SIMP_DELETE_TREE(del1);
	 SIMP_DELETE_TREE(del2);
	 SIMP_DELETE(del3);
	 return (r);
      }
   }
   return (r);
}

/*------------------------------------------------ 
   Simplifications for ==, !=

(x reloporlogop y)==1	x reloporlogop y 
(x reloporlogop y)!=0	x reloporlogop y
(x relop y)==0		x inverserelop Y		
(x relop y)!=1		x inverserelop Y
(x logop y)==0		!(x logop Y)
(x logop y)!=1		!(x logop Y)
(j & c2) != c1 		1 if (c1 & ~c2) is non-zero
(j & c2) == c1 		0 if (c1 & ~c2) is non-zero
(j | c2) != c1 		1 if (c2 & ~c1) is non-zero
(j | c2) == c1 		0 if (c2 & ~c1) is non-zero
(~j & c2) == 0		(j & c2) != 0 if c2 is a power of 2

(j +- c2) relop c1      j relop (c1 -+ c2)
(c2 - j) relop c1      j relop (c2 - c1)
(j * c2) op c1         j op c1/c2 if c2 divides c1


 Aggressive:
j==j			1			A
j!=j			0			A
a==a			1			A, IEEE_comparisons is off
a!=a			0			A, IEEE_comparisons is off

x+-y op x+-z          y op z  (and related)   A, IEEE_comparisons is off, Enable_Cfold_Reassociate on 
x+-y op x             y op 0                  A, IEEE_comparisons is off, Enable_Cfold_Reassociate on 

-------------------------------------------------*/

static simpnode
simp_eq_neq (OPCODE opc, simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL, expr;
   INT64 c1,c2,c3;
   TYPE_ID  ty;
   BOOL   iseq;
   OPCODE firstop,inv_op;
    
   iseq = (OPCODE_operator(opc) == OPR_EQ);

   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }
    
   firstop = SIMPNODE_opcode(k0);
   inv_op = get_inverse_relop(firstop);
   ty = OPCODE_rtype(firstop);

   if (k1const && SIMP_IS_TYPE_INTEGRAL(ty)) {
#ifdef KEY // bug 5208
      if (SIMP_Is_Str_Constant (k1))
        c1 = Targ_To_Host( SIMP_Str_ConstVal (k1) );
      else
#endif
      c1 = SIMP_Int_ConstVal(k1); 
      if (c1 == 0 || c1 == 1) {
	 if (
#ifdef KEY // bug 9878
	     (is_logop(firstop) || inv_op) && 
#else
	     ((is_logop(firstop) && (ty == OPCODE_rtype(opc))) || inv_op) && 
#endif
	     ((iseq && c1==1) || (!iseq && c1==0))){
	    /*
	     * (x reloporlogop y)==1
	     * (x reloporlogop y)!=0
	     */
	    SHOW_RULE("x reloporlogop y ==1,!=0");
#ifdef KEY // bug 9878
	    if (SIMPNODE_kid_count(k0) == 1)
	      r = SIMPNODE_SimpCreateExp1(
		OPCODE_make_op(OPCODE_operator(firstop),
			       OPCODE_rtype(opc),
			       OPCODE_desc(firstop)),
		SIMPNODE_kid0(k0));
	    else
#endif
	    r = SIMPNODE_SimpCreateExp2(
		OPCODE_make_op(OPCODE_operator(firstop),
			       OPCODE_rtype(opc),
			       OPCODE_desc(firstop)),
		SIMPNODE_kid0(k0),SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	    SIMP_DELETE(k1);
	 } else if (is_logop(firstop) &&
		    ((iseq && c1==0) || (!iseq && c1==1))) {
	    /*
	     * (x logop y)==0
	     * (x logop y)!=1
	     */
	    SHOW_RULE("x logop y ==0,!=1");
	    r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_LNOT,ty), k0);
	    SIMP_DELETE(k1);
	 } else if (inv_op &&
		    ((iseq && c1==0) || (!iseq && c1==1))) {
	    /*
	     * (x relop y)==0
	     * (x relop y)!=1
	     */
	    if (!SIMP_IS_TYPE_FLOATING(OPCODE_desc(inv_op)) ||
		!Force_IEEE_Comparisons) {
	       SHOW_RULE("x relop y ==0,!=1");
	       OPCODE inv_relop = OPCODE_make_op(OPCODE_operator(inv_op),
						 OPCODE_rtype(opc),
						 OPCODE_desc(inv_op));
	       r = SIMPNODE_SimpCreateExp2(inv_relop,
					   SIMPNODE_kid0(k0),
					   SIMPNODE_kid1(k0));
	       SIMP_DELETE(k1);
	       SIMP_DELETE(k0);
	    }
	 } else if (iseq &&
		    (c1 == 0) &&
		    (OPCODE_operator(firstop)==OPR_BAND) &&
		    (SIMPNODE_operator(SIMPNODE_kid0(k0))==OPR_BNOT) &&
		    SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */ {
	    c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	    expr = SIMPNODE_kid0(SIMPNODE_kid0(k0));
	    if ((c2 != 0) && IS_POWER_OF_2(c2)) {
	       SHOW_RULE(" (~j & c2) == 0	(j & c2) != 0 if c2 is a power of 2 ");
	       /* c2 is a power of 2 not equal to 0 */
	       r = SIMPNODE_SimpCreateExp2(firstop, expr, SIMPNODE_kid1(k0));
	       r = SIMPNODE_SimpCreateExp2(get_inverse_relop(opc), /* this will be != */
					   r,k1);
	       SIMP_DELETE(SIMPNODE_kid0(k0));
	       SIMP_DELETE(k0);
		    
	    }
	 }

	 /* Only do this simplification if the data structure is WHIRL.
	  * This is so we don't need to make the optimizer reconstruct a lot
	  * of stuff about the symbols.
	  */

	 // LDA optimizations
	 else if (c1 == 0 && OPCODE_operator(firstop)==OPR_LDA) {
	    ST_IDX base_idx;
	    INT64 offset;
	    LDA_FLAGS l;
	    l = get_lda_info(k0,offset,base_idx);
	    if ((l & LDA_CANNOT_BE_ZERO) != 0) {
	       /* Symbol cannot be 0 */
	       /* &x == 0 or &x != 0 */
	       if (iseq) {
		  SHOW_RULE("&x == 0");
		  r = SIMP_INTCONST(OPCODE_rtype(opc), 0);
	       } else {
		  SHOW_RULE("&x != 0");
		  r = SIMP_INTCONST(OPCODE_rtype(opc), 1);
	       }
	       SIMP_DELETE_TREE(k0);
	       SIMP_DELETE_TREE(k1);
	    }
	 }
      }

      /* Exit here for code cleanliness if we did something */
      if (r) return (r);


      if ((!ARCH_has_bit_tests) &&
	  (c1 == 0) &&
	  (OPCODE_operator(firstop)==OPR_BAND) && 
	  SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) && // KEY
	  (SIMP_Int_ConstVal(SIMPNODE_kid1(k0))==1)) { 
	simpnode k0k0 = SIMPNODE_kid0(k0);
	if (((SIMPNODE_operator(k0k0) == OPR_LSHR) ||
	     (SIMPNODE_operator(k0k0) == OPR_ASHR)) &&
	    SIMP_Is_Int_Constant(SIMPNODE_kid1(k0k0))) /* KEY */{
	    ty = SIMPNODE_rtype(k0k0);
	    c2 = MTYPE_bit_size(ty) - 1 - SIMP_Int_ConstVal(SIMPNODE_kid1(k0k0));
	    SHOW_RULE("(x>>c1)&1 ==,!= 0");
	    /* This one is useful for bit field extractions on MIPS */
	    r = SIMPNODE_SimpCreateExp2(MTYPE_bit_size(ty) == 32 ? OPC_I4SHL : OPC_I8SHL,
					SIMPNODE_kid0(k0k0),SIMP_INTCONST(MTYPE_I4,c2));
	    if (MTYPE_is_unsigned(ty)) ty = MTYPE_complement(ty);
	    OPCODE newopc = OPCODE_make_op(iseq ? OPR_GE : OPR_LT,
					   OPCODE_rtype(opc),
					   ty);
	    r = SIMPNODE_SimpCreateExp2(newopc,r,SIMP_INTCONST(ty,0));
	    SIMP_DELETE(SIMPNODE_kid1(k0k0));
	    SIMP_DELETE(k0k0);
	    SIMP_DELETE(k1);
	    SIMP_DELETE(SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	 }
      } else if ((OPCODE_operator(firstop)==OPR_BIOR ||
		   OPCODE_operator(firstop)==OPR_BAND) &&
		  SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
	  c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
#ifdef KEY // bug 2846: handle case where c2 is -0x80000000 and c1 is 0x80000000
	  if (MTYPE_bit_size(OPCODE_rtype(opc)) == 32) {
	    c2 &= 0xffffffff;
	    c1 &= 0xffffffff;
	  }
#endif
	 /*
	  *  (j & c2) == c1 	0 if (c1 & ~c2) is non-zero
	  *  (j | c2) == c1 	0 if (c2 & ~c1) is non-zero
	  *  (j & c2) != c1 	1 if (c1 & ~c2) is non-zero
	  *  (j | c2) != c1 	1 if (c2 & ~c1) is non-zero
	  */
     	 if (((c1 & ~c2)!=0 && OPCODE_operator(firstop)==OPR_BAND) ||
	     ((c2 & ~c1)!=0 && OPCODE_operator(firstop)==OPR_BIOR)) {
	    if (iseq) {
	       c2 = 0;
	    } else {
	       c2 = 1;
	    }
	    SHOW_RULE("(j & c2) == c1 ->0 et al");
	    r = SIMP_INTCONST(OPCODE_rtype(opc), c2);
	    SIMP_DELETE_TREE(k0);
	    SIMP_DELETE_TREE(k1);
	 }
      } else if ((OPCODE_operator(firstop)==OPR_ADD ||
		  OPCODE_operator(firstop)==OPR_SUB) &&
		 SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	 /*
	  *  (j +- c2) op c1    j op (c1 -+ c2)
	  */
	 if (OPCODE_operator(firstop)==OPR_ADD) {
	    c2 = -c2;
	 }
#ifndef KEY
	 c3 = c1 + c2;
#else
	 if (is_add_ok(&c3,c1,c2,ty) || MTYPE_byte_size(ty) == 8) {
#endif
	 SHOW_RULE("(j -+ c2) == c1");
	 r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),
				     SIMP_INTCONST(OPCODE_rtype(firstop), c3));
	 SIMP_DELETE(SIMPNODE_kid1(k0));
	 SIMP_DELETE(k0);
	 SIMP_DELETE(k1);
#ifdef KEY
	 }
#endif
      } else if (OPCODE_operator(firstop)==OPR_SUB &&
		 SIMP_Is_Int_Constant(SIMPNODE_kid0(k0))) /* KEY */ {
	 /*
	  *  (c2 - j) op c1    j op (c2 - c1)
	  */
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid0(k0));
#ifndef KEY
	 c3 = c2 - c1;
#else
	 if (is_sub_ok(&c3,c2,c1,ty) || MTYPE_byte_size(ty) == 8) {
#endif
	 SHOW_RULE("(j -+ c2) == c1");
	 r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid1(k0),
				     SIMP_INTCONST(OPCODE_rtype(firstop), c3));
	 SIMP_DELETE(SIMPNODE_kid0(k0));
	 SIMP_DELETE(k0);
	 SIMP_DELETE(k1);
#ifdef KEY
	 }
#endif
      } else if (OPCODE_operator(firstop)==OPR_MPY &&
		 SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
	 /*
	  *  (j * c2) op c1    j op (c1 / c2), if c2 divides c1
	  */
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	 if (c2 != 0) {
	    if ((c1/c2)*c2 == c1) { 
	       SHOW_RULE("(j * c2) == c1 divides");
	       r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),
					   SIMP_INTCONST(SIMPNODE_rtype(k1), c1/c2));
	       SIMP_DELETE(SIMPNODE_kid1(k0));
	       SIMP_DELETE(k0);
	       SIMP_DELETE(k1);
	    } else {
	       SHOW_RULE("(j * c2) == c1 nodivide");
	       if (iseq) {
		  c2 = 0;
	       } else {
		  c2 = 1;
	       }
	       r = SIMP_INTCONST(OPCODE_rtype(opc), c2);
	       SIMP_DELETE_TREE(k0);
	       SIMP_DELETE_TREE(k1);
	    }
	 }
      }
   }
    
    
   if (!Enable_Cfold_Aggressive || r) return (r);
   
   
   if (!Force_IEEE_Comparisons || SIMP_IS_TYPE_INTEGRAL(OPCODE_desc(opc))) {
      if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
	 if (iseq) {
	    c1 = 1;
	    SHOW_RULE("x==x");
	 } else {
	    c1 = 0;
	    SHOW_RULE("x!=x");
	 }
	 r = SIMP_INTCONST(OPCODE_rtype(opc), c1);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE_TREE(k1);
      }
   }
   if (r) return (r);

   if (OPCODE_operator(firstop)==OPR_LDA &&
       SIMPNODE_operator(k1) == OPR_LDA) {
      YESNOMAYBE lda_comp;
      lda_comp = LDA_Equal_Address(k0,k1);
      if (lda_comp == YES) {
	 SHOW_RULE("&x == &y (true)");
	 r = SIMP_INTCONST(OPCODE_rtype(opc), iseq);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE_TREE(k1);
      } else if (lda_comp == NO) {
	 SHOW_RULE("&x == &y (false)");
	 r = SIMP_INTCONST(OPCODE_rtype(opc), !iseq);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE_TREE(k1);
      }	       
   }
   if (r) return (r);

   r = cancel_in_relop(opc, OPCODE_desc(opc), k0, k1);

   return (r);
}

/*------------------------------------------------ 
   Simplifications for <, > <=, >=

For integers only:

x >= MIN VALUE          1
x >  MAX VALUE          0
x <= MAX VALUE          1
x <  MIN VALUE          0

(x +- c2) op c1         x op (c1 -+ c2)
(c2 - x) op c1          x reversed op (c2 - c1)
x*c1 op c2              x newop c3, c3 is something close to c2/c1

i <= N + -1	 i < N	
i > N + -1	 i >= N	

i < N + 1	 i <= N	
i >= N + 1	 i > N	


 Aggressive:
x <= x			1 (For floating types only if !Force_IEEE_Comparisons)
x >= x                  1  "
x < x                   0  "
x > x                   0  "

-------------------------------------------------*/

static simpnode  simp_relop(OPCODE opc,
		       simpnode k0, simpnode k1, BOOL k0const, BOOL k1const)
{
   simpnode r = NULL;
   INT64 c1,c2,c3;
   TYPE_ID  ty, rtyp;
   BOOL   c1min,c1max;
   BOOL   add_fold_ok;
   OPERATOR  op;
   OPCODE  newopc;
   
   op = OPCODE_operator(opc);
   
   /* k0const should always be false. If it isn't we messed up
      some canonicalizations */
   if (k0const) {
      return (r);
   }

   ty = OPCODE_desc(opc);
   rtyp = OPCODE_rtype(opc);

   /* Need to check for trying to move an unsigned ADD through a signed equality */
   add_fold_ok = (!SIMP_IS_TYPE_UNSIGNED(ty) || Simp_Fold_Unsigned_Relops) && 
      Simp_Unsafe_Relops;
   
   if (!SIMP_IS_TYPE_UNSIGNED(ty) &&
       (SIMP_IS_TYPE_UNSIGNED(SIMPNODE_rtype(k0)) || 
	SIMP_IS_TYPE_UNSIGNED(SIMPNODE_rtype(k1)))) {
      add_fold_ok = Simp_Fold_Unsigned_Relops;
   }
   
   /* Only do integral types */
   if (k1const && SIMP_IS_TYPE_INTEGRAL(ty)) {
      c1 = SIMP_Int_ConstVal(k1);
      c1min = FALSE;
      c1max = FALSE;
      switch (ty) {
       case MTYPE_I4:
	 c1min = (c1 == INT32_MIN);
	 c1max = (c1 == INT32_MAX);
	 break;
       case MTYPE_I8:
	 c1min = (c1 == 0x8000000000000000LL);
	 c1max = (c1 == 0x7fffffffffffffffLL);
	 break;
       case MTYPE_U4:
	 c1min = (c1 == 0);
	 c1max = (c1 == UINT32_MAX);
	 break;
       case MTYPE_U8:
	 c1min = (c1 == 0);
	 c1max = (c1 == 0xffffffffffffffffLL);
	 break;
      }
      if (c1min && op == OPR_LT) {
	 SHOW_RULE("x < MIN");
	 r = SIMP_INTCONST(rtyp, 0);
      } else if (c1min && op == OPR_GE) {
	 SHOW_RULE("x >= MIN");
	 r = SIMP_INTCONST(rtyp, 1);
      } else if (c1max && op == OPR_GT) {
	 SHOW_RULE("x > MAX");
	 r = SIMP_INTCONST(rtyp, 0);
      } else if (c1max && op == OPR_LE) {
	 SHOW_RULE("x <= MAX");
	 r = SIMP_INTCONST(rtyp, 1);
      }
      if (r) {
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE_TREE(k1);
	 return (r);
      }
      
      
      if ((SIMPNODE_operator(k0) == OPR_ADD) &&
	  SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) && // KEY
	  add_fold_ok) {
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	 if (is_sub_ok(&c3,c1,c2,ty)) {
	    SHOW_RULE ("j + c2 relop c1");
	    r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),
					SIMP_INTCONST(ty,c3));
	    SIMP_DELETE(k1);
	    SIMP_DELETE(SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	 }
      } else if (SIMPNODE_operator(k0) == OPR_SUB && add_fold_ok) {
	 if (SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
	    c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	    if (is_add_ok(&c3,c1,c2,ty)) {
	       SHOW_RULE ("j - c2 relop c1");
	       r = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid0(k0),
					   SIMP_INTCONST(ty,c3));
	       SIMP_DELETE(k1);
	       SIMP_DELETE(SIMPNODE_kid1(k0));
	       SIMP_DELETE(k0);
	    }
	 } else if (SIMP_Is_Int_Constant(SIMPNODE_kid0(k0))) /* KEY */ {
	    c2 = SIMP_Int_ConstVal(SIMPNODE_kid0(k0));
	    if (is_sub_ok(&c3,c2,c1,ty)) {
	       SHOW_RULE ("c2 - j relop c1");
	       r = SIMPNODE_SimpCreateExp2(OPCODE_commutative_op(opc),SIMPNODE_kid1(k0),
					   SIMP_INTCONST(ty,c3));
	       SIMP_DELETE(k1);
	       SIMP_DELETE(SIMPNODE_kid0(k0));
	       SIMP_DELETE(k0);
	    }
	 }
      } else if (SIMPNODE_operator(k0)==OPR_MPY && 
		 SIMP_Is_Int_Constant(SIMPNODE_kid1(k0)) && // KEY
		 add_fold_ok) {
	 /*
	  *  (j * c2) relop c1    j newrelop (c1 / c2), if c2 divides c1
	  */
	 c2 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
	 if (c2!=0) {
	    BOOL divides=FALSE;
	    BOOL sign_c3;
	    c3 = c1/c2;
	    sign_c3 = (c1 ^ c2) < 0;
	    if (c1 == c3*c2) divides = TRUE;
	    newopc = (c2 > 0) ? opc : OPCODE_commutative_op(opc);
	    if (divides) {
	       SHOW_RULE("(j * c2) == c1 (divides)");
	       r = SIMPNODE_SimpCreateExp2(newopc,SIMPNODE_kid0(k0),
					   SIMP_INTCONST(ty, c3));
	       SIMP_DELETE(SIMPNODE_kid1(k0));
	       SIMP_DELETE(k0);
	       SIMP_DELETE(k1);
	    } else {
	       /* tricky case, it doesn't divide evenly */
	       if ((op == OPR_LT && c1 > 0) ||
		   (op == OPR_GT && c1 < 0) ||
		   (op == OPR_LE && c1 < 0) ||
		   (op == OPR_GE && c1 > 0)) {
		  /* we need to move |c3| one greater */
		  if (sign_c3) {
		     c3 -= 1;
		  } else {
		     c3 += 1;
		  }
	       }  
	       SHOW_RULE("(j * c2) == c1 (no divides)");
	       r = SIMPNODE_SimpCreateExp2(newopc,SIMPNODE_kid0(k0),
					   SIMP_INTCONST(ty, c3));
	       SIMP_DELETE(SIMPNODE_kid1(k0));
	       SIMP_DELETE(k0);
	       SIMP_DELETE(k1);
	    }
	 }  
      }
   }

   if (r) return (r);
   
   /* 
      i <= N + -1	 i < N	
      i > N + -1	 i >= N	
      
      i < N + 1	         i <= N	
      i >= N + 1	 i > N	

   */

   if (SIMP_IS_TYPE_INTEGRAL(ty) && add_fold_ok &&
       SIMPNODE_operator(k1) == OPR_ADD &&
       SIMP_Is_Int_Constant(SIMPNODE_kid1(k1))) /* KEY */{
      c1 = SIMP_Int_ConstVal(SIMPNODE_kid1(k1));
      if (c1==1 && op == OPR_LT) {
	 SHOW_RULE("i < j+1");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_LE,rtyp,ty),k0,SIMPNODE_kid0(k1));
      }
      if (c1==1 && op == OPR_GE) {
	 SHOW_RULE("i >= j+1");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_GT,rtyp,ty),k0,SIMPNODE_kid0(k1));
      }
      if (c1==-1 && op == OPR_GT) {
	 SHOW_RULE("i > j-1");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_GE,rtyp,ty),k0,SIMPNODE_kid0(k1));
      }
      if (c1==-1 && op == OPR_LE) {
	 SHOW_RULE("i <= j-1");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_LT,rtyp,ty),k0,SIMPNODE_kid0(k1));
      }
      if (r) {
	 SIMP_DELETE(SIMPNODE_kid1(k1));
	 SIMP_DELETE(k1);
	 return (r);
      }
   }
   /* 
      N-1 >= i      N > i	
      N-1 < i	    N <= i
      
      N+1 > i       N >= i
      N+1 <= i      N < i
   */

   if (SIMP_IS_TYPE_INTEGRAL(ty) && add_fold_ok &&
       SIMPNODE_operator(k0) == OPR_ADD &&
       Simp_Unsafe_Relops &&
       SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
      c1 = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
      if (c1==-1 && op == OPR_LT) {
	 SHOW_RULE("j-1 < i");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_LE,rtyp,ty),SIMPNODE_kid0(k0),k1);
      }
      if (c1==-1 && op == OPR_GE) {
	 SHOW_RULE("j-1 >= i");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_GT,rtyp,ty),SIMPNODE_kid0(k0),k1);
      }
      if (c1==1 && op == OPR_GT) {
	 SHOW_RULE("j+1 > i");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_GE,rtyp,ty),SIMPNODE_kid0(k0),k1);
      }
      if (c1==1 && op == OPR_LE) {
	 SHOW_RULE("j+1 <= i");
	 r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_LT,rtyp,ty),SIMPNODE_kid0(k0),k1);
      }
      if (r) {
	 SIMP_DELETE(SIMPNODE_kid1(k0));
	 SIMP_DELETE(k0);
	 return (r);
      }
   }
   
   /* Aggressive optimizations */
   
   if (!Enable_Cfold_Aggressive || r) return (r);

   if (!Force_IEEE_Comparisons || SIMP_IS_TYPE_INTEGRAL(ty)) {
      if (SIMPNODE_Simp_Compare_Trees(k0,k1)==0) {
	 if (op == OPR_LE || op == OPR_GE) {
	    c1 = 1;
	    SHOW_RULE("x<=x, x>=x");
	 } else {
	    c1 = 0;
	    SHOW_RULE("x<x, x>x");
	 }
	 r = SIMP_INTCONST(rtyp, c1);
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE_TREE(k1);
      }
   }

   if (r) return (r);
   r = cancel_in_relop(opc, ty, k0, k1);
   
   return (r);
}
/*================================================================
================================================================
================================================================

End of simplifications

================================================================
================================================================
================================================================*/

/*
 * This array is a dispatch table to a simplifier function for each
 * operator. If an entry is NULL, there is no simplifier for that operator.
 * This table is automatically generated by opcode_gen.
 */

#include "wn_simp_ftable.h"


/*---------------------------------
Do the transformation 

SELECT(x, c0, c1) op c2 -> SELECT(x,c0 op c2, c1 op c2)

-----------------------------------*/

static simpnode Fold2_Into_Select(OPCODE opc, simpnode k0, simpnode k1)
{
   simpnode r = NULL;
   simpnode sk1,sk2,kt;

#ifdef KEY
   if (OPCODE_operator(opc) == OPR_COMPOSE_BITS)
     return r; // no advantage
#endif
   
   if (SIMPNODE_operator(k0)==OPR_SELECT) {
      sk1 = SIMPNODE_kid1(k0);
      sk2 = SIMPNODE_kid(k0,2);
      if (SIMP_Is_Constant(sk1) &&
	  SIMP_Is_Constant(sk2)) {
	 kt = SIMPNODE_CopyNode(k1);
	 r = SIMPNODE_SimpCreateExp3(OPC_FROM_OPR(OPR_SELECT,OPCODE_rtype(opc)),
				     SIMPNODE_kid0(k0),
				     SIMPNODE_SimpCreateExp2(opc,sk1,k1),
				     SIMPNODE_SimpCreateExp2(opc,sk2,kt));
	 SHOW_RULE("SELECT(x,c1,c2) op c0");
      }
   } else if (SIMPNODE_operator(k1)==OPR_SELECT) {
      sk1 = SIMPNODE_kid1(k1);
      sk2 = SIMPNODE_kid(k1,2);
      if (SIMP_Is_Constant(sk1) &&
	  SIMP_Is_Constant(sk2)) {
	 kt = SIMPNODE_CopyNode(k0);
	 r = SIMPNODE_SimpCreateExp3(OPC_FROM_OPR(OPR_SELECT,OPCODE_rtype(opc)),
				     SIMPNODE_kid0(k1),
				     SIMPNODE_SimpCreateExp2(opc,k0,sk1),
				     SIMPNODE_SimpCreateExp2(opc,kt,sk2));
	 SHOW_RULE("co op SELECT(x,c1,c2)");
      }
   }
   return (r);
}



static simpnode  SIMPNODE_ConstantFold1(OPCODE opc, simpnode  k0)
{
   simpnode  r = NULL;
   TCON c0,c1;
   BOOL folded;

   if (SIMP_Is_Flt_Constant(k0)) {
      c0 = SIMP_Flt_ConstVal(k0);
   } else if (SIMP_Is_Int_Constant (k0)) /* KEY */{
      c0 = Host_To_Targ(SIMPNODE_rtype(k0),SIMP_Int_ConstVal(k0)); 
   }
#ifdef KEY
   else if (SIMP_Is_Str_Constant (k0))
      c0 = SIMP_Str_ConstVal (k0);
   else
      Fail_FmtAssertion ("Not a float/int/str constant");
#endif

#ifndef WN_SIMP_WORKING_ON_WHIRL
   // The optimizer doesn't really try very hard to keep the type distinctions
   // clear, so we need to make sure we don't remove necessary converts
   if (OPCODE_operator(opc) == OPR_CVTL ||
       OPCODE_operator(opc) == OPR_PARM ||
       (Split_64_Bit_Int_Ops &&
        OPCODE_operator(opc) == OPR_CVT && 
	MTYPE_bit_size(OPCODE_rtype(opc)) == 64 &&
	MTYPE_bit_size(OPCODE_desc(opc)) == 32)) {
      /* Not possible to folds this here */
     return r;
   } else {
      c1 = Host_To_Targ(MTYPE_I8,0);     
   }
#else
   // For WHIRL, we need to get a least a dummy TCON to pass to the folder
   c1 = Host_To_Targ(MTYPE_I8,0);     
#endif

   
   c0 = Targ_WhirlOp(opc,c0,c1,&folded);
   if (folded) {
      SHOW_RULE("constant fold 1");
      if (SIMP_IS_TYPE_INTEGRAL(TCON_ty(c0))) {
	 r = SIMP_INTCONST(TCON_ty(c0),Targ_To_Host(c0));
      } else {
	 r = SIMPNODE_CreateFloatconstFromTcon(c0);
      }
      SIMP_DELETE(k0);
   }
   return (r);
}

static simpnode  SIMPNODE_ConstantFold2(OPCODE opc, simpnode  k0, simpnode  k1)
{
   simpnode  r = NULL;
   TCON c0,c1;
   BOOL folded;

   if (SIMP_Is_Flt_Constant(k0)) {
      c0 = SIMP_Flt_ConstVal(k0);
   } else if (SIMP_Is_Int_Constant (k0)) /* KEY */{
      c0 = Host_To_Targ(SIMPNODE_rtype(k0),SIMP_Int_ConstVal(k0)); 
   }
#ifdef KEY
   else if (SIMP_Is_Str_Constant (k0))
      c0 = SIMP_Str_ConstVal (k0);
   else
      Fail_FmtAssertion ("Not a float/int/str constant");
#endif

   if (SIMP_Is_Flt_Constant(k1)) {
      c1 = SIMP_Flt_ConstVal(k1);
   } else if (SIMP_Is_Int_Constant (k1)) /* KEY */{
      c1 = Host_To_Targ(SIMPNODE_rtype(k1),SIMP_Int_ConstVal(k1)); 
   }
#ifdef KEY
   else if (SIMP_Is_Str_Constant (k1))
      c1 = SIMP_Str_ConstVal (k1);
   else
      Fail_FmtAssertion ("Not a float/int/str constant");
#endif

#ifndef WN_SIMP_WORKING_ON_WHIRL
#ifdef TARG_X8664 // bug 8865: prevents unsigned x + (-1) + (-1) from becoming
   	   //		x + 0xfffffffe for U4ADD and I8INTCONST
   if (TCON_ty(c0) == TCON_ty(c1) && MTYPE_byte_size(TCON_ty(c0)) == 8 &&
       MTYPE_is_integral(TCON_ty(c0)) && OPCODE_desc(opc) == MTYPE_V &&
       OPCODE_operator(opc) != OPR_DIV)
     opc = OPCODE_make_op(OPCODE_operator(opc), TCON_ty(c0), MTYPE_V);
#endif
#ifdef TARG_X8664 // bug 11830: if any operand is 64-bit, result should be
        // 64-bit; and if any operand is signed, result should be signed
   if (MTYPE_is_integral(OPCODE_rtype(opc)) &&
       (OPCODE_operator(opc) == OPR_ADD || OPCODE_operator(opc) == OPR_SUB ||
        OPCODE_operator(opc) == OPR_MPY)) {
     if (MTYPE_byte_size(TCON_ty(c0)) == 8 || MTYPE_byte_size(TCON_ty(c1)) == 8)
       opc = OPCODE_make_op(OPCODE_operator(opc),
                            Mtype_TransferSize(MTYPE_I8, OPCODE_rtype(opc)),
                            MTYPE_V);
     if (MTYPE_signed(TCON_ty(c0)) || MTYPE_signed(TCON_ty(c1)))
       opc = OPCODE_make_op(OPCODE_operator(opc),
                            Mtype_TransferSign(MTYPE_I4, OPCODE_rtype(opc)),
                            MTYPE_V);
   }
#endif
#endif
   c0 = Targ_WhirlOp(opc,c0,c1,&folded);
   if (folded) {
      SHOW_RULE("constant fold 2");
      if (SIMP_IS_TYPE_INTEGRAL(TCON_ty(c0))) {
	 r = SIMP_INTCONST(TCON_ty(c0),Targ_To_Host(c0));
      } else {
	 r = SIMPNODE_CreateFloatconstFromTcon(c0);
      }
      SIMP_DELETE(k0);
      SIMP_DELETE(k1);
   }
   return (r);
}



static simpnode SIMPNODE_SimplifyExp2_h(OPCODE opc, simpnode k0, simpnode k1) 
{
   BOOL  k0const, k1const;
   OPERATOR  op;
   OPCODE    canon_opc;
   simpnode result=NULL;
   
#ifdef TARG_X8664
   if (MTYPE_is_vector(OPCODE_rtype(opc))) return (result);
#endif
   simpnode  (*simp_func)(OPCODE opc, simpnode k0, simpnode k1, 
			  BOOL k0const, BOOL k1const);
   
   if (!SIMPNODE_enable || !OPCODE_is_expression(opc)) return (result);
   if (!SIMPNODE_simp_initialized) SIMPNODE_Simplify_Initialize();
   
   op = OPCODE_operator(opc);
   simp_func = simplify_function_table[op];
   k0const = SIMP_Is_Constant(k0);
   k1const = SIMP_Is_Constant(k1);
  
   if (k0const && k1const) {
      result = SIMPNODE_ConstantFold2(opc, k0, k1);
      return (result);
   }

   /* Don't swap around MINMAX, DIVREM or the coming XMPY */
   /* This fixes PV 442288 and 443473 */
   if (op != OPR_MINMAX && op != OPR_DIVREM) {
      if (k0const || k1const) {
	 result = Fold2_Into_Select(opc, k0, k1);
	 if (result) return (result);
      }
   }

   if (Simp_Canonicalize) {
      /* Simply canonicalization for constants. *******************************
	 c1 op1 x		x op2 c2	     if op1 is one of the following:
	 op1	op2
	 --	---
	 +,*	+,*
	 &,|,^	&,|,^
	 &&,||	&&,||	
	 MAX,MIN	MAX,MIN
	 <	>
	 <=	>=
	 ==	==
	 != 	!=
	 >=	<=
	 >	<
	 *******************************************************************/
      if (k0const &&
	  (canon_opc = OPCODE_commutative_op(opc)) != 0) {
	 /* Switch things around and rebuild the node */
	 SHOW_RULE("commute constant operand");
	 result = SIMPNODE_SimpCreateExp2(canon_opc, k1, k0);
	 return (result);
      }

      /* Canonicalization for non-constants
	 ----------------------------------
	 y op1 x			x op2 y	  with op1 and op2 as above
	 There will be some "shape-like"
	 order (perhaps the formal number?)
	 The operation will be commuted if 
	 y > x in the shape order.
	 */
      if (!k0const && !k1const &&
	  (canon_opc = OPCODE_commutative_op(opc)) &&
	  Enable_Cfold_Aggressive) {
	 if (SIMPNODE_Simp_Compare_Trees(k0,k1) == 1) {
	    SHOW_RULE("commute operand");
	    result = SIMPNODE_SimpCreateExp2(canon_opc, k1, k0);
	    return(result);
	 }
      }

      /********************************** 
	simple reassociations:  
	---------------------
	(j op c1) op c2		j op (c1 op c2)		op is +,*,&,|,^,MAX,MIN
	(a op c1) op c2		a op (c1 op c2)		R:2 op is +,*
	(a op c1) op c2		a op (c1 op c2)		op is MAX, MIN
	(j op c1) op k		(j op k) op c1		op is +,*,&,|,^,MAX,MIN
	(a op c1) op b		(a op b) op c1		R:2 op is +,*
	(a op c1) op b		(a op b) op c1		op is MAX, MIN
	(j op c1) op (k op c2)	(j op k) op (c1 op c2)	op is +,*,&,|,^,MAX,MIN
	(a op c1) op (b op c2)	(a op b) op (c1 op c2)	R:2 op is +,*
	(a op c1) op (b op c2)	(a op b) op (c1 op c2)	op is MAX, MIN
	
	k op (j op c1)		(k op j) op c1		op is +,*,&,|,^,MAX,MIN
	b op (a op c1)		(b op a) op c1		R:2 op is +,*
	b op (a op c1)		(b op a) op c1		op is MAX, MIN
        x op (y op z)           (x op y) op z           for z being the largest
                                                          expr (bug 10644)
	
	**********************************/
   
      if (is_ok_to_reassociate(opc)) {
	 if (k1const && (SIMPNODE_opcode(k0) == opc) &&
	     SIMP_Is_Constant(SIMPNODE_kid1(k0))) {
	    SHOW_RULE("reassociate 1");
	    result = SIMPNODE_SimpCreateExp2(opc, SIMPNODE_kid0(k0),
					     SIMPNODE_SimpCreateExp2(opc, SIMPNODE_kid1(k0), k1));
	    SIMP_DELETE(k0);
	    return (result);
	 }
      
	 if (!k0const &&
	     SIMPNODE_opcode(k0) == opc &&
	     SIMP_Is_Constant(SIMPNODE_kid1(k0))) {
	    SHOW_RULE("reassociate 2a");
	    result = SIMPNODE_SimpCreateExp2(opc,SIMPNODE_SimpCreateExp2(opc, SIMPNODE_kid0(k0), k1), SIMPNODE_kid1(k0));
	    SIMP_DELETE(k0);
	    return (result);
	 }
      
	 if (!k1const &&
	     SIMPNODE_opcode(k1) == opc &&
	     SIMP_Is_Constant(SIMPNODE_kid1(k1))) {
	    SHOW_RULE("reassociate 2b");
	    result = SIMPNODE_SimpCreateExp2(opc,
					     SIMPNODE_SimpCreateExp2(opc, SIMPNODE_kid0(k1), k0),
					     SIMPNODE_kid1(k1));
	    SIMP_DELETE(k1);
	    return (result);
	 }
      
	 if (SIMPNODE_opcode(k0) == opc &&
	     SIMPNODE_opcode(k1) == opc &&
	     SIMP_Is_Constant(SIMPNODE_kid1(k0)) &&
	     SIMP_Is_Constant(SIMPNODE_kid1(k1))) {
	 
	    SHOW_RULE("reassociate 3");
	    result = SIMPNODE_SimpCreateExp2(opc, SIMPNODE_kid0(k0),SIMPNODE_kid0(k1));
	    result = SIMPNODE_SimpCreateExp2(opc, result,
					     SIMPNODE_SimpCreateExp2(opc,SIMPNODE_kid1(k0),SIMPNODE_kid1(k1)));
	    SIMP_DELETE(k0);
	    SIMP_DELETE(k1);
	    return (result);
	 }
      }

#ifdef KEY // bug 10644
      if (Enable_Cfold_Reassociate &&
          SIMPNODE_opcode(k1) == opc && SIMPNODE_operator(k0) == OPR_LDID) {
        simpnode k10 = SIMPNODE_kid0(k1);
        simpnode k11 = SIMPNODE_kid1(k1);
        if (SIMPNODE_operator(k10) == OPR_ILOAD &&
            SIMPNODE_operator(k11) == OPR_ILOAD &&
            SIMPNODE_operator(SIMPNODE_kid0(k10)) == OPR_ARRAY &&
            SIMPNODE_operator(SIMPNODE_kid0(k11)) == OPR_ARRAY) {
          INT kidcnt10 = SIMPNODE_kid_count(SIMPNODE_kid0(k10));
          INT kidcnt11 = SIMPNODE_kid_count(SIMPNODE_kid0(k11));
          if ((kidcnt10+4) <= kidcnt11) {
            SHOW_RULE("reassociate 4a");
            result = SIMPNODE_SimpCreateExp2(opc,
                                 SIMPNODE_SimpCreateExp2(opc, k0, k10), k11);
            SIMP_DELETE(k1);
            return (result);
          }
          else if ((kidcnt11+4) <= kidcnt10) {
            SHOW_RULE("reassociate 4b");
            result = SIMPNODE_SimpCreateExp2(opc,
                                 SIMPNODE_SimpCreateExp2(opc, k0, k11), k10);
            SIMP_DELETE(k1);
            return (result);
          }
        }
      }
#endif
   }  /* Simp_Canonicalize */
#ifndef KEY
   if (simp_func) {
#else
   if (simp_func && !MTYPE_is_vector(OPCODE_rtype(opc))) {
#endif
      result = simp_func(opc, k0, k1, k0const, k1const);
   } else {
      result = NULL;
   }
   
   return (result);
}

/* This just calls the above routine. It's done this way to simplify my debugging */
simpnode SIMPNODE_SimplifyExp2(OPCODE opc, simpnode k0, simpnode k1) 
{
   simpnode  result;
   result = SIMPNODE_SimplifyExp2_h(opc, k0,k1);
   SHOW_TREE(opc,k0,k1,result);
   return (result);
}
   

simpnode SIMPNODE_SimplifyExp1(OPCODE opc, simpnode k0) 
{
   OPERATOR  op;
   simpnode result=NULL;
   simpnode k1,k2;
   
   simpnode  (*simp_func)(OPCODE opc, simpnode k0, simpnode k1, 
		     BOOL k0const, BOOL k1const);

   if (!SIMPNODE_enable || !OPCODE_is_expression(opc)) return (result);
   if (!SIMPNODE_simp_initialized) SIMPNODE_Simplify_Initialize();

   op = OPCODE_operator(opc);

/* For debugging only */
#undef DEBUG_IGNORE_PARENS
#ifdef DEBUG_IGNORE_PARENS
   if (op == OPR_PAREN) return k0;
#endif


   /* Check for and do the simplification 
    * OP (SELECT(X,c1,c2)) ->
    * SELECT(X,OP(c1),OP(C2))
    */
   if (SIMPNODE_operator(k0) == OPR_SELECT && op != OPR_PARM && op != OPR_EXTRACT_BITS) {
      k1 = SIMPNODE_kid1(k0);
      k2 = SIMPNODE_kid(k0,2);
      /* Check for and do the simplification 
       * OP (SELECT(X,c1,c2)) ->
       * SELECT(X,OP(c1),OP(C2))
       */
      if (SIMP_Is_Constant(k1) && SIMP_Is_Constant(k2)) {
	 result = SIMPNODE_SimpCreateExp3(OPC_FROM_OPR(OPR_SELECT,OPCODE_rtype(opc)),
					  SIMPNODE_kid0(k0),
					  SIMPNODE_SimpCreateExp1(opc,k1),
					  SIMPNODE_SimpCreateExp1(opc,k2));
	 SHOW_RULE("OP(SELECT(x,c1,c2))");
      }
   } else {
      simp_func = simplify_function_table[op];
      if (SIMP_Is_Constant(k0)) {
	 result = SIMPNODE_ConstantFold1(opc, k0);
      } else if (simp_func) {
	 result = simp_func(opc, k0, NULL, FALSE, FALSE);
      } else {
	 result = NULL;
      }
   }

   SHOW_TREE(opc,k0,NULL,result);
   return (result);
}


/*------------------------------------------------ 
   Simplifications for CVTL

   CVTL (SELECT(X,c1,c2))           -> SELECT(X,CVTL(C1),CVTL(C2))
   CVTL n1(CVTL n2(k0)), n2 <= n1   -> cvtl n2(k0)
   CVTL n1(CVTL n2(k0)), n2 > n1    -> cvtl n1(k0)
   CVTL n (X & c1)                  -> cvtl n (x) if (1<<n)-1 & c1 = (1<<n)-1
   
   cvtl n (cvtl n (x) op y)         -> cvtl n (x op y), op is ADD,SUB,MPY,BIOR,BAND,BXOR,SHL
   cvtl n (op cvtl n (x) )          -> cvtl n (op x), op is NEG, BNOT
   cvtl n (op cvtl n (x) )          -> op cvtl n (x)), op is ABS
                                       
   

-------------------------------------------------*/


simpnode simp_cvtl(OPCODE opc, INT16 cvtl_bits, simpnode k0)
{
   simpnode r=NULL,k1,k2;
   TYPE_ID source_ty,dest_ty;
   INT16   s_size,d_size,k0_bits;
   INT64  cval,mask;
   OPERATOR opr0;
   
   source_ty = SIMPNODE_rtype(k0);
   dest_ty = OPCODE_rtype(opc);

   /* Constant fold it */
   if (SIMP_Is_Constant(k0)) {
      BOOL folded = FALSE;
      TCON c0,c1;
#ifndef WN_SIMP_WORKING_ON_WHIRL
      if (Split_64_Bit_Int_Ops && 
	  MTYPE_bit_size(dest_ty) == 64 &&
	  MTYPE_bit_size(source_ty) == 32) {
	// do not fold because cannot change result type to I8/U8 in wopt
      }
      else 
#endif
      {
#ifdef KEY
        if (SIMP_Is_Flt_Constant(k0)) {
          c0 = SIMP_Flt_ConstVal(k0);
        } else if (SIMP_Is_Int_Constant (k0)) {
          c0 = Host_To_Targ(SIMPNODE_rtype(k0),SIMP_Int_ConstVal(k0)); 
        } else if (SIMP_Is_Str_Constant (k0))
          c0 = SIMP_Str_ConstVal (k0);
        else Fail_FmtAssertion ("Not a float/int/str constant");
#else
        c0 = Host_To_Targ(source_ty,SIMP_Int_ConstVal(k0));
#endif // KEY
        c1 = Host_To_Targ(MTYPE_I8,cvtl_bits);
        c0 = Targ_WhirlOp(opc,c0,c1,&folded);
      }
      if (folded) {
	 SHOW_RULE("constant fold cvtl");
	 r = SIMP_INTCONST(dest_ty,Targ_To_Host(c0));
	 SIMP_DELETE(k0);
	 return (r);
      }
   } else if (SIMPNODE_operator(k0) == OPR_SELECT) {
      k1 = SIMPNODE_kid1(k0);
      k2 = SIMPNODE_kid(k0,2);
      /* Check for and do the simplification 
       * CVTL (SELECT(X,c1,c2)) ->
       * SELECT(X,CVTL(c1),CVTL(C2))
       */
      if (SIMP_Is_Constant(k1) || SIMP_Is_Constant(k2) ||
	  SIMPNODE_opcode(k1) == opc || SIMPNODE_opcode(k2) == opc) {
	simpnode t_k1 = SIMPNODE_SimplifyCvtl(opc,cvtl_bits,k1);
	simpnode t_k2 = SIMPNODE_SimplifyCvtl(opc,cvtl_bits,k2);
	r = SIMPNODE_SimpCreateExp3(OPC_FROM_OPR(OPR_SELECT,OPCODE_rtype(opc)),
				    SIMPNODE_kid0(k0),
				    (t_k1) ? t_k1 : SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,k1),
				    (t_k2) ? t_k2 : SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,k2));

	 SHOW_RULE("CVTL(SELECT(x,c1,c2))");
      }
   } else if (SIMPNODE_opcode(k0) == opc) {
      /* CVTL (CVTL) */
      k0_bits = SIMPNODE_cvtl_bits(k0);
      if (k0_bits <= cvtl_bits) {
	 SHOW_RULE("CVTL(n1,CVTL(n2,k0)), n2 <= n1");
	 r = k0;
      } else {
	 SHOW_RULE("CVTL(n1,CVTL(n2,k0)), n2 > n1");
	 r = SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,SIMPNODE_kid0(k0));
      }
   } else if (SIMPNODE_operator(k0) == OPR_BAND &&
	      SIMP_Is_Int_Constant(SIMPNODE_kid1(k0))) /* KEY */{
      cval = SIMP_Int_ConstVal(SIMPNODE_kid1(k0));
      mask = (1ll << cvtl_bits) - 1ll;
      if ((mask & cval) == mask) {
	 SHOW_RULE("cvtl(x&c1) -> cvtl(x)");
	 r = SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,SIMPNODE_kid0(k0));
	 SIMP_DELETE(SIMPNODE_kid1(k0));
	 SIMP_DELETE(k0);
      }
   } else {
      
      s_size = SIMP_TYPE_SIZE(source_ty);
      d_size = SIMP_TYPE_SIZE(dest_ty);
      
      if (s_size == d_size && s_size == cvtl_bits) {
	 SHOW_RULE("CVTL(n, k0) -> k0");
	 r = k0;
      }
   }

   if (r) return (r);

   /* A few more CVTL simplifcations (from PV 364316):
      ALso, if we see CVTL(BINOP(CVTL(X),Y)), I believe we can turn this into
      CVTL(BINOP(X,Y)) for BINOPs BAND, BIOR, BXOR, ADD, SUB, MPY and SHL, because
      these operations always push information toward higher bits. We can also do
      CVTL(BNOT(CVTL(X))). DIV, MOD, LSHR and ASHR can't be done this way, because
      they bring in information from the higher bits. 
      */
   
   opr0 = SIMPNODE_operator(k0);
   switch (opr0) {
    case OPR_NEG:
    case OPR_BNOT:
    case OPR_LNOT:
      k1 = SIMPNODE_kid0(k0);
      if (SIMPNODE_opcode(k1) == opc &&
	  SIMPNODE_cvtl_bits(k1) >= cvtl_bits) {
	 SHOW_RULE("CVTL n (op CVTL n (X))");
	 r = SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,
				     SIMPNODE_SimpCreateExp1(SIMPNODE_opcode(k0),
							     SIMPNODE_kid0(k1)));
	 SIMP_DELETE(k1);
	 SIMP_DELETE(k0);
      } else if (opr0 == OPR_LNOT) {
	 /* Since this returns 0 or 1, we don't need to CVTL it */
	 r = k0;
      }
      break;

#if 0 // this rule is wrong because of the number 0x8000 in 16 bits;
      // ABS(I4CVTL16(0x8000)) == 0x8000, and I4CVTL(0x8000) == 0xffff8000
      // i.e. the negation of MININT is itself 
    case OPR_ABS:
       k1 = SIMPNODE_kid0(k0);
       if (SIMPNODE_opcode(k1) == opc &&
	   SIMPNODE_cvtl_bits(k1) == cvtl_bits) {
	  SHOW_RULE("CVTL n (ABS CVTL n (X))");
	  r = k0;
       }
       break;
#endif

    case OPR_LT:
    case OPR_LE:
    case OPR_GT:
    case OPR_GE:
    case OPR_EQ:
    case OPR_NE:
    case OPR_LIOR:
    case OPR_LAND:
      /* Since these return 0 or 1, we don't need to CVTL them at all */
      r = k0;
      break;

    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_BAND:
    case OPR_SHL:
      k1 = SIMPNODE_kid0(k0);
      k2 = SIMPNODE_kid1(k0);
      if (SIMPNODE_opcode(k1) == opc &&
	  SIMPNODE_cvtl_bits(k1) >= cvtl_bits && 
	  SIMPNODE_opcode(k2) == opc &&
	  SIMPNODE_cvtl_bits(k2) >= cvtl_bits) {
	 SHOW_RULE("CVTL n (CVTL n (X) op CVTL(y))");
	 r = SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,
				     SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),
							     SIMPNODE_kid0(k1),
							     SIMPNODE_kid0(k2)));
	 SIMP_DELETE(k2);
	 SIMP_DELETE(k1);
	 SIMP_DELETE(k0);
      } else if (SIMPNODE_opcode(k1) == opc &&
		 SIMPNODE_cvtl_bits(k1) >= cvtl_bits) {
	 SHOW_RULE("CVTL n (CVTL n (X) op y)");
	 r = SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,
				     SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),
							     SIMPNODE_kid0(k1),
							     k2));
	 SIMP_DELETE(k1);
	 SIMP_DELETE(k0);
      } else if (SIMPNODE_opcode(k2) == opc &&
		 SIMPNODE_cvtl_bits(k2) >= cvtl_bits && opr0 != OPR_SHL) {
	 SHOW_RULE("CVTL n (X op CVTL n (y))");
	 r = SIMPNODE_SimpCreateCvtl(opc,cvtl_bits,
				     SIMPNODE_SimpCreateExp2(SIMPNODE_opcode(k0),
							     k1,
							     SIMPNODE_kid0(k2)));
	 SIMP_DELETE(k2);
	 SIMP_DELETE(k0);
      }
      break;
    default:
      break;
   }
   return (r);
}      

simpnode SIMPNODE_SimplifyCvtl(OPCODE opc, INT16 cvtl_bits, simpnode k0) 
{
   simpnode result = NULL;
   if (!SIMPNODE_enable) return (result);
   if (!SIMPNODE_simp_initialized) SIMPNODE_Simplify_Initialize();
   
   result = simp_cvtl(opc,cvtl_bits,k0);

   if (result) {
      SHOW_TREE(opc,k0,(simpnode) (INTPTR) cvtl_bits, result);
   }
   return (result);
}


/*------------------------------------------------ 
   Simplifications for SELECT

   FALSE ? x : y        y
   TRUE  ? x : y        x
   x ? y : y            y
   boolexpr ? 1 : 0     boolexpr
   x ? 1 : 0            NE(x,0)
   x ? 0 : 1            NOT(x)

-------------------------------------------------*/


simpnode SIMPNODE_SimplifyExp3(OPCODE opc, simpnode k0, simpnode k1, 
			       simpnode k2) 
{
   simpnode r = NULL;
   OPCODE   k0op;
   
   if (!SIMPNODE_enable) return (r);
   if (!SIMPNODE_simp_initialized) SIMPNODE_Simplify_Initialize();

   /* Currently, this only simplifies SELECT */
   if (OPCODE_operator(opc) != OPR_SELECT && OPCODE_operator(opc) != OPR_CSELECT) return (r);
   
   k0op = SIMPNODE_opcode(k0);
   if (SIMP_Is_Int_Constant(k0)) {
      if (SIMP_Int_ConstVal(k0) == 0) {
	 SHOW_RULE("SELECT(FALSE,x,y)");
	 r = k2;
	 SIMP_DELETE_TREE(k1);
      } else {
	 SHOW_RULE("SELECT(TRUE,x,y)");
	 r = k1;
	 SIMP_DELETE_TREE(k2);
      }
      SIMP_DELETE_TREE(k0);
   } else if (SIMP_Is_Int_Constant(k1) &&
   	      SIMP_Is_Int_Constant(k2) &&
	      SIMPNODE_operator(k0) != OPR_CAND &&
	      SIMPNODE_operator(k0) != OPR_CIOR) {
      if (SIMP_Int_ConstVal(k1) == 1 && 
	  SIMP_Int_ConstVal(k2) == 0) {
	 if (get_inverse_relop(k0op) != OPCODE_UNKNOWN ||
	     OPCODE_rtype(k0op) == MTYPE_B) {
	    SHOW_RULE("SELECT(boolexpr,1,0)");
	    r = k0;
	    SIMP_DELETE_TREE(k1);
	    SIMP_DELETE_TREE(k2);
	 } else {
	    TYPE_ID k0_rtype = OPCODE_rtype(k0op);
	    SHOW_RULE("SELECT(expr,1,0)");
	    r = SIMPNODE_SimpCreateExp2(OPCODE_make_op(OPR_NE, k0_rtype, k0_rtype),
					k0, SIMP_INTCONST(k0_rtype, 0));
	    SIMP_DELETE_TREE(k1);
	    SIMP_DELETE_TREE(k2);
	 }
      } else if (SIMP_Int_ConstVal(k1) == 0 && 
		 SIMP_Int_ConstVal(k2) == 1) {
	 SHOW_RULE("SELECT(expr,0,1)");
	 r = SIMPNODE_SimpCreateExp1(OPC_FROM_OPR(OPR_LNOT,OPCODE_rtype(k0op)),k0);
	 SIMP_DELETE_TREE(k1);
	 SIMP_DELETE_TREE(k2);
      }
      if (r) {
	const TYPE_ID opc_rtype = OPCODE_rtype(opc);
	if (OPCODE_rtype(k0op) == MTYPE_B && opc_rtype != MTYPE_B)
	  r = SIMPNODE_SimpCreateExp1(
			OPCODE_make_op(OPR_CVT, opc_rtype, MTYPE_B), r);
	return (r);
      }
   }
   if (r == NULL && Enable_Cfold_Aggressive) {
      if (SIMPNODE_Simp_Compare_Trees(k1,k2)==0) {
	 SHOW_RULE("SELECT(x,y,y)");
	 r = k1;
	 SIMP_DELETE_TREE(k0);
	 SIMP_DELETE_TREE(k2);
      }
   }

   return (r);
}

#if defined(KEY) && defined(WN_SIMP_WORKING_ON_WHIRL)
simpnode SIMPNODE_FoldIntrinsic(OPCODE opc, UINT32 intrinsic, INT32 n, simpnode k[])
{
   simpnode r = NULL;
   switch (intrinsic)
   {
#ifdef TARG_X8664
     case INTRN_C8I4EXPEXPR:
     {
       if (!Is_Target_SSE3())
	 break;
       WN *constant = SIMPNODE_kid0(k[1]);
       WN *variable = SIMPNODE_kid0(k[0]);
       if (WN_operator(constant) != OPR_INTCONST ||
	   WN_const_val(constant) != 2)
	 break;
       r = WN_Binary(OPR_MPY, OPCODE_rtype(opc), variable,
		     WN_COPY_Tree(variable));
       break;
     }
#endif // TARG_X8664

     case INTRN_CEQEXPR:
     {
       Is_True (n == 4, ("Invalid number of parameters for intrinsic"));
       WN * kid0, * kid1, * kid2, * kid3;
       kid0 = k[0];
       if (SIMPNODE_operator (kid0) == OPR_PARM)
         kid0 = SIMPNODE_kid0 (kid0);

       kid1 = k[1];
       if (SIMPNODE_operator (kid1) == OPR_PARM)
         kid1 = SIMPNODE_kid0 (kid1);

       kid2 = k[2];
       if (SIMPNODE_operator (kid2) == OPR_PARM)
	 kid2 = SIMPNODE_kid0 (kid2);

       kid3 = k[3];
       if (SIMPNODE_operator (kid3) == OPR_PARM)
	 kid3 = SIMPNODE_kid0 (kid3);

       if (SIMPNODE_operator (kid0) != OPR_ARRAY ||
           SIMPNODE_operator (kid1) != OPR_ARRAY)
	 break;

       if (SIMPNODE_num_dim (kid0) != 1 || SIMPNODE_num_dim(kid1) != 1)
         break;

       // non-constant indexing into array?
       if (SIMPNODE_operator (SIMPNODE_array_index (kid0, 0))
                != OPR_INTCONST ||
	   SIMPNODE_operator (SIMPNODE_array_index (kid1, 0))
	        != OPR_INTCONST)
	 break;
           
       WN * base0 = SIMPNODE_array_base (kid0);
       WN * base1 = SIMPNODE_array_base (kid1);

       // e.g. the base may be another array
       if (!OPERATOR_has_sym (SIMPNODE_operator (base0)) ||
           !OPERATOR_has_sym (SIMPNODE_operator (base1)))
	 break;

       if (SIMPNODE_operator (kid2) != OPR_INTCONST ||
	   SIMPNODE_operator (kid3) != OPR_INTCONST)
	 break;

       if (ST_class (SIMPNODE_st (base0)) != CLASS_CONST ||
           ST_class (SIMPNODE_st (base1)) != CLASS_CONST)
       {
         if (SIMPNODE_const_val (kid2) != 1 || SIMPNODE_const_val (kid3) != 1)
	   break;

	 // comparing two length 1 non-const strings
	 ST * sym = SIMPNODE_st (base0);
	 WN * k0, * k1;
	 WN_OFFSET ofst = SIMPNODE_const_val (SIMPNODE_array_index (kid0, 0)) + 
	                  SIMPNODE_load_offset (base0);
	 if (ST_class (sym) == CLASS_CONST)
	 {
	   TCON t = ST_tcon_val (sym);
	   if (TCON_ty (t) != MTYPE_STR) break;
	   char * s = Index_to_char_array (TCON_str_idx (t));
	   s += ofst;
	   k0 = SIMP_INTCONST  (OPCODE_rtype (opc), (*s));
	 }
	 else
	 {
	   if (TY_kind (Ty_Table [ST_type (sym)]) == KIND_POINTER)
	   {
	     Is_True (WN_operator (base0) == OPR_LDID,
	              ("Unexpected opcode"));
	     k0 = WN_Iload (MTYPE_U1, 
	           SIMPNODE_const_val (SIMPNODE_array_index (kid0, 0)),
		   TY_pointed (ST_type (sym)),
		   base0);
	   }
	   else
	   {
	     k0 = WN_CreateLdid (OPR_LDID, MTYPE_U4, MTYPE_U1, ofst, 
	                       WN_st_idx (base0), MTYPE_TO_TY_array[MTYPE_U1]);
	   }
	 }

	 ofst = WN_const_val (WN_array_index (kid1, 0)) + 
	                  WN_load_offset (base1);

	 sym = SIMPNODE_st (base1);
	 if (ST_class (sym) == CLASS_CONST)
	 {
	   TCON t = ST_tcon_val (sym);
	   if (TCON_ty (t) != MTYPE_STR) break;
	   char * s = Index_to_char_array (TCON_str_idx (t));
	   s += ofst;
	   k1 = SIMP_INTCONST  (OPCODE_rtype (opc), (*s));
	 }
	 else
	 {
	   if (TY_kind (Ty_Table [ST_type (sym)]) == KIND_POINTER)
	   {
	     Is_True (WN_operator (base1) == OPR_LDID,
	              ("Unexpected operand"));
	     k1 = WN_Iload (MTYPE_U1, 
	           SIMPNODE_const_val (SIMPNODE_array_index (kid1, 0)),
		   TY_pointed (ST_type (sym)),
		   base1);
	   }
	   else
	   {
	     k1 = WN_CreateLdid (OPR_LDID, MTYPE_U4, MTYPE_U1, ofst, 
	                       WN_st_idx (base1), MTYPE_TO_TY_array[MTYPE_U1]);
	   }
	 }
	 Is_True (k0 != NULL && k1 != NULL, ("Invalid WN nodes"));
	 r = WN_Relational (OPR_EQ, OPCODE_rtype (opc), k0, k1);
	 break;
       }

       TCON c0 = ST_tcon_val (SIMPNODE_st (base0));
       TCON c1 = ST_tcon_val (SIMPNODE_st (base1));

       if (TCON_ty (c0) != MTYPE_STR || TCON_ty (c1) != MTYPE_STR)
         break;

       char * str0 = Index_to_char_array (TCON_str_idx (c0));
       char * str1 = Index_to_char_array (TCON_str_idx (c1));
       str0 += WN_const_val (WN_array_index (kid0, 0)) + WN_load_offset (base0);
       str1 += WN_const_val (WN_array_index (kid1, 0)) + WN_load_offset (base1);
       int len0 = WN_const_val (kid2);
       int len1 = WN_const_val (kid3);

       int cmp;
       if (len0 == len1)
         cmp = memcmp (str0, str1, len0);
       else
         break; // for the time being
       r = SIMP_INTCONST (OPCODE_rtype (opc), cmp == 0);
       break;
     }
     default:
       break;
   }

   return r;
}
#endif // KEY && WN_SIMP_WORKING_ON_WHIRL

/*----------------------------------------------------------
  Simplifications for intrinsics

  Currently, only constant folding is attempted

------------------------------------------------------------*/
#define MAX_INTRINSIC_ARGS 6
simpnode SIMPNODE_SimplifyIntrinsic(OPCODE opc, UINT32 intrinsic, INT32 n, simpnode k[])
{
   simpnode r = NULL;
   TCON c[MAX_INTRINSIC_ARGS],c0;
   BOOL allconst;
   BOOL folded;
   INT i;
   INT64  ival;
   TYPE_ID  ty;
   simpnode kid;
   
   if (!SIMPNODE_enable) return (r);
   if (!SIMPNODE_simp_initialized) SIMPNODE_Simplify_Initialize();

   if (OPCODE_operator(opc) != OPR_INTRINSIC_OP) return(r);

   /* It's an intrinsic */
   if (n > MAX_INTRINSIC_ARGS) return (r);
   allconst = TRUE;
   for (i=0; (i < n) && allconst; i++) {
      kid = k[i];
      if (SIMPNODE_operator(kid) == OPR_PARM) {
	 kid = SIMPNODE_kid0(kid);
      }
      if (SIMP_Is_Constant(kid)) {
	 if (SIMP_Is_Flt_Constant(kid)) {
	    c[i] = SIMP_Flt_ConstVal(kid);
	 } else if (SIMP_Is_Int_Constant(kid)) /* KEY */{
	    c[i] = Host_To_Targ(SIMPNODE_rtype(kid),SIMP_Int_ConstVal(kid)); 
	 }
#ifdef KEY
         else if (SIMP_Is_Str_Constant(kid))
	    c[i] = SIMP_Str_ConstVal(kid);
#endif // KEY
      } else {
	 allconst = FALSE;
      }
   }
   if (allconst) {
      SHOW_RULE("intrinsic fold");
      c0 = Targ_IntrinsicOp(intrinsic,c,&folded);
      if (folded) {
	 if (SIMP_IS_TYPE_INTEGRAL(TCON_ty(c0))) {
	    ival = Targ_To_Host(c0);
	    ty = TCON_ty(c0);
	    /* May need to promote type to I4/U4 */
	    if (ty == MTYPE_I1 || ty == MTYPE_I2) {
	       ty = MTYPE_I4;
	    } else if (ty == MTYPE_U1 || ty == MTYPE_U2) {
	       ty = MTYPE_U4;
	    }
	    r = SIMP_INTCONST(ty,ival);
	 } else {
	    r = SIMPNODE_CreateFloatconstFromTcon(c0);
	 }

	 if (n >= 2) {
	   SHOW_TREE(opc,k[0],k[1],r);
	 } else {
	   SHOW_TREE(opc,k[0],NULL,r);
	 }

	 for (i=0; i<n; i++) {
	    SIMP_DELETE(k[i]);
	 }
      }
   }
#if defined(KEY) && defined(WN_SIMP_WORKING_ON_WHIRL)
   else if (OPT_Enable_Simp_Fold)
   {
      r = SIMPNODE_FoldIntrinsic (opc, intrinsic, n, k);
   }
#endif // KEY && WN_SIMP_WORKING_ON_WHIRL
   return (r);
}



#ifdef WN_SIMP_WORKING_ON_WHIRL
/*----------------------------------------------------------
  Simplifications for ILOADS

  Currently, only constant folding is attempted

------------------------------------------------------------*/

simpnode SIMPNODE_SimplifyIload(OPCODE opc, WN_OFFSET offset, 
				TY_IDX ty, UINT field_id, TY_IDX load_addr_ty,
				simpnode addr) 
{
   simpnode r = NULL;
   INT64 lda_offset,new_offset;
   

   if (!SIMPNODE_enable || !WN_Simp_Fold_ILOAD) return (r);
   if (!SIMPNODE_simp_initialized) SIMPNODE_Simplify_Initialize();
#ifdef KEY
   /* Look for ILOAD(LDA) of constants in memory */
   if (SIMPNODE_operator(addr) == OPR_LDA &&
       MTYPE_byte_size(OPCODE_desc(opc)) == 1) {
     ST *s = SIMPNODE_st(addr);
     if (ST_class(s) == CLASS_CONST && TCON_ty (ST_tcon_val (s)) == MTYPE_STR) {
	TCON c = ST_tcon_val(s);
	char *p = Index_to_char_array(TCON_str_idx(c));
	p += SIMPNODE_load_offset(addr) + offset;
	return SIMP_INTCONST(OPCODE_rtype(opc), *p);
     }
   }
#endif

   /* Look for ILOAD(LDA) */
   if (SIMPNODE_operator(addr) == OPR_LDA
#ifndef TARG_MIPS // if LDA is function pointer, can fold only for MIPS ABI
       && ST_class(SIMPNODE_st_idx(addr)) != CLASS_FUNC
#endif
       ) {
      lda_offset = SIMPNODE_lda_offset(addr);
      if (is_add_ok(&new_offset,offset,lda_offset,MTYPE_I4)) {
	 SHOW_RULE("ILOAD(LDA)->LDID");
	 r = WN_CreateLdid(OPCODE_operator(opc) == OPR_ILOAD ? OPR_LDID : OPR_LDBITS,
			   OPCODE_rtype(opc),
			   OPCODE_desc(opc),
			   new_offset,
			   SIMPNODE_st_idx(addr),
			   ty,
			   field_id);
	 SIMP_DELETE(addr);
      }
   }
   return (r);
}

simpnode SIMPNODE_SimplifyIstore(OPCODE opc, WN_OFFSET offset, 
				 TY_IDX ty, UINT field_id, simpnode value,
				 simpnode addr)  
{
   simpnode r = NULL;
   INT64 lda_offset,new_offset;
   TY_IDX pointed;


   if (!SIMPNODE_enable || !WN_Simp_Fold_ILOAD) return (r);
   if (!SIMPNODE_simp_initialized) SIMPNODE_Simplify_Initialize();

   /* Look for ISTORE(LDA) */
   if (SIMPNODE_operator(addr) == OPR_LDA) {
      lda_offset = SIMPNODE_lda_offset(addr);
      if (is_add_ok(&new_offset,offset,lda_offset,MTYPE_I4)) {
         SHOW_RULE("ISTORE(LDA)->STID");
         pointed = TY_pointed(ty);
         DevAssert(pointed, ("TY_pointed of ISTORE type is NULL"));
	 r = WN_CreateStid(OPCODE_operator(opc) == OPR_ISTORE ? OPR_STID : OPR_STBITS,
			   OPCODE_rtype(opc),
			   OPCODE_desc(opc),
                           new_offset,
                           SIMPNODE_st_idx(addr),
                           pointed,
                           value,
			   field_id);
	 SIMP_DELETE(addr);
      }
   }
   return (r);
}
#endif

