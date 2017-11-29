/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007 QLogic Corporation.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: const.c
 *
 * Revision history:
 *  12-Jun-91 - Integrated from Josie
 *  30-Apr-93 - Added constant table manipulation from stab.c
 *
 * Description:
 *
 * Routines for manipulating constants.
 *
 * ====================================================================
 * ====================================================================
 */
#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop


#include "defs.h"
#include "symtab.h"
#include "wn.h"
#include "config_targ.h"
#include <ext/hash_map>
#include <math.h>
using __gnu_cxx::hash_map;

namespace {
    // unnamed namespace for function objects used for removing duplicated
    // constant STs
    struct eq_tcon {
	bool operator () (TCON_IDX t1_idx, TCON_IDX t2_idx) const {
	    if (t1_idx == t2_idx)
		return TRUE;
	    const TCON& t1 = Tcon_Table[t1_idx];
	    const TCON& t2 = Tcon_Table[t2_idx];

	    if (TCON_ty (t1) != TCON_ty (t2))
		return FALSE;
	    if (t1.flags != t2.flags)
		return FALSE;

	    switch (TCON_ty (t1)) {

	    case MTYPE_I1:
	    case MTYPE_I2:
	    case MTYPE_I4:
	    case MTYPE_I8:
	    case MTYPE_U1:
	    case MTYPE_U2:
	    case MTYPE_U4:
	    case MTYPE_U8:
		return TCON_i0 (t1) == TCON_i0 (t2);

	    case MTYPE_F4:
		// Need to use integer inequality because need to match exact bit pattern
		return TCON_ival (t1) == TCON_ival (t2);

	    case MTYPE_F8:
		// Need to use integer inequality because need to match exact bit pattern
		return TCON_k0 (t1) == TCON_k0 (t2);
	    
	    case MTYPE_STR:
		return (TCON_str_idx (t1) == TCON_str_idx (t2) &&
			TCON_str_len (t1) == TCON_str_len (t2));

	    default:
		return memcmp (&t1, &t2, sizeof(TCON)) == 0;
	    }
	}
    };

    struct tcon_hash {
	size_t operator() (TCON_IDX tcon_idx) const {
	    const TCON& tcon = Tcon_Table[tcon_idx];
	    size_t val = TCON_ty (tcon);
	    val ^= TCON_ival (tcon);
	    return val;
	}
    };
} // unnamed namespace
		

typedef hash_map<TCON_IDX, ST *, tcon_hash, eq_tcon> TCON_MERGE;

ST *
New_Const_Sym (TCON_IDX tcon, TY_IDX ty)
{
    static TCON_MERGE merge;

    TCON_MERGE::iterator iter = merge.find (tcon);

    ST* st;
    if (iter == merge.end ()) {
	// create new constant
	st = New_ST (GLOBAL_SYMTAB);

	ST_Init (st, 0, CLASS_CONST, SCLASS_FSTATIC, EXPORT_LOCAL, ty);
	Set_ST_tcon (st, tcon);
	Set_ST_is_initialized (st);
	merge[tcon] = st;
    } else {
	st = (*iter).second;
	Is_True (ST_class (st) == CLASS_CONST &&
		 ST_sclass (st) == SCLASS_FSTATIC &&
		 ST_export (st) == EXPORT_LOCAL &&
		 ST_is_initialized (st), ("Mismatched const ST"));
    }
    return st;
	
} // New_Const_Sym


ST *
Gen_String_Sym (TCON *val, TY_IDX ty, BOOL)
{
    return New_Const_Sym (Enter_tcon (*val), ty);
}


/* ====================================================================
 *
 * Const_Val
 *
 * Return the value of a constant node.
 *
 * WARNING:  The node passed must be know to be one of those with a
 * ND_dec field pointing to a CLASS_CONST symbol.
 *
 * ====================================================================
 */

TCON
Const_Val ( WN *n )
{
    return WN_val (n);
}


/* ====================================================================
 *
 * Make_Const
 *
 * Make a constant node of the given value.
 *
 * ====================================================================
 */

WN *
Make_Const ( TCON c )
{
   WN *n;
   OPCODE opc;
   ST *csym;
  

#ifdef Is_True_On
  Check_TCON(&c);
#endif /* Is_True_On */

  switch(TCON_ty(c)) {
    case MTYPE_F4:
      opc = OPC_F4CONST;
      break;
    case MTYPE_F8:
      opc = OPC_F8CONST;
      break;
    case MTYPE_F10:
      opc = OPC_F10CONST;
      break;
    case MTYPE_FQ:
      opc = OPC_FQCONST;
      break;
    case MTYPE_C4:
      opc = OPC_C4CONST;
      break;
    case MTYPE_C8:
      opc = OPC_C8CONST;
      break;
    case MTYPE_C10:
      opc = OPC_C10CONST;
      break;
    case MTYPE_CQ:
      opc = OPC_CQCONST;
      break;
#ifdef TARG_X8664
    case MTYPE_V8I1:
      opc = OPC_V8I1CONST;
      break;
    case MTYPE_V8I2:
      opc = OPC_V8I2CONST;
      break;
    case MTYPE_V8I4:
      opc = OPC_V8I4CONST;
      break;
    case MTYPE_V8I8:
      opc = OPC_V8I8CONST;
      break;
    case MTYPE_V16I1:
      opc = OPC_V16I1CONST;
      break;
    case MTYPE_V16I4:
      opc = OPC_V16I4CONST;
      break;
    case MTYPE_V16I8:
      opc = OPC_V16I8CONST;
      break;
    case MTYPE_V16F4:
      opc = OPC_V16F4CONST;
      break;
    case MTYPE_V16F8:
      opc = OPC_V16F8CONST;
      break;
    case MTYPE_V16C8:
      opc = OPC_V16C8CONST;
      break;
    case MTYPE_M8I1:
      opc = OPC_M8I1CONST;
      break;
    case MTYPE_M8I2:
      opc = OPC_M8I2CONST;
      break;
    case MTYPE_M8I4:
      opc = OPC_M8I4CONST;
      break;
#endif
    default:
      Is_True ( FALSE, ( "Make_Const can not handle %s",
			 Mtype_Name(TCON_ty(c)) ) );
      return NULL;
  }
   
   csym = New_Const_Sym (Enter_tcon (c), Be_Type_Tbl(TCON_ty(c)));
   n = WN_CreateConst(opc, csym );
   return (n);
   
}

#ifndef MONGOOSE_BE
/* ====================================================================
 *
 * Make_Zerocon
 *
 * Make a zero node of the given type.
 *
 * ====================================================================
 */

WN *
Make_Zerocon ( TYPE_ID ty )
{
  return Make_Const ( Targ_Conv ( ty, Host_To_Targ ( MTYPE_I4, 0 ) ));
}


/* ====================================================================
 *
 * Make_Comparison_Result_Const
 *
 * Make a TRUE or FALSE node of the default comparison result type.
 *
 * ====================================================================
 */

WN *
Make_Comparison_Result_Const ( INT16 val )
{
  return Make_Const ( Host_To_Targ ( (TYPE_ID)Comparison_Result_Mtype, val ) );
}


/* ====================================================================
 *
 * Make_Integer_Const
 *
 * Make an integer constant node of the given type and value.
 *
 * ====================================================================
 */

WN *
Make_Integer_Const ( INT16 mtype, TARG_INT val )
{
   /* Actually, in WHIRL there are no integer constants of the symbol table type */
   /*
    *  return Make_Const ( Host_To_Targ ( mtype, val ) );
    */

   /* NOTE: TARG_INT should be INT64 for this to work! */
   return (WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST,(TYPE_ID)mtype,MTYPE_V), val));

}

#endif /* MONGOOSE_BE */

/******************************************************************************
 This routine makes a WHIRL node representing the reduction
 identity for a given WHIRL op. For example, the reduction identity 
 for + is 0, for * is 1.  The only tricky part is getting the value
 and constant types correct for each mtype (particularly partial-word
 integers.
*******************************************************************************/

WN * Make_Reduction_Identity ( INT32 opr, TYPE_ID mtype )
{
   WN * r;
   TYPE_ID ntype;

   if ((mtype == MTYPE_I1) || (mtype == MTYPE_I2))
     ntype = MTYPE_I4;
   else if ((mtype == MTYPE_U1) || (mtype == MTYPE_U2))
     ntype = MTYPE_U4;
   else
     ntype = mtype;

   switch (opr) {

    case OPR_ADD:
    case OPR_SUB:
      switch (mtype) {
       case MTYPE_I1:
       case MTYPE_I2:
       case MTYPE_I4:
       case MTYPE_I8:
       case MTYPE_U1:
       case MTYPE_U2:
       case MTYPE_U4:
       case MTYPE_U8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0 );
	 break;

       case MTYPE_F4:
       case MTYPE_F8:
       case MTYPE_F10:
       case MTYPE_FQ:
       case MTYPE_C4:
       case MTYPE_C8:
       case MTYPE_C10:
       case MTYPE_CQ:
	 r = Make_Const ( Host_To_Targ_Float ( ntype, 0.0 ) );
	 break;

       default:
	 Fail_FmtAssertion ( "No reduction identity for operator %d, type %d",
			    opr, mtype);
      }
      break;

    case OPR_MPY:
    case OPR_DIV:
      switch (mtype) {
       case MTYPE_I1:
       case MTYPE_I2:
       case MTYPE_I4:
       case MTYPE_I8:
       case MTYPE_U1:
       case MTYPE_U2:
       case MTYPE_U4:
       case MTYPE_U8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 1 );
	 break;

       case MTYPE_F4:
       case MTYPE_F8:
       case MTYPE_F10:
       case MTYPE_FQ:
       case MTYPE_C4:
       case MTYPE_C8:
       case MTYPE_C10:
       case MTYPE_CQ:
	 r = Make_Const ( Host_To_Targ_Float ( ntype, 1.0 ) );
	 break;

       default:
	 Fail_FmtAssertion ( "No reduction identity for operator %d, type %d",
			    opr, mtype);
      }
      break;

    case OPR_MAX:
      switch (mtype) {
       case MTYPE_I1:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), -128LL );
	 break;

       case MTYPE_I2:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), -32768LL );
	 break;

       case MTYPE_I4:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), -2147483648LL);
	 break;

       case MTYPE_I8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V),
				0x8000000000000000LL );
	 break;

       case MTYPE_U1:
       case MTYPE_U2:
       case MTYPE_U4:
       case MTYPE_U8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0 );
	 break;

       case MTYPE_F4:
       case MTYPE_F8:
       case MTYPE_F10:
       case MTYPE_FQ:
	 r = Make_Const ( Host_To_Targ_Float ( ntype, -HUGE_VAL ) );
	 break;

       default:
	 Fail_FmtAssertion ( "No reduction identity for operator %d, type %d",
			    opr, mtype);
      }
      break;

    case OPR_MIN:
      switch (mtype) {
       case MTYPE_I1:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0x7f );
	 break;

       case MTYPE_I2:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0x7fff );
	 break;

       case MTYPE_I4:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0x7fffffff );
	 break;

       case MTYPE_I8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V),
				0x7fffffffffffffffLL );
	 break;

       case MTYPE_U1:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0xff );
	 break;

       case MTYPE_U2:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0xffff );
	 break;

       case MTYPE_U4:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0xffffffff );
	 break;

       case MTYPE_U8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V),
				0xffffffffffffffffLL );
	 break;

       case MTYPE_F4:
       case MTYPE_F8:
       case MTYPE_F10:
       case MTYPE_FQ:
	 r = Make_Const ( Host_To_Targ_Float ( ntype, HUGE_VAL ) );
	 break;

       default:
	 Fail_FmtAssertion ( "No reduction identity for operator %d, type %d",
			    opr, mtype);
      }
      break;

    case OPR_BIOR:
    case OPR_BXOR:
      switch (mtype) {
       case MTYPE_I1:
       case MTYPE_I2:
       case MTYPE_I4:
       case MTYPE_I8:
       case MTYPE_U1:
       case MTYPE_U2:
       case MTYPE_U4:
       case MTYPE_U8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0 );
	 break;

       default:
	 Fail_FmtAssertion ( "No reduction identity for operator %d, type %d",
			    opr, mtype);
      }
      break;

    case OPR_BAND:
      switch (mtype) {
       case MTYPE_I1:
       case MTYPE_U1:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0xff );
	 break;

       case MTYPE_I2:
       case MTYPE_U2:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0xffff );
	 break;

       case MTYPE_I4:
       case MTYPE_U4:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V), 0xffffffff );
	 break;

       case MTYPE_I8:
       case MTYPE_U8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V),
				0xffffffffffffffffLL );
	 break;

       default:
	 Fail_FmtAssertion ( "No reduction identity for operator %d, type %d",
			    opr, mtype);
      }
      break;

    case OPR_LIOR:
      r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype, MTYPE_V ),
			     0 );
      break;
	
    case OPR_LAND:
      r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype, MTYPE_V ),
			     1 );
      break;
	
    case OPR_EQ:    /* Fortran .EQV. operator on type LOGICAL */
    case OPR_NE:    /* Fortran .NEQV. operator on type LOGICAL */
    case OPR_CAND:  /* C/C++ short-circuit "&&" on integral or floating-
                       point type */
    case OPR_CIOR:  /* C/C++ short-circuit "||" on int/float */
      switch (mtype) {
       case MTYPE_I1:
       case MTYPE_I2:
       case MTYPE_I4:
       case MTYPE_I8:
       case MTYPE_U1:
       case MTYPE_U2:
       case MTYPE_U4:
       case MTYPE_U8:
	 r = WN_CreateIntconst ( OPCODE_make_op ( OPR_INTCONST, ntype,
						 MTYPE_V),
                                 (opr == OPR_EQ || opr == OPR_CAND) ? 1
                                                                    : 0 );
	 break;

       case MTYPE_F4:
       case MTYPE_F8:
       case MTYPE_F10:
       case MTYPE_FQ:
        Is_True(opr == OPR_CAND || opr == OPR_CIOR,
	        ("bad opr %d", (INT) opr));
        r = Make_Const(Host_To_Targ_Float(ntype,
                                          (opr == OPR_CAND) ? 1.0 : 0.0));
        break;

      default:
	 Fail_FmtAssertion ( "No reduction identity for operator %d, type %d",
			    opr, mtype);
      }
      break;

    default:
      Fail_FmtAssertion ( "No reduction identity for operator %d, type %d", opr,
			 mtype);
      break;
   }
   return (r);
}
