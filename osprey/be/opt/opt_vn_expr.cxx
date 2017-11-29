//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vn_expr.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vn_expr.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description: 
//
//    This implements the VN_EXPR taxonomy by means of subclassing,
//    where the algorithms to simplify VN_EXPR nodes are implemented
//    in a separate source file (opt_vn_simplify.cxx) where such
//    algorithms turn out to be non-trivial.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include <stdint.h>
#include "defs.h"
#include "erglob.h"
#include "opcode.h"
#include "errors.h"
#include "mtypes.h"
#include "cxx_memory.h"
#include "wn_util.h"
#include "targ_const.h"			// for TCON-related stuff
#include "targ_const_private.h"		// for TCON-related stuff
#include "const.h"			// for symbol/TCON-related

#include "opt_config.h"
#include "opt_wn.h"
#include "opt_util.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_vn_expr_taxonomy.h"
#include "opt_vn.h"

#ifdef __STL_USE_NAMESPACES
using std::pair;
#endif

typedef pair<VN_VALNUM,VN_VALNUM> VN_VALNUM_PAIR;


void 
VN_VALNUM::print(FILE *fp) const
{
   char buf[16]; // Always large enough to hold the string.
   this->sprint(buf);
   fputs(buf, fp);
} // VN_VALNUM::print


INT32 
VN_VALNUM::sprint(char *buf) const
{
   INT32 idx = 0;

   if (is_bottom())
      idx += sprintf(buf, "_|_");
   else if (is_top())
      idx += sprintf(buf, "T");
   else
      idx += sprintf(buf, "vn%u", _num);
   return idx;
} // VN_VALNUM::sprint


//--------------------- General utilities -------------------
//-----------------------------------------------------------

inline void 
Unimplemented(const char *method)
{
   FmtAssert(FALSE, ("Illegal call to method %s", method));
}


inline VN_EXPR *
Create_Folded_Literal(OPCODE             opc, 
		      VN_EXPR::CONST_PTR opnd1, 
		      VN_EXPR::CONST_PTR opnd2)
{
   if ((static_cast<const VN_LITERAL_EXPR*>(opnd1))->is_const_vect () ||
       (static_cast<const VN_LITERAL_EXPR*>(opnd2))->is_const_vect ()) {
     FmtAssert (FALSE, ("not yet able to fold constant vector"));
   }

   // Returns NULL if we cannot fold the given TCONs.
   //
   BOOL folded = FALSE;
   TCON tcon1 = opnd1->get_tcon();
   TCON tcon2 = opnd2->get_tcon();
   TCON tcon = Targ_WhirlOp(opc, tcon1, tcon2, &folded);
   if (folded) {
     // Create new literal VN_EXPR;
      TYPE_ID vect_ty = (static_cast<const VN_LITERAL_EXPR*>(opnd1))->
                            get_vect_type ();
      return VN_EXPR::Create_Literal(tcon, vect_ty);
   } else
      return NULL;
}


inline VN_EXPR *
Create_Unary_Opr(OPERATOR opr, const VN_VALNUM &vn, MTYPE mtype)
{
   return VN_EXPR::Create_Unary(OPCODE_make_op(opr, mtype, MTYPE_V), vn);
}


inline VN_EXPR *
Create_Binary_Opr(OPERATOR         opr, 
		  const VN_VALNUM &vn1,
		  const VN_VALNUM &vn2,
		  MTYPE            rtype,
		  MTYPE            dtype)
{
   return VN_EXPR::Create_Binary(OPCODE_make_op(opr, rtype, dtype), vn1, vn2);
}


static MTYPE
Unify_Rty(OPERATOR opr, MTYPE rty)
{
   // For cases when we can treat two opcodes as identical (this is ISA
   // dependent), we here choose one over the other.  This can be
   // applied in a simplification algorithm to uniformly change
   // opcodes to one out of two or more equivalent forms.
   //
   // For now, this looks ISA independent, since all architectures we
   // intend to compile for obey the equivalences currently embedded 
   // in this function.
   // 
   MTYPE new_rty = rty;

   if (MTYPE_is_integral(rty))
   {
      UINT32 bitsize = MTYPE_bit_size(rty);
   
      switch (opr)
      {
      case OPR_ADD:
      case OPR_SUB:
      case OPR_MPY:
      case OPR_DIV:
      case OPR_REM:
      case OPR_BAND:
      case OPR_BIOR:
      case OPR_BNOR:
      case OPR_BXOR:
      case OPR_SHL:
      case OPR_ASHR:
      case OPR_LSHR:

      case OPR_NEG:
      case OPR_BNOT:
      case OPR_SELECT:
	 if (bitsize == 32U)
	    new_rty = MTYPE_I4;
	 else if (bitsize == 64U)
	    new_rty = MTYPE_I8;
	 break;

      default:
	 break;
      }
   }

   return new_rty;
} // Unify_Rty


inline BOOL
Is_Literal_Expr(const VN_EXPR *expr)
{
   return (expr != NULL && expr->get_kind() == VN_EXPR::LITERAL);
}


inline BOOL
Is_Singular_Expr(const VN_EXPR *expr)
{
   return (expr != NULL                       && 
	   expr->get_kind() == VN_EXPR::UNARY &&
	   expr->get_opc() == OPC_VPARM);
}


inline BOOL
Is_Rty_Opr(VN_EXPR::CONST_PTR expr,
	   VN_EXPR::KIND      kind,
	   OPERATOR           opr1,
	   MTYPE              rty)
{
   if (expr != NULL && expr->get_kind() == kind)
   {
      const OPCODE   opc = expr->get_opc();
      const OPERATOR opr = OPCODE_operator(opc);
      const MTYPE    opc_rty = Unify_Rty(opr, OPCODE_rtype(opc));
      return (opr == opr1 && opc_rty == rty);
   }
   else
      return FALSE;
}


inline BOOL
Is_Rty_Opr2(VN_EXPR::CONST_PTR expr,
	    VN_EXPR::KIND      kind,
	    OPERATOR           opr1,
	    OPERATOR           opr2,
	    MTYPE              rty)
{
   if (expr != NULL && expr->get_kind() == kind)
   {
      const OPCODE   opc = expr->get_opc();
      const OPERATOR opr = OPCODE_operator(opc);
      const MTYPE    opc_rty = Unify_Rty(opr, OPCODE_rtype(opc));
      return ((opr == opr1 || opr == opr2) && opc_rty == rty);
   }
   else
      return FALSE;
}


static BOOL
All_Bits_Set(INT64 i, INT32 num_bytes)
{
   switch (num_bytes)
   {
   case 1:
      return (mUINT8)i == 0xffU;
   case 2:
      return (mUINT16)i == 0xffffU;
   case 4:
      return (mUINT32)i == 0xffffffffU;
   case 8:
      return (mUINT64)i == 0xffffffffffffffffULL;
   default:
      return FALSE;
   }
} // All_Bits_Set


static VN_EXPR::PTR
Create_Scalar_Literal_From_Int(MTYPE mty, INT64 i)
{
   VN_EXPR::PTR p;

   switch (mty)
   {
   case MTYPE_B:
   case MTYPE_I1:
   case MTYPE_I2:
   case MTYPE_I4:
   case MTYPE_I8:
   case MTYPE_U1:
   case MTYPE_U2:
   case MTYPE_U4:
   case MTYPE_U8:
      p = VN_EXPR::Create_Literal(Host_To_Targ(mty, i), MTYPE_UNKNOWN);
      break;
	 
   case MTYPE_F4:
   case MTYPE_F8:
   case MTYPE_F10:
   case MTYPE_FQ:
      p = VN_EXPR::Create_Literal(Host_To_Targ_Float(mty, i), MTYPE_UNKNOWN);
      break;
      
   default:
      FmtAssert(FALSE, ("Unexpected mtype in Create_Literal_From_Int()"));
      break;
   }
   return p;
} // Create_Scalar_Literal_From_Int


//----------------- Implementation of VN_EXPR  -----------------
//--------------------------------------------------------------


// Default mempool for VN_EXPR and associated free-lists.
//
MEM_POOL *VN_EXPR::_Mpool = NULL;

// Initialize all the free lists we use.
//
FREE_STACK *VN_LITERAL_EXPR::_Free = NULL;
FREE_STACK *VN_UNARY_EXPR::_Free = NULL;
FREE_STACK *VN_BINARY_EXPR::_Free = NULL;
FREE_STACK *VN_TERNARY_EXPR::_Free = NULL;
FREE_STACK *VN_INTR_OP_EXPR::_Free = NULL;
FREE_STACK *VN_PHI_EXPR::_Free = NULL;
FREE_STACK *VN_LDA_ADDR_EXPR::_Free = NULL;
FREE_STACK *VN_ARRAY_ADDR_EXPR::_Free = NULL;
FREE_STACK *VN_MEMLOC_EXPR::_Free = NULL;
#ifdef KEY
FREE_STACK *VN_CALL_OP_EXPR::_Free = NULL;
#endif


void 
VN_EXPR::Init_Free_Lists(MEM_POOL *mpool)
{
   _Mpool = mpool;
   VN_LITERAL_EXPR::Init_Free_List();
   VN_UNARY_EXPR::Init_Free_List();
   VN_BINARY_EXPR::Init_Free_List();
   VN_TERNARY_EXPR::Init_Free_List();
   VN_INTR_OP_EXPR::Init_Free_List();
   VN_PHI_EXPR::Init_Free_List();
   VN_LDA_ADDR_EXPR::Init_Free_List();
   VN_ARRAY_ADDR_EXPR::Init_Free_List();
   VN_MEMLOC_EXPR::Init_Free_List();
#ifdef KEY
   VN_CALL_OP_EXPR::Init_Free_List();
#endif
} // VN_EXPR::Init_Free_Lists


void 
VN_EXPR::Reclaim_Free_Lists()
{
   if (_Mpool != NULL)
   {
      VN_LITERAL_EXPR::Reclaim_Free_List();
      VN_UNARY_EXPR::Reclaim_Free_List();
      VN_BINARY_EXPR::Reclaim_Free_List();
      VN_TERNARY_EXPR::Reclaim_Free_List();
      VN_INTR_OP_EXPR::Reclaim_Free_List();
      VN_PHI_EXPR::Reclaim_Free_List();
      VN_LDA_ADDR_EXPR::Reclaim_Free_List();
      VN_ARRAY_ADDR_EXPR::Reclaim_Free_List();
      VN_MEMLOC_EXPR::Reclaim_Free_List();
#ifdef KEY
      VN_CALL_OP_EXPR::Reclaim_Free_List();
#endif
   }
} // VN_EXPR::Reclaim_Free_Lists


VN_EXPR::PTR 
VN_EXPR::Create_Literal(const TCON &tcon, TYPE_ID vect_ty)
{
   return VN_LITERAL_EXPR::Create(tcon, vect_ty);
}


VN_EXPR::PTR 
VN_EXPR::Create_Unary(OPCODE           opc,
		      const VN_VALNUM &vn1)
{
   return VN_UNARY_EXPR::Create(opc, vn1);
}


VN_EXPR::PTR 
VN_EXPR::Create_Binary(OPCODE           opc,
		       const VN_VALNUM &vn1,
		       const VN_VALNUM &vn2)
{
   return VN_BINARY_EXPR::Create(opc, vn1, vn2);
}


VN_EXPR::PTR 
VN_EXPR::Create_Ternary(OPCODE           opc, 
			const VN_VALNUM &vn1,
			const VN_VALNUM &vn2,
			const VN_VALNUM &vn3)
{
   return VN_TERNARY_EXPR::Create(opc, vn1, vn2, vn3);
}


VN_EXPR::PTR 
VN_EXPR::Create_Intr_Op(INTRINSIC intr_opc, UINT32 num_opnds)
{
   return VN_INTR_OP_EXPR::Create(intr_opc, num_opnds);
}


VN_EXPR::PTR 
VN_EXPR::Create_Phi(UINT32  num_opnds,
		    IDTYPE  block_id,
		    PHI_TAG phi_tag)
{
   return VN_PHI_EXPR::Create(num_opnds, block_id, phi_tag);
}


VN_EXPR::PTR 
VN_EXPR::Create_Lda_Addr(INT32 lda_cr_id)
{
   return VN_LDA_ADDR_EXPR::Create(lda_cr_id);
}


VN_EXPR::PTR 
VN_EXPR::Create_Array_Addr(WN_ESIZE esize, INT32 num_dims)
{
   return VN_ARRAY_ADDR_EXPR::Create(esize, num_dims);
}


VN_EXPR::PTR
VN_EXPR::Create_Memloc(MTYPE            dsctype,
		       const VN_VALNUM &bytesize,
		       const VN_VALNUM &offset,
		       const VN_VALNUM &base_addr,
		       const VN_VALNUM &vsym_valnum)
{
   return VN_MEMLOC_EXPR::Create(dsctype, 
				 bytesize, offset,base_addr, vsym_valnum);
}


#ifdef KEY
VN_EXPR::PTR 
VN_EXPR::Create_Call_Op(ST_IDX aux_id, UINT32 num_opnds)
{
   return VN_CALL_OP_EXPR::Create(aux_id, num_opnds);
}
#endif   

VN_VALNUM 
VN_EXPR::get_opnd(UINT) const 
{
   Unimplemented("VN_EXPR::get_opnd");
   return VN_VALNUM::Bottom();
}

OPCODE 
VN_EXPR::get_opc() const 
{
   Unimplemented("VN_EXPR::get_opc");
   return OPCODE_UNKNOWN;
}

INTRINSIC 
VN_EXPR::get_intr_opc() const 
{
   Unimplemented("VN_EXPR::get_intr_opc");
   return INTRINSIC_INVALID;
}

VN_EXPR::PHI_TAG 
VN_EXPR::get_phi_tag() const 
{
   Unimplemented("VN_EXPR::get_phi_tag");
   return PHI_TAG_UNKNOWN;
}

IDTYPE
VN_EXPR::get_block_id() const 
{
   Unimplemented("VN_EXPR::get_block_id");
   return (IDTYPE) 0;
}
   
const TCON &
VN_EXPR::get_tcon() const 
{
   Unimplemented("VN_EXPR::get_tcon");
   return Zero_I4_Tcon;
}
   
void 
VN_EXPR::set_opnd(UINT32, VN_VALNUM)
{
   Unimplemented("VN_EXPR::set_opnd");
}
   
void 
VN_EXPR::set_opnd_vsym(UINT32, VN_VALNUM)
{
   Unimplemented("VN_EXPR::set_opnd");
}
   
INT32
VN_EXPR::get_lda_cr_id() const
{
   Unimplemented("VN_EXPR::get_aux_symbol_id");
   return -1;
}

INT32
VN_EXPR::get_num_dims() const
{
   Unimplemented("VN_EXPR::get_num_dims");
   return -1;
}

WN_ESIZE
VN_EXPR::get_esize() const
{
   Unimplemented("VN_EXPR::get_esize");
   return -1;
}

MTYPE
VN_EXPR::get_dsctype() const
{
   Unimplemented("VN_EXPR::get_dsctype");
   return MTYPE_UNKNOWN;
}

VN_VALNUM
VN_EXPR::get_bytesize() const
{
   Unimplemented("VN_EXPR::get_bytesize");
   return VN_VALNUM::Bottom();
}

VN_VALNUM
VN_EXPR::get_offset() const
{
   Unimplemented("VN_EXPR::get_offset");
   return VN_VALNUM::Bottom();
}

VN_VALNUM
VN_EXPR::get_base_addr() const
{
   Unimplemented("VN_EXPR::get_base_addr");
   return VN_VALNUM::Bottom();
}

VN_VALNUM
VN_EXPR::get_vsym(UINT) const
{
   Unimplemented("VN_EXPR::get_vsym");
   return VN_VALNUM::Bottom();
}

#ifdef KEY
ST_IDX
VN_EXPR::get_aux_id() const
{
   Unimplemented("VN_EXPR::get_aux_id");
   return 0;
}
#endif

//------------- Implementation of VN_LITERAL_EXPR  -------------
//--------------------------------------------------------------


BOOL
VN_LITERAL_EXPR::is_equal_to(CONST_PTR expr) const
{
   // Assumes unused bytes (padding) are zeroed!
   //
   BOOL truth = FALSE;

   if (Is_Literal_Expr(expr))
   {
      const MTYPE this_mty = TCON_ty(_tcon);
      TCON        other_tcon = expr->get_tcon();
      const MTYPE other_mty = TCON_ty(other_tcon);

#ifdef TARG_X8664
      // FIXME: this is statement is a nop as TCON_ty() won't return vector ty.
      if (MTYPE_is_vector(this_mty) || 	MTYPE_is_vector(other_mty))
	return FALSE;
#endif
      
      if (_vect_ty != (static_cast<const VN_LITERAL_EXPR*>(expr))->_vect_ty)
        return FALSE;

      if (MTYPE_is_integral(this_mty) && MTYPE_is_integral(other_mty))
      {
	 // We only care about the value and the signedness of integral types,
	 // not size, since we always represent these as I8 or U8 in the
	 // hash-table.  Furthermore, we only care about the signedness when
	 // one of the values is negative in INT64 representation (i.e. it
	 // may really be negative or it may be a large unsigned quantity).
	 //
	 const INT64 int1 = Targ_To_Host(_tcon);
	 const INT64 int2 = Targ_To_Host(other_tcon);
	 truth = (int1 == int2 &&
		  ((int1 > 0LL && int2 > 0LL) ||
		   MTYPE_is_signed(this_mty) == MTYPE_is_signed(other_mty)));
      }
      else if (this_mty == other_mty)
      { 
	 if (this_mty == MTYPE_STRING)
	    truth = 
	       (Targ_String_Length(_tcon) == Targ_String_Length(other_tcon) &&
		strncmp(Targ_String_Address(_tcon),
			Targ_String_Address(other_tcon),
			Targ_String_Length(_tcon))); 
#ifdef KEY
	 // fix bug 2691: when both are floating-point zero, use integer
	 // comparison so +0 and -0 will be regarded as different for value
	 // numbering purpose
	 else if (Targ_Is_Zero(other_tcon))
            truth = Targ_Is_Zero(_tcon);
	 else if (Targ_Is_Neg_Zero(other_tcon))
            truth = Targ_Is_Neg_Zero(_tcon);
#endif
         else {
	    BOOL folded;
	    other_tcon = 
	       Targ_WhirlOp(OPCODE_make_op(OPR_EQ, 
			      WHIRL_Mtype_B_On ? MTYPE_B : MTYPE_I4, this_mty), 
			    other_tcon, _tcon, &folded);
	    truth = folded && Targ_To_Host(other_tcon) != 0;
	 }
      }
   }
   return truth;
} // VN_LITERAL_EXPR::is_equal_to


//-------------- Implementation of VN_UNARY_EXPR  --------------
//--------------------------------------------------------------

VN_UNARY_EXPR *
VN_UNARY_EXPR::Create(OPCODE opc, const VN_VALNUM &vn) 
{
   Is_True(OPCODE_nkids(opc) == 1,
	   ("Unexpected opcode in call to VN_UNARY_EXPR::Create()"));

   VN_UNARY_EXPR *expr = (VN_UNARY_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_UNARY_EXPR(opc, vn), _Mpool);
   else
   {
      expr->_opc = opc;
      expr->_vn = vn;
   }
   return expr;
} // VN_UNARY_EXPR::Create


VN_EXPR::PTR 
VN_UNARY_EXPR::simplify(VN *v)
{
   PTR            simplified = this;
   PTR            opnd = v->valnum_expr(_vn);
   const OPERATOR opr = OPCODE_operator(_opc);
   const MTYPE    rty = Unify_Rty(opr, OPCODE_rtype(_opc));

   Is_True(opr != OPR_PARM, // Only created temporarily, should not occur here!
	  ("Unexpected opcode in call to VN_UNARY_EXPR::simplify()"));

   // Unify opcodes that are identical for all architectures we ever
   // need to compile for.  This is controlled by Unify_Rty() called
   // above.
   //
   if (rty != OPCODE_rtype(_opc))
      _opc = OPCODE_make_op(opr, rty, OPCODE_desc(_opc));
      
   if (has_bottom_opnd() || has_top_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
   }
   else if (Is_Literal_Expr(opnd))
   {
     if (MTYPE_is_vector (OPCODE_rtype(_opc))) {
       // TODO: handle expression like "OPC_V16F4V16I4CVT V16I4-const"
       //
     } else if ((static_cast<const VN_LITERAL_EXPR*>(opnd))->is_const_vect ()) {
       // TODO: handle expression like "OPC_F4V16F4REDUCE_ADD VF6F4-const"  
       //
     } else if (
     // for IA-32, do not fold away I8I4CVT
#ifndef TARG_X8664
         !Split_64_Bit_Int_Ops ||
#endif
	 opr != OPR_CVT ||
	 MTYPE_bit_size(OPCODE_rtype(_opc)) != 64 ||
	 MTYPE_bit_size(OPCODE_desc(_opc)) != 32) {
        // Fold literal expression.
        //
        PTR s = Create_Folded_Literal(_opc, opnd, opnd);
        if (s != NULL)
	   simplified = s; // Created new literal
     }
   }
   else if (opr == OPR_NEG         &&
	    MTYPE_is_integral(rty) &&
	    Is_Rty_Opr(opnd, BINARY, OPR_SUB, rty))
   {
      // Convert "-(a - b)" into "b - a"
      //
      simplified = Create_Binary(opnd->get_opc(), 
				 opnd->get_opnd(1), 
				 opnd->get_opnd(0));
   }

   if (simplified != this)
      free();

   return simplified;
} // VN_UNARY_EXPR::simplify
   

//------------- Implementation of VN_BINARY_EXPR  --------------
//--------------------------------------------------------------

void 
VN_BINARY_EXPR::_canonicalize()
{
   // Puts operands in increasing order of VN if commutative,
   // and change opcode for inequalities such that we always
   // use LT or LE and never GT or GE.
   //
   OPCODE opc1 = OPCODE_commutative_op(_opc);
   const OPERATOR opr = OPCODE_operator(_opc);
   if (opc1 == _opc && _vn[0] > _vn[1])
   {
      Switch_Vn_Opnd(_vn[0], _vn[1]);
   }
   else if (opr == OPR_GE ||  opr == OPR_GT )
   {
      Switch_Vn_Opnd(_vn[0], _vn[1]);
      _opc = opc1;
   }
} // VN_BINARY_EXPR::_canonicalize


VN_EXPR::PTR  
VN_BINARY_EXPR::_fold_2literals(OPERATOR  opr1, // ADD or SUB
				CONST_PTR literal1,
				OPERATOR  opr2, // ADD or SUB
				CONST_PTR literal2)
{
   const MTYPE rty = OPCODE_rtype(_opc);
   PTR         result = NULL;

   Is_True((opr1 == OPR_ADD || opr1 == OPR_SUB) &&
	   (opr2 == OPR_ADD || opr2 == OPR_SUB),
	   ("Unexpected operator in VN_BINARY_EXPR::_fold_2literals"));
   Is_True(literal1->get_kind() == LITERAL &&
	   literal2->get_kind() == LITERAL &&
	   MTYPE_is_integral(rty),
	   ("Expected int literal in VN_BINARY_EXPR::_fold_2literals"));

   if (opr1 == OPR_SUB && opr2 == OPR_SUB)
   {
      // Need to negate literal1, before subtracting literal2.
      //
      PTR tmp = Create_Folded_Literal(OPCODE_make_op(OPR_NEG, rty, MTYPE_V),
				      literal1, literal1);

      if (tmp != NULL)
      {
	 result = Create_Folded_Literal(OPCODE_make_op(OPR_SUB, rty, MTYPE_V),
					tmp, literal2);
	 tmp->free();
      }
   }
   else
   {
      // At this point at most one operator may be OPR_SUB and we can
      // simply subtract the corresponding literal from the other.  When
      // they are both OPR_ADD, we simply add the literals.
      //
      if (opr1 == OPR_SUB)
	 result = Create_Folded_Literal(OPCODE_make_op(OPR_SUB, rty, MTYPE_V),
					literal2, literal1);
      else // (opr1 == OPR_ADD)
	 result = Create_Folded_Literal(OPCODE_make_op(opr2, rty, MTYPE_V),
					literal1, literal2);
   }
   return result; // NULL if not folded.
} // VN_BINARY_EXPR::_fold_2literals

   
VN_EXPR::PTR  
VN_BINARY_EXPR::_simplify_2literals(OPERATOR  opr1, // ADD or SUB
				    CONST_PTR literal1,
				    OPERATOR  opr2, // ADD or SUB
				    CONST_PTR literal2,
				    OPERATOR  opr3, // ADD or SUB
				    VN_VALNUM vn3,
				    VN       *v)
{
   // Note that we do not take any signedness of the integral type
   // into account here, beyond the signedness implicit in the TCON
   // given by literal->get_tcon().  All integral LITERALS are represented
   // as 64 bits values.  We apply NEG to unsigned values and may
   // subtract an unsigned value from a smaller unsigned value, where
   // such "negative" unsigned results when fitted into larger 
   // expressions should still yield correct results.
   //
   // Returns "this" when the literals could not be folded; otherwise
   // returns the folded literal (an OPR_ADD operand) operated on by
   // opr3.
   //
   const MTYPE rty = OPCODE_rtype(_opc);
   PTR         simplified = this;
   
   Is_True((opr1 == OPR_ADD || opr1 == OPR_SUB) &&
	   (opr2 == OPR_ADD || opr2 == OPR_SUB) &&
	   (opr3 == OPR_ADD || opr3 == OPR_SUB),
	   ("Unexpected operator in VN_BINARY_EXPR::_simplify_2literals"));
   Is_True(literal1->get_kind() == LITERAL &&
	   literal2->get_kind() == LITERAL &&
	   MTYPE_is_integral(rty),
	   ("Expected int literal in VN_BINARY_EXPR::_simplify_2literals"));

   PTR literal = _fold_2literals(opr1, literal1, opr2, literal2);
   
   // Now, combine the literal result with the third opnd.
   //
   if (literal != NULL)
   {
      if (Targ_Is_Zero(literal->get_tcon()))
      {
	 if (opr3 == OPR_ADD)
	    simplified = Create_Unary(OPC_VPARM, vn3);
	 else
	    simplified = Create_Unary_Opr(OPR_NEG, vn3, rty);
      }
      else
      {
	 VN_VALNUM literal_vn = v->valnum_integer(literal->get_tcon());

	 simplified = Create_Binary_Opr(opr3, literal_vn, vn3, rty, MTYPE_V);
      }
      literal->free();
   }
   
   return simplified;
} // VN_BINARY_EXPR::_simplify_2literals


VN_EXPR::PTR  
VN_BINARY_EXPR::_simplify_3adds(OPERATOR  opr0, // ADD or SUB
				VN_VALNUM vn0,
				OPERATOR  opr1, // ADD or SUB
				VN_VALNUM vn1,
				OPERATOR  opr2, // ADD or SUB
				VN_VALNUM vn2,
				VN       *v)
{
   // This expression expands to: opr0(vn0) + opr1(vn1) + opr2(vn2), and 
   // we will try to reduce it to "d +- e".
   //
   PTR       tmp = NULL;
   PTR       simplified = this;
   CONST_PTR opnd0 = v->valnum_expr(vn0);
   CONST_PTR opnd1 = v->valnum_expr(vn1);
   CONST_PTR opnd2 = v->valnum_expr(vn2);
   
   Is_True((opr0 == OPR_ADD || opr0 == OPR_SUB) &&
	   (opr1 == OPR_ADD || opr1 == OPR_SUB) &&
	   (opr2 == OPR_ADD || opr2 == OPR_SUB),
	   ("Unexpected operator in VN_BINARY_EXPR::_simplify_3adds"));

   // First, try to eliminate equal expressions.
   //
   {
      OPERATOR  opr;
      VN_VALNUM vn;
      
      // Find equal expressions (indicated by virtue of: simplified == NULL).
      //
      if (opr0 != opr1 && vn0 == vn1)
      {
	 simplified = NULL; opr = opr2; vn = vn2;
      }
      else if (opr0 != opr2 && vn0 == vn2)
      {
	 simplified = NULL; opr = opr1; vn = vn1;
      }
      else if (opr1 != opr2 && vn1 == vn2)
      {
	 simplified = NULL; opr = opr0; vn = vn0;
      }

      // Eliminate equal expressions.
      //
      if (simplified == NULL)
      {
	 if (opr == OPR_ADD)
	    simplified = Create_Unary(OPC_VPARM, vn);
	 else
	    simplified = Create_Unary_Opr(OPR_NEG, vn, OPCODE_rtype(_opc));
      }
   } // eliminate equal expressions

   // Next try to fold literal expressions.
   //
   if (simplified == this)
   {
      if (Is_Literal_Expr(opnd0))
      {
	 if (Is_Literal_Expr(opnd1))
	 {
	    // Fold opnd0 and opnd1
	    //
	    simplified = 
	       _simplify_2literals(opr0, opnd0, opr1, opnd1, opr2, vn2, v);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    // Fold opnd0 and opnd2
	    //
	    simplified = 
	       _simplify_2literals(opr0, opnd0, opr2, opnd2, opr1, vn1, v);
	 }
      }
      else if (Is_Literal_Expr(opnd1) && Is_Literal_Expr(opnd2))
      {
	 // Fold opnd1 and opnd2
	 //
	 simplified =
	    _simplify_2literals(opr1, opnd1, opr2, opnd2, opr0, vn0, v);
      }
   } // fold literal expressions
   
   return simplified;
} // VN_BINARY_EXPR::_simplify_3adds


VN_EXPR::PTR  
VN_BINARY_EXPR::_simplify_4adds(OPERATOR  opr0, // ADD or SUB
				VN_VALNUM vn0,
				OPERATOR  opr1, // ADD or SUB
				VN_VALNUM vn1,
				OPERATOR  opr2, // ADD or SUB
				VN_VALNUM vn2,
				OPERATOR  opr3, // ADD or SUB
				VN_VALNUM vn3,
				VN       *v)
{
   // This expression expands to: 
   //
   //    opr0(vn0) + opr1(vn1) + opr2(vn2) + opr3(vn3)
   //
   // and we will assume that there are at most two literals (l1 and l2) and
   // two non-literal (v1 and v2) and we try to reduce it to a literal.
   //
   PTR       simplified = this;
   OPERATOR  opr[4];
   VN_VALNUM opnd[4];
   CONST_PTR opnd_expr[4];

   Is_True((opr0 == OPR_ADD || opr0 == OPR_SUB) &&
	   (opr1 == OPR_ADD || opr1 == OPR_SUB) &&
	   (opr2 == OPR_ADD || opr2 == OPR_SUB) &&
	   (opr3 == OPR_ADD || opr3 == OPR_SUB),
	   ("Unexpected operator in VN_BINARY_EXPR::_simplify_4adds"));

   // Initialize vectors.
   //
   opr[0] = opr0;
   opr[1] = opr1;
   opr[2] = opr2;
   opr[3] = opr3;
   opnd[0] = vn0;
   opnd[1] = vn1;
   opnd[2] = vn2;
   opnd[3] = vn3;
   opnd_expr[0] = v->valnum_expr(vn0);
   opnd_expr[1] = v->valnum_expr(vn1);
   opnd_expr[2] = v->valnum_expr(vn2);
   opnd_expr[3] = v->valnum_expr(vn3);

   // Try to fold the expression into a literal value.
   //
   PTR   literal = NULL;
   INT32 first_literal_idx = -1;
   INT32 non_literal_idx = -1;
   INT32 num_non_literals = 0;
   
   for (INT32 i = 0; i < 4; i++)
   {
      if (literal == NULL && Is_Literal_Expr(opnd_expr[i]))
      {
	 if (first_literal_idx == -1)
	    first_literal_idx = i;  // First literal
	 else
	 {
	    literal = _fold_2literals(opr[first_literal_idx],
				      opnd_expr[first_literal_idx],
				      opr[i],
				      opnd_expr[i]);
	 }
      }
      else
      {
	 if (non_literal_idx == -1)
	 {
	    non_literal_idx = i;  // First non literal
	    ++num_non_literals;
	 }
	 else if (opnd[non_literal_idx] == opnd[i] &&
		  opr[non_literal_idx] != opr[i])
	 {
	    --num_non_literals; // Non literals cancelled eachother out
	    non_literal_idx = -1;
	 }
	 else
	 {
	    ++num_non_literals;
	 }
      }
   }
   
   if (num_non_literals == 0 && literal != NULL)
      simplified = literal;
   else if (literal != NULL)
      literal->free();
   
   return simplified;
} // VN_BINARY_EXPR::_simplify_4adds


VN_EXPR::PTR  
VN_BINARY_EXPR::_simplify_add(CONST_PTR opnd1, CONST_PTR opnd2, VN *v)
{
   // Precondition:  this is an integral expression and 
   //                opnd1 != NULL || opnd2 != NULL.
   //
   // Note that we do not "free" any VN_EXPR here, since simplify()
   // always does that for us.  Also note that we do not create
   // non-existent subexpressions, since these may not have been 
   // value numbered (although we potentially could do so at some expense),
   // although the top-level expression returned here may of course be 
   // unique at this point.
   //
   PTR         simplified = this;
   const MTYPE rty = OPCODE_rtype(_opc);
   
   Is_True(OPCODE_operator(_opc) == OPR_ADD,
	   ("Illegal opcode for VN_BINARY_EXPR::_simplify_add"));
   
   if (Is_Rty_Opr(opnd1, UNARY, OPR_NEG, rty))
   {
      // (-a) + opnd2 => opnd2 - a
      //
      simplified = Create_Binary_Opr(OPR_SUB,
				     get_opnd(1), opnd1->get_opnd(),
				     rty, MTYPE_V);
   }
   else if (Is_Rty_Opr(opnd2, UNARY, OPR_NEG, rty))
   {
      // opnd1 + (-a) => opnd1 - a
      //
      simplified = Create_Binary_Opr(OPR_SUB,
				     get_opnd(0), opnd2->get_opnd(),
				     rty, MTYPE_V);
   }
   else
   {
      // Convert this expression into tertiary expression of the
      // form (+-a) + (+-b) + (+-c), if possible, and try to reduce 
      // it into a binary or singular expression.
      //
      if (Is_Rty_Opr2(opnd1, BINARY, OPR_SUB, OPR_ADD, rty) &&
	  Is_Rty_Opr2(opnd2, BINARY, OPR_SUB, OPR_ADD, rty))
      {
	 simplified = _simplify_4adds(OPR_ADD,
				      opnd1->get_opnd(0),
				      OPCODE_operator(opnd1->get_opc()),
				      opnd1->get_opnd(1),
				      OPR_ADD,
				      opnd2->get_opnd(0),
				      OPCODE_operator(opnd2->get_opc()),
				      opnd2->get_opnd(1),
				      v);
      }
   
      if (simplified == this &&
	  Is_Rty_Opr2(opnd1, BINARY, OPR_SUB, OPR_ADD, rty))
      {
	 simplified = _simplify_3adds(OPR_ADD,
				      opnd1->get_opnd(0),
				      OPCODE_operator(opnd1->get_opc()),
				      opnd1->get_opnd(1),
				      OPR_ADD,
				      get_opnd(1),
				      v);
      }

      if (simplified == this &&
	  Is_Rty_Opr2(opnd2, BINARY, OPR_SUB, OPR_ADD, rty))
      {
	 simplified = _simplify_3adds(OPR_ADD,
				      get_opnd(0),
				      OPR_ADD,
				      opnd2->get_opnd(0),
				      OPCODE_operator(opnd2->get_opc()),
				      opnd2->get_opnd(1),
				      v);
      }
   }
   
   if (simplified != this && !Is_Singular_Expr(simplified))
      simplified = simplified->simplify(v); // Try to simplify again

   return simplified;
   
} // VN_BINARY_EXPR::_simplify_add


VN_EXPR::PTR  
VN_BINARY_EXPR::_simplify_sub(CONST_PTR opnd1, CONST_PTR opnd2, VN *v)
{
   // Precondition:  this is an integral expression and 
   //                opnd1 != NULL || opnd2 != NULL.
   //
   // Note that we do not "free" any VN_EXPR here, since simplify()
   // always does that for us.  Also note that we do not create
   // non-existent subexpressions, since these may not have been 
   // value numbered (although we potentially could do so at some expense),
   // although the top-level expression returned here may of course be 
   // unique at this point.   
   //
   PTR         simplified = this;
   const MTYPE rty = OPCODE_rtype(_opc);
   
   Is_True(OPCODE_operator(_opc) == OPR_SUB,
	   ("Illegal opcode for VN_BINARY_EXPR::_simplify_add"));

   // Convert this expression into tertiary expression of the
   // form (+-a) + (+-b) + (+- c), and try to reduce it into
   // a binary or singular expression.
   //
   if (Is_Rty_Opr2(opnd1, BINARY, OPR_SUB, OPR_ADD, rty) &&
       Is_Rty_Opr2(opnd2, BINARY, OPR_SUB, OPR_ADD, rty))
   {
      const OPERATOR opr3 = (OPCODE_operator(opnd2->get_opc()) == OPR_ADD?
			    OPR_SUB : OPR_ADD);
      
      simplified = _simplify_4adds(OPR_ADD,
				   opnd1->get_opnd(0),
				   OPCODE_operator(opnd1->get_opc()),
				   opnd1->get_opnd(1),
				   OPR_SUB,
				   opnd2->get_opnd(0),
				   opr3,
				   opnd2->get_opnd(1),
				   v);
   }
   
   if (simplified == this &&
       Is_Rty_Opr2(opnd1, BINARY, OPR_SUB, OPR_ADD, rty))
   {
      simplified = _simplify_3adds(OPR_ADD,
				   opnd1->get_opnd(0),
				   OPCODE_operator(opnd1->get_opc()),
				   opnd1->get_opnd(1),
				   OPR_SUB,
				   get_opnd(1),
				   v);
   }
      
   if (simplified == this &&
       Is_Rty_Opr2(opnd2, BINARY, OPR_SUB, OPR_ADD, rty))
   {
      const OPERATOR opr2 = (OPCODE_operator(opnd2->get_opc()) == OPR_ADD?
			    OPR_SUB : OPR_ADD);
      
      simplified = _simplify_3adds(OPR_ADD,
				   get_opnd(0),
				   OPR_SUB,
				   opnd2->get_opnd(0),
				   opr2,
				   opnd2->get_opnd(1),
				   v);
   }
   
   if (simplified != this && !Is_Singular_Expr(simplified))
      simplified = simplified->simplify(v); // Try to simplify again

   return simplified;
   
} // VN_BINARY_EXPR::_simplify_sub


VN_BINARY_EXPR *
VN_BINARY_EXPR::Create(OPCODE           opc, 
		       const VN_VALNUM &vn1, 
		       const VN_VALNUM &vn2) 
{
   Is_True((OPCODE_nkids(opc) == 2 || OPCODE_operator(opc) == OPR_CVTL ||
	    OPCODE_operator(opc) == OPR_EXTRACT_BITS ) && 
	   OPCODE_operator(opc) != OPR_COMMA                            &&
	   OPCODE_operator(opc) != OPR_RCOMMA,
	   ("Unexpected opcode in call to VN_BINARY_EXPR::Create()"));

   VN_BINARY_EXPR *expr = (VN_BINARY_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_BINARY_EXPR(opc, vn1, vn2), _Mpool);
   else
   {
      expr->_opc = opc;
      expr->_vn[0] = vn1;
      expr->_vn[1] = vn2;
   }
   return expr;
} // VN_BINARY_EXPR::Create


VN_EXPR::PTR 
VN_BINARY_EXPR::simplify(VN *v)
{
   // This method always returns a new VN_EXPR and free()'s "this"
   // one when simplification is possible.  OPR_PARM is used to
   // return a single value number, and the returned expression
   // always has fewer than or the same number of operands as "this"
   // expression. 
   //
   // Note that we need not use the lattice notion of equality here.  While
   // Top() values should be treated optimistically when they are part of
   // Phi nodes, a Top() value will be treated as a distinct value here.
   //
   // We try to increase the likelyhood of equivalences by simplifying
   // to some common canonical form whenever possible.
   //
   const OPERATOR opr = OPCODE_operator(_opc);
   const MTYPE    rty = Unify_Rty(opr, OPCODE_rtype(_opc));
   CONST_PTR      opnd1 = v->valnum_expr(_vn[0]);
   CONST_PTR      opnd2 = v->valnum_expr(_vn[1]);
   const BOOL     is_integral = 
      (OPERATOR_is_compare(opr)? 
       MTYPE_is_integral(OPCODE_desc(_opc)) : MTYPE_is_integral(rty));

   PTR   simplified = this;
   INT64 intconst;

   // Unify opcodes that are identical for all architectures we ever
   // need to compile for.  This is controlled by Unify_Rty() called
   // above.
   //
   if (rty != OPCODE_rtype(_opc))
      _opc = OPCODE_make_op(opr, rty, OPCODE_desc(_opc));
      
   if (has_bottom_opnd() || has_top_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
   } else if (MTYPE_is_vector(rty)) {
      // TODO : perform simplification for vectorized bin-op.
   }
   else if (Is_Literal_Expr(opnd1) && Is_Literal_Expr(opnd2))
   {
      PTR s = Create_Folded_Literal(_opc, opnd1, opnd2);
      if (s != NULL)
	 simplified = s; // Created new literal
   }
   else if (is_integral)
   {
      // Note that we generally do not simplify for floating point numbers,
      // due to exceptions that may occur due to NANs and denorms.
      //
      switch (opr)
      {
	 // Simplification when at least one operand is not a literal value.
	 //
      case OPR_ADD:
	 if (Is_Literal_Expr(opnd1) && Targ_Is_Zero(opnd1->get_tcon()))
	    simplified = Create_Unary(OPC_VPARM, _vn[1]);
	 else if (Is_Literal_Expr(opnd2) && Targ_Is_Zero(opnd2->get_tcon()))
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 break;
      
      case OPR_SUB:
	 if (_vn[0] == _vn[1])
	    simplified = Create_Scalar_Literal_From_Int(rty, 0);
	 else if (Is_Literal_Expr(opnd1) && Targ_Is_Zero(opnd1->get_tcon()))
	    simplified = Create_Unary_Opr(OPR_NEG, _vn[1], rty);
	 else if (Is_Literal_Expr(opnd2) && Targ_Is_Zero(opnd2->get_tcon()))
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 break;
      
      case OPR_MPY:
	 if (Is_Literal_Expr(opnd1))
	 {
	    intconst = Targ_To_Host(opnd1->get_tcon());
	    if (MTYPE_is_signed(rty) && intconst == -1)
	       simplified = Create_Unary_Opr(OPR_NEG, _vn[1], rty);
	    else if (intconst == 0)
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	    else if(intconst == 1)
	       simplified = Create_Unary(OPC_VPARM, _vn[1]);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (MTYPE_is_signed(rty) && intconst == -1)
	       simplified = Create_Unary_Opr(OPR_NEG, _vn[0], rty);
	    else if (intconst == 0)
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	    else if(intconst == 1)
	       simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 break;
      
      case OPR_DIV:
	 if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (MTYPE_is_signed(rty) && intconst == -1)
	       simplified = Create_Unary_Opr(OPR_NEG, _vn[0], rty);
	    else if(intconst == 1)
	       simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 break;
      
      case OPR_MOD:
      case OPR_REM:
	 if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (intconst == -1 || intconst == 1)
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	 }
	 break;
      
      case OPR_MAX:
      case OPR_MIN:
	 if (_vn[0] == _vn[1])
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 break;

      case OPR_EQ:
      case OPR_LE:
      case OPR_GE:
	 if (_vn[0] == _vn[1])
	    simplified = Create_Scalar_Literal_From_Int(rty, 1); // TRUE
	 break;
      
      case OPR_NE:
      case OPR_LT:
      case OPR_GT:
	 if (_vn[0] == _vn[1])
	    simplified = Create_Scalar_Literal_From_Int(rty, 0); // FALSE
	 break;
      
      case OPR_BAND:
	 if (_vn[0] == _vn[1])
	 {
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 else if (Is_Literal_Expr(opnd1))
	 {
	    intconst = Targ_To_Host(opnd1->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Unary(OPC_VPARM, _vn[1]);
	 }
	 break;
      
      case OPR_BIOR:
	 if (_vn[0] == _vn[1])
	 {
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[0]);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Scalar_Literal_From_Int(rty, ~(0LL));
	 }
	 else if (Is_Literal_Expr(opnd1))
	 {
	    intconst = Targ_To_Host(opnd1->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[1]);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Scalar_Literal_From_Int(rty, ~(0LL));
	 }
	 break;
      
      case OPR_BNOR:
	 if (_vn[0] == _vn[1])
	 {
	    simplified = Create_Unary_Opr(OPR_BNOT, _vn[0], rty);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary_Opr(OPR_BNOT, _vn[0], rty);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	 }
	 else if (Is_Literal_Expr(opnd1))
	 {
	    intconst = Targ_To_Host(opnd1->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary_Opr(OPR_BNOT, _vn[1], rty);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	 }
	 break;
      
      case OPR_BXOR:
	 if (_vn[0] == _vn[1])
	 {
	    simplified = Create_Scalar_Literal_From_Int(rty, 0);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[0]);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Unary_Opr(OPR_BNOT, _vn[0], rty);
	 }
	 else if (Is_Literal_Expr(opnd1))
	 {
	    intconst = Targ_To_Host(opnd1->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[1]);
	    else if (All_Bits_Set(intconst, MTYPE_bit_size(rty)/8))
	       simplified = Create_Unary_Opr(OPR_BNOT, _vn[1], rty);
	 }
	 break;
      
      case OPR_LAND:
      case OPR_CAND:
	 if (_vn[0] == _vn[1])
	 {
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	    else if (intconst != 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 else if (Is_Literal_Expr(opnd1))
	 {
	    intconst = Targ_To_Host(opnd1->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Scalar_Literal_From_Int(rty, 0);
	    else if (intconst != 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[1]);
	 }
	 break;
      
      case OPR_LIOR:
      case OPR_CIOR:
	 if (_vn[0] == _vn[1])
	 {
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 }
	 else if (Is_Literal_Expr(opnd2))
	 {
	    intconst = Targ_To_Host(opnd2->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[0]);
	    else if (intconst != 0)
	       simplified = Create_Literal(opnd2->get_tcon(), MTYPE_UNKNOWN);
	 }
	 else if (Is_Literal_Expr(opnd1))
	 {
	    intconst = Targ_To_Host(opnd1->get_tcon());
	    if (intconst == 0)
	       simplified = Create_Unary(OPC_VPARM, _vn[1]);
	    else if (intconst != 0)
	       simplified = Create_Literal(opnd1->get_tcon(), MTYPE_UNKNOWN);
	 }
	 break;

      case OPR_SHL:
      case OPR_ASHR:
      case OPR_LSHR:
	 // Just handle the simplest cases for now.
	 //
	 if (Is_Literal_Expr(opnd2) && Targ_Is_Zero(opnd2->get_tcon()))
	    simplified = Create_Unary(OPC_VPARM, _vn[0]);
	 else if (Is_Literal_Expr(opnd1) && Targ_Is_Zero(opnd1->get_tcon()))
	    simplified = Create_Scalar_Literal_From_Int(rty, 0);
	 break;

      } // switch (opr)

      if (simplified == this)
      {
	 // Try to simplify across nested SUB/ADD/NEG expressions.
	 //
	 if (OPCODE_operator(_opc) == OPR_SUB)
	    simplified = _simplify_sub(opnd1, opnd2, v);
	 else if (OPCODE_operator(_opc) == OPR_ADD)
	    simplified = _simplify_add(opnd1, opnd2, v);
      }
   } // if (is_integral)

   if (simplified != this)
      free(); // this

   return simplified;
} // VN_BINARY_EXPR::simplify


//------------- Implementation of VN_TERNARY_EXPR  -------------
//--------------------------------------------------------------

VN_TERNARY_EXPR *
VN_TERNARY_EXPR::Create(OPCODE           opc, 
			const VN_VALNUM &vn1, 
			const VN_VALNUM &vn2, 
			const VN_VALNUM &vn3) 
{
   Is_True(OPCODE_nkids(opc) == 3 || OPCODE_operator(opc) == OPR_COMPOSE_BITS,
	  ("Unexpected opcode in call to VN_TERNARY_EXPR::Create()"));

   VN_TERNARY_EXPR *expr = (VN_TERNARY_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_TERNARY_EXPR(opc, vn1, vn2, vn3), _Mpool);
   else
   {
      expr->_opc = opc;
      expr->_vn[0] = vn1;
      expr->_vn[1] = vn2;
      expr->_vn[2] = vn3;
   }
   return expr;
} // VN_TERNARY_EXPR::Create


VN_EXPR::PTR 
VN_TERNARY_EXPR::simplify(VN *v)
{
   PTR            simplified = this;
   PTR            cond = v->valnum_expr(_vn[0]);
   const OPERATOR opr = OPCODE_operator(_opc);
   const MTYPE    rty = Unify_Rty(opr, OPCODE_rtype(_opc));

   // Unify opcodes that are identical for all architectures we ever
   // need to compile for.  This is controlled by Unify_Rty() called
   // above.
   //
   if (rty != OPCODE_rtype(_opc))
      _opc = OPCODE_make_op(opr, rty, OPCODE_desc(_opc));
      
   if (has_bottom_opnd() || has_top_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
   }
   else if (opr == OPR_SELECT || opr == OPR_CSELECT)
   {
      if (Is_Literal_Expr(cond))
      {
	 if (Targ_Is_Zero(cond->get_tcon()))
	    simplified = Create_Unary(OPC_VPARM, _vn[2]); // False condition
	 else
	    simplified = Create_Unary(OPC_VPARM, _vn[1]); // True condition
      }
      else if (_vn[1] == _vn[2])
      {
	 // The condition is irrelevant.
	 //
	 simplified = Create_Unary(OPC_VPARM, _vn[1]);
      }
      else if (_vn[0].is_top())
      {
	 // This optimistic assumption may be redundant for value numbering,
	 // but we insert it here anyway for generality.  We expect Phi
	 // nodes to catch all optimistic "unknown" values and resolve them
	 // to real value numbers.
	 //
	 simplified = Create_Unary(OPC_VPARM, _vn[1]); // Assume True condition
      }
   }

   if (simplified != this)
      free();

   return simplified;
} // VN_TERNARY_EXPR::simplify


//------------- Implementation of VN_INTR_OP_EXPR  -------------
//--------------------------------------------------------------

VN_INTR_OP_EXPR *
VN_INTR_OP_EXPR::Create(INTRINSIC intr_opc, 
			UINT32    num_opnds)
{
   VN_INTR_OP_EXPR *expr = (VN_INTR_OP_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_INTR_OP_EXPR(intr_opc, num_opnds), _Mpool);
   else
   {
      expr->_intr_opc = intr_opc;
      expr->_num_opnds = num_opnds;

      if (num_opnds > 3)
	 expr->_opnd_array = CXX_NEW_ARRAY(VN_VALNUM_PAIR, num_opnds, _Mpool);
   }
	 
   for (INT i = 0; i < num_opnds; i++)
   {
      expr->set_opnd(i, VN_VALNUM::Bottom());      // Must be equivalent
      expr->set_opnd_vsym(i, VN_VALNUM::Bottom()); // Must match exactly
   }

   return expr;
} // VN_INTR_OP_EXPR::Create


VN_EXPR::PTR 
VN_INTR_OP_EXPR::simplify(VN *v)
{
   PTR       simplified = this;
   const INT num_opnds = get_num_opnds();

   if (has_bottom_opnd() || has_top_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
   }
   else if (num_opnds <= 8)
   {
      TCON arg_tconsts[8];    // Handle intrinsics with at most 8 operands
      INT  i;

      for (i = 0; 
	   (i < num_opnds && Is_Literal_Expr(v->valnum_expr(get_opnd(i))));
	    i++)
      {
	 arg_tconsts[i] = v->valnum_expr(get_opnd(i))->get_tcon();
      }

      if (i == num_opnds)
      {
	 // All arguments are literal constants, so try to fold the expression!
	 //
	 BOOL folded = FALSE;
	 TCON folded_tcon = Targ_IntrinsicOp(_intr_opc, arg_tconsts, &folded);
	 if (folded)
	 {
	    simplified = Create_Literal(folded_tcon, MTYPE_UNKNOWN);
	 }
      }
   }

   if (simplified != this)
      free();

   return simplified;
} // VN_INTR_OP_EXPR::simplify


//--------------- Implementation of VN_PHI_EXPR  ---------------
//--------------------------------------------------------------

VN_PHI_EXPR *
VN_PHI_EXPR::Create(UINT32  num_opnds,
		    IDTYPE  block_id,
		    PHI_TAG phi_tag)
{
   VN_PHI_EXPR *expr = (VN_PHI_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_PHI_EXPR(num_opnds, block_id, phi_tag), _Mpool);
   else
   {
      expr->_num_opnds = num_opnds;
      expr->_block_id = block_id;
      expr->_phi_tag = phi_tag;
   }
   
   if (num_opnds > 3)
      expr->_vn_array = CXX_NEW_ARRAY(VN_VALNUM, num_opnds, _Mpool);

   for (INT i = 0; i < num_opnds; i++)
      expr->set_opnd(i, VN_VALNUM::Bottom());

   return expr;
} // VN_PHI_EXPR::Create


VN_EXPR::PTR 
VN_PHI_EXPR::simplify(VN *)
{
   PTR simplified = this;

   if (has_bottom_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
   }
   else
   {
      // When all operands of a phi node are equivalent, then it can be
      // folded into an existing value number.  This is the only place
      // where you will see the use of the lattice notion of equality'
      // ("equivalent_to"); in all other cases we deal specially with 
      // Top() and Bottom() value numbers, and use the identity equality
      // test ("==").
      //
      VN_VALNUM vn = get_opnd(0);
      BOOL      match = TRUE;
      
      for (INT i = 1; match && i < get_num_opnds(); i++)
      {
	 match = vn.equivalent_to(get_opnd(i));
	 if (match && vn.is_top())
	    vn = get_opnd(i);
      }
      if (match)
	 simplified = Create_Unary(OPC_VPARM, vn);
      else if (has_top_opnd())
	 simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
   }

   if (simplified != this)
      free();

   return simplified;
} // VN_PHI_EXPR::simplify


//----------- Implementation of VN_LDA_ADDR_EXPR  --------------
//--------------------------------------------------------------

VN_LDA_ADDR_EXPR *
VN_LDA_ADDR_EXPR::Create(INT32 lda_cr_id)
{
   VN_LDA_ADDR_EXPR *expr = (VN_LDA_ADDR_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_LDA_ADDR_EXPR(lda_cr_id), _Mpool);
   else
      expr->_lda_cr_id = lda_cr_id;
   return expr;
} // VN_LDA_ADDR_EXPR::Create


//----------- Implementation of VN_ARRAY_ADDR_EXPR  ------------
//--------------------------------------------------------------

VN_ARRAY_ADDR_EXPR *
VN_ARRAY_ADDR_EXPR::Create(WN_ESIZE esize, INT32 num_dims)
{
   VN_ARRAY_ADDR_EXPR *expr = (VN_ARRAY_ADDR_EXPR *)_Free->pop();
   const UINT32        num_opnds = 1 + 2*num_dims;

   if (expr == NULL)
      expr = CXX_NEW(VN_ARRAY_ADDR_EXPR(esize, num_dims), _Mpool);
   else
   {
      expr->_num_opnds = num_opnds;
      expr->_esize = esize;

      if (num_opnds > 3)
	 expr->_vn_array = CXX_NEW_ARRAY(VN_VALNUM, num_opnds, _Mpool);
   }

   for (INT i = 0; i < num_opnds; i++)
      expr->set_opnd(i, VN_VALNUM::Bottom());

   return expr;
} // VN_ARRAY_ADDR_EXPR::Create


VN_EXPR::PTR 
VN_ARRAY_ADDR_EXPR::simplify(VN *)
{
   PTR simplified;

   if (has_bottom_opnd() || has_top_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
      free();
   }
   else
   {
      simplified = this;
   }
   return simplified;
} // VN_ARRAY_ADDR_EXPR::simplify



//------------- Implementation of VN_MEMLOC_EXPR  --------------
//--------------------------------------------------------------

VN_MEMLOC_EXPR *
VN_MEMLOC_EXPR::Create(MTYPE            dsctype,
		       const VN_VALNUM &bytesize,
		       const VN_VALNUM &offset,
		       const VN_VALNUM &base_addr,
		       const VN_VALNUM &vsym_valnum)
{
   
   VN_MEMLOC_EXPR *expr = (VN_MEMLOC_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_MEMLOC_EXPR(dsctype, 
				    bytesize, offset, base_addr, vsym_valnum),
		     _Mpool);
   else
   {
      expr->_dsctype = dsctype;
      expr->_bytesize = bytesize;
      expr->_offset = offset;
      expr->_base_addr = base_addr;
      expr->_vsym_valnum = vsym_valnum;
   }
   return expr;
} // VN_MEMLOC_EXPR::Create


VN_EXPR::PTR 
VN_MEMLOC_EXPR::simplify(VN *v)
{
   PTR simplified;

   if (has_bottom_opnd() || has_top_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
   }
   else
   {
      PTR offset_expr = v->valnum_expr(_offset);

      if (!Is_Literal_Expr(offset_expr) ||
	  Targ_To_Host(offset_expr->get_tcon()) != 0)
      {
	 // Try to fold the offset into the base-address expression.
	 //
	 PTR address = Create_Binary_Opr(OPR_ADD,
					 _offset, _base_addr,
					 Pointer_type, MTYPE_V);
	 PTR addr_simplified = address->simplify(v);

	 if (addr_simplified->get_kind() == VN_EXPR::UNARY &&
	     addr_simplified->get_opc() == OPC_VPARM)
	 {
	    VN_VALNUM base_addr = addr_simplified->get_opnd(0);
	    VN_VALNUM zero_ofst = 
	       v->valnum_integer(Host_To_Targ(Pointer_type, 0));

	    simplified = 
	       VN_EXPR::Create_Memloc(_dsctype, 
				      _bytesize,
				      zero_ofst,
				      base_addr,
				      _vsym_valnum);
	 }
	 else
	 {
	    simplified = this;
	 }

	 // Free up the temporarily created address expression.
	 //
	 addr_simplified->free();
      }
      else
      {
	 simplified = this;
      }
   }

   if (simplified != this)
      free();

   return simplified;
} // VN_MEMLOC_EXPR::simplify


#ifdef KEY
//------------- Implementation of VN_CALL_OP_EXPR  -------------
//--------------------------------------------------------------

VN_CALL_OP_EXPR *
VN_CALL_OP_EXPR::Create(ST_IDX    aux_id,
			UINT32    num_opnds)
{
   VN_CALL_OP_EXPR *expr = (VN_CALL_OP_EXPR *)_Free->pop();
   if (expr == NULL)
      expr = CXX_NEW(VN_CALL_OP_EXPR(aux_id, num_opnds), _Mpool);
   else
   {
      expr->_aux_id = aux_id;
      expr->_num_opnds = num_opnds;

      if (num_opnds > 3)
	 expr->_opnd_array = CXX_NEW_ARRAY(VN_VALNUM_PAIR, num_opnds, _Mpool);
   }
	 
   for (INT i = 0; i < num_opnds; i++)
   {
      expr->set_opnd(i, VN_VALNUM::Bottom());      // Must be equivalent
      expr->set_opnd_vsym(i, VN_VALNUM::Bottom()); // Must match exactly
   }

   return expr;
} // VN_CALL_OP_EXPR::Create


VN_EXPR::PTR 
VN_CALL_OP_EXPR::simplify(VN *)
{
   PTR simplified;

   if (has_bottom_opnd() || has_top_opnd())
   {
      simplified = Create_Unary(OPC_VPARM, VN_VALNUM::Bottom());
      free();
   }
   else
   {
      simplified = this;
   }
   return simplified;
} // VN_CALL_OP_EXPR::simplify
#endif
