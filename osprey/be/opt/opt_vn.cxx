//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vn.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vn.cxx,v $
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
//    The "VN::_status.changed" member will be reset and updated on each
//    iteration of the value numbering algorithm over the CFG, and
//    reflects whether or not value numbering changed for any CODEREP
//    in an iteration.  The value numbering algorithm is rooted at the 
//    constructor (VN::VN()) and traverses the dominator tree by means
//    of the internal _valnum methods. 
//
//    Note that we *always* use _set_valnum() to assign to _exprid_to_vn[].  
//    Hence, we should *never* see any occurences of "_exprid_to_vn[xxx] = "
//    in this file!
//
//  * DEBUGGING:  Selective tracing of the iterative value numbering 
//    process can be activated by modifying the Vn_Tracing macro
//    accordingly.  All expressions and phi's of the given CR ids will 
//    be traced in the value-numbering process to stderr, except when
//    they occur as the lhs of stores.  Also look at traces in the .t file.
//
//  * Use DPOBB_ITER to traverse the dominator tree BBs in preorder and/or
//    RPOBB_ITER to traverse the CFG in reverse postorder.
//
//  * Initialize value numbers 1..VN_MAX_PREDEFINED_INT+1 to be integer 
//    constants 0..VN_MAX_PREDEFINED_INT in _vn_to_expr.  There is no
//    need to enter these into the hash-table, since _valnum_integer()
//    will always access them directy.
//
//  * Use codemap->Coderep_id_cnt() to determine sizes for hash-table
//    and/or the vectors.
//
//  * We never use CODEREP::Coderep_id() directly, but instead call
//    _get_exprid().  This is essential since we need to create an EXPRID
//    to CODEREP* mapping.  TODO: Remove this mapping when one is needed
//    outside of value numbering.
//
//  * Initialize _exprid_to_vn to map to Top() for iterative analysis, and
//    to Bottom() for single-pass analysis.  Zero version CK_VAR codereps
//    should ideally be Bottom() for both single and iterative analysis,
//    and we reset such variables at store sites (STID, CHI, PHI nodes) and
//    in _valnum_sym().
//
//  * Initialize _locked_to_vn to FALSE.
//
//  * _vn_to_expr will initialize itself as entries are inserted.
//    When the VN is not associated with a defining expression (e.g.
//    for an expression containing top/bottom values, or a Chi or ENTRY
//    node), the entry will map to NULL (this is automatic and does not
//    need to be done explicitly).  The first 33 entries will
//    map to LITERAL expressions holding value "0" to "32".
//
//  * We do not need to account for the offsets in LDID and STID nodes,
//    since different offsets means different Coderep_id(), and we simply
//    map the Coderep_id() to a value number without any hashing.  For
//    Indirect stores, we currently keep the base address and the offset
//    as seperate attributes of a MEMLOC, while we should ideally create
//    a binary expression and add them together (using offset==0) to take
//    full advantage of equivalences we could find by means simplification
//    of the this complete address expression. TODO!
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
#include "targ_const.h"	  // for TCON-related stuff
#include "const.h"	  // for symbol/TCON-related

#include "opt_config.h"
#include "opt_wn.h"
#include "opt_util.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "opt_cvtl_rule.h"

#include "opt_vn.h"
#include "opt_vn_expr_taxonomy.h"

// Number of predefined value numbers for integer literals.
//
#define VN_MAX_PREDEFINED_INT 32
#define Coderep_id ERROR_DO_NOT_USE_THIS_USE::get_exprid
#define CVTL_BITSIZE_IS_SIGNED TRUE

// Macro so that EXTRACT_BITS amd COMPOSE_BITS only need one additional operand
#define OFFSET_AND_SIZE_TOGETHER(x,y) (((x)<<8) + (y))


inline BOOL Vn_Tracing(VN::EXPRID id)
{
   return FALSE; // (id == 337 || id == 369 || id == 398 || id == 402);
}

inline INT Need_Integral_Conversion(MTYPE from_ty, MTYPE to_ty, OPCODE *opc)
{
   if (MTYPE_is_integral(from_ty) && MTYPE_is_integral(to_ty)
#ifdef KEY
       && !MTYPE_is_vector(from_ty)
#endif
	)
      return Need_type_conversion(from_ty, to_ty, opc);
   else
      return NOT_AT_ALL;
} // Need_Integral_Conversion


void 
VN::_trace(EXPRID id, VN_VALNUM valnum, FILE *fp)
{
   fprintf(fp, "<cr %d> = ", (int)id);
   valnum.print(fp);
   fprintf(fp, " = ");
   if (valnum_expr(valnum) == NULL)
      fputs(" ==> ...Chi result, or has Bottom opnd", fp);
   else
      valnum_expr(valnum)->print(fp);
   fprintf(fp, "\n");
} // VN::_trace


void 
VN::_grow_exprid_maps(EXPRID id)
{
   // Grow maps indexed by EXPRID to encompass the given id number.
   //
   while (_exprid_to_cr.capacity() <= id)
   {
      const INT32 new_capacity = (_exprid_to_cr.capacity() + 
				  _exprid_to_cr.capacity()/2 + 1);

      _exprid_to_cr.reserve(new_capacity);
      _exprid_to_vn.reserve(new_capacity);
      _exprid_to_stmtlist.reserve(new_capacity);
   }
   while (id >= _exprid_to_cr.size())
   {
      _exprid_to_cr.push_back(NULL);
      _exprid_to_vn.push_back(VN_VALNUM::Bottom());
      _exprid_to_stmtlist.push_back(STMT_LIST(STMT_ALLOCATOR(_gpool)));
   }
} // VN::_grow_exprid_maps


inline void 
VN::_set_stmt_map(CODEKIND ck, EXPRID id, const VN_VALNUM &vn)
{
   // Maps the coderep to the current statement. This sets up a 
   // CODEREP==>STMTREP mapping, only the first time around the 
   // iterative algorithm, and with exclusion of the following:
   //
   //    1) Any statement for which _current_stmt is explicitly set to
   //       be NULL (e.g. lhs IVAR or VAR).
   //
   //    2) The coderep has Top() or Bottom() as value number.  Any
   //       expression we care about must get some valid value number
   //       on the first iteration, even if it changes in subsequent 
   //       iterations.
   //
   //    3) The coderep has already been encountered for the 
   //       _current_statement (i.e. it is at the head of the list).
   //
   //    4) The coderep is a CK_CONST or CK_RCONST (the value number must
   //       denote a LITERAL expression).  Such codereps will not change
   //       value numbers in subsequent iterations and they will not be
   //       required in the statement lists.  This substantially decreases
   //       the sizes of the statement lists in general.
   //
   if (_no_of_iterations == 0 &&
       _current_stmt != NULL  &&                                 // (1)
       !vn.is_bottom() && !vn.is_top())                          // (2)
   {
      if ((_exprid_to_stmtlist[id].empty() ||                    // (3)
	   _current_stmt != _exprid_to_stmtlist[id].front()) &&
	  (ck != CK_CONST && ck != CK_RCONST))                   // (4)
								     
      {
	 _exprid_to_stmtlist[id].push_front(_current_stmt);
      }
   }
} // VN::_set_stmt_map


void 
VN::_set_valnum(EXPRID           id, 
		const VN_VALNUM &vn,
		VALNUM_VECTOR   &exprid_to_vn, // May be map for implicit opc
		BIT_VECTOR      &locked_to_vn) // May be map for implicit opc
{
   Is_True(id != 0, 
	   ("Unexpected coderep id (%d) in VN::_set_valnum()", INT32(id)));

   // Only when the given EXPRID is not already mapped to the given value
   // number do we need to change the _exprid_to_vn mapping.
   //
   if (exprid_to_vn[id] != vn)
   {
      
      _status.changed = TRUE;
      exprid_to_vn[id] = vn;

      // If the value number is a new unique value number, we will lock this
      // EXPRID to the given value number.  Note that it may subsequently
      // be unlocked again, if it was previously locked (i.e. unique) and
      // subsequently (in later iterations of the value numbering algorithm)
      // found to be equivalent to another existing value number.
      //
      if (vn == _next_valnum)
      {
	 locked_to_vn[id] = TRUE;
	 _next_valnum = VN_VALNUM::Next(_next_valnum);
      }
      else if (locked_to_vn[id])
      {
	 locked_to_vn[id] = FALSE;
      }
   }
   (*_status.is_numbered)[id] = TRUE;
} // VN::_set_valnum


void
VN::_init_integer_valnum_map()
{
   // Reserve the first VN_MAX_PREDEFINED_INT+1 entries of the VN->EXPR 
   // mapping for integers 0..VN_MAX_PREDEFINED_INT.  Note that we do
   // not put these into the hash-table (expr_to_vn), since we never 
   // expect to look integral literal expressions up in the hash-table.
   //
   _zero_valnum = _next_valnum;
   for (INT64 literal = 0; literal <= VN_MAX_PREDEFINED_INT; literal++)
   {
      const TCON         tc = Host_To_Targ(MTYPE_I8, literal);
      const VN_EXPR::PTR expr = VN_EXPR::Create_Literal(tc, MTYPE_UNKNOWN);

      _vn_to_expr.set_map(_next_valnum, expr);
      _next_valnum = VN_VALNUM::Next(_next_valnum);
   }
} // VN::_init_integer_valnum_map

   
VN_VALNUM 
VN::_valnum_integer(INT64 literal, 
		    BOOL  is_signed)
{
   // Map a literal integral value to a value number.  We will create 
   // a new VN_EXPR when it is necessary and allocate a hash-table entry
   // to denote the literal expression.
   //
   // One advantage of this scheme is that all integral values will be
   // normalized to one type (I8), and as such may match any other integral
   // value irrespective of integral type.  I.e. an I4 value "17" will match
   // both a U1 value "17" and an I8 value "17", and we thereby detect more
   // equivalences than if the type also was significant.
   //
   // Note that we never _set_valnum() here; the caller of this routine must
   // do so if we have a corresponding expression identifier (this will not
   // always be the case .. e.g. for an ILOAD offset).
   //
   // Also, note that we never set "locked_to_vn", but it is imperative
   // for the iterative algorithm that we retain an invariant mapping from
   // literal values to value numbers to guarantee termination (otherwise
   // an ILOAD offset will get a new value number on each iteration and
   // "changed" will always remain true).  To solve this problem we retain
   // the expr_to_vn mappings for literal values between iterations (see
   // VN::VN for details).
   //
   VN_VALNUM valnum;

   if (literal >= 0 && literal < VN_MAX_PREDEFINED_INT)
      valnum = _get_literal_valnum(literal);
   else
   {
      // This is not one of those literals we predefined a value number for,
      // so go through the hash-table, as usual.
      //
      const MTYPE                    mty = (is_signed? MTYPE_I8 : MTYPE_U8);
      const TCON                     tc = Host_To_Targ(mty, literal);
      const VN_EXPR::PTR             expr = VN_EXPR::Create_Literal(tc, MTYPE_UNKNOWN);
      const VN_HASHTAB::EXPR_MAPPING map = 
	 _status.expr_to_vn->lookup_or_insert(expr, _next_valnum);

      valnum = map.second;
      if (map.first == expr) // New entry inserted!
      {
	 // This was the first occurrence of this value, and a new entry
	 // was inserted into the hash-table!
	 //
	 _vn_to_expr.set_map(valnum, expr);
	 _next_valnum = VN_VALNUM::Next(_next_valnum);
      }
      else // found; _vn_to_expr is already set!
      {
	 // This value was encountered before, and all maps have already
	 // been set up for it.
	 //
	 expr->free();
      }
   }
   return valnum;
} // VN::_valnum_integer


VN_VALNUM 
VN::_valnum_vn_expr(EXPRID         exprid,   // Coderep (or WHIRL?) id
		    VN_EXPR::PTR   expr,
		    VALNUM_VECTOR &exprid_to_vn,
		    BIT_VECTOR    &locked_to_vn)
{
   // Get and return a value number for the given valnum expression and set
   // the various tables accordingly.  We do not expect this exprid to
   // already have been value numbered in this iteration of the value-
   // numbering algorithm.  An exprid of zero (0) is treated specially in
   // this respect, and will signify a VN_EXPR without a corresponding
   // coderep (see VN::_set_valnum).
   //
   // We do handle Bottom() operands in _valnum_vn_expr(), since we do 
   // expect to see such operands for Phi expressions which are handled
   // here.  Such operands may also occur when a VAR references a 
   // variable that has not been defined (e.g. an uninitialized or a
   // volatile variable).
   //
   // We never return a Bottom() value number here, instead we use a 
   // "unique" value-number, since that will allow us to do redundancy
   // elimination on subsequent references to the unique value.  E.g. 
   // if v is a volatile variable:
   //
   //   x = v
   //   y = x
   //   z = y
   //   ...... x ... y ... y ... x ... z .. y ... z
   //
   // we can replace all occurrences of x, y, and z above with one
   // preg. If x, y, and z were assigned _|_ value numbers, then they
   // couldnot have been considered the same value.
   //
   VN_VALNUM    valnum;
   VN_EXPR::PTR simplified = expr->simplify(this);

   Is_True(!(*_status.is_numbered)[exprid],
	   ("Each expression %d should only be value numbered once "
	    "in VN::_valnum_vn_expr()", (INT32)exprid));
   
   if (simplified->get_kind() == VN_EXPR::LITERAL && 
       MTYPE_is_integral(TCON_ty(simplified->get_tcon())) &&
       !(static_cast<const VN_LITERAL_EXPR*>(simplified))->is_const_vect ())
   {
      const BOOL  is_signed = MTYPE_is_signed(TCON_ty(simplified->get_tcon()));
      const INT64 intval = Targ_To_Host(simplified->get_tcon());
      
      simplified->free();
      valnum = _valnum_integer(intval, is_signed);
      Is_True(valnum != _next_valnum, 
	      ("Expected _next_valnum to be updated in  _valnum_integer()!"));
   }
   else if (simplified->has_bottom_opnd())
   {
      // We pessimistically know nothing about an operand (top or bottom),
      // and as such we just invent a unique value number for the
      // expression.  Since it should never match any other expression,
      // we should not enter such expressions into the hash-table.
      //
      // Note that this also work for expressions containing volatile or
      // zero-version operands, which have bottom value numbers, since
      // such expression-trees should NEVER have multiple references (i.e.
      // be shared) in the CFG!
      //
      // Any optimistic assumptions about Top() operands should have been
      // handled in the simplification process.
      //
      Is_True(simplified->get_kind() == VN_EXPR::UNARY && 
	      simplified->get_opc() == OPC_VPARM,
	      ("simplify() should have reduced expression to Bottom()!"));

      valnum = _unique_valnum(exprid, exprid_to_vn, locked_to_vn);
      // _vn_to_expr[valnum] is NULL by default
      simplified->free();
   }
   else if (simplified->get_kind() == VN_EXPR::UNARY &&
	    simplified->get_opc() == OPC_VPARM)
   {
      // The expression was simplified down to an existing value number.
      //
      valnum = simplified->get_opnd(0);  // _vn_to_expr is already set
      simplified->free();
   }
   else if (simplified->get_kind() == VN_EXPR::UNARY &&
	    OPCODE_is_volatile(simplified->get_opc()))
   {
      // An example of this is an OPC_ALLOCA.
      //
      valnum = _unique_valnum(exprid, exprid_to_vn, locked_to_vn);
      simplified->free();
   }
   else
   {
      // We look for an identical expression in the VN_EXPR --> VALNUM mapping.
      //
      // Note that the expression may contain Top() operands, in which case
      // hashing will only identify it with other identical expressions with
      // Top() operands in the same operand position.
      //
      const VN_VALNUM new_valnum = 
	 _unique_valnum(exprid, exprid_to_vn, locked_to_vn);
      VN_HASHTAB::EXPR_MAPPING vn_map = 
	 _status.expr_to_vn->lookup_or_insert(simplified, new_valnum);

      valnum = vn_map.second;
      if (vn_map.first == simplified) // Inserted simplified into expr_to_vn
	 _vn_to_expr.set_map(valnum, simplified);
      else                              // Already existed in expr_to_vn
	 simplified->free();
   }

   // Set the mapping from the coderep to the value number.
   //
   _set_valnum(exprid, valnum, exprid_to_vn, locked_to_vn);
   
   return valnum;
} // VN::_valnum_vn_expr


VN_VALNUM 
VN::_valnum_sym(CODEREP *sym)
{
   // We never invent value numbers for symbols, with exception of zero
   // versions which are corrected from an optimistic Top() value to a
   // pessimistic Bottom() value here.  Symbols either have already been
   // assigned a value number (as a result of a store or chi), they are
   // zero versions for which every reference has a unique value number
   // (i.e. Bottom()), or they are as of yet undefined symbols referenced 
   // in phi nodes (as a result of backwards data-flow).
   //
   // Note that "_status.changed" remain unchanged, with exception for
   // zero version initialization, which may cause it to become TRUE for
   // an optimistic algorithm.
   //
   VN_VALNUM valnum = VN_VALNUM::Bottom();
   
   Is_True(sym == NULL || sym->Kind() == CK_VAR, 
	   ("Unexpected coderep kind in VN::_valnum_sym()"));

   if (sym != NULL)
   {
      const EXPRID id = _get_exprid(sym);

      if (sym->Is_var_volatile() || sym->Is_flag_set(CF_IS_ZERO_VERSION))
      {
	 _set_valnum(id, VN_VALNUM::Bottom(),
		     _exprid_to_vn,
		     *_status.locked_to_vn);
      }
      else
      {
	 // Note that we do not take into account any implicit conversion
	 // here, since we assume all such conversions have been taken
	 // care of at the definition point of this symbol.  Each CK_VAR
	 // should be used with one, and only one, type of conversion,
	 // hence the defn point gives the unique valnum.
	 // 
	 valnum = _exprid_to_vn[id];
      }
   }
   return valnum;
} // VN::_valnum_sym


VN_VALNUM 
VN::_valnum_op(CODEREP *cr)
{
   Is_True(cr->Kind() == CK_OP, 
	   ("Unexpected coderep kind in VN::_valnum_op()"));

   VN_EXPR::PTR vn_expr_ptr;
   VN_VALNUM    valnum;
   const EXPRID exprid = _get_exprid(cr);
   OPCODE       opc = cr->Op();

   if (OPCODE_is_fake(opc))
   {
      // Calls (not counting OPR_INTRINSIC_OPs) are not real 
      // expressions, and they should have been handled in valnum_stmt().
      //
      FmtAssert(FALSE, ("Unexpected opcode in VN::_valnum_op()"));
   }
   else if (opc == OPC_VPARM)
   {
      // Set the parm to have the same value-number as its operand
      //
      valnum = _valnum_expr(cr->Opnd(0));
      _set_valnum(exprid, valnum, _exprid_to_vn, *_status.locked_to_vn);
   }
   else if (OPCODE_operator(opc) == OPR_INTRINSIC_OP)
   {
      vn_expr_ptr = VN_EXPR::Create_Intr_Op((INTRINSIC)cr->Intrinsic(), 
					    cr->Kid_count());

      for (INT32 i = 0; i < cr->Kid_count(); i++) 
      {
	 CODEREP *const  vsym = cr->Opnd(i)->Get_ivar_vsym(); // May be NULL
	 const VN_VALNUM vsym_valnum = _valnum_sym(vsym);

	 valnum = _valnum_expr(cr->Opnd(i));
	 vn_expr_ptr->set_opnd(i, valnum);
	 vn_expr_ptr->set_opnd_vsym(i, vsym_valnum);
      }
      valnum = _valnum_vn_expr(exprid, vn_expr_ptr,
			       _exprid_to_vn, *_status.locked_to_vn);

   }
#ifdef KEY
   else if (OPCODE_operator(opc) == OPR_PURE_CALL_OP)
   {
      vn_expr_ptr = VN_EXPR::Create_Call_Op(cr->Call_op_aux_id(),
                                            cr->Kid_count());

      for (INT32 i = 0; i < cr->Kid_count(); i++)
      {
         CODEREP *const vsym = cr->Opnd(i)->Get_ivar_vsym(); // May be NULL
	 const VN_VALNUM vsym_valnum = _valnum_sym(vsym);

	 valnum = _valnum_expr(cr->Opnd(i));
	 vn_expr_ptr->set_opnd(i, valnum);
	 vn_expr_ptr->set_opnd_vsym(i, vsym_valnum);
      }
      valnum = _valnum_vn_expr(exprid, vn_expr_ptr,
                               _exprid_to_vn, *_status.locked_to_vn);
   }
#endif
   else if (OPCODE_operator(opc) == OPR_ARRAY)
   {
      vn_expr_ptr = VN_EXPR::Create_Array_Addr(cr->Elm_siz(), cr->Num_dim());

      for (INT32 i = 0; i < cr->Kid_count(); i++) 
      {
	 valnum = _valnum_expr(cr->Opnd(i));
	 vn_expr_ptr->set_opnd(i, valnum);
      }
      valnum = _valnum_vn_expr(exprid, vn_expr_ptr,
			       _exprid_to_vn, *_status.locked_to_vn);
   }
   else
   {
      VN_VALNUM vn0, vn1, vn2;
      
      switch (cr->Kid_count())
      {
      case 1:
	 if (cr->Opr() == OPR_CVTL)
	 {
	    // We treat a CVTL as a binary operator!
	    //
	    vn0 = _valnum_expr(cr->Opnd(0));
	    vn1 = _valnum_integer(cr->Offset(), CVTL_BITSIZE_IS_SIGNED);
	    vn_expr_ptr = VN_EXPR::Create_Binary(opc, vn0, vn1);
	 } else if (cr->Opr() == OPR_EXTRACT_BITS) {
	   // also treat EXTRACT_BITS as binary
	    vn0 = _valnum_expr(cr->Opnd(0));
	    vn1 = _valnum_integer(OFFSET_AND_SIZE_TOGETHER(cr->Op_bit_size(),cr->Op_bit_offset()), FALSE);
	    vn_expr_ptr = VN_EXPR::Create_Binary(opc, vn0, vn1);
	 }
	 else
	 {
	    vn0 = _valnum_expr(cr->Opnd(0));
	    vn_expr_ptr = VN_EXPR::Create_Unary(opc, vn0);
	 }
	 break;
	 
      case 2:
	if (cr->Opr() == OPR_COMPOSE_BITS) {
	  // treat COMPOSE_BITS as ternary
	  vn0 = _valnum_expr(cr->Opnd(0));
	  vn1 = _valnum_expr(cr->Opnd(1));
	  vn2 = _valnum_integer(OFFSET_AND_SIZE_TOGETHER(cr->Op_bit_size(),cr->Op_bit_offset()), FALSE);
	  vn_expr_ptr = VN_EXPR::Create_Ternary(opc, vn0, vn1, vn2);
	} else {
	  vn0 = _valnum_expr(cr->Opnd(0));
	  vn1 = _valnum_expr(cr->Opnd(1));
	  vn_expr_ptr = VN_EXPR::Create_Binary(opc, vn0, vn1);
	}
	break;
	 
      case 3:
	 vn0 = _valnum_expr(cr->Opnd(0));
	 vn1 = _valnum_expr(cr->Opnd(1));
	 vn2 = _valnum_expr(cr->Opnd(2));
	 vn_expr_ptr = VN_EXPR::Create_Ternary(opc, vn0, vn1, vn2);
	 break;
	 
      default:
	 FmtAssert(FALSE, ("Unexpected number of kids in VN::_valnum_op()"));
	 break;

      } // switch (no of kids)

      valnum = _valnum_vn_expr(exprid, vn_expr_ptr, 
			       _exprid_to_vn, *_status.locked_to_vn);
   }
   return valnum;
} // VN::_valnum_op


VN_VALNUM 
VN::_valnum_expr(CODEREP *cr)
{
   // Convert the cr to a VN_EXPR, and call _valnum_vn_expr.  Any IVAR
   // or VAR encountered here must denote a load expression.  We assume
   // the lhs of store statements are handled specially elsewhere (see
   // _valnum_stmt()).
   // 
   VN_VALNUM      valnum;
   const EXPRID   exprid = _get_exprid(cr);
   const CODEKIND expr_kind = cr->Kind();
   BOOL           is_parm = FALSE;

   if ((*_status.is_numbered)[exprid]) // Already numbered in this iteration?
      valnum = _exprid_to_vn[exprid];
   else
   {
      VN_EXPR::PTR vn_expr_ptr;

      // Create the VN_EXPR::PTR.
      //
      switch (cr->Kind())
      {
      case CK_LDA:
	 // No longer use [cr->Lda_aux_id(), cr->Offset()] to denote
	 // Lda_Addr expression, since it turns out this is not sufficient
	 // for LDAs at different stages of lowering.
	 //
	 vn_expr_ptr = VN_EXPR::Create_Lda_Addr((INT32)exprid);
	 valnum = _valnum_vn_expr(exprid,
				  vn_expr_ptr,
				  _exprid_to_vn,
				  *_status.locked_to_vn);
	 break;

      case CK_CONST:
         // Constant vector, no matter its element type is int or fp, is 
         // depicted by a CR of type CK_RCONST. 
         //
         Is_True (!MTYPE_is_vector (cr->Dtyp()),
                  ("vect cannot be described by a CR_CONST"));
         valnum = _valnum_integer(cr->Const_val(), 
                                  MTYPE_is_signed(cr->Dtyp()));
         _set_valnum(exprid, valnum, _exprid_to_vn, *_status.locked_to_vn);
         break;

      case CK_RCONST:
         {
         TYPE_ID vect_ty = MTYPE_is_vector(cr->Dtyp()) ? 
                             cr->Dtyp() : MTYPE_UNKNOWN;
         vn_expr_ptr = VN_EXPR::Create_Literal(STC_val(cr->Const_id()), vect_ty);
         valnum = _valnum_vn_expr(exprid, vn_expr_ptr,
                                  _exprid_to_vn,
                                  *_status.locked_to_vn);
         }
         break;

      case CK_IVAR:
	 if (cr->Opr() == OPR_PARM)
	 {
	    // A special case.  The actual parameter is in the Ilod_base().
	    // Assign the same value number to this "wrapper" as was
	    // assigned to the actual parameter.
	    //
	    is_parm = TRUE;
	    valnum = _valnum_expr(cr->Ilod_base());
	    if (cr->Is_ivar_volatile())
	       _set_valnum(exprid, VN_VALNUM::Bottom(), 
			   _exprid_to_vn,
			   *_status.locked_to_vn);
	    else
	       _set_valnum(exprid, valnum,  
			   _exprid_to_vn,
			   *_status.locked_to_vn);
	 }
	 else
	    valnum = _valnum_memloc_load(cr);
	 break;

      case CK_OP:
	 valnum = _valnum_op(cr);
	 break;

      case CK_VAR:
	 // We should never reach this case for Phi operands, which is
	 // the only case where optimistic assumptions come into play;
	 // hence we change any Top() into a Bottom().  A Top() value
	 // may be encountered here for any variable without a definition;
	 // these will be either volatile or dedicated register variables.
	 //
	 valnum = _valnum_sym(cr);
	 if (valnum.is_top())
	    valnum = VN_VALNUM::Bottom();
	 break;
	 
      case CK_DELETED:
      default:
	 FmtAssert(FALSE, ("Unexpected CODEREP kind in VN::_valnum_expr()"));
      break;
      } // switch (coderep kind)
   }

   // Set the CODEREP==>STMTREP mapping, before returning the value number,
   // Note that this is the right place to do this, since we should only
   // include CODEREPs that do not represent the lhs of STMTREPs in this
   // mapping (although we do include subexpressions of such a lhs).
   //
   if (!is_parm)
      _set_stmt_map(cr->Kind(), exprid, valnum);

   if (Vn_Tracing(exprid))
      _trace(exprid, valnum);
   
   return valnum;
} // VN::_valnum_expr


VN_VALNUM
VN::_valnum_implicit_integral_cvt(EXPRID         exprid,
				  VN_VALNUM      opnd_valnum,
				  MTYPE          from_mty,
				  MTYPE          to_mty,
				  VALNUM_VECTOR &exprid_to_vn,
				  BIT_VECTOR    &locked_to_vn)
{
   // We have an implicit size conversion, so create an artificial CVT
   // or CVTL operation, which should return the appropriate value number
   // of the converted value.
   //
   VN_EXPR::PTR expr = NULL;
   OPCODE       opc;
   INT          need_cvt = Need_Integral_Conversion(from_mty, to_mty, &opc);

   Is_True(!opnd_valnum.is_top() && !opnd_valnum.is_bottom(),
	   ("Unexpected T or _|_ in VN::_valnum_implicit_integral_cvt"));
   Is_True(need_cvt != NOT_AT_ALL,
	   ("Unexpected types in VN::_valnum_implicit_integral_cvt")); 
   
   // Get the value-numbering expression for the convert operation.
   //
   if (need_cvt == NEED_CVT)
   {
      expr = VN_EXPR::Create_Unary(opc, opnd_valnum);
   }
   else if (need_cvt == NEED_CVTL)
   {
      Is_True(OPCODE_operator(opc) == OPR_CVTL,
	   ("Unexpected conversion in VN::_valnum_implicit_integral_cvt"));

      const VN_VALNUM bitsize_valnum = _valnum_integer(MTYPE_bit_size(to_mty),
						       CVTL_BITSIZE_IS_SIGNED);

      expr = VN_EXPR::Create_Binary(opc, opnd_valnum, bitsize_valnum);
   }

   // Value number the new converted expression, using the given maps
   // to determine locks on value numbers (should not use the _status
   // maps, since this may be an implicit convert denoted as a second 
   // value number for an EXPRID).
   //
   return _valnum_vn_expr(exprid, expr, exprid_to_vn, locked_to_vn);
} // VN::_valnum_implicit_integral_cvt


VN_VALNUM
VN::_valnum_lhs(EXPRID      lhs_exprid,
		VN_VALNUM   valnum,
		MTYPE       lhs_dty,
		MTYPE       lhs_dscty,
		MTYPE       rhs_dty)
{
   // The _vn_to_expr[valnum] map is already set, based on the rhs
   // processing.  All we need to do here is map the lhs EXPRID to the
   // same value number as the rhs EXPRID.  We also need to specially
   // handle implicit CVTs and redefine the value number accordingly.
   //
   const BOOL is_multi_scalar_mstore = (lhs_dscty == MTYPE_M &&
					rhs_dty != MTYPE_M);
   BOOL do_cvt1 = 
      Need_Integral_Conversion(rhs_dty, lhs_dscty, NULL) != NOT_AT_ALL;
#ifdef KEY // bug 11738: honor truncation effect of store
   if (! do_cvt1 &&
       MTYPE_is_integral(lhs_dscty) &&
       MTYPE_byte_size(lhs_dscty) <= 4) {
     if (_vn_to_expr[valnum] != NULL &&
         _vn_to_expr[valnum]->get_kind() == VN_EXPR::LITERAL) {
       TCON tcon = _vn_to_expr[valnum]->get_tcon();
       if (MTYPE_signed(lhs_dscty)) {
         INT64 v = Targ_To_Host(tcon);
         if (((v << 32) >> 32) != Targ_To_Host(tcon)) {
           do_cvt1 = TRUE;
           rhs_dty = TCON_ty(tcon);
         }
       }
       else {
         UINT64 v = Targ_To_Host(tcon);
         if (((v << 32) >> 32) != (UINT64) Targ_To_Host(tcon)) {
           do_cvt1 = TRUE;
           rhs_dty = TCON_ty(tcon);
         }
       }
     }
   }
#endif

   const BOOL do_cvt2 = 
      Need_Integral_Conversion(lhs_dscty, lhs_dty, NULL) != NOT_AT_ALL;
   
   Is_True(!valnum.is_top(), ("Unexpected Top() valnum in VN::_valnum_lhs"));

   if (is_multi_scalar_mstore || valnum.is_bottom())
   {
      // Invent a unique value number for the lhs variable;
      // _vn_to_expr[valnum] is NULL by default
      //
      valnum = _unique_valnum(lhs_exprid, 
			      _exprid_to_vn,
			      *_status.locked_to_vn);
      _set_valnum(lhs_exprid,
		  valnum,
		  _exprid_to_vn,
		  *_status.locked_to_vn);
   }
   else if (do_cvt1 && do_cvt2)
   {
      valnum = _valnum_implicit_integral_cvt(lhs_exprid, valnum, 
					     rhs_dty, lhs_dscty,
					     *_status.exprid_to_vn2,
					     *_status.locked_to_vn2);

      (*_status.is_numbered)[lhs_exprid] = FALSE;
      valnum = _valnum_implicit_integral_cvt(lhs_exprid, valnum,
					     lhs_dscty, lhs_dty,
					     _exprid_to_vn,
					     *_status.locked_to_vn);
   }
   else if (do_cvt1)
   {
      valnum = _valnum_implicit_integral_cvt(lhs_exprid, valnum, 
					     rhs_dty, lhs_dscty,
					     _exprid_to_vn,
					     *_status.locked_to_vn);
   }
   else if (do_cvt2)
   {
      valnum = _valnum_implicit_integral_cvt(lhs_exprid, valnum,
					     lhs_dscty, lhs_dty,
					     _exprid_to_vn,
					     *_status.locked_to_vn);
   }
   else
   {
      _set_valnum(lhs_exprid, valnum, 
		  _exprid_to_vn,
		  *_status.locked_to_vn);
   }
   return valnum;
} // VN::_valnum_lhs


VN_VALNUM
VN::_valnum_memloc_load(CODEREP *cr)
{
   const OPERATOR  opr         = cr->Opr();
         MTYPE     dtype       = cr->Dtyp();
         MTYPE     dsctype     = cr->Dsctyp();
   const WN_OFFSET offset_val  = cr->Offset();
   CODEREP *const  vsym        = cr->Get_ivar_vsym();
   const VN_VALNUM vsym_valnum = _valnum_sym(vsym);
   const VN_VALNUM base_addr   = _valnum_expr(cr->Ilod_base());

   // Get the "bytesize" and "offset".
   //
   VN_VALNUM bytesize;
   VN_VALNUM offset;

   switch (opr)
   {
   case OPR_ILOAD:
   case OPR_ILDBITS:
   case OPR_PREFETCH:
      bytesize = _valnum_integer(MTYPE_byte_size(dsctype), TRUE);
      offset = _valnum_integer(offset_val, TRUE);
      break;

   case OPR_MLOAD:
      dsctype = MTYPE_M; // Why is cr->Dsctyp() MTYPE_V?
      bytesize = _valnum_expr(cr->Mload_size());
      offset = _valnum_integer(offset_val, TRUE);
      break;

   case OPR_ILOADX:
      bytesize = _valnum_integer(MTYPE_byte_size(dsctype), TRUE);
      offset = _valnum_expr(cr->Index());
      break;

   default:
      FmtAssert(FALSE, 
		("Unexpected opcode for ivar in VN::_valnum_memloc_load()"));
      break;
   }
   
   // Create the memory location expression and process it using 
   // _valnum_vn_expr(), which means it will get an existing valnum
   // if a store or another load to the same memory location has been 
   // processed, and a new unique value number otherwise.
   //
   if (cr->Is_ivar_volatile() || opr == OPR_ILDBITS)
   {
      // This should not match other occurrences of itself, so a unique
      // value number will not work.  Assign it Bottom() value number, such
      // that each occurrence is viewed as unique.
      //
      // TODO: do not give up on ILDBITS 
      _set_valnum(_get_exprid(cr), VN_VALNUM::Bottom(),
		  _exprid_to_vn,
		  *_status.locked_to_vn);
      return VN_VALNUM::Bottom();
   }
   else
   {
      VN_VALNUM          valnum;
      const EXPRID       exprid = _get_exprid(cr);
      const VN_EXPR::PTR vn_expr_ptr = 
	 VN_EXPR::Create_Memloc(dsctype, 
				bytesize, offset, base_addr, vsym_valnum);

      // See if we have an implicit conversion of value size
      //
      if (Need_Integral_Conversion(dsctype, dtype, NULL) != NOT_AT_ALL)
      {
	 // First find the value number of the T1T1ILOAD, where T1=dsctype.
	 // This is an implicit value, since the T1T1ILOAD does not really
	 // exist in the program.
	 //
	 valnum = _valnum_vn_expr(exprid, vn_expr_ptr,
				  *_status.exprid_to_vn2,
				  *_status.locked_to_vn2);

	 // Value number the actual ILOAD, taking the conversion into
	 // account.
	 //
	 (*_status.is_numbered)[exprid] = FALSE;
	 valnum = _valnum_implicit_integral_cvt(exprid,
						valnum,
						dsctype,
						dtype,
						_exprid_to_vn,
						*_status.locked_to_vn);
      }
      else
      {
	 valnum = _valnum_vn_expr(exprid, vn_expr_ptr,
				  _exprid_to_vn,
				  *_status.locked_to_vn);
      }
      
      // Set the CODEREP==>STMTREP mapping, before returning the value number.
      //
      if (!valnum.is_bottom())
	 _set_stmt_map(cr->Kind(), exprid, valnum);

      return valnum;
   }
} // VN::_valnum_memloc_load


void 
VN::_valnum_memloc_store(CODEREP   *lhs,
			 VN_VALNUM  rhs_valnum,
			 MTYPE      rhs_mtype)
{
   // The lhs must be an ivar, and we expect the memory location to
   // always hash to a unique location in the hash-table, where the
   // hash-table will map the MEMLOC EXPR_VN to the given "valnum".
   // This is the only case when the hash-table may map two different
   // VN_EXPRs to the same VN_VALNUM, which is why we separate this
   // case out from _valnum_expr().  Note that _valnum_expr() works 
   // fine for a MEMLOC EXPR_VN representative of an indirect load.
   //
   // The store is represented uniquely by the offset, the base address
   // expression and the "owner" vsym.  The owner vsym may be NULL (only
   // assigned to, never referenced), i.e. the chi node that holds it is
   // dead, in which case we need to 
   //
   // TODO: Try to fold constant expressions within the base together
   // with the offset to catch stuff like p+1+2 vs p+3 after value
   // numbering.  Handle this in VN_EXPR::simplify()!!
   //
   Is_True(lhs->Kind() == CK_IVAR &&
	   !(*_status.is_numbered)[_get_exprid(lhs)], 
	   ("Unexpected CODEREP in VN::_valnum_memloc_store()"));

   const OPCODE    opc         = lhs->Op();
   MTYPE           dtype       = lhs->Dtyp();
   MTYPE           dsctype     = lhs->Dsctyp();
   const WN_OFFSET offset_val  = lhs->Offset();
   CODEREP *const  vsym        = lhs->Get_ivar_vsym();
   const VN_VALNUM base_addr   = _valnum_expr(lhs->Istr_base());

   // Get the "bytesize" and "offset".
   //
   VN_VALNUM bytesize;
   VN_VALNUM offset;

   switch (OPCODE_operator(opc))
   {
   case OPR_ILOAD:
      bytesize = _valnum_integer(MTYPE_byte_size(dsctype), TRUE);
      offset = _valnum_integer(offset_val, TRUE);
      break;

   case OPR_MLOAD:
      dsctype = MTYPE_M; // Why is cr->Dsctyp() MTYPE_V?
      bytesize = _valnum_expr(lhs->Mstore_size());
      offset = _valnum_integer(offset_val, TRUE);
      break;

   case OPR_ILOADX:
      bytesize = _valnum_integer(MTYPE_byte_size(dsctype), TRUE);
      offset = _valnum_expr(lhs->Index());
      break;

   default:
      FmtAssert(FALSE, 
		("Unexpected opcode for ivar in VN::_valnum_memloc_store()"));
      break;
   }
   
   if (lhs->Is_ivar_volatile())
   {
      // This should not match other occurrences of itself, so a unique
      // value number will not work.  Assign it Bottom() value number, such
      // that each occurrence is viewed as unique.
      //
      _set_valnum(_get_exprid(lhs), VN_VALNUM::Bottom(),
		  _exprid_to_vn,
		  *_status.locked_to_vn);
   }
   else
   {
      VN_VALNUM lhs_valnum = _valnum_lhs(_get_exprid(lhs),
					 rhs_valnum,
					 dtype, 
					 dsctype, 
					 rhs_mtype);

      // Create the memory location expression.
      //
      const VN_VALNUM vsym_valnum = _valnum_sym(vsym);
      const VN_EXPR::PTR expr = 
	 VN_EXPR::Create_Memloc(dsctype, 
				bytesize, offset, base_addr, vsym_valnum);
      VN_EXPR::PTR simplified = expr->simplify(this);
      
      // If the memory location is well defined, then enter it into the hash
      // table and assign it the value number of the lhs; otherwise do not
      // enter it into the hash-table, such that subsequent identical
      // ("undefined") MEMLOC loads will get unique value numbers.
      //
      if (!simplified->has_bottom_opnd())
      {
	 VN_HASHTAB::EXPR_MAPPING map = 
	    _status.expr_to_vn->lookup_or_insert(simplified, lhs_valnum);
	 Is_True(simplified == map.first,
		 ("Unexpected match found for CK_IVAR in "
		  "_valnum_memloc_store"));
      }
      else
      {
	 simplified->free();
      }
   }
} // VN::_valnum_memloc_store


void
VN::_valnum_phi_list(IDTYPE    bb_id,
		     PHI_LIST *phi_list)
{
   // Get the value-number of "phi(VN[op0], VN[op1], ... , VN[opn])",
   // and associate the resultant value number with phi->RESULT().
   // Note that we do *not* call _valnum_vn_expr() for the phi operands,
   // since some of them may be undefined (Bottom or Top) and we do not
   // wish to invent new value numbers when this is the case.
   //
   PHI_NODE     *phi;
   PHI_LIST_ITER phi_iter;

   FOR_ALL_ELEM(phi, phi_iter, Init(phi_list))
   {
      if (phi->Live())
      {
	 CODEREP * const phi_result = phi->RESULT();
	 const EXPRID    phi_id     = _get_exprid(phi_result);

	 Is_True(phi_result->Kind() == CK_VAR, 
		 ("Unexpected kind of phi result in VN::_valnum_phi_list"));
	 
	 if ((*_status.is_numbered)[phi_id])
	 {
	    // Do nothing (e.g. for zero versions)
	 }
	 else if (phi_result->Is_flag_set(CF_IS_ZERO_VERSION))
	 {
	    // _vn_to_expr[valnum] is NULL by default
	    //
	    _set_valnum(phi_id, 
			VN_VALNUM::Bottom(),
			_exprid_to_vn,
			*_status.locked_to_vn);
	 }
	 else
	 {
	    VN_EXPR::PTR phi_expr = VN_EXPR::Create_Phi(phi->Size(), bb_id);
	 
	    for (INT i = 0; i < phi->Size(); i++)
	    {
	       CODEREP * const phi_opnd = phi->OPND(i);

	       if (phi_opnd != NULL &&
		   Need_Integral_Conversion(phi_opnd->Dtyp(),     // From mty
					    phi_result->Dsctyp(), // To mty
					    NULL) != NOT_AT_ALL)
	       {
		  // We cannot deal with type conversions from phi operands
		  // to the descriptor type of the phi result, so for such
		  // cases we conservatively set the operand to be "bottom"
		  // to ensure that the PHI expression gets a unique value 
		  // number.
		  //
		  phi_expr->set_opnd(i, VN_VALNUM::Bottom());
	       }
	       else
	       {
		  phi_expr->set_opnd(i, _valnum_sym(phi_opnd));
	       }
	    }

	    const MTYPE dtype = phi_result->Dtyp();
	    const MTYPE dsctype = phi_result->Dsctyp();
	    
	    if (Need_Integral_Conversion(dsctype, dtype, NULL) != NOT_AT_ALL)
	    {
	       // First find the value number of the T1T1STID, where 
	       // T1=dsctype.  This is an implicit value, since the 
	       // T1T1STID does not really exist in the program.
	       //
	       VN_VALNUM valnum = _valnum_vn_expr(phi_id, phi_expr,
						  *_status.exprid_to_vn2,
						  *_status.locked_to_vn2);

	       // Value number the actual phi result, taking the conversion
	       // into account.
	       //
	       (*_status.is_numbered)[phi_id] = FALSE;
	       (void) _valnum_implicit_integral_cvt(phi_id,
						    valnum,
						    dsctype,
						    dtype,
						    _exprid_to_vn,
						    *_status.locked_to_vn);
	    }
	    else
	    {
	       (void)_valnum_vn_expr(phi_id, phi_expr,
				     _exprid_to_vn,
				     *_status.locked_to_vn);
	    }
	    
	    if (Vn_Tracing(phi_id))
	       _trace(phi_id, _exprid_to_vn[phi_id]);
	 }
      }
   }
} // VN::_valnum_phi_list


void
VN::_valnum_chi_list(CHI_LIST *chi_list)
{
   // Walk through the Chi list and assigne a unique value number to each
   // live "may alias" CODEREP result.
   //
   CHI_LIST_ITER chi_iter;
   CHI_NODE     *chi;
   FOR_ALL_NODE(chi, chi_iter, Init(chi_list))
   {
      if (chi->Live())
      {
	 CODEREP  *chi_result = chi->RESULT();
	 EXPRID   chi_id = _get_exprid(chi_result);

	 Is_True(chi_result->Kind() == CK_VAR, 
		 ("Unexpected coderep kind in VN::_valnum_chi_list()"));

	 if ((*_status.is_numbered)[chi_id])
	 {
	    // Do nothing (e.g. for zero versions)!
	 }
	 else if (chi_result->Is_var_volatile() ||
		  chi_result->Is_flag_set(CF_IS_ZERO_VERSION))
	 {
	    // _vn_to_expr[VN_VALNUM::Bottom()] is NULL by default
	    _set_valnum(chi_id, 
			VN_VALNUM::Bottom(),
			_exprid_to_vn,
			*_status.locked_to_vn);
	 }
	 else
	 {
	    VN_VALNUM valnum = _unique_valnum(chi_id,
					      _exprid_to_vn,
					      *_status.locked_to_vn);

	    // _vn_to_expr[valnum] is NULL by default
	    _set_valnum(chi_id,
			valnum,
			_exprid_to_vn,
			*_status.locked_to_vn);
	 }
      }
   }
} // VN::_valnum_chi_list


void
VN::_valnum_stmt(STMTREP *stmt)
{
   CODEREP      *rhs = stmt->Rhs();
   CODEREP      *lhs = stmt->Lhs();
   VN_VALNUM     vn;

   // We always have to create unique value-numbers for the entries in
   // Chi lists.
   //
   if (stmt->Has_chi())
   {
       _valnum_chi_list(stmt->Chi_list());
   }

   // Value number all statements that are not black boxes.  For stores
   // the general algorithm is to calculate the value number for the
   // rhs, and then set the value-number for the lhs, be it a "var" or
   // an "ivar" CODEREP, to be the same.  For calls we just walk through
   // the parameter list and value number them independently, without
   // value-numbering the call CODEREP itself.  Note that PARM nodes will
   // get the same value number as their kid.
   //
   _current_stmt = stmt;
   if (OPCODE_is_fake(stmt->Op()))
   {
      // _vn_to_expr[valnum] is NULL by default
      _set_valnum(_get_exprid(rhs), 
		  VN_VALNUM::Bottom(),
		  _exprid_to_vn,
		  *_status.locked_to_vn);

      for (INT32 i = 0; i < rhs->Kid_count(); i++) 
      {
	 _valnum_expr(rhs->Opnd(i));
      }
   }
   else if (!stmt->Black_box())
   {
      switch (OPCODE_operator(stmt->Op()))
      {
      case OPR_STID:
	 Is_True(lhs->Kind() == CK_VAR && 
		 !(*_status.is_numbered)[_get_exprid(lhs)], 
		 ("Unexpected opc for lhs of STID in VN::_valnum_stmt"));

	 // We do not expect to see zero versions other than in PHIs, MUs,
	 // and CHIs.
	 //
	 Is_True(!lhs->Is_flag_set(CF_IS_ZERO_VERSION),
		 ("Unexpected 0 version as lhs of stid in VN::_valnum_stmt"));

	 // rhs
	 //
	 vn = _valnum_expr(rhs);

	 // lhs
	 //
	 if (lhs->Is_var_volatile())
	 {
	    // This should not match other occurrences of itself, so a unique
	    // value number will not work.  Assign it Bottom() value number,
	    // such that each occurrence is viewed as unique.
	    //
	    _set_valnum(_get_exprid(lhs), VN_VALNUM::Bottom(),
			_exprid_to_vn,
			*_status.locked_to_vn);
	    vn = VN_VALNUM::Bottom();
	 }
	 else
	 {
	    vn = _valnum_lhs(_get_exprid(lhs), vn, 
			     lhs->Dtyp(), lhs->Dsctyp(), rhs->Dtyp());
	 }
	 break;

      case OPR_ISTORE:
      case OPR_ISTBITS:
      case OPR_MSTORE:
      case OPR_ISTOREX:
	 Is_True(lhs->Kind() == CK_IVAR &&  
		 !(*_status.is_numbered)[_get_exprid(lhs)], 
		 ("Unexpected opc for lhs of ISTORE in VN::_valnum_stmt"));

	 vn = _valnum_expr(rhs);
	 _valnum_memloc_store(lhs, vn, rhs->Dtyp());
	 break;

      case OPR_PREFETCH:
	 Is_True(rhs->Kind() == CK_IVAR && 
		 !(*_status.is_numbered)[_get_exprid(rhs)],
		 ("Unexpected opc for lhs of PREFETCH in VN::_valnum_stmt"));

	 _valnum_memloc_load(rhs);
	 break;

      default:
	 if (lhs)
	    _valnum_expr(lhs);
	 if (rhs)
	    _valnum_expr(rhs);
	 break;

      } // switch
   } // if not black box
   _current_stmt = NULL;
} // VN::_valnum_stmt


void
VN::_valnum_cfg(CFG *cfg)
{
   // This algorithm for value numbering traverses the CFG in reverse
   // postorder, value-numbering each basic block (BB) before its 
   // successors such that all inputs to phi nodes, except those flowing
   // along back-edges (e.g. in loop structures) will be processed before
   // the dependent phi nodes are processed.
   //
   BB_NODE   *bb;
   // DPOBB_ITER cfg_iter(cfg, TRUE); // pre-order traversal of dominator tree
   RPOBB_ITER cfg_iter(cfg);          // reverse postorder traversal of cfg

   FOR_ALL_ELEM(bb, cfg_iter, Init())
   {
      _valnum_phi_list(bb->Id(), bb->Phi_list());
      
      STMTREP     *stmt;
      STMTREP_ITER stmt_iter(bb->Stmtlist());
      FOR_ALL_NODE(stmt, stmt_iter, Init())
      {
	 _valnum_stmt(stmt);
      }
   }
} // VN::_valnum_cfg


void 
VN::_print_exprid_to_vn(FILE *fp, EXPRID id, INT32 column_width) const
{
   // The maximum column width is 63.  A column width of <= size_of_output
   // will be ignored (use zero width to avoid any extra space characters.
   // We use optimistic Top values during value numbering, and some may
   // survive (e.g. for zero versions

   //
   INT32           buf_idx;
   char            buf[64];
   const VN_VALNUM valnum = _exprid_to_vn[id];
      
   Is_True(column_width < sizeof(buf), 
	   ("Too large column width in VN::_print_exprid_to_vn"));

   buf_idx = sprintf(buf, "cr%d ==> ", id);
   if (valnum.is_top())
      buf_idx += VN_VALNUM::Bottom().sprint(&buf[buf_idx]);
   else
      buf_idx += valnum.sprint(&buf[buf_idx]);
      
   while (buf_idx < column_width) buf[buf_idx++] = ' ';
   buf[buf_idx] = '\0';

   fputs(buf, fp);
} // _print_exprid_to_vn


void 
VN::_print_vn_to_exprid(FILE                      *fp, 
			const VALNUM_TO_EXPR_LIST &vn_to_exprid,
			VN_VALNUM                  valnum) const
{
   // Write out a mapping from a value number (and an associated value
   // number expression), to a sequence of coderep id numbers.
   //
   VALNUM_TO_EXPR_LIST::EXPR_ITERATOR itr = vn_to_exprid.begin(valnum);
   VALNUM_TO_EXPR_LIST::EXPR_ITERATOR end = vn_to_exprid.end(valnum);

   // Print out value number, and the value number expression associated
   // with that value number.
   //
   valnum.print(fp);
   if (_vn_to_expr[valnum] == NULL)
   {
      fputs(" ==> ...Chi result, or has Bottom opnd", fp);
   }
   else
   {
      fputs(" ==> ", fp);
      _vn_to_expr[valnum]->print(fp);
   }

   // Print out coderep sequence (if any) that has been assigned this
   // value number.
   //
   if (itr != end)
   {
      fputs(" ==> {", fp);
      for (BOOL first_cr = TRUE; itr != end; itr++)
      {
	 if (!first_cr)
	    fputc(',', fp);
	 else
	    first_cr = FALSE;
		  
	 fprintf(fp, "cr%d", (INT32)*itr);
      }
      fputs("}\n", fp);
   }
   else
   {
      fputc('\n', fp); // Empty sequence
   }
} // VN::_print_vn_to_exprid


VN::VN(VN_ALGORITHM algo, 
       CFG         *cfg, 
       CODEMAP     *codemap, 
       MEM_POOL    *lpool, 
       MEM_POOL    *gpool):
   _lpool(lpool),
   _gpool(gpool),
   _current_stmt(NULL),
   _no_of_iterations(0),
   _zero_valnum(VN_VALNUM::Bottom()),
   _next_valnum(VN_VALNUM::First()),
   _vn_to_expr(gpool),
   _exprid_to_cr(codemap->Coderep_id_cnt(), 
		 (CODEREP *) NULL, 
		 CODEREP_VECTOR::allocator_type(gpool)),
   _exprid_to_vn(codemap->Coderep_id_cnt(), 
		 Initial_Valnum(algo), 
		 VALNUM_VECTOR::allocator_type(gpool)),
   _exprid_to_stmtlist(codemap->Coderep_id_cnt(),
		       STMT_LIST(STMT_ALLOCATOR(gpool)),
		       STMTLIST_VECTOR::allocator_type(gpool))
{
   // The cfg must be in SSA form and the codemap must be have been set
   // up as prerequisites to this constructor being called.  The 
   // locked_to_vn flag is used across iterations, while the is_numbered
   // flag is reset between iterations and indicates whether or not a value
   // number has already been assigned for this coderep on this iteration.  
   // The only reason we need the is_numbered flag is to take advantage of
   // the fact that when two EXPRIDs are the same they MUST have the
   // same value number; i.e. if a value number for a CODEREP has already 
   // been determined, then there is no need to recalculate it (see 
   // _valnum_expr() for how this is done).
   //
   Is_True(lpool == Malloc_Mem_Pool || lpool != gpool,
	   ("Value numbering (VN::VN) requires two different memory pools"));
   
   // All local data used as part of this algorithm will be allocated in
   // the local memory pool, and will be popped of the mempool once the
   // this VN object has been fully constructed.
   //
   OPT_POOL_Push(_lpool, -1);
   {
      VN_HASHTAB expr_to_vn(codemap->Coderep_id_cnt(),
			    _lpool); // VN_EXPR -> VN_VALNUM mapping
      BIT_VECTOR locked_to_vn(codemap->Coderep_id_cnt(), 
			      bool(FALSE), 
			      BVECTOR_ALLOCATOR(_lpool));
      BIT_VECTOR is_numbered(codemap->Coderep_id_cnt(),
			     bool(FALSE), 
			     BVECTOR_ALLOCATOR(_lpool));

      // For implicit ops we need to associate up to two value numbers
      // with each coderep.  The following maps are needed to ensure 
      // termination of the iterative algorithm.
      //
      BIT_VECTOR    locked_to_vn2(codemap->Coderep_id_cnt(), 
				  bool(FALSE), 
				  BVECTOR_ALLOCATOR(_lpool));
      VALNUM_VECTOR exprid_to_vn2(codemap->Coderep_id_cnt(), 
				  Initial_Valnum(algo), 
				  VALNUM_VECTOR::allocator_type(gpool));

      _status.expr_to_vn = &expr_to_vn;      // Used by VN_EXPR::simplify()
      _status.is_numbered = &is_numbered;    // True when exprid has a valnum
      _status.locked_to_vn = &locked_to_vn;  // True when exprid owns of valnum
      _status.locked_to_vn2 = &locked_to_vn2;// True when id owns second valnum
      _status.exprid_to_vn2 = &exprid_to_vn2;// Maps id to second valnum

      // Predefine value numbers for small integrals.
      //
      _init_integer_valnum_map();

      // Apply the selected value numbering algorithm.
      //
      do
      {
	 _status.changed = FALSE;
	 _valnum_cfg(cfg);
	 expr_to_vn.clear();  // Clear the hash-table 
	 
	 // Delete the vector elements, then reinitialize them to be false.
	 //
	 is_numbered.clear();
	 is_numbered.insert(is_numbered.end(), 
			    is_numbered.capacity(),
			    bool(FALSE));

	 // Free up all VN_EXPR objects, except the predefined integers
	 // and the other literal integral values encountered.  This has an
	 // effect similar to locking integral valued expressions to
	 // imaginary EXPR_IDs (they should never be locked_to_vn[] for
	 // any real EXPR_ID), and ensures that the same integral value
	 // will always have the same value number across iterations.
	 //
	 if (_status.changed && algo == ITERATIVE)
	 {
	    // Skip predefined integers.
	    //
	    VN_VALNUM valnum = 
	       VN_VALNUM::Next(_get_literal_valnum(VN_MAX_PREDEFINED_INT));
	    
	    // For each value number assigned thus far, keep integral
	    // literal expressions and free all other ones.
	    //
	    while (valnum <= _vn_to_expr.last())
	    {
	       VN_EXPR::PTR vn_expr = _vn_to_expr[valnum];

	       if (vn_expr != NULL)
	       {
		  if (vn_expr->get_kind() == VN_EXPR::LITERAL)
		     expr_to_vn.lookup_or_insert(vn_expr, valnum); // Reinsert
		  else
		  {
		     _vn_to_expr.set_map(valnum, NULL);           // Remove
		     vn_expr->free();
		  }
	       }
	       valnum = VN_VALNUM::Next(valnum);
	    } // while more value-numbers
	 } // free up expressions for another value-numbering iteration
	 _no_of_iterations++;
      } while (_status.changed && algo == ITERATIVE);

      _status.expr_to_vn = NULL;    // Used by VN_EXPR::simplify()
      _status.is_numbered = NULL;   // True when exprid has a valnum
      _status.locked_to_vn = NULL;  // True when exprid owns of valnum
      _status.locked_to_vn2 = NULL; // True when id owns second valnum
      _status.exprid_to_vn2 = NULL; // Maps id to second valnum
      _status.changed = FALSE;
   }
   OPT_POOL_Pop(_lpool, -1);
} // VN::VN


VN::~VN()
{
   _vn_to_expr.reset_all_exprs();
} // VN::~VN


void
VN::print(FILE *fp, BOOL emit_stmt_maps) const
{
   INT32                    i;
   VALNUM_VECTOR::size_type no_of_codereps = _exprid_to_vn.size();

   fprintf(fp, "%sGLOBAL VALUE NUMBERING (iterations = %d)\n%s",
	   DBar, _no_of_iterations, DBar);

   // Write out the coderep --> valnum mapping in three columns
   //
   const INT32 start_col1 = 0;
   const INT32 start_col2 = (no_of_codereps + 2)/3;
   const INT32 start_col3 = 2*(no_of_codereps + 2)/3;
   
   for (i = 0; i < start_col2; i++)
   {
      _print_exprid_to_vn(fp, i+start_col1, 20 /*char wide column*/);
      if (i+start_col2 < no_of_codereps)
	 _print_exprid_to_vn(fp, i+start_col2, 20 /*char wide column*/);
      if (i+start_col3 < no_of_codereps)
	 _print_exprid_to_vn(fp, i+start_col3, 20 /*char wide column*/);
      fputc('\n', fp);
   }
   fputc('\n', fp);

   // Write out the valnum --> coderep mapping
   //
   // All local data used as part of this algorithm will be allocated in
   // the local memory pool, and will be popped of the mempool once the
   // output has been produced.
   //
   OPT_POOL_Push(_lpool, -1);
   {
      const VALNUM_TO_EXPR_LIST vn_to_exprid(*this, _lpool);
      char                      buf0[16];
      char                      buf1[16];
      const VN_VALNUM           last = last_valnum();
      const VN_VALNUM           last_predefined_int =
	 _get_literal_valnum(VN_MAX_PREDEFINED_INT);
   
      // Write out a note about the predefined integers and their mappings.
      //
      _zero_valnum.sprint(buf0);
      last_predefined_int.sprint(buf1);
      fprintf(fp,
	      "NOTE: Integers 0..%d are mapped to %s to %s respectively!\n\n",
	      VN_MAX_PREDEFINED_INT, buf0, buf1);
      
      // Write out the mapping for all value numbers, except predefined value
      // numbers not associated with any codereps.
      //
      for (VN_VALNUM next = VN_VALNUM::First(); 
	   next <= last;
	   next = VN_VALNUM::Next(next))
      {
	 if (next < _zero_valnum        || 
	     next > last_predefined_int ||
	     !vn_to_exprid.is_empty(next))
	 {
	    _print_vn_to_exprid(fp, vn_to_exprid, next);
	 }
      }
      fputc('\n', fp);
   }
   OPT_POOL_Pop(_lpool, -1);

   // Write out the coderep --> stmtrep mapping, only when explicitly
   // requested.
   //
   if (emit_stmt_maps)
   {
      for (i = 0; i < _exprid_to_stmtlist.size(); i++)
      {
	 const STMT_LIST::const_iterator stmt_begin = 
	    _exprid_to_stmtlist[i].begin();
	 const STMT_LIST::const_iterator stmt_end =
	    _exprid_to_stmtlist[i].end();

	 for (STMT_LIST::const_iterator stmt_it = stmt_begin; 
	      stmt_it != stmt_end;
	      stmt_it++)
	 {
	    if (stmt_it == stmt_begin)
	       fprintf(fp, "cr%d ==> {", i);
	    else
	       fputc(',', fp);

	    fprintf(fp, "SR %p", *stmt_it);
	 }
	 if (stmt_begin != stmt_end)
	    fputs("}\n", fp);
      }
      fputc('\n', fp);
   }
} // VN::print


VALNUM_TO_EXPR_LIST::VALNUM_TO_EXPR_LIST(const VN &vn, MEM_POOL *mpool):
   _map(vn.last_valnum().ordinal() + 1, 
	EXPR_LIST(EXPRID_ALLOCATOR(mpool)),
	EXPRLIST_ALLOCATOR(mpool))
{
   VN::EXPRID max_exprid = vn.last_exprid();

   for (INT32 i = 0; i <= max_exprid; i++)
   {
      const VN_VALNUM valnum = vn.expr_valnum(i);
      
      if (!valnum.is_top() && !valnum.is_bottom())
	 _map[valnum.ordinal()].push_front(i);
   }
} // VALNUM_TO_EXPR_LIST::VALNUM_TO_EXPR_LIST
