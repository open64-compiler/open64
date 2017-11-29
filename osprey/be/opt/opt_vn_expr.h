//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vn_expr.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vn_expr.h,v $
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
// INCLUSION DEPENDENCIES:
//
//   Before including this file, you must also include the following
//   files directly or indirectly:
//
//      common/com/defs.h
//      common/util/errors.h
//      common/com/targ_const.h
//
//
// INTERFACE DESCRIPTION:
//
//   This file defines three classes:
//
//       VN_VALNUM: Our value-number representation.  Value numbering
//          is an abstract program interpretation mechanism which will
//          use VN_VALNUM as its value domain.  We take two different
//          views of value numbers.  For hashing purposes we view two
//          value numbers as identical (operator '==') only if they are
//          truly exactly the same.
//
//          For VN_EXPR:simplify() purposes we view the range of value
//          numbers as a lattice, where a Bottom() value is different from
//          any value, even other bottom values), while a Top() value 
//          matches any other value except a Bottom() value.  Literals
//          match only identical literals.  This lattice view of equality 
//          is embodied by the "equivalent_to()" method.
//
//       VN_EXPR: Our representation of expressions formed from 
//          value numbers.  This is the domain of expressions over
//          which value numbering operates.  We provide mechanisms
//          for folding value number expressions into simpler ones
//          (fold method) and to hash them into a hash-table.  Note
//          that we have included a notion of memory locations (MEMLOC)
//          in our expression taxonomy, which can be viewed as a form of
//          lvalue expression that allows us to handle indirect loads 
//          and stores.
//
//       VN_EXPR_MAP: A mapping from each value number to its defining
//          value number expression.  Bottom() and Top() always map to
//          a NULL expression, and cannot be changed.  When a value number
//          is intrinsically defined (ENTRY_STMT or Chi node), then the
//          mapping should map the VN_VALNUM to a NULL expression.  Such 
//          use of NULL maps should obviate any need for a special kind 
//          of VN_EXPR for Chi nodes or ENTRY_STMT nodes.
//
//          This map automatically grows to accommodate any value number it
//          is set (set_map) for, so be careful to only define maps for 
//          valid value numbers.  If the NULL valued holes in the map turn
//          out to be very large, it may be better to use a more compact
//          representation for this map (TODO?).
//
//   Note that we only export methods to create mempool allocated
//   VN_EXPR objects.  This is because the VN_EXPR class really
//   represents a taxonomy of expression kinds (literal, unary, binary,
//   phi, etc.), and this taxonomy and associated memory management
//   is easiest maintained in terms of object references.  As a 
//   consequence, the client of a VN_EXPR can never create stack
//   allocated VN_EXPR objects or an array of allocated VN_EXPR objects,
//   only a stack or array of VN_EXPR object pointers.
//
//   From a client's perspective, a VN_EXPR should be viewed as a union
//   of the various kinds (VN_EXPR::KIND).  Some accessors are only valid 
//   for certain kinds of expression and we assert when accessors are 
//   called for the wrong kinds of expressions.
//
//   We do allow creation and simplification of VN_EXPR objects with Top()
//   or Bottom() value numbers as operands, but we *never* allow hashing 
//   of such objects (the hash() method will assert!).
//
//   Each public method is described in further detail at its point 
//   of declaration below.
//
//
// IMPLEMENTATION DETAILS:
//
//   We divide the VN_EXPR objects into different subclasses of 
//   expressions depending on the kind of expression (VN_EXPR::KIND) 
//   we are dealing with.  This is best expressed using an inheritance 
//   hierarchy, and we therefore take the liberty here of creating an 
//   inheritance hierarchy to represent this expression taxonomy.
//
//   The user of a VN_EXPR need never be concerned with this taxonomy,
//   since it is made completely transparant through the class and object
//   interface provided through VN_EXPR.  One consequence of this is that
//   we only export a means for a user to create a pointer to a VN_EXPR,
//   where the VN_EXPR is allocated using a given mempool.  A VN_EXPR
//   may be deleted by means of the free() class method, which in an 
//   implementation defined manner may or may not actually free up the
//   memory.  To free up all allocated VN_EXPR memory (i.e. any free-lists
//   kept around), we provide the method VN_EXPR::Reclaim_Free_Lists().  
//   This should be done even when the memory reclamation is automatic
//   (e.g. by means of popping a mempool), since the free-lists is an 
//   odd mix if malloc'ed and CXX_ALLOC'ed memory.
//   
// POSSIBLE IMPROVEMENTS (TODO):
//
//   If the NULL valued holes in the VN_EXPR_MAP turn out to be very
//   large, it may be better to use a more compact representation for
//   this map.
//
//   Currently, we never expect to see Top() operands in any expressions
//   other than phi nodes, and as such the optimistic simplification of
//   other kinds of expressions is just there for generality and to support
//   potential future functionality.  We can add more aggressive
//   simplification incrementally, while leaving open the possibility
//   that simplified VN_EXPRs may have Top() operands.
//     Ideally we should have various degrees of simplification.  The most
//   aggressive version should always reduces an expression into a form
//   without any Top() values, since *any* value can be substituted for
//   a Top() value.  Conceivably, there may be cases, however, where a
//   less agressive simplification algorithm is desirable (e.g. to keep
//   the simplification implementation simpler, or to delay the folding
//   of Top() values until more context is available).
//     This is then a note to keep adding more aggressive rules of 
//   simplification, and perhaps allow various degrees of simplification.
//
// CONSTRAINTS:
//
//   A UNARY VN_EXPR should never be a PARM expression, except when returned
//   as a result of "simplify()".
//
//   A BINARY VN_EXPR cannot be a COMMA or RCOMMA expression.
//
// SEE ALSO:
//
//    opt_vn_hashtab.h : A mapping from VN_EXPR::PTR to VN_VALNUM, based on
//                       a hashing algorithm.
//
//    opt_vn.h         : Top level interface to the value-numbering 
//                       algorithms, and associated resultant maps.
//
// ====================================================================
// ====================================================================


#ifndef opt_vn_expr_INCLUDED
#define opt_vn_expr_INCLUDED "opt_vn_expr.h"

#include "segmented_array.h"
  
// Some common universally applicable number for the chunksizes used in
// segmented arrays, mainly used to minimize the number of instantiations
// of SEGMENTED_ARRAY.
//
#define VN_BUFFER_CHUNKSIZE 256


//---------------------- A Value Number ------------------------
//--------------------------------------------------------------
//
class VN_VALNUM
{
private:

   UINT32 _num;

   VN_VALNUM(UINT32 v): _num(v) {}

   static UINT32 _top() {return UINT32_MAX;}
   static UINT32 _bottom() {return 0U;}
   
public:

   static VN_VALNUM First() {return VN_VALNUM(1);}

   static VN_VALNUM Next(const VN_VALNUM prev) 
   {
      return VN_VALNUM(prev._num + 1);
   }
   
   static VN_VALNUM Top() {return VN_VALNUM(_top());}       // Matches all

   static VN_VALNUM Bottom() {return VN_VALNUM(_bottom());} // Matches none

   static VN_VALNUM Vn(UINT32 i) {return VN_VALNUM(i);} // For internal use!
   
   VN_VALNUM() : _num(_bottom()) {}
   VN_VALNUM(const VN_VALNUM &v) : _num(v._num) {}

   VN_VALNUM &operator= (const VN_VALNUM &v) {_num = v._num; return *this;}
   BOOL       operator< (const VN_VALNUM &v) const {return _num < v._num;}
   BOOL       operator> (const VN_VALNUM &v) const {return _num > v._num;}
   BOOL       operator<= (const VN_VALNUM &v) const {return _num <= v._num;}
   BOOL       operator>= (const VN_VALNUM &v) const {return _num >= v._num;}
   BOOL       operator== (const VN_VALNUM &v) const {return _num == v._num;}
   BOOL       operator!= (const VN_VALNUM &v) const {return _num != v._num;}

   BOOL is_bottom() const {return _num == _bottom();}
   BOOL is_top() const {return _num == _top();}
   BOOL equivalent_to(const VN_VALNUM &v) const
   {
      return (!(is_bottom() || v.is_bottom()) &&
	      (_num == v._num || is_top() || v.is_top()));
   }

   UINT32 ordinal() const {return _num;} // Only for use by VN_EXPR_MAP!

   void  print(FILE *fp = stderr) const;
   INT32 sprint(char *buf) const; // buf must be large enough to hold "vnNNN"!

}; // VN_VALNUM


//---------------- A Value Number Expression -------------------
//--------------------------------------------------------------
//
class VN;         // Defined in opt_vn.h
class VN_EXPR
{
public:

   typedef VN_EXPR *PTR;
   typedef const VN_EXPR *CONST_PTR;

protected:

   static MEM_POOL *_Mpool;

public:

   // This enumeration should be replaced by an enumeration in PHI_NODE 
   // when we upgrade our intermediate representatiojn to support gated
   // phi nodes.  I just insert it here now as a temporary placeholder.
   //
   enum PHI_TAG {PHI_TAG_UNKNOWN};

   // The kinds of expressions a VN_EXPR may represent.
   //
   enum KIND {LITERAL,     // Constant value
	      UNARY,       // One operand
	      BINARY,      // Two operands
	      TERNARY,     // Three operands
	      INTR_OP,     // Intrinsic function call
	      PHI,         // SSA Phi node
              LDA_ADDR,    // A symbol's address
              ARRAY_ADDR,  // An array element's address
              MEMLOC     // The value of a memory location
#ifdef KEY
              ,CALL_OP     // Pure function call op	
#endif
	      };

   // A call to activate the memory allocation scheme for VN_EXPR
   // objects.  Such objects should always be created using one of
   // the Create_xxx() methods, and should always be deleted using the
   // "free()" method.  This method MUST be called before any other
   // method of the VN_EXPR class.  If there is already a free-list
   // active when this method is called, then the old one will be 
   // discarded but not reclaimed.
   //
   static void Init_Free_Lists(MEM_POOL *mpool = Malloc_Mem_Pool);

   // The following method will reclaim memory for all VN_EXPR nodes
   // currently accumulated on free lists, and it will reclaim memory
   // for the free-list itself.  This is the ONLY way to reclaim 
   // VN_EXPR memory!  Following this call, a new call to Init_Free_Lists()
   // is needed before any call to Create_xxx() methods.  Never call this
   // method before all VN_EXPR objects are free()'ed (i.e. until they are
   // all on the free-list).
   //
   static void Reclaim_Free_Lists();

   // The following are constructors for the various kinds of expressions
   // we handle.  The operands for expressions with a variable number of
   // operands (INTRINISC_OP, PHI and ARRAY_ADDR objects) must be set with
   // separate calls to "set_opnd()"; initially such operands are
   // VN_VALNUM::Bottom().
   //
   // The value number operands to an ARRAY_ADDR should always be set in 
   // the following order (same order as for an OPR_ARRAY whirl node):
   //
   //     opnd(0)                                   == base_address
   //     opnd(1..num_dims())                       == dimension_size
   //     opnd(num_dims()+1..num_dims()+num_dims()) == index
   //
   static PTR Create_Literal(const TCON &elem_ton, TYPE_ID vect_ty);
   static PTR Create_Unary(OPCODE opc, 
			   const VN_VALNUM &vn1);
   static PTR Create_Binary(OPCODE opc, 
			    const VN_VALNUM &vn1,
			    const VN_VALNUM &vn2);
   static PTR Create_Ternary(OPCODE opc, 
			     const VN_VALNUM &vn1,
			     const VN_VALNUM &vn2,
			     const VN_VALNUM &vn3);
   static PTR Create_Intr_Op(INTRINSIC intr_opc, 
			     UINT32    num_opnds);
   static PTR Create_Phi(UINT32  num_opnds,
			 IDTYPE  block_id,
			 PHI_TAG phi_tag = PHI_TAG_UNKNOWN);
   static PTR Create_Lda_Addr(INT32 lda_cr_id);
   static PTR Create_Array_Addr(WN_ESIZE esize, INT32 num_dims);
   static PTR Create_Memloc(MTYPE            dsctype,
			    const VN_VALNUM &bytesize,
			    const VN_VALNUM &offset,
			    const VN_VALNUM &base_addr,
			    const VN_VALNUM &vsym_valnum);
#ifdef KEY
   static PTR Create_Call_Op(ST_IDX, UINT32);
#endif
   
   // The following will put "this" on a free list, and is the only
   // way to set a VN_EXPR object up for memory deallocation.
   //
   virtual void free() = 0;
   
   // Accessors:  The kind() and num_opnds() determine which of the following
   // accessors are valid for a VN_EXPR.  Note that operands are enumerated
   // starting at i=0.  The default implementations, for those that have one,
   // will assert when the method is called.
   //
   virtual KIND        get_kind() const = 0;
   virtual UINT32      get_num_opnds() const = 0;
   virtual VN_VALNUM   get_opnd(UINT i=0) const; // All, but LITERAL/LDA/MEMLOC
   virtual BOOL        has_top_opnd() const = 0;
   virtual BOOL        has_bottom_opnd() const = 0;
   virtual OPCODE      get_opc() const;           // UNARY,BINARY,TERNARY
   virtual INTRINSIC   get_intr_opc() const;      // INTR_OP
   virtual PHI_TAG     get_phi_tag() const;       // PHI
   virtual IDTYPE      get_block_id() const;      // PHI
   virtual const TCON &get_tcon() const;          // LITERAL
   virtual INT32       get_lda_cr_id() const;     // LDA
   virtual INT32       get_num_dims() const;      // ARRAY_ADDR
   virtual WN_ESIZE    get_esize() const;         // ARRAY_ADDR
   virtual MTYPE       get_dsctype() const;       // MEMLOC
   virtual VN_VALNUM   get_bytesize() const;      // MEMLOC
   virtual VN_VALNUM   get_offset() const;        // MEMLOC
   virtual VN_VALNUM   get_base_addr() const;     // MEMLOC
   virtual VN_VALNUM   get_vsym(UINT i=0) const; // MEMLOC/INTR_OP (ivar mu)
#ifdef KEY
   virtual ST_IDX      get_aux_id() const;        // PURE_CALL_OP
#endif
   
   // Modifiers: 
   //
   //   set_opnd() will change the given operand of "this" expression, where
   //   operand numbers are zero (0) based.  set_opnd_vsym() will change
   //   the vsym associated with the given parameter of "this" INTR_OP
   //   expression, where the parameter numbers (i) are zero based.
   //
   //   simplify() will try to fold "this" expression into a simpler
   //   new expression or into a value number.  When successful, "this"
   //   expression will be freed up (this->free()), and a new expression
   //   will be returned; otherwise, this is returned.  Note that simplify
   //   always returns a new expression with fewer operands than "this"
   //   expression (except when a unary expression folds into a value-number).
   //   When the expression folds into a single value number, a unary
   //   OPC_VPARM expression is returned.  Such an expression cannot be
   //   hashed, and MUST BE SPECIALLY RECOGNIZED BY THE CLIENT of this 
   //   class.  An expression with a Bottom() operand will always simplify
   //   to a Bottom() value number.
   //
   virtual void set_opnd(UINT32 i, VN_VALNUM vn);
   virtual void set_opnd_vsym(UINT32 i, VN_VALNUM vn);  // INTR_OP
   virtual PTR simplify(VN *v) = 0;
   
   // Any VN_EXPR can be hashed into a value suited for distribution
   // across a hash-table (see opt_vn_hashtab.h).  Hashing will apply
   // canonicalization rules such as commutativity to catch as many 
   // equivalences as possible, which means this expression may change 
   // as a side-effect.  This method should never be called when
   // this->has_top_opnd() || this->has_bottom_opnd()!
   //
   virtual size_t hash() = 0;

   // Inequality/Equality tests:
   //
   virtual BOOL is_equal_to(CONST_PTR expr) const = 0;

   virtual void print(FILE  *fp = stderr) const = 0;

}; // VN_EXPR


//-------------- A Value Number Expression Map -----------------
//--------------------------------------------------------------
//
class VN_EXPR_MAP
{
private:

   SEGMENTED_ARRAY<VN_EXPR::PTR, VN_BUFFER_CHUNKSIZE> _map;
   
public:

   VN_EXPR_MAP(MEM_POOL *mpool): _map(mpool) {}

   ~VN_EXPR_MAP() 
   {
      for (INT32 i = _map.Size() - 1; i >= 0; i--)
	 if (_map[i] != NULL)
	    _map[i]->free();
      _map.Delete_last(_map.Size());
   }

   // Resets all expressions referenced in the map (puts them on free-
   // lists) between the two given value numbers provided (inclusively),
   // where is_true(from_vn < to_vn) for the call to have any effect.
   //
   void reset_exprs(VN_VALNUM from_vn, VN_VALNUM to_vn)
   {
      INT32 max = (to_vn.ordinal() < _map.Size()? 
		   to_vn.ordinal() : _map.Size() - 1);
      
      for (INT32 i = from_vn.ordinal(); i <= max; i++)
	 if (_map[i] != NULL)
	 {
	    _map[i]->free();
	    _map[i] = NULL;
	 }
   }

   // Resets all expressions referenced in the map (puts them on free-
   // lists), and sets all value numbers to map to NULL expressions.
   //
   void reset_all_exprs()
   {
      reset_exprs(first(), last());
   }
   
   // First value number in the mapping.  Use VN_VALNUM::Next() to iterate.
   // Returns VN_VALNUM::Bottom() when the mapping is empty.
   //
   VN_VALNUM first() const 
   {
      return (_map.Size() > 0?  VN_VALNUM::First() : VN_VALNUM::Bottom());
   }

   // Last value number in the mapping.  Returns VN_VALNUM::Bottom()
   // when the mapping is empty.
   //
   VN_VALNUM last() const 
   {
      return (_map.Size() > 0? 
	      VN_VALNUM::Vn(_map.Size() - 1) :
	      VN_VALNUM::Bottom());
   }

   // Always NULL for Top() and Bottom().
   //
   VN_EXPR::PTR operator[] (const VN_VALNUM& v) const
   {
      if (v.is_top() || v.is_bottom())
	 return NULL;
      else
      {
	 const UINT32 i = v.ordinal();
	 return ((i >= _map.Size())? NULL : _map[i]);
      }
   } // operator[]


   // Never call this for a Top() or Bottom() value number.  Sets 
   // a mapping as indicated (creating a new one if none exists).
   //
   void set_map(const VN_VALNUM& v, VN_EXPR::PTR expr)
   {
      FmtAssert(!(v.is_top() || v.is_bottom()), 
		("Illegal value number in call to VN_EXPR_MAP::set_map"));
      {
	 const UINT32 i = v.ordinal();
	 for (UINT32 sz = _map.Size(); sz <= (i+1); _map.New_entry(sz) = NULL);
	 _map[i] = expr;
      }
   } // set_map
   
}; // VN_EXPR_MAP


#endif // opt_vn_expr_INCLUDED
