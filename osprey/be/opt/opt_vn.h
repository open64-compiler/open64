//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vn.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vn.h,v $
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
// INTERFACE DESCRIPTION:
// ----------------------
//
//   This file introduces a class of object representing a value numbering.
//   The constructor will walk over the CFG and create the value numbering.
//   Once the value numbering (VN) object is constructed, the other exported 
//   methods may be used to access the value numbering results.  Note
//   that the cfg passed in to the constructor MUST already be in SSA
//   form.
//
//   The following outlines characteristics of the results of a value
//   numbering:
//
//     * The expr_cr() mapping maps a CODEREP::Coderep_id() to a CODEREP*.
//       Ideally this ought to be part of the codemap, but since it is not
//       we need to define this mapping as part of the value numbering.
//       TODO: get rid of this when it is created as part of a CODEMAP.
//       
//     * The expr_valnum() mapping maps a CODEREP::Coderep_id() to a value
//       number and should always be well defined and should never be 
//       Bottom() or Top(), with a few exceptions: black boxes 
//       may be Bottom() or Top(), OPR_CALL, OPR_PICCALL, OPR_ICALL,
//       OPR_INTRINSIC_CALL) and zero version VARs will be assigned
//       Bottom() values.  OPR_PARM CK_IVAR codereps and OPR_PARM
//       CK_OP codereps will be assigned the same value number as the 
//       argument expression (Base_opnd() and Opnd0() respectively).  The 
//       client of the interface exported here should assert if any 
//       unexpected Top() or Bottom() value number is returned.
//
//     * Note that we *do* value number dead statements, since no statement
//       is marked as live immediately after the CODEMAP is created, and we
//       may wish to employ offline value numbering before the "live" flags
//       are set reliably on STMTREPs.
//
//     * The expr_stmts() is a mapping from each CODEREP::Coderep_id() to 
//       the STMTREP* that contains it.  The range of the mapping has to be
//       a list of STMTREPs, since our program representation allows a
//       CODEREP to be shared between different statements.  We do not record
//       any STMTREP for CODEREPs that have no valid value number (e.g. _|_),
//       or when the CODEREP is a VAR or IVAR on the lhs of an assignment,
//       or when the value number denotes a LITERAL and the coderep is also
//       a literal value (on the assumption that no consumer of the STMT_LIST
//       ever need these).  The current definition of a STMT_LIST as produced
//       by a VN is made to fit the needs of our redundancy elimination 
//       algorithm.
//
//     * The valnum_expr() mapping maps each value number to the VN_EXPR::PTR
//       used to derive the value number.  This mapping will yield a NULL
//       value when VN_EXPR::PTR contained Bottom() operands or was arrived
//       at through a Chi node.  All other cases should be non-NULL.
//
//     * Integral VN_EXPR::LITERALs will all have type I8, and must be
//       coerced into the appropriate type if they are to be inserted
//       into our IR as part of an optimization.
//
//     * The VALNUM_TO_EXPR_LIST class can be used to construct the reverse
//       mapping of the VN::expr_valnum() mapping.  This reverse mapping
//       will be NULL for value numbers that are not well-defined (such
//       as Top() and Bottom()).
//
// USAGE:
// ------
//
//   The VN class employs the given global memory pool (gpool) to allocate
//   data-structures for a constructed VN object, while it pushes and pops
//   the local memory pool (lpool) during construction (VN::VN) and while
//   performing object methods.  We made the choice of separating the 
//   decision as to what mempool is used for allocation of VN_EXPR objects 
//   from the mempools used by a VN object.  This requires a bit of care 
//   from the user of the VN_EXPR and the VN interfaces:
//
//      1) Make sure you call VN_EXPR::Init_Free_Lists(mpool) before you 
//         construct any VN object; otherwise VN construction will fail 
//         with a segv fault.
//
//      2) Make sure the mpool for VN_EXPRs and the lpool for VNs are
//         either different, or they are both the Malloc_Mem_Pool.
//
//      3) Make sure you call VN_EXPR::Reclaim_Free_Lists() to reclaim 
//         free-lists of VN_EXPR objects.  Deletion of a VN object will 
//         not delete the VN_EXPR free-lists.  If you wish to allocate
//         VN_EXPR from a new mempool always call Reclaim_Free_Lists() 
//         followed by Init_Free_Lists(mpool).
//
//   The decoupling of the VN_EXPR mempool from the VN mempools allows
//   more freedom in the allocation scheme, and in particular it allows
//   repeated use of VN_EXPR free-lists, while constructing and deleting
//   multiple VN objects.
//
//   Example:
//
//      OPT_POOL_Push(gpool, -1);
//      {
//         VN_EXPR::Init_Free_Lists(gpool);
//         {
//            VN vn(VN::ITERATIVE, cfg, codemap, lpool, gpool);
//            ... // use first vn
//         }
//         ... // Do other stuff!
//         {
//            VN vn(VN::SINGLE_PASS, cfg, codemap, lpool, gpool);
//            ... // use second vn
//         }
//         VN_EXPR::Reclaim_Free_Lists();
//      }
//      OPT_POOL_Pop(gpool, -1);
//
//   Note that VALNUM_TO_EXPR_LIST also uses a mempool, which may or may
//   not be different from the VN mempools!
//
//
// IMPLEMENTATION DETAILS AND RELATION OF ALGORITHM TO LITERATURE:
// ---------------------------------------------------------------
//
//   The algorithms implemented here are based on Taylor Simpson's 
//   work, as documented in his Ph.D. thesis from Rice University entitled
//   "Value Driven Redundancy Elimination", April 1996.
//
//   His thesis describes two methods of value numbering which we have
//   implemented here.  One is the pessimistic one-pass "unified hash-table
//   algorithm"; and the other is the similar optimistic iterative 
//   "Reverse Postorder" (RPO) algorithm.  His SCC (strongly connected 
//   components) algorithm is touted as the most efficient version of
//   the RPO algorithm, but it is sufficiently different in implementation
//   and hard to intuitively understand from the theoretically proven 
//   RPO algorithm, that we instead opted to implement the straight-forward 
//   RPO algorithm.  The efficiency benefit of the SCC over the RPO 
//   algorithm is not clear.
//
//   Some characteristics of our SSA representation that affects the
//   algorithm and caused some changes in the way we do things are as
//   follows:
//
//      1) Instead of triplets, we have a program representation in terms
//         of CODEREPS, where the Coderep_id() can be viewed as the
//         notion of a "register" as used in Taylor's algorithms.
//
//      2) Since codereps may be shared between different expressions,
//         we need a is_numbered[] boolean array to denote whether or
//         not a Coderep_id[] has already been value numbered (in a
//         particular iteration of the RPO algorithm).  This saves us
//         from having to construct a VN_EXPR, simplify, and hash an
//         expression twice.
//
//      3) We do not use Coderep_id() as a value number, instead assigning
//         consecutive numbers as value numbers, since there are likely
//         to be many more CODEREP nodes than value numbers.  This reduces
//         the size of any maps we have from value numbers to other info,
//         such as the VN_VALNUM-->VN_EXPR::PTR map.
//
//      4) Our method for finding congruences between indirect loads and 
//         stores is a little different from Taylor's method.  We use
//         the regular hash-table to hash lvalues, and will map such lvalue
//         expressions (MEMLOC expressions) to the lhs of assignments.
//         We may see a load off an lvalue without seeing a store into
//         one beforehand, and will apply a unique value number to such
//         a load.  When a store is seen into an lvalue, the fact that
//         we use the version of the virtual variable as part of the MEMLOC
//         guarantees that a load from the same location has not been seen
//         beforehand.  The lvalue MEMLOC for a store will be mapped to
//         the same value number as the rhs expression's value number, and
//         this is the only case when two different expressions in the
//         hash-table may map to the same value number.
//
//      5) Taylor specifies that the iterative algorithm requires reverse 
//         postorder traversal of the CFG, while the single-pass algorithm
//         requires a preorder (or reverse postorder) travesal of the 
//         dominator tree with a CFG reverse postorder traversal of the 
//         dominator tree kids.  We simplify this to use a reverse postorder
//         traversal of the CFG in both cases.  The algorithms are therefore
//         essentially the same in each pass over the CFG, except that 
//         one of them starts off with optimistic assumptions, while the 
//         other starts off with pessimistic assumptions.  Given this order
//         of traversal, the only undefined values we *ever* encounter 
//         are parameters of phi nodes representing data-flow up back-edges
//         in the CFG and indirect loads of lvalues (see point 4 concerning
//         lvalues).
//
//      6) We create a map from VN_VALNUMs to VN_EXPR:PTR in addition to the
//         Coderep_Id() to VN_VALNUM mapping.  This is useful to any 
//         subsequent optimizations (such as AVAIL based redundancy
//         elimination or partial redundancy elimination).
//
//      7) For phi nodes the value number will be assigned to the RESULT 
//         coderep in the cr_to_vn mapping.  For ivars (indirect loads/stores)
//         the value number will be assigned to the ivar coderep.
//
//      8) Our representation (CODEREPS) may have implicit conversion
//         operations (e.g. U8U4ILOAD for mips3), and we will also need
//         to associate these with an EXPRID.  To handle this, and yet
//         have the iterative algorithm terminate we need a second cr_to_vn
//         mapping.
//    
// SEE ALSO:
// ---------
//
//    opt_cfg.h:    Control flow graph.
//    opt_ssa.h:    Control flow graph in SSA form.
//    opt_htable.h: Utilities for creating the CODEREP form of the cfg.
//
//    opt_vn_expr.h : Implementation of VN_VALNUM, VN_EXPR, and VN_EXPR_MAP.
//
//    opt_vn_hashtab.h : Implementation of VN_HASHTAB, which maps a 
//        VN_EXPR::PTR to VN_VALNUM, based on a hashing algorithm.
//
// ====================================================================
// ====================================================================


#ifndef opt_vn_INCLUDED
#define opt_vn_INCLUDED "opt_vn.h"

#include <ext/slist>
#include <vector>
#include "segmented_array.h"
#include "opt_vn_expr.h"    // For VN_VALNUM, VN_EXPR, and VN_EXPR_MAP
#include "opt_vn_hashtab.h" // For VN_HASHTAB

#ifdef __STL_USE_NAMESPACES
using std::slist;
using std::vector;
#endif

class CODEREP;
class STMTREP;
class CFG;
class CODEMAP;
class VALNUM_TO_EXPR_LIST;


class VN
{
public:

   typedef INT32 EXPRID;      // CODEREP::Coderep_id()

   enum VN_ALGORITHM {SINGLE_PASS, ITERATIVE};

   typedef mempool_allocator<STMTREP*>    STMT_ALLOCATOR;
   typedef slist<STMTREP*, STMT_ALLOCATOR> STMT_LIST; // linked list

   typedef mempool_allocator<VN_VALNUM>        VALNUM_ALLOCATOR;
   typedef vector<VN_VALNUM, VALNUM_ALLOCATOR> VALNUM_VECTOR;

   typedef mempool_allocator<bool>         BVECTOR_ALLOCATOR;
   typedef vector<bool, BVECTOR_ALLOCATOR> BIT_VECTOR;

private:

   typedef mempool_allocator<CODEREP*>      CODEREP_ALLOCATOR;
   typedef vector<CODEREP*, CODEREP_ALLOCATOR> CODEREP_VECTOR;

   typedef mempool_allocator<STMT_LIST> STMTLIST_ALLOCATOR;
   typedef vector<STMT_LIST, STMTLIST_ALLOCATOR> STMTLIST_VECTOR;

   struct VN_ALGORITHM_STATUS
   {
      VN_HASHTAB    *expr_to_vn;    // VN_EXPR->VN_VALNUM (used for callback)
      BIT_VECTOR    *is_numbered;   // VN_VALNUM->BOOL    (for explicit ops)
      BIT_VECTOR    *locked_to_vn;  // VN_VALNUM->BOOL    (for explicit ops)
      BIT_VECTOR    *locked_to_vn2; // VN_VALNUM->BOOL    (for implicit ops)
      VALNUM_VECTOR *exprid_to_vn2; // EXPRID->VN_VALNUM  (for implicit ops)
      BOOL           changed;       // Any valnum changed in this iteration

      VN_ALGORITHM_STATUS(): 
	 expr_to_vn(NULL), 
	 is_numbered(NULL), 
	 locked_to_vn(NULL), 
	 locked_to_vn2(NULL),
	 exprid_to_vn2(NULL),
	 changed(FALSE)
      {}
   }; // struct VN_ALGORITHM_STATUS
      
   MEM_POOL            *_lpool; // Pool used for internal algorithms
   MEM_POOL            *_gpool; // Pool used for a "this" object
   STMTREP             *_current_stmt;     // For use during value numbering
   INT32                _no_of_iterations; // For debugging
   VN_VALNUM            _zero_valnum;      // For integer literals
   VN_VALNUM            _next_valnum;
   VN_EXPR_MAP          _vn_to_expr;         // VN_VALNUM --> VN_EXPR
   VALNUM_VECTOR        _exprid_to_vn;       // Coderep_id --> VN_VALNUM
   STMTLIST_VECTOR      _exprid_to_stmtlist; // Coderep_id --> STMT_LIST;
   CODEREP_VECTOR       _exprid_to_cr;       // Coderep_id --> (CODEREP*)
   VN_ALGORITHM_STATUS  _status;             // Needed only during construction
   
   static VN_VALNUM Initial_Valnum(VN_ALGORITHM algo)
   {
      return (algo == SINGLE_PASS? VN_VALNUM::Bottom() : VN_VALNUM::Top());
   }

   void _grow_exprid_maps(EXPRID id);
   
   EXPRID _get_exprid(const CODEREP *cr)
   {
      const EXPRID exprid = cr->Coderep_id();
      _exprid_to_cr[exprid] = (CODEREP *)cr;
      return exprid;
   }

   EXPRID _append_exprid(const CODEREP *cr)
   {
      // Only use this to modify an existing value numbering, such as
      // changes caused by VNFRE.
      //
      const EXPRID exprid = cr->Coderep_id();
      _grow_exprid_maps(exprid);
      _exprid_to_cr[exprid] = (CODEREP *)cr;
      return exprid;
   }

   VN_VALNUM _unique_valnum(EXPRID               exprid, 
			    const VALNUM_VECTOR &exprid_to_vn,
			    const BIT_VECTOR    &locked_to_vn) const
   {
      return (locked_to_vn[exprid]? exprid_to_vn[exprid] : _next_valnum);
   }

   VN_VALNUM _get_literal_valnum(INT64 i) const
   {
      return VN_VALNUM::Vn(i + _zero_valnum.ordinal());
   }

   void _init_integer_valnum_map();

   VN_VALNUM _valnum_integer(INT64 literal,
			     BOOL  is_signed);

   inline void _set_stmt_map(CODEKIND ck, EXPRID id, const VN_VALNUM &vn);
   
   void _set_valnum(EXPRID           id, 
		    const VN_VALNUM &vn,
		    VALNUM_VECTOR   &exprid_to_vn,
		    BIT_VECTOR      &locked_to_vn);

   VN_VALNUM _valnum_vn_expr(EXPRID        exprid, 
			     VN_EXPR::PTR  expr,
			     VALNUM_VECTOR &exprid_to_vn,
			     BIT_VECTOR    &locked_to_vn);

   VN_VALNUM _valnum_sym(CODEREP *sym);

   VN_VALNUM _valnum_op(CODEREP *cr);


   VN_VALNUM _valnum_expr(CODEREP *cr);

   VN_VALNUM _valnum_implicit_integral_cvt(EXPRID         exprid,
					   VN_VALNUM      opnd_valnum,
					   MTYPE          from_mty,
					   MTYPE          to_mty,
					   VALNUM_VECTOR &exprid_to_vn,
					   BIT_VECTOR    &locked_to_vn);

   VN_VALNUM _valnum_lhs(EXPRID    lhs_exprid,
			 VN_VALNUM valnum,
			 MTYPE     lhs_dty,
			 MTYPE     lhs_dscty,
			 MTYPE     rhs_dty);

   VN_VALNUM _valnum_memloc_load(CODEREP *cr);

   void _valnum_memloc_store(CODEREP   *lhs,
			     VN_VALNUM  rhs_valnum,
			     MTYPE      rhs_mtype);

   void _valnum_phi_list(IDTYPE    bb_id,
			 PHI_LIST *phi_list);

   void _valnum_chi_list(CHI_LIST *chi_list);


   void _valnum_stmt(STMTREP *stmt);

   void _valnum_cfg(CFG *cfg);

   void _trace(EXPRID id, VN_VALNUM valnum, FILE *fp = stderr);

   void _print_exprid_to_vn(FILE *fp, EXPRID id, INT32 column_width) const;

   void _print_vn_to_exprid(FILE                      *fp, 
			    const VALNUM_TO_EXPR_LIST &vn_to_exprid,
			    VN_VALNUM                  valnum) const;
   
public:

   VN(VN_ALGORITHM algo, 
      CFG         *cfg, 
      CODEMAP     *codemap,  
      MEM_POOL    *lpool = Malloc_Mem_Pool,  
      MEM_POOL    *gpool = Malloc_Mem_Pool);

   ~VN();
   
   CODEREP * expr_cr(EXPRID exprid) const 
   {
      Is_True(exprid <_exprid_to_cr.size(), 
	      ("Out of bounds access to VN::expr_cr()"));
      return _exprid_to_cr[exprid];
   }

   VN_VALNUM expr_valnum(EXPRID exprid) const 
   {
      Is_True(exprid < _exprid_to_vn.size(), 
	      ("Out of bounds access to VN::expr_valnum()"));
      return (_exprid_to_vn[exprid] == VN_VALNUM::Top()?
	      VN_VALNUM::Bottom() : _exprid_to_vn[exprid]);
   }

   const STMT_LIST *expr_stmts(EXPRID exprid) const 
   {
      Is_True(exprid < _exprid_to_stmtlist.size(), 
	      ("Out of bounds access to VN::expr_stmts()"));
      return &_exprid_to_stmtlist[exprid];
   }

   VN_EXPR::PTR valnum_expr(VN_VALNUM vn) const
   {
      return _vn_to_expr[vn];
   }

   EXPRID last_exprid() const 
   {
      return _exprid_to_vn.size() - 1;
   }

   VN_VALNUM last_valnum() const 
   {
      return VN_VALNUM::Vn(_next_valnum.ordinal() - 1);
   }

   // Modifications to a computed value numbering.
   //
   void reset_valnum(const CODEREP *cr, VN_VALNUM valnum)
   {
      _exprid_to_vn[_get_exprid(cr)] = valnum;
   }
   
   void new_cr(const CODEREP *cr, VN_VALNUM valnum)
   {
      _exprid_to_vn[_append_exprid(cr)] = valnum;
   }

   // In addition to the expr_valnum() mapping, we also print out the 
   // reverse valnum->exprid mapping for all value numbers that are 
   // neither Bottom() nor Top().  This reverse mapping will also
   // show the value number expression associated with each value number.
   //
   void print(FILE *fp = stderr, BOOL emit_stmt_maps = FALSE) const;

   VN_VALNUM invent_unique_valnum()
   {
      VN_VALNUM unique = _next_valnum;
      _next_valnum = VN_VALNUM::Next(_next_valnum);
      return unique;
   }

   // The following is a callback routine used during VN construction,
   // but may also be called after VN construction, in which case we always
   // simply create a unique value number.
   //
   VN_VALNUM valnum_integer(TCON int_tcon)
   {
      Is_True(MTYPE_is_integral(TCON_ty(int_tcon)),
	      ("Illegal argument to VN::valnum_integer"));

      if (_status.expr_to_vn != NULL)
	 return _valnum_integer(Targ_To_Host(int_tcon),
				MTYPE_is_signed(TCON_ty(int_tcon)));
      else
	 return invent_unique_valnum();
   }

}; // class VN


//---------------------------------------------------------------------------
// VALNUM_TO_EXPR_LIST: Maps each VN_VALNUM to a list of Coderep_id() numbers,
// except Bottom() and Top() values.  Note that the "[]" operator returns
// a NULL value for Top(), Bottom(), or an invalid value number;  Iterate
// over this mapping from VN_VALNUM::First() to last_valnum().
//---------------------------------------------------------------------------
//
class VALNUM_TO_EXPR_LIST
{
public:

   typedef mempool_allocator<VN::EXPRID> EXPRID_ALLOCATOR;
   typedef slist<VN::EXPRID, EXPRID_ALLOCATOR> EXPR_LIST; // linked list
   typedef EXPR_LIST::const_iterator EXPR_ITERATOR;

private:

   typedef mempool_allocator<EXPR_LIST> EXPRLIST_ALLOCATOR;
   vector<EXPR_LIST, EXPRLIST_ALLOCATOR> _map;

public:

   VALNUM_TO_EXPR_LIST(const VN &vn, MEM_POOL *mpool = Malloc_Mem_Pool);

   BOOL is_empty(const VN_VALNUM& v) const
   {
      return (v.is_top() || v.is_bottom() || _map[v.ordinal()].empty());
   }

   INT32 size(const VN_VALNUM& v) const
   {
      return ((v.is_top() || v.is_bottom())? 0 : _map[v.ordinal()].size());
   }
   
   VN::EXPRID front(const VN_VALNUM& v) const
   {
      if (v.is_top() || v.is_bottom() || _map[v.ordinal()].empty())
	 return 0;
      else
	 return _map[v.ordinal()].front();
   }

   EXPR_ITERATOR begin(const VN_VALNUM& v) const
   {
      return ((v.is_top() || v.is_bottom())? 
	      EXPR_ITERATOR() : _map[v.ordinal()].begin());
   }

   EXPR_ITERATOR end(const VN_VALNUM& v) const
   {
      return ((v.is_top() || v.is_bottom())? 
	      EXPR_ITERATOR() : _map[v.ordinal()].end());
   }

}; // VALNUM_TO_EXPR_LIST


#endif // opt_vn_INCLUDED
