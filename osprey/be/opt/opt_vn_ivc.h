//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vn_ivc.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vn_ivc.h,v $
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
// PURPOSE:
// --------
//
//   Value number based induction variable classification (VN_IVC).  Note 
//   that the "ivc" used here is distinct from the "ivc" used in opt_vnfre
//   to denote induction variable coalescing, but both are related as 
//   classification is necessary to do coalescing.
//
// INTERFACE DESCRIPTION:
// ----------------------
//
//   This header file defines a datastructure used to classify induction 
//   variables (i.e. PHI results) into equivalence classes.  Typically, 
//   the interface is used in two phases for each loop header BB, where 
//   the first phase creates the VN_IVC maps, whereas the second phase 
//   accesses the maps for tasks such as induction variable coalescing.
//   The mapping retains enough information to support induction variable
//   coalescing:
//
//     * Call classify() on every live phi for a non-dedicated register 
//       that joins two values in a DOHEAD BB.  A PHI that fits our pattern
//       for an induction variable will be recorded as a member of an
//       equivalence class.
//
//   After classification, induction variable coalescing can proceed along
//   the following lines (see opt_vnfre.cxx for details).
//
//     * For each equivalence class with more than two members, or with one
//       member with more than one "hit" (indicating two or more induction
//       variables with the same value number), do the following:
//
//           2.1) Choose the class member with the most occurrences, and
//                calculate the "offset" from this "base" member for the
//                other members of the class.
//
//           2.2) Insert an assignment "tmp = base" in the loop_header
//                BB, and go through and substitute each occurrence of
//                each equivalence class member with a suitable 
//                "tmp + offset" expression (possibly saved to another
//                temporary.  Note that every member in an equivalence 
//                class will have a different "offset" from every other
//                member.
//
// REQUIREMENTS:
//
//  Note that the full definitions for member functions are provided
//  in this interface.  This was done, since this file is only included
//  in one file currently (opt_vnfre.cxx).  Should this file ever be
//  included in more than one file, a .cxx file should be created for
//  the definitions.  Must include a number of wopt and common header
//  files before this one (see opt_vnfre.cxx).
//
// ====================================================================
// ====================================================================


#ifndef opt_vn_ivc_INCLUDED
#define opt_vn_ivc_INCLUDED "opt_vn_ivc.h"

#include <vector>
#include <iterator>
#include "opt_vn_expr.h"    // For VN_VALNUM, VN_EXPR, and VN_EXPR_MAP
#include "opt_vn.h"         // For VN

using std::vector;

// ----- Interpretation of the -WOPT:vn_ivc=n option -----
// -------------------------------------------------------

// The default is always "1", which means maximize live ranges,
// be conservative in the application of ivc, and do it for
// loop-invariant differences in values between induction variables.

inline BOOL IVC_Enabled()
{
   return (WOPT_Enable_Vn_Ivc > 0);
}

inline BOOL IVC_Maximize_Live_Ranges()
{
   return (WOPT_Enable_Vn_Ivc >= 1 && WOPT_Enable_Vn_Ivc <= 4);
}

inline BOOL IVC_Minimize_Live_Ranges()
{
   return !IVC_Maximize_Live_Ranges();
}

inline BOOL IVC_Conservative()
{
   return (WOPT_Enable_Vn_Ivc == 1 || WOPT_Enable_Vn_Ivc == 2 ||
	   WOPT_Enable_Vn_Ivc == 5 || WOPT_Enable_Vn_Ivc == 6);
}

inline BOOL IVC_Aggressive()
{
   return !IVC_Conservative();
}

inline BOOL IVC_LoopInvariant_Diff()
{
   return (WOPT_Enable_Vn_Ivc == 1 || WOPT_Enable_Vn_Ivc == 3 ||
	   WOPT_Enable_Vn_Ivc == 5 || WOPT_Enable_Vn_Ivc == 7);
}

inline BOOL IVC_Only_Literal_Diff()
{
   return !IVC_LoopInvariant_Diff();
}


// --------------- Some utility data structures ---------
// ------------------------------------------------------

typedef INT32 ENTRY_IDX;
#define INVALID_ENTRY_IDX ENTRY_IDX(-1)

// The following represents a taxonomy of classification:  At the top
// level we classify induction variables only based on the step expression
// (STEP_EQCLASS).  The members of these top level classifications are
// induction variable equivalence classes (IVC_EQCLASS), each determined
// by the base value for the induction.  Each induction variable 
// classification consists of a list of equivalence class members.
//
// Although we use linear search through these classifications, the
// time complexity is O(c1^2 + c2^2 + c3^2) where c1, c2, and c3 are
// the number of members at each level of the classification (view it
// as an n'ary tree), which should on average be much better than the
// alternative O(total_num(IVC_EQCLASS_MEMBER)^2).
//

struct STEP_EQCLASS
{
   VN_VALNUM step_valnum;   // Inductive step amount for this equiv. class
   OPCODE    step_opc;      // Induction opcode for this equiv. class
   ENTRY_IDX first_eqclass; // Linked list of equiv. classes with this step
   ENTRY_IDX last_eqclass;  // Tail of linked list of equiv. classes
   
   STEP_EQCLASS(VN_VALNUM stepsize, OPCODE stepop): 
      step_valnum(stepsize),
      step_opc(stepop), 
      first_eqclass(INVALID_ENTRY_IDX),
      last_eqclass(INVALID_ENTRY_IDX)
   {}
}; // struct STEP_EQCLASS


struct EQCLASS
{
   INT32     num_members;
   ENTRY_IDX first_member; // Linked list of members in this equiv. class
   ENTRY_IDX last_member;  // Tail of linked list of members
   ENTRY_IDX next;         // Next equiv. class with same STEP_EQCLASS
   
   EQCLASS():
      num_members(0),
      first_member(INVALID_ENTRY_IDX),
      last_member(INVALID_ENTRY_IDX),
      next(INVALID_ENTRY_IDX)
   {}
}; // struct EQCLASS

   
struct EQCLASS_MEMBER
{
   INT32     num_occurs;        // Estimated no of real occurrences
   INT32     num_hits;          // Number of PHIs with this value number
   PHI_NODE *indvar_phi;        // Representative phi for induction var
   INT32     indvar_init_kidno; // The phi opnd initializing induction variable
   VN_VALNUM indvar_init_valnum;// Initial value of induction variable
   VN_VALNUM indvar_valnum;     // Induction var value number (of phi result)
   INT64     literal_ofst;      // Only valid if "is_literal_ofst==TRUE"
   BOOL      is_literal_ofst;   // The offset value from a chosen base member
   BOOL      required_index;    // Use as base value for all members in class

   ENTRY_IDX step;         // Induction step entry for this equiv. class
   ENTRY_IDX eqclass;       // Members of this equivalence class
   ENTRY_IDX next;          // Next member of this equivalence class
   
   EQCLASS_MEMBER(PHI_NODE *indphi,
		  INT32     initkid, 
		  VN_VALNUM initval, 
		  VN_VALNUM indval, 
		  ENTRY_IDX step_idx, 
		  ENTRY_IDX eqclass_idx):
      num_occurs(0),
      num_hits(0),
      indvar_phi(indphi),
      indvar_init_kidno(initkid),
      indvar_init_valnum(initval),
      indvar_valnum(indval),
      literal_ofst(0),
      is_literal_ofst(FALSE),
      required_index(FALSE),
      step(step_idx),
      eqclass(eqclass_idx),
      next(INVALID_ENTRY_IDX)
   {}
}; // struct EQCLASS_MEMBER


struct NEXT_EQCLASS_MEMBER
{
   ENTRY_IDX operator () (const EQCLASS_MEMBER &m) const
   {
      return m.next;
   }
};


// -- Forward iterator adaptor for containers of elements with next field ---
// --------------------------------------------------------------------------

template<typename _Tp, typename _Distance>
struct forward_iterator {
  typedef forward_iterator_tag iterator_category;
  typedef _Tp value_type;
  typedef _Distance difference_type;
  typedef _Tp* pointer;
  typedef _Tp& reference;
};

template <class Container, class Next>
class forward_to_next_iterator : 
   public forward_iterator<
     typename std::iterator_traits<typename Container::iterator>::value_type,
     typename std::iterator_traits<typename Container::iterator>::difference_type>
{
public:

   typedef typename Container::iterator iterator_type;
   typedef forward_to_next_iterator<Container, Next> Self;
   typedef typename Self::reference reference;

protected:

   Container * _obj;
   INT32       _idx;

   bool _is_dereferencable() const {return _idx >= 0 && _idx < _obj->size();}

   reference _deref() const 
   {
      Is_True(_is_dereferencable(), 
	      ("forward_to_next_iterator out of bounds %d", _idx));
      return (*_obj)[_idx];
   }

public:

   forward_to_next_iterator() {}
   explicit forward_to_next_iterator(Container &c, INT32 i)
      : _obj(&c), _idx(i) {}

   forward_to_next_iterator(const Self& x) : _obj(x._obj), _idx(x._idx) {}
   
   Container *container() const {return _obj;}   // For "==" operator
   INT32      current_idx() const {return _idx;} // For "==" operator
   
   reference operator*() const 
   {
      return _deref();
   }

   Self& operator++()
   {
      _idx = Next()(_deref());
      return *this;
   }

   Self operator++(int)
   {
      Self tmp = *this;
      _idx = Next()(_deref());
      return tmp;
   }
   
}; // forward_to_next_iterator


template <class Container, class Next>
bool operator== (const forward_to_next_iterator<Container,Next> &it1,
                 const forward_to_next_iterator<Container,Next> &it2)
{
   return (it1.container() == it2.container() &&
	   it1.current_idx() == it2.current_idx());
}

// ----------------- The exported interface -------------
// ------------------------------------------------------

class VN_IVC
{
public:

   enum IVC_KIND {IVC_CONST_DIFF, IVC_INVARIANT_DIFF};
   
private:

   typedef mempool_allocator<STEP_EQCLASS>   STEP_ALLOCATOR;
   typedef mempool_allocator<EQCLASS>        EQCLASS_ALLOCATOR;
   typedef mempool_allocator<EQCLASS_MEMBER> EQCLASS_MEMBER_ALLOCATOR;

   typedef vector<STEP_EQCLASS, STEP_ALLOCATOR>             IVC_STEP;
   typedef vector<EQCLASS, EQCLASS_ALLOCATOR>               IVC_EQCLASS;
   typedef vector<EQCLASS_MEMBER, EQCLASS_MEMBER_ALLOCATOR> IVC_EQCLASS_MEMB;
   
   MEM_POOL        *_lpool;
   VN              *_vn;
   IVC_KIND         _kind;
   IVC_STEP         _steps;
   IVC_EQCLASS      _eqclasses;
   IVC_EQCLASS_MEMB _eqclass_membs;

   static BOOL Is_induction_step(VN_EXPR::PTR step, VN_VALNUM join_result);

   pair<BOOL,INT64> _get_literal_diff(VN_VALNUM v1, VN_VALNUM v2, MTYPE rty);

   ENTRY_IDX _find_or_insert_step(VN_VALNUM step_valnum, OPCODE step_opc);

   ENTRY_IDX _find_or_insert_eqclass(ENTRY_IDX stepi, 
				     VN_VALNUM init_valnum,
				     MTYPE     rty);
   
   ENTRY_IDX _find_or_insert_member(PHI_NODE *phi,        // PHI for induct var
				    INT32     init_kidno, // Init val phi opnd
				    VN_VALNUM join_valnum,// PHI result valnum
				    VN_VALNUM init_valnum,// Init val valnum 
				    ENTRY_IDX stepi, 
				    ENTRY_IDX eqclassi,
				    BOOL      required_index);

   ENTRY_IDX _enter_class(PHI_NODE    *phi,          // PHI for induction var
			  INT32        init_kidno,   // Initial value phi opnd
			  VN_VALNUM    join_valnum,  // PHI result valnum
			  VN_VALNUM    init_valnum,  // induction initial value
			  VN_VALNUM    step_valnum,  // induction step value
			  VN_EXPR::PTR step_expr,    // induction step vn expr
			  BOOL         required_index);
   
public:

   typedef forward_to_next_iterator<IVC_EQCLASS_MEMB, 
                                    NEXT_EQCLASS_MEMBER> members_iterator;
   
   VN_IVC(IVC_KIND kind, VN *vn, MEM_POOL *lpool):
      _kind(kind), _vn (vn), _lpool(lpool) {}
   
   BOOL classify(PHI_NODE *phi, VN_VALNUM join_valnum, BOOL required_index);
   
   // ---------
   // Iterators
   // ---------

   ENTRY_IDX num_eqclasses() const 
   {
      return _eqclasses.size();
   }

   ENTRY_IDX num_members(ENTRY_IDX eqclassi) const 
   {
      Is_True(eqclassi < _eqclasses.size(), 
	      ("Equivalence class idx %d out of bounds in IVC:num_members",
	       eqclassi));
      
      return (_eqclasses[eqclassi].num_members);
   }

   members_iterator members_begin(ENTRY_IDX eqclassi) 
   {
      Is_True(eqclassi < _eqclasses.size(), 
	      ("Equivalence class idx %d out of bounds in IVC:member_num_hits",
	       eqclassi));

      return members_iterator(_eqclass_membs, 
			      (INT32)_eqclasses[eqclassi].first_member);
   }

   members_iterator members_end()
   {
      return members_iterator(_eqclass_membs, INVALID_ENTRY_IDX);
   }

   // ---------
   // Modifiers
   // ---------

   void set_num_occurs(EQCLASS_MEMBER &mem, INT32 num_occurs)
   {
      mem.num_occurs = num_occurs;
   }

   void reset_num_hits(EQCLASS_MEMBER &mem, INT32 num_hits) 
   {
      mem.num_hits = num_hits;
   }

   void set_literal_ofst(EQCLASS_MEMBER &mem, INT64 ofst) 
   {
      mem.literal_ofst = ofst;
      mem.is_literal_ofst = TRUE;
   }

   void reset_literal_ofst(EQCLASS_MEMBER &mem) 
   {
      mem.literal_ofst = 0;
      mem.is_literal_ofst = FALSE;
   }

   INT32 finalize_for_coalescing(const EQCLASS_MEMBER &base,
				 members_iterator      memb_it_start,
				 members_iterator      memb_it_end,
				 BOOL                  be_conservative);

   // ---------
   // Accessors
   // ---------

   IVC_KIND kind() const 
   {
      return _kind;
   }
   
   INT32 num_hits(const EQCLASS_MEMBER &mem) const
   {
      return mem.num_hits;
   }

   INT32 num_occurs(const EQCLASS_MEMBER &mem) const
   {
      return mem.num_occurs;
   }

   PHI_NODE *indvar_phi(const EQCLASS_MEMBER &mem) const
   {
      return mem.indvar_phi;
   }

   VN_VALNUM indvar_valnum(const EQCLASS_MEMBER &mem) const
   {
      return mem.indvar_valnum;
   }

   VN_VALNUM indvar_init_valnum(const EQCLASS_MEMBER &mem) const
   {
      return mem.indvar_init_valnum;
   }

   CODEREP *indvar_init_cr(const EQCLASS_MEMBER &mem) const
   {
      return mem.indvar_phi->OPND(mem.indvar_init_kidno);
   }

   BB_NODE *indvar_init_bb(const EQCLASS_MEMBER &mem) const
   {
      return mem.indvar_phi->Bb()->Nth_pred(mem.indvar_init_kidno);
   }

   BOOL indvar_is_literal_ofst(const EQCLASS_MEMBER &mem) const
   {
      return mem.is_literal_ofst;
   }

   INT64 indvar_literal_ofst(const EQCLASS_MEMBER &mem) const
   {
      return mem.literal_ofst;
   }

   BOOL required_index(const EQCLASS_MEMBER &mem) const
   {
      return mem.required_index;
   }

   OPCODE step_opc(const EQCLASS_MEMBER &mem) const
   {
      return _steps[mem.step].step_opc;
   }

   static BOOL Is_Induction_Var(VN *vn, VN_VALNUM v);

   // ---------
   // Printing
   // ---------
   
   void print(const EQCLASS_MEMBER &base, 
	      const EQCLASS_MEMBER &mem, 
	      FILE                 *outf = stderr);
   
}; // class VN_IVC


// --------------------- Definitions --------------------
// ------------------------------------------------------

BOOL 
VN_IVC::Is_induction_step(VN_EXPR::PTR step, VN_VALNUM join_result)
{
   // We classify induction variables with equal induction steps.  The
   // induction expression must either be an ADD (either opnd may be
   // the index var) or a SUB (lhs must be index var).
   //
   return (step != NULL && 
	   step->get_kind() == VN_EXPR::BINARY &&
	   ((OPCODE_operator(step->get_opc()) == OPR_ADD &&
	     (step->get_opnd(0) == join_result ||
	      step->get_opnd(1) == join_result)) ||
	    (OPCODE_operator(step->get_opc()) == OPR_SUB &&
	     step->get_opnd(0) == join_result)));
} // VN_IVC::Is_induction_step


pair<BOOL,INT64> 
VN_IVC::_get_literal_diff(VN_VALNUM v1, VN_VALNUM v2, MTYPE rty)
{
   // Return the difference between two value numbers as a
   // VN_EXPR::LITERAL, if possible; otherwise, return 
   // VN_VALNUM::Bottom().  This diff is "v1 - v2".
   //
   pair<BOOL, INT64> retval(FALSE, 0);
   OPCODE       opc = OPCODE_make_op(OPR_SUB, rty, MTYPE_V);
   VN_EXPR::PTR offset_expr = VN_EXPR::Create_Binary(opc, v1, v2);
   VN_EXPR::PTR offset_val = offset_expr->simplify(_vn);
   
   Is_True(MTYPE_is_integral(rty), ("Illegal rty in _get_literal_diff()"));
   
   if (offset_val->get_kind() == VN_EXPR::LITERAL)
      retval = pair<BOOL,INT64>(TRUE, Targ_To_Host(offset_val->get_tcon()));
   else
   {
      // Try (v2 - v1), since our simplification algorithm may not
      // work the same both ways.
      //
      VN_EXPR::PTR offset_expr2 = VN_EXPR::Create_Binary(opc, v2, v1);
      VN_EXPR::PTR offset_val2 = offset_expr2->simplify(_vn);
      
      if (offset_val2->get_kind() == VN_EXPR::LITERAL)
	 retval = pair<BOOL,INT64>(TRUE, 
				   -Targ_To_Host(offset_val2->get_tcon()));

      offset_val2->free();
   }
   offset_val->free();
   
   return retval;
} // VN_IVC::_get_literal_diff


ENTRY_IDX 
VN_IVC::_find_or_insert_step(VN_VALNUM step_valnum, OPCODE step_opc)
{
   ENTRY_IDX stepi = INVALID_ENTRY_IDX;

   // Try to find the set of equivalence classes with the given 
   // induction "step" characteristics.
   //
   for (ENTRY_IDX i = 0; i < _steps.size(); i++)
   {
      if (_steps[i].step_valnum == step_valnum &&
	  _steps[i].step_opc == step_opc)
	 stepi = i;
   }
   if (stepi == INVALID_ENTRY_IDX)
   {
      // Insert new step
      //
      _steps.push_back(STEP_EQCLASS(step_valnum, step_opc));
      stepi = _steps.size()-1;
   }
   return stepi;
} // VN_IVC::_find_or_insert_step


ENTRY_IDX
VN_IVC::_find_or_insert_eqclass(ENTRY_IDX stepi, 
				VN_VALNUM init_valnum,
				MTYPE     rty)
{
   ENTRY_IDX eqclassi = INVALID_ENTRY_IDX;

   if (_kind == IVC_INVARIANT_DIFF)
   {
      // In this mode of operation, since the steps match they belong to the
      // same equivalence class, since the initial value will always be
      // loop invariant.
      //
      //
      Is_True(_steps[stepi].first_eqclass == _steps[stepi].last_eqclass,
	      ("Too many equivalence classes for equivalent step-sizes"
	       "VN_IVC::_find_or_insert_eqclass()"));

      if (_steps[stepi].first_eqclass != INVALID_ENTRY_IDX)
	 eqclassi = _steps[stepi].first_eqclass;
   }
   else // (_kind == IVC_CONST_DIFF)
   {
      // Try to find an equivalence class within the given "step" class (stepi)
      // that is within a constant offset from the given induction base
      // (init_valnum).
      //
      for (ENTRY_IDX i = _steps[stepi].first_eqclass; 
	   i != INVALID_ENTRY_IDX && eqclassi == INVALID_ENTRY_IDX; 
	   i = _eqclasses[i].next)
      {
	 const VN_VALNUM next_init_valnum =
	    _eqclass_membs[_eqclasses[i].first_member].indvar_init_valnum;
	 
	 if (next_init_valnum == init_valnum)
	    eqclassi = i;
	 else
	 {
	    pair<BOOL,INT64> ofst = 
	       _get_literal_diff(init_valnum, next_init_valnum, rty);
	    
	    if (ofst.first)
	       eqclassi = i;
	 }
      }
   }
   
   if (eqclassi == INVALID_ENTRY_IDX)
   {
      // Insert new equivalence class
      //
      _eqclasses.push_back(EQCLASS());

      // The new equivalence class is at the tail of the _eqclasses vector.
      //
      eqclassi = _eqclasses.size()-1;
      
      // Append it into the corresponding chain of equivalence classes
      // for the given induction step.
      //
      if (_steps[stepi].first_eqclass == INVALID_ENTRY_IDX)
      {
	 _steps[stepi].first_eqclass = eqclassi;
      }
      else
      {
	 _eqclasses[_steps[stepi].last_eqclass].next = eqclassi;
      }
      _steps[stepi].last_eqclass = eqclassi;
   }
   return eqclassi;
} // VN_IVC::_find_or_insert_eqclass
   

ENTRY_IDX
VN_IVC::_find_or_insert_member(PHI_NODE *phi,         // PHI for induction var
			       INT32     init_kidno,  // Initial value phi opnd
			       VN_VALNUM join_valnum, // PHI result valnum
			       VN_VALNUM init_valnum, // Initial value valnum 
			       ENTRY_IDX stepi, 
			       ENTRY_IDX eqclassi,
			       BOOL      required_index)
{
   ENTRY_IDX eqclass_membi = INVALID_ENTRY_IDX;
   
   // Try to find a member in this equivalence class with the same 
   // profile (same init+step == same phi result valnum).
   //
   for (ENTRY_IDX i = _eqclasses[eqclassi].first_member;
	i != INVALID_ENTRY_IDX; 
	i = _eqclass_membs[i].next)
   {
      if (join_valnum == _eqclass_membs[i].indvar_valnum)
	 eqclass_membi = i;
   }

   // In the event that there is no match, insert a new member into this
   // equivalence class.
   //
   if (eqclass_membi == INVALID_ENTRY_IDX)
   {
      _eqclass_membs.push_back(EQCLASS_MEMBER(phi, init_kidno, init_valnum,
					      join_valnum, stepi, eqclassi));
      
      // The new member is at the end of the _eqclass_membs vector.
      //
      eqclass_membi = _eqclass_membs.size()-1;
      
      // Append it into the corresponding chain of members for this
      // equivalence class.
      //
      if (_eqclasses[eqclassi].first_member == INVALID_ENTRY_IDX)
      {
	 _eqclasses[eqclassi].first_member = eqclass_membi;
      }
      else
      {
	 _eqclass_membs[_eqclasses[eqclassi].last_member].next = 
	    eqclass_membi;
      }
      _eqclasses[eqclassi].last_member = eqclass_membi;
      _eqclasses[eqclassi].num_members += 1;
   }

   // Increment a counter of the number of times this member has been found!
   //
   _eqclass_membs[eqclass_membi].num_hits += 1;
   
   // Reset the representative phi when this induction variable is
   // required.  I.e. the phi represents an induction variable
   // that should not be removed as a result of IVC (e.g. for SWP
   // to work correctly).
   //
   if (required_index)
   {
      _eqclass_membs[eqclass_membi].required_index = TRUE;
      _eqclass_membs[eqclass_membi].indvar_phi = phi;
   }
   
   return eqclass_membi;
} // VN_IVC::_find_or_insert_member

   
ENTRY_IDX 
VN_IVC::_enter_class(PHI_NODE    *phi,          // PHI for induction var
		     INT32        init_kidno,   // Initial value phi opnd
		     VN_VALNUM    join_valnum,  // PHI result valnum
		     VN_VALNUM    init_valnum,  // induction initial value
		     VN_VALNUM    step_valnum,  // induction step value
		     VN_EXPR::PTR step_expr,    // induction step vn expr
		     BOOL         required_index)
{
   // Enter the given induction variable description into our 
   // classification mapping, provided it is not already in our 
   // classification, in which case this call only has the effect
   // of incrementing the VN_IVC::member_num_hits().
   //
   const OPCODE    step_opc  = step_expr->get_opc();
   const MTYPE     rty       = OPCODE_rtype(step_opc);
   const VN_VALNUM step_size = (step_expr->get_opnd(0) == join_valnum? 
				step_expr->get_opnd(1) : 
				step_expr->get_opnd(0));
   
   // Search for equivalence classes that have the same induction step,
   // a constant literal difference in initial value,
   //
   ENTRY_IDX stepi = _find_or_insert_step(step_size, step_opc);
   
   ENTRY_IDX eqclass = _find_or_insert_eqclass(stepi, init_valnum, rty);
   
   ENTRY_IDX member = 
      _find_or_insert_member(phi, init_kidno, join_valnum, init_valnum,
			     stepi, eqclass, required_index);
   return member;
} // VN_IVC::_enter_class


BOOL
VN_IVC::classify(PHI_NODE *phi, VN_VALNUM join_valnum, BOOL required_index)
{
   // If the given value number for a PHI matches the value-numbering
   // pattern we expect for the "join" point of an induction variable,
   // then enter it into the classification and return TRUE.  Otherwise,
   // return FALSE.  The "required_index" indicates that this is a index
   // variable we need to retain (it will be noted in a loopinfo record).
   //
   VN_EXPR::PTR join_expr = _vn->valnum_expr(join_valnum);
   
   Is_True(!join_valnum.is_bottom()              &&
	   join_expr != NULL                     &&
	   join_expr->get_kind() == VN_EXPR::PHI &&
	   join_expr->get_num_opnds() == 2,
	   ("Illegal candidate as induction variable in "
	    "VN_IVC::classify()"));

   const VN_VALNUM valnum0 = join_expr->get_opnd(0);
   const VN_VALNUM valnum1 = join_expr->get_opnd(1);
   ENTRY_IDX       eqclass = INVALID_ENTRY_IDX;
   
   if (!(valnum0.is_bottom() || valnum1.is_bottom()))
   {
      VN_EXPR::PTR expr0 = _vn->valnum_expr(valnum0); // step or init
      VN_EXPR::PTR expr1 = _vn->valnum_expr(valnum1); // step or init
      
      if (Is_induction_step(expr0, join_valnum) && 
	  MTYPE_is_integral(OPCODE_rtype(expr0->get_opc())))
      {
	 eqclass = _enter_class(phi, 1 /*init_kidno*/, join_valnum, 
				valnum1, valnum0, expr0, required_index);
      }
      else if (Is_induction_step(expr1, join_valnum) && 
	       MTYPE_is_integral(OPCODE_rtype(expr1->get_opc())))
      {
	 eqclass = _enter_class(phi, 0 /*init_kidno*/, join_valnum, 
				valnum0, valnum1, expr1, required_index);
      }
   }
   return (eqclass != INVALID_ENTRY_IDX);
} // VN_IVC::classify
   

BOOL 
VN_IVC::Is_Induction_Var(VN *vn, VN_VALNUM v)
{
   BOOL is_ivar = FALSE;
   
   if (!v.is_bottom())
   {
      VN_EXPR::PTR join_expr = vn->valnum_expr(v);
      
      if (join_expr != NULL                     &&
	  join_expr->get_kind() == VN_EXPR::PHI &&
	  join_expr->get_num_opnds() == 2)
      {
	 VN_VALNUM valnum0 = join_expr->get_opnd(0);
	 VN_VALNUM valnum1 = join_expr->get_opnd(1);
	 VN_EXPR::PTR expr0 = vn->valnum_expr(valnum0); // step or init
	 VN_EXPR::PTR expr1 = vn->valnum_expr(valnum1); // step or init
	 
	 if (Is_induction_step(expr0, v) || Is_induction_step(expr1, v))
	    is_ivar = TRUE;
      }
   }
   return is_ivar;
} // VN_IVC::Is_Induction_Var
   

INT32
VN_IVC::finalize_for_coalescing(const EQCLASS_MEMBER    &base,
				VN_IVC::members_iterator memb_it_start,
				VN_IVC::members_iterator memb_it_end,
				BOOL                     aggressive)
{
   // This function embeds our heuristics for when to do iv coalescing. It
   // calculates any literal difference between initial values and initializes
   // each member (literal_ofst) accordingly.  It also returns the total 
   // number of induction variables to be coalesced in this equivalence 
   // class.
   //
   // We mark members we decide not to coalesce with num_hits==0.
   //
   const VN_VALNUM          base_init_valnum = indvar_init_valnum(base);
   VN_IVC::members_iterator memb;
   INT32                    hit_counter = 0;

   for (memb = memb_it_start; memb != memb_it_end; ++memb)
   {
      const VN_VALNUM init_valnum = indvar_init_valnum(*memb);
      
      if (base_init_valnum == init_valnum)
      {
	 set_literal_ofst(*memb, 0);
	 hit_counter += num_hits(*memb);
      }
      else if (!aggressive && num_hits(*memb) < 2)
      {
	 // We assume that when (base_init_valnum != init_valnum), the
	 // offset is non-zero and an offset calculation is necessary
	 // inside the loop.  Every member has a different initial
	 // value, as far as we know at this point (the accuracy of this
	 // assumption depends on the accuracy of the value numbering 
	 // algorithm on the initial values).
	 //
	 // Assuming the calculation of the value of each member, as an offset
	 // from the base, is always saved to a temporary, we are only 
	 // guaranteed to reduce the number of assignments inside the loop
	 // when each such save replaces the step assignments of 2 or more
	 // induction variables (i.e. when num_hits > 2).
	 //
	 reset_num_hits(*memb, 0); // Disable this member
      }
      else // Not base member, and member has enough hits to be coalesced!
      {
	 const MTYPE      rty = OPCODE_rtype(step_opc(*memb));
	 pair<BOOL,INT64> literal_diff =
	    _get_literal_diff(init_valnum, base_init_valnum, rty);
	 
	 if (literal_diff.first)
	 {
	    // Ideally (literal_diff.second != 0), but this is dependent on
	    // the accuracy of our value numbering algorithm and the 
	    // details of _get_literal_diff()!
	    //
	    set_literal_ofst(*memb, literal_diff.second);
	    hit_counter += num_hits(*memb);
	 }
	 else if (kind() == IVC_CONST_DIFF)
	 {
	    // Could not reduce to literal constant.
	    //
	    // This should ideally never happen, but since _get_literal_diff()
	    // is called with different arguments here after we have chosen
	    // a base, as compared with the call in _find_or_insert_eqclass,
	    // it could potentially happen, and we therfore handle this case
	    // for robustness.
	    //
	    reset_num_hits(*memb, 0); // Disable this member
	 }
	 else // Allow non-literal diff
	 {
	    reset_literal_ofst(*memb);
	    hit_counter += num_hits(*memb);
	 }
      }
   } // for each member
   return hit_counter;
} // VN_IVC::finalize_for_coalescing


void 
VN_IVC::print(const EQCLASS_MEMBER &base, 
	      const EQCLASS_MEMBER &mem, 
	      FILE                 *outf)
{
   if (mem.is_literal_ofst)
   {
      fprintf(outf,
	      "EQCLASS %d, "
	      "indvar=vn%d, "
	      "hits=%d, "
	      "init_ofst=%lld, "
	      "init_bb=%d\n",
	      mem.eqclass, 
	      (INT32)mem.indvar_valnum.ordinal(),
	      mem.num_hits,
	      mem.literal_ofst,
	      (INT32)indvar_init_bb(mem)->Id());
   }
   else
   {
      fprintf(outf,
	      "EQCLASS %d, "
	      "indvar=vn%d, "
	      "hits=%d, "
	      "init_ofst=(vn%d(cr%d) - vn%d(cr%d)), "
	      "init_bb=%d\n",
	      mem.eqclass, 
	      (INT32)mem.indvar_valnum.ordinal(),
	      mem.num_hits,
	      (INT32)mem.indvar_init_valnum.ordinal(),
	      (INT32)indvar_init_cr(mem)->Coderep_id(),
	      (INT32)base.indvar_init_valnum.ordinal(),
	      (INT32)indvar_init_cr(base)->Coderep_id(),
	      (INT32)indvar_init_bb(mem)->Id());
   }
} // VN_IVC::print
   

// --------------------- Definitions --------------------
// ------------------------------------------------------

template <class GET_NUM_OCCURS>
VN_IVC::members_iterator
VN_IVC_choose_eqclass_base_indvar(GET_NUM_OCCURS           get_num_occurs,
				  VN_IVC                  &vn_ivc,
				  VN_IVC::members_iterator memb_it_start,
				  VN_IVC::members_iterator memb_it_end)
{
   // Assumption: We are given a sequence of members representing equivalent
   // induction variables (iv).  Inside a loop each iv may only differ in
   // value from any other iv in the sequence by a loop-invariant value.
   //
   // We here select one induction variable representative of this
   // equivalence class, such that it would be desirable to express
   // the other members in terms of a loop invariant offset from this 
   // base.
   //
   BOOL                     found_required_base = FALSE;
   VN_IVC::members_iterator base_memb = memb_it_start;
   VN_IVC::members_iterator memb_it;
   
   // Select one member as the base induction variable, i.e. the one
   // induction variable we want to retain for this equivalence class.
   // Also, set the number of worklist occurrences for each member.
   //
   for (memb_it = memb_it_start; memb_it != memb_it_end; ++memb_it)
   {
      const INT32 num_occurs = get_num_occurs(vn_ivc.indvar_valnum(*memb_it));

      vn_ivc.set_num_occurs(*memb_it, num_occurs);
      if (vn_ivc.required_index(*memb_it))
      {
	 base_memb = memb_it;
	 found_required_base = TRUE;
      }
      else if (!found_required_base &&
	       num_occurs > vn_ivc.num_occurs(*base_memb))
      {
	 base_memb = memb_it;
      }
   } // for each member
   return base_memb;
} // VN_IVC_choose_eqclass_base_indvar

#endif // opt_vn_ivc_INCLUDED
