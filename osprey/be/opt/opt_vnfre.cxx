//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vnfre.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vnfre.cxx,v $
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
//   This file implements our "full redundancy elimination" algorithm
//   based on value numbering.  The algorithm is represented by the
//   subroutine:
//
//      VNFRE::remove_redundancies(...)
//
//   The implementation of the algorithm is encapsulated in terms of a
//   function object VALNUM_FRE, where the constructor sets up the
//   environment for the algorithm, while the application operator
//   "apply()" invokes the algorithm.  Any statistics and such can be 
//   accumulated into private members of the class and printed out as
//   part of the operator "apply()".
//
//   The other public methods exported by VALNUM_FRE are only valid
//   once an invocation of the application operator "apply()" is
//   active, and they may be called by the parts of the algorithm that
//   is delegated out to the etable.
//
//   Note that there are two modes of tracing VNFRE activities.  One is
//   the regular Is_Trace() calls, also used by other opt_xxx files. 
//   The other kind is a compile-time activated VNFRE_TRACE macro,
//   which writes to stderr.
//
//   TODO: unify the two and make the traces of VNFRE activity prettier
//   and more easily readable than the Is_Trace() calls as performed
//   by EPRE.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdint.h>
#include <stack>

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
#include "opt_main.h"
#include "opt_mu_chi.h"
#include "opt_etable.h" // Also includes "opt_vnfre.h"
#include "opt_lftr2.h"  // Should we ever create any of these?
#include "opt_vn.h"
#include "opt_vn_ivc.h" // Induction variable classification for coalescing
#include "opt_vn_expr_taxonomy.h"

// #define DO_VNFRE_TRACE 1


using std::stack;

static inline void dummy(...)
{
}

#ifdef DO_VNFRE_TRACE
  #define VNFRE_TRACE fprintf
#else
  #define VNFRE_TRACE dummy
#endif // VNFRE_TRACE


// Typedef to represent a mapping from value numbers to occurrence lists.
// EXP_WORKLST is defined in sopt_etable.h.
//
typedef mempool_allocator<EXP_WORKLST*>         WORKLST_ALLOCATOR;
typedef vector<EXP_WORKLST*, WORKLST_ALLOCATOR> VALNUM_TO_WORKLST;

// Typedef to represent a stack of value occurrences in terms of
// a vector of such occurrences.
//
typedef mempool_allocator<EXP_OCCURS *>       OCCURS_ALLOCATOR;
typedef vector<EXP_OCCURS*, OCCURS_ALLOCATOR> OCCURS_VECTOR;
typedef stack<EXP_OCCURS *, OCCURS_VECTOR>    OCCURS_STACK;

// Typedef to represent a vector of INT8s.
//
typedef mempool_allocator<INT8>      INT8_ALLOCATOR;
typedef vector<INT8, INT8_ALLOCATOR> INT8_VECTOR;


// ---------------------------------------------------------------------------
// ---------------------------- Utility subroutines --------------------------
// ---------------------------------------------------------------------------

class Match_Cr
{
private:
   const CODEREP *_cr;
public:
   Match_Cr(const CODEREP *cr): _cr(cr) {}
   BOOL operator () (const CODEREP *candidate_cr) const
   {
      return (candidate_cr == _cr);
   }
};

class Match_Vn
{
private:
   VN       *_vn;
   VN_VALNUM _valnum;
public:
   Match_Vn(VN *vn, VN_VALNUM valnum): _vn(vn), _valnum(valnum) {}
   BOOL operator () (CODEREP *candidate_cr) const
   {
      return (_vn->expr_valnum(candidate_cr->Coderep_id()) == _valnum);
   }
};

template <class Match_test>
pair<INT32,CODEREP*> Count_occurs(CODEREP          *cr, 
				  const Match_test &matches,
				  BOOL              is_store_lhs)
{
   // This is modelled on VALNUM_FRE::Collect_cr_occurrences(), which is
   // defined below.  It counts all occurrences of a cr that is not a lhs
   // of a store, not a literal constant and not an OPR_PARM node.  It
   // returns two values: The number of real occurrences and a coderep
   // for one of those occurrences.  We return (0,NULL) when no occurrence
   // is found.
   //
   INT32                counter = 0;
   CODEREP             *occurs_cr = NULL;
   pair<INT32,CODEREP*> counted_occurs;
   
   // Set counter to 1 if we have a match; 
   // otherwise visit nested subexpressions
   //
   switch (cr->Kind())
   {
   case CK_LDA:
   case CK_VAR:
      if (matches(cr) && !is_store_lhs)
      {
	 counter = 1;
	 occurs_cr = cr;
      }
      break;

   case CK_CONST:
   case CK_RCONST:
      // We do not count literal occurrences, since we should never
      // replace such occurrences by temporaries or other literals 
      // in VNFRE.
      //
      break;
      
   case CK_IVAR:
      if (matches(cr) && cr->Opr() != OPR_PARM)
      {
	 counter = 1;
	 occurs_cr = cr;
      }
      else // not found
      {
	 CODEREP *size_cr = 
	    (is_store_lhs? cr->Mstore_size() : cr->Mload_size());
	 if (cr->Opr() == OPR_ILOADX)
	   size_cr = cr->Index();
	 CODEREP *base_cr =
	    (is_store_lhs? cr->Istr_base() : cr->Ilod_base());
	    
	 if (cr->Opr() == OPR_MLOAD || cr->Opr() == OPR_ILOADX)
	 {
	    counted_occurs = Count_occurs(size_cr, matches, FALSE);
	    counter = counted_occurs.first;
	    occurs_cr = counted_occurs.second;
	 }
	 counted_occurs = Count_occurs(base_cr, matches, FALSE);
	 counter += counted_occurs.first;
	 if (occurs_cr == NULL)
	    occurs_cr = counted_occurs.second;
      }
      break;

   case CK_OP:
      Is_True(cr->Opr() != OPR_ARRAY,
	      ("Count_occurs: found an OPR_ARRAY node; should be lowered"));
      
      if (matches(cr)            &&
	  cr->Opr() != OPR_PARM  &&
	  !OPERATOR_is_volatile(cr->Opr()))
      {
	 counter = 1;
	 occurs_cr = cr;
      }
      else // not found
      {
	 for (INT32 i=0; i<cr->Kid_count(); i++)
	 {
	    counted_occurs = Count_occurs(cr->Opnd(i), matches, FALSE);
	    counter += counted_occurs.first;
	    if (occurs_cr == NULL)
	       occurs_cr = counted_occurs.second;
	 }
      }
      break;

   default:
      Is_True(FALSE, ("Count_occurs: bad coderep kind"));
      break;
   }
   return pair<INT32,CODEREP*>(counter,occurs_cr);
} // Count_occurs


// ---------------------------------------------------------------------------
// -- This class encapsulates our algorithm and its functional decomposition -
// ---------------------------------------------------------------------------

class VALNUM_FRE
{
private:

   static VALNUM_FRE   *_current;
   ETABLE              *_etable;       // A specialized etable set up for vn
   COMP_UNIT           *_comp_unit;    // Compilation unit curently processed
   MEM_POOL            *_lpool;        // A pool we can push/pop as we like
   MEM_POOL            *_vpool;        // Popped between processing valnums
   MEM_POOL            *_gpool;        // A pool that is never pushed or popped
   BOOL                 _tracing;      // Tracing is activated

   VN                  *_vn;           // The result of a value numbering
   VALNUM_TO_EXPR_LIST *_vn_to_exprid; // Maps valnums to coderep ids
   VALNUM_TO_WORKLST    _vn_to_worklst;// Maps valnums to occurrence lists
   VN::BIT_VECTOR       _vn_removed;   // Occs may have moved out of stmts
   VN::BIT_VECTOR       _cr_removed;   // Occs may have moved out of stmts
   VN::BIT_VECTOR       _do_fre;       // True for each vn we apply vnfre to
   EXP_OCCURS_CONTAINER _exit_occurs;  // Exit occurrences

   // Phase numbers for various steps in _expression_redundancy_elimination():
   //
   INT32 _phi_placement_phase;
   INT32 _valnum_renaming_phase;
   INT32 _avail_insert_phase;
   INT32 _finalize_phase;
   INT32 _codemotion_phase;
   INT32 _ssa_min_phase;
   INT32 _worklst_prune_phase;
   INT32 _vnfre_misc_phase;
   INT32 _vnfre_delete_occurs_phase;
   INT32 _vnfre_ivc_phase;
   
   // Accessors.
   //
   typedef EXP_WORKLST * EXP_WORKLST_REF;

   BOOL _user_enabled(VN_VALNUM v)
   {
      return (v.ordinal() > WOPT_Enable_Vnfre_After &&
	      v.ordinal() < WOPT_Enable_Vnfre_Before);
   }
   
   EXP_WORKLST_REF &_worklst(VN_VALNUM v)
   {
      Is_True(v.ordinal() < _vn_to_worklst.size(),
	      ("Out of range access VALNUM_FRE::_worklst: (%d > %d)",
	      v.ordinal(), _vn_to_worklst.size()));
      return _vn_to_worklst[v.ordinal()];
   }

   const EXP_WORKLST_REF &_worklst(VN_VALNUM v) const 
   {
      Is_True(v.ordinal() < _vn_to_worklst.size(),
	      ("Out of range access VALNUM_FRE::_worklst: (%d > %d)",
	      v.ordinal(), _do_fre.size()));
      return _vn_to_worklst[v.ordinal()];
   }

   BOOL _cr_maybe_removed(VN::EXPRID id)
   {
      if (id < _cr_removed.size())
	 return BOOL(_cr_removed[id]);
      else
	 return FALSE;
   }      

   void _set_cr_maybe_removed(VN::EXPRID id, BOOL maybe_removed)
   {
      if (id >= _cr_removed.size())
	 _grow_exprid_maps(id);
      _cr_removed[id] = bool(maybe_removed);
   }      

   BOOL _vn_maybe_removed(VN_VALNUM v)
   {
      Is_True(v.ordinal() < _vn_removed.size(),
	     ("Out of range access VALNUM_FRE::_vn_maybe_removed: (%d > %d)",
	      v.ordinal(), _vn_removed.size()));
      return BOOL(_vn_removed[v.ordinal()]);
   }      

   void _set_vn_maybe_removed(VN_VALNUM v, BOOL maybe_removed)
   {
      Is_True(v.ordinal() < _vn_removed.size(),
	     ("Out of range VALNUM_FRE::_set_vn_maybe_removed: (%d > %d)",
	      v.ordinal(), _vn_removed.size()));
      _vn_removed[v.ordinal()] = bool(maybe_removed);
   }

   BOOL _do_vnfre(VN_VALNUM v) const 
   {
      Is_True(v.ordinal() < _do_fre.size(),
	     ("Out of range access VALNUM_FRE::_do_vnfre: (%d > %d)",
	      v.ordinal(), _do_fre.size()));
      return BOOL(_do_fre[v.ordinal()]);
   }

   void _set_do_vnfre(VN_VALNUM v, BOOL doit)
   {
      Is_True(v.ordinal() < _do_fre.size(),
	     ("Out of range access VALNUM_FRE::_do_vnfre: (%d > %d)",
	      v.ordinal(), _do_fre.size()));
      _do_fre[v.ordinal()] = bool(doit);
   }


   BOOL _is_dedicated_preg(CODEREP *cr)
   {
      Is_True(cr->Kind() == CK_VAR, 
	      ("Expected CK_VAR in VALNUM_FRE::_is_dedicated_preg"));
      
      return (_etable->Opt_stab()->
	      Aux_stab_entry(cr->Aux_id())->Is_dedicated_preg());
   }

   BOOL _is_real_var(CODEREP *cr)
   {
      Is_True(cr->Kind() == CK_VAR, 
	      ("Expected CK_VAR in VALNUM_FRE::_is_real_var"));
      
      return (_etable->Opt_stab()->
	      Aux_stab_entry(cr->Aux_id())->Is_real_var());
   }

   EXP_OCCURS *_first_real_occur(EXP_WORKLST *w) const 
   {
      return w->Real_occurs().Head();
   }

   EXP_OCCURS *_last_real_occur(EXP_WORKLST *w) const 
   {
      return w->Real_occurs().Tail();
   }

   BOOL _has_an_occur(EXP_WORKLST *w) const 
   {
      return w != NULL && _first_real_occur(w) != NULL;
   }

   BOOL _num_occurs(EXP_WORKLST *w) const 
   {
      INT32 i = 0;
      EXP_OCCURS *    occ;
      EXP_OCCURS_ITER occ_iter;
      FOR_ALL_NODE(occ, occ_iter, Init(_first_real_occur(w)))
      {
	 i += (occ->Mult_real()? 2 : 1);
      }
      return i;
   }

   BOOL _is_fully_avail(EXP_PHI *value_phi) const
   {
      return !value_phi->Not_user_avail();
   }

   EXP_OCCURS *_def_occur(EXP_OCCURS *occ) const
   {
      // Specialized version of EXP_OCCURS::Def_occur(), which avoids 
      // assertion when applied to PHI occurrences.
      //
      return (occ->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR?
	      occ->Def_occur() : NULL);
   }

   CODEREP * _get_occur_cr(const EXP_OCCURS *occur) const;

   // Tracing.
   //
   void _trace_header();
   
   // Memory allocation
   //
   void _grow_exprid_maps(VN::EXPRID id);
   void _grow_valnum_maps(VN_VALNUM  v);

   // Algorithm to select and sort the value numbers we care about.
   //
   BOOL _disabled_expr(const VN_EXPR::CONST_PTR vexpr) const;
   BOOL _has_valid_stmtrep_occurrence(VN_VALNUM v);
   BOOL _may_be_redundant_expr(VN_VALNUM v, VN_EXPR::CONST_PTR vexpr);
   void _select_for_valnum_list(VN_VALNUM          v, 
				VN::BIT_VECTOR    &visited,
				VN::VALNUM_VECTOR &valnum_list);
   void _select_and_sort_valnums(VN::VALNUM_VECTOR &valnum_list);

   // Algorithm to find all occurrences of each value number and put them
   // into occurrence lists.
   //
   BOOL _contains_undef_val(CODEREP  *cr, STMTREP *stmt) const;
   BOOL _subsumable_by_branch(VN_VALNUM valnum, 
			      STMTREP  *stmt, 
			      CODEREP  *cr) const;

   EXP_OCCURS *_create_real_occurrence(CODEREP *cr,
				       STMTREP *stmt, 
				       INT      stmt_kid_num,
				       UINT     depth);

   EXP_OCCURS *_copy_real_occurrence(EXP_OCCURS *occ)
   {
      return _create_real_occurrence(occ->Occurrence(),
				     occ->Enclosed_in_stmt(),
				     occ->Stmt_kid_num(),
				     occ->Rehash_cost());
   }

   void _append_real_occurrence(CODEREP *cr,
				STMTREP *stmt, 
				INT      stmt_kid_num,
				UINT     depth);

   void _append_exit_occurrence(BB_NODE *bb);

   void _insert_a_cr_occurrence(CODEREP *cr,
				STMTREP *stmt, 
				INT      stmt_kid_num,
				UINT     depth);
   void _collect_all_real_occurrences();
   void _verify_and_remove_occurs(EXP_WORKLST *worklist,
				  VN_VALNUM    valnum);
   
   // Characteristics of a worklist, beyond the inlined functions above.
   //
   BOOL _all_same_occurs(CODEREP       *cr, 
			 BOOL           lhs_of_store,
			 VN_VALNUM      valnum,
			 const CODEREP *occ_cr) const;
   BOOL _same_var_occurs(EXP_OCCURS    *occ,
			 const CODEREP *var_cr) const;
   void _get_worklist_info(EXP_WORKLST *w,
			   BOOL        &same_var_occurs,
			   BOOL        &save_use_occurs,
			   BOOL        &disabled_occurs) const;
   BOOL _is_vnfre_candidate(EXP_WORKLST *w) const;

   // Algorithm to find all phi and phi predecessors and put them
   // into occurrence lists.
   //
   EXP_OCCURS *_append_phi_occurrence(EXP_PHI     *phi, 
				      EXP_WORKLST *worklist);
   EXP_OCCURS *_append_phi_pred_occurrence(BB_NODE     *bb, 
					   EXP_WORKLST *worklist);
   void        _insert_valnum_phi(EXP_WORKLST *worklist);

   // Coalescing of induction vars.
   //
   pair<CODEREP*,STMTREP*> _save_to_temp(BB_NODE *in_bb,
					 STMTREP *after_stmt,
					 BOOL     when_not_after_append,
					 CODEREP *rhs);
   CODEREP* _ivc_insert_initval_diff(BB_NODE *init_bb,
				     CODEREP *init_cr,
				     CODEREP *base_cr);
   void _remove_ivc_incr_occurs(PHI_NODE *phi);
   void _ivc_substitute(BB_NODE        *loop_header,
			STMTREP        *base_stmt,
			CODEREP        *base_cr,
			VN_IVC         &vn_ivc,
			EQCLASS_MEMBER &base,
			EQCLASS_MEMBER &memb);
   void _ivc_classify(BB_NODE *loop_header, VN_IVC &vn_ivc);
   void _ivc_coalesce(BB_NODE *loop_header, VN_IVC &vn_ivc);
   void _ivc();
   
   // Renaming algorithm, which assigns version numbers to occurrences
   // and initializes the phi operands (phi_expr->Opnd()).
   //
   void _create_new_version(EXP_OCCURS   *occur, 
			    OCCURS_STACK &occurs_stack,
			    EXP_WORKLST  &worklist);
   void _rename_valnums(EXP_WORKLST &worklist, BOOL &renamed_ok);

   // Setting and propagation of flags, including the _is_fully_avail() flag.
   //
   void _propagate_flags(EXP_WORKLST &worklist, BOOL &found_redundancy);

   // Redundancy elimination
   //
   void _remove_redundant_phi_nodes();
   void _substitute_literal(VN_VALNUM v);
   void _expression_redundancy_elimination(VN_VALNUM v);

   // Callback helper routines
   //
   void _remove_nested_occurs(const CODEREP  *enclosing_cr, 
			      VN_VALNUM       remove_valnum,
			      BOOL            is_istr_lhs,
			      BOOL            within_valnum);
   
   // bug fix for OSP_101 & & OSP_103 & OSP_117 & OSP_189
   //
   BOOL _check_cr_compatible (VN_VALNUM valnum);
   
#ifdef KEY // bug 9114              
  BOOL _ivc_loop_variant(BB_NODE *, VN_VALNUM);
#endif                          

public:

   static VALNUM_FRE *Current() {return _current;}
   static void        Set_current(VALNUM_FRE *fre) {_current = fre;}

   // Initializes a context for the algorithm
   //
   VALNUM_FRE(VN &vn, ETABLE &etable, COMP_UNIT *comp_unit):
      _etable(&etable),
      _comp_unit(comp_unit),
      _lpool(etable.Etable_local_pool()),
      _vpool(etable.Per_expr_pool()),
      _gpool(etable.Etable_pool()),
      _tracing(etable.Tracing()),
      _vn(&vn), 
      _vn_to_exprid(NULL),
      _vn_to_worklst(vn.last_valnum().ordinal()+1,
		     (EXP_WORKLST *) NULL,
		     VALNUM_TO_WORKLST::allocator_type(_gpool)),
      _vn_removed(vn.last_valnum().ordinal()+1,
		  bool(FALSE), 
		  VN::BVECTOR_ALLOCATOR(_gpool)),
      _cr_removed(comp_unit->Htable()->Coderep_id_cnt(),
		  bool(FALSE), 
		  VN::BVECTOR_ALLOCATOR(_gpool)),
      _do_fre(vn.last_valnum().ordinal()+1,
	      bool(FALSE), 
	      VN::BVECTOR_ALLOCATOR(_gpool)),
      _exit_occurs(),
      _phi_placement_phase(0),
      _valnum_renaming_phase(0),
      _avail_insert_phase(0),
      _finalize_phase(0),
      _codemotion_phase(0),
      _ssa_min_phase(0),
      _worklst_prune_phase(0),
      _vnfre_misc_phase(0),
      _vnfre_delete_occurs_phase(0),
      _vnfre_ivc_phase(0)
   {}

   // The full algorithm.
   //
   void apply();

   // Inquires and updates that may occur during a call to "apply()".
   //
   VN::EXPRID last_exprid() const 
   {
      return _vn->last_exprid();
   }

   VN_VALNUM last_valnum() const 
   {
      return _vn->last_valnum();
   }
   
   VN_VALNUM get_valnum(VN::EXPRID id) const
   {
      return _vn->expr_valnum(id);
   }

   EXP_WORKLST *get_worklst(VN_VALNUM valnum) const 
   {
      return _worklst(valnum);
   }
   
   INT32 num_real_occurs(VN_VALNUM valnum) const 
   {
      return _num_occurs(get_worklst(valnum));
   }
   
   VN_VALNUM compute_valnum(const CODEREP *cr)
   {
      VN_VALNUM v;
 
      if (cr->Coderep_id() > last_exprid() ||
	  get_valnum(cr->Coderep_id()).is_bottom())
      {
	 v = _vn->invent_unique_valnum();
	 _grow_valnum_maps(v);
	 VNFRE::add_valnum(cr, v.ordinal());
      }
      else
      {
	 v = get_valnum(cr->Coderep_id());
      }
      return v;
   }

   void new_cr(const CODEREP *cr, VN_VALNUM valnum)
   {
      // Map a new coderep, outside of our range of ids, to the given value
      // number.  Note that _vn_to_exprid should not be "live" at this point.
      //
      Is_True( /* _vn_to_exprid == NULL && */  // for bug 9114
	      cr->Coderep_id() > last_exprid(),
	      ("Illegal call to VALNUM_FRE::new_cr()"));
      _vn->new_cr(cr, valnum);
   }

   void reset_valnum(const CODEREP *cr, VN_VALNUM valnum);
  
   void replace_cr_in_stmt(const CODEREP *old_cr, 
			   CODEREP       *new_cr, 
			   const STMTREP *stmt);

   void move_rhs_occurs(const STMTREP *old_stmt,
			STMTREP       *new_stmt);

   void delete_all_occurs(const EXP_OCCURS *occur, 
			  const CODEREP    *cr);

   void insert_cr_occurrences(CODEREP *cr,
			      STMTREP *stmt, 
			      INT      stmt_kid_num, 
			      BOOL     is_store,
			      UINT     depth);
   void collect_cr_occurrences(CODEREP *cr,
			       STMTREP *stmt, 
			       INT      stmt_kid_num, 
			       BOOL     is_store,
			       UINT     depth);

}; // class VALNUM_FRE


// ---------------------------------------------------------------------------
// ---------- Definition of static member variables and subroutines ----------
// ---------------------------------------------------------------------------

VALNUM_FRE *VALNUM_FRE::_current = NULL;

// ---------------------------------------------------------------------------
// --------- Definition of function objects used in traversal of cfg ---------
// ---------------------------------------------------------------------------

class INSERT_CR_OCCURS
{
   VALNUM_FRE *_vnfre;
public:
   INSERT_CR_OCCURS(VALNUM_FRE *vnfre) : _vnfre(vnfre) {}
   void operator () (CODEREP *cr, STMTREP *stmt, INT32 kidno)
   {
      _vnfre->insert_cr_occurrences(cr, stmt, kidno, 
				   (stmt->Lhs() == cr &&
				    OPCODE_is_store(stmt->Op())),
				   0/*recursion depth*/);
   }
}; // class INSERT_CR_OCCURS


class COLLECT_CR_OCCURS
{
   VALNUM_FRE *_vnfre;
public:
   COLLECT_CR_OCCURS(VALNUM_FRE *vnfre) : _vnfre(vnfre) {}
   void operator () (CODEREP *cr, STMTREP *stmt, INT32 kidno)
   {
      _vnfre->collect_cr_occurrences(cr, stmt, kidno, 
				    (stmt->Lhs() == cr &&
				     OPCODE_is_store(stmt->Op())),
				    0/*recursion depth*/);
   }
}; // class COLLECT_CR_OCCURS


class GET_NUM_OCCURS
{
   VALNUM_FRE *_vnfre;
public:
   GET_NUM_OCCURS(VALNUM_FRE *vnfre) : _vnfre(vnfre) {}
   INT32 operator () (VN_VALNUM valnum)
   {
      return _vnfre->num_real_occurs(valnum);
   }
}; // class GET_NUM_OCCURS


// ---------------------------------------------------------------------------
// ----- Implementation of methods in the public interface to VALNUM_FRE -----
// ---------------------------------------------------------------------------

void 
VALNUM_FRE::apply(void)
{
   // We use "lpool" for any temporary data allocated during the course
   // of this algorithm and "gpool" for stuff that needs to survive
   // beyond the termination of the algorithm (if anything does).
   //
   _trace_header();

   OPT_POOL_Push(_lpool, -1);
   {
      INT32 i;
      
      SET_OPT_REPEAT_PHASE(_vnfre_misc_phase, "VNFRE: miscellaneous");

      // Step1: Eliminate redundant phi nodes, i.e. phi nodes with the same
      // value number as other phi nodes in the same basic block, and insert 
      // assignments that copy a phi result into another phi result variable.
      // Such removed phi result variables will be replaced by the zero 
      // version.
      //
      _remove_redundant_phi_nodes();
   
      // Step2: Identify the value numbers that are candidates for VNFRE, 
      // and sort them in order of increasing complexity of value number
      // expression.  This sets up a valnum_list of value numbers such that
      // when it is traversed in reverse order we will visit the most complex
      // value-numbering expressions before any of its subexpressions.  Note
      // that this subexpression partial ordering on value-numbering 
      // expressions will not always have a corresponding subexpression 
      // relation in the associated lexical expressions.  We also create
      // a bit-vector as a quick way to check if we intend to do vnfre
      // on a given value number.
      //
      VN::VALNUM_VECTOR valnum_list(0,
				    VN_VALNUM::Bottom(),
				    VN::VALNUM_VECTOR::allocator_type(_lpool));
      valnum_list.reserve(_vn->last_valnum().ordinal()+1);
#ifdef KEY // bug 9114 : this code moved from inside _select_and_sort_valnums
      VALNUM_TO_EXPR_LIST vn_to_exprid(*_vn, _lpool);
      _vn_to_exprid = &vn_to_exprid; // Set temporary state for algorithm
#endif
      _select_and_sort_valnums(valnum_list);

      // Step 3: Set up the table of worklists of real occurrences for each
      // value number for which we really will apply our vnfre algorithm.
      // This will also append any exit occurrences to _exit_occurs and set
      // the stmt_id() attribute of STMTREPs.
      //
      _exit_occurs.Init();
      _collect_all_real_occurrences();

      // Eliminate value numbers without any real occurrences from further
      // consideration.
      //
      VNFRE_TRACE(stderr, "Valnums considered = {");
      for (i = valnum_list.size() - 1; i >= 0; i--)
      {
	 const VN_VALNUM v = valnum_list[i];
	 const BOOL      has_occ = _has_an_occur(_worklst(v));
	 
	 VNFRE_TRACE(stderr, "%d[%c] ", v.ordinal(), (has_occ? 'y' : 'n'));
	 _set_do_vnfre(v, has_occ);
      }
      VNFRE_TRACE(stderr, "}\n");
      
      // For every loop, try to coalesce the index variables such that
      // references to identical or similar index variables are replaced by
      // a "chosen" index variable.
      //
      SET_OPT_REPEAT_PHASE(_vnfre_ivc_phase, "VNFRE: ivc");
      _ivc();
#ifdef KEY // bug 9114 : this code moved from inside _select_and_sort_valnums
      _vn_to_exprid = NULL; // Reset temporary state after algorithm
#endif
      SET_OPT_REPEAT_PHASE(_vnfre_misc_phase, "VNFRE: miscellaneous");

      // Remaining steps: For each value number, one at a time, apply
      // the algorithm to remove full redundancies.
      //
      for (i = valnum_list.size() - 1; i >= 0; i--)
      {
	 const VN_VALNUM valnum = valnum_list[i];
	 
	 // Reuse temporary memory for each value number we process.
	 //
	 OPT_POOL_Push(_lpool, -1);
	 OPT_POOL_Push(_vpool, -1);
	 
	 // bug fix for OSP_101 & OSP_103 $ OSP_117 & OSP_189		
	 //
	 if (_check_cr_compatible(valnum)) {
	   _set_do_vnfre(valnum, FALSE);
	   continue;
	 }
	 if (_do_vnfre(valnum))
	 {
	    EXP_WORKLST *worklst = _worklst(valnum);

	    if (_vn_maybe_removed(valnum))
	    {
	       SET_OPT_REPEAT_PHASE(_vnfre_delete_occurs_phase,
				    "VNFRE: delete occurs");
	       _verify_and_remove_occurs(worklst, valnum);
	       SET_OPT_REPEAT_PHASE(_vnfre_misc_phase, "VNFRE: miscellaneous");
	    }
	    
	    const BOOL   is_vnfre_candidate = _is_vnfre_candidate(worklst);
	    const BOOL   is_const_prop_candidate = 
	       (_vn->valnum_expr(valnum) != NULL &&
		_vn->valnum_expr(valnum)->get_kind() == VN_EXPR::LITERAL &&
		_has_an_occur(worklst));

	    // We are now committed to deal with this value number, and
	    // we need no longer contend with it for future processing.
	    //
	    _set_do_vnfre(valnum, FALSE);

	    if (!_user_enabled(valnum))
	    {
	       if (is_const_prop_candidate || is_vnfre_candidate)
		  DevWarn("VNFRE: skip valnum %d", (int)valnum.ordinal());
	    }
	    else if (is_const_prop_candidate)
	    {
	       // Step3: Substitute all expressions that refer to this literal 
	       // value, and that are not already literal expressions, with the
	       // literal value, using the occurrence list.
	       //
	       VNFRE_TRACE(stderr, "CONST_PROP[%d]\n", valnum.ordinal());
	       _substitute_literal(valnum);
	    }
	    else if (is_vnfre_candidate)
	    {
	       // Steps 4-8: performs value numbered expression redundancy 
	       // elimination, in a fashion similar to the way we perform 
	       // lexical expression redundancy elimination (EPRE).
	       //
	       VNFRE_TRACE(stderr, "REDUN_ELIM[%d] %s\n",
			   valnum.ordinal(), 
			   (_vn->valnum_expr(valnum) != NULL?
			    "expr" : "chi/phi"));
	       _expression_redundancy_elimination(valnum);
	    }
	    else
	    {
	       // These looked like candidates in _select_and_sort_valnums(),
	       // but turned out to be either lvalue candidates or were
	       // eliminated as candidates due to second order effects of
	       // the processing of other value numbers.
	       //
	       VNFRE_TRACE(stderr, "IGNORED[%d]\n", valnum.ordinal());
	    }

	    // Done with this value number, so we reset occurrence list 
	    // for this value number.
	    //
	    CXX_DELETE(_worklst(valnum), _gpool);
	    _worklst(valnum) = NULL;
	 }
	 OPT_POOL_Pop(_lpool, -1);
	 OPT_POOL_Pop(_vpool, -1);
      }
   }
   OPT_POOL_Pop(_lpool, -1);
} // VALNUM_FRE::apply


void 
VALNUM_FRE::reset_valnum(const CODEREP *cr, VN_VALNUM to_valnum)
{
   // This indicates that the coderep has been reused and overwritten,
   // but that it still has the same Coderep_id().  Move occurrences
   // from one worklist to another.
   //
   const VN_VALNUM from_valnum = get_valnum(cr->Coderep_id());
   EXP_WORKLST    *from_worklist = _worklst(from_valnum);

   Is_True( /* _vn_to_exprid == NULL && */ // for bug 9114
	   from_valnum != to_valnum,
	   ("Illegal call to VALNUM_FRE::reset_valnum()"));

   // Change the cr->vn mapping, remove the cr from the old_valnum->cr
   // mapping, and add it into the valnum->cr mapping.
   //
   _vn->reset_valnum(cr, to_valnum);

   // Move all cr occurrences from the occurrence list of its previous
   // value number (if valid) to the occurrence list for its new value
   // number (if it is yet to be processed).
   //
   if (from_worklist != NULL &&
       !(from_valnum.is_bottom() || from_valnum.is_top()))
   {
      // We do not actually remove occurrences from the worklist, but instead
      // we indicate that the worklist needs to be verified before processing
      // it.
      //
      _set_vn_maybe_removed(from_valnum, TRUE);

      // Next, we need to traverse the worklist and insert new occurrences
      // into the worklist for the new value number of the coderep.
      //
      if (_do_vnfre(to_valnum))  // Yet to be processed
      {
	 EXP_WORKLST     *to_worklist = _worklst(to_valnum);
	 EXP_OCCURS      *occ;
	 EXP_OCCURS_ITER  occ_iter;
	 FOR_ALL_NODE(occ, occ_iter, Init(from_worklist->Real_occurs().Head()))
	 {
	    EXP_OCCURS *tail_occ = to_worklist->Real_occurs().Tail();
	    pair<INT32,CODEREP*> counted_occurs = 
	       Count_occurs(_get_occur_cr(occ), Match_Cr(cr), FALSE/*lhs*/);

	    if (counted_occurs.first > 0)
	    {
	       EXP_OCCURS *new_occ = _copy_real_occurrence(occ);
	       
	       if (counted_occurs.first > 1)
		  new_occ->Set_mult_real();
	       else
		  new_occ->Reset_mult_real();
	       
	       if (tail_occ == NULL || tail_occ->Is_DPO_less_than(new_occ)) 
		  to_worklist->Append_occurrence(new_occ);
	       else
		  to_worklist->Insert_occurrence(new_occ, _etable);
	    }
	 }
      }
   }
} // VALNUM_FRE::reset_valnum


void 
VALNUM_FRE::replace_cr_in_stmt(const CODEREP *old_cr, 
			       CODEREP       *new_cr, 
			       const STMTREP *stmt)
{
   // Replaces all occurrences of the form <old_cr, stmt> with 
   // <new_cr, stmt>, assuming the two codereps have identical 
   // value numbers. When all occurrences of an old_cr is replaced
   // with another new_cr, this routine should be called.  When
   // enclosing expression codereps are rehashed, this routine must 
   // be called again for each such enclosing expression (since all
   // of them must be replaced if any one subexpression is replaced).
   //
   VN_VALNUM old_valnum = get_valnum(old_cr->Coderep_id());

   Is_True(old_cr != new_cr && old_valnum == get_valnum(new_cr->Coderep_id()), 
	   ("Illegal call to VALNUM_FRE::replace_cr_in_stmt()"));
   
   // Update occurrence lists, if they are of any significance for future
   // application of vnfre.  Do we really need to do this?  Yes, the
   // coderep referenced by an occurrence for a statement must be a
   // valid subtree of the statement, since such codereps may be saved
   // into temporaries as a result of code-motion and should always reflect
   // a coderep actually referenced by the statement.
   //
   if (_do_vnfre(old_valnum))  // Yet to be processed
   {
      EXP_WORKLST     *worklist = _worklst(old_valnum);
      EXP_OCCURS      *occ;
      EXP_OCCURS_ITER  occ_iter;
      FOR_ALL_NODE(occ, occ_iter, Init(worklist->Real_occurs().Head()))
      {
	 if (occ->Occurrence() == old_cr && occ->Stmt() == stmt)
	    occ->Set_occurrence(new_cr);
      }
   }   
} // VALNUM_FRE::replace_cr_in_stmt


void 
VALNUM_FRE::move_rhs_occurs(const STMTREP *old_stmt,
			    STMTREP       *new_stmt)
{
   // Walk through every occurrence list yet to be processed, and wherever
   // there is a rhs occurrence w.r.t. old_stmt, update the stmt to instead
   // refer to new_stmt.  Such an occurrence (occ) is identified by 
   // occ->Stmt() == old_stmt and occ->Stmt_kid_num() == 0.
   //
   const VN_VALNUM num_valnums = VN_VALNUM::Next(_vn->last_valnum());

   Is_True(OPCODE_is_store(old_stmt->Op()),
	   ("Unexpected operand to VALNUM_FRE::move_rhs_occurs()"));
   
   for (VN_VALNUM v = VN_VALNUM::First(); 
	v != num_valnums; 
	v = VN_VALNUM::Next(v))
   {
      EXP_WORKLST     *worklist = _worklst(v);
      EXP_OCCURS      *occ;
      EXP_OCCURS_ITER  occ_iter;
      FOR_ALL_NODE(occ, occ_iter, Init(worklist->Real_occurs().Head()))
      {
	 if (occ->Stmt() == old_stmt && occ->Stmt_kid_num() == 0)
	    occ->Set_enclose_stmt(new_stmt);
      }
   }
} // VALNUM_FRE::move_rhs_occurs


void 
VALNUM_FRE::delete_all_occurs(const EXP_OCCURS *occur, 
			      const CODEREP    *cr)
{
   // Update the _cr_maybe_removed and _vn_maybe_removed maps to reflect
   // that occurrences may be removed in worklists yet to be processed.
   // The actual removal happens when we are about to process a worklist,
   // by means of _verify_and_remove_occurs().
   //
   // The codereps that we care about are all occurrences with the same
   // value number as occur->Occurrence().  Codereps nested within such
   // occurrences may be in a worklist yet to be processed, so we need to
   // mark them as "maybe_removed".
   //
   STMTREP * const stmt = occur->Stmt();
   const VN_VALNUM valnum = get_valnum(occur->Occurrence()->Coderep_id());

   if ((OPCODE_operator(stmt->Op()) == OPR_MSTORE ||
	OPCODE_operator(stmt->Op()) == OPR_ISTORE ||
	OPCODE_operator(stmt->Op()) == OPR_ISTOREX) &&
       cr->Coderep_id() == stmt->Lhs()->Coderep_id())
      _remove_nested_occurs(cr, valnum, 
			    TRUE/*istr_lhs*/, FALSE/*in_occurrence*/);
   else
      _remove_nested_occurs(cr, valnum, 
			    FALSE/*istr_lhs*/, FALSE/*in_occurrence*/);
} // VALNUM_FRE::delete_all_occurs


// ---------------------------------------------------------------------------
// -------------------------- Helper routines  -------------------------------
// ---------------------------------------------------------------------------

CODEREP  *
VALNUM_FRE::_get_occur_cr(const EXP_OCCURS *occur) const
{
   STMTREP  * const stmt = occur->Stmt();	
   const INT32      kid_num = occur->Stmt_kid_num();	  
   CODEREP  *       cr = NULL;

   // Get the enclosing coderep that contains this occurrence.
   //
   if (OPCODE_is_fake(stmt->Op()))
      cr = stmt->Rhs()->Opnd(kid_num);
   else if (OPCODE_is_store(stmt->Op()))
      switch (kid_num)
      {
      case 0:
	 cr = stmt->Rhs();
	 break;
      case 1:
	 cr = stmt->Lhs()->Istr_base();  // Lhs() must be CK_IVAR
	 break;
      case 3:
	 cr = stmt->Lhs()->Mstore_size(); // stmt must be OPR_MSTORE
	 break;
      default:
	 Is_True(FALSE, 
		 ("VALNUM_FRE::_get_occur_cr: bad stmt_kid_num"));
	 break;
      }
   else if (OPCODE_operator(stmt->Op()) == OPR_PREFETCH)
      cr = stmt->Rhs()->Ilod_base();
   else
      cr = stmt->Rhs();

   return cr;
} // VALNUM_FRE::_get_occur_cr


void
VALNUM_FRE::_remove_nested_occurs(const CODEREP  *enclosing_cr, 
				  VN_VALNUM       remove_valnum,
				  BOOL            is_istr_lhs,
				  BOOL            within_valnum)
{
   // Traverse the enclosing_cr and all its nested subexpressions.  Once
   // we reach an expression of the desired valnum (remove_valnum), we
   // mark the nested codereps (yet to be processed by vnfre) and their
   // associated valnum as "removed".  This will give us a set of value
   // numbers that may have moved from one occurrence to another in terms
   // of _vn_maybe_removed().
   //
   // Note that we need never traverse a coderep for which _cr_maybe_removed()
   // is TRUE, since all subexpressions of such codereps should have already
   // been marked as _vn_maybe_removed()/_cr_maybe_removed().
   //
   if (!_cr_maybe_removed(enclosing_cr->Coderep_id()))
   {
      VN_VALNUM valnum = get_valnum(enclosing_cr->Coderep_id());
   
      if (within_valnum)
      {
	 if (_do_vnfre(valnum))
	 {
	    // Mark for removal.
	    //
	    _set_vn_maybe_removed(valnum, TRUE);
	    _set_cr_maybe_removed(enclosing_cr->Coderep_id(), TRUE);
	 }
      }
      else if (!is_istr_lhs && valnum == remove_valnum)
      {
	 // The subexpressions nested within the matching expression will
	 // be removed from some occurrences.  We do not care about the
	 // current expression, since need never update occurrence lists
	 // for it again.
	 //
	 // We never remove the lvalue in a lhs of a store.
	 //
	 within_valnum = TRUE;
      }

      // Visit nested subexpressions
      //
      switch (enclosing_cr->Kind())
      {
      case CK_LDA:
      case CK_VAR:
      case CK_CONST:
      case CK_RCONST:
	 break;
	 
      case CK_IVAR:
	 if (enclosing_cr->Opr() == OPR_MLOAD)
	    _remove_nested_occurs((is_istr_lhs? 
				   enclosing_cr->Mstore_size():
				   enclosing_cr->Mload_size()),
				  remove_valnum,
				  FALSE, // is_istr_lhs
				  within_valnum);
	 else if (enclosing_cr->Opr() == OPR_ILOADX)
	    _remove_nested_occurs(enclosing_cr->Index(),
				  remove_valnum,
				  FALSE, // is_istr_lhs
				  within_valnum);

	 _remove_nested_occurs((is_istr_lhs? 
				enclosing_cr->Istr_base():
				enclosing_cr->Ilod_base()),
			       remove_valnum,
			       FALSE, // is_istr_lhsw
			       within_valnum);
	 break;
	 
      case CK_OP:
	 Is_True(enclosing_cr->Opr() != OPR_ARRAY,
	      ("VNFRE::_remove_nested_occurs: an OPR_ARRAY node,"
	       " this is a bug in the lowering process"));
	 {
	    for (INT32 i=0; i < enclosing_cr->Kid_count(); i++)
	       _remove_nested_occurs(enclosing_cr->Opnd(i),
				     remove_valnum,
				     FALSE, // is_istr_lhs
				     within_valnum);
	 }
	 break;

      default:
	 Is_True(FALSE,
		 ("VALNUM_FRE::_remove_nested_occurs: bad coderep kind"));
	 break;
      }
   }
} // VALNUM_FRE::_remove_nested_occurs


// bug fix for OSP_101 & OSP_103 & OSP_117 & OSP_189
// check the compatibility of codereps which are mapped to the same value number,
// they are not compatible, if Dtyp() and Dsctyp() of one coderep are all MTYPE_B,
// while there existing coderep whose Dtyp() and Dsctyp() are all not MTYPE_B
// TODO: Is it better to do not map these uncompatible codereps to the same value number?
//
BOOL
VALNUM_FRE::_check_cr_compatible (VN_VALNUM valnum)
{
   BOOL has_uncompatible_cr = FALSE;
   BOOL mtype_bool = FALSE;
   BOOL mtype_non_bool = FALSE;

   EXP_WORKLST *worklist = _worklst(valnum);
   EXP_OCCURS  *occ;
   EXP_OCCURS_ITER  occ_iter;

   if (worklist == NULL)
     return has_uncompatible_cr;
   FOR_ALL_NODE(occ, occ_iter, Init(worklist->Real_occurs().Head()))
   {
     CODEREP *cr = occ->Occurrence();
     if (cr->Dtyp() == MTYPE_B && cr->Dsctyp() == MTYPE_B)
       mtype_bool = TRUE;
     else if (cr->Dtyp() != MTYPE_B && cr->Dsctyp() != MTYPE_B)
     //else if (cr->Dtyp() == MTYPE_I4 && cr->Dsctyp() == MTYPE_I4)
       mtype_non_bool = TRUE;

     if (mtype_bool == TRUE && mtype_non_bool == TRUE) {
       has_uncompatible_cr = TRUE;
     }
   }
   
   return has_uncompatible_cr;
}


// ---------------------------------------------------------------------------
// ------------------------- Memory allocation stuff -------------------------
// ---------------------------------------------------------------------------

template <class ETYPE, class ALLOCATOR>
void VALNUM_FRE_grow_vector(vector<ETYPE,ALLOCATOR> &vec, 
			    ETYPE                    init_val, 
			    UINT32                   to_size)
{
   // Grow vectors to the given size, initializing the elements with
   // the given init_val.
   //
   if (vec.capacity() < to_size)
   {
      UINT32 growth_incr = vec.capacity()/3;

      if (growth_incr == 0)
	 growth_incr = 64;
 
      const UINT32 growth_factor = 1 + (to_size - vec.capacity())/growth_incr;
   
      vec.reserve(vec.capacity() + growth_factor*growth_incr + 1);
   }
   while (to_size > vec.size())
   {
      vec.push_back(init_val);
   }
} // VALNUM_FRE_grow_vector


void
VALNUM_FRE::_grow_exprid_maps(VN::EXPRID id)
{
   // Grow maps indexed by EXPRID to encompass the given id number.
   //
   VALNUM_FRE_grow_vector(_cr_removed, bool(FALSE), (UINT32)id+1);
}


void
VALNUM_FRE::_grow_valnum_maps(VN_VALNUM v)
{
#ifndef KEY // bug 9114
   Is_True(_vn_to_exprid == NULL, 
	   ("Unexpected map in VALNUM_FRE::_grow_valnum_maps()"));
#endif

   VALNUM_FRE_grow_vector(_vn_to_worklst,
			  (EXP_WORKLST*)NULL, (UINT32)v.ordinal()+1);
   VALNUM_FRE_grow_vector(_vn_removed,
			  bool(FALSE), (UINT32)v.ordinal()+1);
   VALNUM_FRE_grow_vector(_do_fre,
			  bool(FALSE), (UINT32)v.ordinal()+1);
}


// ---------------------------------------------------------------------------
// --- The Algorithm for selecting the value numbers we are concerned with ---
// ---------------------------------------------------------------------------

inline BOOL
VALNUM_FRE::_disabled_expr(const VN_EXPR::CONST_PTR vexpr) const
{
   // We currently disable all CSE of LDA and MLOAD expressions,
   // due to relocation and the lack of mtype pregs.  TODO: handle LDAs
   // of non-text symbols, and have EXP_WORKLST::Generate_save_reload()
   // handle mtypes.
   //
   return (vexpr != NULL &&
	   (vexpr->get_kind() == VN_EXPR::LDA_ADDR ||
	    (vexpr->get_kind() == VN_EXPR::MEMLOC &&
	     vexpr->get_dsctype() == MTYPE_M)));
} // VALNUM_FRE::_disabled_expr

	   
BOOL
VALNUM_FRE::_has_valid_stmtrep_occurrence(VN_VALNUM v)
{
   // TRUE iff the value number is assocated with at least one valid
   // statement occurrence and the value numbering expression is not
   // of a kind we have disabled.  No need to check for liveness, since all
   // statements in the valnum->stmt mapping should be live!
   //
   // Note that this may be FALSE, since a value numbering excludes all
   // IVAR and VAR lvalue codereps, as well as all CONST and RCONST 
   // codereps (literal value number expression) from the CODEREP-->STMTREP
   // mapping.  Hence we may have empty STMTREP lists!
   //
   BOOL                               found = FALSE;
   VALNUM_TO_EXPR_LIST::EXPR_ITERATOR expr_end(_vn_to_exprid->end(v));

   for (VALNUM_TO_EXPR_LIST::EXPR_ITERATOR expr_itr(_vn_to_exprid->begin(v));
	!found && expr_itr != expr_end;
	expr_itr++)
   {
      found = !_vn->expr_stmts(*expr_itr)->empty();
   }
   return found;
} // VALNUM_FRE::_has_valid_stmtrep_occurrence


BOOL
VALNUM_FRE::_may_be_redundant_expr(VN_VALNUM v, VN_EXPR::CONST_PTR vexpr)
{
   // True iff the value number is associated with more than one 
   // coderep; or it is associated with exactly one non-literal coderep
   // while the value numbering expression is VN_EXPR::LITERAL or
   // VN_EXPR::INTR_OP.
   //
   const UINT32 num_codereps = _vn_to_exprid->size(v);
   const BOOL   is_disabled = _disabled_expr(vexpr);
   BOOL         may_be_redundant = !is_disabled && num_codereps > 1;

   if (vexpr != NULL && !is_disabled && num_codereps == 1)
   {
      // ASSERT: may_be_redundant == FALSE
      //
      if (vexpr->get_kind() == VN_EXPR::LITERAL)
      {
	 // This is one exception to the rule.
	 //
	 CODEREP *cr = _vn->expr_cr(_vn_to_exprid->front(v));

	 if (cr->Kind() != CK_CONST && cr->Kind() != CK_RCONST)
	    may_be_redundant = TRUE;
      }
      else if (vexpr->get_kind() == VN_EXPR::INTR_OP)
      {
	 // This is another exception to the rule (We handle these
	 // here, since EPRE does not deal with them when they do not
	 // appear as leaf expressions.
	 //
	 may_be_redundant = TRUE;
      }
      else if (VN_IVC::Is_Induction_Var(_vn, v))
      {
	 // This is another exception to the rule, which will be handled
	 // later by IVC.
	 //
	 may_be_redundant = TRUE;
      }
   }
   return may_be_redundant;
} // VALNUM_FRE::_may_be_redundant_expr


void
VALNUM_FRE::_select_for_valnum_list(VN_VALNUM          v, 
				    VN::BIT_VECTOR    &visited,
				    VN::VALNUM_VECTOR &valnum_list)
{
   // If this value number satisfy the following criteria, then add it
   // to the valnum_list, after having dealt with its kids; this ensures
   // that any "smaller" expressions will be linked onto the back of the
   // valnum_list before appending "enclosing" expressions.  The criteria
   // for excluding a value number from the list are as follows:
   //
   //   1) Has no "valid" STMTREP occurrences; i.e. no occurrences we
   //      counted as significant during value numbering.  Value numbering
   //      will disregard occurrences that are lvalues or that are 
   //      const/rconst literals in the CODEREP-->STMTREP mapping.
   //
   //   2) Is associated with only one CODEREP (EPRE should be able to
   //      handle these, so there is no need to handle them here).  One
   //      exception to this rule occurs when the CODEREP is neither
   //      CONST nor RCONST, yet the value number expression is a LITERAL.
   //
   // Note that this gives us a superset of the value numbers we wish to
   // deal with.  We still need to eliminate lvalue occurrences, and we do
   // so when we set up the ocurrence lists.
   //
   if (!visited[v.ordinal()])
   {
      VN_EXPR::CONST_PTR vn_expr = _vn->valnum_expr(v);

      visited[v.ordinal()] = TRUE;
      if (_has_valid_stmtrep_occurrence(v)  && // (1)
	  _may_be_redundant_expr(v, vn_expr))  // (2)
      {
	 if (vn_expr != NULL)
	 {
	    if (vn_expr->get_kind() == VN_EXPR::MEMLOC)
	    {
	       // Do we need to consider all of these (e.g. is a virtual symbol
	       // always defined by chi?).
	       //
	       _select_for_valnum_list(vn_expr->get_bytesize(), 
				       visited, valnum_list);
	       _select_for_valnum_list(vn_expr->get_offset(),
				       visited, valnum_list);
	       _select_for_valnum_list(vn_expr->get_base_addr(),
				       visited, valnum_list);
	       _select_for_valnum_list(vn_expr->get_vsym(),
				       visited, valnum_list);
	    }
	    else if (vn_expr->get_kind() != VN_EXPR::PHI)
	    {
	       // We do not order value numbers based on dependencies of
	       // PHI expressions, since there may be a mutual dependency
	       // between a PHI expression and another expression (in a 
	       // loop), and we would rather do VNFRE for the other (larger?)
	       // expression first.  A phi expression usually has occurrences
	       // that are simple variable/register loads.
	       //
	       for (INT32 opnd = 0; opnd < vn_expr->get_num_opnds(); opnd++)
		  _select_for_valnum_list(vn_expr->get_opnd(opnd), 
					  visited, valnum_list);
	    }
	 }
	 _set_do_vnfre(v, TRUE);
	 valnum_list.push_back(v);
      }
   }
} // VALNUM_FRE::_select_for_valnum_list


void
VALNUM_FRE::_select_and_sort_valnums(VN::VALNUM_VECTOR &valnum_list)
{
   // Put all value numbers representing real occurrences, and which are
   // candidates for redundancy elimination onto the valnum_list.  Furthermore
   // order the value numbers in the valnum_list such that value numbers with
   // "smaller" expressions precede larger ones.  This ensures that a forward
   // walk through the valnum_list will visit expressions bottom-up and vice
   // versa.  There should be no cyclic expression references.  The desired
   // order is for the most part guaranteed by our value numbering algorithm
   // (depth first), with exception of value numbers determined on a second
   // or later iteration of value numbering when new numbers may be invented.
   //
   // Our algorithm is quite simple.  We use a boolean vector on the side
   // to indicate whether or not a value number has been entered into
   // the valnum_list (or should be ignored), and do a depth first walk
   // of the value number expressions to get the order right.  This ensures
   // that the valnum_list is ordered from small to larger expressions.
   //
   OPT_POOL_Push(_lpool, -1);
   {
      const VN_VALNUM     last_valnum = _vn->last_valnum();
#ifndef KEY // bug 9114 : this code moved to caller
      VALNUM_TO_EXPR_LIST vn_to_exprid(*_vn, _lpool);
#endif
      VN::BIT_VECTOR      visited(last_valnum.ordinal()+1,
				  bool(FALSE), 
				  VN::BVECTOR_ALLOCATOR(_lpool));

#ifndef KEY // bug 9114 : this code moved to caller
      _vn_to_exprid = &vn_to_exprid; // Set temporary state for algorithm
#endif

      for (VN_VALNUM v = VN_VALNUM::First(); 
	   v <= last_valnum;
	   v = VN_VALNUM::Next(v))
      {
	 _select_for_valnum_list(v, visited, valnum_list);
      }

#ifndef KEY // bug 9114 : this code moved to caller
      _vn_to_exprid = NULL; // Reset temporary state after algorithm
#endif
   }
   OPT_POOL_Pop(_lpool, -1); // Reclaim bit vector
} // VALNUM_FRE::_select_and_sort_valnums


// ---------------------------------------------------------------------------
// --------- The algorithm for coalescing loop induction variables  ----------
// ---------------------------------------------------------------------------

pair<CODEREP*,STMTREP*>
VALNUM_FRE::_save_to_temp(BB_NODE *in_bb,
			  STMTREP *after_stmt,
			  BOOL     when_not_after_append,
			  CODEREP *rhs)
{
   // The given "rhs" coderep will be assigned to a new temporary (preg).
   // The value number of the "rhs" should be well-defined.  The assignment
   // statement created will be inserted into the in_bb, following the given
   // stmt (after_stmt) or prepended to in_bb if "after_stmt" is NULL.
   //
   // Create the temporary register into which we intend to save the
   // value of the rhs.
   //
   const MTYPE dtype = rhs->Dtyp();
   CODEREP    *lhs = 
      _etable->New_temp_cr(dtype, 
		   rhs->Check_if_result_is_address(_etable->Htable()->Sym()),
		   rhs);

   VNFRE::add_valnum(lhs, get_valnum(rhs->Coderep_id()).ordinal());

   // Generate the save statement
   //
   STMTREP *savestmt =
      _etable->Generate_stid_to_preg(lhs, rhs, dtype, in_bb, in_bb->Linenum());
   savestmt->Set_stmt_id(_etable->Cfg()->Get_stmt_id());

   // Insert the saved statement into the beginning of the given block
   //
   if (after_stmt == NULL)
   {
      if (when_not_after_append)
	 in_bb->Append_stmt_before_branch(savestmt);
      else
	 in_bb->Prepend_stmtrep(savestmt);
   }
   else
      in_bb->Insert_stmtrep_after(savestmt, after_stmt);
   
   Is_Trace(_tracing,
	    (TFile,
	     "----> VALNUM_FRE::_save_to_temp in bb%d\n", in_bb->Id()));
   Is_Trace_cmd(_tracing, savestmt->Print(TFile));
   Is_Trace(_tracing, (TFile, "----------------------- \n"));

   return pair<CODEREP*,STMTREP*>(lhs,savestmt);
} // VALNUM_FRE::_save_to_temp


CODEREP*
VALNUM_FRE::_ivc_insert_initval_diff(BB_NODE *init_bb,
				     CODEREP *init_cr,
				     CODEREP *base_cr)
{
   // Create an expression that holds the difference in initial value
   // between the base indiction variable and some other induction variable.
   //
   const OPCODE    opc = OPCODE_make_op(OPR_SUB, init_cr->Dtyp(), MTYPE_V);
   CODEREP * const rhs = _etable->Htable()->Add_bin_node(opc,init_cr,base_cr);

   (void)compute_valnum(rhs);
   
   // Create a temporary register and save the rhs into this register,
   // appending it to the "init_bb" before any branch stmt.
   //
   pair<CODEREP*,STMTREP*> saved = 
      _save_to_temp(init_bb, NULL, TRUE/*append*/, rhs);

   // Return the lhs of the inserted assignment.
   //
   return saved.first;
} // VALNUM_FRE::_ivc_insert_initval_diff


void 
VALNUM_FRE::_remove_ivc_incr_occurs(PHI_NODE *phi)
{
   // Remove the occurrence where an induction variable is incremented in
   // a loop.
   //
   // Assuming the phi node represents the join of the initial value and
   // each value calculated in the loop body for an induction variable,
   // we here remove any rhs occurrence in an STID statement, where the
   // lhs of the statement has the same aux_id as the phi-result.
   //
   const IDTYPE aux_id = phi->RESULT()->Aux_id();
   EXP_WORKLST *worklist = _worklst(get_valnum(phi->RESULT()->Coderep_id()));

   Is_True(phi->Size()==2 && phi->RESULT()->Kind() == CK_VAR,
	   ("Unexpected kind of PHI node (Size==%d) "
	    "VALNUM_FRE::_remove_ivc_incr_occurs()", phi->Size()));
	 
   EXP_OCCURS     *occ, *next_occ, *prev_occ = NULL;
   EXP_OCCURS_ITER occ_iter;
   occ_iter.Init(_first_real_occur(worklist));
   for (occ = occ_iter.First(); !occ_iter.Is_Empty(); occ = next_occ)
   {
      STMTREP * const stmt = occ->Stmt();

      // Progress to next occurrence before we remove this occurrence.
      //
      next_occ = occ_iter.Next();

      // Remove rhs occurrences for assignment stmts, where the lhs of
      // the assignment has the same aux_id as the phi-result.
      //
      if (OPCODE_operator(stmt->Op()) == OPR_STID && 
	  occ->Stmt_kid_num() == 0                && // rhs occurrence
	  (stmt->Lhs()->Aux_id() == aux_id || 
	   stmt->Lhs()->Aux_id() == aux_id))         // lhs matches
      {
	 // prev_occ remains unchanged!
	 //
	 worklist->Real_occurs().Remove(prev_occ, occ);
	 _etable->Add_to_occ_freelist(occ);
      }
      else
      {
	 prev_occ = occ;
      }
   } // For each occurrence
} // VALNUM_FRE::_remove_ivc_incr_occurs


void
VALNUM_FRE::_ivc_substitute(BB_NODE        *loop_header,
			    STMTREP        *base_stmt,
			    CODEREP        *base_cr,
			    VN_IVC         &vn_ivc,
			    EQCLASS_MEMBER &base,
			    EQCLASS_MEMBER &memb)
{
   // Substitute all occurrences of the given ivc member with an expression
   // that denotes a literal or loop-invariant offset from the "base_cr".
   //
   if (vn_ivc.num_hits(memb) > 0) // not marked as deleted
   {
      const MTYPE dtype = base_cr->Dtyp();
      BOOL        delay_substitution_until_vnfre = FALSE;  
      VN_VALNUM   valnum = vn_ivc.indvar_valnum(memb);
      CODEREP    *ofst_expr_cr = base_cr;

      Is_True(MTYPE_size_min(dtype) >= 32,
	      ("Unexpected dtype (size = %d) in VALNUM_FRE::_ivc_substitute",
	       MTYPE_size_min(dtype)));

      Is_Trace(_tracing, (TFile, "---> IVC substitution for:\n"));
      Is_Trace(_tracing, (TFile, "--->    "));
      Is_Trace_cmd(_tracing, vn_ivc.print(base, memb, TFile));

      if (!vn_ivc.indvar_is_literal_ofst(memb) ||
	  vn_ivc.indvar_literal_ofst(memb) != 0LL)
      {
	 // Create an addition or subtraction expression:
	 //
	 //   base_cr +/- ofst_cr.
	 //
	 CODEMAP * const htable = _etable->Htable();
	 CODEREP *       ofst_cr;
	 OPERATOR        ofst_opr;
	 
	 if (vn_ivc.indvar_is_literal_ofst(memb))
	 {
	    INT64 ofst = vn_ivc.indvar_literal_ofst(memb);

	    ofst_opr = (ofst >=0 ? OPR_ADD : OPR_SUB);
	    ofst = (ofst >=0 ? ofst : (-ofst));
	    ofst_cr = htable->Add_const(dtype, ofst);
	 }
	 else // loop-invariant difference
	 {
	    ofst_opr = OPR_ADD;
	    ofst_cr = _ivc_insert_initval_diff(vn_ivc.indvar_init_bb(base),
					       vn_ivc.indvar_init_cr(memb),
					       vn_ivc.indvar_init_cr(base));
	 }
	 
	 ofst_expr_cr = 
	    htable->Add_bin_node(OPCODE_make_op(ofst_opr, dtype, MTYPE_V),
				 base_cr,
				 ofst_cr);

	 // Set valnum for ofst_expr_cr to valnum of the induction variable
	 // currently being processed.
	 //
	 // Just in case ofst_expr_cr hashes to an already existing 
	 // expression (e.g. "base_cr + 1"), created by the IVC processing
	 // of an earlier member in this equivalence class, we need to
	 // temporarily disable VNFRE for this valnum before calling
	 // VNFRE::add_valnum(): There is little to be gained by moving such
	 // occurrences over to the worklist for "valnum", and we want to
	 // avoid assertion for an attempt to substitute "ofst_expr_cr" by 
	 // "ofst_expr_cr" (bug 671900).
	 //
	 // We could improve slightly on the algorithm by resetting the valnum
	 // for (and moving) all occurrences of old_valnum(ofst_expr_cr) into 
	 // the worklist for the new "valnum", but this entails creating a
	 // _vn_to_exprid() mapping and hardly seems worthwhile.
	 //
	 _set_do_vnfre(valnum, FALSE);
	 VNFRE::add_valnum(ofst_expr_cr, valnum.ordinal());
	 _set_do_vnfre(valnum, TRUE);

	 // Assuming one occurrence is in the "step" at the end of
	 // the loop, and the "step" statement will be removed by DCE,
	 // it seems right to only save the expression into a temporary
	 // if we have more than one occurrence.  NOTE: this can be improved
	 // upon, once software pipelining (swp) is uninhibited by
	 // substitution into the loop termination test, and none of the
	 // occurrences are at a deeper level of loop-nesting, by changing
	 // the following test to >2 or >3 occurrences.
	 //
	 if (vn_ivc.num_occurs(memb) > 1)
	 {
	    if (IVC_Maximize_Live_Ranges()) // default case
	    {
	       // Assign the expression to a temporary at the head of the
	       // loop, thus maximizing the live ranges of the temporaries.
	       //
	       const pair<CODEREP*,STMTREP*> save_info = 
		  _save_to_temp(loop_header,
				base_stmt,
				FALSE/*append*/,
				ofst_expr_cr);    // Right hand side
			
	       ofst_expr_cr = save_info.first; // lhs of assignment
	    }
	    else // IVC_Minimize_Live_Ranges()
	    {
	       // Assign the expression to a temporary, as close to
	       // the real occurrence as is possible, thus minimizing
	       // the live range of the temporary.  This is done by
	       // forcing the VNFRE processing to replace *every*
	       // occurrence in the worklist (mult_real will ensure
	       // this).
	       //
	       EXP_OCCURS *    occ;
	       EXP_OCCURS_ITER occ_iter;
	       FOR_ALL_NODE(occ, 
			    occ_iter, 
			    Init(_first_real_occur(_worklst(valnum))))
	       {
		  // Should also work for our add/sub expressions.
		  //
		  occ->Set_occurrence(ofst_expr_cr);
		  occ->Set_mult_real();
	       }
	       _worklst(valnum)->Set_ivc_cand();

	       // Will be done as part of regular VNFRE processing
	       //
	       delay_substitution_until_vnfre = TRUE;
	    }
	 }
      }
      if (!delay_substitution_until_vnfre)
      {
	 // Verify the worklist occurrences, if necessary.
	 //
	 if (_vn_maybe_removed(valnum))
	 {
	    SET_OPT_REPEAT_PHASE(_vnfre_delete_occurs_phase,
				 "VNFRE: delete occurs");
	    _verify_and_remove_occurs(_worklst(valnum), valnum);
	    SET_OPT_REPEAT_PHASE(_vnfre_ivc_phase, "VNFRE: ivc");
	 }

	 // Once the substitution is done, we are done with this
	 // value number.
	 //
	 _set_do_vnfre(valnum, FALSE);
	 
	 // Substitute all real occurrences with "ofst_expr_cr".  Note that 
	 // the worklist may be empty due to _remove_ivc_incr_occurs!
	 //
	 EXP_OCCURS * const first_real_occ =
	    _first_real_occur(_worklst(valnum));
	 
	 EXP_OCCURS *    occ;
	 EXP_OCCURS_ITER occ_iter;
	 FOR_ALL_NODE(occ, occ_iter, Init(first_real_occ))
	 {
	    // Should also work for our add/sub expressions.
	    //
	    _etable->Replace_by_temp(occ, ofst_expr_cr);
	 }

	 // Done with this value number, so we reset occurrence list 
	 // for this value number.
	 //
	 CXX_DELETE(_worklst(valnum), _gpool);
	 _worklst(valnum) = NULL;

      } // if (!delay_substitution_until_vnfre)
   } // if (not marked as deleted)
} // VALNUM_FRE::_ivc_substitute

#ifdef KEY // bug 9114
BOOL 
VALNUM_FRE::_ivc_loop_variant(BB_NODE *loop_header, VN_VALNUM valnum)
{
   // Go thru all the variable codereps belonging to valnum.  If any of them
   // is not defined in loop_header, then the valnum is loop invariant w.r.t.
   // the loop corresponding to loop_header. Wrong to do IV coalescing in such
   // case.
   VALNUM_TO_EXPR_LIST::EXPR_ITERATOR itr = _vn_to_exprid->begin(valnum);
   VALNUM_TO_EXPR_LIST::EXPR_ITERATOR end = _vn_to_exprid->end(valnum);
   CODEREP *cr;
   while (itr != end) {
     cr = _vn->expr_cr(*itr);
     if (cr->Kind() == CK_VAR && cr->Defbb() != loop_header)
       return FALSE;
     itr++;
   }
   return TRUE;
}
#endif

void
VALNUM_FRE::_ivc_classify(BB_NODE *loop_header, VN_IVC &vn_ivc)
{
   // For a loop header BB, walk through its PHI nodes and classify them
   // into equivalence classes of induction variables.  PHIs that are not
   // induction variables get no classification.
   //
   WN             *index = loop_header->Loop()->Index();
   PHI_NODE       *phi;
   PHI_LIST_ITER   phi_iter;

   // Walk through the PHI list for this BB and classify all PHIs that
   // look like induction variables (ignoring other phi nodes).
   //
   FOR_ALL_ELEM(phi, phi_iter, Init(loop_header->Phi_list()))
   {
      if (phi->Live() &&     // A live induction variable
	  phi->Size() == 2)  // Merges two values
      {
	 CODEREP * const phi_result = phi->RESULT();
	 
	 // Do not consider dedicated registers as induction variables; only
	 // consider "real" variables.
	 //
	 if (!_is_dedicated_preg(phi_result) && // Not a dedicated register
	     _is_real_var(phi_result))          // Has a memory location
	 {
	    VN_VALNUM    result_valnum = 
	       _vn->expr_valnum(phi_result->Coderep_id());
	    VN_EXPR::PTR result_vn_expr = _vn->valnum_expr(result_valnum);

	    if (_user_enabled(result_valnum)   && // Not explicitly disabled
		_do_vnfre(result_valnum)       && // Has real occurrence
		!result_valnum.is_bottom()     && // Valid valnum
		result_vn_expr != NULL         && // Phi valnum expression
		result_vn_expr->get_kind() == VN_EXPR::PHI &&
		result_vn_expr->get_num_opnds() == 2)
	    {
#ifdef KEY // bug 9114 : without this, could falsely identify class for the
	   // 	 enclosing loop, and inserting in loop_header is then wrong
	       if (! _ivc_loop_variant(loop_header, result_valnum))
		 continue;
#endif
	       if (index != NULL                                 &&
		   WN_st(index) == 
		   _etable->Opt_stab()->St(phi_result->Aux_id()) &&
		   WN_idname_offset(index) == 
		   _etable->Opt_stab()->St_ofst(phi_result->Aux_id()))
	       {
		  vn_ivc.classify(phi, result_valnum, TRUE);
		  index = NULL;
	       }
	       else
	       {
		  vn_ivc.classify(phi, result_valnum, FALSE);
	       }
	    }
	 }
      }
   } // for all phi
} // VALNUM_FRE::_ivc_classify


void
VALNUM_FRE::_ivc_coalesce(BB_NODE *loop_header, VN_IVC &vn_ivc)
{
   // Walk through the equivalence classes and coalesce the members in
   // each class.
   //
   // Note on replacement scheme: We need to avoid overlapping live ranges,
   // as when replacing j2 by i2 in:
   //
   //    i2 = PHI(i1,i3)
   //    j2 = PHI(j1,j3)
   //    ...
   //    i3 = i2 + c
   //    j3 = j2 + c
   //
   // We avoid overlapping live ranges, yet have a straightforward
   // replacement scheme, by introducing a new temporary in the loop header
   // to hold the replacement value.
   //
   for (INT32 i = 0; i < vn_ivc.num_eqclasses(); i++)
   {
      VN_IVC::members_iterator memb_it_start = vn_ivc.members_begin(i);
      
      if (vn_ivc.num_members(i) > 1 || 
	  (vn_ivc.num_members(i) == 1 &&
	   vn_ivc.num_hits(*memb_it_start) > 1))
      {
	 VN_IVC::members_iterator memb_it_end = vn_ivc.members_end();
	 VN_IVC::members_iterator memb_it;
	 
	 // Choose one induction variable as the one used to calculate the
	 // value of the others in the same equivalence class.  We call the
	 // chosen member the "base_memb"
	 //
	 VN_IVC::members_iterator base_memb = 
	    VN_IVC_choose_eqclass_base_indvar(GET_NUM_OCCURS(this),
					      vn_ivc,
					      memb_it_start,
					      memb_it_end);

	 // Apply heuristics to select the members of the equivalence class
	 // we wish to coalesce, and calculate the offsets from the selected
	 // base member. Any member removed will be marked with num_hits==0!
	 //
	 INT32 num_candidates = 
	    vn_ivc.finalize_for_coalescing(*base_memb,
					   memb_it_start,
					   memb_it_end,
					   IVC_Aggressive());

	 Is_True(vn_ivc.indvar_is_literal_ofst(*base_memb) &&
		 vn_ivc.indvar_literal_ofst(*base_memb) == 0,
		 ("Unexpected offset for coalescing base in "
		  "VALNUM_FRE::_ivc_coalesce()"));
	 
	 if (_tracing)
	 {
	    fprintf(TFile, "====> IVC for BB %d <====\n", loop_header->Id());
	    for (memb_it = memb_it_start; memb_it != memb_it_end; ++memb_it)
	       vn_ivc.print(*base_memb, *memb_it, TFile);
	 }

	 if (num_candidates > 1)
	 {
	    // At least two induction variables will be coalesced.
	    //
	    Is_Trace(_tracing, 
		     (TFile, "Base induction variable = vn%d!\n", 
		      (INT32)vn_ivc.indvar_valnum(*base_memb).ordinal()));
	    
	    // Create and insert an assignment to a temporary representing
	    // the base member (+ update value numbering for the new coderep).
	    //
	    const pair<CODEREP*,STMTREP*> temp_info = 
	       _save_to_temp(loop_header,
			     NULL,             // insert at beginning of bb)
			     FALSE/*append*/,  // prepend
			     vn_ivc.indvar_phi(*base_memb)->RESULT()); 

	    // To make sure software pipelining works correctly, we wish to
	    // avoid substituting into the rhs of an assignment where the 
	    // base induction variable is incremented: We remove such an
	    // occurrence from the occurrence list for the base.  This is OK,
	    // since this is the one induction variable we do not intend to 
	    // remove from the loop.
	    //
	    _remove_ivc_incr_occurs(vn_ivc.indvar_phi(*base_memb));

	    // Walk through the member-list, this time replacing all
	    // occurrences of any eligible member with an offset expression
	    // relative to the "base_cr":
	    //
	    //    base_cr + ofst(*mem_it) - ofst(*base_memb)
	    //
	    CODEREP * const base_cr = temp_info.first;
	    STMTREP * const base_stmt = temp_info.second;
	    
	    for (memb_it = memb_it_start; memb_it != memb_it_end; ++memb_it)
	    {
	       _ivc_substitute(loop_header,
			       base_stmt,
			       base_cr,
			       vn_ivc,
			       *base_memb,
			       *memb_it);
	    }
	 } // If 2 or more induction variables to be coalesced
      } // If several members or one member with several hits
   } // for each equivalence class
} // VALNUM_FRE::_ivc_coalesce


void
VALNUM_FRE::_ivc()
{
   // TODO: put this under a flag to allow us to turn this off.
   //
   if (IVC_Enabled())
   {
      DPOBB_ITER dpo_iter(_etable->Cfg()); // Dominator tree preorder
      BB_NODE   *bb;

      FOR_ALL_ELEM (bb, dpo_iter, Init())
      {
	 // TODO: remove _coalesce_phi_results(bb)!  What we do here should
	 //       subsume it!
	 //
	 if (bb->Loop() != NULL        && 
	     bb->Loop()->Well_formed() && 
	     bb->Loop()->Header() == bb)
	 {
	    // Classify and coalesce the phi results defined in this bb.
	    //
	    OPT_POOL_Push(_lpool, -1);
	    {
	       VN_IVC vn_ivc((IVC_LoopInvariant_Diff()?
			      VN_IVC::IVC_INVARIANT_DIFF:
			      VN_IVC::IVC_CONST_DIFF),
			     _vn, _lpool); // Classification representation
	       
	       _ivc_classify(bb, vn_ivc);  // Classification
	       _ivc_coalesce(bb, vn_ivc);  // Coalescing (substitution)
	    }
	    OPT_POOL_Pop(_lpool, -1);
	 }
      } // For each bb
   } // If Enabled
} // VALNUM_FRE::_ivc


// ---------------------------------------------------------------------------
// --- The Algorithm for collecting real occurrences of each value number  ---
// ---------------------------------------------------------------------------

BOOL 
VALNUM_FRE::_contains_undef_val(CODEREP *cr, STMTREP *stmt) const
{
   // Determine if a coderep has a nested volatile or zero version 
   // reference.  Note that there should be at most *one* occurrence
   // of a coderep with a volatile reference in a worklist.  Should
   // we eliminate them from the worklist altogether?  Just in case,
   // we here have a handle to check when VNFRE is activated on a
   // worklist!
   //
   BOOL undef = FALSE;
   
   switch (cr->Kind())
   {
   case CK_CONST:
   case CK_RCONST:
   case CK_LDA:
      break;

   case CK_VAR:
      undef = cr->Is_var_volatile() || cr->Is_flag_set(CF_IS_ZERO_VERSION);
      break;

   case CK_IVAR:
      if (cr->Is_ivar_volatile())
	 undef = TRUE;
      else
      {
	 CODEREP *const vsym = cr->Get_ivar_vsym();

	 if (cr->Opr() == OPR_ILOADX)
	    Warn_todo("VALNUM_FRE::_contains_undef_val: Indexed load.");

	 if (vsym != NULL && 
	     (vsym->Is_var_volatile() ||
	      vsym->Is_flag_set(CF_IS_ZERO_VERSION)))
	 {
	    undef = TRUE;
	 }
	 else
	 {
	    // Ilod_base can be different from Istr_base if the Iload side is
	    // dead, i.e. when Usecnt() == 0
	    // 
	    if (cr == stmt->Lhs() && OPCODE_is_store(stmt->Op()))
	    {
	       if (cr->Opr() == OPR_MLOAD)
		  undef = _contains_undef_val(cr->Mstore_size(), stmt);
	       else if (cr->Opr() == OPR_ILOADX)
		  undef = _contains_undef_val(cr->Index(), stmt);
	       if (!undef)
		  undef = _contains_undef_val(cr->Istr_base(), stmt);
	    }
	    else
	    {
	       if (cr->Opr() == OPR_MLOAD)
		  undef = _contains_undef_val(cr->Mload_size(), stmt);
	       else if (cr->Opr() == OPR_ILOADX)
		  undef = _contains_undef_val(cr->Index(), stmt);
	       if (!undef)
		  undef = _contains_undef_val(cr->Ilod_base(), stmt);
	    }
	 }
      }
      break;

   case CK_OP:
      if (OPERATOR_is_volatile(cr->Opr()))
	 undef = TRUE;
      else
      {
	 for (INT32 i=0; i<cr->Kid_count(); i++)
	    undef = undef || _contains_undef_val(cr->Opnd(i), stmt);
      }
      break;

   case CK_DELETED:	// should never happen
   default:		// illegal kind
      FmtAssert(FALSE, 
		("VNFRE::_contains_undef_val(), unexpected kind 0x%x",
		 cr->Kind()));
      break;
   }
   return undef;
} // VALNUM_FRE::_contains_undef_val


inline BOOL
VALNUM_FRE::_subsumable_by_branch(VN_VALNUM valnum, 
				  STMTREP  *stmt, 
				  CODEREP  *cr) const
{
   // This matches the Subsumable_by_branch() calls in opt_etable.cxx,
   // but we also make sure the occurrence is a top-level expression under
   // the branch and that it will not be replaced by a literal constant.
   //
   VN_EXPR::PTR expr = _vn->valnum_expr(valnum);

   return ((stmt->Op() == OPC_TRUEBR || stmt->Op() == OPC_FALSEBR) &&
	   cr == stmt->Rhs()                                       &&
	   (expr == NULL || expr->get_kind() != VN_EXPR::LITERAL)  &&
	   Subsumable_by_branch(cr));
} // VALNUM_FRE::_subsumable_by_branch


EXP_OCCURS *   
VALNUM_FRE::_create_real_occurrence(CODEREP *cr,
				    STMTREP *stmt, 
				    INT      stmt_kid_num,
				    UINT     depth)
{
   // Create the EXP_OCCURS node
   //
   EXP_OCCURS *occurs = _etable->Alloc_vnfre_occurs_node();
   occurs->Set_occurrence(cr);
   occurs->Set_kind(EXP_OCCURS::OCC_REAL_OCCUR);
   occurs->Set_enclose_stmt(stmt);
   occurs->Set_stmt_kid_num(stmt_kid_num);
   occurs->Set_rehash_cost(depth);
   return occurs;
} // VALNUM_FRE::_create_real_occurrence


void    
VALNUM_FRE::_append_real_occurrence(CODEREP *cr,
				    STMTREP *stmt, 
				    INT      stmt_kid_num,
				    UINT     depth)
{
   // This obtains the worklist and updates it to include the given 
   // occurrence, in much the same way this is done by ETABLE::Get_worklst()
   // in combination with ETABLE::Append_real_occurrence().
   //
   // Note that we will use value numbers as E_nums.  We create worklists
   // containing various different expressions, all with the same value-
   // number.  The Exp() attribute of a worklist will be set to be the 
   // very first CODEREP occurrence of the value number.
   //
   // Set_mult_real() will apply for multiple occurrences of a value-number,
   // as opposed to multiple occurrences of a lexical expression.
   //
   // Also, not that the cr can never be an lvalue occurrence, since such
   // occurrences should have been weeded out before reaching this point.
   //
   // Note that we do not maintain EXP_WORKLST::Is_sign_ext() here.
   //
   VN_VALNUM valnum = get_valnum(cr->Coderep_id());

   if (_do_vnfre(valnum) && !_subsumable_by_branch(valnum, stmt, cr))
   {
      if (_worklst(valnum) == NULL)
      {
	 _worklst(valnum) = CXX_NEW(EXP_WORKLST(valnum.ordinal()/*E_num*/,
						cr, PK_VNFRE),
				    _gpool);
      }

      // First check if this is a second real occurrence in the same stmt kid 
      // tree.
      //
      // If they are the same, set flag to OCC_MULT_REAL_OCCUR and return,
      // otherwise create a new EXP_OCCURS node and append it to the end
      // of the worklist.
      //
      EXP_OCCURS *tail_occ = _worklst(valnum)->Real_occurs().Tail();
      if (tail_occ != NULL                     && 
	  tail_occ->Enclosed_in_stmt() == stmt &&
	  tail_occ->Stmt_kid_num() == stmt_kid_num)
      {
	 tail_occ->Set_mult_real();
	 if (tail_occ->Rehash_cost() < depth)
	    tail_occ->Set_rehash_cost(depth);
      }
      else
      {
	 // Create the EXP_OCCURS node and append it to the worklist
	 //
	 EXP_OCCURS *occurs = 
	    _create_real_occurrence(cr, stmt, stmt_kid_num, depth);
	 _worklst(valnum)->Append_occurrence(occurs);

	 // Let the coderep refer to its value number directly (not needed?).
	 //
	 cr->Set_e_num(_worklst(valnum)->E_num());

	 VNFRE_TRACE(stderr,
		     "appending to worklst[%d]: stmt = %p, kid_num = %d\n",
		     valnum.ordinal(), stmt, stmt_kid_num);
      }
   } // if (_do_vnfre)
} // VALNUM_FRE::_append_real_occurrence


void
VALNUM_FRE::_append_exit_occurrence(BB_NODE *bb)
{
   // Create the EXP_PRED_OCCURS node
   //
   EXP_OCCURS *occurs = _etable->Alloc_vnfre_occurs_node();
   occurs->Set_occurrence(NULL);
   occurs->Set_kind(EXP_OCCURS::OCC_EXIT_OCCUR);
   occurs->Set_enclose_bb(bb);

   // call the WORKLST append
   //
   _exit_occurs.Append(occurs);
} // VALNUM_FRE::_append_exit_occurrence


void    
VALNUM_FRE::_insert_a_cr_occurrence(CODEREP *cr,
				    STMTREP *stmt, 
				    INT      stmt_kid_num,
				    UINT     depth)
{
   // Note that we do not maintain EXP_WORKLST::Is_sign_ext() here.
   //
   const VN_VALNUM valnum = get_valnum(cr->Coderep_id());

   if (_do_vnfre(valnum) && !_subsumable_by_branch(valnum, stmt, cr))
   {
      EXP_WORKLST *worklist = _worklst(valnum);
      EXP_OCCURS *occurs = 
	 _create_real_occurrence(cr, stmt, stmt_kid_num, depth);

      Is_True(worklist != NULL, 
	      ("Undefined worklist for valnum in "
	       "VALNUM_FRE::_insert_a_cr_occurrence()"));

      // Note that insertion will not occur if another occurrence for the
      // same stmt and kidno already exists in the worklist.
      //
      worklist->Insert_occurrence(occurs, _etable);
      VNFRE_TRACE(stderr,
		  "inserting to worklst[%d]: stmt = %p, kid_num = %d\n",
		  valnum.ordinal(), stmt, stmt_kid_num);
   }
} // VALNUM_FRE::_insert_a_cr_occurrence


void    
VALNUM_FRE::insert_cr_occurrences(CODEREP *cr,
				  STMTREP *stmt, 
				  INT      stmt_kid_num, 
				  BOOL     is_store,
				  UINT     depth)
{
   // This is similar to _collect_cr_occurrences, but we here insert
   // new occurrences into an existing worklist instead of appending
   // initial occurrences.  We do not bother with codereps for which
   // we have already determined we will not do any further vnfre processing.
   //
   Is_True(cr != NULL, ("VALNUM_FRE::insert_cr_occurrences, cr == NULL"));

   // We first traverse subexpressions nested at deeper levels, before 
   // adding cr to the worklist.  We only add a coderep to worklist
   // if we anticipate that we will be performing redundancy analysis
   // for its value number.
   //
   switch (cr->Kind())
   {
   case CK_CONST:
   case CK_RCONST:
      break;  // We do not collect literal occurrences!

   case CK_LDA:
      _insert_a_cr_occurrence(cr, stmt, stmt_kid_num, depth);
      break;

   case CK_VAR:
      if (!is_store)
	 _insert_a_cr_occurrence(cr, stmt, stmt_kid_num, depth);
      break;

   case CK_IVAR:
      if (cr->Opr() == OPR_ILOADX)
	 Warn_todo("VALNUM_FRE::insert_cr_occurrences: Indexed load.");

      // Ilod_base can be different from Istr_base if the Iload side is
      // dead, i.e. when Usecnt() == 0
      // 
      if (!is_store)
      {
	 if (cr->Opr() == OPR_MLOAD)
	    insert_cr_occurrences(cr->Mload_size(), stmt, stmt_kid_num,
				   FALSE, depth+1);
	 else if (cr->Opr() == OPR_ILOADX)
	    insert_cr_occurrences(cr->Index(), stmt, stmt_kid_num,
				   FALSE, depth+1);
	 insert_cr_occurrences(cr->Ilod_base(), stmt, stmt_kid_num,
				FALSE, depth+1);

	 if (cr->Opr() != OPR_PARM)
	    _insert_a_cr_occurrence(cr, stmt, stmt_kid_num, depth);
      }
      else
      {
	 if (cr->Opr() == OPR_MLOAD)
	    insert_cr_occurrences(cr->Mstore_size(), stmt, stmt_kid_num,
				   FALSE, depth+1);
	 else if (cr->Opr() == OPR_ILOADX)
	    insert_cr_occurrences(cr->Index(), stmt, stmt_kid_num,
				   FALSE, depth+1);
	 insert_cr_occurrences(cr->Istr_base(), stmt, stmt_kid_num,
				FALSE, depth+1);
      }
      break;

   case CK_OP:
      //
      // TODO: follow the example of opt_etable.cxx, using crid-maps to
      // represent the cr->Temp_id() and cr->Is_lcse() flags, to avoid 
      // traversing an expression that has multiple occurrences within 
      // the same stmt more than twice (we have to visit it twice to have 
      // _append_real_occurrence set the OCC_MULT_REAL_OCCUR flag 
      // correctly).
      //
      // Note that we do not set "cr->Max_depth()" either , which was 
      // intended for more optimal rehashing, but which is never used
      // for vnfre.
      //
      Is_True(cr->Opr() != OPR_ARRAY,
	      ("VNFRE::insert_cr_occurrences: reach an OPR_ARRAY node,"
	       " this is a bug in the lowering process"));
      {
	 for (INT32 i=0; i<cr->Kid_count(); i++)
	 { 
	    insert_cr_occurrences(cr->Opnd(i), stmt, stmt_kid_num,
				   FALSE, depth+1);
	 }
      }
      // We need not consider cr->Exp_has_e_num(), since the following stmt
      // only has effect if value numbering has determined that this is a 
      // candidate we need to consider for VNFRE.  We do not take into 
      // account a possible test for _is_nocost_branch_condition() either,
      // since that is irrelevant given that we here do full-redundancy 
      // elimination only (not code-motion in the full sense of the term).
      //
      if (cr->Opr() != OPR_PARM)
	 _insert_a_cr_occurrence(cr, stmt, stmt_kid_num, depth);
      break;

   case CK_DELETED:	// should never happen
   default:		// illegal kind
      FmtAssert(FALSE, 
		("VNFRE::insert_cr_occurrences(), unexpected kind 0x%x",
		 cr->Kind()));
      break;
   }
} // VALNUM_FRE::insert_cr_occurrences


void    
VALNUM_FRE::collect_cr_occurrences(CODEREP *cr,
				   STMTREP *stmt, 
				   INT      stmt_kid_num, 
				   BOOL     is_store_lhs,
				   UINT     depth)
{
   // Depth first traversal of CODEREP nodes.  The value number for the
   // cr gives us the occurrence list to use for this coderep.  This is
   // greatly simplified version of ETABLE::Bottom_up_cr(), which collects
   // the initial real occurrences of every value number into worklists.
   // Unlike ETABLE::Bottom_up_cr(), we do not need to deal with volatiles
   // here, since they should always get unique value numbers and will 
   // therefore be precluded from VNFRE.
   //
   Is_True(cr != NULL, ("VALNUM_FRE::collect_cr_occurrences, cr == NULL"));
   Is_True(cr != NULL, ("VALNUM_FRE::collect_cr_occurrences, cr == NULL"));
//Bug 1573
#ifdef KEY
   if (stmt->Opr() == OPR_ASM_STMT && (cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR)){
     CODEREP *asm_rep = stmt->Rhs();
     for (INT32 i=0; i<asm_rep->Kid_count(); i++){
       CODEREP *kid = asm_rep->Opnd(i);
       if (kid->Opr() == OPR_ASM_INPUT && kid->Opnd(0) == cr)
         return;
     }
   }
#endif

   // This is a coderep for which we intend to carry out the complete
   // VNFRE algorithm. We first traverse subexpressions nested at deeper
   // levels, before adding cr to the worklist.
   //
   switch (cr->Kind())
   {
   case CK_CONST:
   case CK_RCONST:
      break;  // We do not collect literal occurrences!

   case CK_LDA:
      _append_real_occurrence(cr, stmt, stmt_kid_num, depth);
      break;

   case CK_VAR:
      if (!is_store_lhs)
	 _append_real_occurrence(cr, stmt, stmt_kid_num, depth);
      break;

   case CK_IVAR:
      if (cr->Opr() == OPR_ILOADX)
	 Warn_todo("VALNUM_FRE::collect_cr_occurrences: Indexed load.");

      // Ilod_base can be different from Istr_base if the Iload side is
      // dead, i.e. when Usecnt() == 0
      // 
      if (!is_store_lhs)
      {
	 if (cr->Opr() == OPR_MLOAD)
	    collect_cr_occurrences(cr->Mload_size(), stmt, stmt_kid_num,
				    FALSE, depth+1);
	 else if (cr->Opr() == OPR_ILOADX)
	    collect_cr_occurrences(cr->Index(), stmt, stmt_kid_num,
				    FALSE, depth+1);
	 collect_cr_occurrences(cr->Ilod_base(), stmt, stmt_kid_num,
				 FALSE, depth+1);
	 if (cr->Opr() != OPR_PARM)
	    _append_real_occurrence(cr, stmt, stmt_kid_num, depth);
      }
      else
      {
	 if (cr->Opr() == OPR_MLOAD)
	    collect_cr_occurrences(cr->Mstore_size(), stmt, stmt_kid_num,
				    FALSE, depth+1);
	 else if (cr->Opr() == OPR_ILOADX)
	    collect_cr_occurrences(cr->Index(), stmt, stmt_kid_num,
				    FALSE, depth+1);
	 collect_cr_occurrences(cr->Istr_base(), stmt, stmt_kid_num,
				 FALSE, depth+1);
      }
      break;

   case CK_OP:
      //
      // TODO: follow the example of opt_etable.cxx, using crid-maps to
      // represent the cr->Temp_id() and cr->Is_lcse() flags, to avoid 
      // traversing an expression that has multiple occurrences within 
      // the same stmt more than twice (we have to visit it twice to have 
      // _append_real_occurrence set the OCC_MULT_REAL_OCCUR flag 
      // correctly).
      //
      // Note that we do not set "cr->Max_depth()" either , which was 
      // intended for more optimal rehashing, but which is never used
      // for vnfre.
      //
      Is_True(cr->Opr() != OPR_ARRAY,
	      ("VNFRE::collect_cr_occurrences: reach an OPR_ARRAY node,"
	       " this is a bug in the lowering process"));
      {
	 for (INT32 i=0; i<cr->Kid_count(); i++)
	 { 
	    collect_cr_occurrences(cr->Opnd(i), stmt, stmt_kid_num,
				    FALSE, depth+1);
	 }
	 
	 // We need not consider cr->Exp_has_e_num(), since, if we get here,
	 // value numbering has determined that this is a candidate we need
	 // to consider for VNFRE.
	 //
	 OPERATOR opr = cr->Opr();
	 if (opr != OPR_PARM && !OPERATOR_is_volatile(opr)) {
	   if (!WOPT_Enable_CSE_FP_comparison &&
	       (opr == OPR_EQ || opr == OPR_NE ||
		opr == OPR_LT || opr == OPR_LE || 
		opr == OPR_GT || opr == OPR_GE) &&
	       MTYPE_is_float(cr->Dsctyp())) {
	     // skip _append_real_occurrence
	   } else
	     _append_real_occurrence(cr, stmt, stmt_kid_num, depth);
	 }
	 
      }
      break;

   case CK_DELETED:	// should never happen
   default:		// illegal kind
      FmtAssert(FALSE, 
		("VNFRE::collect_cr_occurrences(), unexpected kind 0x%x",
		 cr->Kind()));
      break;
   }
} // VALNUM_FRE::collect_cr_occurrences


void
VALNUM_FRE::_collect_all_real_occurrences()
{
   // This subroutine is modelled as a greatly simplified version of
   // EOCC::Collect_real_occurrences() and ETABLE::Bottom_up_stmt()
   // in opt_eocc.cxx.  It is used to create worklists of initial 
   // occurrences of <stmtrep,coderep> pairs, one worklist for each
   // value number for which we plan to do the complete VNFRE algorithm.
   //
   DPOBB_ITER dpo_iter(_etable->Cfg()); // Dominator tree preorder
   BB_NODE   *bb;

   FOR_ALL_ELEM (bb, dpo_iter, Init())
   {
      COLLECT_CR_OCCURS cr_collector(this);
      STMTREP_ITER      stmt_iter(bb->Stmtlist());
      STMTREP          *stmt;

      FOR_ALL_NODE(stmt, stmt_iter, Init())
      {
	 stmt->Set_stmt_id(_etable->Cfg()->Get_stmt_id());
	 stmt->Reset_RHS_saved();
	 stmt->Reset_saved_RHS();

	 traverseSR(stmt, cr_collector);
      } // for each stmt
      
      if (bb->Kind() == BB_EXIT && bb != _etable->Cfg()->Fake_exit_bb())
	 _append_exit_occurrence(bb);

   } // for each bb
} // VALNUM_FRE::_collect_all_real_occurrences


void
VALNUM_FRE::_verify_and_remove_occurs(EXP_WORKLST *worklist,
				      VN_VALNUM    valnum)
{
   // Walk through each occurrence and verify that its nature remains
   // as indicated.  I.e. that it has at least one occurrence in the
   // child denoted, and whether or not it is a multi-occurrence.
   //
   // Update and remove occurrences as appropriate.
   //
   EXP_OCCURS      *occ, *next_occ, *prev_occ = NULL;
   EXP_OCCURS_ITER  occ_iter;
   occ_iter.Init(worklist->Real_occurs().Head());
   for (occ = occ_iter.First(); !occ_iter.Is_Empty(); occ = next_occ)
   {
      // Count the occurrences. NOTE: _get_occur_cr() never returns a lhs,
      // although it may return a kid of the lhs.
      //
      pair<INT32,CODEREP*> counted_occurs = 
	 Count_occurs(_get_occur_cr(occ), Match_Vn(_vn, valnum), FALSE/*lhs*/);

      // Progress to next occurrence before we remove this occurrence.
      //
      next_occ = occ_iter.Next();

      if (counted_occurs.first == 0)
      {
	 // prev_occ remains unchanged!
	 //
	 worklist->Real_occurs().Remove(prev_occ, occ);
	 _etable->Add_to_occ_freelist(occ);
      }
      else
      {
	  // The Coderep referenced by an occurrence for a statement must be a
	  // valid subtree of the statement, since such codereps may be saved
	  // into temporaries as a result of code-motion and should always 
	  // reflect a coderep actually referenced by the statement.
	  //
	  occ->Set_occurrence(counted_occurs.second);

	 if (worklist->Ivc_cand() || counted_occurs.first > 1)
	    occ->Set_mult_real(); // All ivc occurs considered mult_real()
	 else
	    occ->Reset_mult_real();
	 
	 prev_occ = occ;
      }
   }
} // VALNUM_FRE::_verify_and_remove_occurs


// ---------------------------------------------------------------------------
// ------------- Methods describing worklist characteristics -----------------
// ---------------------------------------------------------------------------

BOOL 
VALNUM_FRE::_all_same_occurs(CODEREP       *cr,
			     BOOL           lhs_of_store,
			     VN_VALNUM      valnum,
			     const CODEREP *occ_cr) const
{
   // Return TRUE if all occurrences of the given valnum is an occ_cr;
   // otherwise return FALSE!
   //
   BOOL same = (get_valnum(cr->Coderep_id()) != valnum || 
		cr == occ_cr);
   
   if (same)
   {
      // Keep going!
      //
      switch (cr->Kind())
      {
      case CK_CONST:
      case CK_RCONST:
      case CK_LDA:
      case CK_VAR:
	 break;  // remains TRUE

      case CK_IVAR:
	 if (!lhs_of_store)
	 {
	    if (cr->Opr() == OPR_MLOAD)
	       same = 
		  _all_same_occurs(cr->Mload_size(), FALSE, valnum, occ_cr);
	    else if (cr->Opr() == OPR_ILOADX)
	       same = 
		  _all_same_occurs(cr->Index(), FALSE, valnum, occ_cr);
	    if (same)
	       same = 
		  _all_same_occurs(cr->Ilod_base(), FALSE, valnum, occ_cr);
	 }
	 else
	 {
	    if (cr->Opr() == OPR_MLOAD)
	       same = 
		  _all_same_occurs(cr->Mstore_size(), FALSE, valnum, occ_cr);
	    else if (cr->Opr() == OPR_ILOADX)
	       same = 
		  _all_same_occurs(cr->Index(), FALSE, valnum, occ_cr);
	    if (same)
	       same =
		  _all_same_occurs(cr->Istr_base(), FALSE, valnum, occ_cr);
	 }
	 break;

      case CK_OP:
	 Is_True(cr->Opr() != OPR_ARRAY,
		 ("VNFRE::all_same_occurs: reach an OPR_ARRAY node,"
		  " this is a bug in the lowering process"));
	 {
	    for (INT32 i=0; same && i<cr->Kid_count(); i++)
	       same = _all_same_occurs(cr->Opnd(i), FALSE, valnum, occ_cr);
	 }
	 break;

      case CK_DELETED:	// should never happen
      default:		// illegal kind
	 FmtAssert(FALSE, 
		   ("VALNUM_FRE::_all_same_occurs(), unexpected kind 0x%x",
		    cr->Kind()));
	 break;
      }
   }

   return same;

} // VALNUM_FRE::_all_same_occurs


BOOL 
VALNUM_FRE::_same_var_occurs(EXP_OCCURS *occ, const CODEREP *var_cr) const
{
   BOOL same = (occ->Occurrence() == var_cr);
   
   if (same && occ->Mult_real())
   {
      // Need to search through the enclosing coderep and look at all
      // occurrences.
      //
      VN_VALNUM  valnum = get_valnum(occ->Occurrence()->Coderep_id());
      CODEREP   *enclosure = _get_occur_cr(occ);
      STMTREP   *stmt = occ->Stmt();
      BOOL       lhs_of_store = (stmt->Lhs() == enclosure && 
				 OPCODE_is_store(stmt->Op()));

      same = _all_same_occurs(enclosure, lhs_of_store, valnum, var_cr);
   }
   return same;
} // VALNUM_FRE::_same_var_occurs


void
VALNUM_FRE::_get_worklist_info(EXP_WORKLST *w,
			       BOOL        &same_var_occurs,
			       BOOL        &save_use_occurs,
			       BOOL        &disabled_occurs) const
{
   // Walk through the worklist once and gather the info.  The info
   // gathered is as follows:
   //
   //    same_var_occurs:  All occurrences refer to the same variable.
   //
   //    save_use_occurs:  First occurrence saves an expression into
   //                      a temporary, and all subsequent occurrences
   //                      refer to the temporary variable.
   //
   //    disabled_occurs:  This mirrors what is modelled by the subroutine
   //                      _disabled_expr(), but if the VN_EXPR is NULL some
   //                      cases may slip through and we check for them here.
   //                      Any one disabled occurrence will disable VNFRE for
   //                      the whole worklist.
   //
   // Typically we do not want to do VNFRE when any of the info returned
   // by this subroutine is FALSE.
   //

   EXP_OCCURS * const head_occ = _first_real_occur(w);
   CODEREP    * const head_cr  = head_occ->Occurrence();
   STMTREP    * const head_stmt = head_occ->Enclosed_in_stmt();
   CODEREP    * const head_stmt_lhs = head_stmt->Lhs();

   // Set predicates to be tentatively true, if at all possible.
   //
   disabled_occurs = (head_cr->Kind() == CK_LDA ||
		      head_cr->Dtyp() == MTYPE_M);
   same_var_occurs = (head_cr->Kind() == CK_VAR && 
		      _same_var_occurs(head_occ, head_cr));
   save_use_occurs = (head_occ != _last_real_occur(w)              &&
		      OPCODE_operator(head_stmt->Op()) == OPR_STID &&
		      head_occ->Occurrence() == head_stmt->Rhs()   &&
		      head_stmt_lhs->Kind() == CK_VAR);
   
   if (same_var_occurs || save_use_occurs || !disabled_occurs)
   {
      // At least one of the predicates *may* be TRUE.
      //
      for (EXP_OCCURS *occ = head_occ->Next();
	   (occ != NULL && 
	    (same_var_occurs || save_use_occurs || !disabled_occurs));
	   occ = occ->Next())
      {
	 CODEREP * const cr = occ->Occurrence();

	 if (cr != head_stmt_lhs)
	    save_use_occurs = FALSE;
	 if (!_same_var_occurs(occ, head_cr))
	    same_var_occurs = FALSE;
	 if (cr->Kind() == CK_LDA || cr->Dtyp() == MTYPE_M)
	    disabled_occurs = TRUE;
      }
   }
} // VALNUM_FRE::_get_worklist_info


BOOL 
VALNUM_FRE::_is_vnfre_candidate(EXP_WORKLST *w) const
{
   // We do not consider just a single occurrence a candidate (will be
   // handled by EPRE).
   //
   const BOOL multiple_occurs = 
      (_has_an_occur(w) &&
       (_first_real_occur(w) != _last_real_occur(w) || 
	_first_real_occur(w)->Mult_real()));
   
   if (multiple_occurs)
   {
      BOOL same_var_occurs;// All occurrences are refs to single var
      BOOL save_use_occurs;// First occ saves to var; subsequent ones ref var
      BOOL disabled_occurs;// One or more occurrences are of a CR kind disabled

      _get_worklist_info(w, same_var_occurs, save_use_occurs, disabled_occurs);

      return (!same_var_occurs  && 
	      !save_use_occurs  && 
	      !disabled_occurs);
   }
   else // Single occurrence
   {
      return FALSE;
   }
} // VALNUM_FRE::_is_vnfre_candidate


// ---------------------------------------------------------------------------
// -------- Methods for collecting phi occurrences for one valnum ------------
// ---------------------------------------------------------------------------

EXP_OCCURS *
VALNUM_FRE::_append_phi_occurrence(EXP_PHI *phi, EXP_WORKLST *worklist)
{
   // Create the EXP_OCCURS node
   //
   EXP_OCCURS *occurs = _etable->Alloc_vnfre_occurs_node();
   occurs->Set_occurrence(worklist->Exp());
   occurs->Set_kind(EXP_OCCURS::OCC_PHI_OCCUR);
   occurs->Set_exp_phi(phi);
   _etable->Set_phi_result(phi, occurs);

   // call the WORKLST append, and return the occurrence.
   //
   worklist->Append_occurrence(occurs);
   return occurs;
} // VALNUM_FRE::_append_phi_occurrence


EXP_OCCURS *
VALNUM_FRE::_append_phi_pred_occurrence(BB_NODE *bb, EXP_WORKLST *worklist)
{
   // Create the EXP_PRED_OCCURS node
   //
  EXP_OCCURS *occurs = _etable->Alloc_vnfre_occurs_node();
  occurs->Set_occurrence(worklist->Exp());
  occurs->Set_kind(EXP_OCCURS::OCC_PHI_PRED_OCCUR);
  occurs->Set_enclose_bb(bb);

  // call the WORKLST append, and return the occurrence.
  //
  worklist->Append_occurrence(occurs);
  return occurs;
} // VALNUM_FRE::_append_phi_pred_occurrence


void
VALNUM_FRE::_insert_valnum_phi(EXP_WORKLST *worklist)
{
   // Step 4: Value number phi insertion.  This is similar to
   // EXP_WORKLST::Insert_exp_phi, but we need not concern ourselves 
   // with variables contained within expressions, nor with the form
   // of the lexical expressions, here.
   //
   // Build expression phi list and phi's predecessor list, both in DPO.
   //
   Is_True(worklist->Pre_kind() == PK_VNFRE, 
	   ("VALNUM_FRE::_insert_valnum_phi: Wrong kind"));

   Is_Trace(_tracing,
	    (TFile, "====== VALNUM_FRE::_insert_valnum_phi ======\n"));
   Is_Trace(_tracing,
	    (TFile, "The real occurrence list is:\n"));
   Is_Trace_cmd(_tracing, worklist->Real_occurs().Print(TFile));

   OPT_POOL_Push(_lpool, -1);
   {
      BB_NODE_SET      &phi_list = _etable->Reuse_phi_list();
      BB_LIST_CONTAINER bb_worklist;
      EXP_OCCURS       *exp_occ;
      EXP_OCCURS_ITER   exp_occ_iter;
      BB_NODE          *bb_orig;
      BB_NODE          *bb_phi;
      BB_NODE_SET_ITER  df_iter;

      // Add elements of the immediate dominance frontier to the
      // phi_list and to the worklist for each real occurrence.
      //
      FOR_ALL_NODE (exp_occ, exp_occ_iter, 
		    Init(worklist->Real_occurs().Head()))
      {
	 bb_orig = exp_occ->Bb();
	 FOR_ALL_ELEM (bb_phi, df_iter, Init(bb_orig->Dom_frontier()))
	 {
	    if (!phi_list.MemberP(bb_phi->Dom_dfs_id()))
	    {
	       Is_Trace(_tracing,
			(TFile, "------ Enter Phi-node: %d ------\n", 
			bb_phi->Id()));
	       phi_list.Union1D(bb_phi->Dom_dfs_id());
	       bb_worklist.Append(bb_phi, _lpool);
	    }
	 }
      }
      
      // Propagate the phi nodes to the iterated dominance frontiers.
      //
      while (bb_orig = bb_worklist.Remove_head(_lpool))
      {
	 FOR_ALL_ELEM (bb_phi, df_iter, Init(bb_orig->Dom_frontier()))
	 {
	    if (!phi_list.MemberP(bb_phi->Dom_dfs_id()))
	    {
	       Is_Trace(_tracing,
			(TFile, "------ Enter Phi-node: %d ------\n",
			 bb_phi->Id()));
	       phi_list.Union1D(bb_phi->Dom_dfs_id());
	       bb_worklist.Append(bb_phi, _lpool);
	    }
	 }
      }

      // At this point, the phi_list contains all the BBs that need
      // Phi-insertion, and we can now create and insert the Phi occurrences
      // into the worklist.  Note that we use EXP_PHI for value numbers,
      // where:
      //
      //   EXP_WORKLST::E_num() == valnum.
      //
      IDTYPE dpo_id;
      FOR_ALL_NODE (dpo_id, df_iter, Init(&phi_list))
      {
	 bb_phi = _etable->Cfg()->Dpo_Bb(dpo_id);

	 Is_Trace(_tracing,
		  (TFile, 
		   "------ Generate EXP_PHI: %d ------\n", bb_phi->Id()));

	 EXP_PHI *new_phi = 
	    CXX_NEW(EXP_PHI(worklist->E_num() /*value number*/,
			    bb_phi->Phi_list()->In_degree(),
			    bb_phi,
			    _vpool),
		    _vpool);
	 EXP_OCCURS *new_occ = _append_phi_occurrence(new_phi, worklist);

	 // The following assumes the _table never will call destructors
	 // for new_occ (we use a local pool to allocate these).
	 //
	 _etable->Set_dpo_phi_occurs(bb_phi, new_occ);
	 bb_phi->Set_exp_phi(new_phi);
      }

      // Having inserted the phi occurrences into the worklist, we now
      // identify all phi-predecessor and accumulate them in the "phi_list".
      //
      BB_LIST_ITER bb_iter;
      phi_list.ClearD();
      FOR_ALL_NODE (exp_occ, 
		    exp_occ_iter, Init(worklist->Phi_occurs().Head()))
      {
	 bb_orig = exp_occ->Bb();
	 FOR_ALL_ELEM (bb_phi, bb_iter, Init(bb_orig->Pred()))
         {
	    Is_Trace(_tracing, (TFile, "------ Enter Phi-pred %d ------\n",
				bb_phi->Id()));
	    phi_list.Union1D(bb_phi->Dom_dfs_id());
	 }
      }
  
      // Insert all phi predecessors.  The "phi_list" is now really the
      // set of phi predecessor basic blocks.
      //
      FOR_ALL_NODE (dpo_id, df_iter, Init(&phi_list))
      {
	 bb_orig = _etable->Cfg()->Dpo_Bb(dpo_id);
	 Is_Trace(_tracing,
		  (TFile,"------ Generate Phi-pred occurrence %d ------\n", 
		   bb_orig->Id()));
	 EXP_OCCURS *phi_pred = _append_phi_pred_occurrence(bb_orig, worklist);
	 FOR_ALL_ELEM (bb_phi, bb_iter, Init(bb_orig->Succ()))
	 {
	    EXP_PHI *exp_phi = bb_phi->Exp_phi();
	    if (exp_phi != NULL)
	    {
	       INT32 opnd_num = bb_phi->Pred()->Pos(bb_orig);
	       exp_phi->Set_pred(opnd_num, phi_pred);
	    }
	 }
      }
   }
   OPT_POOL_Pop(_lpool, -1);
} // VALNUM_FRE::_insert_valnum_phi


// ---------------------------------------------------------------------------
// ------------- Methods for renaming occurrences for one valnum -------------
// ---------------------------------------------------------------------------

inline void
VALNUM_FRE::_create_new_version(EXP_OCCURS   *occur, 
				OCCURS_STACK &occurs_stack,
				EXP_WORKLST  &worklist)
{
  occur->Set_e_version(worklist.Cur_e_version());
  occurs_stack.push(occur);
  worklist.New_e_version();
}


void 
VALNUM_FRE::_rename_valnums(EXP_WORKLST &worklist, BOOL &renamed_ok)
{
   // This is based on the algorithm in opt_essa.cxx, named ESSA::Rename(),
   // but it is substantially simpler and therefore warrants this separate
   // implementation.  We here create version numbers for occurrences,
   // set up the use-def links, and initialize occurrence flags.
   //
   // Besides renaming we here also do some flags initialization:
   //
   //      * By default our _is_fully_avail() flag is set to TRUE (this is 
   //        really phi->not_user_avail() == FALSE).  This is initialized and
   //        propagated correctly later in worklist.Compute_fully_avail().
   //
   //      * We set the downsafe flag consistently to FALSE.
   //
   //      * We set Has_real_occ() to TRUE when a real occurrence is at the
   //        top of the stack upon encountering a phi predecessor occurrence.
   //
   OPT_STAB *opt_stab = _etable->Opt_stab();
   CODEREP  *cr = worklist.Real_occurs().Head()->Occurrence();

   renamed_ok = TRUE;
   OPT_POOL_Push(_lpool, -1);
   {
      OCCURS_VECTOR occurs_vec(0, (EXP_OCCURS*) NULL,
			       OCCURS_VECTOR::allocator_type(_lpool));
      OCCURS_STACK  occurs_stack(occurs_vec);

      // Set initial e-version to 1.
      //
      worklist.Init_e_version();

      // Iterate over the occurences list in dominator preorder
      //
      EXP_ALL_OCCURS_ITER exp_occ_iter(worklist.Real_occurs().Head(),
				       NULL, // Lftr stuff
				       worklist.Phi_occurs().Head(),
				       worklist.Phi_pred_occurs().Head(),
				       _etable->Exit_occurs().Head());

      BOOL        undef = FALSE;
      EXP_OCCURS *occur;
      FOR_ALL_NODE(occur, exp_occ_iter, Init())
      {
	 // Unwind the stack until all EXP_OCCUR on the stack dominates
	 // the current EXP_OCCUR
	 //
	 while (!occurs_stack.empty() &&
		!occurs_stack.top()->Bb()->Dominates(occur->Bb()))
	    occurs_stack.pop();

	 switch (occur->Occ_kind())
	 {
	 case EXP_OCCURS::OCC_PHI_OCCUR:
	    // Create new version and push onto version stack.  We always
	    // consider phi nodes NOT downsafe for a fully available
	    // redundancy elimination, so we also mark that here.
	    //
	    Is_True(occur->Exp_phi()->Result() == occur,
		    ("OCC_PHI_OCCUR phi result is wrong."));
	    _create_new_version(occur, occurs_stack, worklist);
	    occur->Exp_phi()->Set_not_down_safe();
	    break;

	 case EXP_OCCURS::OCC_REAL_OCCUR:
	    // Work around for bug #663599 (should also fix htable to avoid
	    // sharing of volatile iloads).  This ensures that we do not do
	    // VNFRE for what could be occurrences that should have been
	    // given different value numbers, but that are represented by
	    // a common coderep.
	    //
	    if (_contains_undef_val(occur->Occurrence(), occur->Stmt()))
	    {
#ifdef Is_True_On
	       Is_Trace(_tracing,
		 	(TFile, "-----------> "
		 	 "VNFRE renaming undefined occurrence (Sid%d)\n",
			 occur->Stmt()->Stmt_id()));
	       Is_Trace_cmd(_tracing, occur->Occurrence()->Print(2, TFile));
	       Is_Trace(_tracing, (TFile, "-----------\n"));
	       Is_Trace_cmd(_tracing, fflush(TFile));
#endif // Is_True_On

	       //    Is_True(!(undef || occur->Mult_real()),
	       //        ("Encountered two or more volatile/zero-version "
	       // 	"occurrences in same worklist in renaming phase"));

	       if (undef || occur->Mult_real())
		  renamed_ok = FALSE; // Two or more undef occurrences
	       else
		  undef = TRUE;
	    }
					      
	    if (occurs_stack.empty())  
	       _create_new_version(occur, occurs_stack, worklist);
	    else
	    {
	       // Use version on top of stack and push this occurrence
	       //
	       EXP_OCCURS *tos = occurs_stack.top();
	       occur->Set_e_version(tos->E_version());
	       occur->Set_def_occur(_def_occur(tos) != NULL? 
				    _def_occur(tos) : tos);

	       // Push a real occurrence onto the stack if tos is a phi
	       // occurrence.  This is necessary to correctly set the
	       // Has_real_occ() flag when subsequently encountering a
	       // PHI_PRED_OCCUR node.
	       //
	       if (tos->Occ_kind() != EXP_OCCURS::OCC_REAL_OCCUR)
		  occurs_stack.push(occur);
	    }
	    break;

	 case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
	    {
	       // Visit all phi's in the successor BBs, and set the flags
	       // and operands accordingly (We can have more than one
	       // successor BB for unstructured code).
	       //
	       EXP_OCCURS  *tos = 
		  (occurs_stack.empty()? NULL : occurs_stack.top());

	       BB_LIST_ITER bb_iter;
	       BB_NODE     *bb_phi;                 // BB of the EXP_PHI
	       BB_NODE     *bb_pred = occur->Bb();  // BB of the PHI_PRED node

	       FOR_ALL_ELEM (bb_phi, bb_iter, Init(bb_pred->Succ()))
	       {
		  INT      opnd_num = bb_phi->Pred()->Pos(bb_pred);
		  EXP_PHI *exp_phi = bb_phi->Exp_phi();

		  // Not every successor need to be part of the sparse graph.
		  //
		  if (exp_phi != NULL)
		  {
		     // At this point every exp_phi has a Pred(), but only
		     // the occurrences that at this point have a phi or real
		     // occurrence as the top-of-stack (tos), will get a 
		     // non-NULL Opnd().
		     //
		     Is_True(exp_phi->E_num() == worklist.E_num(),
			     ("ETABLE::Add_to_occ_freelist() did not "
			      "correctly set bb->Exp_phi(); error in "
			      "VALNUM_FRE::_rename_valnums!"));

		     if (tos != NULL)
		     {
			if (tos->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR)
			   exp_phi->Set_has_real_occ(opnd_num);

			// Always set the operand to be the Def() if the
			// TOS is not itself a def.
			//
			if (_def_occur(tos) != NULL)
			   exp_phi->Set_opnd(opnd_num, _def_occur(tos));
			else
			   exp_phi->Set_opnd(opnd_num, tos);
		     }
		  } // if in sparse graph
	       } // for all successor phi nodes
	    }
	    break;

	 case EXP_OCCURS::OCC_EXIT_OCCUR:
	    Is_True(occur->Bb()->Kind() == BB_EXIT, ("not an exit BB."));
	    // TODO: If we ever need to set downsafe correctly we should here
	    // do something similar to ESSA::Reset_tos_downsafe().
	    //
	    break;

	 default:
	    Is_True(FALSE, 
		    ("VALNUM_FRE::_rename_valnums, "
		     "unknown occurrence kind: %d", occur->Occ_kind()));
	    break;
	 }
      }
   }
   OPT_POOL_Pop(_lpool, -1);
} // VALNUM_FRE::_rename_valnums


// ---------------------------------------------------------------------------
// --------------- Methods for setting flags for finalize step  --------------
// ---------------------------------------------------------------------------

void 
VALNUM_FRE::_propagate_flags(EXP_WORKLST &worklist,         // in/out
			     BOOL        &found_redundancy) // out
{
   // PRECONDITION: !_is_fully_avail(phi) for all phi occurrences.  We
   //  both calculate the start set (SER_AVAIL_SEARCH::Start_from) and
   //  propagate the flag by means of worklist.Compute_fully_avail().
   //
   // POSTCONDITION:
   //
   //  * _is_fully_avail(phi) is set using EXP_WORKLST::Compute_user_avail()
   //    in opt_eavail.cxx.  We further reduce the set of fully_avail phi
   //    nodes by backward propagation of the Stop() flag from real
   //    occurrences in EXP_WORKLST::Compute_fully_avail_stops().
   //
   //  * Cant_be_avail(phi) = !_is_fully_avail(phi).
   //
   //  * Partial_avail(phi) = _is_fully_avail(phi).
   //
   //  * Has_real_occ(phi) = set correctly (in _rename_valnums).
   //
   //  * Downsafe(phi) = FALSE (set in _rename_valnums).
   //
   //  * phi->Stops() = _is_fully_avail(phi) (same as "not later").  This
   //    could also be done with worklist.Compute_stops(_etable->Tracing()),
   //    but the results should be equivalent.
   //
   //  * found_redundancy = TRUE iff there is at least one real occurrence
   //    with either a real occurrence or a _is_fully_avail() phi occurrence
   //    as Def_occur().
   //
   // With these settings WillBeAvail, as referred to in the SSAPRE journal
   // paper, will be equal to _is_fully_avail().
   //

   // First compute availablity on phi nodes by forward propagation
   // algorithm, then find all fully_avail phi nodes with a subsequent
   // use of the result by either another fully_avail phi node or a real
   // occurrence (backward propagation) and set phi->Stops() to TRUE
   // for these.  As a side-effect of the "stops" calculation we also
   // determine whether or not there is any potential for full redundancy 
   // elimination with this worklist.
   //
   worklist.Compute_fully_avail(_etable);
   worklist.Compute_fully_avail_stops(_etable, found_redundancy);

   if (found_redundancy)
   {
      EXP_OCCURS     *occ;
      EXP_OCCURS_ITER occ_iter;

      // Next reset fully_avail when Stop is FALSE, and also set the 
      // Cant_be_avail() and Partial_avail() flags.
      //
      FOR_ALL_NODE (occ, occ_iter, Init(worklist.Phi_occurs().Head()))
      {
	 EXP_PHI * const phi = occ->Exp_phi();

	 // Update remaining flags.
	 //
	 if (_is_fully_avail(phi))
	 {
	    if (!phi->Stops())
	    {
	       phi->Set_not_user_avail();
	       phi->Set_cant_be_avail();
	    }
	    else
	    {
	       phi->Set_partial_avail();
	    }
	 }
	 else
	 {
	    phi->Set_cant_be_avail();
	 }
      }
   } // if found redundancy
      
} // VALNUM_FRE::_propagate_flags
   
   
// ---------------------------------------------------------------------------
// --------------------- Removal of redundnant Phi nodes  --------------------
// ---------------------------------------------------------------------------

void
VALNUM_FRE::_remove_redundant_phi_nodes()
{
   // TODO:
   Warn_todo("VALNUM_FRE::_remove_redundant_phi_nodes");
} // VALNUM_FRE::_remove_redundant_phi_nodes


// ---------------------------------------------------------------------------
// --------------------------- Constant propagation  -------------------------
// ---------------------------------------------------------------------------

void
VALNUM_FRE::_substitute_literal(VN_VALNUM v)
{
   // The value number was resolved to denote a literal value.
   //
   // Substitute all expressions that refer to this literal value, and 
   // that are not already literal expressions, with the literal value.
   //
   VN_EXPR::PTR       expr = _vn->valnum_expr(v);
   EXP_OCCURS * const head = _worklst(v)->Real_occurs().Head();

   Is_True(head != NULL && 
	   expr != NULL && expr->get_kind() == VN_EXPR::LITERAL, 
	   ("Erroneous application of VALNUM_FRE::_substitute_literal"));

   TCON            tcon = expr->get_tcon();
   EXP_OCCURS *    occ;
   EXP_OCCURS_ITER occ_iter;
   TYPE_ID vect_ty = (static_cast<const VN_LITERAL_EXPR*>(expr))->get_vect_type ();
   FOR_ALL_NODE(occ, occ_iter, Init(head))
   {
      _etable->Replace_by_const(occ, tcon, vect_ty);
   }
} // VALNUM_FRE::_substitute_literal


// ---------------------------------------------------------------------------
// --------------------- Tracing the VNFRE algorithm  --------------------
// ---------------------------------------------------------------------------

void
VALNUM_FRE::_trace_header()
{
   WN         * const wn_tree = _comp_unit->Input_tree();
   const char *       pu_name = 
      (WN_opcode(wn_tree) == OPC_FUNC_ENTRY?
       ST_name(WN_st(wn_tree)) : "<region>");
   
   if (pu_name == NULL)
      pu_name = "<unnamed pu>";
   
   Is_Trace(_tracing,
	    (TFile, "%sVNFRE (%s)\n%s", DBar, pu_name, DBar));
   VNFRE_TRACE(stderr, "%sVNFRE (%s)\n%s", DBar, pu_name, DBar);
} // VALNUM_FRE::_trace_header


// ---------------------------------------------------------------------------
// --------------------- The core of the VNFRE algorithm  --------------------
// ---------------------------------------------------------------------------


void
VALNUM_FRE::_expression_redundancy_elimination(VN_VALNUM v)
{
   // We have already gathered a worklist of real occurrences for
   // each this value number.  Any second order effects due to processing
   // other value numbers should already be reflected in _worklst(v).
   // We proceed with the following steps:
   //
   //   4) Insert Phi nodes and phi-predecessor nodes into the sparse SSA
   //      graph (i.e. into the worklist).
   //
   //   5) Rename all occurrences, creating a new hypothetical variable
   //      version at each first real occurrence or at each Phi node.
   //
   //   6) Propagate and set flag values regarding availability of values.
   //
   //   7) VNFRE finalize step (idenifify saves and deletes of computations).
   //
   //   8) VNFRE code motion step (remove full redundancies and save values
   //      into temporaries).
   //
   // Note that the last step may invalidate our value numbering and
   // occurrence lists for other not yet processed value numbers.  This
   // is tested for in VALNUM_FRE::apply().
   //
   EXP_WORKLST * const worklist = _worklst(v);

   // Let the etable know about this redundancy elimination invocation
   // in terms of the real occurrences for this value number and the
   // exit occurrences.
   //
   _etable->Init_vnfre_worklist(worklist, _exit_occurs);

   // Step 4: Given the real occurrences, insert the phi nodes.
   //
   SET_OPT_REPEAT_PHASE(_phi_placement_phase, "VNFRE: Valnum phi placement");
   _insert_valnum_phi(worklist);
   
   // Step 5: Renaming phase
   //
   SET_OPT_REPEAT_PHASE(_valnum_renaming_phase, "VNFRE: Valnum rename");
   BOOL renamed_ok;
   _rename_valnums(*worklist, renamed_ok);

   // Step 6: NOT(Fully_avail) flag propagation and setting of flags.
   // As a side effect we also compute the def-use info at this point.
   //
   OPT_POOL_Push(_lpool, -1);
   if (renamed_ok)
   {
      BOOL found_redundancy;
	 
      SET_OPT_REPEAT_PHASE(_avail_insert_phase,
			   "VNFRE: Valnum flag setting and propagation");
      _propagate_flags(*worklist, found_redundancy); // Creates du-info

      if (found_redundancy)
      {
	 if (WOPT_Enable_Worklist_Pruning)
	 {
	    SET_OPT_REPEAT_PHASE(_worklst_prune_phase,
				 "VNFRE: Phi/phi-pred pruning");
	    worklist->Prune_phi_phi_pred(_etable);
	 }

	 // Step 7: Finalize step.
	 //
	 SET_OPT_REPEAT_PHASE(_finalize_phase,
			      "VNFRE: Compute var save/reload");
	
	 BOOL optimization_needed =
	    worklist->Compute_save_delete(_etable->Htable(), _etable, NULL);

	 if (optimization_needed)
	 {
	    if (WOPT_Enable_SSA_Minimization)
	    {
	       SET_OPT_REPEAT_PHASE(_ssa_min_phase, "VNFRE: SSA minimization");
	       worklist->Minimize_temp_ssa(_etable, _tracing); //Uses du-info
	    }

	    // Step 8: Code-motion step.
	    //
	    SET_OPT_REPEAT_PHASE(_codemotion_phase, "VNFRE: CodeMotion");
	    worklist->Generate_save_reload(_etable);
	    SET_OPT_REPEAT_PHASE(_vnfre_misc_phase, "VNFRE: miscellaneous");

	    // Verify that the end result is correct after transformations.
	    //
	    if (WOPT_Enable_Verify >= 4)
	    {
	       Is_True(_comp_unit->Verify_CODEMAP(), ("CODEMAP corrupted."));
	       _comp_unit->Verify_version();
	    }
	 }
	 else
	 {
	    VNFRE_TRACE(stderr,
			"VNFRE::expression_redundancy_elimination: "
			"skipping code motion step for valnum %d\n",
			v.ordinal());
	 }
      }
      else 
      {
	 VNFRE_TRACE(stderr,
		     "VNFRE::expression_redundancy_elimination: "
		     "No redundancy for valnum %d\n",
		     v.ordinal());
      }
   }
   else
   {
      Is_Trace(_tracing,
	       (TFile, "---> VNFRE::expression_redundancy_elimination: "
		"Renaming failed for valnum %d\n",
		v.ordinal()));
   }
   OPT_POOL_Pop(_lpool, -1); // We do not free du-info until here!
      
   // Reset the etable for the next value number to be processed (this
   // will also remove occurrences from the worklist.
   //
   _etable->Reset_vnfre_worklist();

} // VALNUM_FRE::_expression_redundancy_elimination


// --------------------------------------------------------------
// ---------- The algorithm exported by this module -------------
// --------------------------------------------------------------

 
void 
VNFRE::remove_redundancies(VN        &vn,
			   ETABLE    &etable,
			   COMP_UNIT *comp_unit)
{
   // The Etable()->Pre_kind() need not be PK_VNFRE prior to this
   // call, since it will be (re)set to PK_VNFRE as a result of this
   // call.  This will allow us to piggy-back off any ETABLE kind
   // and reuse free-lists of occurrences.
   //
   // Postcondition: Etable()->Pre_kind() == PK_VNFRE.
   //
   VALNUM_FRE fre(vn, etable, comp_unit);

   VALNUM_FRE::Set_current(&fre);
   fre.apply(); // Carry out the algorithm
   VALNUM_FRE::Set_current(NULL);
   
} // VNFRE::remove_redundancies


UINT32
VNFRE::get_valnum(const CODEREP *cr)
{
   const VN::EXPRID id = cr->Coderep_id();
   
   if (id == 0 || id > VALNUM_FRE::Current()->last_exprid())
      return VALNUM_FRE::Current()->compute_valnum(cr).ordinal();
   else
      return VALNUM_FRE::Current()->get_valnum(id).ordinal();
} // VNFRE::get_valnum


void
VNFRE::add_valnum(const CODEREP *cr, UINT32 valnum)
{
   const VN::EXPRID id = cr->Coderep_id();

   Is_True(id != 0 && 
	   valnum <= VALNUM_FRE::Current()->last_valnum().ordinal(), 
	   ("Illegal attempt to map coderep (%u) to valnum (%u) in VNFRE",
	    id, VALNUM_FRE::Current()->last_valnum().ordinal()));

   if (id > VALNUM_FRE::Current()->last_exprid())
      VALNUM_FRE::Current()->new_cr(cr, VN_VALNUM::Vn(valnum));
   else if (VALNUM_FRE::Current()->get_valnum(id).ordinal() != valnum)
      VALNUM_FRE::Current()->reset_valnum(cr, VN_VALNUM::Vn(valnum));   
} //VNFRE::add_valnum


void
VNFRE::replace_occurs(const CODEREP *old_cr, 
		      CODEREP       *new_cr, 
		      const STMTREP *stmt)
{
   if (old_cr != new_cr && !(old_cr->Non_leaf() && old_cr->Opr() == OPR_PARM))
   {
      VALNUM_FRE::Current()->replace_cr_in_stmt(old_cr, new_cr, stmt);
   }
} //VNFRE::replace_occurs


void
VNFRE::move_rhs_occurs(const STMTREP *old_stmt, 
		       STMTREP       *new_stmt)
{
   VALNUM_FRE::Current()->move_rhs_occurs(old_stmt, new_stmt);
} // VNFRE::move_rhs_occurs


void
VNFRE::new_occurs(STMTREP *new_stmt)
{
   INSERT_CR_OCCURS cr_inserter(VALNUM_FRE::Current());
   traverseSR(new_stmt, cr_inserter);
} //VNFRE::new_occurs


void
VNFRE::delete_occurs(const EXP_OCCURS *occur, 
		     const CODEREP    *within_cr)
{
   VALNUM_FRE::Current()->delete_all_occurs(occur, within_cr);
} //VNFRE::delete_occurs


EXP_WORKLST *
VNFRE::get_worklst(const CODEREP *cr)
{
   return 
      (VALNUM_FRE::Current()->
       get_worklst(VALNUM_FRE::Current()->
		   get_valnum(cr->Coderep_id())));
} // VNFRE::get_worklst


// --------------------------------------------------------------
// ------- Extensions to other objects to support VNFRE ---------
// --------------------------------------------------------------
 
void COMP_UNIT::Do_vnfre(BOOL before_epre)
{
   // This is modelled after COMP_UNIT::Do_new_pre().
   //
   BOOL doit;

   switch (WOPT_Enable_Value_Numbering)
   {
   case VNFRE_AFTER_EPRE:
   case VNFRE_SINGLE_PASS_AFTER_EPRE:
      doit = !before_epre;
      break;
   case VNFRE_BEFORE_AND_AFTER_EPRE:
   case VNFRE_SINGLE_PASS_BEFORE_AND_AFTER_EPRE:
      doit = TRUE;
      break;
   default:
      doit = FALSE;
      break;
   }
   
   if (doit)
   {
      MEM_POOL etable_pool, phi_pool, etable_local_pool;

      OPT_POOL_Initialize(&etable_pool, "etable pool", FALSE, -1);
      OPT_POOL_Initialize(&phi_pool, "phi pool", FALSE, -1);
      OPT_POOL_Initialize(&etable_local_pool, "etable local pool", FALSE, -1);
      OPT_POOL_Push(&etable_pool, -1);
      OPT_POOL_Push(&phi_pool, -1);
      OPT_POOL_Push(&etable_local_pool, -1);

      {
	 ETABLE etable(Cfg(), Opt_stab(), Htable(), Arule(), 10,
		       &etable_pool, &phi_pool, &etable_local_pool,
		       this, PK_VNFRE);

	 etable.Perform_VNFRE_optimization();
      } // the etable and vn destructors are called here

      OPT_POOL_Pop(&etable_local_pool, -1);
      OPT_POOL_Pop(&phi_pool, -1);
      OPT_POOL_Pop(&etable_pool, -1);
      OPT_POOL_Delete(&etable_local_pool, -1);
      OPT_POOL_Delete(&phi_pool, -1);
      OPT_POOL_Delete(&etable_pool, -1);
   } // if (doit)
} // COMP_UNIT::Do_vnfre


void
ETABLE::Perform_VNFRE_optimization(void)
{
   // This is based upon ETABLE::Perform_EPRE_optimization()
   //
   const INT orig_coderep_id_cnt = Htable()->Coderep_id_cnt();

   Is_True(Pre_kind() == PK_VNFRE,
	   ("Illegal method EPRE::Perform_VNFRE_optimization"));
   
   // Even though they are useless, these structures need to be around
   // since they are actually referenced to determine whether or not they
   // are applicable in cse_pass_2().
   //
   _lftr = CXX_NEW(LFTR(this, Htable(), Cfg(), LFTR_HASH_SIZE), Etable_pool());
   _str_red = NULL;
   
   Cfg()->Dpo_vec();  // To initialize Dpo_vec before Cfg()->Dpo_Bb(dpo_id)
   Cfg()->Reset_stmt_id();

   SET_OPT_PHASE("Offline value numbering");
   {
      // Determine the value numbering algorithm to be employed.
      //
      VN::VN_ALGORITHM vn_algo;
      switch (WOPT_Enable_Value_Numbering)
      {
      case VNFRE_AFTER_EPRE:
      case VNFRE_BEFORE_AND_AFTER_EPRE:
	 vn_algo = VN::ITERATIVE;
      break;
      case VNFRE_SINGLE_PASS_AFTER_EPRE:
      case VNFRE_SINGLE_PASS_BEFORE_AND_AFTER_EPRE:
	 vn_algo = VN::SINGLE_PASS;
	 break;
      default:
	 FmtAssert(FALSE, 
		   ("Unexpected VN_ALGORITHM in "
		    "ETABLE::Perform_VNFRE_optimization()"));
	 break;
      }

      // Do the value numbering.
      //
      VN vn(vn_algo, Cfg(), Htable(), Etable_local_pool(), Etable_pool());

      // Write the value numbering to file if the cfg is also dumped 
      // to file.
      //
      if (Get_Trace(TP_GLOBOPT, CR_DUMP_FLAG))
      {
	 vn.print(TFile);
      }

      // Carry out redundancy elimination.
      //
      SET_OPT_PHASE("VNFRE");
      VNFRE::remove_redundancies(vn, *this, _comp_unit);
   }
   
   // Write out statistics.
   //
   if (Tracing())
   {
      fprintf(TFile, "%sAfter VNFRE\n%s", DBar, DBar);
      fprintf(TFile, "Statistics (all expressions): Insert Count %d, "
	      "Save Count %d, Reload Count %d, Temp Phi Count %d, "
	      "Hoisted Count %d\n",
	      _num_inserted_saves, _num_cse_saves, _num_cse_reloads, 
	      _num_temp_phis, _num_hoisted);
      fprintf(TFile, "Coderep Statistics (entire PU): previous count: "
	      "%d new count: %d\n", 
	      orig_coderep_id_cnt, Htable()->Coderep_id_cnt());
      fprintf(TFile, "     Expr nodes changed to temps without rehashing: "
	      "%d\n",
	      _num_temp_owners);
      Cfg()->Print(TFile);
#ifdef Is_True_On
      if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
	 MEM_Trace();
      }
#endif
   }

   // CXX_DELETE(_str_red,_etable_pool);
   CXX_DELETE(_lftr,_etable_pool);

}  // ETABLE::Perform_VNFRE_optimization


static void
Propagate_stops_for_fully_avail(EXP_PHI *phi)
{
   // Helper function for EXP_WORKLST::Compute_fully_avail_stops.
   // Recursively updates Stops bit for phi along use-def edges.
   //
   if (!phi->Not_user_avail() && !phi->Stops())
   {
      phi->Set_stops(); // Fully avail and not already set.

      for (INT i = phi->Opnd_count() - 1; i >= 0; i--)
      {
	 EXP_OCCURS *def = phi->Opnd(i);

	 if (def != NULL && def->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR)
	    Propagate_stops_for_fully_avail(def->Exp_phi());
      }
   }
} // Propagate_stops_for_fully_avail


void
EXP_WORKLST::Compute_fully_avail_stops(ETABLE *etable, BOOL &found_redundancy)
{
   // Initialize phi->Has_real_use(), which we use later to determine potential
   // redundancy.
   //
   EXP_OCCURS     *occ;
   EXP_OCCURS_ITER occ_iter;

   FOR_ALL_NODE (occ, occ_iter, Init(Phi_occurs().Head()))
      occ->Exp_phi()->Reset_has_real_use();
   
   // Calculate the start set for backward propagation of the Stop bit,
   // and also set found_redundancy.
   //
   found_redundancy = FALSE;
   FOR_ALL_NODE (occ, occ_iter, Init(Real_occurs().Head()))
   {
      EXP_OCCURS * const def = occ->Def_occur();

      if (occ->Mult_real())
	 found_redundancy = TRUE;

      if (def != NULL)
      {
	 if (def->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR)
	 {
	    if (def != occ)
	       found_redundancy = TRUE;
	 }
	 else
	 {
	    EXP_PHI *phi = def->Exp_phi();

	    Propagate_stops_for_fully_avail(phi);

	    if (!phi->Not_user_avail())
	       found_redundancy = TRUE; // fully available
	    else if (phi->Has_real_use())
	       found_redundancy = TRUE; // Second real use may imply redundancy
	    else 
	       phi->Set_has_real_use(); // First real use of phi result
	 }
      }
   }
} // EXP_WORKLST::Compute_fully_avail_stops
