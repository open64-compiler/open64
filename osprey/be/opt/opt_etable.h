/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_etable.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_etable.h,v $
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
//      Data structures and functions for PRE optimization based on
//      SSA.
//
//      EXP_PHI:       expression phi function;
//               the body of this class contains:
//
//        _e_num:      assigned expression number
//        _size:       number of operand + 1 for the result
//        _bb:         the basic block it belongs to
//        _vec:        a fix length array contains _size of elements
//               each element contains:
//
//        _occ:        the expression occurrence (EXP_OCCURS) node
//        _flags:      could contain multiple flags in EXP_PHI_FLAGS
//        _uses:       holds a list of expression phi nodes that use
//                     (as operand) the result of this phi.
//
//
//      Memory Management for major classes:
//
//        Memory pool: _etable_pool
//                     The routine that instantiate the ETABLE has to
//                     create a dedicated memory pool and pushed
//                     before passing it to ETABLE constructor.
//
//        EXP_PHI:     the node itself is recyclable after each
//                     expression is processed.  Since the lifecycle
//                     of any EXP_PHI node is the same as the time we
//                     process that expression, the MEM_Push/Pop works
//                     just fine.
//
//        EXP_OCCURS:  recycled by EXP_WORKLST::Remove_occurs. All
//                     extended real occurrences(real + phi result)
//                     and all phi occurrences are freed by
//                     ETABLE::Add_to_occ_freelist().
//
//        EXP_WORKLST: the node itself is an array element of the
//                     segmented array ETABLE::_exp_worklst.  These
//                     nodes are not freed though out this phase, and
//                     it is freed by the memory pool pop at the
//                     ETABLE instantiator.
//
// ====================================================================
// ====================================================================


#ifndef opt_etable_INCLUDED
#define opt_etable_INCLUDED "opt_etable.h"

#include "defs.h"
#include "mempool.h"
#include "cxx_template.h"
#include "opt_defs.h"
#include "opt_array.h"
#include "cxx_base.h"
#include "opt_base.h"
#include "bb_node_set.h"
#include "opt_htable.h"
#include "opt_vnfre.h"

class ALIAS_RULE;
class BB_NODE;
class BB_NODE_SET;
class CODEMAP;
class CODEREP;
class COMP_UNIT;
class CFG;
class ETABLE;
class EXP_OCCURS;
class EXP_ALL_REAL_ITER;
class EXP_WORKLST;
class LFTR;
class OPT_STAB;
class PHI_NODE;
class STMTREP;
class STR_RED;
class E_VER_TAB;	 // Private to availability/insertion step
class REHASH_CACHE_LIST; // list of rehashed expr
class EXP_HOISTING;
class SSU;
class OCCUR_REPLACEMENT; // Private to opt_etable.cxx (for codemotion)

enum PRE_KIND {
  PK_EPRE  = 0,
  PK_LPRE  = 1,
  PK_SPRE  = 2,
  PK_VNFRE = 3,  // Value numbering full redundancy elimination
};

extern const char *pre_kind_name(PRE_KIND);

class REHASH_INFO;
template <class NODE_TYPE, class KEY_TYPE> class ID_MAP;

class EXP_OCCURS_LIST : public SLIST_NODE {
private:
  EXP_OCCURS *_node;
  mINT32      _opnd_idx;

              EXP_OCCURS_LIST(const EXP_OCCURS_LIST&);
              EXP_OCCURS_LIST& operator = (const EXP_OCCURS_LIST&);
public:
              EXP_OCCURS_LIST(void)        { }
              EXP_OCCURS_LIST(EXP_OCCURS *node, mINT32 i)
					   { Init(node, i); }
             ~EXP_OCCURS_LIST(void)        { }

  DECLARE_SLIST_NODE_CLASS(EXP_OCCURS_LIST)

  void        Init(EXP_OCCURS *nd, mINT32 i)
    					   { _node = nd; _opnd_idx = i; }

  EXP_OCCURS *Node(void) const             { return _node; }

  mINT32      Opnd_idx(void) const         { return _opnd_idx; }
};

class USE_LIST_ENTRY : public SLIST_NODE {
private:
  EXP_PHI *_node;
  mINT32   _opnd_idx;

              USE_LIST_ENTRY(const USE_LIST_ENTRY&);
              USE_LIST_ENTRY& operator = (const USE_LIST_ENTRY&);
public:
              USE_LIST_ENTRY(void)         { }
              USE_LIST_ENTRY(EXP_PHI *node, mINT32 i)
					   { Init(node, i); }
              ~USE_LIST_ENTRY(void)        { }

  DECLARE_SLIST_NODE_CLASS(USE_LIST_ENTRY)

  void        Init(EXP_PHI *node, mINT32 i)
                                           { _node = node; _opnd_idx = i; }

  EXP_PHI    *Node(void) const             { return _node; }

  mINT32      Opnd_idx(void) const         { return _opnd_idx; }
};

class USE_LIST_ITER : public SLIST_ITER {
public:
  // Declared by hand since DECLARE_SLIST_ITER_CLASS requires
  // extraneous garbage.
  USE_LIST_ITER(USE_LIST_ENTRY *nd) { SLIST_ITER::Init(nd); }
  USE_LIST_ITER(void)               { SLIST_ITER::Init();   }
  USE_LIST_ENTRY *First(void)
    { return (USE_LIST_ENTRY *) SLIST_ITER::First(); }
  USE_LIST_ENTRY *Next(void)
    { return (USE_LIST_ENTRY *) SLIST_ITER::Next(); }
};

class EXP_PHI : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(EXP_PHI)
friend class ETABLE;
public:
  // Current flag semantics:
  //
  // EPOF_HAS_REAL_OCC means that the corresponding phi operand has a
  // real occurrence on the path in the CFG from its point of
  // definition to this use as a phi operand.
  //
  // EPOF_DELAYED_RENAME means the renaming step for this opnd has
  // been delayed.  (this bit should be not visible outside step 3).
  //
  // EPOF_INJURED means the phi operand is "injured" for strength-
  // reduction purposes.

  enum EXP_PHI_OPND_FLAGS {
    EPOF_NONE           = 0x0,
    EPOF_HAS_REAL_OCC   = 0x1,
    EPOF_DELAYED_RENAME = 0x2,
    EPOF_INJURED        = 0x4,
    EPOF_OPND_STOPS     = 0x8,
    EPOF_NULL_SSU_VERSION = 0x10,// set by SPRE iphi insertion and used in
			 	 // SPRE SSU renaming phase
  };

  // EPF_I_A_SEEN is used only within Identity_assignment in the
  // phi result to mark the phi as being processed, to avoid infinite
  // loop and to return a cached answer if the result is already known.
  //
  // EPF_NOT_DOWN_SAFE means that the phi's block in the CFG is not
  // down-safe, so insertion along the CFG arcs coming into this
  // phi is not safe (because the expression is not fully
  // anticipated at this point in the CFG). This flag is set in
  // step 3 (opt_essa.cxx) and step 4 (opt_eant.cxx), and is used in
  // step 4 (opt_eant.cxx) and step 5 (opt_epre.cxx).
  //
  // EPF_IDENTITY means that a variable phi for the temp need not be
  // generated in correspondence with this EXP_PHI.
  //
  // EPF_IDENT_INJURED is set for identity phi's in the SSA
  // minimization step whenever an injury repair is required between
  // the occurrence the present phi is Identical_to() and the present
  // phi.
  //
  // EPF_KNOW_REPLACEMENT is set for identity phi's in the SSA
  // minimization step once we know what the phi is Identical_to().

  enum EXP_PHI_FLAGS {
    EPF_NONE             = 0x0,
    EPF_I_A_SEEN         = 0x1,
    EPF_NOT_DOWN_SAFE    = 0x2,
    EPF_IS_LIVE          = 0x4,   // expr is PANT at this phi
    EPF_NOT_USER_AVAIL   = 0x8,   // expr is !AV  at this phi
    EPF_CANT_BE_AVAIL    = 0x10,  // this phi result can't be made available
    EPF_STOPS            = 0x20,  // can't insert later than this phi
    EPF_IDENTITY         = 0x40,  // this phi need not become a temp phi
    EPF_KNOW_REPLACEMENT = 0x80,  // this identity phi's replacement is known
    EPF_IDENT_INJURED    = 0x100, // this phi's identity is injured
    EPF_REVERSE_PHI      = 0x400, // used only in SPRE to represent SSU
    EPF_DEAD_PHI_REGION = 0x800, // used only in SPRE to prevent insertions in such regions
    EPF_PARTIAL_AVAIL   = 0x1000,
    EPF_LOCAL_PRED       = 0x2000 // used in local phases for various reasons
  };

private:
  EXP_ID      _e_num;
  mINT32      _opnd_count;
  UINT16      _flags;
  UINT16      _saved_flags;

  union {
          EXP_OCCURS *_cached_identity_assignment;
	  EXP_OCCURS *_identical_to;
  };

  BB_NODE    *_bb;
  EXP_OCCURS *_result;

  class PHI_ELEM {
  friend class EXP_PHI;
    EXP_OCCURS *_occ;
    UINT32      _flags;
    EXP_OCCURS *_pred_or_succ;             // pointer to its predecessors if it is an EXP_PHI
                                           // pointer to its succ if it is an IPHI
  };
  PHI_ELEM    *_vec;


  //              _uses:       holds a list of phi occurrences that are
  //                           uses of the phi result of this occurrence.
  USE_LIST_ENTRY *_uses;    // used only during
                            // EXP_WORKLST::Compute_forward_attributes() and
                            // EXP_WORKLST::Minimize_temp_ssa()

                  EXP_PHI(void);
                  EXP_PHI(const EXP_PHI&);
                  EXP_PHI& operator = (const EXP_PHI&);

  void            Set_result(EXP_OCCURS *o){ _result = o; }

  void            Set_cached_identity_assignment(EXP_OCCURS *occ)
    { _cached_identity_assignment = occ; }

  EXP_OCCURS     *Cached_identity_assignment(void)
    { return _cached_identity_assignment; }

  void            Set_flag(EXP_PHI_FLAGS bit)
    { _flags |= bit; }
  void            Reset_flag(EXP_PHI_FLAGS bit)
    { _flags &= ~bit; }
  BOOL            Is_flag_set(EXP_PHI_FLAGS bit) const
    { return _flags & bit; }

public:
  INT32           id;  // unique exp-phi id to support feedback PRE

                  EXP_PHI(EXP_ID    e_num, 
                          INT       opnd_count,
                          BB_NODE  *bb,
                          MEM_POOL *pool)  { Init(e_num,opnd_count,bb,pool); }
                 ~EXP_PHI(void)            { }

  void            Init(EXP_ID    e_num,
                       INT       size,
                       BB_NODE  *bb,
                       MEM_POOL *pool);

  EXP_ID          E_num(void) const        { return _e_num; }
  EXP_OCCURS     *Result(void) const       { return _result; }

  EXP_OCCURS     *Opnd(INT i) const        { return _vec[i]._occ; }
  EXP_OCCURS     *Pred(INT i) const        { return _vec[i]._pred_or_succ; }
  EXP_OCCURS     *Succ(INT i) const        { return _vec[i]._pred_or_succ; }
  void            Set_opnd(INT i,
                           EXP_OCCURS *occ){ _vec[i]._occ = occ; }
  void            Set_pred(INT i,
                           EXP_OCCURS *occ){ _vec[i]._pred_or_succ = occ; }
  void            Set_succ(INT i,
                           EXP_OCCURS *occ){ _vec[i]._pred_or_succ = occ; }
  INT             Opnd_count(void) const   { return _opnd_count; }
  BB_NODE        *Bb(void) const           { return _bb; }

  void            Set_not_down_safe(void)  { Set_flag(EPF_NOT_DOWN_SAFE); }
  void            Reset_not_down_safe(void){ Reset_flag(EPF_NOT_DOWN_SAFE); }
  BOOL		  Not_down_safe(void) const
    { return Is_flag_set(EPF_NOT_DOWN_SAFE); }

  void            Set_not_user_avail(void) { Set_flag(EPF_NOT_USER_AVAIL); }
  BOOL            Not_user_avail(void) const
    { return Is_flag_set(EPF_NOT_USER_AVAIL); }

  void            Set_cant_be_avail(void)  { Set_flag(EPF_CANT_BE_AVAIL); }
  BOOL            Cant_be_avail(void) const
    { return Is_flag_set(EPF_CANT_BE_AVAIL); }

  void		  Set_live(void)           { Set_flag(EPF_IS_LIVE); }
  BOOL		  Is_live(void) const
    { // return Is_flag_set(EPF_IS_LIVE);
      return TRUE;                         }

  void            Set_has_real_occ(INT i)
    { _vec[i]._flags |= EPOF_HAS_REAL_OCC; }
  BOOL            Has_real_occ(INT i) const
    { return (_vec[i]._flags & EPOF_HAS_REAL_OCC); }

  void            Set_identity(void)   { Set_flag(EPF_IDENTITY); }
  void            Reset_identity(void) { Reset_flag(EPF_IDENTITY); }
  BOOL            Identity(void) const
    { return Is_flag_set(EPF_IDENTITY); }

  void            Set_reverse_phi(void)  { Set_flag(EPF_REVERSE_PHI); }
  BOOL		  Reverse_phi(void) const
    { return Is_flag_set(EPF_REVERSE_PHI); }

  void            Set_identical_to(EXP_OCCURS *const occ)
    { _identical_to = occ; }
  EXP_OCCURS     *Identical_to(void) const
    {
      Is_True(Will_b_avail() && Identity(),
	      ("EXP_PHI::Identical_to: availability and "
	       "identity are required"));
      return _identical_to;
    }

  void            Set_identity_injured(void)
    { Set_flag(EPF_IDENT_INJURED); }
  BOOL            Identity_injured(void) const
    { return Is_flag_set(EPF_IDENT_INJURED); }

  void            Set_replacing_occur_known(void)
    { Set_flag(EPF_KNOW_REPLACEMENT); }
  BOOL            Replacing_occur_known(void) const
    { return Is_flag_set(EPF_KNOW_REPLACEMENT); }

  void		  Set_I_A_Seen(void)   { Set_flag(EPF_I_A_SEEN); }
  void		  Reset_I_A_Seen(void) { Reset_flag(EPF_I_A_SEEN); }
  BOOL		  I_A_Seen(void)const  { return Is_flag_set(EPF_I_A_SEEN); }

  void            Set_stops(void)      { Set_flag(EPF_STOPS); }
  BOOL            Stops(void) const    { return Is_flag_set(EPF_STOPS); }

  void            Set_dead_phi_region(void) { Set_flag(EPF_DEAD_PHI_REGION); }
  BOOL            Dead_phi_region(void) const { return Is_flag_set(EPF_DEAD_PHI_REGION); }

  BOOL            Will_b_avail(void) const
    { return !Cant_be_avail() && Stops(); }

  BOOL            Partial_avail() const { return Is_flag_set(EPF_PARTIAL_AVAIL); }

  void            Set_partial_avail() { Set_flag(EPF_PARTIAL_AVAIL); }

  void            Set_injured(INT i)
    { _vec[i]._flags |= EPOF_INJURED; }
  void            Reset_injured(INT i)
    { _vec[i]._flags &= ~EPOF_INJURED; }
  BOOL            Injured(INT i) const
    { return (_vec[i]._flags & EPOF_INJURED); }
  BOOL            Any_opnd_injured(void) const;

  void            Set_has_real_use() {Set_flag(EPF_LOCAL_PRED);}
  void            Reset_has_real_use() {Reset_flag(EPF_LOCAL_PRED);}
  BOOL            Has_real_use() {return Is_flag_set(EPF_LOCAL_PRED);}

  USE_LIST_ENTRY   *Uses(void) const         { return _uses; }

  void            Set_uses(USE_LIST_ENTRY *use)
    { _uses = use; }

  void            Add_use(EXP_PHI    *use,
			  INT         opnd_idx,
                          MEM_POOL   *pool)
    {
      USE_LIST_ENTRY *new_ll_node =
	CXX_NEW(USE_LIST_ENTRY(use, opnd_idx),
		pool);
      if (Uses() == NULL) {
	Set_uses(new_ll_node);
      }
      else {
	Uses()->Insert_After(new_ll_node);
      }
    }

  BOOL            Delayed_rename(INT i) const
                                           { return (_vec[i]._flags &
                                               EPOF_DELAYED_RENAME); }

  void            Set_delayed_rename(INT i){ _vec[i]._flags |=
                                               EPOF_DELAYED_RENAME; }

  void            Reset_delayed_rename(INT i)
                                           { _vec[i]._flags &= 
						 ~EPOF_DELAYED_RENAME; }

  void            Set_opnd_stops(INT i)    { _vec[i]._flags |=
					       EPOF_OPND_STOPS; }

  BOOL            Opnd_stops(INT i) const  { return (_vec[i]._flags &
						     EPOF_OPND_STOPS); }

  void            Set_null_ssu_version(INT i) { _vec[i]._flags |=
					       EPOF_NULL_SSU_VERSION; }

  BOOL            Null_ssu_version(INT i) const { return (_vec[i]._flags &
						     EPOF_NULL_SSU_VERSION); }
  BOOL            Identity_assignment(BOOL &, EXP_PHI *, BOOL, STACK<EXP_PHI*> &);
  
  BOOL            Need_insertion(INT) const;

  void            Save_flags() { _saved_flags = _flags; }
  void            Restore_flags() { _flags =_saved_flags; }

  // used in CANT_BE_AVAIL_SEARCH, after USER_AVAIL_SEARCH has been finished 
  BOOL            Phi_opnd_not_avail(INT) const;

  void            Print(FILE *fp=stderr, BOOL show_preds = TRUE) const;
};

class EXP_PHI_LIST : public SLIST {
  DECLARE_SLIST_CLASS (EXP_PHI_LIST, EXP_PHI)

private:
  INT32           _out_degree;            // for SPRE

  EXP_PHI_LIST(const EXP_PHI_LIST&);
  EXP_PHI_LIST& operator = (const EXP_PHI_LIST&);

public:
  EXP_PHI_LIST(INT32 out_degree)            { _out_degree = out_degree; }
  EXP_PHI_LIST(BB_NODE *bb);
  ~EXP_PHI_LIST(void)                       {}

  INT32           Out_degree() const        { return _out_degree; }

  // Remove the i'th operand from the phi-nodes (0 is first opnd)
  void            Remove_opnd(INT32 i);
  void            Print(FILE *fp=stderr);    
};


class EXP_PHI_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (EXP_PHI_LIST_ITER, EXP_PHI, EXP_PHI_LIST)

  EXP_PHI_LIST_ITER(const EXP_PHI_LIST_ITER&);
  EXP_PHI_LIST_ITER& operator = (const EXP_PHI_LIST_ITER&);

public:
  ~EXP_PHI_LIST_ITER(void)		{}

  EXP_PHI       *First_elem(void)       { return First(); }
  EXP_PHI       *Next_elem(void)        { return Next();  }
};


// ====================================================================
// EXP_OCCURS: This node represent one occurence of an expression as
//             the PRE candidate.  Members of this class are:
//             _occurrence:  the coderep node of this occurence.
//             _e_version:   the version number assigned by rename phase
//             _temp_cr:     coderep node of the temp corresponding to
//                           _e_version
//             _enclosed_in: the enclosing statement, from where we
//                           could find the BB it belongs to; or a
//                           phi_node.
//             _kind:        the kind of the occurrence
//             _flag:        reserve the location for all kinds of
//                           flags we might need.
//             _stmt_kid_num:for real occurrence, which kid of the
//                           stmt has the occurrence 
// ====================================================================
class EXP_OCCURS : public SLIST_NODE {

  DECLARE_SLIST_NODE_CLASS( EXP_OCCURS )

private:
  // A note on flags and unions: Any flag or field that can pertain to
  // a real occurrence has the same meaning for a phi-pred occurrence
  // because the insertion scheme we use embeds inserted real
  // occurrences in phi-predecessors.
  enum OCC_FLAG {
    OCC_NONE            = 0x000,
    OCC_MULT_REAL_OCCUR = 0x001, // multiple real occurrence in the same kid
    OCC_FAKE_STORE_OCCUR = 0x001,// used in SPRE to mark fake stores to locals
				 // at CFG exit
    OCC_INSERTED_OCCUR  = 0x002, // used only for OCC_PHI_PRED_OCCUR
				 // kind; means that an insertion has
				 // happened for the corresponding phi
				 // operand. OVERLOADED: same bit as
				 // OCC_CODEREP_OWNER. If you change
				 // this overloading, make sure to
				 // update the accessor functions,
				 // too.
    OCC_CODEREP_OWNER   = 0x002, // used only for OCC_REAL_OCCUR kind;
				 // means this occurrence owns its
				 // coderep and is allowed to
				 // substitute without rehashing.
				 // OVERLOADED: same bit as
				 // OCC_INSERTED_OCCUR. If you change
				 // this overloading, make sure to
				 // update the accessor functions,
				 // too.
    OCC_ENCL_STMT_SET   = 0x004, // used only for OCC_PHI_PRED_OCCUR kind;
                                 // means this occurrence was a PRE
				 // insertion and it didn't get
				 // hoisted to a different place.
    OCC_AS_L_VALUE      = 0x008, // occurrence of an ISTORE *p = ... in [EL]PRE
				 // and r = x in SPRE
    OCC_REQUIRED_PRED   = 0x008, // phi pred has a Will_b_avail() succ phi
    OCC_INJURED		= 0x010, // used only for OCC_REAL_OCCUR kind;
				 // modified by IV update statement
    OCC_DELETE_COMP     = 0x020, // used only for OCC_REAL_OCCUR kind;
                                 // means this real occurrence will not
				 // actually be computed in the
				 // optimized program (load from temp)
    OCC_SAVE_TO_TEMP    = 0x040, // used only for OCC_REAL_OCCUR kind
				 // with OCC_DELETE_COMP == FALSE; means
				 // this real occurrence will be saved
				 // to a temp (preg) because its result
				 // is used later.
    OCC_HOISTED         = 0x080, // a hoisted occurrence
    OCC_REHASH_CHANGED  = 0x100, // rehashing changed this inserted expr (CVT)
    OCC_FOR_SPRE        = 0x200, // this node is used in SPRE phase
    OCC_OBSOLETE_COMPARISON = 0x400, // comp occur node is obsolete and is to be
				     //ignored
    OCC_SUNK_L_VALUE    = 0x800, // in LPRE, when doing live range shrinking;
				 // if OCC_AS_L_VALUE is set, indicates the
				 // sunk def; otherwise, indicates the first use
				 // before which the sunk def is to be inserted
#if defined(TARG_SL) // PARA_EXTENSION
   OCC_IN_PARA_REGION  = 0x1000, // this occurrencse is enclosed in a sl2 parallel region (major or minor)
#endif 
  };

  UINT32          _kind:3;          // the kind of occurence
  UINT32          _flag:13;         // contains all kinds of flags
  UINT32          _stmt_kid_num:16; // for real occurrence, which kid
                                    // of the stmt has the occurrence
  UINT32          _saved_flags:13;

  union {
    CODEREP      *_occurrence;      // the occurence
  } _oc;
  IDTYPE          _e_version:24;    // the expression version number

  // Maximum representable rehash cost
  enum { OCC_REHASH_COST_LIMIT = 255 };

  UINT32	  _rehash_cost:8;   // for real occurrences, a measure
				    // of the cost of rehashing the
				    // occurrence in the expression
				    // tree. Currently, this is the
				    // maximum depth in the expression
				    // tree.
  union {
    EXP_OCCURS   *_def_occur;       // the OCC_PHI_OCCUR defines this
				    // OCC_REAL_OCC node, used in step 3;
				    // Also used in steps 5 and 6 for
				    // a different purpose: set up by
				    // step 5 to communicate the
				    // available definition of each
				    // real and comp occurrence to
				    // step 6.
    CODEREP      *_temp_cr;         // coderep node of the temp; this field
				    // is not allowed to be used whenver
				    // _def_occur is set (i.e. the expression
				    // occurrence is not a def); assertions
				    // enforce this
    EXP_WORKLST  *_spre_wk;         // for SPRE only: point to worklist
                                    // node, and used by rename phase.
                                    // After rename phase, this field get
                                    // invalid value.
  } _temp;			    // corresponding to _e_version, used in step 6
  
  union {
    STMTREP      *_stmt;            // inside this statement (real occ)
    EXP_PHI      *_exp_phi;         // defined by this phi node (phi occ)
    BB_NODE      *_bb;              // for exit occurrence or phi pred
  } _enclosed_in;

#if defined(TARG_SL) // PARA_EXTENSION
  RID* _rid;         // record the region id which the expression occurrence locate in. 
#endif 

                  EXP_OCCURS(const EXP_OCCURS&);
                  EXP_OCCURS& operator = (const EXP_OCCURS&);

  void            Reset_flag(void)         { _flag = OCC_NONE; }
  void            Reset_flag(OCC_FLAG f)   { _flag = (_flag & (~f)); }
  void            Set_flag(OCC_FLAG f)     { _flag = (_flag|f); }
  BOOL            Is_flag_set(OCC_FLAG f)
                                     const { return _flag & f; }

public:
  // Don't change the order of the items in the following type
  // definition; the OCC_KIND ordering is used to distinguish
  // dominance for occurrences of different kinds within the same
  // block. To wit: PHI_OCCURs dominate REAL_OCCURs, which dominate
  // PHI_PRED_OCCURs and EXIT_OCCURs. COMP_OCCURs are a special case
  // since they can appear interspersed with REAL_OCCURs, and must be
  // distinguished by statement ordering within the block.
  enum OCC_KIND {
    OCC_UNKNOWN_OCCUR  = 0,
    OCC_PHI_OCCUR      = 1, // DO NOT CHANGE THE ORDER of phi,
    OCC_REAL_OCCUR     = 2, // real,
    OCC_COMP_OCCUR     = 3, // comparison occurrence for LFTR
    OCC_PHI_PRED_OCCUR = 4, // phi_pred
    OCC_EXIT_OCCUR     = 5, // exit
#if defined(TARG_SL)
    OCC_PI_OCCUR       = 6, // the occurrence enclosed in a parallel region
#endif
  };

  enum E_VERSION_VALUE {
    ILLEGAL_E_VERSION = 0,	// illegal e-version
  };

                  EXP_OCCURS(CODEREP*, STMTREP*, EXP_PHI*, BOOL);
  void            Init(void);
                  EXP_OCCURS(void)         { Init(); }
                 ~EXP_OCCURS(void)         { }

  OCC_KIND        Occ_kind(void) const     { return (OCC_KIND)_kind; }
  void            Set_kind(OCC_KIND k)     { _kind = k; }

  mINT16          Stmt_kid_num(void) const
    { Is_True(Occ_kind() == OCC_REAL_OCCUR || Occ_kind() == OCC_COMP_OCCUR,
	      ("EXP_OCCURS::Stmt_kid_num: Must be real occ!"));
      return _stmt_kid_num;
    }
  CODEREP  	 *Occurrence(void)const    { return _oc._occurrence; }
  IDTYPE          E_version(void) const
    { Is_True(Occ_kind() != OCC_PHI_PRED_OCCUR || Inserted_computation() ||
	      Sunk_lvalue(),
	      ("EXP_OCCURS::E_version: Cannot be phi pred occ!"));
      return _e_version;
    }

  void             Set_rehash_cost(UINT32 rehash_cost)
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR ||
	      Occ_kind() == OCC_UNKNOWN_OCCUR,  // To allow initialization
	      ("EXP_OCCURS::Set_rehash_cost: Must be real occ!"));
      if (rehash_cost <= OCC_REHASH_COST_LIMIT) {
	_rehash_cost = rehash_cost;
      }
      else {
	_rehash_cost = OCC_REHASH_COST_LIMIT;
      }
    }

  UINT32          Rehash_cost(void) const
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR,
	      ("EXP_OCCURS::Rehash_cost: Must be real occ!"));
      return _rehash_cost;
    }

  CODEREP        *Temp_cr(void) const
    {
      // It is not kosher to use temp_cr in the case of:
      Is_True(!Is_flag_set(OCC_DELETE_COMP) &&    
	      (Occ_kind() == OCC_PHI_OCCUR ||    
	       Is_flag_set(OCC_SAVE_TO_TEMP) || 
	       Is_flag_set(OCC_SUNK_L_VALUE) ||
	       _temp._temp_cr == NULL),         // unless lftr is
						  // on, in which case
						  // we must return
						  // NULL.
	      ("EXP_OCCURS::Temp_cr: cannot use temp_cr field"));
      return _temp._temp_cr; 
    }

  void		  Set_temp_cr(CODEREP *x)  { 
      Is_True(!Is_flag_set(OCC_DELETE_COMP) && 
	      !(Occ_kind() == OCC_REAL_OCCUR && !Is_flag_set(OCC_SAVE_TO_TEMP)),
	      ("EXP_OCCURS::Set_temp_cr: cannot use temp_cr field"));
      _temp._temp_cr = x; 
    }

  void		  Clear_temp_cr(void)  { _temp._temp_cr = NULL; }
  
  // get a temporary code rep (or return existing one)
  CODEREP	 *Get_temp_cr(EXP_WORKLST *wk, CODEMAP *htable);

  EXP_OCCURS     *Def_occur(void) const   
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR ||
	      Occ_kind() == OCC_COMP_OCCUR ||
	      (Occ_kind() == OCC_PHI_PRED_OCCUR &&
	       Inserted_computation()),
	      ("EXP_OCCURS::Def_occur: Must be real or comp occur."));
      return _temp._def_occur;
    }
  void            Set_def_occur(EXP_OCCURS *def_occur)
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR ||
	      Occ_kind() == OCC_COMP_OCCUR ||
	      (Occ_kind() == OCC_PHI_PRED_OCCUR &&
	       def_occur == NULL),
	      ("EXP_OCCURS::Set_def_occur: Must be real or comp occur."));
      _temp._def_occur = def_occur;
    }

  // A member function to tell whether an occurrence is a real
  // available definition of its e-version, i.e., whether the
  // occurrence corresponds to a computation of the expression in the
  // optimized program.
  BOOL            Is_real_avail_def(void) const
    {
      return ((Occ_kind() == OCC_PHI_PRED_OCCUR &&
	       (Inserted_computation() || Sunk_lvalue())) ||
	      (Occ_kind() == OCC_REAL_OCCUR &&
	       Def_occur() == NULL));
    }

  EXP_WORKLST    *Spre_wk(void) const	   { return _temp._spre_wk; }
  void            Set_spre_wk(EXP_WORKLST *wk)
    {
      Is_True(Occ_kind() == OCC_PHI_OCCUR,
              ("EXP_OCCURS::Set_spre_wk: Must be phi occur"));
      _temp._spre_wk = wk;
    }

  STMTREP        *Stmt(void) const;
  STMTREP  	 *Enclosed_in_stmt(void) const
    { Is_True(Occ_kind() == OCC_REAL_OCCUR ||
	      Occ_kind() == OCC_COMP_OCCUR ||
	      (Occ_kind() == OCC_PHI_PRED_OCCUR &&
	       Inserted_computation() && Encl_stmt_set()),
	      ("EXP_OCCURS::Enclosed_in_stmt: Must be real occ!"));
      return Stmt();
    }
  STMTREP	 *Spre_store(void) const { return _enclosed_in._stmt; }
  STMTREP	 *Spre_kill_stmt(void) const { return _enclosed_in._stmt; }

  EXP_PHI        *Exp_phi(void) const
    { Is_True(Occ_kind() == OCC_PHI_OCCUR,
	      ("Must be phi occ!"));
      return _enclosed_in._exp_phi;
    }

  const BB_NODE  *Enclosed_in_bb(void) const
    { Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR ||
	      Occ_kind() == OCC_EXIT_OCCUR ||
	      (Occ_kind() == OCC_REAL_OCCUR &&
	       Inserted_computation()) ||
	      (Occ_kind() == OCC_REAL_OCCUR &&
	       Is_flag_set(OCC_HOISTED)),
	      ("Must be fake or inserted real occ!"));
      return _enclosed_in._bb;
    }
  void            Set_occurrence(CODEREP *cr) { _oc._occurrence = cr; }
  void            Set_e_version(IDTYPE v)  { _e_version = v; }
  void            Set_stmt_kid_num(INT stmt_kid_num)
    { Is_True(Occ_kind() == OCC_REAL_OCCUR || Occ_kind() == OCC_COMP_OCCUR ||
              (Occ_kind() == OCC_PHI_PRED_OCCUR && Inserted_computation()),
	      ("EXP_OCCURS::Set_stmt_kid_num: Must be real occ!"));
      FmtAssert((stmt_kid_num>=0) && (stmt_kid_num<=32767),
		("Kid number < 0 or > 32767"));
      _stmt_kid_num = stmt_kid_num;
    }
  void            Clear_flags(void)        { Reset_flag(); }
  void		  Reset_enclosed_in(void)  { _enclosed_in._stmt = NULL; }
  void            Set_enclose_stmt(STMTREP *stmt)
    { Is_True(Occ_kind() == OCC_REAL_OCCUR || Occ_kind() == OCC_COMP_OCCUR ||
              (Occ_kind() == OCC_PHI_PRED_OCCUR && 
	       (Inserted_computation() || Sunk_lvalue())), 
	      ("EXP_OCCURS::Set_enclose_stmt: Must be real occ!"));
      _enclosed_in._stmt = stmt;
    }
  void            Set_exp_phi(EXP_PHI *exp_phi)
    { Is_True(Occ_kind() == OCC_PHI_OCCUR,
	      ("EXP_OCCURS::Set_exp_phi: Must be phi occ!"));
      _enclosed_in._exp_phi = exp_phi;
    }
  void            Set_enclose_bb(BB_NODE *bb)
    { Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR ||
	      Occ_kind() == OCC_EXIT_OCCUR ||
	      (Occ_kind() == OCC_REAL_OCCUR &&
	       Inserted_computation()) ||
	      (Occ_kind() == OCC_REAL_OCCUR &&
	       Is_flag_set(OCC_HOISTED)),
	      ("EXP_OCCURS::Set_enclose_bb: Must be fake or inserted"
	       "real occ!"));
      _enclosed_in._bb = bb;
    }

  void            Set_rehash_changed_expr(void)
    { Set_flag(OCC_REHASH_CHANGED); }
  void            Set_mult_real(void)      { Set_flag(OCC_MULT_REAL_OCCUR); }
  void            Reset_mult_real(void)    { Reset_flag(OCC_MULT_REAL_OCCUR); }
  void            Set_fake_store(void)     { Set_flag(OCC_FAKE_STORE_OCCUR); }
  void            Reset_encl_stmt_set(void){
      Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR,
	      ("EXP_OCCURS::Reset_encl_stmt_set: bad occurrence kind"));
      Reset_flag(OCC_ENCL_STMT_SET);
    }
  void            Set_encl_stmt_set(void)
    {
      Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR,
	      ("EXP_OCCURS::Set_encl_stmt_set: bad occurrence kind"));
      Set_flag(OCC_ENCL_STMT_SET);
    }
  void            Reset_for_spre(void)     { Reset_flag(OCC_FOR_SPRE); }
  void            Set_for_spre(void)       { Set_flag(OCC_FOR_SPRE); }
  void            Set_delete_comp(void)    { Set_flag(OCC_DELETE_COMP); }
  void            Reset_save_to_temp(void) { Reset_flag(OCC_SAVE_TO_TEMP); }
  void            Set_save_to_temp(void)   { Set_flag(OCC_SAVE_TO_TEMP); }
  void            Reset_inserted(void)     { Reset_flag(OCC_INSERTED_OCCUR); }
  void            Set_inserted(void)
    {
      Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR,
	      ("EXP_OCCURS::Set_inserted: bad occurrence kind"));
      Set_flag(OCC_INSERTED_OCCUR);
    }

  void            Reset_t_ver_owns_coderep(void)
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR ||
	      Occ_kind() == OCC_PHI_OCCUR,
	      ("EXP_OCCURS::Reset_t_ver_owns_coderep: bad occurrence kind"));
      Reset_flag(OCC_CODEREP_OWNER);
    }

  void            Set_t_ver_owns_coderep(void)
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR ||
	      Occ_kind() == OCC_PHI_OCCUR,
	      ("EXP_OCCURS::Set_t_ver_owns_coderep: bad occurrence kind"));
      Set_flag(OCC_CODEREP_OWNER);
    }

  void            Set_occurs_as_lvalue(void)
    {
      Is_True(OPERATOR_is_scalar_istore (Stmt()->Opr()) ||
	      OPERATOR_is_scalar_store (Stmt()->Opr()),
	      ("EXP_OCCURS::Set_occurs_as_lvalue: not ISTORE"));
      Set_flag(OCC_AS_L_VALUE);
    }

  void		  Set_injured_occ(void)
    {
      // Someone in the know, please un-comment the following
      // assertion:
      // Is_True(Occ_kind() == OCC_COMP_OCCUR ||
      //         (Occ_kind() == OCC_REAL_OCCUR &&
      //          Delete_comp()),
      //         ("EXP_OCCURS::Set_injured_occ: Occurrence must be "
      //          "comparison or reload."));
      Set_flag(OCC_INJURED);
    }

  void            Reset_injured_occ(void)  { Reset_flag(OCC_INJURED); }

#if defined(TARG_SL) // PARA_EXTENSION
  BOOL Occ_in_para_region(void)            { Is_flag_set(OCC_IN_PARA_REGION); }
  void Set_occ_in_para_region(void)        { Set_flag(OCC_IN_PARA_REGION);}
  void Reset_occ_in_para_region(void)      { Reset_flag(OCC_IN_PARA_REGION); }
#endif

  void		  Set_sunk_lvalue(void)    { Set_flag(OCC_SUNK_L_VALUE); }
#if defined(TARG_SL) // PARA_EXTENSION
  void 	  	  Reset_sunk_lvalue(void)  { return Reset_flag(OCC_SUNK_L_VALUE);}
#endif

  BOOL		  Mult_real(void) const	   { return Is_flag_set
					       (OCC_MULT_REAL_OCCUR); }
  BOOL		  Fake_store(void) const   { return Is_flag_set
					       (OCC_FAKE_STORE_OCCUR); }
  BOOL            Encl_stmt_set(void) const{ return Is_flag_set
                                               (OCC_ENCL_STMT_SET); }
  BOOL            For_spre(void) const     { return Is_flag_set
                                               (OCC_FOR_SPRE); }
  BOOL            Rehash_changed_expr(void) const { return Is_flag_set
						      (OCC_REHASH_CHANGED); }

  BOOL            Delete_comp(void) const
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR, ("Bad occurrence kind"));
      return Is_flag_set(OCC_DELETE_COMP);
    }

  BOOL            Save_to_temp(void) const
    {
      Is_True(Occ_kind() == OCC_REAL_OCCUR || 
	      Occ_kind() == OCC_PHI_PRED_OCCUR, ("Bad occurrence kind"));
      return Is_flag_set(OCC_SAVE_TO_TEMP);
    }

  BOOL		  Inserted_computation(void) const
    {
      // The CSE/CodeMotion step is cleaner if we sometimes check a
      // real or phi occurrence for an insertion; this is OK as long
      // as we're going to return FALSE.
      Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR ||
	      Occ_kind() == OCC_PHI_OCCUR ||
	      Occ_kind() == OCC_REAL_OCCUR,
	      ("EXP_OCCURS::Inserted_computation: Bad occurrence kind"));

      // Slightly convoluted code because of overloaded flag bits:
      if (Occ_kind() == OCC_REAL_OCCUR) {
	return FALSE;
      }
      else {
	return Is_flag_set(OCC_INSERTED_OCCUR);
      }
    }

  BOOL            T_ver_owns_coderep(void) const
    {
      Is_True(Occ_kind() == OCC_PHI_OCCUR ||
	      Occ_kind() == OCC_REAL_OCCUR,
	      ("EXP_OCCURS::T_ver_owns_coderep: Bad occurrence kind"));

      return Is_flag_set(OCC_CODEREP_OWNER);
    }

  void            Bid_for_coderep(const ETABLE                       *const,
				        ID_MAP<REHASH_INFO *, INT32> &,
				  const UINT32);

  void            Render_coderep_unownable(const ETABLE                 *const,
					   ID_MAP<REHASH_INFO *, INT32> &);

  BOOL		  Occurs_as_lvalue(void) const
    {
      return Is_flag_set(OCC_AS_L_VALUE);
    }

  BOOL		  Sunk_lvalue(void) const
    {
      return Is_flag_set(OCC_SUNK_L_VALUE);
    }

  void            Set_required_pred(void)
    {
      Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR,
	      ("EXP_OCCURS::Set_required_pred: Only preds can be required"));
      Set_flag(OCC_REQUIRED_PRED);
    }

  void            Reset_required_pred(void)
    {
      Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR,
	      ("EXP_OCCURS::Set_required_pred: Only preds can be required"));
      Reset_flag(OCC_REQUIRED_PRED);
    }

  BOOL            Required_pred(void) const
    {
      Is_True(Occ_kind() == OCC_PHI_PRED_OCCUR,
	      ("EXP_OCCURS::Required_pred: Only preds can be required"));
      return Is_flag_set(OCC_REQUIRED_PRED);
    }

  BOOL            Temp_eligible_as_owner(EXP_WORKLST *worklist) const;

  BOOL            Occurs_as_hoisted(void) const
    {
      return Is_flag_set(OCC_HOISTED);
    }  
    
  // Return a CR with necessary CVT/CVTL inserted
  CODEREP        *Load_use_cr(ETABLE *etable, CODEREP *old_cr, CODEREP *cr);

  BOOL		  Injured_occ(void) const
    {
      // Please add assertion(s) here to verify invariants.
      return Is_flag_set(OCC_INJURED);
    }

  BOOL		  Obsolete_comparison(void) const
    {
      Is_True(Occ_kind() == OCC_COMP_OCCUR,
	      ("EXP_OCCURS::Obsolete_comparison: Bad occurrence kind"));
      return Is_flag_set(OCC_OBSOLETE_COMPARISON);
    }

  void            Set_obsolete_comparison(void)
    {
      Is_True(Occ_kind() == OCC_COMP_OCCUR,
	      ("EXP_OCCURS::Set_obsolete_comparison: Bad occurrence kind"));
      Set_flag(OCC_OBSOLETE_COMPARISON);
    }

  // Test if 'this' is a descendant of 'anc' in the Dominator Tree(DT)
  BOOL            Is_DT_descendant_of( EXP_OCCURS *anc );

  // Test if the DPO of 'this' less than the DPO of 'x'.
  // Asserts if they are the same. [ No it doesn't. It returns FALSE -- RK ]
  BOOL            Is_DPO_less_than(EXP_OCCURS *x);

  BOOL            Stmt_order_less_or_equal(EXP_OCCURS *x) const
    { return Stmt()->Stmt_order_less_or_equal(x->Stmt()); }

#if defined(TARG_SL)  // PARA_EXTENSION
  RID*     Rid(void)      { return _rid; }
  void Set_Rid(RID* rid)  { _rid = rid;}
#endif

  BB_NODE        *Bb(void) const;          // return the BB that contains this.

  void            Print(FILE *fp = stderr, BOOL show_phi_preds = TRUE) const;

  // Set the real occurrence and hoisted occurrence flag
  void            Set_hoisted_occ(void) 
  {
    Set_kind(OCC_REAL_OCCUR);
    Set_flag(OCC_HOISTED);
  }

  // Reset the hoisted occ flag
  void           Reset_hoisted(void)   {  Reset_flag(OCC_HOISTED); }

  void           Save_flags() { _saved_flags = _flag; }
  void           Restore_flags() { _flag = _saved_flags; }

  // A function used in SSA minimization and CodeMotion to tell what
  // occurrence *this is identical to.
  EXP_OCCURS *Occ_identical_to(void);

  bool Will_b_deleted()  {  return Occ_kind() == OCC_REAL_OCCUR &&
			      !Occurs_as_lvalue() &&
			      Def_occur() != NULL &&
			      (Def_occur()->Occ_kind() == OCC_REAL_OCCUR ||
			       (Def_occur()->Occ_kind() == OCC_PHI_OCCUR &&
				Def_occur()->Exp_phi()->Will_b_avail())); }
};

class EXP_OCCURS_CONTAINER : public SLIST {
private:
  EXP_OCCURS_CONTAINER(const EXP_OCCURS_CONTAINER&);
  EXP_OCCURS_CONTAINER& operator = (const EXP_OCCURS_CONTAINER&);

  DECLARE_SLIST_CLASS( EXP_OCCURS_CONTAINER, EXP_OCCURS )
public:  
  ~EXP_OCCURS_CONTAINER(void)              { }
  void            Init(void);
  void            Print(FILE *fp = stderr);
  // used when deleting the first element from the list 
  void		  Set_Head(EXP_OCCURS *exp) { SLIST::Set_Head(exp); }
  void		  Set_Tail(EXP_OCCURS *exp) { SLIST::Set_Tail(exp); }
};

// single list of occurrences
class EXP_OCCURS_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(EXP_OCCURS_ITER,
                           EXP_OCCURS,
                           EXP_OCCURS_CONTAINER )
public:
  void            Init(void)               { }
};

class EXP_PHI_OPND_USE_ITER {
private:
  EXP_PHI *_phi;
  mINT32   _cur;

public:
  EXP_PHI_OPND_USE_ITER(void)               { }
  EXP_PHI_OPND_USE_ITER(EXP_PHI *const phi) { Init(phi); }

  void            Init(void)                { _cur = 0; }
  void            Init(EXP_PHI *const phi)  { _cur = 0; _phi = phi; }
  void            Init(EXP_PHI_OPND_USE_ITER *const iter)
    { Init(iter->_phi); }

  BOOL Is_Empty(void) const { return _cur >= _phi->Opnd_count(); }

  EXP_PHI_OPND_USE_ITER *First(void)
    {
      _cur = 0;
      while (!Is_Empty() &&
	     (_phi->Opnd(_cur) == NULL ||
	      _phi->Has_real_occ(_cur))) {
	++_cur;
      }
      return this;
    }

  EXP_PHI_OPND_USE_ITER *Next(void)
    {
      ++_cur;
      while (!Is_Empty() &&
	     (_phi->Opnd(_cur) == NULL ||
	      _phi->Has_real_occ(_cur))) {
	++_cur;
      }
      return this;
    }

  EXP_PHI *Node(void) const
    {
      Is_True(_phi->Opnd(_cur) != NULL,
	      ("EXP_PHI_OPND_USE_ITER::Node: current operand "
	       "must not be NULL"));
      return _phi->Opnd(_cur)->Exp_phi();
    }

  // Placeholder member function; not useful in use-def DFS.
  INT Opnd_idx(void) const { return 0; }
};

// The following iterator is useful only with FOR_ALL_NODE. It does
// not support, e.g., FOR_ALL_NODE_REVERSE.

class EXP_PHI_OCC_ITER {
private:
  EXP_OCCURS_ITER _occ_iter;

public:
  EXP_PHI_OCC_ITER(void) { };
  EXP_PHI_OCC_ITER(EXP_OCCURS_CONTAINER &occs) :
    _occ_iter(occs.Head())
      { }

  void Init(void) { };
  void Init(EXP_OCCURS_CONTAINER &occs) { _occ_iter.Init(occs.Head()); }

  EXP_PHI *First(void)
    {
      EXP_OCCURS *phi_occ = _occ_iter.First();
      return (phi_occ != NULL ? phi_occ->Exp_phi() : NULL);
    }

  EXP_PHI *Next(void)
    {
      EXP_OCCURS *phi_occ = _occ_iter.Next();
      return (phi_occ != NULL ? phi_occ->Exp_phi() : NULL);
    }

  BOOL     Is_Empty(void) { return _occ_iter.Is_Empty(); }
};

class EXP_OCCURS_PAIR {
private:
  EXP_OCCURS  *_occ[2];
  
              EXP_OCCURS_PAIR(const EXP_OCCURS_PAIR&);
              EXP_OCCURS_PAIR& operator = (const EXP_OCCURS_PAIR&);
public:
              EXP_OCCURS_PAIR(void)           { Clear(); }

              ~EXP_OCCURS_PAIR(void)          {}

  void        Clear(void)                     { _occ[0] = NULL;
                                                _occ[1] = NULL; }
  void        Set_occ(INT i, EXP_OCCURS *occ) { _occ[i] = occ;  }
  EXP_OCCURS *Occ1(void) const                { return _occ[0]; }
  EXP_OCCURS *Occ2(void) const                { return _occ[1]; }
  BOOL        Is_Empty(void) const            { return _occ[0] == NULL
                                                  && _occ[1] == NULL; }
};

// This is the iterator for each of the four entries in the master iterator
// list. Each iterator in the list can be a normal iterator, or an iterator
// that iterates over several lists.
class OCC_ITER_LIST : public SLIST_NODE {
private:
  union {
    EXP_OCCURS_ITER *_occ_iter; // PHI_OCCUR, PHI_PRED_OCCUR, EXIT_OCCUR
    EXP_ALL_REAL_ITER *_all_real_iter; // REAL_OCCUR, COMP_OCCUR
  } _node;
  BOOL _is_occ_iter;	// TRUE for occ_iter, FALSE for all_real_iter

  BOOL Is_occ_iter(void) const { return _is_occ_iter; }

  EXP_OCCURS_ITER *Occ_iter(void) const
    { Is_True(_is_occ_iter,
	      ("OCC_ITER_LIST::Occ_iter, _is_occ_iter incorrect"));
      return _node._occ_iter;
    }

  EXP_ALL_REAL_ITER *All_real_iter(void) const
    { Is_True(!_is_occ_iter,
	      ("OCC_ITER_LIST::All_real_iter, _is_occ_iter incorrect"));
      return _node._all_real_iter;
    }

                  OCC_ITER_LIST(const OCC_ITER_LIST&);
                  OCC_ITER_LIST& operator = (const OCC_ITER_LIST&);
public:
                  OCC_ITER_LIST(void)      { }
                  OCC_ITER_LIST(EXP_OCCURS_ITER *occ_iter)
		    { _node._occ_iter = occ_iter;
		      _is_occ_iter = TRUE;
		    }
                  OCC_ITER_LIST(EXP_ALL_REAL_ITER *all_real_iter)
		    { _node._all_real_iter = all_real_iter;
		      _is_occ_iter = FALSE;
		    }
                 ~OCC_ITER_LIST(void)      { }

  DECLARE_SLIST_NODE_CLASS( OCC_ITER_LIST )

  // member access functions
  EXP_OCCURS *Get_cur(void) const;

  EXP_OCCURS *Next_occ(void) const;
  
}; // end of class OCC_ITER_LIST

// This is the container that holds the four iterators in the master list.
class OCC_CONTAINER : public SLIST {
private:
  OCC_CONTAINER(const OCC_CONTAINER&);
  OCC_CONTAINER& operator = (const OCC_CONTAINER&);

  DECLARE_SLIST_CLASS( OCC_CONTAINER, OCC_ITER_LIST )
public:  
  ~OCC_CONTAINER(void)               { }

  void            Insert_sort(OCC_ITER_LIST *l);
};

// This iterator iterates over the master iterator list.
class OCC_ITER_LIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(OCC_ITER_LIST_ITER,
                           OCC_ITER_LIST,
                           OCC_CONTAINER )
public:
  void            Init(void)               { }
};

// ====================================================================
// EXP_ALL_REAL_ITER: This is an iterator to scan two EXP_OCCURS
//                       lists (one real occurrences, one comp occurrences)
//                       according to the Dominator tree PreOrder.
// ====================================================================  
class EXP_ALL_REAL_ITER  {
private:
  // individual iterators over the lists of occurrence nodes
  EXP_OCCURS_ITER	_real;
  EXP_OCCURS_ITER	_comp1;
  EXP_OCCURS_ITER	_comp2;

  // iterators for each element in the master iterator list
  OCC_ITER_LIST         _real_iter;
  OCC_ITER_LIST         _comp1_iter;
  OCC_ITER_LIST         _comp2_iter;

  // the master iterator list
  OCC_CONTAINER         _all_iter;


                  EXP_ALL_REAL_ITER(void);
                  EXP_ALL_REAL_ITER(const EXP_ALL_REAL_ITER&);
                  EXP_ALL_REAL_ITER& operator=(const EXP_ALL_REAL_ITER&);

  EXP_OCCURS     *Get_cur(void) const;

public:
  EXP_ALL_REAL_ITER(EXP_OCCURS *, EXP_OCCURS_PAIR *);
  ~EXP_ALL_REAL_ITER(void)		{ }
  
  void            Init(void)            { }

  EXP_OCCURS     *First(void);
  EXP_OCCURS     *Next(void);   
  EXP_OCCURS	 *Cur(void)             { return Get_cur(); }
  BOOL            Is_Empty(void) const;
};
    

// ====================================================================
// EXP_ALL_OCCURS_ITER:  This is an iterator to scan two EXP_OCCURS
//                       lists (one real occurrences, one phi occurrences)
//                       according to the Dominator tree PreOrder.
// ====================================================================  
class EXP_ALL_OCCURS_ITER {
private:
  // individual iterators over the lists of occurrence nodes
  EXP_ALL_REAL_ITER	_real;	// real and comp occurs
  EXP_OCCURS_ITER	_phi;
  EXP_OCCURS_ITER	_phi_pred;
  EXP_OCCURS_ITER	_exit;

  // iterators for each element in the master iterator list
  OCC_ITER_LIST         _real_iter; // both REAL_OCCUR and COMP_OCCUR
  OCC_ITER_LIST         _phi_iter;
  OCC_ITER_LIST         _pred_iter;
  OCC_ITER_LIST         _exit_iter;

  // the master iterator list
  OCC_CONTAINER         _all_iter;

  // array of pointers to EXP_OCCURS for 
  EXP_OCCURS	       **_fast_array;

                  EXP_ALL_OCCURS_ITER(void);
                  EXP_ALL_OCCURS_ITER(const EXP_ALL_OCCURS_ITER&);
                  EXP_ALL_OCCURS_ITER& operator = (const EXP_ALL_OCCURS_ITER&);

  EXP_OCCURS     *Get_cur(void) const;

public:
  // constructor for normal iterator (some lists can be missing)
  EXP_ALL_OCCURS_ITER(EXP_OCCURS *real,     EXP_OCCURS_PAIR *comp,
                      EXP_OCCURS *phi,      EXP_OCCURS *phi_pred,
                      EXP_OCCURS *exit);
  // constructor for fast reuse iterator
  EXP_ALL_OCCURS_ITER(EXP_WORKLST *worklst, ETABLE *etable, LFTR *lftr);
  ~EXP_ALL_OCCURS_ITER(void)               { }

  void            Init(void)               { }
  EXP_OCCURS     *First(void);
  EXP_OCCURS     *Next(void);
  BOOL            Is_Empty(void) const;
  void		  Remove_iter(void);
};

// ====================================================================
// EXP_WORKLST: This is a worklist entry for a specific expression.
//              Note: never include any expression contains volatile
//              memory access!  It contains:
//
//             _e_num:           the E-Number for the expression we are
//                               processing.
//             _exp:             the first instance of the expression of this
//                               specific syntactical order.
//             _preg:            the preg aux_id assigned for this specific
//                               exp.
//             _real_occurs:     the list of all real occurrences of this 
//                               expression in this PU in DPO.
//             _phi_occurs:      the list of all phi occurrences.
//             _phi_pred_occurs: the list of all phi occurrences.
//             _comp_occurs:     the pair(possibly) of all comp occurrences.
//
// NOTE: EXP_WORKLST is an slist for it's use as a hash bucket list when
//	finding an expression. It also has another singly linked list
//	(_next) for use in the worklst itself. LFTR needs to be able to
//	insert at the beginning of the worklst.
// ====================================================================
class EXP_WORKLST : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS( EXP_WORKLST )
public:
  enum EXP_WORKLST_FLAG {
    EWF_NONE             = 0x00,
    EWF_SIGN_EXTD        = 0x01,// this CK_IVAR is sign extended hi/lo 32 bits
    EWF_EXCLUDE_SR_CAND  = 0x02,// this expr won't be strength-reduced
    EWF_IS_PROCESSED     = 0x04,// this worklist has been processed and
                                // sits in processed_worklist
    EWF_IS_URGENT        = 0x08,// this worklist is in urgent_worklist
    EWF_PHI_PREDS_PRUNED = 0x10,// inconsequential phi preds have been
				// recycled
    EWF_HAS_CONST        = 0x20,// the expression contains constant node
    EWF_HAS_RCONST       = 0x40,// the expression contains RCONST node
    EWF_HAS_LDA          = 0x80,// the expression contains LDA node
    EWF_LR_SHRINK_CAND   = 0x100,// live range shrinking candidate (LPRE only)
    EWF_IVC_CAND         = 0x200,// induction variable coalescing candidate
    EWF_NO_LFTR          = 0x400,// no LFTR for this expression (hack
                                 // around 665964)
    EWF_SIZE_DIFFERENT   = 0x800 // loads have different size in LPRE
  };

  enum EXP_PREG_VALUE {
    ILLEGAL_PREG = 0,	// illegal preg
  };

private:
  EXP_ID                _e_num;            // the e_num for _exp
  CODEREP              *_exp;              // the coderep node
  IDTYPE                _preg;             // the preg containing value of _exp;
					   // in SPRE, preg assigned to variable
  UINT32		_flags:30;         // EWF_... flags
  UINT32		_pre_kind:2;       // PRE_KIND

  EXP_OCCURS_CONTAINER  _real_occurs;      // real occurrences
  EXP_OCCURS_CONTAINER  _phi_occurs;       // phi occurrences
  EXP_OCCURS_CONTAINER _phi_pred_occurs;   // phi predecessor occurrences
  EXP_OCCURS_PAIR      _comp_occurs;       // comparison occurrences
  IDTYPE		_temp_id;	   // has processed this var in this BB in SPRE's iphi insertion 
  INT                   _cur_e_version;    // next e-version to use

  EXP_ALL_OCCURS_ITER  *_iterator;	   // occurrence iterator
  union {
    BB_NODE_SET	       *_iphi_bbs;	   // BBs with iphi inserted in SPRE
    STACK<EXP_OCCURS*> *_spre_stack;       // stack used in step 3 of SPRE
  } _u1;    
  INT                   _phi_cnt;          // for statistics
  INT                   _realocc_cnt;      // for statistics
  INT			_ssa_edge_cnt;	   // for statistics
  INT                   _optimistic_ssa_cnt;// for statistics
  INT                   _dense_ssa_cnt;    // for statistics
  INT                   _insert_cnt;       // for statistics
  INT                   _save_cnt;         // for statistics
  INT                   _reload_cnt;       // for statistics
  INT                   _temp_phi_cnt;     // for statistics
  INT                   _hoisted_cnt;      // for statistics
  INT			_temp_owner_cnt;   // for statistics

                  EXP_WORKLST(void);
                  EXP_WORKLST(const EXP_WORKLST&);
                  EXP_WORKLST& operator = (const EXP_WORKLST&);

  // verify if the occ is in dpo order of occ is the last in the worklist
  BOOL            Verify_dpo_order(EXP_OCCURS_CONTAINER &worklist,
                                   EXP_OCCURS *occ);
  BOOL            Verify_dpo_order(EXP_OCCURS_CONTAINER &worklist);

  void            Compute_du_info(MEM_POOL *);
  void            Compute_user_avail(BOOL);
  void            Compute_avail(BOOL);
  void            Compute_stops(BOOL);
  void            Compute_partial_avail(BOOL);

  void            Insert_one_operand(ETABLE  *,
				     CODEMAP *,
				     EXP_PHI *,
				     INT);
  BOOL            Determine_inserts_saves_deletions(CODEMAP *,
						    ETABLE *,
						    E_VER_TAB *,
						    EXP_OCCURS_PAIR *);
  void            SPRE_Determine_inserts_saves_deletions(CODEMAP *,
							 ETABLE *,
							 E_VER_TAB *);
#if Is_True_On
  void            Verify_saved_occurs(E_VER_TAB *);
#endif

  // helper of varible assignment induced phi-insertion
  void            Generate_variable_phi_list(INT, BB_NODE_SET&, BOOL, ETABLE*);  
  // helper of varible assignment induced phi-insertion for indirect load/store
  void            Generate_ivariable_phi_list_addr(BB_NODE_SET&, BOOL, ETABLE*);  
  void            Generate_ivariable_phi_list_vsym(BB_NODE_SET&, BOOL);
                                                     
public:
                  EXP_WORKLST(EXP_ID e_num, CODEREP *exp, PRE_KIND pre_kind)
                    { _pre_kind = pre_kind; Init (e_num, exp); Exam_const(); }

                 ~EXP_WORKLST(void)        { }

  // Member access functions
  EXP_OCCURS_CONTAINER& 
                  Real_occurs(void)        { return _real_occurs; }
  EXP_OCCURS_CONTAINER& 
                  Phi_occurs(void)         { return _phi_occurs; }
  EXP_OCCURS_CONTAINER&
                  Phi_pred_occurs(void)    { return _phi_pred_occurs; }
  EXP_OCCURS_PAIR&
                  Comp_occurs(void)        { return _comp_occurs; }

  PRE_KIND        Pre_kind(void) const     { return (PRE_KIND)_pre_kind; }
  EXP_ID          E_num(void)      const   { return _e_num; }
  void            Set_e_num(EXP_ID n)  	   { _e_num = n; }
  CODEREP        *Exp(void)        const   { return _exp; }
  IDTYPE          Preg(void)   	   const   { return _preg; }
  void            Set_preg(IDTYPE s)   	   { _preg = s; }
  STACK<EXP_OCCURS*>
                 *Spre_stack(void) const   { Is_True(_pre_kind == PK_SPRE,
                                             ("Spre_stack must be SPRE"));
                                             return _u1._spre_stack;}
  void            Set_spre_stack
                    (STACK<EXP_OCCURS*> *s){ Is_True(_pre_kind == PK_SPRE,
                                             ("Set_spre_stack must be SPRE"));
                                             _u1._spre_stack = s;}
  BB_NODE_SET    *Iphi_bbs(void) const     { Is_True(_pre_kind == PK_SPRE,
                                             ("Iphi_bbs must be SPRE"));
                                             return _u1._iphi_bbs;}
  void    	  Set_iphi_bbs(BB_NODE_SET *s) { Is_True(_pre_kind == PK_SPRE,
                                             ("Set_iphi_bbs must be SPRE"));
                                             _u1._iphi_bbs = s;}
  IDTYPE          Temp_id(void)    const   { Is_True(_pre_kind == PK_SPRE,
                                             ("temp_id must be SPRE"));
                                             return _temp_id; }
  void            Set_temp_id(IDTYPE s)    { Is_True(_pre_kind == PK_SPRE,
                                             ("Set_temp_id must be SPRE"));
                                             _temp_id = s; }
  INT             Insert_count(void) const { return _insert_cnt; }
  void            Inc_insert_count(void)   { _insert_cnt++; }
  INT             Save_count(void)  const  { return _save_cnt; }
  void            Inc_save_count(void)     { _save_cnt++; }
  INT             Reload_count(void) const { return _reload_cnt; }
  void            Inc_reload_count(void)   { _reload_cnt++; }
  INT             Temp_phi_count(void) const { return _temp_phi_cnt; }
  void            Inc_temp_phi_count(void)   { _temp_phi_cnt++; }
  INT             Hoisted_count(void) const { return _hoisted_cnt; }
  void            Inc_hoisted_count(void)   { _hoisted_cnt++; }
  INT             Temp_owner_count(void) const { return _temp_owner_cnt; }
  void            Inc_temp_owner_count(void)   { _temp_owner_cnt++; }

  void            Reset_statistics(void)   { _phi_cnt = 0;
					     _realocc_cnt = 0;
					     _ssa_edge_cnt = 0;
                                             _optimistic_ssa_cnt = 0;
                                             _dense_ssa_cnt = 0;
                                             _insert_cnt = 0;
                                             _save_cnt = 0;
                                             _reload_cnt = 0;
                                             _temp_phi_cnt = 0;
                                             _hoisted_cnt = 0;
					     _temp_owner_cnt = 0;
                                           }
  INT             Phi_count(void) const            { return _phi_cnt; }
  INT             Realocc_count(void) const        { return _realocc_cnt; }
  INT             Ssa_edge_count(void) const       { return _ssa_edge_cnt; }
  INT             Optimistic_ssa_count(void) const { return _optimistic_ssa_cnt; }
  INT             Dense_ssa_count(void) const      { return _dense_ssa_cnt; }
  void            Inc_phi_count(void)              { _phi_cnt++; }
  void            Inc_realocc_count(void)          { _realocc_cnt++; }
  void            Inc_ssa_edge_count(void)         { _ssa_edge_cnt++; }
  void            Inc_optimistic_ssa_count(INT n)  { _optimistic_ssa_cnt += n; }
  void            Inc_dense_ssa_count(void)        { _dense_ssa_cnt++; }

  // Cur_e_version() is the next version to use. After
  // Cur_e_version()'s return value has been saved somewhere,
  // New_e_version() should be called.
  INT             Cur_e_version(void) const { return _cur_e_version; }
  void            New_e_version(void)       { ++_cur_e_version; }
  void            Init_e_version(void)      { _cur_e_version =
					    EXP_OCCURS::ILLEGAL_E_VERSION + 1;}

  // fast occurrence iterator
  void		  Set_iterator(EXP_ALL_OCCURS_ITER *iter) { _iterator = iter; }
  EXP_ALL_OCCURS_ITER *Iterator(void)	    { return _iterator; }

  void            Init(EXP_ID e_num, CODEREP *exp);
  void            Exam_const(void);

  void            Adjust_combined_types(CODEREP *cr);
  void            Set_sign_extd( void )     { _flags |= EWF_SIGN_EXTD; }
  void            Reset_sign_extd( void )   { _flags &= ~EWF_SIGN_EXTD; }
  BOOL            Sign_extd( void )         { return _flags & EWF_SIGN_EXTD; }

  // various EWF_... flags
  // this expr is going to be excluded from being an strength-reduction
  // candidate
  void            Clear_flags(void)         { _flags = EWF_NONE; }

  BOOL            Exclude_sr_cand(void)const{ return (Pre_kind()==PK_EPRE) &&
					       (_flags & EWF_EXCLUDE_SR_CAND);}
  void            Set_exclude_sr_cand( void){ _flags |= EWF_EXCLUDE_SR_CAND; }
  void            Reset_exclude_sr_cand(void){ _flags &= ~EWF_EXCLUDE_SR_CAND;}

  // this expr is processed
  BOOL            Is_processed(void)const   { return _flags&EWF_IS_PROCESSED;}
  void            Set_is_processed( void )  { _flags |= EWF_IS_PROCESSED; }
  void            Reset_is_processed( void ){ _flags &= ~EWF_IS_PROCESSED; }

  // this expr is urgent
  BOOL            Is_urgent(void)const      { return _flags & EWF_IS_URGENT;}
  void            Set_is_urgent( void )     { _flags |= EWF_IS_URGENT; }
  void            Reset_is_urgent( void )   { _flags &=
						~EWF_IS_URGENT; }
  // this expr contains int constant
  BOOL            Has_const(void)const      { return _flags & EWF_HAS_CONST;}
  void            Set_has_const( void )     { _flags |= EWF_HAS_CONST; }
  void            Reset_has_const( void )   { _flags &=
						~EWF_HAS_CONST; }

  // this expr contains floating constant
  BOOL            Has_rconst(void)const     { return _flags & EWF_HAS_RCONST;}
  void            Set_has_rconst( void )    { _flags |= EWF_HAS_RCONST; }
  void            Reset_has_rconst( void )  { _flags &=
						~EWF_HAS_RCONST; }

  // this expr contains LDA node
  BOOL            Has_lda(void)const        { return _flags & EWF_HAS_LDA;}
  void            Set_has_lda( void )       { _flags |= EWF_HAS_LDA; }
  void            Reset_has_lda( void )     { _flags &= ~EWF_HAS_LDA; }

  // variable is a live range shrinking candidate (LPRE only)
  BOOL            LR_shrink_cand(void) const { return _flags & EWF_LR_SHRINK_CAND;}
  void            Set_LR_shrink_cand( void ) { _flags |= EWF_LR_SHRINK_CAND; }

  // variable has loads of different sizes
  BOOL            Has_unequal_sizes(void) const { 
    return _flags & EWF_SIZE_DIFFERENT;}
  void            Set_has_unequal_sizes(void) { 
    _flags |= EWF_SIZE_DIFFERENT; }

  // variable is an induction variable coalesing candidate (VNFRE only)
  BOOL            Ivc_cand(void) const { return _flags & EWF_IVC_CAND;}
  void            Set_ivc_cand( void ) { _flags |= EWF_IVC_CAND; }
  void            Reset_ivc_cand( void ) { _flags &= ~EWF_IVC_CAND; }

  // Defeat LFTR on a per-expr basis. This is Fred's hack to avoid
  // 665964 without doing anything intelligent. The flag is set in
  // EXP_WORKLST::Propagate_downsafe when we perform speculation.
  BOOL            No_lftr(void) const { return _flags & EWF_NO_LFTR; }
  void            Set_no_lftr(void) { _flags |= EWF_NO_LFTR; }
  void            Reset_no_lftr(void) { _flags &= EWF_NO_LFTR; }

 BOOL            Weight_less_than(EXP_WORKLST*);

  // this expr's phi-preds that precede only non-Will_b_avail() phi's
  // have been removed from the phi-pred occurrence list and recycled.
  BOOL            Phi_preds_pruned(void) const
    { return _flags & EWF_PHI_PREDS_PRUNED; }
  void            Set_phi_preds_pruned(void)
    { _flags |= EWF_PHI_PREDS_PRUNED; }

  // Remove all occurrences in the worklist
  void            Remove_occurs(ETABLE *etable);

  // Append an occurrence node to one of the list.
  // Asserts if it is not the highest in DPO.
  void            Append_occurrence(EXP_OCCURS *occ);

  // Insert occurrence node to one of the lists in the right slot to
  // keep the DPO. BB that determines DPO is derived through the
  // occurrence's Stmt() field. 
  void            Insert_occurrence(EXP_OCCURS *occ, ETABLE *etable);

  // Insert occurrence node to one of the lists in the right slot to
  // keep the DPO. BB that determines DPO is specified explicitly, so
  // insertion into the occurrence list can be made before the
  // assignment STMTREP is built.
  void            Insert_occurrence(EXP_OCCURS *, BB_NODE *);

  // remove a single occurrence from one of the worklsts
  BOOL            Remove_real_occurrence(STMTREP *stmt);
  BOOL            Remove_phi_pred_occurrence(STMTREP *stmt);

  // 'this' is the same as 'cr'
  BOOL            Is_the_same_as(const CODEREP *cr);

  // Given a BB, find the phi occurrence (if any) in that BB
  EXP_OCCURS     *Lookup_phi_occ(const BB_NODE *bb);

  // Insert necessary CVT/CVTL when save 'old_cr' to PREG
  CODEREP       *Save_use_cr(const ETABLE *etable, CODEREP *old_cr);

  // Check for expressions that show up on rhs of iv-updates that
  // cannot be candidates for strength-reduction
  void		  Exclude_strength_reduction_cands(ETABLE *etable);

  void            Remove_if_saved_to_preg(ETABLE *etable);

  // Insert phi functions
  BOOL            Insert_exp_phi(ETABLE *etable);

  // Rename one expression worklst
  void            Rename_expression(ETABLE *etable);

  // Rename one value numbering (vnfre) worklst
  void            Rename_valnums(ETABLE *etable);

  // Backward down-safe propagation
  BOOL            Propagate_downsafe(ETABLE *etable);

  // Backward determine live phi's
  void            Determine_live_phi(ETABLE *etable);

  // Forward WILL-B-AVAILABLE/INSERT computation
  void            Compute_forward_attributes(ETABLE *etable, BOOL compute_partial_avail = FALSE);

  // Forward USER_AVAIL computation
  void Compute_fully_avail(ETABLE *etable);
  void Compute_fully_avail_stops(ETABLE *etable, BOOL &found_redundancy);

  // Remove phi's that aren't Will_b_avail() and any preds that don't
  // have Will_b_avail() successors.
  void            Prune_phi_phi_pred(ETABLE *);

  // Compute save/delete decisions and perform insertions
  BOOL            Compute_save_delete(CODEMAP *htable, ETABLE *etable,
				      EXP_OCCURS_PAIR *comp_occurs);
  // Perform code hoisting
  void            Hoist_expression(EXP_HOISTING *);

  // Minimize SSA of the temp
  void            Minimize_temp_ssa(const ETABLE *, BOOL);

  // Generate save-restore for CSE
  void            Generate_save_reload(ETABLE *etable);

  void		  Verify(void);

  void            Print(FILE *fp = stderr, EXP_OCCURS_PAIR *comp_occurs=NULL);

  // SPRE specific functions
  void            SPRE_append_occurrence(EXP_OCCURS *occ) {
                               occ->Set_for_spre();
                               Append_occurrence(occ); }
  void            SPRE_create_iphi_succ(ETABLE *etable);
  void            SPRE_determine_live_phi(ETABLE *etable);
  void            SPRE_compute_backward_attributes(ETABLE *etable);
  void            SPRE_compute_insert_delete(CODEMAP *htable, ETABLE *etable);
  void            SPRE_perform_insert_delete(ETABLE *etable);
  void            Estimate_cost(ETABLE *, PRE_KIND);
  void            Save_flags();
};

// EXP_WORKLST_CONTAINER and EXP_WORKLST_ITER are used by the hash table
class EXP_WORKLST_CONTAINER : public SLIST {
  DECLARE_SLIST_CLASS( EXP_WORKLST_CONTAINER, EXP_WORKLST )
private:
                  EXP_WORKLST_CONTAINER(const EXP_WORKLST_CONTAINER&);
                  EXP_WORKLST_CONTAINER& operator=(const EXP_WORKLST_CONTAINER&);

public:  
                 ~EXP_WORKLST_CONTAINER(void){};
  void            Init(EXP_WORKLST *w)     { SLIST::Set_Head(w); }
  void            Clear(void)              { SLIST::Clear(); }
  void            Insert_sorted(EXP_WORKLST*);
};

class EXP_WORKLST_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(EXP_WORKLST_ITER,
                           EXP_WORKLST,
                           EXP_WORKLST_CONTAINER )
public:
  void            Init(void)               { }
};

// EXP_WORKLST_NODE, is a doubly linked list that contains only one
// member such that we can create a list of EXP_WORKLST nodes.  The
// EXP_WORKLIST node itself is a singly linked list.  It's link is
// used by the Exp_worklst and Urgent_worklst in the ETABLE.  The
// EXP_WORKLST_NODE gives us the second next pointer to be used in
// the ETABLE's expression hash function.
class EXP_WORKLST_NODE : public SLIST_NODE {
private:
  EXP_WORKLST    *node;

                  EXP_WORKLST_NODE(const EXP_WORKLST_NODE&);
                  EXP_WORKLST_NODE& operator = (const EXP_WORKLST_NODE&);
public:
                  EXP_WORKLST_NODE(void)   {}
                  EXP_WORKLST_NODE
                    (EXP_WORKLST *nd)      { node = nd; }
                 ~EXP_WORKLST_NODE(void)   {}

  DECLARE_SLIST_NODE_CLASS( EXP_WORKLST_NODE )

  void            Init(EXP_WORKLST *nd)    { node = nd; }
  BOOL            Contains(EXP_WORKLST *); // check membership in list

  // member access functions
  EXP_WORKLST    *Node(void) const         { return node; }
}; // end of class EXP_WORKLST_NODE

// use the name EXP_WORKLST_NODE_CONTAINER in the rest of the program.
#define EXP_WORKLST_NODE_CONTAINER EXP_WORKLST_NODE_CONT
class EXP_WORKLST_NODE_CONT : public SLIST {
private:
                  EXP_WORKLST_NODE_CONT(const EXP_WORKLST_NODE_CONT&);
                  EXP_WORKLST_NODE_CONT& operator = 
                    (const EXP_WORKLST_NODE_CONT&);

  DECLARE_SLIST_CLASS( EXP_WORKLST_NODE_CONT, EXP_WORKLST_NODE )
public:  
                 ~EXP_WORKLST_NODE_CONT(void){};
  void            Init(EXP_WORKLST_NODE *w){ SLIST::Set_Head(w); }
  void            Clear(void)              { SLIST::Clear(); }
  EXP_WORKLST_NODE *Find_exp_worklst(const CODEREP *cr);
  void            Append (EXP_WORKLST_NODE *worklst,
                          IDX_32            idx,
                          ETABLE           *etable);                
};

class EXP_WORKLST_NODE_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( EXP_WORKLST_NODE_ITER, EXP_WORKLST_NODE, EXP_WORKLST_NODE_CONT )
public:
  void            Init(void)               { }
};


// EXP_WORKLST_ITER2 allows us to process on WORKLST at a time.  It
// takes etable as input.  Actually, it works on the Exp_worklst and
// the Urgent_worklst in the etable.
// If there exist any entry inside the urgent list, the First() or
// Next() function pick up the first entry in it, otherwise, the first
// entry in the Exp_worklst is pulled out.  Whatever the worklst get
// chosen, this entry is removed from the etable.
class EXP_WORKLST_ITER2 {
private:
  EXP_WORKLST_CONTAINER  *_exp_worklst;         // the work list
  EXP_WORKLST_CONTAINER  *_urgent_worklst;      // the urgent list
  EXP_WORKLST            *_cur;

  void		  Set_cur(EXP_WORKLST *c)  { _cur = c; }

public:
  EXP_WORKLST_ITER2(EXP_WORKLST_CONTAINER
                    *exp_worklst,
                    EXP_WORKLST_CONTAINER
                    *urgent_worklst)       { _exp_worklst = exp_worklst;
                                             _urgent_worklst = urgent_worklst;
                                             _cur = NULL;
                                           }
  void		  Init(void)               { }

  EXP_WORKLST    *Cur(void)                { return _cur; }
  EXP_WORKLST    *First(void);
  EXP_WORKLST    *Next(void);
  BOOL		  Is_Empty(void)           { return _cur == NULL; }
};

// ====================================================================
// ETABLE:     carries the essential data structure for PRE/SSA phase.
//             it contains one memory pool that is used for this phase
//             only.
//
//             _cfg          keeps the source code
//             _opt_stab     keeps the variable info 
//             _htable       such that we could create new CODEREP and
//                           STMTREP node. 
//             _ssu          SSU for SPRE
//             _etable_pool  is set up by the function that creates the
//                           instance of ETABLE.
//             _per_expr_pool  is freed after each expr is done
//             _etable_local_pool
//                           is used for each pass as local pool
//             _cur_e_num    keeps track of the e-number.
//             _exit_occurs  represents all the occurs at EXIT block
//             _dpo_exp_phi  an array of pointers to the EXP_PHI for 
//			     each BB, if any. 
//             _phi_pred_cr  an array of pointers to the CODEREP for 
//			     each phi-pred BB, if any.  Used by Rename
//			     and Strength-reduction
//             _phi_work_set a bitvector set for inserting phi
//             _var_phi_set  a bitvector set for inserting phi from variables
//             _exp_hash     quickly finds the worklst for an expression.
//             _exp_worklst  contains expressions to be processed.
//             _occ_freelist recycle pool for EXP_OCCURS nodes 
// ====================================================================
class ETABLE {

friend class EXP_WORKLST;

private:
  CFG                    *_cfg;
  OPT_STAB               *_opt_stab;
  CODEMAP                *_htable;
  SSU                    *_ssu;
  ALIAS_RULE             *_arule;
  STR_RED		 *_str_red;
  LFTR			 *_lftr;
  MEM_POOL               *_etable_pool;
  MEM_POOL               *_per_expr_pool;
  MEM_POOL               *_etable_local_pool;
  EXP_ID                  _cur_e_num;
  EXP_OCCURS_CONTAINER    _exit_occurs;
  EXP_OCCURS            **_dpo_exp_phi;  
  CODEREP		**_phi_pred_cr;  
  BB_NODE_SET             _phi_work_set;
  BB_NODE_SET             _var_phi_set;
  ARRAY<EXP_WORKLST_NODE*>_exp_hash;

  EXP_WORKLST_CONTAINER   _exp_worklst;         // the work list
  EXP_WORKLST_CONTAINER   _urgent_worklst;      // the urgent list

  STACK<EXP_OCCURS*>      _occ_freelist;
  STACK<EXP_OCCURS*>      _deferred_ocopy_occurs;
  STACK<STMTREP*>         _deferred_cfold_stmts;
  EXP_OCCURS 		  _nil_exp_phi_opnd;	// fake node to represent nil 
  PRE_KIND                _pre_kind;
  BOOL                    _tracing;
  BOOL                    _lpre_do_consts;      // By default TRUE for LPRE
  BOOL                    _lpre_do_loads;       // By default TRUE for LPRE
  INT32			  _num_cse_reloads;	// statistics
  INT32			  _num_cse_saves;	// statistics
  INT32			  _num_inserted_saves;	// statistics
  INT32			  _num_temp_phis;	// statistics
  INT32                   _num_hoisted;         // statistics
  INT32                   _num_temp_owners;     // statistics
  COMP_UNIT		 *_comp_unit;
  EXP_HOISTING           *_exp_hoisting;        // data structure for code hoisting

  STMTREP                *_entry_chi;           // used by SPRE rename

  std::map<IDTYPE, BOOL> _complex_loop_map;

  // The following fields are for speeding up compile-time
  // INT32                _temp_id;       // used by step 1 (moved to htable)
  REHASH_CACHE_LIST      *_rehash_cache;        // cache rehashed expressions

  // three functions required by coding convention
                          ETABLE(void);
                          ETABLE(const ETABLE&);
                          ETABLE& operator = (const ETABLE&);

  // list of worklsts
  EXP_WORKLST_CONTAINER  *Urgent_worklst(void) { return &_urgent_worklst; }

  STACK<EXP_OCCURS*>     *Occ_freelist(void)       { return &_occ_freelist; }
  EXP_ID                  Alloc_e_num(void)        { return ++_cur_e_num; }

  void            Clear_dpo_exp_phi(void);
  void            Clear_dpo_exp_phi(EXP_OCCURS_CONTAINER &worklist);
  EXP_OCCURS     *Set_exp_phi_bb(const BB_NODE *bb, EXP_OCCURS *exp_phi);
  EXP_OCCURS     *Get_exp_phi_bb(const BB_NODE *bb);

  BB_NODE_SET    &Phi_work_set(void)       { return _phi_work_set; }
  BB_NODE_SET    &Var_phi_set(void)        { return _var_phi_set; }

  // any cleanup per expression goes in here
  void		  Per_worklst_cleanup(EXP_WORKLST *exp_worklst) const;

  // walk the PU, create initial work list
  void            Init_worklst( void );

  EXP_OCCURS     *Alloc_occurs_node(void);

  // exp hash related routines
  IDX_32          Exp_hashvalue(const CODEREP *cr);
  IDX_32          Hash_exp(const CODEREP *cr);
  EXP_WORKLST_NODE*Exp_hash_bucket(IDX_32 idx) const
                                           { return _exp_hash[idx]; }

  // Generate temp_id for step 1 and step 6
  void            New_temp_id(void)        { Htable()->New_temp_id(); }
  INT32           Cur_temp_id(void) const  { return Htable()->Cur_temp_id(); }

  // Maintain a cache for rehashed expressions
  void            Clear_rehash_cache(void) { _rehash_cache = NULL; }
  void            Add_rehash_expr(CODEREP *cr, CODEREP *rehash_cr);
  CODEREP        *Lookup_rehash_expr(CODEREP *);

  // Recursive replacement of real occurrences with a replacement (repl).
  //
  CODEREP        *Recursive_rehash_and_replace(
				       CODEREP           *x,
				       EXP_OCCURS        *occur,
				       OCCUR_REPLACEMENT *repl,
				       const BOOL         replacing_istr_base,
				       UINT               depth,
  				       OPCODE		  opc);
   CODEREP       *Rehash_and_replace(CODEREP           *x,
				     EXP_OCCURS        *occur,
				     OCCUR_REPLACEMENT *repl,
				     const BOOL         replacing_istr_base,
				     OPCODE		parent_opc);
   void           Replace_occurs(EXP_OCCURS *occur, OCCUR_REPLACEMENT *repl);

  // remove a single occurrence from one of the worklsts
  BOOL            Remove_real_occurrence(EXP_WORKLST_CONTAINER *worklist,
                                         CODEREP *old_cr, STMTREP *stmt);

  STMTREP        *Entry_chi(void) const    { return _entry_chi; }
  void            Set_entry_chi(STMTREP *entry_chi) { _entry_chi = entry_chi; }

#ifdef KEY
  void            Mark_phi_live(PHI_NODE *phi);
#endif
public:
                  ETABLE(CFG      *cfg,
                         OPT_STAB *opt_stab,
                         CODEMAP  *htable,
			 ALIAS_RULE *ar,
                         IDX_32    exp_hash_size,
                         MEM_POOL *etable_pool,
                         MEM_POOL *per_expr_pool,
			 MEM_POOL *etable_local_pool,
			 COMP_UNIT *comp_unit,
                         PRE_KIND   pre_kind);

                 ~ETABLE(void);

  enum {
    ILLEGAL_E_NUMBER = 0,
  };

  enum URGENCY {
    NOT_URGENT    = 0,  // append to non-urgent list
    URGENT_INSERT = 1,  // insert in urgent list
    NOT_URGENT_INS= 2,  // insert in non-urgent list
  };

  CFG            *Cfg(void) const          { return _cfg; }
  OPT_STAB       *Opt_stab(void) const     { return _opt_stab; }
  CODEMAP        *Htable(void) const       { return _htable; }
  SSU            *Ssu(void) const          { return _ssu; }
  ALIAS_RULE     *Arule(void) const        { return _arule; }
  STR_RED     	 *Str_red(void) const      { return _str_red; }
  LFTR     	 *Lftr(void) const         { return _lftr; }
  MEM_POOL       *Per_expr_pool(void) const{ return _per_expr_pool; }
  EXP_HOISTING   *Exp_hoisting(void) const { return _exp_hoisting; }
  EXP_WORKLST_CONTAINER  *Exp_worklst(void)    { return &_exp_worklst; }
  std::map<IDTYPE, BOOL> *Complex_loop_map(void)   { return &_complex_loop_map; }
   

  void		  Count_lex_ident_exprs(INT32); // for statistics only

  // remove a single occurrence from one of the worklsts
  void            Remove_real_occurrence(CODEREP *old_cr, STMTREP *stmt);

  // A version of Alloc_occurs_node to match the fancy
  // constructor. This is used for insertions, and is always for real
  // occurrences.
  EXP_OCCURS     *Alloc_occurs_node(CODEREP *);

  // to get local pool of Etable
  MEM_POOL       *Etable_pool(void) const  { return _etable_pool; }
  MEM_POOL       *Etable_local_pool(void) const     
                                           { return _etable_local_pool; }

  // get the worklist from the hash table
  EXP_WORKLST    *Get_worklst(CODEREP *cr,
                              BOOL     urgent = FALSE,
                              BOOL     lookup_only = FALSE);

  // Generate a temporary register independently of any worklist.
  CODEREP	 *New_temp_cr(MTYPE dtype, ADDRESSABILITY addressable, 
				CODEREP *rhs);

  // used by EOCC, SR, LFTR
  void		  Check_lftr_non_candidate(STMTREP *stmt, CODEREP *cr, OPCODE opc);
  void            Bottom_up_stmt(STMTREP *stmt);
  void            Bottom_up_cr(STMTREP *stmt,
                               INT      stmt_kid_num,
                               CODEREP *cr,       // tree to be processed
                               BOOL     is_store, // LHS of assignment
                               URGENCY  urgent,   // insert in urgent list
			       UINT	depth,    // in the tree
			       OPCODE opc,	  // operation on this node
			       BOOL no_estr);

  // used by LPRE
  void            LPRE_set_do_loads(BOOL truth) {_lpre_do_loads = truth;}
  void            LPRE_set_do_consts(BOOL truth) {_lpre_do_consts = truth;}

  BOOL            LPRE_do_loads() const {return _lpre_do_loads;}
  BOOL            LPRE_do_consts() const {return _lpre_do_consts;}
  
  void            LPRE_bottom_up_stmt(STMTREP *stmt);
  void            LPRE_bottom_up_cr(STMTREP *stmt,
				    INT      stmt_kid_num,
				    CODEREP *cr,
				    BOOL     is_store,
				    UINT     depth,
				    CODEREP *parent,
				    INT      whichkid);

  // used by CSE
  STMTREP 	 *Generate_stid_to_preg( CODEREP *lhs, CODEREP *rhs,
		      MTYPE rhs_type, BB_NODE *bb, SRCPOS linenum ) const;

  STMTREP	 *Save_replace_rhs_by_preg(STMTREP *stmt, CODEREP *pregcr,
					   EXP_WORKLST *wk);

  void            Insert_stmtrep_after(STMTREP *new_stmt,
                                       STMTREP *old_stmt);

  void 		  Replace_by_temp(EXP_OCCURS *occur, CODEREP *tempcr);
  void 		  Replace_by_const(EXP_OCCURS *occur, TCON tcon, TYPE_ID vect_ty);

  // Always use Rehash_exp() instead of Htable()->rehash() for an etable.
  CODEREP        *Rehash_exp(CODEREP *cr, UINT32 gvn, BOOL canon=TRUE) const;

  void 		  No_replace(EXP_OCCURS *occur, BOOL dont_rehash);

  void		  Find_1st_order_exprs_with_temp(STMTREP *stmt,
					         INT      stmt_kid_num,
					         CODEREP *cr,
					         CODEREP *tempcr,
					         BOOL     is_store,
					         UINT	depth);
						 
  void		  Find_new_1st_order_exprs(EXP_OCCURS *occur, CODEREP *tempcr);

  CODEREP        *Rehash_but_no_replace(CODEREP *x, 
					EXP_OCCURS *occur,
					BOOL        replacing_istr_base,
					INT32 orig_coderep_id);

  // to append a real occurrence into the worklst
  void            Append_real_occurrence(CODEREP *cr,
                                         STMTREP *stmt,
					 INT      stmt_kid_num,
					 UINT	  depth,
					 BOOL     is_istore = FALSE);

  // to insert a real occurrence into the worklst
  void            Insert_real_occurrence(CODEREP *cr,
                                         STMTREP *stmt,
					 INT      stmt_kid_num,
					 UINT	  depth,
					 BOOL     is_istore = FALSE,
					 BOOL     urgent = FALSE);

  // to append a phi occurence into the worklst
  EXP_OCCURS     *Append_phi_occurrence(CODEREP *cr, EXP_PHI *phi, 
                                        EXP_WORKLST *worklist);

  // to append a phi pred occurence into the worklst
  EXP_OCCURS     *Append_phi_pred_occurrence(CODEREP *cr, BB_NODE *bb,
                                             EXP_WORKLST *worklist);

  // to append an exit occurence into the etable
  void            Append_exit_occurrence(BB_NODE *bb);

  // to insert a phi occurence into the worklst
  void            Insert_phi_occurrence(EXP_PHI *phi);

  EXP_OCCURS     *Append_spre_real_occurrence(STMTREP *stmt,
					      EXP_WORKLST *worklist);
  EXP_OCCURS     *Append_iphi_succ_occurrence(BB_NODE *bb,
					      EXP_WORKLST *worklist);

  void            Set_exp_hash_bucket(IDX_32 idx, EXP_WORKLST_NODE *etmp)
                                           { _exp_hash[idx] = etmp; }

  EXP_PHI        *Lookup_exp_phi(const BB_NODE *bb, const CODEREP *cr) const;

  // Be aware, the returning PHI_NODE could be !Live().
  PHI_NODE       *Lookup_var_phi(const BB_NODE *bb, const AUX_ID id) const
                                           { return Htable()->Lookup_var_phi(bb,id); }

  // provide an array of coderep's generated per expression
  void		  Set_phi_pred_cr(const BB_NODE *bb, CODEREP *cr) const
			{ _phi_pred_cr[bb->Id()] = cr; }
  CODEREP	 *Phi_pred_cr(const BB_NODE *bb) const
			{ return _phi_pred_cr[bb->Id()]; }

  // derive phi opnd codereps from result coderep
  CODEREP            *Generate_cur_expr(const BB_NODE *, INT, CODEREP *, BOOL);
  CODEREP            *Get_cached_cur_expr(const BB_NODE *, INT);
  void                Update_cached_cur_expr(const BB_NODE *, INT, CODEREP *);
  CODEREP            *Alloc_and_generate_cur_expr(const CODEREP *,
						  const BB_NODE *,
						  INT,
						  MEM_POOL *,
						  BOOL) ;

  STACK<EXP_OCCURS *> *Deferred_ocopy_occurs(void)
    { return &_deferred_ocopy_occurs; }
  STACK<STMTREP *>    *Deferred_cfold_stmts(void) { return &_deferred_cfold_stmts; }

  void            Schedule_for_ocopy(EXP_OCCURS *);
  void            Perform_deferred_ocopy_and_get_new_exprs(EXP_WORKLST *);
  void            Perform_deferred_cfold();

  EXP_OCCURS_CONTAINER &Exit_occurs(void)      { return _exit_occurs; }
  EXP_OCCURS	 *Nil_exp_phi_opnd(void)       { return &_nil_exp_phi_opnd; }

  PRE_KIND        Pre_kind(void) const         { return _pre_kind; }

  BOOL            Tracing(void) const          { return _tracing; }

  void		  Inc_cse_reloads(INT32 n)     { _num_cse_reloads += n; }
  void		  Inc_cse_saves(INT32 n)       { _num_cse_saves += n; }
  void		  Inc_inserted_saves(INT32 n)  { _num_inserted_saves += n; }
  void		  Inc_temp_phis(INT32 n)       { _num_temp_phis += n; }
  void            Inc_hoisted(INT32 n)         { _num_hoisted += n; }
  void		  Inc_temp_owners(INT32 n)       { _num_temp_owners += n; }
  
  // perform the PRE/SSA optimization
  void            Perform_PRE_optimization(void);

  // perform the LPRE optimization
  void            Perform_LPRE_optimization(void);

  // perform the VNFRE optimization
  void            Perform_VNFRE_optimization(void);

  // SPRE specific functions
  void            Perform_SPRE_optimization(void);
  void            SPRE_update_ssa(void);
  EXP_OCCURS     *New_phi_occurrence(EXP_WORKLST *worklst, 
				     MEM_POOL    *pool, 
				     BB_NODE     *bb);

  void            Add_stmt(STMTREP *, BB_NODE *);
  void            SPRE_rename(BB_NODE *);
  void            SPRE_rename_stmt(STMTREP *, BB_NODE *);
  CODEREP        *SPRE_rename_expr(CODEREP *, BB_NODE *);
  CODEREP        *SPRE_rename_var(CODEREP *, BOOL);

  BOOL            Stmt_is_redundant(STMTREP *);
  BOOL            RHS_is_fully_avail(CODEREP *, CODEREP *);

  void            Add_to_occ_freelist(EXP_OCCURS *node);

   // -----------------------
   // VNFRE related utilities
   // -----------------------

   UINT32 Gvn(const CODEREP *cr) const
   {
      return (Pre_kind() == PK_VNFRE? VNFRE::get_valnum(cr) : 0U);
   }
   
   void Init_vnfre_worklist(EXP_WORKLST          *occurs, 
			    EXP_OCCURS_CONTAINER &exit_occurs)
   {
      Is_True(Pre_kind() == PK_VNFRE,
	      ("Illegal call to ETABLE::Init_vnfre_worklist()"));
      Exit_occurs().Set_Head(exit_occurs.Head());
      Exit_occurs().Set_Tail(exit_occurs.Tail());
      Exp_worklst()->Init(occurs); // Sets head
   }

   void Reset_vnfre_worklist()
   {
      Is_True(Pre_kind() == PK_VNFRE, 
	      ("Illegal call to TABLE::Reset_vnfre_worklist()"));
      Exp_worklst()->Head()->Remove_occurs(this);
      Exp_worklst()->Clear(); // Resets head
      Exit_occurs().Set_Head(NULL);
      Exit_occurs().Set_Tail(NULL);
   }

   EXP_OCCURS *Alloc_vnfre_occurs_node()
   {
      // This may be called before _pre_kind == PK_VNFRE, and it alows
      // us to reuse occurrence nodes from EPRE or other optimization
      // that has used the same ETABLE.
      //
      return Alloc_occurs_node();
   }

   BB_NODE_SET &Reuse_phi_list()
   {
      Phi_work_set().ClearD();
      return Phi_work_set();
   }

   void Set_phi_result(EXP_PHI *phi, EXP_OCCURS *occurs) const
   {
      phi->Set_result(occurs);
   }

   void Set_dpo_phi_occurs(const BB_NODE *bb, EXP_OCCURS *phi_occ)
   {
      (void) Set_exp_phi_bb(bb, phi_occ);
   }
};


extern EXP_HOISTING *New_EXP_HOISTING(ETABLE *, MEM_POOL *);
extern void          Delete_EXP_HOISTING(EXP_HOISTING *);
extern BOOL          Subsumable_by_branch(CODEREP *cr);

#endif  // opt_etable_INCLUDED
