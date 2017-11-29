//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_alias_class.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_alias_class.h,v $
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
// This code implements a slightly simplified form of:
// Steensgaard, B.  "Points-to analysis in almost linear time."
// Proceedings of 23rd Annual ACM SIGACT-SIGPLAN Symposium.  Held:
// St. Petersburg Beach, FL, USA, 21-24 Jan. 1996.  (USA:  ACM, 1996.
// p. 32-41)  (Conference paper - English)
//
// The main modification is simplification of function-pointer
// handling because we don't see all the code at once.
//
// ====================================================================
//
// Interface to the outside:
//   ALIAS_CLASSIFICATION::ALIAS_CLASSIFICATION
//     This constructor does only constant-time stuff, since the
//     ALIAS_CLASSIFICATION instance gets constructed whether or
//     not alias classification will be performed. This unconditional
//     construction greatly simplifies the transfer of information
//     between ALIAS_CLASSIFICATION and the rest of alias analysis.
//
//   void ALIAS_CLASSIFICATION::Classify_memops(WN *)
//     This routine takes as a parameter a pointer to the FUNC_ENTRY
//     (or REGION) node for the compilation unit and gets the job of
//     alias classification done. The output from the routine is
//     accessible through the member function
//     ALIAS_CLASSIFICATION::Alias_class(const WN *). Until and unless
//     this routine is called, the only other legal member calls are
//     to ALIAS_CLASSIFICATION::Release_resources(void) and to the
//     ALIAS_CLASSIFICATION destructor.
//
//   BS *ALIAS_CLASSIFICATION::Inaccessible_to_callees(void) const
//     This routine returns the set of aux_id's determined by alias
//     classification to be invisible to and unchangeable by any
//     callee of the present compilation unit. This bit set is used to
//     refine the bit set of the same name in the OPT_STAB instance
//     for the compilation unit; that bit set is used in turn to
//     determine the chi list for each CALL statement.
//
//   IDTYPE Alias_class(const WN *) const
//     This routine returns an identifier corresponding to the alias
//     equivalence class of the specified WHIRL node. The WHIRL node
//     must correspond to a memory operation (load, store, or
//     parameter). If two WN's have different alias classes, they
//     provably do not alias.
//
//   void Print(FILE *) const;
//     This routine prints ASCII information about the
//     ALIAS_CLASSIFICATION instance on the specified file.
//
//   void Release_resources(void);
//     This routine discards all the resources associated with the
//     ALIAS_CLASSIFICATION instance, in preparation to destruct the
//     instance. This routine is separate from the destructor because
//     we need a place to destroy the parts that get created only if
//     alias classification is actually done.
//
// ====================================================================


#ifndef opt_alias_class_INCLUDED
#define opt_alias_class_INCLUDED "opt_alias_class.h"

#include "cxx_memory.h"
#include "cxx_template.h"
#include "errors.h"
#include "tracing.h"
#include "opt_defs.h"
#include "opt_union_find.h"
#include "opt_wn.h"
#include "id_map.h"
#ifndef opt_points_to_INCLUDED
#include "opt_points_to.h"
#endif

extern "C" {
#include "bitset.h"
}

using idmap::ID_MAP;


// ====================================================================


// TODO: The following should be replaced with slist.
// A singly-linked list of references to T.
template <class T>
class SLIST_RECYCLE {
private:
  SLIST_RECYCLE<T> *_next;
  T                 _node;
public:
  SLIST_RECYCLE(const T node, SLIST_RECYCLE<T> *next) :
    _node(node), _next(next) { }

  void              Set_next(SLIST_RECYCLE<T> *next) { _next = next; }
  void              Set_node(const T node) { _node = node; }
  SLIST_RECYCLE<T> *Next(void) const { return _next; }
  T                 Node(void) const { return _node; }

  SLIST_RECYCLE<T> *Merge(SLIST_RECYCLE<T> *that) {
    if (that == NULL)
      return this;
    SLIST_RECYCLE<T> *curr, *next = that;
    do {
      curr = next;
      next = curr->Next();
    } while (next != NULL);
    curr->Set_next(this);
    return that;
  }
};


template <class T>
class SLIST_RECYCLE_HOME {
private:
  MEM_POOL         *_pool;
  SLIST_RECYCLE<T> *_available;
public:
  SLIST_RECYCLE_HOME(MEM_POOL *pool) :
    _pool(pool), _available(NULL) { }

  ~SLIST_RECYCLE_HOME() {
    // Not necessary to free elements of _available list, since MEM_POOL
    // will be deleted later.
    //     while ( _available != NULL ) {
    //       SLIST_RECYCLE<T> *temp = _available->Next();
    //       MEM_POOL_FREE( _pool, _available );
    //       _available = temp;
    //     }
  }

  SLIST_RECYCLE<T> *Produce(T node, SLIST_RECYCLE<T> *next) {
    SLIST_RECYCLE<T> *result;
    if (_available == NULL) {
      result = CXX_NEW(SLIST_RECYCLE<T>(node, next), _pool);
    } else {
      result = _available;
      _available = _available->Next();
      result->Set_node(node);
      result->Set_next(next);
    }
    return result;
  }

  SLIST_RECYCLE<T> *Receive(SLIST_RECYCLE<T> *head) {
    SLIST_RECYCLE<T> *next = head->Next();
    head->Set_next(_available);
    _available = head;
    return next;
  }
};


// ----------------------------------------------------------------------


class ALIAS_CLASS_REP;

class ALIAS_CLASS_MEMBER : public U_F_ELEMENT<ALIAS_CLASS_MEMBER> {
private:
  typedef enum {
    ACM_NONE,
    ACM_BASE,
    ACM_WN,
    ACM_LDA
  } ACM_KIND;

  ACM_KIND  _kind;
  union {
    IDTYPE  _base_id;
    WN     *_wn;
  };

  void    Set_kind(ACM_KIND kind)   { _kind = kind; }

public:
  ALIAS_CLASS_MEMBER(IDTYPE base_id) :
    _kind(ACM_BASE), _base_id(base_id)
      { }

  ALIAS_CLASS_MEMBER(WN *wn) :
    _kind(ACM_WN), _wn(wn)
      { }

  ALIAS_CLASS_MEMBER(IDTYPE base_id, ALIAS_CLASS_REP *acr) :
    _kind(ACM_BASE), _base_id(base_id)
      {	Put_in_set((U_F_REP<ALIAS_CLASS_MEMBER> *) acr); }

  ALIAS_CLASS_MEMBER(void) :
    _kind(ACM_NONE), _base_id(0)
      { }

  IDTYPE  Base_id(void) const
    {
      Is_True(_kind == ACM_BASE,
	      ("ALIAS_CLASS_MEMBER::Base_id: ACM must be BASE kind"));
      return _base_id;
    }

  void    Set_base_id(IDTYPE base_id)
    {
      Is_True(_kind == ACM_NONE,
	      ("ALIAS_CLASS_MEMBER::Set_base_id: ACM kind must not be set"));
      Set_kind(ACM_BASE);
      _base_id = base_id;
    }

  WN     *Wn(void) const
    {
      Is_True(_kind == ACM_WN,
	      ("ALIAS_CLASS_MEMBER::Wn: ACM must be WN kind"));
      return _wn;
    }

  void    Set_wn(WN *wn)
    {
      Is_True(_kind == ACM_NONE,
	      ("ALIAS_CLASS_MEMBER::Set_wn: ACM kind must not be set"));
      Set_kind(ACM_WN);
      _wn = wn;
    }

  void    Set_lda_kind(void)
    { Set_kind(ACM_LDA); }

  BOOL Is_LDA_kind(void) const
    { return _kind == ACM_LDA; }

  void    Print(FILE *) const;

  // Do we need to define operators to cast to and from the base
  // class? What about copies of the base class member functions for
  // the derived class?

  // The following is non-const because find entails path
  // compression...
  ALIAS_CLASS_REP *Alias_class(void)
    { return (ALIAS_CLASS_REP *) Find(); }
};


typedef slist<ALIAS_CLASS_MEMBER *,
              mempool_allocator<ALIAS_CLASS_MEMBER *> >
  ALIAS_CLASS_MEMBER_LIST;


// ----------------------------------------------------------------------

typedef SLIST_RECYCLE<ALIAS_CLASS_MEMBER *> *PENDING_LIST;
typedef SLIST_RECYCLE_HOME<ALIAS_CLASS_MEMBER *> PENDING_LIST_HOME;

class ALIAS_CLASSIFICATION;

// The C++ class representing an alias class
class ALIAS_CLASS_REP : public U_F_REP<ALIAS_CLASS_MEMBER> {
  friend class ALIAS_CLASSIFICATION;
#if Is_True_On
  friend void  print_table(void);
#endif

  enum ACR_FLAGS {
    ACR_FLAG_NONE             = 0x0,
    ACR_FLAG_WRITABLE_BY_CALL = 0x1,   // items may be altered by calls
    ACR_FLAG_ALLOCA_CLASS     = 0x2,   // items may be alloca() memory
  };

private:
  static IDTYPE       _last_id_used;
  static BOOL         _structure_not_frozen;

  IDTYPE              _id;

  // A representative member of the class of objects that items in
  // this class may point to; NULL indicates that members of this
  // alias class can't be pointers. We can't point to the
  // ALIAS_CLASS_REP for the pointed-to alias class because of an
  // efficiency issue: We need to combine alias classes from time to
  // time, and it would be painful to find and update everyone who
  // refers to an alias class involved in such a combination.
  ALIAS_CLASS_MEMBER *_member_of_class_pointed_to;

  PENDING_LIST        _pending;
  mUINT32             _flags;

#if Is_True_On
  BOOL                _lda_class;
#endif

public:
  ALIAS_CLASS_REP(void) :
#if Is_True_On
    _lda_class(FALSE),
#endif
    _member_of_class_pointed_to(NULL), _pending(NULL),
    _flags(ACR_FLAG_NONE), _id(++_last_id_used)
      {
	Is_True(_structure_not_frozen,
		("ALIAS_CLASS_REP: Cannot construct with frozen structure"));
      }

  IDTYPE            Id(void) const { return _id; }

  void              Add_pending(ALIAS_CLASS_REP *, ALIAS_CLASSIFICATION &);
  void              Merge_pending(ALIAS_CLASS_REP &);
  void              Process_pending(ALIAS_CLASSIFICATION &);
  PENDING_LIST     &Pending(void) { return _pending; }
#ifdef KEY
  BOOL              Pending_rep_match(ALIAS_CLASS_REP *);
#endif
  void              Join_object_class(ALIAS_CLASS_REP &,
				      ALIAS_CLASSIFICATION &);

  BOOL              Is_pointer_class(void) const
    { return _member_of_class_pointed_to != NULL; }

  ALIAS_CLASS_REP  *Class_pointed_to(void) const
    {
      Is_True(_member_of_class_pointed_to == NULL ||
	      _member_of_class_pointed_to->Alias_class() != NULL,
	      ("ALIAS_CLASS_REP::Class_pointed_to: inconsistent "
	       "data structure"));
      return (_member_of_class_pointed_to != NULL ?
	      _member_of_class_pointed_to->Alias_class() :
	      NULL);
    }

  void              Set_class_pointed_to(ALIAS_CLASS_REP *const cpt)
    {
      Is_True(cpt->Representative() != NULL,
	      ("object class has no representative"));
      _member_of_class_pointed_to = cpt->Representative();
    }

  void              Set_writable_by_call(void)
    { _flags |= ACR_FLAG_WRITABLE_BY_CALL; }

  BOOL              Writable_by_call(void) const
    { return _flags & ACR_FLAG_WRITABLE_BY_CALL; }

  void              Set_alloca_class(void)
    { _flags |= ACR_FLAG_ALLOCA_CLASS; }

  BOOL              Alloca_class(void) const
    { return _flags & ACR_FLAG_ALLOCA_CLASS; }

  void              Print(FILE *,
			  ALIAS_CLASS_REP *global_class = NULL) const;
};


// ----------------------------------------------------------------------


// A class encapsulating a reference (pointer) and its referent
// (object pointed to).

class AC_PTR_OBJ_PAIR {
private:
  ALIAS_CLASS_REP *_ref_class;
  ALIAS_CLASS_REP *_obj_class;

public:
  AC_PTR_OBJ_PAIR(void) : _ref_class(NULL), _obj_class(NULL)
    { }

  AC_PTR_OBJ_PAIR(ALIAS_CLASS_REP *ref_class,
		  ALIAS_CLASS_REP *obj_class) :
    _ref_class(ref_class), _obj_class(obj_class)
      {
	Is_True(ref_class == NULL ||
		ref_class->Class_pointed_to() == obj_class,
		("AC_PTR_OBJ_PAIR: reference must point to object"));
      }

  void           Set_ref_class(ALIAS_CLASS_REP *ref_class)
    { _ref_class = ref_class; }

  ALIAS_CLASS_REP *Ref_class(void) const
    { return _ref_class; }

  void           Set_obj_class(ALIAS_CLASS_REP *obj_class)
    { _obj_class = obj_class; }

  ALIAS_CLASS_REP *Obj_class(void) const
    {
      Is_True(_ref_class == NULL ||
	      _ref_class->Class_pointed_to() == _obj_class,
	      ("AC_PTR_OBJ_PAIR::Obj_class: Reference must refer to object"));
      return _obj_class;
    }
};


// ======================================================================
//
// ALIAS_CLASSIFICATION class fields:
//
// _destination  indicates the context in which alias classification was
//   invoked:
//   AC_DESTINATION_OPT_STAB       --> invoked using the optimizer's
//     auxilliary symbol table OPT_STAB
//   AC_DESTINATION_ALIAS_MANAGER  --> invoked using the symbol table ST,
//     during preopt for LNO
//
// _opt_stab  points to the optimizer auxillary symbol table
//
// _st_idx_to_base_id_map;  ID_MAP<IDTYPE, ST_IDX>
// _ac_id_to_acr_map;  ID_MAP<const ALIAS_CLASS_REP *, IDTYPE>
// _base_id_map;  DYN_ARRAY<BASE_ID_MAP_ENTRY *>
// _preg_num_base_id_map;  ID_MAP<IDTYPE, INT64>
// _memop_classification_map;  WN_MAP
// _indir_classification_map;  WN_MAP
// _memops_classified;  BOOL

// _pool  is the MEM_POOL* for all alias classification allocations
// _mem_pool_valid  is TRUE iff _pool is a valid MEM_POOL
//   (Prelease_resources deletes _pool).

// _collapsed_nested_references;  BOOL
// _member_of_global_class;  ALIAS_CLASS_MEMBER *
// _altered_non_points_to_parms;  ALIAS_CLASS_MEMBER_LIST
// _alloca_memory_members;  ALIAS_CLASS_MEMBER_LIST

// _inaccessible_to_callees  points to a set of aux ID's that can't be
//   accessed by any callee of this routine.
// _const_addr_class  points to the ALIAS_CLASS_REP assigned to any
//   INTCONST that gets dereferenced. For now, we say this points to
//   _global_class.
// _pending_list_home  contains a stack of PENDING_LIST_ITEM objects
//   that have been allocated but are currently available for use
//   NOTE: As far as I can tell, nothing is currently put on the stack,
//   so this list is always empty.  Why not use the linked list structure
//   instead of another stack?
// _tracing  is TRUE iff the alias classificate trace flag is on

// ======================================================================


typedef enum {
  AC_DESTINATION_OPT_STAB,
  AC_DESTINATION_ALIAS_MANAGER
} AC_DESTINATION;


class BASE_ID_MAP_ENTRY;	// defined in opt_alias_class.cxx.


class ALIAS_CLASSIFICATION {
  friend class ALIAS_CLASS_REP;
  friend class MERGE_NEST_REF_CLASSES;

private:
  AC_DESTINATION                _destination;
  OPT_STAB                     *_opt_stab;
  ID_MAP<IDTYPE, ST_IDX>                  _st_idx_to_base_id_map;
  ID_MAP<const ALIAS_CLASS_REP *, IDTYPE> _ac_id_to_acr_map;
  DYN_ARRAY<BASE_ID_MAP_ENTRY *>          _base_id_map;
  ID_MAP<IDTYPE, INT64>                   _preg_num_base_id_map;
  WN_MAP                        _memop_classification_map;
  WN_MAP                        _indir_classification_map;
  BOOL                          _memops_classified;
  MEM_POOL                     *_pool;
  BOOL                          _mem_pool_valid;
  BOOL                          _collapsed_nested_references;
  ALIAS_CLASS_MEMBER           *_member_of_global_class;

  ALIAS_CLASS_MEMBER_LIST       _altered_non_points_to_parms;
  ALIAS_CLASS_MEMBER_LIST       _alloca_memory_members;

  // Set of aux ID's that can't be accessed by any callee of this routine.
  BS                           *_inaccessible_to_callees;

  // class of INTCONST that gets dereferenced. For now, we say this
  // class points to _global_class.
  ALIAS_CLASS_REP              *_const_addr_class;

  PENDING_LIST_HOME             _pending_list_home;

  BOOL                          _tracing;

  // Member functions for internal use
  BOOL                Is_LDA_of_variable(const WN *) const;

  ALIAS_CLASS_REP    *Class_of_base_id_LDID(IDTYPE) const;

  ALIAS_CLASS_REP    *Class_of_base_id_LDA(IDTYPE) const;

  WN                 *Classify_wn_and_kids(WN *);

  AC_PTR_OBJ_PAIR     Classify_lhs_of_store(WN *);
  BOOL                Expr_may_contain_pointer (WN* const expr);
  AC_PTR_OBJ_PAIR     Classify_deref_of_expr(WN *, BOOL);

  void                Set_collapsed_nested_references(void)
    { _collapsed_nested_references = TRUE; }
  BOOL                Collapsed_nested_references(void)
    { return _collapsed_nested_references; }

  BOOL                Assignment_may_xfer_pointer (WN* const); 
  WN                 *Handle_assignment(WN *);
  WN                 *Handle_call(WN *);
  void                Handle_call_of_nested_PU(ST *);
  BOOL                May_icall_nested_PU(const WN *, ST **);

  BOOL                WN_is_alloca_intrinsic(const WN *);

  BOOL                Callee_changes_no_points_to(const WN *, const WN *);
  BOOL                Callee_returns_new_memory(const WN *);
  BOOL                Stmt_stores_return_value(const WN *);
  BOOL                Uses_no_return_value(const WN *);

  WN_MAP              Indir_classification_map(void) const
    { return _indir_classification_map; }

  void                Finalize_ac_map(WN *);
  void                Finalize_ac_map_wn(WN *);

  ALIAS_CLASS_REP    *New_alias_class(ALIAS_CLASS_MEMBER *);

  ALIAS_CLASS_MEMBER *New_alias_class_member(void);
  ALIAS_CLASS_MEMBER *New_alias_class_member(IDTYPE);
  ALIAS_CLASS_MEMBER *New_alias_class_member(WN *);

  void                Find_declared_base_and_offset(ST_IDX,
						    ST_IDX &,
						    INT64 &);
  IDTYPE              New_base_id(const ST *, TY_IDX);
  IDTYPE              ST_base_id(ST *, TY_IDX);
  IDTYPE              Base_id(AUX_ID, TY_IDX);
  IDTYPE              Base_id(ST *, INT64, TY_IDX);
  IDTYPE              WN_base_id(const WN *);
  ID_MAP<IDTYPE, INT64> &Preg_num_base_id_map(void)
    { return _preg_num_base_id_map; }

  ST                 *ST_of_wn(const WN *) const;

  MEM_POOL           *Pool(void) const { return _pool; }

  // Alloc_pending allocates and returns an available PENDING_LIST item
  // from _pending_list_home and initializes it.  Release_pending returns
  // a PENDING_LIST item to _pending_list_home for later reuse.

  PENDING_LIST        Alloc_pending(ALIAS_CLASS_MEMBER *mbr, PENDING_LIST pdg)
    { return _pending_list_home.Produce(mbr, pdg); }
  PENDING_LIST        Release_pending(PENDING_LIST pdg)
    { return _pending_list_home.Receive(pdg); }

  void                Merge_conditional(AC_PTR_OBJ_PAIR,
					AC_PTR_OBJ_PAIR);

  ALIAS_CLASS_REP    *Global_class(void) const
    { return _member_of_global_class->Alias_class(); }

  ALIAS_CLASS_REP    *Const_addr_class(void) const
    { return _const_addr_class; }

  void                Set_inaccessible_to_callees(BS *bs)
    { _inaccessible_to_callees = bs; }

  OPT_STAB           *Opt_stab(void) const { return _opt_stab; }

  BOOL                Tracing(void) const { return _tracing; }

  WN_MAP              Memop_classification_map(void) const
    { return _memop_classification_map; }

  void                Dump_wn_tree(FILE *fp, WN   *wn) const
  {
    if (_destination == AC_DESTINATION_OPT_STAB) {
      fdump_tree_no_st(fp, wn);
    }
    else {
      fdump_tree(fp, wn);
    }
  }

  void                Dump_wn(FILE *fp, WN   *wn) const
  {
    if (_destination == AC_DESTINATION_OPT_STAB) {
      fdump_wn_no_st(fp, wn);
    }
    else {
      fdump_wn(fp, wn);
    }
  }

public:
  ALIAS_CLASSIFICATION(OPT_STAB *, AC_DESTINATION, MEM_POOL *);

  void                Release_resources(void);

  void                Add_to_altered_non_points_to_parms(const ALIAS_CLASS_REP *const acr)
    {
      Is_True(acr->Representative() != NULL,
	      ("ALIAS_CLASSIFICATION::Add_to_altered_non_points_to_parms: "
	       "Representative must not be NULL"));
      _altered_non_points_to_parms.push_front(acr->Representative());
    }

  ALIAS_CLASS_MEMBER_LIST &Altered_non_points_to_parms(void)
    { return _altered_non_points_to_parms; }

  BS                 *Inaccessible_to_callees(void) const
    { return _inaccessible_to_callees; }

  void                Classify_memops(WN *);

  IDTYPE              Alias_class(const WN *) const;

  void                Copy_alias_class(const WN *, WN *);

  BOOL                Non_alloca_memop(IDTYPE) const;

  BOOL                Writable_by_call(IDTYPE) const;

  void                Print(FILE *) const;
};
#endif
