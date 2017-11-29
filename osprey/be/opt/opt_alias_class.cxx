/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_alias_class.cxx,v $
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
// This code implements a slightly simplified version of
// Bjarne Steensgaard, "Points-to Analysis in Almost Linear Time",
// Proceedings of 23rd Annual ACM SIGACT-SIGPLAN Symposium. 
// (USA:  ACM, 1996. p. 32-41)

// Question: Do we have vsym's in the aux_stab already at this time
//           (i.e., prior to flow-sensitive analysis or mu/chi build)?
//   Answer: Yes. Each array access, for example, has a vsym
//   associated with it, since such accesses always show up as
//   ILOAD/ISTORE(LDA).

//   EXPR
// ILOAD
//
// gets treated like
//
//   EXPR
// STID unique_var
//   LDID unique_var
// ILOAD
//
// except that we don't represent unique_var's alias class information
// at all.

// The classification has the following properties:
//   it is flow-free;
//   if two expressions or variables may be aliased, they are in the
//   same alias class;
//   if two variables or expressions may be pointed to by the same
//   pointer, they are in the same alias class (this is the biggest
//   conservatism inherent in the algorithm).

// For the interface to clients, see opt_alias_class.h

#include <stdint.h>
#include <set>
#include <math.h>

#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "opt_points_to.h"	// PESSIMISTIC_AC_ID, OPTIMISTIC_AC_ID
#include "opt_base.h"
#include "opt_config.h"		// WOPT_Enable_Verbose
#include "opt_sym.h"
#include "opt_alias_class.h"
#include "opt_alias_mgr.h"
#include "wintrinsic.h"

extern "C" {
#include "bitset.h"
}
#ifdef KEY
extern BOOL ST_Has_Dope_Vector(ST *st);
#endif

IDTYPE ALIAS_CLASS_REP::_last_id_used;
BOOL   ALIAS_CLASS_REP::_structure_not_frozen;


#if Is_True_On
static const int num_acr_table_entries = 100;
ALIAS_CLASS_REP *acr_table[num_acr_table_entries];

static const int num_acm_table_entries = 100;
ALIAS_CLASS_MEMBER *acm_table[num_acm_table_entries];
static int next_acm_idx = 0;


void print_table(void)
{
  UINT i;
  INT acr_size = Min(ALIAS_CLASS_REP::_last_id_used + 1,
		     num_acr_table_entries);
  for (i = 0; i < acr_size; ++i) {
    if (acr_table[i] != NULL) {
      fprintf(TFile, "acr_table[%2u]: ", i);
      acr_table[i]->Print(TFile);
    }
  }
  INT acm_size = Min(next_acm_idx, num_acm_table_entries);
  for (i = 0; i < acm_size; ++i) {
    fprintf(TFile, "acm_table[%2u]: ", i);
    if (acm_table[i] == acm_table[i]->Alias_class()->Representative()) {
      fprintf(TFile, "self");
    }
    else {
      acm_table[i]->Print(TFile);
    }
    fprintf(TFile, " in ");
    if (acm_table[i]->Alias_class() == NULL) {
      fprintf(TFile, "no class\n");
    }
    else {
      acm_table[i]->Alias_class()->Print(TFile);
    }
  }
}
#endif


class BASE_ID_MAP_ENTRY {
private:
  const ST        *_st;
  ALIAS_CLASS_REP  _base_lda_class;
public:
#if Is_True_On
  BASE_ID_MAP_ENTRY(void)
    {
      if (_base_lda_class.Id() < num_acr_table_entries) {
	acr_table[_base_lda_class.Id()] = &_base_lda_class;
      }
    }
#endif

  void             Set_st(const ST *st)  { _st = st; }
  const ST        *St(void)        const { return _st; }
  ALIAS_CLASS_REP &Lda_class(void)       { return _base_lda_class; }
};


ALIAS_CLASSIFICATION::ALIAS_CLASSIFICATION(OPT_STAB       *opt_stab,
					   AC_DESTINATION  destination,
					   MEM_POOL       *pool) :
  _opt_stab(opt_stab), _destination(destination), _pool(pool),
  _pending_list_home(pool), _base_id_map(pool),
  _collapsed_nested_references(FALSE),
  _preg_num_base_id_map(128, (IDTYPE) 0, pool, FALSE),
  _st_idx_to_base_id_map(256, (IDTYPE) 0, pool, FALSE),
  _ac_id_to_acr_map(256, NULL, pool, FALSE),
  _altered_non_points_to_parms(pool), _alloca_memory_members(pool),
  _memops_classified(FALSE), _mem_pool_valid(TRUE)
{
  OPT_POOL_Push(_pool, MEM_DUMP_FLAG+20);
  _tracing = Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG);
  ALIAS_CLASS_REP::_last_id_used = PESSIMISTIC_AC_ID;
  ALIAS_CLASS_REP::_structure_not_frozen = TRUE;

  // Set up the _member_of_global_class:
  _member_of_global_class = New_alias_class_member();
  ALIAS_CLASS_REP *dummy_acr = New_alias_class(_member_of_global_class);
  dummy_acr->Set_class_pointed_to(Global_class());

  // Set up the _const_addr_class:
  ALIAS_CLASS_MEMBER *dummy_acm = New_alias_class_member();
  _const_addr_class = New_alias_class(dummy_acm);
  _const_addr_class->Set_class_pointed_to(Global_class());

  // Skip base ID zero
  _base_id_map.Initidx(0);
}


// ======================================================================
//
// ALIAS_CLASS_REP * New_alias_class(ALIAS_CLASS_MEMBER *const acm)
//   allocates and initializes an ALIAS_CLASS_REP for a set containing
//   only the ALIAS_CLASS_MEMBER *acm.
//
// ALIAS_CLASS_MEMBER * New_alias_class_member(void)
// ALIAS_CLASS_MEMBER * New_alias_class_member(const IDTYPE)
// ALIAS_CLASS_MEMBER * New_alias_class_member(WN *)
//   Each of these three forms allocates and initializes an
//   ALIAS_CLASS_MEMBER object of type ACM_NONE, ACM_BASE, and ACM_WN
//   respectively.  The resulting ALIAS_CLASS_MEMBER is not yet assigned
//   to an ALIAS_CLASS_REP set, so New_alias_class or Put_in_set is
//   usually invoked immediately New_alias_class_member finishes.
//
// ======================================================================


ALIAS_CLASS_REP *
ALIAS_CLASSIFICATION::New_alias_class(ALIAS_CLASS_MEMBER *const acm)
{
  ALIAS_CLASS_REP *retval = CXX_NEW(ALIAS_CLASS_REP, Pool());
#if Is_True_On
  if (retval->Id() < num_acr_table_entries) {
    acr_table[retval->Id()] = retval;
  }
#endif
  acm->Put_in_set(retval);
  if (Tracing() && WOPT_Enable_Verbose) {
    fprintf(TFile, "Creating ");
    retval->Print(TFile, Global_class());
  }
  return retval;
}


// Improve the following to use an internally-maintained free list.
ALIAS_CLASS_MEMBER *
ALIAS_CLASSIFICATION::New_alias_class_member(void)
{
  ALIAS_CLASS_MEMBER *acm = CXX_NEW(ALIAS_CLASS_MEMBER, Pool());
#if Is_True_On
  if (next_acm_idx < num_acm_table_entries) {
    acm_table[next_acm_idx++] = acm;
  }
#endif

  return acm;
}


ALIAS_CLASS_MEMBER *
ALIAS_CLASSIFICATION::New_alias_class_member(const IDTYPE base_id)
{
  ALIAS_CLASS_MEMBER *acm = CXX_NEW(ALIAS_CLASS_MEMBER(base_id), Pool());

#if Is_True_On
  if (next_acm_idx < num_acm_table_entries) {
    acm_table[next_acm_idx++] = acm;
  }
#endif

  return acm;
}


ALIAS_CLASS_MEMBER *
ALIAS_CLASSIFICATION::New_alias_class_member(WN *wn)
{
  ALIAS_CLASS_MEMBER *acm = CXX_NEW(ALIAS_CLASS_MEMBER(wn), Pool());

#if Is_True_On
  if (next_acm_idx < num_acm_table_entries) {
    acm_table[next_acm_idx++] = acm;
  }
#endif

  return acm;
}


// ======================================================================
//
// ST * ST_of_wn(const WN *const wn)
//   returns a pointer to the ST entry corresponding to wn
//
// BOOL Is_LDA_of_variable(const WN *const)
//
// ======================================================================


inline ST *
ALIAS_CLASSIFICATION::ST_of_wn(const WN *const wn) const
{
  return (_destination == AC_DESTINATION_OPT_STAB ?
	  Opt_stab()->St(WN_aux(wn)) :
	  WN_st(wn));
}


inline BOOL
ALIAS_CLASSIFICATION::Is_LDA_of_variable(const WN *const wn) const
{
  ST *st = ST_of_wn(wn);

  Is_True(ST_class(st) != CLASS_PREG, ("Is_LDA_of_variable: CLASS_PREG"));

  return (ST_class(st) == CLASS_UNK ||
	  ST_class(st) == CLASS_VAR ||
	  // Stupid FORTRAN -- takes addresses of constants.
	  ST_class(st) == CLASS_CONST ||  // constant in memory
	  ST_class(st) == CLASS_BLOCK);
}


// ======================================================================
//
// A base_id integer is an index into the DYN_ARRAY _base_id_map of
// pointers to BASE_ID_MAP_ENTRY *, through which ALIAS_CLASS_REP objects
// can be accessed.  Two maps, _st_idx_to_base_id_map and
// _preg_num_base_id_map, provide the means of converting ST_IDXs and
// register PREG numbers into base_id numbers.
//
// IDTYPE New_base_id(const ST *st, TY_IDX ty)
//   Invoked only by Base_id and ST_base_id.  Allocates and initializes
//   a new BASE_ID_MAP_ENTRY (including ALIAS_CLASS_ENTRY) and a new
//   ALIAS_CLASS_REP for for the ST st.  Should never be called after
//   _structure_not_frozen is set to FALSE.
//
// IDTYPE ST_base_id(ST *st, const TY_IDX ty)
//   helper procedure invoked by Base_id; looks up and returns the
//   base_id for the given (non-PREG) ST entry, invoking New_base_id to
//   create a new one if necessary.
//
// IDTYPE Base_id(ST *const, const INT64, const TY_IDX)
//   looks up and returns the base_id for the given ST entry and offset,
//   invoking New_base_id to create a new one if necessary.
//
// IDTYPE Base_id(const AUX_ID aux_id, const TY_IDX)
//   looks up and returns the base_id for optimizer symbol table OPT_STAB
//   entry aux_id, invoking New_base_id to create a new one if necessary.
//
// void Find_declared_base_and_offset(ST_IDX st_idx,
//                                    ST_IDX &declared_base_idx,
//                                    INT64  &declared_offset)
//
// IDTYPE WN_base_id(const WN *)
//   looks up and returns the base_id of a WN, invoking New_base_id to
//   create a new one if necessary.
//
// ALIAS_CLASS_REP * Class_of_base_id_LDA(const IDTYPE) const
//   looks up the base_id of a LDA or LDMA expression and returns a
//   pointer to its ALIAS_CLASS_REP alias class representative.
//
// ALIAS_CLASS_REP * Class_of_base_id_LDID(const IDTYPE) const
//   looks up the base_id of a LDID or LDBITS expression and returns a
//   pointer to its ALIAS_CLASS_REP alias class representative.
//
// ======================================================================


IDTYPE
ALIAS_CLASSIFICATION::New_base_id(const ST *st, TY_IDX ty)
{
  BOOL   weird_class = FALSE;
  IDTYPE id          = _base_id_map.Newidx();

  if (Tracing()) {
    fprintf(TFile, "Allocating base ID %u\n", id);
  }

  // Insert  st --> id  into the _st_idx_to_base_id_map map.
  // Don't mess with the mapping from ST to base ID for registers;
  // their base ID's are found through the Preg_num_base_id_map using
  // the register number.  The calling procedure Base_id will update
  // Preg_num_base_id_map. 
  if (ST_sclass(st) != SCLASS_REG) {
    ST_IDX st_idx = ST_st_idx(st);
    _st_idx_to_base_id_map.Insert(st_idx, id);
  }

  // Allocation of LDA class as part of the base ID map entry, so
  // fields have to be set manually here instead of through
  // New_alias_class(), etc.
  //
  // We used to have no representative for LDA classes, but now that
  // we have the Altered_non_points_to_parms() mechanism, we need one
  // for uniformity with all the other classes.
  _base_id_map[id] = CXX_NEW(BASE_ID_MAP_ENTRY, Pool());
  New_alias_class_member()->Put_in_set(&(_base_id_map[id]->Lda_class()));
  _base_id_map[id]->Lda_class().Representative()->Set_lda_kind();
  _base_id_map[id]->Set_st(st);

  if (Tracing()) {
    fprintf(TFile, "Base ID %3u is ", id);
  }

  ST_SCLASS storage_class = ST_sclass(st);

  // If this base_id is a local variable or parameter, it gets its
  // own class.
  if (((storage_class == SCLASS_AUTO ||
        storage_class == SCLASS_FORMAL ||
        storage_class == SCLASS_FORMAL_REF) &&
       ST_IDX_level(ST_st_idx(st)) == CURRENT_SYMTAB) ||
      storage_class == SCLASS_REG) {
    // Set up the LDA and LDID classes for this variable...
    ALIAS_CLASS_MEMBER *ldid_item = New_alias_class_member(id);
    ALIAS_CLASS_REP    *ldid_class = New_alias_class(ldid_item);
    _base_id_map[id]->Lda_class().Set_class_pointed_to(ldid_class);

    // If this base_id is a parameter, its class points to the global
    // class if it can point to anything.
    if (storage_class == SCLASS_FORMAL ||
	storage_class == SCLASS_FORMAL_REF) {
      BOOL cannot_be_pointer = FALSE;

      if ((ST_class(st) == CLASS_VAR) ||
	  (ST_class(st) == CLASS_CONST) ||
	  (ST_class(st) == CLASS_NAME)) {
	TY_IDX    st_ty         = ST_type(st);

	while (TY_kind(st_ty) == KIND_ARRAY) {
	  st_ty = TY_etype(st_ty);
	}

	UINT64 lang = PU_src_lang(Get_Current_PU());
	// TODO: In the following check, we can do better than just
	// checking for KIND_SCALAR. What we really want is to make sure
	// no pointer is being passed. If it's KIND_STRUCT with no members
	// containing a KIND_POINTER, we're OK. That sort of thing.
	cannot_be_pointer = (WOPT_Enable_Alias_Class_Fortran_Rule &&
			     (lang == PU_F77_LANG ||
			      lang == PU_F90_LANG) &&
			     (TY_kind(st_ty) == KIND_SCALAR));
	// Here is the only place we use the argument "ty"; we use it only
	// for assertion checking, to guarantee that the WN_ty doesn't
	// contradict the pointer-ness of the ST_ty.
	if (cannot_be_pointer) {
	  while (TY_kind(ty) == KIND_ARRAY) {
	    ty = TY_etype(ty);
	  }
	  Is_True(ty == (TY_IDX) 0 ||
		  TY_kind(ty) == KIND_SCALAR,
		  ("Non-pointer ST type, but possibly pointer WN type"));
	}
      }

      if (cannot_be_pointer) {
	if (Tracing()) {
	  fprintf(TFile, "nonptr: ");
	}
      }
      else {
	if (Tracing()) {
	  fprintf(TFile, "  parm: ");
	}
	_base_id_map[id]->Lda_class().Class_pointed_to()->
	  Set_class_pointed_to(Global_class());
      }
    }
    else {
      if (Tracing()) {
	fprintf(TFile, " local: ");
      }
    }
  }
  // If this base_id is a global, it goes in the global class.
  else if (storage_class == SCLASS_PSTATIC || // PSTATIC can be
					      // initialized with
					      // someone's address,
					      // and someone can be
					      // initialized with
					      // its address.
	   storage_class == SCLASS_FSTATIC ||
	   storage_class == SCLASS_COMMON  ||
	   storage_class == SCLASS_UGLOBAL ||
	   storage_class == SCLASS_DGLOBAL ||
	   // storage class AUTO here signifies that the item is an
	   // uplevel reference and therefore must be treated as
	   // global because its address can be stored into a global,
	   // and it may contain the address of a global. Non-uplevel
	   // references with SCLASS_AUTO, SCLASS_FORMAL or
           // SCLASS_FORMAL_REF are handled as locals above.
	   // Including uplevel references here is the fix for 555533.
	   // 634200 was a milder case of the same problem, and only
	   // part of the fix was required to get that one right; that
	   // bug made me mistakenly treat uplevel references as
	   // parameters; 555533 makes clear that we have to go
	   // all the way and treat them as globals.
	   storage_class == SCLASS_AUTO    ||
           storage_class == SCLASS_FORMAL  ||
           storage_class == SCLASS_FORMAL_REF  ||
	   // storage class can be UNKNOWN for constant data because CG may
	   // already have run for an earlier PU and in the process lowered
	   // string (and maybe other) constants to .rodata. Unfortunately,
	   // storage class can be UNKNOWN for PU-level static data,
	   // too. Ugh. We have to be conservative.
	   storage_class == SCLASS_UNKNOWN ||
	   (storage_class == SCLASS_EXTERN &&
	    ST_class(st) != CLASS_FUNC)) {
    // The LDA of this variable points to the global class. Do we
    // need to make a separate entry in the global class for this
    // object?
    if (Tracing()) {
      fprintf(TFile, "global: ");
    }
    _base_id_map[id]->Lda_class().Set_class_pointed_to(Global_class());
  }
  else {
    // An LDA of this symbol cannot be a pointer to a variable. Set up
    // the LDA class, but make sure the class it points to is NULL.
    Is_True(_base_id_map[id]->Lda_class().Class_pointed_to() == NULL,
	    ("ALIAS_CLASSIFICATION::New_base_id: non-variable LDA must "
	     "not point"));
    if (Tracing()) {
      fprintf(TFile, " other: ");
      weird_class = TRUE;
    }
  }
  if (Tracing()) {
    Print_ST(TFile, st, WOPT_Enable_Verbose || weird_class);
  }
  return id;
}


IDTYPE
ALIAS_CLASSIFICATION::ST_base_id(ST *st, const TY_IDX ty)
{
  IDTYPE base_id = _st_idx_to_base_id_map.Lookup(ST_st_idx(st));
  if (base_id == (IDTYPE) 0) {
    base_id = New_base_id(st, ty);
  }
  return base_id;
}


IDTYPE
ALIAS_CLASSIFICATION::Base_id(      ST     *const base_st,
			      const INT64         base_ofst,
			      const TY_IDX        ty)
{
  // If the base_st is the PREG base, we treat it specially. Every
  // PREG gets its own base ID, even though they share a base ST. In
  // principle we can do this sort of thing for any base ST where we
  // know that different offsets from that base do not overlap.
  if (ST_sclass(base_st) == SCLASS_REG) {
    // This is a PREG. Use the preg number to map to the base ID.
    IDTYPE base_id = Preg_num_base_id_map().Lookup(base_ofst);
    if (base_id == (IDTYPE) 0) {
      base_id = New_base_id(base_st, ty);
      if (Tracing()) {
	fprintf(TFile, "base_ofst is %lld\n", base_ofst);
      }
      Preg_num_base_id_map().Insert(base_ofst, base_id);
    }
    return base_id;
  }
  else {
    return ST_base_id(base_st, ty);
  }
}


IDTYPE
ALIAS_CLASSIFICATION::Base_id(const AUX_ID aux_id, const TY_IDX ty)
{
  ST    *base_st   = Opt_stab()->Base(aux_id);
  INT64  base_ofst = Opt_stab()->Base_ofst(aux_id);

  return Base_id(base_st, base_ofst, ty);
}


void
ALIAS_CLASSIFICATION::Find_declared_base_and_offset(ST_IDX  st_idx,
						    ST_IDX &declared_base_idx,
						    INT64  &declared_offset)
{
  ST *st             = &St_Table[st_idx];
  ST_IDX base_st_idx = ST_base_idx(st);
  ST *base_st;

  // REMOVE: declared_offset   = 0;
  declared_base_idx = st_idx;

  while (st_idx != base_st_idx &&
	 (base_st = &St_Table[base_st_idx], TRUE) &&
	 (ST_class(base_st) != CLASS_BLOCK ||
	  !STB_compiler_layout(base_st))) {
    declared_offset   += ST_ofst(st);
    declared_base_idx  = ST_base_idx(st);
    st = base_st;
    st_idx = base_st_idx;
  }
}


IDTYPE
ALIAS_CLASSIFICATION::WN_base_id(const WN *wn)
{
  TY_IDX ty;
  if (WN_operator(wn) == OPR_LDA ||
      WN_operator(wn) == OPR_LDMA) {
    if (TY_kind(WN_ty(wn)) == KIND_POINTER) {
      ty = TY_pointed(WN_ty(wn));
    }
    else {
      // It can sometimes happen through someone's screw-up and through
      // the fact that we don't enforce our verifier's rules that the type
      // of an LDA node is not a KIND_POINTER type. If that happens, pass
      // (TY_IDX) 0 as the type index for the WN.
      ty = (TY_IDX) 0;
    }
  }
  else {
    ty = WN_ty(wn);
  }
  if (_destination == AC_DESTINATION_OPT_STAB) {
    return Base_id(WN_aux(wn), ty);
  }
  else {
    Is_True(_destination == AC_DESTINATION_ALIAS_MANAGER,
	    ("ALIAS_CLASSIFICATION: Someone added a new "
	     "destination"));
    ST_IDX base_st_idx;
    INT64  base_offset = WN_offset(wn);
    Find_declared_base_and_offset(WN_st_idx(wn), base_st_idx,
				  base_offset);
    return Base_id(&St_Table[base_st_idx], base_offset, ty);
  }
}


ALIAS_CLASS_REP *
ALIAS_CLASSIFICATION::Class_of_base_id_LDA(const IDTYPE base_id) const
{
  return &_base_id_map[base_id]->Lda_class();
}


ALIAS_CLASS_REP *
ALIAS_CLASSIFICATION::Class_of_base_id_LDID(const IDTYPE base_id) const
{
  return Class_of_base_id_LDA(base_id)->Class_pointed_to();
}


// ======================================================================
//
// Each ALIAS_CLASS_REP maintains a "pending" list of pointers to
// ALIAS_CLASS_MEMBER objects that need to be merged with this class.
//
// The pending lists are implemented as linked lists.
// ALIAS_CLASSIFICATION maintains a list of PENDING_LIST objects
// that have been allocated but are currently available for reuse.
//
// void
// ALIAS_CLASS_REP::Add_pending(ALIAS_CLASS_REP, ALIAS_CLASSIFICATION &);
//   Prepends the given ALIAS_CLASS_REP to the front of the pending list
//   of this ALIAS_CLASS_REP.
//
// void
// ALIAS_CLASS_REP::Process_pending(ALIAS_CLASSIFICATION &);
//   For each item in Pending(), join the item's pointed-to class with
//   Class_pointed_to(). Free the Pending() entries as we go.
//
// void
// ALIAS_CLASS_REP::Merge_pending(ALIAS_CLASS_REP &that);
//   Move all pending items from that ALIAS_CLASS_REP to this
//   ALIAS_CLASS_REP.
//
// ======================================================================


void
ALIAS_CLASS_REP::Add_pending(ALIAS_CLASS_REP      *item,
			     ALIAS_CLASSIFICATION &ac)
{
  Is_True(item != NULL && item->Representative() != NULL,
	  ("ALIAS_CLASSIFICATION: Cannot add <NULL> to pending list"));
  _pending = ac.Alloc_pending(item->Representative(), _pending);
}


void
ALIAS_CLASS_REP::Process_pending(ALIAS_CLASSIFICATION &ac)
{
  // For each item in _pending, join the item's pointed-to class with
  // Class_pointed_to(). Free the _pending entries as we go.
  Is_True(Is_pointer_class(),
	  ("ACR::Process_pending: Must be pointer class"));


  while (_pending != NULL) {
    ALIAS_CLASS_REP *item = _pending->Node()->Alias_class();
    _pending = ac.Release_pending(_pending);
    Join_object_class(*item, ac);
  }
}

#ifdef KEY
BOOL
ALIAS_CLASS_REP::Pending_rep_match(ALIAS_CLASS_REP *rep)
{
  PENDING_LIST lst = _pending;
  while (lst != NULL) {
    ALIAS_CLASS_REP *item = lst->Node()->Alias_class();
    if (item == rep) 
      return TRUE;
    lst = lst->Next();
  }
  return FALSE;
}
#endif
void
ALIAS_CLASS_REP::Merge_pending(ALIAS_CLASS_REP &that)
{
  // Put all of that's pending items into *this's pending set.
  _pending = _pending->Merge(that.Pending());
}


void
ALIAS_CLASS_REP::Join_object_class(ALIAS_CLASS_REP &that,
				   ALIAS_CLASSIFICATION &ac)
{
  ALIAS_CLASS_REP *cpt      =      Class_pointed_to();
  ALIAS_CLASS_REP *that_cpt = that.Class_pointed_to();

  if (cpt == NULL && that_cpt == NULL) {
    // Merge the two pending lists.
    Merge_pending(that);
  }
  else if (cpt == NULL) {
    Set_class_pointed_to(that_cpt);
    Process_pending(ac);
  }
  else if (that_cpt == NULL) {
    that.Set_class_pointed_to(cpt);
    that.Process_pending(ac);
  }
  else {
    Is_True(!cpt->Representative()->Is_LDA_kind(),
	    ("ALIAS_CLASSIFICATION: Cannot join LDA class"));
    cpt->Union(*that_cpt);
  }

  if (cpt != NULL && that_cpt != NULL && cpt != that_cpt) {
    cpt->Join_object_class(*that_cpt, ac);
    Is_True(cpt->Class_pointed_to() == that_cpt->Class_pointed_to(),
	    ("ACR::Join: Inconsistent join"));
  }
}


// ======================================================================
//
// Print functions for tracing ALIAS_CLASS_REP and ALIAS_CLASS_MEMBER
//
// ======================================================================


void
ALIAS_CLASS_REP::Print(FILE *fp, ALIAS_CLASS_REP *global_class) const
{
  fprintf(fp, "class %u ", Id());
  FmtAssert(Id() != 0 || global_class == NULL || global_class == this,
	    ("Class of ID 1 must be global"));
  if (global_class != NULL &&
      global_class == this) {
    fprintf(fp, "(global)");
  }
  else {
    if (_representative->Null_parent()) {
      fprintf(fp, "represented by ");
      if (Representative() == NULL) {
	fprintf(fp, "<null> ");
      }
      else {
	Representative()->Print(fp);
      }
    }
    else {
      fprintf(fp, "[stale] ");
    }
  }
  fprintf(fp, " -->");
  if (Is_pointer_class()) {
    fprintf(fp, " class %u ", Class_pointed_to()->Id());
    if (global_class != NULL &&
	Class_pointed_to() == global_class) {
      fprintf(fp, "(global)");
    }
    else {
      fprintf(fp, "(containing ");
      _member_of_class_pointed_to->Print(fp);
      fprintf(fp, ")");
    }
  }
  else {
    fprintf(fp, " <NULL>");
  }
  fprintf(fp, "\n");
}


void
ALIAS_CLASS_MEMBER::Print(FILE *fp) const
{
  if (_kind == ACM_BASE) {
    fprintf(fp, "base ID %u", Base_id());
  }
  else if (_kind == ACM_WN) {
    fprintf(fp, "wn ");
    fdump_wn_no_st(fp, Wn());
  }
  else if (_kind == ACM_LDA) {
    fprintf(fp, "LDA");
  }
  else {
    fprintf(fp, "unknown <0x%p>", this);
  }
}


// ======================================================================


void
ALIAS_CLASSIFICATION::Merge_conditional(AC_PTR_OBJ_PAIR lhs,
					AC_PTR_OBJ_PAIR rhs)
{
  if (Tracing()) {
    fprintf(TFile, "Conditional merge of ");
    if (lhs.Obj_class() == NULL) {
      fprintf(TFile, "<NULL> ");
    }
    else {
      lhs.Obj_class()->Print(TFile);
    }
    fprintf(TFile, "with ");
    if (rhs.Obj_class() == NULL) {
      fprintf(TFile, "<NULL>\n");
    }
    else {
      rhs.Obj_class()->Print(TFile);
    }
  }
  // If the rhs ref class ever becomes a pointer, the lhs obj class
  // must join the rhs obj class.
  if (rhs.Obj_class() != NULL) {
    lhs.Ref_class()->Join_object_class(*rhs.Ref_class(), *this);
  }
  else {
    FmtAssert(lhs.Ref_class() != NULL,
	      ("AC::Merge_conditional: Cannot merge deref(LDA) class"));

    // The RHS could be a constant expression or something else
    // unclassifiable.
    if (rhs.Ref_class() != NULL) {
      rhs.Ref_class()->Add_pending(lhs.Ref_class(), *this);
    }
  }
}


static BOOL
Is_fortran_reference_parm(WN *expr)
{
  if (WN_operator(expr) != OPR_PARM) {
    return FALSE;
  }
  return WN_Parm_By_Reference(expr);
}


// Until I do something sensible like move common pieces into one
// source file, changes made in the following function should also be
// made in the analogous function in
// ipa/main/optimize/ipo_alias_class.cxx.
static BOOL
Opcode_cannot_be_pointer_value(const OPERATOR opr,
			       const OPCODE   opc)
{
  switch (opr) {
  case OPR_LAND:
  case OPR_LIOR:
  case OPR_CAND:
  case OPR_CIOR:
  case OPR_LT:
  case OPR_GT:
  case OPR_LE:
  case OPR_GE:
  case OPR_EQ:
  case OPR_NE:
    return TRUE;
  case OPR_MPY:
  case OPR_DIV:
  case OPR_MOD:
  case OPR_ADD:
  case OPR_SUB:
  case OPR_NEG:
    return MTYPE_is_float(OPCODE_rtype(opc));
  default:
    return FALSE;
  }
}


// ======================================================================


// NULL in both components means the expression is not now and can
// never be a pointer.

AC_PTR_OBJ_PAIR
ALIAS_CLASSIFICATION::Classify_deref_of_expr(WN  *const expr,
					     BOOL expr_must_point)
{
  OPCODE   opc = WN_opcode(expr);
  OPERATOR opr = OPCODE_operator(opc);

  if (WOPT_Enable_Verbose && Tracing()) {
    fprintf(TFile, "cdoe: analyzing ");
    Dump_wn(TFile, expr);
  }
  if (OPCODE_is_leaf(opc)) {
    if ((_destination == AC_DESTINATION_OPT_STAB) &&
	OPERATOR_has_aux(opr)) {
      // Take care of the case where we have initialization like:
      // int y;
      // int *const x = &y;
      //
      // In this case, we want y to have a consistent alias class even
      // if it isn't directly mentioned anywhere in the code. The
      // reason is that the OPT_STAB has assigned an aux ID to y, and
      // will sometimes generate LDID/STID of y in place of
      // ILOAD/ISTORE of x.
      //
      // See the related comments in OPT_STAB::Count_syms(WN *) in
      // opt_sym.cxx.
      std::set<AUX_ID> processed_ids;

      AUX_ID lhs_aux_id = WN_aux(expr);
      ST *lhs_st = Opt_stab()->Aux_stab_entry(lhs_aux_id)->St();
      while (lhs_st != NULL && ST_is_initialized(lhs_st)) {
	INITV_IDX initv;
	if ((initv = ST_has_initv(lhs_st)) &&
	    INITV_kind(Initv_Table[initv]) == INITVKIND_SYMOFF) {
	  ST *rhs_st = &St_Table[INITV_st(Initv_Table[initv])];
	  AUX_ID rhs_aux_id =
	    Opt_stab()->Find_sym_with_st_and_ofst(rhs_st,
						  INITV_ofst(Initv_Table[initv]));
	  if (Tracing()) {
	    fprintf(TFile, "ALIAS_CLASSIFICATION: process initialization lhs=%d rhs=%d\n",
		    lhs_aux_id, rhs_aux_id);
	  }

	  if (processed_ids.find(lhs_aux_id) == processed_ids.end())
	    processed_ids.insert(lhs_aux_id);

	  // 
	  // FmtAssert(rhs_aux_id != (AUX_ID) NULL,
	  // ("ALIAS_CLASSIFICATION: Symbol table inconsistent "
	  //  "for int *const var1 = &var2"));
	  //  alias class should follow what has been entered into opt_stab.
	  //
	  if (rhs_aux_id != 0) {
	    IDTYPE lhs_base_id = Base_id(lhs_aux_id, WN_ty(expr));
	    ALIAS_CLASS_REP *lhs_acr = Class_of_base_id_LDID(lhs_base_id);
	    AC_PTR_OBJ_PAIR lhs_class(lhs_acr, lhs_acr->Class_pointed_to());

	    IDTYPE rhs_base_id =
	      Base_id(rhs_aux_id,
		      Opt_stab()->Aux_stab_entry(rhs_aux_id)->Ty());
	    ALIAS_CLASS_REP *rhs_lda_acr = Class_of_base_id_LDA(rhs_base_id);
	    AC_PTR_OBJ_PAIR rhs_class(rhs_lda_acr,
				      rhs_lda_acr->Class_pointed_to());
	    Merge_conditional(lhs_class, rhs_class);

	    // break recurrence due to "struct s1 = {&s1; };"
	    if (processed_ids.find(rhs_aux_id) != processed_ids.end())
	      lhs_st = NULL;
	    else {
	      // Keep following the chain now...
	      lhs_aux_id = rhs_aux_id;
	      lhs_st = Opt_stab()->Aux_stab_entry(lhs_aux_id)->St();
#if Is_True_On
	      // Check that lhs_st and rhs_st have the same base before
	      // looping back.
	      INT64  dummy_offset;
	      ST_IDX lhs_base_idx;
	      Find_declared_base_and_offset(ST_st_idx(lhs_st),
					    lhs_base_idx,
					    dummy_offset);
	      ST_IDX rhs_base_idx;
	      Find_declared_base_and_offset(ST_st_idx(rhs_st),
					    rhs_base_idx,
					    dummy_offset);
	      FmtAssert(lhs_base_idx == rhs_base_idx,
			("ALIAS_CLASSIFICATION: Inconsistent bases for "
			 "initializer ST's"));
#endif
	    }
	  } else {
	    lhs_st = NULL;
	  }
	} else {
	  lhs_st = NULL;
	}
      }
    }
    switch (opr) {
    case OPR_LDA:
    case OPR_LDMA:
      {
	ALIAS_CLASS_REP *lda_class  = Class_of_base_id_LDA(WN_base_id(expr));
	ALIAS_CLASS_REP *ldid_class = Class_of_base_id_LDID(WN_base_id(expr));
	if (expr_must_point &&
	    (ldid_class == NULL)) {
	  // This can happen when people perform direct loads from
	  // code items. For example,
	  // return *((int *) main);
	  if (WOPT_Enable_Verbose && Tracing()) {
	    fprintf(TFile, " --- Expression is indirect load of code\n");
	  }
	  lda_class->Set_class_pointed_to(New_alias_class(New_alias_class_member()));
	  ldid_class = lda_class->Class_pointed_to();
	}
	return AC_PTR_OBJ_PAIR(lda_class, ldid_class);
      }
    case OPR_LDA_LABEL:
      return AC_PTR_OBJ_PAIR(Const_addr_class(), Global_class());
    case OPR_INTCONST:
      // TODO: Maybe find a way to
      // assign base_id's to integer constants so we can match them up
      // here. We need such a mechanism to fully analyze the points-to
      // behavior of ILOAD(INTCONST) or ISTORE(INTCONST).
      if (expr_must_point) {
	//DevWarn("ALIAS_CLASSIFICATION: Dereference of INTCONST "
	//"not fully analyzed");
	return AC_PTR_OBJ_PAIR(Const_addr_class(), Global_class());
      }
      return AC_PTR_OBJ_PAIR(NULL, NULL);
    case OPR_LDID:
    case OPR_LDBITS:
      {
	ALIAS_CLASS_REP *ref = Class_of_base_id_LDID(WN_base_id(expr));
	if (ref == NULL) {
	  // This can happen when people perform direct loads from
	  // code items. For example,
	  // return *((int *) main);
	  if (WOPT_Enable_Verbose && Tracing()) {
	    fprintf(TFile, " --- Expression is direct load of code\n");
	  }
	  Class_of_base_id_LDA(WN_base_id(expr))->
	    Set_class_pointed_to(New_alias_class(New_alias_class_member()));
	  ref = Class_of_base_id_LDID(WN_base_id(expr));
	  Is_True(ref != NULL,
		  ("ALIAS_CLASSIFICATION: Unable to make function pointer "
		   "point to data"));
	}
	if (expr_must_point && !ref->Is_pointer_class()) {
	  ALIAS_CLASS_MEMBER *obj_dummy = New_alias_class_member();
	  ref->Set_class_pointed_to(New_alias_class(obj_dummy));
#ifdef KEY
// Fix for Bug 328
/*
          AUX_ID lhs_aux_id = WN_aux(expr);
          ST *lhs_st = Opt_stab()->Aux_stab_entry(lhs_aux_id)->St();
	  if (WOPT_Enable_Unique_Pt_Vsym && ST_Has_Dope_Vector(lhs_st)){
	    for (INT i = 1; i <= _base_id_map.Lastidx(); i++) {
	      ALIAS_CLASS_REP* class_rep = _base_id_map[i]->Lda_class().Class_pointed_to();
	      if (class_rep != NULL && class_rep->Pending_rep_match(ref))
		ref->Add_pending(class_rep, *this);		
	    }
	  }
*/
#endif
	  ref->Process_pending(*this);
	}
	if (WOPT_Enable_Verbose && Tracing()) {
	  fprintf(TFile, " --- After analyzing expression ");
	  Dump_wn(TFile, expr);
	  fprintf(TFile, "     ref is ");
	  ref->Print(TFile, Global_class());
	  Print(TFile);
#if Is_True_On
	  print_table();
#endif
	}
	return AC_PTR_OBJ_PAIR(ref, ref->Class_pointed_to());
      }
    default:
      // Return both components NULL -- the given expression can't be
      // a pointer.
      FmtAssert(!expr_must_point,
		("ALIAS_CLASSIFICATION::Classify_deref_of_expr: "
		 "non-pointer expression must point"));
      return AC_PTR_OBJ_PAIR(NULL, NULL);
    }
  }
  // PARM by reference gets the alias class corresponding to
  // dereferencing its kid, but we return the class for the kid of
  // PARM, since we want our return value to describe all the
  // information available to the callee.
  else if (OPCODE_is_load(opc) ||
 	   (opr == OPR_PARM && WN_Parm_Dereference(expr)) ||
	   Is_fortran_reference_parm(expr)) {
    FmtAssert(OPERATOR_is_scalar_iload (opr) ||
	      opr == OPR_MLOAD ||
	      opr == OPR_ILOADX ||
	      opr == OPR_PARM,
	      ("ALIAS_CLASSIFICATION: Indirect load: unexpected opcode"));

    // Classify the load and then record a reference to the class
    // characterizing it in its map.
    AC_PTR_OBJ_PAIR t = Classify_deref_of_expr(WN_kid0(expr), TRUE);

    ALIAS_CLASS_MEMBER *wn_member = New_alias_class_member(expr);
    if (!t.Ref_class()->Is_pointer_class()) {
      if (Tracing() && WOPT_Enable_Verbose) {
	fprintf(TFile, "Ref class is not pointer. Ref is ");
	t.Ref_class()->Print(TFile, Global_class());
      }
      // This case can happen, in spite of passing TRUE for
      // expr_must_point above. An example in which it arises is for a
      // dereference of an LDA of a non-data item (for example,
      // passing a function pointer as a FORTRAN reference
      // parameter). When that happens, we don't lose because the
      // class we're about to create won't be merged with
      // anything. Nevertheless we have to create it because
      // subsequent processing may assume it exists.
      //
      // The dereferenced kid is not a pointer yet. For now, make it a
      // pointer. Maybe do something fancy with pending sets later?
      ALIAS_CLASS_REP *wn_class = New_alias_class(wn_member);

      t.Ref_class()->Set_class_pointed_to(wn_class);
      t.Ref_class()->Process_pending(*this);
      t.Set_ref_class(t.Ref_class()->Representative()->Alias_class());
      t.Set_obj_class(t.Ref_class()->Class_pointed_to());
    }
    else {
      wn_member->Put_in_set(t.Obj_class());
    }

    WN_MAP_Set(Indir_classification_map(), expr,
	       (void *) t.Obj_class()->Representative());

    if (expr_must_point && !t.Obj_class()->Is_pointer_class()) {
      t.Obj_class()->Set_class_pointed_to(New_alias_class(New_alias_class_member()));
      t.Obj_class()->Process_pending(*this);
      t.Set_ref_class(t.Ref_class()->Representative()->Alias_class());
      t.Set_obj_class(t.Ref_class()->Class_pointed_to());
    }
    if (WOPT_Enable_Verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing expression ");
      Dump_wn(TFile, expr);
      fprintf(TFile, "     t.Obj_class() is ");
      t.Obj_class()->Print(TFile, Global_class());
      fprintf(TFile, "     t.Ref_class() is ");
      t.Ref_class()->Print(TFile, Global_class());
      Print(TFile);
#if Is_True_On
      print_table();
#endif
    }
    if (opr == OPR_PARM) {
      return t;
    }
    else {
      return AC_PTR_OBJ_PAIR(t.Obj_class(),
			     t.Obj_class()->Class_pointed_to());
    }
  }
  else if (opr == OPR_ARRAY) {
    for (INT i = 1; i < WN_kid_count(expr); i++) {
      (void) Classify_deref_of_expr(WN_kid(expr, i), FALSE);
    }
    if (WOPT_Enable_Verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing expression ");
      Dump_wn(TFile, expr);
      Print(TFile);
    }
    return Classify_deref_of_expr(WN_kid0(expr), expr_must_point);
  }
  else if (opr == OPR_ALLOCA) {
    for (INT i = 0; i < WN_kid_count(expr); i++) {
      (void) Classify_deref_of_expr(WN_kid(expr, i), FALSE);
    }
    if (WOPT_Enable_Verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing ALLOCA kids ");
      Dump_wn(TFile, expr);
      Print(TFile);
    }
    ALIAS_CLASS_MEMBER *alloca_mem_acm = New_alias_class_member();
    ALIAS_CLASS_REP    *alloca_mem_acr = New_alias_class(alloca_mem_acm);
    ALIAS_CLASS_MEMBER *return_acm = New_alias_class_member(expr);
    ALIAS_CLASS_REP    *return_acr = New_alias_class(return_acm);
    return_acr->Set_class_pointed_to(alloca_mem_acr);
    _alloca_memory_members.push_front(alloca_mem_acm);
    return AC_PTR_OBJ_PAIR(return_acr, alloca_mem_acr);
  }
  else {

    // The opcode is some sort of expression and it's not a
    // leaf. Create a class to hold the value of the expression (as if
    // it were stored in a temporary) and handle the expression kids.

    // This whole business with maintaining AC_PTR_OBJ_PAIRs has
    // gotten really ugly. -- RK 981112
    ALIAS_CLASS_MEMBER *t_member = New_alias_class_member(expr);
    (void) New_alias_class(t_member);

    BOOL expr_maybe_point = Expr_may_contain_pointer (expr);
    for (INT i = 0; i < WN_kid_count(expr); i++) {
      // Tell the recursive call that the expression need not
      // point. If it must point, we'll handle it here.
      WN * wn_kid = WN_kid(expr, i);
      OPCODE kid_opc = WN_opcode(wn_kid);
      OPERATOR kid_opr = OPCODE_operator(kid_opc);
      BOOL kid_maybe_point = Expr_may_contain_pointer(wn_kid);
      BOOL kid_must_point = FALSE;

      // AMD Bug 15176. For an address expression "U8ADD" as shown below, 
      // kid "U8U8ILOAD" gives base address, but we got a NULL object class
      // for "U8U8ILOAD" from "Classify_deref_of_expr" since we passed down 
      // a value of "FALSE" for "expr_must_point".
      // i.e,, we create an object class for a pointer expression "p", 
      // but we did not create an object class for pointer expression
      // "*p" even though "*p" itself is also a pointer.  As a result,
      // "(*p + e1)" and "(*p + e2)" have different object classes
      // even though "e1" could have the same runtime value as "e2".
      //
      //    U8U8LDID 0 <3,69,_temp___slink_sym25> T<126,anon_ptr.,8>
      //   U8U8ILOAD 40 T<52,anon_ptr.,8> T<126,anon_ptr.,8> 
      //      I4I4LDID 0 <3,24,IATM> T<4,.predef_I4,4>
      //      I4INTCONST -1 (0xffffffffffffffff)
      //     I4ADD
      //    I8I4CVT
      //    U8INTCONST 24 (0x18)
      //   U8MPY
      //  U8ADD 
      // F8F8ILOAD T<11,.predef_F8,8> T<75,anon_ptr.,8>
      //
      // As a fix, for an address expression rooted at a "ADD" or a "SUB",
      // if kid is an "ILOAD" that could be a pointer, we pass down "TRUE"
      // to "Classify_deref_of_expr" for the "ILOAD", where an object class
      // will be created for it if none exists yet.
      if (expr_must_point && kid_maybe_point
	  && (kid_opr == OPR_ILOAD)
	  && ((opr == OPR_ADD) || (opr == OPR_SUB))
	  && !Opcode_cannot_be_pointer_value(opr, opc)
	  && !Opcode_cannot_be_pointer_value(kid_opr, kid_opc))
	kid_must_point = TRUE;
 
      AC_PTR_OBJ_PAIR u = Classify_deref_of_expr(WN_kid(expr, i), kid_must_point);
      
      if (expr_maybe_point && kid_maybe_point) {
        AC_PTR_OBJ_PAIR t(t_member->Alias_class(),
			  t_member->Alias_class()->Class_pointed_to());
        Merge_conditional(t, u);
      }
    }
    AC_PTR_OBJ_PAIR t(t_member->Alias_class(),
		      t_member->Alias_class()->Class_pointed_to());

    if (expr_must_point && t.Obj_class() == NULL) {
      ALIAS_CLASS_MEMBER *dummy_acm = New_alias_class_member();
      ALIAS_CLASS_REP    *obj_acr   = New_alias_class(dummy_acm);
      t.Set_obj_class(obj_acr);
      t.Ref_class()->Set_class_pointed_to(obj_acr);
#ifdef KEY
// Fix for Bug 328
/*
      if (WOPT_Enable_Unique_Pt_Vsym){
        for (INT i = 0; i < WN_kid_count(expr); i++) {
          AC_PTR_OBJ_PAIR u = Classify_deref_of_expr(WN_kid(expr, i), FALSE);
          if (!u.Obj_class())
            u.Ref_class()->Set_class_pointed_to(obj_acr);
        }
      }
*/
#endif
      t.Ref_class()->Process_pending(*this);
    }
    if (WOPT_Enable_Verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing expression ");
      Dump_wn(TFile, expr);
      Print(TFile);
    }
    if (Opcode_cannot_be_pointer_value(opr, opc)) {
      // The opcode is not a leaf and cannot be a pointer expression.
      // Return both components NULL -- the given expression can't be
      // a pointer.
      FmtAssert(!expr_must_point,
		("ALIAS_CLASSIFICATION::Classify_deref_of_expr: "
		 "non-pointer expression must point"));
      return AC_PTR_OBJ_PAIR(NULL, NULL);
    }
    else {
      return t;
    }
  }
}


AC_PTR_OBJ_PAIR
ALIAS_CLASSIFICATION::Classify_lhs_of_store(WN *const stmt)
{
  AC_PTR_OBJ_PAIR lhs_class;
  OPERATOR opr = WN_operator(stmt);

  if (OPERATOR_is_scalar_store (opr)) {
    // The lhs reference class must exist
    lhs_class.Set_ref_class(Class_of_base_id_LDID(WN_base_id(stmt)));
    lhs_class.Set_obj_class(lhs_class.Ref_class()->Class_pointed_to());
  }
  else if (OPERATOR_is_scalar_istore (opr) || opr == OPR_MSTORE) {
    AC_PTR_OBJ_PAIR lhs_ptr_class =
      Classify_deref_of_expr(WN_kid1(stmt), TRUE);
    lhs_class.Set_ref_class(lhs_ptr_class.Obj_class());
    lhs_class.Set_obj_class(lhs_ptr_class.Obj_class()->Class_pointed_to());
    if (Tracing()) {
      fprintf(TFile, "Setting ISTORE Indir map to 0x%p\n",
	      lhs_class.Ref_class());
    }
    WN_MAP_Set(Indir_classification_map(), stmt,
	       lhs_class.Ref_class()->Representative());
  }
  else {
    FmtAssert(FALSE, ("AC::Classify_lhs_of_store: Other stores "
		      "not handled"));
  }
  return lhs_class;
}

// return TURE if the <expr> may "encode" a pointer value.
// TODO: This function is too conservative. It need to be polished. 
//
BOOL
ALIAS_CLASSIFICATION::Expr_may_contain_pointer (WN* const expr) {

  TYPE_ID res = WN_rtype (expr); 
  if (MTYPE_byte_size (res) == 0) {
    // The <res> can be TYPE_M which may contain pointer. 
    // TODO:: A better solution is to take a closer look of what in 
    //   the MTYPE_M. However, the fix is done at the last minute of 
    //   2.1 release. OTOH, some functions don't work with MTYPE_M, 
    //   say WN_object_size() may return 0 when the object of MTYPE_M. 
    //   We have to be conservative at this moment.
    return TRUE;
  }

  if (MTYPE_byte_size (res) < Pointer_Size ||
      (MTYPE_is_void (res)    || MTYPE_is_float (res) || 
       MTYPE_is_complex (res) || MTYPE_is_vector (res))) {
     return FALSE;
  }
   
  switch (WN_operator (expr)) {
  case OPR_MPY:
    return FALSE;

  case OPR_LDBITS:
  case OPR_ILDBITS:
    return WN_bit_size (expr) < Pointer_Size * 8;

  case OPR_LDID:
    {
    ST* sym = ST_of_wn (expr);
    return ST_class(sym) != CLASS_PREG && 
           TY_kind(WN_object_ty (expr)) == KIND_POINTER ||
           WN_object_size (expr) >= Pointer_Size;
    }
  case OPR_ILOAD:
    return TY_kind(WN_object_ty (expr)) == KIND_POINTER ||
           WN_object_size (expr) >= Pointer_Size;

  case OPR_NEG: case OPR_ABS:
    return Expr_may_contain_pointer (WN_kid0(expr));

  case OPR_RND: 
  case OPR_TRUNC:
  case OPR_CEIL:
  case OPR_FLOOR:
  case OPR_BNOT:
  case OPR_LNOT:
    return FALSE;
  }

  if (!WOPT_Enable_Aggressive_Alias_Classification || !Alias_Pointer_Types) {
    return TRUE;
  }

  return TRUE;
}

BOOL
ALIAS_CLASSIFICATION::Assignment_may_xfer_pointer (WN* const stmt) {

  if (!WOPT_Enable_Aggressive_Alias_Classification || !Alias_Pointer_Types) {
    return TRUE;
  }

  if (!Expr_may_contain_pointer (WN_kid0(stmt))) {
    return FALSE;
  }

  TY_IDX obj_ty = WN_object_ty (stmt);
  switch (TY_kind(obj_ty)) {
  case KIND_SCALAR:
    return WN_object_size(stmt) >= Pointer_Size; 

  case KIND_STRUCT:
    // TODO: this should be refined.

  case KIND_POINTER:
    return TRUE;

  case KIND_ARRAY:
    return TRUE;

  case KIND_VOID:
    return WN_object_size (stmt) && (WN_object_size (stmt) >= Pointer_Size);

  default:
    Is_True (FALSE, ("Unexected results returned from WN_object_ty ()"));
  }

  return TRUE;
}

WN *
ALIAS_CLASSIFICATION::Handle_assignment(WN *const stmt)
{
  AC_PTR_OBJ_PAIR lhs_class = Classify_lhs_of_store(stmt);
  // Now deal with the RHS
  WN *rhs = WN_kid0(stmt);

  ALIAS_CLASS_MEMBER *lhs_ref_member =
    lhs_class.Ref_class()->Representative();

  // Note that the following line can change things about the
  // lhs_class, and that afterward we have to recompute the
  // AC_PTR_OBJ_PAIR for the lhs_class from the lhs._ref_class
  // representative.
  TY_IDX rhs_obj_ty = WN_object_ty (rhs);
  AC_PTR_OBJ_PAIR rhs_class = Classify_deref_of_expr(rhs, 
#if defined(TARG_SL)
  // used to decide if p = 0xhardaddr is a pointer (embedded)
			         TY_kind(WN_object_ty(stmt))==KIND_POINTER ||
#endif
                                 TY_kind(rhs_obj_ty) == KIND_POINTER);

  lhs_class.
    Set_ref_class(lhs_ref_member->Alias_class());
  lhs_class.
    Set_obj_class(lhs_ref_member->Alias_class()->Class_pointed_to());

  Is_True(lhs_class.Ref_class()->Class_pointed_to() == 
	  lhs_class.Obj_class(),
	  ("ALIAS_CLASSIFICATION::Handle_assignment: "
	   "RHS classification changed LHS points-to relation"));

  if (WN_operator(stmt) == OPR_MSTORE) {
    (void) Classify_deref_of_expr(WN_kid2(stmt), FALSE);
  }

  if (rhs_class.Ref_class() != NULL && 
      Assignment_may_xfer_pointer (stmt)) {
    // Conditional join
    Merge_conditional(lhs_class, rhs_class);
  }
  else {
    // RHS is non-pointer constant
  }

  if (WOPT_Enable_Verbose && Tracing()) {
    fprintf(TFile, "  after handling assignment:\n");
#if Is_True_On
    print_table();
#endif
    if (OPERATOR_is_scalar_istore (WN_operator(stmt)) ||
	WN_operator(stmt) == OPR_MSTORE) {
      fprintf(TFile, "    (M/I)STORE placed in ");
      ((ALIAS_CLASS_MEMBER *) WN_MAP_Get(Indir_classification_map(),
					 stmt))->Alias_class()->
					   Print(TFile, Global_class());
    }
    else if (OPERATOR_is_scalar_store (WN_operator(stmt))) {
      fprintf(TFile, "    %s placed in ", WN_operator(stmt) == OPR_STID ?
	      "STID" : "STBITS");
      Class_of_base_id_LDID(WN_base_id(stmt))->Print(TFile, Global_class());
    }
    Print(TFile);
  }

  return WN_next(stmt);
}


// ======================================================================


BOOL
ALIAS_CLASSIFICATION::Stmt_stores_return_value(const WN *const stmt)
{
  WN *rhs = WN_kid0(stmt);

  return (OPCODE_is_store(WN_opcode(stmt)) &&
	  (WN_operator(rhs) == OPR_LDID) &&
	  (ST_sclass(ST_of_wn(rhs)) == SCLASS_REG) &&
	  Preg_Is_Dedicated(WN_offset(rhs)));
}


BOOL
ALIAS_CLASSIFICATION::Uses_no_return_value(const WN *const stmt)
{
  return TRUE;
}


BOOL
ALIAS_CLASSIFICATION::Callee_changes_no_points_to(const WN *const call_wn,
						  const WN *const parm_wn)
{
#if defined(TARG_SL)
  if(WN_operator(call_wn) == OPR_INTRINSIC_CALL) {
    if(INTRN_has_no_side_effects(WN_intrinsic(call_wn)))
      return TRUE;
  }
#endif
  if (WN_Call_Never_Return(call_wn)) {
    return TRUE;
  }
  else if (WN_Call_Does_Mem_Free(call_wn)) {
    return TRUE;
  }
  else if ((WN_operator(call_wn) == OPR_CALL) &&
	   (strcmp("free", ST_name(WN_st(call_wn))) == 0)) {
    return TRUE;
  }
  else if (Callee_returns_new_memory(call_wn)) {
    return TRUE;
  }
  else if (WOPT_Enable_Alias_Class_Fortran_Rule &&
	   WN_Call_Fortran_Pointer_Rule(call_wn)) {
    const TY_IDX ty = WN_ty(parm_wn);
    if ((TY_kind(ty) == KIND_POINTER) &&
	(TY_kind(TY_pointed(ty)) != KIND_POINTER) &&	// Not a Cray pointer
	!TY_is_f90_pointer(ty)) {			// Not an f90 pointer
      const WN *const parm_kid = WN_kid0(parm_wn);
      if ((WN_operator(parm_kid) == OPR_LDA) ||
	  (WN_operator(parm_kid) == OPR_LDMA)) {
	const ST *const st = ST_of_wn(parm_kid);
	if (!ST_is_f90_target(st)) {
	  return TRUE;
	}
      }
      else {
	return TRUE;
      }
    }
  }
  return FALSE;
}


BOOL
ALIAS_CLASSIFICATION::WN_is_alloca_intrinsic(const WN *const call_wn)
{
  if ((WN_operator(call_wn) == OPR_INTRINSIC_CALL) &&
      ((WN_intrinsic(call_wn) == INTRN_U4I4ALLOCA) ||
       (WN_intrinsic(call_wn) == INTRN_U8I8ALLOCA) ||
       (WN_intrinsic(call_wn) == INTRN_F90_STACKTEMPALLOC))) {
    return TRUE;
  }
  else {
#ifdef KEY
    return Callee_returns_new_memory(call_wn);
#else
    return FALSE;
#endif
  }
}


BOOL
ALIAS_CLASSIFICATION::Callee_returns_new_memory(const WN *const call_wn)
{
#ifndef KEY
  return WN_Call_Does_Mem_Alloc(call_wn);
#else
  if (WN_Call_Does_Mem_Alloc(call_wn))
    return TRUE;
  if (WN_operator(call_wn) == OPR_CALL) {
    const ST *const st = WN_st(call_wn);

    // Cheap hack for now, to test performance. This should be based on
    // some real mechanism in the future instead of cheesebag hacks.
    if ((strcmp("malloc", ST_name(st)) == 0) ||
	(strcmp("alloca", ST_name(st)) == 0) ||
	(strcmp("calloc", ST_name(st)) == 0) ||
	(strcmp("_F90_ALLOCATE", ST_name(st)) == 0) ||
        WOPT_Enable_Disambiguate_Heap_Obj && PU_has_attr_malloc (Pu_Table[ST_pu(st)])) {
      return TRUE;
    }
  }
  else if (WN_operator(call_wn) == OPR_INTRINSIC_CALL) {
    if ((WN_intrinsic(call_wn) == INTRN_U4I4ALLOCA) ||
	(WN_intrinsic(call_wn) == INTRN_U8I8ALLOCA) ||
	(WN_intrinsic(call_wn) == INTRN_U4I4MALLOC) ||
	(WN_intrinsic(call_wn) == INTRN_U8I8MALLOC)) {
      return TRUE;
    }
  }
  return FALSE;
#endif
}


BOOL
ALIAS_CLASSIFICATION::May_icall_nested_PU(const WN *const  call_wn,
					  ST       *      *callee_st)
{
  if (PU_uplevel(Get_Current_PU())) {
    // If we had more smarts, we would set *callee_st to NULL only if
    // we couldn't narrow down the set of potential callees to a
    // singleton. NULL means we don't know which nested PU is
    // called. If we knew which one it was, we would set *callee_st to
    // the ST for the one callee.
    *callee_st = NULL;
    return TRUE;
  }
  else {
    return FALSE;
  }
}


struct MERGE_NEST_REF_CLASSES {
  ALIAS_CLASSIFICATION *_ac;
  AC_PTR_OBJ_PAIR       _globals;

  MERGE_NEST_REF_CLASSES(ALIAS_CLASSIFICATION *ac,
			 AC_PTR_OBJ_PAIR       globals) :
    _ac(ac), _globals(globals)
      { }

  void operator() (UINT32, const ST *st) const {
    IDTYPE base_id;
    if (ST_sym_class(st) == CLASS_VAR && ST_has_nested_ref(st)) {
      base_id = _ac->Base_id(ST_base(st), ST_ofst(st), (TY_IDX) 0);

      AC_PTR_OBJ_PAIR
	nested_ref_var_class(_ac->Class_of_base_id_LDA(base_id),
			     _ac->Class_of_base_id_LDID(base_id));
      _ac->Merge_conditional(_globals, nested_ref_var_class);
    }
  }
};


void
ALIAS_CLASSIFICATION::Handle_call_of_nested_PU(ST *const callee_st)
{
  if (Tracing()) {
    fprintf(TFile, "Handling nested call of %s", callee_st ? "" : "(NULL)");
    if (callee_st) Print_ST(TFile, callee_st, FALSE);
    fflush(TFile);
  }

  WN *pragma_list = (callee_st != NULL ?
		     Get_MP_accessed_id_list(callee_st) :
		     NULL);

  if (pragma_list != NULL && WOPT_Enable_MP_varref) {
    // For each base ID, check to see how the base ID may be affected by
    // a call to the nested PU.
    //
    // Things read by the nested PU may be stored to things written by
    // the nested PU. Things whose address is taken by the nested PU
    // (i.e., things "indirectly read" by the nested PU) may be pointed
    // to by things written by the nested PU. The set of things written
    // by the nested PU includes all classes pointed to by things
    // indirectly written through by the nested PU.
    //
    // For each class directly read
    //   For each class written
    for (WN *wn_read = WN_first(pragma_list);
	 wn_read != NULL;
	 wn_read = WN_next(wn_read)) {
      ST *read_st = WN_st(wn_read);
      if (read_st != NULL &&
	  (WN_pragma_arg2(wn_read) & ACCESSED_LOAD)) {
	if (Tracing()) {
	  fprintf(TFile, "Read (%s%s) by nested callee : ",
		  (WN_pragma_arg2(wn_read) & ACCESSED_LOAD ? "d" : ""),
		  (WN_pragma_arg2(wn_read) & ACCESSED_ILOAD ? "i" : ""));
	  Print_ST(TFile, read_st, FALSE);
	  fflush(TFile);
	}

	ST    *read_base_st;
	INT64  read_base_ofst;
	Expand_ST_into_base_and_ofst(read_st, (INT64) 0,
				     &read_base_st, &read_base_ofst);

	IDTYPE read_base_id = Base_id(read_base_st, read_base_ofst,
				      (TY_IDX) 0);

	ALIAS_CLASS_MEMBER *read_member =
	  Class_of_base_id_LDID(read_base_id)->Representative();

	for (WN *wn_written = WN_first(pragma_list);
	     wn_written != NULL;
	     wn_written = WN_next(wn_written)) {
	  Is_True(WN_pragma(wn_written) == WN_PRAGMA_ACCESSED_ID,
		  ("ALIAS_CLASSIFICATION::Handle_call_of_nested_PU: "
		   "pragma must be WN_PRAGMA_ACCESSED_ID."));
	  ST *written_st = WN_st(wn_written);
	  if (written_st != NULL &&
	      (WN_pragma_arg2(wn_written) &
	       (ACCESSED_STORE | ACCESSED_ISTORE))) {
	    if (Tracing()) {
	      fprintf(TFile, "Written (%s%s) by nested callee : ",
		      (WN_pragma_arg2(wn_written) & ACCESSED_STORE ?
		       "d" : ""),
		      (WN_pragma_arg2(wn_written) & ACCESSED_ISTORE ?
		       "i" : ""));
	      Print_ST(TFile, written_st, FALSE);
	      fflush(TFile);
	    }

	    ST    *written_base_st;
	    INT64  written_base_ofst;
	    Expand_ST_into_base_and_ofst(written_st, (INT64) 0,
					 &written_base_st,
					 &written_base_ofst);
	    IDTYPE written_base_id = Base_id(written_base_st,
					     written_base_ofst,
					     (TY_IDX) 0);

	    if ((WN_pragma_arg2(wn_written) & ACCESSED_STORE) &&
		(WN_pragma_arg2(wn_read) & ACCESSED_LOAD) &&
		read_base_id != written_base_id) {
	      // Direct read and direct write.
	      ALIAS_CLASS_REP *read_rep = read_member->Alias_class();

	      AC_PTR_OBJ_PAIR read_class(read_rep,
					 read_rep->Class_pointed_to());

	      ALIAS_CLASS_REP *written_rep =
		Class_of_base_id_LDID(written_base_id);

	      AC_PTR_OBJ_PAIR written_class(written_rep,
					    written_rep->Class_pointed_to());

	      // Now join the class pointed to by the written thing with
	      // the class pointed to by the read thing.
	      Merge_conditional(written_class, read_class);
	    }
	    if ((WN_pragma_arg2(wn_written) & ACCESSED_STORE) &&
		(WN_pragma_arg2(wn_read) & ACCESSED_ILOAD)) {
	      // Indirect read and direct write.

	      ALIAS_CLASS_REP *read_rep = read_member->Alias_class();
	      if (!read_rep->Is_pointer_class()) {
		// Make *read_rep a pointer class since it is
		// dereferenced in the nested callee.
		read_rep->Set_class_pointed_to(New_alias_class(New_alias_class_member()));
		read_rep->Process_pending(*this);
	      }
	      read_rep = read_rep->Class_pointed_to();

	      AC_PTR_OBJ_PAIR read_class(read_rep,
					 read_rep->Class_pointed_to());

	      ALIAS_CLASS_REP *written_rep =
		Class_of_base_id_LDID(written_base_id);

	      AC_PTR_OBJ_PAIR written_class(written_rep,
					    written_rep->Class_pointed_to());

	      // Now join the class pointed to by the written thing with
	      // the class pointed to by the read thing.
	      Merge_conditional(written_class, read_class);
	    }
	    if ((WN_pragma_arg2(wn_written) & ACCESSED_ISTORE) &&
		(WN_pragma_arg2(wn_read) & ACCESSED_LOAD)) {
	      // Direct read and indirect write.

	      ALIAS_CLASS_REP *read_rep = read_member->Alias_class();

	      ALIAS_CLASS_REP *written_rep =
		Class_of_base_id_LDID(written_base_id);

	      if (!written_rep->Is_pointer_class()) {
		// Make *written_rep a pointer class since it is
		// stored through in the nested callee.
		written_rep->Set_class_pointed_to(New_alias_class(New_alias_class_member()));
		written_rep->Process_pending(*this);
	      }
	      written_rep = written_rep->Class_pointed_to();

	      AC_PTR_OBJ_PAIR read_class(read_rep,
					 read_rep->Class_pointed_to());
	      AC_PTR_OBJ_PAIR written_class(written_rep,
					    written_rep->Class_pointed_to());

	      // Now join the class pointed to by the written thing with
	      // the class pointed to by the read thing.
	      Merge_conditional(written_class, read_class);
	    }
	    if ((WN_pragma_arg2(wn_written) & ACCESSED_ISTORE) &&
		(WN_pragma_arg2(wn_read) & ACCESSED_ILOAD) &&
		read_base_id != written_base_id) {
	      // Indirect read and indirect write.

	      ALIAS_CLASS_REP *read_rep = read_member->Alias_class();
	      if (!read_rep->Is_pointer_class()) {
		// Make *read_rep a pointer class since it is
		// dereferenced in the nested callee.
		read_rep->Set_class_pointed_to(New_alias_class(New_alias_class_member()));
		read_rep->Process_pending(*this);
	      }
	      read_rep = read_rep->Class_pointed_to();

	      AC_PTR_OBJ_PAIR read_class(read_rep,
					 read_rep->Class_pointed_to());

	      ALIAS_CLASS_REP *written_rep =
		Class_of_base_id_LDID(written_base_id);

	      if (!written_rep->Is_pointer_class()) {
		// Make *written_rep a pointer class since it is
		// stored through in the nested callee.
		written_rep->Set_class_pointed_to(New_alias_class(New_alias_class_member()));
		written_rep->Process_pending(*this);
	      }
	      written_rep = written_rep->Class_pointed_to();

	      AC_PTR_OBJ_PAIR written_class(written_rep,
					    written_rep->Class_pointed_to());

	      // Now join the class pointed to by the written thing with
	      // the class pointed to by the read thing.
	      Merge_conditional(written_class, read_class);
	    }
	  }
	}
      }
    }
  }
  else {
    // No MP mod/ref pragma list available. There is nothing we can do
    // except assume the nested PU makes a global point to everything
    // it examines, and makes everything it touches point to
    // everything it examines. Don't do this walk over the symbol
    // table twice, since the collapsing we do here is idempotent.
    if (!Collapsed_nested_references()) {
      AC_PTR_OBJ_PAIR globals(Global_class(), Global_class());

      ST *st;

      For_all(*Scope_tab[CURRENT_SYMTAB].st_tab,
	      MERGE_NEST_REF_CLASSES(this, globals));

      Set_collapsed_nested_references();
    }
  }
}


// We handle two kinds of calls to nested procedures. The first is the
// obvious one in which we see a direct call to the nested PU. The
// second is a call to some other routine that takes an LDA of a
// nested PU as a parameter. WARNING: As in the above paragraph, we
// don't handle LDA's of nested PU's sneaking into parameters in
// arbitrary ways. They have to be exposed as PARM(LDA).

WN *
ALIAS_CLASSIFICATION::Handle_call(WN *call_wn)
{
  OPCODE    opc     = WN_opcode(call_wn);
  OPERATOR  opr     = OPCODE_operator(opc);

  Is_True(opr == OPR_CALL ||
	  opr == OPR_ICALL ||
	  opr == OPR_INTRINSIC_CALL,
	  ("AC::Handle_call: Can handle only calls"));

  if (opr == OPR_CALL) {
    ST *callee_st = WN_st(call_wn);
    if (PU_is_nested_func(Pu_Table[ST_pu(*callee_st)])) {
      // This is a direct call to a nested PU.
      Handle_call_of_nested_PU(callee_st);
    }
  }

  // For each parameter, merge the class containing (what) globals
  // (point to) with the class pointed to by the parameter, since the
  // called routine can store any parameter into a global.

  UINT  n_parms;

  if (opr == OPR_ICALL) {
    n_parms = WN_kid_count(call_wn) - 1;
    (void) Classify_deref_of_expr(WN_kid(call_wn,
					 WN_kid_count(call_wn) - 1),
				  FALSE);

    // If this ICALL may call a nested PU, take that into account.
    ST *callee_st;
    if (May_icall_nested_PU(call_wn, &callee_st)) {
      Handle_call_of_nested_PU(callee_st);
    }
  }
  else {
    n_parms = WN_kid_count(call_wn);
  }

  WN   *parm_wn;

#if defined(TARG_SL)
  if (WN_operator(call_wn)==OPR_INTRINSIC_CALL && 
      INTRN_copy_addr(WN_intrinsic(call_wn))) {
    // some intrinsic_call exist just for easy code generation, 
    // its works like copying the pointer in parameter to lhs
    if (WN_kid_count(call_wn)>1)
      DevWarn("Handle_call: intrinsic_call which copy addr has more than 1 parms");
    WN *rhs = WN_kid0(WN_kid0(call_wn));
    TY_IDX rhs_obj_ty = WN_object_ty (rhs);
    AC_PTR_OBJ_PAIR rhs_class = Classify_deref_of_expr(rhs, 
        					TRUE);

    //deal with lhs_class like handle_assignment
    WN* stmt = WN_next(call_wn);
    if (stmt != NULL && Stmt_stores_return_value(stmt)) {
      AC_PTR_OBJ_PAIR lhs_class = Classify_lhs_of_store(stmt);
  
      ALIAS_CLASS_MEMBER *lhs_ref_member =
                             lhs_class.Ref_class()->Representative();

      // Note that the following line can change things about the
      // lhs_class, and that afterward we have to recompute the
      // AC_PTR_OBJ_PAIR for the lhs_class from the lhs._ref_class
      // representative.

      lhs_class.Set_ref_class(lhs_ref_member->Alias_class());
      lhs_class.Set_obj_class(lhs_ref_member->Alias_class()->Class_pointed_to());

      Is_True(lhs_class.Ref_class()->Class_pointed_to() == 
             	      lhs_class.Obj_class(),
	          ("ALIAS_CLASSIFICATION::Handle_call:"
  	  	   "dealing with intrinsic_copy_addr, RHS classification changed LHS points-to relation"));

      if (rhs_class.Ref_class() != NULL && 
          Assignment_may_xfer_pointer (stmt)) {
        // Conditional join
        Merge_conditional(lhs_class, rhs_class);
      }

      stmt = WN_next(stmt);  
      FmtAssert(!(stmt != NULL && Stmt_stores_return_value(stmt)), 
                 ("Handle_call: multiple stmt store return value after call"));
    }

    // dump trace and return	
    if (WOPT_Enable_Verbose && Tracing()) {
      fprintf(TFile, "  after handling call:\n");
      Print(TFile);
    }
    return stmt;  
  }
#endif

  for (UINT i = 0; i < n_parms; ++i) {
    parm_wn = WN_kid(call_wn, i);
    Is_True(WN_operator(parm_wn) == OPR_PARM,
	    ("AC::Handle_call: Can't handle non-PARM parameter"));
    WN *actual = WN_kid0(parm_wn);
    if ((WN_operator(actual) == OPR_LDA) ||
	(WN_operator(actual) == OPR_LDMA)) {
      ST *lda_st = ST_of_wn(actual);
      if (ST_class(lda_st) == CLASS_FUNC &&
	  PU_is_nested_func(Pu_Table[ST_pu(lda_st)])) {
	// This PARM is an LDA of a nested PU. We consider it to be a
	// call to the nested PU.
	Handle_call_of_nested_PU(lda_st);
      }
    }
    AC_PTR_OBJ_PAIR rhs_class = Classify_deref_of_expr(parm_wn,
                                 TY_kind (WN_ty(parm_wn)) == KIND_POINTER);

    // There are really three levels of evil we can handle for each
    // parameter:
    // 1: Minimum evil: No points-to relationship can be changed by
    //    the callee through this parameter. In this case, no change
    //    in our storage-shape graph is needed for this parameter.
    // 2: Moderate evil: Arbitrary pointers can be saved through the
    //    parameter, but no address passed as the parameter value can
    //    be saved by the callee. In this case, **p's class must be
    //    merged with the global class, where p is the value of the
    //    parameter passed.
    // 3: Maximum evil: Arbitrary behavior allowed on the part of the
    //    callee as regards this parameter. In this case, *p's class
    //    must be merged with the global class, where p is the value
    //    of the parameter passed.
    //
    // TODO: In the current implementation, we really handle only
    // cases 1 and 3. If a parameter doesn't fall into case 1, we
    // treat it as case 3 even though it might really be case 2.
    // WN_Parm_Passed_Not_Saved is enough to establish case 2, so
    // perhaps we really should handle it properly.

    if (!Callee_changes_no_points_to(call_wn, parm_wn)) {
      AC_PTR_OBJ_PAIR lhs_class(Global_class(), Global_class());
      Merge_conditional(lhs_class, rhs_class);
    }
    else if (rhs_class.Ref_class() != NULL) {
      // Objects pointed to by this argument may be altered by the
      // call, but the call cannot change any visible points-to
      // relationship through this argument.
      Add_to_altered_non_points_to_parms(rhs_class.Ref_class());
    }
  }

  WN *stmt;

  stmt = WN_next(call_wn);
  while (stmt != NULL && Stmt_stores_return_value(stmt)) {
    if (Tracing()) {
      fprintf(TFile, "Store of return value:\n");
      Dump_wn_tree(TFile, stmt);
    }
    AC_PTR_OBJ_PAIR lhs_class = Classify_lhs_of_store(stmt);
    if (!Callee_returns_new_memory(call_wn)) {
      // Any rvalue involving the return value can be (point to) a
      // global or item pointed to by one of the parameters to the
      // call. Therefore the appropriate action is to make the lhs of
      // the store statement point to the global class.
      Merge_conditional(lhs_class,
			AC_PTR_OBJ_PAIR(Global_class(),
					Global_class()));
    }
    else {
      // Callee is like malloc(). The only way of pointing to the memory
      // it returns is via the return value. If the place where the
      // return value gets stored isn't a pointer already, make it
      // one. Making the new class a pointer is the fix for 633362.
      if (!lhs_class.Ref_class()->Is_pointer_class()) {
	ALIAS_CLASS_MEMBER *return_acm = New_alias_class_member();
	ALIAS_CLASS_REP    *return_acr = New_alias_class(return_acm);
	lhs_class.Ref_class()->Set_class_pointed_to(return_acr);
	lhs_class.Ref_class()->Process_pending(*this);
	lhs_class.Set_obj_class(return_acr);
      }
      if (WN_is_alloca_intrinsic(call_wn)) {
	_alloca_memory_members.
	  push_front(lhs_class.Obj_class()->Representative());
      }
    }
    stmt = WN_next(stmt);
  }
  Is_True(Uses_no_return_value(stmt),
	  ("ALIAS_CLASSIFICATION: General use of return value illegal"));

  if (WOPT_Enable_Verbose && Tracing()) {
    fprintf(TFile, "  after handling call:\n");
    Print(TFile);
  }
  return stmt;
}


// ======================================================================
//
// Classify_wn_and_kids invoked by Classify_memops
//
// ======================================================================


WN *
ALIAS_CLASSIFICATION::Classify_wn_and_kids(WN *const wn)
{
  OPCODE opc = WN_opcode(wn);

#if Is_True_On
  if (opc == OPC_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL, ("ALIAS_CLASSIFICATION::Classify_wn_and_kids: "
			  "rid must not be NULL"));
    // If the destination for our information is the alias manager, we
    // will already have emitted regions and raised their level to the
    // current Opt_stab()->Rgn_level().
    FmtAssert((RID_level(rid) <
	       (Opt_stab()->Rgn_level() +
		(_destination == AC_DESTINATION_ALIAS_MANAGER))),
	      ("ALIAS_CLASSIFICATION: Cannot perform alias classification "
	       "in the presence of black-box regions"));
  }
#endif

  if (opc == OPC_BLOCK) {
    for (WN *wn2 = WN_first(wn); wn2 != NULL; ) {
      wn2 = Classify_wn_and_kids(wn2);
    }
    return NULL;
  }
  else if (OPCODE_is_store(opc)) {
    if (WOPT_Enable_Verbose && Tracing()) {
      fprintf(TFile, "cwnk: Handling assignment:\n");
      Dump_wn_tree(TFile, wn);
    }
    return Handle_assignment(wn);
  }
  else if (OPCODE_is_call(opc)) {
    if (WOPT_Enable_Verbose && Tracing()) {
      fprintf(TFile, "cwnk: Handling call:\n");
      Dump_wn_tree(TFile, wn);
    }
    return Handle_call(wn);
  }
  else if (OPCODE_is_expression(opc)) {
    // wn is an expression kid of SCF and needs to be handled like the
    // RHS of an assignment statement.
    (void) Classify_deref_of_expr(wn, FALSE);
    return NULL;
  }
  else if (opc == OPC_IO) {
    Warn_todo("ALIAS_CLASSIFICATION: Handle %s", OPCODE_name(opc));
    return WN_next(wn);
  }
  else if ( ! OPCODE_is_black_box( opc )) {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      WN *kid_i = WN_kid(wn, i);
      Classify_wn_and_kids(kid_i);
    }
    return WN_next(wn);
  }
  else {
    return WN_next(wn);
  }
}


// ======================================================================


void
ALIAS_CLASSIFICATION::Finalize_ac_map_wn(WN *wn)
{
  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  if (Tracing()) {
    fprintf(TFile, "Finalizing : ");
    Dump_wn(TFile, wn);
    fflush(TFile);
  }

  if (OPCODE_is_call(opc)) {
    // Do call stuff
  }
  else if (OPCODE_is_load(opc) ||
	   OPCODE_is_store(opc) ||
	   (opr==OPR_PARM && WN_Parm_Dereference(wn)) ||
	   Is_fortran_reference_parm(wn) ||
	   ((opr == OPR_LDA || opr == OPR_LDMA) &&
	    Is_LDA_of_variable(wn))) {
    if (Tracing()) {
      fprintf(TFile, "   placed in ");
    }
    if (OPERATOR_is_scalar_load (opr) ||
	OPERATOR_is_scalar_store (opr) ||
	opr == OPR_LDA  || opr == OPR_LDMA) {
      // Direct memop; translation through base_id.
      ALIAS_CLASS_REP *acr = Class_of_base_id_LDID(WN_base_id(wn));
      IDTYPE class_id = acr->Id();

      WN_MAP32_Set(Memop_classification_map(), wn, class_id);
      if (_ac_id_to_acr_map.Lookup(class_id) == NULL) {
	_ac_id_to_acr_map.Insert(class_id, acr);
      }

      if (Tracing()) {
	Class_of_base_id_LDID(WN_base_id(wn))->Print(TFile, Global_class());
      }

      if (_destination == AC_DESTINATION_OPT_STAB) {
	// Maybe we should put information into the POINTS_TO in
	// OPT_STAB::Transfer_alias_class_to_occ_and_aux(RID *, WN *)
	// instead of here because we can't do the analogous thing for
	// indirects here, since the memop ==> POINTS_TO
	// correspondence isn't set up for indirects yet. Furthermore,
	// if we do the transfer here, why bother setting the
	// Memop_classification_map() for these direct memops?
	POINTS_TO *pt = Opt_stab()->Aux_stab_entry(WN_aux(wn))->Points_to();

	if (pt->Alias_class() == OPTIMISTIC_AC_ID) {
	  pt->Set_alias_class(class_id);
	}
	else {
	  Is_True(((WOPT_Alias_Class_Limit != UINT32_MAX) &&
		   (pt->Alias_class() == PESSIMISTIC_AC_ID)) ||
		  (pt->Alias_class() == class_id),
		  ("ALIAS_CLASSIFICATION: Inconsistent alias class "
		   "for base ID %u, aux ID %u", WN_base_id(wn), WN_aux(wn)));
	  // The following tests the same condition as the Is_True
	  // just above; it is for the production compiler only.
	  if ((pt->Alias_class() != class_id) &&
	      (pt->Alias_class() != PESSIMISTIC_AC_ID)) {
	    // This should never happen, but in case it does, recover
	    // gracefully with no information.
	    DevWarn("ALIAS_CLASSIFICATION: Inconsistent alias class "
		    "for base ID %u, aux ID %u", WN_base_id(wn), WN_aux(wn));
	    pt->Set_alias_class(PESSIMISTIC_AC_ID);
	  }
	}
      }
    }
    else {
      // Indirect memop. Translation through existing map entry that
      // gives the alias class member corresponding to the WN.
      ALIAS_CLASS_MEMBER *acm =
	(ALIAS_CLASS_MEMBER *) WN_MAP_Get(Indir_classification_map(), wn);
      ALIAS_CLASS_REP    *acr = acm->Alias_class();

      if (Tracing()) {
	acr->Print(TFile, Global_class());
      }

      IDTYPE class_id = acr->Id();
      WN_MAP32_Set(Memop_classification_map(), wn, class_id);
      if (_ac_id_to_acr_map.Lookup(class_id) == NULL) {
	_ac_id_to_acr_map.Insert(class_id, acr);
      }
    }
  }
}


void
ALIAS_CLASSIFICATION::Finalize_ac_map(WN *const wn)
{
  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

#if Is_True_On
  if (opc == OPC_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL, ("ALIAS_CLASSIFICATION::Finalize_ac_map: "
			  "rid must not be NULL"));
    // If the destination for our information is the alias manager, we
    // will already have emitted regions and raised their level to the
    // current Opt_stab()->Rgn_level().
    FmtAssert((RID_level(rid) <
	       (Opt_stab()->Rgn_level() +
		(_destination == AC_DESTINATION_ALIAS_MANAGER))),
	      ("ALIAS_CLASSIFICATION: Cannot perform alias classification "
	       "in the presence of black-box regions"));
  }
#endif

  if (opc == OPC_BLOCK) {
    for (WN *wn2 = WN_first(wn); wn2 != NULL; wn2 = WN_next(wn2)) {
      Finalize_ac_map(wn2);
    }
  }
  else if (!OPCODE_is_black_box(opc)) {
    INT rhs_idx;
    if (OPCODE_is_store(opc)) {
      rhs_idx = 0;
    }
    else {
      rhs_idx = -1;
    }
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      // Finalize all kids except the RHS of a store of a return value
      // from a callee.
      if (!OPCODE_is_store(opc) ||
	  !Stmt_stores_return_value(wn) ||
	  i != rhs_idx) {
	Finalize_ac_map(WN_kid(wn, i));
      }
    }
    Finalize_ac_map_wn(wn);
  }
}


// ======================================================================
// Public interface routines for clients.
// ======================================================================


void
ALIAS_CLASSIFICATION::Classify_memops(WN *entry_wn)
{
#if Is_True_On
  next_acm_idx = 0;
#endif

  // Set up the ID mapping from aux ID to base ID for PREGs.
  Preg_num_base_id_map().Init();
  _st_idx_to_base_id_map.Init();

  if (Tracing()) {
    fprintf(TFile, "%sBeginning alias classification for %s\n%s",
	    DBar,
	    (_destination == AC_DESTINATION_OPT_STAB ?
	     "OPT_STAB" :
	     "alias manager"),
	    DBar);
    Dump_wn_tree(TFile, entry_wn);
    if (_destination == AC_DESTINATION_OPT_STAB) {
      Opt_stab()->Print(TFile);
    }
  }

  if (Tracing()) {
    fprintf(TFile, "----------\nBefore program analysis:\n");
    Print(TFile);
    fprintf(TFile, "----------\n");
  }

  _memop_classification_map = WN_MAP32_Create(Pool());
  _indir_classification_map = WN_MAP_Create(Pool());

  if (Tracing()) {
    fprintf(TFile, "indir map is %u\n", _indir_classification_map);
  }

  Classify_wn_and_kids(entry_wn);

  // Assert that the structure is frozen now; no new classes can be
  // created. This is only a check for debugging.
  ALIAS_CLASS_REP::_structure_not_frozen = FALSE;

  if (Tracing()) {
    fprintf(TFile, "----------\nAfter program analysis:\n");
    Print(TFile);
    fprintf(TFile, "----------\n");
  }

  if (_destination == AC_DESTINATION_OPT_STAB) {
    // The current implementation of alias classification has the
    // property that since all globals begin in the same class, that
    // class must contain all variables that can be altered by any
    // procedure call, except for those that may be altered only
    // through calls to memory allocation/deallocation
    // routines. Therefore, we trim back the
    // Opt_stab()->Call_by_value() and Opt_stab()->Call_by_reference()
    // sets to exclude anything not in the Global_class() and not in
    // any of the classes represented by the
    // Altered_non_points_to_parms() set. The following loop builds a
    // bit vector to facilitate this process; the transfer of
    // information to the OPT_STAB actually happens in
    // Collect_ST_Attr_Reset_temp().

    // First, mark the set of alias classes that might be altered by
    // call sites as a result of their inclusion in
    // Altered_non_points_to_parms(). Note that this information could
    // be accumulated on the fly with some slightly more complicated
    // logic. I choose to do it somewhat less efficiently here for
    // easier debugging.
    ALIAS_CLASS_MEMBER_LIST::iterator p;
    for (p = Altered_non_points_to_parms().begin();
	 p != Altered_non_points_to_parms().end();
	 ++p) {
      // Skip one level because the call site can't alter the argument
      // value passed; only dereferences of that argument can be
      // altered.
      for (ALIAS_CLASS_REP *acr = (*p)->Alias_class()->Class_pointed_to();
	   acr != NULL && !acr->Writable_by_call();
	   acr = acr->Class_pointed_to()) {
	acr->Set_writable_by_call();
      }
    }

    for (p = _alloca_memory_members.begin();
	 p != _alloca_memory_members.end();
	 ++p) {
      (*p)->Alias_class()->Set_alloca_class();
    }

    // Now mark the global class, which might be altered by any call
    // just because it's global.
    Global_class()->Set_writable_by_call();

    // Now go through all the aux_id's currently in use and decide for
    // each one whether it belongs in inaccessible_to_callees or not.
    AUX_STAB_ITER  aux_stab_iter(Opt_stab());
    AUX_ID         aux_id;
    BS            *inaccessible_to_callees;

    inaccessible_to_callees = BS_Create_Empty(Opt_stab()->Lastidx(),
					      Pool());

    FOR_ALL_NODE(aux_id, aux_stab_iter, Init()) {
      // Class can only be found for aux_id's with non-NULL ST; return
      // vsym and other such things have NULL ST and can't be
      // classified.
      AUX_STAB_ENTRY *aux_stab_entry = Opt_stab()->Aux_stab_entry(aux_id);
      if (aux_stab_entry->St() != NULL &&
	  !aux_stab_entry->Is_dedicated_preg() &&
	  // Class_of_base_id_LDID can be NULL for functions and other
	  // non-data items.
	  Class_of_base_id_LDID(Base_id(aux_id, (TY_IDX) 0)) != NULL &&
	  !Class_of_base_id_LDID(Base_id(aux_id, (TY_IDX) 0))->Writable_by_call()) {
	inaccessible_to_callees =
	  BS_Union1D(inaccessible_to_callees, aux_id, Pool());
      }
    }

    // Keep the updated information locally for now. It will be
    // incorporated into the Opt_stab()'s information later by
    // OPT_STAB::Incorporate_alias_class_info().
    Set_inaccessible_to_callees(inaccessible_to_callees);

    if (Tracing()) {
      fprintf(TFile, "----------\nAux ID's that can't be touched by callees:\n");
      BS_Print(Inaccessible_to_callees(), TFile);
      fprintf(TFile, "----------\n");
    }
  }

  // Set up the mapping from class number to information that will be
  // needed later to support member functions with alias class ID's as
  // arguments, namely Inaccessible_to_callees(IDTYPE) and
  // Non_alloca_memop(IDTYPE).
  _ac_id_to_acr_map.Init();

  Finalize_ac_map(entry_wn);

  WN_MAP_Delete(_indir_classification_map);

  _memops_classified = TRUE;
}


// ======================================================================


IDTYPE
ALIAS_CLASSIFICATION::Alias_class(const WN *const wn) const
{
  Is_True(OPCODE_is_store(WN_opcode(wn)) ||
	  OPCODE_is_load(WN_opcode(wn)) ||
	  WN_operator(wn) == OPR_LDA ||
	  WN_operator(wn) == OPR_LDMA ||
	  WN_operator(wn) == OPR_PARM,
	  ("ALIAS_CLASSIFICATION: WN must be memop"));

  if (_memops_classified) {
    FmtAssert(_mem_pool_valid,
	      ("ALIAS_CLASSIFICATION: Our memory is gone."));
    return WN_MAP32_Get(Memop_classification_map(), wn);
  }
  else {
    return PESSIMISTIC_AC_ID;
  }
}

// make targwn have the same alias class as srcwn
void
ALIAS_CLASSIFICATION::Copy_alias_class(const WN *const srcwn, WN *const targwn)
{
  Is_True(OPCODE_is_store(WN_opcode(srcwn)) ||
	  OPCODE_is_load(WN_opcode(srcwn)) ||
	  WN_operator(srcwn) == OPR_LDA ||
	  WN_operator(srcwn) == OPR_LDMA ||
	  WN_operator(srcwn) == OPR_PARM,
	  ("ALIAS_CLASSIFICATION: WN must be memop"));

  if (_memops_classified) {
    FmtAssert(_mem_pool_valid,
	      ("ALIAS_CLASSIFICATION: Our memory is gone."));
    IDTYPE class_id = WN_MAP32_Get(Memop_classification_map(), srcwn);
    WN_MAP32_Set(Memop_classification_map(), targwn, class_id);
  }
}

BOOL
ALIAS_CLASSIFICATION::Non_alloca_memop(const IDTYPE class_id) const
{
  if (_memops_classified &&
      class_id != OPTIMISTIC_AC_ID &&
      class_id != PESSIMISTIC_AC_ID) {
    FmtAssert(_mem_pool_valid,
	      ("ALIAS_CLASSIFICATION: Our memory is gone."));
    const ALIAS_CLASS_REP *const acr = _ac_id_to_acr_map.Lookup(class_id);
    FmtAssert(acr != NULL,
	      ("ALIAS_CLASSIFICATION::Non_alloca_memop: ACR with ID "
	       "%lu not found", class_id));
    return !acr->Alloca_class();
  }
  else {
    // Don't know.
    return FALSE;
  }
}


BOOL
ALIAS_CLASSIFICATION::Writable_by_call(const IDTYPE class_id) const
{
  if (_memops_classified &&
      class_id != OPTIMISTIC_AC_ID &&
      class_id != PESSIMISTIC_AC_ID) {
    FmtAssert(_mem_pool_valid,
	      ("ALIAS_CLASSIFICATION: Our memory is gone."));
    const ALIAS_CLASS_REP *const acr = _ac_id_to_acr_map.Lookup(class_id);
    FmtAssert(acr != NULL,
	      ("ALIAS_CLASSIFICATION::Writable_by_call: ACR with ID "
	       "%lu not found", class_id));
    return acr->Writable_by_call();
  }
  else {
    return TRUE;
  }
}


void
ALIAS_CLASSIFICATION::Release_resources(void)
{
  _base_id_map.Free_array();
  if (_memops_classified) {
    WN_MAP_Delete(Memop_classification_map());

    (&_preg_num_base_id_map)->~ID_MAP();
    (&_st_idx_to_base_id_map)->~ID_MAP();
    (&_ac_id_to_acr_map)->~ID_MAP();
    (&_altered_non_points_to_parms)->~ALIAS_CLASS_MEMBER_LIST();
    (&_alloca_memory_members)->~ALIAS_CLASS_MEMBER_LIST();
  }

  OPT_POOL_Pop(_pool, MEM_DUMP_FLAG+20);
  _mem_pool_valid = FALSE;
  OPT_POOL_Delete(_pool, MEM_DUMP_FLAG+20);
  _pool = NULL;
}


// ==============================================================
// ALIAS_CLASSIFICATION  print function for tracing
// ==============================================================


void
ALIAS_CLASSIFICATION::Print(FILE *fp) const
{
  // Print the storage-shape graph with pending lists
  // For each base_id, print the ID of the graph node that
  // characterizes it.
  fprintf(fp, "Global class: ");
  Global_class()->Print(fp);
  for (INT i = 1; i <= _base_id_map.Lastidx(); i++) {
    fprintf(fp, "LDA  class  for  base ID %u is ", i);
    _base_id_map[i]->Lda_class().Print(fp, Global_class());
    if (_base_id_map[i]->Lda_class().Class_pointed_to() != NULL) {
      fprintf(fp, "Class containing base ID %u is ", i);
      _base_id_map[i]->Lda_class().Class_pointed_to()->
	Print(fp, Global_class());
    }
  }
}


// ==============================================================
// ALIAS_MANAGER member functions to support alias classification
// ==============================================================


void
ALIAS_MANAGER::Transfer_alias_class_to_alias_manager(const
						     ALIAS_CLASSIFICATION &ac,
						     WN                   *wn)
{
  IDTYPE alias_id = Id(wn);
  OPERATOR opr = WN_operator(wn);
  if (alias_id != 0 &&
      alias_id != Preg_id() &&
      opr != OPR_FORWARD_BARRIER &&
      opr != OPR_BACKWARD_BARRIER &&
      opr != OPR_DEALLOCA) {
    // Copy the new alias class information into the POINTS_TO for
    // this WN, or create a new POINTS_TO if there's already different
    // alias class info there.
    IDTYPE     wn_alias_class = ac.Alias_class(wn);
    POINTS_TO *pt = Pt(alias_id);
    if (pt->Alias_class() == OPTIMISTIC_AC_ID) {
      pt->Set_alias_class(wn_alias_class);
    }
    else if (pt->Alias_class() != wn_alias_class) {
      // Clone a new alias ID and POINTS_TO for this WN.
      DevWarn("New alias ID on second pass");
      alias_id = Cross_dso_new_alias_id();
      Cross_dso_set_id(wn, alias_id);
      POINTS_TO *npt = Pt(alias_id);
      npt->Copy_fully(pt);
      npt->Set_alias_class(wn_alias_class);
    }
  }
  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN *wn2 = WN_first(wn); wn2 != NULL; wn2 = WN_next(wn2)) {
      Transfer_alias_class_to_alias_manager(ac, wn2);
    }
  }
  else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      Transfer_alias_class_to_alias_manager(ac, WN_kid(wn, i));
    }
  }
}


void
ALIAS_MANAGER::Forget_alias_class_info(void)
{
  for (IDTYPE alias_id = 1; alias_id <= _last_alias_id; ++alias_id) {
    if (alias_id != Preg_id()) {
      Pt(alias_id)->Set_alias_class(OPTIMISTIC_AC_ID);
    }
  }
}


// ======================================================================
