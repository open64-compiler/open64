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


//-*-c++-*-
//
// This code implements:
// Steensgaard, B.  "Points-to analysis in almost linear time."  Proceedings 
// of 23rd Annual ACM SIGACT-SIGPLAN Symposium on Principles of Programming
// Languages.  Held: St. Petersburg Beach, FL, USA, 21-24 Jan. 1996.
// (USA:  ACM, 1996. p. 32-41)  (Conference paper - English)

//   EXPR
// ILOAD
//
// gets treated like
//
//   EXPR
// STID unique_var
//   LDID unique_var
// ILOAD

// The classification has the following properties:
//   it is flow-free;
//   if two expressions or variables may be aliased, they are in the
//   same alias class;
//   if two variables or expressions may be pointed to by the same
//   pointer, they are in the same alias class (this is the biggest
//   conservatism inherent in the algorithm).

// For the interface to clients, see ipo_alias_class.h

#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "opt_points_to.h"	// OPTIMISTIC_AC_ID, PESSIMISTIC_AC_ID
#include "ipo_alias_class.h"
#include "targ_sim.h"		// Preg_Is_Dedicated

extern "C" {
#include "bitset.h"
}

IP_ALIAS_CLASSIFICATION *Ip_alias_class;
vector<char *>           Ip_alias_class_files;

// The following is initialized in the IP_ALIAS_CLASSIFICATION
// constructor because we want to support repeating alias
// classification. A single initialization at start-up is not enough
// for that.
IDTYPE              IP_ALIAS_CLASS_REP::_last_id_used;
IP_ACR_SLIST        IP_ALIAS_CLASS_REP::_free_list;
UINT32              IP_ALIAS_CLASS_REP::_recycled_acr_nodes;

#if Is_True_On
static const int num_acr_table_entries = 100;
IP_ALIAS_CLASS_REP *acr_table[num_acr_table_entries];

static const int num_acm_table_entries = 100;
IP_ALIAS_CLASS_MEMBER *acm_table[num_acm_table_entries];
static int next_acm_idx = 0;

void print_table(void)
{
  UINT i;

  for (i = 0;
       ((i <= IP_ALIAS_CLASS_REP::_last_id_used) &&
	(i < num_acr_table_entries));
       ++i) {
    if (acr_table[i] != NULL) {
      fprintf(TFile, "acr_table[%2u]: ", i);
      acr_table[i]->Print(TFile);
    }
  }

  for (i = 0;
       ((i < next_acm_idx) && (i < num_acm_table_entries));
       ++i) {
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

class IP_BASE_ID_MAP_ENTRY {
private:
  const ST_IDX       _pu_st_idx;
  const ST_IDX       _base_st_idx;
  IP_ALIAS_CLASS_MEMBER _base_member;
public:
  IP_BASE_ID_MAP_ENTRY(const ST_IDX    pu_st_idx,
		       const ST_IDX    base_st_idx,
		             IDTYPE    base_id) :
    _pu_st_idx(pu_st_idx), _base_st_idx(base_st_idx),
    _base_member(base_id)
      {
#if Is_True_On
	if (next_acm_idx < num_acm_table_entries) {
	  acm_table[next_acm_idx++] = &_base_member;
	}
#endif
      }

  IP_ALIAS_CLASS_MEMBER &Base_member(void)
    { return _base_member; }
};


IP_ALIAS_CLASS_REP *
IP_ALIAS_CLASSIFICATION::New_alias_class(const IP_ACR_TYPE_SORT sort,
					       IP_ALIAS_CLASS_MEMBER &acm) const
{
  IP_ALIAS_CLASS_REP *retval;

  if (IP_ALIAS_CLASS_REP::_free_list.front() != NULL) {
    retval = IP_ALIAS_CLASS_REP::_free_list.pop_front();

    // Destruct and reconstruct.
    retval->~IP_ALIAS_CLASS_REP();
    new(retval) IP_ALIAS_CLASS_REP(Pool());
    ++IP_ALIAS_CLASS_REP::_recycled_acr_nodes;
  }
  else {
    retval = CXX_NEW(IP_ALIAS_CLASS_REP(Pool()), Pool());
  }

#if Is_True_On
  if (retval->Id() < num_acr_table_entries) {
    acr_table[retval->Id()] = retval;
  }
#endif
  acm.Put_in_set(retval);
  if (Tracing() && _verbose) {
    fprintf(TFile, "Creating ");
    retval->Print(TFile, Global_data_class());
  }

  retval->_type_info._sort = sort;

  switch (sort) {
  case IP_ACR_BOTTOM_TYPE:
    // nothing to do, since the ref default constructor sets the ref
    // fields to NULL.
    break;
  case IP_ACR_REF_TYPE:
    {
      IP_ALIAS_CLASS_MEMBER *acm = New_alias_class_member();
      (void) New_alias_class(IP_ACR_BOTTOM_TYPE, *acm);
      retval->_type_info._ref.Data_member() = acm;
      acm = New_alias_class_member();
      (void) New_alias_class(IP_ACR_BOTTOM_TYPE, *acm);
      retval->_type_info._ref.Code_member() = acm;
      break;
    }
  case IP_ACR_LAMBDA_TYPE:
    // We don't do anything but allocate the signature here; wait
    // until the arity gets established and then set things up.
    retval->_type_info._signature =
      CXX_NEW(IP_AC_LAMBDA_TYPE_REP(Pool()), Pool());
    break;
  case IP_ACR_VALUE_TYPE:
  default:
    FmtAssert(FALSE, ("New_alias_class: illegal type"));
    break;
  }
  return retval;
}

// Improve the following to use an internally-maintained free list.
IP_ALIAS_CLASS_MEMBER *
IP_ALIAS_CLASSIFICATION::New_alias_class_member(void) const
{
  IP_ALIAS_CLASS_MEMBER *acm = CXX_NEW(IP_ALIAS_CLASS_MEMBER, Pool());

#if Is_True_On
  if (next_acm_idx < num_acm_table_entries) {
    acm_table[next_acm_idx++] = acm;
  }
#endif

  return acm;
}

#if DELETE_ME
IP_ALIAS_CLASS_MEMBER *
IP_ALIAS_CLASSIFICATION::New_alias_class_member(const IDTYPE base_id) const
{
  IP_ALIAS_CLASS_MEMBER *acm = CXX_NEW(IP_ALIAS_CLASS_MEMBER(base_id),
				       Pool());

#if Is_True_On
  if (next_acm_idx < num_acm_table_entries) {
    acm_table[next_acm_idx++] = acm;
  }
#endif

  return acm;
}
#endif // DELETE_ME

IP_ALIAS_CLASS_MEMBER *
IP_ALIAS_CLASSIFICATION::New_alias_class_member(const WN *wn) const
{
  IP_ALIAS_CLASS_MEMBER *acm = CXX_NEW(IP_ALIAS_CLASS_MEMBER(wn),
				       Pool());

#if Is_True_On
  if (next_acm_idx < num_acm_table_entries) {
    acm_table[next_acm_idx++] = acm;
  }
#endif

  return acm;
}

/* ARGSUSED */
void
IP_AC_LAMBDA_TYPE_REP::Set_arities(const UINT in_arity,
				   const UINT out_arity,
				   const IP_ALIAS_CLASSIFICATION *const ip_ac)
{
  arity_established = TRUE;

  IP_ALIAS_CLASS_MEMBER *dummy_arg;

  for (UINT i = 1; i < in_arity; ++i) {
    dummy_arg = ip_ac->New_alias_class_member();
    (void) ip_ac->New_alias_class(IP_ACR_REF_TYPE, *dummy_arg);
    fixed_args.push_back(dummy_arg);
  }

  dummy_arg = ip_ac->New_alias_class_member();
  (void) ip_ac->New_alias_class(IP_ACR_REF_TYPE, *dummy_arg);
  remaining_args = dummy_arg;

  if (!returns_new_memory) {
    dummy_arg = ip_ac->New_alias_class_member();
    (void) ip_ac->New_alias_class(IP_ACR_REF_TYPE, *dummy_arg);
    returns = dummy_arg;
  }
}

WN *
IP_ALIAS_CLASSIFICATION::Classify_wn_and_kids(WN *const wn)
{
  OPCODE opc = WN_opcode(wn);

  if (opc == OPC_BLOCK) {
    for (WN *wn2 = WN_first(wn); wn2 != NULL; ) {
      wn2 = Classify_wn_and_kids(wn2);
    }
    return NULL;
  }
  else if (OPCODE_is_store(opc)) {
    if (_verbose && Tracing()) {
      fprintf(TFile, "cwnk: Handling assignment:\n");
      fdump_tree(TFile, wn);
    }
    return Handle_assignment(wn);
  }
  else if (WN_operator(wn) == OPR_RETURN_VAL) {
    if (_verbose && Tracing()) {
      fprintf(TFile, "cwnk: Handling return value:\n");
      fdump_tree(TFile, wn);
    }
    return Handle_return_val(wn);
  }
  else if (OPCODE_is_call(opc)) {
    if (_verbose && Tracing()) {
      fprintf(TFile, "cwnk: Handling call:\n");
      fdump_tree(TFile, wn);
    }
    return Handle_call(wn);
  }
  else if (OPCODE_is_expression(opc)) {
    // wn is an expression kid of SCF and needs to be handled like the
    // RHS of an assignment statement.
    Classify_deref_of_expr(NULL, wn, FALSE);
    return NULL;
  }
  else if (opc == OPC_IO) {
    return WN_next(wn);
  }
  else if (!OPCODE_is_black_box(opc)) {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      WN *kid_i = WN_kid(wn, i);
      (void) Classify_wn_and_kids(kid_i);
    }
    return WN_next(wn);
  }
  else {
    return WN_next(wn);
  }
}

static BOOL Data_item(const ST *const st)
{
  BOOL      retval = ST_class(st) != CLASS_FUNC;
  Is_True(retval == (ST_sclass(st) == SCLASS_AUTO       ||
		     ST_sclass(st) == SCLASS_FORMAL     ||
		     ST_sclass(st) == SCLASS_FORMAL_REF ||
		     ST_sclass(st) == SCLASS_REG        ||
		     ST_sclass(st) == SCLASS_PSTATIC    ||
		     ST_sclass(st) == SCLASS_FSTATIC    ||
		     ST_sclass(st) == SCLASS_COMMON     ||
		     ST_sclass(st) == SCLASS_UGLOBAL    ||
		     ST_sclass(st) == SCLASS_DGLOBAL    ||
		     ST_sclass(st) == SCLASS_UNKNOWN    ||
		     ST_sclass(st) == SCLASS_THREAD_PRIVATE_FUNCS ||
		     ((ST_sclass(st) == SCLASS_EXTERN) &&
		      (ST_class(st) != CLASS_FUNC))),
	  ("IP_ALIAS_CLASSIFICATION: Data_item inconsistency"));
  return retval;
}

static BOOL Code_item(const ST *const st)
{
  return ST_class(st) == CLASS_FUNC;
}

static BOOL Directly_assignable_outside_analyzed_code(const ST *const st)
{
  Is_True(Data_item(st),
	  ("IP_ALIAS_CLASSIFICATION: Data item required"));
  Is_True(ST_export(st) != EXPORT_OPTIONAL,
	  ("IP_ALIAS_CLASSIFICATION: What is EXPORT_OPTIONAL?"));

  return (ST_export(st) == EXPORT_PROTECTED ||
	  ST_export(st) == EXPORT_PREEMPTIBLE ||
	  // Handle EXPORT_OPTIONAL conservatively in case we see it
	  // in production. We won't see it in debugging because of
	  // the Is_True above.
	  ST_export(st) == EXPORT_OPTIONAL);
}

static BOOL Directly_callable_outside_analyzed_code(const ST *const st)
{
  Is_True(Code_item(st),
	  ("IP_ALIAS_CLASSIFICATION: Code item required"));
  Is_True(ST_export(st) != EXPORT_OPTIONAL,
	  ("IP_ALIAS_CLASSIFICATION: What is EXPORT_OPTIONAL?"));

  return (ST_export(st) == EXPORT_PROTECTED ||
	  ST_export(st) == EXPORT_PREEMPTIBLE ||
	  // Handle EXPORT_OPTIONAL conservatively in case we see it
	  // in production. We won't see it in debugging because of
	  // the Is_True above.
	  ST_export(st) == EXPORT_OPTIONAL);
}

IDTYPE
IP_ALIAS_CLASSIFICATION::New_base_id(const ST_IDX pu_st_idx,
				     const ST_IDX st_idx)
{
  BOOL    function_class = FALSE;
  IDTYPE  id             = _base_id_map.Newidx();
  ST     *st             = &St_Table[st_idx];

  if (Tracing()) {
    fprintf(TFile, "Allocating base ID %u\n", id);
  }

  // Don't mess with the mapping from ST to base ID for registers;
  // their base ID's are found through the Preg_id_to_base_id_map
  // using the register number.
  if (ST_sclass(st) != SCLASS_REG) {
    AC_ST_IDENTIFIER st;
    st.Set_pu_st_idx(pu_st_idx);
    st.Set_base_st_idx(st_idx);
    _st_id_to_base_id_map.Insert(st, id);
  }

  _base_id_map[id] = CXX_NEW(IP_BASE_ID_MAP_ENTRY(pu_st_idx, st_idx, id),
			     Pool());

  if (Tracing()) {
    fprintf(TFile, "Base ID %3d is ", id);
  }

  ST_SCLASS storage_class = ST_sclass(st);

  // If this base_id is a variable local to this DSO, it gets a
  // regular old class.
  if (Data_item(st)) {
    // LDA of this symbol cannot point to a function.
    if (Directly_assignable_outside_analyzed_code(st)) {
      // This variable can be assigned directly by code that we will
      // not see during this compilation.
      //
      // This variable belongs in the global class because it can be
      // pointed to by other global variables assignments to which we
      // won't see. Note that for applications (such as embedded
      // applications, OS kernel, etc.) that don't actually link to
      // code that we won't see, the decision to make worst-case
      // assumptions about this variable at this stage may be
      // suboptimal, because there may actually be no calls to code we
      // will not analyze in this compilation. For the moment, we
      // don't worry about this; we assume that either there is code
      // we will not analyze (in the form of a DSO or .a file like
      // libc), or we can tolerate the performance hit associated with
      // grossly imprecise alias information for variables like this
      // one.
      if (Tracing()) {
	fprintf(TFile, "global: ");
      }
      _base_id_map[id]->Base_member().Put_in_set(Global_data_class());
    }
    else {
      // We will see all the code that can directly assign this
      // variable. If the variable is changed by code we won't see,
      // the variable's address must be passed to that code somehow.
      IP_ALIAS_CLASS_REP *ldid_class =
	New_alias_class(IP_ACR_REF_TYPE, _base_id_map[id]->Base_member());
      if (Tracing()) {
	fprintf(TFile, " local: ");
      }
    }
  }
  else {
    Is_True(Code_item(st),
	    ("IP_ALIAS_CLASSIFICATION: Referenced ST's must be either "
	     "code or data"));
    // LDA of this symbol cannot point to a variable.
    BOOL callee_returns_new_memory = Callee_returns_new_memory(st);
    BOOL callee_frees_memory       = Callee_frees_memory(st);

    BOOL require_new_class = (callee_returns_new_memory ||
			      callee_frees_memory);
    if (Directly_callable_outside_analyzed_code(st) &&
	!require_new_class) {
      // The function corresponding to this ST can be directly called
      // from code we won't see during this compilation. Therefore we
      // have to assume parameters to this function can be arbitrarily
      // nasty, and that return values from the function can be used
      // in arbitrarily nefarious ways.
      //
      // TODO: Does it make sense to distinguish direct access from
      // indirect access for functions? If we make the distinction
      // here, we will just have to duplicate the following code in
      // places where the address of some otherwise-hidden function is
      // passed to code outside the boundaries of what we'll see in
      // this compilation.
      _base_id_map[id]->Base_member().Put_in_set(Global_func_class());
    }
    else {
      // The function corresponding to this ST can be directly called
      // only from code we will see during this compilation, or we
      // know it returns new memory (like malloc) or deallocates
      // memory (like free).
      IP_ALIAS_CLASS_REP *callee_class;
      if (callee_returns_new_memory ||
	  callee_frees_memory) {
	// We go ahead and allocate a LAMBDA-typed class, since the
	// arity doesn't matter, and so we can go ahead and set up its
	// signature.
	callee_class =
	  New_alias_class(IP_ACR_LAMBDA_TYPE,
			  _base_id_map[id]->Base_member());
      }
      else {
	// Defer conversion to LAMBDA type until arity is known
	// (either from a function declaration or a call site).
	callee_class =
	  New_alias_class(IP_ACR_BOTTOM_TYPE,
			  _base_id_map[id]->Base_member());
      }
      if (callee_returns_new_memory) {
	callee_class->Signature().returns_new_memory = TRUE;
	callee_class->Signature().Set_arities(0, 1, this);
      }
      if (callee_frees_memory) {
	callee_class->Signature().frees_memory = TRUE;
      }
    }
    if (Tracing()) {
      fprintf(TFile, " function: ");
      function_class = TRUE;
    }
  }
  if (Tracing()) {
    Print_ST(TFile, st, _verbose || function_class);
  }
  return id;
}

IDTYPE
IP_ALIAS_CLASSIFICATION::ST_base_id(const ST_IDX pu_st_idx,
				    const ST_IDX base_st_idx)
{
  Is_True(ST_sclass(St_Table[base_st_idx]) != SCLASS_REG &&
	  ST_class(St_Table[base_st_idx]) != CLASS_PREG,
	  ("IP_ALIAS_CLASSIFICATION::St_base_id: wrong mechanism for PREG"));

  IDTYPE base_id;

  AC_ST_IDENTIFIER st;
  st.Set_pu_st_idx(pu_st_idx);
  st.Set_base_st_idx(base_st_idx);
  base_id = _st_id_to_base_id_map.Lookup(st);

  if (base_id == (IDTYPE) 0) {
    return New_base_id(pu_st_idx, base_st_idx);
  }
  else {
    return base_id;
  }
}

IDTYPE
IP_ALIAS_CLASSIFICATION::Base_id(const ST    *const base_st,
				 const INT64        base_ofst)
{
  // If the base_st is a PREG base, we treat it specially. Every
  // PREG gets its own base ID, even though they all share a small
  // fixed number of base ST's (one per MTYPE). In principle we can do
  // this sort of thing for any base ST where we know that different
  // offsets from that base do not overlap and do not have addresses
  // that can be derived from one another via legal address
  // arithmetic.
  if (ST_sclass(base_st) == SCLASS_REG) {
    // This is a PREG. Use the preg number to map to the base ID.
    ST_IDX base_st_idx = ST_st_idx(base_st);
    ST_IDX pu_st_idx =
      ST_st_idx(Scope_tab[CURRENT_SYMTAB].st);
    AC_PREG_IDENTIFIER preg;
    preg.Set_pu_st_idx(pu_st_idx);
    preg.Set_preg_num(base_ofst);
    IDTYPE base_id = Preg_id_to_base_id_map().Lookup(preg);
    if (base_id == 0) {
      base_id = New_base_id(pu_st_idx, base_st_idx);
      Preg_id_to_base_id_map().Insert(preg, base_id);
    }
    return base_id;
  }
  else {
    ST_IDX base_st_idx = ST_st_idx(base_st);
    ST_IDX pu_st_idx = ST_st_idx(Scope_tab[ST_IDX_level(base_st_idx)].st);
    return ST_base_id(pu_st_idx, base_st_idx);
  }
}

void
IP_ALIAS_CLASSIFICATION::Find_declared_base_and_offset(ST_IDX  st_idx,
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
IP_ALIAS_CLASSIFICATION::WN_base_id(const WN *wn)
{
  ST_IDX declared_base_idx;
  INT64  declared_offset;

  if (OPCODE_is_call(WN_opcode(wn))) {
    declared_offset = (INT64) 0;
  }
  else {
    declared_offset = WN_offset(wn);
  }
  Find_declared_base_and_offset(WN_st_idx(wn),
				declared_base_idx,
				declared_offset);
  return Base_id(&St_Table[declared_base_idx], declared_offset);
}

static BOOL
Is_LDA_of_variable(const WN       *const wn)
{
  Is_True(WN_operator(wn) == OPR_LDA,
	  ("Is_LDA_of_variable: WN must be LDA node"));

  ST *st = WN_st(wn);
  Is_True(ST_class(st) != CLASS_PREG,
	  ("Is_LDA_of_variable: CLASS_PREG"));

  return (ST_class(st) == CLASS_UNK ||
	  ST_class(st) == CLASS_VAR ||
	  // Stupid FORTRAN -- takes addresses of constants.
	  ST_class(st) == CLASS_CONST ||  // constant in memory
	  ST_class(st) == CLASS_BLOCK);
}

IP_ALIAS_CLASS_REP *
IP_ALIAS_CLASSIFICATION::Class_of_base_id(const IDTYPE base_id) const
{
  return _base_id_map[base_id]->Base_member().Alias_class();
}


void
IP_ALIAS_CLASS_REP::Add_pending(IP_ALIAS_CLASS_REP *item,
				BOOL                trace)
{
  Is_True(item != NULL && item->Representative() != NULL,
	  ("IP_ALIAS_CLASSIFICATION: Cannot add <NULL> to pending list"));

  if (trace) {
    fprintf(TFile, "Pending(%d) += %d\n", Id(), item->Id());
  }

    _pending.push_front(item->Representative());
}

void
IP_ALIAS_CLASS_REP::Process_pending(      IP_ALIAS_CLASS_MEMBER *member,
				          MEM_POOL              *pool,
				    const BOOL                   trace)
{
  // For each item in Pending(), join the item's class with this.

  if (trace) {
    fprintf(TFile, "Process pending of ");
    Print(TFile);
    fprintf(TFile, "              into ");
    member->Alias_class()->Print(TFile);
  }

  for (PENDING_SET::iterator p_item = Pending().begin();
       p_item != Pending().end();
       ++p_item) {
    member->Alias_class()->Join(*((*p_item)->Alias_class()), pool, trace);
  }
}

void
IP_ALIAS_CLASS_REP::Settype(const IP_AC_REF_TYPE_REP &new_ref,
			          MEM_POOL           *pool,
			    const BOOL                trace)
{
  Is_True(Sort() == IP_ACR_BOTTOM_TYPE,
	  ("Settype: must be BOTTOM"));
  _type_info._sort = IP_ACR_REF_TYPE;
  _type_info._ref = new_ref;
  Process_pending(Representative(), pool, trace);
}

void
IP_ALIAS_CLASS_REP::Merge_pending(      IP_ALIAS_CLASS_REP &that,
				  const BOOL                trace)
{
  // Put all of that's pending items into *this's pending set.

  if (trace) {
    fprintf(TFile, "Pending(%d) += Pending(%d)\n", Id(), that.Id());
  }

  Pending().splice(Pending().begin(), that.Pending());
  Is_True(that.Pending().empty(),
	  ("IP_ALIAS_CLASSIFICATION: Emptied pending container "
	   "must be empty"));
}

void
IP_ALIAS_CLASS_REP::Free_acr(void)
{
#if Is_True_On
  if (Id() < num_acr_table_entries) {
    acr_table[Id()] = NULL;
  }
#endif
  _free_list.push_front(this);
}

void
IP_ALIAS_CLASS_REP::Join(      IP_ALIAS_CLASS_REP &that,
			       MEM_POOL           *pool,
			 const BOOL                trace)
{
  if (this != &that) {
    IP_ALIAS_CLASS_REP *result = (IP_ALIAS_CLASS_REP *) Union(that);
    IP_ALIAS_CLASS_REP *non_result = (result == this ? &that : this);
    if (trace) {
      fprintf(TFile, "%d <-- Join(%d,%d)\n",
	      result->Id(), Id(), that.Id());
    }
    if (Sort()      == IP_ACR_BOTTOM_TYPE &&
	that.Sort() == IP_ACR_BOTTOM_TYPE) {
      // Merge the two pending lists.
      result->Merge_pending(*non_result, trace);
    }
    else if (Sort() == IP_ACR_BOTTOM_TYPE) {
      if (result == this) {
	result->_type_info = that._type_info;
      }
      Process_pending(result->Representative(), pool, trace);
    }
    else if (that.Sort() == IP_ACR_BOTTOM_TYPE) {
      if (result == &that) {
	result->_type_info = _type_info;
      }
      that.Process_pending(result->Representative(), pool, trace);
    }
    else {
      if (Sort() == IP_ACR_REF_TYPE) {
	FmtAssert(that.Sort() == IP_ACR_REF_TYPE,
		  ("Join: Cannot join REF to another type"));
	// The "unify" function for REF types from Steensgaard's paper is
	// expanded inline here.
	Data_class_pointed_to()->Join(*that.Data_class_pointed_to(), pool, trace);
	Func_class_pointed_to()->Join(*that.Func_class_pointed_to(), pool, trace);
	Is_True(Data_class_pointed_to() == that.Data_class_pointed_to(),
		("IP_ACR::Join: Inconsistent join"));
	Is_True(Func_class_pointed_to() == that.Func_class_pointed_to(),
		("IP_ACR::Join: Inconsistent join"));
      }
      else {
	FmtAssert(Sort()      == IP_ACR_LAMBDA_TYPE &&
		  that.Sort() == IP_ACR_LAMBDA_TYPE,
		  ("Join: Cannot join mismatched types"));
	// The "unify" function for LAMBDA types from Steensgaard's
	// paper is called Union_func_args.
	Signature().Union_func_args(that.Signature(), pool, trace);
      }
    }
    non_result->Free_acr();
  }
}

void
IP_AC_LAMBDA_TYPE_REP::Union_func_args(      IP_AC_LAMBDA_TYPE_REP &that,
				             MEM_POOL              *pool,
				       const BOOL                   trace)
{
  Is_True(arity_established,
	  ("IP_AC_LAMBDA_TYPE_REP: arity must be established"));
  Is_True(that.arity_established,
	  ("IP_AC_LAMBDA_TYPE_REP: that.arity must be established"));

  // For each argument and each return value, join the appropriate
  // classes.
  IP_ALIAS_CLASS_MEMBER **arg;
  IP_ALIAS_CLASS_MEMBER **that_arg;
  for (arg = &(*fixed_args.begin()),
       that_arg = &(*that.fixed_args.begin());
       (arg != &(*fixed_args.end()) &&
	that_arg != &(*that.fixed_args.end()));
       ++arg, ++that_arg)
  {
    IP_AC_VALUE_TYPE_REP &arg1 = (*     arg)->Alias_class()->Ref()._value;
    IP_AC_VALUE_TYPE_REP &arg2 = (*that_arg)->Alias_class()->Ref()._value;

    IP_ALIAS_CLASS_MEMBER *data_deref_arg1 = arg1.Data_member();
    IP_ALIAS_CLASS_MEMBER *data_deref_arg2 = arg2.Data_member();
    IP_ALIAS_CLASS_MEMBER *code_deref_arg1 = arg1.Code_member();
    IP_ALIAS_CLASS_MEMBER *code_deref_arg2 = arg2.Code_member();
    data_deref_arg1->Alias_class()->
      Join(*data_deref_arg2->Alias_class(), pool, trace);
    code_deref_arg1->Alias_class()->
      Join(*code_deref_arg2->Alias_class(), pool, trace);
  }

  // Clean up the tails of the lists in case the argument counts
  // didn't match.
  IP_ALIAS_CLASS_MEMBER *vararg_data_representative,
                        *vararg_code_representative;
  IP_ALIAS_CLASS_MEMBER **start, **stop;

  if (arg != &(*fixed_args.end()))
  {
    start = arg;
    stop  = &(*fixed_args.end());
    IP_AC_VALUE_TYPE_REP &vararg_rep =
      that.remaining_args->Alias_class()->Ref()._value;
    vararg_data_representative = vararg_rep.Data_member();
    vararg_code_representative = vararg_rep.Code_member();
  }
  else {
    start = that_arg;
    stop = &(*that.fixed_args.end());
    IP_AC_VALUE_TYPE_REP &vararg_rep =
      remaining_args->Alias_class()->Ref()._value;
    vararg_data_representative = vararg_rep.Data_member();
    vararg_code_representative = vararg_rep.Code_member();
  }
  while (start != stop) {
    (*start)->Alias_class()->Data_class_pointed_to()->
      Join(*vararg_data_representative->Alias_class(), pool, trace);
    (*start)->Alias_class()->Func_class_pointed_to()->
      Join(*vararg_code_representative->Alias_class(), pool, trace);
    ++start;
  }

  remaining_args->Alias_class()->Data_class_pointed_to()->
    Join(*(that.remaining_args->Alias_class()->Data_class_pointed_to()),
	 pool, trace);
  remaining_args->Alias_class()->Func_class_pointed_to()->
    Join(*(that.remaining_args->Alias_class()->Func_class_pointed_to()),
	 pool, trace);

#ifdef KEY // bug 8109
  Return_class_member()->Alias_class()->Data_class_pointed_to()->
    Join(*(that.Return_class_member()->Alias_class()->Data_class_pointed_to()), pool, trace);
  Return_class_member()->Alias_class()->Func_class_pointed_to()->
    Join(*(that.Return_class_member()->Alias_class()->Func_class_pointed_to()), pool, trace);
#else
  Returns()->Alias_class()->Data_class_pointed_to()->
    Join(*(that.Returns()->Alias_class()->Data_class_pointed_to()), pool, trace);
  Returns()->Alias_class()->Func_class_pointed_to()->
    Join(*(that.Returns()->Alias_class()->Func_class_pointed_to()), pool, trace);
#endif
}

void
IP_ALIAS_CLASSIFICATION::Conditional_join(IP_ALIAS_CLASS_REP *lhs_deref_class,
					  IP_ALIAS_CLASS_REP *rhs_deref_class)
{
  if (lhs_deref_class != rhs_deref_class) {
    if (rhs_deref_class->Sort() == IP_ACR_BOTTOM_TYPE) {
      // If whoever points to the rhs ever becomes a pointer to anything
      // (i.e., if the rhs becomes non-BOTTOM), the lhs class must join
      // the rhs class.
      rhs_deref_class->Add_pending(lhs_deref_class, Tracing());
    }
    else {
      lhs_deref_class->Join(*rhs_deref_class, Pool(), Tracing());
    }
  }
}

void
IP_ALIAS_CLASS_REP::Print(FILE               *fp,
			  IP_ALIAS_CLASS_REP *global_class) const
{
  fprintf(fp, "class %d ", Id());
  FmtAssert(Id() != 0 || global_class == NULL || global_class == this,
	    ("Class of nonzero ID must not be global"));
  if (global_class == this) {
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
  switch (Sort()) {
  case IP_ACR_BOTTOM_TYPE:
    fprintf(fp, " bot\n");
    break;
  case IP_ACR_REF_TYPE:
    fprintf(fp, " --> (%d<%p> x %d<%p>)\n",
	    Data_class_pointed_to()->Id(), Ref().Data_member(),
	    Func_class_pointed_to()->Id(), Ref().Code_member());
    break;
  case IP_ACR_LAMBDA_TYPE:
    if (Signature().returns_new_memory) {
      if (Signature().frees_memory) {
	fprintf(fp, " realloc(");
      }
      else {
	fprintf(fp, " alloc(");
      }
    }
    else if (Signature().frees_memory) {
      fprintf(fp, " free(");
    }
    else {
      fprintf(fp, " (&%d<%p>) <-- lambda(",
	      Signature().Return_class()->Id(),
	      Signature().Returns());
      IP_ALIAS_CLASS_MEMBER * const *arg;
      for (arg = &(*Signature().fixed_args.begin());
	   arg != &(*Signature().fixed_args.end());
	   ++arg)
      {
	fprintf(fp, "&%d<%p>,",
		(*arg)->Alias_class()->Id(), *arg);
      }
      fprintf(fp, "&%d<%p>",
	      Signature().remaining_args->Alias_class()->Id(),
	      Signature().remaining_args);
    }
    fprintf(fp, ")\n");
    break;
  case IP_ACR_VALUE_TYPE:
  default:
    FmtAssert(FALSE, ("Illegal IP_ACR sort in IP_ACR::Print()"));
  }
}

void
IP_ALIAS_CLASS_MEMBER::Print(FILE *fp) const
{
  if (_kind == ACM_BASE) {
    fprintf(fp, "base ID %d", Base_id());
  }
  else if (_kind == ACM_WN) {
    fprintf(fp, "wn");
    // Cast away const because of piss-poor design of various
    // low-level components (like the alias manager) queried by
    // ir_put_wn (in ir_reader.cxx).
    //
    // Note: Can't dump the WN. It might belong to another PU whose
    // memory (or whose symbol table's memory) has been recycled.
    // fdump_wn(fp, (WN *) Wn());
  }
  else {
    fprintf(fp, "unknown <%p>", this);
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
// made in the analogous function in be/opt/opt_alias_class.cxx.
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

// Handle an assignment of the value of expr to a variable in the
// class of the given lhs_member.
//
// The class containing lhs_member will be the class corresponding to
// a new unique temporary in the case of subexpressions; the new
// unique temporary may or may not have been joined with something
// else.
//
// The argument directly_dereferenced is partly a hack to handle cases
// like
// ILOAD(INTCONST 0x10447270)
// that may be used to access memory mapped I/O, etc. Those memory
// operations should be declared volatile, so we shouldn't have to
// analyze them but I'm still uncomfortable with our analysis saying
// that such an ILOAD doesn't alias with another identical ILOAD
// somewhere else in the program. Therefore we don't give a fresh type
// variable to INTCONST nodes unless directly_dereferenced is
// false. This is a hack because in principle we could still have
// something like
// ILOAD(ADD(INTCONST 0x10447270, INTCONST 0x8)).
// In this situation directly_dereferenced will be false and we will
// say that the ILOAD doesn't alias with another identical ILOAD. If
// we never gave fresh type variables to INTCONST nodes, performance
// would suffer horribly. In such a case, we will really depend on the
// user to declare the access volatile.
//
// The other part of the role of directly_dereferenced is to make sure
// that ILOADs of each symbol alias with each other, even if the
// symbol is never assigned a pointer value anywhere in the
// program. This is necessary in some situations (i.e., if the symbol
// is ST_PT_TO_UNIQUE_MEM) to keep the compiler from asserting during
// WOPT's vsym assignment phase. If directly_dereferenced is TRUE, we
// assume that the lhs_member's Data_class_pointed_to() needs to be
// joined unconditionally with the class in which we place expr.
// Exception: We don't do that join in the case where we have an LDA
// of a symbol whose base ID class has BOTTOM type. Such a symbol can
// only be a code item, and it is handled as explained in the OPR_LDA
// case below.
//
void
IP_ALIAS_CLASSIFICATION::Classify_deref_of_expr(IP_ALIAS_CLASS_MEMBER        *lhs_member,
						WN	              *const  expr,
						BOOL                          directly_dereferenced)
{
  Is_True(lhs_member == NULL ||
	  lhs_member->Alias_class()->Sort() == IP_ACR_REF_TYPE,
	  ("Classify_deref_of_expr: lhs must be REF type"));

  OPCODE   opc = WN_opcode(expr);
  OPERATOR opr = OPCODE_operator(opc);

  if (_verbose && Tracing()) {
    fprintf(TFile, "cdoe: analyzing ");
    fdump_wn(TFile, expr);
  }
  if (OPCODE_is_leaf(opc)) {
    if (lhs_member != NULL) {
      switch (opr) {
      case OPR_LDA:
	{
	  IP_ALIAS_CLASS_REP *acr = Class_of_base_id(WN_base_id(expr));
	  IP_ALIAS_CLASS_REP *lhs_deref_class;
	  if (acr->Sort() == IP_ACR_BOTTOM_TYPE) {
	    // A rather rare case, in which we have established the
	    // base ID for a symbol but haven't yet established the
	    // type of the symbol. This can happen only for code items
	    // whose addresses are referenced without any arity
	    // information available (i.e., when a function's address
	    // is passed as a parameter or assigned to a variable). We
	    // add the lhs class to the pending set of the base ID
	    // class so that when the arity gets established (and the
	    // type is converted to LAMBDA), the required join will
	    // occur.
	    //
	    // Note: we don't do the join even if
	    // directly_dereferenced is TRUE. We assume we will
	    // eventually see the arity come to light later.
	    Is_True(Code_item(WN_st(expr)),
		    ("Classify_deref_of_expr: BOTTOM base ID type must "
		     "be code"));
	    acr->Add_pending(lhs_member->Alias_class()->Func_class_pointed_to(),
			     Tracing());
	  }
	  else {
	    if (acr->Sort() == IP_ACR_LAMBDA_TYPE) {
	      lhs_deref_class = lhs_member->Alias_class()->Func_class_pointed_to();
	    }
	    else if (acr->Sort() == IP_ACR_REF_TYPE) {
	      lhs_deref_class = lhs_member->Alias_class()->Data_class_pointed_to();
	    }
	    // Unconditional join; the lhs class is definitely a
	    // pointer now.
	    lhs_deref_class->Join(*acr, Pool(), Tracing());
	  }
	  break;
	}
      case OPR_INTCONST:
	// See comments about directly_dereferenced at the head of this
	// procedure.
	if (directly_dereferenced) {
	  // TODO: Maybe find a way to assign base ID's to integer
	  // constants so we can match them up here. We need such a
	  // mechanism to fully analyze the points-to behavior of
	  // ILOAD(INTCONST) or ISTORE(INTCONST).
	  DevWarn("dereference of INTCONST not fully analyzed");
	  lhs_member->Alias_class()->Join(*Const_addr_class(), Pool(), Tracing());
	}
	else {
	  // The common case, in which we presume the INTCONST is not a
	  // pointer to anything. Do nothing.
	}
	break;
      case OPR_LDID:
	{
	  IP_ALIAS_CLASS_MEMBER *rhs_member =
	    Class_of_base_id(WN_base_id(expr))->Representative();
	  if (rhs_member->Alias_class()->Sort() == IP_ACR_REF_TYPE) {
#ifdef KEY
	    if (Ty_Table[WN_ty (expr)].kind == KIND_POINTER)
	      directly_dereferenced = TRUE;
#endif // KEY
	    if (directly_dereferenced) {
	      // Join unconditionally; the lhs_member's class should
	      // eventually become a pointer, even if it isn't
	      // already.
	      lhs_member->Alias_class()->Data_class_pointed_to()->
		Join(*rhs_member->Alias_class()->Data_class_pointed_to(),
		     Pool(), Tracing());
	      lhs_member->Alias_class()->Func_class_pointed_to()->
		Join(*rhs_member->Alias_class()->Func_class_pointed_to(),
		     Pool(), Tracing());
	    }
	    else {
	      Conditional_join(lhs_member->Alias_class()->
			         Data_class_pointed_to(),
			       rhs_member->Alias_class()->
			         Data_class_pointed_to());
	      Conditional_join(lhs_member->Alias_class()->
			         Func_class_pointed_to(),
			       rhs_member->Alias_class()->
			         Func_class_pointed_to());
	    }
	  }
	  else {
	    Is_True(Code_item(WN_st(expr)),
		    ("Classify_deref_of_expr: Non-code item must have "
		     "REF type"));
	    // This can happen when people perform direct loads from
	    // code items. For example,
	    // return *((int *) main); // can be expressed as LDID(main)...
	    lhs_member->Alias_class()->Data_class_pointed_to()->
	      Join(*Class_of_code_misused_as_data(), Pool(), Tracing());
	    if (_verbose && Tracing()) {
	      fprintf(TFile, " --- Expression is direct load of code\n");
	    }
	  }
	  if (_verbose && Tracing()) {
	    fprintf(TFile, " --- After analyzing LDID ");
	    fdump_wn(TFile, expr);
	    fprintf(TFile, "     lhs is ");
	    lhs_member->Alias_class()->Print(TFile, Global_data_class());
	    Print(TFile);
	  }
	  break;
	}
      default:
	// Do nothing -- the given expression can't be a pointer.
	break;
      }
    }
  }
  // PARM by reference gets the alias class corresponding to
  // dereferencing its kid.
  else if (OPCODE_is_load(opc) ||
	   Is_fortran_reference_parm(expr)) {
    // Handling reference parameters here is a little strange, but we
    // do it because we need to set the Indir_classification_map for
    // them. They are considered to be indirect memory operations by
    // WOPT's alias analysis.
    //
    // Note, though, that the join operations are different for PARMs,
    // since the formal must end up pointing to whatever the actual
    // points to, not to what a dereference of the actual points to.
    FmtAssert(opr == OPR_ILOAD ||
	      opr == OPR_MLOAD ||
	      opr == OPR_PARM,
	      ("IP_ALIAS_CLASSIFICATION: Indirect load: unexpected opcode"));

    // Classify the base address of the indirect load, merged with a
    // new unique temporary.
    IP_ALIAS_CLASS_MEMBER *address_member = New_alias_class_member();
    (void) New_alias_class(IP_ACR_REF_TYPE, *address_member);

    Classify_deref_of_expr(address_member, WN_kid0(expr), TRUE);

    IP_ALIAS_CLASS_REP    *address_class = address_member->Alias_class();

    IP_ALIAS_CLASS_MEMBER *expr_member = New_alias_class_member(expr);
    IP_ALIAS_CLASS_REP    *expr_class  = address_class->Data_class_pointed_to(); 

    expr_member->Put_in_set(expr_class);

    if (expr_class->Sort() == IP_ACR_BOTTOM_TYPE) {
      IP_ALIAS_CLASS_REP *expr_deref_data_acr =
	New_alias_class(IP_ACR_BOTTOM_TYPE, *New_alias_class_member());
      IP_ALIAS_CLASS_REP *expr_deref_code_acr =
	New_alias_class(IP_ACR_BOTTOM_TYPE, *New_alias_class_member());
      // How exactly do we want to set up the interface to Settype?
      expr_class->
	Settype(IP_AC_REF_TYPE_REP(expr_deref_data_acr->Representative(),
				   expr_deref_code_acr->Representative()),
		Pool(), Tracing());
    }

    if (lhs_member != NULL) {
      if (directly_dereferenced) {
	Is_True(opr != OPR_PARM, ("Classify_deref_of_expr: Need to "
				  "add code to handle PARM"));
	lhs_member->Alias_class()->Data_class_pointed_to()->
	  Join(*expr_member->Alias_class()->Data_class_pointed_to(),
	       Pool(), Tracing());
	lhs_member->Alias_class()->Func_class_pointed_to()->
	  Join(*expr_member->Alias_class()->Func_class_pointed_to(),
	       Pool(), Tracing());
      }
      else {
	if (opr == OPR_PARM) {
	  // Use members as handles.
	  Conditional_join(lhs_member->Alias_class()->Data_class_pointed_to(),
			   address_member->Alias_class()->Data_class_pointed_to());
	  Conditional_join(lhs_member->Alias_class()->Func_class_pointed_to(),
			   address_member->Alias_class()->Func_class_pointed_to());
	}
	else {
	  // Don't use expr_class here because the first join may
	  // invalidate it. Use members as handles when there's any doubt.
	  Conditional_join(lhs_member->Alias_class()->Data_class_pointed_to(),
			   expr_member->Alias_class()->Data_class_pointed_to());
	  Conditional_join(lhs_member->Alias_class()->Func_class_pointed_to(),
			   expr_member->Alias_class()->Func_class_pointed_to());
	}
      }
    }

    WN_MAP_Set(Indir_classification_map(), expr, (void *) expr_member);

    if (_verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing expression ");
      fdump_wn(TFile, expr);
      fprintf(TFile, "     expr_class is ");
      expr_class->Print(TFile, Global_data_class());
      Print(TFile);
#if Is_True_On
      print_table();
#endif
    }
  }
  else if (opr == OPR_ARRAY) {
    for (INT i = 1; i < WN_kid_count(expr); i++) {
      Classify_deref_of_expr(NULL, WN_kid(expr, i), FALSE);
    }
    if (_verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing expression ");
      fdump_wn(TFile, expr);
      Print(TFile);
    }
    Classify_deref_of_expr(lhs_member, WN_kid0(expr), directly_dereferenced);
  }
  else if (opr == OPR_ALLOCA) {
    for (INT i = 0; i < WN_kid_count(expr); i++) {
      Classify_deref_of_expr(NULL, WN_kid(expr, i), FALSE);
    }
    if (_verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing ALLOCA kids ");
      fdump_wn(TFile, expr);
      Print(TFile);
    }
    if (lhs_member != NULL) {
      // Unconditionally make lhs_member's class a pointer class; it
      // is becoming a pointer right now if it wasn't already.
      IP_ALIAS_CLASS_REP *lhs_deref_class =
	lhs_member->Alias_class()->Data_class_pointed_to();
      if (lhs_deref_class->Sort() == IP_ACR_BOTTOM_TYPE) {
	IP_ALIAS_CLASS_REP *alloca_mem_data_ptr_acr =
	  New_alias_class(IP_ACR_BOTTOM_TYPE, *New_alias_class_member());
	IP_ALIAS_CLASS_REP *alloca_mem_code_ptr_acr =
	  New_alias_class(IP_ACR_BOTTOM_TYPE, *New_alias_class_member());
	lhs_deref_class->
	  Settype(IP_AC_REF_TYPE_REP(alloca_mem_data_ptr_acr->Representative(),
				     alloca_mem_code_ptr_acr->Representative()),
		  Pool(), Tracing());
      }
    }
    else {
      if (Opt_Level >= 2)
	Fail_FmtAssertion("Classify_deref_of_expr: Dangling ALLOCA is illegal");
      else
	DevWarn("Classify_deref_of_expr: Dangling ALLOCA is illegal");
    }
  }
  else {
    // The opcode is some sort of expression that might be a pointer
    // value, and it's not a leaf. Create a class to hold the value of
    // the expression (as if it were stored in a temporary) and handle
    // the expression kids.

    IP_ALIAS_CLASS_MEMBER *expr_member;

    // Join with the LHS class only if a pointer value can be
    // recovered from the present expression.
    if (Opcode_cannot_be_pointer_value(opr, opc)) {
      expr_member = NULL;
    }
    else {
      expr_member = New_alias_class_member(expr);
      (void) New_alias_class(IP_ACR_REF_TYPE, *expr_member);
    }
    for (INT i = 0; i < WN_kid_count(expr); i++) {
      Classify_deref_of_expr(expr_member, WN_kid(expr, i), FALSE);
    }
    if (expr_member != NULL &&
	lhs_member != NULL) {
      if (directly_dereferenced) {
	lhs_member->Alias_class()->Data_class_pointed_to()->
	  Join(*expr_member->Alias_class()->Data_class_pointed_to(),
	       Pool(), Tracing());
	lhs_member->Alias_class()->Func_class_pointed_to()->
	  Join(*expr_member->Alias_class()->Func_class_pointed_to(),
	       Pool(), Tracing());
      }
      else {
	Conditional_join(lhs_member->Alias_class()->Data_class_pointed_to(),
			 expr_member->Alias_class()->Data_class_pointed_to());
	Conditional_join(lhs_member->Alias_class()->Func_class_pointed_to(),
			 expr_member->Alias_class()->Func_class_pointed_to());
      }
    }
    if (_verbose && Tracing()) {
      fprintf(TFile, " --- After analyzing expression ");
      fdump_wn(TFile, expr);
      fprintf(TFile, "     local lhs is ");
      if (lhs_member != NULL) {
	lhs_member->Alias_class()->Print(TFile, Global_data_class());
      }
      else {
	fprintf(TFile, "<NULL>\n");
      }
      Print(TFile);
    }
  }
}

IP_ALIAS_CLASS_MEMBER *
IP_ALIAS_CLASSIFICATION::Classify_lhs_of_store(WN *const stmt_or_idname)
{
  OPERATOR opr = WN_operator(stmt_or_idname);

  if (opr == OPR_STID || opr == OPR_IDNAME) {
    return Class_of_base_id(WN_base_id(stmt_or_idname))->Representative();
  }
  else if (opr == OPR_ISTORE || opr == OPR_MSTORE) {
    IP_ALIAS_CLASS_MEMBER *lhs_address_member =
      New_alias_class_member(WN_kid1(stmt_or_idname));
    (void) New_alias_class(IP_ACR_REF_TYPE, *lhs_address_member);
    Classify_deref_of_expr(lhs_address_member, WN_kid1(stmt_or_idname), TRUE);
    IP_ALIAS_CLASS_MEMBER *lhs_member =
      lhs_address_member->Alias_class()->Ref().Data_member();
    if (Tracing()) {
      fprintf(TFile, "Setting ISTORE Indir map to %p\n",
	      lhs_member->Alias_class());
    }
    WN_MAP_Set(Indir_classification_map(), stmt_or_idname, lhs_member);
    // Now create the class referenced by the ISTORE if it doesn't
    // exist already.
    if (lhs_member->Alias_class()->Sort() == IP_ACR_BOTTOM_TYPE) {
      IP_ALIAS_CLASS_REP *lhs_deref_data_acr =
	New_alias_class(IP_ACR_BOTTOM_TYPE, *New_alias_class_member());
      IP_ALIAS_CLASS_REP *lhs_deref_code_acr =
	New_alias_class(IP_ACR_BOTTOM_TYPE, *New_alias_class_member());
      // How exactly do we want to set up the interface to Settype?
      lhs_member->Alias_class()->
	Settype(IP_AC_REF_TYPE_REP(lhs_deref_data_acr->Representative(),
				   lhs_deref_code_acr->Representative()),
		Pool(), Tracing());
    }
    return lhs_member;
  }
  else {
    FmtAssert(FALSE, ("AC::Classify_lhs_of_store: Other stores "
		      "not handled"));
    return NULL;	// Compiler palliative
  }
}

WN *
IP_ALIAS_CLASSIFICATION::Handle_assignment(WN *const stmt)
{
  IP_ALIAS_CLASS_MEMBER *lhs_member = Classify_lhs_of_store(stmt);

  // Now deal with the RHS
  WN *rhs = WN_kid0(stmt);

  Classify_deref_of_expr(lhs_member, rhs, FALSE);

  if (WN_operator(stmt) == OPR_MSTORE) {
    // Classify the size expression.
    Classify_deref_of_expr(NULL, WN_kid2(stmt), FALSE);
  }

  // If this assignment stores a value that will be returned to
  // callers of the current function, merge the lhs class with the
  // appropriate return class for the current function.
  if (Stmt_stores_return_value_to_caller(stmt)) {
    ST_IDX pu_st_idx = ST_st_idx(Scope_tab[CURRENT_SYMTAB].st);
    // Use direct lookup rather than Function_descriptor() in the
    // following line so we can make sure we catch and barf on the
    // case where the current function has no entry.
    Is_True(ST_base_idx(St_Table[pu_st_idx]) == pu_st_idx,
	    ("Handle_assignment: Current PU must not have based address"));
    Is_True(ST_IDX_level(pu_st_idx) == GLOBAL_SYMTAB,
	    ("Handle_assignment: PU must be global symbol"));
    IP_ALIAS_CLASS_REP *current_func_acr =
      _base_id_map[Base_id(&St_Table[pu_st_idx],
			   (INT64) 0)]->Base_member().Alias_class();
    Conditional_join(current_func_acr->Signature().Return_class()->
		       Data_class_pointed_to(),
		     lhs_member->Alias_class()->Data_class_pointed_to());
    Conditional_join(current_func_acr->Signature().Return_class()->
		       Func_class_pointed_to(),
		     lhs_member->Alias_class()->Func_class_pointed_to());
  }

  if (_verbose && Tracing()) {
    fprintf(TFile, "  after handling assignment:\n");
    if (WN_operator(stmt) == OPR_ISTORE ||
	WN_operator(stmt) == OPR_MSTORE) {
      fprintf(TFile, "    (M/I)STORE placed in ");
      ((IP_ALIAS_CLASS_MEMBER *) WN_MAP_Get(Indir_classification_map(),
					    stmt))->
	Alias_class()->Print(TFile, Global_data_class());
    }
    else if (WN_operator(stmt) == OPR_STID) {
      fprintf(TFile, "    STID placed in ");
      Class_of_base_id(WN_base_id(stmt))->Print(TFile, Global_data_class());
    }
    Print(TFile);
#if Is_True_On
    print_table();
#endif
  }

  return WN_next(stmt);
}

WN *
IP_ALIAS_CLASSIFICATION::Handle_return_val(WN *const stmt)
{
  ST *pu_st = Scope_tab[CURRENT_SYMTAB].st;
  IP_ALIAS_CLASS_REP *pu_acr =
    _base_id_map[Base_id(pu_st, 0ll)]->Base_member().Alias_class();
  IP_ALIAS_CLASS_MEMBER *return_ac_member = pu_acr->Signature().Return_class_member();

  Classify_deref_of_expr(return_ac_member, WN_kid0(stmt), FALSE);

  if (_verbose && Tracing()) {
    fprintf(TFile, "  after handling return value:\n");
    fprintf(TFile, "    RETURN_VAL placed in ");
    pu_acr->Signature().Return_class()->Print(TFile, Global_data_class());
    Print(TFile);
#if Is_True_On
    print_table();
#endif
  }

  return WN_next(stmt);
}

BOOL
IP_ALIAS_CLASSIFICATION::Stmt_stores_return_value_from_callee(const WN *const stmt)
{
  WN *rhs = WN_kid0(stmt);

  return (OPCODE_is_store(WN_opcode(stmt)) &&
	  (WN_operator(rhs) == OPR_LDID) &&
	  (ST_sclass(WN_st(rhs)) == SCLASS_REG) &&
	  Preg_Is_Dedicated(WN_offset(rhs)));
}

BOOL
IP_ALIAS_CLASSIFICATION::Stmt_stores_return_value_to_caller(const WN *const stmt)
{
  // Assume that any store to a dedicated PREG is a store of a return
  // value computed by this function.
  return ((WN_operator(stmt) == OPR_STID) &&
	  (ST_sclass(WN_st(stmt)) == SCLASS_REG) &&
	  Preg_Is_Dedicated(WN_offset(stmt)));
}

/* ARGSUSED */
BOOL
IP_ALIAS_CLASSIFICATION::Uses_no_return_value(const WN *const stmt)
{
  return TRUE;
}

BOOL
IP_ALIAS_CLASSIFICATION::Callee_saves_no_parms(const WN *const call_wn)
{
  if (WN_Call_Never_Return(call_wn)) {
    DevWarn("IP_ALIAS_CLASSIFICATION: I see a call that never returns");
    return TRUE;
  }
  else if (Callee_frees_memory(call_wn)) {
    return TRUE;
  }
  else {
    return Callee_returns_new_memory(call_wn);
  }
}

BOOL
IP_ALIAS_CLASSIFICATION::Callee_returns_new_memory(const WN *const call_wn)
{
#ifndef KEY
  return WN_Call_Does_Mem_Alloc(call_wn);
#else
  if (WN_Call_Does_Mem_Alloc(call_wn))
    return TRUE;
  if (WN_operator(call_wn) == OPR_CALL) {
    return Callee_returns_new_memory(WN_st(call_wn));
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

/* ARGSUSED */
BOOL
IP_ALIAS_CLASSIFICATION::Callee_returns_new_memory(const ST *const st)
{
  Is_True(ST_class(st) == CLASS_FUNC, ("invalid st class"));
  if (PU_has_attr_malloc (Pu_Table[ST_pu(st)]))
    return TRUE;

#ifdef KEY
  // Cheap hack for now, to test performance. This should be based on
  // some real mechanism in the future instead of cheesebag hacks.
  if ((strcmp("malloc", ST_name(st)) == 0 ||
       strcmp("alloca", ST_name(st)) == 0 ||
       strcmp("calloc", ST_name(st)) == 0 ||
       strcmp("_F90_ALLOCATE", ST_name(st)) == 0) &&
      ST_sclass(st) == SCLASS_EXTERN) {
    return TRUE;
  }
  else {
    return FALSE;
  }
#else
  return FALSE;
#endif
}

BOOL
IP_ALIAS_CLASSIFICATION::Callee_frees_memory(const WN *const call_wn)
{
  return WN_Call_Does_Mem_Free(call_wn);
}

/* ARGSUSED */
BOOL
IP_ALIAS_CLASSIFICATION::Callee_frees_memory(const ST *const st)
{
  return FALSE;
}

IP_ALIAS_CLASS_MEMBER *
IP_ALIAS_CLASSIFICATION::Incorporate_call_and_parm_flags(      IP_ALIAS_CLASS_MEMBER *      func_member,
							 const WN                    *const call_wn)
{
  if (WN_Call_Pure(call_wn)) {
    // Make best-case assumptions: The return value is a copy of all
    // the arguments. To denote this situation, we put the fresh
    // return class in the pending set for each of the fresh parameter
    // classes. If any parameter class ever points, the return class
    // needs to point to the same thing.
    OPERATOR opr = WN_operator(call_wn);
    UINT     n_parms;

    if (opr == OPR_ICALL) {
      n_parms = WN_kid_count(call_wn) - 1;
    }
    else {
      n_parms = WN_kid_count(call_wn);
    }

    func_member = New_alias_class_member(call_wn);
    IP_ALIAS_CLASS_REP *pure_func_acr =
      New_alias_class(IP_ACR_LAMBDA_TYPE, *func_member);
    IP_AC_LAMBDA_TYPE_REP &pure_func_desc = pure_func_acr->Signature();

    IP_ALIAS_CLASS_MEMBER *const return_member = New_alias_class_member();
    IP_ALIAS_CLASS_REP    *const return_class  =
      New_alias_class(IP_ACR_REF_TYPE, *return_member);
    pure_func_desc.Returns() = return_member;

    for (UINT i = 1; i < n_parms; ++i) {
      IP_ALIAS_CLASS_MEMBER *dummy_arg = New_alias_class_member();
      IP_ALIAS_CLASS_REP    *arg_class =
	New_alias_class(IP_ACR_REF_TYPE, *dummy_arg);
      arg_class->Add_pending(return_class, Tracing());
      pure_func_desc.fixed_args.push_back(dummy_arg);
    }
    IP_ALIAS_CLASS_MEMBER *dummy_arg = New_alias_class_member();
    IP_ALIAS_CLASS_REP    *arg_class =
      New_alias_class(IP_ACR_REF_TYPE, *dummy_arg);
    arg_class->Add_pending(return_class, Tracing());
    pure_func_desc.remaining_args = dummy_arg;
    pure_func_desc.arity_established = TRUE;
    return func_member;
  }
  BOOL callee_returns_new_memory = Callee_returns_new_memory(call_wn);
  BOOL callee_frees_memory       = Callee_frees_memory(call_wn);

  BOOL require_new_func_desc = (callee_returns_new_memory ||
				callee_frees_memory);
  if (require_new_func_desc) {
    IP_ALIAS_CLASS_MEMBER *new_func_member = New_alias_class_member(call_wn);
    IP_ALIAS_CLASS_REP *func_acr =
      New_alias_class(IP_ACR_LAMBDA_TYPE, *new_func_member);
    IP_AC_LAMBDA_TYPE_REP &new_func_desc = func_acr->Signature();
    if (callee_returns_new_memory) {
      new_func_desc.returns_new_memory = TRUE;
      new_func_desc.Set_arities(0, 1, this);
    }
    else {
      new_func_desc = func_member->Alias_class()->Signature();
    }
    if (callee_frees_memory) {
      new_func_desc.frees_memory = TRUE;
    }
    return new_func_member;
  }
  else {
    return func_member;
  }
}

// A call amounts to assignment of parameters to formals followed by
// assignment of return values to wherever they get stored. Side
// effects of the function are taken into account when the code for
// the callee is analyzed.

// TODO: A call to a function whose definition is a weak symbol could
// be a transfer to code we can't see.

WN *
IP_ALIAS_CLASSIFICATION::Handle_call(WN *const call_wn)
{
  OPCODE    opc     = WN_opcode(call_wn);
  OPERATOR  opr     = OPCODE_operator(opc);

  IP_ALIAS_CLASS_MEMBER *callee_member;
  UINT  n_parms;

  if (opr == OPR_ICALL) {
    n_parms = WN_kid_count(call_wn) - 1;
    IP_ALIAS_CLASS_MEMBER *callee_address_member =
      New_alias_class_member(WN_kid(call_wn,
				    WN_kid_count(call_wn) - 1));
    (void) New_alias_class(IP_ACR_REF_TYPE, *callee_address_member);
    Classify_deref_of_expr(callee_address_member,
			   WN_kid(call_wn,
				  WN_kid_count(call_wn) - 1),
			   TRUE);
    callee_member =
      callee_address_member->Alias_class()->Ref().Code_member();
  }
  else {
    n_parms = WN_kid_count(call_wn);
    if (opr == OPR_CALL) {
      // Direct call. We get the callee descriptor from the (base of
      // the) ST specified in the call WN.
      ST_IDX callee_st_idx = WN_st_idx(call_wn);
#ifndef KEY // Bug 8433
      Is_True(ST_base_idx(St_Table[callee_st_idx]) == callee_st_idx,
	      ("AC:Handle_call: Called routine must not have based address"));
#endif
      callee_member = Class_of_base_id(WN_base_id(call_wn))->Representative();
    }
    else {
      Is_True(opr == OPR_INTRINSIC_CALL,
	      ("AC::Handle_call: Can handle only calls"));
      // What will we do with intrinsic calls?
      // Here we have nothing but flag bits to go on.
      //
      // If the intrinsic call is a pure function, it can't do
      // anything but essentially copy each of its arguments to its
      // return value. Such things are taken into account in the call
      // to Incorporate_call_and_parm_flags(), below.
      callee_member = _member_of_global_func_class;
    }
  }

  IP_ALIAS_CLASS_REP *callee_class = callee_member->Alias_class();

  // Here is where we catch people trying to do "call X" where X is a
  // data item.
  //
  // TODO: This could still be a problem, because we don't want the
  // compiler to assert when the user says:
  // int x;
  // ((void(*)()) &x)();
  //
  // Currently no assertion happens because we treat ICALL(&x) as
  // unique_tmp <-- &x; ICALL(unique_tmp);
  // where unique_tmp starts out with type REF(bot x bot). Because of
  // this, the assertion passes, since what we see here is the
  // Func_class_pointed_to() of the class containing unique_tmp. That
  // Func_class_pointed_to() has is of sort BOTTOM.
  //
  Is_True(callee_class->Sort() == IP_ACR_BOTTOM_TYPE ||
	  callee_class->Sort() == IP_ACR_LAMBDA_TYPE,
	  ("Handle_call: illegal callee class type"));
    
  if (callee_class->Sort() == IP_ACR_BOTTOM_TYPE) {
    callee_class->_type_info._sort = IP_ACR_LAMBDA_TYPE;
    callee_class->_type_info._signature =
      CXX_NEW(IP_AC_LAMBDA_TYPE_REP(Pool()), Pool());
    callee_class->Signature().Set_arities(n_parms, 1, this);
    callee_class->Process_pending(callee_member, Pool(), Tracing());
    callee_class = callee_member->Alias_class();
  }

  Is_True(callee_class->Signature().arity_established,
	  ("Handle_call: Arity must be established at LAMBDA time"));

  // Now we have the basic callee descriptor. Produce a modified copy
  // of the descriptor according to the flag bits associated with this
  // particular call site, and use the modified copy for our analysis.
  callee_member =
    Incorporate_call_and_parm_flags(callee_member, call_wn);

  // Now for each actual parameter, merge the class pointed to by the
  // parameter with the class pointed to by the corresponding formal,
  // as if the formal were on the left-hand side of an assignment with
  // the actual as its right-hand side.
  for (UINT i = 0; i < n_parms; ++i) {
    WN *parm_wn = WN_kid(call_wn, i);
    Is_True(WN_operator(parm_wn) == OPR_PARM,
	    ("AC::Handle_call: Can't handle non-PARM parameter"));

    IP_ALIAS_CLASS_MEMBER *formal_member;
    if (callee_member->Alias_class()->Signature().returns_new_memory ||
	callee_member->Alias_class()->Signature().frees_memory) {
      // Special treatment for memory allocation/deallocation
      // operations: no merge for them since we guarantee that they
      // don't change visible points-to relationships.
      formal_member = NULL;
    }
    else {
      formal_member =
	callee_member->Alias_class()->Signature().Argument_member(i);
    }

    Classify_deref_of_expr(formal_member, parm_wn, FALSE);
  }

  IP_ALIAS_CLASS_MEMBER *return_class_member;
  if (callee_member->Alias_class()->Signature().returns_new_memory) {
    // Here we try to take advantage of the fact that different call
    // sites return different memory blocks. Unfortunately this code
    // is in place largely for show at the moment because the return
    // value from every memory allocator call (and indeed every other
    // call) is placed in a common PREG, so there is a variable in
    // each PU that points to *all* the memory blocks returned by
    // memory allocation calls in that PU. As a result, the classes
    // for all the allocated blocks get merged together regardless.
    //
    // It is no longer so bad as all that, since PREG renumbering is
    // now implemented in preopt's DCE and emitter.
    return_class_member =
      callee_member->Alias_class()->
      Signature().Memory_allocator_return_class_member(this);
  }
  else {
    return_class_member = callee_member->Alias_class()->Signature().Returns();
  }

  WN *stmt = WN_next(call_wn);
  while (stmt != NULL && Stmt_stores_return_value_from_callee(stmt)) {
    if (Tracing()) {
      fprintf(TFile, "Store of return value:\n");
      fdump_tree(TFile, stmt);
    }
    IP_ALIAS_CLASS_MEMBER *lhs_member = Classify_lhs_of_store(stmt);;
    IP_ALIAS_CLASS_MEMBER *rhs_member = return_class_member;
    Conditional_join(lhs_member->Alias_class()->Data_class_pointed_to(),
		     rhs_member->Alias_class()->Data_class_pointed_to());
    Conditional_join(lhs_member->Alias_class()->Func_class_pointed_to(),
		     rhs_member->Alias_class()->Func_class_pointed_to());
    stmt = WN_next(stmt);
  }
  Is_True(Uses_no_return_value(stmt),
	  ("IP_ALIAS_CLASSIFICATION: General use of return value illegal"));

  if (_verbose && Tracing()) {
    fprintf(TFile, "  after handling call:\n");
    Print(TFile);
#if Is_True_On
    print_table();
#endif
  }
  return stmt;
}

void
IP_ALIAS_CLASSIFICATION::Handle_function_definition(WN *entry_wn)
{
  FmtAssert(WN_opcode(entry_wn) == OPC_FUNC_ENTRY,
	    ("IP_ALIAS_CLASSIFICATION: FUNC_ENTRY expected"));
  // Go through the IDNAME argument list and model "virtual
  // assignments" of parameters (from call sites) to formal
  // arguments. In some varargs cases, it seems like we will get an
  // arity mismatch here. TODO: What should we do about this?
  IP_ALIAS_CLASS_REP *func_class = Class_of_base_id(WN_base_id(entry_wn));

  Is_True(func_class->Sort() == IP_ACR_BOTTOM_TYPE ||
	  func_class->Sort() == IP_ACR_LAMBDA_TYPE,
	  ("Handle_call: illegal function class type"));
    
  IP_ALIAS_CLASS_MEMBER *func_class_member = func_class->Representative();

  if (func_class->Sort() == IP_ACR_BOTTOM_TYPE) {
    func_class->_type_info._sort = IP_ACR_LAMBDA_TYPE;
    func_class->_type_info._signature =
      CXX_NEW(IP_AC_LAMBDA_TYPE_REP(Pool()), Pool());
    func_class->Signature().Set_arities(WN_kid_count(entry_wn) - 3,
					1, this);
    func_class->Process_pending(func_class_member, Pool(), Tracing());
    func_class = func_class_member->Alias_class();
  }

  Is_True(func_class->Signature().arity_established,
	  ("Handle_function_definition: Arity must be established at "
	   "LAMBDA time"));

  for (UINT i = 0; i < WN_kid_count(entry_wn) - 3; ++i) {
    WN *kid_i = WN_kid(entry_wn, i);
    FmtAssert(WN_opcode(kid_i) == OPC_IDNAME,
	      ("IP_ALIAS_CLASSIFICATION: Parameter IDNAME expected"));
    IP_ALIAS_CLASS_MEMBER *lhs_member = Classify_lhs_of_store(kid_i);
    IP_ALIAS_CLASS_MEMBER *rhs_member =
      func_class_member->Alias_class()->Signature().Argument_member(i);
    if (ST_sclass(WN_st(kid_i)) == SCLASS_FORMAL_REF) {
      // Formal ref arguments are strange. Think about it.
      // Notice that the RHS (actual) function pointee gets ignored
      // here.
      //
      // In Steensgaard's paper, the following joins are
      // unconditional. Whether they're conditional or not should
      // change nothing either way, since the corresponding join of
      // actuals to formals at call sites is conditional.
      Is_True(lhs_member->Alias_class()->Sort() == IP_ACR_REF_TYPE,
	      ("Handle_func_entry: FORMAL_REF IDNAME kid must have "
	       "REF type"));
      Conditional_join(lhs_member->Alias_class(),
		       rhs_member->Alias_class()->Data_class_pointed_to());
    }
    else {
      // In Steensgaard's paper, the following joins are
      // unconditional. Whether they're conditional or not should
      // change nothing either way, since the corresponding join of
      // actuals to formals at call sites is conditional.
      Conditional_join(lhs_member->Alias_class()->Data_class_pointed_to(),
		       rhs_member->Alias_class()->Data_class_pointed_to());
      Conditional_join(lhs_member->Alias_class()->Func_class_pointed_to(),
		       rhs_member->Alias_class()->Func_class_pointed_to());
    }
  }

  if (Tracing()) {
    fprintf(TFile, "  after handling function arguments:\n");
    Print(TFile);
#if Is_True_On
    print_table();
#endif
  }

  // Kids 0 - WN_kid_count(entry_wn) - 4 are IDNAME leaves
  // corresponding to formal arguments to this function.  We skip kids
  // WN_kid_count(entry_wn) - 3 and WN_kid_count(entry_wn) - 2. These
  // are two pragma blocks, one to govern compilation of this function
  // and the other to govern compilation of call sites that may refer
  // to this function. Pragmas have no alias behavior per se.

  // Values returned to callers of this function will be merged with
  // the "returns" set during classification of the
  // body. Specifically, this happens in Handle_assignment.
  WN *body_wn = WN_kid(entry_wn, WN_kid_count(entry_wn) - 1);
  Classify_wn_and_kids(body_wn);
}

void
IP_ALIAS_CLASSIFICATION::Finalize_ac_map_wn(WN *wn)
{
  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  if (Tracing()) {
    fprintf(TFile, "Finalizing : ");
    fdump_wn(TFile, wn);
    fflush(TFile);
  }

  if (OPCODE_is_call(opc)) {
    // Do call stuff, which currently is nothing.
  }
  else if (OPCODE_is_load(opc) ||
	   OPCODE_is_store(opc) ||
	   Is_fortran_reference_parm(wn) ||
	   (opr == OPR_LDA &&
	    Is_LDA_of_variable(wn))) {
    if (Tracing()) {
      fprintf(TFile, "   placed in ");
    }
    if (opr == OPR_LDID || opr == OPR_STID || opr == OPR_LDA
#ifdef KEY
	|| opr == OPR_LDBITS
#endif
	) {
      // Direct memop; translation through base_id.
      IDTYPE class_id = Class_of_base_id(WN_base_id(wn))->Id();
      WN_MAP32_Set(Memop_classification_map(), wn, class_id);
      if (Tracing()) {
	Class_of_base_id(WN_base_id(wn))->Print(TFile, Global_data_class());
      }
    }
    else {
      // Indirect memop. Translation through existing map entry that
      // gives the alias class member corresponding to the WN.
      IP_ALIAS_CLASS_MEMBER *acm =
	(IP_ALIAS_CLASS_MEMBER *) WN_MAP_Get(Indir_classification_map(), wn);
      if (Tracing()) {
	fprintf(TFile, "Got %p from indir map %u on\n", acm,
		Indir_classification_map());
	fdump_tree(TFile, wn);
      }
      IP_ALIAS_CLASS_REP *acr = acm->Alias_class();

      if (Tracing()) {
	acr->Print(TFile, Global_data_class());
      }

      WN_MAP32_Set(Memop_classification_map(), wn, acr->Id());
    }
  }
}

void
IP_ALIAS_CLASSIFICATION::Finalize_ac_map(WN *const wn)
{
  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

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
	  !Stmt_stores_return_value_from_callee(wn) ||
	  i != rhs_idx) {
	Finalize_ac_map(WN_kid(wn, i));
      }
    }
    Finalize_ac_map_wn(wn);
  }
}

IDTYPE
IP_ALIAS_CLASSIFICATION::Alias_class(IDTYPE base_id) const
{
  if (_memops_classified) {
    return Class_of_base_id(base_id)->Id();
  }
  else {
    return PESSIMISTIC_AC_ID;
  }
}

void
IP_ALIAS_CLASSIFICATION::Merge_initial_value_classes(IDTYPE    lhs_base_id,
						     INITV_IDX val)
{
  while (val) {
    const INITV &initv = Initv_Table[val];
    if (INITV_kind(initv) == INITVKIND_SYMOFF) {
      ST_IDX rhs_base_st_idx;
      INT64  rhs_base_offset = (INT64) INITV_ofst(initv);
      Find_declared_base_and_offset(INITV_st(initv),
				    rhs_base_st_idx,
				    rhs_base_offset);
      ST *rhs_base_st = &St_Table[rhs_base_st_idx];
      IDTYPE rhs_id = Base_id(rhs_base_st, rhs_base_offset);

      IP_ALIAS_CLASS_MEMBER *lhs_deref_member;
      if (Data_item(rhs_base_st)) {
	lhs_deref_member =
	  Class_of_base_id(lhs_base_id)->Ref().Data_member();
      }
      else {
	Is_True(Code_item(rhs_base_st),
		("Non-data item must be code item"));
	lhs_deref_member =
	  Class_of_base_id(lhs_base_id)->Ref().Code_member();
      }
      lhs_deref_member->Alias_class()->Join(*Class_of_base_id(rhs_id),
					    Pool(), Tracing());
    }
    else if (INITV_kind(initv) == INITVKIND_BLOCK) {
      Merge_initial_value_classes(lhs_base_id, INITV_blk(initv));
    }
    val = INITV_next(initv);
  }
}

BOOL
IP_ALIAS_CLASSIFICATION::Can_be_referred_to_by_code(const ST_IDX st_idx)
{
  ST *st = &St_Table[st_idx];
  // Items with EH_REGION_SUPP storage class cannot be referred to by
  // the code we're compiling, so they need not have their alias
  // behavior classified. They are strictly for reference by the
  // runtime system that supports exception handling.
  //
  // TODO: Should probably add SCLASS_THREAD_PRIVATE_FUNCS here after
  // 7.3 beta is released.
  return ((ST_sclass(st) != SCLASS_EH_REGION_SUPP) &&
	  (ST_sclass(st) != SCLASS_CPLINIT));
}

void
IP_ALIAS_CLASSIFICATION::Classify_inito(const INITO *const inito)
{
  ST_IDX inito_st_idx = INITO_st_idx(*inito);
  if (Can_be_referred_to_by_code(inito_st_idx)) {
    ST_IDX base_st_idx;
    INT64  base_offset = (INT64) 0;
    Find_declared_base_and_offset(inito_st_idx,
				  base_st_idx,
				  base_offset);
    IDTYPE id = Base_id(&St_Table[base_st_idx], base_offset);

    Merge_initial_value_classes(id,
				INITO_val(*inito));
  }
}

struct CLASSIFY_INITIALIZED_DATA {
  IP_ALIAS_CLASSIFICATION *const _ip_alias_classification;

  CLASSIFY_INITIALIZED_DATA(IP_ALIAS_CLASSIFICATION *const ip_alias_classification) :
    _ip_alias_classification(ip_alias_classification)
      { }

  void operator() (UINT32, const INITO *const inito) const {
    _ip_alias_classification->Classify_inito(inito);
  }
};

// ======================================================================
// Public interface routines for clients.
// ======================================================================
void
IP_ALIAS_CLASSIFICATION::Init_maps(void)
{
  _st_id_to_base_id_map.Init();
  Preg_id_to_base_id_map().Init();
  _maps_initialized = TRUE;
}

void
IP_ALIAS_CLASSIFICATION::Classify_initialized_data(INITO_TAB *inito_tab)
{
  For_all(*inito_tab, CLASSIFY_INITIALIZED_DATA(this));
}

void
IP_ALIAS_CLASSIFICATION::Classify_memops(WN *entry_wn)
{
  if (Tracing()) {
    fprintf(TFile, "%sBeginning alias classification\n%s",
	    DBar, DBar);
    fdump_tree(TFile, entry_wn);
  }

  if (Tracing()) {
    fprintf(TFile, "----------\nBefore program analysis:\n");
    Print(TFile);
    fprintf(TFile, "----------\n");
  }

  // Now that we write the alias classification map to a file, we
  // have a fixed map number defined in wn_map.h, so we don't call
  // WN_MAP32_Create here. Instead, _memop_classification_map is copy
  // constructed prior to the IP_ALIAS_CLASSIFICATION constructor.
  // _memop_classification_map = WN_MAP32_Create(Pool());

  // Now that we have a single indir_classification_map for all PU's,
  // we create that map prior to the IP_ALIAS_CLASSIFICATION
  // constructor.
  // _indir_classification_map = WN_MAP_Create(Pool());

  if (Tracing()) {
    fprintf(TFile, "Current_Map_Tab == %p\n", Current_Map_Tab);
    fprintf(TFile, "indir map is %u\n", _indir_classification_map);
  }

  Handle_function_definition(entry_wn);
}

void
IP_ALIAS_CLASSIFICATION::Finalize_memops(WN *entry_wn)
{
  if (Tracing()) {
    fprintf(TFile, "----------\nAfter program analysis:\n");
    Print(TFile);
    fprintf(TFile, "----------\n");
  }

  Finalize_ac_map(entry_wn);
  _memops_classified = TRUE;
}

IDTYPE
IP_ALIAS_CLASSIFICATION::Alias_class(const WN *wn) const
{
  Is_True(OPCODE_is_store(WN_opcode(wn)) ||
	  OPCODE_is_load(WN_opcode(wn)) ||
	  WN_operator(wn) == OPR_PARM,
	  ("IP_ALIAS_CLASSIFICATION: WN must be memop"));

  if (_memops_classified) {
    FmtAssert(_mem_pool_valid,
	      ("IP_ALIAS_CLASSIFICATION: Our memory is gone."));
    return WN_MAP32_Get(Memop_classification_map(), wn);
  }
  else {
    return PESSIMISTIC_AC_ID;
  }
}

void
IP_ALIAS_CLASSIFICATION::Release_resources(void)
{
  DevWarn("Recycled %u of %d ACR's",
	  IP_ALIAS_CLASS_REP::_recycled_acr_nodes,
	  IP_ALIAS_CLASS_REP::_last_id_used);

  _base_id_map.Free_array();
  if (_maps_initialized) {
    (&_preg_id_to_base_id_map)->~ID_MAP();
    (&_st_id_to_base_id_map)->~ID_MAP();
  }

  MEM_POOL_Pop(_pool);
  _mem_pool_valid = FALSE;
  _pool = NULL;
}

void
IP_ALIAS_CLASSIFICATION::Print(FILE *fp) const
{
  // Print the storage-shape graph with pending lists
  // For each base_id, print the ID of the graph node that
  // characterizes it.
  fprintf(fp, "Global data class: ");
  Global_data_class()->Print(fp);
  fprintf(fp, "Global code class: ");
  Global_func_class()->Print(fp);
  for (INT i = 1; i <= _base_id_map.Lastidx(); i++) {
    fprintf(fp, "Class containing base ID %d is ", i);
    _base_id_map[i]->Base_member().Alias_class()->Print(fp,
							Global_data_class());
  }
}
