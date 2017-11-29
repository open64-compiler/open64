/*
 * Copyright (C) 2009-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007, 2008. PathScale, LLC. All Rights Reserved.
//
// Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it is
// free of the rightful claim of any third person regarding infringement
// or the like.  Any license provided herein, whether implied or
// otherwise, applies only to this software file.  Patent licenses, if
// any, provided herein do not apply to combinations of this program with
// other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write the Free Software Foundation, Inc., 59
// Temple Place - Suite 330, Boston MA 02111-1307, USA.
////////////////////////////////////////////////////////////////////////////

#include <stdint.h>
#include <math.h>

#include "defs.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "config_ipa.h"

#include "ipa_cg.h"
#include "ipo_parent.h"
#include "ipa_struct_opt.h"
#include "tracing.h"
#include "ipa_option.h" // Trace_IPA
#include "lwn_util.h"

// ======================================================================
// Implements a form of structure splitting.
// ======================================================================
//
// Assumptions:
// ------------
// Let us call TY to be the 'struct' type on which we are performing the
// optimization.

// When a function returns an object of type pointer-to-TY where TY is 
// a type we are performing the optimization on, the function must be an
// allocation function like malloc, calloc, realloc.
//
// TY should have 2 fields only (being relaxed)
//
// An object of pointer-to-TY should not be present as a function parameter.
//
// It should be a local variable (being relaxed)
//
// An object of pointer-to-TY may appear as a field in another 'struct'
// type parent_TY, if there is only one such parent_TY, and parent_TY
// contains only one such field.

mUINT32 candidate_ty_idx = 0;
mUINT32 candidate_fld_id = 0;
mUINT32 Struct_split_candidate_index = 0;
mUINT32 Struct_update_index = 0;

static WN_MAP PU_Parent_Map = 0;
static WN_MAP_TAB * PU_Map_Tab = NULL;

// TODO:
//  * map should be always updated whenever the whirl is changed.
//  * change assertions to debug assertions later.
//  * we should find a better way to detect memory management routines,
//    like generating intrinsics for them and detecting these intrinsics.

ST * fld_st[10]; // TODO: dynamic memory allocation
TY_IDX Struct_split_types[20]; // TODO: dynamic memory allocation

WN_OFFSET preg_id = 0;

struct FLD_MAP
{
  mUINT32 old_field_id; // not required
  mUINT32 new_field_id;
  WN_OFFSET old_ofst; // unused
  WN_OFFSET new_ofst; // unused
};

static FLD_MAP * field_map_info;

__gnu_cxx::hash_map<ST_IDX, ST*> singleDefHeapAllocedGlbls;

// expr is a size expression, return the WN for the constant
// part of the expr that needs update.
WN * size_wn (WN * expr)
{
  WN * wn = NULL;
  if (WN_operator(expr) == OPR_INTCONST)
    wn = expr;
  else if (WN_operator(expr) == OPR_MPY)
    wn = WN_kid1(expr);
  else Fail_FmtAssertion ("Unrecognized expression");

  FmtAssert (WN_operator(wn) == OPR_INTCONST, ("Constant size expected"));
  FmtAssert (WN_const_val(wn) >= TY_size(Ty_tab[candidate_ty_idx]),
              ("allocation constant cannot be less than type size"));
  return wn;
}

// Given a memory allocation function call, get the argument that has the
// size expression. Returns size_wn() of it.
WN * size_expr (WN * call)
{
  FmtAssert (WN_operator(call) == OPR_CALL, ("Call stmt expected"));
  FmtAssert (!strcmp(ST_name(WN_st(call)), "malloc") ||
             !strcmp(ST_name(WN_st(call)), "realloc") ||
             !strcmp(ST_name(WN_st(call)), "calloc"),
             ("Unexpected function call"));

  if (!strcmp(ST_name(WN_st(call)), "malloc"))
    return size_wn(WN_kid0(WN_kid0(call)));
  else
    return size_wn(WN_kid0(WN_kid1(call)));
}

// Adjusts first argument to realloc() if necessary.
void fixup_realloc_pointer (WN * call)
{
  Is_True (WN_operator(call) == OPR_CALL, ("Expected call stmt"));
  WN * ptr = WN_kid0(WN_kid0(call));

  if (WN_operator(ptr) == OPR_ILOAD && WN_operator(WN_kid0(ptr)) == OPR_LDID &&
      TY_IDX_index(WN_ty(ptr)) == Struct_update_index)
  {
    UINT field_id = WN_field_id(ptr);
    FmtAssert (field_id == candidate_fld_id, ("Unexpected field id in iload"));
    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8, ++field_id, cur_field_id);
    FmtAssert (!fld.Is_Null(), ("Field not found"));
    WN_set_field_id(ptr, field_id);
    WN_offset(ptr) = FLD_ofst(fld);
  }
}

// call to memory allocation routine
void handle_function_return (WN * block, WN * wn)
{
  FmtAssert (WN_operator(wn) == OPR_STID, ("Stid expected"));

  // must be an allocation routine call
  WN * call = WN_prev(wn);
  FmtAssert (call && WN_operator(call) == OPR_CALL, ("Call stmt expected"));

  // clone the call-stmt before it is updated
  WN * new_call = WN_COPY_Tree (call);

  WN * size = size_expr(call);

  INT num_elements = WN_const_val(size) / TY_size(Ty_tab[candidate_ty_idx]);

  TY_IDX type = TY_IDX_ZERO;
  TY_IDX ptr_ty[10]; // TODO: dynamic mem alloc
  for (INT i=0; i<Struct_split_count; i++)
  {
      type = Struct_split_types[i];
      ptr_ty[i] = Make_Pointer_Type(type);
      fld_st[i] = Gen_Temp_Symbol (ptr_ty[i], "fld");
  }

  // allocation for first field
  WN_const_val(size) = num_elements * TY_size(Struct_split_types[0]);
  // wn is stid
  {
    // Sanity check, and track ST being replaced.
    ST * sym = WN_st(wn);
    FmtAssert (ST_class(sym) == CLASS_PREG,
               ("handle_function_return: Expected return store into preg"));
    preg_id = WN_offset(wn);
  }
  WN_st_idx(wn) = ST_st_idx(fld_st[0]);
  WN_offset(wn) = 0;
  WN_set_ty(wn, ptr_ty[0]);
  WN_set_ty(WN_kid0(wn), ptr_ty[0]);

  // allocation for second field
  if (!strcmp(ST_name(WN_st(new_call)), "realloc"))
  {
    // Fix up first argument of realloc for alloc of second field
    fixup_realloc_pointer(new_call);
  }

  WN * last_alloc = new_call;
  for (INT i=1; i<Struct_split_count; i++)
  {
    if (i < Struct_split_count-1)
      new_call = WN_COPY_Tree (new_call);
    size = size_expr(last_alloc);
    WN_const_val(size) = num_elements * TY_size(Struct_split_types[i]);
    WN * val = WN_Ldid(WN_desc(WN_kid0(wn)), -1, Return_Val_Preg, ptr_ty[i]);
    WN * stid = WN_Stid(WN_rtype(val), 0, fld_st[i], ST_type(fld_st[i]), val);
    WN_INSERT_BlockLast(block, last_alloc);
    WN_INSERT_BlockLast(block, stid);
    last_alloc = new_call;
  }
}

// Handle assignment statement
void handle_assignment (WN * block, WN * wn)
{
  FmtAssert (WN_operator(wn) == OPR_STID, ("Expected assignment stmt"));

  if (IPA_Enable_Struct_Opt == 2 &&
      TY_kind(WN_ty(wn)) == KIND_POINTER &&
      TY_IDX_index(TY_pointed(WN_ty(wn))) == candidate_ty_idx)
  { // pointer assignment
    // First, some sanity checks
    WN * kid = WN_kid0(wn);
    // Must be ldid of a preg
    Is_True (WN_operator(kid) == OPR_LDID &&
             ST_class(WN_st(kid)) == CLASS_PREG, ("NYI"));

    TY_IDX ptr_ty0 = Make_Pointer_Type(Struct_field_layout[0].u.new_ty);
    WN_set_ty(kid, ptr_ty0);
    WN_st_idx(kid) = ST_st_idx(fld_st[0]);
    WN_offset(kid) = 0; // reset offset
    WN_set_ty(wn, ptr_ty0);
    WN_st_idx(wn) = Struct_field_layout[0].st_idx;

    // Generate rest of assignments
    for (INT i=1; i<Struct_split_count; i++)
    {
      TY_IDX ptr_ty = Make_Pointer_Type(Struct_field_layout[i].u.new_ty);
      WN * stid = WN_Stid(WN_desc(wn),
                          0,
                          &St_Table[Struct_field_layout[i].st_idx],
                          ptr_ty,
                          WN_Ldid(WN_desc(wn), 0, fld_st[i], ptr_ty));
      WN_INSERT_BlockLast(block, stid);
    }
  }
  else if (TY_IDX_index(WN_ty(wn)) == Struct_update_index)
  {
    UINT32 field_id = WN_field_id(wn);
    FmtAssert (field_id != 0, ("Field-id should be non-zero"));
    UINT32 new_field_id = field_map_info[field_id-1].new_field_id;

    FmtAssert (new_field_id == candidate_fld_id, ("Unexpected field-id found"));

    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                           new_field_id, cur_field_id);
    FmtAssert (!fld.Is_Null(), ("Field not found"));
    // update assignment
    WN_set_field_id(wn, new_field_id);
    WN_offset(wn) = FLD_ofst(fld);
    WN_Delete(WN_kid0(wn));
    WN_kid0(wn) = WN_Ldid(WN_desc(wn), 0, fld_st[0], Make_Pointer_Type(FLD_type(fld)));
    // get next field
    // TODO: generalize
    cur_field_id = 0;
    fld = FLD_get_to_field (Struct_update_index<<8, ++new_field_id, cur_field_id);
    FmtAssert (!fld.Is_Null(), ("Field not found"));

    // generate assignment for next field
    WN * val = WN_Ldid(WN_desc(WN_kid0(wn)), 0, fld_st[1], Make_Pointer_Type(FLD_type(fld)));
    WN * stid = WN_Stid(WN_rtype(val), FLD_ofst(fld), WN_st(wn), WN_ty(wn), val, new_field_id);
    WN_INSERT_BlockLast(block, stid);
  }
}

// call to memory free routine
WN * handle_function_call (WN * parent, WN * wn)
{
  FmtAssert (WN_operator(wn) == OPR_CALL, ("Call stmt expected"));

  WN * call = wn;

  if (!strcmp(ST_name(WN_st(call)), "free"))
  {
    // pointer to be freed.
    WN * ptr = WN_kid0(WN_kid0(call));

    // free for first field
    WN_st_idx(ptr) = ST_st_idx(fld_st[0]);
    WN_offset(ptr) = 0;
    WN_set_ty(ptr, Make_Pointer_Type(Struct_split_types[0]));

    // free for remaining fields
    for (INT i=1; i<Struct_split_count; i++)
    {
      WN * new_call = WN_COPY_Tree (call);
      WN_st_idx(WN_kid0(WN_kid0(new_call))) = ST_st_idx(fld_st[i]);
      WN_set_ty(WN_kid0(WN_kid0(new_call)),
                Make_Pointer_Type(Struct_split_types[i]));

      WN_INSERT_BlockAfter(parent, wn, new_call);
      call = new_call;
    }
    return call;
  }
  else Fail_FmtAssertion ("NYI");

  return NULL;
}

// Call to memory free routine, similar to handle_function_call() and
// the two should be merged.
void duplicate_call (WN * block, WN * call)
{
  FmtAssert (WN_operator(call) == OPR_CALL, ("Call stmt expected"));

  if (!strcmp(ST_name(WN_st(call)), "realloc"))
    return;

  FmtAssert (!strcmp(ST_name(WN_st(call)), "free") && WN_kid_count(call) == 1,
             ("Unexpected function call"));

  WN * new_call = WN_COPY_Tree (call);
  WN * addr = WN_kid0(WN_kid0(new_call));

  UINT32 new_field_id = WN_field_id(addr);

  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                           ++new_field_id, cur_field_id);
  FmtAssert (!fld.Is_Null(), ("Field not found"));
  WN_set_field_id(addr, new_field_id);
  WN_offset(addr) = FLD_ofst(fld);

  WN_INSERT_BlockLast (block, new_call);
}

// Handle an ISTORE when it needs to be duplicated.
void handle_istore_assignment (WN * block, WN * stmt)
{
  FmtAssert (WN_operator(stmt) == OPR_ISTORE,
             ("handle_istore_assignment: expected ISTORE"));
  if (TY_IDX_index(TY_pointed(WN_ty(stmt))) == Struct_update_index)
  {
    // Handle LHS of istore.
    UINT field_id = WN_field_id(stmt);
    FmtAssert (field_id == candidate_fld_id,
               ("handle_istore_assignment: unexpected field id"));
    WN * new_istore = WN_COPY_Tree(stmt);
    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                           field_id, cur_field_id);
    FmtAssert (!fld.Is_Null(), ("Field not found"));
    FmtAssert (FLD_ofst(fld) == WN_offset(stmt),
               ("handle_istore_assignment: Unexpected offset in ISTORE"));
    FmtAssert (field_id == field_map_info[field_id-1].new_field_id,
               ("Error in field-id computation"));

    // Handle LHS of istore for next field.
    cur_field_id = 0;
    fld = FLD_get_to_field (Struct_update_index<<8,
                                           ++field_id, cur_field_id);
    FmtAssert (!fld.Is_Null(), ("Field not found"));
    WN_set_field_id(new_istore, field_id);
    WN_offset(new_istore) = FLD_ofst(fld);

    WN * rhs = WN_kid0(stmt);
    if (WN_operator(rhs) == OPR_LDID)
    {
      // Update RHS of both assignment statements.
      FmtAssert (TY_kind(WN_ty(rhs)) == KIND_POINTER &&
                 TY_IDX_index(TY_pointed(WN_ty(rhs))) == candidate_ty_idx,
                 ("Unexpected pointer"));

      TY_IDX ty = Struct_field_layout[0].u.new_ty;
      WN_kid0(stmt) = WN_Ldid(WN_desc(rhs),
                                    0,
                                    fld_st[0],
                                    Make_Pointer_Type(ty));
      WN_Delete(rhs);

      ty = Struct_field_layout[1].u.new_ty;
      rhs = WN_kid0(new_istore);
      WN_kid0(new_istore) = WN_Ldid(WN_desc(rhs),
                                    0,
                                    fld_st[1],
                                    Make_Pointer_Type(ty));
      WN_Delete(rhs);
    }
    else
    {
      FmtAssert (WN_operator(rhs) == OPR_INTCONST, ("NYI"));
    }
    WN_INSERT_BlockLast(block, new_istore);
  }
  else Fail_FmtAssertion ("NYI");
}

// Input:
//   block: to insert any new statements generated.
//   wn is the kidid-th kid of parent.
//   field_id is the field-id being promoted from the parent.
//   lhs denotes if we are processing the lhs or rhs of the istore.
//
// Recursively traverses the tree and makes updates.
void handle_kid_of_istore (WN * block, WN * parent, WN * wn, INT kidid, UINT field_id, BOOL lhs)
{
  FmtAssert (!OPERATOR_is_stmt(WN_operator(wn)),
             ("ISTORE cannot have a statement as its kid"));

  if (!OPCODE_is_leaf (WN_opcode (wn)))
  {
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++)
      {
        kid = WN_kid (wn, kidno);
        if (kid)
        {
          UINT id = field_id;
          // Propagate the new field-id if required.
          if (WN_operator(wn) == OPR_ILOAD &&
              TY_IDX_index(WN_ty(wn)) == candidate_ty_idx &&
              WN_field_id(wn))
            id = WN_field_id(wn);
          handle_kid_of_istore (block, wn, kid, kidno, id, lhs);
        }
      }
  }

  if (WN_operator(wn) == OPR_ILOAD)
  {
    WN * addr = WN_kid0(wn);
    if (TY_IDX_index(WN_ty(wn)) == Struct_update_index && WN_field_id(wn))
    {
      FmtAssert (WN_operator(addr) == OPR_LDID, ("NYI"));
      UINT id = WN_field_id(wn);
      UINT32 new_field_id = field_map_info[id-1].new_field_id;
      if (id == candidate_fld_id && field_id == 2)
        new_field_id++;

      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                         new_field_id, cur_field_id);
      FmtAssert (!fld.Is_Null(), ("Field not found"));
      WN_set_field_id(wn, new_field_id);
      WN_offset(wn) = FLD_ofst(fld);
    }
    else if (TY_IDX_index(WN_ty(wn)) == candidate_ty_idx && WN_field_id(wn))
    {
      INT field_id = WN_field_id(wn);
      TY_IDX ty = Struct_field_layout[field_id-1].u.new_ty;
      INT field_num = 0;

      if (TY_kind(ty) == KIND_STRUCT)
        field_num = Struct_field_layout[field_id-1].fld_id;

      // adjust type size if present
      if (WN_operator(WN_kid0(wn)) == OPR_ADD &&
          WN_operator(WN_kid1(WN_kid0(wn))) == OPR_MPY)
      {
        WN * size = WN_kid1(WN_kid1(WN_kid0(wn)));
        FmtAssert (WN_operator(size) == OPR_INTCONST &&
                   WN_const_val(size) == TY_size(Ty_tab[candidate_ty_idx]),
                   ("Error while updating type size under iload"));
        WN_const_val(size) = TY_size(ty);
      }

      WN_kid(parent, kidid) = WN_Iload(WN_desc(wn), 0, ty, addr, field_num);
      WN_Delete(wn);
    }
  }
  else if (WN_operator(wn) == OPR_LDID &&
           (lhs ||
            IPA_Enable_Struct_Opt == 2) && // lhs of istore?
           TY_kind(WN_ty(wn)) == KIND_POINTER &&
           TY_IDX_index(TY_pointed(WN_ty(wn))) == candidate_ty_idx)
  {
    ST * base_st = fld_st[field_id - 1];
    TY_IDX ty = Struct_field_layout[field_id - 1].u.new_ty;

    if (IPA_Enable_Struct_Opt == 2 &&
        ST_class(WN_st(wn)) == CLASS_VAR &&
        Is_Global_Symbol(WN_st(wn)))
      base_st = &St_Table[Struct_field_layout[field_id - 1].st_idx];

    WN_kid(parent, kidid) = WN_Ldid(WN_desc(wn),
                                    0,
                                    base_st,
                                    Make_Pointer_Type(ty));
    WN_Delete(wn);

    wn = WN_kid(parent, kidid);
    WN_set_field_id(wn,0);
    WN_offset(wn) = 0;
    WN_set_ty(wn, Make_Pointer_Type(ty));
  }
}

void handle_istore (WN * block, WN * parent, WN * wn)
{
  // handle lhs and rhs first
  handle_kid_of_istore(block, wn, WN_kid1(wn), 1, WN_field_id(wn), TRUE);
  handle_kid_of_istore(block, wn, WN_kid0(wn), 0, WN_field_id(wn), FALSE);

  // handle istore now
  if (TY_IDX_index(TY_pointed(WN_ty(wn))) == candidate_ty_idx &&
      WN_field_id(wn))
  {
    WN * addr = WN_kid1(wn);
    const INT field_id = WN_field_id(wn);

    TY_IDX ty = Struct_field_layout[field_id - 1].u.new_ty;

    // Update the type size.
    if (WN_operator(addr) == OPR_ADD && WN_operator(WN_kid1(addr)) == OPR_MPY)
    {
      WN * size = WN_kid1(WN_kid1(addr));
      FmtAssert (WN_operator(size) == OPR_INTCONST &&
                 WN_const_val(size) == TY_size(Ty_tab[candidate_ty_idx]),
                 ("Error while updating type size under istore"));
      WN_const_val(size) = TY_size(ty);
    }
    if (TY_kind(ty) == KIND_STRUCT)
      WN_set_field_id(wn, Struct_field_layout[field_id - 1].fld_id);
    else
      WN_set_field_id(wn, 0);
    WN_offset(wn) = 0;
    WN_set_ty(wn, Make_Pointer_Type(ty));
  }
  else if (TY_IDX_index(TY_pointed(WN_ty(wn))) == Struct_update_index &&
           // check if loading the whole struct or a field from it.
           WN_field_id(wn))
  {
    UINT field_id = WN_field_id(wn);
    if (field_id != candidate_fld_id)
    {
      UINT new_field_id = field_map_info[field_id-1].new_field_id;
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                           new_field_id, cur_field_id);
      FmtAssert (!fld.Is_Null(), ("Field not found"));
      WN_set_field_id (wn, new_field_id);
      WN_offset(wn) = FLD_ofst(fld);
    }
    else
    {
      // Need to duplicate istore
      handle_istore_assignment (block, wn);
    }
  }
}

// Analyze the expression containing the pointer to TY-being-split.
// WN is KIDID'th kid of PARENT.
WN * analyze_addressof_ty_being_split (WN * parent, WN * wn, WN * addr,
                                       INT kidid)
{
  Is_True (WN_operator(wn) == OPR_ILOAD && TY_IDX_index(WN_ty(wn)) == candidate_ty_idx,
           ("Expected ILOAD through pointer to TY being split"));

  FLD_HANDLE fld1 = TY_fld(Ty_tab[candidate_ty_idx]);
  FLD_HANDLE fld2 = FLD_next(fld1);

  switch (WN_operator(addr))
  {
    case OPR_LDID:
    {
      // Addr is a pointer to the TY being split, so update the LDID symbol
      // and any offset present in the iload.
      WN_OFFSET ofst = 0;
      TY_IDX ty = TY_IDX_ZERO;
      ST * base_st = NULL;
      if (WN_field_id(wn) == 1)
      {
        ofst = FLD_ofst(fld1);
        ty = FLD_type(fld1);
        base_st = fld_st[0];
      }
      else
      {
        Is_True (WN_field_id(wn) == 2, ("Unexpected field id"));
        ofst = FLD_ofst(fld2);
        ty = FLD_type(fld2);
        base_st = fld_st[1];
      }
      INT index = (WN_load_offset(wn)-ofst) / TY_size(Ty_tab[candidate_ty_idx]);
      WN * base = WN_Ldid(WN_desc(addr), 0, base_st, Make_Pointer_Type(ty));
      WN_kid(parent, kidid) = WN_Iload(WN_desc(wn), index*TY_size(ty), ty, base);
      // delete old address expression
      WN_DELETE_Tree(wn);
      wn = WN_kid(parent, kidid);
    }
    break;

    case OPR_ADD:
    {
      WN * base = WN_kid0(addr);
      WN * ofst = WN_kid1(addr);

      FmtAssert (WN_operator(ofst) == OPR_MPY, ("Unexpected offset expression"));
      if (WN_operator(base) == OPR_LDID &&
          TY_IDX_index(WN_ty(base)) == Struct_update_index)
      {
        // Loading a struct that needs update because one of its fields is
        // a pointer to the type being split. Field-id gives the field being
        // loaded, and must be a pointer to candidate_ty_idx.
        TY_IDX ty = TY_IDX_ZERO;
        UINT32 field_id = field_map_info[WN_field_id(base)-1].new_field_id;
        if (WN_field_id(wn) == 1)
        {
          ty = Struct_split_types[0];
        }
        else
        {
          ty = Struct_split_types[1];
          field_id++;
        }
        UINT cur_field_id = 0;
        FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                               field_id, cur_field_id);
        FmtAssert (!fld.Is_Null(), ("Field not found"));
        WN_set_field_id(base, field_id);
        WN_offset(base) = FLD_ofst(fld);

        // update offset
        WN * size = WN_kid1(ofst);
        FmtAssert (WN_operator(size) == OPR_INTCONST, ("NYI"));
        WN_const_val(size) = TY_size(ty);
        // The offset may have been folded into the original iload, so
        // determine if we need the offset from it.
        UINT load_ofst = WN_load_offset(wn) >= 0 ? 0 : WN_load_offset(wn);
        WN_kid(parent, kidid) = WN_Iload(WN_desc(wn), load_ofst, ty, addr);
        WN_Delete(wn);
        wn = WN_kid(parent, kidid);
      }
      else if (WN_operator(base) == OPR_LDID)
      {
        const INT field_id = WN_field_id(wn);
        ST * base_st = fld_st[field_id - 1];
        TY_IDX ty = Struct_field_layout[field_id - 1].u.new_ty;

        if (IPA_Enable_Struct_Opt == 2 &&
            ST_class(WN_st(base)) == CLASS_VAR &&
            Is_Global_Symbol(WN_st(base)))
          base_st = &St_Table[Struct_field_layout[field_id - 1].st_idx];

        // update offset
        WN * size = WN_kid1(ofst);
        FmtAssert (WN_operator(size) == OPR_INTCONST, ("NYI"));
        WN_const_val(size) = TY_size(ty);
        // update base
        WN_kid0(addr) = WN_Ldid(WN_desc(base), 0, base_st, Make_Pointer_Type(ty));
        WN_kid(parent, kidid) = WN_Iload(WN_desc(wn), 0, ty, addr);
        // delete old base ptr
        WN_DELETE_Tree(base);
        WN_Delete(wn);
        wn = WN_kid(parent, kidid);
      }
      else
      {
        TY_IDX ty = Struct_field_layout[WN_field_id(wn) - 1].u.new_ty;
        // update offset
        WN * size = WN_kid1(ofst);
        FmtAssert (WN_operator(size) == OPR_INTCONST, ("NYI"));
        WN_const_val(size) = TY_size(ty);
        WN_kid(parent, kidid) = WN_Iload(WN_desc(wn), 0, ty, addr);
        WN_Delete(wn);
        wn = WN_kid(parent, kidid);
      }
    }
    break;

    case OPR_ILOAD:
      if (TY_IDX_index(WN_ty(addr)) == Struct_update_index)
      {
        // base will be updated later
        WN_OFFSET ofst = 0;
        TY_IDX ty = TY_IDX_ZERO;
        if (WN_field_id(wn) == 1)
        {
          ofst = FLD_ofst(fld1);
          ty = FLD_type(fld1);
        }
        else
        {
          ofst = FLD_ofst(fld2);
          ty = FLD_type(fld2);
        }
        INT index = (WN_load_offset(wn)-ofst) / TY_size(Ty_tab[candidate_ty_idx]);
        WN_kid(parent, kidid) = WN_Iload(WN_desc(wn), index*TY_size(ty), ty, addr, 0);
        // delete old kid
        WN_Delete(wn);
        wn = WN_kid(parent, kidid);
      }
      break;

    default: Fail_FmtAssertion ("NYI");
  }

  return wn;
}

void handle_compare (WN * g)
{
  if (preg_id == 0)
    return;

  WN * p = WN_kid0(g);
  WN * w = WN_kid0(p);

  Is_True (WN_operator(w) == OPR_LDID, (""));

  ST * sym = WN_st(w);

  if (ST_class(sym) != CLASS_PREG ||
      WN_offset(w) != preg_id)
    return;

  // TODO: generalize
  if (WN_operator(p) != OPR_EQ)
    Fail_FmtAssertion ("NYI");

  WN * kid1 = WN_kid1(p);
  Is_True (kid1 != w, (""));

  if (WN_operator(kid1) != OPR_INTCONST ||
      WN_const_val(kid1) != 0)
    return;

  WN * sym1 = WN_Ldid(WN_desc(w), 0, fld_st[0], ST_type(fld_st[0]));
  WN * sym2 = WN_Ldid(WN_desc(w), 0, fld_st[1], ST_type(fld_st[1]));

  WN * cond = WN_CIOR(WN_EQ(WN_desc(p), sym1, WN_Intconst(MTYPE_U4, 0)),
                      WN_EQ(WN_desc(p), sym2, WN_Intconst(MTYPE_U4, 0)));

  for (INT id=0; id<WN_kid_count(g); id++)
    if (WN_kid(g,id) == p)
    {
      WN_kid(g,id) = cond;
      break;
    }

  WN_DELETE_Tree(p);
}

WN * traverse_wn_tree (WN * block, WN * grandparent, WN * parent, WN * wn, INT kidid)
{
  if (wn == NULL) return NULL;

  // recursive traversal
  if (!OPCODE_is_leaf (WN_opcode (wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN* kid = WN_first (wn);
      while (kid)
      {
        WN * out = WN_CreateBlock();
        kid = traverse_wn_tree (out, parent, wn, kid, 0);
        if (WN_last(out))
        {
          WN * ptr = kid;
          kid = WN_last(out);
          WN_INSERT_BlockAfter (wn, ptr, out);
        }
        kid = WN_next (kid);
      }
    }
    else if (WN_operator(wn) != OPR_ISTORE)
    {
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++)
      {
        kid = WN_kid (wn, kidno);
        if (kid)
          traverse_wn_tree (block, parent, wn, kid, kidno);
      }
    }
  }

  switch (WN_operator(wn))
  {
    case OPR_STID:
    {
      if (TY_IDX_index(WN_ty(wn)) == Struct_update_index && WN_field_id(wn))
      {
        // We are storing into object of type struct that is being
        // updated, check what field is being accessed.
        if (WN_field_id(wn) != candidate_fld_id)
        { // update the field id
          UINT32 field_id = WN_field_id(wn);
          UINT32 new_field_id = field_map_info[field_id-1].new_field_id;
          UINT cur_field_id = 0;
          FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                           new_field_id, cur_field_id);
          FmtAssert (!fld.Is_Null(), ("Field not found"));
          WN_set_field_id(wn, new_field_id);
          WN_offset(wn) = FLD_ofst(fld);
        }
      }
    }
    break;

    case OPR_IF:
    {
      // If there is a conditional checking the return value from a
      // memory allocation routine for NULL, we need to update the check.
      // At this point, we just assume this is such a conditional.
      WN * test = WN_if_test(wn);

      if (WN_operator(test) == OPR_EQ &&
          WN_operator(WN_kid0(test)) == OPR_LDID)
      {
        TY_IDX ty = WN_ty(WN_kid0(test));
        if (TY_kind(ty) == KIND_POINTER &&
            TY_IDX_index(TY_pointed(ty)) == candidate_ty_idx)
          handle_compare (wn);
      }
    }
    break;

    case OPR_LDID:
    {
      TY_IDX ty = WN_ty(wn);
      if (TY_kind(ty) == KIND_POINTER &&
          TY_IDX_index(TY_pointed(ty)) == candidate_ty_idx)
      {
        if (WN_st(wn) == Return_Val_Preg)
          handle_function_return (block, parent);
        else if (WN_operator(parent) == OPR_STID) //&&
                // TY_IDX_index(WN_ty(parent)) == Struct_update_index)
          handle_assignment (block, parent);
        else if (WN_operator(parent) == OPR_ISTORE)
        {
          Fail_FmtAssertion ("Should not reach here");
          //handle_istore_assignment(block, parent, wn, kidid);
        }
      }
      else if (TY_IDX_index(ty) == Struct_update_index)
      {
        // We are loading an object of type struct that is being
        // updated, check what field is being accessed.
        if (WN_field_id(wn) == candidate_fld_id)
        {
          if (WN_operator(parent) == OPR_PARM)
            duplicate_call(block, grandparent);
        }
        else if (WN_field_id(wn))
        { // update the field id
          UINT32 field_id = WN_field_id(wn);
          UINT32 new_field_id = field_map_info[field_id-1].new_field_id;
          UINT cur_field_id = 0;
          FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                           new_field_id, cur_field_id);
          FmtAssert (!fld.Is_Null(), ("Field not found"));
          WN_set_field_id(wn, new_field_id);
          WN_offset(wn) = FLD_ofst(fld);
        }
      }
    }
    break;

    case OPR_ILOAD:
    {
      if (TY_IDX_index(WN_ty(wn)) == candidate_ty_idx)
      { // ILOAD <TY-being-split> <pointer-to-TY-being-split>
        wn = analyze_addressof_ty_being_split (parent, wn, WN_kid0(wn), kidid);
      }
      else if (TY_IDX_index(WN_ty(wn)) == Struct_update_index && WN_field_id(wn))
      {
        if (WN_operator(parent) == OPR_PARM &&
            WN_field_id(wn) == candidate_fld_id)
          duplicate_call(block, grandparent);
        else
        {
          UINT32 field_id = WN_field_id(wn);
          FmtAssert (field_id != 0, ("Non-zero field-id expected"));
          UINT32 new_field_id = field_map_info[field_id-1].new_field_id;
          WN * addr = WN_operator(parent) == OPR_ILOAD ? parent :
                      WN_operator(grandparent) == OPR_ILOAD ? grandparent :
                      NULL;
          if (addr &&
              TY_IDX_index(WN_ty(addr)) == candidate_ty_idx &&
              WN_field_id(addr) == 2)
            new_field_id++;
          else
          {
            addr = WN_operator(parent) == OPR_ISTORE ? parent :
                   WN_operator(grandparent) == OPR_ISTORE ? grandparent :
                   NULL;
            if (addr &&
                TY_IDX_index(TY_pointed(WN_ty(addr))) == candidate_ty_idx &&
                WN_field_id(addr) == 2)
              new_field_id++;
          }
          UINT cur_field_id = 0;
          FLD_HANDLE fld = FLD_get_to_field (Struct_update_index<<8,
                                             new_field_id, cur_field_id);
          FmtAssert (!fld.Is_Null(), ("Field not found"));
          WN_set_field_id(wn, new_field_id);
          WN_offset(wn) = FLD_ofst(fld);
        }
      }
    }
    break;

    case OPR_ISTORE:
      handle_istore (block, parent, wn);
      break;

    case OPR_CALL:
    {
      if (!strcmp(ST_name(WN_st(wn)), "free") && WN_kid_count(wn) == 1)
      {
        WN * arg = WN_kid0(WN_kid0(wn));
        if (WN_operator(arg) == OPR_LDID &&
            TY_kind(WN_ty(arg)) == KIND_POINTER &&
            TY_IDX_index(TY_pointed(WN_ty(arg))) == candidate_ty_idx)
          wn = handle_function_call (parent, wn);
      }
    }
    break;

    default: break;
  }

  return wn;
}

// complete structure relayout optimization

static int global_ptrs_for_complete_struct_relayout_created = 0;
static int complete_struct_relayout_size = 0;
static int orig_complete_struct_relayout_size = 0;
static TY_IDX ptr_to_gptr_ty_idx
  [MAX_NUM_FIELDS_IN_COMPLETE_STRUCT_RELAYOUT+1];
static ST *gptr_st[MAX_NUM_FIELDS_IN_COMPLETE_STRUCT_RELAYOUT+1];
static int field_offset_in_complete_struct_relayout
  [MAX_NUM_FIELDS_IN_COMPLETE_STRUCT_RELAYOUT+1];
static int num_fields_in_complete_struct_relayout = 0;
static int continue_with_complete_struct_relayout = 1;
static int encountered_calloc_for_complete_struct_relayout = 0;

// Create the global pointers gptr[0..num_fields_in_complete_struct_relayout]
// for the structure that will be completely relayed-out.  There is no harm in
// creating these pointers even if we decide not to invoke the optimization
// later (most likely due to legality).  At the same time, save the ty_idx's for// the members of complete_struct_relayout also.
static void create_global_ptrs_for_complete_struct_relayout(void)
{
  global_ptrs_for_complete_struct_relayout_created = 1;
  complete_struct_relayout_size = TY_size(complete_struct_relayout_type_id <<
    8);
  orig_complete_struct_relayout_size = complete_struct_relayout_size;
  if (complete_struct_relayout_size > 48 && complete_struct_relayout_size < 64)
    complete_struct_relayout_size = 64; // close enough to pad to improve
      // performance
  if (complete_struct_relayout_size > 96 && complete_struct_relayout_size < 128)
    complete_struct_relayout_size = 128; // close enough to pad to improve
      // performance
  if (complete_struct_relayout_size != 1 &&
      complete_struct_relayout_size != 2 &&
      complete_struct_relayout_size != 4 &&
      complete_struct_relayout_size != 8 &&
      complete_struct_relayout_size != 16 &&
      complete_struct_relayout_size != 32 &&
      complete_struct_relayout_size != 64 &&
      complete_struct_relayout_size != 128)
  {
    // for performance reasons, only allow structure size that is a power of 2
    // and not too big
    continue_with_complete_struct_relayout = 0;
    return;
  }

  // gptr[0] is a special "base" pointer; it points to the original calloc-ed
  // memory and has the type (complete_struct_relayout *)
  ptr_to_gptr_ty_idx[0] = Make_Pointer_Type(complete_struct_relayout_type_id <<
    8);
  gptr_st[0] = New_ST(GLOBAL_SYMTAB);
  ST_Init(gptr_st[0], Save_Str2i("g", "ptr", 0), CLASS_VAR, SCLASS_COMMON,
    EXPORT_INTERNAL, ptr_to_gptr_ty_idx[0]);

  // gptr[1..num_fields_in_complete_struct_relayout] points to the beginning of
  // each relayed-out array; it has the type (field_i_type *)
  int i = 1;
  FLD_ITER field_iter = Make_fld_iter(TY_fld(complete_struct_relayout_type_id
    << 8));
  do
  {
    FLD_HANDLE field_handle(field_iter);
    ptr_to_gptr_ty_idx[i] = Make_Pointer_Type(FLD_type(field_handle));
    gptr_st[i] = New_ST(GLOBAL_SYMTAB);
    ST_Init(gptr_st[i], Save_Str2i("g", "ptr", i), CLASS_VAR, SCLASS_COMMON,
      EXPORT_INTERNAL, ptr_to_gptr_ty_idx[i]);
    field_offset_in_complete_struct_relayout[i] = FLD_ofst(field_handle);
    i++;
  } while (!FLD_last_field(field_iter++));
  num_fields_in_complete_struct_relayout = i-1;

  // these pointers are not initialized here; that will be done when memory is
  // allocated for the entire complete_struct_relayout
  return;
}

// Does the input wn contain the type complete_struct_relayout?
static BOOL expr_contains_complete_struct_relayout(WN *wn)
{
  int child_num;
  WN *child_wn;
  TY_IDX child_ty_idx;
  int i;

  if (wn == NULL)
    return FALSE;
  if (WN_operator(wn) != OPR_ADD && WN_operator(wn) != OPR_SUB)
    return FALSE; // we are only interested in ptr +/- n, where ptr points to
      // complete_struct_relayout
  for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
  {
    child_wn = WN_kid(wn, child_num);
    if (child_wn != NULL)
    {
      child_ty_idx = WN_ty(child_wn);
      if (child_ty_idx != 0 &&
          TY_IDX_index(child_ty_idx) == complete_struct_relayout_type_id ||
          (TY_kind(child_ty_idx) == KIND_POINTER &&
           TY_IDX_index(TY_pointed(child_ty_idx)) ==
             complete_struct_relayout_type_id))
        return TRUE;
      if (child_ty_idx != 0 && TY_kind(child_ty_idx) == KIND_STRUCT &&
          WN_operator(child_wn) == OPR_ILOAD)
      {
        int field_id = WN_field_id(child_wn);
        for (i = 0; i <
             num_structs_with_field_pointing_to_complete_struct_relayout; i++)
        {
          if (struct_with_field_pointing_to_complete_struct_relayout_type_id[i]
                == TY_IDX_index(child_ty_idx) &&
              struct_with_field_pointing_to_complete_struct_relayout_field_num
                [i] == field_id)
          return TRUE;
        }
      }
      if (WN_operator(child_wn) == OPR_ADD || WN_operator(child_wn) == OPR_SUB)
        if (expr_contains_complete_struct_relayout(child_wn) == TRUE)
          return TRUE;
    }
  }
  return FALSE;
}

// First pass:  is it legal (safe) to do complete_struct_relayout?  (Read the
// code in traverse_wn_tree_for_complete_struct_relayout to understand this
// function.)
static WN *traverse_wn_tree_for_complete_struct_relayout_legality(WN *block_wn,
  WN *grandparent_wn, WN *parent_wn, WN *wn, INT child_num)
{
  if (wn == NULL || continue_with_complete_struct_relayout == 0)
    return NULL;

  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        WN *new_block_wn = WN_CreateBlock();
        child_wn = traverse_wn_tree_for_complete_struct_relayout_legality
          (new_block_wn, parent_wn, wn, child_wn, 0);
        if (continue_with_complete_struct_relayout == 0)
          return wn;
        if (WN_last(new_block_wn) != NULL)
        {
          // something was added to new_block_wn
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable -1\n");
          return wn;
        }
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
        {
          traverse_wn_tree_for_complete_struct_relayout_legality(block_wn,
            parent_wn, wn, child_wn, child_num);
          if (continue_with_complete_struct_relayout == 0)
            return wn;
        }
      }
    }
  }

  switch (WN_operator(wn))
  {
    case OPR_INTCONST:
      if (complete_struct_relayout_size != orig_complete_struct_relayout_size)
      {
        if (WN_const_val(wn) == orig_complete_struct_relayout_size)
        {
          if (parent_wn != NULL &&
              (WN_operator(parent_wn) == OPR_ADD ||
               WN_operator(parent_wn) == OPR_SUB))
          {
            if (expr_contains_complete_struct_relayout(parent_wn))
            {
              break;
            }
          }
          if (parent_wn != NULL && WN_operator(parent_wn) == OPR_MPY)
          {
            if (grandparent_wn != NULL &&
                (WN_operator(grandparent_wn) == OPR_ADD ||
                 WN_operator(grandparent_wn) == OPR_SUB))
            {
              if (expr_contains_complete_struct_relayout(grandparent_wn))
              {
                break;
              }
            }
          }
          if (parent_wn != NULL && WN_operator(parent_wn) == OPR_PARM)
          {
            if (grandparent_wn != NULL &&
                WN_operator(grandparent_wn) == OPR_CALL &&
                strcmp(ST_name(WN_st(grandparent_wn)), "calloc") == 0)
              break;
          }

          // the following may be too conservative; perhaps we should just let
          // the rest escape unchanged?
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable 0\n");
          return wn;
        }
      }
      break;

    case OPR_LDID:
    case OPR_STID:
      if (TY_IDX_index(WN_ty(wn)) == complete_struct_relayout_type_id)
      {
        continue_with_complete_struct_relayout = 0;
        if (Get_Trace(TP_IPA, 1))
          fprintf(TFile, "ipo -> complete_struct_relayout disable 1 2\n");
        return wn;
      }
      break;

    case OPR_ILOAD:
      if (TY_IDX_index(WN_ty(wn)) == complete_struct_relayout_type_id)
      {
        if (WN_operator(WN_kid0(wn)) == OPR_LDID &&
            (TY_kind(WN_ty(WN_kid0(wn))) != KIND_POINTER ||
             TY_pointed(WN_ty(WN_kid0(wn))) != WN_ty(wn)))
        {
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable 3\n");
          return wn;
        }
      }
      break;

    case OPR_ISTORE:
      if (TY_kind(WN_ty(wn)) == KIND_POINTER &&
          TY_IDX_index(TY_pointed(WN_ty(wn))) ==
            complete_struct_relayout_type_id)
      {
        if (WN_operator(WN_kid1(wn)) == OPR_LDID &&
            WN_ty(WN_kid1(wn)) != WN_ty(wn))
        {
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable 4\n");
          return wn;
        }
      }
      break;

    case OPR_CALL:
      if (strcmp(ST_name(WN_st(wn)), "malloc") == 0 ||
          strcmp(ST_name(WN_st(wn)), "realloc") == 0)
      {
        if (WN_operator(WN_next(wn)) != OPR_STID ||
            WN_operator(WN_kid0(WN_next(wn))) != OPR_LDID)
          return wn;

        TY_IDX ty_idx = WN_ty(WN_next(wn));
        if (TY_kind(ty_idx) == KIND_POINTER &&
            TY_IDX_index(TY_pointed(ty_idx)) ==
             complete_struct_relayout_type_id)
        {
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable 5\n");
          return wn;
        }
      }
      if (strcmp(ST_name(WN_st(wn)), "calloc") == 0 && WN_kid_count(wn) == 2)
      {
        if (WN_operator(WN_next(wn)) != OPR_STID ||
            WN_operator(WN_kid0(WN_next(wn))) != OPR_LDID)
          return wn;

        TY_IDX ptr_to_struct_ty_idx = WN_ty(WN_kid0(WN_next(wn)));
        if (WN_ty(WN_next(wn)) != ptr_to_struct_ty_idx ||
            TY_kind(ptr_to_struct_ty_idx) != KIND_POINTER ||
            TY_IDX_index(TY_pointed(ptr_to_struct_ty_idx)) !=
              complete_struct_relayout_type_id)
          return wn;

        WN *arg0_wn = WN_kid0(WN_kid0(wn));
        WN *arg1_wn = WN_kid0(WN_kid1(wn));
        if (WN_operator(arg0_wn) == OPR_INTCONST &&
            WN_const_val(arg0_wn) == orig_complete_struct_relayout_size)
        {
        }
        else if (WN_operator(arg1_wn) == OPR_INTCONST &&
                 WN_const_val(arg1_wn) == orig_complete_struct_relayout_size)
        {
        }
        else
        {
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable 6\n");
          return wn;
        }

        if (encountered_calloc_for_complete_struct_relayout == 1)
        {
          //  for now, only one global calloc is allowed
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable 6.5\n");
          return wn;
        }

        encountered_calloc_for_complete_struct_relayout = 1;
        wn = WN_next(wn);
        if (TY_IDX_index(TY_pointed(ptr_to_struct_ty_idx)) !=
            TY_IDX_index(TY_pointed(ptr_to_gptr_ty_idx[0])))
        {
          continue_with_complete_struct_relayout = 0;
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> complete_struct_relayout disable 7\n");
          return wn;
        }
      }
      break;

    default:
      break;
  }

  return wn;
}

// Second pass:  we already know that it is legal (safe) to do
// complete_struct_relayout; now perform the optimization.
static WN *traverse_wn_tree_for_complete_struct_relayout(WN *block_wn,
  WN *grandparent_wn, WN *parent_wn, WN *wn, INT child_num)
{
  if (wn == NULL)
    return NULL;

  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        WN *new_block_wn = WN_CreateBlock();
        child_wn = traverse_wn_tree_for_complete_struct_relayout(new_block_wn,
          parent_wn, wn, child_wn, 0);
        if (WN_last(new_block_wn) != NULL)
        {
          // something was added to new_block_wn
          WN *save_child_wn = child_wn;
          child_wn = WN_last(new_block_wn);
          WN_INSERT_BlockAfter(wn, save_child_wn, new_block_wn);
        }
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
          traverse_wn_tree_for_complete_struct_relayout(block_wn, parent_wn, wn,
            child_wn, child_num);
      }
    }
  }

  // nodes that are of interest
  switch (WN_operator(wn))
  {
    case OPR_INTCONST:
      if (complete_struct_relayout_size != orig_complete_struct_relayout_size)
      {
        // We need to change all the occurrences of the original size of our
        // complete_struct_relayout to the new size.  This is because the front
        // end has already replaced all sizeof(complete_struct_relayout) by its
        // constant, even before the ipl phase.
        if (WN_const_val(wn) == orig_complete_struct_relayout_size)
        {
          // examine all such constants, but need to replace only those
          // implicitly generated by the compiler in cases such as node[i] or
          // node++, where node is a pointer to our complete_struct_relayout;
          // all other occurrences, even those that are
          // sizeof(complete_struct_relayout), should remain as they are to
          // match the user's expectation
          if (parent_wn != NULL &&
              (WN_operator(parent_wn) == OPR_ADD ||
               WN_operator(parent_wn) == OPR_SUB))
          {
            if (expr_contains_complete_struct_relayout(parent_wn))
            {
              WN_const_val(wn) = complete_struct_relayout_size;
              break;
            }
          }
          if (parent_wn != NULL && WN_operator(parent_wn) == OPR_MPY)
          {
            if (grandparent_wn != NULL &&
                (WN_operator(grandparent_wn) == OPR_ADD ||
                 WN_operator(grandparent_wn) == OPR_SUB))
            {
              if (expr_contains_complete_struct_relayout(grandparent_wn))
              {
                WN_const_val(wn) = complete_struct_relayout_size;
                break;
              }
            }
          }
          if (parent_wn != NULL && WN_operator(parent_wn) == OPR_PARM)
          {
            if (grandparent_wn != NULL &&
                WN_operator(grandparent_wn) == OPR_CALL &&
                strcmp(ST_name(WN_st(grandparent_wn)), "calloc") == 0)
              break; // the calloc code below will take care of this case
          }

          // the following may be too conservative; perhaps we should just let
          // the rest escape unchanged?
          // fprintf(stderr, "disable0\n"); legality taken care of
          return wn;
        }
      }
      break;

    case OPR_ILOAD:
      // Look for a load from our complete_struct_relayout.  Need to change
      // "= ptr->field_i" into
      // "= gptr_i[(ptr-gptr_0)/sizeof(complete_struct_relayout)]"
      if (TY_IDX_index(WN_ty(wn)) == complete_struct_relayout_type_id)
      {
        TYPE_ID desc;
        WN *ldid_ptr_wn;
        WN *ldid_gptr0_wn;
        WN *sub_wn;
        WN *complete_struct_size_wn;
        WN *div_wn;
        WN *field_size_wn;
        WN *mult_wn;
        WN *ldid_gptri_wn;
        WN *add_wn;
        int index_offset;

        if (WN_operator(WN_kid0(wn)) == OPR_LDID &&
            (TY_kind(WN_ty(WN_kid0(wn))) != KIND_POINTER ||
             TY_pointed(WN_ty(WN_kid0(wn))) != WN_ty(wn)))
        {
          // fprintf(stderr, "disable3\n"); legailty taken care of
          return wn;
        }
        desc = TY_mtype(ptr_to_gptr_ty_idx[0]); // pointer mtype
        ldid_ptr_wn = WN_COPY_Tree(WN_kid0(wn));
        ldid_gptr0_wn = WN_Ldid(desc, 0, gptr_st[0], ptr_to_gptr_ty_idx[0]);
        sub_wn = WN_CreateExp2(OPCODE_make_op(OPR_SUB, desc, MTYPE_V),
          ldid_ptr_wn, ldid_gptr0_wn);
        complete_struct_size_wn = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST,
          MTYPE_U4, MTYPE_V), (INT)complete_struct_relayout_size);
        div_wn = WN_CreateExp2(OPCODE_make_op(OPR_DIV, MTYPE_U4, MTYPE_V),
          sub_wn, complete_struct_size_wn);
        field_size_wn = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, MTYPE_U4,
          MTYPE_V), (INT)TY_size(TY_pointed(ptr_to_gptr_ty_idx[WN_field_id
          (wn)])));
        mult_wn = WN_CreateExp2(OPCODE_make_op(OPR_MPY, MTYPE_U4, MTYPE_V),
          div_wn, field_size_wn);
        ldid_gptri_wn = WN_Ldid(desc, 0, gptr_st[WN_field_id(wn)],
          ptr_to_gptr_ty_idx[WN_field_id(wn)]);
        add_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, desc, MTYPE_V),
          ldid_gptri_wn, mult_wn);
        WN_kid0(wn) = add_wn;
        WN_set_ty(wn, TY_pointed(ptr_to_gptr_ty_idx[WN_field_id(wn)]));
        WN_set_load_addr_ty(wn, ptr_to_gptr_ty_idx[WN_field_id(wn)]);
        index_offset = (WN_offset(wn) -
          field_offset_in_complete_struct_relayout[WN_field_id(wn)]) /
          orig_complete_struct_relayout_size; // positive or negative
        WN_offset(wn) = index_offset * TY_size(TY_pointed(ptr_to_gptr_ty_idx
          [WN_field_id(wn)]));
        WN_set_field_id(wn, 0);
        IPA_WN_MAP32_Set(Current_Map_Tab, WN_MAP_ALIAS_CGNODE, wn, 0);
      }
      break;

    case OPR_ISTORE:
      // Look for an assignment into our complete_struct_relayout.  Need to
      // change "ptr->field_i = ..." into
      // "gptr_i[(ptr-gptr_0)/sizeof(complete_struct_relayout)] = ..."
      if (TY_kind(WN_ty(wn)) == KIND_POINTER &&
          TY_IDX_index(TY_pointed(WN_ty(wn))) ==
            complete_struct_relayout_type_id)
      {
        TYPE_ID desc;
        WN *ldid_ptr_wn;
        WN *ldid_gptr0_wn;
        WN *sub_wn;
        WN *complete_struct_size_wn;
        WN *div_wn;
        WN *field_size_wn;
        WN *mult_wn;
        WN *ldid_gptri_wn;
        WN *add_wn;
        int index_offset;

        if (WN_operator(WN_kid1(wn)) == OPR_LDID &&
            WN_ty(WN_kid1(wn)) != WN_ty(wn))
        {
          // fprintf(stderr, "disable4\n"); legailty taken care of
          return wn;
        }
        desc = TY_mtype(ptr_to_gptr_ty_idx[0]); // pointer mtype
        ldid_ptr_wn = WN_COPY_Tree(WN_kid1(wn));
        ldid_gptr0_wn = WN_Ldid(desc, 0, gptr_st[0], ptr_to_gptr_ty_idx[0]);
        sub_wn = WN_CreateExp2(OPCODE_make_op(OPR_SUB, desc, MTYPE_V),
          ldid_ptr_wn, ldid_gptr0_wn);
        complete_struct_size_wn = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST,
          MTYPE_U4, MTYPE_V), (INT)complete_struct_relayout_size);
        div_wn = WN_CreateExp2(OPCODE_make_op(OPR_DIV, MTYPE_U4, MTYPE_V),
          sub_wn, complete_struct_size_wn);
        field_size_wn = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, MTYPE_U4,
          MTYPE_V), (INT)TY_size(TY_pointed(ptr_to_gptr_ty_idx[WN_field_id
          (wn)])));
        mult_wn = WN_CreateExp2(OPCODE_make_op(OPR_MPY, MTYPE_U4, MTYPE_V),
          div_wn, field_size_wn);
        ldid_gptri_wn = WN_Ldid(desc, 0, gptr_st[WN_field_id(wn)],
          ptr_to_gptr_ty_idx[WN_field_id(wn)]);
        add_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, desc, MTYPE_V),
          ldid_gptri_wn, mult_wn);
        WN_kid1(wn) = add_wn;
        WN_set_ty(wn, ptr_to_gptr_ty_idx[WN_field_id(wn)]);
        index_offset = (WN_offset(wn) -
          field_offset_in_complete_struct_relayout[WN_field_id(wn)]) /
          orig_complete_struct_relayout_size; // positive or negative
        WN_offset(wn) = index_offset * TY_size(TY_pointed(ptr_to_gptr_ty_idx
          [WN_field_id(wn)]));
        WN_set_field_id(wn, 0);
        IPA_WN_MAP32_Set(Current_Map_Tab, WN_MAP_ALIAS_CGNODE, wn, 0);
      }
      break;

    case OPR_CALL:
      // Look for "ptr = (complete_struct_relayout *)calloc(num, sizeof
      // (complete_struct_relayout))" and reject
      // "ptr = (complete_struct_relayout *)malloc(...)"
      // (since num is not known (although it can be rediscovered by size/sizeof
      // (complete_struct_relayout))) and "ptr = (complete_struct_relayout *)
      // realloc(ptr1, new_size)" (since we currently do not keep track of the
      // old_size, which will be needed in the realloc-ing of the
      // complete_struct_relayout.
      if (strcmp(ST_name(WN_st(wn)), "malloc") == 0 ||
          strcmp(ST_name(WN_st(wn)), "realloc") == 0)
      {
        if (WN_operator(WN_next(wn)) != OPR_STID ||
            WN_operator(WN_kid0(WN_next(wn))) != OPR_LDID)
          return wn; // does not assign into our complete_struct_relayout

        TY_IDX ty_idx = WN_ty(WN_next(wn));
        if (TY_kind(ty_idx) == KIND_POINTER &&
            TY_IDX_index(TY_pointed(ty_idx)) ==
              complete_struct_relayout_type_id)
        {
          // fprintf(stderr, "disable5\n"); legailty taken care of
          return wn;
        }
      }
      if (strcmp(ST_name(WN_st(wn)), "calloc") == 0 && WN_kid_count(wn) == 2)
      {
        if (WN_operator(WN_next(wn)) != OPR_STID ||
            WN_operator(WN_kid0(WN_next(wn))) != OPR_LDID)
          return wn; // does not assign into our complete_struct_relayout

        TY_IDX ptr_to_struct_ty_idx = WN_ty(WN_kid0(WN_next(wn)));
        if (WN_ty(WN_next(wn)) != ptr_to_struct_ty_idx ||
            TY_kind(ptr_to_struct_ty_idx) != KIND_POINTER ||
            TY_IDX_index(TY_pointed(ptr_to_struct_ty_idx)) !=
              complete_struct_relayout_type_id)
          return wn; // does not assign into our complete_struct_relayout

        // Yes, this is the special calloc.  Do the following for every such
        // calloc encountered, so that the global gptr[]'s will get sync-ed up
        // with the calloc's.  From now on, anytime we encounter something not
        // right, we need to disable the optimization, not just return and
        // pretend that this optimization does not apply
        WN *num_elements_wn;
        ST *calloc_returned_ptr_st; // doesn't work
        TYPE_ID desc;
        WN *ldid_wn;
        WN *stid_wn;
        TY_IDX field_ty_idx;
        TY_IDX ptr_to_field_ty_idx;
        TY_IDX ptr_to_prev_field_ty_idx;
        int prev_field_size;
        WN *field_size_wn;
        WN *mult_wn;
        WN *add_wn;

        // find out from the calloc arguments the number of elements being
        // allocated
        WN *arg0_wn = WN_kid0(WN_kid0(wn));
        WN *arg1_wn = WN_kid0(WN_kid1(wn));
        if (WN_operator(arg0_wn) == OPR_INTCONST &&
            WN_const_val(arg0_wn) == orig_complete_struct_relayout_size)
        {
          num_elements_wn = arg1_wn;
          WN_const_val(arg0_wn) = complete_struct_relayout_size;
        }
        else if (WN_operator(arg1_wn) == OPR_INTCONST &&
                 WN_const_val(arg1_wn) == orig_complete_struct_relayout_size)
        {
          num_elements_wn = arg0_wn;
          WN_const_val(arg1_wn) = complete_struct_relayout_size;
        }
        else
        {
          // fprintf(stderr, "disable6\n"); legailty taken care of
          return wn;
        }

        // only now do we advance wn, and get the pointer calloc returns and
        // start dividing up the allocated memory
        wn = WN_next(wn);
        calloc_returned_ptr_st = WN_st(wn); // doesn't work
        desc = TY_mtype(ptr_to_gptr_ty_idx[0]); // pointer mtype

        // gptr[0] = calloc_returned_ptr
        if (TY_IDX_index(TY_pointed(ptr_to_struct_ty_idx)) !=
            TY_IDX_index(TY_pointed(ptr_to_gptr_ty_idx[0])))
        {
          // fprintf(stderr, "disable7\n"); legailty taken care of
          return wn;
        }
        ldid_wn = WN_COPY_Tree(WN_kid0(wn)); // for some reason building a new
          // WN_Ldid with the calloc_returned_ptr does not work
        stid_wn = WN_Stid(desc, 0, gptr_st[0], ptr_to_gptr_ty_idx[0], ldid_wn);
        WN_INSERT_BlockLast(block_wn, stid_wn);

        // gptr[i] = gptr[i-1] + num_elements * sizeof(field[i-1])
        for (int i = 1; i <= num_fields_in_complete_struct_relayout; i++)
        {
          ldid_wn = WN_Ldid(desc, 0, gptr_st[i-1], ptr_to_gptr_ty_idx[i-1]);
          if (i == 1)
            add_wn = ldid_wn;
          else
          {
            field_size_wn = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST,
              MTYPE_U4, MTYPE_V), (INT)TY_size(TY_pointed(ptr_to_gptr_ty_idx
              [i-1])));
            mult_wn = WN_CreateExp2(OPCODE_make_op(OPR_MPY, MTYPE_U4, MTYPE_V),
              WN_COPY_Tree(num_elements_wn), field_size_wn);
            add_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, desc, MTYPE_V),
              ldid_wn, mult_wn);
          }
          stid_wn = WN_Stid(desc, 0, gptr_st[i], ptr_to_gptr_ty_idx[i], add_wn);
          WN_INSERT_BlockLast(block_wn, stid_wn);
        }
      }

      // all other calls (including free()) do not affect this optimization
      break;

    default:
      break;
  }

  return wn;
}

// array remapping optimization

static int continue_with_array_remapping = 1;
static int array_remapping_candidate_mtype = 0;
static int array_remapping_candidate_mtype_size = 0;
static int array_remapping_malloc_size = 0;
static int array_remapping_pre_margin_size = 0;
static int array_remapping_loop_depth = 0;
static int array_remapping_num_groups = 0;
static int array_remapping_loop_num_iterations = 0;
static int array_remapping_num_elements_in_pre_margin;
static int array_remapping_num_elements_in_array;
static int array_remapping_num_elements_in_post_margin;
static int array_remapping_num_elements_in_group;
static int array_remapping_final_checked = 0;

extern BOOL argument_in_callers_is_array_remapping_candidate_malloc
  (IPA_NODE *node, int argument_num);

// This function returns the argument number (0, 1, 2, ...) of the input st in
// the input function_wn.  If this is not possible, -1 is returned.
static int get_argument_num(ST *st, WN *function_wn)
{
  if (WN_operator(function_wn) == OPR_FUNC_ENTRY)
  {
    for (INT i = 0; i < WN_kid_count(function_wn); i++)
    {
      if (WN_operator(WN_kid(function_wn, i)) == OPR_IDNAME &&
          WN_st(WN_kid(function_wn, i)) == st)
        return i;
    }
  }
  return -1;
}

// Can the input wn possibly be an array remapping candidate?  When in doubt,
// return TRUE (and let the input wn go through more rigorous testing).  When it
// is obvious that it cannot be (e.g. not an array reference, another array of
// another type, etc.), return FALSE.
static BOOL wn_can_be_array_remapping_candidate(WN *wn)
{
  if (WN_operator(wn) == OPR_LDID)
  {
    if (!(TY_kind(WN_ty(wn)) == KIND_POINTER &&
          TY_kind(TY_pointed(WN_ty(wn))) == KIND_SCALAR))
      return FALSE; // an array remapping candidate is a pointer to a scalar
    if (array_remapping_candidate_mtype == 0)
      return TRUE;
    if (TY_mtype(TY_pointed(WN_ty(wn))) != array_remapping_candidate_mtype)
      return FALSE; // array of another type
  }
  if (WN_operator(wn) == OPR_ARRAY)
  {
    // can we summarily return FALSE because OPR_ARRAY's are not malloced,
    // whereas array remapping candidates must be?
    WN *array_base_wn = WN_kid0(wn);
    if (WN_operator(array_base_wn) == OPR_LDA &&
        TY_kind(WN_ty(array_base_wn)) == KIND_POINTER &&
        TY_kind(TY_pointed(WN_ty(array_base_wn))) == KIND_ARRAY &&
        array_remapping_candidate_mtype != 0 &&
        TY_mtype(TY_etype(TY_pointed(WN_ty(array_base_wn)))) !=
          array_remapping_candidate_mtype)
      return FALSE; // array of another type
    if (WN_operator(array_base_wn) == OPR_LDA &&
        WN_field_id(array_base_wn) != 0)
      return FALSE; // base array just a little too complicated:  e.g. it is a
                    // member of a struct, etc.
  }
  return TRUE;
}

// Given a loop_wn, this function fills in the loop information (loop_index_st,
// iteration starting, ending, and stride values).  If this is not possible,
// FALSE is returned.
static BOOL get_do_loop_info(WN *loop_wn, ST **loop_index_st, int *start,
  int *end, int *stride)
{
  int temp_stride;

  if (loop_wn == NULL)
    return FALSE;
  if (WN_operator(loop_wn) != OPR_DO_LOOP)
    return FALSE;
  *loop_index_st = WN_st(WN_kid(loop_wn, 0));
  if (WN_operator(WN_kid(loop_wn, 1)) == OPR_STID &&
      WN_st(WN_kid(loop_wn, 1)) == *loop_index_st &&
      WN_operator(WN_kid0(WN_kid(loop_wn, 1))) == OPR_INTCONST)
    *start = WN_const_val(WN_kid0(WN_kid(loop_wn, 1)));
  else
    return FALSE;

  temp_stride = 1;
  if (WN_operator(WN_kid(loop_wn, 2)) == OPR_LE &&
      WN_operator(WN_kid1(WN_kid(loop_wn, 2))) == OPR_INTCONST)
  {
    if (WN_operator(WN_kid0(WN_kid(loop_wn, 2))) == OPR_LDID &&
        WN_st(WN_kid0(WN_kid(loop_wn, 2))) == *loop_index_st)
      *end = WN_const_val(WN_kid1(WN_kid(loop_wn, 2)));
    else if (WN_operator(WN_kid0(WN_kid(loop_wn, 2))) == OPR_MPY &&
             WN_operator(WN_kid0(WN_kid0(WN_kid(loop_wn, 2)))) == OPR_LDID &&
             WN_st(WN_kid0(WN_kid0(WN_kid(loop_wn, 2)))) == *loop_index_st &&
             WN_operator(WN_kid1(WN_kid0(WN_kid(loop_wn, 2)))) == OPR_INTCONST)
    {
      // index * c1 <= c2
      *end = WN_const_val(WN_kid1(WN_kid(loop_wn, 2)));
      temp_stride = WN_const_val(WN_kid1(WN_kid0(WN_kid(loop_wn, 2))));
    }
    else if (WN_operator(WN_kid0(WN_kid(loop_wn, 2))) == OPR_ADD &&
             WN_operator(WN_kid1(WN_kid0(WN_kid(loop_wn, 2)))) ==
               OPR_INTCONST &&
             WN_operator(WN_kid0(WN_kid0(WN_kid(loop_wn, 2)))) == OPR_MPY &&
             WN_operator(WN_kid0(WN_kid0(WN_kid0(WN_kid(loop_wn, 2))))) ==
               OPR_LDID &&
             WN_st(WN_kid0(WN_kid0(WN_kid0(WN_kid(loop_wn, 2))))) ==
               *loop_index_st &&
             WN_operator(WN_kid1(WN_kid0(WN_kid0(WN_kid(loop_wn, 2))))) ==
               OPR_INTCONST)
    {
      // index * c1 + c2 <= c3
      *end = WN_const_val(WN_kid1(WN_kid(loop_wn, 2))) -
             WN_const_val(WN_kid1(WN_kid0(WN_kid(loop_wn, 2))));
      temp_stride = WN_const_val(WN_kid1(WN_kid0(WN_kid0(WN_kid(loop_wn, 2)))));
    }
    else
      return FALSE;
  }
  else
    return FALSE;
  if (WN_operator(WN_kid(loop_wn, 3)) == OPR_STID &&
      WN_st(WN_kid(loop_wn, 3)) == *loop_index_st &&
      WN_operator(WN_kid0(WN_kid(loop_wn, 3))) == OPR_ADD &&
      WN_operator(WN_kid0(WN_kid0(WN_kid(loop_wn, 3)))) == OPR_LDID &&
      WN_st(WN_kid0(WN_kid0(WN_kid(loop_wn, 3)))) == *loop_index_st &&
      WN_operator(WN_kid1(WN_kid0(WN_kid(loop_wn, 3)))) == OPR_INTCONST)
    *stride = temp_stride * WN_const_val(WN_kid1(WN_kid0(WN_kid(loop_wn, 3))));
  else
    return FALSE;
  // everything is set, we can now return TRUE
  return TRUE;
}

// This function visits the input wn looking for OPR_DIV and OPR_REM and tries
// to simplify them.  Caution on the OPR_REM simplification:  this function will
// always return the positive remainder.
static WN *simplify_wn_for_div_and_rem(WN *wn)
{
  WN *wn0;
  WN *wn1;
  WN *constant_wn;

  if (wn == NULL)
    return wn;
  switch (WN_operator(wn))
  {
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
      wn0 = simplify_wn_for_div_and_rem(WN_kid0(wn));
      wn1 = simplify_wn_for_div_and_rem(WN_kid1(wn));
      if (wn0 != WN_kid0(wn) || wn1 != WN_kid1(wn))
        // something was changed
        return WN_CreateExp2(OPCODE_make_op(WN_operator(wn), WN_rtype(wn),
          WN_desc(wn)), WN_COPY_Tree(wn0), WN_COPY_Tree(wn1));
      break;
    case OPR_CVT:
      wn0 = simplify_wn_for_div_and_rem(WN_kid0(wn));
      if (wn0 != WN_kid0(wn))
        // something was changed
        return WN_CreateExp1(OPCODE_make_op(WN_operator(wn), WN_rtype(wn),
          WN_desc(wn)), WN_COPY_Tree(wn0));
      break;
    case OPR_DIV:
      wn0 = simplify_wn_for_div_and_rem(WN_kid0(wn));
      wn1 = simplify_wn_for_div_and_rem(WN_kid1(wn));
      if (WN_operator(wn1) == OPR_INTCONST &&
          WN_operator(wn0) == OPR_MPY &&
          WN_operator(WN_kid1(wn0)) == OPR_INTCONST &&
          WN_const_val(wn1) == WN_const_val(WN_kid1(wn0)) &&
          WN_const_val(wn1) > 0)
        // (x * c) / c => x
        return WN_COPY_Tree(WN_kid0(wn0));
      if (WN_operator(wn1) == OPR_INTCONST &&
          (WN_operator(wn0) == OPR_ADD || WN_operator(wn0) == OPR_SUB) &&
          WN_operator(WN_kid0(wn0)) == OPR_MPY &&
          WN_operator(WN_kid1(wn0)) == OPR_INTCONST &&
          WN_operator(WN_kid1(WN_kid0(wn0))) == OPR_INTCONST &&
          WN_const_val(WN_kid1(WN_kid0(wn0))) == WN_const_val(wn1) &&
          WN_const_val(wn1) > 0 &&
          WN_const_val(WN_kid1(wn0)) >= 0)
      {
        // ((x * c) +/- c1) / c => x +/- c1 / c
        constant_wn = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, MTYPE_U4,
          MTYPE_V), (INT)(WN_const_val(WN_kid1(wn0)) / WN_const_val(wn1)));
        return WN_CreateExp2(OPCODE_make_op(WN_operator(wn0), WN_rtype(wn0),
          WN_desc(wn0)), WN_COPY_Tree(WN_kid0(WN_kid0(wn0))), constant_wn);
      }
      if (wn0 != WN_kid0(wn) || wn1 != WN_kid1(wn))
        // something was changed
        return WN_CreateExp2(OPCODE_make_op(WN_operator(wn), WN_rtype(wn),
          WN_desc(wn)), WN_COPY_Tree(wn0), WN_COPY_Tree(wn1));
      break;
    case OPR_REM:
      wn0 = simplify_wn_for_div_and_rem(WN_kid0(wn));
      wn1 = simplify_wn_for_div_and_rem(WN_kid1(wn));
      if (WN_operator(wn1) == OPR_INTCONST &&
          WN_operator(wn0) == OPR_MPY &&
          WN_operator(WN_kid1(wn0)) == OPR_INTCONST &&
          WN_const_val(wn1) == WN_const_val(WN_kid1(wn0)) &&
          WN_const_val(wn1) > 0)
        // (x * c) % c => 0
        return WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, MTYPE_U4,
          MTYPE_V), (INT)0);
      if (WN_operator(wn1) == OPR_INTCONST &&
          (WN_operator(wn0) == OPR_ADD || WN_operator(wn0) == OPR_SUB) &&
          WN_operator(WN_kid0(wn0)) == OPR_MPY &&
          WN_operator(WN_kid1(wn0)) == OPR_INTCONST &&
          WN_operator(WN_kid1(WN_kid0(wn0))) == OPR_INTCONST &&
          WN_const_val(WN_kid1(WN_kid0(wn0))) == WN_const_val(wn1) &&
          WN_const_val(wn1) > 0 &&
          WN_const_val(WN_kid1(wn0)) >= 0)
        // ((x * c) +/- c1) % c => c1 % c
        // note:  this will always return a positive remainder
        return WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, MTYPE_U4,
          MTYPE_V), (INT)(WN_const_val(WN_kid1(wn0)) % WN_const_val(wn1)));
      if (wn0 != WN_kid0(wn) || wn1 != WN_kid1(wn))
        // something was changed
        return WN_CreateExp2(OPCODE_make_op(WN_operator(wn), WN_rtype(wn),
          WN_desc(wn)), WN_COPY_Tree(wn0), WN_COPY_Tree(wn1));
      break;
    default:
      break;
  }
  return wn;
}

// The first step in determining if we can safely remap an array candidate is to
// check if we understand how this array was malloced.  This way, we can collect
// information on the size of the array, whether it has padding for the margins
// (pre- and post-array body), etc.  Traverse the input wn to mark all the
// arrays that have been nicely malloced this way.
static void traverse_wn_tree_for_array_remapping_legality_1(WN *wn,
  int *argument_num, WN *function_wn)
{
  if (wn == NULL || continue_with_array_remapping == 0)
    return; // nothing to do
  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        traverse_wn_tree_for_array_remapping_legality_1(child_wn, argument_num,
          function_wn);
        if (continue_with_array_remapping == 0)
          return;
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
        {
          traverse_wn_tree_for_array_remapping_legality_1(child_wn,
            argument_num, function_wn);
          if (continue_with_array_remapping == 0)
            return;
        }
      }
    }
  }
  switch (WN_operator(wn))
  {
    case OPR_LDID:
    case OPR_STID:
      // (overly-stringent) restriction:  if there exists one array remapping
      // candidate of scalar type "t", we require that *all* arrays of type "t"
      // to be array remapping candidates also.  This is purely to simplify
      // implementation.  If not, we will have to keep track of each individual
      // array remapping candidate, and that may involve constructing an
      // equivalence class of these arrays.  (Keep in mind that arrays can be
      // assigned (b = array_remapping_candidate_a) or passed around as
      // arguments).  The following records this type information
      int mtype;
      int mtype_size;

      if (ST_is_array_remapping_candidate(WN_st(wn)))
      {
        // an array is simply a pointer to a scalar type
        if (TY_kind(WN_ty(wn)) == KIND_POINTER &&
            TY_kind(TY_pointed(WN_ty(wn))) == KIND_SCALAR)
        {
          mtype = TY_mtype(TY_pointed(WN_ty(wn)));
          mtype_size = TY_size(TY_pointed(WN_ty(wn)));
        }
        else
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable -1\n");
          continue_with_array_remapping = 0;
          return;
        }
        if (array_remapping_candidate_mtype == 0)
        {
          array_remapping_candidate_mtype = mtype; // first time
          array_remapping_candidate_mtype_size = mtype_size; // first time
        }
        else if (array_remapping_candidate_mtype != mtype ||
                 array_remapping_candidate_mtype_size != mtype_size)
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable -2\n");
          continue_with_array_remapping = 0;
          return;
        }
      }
      break;

    case OPR_CALL:
      INT need_to_check_parms;
      ST *malloc_st;
      ST *temp_malloc_st;
      INT malloc_size;
      INT pre_margin_size;
      WN *wn1;
      WN *wn2;
      WN *wn3;

      if (strcmp(ST_name(WN_st(wn)), "malloc") == 0)
      {
        wn1 = WN_next(wn);
        if (!(WN_operator(wn1) == OPR_STID &&
              WN_operator(WN_kid0(wn1)) == OPR_LDID))
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable -3\n");
          continue_with_array_remapping = 0;
          return; // not a regular malloc
        }
        need_to_check_parms = 0;
        *argument_num = -1;
        malloc_st = WN_st(wn1);
        if (ST_class(malloc_st) == CLASS_VAR)
          wn3 = wn1; // wn3 = malloc()
        else
        {
          wn2 = WN_next(wn1);
          if (WN_operator(wn2) == OPR_STID &&
              WN_operator(WN_kid0(wn2)) == OPR_LDID &&
              WN_st(WN_kid0(wn2)) == malloc_st &&
              ST_class(WN_st(wn2)) == CLASS_VAR)
            malloc_st = WN_st(wn2); // wn2 = wn1 = malloc()
          else if (WN_operator(wn2) == OPR_ISTORE &&
                   WN_operator(WN_kid0(wn2)) == OPR_LDID &&
                   WN_st(WN_kid0(wn2)) == malloc_st &&
                   WN_has_sym(WN_kid1(wn2)) &&
                   WN_st(WN_kid1(wn2)) != NULL &&
                   ST_class(WN_st(WN_kid1(wn2))) == CLASS_VAR)
          {
            temp_malloc_st = malloc_st;
            malloc_st = WN_st(WN_kid1(wn2)); // *(malloc_st) =
                                               // temp_malloc_st = malloc()
            need_to_check_parms = 1;
          }
          else
          {
            // we may have to be more aggressive in the future
            if (Get_Trace(TP_IPA, 1))
              fprintf(TFile, "ipo -> array remapping disable -4\n");
            continue_with_array_remapping = 0;
            return;
          }
          wn3 = wn2; // wn3 = malloc()
        }
         
        // determine the malloced size and pre-margin size
        if (!(WN_operator(WN_kid0(wn)) == OPR_PARM &&
              WN_operator(WN_kid0(WN_kid0(wn))) == OPR_INTCONST))
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable -5\n");
          continue_with_array_remapping = 0;
          return;
        }
        malloc_size = WN_const_val(WN_kid0(WN_kid0(wn)));
        pre_margin_size = 0;
        // look for an assignment such as malloc_st = malloc_st + constant
        wn3 = WN_next(wn3);
        while (wn3 != NULL)
        {
          if (need_to_check_parms == 0 &&
              WN_operator(wn3) == OPR_STID &&
              WN_st(wn3) == malloc_st &&
              WN_operator(WN_kid0(wn3)) == OPR_ADD &&
              WN_operator(WN_kid0(WN_kid0(wn3))) == OPR_LDID &&
              WN_st(WN_kid0(WN_kid0(wn3))) == malloc_st &&
              WN_operator(WN_kid1(WN_kid0(wn3))) == OPR_INTCONST)
          {
            pre_margin_size += WN_const_val(WN_kid1(WN_kid0(wn3))); // allow the
              // pre-margin size to be accumulative
            wn3 = WN_next(wn3);
          }
          else if (need_to_check_parms == 1 &&
                   WN_operator(wn3) == OPR_ISTORE &&
                   WN_operator(WN_kid1(wn3)) == OPR_LDID &&
                   WN_st(WN_kid1(wn3)) == malloc_st &&
                   WN_operator(WN_kid0(wn3)) == OPR_ADD &&
                   WN_operator(WN_kid0(WN_kid0(wn3))) == OPR_LDID &&
                   WN_st(WN_kid0(WN_kid0(wn3))) == temp_malloc_st &&
                   WN_operator(WN_kid1(WN_kid0(wn3))) == OPR_INTCONST)
          {
            pre_margin_size += WN_const_val(WN_kid1(WN_kid0(wn3)));
            wn3 = WN_next(wn3);
          }
          else if (WN_operator(wn3) == OPR_IF ||
                   WN_operator(wn3) == OPR_PRAGMA)
            wn3 = WN_next(wn3); // what if the increment is inside the if?
          else
            break; // encountered other operators
        }
        // the above code to look for pre-margin is not exhaustive; so what if
        // the pre-margin thus determined is not correct?  We should note that
        // the more accurate the pre-margin is the more efficient the array
        // array remapping will be, but as long as the "map" is a bijection, it
        // will still be correct.  As an additional precaution, since we are
        // remapping all the arrays of type "t" (see stringent requirement
        // above), we require that all such malloced sizes and pre-margins to be
        // identical
        if (array_remapping_malloc_size == 0)
          array_remapping_malloc_size = malloc_size; // first time
        if (array_remapping_pre_margin_size == 0)
          array_remapping_pre_margin_size = pre_margin_size; // first time
        if (array_remapping_malloc_size != malloc_size ||
            array_remapping_pre_margin_size != pre_margin_size)
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable -6\n");
          continue_with_array_remapping = 0;
          return;
        }
        // mark malloc_st
        Set_ST_is_array_remapping_candidate_malloc(malloc_st);
        // but what if malloc_st is a passed-in argument?  In this case we will
        // need to mark the corresponding argument in the caller also.  We
        // simply do this by setting the argument_num and let the caller of this
        // function do what needs to be done
        if (need_to_check_parms == 1)
        {
          *argument_num = get_argument_num(malloc_st, function_wn);
          if (*argument_num < 0)
          {
            if (Get_Trace(TP_IPA, 1))
              fprintf(TFile, "ipo -> array remapping disable -7\n");
            continue_with_array_remapping = 0;
            return;
          }
        }
      }
      break;

    default:
      break;
  }
  return;
}

// The second step in determining if we can safely remap an array candidate is
// to traverse the input wn and see if all the candidates have been correctly
// malloced, and that there are no unexpected constructs that may render it
// unsafe to remap the candidates.
static void traverse_wn_tree_for_array_remapping_legality_2(WN *wn,
  WN *function_wn, IPA_NODE *node, WN *loop_wn)
{
  if (wn == NULL || continue_with_array_remapping == 0)
    return; // nothing to do
  if (array_remapping_candidate_mtype == 0 ||
      array_remapping_candidate_mtype_size == 0 ||
      array_remapping_malloc_size == 0)
  {
    // traverse_wn_tree_for_array_remapping_legality_1 phase should have set
    // these variables
    if (Get_Trace(TP_IPA, 1))
      fprintf(TFile, "ipo -> array remapping disable 0\n");
    continue_with_array_remapping = 0;
    return;
  }

  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        traverse_wn_tree_for_array_remapping_legality_2(child_wn, function_wn,
          node, loop_wn);
        if (continue_with_array_remapping == 0)
          return;
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      if (WN_operator(wn) == OPR_DO_LOOP ||
          WN_operator(wn) == OPR_WHILE_DO ||
          WN_operator(wn) == OPR_DO_WHILE)
      {
        // found a loop.  Send loop information along down to
        // traverse_wn_tree_for_array_remapping_legality_2 because if it is the
        // outermost loop, there are additional legality checks
        array_remapping_loop_depth++;
        if (array_remapping_loop_depth == 1)
          loop_wn = wn; // update the loop_wn
      }
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
        {
          traverse_wn_tree_for_array_remapping_legality_2(child_wn, function_wn,
            node, loop_wn);
          if (continue_with_array_remapping == 0)
            return;
        }
      }
      if (WN_operator(wn) == OPR_DO_LOOP ||
          WN_operator(wn) == OPR_WHILE_DO ||
          WN_operator(wn) == OPR_DO_WHILE)
        array_remapping_loop_depth--; // reset array_remapping_loop_depth
    }
  }

  switch (WN_operator(wn))
  {
    WN *array_index_mult_wn;
    WN *array_element_size_wn;
    int array_element_size;
    ST *array_st;
    int argument_num;
    WN *wn1;

    // this section of code has to be in sync with the corresponding section of
    // code in the function traverse_wn_tree_for_array_remapping because the
    // latter function will assume that all the array remapping candidates have
    // passed the legality tests that are happening right here
    case OPR_ILOAD:
    case OPR_ISTORE:
      if (WN_operator(wn) == OPR_ILOAD) 
        wn1 = WN_kid0(wn);
      else
        wn1 = WN_kid1(wn);
      // looking for array[]
      if (wn_can_be_array_remapping_candidate(wn1) == FALSE)
        return; // not a problem
      if (WN_operator(wn1) == OPR_ADD)
      {
        if (wn_can_be_array_remapping_candidate(WN_kid0(wn1)) == FALSE ||
            wn_can_be_array_remapping_candidate(WN_kid1(wn1)) == FALSE)
          return; // not a problem
      }
      if (WN_operator(wn1) == OPR_SUB)
      {
        if (wn_can_be_array_remapping_candidate(WN_kid0(wn1)) == FALSE)
          return; // not a problem
      }
      // wn1 is an array remapping candidate; see if it can pass the legality
      // tests
      if (!(WN_operator(wn1) == OPR_ADD &&
            WN_operator(WN_kid0(wn1)) == OPR_LDID &&
            WN_operator(WN_kid1(wn1)) == OPR_MPY &&
            TY_kind(WN_ty(WN_kid0(wn1))) == KIND_POINTER &&
            TY_kind(TY_pointed(WN_ty(WN_kid0(wn1)))) == KIND_SCALAR &&
            TY_mtype(TY_pointed(WN_ty(WN_kid0(wn1)))) ==
              array_remapping_candidate_mtype &&
            TY_size(TY_pointed(WN_ty(WN_kid0(wn1)))) ==
              array_remapping_candidate_mtype_size))
      {
        if (Get_Trace(TP_IPA, 1))
          fprintf(TFile, "ipo -> array remapping disable 1\n");
        continue_with_array_remapping = 0;
        return;
      }
      array_index_mult_wn = WN_kid1(wn1);
      if (WN_operator(WN_kid0(array_index_mult_wn)) == OPR_INTCONST)
      {
        // array_index_mult_wn = constant * index
        array_element_size_wn = WN_kid0(array_index_mult_wn);
        array_element_size = WN_const_val(array_element_size_wn);
        if (array_element_size % array_remapping_candidate_mtype_size != 0)
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable 2\n");
          continue_with_array_remapping = 0;
          return;
        }
      }
      else if (WN_operator(WN_kid1(array_index_mult_wn)) == OPR_INTCONST)
      {
        // array_index_mult_wn = index * constant
        array_element_size_wn = WN_kid1(array_index_mult_wn);
        array_element_size = WN_const_val(array_element_size_wn);
        if (array_element_size % array_remapping_candidate_mtype_size != 0)
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable 3\n");
          continue_with_array_remapping = 0;
          return;
        }
      }
      else
      {
        if (Get_Trace(TP_IPA, 1))
          fprintf(TFile, "ipo -> array remapping disable 4\n");
        continue_with_array_remapping = 0;
        return;
      }
      if (WN_offset(wn) % array_remapping_candidate_mtype_size != 0)
      {
        if (Get_Trace(TP_IPA, 1))
          fprintf(TFile, "ipo -> array remapping disable 5\n");
        continue_with_array_remapping = 0;
        return;
      }

      array_st = WN_st(WN_kid0(wn1));
      // array_st is an array remapping candidate, it must have been malloced
      // (and hence accordingly marked by
      // traverse_wn_tree_for_array_remapping_legality_1)
      if (!ST_is_array_remapping_candidate_malloc(array_st))
      {
        // perhaps the array is passed in as an argument and thus has not been
        // propagated yet
        argument_num = get_argument_num(array_st, function_wn);
        if (argument_num >= 0)
        {
          // the array remapping candidate is indeed passed in as an arugment
          if (argument_in_callers_is_array_remapping_candidate_malloc(node,
              argument_num))
            // propagate the property that this array remapping candidate has
            // been malloced
            Set_ST_is_array_remapping_candidate_malloc(array_st);
          else
          {
            if (Get_Trace(TP_IPA, 1))
              fprintf(TFile, "ipo -> array remapping disable 6\n");
            continue_with_array_remapping = 0;
            return;
          }
        }
        else
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable 7\n");
          continue_with_array_remapping = 0;
          return;
        }
      }

      // no legality restriction on the array index, but if this array remapping
      // candidate is used inside an outermost loop, make sure that the loop
      // iteration count and the group number are consistent throughout
      if (array_remapping_loop_depth == 1)
      {
        if (loop_wn != NULL && WN_operator(loop_wn) == OPR_DO_LOOP)
        {
          BOOL loop_info_status;
          ST *loop_index_st;
          int start;
          int end;
          int stride;
          int loop_num_iterations;

          loop_info_status = get_do_loop_info(loop_wn, &loop_index_st, &start,
            &end, &stride);
          if (loop_info_status == FALSE)
          {
            if (Get_Trace(TP_IPA, 1))
              fprintf(TFile, "ipo -> array remapping disable 8\n");
            continue_with_array_remapping = 0;
            return;
          }
          if (!(start >= 0 && end > 0 && end >= start && stride > 0))
          {
            if (Get_Trace(TP_IPA, 1))
              fprintf(TFile, "ipo -> array remapping disable 9\n");
            continue_with_array_remapping = 0;
            return;
          }
          loop_num_iterations = (end - start + stride) / stride;
          if (array_remapping_num_groups == 0)
            array_remapping_num_groups = stride; // first time
          if (array_remapping_loop_num_iterations == 0)
            array_remapping_loop_num_iterations = loop_num_iterations; // first
                                                                       // time
          if (loop_num_iterations > array_remapping_loop_num_iterations)
            array_remapping_loop_num_iterations = loop_num_iterations;
          if (array_remapping_num_groups != stride)
          {
            if (Get_Trace(TP_IPA, 1))
              fprintf(TFile, "ipo -> array remapping disable 10\n");
            continue_with_array_remapping = 0;
            return;
          }
        }
        else
        {
          if (Get_Trace(TP_IPA, 1))
            fprintf(TFile, "ipo -> array remapping disable 11\n");
          continue_with_array_remapping = 0;
          return;
        }
      }

      break;

    case OPR_LDID:
    case OPR_STID:
      // what's not allowed?  dereferencing array not going through iload/store
      // walk up tree and only allow the familiar ops?
      break;

    default:
      break;
  }

  return;
}

// We already know that it is legal (safe) to do array remapping, now perform
// the optimization.
static void traverse_wn_tree_for_array_remapping(WN *wn)
{
  if (wn == NULL)
    return; // nothing to do

  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        traverse_wn_tree_for_array_remapping(child_wn);
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
          traverse_wn_tree_for_array_remapping(child_wn);
      }
    }
  }

  // nodes that are of interest
  switch (WN_operator(wn))
  {
    WN *num_elements_in_pre_margin_wn;
    WN *num_elements_in_array_wn;
    WN *num_elements_in_post_margin_wn;
    WN *num_elements_in_group_wn;
    WN *array_index_mult_wn;
    WN *array_element_size_wn;
    int array_element_size;
    WN *array_index_wn;
    WN *anchor_wn;
    int anchor_kid_num;
    TYPE_ID rtype;
    TYPE_ID desc;
    WN *pre_margin_mult_wn;
    WN *new_wn;
    WN *mod_wn;
    WN *div_wn;
    WN *wn1;

    case OPR_ILOAD:
    case OPR_ISTORE:
      if (WN_operator(wn) == OPR_ILOAD) 
        wn1 = WN_kid0(wn);
      else
        wn1 = WN_kid1(wn);
      // looking for array[]
      if (wn_can_be_array_remapping_candidate(wn1) == FALSE)
        return; // not a problem
      if (WN_operator(wn1) == OPR_ADD)
      {
        if (wn_can_be_array_remapping_candidate(WN_kid0(wn1)) == FALSE ||
            wn_can_be_array_remapping_candidate(WN_kid1(wn1)) == FALSE)
          return; // not a problem
      }
      if (WN_operator(wn1) == OPR_SUB)
      {
        if (wn_can_be_array_remapping_candidate(WN_kid0(wn1)) == FALSE)
          return; // not a problem
      }
      // wn1 is an array remapping candidate; moreover, it has passed the
      // legality tests
      if (!(WN_operator(wn1) == OPR_ADD &&
            WN_operator(WN_kid0(wn1)) == OPR_LDID &&
            WN_operator(WN_kid1(wn1)) == OPR_MPY &&
            TY_mtype(TY_pointed(WN_ty(WN_kid0(wn1)))) ==
              array_remapping_candidate_mtype))
      {
        // legality tests assure us that this is not possible
        FmtAssert(FALSE, ("Illegal array remapping construct 1."));
      }
      num_elements_in_pre_margin_wn = WN_CreateIntconst(OPCODE_make_op
        (OPR_INTCONST, MTYPE_U4, MTYPE_V),
        (INT)array_remapping_num_elements_in_pre_margin);
      num_elements_in_array_wn = WN_CreateIntconst(OPCODE_make_op
        (OPR_INTCONST, MTYPE_U4, MTYPE_V),
        (INT)array_remapping_num_elements_in_array);
      num_elements_in_post_margin_wn = WN_CreateIntconst(OPCODE_make_op
        (OPR_INTCONST, MTYPE_U4, MTYPE_V),
        (INT)array_remapping_num_elements_in_post_margin);
      num_elements_in_group_wn = WN_CreateIntconst(OPCODE_make_op
        (OPR_INTCONST, MTYPE_U4, MTYPE_V),
        (INT)array_remapping_num_elements_in_group);
      array_index_mult_wn = WN_kid1(wn1);
      if (WN_operator(WN_kid0(array_index_mult_wn)) == OPR_INTCONST)
      {
        // array_index_mult_wn = constant * index
        array_element_size_wn = WN_kid0(array_index_mult_wn);
        array_element_size = WN_const_val(array_element_size_wn);
        array_index_wn = WN_kid1(array_index_mult_wn);
        anchor_wn = array_index_mult_wn;
        anchor_kid_num = 1;
        if (WN_operator(array_index_wn) == OPR_CVT)
        {
          // move the anchor down
          anchor_wn = array_index_wn;
          anchor_kid_num = 0;
          array_index_wn = WN_kid0(array_index_wn);
        }
        rtype = WN_rtype(array_index_wn);
        desc = WN_desc(array_index_wn);
        if (array_element_size % array_remapping_candidate_mtype_size != 0)
        {
          // legality tests assure us that this is not possible
          FmtAssert(FALSE, ("Illegal array remapping construct 2."));
        }
        if (array_element_size != array_remapping_candidate_mtype_size)
        {
          // normalize (array_element_size * index) to
          // (array_remapping_candidate_mtype_size * (index *
          // array_element_size / array_remapping_candidate_mtype_size))
          array_index_wn = WN_CreateExp2(OPCODE_make_op(OPR_MPY, rtype, desc),
            WN_COPY_Tree(array_index_wn), WN_CreateIntconst(OPCODE_make_op
            (OPR_INTCONST, MTYPE_U4, MTYPE_V), (INT)(array_element_size /
            array_remapping_candidate_mtype_size)));
          WN_kid0(array_index_mult_wn) = WN_CreateIntconst(OPCODE_make_op
            (OPR_INTCONST, MTYPE_U4, MTYPE_V),
            (INT)array_remapping_candidate_mtype_size);
        }
      }
      else if (WN_operator(WN_kid1(array_index_mult_wn)) == OPR_INTCONST)
      {
        // array_index_mult_wn = index * constant
        array_element_size_wn = WN_kid1(array_index_mult_wn);
        array_element_size = WN_const_val(array_element_size_wn);
        array_index_wn = WN_kid0(array_index_mult_wn);
        anchor_wn = array_index_mult_wn;
        anchor_kid_num = 0;
        if (WN_operator(array_index_wn) == OPR_CVT)
        {
          // move the anchor down
          anchor_wn = array_index_wn;
          anchor_kid_num = 0;
          array_index_wn = WN_kid0(array_index_wn);
        }
        rtype = WN_rtype(array_index_wn);
        desc = WN_desc(array_index_wn);
        if (array_element_size % array_remapping_candidate_mtype_size != 0)
        {
          // legality tests assure us that this is not possible
          FmtAssert(FALSE, ("Illegal array remapping construct 3."));
        }
        if (array_element_size != array_remapping_candidate_mtype_size)
        {
          // normalize (index * array_element_size) to ((index *
          // array_element_size / array_remapping_candidate_mtype_size) *
          // array_remapping_candidate_mtype_size)
          array_index_wn = WN_CreateExp2(OPCODE_make_op(OPR_MPY, rtype, desc),
            WN_COPY_Tree(array_index_wn), WN_CreateIntconst(OPCODE_make_op
            (OPR_INTCONST, MTYPE_U4, MTYPE_V), (INT)(array_element_size /
            array_remapping_candidate_mtype_size)));
          WN_kid1(array_index_mult_wn) = WN_CreateIntconst(OPCODE_make_op
            (OPR_INTCONST, MTYPE_U4, MTYPE_V),
            (INT)array_remapping_candidate_mtype_size);
        }
      }
      else
      {
        // legality tests assure us that this is not possible
        FmtAssert(FALSE, ("Illegal array remapping construct 4."));
      }
      if (WN_offset(wn) % array_remapping_candidate_mtype_size != 0)
      {
        // legality tests assure us that this is not possible
        FmtAssert(FALSE, ("Illegal array remapping construct 5."));
      }
      if (WN_offset(wn) != 0)
      {
        // normalize offset to 0 by adding offset /
        // array_remapping_candidate_mtype_size to the index
        array_index_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, rtype, desc),
          WN_COPY_Tree(array_index_wn), WN_CreateIntconst(OPCODE_make_op
          (OPR_INTCONST, MTYPE_U4, MTYPE_V), (INT)(WN_offset(wn) /
          array_remapping_candidate_mtype_size)));
        WN_offset(wn) = 0;
      }
      // now perform the array remapping transformation:
      // array[index] -> array[((index +
      //                         array_remapping_num_elements_in_pre_margin *
      //                         array_remapping_num_elements_in_group) %
      //                        array_remapping_num_elements_in_group) *
      //                       (array_remapping_num_elements_in_array +
      //                        array_remapping_num_elements_in_pre_margin +
      //                        array_remapping_num_elements_in_post_margin) +
      //                       ((index +
      //                         array_remapping_num_elements_in_pre_margin *
      //                         array_remapping_num_elements_in_group) /
      //                        array_remapping_num_elements_in_group) -
      //                       array_remapping_num_elements_in_pre_margin *
      //                       array_remapping_num_elements_in_group]
      pre_margin_mult_wn = WN_CreateExp2(OPCODE_make_op(OPR_MPY, rtype, desc),
        WN_COPY_Tree(num_elements_in_pre_margin_wn), WN_COPY_Tree
        (num_elements_in_group_wn));
      new_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, rtype, desc),
        WN_COPY_Tree(array_index_wn), WN_COPY_Tree(pre_margin_mult_wn));
      mod_wn = WN_CreateExp2(OPCODE_make_op(OPR_REM, rtype, desc),
        WN_COPY_Tree(new_wn), WN_COPY_Tree(num_elements_in_group_wn));
      div_wn = WN_CreateExp2(OPCODE_make_op(OPR_DIV, rtype, desc),
        WN_COPY_Tree(new_wn), WN_COPY_Tree(num_elements_in_group_wn));
      new_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, rtype, desc),
        num_elements_in_array_wn, WN_COPY_Tree(num_elements_in_pre_margin_wn));
      new_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, rtype, desc), new_wn,
        num_elements_in_post_margin_wn);
      new_wn = WN_CreateExp2(OPCODE_make_op(OPR_MPY, rtype, desc), mod_wn,
        new_wn);
      new_wn = WN_CreateExp2(OPCODE_make_op(OPR_ADD, rtype, desc), div_wn,
        new_wn);
      new_wn = WN_CreateExp2(OPCODE_make_op(OPR_SUB, rtype, desc), new_wn,
        WN_COPY_Tree(pre_margin_mult_wn));
      // since the array remapping optimization guarantees that the remainders
      // of interest are always positive, we can perform further simplification
      new_wn = simplify_wn_for_div_and_rem(new_wn);
      // now transform the subscript
      WN_kid(anchor_wn, anchor_kid_num) = new_wn;
      if (Get_Trace(TP_IPA, 1))
        fprintf(TFile, "ipo -> array remapping for %s\n",
          ST_name(WN_st(WN_kid0(wn1))));
      break;

    default:
      break;
  }

  return;
}

extern INT struct_field_count(TY_IDX);
static BOOL new_types_created = FALSE;

void IPO_generate_new_types (mUINT32 ty)
{
  TY_IDX orig_ty = ty << 8;
  new_types_created = TRUE;

  INT field_count = struct_field_count (orig_ty);

  INT idx = 0;
  INT type_count = 0;

  while (idx < field_count)
  {
    const INT struct_id = Struct_field_layout[idx].u.struct_id;
    BOOL create_new_ty = TRUE;
    FLD_HANDLE last_fld = FLD_HANDLE();
    INT offset = 0;
    TY_IDX new_ty = TY_IDX_ZERO;

    // Get the current field from the original struct that is to be
    // inserted first in the new struct, or to be handled as a separate
    // field by itself.
    UINT cur_field_id = 0;
    FLD_HANDLE orig = FLD_get_to_field (orig_ty, idx+1, cur_field_id);

    for (INT i=idx+1; i<field_count; i++)
    {
      if (struct_id == Struct_field_layout[i].u.struct_id)
      {
        // Got multiple fields together, so create a struct TY if not
        // already done.
        if (create_new_ty)
        {
          // Create first field
          FLD_HANDLE fld = New_FLD();
          memcpy (fld.Entry(), orig.Entry(), sizeof(FLD));
          Is_True (offset == 0, ("First field should have offset zero"));
          Set_FLD_ofst (fld, offset);
          offset += TY_size(FLD_type(fld));
          // Create new struct TY
          // The name of the new type ends in a number which is the
          // field-id of the field in the original struct (this number
          // is just for readability purposes).
          TY_Init (New_TY(new_ty), sizeof(int) /* dummy size */,
                   KIND_STRUCT, MTYPE_M,
                   Save_Str2i(TY_name(orig_ty), "..", idx+1));
          Set_TY_fld(new_ty, fld);
          create_new_ty = FALSE;
        }
        cur_field_id = 0;
        orig = FLD_get_to_field (orig_ty, i+1, cur_field_id);
        // Create current field
        FLD_HANDLE fld = New_FLD();
        memcpy (fld.Entry(), orig.Entry(), sizeof(FLD));
        INT align = TY_align(FLD_type(fld));
        offset = (INT)ceil(((double)offset)/align)*align;
        Set_FLD_ofst (fld, offset);
        offset += TY_size(FLD_type(fld));
        last_fld = fld; // last field in new struct
      }
    }

    if (create_new_ty)
    {
      // New struct TY has not been created. So this will be a single field
      // by itself.
      new_ty = FLD_type(orig);
    }
    else
    {
      // Struct TY
      Is_True (new_ty != TY_IDX_ZERO, (""));
      Is_True (TY_kind(new_ty) == KIND_STRUCT, (""));
      Is_True (!last_fld.Is_Null(), (""));
      Set_FLD_last_field (last_fld);
      Set_TY_size (new_ty, offset);
    }

    ST * st = NULL;
    if (IPA_Enable_Struct_Opt == 2)
    {
      st = New_ST(GLOBAL_SYMTAB);
      ST_Init (st, Save_Str2i("aa", "..", idx), CLASS_VAR, SCLASS_COMMON, EXPORT_PREEMPTIBLE, Make_Pointer_Type(new_ty));
    }

    while (idx < field_count &&
           struct_id == Struct_field_layout[idx].u.struct_id)
    {
      if (IPA_Enable_Struct_Opt == 2)
        Struct_field_layout[idx].st_idx = ST_st_idx(st);
      Struct_field_layout[idx++].u.new_ty = new_ty;
    }

    Struct_split_types[type_count++] = new_ty;
  }

  FmtAssert (type_count == Struct_split_count, (""));
  Struct_split_types[type_count] = TY_IDX_ZERO;
}

static BOOL fld_table_updated = FALSE;

// Update field table before whirl update.
void IPO_Fld_Table_Update_For_Struct_Opt (mUINT32 ty)
{
  fld_table_updated = TRUE;

  mUINT32 old_field_id, new_field_id;
  WN_OFFSET old_ofst, new_ofst;

  old_field_id = new_field_id = 1;
  old_ofst = new_ofst = 0;

  FmtAssert (TY_kind(Ty_tab[ty]) == KIND_STRUCT, ("struct type expected"));

  FLD_ITER fld_iter = Make_fld_iter(TY_fld(Ty_tab[ty]));
  FLD_IDX first_fld_idx = Fld_Table.size();
  FLD_HANDLE last_field;
  BOOL child_flattened = FALSE;

  UINT32 fld_count = 1; // 1 from the nested struct ??
  do
  {
    fld_count++;
  } while (!FLD_last_field(fld_iter++));

  FmtAssert (fld_count > 1, ("This struct should have > 1 field"));

  field_map_info = (FLD_MAP *) malloc (fld_count * sizeof(FLD_MAP));

  fld_iter = Make_fld_iter(TY_fld(Ty_tab[ty]));
  do
  {
    FLD_HANDLE fld(fld_iter);
    FLD_HANDLE new_fld = New_FLD();

    field_map_info[old_field_id-1].old_field_id = old_field_id;
    field_map_info[old_field_id-1].new_field_id = new_field_id;

    if (TY_kind(FLD_type(fld)) == KIND_POINTER &&
        TY_IDX_index(TY_pointed(FLD_type(fld))) == candidate_ty_idx)
    {
      FmtAssert (!child_flattened, ("Unexpected struct-type field"));
      FmtAssert (old_field_id == new_field_id, ("Field update error"));
      candidate_fld_id = old_field_id;
      child_flattened = TRUE;
      UINT32 ofst = FLD_ofst(fld);
      FmtAssert (TY_kind(Ty_tab[candidate_ty_idx]) == KIND_STRUCT, ("struct expected"));
      FLD_HANDLE nested = TY_fld(Ty_tab[candidate_ty_idx]);
      memcpy (&Fld_Table[new_fld.Idx()], &Fld_Table[nested.Idx()], sizeof(FLD));
      FmtAssert (FLD_ofst(new_fld) == 0, ("Offset mismatch"));
      Set_FLD_ofst (new_fld, ofst);
      Set_FLD_type (new_fld, Make_Pointer_Type (FLD_type(new_fld)));

      new_fld = New_FLD();
      new_field_id++;
      memcpy (&Fld_Table[new_fld.Idx()], &Fld_Table[nested.Idx()+1], sizeof(FLD));
      Set_FLD_ofst (new_fld, ofst+Pointer_Size);
      Set_FLD_type (new_fld, Make_Pointer_Type (FLD_type(new_fld)));

      FmtAssert (FLD_last_field(new_fld), ("Last field flag missing"));
      Clear_FLD_last_field(new_fld);
      last_field = new_fld;
    }
    else
    {
      memcpy (&Fld_Table[new_fld.Idx()], &Fld_Table[fld.Idx()], sizeof(FLD));
      if (child_flattened) // we increased the size by a pointer size
        Set_FLD_ofst (new_fld, FLD_ofst(new_fld)+Pointer_Size);
      last_field = new_fld;
    }
    old_field_id++;
    new_field_id++;
  } while (!FLD_last_field(fld_iter++));

  Ty_tab[ty].Set_fld(first_fld_idx);
  Set_TY_size(Ty_tab[ty], TY_size(Ty_tab[ty])+Pointer_Size);
  Set_FLD_last_field(last_field);
}

// Top level routine for optimization
void IPO_WN_Update_For_Struct_Opt (IPA_NODE * node)
{
  if (IPA_Update_Struct)
  {
    // This option will soon be removed, and is kept as a workaround that
    // the user can use to specify the 2 type IDs.
    Struct_split_candidate_index = IPA_Enable_Struct_Opt;
    Struct_update_index = IPA_Update_Struct;
  }

  // before the addition of array remapping ...
  // if (!Struct_split_candidate_index && complete_struct_relayout_type_id == 0)
  //   return; no structure splitting/peeling or complete_struct_relayout to do

  WN * tree = node->Whirl_Tree();
  preg_id = 0;
  FmtAssert (tree, ("Null whirl tree"));

  PU_Parent_Map = node->Parent_Map();
  PU_Map_Tab = node->Map_Table();

  // Map callsites now before we introduce any new calls. If any new
  // calls are introduced here, they are guaranteed to not require any
  // new edge.
  IPA_Call_Graph->Map_Callsites(node);

  // parentize before
  WN_Parentize(tree, PU_Parent_Map, PU_Map_Tab);

  if (Struct_split_candidate_index != 0)
  {
    // structure splitting/peeling

  candidate_ty_idx = Struct_split_candidate_index;
  // Assign candidate_ty_idx here.
  FmtAssert (candidate_ty_idx != 0, ("No TY to optimize"));
  if (Struct_update_index && !fld_table_updated)
    IPO_Fld_Table_Update_For_Struct_Opt (Struct_update_index);

  if (!new_types_created)
    IPO_generate_new_types (Struct_split_candidate_index);
  traverse_wn_tree (NULL, NULL, tree, WN_func_body(tree), 0);
  }
  else
  {
    if (IPA_Enable_Struct_Opt == 0 ||
        complete_struct_relayout_type_id == 0 ||
        continue_with_complete_struct_relayout == 0 ||
        encountered_calloc_for_complete_struct_relayout == 0)
    {
      // before the addition of array remapping ...
      // return; nothing to do
    }
    else
    {
      // perform complete_struct_relayout optimization for this function

      if (global_ptrs_for_complete_struct_relayout_created == 0)
        create_global_ptrs_for_complete_struct_relayout(); // do this once only
      traverse_wn_tree_for_complete_struct_relayout(NULL, NULL, tree,
        WN_func_body(tree), 0);
      if (Get_Trace(TP_IPA, 1))
        fprintf(TFile, "ipo -> complete_struct_relayout for %s\n",
          ST_name(node->Func_ST()));
    }
  }

  // array remapping optimization
  if (IPA_Enable_Struct_Opt != 0 && continue_with_array_remapping != 0)
  {
    if (array_remapping_final_checked == 0)
    {
      // one final check:  make sure the total malloc size for the array
      // remapping candidate is consistent with the pre-margin size, post-margin
      // size, and loop iteration size
      if (array_remapping_malloc_size <=0 ||
          array_remapping_candidate_mtype_size <= 0 ||
          array_remapping_num_groups <= 0 ||
          array_remapping_loop_num_iterations <= 0 ||
          array_remapping_pre_margin_size % (array_remapping_num_groups *
            array_remapping_candidate_mtype_size) != 0)
      {
        if (Get_Trace(TP_IPA, 1))
          fprintf(TFile, "ipo -> array remapping disable 12\n");
        continue_with_array_remapping = 0;
      }
      if (array_remapping_pre_margin_size == 0 &&
          array_remapping_malloc_size == (array_remapping_candidate_mtype_size *
            array_remapping_num_groups * array_remapping_loop_num_iterations))
      {
        array_remapping_num_elements_in_pre_margin =
          array_remapping_pre_margin_size / array_remapping_num_groups /
          array_remapping_candidate_mtype_size;
        array_remapping_num_elements_in_post_margin =
          array_remapping_num_elements_in_pre_margin;
        array_remapping_num_elements_in_group = array_remapping_num_groups;
        array_remapping_num_elements_in_array =
          array_remapping_loop_num_iterations - 2 *
          array_remapping_num_elements_in_post_margin;
      }
      // the following is more exact, but the problem is that sometimes the
      // array_remapping_loop_num_iterations already *include* the pre/post-
      // margin size (e.g., to initialize the *entire* array, etc.)
      /* else if (array_remapping_pre_margin_size != 0 &&
          array_remapping_malloc_size == (array_remapping_candidate_mtype_size *
            array_remapping_num_groups * (array_remapping_loop_num_iterations +
            2 * array_remapping_pre_margin_size / array_remapping_num_groups /
            array_remapping_candidate_mtype_size))) */
      else if (array_remapping_pre_margin_size != 0 &&
          array_remapping_malloc_size == (array_remapping_candidate_mtype_size *
            array_remapping_num_groups * array_remapping_loop_num_iterations))
      {
        array_remapping_num_elements_in_pre_margin =
          array_remapping_pre_margin_size / array_remapping_num_groups /
          array_remapping_candidate_mtype_size;
        array_remapping_num_elements_in_post_margin =
          array_remapping_num_elements_in_pre_margin;
        array_remapping_num_elements_in_group = array_remapping_num_groups;
        array_remapping_num_elements_in_array =
          array_remapping_loop_num_iterations - 2 *
          array_remapping_num_elements_in_post_margin;
      }
      else
      {
        if (Get_Trace(TP_IPA, 1))
          fprintf(TFile, "ipo -> array remapping disable 13\n");
        continue_with_array_remapping = 0;
      }
      array_remapping_final_checked = 1;
    }
    if (continue_with_array_remapping != 0)
      // perform array remapping optimization for this function
      traverse_wn_tree_for_array_remapping(WN_func_body(tree));
  }

  // parentize after
  WN_Parentize(tree, PU_Parent_Map, PU_Map_Tab);
}

// This function checks if it is legal to perform the complete structure
// relayout optimization.
void IPO_WN_Update_For_Complete_Structure_Relayout_Legality(IPA_NODE *node)
{
  if (IPA_Enable_Struct_Opt == 0 ||
      complete_struct_relayout_type_id == 0 ||
      continue_with_complete_struct_relayout == 0)
    return; // nothing to do

  WN *tree = node->Whirl_Tree();
  if (global_ptrs_for_complete_struct_relayout_created == 0)
    create_global_ptrs_for_complete_struct_relayout(); // do this once only
  traverse_wn_tree_for_complete_struct_relayout_legality(NULL, NULL, tree,
    WN_func_body(tree), 0);
  // continue_with_complete_struct_relayout is set to 0 if anything is wrong
}

// This function checks if it is legal to perform the array remapping
// optimization.
void IPO_WN_Update_For_Array_Remapping_Legality(IPA_NODE *node, int pass,
  int *argument_num)
{
  if (IPA_Enable_Struct_Opt == 0 || continue_with_array_remapping == 0)
    return; // nothing to do

  WN *tree = node->Whirl_Tree();
  if (pass == 1)
{
    traverse_wn_tree_for_array_remapping_legality_1(WN_func_body(tree),
      argument_num, tree);
}
  else
    traverse_wn_tree_for_array_remapping_legality_2(WN_func_body(tree), tree,
      node, NULL);
  // continue_with_array_remapping is set to 0 if anything is wrong
}

void IPO_Identify_Single_Define_To_HeapAlloced_GlobalVar(WN *wn)
{
  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN *wn2 = WN_first(wn); wn2 != NULL; wn2 = WN_next(wn2))
      IPO_Identify_Single_Define_To_HeapAlloced_GlobalVar(wn2);
    return;
  }
  else if (WN_operator(wn) == OPR_CALL) 
  {
    const ST *const st = WN_st(wn);
    if (!strcmp(ST_name(WN_st(wn)), "calloc") &&
        WN_operator(WN_kid0(WN_kid0(wn))) == OPR_LDID) 
    {
      ST_IDX orig_size = WN_st_idx(WN_kid0(WN_kid0(wn)));
      WN *nextStmt = WN_next(wn);
      WN *nextNextStmt = WN_next(nextStmt);
      WN *rhs = WN_kid0(nextNextStmt);
      ST_IDX g_st_idx;
      if (WN_operator(nextStmt) == OPR_STID &&
          WN_operator(nextNextStmt) == OPR_STID &&
          WN_operator(rhs) == OPR_LDID &&
          ST_sclass(WN_st(rhs)) == SCLASS_REG &&
          ST_sclass(WN_st(nextStmt)) == SCLASS_REG &&
          WN_offset(rhs) == WN_offset(nextStmt)) 
      {
        ST_IDX g_st_idx = WN_st_idx(nextNextStmt);
        if (ST_IDX_level(g_st_idx) == GLOBAL_SYMTAB) 
        {
          if (singleDefHeapAllocedGlbls.find(g_st_idx) == 
              singleDefHeapAllocedGlbls.end())
          {

	    ST *new_st = New_ST(GLOBAL_SYMTAB);
	    ST_Init(new_st, Save_Str2("__temp_sac_size_", ST_name(g_st_idx)),
		    CLASS_VAR, SCLASS_COMMON, EXPORT_INTERNAL,
		    WN_ty(WN_kid0(WN_kid0(wn))));
            
            WN *ldid = WN_CreateLdid(OPR_LDID, WN_rtype(WN_kid0(WN_kid0(wn))), 
                                     WN_desc(WN_kid0(WN_kid0(wn))), 0,
                                     orig_size, WN_ty(WN_kid0(WN_kid0(wn))), 0);
            WN *stid = WN_CreateStid(OPR_STID, MTYPE_V, WN_rtype(ldid), 0, 
                                     new_st, WN_ty(ldid), ldid, 0);
            LWN_Set_Parent(ldid, stid);
            LWN_Insert_Block_Before(LWN_Get_Parent(wn), wn, stid);
            LWN_Set_Parent(stid, LWN_Get_Parent(wn));
            singleDefHeapAllocedGlbls[g_st_idx] = new_st;
          }
          else if(singleDefHeapAllocedGlbls[g_st_idx])
	  {
	    // Found duplicate, not a single def heap alloc st
	    ST * st = singleDefHeapAllocedGlbls[g_st_idx]; 
	    Set_ST_name(st, Save_Str2("_no_single_", ST_name(st)));
	    singleDefHeapAllocedGlbls[g_st_idx] = NULL;
          }
        }
      }
    }
  }
}

