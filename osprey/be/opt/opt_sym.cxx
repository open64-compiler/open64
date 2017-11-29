/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

//-*-c++-*- 

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_sym.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_sym.cxx,v $
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
// Implementation:
//
// OPT_STAB::Create builds the optimizer's symbol table.  The main
// steps involved are:
// 1.  OPT_STAB::Collect_addr_passed_for_PU sets the flag
//     BE_ST_addr_passed for appropriate ST_IDX within the PU.
// 2.  Count_syms counts the number of symbols in the PU with distinct
//     ST base entries, and initializes the mapping st_chain_map from
//     st_idx to aux_stab indices (even though aux_stab will not be
//     allocated and initialized until after Count_syms returns).
// 3.  Allocate memory for the array OPT_STAB::aux_stab.
// 4.  OPT_STAB::Convert_ST_to_AUX determines the values of all the
//     OPT_STAB_ENTRYs in aux_stab.
// 5.  OPT_STAB::Collect_nested_ref_info
// 6.  OPT_STAB::Make_st_group links up and orders the static alias
//     groups.
// 7.  OPT_STAB::Canonicalize identifies synonyms within aux_stab, and
//     convert STs into their lowest numbered synonyms
// 8.  Create the return vsym
// 9.  OPT_STAB::Collect_ST_attr collects ST attributes for FFA & FSA
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include <stdint.h>
#include "limits.h"

#define USE_STANDARD_TYPES
#include "defs.h"

#include "stab.h"
#include "irbdata.h"
#include "config_targ.h"
#include "config_opt.h"         // for Delay_U64_Lowering
#include "targ_sim.h"
#include "tracing.h"
#include "stblock.h"
#include "config.h" 		// LANG_Recursive
#include "wn_util.h"
#include "wn_simp.h"
#include "mempool.h"
#include "be_symtab.h"
#include "wn_lower.h"
#include "opt_config.h"
#include "erglob.h"
#include "opt_htable.h"
#include "opt_util.h"
#include "opt_base.h"
#include "opt_sym.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_wn.h"
#include "opt_mu_chi.h"
#include "opt_main.h"
#include "opt_fold.h"
#include "opt_alias_class.h"
#ifdef KEY
#include "opt_alias_rule.h"
#endif
#include "opt_points_to.h"
#include "opt_cvtl_rule.h"
#include "opt_alias_mgr.h"

#include <algorithm>

// Used by the Symbol table dump.  Otherwise, we should not
// include these headers.
//
extern "C" {
#include "targ_const.h"
#include "stdlib.h"
char *Targ_Print( const char *fmt, TCON cvalue );
}

#ifdef KEY
static BOOL in_parallel_region = FALSE;
#endif

// ====================================================================
//
// OPT_STAB Constructor and Destructor
//
// ====================================================================


OPT_STAB::OPT_STAB(MEM_POOL *pool) : aux_stab(pool), 
  _ac_2_vsym_map(256, (IDTYPE)0, pool, FALSE),
  _pt_sum(pool)
{
  mem_pool = pool;
  // _aux_pool = CXX_NEW(MEM_POOL, mem_pool);
  // _ver_pool = CXX_NEW(MEM_POOL, mem_pool);
  // _occ_pool = CXX_NEW(MEM_POOL, mem_pool);
  // OPT_POOL_Initialize(&_aux_pool, "AUX Pool", FALSE, MEM_DUMP_FLAG+7);
  OPT_POOL_Initialize(&_ver_pool, "VER Pool", FALSE, MEM_DUMP_FLAG+8);
  OPT_POOL_Initialize(&_occ_pool, "OCC Pool", FALSE, MEM_DUMP_FLAG+9);
  OPT_POOL_Initialize(&_st_chain_pool, "ST chain mapping", FALSE, MEM_DUMP_FLAG+12);

  /* Push each pool to prepare for allocation. */
  // OPT_POOL_Push(&_aux_pool, MEM_DUMP_FLAG+7);
  OPT_POOL_Push(&_ver_pool, MEM_DUMP_FLAG+8);
  OPT_POOL_Push(&_occ_pool, MEM_DUMP_FLAG+9);
  OPT_POOL_Push(&_st_chain_pool, MEM_DUMP_FLAG+12);

  _default_vsym=0;
  Set_WN_sym_map(WN_MAP_Create(mem_pool));
  Set_WN_box_refs(WN_MAP_Create(mem_pool));
  Set_WN_box_defs(WN_MAP_Create(mem_pool));

  // tlog counter
  _const_found = 0;

  // region boundary trace flag
  _rgn_trace = Get_Trace(TP_REGION, TT_REGION_WOPT_DEBUG) ||
	       Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG) ||
	       Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG);

  // create a points_to structure for globals
  Set_points_to_globals( CXX_NEW( POINTS_TO, mem_pool )  );
  Points_to_globals()->Init();
  Points_to_globals()->Set_global();
  Points_to_globals()->Set_expr_kind(EXPR_IS_ADDR);
  Points_to_globals()->Set_base_kind(BASE_IS_UNKNOWN);
  Points_to_globals()->Set_ofst_kind(OFST_IS_UNKNOWN);

  // The following are initialized in Create:
  _has_exc_handler = FALSE;
#ifdef KEY
  _has_nonlocal_goto_target = FALSE;
#endif

  _ac_2_vsym_map.Init ();
  _pt_sum.Set_opt_stab (this);  

  BOOL t = Get_Trace (TP_GLOBOPT, CR_DUMP_FLAG);
  _cr_sr_annot_mgr = CXX_NEW (MEMOP_ANNOT_CR_SR_MGR (mem_pool, t), mem_pool);
  _cr_sr_annot_mgr->Set_active_mgr ();
}


OPT_STAB::~OPT_STAB(void)
{
  WN_MAP_Delete(WN_sym_map());
  WN_MAP_Delete(WN_box_refs());
  WN_MAP_Delete(WN_box_defs());

  CXX_DELETE (_cr_sr_annot_mgr, _cr_sr_annot_mgr->Mem_pool());

  // Is_True(_ver_pool == NULL, ("ver pool is not deleted."));

  Opt_tlog( "SYM", 0, "PU static const %d", Const_found());

  OPT_POOL_Pop(&_occ_pool, MEM_DUMP_FLAG+9);
  // OPT_POOL_Pop(&_aux_pool, MEM_DUMP_FLAG+7);
  OPT_POOL_Pop(&_st_chain_pool, MEM_DUMP_FLAG+12);

  OPT_POOL_Delete(&_occ_pool, MEM_DUMP_FLAG+9);
  // OPT_POOL_Delete(&_aux_pool, MEM_DUMP_FLAG+7);
  OPT_POOL_Delete(&_st_chain_pool, MEM_DUMP_FLAG+12);
}


// ====================================================================
// ====================================================================
//
// CREATION OF OPT_STAB
//
// After OPT_STAB constructor has finished, Pre_Optimizer (in
// opt_main.cxx) invokes OPT_STAB::Create, which invokes the procedures
// below to construct aux_stab.
//
// ====================================================================
// ====================================================================


// ====================================================================
//
// Collect_addr_passed_for_PU is invoked by OPT_STAB::Create to set the
// flag BE_ST_addr_passed for appropriate ST_IDX within the PU.
//
// Collect_addr_passed, Init_addr_passed_for_io, and
// Collect_addr_passed_for_io are helper procedures invoked only by
// Collect_addr_passed_for_PU.
//
// ====================================================================


static void
Collect_addr_passed(WN *wn)
{
  OPERATOR opr = WN_operator(wn);

  // Note that we need to deal with calls, and PARM nodes since they may
  // occur under IO_ITEM nodes.
  //
  if (OPERATOR_is_call(opr)
#ifdef KEY
      || opr == OPR_PURE_CALL_OP
#endif
      )
  {
    for (INT i = 0; i < WN_kid_count(wn); i++)
      Collect_addr_passed(WN_kid(wn,i));
  }
  else if (opr == OPR_PARM)
  {
    Collect_addr_passed(WN_kid0(wn));
  }
  else if (OPERATOR_is_expression(opr))
  {
    // IO_ITEMs may have labels and gotos under them, and who knows what
    // else.  Hence the guard on this code to exclude such non-expression
    // nodes.
    //
    if (OPERATOR_is_load(opr))
      return;

    if (opr == OPR_LDA) {
      Set_BE_ST_addr_passed(WN_st_idx(wn));
      if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) {
	fprintf(TFile, "set addr used locally: ");
	Print_ST(TFile, WN_st(wn), TRUE);
      }
    }
    for (INT i = 0; i < WN_kid_count(wn); i++) 
      Collect_addr_passed(WN_kid(wn,i));
  }
} // Collect_addr_passed


static void
Init_addr_passed_for_io(BOOL *collect_for_stmt, BOOL *collect_for_item)
{
  INT i;

  // TODO: Finetune. Only set addr_passed for "read" stmts an appropriate 
  // ioitems.
  //
  // For now, set it conservatively for any iostmt and any ioitem.
  //
  for (i = 0; i <= IOSTATEMENT_LAST; i++) collect_for_stmt[i] = TRUE;
  for (i = 0; i <= IOITEM_LAST; i++) collect_for_item[i] = TRUE;
} // Init_addr_passed_for_io


static void
Collect_addr_passed_for_io(WN *wn)
{
  // TODO: only set the addr_passed flag for IO_ITEMS that may truly
  // cause the addresses passed to be modified.
  //
  static BOOL collect_for_stmt[IOSTATEMENT_LAST+1];
  static BOOL collect_for_item[IOITEM_LAST+1];
  static BOOL initialized = FALSE;
   
  if (!initialized)
  {
    initialized = TRUE;
    Init_addr_passed_for_io(collect_for_stmt, collect_for_item);
  }

  Is_True(WN_operator(wn) == OPR_IO,
	  ("Unexpected opcode in Collect_addr_passed_for_io()"));
   
  if (collect_for_stmt[WN_io_statement(wn)])
  {
    for (INT io_kidno = 0; 
	 io_kidno < WN_kid_count(wn); 
	 io_kidno++)
    {
      WN * const io_item = WN_kid(wn, io_kidno);
      
      Is_True(WN_operator(io_item) == OPR_IO_ITEM,
	      ("Unexpected kid of IO stmt in "
	       "Collect_addr_passed_for_io()"));
      
      if (collect_for_item[WN_io_item(io_item)])
      {
	for (INT item_kidno = 0;
	     item_kidno < WN_kid_count(io_item);
	     item_kidno++)
	{
	  // Use same algorithm used for parameter passing in function calls.
	  //
	  Collect_addr_passed(WN_kid(io_item, item_kidno));
	} // for each kid of item
      }
    } // for each item
  }
} // Collect_addr_passed_for_io


static void Collect_addr_passed_for_PU(WN *wn)
{
  if (wn == NULL)
    return;

  // Fix 625656:
  //  Catch the cases not covered by addr_saved in be/com/opt_addr_flags.cxx.
  if (WN_operator(wn) == OPR_PARM && (
      WN_Parm_Dereference(wn) ||
      WN_Parm_By_Reference(wn) || WN_Parm_Passed_Not_Saved(wn))) 
    Collect_addr_passed(WN_kid0(wn));
  //
  // Fix 642145:
  //
  else if (WN_operator(wn) == OPR_IO)
    Collect_addr_passed_for_io(wn);

  if (WN_operator(wn) == OPR_BLOCK) 
    for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  
      Collect_addr_passed_for_PU(stmt);
  else if ( !OPERATOR_is_black_box( WN_operator(wn) ) ) {
    for (INT32 i = 0; i < WN_kid_count(wn); i++) 
      Collect_addr_passed_for_PU(WN_kid(wn,i));
  }
}


// ====================================================================
//
// Count_syms counts the number of symbols in the PU with distinct ST
// base entries, and initializes the mapping st_chain_map from st_idx
// to aux_stab indices (even though aux_stab will not be allocated and
// initialized until after Count_syms returns).
//
// aux_sym_cnt is initialized to zero by Create, then incremented as
// Count_syms walks through the tree.  When Count_syms returns to
// Create, aux_sym_cnt equals the number of symbols in the PU with
// distinct ST entries, and is used to estimate the size of aux_stab.
//
// Also, Count_syms determines appropriate values for the flags
// BE_ST_addr_used_locally and _has_exc_handler.
//
// ====================================================================


static INT32 aux_sym_cnt;


void
OPT_STAB::Count_syms(WN *wn)
{
  INT32 i;
  WN *stmt;

  if (wn == NULL)
    return;

  if (OPERATOR_has_aux(WN_operator(wn)) &&
      WN_st(wn) != NULL) {
    ST *st = WN_st(wn);
    ST_CHAIN_INFO *st_chain_info = st_chain_map->Lookup(WN_st_idx(wn));
    if (st_chain_info == NULL) {

      // first time seeing this symbol or this type.
      aux_sym_cnt++;
      st_chain_info = CXX_NEW(ST_CHAIN_INFO, &_st_chain_pool);
      st_chain_info->Set_list_head(aux_sym_cnt);
      st_chain_map->Insert(WN_st_idx(wn), st_chain_info);

      // If this st is initialized with the address of some other
      // variable, make sure we assign an aux ID to that variable
      // even if it isn't directly mentioned in the code so we can
      // constant propagate later.
      //
      // Don't we need to set addr_used_locally for the new aux ID?
      // Where does it get set? Answer: With the old symbol table,
      // it must have been set by the front end. Since there was no
      // LDID or STID for this symbol in the PU, we won't reset the
      // bit below, so when we come to OPT_STAB::Update_attr_cache,
      // we won't have messed it up. For the new symbol table, we
      // need to make sure we set the corresponding indication that
      // the new aux ID's address is used locally.
      BOOL done = FALSE;
      while (!done) {
	done = TRUE;
	BOOL const_initialized = ST_is_const_initialized(st);
	if (const_initialized || (ST_is_initialized(st) &&
	    ST_sclass(st) == SCLASS_PSTATIC)) {
	  INITV_IDX initv = ST_has_initv(st);
	  if (initv &&
	      INITV_kind(Initv_Table[initv]) == INITVKIND_SYMOFF) {
	    st = &St_Table[INITV_st(Initv_Table[initv])];
	    if (const_initialized || ST_class(st) == CLASS_VAR) {
	      st_chain_info = st_chain_map->Lookup(ST_st_idx(st));
	      if (st_chain_info == NULL) {
		aux_sym_cnt++;
		st_chain_info = CXX_NEW(ST_CHAIN_INFO, &_st_chain_pool);
		st_chain_info->Set_list_head(aux_sym_cnt);
		st_chain_map->Insert(ST_st_idx(st), st_chain_info);
		done = FALSE;   // keep on adding
	      }
	    }
	  }
	}
      }
    }
  }

  // We recompute addr_used_locally for some reason?
  if (OPERATOR_is_scalar_load (WN_operator(wn)) ||
      OPERATOR_is_scalar_store (WN_operator(wn))) {
    ST *st = WN_st(wn);
    if (ST_class(st) == CLASS_VAR) 
      Clear_BE_ST_addr_used_locally(st);
  }

  if (WN_operator(wn) == OPR_REGION && REGION_is_EH(wn))
    _has_exc_handler = TRUE;
#ifdef KEY
  if (PU_has_nonlocal_goto_label(Get_Current_PU()))
    _has_nonlocal_goto_target = TRUE;
#endif

  // any regions in the whirl are black boxes, ignore
  if (WN_operator(wn) == OPR_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL,("OPT_STAB::Count_syms, NULL rid"));
    if (RID_level(rid) >= Rgn_level()) 
      return;
  }

  // Count in pointers which have points-to records in the points-to- 
  // summary of this callee.
  // 
  if (WN_operator(wn) == OPR_CALL && WOPT_Enable_Pt_Summary) {
    PU_POINTS_TO_SUMMARY* sum = _pt_sum.Get_bound_pt_sum (wn);
    if (sum) {
      UNAME_VECTOR& ptrs = sum->Out_set().Name_space()->All_names();  
      for (UNAME_VECTOR_ITER iter = ptrs.begin (); 
           iter != ptrs.end (); iter++) {
        UNIFORM_NAME* name = *iter;  
        if (name->Type () == UN_NAMED_GLOBAL) {
	  ST* st = name->ST_for_named_global ();
          ST_CHAIN_INFO *st_chain_info = st_chain_map->Lookup(ST_index(st));
          if (st_chain_info == NULL) {
            aux_sym_cnt++;
            st_chain_info = CXX_NEW(ST_CHAIN_INFO, &_st_chain_pool);
            st_chain_info->Set_list_head(aux_sym_cnt);
            st_chain_map->Insert(ST_st_idx(st), st_chain_info);
	  }
	}
      } /* end of for-loop */
    }
  }

  // For compatibility with old BARRIER nodes.
  if (WN_operator(wn) == OPR_FORWARD_BARRIER ||
      WN_operator(wn) == OPR_BACKWARD_BARRIER) {
    
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      if (WN_operator(WN_kid(wn,i)) == OPR_IDNAME) {
	DevWarn("old style BARRIER: converting IDNAME kid of BARRIER"
		" into IDNAME.");
	OPCODE op = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
	ST *st = WN_st(WN_kid(wn,i));
	WN *lda = WN_CreateLda(op, 0, ST_type(st), st);
	WN_kid(wn,i) = lda;
      }
    }
  }

  if (WN_operator(wn) == OPR_BLOCK) 
    for (stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  
      Count_syms(stmt);
  else if ( !OPERATOR_is_black_box( WN_operator(wn) ) ) {
    for (i = 0; i < WN_kid_count(wn); i++) 
      Count_syms(WN_kid(wn,i));
  }
}


// ====================================================================
//
//  St_name returns the name string of the ST entry
//
// ====================================================================


const char *
AUX_STAB_ENTRY::St_name(void)
{
  if (st) { 
    if (ST_class(st) == CLASS_CONST)
      return Targ_Print(NULL, STC_val(st));
    else
      return ST_name(st);
  } else {
    return "null";
  }
}


// ====================================================================
//
// Check_volatility is invoked by OPT_STAB::Convert_ST_to_AUX to check
// for the "volatile" attribute and fix up the WN's type if necessary.
// Returns TRUE iff the reference is volatile.
//
// ====================================================================


inline BOOL
Check_volatility( WN *wn, ST *st )
{
  Is_True(WN_ty(wn) != 0, ("WN has null TY."));

  // is this type a volatile
  if ( TY_is_volatile(WN_ty(wn)) )
    return TRUE;
  
  if (ST_class(st) != CLASS_VAR)
    return FALSE;

  if ( TY_is_volatile(ST_type(st)) ) {
    // if the variable is volatile, but the type isn't, go ahead and
    // change the type to be volatile so all accesses are consistent
    if ( ! TY_is_volatile(WN_ty(wn)) ) {
      // The following code makes clear that we still don't hide our
      // implementation very well. If the volatile bit were on the TY
      // instead of on the TY_IDX, things would need to be different
      // here.
      TY_IDX vty = WN_ty(wn);
      Set_TY_is_volatile(vty);
      WN_set_ty(wn, vty);
    }
    return TRUE;
  }

  // must not be volatile
  return FALSE;
}


// ====================================================================
//
// Enter_symbol enters the symbol defined by <st, ofst, sizeof(dtype)>
// into the aux_stab.
//
// Enter_preg enters the dedicated preg defined by <st, ofst> into the
// aux_stab.
//
// Both Enter_symbol and Enter_ded_preg are invoked by
// OPT_STAB::Convert_ST_to_AUX.  The routines are almost identical
// except that the desired information is passed to Enter_preg as
// parameters, but obtained by Enter_symbol from the WN node.
//
// ====================================================================


static UINT64 Desc_type_byte_size(const WN* wn) {
  Is_True(WN_operator(wn) == OPR_LDID || WN_operator(wn) == OPR_STID,
          ("Desc_type_byte_size: operator not LDID or STID"));
  Is_True(WN_desc(wn) == MTYPE_M, ("Desc_type_byte_size: desc not MTYPE_M"));
  TY_IDX ty_idx = WN_ty(wn);
  UINT field_id = WN_field_id(wn);
  if (field_id != 0) {
    Is_True(TY_kind(ty_idx) == KIND_STRUCT,
            ("Desc_type_size_min: expecting KIND_STRUCT"));
    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field(ty_idx, field_id, cur_field_id);
    Is_True(! fld.Is_Null(),
            ("Desc_type_byte_size: Invalid field id %d for type 0x%x",
	     field_id, ty_idx));
    ty_idx = FLD_type(fld);
  }
  return TY_size(ty_idx);
}

#ifdef KEY
// Input wn is an LDA, if it has correct field_id information, then
// return the size of the field, else return 0. Set second argument
// appropriately.
static UINT64 Field_type_byte_size(const WN* wn, BOOL& fld_correct)
{
  fld_correct = TRUE;
  Is_True (WN_operator(wn) == OPR_LDA,
           ("Field_type_byte_size: operator not LDA"));
  TY_IDX ty_idx = WN_ty(wn);
  Is_True (TY_kind(ty_idx) == KIND_POINTER,
           ("Field_type_byte_size: expected pointer type"));
  UINT field_id = WN_field_id(wn);
  Is_True (field_id, ("Field_type_byte_size: Expected non-zero field-id"));
  TY_IDX pointee = TY_pointed(ty_idx);
  while (TY_kind(pointee) == KIND_ARRAY)
    pointee = TY_etype(pointee);
  Is_True (TY_kind(pointee) == KIND_STRUCT,
           ("Field_type_byte_size: Field-id can only be in a struct"));
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field(pointee, field_id, cur_field_id);
  // If there is invalid field_id information, we cannot do anything here.
  if (fld.Is_Null() || (FLD_ofst(fld) != WN_offset(wn)))
  {
    fld_correct = FALSE;
    return 0;
  }
  TY_IDX fld_ty_idx = FLD_type(fld);
  return TY_size(fld_ty_idx);
}

// Return the access size. If wn accesses a field in a struct, return
// the correct field_id in 'field_id'.
static INT32 Get_byte_size (ST * st, WN * wn, UINT& field_id)
{
  INT32 byte_size = 0;

  if (ST_class(st) == CLASS_VAR)
  {
    if (wn && WN_field_id(wn))
    {
      BOOL fld_correct;
      INT32 size = Field_type_byte_size (wn, fld_correct);
      if (fld_correct)
      {
        byte_size = size;
        field_id = WN_field_id(wn);
      }
      else byte_size = TY_size(ST_type(st));
    }
    else byte_size = TY_size(ST_type(st));
  }
  return byte_size;
}
#endif

// KEY: Bugs 9989, 10139: Added field-id information for LDID and STID
// even for non-bit-fields. But for non-bit-fields, we do not need to
// match field-id while searching for existing symbol idx.
AUX_ID
OPT_STAB::Enter_symbol(OPERATOR opr, ST* st, INT64 ofst,
		       TY_IDX wn_object_ty, BOOL is_volatile, WN* wn)
{
  Is_True(st, ("Enter_symbol:  can't enter NULL symbol."));

  INT32 stype, byte_size;
  UINT8 bit_ofst = 0;
  UINT8 bit_size = 0;
  INT32 mclass = 0;
  MTYPE mtype = MTYPE_UNKNOWN;
  BOOL is_virtual = FALSE;
  BOOL is_scalar = FALSE;
  BOOL no_register = FALSE;
  BOOL dmod = FALSE;
  UINT field_id = 0;
#ifdef KEY
  BOOL is_bit_field = FALSE;
#endif
  TY_IDX hl_ty = (TY_IDX)0;

  switch (opr) {
  case OPR_LDA:
    is_virtual = TRUE;
#ifdef KEY
    byte_size = Get_byte_size (st, wn, field_id);
#else
    byte_size = (ST_class(st) == CLASS_VAR) ? TY_size(ST_type(st)) : 0;
#endif
    stype = VT_LDA_SCALAR;
    break;
  case OPR_LDBITS:
    bit_size = WN_bit_size(wn);
    bit_ofst = WN_bit_offset(wn);
    // fall through
  case OPR_LDID:
    if (WN_desc(wn) == MTYPE_BS) {
      UINT cur_field_id = 0;
      UINT64 field_offset = 0;
      FLD_HANDLE fld = FLD_And_Offset_From_Field_Id(WN_ty(wn), WN_field_id(wn),
						    cur_field_id, field_offset);
      ofst += field_offset;
      bit_size = FLD_bsize(fld);
      bit_ofst = FLD_bofst(fld);
      wn_object_ty = FLD_type(fld);
      byte_size = TY_size(wn_object_ty);
#ifdef KEY
      is_bit_field = TRUE;
#else
      field_id = WN_field_id(wn);
#endif
    }
    else if (WN_desc(wn) == MTYPE_M) {
      byte_size = Desc_type_byte_size(wn);
      if (opr != OPR_LDBITS) {
        no_register = TRUE;
      }
    } else {
      byte_size = MTYPE_size_min(WN_desc(wn)) >> 3;
      if (ST_sclass(st) == SCLASS_REG)
        byte_size = TY_size(ST_type(st));
    }
    mtype = WN_rtype(wn);
    mclass = Get_mtype_class(mtype);
    is_scalar = TRUE;
    stype = VT_NO_LDA_SCALAR;
#ifdef KEY
    if (opr == OPR_LDID)
      field_id = WN_field_id(wn);
#endif
    if (WN_field_id(wn) != (TY_IDX)0) {
      UINT32 dummy;
      WN_hl_object_ty(wn, hl_ty, dummy);
    }
    break;
  case OPR_STBITS:
    bit_size = WN_bit_size(wn);
    bit_ofst = WN_bit_offset(wn);
    // fall through
  case OPR_STID:
    if (WN_desc(wn) == MTYPE_BS) {
      UINT cur_field_id = 0;
      UINT64 field_offset = 0;
      FLD_HANDLE fld = FLD_And_Offset_From_Field_Id(WN_ty(wn), WN_field_id(wn),
						    cur_field_id, field_offset);
      ofst += field_offset;
      bit_size = FLD_bsize(fld);
      bit_ofst = FLD_bofst(fld);
      wn_object_ty = FLD_type(fld);
      byte_size = TY_size(wn_object_ty);
#ifdef KEY
      is_bit_field = TRUE;
#else
      field_id = WN_field_id(wn);
#endif
    }
    else {
      if (WN_desc(wn) == MTYPE_M) {
        byte_size = Desc_type_byte_size(wn); 
        if (opr != OPR_STBITS) {
          no_register = TRUE;
        }
      } else
	byte_size = MTYPE_size_min(WN_desc(wn)) >> 3;
      if (ST_sclass(st) == SCLASS_REG)
        byte_size = TY_size(ST_type(st));
    }
    mtype = WN_desc(wn);
    mclass = Get_mtype_class(mtype);
    dmod = TRUE;
    is_scalar = TRUE;
    stype = VT_NO_LDA_SCALAR;
#ifdef KEY
    if (opr == OPR_STID)
      field_id = WN_field_id(wn);
#endif
    if (WN_field_id(wn) != (TY_IDX)0) {
      UINT32 dummy;
      WN_hl_object_ty(wn, hl_ty, dummy);
    }
    break;
  default:
    stype = VT_OTHER;
    byte_size = 0;
  }

  // Get the set of aux ID's based on the ST of the given symbol and
  // look through the set for one whose kind matches the kind of the
  // symbol we're entering. If we don't find a match according to the
  // kind, make a new aux ID corresponding to this ST and add the new
  // aux ID to the set of this ST's aux ID's.

  // Get the tentative aux_id of the given symbol
  ST_CHAIN_INFO *st_chain_info = st_chain_map->Lookup(ST_st_idx(st));
  Is_True(st_chain_info != NULL,
	  ("OPT_STAB::Enter_symbol: st chain must be defined.\n"));
  AUX_ID idx = st_chain_info->List_head();
  Is_True(idx != 0, ("OPT_STAB::Enter_symbol: idx is 0."));
  
  // Lookup the opt_stab first
  while (idx && aux_stab[idx].St() != NULL) {
    BOOL kind_match = FALSE;
    switch (aux_stab[idx].Stype()) {
    case VT_NO_LDA_SCALAR:
    case VT_LDA_SCALAR:
      kind_match = ((is_scalar && aux_stab[idx].Mclass() == mclass)
		    || is_virtual);
      break;
    case VT_LDA_VSYM:
    case VT_UNIQUE_VSYM:
    case VT_SPECIAL_VSYM:
      kind_match = is_scalar || is_virtual;
      break;
    case VT_OTHER:
      // How do we know stype doesn't contain garbage here?
      kind_match = (stype == VT_OTHER);
      break;
    default:
      break;
    }

    if (kind_match &&
	aux_stab[idx].Byte_size() == byte_size &&
#ifdef KEY // bug 12321
	aux_stab[idx].St() == st &&
#endif
	aux_stab[idx].St_ofst() == ofst &&
	aux_stab[idx].Bit_size() == bit_size &&
	aux_stab[idx].Bit_ofst() == bit_ofst &&
#ifdef KEY
	(!is_bit_field || (aux_stab[idx].Field_id() == field_id))
#else
	aux_stab[idx].Field_id() == field_id
#endif
       ) {
      
      if ( is_volatile && ! aux_stab[idx].Is_volatile() ) {
	aux_stab[idx].Set_volatile();
      }
#ifdef KEY // bug 5401 and 5267
      else if (in_parallel_region && OPERATOR_is_scalar_store(opr) &&
	       ST_sclass(st) != SCLASS_REG)
	aux_stab[idx].Set_mp_no_dse();
#endif
      // found -- update symtab entry
      if (is_scalar) {
	if (!aux_stab[idx].Is_real_var())
	  aux_stab[idx].Set_stype(VT_LDA_SCALAR);
	aux_stab[idx].Set_mclass(mclass);
	aux_stab[idx].Set_mtype(mtype);
      }
      if (is_virtual && !aux_stab[idx].Is_virtual())
	aux_stab[idx].Set_stype(VT_LDA_SCALAR);

      if (no_register) {
        aux_stab[idx].Set_no_register();
      }

      if (dmod) aux_stab[idx].Set_dmod();
      return idx;    
    }
    idx = aux_stab[idx].St_chain();
  }

  // not found -- create new entry
  // It appears impossible that the aux_stab[idx].St() != NULL
  // evaluates to TRUE in the following 'if' statement, given the
  // while loop condition above. Either the expression isn't evaluated
  // because idx == 0, or it evaluates to FALSE as guaranteed by the
  // while loop. There is no break out of the loop.
  Is_True(idx != 0 || aux_stab[idx].St() == NULL,
	  ("OPT_STAB::Enter_symbol: Robert misunderstood the logic"));
  if (idx == 0 || aux_stab[idx].St() != NULL) {
    // Make a new entry at the head of the list
    idx = aux_stab.Newidx();
    aux_stab[idx].Set_st_chain(st_chain_info->List_head());
  } else {
    // We encountered a NULL St() field for this aux ID. This happens
    // (only) when we encounter this chain for the first time.
    aux_stab[idx].Set_st_chain((AUX_ID) 0);
  }

  // create a new entry.
  st_chain_info->Set_list_head(idx);

  AUX_STAB_ENTRY *sym = Aux_stab_entry(idx);

  sym->Set_stype(stype);
  sym->Clear_flags();
  sym->Set_st(st);
  sym->Set_st_ofst(ofst);
  sym->Set_nonzerophis(NULL);
  sym->Set_aux_id_list(NULL);
  sym->Set_st_group((AUX_ID) 0);
  sym->Set_synonym((AUX_ID) 0);
  sym->Set_home_sym((AUX_ID) 0);
  sym->Set_zero_cr(NULL);
  sym->Set_field_id(field_id);
  sym->Set_mclass(0);
  sym->Set_mtype(MTYPE_UNKNOWN);
  sym->Set_def_bbs(NULL);  

#ifdef KEY 
  // bug 13670: don't leave the field uninitialized
  sym->Set_value_size(0);
  sym->Set_spre_node(NULL);
  // bug 3091: to help create correct identity assignment for BS var
  if (opr == OPR_LDID || opr == OPR_STID)
    sym->Set_wn(wn);
#endif

  if (is_scalar) {
    sym->Set_stype(VT_NO_LDA_SCALAR);
    sym->Set_mclass(mclass);
    sym->Set_mtype(mtype);
  }
  if (is_virtual) {
    sym->Set_stype(VT_LDA_VSYM);
    sym->Set_mclass(0);
    sym->Set_mtype(MTYPE_UNKNOWN);
  }

  if (no_register) {
    sym->Set_no_register();
  }

  if (dmod) sym->Set_dmod();
  if ( is_volatile )   sym->Set_volatile();

#ifdef KEY // bug 5401 and 5267
  if (in_parallel_region && OPERATOR_is_scalar_store(opr) && 
      ST_sclass(st) != SCLASS_REG)
    sym->Set_mp_no_dse();
#endif

  if (ST_class(st) == CLASS_VAR && ST_has_nested_ref(st)) {
    sym->Set_has_nested_ref();
  }

  sym->Points_to()->Init();
  sym->Set_byte_size(byte_size);
  sym->Set_bit_ofst_size(bit_ofst, bit_size);
  sym->Set_ty(wn_object_ty);
  sym->Points_to()->Set_hl_ty(hl_ty);

  INT64 tmpofst;
  ST    *tmpbase;
  Expand_ST_into_base_and_ofst(st, ofst, &tmpbase, &tmpofst);
  sym->Set_base(tmpbase);
  sym->Set_base_byte_ofst(tmpofst);
#ifdef KEY
  sym->Set_base_kind(BASE_IS_FIXED);
#endif

  if (WOPT_Set_Unique_Pt != NULL && 
      ST_name(st) != NULL &&
      strncmp(WOPT_Set_Unique_Pt, ST_name(st), strlen(WOPT_Set_Unique_Pt))
      == 0) {
    // Note that there's no way to set this for only a variable 'x' in
    // a program containing other variables whose names begin with
    // x. You'll catch all those others, too.
    Set_ST_pt_to_unique_mem(st);
  }

  // see if it's a constant scalar variable that's been initialized
  // that we can constant propagate
  BOOL const_initialized = ST_is_const_initialized(st);
  if (const_initialized || (ST_is_initialized(st) && 
      ST_sclass(st) == SCLASS_PSTATIC)) {
    if (ST_is_const_initialized(st))
      sym->Set_const_init();
    INITV_IDX initv;
    if ((initv = ST_has_initv(st)) &&
	INITV_kind(Initv_Table[initv]) == INITVKIND_SYMOFF) {
      ST *st = &St_Table[INITV_st(Initv_Table[initv])];
      if (const_initialized || ST_class(st) == CLASS_VAR) {
	if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) {
	  fprintf(TFile, "Enter initv: %s+%d\n", ST_name(st),
		  INITV_ofst(Initv_Table[initv]));
	}
	Enter_symbol(OPR_LDA, st, INITV_ofst(Initv_Table[initv]),
		     ST_type(st), TY_is_volatile(ST_type(st))); 
      }
    }
  }
  return idx;
}


AUX_ID
OPT_STAB::Enter_ded_preg(ST *st, INT64 ofst, TY_IDX ty, INT32 mclass)
{
  ST_CHAIN_INFO *st_chain_info = st_chain_map->Lookup(ST_st_idx(st));
  AUX_ID         idx;
  if (st_chain_info != NULL) {
    idx = st_chain_info->List_head();
  }
  else {
    idx = (AUX_ID) 0;  // 0 indicates not found.
  }
  
  while (idx && aux_stab[idx].St() != NULL) {
    Is_True(aux_stab[idx].St() == st,
	    ("Enter_ded_preg::lookup wrong ST chain."));
    if (aux_stab[idx].Stype() == VT_NO_LDA_SCALAR &&
	aux_stab[idx].Mclass() == mclass &&
	aux_stab[idx].St_ofst() == ofst) {
	  return idx;      // found
	}
    idx = aux_stab[idx].St_chain();
  }

  // not found
  if (idx == 0 || aux_stab[idx].St() != NULL) {
    idx = aux_stab.Newidx();
    // It may happen that st_chain_info is NULL here. An example is a
    // C function implicitly "declared" to return int (i.e., the user
    // specifies no return type) and lacking any return value in the
    // source code. We still will assign an aux_stab entry for the
    // return PREG even though it isn't mentioned in the code of the
    // PU.
    if (st_chain_info == NULL) {
      st_chain_info = CXX_NEW(ST_CHAIN_INFO(0), &_st_chain_pool);
      st_chain_map->Insert(ST_st_idx(st), st_chain_info);
    }
    aux_stab[idx].Set_st_chain(st_chain_info->List_head());
  }
  else {
    aux_stab[idx].Set_st_chain((AUX_ID) 0);
  }

  // create a new entry
  st_chain_info->Set_list_head(idx);

  AUX_STAB_ENTRY *sym = Aux_stab_entry(idx);

  sym->Set_stype(VT_NO_LDA_SCALAR);
  sym->Set_mclass(mclass);
  sym->Set_mtype(MTYPE_UNKNOWN);
#ifdef KEY
  // bug 13670
  sym->Set_value_size(0);
  sym->Set_spre_node(NULL);
#endif
  sym->Clear_flags();
  sym->Set_st(st);
  sym->Set_st_ofst(ofst);
  sym->Set_nonzerophis(NULL);
  // sym->Set_st_chain() -- taken care of earlier
  sym->Set_st_group((AUX_ID) 0);
  sym->Set_synonym((AUX_ID) 0);
  sym->Set_home_sym((AUX_ID) 0);
  sym->Set_zero_cr(NULL);
  sym->Points_to()->Analyze_ST(st, sym->St_ofst(), TY_size(ST_type(st)), 0, 
			       0, ty, FALSE /* no equiv */);

  return idx;
}


// ====================================================================
//
// Create_vsym creates a new vsym entry in aux_stab.
//
// Create_preg creates a new preg entry in aux_stab.
//
// Change_to_new_preg is invoked by COMP_UNIT::Do_local_rvi to convert
// a variable into a preg and truncate all its STIDs
//
// ====================================================================


AUX_ID
OPT_STAB::Create_vsym(EXPR_KIND k)
{
  AUX_ID retv = aux_stab.Newidx();
  AUX_STAB_ENTRY *vsym = Aux_stab_entry(retv);
  vsym->Set_stype(VT_SPECIAL_VSYM);
  vsym->Clear_flags();
  vsym->Set_mclass(0);
  vsym->Set_mtype(MTYPE_UNKNOWN);
#ifdef KEY
  // bug 13670
  vsym->Set_value_size(0);
  vsym->Set_spre_node(NULL);
#endif
  vsym->Set_st(NULL);
  vsym->Set_st_ofst(0);
  vsym->Set_nonzerophis(NULL);
  vsym->Set_st_chain(0);
  vsym->Set_st_group(0);   
  vsym->Set_aux_id_list(NULL);
  vsym->Set_def_bbs(NULL);
  vsym->Set_home_sym((AUX_ID) 0);
  vsym->Set_zero_cr(NULL);
  vsym->Points_to()->Init();
  vsym->Points_to()->Set_expr_kind(k);
  return retv;
}


AUX_ID
OPT_STAB::Create_preg(MTYPE preg_ty, const char *name, WN *home_wn)
{
  ST *st;
#ifdef KEY // bug 1523: preopt in ipl cannot use pregs due to exception handling
  if ((Has_exc_handler()  || Has_nonlocal_goto_target())
      && Phase() == PREOPT_IPA0_PHASE)
    st = Gen_Temp_Symbol(MTYPE_To_TY(preg_ty), name);
  else
#endif
  st = MTYPE_To_PREG(preg_ty);
  AUX_ID retv = aux_stab.Newidx();

  if (st_chain_map != NULL) {
    ST_CHAIN_INFO *st_chain_info = st_chain_map->Lookup(ST_st_idx(st));
    if (st_chain_info != NULL) {
      aux_stab[retv].Set_st_chain(st_chain_info->List_head());
    }
    else {
      aux_stab[retv].Set_st_chain((AUX_ID) 0);
    }
  }
  AUX_STAB_ENTRY *sym = Aux_stab_entry(retv);
  sym->Set_stype(VT_NO_LDA_SCALAR);
  sym->Clear_flags();
  sym->Set_mclass(Get_mtype_class(preg_ty));
  sym->Set_mtype(preg_ty);
  sym->Set_st(st);
#ifdef KEY // due to about change, st is no longer always preg
  if (ST_class(st) == CLASS_PREG)
    sym->Set_st_ofst(Alloc_preg(preg_ty,name,home_wn));
  else
// Fix bug 1748
    sym->Set_st_ofst(0);
#else
  sym->Set_st_ofst(Alloc_preg(preg_ty,name,home_wn));
#endif
  sym->Set_nonzerophis(NULL);
  sym->Set_st_group(0);	// clears "overlap" union
  sym->Set_synonym((AUX_ID) 0);
  sym->Set_home_sym((AUX_ID) 0);
  sym->Set_zero_cr(NULL);
#ifdef KEY
  // bug 13670
  sym->Set_value_size(0);
  sym->Set_spre_node(NULL);
#endif

  sym->Set_def_bbs(NULL);
  
  sym->Points_to()->Analyze_ST(st, sym->St_ofst(),
			       TY_size(MTYPE_To_TY(preg_ty)), 0, 0, 0,
			       FALSE /* no equiv */);
  return retv;
}


void
AUX_STAB_ENTRY::Change_to_new_preg(OPT_STAB *opt_stab, CODEMAP *htable)
{
  Is_True(Cr_list(), ("AUX_STAB_ENTRY::Change_to_new_preg: Cr_list is NULL"));

  BOOL was_formal = (ST_sclass(st) == SCLASS_FORMAL);
  MTYPE preg_ty = 0;

  CODEREP *cr;
  CODEREP_ITER iter;
  FOR_ALL_NODE(cr, iter, Init(Cr_list())) {
    if (!cr->Is_flag_set(CF_MADEUP_TYPE)) {
      preg_ty = cr->Dtyp();
      break;
    }
  }

  if (preg_ty == 0) return;

  // Turn the variable into a PREG
  const char * name = St_name();
  Set_st(MTYPE_To_PREG(preg_ty));
  Set_stype(VT_NO_LDA_SCALAR);
#ifdef TARG_NVISA
  // want to find def for filling in home_wn of preg,
  // which is needed for correct memory state info in Find_Lda.
  WN *home_wn = NULL;
  if (cr->Defstmt() && cr->Defstmt()->Rhs()) {
    home_wn = cr->Defstmt()->Rhs()->Rvi_home_wn(opt_stab);
  }
  mINT64 offset = opt_stab->Alloc_preg(preg_ty, name, home_wn);
#else
  mINT64 offset = opt_stab->Alloc_preg(preg_ty,name);
#endif
  Set_st_ofst(offset);
  Set_st_group(0);	// clears "overlap" union
  Set_synonym((AUX_ID) 0);
  Set_home_sym((AUX_ID) 0);
  Set_zero_cr(NULL);

  Set_value_size(MTYPE_size_min(cr->Dsctyp()));
  /* CVTL-RELATED start (correctness) */
#ifdef Is_True_On
  BOOL is_sign_extd = cr->Is_sign_extd();
  cr->Set_sign_extension_flag();
  Is_True(is_sign_extd == cr->Is_sign_extd(),
	  ("AUX_STAB_ENTRY::Change_to_new_preg: existing cr's sign"
	   " extension flag is inconsistent"));
#endif // Is_True_On
  if (cr->Is_sign_extd()) 
    Set_sign_extd();
  else
    Set_zero_extd();

  // Insert proper CVT/CVTL for all its STIDs
  STMTREP      *defstmt;
  CODEREP      *rhs;
  FOLD          ftmp;
  INT           cvt_kind;
  OPCODE        opc;

  FOR_ALL_NODE(cr, iter, Init(Cr_list())) {
    cr->Set_offset(offset);
    Is_True(cr->Kind()==CK_VAR,
	    ("AUX_STAB_ENTRY::Change_to_new_preg: cr is not CK_VAR"));
    if (!cr->Is_flag_set(CF_DEF_BY_CHI) && 
	(defstmt = cr->Defstmt()) != NULL && 
	(rhs = defstmt->Rhs()) != NULL){
      MTYPE    dsctyp = cr->Dsctyp();
      MTYPE    rhs_type = rhs->Dtyp();
      CODEREP *tmpcr  = Alloc_stack_cr(0);

      if ( MTYPE_is_integral(rhs_type) && MTYPE_is_integral(dsctyp) ) {
	if (WOPT_Enable_Min_Type &&
	    (rhs->Kind() == CK_VAR ||
	     (rhs->Kind() == CK_IVAR && rhs->Ivar_has_e_num())) &&
	    MTYPE_size_min(dsctyp) == MTYPE_size_min(rhs->Dsctyp()) &&
	    cr->Is_sign_extd() == rhs->Is_sign_extd())
	  continue;
	cvt_kind = NOT_AT_ALL;
	if ( MTYPE_size_min(dsctyp) < MTYPE_size_min(rhs_type) ) {
	  // truncation
	  cvt_kind = Need_type_conversion(rhs_type, dsctyp, &opc);
	  if (WOPT_Enable_Min_Type && 
	      rhs->Kind() == CK_VAR &&
	      No_truncation_by_value_size(dsctyp, Is_sign_extd(), rhs,
					  htable->Sym())) 
	    cvt_kind = NOT_AT_ALL;
	  else if (!Only_Unsigned_64_Bit_Ops && 
		   was_formal &&
		   (Language == LANG_CPLUS || Language == LANG_ANSI_C) &&
#ifdef KEY
		   opt_stab->Is_prototyped_func() &&
#endif
		   MTYPE_is_signed(dsctyp) == MTYPE_is_signed(rhs_type) &&
		   rhs->Kind() == CK_VAR &&
		   opt_stab->Aux_stab_entry(rhs->Aux_id())->Is_dedicated_preg())
	    cvt_kind = NOT_AT_ALL;
	}
      } else { // conversion between int, float, complex
	cvt_kind = Need_type_conversion(rhs_type, dsctyp, &opc);
      }

      switch ( cvt_kind ) {
      case NOT_AT_ALL:
	break;
      case NEED_CVT:
	if ( (opc == OPC_U4U8CVT || opc == OPC_U4I8CVT) && 
	    cr->Dtyp() == MTYPE_U8 ) {
	  opc = OPC_U8CVTL;
	  tmpcr->Init_expr(opc, rhs);
	  tmpcr->Set_offset(MTYPE_size_min(dsctyp));
	  rhs = ftmp.Fold_Expr(tmpcr);
	  if (!rhs) {
	    rhs = htable->Rehash(tmpcr);
//	    rhs = Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	  }
	} else {
	  tmpcr->Init_expr(opc,rhs);
	  rhs = ftmp.Fold_Expr(tmpcr);
	  if (!rhs) {
	    rhs = htable->Rehash(tmpcr);
//	    rhs = Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	  }
	}
	break;
      case NEED_CVTL:
	tmpcr->Init_expr(opc,rhs);
	tmpcr->Set_offset(MTYPE_size_min(dsctyp));
	rhs = ftmp.Fold_Expr(tmpcr);
	if (!rhs) {
	  rhs = htable->Rehash(tmpcr);
//	  rhs = Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	}
      }
      defstmt->Set_rhs(rhs);
    }
  }
  /* CVTL-RELATED finish */
}


// ====================================================================
//
// Convert_black_box is invoked by OPT_STAB::Convert_ST_to_AUX.  It
// processes the statement and generates a list of memory locations
// referenced and defined by this "black box."
// NOTE:  For future development, some black box operations may already
// have the necessary information from the input (.B file)
//
// Convert_black_box has three helper procedures:
//
// Get_symbol_info_for_cvt_io fills in the points_to structure for
// Convert_IO_statement.
//
// Process_varfmt_for_cvt_io processes a varfmt access list for
// convert_io_statement.
//
// Convert_IO_statement generates a list of memory locations referenced
// and invoked by by this "IO" statement.
// NOTE:  We treat these statements as "black boxes" everywhere else,
// but this routine allows us to peek inside the box to do a better
// job.
//
// ====================================================================


void
Get_symbol_info_for_cvt_io( POINTS_TO *points_to, WN *wn )
{
  const OPERATOR opr = WN_operator(wn);
  
  Is_True ( OPERATOR_has_aux(opr) && WN_st(wn) != NULL,
	    ("Get_symbol_info_for_cvt_io: %s has no symbol",
	     OPERATOR_name(opr)) );
  
  ST     *st = WN_st(wn);
  TY_IDX  ty = ST_type(st);
  INT64   ofst = WN_offset(wn);
  switch ( opr ) {
  case OPR_IDNAME:
  case OPR_LDA:
  case OPR_LDID:
  case OPR_STID:
    {
      INT64 size = 0;
      if ( opr == OPR_IDNAME || opr == OPR_LDA )
        size = TY_size(ty);
      else if (WN_desc(wn) == MTYPE_M)
	size = Desc_type_byte_size(wn);
      else
	size = MTYPE_size_min(WN_desc(wn)) >> 3;
      points_to->Analyze_ST(st, ofst, size, 0, 0, ty,
			    TRUE /* assume has equiv */);
    }
    break;
  case OPR_LDBITS:
  case OPR_STBITS:
      points_to->Analyze_ST(st, ofst, MTYPE_size_min(WN_desc(wn)) >> 3,
			    WN_bit_offset (wn), WN_bit_size (wn), ty, TRUE);
      break;
  default:
    points_to->Init();   
  }
}


void
OPT_STAB::Process_varfmt_for_cvt_io( WN *iown )
{
  AUX_ID idx;
  AUX_STAB_ITER aux_stab_iter(this);

  FOR_ALL_NODE(idx, aux_stab_iter, Init()) { 
    AUX_STAB_ENTRY *psym = Aux_stab_entry(idx);

    if ( ! psym->Has_nested_ref() )
      continue;

    ST     *st = psym->St();
    TY_IDX  ty = ST_type(st);

    if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) {
      fprintf( TFile, "Process_varfmt_for_cvt_io: refs:%s\n", 
		ST_name(st) );
    }

    // set some defaults
    POINTS_TO *points_to = CXX_NEW(POINTS_TO,mem_pool);
    points_to->Analyze_ST(st, 0, TY_size(ty), 0, 0, ty,
			  TRUE /* assume has equiv */);

    // assume both ref'd and def'd
    Add_black_box_ref( iown, points_to );
    Add_black_box_def( iown, points_to );
  }
}


void 
OPT_STAB::Convert_IO_statement( WN *iown, WN *wn, INT level )
{
  const OPERATOR opr = WN_operator(wn);

  Is_True( opr == OPR_IO || level > 0,
    ("OPT_STAB::Convert_IO_statement: Not an IO statement: %s",
      OPERATOR_name(opr)) );

  if ( OPERATOR_has_aux(opr)) {
    ST *st = WN_st(wn);

    // handle only variables
    if (ST_class(st) != CLASS_VAR)
      goto has_sym_end;

    // Add some indication that this value is referenced/defined
    POINTS_TO *points_to = CXX_NEW(POINTS_TO,mem_pool);
    Get_symbol_info_for_cvt_io( points_to, wn );

    Warn_todo( "OPT_STAB::Convert_IO_statement: deal with Fortran pointer.");
    if (OPERATOR_is_load(opr) || opr == OPR_LDA)
      Add_black_box_ref( iown, points_to );

    if (OPERATOR_is_store(opr) || opr == OPR_LDA)
      Add_black_box_def( iown, points_to );

    if (opr == OPR_LDID &&
	IS_FORTRAN &&
	ST_sclass(st) == SCLASS_FORMAL &&
	!ST_is_value_parm(st)) {
      POINTS_TO *points_to = CXX_NEW(POINTS_TO,mem_pool);
      points_to->Analyze_ST_as_base(st, WN_offset(wn), WN_object_ty(wn));
      Add_black_box_ref( iown, points_to );
      Add_black_box_def( iown, points_to );
    }

    if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) {
      fprintf( TFile, "Convert_IO_statement: refs:%s\n", ST_name(st) );
    }
  }
  has_sym_end:

  // check for possible references from a variable format function
  // call.  Note that this node only shows up once in an IO statement,
  // so we don't need to worry about seeing it multiple times.
  if ( opr == OPR_IO_ITEM && int(WN_intrinsic(wn)) == int(IOC_VARFMT) ) {
    Is_True( WN_operator(WN_kid0(wn)) == OPR_LDA,
      ("VARFMT without LDA") );

    Process_varfmt_for_cvt_io( iown );
  }

  // process all of the kids
  for ( INT i = 0; i < WN_kid_count(wn); i++ )
    Convert_IO_statement( iown, WN_kid(wn,i), level+1 );
}


void 
OPT_STAB::Convert_black_box( WN *wn )
{
  switch ( WN_operator(wn) ) {
    case OPR_IO:
      Convert_IO_statement( wn, wn, 0 );
      break;
    default:
      FmtAssert( FALSE,("OPT_STAB::Convert_black_box: Unknown opcode: %s",
			OPERATOR_name(WN_operator(wn))) );
      break;
  }
}


// ====================================================================
// invoked by OPT_STAB::Convert_ST_to_AUX

//// HACK HACK HACK  Move this to some common place later
static ST *
MTYPE_To_Dedicated_PREG( TYPE_ID mtype )
{
  switch ( mtype ) {
  case MTYPE_I1:
  case MTYPE_I2:
  case MTYPE_I4:
  case MTYPE_U1:
  case MTYPE_U2:
  case MTYPE_U4:
    return MTYPE_To_PREG( MTYPE_I4 );
  case MTYPE_I8:
  case MTYPE_U8:
    return MTYPE_To_PREG( MTYPE_I8 );
  default:
    return MTYPE_To_PREG( mtype );
  }
}


// ====================================================================
//
// Convert_ST_to_AUX is invoked by OPT_STAB::Create to determine the
// values of all the OPT_STAB_ENTRYs in aux_stab.  (Count_syms has
// already initialized the map st_chain_map required by Enter_symbol
// and Enter_ded_preg and Create_preg.)
//
// Convert_ST_to_AUX is one of the earlier passes in preopt over the
// whirl code, so some other folding is performed here to save another
// pass.
//
// block_wn gives the OPC_BLOCK wn that owns the wn if wn is a
// statement; otherwise, block_wn is NULL
//
// ====================================================================


void 
OPT_STAB::Convert_ST_to_AUX(WN *wn, WN *block_wn)
{
  INT32 i;
  WN *stmt;
  AUX_ID idx;
  
  if (wn == NULL)
    return;	
  
  OPERATOR   opr = WN_operator(wn);
  TYPE_ID    rtype = WN_rtype(wn);
  TYPE_ID    desc = WN_desc(wn);

  // For procedure calls, require one dedicated preg for rtype and one
  // for desc for each return register.

  if (opr == OPR_CALL || opr == OPR_ICALL || opr == OPR_INTRINSIC_CALL) {
    PREG_NUM retreg[MAX_NUMBER_OF_REGISTERS_FOR_RETURN];
    TYPE_ID      ty[MAX_NUMBER_OF_REGISTERS_FOR_RETURN];
#ifdef TARG_X8664
    ST *call_st = NULL;
    if (opr == OPR_CALL)
      call_st = WN_st(wn);
#endif

    ty[0] = MTYPE_V; // initialize it
    ty[1] = MTYPE_V; // initialize it

    // setup return preg. Later we will need to enter this into chi list
    if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (
				  MTYPE_To_TY(rtype),
				  Allow_sim_type() ? Use_Simulated
						   : Complex_Not_Simulated
#ifdef TARG_X8664
		    , call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE
#endif
				  );

      if (RETURN_INFO_count(return_info)
	  <= MAX_NUMBER_OF_REGISTERS_FOR_RETURN) 
      {
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
	  ty[i] = RETURN_INFO_mtype (return_info, i);
	  retreg[i] = RETURN_INFO_preg (return_info, i);
	}
      }

      else
	Fail_FmtAssertion ("OPT_STAB::Convert_ST_to_AUX: more than %d return"
			   " registers", MAX_NUMBER_OF_REGISTERS_FOR_RETURN);
    }

    else {  // ! WHIRL_Return_Info_On
      Get_Return_Mtypes(MTYPE_To_TY(rtype), 
			(Allow_sim_type() ? Use_Simulated
			 : Complex_Not_Simulated), &ty[0], &ty[1]);
      Get_Return_Pregs(ty[0], ty[1], &retreg[0], &retreg[1]);
    }

    if (ty[0] != MTYPE_V) {
      Enter_ded_preg(MTYPE_To_Dedicated_PREG(rtype), 
		     retreg[0],
		     MTYPE_To_TY(rtype),
		     Get_mtype_class(ty[0]));

      ty[0] = MTYPE_V; // initialize it
      ty[1] = MTYPE_V; // initialize it

      // for complex this may be non-void after M-Whirl
      if (WHIRL_Return_Info_On) {

	RETURN_INFO return_info = Get_Return_Info (
				    MTYPE_To_TY(desc),
				    Allow_sim_type() ? Use_Simulated
						     : Complex_Not_Simulated
#ifdef TARG_X8664
		    , call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE
#endif
						  );

        if (RETURN_INFO_count(return_info)
	    <= MAX_NUMBER_OF_REGISTERS_FOR_RETURN) 
        {
	  for (i = 0; i < RETURN_INFO_count(return_info); i++) {
	    ty[i] = RETURN_INFO_mtype (return_info, i);
	    retreg[i] = RETURN_INFO_preg (return_info, i);
	  }
        }
  
        else
	  Fail_FmtAssertion ("OPT_STAB::Convert_ST_to_AUX: more than %d return"
			     " registers", MAX_NUMBER_OF_REGISTERS_FOR_RETURN);
      }

      else {
	Get_Return_Mtypes(MTYPE_To_TY(desc),
			  (Allow_sim_type() ? Use_Simulated
			   : Complex_Not_Simulated), &ty[0], &ty[1]);
        Get_Return_Pregs(ty[0], ty[1], &retreg[0], &retreg[1]);
      }
      if (ty[0] != MTYPE_V) {
	Enter_ded_preg(MTYPE_To_Dedicated_PREG(desc),
		       retreg[0], 
		       MTYPE_To_TY(desc),
		       Get_mtype_class(ty[0]));
      }
    }
  }
  
  if (opr == OPR_FUNC_ENTRY || opr == OPR_ALTENTRY) {
    PREG_NUM retreg[MAX_NUMBER_OF_REGISTERS_FOR_RETURN];
    TYPE_ID	ty[MAX_NUMBER_OF_REGISTERS_FOR_RETURN];

    ty[0] = MTYPE_V; // initialize it
    ty[1] = MTYPE_V; // initialize it
    
    TY_IDX ret_ty = TY_ret_type(ST_pu_type(WN_st(wn)));

    if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (
				  ret_ty,
				  Allow_sim_type() ? Use_Simulated
						   : Complex_Not_Simulated
#ifdef TARG_X8664
				  , PU_ff2c_abi(Pu_Table[ST_pu(WN_st(wn))])
#endif
					       );

      if (RETURN_INFO_count(return_info)
	  <= MAX_NUMBER_OF_REGISTERS_FOR_RETURN) 
      {
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
	  ty[i] = RETURN_INFO_mtype (return_info, i);
	  retreg[i] = RETURN_INFO_preg (return_info, i);
	}
      }

      else
	Fail_FmtAssertion ("OPT_STAB::Convert_ST_to_AUX: more than %d return"
			   " registers", MAX_NUMBER_OF_REGISTERS_FOR_RETURN);
    }

    else {
      Get_Return_Mtypes(ret_ty,
			(Allow_sim_type() ? Use_Simulated
			 : Complex_Not_Simulated), &ty[0], &ty[1]);
      Get_Return_Pregs(ty[0], ty[1], &retreg[0], &retreg[1]);
    }
    if (ty[0] != MTYPE_V) {
      // exit mu list contains dedicated registers.
      idx = Enter_ded_preg(MTYPE_To_Dedicated_PREG(ty[0]), retreg[0],
			   MTYPE_To_TY(ty[0]), Get_mtype_class(ty[0]));
      if (ty[1] != MTYPE_V) {
	idx = Enter_ded_preg(MTYPE_To_Dedicated_PREG(ty[1]), retreg[1], 
			     MTYPE_To_TY(ty[1]), Get_mtype_class(ty[1]));
      }
    }
  }

  // fold ILOAD-LDA to LDID
  if (WOPT_Enable_Simp_Iload) {
    if (OPERATOR_is_scalar_iload (opr)) {
      WN *twn = WN_SimplifyIload(OPCODE_make_op(opr, rtype, desc),
				 WN_load_offset(wn), WN_ty(wn),
				 WN_field_id(wn), WN_load_addr_ty(wn),
				 WN_kid0(wn)); 
      if (twn) {
	opr = WN_operator(twn);
        FmtAssert(opr == OPR_LDID || opr == OPR_LDBITS || opr == OPR_INTCONST,
                  ("Unknown operator: opr:%s when simpilfying iload", 
                  OPERATOR_name(opr)));
	rtype = WN_rtype(twn);
	desc = WN_desc(twn);
	// reset map_id since OPR_INTCONST is in different mapcat group than OPR_LDID/OPR_LDBITS.
	if (opr == OPR_INTCONST) 
	  WN_set_map_id(wn, -1);
	WN_change_operator(wn, opr);
	WN_set_rtype(wn, rtype);
	WN_set_desc(wn, desc);
        if (opr == OPR_INTCONST)
          WN_const_val(wn) = WN_const_val(twn);
        else {
          WN_load_offset(wn) = WN_load_offset(twn);
          WN_st_idx(wn) = WN_st_idx(twn);
          WN_set_ty(wn, WN_ty(twn));
	  WN_kid0(wn) = NULL;
        }
	WN_Delete(twn);
      }
    }
    else if (OPERATOR_is_scalar_istore (opr)) {
/*
 * This problem has been fixed in WN_SimplifyIstore.
 *    TY_IDX newty;
 *    if (TY_kind(WN_ty(wn)) == KIND_POINTER)
 *      newty = TY_pointed(WN_ty(wn));
 *     else newty = WN_ty(wn);
 *    WN *twn = WN_SimplifyIstore(OPCODE_make_op(opr, rtype, desc),
 *			         WN_load_offset(wn), newty,
 *			         WN_kid0(wn), WN_kid1(wn));
*/

      WN *twn = WN_SimplifyIstore(OPCODE_make_op(opr, rtype, desc),
				  WN_load_offset(wn), WN_ty(wn),
				  WN_field_id(wn), WN_kid0(wn), WN_kid1(wn));
      if (twn) {
	opr = WN_operator(twn);
	rtype = WN_rtype(twn);
	desc = WN_desc(twn);
        WN_change_operator(wn, opr);
	WN_set_rtype(wn, rtype);
	WN_set_desc(wn, desc);
        WN_load_offset(wn) = WN_load_offset(twn);
        WN_st_idx(wn) = WN_st_idx(twn);
        WN_set_ty(wn, WN_ty(twn));
	WN_kid0(wn) = WN_kid0(twn);
	WN_Delete(twn);
      }
    }
  }

  if (Phase() == MAINOPT_PHASE && 
      Only_Unsigned_64_Bit_Ops && 
      ! Delay_U64_Lowering) {
    if ((OPERATOR_is_scalar_load (opr) && WN_class(wn) != CLASS_PREG ||
	 opr == OPR_ILOAD) &&
        MTYPE_is_integral(rtype))
      Is_True(MTYPE_size_min(rtype) == 64 &&
	      (MTYPE_size_min(desc) == 64 || !MTYPE_signed(desc)) ||
	      MTYPE_size_min(desc) == 64 && MTYPE_size_min(rtype) == 32,
	      ("Convert_ST_to_AUX: illegal types in load instruction."));
  }

  // fold TRUEBR/FALSEBR whose condition is constant to goto 
  if ((opr == OPR_TRUEBR || opr == OPR_FALSEBR) && 
      WN_operator(WN_kid0(wn)) == OPR_INTCONST) {
    WN *test = WN_kid0(wn);
    if (opr == OPR_TRUEBR && WN_const_val(test) != 0 ||
	opr == OPR_FALSEBR && WN_const_val(test) == 0) {
      WN_Delete(test);
      WN_change_operator(wn, OPR_GOTO);
      WN_set_rtype(wn, MTYPE_V);
      WN_set_desc(wn, MTYPE_V);
      WN_set_kid_count(wn, 0);
      WN_kid0(wn) = NULL;
      WN_st_idx(wn) = (ST_IDX) 0;

      if ( Cur_PU_Feedback )
	Cur_PU_Feedback->FB_simplify_branch_to_goto( wn );
    }
  }

  //  Recompute addr used locally
  if (opr == OPR_LDA) {
    ST *st = WN_st(wn);
    if (ST_class(st) == CLASS_VAR) {
      
      Set_BE_ST_addr_used_locally(st);
    }
  }

  if (OPERATOR_has_aux(opr)) {
    ST    *st  = WN_st(wn);
    idx = Enter_symbol(WN_operator(wn), st, WN_offset(wn), WN_object_ty(wn), 
		       Check_volatility (wn, st), wn);
    Is_True(idx != 0, ("Convert_ST_to_AUX:  index is 0."));
    WN_set_aux(wn, idx);
  }

  // some regions in the whirl are black boxes; ignore them
  if (opr == OPR_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL,("OPT_STAB::Convert_ST_to_AUX, NULL rid"));
    Is_True(REGION_consistency_check(wn),("OPT_STAB::Convert_ST_to_AUX"));
    if (RID_level(rid) >= Rgn_level()) { // black box region
      if (_rgn_trace)
	fprintf(TFile,"OPT_STAB::Convert_ST_to_AUX, skipping previously "
		"processed RGN %d, %s\n", RID_id(rid),RID_level_str(rid));
      return;
    }
#ifdef KEY
    BOOL has_parallel_pragma = FALSE;
    BOOL in_parallel_region_save = FALSE;
    if (Phase() != MAINOPT_PHASE && PU_has_mp(Get_Current_PU()) && 
	! PU_mp_lower_generated(Get_Current_PU())) {
      has_parallel_pragma = Is_region_with_pragma(wn, WN_PRAGMA_MASTER_BEGIN);
    }
    for (i = 0; i < WN_kid_count(wn); i++) {
      if (has_parallel_pragma && i == 2) {
	in_parallel_region_save = in_parallel_region; // save previous value
	in_parallel_region = TRUE; // set new value
      }
      Convert_ST_to_AUX(WN_kid(wn,i), NULL);
      if (has_parallel_pragma && i == 2) 
	in_parallel_region = in_parallel_region_save; // restore previous value
    }
    return;
#endif
  }

  if (opr == OPR_ASM_STMT) {
    // Special handling for output operands of asm statements.
    WN *prag = WN_first(WN_asm_constraints(wn));
    while (prag != NULL) {
      FmtAssert(WN_pragma(prag) == WN_PRAGMA_ASM_CONSTRAINT,
		("Unknown pragma type for ASM output constraint"));
      AUX_ID idx = Enter_ded_preg(WN_st(prag),
				  WN_pragma_asm_copyout_preg(prag),
				  ST_type(WN_st(prag)),
				  Get_mtype_class(TY_mtype(ST_type(WN_st(prag)))));
      WN_set_aux(prag, idx);
      prag = WN_next(prag);
    }
  }

  if (opr == OPR_BLOCK) {
    for (stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  
      Convert_ST_to_AUX(stmt, wn);
  }
  else if (OPERATOR_is_black_box(opr)) {
    // process the statement so we get list of memory locations
    // referenced and defined by this statement
    Convert_black_box( wn );
  }
  else {
    for (i = 0; i < WN_kid_count(wn); i++) {
      Convert_ST_to_AUX(WN_kid(wn,i), NULL);
    }
    // after the kids are entered into the symbol table,
    // update the loop-index attribute
    if (opr == OPR_DO_LOOP) {
      WN *init_stmt = WN_start(wn);  // the init statement
      Is_True(WN_operator(init_stmt) == OPR_STID,
	      ("bad DO-loop init statement."));
      AUX_ID aux = WN_aux(init_stmt);
      aux_stab[aux].Set_loop_index();  // mark this ST as an loop index variable
    }
  }
}


// ====================================================================
//
// For scalar variables that are based on the same base_st and that are
// statically aliased (by equivalence, union, etc.) st_group is
// maintained as a circularly linked list with the properties that:
// 1. The entries in any list cover a contiguous region of address
//    space (no gaps), and
// 2. If two aux ID's overlap, they are in the same list, and
// 3. Each list is minimal in size subject to the first two properties.
//
// Make_st_group (invoked by OPT_STAB::Create) links up and orders the
// static alias groups, using st_chain_map as a starting point.
//
// cmp_ofst compares the offset fields of two AUX_STAB_ENTRY.
// cmp_ofst is used by the qsort in Make_st_group().
//
// ====================================================================


static INT32 
cmp_ofst(AUX_STAB_ENTRY **t1, AUX_STAB_ENTRY **t2)
{
  INT64 ofst1 = (*t1)->Base_byte_ofst() * 8 + (*t1)->Bit_ofst();
  INT64 ofst2 = (*t2)->Base_byte_ofst() * 8 + (*t2)->Bit_ofst();
  if (ofst1 < ofst2)
    return -1;
  else if (ofst1 > ofst2)
    return 1;
  else
    return 0;
}
  

void 
OPT_STAB::Make_st_group(void)
{
  AUX_ID idx, group;
  ST *base;
  AUX_STAB_ITER aux_stab_iter(this);
  MEM_POOL tmp_pool;

  // Step 1:  Link up the symbols with identical bases.

  FOR_ALL_NODE(idx, aux_stab_iter, Init()) {

    ST *st = aux_stab[idx].St();
    if (st == NULL)
      continue;

    if (ST_class(st) != CLASS_VAR) {
      // Skip const and other
      // non-variable objects
      continue;
    }

    if (ST_sclass(aux_stab[idx].St()) == SCLASS_REG)    // Skip pregs
      continue;

    base = aux_stab[idx].Base();

    //  If the base is not allocated an entry
    //  use the index of the current aux stab entry.

    ST_CHAIN_INFO *st_chain_info = st_chain_map->Lookup(ST_st_idx(base));
    if (st_chain_info == NULL) {
      st_chain_info = CXX_NEW(ST_CHAIN_INFO(idx), &_st_chain_pool);
      st_chain_map->Insert(ST_st_idx(base), st_chain_info);
      group = idx;
    }
    else {
      group = st_chain_info->List_head();
      Is_True(group != (AUX_ID) 0,
	      ("OPT_STAB::Make_st_group: Inconsistent st_chain_info"));
    }

    //  Prepend to the st_group list
    if (group != idx) {
       aux_stab[idx].Set_st_group(aux_stab[group].St_group());
       aux_stab[group].Set_st_group(idx);
    }
  }

  // Step 2:  Break into smaller alias group (as circular list)
  // At this point, ST_temp (or the st_chain_info->List_head()) of
  // each base ST contains the aux ID of the beginning of the unsorted
  // circular list representing the group for that base.

  OPT_POOL_Initialize(&tmp_pool, "Temp used in OPT_STAB::Make_st_group",
		      FALSE, MEM_DUMP_FLAG+10);
  OPT_POOL_Push(&tmp_pool, MEM_DUMP_FLAG+10);

  AUX_STAB_ENTRY **sorted = (AUX_STAB_ENTRY **) 
     OPT_POOL_Alloc(&tmp_pool, aux_stab.Lastidx() * sizeof(AUX_STAB_ENTRY *),
		    MEM_DUMP_FLAG+10);

  FOR_ALL_NODE(idx, aux_stab_iter, Init()) {

    // Skip non-variables
    if (aux_stab[idx].St() == NULL ||
	ST_class(aux_stab[idx].St()) != CLASS_VAR)
      continue;

    // Reset ST_group for registers because they don't have equiv class.
    if (ST_sclass(aux_stab[idx].St()) == SCLASS_REG) {
      aux_stab[idx].Set_st_group(0);
      continue;
    }

    base = aux_stab[idx].Base();
    ST_CHAIN_INFO *st_chain_info = st_chain_map->Lookup(ST_st_idx(base));
    Is_True(st_chain_info != NULL,
	    ("OPT_STAB::Make_st_group: Inconsistent st_chain_map"));
    group = st_chain_info->List_head();
    Is_True(group != (AUX_ID) 0,
	    ("OPT_STAB::Make_st_group: Inconsistent st_chain_info"));

    if (group != idx) continue;   // skip non-head links 

    if ( aux_stab[group].St_group() ) {
      INT32 count = 0;
      while ( group ) {
	sorted[count++] = &aux_stab[group]; 
        group = aux_stab[group].St_group();
      }
      Is_True( count > 1, ("count must be > 1."));

      // Sorting
      qsort(sorted, count, sizeof(AUX_STAB_ENTRY *),
	    (INT32 (*)(const void *, const void *)) cmp_ofst);

      // Populate addr_taken attributes
      // According to Raymond, we need WOPT_Enable_Improved_Addr_Taken
      // to be TRUE for correctness. The idea here is that we need to
      // make sure the appropriate addr-taken bits are set for
      // anything statically aliased with an object that has one or
      // more of the bits set. The front end doesn't provide this
      // condition, so we do it here.
      if (WOPT_Enable_Improved_Addr_Taken) {
	INT64 addr_used_high = sorted[0]->Base_byte_ofst() - 1;
	INT64 addr_saved_high = sorted[0]->Base_byte_ofst() - 1;
	INT64 addr_passed_high = sorted[0]->Base_byte_ofst() - 1;
	for (INT32 j = 0; j < count; j++) {
	  INT64 upper = sorted[j]->Base_byte_ofst();
	  if (sorted[j]->Byte_size() > 0)
	    upper = sorted[j]->Base_byte_ofst() + sorted[j]->Byte_size() - 1;
	  else {
	    TY_IDX ty = ST_type(sorted[j]->St());
	    if (ty != 0 && TY_kind(ty) == KIND_ARRAY)
	      upper = LONG_MAX; // assume array size is infinite
	  }

	  // For addr-used-locally
	  if (BE_ST_addr_used_locally(sorted[j]->St())) {
	    if (upper > addr_used_high)
	      addr_used_high = upper;
	  }
	  else {
	    // Include the addr_taken of the ST with smaller ofsts
	    if (sorted[j]->Base_byte_ofst() <= addr_used_high) 
	      Set_BE_ST_addr_used_locally(sorted[j]->St());
	    else {
	      // Include the addr_taken of the ST with larger ofsts
	      for (INT32 k = j + 1; k < count; k++) {
		if (sorted[k]->Base_byte_ofst() > upper) break; // out of range
		if (BE_ST_addr_used_locally(sorted[k]->St())) {
		  Set_BE_ST_addr_used_locally(sorted[j]->St());
		  break;
		}
	      }
	    }
	  }

	  // For addr-saved
	  if (ST_addr_saved(sorted[j]->St())) {
	    if (upper > addr_saved_high)
	      addr_saved_high = upper;
	  } else {
	    // Include the addr_taken of the ST with smaller ofsts
	    if (sorted[j]->Base_byte_ofst() <= addr_saved_high) {
	      Set_ST_addr_saved(sorted[j]->St());
	    }
	    else 
	      // Include the addr_taken of the ST with larger ofsts
	      for (INT32 k = j + 1; k < count; k++) {
		if (sorted[k]->Base_byte_ofst() > upper) break; // out of range
		if (ST_addr_saved(sorted[k]->St())) {
		  Set_ST_addr_saved(sorted[j]->St());
		  break;
		}
	      }
	  }

	  // For addr-passed
	  if (BE_ST_addr_passed(sorted[j]->St())) {
	    if (upper > addr_passed_high)
	      addr_passed_high = upper;
	  } else {
	    // Include the addr_taken of the ST with smaller ofsts
	    if (sorted[j]->Base_byte_ofst() <= addr_passed_high) {
	      Set_BE_ST_addr_passed(sorted[j]->St());
            }
	    else 
	      // Include the addr_taken of the ST with larger ofsts
	      for (INT32 k = j + 1; k < count; k++) {
		if (sorted[k]->Base_byte_ofst() > upper) break; // out of range
		if (BE_ST_addr_passed(sorted[k]->St())) {
		  Set_BE_ST_addr_passed(sorted[j]->St());
		  break;
		}
	    }
	  }
	}
      }

      // Remove virtual variables
      INT32 count_var = 0;
      for (INT32 j = 0; j < count; j++) {
#ifndef KEY // cannot remove virtual variables because Lower_to_extract_compose 
	    // can introduce a use of the virtual var as real var
	if (sorted[j]->Is_real_var())
#endif
	  sorted[count_var++] = sorted[j];
      }

      // Separate into subgroups
      INT64 low = sorted[0]->Base_byte_ofst() * 8 +
	sorted[0]->Bit_ofst();				    /* inclusive */
      INT64 hi = low + (sorted[0]->Bit_size() == 0 ?
			sorted[0]->Byte_size() * 8 :
			sorted[0]->Bit_size());		    /* exclusive */
      AUX_ID st_group_head = aux_stab.Idx(sorted[0]);
      AUX_ID st_group_tail = st_group_head;
      aux_stab[st_group_tail].Set_st_group(0);

      for (INT32 i = 1; i < count_var; i++) {
	INT64 ofst = sorted[i]->Base_byte_ofst() * 8 + sorted[i]->Bit_ofst();
        Is_True( ofst >= 
		 sorted[i-1]->Base_byte_ofst() * 8 + sorted[i-1]->Bit_ofst(),
		 ("Make_st_group: bad sorting."));
	UINT64 size = sorted[i]->Bit_size() == 0 ?
	  sorted[i]->Byte_size() * 8 : sorted[i]->Bit_size();
        if (ofst < hi) {
#if defined(linux) || defined(BUILD_OS_DARWIN)
//         hi = (( hi > ofst + size ) ?  hi : ofst + size ) ;
           if (hi < ofst + size)
             hi = ofst + size;
#else
           hi = max(hi, (INT64) (ofst + size));
#endif /* linux */
           AUX_ID new_tail = aux_stab.Idx(sorted[i]);
           aux_stab[st_group_tail].Set_st_group( new_tail );
           aux_stab[new_tail].Set_st_group(st_group_head);
           st_group_tail = new_tail;
        } else {
           // Split
           low = ofst;
           hi = low + size;
           st_group_head = st_group_tail = aux_stab.Idx(sorted[i]);
           aux_stab[st_group_tail].Set_st_group(0);
        }
      }
    }
  }

  OPT_POOL_Pop( &tmp_pool, MEM_DUMP_FLAG+10 );
  OPT_POOL_Delete ( &tmp_pool, MEM_DUMP_FLAG+10 );
}


// ====================================================================
//
// Canonicalize is invoked near the end of OPT_STAB::Create to identify
// and combine "synonyms" within the aux_stab.  Two variables with
// identical <base, offset, size> may have different aux_stab entries
// because initially they have different ST.
//
// Canonicalize first searches aux_stab for pairs of equivalent
// entries, and replaces the higher numbered entry with the lower
// numbered synonym entry.  Next, Cononicalize remaps STs into their
// lowest numbered synonym.
//
// Canonicalize has three helper procedures:
//
// Equivalent returns TRUE iff two symbols are equivalent.
//
// Completely_replaced returns TRUE iff that symbol can be safely
// replaced by a lower numbered synonym.
//
// Remap_aux_synonym walks through the WN tree to convert all STs into
// their lowest numbered synonyms.
// 
// ====================================================================


BOOL 
AUX_STAB_ENTRY::Equivalent(AUX_STAB_ENTRY *sym)
{
  if (this->Base() == sym->Base() &&
      this->Base_byte_ofst() == sym->Base_byte_ofst() &&
      this->Byte_size() == sym->Byte_size() &&
      this->Bit_ofst() == sym->Bit_ofst() &&
      this->Bit_size() == sym->Bit_size() &&
      this->Mclass() == sym->Mclass() &&
      this->Field_id() == sym->Field_id())
    return TRUE;
  return FALSE;
}


BOOL
Completely_replaced(AUX_STAB_ENTRY *psym)
{
  if (psym->Is_real_var() &&
      !psym->Is_virtual() &&
      // Fix 647617:  the loop index variable has a equivalenced synonym.
      // Do not remap a loop index variable.
      //
      // If remap is done (i.e., the index variable is replaced),
      // we need to update the loop->Index() that is represented in WHIRL.
      // The update is required so that alias analysis knows a CALL in
      // the loop nest cannot modify the replacement.
      //
      // If we replace the index variable by an array reference, the
      // update would be complicated.
      //   
      !psym->Loop_index() &&
      // Part of the fix for 666220: Do not canonicalize variables
      // that had inconsistent IP alias class information (usually due
      // to guarded out of bounds array accesses).
      (psym->Points_to()->Ip_alias_class() != PESSIMISTIC_AC_ID) &&
      psym->Synonym())
    return TRUE;
  else
    return FALSE;
}


void 
OPT_STAB::Remap_aux_synonym(WN *wn)
{
  INT32 i;
  
  if (wn == NULL) return;	
  const OPERATOR opr = WN_operator(wn);

  // Remap aux for wn
  if ( OPERATOR_has_aux(opr)) {
    // Deal with LDID and STID
    AUX_ID idx = WN_aux(wn);
    if (Completely_replaced(Aux_stab_entry(idx))) {
      AUX_ID syn = aux_stab[idx].Synonym();
      WN_set_aux(wn, syn);
      if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) 
	fprintf(TFile, "remap auxid %d to %d\n", idx, syn);
      WN_offset(wn) = aux_stab[syn].St_ofst();
    }
  }

  // Apply Remap_aux_synonym to children of wn
  // any regions in the whirl are black boxes, ignore
  if (opr == OPR_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL, ("OPT_STAB::Remap_aux_synonym, NULL rid"));
    if (RID_level(rid) >= Rgn_level()) // black box
      return;
  }
  if (opr == OPR_BLOCK) {
    for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  
      Remap_aux_synonym(stmt);
  }
  else if ( !OPERATOR_is_black_box( opr ) ) {
    for (i = 0; i < WN_kid_count(wn); i++) 
      Remap_aux_synonym(WN_kid(wn, i));
  }
}


void 
OPT_STAB::Canonicalize(void)
{
  AUX_ID idx, cur;
  AUX_STAB_ITER aux_stab_iter(this);

  // Step 1:  Identify all synonyms
  FOR_ALL_NODE(idx, aux_stab_iter, Init()) {
    if (!aux_stab[idx].Is_real_var())        // Skip non-variables
      continue;
    if (cur = aux_stab[idx].St_group()) {    // things belongs the same base.
      AUX_ID min = idx;
      while (cur && cur != idx) {
	if (min > cur && aux_stab[idx].Equivalent(&aux_stab[cur])) {
	  min = cur;
	}
        cur = aux_stab[cur].St_group();
      }
      if (min != idx) { 	// Synonym found
	aux_stab[idx].Set_synonym(min);
      } else {
	aux_stab[idx].Set_synonym(0);
      }
    }
  }

  //  Step 2:  Remap STs into their lowest numbered synonyms
  Remap_aux_synonym(pu_wn);
}


// ====================================================================
//
// Collect_ST_attr is invoked by OPT_STAB::Create to accumulate alias
// information from the OPT_STAB_ENTRYs into bitsets stored as fields
// of OPT_STAB.
//
// Update_attr_cache is a helper procedure invoked by Collect_ST_attr.
// Update_attr_cache accumulates alias information for one
// OPT_STAB_ENTRY.
//
// Incorporate_alias_class_info is invoked by Pre_optimizer in
// opt_main.cxx after OPT_STAB creation and after alias analysis, in
// order to refine information in the OPT_STAB to reflect alias
// classification's analysis.
//
// ====================================================================


//  Update_attr_cache updates the bitvector cache of the alias information
//  in the POINTS_TO data structure.  There are some adjustment to the
//  alias cache that is used to compute mu/chi.
//
//   local_static:   Not in POINTS_TO.  Remember all the PSTATIC variables
//                   in the same nest level. Those might be converted into 
//                   simple scalars.
//   dedicated:      Collect dedicated registers.
//   external:       Variables that has global attr in the POINTS_TO or
//                   return registers.
//   const:          Include PSTATIC that is not modified.
//   virtual_var:    Collect all virtual variables.
//
void OPT_STAB::Update_attr_cache(AUX_ID idx, ST *st, POINTS_TO *pt,
				 BOOL *has_weak_var)
{
  // Update the cache from POINTS_TO info
  if (WOPT_Enable_Update_Vsym && aux_stab[idx].Is_virtual()) 
    Set_virtual_var(idx);

  if (st == NULL)  {

    Set_addr_used_locally(idx);
    Set_external(idx);

  } else {

    // Collect information readily available from POINTS_TO
    if (!pt->Not_addr_saved())
      Set_addr_saved(idx);
    if (!pt->Not_addr_passed())
      Set_addr_passed(idx);
    if (pt->Global())
      Set_external(idx);
    if (pt->Formal() && IS_FORTRAN && !ST_is_value_parm(st))
      Set_ref_formal(idx);
#ifdef KEY
    if (pt->Const()) {
      if (ST_is_initialized(st)) {
	aux_stab[idx].Set_const_init();
      }
#else
    if (pt->Const()) 
#endif
      Set_const(idx);
#ifdef KEY
    }
#endif
    if (pt->Named())
      Set_named(idx);

    // Compute the other information from STs
    if (!pt->Const()) {
      ST *sclass_st = st;

      // According to Mike Murphy, the new symbol table guarantees us
      // that the sclass for the st is the same as the sclass for its
      // base, unless the base is SCLASS_UNKNOWN and CLASS_BLOCK:
      //
      // Mike sez:
      // In the new symtab we don't have SCLASS_BASED, but we continue
      // to allow any sclass ST to be based on another ST, as long as
      // they follow the restrictions that ... the sclass'es must
      // match, or the base sclass must be unknown and class block.
      //
      // The upshot is that we shouldn't have to do anything here. The
      // st will tell us the storage class.

      switch (ST_sclass(sclass_st)) {
      case SCLASS_AUTO:
      case SCLASS_FORMAL:
      case SCLASS_FSTATIC:
	if (BE_ST_addr_used_locally(st))
	  Set_addr_used_locally(idx);
	break;

      case SCLASS_FORMAL_REF:
	if (BE_ST_addr_used_locally(st))
	  Set_addr_used_locally(idx);
	break;

      case SCLASS_PSTATIC:
	if (BE_ST_addr_used_locally(st))
	  Set_addr_used_locally(idx);
	// #ifdef __761842_FIXED__
        //
        // Previoys, PSTATICs were in the global symbol table,
        // which means that this if statement always evaluated to
        // FALSE (CURRENT_SYMTAB != GLOBAL_SYMTAB). 
        // After Mike changed gfec to place PSTATICs in the local symtab,
        // the if here may evaluate to TRUE. This caused the assertion
        // failure in ALIAS_CLASSIFICATION::Classify_deref_of_expr, 
        // because we were looking for AUX_ID corresponding to the RHS
        // of const initialized st, but the RHS st was not found because
        // at the times when it could have been entered (Count_syms,
        // Enter_symbol), the LHS st was only initialized and not yet const.
        // 
        // However silly it may look, #ifdef-ing this whole if statement
        // should give us back the old behaviour (when PSTATICs were in
        // the global symtab). It would probably be better if we could
        // assert the constness of this PSTATIC, but it is too late to
        // enter the RHS of its INITV at this point.
        //
	if (
	    !ST_addr_saved(st) &&
	    !BE_ST_addr_passed(st) &&
	    !BE_ST_addr_used_locally(st) &&
	    CURRENT_SYMTAB == ST_IDX_level(ST_st_idx(st)) && // not an uplevel ref
	    aux_stab[idx].St_group() == 0 &&  // not a const if has equiv
            aux_stab[idx].Byte_size() == TY_size(ST_type(st)) && // not a const if other
							    // aux ID's share this ST
	    !aux_stab[idx].Is_virtual() &&    // not a const if virtual
	    !aux_stab[idx].Dmod()) {  // then it is a const
#ifndef KEY // this cause the variable to become read-only, then srdata, which
	    // causes some unknown problem with the SGI assembler; by not
	    // setting this, it will become sdata which is more efficiently
	    // accessed than rodata
	  Set_ST_is_const_var(st);	    
#endif
	  Set_const(idx);
	  TCON tc;
	  if (ST_is_const_initialized(st)) {
	    Inc_const_found();
	    aux_stab[idx].Set_const_init();
	  }
	}
	// #endif /* __761842_FIXED__ */
	if (ST_is_private_local(st))
	  Set_local_static(idx);
	break;
	  
      case SCLASS_COMMON:
      case SCLASS_EXTERN:
      case SCLASS_UGLOBAL:
      case SCLASS_DGLOBAL:  // public globals
	if (BE_ST_addr_used_locally(st))
	  Set_addr_used_locally(idx);

	if (pt->Weak()) {
	  Set_weak_var(idx);
	  if (has_weak_var != NULL)
	    *has_weak_var = TRUE;
	}

#ifdef KEY
 	if (ST_visible_outside_dso(st)) break;
	if (ST_is_const_var(st) && ST_is_initialized(st)) {
	  aux_stab[idx].Set_const_init();
	}
#endif
	break;
	
      case SCLASS_REG:
	// neither address taken nor global
	// set up for calls that may modify dedicated registers
	if (pt->Dedicated())
	  Set_dedicated(idx);
	break;

      case SCLASS_TEXT:
	break;

      default:  
	FmtAssert(FALSE, ("Handle ST class %d.", ST_sclass(st)));
      }
    }
  }
}


//  Collect the ST attribute for individual OPT_STAB entries
//  Cleanup for next PU.
void 
OPT_STAB::Collect_ST_attr(void)
{
  AUX_ID idx;
  AUX_STAB_ITER aux_stab_iter(this);
  INT32 n = aux_stab.Lastidx() + 1;
  BOOL  have_weak_var = FALSE;
  
  Set_addr_saved( BS_Create_Empty(n, mem_pool) );
  Set_addr_passed( BS_Create_Empty(n, mem_pool) );
  Set_addr_used_locally( BS_Create_Empty(n, mem_pool) );
  Set_external  ( BS_Create_Empty(n, mem_pool) );
  Set_dedicated ( BS_Create_Empty(n, mem_pool) );
  Set_ref_formal( BS_Create_Empty(n, mem_pool) );
  Set_const     ( BS_Create_Empty(n, mem_pool) );
  Set_named     ( BS_Create_Empty(n, mem_pool) );
  Set_local_static( BS_Create_Empty(n, mem_pool) );
  if (WOPT_Enable_Unique_Pt_Vsym)
    Set_unique_pt ( BS_Create_Empty(n, mem_pool) );
  Set_virtual_var( BS_Create_Empty(n, mem_pool) ); 
  Set_weak_var( BS_Create_Empty(n, mem_pool) );
  Set_weak_base( BS_Create_Empty(n, mem_pool) );
  Set_inaccessible_to_callees( BS_Create_Empty(n, mem_pool) );
  
  FOR_ALL_NODE(idx, aux_stab_iter, Init()) {
    
    AUX_STAB_ENTRY *psym = Aux_stab_entry(idx);
    psym->Set_def_bbs(NULL);
    ST *st = psym->St();
    const INT32 stype = psym->Stype();

    if (stype == VT_OTHER || stype == VT_UNKNOWN) continue;
      
    // Update POINTS_TO 
    POINTS_TO *pt = psym->Points_to();
    
    if (st != NULL) {
#ifdef KEY
      // If "psym" has a field-id, we probably have very accurate type
      // information, so don't destroy it. (bug 9989)
      TY_IDX ty = (psym->Field_id() != 0 && psym->Ty() != 0) ? psym->Ty() :
                   (ST_class(st) == CLASS_VAR ? ST_type(st) : (TY_IDX)0);
#endif 
      // Do not discard the high-level type which may be different from
      // object-type. This is an example: "LDID agg.field". The agg.field
      // is treated as a separate symbol (i.e. it has unique aux_id) by 
      // Enter_symbol(). The pt->Ty() record the type-of(agg.field), and
      // pt->Highlevel_ty() records type-of(agg).
      //
      TY_IDX hl_ty = pt->Highlevel_Ty();
      pt->Analyze_ST(st, psym->St_ofst(), psym->Byte_size(),
		     psym->Bit_ofst(), psym->Bit_size(),
#ifdef KEY
		     ty,
#else 
		     ST_class(st) == CLASS_VAR ? ST_type(st) : (TY_IDX)0,
#endif
		     psym->St_group() != 0 /* has equiv? */);
      // Fix 541255: 
      //  if a symbol has size 0, its offset is considered unknown.
      if (pt->Byte_Size() == 0)
	pt->Set_ofst_kind(OFST_IS_UNKNOWN);

      if (hl_ty != (TY_IDX)0 && pt->Highlevel_Ty() == (TY_IDX)0) {
	pt->Set_hl_ty(hl_ty);
      }
    }

    // Precompute the alias attributes and put them in bitsets.
    Update_attr_cache(idx, st, pt, &have_weak_var);
  }

  // Support weak symbols.  This is not the best way.  It is better
  // if frontend can mark all the symbols that are strong.
  //
  if (have_weak_var) {
    const BS *weak_set = Weak_var();
    for (AUX_ID idx = BS_Choose( weak_set );
	 idx != (AUX_ID) BS_CHOOSE_FAILURE;
	 idx = BS_Choose_Next ( weak_set, idx)) {
      AUX_STAB_ENTRY *psym = Aux_stab_entry(idx);
      ST *st = psym->St();
      ST *weak_base = ST_strong(st);
      AUX_ID weakidx;
      FOR_ALL_NODE(weakidx, aux_stab_iter, Init()) {
	AUX_STAB_ENTRY *psym = Aux_stab_entry(weakidx);
	if (psym->Is_real_var() && psym->St() == weak_base) {
	  POINTS_TO *pt = psym->Points_to();
	  Set_weak_base(weakidx);
	  pt->Set_weak_base();
	  pt->Reset_no_alias();
	  // because the weak sym may be addr taken/passed!
	  pt->Reset_not_addr_saved();
	  pt->Reset_not_addr_passed();

	  // Update the bitvector cache
	  Update_attr_cache(weakidx, psym->St(), pt,
			    NULL /*no update to has_weak_var*/);
	}
      }
    }
  }

  // Precomputed bit vector for more efficient alias analysis.
  //   -- collect the set of scalars that might be aliased with
  //      an indirect memop.
  {
    // should include all external()s if IPA is not on.
    BS *indirect = Addr_saved();
    indirect = BS_Union(indirect, Addr_passed(), mem_pool);
    indirect = BS_UnionD(indirect, Addr_used_locally(), mem_pool);
    indirect = BS_IntersectionD(indirect, Named());
    indirect = BS_DifferenceD(indirect, Const());
    Set_indirect(indirect);
  }

  // Precomputed bit vector for more efficient alias analysis.
  //   -- collect the set of scalars that might be aliased with a call.
  {
    // have both addr_saved and addr_passed
    BS *call_by_value = BS_Union(Addr_saved(), External(), mem_pool);
    call_by_value = BS_UnionD(call_by_value, Addr_passed(), mem_pool);
    call_by_value = BS_IntersectionD(call_by_value, Named());
    call_by_value = BS_DifferenceD(call_by_value, Const());
    call_by_value = BS_UnionD(call_by_value, Dedicated(), mem_pool);
    Set_call_by_value(call_by_value);   // used for call by value

    BS *call_by_ref = BS_Union(Addr_saved(), External(), mem_pool);
    call_by_ref = BS_UnionD(call_by_ref, Addr_passed(), mem_pool);
    call_by_ref = BS_IntersectionD(call_by_ref, Named());
    call_by_ref = BS_DifferenceD(call_by_ref, Const());
    call_by_ref = BS_UnionD(call_by_ref, Dedicated(), mem_pool);
    Set_call_by_ref(call_by_ref);  // used for call by reference
  }

  // Precomputed bit vector for more efficient alias analysis.
  //   -- collect the set of scalars that might be aliased with an
  //      inline assembly statement
  {
    // asm can dereference pointers
    BS *asm_alias_set = Addr_saved();
    // asm can call other procedures
    asm_alias_set = BS_Union(asm_alias_set, Addr_passed(), mem_pool);
    // worry only about named objects; virtuals come later
    asm_alias_set = BS_IntersectionD(asm_alias_set, Named());
    // don't worry about const objects either
    asm_alias_set = BS_DifferenceD(asm_alias_set, Const());
    // asm can access dedicated registers; note that they must be
    // listed in the clobber set to be altered, but they can be read
    // regardless. There is no way for users to tell us they aren't.
    asm_alias_set = BS_UnionD(asm_alias_set, Dedicated(), mem_pool);
    Set_asm_alias(asm_alias_set);
  }
}


// Refine information in the OPT_STAB to reflect alias
// classification's analysis.
void
OPT_STAB::Incorporate_alias_class_info(void)
{
  Set_inaccessible_to_callees(BS_UnionD(Alias_classification()->
					Inaccessible_to_callees(),
					Inaccessible_to_callees(),
					mem_pool));
}


// ====================================================================
//
// Create creates the optimizer symbol table.
//
// ====================================================================
void 
OPT_STAB::Create(COMP_UNIT *cu, REGION_LEVEL rgn_level)
{
  AUX_STAB_ITER aux_stab_iter(this);

  // Initialize fields in this OPT_STAB
  htable = cu->Htable();
  _cfg   = cu->Cfg();
  pu_wn  = cu->Input_tree();
  _rule  = cu->Arule();
  _allow_sim_type = (cu->Phase() != MAINOPT_PHASE);
  _phase = cu->Phase();
  _rgn_level = rgn_level;
#ifdef KEY
  _alias_mgr = cu->Alias_mgr();
#endif

  OPERATOR opr = WN_operator(pu_wn);
  Is_True(opr == OPR_FUNC_ENTRY || opr == OPR_REGION,
	  ("WN is not FUNC_ENTRY or REGION entry"));
  if (opr == OPR_FUNC_ENTRY) {
    _is_varargs_func = TY_is_varargs(ST_pu_type(WN_st(pu_wn)));
#ifdef KEY
    _is_prototyped_func = TY_has_prototype(ST_pu_type(WN_st(pu_wn)));
#endif
  }
  else {
    _is_varargs_func = FALSE; // regions don't even have parameters
#ifdef KEY
    _is_prototyped_func = TRUE;
#endif
  }

  // Initial ST chain mapping is built in Count_syms, and then refined
  // in Convert_ST_to_AUX before its final use in Make_st_group().
  // st_chain_map groups the ST's into sets sharing a common base for
  // use in Convert_ST_to_AUX.

  // Note: C++ is broken in that it uses the same preprocessor as C,
  // and the C preprocessor sees '<' as "less than" instead of "begin
  // template argument". Therefore it seems we can't use CXX_NEW
  // properly to construct instances of template classes, so I've had
  // to introduce a stupid typedef. Yucky.
  typedef ID_MAP<ST_CHAIN_INFO *, ST_IDX> STUPID_COMPILER;

  st_chain_map = CXX_NEW(STUPID_COMPILER(256,
					 NULL,
					 &_st_chain_pool,
					 FALSE),
			 &_st_chain_pool);

  st_chain_map->Init();

  Collect_addr_passed_for_PU(pu_wn);

  // Fix 648051: move loop normalization after BE address flag is valid
  if (WOPT_Enable_IVR) {

    SET_OPT_PHASE("Loop normalization");
    BOOL trace_loop = Get_Trace (TP_WOPT2, LOOP_NORM_FLAG);
    if (trace_loop) {
      fprintf (TFile, "%sDump before Loop Normalization \n%s", DBar, DBar);
      fdump_tree (TFile, cu->Input_tree ());
    }

    cu->Normalize_loop(cu->Input_tree());
    
    if (trace_loop) {
      fprintf (TFile, "%sDump after Loop Normalization \n%s", DBar, DBar);
      fdump_tree (TFile, cu->Input_tree ());
    }

    SET_OPT_PHASE("Create AUX Symbol table");
  } 

  // Setup links to regular symtab, build chi and mu functions
  aux_sym_cnt = 0;
  Count_syms(pu_wn);

  // set the BE address flag
  Set_BE_ST_pu_has_valid_addr_flags(Get_Current_PU_ST());

  // allocate a slightly larger array to avoid realloc.
  aux_stab.Alloc_array(unsigned(aux_sym_cnt*1.2)+10);
  aux_stab.Setidx(aux_sym_cnt);
  aux_stab.Bzero_array();

  // Initialize aux_stab entries
  Convert_ST_to_AUX(pu_wn, NULL);
  
  Collect_nested_ref_info();

  // PREG related initialization
  Init_last_preg(Get_Preg_Num(PREG_Table_Size(CURRENT_SYMTAB)));
  
  // Setup ST alias groups
  Make_st_group();
  // Identify synonyms within aux_stab, and convert STs into their
  // lowest numbered synonyms
  Canonicalize();

  // Create the return vsym
  AUX_ID return_vsym = Create_vsym(EXPR_IS_ADDR);
  Set_return_vsym(return_vsym);
  POINTS_TO *pt = Aux_stab_entry(return_vsym)->Points_to();
  pt->Init();
  pt->Set_expr_kind(EXPR_IS_ADDR);
  pt->Set_base_kind(BASE_IS_UNKNOWN);
  pt->Set_ofst_kind(OFST_IS_UNKNOWN);
  pt->Set_base(NULL);
  pt->Set_global();
 
  // Collect ST attributes (used by FFA and FSA)
  Collect_ST_attr();
  

  // Setup empty du chain
  _ver_stab = CXX_NEW(VER_STAB_ARRAY_TYPE(&_ver_pool), &_ver_pool);
  // _ver_stab->Alloc_array(aux_sym_cnt*2);
  // _ver_stab->Setidx(1);  // Do not use the 0-th and 1-th entry 
  UINT dummy;
  _ver_stab->New_entry(dummy);  // Do not use the 0-th and 1-th entry
  _ver_stab->New_entry(dummy);
  Is_True(_ver_stab->Size() == 2, ("New_entry of ver_stab failed."));
}


// ====================================================================
// ====================================================================
//
// Done with the code for OPT_STAB::Create.  Below is the other stuff.
//
// ====================================================================
// ====================================================================


AUX_ID
OPT_STAB::Find_vsym_with_base(ST *st)
{
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(this);
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *vsym = Aux_stab_entry(i);
    if (vsym->Points_to()->Based_sym() == st)
      return i;
  }
  return (AUX_ID) 0;
}


void
OPT_STAB::Init_mp_attribute(void)
{
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(this);
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *vsym = Aux_stab_entry(i);
    vsym->Set_mp_shared();
    vsym->Reset_mp_lastlocal();
    vsym->Reset_mp_reduction();
    vsym->Reset_mp_firstprivate();
  }
}


AUX_ID
OPT_STAB::Find_vsym_with_base_ofst_and_size(ST *base,
					    INT64 byte_ofst, INT64 byte_size, 
					    UINT8 bit_ofst, UINT8 bit_size)
{
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(this);
  AUX_ID best_vsym = 0;

  if (byte_size > 0) {
    // look for scalar vsym  with the best size fit.
    //
    UINT64 best_bit_size = UINT64_MAX;
    UINT8 best_bit_ofst = 0;
    UINT64 target_bit_size;
    UINT8 target_bit_ofst;

    if (bit_size == 0) {
      // not a bit field
      target_bit_size = byte_size * 8;
      target_bit_ofst = 0;
    } else {
      target_bit_size = bit_size;
      target_bit_ofst = bit_ofst;
    }
    
    FOR_ALL_NODE(i, aux_stab_iter, Init()) {
      AUX_STAB_ENTRY *vsym = Aux_stab_entry(i);
      if ((vsym->Is_real_var() || vsym->Stype() == VT_LDA_VSYM) && 
	  vsym->Base() == base &&
	  vsym->Base_byte_ofst() == byte_ofst) {

	if (vsym->Bit_size() == 0) {
	  // vsym not a bit field
	  UINT64 vsym_size = vsym->Byte_size() * 8;
	  if (target_bit_ofst == 0 && vsym_size == target_bit_size) {
	    best_vsym = i;
	    goto found_scalar_vsym;
	  } else if (best_bit_ofst == 0 &&
		     vsym_size < best_bit_size &&
		     vsym_size >= target_bit_size) {
	    best_vsym = i;
	    best_bit_size = vsym_size;
	  }
	} else {
	  // vsym is a bit field
	  if (vsym->Bit_ofst() == target_bit_ofst &&
	      vsym->Bit_size() == target_bit_size) {
	    best_vsym = i;
	    goto found_scalar_vsym;
	  } else if (vsym->Bit_ofst() <= target_bit_ofst &&
		     (vsym->Bit_ofst() + vsym->Bit_size()) >=
		     (target_bit_ofst + target_bit_size)) {
	    if (vsym->Bit_ofst() >= best_bit_ofst &&
		(vsym->Bit_ofst() + vsym->Bit_size()) <
		(best_bit_ofst + best_bit_size)) {
	      best_vsym = i;
	      best_bit_size = vsym->Bit_size();
	      best_bit_ofst = vsym->Bit_ofst();
	    }
	  }
	}
      }
    }
    found_scalar_vsym:
    if (best_vsym != 0) {
      INT32 stype = Aux_stab_entry(best_vsym)->Stype();
      if (stype == VT_NO_LDA_SCALAR ||
	  (stype == VT_LDA_VSYM &&
	   Aux_stab_entry(best_vsym)->Byte_size() == byte_size &&
	   Aux_stab_entry(best_vsym)->Bit_size() == bit_size)) {
	Aux_stab_entry(best_vsym)->Set_stype(VT_LDA_SCALAR);
      }
      return best_vsym;
    }

    // if no scalar can be found, the iload/istore cannot be
    // converted into a ldid/stid, so simply pick
    // one of the LDA vsym that covers the accessed range.
    FOR_ALL_NODE(i, aux_stab_iter, Init()) {
      AUX_STAB_ENTRY *vsym = Aux_stab_entry(i);
      if ((vsym->Stype() == VT_LDA_VSYM || vsym->Stype() == VT_LDA_SCALAR) &&
	  vsym->Base() == base &&
	  vsym->Base_byte_ofst() <= byte_ofst &&
	  vsym->Base_byte_ofst() + vsym->Byte_size() >= byte_ofst + byte_size)
	return i;
    }
  } 

  // if ofst and size are not known
  // pick up the LDA vsym with base+ofst with size 0
  // (that's the LDA of the ST with TY_size(ST_type()) == 0.
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *vsym = Aux_stab_entry(i);
    if (vsym->Stype() == VT_LDA_VSYM &&
	vsym->Base() == base &&
	vsym->Base_byte_ofst() == byte_ofst &&
	vsym->Byte_size() == 0) 
	return i;
  }
  return 0;
}


AUX_ID
#ifdef KEY
OPT_STAB::Find_vsym_with_st(ST *st, BOOL indirect, POINTS_TO * pt)
#else
OPT_STAB::Find_vsym_with_st(ST *st)
#endif
{
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(this);
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *vsym = Aux_stab_entry(i);
#ifdef KEY
    if (vsym->Is_virtual() && vsym->St() == st &&
        (vsym->Indirect_access()!=0) == indirect &&
        (!WOPT_Enable_New_Vsym_Allocation || !pt /* probably temporary */ ||
         Rule()->Aliased_Memop(vsym->Points_to(), pt)))
#else
    if (vsym->Is_virtual() && vsym->St() == st)
#endif
      return i;
  }
  return (AUX_ID) 0;
}


AUX_ID
OPT_STAB::Find_sym_with_st_and_ofst(ST *st, INT64 ofst)
{
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(this);
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *sym = Aux_stab_entry(i);
    if (sym->St() == st && sym->St_ofst() == ofst)
      return i;
  }
  return (AUX_ID) 0;
}

#ifdef KEY // taken from wn_mp.cxx
static const char * const dope_str_prefix = ".dope." ;
static const INT dope_str_prefix_len = 6;

BOOL
ST_Has_Dope_Vector(ST *st) {
  if (ST_class(st) != CLASS_VAR)
    return FALSE;

  if ( TY_is_f90_pointer(ST_type(st)) )
    return TRUE;

  TY_IDX ty = ST_type(st);
  while (TY_kind(ty) == KIND_POINTER)
    ty = TY_pointed(ty);

  if (TY_kind(ty) == KIND_STRUCT &&
      strncmp(TY_name(ty), dope_str_prefix, dope_str_prefix_len) == 0)
    return TRUE;

  return FALSE;
}
#endif

// ====================================================================
//
// Identify_vsym enters a virtual symbol into the symbool table.
// This code contains the heuristics to assign virtual variables.
//
// This routine assumes OPT_STAB::Create() has run to completion.
// The client of this routine is flow-free alias analysis
// (OPT_STAB::Allocate_mu_chi_and_virtual_var in
// opt_alias_analysis.cxx).
//
// ====================================================================

AUX_ID
OPT_STAB::Identify_vsym(WN *memop_wn)
{
  POINTS_TO scratch_pt;
  scratch_pt.Init();
  if (Update_From_Restricted_Map(memop_wn, &scratch_pt)) {
    // the memop has a restricted map entry, and its based_sym and
    // attributes appear in scratch_pt now.
    Is_True(scratch_pt.Based_sym() != NULL,
	    ("Based symbol must be set for restricted map entry"));
    // Only Unique_pt and Restricted items get this special
    // non-analysis.
    if (scratch_pt.Unique_pt() ||
	scratch_pt.Restricted()) {
      AUX_ID var = Find_vsym_with_base(scratch_pt.Based_sym());
      if (var == 0) {
	var = Create_vsym(EXPR_IS_ANY);
	AUX_STAB_ENTRY *vsym = Aux_stab_entry(var);
	vsym->Points_to()->Copy_fully(scratch_pt);
	vsym->Set_stype(VT_UNIQUE_VSYM);
      }
      else {
	Is_True(Aux_stab_entry(var)->Stype() == VT_UNIQUE_VSYM ||
		Aux_stab_entry(var)->Stype() == VT_SPECIAL_VSYM,
		("vsym based on unique/restrict pointer must be unique "
		 "or special"));
      }
      if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) {
	fprintf(TFile, "Identify_vsym: Returning aux_id %u as unique "
		"based on\n", var);
	Print_ST(TFile, Aux_stab_entry(var)->Points_to()->Based_sym(), TRUE);
      }
      return var;
    }
  }
  OPERATOR opr = WN_operator(memop_wn);
  WN *addr_wn = ((OPERATOR_is_scalar_istore (opr) ||
		  opr == OPR_MSTORE) ? WN_kid1(memop_wn) : WN_kid0(memop_wn));
  INT64 offset = (opr == OPR_PARM || opr == OPR_ASM_INPUT ? (INT64) 0 
  							: WN_offset(memop_wn));

  // Raymond says to delete the (offset == 0) clause. -- RK 981106
  BOOL direct_use = ((addr_wn != NULL) &&
		     (WN_operator(addr_wn) == OPR_LDID) &&
		     (offset == 0));

  addr_wn = Find_addr_recur(addr_wn, *this);
  //  identify the LDA from the addr expression
  if (addr_wn != NULL) {
    switch (WN_operator(addr_wn)) {
    case OPR_LDA:
      AUX_ID var;
      var = WN_aux(addr_wn);
      Is_True(var != 0, ("lda not entered into aux_stab."));
      return var;
    case OPR_LDID: 
      {
        ST *st = aux_stab[WN_aux(addr_wn)].St();
        AUX_ID vsym_id;
	// if it is the Fortran parameter, return the vsym
	if (ST_sclass(st) == SCLASS_FORMAL&&
	    IS_FORTRAN &&
	    Alias_Pointer_Parms &&
	    ! ST_is_value_parm(st)) {
	  vsym_id = Find_vsym_with_base(st);
	  if (vsym_id == 0) {
	    vsym_id = Create_vsym(EXPR_IS_ANY);
	    AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	    vsym->Points_to()->Set_based_sym(st);
	    if (direct_use) {
	      vsym->Set_stype(VT_UNIQUE_VSYM);
	    }
	  }
	  else {
	    if (! direct_use) {
	      AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	      vsym->Set_stype(VT_SPECIAL_VSYM);
	    }
	  }
	  return vsym_id;
	}
	if (WOPT_Enable_Unique_Pt_Vsym && 
	    ST_class(st) == CLASS_VAR &&
	    (ST_pt_to_unique_mem(st) ||
#ifdef KEY // workaround f90 front-end not setting ST_pt_to_unique_mem flag
//Bug 327 & 328
	     // ST_Has_Dope_Vector(st) ||
#endif
	     TY_is_restrict(ST_type(st)))) {
	  vsym_id = Find_vsym_with_base(st);
#ifdef KEY // workaround f90 front-end not setting ST_pt_to_unique_mem flag
//Bug 327 & 328
	  //if (WOPT_Enable_Unique_Pt_Vsym && ST_Has_Dope_Vector(st))
	  //  Set_ST_pt_to_unique_mem(st);
#endif
	  if (vsym_id == 0) {
	    vsym_id = Create_vsym(EXPR_IS_ANY);
	    AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	    if (ST_pt_to_unique_mem(st)) {
	      vsym->Points_to()->Set_unique_pt();
	    }
	    if (TY_is_restrict(ST_type(st))) {
	      vsym->Points_to()->Set_restricted();
	    }
	    vsym->Points_to()->Set_based_sym(st);
	    if (direct_use) {
	      vsym->Set_stype(VT_UNIQUE_VSYM);
	    }
	  }
	  else {
	    if (! direct_use) {
	      AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	      vsym->Set_stype(VT_SPECIAL_VSYM);
	    }
	  }
	  return vsym_id;
	}
#ifdef KEY
	// bug 9582: Don't give unique vsyms to dope vectors.
	if (WOPT_Enable_Vsym_Unique && !ST_Has_Dope_Vector(st)) {
	  vsym_id = Find_vsym_with_st(st, !direct_use);
	  if (vsym_id == 0) {
	    vsym_id = Create_vsym(EXPR_IS_ANY);
	    AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	    vsym->Points_to()->Set_based_sym(NULL);
	    vsym->Set_st(st);
	    if (direct_use) {
	      vsym->Set_stype(VT_UNIQUE_VSYM);
	    }
	    else {
	      Is_True (vsym->Special_vsym(),
	               ("Identify_vsym: Expected VT_SPECIAL_VSYM"));
	      vsym->Set_indirect_access();
	    }
	  }
	  else {
//#ifdef Is_True_On
	    AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	    // TODO: Change to is_true later.
	    FmtAssert (direct_use == !vsym->Indirect_access(),
	             ("Identify_vsym: incompatible access flag in vsym"));
	    if (!direct_use)
	      // Change to is_true later.
	      FmtAssert (vsym->Special_vsym(),
	                 ("Identify_vsym: Expected VT_SPECIAL_VSYM"));

//#endif
	  }
	  return vsym_id;
	}
#else
	if (WOPT_Enable_Vsym_Unique) {
	  vsym_id = Find_vsym_with_st(st);
	  if (vsym_id == 0) {
	    vsym_id = Create_vsym(EXPR_IS_ANY);
	    AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	    vsym->Points_to()->Set_based_sym(NULL);
	    vsym->Set_st(st);
	    if (direct_use) {
	      vsym->Set_stype(VT_UNIQUE_VSYM);
	    }
	  }
	  else {
	    if (! direct_use) {
	      AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
	      vsym->Set_stype(VT_SPECIAL_VSYM);
	    }
	  }
	  return vsym_id;
	}
#endif
      }
    default:
      addr_wn = NULL;
    }
  }
  
  if ((OPERATOR_is_load (opr) || OPERATOR_is_store (opr)) &&
       WOPT_Enable_Vsym_Unique) {
      IDTYPE ac = _alias_classification->Alias_class(memop_wn);
      if (ac != OPTIMISTIC_AC_ID && ac != PESSIMISTIC_AC_ID) {
          AUX_ID vsym_id = _ac_2_vsym_map.Lookup(ac);
        if (vsym_id == (AUX_ID)0) {
          vsym_id = Create_vsym(EXPR_IS_ANY);
          AUX_STAB_ENTRY *vsym = Aux_stab_entry(vsym_id);
          vsym->Set_stype(VT_UNIQUE_VSYM);
          vsym->Points_to()->Set_alias_class (ac);
          _ac_2_vsym_map.Insert(ac, vsym_id);
        }
        return vsym_id;
     }
  }

  // return default vsym
  if (Default_vsym() == 0) {
    // Setup default vsym
    Set_default_vsym(Create_vsym(EXPR_IS_ANY));
    // Update the POINTS_TO with default vsym info
    Aux_stab_entry(Default_vsym())->Points_to()->Set_default_vsym();
  }
  return Default_vsym();
}


// ====================================================================
// ====================================================================


//  Clear the coderep field.   Used by Emitter.
void 
OPT_STAB::Clear_coderep(void)
{
  for (INT32 i = 0; i <= aux_stab.Lastidx(); i++) 
    aux_stab[i].Clear_coderep(); 
}


// Reset the def_bbs field.  Used by SSU
void 
OPT_STAB::Reset_def_bbs(void)
{
  for (INT32 i = 0; i <= aux_stab.Lastidx(); i++) 
    aux_stab[i].Set_def_bbs(NULL);
}


//   Allocate coderep stacks into the aux_stab
void 
OPT_STAB::New_coderep(MEM_POOL *pool)
{
  for (INT32 i = 0; i <= aux_stab.Lastidx(); i++) 
    aux_stab[i].Set_coderep(CXX_NEW(STACK<CODEREP*>(pool), pool));
}


//   Allocate stacks into the aux_stab
void 
OPT_STAB::New_stack(MEM_POOL *pool)
{
  for (INT32 i = 0; i <= aux_stab.Lastidx(); i++)
    aux_stab[i].Set_stack(CXX_NEW(STACK<AUX_ID>(pool), pool));
}


//   At end of phase, check that all variable stacks are empty
//   Allow 1 element on stack for global vars referenced before defined.
void
OPT_STAB::Check_stack(void)
{
  for (INT32 i = 0; i <= aux_stab.Lastidx(); i++) 
    Is_True(Stack_elements(i) <= 1, ("Non empty stack in aux_stab"));
}


//   Generate a new SSA name for var.
//      - modifies the version & stack of aux_stab.
//
VER_ID
OPT_STAB::Gen_name(AUX_ID var)
{
  UINT32 i = Version(var);
  VER_ID du;
  _ver_stab->New_entry(du);
  Ver_stab_entry(du)->Init(var, i+1, NULL, NULL, NO_STMT);
  Set_version(var, i+1);
  Stack(var)->Push(du);
  return du;
}


//   Generate a temp SSA for the result of a phi function.
//   Return 0 if all of its operand are 0.
//
VER_ID
OPT_STAB::Gen_name_phi(PHI_NODE *phi)
{
  VER_ID du;
  du = Gen_name(phi->Aux_id());
  phi->Set_result(du);
  return du;
}


//   Generate a temp SSA for the result of a chi function.
//
VER_ID
OPT_STAB::Gen_name_chi(CHI_NODE *chi, WN *wn)
{
  VER_ID du;
  du = Gen_name(chi->Aux_id());
  Ver_stab_entry(du)->Set_chi_wn(wn);
  chi->Set_result(du);
  return du;
}


//   Get the current SSA name for var.
//      - update stack top if the name is 0.
//
VER_ID
OPT_STAB::Get_name(AUX_ID var)
{
  VER_ID du;
  du = Stack(var)->Top();
  return du;
}


//  Representation of references/definitions for black-boxes
//
POINTS_TO_LIST *
OPT_STAB::Black_box_refs(const WN *wn) const
{
  return (POINTS_TO_LIST *) WN_MAP_Get(WN_box_refs(), wn);
}


POINTS_TO_LIST *
OPT_STAB::Black_box_defs(const WN *wn) const
{
  return (POINTS_TO_LIST *) WN_MAP_Get(WN_box_defs(), wn);
}


void
OPT_STAB::Add_black_box_ref( WN *wn, POINTS_TO *ref ) const
{
  POINTS_TO_LIST *ptl = Black_box_refs(wn);

  if ( ptl == NULL ) {
    ptl = CXX_NEW(POINTS_TO_LIST,mem_pool);
    WN_MAP_Set( WN_box_refs(), wn, (void *)ptl );
  }

  ptl->Prepend( ref, mem_pool );
}


void
OPT_STAB::Add_black_box_def( WN *wn, POINTS_TO *def ) const
{
  POINTS_TO_LIST *ptl = Black_box_defs(wn);

  if ( ptl == NULL ) {
    ptl = CXX_NEW(POINTS_TO_LIST,mem_pool);
    WN_MAP_Set( WN_box_defs(), wn, (void *)ptl );
  }

  ptl->Prepend( def, mem_pool );
}


// search a mu list for a var
// (this can't be in opt_mu_chi.h because it references the iterator)
MU_NODE *
MU_LIST::Search_mu_node( AUX_ID var )
{
  MU_LIST_ITER mu_iter;
  MU_NODE *munode;
  FOR_ALL_NODE(munode, mu_iter, Init(this))
    if ( munode->Aux_id() == var )
      return munode;
  return NULL;
}


// search a chi list for a var
// (this can't be in opt_mu_chi.h because it references the iterator)
CHI_NODE *
CHI_LIST::Search_chi_node( AUX_ID var )
{
  CHI_LIST_ITER chi_iter;
  CHI_NODE *chinode;
  FOR_ALL_NODE(chinode, chi_iter, Init(this))
    if ( chinode->Aux_id() == var )
      return chinode;
  return NULL;
}


//  Set the operand of the MU_NODE
void
MU_NODE::Set_OPND(CODEREP *cr, BOOL set_dont_prop)
{
  _u._cr = cr; 
  if (cr && set_dont_prop)
    cr->Set_flag(CF_DONT_PROP);
}


//  Set the operand of the CHI_NODE
void
CHI_NODE::Set_OPND(CODEREP *cr, BOOL set_dont_prop)
{
  _opnd._cr = cr;
  if (cr && set_dont_prop)
    cr->Set_flag(CF_DONT_PROP);
}


//  Walk the WN tree to convert ST into their
//  lowest numbered synonyms.
//
void 
OPT_STAB::Remap_ver_synonym(WN *wn)
{
  INT32 i;

  if (wn == NULL) return;	

  const OPERATOR opr = WN_operator(wn);

  Is_True(!OPERATOR_is_scf(opr) || opr == OPR_REGION, ("Wn is SCF"));

  if (WN_has_ver(wn)) {
    VER_ID idx = WN_ver(wn);
    if (Ver_stab_entry(idx)->Synonym())  {
      VER_ID s = idx;
      while (s != 0 && Ver_stab_entry(s)->Synonym())
	s = Ver_stab_entry(s)->Synonym();
      WN_set_ver(wn, s);
      if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) 
	fprintf(TFile, "remap verid %d to %d\n", idx, s);
    }
  }

  if ( WN_has_mu(wn, Cfg()->Rgn_level()) ) {
    OCC_TAB_ENTRY *occ = Get_occ(wn);
    if (occ->Is_stmt()) {
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;
      MU_LIST *mu_list = Get_stmt_mu_list(wn);
      FOR_ALL_NODE( mnode, mu_iter, Init( mu_list)) {
	VER_ID idx = mnode->Opnd();
	if (Ver_stab_entry(idx)->Synonym())  {
	  VER_ID s = idx;
	  while (s != 0 && Ver_stab_entry(s)->Synonym())
	    s = Ver_stab_entry(s)->Synonym();
	  mnode->Set_opnd(s);
	}
      }
    } else {
      MU_NODE *mnode = Get_mem_mu_node(wn);
      VER_ID idx = mnode->Opnd();
      if (Ver_stab_entry(idx)->Synonym())  {
	VER_ID s = idx;
	while (s != 0 && Ver_stab_entry(s)->Synonym())
	  s = Ver_stab_entry(s)->Synonym();
	mnode->Set_opnd(s);
      }
    }
  }
  
  if ( WN_has_chi(wn, Cfg()->Rgn_level()) ) {
    CHI_LIST_ITER chi_iter;
    CHI_NODE *cnode;
    CHI_LIST *chi_list = Get_generic_chi_list(wn);
    FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
      VER_ID idx = cnode->Opnd();
      if (Ver_stab_entry(idx)->Synonym())  {
	VER_ID s = idx;
	while (s != 0 && Ver_stab_entry(s)->Synonym())
	  s = Ver_stab_entry(s)->Synonym();
	cnode->Set_opnd(s);
      }
    }
  }
  // any regions in the whirl are black boxes, ignore
  if (opr == OPR_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL,("OPT_STAB::Remap_aux_synonym, NULL rid"));
    if (RID_level(rid) >= Rgn_level()) 
      return;
  }

  if ( opr == OPR_COMPGOTO ) {
    // only handle the index expression
    Remap_ver_synonym(WN_kid(wn,0));
  }
  // don't remap "black-box" statements
  else if (!OPERATOR_is_black_box(opr) && opr != OPR_EXC_SCOPE_BEGIN) {
    if (opr == OPR_ASM_STMT) {
      i = 2;
    }
    else {
      i = 0;
    }
    for (; i < WN_kid_count(wn); i++) {
      Remap_ver_synonym(WN_kid(wn,i));
    }
  }
}


// ====================================================================
// Manage the mapping of aux_stab entries to itable bit positions
// ====================================================================


// Clear all of the entries (setup)
//
void
OPT_STAB::Clear_itab_bitpos( void )
{
  AUX_ID var;
  AUX_STAB_ITER aux_stab_iter(this);

  FOR_ALL_NODE(var, aux_stab_iter, Init()) {
    Set_itab_bitpos( var, ILLEGAL_BP );
  }
}


// Process virtual variables' aux_id_list and rename them to the
// corresponding bit positions assigned to the variables
//
void
OPT_STAB::Rename_aux_id_list_to_bitpos( void )
{
  AUX_ID var;
  AUX_STAB_ITER aux_stab_iter(this);

  FOR_ALL_NODE(var, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *vsym = Aux_stab_entry( var );
    if ( vsym->Is_virtual() ) {
      AUX_ID_LIST_ITER id_list_iter;
      AUX_ID_NODE *id_node;
      FOR_ALL_ELEM( id_node, id_list_iter, Init(vsym->Aux_id_list()) ) {
	AUX_STAB_ENTRY *id_entry = Aux_stab_entry(id_node->Aux_id());
	id_node->Set_aux_id( (AUX_ID) id_entry->Itab_bitpos() );
      }
    }
  }
}


// go through region pragmas and convert aux_ids back to STs and offsets
// this is called by the main emitter
void
OPT_STAB::Convert_EH_pragmas(WN *wn)
{
  Is_True(REGION_is_EH(wn), ("OPT_STAB::Convert_EH_pragmas, wrong kind"));
  WN *stmt, *pragmas = WN_region_pragmas(wn);
  STMT_ITER stmt_iter;
  FOR_ALL_ELEM(stmt, stmt_iter, Init(WN_first(pragmas), WN_last(pragmas))) {
    if (WN_operator(stmt) == OPR_CALL) {
      for (INT i=0; i<WN_kid_count(stmt); i++) { // go through params
	WN *kid = WN_kid0(WN_kid(stmt,i)); // parm, then LDA
	if (WN_has_aux(kid)) {	// convert aux_id to ST and offset
	  AUX_ID aux_id = WN_aux(kid);
	  WN_st_idx(kid) = ST_st_idx(St(aux_id));
	  WN_offset(kid) = St_ofst(aux_id);
	}
      }
      return;
    }
  }
}


// ====================================================================
// ====================================================================
// PRINT FUNCTIONS
// ====================================================================
// ====================================================================


//  Print the list of chi functions.
//
void
CHI_NODE::Print(FILE *fp) const
{
#ifdef KEY
  fprintf(fp, "sym%dv%d <- chi( sym%dv%d ) %s\n", Aux_id(), Result(), Aux_id(), Opnd(), Live() ? "LIVE" : "NOT LIVE");
#else
  fprintf(fp, "sym%d <- chi( sym%d ) %s\n", Result(), Opnd(),
	  Live() ? "LIVE" : "NOT LIVE");
#endif
}

void     
CHI_NODE::Print_Deref(FILE *fp) const
{
  if (Live())
    fprintf(fp, "sym%dv%d <- chi( sym%dv%d )\n",
                Aux_id(), RESULT()->Version(), 
                Aux_id(), OPND()->Version());
  else
    fprintf(fp, "(not live) sym%dv%d <- chi( sym%dv%d )\n",
                Aux_id(), Result(), 
                Aux_id(), Opnd());
}

void
CHI_LIST::Print(FILE *fp)
{
  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;

  FOR_ALL_NODE(cnode, chi_iter, Init(this)) {
    cnode->Print(fp);
  }
}


//  Print the list of mu functions.
//
void
MU_NODE::Print(FILE *fp) const
{
#ifdef KEY
  fprintf(fp, " mu[ sym%dv%d ]\n", Aux_id(), Opnd());
#else
  fprintf(fp, " sym%d ", Aux_id());
#endif
}

void
MU_NODE::Print_Deref(FILE *fp) const
{
  fprintf(fp, " sym%dv%d ", Aux_id(), OPND()->Version());
}

void
MU_LIST::Print(FILE *fp)
{
  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;

  fprintf(fp, "       mu[");
  FOR_ALL_NODE(mnode, mu_iter, Init(this)) {
    fprintf(fp, " sym%dv%d ", mnode->Aux_id(), mnode->Opnd());
  }
  fprintf(fp, "]\n");
}


//   Print the auxillary stab entry
//
void OPT_STAB::Print_aux_entry(AUX_ID i, FILE *fp)
{
  BB_LIST *bbl;
  AUX_STAB_ENTRY *psym = &aux_stab[i];
  
  bbl = psym->Def_bbs();
  fprintf(fp, " [%3d] ", i);
  if (i == Return_vsym())  fprintf(fp,"Return_vsym ");
  if (i == Default_vsym()) fprintf(fp,"Default_vsym ");
  switch (psym->Stype()) {
  case VT_NO_LDA_SCALAR:
  case VT_LDA_SCALAR:
  case VT_LDA_VSYM:
  case VT_UNIQUE_VSYM:
  case VT_SPECIAL_VSYM:

    if (aux_stab[i].St() != NULL) {
      fprintf(fp, "VAR   st=%s base=%s byte ofst=%lld byte size=%lld"
	      " bit ofst=%d bit size=%d sclass=%d mclass=%d\n",
	      aux_stab[i].St_name(), aux_stab[i].Base_name(),
	      aux_stab[i].Base_byte_ofst(), aux_stab[i].Byte_size(),
	      aux_stab[i].Bit_ofst(), aux_stab[i].Bit_size(),
	      ST_sclass(aux_stab[i].St()), aux_stab[i].Mclass());
    } else 
      fprintf(fp, "VAR   st=null\n");
    
    if (psym->Is_real_var())
      fprintf(fp, " is_real");
    if (psym->Is_virtual())
      fprintf(fp, " is_virtual");
    if (psym->Loop_index())
      fprintf(fp, " loop-index");
    if (psym->Has_nested_ref())
      fprintf(fp, " nested-ref");
    if (psym->Is_volatile())
      fprintf(fp, " volatile");
    if (psym->Disable_local_rvi())
      fprintf(fp, " disable-local-rvi");
    if (psym->Lr_shrink_cand())
      fprintf(fp, " lr-shrink-cand");
    fprintf(fp, "\n");
    if (Addr_saved(i) || Addr_passed(i) || External(i)
	|| Ref_formal(i) || Unique_vsym(i)) {
      fprintf(fp, "       attr=");
      if (Addr_saved(i)) fprintf(fp, "addr_saved ");
      if (Addr_passed(i)) fprintf(fp, "addr_passed ");
      if (External(i))   fprintf(fp, "extern ");
      if (Ref_formal(i)) fprintf(fp, "ref_formal ");
      if (Unique_vsym(i)) fprintf(fp, "unique_vsym ");
      fprintf(fp, "\n");
    }
    break;
    
  case VT_OTHER:
    fprintf(fp, "SYMBL st=%s base=%s ofst=%lld\n",
	    aux_stab[i].St_name(), aux_stab[i].Base_name(),
	    aux_stab[i].Base_byte_ofst());
    break;
    
  default:
    fprintf(fp, "UNKNOWN TYPE\n" );
    break;
  }
  if (psym->Is_real_var() || psym->Is_virtual()) {
    if (bbl && bbl->Len() > 0) {
      fprintf(fp, "       defined in BBs ");
      bbl->Print(fp);
      fprintf(fp, "\n");
    }
  }
  if (psym->Is_real_var()) {
    if (aux_stab[i].St_chain()) {
      fprintf(fp, "       st_chain ");
      VER_ID cur = i;
      while (cur) {
	fprintf(fp, " %d", cur);
	cur = aux_stab[cur].St_chain();
      }
      fprintf(fp, "\n");
    }
    if (aux_stab[i].St_group()) {
      fprintf(fp, "       st_group ");
      VER_ID cur = i;
      do {
	fprintf(fp, " %d", cur);
	cur = aux_stab[cur].St_group();
      } while (cur && cur != i); 
      fprintf(fp, "\n");
    }
  }
}


//  Print the AUX_STAB
//
#ifdef KEY
void OPT_STAB::Print(FILE *fp, WN *entry_wn)
#endif
{
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(this);

  fprintf( TFile, "%sAux symbol table\n%s", DBar, DBar );

  fprintf(fp, "aux_stab.Lastidx()=%d\n", aux_stab.Lastidx());
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    Print_aux_entry(i, fp);
  }

  fprintf( TFile, "%sOcc table\n%s", DBar, DBar );
#ifdef KEY
  if (entry_wn)
    Print_occ_tab(fp,WN_func_body(entry_wn));
#endif
  //Print_occ_tab(fp);
}


BOOL
AUX_STAB_ENTRY::Has_multiple_signs(void) const
{
  CODEREP *cr;
  CODEREP_ITER iter;

  if (Cr_list()) {
    BOOL sign_extd;
    FOR_ALL_NODE(cr, iter, Init(Cr_list())) {
      if (!cr->Is_flag_set(CF_MADEUP_TYPE))
	sign_extd = cr->Is_sign_extd();
    }

    FOR_ALL_NODE(cr, iter, Init(Cr_list())) {
      if (( !cr->Is_flag_set(CF_MADEUP_TYPE) && 
	   cr->Is_sign_extd() != sign_extd ) 
	  || 
	  ( cr->Is_flag_set(CF_DEF_BY_CHI) && 
	   (cr->Defstmt()->Op() == OPC_OPT_CHI || 
	    cr->Defstmt()->Op()==OPC_REGION )) )
	return TRUE;
    }
  }
  return FALSE;
  
}
    
#ifdef KEY
void OPT_STAB::Print_occ_tab(FILE *fp, WN *wn)
#endif
{
  /* WN_MAP_ITER has been removed because it does not work reliably for
     this sort of thing.  (The mapping may contain dangling pointers for
     WNs that have been deleted.)  If anyone needs this to work, it
     should be reimplemented to walk the WHIRL tree to find the map entries. */

  //fprintf(fp, "< Print_occ_tab not implemented >\n");
#ifdef KEY
  if (wn==NULL)
    return;
  OPERATOR opr = WN_operator( wn );
  if (opr == OPR_BLOCK) {
    for (WN *wn2 = WN_first(wn); wn2 != NULL; wn2 = WN_next (wn2) ) 
      Print_occ_tab(fp, wn2);
  } else {
    OCC_TAB_ENTRY *occ = Get_occ(wn);
    if (occ)
      occ->Print(fp);
      for ( INT32 i = 0; i < WN_kid_count( wn ); i++ )
        Print_occ_tab( fp, WN_kid( wn, i ));
  }
#endif
}


void
OPT_STAB::Print_top_nth_coderep(AUX_ID i, INT32 n, FILE *fp)
{
  Top_nth_coderep(i, n)->Print(0, fp);
}


// ====================================================================
// Print the flags in a ver_stab_entry
// ====================================================================


static void
VER_STAB_FLAG_Print( INT flag, BOOL verbose, FILE *fp=stderr )
{
  fprintf( fp, " flag:%02x ", flag );
  if ( verbose ) {
    if ( flag & VS_ANY_USE )
      fprintf( fp, " any_use " );
    if ( flag & VS_REAL_USE )
      fprintf( fp, " real_use " );
  }
}


//  Print the DU chain entry
//
void VER_STAB_ENTRY::Print(FILE *fp, VER_ID ver_id) const
{
  fprintf(fp, " vers %d: sym=%3d ver=%3d ", ver_id, Aux_id(), Version());
  VER_STAB_FLAG_Print( flags, FALSE, fp );

  switch (Type()) {
  case WHIRL_STMT:
    fprintf(fp, "Wn  *0x%p ", Wn());
    fprintf(fp, " use=");
    Print_use(Wn(), fp);
    break;
  case PHI_STMT:
    fprintf(fp, "Phi *0x%p ", Phi());
    fprintf(fp, " use=");
    Print_use(Phi(), Bb(), fp);
    if (!Phi()->Live()) 
      fprintf(fp, "^");
    else
      fprintf(fp, "ref_wn: 0x%p", Ref_wn());
    break;
  case CHI_STMT:
    fprintf(fp, "Chi *0x%p ", Chi());
    fprintf(fp, " use=%d", Chi()->Opnd());
    break;
  default:
    break;
  }
  fprintf(fp,"\n");
}


static inline BOOL
Overlap(INT64 offset1, INT64 size1,
	INT64 offset2, INT64 size2)
{
  return ((offset1 + size1 > offset2) &&
	  (offset2 + size2 > offset1));
}


// ====================================================================
// Determine which symbols correspond to memory that may be referenced
// in a nested PU. Coming from the front end, symbols are marked
// (has_nested_ref) according to whether they are referenced in a
// nested PU, but for us that isn't enough. If two symbols have the
// same base and offset, and if exactly one of them has a nested
// reference, and if only the other appears in the aux_stab, we want
// to make sure its aux_stab entry is marked as having a nested
// reference. This fixes bug 524653.
// ====================================================================


// Find_symtab_of should probably be in opt_util.cxx, but it's here
// because right now this is the only client.
static ST_TAB *
Find_symtab_of(const ST *st)
{
  return Scope_tab[ST_IDX_level(ST_st_idx(st))].st_tab;
}


struct NEST_REF_CAND {
  INT64  offset;
  INT64  size;
  AUX_ID aux_id;

  NEST_REF_CAND(void) { }

  NEST_REF_CAND(INT64  ofst,
		INT64  sz,
		AUX_ID id) : offset(ofst), size(sz), aux_id(id) { }
};


struct transfer_attributes_as_needed {
  BOOL                             tracing;
  OPT_STAB                        *opt_stab;
  vector<vector<NEST_REF_CAND> >  &nest_ref_cands;
  vector<const ST *>              &nested_ref_bases;

  transfer_attributes_as_needed(BOOL                            trace,
				OPT_STAB                       *stab,
				vector<vector<NEST_REF_CAND> > &cands,
				vector<const ST *>             &bases) :
    tracing(trace),
    opt_stab(stab),
    nest_ref_cands(cands),
    nested_ref_bases(bases) { }

  void operator()(UINT, ST *st) const
    {
      if (ST_class(st) == CLASS_VAR &&
	  ST_has_nested_ref(st)) {
	ST    *base;
	INT64  offset;
	Expand_ST_into_base_and_ofst(st, 0, &base, &offset);
	if (base != st) {
	  // See if anyone local to our PU cares about this base...
	  const ST **var_base_pos = &(*find(nested_ref_bases.begin(),
					 nested_ref_bases.end(), 
					 base));
	  if (var_base_pos != &(*nested_ref_bases.end())) {
	    // Someone in the PU referred to this base and might need a
	    // status update. See if any of the references to this base
	    // from the PU overlap with the current symbol.
	    INT var_base_index = (var_base_pos -
				  &(*nested_ref_bases.begin()));
	    const NEST_REF_CAND *first_nrc =
	      &(*nest_ref_cands[var_base_index].begin());
	    const NEST_REF_CAND *last_nrc =
	      &(*nest_ref_cands[var_base_index].end());
	    while (first_nrc != last_nrc) {
	      if (Overlap(offset, TY_size(ST_type(st)),
			  first_nrc->offset, first_nrc->size)) {
		// At last! Done with all that checking, and we
		// finally have something real to do!
		Is_Trace(tracing, (TFile, "Setting nested ref bit "
                                   "on aux_id %d because of\n",
				   first_nrc->aux_id));
		Is_Trace_cmd(tracing, Print_ST(TFile,
					       (ST *) *var_base_pos,
					       TRUE));
		opt_stab->Aux_stab_entry(first_nrc->aux_id)->
		  Set_has_nested_ref();
	      }
	      ++first_nrc;
	    }
	  }
	}
      }
    }
};


void
OPT_STAB::Collect_nested_ref_info(void)
{
  BOOL   tracing = Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG);

  AUX_ID var;
  AUX_STAB_ITER aux_stab_iter(this);

  vector <ST_TAB *>                     symtabs;

  vector<const ST *>              nested_ref_bases;

  // Indexed by the index of the base ST in nested_ref_bases:
  vector<vector<NEST_REF_CAND> >  nest_ref_cands;

  // First we go through the aux stab looking for things whose
  // nested_ref status might not be set appropriately. Such items are
  // identified by having base symbols with has_nested_ref, but
  // lacking the has_nested_ref attribute themselves. In the second
  // outer loop (below), we check each of these candidates to
  // determine whether each one should have the has_nested_ref
  // attribute or not. The point of this first loop is to minimize the
  // amount of work we have to do in looking through symtabs that
  // don't contain any symbol referenced in the PU we're compiling,
  // bases that aren't referenced by any symbol that appears in our
  // PU, etc. Our assumption is that the aux_stab will generally be
  // significantly smaller than the total size of all the visible
  // symtabs.
  FOR_ALL_NODE(var, aux_stab_iter, Init()) {
    ST *var_base;

    if (Aux_stab_entry(var)->Stype() == VT_UNKNOWN)
      continue;

    if (!Aux_stab_entry(var)->Has_nested_ref() &&
	(ST_class(var_base = Aux_stab_entry(var)->Base()) == CLASS_VAR) &&
	ST_has_nested_ref(var_base)) {
      Is_Trace(tracing, (TFile, "Found non-nested-ref symbol "
                         "with nested_ref base:\n"));
      Is_Trace_cmd(tracing, Print_ST(TFile, Aux_stab_entry(var)->St(), TRUE));
      Is_Trace_cmd(tracing, Print_ST(TFile, var_base, TRUE));

      /* Not const */ ST_TAB *my_symtab = Find_symtab_of(var_base);

      const ST **var_base_pos = &(*find(nested_ref_bases.begin(),
				     nested_ref_bases.end(),
				     var_base));
      INT var_base_index = var_base_pos - &(*nested_ref_bases.begin());

      if (var_base_pos ==
	  &(*nested_ref_bases.end())) {
	Is_True(nested_ref_bases.end() - nested_ref_bases.begin() ==
		var_base_index,
		("Robert misunderstood STL vector: %d vs. %d",
		 nested_ref_bases.end() - nested_ref_bases.begin(),
		 var_base_index));
	nested_ref_bases.push_back(var_base);
	// Make sure we construct the new entry in the parallel
	// vector...
	nest_ref_cands.push_back(vector<NEST_REF_CAND>());

	// Put this symtab in the list of places to check if it won't
	// be a duplicate.
	if (find(symtabs.begin(), symtabs.end(), my_symtab) ==
	    symtabs.end()) {
	  symtabs.push_back(my_symtab);
	}
      }

      NEST_REF_CAND  nrc;
      ST            *base;
      Expand_ST_into_base_and_ofst(Aux_stab_entry(var)->St(),
				   Aux_stab_entry(var)->St_ofst(),
				   &base, &nrc.offset);
      nrc.size = Aux_stab_entry(var)->Byte_size();
      nrc.aux_id = var;

      nest_ref_cands[var_base_index].push_back(nrc);
    }
  }

  /* Not const */ ST_TAB **symtab;

  for (symtab = &(*symtabs.begin());
       symtab != &(*symtabs.end());
       symtab++) {
    For_all_entries(**symtab,
		    transfer_attributes_as_needed(tracing, this,
						  nest_ref_cands,
						  nested_ref_bases),
		    1);
  }
}


void
VER_STAB_ENTRY::Print_use(WN *wn, FILE *fp) const
{
  OPERATOR opr = WN_operator(wn);

  if (OPERATOR_has_aux(opr) && OPERATOR_is_scalar_load (opr)) {
    fprintf(fp, "%d ", WN_ver(wn));
  }
  for (INT32 i = 0; i < WN_kid_count(wn); i++)
    Print_use(WN_kid(wn,i), fp); 
} 


void
VER_STAB_ENTRY::Print_use(PHI_NODE *phi, BB_NODE *bb, FILE *fp) const
{
  INT32 in_degree = bb->Pred()->Len();
  for (INT32 i = 0; i < in_degree; i++) {
    fprintf(fp, "%d ", phi->Opnd(i));
  }
} 


// go thru all its SSA versions to see if there is any whose value
// is a constant
BOOL
AUX_STAB_ENTRY::Has_def_by_const(void)
{
  CODEREP *cr;
  CODEREP_ITER cr_iter;
  STMTREP *stmt;
  FOR_ALL_NODE(cr, cr_iter, Init(Cr_list())) {
    if (cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI | CF_DEF_BY_CHI |
				  CF_IS_ZERO_VERSION)))
      continue;
    stmt = cr->Defstmt();
    Is_True(OPERATOR_is_scalar_store (stmt->Opr()),
	    ("AUX_STAB_ENTRY::Has_def_by_const: not defined by STID"));
    if (inCODEKIND(stmt->Rhs()->Kind(), CK_LDA|CK_RCONST|CK_CONST))
      return TRUE;
  }
  return FALSE;
}


// determine candidates for local RVI and live range shrinking; a symbol
// is a candidate for live range shrinking only if it is a candidate for
// local RVI and at least one of its def is by constant, in which case
// local RVI is skipped for it.
void COMP_UNIT::Do_local_rvi(void)
{
  AUX_ID i;
  OPT_STAB *opt_stab = Opt_stab();
  AUX_STAB_ITER aux_stab_iter(opt_stab);

  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *var = opt_stab->Aux_stab_entry(i);
    if (var->Is_local_rvi_candidate( opt_stab->Is_varargs_func() )) {
      if (var->Has_def_by_const()) {
	if (WOPT_Enable_Shrink)
          var->Set_lr_shrink_cand();
      }
      else {
	if (WOPT_Enable_Local_Rvi_Limit != -1 &&
	    i > WOPT_Enable_Local_Rvi_Limit) 
	  DevWarn("Do_local_rvi: skip aux_id > %d",
		  WOPT_Enable_Local_Rvi_Limit);
	else var->Change_to_new_preg(opt_stab,Htable());
      }
    }
  }
}


// this is called only if WOPT_Enable_Local_Rvi is false (i.e. Do_local_rvi
// is not called; this finds candidates for live range shrinking and set
// the lr_shrink_cand flag.  The purpose is so that live range shrinking will
// not be disabled when local rvi is turned off.
void COMP_UNIT::Find_lr_shrink_cand(void)
{
  if (! WOPT_Enable_Shrink)
    return;

  AUX_ID i;
  OPT_STAB *opt_stab = Opt_stab();
  AUX_STAB_ITER aux_stab_iter(opt_stab);

  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *var = opt_stab->Aux_stab_entry(i);
    if (var->Is_local_rvi_candidate( opt_stab->Is_varargs_func() )) {
      if (var->Has_def_by_const())
        var->Set_lr_shrink_cand();
    }
  }
}


BOOL OPT_STAB::Safe_to_speculate(AUX_ID id)
{
  return Aux_stab_entry(id)->Points_to()->Safe_to_speculate();
}

#ifdef KEY
// x is a 1- or 2-byte-sized symbol.  checks whether it is part of an existing
// 4- or 8-byte-sized symbol; if so, return that aux_id; otherwise, return 0
AUX_ID OPT_STAB::Part_of_reg_size_symbol(AUX_ID x)
{
  if (! WOPT_Enable_Subword_Opt)
    return 0;
  AUX_ID cur;
  if (aux_stab[x].St_group()) {
    for (cur = aux_stab[x].St_group(); 
	 cur != x; 
	 cur = aux_stab[cur].St_group()) {
      if (aux_stab[cur].Base() != aux_stab[x].Base())
	continue;
      if (aux_stab[cur].No_register() || aux_stab[cur].Has_nested_ref())
	continue;
      if (aux_stab[cur].Byte_size() != 4 && aux_stab[cur].Byte_size() != 8) 
	continue;
      if ((aux_stab[cur].Mclass() & MTYPE_CLASS_INTEGER) == 0)
	continue;
      if (ST_sclass(aux_stab[cur].St()) != SCLASS_REG &&
	  ST_sclass(aux_stab[cur].St()) != SCLASS_AUTO)
	continue;
      if (aux_stab[cur].Bit_size() != 0) // it is a bit-field
	continue;
      if (ST_is_temp_var(aux_stab[cur].St())) // no compiler-generated temps
	continue;
      if (aux_stab[cur].Is_volatile())
	continue;
      if (aux_stab[cur].Base_byte_ofst() <= aux_stab[x].Base_byte_ofst() &&
          (aux_stab[cur].Base_byte_ofst() + aux_stab[cur].Byte_size()) >= 
	  (aux_stab[x].Base_byte_ofst() + aux_stab[x].Byte_size()))
        break; 
    }
    if (cur != x)
      return cur;
  }
  return 0;
}
#endif

// ====================================================================
