/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
/* ====================================================================
 * Module: privatize_common.cxx
 * $Revision: 1.22 $
 * $Date: 06/02/01 15:18:44-08:00 $
 * $Author: gautam@jacinth.keyresearch $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.privatize_common.cxx $
 *
 * Revision history:
 *  11-12-97 : First created by Dave Kohr
 *
 * Description:
 * fixes bug involving COMMON/inlining/MP privatization
 *
 * Exported functions:
 * Rename_Privatized_COMMON()
 * ==================================================================== */

#include <alloca.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <stdio.h>

#include "privatize_common.h"

#include "config.h"     // for Debug_Level
#include "dwarf_DST.h"  // for DST_IDX and DST_INFO_IDX
#include "pu_info.h"    // for Current_PU_Info
#include "strtab.h"
#include "wn_mp.h"      // for Find_DST_From_ST() and Create_New_DST()
#include "wn_util.h"

#include "symtab.h"
#include "be_symtab.h"

  // deletes from priv_list all pragmas in old_prags set
RENAMING_SCOPE::~RENAMING_SCOPE()
{
  WN *old_priv_prag;
  BOOL present;

  HASH_TABLE_ITER<WN *, BOOL> it(&old_prags);

  while (it.Step(&old_priv_prag, &present))
    WN_DELETE_FromBlock(priv_list, old_priv_prag);
}


/***********************************************************************
 * Return TRUE if the given ST is a COMMON, FALSE otherwise.
 *
 * Must be F77/F90, either SCLASS_COMMON or SCLASS_DGLOBAL (for initialized)
 ***********************************************************************/

#ifdef TARG_LOONGSON
BOOL
#else
static BOOL
#endif 
ST_Is_Common_Block (ST *st)
{

    // N.B.: COMMON blocks are always in the global symtab
  PU& pu = Get_Current_PU();
  if (PU_ftn_lang(pu) &&
      (ST_sclass(st) == SCLASS_COMMON || ST_sclass(st) == SCLASS_DGLOBAL
#ifdef KEY
      // bug 4576
      || ST_sclass(st) == SCLASS_EXTERN
#endif
      ) &&
      TY_kind(ST_type(st)) == KIND_STRUCT)
    return TRUE;

  return FALSE;
}


/***********************************************************************
 * If st is a member of a COMMON block, return the ST that corresponds to
 * the COMMON block in the original source (as opposed to any compiler-
 * generated split COMMON blocks).  Otherwise, return NULL.  If the return
 * parameter split is not NULL, then *split is set to the split COMMON
 * block in which st appears (or NULL if st is not in a split COMMON).
 ***********************************************************************/

#ifdef KEY
// 'want_st' defaults to false, it needs to be true for C. If we end up
// making it true at all callsites, we should remove this parameter and
// fix this function accordingly.
ST *ST_Source_COMMON_Block(ST *st, ST **split, BOOL want_st)
#else
ST *ST_Source_COMMON_Block(ST *st, ST **split)
#endif // KEY
{
  Is_True(st, ("ST_Source_COMMON_Block(): NULL st argument"));

  if (split)
    *split = NULL;

  ST *base = ST_base(st);
#ifdef KEY
  if (want_st && base == st)
    return st;
#endif // KEY
  if (base == st || !ST_Is_Common_Block(base))
    return NULL;  // st is not in COMMON block

  if (!ST_is_split_common(base))
    return base;  // base is not a split-off COMMON

  ST *base_full = ST_full(base);
  Is_True(base != base_full,
          ("invalid base for split-off COMMON for symbol %s", ST_name(st)));
  Is_True(ST_Is_Common_Block(base_full),
          ("base of split-off COMMON for symbol %s is not a COMMON "
           "block", ST_name(st)));

    // base is split-off COMMON, so return base_full
  if (split)
    *split = base;

  return base_full;
}

#ifdef KEY
// Differs from ST_Source_COMMON_Block() in that the return value does not
// have to be COMMON, and it does not need to be the base of st.
//
// Returns the base of st if the base is threadprivate and COMMON. Returns
// st itself if the base is not threadprivate.
// For static threadprivate in C, the base is not threadprivate, so returns
// st itself in such cases.
ST *ST_Source_Block(ST *st, ST **split)
{
  Is_True(st, ("ST_Source_Block(): NULL st argument"));

  if (split)
    *split = NULL;

  ST *base = ST_base(st);
  if (base == st) return st;

  if (!ST_is_thread_private (base)) return st;

  if (!ST_Is_Common_Block(base))
    return NULL;  // st is not in COMMON block

  if (!ST_is_split_common(base))
    return base;  // base is not a split-off COMMON

  ST *base_full = ST_full(base);
  Is_True(base != base_full,
          ("invalid base for split-off COMMON for symbol %s", ST_name(st)));
  Is_True(ST_Is_Common_Block(base_full),
          ("base of split-off COMMON for symbol %s is not a COMMON "
           "block", ST_name(st)));

    // base is split-off COMMON, so return base_full
  if (split)
    *split = base;

  return base_full;
}
#endif

/***********************************************************************
 * Return TRUE if sclass is not that of a variable local to a PU (i.e. if
 * it's Fortran COMMON or C/C++ file static or global), FALSE otherwise.  
 ***********************************************************************/

static BOOL SCLASS_Is_Not_PU_Local(ST_SCLASS sclass)
{
  switch (sclass) {
  case SCLASS_FSTATIC:
  case SCLASS_COMMON:
  case SCLASS_EXTERN:
  case SCLASS_UGLOBAL:
  case SCLASS_DGLOBAL:
    return TRUE;

  case SCLASS_UNKNOWN:
    Fail_FmtAssertion("SCLASS_Is_Not_PU_Local() : got SCLASS_UNKNOWN");

  default:
    break;
  }

  return FALSE;
}
  

/***********************************************************************
 * Create a new symbol table entry for a symbol that has local scope and
 * stack allocation but otherwise has attributes identical to that given
 * by st.
 *
 * This code was lifted more or less as-is from Localize_Variable() in
 * be/com/wn_mp.cxx
 ***********************************************************************/

static ST *Create_Local_Symbol(ST *old_st)
{
  ST_SCLASS sclass = ST_sclass(old_st);
  Is_True(SCLASS_Is_Not_PU_Local(sclass),
          ("Create_Local_Symbol() called for ST with sclass %d",
           (INT) sclass ) );

  char *new_name, *old_name = ST_name(old_st);
  new_name = (char *) alloca(strlen(old_name) + 32);
  sprintf(new_name, "__localized_common_%s", old_name);
//  DevWarn("Create_Local_Symbol() making \"%s\"", new_name);

  TY_IDX old_ty_idx = ST_type(old_st), new_ty_idx = old_ty_idx;

  TY &old_ty =  Ty_Table[old_ty_idx];

  BOOL is_dynamic_array = TY_kind(old_ty) == KIND_POINTER &&
                          TY_kind(TY_pointed(old_ty)) == KIND_ARRAY &&
                          TY_size(TY_pointed(old_ty)) == 0;
    // we don't bother to compute is_static_array because  old_st is from
    // a COMMON rather than a formal parameter

  ST *new_st = New_ST(), &new_st_ref = *new_st, old_st_ref = *old_st;
  ST_Init(new_st, Save_Str(new_name), ST_class(old_st), SCLASS_AUTO,
          EXPORT_LOCAL, new_ty_idx);

  if (ST_addr_saved(old_st_ref))
    Set_ST_addr_saved(new_st_ref);
  if (ST_addr_passed(old_st_ref))
    Set_ST_addr_passed(new_st_ref);
  if (is_dynamic_array) {
    Set_ST_pt_to_unique_mem(new_st_ref);
    Set_ST_pt_to_compiler_generated_mem(new_st_ref);
  }

  if (Debug_Level > 0) {
      // create entry in DWARF Symbol Table (DST)
    DST_INFO_IDX dst_idx = Find_DST_From_ST(old_st, Current_PU_Info);
    if (!DST_IS_NULL(dst_idx))
      Create_New_DST(dst_idx, new_st, FALSE);
  }

  return new_st;
}


/***********************************************************************
 * If a PRIVATE clause of an MP region refers to a COMMON block variable,
 * create a new ST for the variable.  Rename to the new ST the PRIVATE
 * clause ST and all references to the old ST that appear in the MP region.
 * This fixes PV 437716, in which references to a COMMON block variable in
 * a routine inlined into an MP region are rewritten by the MP lowerer to
 * refer (incorrectly) to the privatized version of the variable, rather
 * than left alone to refer to the original COMMON block variable.
 *
 * For C/C++ programs, privatizing file static or global data causes the
 * same problem, so rename these classes of variables also.
 ***********************************************************************/

void
Rename_Privatized_COMMON(WN *wn, RENAMING_STACK *stack)
{
  Is_True(wn, ("Rename_Privatized_COMMON(): NULL wn argument"));

  ST *st = WN_has_sym(wn) ? WN_st(wn) : NULL;
  ST_SET *ignore = &stack->Top()->ignore;

  if (st && !ignore->Find(st)) {
    ST *new_st; // renamed version of st
    ST *split_block;
    ST *common_block = ST_Source_COMMON_Block(st, &split_block);

    BOOL add_to_ignore;

#ifdef KEY /* Bug 4435 */
    if (ST_class(st) == CLASS_NAME) {
#else
    if (ST_is_equivalenced(st) ||
	ST_class(st) == CLASS_NAME) {
#endif
        // always ignore st if it's EQUIVALENCEd to something or just a name
      add_to_ignore = TRUE;
    } else if (ST_is_thread_private(st) ||
               (split_block && ST_is_thread_private(split_block)) ||
               (common_block && ST_is_thread_private(common_block))) {
        // PV 596988: ignore st if it's THREADPRIVATE: if it's also
	// privatized, this is a user error caught by the OMP Prelowerer.
      add_to_ignore = TRUE;
    } else {
        // ignore st if it's neither COMMON nor defined outside the PU
      add_to_ignore = 
        ! (common_block || SCLASS_Is_Not_PU_Local(ST_sclass(st)));
    }

    if (!add_to_ignore) {
      RENAMING_SCOPE *scope = NULL;
      WN *old_priv_prag;

        // find innermost scope that privatizes ST or its common block
      for (INT i = 0; i < stack->Elements(); i++) {
        WN *priv_list = stack->Top_nth(i)->priv_list;

        for (old_priv_prag = priv_list ? WN_first(priv_list) : NULL;
	     old_priv_prag; old_priv_prag = WN_next(old_priv_prag))
            // problem isn't fixed for FIRSTPRIVATE or LASTPRIVATE
          if (WN_opcode(old_priv_prag) == OPC_PRAGMA &&
	      WN_pragma(old_priv_prag) == WN_PRAGMA_LOCAL &&
	      (WN_st(old_priv_prag) == st ||
	       WN_st(old_priv_prag) == common_block) ) {
            scope = stack->Top_nth(i);
            break;  // found a match
          }

        if (scope)
          break;  // found a match
      }

      if (scope) {
        new_st = scope->map.Find(st);
        if (!new_st) {
          new_st = Create_Local_Symbol(st);
          scope->map.Enter(st, new_st);
          WN *priv_pragma = WN_CreatePragma(WN_PRAGMA_LOCAL, new_st, 0, 0);
          WN_set_pragma_compiler_generated(priv_pragma);
          WN_INSERT_BlockLast(scope->priv_list, priv_pragma);
          scope->old_prags.Enter_If_Unique(old_priv_prag, TRUE);
        }
      } else
        add_to_ignore = TRUE; // st isn't privatized anywhere
    }

    if (!add_to_ignore)
      WN_st_idx(wn) = ST_st_idx(new_st);
    else
      ignore->Enter(st, TRUE);
  }

  WN *priv_pragma_block = NULL;
  OPCODE opc = WN_opcode(wn);

    // see if wn is a region that privatizes any variables
  if (opc == OPC_REGION) {
    WN *priv_prag = WN_first(WN_region_pragmas(wn));

    while (priv_prag &&
           (WN_opcode(priv_prag) != OPC_PRAGMA ||
            WN_pragma(priv_prag) != WN_PRAGMA_LOCAL) )
      priv_prag = WN_next(priv_prag);

    if (priv_prag) {
      priv_pragma_block = WN_region_pragmas(wn);
      MEM_POOL *pool = stack->Top()->pool;
      stack->Push(CXX_NEW(RENAMING_SCOPE(priv_pragma_block, pool), pool));
    }
  }

    // recursively process all children except priv_pragma_block
  if (!OPCODE_is_leaf(opc)) {
    if (opc == OPC_BLOCK)
      for (WN *kid = WN_first(wn); kid; kid = WN_next(kid))
        Rename_Privatized_COMMON(kid, stack);
    else {
      for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
        WN *kid = WN_kid(wn, kidno);

        if (kid && kid != priv_pragma_block)
          Rename_Privatized_COMMON(kid, stack);
      }
    }
  }

  if (priv_pragma_block) {
    // return to next-outermost scope, delete PRIVATE
    // pragmas that refer to COMMON variables
    CXX_DELETE(stack->Pop(), stack->Top()->pool);
  }
}
#ifdef KEY
/***********************************************************************
 * Create a new symbol table entry for a symbol that has local scope and 
 * pointer type for the old symbol
 *
 ***********************************************************************/

static ST *Create_Local_Threadprivate_Symbol(ST *old_st)
{
  ST_SCLASS sclass = ST_sclass(old_st);
  Is_True(sclass != SCLASS_AUTO,
          ("Create_Local_Symbol() called for ST with sclass %d",
           (INT) sclass ) );

  char *new_name, *old_name = ST_name(old_st);
  new_name = (char *) alloca(strlen(old_name) + 32);
  sprintf(new_name, "__ppthd_common_%s", old_name);

  TY_IDX old_ty_idx, new_ty_idx;

  old_ty_idx = ST_type(old_st);
  new_ty_idx = Make_Pointer_Type(old_ty_idx);

  ST *new_ppthd_st = New_ST(CURRENT_SYMTAB);

  ST_Init(new_ppthd_st, Save_Str(new_name), ST_class(old_st), SCLASS_AUTO,
          EXPORT_LOCAL, new_ty_idx);
  //Set_ST_is_thread_private(new_ppthd_st);

  if (Debug_Level > 0) {
      // create entry in DWARF Symbol Table (DST)
    DST_INFO_IDX dst_idx = Find_DST_From_ST(old_st, Current_PU_Info);
    if (!DST_IS_NULL(dst_idx))
      Create_New_DST(dst_idx, new_ppthd_st, FALSE);
  }

  return new_ppthd_st;
}

/***********************************************************************
 * Create a new symbol table entry for a symbol that has global scope and 
 * pointer type for the old symbol
 *
 ***********************************************************************/
static ST *Create_Global_Threadprivate_Symbol(ST *old_st)
{
  char *new_name, *old_name = ST_name(old_st);
  new_name = (char *) alloca(strlen(old_name) + 32);
  sprintf(new_name, "__thdprv_common_%s", old_name);

  ST *new_thdprv_st = New_ST(GLOBAL_SYMTAB); 

  ST_SCLASS sclass = ((ST_sclass (old_st) == SCLASS_FSTATIC
		      || ST_sclass (old_st) == SCLASS_PSTATIC) ?
		      SCLASS_FSTATIC : SCLASS_COMMON);

  TY_IDX old_ty_idx, new_ty_idx;

  old_ty_idx = ST_type(old_st);
  new_ty_idx = Make_Pointer_Type(Make_Pointer_Type(old_ty_idx));
  ST_Init(new_thdprv_st, Save_Str(new_name), ST_class(old_st), sclass,
          ST_export(old_st), new_ty_idx);

  if (Debug_Level > 0) {
    DST_INFO_IDX dst_idx = Find_DST_From_ST(old_st, Current_PU_Info);
    if (!DST_IS_NULL(dst_idx))
      Create_New_DST(dst_idx, new_thdprv_st, FALSE);
  }

  return new_thdprv_st;
}

/***********************************************************************
 * If a COMMON block variable is with threadprivate attribute, 
 * create a new ST for the variable, where the new ST is of pointer type
 * Replace all references to the old ST appearing in the MP region with 
 * the deference of the new ST. 
 *
 ***********************************************************************/
void
Rename_Threadprivate_COMMON(WN* pu, WN* parent, WN *wn, RENAMING_STACK *stack, RENAMING_SCOPE *scope, RENAMING_SCOPE *common_blk_scope)
{

  Is_True(wn, ("Rename_Threadprivate_COMMON(): NULL wn argument"));

  ST *st = WN_has_sym(wn) ? WN_st(wn) : NULL;

  if (st) {
    ST *split_block;
    ST *common_block = ST_Source_Block(st, &split_block);

    // Note that symbols in blocks are now marked to be thread private
    // since it is possible that some symbols can be referenced as thread
    // private and some as global.
    if (ST_is_thread_private(st)) {
      ST *new_st; // renamed version of st
      ST *new_st_for_common_blk;

      new_st_for_common_blk = common_blk_scope->map.Find(common_block);
      if (!new_st_for_common_blk) {
        new_st_for_common_blk = Create_Global_Threadprivate_Symbol(common_block);
        common_blk_scope->map.Enter(common_block, new_st_for_common_blk);
	// Keep track of local symbols, whose mappings need to be removed before
	// processing the next PU.
	if (ST_IDX_level(ST_st_idx(common_block)) != GLOBAL_SYMTAB)
	  common_blk_scope->local_mappings.push_front(common_block);
      }

      new_st = scope->map.Find(common_block);
      if (!new_st) {
        new_st = Create_Local_Threadprivate_Symbol(common_block);
        scope->map.Enter(common_block, new_st);
        WN *priv_pragma = WN_CreatePragma(WN_PRAGMA_THREADPRIVATE, common_block, ST_st_idx(new_st), ST_st_idx(new_st_for_common_blk));
        WN_set_pragma_compiler_generated(priv_pragma);
        WN_INSERT_BlockLast(WN_func_pragmas(pu), priv_pragma);
      }

      WN *priv_list;

      if (stack->Elements() > 1)
        priv_list = stack->Top_nth(0)->priv_list;
      else
        priv_list = WN_func_pragmas(pu);

      // Add local pragma to function pragmas also, so do a second
      // iteration if not done in the first pass
      for (INT i=0; i<2; i++)
      {
        BOOL match = FALSE;
        WN *old_priv_prag;
        for (old_priv_prag = priv_list ? WN_first(priv_list) : NULL;
             old_priv_prag; old_priv_prag = WN_next(old_priv_prag))
          // problem isn't fixed for FIRSTPRIVATE or LASTPRIVATE
          if (WN_opcode(old_priv_prag) == OPC_PRAGMA &&
              WN_pragma(old_priv_prag) == WN_PRAGMA_LOCAL &&
              WN_st(old_priv_prag) == new_st) {
            match = TRUE;
            break;  // found a match
          }

        if (match == FALSE && priv_list) {
          WN *priv_pragma = WN_CreatePragma(WN_PRAGMA_LOCAL, new_st, 0, 0);
          WN_set_pragma_compiler_generated(priv_pragma);
          WN_INSERT_BlockLast(priv_list, priv_pragma);
          match = TRUE;
        }
        if (priv_list == WN_func_pragmas (pu))
          break;
        else
          priv_list = WN_func_pragmas (pu);
      }

      WN *new_wn = NULL;
      // Use ST_ofst only if we are generating a pointer to the threadprivate
      // base of the st, instead of to the st itself.
      WN_OFFSET ofst = (st == common_block) ? 0 : ST_ofst (st);
      if (WN_operator(wn) == OPR_LDA) {
        new_wn = WN_CreateLdid(
                 OPCODE_make_op(OPR_LDID, 
                                TY_mtype(ST_type(new_st)), 
                                TY_mtype(ST_type(new_st))),
                 0, new_st, ST_type(new_st));
	ofst += WN_load_offset (wn);
	if (ofst)
	  new_wn = WN_Add(Pointer_type, new_wn,
	    		  WN_Intconst(Pointer_type, ofst));
        for (INT kidno = 0; kidno < WN_kid_count(parent); kidno++) 
          if (WN_kid(parent, kidno) == wn)
            WN_kid(parent, kidno) = new_wn;
      }
      else if (WN_operator(wn) == OPR_LDID){
        ofst += WN_load_offset (wn);
        new_wn = WN_CreateIload(
                   OPCODE_make_op(OPR_ILOAD,
                                  WN_rtype(wn), 
                                  WN_desc(wn)),
                   ofst,
                   ST_type(st),
                   Make_Pointer_Type(ST_type(st),FALSE),
                   WN_CreateLdid(
                     OPCODE_make_op(OPR_LDID,
                                    TY_mtype(ST_type(new_st)),
                                    TY_mtype(ST_type(new_st))),
                     0, new_st, ST_type(new_st)));
        for (INT kidno = 0; kidno < WN_kid_count(parent); kidno++) 
          if (WN_kid(parent, kidno) == wn)
            WN_kid(parent, kidno) = new_wn;
      }
      else if (WN_operator(wn) == OPR_LDBITS)
        Fail_FmtAssertion("SCLASS_Is_Not_PU_Local() : got SCLASS_UNKNOWN");
      else if (WN_operator(wn) == OPR_STID){
        ofst += WN_store_offset (wn);
        new_wn = WN_CreateIstore(
                   OPCODE_make_op(OPR_ISTORE,WN_rtype(wn),WN_desc(wn)),
                   ofst,
                   Make_Pointer_Type(ST_type(st),FALSE),
                   WN_COPY_Tree(WN_kid(wn,0)),
                   WN_CreateLdid(
                     OPCODE_make_op(OPR_LDID,
                                    TY_mtype(ST_type(new_st)),
                                    TY_mtype(ST_type(new_st))),
                     0, new_st, ST_type(new_st)));
	if (WN_operator(parent) == OPR_BLOCK)
	{
          WN_INSERT_BlockBefore(parent, wn, new_wn);
          WN_DELETE_FromBlock(parent, wn);
	}
	else
	{
          for (INT kidno = 0; kidno < WN_kid_count(parent); kidno++) 
            if (WN_kid(parent, kidno) == wn)
              WN_kid(parent, kidno) = new_wn;
	}
        wn = new_wn;
      }
      else if (WN_operator(wn) == OPR_STBITS)
        Fail_FmtAssertion("SCLASS_Is_Not_PU_Local() : got SCLASS_UNKNOWN");
#ifdef KEY
      // Bug 9084: Handle the case where a threadprivate var is marked with
      // another pragma, e.g., copyprivate.
      else if (WN_operator(wn) == OPR_PRAGMA &&
               WN_pragma(wn) == WN_PRAGMA_COPYPRIVATE)
        WN_st_idx (wn) = ST_st_idx (new_st);
#endif
         
    }
  }

  WN *priv_pragma_block = NULL;
  OPCODE opc = WN_opcode(wn);

  if (opc == OPC_REGION) {
    WN *priv_prag = WN_first(WN_region_pragmas(wn));

    if (priv_prag && WN_pragma(priv_prag) != WN_PRAGMA_SINGLE_PROCESS_BEGIN && WN_pragma(priv_prag) != WN_PRAGMA_MASTER_BEGIN) {
      priv_pragma_block = WN_region_pragmas(wn);
      MEM_POOL *pool = stack->Top()->pool;
      stack->Push(CXX_NEW(RENAMING_SCOPE(priv_pragma_block, pool), pool));
    }
  }


  // recursively process all children
  if (!OPCODE_is_leaf(opc)) {
    if (opc == OPC_BLOCK)
      for (WN *kid = WN_first(wn); kid; ){
        WN *old_kid = WN_next(kid);
        Rename_Threadprivate_COMMON(pu, wn, kid, stack, scope, common_blk_scope);
        kid = old_kid;
      }
    else {
      for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
        WN *kid = WN_kid(wn, kidno);
        Rename_Threadprivate_COMMON(pu, wn, kid, stack, scope, common_blk_scope);
      }
    }
  }
  if (priv_pragma_block) 
    CXX_DELETE(stack->Pop(), stack->Top()->pool);
}
#endif
