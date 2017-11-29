/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/*
 * Comment:
 *  - overall data structures: dinfo, what they contain.
 *  - da_hash
 *  - when are darts created, when not
 *  - scope of jennifer/pregs
 *  - storage allocation for redistributed variables (locals, globals,
 *      partial-split and full-split commons)
 *
 * Jennifer Pregs:
 *  - for reshaped variables their values never change, and both array lowering
 *    and affinity scheduling can safely use them.
 *  - for non-reshaped variables these variables are used only while doing
 *    affinity scheduling. If distribution is non-dynamic, then we can use
 *    them, otherwise we must get their latest values from the dart in the
 *    dynamic lookup version of the parallel loop (in the static version
 *    we can use the variables, and presume that their values didn't change)
 *  - ??? explain their allocation ???
 *  - these variables could become inconsistent with the dart in the following
 *    scenario:
 *      foo () {                bar (A) {
 *          local A                 redistribute A // change local pregs
 *          distribute A        }
 *          dynamic A
 *          bar (A)
 *          affinity (A) // although dart has changed, pregs haven't
 *      }
 *    however, we will still get correct execution, since affinity(A) will
 *    use the pregs ONLY if the array didn't get redistributed, based on 
 *    a DYNAMIC check. If the distribution did change, then we do a dynamic
 *    lookup of the dart in the runtime HT, and we're OK.
 *  
 *
 * Alias analysis
 * --------------
 * DART: 
 *   dart-defs: 
 *      - allocate a dart, lookup a dart
 *   dart-uses:
 *      - calls to runtime routines (alloc-reshape, init-dart, init-pregs)
 *   dart-ptr-defs:
 *      - initialize values in dart
 *   dart-ptr-uses:
 *      - initialize pregs
 * We use two aliases for darts: 
 *      - Create_local_alias for the dart itself,
 *      - Create_unique_pointer_alias for locations dart points to.
 * The dinfo stores a WN for each of these aliases.
 * In addition, we also store all the possible "defs" of a dart in 
 * the dinfo structure (from Gen_Alloc_DART and Gen_Lookup_DART). Whenever
 * we generate a use of the dart, we add a DU-chain from all the defs to
 * the use.
 *
 * Array:
 * 
 *
 * Allocation of locals, globals, partial-split commons, full-split commons.
 * -------------------------------------------------------------------------
 * This is an issue only for reshaped arrays.
 * There are two issues:
 *  - updating the ST/TY entries, and 
 *  - allocating space
 * (1) The function Reshape_ST_Entry does the first. It basically mangles the
 *  type of the ST entry to be a pointer type. Also, if the variable being
 *  reshaped was in a common block (either partial or full split), then it
 *  does the following. If the variable was in a partial split, then all
 *  the other ST entries that are sharing the same base have their offsets
 *  updated based on the new size of this reshaped array (we walk all the STs
 *  in the symtab that have the same base).  For full splits we only update
 *  the ST entry for the reshaped array (currently there is no easy way to
 *  locate the other STs from the same common block). However, this should
 *  still be OK, since reshaping is legal only if it is consistent across
 *  multiple files etc. If the linker finds an inconsistency and tries to
 *  patch things back, then it should check if any ST was reshaped. If so,
 *  it should just flag an error. If not, I'm hoping that just changing
 *  that one entry will enable the linker to handle the consistent cases
 *  correctly.
 *  If the full-split scheme has problems, we'll need to find the other STs
 *  in the common block, and update their "offsets-within-the-name".
 *  - We cannot muck-up the ST entries for non-reshaped variables that are
 *    within the same common block, since that screws up the alias info
 *    for those variables for the rest of LNO. So we save up the mods to
 *    all the non-reshaped ST entries, and do them at the end of LNO.
 *    This must be done for all partial and full split common blocks.
 *    Post LNO the alias information is recomputed anyway.
 * 
 *
 * (2) The second part is allocating space. Space is allocated both before and
 *  after LNO. Before LNO is from Finalize_Static_Layout, which allocates
 *  all symbols in the global symtab. The interesting things in the 
 *  global symtab are C global arrays, and if IPA has been run then perhaps
 *  partial/full-split commons from Fortran. F_S_L has been modified to 
 *  not call Allocate_Object if the ST entry is reshaped -- so LNO is 
 *  responsible for calling Allocate_Object for these entries (reshaped STs
 *  in the global symtab).  
 *      - for globals LNO just calls Allocate_Object on the ST
 *      - for partial split commons LNO calls Allocate_Object on the
 *        ST_base(st). This may cause A_O to be called twice on the same
 *        ST_base, since other vars in the same common may not be reshaped
 *        and F_S_L may call A_O on them. But this is not a problem, since
 *        A_O just marks space for ld to allocate, and multiple calls don't
 *        hurt.
 *      - for full split commons LNO calls Allocate_Object on just the one
 *        reshaped ST entry. This will allocate the ST_base, and should
 *        not be a problem either - same as for partial split.
 *  In any case, if IPA had run and we find STs from common blocks in the 
 *  global symtab, then presumably they were found to be consistent. 
 *
 *  STs that are not in the global symtab are allocated after LNO, and so
 *  long as LNO mangles the type, they should be allocated properly.
 *
 * 
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdarg.h>
#include <alloca.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "lego_gen.h"
#include "lego_util.h"
#include "wn_pragmas.h"
#include "cxx_memory.h"
#include "lwn_util.h"
#include "lego.h"
#include "strtab.h"
#include "config_targ.h"
#include "const.h"
#include "dra_demangle.h"
#include "lnoutils.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "irbdata.h"
#include "data_layout.h"
#include "lnopt_main.h"
#include "errors.h"
#include "erbe.h"

/***********************************************************************
 *
 * External declarations 
 *
 ************************************************************************/
extern void Lower_Distr_Pragmas (WN* func_nd);
extern DU_MANAGER *Du_Mgr;
extern class ALIAS_MANAGER *Alias_Mgr;
extern ST *Find_Return_Registers(TYPE_ID type,PREG_NUM *rreg1,PREG_NUM *rreg2);
extern WN *AWN_LdidSym(SYMBOL *var);
extern WN *AWN_StidIntoSym(SYMBOL *var, WN *val);
extern WN *Create_Positive_Divceil(TYPE_ID type, WN *kid0, WN *kid1,
                                   BOOL can_speculate);
extern WN *AWN_Binary(OPERATOR opr, TYPE_ID rtype, WN *kid0, WN *kid1);
extern WN* Create_Array_Store(ST* st_array,
                              TYPE_ID mtype,   
                              INT index,   
                              INT element_size,   
                              INT element_count, 
                              WN* wn_value);

extern BOOL  Run_Dsm_Check;
extern WN* Find_SCF_Inside(WN* parent_wn, OPCODE opc);


/***********************************************************************
 *
 * Extern stuff declared here.
 *
 ************************************************************************/

extern ST* distr_st_entries[DST_MAX];
extern TY_IDX distr_ty_entries[DTY_MAX];
ST* distr_st_entries[DST_MAX];
TY_IDX distr_ty_entries[DTY_MAX];

/***********************************************************************
 *
 * Local definitions
 *
 ************************************************************************/
typedef STACK<WN *> STACK_OF_WN;
static WN* exit_wn = NULL;
static STACK_OF_WN *altentry_stack = NULL;

static void Lower_Distr_Array                 (DISTR_INFO* dinfo);
static void Lower_Pragma_Distribute           (DISTR_ARRAY* dact);
static void Lower_Pragma_Redistribute         (DISTR_ARRAY* dact);
static void Lower_Pragma_Distribute_Reshape   (DISTR_ARRAY* dact);

static WN* Process_Alt_Entries_Local  (DISTR_ARRAY* dact, WN* prev_wn);
static WN* Process_Alt_Entries_Formal (DISTR_ARRAY* dact, WN* prev_wn);
static WN* Gen_Symbols_In_DINFO (DISTR_ARRAY* dact, WN* prev_wn);
static WN* Gen_Init_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* st = NULL);
static WN* Gen_Migrate_Array (DISTR_INFO* dinfo, BOOL do_exit, WN *prev_wn);
static WN* Gen_HT_DART (DISTR_INFO* dinfo, BOOL do_exit, BOOL is_redistr,
                        WN* prev_wn);
static void Process_Global_Distribute (DISTR_ARRAY* dact);
static WN* Gen_Alloc_Reshape (DISTR_ARRAY* dact, BOOL do_exit, WN* prev_wn);

static WN* Gen_EC_Reshaped_Array(DISTR_ARRAY *dact, WN* prev_wn);
static WN* Gen_Compare_DARTS (WN* dart_ldid, WN* ec_dart_ldid, WN* prev_wn,
                              char* array_name);
static WN* Gen_Dealloc_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* st = NULL);

static ST* Create_Local_ST (ST* dart_st);
static void Insert_Exit_Code (STACK_OF_WN* exit_stack);
static void Reshape_ST_Entry (DISTR_INFO* dinfo);
static WN* Gen_CheckNo_DART (DISTR_ARRAY* dact, WN* prev_wn);
static WN* Gen_Lookup_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* func_st);
static WN* Gen_Alloc_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* st = NULL);
static WN* Gen_Call_Array_Dart_Args(DISTR_INFO* dinfo, ST *func_st);
static WN* Gen_Call_Array_Arg(DISTR_INFO* dinfo, ST *func_st);
// static WN* Read_Gen_Pragma_Redistribute (wn);

static WN* Get_Array_Dimension_LB (TY_IDX array_ty, INT i);

#define fl_offset_sz     0
#define fl_offset_chunk  4
#define fl_offset_flist  8



/***********************************************************************
 *
 * Generate code for data distribution pragmas.
 * The pragmas are available in da_hash (both distribute and redistribute)
 *
 ***********************************************************************/
extern void Lower_Distr_Pragmas (WN* func_nd) {
  WN* wn = func_nd;

  // Initialize exit_wn to an empty block node
  exit_wn = WN_CreateBlock ();
  LWN_Set_Parent(exit_wn, NULL);

  // Create stack to collect the exit points in the PU
  STACK_OF_WN *exit_stack = CXX_NEW(STACK_OF_WN(LEGO_pool), LEGO_pool);

  // Create stack to collect the alt-entries in the PU
  if (PU_has_altentry(Get_Current_PU())) 
    altentry_stack = CXX_NEW (STACK_OF_WN(LEGO_pool), LEGO_pool);

  // Walk the tree, finding exits, and actual redistributes
  // also collect altentries, if any
  while (wn) {
    if (WN_operator(wn) == OPR_RETURN
#ifdef KEY
  	|| WN_operator(wn) == OPR_GOTO_OUTER_BLOCK
#endif
       ) {
      exit_stack->Push(wn);
      wn = LWN_Get_Next_Stmt_Node (wn);
    } else if ((WN_operator(wn) == OPR_PRAGMA) &&
               (WN_pragma(wn) == WN_PRAGMA_REDISTRIBUTE)) {
      /* it is a redistribute -- collect it */
      wn = Read_Pragma_Redistribute (wn, TRUE);
    } else if (WN_operator(wn) == OPR_ALTENTRY) {
      altentry_stack->Push (wn);
      wn = LWN_Get_Next_Stmt_Node (wn);
    }
    else wn = LWN_Get_Next_Stmt_Node (wn);
  }

  // Walk the list of distributes and redistributes
  /* Walk the stack first to last because we must 
   * ensure that the exit code comes out in reverse order 
   * compared to entry code, in particular the allocation/deallocation
   * of reshaped array. This is because we follow a stack discipline
   * in the runtime library.
   * We generate exit code at the head of exit_wn, so walking
   * first to last ensures that the code for the last pragma
   * is at the head of exit_wn.
   */
  BOOL local_reshapes = FALSE;
  for (INT i=0; i<da_stack->Elements(); i++) {
    DISTR_INFO* dinfo = da_stack->Bottom_nth(i);
    Lower_Distr_Array (dinfo);
    if (dinfo->IsReshaped() && (ST_Var_Kind(dinfo->Array_ST()) == var_local))
      local_reshapes = TRUE;
  }

  /* If local arrays are being allocated, and PU has return statement
   * (i.e. it is not "main", then push/pop upon entry/exit.
   */
  if (local_reshapes && exit_stack->Elements()) {
    /* Generate a call to Proc_Pool_Push before the first distribute pragma */
    OPCODE callop = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);
    WN* call_wn = WN_Create (callop, 0);
    WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Proc_Pool_Push]);
    Set_Runtime_Call_Side_Effects (call_wn);
    LWN_Insert_Block_Before
      (NULL,
       da_stack->Bottom_nth(0)->Get_Dact(0)-> First_Pragma_WN(),
       call_wn);

    // if altentries, do the same for each entry
    if (altentry_stack) {
      for (INT i=0; i<altentry_stack->Elements(); i++) {
        wn = altentry_stack->Bottom_nth(i);
        while (wn) {
          if (WN_opcode(wn) == OPC_PRAGMA &&
              WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END)
            break;
          wn = WN_next(wn);
        }
        LWN_Insert_Block_After (NULL, wn, LWN_Copy_Tree(call_wn));
      }
    }

    /* and a call to Proc_Pool_Pop at the end of the exit block */
    call_wn = WN_Create (callop, 0);
    WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Proc_Pool_Pop]);
    Set_Runtime_Call_Side_Effects (call_wn);
    LWN_Insert_Block_Before (exit_wn, NULL, call_wn);
  }

  Insert_Exit_Code (exit_stack);
  if (altentry_stack) {
    CXX_DELETE (altentry_stack, LEGO_pool);
    altentry_stack = NULL;
  }
  CXX_DELETE(exit_stack, LEGO_pool);
} /* Lower_Distr_Pragmas */

static void Lower_Distr_Array (DISTR_INFO* dinfo) {
  INT i;
  for (i=0; i<dinfo->Num_Dact(); i++) {
    DISTR_ARRAY* dact = dinfo->Get_Dact(i);
    if (dinfo->IsReshaped()) Lower_Pragma_Distribute_Reshape (dact);
    else Lower_Pragma_Distribute (dact);
  }
  for (i=0; i<dinfo->Num_Gen_Redistr(); i++) {
    DISTR_ARRAY* dact = dinfo->Get_Gen_Redistr(i);
    Lower_Pragma_Redistribute (dact);
  }
}

/***********************************************************************
 *
 * Lower a distribute pragma. This involves the following steps:
 * 
 * Globals: 
 *     Write out distribution to object file
 *        -- this step also fills in DACT with ST entries for the
 *           global DART, numprocs and dimsize variables
 * Formals:    
 *     Lookup the DART in the hash table (call HT_Top)
 *     Allocate and initialize numprocs,dimsize pregs
 *
 * Locals:
 *     Allocate and initialize DART
 *     Allocate and initialize numprocs,dimsize pregs
 *     Push DART into hash table (call HT_Push)
 *     Generate call to Migrate_Array
 *     Pop DART from hash table at end of PU (call HT_Pop)
 *     Deallocate DART (call __dsm_Deallocate_Dart)
 *
 ***********************************************************************/
static void Lower_Pragma_Distribute (DISTR_ARRAY* dact) {

  DISTR_INFO *dinfo = dact->Dinfo();

  ST* array_st = dinfo->Array_ST();
  WN* pwn = dact->Last_Pragma_WN ();

  TY_IDX array_ty = Lego_Get_Array_Type(array_st);

  if (TY_kind(array_ty) != KIND_ARRAY) {
    printf ("Pragma Distribute on a non-array\n");
    return;
  }

  WN *prev_wn = NULL;

  switch (ST_Var_Kind(array_st)) {
  case var_global:
  case var_common:
    Process_Global_Distribute (dact);
    break;
  case var_formal:
    prev_wn = pwn;      /* pwn now points to the last pragma node,
                         * so the generated code can now follow this
                         * last pragma node.
                         */
    if (altentry_stack) prev_wn = Process_Alt_Entries_Formal (dact, prev_wn);
    else {
      /* generate a call to HT_Top and assign
       * return value into local DART pointer.
       */
      prev_wn = Gen_Lookup_DART (dact, prev_wn, distr_st_entries[HT_Check]);
      Is_True (!dinfo->IsReshaped(),
               ("Lower_Pragma_Distribute on a reshaped array\n"));
      prev_wn = Gen_CheckNo_DART (dact, prev_wn);
      prev_wn = Gen_Symbols_In_DINFO (dact,prev_wn);
    }
    break;
  case var_local: 
    prev_wn = pwn;      /* pwn now points to the last pragma node,
                         * so the generated code can now follow this
                         * last pragma node.
                         */
    if (altentry_stack) prev_wn = Process_Alt_Entries_Local (dact, prev_wn);
    else {
      prev_wn = Gen_Alloc_DART (dact, prev_wn); // allocate dart
      prev_wn = Gen_Init_DART  (dact, prev_wn);
      prev_wn = Gen_Symbols_In_DINFO (dact, prev_wn);
      if (TY_kind(array_ty) == KIND_POINTER) {
        // adjustable-size local array.
        // locate the alloca for DU-info.
        dinfo->Find_Alloca ();
      }
      prev_wn = Gen_Migrate_Array (dinfo, TRUE, prev_wn);
      prev_wn = Gen_HT_DART(dinfo, TRUE, FALSE, prev_wn);
    }
    break;
  default:
    FmtAssert (FALSE, ("Weird variable kind %s\n", ST_name(array_st)));
    break;
  }

  return;
} /* Lower_Pragma_Distribute */

/***********************************************************************
 *
 * Lower a distribute-reshape pragma. This involves the following steps:
 * 
 * Globals: 
 *     Write out distribution to object file
 *        -- this step also fills in DACT with ST entries for the
 *           global DART, numprocs and dimsize variables
 * Formals:    
 *     Lookup the DART in the hash table (call HT_Top)
 *     Allocate and initialize numprocs,dimsize pregs
 *
 * Locals:
 *     Allocate and initialize DART
 *     Allocate and initialize numprocs,dimsize pregs
 *     Update ST entry for the array to its new type 
 *     Allocate reshaped array (call __dsm_Alloc_Reshaped_Array)
 *     Deallocate reshaped array at end of PU 
 *         (call __dsm_Dealloc_Reshaped_Array)
 *     Push DART into hash table (call HT_Push)
 *     Pop DART from hash table at end of PU (call HT_Pop)
 *     Deallocate DART (call __dsm_Deallocate_Dart)
 *
 * Note: Since we currently do not allow reshaped arrays to be redistributed,
 *    it is not strictly necessary to Push/Pop the DARTs because the
 *    distribution will always be known statically.  We Push/Pop the
 *    DARTs anyway for now in case we allow redistributed reshapes later,
 *    and for consistency with plain distribute pragmas.
 *
 ***********************************************************************/
static void Lower_Pragma_Distribute_Reshape (DISTR_ARRAY* dact) {
  DISTR_INFO *dinfo = dact->Dinfo();

  ST* array_st = dinfo->Array_ST();
  TY_IDX array_ty = Lego_Get_Array_Type(array_st);
  WN* pwn = dact->Last_Pragma_WN ();

  if (TY_kind(array_ty) != KIND_ARRAY) {
    FmtAssert (FALSE, ("Pragma Distribute-Reshape on a non-array (%s)\n",
                       ST_name(array_st)));
    return;
  }

  FmtAssert (ST_class(array_st) == CLASS_VAR,
             ("Distributed array is not a variable"));

  FmtAssert (TY_AR_ndims(array_ty) > 0, ("Array with zero dimensions?\n"));

  if (ST_is_initialized(array_st)) {
    /* cannot handle initialized ST for now */
    DevWarn ("Cannot reshaped initialized data (%s) for now, ignoring",
             ST_name(array_st));
    return;
  }

  WN *prev_wn = NULL;

  switch (ST_Var_Kind(array_st)) {
  case var_global:
  case var_common:
    Process_Global_Distribute (dact);
    break;
  case var_formal:
    prev_wn = pwn;
    if (altentry_stack) prev_wn = Process_Alt_Entries_Formal (dact, prev_wn);
    else {
      prev_wn = Gen_Lookup_DART (dact, prev_wn, distr_st_entries[HT_Top]);
      if (Run_Dsm_Check) {
        prev_wn = Gen_EC_Reshaped_Array(dact, prev_wn);
      }
      prev_wn = Gen_Symbols_In_DINFO (dact, prev_wn);
      Reshape_ST_Entry (dinfo);
    }
    break;
  case var_local:
    prev_wn = pwn;
    if (altentry_stack) prev_wn = Process_Alt_Entries_Local (dact, prev_wn);
    else {
      prev_wn = Gen_Alloc_DART (dact, prev_wn);
      prev_wn = Gen_Init_DART  (dact, prev_wn);
      prev_wn = Gen_Symbols_In_DINFO (dact, prev_wn);
      Reshape_ST_Entry (dinfo);
      prev_wn = Gen_Alloc_Reshape(dact, TRUE, prev_wn);
      prev_wn = Gen_HT_DART(dinfo, TRUE, FALSE, prev_wn);
    }
    break;
  default:
    FmtAssert (FALSE, ("Weird variable kind %s\n", ST_name(array_st)));
    break;
  }
  return;
} /* Lower_Pragma_Distribute_Reshape () */

/***********************************************************************
 *
 * Lower a redistribute pragma.  This involves the following steps:
 * 
 * Globals, Formals and Locals: 
 *     Allocate and initialize DART
 *     Reset numprocs,dimsize pregs 
 *         -- Resets numprocs, dimsize from existing DACT if one exists
 *            This way the variables match the distribution in the DART
 *            within the current PU (note: the variables won't match if
 *            the array is reshaped in another procedure -- this is ok
 *            since dynamic lookup is required for scheduling and the 
 *            variables won't be used)
 *     Replace DART into hash table (call HT_Replace)
 *     Generate call to Migrate_Array
 *
 ***********************************************************************/
static void Lower_Pragma_Redistribute (DISTR_ARRAY* dact) {
  /* Compile time data structures:
   * Generate a def at the redistribute point, and a use at each use for
   * affinity scheduling. DU-chains will help identify reaching
   * redistributes, that may enable compile-time affinity scheduling,
   * otherwise dynamic.
   */
  DISTR_INFO *dinfo = dact->Dinfo();

  ST* array_st = dinfo->Array_ST();
  TY_IDX array_ty = Lego_Get_Array_Type(array_st);
  WN* pwn = dact->Last_Pragma_WN ();

  if (TY_kind(array_ty) != KIND_ARRAY) {
    printf ("Pragma Distribute on a non-array\n");
    return;
  }

  WN *prev_wn = pwn;  /* pwn now points to the last pragma node,
                       * so the generated code can now follow this
                       * last pragma node.
                       */
  prev_wn = Gen_Alloc_DART (dact, prev_wn); // allocate dart
  prev_wn = Gen_Init_DART  (dact, prev_wn);
  prev_wn = Gen_Symbols_In_DINFO (dact, prev_wn);
  if ((ST_Var_Kind(array_st) == var_local) &&
      TY_kind(array_ty) == KIND_POINTER) {
    // adjustable-size local array.
    // locate the alloca for DU-info.
    dinfo->Find_Alloca ();
  }
  
  prev_wn = Gen_Migrate_Array (dinfo, FALSE, prev_wn);
  prev_wn = Gen_HT_DART(dinfo, FALSE, TRUE, prev_wn);
  return;
} /* Lower_Pragma_Redistribute () */


/***********************************************************************
 *
 * Given a WHIRL tree, replace all LDIDs of an AUTO, temp variable
 * that has a matching FORMAL_REF 
 *
 * with an LDID of the matching formal-ref variable instead.
 *
 ***********************************************************************/
static void Hack_AltEntry_Rewrite_Formals (WN* rhs) {
  if (rhs == NULL) return;
  if (WN_operator(rhs) == OPR_LDID) {
    ST* st = WN_st(rhs);
#ifdef _NEW_SYMTAB
    if (ST_sclass(st) == SCLASS_AUTO && ST_is_temp_var(st)) {
#else
    if (ST_sclass(st) == SCLASS_AUTO && ST_temp(st)) {
#endif
      // can we find a formal_ref with the same name?

      ST* fst;
#ifdef _NEW_SYMTAB
      INT i;
      FOREACH_SYMBOL (CURRENT_SYMTAB,fst,i) {
#else
      FOR_ALL_SYMBOLS(Current_Symtab, fst) {
#endif
        if (ST_class(fst) == CLASS_VAR &&
            ST_type(fst) == ST_type(st) &&
            strcmp(ST_name(fst), ST_name(st)) == 0 &&
            ST_sclass(fst) == SCLASS_FORMAL_REF) {
          WN_st_idx(rhs) = ST_st_idx(fst);
          break;
        }
      }
    }
    return;
  }

  for (INT i=0; i<WN_kid_count(rhs); i++) {
    Hack_AltEntry_Rewrite_Formals (WN_kid(rhs, i));
  }
}


/***********************************************************************
 *
 * Generate a conditional test to see if dart has a NULL value.
 * If so, recreate the dart from the specs in the formal parameters.
 * Called only for formal, regular distributes.
 * Return the last WN* created.
 *
 ***********************************************************************/
static WN* Gen_CheckNo_DART (DISTR_ARRAY* dact, WN* prev_wn) {
  DISTR_INFO* dinfo = dact->Dinfo();
  Is_True (!dinfo->IsReshaped(),
           ("Gen_CheckNo_DART should not be called on reshaped arrays\n"));
  /* Generate a conditional */
  WN* dart_ldid = dinfo->DART_Ldid();
  WN* if_wn = WN_CreateIf(dart_ldid, WN_CreateBlock(), WN_CreateBlock());
  LWN_Copy_Linenumber (prev_wn, if_wn);
  LWN_Parentize(if_wn);

  // gross hack -- else block is empty, so insert something for now
  WN* gross_wn = WN_CreatePragma(WN_PRAGMA_LOCAL, dinfo->Array_ST(), 0, 0);
  LWN_Insert_Block_After (WN_else(if_wn), NULL, gross_wn);
  WN* else_wn = Gen_Alloc_DART (dact, gross_wn);
  else_wn = Gen_Init_DART (dact, else_wn);

  // now remove the gross hack, and insert the if-wn in the tree
  LWN_Delete_Tree_From_Block (gross_wn);
  LWN_Insert_Block_After (NULL, prev_wn, if_wn);
  prev_wn = if_wn;

  // Update access vectors for expr in the ifnode
  IF_INFO *ii=CXX_NEW (IF_INFO(&LNO_default_pool,TRUE,
                               Find_SCF_Inside(if_wn,OPC_REGION)!=NULL),
                       &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map,if_wn,(void *)ii);
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                &LNO_local_pool);
  Build_Doloop_Stack(if_wn, stack);
  LNO_Build_If_Access(if_wn, stack);
  CXX_DELETE(stack, &LNO_local_pool);
  return prev_wn;
}

/***********************************************************************
 *
 * Walk all istore WNs from start to end, and replace references
 * to local, temp variables that correspond to formal-ref instances
 * with references to the formal-ref variables instead.
 *
 ***********************************************************************/
static void Hack_AltEntry_Formals (WN* start_wn, WN* end_wn) {
  WN* wn = start_wn;
  while (wn != end_wn) {

    // check if we've reached the call to __dsm_Initialize_DART
    if (WN_operator(wn) == OPR_CALL) break;

    Is_True (WN_operator(wn) == OPR_ISTORE,
             ("Hack_AltEntry_Formals: expected an ISTORE\n"));
    WN* rhs = WN_kid0(wn);
    Hack_AltEntry_Rewrite_Formals(rhs);

    wn = WN_next(wn);
  }
}

/***********************************************************************
 *
 * Process Main and Alternate Entries for each of 
 *  - distribute or
 *  - distribute_reshape directive.
 * for local arrays.
 *
 * Since the array is local, it is processed at ALL entry points
 * including main and alternates. The processing is identical at each
 * entry point. 
 *
 * Some exit code is generated corresponding to the entry code. We are
 * careful to generate the exit code just once, not for each entry.
 * (The routines that generate exit code are:
 *      - Gen_HT_DART
 *      - Gen_Migrate_Array
 *      - Gen_Alloc_Reshape
 *
 ***********************************************************************/
static WN* Process_Alt_Entries_Local (DISTR_ARRAY* dact, WN* prev_wn) {
  // come here only if alt entries
  DISTR_INFO* dinfo = dact->Dinfo();
  ST* array_st = dinfo->Array_ST();
  TY_IDX array_ty = Lego_Get_Original_Type(array_st);
  Is_True (altentry_stack, ("Process_Alt_Entries: no alt entries\n"));
  Is_True (ST_Var_Kind(array_st) == var_local,
           ("Process_Alt_Entries_Local: ST %s is not local\n",
            ST_name(array_st)));

  // First the regular entry
  prev_wn = Gen_Alloc_DART (dact, prev_wn); // allocate dart
  prev_wn = Gen_Init_DART  (dact, prev_wn);
  prev_wn = Gen_Symbols_In_DINFO (dact, prev_wn);
  if (dinfo->IsReshaped()) {
    Reshape_ST_Entry (dinfo);
    prev_wn = Gen_Alloc_Reshape(dact, TRUE, prev_wn);
  }
  else {
    if (TY_kind(array_ty) == KIND_POINTER) {
      // adjustable-size local array.
      // locate the alloca for DU-info.
      dinfo->Find_Alloca ();
    }
    prev_wn = Gen_Migrate_Array (dinfo, TRUE, prev_wn);
  }
  prev_wn = Gen_HT_DART(dinfo, TRUE, FALSE, prev_wn);

  // now the alternate entries
  WN* wn;
  for (INT i=0; i<altentry_stack->Elements(); i++) {
    wn = altentry_stack->Bottom_nth(i);
    while (wn) {
      if (WN_opcode(wn) == OPC_PRAGMA &&
          WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END)
        break;
      wn = WN_next(wn);
    }
    Is_True (wn, ("Alt_Entry: Could not find PREAMBLE_END\n"));

    // generate code
    wn = Gen_Alloc_DART (dact, wn); // allocate dart
    WN* save_wn = wn;
    wn = Gen_Init_DART  (dact, wn);
    Hack_AltEntry_Formals (WN_next(save_wn), wn);
    wn = Gen_Symbols_In_DINFO (dact, wn);
    if (dinfo->IsReshaped()) {
      wn = Gen_Alloc_Reshape(dact, FALSE, wn);
    }
    else {
      wn = Gen_Migrate_Array (dinfo, FALSE, wn);
    }
    wn = Gen_HT_DART(dinfo, FALSE, FALSE, wn);
  }
  return prev_wn;
} /* Process_Alt_Entries_Local () */

/***********************************************************************
 *
 * Process Main and Alternate Entries for each of 
 *  - distribute or
 *  - distribute_reshape directive.
 * for formal parameters.
 *
 * Since the array may be a formal at some subset of the entry points,
 * we must process each array only at those entry points where it occurs
 * in the OPC_IDNAME list. The processing itself is identical at each
 * entry point.
 *
 * No exit code is generated for formals.
 *
 ***********************************************************************/
static WN* Process_Alt_Entries_Formal (DISTR_ARRAY* dact, WN* prev_wn) {
  DISTR_INFO* dinfo = dact->Dinfo();
  ST* array_st = dinfo->Array_ST();
  INT i;
  Is_True (altentry_stack, ("Process_Alt_Entries: no alt entries\n"));
  Is_True (ST_Var_Kind(array_st) == var_formal,
           ("Process_Alt_Entries_Formal: ST %s is not formal\n",
            ST_name(array_st)));
  
  // main entry
  for (i=0; i<WN_num_formals(Current_Func_Node); i++) {
    WN* formal_wn = WN_formal(Current_Func_Node, i);
    if (WN_st(formal_wn) == array_st) {
      // it is a formal at this entry. do the processing
      if (dinfo->IsReshaped()) {
        prev_wn = Gen_Lookup_DART (dact, prev_wn, distr_st_entries[HT_Top]);
      }
      else {
        prev_wn = Gen_Lookup_DART (dact, prev_wn, distr_st_entries[HT_Check]);
        prev_wn = Gen_CheckNo_DART (dact, prev_wn);
      }
      if (Run_Dsm_Check) {
        prev_wn = Gen_EC_Reshaped_Array(dact, prev_wn);
      }
      prev_wn = Gen_Symbols_In_DINFO (dact,prev_wn);
      break;
    }
  }

  // alternate entries
  for (i=0; i<altentry_stack->Elements(); i++) {
    WN* wn = altentry_stack->Bottom_nth(i);
    WN* formal_wn;
    for (INT i=0; i<WN_kid_count(wn); i++) {
      WN* formal_wn = WN_kid(wn, i);
      Is_True (WN_opcode(formal_wn) == OPC_IDNAME,
               ("Alt_Entries: Expected OPC_IDNAME\n"));
      if (WN_st(formal_wn) == array_st) {
        // it is a formal at this entry. do the processing
        while (wn) {
          if (WN_opcode(wn) == OPC_PRAGMA &&
              WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END)
            break;
          wn = WN_next(wn);
        }
        Is_True (wn, ("Alt_Entry: Could not find PREAMBLE_END\n"));

        if (dinfo->IsReshaped()) {
          wn = Gen_Lookup_DART (dact, wn, distr_st_entries[HT_Top]);
        }
        else {
          wn = Gen_Lookup_DART (dact, wn, distr_st_entries[HT_Check]);
          wn = Gen_CheckNo_DART (dact, wn);
        }
        if (Run_Dsm_Check) {
          wn = Gen_EC_Reshaped_Array(dact, wn);
        }
        wn = Gen_Symbols_In_DINFO (dact,wn);
        break;
      }
    }
  }

  // mercifully no exit code for formals
  if (dinfo->IsReshaped()) Reshape_ST_Entry (dinfo);
  return prev_wn;
} /* Process_Alt_Entries_Formal () */


/***********************************************************************
 *
 * Replace all accesses to a reshaped common array by the new ST.
 * Called after array lowering.
 *
 ***********************************************************************/
extern void Rewrite_Reshaped_Commons (WN* wn) {
  WN *kid;

  if (!wn) return;

  if (WN_operator(wn) == OPR_LDID) {
    // commons can only be accessed using LDA. 
    // however, by the time we see them (after array lowering)
    // all LDAs of common arrays have been converted to LDIDs
    ST* st = WN_st(wn);
#ifdef _NEW_SYMTAB
    if (ST_base_idx(st) != ST_st_idx(st) &&
#else
    if (ST_sclass(st) == SCLASS_BASED &&
#endif
        ST_sclass(ST_base(st)) == SCLASS_COMMON) {
      // common
      DISTR_INFO* dinfo = da_hash->Find(st);
      if (dinfo && dinfo->IsReshaped()) {
        // reshaped, so rewrite
        WN* ldid_wn = dinfo->Load_New_Distr_Array ();
        Replace_WN (wn, ldid_wn);
      }
    }
    return;
  }

  if (WN_operator(wn) == OPR_LDA ||
      ((OPCODE_is_load(WN_opcode(wn)) || OPCODE_is_store(WN_opcode(wn))) &&
       WN_operator(wn) != OPR_ILOAD && WN_operator(wn) != OPR_ISTORE)) {
    // memory op, but not ILOAD or ISTORE (for those we follow addr-kid).
    ST* st = WN_st(wn);
#ifdef _NEW_SYMTAB
    if (ST_base_idx(st) != ST_st_idx(st) &&
#else
    if (ST_sclass(st) == SCLASS_BASED &&
#endif
        ST_sclass(ST_base(st)) == SCLASS_COMMON) {
      // common
      DISTR_INFO* dinfo = da_hash->Find(st);
      if (dinfo && dinfo->IsReshaped()) {
        // reshaped, error
        ErrMsgSrcpos (EC_LNO_Generic2String, LWN_Get_Linenum(wn),
                      "Bad reference to reshaped common array",
                      ST_name(st));
      }
    }
  }

  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first(wn);
    while (kid) {
      Rewrite_Reshaped_Commons(kid);
      kid = WN_next(kid);
    }
    return;
  } 

  for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn, kidno);
    Rewrite_Reshaped_Commons(kid);
  }
  return;
}


/***********************************************************************
 *
 * Return next expr node in in-order traversal.
 *
 ***********************************************************************/
WN* LWN_Get_Next_Expr_Node (WN* wn) {
  if (wn) {
    do {
      wn = LWN_Get_Next_Tree_Node (wn);
    } while (wn && !OPCODE_is_expression(WN_opcode(wn)));
  }
  return wn;
}

/***********************************************************************
 *
 * Returns true if child is a descendent of parent -- i.e. 
 * in the sub-tree rooted at the parent. 
 * Does not depend on parent pointers being set and consistent.
 * (distinct from Is_Descendent in lwn_util.cxx which checks if parent
 *  is reachable from child through parent pointers).
 *
 ***********************************************************************/
BOOL Is_Child (WN* child, WN* parent) {
  if ((child == NULL) || (parent == NULL)) return FALSE;
  printf ("Is_Child: 0x%p, 0x%p\n", child, parent);
  if (child == parent) return TRUE;
  if (WN_opcode(parent) == OPC_BLOCK) {
    WN* kid_wn = WN_first(parent);
    while (kid_wn) {
      if (Is_Child (child, kid_wn)) return TRUE;
      kid_wn = WN_next(kid_wn);
    }
    return FALSE;
  }
  else {
    for (INT i=0; i<WN_kid_count(parent); i++)
      if (Is_Child (child, WN_kid(parent,i))) return TRUE;
    return FALSE;
  }
}

/***********************************************************************
 *
 * orig and copy are identical trees.
 * Walk the trees, searching for expr nodes,
 * and then doing a copy of the def-list on each use.
 *
 ***********************************************************************/
static void Expr_DU_Copy (WN* orig, WN* copy) {

  FmtAssert (orig, ("Expr_DU_Copy: exit_wn (orig) is NULL"));
  orig = LWN_Get_Next_Expr_Node (orig);
  copy = LWN_Get_Next_Expr_Node (copy);
  // This loop must go till orig is NULL.
  // Note that the parent pointer of orig (exit_wn)
  // is carefully initialized to NULL.
  while (orig) {
    OPCODE fop = WN_opcode(orig);
    OPCODE top = WN_opcode(copy);

    FmtAssert(fop == top && OPCODE_is_expression(fop),
              ("Opcodes unequal Expr_DU_Copy(%d,%d) or not expr", fop, top));

    if (OPCODE_is_expression(WN_opcode(orig))) {
      DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(orig);
      if (deflist) {
        DEF_LIST_ITER iter(deflist);
        for (DU_NODE* n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
          WN* def = n->Wn();
          Du_Mgr->Add_Def_Use(def, copy);
        }
        DEF_LIST* deflist2 = Du_Mgr->Ud_Get_Def(copy);
        if (deflist2) deflist2->Set_loop_stmt(deflist->Loop_stmt());
        if (deflist->Incomplete()) deflist2->Set_Incomplete();
      }
    }
    orig = LWN_Get_Next_Expr_Node (orig);
    copy = LWN_Get_Next_Expr_Node (copy);
  }
} /* Expr_DU_Copy */

/***********************************************************************
 *
 * Patch in the exit code at all the return points of the PU
 *
 ***********************************************************************/
static void Insert_Exit_Code (STACK_OF_WN* exit_stack) {
  WN *pwn;

  if (WN_first(exit_wn) != NULL) {
    // Processing return nodes in the stack
    for (INT i=0; i < exit_stack->Elements(); i++) {
      pwn = exit_stack->Bottom_nth(i);
      FmtAssert (WN_operator(pwn) == OPR_RETURN
#ifdef KEY
  		 || WN_operator(pwn) == OPR_GOTO_OUTER_BLOCK
#endif
      		 ,
                 ("Insert_Exit_Code found non-return node (got opcode=%d)\n",
                  WN_opcode(pwn)));
      
      WN* copy_wn = LWN_Copy_Tree(exit_wn);
      // Copy def-use. Assumes that exit_wn has only uses, no defs 
      Expr_DU_Copy (exit_wn, copy_wn);
      // Copy line numbers.
      {
        WN* tmp_wn = WN_first(copy_wn);
        while (tmp_wn) {
          WN_linenum(tmp_wn) = LWN_Get_Linenum(pwn);
          tmp_wn = WN_next(tmp_wn);
        }
      }
      LWN_Insert_Block_Before (NULL, pwn, copy_wn);
    }
  } else {
    FmtAssert (WN_last(exit_wn) == NULL, ("exit_wn block should be NULL"));
  }

  LWN_Update_Def_Use_Delete_Tree (exit_wn, Du_Mgr);
  LWN_Delete_Tree (exit_wn);
} /* Insert_Exit_Code */

/***********************************************************************
 *
 * Generate code to call the runtime routine to allocate a dart,
 * and store the return value into the supplied dart_st entry.
 *
 * The generated code is added after pwn in the whirl tree.
 * ndims provides the number of dimensions in the distributed array. 
 * 
 * Return a pointer to the last wn inserted, so that caller can continue
 * to insert code after this.
 *
 ***********************************************************************/
static WN *Gen_Alloc_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* st) {

  DISTR_INFO* dinfo = dact->Dinfo();
  ST* dart_st = (st ? st : dinfo->Dart_ST());
  INT ndims = dinfo->Num_Dim();
  // Generate call to __dsm_Allocate_Dart to get memory
  OPCODE callop = OPCODE_make_op(OPR_CALL, Pointer_type, MTYPE_V);
  WN *call_wn = WN_Create(callop, 1);
  WN *dims_wn = LWN_Make_Icon (MTYPE_I8, ndims);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Allocate_Dart]);
  Set_Runtime_Call_Side_Effects (call_wn);
  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (MTYPE_I8, dims_wn, Be_Type_Tbl(MTYPE_I8),
                                 WN_PARM_BY_VALUE);
    LWN_Set_Parent (dims_wn, parm_wn);
    dims_wn = parm_wn;
  }
  WN_kid0(call_wn) = dims_wn;
  LWN_Set_Parent(dims_wn, call_wn);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = call_wn;

  // Generate code to store the return values into dart
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers (Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
  
  //OPCODE        stop = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
  //OPCODE        ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);

  WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                              Pointer_type,
                                              Pointer_type),
                               rreg1, rst, Be_Type_Tbl(Pointer_type));
  Create_alias (Alias_Mgr, ldid_wn); // just a register
  Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
  WN *stid_wn = LWN_CreateStid (OPCODE_make_op(OPR_STID,
                                               MTYPE_V,
                                               Pointer_type),
                                0,
                                dart_st,
                                distr_ty_entries[RT_ptr],
                                ldid_wn);
  dinfo->DART_Stid (stid_wn, st);
  LWN_Insert_Block_After (NULL, prev_wn, stid_wn);
  WN_linenum(stid_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = stid_wn;
  return prev_wn;
} /* Gen_Alloc_DART */

/***********************************************************************
 *
 * Generate call to HT_Top, store ret-val into dart_st.
 *
 ***********************************************************************/
static WN *Gen_Lookup_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* func_st) {

  DISTR_INFO *dinfo = dact->Dinfo();
  ST* dart_st = dinfo->Dart_ST();

  Is_True (func_st == distr_st_entries[HT_Top] ||
           func_st == distr_st_entries[HT_Check],
           ("Gen_Lookup_DART must be called with HT_Top or HT_Check\n"));

  OPCODE call_op = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);

  WN *call_wn = WN_Create(call_op, 1);
  WN *array_load_wn = dinfo->Load_Distr_Array ();

  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, array_load_wn,
                                 Be_Type_Tbl(Pointer_type),
                                 WN_PARM_BY_VALUE);
    LWN_Set_Parent (array_load_wn, parm_wn);
    array_load_wn = parm_wn;
  }
  WN_kid(call_wn, 0) = array_load_wn;
  WN_st_idx(call_wn) = ST_st_idx(func_st);
  Set_Runtime_Call_Side_Effects (call_wn);
  LWN_Set_Parent(array_load_wn, call_wn);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN *wn;
  for (wn = call_wn; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      break; 
  if (wn != NULL)
    Array_Dependence_Graph->Add_Vertex(call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = call_wn;

  // Generate code to store the return values into dart
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers (Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
  
  //OPCODE        stop = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
  //OPCODE        ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);

  WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                              Pointer_type,
                                              Pointer_type),
                               rreg1, rst, Be_Type_Tbl(Pointer_type));
  Create_alias (Alias_Mgr, ldid_wn); // just a preg
  Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
  WN *stid_wn = LWN_CreateStid (OPCODE_make_op(OPR_STID,
                                               MTYPE_V,
                                               Pointer_type),
                                0,
                                dart_st,
                                distr_ty_entries[RT_ptr],
                                ldid_wn);
  dinfo->DART_Stid(stid_wn);
  LWN_Insert_Block_After (NULL, prev_wn, stid_wn);
  WN_linenum(stid_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = stid_wn;
  return prev_wn;
} /* Gen_Lookup_DART */

/***********************************************************************
 *
 * Generate code to check if reshaped formal parameters match
 * actual reshaped arguments that are bieng passed.
 *
 * Return the WN* for the last node generated.
 *
 ***********************************************************************/
static WN* Gen_EC_Reshaped_Array(DISTR_ARRAY *dact, WN* prev_wn) 
{
  DISTR_INFO* dinfo = dact->Dinfo();
  
  WN *dart_ldid_wn = dinfo->DART_Ldid ();
  ST *ec_dart_st = Create_Local_ST (dinfo->Dart_ST());

  prev_wn = Gen_Alloc_DART (dact, prev_wn, ec_dart_st);
  prev_wn = Gen_Init_DART  (dact, prev_wn, ec_dart_st);

  WN *ec_dart_ldid_wn = dinfo->DART_Ldid (ec_dart_st);

  prev_wn = Gen_Compare_DARTS (dart_ldid_wn, 
                               ec_dart_ldid_wn, 
                               prev_wn,
                               ST_name(dinfo->Array_ST()));
  prev_wn = Gen_Dealloc_DART (dact, prev_wn, ec_dart_st);
  
  return prev_wn;
} /* Gen_EC_Reshaped_Array */

/***********************************************************************
 *
 * Generate code to call the library routine to deallocate a dart.
 *
 * Return the WN* for the last node generated.
 *
 ***********************************************************************/
static WN* 
Gen_Compare_DARTS (WN* dart1_ldid, 
                   WN* dart2_ldid, 
                   WN* prev_wn, 
                   char* array_name) 
{
  // Call a runtime routine to compare two darts
  OPCODE call_op = OPCODE_make_op(OPR_CALL,MTYPE_V,MTYPE_V);
  WN *call_wn = WN_Create(call_op, 4);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Compare_Darts]);
  Set_Runtime_Call_Side_Effects (call_wn);

  TCON tcon;
  OPCODE lda_op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
  TY_IDX string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);

  char *func_name = DRA_Demangle_Func(ST_name(WN_st(Current_Func_Node)));
  if (func_name == NULL) func_name = ST_name(WN_st(Current_Func_Node));
  tcon = Host_To_Targ_String (MTYPE_STRING, func_name, strlen(func_name)+1);
  ST *rt_func_name = Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
  WN *func_name_wn = WN_CreateLda(lda_op, 0, string_ty, rt_func_name);

  tcon = Host_To_Targ_String (MTYPE_STRING, array_name, strlen(array_name)+1);
  ST *rt_array_name = Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
  WN *array_name_wn = WN_CreateLda(lda_op, 0, string_ty, rt_array_name);

  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, 
                                 dart1_ldid,
                                 Be_Type_Tbl(Pointer_type),
                                 WN_PARM_BY_REFERENCE);
    LWN_Set_Parent (dart1_ldid, parm_wn);
    dart1_ldid = parm_wn;

    parm_wn = WN_CreateParm (Pointer_type, 
                             dart2_ldid,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    LWN_Set_Parent (dart2_ldid, parm_wn);
    dart2_ldid = parm_wn;
    
    parm_wn = WN_CreateParm (Pointer_type, 
                             func_name_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    LWN_Set_Parent (func_name_wn, parm_wn);
    func_name_wn = parm_wn;

    parm_wn = WN_CreateParm (Pointer_type, 
                             array_name_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    LWN_Set_Parent (array_name_wn, parm_wn);
    array_name_wn = parm_wn;
  }

  WN_kid0(call_wn) = dart1_ldid;
  WN_kid1(call_wn) = dart2_ldid;
  WN_kid2(call_wn) = func_name_wn;
  WN_kid3(call_wn) = array_name_wn;
  LWN_Set_Parent(dart1_ldid, call_wn);
  LWN_Set_Parent(dart2_ldid, call_wn);
  LWN_Set_Parent(func_name_wn, call_wn);
  LWN_Set_Parent(array_name_wn, call_wn);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);  
  prev_wn = call_wn;
  return prev_wn;
} /* Gen_Compare_DARTS */

/***********************************************************************
 *
 * Generate code to call the library routine to deallocate a dart.
 *
 * Return the WN* for the last node generated.
 *
 ***********************************************************************/
static WN* Gen_Dealloc_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* st) {

  DISTR_INFO* dinfo = dact->Dinfo();

  // Call a runtime routine to deallocate dart
  OPCODE call_op = OPCODE_make_op(OPR_CALL,MTYPE_V,MTYPE_V);
  WN *call_wn = WN_Create(call_op, 1);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Deallocate_Dart]);
  Set_Runtime_Call_Side_Effects (call_wn);
  WN *dart_ldid_wn = dinfo->DART_Ldid (st);
  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, dart_ldid_wn,
                                 Be_Type_Tbl(Pointer_type),
                                 WN_PARM_BY_REFERENCE);
    LWN_Set_Parent (dart_ldid_wn, parm_wn);
    dart_ldid_wn = parm_wn;
  }
  WN_kid0(call_wn) = dart_ldid_wn;
  LWN_Set_Parent(dart_ldid_wn, call_wn);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);  
  prev_wn = call_wn;
  return prev_wn;
} /* Gen_Dealloc_DART */


/***********************************************************************
 *
 * Given an array ty and a dimension number, 
 * return whirl for the lower-bound of that dimension.
 *
 ***********************************************************************/
static WN* Get_Array_Dimension_LB (TY_IDX array_ty, INT i) {

  if (TY_AR_const_lbnd(array_ty, i))
    return LWN_Make_Icon (MTYPE_I8, TY_AR_lbnd_val(array_ty,i));
#ifdef _NEW_SYMTAB
  else {
    ST_IDX st_idx = TY_AR_lbnd_var(array_ty,i);
    TYPE_ID type = TY_mtype(ST_type(st_idx));
    // TODO: fix alias and du info
    return WN_CreateLdid(OPCODE_make_op(OPR_LDID,Promote_Type(type),type),
                         0, ST_ptr(st_idx),type);
  } 
#else
  else return LWN_Copy_Tree (TY_AR_lbnd_tree(array_ty, i));
#endif
} /* Get_Array_Dimension_LB */

/***********************************************************************
 *
 * Given an array ty and a dimension number, 
 * return whirl for the size of that dimension.
 *
 ***********************************************************************/
extern WN* Get_Array_Dimension_Size (TY_IDX array_ty, INT i) {
  WN *ret_wn;
  INT elem_size = TY_size(TY_AR_etype(array_ty));
/* TODO: check if strides are meaningful when the code reaches us.
 *  INT j;
 *  // compute the real element size
 *  for (j=0; j<i; j++) {
 *  }
 */


  if (TY_AR_const_ubnd(array_ty, i) &&
      TY_AR_const_lbnd(array_ty, i) &&
      TY_AR_const_stride(array_ty, i)) {
    // assume stride is always "1" for now 
    ret_wn = LWN_Make_Icon (MTYPE_I8, (TY_AR_ubnd_val(array_ty,i) -
                                    TY_AR_lbnd_val(array_ty,i) + 1));
    
  }
  else {
    // something is non-const, so generate an expression
    WN *ubnd_wn, *lbnd_wn, *stride_wn;
    if (TY_AR_const_ubnd(array_ty,i))
      ubnd_wn = LWN_Make_Icon(MTYPE_I8, TY_AR_ubnd_val(array_ty, i));
#ifdef _NEW_SYMTAB
    else {
      ST_IDX st_idx = TY_AR_ubnd_var(array_ty,i);
      TYPE_ID type = TY_mtype(ST_type(st_idx));
      // TODO: fix alias and du info
      ubnd_wn=WN_CreateLdid(OPCODE_make_op(OPR_LDID,Promote_Type(type),type),
                            0, ST_ptr(st_idx),type);
    } 
#else
    else ubnd_wn = LWN_Copy_Tree (TY_AR_ubnd_tree(array_ty, i));
#endif

    if (TY_AR_const_lbnd(array_ty, i))
      if (TY_AR_lbnd_val(array_ty, i) == 1) lbnd_wn = NULL;
      else lbnd_wn = LWN_Make_Icon(MTYPE_I8, 
                                   TY_AR_lbnd_val(array_ty,i));
#ifdef _NEW_SYMTAB
    else {
      ST_IDX st_idx = TY_AR_lbnd_var(array_ty,i);
      TYPE_ID type = TY_mtype(ST_type(st_idx));
      // TODO: fix alias and du info
      lbnd_wn = WN_CreateLdid(OPCODE_make_op(OPR_LDID,Promote_Type(type),type),
                              0, ST_ptr(st_idx),type);
    } 
#else
    else lbnd_wn = LWN_Copy_Tree (TY_AR_lbnd_tree(array_ty, i));
#endif

    if (TY_AR_const_stride(array_ty, i))
      if (TY_AR_stride_val(array_ty, i) == elem_size) 
        stride_wn = NULL;
      else {
        stride_wn = 
          LWN_Make_Icon(MTYPE_I8, TY_AR_stride_val(array_ty,i));
        stride_wn = AWN_Div_Safe (MTYPE_I8,
                                  stride_wn,
                                  LWN_Make_Icon(MTYPE_I8,elem_size));
        DevWarn ("Stride on distributed array is not 1");
      }
    else {
#ifdef _NEW_SYMTAB
      ST_IDX st_idx = TY_AR_stride_var(array_ty,i);
      TYPE_ID type = TY_mtype(ST_type(st_idx));
      // TODO: fix alias and du info
      stride_wn=WN_CreateLdid(OPCODE_make_op(OPR_LDID,Promote_Type(type),type),
                              0, ST_ptr(st_idx),type);
#else
      stride_wn = LWN_Copy_Tree (TY_AR_stride_tree(array_ty, i));
#endif
      stride_wn = AWN_Div_Safe (MTYPE_I8,
                                stride_wn,
                                LWN_Make_Icon(MTYPE_I8,elem_size));
      DevWarn ("Stride on distributed array is not constant, maybe not 1");
    }

    if (lbnd_wn == NULL) ret_wn = ubnd_wn;
    else {
      ret_wn = AWN_Sub (MTYPE_I8, ubnd_wn, lbnd_wn);  // ubnd-lbnd
      ret_wn = AWN_Add (MTYPE_I8, ret_wn, LWN_Make_Icon(MTYPE_I8, 1)); // + 1
    }
    if (stride_wn != NULL) ret_wn = AWN_Div_Safe (MTYPE_I8, ret_wn, stride_wn);
  }
  return ret_wn;
} /* Get_Array_Dimension_Size */

/***********************************************************************
 *
 * Given a dart, generate code to initialize its various values,
 * and then generate code to call the library routine to initialize
 * the number of processors.
 *
 * Return the WN* for the last node generated.
 *
 ***********************************************************************/
static WN* Gen_Init_DART (DISTR_ARRAY* dact, WN* prev_wn, ST* st) {

  INT i;
  DISTR_INFO* dinfo = dact->Dinfo();
  ST* array_st = dinfo->Array_ST();
  INT ndims = dinfo->Num_Dim();

  TY_IDX array_ty = dinfo->Orig_TY();
  TY_IDX I8ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  OPCODE const_op = OPCODE_make_op(OPR_INTCONST, MTYPE_I8, MTYPE_V);
  WN *dart_ldid_wn = dinfo->DART_Ldid (st);
  // dart->num_dim = ndims
  WN *const_wn = WN_CreateIntconst(const_op, ndims);
  OPCODE istore_op = OPCODE_make_op(OPR_ISTORE, MTYPE_V, MTYPE_I8);
  WN* istore_wn = LWN_CreateIstore (istore_op, dart_offset_num_dim,
                                    I8ptr_ty,
                                    const_wn,
                                    dart_ldid_wn);
  dinfo->DART_Ptr_Ref (istore_wn, st);
  LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
  WN_linenum(istore_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = istore_wn;

  // dart->element_size = element_size
  const_wn = WN_CreateIntconst (const_op,
                                TY_size(TY_AR_etype(array_ty)));
  dart_ldid_wn = dinfo->DART_Ldid (st);
  istore_wn = LWN_CreateIstore (istore_op, dart_offset_element_size,
                                I8ptr_ty,
                                const_wn, dart_ldid_wn);
  dinfo->DART_Ptr_Ref (istore_wn, st);
  LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
  WN_linenum(istore_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = istore_wn;

  // dart->flags = reshaped_p
  const_wn = WN_CreateIntconst (const_op,
                                (dinfo->IsReshaped() ? 1 : 0));
  dart_ldid_wn = dinfo->DART_Ldid (st);
  istore_wn = LWN_CreateIstore (istore_op, dart_offset_flags,
                                I8ptr_ty,
                                const_wn, dart_ldid_wn);
  dinfo->DART_Ptr_Ref (istore_wn, st);
  LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
  WN_linenum(istore_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = istore_wn;

  // fill in the fields for each dimension.
  istore_op = OPCODE_make_op (OPR_ISTORE, MTYPE_V, MTYPE_I8);
  for (i=0; i<ndims; i++) {
    WN *n_wn, *p_wn;
    // dart->distr[i].n = <dimension-size>
    // old: n_wn = Get_Array_Dimension_Size (array_ty, i);
    n_wn = LWN_Copy_Tree(dinfo->Get_Array_Dim_Size(i));
    dart_ldid_wn = dinfo->DART_Ldid (st);
    istore_wn = LWN_CreateIstore
      (istore_op,
       dart_offset_distr_n+i*TY_size(distr_ty_entries[RT_dim_struct]),
       I8ptr_ty,
       n_wn,
       dart_ldid_wn);
    dinfo->DART_Ptr_Ref (istore_wn, st);
    LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
    WN_linenum(istore_wn) = LWN_Get_Linenum(prev_wn);
    prev_wn = istore_wn;

    // dart->distr[i].k = 
    //  -1 if not distributed
    //   0 if block
    //   k if cyclic or block-cyclic
    WN* k_wn;
    DISTR_DIM* dd = dact->Get_Dim(i);
    DISTRIBUTE_TYPE dt = dd->Distr_Type();
    switch (dt) {
    case DISTRIBUTE_STAR:
      k_wn =  WN_CreateIntconst (const_op, -1);
      break;
    case DISTRIBUTE_BLOCK:
      k_wn =  WN_CreateIntconst (const_op, 0);
      break;
    default:
      k_wn = dd->Chunksize();
      break;
    }
    dart_ldid_wn = dinfo->DART_Ldid(st);
    istore_wn = LWN_CreateIstore
      (istore_op,
       dart_offset_distr_k+i*TY_size(distr_ty_entries[RT_dim_struct]),
       I8ptr_ty,
       k_wn,
       dart_ldid_wn);
    dinfo->DART_Ptr_Ref (istore_wn, st);
    LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
    WN_linenum(istore_wn) = LWN_Get_Linenum(prev_wn);
    prev_wn = istore_wn;
    


    // dart->distr[i].lb = lower bound of the array
    WN *lb_wn = Get_Array_Dimension_LB (array_ty, i);
    dart_ldid_wn = dinfo->DART_Ldid (st);
    istore_wn = LWN_CreateIstore
      (istore_op,
       dart_offset_distr_lb+i*TY_size(distr_ty_entries[RT_dim_struct]),
       I8ptr_ty,
       lb_wn, dart_ldid_wn);
    dinfo->DART_Ptr_Ref (istore_wn, st);
    LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
    WN_linenum(istore_wn) = LWN_Get_Linenum(prev_wn);
    prev_wn = istore_wn;
  }

  // Now call a runtime routine to fill in dart appropriately
  // and to enter into hash-table
  OPCODE call_op = OPCODE_make_op(OPR_CALL,MTYPE_V,MTYPE_V);
  WN *call_wn = WN_Create(call_op, 2);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Initialize_Dart]);
  Set_Runtime_Call_Side_Effects (call_wn);
  dart_ldid_wn = dinfo->DART_Ldid (st);

  WN* onto_wn;
  if (dact->Has_Onto()) {
    char name[64];
    sprintf (name, "$onto_%s", ST_name(array_st));
    ST* onto_st = Create_Local_Array_ST(name, Be_Type_Tbl(MTYPE_I8), ndims);

    for (i=0; i<ndims; i++) {
      istore_wn = Create_Array_Store (onto_st, MTYPE_I8, i, 8, ndims,
                                      LWN_Make_Icon(MTYPE_I8, dact->Onto(i)));
      LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
      LWN_Copy_Linenumber (prev_wn, istore_wn);
      prev_wn = istore_wn;
    }
    OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
    onto_wn = WN_CreateLda(op_lda, 0,
                           Make_Pointer_Type(ST_type(onto_st)),
                           onto_st);
    Add_Pragma_To_MP_Region (prev_wn, onto_st, 0, WN_PRAGMA_LOCAL);
  }
  else {
    // need to pass NULL
    onto_wn = LWN_Make_Icon (Pointer_type, 0);
  }

  // dart
  WN* parm_wn = WN_CreateParm (Pointer_type, dart_ldid_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_REFERENCE);
  LWN_Set_Parent (dart_ldid_wn, parm_wn);
  WN_kid(call_wn, 0) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);
  // onto
  parm_wn = WN_CreateParm (Pointer_type, onto_wn,
                           Be_Type_Tbl(Pointer_type),
                           WN_PARM_BY_REFERENCE);
  LWN_Set_Parent (onto_wn, parm_wn);
  WN_kid(call_wn, 1) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);
    
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);  
  prev_wn = call_wn;
  return prev_wn;
} /* Gen_Init_DART */

/***********************************************************************
 *
 * generate code to store the respective values into Jennifer pregs,
 * and store the symbols in dact.
 * Based on the ST, see if these symbols have already been allocated or
 * not. This is done by searching for a dact for this ST -- if one exists,
 * then it contains the symbols for these pregs.
 *
 ***********************************************************************/
static WN* Gen_Symbols_In_DINFO (DISTR_ARRAY* dact, WN* prev_wn) {

  DISTR_INFO* dinfo = dact->Dinfo();
  ST* array_st = dinfo->Array_ST();
  INT ndims = dinfo->Num_Dim();

  INT i;

  OPCODE iload_op = OPCODE_make_op(OPR_ILOAD, MTYPE_I8, MTYPE_I8);
  OPCODE stid_op =  OPCODE_make_op(OPR_STID,  MTYPE_V,  MTYPE_I8);

  // allocate and initialize Jennifer's pregs,
  // and store them in dinfo.
  for (i=0; i<ndims; i++) {
    // First do the Num_procs
    // local_p_i = dart->distr[i].p
    WN* stid_wn;
    SYMBOL *numprocs = dinfo->Get_Numprocs(i);
    // Must find a previous symbol, since these are now created during
    // read pragma phase
    FmtAssert (numprocs, ("Could not find numprocs variable for array (%s)\n",
                          ST_name(array_st)));
    WN* dart_ldid_wn = dinfo->DART_Ldid();
    WN* dart_p_wn = LWN_CreateIload
      (iload_op,
       dart_offset_distr_p+i*TY_size(distr_ty_entries[RT_dim_struct]),
       Be_Type_Tbl(MTYPE_I8),
       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8)),
       dart_ldid_wn);
    dinfo->DART_Ptr_Ref (dart_p_wn);
    stid_wn = LWN_CreateStid (stid_op,
                              numprocs->WN_Offset(), numprocs->St(),
                              Be_Type_Tbl(MTYPE_I8),
                              dart_p_wn);
    LWN_Check_Parentize (stid_wn);
    LWN_Insert_Block_After (NULL, prev_wn, stid_wn); 
    WN_linenum(stid_wn) = LWN_Get_Linenum(prev_wn);
    prev_wn = stid_wn;
    dinfo->Numprocs_Stid (i, stid_wn);
 
    // Next the jennifer-dimension size: 
    // local_n_i = dart->distr[i].n
    DISTR_DIM* dd = dact->Get_Dim(i);
    DISTRIBUTE_TYPE dt = dd->Distr_Type();
    if (dt == DISTRIBUTE_STAR) {
      continue;
    }
    // not star, so need a real preg/symbol.
    // Find a previous symbol, otherwise create one
    SYMBOL *dimsize = dinfo->Get_Dimsize(i);
    FmtAssert (dimsize, ("Could not find dimsize variable for array (%s)\n",
                         ST_name(array_st)));
    WN *n_wn, *p_wn;
    // old: n_wn = Get_Array_Dimension_Size (array_ty, i);
    n_wn = dinfo->Get_Array_Dim_Size(i);
    p_wn = dinfo->Numprocs(i);

    if ((dt == DISTRIBUTE_BLOCK) ||
        ((dt == DISTRIBUTE_CYCLIC_CONST) &&
         (dd->Chunk_Const_Val() == 1))) {
      // Block, or chunk with chunksize of 1
      // generate divceil (n/p)
      WN *divceil_wn;
      if (dinfo->Small_Index())
        divceil_wn = Create_Positive_Divceil (MTYPE_I4, n_wn, p_wn, TRUE);
      else divceil_wn = Create_Positive_Divceil (MTYPE_I8, n_wn, p_wn, TRUE);
      stid_wn = AWN_StidIntoSym (dimsize, divceil_wn);
    }
    else {
      // Must be cyclic_const with k != 1, or cyclic_var.
      // generate divceil (divceil(n/k)/p)
      WN *k_wn, *nk_wn, *divceil_wn;
      k_wn = dd->Chunksize ();
      if (dinfo->Small_Index()) {
        nk_wn = Create_Positive_Divceil (MTYPE_I4, n_wn, k_wn, TRUE);
        divceil_wn = Create_Positive_Divceil (MTYPE_I4, nk_wn, p_wn, TRUE);
      }
      else {
        nk_wn = Create_Positive_Divceil (MTYPE_I8, n_wn, k_wn, TRUE);
        divceil_wn = Create_Positive_Divceil (MTYPE_I8, nk_wn, p_wn, TRUE);
      }
      stid_wn = AWN_StidIntoSym (dimsize, divceil_wn);
    }
    LWN_Insert_Block_After (NULL, prev_wn, stid_wn);
    WN_linenum(stid_wn) = LWN_Get_Linenum(prev_wn);
    prev_wn = stid_wn;
    dinfo->Dimsize_Stid(i, stid_wn);
  }
  return prev_wn;
} /* Gen_Symbols_In_DINFO */


/***********************************************************************
 *
 * Generate call to given routine with the array_st and the dart as the
 * arguments (e.g. HT_Push, HT_Replace, Migrate_Array).  If dact is NULL
 * then we know that the array is not reshaped.
 *
 ***********************************************************************/
static WN* Gen_Call_Array_Dart_Args(DISTR_INFO* dinfo, ST *func_st) {

  OPCODE call_op = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);

  WN *call_wn = WN_Create(call_op, 2);
  WN *array_load_wn = dinfo->Load_Distr_Array();

  WN* dart_ldid_wn = dinfo->DART_Ldid();

  // If dart_def_wn is the function entry, then the dart is a global
  // variable that is defined at startup.  
  //

  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, array_load_wn,
                                 Be_Type_Tbl(Pointer_type),
                                 WN_PARM_BY_VALUE);
    LWN_Set_Parent (array_load_wn, parm_wn);
    array_load_wn = parm_wn;

    if ((func_st == distr_st_entries[Migrate_Array]) ||
        (func_st == distr_st_entries[Unmigrate_Array])) {
      /* this function dereferences the dart */
      parm_wn = WN_CreateParm (Pointer_type, dart_ldid_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_REFERENCE);
    } else {
      parm_wn = WN_CreateParm (Pointer_type, dart_ldid_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_VALUE);
    }
    LWN_Set_Parent (dart_ldid_wn, parm_wn);
    dart_ldid_wn = parm_wn;
  }

  WN_kid(call_wn, 0) = array_load_wn;
  WN_kid(call_wn, 1) = dart_ldid_wn;
  WN_st_idx(call_wn) = ST_st_idx(func_st);

  LWN_Set_Parent(array_load_wn, call_wn);
  LWN_Set_Parent(dart_ldid_wn, call_wn);

  return call_wn;
} /* Gen_Call_Array_Parm_Args */

/***********************************************************************
 *
 * Generate call to given routine with the array_st as the single 
 * argument (e.g. HT_Pop, HT_Freeze, HT_Unfreeze).
 *
 ***********************************************************************/
static WN* Gen_Call_Array_Arg(DISTR_INFO* dinfo, ST *func_st) {
  OPCODE call_op = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);

  WN *call_wn = WN_Create(call_op, 1);
  WN *array_load_wn = dinfo->Load_Distr_Array ();

  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, array_load_wn,
                                 Be_Type_Tbl(Pointer_type),
                                 WN_PARM_BY_VALUE);
    LWN_Set_Parent (array_load_wn, parm_wn);
    array_load_wn = parm_wn;
  }
  WN_kid(call_wn, 0) = array_load_wn;
  WN_st_idx(call_wn) = ST_st_idx(func_st);
  LWN_Set_Parent(array_load_wn, call_wn);

  return call_wn;
} /* Gen_Call_Array_Arg */


/***********************************************************************
 *
 * Generate call to runtime routine to enter dart into hash-table.
 * Also generate call to do the exit processing of hash-table,
 * and store that into exit_code.
 * If called while processing a redistribute, then do_exit is FALSE,
 * and no exit code is generated.
 *
 ***********************************************************************/
static WN* Gen_HT_DART (DISTR_INFO* dinfo, BOOL do_exit, BOOL is_redistr,
                        WN* prev_wn)  {
  ST* array_st = dinfo->Array_ST();
  WN *call_wn;

  if (is_redistr) {
    call_wn = Gen_Call_Array_Dart_Args(dinfo, distr_st_entries[HT_Replace]);
  } else {
    call_wn = Gen_Call_Array_Dart_Args(dinfo, distr_st_entries[HT_Push]);
  }
  Set_Runtime_Call_Side_Effects (call_wn);

  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = call_wn;

  // If it's a redistribute then only generate the above call to HT_Replace
  // and return.  We don't want to re-generate exit code for every 
  // redistribute.
  //
  if (!do_exit) return prev_wn;

  // Now generate exit code
  call_wn = Gen_Call_Array_Arg(dinfo, distr_st_entries[HT_Pop]);
  Set_Runtime_Call_Side_Effects (call_wn);
  LWN_Insert_Block_Before (exit_wn, NULL, call_wn);

  return prev_wn;
} /* Gen_HT_DART */

/***********************************************************************
 *
 * Generate code to call __dsm_Migrate_Array,
 * and possibly __dsm_Unmigrate_Array.
 *
 ***********************************************************************/
static WN* Gen_Migrate_Array (DISTR_INFO* dinfo, BOOL do_exit, WN *prev_wn) 
{
  WN *call_wn=Gen_Call_Array_Dart_Args(dinfo,distr_st_entries[Migrate_Array]);
  Set_Runtime_Call_Side_Effects (call_wn);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);

  // If it's a redistribute then only generate the above call to
  // Migrate_Array and return.
  //
  if (!do_exit) return call_wn;

  // Now generate exit code
  WN* unmigr_wn =
    Gen_Call_Array_Dart_Args (dinfo, distr_st_entries[Unmigrate_Array]);
  Set_Runtime_Call_Side_Effects (unmigr_wn);
  LWN_Insert_Block_Before (exit_wn, NULL, unmigr_wn);
  return call_wn;
}


/***********************************************************************
 *
 * Return a global TY entry for an array of the appropriate size based
 * on ndim.
 *
 ***********************************************************************/
static TY_IDX Section_Variable_TY (INT ndim, INT32 name_length) {
  static TY_IDX* da_ty = NULL; /* array of types, one for each value of size.
                             * (Most of the low index values remain unused)
                             */
  static INT idx = 0;

  INT sz = (2*Pointer_Size +  // array, dart
            4*4 +             // is_reshaped, element_size, ndims, fluff
            ndim * (dart_distr_size*8 +   // dart fields
                    8 +                   // onto
                    2*Pointer_Size) +     // distr[..]
            name_length);     // name
  
  if (da_ty == NULL) {
    // hasn't been allocated yet.
    // Use Malloc_Mem_Pool since these are global TYs and survive
    // across multiple PUs.
    idx = sz+10;   // allocate upto 10 extra byte sizes
    da_ty = CXX_NEW_ARRAY (TY_IDX, idx, Malloc_Mem_Pool);
    for (INT i=0; i<idx; i++) da_ty[i] = (TY_IDX) NULL;
  }

  if (sz >= idx) {
    // exceeded array, realloc 10 extra elements
    INT new_size = sz+10;
    da_ty = (TY_IDX*) MEM_POOL_Realloc (Malloc_Mem_Pool, da_ty, idx*sizeof(TY_IDX),
                                     new_size*sizeof(TY_IDX));
    for (INT i=idx; i<new_size; i++) da_ty[i] = (TY_IDX) NULL;
    idx = new_size;
  }

  if (da_ty[sz] == (TY_IDX) NULL) {
    
    char ty_name[64];
    sprintf (ty_name, "distr_array_data_ty_%d", sz);

#ifdef _NEW_SYMTAB
    TY& new_ty = New_TY (da_ty[sz]);
    TY_Init (new_ty,
             sz,
             KIND_ARRAY,
             MTYPE_UNKNOWN,
             Save_Str(ty_name));

    ARB_HANDLE arb = New_ARB();
    ARB_Init(arb,0,sz,1);
    Set_ARB_first_dimen (arb);
    Set_ARB_last_dimen (arb);

    Set_TY_align(da_ty[sz],8);
    Set_TY_arb(new_ty,arb);
    Set_TY_etype(new_ty,Be_Type_Tbl(MTYPE_I1));

#else
    da_ty[sz] = New_TY (TRUE);
    TY_kind(da_ty[sz]) =  KIND_ARRAY;

    ARI* ari = New_ARI (1, TRUE);
    ARI_etype (ari)        = Be_Type_Tbl(MTYPE_I1); // byte
    ARI_const_zofst(ari)   = TRUE;
    ARI_zofst_val(ari)     = 0;

    ARB_const_lbnd(ARI_bnd(ari,0)) = TRUE;
    ARB_lbnd_val(ARI_bnd(ari,0)) = 0;
    ARB_const_ubnd(ARI_bnd(ari,0)) = TRUE;
    ARB_ubnd_val(ARI_bnd(ari,0)) = sz;
    ARB_const_stride(ARI_bnd(ari,0)) = TRUE;
    ARB_stride_val(ARI_bnd(ari,0)) = 1;

    TY_size(da_ty[sz]) = sz;
    TY_align(da_ty[sz]) = 8;
    TY_name(da_ty[sz]) = Save_Str (ty_name);
    TY_arinfo(da_ty[sz]) = ari;
#endif
    Enter_TY (da_ty[sz]);
  }
  return da_ty[sz];
}

/***********************************************************************
 *
 * Given the name of the variable, and a TY, 
 * return an ST of the appropriate type allocation from the 
 * _MIPS_distr_array section.
 *
 ***********************************************************************/
extern ST* Section_Variable_ST (char* name, TY_IDX daty, BOOL is_global) {
  ST* st;
  st = New_ST(is_global ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_DISTR_ARRAY,
           EXPORT_LOCAL,
           daty);
  return st;
}

/***********************************************************************
 *
 * Process distribution of a global/common array, generating info
 * into the distr_array section.
 * 
 * Don't need dart pointer or jennifer pregs, since those are global
 * variables.
 *
 ***********************************************************************/
static void Process_Global_Distribute (DISTR_ARRAY* dact) {

  /* Write out the following structure:
   *    void**  array;      address of array
   *    void**  dart;       address of dart
   *    INT32   isreshaped;
   *    INT32   element_size;
   *    INT32   ndims;
   *    INT32   fluff;      dummy 4 bytes, for 8-byte alignment
   * dims X
   *    INT64   n;          dimension size
   *    INT64   p;          number of processors (actually a code)
   *    INT64   k;          chunksize, if cyclic
   *    INT64   lb;         lower-bound of array dimension
   *    INT64   onto        value of onto clause in this dimension, if any
   *    INT64*  numprocs;   address of numprocs (jennifer variable)
   *    INT64*  dimsize;    address of dimsize (jennifer variable)
   *
   *    char*   name;       name of array
   *
   */

  DISTR_INFO *dinfo = dact->Dinfo();

  ST* array_st = dinfo->Array_ST();
  TY_IDX array_ty = Lego_Get_Array_Type(array_st);
  INT ndims = TY_AR_ndims(array_ty);
  extern INT32 da_count;
  da_count++;
  ST* dast;
  {
    /* allocate the ST of the appropriate size in the section */
    TY_IDX daty = Section_Variable_TY (ndims, strlen(ST_name(array_st))+1);
    char name[64];
    sprintf (name, "_%s_da_spec", ((strlen(ST_name(array_st)) < 50) ?
                                   ST_name(array_st) : "LongName"));
    dast = Section_Variable_ST (name, daty, TRUE); /* for a common,
                                                     * which is in local symtab
                                                     */
  }

  INITO_IDX ino = New_INITO (dast);
  Set_ST_is_initialized(dast);
  INITV_IDX inv = (INITV_IDX) NULL;
  if (dinfo->IsReshaped() && (ST_Var_Kind(array_st) == var_common)) {
    SYMBOL* array_common = dinfo->Array_Common_Symbol();
    inv = Irb_Init_Symoff (ino, inv, 1, array_common->St(),
                           array_common->WN_Offset());   // &array
  }
  else inv = Irb_Init_Symoff (ino, inv, 1, array_st, 0);   // &array
  ST* dart_st = dinfo->Dart_ST();
  inv = Irb_Init_Symoff (ino, inv, 1, dart_st, 0);    // &dart
  inv = Irb_Init_Integer (4, (dinfo->IsReshaped() ? 1 : 0),
                          1, ino, inv);               // is_reshaped
  inv = Irb_Init_Integer (4,
                          TY_size(TY_AR_etype(Lego_Get_Array_Type(array_st))),
                          1, ino, inv);               // element_size

  inv = Irb_Init_Integer (4, ndims, 1, ino, inv);     // ndims
  if (dact->Is_Compiler_Generated()) {
    inv = Irb_Init_Integer (4, 1, 1, ino, inv);         // fluff
  }
  else {
    inv = Irb_Init_Integer (4, 0, 1, ino, inv);         // fluff
  }
  for (INT i=0; i<ndims; i++) {
    FmtAssert (TY_AR_const_lbnd(array_ty, i) &&
               TY_AR_const_ubnd(array_ty, i) &&
               TY_AR_const_stride(array_ty, i),
               ("Global array (%s) must have constant %s\n",
                ST_name(array_st),
                (!TY_AR_const_lbnd(array_ty,i) ? "lbnd" :
                 (!TY_AR_const_ubnd(array_ty,i) ? "ubnd" :
                  "stride"))));
#ifdef Is_True_On
    {
      INT stride = TY_size(TY_AR_etype(array_ty));
      for (INT j=i+1; j<ndims; j++)
        stride = (stride * (TY_AR_ubnd_val(array_ty, j) -
                            TY_AR_lbnd_val(array_ty, j) + 1));
      if (TY_AR_stride_val(array_ty, i) != stride)
        DevWarn ("Global array (%s): stride (%lld) != expected stride (%d)\n",
                 ST_name(array_st),
                 TY_AR_stride_val(array_ty, i), stride);
    }
#endif
    inv = Irb_Init_Integer (8, (TY_AR_ubnd_val(array_ty, i)-
                                TY_AR_lbnd_val(array_ty, i)+1),
                            1, ino, inv);             // n
    // Now fill in "p" and "k"
    // k = -1 (not distributed), 0 (block), >1 (cyclic)
    DISTR_DIM* dd = dact->Get_Dim(i);
    DISTRIBUTE_TYPE dt = dd->Distr_Type();
    INT32 p_val = 0, k_val = 0;
    switch (dt) {
    case DISTRIBUTE_STAR:
      k_val = -1;
      break;
    case DISTRIBUTE_BLOCK:
      k_val = 0;
      break;
    case DISTRIBUTE_CYCLIC_CONST:
      k_val = dd->Chunk_Const_Val();
      break;
    case DISTRIBUTE_CYCLIC_EXPR:
      FmtAssert (FALSE,("Variable chunk-size disallowed for global arrays"));
    default:
      FmtAssert (FALSE, ("Unknown distribution type"));
    }
    inv = Irb_Init_Integer (8, p_val, 1, ino, inv);
      
    // Now fill in "k"
    inv = Irb_Init_Integer (8, k_val, 1, ino, inv);
      
    // Now fill in "lb"
    inv = Irb_Init_Integer(8, TY_AR_lbnd_val(array_ty,i), 1, ino, inv);
      
    // Now fill in "onto"
    inv = Irb_Init_Integer(8, (dact->Has_Onto()?dact->Onto(i):0), 1, ino, inv);

    // Now allocate symbols for Jennifer's variables,
    // and write them out.
    ST * numprocs_st = dinfo->Get_Numprocs(i)->St();
    ST* dimsize_st =  dinfo->Get_Dimsize(i)->St();
    inv = Irb_Init_Symoff (ino, inv, 1, numprocs_st, 0);  // &numprocs
    inv = Irb_Init_Symoff (ino, inv, 1, dimsize_st, 0);   // &dimsize
  }
  inv = Irb_Init_String (strlen(ST_name(array_st))+1,
                         ST_name(array_st),
                         1, ino, inv);                // name of array
  if (dinfo->IsReshaped()) {
    Reshape_ST_Entry (dinfo);
#ifdef _NEW_SYMTAB
    if (ST_level(array_st) == GLOBAL_SYMTAB) {
#else
    if (ST_is_global(array_st)) {
#endif
      /* i.e. it is in the global symbol table,
       * so F_S_L did not call Allocate_Object on it.
       */
      FmtAssert (ST_is_reshaped(array_st),
                 ("ST (%s) is reshaped, but symbol table doesn't think so",
                  ST_name(array_st)));
      if (Has_Base_Block(array_st)) Allocate_Object(ST_base(array_st));
      else Allocate_Object (array_st);
    }
  }
  /* Now allocate appropriate storage for this global.
   * Simple global? 
   *    - then Finalize_Static_Layout did not call Allocate_Object on it, 
   *      so call it now.
   * Common, with partial split?
   *    - F_S_L did not call Allocate_Object on it,
   *      so call Allocate_Object on ST_base of this ST.
   * Common, with full split?
   *    - F_S_L did not call Allocate_Object on just this ST,
   *      call it now.
   */

  /* Allocate the variable in the distr_array section. */
  Allocate_Object (dast);
}

/***********************************************************************
 *
 * Create local ST* entry corresponding to dart_st 
 * that will be used in error checking
 *
 ***********************************************************************/
static ST* Create_Local_ST (ST* dart_st) {
  char name[64];
  char* tmps = ST_name(dart_st);
  sprintf (name, "_ec%s", ((strlen(tmps) < 60) ? tmps : "LongName"));
  ST *st = New_ST(CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           ST_type(dart_st));
  Set_ST_is_temp_var(st);
  Set_ST_pt_to_unique_mem(st);
  Set_ST_pt_to_compiler_generated_mem(st);
  return st;
}

extern ST* Create_Local_Array_ST (char* array_name, 
                                  TY_IDX ty, 
                                  INT num) {
  char name[64];
  ST* st;

#ifdef _NEW_SYMTAB
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb,0,num-1,1);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  TY_IDX ty_idx;

  sprintf (name, "_%s_ty%d", ((strlen(array_name)<50)?
                                           array_name : "LongName") , num);

  TY& arr_ty = New_TY(ty_idx);
  TY_Init (arr_ty,
           TY_size(ty)*num,
           KIND_ARRAY,
           MTYPE_UNKNOWN,
           Save_Str(name));
  Set_TY_arb(arr_ty,arb);
  Set_TY_align(ty_idx,8);
  Set_TY_etype(arr_ty,ty);

#else

  ARI *ari;
  ari = New_ARI (1, FALSE);
  ARI_etype(ari) = ty;
  ARB_const_lbnd(ARI_bnd(ari, 0)) = TRUE;
  ARB_const_ubnd(ARI_bnd(ari, 0)) = TRUE;
  ARB_const_stride(ARI_bnd(ari, 0)) = TRUE;
  ARB_lbnd_val(ARI_bnd(ari, 0)) = 0;
  ARB_ubnd_val(ARI_bnd(ari, 0)) = num-1;
  ARB_stride_val(ARI_bnd(ari, 0)) = 1;
  

  TY_IDX arr_ty = New_TY(FALSE);
  TY_kind(arr_ty) = KIND_ARRAY;
  TY_btype(arr_ty) = MTYPE_M;
  TY_arinfo(arr_ty) = ari;
  sprintf (name, "_%s_ty%d", ((strlen(array_name)<50)?
                                           array_name : "LongName") , num);
  TY_name(arr_ty) = Save_Str(name);
  TY_size(arr_ty) = TY_size(ty)*num;
  TY_align(arr_ty) = 8;
#endif
  Enter_TY(arr_ty);
  

  sprintf (name, "_%s",
           ((strlen(array_name)<50) ? array_name : "LongName"));
  st = New_ST(CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           ty_idx);
  Set_ST_pt_to_unique_mem(st);
  Set_ST_pt_to_compiler_generated_mem(st);
  Set_ST_is_temp_var(st);
  return st;
}


/***********************************************************************
 *
 * Generate code to call __dsm_Alloc_Reshaped_Array (dart)
 * and store the return value into the array_st.
 * Called ONLY for local reshaped arrays.
 *
 ***********************************************************************/
static WN* Gen_Alloc_Reshape (DISTR_ARRAY* dact, BOOL do_exit, WN* prev_wn) {

  DISTR_INFO* dinfo = dact->Dinfo();
  ST* array_st = dinfo->Array_ST();
  
  WN *dart_ldid_wn = dinfo->DART_Ldid();
  OPCODE call_op = OPCODE_make_op(OPR_CALL, Pointer_type, MTYPE_V);
  WN *call_wn = WN_Create(call_op, 1);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Alloc_Reshape]);
  Set_Runtime_Call_Side_Effects (call_wn);
  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, dart_ldid_wn,
                                 Be_Type_Tbl(Pointer_type),
                                 WN_PARM_BY_REFERENCE);
    LWN_Set_Parent(dart_ldid_wn, parm_wn);
    dart_ldid_wn = parm_wn;
  }
  WN_kid0(call_wn) = dart_ldid_wn;
  LWN_Set_Parent (dart_ldid_wn, call_wn);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = call_wn;
  
  // Generate code to store the return values into the array
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers (Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
  
  WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                              Pointer_type,
                                              Pointer_type),
                               rreg1, rst, Be_Type_Tbl(Pointer_type));
  Create_alias (Alias_Mgr, ldid_wn);
  Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
  WN *stid_wn = LWN_CreateStid (OPCODE_make_op(OPR_STID,
                                               MTYPE_V,
                                               Pointer_type),
                                0,
                                array_st,
                                ST_type(array_st),
                                ldid_wn);
  Set_ST_pt_to_unique_mem(array_st);
  Set_ST_pt_to_compiler_generated_mem(array_st);
  Create_unique_pointer_alias (Alias_Mgr, array_st, stid_wn, NULL);
  WN* copy_wn = LWN_Copy_Tree(stid_wn);
  Copy_alias_info (Alias_Mgr, stid_wn, copy_wn);
  dinfo->Set_Array_Alias_WN (copy_wn);
  dinfo->Set_Array_Def_WN (stid_wn);
  LWN_Insert_Block_After (NULL, prev_wn, stid_wn);
  WN_linenum(stid_wn) = LWN_Get_Linenum(prev_wn);
  prev_wn = stid_wn;

  if (!do_exit) return prev_wn;

  // Now generate exit code
  OPCODE ldid_op = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  WN *array_ldid_wn = WN_CreateLdid (ldid_op, 0, array_st, ST_type(array_st));
  Copy_alias_info (Alias_Mgr, stid_wn, array_ldid_wn);
  Du_Mgr->Add_Def_Use (stid_wn, array_ldid_wn);
  call_wn = WN_Create(OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V), 2);

  dart_ldid_wn = dinfo->DART_Ldid();

  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, dart_ldid_wn,
                                 Be_Type_Tbl(Pointer_type),
                                 WN_PARM_BY_REFERENCE);
    LWN_Set_Parent(dart_ldid_wn, parm_wn);
    dart_ldid_wn = parm_wn;

    parm_wn = WN_CreateParm (Pointer_type, array_ldid_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    LWN_Set_Parent(array_ldid_wn, parm_wn);
    array_ldid_wn = parm_wn;
  }
  WN_kid(call_wn, 0) = dart_ldid_wn;
  WN_kid(call_wn, 1) = array_ldid_wn;
  /* Might need kid-1 so that dealloc knows structure of array.
   * in that case update du-chains.
   */
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Dealloc_Reshape]);
  Set_Runtime_Call_Side_Effects (call_wn);
  LWN_Set_Parent(dart_ldid_wn, call_wn);
  LWN_Set_Parent(array_ldid_wn, call_wn);
  LWN_Insert_Block_After (exit_wn, NULL, call_wn);

  return prev_wn;
} /* Gen_Alloc_Reshape */

/***********************************************************************
 *
 * Mangle the ST entry for a reshaped array to void** 
 * (for each of local/global/common/formal).
 *
 ***********************************************************************/
static void Reshape_ST_Entry (DISTR_INFO* dinfo) {
  ST* array_st = dinfo->Array_ST();

#ifdef _NEW_SYMTAB
  if (ST_level(array_st) == GLOBAL_SYMTAB &&
#else
  if (ST_is_global(array_st) &&
#endif
      (da_global->Find(array_st)) &&
      (TY_kind(ST_type(array_st)) == KIND_POINTER)) {
    // this ST has already been reshaped
    return;
  }

  FmtAssert ((TY_kind(ST_type(array_st)) == KIND_ARRAY) ||
             ((TY_kind(ST_type(array_st)) == KIND_POINTER) &&
              (ST_isFormal(array_st) || ST_isLocal(array_st))),
             ("Reshape_ST_Entry: ST (%s) not an array, and not formal/local",
              ST_name(array_st)));
  TY_IDX array_ty = Lego_Get_Array_Type(array_st);

  // The new type of the array: pointer-to-pointer to element-type
  //
  TY_IDX new_ty = Make_Pointer_Type(Make_Pointer_Type(TY_AR_etype(array_ty)));

  /* If it has a base block, then it could be a common 
   * or an assumed size formal allocated using alloca.
   * Catch the common case, and update other variables in the common.
   */
  if (Has_Base_Block(array_st)) {
#ifdef _NEW_SYMTAB
  if (ST_base_idx(array_st) != ST_st_idx(array_st)) {
#else
  if (ST_sclass(array_st) == SCLASS_BASED) {
#endif
      if (ST_sclass(ST_base(array_st)) == SCLASS_COMMON) {
        // is a common
        ST* st;

        // mINT64 delta = ST_size(array_st) - Pointer_Size;
        // reduce stuff by some multiple of 16 bytes (the largest alignment)
        // to avoid screwing up the alignment of following objects.

#ifdef _NEW_SYMTAB
        mINT64 delta = (TY_size(ST_type(array_st))/16)*16;
        if (delta == TY_size(ST_type(array_st))) {
#else
        mINT64 delta = (ST_size(array_st)/16)*16;
        if (delta == ST_size(array_st)) {
#endif
          // avoid reducing by the entire size, since this might have the
          // unfortunate side-effect of making the common disappear altogether.
          delta = delta - 16;
        }
        if (delta > 0) {
#ifdef _NEW_SYMTAB
          INT i;
          FOREACH_SYMBOL (GLOBAL_SYMTAB,st,i)
#else
          FOR_ALL_SYMBOLS(Current_Symtab, st)
#endif
            {
              if ((ST_base(st) == ST_base(array_st)) && (st != array_st)) {
                VB_PRINT(printf ("Reshape_ST_Entry: ");
                        printf ("Var %s in same common as reshaped array %s\n",
                                ST_name(st), ST_name (array_st)));
                // st must be a partial split portion
                if (ST_ofst(st) > ST_ofst(array_st)) {
                  Set_ST_ofst(st, ST_ofst(st) - delta);
                }
                else {
                  // before reshaped entry in common, so offset does not change
                  continue;
                }
              }
            }
        
          // adjust the type specification of the underlying struct
          // of the COMMON block
          TY_IDX struct_ty = ST_type(ST_base(array_st));
          Is_True (TY_kind(struct_ty) == KIND_STRUCT,
                   ("Expected KIND_STRUCT for base of COMMON block %s",
                    ST_name(ST_base(array_st))));

          FLD_ITER fld_iter = Make_fld_iter (TY_fld(struct_ty));
          do {
	    FLD_HANDLE fld (fld_iter);
            if (FLD_ofst(fld) == ST_ofst(array_st)) {
              Set_FLD_type(fld, new_ty);
            }
            else if (FLD_ofst(fld) > ST_ofst(array_st)) {
              Set_FLD_ofst(fld, FLD_ofst(fld)-delta);
            }
          } while (!FLD_last_field(fld_iter++));

          // decrease the size of the overall common
          TY_IDX ty = ST_type(ST_base(array_st));
          Set_TY_size(ty,TY_size(ty) - delta);
        }
        else {
          DevWarn ("Reshape_ST_Entry (%s): delta is <= 0", ST_name(array_st));
        }
      }
      else {
        FmtAssert (ST_sclass(ST_base(array_st)) == SCLASS_AUTO,
                 ("ST (%s) is BASED, but not common/auto\n",
                  ST_name(array_st)));
        /* Since we're going to malloc storage for this,
         * this ST is no longer based.
         */
        Set_ST_sclass(array_st, SCLASS_AUTO);
      }
    }
    else {
      DevWarn ("ST (%s) has base block, but is not BASED", ST_name(array_st));
      FmtAssert ((ST_sclass(array_st) == SCLASS_AUTO),
                 ("ST (%s) has base block, but is not sclass_based\n", 
                  ST_name(array_st)));
    }
  }

#ifdef _NEW_SYMTAB
  Set_ST_type(array_st,new_ty);
  Set_TY_ptr_as_array(new_ty);
  Set_TY_ptr_as_array(TY_pointed(new_ty));
#else
  ST_type(array_st) = Make_Pointer_Type(Make_Pointer_Type
                                        (TY_AR_etype(array_ty),
                                         ST_is_global(array_st)),
                                        ST_is_global(array_st));
  Set_TY_ptr_as_array(ST_type(array_st));
  Set_TY_ptr_as_array(TY_pointed(ST_type(array_st)));
#endif
  Set_ST_pt_to_unique_mem(array_st);
  Set_ST_pt_to_compiler_generated_mem(array_st);
}


static ST* Declare_Func_Zero_Arg (const char* ty_name, const char* st_name,
                                  TY_IDX ret_ty) {
  ST* func_st;
  TY_IDX func_ty;
  INT nparms = 0;
  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));

#ifdef _NEW_SYMTAB
  TY& new_ty = New_TY(func_ty);
  TY_Init (new_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(ty_name));
  Set_TY_has_prototype(func_ty);
  Set_TY_align(func_ty,TY_align(voidpty));

  TYLIST_IDX tylist_idx;
  TYLIST&    tylist_head = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = ret_ty;
  TYLIST_IDX first_tylist_idx = tylist_idx;
  TYLIST& tylist_tail = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = 0;
  Set_TY_tylist (new_ty, first_tylist_idx);

#else
  func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (nparms, TRUE /* global/local */ );
  TY_name(func_ty) = Save_Str(ty_name);
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = ret_ty;
  Enter_TY (func_ty);
#endif

  /* Now make a PU */
  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);
  PU_Init (pu, func_ty, GLOBAL_SYMTAB + 1);

  /* Make a ST: add function to global symbol table */
  func_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (func_st,
           Save_Str(st_name),
           CLASS_FUNC,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           pu_idx);
  return func_st;
}

static ST* Declare_Func_One_Arg (const char* ty_name, const char* st_name,
                                  TY_IDX ret_ty, TY_IDX arg1_ty) {
  ST* func_st;
  TY_IDX func_ty;
  INT nparms = 1;
  TYLIST* parms;
  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));

#ifdef _NEW_SYMTAB
  TY& new_ty = New_TY(func_ty);
  TY_Init (new_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(ty_name));
  Set_TY_has_prototype(func_ty);
  Set_TY_align(func_ty,TY_align(voidpty));

  TYLIST_IDX tylist_idx;
  TYLIST&    tylist_head = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = ret_ty;
  TYLIST_IDX first_tylist_idx = tylist_idx;

  TYLIST &tylist_arg = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = arg1_ty;

  TYLIST& tylist_tail = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = 0;
  Set_TY_tylist (new_ty, first_tylist_idx);

#else
  func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (nparms, TRUE /* global/local */ );
  parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = arg1_ty;
  TY_name(func_ty) = Save_Str(ty_name);
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = ret_ty;
  Enter_TY (func_ty);

#endif

  /* Now make a PU */
  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);
  PU_Init (pu, func_ty, GLOBAL_SYMTAB + 1);

  /* Make a ST: add function to global symbol table */
  func_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (func_st,
           Save_Str(st_name),
           CLASS_FUNC,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           pu_idx);
  return func_st;
}

static ST* Declare_Func_Two_Arg (const char* ty_name, const char* st_name,
                              TY_IDX ret_ty, TY_IDX arg1_ty, TY_IDX arg2_ty) {
                                 
  ST* func_st;
  TY_IDX func_ty;
  INT nparms = 2;
  TYLIST* parms;
  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));

#ifdef _NEW_SYMTAB
  TY& new_ty = New_TY(func_ty);
  TY_Init (new_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(ty_name));
  Set_TY_has_prototype(func_ty);
  Set_TY_align(func_ty,TY_align(voidpty));

  TYLIST_IDX tylist_idx;
  TYLIST&    tylist_head = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = ret_ty;
  TYLIST_IDX first_tylist_idx = tylist_idx;

  TYLIST &tylist_arg = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = arg1_ty;

  TYLIST &tylist_arg2 = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = arg2_ty;

  TYLIST& tylist_tail = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = 0;
  Set_TY_tylist (new_ty, first_tylist_idx);
#else

  func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (nparms, TRUE /* global/local */ );
  parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = arg1_ty;
  TYLIST_item(&parms[1]) = arg2_ty;
  TY_name(func_ty) = Save_Str(ty_name);
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = ret_ty;
  Enter_TY (func_ty);
#endif

  /* Now make a PU */
  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);
  PU_Init (pu, func_ty, GLOBAL_SYMTAB + 1);

  /* Make a ST: add function to global symbol table */
  func_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (func_st,
           Save_Str(st_name),
           CLASS_FUNC,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           pu_idx);
  return func_st;
}

static ST* Declare_Func_Three_Arg (const char* ty_name, const char* st_name,
                                   TY_IDX ret_ty,
                                TY_IDX arg1_ty, TY_IDX arg2_ty, TY_IDX arg3_ty) {
  ST* func_st;
  TY_IDX func_ty;
  INT nparms = 3;
  TYLIST* parms;
  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));

#ifdef _NEW_SYMTAB
  TY& new_ty = New_TY(func_ty);
  TY_Init (new_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(ty_name));
  Set_TY_has_prototype(func_ty);
  Set_TY_align(func_ty,TY_align(voidpty));

  TYLIST_IDX tylist_idx;
  TYLIST&    tylist_head = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = ret_ty;
  TYLIST_IDX first_tylist_idx = tylist_idx;

  TYLIST &tylist_arg = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = arg1_ty;

  TYLIST tylist_arg2 = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = arg2_ty;

  TYLIST tylist_arg3 = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = arg3_ty;

  TYLIST& tylist_tail = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = 0;
  Set_TY_tylist (new_ty, first_tylist_idx);

#else

  func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (nparms, TRUE /* global/local */ );
  parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = arg1_ty;
  TYLIST_item(&parms[1]) = arg2_ty;
  TYLIST_item(&parms[2]) = arg3_ty;
  TY_name(func_ty) = Save_Str(ty_name);
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = ret_ty;
  Enter_TY (func_ty);
#endif

  /* Now make a PU */
  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);
  PU_Init (pu, func_ty, GLOBAL_SYMTAB + 1);

  /* Make a ST: add function to global symbol table */
  func_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (func_st,
           Save_Str(st_name),
           CLASS_FUNC,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           pu_idx);
  return func_st;
}

static ST* Declare_Func_N_Arg (const char* ty_name, const char* st_name, 
                               TY_IDX ret_ty, INT nargs, TY_IDX ty_array[])

{
  ST* func_st;
  TY_IDX func_ty;
  INT nparms = nargs;
  TYLIST* parms;
  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V)); 

#ifdef _NEW_SYMTAB
  TY& new_ty = New_TY(func_ty);
  TY_Init (new_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(ty_name));
  Set_TY_has_prototype(func_ty);
  Set_TY_align(func_ty,TY_align(voidpty));

  TYLIST_IDX tylist_idx;
  TYLIST&    tylist_head = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = ret_ty;
  TYLIST_IDX first_tylist_idx = tylist_idx;

  for (INT i=0; i<nargs; i++) {
    TYLIST &tylist_arg = New_TYLIST (tylist_idx);
    Tylist_Table [tylist_idx] = ty_array[i];
  }

  TYLIST& tylist_tail = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = 0;
  Set_TY_tylist (new_ty, first_tylist_idx);

#else


  func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (nparms, TRUE /* global/local */ );
  parms = TY_parms(func_ty);

  for (INT i = 0; i < nargs; i++) {
    TYLIST_item(&parms[i]) = ty_array[i];
  }

  TY_name(func_ty) = Save_Str(ty_name);
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = ret_ty;
  Enter_TY (func_ty);
#endif

  /* Now make a PU */
  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);
  PU_Init (pu, func_ty, GLOBAL_SYMTAB + 1);

  /* Make a ST: add function to global symbol table */
  func_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (func_st,
           Save_Str(st_name),
           CLASS_FUNC,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           pu_idx);
  return func_st;
}

extern void Init_Special_Lego_Mp_Call() {
  {
    TY_IDX i4_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4));
    TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
    TY_IDX ty_array[8];

    ty_array[0] = Be_Type_Tbl(MTYPE_I8);     /* original lb */
    ty_array[1] = Be_Type_Tbl(MTYPE_I8);     /* original ub */
    ty_array[2] = Be_Type_Tbl(MTYPE_I8);     /* original step */
    ty_array[3] = Be_Type_Tbl(MTYPE_I8);     /* this dim's number of threads */
    ty_array[4] = Be_Type_Tbl(MTYPE_I8);     /* my pid */
    ty_array[5] = i8_ptr_ty;                 /* ptr to new lb variable */
    ty_array[6] = i8_ptr_ty;                 /* ptr to new ub variable */
    ty_array[7] = i4_ptr_ty;                 /* ptr to lastlocal flag */

    ST* st = Declare_Func_N_Arg (".__dsm_simple_bounds", "__dsm_simple_bounds",
                                 Be_Type_Tbl(MTYPE_V),
                                 8,
                                 ty_array);

    distr_st_entries[Simple_Bounds] = st;
  }
  {
    TY_IDX i4_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4));
    TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
    TY_IDX ty_array[4];

    ty_array[0] = Be_Type_Tbl(MTYPE_I8);      /* number of threads */
    ty_array[1] = Be_Type_Tbl(MTYPE_I8);      /* number of nested loops */
    ty_array[2] = i8_ptr_ty;                  /* onto */
    ty_array[3] = i8_ptr_ty;                  /* layout (output) */

    ST* st = Declare_Func_N_Arg (".__dsm_processor_layout", 
                                 "__dsm_processor_layout", 
                                 Be_Type_Tbl(MTYPE_V),
                                 4,
                                 ty_array);

    distr_st_entries[Processor_Layout] = st;  
  }
  {
    TY_IDX i4_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4));
    TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
    TY_IDX ty_array[4];

    ty_array[0] = i8_ptr_ty;                  /* layout */
    ty_array[1] = Be_Type_Tbl(MTYPE_I8);      /* number of nested loops */
    ty_array[2] = Be_Type_Tbl(MTYPE_I8);      /* my thread id */
    ty_array[3] = i8_ptr_ty;                  /* indices (output) */

    ST* st = Declare_Func_N_Arg (".__dsm_processor_coordinates", 
                                 "__dsm_processor_coordinates", 
                                 Be_Type_Tbl(MTYPE_V),
                                 4,
                                 ty_array);
    distr_st_entries[Processor_Coordinates] = st; 
  }    
  {
    TY_IDX i4_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4));
    TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
    TY_IDX ty_array[14];

    ty_array[0] = Be_Type_Tbl(Pointer_type);   /* array */
    ty_array[1] = Be_Type_Tbl(MTYPE_I8);       /* array dimension */
    ty_array[2] = Be_Type_Tbl(MTYPE_I8);       /* outer index */
    ty_array[3] = Be_Type_Tbl(MTYPE_I8);       /* inner index */
    ty_array[4] = Be_Type_Tbl(MTYPE_I8);       /* loop number */
    ty_array[5] = Be_Type_Tbl(MTYPE_I8);       /* stride */
    ty_array[6] = Be_Type_Tbl(MTYPE_I8);       /* offset */
    ty_array[7] = Be_Type_Tbl(MTYPE_I8);       /* original lb */
    ty_array[8] = Be_Type_Tbl(MTYPE_I8);       /* original ub */
    ty_array[9] = Be_Type_Tbl(MTYPE_I8);       /* original step */
    ty_array[10] = i8_ptr_ty;                  /* ptr to new lb variable */
    ty_array[11] = i8_ptr_ty;                  /* ptr to new ub variable */
    ty_array[12] = i8_ptr_ty;                  /* ptr to new step variable */
    ty_array[13] = i4_ptr_ty;                  /* ptr to lastlocal flag */

    ST* st = Declare_Func_N_Arg (".__dsm_dynamic_affinity_bounds", 
                                 "__dsm_dynamic_affinity_bounds", 
                                 Be_Type_Tbl(MTYPE_V),
                                 14,
                                 ty_array);

    distr_st_entries[Dynamic_Affinity_Bounds] = st;  
  }    

  {
#ifdef _NEW_SYMTAB
    TY_IDX vi4_ty = Copy_TY(Be_Type_Tbl(MTYPE_I4));
#else
    TY_IDX vi4_ty = Copy_TY(Be_Type_Tbl(MTYPE_I4), TRUE);
#endif
    Set_TY_is_volatile(vi4_ty);
    ST* st;
    st             = New_ST(GLOBAL_SYMTAB);
    ST_Init (st,
#ifndef KEY
             Save_Str("__mp_sug_numthreads"),
#else
             Save_Str("__ompc_sug_numthreads"),
#endif
             CLASS_VAR,
             SCLASS_EXTERN,
             EXPORT_PREEMPTIBLE,
             vi4_ty);
    Set_ST_not_gprel(st);    // avoid runtime warnings.
    distr_st_entries[mp_sug_numthreads] = st;

    st             = New_ST(GLOBAL_SYMTAB);
    ST_Init (st,
#ifndef KEY
             Save_Str("__mp_cur_numthreads"),
#else
             Save_Str("__ompc_cur_numthreads"),
#endif
             CLASS_VAR,
             SCLASS_EXTERN,
             EXPORT_PREEMPTIBLE,
             vi4_ty);
    Set_ST_not_gprel(st);    // avoid runtime warnings.
    distr_st_entries[mp_cur_numthreads] = st;
  }
  {
    distr_st_entries[mp_numthreads_fn] =
#ifndef KEY
      Declare_Func_Zero_Arg (".__mp_numthreads", "__mp_numthreads",
                             Be_Type_Tbl(MTYPE_I4));
#else
      Declare_Func_Zero_Arg (".omp_get_num_threads", "omp_get_num_threads",
                             Be_Type_Tbl(MTYPE_I4));
#endif
  }
#ifndef KEY // Pathscale does not have this
  { 
    distr_st_entries[mp_cur_numthreads_func] =
      Declare_Func_Zero_Arg (".__mp_cur_numthreads_func", 
                             "__mp_cur_numthreads_func",
                             Be_Type_Tbl(MTYPE_I4));
  }
#endif
  { 
    distr_st_entries[mp_my_threadnum] =
#ifndef KEY
      Declare_Func_Zero_Arg (".mp_my_threadnum", "mp_my_threadnum",
			    Be_Type_Tbl(MTYPE_I4));
#else
      Declare_Func_Zero_Arg (".omp_get_thread_num", "omp_get_thread_num",
			    Be_Type_Tbl(MTYPE_I4));
#endif
  } 
}

extern BOOL Special_Lego_Or_Mp_Call(ST* st_call)
{
  if ((st_call == distr_st_entries[Simple_Bounds]) ||
      (st_call == distr_st_entries[Processor_Layout]) ||
      (st_call == distr_st_entries[Processor_Coordinates]) ||
      (st_call == distr_st_entries[Cyclic_Bounds]) ||
      (st_call == distr_st_entries[Dynamic_Affinity_Bounds]) ||
      (st_call == distr_st_entries[mp_my_threadnum]))
    return TRUE;
  return FALSE; 
}


/***********************************************************************
 *
 * Generate ST and TY entries in the global symbol table for
 * types and functions in the runtime system:
 *
 ***********************************************************************/
void Generate_Runtime_Stuff () {
  TY_IDX rt_struct_ty_idx;
  TY_IDX rt_ptr_ty;
  TY_IDX rt_dim_struct_ty;
  TY_IDX rt_dim_ptr_ty;

  TY_IDX voidpty;
  TY_IDX voidppty;
  TY_IDX func_ty;
  ST* func_st;
  TYLIST *parms;
  INT32  nparms = 2;

  /* Make types */
  /* pointer to void */
  voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));
  distr_ty_entries[VOID_ptr] = voidpty;

  /* pointer to pointer to void */
  voidppty = Make_Pointer_Type (voidpty);

  /* Changes to this structure need to made in each of the following places:
   *    dsm_runtime.h - obviously, since that defines this struct
   *    dsm_startup.c - where we read the MIPS_distr_array section
   *                    and initialize a dart
   *                    Also declaration of DA_RECORD.
   *    lego_util.h   - where we #define the offsets into this struct
   *    lego_pragma.cxx - where we read the chunksize from the dart
   *                    for cyclic(k) on a formal since the directive
   *                    was propagated by the auto-cloner.
   *                    Also used to read num-threads during dynamic affinity.
   *    lego_gen.cxx  - here
   *                    Gen_Init_Dart ()
   *                    Gen_Symbols_In_Dinfo ()
   *                    Process_Global_Distribute ()
   *                    Section_Variable_TY ()
   *    
   *    
   */

  /* struct DISTR_DIM_RT */
#ifdef _NEW_SYMTAB
  FLD_HANDLE fld = New_FLD ();
  FLD_HANDLE first_fld = fld;
  FLD_Init(fld,Save_Str("n"),Be_Type_Tbl(MTYPE_I8),0);
  FmtAssert (dart_offset_distr_n == dart_offset_flags+8,
             ("Mismatch in n offset in DISTR_DIM_RT type decl\n"));

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("p"),Be_Type_Tbl(MTYPE_I8),
           TY_size(Be_Type_Tbl(MTYPE_I8)));

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("k"),Be_Type_Tbl(MTYPE_I8),
           TY_size(Be_Type_Tbl(MTYPE_I8)) *2);

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("lb"),Be_Type_Tbl(MTYPE_I8),
           TY_size(Be_Type_Tbl(MTYPE_I8)) *3);
  Set_FLD_last_field (fld);

  TY &ty = New_TY(rt_dim_struct_ty);
  TY_Init (ty,
           dart_distr_size*TY_size(Be_Type_Tbl(MTYPE_I8)),
           KIND_STRUCT,
           MTYPE_M,
           Save_Str("DISTR_DIM_RT"));
  Set_TY_fld(ty, first_fld);
  Set_TY_align(rt_dim_struct_ty,8);
#else
  FLD *field, *next;
  field = New_FLD (4, TRUE);
  FLD_name(field)   = Save_Str("n");
  FLD_type(field)   = Be_Type_Tbl(MTYPE_I8);
  FLD_ofst(field)   = 0;
  FmtAssert (dart_offset_distr_n == dart_offset_flags+8,
             ("Mismatch in n offset in DISTR_DIM_RT type decl\n"));
  FLD_flags(field)  = 0;
  next = FLD_next(field);
  FLD_name(next)    = Save_Str("p");
  FLD_type(next)    = Be_Type_Tbl(MTYPE_I8);
  FLD_ofst(next)    = TY_size(Be_Type_Tbl(MTYPE_I8));
  FmtAssert (dart_offset_distr_p-dart_offset_distr_n == FLD_ofst(next),
             ("Mismatch in p offset in DISTR_DIM_RT type decl\n"));
  FLD_flags(next)   = 0;
  next = FLD_next(next);
  FLD_name(next)    = Save_Str("k");
  FLD_type(next)    = Be_Type_Tbl(MTYPE_I8);
  FLD_ofst(next)    = TY_size(Be_Type_Tbl(MTYPE_I8)) * 2;
  FmtAssert (dart_offset_distr_k-dart_offset_distr_n == FLD_ofst(next),
             ("Mismatch in k offset in DISTR_DIM_RT type decl\n"));
  FLD_flags(next)   = 0;
  next = FLD_next(next);
  FLD_name(next)    = Save_Str("lb");
  FLD_type(next)    = Be_Type_Tbl(MTYPE_I8);
  FLD_ofst(next)    = TY_size(Be_Type_Tbl(MTYPE_I8)) * 3;
  FmtAssert (dart_offset_distr_lb-dart_offset_distr_n == FLD_ofst(next),
             ("Mismatch in lb offset in DISTR_DIM_RT type decl\n"));
  FLD_flags(next)   = 0;

  rt_dim_struct_ty = New_TY(TRUE);
  TY_kind(rt_dim_struct_ty) = KIND_STRUCT;
  TY_btype(rt_dim_struct_ty) = MTYPE_M;
  TY_name(rt_dim_struct_ty) = Save_Str("DISTR_DIM_RT");
  TY_flist(rt_dim_struct_ty) = field;
  TY_size(rt_dim_struct_ty) = dart_distr_size*TY_size(Be_Type_Tbl(MTYPE_I8));
  TY_align(rt_dim_struct_ty) = 8;
#endif

  Enter_TY (rt_dim_struct_ty);
  distr_ty_entries[RT_dim_struct] = rt_dim_struct_ty;

  /* struct DISTR_DIM_RT* -- pointer to struct DISTR_DIM_RT */
  rt_dim_ptr_ty = Make_Pointer_Type ( rt_dim_struct_ty);
  Set_TY_ptr_as_array(rt_dim_ptr_ty);
  distr_ty_entries[RT_dim_ptr] = rt_dim_ptr_ty;

  /* Now declare an array of rt_dim_struct_ty */

#ifdef _NEW_SYMTAB
  TY_IDX arr_dim_struct_ty_idx;
  TY& arr_dim_struct_ty = New_TY(arr_dim_struct_ty_idx);
  TY_Init (arr_dim_struct_ty,
           9*TY_size(rt_dim_struct_ty),
           KIND_ARRAY,
           MTYPE_UNKNOWN,
           Save_Str("DISTR_DIM_RT_ARRAY"));
  ARB_HANDLE new_arb = New_ARB();
  ARB_Init(new_arb,0,9,TY_size(rt_dim_struct_ty));
  Set_ARB_first_dimen (new_arb);
  Set_ARB_last_dimen (new_arb);

  Set_TY_etype(arr_dim_struct_ty,rt_dim_struct_ty);
  Set_TY_arb (arr_dim_struct_ty, new_arb);
  Set_TY_align(arr_dim_struct_ty_idx,8);

#else

  TY_IDX arr_dim_struct_ty = New_TY (TRUE);
  TY_btype(arr_dim_struct_ty) = MTYPE_M;
  Set_TY_kind(arr_dim_struct_ty,KIND_ARRAY);
  ARI* new_ari = New_ARI (1, TRUE);
  ARI_etype(new_ari) = rt_dim_struct_ty;
  ARI_const_zofst(new_ari) = TRUE;
  ARI_zofst_val(new_ari) = 0;
  ARB_const_lbnd(ARI_bnd(new_ari,0)) = TRUE;
  ARB_lbnd_val(ARI_bnd(new_ari,0)) = 0;
  ARB_const_ubnd(ARI_bnd(new_ari,0)) = TRUE;
  ARB_ubnd_val(ARI_bnd(new_ari,0)) = 9; /* make it 9 for whirl2c/f */
  ARB_const_stride(ARI_bnd(new_ari,0)) = TRUE;
  ARB_stride_val(ARI_bnd(new_ari,0)) = TY_size(rt_dim_struct_ty);
  TY_arinfo(arr_dim_struct_ty) = new_ari;
  TY_align(arr_dim_struct_ty) = 8;
  TY_name(arr_dim_struct_ty) = Save_Str("DISTR_DIM_RT_ARRAY");
  Set_TY_size(arr_dim_struct_ty,9*TY_size(rt_dim_struct_ty));
  Enter_TY(arr_dim_struct_ty);
#endif

#ifdef _NEW_SYMTAB
  fld = New_FLD ();
  first_fld = fld;
  FLD_Init(fld,
           Save_Str("num_dim"),
           Be_Type_Tbl(MTYPE_I8),
           dart_offset_num_dim);
  
  fld = New_FLD ();
  FLD_Init(fld,Save_Str("element_size"),Be_Type_Tbl(MTYPE_I8),
           dart_offset_element_size);
  FmtAssert (dart_offset_element_size == TY_size(Be_Type_Tbl(MTYPE_I8)),
             ("Mismatch in element_size offset in DISTR_ARRAY_RT decl\n"));

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("flags"),Be_Type_Tbl(MTYPE_I8),dart_offset_flags);
  FmtAssert (dart_offset_flags == 2*TY_size(Be_Type_Tbl(MTYPE_I8)),
             ("Mismatch in element_size offset in DISTR_ARRAY_RT decl\n"));

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("distr"),arr_dim_struct_ty_idx,
           dart_offset_distr_n);
  Set_FLD_last_field(fld);
  FmtAssert (dart_offset_distr_n == TY_size(Be_Type_Tbl(MTYPE_I8))*3,
             ("Mismatch in element_size offset in DISTR_ARRAY_RT decl\n"));

  TY& rt_struct_ty = New_TY(rt_struct_ty_idx);
  TY_Init (rt_struct_ty,
           (dart_base_size*TY_size(Be_Type_Tbl(MTYPE_I8)) +
            TY_size(arr_dim_struct_ty)),
           KIND_STRUCT,
           MTYPE_M,
           Save_Str("DISTR_ARRAY_RT"));
  Set_TY_fld(rt_struct_ty, first_fld);
  Set_TY_align(rt_struct_ty_idx,8);

#else
  /* struct DISTR_ARRAY_RT */
  field = New_FLD (4, TRUE);
  FLD_name(field)   = Save_Str("num_dim");
  FLD_type(field)   = Be_Type_Tbl(MTYPE_I8);
  FLD_ofst(field)   = dart_offset_num_dim;
  FLD_flags(field)  = 0;
  next = FLD_next(field);
  FLD_name(next)    = Save_Str("element_size");
  FLD_type(next)    = Be_Type_Tbl(MTYPE_I8);
  FLD_ofst(next)    = dart_offset_element_size;
  FmtAssert (dart_offset_element_size == TY_size(Be_Type_Tbl(MTYPE_I8)),
             ("Mismatch in element_size offset in DISTR_ARRAY_RT decl\n"));
  FLD_flags(next)   = 0;
  next = FLD_next(next);
  FLD_name(next)    = Save_Str("flags");
  FLD_type(next)    = Be_Type_Tbl(MTYPE_I8);
  FLD_ofst(next)    = dart_offset_flags;
  FmtAssert (dart_offset_flags == 2*TY_size(Be_Type_Tbl(MTYPE_I8)),
             ("Mismatch in element_size offset in DISTR_ARRAY_RT decl\n"));
  FLD_flags(next)   = 0;
  next = FLD_next(next);
  FLD_name(next)    = Save_Str("distr");
  FLD_type(next)    = arr_dim_struct_ty;
  FLD_ofst(next)    = dart_offset_distr_n;
  FmtAssert (dart_offset_distr_n == TY_size(Be_Type_Tbl(MTYPE_I8))*3,
             ("Mismatch in element_size offset in DISTR_ARRAY_RT decl\n"));
  FLD_flags(next)   = 0;

  rt_struct_ty_idx = New_TY(TRUE);
  TY_kind(rt_struct_ty_idx) = KIND_STRUCT;
  TY_btype(rt_struct_ty_idx) = MTYPE_M;
  TY_name(rt_struct_ty_idx) = Save_Str("DISTR_ARRAY_RT");
  TY_flist(rt_struct_ty_idx) = field;
  TY_size(rt_struct_ty_idx) =
    dart_base_size*TY_size(Be_Type_Tbl(MTYPE_I8)) + TY_size(arr_dim_struct_ty);
  TY_align(rt_struct_ty_idx) = 8;
  Enter_TY (rt_struct_ty_idx);
#endif
  distr_ty_entries[RT_struct] = rt_struct_ty_idx;


  /* struct DISTR_ARRAY_RT* -- pointer to struct DISTR_ARRAY_RT */
  rt_ptr_ty = Make_Pointer_Type ( rt_struct_ty_idx);
  Set_TY_ptr_as_array(rt_ptr_ty);
  distr_ty_entries[RT_ptr] = rt_ptr_ty;
  /* Store ty of dart-ptr in the global variable dart_ptr_ty,
   * since it is used during read-pragas phase to allocate
   * ST for dart.
   */
  DART_ptr_TY = rt_ptr_ty;


  /* Now create the TY and ST entries for the runtime functions that
   * we will call.
   */

  distr_st_entries[HT_Push] =
    Declare_Func_Two_Arg (".__dsm_ht_push", "__dsm_ht_push",
                          Be_Type_Tbl(MTYPE_V),
                          voidpty, rt_ptr_ty);

  distr_st_entries[HT_Pop] =
    Declare_Func_One_Arg (".__dsm_ht_pop", "__dsm_ht_pop",
                          Be_Type_Tbl(MTYPE_V),
                          voidpty);

  distr_st_entries[HT_Top] = 
    Declare_Func_One_Arg (".__dsm_ht_top", "__dsm_ht_top",
                          rt_ptr_ty,
                          voidpty);

  distr_st_entries[HT_Check] = 
    Declare_Func_One_Arg (".__dsm_ht_check", "__dsm_ht_check",
                          rt_ptr_ty,
                          voidpty);

  distr_st_entries[HT_Replace] = 
    Declare_Func_Two_Arg (".__dsm_ht_replace", "__dsm_ht_replace",
                          Be_Type_Tbl(MTYPE_V),
                          voidpty, rt_ptr_ty);

  TY_IDX i8_ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
  distr_st_entries[Initialize_Dart] =
    Declare_Func_Two_Arg (".__dsm_initialize_dart", "__dsm_initialize_dart",
                          Be_Type_Tbl(MTYPE_V),
                          rt_ptr_ty, i8_ptr_ty);

  distr_st_entries[Allocate_Dart] =
    Declare_Func_One_Arg (".__dsm_allocate_dart", "__dsm_allocate_dart",
                           rt_ptr_ty,
                           Be_Type_Tbl(MTYPE_I4));

  distr_st_entries[Alloc_Reshape] =
    Declare_Func_One_Arg (".__dsm_alloc_reshaped_array", "__dsm_alloc_reshaped_array",
                          voidppty,
                          rt_ptr_ty);

  distr_st_entries[Dealloc_Reshape] =
    Declare_Func_Two_Arg (".__dsm_dealloc_reshaped_array", "__dsm_dealloc_reshaped_array",
                          Be_Type_Tbl(MTYPE_V),
                          rt_ptr_ty, voidppty);

  distr_st_entries[Migrate_Array] =
    Declare_Func_Two_Arg (".__dsm_migrate_array", "__dsm_migrate_array",
                          Be_Type_Tbl(MTYPE_V),
                          voidpty, rt_ptr_ty);

  distr_st_entries[Unmigrate_Array] =
    Declare_Func_Two_Arg (".__dsm_unmigrate_array", "__dsm_unmigrate_array",
                          Be_Type_Tbl(MTYPE_V),
                          voidpty, rt_ptr_ty);

  distr_st_entries[Migrate_Pages] =
    Declare_Func_Three_Arg (".__dsm_migrate_pages", "__dsm_migrate_pages",
                            Be_Type_Tbl(MTYPE_V),
                            voidpty,                    /* start address */
                            Be_Type_Tbl(MTYPE_I8),      /* size in bytes */
                            Be_Type_Tbl(MTYPE_I8));     /* threadnum     */

  distr_st_entries[mp_my_threadnum] =
    Declare_Func_Zero_Arg (".mp_my_threadnum", "mp_my_threadnum",
                          Be_Type_Tbl(MTYPE_I4));

  distr_st_entries[Proc_Pool_Push] = 
    Declare_Func_Zero_Arg (".__dsm_proc_pool_push", "__dsm_proc_pool_push",
                           Be_Type_Tbl(MTYPE_V));

  distr_st_entries[Proc_Pool_Pop] = 
    Declare_Func_Zero_Arg (".__dsm_proc_pool_pop", "__dsm_proc_pool_pop",
                           Be_Type_Tbl(MTYPE_V));

  TY_IDX ty_array[11];
  ty_array[0] = rt_ptr_ty;                 /* dart */
  ty_array[1] = Be_Type_Tbl(MTYPE_I8);     /* current dimension number */ 
  ty_array[2] = Be_Type_Tbl(MTYPE_I8);     /* my pid */
  ty_array[3] = Be_Type_Tbl(MTYPE_I8);     /* stride */
  ty_array[4] = Be_Type_Tbl(MTYPE_I8);     /* offset */
  ty_array[5] = Be_Type_Tbl(MTYPE_I8);     /* original lb */
  ty_array[6] = Be_Type_Tbl(MTYPE_I8);     /* original ub */
  ty_array[7] = Be_Type_Tbl(MTYPE_I8);     /* original step */
  ty_array[8] = i8_ptr_ty;                 /* ptr to new lb variable */
  ty_array[9] = i8_ptr_ty;                 /* ptr to new ub variable */
  ty_array[10] = i8_ptr_ty;                /* ptr to new step variable */

  distr_st_entries[Cyclic_Bounds] = 
    Declare_Func_N_Arg (".__dsm_cyclic_bounds", "__dsm_cyclic_bounds", 
                        Be_Type_Tbl(MTYPE_V),
                        11,
                        ty_array);

  distr_st_entries[Deallocate_Dart] =
    Declare_Func_One_Arg (".__dsm_deallocate_dart", "__dsm_deallocate_dart",
                           Be_Type_Tbl(MTYPE_V),
                           rt_ptr_ty);

  
  TY_IDX string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1));
  TY_IDX arg_ty[4] = {rt_ptr_ty, rt_ptr_ty, string_ty, string_ty};

  distr_st_entries[Compare_Darts] = 
    Declare_Func_N_Arg (".__dsm_ec_compare_darts", 
                        "__dsm_ec_compare_darts",
                        Be_Type_Tbl(MTYPE_V),
                        4,
                        arg_ty);


  distr_st_entries[ECHT_Push] =
    Declare_Func_Two_Arg (".__dsm_echt_push", "__dsm_echt_push",
                          Be_Type_Tbl(MTYPE_V),
                          voidpty, voidpty);

  distr_st_entries[ECHT_Pop] =
    Declare_Func_One_Arg (".__dsm_echt_pop", "__dsm_echt_pop",
                          Be_Type_Tbl(MTYPE_V),
                          voidpty);

}
