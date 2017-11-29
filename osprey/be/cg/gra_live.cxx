/*
 *  Copyright (C) 2007 QLogic Corporation.  All Rights Reserved.
 */

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


/* =======================================================================
 * =======================================================================
 *
 *  Module: gra_live.c
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/gra_live.cxx,v $
 *
 *  Description:
 *  ============
 *
 *  GRA liveness implementation.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "mempool.h"
#include "cxx_memory.h"
#include "bitset.h"
#include "cgir.h"
#include "data_layout.h"
#include "calls.h"
#include "tn_set.h"
#include "tn_list.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "gtn_tn_set.h"
#include "bb_list.h"
#include "bb_set.h"
#include "register.h"
#include "bbregs.h"
#include "gra_live.h"
#include "region_util.h"
#include "cg_region.h"
#include "cg_flags.h"
#include "tn_map.h"
#include "cg.h"
#include "cg_internal.h"
#include "whirl2ops.h"
#include "reg_live.h"
#include "cg_loop.h"
#include "pqs_cg.h"
#include "cgtarget.h"
#include "eh_region.h"

#ifdef TARG_IA64
extern TN *Caller_GP_TN;  // OSP_426, always mark Caller_GP_TN global
#endif

static BB_LIST *region_entry_list;
static BB_SET  *region_exit_set;
static MEM_POOL gra_live_pool;
static MEM_POOL gra_live_local_pool;
static MEM_POOL tn_bb_list_map_pool;
static BOOL     gra_live_pools_initialized = FALSE;
static BOOL     change;
static MEM_POOL liveness_pool;
static BOOL     liveness_pool_initialized = FALSE;
static BB_LIST *new_bbs;
static TN_SET  *tmp_live_use;
static TN_SET  *tmp_live_def;

/* We have to force these to be live on exit from each block.  They are
 * the save TNs and the RA.  If we don't force them, functions with no exits
 * will loose them -- to the dismay of debuggers, trap functions, etc. */
GTN_SET *force_live_gtns;

/*
 * The RID associated with the entire REGION being
 * analyzed by GRA_LIVE
 */
static RID *gra_live_scope_rid;

/* Provide a cheap BB set for the depth first search algorithm.  A BB is a
 * member of the set of BB_visited_mark(bb) == visited_mark.  Thus we can
 * clear out the set just by incrementing visited_mark.
 */
static UINT32   visited_mark;
static UINT32  *bb_visited_mark;

#define BB_visited_mark(bb) (bb_visited_mark[BB_id(bb)])


/* TYPE TN_BB_LIST_MAP
 *
 *      A structure type for keeping track of the BB's that have certain
 *      liveness information for each particular TN.  This representation
 *      (listing the BBs that use, define, etc each TN) should be much
 *      smaller than the TN sets for each BB.  After we have calculated
 *      the set of globally live TNs, we can translate these BB lists into
 *      the per BB TN_SETs.
 *
 */
typedef struct {
  INT32     tn_bbs_size;        /* Number of TNs in the allocated mapping */
  BB_LIST **tn_bbs;             /* Maps tn_numbers to BB_LISTs */
  TN_LIST  *tns_with_bbs;       /* List of TNs with nonempty mappings */
} TN_BB_LIST_MAP;

#define TN_BB_LIST_MAP_tn_bbs_size(x)       ((x)->tn_bbs_size)
#define TN_BB_LIST_MAP_tn_bbs(x)            ((x)->tn_bbs)
#define TN_BB_LIST_MAP_tns_with_bbs(x)      ((x)->tns_with_bbs)

/* =======================================================================
 *  Initializing
 *  ============
 *
 *      Before any GRA_LIVE computation is done, GRA_LIVE_Initialize_Memory
 *      should have been called at least once to Initialize the memory pools
 *      which belong to GRA_LIVE.  Currently, GRA_LIVE_Initialize_Memory
 *      is only accessible from GRA_LIVE_Init().
 *      GRA_LIVE_Finalize_Memory is called only from GRA_LIVE_Finish_PU.
 *      This Pops each initialized pool, and then declares them to be 
 *      uninitialized.
 *      For more details, see the description of memory pools local to this 
 *	module in gra_live.c.
 *
 *      There are four memory pools local to this module.
 *
 *           liveness_pool        
 *		Potentially used throughout the compilation of
 *              a PU including separate compilation of sub-REGIONs.
 *              This holds the liveness GTN Sets for each BB
 *
 *           gra_live_pool        
 *		Used only within a single GRA_LIVE computation
 *              to hold entry and exit info and bb_visited_mark.
 *              PUSH'd in GRA_LIVE_Region_Start, and then POP'd
 *              in GRA_LIVE_Region_Compute_Global_Live_Info.
 *
 *           tn_bb_list_map_pool  
 *		Used only within a single GRA_LIVE computation.
 *              Local info is computed into TN_BB_LIST_MAPs in this pool.
 *              The pool is PUSH'd by Check_TN_BB_LIST_MAPs_Allocated
 *              called from GRA_LIVE_Init_BB_End, and then it is POP'd
 *              by Deallocate_TN_BB_LIST_MAPs called from
 *              Distribute_TN_BB_LIST_MAPs called at the end of 
 *		Update_Local_Liveness_Info.
 *
 *           gra_live_local_pool  
 *		Two levels of this pool are used within a single 
 *		GRA_LIVE computation. The first level is used for the list 
 *		of BBs to be analyzed (new_bbs). The second level is used 
 *		within the local computation for a single BB.
 *              For this purpose the pool is PUSH'd by Init_BB_Start and then
 *              POP'd by Init_BB_End.
 *
 *       Note that each call to MEM_POOL_Initialize is followed immediately
 *       by a call to MEM_POOL_Push, because Purify works better if we keep
 *       PUSHes separate from Initializations.
 *
 * =======================================================================
 */

 
/* =======================================================================
 *
 *  TN_BB_LIST_MAP_Create
 *
 *  Create a new (empty) TN_BB_LIST_MAP and return a pointer to it.
 *
 * =======================================================================
 */
static TN_BB_LIST_MAP *
TN_BB_LIST_MAP_Create(void)
{
  return TYPE_MEM_POOL_ALLOC(TN_BB_LIST_MAP,&tn_bb_list_map_pool);
}


/* =======================================================================
 *
 *  TN_BB_LIST_MAP_TN_To_BB_LIST
 *
 *  Return the BB_LIST associated wth a particular 'tn' by 'localinfo'.
 *
 * =======================================================================
 */
static BB_LIST *
TN_BB_LIST_MAP_TN_To_BB_LIST(
  TN_BB_LIST_MAP *map,
  TN             *tn
)
{
  if ( TN_BB_LIST_MAP_tn_bbs_size(map) <= TN_number(tn) )
    return NULL;
  else
    return TN_BB_LIST_MAP_tn_bbs(map)[TN_number(tn)];
}


/* =======================================================================
 *
 *  TN_BB_LIST_MAP_Add
 *
 *  Add a new 'bb' to 'tn's associated BB_LIST in 'map'.
 *
 * =======================================================================
 */
static void
TN_BB_LIST_MAP_Add(
  TN_BB_LIST_MAP *map,
  TN             *tn,
  BB             *bb
)
{
  /* Check if the mapping from TNs to BB_LISTs is allocated and large
   * enough:
   */
  if ( TN_BB_LIST_MAP_tn_bbs_size(map) == 0 ) {
    TN_BB_LIST_MAP_tn_bbs(map) =
      TYPE_MEM_POOL_ALLOC_N(BB_LIST*,&tn_bb_list_map_pool,Last_TN + 1);
    TN_BB_LIST_MAP_tn_bbs_size(map) = Last_TN + 1;
  }
  else if ( TN_BB_LIST_MAP_tn_bbs_size(map) <= TN_number(tn) ) {
    INT32 old_size = TN_BB_LIST_MAP_tn_bbs_size(map);
    INT32 new_size = MAX(Last_TN + 1, 2 * old_size);
    TN_BB_LIST_MAP_tn_bbs(map) =
      TYPE_MEM_POOL_REALLOC_N(BB_LIST*,&tn_bb_list_map_pool,
                                       TN_BB_LIST_MAP_tn_bbs(map),
                                       old_size,
                                       new_size);
    TN_BB_LIST_MAP_tn_bbs_size(map) = new_size;
  }

  /* Keep track of the TNs that have associated BB_LISTs
   */
  if ( TN_BB_LIST_MAP_tn_bbs(map)[TN_number(tn)] == NULL ) {
    TN_BB_LIST_MAP_tns_with_bbs(map) =
      TN_LIST_Push(tn,TN_BB_LIST_MAP_tns_with_bbs(map),
                      &tn_bb_list_map_pool);
  }

  /* And finally add the BB to the TN's list:
   */
  TN_BB_LIST_MAP_tn_bbs(map)[TN_number(tn)] =
    BB_LIST_Push(bb,TN_BB_LIST_MAP_tn_bbs(map)[TN_number(tn)],
                    &tn_bb_list_map_pool);
}


/* =======================================================================
 *
 *  TN_BB_LIST_MAP_Process_Pairs
 *
 *  Invert the given 'map' by distributing the liveness information to the
 *  BB's GTN_SET representation.  For each <tn,bb> pair in the mapping.
 *
 * =======================================================================
 */
static void
TN_BB_LIST_MAP_Process_Pairs(
  TN_BB_LIST_MAP *map,
  void (*union1_fn)(BB *,TN *)
)
{
  TN_LIST *tn_list;

  for ( tn_list = TN_BB_LIST_MAP_tns_with_bbs(map);
        tn_list != NULL;
        tn_list = TN_LIST_rest(tn_list)
  ) {
    TN      *tn = TN_LIST_first(tn_list);
    BB_LIST *bb_list;

    if (TN_is_global_reg(tn)) {

      for ( bb_list = TN_BB_LIST_MAP_TN_To_BB_LIST(map,tn);
            bb_list != NULL;
            bb_list = BB_LIST_rest(bb_list)
      ) {
        BB *bb = BB_LIST_first(bb_list);

        union1_fn (bb, tn);
      }
    }
  }
}


/* Are the instances allocated and valid?
 */
static BOOL tn_bb_list_maps_allocated;


/* An instance for each kind of temporary info we want to keep:
 */
static TN_BB_LIST_MAP *tn_live_use_bbs_map;
static TN_BB_LIST_MAP *tn_live_def_bbs_map;


/* =======================================================================
 *
 *  Check_TN_BB_LIST_MAPs_Allocated
 *
 *  Make sure the three temporary local information structures have been
 *  allocated  and are valid.  If not, make it so.
 *
 * =======================================================================
 */
static void
Check_TN_BB_LIST_MAPs_Allocated(void)
{
  if ( ! tn_bb_list_maps_allocated ) {
    MEM_POOL_Push(&tn_bb_list_map_pool);
    tn_live_def_bbs_map = TN_BB_LIST_MAP_Create();
    tn_live_use_bbs_map = TN_BB_LIST_MAP_Create();
    tn_bb_list_maps_allocated = TRUE;
  }
}


/* =======================================================================
 *
 *  Deallocate_TN_BB_LIST_MAPs
 *
 *  Deallocate (and make invalid) the three temporary local information
 *  structures.
 *
 * =======================================================================
 */
static void
Deallocate_TN_BB_LIST_MAPs(void)
{
  MEM_POOL_Pop(&tn_bb_list_map_pool);
  tn_live_def_bbs_map = NULL;
  tn_live_use_bbs_map = NULL;
  tn_bb_list_maps_allocated = FALSE;
}


 /* =======================================================================
 *
 *  BB_Live_Def_Union1
 *  BB_Live_Use_Union1
 *
 *  Add the given 'tn' to the appropriate filed in 'bb'.
 *
 * =======================================================================
 */
static void
BB_Live_Def_Union1(
  BB *bb,
  TN *tn
)
{
  FmtAssert(TN_is_global_reg (tn),
            ("TN%d not in GTN Universe\n",TN_number(tn)));

  BB_live_def(bb) = GTN_SET_Union1D(BB_live_def(bb),tn,
                                    &liveness_pool);
}

static void
BB_Live_Use_Union1(
  BB *bb,
  TN *tn
)
{
  FmtAssert(TN_is_global_reg (tn),
            ("TN%d not in GTN Universe\n",TN_number(tn)));

  BB_live_use(bb) = GTN_SET_Union1D(BB_live_use(bb),tn,
                                    &liveness_pool);
}


/* =======================================================================
 *
 *  Distribute_TN_BB_LIST_MAPs
 *
 *  Invert the representation of local liveness properties.  For each TN,
 *  add it to the TN_SETs of its associated BBs for each of the three
 *  local liveness properties.  Before this is done, we have for each TN a
 *  list of the BBs in which it is defined before used.  Afterward, the
 *  information is also represented in the _live_def TN_SETs of the BBs.
 *  (And similarly for _live_use).
 *
 *  Deallocate the TN based liveness information, as we are now done with
 *  it.
 *
 * =======================================================================
 */
static void
Distribute_TN_BB_LIST_MAPs(void)
{
  if ( tn_bb_list_maps_allocated ) {
    TN_BB_LIST_MAP_Process_Pairs(tn_live_def_bbs_map, BB_Live_Def_Union1);
    TN_BB_LIST_MAP_Process_Pairs(tn_live_use_bbs_map, BB_Live_Use_Union1);

    Deallocate_TN_BB_LIST_MAPs();
  }
}



static void
Union1D_BB_Defreach_In(
  TN *tn,
  BB *bb
)
{
  FmtAssert(TN_is_global_reg (tn),
            ("TN%d not in GTN Universe\n",TN_number(tn)));

  BB_defreach_in(bb) = GTN_SET_Union1D(BB_defreach_in(bb),
                                       tn,
                                       &liveness_pool);
}


static void
Union1D_BB_Live_Out(
  TN *tn,
  BB *bb
)
{
  FmtAssert(TN_is_global_reg (tn),
            ("TN%d not in GTN Universe\n",TN_number(tn)));

  BB_live_out(bb) = GTN_SET_Union1D(BB_live_out(bb),
                                    tn,
                                    &liveness_pool);
}


/* =======================================================================
 *
 *  GRA_LIVE_Initialize_Memory
 *
 *
 * =======================================================================
 */
static void
GRA_LIVE_Initialize_Memory(void)
{
  BB *bb;

  if ( ! liveness_pool_initialized ) {
    MEM_POOL_Initialize(&liveness_pool,"MEM_Liveness_pool",TRUE);
    MEM_POOL_Push(&liveness_pool);
    liveness_pool_initialized = TRUE;
  }

  if ( ! gra_live_pools_initialized ) {
    MEM_POOL_Initialize(&gra_live_pool,"GRA_LIVE_pool",TRUE);
    MEM_POOL_Push(&gra_live_pool);
    MEM_POOL_Initialize(&gra_live_local_pool,"GRA_LIVE_local_pool",TRUE);
    MEM_POOL_Push(&gra_live_local_pool);
    MEM_POOL_Initialize(&tn_bb_list_map_pool,
                        "GRA_LIVE_tn_bb_list_map_pool",
                        TRUE);
    gra_live_pools_initialized = TRUE;
  }

  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    /* Can't calculate liveness info without a place to put it:  */
    if ( BB_bbregs(bb) == NULL )
      BB_bbregs(bb) = TYPE_MEM_POOL_ALLOC(BBREGS,&MEM_pu_pool);

    /* cause GRA_LIVE_Init_BB_Start to allocate GTN_SETs */
    BB_live_in(bb) = NULL;
  }

  new_bbs = NULL;
}

/* =======================================================================
 *
 *  GRA_LIVE_Finalize_Memory
 *
 *
 * =======================================================================
 */
static void
GRA_LIVE_Finalize_Memory(void)
{
  if ( liveness_pool_initialized ) {
    MEM_POOL_Pop (&liveness_pool);
    MEM_POOL_Delete(&liveness_pool);
    liveness_pool_initialized = FALSE;
  }
  if ( gra_live_pools_initialized ) {
    MEM_POOL_Pop(&gra_live_pool);
    MEM_POOL_Delete(&gra_live_pool);
    MEM_POOL_Pop(&gra_live_local_pool);
    MEM_POOL_Delete(&gra_live_local_pool);
    MEM_POOL_Delete(&tn_bb_list_map_pool);
    gra_live_pools_initialized = FALSE;
  }
}

/* =======================================================================
 *
 *  Force_Live_Add
 *
 *  Add <tn> to the set force_live_tns.
 *
 * =======================================================================
 */
void
Force_Live_Add( TN* tn )
{
  GTN_UNIVERSE_Add_TN(tn);
  force_live_gtns = GTN_SET_Union1D(force_live_gtns, tn, &liveness_pool);
}

/* =======================================================================
 *
 *  Force_Live_Remove
 *
 *  Remove <tn> from the set of force_live_tns.
 *
 * =======================================================================
 */
void
Force_Live_Remove( TN* tn )
{
  force_live_gtns = GTN_SET_Difference1D(force_live_gtns, tn);
}


/* =======================================================================
 *
 *  Compute_Force_TNs
 *
 *  Compute force_live_tns, the set of TNs which we have to fore to be live at
 *  the exit of each block.  The natural mechanism won't keep the save TNs
 *  live across a call if it does not reach an exit.  We need to do this so
 *  that the debugger exception handling, etc will work in these cases.
 *
 * =======================================================================
 */
static void
Compute_Force_TNs(void)
{
  INT i;

  force_live_gtns = GTN_SET_Create(Callee_Saved_Regs_Count, &liveness_pool);

  for ( i = Callee_Saved_Regs_Count - 1; i >= 0; --i ) {
    if ( TN_is_save_reg(CALLEE_tn(i)))
      Force_Live_Add(CALLEE_tn(i));
  }

  if (Find_Special_Return_Address_Symbol() == NULL &&
      RA_TN != NULL) {

    /* No horrible return address builtin was used, hence we have to force
     * the return address save TN to be live */

    Force_Live_Add(SAVE_tn(Return_Address_Reg));
  }
#ifdef TARG_MIPS
  else if (SAVE_tn(Return_Address_Reg) != NULL) {
    Force_Live_Add(SAVE_tn(Return_Address_Reg));
  }
#endif

#if !defined(TARG_X8664) && !defined(TARG_LOONGSON) && !defined(TARG_PPC32)
  // OSP_426, always mark Caller_GP_TN global
  if (Caller_GP_TN != NULL) {
    Force_Live_Add(Caller_GP_TN);
  }
#endif
}


/* =======================================================================
 *
 *  GRA_LIVE_Print_Liveness
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Print_Liveness( BB* bb )
{
  if ( GRA_LIVE_Phase_Invoked && BB_bbregs(bb) && BB_live_in(bb) ) {
    fprintf ( TFile, "  defreach_in : " );
    GTN_TN_SET_Print( BB_defreach_in(bb), TFile );
    fprintf ( TFile, "\n" );
    fprintf ( TFile, "  live_in     : " );
    GTN_TN_SET_Print( BB_live_in(bb), TFile );
    fprintf ( TFile, "\n" );
    fprintf ( TFile, "  defreach_out: " );
    GTN_TN_SET_Print( BB_defreach_out(bb), TFile );
    fprintf ( TFile, "\n" );
    fprintf ( TFile, "  live_out    : " );
    GTN_TN_SET_Print( BB_live_out(bb), TFile );
    fprintf ( TFile, "\n" );
    fprintf ( TFile, "  live_def    : " );
    GTN_TN_SET_Print( BB_live_def(bb), TFile );
    fprintf ( TFile, "\n" );
    fprintf ( TFile, "  live_use    : " );
    GTN_TN_SET_Print( BB_live_use(bb), TFile );
    fprintf ( TFile, "\n" );
  }
}

void 
GRA_LIVE_fdump_liveness(FILE *f)
{
  FILE *save_tfile=TFile;
  Set_Trace_File_internal(f);
  BB *bb;

  fprintf(TFile,"GRA_LIVE dump =======================================================\n");
  FOR_ALL_BBLIST_ITEMS(REGION_First_BB,bb) {
    fprintf(TFile,"--- BB %d ----\n",BB_id(bb));
    GRA_LIVE_Print_Liveness(bb);
  }
  fprintf(TFile,"=====================================================================\n");
  Set_Trace_File_internal(save_tfile);
}

void
GRA_LIVE_dump_liveness(void)
{
  GRA_LIVE_fdump_liveness(stdout);
}

/* =======================================================================
 *
 *  Initializing and updating the local liveness information:
 *  =========================================================
 *
 *      GRA requires that the TNs read and written in each block be
 *      presented in reverse order (this is more convenient than
 *      forward order for computing liveness, etc).  We hide the exact
 *      representation of the sequence of instructions and only
 *      present the TNs read/written as this is all the information
 *      required by GRA.
 *
 *      The schedulers may perform transfomations that change global
 *      liveness information.  For example, SWP may globalize what was
 *      a local TN by stretching its lifetime across two schedule
 *      replications.  Similarly, GCM can globalize TNs by moving
 *      their definitions or uses.  Furthermore, SWP may create
 *      entirely new BBs.
 *
 *      When any such change is made, the BB's involved need to have
 *      their liveness information recalculated.  This is done by
 *      reinitializing it with the functions given in the section above.
 *      This process recalculates the BB's local live information and
 *      perpares it for global liveness analysis.
 *
 *      The order in which the routines is called is significant.
 *      First the _Start function is called for a given block.  Then
 *      the instructions in the block are visited in reverse order.
 *      The result TNs of each instruction are first presented to the
 *      interface (using _Result_TN), followed by its operands (using
 *      _Operand_TN).  Finally, the initialization of the block is
 *      completed (useing the _End function).
 *
 *
 *      void GRA_LIVE_Init_BB_Start(BB *bb)
 *
 *          Begin initialization of a given basic block 'bb'.
 *
 *      void GRA_LIVE_Init_BB_End(BB *bb)
 *
 *          Initialization of liveness information for given block 'bb'.
 *          is complete.
 *
 * =======================================================================
 */

static void
GRA_LIVE_Init_BB_Start(BB *bb)
{
  new_bbs = BB_LIST_Push(bb,new_bbs,&gra_live_local_pool);

  MEM_POOL_Push(&gra_live_local_pool);

  tmp_live_use     = TN_SET_Create(Last_TN + 1,&gra_live_local_pool);
  tmp_live_def     = TN_SET_Create(Last_TN + 1,&gra_live_local_pool);
}



static void 
GRA_LIVE_Init_BB_End(BB *bb)
{
  TN *tn;

  Check_TN_BB_LIST_MAPs_Allocated();

  for ( tn = TN_SET_Choose(tmp_live_use);
        tn != TN_SET_CHOOSE_FAILURE;
        tn = TN_SET_Choose_Next(tmp_live_use,tn)
  ) {
    GTN_UNIVERSE_Add_TN(tn);

    TN_BB_LIST_MAP_Add(tn_live_use_bbs_map,tn,bb);
  }

  ANNOTATION *annot = ANNOT_Get(BB_annotations(bb), ANNOT_LOOPINFO);
  if (annot) {
    LOOPINFO *info = ANNOT_loopinfo(annot);
    TN *tn = LOOPINFO_trip_count_tn(info);
    if (tn != NULL && TN_is_register(tn)) {
      GTN_UNIVERSE_Add_TN(tn);
      TN_BB_LIST_MAP_Add(tn_live_use_bbs_map,tn,bb);
    }
  }

  for ( tn = TN_SET_Choose(tmp_live_def);
        tn != TN_SET_CHOOSE_FAILURE;
        tn = TN_SET_Choose_Next(tmp_live_def,tn)
  ) {
    TN_BB_LIST_MAP_Add(tn_live_def_bbs_map,tn,bb);
  }

  MEM_POOL_Pop(&gra_live_local_pool);
}


// Localize all global TNs in <bb>. 
static void 
Localize_TNs_For_BB (BB *bb)
{
  OP *op;

  FOR_ALL_BB_OPs_FWD (bb, op) {
    TN *tn;
    INT i;
    for (i = 0; i < OP_results(op); i++) {
      tn = OP_result(op, i);
      if (TN_is_constant(tn)) continue;
      if (TN_is_global_reg(tn)) Reset_TN_is_global_reg(tn);
    }
    for (i = 0; i < OP_opnds(op); i++) {
      tn = OP_opnd(op, i);
      if (TN_is_constant(tn)) continue;
      if (TN_is_global_reg(tn)) Reset_TN_is_global_reg(tn);
    }
  }
}

#define TN_is_local_reg(r)  (!(TN_is_dedicated(r) | TN_is_global_reg(r)))

// =======================================================================
//
// Detect GTNs
//
// This routine traverses all the blocks in the "new_bbs" list to find
// TNs that should be marked as a GTN. It does this by identifying
// any TN that is referenced in more than one basic block. This is 
// better than looking for live_uses (exposed uses). For predicated
// definitions, subsequent uses will always appear in the live_use 
// set even if all the definitions and uses are in the same bb.
// =======================================================================
static void 
Detect_GTNs (void)
{
  BB_LIST *bb_list;
  TN_MAP bb_for_tn = TN_MAP_Create ();  // used for detecting GTNs.

  for ( bb_list = new_bbs;
        bb_list != NULL;
        bb_list = BB_LIST_rest(bb_list)) 
  {
    BB *bb = BB_LIST_first(bb_list);

    ANNOTATION *annot = ANNOT_Get(BB_annotations(bb), ANNOT_LOOPINFO);
    if (annot) {
      LOOPINFO *info = ANNOT_loopinfo(annot);
      TN *tn = LOOPINFO_trip_count_tn(info);
      if (tn != NULL && TN_is_register(tn)) 
	GTN_UNIVERSE_Add_TN(tn);
    }

    OP *op;
    INT i;
    FOR_ALL_BB_OPs_FWD (bb, op) {
     for (i = 0; i < OP_results(op); i++) {
        TN *tn = OP_result(op, i);
	if (TN_is_local_reg(tn)) {
          BB *tn_bb = (BB *)TN_MAP_Get (bb_for_tn, tn);
          if (tn_bb != NULL) {
	    if (tn_bb != bb) 
	      GTN_UNIVERSE_Add_TN(tn);
	  }
	  else {
	    TN_MAP_Set (bb_for_tn, tn, bb);
	  }
	}
      }
      for (i = 0; i < OP_opnds(op); i++) {
	TN *tn = OP_opnd(op, i);
	if (TN_is_register(tn) && TN_is_local_reg(tn)) {
          BB *tn_bb = (BB *)TN_MAP_Get (bb_for_tn, tn);
          if (tn_bb != NULL) {
	    if (tn_bb != bb) 
	      GTN_UNIVERSE_Add_TN(tn);
	  }
	  else {
	    TN_MAP_Set (bb_for_tn, tn, bb);
	  }
	}
      }
    }
  }
  TN_MAP_Delete (bb_for_tn);
}

// =======================================================================
//
// Detect_Multiply_Defined_TNs
// Detect all GTNs which have been defined more than once and mark them
// as multiply-defined in the <multiple_defined_set> passed as a parameter.
//
// =======================================================================
static void 
Detect_Multiply_Defined_GTNs (GTN_SET *multiple_defined_set, MEM_POOL *pool)
{
  TN_MAP op_for_tn = TN_MAP_Create ();  // used for detecting GTNs.
  BB *bb;

  for (bb= REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    OP *op;
    INT i;
    FOR_ALL_BB_OPs_FWD (bb, op) {
      for (i = 0; i < OP_results(op); i++) {
        TN *tn = OP_result(op, i);

	if (TN_is_global_reg(tn)) {
	  OP *op_tn = (OP *) TN_MAP_Get (op_for_tn, tn);
	  if (op_tn && op_tn != op) {
	    multiple_defined_set = GTN_SET_Union1D (multiple_defined_set, tn, pool);
	  } else {
	    TN_MAP_Set (op_for_tn, tn, op);
	  }
	} /* TN_is_global_reg */
      } /* for .. */
    } /* FOR_ALL_BB_OPs_FWD */
  } /* for .. */
  
  TN_MAP_Delete (op_for_tn);
}

// =======================================================================
//
// GRA_LIVE_Detect_GTNs_In_Set
// Like Detect_GTNs above, but considers as its set of things to look at the
// BB's in a BB_SET. Used mainly by hyperblock if-conversion to globalize TNs
// used across BB's before doing a full update
//
// =======================================================================
void 
GRA_LIVE_Detect_GTNs_In_Set (BB_SET *bbs)
{
  BB *bb;
  TN_MAP bb_for_tn = TN_MAP_Create ();  // used for detecting GTNs.

  FOR_ALL_BB_SET_members(bbs, bb) {
    OP *op;
    INT i;
    FOR_ALL_BB_OPs_FWD (bb, op) {
      for (i = 0; i < OP_results(op); i++) {
        TN *tn = OP_result(op, i);
	if (TN_is_local_reg(tn)) {
          BB *tn_bb = (BB *) TN_MAP_Get (bb_for_tn, tn);
          if (tn_bb != NULL) {
	    if (tn_bb != bb) {
	      GTN_UNIVERSE_Add_TN(tn);
	    }
	  }
	  else {
	    TN_MAP_Set (bb_for_tn, tn, bb);
	  }
	}
	else {
	  TN_MAP_Set (bb_for_tn, tn, bb);
	}
      }

      // Check for operand usages now.
      for (i = 0; i < OP_opnds(op); i++) {
	TN *tn = OP_opnd(op, i);
	if (TN_is_register(tn) && TN_is_local_reg(tn)) {
          BB *tn_bb = (BB *) TN_MAP_Get (bb_for_tn, tn);
          if (tn_bb != NULL) {
	    if (tn_bb != bb) {
	      GTN_UNIVERSE_Add_TN(tn);
	    } 
	  }
	  else {
	    TN_MAP_Set (bb_for_tn, tn, bb);
	  }
	}
      }
    }
  }

  TN_MAP_Delete (bb_for_tn);
}


static void
GRA_LIVE_Localize_TNs (void)
{
  BB *bb;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (!BB_reg_alloc(bb) &&
        ((BB_rid( bb ) == NULL) ||
         (RID_level(BB_rid(bb)) < RL_CGSCHED))) 
    {
      Localize_TNs_For_BB (bb);
    }
  }
}

/* =======================================================================
 *
 *  Update_Local_Liveness_Info
 *
 *  At this point we may have some BBs that:
 *
 *      1. have been processed for local liveness information, and
 *
 *      2. haven't been initialized to hold gra_live data.
 *
 *  We'll initialize the newly processed BBs' to hold gra_live data.  We
 *  also need to invert the representation of local liveness information.
 *  At this point, it is represented as three BB_LISTs for each TN:
 *
 *      1. tn_live_def_bbs_map - Maps 'tn' to BBs in which 'tn' is defined
 *         above any use
 *
 *      2. tn_live_use_bbs_map - Maps 'tn' to BBs in which 'tn' is used
 *         above any def
 *
 *  In theory this representation is much smaller than the sets over the
 *  universe of all TNs that would be required to hold the information up
 *  to this point in a BB-major scheme.  Now, though we have a valid
 *  universe of global TNs (the GTN universe) and we can represent the
 *  information as sets over the universe of globally live TNs (TNs with
 *  an upward exposed use in some block).  This form is required for
 *  performing the global liveness calculations.  So we convert to that
 *  form and deallocate the TN-major representation.
 *
 * =======================================================================
 */
static void
Update_Local_Liveness_Info(void)
{
  INT32    size = GTN_UNIVERSE_size;
  BB_LIST *bb_list;

  Detect_GTNs ();

  for ( bb_list = new_bbs;
        bb_list != NULL;
        bb_list = BB_LIST_rest(bb_list)
  ) {
    BB *bb = BB_LIST_first(bb_list);

    if ( BB_bbregs(bb) == NULL ) {
      BB_bbregs(bb) = TYPE_MEM_POOL_ALLOC(BBREGS,&MEM_pu_pool);
      BB_live_in(bb) = NULL;
    }

    if ( BB_live_in(bb) == NULL ) {
      BB_live_in(bb)      = GTN_SET_Create(size,&liveness_pool);
      BB_live_out(bb)     = GTN_SET_Create(size,&liveness_pool);
      BB_defreach_in(bb)  = GTN_SET_Create(size,&liveness_pool);
      BB_defreach_out(bb) = GTN_SET_Create(size,&liveness_pool);
      BB_live_use(bb)     = GTN_SET_Create(size,&liveness_pool);
      BB_live_def(bb)     = GTN_SET_Create(size,&liveness_pool);
    }
    else {
      BB_live_in(bb)      = GTN_SET_ClearD(BB_live_in(bb));
      BB_live_out(bb)     = GTN_SET_ClearD(BB_live_out(bb));
      BB_defreach_in(bb)  = GTN_SET_ClearD(BB_defreach_in(bb));
      BB_defreach_out(bb) = GTN_SET_ClearD(BB_defreach_out(bb));
      BB_live_use(bb)     = GTN_SET_ClearD(BB_live_use(bb));
      BB_live_def(bb)     = GTN_SET_ClearD(BB_live_def(bb));
    }
  }


  /* We now have local liveness information (live_use, live_def)
   * represented as mappings from TNs to list of BBs in
   * which they have the property.  This was a fairly sparse
   * representation, but now we need to turn it into a BB -> GTN_SET
   * mapping so we can use it in the data flow equations.  Fortunately we
   * now have a valid global TN universe over which to represent the sets
   * so they won't be too expensive to represent.
   */

  if ( new_bbs != NULL ) {
    new_bbs = NULL;
    Distribute_TN_BB_LIST_MAPs();
  }
}

/* =======================================================================
 *
 *  GRA_LIVE_Set_Entries_Exits
 *
 *  Tell GRA_LIVE which BBs are entries and exits for the current REGION.
 *  If RID == NULL, assume the current REGION is the whole PU, and
 *  find entries and exits based on flow-graph properties.
 *
 * =======================================================================
 */
static void
GRA_LIVE_Set_Entries_Exits( RID *rid )
{
  CGRIN *cgrin;
  BB *bb;
  INT i,n;

  if ( rid == NULL ) {
    /*
     * whole PU
     */
    for ( bb = REGION_First_BB; bb; bb = BB_next(bb) ) {
      if ( BB_succs(bb) == NULL )
	GRA_LIVE_Region_Exit(bb);
      if ( BB_preds(bb) == NULL )
	GRA_LIVE_Region_Entry(bb);
    }
    return;
  }

  /*
   * use entry and exit info from the CGRIN
   */
  cgrin = RID_cginfo( rid );
  bb = CGRIN_entry( cgrin );
  GRA_LIVE_Region_Entry( bb );
  n = RID_num_exits( rid );
  for ( i = 0; i < n; i++ ) {
    bb = CGRIN_exit_i( cgrin, i );
    GRA_LIVE_Region_Exit( bb );
  }
  gra_live_scope_rid = rid;
}


/* =======================================================================
 *
 *  Region_Boundary_Fixup
 *
 *  For entries and exits to a PU we mark the callee-saves registers
 *  defreach_in at the entries, and live_out at the exits.  Similar
 *  treatment for sp and other special registers.
 *
 *  For entries and exits to a REGION, there may be lists of pregs
 *  available from the optimizer giving info on register variables
 *  which are live in and out of the REGION.  This info is added to
 *  the defreach_in and live_out info for the REGION entry and exit
 *  blocks respectively.
 *
 * =======================================================================
 */
static void
Region_Boundary_Fixup(void)
{
  BB *bb;
  TN_LIST *tnl;
  TN_LIST *tns_in;
  TN_LIST *tns_out;
  CGRIN *cgrin;
  INT i,n;
  TN *tn;

  if ( gra_live_scope_rid == NULL ) return;

    cgrin = RID_cginfo( gra_live_scope_rid );

    tns_in  = REGION_Get_TN_In_List (gra_live_scope_rid);
    if ( tns_in ) {
      bb = CGRIN_entry( cgrin );
      for ( tnl = tns_in; tnl; tnl = TN_LIST_rest( tnl ) ) {
	tn = TN_LIST_first( tnl );
	if ( ! TN_is_dedicated( tn ) ) {
	  BB_defreach_in( bb ) = GTN_SET_Union1D( BB_defreach_in( bb ), tn, &liveness_pool );
	}
      }
    }

    n = RID_num_exits( gra_live_scope_rid );
    for ( i = 0; i < n; i++ ) {
      bb = CGRIN_exit_i( cgrin, i );
      tns_out  = REGION_Get_TN_Out_List (gra_live_scope_rid, i);
      if (tns_out) {
	for ( tnl = tns_out; tnl; tnl = TN_LIST_rest( tnl ) ) {
	  tn = TN_LIST_first( tnl );
	  if ( ! TN_is_dedicated( tn ) ) {
	    BB_live_out( bb ) = GTN_SET_Union1D( BB_live_out( bb ), tn, &liveness_pool );
	  }
	}
      }
    }
}


#ifdef TARG_MIPS
/* =======================================================================
 *
 *  Handler_Boundary_Fixup
 *
 *  For exception handler entries to the PU, mark the callee-save registers
 *  defreach_in at the entry BB.
 *
 * =======================================================================
 */
static void
Handler_Boundary_Fixup(void)
{
  BB *bb;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (BB_handler(bb)) {
      if (SAVE_tn(Return_Address_Reg) != NULL) {
	BB_defreach_in(bb) = GTN_SET_Union1D(BB_defreach_in(bb),
					     SAVE_tn(Return_Address_Reg),
					     &liveness_pool);
      }
      if (Caller_GP_TN != NULL) {
	BB_defreach_in(bb) = GTN_SET_Union1D(BB_defreach_in(bb),
					     Caller_GP_TN,
					     &liveness_pool);
      }
    }
  }
}
#endif


/* =======================================================================
 *
 *  Live_Init
 *
 *  Initialize the global live information of 'bb'.
 *
 * =======================================================================
 */
static void
Live_Init(
  BB *bb
)
{
  /*
   * don't make save tn's live out of exit blocks.  they will be made
   * live in to the block below since they're exposed uses.  if we make
   * them live out, GRA will give the exit block greater priority than
   * it should have.
   */
  if (!BB_exit(bb)) {
    BB_live_out(bb)     = GTN_SET_CopyD(BB_live_out(bb), force_live_gtns,
					&liveness_pool);
  } else {
    BB_live_out(bb)     = GTN_SET_ClearD(BB_live_out(bb));
  }

  /* account for the implicit uses at a call site. Add $sp, $gp to the 
   * set of liveout TNs for the block.
   */
  if (BB_call(bb)) {
    GTN_UNIVERSE_Add_TN(SP_TN);
    GTN_SET_Union1D (BB_live_out(bb), SP_TN, &liveness_pool);
    if( GP_TN != NULL ){
      GTN_UNIVERSE_Add_TN(GP_TN);
      GTN_SET_Union1D (BB_live_out(bb), GP_TN, &liveness_pool);
    }
  }

  BB_defreach_in(bb)  = GTN_SET_ClearD(BB_defreach_in(bb));

  /* Initialize live_in to be the upward exposed uses.  There are two
   * reasons for this:
   *
   *    1. Prevents the need for checking for a change in
   *       the live_in set in Live_Propagate, which is harder than
   *       checking for a change in the live_out set.  We don't need to
   *       chack because the upward exposed uses are invariant in this
   *       algorithm and so doing this first ensures that they will be
   *       added to the predecessors live_out sets (causing a change) on
   *       the first iteration.
   *
   *    2. In many simple cases will obviate the need for a second
   *       iteration.
   */
  BB_live_in(bb)      = GTN_SET_CopyD(BB_live_in(bb),
                                      BB_live_use(bb),
                                      &liveness_pool);
#ifdef TARG_IA64
  /* if current PU haven't landing pad at all, 
   * needn't to added the corresponding defreach and live info
   */
  if (PU_has_exc_scopes(Get_Current_PU()) && !PU_Need_Not_Create_LSDA ()) {
    extern TN *Caller_GP_TN;
    extern TN *Caller_FP_TN;
    extern TN *Caller_Pfs_TN;
    extern TN *ra_intsave_tn;
    if (BB_handler(bb)) {
      BB_defreach_in(bb) = GTN_SET_CopyD(BB_defreach_in(bb), force_live_gtns, &liveness_pool);
      if (Caller_GP_TN) {
        BB_defreach_in(bb) = GTN_SET_Union1D(BB_defreach_in(bb), Caller_GP_TN, &liveness_pool);
        GTN_UNIVERSE_Add_TN(Caller_GP_TN);
      }
      if (ra_intsave_tn) {
        BB_defreach_in(bb) = GTN_SET_Union1D(BB_defreach_in(bb), ra_intsave_tn, &liveness_pool);
        GTN_UNIVERSE_Add_TN(ra_intsave_tn);
      }
      if (Caller_FP_TN) {
        BB_defreach_in(bb) = GTN_SET_Union1D(BB_defreach_in(bb), Caller_FP_TN, &liveness_pool);
        GTN_UNIVERSE_Add_TN(Caller_FP_TN);
      }
      if (Caller_Pfs_TN) {
        BB_defreach_in(bb) = GTN_SET_Union1D(BB_defreach_in(bb), Caller_Pfs_TN, &liveness_pool);
        GTN_UNIVERSE_Add_TN(Caller_Pfs_TN);
      }
    }
  
    if (BB_Has_Exc_Label(bb) || (!BB_exit(bb) && !BB_succs(bb))) {
      BB_live_out(bb) = GTN_SET_CopyD(BB_live_out(bb), force_live_gtns, &liveness_pool);
      if (Caller_GP_TN) {
          BB_live_out(bb) = GTN_SET_Union1D(BB_live_out(bb), Caller_GP_TN, &liveness_pool);
          GTN_UNIVERSE_Add_TN(Caller_GP_TN);
      }
      if (ra_intsave_tn) {
          BB_live_out(bb) = GTN_SET_Union1D(BB_live_out(bb), ra_intsave_tn, &liveness_pool);
          GTN_UNIVERSE_Add_TN(ra_intsave_tn);
      }
      if (Caller_FP_TN) {
          BB_live_out(bb) = GTN_SET_Union1D(BB_live_out(bb), Caller_FP_TN, &liveness_pool);
          GTN_UNIVERSE_Add_TN(Caller_FP_TN);
      }
      if (Caller_Pfs_TN) {
          BB_live_out(bb) = GTN_SET_Union1D(BB_live_out(bb), Caller_Pfs_TN, &liveness_pool);
          GTN_UNIVERSE_Add_TN(Caller_Pfs_TN);
      }      
    }
  }
#endif
  
  // We are no longer computing BB_defreach_gen. Make a quick pass 
  // through the bb and initialize the defreach_out set with the
  // the GTNs defined in the block.
  BB_defreach_out(bb)  = GTN_SET_ClearD(BB_defreach_out(bb));

  OP *op;
  INT i;
  FOR_ALL_BB_OPs_FWD (bb, op) {
    for (i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (TN_is_global_reg(tn) && !TN_is_const_reg(tn)) {
	BB_defreach_out(bb) = GTN_SET_Union1D (BB_defreach_out(bb), tn, &liveness_pool);
      }
    }
  }

  // If BB is a rotating kernel, get the livein/liveout TN from
  // its annotation!
  if (BB_rotating_kernel(bb)) {
    ANNOTATION *annot = ANNOT_Get(BB_annotations(bb), ANNOT_ROTATING_KERNEL);
    ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);
      
    INT i;
    for (i = 0; i < ROTATING_KERNEL_INFO_copyin(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_copyin(info)[i];
      BB_live_in(bb) = GTN_SET_Union1D(BB_live_in(bb), tn, &liveness_pool);
    }
    for (i = 0; i < ROTATING_KERNEL_INFO_copyout(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_copyout(info)[i];
      BB_defreach_out(bb) = GTN_SET_Union1D(BB_defreach_out(bb), tn, &liveness_pool);
    }
#ifdef TARG_IA64
    for (i = 0; i < ROTATING_KERNEL_INFO_localdef(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_localdef(info)[i];
      BB_defreach_out(bb) = GTN_SET_Union1D(BB_defreach_out(bb), tn, &liveness_pool);
    }
#endif
  }
#ifdef TARG_IA64
  // If BB is ended by function call, add the return value to the defreach-out set.
  if (BB_call (bb)) {
     ANNOTATION* annot = ANNOT_Get(BB_annotations(bb),ANNOT_CALLINFO);
     WN *call_wn = CALLINFO_call_wn(ANNOT_callinfo(annot));
     ST *call_st = (WN_operator(call_wn) != OPR_ICALL) ? WN_st(call_wn) : NULL;
     TY_IDX call_ty = (call_st != NULL) ? ST_pu_type(call_st) : WN_ty(call_wn);

     RETURN_INFO return_info = Get_Return_Info (TY_ret_type(call_ty), No_Simulated
                                    #ifdef TARG_X8664
                                     ,call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE
                                    #endif
                                    );

     for (i = 0; i < RETURN_INFO_count(return_info); i++) {
        PREG_NUM rt_preg = RETURN_INFO_preg (return_info, i);
        TYPE_ID rt_mtype = RETURN_INFO_mtype (return_info, i);
        Is_True (Preg_Is_Dedicated(rt_preg),
                 ("return value PREG (%d) is not dedicated", (INT)rt_preg));
        TN* tn = PREG_To_TN (MTYPE_TO_PREG_array[rt_mtype], rt_preg);
        GTN_UNIVERSE_Add_TN(tn);
        BB_defreach_out(bb) = GTN_SET_Union1D (BB_defreach_out(bb), tn, &liveness_pool);
     }
  }
#endif
}


/* =======================================================================
 *
 *  GRA_LIVE_Local_Live_Propagate
 *
 *  (Re)calculate BB_live_in(bb), assuming that BB_live_out(bb) is
 *  correct.
 *
 * =======================================================================
 */
void
GRA_LIVE_Local_Live_Propagate(
  BB *bb
)
{
  if ( new_bbs != NULL )
    Update_Local_Liveness_Info();

  BB_live_in(bb) = GTN_SET_CopyD(BB_live_in(bb),
                                 BB_live_out(bb),
                                 &liveness_pool);

  
  BB_live_in(bb) = GTN_SET_UnionD(GTN_SET_DifferenceD(BB_live_in(bb),
                                                      BB_live_def(bb)),
                                  BB_live_use(bb),
                                  &liveness_pool);
}


/* =======================================================================
 *
 *  GRA_LIVE_Local_Live_Propagate2
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Local_Live_Propagate2(
  BB *pred,
  BB *succ
)
{
  Update_Local_Liveness_Info();
  GRA_LIVE_Local_Live_Propagate(succ);
  BB_live_out(pred) =
    GTN_SET_CopyD(BB_live_out(pred),BB_live_in(succ),&liveness_pool);
  GRA_LIVE_Local_Live_Propagate(pred);
}


/* =======================================================================
 *
 *  Live_Propagate
 *
 *  Recalculate the live_in and live_out of 'bb' from the live_in of its
 *  successors.
 *
 * =======================================================================
 */
static void
Live_Propagate(
  BB *bb
)
{
  BBLIST *lst;
  BB     *succ;

  FOR_ALL_BB_SUCCS (bb, lst) {
    succ = BBLIST_item(lst);
    if ( ! GTN_SET_ContainsP(BB_live_out(bb),BB_live_in(succ)) ) {
      change = TRUE;
      BB_live_out(bb) = GTN_SET_UnionD(BB_live_out(bb),
                                       BB_live_in(succ),
                                       &liveness_pool);
    }
  }

  /* Don't have to check for a change here, since live_def, live_use are
   * invariant in this algorithm.
   */
  GRA_LIVE_Local_Live_Propagate(bb);
}


/* =======================================================================
 *
 *  Defreach_Propagate
 *
 *  Recalculate the defreach_in and reach_def of 'bb' from the
 *  defreach_out of its predecessors.
 *
 * =======================================================================
 */
static void
Defreach_Propagate(
  BB *bb
)
{
  BBLIST  *lst;
  BB      *pred;

  FOR_ALL_BB_PREDS (bb, lst) {
    pred = BBLIST_item(lst);
    if ( ! GTN_SET_ContainsP(BB_defreach_in(bb),BB_defreach_out(pred)) ) {
      change = TRUE;
      BB_defreach_in(bb) = GTN_SET_UnionD(BB_defreach_in(bb),
                                          BB_defreach_out(pred),
                                          &liveness_pool);
    }
  }

  BB_defreach_out(bb) = GTN_SET_UnionD(BB_defreach_out(bb),
                                       BB_defreach_in(bb),
                                       &liveness_pool);
}


/* =======================================================================
 *
 *  BB_Forward_Depth_First_Visit
 *
 *  Visit 'bb' then any unvisited successor.  See BB_Forward_Depth_First
 *  for a more detailed account.
 *
 * =======================================================================
 */
static void
BB_Forward_Depth_First_Visit(
  BB      *bb,
  BB_SET  *border_bb_set,
  void (*pre_visit_func)  (BB *bb),
  void (*post_visit_func) (BB *bb)
)
{
  BB        *bbi;
  BBLIST    *lst;

  BB_visited_mark(bb) = visited_mark;

  pre_visit_func(bb);

  if ( ! BB_SET_MemberP(border_bb_set,bb) ) {

    FOR_ALL_BB_SUCCS (bb, lst) {
      bbi = BBLIST_item(lst);
      if ( BB_visited_mark(bbi) != visited_mark ) {
        BB_Forward_Depth_First_Visit(bbi,border_bb_set,pre_visit_func,
                                                       post_visit_func);
      }
    }
  }

  post_visit_func(bb);
}


/* =======================================================================
 *
 *  Do_Nothing
 *
 *  An empty function to use as an argument to BB_Forward_Depth_First when
 *  we don't want to do anything in one of the two possible visitations
 *  positions (either post or pre).
 *
 * =======================================================================
 */
static void
Do_Nothing(
  BB *bb
)
{
}


/* =======================================================================
 *
 *  BB_Forward_Depth_First
 *
 *  start_bb_list   - List of BBs from which to start searching
 *  border_bb_set   - BB_SET to limit search to a region
 *  pre_visit_func  - Actual action to take for 'bb' before visiting its
 *                    successors
 *  post_visit_func - Actual action to take for 'bb' after visiting its
 *                    successors
 *
 *  Drive a forward depth first search for the BB's in 'start_bb_list'.
 *  The 'pre_visit_func' is invoked before visiting the successors and the
 *  'post_visit_func' is invoked afterward.  This allow us to do both pre
 *  and post order searches with a single pass.  The 'border_bb_set'
 *  limits the search.  If 'bb' is a member of 'border_bb_set', then the
 *  search stops with 'bb'.
 *
 * =======================================================================
 */
static void
BB_Forward_Depth_First(
  BB_LIST *start_bb_list,
  BB_SET  *border_bb_set,
  void (*pre_visit_func)  (BB *bb),
  void (*post_visit_func) (BB *bb)
)
{
  BB_LIST *l;

  ++visited_mark;

  for ( l = start_bb_list; l; l = BB_LIST_rest(l) ) {
    BB *bb = BB_LIST_first(l);

    if ( BB_visited_mark(bb) != visited_mark )
      BB_Forward_Depth_First_Visit(bb,border_bb_set,pre_visit_func,
                                                    post_visit_func);
  }
}


/* =======================================================================
 *
 *  GRA_LIVE_Region_Compute_Global_Live_Info
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Region_Compute_Global_Live_Info(void)
{
  Update_Local_Liveness_Info();

  bb_visited_mark = TYPE_MEM_POOL_ALLOC_N(UINT32,&gra_live_pool,
                                                 PU_BB_Count + 2);
  visited_mark = 0;

  BB_Forward_Depth_First(region_entry_list,region_exit_set,
                                           Live_Init,
                                           Do_Nothing);

  Region_Boundary_Fixup();

#ifdef TARG_MIPS
  Handler_Boundary_Fixup();	// Bug 12703
#endif

  /* We'll keep iterating so long as either the defreach or live
   * calculation changes anything.  I suppose sometimes this will mean
   * that we calculate liveness or defreachness after it has reached the
   * fixed point, but I think that will be rare and less expensive than
   * checking separately.
   */
  do {
    change = FALSE;
    BB_Forward_Depth_First(region_entry_list,region_exit_set,
                                             Defreach_Propagate,
                                             Live_Propagate);
  }
  while (change);

  if (Get_Trace(TP_FIND_GLOB, 0x1)) {
	GRA_LIVE_fdump_liveness(TFile);
  }
  /* Matches push in _Start function below.
   */
  MEM_POOL_Pop(&gra_live_pool);

}


/* =======================================================================
 *
 *  GRA_LIVE_Region_Start
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Region_Start(void)
{
  MEM_POOL_Push(&gra_live_pool);

  /* Why + 2?  Nobody seems to know.
   */
  region_exit_set  = BB_SET_Create(PU_BB_Count + 2,&gra_live_pool);

  region_entry_list = NULL;

  gra_live_scope_rid = NULL;
}


/* =======================================================================
 *
 *  GRA_LIVE_Region_Entry
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Region_Entry(BB *bb)
{
  region_entry_list = BB_LIST_Push(bb,region_entry_list,&gra_live_pool);
}


/* =======================================================================
 *
 *  GRA_LIVE_Region_Exit
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Region_Exit(BB *bb)
{
  region_exit_set  = BB_SET_Union1D(region_exit_set,bb,&gra_live_pool);
}


/* =======================================================================
 *
 *  BB_REGION_Forward_Depth_First_Visit_BB
 *
 *   Apply pre_visit_func and post_visit_func to one BB, and then
 *   recursively apply them to its un-visited successors.
 *
 * =======================================================================
 */
static void
BB_REGION_Forward_Depth_First_Visit_BB(
  BB      *bb,
  BB_VISITED_COUNTER *counter,
  void (*pre_visit_func)  (BB *bb),
  void (*post_visit_func) (BB *bb)
)
{
  counter->Set_visited(bb);
  pre_visit_func(bb);
  
  BBLIST    *lst;
  FOR_ALL_BB_SUCCS (bb, lst) {
    BB *succ = BBLIST_item(lst);
    if ( !counter->Visited(succ) )
      BB_REGION_Forward_Depth_First_Visit_BB(succ,
					     counter,
					     pre_visit_func,
					     post_visit_func);
  }

  post_visit_func(bb);
}


/* =======================================================================
 *
 *  BB_REGION_Forward_Depth_First_Visit
 *
 *    Initialize the exit blocks as visited.
 *    Apply pre_visit_func and post_visit_func in DFS preorder and postorder
 *    respectively.
 *
 * =======================================================================
 */
static void
BB_REGION_Forward_Depth_First_Visit(const BB_REGION& region,
				    BB_VISITED_COUNTER *counter,
				    void (*pre_visit_func)  (BB *bb),
				    void (*post_visit_func) (BB *bb))
{
  counter->Init();

  // Initialize BB_visited_mark to prevent visiting the exit-blocks
  INT i;
  for (i = 0; i < region.exits.size(); i++) 
    counter->Set_visited(region.exits[i]);

  // Visit all entries and their successors
  for (i = 0; i < region.entries.size(); i++) {
    if (!counter->Visited(region.entries[i])) 
      BB_REGION_Forward_Depth_First_Visit_BB(region.entries[i], counter, pre_visit_func, post_visit_func);
  }
}


/* =======================================================================
 *
 *  BB_REGION_Recompute_Global_Live_Info
 *
 *    Recompute global liveness info for the BBs in the region
 *
 * =======================================================================
 */
void
BB_REGION_Recompute_Global_Live_Info(const BB_REGION& region, BOOL recompute_local_info)
{

  MEM_POOL_Push(&gra_live_pool);

  BB_VISITED_COUNTER counter(&gra_live_pool);

#ifdef Is_True_On
  region.Verify();
#endif

  if (recompute_local_info) {
    BB_REGION_Forward_Depth_First_Visit(region, 
					&counter, 
					GRA_LIVE_Compute_Local_Info,
					Do_Nothing);

    // Must call Update_Local_Liveness_Info after calling GRA_LIVE_Compute_Local_Info
    // There are hidden states.
    Update_Local_Liveness_Info();
  }

  BB_REGION_Forward_Depth_First_Visit(region, 
				      &counter, 
				      Live_Init,
				      Do_Nothing);

  /* We'll keep iterating so long as either the defreach or live
   * calculation changes anything.  I suppose sometimes this will mean
   * that we calculate liveness or defreachness after it has reached the
   * fixed point, but I think that will be rare and less expensive than
   * checking separately.
   */
  do {
    change = FALSE;
    BB_REGION_Forward_Depth_First_Visit(region, 
					&counter, 
					Defreach_Propagate,
					Live_Propagate);
  }
  while (change);

  if (Get_Trace(TP_FIND_GLOB, 0x1)) {
	GRA_LIVE_fdump_liveness(TFile);
  }
  MEM_POOL_Pop(&gra_live_pool);
}


/* =======================================================================
 *
 *  GRA_LIVE_Add_Live_In_GTN
 *  GRA_LIVE_Remove_Live_In_GTN
 *  GRA_LIVE_Add_Live_Out_GTN
 *  GRA_LIVE_Remove_Live_Out_GTN
 *  GRA_LIVE_Add_Defreach_In_GTN
 *  GRA_LIVE_Remove_Defreach_In_GTN
 *  GRA_LIVE_Add_Defreach_Out_GTN
 *  GRA_LIVE_Remove_Defreach_Out_GTN
 *  GRA_LIVE_Add_Live_Use_GTN
 *  GRA_LIVE_Remove_Live_Use_GTN
 *  GRA_LIVE_Add_Live_Def_GTN
 *  GRA_LIVE_Remove_Live_Def_GTN
 *
 *  See interface description.
 *
 * =======================================================================
 */

void
GRA_LIVE_Add_Live_In_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_live_in(bb) = GTN_SET_Union1D(BB_live_in(bb),tn,&liveness_pool);
}

void
GRA_LIVE_Remove_Live_In_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_live_in(bb) = GTN_SET_Difference1D(BB_live_in(bb),tn);
}

void
GRA_LIVE_Add_Live_Out_GTN( BB* bb, TN* tn )
{
  BB_live_out(bb) = GTN_SET_Union1D(BB_live_out(bb),tn,&liveness_pool);
}

void
GRA_LIVE_Remove_Live_Out_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_live_out(bb) = GTN_SET_Difference1D(BB_live_out(bb),tn);
}

void
GRA_LIVE_Add_Defreach_In_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_defreach_in(bb) =
    GTN_SET_Union1D(BB_defreach_in(bb),tn,&liveness_pool);
}

void
GRA_LIVE_Remove_Defreach_In_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_defreach_in(bb) = GTN_SET_Difference1D(BB_defreach_in(bb),tn);
}


void
GRA_LIVE_Add_Defreach_Out_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_defreach_out(bb) =
    GTN_SET_Union1D(BB_defreach_out(bb),tn,&liveness_pool);
}

void
GRA_LIVE_Remove_Defreach_Out_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_defreach_out(bb) = GTN_SET_Difference1D(BB_defreach_out(bb),tn);
}


void
GRA_LIVE_Add_Live_Use_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_live_use(bb) =
    GTN_SET_Union1D(BB_live_use(bb),tn,&liveness_pool);
}

void
GRA_LIVE_Remove_Live_Use_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_live_use(bb) = GTN_SET_Difference1D(BB_live_use(bb),tn);
}

void
GRA_LIVE_Add_Live_Def_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_live_def(bb) =
    GTN_SET_Union1D(BB_live_def(bb),tn,&liveness_pool);
}

void
GRA_LIVE_Remove_Live_Def_GTN( BB* bb, TN* tn )
{
  Is_True(TN_is_global_reg(tn), ("TN is not GTN."));
  BB_live_def(bb) = GTN_SET_Difference1D(BB_live_def(bb),tn);
}


/* =======================================================================
 *
 *  GRA_LIVE_Merge_Blocks
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Merge_Blocks( BB *dst, BB *a, BB *b )
{
  GTN_SET *live_use;
  GTN_SET *live_def;


  /* The 'out' vectors of the merged block are the 'out' vectors of the
   * block being merged in.
   */
  BB_live_out(dst) = GTN_SET_CopyD(BB_live_out(a),BB_live_out(b),&liveness_pool);
  BB_defreach_out(dst) = GTN_SET_CopyD(BB_defreach_out(a),BB_defreach_out(b),&liveness_pool);

  live_use = GTN_SET_DifferenceD(GTN_SET_Copy(BB_live_use(b),&liveness_pool),
				 BB_live_def(a));
  live_def = GTN_SET_DifferenceD(GTN_SET_Copy(BB_live_def(b),&liveness_pool),
				 BB_live_use(a));
  BB_live_use(dst) = GTN_SET_UnionD(live_use,BB_live_use(a),&liveness_pool);
  BB_live_def(dst) = GTN_SET_UnionD(live_def,BB_live_def(a),&liveness_pool);
}

/* =======================================================================
 *
 *  GRA_LIVE_Compute_Liveness_For_BB
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Compute_Liveness_For_BB(BB *bb)
{
  GRA_LIVE_Compute_Local_Info(bb);
  GRA_LIVE_Region_Start();
  GRA_LIVE_Region_Entry(bb);
  GRA_LIVE_Region_Exit(bb);
  GRA_LIVE_Region_Compute_Global_Live_Info();
}

/* =======================================================================
 *
 *  GRA_LIVE_Init
 *
 * =======================================================================
 */
void
GRA_LIVE_Init( RID *rid )	/* RID is forced to be NULL for PU */
{
  BB   *bb;

  GRA_LIVE_Phase_Invoked = TRUE;        // flag to mark invocation

  GRA_LIVE_Init_Loop(NULL, NULL, NULL, NULL, NULL);

  GRA_LIVE_Initialize_Memory();
  if (rid) {
   /* we defer handling the saved tn's for regions until we handle the PU
    * (actually, we only care about handling them at returns that may be
    * contained within the region).  */
   force_live_gtns = GTN_SET_Create_Empty(1, &liveness_pool);
  } 
  else if (!force_live_gtns) Compute_Force_TNs();

  if (GRA_LIVE_Predicate_Aware) {
    PQSCG_reinit(REGION_First_BB);
  }

  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    GRA_LIVE_Compute_Local_Info(bb);
  }

  GRA_LIVE_Region_Start();
  GRA_LIVE_Set_Entries_Exits( rid );
  GRA_LIVE_Region_Compute_Global_Live_Info();

}

/* =======================================================================
 *
 *  GRA_LIVE_Recalc_Liveness
 *
 * =======================================================================
 */
void
GRA_LIVE_Recalc_Liveness( RID *rid )	/* RID is forced to be NULL for PU */
{

  if (rid) {
    GRA_LIVE_Finish_REGION();
  } else {
    GTN_UNIVERSE_Pu_End ();
    GTN_UNIVERSE_Pu_Begin();
    GRA_LIVE_Finalize_Memory(); 
  }

  force_live_gtns = NULL;
  GRA_LIVE_Localize_TNs ();
  GRA_LIVE_Init(rid);  
}

/* =======================================================================
 *
 *  GRA_LIVE_Finish_PU
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Finish_PU(void)
{
  force_live_gtns = NULL;
  GRA_LIVE_Phase_Invoked = FALSE;  
  GRA_LIVE_Finalize_Memory();
}

/* =======================================================================
 *
 *  GRA_LIVE_Finish_REGION
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Finish_REGION(void)
{
  force_live_gtns = NULL;
  GRA_LIVE_Phase_Invoked = FALSE;
  return;
}


/* =======================================================================
 *
 *  GRA_LIVE_Init_Loop
 *
 *  See interface description.
 *
 * =======================================================================
 */
static BB *prolog_bb;
static BB *body_bb;
static BB *epilog_bb;
static CG_LOOP_BACKPATCH *prolog_bp;
static CG_LOOP_BACKPATCH *epilog_bp;

void GRA_LIVE_Init_Loop(BB *pbb, BB *bbb, BB *ebb, CG_LOOP_BACKPATCH *pbp, CG_LOOP_BACKPATCH *ebp)
{
  body_bb = bbb;
  prolog_bb = pbb;
  prolog_bp = pbp;
  epilog_bb = ebb;
  epilog_bp = ebp;
}


/* =======================================================================
 *
 *  GRA_LIVE_Fini_Loop
 *
 *  See interface description.
 *
 * =======================================================================
 */
void GRA_LIVE_Fini_Loop()
{
  body_bb = (BB*)NULL;
  prolog_bb = (BB*)NULL;
  epilog_bb = (BB*)NULL;
  prolog_bp = (CG_LOOP_BACKPATCH*)NULL;
  epilog_bp = (CG_LOOP_BACKPATCH*)NULL;
}


// Utility to get (and allocate if necessary) the TN maps for def sets and use sets

static PQS_TN_SET * get_usedef_set(TN_MAP map, TN *tn) {
  PQS_TN_SET * def_set;
  def_set = (PQS_TN_SET *) TN_MAP_Get(map,tn);
  if (!def_set) {
    def_set = CXX_NEW(PQS_TN_SET,&PQS_mem_pool);
    TN_MAP_Set(map,tn,def_set);
  }
  return def_set;
}



/* =======================================================================
 *
 *  GRA_LIVE_Compute_Local_Info
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
GRA_LIVE_Compute_Local_Info(
  BB *bb
)
{
  INT32 i;
  OP   *op;
  RID *bbrid;
  CGRIN *cgrin;
  TN_LIST *tns_in, *tnl;
  TN *tn;
  TN *pred_tn;
  PQS_TN_SET *def_set;
  PQS_TN_SET *use_set;
  TN_MAP def_map;
  TN_MAP use_map;
  BOOL sub_from_use;
  BOOL add_to_def;

  GRA_LIVE_Init_BB_Start(bb);

  if (bb == prolog_bb) {
    for (CG_LOOP_BACKPATCH *bp = prolog_bp; bp != NULL; bp = CG_LOOP_Backpatch_Next(bp)) {
      TN *def_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN *use_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
      tmp_live_def = TN_SET_Union1D(tmp_live_def,def_tn,&gra_live_local_pool);
      tmp_live_use = TN_SET_Difference1D(tmp_live_use,def_tn);
      tmp_live_use = TN_SET_Union1D(tmp_live_use,use_tn,&gra_live_local_pool);
    }
  }

  /* process all ops in reverse order */
  if (GRA_LIVE_Predicate_Aware && PQSCG_pqs_valid()) {
    MEM_POOL_Push(&PQS_mem_pool);
    def_map = TN_MAP_Create();
    use_map = TN_MAP_Create();
    for ( op = BB_last_op(bb); op != NULL; op = OP_prev(op) ) {
      // If there is no predicate TN, pretend it's the TRUE predicate
      if (OP_has_predicate(op)) {
	pred_tn = OP_opnd(op, OP_PREDICATE_OPND);
	// Predicates behave as if they are used under the TRUE predicate
	use_set = get_usedef_set(use_map,pred_tn);
	use_set->Insert(True_TN);
      } else {
	pred_tn = True_TN;
      }
      
      for ( i = OP_results(op) - 1; i >= 0; --i ) {
	TN *result_tn = OP_result(op, i);
	if (TN_is_register(result_tn) && !TN_is_const_reg(result_tn)) {
	  //
	  // Add the current predicate TN to the def set, or if there is no predicate
	  // TN, add P0.
	  //
	  def_set = get_usedef_set(def_map,result_tn);
	  use_set = get_usedef_set(use_map,result_tn);
	  //
	  // We need to check to make sure that the result is actually
	  // set if the qualifying predicate is TRUE.
	  //
	  if (PQSCG_sets_results_if_qual_true(op)) {
	    def_set->Insert(pred_tn);
	  } else {
	    // Treat this as both a use and an unconditional def
	    // clear both predicate sets
	    tmp_live_use = TN_SET_Union1D(tmp_live_use,result_tn,&gra_live_local_pool);
	    tmp_live_def = TN_SET_Union1D(tmp_live_def,result_tn,&gra_live_local_pool);
	    def_set->Clear();
	    use_set->Clear();
	    continue;
	  }

	  //
	  // If the defs cover the uses, remove it from tmp_live_use.
	  // If the def is unconditional, add it to tmp_live_def
	  //
	  sub_from_use = PQSCG_is_subset_of(*use_set, *def_set);
	  add_to_def = PQSCG_is_subset_of(True_TN, *def_set);
	  if (sub_from_use) {
	    tmp_live_use = 
	      TN_SET_Difference1D(tmp_live_use,result_tn);
	  }
	  if (add_to_def) {
	    tmp_live_def = 
	      TN_SET_Union1D(tmp_live_def,result_tn,&gra_live_local_pool);
	  }
	  if (add_to_def || sub_from_use) {
	    // Clear out the def and use sets (i.e. start afresh)
	    use_set->Clear();
	    def_set->Clear();
	  }
	}
      }
	  
      
      for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
	TN *opnd_tn = OP_opnd(op, i);
	if (TN_is_register(opnd_tn) && !TN_is_const_reg(opnd_tn)) {
	  // Add to the use_set, clear the def_set
	  tmp_live_use = TN_SET_Union1D(tmp_live_use,opnd_tn,&gra_live_local_pool);
	  use_set = get_usedef_set(use_map,opnd_tn);
	  use_set->Insert(pred_tn);

	  tmp_live_def = TN_SET_Difference1D(tmp_live_def,opnd_tn);
	  def_set = get_usedef_set(def_map,opnd_tn);
	  def_set->Clear();
	}
      }
    }
    TN_MAP_Delete(def_map);
    TN_MAP_Delete(use_map);
    MEM_POOL_Pop(&PQS_mem_pool);
    
  } else {
    // Non-predicate aware form
    for ( op = BB_last_op(bb); op != NULL; op = OP_prev(op) ) {
      
      for ( i = OP_results(op) - 1; i >= 0; --i ) {
	TN *result_tn = OP_result(op, i);
	if (TN_is_register(result_tn) && !TN_is_const_reg(result_tn)) {
	  tmp_live_def = 
	    TN_SET_Union1D(tmp_live_def,result_tn,&gra_live_local_pool);
	  
	  // For conditional def ops (until not fully predicate aware),
	  // add the result TNs to live_use sets.
	  if (OP_cond_def(op))  {
	    tmp_live_use = 
	      TN_SET_Union1D(tmp_live_use,result_tn,&gra_live_local_pool);
	  } else {
	    tmp_live_use = 
	      TN_SET_Difference1D(tmp_live_use,result_tn);
	  }
	}
      }
      
      for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
	TN *opnd_tn = OP_opnd(op, i);
	if (TN_is_register(opnd_tn) && !TN_is_const_reg(opnd_tn)) {
	  tmp_live_use = TN_SET_Union1D(tmp_live_use,opnd_tn,&gra_live_local_pool);
	  tmp_live_def = TN_SET_Difference1D(tmp_live_def,opnd_tn);
	}
      }
    }
  }

  if (bb == body_bb) {
    OP *op;
    FOR_ALL_BB_OPs(bb, op) {
      for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
	TN *tn = OP_opnd(op,opnd);
	if (Is_CG_LOOP_Op(op) &&
	    OP_omega(op, opnd) != 0)
	  tmp_live_use = TN_SET_Union1D(tmp_live_use,tn,&gra_live_local_pool);
      }
    }
  }

  if (bb == epilog_bb) {
    for (CG_LOOP_BACKPATCH *bp = epilog_bp; bp != NULL; bp = CG_LOOP_Backpatch_Next(bp)) {
      TN *use_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN *def_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
      tmp_live_def = TN_SET_Union1D(tmp_live_def,def_tn,&gra_live_local_pool);
      tmp_live_use = TN_SET_Difference1D(tmp_live_use,def_tn);
      tmp_live_use = TN_SET_Union1D(tmp_live_use,use_tn,&gra_live_local_pool);
    }
  }

  /*
   * The tns_in for a previously compiled REGION entry
   * should be added to tmp_live_use, because although the uses
   * may have been removed, we still want to consider them
   * as globals.  Example: callee_saved_register save tns.
   */

  if (    ( bbrid = BB_rid( bb ) )
       && ( RID_level( bbrid ) >= RL_CGSCHED )
       && ( cgrin = RID_cginfo( bbrid ) )
       && ( bb == CGRIN_entry( cgrin ) )
       && ( tns_in = CGRIN_tns_in( cgrin ) ) ) {

    for ( tnl = tns_in; tnl; tnl = TN_LIST_rest( tnl ) ) {
      tn = TN_LIST_first( tnl );
      tmp_live_use = TN_SET_Union1D(tmp_live_use,tn,&gra_live_local_pool);
    }
  }

  // If BB is a rotating kernel, get the livein/liveout TN from
  // its annotation!
  if (BB_rotating_kernel(bb)) {
    ANNOTATION *annot = ANNOT_Get(BB_annotations(bb), ANNOT_ROTATING_KERNEL);
    ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);
      
    INT i;
    for (i = 0; i < ROTATING_KERNEL_INFO_copyin(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_copyin(info)[i];
      tmp_live_use = TN_SET_Union1D(tmp_live_use,tn,&gra_live_local_pool);
      
    }
    for (i = 0; i < ROTATING_KERNEL_INFO_copyout(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_copyout(info)[i];
      tmp_live_def = TN_SET_Union1D(tmp_live_def,tn,&gra_live_local_pool);
    }
#ifdef TARG_IA64
    for (i = 0; i < ROTATING_KERNEL_INFO_localdef(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_localdef(info)[i];
      tmp_live_def = TN_SET_Union1D(tmp_live_def,tn,&gra_live_local_pool);
    }
#endif
  }

  GRA_LIVE_Init_BB_End(bb);
}



BOOL GRA_LIVE_TN_Live_Outof_BB (TN *tn, BB *bb)
{
  if (TN_is_global_reg(tn) && 
      GTN_SET_Intersection_MemberP(BB_live_out(bb), BB_defreach_out(bb), tn)) 
  {
    return TRUE;
  }

  if (TN_is_dedicated (tn)) {
    return REG_LIVE_Implicit_Use_Outof_BB (TN_register_class(tn), 
					   TN_register(tn), 
					   bb);
  }
  return FALSE;
}


BOOL GRA_LIVE_TN_Live_Into_BB (TN *tn, BB *bb)
{
  if (TN_is_global_reg(tn) && 
      GTN_SET_Intersection_MemberP (BB_live_in(bb), BB_defreach_in(bb), tn))
  {
    return TRUE;
  }
  if (TN_is_dedicated (tn)) {
    return REG_LIVE_Implicit_Def_Into_BB (TN_register_class(tn), 
					  TN_register(tn), 
					  bb);
  }
  return FALSE;
}


// Replace <tn> in the range of OPs from <op1> to <op2> by a new tn.
// The range does not include the operands of op1 and the results of op2.
// (it does include the results of op1 and the operands of op2)
// If op2 is NULL, the range is till the end of the bb.
static void
Rename_TN_In_Range (TN *tn, OP *op1, OP *op2)
{
  TN *new_tn = Dup_TN (tn);
  OP *op = op1;
  INT i;
  
#ifdef TARG_IA64
  if (Get_Trace(TP_CGPREP, 0x8))
#else
  if (Get_Trace(TP_FIND_GLOB, 0x8))
#endif
    fprintf (TFile, "<Rename_TNs> TN%d renamed to TN%d in BB:%d\n",
	     TN_number(tn), TN_number(new_tn), BB_id(OP_bb(op1)));
  
  do {
    for (i = 0; i < OP_results(op); i++) {
      if (OP_result(op, i) == tn) {
        Set_OP_result (op, i, new_tn);
      }
    }
    op = OP_next(op);
    if (op == NULL) break;
    for (i = 0; i < OP_opnds(op); i++) {
      if (OP_opnd(op, i) == tn) {
        Set_OP_opnd (op, i, new_tn);
      }
    }
  } while (op != op2);
}


/* =======================================================================
 *
 *  Clear_Defreach
 *
 *  Clear the deefreach_in and defreach_out for blocks that are visited.
 *
 * =======================================================================
 */
static TN *defreach_tn;
static void
Clear_Defreach(
  BB *bb
)
{
  if (GTN_SET_MemberP(BB_defreach_in(bb), defreach_tn))
    GRA_LIVE_Remove_Defreach_In_GTN(bb, defreach_tn);

  if (GTN_SET_MemberP(BB_defreach_out(bb), defreach_tn)) 
    GRA_LIVE_Remove_Defreach_Out_GTN(bb, defreach_tn);
}



// Detect TNs that should be renamed in the <bb>. 
void 
Rename_TNs_For_BB (BB *bb, GTN_SET *multiple_defined_set
#ifdef KEY
		   , OP *rename_local_TN_op
#endif
		   )
{
  TN_MAP op_for_tn = TN_MAP_Create ();
  OP *op;
#ifdef KEY
  BOOL rename_local_TNs = FALSE;
#endif

  FOR_ALL_BB_OPs_FWD (bb, op) {
#ifdef KEY
    // Rename local TNs starting at rename_local_TN_op, if it exists,
    // Bug 4327.
    rename_local_TNs |= (rename_local_TN_op == op);
#endif
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      // Don't rename under the following conditions.
      if (TN_is_dedicated(tn) || OP_cond_def(op) || 
#ifdef TARG_X8664
          TN_is_norename(tn) || OP_res_norename(op) || 
#endif
          OP_same_res(op)) continue;

      OP *last_def = (OP *) TN_MAP_Get (op_for_tn, tn);
#ifdef TARG_LOONGSON
      if ((last_def != NULL) && (OP_code(op) != TOP_ldl && OP_code(op) != TOP_lwl))
#else
      if (last_def != NULL)
#endif
      {
        // rename tn to new_tn between last_def and op.
        Rename_TN_In_Range (tn, last_def, op);
      }
#ifdef TARG_IA64
      else if (tn == Caller_GP_TN) {
        // OSP_426, Don't rename the caller GP TN, keep it global
      }
#endif
#ifdef TARG_MIPS
      else if (PU_Has_Exc_Handler &&
	       (tn == Caller_GP_TN ||
	        tn == SAVE_tn(Return_Address_Reg))) {
	// Don't rename the caller GP TN and the saved return address TN.  They
	// should remain global.
      }
#endif
      else if (TN_is_global_reg(tn) &&
	       !TN_is_const_reg(tn) &&
	       !GTN_SET_MemberP(BB_live_out(bb), tn)) {

        // rename GTN to new local TN between op and end of bb.
	Rename_TN_In_Range (tn, op, NULL);

	// #762083: Remove the GTN from the live-def set of <bb> now.
	GRA_LIVE_Remove_Live_Def_GTN(bb, tn);
	GRA_LIVE_Remove_Defreach_Out_GTN(bb, tn);
	Force_Live_Remove(tn);

	if (multiple_defined_set && !TN_is_constant(tn) &&
	    !GTN_SET_MemberP(BB_live_in(bb), tn) &&
	    !GTN_SET_MemberP(multiple_defined_set, tn)) {

	  // Before, we reset the global TN flag, we need to make sure that this
	  // TN doesn't exist in live-sets for any BB.

	  // ONLY need to check for defreach_in and defreach_out sets. 
#ifdef TARG_IA64
          BB *cur_bb;
          for (cur_bb = bb; cur_bb != NULL; cur_bb = BB_next(cur_bb)) {
            if (GTN_SET_MemberP(BB_defreach_in(cur_bb), tn))
              GRA_LIVE_Remove_Defreach_In_GTN(cur_bb, tn);

            if (GTN_SET_MemberP(BB_defreach_out(cur_bb), tn))
              GRA_LIVE_Remove_Defreach_Out_GTN(cur_bb, tn);
          }
#else // TARG_IA64
          defreach_tn = tn;
#ifndef KEY
	  // Purify_pools (trace) on exposes the MEM_POOL bug. The bug is that
	  // the following code should be using MEM_local_pool - look at caller
	  // - instead of gra_live_pool which is already popped out. Bug #24
	  // can expose this problem.
          BB_VISITED_COUNTER counter_data(&gra_live_pool);
#else
          BB_VISITED_COUNTER counter_data(&MEM_local_pool);
#endif
          BB_VISITED_COUNTER *counter = &counter_data;
          counter->Init();
          BB_REGION_Forward_Depth_First_Visit_BB (bb, counter, Clear_Defreach, Do_Nothing);
#ifndef KEY
	  // see comments above. Actually, this could be a real bug because
	  // gra_live_pool is already popped out before the caller calls this 
	  // function. The bug was not exposed.
          MEM_POOL_Pop(&gra_live_pool);
#endif
#endif // TARG_IA64
	  Reset_TN_is_global_reg (tn);

	}

      }
#ifdef KEY
      else if (rename_local_TNs &&
	       !TN_is_global_reg(tn) &&
	       !TN_is_const_reg(tn)) {
        // Rename local TN to new local TN between op and end of bb.  Bug 4327.
	Rename_TN_In_Range (tn, op, NULL);
      }
#endif
      TN_MAP_Set (op_for_tn, OP_result(op, i), op);
    }
  }
  TN_MAP_Delete (op_for_tn);
}


void GRA_LIVE_Rename_TNs (void)
{
  BB *bb;

  MEM_POOL_Push (&MEM_local_pool);
  GTN_SET *multiple_defined_set = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
  Detect_Multiply_Defined_GTNs (multiple_defined_set, &MEM_local_pool);
 
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (!BB_reg_alloc(bb) &&
        ((BB_rid( bb ) == NULL) ||
         (RID_level(BB_rid(bb)) < RL_CGSCHED))) 
    {
      Rename_TNs_For_BB (bb, multiple_defined_set);
    }
  }

  MEM_POOL_Pop (&MEM_local_pool);
}
