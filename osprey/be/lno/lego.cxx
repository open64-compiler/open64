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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <alloca.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "wn.h"
#include "lego.h"
#include "cxx_memory.h"
#include "lego_pragma.h"
#include "lego_gen.h"
#include "array_lower.h"
#include "tracing.h"
#include "lnopt_main.h"
#include "ir_reader.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "irbdata.h"
#include "stblock.h"
#include "tile.h"
#include "data_layout.h"
#include "permute.h"
#include "soe.h"
#include "cond.h"
#include "strtab.h"
#include "ff_pragmas.h"

/***********************************************************************
 *
 * Extern declarations.
 *
 ***********************************************************************/
extern ST* Section_Variable_ST (char* name, TY_IDX daty, BOOL is_global);
extern void Do_Loop_Explicit_Affinity (WN* loop);
extern char* Cur_PU_Name;
extern void Whack_Do_Loops (WN* func_nd);

extern BOOL Run_Dsm_Check;
extern void EC_Array_Portion_Calls (WN* func_nd);
extern void Parallel_And_Padding_Phase(PU_Info* current_pu, WN* func_nd);

/***********************************************************************
 *
 * Externally visible declarations.
 *
 ***********************************************************************/

extern "C" {
  void Lego_File_Init (void);
  void Mp_File_Init (void);
  void Lego_File_Fini (void);
}
extern void Lego_OZero_Driver(PU_Info* current_pu, WN* func_nd);
extern void Lego_Read_Pragmas(WN* func_nd);
extern void Lego_Lower_Pragmas(WN* func_nd);
extern void Lego_PU_Init(void);
extern void Lego_PU_Fini(void);

extern INT32 da_count;
INT32 da_count = 0;

extern MEM_POOL LEGO_memory_pool, *LEGO_pool;
MEM_POOL LEGO_memory_pool, *LEGO_pool;

extern BOOL Debug_Lego;
extern BOOL Verbose_Lego;
BOOL Debug_Lego = FALSE;
BOOL Verbose_Lego = FALSE;

extern BOOL disable_divmod_opts;
BOOL disable_divmod_opts = FALSE;

extern BOOL disable_rr_maps;
BOOL disable_rr_maps = TRUE;

// This map exists only during PUs that contain mp-needs-lno.
// It applies only to div/rem operators.
// If a div/rem node has a non-NULL value for this map, then that
// node is speculatable (modulo DU-chains, of course).
extern WN_MAP Safe_Spec_MAP;
WN_MAP Safe_Spec_Map = WN_MAP_UNDEFINED;

extern WN_MAP RR_Map;
WN_MAP RR_Map = WN_MAP_UNDEFINED;

BOOL PU_has_reshaped_commons = FALSE;

/***********************************************************************
 *
 * Static declarations.
 *
 ***********************************************************************/
static INT64 pu_count = 0;
static void Dealloc_HashTable_Entries ();
static void Delete_Distr_Pragmas (DISTR_ARRAY* dact);


/***********************************************************************
 *
 * Walk the hash table of compile-time distributed array descriptors,
 * and deallocate each of them.
 *
 ***********************************************************************/
static void Dealloc_HashTable_Entries () {
  for (INT i=0; i<da_stack->Elements(); i++) {
    DISTR_INFO* dinfo = da_stack->Bottom_nth(i);
#ifdef Is_True_On
    // Check: it must also be present in the hash-table
    Is_True (da_hash->Find(dinfo->Array_ST()) == dinfo,
             ("Mismatch between dinfo stack and hash-table"));
    da_hash->Remove(dinfo->Array_ST());
#endif
    CXX_DELETE (dinfo, LEGO_pool);
  }

#ifdef Is_True_On
  {
    // Check: the hash-table must now be empty
    HASH_TABLE_ITER<ST*, DISTR_INFO*> iter (da_hash);
    ST* st;
    DISTR_INFO* dinfo;
    while (iter.Step (&st, &dinfo)) {
      FmtAssert (FALSE, ("Hash-table is not empty. Has array %s!",
                         ST_name(st)));
    }
  }
#endif
}

/***********************************************************************
 *
 * Read the data distribution pragmas, build up internal representation.
 *
 ***********************************************************************/
extern void Lego_Read_Pragmas (WN* func_nd) {

  if (Debug_Lego) printf ("===== DSM Processing function %s =====\n",
                          Cur_PU_Name);

  Read_Distr_Pragmas (func_nd);

  if (Debug_Lego) {
    printf("===== Done Reading Pragmas ====\n");
    // Dump_WN(func_nd, stdout, 1);
    // dump_tree(func_nd);
    printf("===== Begin Sanity Tests ====\n");
    printf("(**) Du_Sanity_Check\n");
    Du_Sanity_Check(func_nd);
#ifdef Is_True_On
    printf("(**) MP_Sanity_Check\n");
    MP_Sanity_Check_Func(func_nd);
#endif
    printf("(**) LWN_Check_Parentize\n");
    LWN_Check_Parentize(func_nd);
  }
}

/***********************************************************************
 *
 * Determine the tiling and peeling factors to optimize references
 * to reshaped arrays.
 *
 ***********************************************************************/
extern void Lego_Compute_Tile_Peel (WN* func_nd) {
  if (!Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_IMPLICIT_AFFINITY))
    Whack_Do_Loops (func_nd);
}



/***********************************************************************
 *
 * Set up buddies for confirming reshaped dinfos.
 *
 ***********************************************************************/
static void Find_Buddy () {

  for (INT i=1; i<da_stack->Elements(); i++) {
    DISTR_INFO* dinfo = da_stack->Bottom_nth(i);
    DISTR_ARRAY* dact = NULL;

    // make sure we have a dact. (it is possible there is ONLY a
    // redistribute directive, in which case we'll have a dinfo, but
    // no dact.
    if (dinfo->Num_Dact()) {
      dact = dinfo->Get_Dact (0);
      for (INT j=0; j<i; j++) {
        DISTR_INFO* old_dinfo = da_stack->Bottom_nth(j);
        if (old_dinfo->IsReshaped()) {
          DISTR_ARRAY* old_dact = old_dinfo->Get_Dact(0);
          if (dact->DACT_Equiv (old_dact)) {
            dinfo->Set_Buddy (old_dinfo);
            break;    // from the "j" loop
          }
        }
      }
    }
  }
}

/***********************************************************************
 *
 * Lower the data distribution pragmas and generate code.
 *
 ***********************************************************************/
extern void Lego_Lower_Pragmas (WN* func_nd) {
  Lower_Distr_Pragmas (func_nd);
  if (Run_Dsm_Check) 
    EC_Array_Portion_Calls(func_nd);
  Find_Buddy ();
  if (!Get_Trace(TP_LNOPT2, TT_LEGO_PRAGMAS_ONLY))
    Lower_Array_Accesses (func_nd);
  if (PU_has_reshaped_commons) Rewrite_Reshaped_Commons (func_nd);

  if (Debug_Lego) {
    printf("===== Done Lowering Arrays ====\n");
    // Dump_WN(func_nd, stdout, 1);
    // dump_tree(func_nd);
    printf("===== Begin Sanity Tests ====\n");
    printf("(**) Du_Sanity_Check\n");
    Du_Sanity_Check(func_nd);
#ifdef Is_True_On
    printf("(**) MP_Sanity_Check\n");
    MP_Sanity_Check_Func(func_nd);
#endif
    printf("(**) LWN_Check_Parentize\n");
    LWN_Check_Parentize(func_nd);
  }
}

/***********************************************************************
 *
 * This routine is called if LNO is turned off and only LEGO lowering
 * (without any optimizations) is to be performed. This involves 
 * implementing data distribution and affinity pragmas. Just do the 
 * minimum LNO related stuff, necessary for the LEGO pragmas
 *
 ***********************************************************************/
extern void Lego_OZero_Driver(PU_Info* current_pu, 
			      WN* func_nd) 
{
  extern BOOL Run_lno;

  Lego_PU_Init ();

  BOOL has_do_loops = Mark_Code(func_nd, TRUE, TRUE);
  Lego_Read_Pragmas (func_nd);

  void Lego_Fix_Local(WN *func_nd);
  if (LNO_Run_Lego_Localizer) Lego_Fix_Local(func_nd);

  void Lego_Fix_IO(WN *func_nd, BOOL *has_dos);
  Lego_Fix_IO(func_nd,&has_do_loops);

  // Build and map all access arrays
  LNO_Build_Access(func_nd,&LNO_default_pool);  
  if (LNO_Verbose) {
    LNO_Print_Access(TFile,func_nd);  
  }


  if (!has_do_loops) return;

  // Get rid of inconsistent control flow
  if (Eliminate_Dead_SCF(func_nd,LWN_Delete_Tree)) {
    Mark_Code(func_nd, FALSE, TRUE);  
	// remark because elimination may have changed things
  }

  // Build the array dependence graph
  BOOL graph_is_ok = Build_Array_Dependence_Graph (func_nd);

  if (graph_is_ok) {
    Lego_Mp_Tile(func_nd, TRUE);
    if (Get_Trace(TP_LNOPT,TT_LNO_DEP2) || 
        Get_Trace(TP_LNOPT,TT_LNO_DEP)) {
      fprintf(TFile, "%sLNO dep graph for CG, after LNO\n%s", DBar, DBar);
      Current_Dep_Graph->Print(TFile);
      fprintf(TFile, "%s", DBar);
    }
    if (Run_autopar) { 
      LWN_Process_FF_Pragmas(func_nd); 
      Parallel_And_Padding_Phase(current_pu, func_nd); 
    } 
    Build_CG_Dependence_Graph (Array_Dependence_Graph);
  }
  else Build_CG_Dependence_Graph (func_nd);

  if (!Get_Trace(TP_LNOPT, TT_LNO_GUARD)) {
    Guard_Dos(func_nd); // put guards around all the do statments
  }

  return;
}

static INITO_IDX pu_ino;

/***********************************************************************
 *
 * PU-level initialization (done at the start of LNO on a per-PU basis).
 *
 ***********************************************************************/
extern void Lego_PU_Init (void) {
  MEM_POOL_Push (LEGO_pool);

  da_hash = CXX_NEW (DA_HASH_TABLE(20, LEGO_pool), LEGO_pool);
  da_stack = CXX_NEW (DA_STACK(LEGO_pool), LEGO_pool);
  PU_has_reshaped_commons = FALSE;

  if (!disable_rr_maps) 
    RR_Map = WN_MAP_Create(LEGO_pool);

  /* Write out markers for distr array count in this PU,
   * into MIPS_distr_array section.
   */
  da_count = 0;
  char name[64];
  sprintf (name, "_da_count_PUnum_%lld", pu_count);
  ST* da_count_st = Section_Variable_ST (name, Be_Type_Tbl(MTYPE_I8), TRUE);
  pu_ino = New_INITO (da_count_st);
  Set_ST_is_initialized(da_count_st);
  Allocate_Object(da_count_st);
  pu_count++;
}

/***********************************************************************
 *
 * Given a dact, delete all its associated pragmas from the WHIRL tree.
 *
 ***********************************************************************/
static void Delete_Distr_Pragmas (DISTR_ARRAY* dact) {
  WN* wn = dact->First_Pragma_WN();
  WN* end_wn = WN_next(dact->Last_Pragma_WN());
  while (wn != end_wn) {
    Is_True (WN_operator(wn) == OPR_PRAGMA ||
             WN_operator(wn) == OPR_XPRAGMA ||
             WN_operator(wn) == OPR_STID,
             ("Delete_Distr_Pragmas: Expected a pragma/stid node\n"));
    WN* next_wn = WN_next(wn);
    LWN_Delete_Tree_From_Block (wn);
    wn = next_wn;
  }
}

/***********************************************************************
 *
 * Walk all distributions, and call Delete_Distr_Pragmas for each dact.
 *
 ***********************************************************************/
static void Delete_All_Distr_Pragmas () {
  for (INT i=0; i<da_stack->Elements(); i++) {
    DISTR_INFO* dinfo = da_stack->Bottom_nth(i);
    INT j;
    for (j=0; j<dinfo->Num_Dact(); j++) {
      DISTR_ARRAY* dact = dinfo->Get_Dact(j);
      Delete_Distr_Pragmas (dact);
    }
    for (j=0; j<dinfo->Num_Gen_Redistr(); j++) {
      DISTR_ARRAY* dact = dinfo->Get_Gen_Redistr(j);
      Delete_Distr_Pragmas (dact);
    }
  }
}

/***********************************************************************
 *
 * PU-level finalization (done at the end of LNO on a per-PU basis).
 *
 ***********************************************************************/
extern void Lego_PU_Fini (void) {
  /* Now write out da_count. inv is NULL, since just one value
   * in this ST.
   */
  Irb_Init_Integer (8, da_count, 1, pu_ino, INITV_IDX_ZERO);
  if (Verbose_Lego) {
    extern char* Cur_PU_Name;
    printf ("PU: %s, had %d global distributed arrays\n",
            Cur_PU_Name, da_count);
  }



  if (!disable_rr_maps)
    WN_MAP_Delete(RR_Map);
  RR_Map = WN_MAP_UNDEFINED;

  Delete_All_Distr_Pragmas ();
  Dealloc_HashTable_Entries ();
  CXX_DELETE (da_hash, LEGO_pool);
  da_hash = NULL;
  CXX_DELETE (da_stack, LEGO_pool);
  da_stack = NULL;
  MEM_POOL_Pop (LEGO_pool);
}

static INITO_IDX file_ino;
static INITO_IDX version_number_ino;
#define DSM_VERSION_NUMBER  1

/***********************************************************************
 *
 * One time (startup) initialization.
 *
 ***********************************************************************/


extern void Mp_File_Init(void) {
  static BOOL Lego_Mp_Special_Array_Initialized = FALSE;
  if (!Lego_Mp_Special_Array_Initialized) {
    Init_Special_Lego_Mp_Call();
    Lego_Mp_Special_Array_Initialized = TRUE; 
  } 
}

extern void Lego_File_Init (void) {

  {
    static BOOL Lego_File_Inited = FALSE; 
    if (Lego_File_Inited) 
      return; 
    Lego_File_Inited = TRUE;
  }

  Mp_File_Init ();

  {
    static BOOL LEGO_pool_initialized = FALSE;
    if (!LEGO_pool_initialized) {
      MEM_POOL_Initialize (&LEGO_memory_pool, "LEGO_pool", FALSE);
      LEGO_pool = &LEGO_memory_pool;
      LEGO_pool_initialized = TRUE;
      Generate_Runtime_Stuff ();
    }
    else FmtAssert (FALSE, ("How did LEGO pool get initialized?"));
  }

  // information about globals must survive PUs
  da_global = CXX_NEW (DA_GLOBAL_HASH_TABLE(20, Malloc_Mem_Pool),
                       Malloc_Mem_Pool);

  Verbose_Lego  = Get_Trace(TP_LNOPT2, TT_LEGO_VERBOSE);
  Debug_Lego    = Get_Trace(TP_LNOPT2, TT_LEGO_DEBUG);
  disable_divmod_opts = Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_DIVMOD);
  // once we flip the default to FALSE, flip the following too.
  disable_rr_maps = !Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_RR_MAPS);

  if (Verbose_Lego && disable_divmod_opts) 
    printf ("div/mod optimizations disabled\n");

  {
    /* Allocate storage for version number in this file */
    char name[64];
    sprintf (name, "_dsm_version_number");
    ST* version_number_st = Section_Variable_ST (name,
                                                 Be_Type_Tbl(MTYPE_I8),
                                                 TRUE);
    version_number_ino = New_INITO (version_number_st);
    Set_ST_is_initialized(version_number_st);
    Allocate_Object (version_number_st);
  }

  {
    /* Allocate storage for number of PUs in this file */
    char name[64];
    sprintf (name, "_pu_count");
    ST* pu_count_st = Section_Variable_ST (name, Be_Type_Tbl(MTYPE_I8), TRUE);
    file_ino = New_INITO (pu_count_st);
    Set_ST_is_initialized(pu_count_st);
    Allocate_Object (pu_count_st);
  }
  if (Verbose_Lego) printf ("Done Lego_File_Init\n");
}

/***********************************************************************
 *
 * One time (startup) finalization.
 *
 ***********************************************************************/
extern void Lego_File_Fini (void) {

  // delete info about distributed globals
  {
    HASH_TABLE_ITER<ST*, DISTR_GLOBAL_INFO*> iter (da_global);
    ST* st;
    DISTR_GLOBAL_INFO* dgi;
    while (iter.Step (&st, &dgi)) {
      CXX_DELETE (dgi, Malloc_Mem_Pool);
    }
    CXX_DELETE (da_global, Malloc_Mem_Pool);
    da_global = NULL;
  }
  
  Irb_Init_Integer (8, DSM_VERSION_NUMBER, 1, version_number_ino, INITV_IDX_ZERO);
  Irb_Init_Integer (8, pu_count, 1, file_ino, INITV_IDX_ZERO);
  if (Verbose_Lego)
    printf ("Done Lego_File_Fini: File had %lld PUs\n", pu_count);
  MEM_POOL_Delete (LEGO_pool);
}
