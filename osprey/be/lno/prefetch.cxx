/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// c-or-c++

/* Implement the main prefetching driver, Prefetch_Driver (WN* func_nd),
 * and some support routines.
 *
 *
 * Basic Concepts:
 * ===============
 *
 * First of all, here's the basic intuition:
 * (ref:Todd Mowry thesis from Stanford, CSL-TR-94-628, June 1994).
 *  - represent an array reference as a matrix transform (H) 
 *    on the iteration space.  Thus a(2i, 3j) is 
 *      [ 2 0] [i]
 *      [ 0 3] [j]
 *      ^^^^^^ matrix
 *             ^^^ vector
 *  - The null-space of this matrix (if an integer solution exists)
 *    identifies a vector in the iteration space along which we get
 *    temporal locality, i.e. the references are to the same location.
 *    The intuition is simple: two references, one in 
 *    iteration x1 and another in iteration x2 refer to the same location
 *    (temporal locality) if         
 *      H x1 = H x2 (x1 and x2 are iteration vectors)
 *    or H delta_x = 0
 *    Basically the null-space of H gives the vector(s) along which 
 *    we get temporal locality.
 *    Similarly, to identify spatial locality -- i.e. when do two instances
 *    of a reference lie within the same cache line, do two steps:
 *      - when are all dimensions (except stride-one dimension) equal?
 *        Construct Hs (H, with the last row zero)
 *        and solve for null-space of Hs.
 *      - having found a soln to Hs x = 0, see if a trip along
 *        that vector in the stride-one dimension (take a dot-product)
 *        lies within a cache line. If so, then we have spatial locality
 *        along that vector.
 *
 * - the above analysis helps identify the pattern with which a particular 
 *   reference gets reuse. The next step is to identify "group locality"
 *   i.e. identify sets of references that 
 *      - refer to the same location (group temporal), or
 *      - refer to the same cache line (group spatial)
 *   For instance, references a(i) and a(i+1) are likely to be in the
 *   same cache line most of the time, and we'd like to recognize that.
 *   The analysis for this is similar to the above locality analysis.
 *   Basically, we do this only for references in the same UGS -- i.e.
 *   references with the same index expressions in each dimension, except
 *   for constants.
 *      basically we solve H x1 + c1 = H x2 + c2
 *      or H delta_x = c2 - c1.
 *   If a soln exists, then the two references refer to the same location
 *   along the solution vector(s). We can extend this analysis to find
 *   group spatial by calculating to see if the references lie within the
 *   same cache line along that vector.
 *
 * - Having identified the locality vectors in the iteration space, 
 *   one twist to the above analysis is that we benefit from this locality
 *   only if the vector is "localized" -- i.e. the amount of data
 *   referenced in one trip of the locality vector doesn't exceed the
 *   size of the cache. For instance, if the locality vector is [1 1]
 *   then we will get locality only if the amount of data accessed in
 *   going from (say) iteration values (i, j) to (i+1, j+1) is less than
 *   the size of the cache. Only then is the value likely to be still within
 *   the cache, giving us locality.
 *
 * - This leads us to volume analysis --- trying to compute the amount of 
 *   data referenced within each loop in the nest. This is a chicken-and-egg
 *   problem -- if you don't know the locality you're going to get,
 *   you can't compute the volume, and if you don't know the volume and
 *   hence what is localized, you can't get the locality. We therefore
 *   do this analysis by computing the distinct number of cache lines
 *   accessed by each loop in the nest, using the locality vectors. If this 
 *   number is contained within a cache, then the loop is localized, otherwise
 *   it is not. We do this one loop at a time, innermost outwards, and stop
 *   when the cache-size gets exceeded.
 *   To facilitate this analysis, we introduce the notion of a "locality-group"
 *   An LG is a set of references within the same UGS that can potentially
 *   lie within the same cache line. As discussed above in group locality,
 *   this happens when H delta_x = c2 - c1 has a solution.
 *   If the solution vector happens to be [0 2 1] for a triply-nested loop,
 *   then the two references are 
 *      - in separate cache lines for the innermost loop
 *        (and therefore in separate LGs)
 *      - can be in the same cache line across iterations of the middle loop
 *        (hence in the same LG)
 *      - is the same cache line in the outermost loop, of course
 *        (hence in the same LG).
 *   Therefore, for each UGS, there is a locality group for each nesting-level 
 *   in the loop-nest. 
 *  
 * - Having computed the volume for each loop, we know which loops are
 *   localized. We now know which references will hit/miss as follows:
 *      - all references in a non-localized loop will always miss
 *        from one iteration to the next. Only those references where the 
 *        locality vector doesn't cross the iteration space at all 
 *        (e.g. a(i) a(i+1)) will get locality.
 *      - references within a localized loop get locality, and miss based
 *        on the pattern predicted by the locality vector of the reference
 *        matrix. 
 *          - If temporal locality, then only the first reference will miss.
 *          - if spatial locality, then miss every few trips of the locality
 *            vector in going from one cache line to another.
 *
 * - Based on the miss pattern (above) we know what to prefetch.
 *   A brute force way to prefetch would be to have a conditional 
 *   before each prefetch that tested if the pattern was met
 *   e.g. if we should prefetch every 16 iterations, it would look like
 *   if (i % 16 == 0) prefetch(a(i)).
 *   Here's how we handle the actual prefetching:
 *   - for non-innermost loops, we "version" the loops by converting the
 *      loop do i to
 *     do i
 *      if (i%16 == 0)
 *          loop-body with prefetching code
 *      else
 *          loop-body w/o prefetching
 *     enddo
 *  - for innermost loops we don't play these games. Instead, we 
 *    supply this information in the prefetch node for CG to use
 *    while deciding how to unroll/software-pipeline the loop.
 *
 *
 *  Phew: now you know the basic algorithm. Here's an outline
 *  of the specific code.
 *
 *
 * Algorithm Outline:
 * ==================
 *
 * - analyze only array references
 * - partition array references based on the base array (type PF_BASE_ARRAY)
 * - partition references within the same base array into 
 *   uniformly generated sets (UGS). Two array references are within the same
 *   UGS if they have the same index expression in EVERY dimension of the
 *   array excepting for constants. Therefore
 *   a(2*i+3*j, 4*i+5*j) and a(2*i+3*j+71, 4*i+5*j+49) belong to the same UGS
 *
 * The prefetching algorithm works on one PU at a time, in the following steps.
 * 1. Construct a data structure (internal to prefetching) that captures
 *    the loop structure of the PU. The type is PF_LOOPNODE in pf_loop.h.
 *    The root loopnode corresponds to the PU as a whole and has a list
 *    of children, one for each top-level loop in the PU.
 *    The loop structure is a tree, in that each node has a list of
 *    children, one for each loop directly within it.
 *    The tree is double linked in that each node has a pointer to its parent.
 *
 *    Furthermore, each loop contains all the references contained directly
 *    within that loop as a list of base arrays. Within each base array,
 *    the references are further partitioned into UGS. 
 *      
 *    While constructing a UGS, we build useful information like the 
 *    matrix representation of the reference (H), Hs, ker(H) -- kernel of H,
 *    ker(Hs) etc. These are computed when the UGS is created, and used for
 *    locality analysis later.
 *
 *    The subsequent processing is currently done in a "breadth-first" fashion,
 *    in that each step is performed for all the top-level loops before going
 *    on to the next step. An alternate would be to go "depth-first" by doing
 *    all the steps for a loop-nest before going on to the next loop-nest.
 *    This would likely have better compile-time data locality. However, since
 *    the "depth-first" approach may be more amenable to array section analysis
 *    from one loop-nest to the next, I've stayed with breadth-first. But it's
 *    easy to change.
 *
 * 2. After this data structure has been built, we next process each UGS and 
 *    construct the "base" locality groups -- i.e. references that get
 *    reuse within the *same* iteration of the loop that they're in.
 *    We compute the LGs for the loops as we go along (and as needed), since
 *    all loops may not be localized.
 *
 * 3. We next do volume analysis, by computing the volume for each loop nest,
 *    innermost loop outwards, constructing and using the locality groups
 *    as we go along. The details of volume computation are described and 
 *    coded in pf_ref.cxx.
 *  
 * 4. After volume computation, determine what loop-versioning is required,
 *    and the do the versioning. The details are coded and described in
 *    pf_loop.cxx.
 *
 * 5. After loop versioning, generate the prefetches. This is described
 *    in pf_loop.cxx and pf_ref.cxx. The meat of the code is in pf_ref.cxx.
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "prefetch.h"
#include "config_lno.h"
#include "pf_loop.h"
#include "lwn_util.h"
#include "wn_map.h"
#include "mat.h"
#include "pf_cache.h"
#include "lnoutils.h"
#include "pf_manual.h"
#include "whirl2src.h"

CACHE_PARAMETERS Cache;

extern void Dump_WN(WN*, FILE*, INT, INT = 2, INT = 2, WN** = NULL, WN* = NULL,
		    ARRAY_DIRECTED_GRAPH16* =NULL);

extern WN_MAP Parent_Map;
WN_MAP version_map = 0;

static BOOL pf_mempools_initialized = FALSE;
MEM_POOL PF_memory_pool, *PF_mpool;
MEM_POOL PF_CG_mpool;

WN* pf_func_nd;

UINT ls_num_indent = 0;
UINT vb_num_indent = 0;
BOOL vb_print_split = TRUE;

static WN* PF_Get_First_Do_Loop (WN* wn);  /* support routine */
static WN* PF_Get_Next_Do_Loop (WN* wn);   /* support routine */

ARRAY_DIRECTED_GRAPH16 *pf_array_dep_graph;

BOOL Debug_Prefetch = FALSE;
BOOL Verbose_Prefetch = FALSE;
BOOL PU_has_manual_prefetch = FALSE;

#ifdef KEY
#include "glob.h"        // Src_File_Name

INT Num_Prefetches;
#endif

static void Prefetch_Manual (WN* func_nd);
static void Disable_Prefetch_Manual (WN* func_nd);
static void Prefetch_Auto   (WN* func_nd,
                             ARRAY_DIRECTED_GRAPH16 *array_dep_graph); 
static void Process_PU_Pragmas (WN* func_nd);


// mips4+ and IA-64 ISAs have prefetches
static inline BOOL
Target_ISA_Has_Prefetch()
{
  return (Is_Target_ISA_M4Plus() || Is_Target_ISA_I1Plus()
#ifdef TARG_X8664
          || Is_Target_x86_64()
#endif
         );
}

// R10K (and its successors) and Itanium (TM) support prefetching
static inline UINT32
Target_Proc_Run_Prefetch()
{
  if (Is_Target_R10K())   return 1;
#ifdef TARG_MIPS
  if (Is_Target_Sb1())   return 1;
#endif
#ifdef TARG_IA64
  if (Is_Target_Itanium()) return 2; // more aggressive
#endif
#ifdef TARG_X8664
  if (Is_Target_Orochi()) return NO_PREFETCH; // rely on hardware prefetcher
  if (Is_Target_x86_64()) return CONSERVATIVE_PREFETCH; // more aggressive
#endif
  return 0;
}

/***********************************************************************
 *
 * Parse command line options to determine global prefetching options.
 *
 ***********************************************************************/
extern void Init_Prefetch_Options (WN* func_nd)
{
  INT i;

  if (!Target_ISA_Has_Prefetch()) {
    static BOOL warned_isa_noprefetch = FALSE;
    if ((!warned_isa_noprefetch) && 
        (LNO_Run_Prefetch_Set && LNO_Run_Prefetch) ||
        (LNO_Run_Prefetch_Manual_Set && LNO_Run_Prefetch_Manual))
    {
      // user explicitly asked for prefetches -- warn
      fprintf ( stderr,
		"Warning: Prefetching disabled since ISA %s "
		"does not support prefetch.\n",
                Isa_Name(Target_ISA));
      warned_isa_noprefetch = TRUE;
    }
#ifdef KEY
    LNO_Run_Prefetch = NO_PREFETCH;
#else
    LNO_Run_Prefetch = 0;
#endif
    LNO_Run_Prefetch_Manual = FALSE;
    return;
  }

  if (!LNO_Run_Prefetch_Set) {
    LNO_Run_Prefetch = Target_Proc_Run_Prefetch();
  }

  if (!LNO_Run_Prefetch_Manual_Set) {
    if (Target_Proc_Run_Prefetch() > 0) 
      LNO_Run_Prefetch_Manual = TRUE;
    else LNO_Run_Prefetch_Manual = FALSE;
  }

  // Process command line options

  // allow override through -tt31 options
#ifndef KEY
  if (LNO_Run_Prefetch == 0) 
#else
  if (LNO_Run_Prefetch == NO_PREFETCH) 
#endif
    if (Get_Trace(TP_LNOPT,TT_LNO_PREFETCH))
      LNO_Run_Prefetch = Target_Proc_Run_Prefetch();
  Debug_Prefetch = Get_Trace(TP_LNOPT,TT_LNO_PREFETCH_DEBUG);
  Verbose_Prefetch = Get_Trace(TP_LNOPT,TT_LNO_PREFETCH_VERBOSE);

#ifdef KEY
  // prefetch stores starting from CONSERVATIVE_PREFETCH
  if (!LNO_Prefetch_Stores_Set)
    LNO_Prefetch_Stores = LNO_Run_Prefetch > SOME_PREFETCH;
#endif

  /* These must be processed here since they determine
   * Run_Prefetch etc, used in Mhd.Initialize
   */
  if (!LNO_Ignore_Pragmas) {
    Process_PU_Pragmas (WN_func_pragmas(func_nd));
    Process_PU_Pragmas (WN_func_body(func_nd));
  }

  if (Verbose_Prefetch) {
#ifdef KEY
    printf ("LNO:Run_Prefetch          = %s\n",
	    ((LNO_Run_Prefetch == NO_PREFETCH) ? "false" :
	((LNO_Run_Prefetch == SOME_PREFETCH) ? "very conservative" 			     : ((LNO_Run_Prefetch == CONSERVATIVE_PREFETCH) ?
					  "conservative" : "aggressive"))));
#else
    printf ("LNO:Run_Prefetch          = %s\n",
	    ((LNO_Run_Prefetch == 0) ? "false"
				     : ((LNO_Run_Prefetch == 1) ?
					  "conservative" : "aggressive")));
#endif
    printf ("LNO:Run_Prefetch_Manual   = %s\n", (LNO_Run_Prefetch_Manual
                                           ? "true" : "false"));
    printf ("Debug_Prefetch            = %s\n", (Debug_Prefetch
                                           ? "true" : "false"));
    printf ("Verbose_Prefetch          = %s\n", (Verbose_Prefetch
                                           ? "true" : "false"));
    printf ("LNO:Prefetch_Ahead        = %d\n", LNO_Prefetch_Ahead);
    printf ("LNO:Prefetch_Cache_Factor = %d\n", LNO_Prefetch_Cache_Factor);
    for (i=0; i<4; i++)
      printf ("Cache level %d       = %s\n", i+1,
	      (LNO_FLAGS_mhd(Current_LNO)->L[i].Prefetch_Level
					? "true" : "false"));
  }
}

/***********************************************************************
 *
 *  Process PU level prefetching pragmas.
 *  Run before can, so need to walk the entire PU.
 *
 ***********************************************************************/
static void Process_PU_Pragmas (WN* func_nd) {
  WN* pwn = func_nd;
  if (WN_operator(pwn) == OPR_PRAGMA) {
    switch (WN_pragma(pwn)) {
    case WN_PRAGMA_PREFETCH:
      if (!WN_pragma_arg1(pwn) && !WN_pragma_arg2(pwn)) {
        // no prefetching enabled for any level.
        VB_PRINT (printf ("Disable automatic prefetching\n"));
#ifdef KEY
        LNO_Run_Prefetch = NO_PREFETCH;
#else
        LNO_Run_Prefetch = 0;
#endif
      }
      else {
        // prefetching enabled for some level.
#ifdef KEY
        LNO_Run_Prefetch = CONSERVATIVE_PREFETCH;
#else
        LNO_Run_Prefetch = 1;
#endif
        if (WN_pragma_arg1(pwn)) {
          VB_PRINT (printf ("Enable auto-prefetch, level-1 cache\n"));
          LNO_FLAGS_mhd(Current_LNO)->L[0].Prefetch_Level = TRUE;
          if (WN_pragma_arg1(pwn) > LNO_Run_Prefetch)
            LNO_Run_Prefetch = WN_pragma_arg1(pwn);
        }
        else {
          VB_PRINT (printf ("Disable auto-prefetch, level-1 cache\n"));
          LNO_FLAGS_mhd(Current_LNO)->L[0].Prefetch_Level = FALSE;
        }
        if (WN_pragma_arg2(pwn)) {
          VB_PRINT (printf ("Enable auto-prefetch, level-2 cache\n"));
          LNO_FLAGS_mhd(Current_LNO)->L[1].Prefetch_Level = TRUE;
          if (WN_pragma_arg2(pwn) > LNO_Run_Prefetch)
            LNO_Run_Prefetch = WN_pragma_arg2(pwn);
        }
        else {
          VB_PRINT (printf ("Disable auto-prefetch, level-2 cache\n"));
          LNO_FLAGS_mhd(Current_LNO)->L[1].Prefetch_Level = FALSE;
        }
      }
      break;
    case WN_PRAGMA_PREFETCH_MANUAL:
      if (WN_pragma_arg1(pwn) == 0) {
        // ignore manual prefetches
        VB_PRINT (printf ("Disable manual-prefetching\n"));
        LNO_Run_Prefetch_Manual = FALSE;
      }
      else {
        VB_PRINT (printf ("Enable manual-prefetching\n"));
        LNO_Run_Prefetch_Manual = TRUE;
      }
      break;
    default:
      break;
    }
    return;
  }

  if (WN_operator(pwn) == OPR_BLOCK) {
    WN* wn = WN_first(pwn);
    while (wn) {
      Process_PU_Pragmas(wn);
      wn = WN_next(wn);
    }
  }
  else {
    for (INT i=0; i<WN_kid_count(pwn); i++) {
      Process_PU_Pragmas (WN_kid(pwn, i));
    }
  }
} /* Process_PU_Pragmas */

/***********************************************************************
 *
 *  Process PU level prefetch-disable pragmas.
 *
 ***********************************************************************/
static void Process_PU_Disable_Pragmas (WN* func_nd) {
  WN* plist = WN_func_pragmas(func_nd);
  Is_True (WN_opcode(plist) == OPC_BLOCK,
           ("Pragma list is not a block"));
  WN* pwn = WN_first(plist);

  while (pwn) {
    Is_True (WN_operator(pwn) == OPR_PRAGMA,
             ("Pragma list contains non-pragma node"));
    switch (WN_pragma(pwn)) {
    case WN_PRAGMA_PREFETCH_REF_DISABLE:
      /* Add this ST to manual prefetch symbols in hash table */
      {
	SYMBOL sym (WN_st(pwn), 0, 0);
	mpf_syms->Enter (&sym, WN_pragma_arg2(pwn));
	VB_PRINT(printf ("Prefetch ref disable:  ");
		 sym.Print (stdout);
		 printf ("\n"));
      }
      break;
    default:
      break;
    }
    pwn = WN_next(pwn);
  }
}


/***********************************************************************
 *
 * Process manual prefetches.
 *
 ***********************************************************************/
static void Prefetch_Manual (WN* func_nd) {
  VB_PRINT (printf ("What about scalars?\n"));

  /* 1. Find corresponding load(s)/store(s), 
   *    link to the prefetch. (multiple loads in same cache line?)
   * 2. don't consider this array in automatic analysis,
   *    instead use the supplied volume (which cache level?)
   * 3. Smarts: cacheline analysis, make sure info in prefetch node
   *    is made fully consistent.
   */

  /* don't worry about do-loops, just go for the whole tree */
  // treat top-level code in func as a "loop"
  SINGLE_LOOP loop (PF_mpool);
  loop.Process_Loop_Manual (WN_func_body(func_nd));
  VB_PRINT (printf ("After manual prefetching ");
            mpf_syms->Print (stdout));
  return;
  

}

/***********************************************************************
 *
 * Disable manual prefetches.
 *
 ***********************************************************************/
static void Disable_Prefetch_Manual (WN* wn) {
  if (!wn) return;

  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_PREFETCH) {
    // must be a manual prefetch. disable it
    WN_pf_set_confidence(wn, 0);
    return;
  }

  if (opr == OPR_BLOCK) {
    WN* kid = WN_first(wn);
    while (kid) {
      Disable_Prefetch_Manual(kid);
      kid = WN_next(kid);
    }
  }
  else {
    for (INT i=0; i<WN_kid_count(wn); i++)
      Disable_Prefetch_Manual (WN_kid(wn, i));
  }
}

static void Prefetch_Auto (WN* func_nd,
                           ARRAY_DIRECTED_GRAPH16 *array_dep_graph) {
  WN* doloop;
  INT loopno = 0;

  /* du-chain-debug  Du_Sanity_Check (func_nd); */
  /* du-chain-debug  Print_Def_Use (func_nd, stdout); */

  Cache.Initialize ();
  if (Cache.Levels() == 0) return;

  // the following is to avoid passing it as an argument all over the place
  pf_array_dep_graph = array_dep_graph;
  MAT<FRAC>::Set_Default_Pool(&PF_memory_pool);
  version_map = WN_MAP_Create (PF_mpool);
  Initialize_Lvs ();

  PF_LOOPNODE *rootnode = CXX_NEW (PF_LOOPNODE(0, func_nd, -1), PF_mpool);

  /* walk the outer do loops and collect them */
  doloop = PF_Get_First_Do_Loop (func_nd);
  while (doloop) { 
    PF_LOOPNODE *childnode =
      CXX_NEW (PF_LOOPNODE(rootnode, doloop, 0), PF_mpool);
    rootnode->Add_Child (childnode);
    doloop = PF_Get_Next_Do_Loop (doloop);
  }

  /* now walk the outer do loops and process them */
  for (loopno=0; loopno<rootnode->Num_Children(); loopno++) {
    PF_LOOPNODE *childnode = rootnode->Get_Child(loopno);

    childnode->Process_Loop ();

    // ============== now process each loop nest at a time ====================
    // child node is a top-level loop nest, process it.


    PF_PRINT(fprintf (TFile, "------ Loop nest number: %d --------\n", loopno);
              childnode->Print (TFile);
              fprintf(TFile, "------------- Now build base LGs ----------\n"));
    VB_PRINT (printf ("\n================ Loop nest number: %d =========\n",
                      loopno);
              printf ("---------------- structure ----------------\n");
              childnode->Print_Structure ());

    childnode->Build_Base_LGs ();

    PF_PRINT (childnode->Print (TFile);
              fprintf (TFile, "----------- Now do volume computation -----\n");
              fprintf (TFile, "     --- cache parameters: %d (%d), %d (%d) \n",
                       Cache.EffSize(1), Cache.LineSize(1),
                       Cache.EffSize(2), Cache.LineSize(2)));

    childnode->Volume ();

    PF_PRINT (fprintf (TFile, "------- done with volume computation -----\n");
              childnode->Print (TFile);
              fprintf(TFile,"\n----- find loc loops+what to prefetch ---\n"));
    VB_PRINT (printf ("\n---------------- volume ----------------\n");
              childnode->Print_Volume ());

    {
      PF_LOCLOOP tmp;
      childnode->Find_Loc_Loops (tmp);
    }

    PF_PRINT (fprintf (TFile, "\n----- now split and gen prefetch -----\n"));
    VB_PRINT (printf ("\n---------------- splits ----------------\n");
              childnode->Print_Splits ();
              printf ("\n---------------- prefetches ----------------\n");
              printf ("       (cannot coordinate with splits) \n"));

#ifdef KEY
    Num_Prefetches = 0;
#endif
    childnode->Gen_Prefetch (NULL);
#ifdef KEY
    if (Num_Prefetches > 0 && LNO_Prefetch_Verbose) {
      printf("(%s:%d) ", 
	     Src_File_Name, 
	     Srcpos_To_Line(WN_Get_Linenum(childnode->Get_Code())));
      printf ("Generated %d prefetch instructions for this loop\n", 
	      Num_Prefetches);
    }      
#endif

    PF_PRINT (fprintf (TFile, "---- Done with Loop nest number: %d ----\n",
                       loopno));
    // ============== done processing this loop nest ==========================
    
    Is_True (LWN_Check_Parentize (childnode->Get_Code()),
             ("Check_Parentize failed\n"));
  }


  Is_True (LWN_Check_Parentize (func_nd), ("Check_Parentize failed\n"));
  Cleanup_Lvs ();
  CXX_DELETE (rootnode, PF_mpool);
  WN_MAP_Delete (version_map);
  return;
}

/***********************************************************************
 *
 * Main prefetch driver.
 *
 ***********************************************************************/
void Prefetch_Driver ( WN* func_nd, ARRAY_DIRECTED_GRAPH16 * ) {

  pf_func_nd = func_nd;
  if (LNO_Analysis || Verbose_Prefetch) Whirl2Src_Init (func_nd);

  // Store original flag values
  INT  Run_Prefetch_save = LNO_Run_Prefetch;
  BOOL Run_Prefetch_Manual_save = LNO_Run_Prefetch_Manual;
  BOOL pf_level_save[4];
  INT i;
  for (i=0; i<4; i++ ) {
    pf_level_save[i] = LNO_FLAGS_mhd(Current_LNO)->L[i].Prefetch_Level;
  }

  // Move this to before Mhd.Initialize() in lnopt_main.cxx
  // Init_Prefetch_Options (func_nd);

  if (LNO_Run_Prefetch || LNO_Run_Prefetch_Manual) {
    PF_PRINT (fprintf (TFile, "Process PU %s\n", Cur_PU_Name));
    VB_PRINT (printf ("Process PU %s\n", Cur_PU_Name));
    if (!pf_mempools_initialized) {
      MEM_POOL_Initialize (&PF_memory_pool, "Prefetch_pool", FALSE);
      MEM_POOL_Initialize (&PF_CG_mpool, "Prefetch_to_cg_mpool", FALSE);
      MEM_POOL_Push_Freeze (&PF_CG_mpool);
      PF_mpool = &(PF_memory_pool);
      pf_mempools_initialized = TRUE;
    }
    else {
      // the previous PU has gone all the way through CG
      // so we can pop and re-push.
      MEM_POOL_Pop_Unfreeze (&PF_CG_mpool);
      MEM_POOL_Push_Freeze (&PF_CG_mpool);
    }
      
    MEM_POOL_Push_Freeze (PF_mpool);
    mpf_syms = CXX_NEW(MANUAL_PREFETCH_SYMBOLS(PF_mpool), PF_mpool);

    /* Process PU-level pragmas */
    if (!LNO_Ignore_Pragmas) {
      Process_PU_Disable_Pragmas (func_nd);
    }

    if (LNO_Run_Prefetch_Manual && PU_has_manual_prefetch) {
      Prefetch_Manual (func_nd);
    }
    else if (!LNO_Run_Prefetch_Manual && PU_has_manual_prefetch) {
      Disable_Prefetch_Manual (func_nd);
    }
    VB_PRINT (printf ("No manual prefetches\n"));
    if (LNO_Run_Prefetch) {
      PF_PRINT ( fprintf(TFile,"Prefetch_Driver: before invoking Prefetch_Auto\n");
                 fdump_tree(TFile, func_nd); );

      Prefetch_Auto (func_nd, Array_Dependence_Graph);

      PF_PRINT ( fprintf(TFile,"Prefetch_Driver: after invoking Prefetch_Auto\n");
                 fdump_tree(TFile, func_nd); );
    }

    Is_True (mpf_syms || !LNO_Run_Prefetch_Manual,
             ("mpf_syms incorrect relative to manual_prefetch flag"));
    CXX_DELETE (mpf_syms, PF_mpool);
    mpf_syms = NULL;
    MEM_POOL_Pop_Unfreeze (PF_mpool);
  }

  /* Restore original prefetch options */
  LNO_Run_Prefetch = Run_Prefetch_save;
  LNO_Run_Prefetch_Manual = Run_Prefetch_Manual_save;
  for ( i=0; i<4; i++ ) {
    LNO_FLAGS_mhd(Current_LNO)->L[i].Prefetch_Level = pf_level_save[i];
  }
}


/* Given a whirl node, return the first DO loop in a 
 * depth-first-traversal of the tree.
 * This will correspond to the lexically first, and outermost DO loop
 * in the whirl tree.
 * Perform a simple DFS traversal, return the first do loop.
 */
static WN* PF_Get_First_Do_Loop (WN* wn) {
  WN* tmp;
  if (OPCODE_is_leaf(WN_opcode(wn))) return NULL;
  if (WN_opcode(wn) == OPC_DO_LOOP) return wn;
  if (WN_opcode(wn) == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      if (WN_opcode(kid) == OPC_DO_LOOP) return kid;
      if ((OPCODE_is_scf(WN_opcode(kid))) &&
          (tmp = PF_Get_First_Do_Loop (kid))) return tmp;
      kid = WN_next (kid);
    }
    return NULL;
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    if (WN_opcode(kid) == OPC_DO_LOOP) return kid;
      if ((OPCODE_is_scf(WN_opcode(kid))) &&
          (tmp = PF_Get_First_Do_Loop (kid))) return tmp;
  }

  return NULL;
}

/* Given a whirl tree (with wn a DO loop), return the lexically 
 * next DO Loop. Do NOT return any do loops within the current loop.
 * Repeated calls to PF_Get_Next_Do_Loop return successive (lexically) 
 * outermost do loops, until we are out of loops.
 * Return NULL if out of loops.
 */
static WN* PF_Get_Next_Do_Loop (WN* wn) {
  WN *pwn, *tmp;
  Is_True ((WN_opcode(wn) == OPC_DO_LOOP),
           ("PF_Get_Next_Do_Loop called on a non-DO loop\n"));
  pwn = LWN_Get_Parent (wn);

  while (1) { 
    if (pwn == NULL) {
      /* reached the top of the tree, done. */
      return NULL;
    }
    if (WN_opcode(pwn) == OPC_BLOCK) {
      WN* kid = WN_next (wn);
      while (kid) {
        if (WN_opcode(kid) == OPC_DO_LOOP) return kid;
        if ((OPCODE_is_scf(WN_opcode(kid))) &&
            (tmp = PF_Get_First_Do_Loop (kid))) return tmp;
        kid = WN_next (kid);
      }
    }
    else {
      /* first find this kid */
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(pwn); kidno++)
        if (wn == WN_kid (pwn, kidno)) break;
      Is_True ((kidno < WN_kid_count (pwn)),
               ("kid 0x%lx not a child of its parent 0x%lx\n", wn, pwn));
      
      /* now look at the remaining kids */
      kidno++;
      for (; kidno<WN_kid_count (pwn); kidno++) {
        kid = WN_kid (pwn, kidno);
        if (WN_opcode(kid) == OPC_DO_LOOP) return kid;
        if ((OPCODE_is_scf(WN_opcode(kid))) &&
            (tmp = PF_Get_First_Do_Loop (kid))) return tmp;
      }
    }
    /* didn't find anything, go up another level */
    wn = pwn;
    pwn = LWN_Get_Parent (wn);
  }
  /* should never reach here */
}
