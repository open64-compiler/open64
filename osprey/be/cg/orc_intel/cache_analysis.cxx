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
/* =======================================================================
 * =======================================================================
 *
 *  Module: cache_analysis.cxx
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/cache_analysis.cxx,v $
 *
 *  Description:
 *  ============
 *
 *  Do cache analysis and cache optimization, mainly is to use correct latency 
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define USE_STANDARD_TYPES
#include <sys/types.h>
#include <map>
#include "defs.h"
#include "mempool.h"
#include "errors.h"
#include "op.h"
#include "tn.h"

#include "wn.h"
#include "opt_alias_interface.h"		/* for WOPT alias mgr */
#include "opt_points_to.h"
#include "whirl2ops.h"
#include "pf_cg.h"
#include "wn_map.h"

#include "cg.h"
#include "cgir.h"
#include "cg_dep_graph.h"
#include "data_layout.h"

#include "opt_wn.h"
#include "targ_issue_port.h"
#include "targ_cache_info.h"

#include "ipfec_options.h"
#include "be_util.h"

static WN_MAP wn_in_cache;
BOOL cache_analyzed = FALSE;
BOOL trace = FALSE;
MEM_POOL cache_map_pool;

// Answer whether op access data in L2 or not.
// It is analyzed in Cache Location Analysis, 
BOOL Cache_L2_Has_Data(OP *op)
{
    if (!cache_analyzed) return FALSE;
    WN *wn = Get_WN_From_Memory_OP( op );
    return wn && WN_MAP32_Get(wn_in_cache,wn)==2 ? TRUE : FALSE;
}

// Answer whether op access data in L1 or not.
// It is analyzed in Cache Location Analysis, 
BOOL Cache_L1_Has_Data(OP *op)
{
    if (!cache_analyzed) return FALSE;
    WN *wn = Get_WN_From_Memory_OP( op );
    return wn && WN_MAP32_Get(wn_in_cache,wn)==1 ? TRUE : FALSE;
}

// Called by Location analysis for deciding data is L2.
// It should called before SWP and after Prefetch.
BOOL Cache_L2_Analysis(OP *op)
{
    Is_True(OP_load(op)||OP_store(op), ("wrong OP here for only permitting load!"));

    // Some heuristics: 
    // 1) not do any prefetch to L1
    // 2) many point to other structure and access data
    // 3) many loads can influence the performance
    // 4) BB information, enough ops for hiding long latency caused
    //    by L2. And high frequency is preferred. 
    // 5) Cache information
    

    // WN prefetch 
    WN *wn, *pf_wn;
    PF_POINTER *pf_ptr;
    if (( wn = Get_WN_From_Memory_OP( op ) )
        && (pf_ptr = (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,wn) ) ) {

        UINT confidence;
        
	if (( pf_wn = PF_PTR_wn_pref_1L(pf_ptr) )
	    && ( (confidence = WN_pf_confidence( pf_wn )) != 1 )
	    && ( Prefetch_Kind_Enabled( pf_wn ) ) ) {
	    return FALSE;
	}

        if (( pf_wn = PF_PTR_wn_pref_2L(pf_ptr) )
            && ( (confidence = WN_pf_confidence( pf_wn )) != 1 )
            && (  Prefetch_Kind_Enabled( pf_wn ) ) ) {
               if ( confidence) {
                  // many load instruction to this prefetch, 
                  // it will be loaded to L1D.So, ignore this case;
                  BB *bb = OP_bb(op);
                  OP *cur_op;
                  FOR_ALL_BB_OPs(bb,cur_op) {
                      if (cur_op == op) continue;
                      if (OP_load(cur_op)) {
                          WN *cur_wn = Get_WN_From_Memory_OP( cur_op );
                          PF_POINTER *cur_pf_ptr = (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,wn);
                          if (cur_wn && cur_pf_ptr &&
                              pf_wn == PF_PTR_wn_pref_2L(cur_pf_ptr)) {
                              return FALSE;
                          }
                      }
                  }
                  return TRUE;
               } 
        }
    }
    
    if (OP_Prefetched(op)) return FALSE;   // stride prefetch L1
    if (wn == NULL) return FALSE;
    
    BB *bb = OP_bb(op);
    if (BB_loop_head_bb(bb) == NULL ) return FALSE; // should be loop

    //load address not a constant 
    OPERATOR opr = OPCODE_operator(WN_opcode(wn));
    if (OPERATOR_is_scalar_iload(opr) ||
        OPERATOR_is_scalar_istore(opr)) {
        // analysis the BB
        if (BB_length(bb) < 7 ) return FALSE;
        if (BB_freq(bb)<50000000) return FALSE;
        if (!BB_innermost(bb) || BB_nest_level(bb)<2) return FALSE;

        TY_IDX ty;
        if (OP_load(op)) {ty=WN_load_addr_ty(wn);}
        if (OP_load(op) && TY_kind(ty) == KIND_POINTER ||
            OP_store(op) ){
            WN *kid; 
            if (OP_load(op)) {
              TY_IDX p_ty = TY_pointed(ty); 
              if (!(TY_kind(p_ty) == KIND_STRUCT && TY_size(p_ty) >= 64 ||
                    TY_kind(p_ty) == KIND_SCALAR && TY_size(p_ty) <=1 )
                      ) return FALSE;
               kid = WN_kid0(wn);
            } else {
               kid = WN_kid1(wn);
            }
            //preg or ILOAD 
            if (kid != NULL) opr = OPCODE_operator(WN_opcode(kid));
            else {return FALSE;}
            INT pred_idx;
             if ( ((pred_idx=WN_offset(kid)-Last_Dedicated_Preg_Offset)>0) || 
                   OPERATOR_is_scalar_iload(opr) ||
                   OPERATOR_is_scalar_istore(opr) ) {

                if (OPERATOR_is_load(opr) ||
                    OPERATOR_is_store(opr)) {
                    DevWarn("Catch L2 case in BB %d", BB_id(bb));
                    return TRUE;
                }
            }
        }
         
    }
    
    return FALSE;
}

// Simply use information for stride prefetch and WN prefetch
BOOL Cache_L1_Analysis(OP *op)
{
    Is_True(OP_load(op)||OP_store(op), ("wrong OP here for only permitting load!"));

    if (OP_Prefetched(op)) return TRUE;   // stride prefetch L1
    // WN prefetch 
    WN *wn, *pf_wn;
    PF_POINTER *pf_ptr;
    if (( wn = Get_WN_From_Memory_OP( op ) )
        && (pf_ptr = (PF_POINTER *) WN_MAP_Get(WN_MAP_PREFETCH,wn) ) ) {

        UINT confidence;
        
	if (( pf_wn = PF_PTR_wn_pref_1L(pf_ptr) )
	    && ( (confidence = WN_pf_confidence( pf_wn )) != 1 )
	    && (  Prefetch_Kind_Enabled( pf_wn ) ) ) {
	    return TRUE;
	}
    }
    return FALSE;

}

// return kid of address of load/store.
WN *WN_address_kid(WN *wn)
{
      if (OPERATOR_is_scalar_iload (WN_operator(wn)) ||
          OPERATOR_is_scalar_istore (WN_operator(wn))) {
          WN *wn_tmp = OPERATOR_is_load (WN_operator(wn)) ?
                       WN_kid0(wn) : WN_kid1(wn);
      if (OPERATOR_is_load(WN_operator(wn_tmp)))
          return wn_tmp;
      }
      return NULL;
}

// Access same cache line or not, give difference of address
BOOL Cache_Access_Same_Line(const ALIAS_MANAGER *am, WN *wn1, WN *wn2, INT *diff)
{
    *diff = 0;
    
    // wn1 == wn2 or NULL , don't consider them
    if (wn1 == wn2 || wn1 == NULL || wn2 == NULL ) return FALSE;

    ALIAS_RESULT alias_result = Aliased(am,wn1,wn2);

    // Access same location must access same cache line
    if (alias_result == SAME_LOCATION)  return TRUE;

    // if base is same and offset diffrence <=8
    WN *addr1 = WN_address_kid(wn1);
    WN *addr2 = WN_address_kid(wn2);

    if (addr1 && addr2 && alias_result != NOT_ALIASED &&
        Aliased(am, addr1,addr2)==SAME_LOCATION &&
        ((WN_offset(wn1) - WN_offset(wn2)) <= 256) &&
         (WN_offset(wn1) - WN_offset(wn2)) >= -256) {

        *diff = WN_offset(wn1) - WN_offset(wn2);
        if (*diff < 0) {*diff = WN_offset(wn2) - WN_offset(wn1);}
        return TRUE;
    }

    // if the struction is same and possible aliased
    if (alias_result == POSSIBLY_ALIASED &&
        WN_offset(wn1) == WN_offset(wn2) &&
        WN_offset(wn1) != 0)
        return TRUE;

    return FALSE;
}

BOOL Cache_Access_Same_Line(OP *op1, OP *op2, INT *diff)
{
    WN *wn1 = Get_WN_From_Memory_OP( op1 );
    WN *wn2 = Get_WN_From_Memory_OP( op2 );

    if (Alias_Manager && wn1 && wn2) {
        return Cache_Access_Same_Line(Alias_Manager, wn1, wn2, diff);
    }
    return FALSE;
}
    
////////////////////////////////////////////////////////////////////
// It is special for Itanium2.
// Use rule to reply whether cache conflict is exist. 
// And give the distance required between two ops.
// equal is Bool variable, when it is set TRUE, it means
// the latency should be >= distance. But sometimes we
// need the option to set latency != distance, then, we set
// equal as FALSE.  
// 
BOOL Cache_Has_Conflict(OP *pred, OP *op, INT *distance, BOOL *equal)
{
    if (PROCESSOR_Version == 1) return FALSE; // ignore Itanium-1

    Is_True((OP_load(pred) || OP_store(pred)), ("pred op is not a memory op"));
    Is_True((OP_load(op) || OP_store(op)), ("pred op is not a memory op"));
    
    *equal = TRUE;
    *distance = 0;
    BOOL result = FALSE;
    
    // It is the fraction for determining where address is in same cache 
    // line or not we can tuning it for better performance; 
    float  fraction_clsize = 0.1; 
    
    if (!cache_analyzed) return FALSE;
    //disjoint and same op return back;
    if (OP_has_disjoint_predicate(pred,op)) return FALSE;
    if (pred == op || OP_has_immediate(pred) || OP_has_immediate(op)) 
    {  return FALSE;}
    if (OP_unrolling(op) != OP_unrolling(pred)) return FALSE;
    
    WN *pred_wn = OP_hoisted(pred) ? NULL : Get_WN_From_Memory_OP(pred);
    WN *succ_wn = OP_hoisted(op) ? NULL : Get_WN_From_Memory_OP(op);	      
    
    // pred and op access same cache line;
    INT addr_diff;
    if ((Alias_Manager && pred_wn && succ_wn &&
        Cache_Access_Same_Line(Alias_Manager, pred_wn, succ_wn, &addr_diff)))
    {
        float diff_threshold = fraction_clsize * Cache_Line_Size(CACHE_L1D);
        
        // store/load in L1
        if (Cache_L1_Has_Data(pred) && addr_diff<= diff_threshold) {
            if (OP_store(pred) && OP_load(op)) {
                *distance = 3;
                result = TRUE;
            } 

            // comment it for DEP analyze didn't care for loop
            // store/store in L1
            if (OP_store(pred) && OP_store(op)) {
                *distance = 1;
                result = TRUE;
            }
        }

        diff_threshold = fraction_clsize * Cache_Line_Size(CACHE_L2);

        // For L2 cache 
        if (Cache_L2_Has_Data(pred)) {

            // load/store in L2 
            if (OP_load(pred) && OP_store(op) && 
                addr_diff <= diff_threshold) {
                *distance = 2;
                result = TRUE;
            } 
    
            // L2 Bank conflict load/load store/store can't be same cycle
            // Bank is 4-7, so diff should be less than 16;
            if (addr_diff < 16 && (OP_load(pred) && OP_load(op) ||
                OP_store(pred) && OP_store(op))) {
                *distance = 1;
                result = TRUE;
            }
    
            // L2 Bank conflict store/load
            if (addr_diff < 16 && OP_store(pred) && OP_load(op)) {
             /*  TODO:: not be 3 cycle, but it has a little performance 
              *  impact, so ignore it */
            }
         }
        
         if (result) {
             if (trace) {
                 fprintf(TFile, "Conflict in Cache %d distance %d equal %d in BB %d between\n",
                       Cache_L2_Has_Data(pred)?2:1,
                       *distance,
                       *equal,
                       BB_id(OP_bb(pred)));
                 Print_OP_No_SrcLine(pred);
                 fprintf(TFile, "and\n");
                 Print_OP_No_SrcLine(op);
             }
             return TRUE;
        }
    }
    return FALSE;
}
BOOL Cache_Has_Conflict(OP *pred, OP *op, CG_DEP_KIND kind)
{
    INT distance;
    BOOL equal;
    if (kind != CG_DEP_MEMIN &&
        kind != CG_DEP_MEMANTI &&
        kind != CG_DEP_MEMOUT &&
        kind != CG_DEP_MEMREAD)  return FALSE;
    
    return Cache_Has_Conflict(pred,op, &distance, &equal);
}
void Cache_Adjust_Latency(OP *pred, OP *op, CG_DEP_KIND kind, INT *latency)
{    
      
      INT ident=FALSE;
      if (!(OP_load(pred) || OP_store(pred)) || 
	  !(OP_load(op) ||OP_store(op))) return;
      if (kind != CG_DEP_MEMIN &&
          kind != CG_DEP_MEMANTI &&
	  kind != CG_DEP_MEMOUT)  return;
     
      INT distance=0;
      BOOL equal=TRUE; // for some cases, latency should be not equal N!
      
      // if cache conflict, do something
      if (Cache_Has_Conflict(pred,op, &distance, &equal)) {
          if (equal) *latency = MAX(distance,*latency); 
          // why no use!
          if (OP_has_disjoint_predicate(pred,op)) *latency = 0;
      }
}

/////////////////////////////////////////////////////////////
//  Do location analysis, called by cg driver
//  Save result into a map of cache.
void Cache_Location_Analysis(void)
{
    // initialize memory
    MEM_POOL_Initialize(&cache_map_pool, "Cache MAP", FALSE);
    MEM_POOL_Push(&cache_map_pool);
    wn_in_cache = WN_MAP32_Create(&cache_map_pool);

    cache_analyzed = TRUE;
    trace = Get_Trace(TP_A_CANA, 0x1);


    // analyze data location in cache
    for  (BB *bb= REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
        OP *op;
        FOR_ALL_BB_OPs(bb,op) {
            if (!OP_load(op) && !OP_store(op)) continue;
             WN *wn = Get_WN_From_Memory_OP(op);
            if (wn && Cache_L2_Analysis(op)) {
                WN_MAP32_Set(wn_in_cache, wn, 2);
                continue;
            }
            if (wn && Cache_L1_Analysis(op)) {
                WN_MAP32_Set(wn_in_cache, wn, 1);
                continue;
            }
        }
    }
}

// End of Analysis, free memory
void Cache_Analysis_End(void)
{
    if (!cache_analyzed) return;

    // release memory
    WN_MAP_Delete(wn_in_cache);
    MEM_POOL_Pop(&cache_map_pool);
    MEM_POOL_Delete(&cache_map_pool);
    cache_analyzed = FALSE;
}
