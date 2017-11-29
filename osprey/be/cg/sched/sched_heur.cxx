/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

/*
 *  Copyright (C) 2000-2003, Intel Corporation
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *  
 *  Redistributions of source code must retain the above copyright notice, this list
 *  of conditions and the following disclaimer. 
 *  
 *  Redistributions in binary form must reproduce the above copyright notice, this list
 *  of conditions and the following disclaimer in the documentation and/or other materials
 *  provided with the distribution. 
 *
 *  Neither the name of the owner nor the names of its contributors may be used to endorse or
 *  promote products derived from this software without specific prior written permission. 
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 *  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

 /* =======================================================================
  * =======================================================================
  * 
  * Module: sched_heur.cxx
  * $Revision: 1.1 $
  * $Date: 2005/12/30 01:50:23 $
  * $Author: weitang $
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_heur.cxx,v $
  *
  * Revision comments:
  * 
  * Description :
  * ==============
  *
  *  Implementation of code motion heuristic 
  *
  * =======================================================================
  * =======================================================================
  */

#include "tracing.h"
#include "errors.h"

#include "op.h"
#include "bb.h"
#include "be_util.h"
#include "cgtarget.h"
#include "gra_live.h"

#include "ipfec_defs.h"
#include "ipfec_options.h"

#include "region.h"
#include "dag.h"

#include "sched_util.h"
#include "sched_heur.h"
#include "sched_cflow.h"
#include "sched_cand.h"
#include "sched_rgn_info.h"

/* ===============================================================
 * 
 * Fuzzy_Cmp :
 * Make a comparison between two floating-point values. 
 *  
 * return 
 * - SIGNIFICANT_LESS  iff f1 < (f2 - deviation)
 * - SIGNIFICANT_GREAT iff f1 > (f2 + deviation)
 * - ROUGHLY_EQU       otherwize
 *
 * ===============================================================
 */
typedef enum {
    SIGNIFICANT_LESS ,
    SIGNIFICANT_GREAT, 
    ROUGHLY_EQU,
} F_CMP_RESULT ; 

static F_CMP_RESULT
Fuzzy_Cmp (float f1, float f2, float deviation) {

    if (f1 < f2 - deviation) {
        return SIGNIFICANT_LESS ;
    } else if (f1 > f2 + deviation) {
        return SIGNIFICANT_GREAT ;
    }

    return ROUGHLY_EQU;
}

enum { LD_VIOLATE_DATA_DEP_MAX=2,};
enum { LD_VIOLATE_CHK_DEP_MAX=1,} ;
enum { LD_VIOLATE_CNTL_DEP_MAX=LD_VIOLATE_CHK_DEP_MAX + 5,};



    /* =============================================================
     * =============================================================
     * 
     *          Implementation of class FAVOR_DELAY_HEUR 
     *
     * =============================================================
     * =============================================================
     */ 
FAVOR_DELAY_HEUR :: FAVOR_DELAY_HEUR (MEM_POOL* mp) : 
    _mp(mp), _heur_map (mp) {
    
    _target_block = NULL;
    _xfer_op      = NULL;

    _rgn_scope    = NULL;
    _bb_scope     = NULL;

    _initialize   = FALSE ;

    _trace_cand_sel = FALSE;
    _trace_file   = NULL;
}

FAVOR_DELAY_HEUR :: ~FAVOR_DELAY_HEUR (void) {
    
    _trace_cand_sel = FALSE ;

    if (_trace_file != TFile && _trace_file) {
        fclose(_trace_file);
        _trace_file = NULL ;
    }

}


void
FAVOR_DELAY_HEUR :: Reset_BB_OPs_etime (BB *bb) {
    OP * op ;
    FOR_ALL_BB_OPs (bb, op) {
       Set_OP_etime (op, 0); 
    }
}


void
FAVOR_DELAY_HEUR :: Find_Significant_Pred_For_Target_Blk (void) {

    if (!Is_In_Global_Scope ()) {
       _significant_pred  = NULL;
       return ;
    }

        /* find out the immediate pred which affect the schedule 
         * of <_target_block> greatly.
         */
    float v=0.0f;
    REGIONAL_CFG_NODE* node = Regional_Cfg_Node(_target_block);
    _significant_pred = NULL;

    for (REGIONAL_CFG_EDGE* e = node->First_Pred () ;
         e; 
         e = e->Next_Pred ()) {

        REGIONAL_CFG_NODE* pred = e->Src ();

            /* assume the nested region and call hide *ANY* latency
             */
        if (pred->Is_Region ()) continue ;

        BB* b = pred->BB_Node ();
        if (BB_call(b)) continue ; 

        float t = BB_freq(b) * e->Prob ();

        if (t > v) { _significant_pred = b; }
    }
}

void
FAVOR_DELAY_HEUR :: Estimate_Cand_Etime (OP* op) {

    if (!_significant_pred) {
        Set_OP_etime (op, 0);
        return ;
    }

    for (ARC_LIST* list = OP_preds(op);
         list ; 
         list = ARC_LIST_rest(list)) {

         ARC* arc  = ARC_LIST_first (list) ;
         OP* pred = ARC_pred (arc);

         mINT16 latency = ARC_latency (arc); 
         if (latency <= 1) { continue ;}

         if (OP_bb(pred) != _significant_pred) {
            continue;
         }

         INT32 start_cyc = OP_scycle(pred) + latency - 
                            BB_cycle(_significant_pred);

         if (start_cyc > 0) {
            Set_OP_etime (op, start_cyc);
         }

    } /* end of for(ARC_LIST*...) */
}

void
FAVOR_DELAY_HEUR :: Adjust_Etime_For_Target_Block (void) {
    
    OP* op;
    FOR_ALL_BB_OPs(_target_block, op) {
        Estimate_Cand_Etime (op);
    }
}

void
FAVOR_DELAY_HEUR :: Reset_BB_OPs_etime (const BB_VECTOR *bbs) {

    for (BB_VECTOR_CONST_ITER iter = bbs->begin () ; 
         iter != bbs->end () ; iter++) {

        Reset_BB_OPs_etime (*iter);

    }
}


BOOL
FAVOR_DELAY_HEUR :: BB_Need_Adjusting_Delay (BB *bb) {

    if (BB_scheduled(bb) || 
        BB_Is_Isolated_From_Sched (bb)) {
        return FALSE ;    
    }

    BB_HEUR_STUFF * bb_info = Get_BB_Heur_Stuff (bb) ;
    return (BOOL)bb_info->_heur_need_adjust;
}

void
FAVOR_DELAY_HEUR :: Set_BB_Need_Adjusting_Delay (BB *bb) {

    if (BB_scheduled(bb) || BB_Is_Isolated_From_Sched (bb)) {
        DevWarn ("BB:%d has been scheduled or isolated but "
                 "it is still required to adjust Delay", 
                 BB_id (bb));
    }
        
    BB_HEUR_STUFF * bb_info = Get_BB_Heur_Stuff (bb) ;
    bb_info-> _heur_need_adjust = TRUE;
}

void
FAVOR_DELAY_HEUR :: Set_BB_Need_Not_Adjusting_Delay (BB * bb) {

    BB_HEUR_STUFF* bb_info = Get_BB_Heur_Stuff (bb) ;
    
    if (bb_info) {
        bb_info-> _heur_need_adjust = FALSE ;
    }
}

/* =============================================================
 *
 *  Compute_Delay 
 *  
 *  Compute delay for a paticular op 
 *
 * ============================================================
 */
void
FAVOR_DELAY_HEUR :: Compute_Delay (
    OP *op, SUCC_INFO * succ_info, INT32 succ_num) {

    BOOL Is_Leaf = TRUE ;  
    BOOL delay_base_on_home_bb_op = TRUE;

    OP_HEUR_INFO *op_info = Get_OP_Heur_Info (op) ;
    BB * home_bb = OP_bb(op);

    float max_delay_base_on_home_bb = -1.0f ;
    float max_delay_base_on_succ_bb = -1.0f ;

    if (succ_info) {
        for (INT i = 0 ; i < succ_num ; i++) { 
            succ_info[i].max_delay = 0.0f ; 
        }
    }

    for (ARC_LIST* list = OP_succs(op);
        list ; list = ARC_LIST_rest(list)) {
        ARC * arc = ARC_LIST_first (list) ;
        OP  * succ = ARC_succ (arc);

        switch (ARC_kind(arc)) {
        case CG_DEP_POSTBR :
        case CG_DEP_PREBR :
             continue ;

#ifdef TARG_IA64
        case CG_DEP_PRECHK :
        case CG_DEP_POSTCHK :
             if (!OP_chk(op)) continue ;
             break ;
#endif
        }

        BB* descendant_bb = OP_bb(succ);
        Is_Leaf = FALSE ;
        mINT16 latency = ARC_latency(arc) ;
        
        if (ARC_kind(arc) == CG_DEP_POSTBR) {
            latency = 1 ;
            for (INT i = 0 ; i < succ_num ; i++) {
                if (descendant_bb == succ_info[i].succ) {
                    latency = succ_info[i].flow_shift_latency ;
                    break ;
                }
            }
        }


        float delay = latency + Get_OP_Heur_Info(succ)->_delay;

        if (home_bb == descendant_bb) {
            max_delay_base_on_home_bb = (max_delay_base_on_home_bb > delay) ? 
                                        max_delay_base_on_home_bb : delay ;

        } else {

            delay_base_on_home_bb_op = FALSE ;

            if (!succ_info) { 
                max_delay_base_on_succ_bb = 
                    (max_delay_base_on_succ_bb > delay) ? 
                     max_delay_base_on_succ_bb : delay ;
            } else {
                for (INT i = 0 ; i < succ_num ; i++) {
                    if ((descendant_bb == succ_info[i].succ || 
                        _cflow_mgr->BB1_Reachable_From_BB2 (
                            descendant_bb,succ_info[i].succ)) &&
                        succ_info[i].max_delay < delay) {
                        succ_info[i].max_delay = delay ;
                    }
                }
            }

        }
    }


    if (Is_Leaf) { 
        if (TOP_is_xfer(OP_code(op))) { op_info->_delay = 1.0f ; } 
        else if (OP_load(op)) { op_info->_delay = 2.0f ; } 
#ifdef TARG_SL
        else if (TOP_is_mvtc(OP_code(op))) { op_info->_delay = 5.0f; }
#endif
        else { op_info->_delay = 1.0f ; }  ;

        return ;
    }

    if (delay_base_on_home_bb_op) {
        op_info->_delay = op_info->_delay > max_delay_base_on_home_bb ? 
                          op_info->_delay : max_delay_base_on_home_bb ;
        return ;
    }

    if (succ_info) { 
        max_delay_base_on_succ_bb = 0.0f;

        for (INT i = 0 ; i < succ_num ; i++) {
            max_delay_base_on_succ_bb += 
                succ_info[i].max_delay * succ_info[i].reach_prob ;
        }
    }

    float f = max_delay_base_on_succ_bb > max_delay_base_on_home_bb ? 
              max_delay_base_on_succ_bb : max_delay_base_on_home_bb ;
    op_info->_delay = op_info->_delay > f ? op_info->_delay : f ;
    
}

/* ====================================================================
 *
 *  Compute_Delay 
 * 
 *  compute delay for each OP of <bb> 
 *
 * ===================================================================
 */
void
FAVOR_DELAY_HEUR :: Compute_Delay (BB * bb) {

    /* reverse-topological traverse ops 
     */
    if (!BB_Need_Adjusting_Delay (bb)) { return ; }

    REGIONAL_CFG_NODE * node = NULL; 
    REGIONAL_CFG  * cfg = NULL ;
    SUCC_INFO  succs_info[8], * psucc_info = NULL;
    INT32    succ_num = 0 ;

    if (Is_In_Global_Scope()) {

        node = Regional_Cfg_Node (bb) ;
        cfg  = Home_Region(bb)->Regional_Cfg () ;
        succ_num = node->Succ_Num ();
        
        if (succ_num > 8) {
            psucc_info = TYPE_ALLOCA_N(SUCC_INFO,succ_num) ;  
        } else {
            psucc_info = &succs_info[0] ;
        }

        INT32 bb_succ_num = 0 ;

        for (CFG_SUCC_EDGE_ITER iter(node); iter != 0; ++iter) {

            REGIONAL_CFG_EDGE * edge;

            edge = *iter;
            REGIONAL_CFG_NODE * succ_node = edge->Dest() ;
            
            if (succ_node->Is_Region()) continue ;
            BB* succ_bb = succ_node->BB_Node();

            psucc_info[bb_succ_num].succ = succ_bb ;
            psucc_info[bb_succ_num].flow_shift_latency = 
                (succ_bb == BB_next(bb)) ? 1 :
                CGTARG_Branch_Taken_Penalty ();
            psucc_info[bb_succ_num].reach_prob = cfg->Edge_Prob (edge) ;
            psucc_info[bb_succ_num++].max_delay = 0.0f;
        }
        
        succ_num = bb_succ_num ;
        if (!bb_succ_num) { psucc_info = NULL ; } 
    }

    OP * op ;
    FOR_ALL_BB_OPs_REV (bb, op) {
        Compute_Delay (op, psucc_info, succ_num);
    }

    Set_BB_Need_Not_Adjusting_Delay (bb);
}

/* ==============================================================
 *
 *  Compute_Delay 
 * 
 *  Compute delay for each OP initially resides in <rgn>(not 
 *  including nested region and PU's entry and exit BB).
 *  
 * =============================================================
 */
void 
FAVOR_DELAY_HEUR :: Compute_Delay (REGION *rgn) {

    for (REVERSE_TOPO_REGIONAL_CFG_ITER iter(rgn->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {
        if ((*iter)->Is_Region()) continue ;
        BB * bb = (*iter)->BB_Node ();
        if (BB_exit (bb) || BB_entry (bb)) continue;

        Compute_Delay (bb);
    }
    
}

void
FAVOR_DELAY_HEUR :: Adjust_Delay (REGION *rgn) {

    for (REVERSE_TOPO_REGIONAL_CFG_ITER iter(rgn->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {
        if ((*iter)->Is_Region()) continue ;

        BB * bb = (*iter)->BB_Node () ;
        if (BB_entry (bb) || BB_exit(bb) || 
            !BB_Need_Adjusting_Delay (bb)) {
            continue ;
        }

        Compute_Delay (bb);
    }
}


void
FAVOR_DELAY_HEUR :: Compute_FanOut (OP* op) {

    INT fanout = 0 ;

    for (ARC_LIST* list = OP_succs(op);
        list ; list = ARC_LIST_rest(list)) {

        ARC * arc = ARC_LIST_first (list) ;
        switch (ARC_kind (arc)) {
#ifdef TARG_IA64
        case CG_DEP_PRECHK :
#endif
        case CG_DEP_PREBR : 
            break ;
        default :
            ++ fanout ;
        }
    }

    OP_HEUR_INFO  * info = Get_OP_Heur_Info (op) ;
    fanout = max(fanout,1);

    info -> _fan_out = fanout ;
}

void 
FAVOR_DELAY_HEUR :: Compute_FanOut_For_All_OP (BB *bb) {
    
    OP * op ;
    FOR_ALL_BB_OPs (bb, op) {
        Compute_FanOut (op);
    }
}

void
FAVOR_DELAY_HEUR :: Compute_FanOut_For_All_OP (REGION *rgn) {

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter (rgn->Regional_Cfg()) ;
        iter != 0 ; ++ iter) {
        
        if ((*iter)->Is_Region ()) continue ;

        BB * bb = (*iter)->BB_Node () ;
        if (BB_entry (bb) || BB_exit (bb)) continue ;

        Compute_FanOut_For_All_OP (bb);
    }
}

void
FAVOR_DELAY_HEUR :: Alloc_Heur_Data (BB * bb) {

    BB_HEUR_STUFF * bb_heur_stuff = 
        CXX_NEW (BB_HEUR_STUFF(bb,_mp), _mp);

    _heur_map.Set_Map (bb, bb_heur_stuff) ;
}

void
FAVOR_DELAY_HEUR :: Alloc_Heur_Data (REGION * rgn) {

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(rgn->Regional_Cfg());
         iter != 0; ++iter) {
        
        if ((*iter)->Is_Region ()) continue; 

        BB * bb = (*iter)->BB_Node ();
        if (BB_entry(bb) || BB_exit(bb)) continue ;

        Alloc_Heur_Data (bb); 
    }
}


void
FAVOR_DELAY_HEUR :: Initialize (
        REGION *rgn, 
        RGN_CFLOW_MGR* cflow_mgr) {

    Is_True (!_rgn_scope && !_bb_scope, ("Initialized twice"));

    _initialize = TRUE; 

    _rgn_scope = rgn ;
    _cflow_mgr = cflow_mgr;

    Alloc_Heur_Data (rgn);

    Compute_Delay (rgn);

    Compute_FanOut_For_All_OP (rgn);

    _trace_file = TFile;
    _trace_cand_sel = SCHED_TF_CANDSEL_DUMP ;
#if defined(TARG_SL)
    _size_spec = FALSE;
    _unpaired_16bit = FALSE;
    _zdl_op_tag = 0;
#endif
}

void
FAVOR_DELAY_HEUR :: Initialize (BB *bb, RGN_CFLOW_MGR* cflow_mgr) {

  Is_True (!_rgn_scope && !_bb_scope, ("Initialized twice"));

  _bb_scope = bb;
  _cflow_mgr = cflow_mgr;

#if defined(TARG_SL) || defined(TARG_SL2)
  //set control flag for round robin candidate selection
  // the -CG:cand_pattern="" option inputs non pattern to disable this model
  if(Cand_List_Pattern) 
    Get_Round_Robin_Model();
  
  if( _cand_sel_model.size() > 0 )
    Enable_Round_Robin_Cand_Sel(); 
  else 
    Disable_Round_Robin_Cand_Sel(); 
#endif


  Alloc_Heur_Data (bb);

  Set_BB_Need_Adjusting_Delay (bb);
  Compute_FanOut_For_All_OP (bb);

  _trace_file = TFile;
  _trace_cand_sel = SCHED_TF_CANDSEL_DUMP ;
    
  _initialize = TRUE; 

#if defined(TARG_SL)
  _size_spec = FALSE;
  _unpaired_16bit = FALSE;
  _zdl_op_tag = 0;
#endif	
}


void
FAVOR_DELAY_HEUR :: Finialize  (void) {
}

void *
FAVOR_DELAY_HEUR :: Detach_OP_Heur_Info (OP * op) {

    BB_HEUR_STUFF * bb_heur_stuff = 
        (BB_HEUR_STUFF *)_heur_map.Get_Map (OP_bb(op)) ;
    Is_True (bb_heur_stuff, 
             ("fail to get BB:%d's heuristic stuff!", 
              BB_id(OP_bb(op))));

    OP_HEUR_INFO * op_info = bb_heur_stuff->Get_OP_Heur_Info (op) ;
    bb_heur_stuff->Set_OP_Heur_Info (op, NULL);
    
    return op_info;
}


void
FAVOR_DELAY_HEUR :: Attach_OP_Heur_Info (OP * op, void * Heur_Data) {
    
    OP_HEUR_INFO * info = (OP_HEUR_INFO *)Heur_Data;

    if (info && 
        (info->_magic_num != OP_HEUR_INFO_MAGIC_NUM ||
         info->_op != op)) { 
        Fail_FmtAssertion (
            "It is not a valid heuristic structure for OP[%d] (BB:%d)!\n",
            OP_map_idx(op), BB_id(OP_bb(op)));
    }

    BB_HEUR_STUFF * bb_heur_stuff = 
        (BB_HEUR_STUFF *)_heur_map.Get_Map (OP_bb(op)) ;
    
    bb_heur_stuff->Set_OP_Heur_Info (op, info);
}


void
FAVOR_DELAY_HEUR :: Adjust_Heur_Stuff_When_BB_Changed 
    (BB * new_target,SRC_BB_MGR& src_bb_mgr) {

    Is_True (_initialize, ("FAVOR_DELAY_HEUR has not yet initialized!"));
    Is_True (_rgn_scope || _bb_scope == new_target, 
             ("target bb change while scheduling scope is not 'global'"));

    _target_block = new_target ;
    _xfer_op      = BB_xfer_op(new_target);

    const BB_VECTOR * bbs = src_bb_mgr.Src_BBs ();
    Reset_BB_OPs_etime (bbs);

    if (Is_In_Global_Scope ()) {
        /* compute delay for some BBs's OP if needed 
         */
        Compute_Delay (_rgn_scope);
    } else {
        Compute_Delay (_bb_scope);
    }

    Find_Significant_Pred_For_Target_Blk ();

    if (Is_In_Global_Scope ()) {
        Adjust_Etime_For_Target_Block ();
    }
}

void 
FAVOR_DELAY_HEUR :: Compute_Heur_Data_For_Inserted_OP (OP *op) {

    OP_HEUR_INFO * heur_info = Get_OP_Heur_Info (op);

    if (!heur_info) {
        heur_info = CXX_NEW (OP_HEUR_INFO(op),_mp); 
        Attach_OP_Heur_Info (op, heur_info);
    }
    
    if (!Is_In_Global_Scope ()) {
        Compute_Delay (op, NULL, 0);
        return ;
    }

    REGIONAL_CFG_NODE * node = NULL; 
    REGIONAL_CFG  * cfg = NULL ;
    SUCC_INFO  succs_info[8], * psucc_info = NULL;
    INT32    succ_num = 0 ;

    node = Regional_Cfg_Node (OP_bb(op)) ;
    cfg  = Home_Region(OP_bb(op))->Regional_Cfg () ;
    succ_num = node->Succ_Num ();
        
    if (succ_num > 8) {
        psucc_info = TYPE_ALLOCA_N(SUCC_INFO,succ_num) ;  
    } else {
        psucc_info = &succs_info[0] ;
    }

    INT32 bb_succ_num = 0 ;

    for (CFG_SUCC_EDGE_ITER iter(node); iter != 0; ++iter) {

        REGIONAL_CFG_EDGE * edge ;
        edge = *iter ;

        REGIONAL_CFG_NODE * succ_node = edge->Dest() ;
            
        if (succ_node->Is_Region()) continue ;
        BB * succ_bb = succ_node->BB_Node();

        psucc_info[bb_succ_num].succ = succ_bb ;
        psucc_info[bb_succ_num].reach_prob = cfg->Edge_Prob (edge) ;
        psucc_info[bb_succ_num].flow_shift_latency = 1 ;
        psucc_info[bb_succ_num++].max_delay = 0.0f;
    }
        
    succ_num = bb_succ_num ;
    if (!bb_succ_num) { psucc_info = NULL ; } 

    Compute_Delay (op, psucc_info, succ_num);
    Compute_FanOut (op);
}

void
FAVOR_DELAY_HEUR :: Compute_Heur_Data_For_Appended_OP (OP *op) {
    Compute_Heur_Data_For_Inserted_OP (op);
}

void 
FAVOR_DELAY_HEUR :: Compute_Heur_Data_For_Prepended_OP (OP *op) {
    /* not yet implemented */
}


/* ============================================================
 *
 * Exclude_Unqualifed_Cand_Under_Etime_Constraint
 * 
 * used only by <Select_Best_Candidate> from A given list. 
 *
 * o. set candidate *TRIED* if it does not satisfy the etime
 *    consttraint.
 * 
 * o. return eariest issue-cycle of qualified candidates,
 *    ( >= constraint->threshold),CYCLE_MAX iff there are 
 *    no qualified candidates. 
 *
 * ============================================================
 */

CYCLE
FAVOR_DELAY_HEUR::Exclude_Unqualifed_Cand_Under_Etime_Constraint
    (CAND_LIST& cand_lst, E_Time_Constraint* constraint) {

    CYCLE e_time = CYCLE_MAX ;

    OP_HEUR_INFO* op_sched_info = NULL;

    if (_trace_cand_sel) {

        fputs ("Exclude cand not qualified due to etime::\n", _trace_file);
    }

    if (constraint->constraint == NO_LATER) {

        for (CAND_LIST_ITER cand_iter(&cand_lst); 
            !cand_iter.done (); cand_iter.step ()) {

            CANDIDATE * cand = cand_iter.cur ();

            if (cand_lst.Cand_Has_Been_Tried (cand)) {
                continue ;
            }

            mINT16 etime_tmp = (mINT16)Get_Cand_Issue_Cyc (cand);
            if (etime_tmp > constraint->threshold) {
                /* ignore candidate which can not be issued 
                 * at cycle <etime_threshold>
                 */
                cand_lst.Set_Cand_Has_Been_Tried (cand);
                if (_trace_cand_sel) {
                      
                    fprintf (_trace_file,
                             "[OP%3d][BB%3d] not satisfy etime constraint (%d>%d)\n",
                             OP_map_idx(cand->Op()),
                             BB_id(OP_bb(cand->Op())),
                             etime_tmp,
                             constraint->threshold);

                }

                continue ;
            }

            e_time = min(etime_tmp, e_time);
        }

        return e_time ;
    }


    Is_True (constraint->constraint == AS_EARLY_AS_POSSIBLE, 
             ("Unknown e-time constraint type(%d)", 
              constraint->constraint));

    for (CAND_LIST_ITER cand_iter(&cand_lst); 
         !cand_iter.done (); cand_iter.step ()) {

        CANDIDATE* cand = cand_iter.cur ();

        if (cand_lst.Cand_Has_Been_Tried (cand)) {
            continue ;
        }

        mINT16 etime_tmp = (mINT16)Get_Cand_Issue_Cyc (cand);
        e_time = min (etime_tmp, e_time);  
    }
    
    if (e_time == CYCLE_MAX) return CYCLE_MAX ;
    if (constraint->threshold != CYCLE_MAX) {
        e_time = max(e_time, constraint->threshold);
    }

    E_Time_Constraint tmp ;
    tmp.threshold  = e_time ;
    tmp.constraint = NO_LATER ;

    return Exclude_Unqualifed_Cand_Under_Etime_Constraint (
                cand_lst, &tmp); 

}

/* ===============================================================
 * 
 *  Choose_Better_Of_Tie 
 *
 *  From <Select_Best_Candidate()>'s perspective, <cand1> is not 
 *  better, and also not worse, than <cand2>, as the last resort, 
 *  it calls this routine to choose a better one.
 *
 *  return <cand1> if Choose_Better_Of_Tie itself still could not 
 *  determine which one is better, otherwise, return the better 
 *  one.
 *
 * ===============================================================
 */
CANDIDATE*
FAVOR_DELAY_HEUR :: Choose_Better_Of_Tie 
    (CANDIDATE * cand1, OP_HEUR_INFO * info1, 
     CANDIDATE * cand2, OP_HEUR_INFO * info2,
     BOOL comes_from_targ_bb) {

   /* rule 1: 
    *     favor non-speculative candidate than speculative one.
    */
    if (!cand1->Is_Spec () && cand2->Is_Spec ()) {
        return cand1 ;
    } else if (cand1->Is_Spec () && !cand2->Is_Spec ()) {
        if (_trace_cand_sel) {
            fprintf (_trace_file, 
                     "favor  [OP%3d][BB%3d] : Non-speculative\n",
                     OP_map_idx(cand2->Op()),
                     BB_id(OP_bb(cand2->Op())));
        }
        return cand2 ;
    }

    if (!comes_from_targ_bb) {

        /* rule 2:
         *  favor candidate with larger delay.
         *
         * An auxiliary to routine Select_Best_Candidate which
         * favor candidate with larger (delay * reach-probability).
         */

        switch ((INT)Fuzzy_Cmp(info1->_delay, info2->_delay, 0.005f)) {
        case SIGNIFICANT_LESS:

            if (_trace_cand_sel) {
                fprintf (_trace_file, 
                        "favor  [OP%3d][BB%3d] : Delay(not weight "
                        "by reach prob) (%f > %f)\n",
                        OP_map_idx(cand2->Op()),
                        BB_id(OP_bb(cand2->Op())),
                        info2->_delay, 
                        info1->_delay);
            }

            return cand2 ;
        case SIGNIFICANT_GREAT:
            return cand1 ;
        case ROUGHLY_EQU :
            break ;
        default :
            Fail_FmtAssertion ("Unknown fuzzy-comparison result\n");
        }
    }

    /* rule 3 : favor store over non-store OP 
     *
     */
#if defined(TARG_SL)
    if (OP_like_store(cand1->Op()) || OP_load(cand1->Op())) {
        return cand1 ;
    } else if (OP_like_store (cand2->Op())|| OP_load(cand2->Op())) {
#else
    if (OP_like_store(cand1->Op())) {
        return cand1 ;
    } else if (OP_like_store (cand2->Op())) {
#endif
        if (_trace_cand_sel) {
            fprintf (_trace_file,
                     "favor  [OP%3d][BB%3d] : Store\n",
                     OP_map_idx(cand2->Op()),
                     BB_id(OP_bb(cand2->Op())));
        }

        return cand2 ;
    }

    /* rule 4 : favor cadidate with more issueable ports
     */
    if (info1->_issuable_port_num < 
        info2->_issuable_port_num) {
        return cand1;
    } else if (info1->_issuable_port_num > info2->_issuable_port_num) {

        if (_trace_cand_sel) {
            fprintf (_trace_file,
                     "favor [OP%3d][BB%3d] : Less issuable ports (%d < %d)\n",
                     OP_map_idx(cand2->Op()),
                     BB_id(OP_bb(cand2->Op())),
                     info2->_issuable_port_num,
                     info1->_issuable_port_num);
        }

        return cand2;
    }
          

    /* rule 5 : favor candidate with more fan-outs.
     */

    INT int_tmp1, int_tmp2 ;

    int_tmp1 = info1->_fan_out ;
    int_tmp2 = info2->_fan_out ;

    if (int_tmp1 > int_tmp2) {
        return cand1 ; 
    } else if (int_tmp1 < int_tmp2) {

        if (_trace_cand_sel) {
            fprintf (_trace_file,
                     "favor [OP%3d][BB%3d] : More fan-outs (%d > %d)\n",
                     OP_map_idx(cand2->Op()),
                     BB_id(OP_bb(cand2->Op())),
                     int_tmp2, int_tmp1);
        }

        return cand2 ;
    }
    
    return cand1; 
}


#if defined(TARG_SL) || defined(TARG_SL2)
/////////////////////////////////////////////////////////
// Print_Model 
//
/////////////////////////////////////////////////////////
static void Print_One_Model( model_kind model, FILE *file )
{
  //print model name
  char model_name[20];
  switch( model ){
    case CAND_KIND_MEM:
      strncpy( model_name, " memory ", 8 ); 
      model_name[8] = 0;
      break; 
    case CAND_KIND_INTEGER:
      strncpy( model_name, " alu ", 5 ); 
      model_name[5] = 0;
      break; 
    case CAND_KIND_OTHER:
      strncpy( model_name, " other ", 7 ); 
      model_name[7] = 0;
      break; 
    default:
      break; 
  }
  fprintf( file, "%s ", model_name );
}

/////////////////////////////////////////////////////////
// Print_All_Models 
//
// This is to print out all the models in current model list
/////////////////////////////////////////////////////////
void FAVOR_DELAY_HEUR::Print_All_Models( FILE *file)
{
  fprintf( file, "All the models are: " );
  std::list<model_kind>::const_iterator cit = _cand_sel_model.begin();
  for( ; cit != _cand_sel_model.end(); cit++ )
    Print_One_Model( *cit, file );
  fprintf( file, "\n" );
}

BOOL
FAVOR_DELAY_HEUR::Cand_Is_Expected(CANDIDATE * cand)
{
  Is_True(!_cand_sel_model.empty(),  ("candidate selection model is empty"));
  model_kind expected_op_kind = *(_cand_sel_model.begin()); 

  // following switch statement need to be expanded to handle more kind of 
  // candidate op, and first need to category all op into different kind in 
  //file isa_properties.cxx
  BOOL ret = FALSE;
  switch(expected_op_kind){
    case CAND_KIND_MEM:
      if(OP_memory(cand->Op()))
        ret = TRUE;
      break;
    case CAND_KIND_INTEGER:
      if(!OP_memory(cand->Op()))
        ret = TRUE;
      break;
    case CAND_KIND_OTHER:
    default:
      ; 
  }
  return ret;
}

void 
FAVOR_DELAY_HEUR::Get_Round_Robin_Model()
{
  const char * s = Cand_List_Pattern; 
  if(!Cand_List_Pattern)
    return; 
  
  while (*s)
  {
    switch(*s++)
    {
      case 'I':
      case 'i': 
        _cand_sel_model.push_back(CAND_KIND_INTEGER); 
        break;
      case 'M':
      case 'm':
        _cand_sel_model.push_back(CAND_KIND_MEM); 
        break;
      default:
        break;
   }
  }
//  model.push_back(CAND_KIND_OTHER); 
  return; 
}

void
FAVOR_DELAY_HEUR::Update_Cand_Sel_Model_Status()
{
  model_kind kind;
  kind = *(_cand_sel_model.begin()); 
  _cand_sel_model.pop_front(); 
  _cand_sel_model.push_back(kind); 
  return; 
}
#endif 
/* =============================================================
 *
 *  Select_Best_Candidate 
 * 
 *  Select the best candidate from a given candidate-list
 *
 * ============================================================
 */
CANDIDATE*
FAVOR_DELAY_HEUR :: Select_Best_Candidate (
        CAND_LIST& cand_lst, 
        BB*        target,
        E_Time_Constraint* etime_constraint) {

  UINT8 nums_16bit_candi = 0;
  CANDIDATE *minor_candi = NULL;
  CYCLE e_time = Exclude_Unqualifed_Cand_Under_Etime_Constraint 
                  (cand_lst,etime_constraint);
  if (etime_constraint->threshold < e_time) {
      etime_constraint->threshold = e_time ;
  }

  if (e_time == CYCLE_MAX) {
    Is_True (cand_lst.All_Cands_Have_Been_Tried (), 
             ("Not all candidates have been tried!")) ;
    if (_trace_cand_sel) {
        fputs ("No candidate satisfy e-time constraint\n", _trace_file);
    }

    return NULL ;
  }

#ifdef Is_True_On 
  if (etime_constraint->constraint == NO_LATER) {
      Is_True (e_time <= etime_constraint->threshold,
              ("e_time(%d) > threshold (%d)\n", 
                e_time, etime_constraint->threshold));
  }
#endif 

  if( _trace_cand_sel ) {
    fprintf(TFile, "candidate list ===========================\n");
    for( CAND_LIST_ITER cand_iter(&cand_lst);
        !cand_iter.done (); cand_iter.step ()) {
      CANDIDATE* cand = cand_iter.cur ();
      OP_HEUR_INFO * op_info = Get_OP_Heur_Info (cand->Op()) ;
      fprintf(TFile,"[OP%3d](%f)",  OP_map_idx(cand->Op()), op_info->_delay);
      Print_OP_No_SrcLine(cand->Op());
    }
  }

  /* count numbers of 16 bit instruction in cand_list if scheduling for code size */
  if (Sched_For_Codesize_Spec()) {      
    for (CAND_LIST_ITER cand_iter(&cand_lst); !cand_iter.done(); cand_iter.step()) {
      CANDIDATE* cand = cand_iter.cur();		
      if (OP_16bit_op(cand->Op()) && !cand_lst.Cand_Has_Been_Tried(cand))
        ++nums_16bit_candi;  
      }
  }

  CANDIDATE *best_cand_of_other_bb = NULL, 
            *best_cand_of_target_bb = NULL ;

  float weigh_delay_of_best_cand_of_other_bb = -100.0f;
  float delay_of_best_cand_of_target_bb = -1.0f;

  OP_HEUR_INFO *targ_bb_cand_heur = NULL,
               *other_bb_cand_heur = NULL ;

#if defined(TARG_SL) || defined(TARG_SL2)
  BOOL has_expected_op; 
  has_expected_op = FALSE;
  if(Round_Robin_Cand_Sel()){
    for( CAND_LIST_ITER cand_iter(&cand_lst); 
         !cand_iter.done(); cand_iter.step()) {
      CANDIDATE *cand = cand_iter.cur();
      if(!cand_lst.Cand_Has_Been_Tried(cand) && Cand_Is_Expected(cand)) {
        if (!Prev_unpaired_16bit() || (Prev_unpaired_16bit() && OP_16bit_op(cand->Op()))) {
           has_expected_op = TRUE;
           break;
        }
      }
    }

    if( _trace_cand_sel )
      Print_All_Models(TFile);
  }
#endif 
  // This outer loop is to solve the problem: if all the candidates are
  // ignored by model, or by structure hazards, then I need to release
  // the model constraints. Whatever, there MUST be best candidates after
  // I release the model constraints.
  //
  // If the best candidate is found after release model constraints,
  // then we will NOT shift our resource model since we still hope to find
  // the next suitable instruction for our model.
  
  INT32 while_count = 0;
  CANDIDATE * best;
  while(1){
    while_count++;
    Is_True( while_count < 3, ("failed to find best candidates") );

    // from the second time on, we release the model constraints
    // by setting has_expected_op to FALSE;
    if( while_count > 1 ){
      has_expected_op = FALSE;
      if( _trace_cand_sel ){
        fprintf( TFile, " Cannot find best cand with model constraint, " );
        fprintf( TFile, " now release the model\n " );
      }
    }else{
      if( !has_expected_op && _trace_cand_sel ){
        fprintf( TFile, " We have no model-matching cand OP at the first " );
        fprintf( TFile, " while(1) iteration \n" );
      }
    }

    CAND_LIST_ITER cand_iter;
    cand_iter.set_cand_list( &cand_lst );

    /* loop over candidate list for the "best" candidate */
    for( ; !cand_iter.done (); cand_iter.step ()) {
      CANDIDATE* cand = cand_iter.cur ();
      BOOL cur_16bit = OP_16bit_op(cand->Op());
      /* ignore some candidates that have already been tried */
      if( cand_lst.Cand_Has_Been_Tried (cand) ) 
        continue;
      // if numbers of 16 bit op in candidate list are larger than 0, 
      // ignore 32 bit candidate for previous committed unpaired 16 bit instructions
      if (Prev_unpaired_16bit() && (nums_16bit_candi > 0) && (!cur_16bit)) 
        continue;
#if defined(TARG_SL) || defined(TARG_SL2)
      if (Round_Robin_Cand_Sel() && 
          has_expected_op && 
          !Cand_Is_Expected(cand)){
        if( _trace_cand_sel )
          fprintf( TFile, "Skip OP[%3d] by model\n", OP_map_idx(cand->Op()) );
        continue; 
      }
#endif

      BB* home_bb  = OP_bb (cand->Op ());
      OP_HEUR_INFO * op_sched_info = Get_OP_Heur_Info (cand->Op ()) ;
      #define DELAY_DEVIATION (0.05f)
      BOOL choose_current = FALSE ;
      // We distinguish candidate comes from target-bb and 
      // other-than-target-bb.
      if( home_bb == target ){
        // select the better between <cand> and <targ_bb_best_cand>. 
        // delay of cadidate is out major concern. 
        switch( Fuzzy_Cmp (op_sched_info->_delay, 
                           targ_bb_cand_heur ? 
                           targ_bb_cand_heur->_delay : -1.0f,
                           DELAY_DEVIATION)) {
        case SIGNIFICANT_LESS:
        { 
          if (Sched_For_Codesize_Spec())
            minor_candi = cand;
          continue;
        }
        case SIGNIFICANT_GREAT:
        {
          if (_trace_cand_sel) {
            fprintf (_trace_file,
                     "Best cand => to [OP%3d][BB%3d]"
                     ": Delay(%f) greater\n",
                     OP_map_idx(cand->Op()),
                     BB_id(OP_bb(cand->Op())),
                     op_sched_info->_delay);
          }
          if (Sched_For_Codesize_Spec()) {
            // last committed op is 16 bit 
            if (Prev_unpaired_16bit()) {
              if (cur_16bit) {
                choose_current = TRUE;
                break;
              }
            } else {
              // current op is 32-bit , select current op  or 
              // more than two 16-bit ops in current cand-list	
              if((!cur_16bit) || (cur_16bit && (nums_16bit_candi > 1))){
               	choose_current = TRUE;
               	break;
              }
            } 
            //need insert nop16
            minor_candi = cand;
          } else {
            choose_current = TRUE; 
          }
          break;
        }
        case ROUGHLY_EQU:
        {
          CANDIDATE* tmp = Choose_Better_Of_Tie(best_cand_of_target_bb,
                                                targ_bb_cand_heur,
                                                cand, op_sched_info,
                                                TRUE); 
          if (tmp != best_cand_of_target_bb) {
            if (Sched_For_Codesize_Spec()) {
              /* last committed op is unpaired 16 bit */
              if (Prev_unpaired_16bit()) {
                if (cur_16bit) {
                  choose_current = TRUE;
                  break;
                }
              } else {
                // current op is 32 bit or 
                // current op is 16-bit and more than two 16-bit ops in cand-list	
                if ((!cur_16bit) || (cur_16bit && (nums_16bit_candi > 1))) {
                  choose_current = TRUE;
                  break ;
                }
              } 
              /* need insert nop16 */
              minor_candi = cand;			
            } else {
              choose_current = TRUE ;
            }
          }
          break; 
        }
        default:
            Fail_FmtAssertion ("Unknown fuzzy-comparision result\n");
        } /* end of switch */

        if (choose_current) {
            best_cand_of_target_bb = cand; 
            delay_of_best_cand_of_target_bb = op_sched_info->_delay;
            targ_bb_cand_heur = op_sched_info ;
        }
      }else{
        INT32 reach_prob = 
            _cflow_mgr->Reachable_Prob (_target_block,home_bb);

        float delay = op_sched_info->_delay * reach_prob;
        switch (Fuzzy_Cmp (delay, 
                           weigh_delay_of_best_cand_of_other_bb, 
                           DELAY_DEVIATION * REACH_PROB_SCALE)) {
        case SIGNIFICANT_LESS:
            break ;
        case SIGNIFICANT_GREAT:
            if (_trace_cand_sel) {
                fprintf (_trace_file,
                         "Best cand (non-targ-bb) => [OP%3d][BB%3d] "
                         ": Delay greater (%f)\n",
                         OP_map_idx(cand->Op()),
                         BB_id(OP_bb(cand->Op())),
                         op_sched_info->_delay);
            }
            choose_current = TRUE; 
            break ;
        case ROUGHLY_EQU :
            if (Choose_Better_Of_Tie (best_cand_of_other_bb,
                                      other_bb_cand_heur,
                                      cand, op_sched_info,
                                      FALSE)
                   != best_cand_of_other_bb) {
                choose_current = TRUE ;
            }

            break ;
        default :
            Fail_FmtAssertion ("Unknown fuzzy-comparison result\n");
        } /* end of switch*/

        if (choose_current) {
           best_cand_of_other_bb = cand ;
           weigh_delay_of_best_cand_of_other_bb = delay;
           other_bb_cand_heur = op_sched_info ;
        }
      } /* end of else clause */
    } /* end of for loop */

    if (best_cand_of_other_bb &&
        weigh_delay_of_best_cand_of_other_bb > 
            delay_of_best_cand_of_target_bb*REACH_PROB_SCALE &&
        !best_cand_of_other_bb->Is_Spec()) {
        best = best_cand_of_other_bb;
    } else {
        best = best_cand_of_target_bb ? 
                   best_cand_of_target_bb:
                   best_cand_of_other_bb;                           
    }
    
    if(best)
      break;
    if (Sched_For_Codesize_Spec() && !best) {
      best = minor_candi;
      break;
    } 
  }// end of while(1) 

  Is_True(best != NULL, ("there are must be at least one candidate"));
  best->Set_Match_Model( has_expected_op );
  if( _trace_cand_sel) {
      OP_HEUR_INFO * op_info = Get_OP_Heur_Info (best->Op ()) ;
    fprintf( _trace_file,
             "Final best cand::  [OP%3d][BB%3d](%f) : ",
             OP_map_idx(best->Op()), 
             BB_id(OP_bb(best->Op())),op_info->_delay);
    Print_OP_No_SrcLine(best->Op());
  }

  return best ; 
} /* Select_Best_Candidate */

/* ================================================================
 *
 *  Select_Best_Candidate 
 * 
 *  select the best candidate from dual lists (m-ready and p-ready
 *  candidate lists). 
 * 
 * ================================================================
 */
CANDIDATE*
FAVOR_DELAY_HEUR :: Select_Best_Candidate (
    CAND_LIST& m_ready_cand_lst,
    CAND_LIST& p_ready_cand_lst, 
    BB*        target,
    E_Time_Constraint* etime_constraint) {

    E_Time_Constraint Metc_tmp = *etime_constraint;

    if (IPFEC_Stress_Spec) {
       return Select_Best_Candidate_For_Stress_Spec_Purpose ( 
                            m_ready_cand_lst,
                            p_ready_cand_lst,
                            target,
                            etime_constraint);
    }
	
        /* 1st : quest for a best M-ready candidate 
         *       under the e-time constraint.
         */
    CANDIDATE* m_cand = Select_Best_Candidate (
                            m_ready_cand_lst, 
                            target,
                            &Metc_tmp);

    if (m_cand) {

        /* NOTE: Do not return <m_cand>! we should make comparison 
         *       between M-ready and P-ready candidates to choose 
         *       a select candidate.
         */       
        /* return m_cand ; */

    } else {
        Is_True (m_ready_cand_lst.All_Cands_Have_Been_Tried (),
                 ("There are at least one M-ready candidate "
                  "for BB:%d\n", BB_id (target)));
    }

        /* 2nd ; select a best P-ready candidate (if any)
         */
    CANDIDATE* p_cand = NULL;
    E_Time_Constraint Petc_tmp = *etime_constraint;

    if (p_ready_cand_lst.All_Cands_Have_Been_Tried () || 
        !(p_cand = Select_Best_Candidate 
            (p_ready_cand_lst, target, &Petc_tmp))) {

        *etime_constraint = Metc_tmp ;
        return m_cand;      
    }

        /* 3rd : make comparison between "best" M-ready candidate 
         *       and "best" P-ready candidate
         */
    BOOL choose_m_cand = TRUE;

    if (!m_cand) {
        choose_m_cand = FALSE;
    } else {
            /* we have M-ready cand P-ready candidates.
             */
        switch (etime_constraint->constraint) {
        case AS_EARLY_AS_POSSIBLE:
            {
            CYCLE immediate_prev_cyc = etime_constraint->threshold;
            if (Metc_tmp.threshold > immediate_prev_cyc + 1 &&
                Metc_tmp.threshold > Petc_tmp.threshold) {
                choose_m_cand = FALSE;
            }
            }
            break;

        case NO_LATER:
                /* both M-ready cand and P-ready candidates can 
                 * be issued at cycle <etime_constraint.threshold.
                 * We favor M-ready candidate over P-ready one.
                 */
            break;

        default:
            FmtAssert (FALSE, ("Unknown constraint!"));
        }
    }

    if (choose_m_cand) {
        *etime_constraint = Metc_tmp;
        return m_cand;
    }

    *etime_constraint = Petc_tmp;
    return p_cand; 
}


CYCLE
FAVOR_DELAY_HEUR :: Get_Cand_Issue_Cyc (CANDIDATE *cand) {
    return Get_OP_Heur_Info (cand->Op()) -> _e_time ;
}

void
FAVOR_DELAY_HEUR :: Adjust_Heur_After_Cand_Sched 
    (OP *op, CYCLE issue_cyc) {

    for (ARC_LIST* arcs = OP_succs(op); 
        arcs != NULL; 
        arcs = ARC_LIST_rest(arcs)) {

        ARC * arc       = ARC_LIST_first(arcs);
        OP  * succ      = ARC_succ(arc);
        mUINT16 latency = ARC_latency (arc);

        OP_HEUR_INFO * op_info = Get_OP_Heur_Info (succ) ;

        Is_True (op_info,
                ("OP[%d] of BB:%d has no heuristic data!",
                OP_map_idx(succ), BB_id(OP_bb(succ))));

        CYCLE succ_issue_cyc = 
            op_info->_etime_set_by_which_bb == OP_bb(op) ? 
            op_info->_e_time : 0 ;
            
        if (!latency) {
            succ_issue_cyc = max(issue_cyc, succ_issue_cyc) ; 
        } else if (issue_cyc + 1 >= succ_issue_cyc) {
            succ_issue_cyc = issue_cyc + 1;
        }

        op_info->_e_time = succ_issue_cyc ;
        op_info->_etime_set_by_which_bb = _target_block ;
    }
}

    /* ===========================================================
     *
     *  Adjust_Heur_After_Sched_One_Cyc 
     *
     *  ref the header file for details.
     *
     *===========================================================
     */
void
FAVOR_DELAY_HEUR:: Adjust_Heur_After_Sched_One_Cyc 
    (OP_Vector& op_vect, CYCLE issue_cyc) {

    for (OP_Vector_Iter iter = op_vect.begin () ;
         iter != op_vect.end () ; iter ++) {

        OP * op = *iter ;
        for (ARC_LIST* arcs = OP_succs(op); 
            arcs != NULL; 
            arcs = ARC_LIST_rest(arcs)) {

            ARC* arc  = ARC_LIST_first(arcs);
            OP*  succ = ARC_succ(arc);

#ifdef TARG_IA64
            extern BOOL Is_MMX_Dependency (OP*, OP*,CG_DEP_KIND) ;
            extern INT32 MMX_Dep_Latency (void);

            if (Is_MMX_Dependency (ARC_pred(arc), ARC_succ(arc),
                                   ARC_kind(arc))) {
                INT32 l = MMX_Dep_Latency ();
                if (l > ARC_latency(arc)) {
                    arc->latency = l;
                }
            }
#endif
            mUINT16 latency = ARC_latency (arc);

            OP_HEUR_INFO* op_info = Get_OP_Heur_Info (succ) ;

            Is_True (op_info,
                     ("OP[%d] of BB:%d has no heuristic data!",
                     OP_map_idx(succ), BB_id(OP_bb(succ))));

            CYCLE succ_issue_cyc = 
                op_info->_etime_set_by_which_bb == OP_bb(op) ? 
                op_info->_e_time : 0 ;
            
            if (issue_cyc + latency > succ_issue_cyc) {
                succ_issue_cyc = issue_cyc + latency ;
            }

            op_info->_e_time = succ_issue_cyc ;
            op_info->_etime_set_by_which_bb = _target_block ;

        } /* nested for */

    } /* outer for */
}

    /* =====================================================
     *
     *  Upward_Global_Sched_Is_Profitable 
     *
     *  return TRUE iff sched <cand> is really profitable.
     *
     * ====================================================
     */
BOOL
FAVOR_DELAY_HEUR :: Upward_Global_Sched_Is_Profitable 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info, 
     RGN_CFLOW_MGR* cflow_info) {
    
    if (cand->Is_Spec ()) {
        return Upward_Spec_Global_Sched_Is_Profitable 
                    (cand, bb_info, cflow_info);
    }

    return Upward_Useful_Sched_Is_Profitable 
              (cand, bb_info, cflow_info);
}


    /* =============================================================
     *
     *  Upward_Code_Motion_Inc_Live_Range_Greatly 
     * 
     *  Estimate increased register pressure caused by code motion. 
     *  return TRUE iff the code motion of <op> from <from> to 
     *  <cutting_set> end up by adding "significant" register 
     *  pressure, FALSE otherwise.
     * 
     * =============================================================
     */

BOOL
FAVOR_DELAY_HEUR :: Upward_Code_Motion_Inc_Live_Range_Greatly 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info,
     RGN_CFLOW_MGR* cflow_info) {

    REGION_VECTOR* rv = cand->Move_Across_Rgns ();
    OP* op = cand->Op ();

    if (rv->size () > 0) {
    
        BOOKEEPING_LST* bkl = cand->Bookeeping_Lst ();
        BOOKEEPING* bk;
        
        for (REGION_VECTOR_ITER iter = rv->begin () ; 
             iter != rv->end (); 
             ++ iter) {
            
            REGION* r = *iter ;
            if (!Is_Large_Region (r)) {
                continue;
            }

            for (bk = bkl->First_Item (); 
                 bk != NULL;
                 bk = bkl->Next_Item (bk)) {

                INT inc_live_out = 0 ;
                BB* cs_elem = bk->Get_Placement ();

                for (INT i = OP_results (op) - 1 ; i >= 0 ; i--) {

                    TN * result = OP_result(op, i) ;
                    if (TN_is_constant (result) ||
                        TN_is_const_reg(result)) {
                        continue ;
                    }

                    if (!TN_is_global_reg(result)) {
                        ++ inc_live_out;
                    } else if (GRA_LIVE_TN_Live_Outof_BB (result,cs_elem)) {
                        -- inc_live_out ; 
                    } else {
                        ++ inc_live_out ;
                    }
                
                } /* for (INT i = OP_results (op) - 1 ... i--) */

                for (INT i = OP_opnds (op) - 1 ; i >= 0 ; i--) {

                    TN * src_opnd = OP_opnd(op,i);
                    if (TN_is_constant(src_opnd) ||
                        TN_is_const_reg(src_opnd)) {
                        continue ;
                    }

                    if (!TN_is_global_reg(src_opnd)) {
                        ++ inc_live_out;
                    } else if (GRA_LIVE_TN_Live_Outof_BB (src_opnd,cs_elem)) {
                        -- inc_live_out ;
                    } else {
                        ++ inc_live_out ;
                    }
                }

                if (inc_live_out > 0) {
                    return TRUE;
                }
            } /* end of for (BB_VECTOR ...) */

        } /* for (RGN_VECTOR_CONST_ITER ...) */
    }

    return FALSE ;
}


    /* ===============================================================
     *
     *  Spec_Global_Sched_Is_Profitable 
     *  
     *  return TRUE iff speculation from <from> to <to> is "profitable"
     *  FALSE otherwise.
     *
     *  <from> is where the candidate initially resides, hence it
     *  is in the lower-reach of control-flow. and <to> is taget BB, 
     *  therefore it is in the upper-reach of control-flow. 
     *   
     * =================================================================
     */
BOOL
FAVOR_DELAY_HEUR::Upward_Spec_Global_Sched_Is_Profitable 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info,
     RGN_CFLOW_MGR* cflow_info) {

    OP* op = cand->Op ();
    BB* home = OP_bb(op); 
    PROBABILITY useful_exec_prob = cand->Useful_Exec_Prob ();

    Is_True (cand->Spec_Type () != SPEC_NONE,
             ("candidate's code motion type is expected to be speculation"));


    INT32 across_chk_num = 0;
    INT32 data_spec_num = 0 ;
    INT32 cntl_spec_num = 0; 

    if (OP_load (op)) {
        

        INT32 spec_count = 0 ;

        UNRESOLVED_DEP* dep ;  
        UNRESOLVED_DEP_LST* dep_lst = cand->Unresolved_Dep_List(); 

        for (dep = dep_lst->First_Item (); 
             dep != NULL; 
             dep = dep_lst->Next_Item(dep)) {

            SPEC_TYPE spec_type = dep->Spec_Type ();

            switch (spec_type) {
            case SPEC_NONE :
                continue;

            case SPEC_CNTL :
#ifdef TARG_IA64
                if (OP_chk(dep->Pred ())) {
                    ++across_chk_num ;
                }
#endif
                ++ cntl_spec_num ;
                break ;

            case SPEC_DATA :
                ++ data_spec_num ;
                break ;

            default:
                Fail_FmtAssertion("Unknown SPEC_TYPE(%d) \n", spec_type);
            } /* end of switch */

        } /* end of for (dep...) */


        if (across_chk_num > LD_VIOLATE_CHK_DEP_MAX     ||
            data_spec_num  > LD_VIOLATE_DATA_DEP_MAX    ||
            cntl_spec_num  > LD_VIOLATE_CNTL_DEP_MAX) {
            return FALSE ;
        }

        if (OP_no_alias (op) || 
            Load_Has_Valid_Vaddr (op) && !across_chk_num && !data_spec_num) {
                /* <op> need not transform for these cases. So 
                 * we requires higher reach-prob to reduce speculation
                 * penalty. 
                 */
            if (useful_exec_prob < 
                SPEC_SAFE_LOAD_WITHOUT_TRANSFORM_REACH_PROB) {
                return FALSE;
            }
        } else if (useful_exec_prob < UNSAFE_CNTL_SPEC_PROB) {
            return FALSE; 
        }

    } else if (SAFE_CNTL_SPEC_PROB > useful_exec_prob) { 
            return FALSE; 
    }

    return !Upward_Code_Motion_Inc_Live_Range_Greatly 
              (cand, bb_info, cflow_info);
}




    /* ================================================================
     *
     *  Upward_Useful_Global_Sched_Is_Profitable 
     *
     *  return TRUE iff upward useful code motion is profitable 
     *  (from <from> to <_target_bb> and duplicated to any BB in 
     *  cutting_set\{<_target_bb>})
     * 
     *  FALSE, otherwise.
     *
     * ================================================================
     */
BOOL
FAVOR_DELAY_HEUR::Upward_Useful_Sched_Is_Profitable 
        (CANDIDATE* cand, SRC_BB_INFO* bb_info, 
         RGN_CFLOW_MGR* cflow_info) {

    return !Upward_Code_Motion_Inc_Live_Range_Greatly 
                (cand, bb_info, cflow_info);
}

    /* =================================================================
     * 
     *  Renaming_Is_Profitable
     *  
     *  Renaming <cand> is profitable?
     *  
     * =================================================================
     */
BOOL 
FAVOR_DELAY_HEUR::Renaming_Is_Profitable (CANDIDATE *cand)
{
   OP* op = cand->Op();
   BB* home_bb = OP_bb(op);
        /* We don't do renaming for these OPs:
           1) Multi-assignment OPs
           2) Defining a dedicated TN
           3) Already renamed OPs
           4) Conditional defined OPs.  eg
                S1:  (p) x = 
                S2:  (q)  = x
              It's dangerous to rename x because S2 may not definitely use def of S1
           5) Speculative loads, to avoid the complexity of renaming chks.
         */
   if (OP_results(op) != 1 || TN_is_dedicated(OP_result(op, 0)) ||
      OP_renamed(op) ||  OP_cond_def(op) || OP_speculative(op) && OP_load(op))
   return FALSE;
   
        // TODO: Add more acurate renaming heuristic here.
   for (ARC_LIST* arcs = OP_succs(op); arcs; ) {
        ARC *arc = ARC_LIST_first(arcs);
        arcs = ARC_LIST_rest(arcs);
        OP* succ = ARC_succ (arc);   
        if (ARC_kind(arc) == CG_DEP_REGIN &&
            !(OP_speculative(succ) && OP_load(succ)) && // Don't feed into a speculative load
            OP_bb(op) == OP_bb(succ)) // Hope these successors' dependences are resolved earlier, 
        return TRUE;                  // May be too agreessive 
   }
   
   return FALSE;
}


    /* ================================================================
     *
     *  It_is_Better_No_New_Cycle_For_Cur_BB 
     *
     *  check to see whether it is the right time for scheduler to 
     *  close the code motion of <_target_block>. 
     *
     * ================================================================
     */
BOOL
FAVOR_DELAY_HEUR :: It_is_Better_No_New_Cycle_For_Cur_BB (void) {

     return (_xfer_op && OP_Scheduled(_xfer_op)) || 
            OP_Scheduled(BB_last_op(_target_block));

}

CANDIDATE *
FAVOR_DELAY_HEUR :: Select_Best_Candidate_For_Stress_Spec_Purpose 
    (CAND_LIST& m_ready_cand_lst, 
     CAND_LIST& p_ready_cand_lst, 
     BB *       targ,
     E_Time_Constraint * etime_constraint) {
    
    /* has yet implemented */
    return NULL ;
}
                
CANDIDATE*
FAVOR_DELAY_HEUR :: Select_Best_Candidate_For_Stress_Spec_Purpose 
    (CAND_LIST& cand_lst, BB *  targ,
     E_Time_Constraint * etime_constraint) {

    /* has yet implemented */
    return NULL ;
}
