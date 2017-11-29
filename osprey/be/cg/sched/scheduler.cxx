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

/* ======================================================================
 * ======================================================================
 *
 *
 * Module: scheduler.cxx
 * $Date: 2006/06/07 08:36:29 $
 * $Author: weitang $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/scheduler.cxx,v $
 *
 * Description:
 * ===========
 *
 * Implementation of Ipfec instruction scheduler.
 * See scheduler.h for the description.
 *
 *
 * ======================================================================
 * ======================================================================
 */

#include "hb_hazards.h"
#include "timing.h"
#include "cg_dep_graph.h"
#include "tracing.h"
#include "errors.h"
#include "be_util.h"
#include "cg.h"
#include "cgtarget.h"
#include "targ_isa_enums.h"
#include "ipfec_options.h"
#include "region_bb_util.h"

#include "scheduler.h"
#include "cggrp_microsched.h"
#ifdef TARG_IA64
#include "speculation.h"
#include "prdb_util.h"
#include "pqs_cg.h"
#endif
#include "sched_spec_itf.h"

#include "sched_util.h"
#include "sched_cflow.h"
#include "sched_dflow.h"
#include "sched_cand.h"
#include "sched_rgn_info.h"
#include "sched_seq.h"
#include "sched_spec_itf.h"

/* memory management */
#include "cxx_memory.h"

/* dep DAG & liveness */
#include "cg_dep_graph.h"
#include "dag.h"
#include "reg_live.h"
#include "gra_live.h"

/* calc dominator and postdominator */
#include "dominate.h"

/* DaVinci */
#include "vt_region.h"

#include <stdarg.h>       /* for va_start va_list and vnsprintf */
#include <tlog.h>
#include "whirl2ops.h"    /* for Copy_WN_For_Memory_OP */

#if defined(TARG_SL)
#include "tag.h"
#include "hb_sched.h"
#endif

static char* _Global_Insn_Sched_Phase_Name = "ORC:Global code motion"  ;
static char* _Local_Insn_Sched_Phase_Name = "ORC:Local code motion" ;
static char* _Cur_Phase_Name = NULL ;
static int _Local_Insn_Op_id = 0;

#ifndef TARG_SL
void
Clean_Up (BB* bb) {
    OP *op, *next_op;
  
    Reset_BB_scheduled (bb);
    for (op = BB_first_op(bb); op; op = next_op) {

        next_op = OP_next(op);

        if (OP_noop(op)) {
            BB_Remove_Op(bb, op);
        } else {
            Reset_OP_Scheduled (op);
#ifdef TARG_IA64
            Reset_OP_start_bundle(op);
            Reset_OP_end_group(op);
            Reset_OP_bundled(op);
            Reset_OP_m_unit(op);
#endif
        }
    }
}
#endif 
    /* ================================================================
     *
     *  Cycle_Advance 
     *
     * 1. inform micro-scheduler that current cycle cannot bundle any 
     *    candidates available, to that micro-scheduler reset its 
     *    internal status and prepare for the next cycle.
     *
     * 2. update candidate list, set all candidates, which are tried to 
     *    be issued in current cycle but fail, to be "untired"
     * 
     * 3. Let some new candidates under the control of candidate list.
     *    (these new candidates depend upon OPs that are issued in 
     *     current cycle, and the latency between them is non-ZERO)
     * 
     * 4. adjust schedule status accordingly.
     *
     * ===============================================================
     */
void
SCHEDULER::Cycle_Advance (void) {

    _cand_mgr.M_Ready_Cand_List ()->Clear_All_Cands_Tried_Mark ();
    _cand_mgr.P_Ready_Cand_List ()->Clear_All_Cands_Tried_Mark ();

        /* inform micro-scheduler to change its internal status
         */
    CGGRP_Cycle_Advance();

        /* let some candidates under the contol of candidate-list
         */
    Update_Cand_Lst_After_Cycle_Advancing ();
    _heur_mgr.Adjust_Heur_After_Sched_One_Cyc 
                (_ops_in_cur_cyc,_cur_cyc);


    if (SCHED_TF_SUMMARY_DUMP) {
        fprintf(TFile, "\n    Cycle: %d\n", _cur_cyc);
    }

        /* reset schedule status 
         */
    _ops_in_cur_cyc.clear ();

        /* next cycle start at least _cur_cyc + 1 
         */
    ++_cur_cyc ; 
}


          /* ====================================================
           * ====================================================
           *
           *            CANDIDATES STUFF 
           *
           * ====================================================
           * ====================================================
           */
BOOL
SCHEDULER::OP_Cannot_be_Candidate_Since_Obvious_Reason (OP* op) {

    if (OP_Scheduled(op)) return TRUE;

    if (_prepass&& 
        (OP_glue(op) || OP_no_move_before_gra(op) || 
         OP_chk(op) ||
         OP_access_reg_bank (op)) && 
         OP_bb(op) != _target_bb) {

        return TRUE;
    }

    if (OP_bb (op) != _target_bb) {
        if (OP_Cannot_Be_Moved_Outof_HomeBB (op) ||
            !BB1_Dominate_BB2 (_target_bb, OP_bb(op)) &&
            !CGTARG_Can_Be_Speculative (op)) {
            return TRUE;
        }
    }

    return _cand_mgr.OP_Is_In_Cand_List (op);
}

    /* ========================================================
     *
     * Succ_Pred_Transposed_If_Sched 
     *
     * ref the header file for details.
     *
     * =======================================================
     */
BOOL
SCHEDULER::Succ_Pred_Transposed_If_Sched 
    (ARC* arc, BB_VECTOR* cutting_set) {

    OP* pred = ARC_pred(arc);
    BB* pred_home = OP_bb(pred);

    if (OP_Scheduled(pred)) { 
        if (pred_home != _target_bb || !OP_xfer(pred)) {
            return FALSE;
        } else {
            return TRUE;
        }
    }

    BB_POS pos = BB_Pos_Analysis (pred_home, cutting_set, &_cflow_mgr);

    BOOL transposed = FALSE;
    switch (pos) {
    case ABOVE_SISS:
        break;

    case IN_SISS:
        transposed = (pred_home == _target_bb || OP_xfer(pred));
        break;

    case BELOW_SISS:
        transposed = TRUE ; 
        break ;

    default :
        Is_True (FALSE, ("fail to analysis src position for BB:%d!",
                BB_id(OP_bb(pred))));
    }

    return transposed;
}


    /* ===============================================================
     *
     * Collect_And_Analyse_Unresolved_Dep
     *
     * acquire the barrier that MAY render the code motion impossible 
     * there are two kind of barriers:
     *
     *   - the dependency scheduler should violate.
     *   - the nested REGION we need moving across.
     *
     *
     *  e.g. assume CFG like this
     *      
     *               +--------------------+
     *               |       BB1          |
     *               +--------------------+
     *               /                V
     *  +-----------------+    +-------------------+
     *  | BB2: call...    |    | BB:4 empty block  |
     *  +-----------------+    +-------------------+
     *          V                     /
     *  +-----------------+          /  
     *  | BB3: gp=...     |         /
     *  +-----------------+        /
     *                 \          /
     *                  \        /
     *          +--------------------+
     *          |   x=gp+5   BB:5    |
     *          |   ld y=[x]         |
     *          +--------------------+
     * TODO: fini this comment 
     *
     * Assumption: 
     *
     *     parameter <cand> is newly created(put in other word, 
     *     <cand> is initialized) except cand->_op is set properly.
     *
     * ===============================================================
     */
BOOL
SCHEDULER::Collect_And_Analyse_Unresolved_Dep
    (CANDIDATE* cand, SRC_BB_INFO* bb_info) {

        /* prepare for the P-ready candidate identification
         */
    BOOL donate_p_ready = bb_info->Can_Donate_P_Ready_Cand ();
    OP* cand_op = cand->Op ();
    BB* home_bb = OP_bb(cand->Op ());

    cand->Free_Bookeeping_Lst ();
    cand->Move_Across_Rgns()->clear ();
    BB_VECTOR* cs_between_targ_src = NULL;
    EXEC_PATH_SET move_around_paths(&_mem_pool);

    if (donate_p_ready) {
        cand->Move_Against_Path_Set ()->Clear ();
        cs_between_targ_src = bb_info->Get_P_Ready_Bookeeping_Blks ();
        move_around_paths.Resize 
            (_cflow_mgr.Get_Exec_Path_Mgr()->Path_In_Total ());
    }

        /* step 1.a : collect some unresolved dependency, analysis 
         *            is applied upon these depencies to make desition
         *            whether we simply violate these unresolved deps 
         *            (speculate load across store) or escape this 
         *            barrier by P-ready book-keeping/compensation.
         */
    for (ARC_LIST* arcs = OP_preds(cand_op);
         arcs != NULL; 
         arcs = ARC_LIST_rest(arcs)) {

        ARC* arc  = ARC_LIST_first(arcs) ;
        OP*  pred = ARC_pred(arc) ;

        if (!_prepass && !_global) {

                /* We can determine whether a op is qualified to be a 
                 * candidate or not at very early stage. this if-
                 * statement obiviate the need of going through the rest
                 * for-loop body to test whether <cand> is qualified.
                 */ 
            if (OP_Scheduled(pred)) {
                continue; 
            } else { 
                return FALSE; 
            }
        }

        if (!Succ_Pred_Transposed_If_Sched 
             (arc, bb_info->Get_Cutting_Set ())) {

                /* sequence of pred and succ remains unchanged
                 */
            continue;
        }
        
        
        SPEC_TYPE spec_tmp = Derive_Spec_Type_If_Violate_Dep (arc);
        if (spec_tmp == SPEC_NONE) {
                /* case 1: dependency we simply ignore
                 */
            continue;
        }

            /* make the desition for the unresolved dependency, 
             * violate it (move across) of escape (move around) it. 
             */

            /* case 2: dependency that we can violate
             */
        if (spec_tmp == SPEC_DATA || spec_tmp == SPEC_CNTL || 
            spec_tmp == SPEC_COMB) {

            cand->Add_Spec (spec_tmp);

                /* keep track of each violated dependece 
                 */
            UNRESOLVED_DEP* t = _cand_mgr.New_Unresolved_Dep ();
            t->Set_Arc (arc); 
            t->Set_Spec_Type (spec_tmp);
            cand->Unresolved_Dep_List ()->Prepend (t);

            continue;
        }

            /* case 3: dependency that we should observe
             */
        BB* pred_bb = OP_bb(pred);
        if (spec_tmp == SPEC_DISABLED) {
            if (!donate_p_ready || pred_bb == _target_bb) {
                    /* There is no way to move around this 
                     * barrier.
                     */
                return FALSE;
            }
            
            BB_POS pos = BB_Pos_Analysis (pred_bb,
                                          cs_between_targ_src,
                                          &_cflow_mgr);

            if (pos == BELOW_SISS) { return FALSE ; }
            
            if (pos == IN_SISS || pos == ABOVE_SISS) {

                cand->Set_P_ready ();

                for (BB_VECTOR_ITER iter = cs_between_targ_src->begin();
                     iter != cs_between_targ_src->end (); iter++) {

                    BB* bk_blk = *iter;

                    if (!(cand->Bookeeping_Lst ()->Retrieve (bk_blk)) &&
                         (bk_blk == pred_bb || 
                          _cflow_mgr.BB1_Reachable_From_BB2 (bk_blk,pred_bb))) {
                        
                        EXEC_PATH_SET* eps_tmp = 
                            _cflow_mgr.Get_Path_Flow_Thru (bk_blk);

                        BOOKEEPING* bk = _cand_mgr.New_Empty_Bookeeping ();
                        bk -> Set_Placement (bk_blk);
                        bk -> Set_P_Ready_Bookeeping ();
                        cand->Bookeeping_Lst () -> Prepend (bk);

                        move_around_paths += *eps_tmp;
                    }
                }
                
            } else {
                FmtAssert (FALSE, ("Unknown BB_POS(%d)", pos));
                return FALSE;
            }

        } /* end of 'if (spec_tmp == SPEC_DISABLED)' */

    } /* end of for (ARC_LIST* arcs = ... ) */ 

    
    if (!_global) { return TRUE; }

    if (cand->Is_P_Ready ()) {

            /* step 2: calc the move-against paths
             */
        EXEC_PATH_SET* eps = cand->Move_Against_Path_Set();
        *eps = *_cflow_mgr.Get_Path_Flow_Thru (OP_bb(cand->Op()));
        *eps -= move_around_paths;
        if (eps->Is_Empty ()) {
                /* there is no barrier-free exec-path we can moved 
                 * against from cand's home bb toward target-block.
                 */
            return FALSE; 
        }

        eps = cand->P_Ready_Bookeeping_Path_Set ();
        *eps = move_around_paths;

            /* step 1.b: During the process of step1.a, we analyse 
             *      each unresolved dependency, determine whether we 
             *      choose violate- or escape-strategy. However, 
             *      we my previously encount an violable-unresolved-dep, 
             *      and we choose violate-strategy, and later on, we 
             *      encount an shold-be-strictly-observe dependency,
             *      to make op to be an qualified candidate, we 
             *      can sched op by P-ready-book-keeping which may shadow
             *      violable-dep we previously come across. 
             */
        UNRESOLVED_DEP* dep, *next_dep;
        UNRESOLVED_DEP_LST* deplst = cand->Unresolved_Dep_List ();

        for (dep = deplst->First_Item (); dep; dep = next_dep) {

            next_dep = deplst->Next_Item(dep);
            if (cand->Shadowed_By_P_Ready_Bookeeping 
                (OP_bb(dep->Pred()), &_cflow_mgr)) {
                deplst->Delete_Item (dep);
            }

        }
    }

        /* step 3 : determine non-p-ready book-keeping places.
         */
    Determine_Non_P_Ready_Bookeeping_Places (cand, bb_info);

    cand->Calc_Useful_Exec_Prob (_target_bb,&_cflow_mgr);

        /* step 4 : Futher determine whether code motion is control 
         *          speculation or not. (We could not figure out 
         *          candidate's code motion type just from unresolved
         *          dependency.)
         */
    if (cand->Is_P_Ready ()) {
            /* Partial-ready candidate is cntl-speculated anyway
             */
        cand->Add_Spec (SPEC_CNTL); 

            /* The following statements does not fit this routine 
             * very much. but it can improve the compilation time
             * since we determine at very early stage that to 
             * schedule some P-ready canidate definitely worthless.
             */
        if (cand->Useful_Exec_Prob () < 
            DONATE_P_READY_CAND_BB_REACH_PROB) {
            return FALSE;
        }
    } else if (!(cand->Spec_Type () & SPEC_CNTL)) {

        BOOKEEPING_LST* bkl = cand->Bookeeping_Lst ();
             
        for (BOOKEEPING* bk = bkl->First_Item ();
             bk != NULL;
             bk = bkl->Next_Item (bk)) {

            if (!BB1_Postdominate_BB2 (home_bb, bk->Get_Placement ())) {
                cand->Add_Spec (SPEC_CNTL);
                break;
            }
        }
    }

    return TRUE;
}

    /* =======================================================
     *
     * Determine_Non_P_Ready_Bookeeping_Places 
     * 
     * Determine those blocks we need appending duplicated 
     * instruction book to.
     *
     * Assumption:
     *
     *  P-ready bookeeping places have already been specified 
     *  by <cand>->Bookeeping_Lst(). So this routine should
     *  be called after <Collect_And_Analyse_Unresolved_Dep>.
     *
     * ========================================================
     */
void
SCHEDULER::Determine_Non_P_Ready_Bookeeping_Places 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info) {

    BB_VECTOR* cs = bb_info->Get_Cutting_Set ();
    BB* home_bb = OP_bb(cand->Op ());

    if (!cand->Is_P_Ready ()) {
        
        for (BB_VECTOR_ITER iter = cs->begin (); 
             iter != cs->end (); iter ++) {

            BB* bk_blk = *iter;
            if (bk_blk == _target_bb) { continue; }

            BOOKEEPING* bk = _cand_mgr.New_Empty_Bookeeping ();
            bk->Set_Placement (bk_blk);
            bk->Set_Dup_Bookeeping ();

            cand->Bookeeping_Lst () -> Append (bk);
        }

        return;
    }

    for (BB_VECTOR_ITER iter = cs->begin (); 
         iter != cs->end (); iter++) {

        BB* b = *iter;
        if (b == _target_bb) { continue; }

        if (!cand->Shadowed_By_P_Ready_Bookeeping (b,&_cflow_mgr)) {

            BOOKEEPING* bk = _cand_mgr.New_Empty_Bookeeping ();
            bk->Set_Placement (b);
            bk->Set_Dup_Bookeeping ();

            cand->Bookeeping_Lst () -> Append (bk);
        }
    }
}

    /* ==========================================================
     *
     *  Collect_And_Analyse_Other_Than_Dep_Constraints 
     *
     *  ref the header file for details.
     *
     * ==========================================================
     */
BOOL
SCHEDULER::Collect_And_Analyse_Other_Than_Dep_Constraints 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info) {

    if (!_global && !_prepass) {

       /* since scheduling scope is confined within a single basic block,
        * and any speculation is turned off, it is no need to checck to 
        * following conditions.
        */

        return TRUE;
    }

    OP* op = cand->Op ();

        /* 1. We do not speculate following two kinds of instructions.
         *
         * - Some instrutions(e.g store), by its nature, can not be 
         *   speculated, 
         * - For debugging purpose, we do not speculated specific OPs.
         */ 
    SPEC_TYPE spec_type = cand->Spec_Type ();
    if ((spec_type & SPEC_DATA) && OP_ANNOT_Cannot_Data_Spec (op) ||
        (spec_type & SPEC_CNTL) && OP_ANNOT_Cannot_Cntl_Spec (op)) {
        return FALSE;
    }

    if (SCHED_SPEC_HANDSHAKE::OP_Can_not_be_Candidate (op, spec_type)) {
            /* speculation package does not feel happy 
             */
        return FALSE;
    }
    
    if (OP_bb(op) == _target_bb) {
           return TRUE; 
    }
   
        /* 2. check to see whether cand will kill some liveout defs. Renaming chances!
         */
    if (_dflow_mgr.Upward_Sched_Kill_LiveOut_Defs 
            (cand, bb_info, &_cflow_mgr)) {
            
        /* Now, we just do renaming for single assignment cand.
         */
        if (OP_results(op) != 1) {
            return FALSE;
        }
    }


        /* 3. find out all move-across-nested-regions.
         */
    REGION_VECTOR* rv = cand->Move_Across_Rgns();
    rv->clear ();


    REGION_VECTOR* rv_tmp = bb_info->Move_Across_Or_Around_Nested_Rgns ();
    for (REGION_VECTOR_ITER iter = rv_tmp->begin (); 
         iter != rv_tmp->end ();
         iter ++) {

        REGION *r = *iter;
        if (cand->Is_M_Ready () || 
            !cand->Shadowed_By_P_Ready_Bookeeping (r, &_cflow_mgr)) {
            rv->push_back (r);        
        }
    }

        /* 4. check to see whether live-ranges interference with each other.
         */
    if (_dflow_mgr.Upward_Sched_Interfere_Nested_Rgns_LiveRanges 
           (cand, bb_info)) {
        return FALSE;
    }

    return TRUE;
}

    /* =========================================================
     *
     * Try_Add_OP_to_Candidate_List 
     *
     * ref the header file for details.
     *
     * ========================================================
     */
BOOL
SCHEDULER::Try_Add_OP_to_Candidate_List (OP* op) {

    if (OP_Cannot_be_Candidate_Since_Obvious_Reason (op)) {
        return FALSE;
    }

    SRC_BB_INFO* bb_info = _src_bb_mgr.Get_Src_Info (OP_bb(op));

    CANDIDATE* cand = _cand_mgr.Create_Empty_Cand ();
    cand->Set_OP (op);

    
    if (!Collect_And_Analyse_Unresolved_Dep (cand, bb_info) ||
        !Collect_And_Analyse_Other_Than_Dep_Constraints 
            (cand, bb_info)) {

        _cand_mgr.Erase_Cand (cand); 
        return FALSE;

    }

        /* Now, <op> are qualified as an candidate, but it may not 
         * be a "good" one. So we need futher check whether it 
         * satify our cost-model.
         */
    if (OP_bb(op) != _target_bb &&
        !_heur_mgr.Upward_Global_Sched_Is_Profitable 
            (cand, bb_info, &_cflow_mgr)) {

        _cand_mgr.Erase_Cand (cand); 
        return FALSE; 
    }

#if  defined(TARG_SL)
   /* For SL we don't have speculation, erase candidate of type 
    * data speculation and comb speculation from candidate list */ 
    if( cand && ((cand->Spec_Type()!=SPEC_NONE) || cand->Is_P_Ready())) {
        _cand_mgr.Erase_Cand (cand); 
         return FALSE;
    }     
#endif 

        /* add this candidate to cand-list.
         */
    CAND_LIST* cand_lst = cand->Is_M_Ready () ? 
                          _cand_mgr.M_Ready_Cand_List () : 
                          _cand_mgr.P_Ready_Cand_List () ; 

    cand_lst->Add_Candidate (cand);

    return TRUE;

}
    /* ==========================================================
     *
     *  Find_All_Candidates 
     *
     *  ref the header file for details
     *
     * ==========================================================
     */
void
SCHEDULER::Find_All_Candidates (void) {

    _cand_mgr.M_Ready_Cand_List ()-> Erase_All_Cand ();
    _cand_mgr.P_Ready_Cand_List ()-> Erase_All_Cand ();

    if (_global) {
        Determine_P_Ready_is_Profitable_or_not () ;
    } else {
        Disable_P_Pready_Cand () ;
    }

    const BB_VECTOR* src_bbs = _src_bb_mgr.Src_BBs () ;

    for (BB_VECTOR_CONST_ITER iter = src_bbs->begin(); 
        iter != src_bbs->end(); iter++) {

        OP* op;
        BB* b = *iter;
        FOR_ALL_BB_OPs(b, op) {
            Try_Add_OP_to_Candidate_List (op);
        }
    }

        /* Candidate list should not be empty here.
         */
    Is_True (!_cand_mgr.M_Ready_Cand_List ()-> Cand_Lst_Is_Empty () ||
             !BB_length(_target_bb),
             ("Fail to find any candidate!"));

}



/* =================================================================
 *
 *  Update_Cand_Lst_During_Sched_Cyc 
 *
 *
 *  After commiting <cand>, the dependences are resoved, hence 
 *  some non-candidates are now become P-ready or M-ready candidates.
 *  
 *  We add _SOME_ of these new candidate into candidate list. the 
 *  newly added candidates are dep upon <cand> with _ZERO_ latency 
 *  before <cand> is committed. 
 * 
 *  the rest new candidates are added to candidates after cycle
 *  advancing by calling <Update_Cand_Lst_After_Cycle_Advance>.
 *  
 * ================================================================
 */
void
SCHEDULER::Update_Cand_Lst_During_Sched_Cyc (CANDIDATE& cand) {

    OP* op = cand.Op ();
    OP* succ;

    for (ARC_LIST* arcs = OP_succs(op); 
        arcs != NULL; arcs = ARC_LIST_rest(arcs)) {

        ARC *arc = ARC_LIST_first(arcs);
        OP *succ = ARC_succ(arc);

        if (!_src_bb_mgr.Is_Src_BB (OP_bb(succ))) {

            /* the successor does not reside in any source BBs, 
             * hence, they cannot be an candidate
             */

            continue;
        }

        INT32 latency = ARC_latency(arc) ;

        if (latency > 0) {
            /* obviously, the successor should not be bundled into 
             * current instruction group, otherwise, we would got
             * a splitted issue.
             * 
             * these candidates are added into candidate list after
             * cycle is advanced.
             */
            continue ;
        } else if (latency < 0) {

            /* some component once have produced negative dep letency
             */
            FmtAssert (FALSE,
                ("Latency of dep between OP[%d](BB:%d) and OP[%d](BB:%d)"
                 " should not be a negative %d\n", 
                 OP_map_idx(op),   BB_id(OP_bb(op)), 
                 OP_map_idx(succ), BB_id(OP_bb(succ)),
                 latency)) ;

        }

            /* for the case of ZERO latency, _IF_ the non-candidate-
             * successor become an candidate, add them into 
             * now becomes a candidates, we add them into candidate
             * list.
             */
        CAND_LIST* cand_lst = _cand_mgr.P_Ready_Cand_List ();
        if (cand_lst->Cand_In_Total ()) {
            cand_lst-> Erase_Cand (succ,FALSE);
        }

        if (Try_Add_OP_to_Candidate_List (succ) &&
            SCHED_TF_SUMMARY_DUMP) {

            fprintf (TFile, "%d ", OP_map_idx(succ));
        }
    }
  
    _heur_mgr.Adjust_Heur_After_Cand_Sched (op, _cur_cyc);
    if (SCHED_TF_SUMMARY_DUMP) { fprintf(TFile, "\n"); }
  
        /* <cand> has currently scheduled, remove it from candidate list
         */
    if (!cand.Is_P_Ready ()) {
        _cand_mgr.M_Ready_Cand_List () -> Erase_Cand (&cand) ;
    } else {
        _cand_mgr.P_Ready_Cand_List () -> Erase_Cand (&cand) ;
    }
}
      
    /* ================================================================
     *
     *  Update_Cand_Lst_After_Cycle_Advancing 
     * 
     *  Add all M-ready and P-ready(that are not in any canidate list)
     *  into M-ready candidate list or P-ready candidate list seperately.
     * 
     * ================================================================
     */
void
SCHEDULER::Update_Cand_Lst_After_Cycle_Advancing (void) {

    for (OP_Vector_Iter iter = _ops_in_cur_cyc.begin () ;
         iter != _ops_in_cur_cyc.end () ; iter ++) {

        OP * op = *iter ;

        for (ARC_LIST* arcs = OP_succs(op);
            arcs != NULL; arcs = ARC_LIST_rest(arcs)) {

            ARC *arc = ARC_LIST_first(arcs);
            OP *succ = ARC_succ(arc);

            INT32 latency = ARC_latency(arc) ;

            if (!latency) continue ;
            if (!_src_bb_mgr.Is_Src_BB (OP_bb(succ))) continue;
  

            CAND_LIST* cand_lst = _cand_mgr.P_Ready_Cand_List ();
            if (cand_lst->Cand_In_Total ()) {
                cand_lst-> Erase_Cand (succ,FALSE);
            }

            if (Try_Add_OP_to_Candidate_List(succ) &&
                SCHED_TF_SUMMARY_DUMP) {
                fprintf(TFile, "%d ", OP_map_idx(succ));
            }
        }
  
        if (SCHED_TF_SUMMARY_DUMP) { 
            fprintf(TFile, "\n"); 
        }
    }
}


inline BOOL
SCHEDULER::OP_Cannot_Be_Moved_Outof_HomeBB (OP* op) {
    return  OP_xfer (op) ||
            OP_call (op) ||
            OP_chk  (op) ||
            OP_ANNOT_OP_Def_Actual_Para (op)
            ;
}

SPEC_TYPE
SCHEDULER::Get_OP_Prohibited_Spec_Type (OP *op) {

    /* 1. techanical weakness make us cannot speculate some kinds 
     *     of OPs 
     */
    if (OP_Is_Float_Mem(op) || OP_ANNOT_OP_Def_Actual_Para (op)) {
        return SPEC_COMB ;
    }


    /* 2. algorithm required or architecture constraint 
     */
    if (!_prepass) { return SPEC_COMB ; }

    TOP opcode = OP_code(op);

    if (OP_xfer(op)         || OP_like_store(op)    ||
        TOP_is_ftrap(opcode)|| TOP_is_itrap(opcode) ||
        TOP_is_fdiv(opcode) || OP_idiv (op)         ||
        OP_sqrt (op)) {

        /* there are actually no data-spec 
         */
        return SPEC_CNTL ;
    }

    if (OP_volatile(op)) { return SPEC_COMB ; }


    /* We could not speculate post-inc load. 
     * TODO:  exclude those load which is safe to be speculated 
     *        without any transformation from this category.
     */
     if (OP_load (op)) {

        for (INT i = OP_results(op) - 1 ; i >= 0 ; --i) {

            TN * result_tn = OP_result (op,i) ;
            for (INT j = OP_opnds(op) - 1 ; j >= 0 ; --j) {

                if (result_tn == OP_opnd(op,j)) {
                    return SPEC_COMB ;
                }
            }
        }
    }

    SPEC_TYPE spec_type = IPFEC_Enable_Speculation ? SPEC_NONE : SPEC_COMB ;

    if (!IPFEC_Enable_Data_Speculation) {
        spec_type = SPEC_TYPE(spec_type | SPEC_DATA);
    }

    if (!IPFEC_Enable_Cntl_Speculation) {
        spec_type = SPEC_TYPE(spec_type | SPEC_CNTL);
    }


    if (spec_type != SPEC_COMB && (
        IPFEC_Query_Skiplist (spec_skip_bb, (INT32)BB_id(OP_bb(op)),
                (INT32)Current_PU_Count()) ||
        IPFEC_Query_Skiplist (spec_skip_op, (INT32)OP_map_idx(op), 
                (INT32)BB_id(OP_bb(op))))) { 
        fprintf (stderr, "BB:%d OP:%d is skipped\n", BB_id(OP_bb(op)), 
                OP_map_idx(op));
        spec_type = SPEC_COMB ;
    }

    return spec_type ;
}

    /* ====================================================================
     * 
     *  Append_Dup_Op_2_BB 
     *
     *  append <op> to <bb> if <bb> has no control-transfer OP, otherwise, 
     *  insert <op> right before cntl-xfer-op
     * 
     *  maintain some corresponding data structure properly.
     * 
     *  this routine is called when we generate compensation code
     * 
     * ====================================================================
     */
void
SCHEDULER::Identify_Cannot_Spec_OPs (BB *bb) {

    OP * op;
    FOR_ALL_BB_OPs(bb, op) {

        SPEC_TYPE spec_type = Get_OP_Prohibited_Spec_Type (op);
    
        if (spec_type == SPEC_NONE) continue ;

        if (spec_type & SPEC_CNTL) {
            OP_ANNOT_Set_Cannot_Cntl_Spec (op) ;
        }

        if (spec_type & SPEC_DATA) {
            OP_ANNOT_Set_Cannot_Data_Spec (op) ;
        }
    }
}

/* ====================================================================
 *
 *  Identify_Cannot_Spec_OPs
 * 
 *  identify all ops (of region <rgn_ptr>) that cannot be speculated
 * 
 * ====================================================================
 */

void
SCHEDULER::Identify_Cannot_Spec_OPs (REGION *rgn) {

    for (TOPOLOGICAL_REGIONAL_CFG_ITER 
        iter (rgn->Regional_Cfg()) ; iter != 0 ; ++ iter) {

        if ((*iter)->Is_Region()) continue ; 
        Identify_Cannot_Spec_OPs ((*iter)->BB_Node()); 
    }
}


void
SCHEDULER::Init_Sched_Status (void) {
    
    _cur_cyc = (CYCLE)0 ;

    _ops_in_cur_cyc.clear ();
    _frontier_op = BB_length(_target_bb) ? 
                   BB_first_op (_target_bb) : NULL;

    _upward_motion_num = _downward_motion_num = 0;
    _sched_times = 0;
}

void
SCHEDULER::Adjust_Status_For_Resched (void) {

    _cur_cyc = (CYCLE)0;
    _ops_in_cur_cyc.clear ();

    _upward_motion_num = _downward_motion_num = 0;
    _sched_times ++;

    Clean_Up (_target_bb);
    _frontier_op = BB_first_op(_target_bb);


        /* adjust heuristic stuff 
         */
    _heur_mgr.Adjust_Heur_Stuff_When_BB_Changed (_target_bb,_src_bb_mgr);

        /* Initialize the candidate list 
         */
    OP_Vector opv(&_mem_pool);
    for (CAND_LIST_ITER iter(_cand_mgr.M_Ready_Cand_List ()); 
        !iter.done();) {

        opv.push_back (iter.cur()->Op ());
        iter.erase_cur_and_advance ();

    }

  if(_global) {
    for (CAND_LIST_ITER iter(_cand_mgr.P_Ready_Cand_List ()); 
         !iter.done();) {

        opv.push_back (iter.cur()->Op ());
        iter.erase_cur_and_advance ();

    }
  }
    OP* op;
    FOR_ALL_BB_OPs (_target_bb,op) {
        Try_Add_OP_to_Candidate_List (op);
    }
  if(_global)
  {  
    for (OP_Vector_Iter iter = opv.begin(); iter != opv.end(); iter++) {
      Try_Add_OP_to_Candidate_List (*iter);
    }
  }

#ifdef TARG_IA64
        /* clear the multiway-branch-spans-bb vector */
    for (BB_VECTOR_ITER iter = _multiway_br_span_bbs.begin ();
         iter != _multiway_br_span_bbs.end ();
         iter ++) {

        BB* b = *iter;
        _multiway_br_candidates = BB_SET_Difference1D
             (_multiway_br_candidates, b);
    }
    _multiway_br_span_bbs.clear ();
#endif
}

    /* ==========================================================
     * ==========================================================
     *
     *       Commiting scheduling 
     *
     * ==========================================================
     * =========================================================
     */

#ifdef TARG_IA64
    /* ==========================================================
     *
     *  Insert_Check 
     *
     *  Insert chk-op at <pos> of <home_bb> for speculative 
     *  load <ld> which resides <home_bb> before it being 
     *  moved. At the same time, maintain dependence 
     *  etc if necessary.
     *  
     *  Not all candidates are leagal to be move across a given
     *  check, however, these candidates may have already been
     *  added into candidate list. We evict these candidates from
     *  cand-list.
     *
     * ==========================================================
     */
OP*
SCHEDULER::Insert_Check (OP * ld, BB * home_bb, OP* pos) {

    Is_True (OP_load(ld), ("OP is not load!")) ;
    Is_True (!OP_Is_Float_Mem(ld), 
             ("floating-point load shouldn't be spec now"));

    vector<OP*> dup_ops;
    OP *chk_op = ::Insert_CHK (ld, dup_ops, home_bb, pos, 
                               OP_opnd(ld, OP_PREDICATE_OPND));
      
    /* Some candidates annot be move across a given check-op 
     * (those OPs are termed "baneful"-OP by speculation 
     * package author), drive them out of candidate list.
     */
    for (ARC_LIST * arcs = OP_succs(chk_op) ;
         arcs != NULL ; arcs = ARC_LIST_rest (arcs)) {

        ARC* arc = ARC_LIST_first (arcs) ;
        OP* succ = ARC_succ (arc);

        CAND_LIST       * cand_list = NULL ;
        CANDIDATE       * cand = NULL;

        cand = _cand_mgr.Get_Candidate (succ);
        if (!cand) {
            continue; /* succ is not candidate */
        } else {
            cand_list = cand->Is_M_Ready () ? 
                        _cand_mgr.M_Ready_Cand_List ():
                        _cand_mgr.P_Ready_Cand_List ();

        } 

            /* examining whether <succ> is "baneful" from 
             * <chk_op>'s point of view.
             */
        BB* b = OP_bb(ARC_pred(arc));
        if (!cand->Shadowed_By_P_Ready_Bookeeping (b,&_cflow_mgr)) {

            if ((!ARC_is_dotted (arc) || OP_baneful(succ))) {

                /* the 1st logical ORed condition is redundant since 
                 * <OP_baneful> itself consider ARC_succ(<arc>) is 
                 * "baneful" to <chk_op>, but <OP_baneful> is very
                 * expensive. We put the "ARC_is_dotted" condition
                 * ahead to identify some "baneful" OP quickly.
                 */ 
                cand_list->Erase_Cand (cand);

            } else {

                if (OP_ANNOT_Cannot_Cntl_Spec (cand->Op())) {
                    cand_list->Erase_Cand (cand);
                } else {
                    cand->Add_Spec (SPEC_CNTL); 
                }
            }
        }

    }

        /* maintain annotation 
         */
    SCHED_BB_ANNOT* bb_annot = sched_annot.Get_BB_Annot (home_bb);
    bb_annot->Init_New_OP_Annot (chk_op);

        /* maintain heuristic data 
         */
    _heur_mgr.Compute_Heur_Data_For_Inserted_OP (chk_op);

        /* and maintain other miscellaneous things
         */
    OP_ANNOT_Set_Cannot_Spec (chk_op);

    if (BB_call(OP_bb(chk_op))) {
        
        for (OP * op = chk_op ; op ; op = OP_next(op)) {
            if (OP_ANNOT_OP_Def_Actual_Para (op)) {
                new_arc_with_latency(CG_DEP_POSTCHK, chk_op, 
                    op, 0, 0, 0, FALSE);
            }
        }
    }

    return chk_op ;
}
#endif /* TARG_IA64 */



#if defined(TARG_SL2) 
// ======================================================================
// Can_OP_Move
// returns TRUE if <op> can be moved thru all blocks between <src_bb> and
// <tgt_bb>. 
// ======================================================================
static BOOL
Can_OP_Move(OP *cur_op,  OP* point, BOOL fwd = TRUE)
{
  REGISTER_SET mid_reg_defs[ISA_REGISTER_CLASS_MAX+1], 
		mid_reg_uses[ISA_REGISTER_CLASS_MAX+1];
  GTN_SET *mid_gtn_defs, *mid_gtn_uses;
  TN_SET *mid_tn_defs, *mid_tn_uses;

  REGSET_CLEAR(mid_reg_defs);
  REGSET_CLEAR(mid_reg_uses);

  OP* op; 
  OP* prev_op; // for debugging

  if(fwd) 
    op = OP_next(cur_op); 
  else 
    op = OP_prev(cur_op); 
	
  for (;  op != point; prev_op = op , op = fwd ? OP_next(op) : OP_prev(op)) 
  {
    Is_True(op, ("op is null")); 
    if (OP_dummy(op)) {
      if (!CGTARG_Is_OP_Barrier(op)) continue;
    }

       // accumulate all the mid_defs
    for ( INT i = 0; i < OP_results(op); ++i) {
      TN *result_tn = OP_result(op,i);
      REGISTER result_reg = TN_register (result_tn);
      ISA_REGISTER_CLASS cl = TN_register_class (result_tn);
      mid_reg_defs[cl] = REGISTER_SET_Union1(mid_reg_defs[cl],result_reg);
   }

       // accumulate all the mid_uses
    for (INT i = 0; i < OP_opnds(op); ++i) {
      TN *opnd_tn = OP_opnd(op,i);
      if (TN_is_constant(opnd_tn)) continue;
 
      REGISTER opnd_reg = TN_register (opnd_tn);
      ISA_REGISTER_CLASS cl = TN_register_class (opnd_tn);
      mid_reg_uses[cl] = REGISTER_SET_Union1(mid_reg_uses[cl], opnd_reg);
    } // accumulate all the mid_uses
  }

  Is_True(op==point, ("op will stop at point op")); 

  for(INT i = 0; i < OP_results(point); ++i)
  {
    TN *result_tn = OP_result(point, i);
    REGISTER result_reg = TN_register (result_tn);
    ISA_REGISTER_CLASS cl = TN_register_class (result_tn);
    mid_reg_defs[cl] = REGISTER_SET_Union1(mid_reg_defs[cl],result_reg);
  }

  for (INT i = 0; i < OP_opnds(point); ++i) {
    TN *opnd_tn = OP_opnd(point, i);
    
    if (TN_is_constant(opnd_tn)) continue;
    
    REGISTER opnd_reg = TN_register (opnd_tn);
    
    ISA_REGISTER_CLASS cl = TN_register_class (opnd_tn);
    
    mid_reg_uses[cl] = REGISTER_SET_Union1(mid_reg_uses[cl], opnd_reg);
    
  } // accumulate all the mid_uses
   
  for( INT i =0; i < OP_results(cur_op); i++) 
  {
     TN* result_tn = OP_result(cur_op, i); 
     ISA_REGISTER_CLASS cl = TN_register_class(result_tn); 
     REGISTER result_reg = TN_register(result_tn); 
     if( REGISTER_SET_Intersection1(mid_reg_uses[cl],  result_reg) || 
         REGISTER_SET_Intersection1(mid_reg_defs[cl], result_reg)) return FALSE; 
  }
  
  for( INT i =0; i < OP_opnds(cur_op); i++) 
  {
    TN* opnd_tn = OP_opnd(cur_op, i); 
    
    if(TN_is_constant(opnd_tn)) continue; 
    
    ISA_REGISTER_CLASS cl = TN_register_class(opnd_tn); 

    REGISTER opnd_reg = TN_register(opnd_tn); 
  
    if( REGISTER_SET_Intersection1(mid_reg_defs[cl],  opnd_reg)) return FALSE; 
  }
  return TRUE;
}


// foreach satd_op do following:
// {
//     find instr which satd_op depends on and 
//     is before satd and is closest to the satd_op
//     the op_idx for the instr is the start_idx; 
//     foreach instr which is after satd_op 
//           search such instr which depend on satd_op
//     the instr is the end_idx. 
//     the motion region of satd_op is limited to (start_idx, end_idx];
//
// }
// future improvent:
//    can move start_idx and end_idx to extend the motion region of satd. 
//      
typedef mUINT16 MOTION_BOUND;

class SATD_NODE {
  OP* _op; 
  BOOL combined; // flag used to descibe if the node has been combined;   
  vector<SATD_NODE*> _neighbors; 
  MOTION_BOUND _lower_bound; 
  MOTION_BOUND _upper_bound; 
  OP* _lower_bound_op; 
  OP* _upper_bound_op; 
  static std::map<OP*,  INT> _map_op_to_gid; 
  static std::map<INT, OP* > _map_gid_to_op; 
public: 
  SATD_NODE() {_lower_bound = 0; _upper_bound = 0; _op = NULL; _neighbors.clear(); };
  SATD_NODE(OP* op) { _op = op; _lower_bound = 0;  _upper_bound = 0; _neighbors.clear(); }
  ~SATD_NODE() {}; 
  MOTION_BOUND Get_Lower_Bound() {  return _lower_bound; }
  MOTION_BOUND Get_Upper_Bound() { return _upper_bound; }
  void Set_Lower_Bound(MOTION_BOUND bound ) { _lower_bound  =   bound; }
  void Set_Upper_Bound(MOTION_BOUND bound) { _upper_bound = bound; }
  OP* Get_Lower_Bound_OP() { return _lower_bound_op; }
  OP* Get_Upper_Bound_OP() { return _upper_bound_op; }
  void Set_Lower_Bound_OP(OP* bound_op ) { _lower_bound_op  =   bound_op; }
  void Set_Upper_Bound_OP(OP* bound_op) { _upper_bound_op = bound_op; }
  void Set_Node_Combined() { combined = TRUE; }
  BOOL Node_Combined() { return (combined == TRUE); }
  void Reset_Node_Combined() { combined = FALSE; }
  OP* Get_OP() {return _op; }
  BB* Get_BB() { return OP_bb(_op); }
  void Add_To_Neighbors( SATD_NODE *neighbor);  
  void Clear_Neighbors() {  _neighbors.clear(); }
  vector<SATD_NODE*> * Get_Neighbors() {return &_neighbors; }
  UINT Number_Of_Neighbors() {return _neighbors.size(); }
  static OP* Get_OP_From_Gid(INT gid) { return _map_gid_to_op[gid]; }
  static INT Get_Gid_From_OP(OP* op) {return _map_op_to_gid[op]; }
  static void Set_Gid_To_OP(INT gid, OP* op) { _map_op_to_gid[op]  = gid;}
  static void Set_OP_To_Gid(OP* op, INT gid)  { _map_gid_to_op[gid] = op;}
  BOOL Has_Interference(SATD_NODE* node); 

}; 

std::map<OP*, INT> SATD_NODE::_map_op_to_gid; 
std::map<INT, OP*> SATD_NODE::_map_gid_to_op; 

void 
SATD_NODE::Add_To_Neighbors(SATD_NODE *neighbor)
{
  _neighbors.push_back(neighbor); 
}

BOOL
SATD_NODE::Has_Interference(SATD_NODE * node)
{
 
//if one op lower bound even greater than the other op's upper bound
//it must has no iterference; 
  MOTION_BOUND upper_bound = node->Get_Upper_Bound(); 
  MOTION_BOUND lower_bound = node->Get_Lower_Bound(); 
  
  if(((upper_bound > _lower_bound) && (upper_bound <= _upper_bound)) ||
      ((_upper_bound > lower_bound) && (_upper_bound <= upper_bound)))
    return TRUE; 
  
  return FALSE;    
}

inline bool
less_than( SATD_NODE* node1, SATD_NODE* node2)
{
  if((node1->Get_Neighbors())->size() < (node2->Get_Neighbors())->size())
    return TRUE; 
  else if((node1->Get_Lower_Bound()) < (node2->Get_Lower_Bound()))
    return TRUE;
  else 
    return FALSE; 
}

void
Collect_SATD_In_BB(BB* bb, vector < SATD_NODE*  > & satd_nodes)
{
  for(OP* op = BB_first_op(bb); op; op= OP_next(op))
  {
    if(OP_code(op) == TOP_c2_satd) {
      SATD_NODE* satd_node = new SATD_NODE(op); 
      satd_nodes.push_back(satd_node); 
    }
  }
  return; 
}

void 
Valid_Neighbors(vector<SATD_NODE*> &satd_nodes) 
{
  vector<SATD_NODE*>::iterator iter; 
  vector<SATD_NODE*>* neighbors; 
  vector<SATD_NODE*>::iterator neighbor_iter; 
  INT upper_bound; 
  INT lower_bound; 
  INT neighbor_upper_bound; 
  INT neighbor_lower_bound; 
  for(iter = satd_nodes.begin(); iter !=satd_nodes.end(); iter++)
  {
    neighbors = (*iter)->Get_Neighbors(); 
    lower_bound = (*iter)->Get_Lower_Bound(); 
    upper_bound = (*iter)->Get_Upper_Bound(); 
    for(neighbor_iter = (*neighbors).begin(); neighbor_iter != (*neighbors).end(); neighbor_iter++)
    {
       neighbor_lower_bound = (*neighbor_iter)->Get_Lower_Bound(); 
       neighbor_upper_bound = (*neighbor_iter)->Get_Upper_Bound(); 
       if( (upper_bound < neighbor_lower_bound) || lower_bound > neighbor_upper_bound) 
         Fail_FmtAssertion("is not a valid neighbor");
    }
  }
  return; 
}

void 
Build_Interference_Graph_For_SATD(
   vector<SATD_NODE * > & satd_nodes) 
{
  vector<SATD_NODE*>::iterator iter1; 
  vector<SATD_NODE*>::iterator iter2; 
  for(iter1 = satd_nodes.begin(); iter1 != satd_nodes.end(); iter1++)
    (*iter1)->Clear_Neighbors(); 
  
  for(iter1 = satd_nodes.begin(); iter1 != satd_nodes.end() ; iter1++)
  {
    for(iter2 = satd_nodes.begin(); iter2 != satd_nodes.end(); iter2++)
    {
      if((*iter1) == (*iter2)) continue; 
      
      if((*iter1)->Has_Interference(*iter2))
      {
        (*iter1)->Add_To_Neighbors(*iter2); 
      }      
    }
  }
  Valid_Neighbors(satd_nodes);
  return; 
}
    

void 
Calculate_Boundary_For_Each_SATD(vector<SATD_NODE*> &satd_nodes)
{
  OP *satd_op, *tmp; 
  vector< SATD_NODE*>::iterator iter1; 
  vector< SATD_NODE*>::iterator iter2; 
  
// find lower bound 
  for(iter1 = satd_nodes.begin(); iter1 != satd_nodes.end(); iter1++)
  {
    satd_op =(*iter1)->Get_OP(); 
    BB* bb = (*iter1)->Get_BB(); 
    for(tmp = OP_prev(satd_op); tmp != BB_first_op(bb); tmp = OP_prev(tmp))
    {
// satd_op is the first op in bb 
      if( !tmp ) {
        Is_True((satd_op == BB_first_op(bb)),  ("satd_op must be the first op in bb"));
        (*iter1)->Set_Lower_Bound_OP(satd_op); 
        (*iter1)->Set_Lower_Bound(SATD_NODE::Get_Gid_From_OP(satd_op));
        break;       
      }
       
       if(!Can_OP_Move(satd_op, tmp, FALSE)) {
         (*iter1)->Set_Lower_Bound_OP(tmp); 
         (*iter1)->Set_Lower_Bound(SATD_NODE::Get_Gid_From_OP(tmp)); 
        break;          
      }          
    }
    if(tmp == BB_first_op(bb)) {
      (*iter1)->Set_Lower_Bound_OP(tmp); 
      (*iter1)->Set_Lower_Bound(SATD_NODE::Get_Gid_From_OP(tmp)); 
    }
    
// find upper bound     
    for(tmp = OP_next(satd_op); tmp != BB_last_op(bb); tmp = OP_next(tmp))
    {
//satd_op is the last op in bb;     
      if(!tmp) {
        Is_True((satd_op == BB_last_op(bb)), ("satd_op must be the last op in bb"));
        (*iter1)->Set_Upper_Bound_OP(satd_op); 
        (*iter1)->Set_Upper_Bound(SATD_NODE::Get_Gid_From_OP(satd_op)); 
        break;
      }
      if(!Can_OP_Move(satd_op, tmp)) {
       (*iter1)->Set_Upper_Bound_OP(tmp); 
       (*iter1)->Set_Upper_Bound(SATD_NODE::Get_Gid_From_OP(tmp)); 
       break;       
     }        
    }
    if(tmp == BB_last_op(bb)) {
      (*iter1)->Set_Upper_Bound_OP(tmp);         
      (*iter1)->Set_Upper_Bound(SATD_NODE::Get_Gid_From_OP(BB_last_op(bb)));         
    }
  }
  return; 
}

void 
Dump_BB(BB* bb,  char *output_msg, SATD_NODE* satd_node1, SATD_NODE* satd_node2, OP* point_op)
{
  OP * tmp;
  OP *op1 = satd_node1->Get_OP(); 
  OP* op2 = satd_node2->Get_OP(); 
  
  fprintf(TFile,  "%s   %s    %s", DBar,   output_msg,  DBar); 
  for(tmp = BB_first_op(bb);  tmp; tmp = OP_next(tmp)) {

    if(tmp == op1 || tmp == op2)
      fprintf(TFile, "===>"); 
    
    Print_OP_No_SrcLine(tmp); 
  }
  fprintf(TFile, "point op is:"); 
  Print_OP_No_SrcLine(point_op);
  fprintf(TFile, "lower_bound of op1:"); 
  Print_OP_No_SrcLine(satd_node1->Get_Lower_Bound_OP());
  fprintf(TFile, "upper_bound of op1:"); 
  Print_OP_No_SrcLine(satd_node1->Get_Upper_Bound_OP());  
  fprintf(TFile, "lower_bound of op2:"); 
  Print_OP_No_SrcLine(satd_node2->Get_Lower_Bound_OP());
  fprintf(TFile, "upper_bound of op2:"); 
  Print_OP_No_SrcLine(satd_node2->Get_Upper_Bound_OP());  
  
  return; 
}

// this function is used to assign each op to a gid and build a mapping 
// between op and gid. remember to call this function when satd 
// combination happens. 
void 
Numbering_BB_OPs(BB* bb)
{
  INT index = 0; 
  for(OP* op = BB_first_op(bb); op ; op = OP_next(op), index++)
  {
    SATD_NODE::Set_Gid_To_OP(index, op); 
    SATD_NODE::Set_OP_To_Gid(op, index); 
  }
}

void
Check_Combination(BB* bb,  vector<SATD_NODE*> & satd_nodes)
{
  vector<SATD_NODE*>::iterator iter;
    
  Numbering_BB_OPs(bb);
  
  for(iter = satd_nodes.begin(); iter != satd_nodes.end(); iter++)
  {
    OP* op = (*iter)->Get_OP(); 
    OP* lower_bound_op = (*iter)->Get_Lower_Bound_OP(); 
    OP* upper_bound_op = (*iter)->Get_Upper_Bound_OP(); 
    INT gid = SATD_NODE::Get_Gid_From_OP(op); 
    INT lower_bound = SATD_NODE::Get_Gid_From_OP(lower_bound_op); 
    INT upper_bound = SATD_NODE::Get_Gid_From_OP(upper_bound_op); 
    if(lower_bound > gid || upper_bound < gid  )
      Fail_FmtAssertion("Move op across bound op"); 
  }
}


//  lower_bound_op1                  lower_bound_op1
//    ......                                          .....
//   c2.satd                                lower_bound_op2
//    .......                                         ....
//  lower_bound_op2                         c2.satd
//    .....                       ====>         c2.satd
//  upper_bound_op1                         .....
//    ....                                     upper_bound_op1
//    c2.satd                                       ....
//   ....                                      upper_bound_op2
//  upper_bound_op2
//
// function: this function is used to combine two satd instructions 
//   step1:
//      find a point in the intersection of the two nodes' motion boundary
//  step2 
//      move the two nodes to the intersection point; 
void 
Do_Combination(SATD_NODE * node1,  SATD_NODE * node2)    
{

  MOTION_BOUND lower_bound1, lower_bound2; 
  MOTION_BOUND upper_bound1, upper_bound2; 
  OP* lower_bound_op, *upper_bound_op; 
  OP* point_op; 
  
  lower_bound1  = node1->Get_Lower_Bound(); 
  lower_bound2 = node2->Get_Lower_Bound(); 
  upper_bound1 = node1->Get_Upper_Bound(); 
  upper_bound2 = node2->Get_Upper_Bound(); 

  Is_True((upper_bound1 >= lower_bound2), ("two node has no intersection region for combination"));


  if(lower_bound1 <= lower_bound2) 
    lower_bound_op = SATD_NODE::Get_OP_From_Gid(lower_bound2); 
  else 
    lower_bound_op = SATD_NODE::Get_OP_From_Gid(lower_bound1); 

  if(upper_bound1 <= upper_bound2)
    upper_bound_op = SATD_NODE::Get_OP_From_Gid(upper_bound1);
  else 
    upper_bound_op = SATD_NODE::Get_OP_From_Gid(upper_bound2); 

  if(lower_bound_op == upper_bound_op)
    point_op = lower_bound_op;
  else 
    point_op = OP_next(lower_bound_op); 
  
  OP* satd_op1 = node1->Get_OP(); 
  
  OP* satd_op2 = node2->Get_OP(); 

  if(SCHED_TF_DUMP_SATD_COMB) {
    fprintf(TFile,  "before node1 combined = %d\n", node1->Node_Combined()); 
    fprintf(TFile,  "before node2 combined = %d\n", node2->Node_Combined());     
  }
  
  node1->Set_Node_Combined(); 
  node2->Set_Node_Combined(); 
  
// satd_op1 is also adjacent to satd_op2; 
  if((OP_next(satd_op1) ==satd_op2) || 
    (OP_next(satd_op2) == satd_op1))
    return;
  if(SCHED_TF_DUMP_SATD_COMB)
    Dump_BB(OP_bb(satd_op1), "Before Combination", node1, node2, point_op); 

// v1 $1 c2.satd .. <== satd_op2 
//     ...
//   point_op
//     ...
// v2 $1 c2.satd  <== satd_op1
// if gid1 of satd_op1 greater than that of satd_op2, first move satd_op2 and 
// then satd_op1 to keep order 

  INT gid1 = SATD_NODE::Get_Gid_From_OP(satd_op1); 
  INT gid2 = SATD_NODE::Get_Gid_From_OP(satd_op2); 
  
  if(satd_op1 != point_op) {
    BB_Move_Op_Before(OP_bb(point_op), point_op, OP_bb(point_op), satd_op1);
  }
  if(satd_op2 != point_op) {
    if(gid1 > gid2)  {
      BB_Move_Op_Before(OP_bb(point_op),  satd_op1, OP_bb(point_op), satd_op2);    
    }
    else {
      BB_Move_Op_Before(OP_bb(point_op), point_op, OP_bb(point_op), satd_op2);
    }
  }

  if(SCHED_TF_DUMP_SATD_COMB) {
    fprintf(TFile,  "node1 combined = %d\n", node1->Node_Combined()); 
    fprintf(TFile,  "node2 combined = %d\n", node2->Node_Combined());     
    Dump_BB(OP_bb(satd_op1),  "After Combination", node1, node2, point_op); 
  }
  return; 
}

SATD_NODE*
Find_One_Neighbor(SATD_NODE* node)
{
  vector<SATD_NODE*> * neighbors = node->Get_Neighbors(); 
  vector<SATD_NODE*>::iterator neighbor_iter; 

  for(neighbor_iter = neighbors->begin(); neighbor_iter != neighbors->end(); neighbor_iter++)
  {

// if the satd node has been processed         
    if((*neighbor_iter)->Node_Combined())
      continue;           
    else {
      return (*neighbor_iter); 
    }
  }
  return NULL; 
}

void 
Combine_SATD_In_Pair(vector<SATD_NODE*> &satd_nodes)
{
  vector<SATD_NODE*>::iterator iter; 
  vector<SATD_NODE*> zero_neighbor_nodes; 

// don's has enough satd for combination. 
  if(satd_nodes.size() < 2)
    return; 

  for(iter = satd_nodes.begin();  iter != satd_nodes.end(); iter++)
  {
    if((*iter)->Number_Of_Neighbors() == 0 )
      continue; 
    else if((*iter)->Node_Combined())
      continue; 
    else 
    {
      SATD_NODE* neighbor = Find_One_Neighbor(*iter); 

// don't find neighbor for combination.     
      if(neighbor == NULL) 
        continue; 
      else {
        Do_Combination(*iter, neighbor); 
        Numbering_BB_OPs((*iter)->Get_BB()); 
        Calculate_Boundary_For_Each_SATD(satd_nodes); 
        Build_Interference_Graph_For_SATD(satd_nodes); 
      }
      // get neighbor node 
      // generate macro instruction 
      // processed 
    }
  }
  return; 
}


void 
Find_Zero_Neighbor_Nodes(vector<SATD_NODE*> &satd_nodes,  vector < SATD_NODE*> &zero_neighbor_nodes)
{
  vector<SATD_NODE*>::iterator iter; 
  for(iter = satd_nodes.begin(); iter != satd_nodes.end(); iter++)
  {
    if((*iter)->Number_Of_Neighbors() == 0)
      zero_neighbor_nodes.push_back(*iter);         
  }
  return; 
}



//  lower_bound_op1                  lower_bound_op1                  lower_bound_op1
//    ......                                          .....                                      .......
//   c2.satd                                       c2.satd                          lower_bound_op2      
//    .......                                         ....                                       .......
//  upper_bound_op1                 lower_bound_op2                         c2.satd 
//    .....                       ====> upper_bound_op1  =====>          c2.satd
//  lower_bound_op2                         .....                                      ........
//    ....                                            c2.satd                          upper_bound_op1
//    c2.satd                                       ....                                       ........
//   ....                                      upper_bound_op2                 uppper_bound_op2
//  upper_bound_op2
//
// function: this function is used to combine two satd instructions 
//   step1:
//      find a point in the intersection of the two nodes' motion boundary
//  step2 
//      move the two nodes to the intersection point; 
void
Expand_Motion_Space(vector<SATD_NODE*> & satd_nodes)
{
  vector< SATD_NODE *> zero_neighbor_nodes;
  vector< SATD_NODE *> processed;
  vector< SATD_NODE *>::iterator iter;
  vector< SATD_NODE *>::iterator iter2;
  Find_Zero_Neighbor_Nodes(satd_nodes, zero_neighbor_nodes);
 
  for(iter = zero_neighbor_nodes.begin(); iter != zero_neighbor_nodes.end(); iter++)
  {
    for(iter2 = zero_neighbor_nodes.begin(); iter2 != zero_neighbor_nodes.end(); iter2++) 
    {
      if(iter2 == iter) 
        continue; 

      if(find(processed.begin(), processed.end(), *iter) != processed.end())
        continue; 

      MOTION_BOUND lower_bound1 = (*iter)->Get_Lower_Bound(); 
      MOTION_BOUND upper_bound1 = (*iter)->Get_Upper_Bound(); 
      MOTION_BOUND lower_bound2 = (*iter2)->Get_Lower_Bound(); 
      MOTION_BOUND upper_bound2 = (*iter2)->Get_Upper_Bound(); 

      if( upper_bound1 < lower_bound2)
      {
        OP* upper_bound_op = (*iter)->Get_Upper_Bound_OP(); 
        OP* lower_bound_op = (*iter2) ->Get_Lower_Bound_OP(); 
        OP* tmp = OP_next(upper_bound_op); 
        while(tmp != lower_bound_op) {
          OP* next = OP_next(tmp); 
          if(Can_OP_Move(tmp, upper_bound_op, FALSE)) 
          {
            BB_Move_Op_Before(OP_bb(tmp), upper_bound_op, OP_bb(tmp), tmp); 
          }
          tmp = next; 
        }
        if(tmp == lower_bound_op && Can_OP_Move(tmp, upper_bound_op, FALSE)) {
          BB_Move_Op_Before(OP_bb(tmp), upper_bound_op, OP_bb(tmp), tmp); 
          processed.push_back(*iter); 
          processed.push_back(*iter2); 
        }
      }
      else if(lower_bound1 > upper_bound2)
      {
        OP* upper_bound_op = (*iter2)->Get_Upper_Bound_OP(); 
        OP* lower_bound_op = (*iter) ->Get_Lower_Bound_OP(); 
        OP* tmp = OP_next(upper_bound_op); 
        while(tmp != lower_bound_op) {
          OP* next = OP_next(tmp); 
          if(Can_OP_Move(tmp, upper_bound_op, FALSE)) 
          {
            BB_Move_Op_Before(OP_bb(tmp), upper_bound_op, OP_bb(tmp), tmp); 
          }
          tmp = next; 
        }
        if(tmp == lower_bound_op && Can_OP_Move(tmp, upper_bound_op, FALSE)) {
          BB_Move_Op_Before(OP_bb(tmp), upper_bound_op, OP_bb(tmp), tmp); 
          processed.push_back(*iter); 
          processed.push_back(*iter2); 
        }
      }
    }
  }
}

void 
Pairwise_SATD_OP(BB* bb)
{
  vector <SATD_NODE*> satd_nodes ;     
  vector < OP* >::iterator iter; 
  vector < SATD_NODE*>::iterator node_iter; 

// due to map_idx is not ordering after schedling, we need allocate a 
// global number for each op in BB
  Numbering_BB_OPs(bb); 
  
//step 1: find all c2.satd op   
  Collect_SATD_In_BB(bb, satd_nodes); 

//no satd in bb, directly return; 
  if(satd_nodes.empty()) return; 

//step 2:   calculate (lower_bound, upper_bound]
  Calculate_Boundary_For_Each_SATD(satd_nodes); 

//step 3: build interfere_graph for satd boundary region 
  Build_Interference_Graph_For_SATD(satd_nodes); 

  Expand_Motion_Space(satd_nodes); 

// expand motion space will move boundary op and 
// causes the global number is not correct, we need 
// to update again 
  Numbering_BB_OPs(bb); 

//recalculate boundary after expanding motion space
  Calculate_Boundary_For_Each_SATD(satd_nodes); 

//rebuild interference graph for satd after expanding motion space
  Build_Interference_Graph_For_SATD(satd_nodes); 

  Valid_Neighbors(satd_nodes);

  std::stable_sort(satd_nodes.begin(), satd_nodes.end(), less_than); 

  Valid_Neighbors(satd_nodes);

  if(SCHED_TF_DUMP_SATD_COMB)
  {
    vector<class SATD_NODE*>::iterator iter; 

    fprintf(TFile, "%s  BB:%d   SATD_COMBINATION  Start \n", DBar, BB_id(bb));

    for( iter = satd_nodes.begin(); iter != satd_nodes.end(); iter++) {
      fprintf(TFile,  "satd_nodes neighbours  number:     %d\n", ((*iter)->Get_Neighbors())->size()); 
      fprintf(TFile, "lower bound is : %d\n" , ((*iter)->Get_Lower_Bound())); 
      fprintf(TFile,  "upper bound is: %d\n",  ((*iter)->Get_Upper_Bound())); 
    }
    
    fprintf(TFile,  "%s, SATD_COMBINATION END %s\n", DBar, DBar); 
  }

  Valid_Neighbors(satd_nodes);

  Combine_SATD_In_Pair(satd_nodes); 

  Valid_Neighbors(satd_nodes);

  Check_Combination(bb, satd_nodes);

  return; 
}

void 
Move_Macro_Insn_Together() 
{
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    Pairwise_SATD_OP(bb); 
  }
  return; 
}
#endif 
    /* ===============================================================
     * 
     *  BB_Move_Op_Before 
     * 
     *  move <op> from <from_bb> to <to_bb> right before <point>, 
     *  maintain dependence, annotation if necessary.
     *
     *  if <point> is NIL, we append <op> to <to_bb>.
     *
     * ================================================================
     */
void
SCHEDULER::BB_Move_Op_Before (BB *to_bb, OP *point, BB *from_bb, OP *op) {
    
    /* before code motion, keep something:
     * (1) orignal annotation, and (2) dep info 
     */

    SCHED_BB_ANNOT * bb_annot ;
    SCHED_OP_ANNOT * op_annot ;
    void           * op_heur ;

    BB_OP_MAP  omap 
        = (BB_OP_MAP) BB_MAP_Get(_cg_dep_op_info, OP_bb(op));
    _CG_DEP_OP_INFO *dep_info = 
        (_CG_DEP_OP_INFO *) BB_OP_MAP_Get(omap, op);

    bb_annot = sched_annot.Get_BB_Annot (from_bb); 
    op_annot = bb_annot->Detach_OP_Annot (op);
    op_heur  = _heur_mgr.Detach_OP_Heur_Info (op);

    if (point) {
        ::BB_Move_Op_Before (to_bb, point, from_bb, op);
    } else {
        /* "move OP before _NOTHING_", so we append <op> to 
         * the end of <to_bb>
         */
#if defined(TARG_SL)         
        OP* last_op = BB_last_op (to_bb) ;
        if (last_op && TOP_is_xfer (OP_code(last_op))) {
          ::BB_Move_Op_Before (to_bb, last_op, from_bb, op);
        } else {
        /* empty bb or the last op is not branch, append <op>
         * at the end of <bb>
         */
          ::BB_Move_Op_To_End (to_bb, from_bb, op);
        }
#else 
        ::BB_Move_Op_To_End (to_bb, from_bb, op);
#endif         
    }

    omap = (BB_OP_MAP) BB_MAP_Get(_cg_dep_op_info, OP_bb(op));
    BB_OP_MAP_Set (omap, op, dep_info);

    bb_annot = sched_annot.Get_BB_Annot (to_bb);
    bb_annot->Attach_OP_Annot (op, op_annot) ; 
    _heur_mgr.Attach_OP_Heur_Info (op,op_heur);
}

#if defined(TARG_SL) || defined(TARG_SL2)
    /* ===============================================================
     * 
     *  BB_Move_Op_After
     * 
     *  move <op> from <from_bb> to <to_bb> right after <point>, 
     *  maintain dependence, annotation if necessary.
     *
     *  if <point> is NIL, we append <op> to <to_bb>.
     *
     * ================================================================
     */
void
SCHEDULER::BB_Move_Op_After (BB *to_bb, OP *point, BB *from_bb, OP *op) 
{
    
    /* before code motion, keep something:
     * (1) orignal annotation, and (2) dep info 
     */

    SCHED_BB_ANNOT * bb_annot ;
    SCHED_OP_ANNOT * op_annot ;
    void           * op_heur ;

    BB_OP_MAP  omap 
        = (BB_OP_MAP) BB_MAP_Get(_cg_dep_op_info, OP_bb(op));
    _CG_DEP_OP_INFO *dep_info = 
        (_CG_DEP_OP_INFO *) BB_OP_MAP_Get(omap, op);

    bb_annot = sched_annot.Get_BB_Annot (from_bb); 
    op_annot = bb_annot->Detach_OP_Annot (op);
    op_heur  = _heur_mgr.Detach_OP_Heur_Info (op);

    if (point) {
        ::BB_Move_Op_After (to_bb, point, from_bb, op);
    } else {
        /* "move OP before _NOTHING_", so we append <op> to 
         * the end of <to_bb>
         */
        OP* last_op = BB_last_op (to_bb) ;
        if (last_op && TOP_is_xfer (OP_code(last_op))) {
          ::BB_Move_Op_Before (to_bb, last_op, from_bb, op);
        } else {
                /* empty bb or the last op is not branch, append <op>
                 * at the end of <bb>
                 */
          ::BB_Move_Op_To_End (to_bb, from_bb, op);
        }
    }

    omap = (BB_OP_MAP) BB_MAP_Get(_cg_dep_op_info, OP_bb(op));
    BB_OP_MAP_Set (omap, op, dep_info);

    bb_annot = sched_annot.Get_BB_Annot (to_bb);
    bb_annot->Attach_OP_Annot (op, op_annot) ; 
    _heur_mgr.Attach_OP_Heur_Info (op,op_heur);
}
#endif 
    /* ===================================================
     * 
     *  Gen_P_Ready_Bookeeping_OP_DAG 
     *
     *  supporint routine to Gen_Compensation_Code.
     *  this func is supposed to generate dependency ARCs
     *  for bookeeping instruction.
     *
     * ==================================================
     */
void
SCHEDULER :: Gen_Bookeeping_OP_DAG 
    (CANDIDATE& cand, OP* compensate, BOOKEEPING* bk) {


    BOOL p_ready_bookeeping = bk->Is_P_Ready_Bookeeping ();

    OP* model = cand.Op ();
    BB* place = bk->Get_Placement ();
     
    BB_OP_MAP_Set (
        (BB_OP_MAP) BB_MAP_Get(_cg_dep_op_info,place), 
         compensate, new_op_info ());

        /* generate the out-going ARCs 
         */
    ARC_LIST* dep_lst ;
    for (dep_lst = OP_succs(model); 
         dep_lst != NULL;
         dep_lst = ARC_LIST_rest(dep_lst)) {

        ARC* tmp = ARC_LIST_first(dep_lst);

        if (!tmp || ARC_is_br(tmp)) continue ;
        new_arc_with_latency 
                (ARC_kind(tmp),  /* arc kind remain unchanged */
                 compensate,     /* predecessor */
                 ARC_succ(tmp),  /* successor remains unchanged */
                 ARC_latency(tmp),
                 ARC_omega(tmp), /* dose not care */
                 ARC_opnd(tmp),
                 ARC_is_definite(tmp));
    }

        /* generate incoming ARCs
         */
    for (dep_lst = OP_preds(model);
         dep_lst != NULL;
         dep_lst = ARC_LIST_rest(dep_lst)) {

        ARC* tmp = ARC_LIST_first(dep_lst);
        if (!tmp || ARC_is_br(tmp)) {
                /* leave to DAB_BUILDER::Build_Branch_Arcs to 
                 * regenerate CG_DEP_PRE|POST_BR arcs from scratch.
                 */
            continue ;
        }

        BB* pred_bb = OP_bb(ARC_pred(tmp));

        if (pred_bb == place||
            _cflow_mgr.BB1_Reachable_From_BB2 (place,pred_bb)) {

            new_arc_with_latency (ARC_kind(tmp), 
                     ARC_pred(tmp), compensate, 
                     ARC_latency(tmp), ARC_omega(tmp), 
                     ARC_opnd(tmp), ARC_is_definite(tmp));

        } else if (_cflow_mgr.BB1_Reachable_From_BB2 (pred_bb,place)) {
            Gen_Inverted_Arc (&cand, tmp,   /* ref arc */
                              compensate,   /* pred */
                              ARC_pred(tmp) /* succ */);
        }
    }

    _dag_constructor.Build_Branch_Arcs (compensate, INCLUDE_CONTROL_ARCS);
}

    /* ===================================================
     *
     *  Gen_Compensation_Code 
     *
     *  copy a indentical copy to the bookeeping place.
     *  maintain something associated with <cand>. 
     *  and return the copied verion.
     *
     * ===================================================
     */
OP* 
SCHEDULER::Gen_Compensation_Code 
    (CANDIDATE& model_cand, BB* org_home, 
     BOOKEEPING* bk, BOOL append) {

    BB* place = bk->Get_Placement ();
    Is_True (!BB_Is_Isolated_From_Sched (place),
("Cannot prepend or append compensation code to a isolated BB:%d", 
            BB_id(place)));

        /* step 1 : duplication op which is look like <model>.
         *          copy model's attribute,etc 
         */
    OP* model = model_cand.Op ();
    OP* op    = Dup_OP (model) ;
    OP_srcpos (op) = OP_srcpos(model);
    
        /* step 4.a  preparation for maintaining annotation 
         */
    SCHED_BB_ANNOT* annot = sched_annot.Get_BB_Annot (place);

        /* step 2: place compensation code at proper place
         */
    BOOL insert_op = FALSE;
    if (append) {

            /* If the last op of bb is branch , insert right before it 
             */
        OP* last_op = BB_last_op (place) ;
        if (last_op && TOP_is_xfer (OP_code(last_op))) {
            BB_Insert_Op_Before (place, last_op, op) ;
            insert_op = TRUE;
        } else {
                /* empty bb or the last op is not branch, append <op>
                 * at the end of <bb>
                 */
            BB_Append_Op (place, op) ;
        }
    } else {
        FmtAssert (FALSE, 
("OP[%d] which is now in BB:%d is moved downward,however downward code has yet implemented",
        OP_map_idx(model),BB_id(place)));
    }

        /* step 3 : maintain the Whirl node 
         */
    if (OP_memory(op)) {
        Copy_WN_For_Memory_OP (op,model);
    }

        /* step 4.b : maintain the SCHED_OP_ANNOT structure
         */
    SCHED_OP_ANNOT* op_annot = annot->Init_New_OP_Annot (op) ;
    SCHED_OP_ANNOT* model_annot = sched_annot.Get_OP_Annot (model) ;

    op_annot->_ext_flags = model_annot->_ext_flags & 
                            (OP_EXT_MASK_NO_CNTL_SPEC | 
                             OP_EXT_MASK_NO_DATA_SPEC |
                             OP_EXT_MASK_ACTUAL) |
                             OP_EXT_MASK_COMPENSATION; 

    op_annot->_op = op ;
    op_annot->_org_home_bb = org_home;


        /* step 5: maintain dependence info
         */
    Gen_Bookeeping_OP_DAG (model_cand,op,bk);


        /* step 6 : maintain heuristic info. this should be performed 
         *          after the dependence info become valid up-to-date.
         */
    if (append) {
        if (insert_op) {
            _heur_mgr.Compute_Heur_Data_For_Inserted_OP (op);
        } else {
            _heur_mgr.Compute_Heur_Data_For_Appended_OP (op);
        }
    } else {
        FmtAssert (FALSE,("Downward code motion has yet implemented\n"));
    }

        /* step 7: other miscellaneous works.
         */
    SPEC_TYPE spec_type = model_cand.Spec_Type ();
    if (spec_type & SPEC_DATA) {
        Set_OP_data_spec  (op);
        Set_OP_orig_bb_id (op,BB_id(org_home));
    }

    if (spec_type & SPEC_CNTL) {
        Set_OP_cntl_spec  (op);
        Set_OP_orig_bb_id (op,BB_id(org_home));
    }

    return op ;
}


    /* ==============================================================
     *
     *  Gen_Inverted_Arc 
     *
     * ref the header file for details.
     *
     * =============================================================
     */
void
SCHEDULER::Gen_Inverted_Arc 
    (CANDIDATE* cand, ARC* ref_arc, OP* pred, OP* succ) {

    CG_DEP_KIND arc_kind;
    INT16 arc_opnd = 0;
    TN*  arc_opnd_tn;

    OP* arc_pred = ARC_pred(ref_arc);
    OP* arc_succ = ARC_succ(ref_arc);
     

    switch (ARC_kind(ref_arc)) {
    case CG_DEP_REGIN:
        arc_kind = CG_DEP_REGANTI;

        arc_opnd_tn = OP_opnd(arc_succ,ARC_opnd(ref_arc));
        for (arc_opnd = OP_results(arc_succ) - 1;
             arc_opnd >= 0;
             arc_opnd--) {
            if (OP_result(arc_pred, arc_opnd) == arc_opnd_tn) {
                break;
            }
        }

        arc_opnd = (arc_opnd >= 0) ? arc_opnd : 0;
        break;


    case CG_DEP_REGOUT:
        arc_kind = CG_DEP_REGIN;
        break;


    case CG_DEP_REGANTI:
        arc_kind = CG_DEP_REGIN;
        arc_opnd_tn = OP_result(arc_succ, ARC_opnd(ref_arc));
        for (arc_opnd = OP_opnds (arc_pred) - 1;
             arc_opnd >= 0;
             arc_opnd --) {
            if (OP_opnd(arc_pred, arc_opnd) == arc_opnd_tn) { break; }
        }

        arc_opnd = (arc_opnd >= 0) ? arc_opnd : 0;
        break;

    case CG_DEP_MEMOUT:
        arc_kind = CG_DEP_MEMOUT; break;

    case CG_DEP_MEMANTI:
        arc_kind = CG_DEP_MEMIN; break;

    case CG_DEP_MEMVOL:
        arc_kind = CG_DEP_MEMVOL; break;

    case CG_DEP_MEMREAD:
        arc_kind = CG_DEP_MEMREAD; break;
        
    case CG_DEP_SPILLIN:
    case CG_DEP_PREFIN:
    case CG_DEP_PREFOUT:
    case CG_DEP_SCC:
    case CG_DEP_MISC:
        arc_kind = CG_DEP_MISC; break;

    case CG_DEP_MEMIN:
        arc_kind = CG_DEP_MEMANTI; break;

    case CG_DEP_PREBR:
        arc_kind = CG_DEP_POSTBR; break;

#ifdef TARG_IA64
    case CG_DEP_PRECHK:
        arc_kind = CG_DEP_POSTCHK; break;

    case CG_DEP_POSTCHK:
        arc_kind = CG_DEP_PRECHK; break;
        break;
#endif

    case CG_DEP_POSTBR:
        arc_kind = CG_DEP_PREBR; break;

    default:
        FmtAssert (FALSE, ("unknown CG_DEP_KIND %X\n", ARC_kind(ref_arc)));
        break;
    } /* end of switch */

    
        /* this ugly code fragment is just a workaround to new_arc,
         * which may generate ARC with negative latency.
         */
    ARC* New_Arc;
    switch (ARC_kind(ref_arc)) {
    case CG_DEP_PREBR:
    case CG_DEP_POSTBR:
#ifdef TRAG_IA64
    case CG_DEP_PRECHK:
    case CG_DEP_POSTCHK:
#endif
        New_Arc = new_arc_with_latency 
                      (arc_kind, 
                       pred, succ, 
                       0, 0 /* omega. we do not care this parameter */,
		               arc_opnd, TRUE);
        break;
    default:
        New_Arc = new_arc (arc_kind, pred, succ, 0, arc_opnd, TRUE);
    }

    Is_True (ARC_latency(New_Arc) >= 0, 
             ("Negative latency (%d)", ARC_latency(New_Arc)));
}

    /* ===============================================================
     *
     * Maintain_Dep_Arcs_After_Sched 
     *
     * This should be done after 
     *  - instruction is actually moved from its original place to 
     *    new one.
     *  - 'Gen_Compensation_Code' for all book-keeping.
     *
     * ==============================================================
     */
void
SCHEDULER::Maintain_Dep_Arcs_After_Sched (CANDIDATE* cand) {

    OP* op = cand->Op ();

        /* Make sure <op> has already scheduled
         */
    Is_True (OP_Scheduled(op), 
             ("OP:%d(BB:%d) must be scheduled",
              OP_map_idx(op), BB_id(OP_bb(op))));

        /* Invert Up-side down arcs and prune unnecessary ARCs
         */
    for (ARC_LIST* arcs = OP_preds(op); arcs ;) {

        ARC *arc = ARC_LIST_first(arcs);
        arcs = ARC_LIST_rest(arcs);
        OP* pred = ARC_pred (arc);

        if (OP_Scheduled(pred)) {
                /* case 1: when this situation occurs, it indicate
                 *         the dep arc between <pred> and op is not
                 *         up-side-down with the *EXCEPTION*:
                 * 
                 *         ====> EXCEPTION <=======
                 *
                 *         cntl-xfer-op (of the same block of <op>) 
                 *         may be scheduled before op. We categorize
                 *         this exception to case 3. 
                 */
            if (!OP_xfer(pred) || OP_bb(pred) != OP_bb(op)) {
                continue;
            }
        }

            /* case 2 : the arc become a "cross" one. prune these kind
             *          of arcs.
             *    e.g 
             *          cfg : BB1 => { BB2 , BB3 } => BB4.
             *          'ld4 r2=[r3]' is a instruction of BB4, and it 
             *          depends upon 'add r2=r5,r6' in BB3. when ld4 is 
             *          sched from BB4 to BB2 with its duplication ld4'
             *          being appended to the end of BB3, the arc between
             *          add and ld4(not ld4') become "cross" arcs.
             */
        if (!_cflow_mgr.BB1_Reachable_From_BB2 (OP_bb(op),OP_bb(pred)) &&
            !_cflow_mgr.BB1_Reachable_From_BB2 (OP_bb(pred),OP_bb(op)) &&
            OP_bb(op) != OP_bb(pred)) {

           CG_DEP_Detach_Arc (arc);
           continue;     

        }
             
            /* case 3 : after sched, arc become up-side-down.
             */
         Gen_Inverted_Arc (cand, arc,   /* ref arc */
                           ARC_succ(arc),/* pred */
                           ARC_pred(arc) /* succ */);

         CG_DEP_Detach_Arc (arc);
          
    } /* for(ARC_LIST* arcs = OP_preds(br); arcs ;) */
}

INT32 moved_op_num = 0; 
    /* ===========================================================
     *
     *  Commit_Schedule 
     *
     *  Perform "actual" schedule of the best candidate - <cand>. 
     *  Following actions are involved:
     *
     *    - Move candidate right after <_frontier_op>. For a scheduler
     *      in top-down flavor, the scheduled and unscheduled OPs 
     *      are separated. Namely, there exist a point that carve
     *      OPs into two parts, all OPs in the upper part(inc this point)
     *      are scheduled, and all OPs in the lower part are unscheduled. 
     *      We use <_fontier_op> to indicate this point.
     *
     *    - Transform <cand> if 
     *          - it is a load, and 
     *          - speculation is involved in this code motion, and
     *          - if necessary (safe load can not trigger exception   
     *            even if it is control-speculated.).
     *      
     *    - Insert chk instruction is necessary.  
     *
     *    - perform compensation or book-keeping at the same time.  
     *
     *    - Maintain the data and control dependence among this 
     *      candidate, compensation code and other instructions.  
     * 
     *    - update liveness if instruction is moved beyond basic 
     *      block's boundary. 
     * 
     *    - inform micro-scheduler to change its internal status.
     *      
     * ===========================================================
     */
BOOL
SCHEDULER::Commit_Schedule (CANDIDATE& cand) {

    moved_op_num++; 

    OP* op        = cand.Op (); 
    BB* home_bb   = OP_bb(op);
    OP* cmp_op = NULL; /* comp OP that define <op>'s guarding predicate */ 
    TN* op_qp_bak = NULL;

    BOOL insert_chk = FALSE ;
    BOOL cntl_spec_if_converted_code = FALSE;
    SRC_BB_INFO * src_info = _src_bb_mgr.Get_Src_Info (OP_bb(cand.Op()));
    
        /* ref the comment rigth before Get_Up_to_Date_Spec_Type ()
         * to see why we need to get "-*CURRENT*- spec type. 
         */
    if (OP_load(op) && cand.Is_Spec ()) {
        cand.Get_Up_to_Date_Spec_Type ();
    }

        /* Hint: <_frontier_op> marks the boundary of scheduled code 
         *   and unscheduled code. <_frontier_op> itself is the latest 
         *   scheduled instruction. 
         */
    if (op != _frontier_op) {
    	
        OP* pos = OP_prev(op);
        OP* chk_op = NULL;

        BB_VECTOR* cutting_set = src_info->Get_Cutting_Set ();
        SCHED_SPEC_HANDSHAKE::Change_Load_Spec_Form 
            (&cand, cutting_set->size(),&insert_chk, this); 

        BB_Move_Op_Before (_target_bb, _frontier_op, OP_bb(op), op) ;

#ifdef TARG_IA64
        if (insert_chk) { chk_op = Insert_Check (op, home_bb, pos); }
#endif
        BOOKEEPING_LST* bkl = cand.Bookeeping_Lst ();

        for (BOOKEEPING* bk = bkl->First_Item ();
             bk != NULL;
             bk = bkl->Next_Item (bk)) {

             OP* bookeeping_op = Gen_Compensation_Code (cand, home_bb, bk);
#ifdef TARG_IA64
    	     if (insert_chk && bk->Is_P_Ready_Bookeeping ()) {
        		Set_Speculative_Chain_Begin_Point (chk_op,bookeeping_op);
    	     }
#endif
        }
    } else {
        _frontier_op = OP_next(_frontier_op);
    }

    Set_OP_Scheduled (op);   /* mark inst has been scheduled */
    OP_scycle(op) = (mINT16)_cur_cyc ; /* the issue cycle */

        /* ASM file annotation support */ 
    SPEC_TYPE spec_type = cand.Spec_Type ();
    if (spec_type & SPEC_DATA) { Set_OP_data_spec(op); }
    if (spec_type & SPEC_CNTL) { Set_OP_cntl_spec(op); }
    if (spec_type & (SPEC_DATA | SPEC_CNTL)) { 
        Set_OP_orig_bb_id (op,BB_id(home_bb));
    }

    if (cand.Spec_Type () != SPEC_NONE ||
        home_bb != _target_bb) {

        _upward_motion_num ++;

            /* maintain dependency ARCs */
        Maintain_Dep_Arcs_After_Sched (&cand);
    }

        /* update the liveness info */
    if (home_bb != _target_bb) {
        _dflow_mgr.Update_Liveness_After_Upward_Sched 
            (&cand,src_info, &_cflow_mgr) ; 
    }


        /* Those instructions just becoming ready should be 
         * added into the list of candidates.
         */
    Update_Cand_Lst_During_Sched_Cyc (cand);

        /* Inform Micro-Scheduler to update its internal state.
         */
    _ops_in_cur_cyc.push_back(op) ;

    BOOL retcode = CGGRP_Issue_OP(op, TRUE);
    Is_True (retcode, ("fail to issue op"));

    return OP_xfer (op);
}

    /* ===============================================================
     *
     *          preprocess before and postprocess after scheduling
     *
     * ==============================================================
     */
void
SCHEDULER::Verify (void) {

    OP* op;

    FOR_ALL_BB_OPs(_target_bb, op) {
        Is_True(OP_Scheduled(op),
                ("Not all ops scheduled: %d 0x%x\tin BB: %d",
                OP_map_idx(op), op, BB_id(OP_bb(op))));
    }

}

void
SCHEDULER::Identify_Actual_Argument_Defs (BB* bb) {

    if (!BB_call (bb)) return ;

    OP * call_op = BB_xfer_op(bb) ;
    Is_True (OP_call(call_op), ("OP is not a call!"));

    OP * op ;
    FOR_ALL_BB_OPs(bb, op) {
        OP_ANNOT_Set_OP_Def_Actual_Para (op);
    }  

    OP_ANNOT_Reset_OP_Def_Actual_Para (call_op);


    FOR_ALL_BB_OPs_REV (bb,op) {
        if (OP_store(op) || OP_prefetch(op)) {  
            OP_ANNOT_Reset_OP_Def_Actual_Para (op);
        }

        BOOL safe_move_across_bb = FALSE ;

        if (safe_move_across_bb) {
            OP_ANNOT_Reset_OP_Def_Actual_Para (op);
        }

        for (ARC_LIST * list = OP_preds(op); list ; 
             list = ARC_LIST_rest(list)) { 

            ARC * arc = ARC_LIST_first (list) ;
            if (ARC_kind(arc) == CG_DEP_REGIN && 
                OP_bb (ARC_pred(arc)) == bb) {

                OP_ANNOT_Reset_OP_Def_Actual_Para (ARC_pred(arc));
            }
        }
    }
  
    OP_ANNOT_Reset_OP_Def_Actual_Para (call_op);

    BOOL find_first_def = FALSE ;
    FOR_ALL_BB_OPs (bb,op) {
        if (OP_ANNOT_OP_Def_Actual_Para (op)) find_first_def = TRUE ; 
        if (OP_load (op) && find_first_def ) { 
            OP_ANNOT_Set_OP_Def_Actual_Para (op) ; 
        };
    }

}


    /* ========================================================
     *
     *   Sched_Rgn_Preproc 
     * 
     *  Somthing that should be done prior to schedule REGION 
     *
     * ========================================================
     */
BOOL
SCHEDULER::Sched_Rgn_Preproc (void) {


    if (_cflow_mgr.Critical_Edge_Present(_region)) { 

        /* currently our global code motion algirithm is not 
         * applicable to a REGION with critical edge presence.
         */
        return FALSE; 
    }

    if (!_cflow_mgr.BB_Node_Num()) { 

        /* this region contains only inner region (but no BB),
         * it is obviously nothing to be scheduled.
         */

        return FALSE; 
    }
        
    /* initialize BB,inner-REGION and OP's annotations
     */
    sched_annot.Init (_region);

    /* Build dep DAG. DAG construction is time-consuming, 
     * we keep and timer to acquire the time spent in 
     * this work.
     */
    Start_Timer (T_Ipfec_DAG_CU);
    _dag_constructor.Build_DAG();
    Stop_Timer (T_Ipfec_DAG_CU);
    if (SCHED_TF_DUMP_DAG) { Dump_DAG(); fprintf(TFile, "\n"); }

    CGGRP_Init (_region);
    _heur_mgr.Initialize (_region,&_cflow_mgr);

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_region->Regional_Cfg());
         iter != 0; ++iter) {

        if ((*iter)->Is_Region()) continue ;
        BB * bb = (*iter)->BB_Node();
        if (BB_entry(bb) || BB_exit(bb)) continue ;

        Identify_Actual_Argument_Defs (bb);
    }

    Identify_Cannot_Spec_OPs (_region);

    return TRUE;
}

    /* ========================================================
     *
     * Sched_Rgn_Postproc 
     * 
     * Something should be done after schedule REGION.
     *
     * =======================================================
     */
void 
SCHEDULER::Sched_Rgn_Postproc (void) {
    Build_Region_Summary (_region, &_cflow_mgr) ;
}


/* ===========================================================================
 *
 *    Handle GP-problem  
 * 
 *  GP-problem in brief : 
 *  before sched code sequence : (1) mov gp = xxxx (2) add TN2=TN3,GTN4
 *  after  global scheduing : OP1 and OP2 are transposed: add TN2=TN3, GTN4, 
 *                                                         mov gp = xxxx
 *  GRA, assume gp is valid, spill GTN4 and prepend the code to BB, hence
 *  got the sequence :
 *   (1) add TNgtn4=gp, some-const, (2) ld GTN4=[TNgtn4] (3) mov gp=xxx
 *  
 *  it is obviously an wrong sequence. 
 *  
 *  Our workaround is just for the time bing, it is better to fix this bug
 *  in GRA phase. 
 *
 *  TODO : fix the GP-problem in GRA phase rather than provide
 *         a workaround in code motion phase.
 * 
 * ============================================================
 */

void
SCHEDULER::Preprocess_GP_def_op (void) {
    
    _gp_def_op = NULL ; 

    if (!BB_length(_target_bb) || 
        !OP_def_GP (BB_first_op(_target_bb))) {
        return ; 
    }

    _gp_def_op = BB_first_op(_target_bb);
}

void
SCHEDULER::Postprocess_GP_def_op (void) {

    if (_gp_def_op && BB_first_op(_target_bb) != _gp_def_op) {

        BB_Move_Op_Before (_target_bb, 
                           BB_first_op(_target_bb),
                           _target_bb, _gp_def_op) ;
        Reset_BB_scheduled (_target_bb); 
    }

    _gp_def_op = NULL;
}

inline BOOL
SCHEDULER::OP_def_GP (OP* op) {
    if (OP_results (op) != 1) return FALSE;

    return OP_result(op,0) == GP_TN ; 
}

inline void
SCHEDULER::Bug_Workaround_Before_Schedule_BB (void) {
    if (_prepass) {
        Preprocess_GP_def_op ();
    }
}

inline void
SCHEDULER::Bug_Workaround_After_Schedule_BB (void) {

    if (_prepass) {
        Postprocess_GP_def_op ();
    }
}

void
SCHEDULER::Glos_Sched_BB_Preproc (void) {

    if (_global) {
        Determine_P_Ready_is_Profitable_or_not () ;
    }

#ifdef TARG_IA64
    _multiway_br_span_bbs.clear ();
#endif

    Init_Sched_Status () ;

    /* init the micro-sched component */
    CGGRP_Begin_BB (_target_bb);

    /* find out all BBs (including <_target_bb> itself) 
     * that are potentialy donate candidates to be 
     * moved into <_target_bb>
     */
    _src_bb_mgr.Find_Src_BBs (_region, _target_bb, &_cflow_mgr, 
                                  TRUE /*prepass phase*/);

    /* adjust heuristic stuff : 
     *
     *      heuristic stuff should be initialized properly and 
     *      up-to-date before we begin to find candidates,since 
     *      we prevent some "bad" candidate from being selected
     *      into candidate-list at this time.
     */
    _heur_mgr.Adjust_Heur_Stuff_When_BB_Changed (_target_bb,_src_bb_mgr);

    /* now find all candidates from src BBs */
    Find_All_Candidates ();
}


    /* ==========================================================
     *
     * Glos_Sched_BB_Postproc 
     * 
     * Perform something after a basic block has been scheduled
     *
     * ==========================================================
     */
void
SCHEDULER::Glos_Sched_BB_Postproc (void) {

        /* Inform micro-scheduler to change its internal state.
         */
    CGGRP_End_BB(); 
    Set_BB_scheduled (_target_bb);
    Isolate_BB_From_Sched_Scope (_target_bb); 

        /* calc total number of cycles required by this bb 
         */
    BB_cycle(_target_bb) = 
    BB_length(_target_bb) ? _cur_cyc : 0 ;

    Verify();
}

    /* ===========================================================
     *
     *  Glos_Should_Sched_This_BB 
     *
     *  Check to see whether we need schedule a specific block.
     *
     * ===========================================================
     */
BOOL
SCHEDULER::Glos_Should_Sched_This_BB (BB* b) {

    if (BB_Is_Isolated_From_Sched (b)) {
        return FALSE;
    }

    INT32 len = BB_length(b);
    if (len > 1)  { return TRUE;  }
    if (len == 0) { return FALSE; }

        /* 1. provide a latitude for CFLOW to delete blocks that 
         *    contains only one non-call cntl-xfer op.
         *
         * 2. provide a chance for multiway-branch-phase.
         */ 
    if (len == 1 && BB_branch_op (b)) {
        if (BB_edge_splitting (b)) {
            return FALSE;
        }

#ifdef TARG_IA64
        if (BB_SET_MemberP (_multiway_br_candidates, b)) {
            return FALSE;
        }
#endif
    }

    if (BB_entry(b) || BB_exit(b)) {
        return FALSE;
    }

    return TRUE;
}


/* ==================================================================
 *
 *      Schedule_Cycle
 *
 * ==================================================================
 */
void
SCHEDULER::Schedule_Cycle (void) {

        /* make sure all candidates avaiable is untried, so that they
         * have a change to be issued in next cycle.
         */
    _cand_mgr.Clear_All_Cands_Tried_Mark ();

    BOOL commit_br = FALSE;
    while (!_cand_mgr.Cand_Lst_Is_Empty ()) {
    
            /* if cycle full or no untried candidates 
             * are available, advance to next cycle.
             */
        if (CGGRP_Cycle_Full() || 
            _cand_mgr.All_Cands_Have_Been_Tried ()) {
            return ;
        }

            /* Select an untried instruction with highest priority.
             */
        E_Time_Constraint etime_constraint;

        etime_constraint.threshold  = _cur_cyc;
        etime_constraint.constraint = 
            _ops_in_cur_cyc.empty() ? AS_EARLY_AS_POSSIBLE : 
                                      NO_LATER; 

        CANDIDATE* cand = 
            _heur_mgr.Select_Best_Candidate (
                         *_cand_mgr.M_Ready_Cand_List (), 
                         *_cand_mgr.P_Ready_Cand_List (), 
                         _target_bb,
                         &etime_constraint);

        if (!cand) { return ; }

        if (!CGGRP_Issue_OP(cand->Op())) {

                /* This candidate cannot be issued in current cycle
                 * due to structrual hazard.
                 */
            if (_heur_mgr.Trace_Cand_Sel_Enabled ()) {
                _heur_mgr.Trace_Cand_Sel_Process (
                   "\tDiscard best candidate: structual hazard!!\n");
            }

            _cand_mgr.Get_Cand_List (cand)->Set_Cand_Has_Been_Tried (cand);
            continue;
        }
      
            /* check whether candidate kills some liveout definitions.
             * 
             * e.g 
             *    For a diamon-shaped flow, its layout is depicted 
             *    below:
             *
             *    BB1 has two successors, BB2 and BB3, they are also BB4's
             *    2 preds.
             *    
             *    Both OP2(of BB2) and OP3 (of BB3) define same TN, say 
             *    TN234. at the beginging, both OP2 and OP3 qualified 
             *    as candidate and they compete a slot in BB1. after 
             *    speculating OP3 to BB1, OP2 is no longer a candidate
             *    to BB1, but it still resides in candidate list.
             */
        BB *cand_home_bb = OP_bb (cand->Op()) ; 

        SRC_BB_INFO* src_info = _src_bb_mgr.Get_Src_Info (cand_home_bb);
        if (cand_home_bb != _target_bb &&
            _dflow_mgr.Upward_Sched_Kill_LiveOut_Defs (
                    cand, src_info, &_cflow_mgr)) {
                    
            if ( IPFEC_Glos_Enable_Renaming &&
                !IPFEC_Query_Skiplist (glos_rename_skip_bb, BB_id(cand_home_bb), Current_PU_Count()) &&
                !IPFEC_Query_Skiplist (glos_rename_skip_op, OP_map_idx(cand->Op()), BB_id(cand_home_bb)) &&
                _heur_mgr.Renaming_Is_Profitable(cand)) {

                Renaming(cand);
            } else {

                _cand_mgr.Get_Cand_List (cand)-> Erase_Cand (cand);
                if (_heur_mgr.Trace_Cand_Sel_Enabled ()) {
                    _heur_mgr.Trace_Cand_Sel_Process(
                        "\tDiscard best candidate since it kill some live out TN");
                }

                continue ;
            }
        }
    
#ifdef Is_True_On 

        if (!_ops_in_cur_cyc.empty ()) {
            Is_True (_cur_cyc == etime_constraint.threshold , 
                     ("best candidate [OP%3d][BB%3d] should be issued exactly"
                      " at cycle %d",
                      OP_map_idx(cand->Op()),
                      BB_id(OP_bb(cand->Op())),
                      _cur_cyc));
        } else {
            Is_True (_cur_cyc <= etime_constraint.threshold,
                     ("best candidate [OP%3d][BB%3d] should be issued" 
                      " before cycle %d",
                      OP_map_idx(cand->Op()),
                      BB_id(OP_bb(cand->Op())),
                      _cur_cyc));
        }
#endif /* Is_True_On */

        _cur_cyc = etime_constraint.threshold ; 

            /* now commit schedule 
             */
        OP* op = cand->Op ();
        if (OP_br(op)) { commit_br = TRUE; }

        Commit_Schedule (*cand);
        
        if (OP_call(op) && 
            !IPFEC_Glos_Motion_Across_Calls) {
            return;
        }
    }

        /* find a chance of multi-way-branch */
    if (commit_br && _prepass) {

        BB* b; 
        BB_VECTOR bbs(&_mem_pool);

        for (b = BB_Fall_Thru_Successor (_target_bb); b ; 
             b = BB_Fall_Thru_Successor (b)) {
              
            if (!BB_Unique_Predecessor (b)) {
                b = NULL;  break;
            } 

                /* for the case of empty block */
            if (!BB_length(b)) { continue ; }

                /* for the block which has only one branch op */
            if (BB_length(b) == 1 && BB_branch_op (b)) {
                bbs.push_back (b);
                if (BB_edge_splitting(b)) { continue; } else break ;
            }
                
            b = NULL; break;
        }

#ifdef TARG_IA64
        if (b && BB_length(b) == 1 && BB_branch_op(b)) {
            if (::Home_Region (b) == _region) {
                OP* br = BB_branch_op(b);
                if (br && CGGRP_Issue_OP (br, FALSE)) {

                    for (BB_VECTOR_ITER iter = bbs.begin () ;
                         iter != bbs.end () ; iter++) {

                        _multiway_br_candidates = BB_SET_Union1D 
                            (_multiway_br_candidates, *iter, &_mem_pool);

                        _multiway_br_span_bbs.push_back (*iter);
                    }
                }
            }
        }
#endif /* TARG_IA64 */
    }
}


    /* ==========================================================
     *
     * No_New_Cycle 
     * 
     * Check to see whether we need create extra cycle for
     * target block(_target_bb).
     *
     * ==========================================================
     */
BOOL
SCHEDULER :: No_New_Cycle (void) {

    BOOL no_new_cyc = FALSE ;

    if (BB_length(_target_bb) == 0 || 
        OP_Scheduled(BB_last_op(_target_bb))) {
        no_new_cyc = TRUE;
    }

#ifdef Is_True_On
    if (!no_new_cyc) {
        Is_True (!_cand_mgr.Cand_Lst_Is_Empty (), 
                 ("Candidate list is Empty!"));
    }
#endif 
    
    return no_new_cyc;

}

    /* ====================================================
     *
     * Need_Resched_To_Obtain_Better_Performance 
     *
     * determine whether <_target_bb> need rescheduling 
     * to get better performance.
     *
     * ====================================================
     */
BOOL
SCHEDULER::Need_Resched_To_Obtain_Better_Performance (void) {

        /* do not reschedule too many times 
         */ 
    if (_sched_times >= (MAX_SCHED_TIMES - 1)) { return FALSE ; } 

    if (_src_bb_mgr.Src_BBs ()->size () <= 1) {
        return FALSE ;
    }

    if (_cand_mgr.Cand_Lst_Is_Empty ()) {
        return FALSE;
    }

    if (_upward_motion_num <= 0) {
        return FALSE;
    }

    return TRUE;
}


//===============================================================
// Should_Skip
//
// Given three parameters : before/after/equal, decide whether the input
// parameter 'id' is in the skip range.
//===============================================================
BOOL Should_Skip( int before, int after, int equal, int id )
{
  if( equal >= 0 )
    if( id == equal )
      return TRUE;

  if( (before == -1) && (after == -1) ) 
    return FALSE;

  // if only defined after
  if( before == -1 )
    if( id > after )
      return TRUE;
    else 
      return FALSE; 

  // if only defined before
  if( after == -1 )
    if( id < before )
      return TRUE;
    else 
      return FALSE; 

  // if defined both before and after, there are two situations:
  // (1) there are a inter-section by before and after
  // (2) no inter-section
  if( before <= after ){
    // no inter-section
    if( (id < before) || (id > after) )
      return TRUE;
    else
      return FALSE;
  }else{
    if( (id < before) && (id > after) )
      return TRUE;
    else
      return FALSE;
  }
}

static BOOL Local_Sched_PU_Should_Skip(int pu_id)
{
  BOOL skip = Should_Skip( CG_local_sched_pu_skip_before, 
                           CG_local_sched_pu_skip_after,
                           CG_local_sched_pu_skip_equal,
                           pu_id );
  return skip;
}

static BOOL Local_Sched_OP_Should_Skip()
{
  if(CG_local_sched_op_skip_after==-1)
    return FALSE;

   return _Local_Insn_Op_id > CG_local_sched_op_skip_after? TRUE:FALSE;
}

static void Local_Sched_Opid_Incr()
{
  _Local_Insn_Op_id++;
}

static BOOL BB_Should_Skip(BB* bb) 
{
  int id = BB_id(bb);
  BOOL skip = Should_Skip( CG_local_sched_bb_skip_before, 
                           CG_local_sched_bb_skip_after,
                           CG_local_sched_bb_skip_equal,
                           id );
  return skip;
}

/* ==================================================================
 *
 *      Schedule_Region
 * 
 *  schedule each BB in <_region> except PU's entry- and exit-BB
 * 
 *  NOTE:  Schedule_Region should only be called before register 
 *         allocation
 * ==================================================================
 */

void
SCHEDULER::Schedule_Region (void) {

    if (!Sched_Rgn_Preproc ()) return;

    TOPDOWN_SCHED_SEQ seq (_region, &_mem_pool);
    for (_target_bb = seq.First (); 
         _target_bb ; 
         _target_bb = seq.Next()) {

        if (!Glos_Should_Sched_This_BB (_target_bb)) {
            Isolate_BB_From_Sched_Scope (_target_bb); 
            continue ;
        }

        Bug_Workaround_Before_Schedule_BB ();
        Glos_Sched_BB_Preproc ();

        BOOL no_new_cycle = FALSE;
        OP * xfer_op = BB_xfer_op(_target_bb);

        do {
            do {
                Schedule_Cycle ();
                Cycle_Advance ();
            } while (!No_New_Cycle ());

            Glos_Sched_BB_Postproc ();

            if (Need_Resched_To_Obtain_Better_Performance ()) {
                Adjust_Status_For_Resched ();
                /* Reinit micro-sched component */
                CGGRP_Begin_BB (_target_bb);
            } else {
                break ;
            }
        } while (TRUE);

        Bug_Workaround_After_Schedule_BB ();

    } /* end of "for (_target_bb = ... = seq.Next())" */

    Sched_Rgn_Postproc ();
}


#if defined(TARG_SL) || defined(TARG_SL2)
/* =========================================================================
 * this function is a driver to split original implementation of schedule_bb
 * into three parts: 
 *      initialization 
 *      schedule bb
 *      finialize 
 *==========================================================================
 */
void 
SCHEDULER::Schedule_BB_Driver() 
{
  if (BB_length(_target_bb) == 0)  { return; }

  _src_bb_mgr.Find_Src_BBs (_region, _target_bb, 
                              &_cflow_mgr, _prepass);

//  build dependence DAG 
  _dag_constructor.Build_DAG ();



  if (SCHED_TF_DUMP_DAG) {
    Dump_DAG(); fprintf(TFile, "\n"); 
  }

  CGGRP_Init (_target_bb);
  _heur_mgr.Initialize (_target_bb,&_cflow_mgr);
    
  Schedule_BB(); 

  if (BB_length(_target_bb)) CGGRP_End_BB();

#ifndef TARG_SL 
/* don't do verification for debugging purpose. because we don't want to schedule all instructions.*/
  Verify(); 
#endif 

  if (_prepass) Postprocess_GP_def_op ();

  CG_DEP_Delete_Graph (_target_bb) ;
  
  return;
}
#endif 
/* =======================================================================
 * 
 *  Schedule_BB
 *  
 *  perform scheduling within BB scope. 
 *
 *  NOTE: o. local scheduling does not imply no control or data speculation.
 * 
 *           before register allocation, these two kind of speculation are 
 *           omnipresent unless speculation is turned off explicitly through
 *           (compiler's) command option. and,
 * 
 *           after reg-alloc any speculation is disallowed.
 *   
 * 
 * =======================================================================
 */
#if defined (TARG_SL2)
void
SCHEDULER::Schedule_BB (BOOL reschd) {
#else 
void
SCHEDULER::Schedule_BB (void) {
#endif 

  _heur_mgr.Adjust_Heur_Stuff_When_BB_Changed (_target_bb,_src_bb_mgr);

  /* identify OPs which cannot be speculated */
  if (_prepass) {
    Identify_Cannot_Spec_OPs (_target_bb);
    Preprocess_GP_def_op () ;
  }
#if defined(TARG_SL)
  moved_op_num = 0; 
#endif 
#if defined(TARG_SL)
  if ((!_prepass) && (!_global) && 
      CG_Gen_16bit && ((CG_localsch_pre_size > 1) || ((CG_localsch_pre_size == 1) && !BB_loop_head_bb(_target_bb)))) 
  {
    _heur_mgr.Set_Sched_For_CodeSize_Spec();
    _heur_mgr.Reset_prev_unpaired_16bit();   
  }
#endif

  /* now find all candidates from src BBs */
  Find_All_Candidates ();

  Init_Sched_Status () ;
  
  CGGRP_Begin_BB (_target_bb);

  BOOL last_op_sched = FALSE ;

  CAND_LIST *m_ready_cand = _cand_mgr.M_Ready_Cand_List ();
  CAND_LIST *p_ready_cand = _cand_mgr.P_Ready_Cand_List ();

  while (TRUE) {
    while (!m_ready_cand->Cand_Lst_Is_Empty ()) {

      if(CGGRP_Cycle_Full() || m_ready_cand->All_Cands_Have_Been_Tried() )
        Cycle_Advance();

      E_Time_Constraint etime_constraint;
      etime_constraint.threshold  = _cur_cyc;
      etime_constraint.constraint = _ops_in_cur_cyc.empty() ? 
                                              AS_EARLY_AS_POSSIBLE : 
                                              NO_LATER; 
      CANDIDATE* cand = _heur_mgr.Select_Best_Candidate (
                                            *m_ready_cand, 
                                            *p_ready_cand, 
                                            _target_bb, 
                                            &etime_constraint);

      if (!cand) continue ;
      
      if (!CGGRP_Issue_OP(cand->Op())) {
        _cand_mgr.Get_Cand_List(cand)->Set_Cand_Has_Been_Tried (cand); 
        
        if (_heur_mgr.Trace_Cand_Sel_Enabled ()) {
          _heur_mgr.Trace_Cand_Sel_Process (
            "\tDiscard best candidate due to structual hazard\n");
        }
        
        continue;

      }

#ifdef Is_True_On

      if (!_ops_in_cur_cyc.empty ()) {
        Is_True (_cur_cyc == etime_constraint.threshold , 
                         ("best candidate [OP%3d][BB%3d] should be issued exactly"
                          " at cycle %d",
                          OP_map_idx(cand->Op()),
                          BB_id(OP_bb(cand->Op())),
                          _cur_cyc));
      } else {
        Is_True (_cur_cyc <= etime_constraint.threshold,
                ("best candidate [OP%3d][BB%3d] should be issued" 
                 " before cycle %d",
                 OP_map_idx(cand->Op()),
                 BB_id(OP_bb(cand->Op())),
                 _cur_cyc));
      }
#endif

      _cur_cyc = etime_constraint.threshold ; 

      Commit_Schedule(*cand);

#if defined(TARG_SL)	
      if(_heur_mgr.Sched_For_Codesize_Spec()) {
        if (!_heur_mgr.Prev_unpaired_16bit() && OP_16bit_op(cand->Op())) 
          _heur_mgr.Set_prev_unpaired_16bit();
        else 
          _heur_mgr.Reset_prev_unpaired_16bit();

        if (SCHED_TF_DUMP_CS_PROCESS) { 
          fprintf(TFile, "commit sched: (%d)\t", _cur_cyc);
	  Print_OP_No_SrcLine(cand->Op());	 
        }
      }
#endif		 

      Local_Sched_Opid_Incr();
      if(Local_Sched_OP_Should_Skip()) {
        last_op_sched=TRUE;
        break;
      } 
           
      last_op_sched |= OP_Scheduled (BB_last_op(_target_bb)) ;

#if defined(TARG_SL) || defined(TARG_SL2)
      if(CG_bb_sched_op_num_max) { 
        if(moved_op_num == CG_bb_sched_op_num_max) { 
          last_op_sched = TRUE; 
          fprintf(TFile, "last moved op is \n"); 
	  Print_OP_No_SrcLine(cand->Op());
          break; 
        } 
      }

      if( _heur_mgr.Round_Robin_Cand_Sel() && cand->Match_Model() )
        _heur_mgr.Update_Cand_Sel_Model_Status(); 
#endif 
    }

    if (last_op_sched) break ;
 
    Cycle_Advance () ;
  }

    /* calculate BB's execution cycle */
  BB_cycle(_target_bb) = BB_length(_target_bb) ? _cur_cyc + 1 : 0;

    /* Inform machine model the beginning of a new basic block.  */

  return; 
}


    /* =================================================================
     * 
     *  Renaming
     *  
     *  Renaming <cand> to schedule it up
     *
     * =================================================================
     */
void
SCHEDULER::Renaming (CANDIDATE* cand) 
{
    OP* op = cand->Op();
    BB* home_bb = OP_bb(op);
    OP* copy_op = NULL;
    TN *orig_tn = OP_result(op, 0);
    TN* new_tn = Dup_TN(orig_tn); 
        
    FmtAssert(OP_results(op) == 1, 
        ("We don't do renaming for a multi-assignment OP: [OP:%3d][BB:%3d]\n",
            OP_map_idx(op), BB_id(OP_bb(op)))); 
    
    /* rename <orig_tn> to <new_tn> used in the OPs after <op>
     */
    for (OP* tmp_op = OP_next(op); tmp_op; tmp_op = OP_next(tmp_op)) {
        for (INT i = 0; i < OP_opnds(tmp_op); i++) {
            if (orig_tn == OP_opnd(tmp_op, i))
                Set_OP_opnd(tmp_op, i, new_tn);
        }
        for (INT i = 0; i < OP_results(tmp_op); i++) {
            if (orig_tn == OP_result(tmp_op, i))
                Set_OP_result(tmp_op, i, new_tn);
        }
    }
    
    /* generate a copy op and append it to <home_bb> if necessary
     */
    if (_dflow_mgr.Are_Defs_Live_Out(op, home_bb)) {
#ifdef TARG_SL 
        copy_op = Mk_OP((TN_is_float(orig_tn) ? TOP_mov_s : TOP_or), orig_tn, new_tn, Zero_TN);
#else 
        copy_op = Mk_OP(CGTARG_Copy_Op(TN_size(orig_tn), TN_is_float(orig_tn)),
                         orig_tn, OP_opnd(op, OP_PREDICATE_OPND), new_tn); 
#endif                          
        BB_Insert_Op_After(home_bb, op, copy_op); 
    }

    /* maintain dependence arcs
     */    
    Maintain_Dep_Arcs_After_Renaming(op, copy_op);

    /* maintain annotation and heuristic data
     */
    if(copy_op) {
        SCHED_BB_ANNOT* bb_annot = sched_annot.Get_BB_Annot (home_bb);
        bb_annot->Init_New_OP_Annot (copy_op);
        _heur_mgr.Compute_Heur_Data_For_Inserted_OP (copy_op);
        Set_OP_renamed(copy_op);
    }

    /* finally, rename candidate op and annotate it as renamed
     */
    Set_OP_result(op, 0, new_tn); 
    Set_OP_renamed(op);
         
}

    /* =================================================================
     * 
     *  Maintain_Dep_Arcs_After_Renaming
     *  
     *  Re-compute the dependence arcs for the candidate op <renamed_op> and <copy_op>
     *
     * =================================================================
     */
void
SCHEDULER::Maintain_Dep_Arcs_After_Renaming (OP* renamed_op, OP* copy_op) 
{
    for (ARC_LIST* arcs = OP_succs(renamed_op); arcs; ) {
        ARC *arc = ARC_LIST_first(arcs);
        arcs = ARC_LIST_rest(arcs);
        OP* succ = ARC_succ (arc);
        switch (ARC_kind(arc)) {
            case CG_DEP_REGOUT:
                if (copy_op) {
                    if (OP_bb(renamed_op) != OP_bb(succ)) {
                        new_arc_with_latency(ARC_kind(arc), copy_op, succ, ARC_latency(arc), 
                            ARC_omega(arc), ARC_opnd(arc), ARC_is_definite(arc));
                        CG_DEP_Detach_Arc(arc);
                    } else {
                        new_arc(CG_DEP_REGIN,  succ, copy_op, 0, 0, TRUE); 
                    }
                }
                break;
            case CG_DEP_REGIN:
                if (copy_op && OP_bb(renamed_op) != OP_bb(succ)) {
                    new_arc_with_latency(CG_DEP_REGIN, copy_op, succ, ARC_latency(arc),
                        ARC_omega(arc), ARC_opnd(arc), ARC_is_definite(arc));
                    CG_DEP_Detach_Arc(arc);
                }
                break;
            case CG_DEP_PREBR:
            case CG_DEP_POSTBR:
#ifdef TARG_IA64
            case CG_DEP_PRECHK:
            case CG_DEP_POSTCHK:
#endif
            case CG_DEP_SCC:
            case CG_DEP_MISC:
                if (copy_op) {
                    new_arc_with_latency(ARC_kind(arc), copy_op, succ, ARC_latency(arc), 
                        ARC_omega(arc), ARC_opnd(arc), ARC_is_definite(arc));
                }
                break;
            default:
                break;
        }
    }

    for (ARC_LIST* arcs = OP_preds(renamed_op); arcs; ) {
        ARC *arc = ARC_LIST_first(arcs);
        arcs = ARC_LIST_rest(arcs);
        OP* pred = ARC_pred (arc);
        switch (ARC_kind(arc)) {
            case CG_DEP_REGOUT:
                if (copy_op) {
                    new_arc_with_latency(ARC_kind(arc), pred, copy_op, ARC_latency(arc), 
                        ARC_omega(arc), ARC_opnd(arc), ARC_is_definite(arc));
                    CG_DEP_Detach_Arc(arc);
                }
                break;
            default:
                break;
        }
    }
    
    if (copy_op) {
        new_arc(CG_DEP_REGIN, renamed_op, copy_op, 0, 0, TRUE);
    }      
}

/* ====================================================================
 *
 *  Clean_Up 
 *  
 *  "clean up" prior to code motion, NOTE: DO NOT apply static qualifer 
 *   to this routine, since it is also issued by micro-scheduling. 
 * 
 * ====================================================================
 */
void
SCHEDULER::Clean_Up (BB* bb) {
    OP *op, *next_op;
  
    Reset_BB_scheduled (bb);
    for (op = BB_first_op(bb); op; op = next_op) {

        next_op = OP_next(op);

        if (OP_noop(op)) {
            BB_Remove_Op(bb, op);
        } else {
            Reset_OP_Scheduled (op);
#ifdef TARG_IA64
            Reset_OP_start_bundle(op);
            Reset_OP_end_group(op);
            Reset_OP_bundled(op);
            Reset_OP_m_unit(op);
#endif
        }
    }
}
    /* =================================================================
     * =================================================================
     * 
     *          Constructor and Destructor 
     *
     * =================================================================
     * =================================================================
     */
#ifdef TARG_IA64
SCHEDULER::SCHEDULER (BB* bb, BOOL prepass ,PRDB_GEN *prdb) :
        _dag_constructor (bb,prdb),
        _cand_mgr(&_mem_pool),
        _heur_mgr(&_mem_pool),
        _src_bb_mgr(&_mem_pool),
        _ops_in_cur_cyc(OP_ALLOC(&_mem_pool)),
        _multiway_br_span_bbs(&_mem_pool),
        _global(FALSE), _prepass(prepass)
#else
SCHEDULER::SCHEDULER (BB* bb, BOOL prepass) :
        _dag_constructor (bb),
        _cand_mgr(&_mem_pool),
        _heur_mgr(&_mem_pool),
        _src_bb_mgr(&_mem_pool),
        _ops_in_cur_cyc(OP_ALLOC(&_mem_pool)),
        _global(FALSE), _prepass(prepass)
#endif
{
    _region  = NULL;
    _target_bb = bb, 

    Get_Sched_Opts (prepass);
    Clean_Up(bb);

#ifdef TARG_IA64
        /* local scheduling actually need not use this variable */
    _multiway_br_candidates = NULL;
#endif

    _cflow_mgr.Init (_target_bb);
    sched_annot.Init (_target_bb);

}

#ifdef TARG_IA64
SCHEDULER::SCHEDULER (struct tagRGN_INFO * rgn_info, BOOL prepass, 
                PRDB_GEN * prdb) :
        _dag_constructor (rgn_info->rgn, prdb, 
                          INCLUDE_ASSIGNED_REG_DEPS,
                          INCLUDE_MEMREAD_ARCS, 
                          NO_MEMIN_ARCS,
                          INCLUDE_CONTROL_ARCS),
        _cand_mgr(&_mem_pool),
        _src_bb_mgr(&_mem_pool),
        _ops_in_cur_cyc(OP_ALLOC(&_mem_pool)),
        _heur_mgr(&_mem_pool),
        _multiway_br_span_bbs(&_mem_pool),
        _global (TRUE), _prepass(TRUE)
#else
SCHEDULER::SCHEDULER (struct tagRGN_INFO * rgn_info, BOOL prepass) :
        _dag_constructor (rgn_info->rgn, 
                          INCLUDE_ASSIGNED_REG_DEPS,
                          INCLUDE_MEMREAD_ARCS, 
                          NO_MEMIN_ARCS,
                          INCLUDE_CONTROL_ARCS),
        _cand_mgr(&_mem_pool),
        _src_bb_mgr(&_mem_pool),
        _ops_in_cur_cyc(OP_ALLOC(&_mem_pool)),
        _heur_mgr(&_mem_pool),
        _global (TRUE), _prepass(TRUE)
#endif
{
    _region = rgn_info->rgn ;
    _target_bb = NULL ;

    Get_Sched_Opts (prepass);
    _cflow_mgr.Init (_region) ;
    sched_annot.Init (_region);

    for (TOPOLOGICAL_REGIONAL_CFG_ITER cfg_iter(_region->Regional_Cfg());
         cfg_iter != 0; ++cfg_iter) {

        if ((*cfg_iter)->Is_Region()) {
            continue;
        } else {
            Clean_Up((*cfg_iter)->BB_Node());
        }
    }
  
#ifdef TARG_IA64
        /* miscellaneous init */
    _multiway_br_candidates = BB_SET_Create_Empty (PU_BB_Count, &_mem_pool);
#endif

}

SCHEDULER::~SCHEDULER () {
    _heur_mgr.Finialize ();
    CGGRP_Fini ();
}


/* ======================================================================
 * ======================================================================
 * 
 *
 *  Global_Insn_Sched : global-instruction-scheduling driver which is 
 *                      called ONLY prior to register allocation if 
 *                      and only if both the following two conditions
 *                      are satisfied. 
 *                       
 *                      o. IPFEC_Enable_Prepass_GLOS != 0 , and 
 *                      o. CG_opt_level > 1
 * 
 *  Local_Insn_Sched  : local-instruction-scheduling driver. 
 *                      which is called before reg-alloc if and only if 
 *                       
 *                      (!IPFEC_Enable_Prepass_GLOS || CG_opt_level <= 1)
 *                      && IPFEC_Enable_Prepass_LOCS 
 *                      
 *                      and it is called after reg-alloc iff
 * 
 *                      IPFEC_Enable_Postpass_LOCS && 
 *                      IPFEC_sched_care_machine == Sched_care_bundle 
 *                      
 * ======================================================================
 * ======================================================================
 */

void
Global_Insn_Sched_Preproc (
    REGION_TREE * rgn_tree, 
    INT& how_many_rgn_need_sched) {

    how_many_rgn_need_sched = 0 ;

    Init_Split_PU_Entry_Or_Exit_BB ();

    Acquire_Region_Info (rgn_tree);

    for (INNERMOST_REGION_FIRST_ITER iter(rgn_tree);
         iter != 0 ; ++iter) {
       
        RGN_INFO* rgn_info = Get_Region_Info (*iter);

        #ifdef Is_True_On 
        if (rgn_info->skip_reason == SKIP_RGN_DEBUG) {
            DevWarn ("Skip schedule RGN:%d of PU:%d", 
                      rgn_info->rgn->Id (), Current_PU_Count ());
        }
        #endif /* Is_True_On  */

        if (rgn_info->skip_reason != SKIP_RGN_NONE) { continue ; }

            /* 1. perform edge splitting 
             */
        rgn_info->rgn->Edge_Splitting () ;

        if (RGN_CFLOW_MGR::Critical_Edge_Present(rgn_info->rgn)) {
            
                /* TODO : Adding ficticious block, and prevent any OPs from 
                 *        being moved into this block to make our global 
                 *        scheduling algorithm still appliable to regions 
                 *        with critical-edge presence.
                 */        
            rgn_info->skip_reason = SKIP_RGN_CRITICAL_EDGE;
            continue ;    
        }

        ++ how_many_rgn_need_sched ;

            /* 2. split (PU) entry-BB in <rgn_info->rgn> if any 
             */
        if (IPFEC_Glos_Split_Entry_BB) {
            Split_PU_Entry_BB (rgn_info->rgn);
        }

            /* 3. split (PU) exit-block in <rgn_info->rgn> if any 
             */
        if (IPFEC_Glos_Split_Exit_BB) {
            Split_PU_Exit_BB (rgn_info->rgn);
        }
    }


    GRA_LIVE_Init (NULL);
}

void
Global_Insn_Sched_Postproc (void) {

    /* 1. merge all splitted entry and exit bb
     */
    Merge_All_Splitted_Entry_and_Exit_BB ();

    
    /* 2. make liveness info up-to-date 
     */
    GRA_LIVE_Init (NULL);

    Free_Region_Info_Memory () ;
}

/* ======================================================================
 * ======================================================================
 * 
 *
 *  Global_Insn_Sched : global-instruction-scheduling driver which is 
 *                      called ONLY prior to register allocation if 
 *                      and only if both the following two conditions
 *                      are satisfied. 
 *                       
 *                      o. IPFEC_Enable_Prepass_GLOS != 0 , and 
 *                      o. CG_opt_level > 1
 * 
 *  Local_Insn_Sched  : local-instruction-scheduling driver. 
 *                      which is called before reg-alloc if and only if 
 *                       
 *                      (!IPFEC_Enable_Prepass_GLOS || CG_opt_level <= 1)
 *                      && IPFEC_Enable_Prepass_LOCS 
 *                      
 *                      and it is called after reg-alloc iff
 * 
 *                      IPFEC_Enable_Postpass_LOCS && 
 *                      IPFEC_sched_care_machine == Sched_care_bundle 
 *                      
 * ======================================================================
 * ======================================================================
 */

extern void SCHED_Dump_IR (BOOL prepass, 
                           BOOL bef_sched, 
                           BOOL gcm,FILE *f=stderr) ;



//#ifdef TARG_IA64
    /* ==============================================================
     * ==============================================================
     *
     *  Local_Insn_Sched 
     * 
     *  perform local code motion for each BB in current PU if 
     *  necessary.
     * 
     * ==============================================================
     * ==============================================================
     */
void
Local_Insn_Sched (BOOL prepass) 
{
    int pu_id = Current_PU_Count();
    if( Local_Sched_PU_Should_Skip(pu_id) ){
      printf(" .. current pu is skipped by LIS, %s\n",
             prepass ? "Prepasss" : "Postpass" );
      return;
    }

    Set_Error_Phase (_Cur_Phase_Name = _Local_Insn_Sched_Phase_Name);
#ifdef TARG_IA64
    Start_Timer(T_Ipfec_LOCS_CU);
#endif 
    Get_Sched_Opts (prepass);
    if (SCHED_TF_DUMP_IR) SCHED_Dump_IR (prepass, 
                                         TRUE,  /* bef code motion   */
                                         FALSE, /* local code motion */
                                         TFile) ;

    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    	
    	
        if (BB_length(bb) == 0) { continue; }

        /* local scheduling before register allocation */
        if (prepass) { 
#ifdef TARG_SL
            if (!BB_Should_Skip(bb) && !BB_scheduled(bb) && !BB_reg_alloc(bb) &&
                !BB_entry(bb) && !BB_exit(bb)) {
#else 
        	if (!BB_scheduled(bb) && !BB_reg_alloc(bb)) {
#endif 
                SCHEDULER local_scheduler(bb, prepass);
                local_scheduler.Schedule_BB_Driver();
       	}
        } else {
            /* local scheduling after register allocation */
#ifdef TARG_IA64
            if (BB_chk_split_head(bb)){
                bb = Handle_Chk_Split_Bunch(bb);
                continue;
            }
#endif 	    
#ifdef TARG_SL
	    if( ! BB_Should_Skip(bb) 
                && ( !BB_scheduled(bb) 
                     || ( !BB_scheduled(bb) && BB_scheduled_hbs(bb) ) 
                     || ( !prepass 
                          && CG_Gen_16bit 
                          && CG_localsch_pre_size > 0 
                          && !BB_SCHED_SIZE(bb) ) 
                     || BB_entry(bb) 
                     || BB_exit(bb)
                   ) 
              ) {
#else              
            if (!BB_scheduled(bb) || BB_scheduled_hbs(bb) || BB_entry(bb) || 
                BB_exit(bb)){
#endif                 	

                if (IPFEC_Query_Skiplist(locs_skip_bb, 
                                         BB_id(bb),
                                         Current_PU_Count ())) {

                    DevWarn ("Skip local schedule BB:%d of PU:%d", 
                              BB_id(bb), 
                              Current_PU_Count ());

                    SCHEDULER::Clean_Up(bb);
                    Handle_All_Hazards (bb);
                        
                } else {
                    SCHEDULER local_scheduler(bb, prepass);
                    local_scheduler.Schedule_BB_Driver();


                }
            }
        }
        Set_BB_scheduled(bb);

#ifdef TARG_SL
        if (!prepass && CG_Gen_16bit && (CG_localsch_pre_size > 0)) {
		Set_BB_sched_size(bb);
        }
#endif     	
        
    }

    if (SCHED_TF_DUMP_IR) SCHED_Dump_IR (prepass, 
                                         FALSE,  /* after code motion  */
                                         FALSE,  /* local code motion */
                                         TFile) ;

    // reset the order field of OPs
    for (BB *bp = REGION_First_BB; bp; bp = BB_next(bp)) {
        BB_Update_OP_Order(bp);
    }
#ifdef TARG_IA64
    Stop_Timer(T_Ipfec_LOCS_CU);
#endif 
}
//#endif

/* ================================================================
 *
 *  Perform_Global_Schedule 
 *
 * The driver of global scheduling upon region tree 
 *
 * ================================================================
 */
// Memory pool for LOOP_DESCR 
static MEM_POOL loop_descr_pool;
static MEM_POOL bb_cand_pool; 
static MEM_POOL op_sch_pool;
void
Perform_Global_Schedule (BOOL prepass) {
BB* cand_bb; 
BB* bb; 
VECTOR cand_bbs; 
  MEM_POOL_Initialize (&loop_descr_pool, "LOOP_DESCR_pool", FALSE);
  MEM_POOL_Push(&loop_descr_pool);
  MEM_POOL_Initialize (&bb_cand_pool, "BB_CAND_pool", FALSE);
  MEM_POOL_Push(&bb_cand_pool);
  MEM_POOL_Initialize (&op_sch_pool, "OPSCH_pool", FALSE);
  MEM_POOL_Push(&op_sch_pool);

  
Calculate_Dominators();
L_Save();
LOOP_DESCR *loop_list;
loop_list = LOOP_DESCR_Detect_Loops (&loop_descr_pool);
cand_bbs = VECTOR_Init(PU_BB_Count+2,  &bb_cand_pool);

for( bb = REGION_First_BB; bb!=NULL; bb = BB_next(bb))
{

   if(BB_entry(bb) || BB_exit(bb) || BB_scheduled(bb)) 
   	continue; 

//  Sort_BB_by_topological_order(bb); 

   BB_MAP bb_map; 
   bb_map = BB_MAP_Create();
   _cg_dep_op_info = BB_MAP_Create();
   BB_OP_MAP omap = BB_OP_MAP_Create(bb, &op_sch_pool);
   BB_MAP_Set(_cg_dep_op_info, bb, omap);

   
   Compute_OPSCH (bb,  _cg_dep_op_info, &op_sch_pool);


   for(cand_bb = REGION_First_BB; cand_bb != NULL; cand_bb=BB_next(cand_bb))

	if(bb != cand_bb && BB1_BB2_Cntl_Equiv(bb, cand_bb)) {
		if(VECTOR_Member_Element(cand_bbs, cand_bb)) continue; 
            	VECTOR_Add_Element(cand_bbs,  (void*) cand_bb);
	}				

/* if bb has been scheduled or bb is from software pipeline, ignore it as a candidate */ 
//     Select_Best_Candidate(bb, cand_bb); 

}
Free_Dominator_Info_Memory () ;
MEM_POOL_Pop (&loop_descr_pool);
MEM_POOL_Delete (&loop_descr_pool);
MEM_POOL_Pop (&bb_cand_pool);
MEM_POOL_Delete (&bb_cand_pool);
MEM_POOL_Pop (&op_sch_pool);
MEM_POOL_Delete (&op_sch_pool);
L_Free();
// Find_Candidate_BBs(); 
// Find_Candidate_Ops();
}
