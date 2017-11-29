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

 /* =========================================================================
  * =========================================================================
  * 
  * Module: sched_dflow.cxx
  * $Revision: 1.1.1.1 $
  * $Date: 2005/10/21 19:00:00 $
  * $Author: marcel $
  * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/sched_dflow.cxx,v $
  *
  * Description:
  * ============
  *
  *   This module implement data-flow-related functionality on behalf 
  *   of scheduler.
  *   ref sched_dflow.h for more details
  * 
  * =========================================================================
  * =========================================================================
  */
  
#include "tracing.h"
#include "errors.h"

#include "reg_live.h"
#include "gra_live.h"

#include "region.h"
#include "sched_util.h"
#include "sched_cand.h"
#include "sched_dflow.h"
#include "sched_cflow.h"
#include "sched_rgn_info.h"
#include "sched_path.h"


SCHED_DFLOW_MGR :: SCHED_DFLOW_MGR (void) {
}

SCHED_DFLOW_MGR :: ~SCHED_DFLOW_MGR (void) {} 

    

    /* ============================================================
     *
     *  ::Are_Defs_Live_In 
     *
     *  Check to see whether one of the results that <op> defines 
     *  live at the entry point of <bb>
     *
     * ============================================================
     */
BOOL
SCHED_DFLOW_MGR :: Are_Defs_Live_In (OP* op, BB* bb) {

    for (INT i = OP_results (op) - 1; i >= 0 ; i --) {
        if (GRA_LIVE_TN_Live_Into_BB (OP_result(op, i), bb)) {
            return TRUE; 
        }
    }

    return FALSE ;
}

BOOL
SCHED_DFLOW_MGR :: Are_Defs_Live_In (OP* op, REGION* r) {

    switch (r->Region_Type()) {
    case SEME :
    case LOOP :
        break;
    default:
        return TRUE;
    }

    NODE_VECTOR entries = r->Entries () ;
    if (entries.size () != 1) return TRUE;

    return Are_Defs_Live_In (op,entries[0]);
}



    /* =======================================================
     *
     * ::Are_Defs_Live_Out
     * 
     * Ref the header file for more details
     * 
     * =======================================================
     */
BOOL
SCHED_DFLOW_MGR::Are_Defs_Live_Out  (OP* op, BB* bb) {

    for (INT i = 0; i < OP_results(op); ++i) {

        TN *result_tn = OP_result(op,i);
        if (GTN_SET_Intersection_MemberP(BB_live_out(bb), 
            BB_defreach_out(bb), result_tn)) {
            return TRUE;
        }

        if (TN_is_dedicated (result_tn) &&
            REG_LIVE_Implicit_Use_Outof_BB (TN_register_class(result_tn), 
					   TN_register(result_tn), bb)) {
            return TRUE ;
        }
    }

    return FALSE;
}

BOOL
SCHED_DFLOW_MGR :: Are_Defs_Live_Out (OP* op, REGION*r) {
    FmtAssert (FALSE, ("Not yet implemented"));
    return TRUE;
}

    /* =======================================================
     *
     * SCHED_DFLOW_MGR::Are_Defs_Live_Out
     *
     * Ref the header file for more details
     * 
     * =======================================================
     */
BOOL
SCHED_DFLOW_MGR::Are_Defs_Live_Out  (OP* op, BB_VECTOR *bbv) {

    for (BB_VECTOR_ITER iter = bbv->begin () ; 
        iter != bbv->end () ; iter++) {

        if (Are_Defs_Live_Out (op, *iter)) {
            return TRUE ;
        }
    }
    
    return FALSE;
}

    /* =======================================================
     *
     * SCHED_DFLOW_MGR::Add_Defs_Live_In
     *
     * add the resultes of <op> live into <bb>'s entry
     * 
     * =======================================================
     */
void
SCHED_DFLOW_MGR::Add_Defs_Live_In (OP* op, BB* bb) {

    for (INT i = OP_results(op) - 1 ; i >= 0 ; --i) {

        TN *result_tn = OP_result(op,i);

        Set_TN_is_global_reg (result_tn) ;
        GTN_UNIVERSE_Add_TN (result_tn) ;

        GRA_LIVE_Add_Defreach_In_GTN (bb,result_tn);
        GRA_LIVE_Add_Live_In_GTN (bb,result_tn);
    }
}

    /* =======================================================
     *
     * SCHED_DFLOW_MGR::Add_Defs_Live_In
     *
     * add the resultes of <op> live into <rgn>
     * 
     * =======================================================
     */
void
SCHED_DFLOW_MGR::Add_Defs_Live_In (OP* op, REGION*rgn) {
   
    NODE_VECTOR entries = rgn->Entries () ;

    for (NODE_VECTOR_ITER iter = entries.begin () ; 
         iter != entries.end () ; 
         iter ++) {
        
        REGIONAL_CFG_NODE* n = *iter ;
        if (n->Is_Region ()) {
            Add_Defs_Live_In (op, n->Region_Node());
        } else {
            Add_Defs_Live_In (op, n->BB_Node());
        }
    }
}

    /* =======================================================
     *
     * SCHED_DFLOW_MGR::Add_Defs_Live_Out 
     *
     * Ref the header file for more details
     * 
     * =======================================================
     */
void
SCHED_DFLOW_MGR::Add_Defs_Live_Out (OP * op, BB * bb) {

    for (INT i = OP_results(op) - 1 ; i >= 0 ; --i) {

        TN *result_tn = OP_result(op,i);

        Set_TN_is_global_reg(result_tn) ;
        GTN_UNIVERSE_Add_TN (result_tn) ;
        GRA_LIVE_Add_Defreach_Out_GTN (bb,result_tn);
        GRA_LIVE_Add_Live_Out_GTN (bb,result_tn);
    }
}

    /* =======================================================
     *
     * SCHED_DFLOW_MGR::Add_Defs_Live_Out 
     *
     * Ref the header file for more details
     * 
     * =======================================================
     */
void
SCHED_DFLOW_MGR::Add_Defs_Live_Out (OP * op, REGION *rgn) {

    NODE_VECTOR exits = rgn->Exits () ;

    for (NODE_VECTOR_ITER iter = exits.begin () ; 
         iter != exits.end () ; 
         iter ++) {
        
        REGIONAL_CFG_NODE * n = *iter ;
        if (n->Is_Region ()) {
            Add_Defs_Live_Out (op, n->Region_Node()); 
        } else {
            Add_Defs_Live_Out (op, n->BB_Node ());
        }
    }
}

    /* ===========================================================
     *
     * P_Ready_Moving_Against_These_Path_Kill_Live_Defs 
     * 
     * TODO: fini this commment.
     *
     * ===========================================================
     */
BOOL
SCHED_DFLOW_MGR :: P_Ready_Moving_Against_These_Path_Kill_Live_Defs 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info, BB* to, 
     EXEC_PATH_SET* move_against, RGN_CFLOW_MGR* cflow_info) {

    Is_True (cand->Is_P_Ready (), 
             ("this routine is expected to be called when"
              "candidate is a P-ready one")); 
              
    REGION* scope = cflow_info -> Scope (); 
    OP* def = cand->Op ();
    BB* home_bb  = bb_info->Source_BB ();

    BBLIST* succs;
    FOR_ALL_BB_SUCCS(to, succs) {
        
        BB* succ = BBLIST_item(succs);
        if (!Are_Defs_Live_In (def, succ)) {
            continue;
        }

        REGION* homer = Home_Region (succ);
        if (homer != scope) { return TRUE; }

        EXEC_PATH_SET* eps = cflow_info->Get_Path_Flow_Thru (succ);
        if (!eps->Is_Subset_Of (move_against) || 
            !BB1_Postdominate_BB2 (home_bb, succ)) {
            return TRUE;
        }
    }

    return FALSE;
}

    /* ==========================================================
     *
     * Upward_Sched_Kill_Def_LiveOut_Of_Target_BB
     * 
     * ref the header file for details.
     *
     * =========================================================
     */
BOOL
SCHED_DFLOW_MGR::Upward_Sched_Kill_Def_LiveOut_Of_Target_BB
    (CANDIDATE* cand, SRC_BB_INFO* bb_info,RGN_CFLOW_MGR* cflow_info) {
    
    OP* def = cand->Op ();
    BB* to = bb_info->Target_BB ();

    if (!Are_Defs_Live_Out (def, to)) {
        return FALSE;
    }

    if (cand->Is_M_Ready ()) {

        BB* home_bb = OP_bb(cand->Op ());
        REGION* scope = ::Home_Region (home_bb);

        BBLIST* succs;
        FOR_ALL_BB_SUCCS(to, succs) {
        
            BB* succ = BBLIST_item(succs);
            if (!Are_Defs_Live_In (def, succ)) {
                continue;
            }

            REGION* homer = Home_Region (succ);
            if (homer != scope) { return TRUE; }

            if (!BB1_Postdominate_BB2 (home_bb, succ)) {
                return TRUE;
            }
        }

        return FALSE;
    }

    return P_Ready_Moving_Against_These_Path_Kill_Live_Defs 
            (cand, bb_info, to, 
             cand->Move_Against_Path_Set (), 
             cflow_info);
}

    /* ===========================================================
     *
     * Upward_Sched_Kill_Def_LiveOut_Of_Bookeeping_Place 
     * 
     * ref the header file for details.
     *
     * ===========================================================
     */
BOOL
SCHED_DFLOW_MGR::Upward_Sched_Kill_Def_LiveOut_Of_Bookeeping_Place 
    (CANDIDATE* cand, BOOKEEPING* bk,
     SRC_BB_INFO* bb_info, RGN_CFLOW_MGR* cflow_info) {

    OP* def = cand->Op ();
    BB* place = bk->Get_Placement ();

    if (!Are_Defs_Live_Out (def, place)) {
        return TRUE;
    }

    BB* home_bb = OP_bb(cand->Op ());
    REGION* scope = ::Home_Region (home_bb);

    if (cand->Is_M_Ready ()) {

        BBLIST* succs;
        FOR_ALL_BB_SUCCS(place, succs) {
        
            BB* succ = BBLIST_item(succs);
            if (!Are_Defs_Live_In (def, succ)) {
                continue;
            }

            REGION* homer = Home_Region (succ);
            if (homer != scope) { return TRUE; }

            if (!BB1_Postdominate_BB2 (home_bb, succ)) {
                return TRUE;
            }
        }

        return FALSE;
    }

    if (bk->Is_Dup_Bookeeping ()) {

        return P_Ready_Moving_Against_These_Path_Kill_Live_Defs 
                    (cand, bb_info, place, 
                     cand->Move_Against_Path_Set (), 
                     cflow_info);
    } 

    Is_True (bk->Is_P_Ready_Bookeeping (), 
            ("book keeping should be for the P_ready purpose"));
                         
    if (!BB1_Postdominate_BB2 (home_bb, place)) {
        EXEC_PATH_SET eps = 
            *(cflow_info->Get_Path_Flow_Thru (home_bb));
                    
        eps -= *(cflow_info->Get_Path_Flow_Thru (place));
        return P_Ready_Moving_Against_These_Path_Kill_Live_Defs 
                    (cand, bb_info, place, &eps, cflow_info);
    }
   
    return FALSE;
}

    /* ============================================================
     *
     * SCHED_DFLOW_MGR::Upward_Sched_Kill_LiveOut_Defs 
     *
     * Ref the header file for more details
     * 
     * ============================================================
     */
BOOL
SCHED_DFLOW_MGR::Upward_Sched_Kill_LiveOut_Defs 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info, 
     RGN_CFLOW_MGR* cflow_info) {

    OP* op = cand->Op ();
    BB* to = bb_info->Target_BB ();
    BB* home_bb = OP_bb(op);
    REGIONAL_CFG_NODE* home_nd = bb_info->Src_Node ();

    if (home_bb == to ||
        (cand->Is_M_Ready () && bb_info->Is_Cntl_Equiv ())) {
        return FALSE;
    }


        /* 1. check to see whether <op> kill defs live outof 
         *    target block.
         */
    if (Upward_Sched_Kill_Def_LiveOut_Of_Target_BB 
            (cand, bb_info, cflow_info)) {
        return TRUE;
    }

        /* 2. check to see whether <op> kill defs live outof
         *    bookeeping blocks.
         */
    BOOKEEPING_LST* bkl = cand->Bookeeping_Lst ();
    for (BOOKEEPING* bk = bkl->First_Item (); 
         bk != NULL;
         bk = bkl->Next_Item (bk)) {
    
        BB* place = bk->Get_Placement ();
        if (!Are_Defs_Live_Out (op, place)) {
            continue;
        }

        if (Upward_Sched_Kill_Def_LiveOut_Of_Bookeeping_Place 
                (cand, bk, bb_info, cflow_info)) {
            return TRUE;
        }
    }

    return FALSE;
}

    /* ============================================================
     *
     * SCHED_DFLOW_MGR::Downard_Sched_Kill_LiveIn_Defs 
     *
     * has yet implemented.
     * 
     * ============================================================
     */
BOOL SCHED_DFLOW_MGR :: Downard_Sched_Kill_LiveIn_Defs 
    (CANDIDATE* cand, SRC_BB_INFO* src_bb_info, 
     RGN_CFLOW_MGR* cflow_info) {

    FmtAssert (FALSE, 
        ("Downard_Sched_Kill_LiveIn_Defs has yet implemented"));

    return TRUE; /* make compiler happy */
}

    /* ========================================================
     *
     * Upward_Sched_Interfere_Nested_Rgns_LiveRanges 
     * 
     * ref the header file for details.
     *
     * ========================================================
     */
BOOL
SCHED_DFLOW_MGR :: Upward_Sched_Interfere_Nested_Rgns_LiveRanges 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info) {

    REGION_VECTOR* rv = cand->Move_Across_Rgns ();
    OP* op = cand->Op ();

    for (REGION_VECTOR_ITER iter = rv->begin ();
         iter != rv->end ();
         iter ++) {

        REGION* r = *iter;
        RGN_SUMMARY* rs = Get_Region_Summary (r);

        if (rs->Has_Call ()) { return TRUE; }
        if (rs->Has_Rotating_Kernel ()) { return TRUE; }
    
        TN_SET* def = rs->Killed_Def ();
        TN_SET* use = rs->TN_Used ();
        
            /* check output dependence and anti-dep 
             */
        for (INT i = OP_results(op) - 1 ; i >= 0 ; i--) {
            TN * result = OP_result(op,i);

            if (!TN_is_register(result) || TN_is_const_reg(result)) {
                continue ;
            }

            if (TN_SET_MemberP(def, result) ||
                TN_SET_MemberP(use, result)) {
                return TRUE;
            }
        } /* end of for(INT i= ...) */

            /* check flow dependence 
             */
        for (INT i = OP_opnds(op) - 1 ; i >= 0 ; i --) {
            TN * opnd = OP_opnd(op,i) ;

            if (!TN_is_register(opnd) || TN_is_const_reg(opnd)) {
                continue ; 
            }

            if (TN_SET_MemberP (def, opnd)) {
                return TRUE;
            }
        } /* end of for (INT i=...) */

        if (!rs->Has_Mem_OP ()) { return TRUE ; } 

        if (OP_load (op) && rs->Has_Store() || OP_like_store(op)) {
            return TRUE;
        }
        
    } /* end of for(REGION...ITER iter...) */

    return FALSE;
}

    /* =========================================================
     *
     * Downward_Sched_Interfere_Nested_Rgns_LiveRanges 
     *
     * Has yet implemented.
     *
     * ========================================================
     */
BOOL
SCHED_DFLOW_MGR :: Downward_Sched_Interfere_Nested_Rgns_LiveRanges 
    (CANDIDATE* cand, SRC_BB_INFO* src_bb_info) {
    
    FmtAssert (FALSE, 
("Downward_Sched_Interfere_Nested_Rgns_LiveRanges has yet implemented"));

    return TRUE; /* just to make compiler happy */
}


    /* ============================================================
     *
     * SCHED_DFLOW_MGR::Update_Liveness_After_Upward_Sched
     *
     * Ref the header file for more details
     * 
     * ============================================================
     */
void
SCHED_DFLOW_MGR::Update_Liveness_After_Upward_Sched
    (CANDIDATE* cand, SRC_BB_INFO* src_info, RGN_CFLOW_MGR* cflow_info) {

    BB* targ = src_info->Target_BB ();
    BB* src  = src_info->Source_BB ();
    OP* op   = cand->Op ();

    if (targ == src || !OP_results (op)) {
        return ; /* no need to update liveness */ 
    }

        /* step 1 : Let what <op> defs live out of target block. 
         */
    Add_Defs_Live_Out (op, src_info->Target_BB ());

        /* step 2 : let what <op> defs live at the exit-point of 
         *          each bookeeping block.
         */
    BOOKEEPING_LST* bkl = cand->Bookeeping_Lst ();
    for (BOOKEEPING* bk = bkl->First_Item (); 
         bk != NULL;
         bk = bkl->Next_Item (bk)) {
        Add_Defs_Live_Out (op, bk->Get_Placement ());
    }


        /* step 3: Let what <op> defs live into the entry point of 
         *        its origninal home block. 
         */
    Add_Defs_Live_In (op, src_info->Source_BB ()) ;
         

        /* step 4: Let what <op> defs live into and outof some blocks
         *         between cutting-set and src block.
         */
    BOOL is_m_ready = cand->Is_M_Ready ();
    BB_VECTOR* bbv = src_info->Move_Across_Or_Around_BBs ();
    for (BB_VECTOR_ITER iter = bbv->begin ();
         iter != bbv->end ();
         iter ++) {

        BB* b = *iter;

        BOOL btmp;
        if (is_m_ready) {
            btmp = TRUE;
        } else {
            EXEC_PATH_SET* eps = cflow_info->Get_Path_Flow_Thru (b);  
            btmp = !(cand->Move_Against_Path_Set()->
                        Intersection_Is_Empty (eps));
        }

        if (btmp) {
            Add_Defs_Live_In  (op, b);
            Add_Defs_Live_Out (op, b);
        } 
    }


    REGION_VECTOR* rv = src_info->Move_Across_Or_Around_Nested_Rgns ();
    for (REGION_VECTOR_ITER iter = rv->begin ();
        iter != rv->end ();
        iter ++) {

        BOOL btmp;
        REGION* r = *iter;

        if (is_m_ready) {
            btmp = TRUE;
        } else {
            EXEC_PATH_SET* eps = cflow_info->Get_Path_Flow_Thru (r);  
            btmp = !(cand->Move_Against_Path_Set()->
                     Intersection_Is_Empty (eps));
        }

        if (btmp) {
            Add_Defs_Live_In  (op, r);
            Add_Defs_Live_Out (op, r);
        }
    }
}



    /* ============================================================
     *
     * SCHED_DFLOW_MGR::Update_Liveness_After_Downward_Sched
     *
     * Ref the header file for more details
     * 
     * ============================================================
     */
void
SCHED_DFLOW_MGR::Update_Liveness_After_Downward_Sched
    (CANDIDATE* cand, 
     SRC_BB_INFO * src_info, 
     RGN_CFLOW_MGR* cflow_info) {

        /* has yet not implemented 
         */
    FmtAssert (FALSE, 
("Update_Liveness_After_Downward_Sched has not been implemented"));

}



    /* ============================================================
     *
     * SCHED_DFLOW_MGR::Upward_Sched_Violate_Dflow_Constrait
     *
     * Ref the header file for more details
     * 
     * ============================================================
     */
BOOL
SCHED_DFLOW_MGR::Upward_Sched_Violate_Dflow_Constrait
    (CANDIDATE* cand, SRC_BB_INFO* bb_info) {

    FmtAssert (FALSE, ("has yet implemented"));
    return FALSE;
}

    /* ============================================================
     *
     * SCHED_DFLOW_MGR::Downward_Sched_Violate_Dflow_Constrait
     *
     * Ref the header file for more details
     * 
     * ============================================================
     */
BOOL
SCHED_DFLOW_MGR::Downward_Sched_Violate_Dflow_Constrait 
    (CANDIDATE* cand, SRC_BB_INFO* bb_info) {

    FmtAssert (FALSE,("Downward_Sched_Violate_Dflow_Constrait "
                      "has not been implemented"));
    return FALSE;
}

