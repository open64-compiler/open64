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

#include "defs.h"

/* STL stuff */
#include <list>
#include <vector> 
#include <map>

/* memory management */
#include "mempool.h"
#include "mempool_allocator.h"

/* commonly used data structure and facilities */
#include "topcode.h" /* for TOP_Name */
#include "op.h"

#include "bb.h"
#include "bb_set.h"

#include "cgtarget.h"
#include "be_util.h"

#include "tracing.h"
#include "error.h"

#include "ipfec_defs.h"
#include "ipfec_options.h"

/* data-flow related stuff */
#include "gra_live.h"

#include "sched_util.h"
#include "sched_cflow.h"
#include "sched_rgn_info.h"

#define ACROSS_NODE_THRESHOLD 3
    
    /* ==============================================================
     *
     *      Tracing flags, options and some thresholds
     *
     * =============================================================
     */

/* part 1: tracing flags */
BOOL SCHED_TF_DUMP_IR      = FALSE ; 
BOOL SCHED_TF_DUMP_DAG     = FALSE ;
BOOL SCHED_TF_DUMP_CAND    = FALSE ;
BOOL SCHED_TF_SUMMARY_DUMP = FALSE ;
BOOL SCHED_TF_VERBOSE_DUMP = FALSE ;
BOOL SCHED_TF_DRAW_GLBL_CFG  = FALSE ;
BOOL SCHED_TF_DRAW_RGNL_CFG  = FALSE ;
BOOL SCHED_TF_DRAW_LOCAL_DAG = FALSE ;
BOOL SCHED_TF_DRAW_RGNL_DAG  = FALSE ;
BOOL SCHED_TF_TEST_SPEC      = FALSE ;
BOOL SCHED_TF_CANDSEL_DUMP   = FALSE ;
#if defined(TARG_SL)
BOOL SCHED_TF_DUMP_CS_PROCESS = FALSE;
#endif

#if defined(TARG_SL2)
/* this switch is used to trace sl2 macro instruction combination phase */ 
BOOL SCHED_TF_DUMP_MACRO_INSN_COMBINATION = FALSE; 

// this switch is used to trace c2.satd instruction combination 
BOOL SCHED_TF_DUMP_SATD_COMB = FALSE; 

/* this switch is used to trace op motion for some bb */ 
BOOL SCHED_TF_DUMP_OP_MOVE_IN_BB = FALSE; 

BOOL SCHED_TF_DUMP_CAND_SELECTION_PROCESS = FALSE;

BOOL SCHED_TF_DUMP_COND_MV_COMBINE = FALSE; 
#endif 

/*  threshold */
INT32 SAFE_CNTL_SPEC_PROB   = ((REACH_PROB)(REACH_PROB_SCALE * 0.20f)) ;
INT32 UNSAFE_CNTL_SPEC_PROB = ((REACH_PROB)(REACH_PROB_SCALE * 0.40f)) ;
INT32 SPEC_SAFE_LOAD_WITHOUT_TRANSFORM_REACH_PROB = 
                              ((REACH_PROB)(REACH_PROB_SCALE * 0.65f)) ;
static void
get_trace_flags (BOOL prepass) {

  if (prepass) {

    SCHED_TF_DUMP_IR        = Get_Trace (TP_A_GSCHED,DUMP_IR) ||
                                  Get_Trace (TKIND_IR, TP_A_GSCHED) ;
    SCHED_TF_DUMP_DAG       = Get_Trace (TP_A_GSCHED,DUMP_DAG) ;
    SCHED_TF_DUMP_CAND      = Get_Trace (TP_A_GSCHED,DUMP_CAND) ;
    SCHED_TF_SUMMARY_DUMP   = Get_Trace (TP_A_GSCHED,SUMMARY_DUMP) ;
    SCHED_TF_VERBOSE_DUMP   = Get_Trace (TP_A_GSCHED,VERBOSE_DUMP) ;
    SCHED_TF_DRAW_GLBL_CFG  = Get_Trace (TP_A_GSCHED,DRAW_GLBL_CFG) ;
    SCHED_TF_DRAW_RGNL_CFG  = Get_Trace (TP_A_GSCHED,DRAW_RGNL_CFG) ;
    SCHED_TF_DRAW_LOCAL_DAG = FALSE ; 
    SCHED_TF_DRAW_RGNL_DAG  = Get_Trace (TP_A_GSCHED,DRAW_RGNL_DAG) ;
    SCHED_TF_TEST_SPEC      = Get_Trace (TP_A_GSCHED,TEST_SPEC) ;
    SCHED_TF_CANDSEL_DUMP   = Get_Trace (TP_A_GSCHED,CANDSEL_DUMP) ;
#if defined(TARG_SL2)
    SCHED_TF_DUMP_COND_MV_COMBINE = Get_Trace(TP_A_GSCHED,  DUMP_COND_MV_COMBINE); 
#endif
  } else {

    SCHED_TF_DUMP_IR        = Get_Trace (TP_A_LSCHED,DUMP_IR) ||
                                  Get_Trace (TKIND_IR, TP_A_LSCHED) ;
    SCHED_TF_DUMP_DAG       = Get_Trace (TP_A_LSCHED,DUMP_DAG) ;
    SCHED_TF_DUMP_CAND      = Get_Trace (TP_A_LSCHED,DUMP_CAND) ;
    SCHED_TF_SUMMARY_DUMP   = Get_Trace (TP_A_LSCHED,SUMMARY_DUMP) ;
    SCHED_TF_VERBOSE_DUMP   = Get_Trace (TP_A_LSCHED,VERBOSE_DUMP) ;
    SCHED_TF_DRAW_GLBL_CFG  = Get_Trace (TP_A_LSCHED,DRAW_GLBL_CFG) ;
    SCHED_TF_DRAW_RGNL_CFG  = Get_Trace (TP_A_LSCHED,DRAW_RGNL_CFG) ;
    SCHED_TF_DRAW_LOCAL_DAG = Get_Trace (TP_A_LSCHED,DRAW_LOCAL_DAG); 
    SCHED_TF_DRAW_RGNL_DAG  = Get_Trace (TP_A_LSCHED,DRAW_RGNL_DAG) ;
    SCHED_TF_TEST_SPEC      = Get_Trace (TP_A_LSCHED,TEST_SPEC) ;
    SCHED_TF_CANDSEL_DUMP   = Get_Trace (TP_A_LSCHED,CANDSEL_DUMP) ;
#if defined(TARG_SL)
    SCHED_TF_DUMP_CS_PROCESS = Get_Trace(TP_A_LSCHED,DUMP_CS_PURPOSE_PROCESS);
#endif
#if defined(TARG_SL2)
    SCHED_TF_DUMP_MACRO_INSN_COMBINATION = Get_Trace(TP_A_LSCHED, DUMP_MACRO_INSN_COMBINATION); 
    SCHED_TF_DUMP_OP_MOVE_IN_BB = Get_Trace(TP_A_LSCHED, DUMP_OP_MOVE_IN_TARGET_BB); 
    SCHED_TF_DUMP_CAND_SELECTION_PROCESS = Get_Trace(TP_A_LSCHED, DUMP_CAND_SELECTION_PROCESS); 
    SCHED_TF_DUMP_SATD_COMB = Get_Trace(TP_A_LSCHED, DUMP_SATD_COMB); 
    SCHED_TF_DUMP_COND_MV_COMBINE = Get_Trace(TP_A_LSCHED,  DUMP_COND_MV_COMBINE); 
#endif 
    }

}


static void
get_spec_prob (void) {

    float f_safe_cntl_spec_prob , f_unsafe_cntl_spec_prob ;

    if (IPFEC_safe_cntl_spec_prob) {
        sscanf (IPFEC_safe_cntl_spec_prob,"%f", &f_safe_cntl_spec_prob) ;      
        if (f_safe_cntl_spec_prob >= 0.0 && f_safe_cntl_spec_prob <= 1.0) {
	        SAFE_CNTL_SPEC_PROB = INT32(f_safe_cntl_spec_prob * REACH_PROB_SCALE);
        }
    }

    if (IPFEC_unsafe_cntl_spec_prob) {
        sscanf (IPFEC_unsafe_cntl_spec_prob,"%f", &f_unsafe_cntl_spec_prob) ;      
        if (f_unsafe_cntl_spec_prob >= 0.0 && f_unsafe_cntl_spec_prob <= 1.0) {
	        UNSAFE_CNTL_SPEC_PROB = INT32(f_unsafe_cntl_spec_prob * REACH_PROB_SCALE);
        }
    }
}

void
Get_Sched_Opts (BOOL prepass) {

    get_trace_flags (prepass) ;
    get_spec_prob () ; 

    /* just for the time being */
    IPFEC_Glos_Reg_Pressure_Aware = FALSE ;
}


    /* ===============================================================
     * ===============================================================
     *
     *      implementation of CFG_NODE_MAP
     *
     * ===============================================================
     * ===============================================================
     */
void
CFG_NODE_MAP::Initialize_Map (REGION *rgn) {

    _bb_map_vect.clear () ;
    _rgn_map_vect.clear () ;

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(rgn->Regional_Cfg()) ; 
        iter != 0 ; ++ iter) {

        if ((*iter)->Is_Region()) {
            _rgn_map_vect.push_back (
                    tagNODE_ID_VAL_PAIR(NULL,(*iter)->Region_Node()->Id()));      
        } else {
            _bb_map_vect.push_back (
                    tagNODE_ID_VAL_PAIR(NULL,BB_id((*iter)->BB_Node())));
        }
    }

    INT32 upbound = _bb_map_vect.size() - 1;
    if (upbound > 0) _bubble_sort (_bb_map_vect,0,upbound) ;

    upbound = _rgn_map_vect.size() - 1 ;
    if (upbound > 0) _bubble_sort (_rgn_map_vect,0,upbound);
}


void
CFG_NODE_MAP::Initialize_Map (BB *bb) {
    _bb_map_vect.clear () ;
    _rgn_map_vect.clear () ;

    _bb_map_vect.push_back (NODE_ID_VAL_PAIR(NULL,BB_id(bb)));
}

void
CFG_NODE_MAP::_bubble_sort (
    _PAIR_VECT& vect, INT32 low_idx, INT32 hign_idx) 
{
    INT32 last_elem_idx = vect.size () - 1;
    BOOL change = TRUE;
    
    for (INT32 upbound = last_elem_idx; 
         change && upbound > 0 ; upbound--) {

        change = FALSE;

        for (INT32 j = 0 ; j < upbound; j ++) {
            INT32 tmp = vect[j].node_id - vect[j+1].node_id ;

            if (tmp < 0) { continue ; }

            if (tmp > 0) { /* swap two element : vect[i] <=> vect[j] */
                NODE_ID_VAL_PAIR p = vect[j+1] ;
                vect[j+1] = vect[j]; vect[j] = p;
            } else {
                FmtAssert (FALSE, ("two node with identical id %d", 
                            vect[j].node_id)) ;
            }

            change = TRUE;
        }
    }
}


INT32
CFG_NODE_MAP::_find_elem (_PAIR_VECT& vect, INT32 id) {
    INT32 low = 1, high = vect.size(); 
    
    while (low < high) {
        INT32 mid = ((low + high) >> 1) ;
        INT32 diff = vect[mid-1].node_id - id;

        if (diff < 0) { low = mid + 1 ; continue ; } 
        if (diff > 0) { high = mid - 1 ; continue ; } 
        else return mid - 1;
    }

    if (low > high) return -1 ;
    return vect[low-1].node_id == id ? low - 1 : -1 ;
}

void 
CFG_NODE_MAP::_set_map (_PAIR_VECT& vect, INT32 id, void* value) {
    INT32 pos = _find_elem (vect, id) ;
    if (pos < 0) {
        vect.push_back (tagNODE_ID_VAL_PAIR(value,id));  
        _bubble_sort (vect,0,vect.size()-1) ;
        pos = _find_elem (vect, id);
        Is_True (pos >= 0, ("fail to find element")); 
    }

    vect[pos].value = value ;
}


    /* =============================================================
     * =============================================================
     *
     *      implementation of SCHED_BB_ANNOT
     * 
     * =============================================================
     * =============================================================
     */
#define OP_INC_NUM (20)

inline void
SCHED_BB_ANNOT::_set_op_annot (OP * op, SCHED_OP_ANNOT * annot) {
    Is_True (OP_bb(op) == _bb, 
             ("BB%d is not OP[BB:%d map%d]'s home BB",
              BB_id(_bb), BB_id(OP_bb(op)), OP_map_idx(op)));
    
    _annot_inited_ops = 
        BS_Union1D (_annot_inited_ops,OP_map_idx(op),_mem_pool);
    BB_OP_MAP_Set (_ops_annot_map, op, (void*)annot);
}


SCHED_BB_ANNOT::SCHED_BB_ANNOT (BB * bb, MEM_POOL *mp) : 
    _bb(bb), _mem_pool(mp)
{
    _ops_annot_map      = BB_OP_MAP_Create (bb, _mem_pool);

    _1st_append_op      = _last_prepend_op = NULL;                 
    _xfer_op  = BB_xfer_op (bb);

    _annot_inited_ops =
        BS_Create_Empty (BB_next_op_map_idx(_bb) + OP_INC_NUM, _mem_pool);
    _init_ops_annot () ;
}


SCHED_OP_ANNOT*
SCHED_BB_ANNOT::Detach_OP_Annot (OP* op) {

    SCHED_OP_ANNOT * op_annot = Get_OP_Annot (op) ;
    _annot_inited_ops = BS_Difference1D (_annot_inited_ops,
                                         OP_map_idx(op));
    return op_annot ;
}


void
SCHED_BB_ANNOT::Attach_OP_Annot (OP * op, SCHED_OP_ANNOT * annot) {
    _set_op_annot (op,annot) ;
}


void
SCHED_BB_ANNOT::_init_ops_annot (void) {

    INT32 size = BB_length (_bb) + OP_INC_NUM;
    _annot_inited_ops = BS_Create_Empty (size, _mem_pool);

    OP * op;
    FOR_ALL_BB_OPs (_bb, op) {

        SCHED_OP_ANNOT * op_annot = 
            CXX_NEW (SCHED_OP_ANNOT(op),_mem_pool); 

        BB_OP_MAP_Set (_ops_annot_map, op, (void*)op_annot);
        
        _annot_inited_ops = 
            BS_Union1D(_annot_inited_ops, OP_map_idx(op), _mem_pool);
    }
}

SCHED_OP_ANNOT * 
SCHED_BB_ANNOT::Init_New_OP_Annot (OP* op) {

    _annot_inited_ops = 
        BS_Union1D (_annot_inited_ops, OP_map_idx(op), _mem_pool); 

    SCHED_OP_ANNOT * op_annot = 
                        CXX_NEW (SCHED_OP_ANNOT(op),_mem_pool);
    Set_OP_Annot (op, op_annot);
                    
    return op_annot;
}



    /* ===========================================================
     * ===========================================================
     *
     *              IMPLEMENTATION of SCHED_RGN_ANNOT
     * 
     * ===========================================================
     * ===========================================================
     */

/* NONE */

    /* =============================================================
     * =============================================================
     *
     *          SCHED_OP_ANNOT implementation
     *
     * =============================================================
     * =============================================================
     */

#define OP_INC_NUM (20)


    /* ===============================================================
     * ===============================================================
     *
     *          SCHED_ANNOT  implementation 
     *
     * ===============================================================
     * ===============================================================
     */

INT32 SCHED_ANNOT::_inst_num = 0 ;
SCHED_ANNOT  sched_annot ;

SCHED_ANNOT::SCHED_ANNOT (void) {
    _node_map = NULL;

    if (_inst_num++) {
       Is_True (FALSE, 
            ("two instances of class SCHED_ANNOT live simutaneously!"));
    }

    _rgn_scope = NULL;
    _local_scope = NULL;
}

void
SCHED_ANNOT::Init (REGION *scope) {

    MEM_POOL_Pop (&_mem_pool);
    MEM_POOL_Push (&_mem_pool);

    _node_map = CXX_NEW(CFG_NODE_MAP(&_mem_pool),&_mem_pool);
    _node_map -> Initialize_Map (scope) ; 
    
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter = scope->Regional_Cfg() ;
         iter != 0 ; ++iter) {

        if ((*iter)->Is_Region()) {

           REGION * rgn = (*iter)->Region_Node();
           _node_map->Set_Map (rgn, 
                CXX_NEW(SCHED_RGN_ANNOT(rgn,&_mem_pool), &_mem_pool));

        } else {

           BB * bb = (*iter)->BB_Node();
           _node_map->Set_Map (bb, 
                CXX_NEW(SCHED_BB_ANNOT(bb,&_mem_pool),&_mem_pool));

        }
    }

    _isolated_bbs = BB_SET_Create_Empty (PU_BB_Count+2,&_mem_pool);

    _rgn_scope = scope;
    _local_scope = NULL;
}

void
SCHED_ANNOT::Init (BB *scope) {

    MEM_POOL_Pop (&_mem_pool) ;
    MEM_POOL_Push (&_mem_pool);

    _node_map = CXX_NEW(CFG_NODE_MAP(&_mem_pool),&_mem_pool);
    _node_map->Initialize_Map (scope);
    _node_map->Set_Map (scope, 
                CXX_NEW(SCHED_BB_ANNOT(scope,&_mem_pool),&_mem_pool));

    _isolated_bbs = BB_SET_Create_Empty (PU_BB_Count+2, &_mem_pool);

    _rgn_scope = NULL;
    _local_scope = scope;
}

    /* =====================================================================
     *
     *  ::Dump
     * 
     *  Dump the status of SCHED_ANNOT
     * 
     * ====================================================================
     */
void
SCHED_ANNOT::Dump (FILE *f) {

    if (!_node_map) {
        fprintf (f, "NULL\n"); return ;
    } 
         
    
    if (_rgn_scope) {
        for (TOPOLOGICAL_REGIONAL_CFG_ITER iter = _rgn_scope->Regional_Cfg() ; 
             iter != 0 ; ++iter) {
            if ((*iter) -> Is_Region()) {
                REGION *rgn = (*iter)->Region_Node();
                SCHED_RGN_ANNOT * rgn_annot = 
                    (SCHED_RGN_ANNOT *)_node_map->Get_Map (rgn);

                fprintf (f, "RGN:%d ", rgn->Id()) ;
                if (!rgn_annot) fprintf (f, "NULL\n");
                else  rgn_annot->Dump (f);
            } else {
                BB * bb = (*iter)->BB_Node () ;
                SCHED_BB_ANNOT * bb_annot = 
                    (SCHED_BB_ANNOT *)_node_map->Get_Map (bb);
                
                fprintf (f, "BB:%d ", BB_id(bb));
                if (!bb_annot) fprintf (f, "NULL \n");
                else bb_annot->Dump (f);
            }
        }
    } else {
        SCHED_BB_ANNOT * bb_annot = 
            (SCHED_BB_ANNOT *)_node_map->Get_Map(_local_scope);
        
        fprintf (f, "BB:%d ", BB_id(_local_scope));
        if (bb_annot) fprintf (f, "NULL \n");
        else bb_annot->Dump (f);
    }
}






    /* ============================================================
     * ============================================================
     * 
     *          other miscellaneous utilities 
     * 
     * ============================================================
     * ==========================================================
     */

const char *spec_text[] = {
    "non-spec", "cntl-spec", "data-spec", "comb-spec", "can't spec"
};

    /* ==============================================================
     * 
     * Load_Has_Valid_Vaddr 
     * 
     * return TRUE if <op> is guaranteed to have valid virutal address. 
     * This is a ugly patch to many phase which does not mark safe load
     * as OP_safe_load ().
     * 
     * ==============================================================
     */ 
BOOL
Load_Has_Valid_Vaddr (OP* ld) {
    return OP_load (ld) && CGTARG_Can_Be_Speculative(ld);
}

#ifdef TARG_IA64
    /* =============================================================
     *
     * CGTARG_adjust_latency:
     *
     *    On Itanium processor, some latency may vary depending on 
     *    which issue ports are used by pred and/or succ. 
     *    The latency is first assigned to be its possible 
     *    max value. Scheduler should adjust these latencies in 
     *    the course of scheduling. 
     *
     * =============================================================
     */
INT32 
CGTARG_adjust_latency (ARC* arc, ISSUE_PORT pred_port, ISSUE_PORT succ_port) {

    OP* pred = ARC_pred(arc);
    OP* succ = ARC_succ(arc);

        /* rule 1: 
         *   when pred can be 
         *      - issued both on I and M-unit and 
         *      - it is *ACTUALLY* issued on M-unit. 
         *     the successor is 
         *      - either load 
         *      - or store, 
         *   we minus latency by 1.
         */
    if (ARC_kind(arc) != CG_DEP_REGIN) {
        return 0;
    }

    if (!EXEC_PROPERTY_is_I_Unit(OP_code(pred)) ||
        !EXEC_PROPERTY_is_M_Unit(OP_code(pred)) ||
        !OP_m_unit(pred)) {

        return 0;
    }

    if (OP_load(succ) || OP_store(succ)) {

        if (ARC_opnd(arc) == OP_find_opnd_use (succ, OU_base)) {
            return -1;
        }
    }

    return 0;
}
#endif

    /* =============================================================
     *
     * Derive_Spec_Type_If_Violate_Dep 
     * 
     *  Derive speculation type(data and/or control spec) if we 
     *  violate the given dependence.
     *
     *  return values include:
     *      - SPEC_DISABLED: we could violate this dependence. 
     *      - SPEC_DATA:     data speculation
     *      - SPEC_CNTL:     control speculation.
     *      - SPEC_NONE:     it is fake dependence 
     *
     * =============================================================
     */
SPEC_TYPE
Derive_Spec_Type_If_Violate_Dep (ARC* Arc) {

    if (!ARC_is_dotted(Arc)) { return SPEC_DISABLED; }

    if (ARC_is_data_spec(Arc)) { return SPEC_DATA ; } 

    Is_True (ARC_is_control_spec(Arc), ("Arc is not control speculative!"));


    OP* pred = ARC_pred(Arc);
    OP* succ = ARC_succ(Arc);
        
    BB* pred_home = OP_bb(pred);
    BB* succ_home = OP_bb(succ);

    if (pred_home != succ_home && TOP_is_xfer (OP_code(pred)) && 
        !OP_call(pred) && !OP_chk(pred) &&
        BB1_Postdominate_BB2 (succ_home, pred_home)) {
            /* pred is a normal branch. if <succ>'s home post-dorm
             * pred's home, then moving <succ> across <pred>'s home
             * is not speculation. 
             */
        return SPEC_NONE ; 
    }
    
    if (OP_call(pred) && !IPFEC_Glos_Motion_Across_Calls) {
        return SPEC_DISABLED ;
    }

    return SPEC_CNTL ;    
}

BOOL
OP1_Defs_Are_Used_By_OP2 (OP* op1, OP* op2) {

    for (INT16 i = 0; i < OP_results(op1); i++) {
		for (INT16 j = 0; j < OP_opnds(op2); j++) {
			if (OP_opnd(op2, j) == OP_result(op1, i)) return TRUE;
		}
	}
    return FALSE;
}

BOOL
OP1_Defs_Are_Killed_By_OP2 (OP* op1, OP* op2) {

    BOOL killed;
	for (INT16 i = 0; i < OP_results(op1); i++) {
	    killed = FALSE;
		for (INT16 j = 0; j < OP_results(op2); j++) {
			if (OP_result(op2, j) == OP_result(op1, i)) killed = TRUE;
		}
		if (!killed) return FALSE;
	}
	return TRUE;
}
