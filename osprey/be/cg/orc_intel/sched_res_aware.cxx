//-*-c++-*-

#include "defs.h"
#include "tracing.h"
#include "ipfec_defs.h"
#include "ipfec_options.h"

#include "sched_util.h"
#include "sched_res_aware.h"

/* TODO: move this quantum to ia64/ia64target.cxx: */
#define ALAT_ENTRY_NUM (28)

void
BB_VIGILANT_PNT_MGR::Init (void) {
    OP* op;
    FOR_ALL_BB_OPs (_bb, op) {
        if (VIGILANT_PNT::Candidate (op)) {
            Add_Vigilant_Point (op);
        }
    }
}

void
BB_VIGILANT_PNT_MGR::Update_Vp_For_Compensation_Code 
    (OP* bookeeping) {

    if (!VIGILANT_PNT::Candidate (bookeeping)) {
        return;
    }

    VIGILANT_PNT* vp = CXX_NEW(VIGILANT_PNT(bookeeping), _mp);
    _vigilant_pnt_list.push_back (vp);
    if (CGTARG_Is_OP_Advanced_Load (bookeeping)) {
        _exit_vp.Inc_Pending_Adv_Ld ();  
    } else if (OP_chk_a (bookeeping)) {
        _exit_vp.Dec_Pending_Adv_Ld ();  
    }

    vp->Set_Pending_Adv_Ld (_exit_vp.Pending_Adv_Ld ());
}

DATA_SPEC_RES_CONSTRAIT_MGR::DATA_SPEC_RES_CONSTRAIT_MGR 
    (MEM_POOL* mp) : _mp(mp) {

    for (INT i = 0; i < sizeof(_buckets)/sizeof(_buckets[0]); i++) {
        _buckets[i] = CXX_NEW(BB_VIGILANT_PNT_MGR_LIST(_mp), _mp);  
    }
    _rgn = NULL;
}

void
DATA_SPEC_RES_CONSTRAIT_MGR::Init (REGION* gscope) {
    _rgn = gscope;
    for (SEQ_REGIONAL_CFG_ITER iter(gscope->Regional_Cfg());
         iter != 0;++iter) {
        REGIONAL_CFG_NODE *n = *iter; 
        if (!n->Is_Region ()) {
            BB* blk = n->BB_Node ();
            INT idx = hash_idx (blk);
            BB_VIGILANT_PNT_MGR* vpmgr = 
                CXX_NEW(BB_VIGILANT_PNT_MGR(_mp, blk), _mp);
            vpmgr->Init ();
            _buckets[idx]->push_back (vpmgr);
        }
    }
}

void
DATA_SPEC_RES_CONSTRAIT_MGR::Init (BB* lscope) {
    INT idx = hash_idx (lscope);
    BB_VIGILANT_PNT_MGR* vpmgr = 
        CXX_NEW(BB_VIGILANT_PNT_MGR(_mp, lscope), _mp);
    vpmgr->Init ();
    _buckets[idx]->push_back (vpmgr);
}

BOOL
DATA_SPEC_RES_CONSTRAIT_MGR::There_Exist_Misc_Dep_Arc (OP* pred, OP* succ) {

    for (ARC_LIST* arcs = OP_succs(pred); arcs != NULL; 
         arcs = ARC_LIST_rest(arcs)) {

        ARC *arc = ARC_LIST_first(arcs) ;
        if (ARC_kind(arc) == CG_DEP_MISC && ARC_succ(arc) == succ) {
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
DATA_SPEC_RES_CONSTRAIT_MGR::Check_Res_Constraint_Helper
    (BB* bb, OP* predbound, OP* succbound, OP* op) {

    BB_VIGILANT_PNT_MGR* vpmgr = Find (bb);
    Is_True (vpmgr != NULL, 
        ("BB_VIGILANT_PNT_MGR corresponding to BB:%d does not exist", 
         BB_id(bb)));

    VIGILANT_PNT_LIST& vps = vpmgr->Vigilant_point_list ();
    for (VIGILANT_PNT_LIST_ITER iter = vps.begin ();
         iter != vps.end (); iter++) {
        VIGILANT_PNT* vp = *iter;
        OP* vpop = vp->Op ();
        if (predbound && OP_Precedes(vpop, predbound) ||
            succbound && OP_Follows(vpop, succbound) || 
            OP_Scheduled (vpop)) {
            continue;                    
        }

        if (OP_bb(op) == OP_bb(vpop) && !OP_Precedes (vpop, op)) {
            continue;
        }

        if (OP_load(vpop) && !CGTARG_Is_OP_Advanced_Load(vpop)) {
            continue;
        }

        if (vp->Pending_Adv_Ld () >= ALAT_ENTRY_NUM) {
            if (OP_like_store(vpop) && OP_load(op) && 
                !CGTARG_Is_OP_Advanced_Load(op) &&
                !There_Exist_Misc_Dep_Arc (vpop,op)) {
                continue;
            }

            if (!There_Exist_Misc_Dep_Arc (vpop, op)) {
                new_arc_with_latency (CG_DEP_MISC, vpop, op, 0, 0, 0, TRUE);
            }
            return FALSE;
        }
    }

    if (bb != OP_bb(op) && succbound) {
        VIGILANT_PNT& vp = vpmgr->Vp_for_Exit();
        if (vp.Pending_Adv_Ld () >= ALAT_ENTRY_NUM) {
            return FALSE; 
        }
    }

    return TRUE;
}

BOOL
DATA_SPEC_RES_CONSTRAIT_MGR::Check_Res_Constraint 
    (BB* bb, OP* op) {
    return Check_Res_Constraint_Helper (bb, NULL, NULL, op);
}

BOOL
DATA_SPEC_RES_CONSTRAIT_MGR::Check_Res_Constraint 
    (REGION* rgn, OP* op, SRC_BB_INFO* src_info) {

    BB* blk = src_info->Target_BB ();
    if (!Check_Res_Constraint_Helper (blk, NULL, NULL, op)) {
        return FALSE;
    }

    BB_VECTOR* bbv = src_info->Move_Across_Or_Around_BBs ();
    for (BB_VECTOR_ITER iter = bbv->begin (); 
         iter != bbv->end ();
         iter ++) {
        if (!Check_Res_Constraint_Helper (*iter, NULL, NULL, op)) {
            return FALSE;
        }
    }

    blk = src_info->Source_BB ();
    if (BB_first_op (blk) != op) {
        if (!Check_Res_Constraint_Helper (blk, NULL, OP_prev(op), op)) {
            return FALSE;
        }
    }
    return TRUE;
}


/* Update the number-of-pending-adv-load of vigilants points 
 * between <upperbound> and <lowerbound> inclusively in block 
 * <blk>. <upperbound> and <lowerbound> can be NULL, which 
 * means entry-point and exit point of <blk> respectively.
 */
void
DATA_SPEC_RES_CONSTRAIT_MGR::Update_Pending_Adv_Ld_Info 
    (BB* blk, OP* upperbound, OP* lowerbound, OP* sched_op) {

    BB_VIGILANT_PNT_MGR* vpmgr = Find (blk);
    Is_True (vpmgr != NULL, 
        ("BB_VIGILANT_PNT_MGR corresponding to BB:%d does not exist", 
         BB_id(blk)));

    VIGILANT_PNT_LIST& vps = vpmgr->Vigilant_point_list ();

    BOOL inc = OP_chk(sched_op) ? FALSE: TRUE;
    for (VIGILANT_PNT_LIST_ITER iter = vps.begin ();
         iter != vps.end (); iter++) {
        VIGILANT_PNT* vp = *iter;
        OP* vpop = vp->Op ();
        if (upperbound && OP_Precedes(vpop, upperbound) ||
            lowerbound && OP_Follows(vpop, lowerbound) || 
            OP_Scheduled (vpop)) {
            continue;                    
        }

        if (OP_bb(sched_op) == blk && !OP_Precedes(vpop, sched_op))
            continue;

        if (inc) {
            vp->Inc_Pending_Adv_Ld ();
        } else if (vp->Pending_Adv_Ld() > 0) {
            vp->Dec_Pending_Adv_Ld ();
        }
    } /* end of for */

    if (!lowerbound) {
        vpmgr->Vp_for_Exit().Inc_Pending_Adv_Ld ();
    }
}

void
DATA_SPEC_RES_CONSTRAIT_MGR::Update_Pending_Adv_Ld_Info 
    (SRC_BB_INFO* srcinfo, OP* sched_op) {

    Is_True (OP_bb(sched_op) == srcinfo->Source_BB () &&
             !OP_Scheduled(sched_op), 
             ("We require that this function should be "
              "called before the OP is physically moved"));

    /* 1. update the number of pending advanced loads of 
     * vigilant point in target block 
     */
    BB* blk = srcinfo->Target_BB ();
    if (OP_bb(sched_op) != blk) {
        Update_Pending_Adv_Ld_Info (blk, NULL, NULL, sched_op);
    } else if (BB_first_op(blk) != sched_op) {
        Update_Pending_Adv_Ld_Info (blk, NULL, OP_prev(sched_op), sched_op);
    }

    /* 2. update the number of pending advanced loads of 
     * vigilant point in source block 
     */
    blk = srcinfo->Target_BB ();

    blk = srcinfo->Source_BB ();
    if (blk != srcinfo->Target_BB () && OP_prev(sched_op)) {
        Update_Pending_Adv_Ld_Info (blk, NULL, OP_prev(sched_op), sched_op);
    }
    
    /* 3. update the number of pending advanced loads of 
     * vigilant point in those block the code motion across.
     */
    BB_VECTOR* bbv = srcinfo->Move_Across_Or_Around_BBs ();
    for (BB_VECTOR_ITER iter = bbv->begin ();
         iter != bbv->end ();
         iter ++) {
        Update_Pending_Adv_Ld_Info (*iter, NULL, NULL, sched_op);
    }
}


void
DATA_SPEC_RES_CONSTRAIT_MGR::Update_Vp_For_Compensation_Code 
    (OP* bookeeping, BB* compensation_blk) {

    Is_True (OP_bb(bookeeping) == compensation_blk, 
        ("Update_Vp_For_Compensation_Code () should be called "
         "after compensation code is really generated since "
         "the initial value of the number-of-pending-advanced-ld "
         "is derived from the info recorded in dummy vigilant point"));

    if (!VIGILANT_PNT::Candidate (bookeeping)) {
        return;
    }

    BB_VIGILANT_PNT_MGR* vpmgr = Find (OP_bb(bookeeping)); 
    vpmgr->Update_Vp_For_Compensation_Code (bookeeping);
}
