//-*-c++-*-

/* The placeholer for license -- it is up to our manager shinmin
 * to determine which license is appropriate for the code 
 * contributed by the 'osprey' probject. 
 */ 

#ifndef SCHED_RES_AWARE_included
#define SCHED_RES_AWARE_included

/* ==============================================================
 * ==============================================================
 * 
 * This module is dedicated to resource-awareness of the data 
 * speculation performed during the prepass scheduling.
 * 
 * o. motivating example:
 * =======================
 *   original code:
 *     st4 [..] = ry;
 *     ld4 r1<=     # first ld
 *       ...
 *     ld4 r33<=    # 33rd ld
 * 
 *   non-resource-aware data speculation: 
 *     ld4.a r1,   # first ld
 *     ...
 *     ld4.a r33,  # 33rd ld
 *     st4 
 *     chk.a r1,   # first ld
 *     ...
 *     chk.a r33   # 33rd ld.
 * Suppose that all loads illustrated in the example alias with
 * the st4, and all these loads are highly desirable to be 
 * scheduled before st4. 
 *   In IA-64, the status of each data speculation is kept 
 * track of via an ALAT entry. ALAT has limited entries -- 
 * e.g itanium-2 has 32 entries. Therefore, if scheduler data 
 * speculates all the 33 loads, the 33rd data speculation will 
 * evict an entry corresponding a load, say 2nd load illustrated. 
 * Therefore, 2nd chk, which is corresponing to 2nd advanced load,
 * fail to find an matching ALAT entry and hence incur high 
 * penalty check-miss. The penalty is high. In itanium-2 system,
 * it is 18 cycles plus the cycle in executing the instructions 
 * in the speculative chain and others (e.g I-cache-miss when 
 * control shift from check instruction to recovery block).
 * 
 *
 * o. Our approach
 * ================
 * 
 *   We introduce a data structure called 'vigilant point' which
 * keep track of the number of 'pending' data speculation across
 * it. 
 *    A data speuclation of load <l> is called 'pending' at point 
 * <p> if the chk.a corresponding to <l> dose not reside between 
 * load <l> and point <p>. If a data speculation is not 'pending', 
 * the corresponding ALAT entry has already removed by chk.a.  
 *
 *   We curtail the data speculation by disallowing any data 
 * speculation across a vigilant point where the number of 
 * pending data speculation reaches the capacity of ALAT (32 in 
 * itanium-2 processor).
 *  
 *  We associate following instrutions with a vigilant point since
 * them (may) add or remove an ALAT entry: load (not marked 
 * OP_no_alias()), store, chk.a. Besides, the exit-point of each 
 * basic block is associated with a dummy vigilant point. 
 * The dummy vp help scheduler to evalute the initial value of 
 * the number-of-pending-data-spec of some compensation codes. 
 * The number of vigilant point (inc dummy vp) is rougly equal to  
 * 10% of instructions in the PU. 
 * 
 *  The number-of-pending-data-spec of vigilants points are 
 * updated after each data speculation. The area where vp should 
 * be updated is exactly the one where liveness should be updated. 
 *
 * ==================================================================
 */  

#include "defs.h"
#include "ipfec_defs.h"
#include "ipfec_options.h"

#include "tracing.h"
#include "errors.h"

#include "op.h"
#include "bb.h"
#include "be_util.h"

#include "region.h"
#include "sched_cand.h"

#include <list>

class VIGILANT_PNT {
private:
   OP* _op;  /* corresponding OP, NULL for dummy vigilant point*/  
   INT32 _pending_adv_ld;  /* num of pending advanced load */

public:
    VIGILANT_PNT (OP* op) : _op(op) { 
        _pending_adv_ld = 0; 
    }

    ~VIGILANT_PNT (void) {}

    OP* Op (void) const { return _op; }

    INT32 Pending_Adv_Ld (void) const { return _pending_adv_ld; }
    void Inc_Pending_Adv_Ld (void) { _pending_adv_ld ++; }
    void Dec_Pending_Adv_Ld (void) { 
        _pending_adv_ld--; 
        Is_True (_pending_adv_ld >= 0, 
                 ("The number of pending advanced load is negative"));
    }
    void Set_Pending_Adv_Ld (INT num) { _pending_adv_ld = num; }

    /* whether <op> should be associated with a vigilant pt or not */
    static BOOL Candidate (OP* op) {
        return OP_chk_a(op) || OP_like_store(op) || 
               OP_load(op) && !OP_no_alias(op);
    }
};


/* ==================================================================
 * 
 * BB_VIGILANT_PNT_MGR is introduced to manage the vigilant points 
 * in one block. All BB_VIGILANT_PNT_MGRs are futher managed by 
 * DATA_SPEC_RES_CONSTRAIT_MGR.
 *
 * ==================================================================
 */
typedef mempool_allocator<VIGILANT_PNT*> VIGILANT_PNT_ALLOC;
typedef std::list<VIGILANT_PNT*, VIGILANT_PNT_ALLOC> VIGILANT_PNT_LIST;
typedef VIGILANT_PNT_LIST::iterator VIGILANT_PNT_LIST_ITER;

class BB_VIGILANT_PNT_MGR {
private:
    MEM_POOL* _mp;
    BB* _bb;

    VIGILANT_PNT_LIST _vigilant_pnt_list;
    VIGILANT_PNT _exit_vp; /* the dummy vp for exit-point of <_bb> */

public:
    BB_VIGILANT_PNT_MGR (MEM_POOL* mp, BB* bb) : 
        _mp(mp), _vigilant_pnt_list(mp), _bb(bb), _exit_vp(NULL) { }
    void Init (void);
    ~BB_VIGILANT_PNT_MGR (void) {};

    BB* Bb (void) const { return _bb; }

    /* Calc the initial num-of-pending-data-spec of given compenstion code*/
    void Update_Vp_For_Compensation_Code (OP* bookeeping);

    VIGILANT_PNT_LIST& Vigilant_point_list (void)
        { return _vigilant_pnt_list;}

    VIGILANT_PNT* Find (OP* op) {
        if (!VIGILANT_PNT::Candidate (op)) { return NULL; }
        for (VIGILANT_PNT_LIST_ITER iter = _vigilant_pnt_list.begin ();
             iter != _vigilant_pnt_list.end (); iter++) {
            VIGILANT_PNT* pnt = *iter;
            if (pnt->Op () == op) {
                return pnt;
            }
        }
        return NULL;
    }
    
    VIGILANT_PNT* Add_Vigilant_Point (OP* op, VIGILANT_PNT* vp=NULL) {
        Is_True (VIGILANT_PNT::Candidate (op), 
                 ("Op is not qualified to be a vigilant point"));
        Is_True (Find (op) == NULL, ("Duplicated vigilant point"));
        if (!vp) {
            vp = CXX_NEW(VIGILANT_PNT(op), _mp);
        }
        _vigilant_pnt_list.push_back (vp);
        return vp;
    }

    VIGILANT_PNT* Remove_Vigilant_Point (OP* op) {
        for (VIGILANT_PNT_LIST_ITER iter = _vigilant_pnt_list.begin ();
             iter != _vigilant_pnt_list.end (); iter++) {
            if ((*iter)->Op() == op) {
                VIGILANT_PNT* vp = *iter;
                _vigilant_pnt_list.erase(iter);
                return vp;
            }
        }
        return NULL;
    }

    VIGILANT_PNT& Vp_for_Exit (void) { return _exit_vp; } 
};

typedef mempool_allocator<BB_VIGILANT_PNT_MGR*> BB_VIGILANT_PNT_MGR_ALLOC;
typedef std::list<BB_VIGILANT_PNT_MGR*, BB_VIGILANT_PNT_MGR_ALLOC> 
    BB_VIGILANT_PNT_MGR_LIST;
typedef BB_VIGILANT_PNT_MGR_LIST::iterator BB_VIGILANT_PNT_MGR_LIST_ITER;

class DATA_SPEC_RES_CONSTRAIT_MGR {
private:
    MEM_POOL* _mp;
    REGION* _rgn; 
    BB_VIGILANT_PNT_MGR_LIST* _buckets[32];
    INT hash_idx (BB* blk) const {
        return BB_id(blk) % (sizeof(_buckets)/(sizeof(_buckets[0])));
    }

    BB_VIGILANT_PNT_MGR* Find (BB* blk) {
        INT idx = hash_idx (blk);
        for (BB_VIGILANT_PNT_MGR_LIST_ITER iter = _buckets[idx]->begin ();
             iter !=  _buckets[idx]->end(); iter++) {
            if ((*iter)->Bb() == blk) return *iter; 
        }
        return NULL;
    }

    void Update_Pending_Adv_Ld_Info 
        (BB* blk, OP* upperbound, OP* lowerbound, OP* sched_op);

    BOOL There_Exist_Misc_Dep_Arc (OP* pred, OP* succ);

    BOOL Check_Res_Constraint_Helper
        (BB* bb, OP* predbound, OP* succbound, OP* op);

public:
    DATA_SPEC_RES_CONSTRAIT_MGR (MEM_POOL* mp);
    ~DATA_SPEC_RES_CONSTRAIT_MGR (void) {}

    /* the init function for global and local scheduling respectively*/
    void Init (REGION* gscope);
    void Init (BB* lscope);

    /* Check to see whether data speculation will incur check-miss. 
     * 
     * NOTE: that the <src_info> hold some info on how <op> is going 
     *    to be scheduled: the src block, target-block, compensation 
     *    blocks and all other blocks that this schedule will move across.
     *    therefore, it is handy for the case when <op> is moved out of 
     *    its home block.
     */
    BOOL Check_Res_Constraint (REGION* rgn, OP* op, SRC_BB_INFO* src_info);
    BOOL Check_Res_Constraint (BB* bb, OP* op);

    VIGILANT_PNT* Get_Vigilant_Point (OP* op) 
        { return Find (OP_bb(op))->Find (op); }
    VIGILANT_PNT* Add_Vigilant_Point (OP* op,VIGILANT_PNT* vp=NULL)
        { return Find (OP_bb(op))->Add_Vigilant_Point (op, vp); }
    VIGILANT_PNT* Remove_Vigilant_Point (OP* op) {
          return Find (OP_bb(op))->Remove_Vigilant_Point (op);
        }
            
    /* Update the number of pending advanced load of the vigilant 
     * points in the area the schedule affects. 
     * 
     * NOTE: This function should be called before the schedule if 
     * <schedp> is physically performed in that we need to position
     * info to figures out those vigilants points <schedop>, in 
     * source block, which <schedop> move across 
     */
    void Update_Pending_Adv_Ld_Info (SRC_BB_INFO* srcinfo, OP* schedop);

    void Update_Vp_For_Compensation_Code 
        (OP* bookeeping, BB* compensation_blk);

    void Print (FILE* f);
};

#endif // SCHED_RES_AWARE_included
