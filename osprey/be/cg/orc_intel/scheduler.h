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
  * Module: sched_cflow.h
  * $Revision: 1.1.1.1 $
  * $Date: 2005/10/21 19:00:00 $
  * $Author: marcel $
  *
  * Revision comments:
  *
  * ======================================================================
  * ======================================================================
  */
#ifndef scheduler_INCLUDED
#define scheduler_INCLUDED

    /* Using STL template 
     */
#include <ext/slist> 
#include <list>
#include <vector> 
#include <map>

    /* memory management 
     */
#include "mempool.h"
#include "mempool_allocator.h"

    /* definition used globally in backend 
     */
#include "ipfec_defs.h"

    /* tracing facility and commomly used data structure and 
     * utilities
     */
#include "tracing.h"
#include "bb.h"
#include "bb_set.h"
#include "op.h"
#include "prdb.h"
#include "tn.h"

    /* dependence DAG 
     */
#include "dag.h"

    /* communication with microscheduling and region packages 
     */
#include "targ_issue_port.h"
#include "cggrp_microsched.h"
#include "region.h"
#include "region_verify.h"


#include "sched_util.h"
#include "sched_cflow.h"
#include "sched_dflow.h"
#include "sched_heur.h"
#include "sched_cand.h"
#include "sched_res_aware.h"

    /* exported interfaces of this package 
     */
extern void Global_Insn_Sched (REGION_TREE& region_tree);
extern void Local_Insn_Sched  (BOOL);
extern void Global_Insn_Merge_Splitted_BBs () ;
extern void Clean_Up          (BB* bb);


class SCHED_MEM {
protected:
    MEM_POOL _mem_pool;

    SCHED_MEM () {
        MEM_POOL_Initialize (&_mem_pool, "SCHED_MEM", FALSE);
        MEM_POOL_Push (&_mem_pool);
    }

    ~SCHED_MEM () {
        MEM_POOL_Pop (&_mem_pool);
        MEM_POOL_Delete (&_mem_pool);
    }

public:

};


class SCHEDULER : public SCHED_MEM {

friend void Global_Insn_Sched (REGION_TREE* rgn_tree, BOOL prepass) ;
friend class SCHED_SPEC_HANDSHAKE;

private:

    /* dependence DAG 
     */
    DAG_BUILDER _dag_constructor;
    void Gen_Bookeeping_OP_DAG 
            (CANDIDATE& cand, OP* compensate, BOOKEEPING* bk);
    void Gen_Inverted_Arc (CANDIDATE* cand,ARC* ref_arc,
                           OP* pred, OP* succ);
    void Maintain_Dep_Arcs_After_Sched (CANDIDATE* cand);
    void Maintain_Dep_Arcs_After_Cntl_Spec_If_Converted_Code
		    (OP* sched_op, TN* sched_qp, OP* cmp_op);

        /* candidate stuff 
         */
    CAND_MGR _cand_mgr;
    DATA_SPEC_RES_CONSTRAIT_MGR _dsrmgr;

                /* find out all candidate which are potentially 
                 * scheduled into <_target_bb> */
    void Find_All_Candidates (void) ; 

                /* Add <op> to candidate list if it is qualified 
                 * being one by calling Try_Add_OP_to_Candidate_List(). 
                 * Other routines are supportings for this purpose.
                 */ 
    BOOL OP_Cannot_be_Candidate_Since_Obvious_Reason (OP* op) ;
    BOOL Succ_Pred_Transposed_If_Sched (ARC* arc, BB_VECTOR* cs);
    BOOL Collect_And_Analyse_Unresolved_Dep 
                (CANDIDATE* cand, SRC_BB_INFO* bb_info);
    BOOL Collect_And_Analyse_Other_Than_Dep_Constraints 
                (CANDIDATE* cand, SRC_BB_INFO* bb_info);
    void Determine_Non_P_Ready_Bookeeping_Places 
                (CANDIDATE* cand, SRC_BB_INFO* bb_info) ;
    BOOL Try_Add_OP_to_Candidate_List (OP* op);

                /* update candidate list  o. after one candidate 
                 *     has been schedule. o. right after cycle 
                 *     advance.
                 */
    void Update_Cand_Lst_During_Sched_Cyc (CANDIDATE& cand);
    void Update_Cand_Lst_After_Cycle_Advancing (void) ;
    void Update_Cand_Lst_After_Cntl_Spec_If_Converted_Code
		    (OP* sched_op, TN* sched_qp);
    



        /* speculation stuff 
         */ 

                /* check to see whether <op> can be moved out of 
                 * its home block */
    BOOL OP_Cannot_Be_Moved_Outof_HomeBB (OP * op) ;
        		/* control speculation of if-converted code stuff
        		 */
    BOOL OP_QP_Cannot_Be_Removed_By_Cntl_Spec (OP* op);
    BOOL Can_Cntl_Spec_If_Converted_Code(CANDIDATE *cand);
        
                /* return the speculation type that we can not 
                 * apply upon <op> */
    SPEC_TYPE Get_OP_Prohibited_Spec_Type (OP * op) ;
                
                /* identify all OPs in <bb> or <rgn> that could not 
                 * be speculated */ 
    void Identify_Cannot_Spec_OPs (BB * bb    ) ;
    void Identify_Cannot_Spec_OPs (REGION *rgn) ;


        /* heuristic stuff 
         */
    void    Determine_P_Ready_is_Profitable_or_not (void) {}
    void    Disable_P_Pready_Cand (void) { } ;
    BOOL    P_READY_Cand_Disabled (void) { return TRUE; }

    	/* renaming stuff
    	 */
	void	Renaming(CANDIDATE* cand);
	void	Maintain_Dep_Arcs_After_Renaming (OP* renamed_op, OP* copy_op);

        /* Commit schedule: all routines other than <Commit_Schedule> is 
         *                  only called by it. 
         */
    void  BB_Move_Op_Before (BB* to_bb, OP* point, BB* from_bb, OP* op) ;
    OP*   Insert_Check (OP* ld, BB* home_bb, OP* pos);
    OP*   Gen_Compensation_Code 
                (CANDIDATE& model_cand, BB* org_home, 
                 BOOKEEPING* bk, BOOL append=TRUE);

    void Cycle_Advance   (void);
    BOOL Commit_Schedule (CANDIDATE& cand);


        /* workaround for gp problem 
         */
    void Preprocess_GP_def_op  (void) ;
    void Postprocess_GP_def_op (void) ;

    OP*  _gp_def_op ; 
    ARC_LIST* _dep_upon_this_op ;

    BOOL OP_def_GP (OP* op) ;

        /* workaround for actual-argument problem 
         */
    void Identify_Actual_Argument_Defs (BB* bb);


        /*  schedule status and context
         */
    const BOOL _prepass;   /* before or after reg allocation */ 
    const BOOL _global;    /* flag indicate schedule scope: within region 
                          * confined within single BB */
    REGION*  _region;     /* the global code motion scope */
    BB*      _bb;         /* the local code motion scope */


    BB*    _target_bb;    /* the very block scheduler currently deals with */

    CYCLE  _cur_cyc;      /* current cycle */
                          /* this many instructions have been hoisted
                           * or sunk when schedule <_target_bb>
                           */ 
    mINT16 _upward_motion_num, _downward_motion_num;  
    mINT16 _sched_times; /* how many schedule performed upon <_target_bb>.*/
    enum { MAX_SCHED_TIMES = 2, };

    OP*  _frontier_op;  /* boundary OP between unscheduled and 
                          * scheduled OPs in <_target_bb> */ 
    OP_Vector _ops_in_cur_cyc; /* ops scheduled in current cycle, 
                                * not including nops
                                */
    SRC_BB_MGR _src_bb_mgr ; /* these BBs potentially donate candidates to 
                              * <_target_bb> */

                  /* specify all blocks that multiway-branch (leading from 
                   * the last cycle of <_target_bb>) spans.
                   */ 
    BB_VECTOR _multiway_br_span_bbs;
                
                  /* the blocks that potentially is part of multiway branch.
                   */
    BB_SET* _multiway_br_candidates;

    void Init_Sched_Status (void) ;
    void Adjust_Status_For_Resched (void);


    RGN_CFLOW_MGR    _cflow_mgr ;
    SCHED_DFLOW_MGR  _dflow_mgr ;
    FAVOR_DELAY_HEUR _heur_mgr;

        /* Bug workaround 
         */
    inline void Bug_Workaround_After_Schedule_BB (void) ;
    inline void Bug_Workaround_Before_Schedule_BB (void) ;


        /* schedule on cycle basis 
         */
    void Schedule_Cycle (void); 
    BOOL No_New_Cycle (void);

        /* schedule REGION
         */
    BOOL Sched_Rgn_Preproc  (void);
    void Sched_Rgn_Postproc (void);

    void Glos_Sched_BB_Postproc (void) ;
    void Glos_Sched_BB_Preproc  (void) ;
    BOOL Glos_Should_Sched_This_BB (BB *b);
    BOOL Need_Resched_To_Obtain_Better_Performance (void);

    SCHEDULER (struct tagRGN_INFO * rgn_info, BOOL before_regalloc , 
               PRDB_GEN * prdb=NULL);
    void Schedule_Region (void);


        /* tracing and debugging supports
         */
    void Verify (void) ;
    void Dump_OP_Verbose_Info (OP* op, FILE * f=stderr);


        /* exported interface */

public:

    SCHEDULER (BB* bb, BOOL before_regalloc,PRDB_GEN *prdb=NULL) ;
    ~SCHEDULER (void) ;

    void  Schedule_BB     (void);

    /* Tracing and debugging 
     */
    void Dump (FILE* file = stderr);
    void Dump_DAG (FILE *f=stderr );
    void Dump_IR  (FILE *f=stderr );
};

#endif
