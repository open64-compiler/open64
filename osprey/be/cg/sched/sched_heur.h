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
  * Module: sched_heur.h
  * $Revision: 1.1 $
  * $Date: 2005/12/30 01:50:23 $
  * $Author: weitang $
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_heur.h,v $
  *
  * Revision comments:
  * 
  * Description:
  * ============
  *     
  *     TODO: finish this comment
  *
  * =======================================================================
  * =======================================================================
  */
#ifndef sched_heur_INCLUDED
#define sched_heur_INCLUDED

#ifdef TARG_IA64
#include "targ_issue_port.h" /* for TSI_Issue_Ports */
#endif

/* STL template */
#include <vector> 
using std::vector;

#include "tracing.h"
#include "bb.h"
#include "op.h"
#include "errors.h"

/* memory management */
#include "mempool.h"
#include "mempool_allocator.h"

#include "region.h"
#include "sched_util.h"
#include "sched_cand.h"


#define COLD_PATH_EXEC_PROB  (0.1f)
#define HOT_PATH_EXEC_PROB   (0.7f)

#define DONATE_P_READY_CAND_BB_REACH_PROB (0.7f * REACH_PROB_SCALE)

/* The following two structures apply a e-time constraint to 
 * candidate's qualification.
 * 
 */
typedef enum {
    NO_LATER,
    AS_EARLY_AS_POSSIBLE, 
} E_TIME_CONSTRAIT ;

 /* the semantic of the combination of <threshold> and <constraint>
  *
  *  +--------------+-----------------------------------------------+
  *  | <constraint> |    qualified candidate(s)                     |
  *  +--------------+-----------------------------------------------+
  *  | NO_LATER     |  e_time of *UNTRIED* candidate is not later   |
  *  |              |  than <threshold>.                            |
  *  +--------------+------------------------+----------------------+
  *  | AS_EAR...BLE |    If there exist at   |                      |
  *  |              | least untried candidate|                      |
  *  |              | whose issue cycle is no|     otherwise        |
  *  |              | great than <threshold> |                      |
  *  |              +------------------------+----------------------+
  *  |              | all untried candidates | *UNTRIED* candidates |
  *  |              | whose issue cycle is   | whose issue cycle is |
  *  |              | no-great than          | EXACTLY equal to     |
  *  |              | <threshold>.           | min{ e_time(x) },    |
  *  |              |                        | where x are the      |
  *  |              |                        | untried candidates.  |
  *  +--------------+------------------------+----------------------+
  *
  */
typedef struct tagE_Time_Constraint {

    CYCLE threshold;
    E_TIME_CONSTRAIT constraint;

} E_Time_Constraint ;


#if defined(TARG_SL) || defined(TARG_SL2)
// following model kind is used to provide a candidate selection in round robin way. 
typedef enum 
{
  CAND_KIND_INTEGER,
  CAND_KIND_MEM,
  CAND_KIND_OTHER,
}  model_kind; 
#endif


class FAVOR_DELAY_HEUR {

private:
    MEM_POOL* _mp ;        /* underlying mempool */ 

    REGION* _rgn_scope ;   /* the global schedule scope */
    BB*     _bb_scope ;    /* the local  scheudle scope */
    BB*     _target_block; /* which BB we now deal with */
    BB*     _significant_pred;
    OP*     _xfer_op  ;    /* cntl-transfer op of <_target_block> */
    char*   _describe ;    /* the literal text to describe this heuristic 
                            */
    
    BOOL    _initialize ;  /* indicate whether this class has 
                            * initialized or not */
    BOOL    _stress_spec ; /* heuristic is used to stress speculation
                            * rather than to better expolit ILP, which 
                            * is used to expose dormant bugs.
                            */
#if defined(TARG_SL)
   BOOL  _size_spec;       /*local sched for code size purpose*/
   BOOL  _unpaired_16bit;  /*prev committed op is 16-bit, current candicate op should be 16 better*/
   LABEL_IDX _zdl_op_tag; /* op has tag of zero delay loop*/
#endif

#if defined(TARG_SL)
  std::list < model_kind > _cand_sel_model; 

// this flag is used to indicate if scheduler select candidate in round robin way
// according to model above 
  BOOL _round_robin_select_cand;  
#endif 
    RGN_CFLOW_MGR* _cflow_mgr;/* _cflow_mgr is used to query 
                               * cntl flow information
                               */
        /* data structure used to hold some information of OP. these 
         * information can help me to determine which OP/candidate
         * is the best one.
         */ 
    #define OP_HEUR_INFO_MAGIC_NUM (0x55aa) 
    typedef struct tagOP_HEUR_INFO {

        OP*    _op;
        BB*    _etime_set_by_which_bb ;
        INT32  _fan_out ;
        INT32  _issuable_port_num ;
        float  _delay ;
        CYCLE  _e_time ;
        mINT16 _magic_num ;

        tagOP_HEUR_INFO (OP *op) : _op(op) {
            _etime_set_by_which_bb = NULL;
            _delay      = 0.0f ;
            _e_time     = (CYCLE)0;
            _magic_num  = OP_HEUR_INFO_MAGIC_NUM ;

            _fan_out    = 0;
            _issuable_port_num = 0;

#ifdef TARG_IA64
            PORT_SET port_set = TSI_Issue_Ports(OP_code(op));
            while (port_set.Body()) {

                if (port_set.Body() & 1) { ++ _issuable_port_num ; }
                port_set = (PORT_SET)((port_set.Body() >> 1)); 
            }
#endif
        }

        void Dump (FILE *f=stderr,BOOL verbose=FALSE) ;
        
    } OP_HEUR_INFO ;

        /* data structure used to hold some information of BB. ;
         */
    typedef struct tagBB_HEUR_STUFF {

        mBOOL     _heur_need_adjust; 
        BB*       _bb;
        BB_OP_MAP _op_2_op_heur_info;
        MEM_POOL* _mp;

        void  Set_OP_Heur_Info (OP* op, OP_HEUR_INFO * info) {
                    
                    if (info) {
                        Is_True (op == info->_op, 
                                ("corrupted heuristic structure!"));

                        Is_True (OP_bb(info->_op) == _bb, 
                                 ("BB:%d is not op's home bb", 
                                  BB_id (OP_bb(info->_op))));
                    }

                    BB_OP_MAP_Set (_op_2_op_heur_info, op, (void*)info);
              }

        OP_HEUR_INFO * Get_OP_Heur_Info (OP * op) {
            return (OP_HEUR_INFO *)
                    BB_OP_MAP_Get (_op_2_op_heur_info, op);
        }

        tagBB_HEUR_STUFF (BB* bb, MEM_POOL *mp) :
                _bb(bb), _mp(mp) {

            _op_2_op_heur_info = BB_OP_MAP_Create (_bb, _mp);

            _heur_need_adjust = TRUE ;

            OP * op ;
            FOR_ALL_BB_OPs (_bb, op) {

                 OP_HEUR_INFO *tmp = CXX_NEW(OP_HEUR_INFO(op),_mp);
                 BB_OP_MAP_Set (_op_2_op_heur_info, op, tmp);
            }

        }
    } BB_HEUR_STUFF; 

    CFG_NODE_MAP  _heur_map; /* f: regional-cfg-node -> 
                              *    info-associated-with-this-code
                              */

    BOOL  _trace_cand_sel; /* turn on/off candidate selection process*/
    FILE* _trace_file;     /* dump the tracing text into this file*/ 

    typedef struct { BB* succ ; float reach_prob ; float max_delay ; 
                     INT32 flow_shift_latency ; 
                   } SUCC_INFO ;

    void    Alloc_Heur_Data (BB* bb);
    void    Alloc_Heur_Data (REGION* rgn);

    BB_HEUR_STUFF* Get_BB_Heur_Stuff (BB* bb) {
                        return (BB_HEUR_STUFF*)_heur_map.Get_Map (bb);
                   }
                    
    OP_HEUR_INFO*  Get_OP_Heur_Info (OP* op) {

                        BB_HEUR_STUFF* bb_heur_stuff = 
                                Get_BB_Heur_Stuff (OP_bb(op));

                        Is_True (bb_heur_stuff, 
                                 ("Fail to get BB:%d's heuristic stuff",
                                 BB_id(OP_bb(op))));

                        return  (OP_HEUR_INFO*)BB_OP_MAP_Get (
                                    bb_heur_stuff->_op_2_op_heur_info, op);
                    }


    void Reset_BB_OPs_etime (const BB_VECTOR *bbs) ;
    void Reset_BB_OPs_etime (BB *bb);
    void Set_OP_etime (OP* op, CYCLE cyc) {

            OP_HEUR_INFO* heur = Get_OP_Heur_Info (op);
            Is_True (heur, ("OP[%d] of BB:%d has no heuristic information")); 
            
            heur->_e_time = cyc ;
         }
    void Find_Significant_Pred_For_Target_Blk (void);
    void Adjust_Etime_For_Target_Block (void);

    BOOL BB_Need_Adjusting_Delay (BB* bb);
    void Set_BB_Need_Adjusting_Delay (BB* bb);
    void Set_BB_Need_Not_Adjusting_Delay (BB* bb);

    void Compute_Delay (OP* op, SUCC_INFO* succ_info, INT32 succ_num) ;
    void Compute_Delay (BB* bb);
    void Compute_Delay (REGION* rgn);
    
    void Adjust_Delay (BB* bb);
    void Adjust_Delay (REGION* rgn);

    void Compute_FanOut (OP* op);
    void Compute_FanOut_For_All_OP (BB* bb);
    void Compute_FanOut_For_All_OP (REGION* rgn);

    CYCLE Exclude_Unqualifed_Cand_Under_Etime_Constraint
                (CAND_LIST& cand_lst, E_Time_Constraint* constraint) ;

    CANDIDATE* Choose_Better_Of_Tie (
                    CANDIDATE*, OP_HEUR_INFO* , 
                    CANDIDATE* , OP_HEUR_INFO* ,
                    BOOL comes_from_targ_bb) ;

    CANDIDATE* Select_Best_Candidate (
                    CAND_LIST& cand_lst,
                    BB* targ,
                    E_Time_Constraint* etime_constraint);

    void Initialize (REGION* rgn);
    void Initialize (BB* bb);


    BOOL Is_In_Global_Scope (void) {
            Is_True (_initialize, ("have not been initialized!"));
            return _rgn_scope != NULL;
         }

            /* return TRUE iff we (cntl-)speculate <op> without 
             * trasforming its form is profitable. called only by 
             * <Spec_Code_Motion_Is_Profitable>.
             */
    inline BOOL Cntl_Spec_Ld_In_Normal_Form_Is_Profitable (OP* op) ;

    BOOL Upward_Code_Motion_Inc_Live_Range_Greatly 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info,
             RGN_CFLOW_MGR* cflow_info) ;

        /* stress speculation stuff :
         *
         *  Currently, some phases such as GRA LRA EBO etc are lack 
         *  of speculation awareness. Sometimes, these phases 
         *  get confused or are clumzy at the speculative OPs. 
         *  strees spculation stuff are used to trigger those doment 
         *  bugs, which are caused by speculation, in these phases.
         * 
         *  The stress-speculation is implemented in a transparent
         *  approach in heurstic-package. When stress-speculation 
         *  is turned on (option at command line "-Wb,-IPFEC:stress_spec=on")
         *  We try to speculate OPs even if there are non-speculative OPs     
         *  ready to be scheduled. 
         */ 
    CANDIDATE* Select_Best_Candidate_For_Stress_Spec_Purpose ( 
                      CAND_LIST& m_ready_cand_lst,
                      CAND_LIST& p_ready_cand_lst, 
                      BB*  targ,
                      E_Time_Constraint* etime_constraint);
                
    CANDIDATE* Select_Best_Candidate_For_Stress_Spec_Purpose (
                      CAND_LIST& cand_lst,
                      BB* targ,
                      E_Time_Constraint* etime_constraint);

public:
        
        /* =================================================
         *
         *    EXPORTED INTERFACE 
         *
         * ================================================
         */
    FAVOR_DELAY_HEUR (MEM_POOL *mp) ; 

    ~FAVOR_DELAY_HEUR (void) ;

        /*  Initialize and clean-up this class 
         */
    void Initialize (REGION *rgn,RGN_CFLOW_MGR* rgn_cflow_mgr) ;
    void Initialize (BB *bb,RGN_CFLOW_MGR* rgn_cflow_mgr) ;
    void Finialize  (void) ;

        /*  Select the best candidate from dual candidate list. 
         *
         *  Upon returning, etime_constraint->threshold is set 
         *  to the earilest cycle at which candidate can be issued,
         *  iff we have found the best candidate. and 
         *
         *  etime_constraint->threshold remain unchanged when 
         *  etime_constraint->constraint is AS_EARLY_AS_POSSIBLE.
         */
    CANDIDATE * Select_Best_Candidate (
                            CAND_LIST& m_ready_cand_lst,
                            CAND_LIST& p_ready_cand_lst, 
                            BB *       targ,
                            E_Time_Constraint * etime_constraint);

         /*  Get the <cand>'s earlist issue cycle 
          */
    CYCLE Get_Cand_Issue_Cyc (CANDIDATE *cand);

         /* Adjust heurstic stuff when target-bb shifts.
          */
    void Adjust_Heur_Stuff_When_BB_Changed   
           (BB * new_target,SRC_BB_MGR& src_bb_mgr);

         /* Adjust heuristic stuff after we schedule <cand> at
          * cycle <issue_cyc>.
          */
    void Adjust_Heur_After_Cand_Sched 
                    (OP *cand,CYCLE issue_cyc);

         /* Adjust heuristic stuff when there are no qualified 
          * candidate which can be issued at <issue_cyc>(and 
          * than we call SCHEDULER::Cycle_Advance().
          * 
          * <op_vect> contains all OPs that have been issued at 
          * current cycle <issue_cyc>.
          */
    void Adjust_Heur_After_Sched_One_Cyc 
                    (OP_Vector& op_vect, CYCLE issue_cyc);

          /* Compute heuristic data associated with <op> which is 
           * currently inserted, appended or prepended to OP_bb(op)
           */
    void Compute_Heur_Data_For_Inserted_OP   (OP* op) ;
    void Compute_Heur_Data_For_Appended_OP   (OP* op) ;
    void Compute_Heur_Data_For_Prepended_OP  (OP* op);

          /* Cut off the association between <op> and its heuristic data
           */
    void * Detach_OP_Heur_Info (OP* op) ;

          /* Bind <op> with <Heur_Data> which is now between <op>'s 
           * heuristic data
           */
    void   Attach_OP_Heur_Info (OP* op, void* Heur_Data);

    BOOL Upward_Global_Sched_Inc_Live_Range_Greatly 
            (CANDIDATE* cand,
             SRC_BB_INFO* bb_info,
             RGN_CFLOW_MGR* cflow_info);

    BOOL Upward_Spec_Global_Sched_Is_Profitable 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info,
             RGN_CFLOW_MGR* cflow_info);

    BOOL Upward_Useful_Sched_Is_Profitable 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info, 
             RGN_CFLOW_MGR* cflow_info) ;

    BOOL Upward_Global_Sched_Is_Profitable 
            (CANDIDATE* cand, 
             SRC_BB_INFO* bb_info, 
             RGN_CFLOW_MGR* cflow_info);
    
    BOOL Renaming_Is_Profitable(CANDIDATE * cand);

          /* check to see whether it is the right time for scheudler to 
           * close the code motion of <_target_block>.   
           */
    BOOL  It_is_Better_No_New_Cycle_For_Cur_BB (void) ;
    BOOL  BB_Can_Potentially_Donate_P_Ready_Cand (BB* src, BB* targ);

          /* misc */
    void Estimate_Cand_Etime (OP* op) ;

          /* tracing facility */
    void  Dump_OP_Heur_Info (OP* op, FILE* f=stderr,
                             BOOL verbose=FALSE);
    void  Dump_OP_Heur_Info (BB* bb, FILE* f=stderr,
                             BOOL verbose=FALSE); 
    void  Dump (FILE* f=stderr, BOOL verbose=TRUE);
    
          /* trace candidate selection process 
           */
    void Enable_Trace_Cand_Sel_Process (FILE* f) ;
    void Disable_Trace_Cand_Sel_Process (void);
    BOOL Trace_Cand_Sel_Enabled (void) { return  _trace_cand_sel; }
    void Trace_Cand_Sel_Process (const char* fmt, ...); 

#if defined(TARG_SL)
    void Set_Sched_For_CodeSize_Spec() { _size_spec = TRUE; }
    BOOL Sched_For_Codesize_Spec() { return _size_spec; }
    void Reset_Sched_For_CodeSize_Spec() { _size_spec = FALSE; }
    void Set_prev_unpaired_16bit() { _unpaired_16bit = TRUE; }
    BOOL Prev_unpaired_16bit() { return _unpaired_16bit; }
    void Reset_prev_unpaired_16bit() { _unpaired_16bit = FALSE; }
    void Set_ZDL_Last_OP_Tag (LABEL_IDX tag) { _zdl_op_tag = tag; }
    LABEL_IDX Get_ZDL_Op_Tag () { return _zdl_op_tag; }
    void Reset_ZDL_Last_OP_Tag() { _zdl_op_tag = 0; }
#endif

#if defined(TARG_SL) || defined(TARG_SL2)
    void Print_All_Models(FILE*);
    BOOL Round_Robin_Cand_Sel() { return _round_robin_select_cand; }
    BOOL Enable_Round_Robin_Cand_Sel() { _round_robin_select_cand = TRUE; }
    BOOL Disable_Round_Robin_Cand_Sel() { _round_robin_select_cand = FALSE; }
    BOOL Cand_Is_Expected(CANDIDATE * cand); 
    void   Update_Cand_Sel_Model_Status(); 
    void  Get_Round_Robin_Model(); 
#endif

#ifdef Is_True_On     
    void gdb_dump(void);
#endif 

};


#endif /* sched_heur_INCLUDED */
