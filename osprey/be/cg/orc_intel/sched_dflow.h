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
#ifndef sched_dflow_INCLUDED
#define sched_dflow_INCLUDED

 /* =========================================================================
  * =========================================================================
  * 
  * Module: sched_dflow.h
  * $Revision: 1.1.1.1 $
  * $Date: 2005/10/21 19:00:00 $
  * $Author: marcel $
  * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/sched_dflow.h,v $
  *
  * Description:
  * ============
  *
  *   This module implement data-flow-related functionality on behalf 
  *   of scheduler.
  *
  *   Exported interface:
  *
  *   o. class SCHED_DFLOW_MGR 
  *
  *         - SCHED_DFLOW_MGR (void) 
  *         - ~SCHED_DFLOW_MGR (void) 
  *             constructor & destructor
  * 
  *  
  *         - BOOL Are_Defs_Live_Out  (OP * op, BB * bb) 
  *             return TRUE iff one of results that <op> defines live out <bb>
  *        
  *         - BOOL Are_Defs_Live_Out  (OP * op, BB_VECTOR* bbv)
  *             return TRUE iff one of results that <op> defines live out of
  *             at least one element in <bbv>.
  *  
  *         - BOOL Are_Defs_Live_In   (OP * op, BB * bb);
  *             check to see whether one of the results that <op> defines live 
  *             at the entry point of <bb>
  *
  *         - void Add_Defs_Live_Out (OP * op, BB * bb)
  *             make *ALL* results that <op> defines live out of <bb>
  *
  *         - void Add_Defs_Live_In (OP * op, BB * bb) 
  *             make *ALL* results that <op> defines live into <bb>
  *
  *         - void Update_Liveness_After_Upward_Sched
  *                  (OP *op, SRC_BB_INFO * src_info)
  *             this routine maintain the liveness info properly after 
  *             upward code motion.
  *
  *         - void Update_Liveness_After_Downward_Sched
  *                  (OP *op, SRC_BB_INFO * src_info);
  *             this routines is dedicated to maintain the liveness info
  *             properly after downward code motion
  *
  *         - BOOL Upward_Sched_Kill_Some_LiveOut_Defs 
  *         - BOOL Downard_Sched_Kill_Some_LiveIn_Defs 
  *             check to see whether the upward code motion of <op>, 
  *             (from <from> to <to> and cutting_set is specified by 
  *              <cutting_set> will kill some live-out definitions.
  * 
  * 
  *         - BOOL Upward_Sched_Violate_Dflow_Constrait
  *         - BOOL Downward_Sched_Violate_Dflow_Constrait 
  *             TODO: finish this comments
  * 
  * ==========================================================================
  * ==========================================================================
  */

#include "ipfec_defs.h"
#include "sched_util.h"
#include "sched_path.h"
#include "sched_cflow.h"
#include "sched_cand.h"

    /* =========================================================
     * =========================================================
     *
     *              classs SCHED_DFLOW_MGR 
     *
     * =========================================================
     * =========================================================
     */ 
class SRC_BB_INFO;
class CANDIDATE;
class SCHED_DFLOW_MGR {
private:

    BOOL Upward_Sched_Kill_Def_LiveOut_Of_Target_BB
            (CANDIDATE* cand, SRC_BB_INFO* bb_info,
             RGN_CFLOW_MGR* cflow_info);

    BOOL Upward_Sched_Kill_Def_LiveOut_Of_Bookeeping_Place 
            (CANDIDATE* cand, BOOKEEPING* bk,
             SRC_BB_INFO* bb_info, RGN_CFLOW_MGR* cflow_info) ;

    BOOL P_Ready_Moving_Against_These_Path_Kill_Live_Defs 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info, BB* to,
             EXEC_PATH_SET* move_against, RGN_CFLOW_MGR* cflow_info);


        /* Exported interface */
public :

    SCHED_DFLOW_MGR  (void) ;
    ~SCHED_DFLOW_MGR (void) ;

    BOOL Are_Defs_Live_In (OP* op, BB* bb);
    BOOL Are_Defs_Live_In (OP* op, REGION* r);
    BOOL Are_Defs_Live_In (OP* op, REGIONAL_CFG_NODE* n) {
                return n->Is_Region () ? 
                        Are_Defs_Live_In (op,n->Region_Node ()) :
                        Are_Defs_Live_In (op,n->BB_Node ()) ;
         }

    BOOL Are_Defs_Live_Out (OP* op, BB* bb) ;
    BOOL Are_Defs_Live_Out (OP* op, REGION* r);
    BOOL Are_Defs_Live_Out (OP* op, REGIONAL_CFG_NODE* n) {
                return n->Is_Region () ? 
                        Are_Defs_Live_Out (op,n->Region_Node ()) :
                        Are_Defs_Live_Out (op,n->BB_Node ());
         }

    BOOL Are_Defs_Live_Out  (OP* op, BB_VECTOR* bbv) ;

    
    void Add_Defs_Live_Out  (OP* op, BB* bb);
    void Add_Defs_Live_Out  (OP* op, REGION* rgn);
    void Add_Defs_Live_In   (OP* op, BB* bb) ;
    void Add_Defs_Live_In   (OP* op, REGION* rgn);
    
        /* update liveness after upward/downward code motion.
         */
    void Update_Liveness_After_Upward_Sched
            (CANDIDATE* cand, SRC_BB_INFO* src_info,
             RGN_CFLOW_MGR* cflow_info);

    void Update_Liveness_After_Downward_Sched
            (CANDIDATE* cand, SRC_BB_INFO * src_info, 
             RGN_CFLOW_MGR* cflow_info);


        /* group1: Check to see whether upward code motion kill some 
         * liveout/livein defs.
         */
    BOOL Upward_Sched_Kill_LiveOut_Defs 
            (CANDIDATE* cand, SRC_BB_INFO* src_bb_info, 
             RGN_CFLOW_MGR* cflow_info);

    BOOL Downard_Sched_Kill_LiveIn_Defs 
            (CANDIDATE* cand, SRC_BB_INFO* src_bb_info, 
             RGN_CFLOW_MGR* cflow_info);
        


        /* group2: Check to see whether upward/downward code motion 
         * create interference with live-ranges leading from nested 
         * REGIONs.
         */
    BOOL Upward_Sched_Interfere_Nested_Rgns_LiveRanges 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info);

    BOOL Downward_Sched_Interfere_Nested_Rgns_LiveRanges 
            (CANDIDATE* cand, SRC_BB_INFO* src_bb_info);


        /* group3 : Check to see whether instruction alias with OPs in 
         * enclosed by nested rgns.
         */
    BOOL Upward_Sched_Alias_With_Nested_Rgn_OPs 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info);

    BOOL Downward_Sched_Alias_With_Nested_Rgn_OPs 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info);


        /* group 4: functionaly combine group1 through 3 inclusively
         */
    BOOL Upward_Sched_Violate_Dflow_Constrait
            (CANDIDATE* cand, SRC_BB_INFO* bb_info);

    BOOL Downward_Sched_Violate_Dflow_Constrait 
            (CANDIDATE* cand, SRC_BB_INFO* bb_info);
};

#endif /* sched_dflow_INCLUDED */
