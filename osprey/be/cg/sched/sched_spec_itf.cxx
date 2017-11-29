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
#include "cgtarget.h"
#include "scheduler.h"
#include "sched_spec_itf.h"

    /* ======================================================================
     * ======================================================================
     *
     *  Module sched_spec_itf.cxx
     *
     *  $Revision: 1.1 $
     *  $Date: 2005/12/30 01:50:23 $
     *  $Author: weitang $
     * 
     *  Description: 
     *  ============
     * 
     *  This module serves as handshake between scheduler and speculation-
     *  package src/osprey1.0/be/cg/orc_intel/{speculation.*,recovery.*}
     * 
     *  We centralize all workarounds to spec-package here to prevent it     
     *  from scattering anywhere in scheduling phase.  
     * 
     * ======================================================================
     * ======================================================================
     */ 


/* ====================================================================
 *
 *  Change_Load_Spec_Form
 *
 *  Change load to be speculative form IF NECESSARY. 
 *  This function do some ugly things for Commit_Schedule(), whichby
 *  to keep Commit_Schedule() concise.
 * 
 *
 *  - return FALSE if <cand> remains unchanged, TRUE otherwise. 
 *
 *  - Insert_chk is set TRUE, if extra chk instruction is required. 
 *    We need not to point out which kind of chk is expected, 
 *    since spec-package is highly adaptive -- the author of 
 *    spec-package told me so, but I have not examine how it work. 
 *
 *  - ***** <cutting_set_size> ****  
 *    cutting_set_size = (the number of compensation blocks + 1).
 *    Here, "1" corresponds to the target block(or,<_target_bb>)
 *
 *    The seem-to-be-extraneous parameter is required by the 
 *    spec-package to determine the final form the load: if 
 *    cutting-set_size > 1, this function will uniformaly 
 *    change the <cand> to be the form of *.sa reguardless 
 *    the spec-type of <cand> is cntl-spec or data-spec unless
 *    <IPFEC_Enable_Data_Speculation> is turned off. 
 *    
 *  There are much painful experence of this little function which 
 *  serve as (a part of) handshake between scheduler and spec-package
 *  (speculation.{cxx,h}, recovery.{cxx,h}). It has ever create 
 *  10**10000 bugs. change that number if you ever encounter one. 
 *  10**10000++, 5/20/2003
 *   
 * ====================================================================
 */
BOOL
SCHED_SPEC_HANDSHAKE :: Change_Load_Spec_Form 
    (CANDIDATE* cand, INT32 cutting_set_size, 
     BOOL* insert_chk, SCHEDULER* sched) {

#if !defined(TARG_IA64) 
    *insert_chk = FALSE;

    /* suppress compilation complain */
    cand = cand; 
    cutting_set_size = cutting_set_size;
    sched = sched;
    return FALSE;
#else
    *insert_chk = FALSE;

        /* (a): rule out cases that need not transform 
         */
    OP* op = cand->Op ();
    if (!OP_load(op) || !cand->Is_Spec ()) {
        return FALSE; /* obviously */ 
    }

    if (CGTARG_Is_OP_Speculative_Load(op)) {
         /* What about ld.s => ld.sa ? 
          *
          * For ld.s: we can not
          * if you have a ld.s, you should have a chk.s and
          * a speculative chain between them. if you want to change
          * ld.s to ld.sa, you should change chk.s to chk.a at the 
          * same time. This may not be difficult. But how about the 
          * speculative chain? It is not easy to despeculate all 
          * OPs on the speculative chain and control-speculate 
          * them again.
          *
          * For ld.sa: we need not
          * need not transform a ld.sa
          */
        return FALSE;
    }

    if (CGTARG_Is_OP_Check_Load(op)) {
         /* What about ld.a => ld.sa ? 
          * Can never meet ld.c here.
          */
        return FALSE;
    }

    SPEC_TYPE spec_type = cand->Spec_Type ();
    if ((spec_type & SPEC_DATA) && OP_no_alias(op)) {
        DevWarn ("data speculate no_alias OP[%d] of BB:%d", 
                  OP_map_idx(op), BB_id(OP_bb(op)));
        spec_type = SPEC_TYPE(spec_type & SPEC_CNTL); /* remove SPEC_DATA */
    }


    if (Load_Has_Valid_Vaddr (op) && !(spec_type & SPEC_DATA)) {
            /* load always has valid virtual address and it 
             * does not being moved beyond any aliasing stores. 
             *
             * NOTE: Load_Has_Valid_Vaddr () does not imply that no OPs
             *    alias with this op.  
             */
        return FALSE; 
    }

        /* (b): tranform to .sa IF NECESSARY 
         */
    BOOL to_dot_sa_form = (spec_type == SPEC_COMB);

    if (IPFEC_Enable_Data_Speculation && !OP_no_alias(op)) {
        /* In this case, a single load may be speculated to multiple
         * paths, causing a bunch of speculative loads. They share one
         * common check, either chk.s or chk.a. Once the spec_type is
         * fixed at a pure cntl_spec, we should insert a chk.s. According
         * to the reason described in line 109, we should gurantee those
         * control speculative load being not speculated across any store.
         * We can't make sure this and, further, we may lose many data
         * speculation opportunities. So, I decide to give them a cntl&data
         * speculative form. Hence, they can be speculated across "any" 
         * instructions. 
         */
        if (cutting_set_size > 1) { 
            to_dot_sa_form = TRUE;
        }
    }
         
    if (to_dot_sa_form) {
        //  need not insert check for a speculative load again.
        if (!CGTARG_Is_OP_Speculative(op)) {        
            *insert_chk = TRUE; 
        }
        Change_ld_Form(op, ECV_ldtype_sa);
        return TRUE;
    }

    Is_True (spec_type != SPEC_COMB, 
        ("sepculation type can't be SPEC_COMB"));

        /* (c): transform to .a IF NECESSARY 
         */
    if (spec_type & SPEC_DATA) {
        if (!CGTARG_Is_OP_Advanced_Load(op)) {
            Change_ld_Form(op, ECV_ldtype_a);
            return *insert_chk = TRUE;
        } else {
                /* already in *.a form */
            return FALSE; 
        }
    }
    
        /* (d): transform to .s IF NECESSARY 
         */
    Is_True (spec_type & SPEC_CNTL, 
           ("speculation type should be SPEC_CNTL"));

    if (CGTARG_Is_OP_Speculative (op)) {
        /* already in .s form */
        return FALSE;
    }

    if (cutting_set_size == 1 && cand->Is_M_Ready () &&
        Is_Control_Speculation_Gratuitous 
            (op, sched->_target_bb, sched->_frontier_op)) {
            /* Why this condition? 
             * ld ..=[r4]
             * ld.s ..=[r4]
             * the second load can be a safe load.
             */
        return FALSE;
    }

    Change_ld_Form(op, ECV_ldtype_s);
    return *insert_chk = TRUE;
#endif
}

BOOL
SCHED_SPEC_HANDSHAKE :: OP_Can_not_be_Candidate 
        (OP* op, SPEC_TYPE spec_ty) {
#ifdef TARG_IA64
    if ((spec_ty & SPEC_DATA)  &&  OP_load(op) && 
         CGTARG_Is_OP_Speculative_Load (op) && 
         !CGTARG_Is_OP_Advanced_Load (op)) {
         return TRUE;	
	}
    return FALSE;
#else
    if (spec_ty & SPEC_DATA ||
        (spec_ty & SPEC_CNTL) && CGTARG_Can_Be_Speculative(op)) {
        return TRUE;
    }
                    
    return FALSE;
#endif
}
