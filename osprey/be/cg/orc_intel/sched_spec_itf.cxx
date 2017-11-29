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

#include "scheduler.h"
#include "sched_spec_itf.h"

    /* ======================================================================
     * ======================================================================
     *
     *  Module sched_spec_itf.cxx
     *
     *  $Revision: 1.1.1.1 $
     *  $Date: 2005/10/21 19:00:00 $
     *  $Author: marcel $
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
 *  (speculation.{cxx,h}, recovery.{cxx,h}). 
 *   
 * ====================================================================
 */
BOOL
SCHED_SPEC_HANDSHAKE :: Change_Load_Spec_Form 
    (CANDIDATE* cand, INT32 cutting_set_size, 
     BOOL* insert_chk, SCHEDULER* sched, 
     ISA_ENUM_CLASS_VALUE* ldform, /* the form <cand> should be changed to*/
     BOOL test /*if TRUE, don't do actual transform upon <cand>*/) {

    *insert_chk = FALSE;
    if (ldform) {*ldform = ECV_UNDEFINED;};

        /* (a): rule out cases that need not transform 
         */
    OP* op = cand->Op ();
    if (!OP_load(op) || !cand->Is_Spec ()) {
        return FALSE; /* obviously */ 
    }

    // fix bug no. OSP_76 for implicit use of Actuals 
    for (OP* prev_op = OP_prev(op); prev_op; prev_op = OP_prev(prev_op)){
        if (!OP_Scheduled(prev_op) && OP_ANNOT_OP_Def_Actual_Para (prev_op))
            return FALSE;
    }

    /* ignore some speculation type since they do not entail transformation
     * up the given load.
     */
    SPEC_TYPE spec_type = cand->Spec_Type();
    if (OP_no_alias (op)) {
        spec_type = SPEC_TYPE(spec_type & ~SPEC_DATA); 
    }
    if (SPEC_TYPE(spec_type & SPEC_CNTL) == SPEC_CNTL &&
        Load_Has_Valid_Vaddr (op)) {
        spec_type = SPEC_TYPE(spec_type & ~SPEC_CNTL); 
    }
    if (spec_type == SPEC_NONE) {
        return FALSE;
    }

    Is_True (!OP_Can_not_be_Candidate (op, spec_type), 
            ("Load cannot be further speculated due to some limitations"));
  
    if (CGTARG_Is_OP_Speculative(op) ||
        CGTARG_Is_OP_Advanced_Load (op) ||
        CGTARG_Is_OP_Check_Load (op)) {
         /* OP_Can_not_be_Candidate () guarantees that 
          *   a cntl/data speculated but not data/cntl speculated load
          *  cannot be further data/cntl speculated. and 
          *  a check load cannot be further speculatd.
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
        *insert_chk = TRUE; 

        if (ldform) { 
            *ldform = OP_Is_Float_Mem(op) ? ECV_fldtype_sa: ECV_ldtype_sa;
        }

        if (!test) {
            Change_ld_Form (op, OP_Is_Float_Mem(op) ? 
                            ECV_fldtype_sa: ECV_ldtype_sa);
        }
        return TRUE;
    }

    Is_True (spec_type != SPEC_COMB, 
        ("sepculation type can't be SPEC_COMB"));

        /* (c): transform to .a IF NECESSARY 
         */
    if (spec_type & SPEC_DATA) {
        if (ldform) { 
            *ldform = OP_Is_Float_Mem(op) ? ECV_fldtype_a: ECV_ldtype_a;
        }
        if (!test) {
            Change_ld_Form(op, OP_Is_Float_Mem(op) ? ECV_fldtype_a: ECV_ldtype_a);
        }
        return *insert_chk = TRUE;
    }
    
        /* (d): transform to .s IF NECESSARY 
         */
    Is_True (spec_type & SPEC_CNTL, 
           ("speculation type should be SPEC_CNTL"));

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

    if (ldform) *ldform = OP_Is_Float_Mem(op) ? ECV_fldtype_s : ECV_ldtype_s;
    if (!test) {
        Change_ld_Form(op, OP_Is_Float_Mem(op) ? ECV_fldtype_s : ECV_ldtype_s);
    }
    return *insert_chk = TRUE;
}

/* ====================================================================
 * 
 *  OP_Can_not_be_Candidate 
 *
 *  This function encapsulate the limitations of speculation supporting
 * routings for the scheduler. Currently, following scenarios are not 
 * supported by speculation supporting routines. As GanGe told me, one 
 * of the reasons is that it is hard to update the speculative chain.
 * The scenarios are:
 *
 *   - a data speculated but not control speculated load is going to 
 *     be control speculated.
 *   - a control speculated but not data speculatd load is going to be 
 *     data speculatd. 
 *   - a check-load cannot be further data or control speculated.
 * 
 * This function return TRUE iff the one of above scenarios are met. 
 * The input parameter <spec> specify how the load <op> is further 
 * speculated.
 * 
 * ====================================================================
 */
BOOL
SCHED_SPEC_HANDSHAKE :: OP_Can_not_be_Candidate (OP* op, SPEC_TYPE spec_type) {

    if (!OP_load (op) || spec_type == SPEC_NONE) {
        return FALSE;
    }

    /* Get load's current form */
    BOOL in_cntl_spec_form = CGTARG_Is_OP_Speculative_Load(op);
    BOOL in_data_spec_form = CGTARG_Is_OP_Advanced_Load(op);
    BOOL is_check_ld = CGTARG_Is_OP_Check_Load(op);

    if (!in_cntl_spec_form && !in_data_spec_form && !is_check_ld) {
       return FALSE; 
    }

    if (is_check_ld) {
       /* a check load cannot be further speculated */
       return TRUE;
    }

    /* ignore data or control specultion if it does not entail
     * transformation upon the load.
     */
    if (OP_no_alias (op)) {
        spec_type = SPEC_TYPE(spec_type & ~SPEC_DATA); 
    }

    if (SPEC_TYPE(spec_type & SPEC_CNTL) == SPEC_CNTL &&
        Load_Has_Valid_Vaddr (op)) {
        spec_type = SPEC_TYPE(spec_type & ~SPEC_CNTL); 
    }

    BOOL to_be_cntl_spec = (spec_type == SPEC_CNTL || 
                            spec_type == SPEC_COMB);
    BOOL to_be_data_spec = (spec_type == SPEC_DATA ||
                            spec_type == SPEC_COMB);

    if (!in_cntl_spec_form && to_be_cntl_spec ||
        !in_data_spec_form && to_be_data_spec) {
        return TRUE;  
    }
   
    return FALSE;
}
