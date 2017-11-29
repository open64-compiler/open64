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
     *  Module sched_spec_itf.cxx
     *
     *  $Revision: 1.1 $
     *  $Date: 2005/12/30 01:50:23 $
     *  $Author: weitang $
     *  $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_spec_itf.h,v $
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
#ifndef sched_spec_handshake_INCLUDED 
#define sched_spec_handshake_INCLUDED 

#include "defs.h"
#include "ipfec_defs.h"
#include "ipfec_options.h"
#include "sched_util.h"
#include "sched_cand.h"

class SCHED_SPEC_HANDSHAKE {
public:
friend class SCHEDULER; 

    SCHED_SPEC_HANDSHAKE ()  {};
    ~SCHED_SPEC_HANDSHAKE () {}; 

        /* ref the comment to its definition for details
         */
    static BOOL Change_Load_Spec_Form (CANDIDATE* cand, 
                INT32 cutting_set_size, BOOL* insert_chk, SCHEDULER* sched);

    static BOOL OP_Can_not_be_Candidate (OP*, SPEC_TYPE); 
};

#endif
