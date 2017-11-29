/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 
  
  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//-*-c++-*-
#ifndef cggrp_microsched_INCLUDED
#define cggrp_microsched_INCLUDED

#include "cg_flags.h"
#include "ipfec_options.h"
#ifdef TARG_IA64
#include "targ_issue_port.h"
#endif
// ==================================================================
//
//  Module :  cggrp_microsched.h
//  $Date  : $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/cggrp_microsched.h,v $
//
//  Description:
//  ============
//
//  This file contains the interface for micro scheduler.
//
//  - Micro scheduler interact with scheduler: it simulates the machine's
//    state and provides resource hazard checking and template
//    selection. We plan to hide the information about template
//    selection, function unit assignment and machine width from
//    the scheduler.
//  - Micro-scheduling should have the capacity to reorder
//    instructions in a group to eliminate unnecessary stops. 
//  - Currently we only support cycle scheduling, 
//
// ==================================================================

// ==================================================================
// Functions answer the scheduler's query.
// It should get called only in scheduler.

// Return true if the current cycle is fully occupied. No op can be
// issued in this cycle any more.
extern BOOL CGGRP_Cycle_Full(void);

// Check current resource, template and dependence constraint to see
// whether the given operation inst is able to be issued in the current
// cycle. Return true if there is a valid issue. If commit is true,
// the new operation is added and the internal issue state is updated. 
extern BOOL CGGRP_Issue_OP(OP *inst, BOOL commit = FALSE);

// Tell the micro-scheduler to advance the current cycle, which will
// cause the micro scheduler to bundle the operations, add the stop
// bit and clear the reservation table.
extern void CGGRP_Cycle_Advance(void); 

/* CGGRP_{Begin|End}_BB should be called before and after we 
 * finish scheduling a block. This requirement holds both the 
 * global and local scheduling phases.
 */
void CGGRP_Begin_BB (BB* blk);
void CGGRP_End_BB(void);

/* Do init/fini for the at the begining/after the scheduling process.
 * Normally, CGGRP_Init() should be called right before sched begins
 * but after DAG is build. 
 *
 * CGGRP_Fini() is called after scheduling finished.
 */
class REGION;
BOOL CGGRP_Init (REGION* rgn);
BOOL CGGRP_Init (BB* bb);
void CGGRP_Fini (void);

#endif // CGGRP_microsched_INCLUDED
