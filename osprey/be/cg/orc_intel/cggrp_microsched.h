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
#include "targ_issue_port.h"

// ==================================================================
//
//  Module :  cggrp_microsched.h
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/cggrp_microsched.h,v $
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

// This should be used by the scheduler to tell the micro-scheduler
// that it is the end of a basic block; do NOT tail a partial bundle
// in the next cycle. And make sure this cycle do NOT end with a
// compressed template.
extern void CGGRP_Force_Bundle_End(void);

//This should be used by the scheduler to get the op's issue port.
extern ISSUE_PORT CGGRP_Get_Issue_Port(OP *inst);

//This should be used by the scheduler to tell the micro_scheduler which port 
//the specified op should be issued to.
extern void CGGRP_Set_Issue_Port(OP *inst,PORT_SET set_op_fu);


inline void CGGRP_End_BB(void)
{
  if (IPFEC_sched_care_machine == Sched_care_bundle)
      CGGRP_Force_Bundle_End();
  else
      CGGRP_Cycle_Advance();
}

extern void CGGRP_Bundle(void);
extern void Calculate_BB_Cycle(BB* bb, BOOL dag_exist);
extern BOOL CGGRP_Bundle_OPS(OPS *ops,OPS *new_ops, INT stop_idx, BOOL cyclic=FALSE);
extern BOOL CGGRP_Bundle_OPS(OPS *ops,OP *op, INT stop_idx, BOOL cyclic=FALSE);

extern BOOL CGGRP_Check_Split_BB(BB* split_bb, BB** end_bbp);

// ==================================================================
// Global data structure:
// We need two flags controlled by compiler options to control the
// behavior of the micro-schedulor:

// This is the flag in current Pro64. If it is turned on, we do
// bundling. Else where we just do one instruction per cycle. It is
// for purpose of debugging, for both users and us.
// LOCS_Enable_Bundle_Formation

// This is the flag to control compressed template being using. We do not
// use compressed template if it is turned off.
extern BOOL IPFEC_Enable_Compressed_Template;

// This flag is used to control NOP padding
// When it is true, we insert NOPs to empty slots to enable proper
// bundling. When it is false, we do NOT insert NOPs for we believe
// that this is not the last time we do bundling.
extern BOOL IPFEC_Pad_Nop;


inline OP *OP_far_next(OP *prev_op)
{

    OP* next_op = OP_next(prev_op);
    if (next_op != NULL)
        return next_op;

    BB* prev_bb = OP_bb(prev_op);
    BB* next_bb = BB_next(prev_bb);
    
    if (next_bb == NULL || !BB_chk_split(prev_bb) || !BB_chk_split(next_bb))
        if (next_bb == NULL || !BB_partial_bundle(next_bb) || !BB_partial_bundle(prev_bb))
        return NULL;
    
    while(BB_length(next_bb)==0 && BB_partial_bundle(next_bb)){
        next_bb = BB_next(next_bb);
        if (next_bb == NULL) {return NULL;} 
    }
    next_op = BB_first_op(next_bb);
    if (!next_op || OP_start_bundle(next_op))
        return NULL;

    return next_op;
}

#endif // CGGRP_microsched_INCLUDED
// End of File
