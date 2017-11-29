/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
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

//======================================================================
//
// Module: global_cycles_finder.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/global_cycles_finder.cxx,v $
//
//======================================================================

#include "global_cycles_finder.h"

//============================================================================
//  
//  Find Cycles
//
//  Main control function of finding out all cycles.
//   
//============================================================================ 
void   
GLOBAL_CYCLES_FINDER::Find_Global_Cycles(GLOBAL_CYCLE_VECTOR& cycles) {
    INT_VECTOR   dfn(PU_BB_Count+2, (INT32)0,INT_ALLOC(&_m)); 

    INT32        next_dfn = 0;
    BS           *visited      = BS_Create_Empty(PU_BB_Count, &_m); 

    for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
        Is_True(bb != NULL,("The BB is NULL in Find Global Cycles."));
        if (BB_First_Pred(bb) == NULL) 
            Detect_Global_Cycle(cycles,bb,dfn,visited,next_dfn);
    }   
}

void 
GLOBAL_CYCLES_FINDER::Detect_Global_Cycle(GLOBAL_CYCLE_VECTOR& cycles,
                      BB *bb,INT_VECTOR dfn,BS *visited,INT32 next_dfn) {
    
    dfn[BB_id(bb)] = ++next_dfn;
    
    BBLIST *succs;

    FOR_ALL_BB_SUCCS(bb, succs) { 
        BB *succ = BBLIST_item(succs);
        Is_True(succ != NULL,("Succ BB is NULL in Detect Global Cycle."));

        if (!BS_MemberP(visited,BB_id(succ))) {
            if (dfn[BB_id(succ)] == 0) {
                Detect_Global_Cycle(cycles,succ,dfn,visited,next_dfn);
            }    
            else if (dfn[BB_id(succ)] <= dfn[BB_id(bb)]) {
	        GLOBAL_CYCLE cycle;
                cycle.src  = bb;
                cycle.dest = succ;
                cycles.push_back(cycle);
            }
        }
    }     

    BS_Union1D(visited,BB_id(bb),&_m);
}

void 
GLOBAL_CYCLES_FINDER::Print(GLOBAL_CYCLE_VECTOR cycles,FILE *f) {
    for (GLOBAL_CYCLE_VECTOR_ITER iter = cycles.begin();
        iter != cycles.end();iter++) {
        fprintf(f," The cycle is : ");
        fprintf(f," pred %d:",BB_id((*iter).src));
        fprintf(f," succ %d:\n",BB_id((*iter).dest));
    }
}
