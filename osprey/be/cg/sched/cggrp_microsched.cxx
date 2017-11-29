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

#include "defs.h"
#include "cg.h"
#include "op.h"
#include "bb.h"
#include "cg_dep_graph.h"
#include "ti_res.h"
#include "ti_res_res.h"
#include "cggrp_microsched.h"
#include "region.h"

static INT cur_cyc;
static TI_RES_RES* rr_tab;
static MEM_POOL cggrp_mp;

/* the maximum possible cycle of a block after schedule.  */
static INT32 _max_blk_cyc = -1; 

BOOL
CGGRP_Init (REGION* rgn) {
    MEM_POOL_Initialize (&cggrp_mp, "Resource Reservation Table", FALSE);
    MEM_POOL_Push (&cggrp_mp);

    _max_blk_cyc = 0;
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(rgn->Regional_Cfg());
         iter != 0; ++iter) {

        if ((*iter)->Is_Region()) continue ;
        BB* blk = (*iter)->BB_Node();
        if (BB_entry(blk) || BB_exit(blk)) {
            continue;
        }

        OP* op;
        INT max_resource_cycles = 0;
        FOR_ALL_BB_OPs_FWD (blk, op) {
            INT cur_resource_cycles = TI_RES_Cycle_Count(OP_code(op));
            if (cur_resource_cycles > max_resource_cycles) {
	            max_resource_cycles = cur_resource_cycles;
            }
            INT op_latency = cur_resource_cycles;
            ARC_LIST *arcs;
            for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	            ARC *arc = ARC_LIST_first(arcs);
	            if (ARC_latency(arc) > op_latency) {
	                op_latency = ARC_latency(arc);
	            }
            }
            _max_blk_cyc += op_latency;
        }

        /* increase table size by the maximum number of resource cycles 
         * needed by any OP.
         */
        _max_blk_cyc += max_resource_cycles;
    }

    return TRUE;
}

BOOL
CGGRP_Init (BB* blk) {

    MEM_POOL_Initialize (&cggrp_mp, "Resource Reservation Table", FALSE);
    MEM_POOL_Push (&cggrp_mp);

    /* estimiate the max cycle of a block after sched */
    INT max_resource_cycles = 0;
    _max_blk_cyc = 0;

    OP* op;
    FOR_ALL_BB_OPs_FWD (blk, op) {
        INT cur_resource_cycles = TI_RES_Cycle_Count(OP_code(op));
        if (cur_resource_cycles > max_resource_cycles) {
	        max_resource_cycles = cur_resource_cycles;
        }
        INT op_latency = cur_resource_cycles;
        ARC_LIST *arcs;
        for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	        ARC *arc = ARC_LIST_first(arcs);
	        if (ARC_latency(arc) > op_latency) {
	            op_latency = ARC_latency(arc);
	        }
        }
        _max_blk_cyc += op_latency;
    }

    /* increase table size by the maximum number of resource cycles 
     * needed by any OP.
     */
    _max_blk_cyc += max_resource_cycles;
    return TRUE;
}

void
CGGRP_Fini (void) {
    MEM_POOL_Pop (&cggrp_mp);
    MEM_POOL_Delete (&cggrp_mp);
}

BOOL
CGGRP_Cycle_Full(void) {
    return FALSE;
}

BOOL
CGGRP_Issue_OP(OP *inst, BOOL commit) {
    if (!commit) 
    {
#if (defined(TARG_SL) || defined(TARG_SL2))
       if(TI_RES_RES_Resources_Available
                    (rr_tab, OP_code(inst), cur_cyc))
           return TRUE; 
       else if( OP_alu(inst) && 
                ( Is_Target_Sl2_pcore() || Is_Target_Sl2_mcore() ) &&
                TI_RES_RES_Resources_Available (rr_tab,  TOP_c2_cmov,  cur_cyc)) 
            // TI_RES_RES_Alternative_Resources_Available
            return TRUE;
       else 
            return FALSE;
#else 
        return TI_RES_RES_Resources_Available
                    (rr_tab, OP_code(inst), cur_cyc);
#endif 
    }

#if (defined(TARG_SL) || defined(TARG_SL2))
     if(!OP_alu(inst)) 
          TI_RES_RES_Reserve_Resources(rr_tab, OP_code(inst), cur_cyc); 
     else if( OP_alu(inst) && 
              ! Is_Target_Sl2_pcore() &&
              ! Is_Target_Sl2_mcore() &&
              TI_RES_RES_Resources_Available(rr_tab, OP_code(inst), cur_cyc))
          TI_RES_RES_Reserve_Resources(rr_tab, OP_code(inst), cur_cyc); 
     else 
          //TI_RES_RES_Reserve_Alternative_Resources(rr_tab, OP_code(inst), cur_cyc);        
          TI_RES_RES_Reserve_Resources(rr_tab, TOP_c2_cmov, cur_cyc); 
#else 
    TI_RES_RES_Reserve_Resources (rr_tab, OP_code(inst), cur_cyc);
#endif 
    return TRUE;
}

void
CGGRP_Cycle_Advance(void) {
    ++cur_cyc;
}

/* the codes are stolen from HB_Schedule::Init_RFlag_Table */
void
CGGRP_Begin_BB (BB* blk) {

    INT max_resource_cycles = 0;
    INT rtable_size = 0;
    rr_tab = TI_RES_RES_Alloc(FALSE,&cggrp_mp); 

    Is_True (_max_blk_cyc >= 0, ("Invalid estimated block cycle"));
    TI_RES_RES_Set_BB_Cycle_Count(rr_tab, _max_blk_cyc);
}

void
CGGRP_End_BB (void) {
    rr_tab = NULL;
}
