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


//*********************************************************************
//
// Module: prdb.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/prdb_util.cxx,v $
//
// Description:
//
// Implementation of some extra utilities to test PRDB.
//
//
//
//*********************************************************************

#include "tracing.h"
#include "ipfec_defs.h"
#include "prdb_util.h"
#include "prdb.h"


void PRDB_term()
{
    Delete_PRDB();
}

void
Test_PRDB(REGION* region, PARTITION_GRAPH* graph)
{
    TOPOLOGICAL_REGIONAL_CFG_ITER top_iter(region->Regional_Cfg());
    REGIONAL_CFG_NODE* cfg_node = *top_iter;
    if(cfg_node->Is_Region()) return;
    Is_True(!graph->Cycle_Detector(), ("Cycle partitioin graph detected!!!"));
    Is_True(!graph->Illegal_Partition(), ("Illegal partition detected!!!"));
    BB* bb, *bb2;
    BB_CONTAINER bbs; 
    BB_CONTAINER::iterator iter, iter1;
    for (top_iter; top_iter!=0; ++top_iter)
    {
        REGIONAL_CFG_NODE *node = *top_iter;
        if ( node -> Is_Region()) continue;

        bb = node -> BB_Node();
        bbs.push_back(bb);
    }

    TN_OP_PAIR tn_op1, tn_op2;
    for(iter = bbs.begin(); iter != bbs.end(); ++iter) {
        bb = *iter;
        if(BB_last_op(bb))
            tn_op1.second = OP_prev(BB_last_op(bb));
        if(!tn_op1.second) continue;
        tn_op1.first = OP_results(tn_op1.second)>0?
            OP_result(tn_op1.second, 0):OP_opnd(tn_op1.second, 0);
        Is_True(tn_op1.first, (" Illegal TN \n"));
        if(!TN_is_register(tn_op1.first)||
            TN_register_class(tn_op1.first) != ISA_REGISTER_CLASS_predicate)
            continue;
        for(iter1 = bbs.begin(); iter1 != bbs.end(); ++iter1) {
            bb2 = *iter1;
            if(BB_last_op(bb2))
              tn_op2.second = OP_prev(BB_last_op(bb2));
            if(!tn_op2.second) continue;
            tn_op2.first = OP_results(tn_op2.second)>0?
                OP_result(tn_op2.second, 0):OP_opnd(tn_op2.second, 0);
            Is_True(tn_op2.first, (" Illegal TN \n"));
            if(!TN_is_register(tn_op2.first)||
                TN_register_class(tn_op2.first) != ISA_REGISTER_CLASS_predicate)
                continue;
            if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
            fPrint_TN ( TFile, "tn1-:%s", tn_op1.first);
            fprintf(TFile, " probability is %f; ", graph->Get_Probability(tn_op1.first));
            fPrint_TN ( TFile, "and tn2-:%s", tn_op2.first);
            fprintf(TFile, " probability is %f.\n", graph->Get_Probability(tn_op2.first));
            fPrint_TN ( TFile, "tn1-:%s", tn_op1.first);
            fprintf(TFile, "/");
            fPrint_TN ( TFile, "tn2-:%s", tn_op2.first);
            fprintf(TFile, " probability is %f\n", graph->Get_Probability(tn_op1.first,tn_op2.first));
        }
        }
        TP_ALLOCATOR tp_allocator;
        TP_CONTAINER tp_result(tp_allocator);
        if(graph->Get_Complementary(&tp_result, tn_op1, bb))
        {
            TP_CONTAINER::iterator tp_iter;
            fPrint_TN ( TFile, "tn1-:%s", tn_op1.first);
            fprintf(TFile, " cpty w.s.t BB-%d includes: ", BB_id(bb));
            for(tp_iter=tp_result.begin();tp_iter!=tp_result.end();tp_iter++)
            {
                fPrint_TN(TFile," %s ", (*tp_iter)->first);
            }
        }
    }
}

void 
Verify_PRDB(REGION* region, PRDB_GEN* prdb)
{
    if(region->Region_Type() == IMPROPER ||
        region->Is_No_Further_Opt())
        return;
    PARTITION_GRAPH* graph = prdb->Partition_Graph(region);
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter( region -> Regional_Cfg()); 
    iter!=0; 
    ++iter)
    {
        REGIONAL_CFG_NODE *node = *iter;
        if ( node -> Is_Region()) 
            continue;

        BB* bb = node -> BB_Node();
        OP* op;
        FOR_ALL_BB_OPs(bb, op){
            PG_CONTAINER* node_set = (PG_CONTAINER*)OP_MAP_Get(graph->Tn_Node_Map(), op);
            Is_True(node_set, ("No partition node set mapped to op!"));
        }
    }
}

