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
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/prdb.cxx,v $
//
// Description:
//
// Implementation of Ipfec predicate relation datebase.
// See prdb.h for the description.
//
//
//
//*********************************************************************

#include <vector>
#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "if_conv.h"
#include "op.h"
#include "tn.h"
#include "tn_map.h"
#include "tracing.h"
#include "cg.h"
#include "dominate.h"
#include "timing.h"
#include "prdb.h"
#include "ipfec_defs.h"
#include <ipfec_options.h>
#include "vt_region.h"
#include "vt_partition.h"
#include "region_bb_util.h"

//for PRDB experiments
#include "be_util.h"

//declarations:
void Find_BB_Predicates(BB* bb, TN*& first_tn, TN*& second_tn);
void  Print_OP_No_SrcLine(const OP *op);
void  fPrint_TN ( FILE *f, char *fmt, TN *tn);
BOOL  Is_In_Infinite_Loop(REGION*);
BOOL  Is_Abnormal_Loop(REGION*);
BOOL  Is_OP_Cond(OP *op);

static PRDB_GEN* prdb = NULL;

MEM_POOL* PRDB_pool;

COMPARE_TYPE Compare_Type(TOP opcode);

//return true if the edge is a critical edge according to global cfg.
inline BOOL Is_Critical_Edge(REGIONAL_CFG_EDGE* edge)
{
    REGIONAL_CFG_NODE* cfg_node = edge->Dest();
    if(cfg_node->Pred_Num()==1) return FALSE;
    cfg_node = edge->Src();
    if (cfg_node->Succ_Num()==1&&!cfg_node->Is_Exit()) return FALSE;
    return TRUE;
}

inline BB* Find_Region_Entry_BB(REGION* region)
{
    REGIONAL_CFG_NODE *node = region->Entries()[0];
    do{
        if ( !node -> Is_Region()) return node->BB_Node();
        node = node->Region_Node()->Entries()[0];
    }while(1);
    return NULL;
}

BOOL
Is_No_BB_Region(REGION *region)
{
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter( region -> Regional_Cfg()); 
    iter!=0; 
    ++iter)
    {
        REGIONAL_CFG_NODE *node = *iter;
        if(!node->Is_Region()) 
        {
            return FALSE;
        }
    }
    return TRUE;
}

//*****************************************************************************
// Function : Find_Reaching_Def_Use_Set
// Input : 
//   - result_set : the found reaching defition/use ops
//   - tn : the tn for which we want to find reaching definition/use  
//   - home_op : the op where 'tn' locates
//   - home_bb : the bb where 'op' locates
//   - reaching_def : indicate we should look for reaching_def or reaching_use
// Output :
//   < none >
// Description :
//   the function is to look for reaching defintion/use for 'tn' in 'home_op'. 
//*****************************************************************************
void
Find_Reaching_Def_Use_Set(OP_CONTAINER* result_set, 
                      TN* tn, 
                      OP* home_op, 
                      BB* home_bb, 
                      BB_SET* visited_bb, 
                      BOOL reaching_def,
                      MEM_POOL* mem_pool)
{
    visited_bb = BB_SET_Union1D(visited_bb, home_bb, mem_pool);
    // look for the reaching definition of 'tn' backward from 'home_op'
    if (!TN_is_register(tn)) 
    {
        Is_True(0,
            ("look for reaching definition/use for REGISTER_UNDEFINED"));
    }

    OP *op;
    if (home_op) {    
        op = OP_prev(home_op);
    } else {
        op = BB_last_op(home_bb);
    }

    while (op) {
        if ( reaching_def )
        {
            if( OP_Defs_TN(op, tn) ) {
                result_set->push_back(op);
                if (!Is_OP_Cond(op) || (home_op && Is_OP_Cond(home_op) &&
                    OP_bb(op) == home_bb && OP_opnd(op, OP_PREDICATE_OPND)
                    ==  OP_opnd(home_op, OP_PREDICATE_OPND))) 
                    return;
            }
        } else {
            if(OP_Refs_TN(op, tn)){
                result_set->push_back(op);
                return;
            }
        }
        op = OP_prev(op);
    }

    REGIONAL_CFG_NODE* rgn_node = Regional_Cfg_Node(home_bb);
    CFG_PRED_NODE_ITER pre_iter(rgn_node);
    for(pre_iter; pre_iter != 0; ++pre_iter){
        if((*pre_iter)->Is_Region()) continue;
        BB *pred = (*pre_iter)->BB_Node();
        if(!BB_SET_MemberP(visited_bb, pred))
            Find_Reaching_Def_Use_Set(result_set, tn, NULL, pred, visited_bb, reaching_def, mem_pool);
    }
    
    return;
}

//return true if set1 and set2 have the same OPs
BOOL
Same_OPS(OP_CONTAINER* set1, OP_CONTAINER* set2)
{
    if(set1->size() != set2->size()) return FALSE;
    OP_CONTAINER::iterator iter1, iter2;
    for(iter1 = set1->begin(); iter1 != set1->end(); ++iter1){
        OP* op1 = *iter1;
        BOOL match = FALSE;
        for(iter2 = set2->begin(); iter2 != set2->end(); ++iter2){
            OP* op2 = *iter2;
            if(op1 == op2)
            {
                match = TRUE;
                break;
            }
        }
        if(!match) return FALSE;
    }
    
    return TRUE;
}

//Is_In_Abnormal_Loop judges whether current region is nested in abnormal
//loop. The interface uses Is_Abnormal_Loop to judge current region's parent
//region

BOOL
Is_In_Abnormal_Loop(REGION* r)
{
    while(r){
        if(Is_Abnormal_Loop(r)) return TRUE;
        r = r->Parent();
    }
    return FALSE;
}

//*********************************************************************
//Part 1: Class PARTITION
//*********************************************************************
BOOL 
PARTITION::operator==(PARTITION* par1)
{
    if ( this ->_child .size() == par1->Child().size())
    {
        if ( this -> Parent() -> Index() == par1->Parent() ->Index())
        {
            for (INT i = 0; i< this ->_child. size(); i++)
            {
                if (this ->_child[i]->Index() 
                    != (par1 ->Child())[i] -> Index()) 
                    return FALSE;
            }
            return TRUE;
        }
    }
    return FALSE;
}


PARTITION_GRAPH_NODE::PARTITION_GRAPH_NODE():
      _parent_partitions(PT_ALLOC(PRDB_pool)),
      _child_partitions(PT_ALLOC(PRDB_pool)),
      _related_tns(TN_CONTAINER_ALLOC(PRDB_pool))

    {
          _related_bbs = BB_SET_Create_Empty( PU_BB_Count+2, PRDB_pool);
          _level = -1;
          _control_pred = NULL;
    }

void 
PARTITION_GRAPH_NODE::Del_Related_TN(TN* tn, OP* op)
{
//delete the <tn, op> pair from _related_tns if there is no other
// def and use of this tn
    TP_CONTAINER* tn_set = &_related_tns;
    TP_CONTAINER::iterator iter;
    DEF_KIND kind;
    if (TN_Reaching_Value_At_Op(tn , op, &kind, TRUE) != op &&
        TN_Reaching_Value_At_Op(tn , op, &kind, FALSE) != op)
    {
        for(iter = tn_set->begin(); iter!=tn_set->end(); iter++)
        {
            if ((*iter)->first == tn) 
            {
                iter = tn_set->erase(iter);
                break;
            }
        }
    }
}

COMPARE_TYPE Compare_Type(TOP opcode)
{
    switch(opcode) {
        case TOP_cmp4_eq_or_andcm: 
        case TOP_cmp4_ge_or_andcm: 
        case TOP_cmp4_gt_or_andcm: 
        case TOP_cmp4_i_eq_or_andcm: 
        case TOP_cmp4_i_ne_or_andcm: 
        case TOP_cmp4_le_or_andcm: 
        case TOP_cmp4_lt_or_andcm: 
        case TOP_cmp4_ne_or_andcm: 
        case TOP_cmp4_z1_ge_or_andcm: 
        case TOP_cmp4_z1_gt_or_andcm: 
        case TOP_cmp4_z1_le_or_andcm: 
        case TOP_cmp4_z1_lt_or_andcm: 
        case TOP_cmp4_z2_ge_or_andcm: 
        case TOP_cmp4_z2_gt_or_andcm: 
        case TOP_cmp4_z2_le_or_andcm: 
        case TOP_cmp4_z2_lt_or_andcm: 
        case TOP_cmp_eq_or_andcm: 
        case TOP_cmp_ge_or_andcm: 
        case TOP_cmp_gt_or_andcm: 
        case TOP_cmp_i_eq_or_andcm: 
        case TOP_cmp_i_ne_or_andcm: 
        case TOP_cmp_le_or_andcm: 
        case TOP_cmp_lt_or_andcm: 
        case TOP_cmp_ne_or_andcm: 
        case TOP_cmp_z1_ge_or_andcm: 
        case TOP_cmp_z1_gt_or_andcm: 
        case TOP_cmp_z1_le_or_andcm: 
        case TOP_cmp_z1_lt_or_andcm: 
        case TOP_cmp_z2_ge_or_andcm: 
        case TOP_cmp_z2_gt_or_andcm: 
        case TOP_cmp_z2_le_or_andcm: 
        case TOP_cmp_z2_lt_or_andcm: 
        case TOP_tbit_nz_or_andcm: 
        case TOP_tbit_z_or_andcm: 
        case TOP_tnat_nz_or_andcm: 
        case TOP_tnat_z_or_andcm: 
           return COMPARE_TYPE_or_andcm;

        case TOP_cmp4_eq_and_orcm: 
        case TOP_cmp4_ge_and_orcm: 
        case TOP_cmp4_gt_and_orcm: 
        case TOP_cmp4_i_eq_and_orcm: 
        case TOP_cmp4_i_ne_and_orcm: 
        case TOP_cmp4_le_and_orcm: 
        case TOP_cmp4_lt_and_orcm: 
        case TOP_cmp4_ne_and_orcm: 
        case TOP_cmp4_z1_ge_and_orcm: 
        case TOP_cmp4_z1_gt_and_orcm: 
        case TOP_cmp4_z1_le_and_orcm: 
        case TOP_cmp4_z1_lt_and_orcm: 
        case TOP_cmp4_z2_ge_and_orcm: 
        case TOP_cmp4_z2_gt_and_orcm: 
        case TOP_cmp4_z2_le_and_orcm: 
        case TOP_cmp4_z2_lt_and_orcm: 
        case TOP_cmp_eq_and_orcm: 
        case TOP_cmp_ge_and_orcm: 
        case TOP_cmp_gt_and_orcm: 
        case TOP_cmp_i_eq_and_orcm: 
        case TOP_cmp_i_ne_and_orcm: 
        case TOP_cmp_le_and_orcm: 
        case TOP_cmp_lt_and_orcm: 
        case TOP_cmp_ne_and_orcm: 
        case TOP_cmp_z1_ge_and_orcm: 
        case TOP_cmp_z1_gt_and_orcm: 
        case TOP_cmp_z1_le_and_orcm: 
        case TOP_cmp_z1_lt_and_orcm: 
        case TOP_cmp_z2_ge_and_orcm: 
        case TOP_cmp_z2_gt_and_orcm: 
        case TOP_cmp_z2_le_and_orcm: 
        case TOP_cmp_z2_lt_and_orcm: 
           return COMPARE_TYPE_and_orcm;

        case TOP_cmp4_eq_andcm: 
        case TOP_cmp4_ge_andcm: 
        case TOP_cmp4_gt_andcm: 
        case TOP_cmp4_i_eq_andcm: 
        case TOP_cmp4_i_ne_andcm: 
        case TOP_cmp4_le_andcm: 
        case TOP_cmp4_lt_andcm: 
        case TOP_cmp4_ne_andcm: 
        case TOP_cmp4_z1_ge_andcm: 
        case TOP_cmp4_z1_gt_andcm: 
        case TOP_cmp4_z1_le_andcm: 
        case TOP_cmp4_z1_lt_andcm: 
        case TOP_cmp4_z2_ge_andcm: 
        case TOP_cmp4_z2_gt_andcm: 
        case TOP_cmp4_z2_le_andcm: 
        case TOP_cmp4_z2_lt_andcm: 
        case TOP_cmp_eq_andcm: 
        case TOP_cmp_ge_andcm: 
        case TOP_cmp_gt_andcm: 
        case TOP_cmp_i_eq_andcm: 
        case TOP_cmp_i_ne_andcm: 
        case TOP_cmp_le_andcm: 
        case TOP_cmp_lt_andcm: 
        case TOP_cmp_ne_andcm: 
        case TOP_cmp_z1_ge_andcm: 
        case TOP_cmp_z1_gt_andcm: 
        case TOP_cmp_z1_le_andcm: 
        case TOP_cmp_z1_lt_andcm: 
        case TOP_cmp_z2_ge_andcm: 
        case TOP_cmp_z2_gt_andcm: 
        case TOP_cmp_z2_le_andcm: 
        case TOP_cmp_z2_lt_andcm: 
           return COMPARE_TYPE_andcm;

        case TOP_cmp4_eq_and: 
        case TOP_cmp4_ge_and: 
        case TOP_cmp4_gt_and: 
        case TOP_cmp4_i_eq_and: 
        case TOP_cmp4_i_ne_and: 
        case TOP_cmp4_le_and: 
        case TOP_cmp4_lt_and: 
        case TOP_cmp4_ne_and: 
        case TOP_cmp4_z1_ge_and: 
        case TOP_cmp4_z1_gt_and: 
        case TOP_cmp4_z1_le_and: 
        case TOP_cmp4_z1_lt_and: 
        case TOP_cmp4_z2_ge_and: 
        case TOP_cmp4_z2_gt_and: 
        case TOP_cmp4_z2_le_and: 
        case TOP_cmp4_z2_lt_and: 
        case TOP_cmp_eq_and: 
        case TOP_cmp_ge_and: 
        case TOP_cmp_gt_and: 
        case TOP_cmp_i_eq_and: 
        case TOP_cmp_i_ne_and: 
        case TOP_cmp_le_and: 
        case TOP_cmp_lt_and: 
        case TOP_cmp_ne_and: 
        case TOP_cmp_z1_ge_and: 
        case TOP_cmp_z1_gt_and: 
        case TOP_cmp_z1_le_and: 
        case TOP_cmp_z1_lt_and: 
        case TOP_cmp_z2_ge_and: 
        case TOP_cmp_z2_gt_and: 
        case TOP_cmp_z2_le_and: 
        case TOP_cmp_z2_lt_and: 
        case TOP_tbit_nz_and: 
        case TOP_tbit_z_and: 
        case TOP_tnat_nz_and: 
        case TOP_tnat_z_and: 
           return COMPARE_TYPE_and;

        case TOP_cmp4_eq_orcm: 
        case TOP_cmp4_ge_orcm: 
        case TOP_cmp4_gt_orcm: 
        case TOP_cmp4_i_eq_orcm: 
        case TOP_cmp4_i_ne_orcm: 
        case TOP_cmp4_le_orcm: 
        case TOP_cmp4_lt_orcm: 
        case TOP_cmp4_ne_orcm: 
        case TOP_cmp4_z1_ge_orcm: 
        case TOP_cmp4_z1_gt_orcm: 
        case TOP_cmp4_z1_le_orcm: 
        case TOP_cmp4_z1_lt_orcm: 
        case TOP_cmp4_z2_ge_orcm: 
        case TOP_cmp4_z2_gt_orcm: 
        case TOP_cmp4_z2_le_orcm: 
        case TOP_cmp4_z2_lt_orcm: 
        case TOP_cmp_eq_orcm: 
        case TOP_cmp_ge_orcm: 
        case TOP_cmp_gt_orcm: 
        case TOP_cmp_i_eq_orcm: 
        case TOP_cmp_i_ne_orcm: 
        case TOP_cmp_le_orcm: 
        case TOP_cmp_lt_orcm: 
        case TOP_cmp_ne_orcm: 
        case TOP_cmp_z1_ge_orcm: 
        case TOP_cmp_z1_gt_orcm: 
        case TOP_cmp_z1_le_orcm: 
        case TOP_cmp_z1_lt_orcm: 
        case TOP_cmp_z2_ge_orcm: 
        case TOP_cmp_z2_gt_orcm: 
        case TOP_cmp_z2_le_orcm: 
        case TOP_cmp_z2_lt_orcm: 
           return COMPARE_TYPE_orcm;

        case TOP_cmp4_eq_or: 
        case TOP_cmp4_ge_or: 
        case TOP_cmp4_gt_or: 
        case TOP_cmp4_i_eq_or: 
        case TOP_cmp4_i_ne_or: 
        case TOP_cmp4_le_or: 
        case TOP_cmp4_lt_or: 
        case TOP_cmp4_ne_or: 
        case TOP_cmp4_z1_ge_or: 
        case TOP_cmp4_z1_gt_or: 
        case TOP_cmp4_z1_le_or: 
        case TOP_cmp4_z1_lt_or: 
        case TOP_cmp4_z2_ge_or: 
        case TOP_cmp4_z2_gt_or: 
        case TOP_cmp4_z2_le_or: 
        case TOP_cmp4_z2_lt_or: 
        case TOP_cmp_eq_or: 
        case TOP_cmp_ge_or: 
        case TOP_cmp_gt_or: 
        case TOP_cmp_i_eq_or: 
        case TOP_cmp_i_ne_or: 
        case TOP_cmp_le_or: 
        case TOP_cmp_lt_or: 
        case TOP_cmp_ne_or: 
        case TOP_cmp_z1_ge_or: 
        case TOP_cmp_z1_gt_or: 
        case TOP_cmp_z1_le_or: 
        case TOP_cmp_z1_lt_or: 
        case TOP_cmp_z2_ge_or: 
        case TOP_cmp_z2_gt_or: 
        case TOP_cmp_z2_le_or: 
        case TOP_cmp_z2_lt_or: 
        case TOP_tbit_nz_or: 
        case TOP_tbit_z_or: 
        case TOP_tnat_nz_or: 
        case TOP_tnat_z_or: 
           return COMPARE_TYPE_or;

        case TOP_fcmp_ord_unc: 
        case TOP_fcmp_unord_unc: 
        case TOP_cmp4_eq_unc: 
        case TOP_cmp4_ge_unc: 
        case TOP_cmp4_geu_unc: 
        case TOP_cmp4_gt_unc: 
        case TOP_cmp4_gtu_unc: 
        case TOP_cmp4_i_eq_unc: 
        case TOP_cmp4_i_ge_unc: 
        case TOP_cmp4_i_geu_unc: 
        case TOP_cmp4_i_gt_unc: 
        case TOP_cmp4_i_gtu_unc: 
        case TOP_cmp4_i_le_unc: 
        case TOP_cmp4_i_leu_unc: 
        case TOP_cmp4_i_lt_unc: 
        case TOP_cmp4_i_ltu_unc: 
        case TOP_cmp4_i_ne_unc: 
        case TOP_cmp4_le_unc: 
        case TOP_cmp4_leu_unc: 
        case TOP_cmp4_lt_unc: 
        case TOP_cmp4_ltu_unc: 
        case TOP_cmp4_ne_unc: 
        case TOP_cmp_eq_unc: 
        case TOP_cmp_ge_unc: 
        case TOP_cmp_geu_unc: 
        case TOP_cmp_gt_unc: 
        case TOP_cmp_gtu_unc: 
        case TOP_cmp_i_eq_unc: 
        case TOP_cmp_i_ge_unc: 
        case TOP_cmp_i_geu_unc: 
        case TOP_cmp_i_gt_unc: 
        case TOP_cmp_i_gtu_unc: 
        case TOP_cmp_i_le_unc: 
        case TOP_cmp_i_leu_unc: 
        case TOP_cmp_i_lt_unc: 
        case TOP_cmp_i_ltu_unc: 
        case TOP_cmp_i_ne_unc: 
        case TOP_cmp_le_unc: 
        case TOP_cmp_leu_unc: 
        case TOP_cmp_lt_unc: 
        case TOP_cmp_ltu_unc: 
        case TOP_cmp_ne_unc: 
        case TOP_fclass_m_unc: 
        case TOP_fclass_nm_unc: 
        case TOP_fcmp_eq_unc: 
        case TOP_fcmp_ge_unc: 
        case TOP_fcmp_gt_unc: 
        case TOP_fcmp_le_unc: 
        case TOP_fcmp_lt_unc: 
        case TOP_fcmp_neq_unc: 
        case TOP_fcmp_nge_unc: 
        case TOP_fcmp_ngt_unc: 
        case TOP_fcmp_nle_unc: 
        case TOP_fcmp_nlt_unc: 
        case TOP_tbit_nz_unc: 
        case TOP_tbit_z_unc: 
        case TOP_tnat_nz_unc: 
        case TOP_tnat_z_unc: 
           return COMPARE_TYPE_unc;

        case TOP_fcmp_ord: 
        case TOP_fcmp_unord: 
        case TOP_cmp4_eq: 
        case TOP_cmp4_ge: 
        case TOP_cmp4_geu: 
        case TOP_cmp4_gt: 
        case TOP_cmp4_gtu: 
        case TOP_cmp4_i_eq: 
        case TOP_cmp4_i_ge: 
        case TOP_cmp4_i_geu: 
        case TOP_cmp4_i_gt: 
        case TOP_cmp4_i_gtu: 
        case TOP_cmp4_i_le: 
        case TOP_cmp4_i_leu: 
        case TOP_cmp4_i_lt: 
        case TOP_cmp4_i_ltu: 
        case TOP_cmp4_i_ne: 
        case TOP_cmp4_le: 
        case TOP_cmp4_leu: 
        case TOP_cmp4_lt: 
        case TOP_cmp4_ltu: 
        case TOP_cmp4_ne: 
        case TOP_cmp_eq: 
        case TOP_cmp_ge: 
        case TOP_cmp_geu: 
        case TOP_cmp_gt: 
        case TOP_cmp_gtu: 
        case TOP_cmp_i_eq: 
        case TOP_cmp_i_ge: 
        case TOP_cmp_i_geu: 
        case TOP_cmp_i_gt: 
        case TOP_cmp_i_gtu: 
        case TOP_cmp_i_le: 
        case TOP_cmp_i_leu: 
        case TOP_cmp_i_lt: 
        case TOP_cmp_i_ltu: 
        case TOP_cmp_i_ne: 
        case TOP_cmp_le: 
        case TOP_cmp_leu: 
        case TOP_cmp_lt: 
        case TOP_cmp_ltu: 
        case TOP_cmp_ne: 
        case TOP_fclass_m: 
        case TOP_fclass_nm: 
        case TOP_fcmp_eq: 
        case TOP_fcmp_ge: 
        case TOP_fcmp_gt: 
        case TOP_fcmp_le: 
        case TOP_fcmp_lt: 
        case TOP_fcmp_neq: 
        case TOP_fcmp_nge: 
        case TOP_fcmp_ngt: 
        case TOP_fcmp_nle: 
        case TOP_fcmp_nlt: 
        case TOP_tbit_nz: 
        case TOP_tbit_z: 
        case TOP_tnat_nz: 
        case TOP_tnat_z: 
           return COMPARE_TYPE_normal;

        default: 
            Is_True(TRUE, ("Not compare instruction!!!"));
            return (COMPARE_TYPE) -1;
    }
}

//*********************************************************************
//Part 2: Class PARTITION GRAPH
//*********************************************************************
//Constructor of partition graph
PARTITION_GRAPH::PARTITION_GRAPH(REGION* region)
    :_nodes(PG_ALLOC(PRDB_pool)),
    _disjoint_relation(BV_ALLOC(PRDB_pool)),
    _subset_relation(BV_ALLOC(PRDB_pool)),
    _visited(0,PRDB_pool)
{
    partition_graph_node_number = 0;
    _tn_node_map = OP_MAP_Create();
    _bb_node_map = BB_MAP_Create();

    // step1: generate the root node and dummy node
    _dummy_node = CXX_NEW(PARTITION_GRAPH_NODE(), PRDB_pool);
    _dummy_node -> Index(partition_graph_node_number++);
    _root = CXX_NEW(PARTITION_GRAPH_NODE(), PRDB_pool);
    _root -> Index(partition_graph_node_number++);
    _root ->Level(1);

    Add_Node(_dummy_node);
    Add_Node(_root);

    //step2: construct mapping relations between partition graph nodes
    //and PN 
    Collect_Info (region); 
    if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        Print(TFile);

    //step3: look for partitions and construct partition graph 
    Look_For_Partition(region);

    for(INT k = 0; k<partition_graph_node_number; k++){
        _visited.push_back(FALSE);
    }
    Mark_Level(_root);
    //step4: Complete_partition_graph marks reachable and level properties
    if(VT_Enable_Partition_Graph) draw_partition_graph(this, "before complete!");
    Complete_Partition_Graph();

    if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, "\nbefore pre_computing\n");
    //step5: Pre_Computing handles disjoint and subset relations
    Pre_Computing();
}

//create node for equivalent class. First, traverse control flow graph, build
//  node for control predicates; second, traverse ops to build node for mat-
//  erialized predicates. Equivalent classes need to be formed as follows:
//  (1) All control equivalent basic blocks belong to an equivalent class;
//  (2) The basic block and its corresponding materialized predicate belong
//      to an equivalent class;
//  (3) Each predicate definition point is an equivalent class;
//  (4) The predicate use point, which has only one reaching definition and whose
//     definition point locates in the same basic block or in the control equivalent
//     block, belong to the same equivalent class as its definition point;
//  (5) For other predicate use points in one basic block, who are use-references
//      of one predicate and have same reaching definitions, they belong to an
//      equivalent class.

void 
PARTITION_GRAPH::Collect_Info(REGION* region)
{
    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, "\nstarting to collect info.\n");

    //step1: collect all basic blocks in the region
    BB* bb, *entry_bb = NULL;
    BB_CONTAINER bbs(PRDB_pool); 
    BB_CONTAINER::iterator iter;
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter( region -> Regional_Cfg()); 
    iter!=0; 
    ++iter)
    {
    //collect bb_node in regional_cfg, and get entry_bb(regional innermost
    //bb_node) if regional_cfg entry is a region
        REGIONAL_CFG_NODE *node = *iter;
        if ( node -> Is_Region()) {
            if(node->Is_Entry()) 
            {
                entry_bb = Find_Region_Entry_BB(region);
                Is_True(entry_bb, ("The region without entry BB node!"));
                bbs.push_back(entry_bb);
            }
            continue;
        }

        bb = node -> BB_Node();
        bbs.push_back(bb);
    }

    // divide blocks into different equal class 
    // and create partition graph node for them
    
    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, "starting to collect infomation about BBs.\n");

    //for infinite or abnormal loop region, we don't care control equivalent
    //classes because post dominate sets are not reliable
    if(Is_In_Infinite_Loop(region) || Is_In_Abnormal_Loop(region))
    {
        for(iter = bbs.begin(); iter != bbs.end(); ++iter) {
            bb = *iter;

            if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)) {
                fprintf(TFile, "  == collecting infomation about BB_%d.\n", BB_id(bb));
            }
            PARTITION_GRAPH_NODE* graph_node = NULL;
                    
            BB* eq_bb;
            INT bb_size = 0;
            FOR_ALL_BB_SET_members(BB_dom_set(bb), eq_bb)
            {
                if (Region_Contains_BB(region, eq_bb) && eq_bb != bb)
                {
                    bb_size++;
                    break;
                }
            }
            
            //map entry_bb or bb(without dom bb) to _root
            if ((entry_bb && bb == entry_bb) || !bb_size){
                graph_node = _root;
                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                    fprintf(TFile, "     it can share node with root. \n");
            } else {
                graph_node = CXX_NEW(PARTITION_GRAPH_NODE(),PRDB_pool);
                graph_node -> Index(partition_graph_node_number++);
                Add_Node(graph_node);
                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)) {
                    fprintf(TFile, "     create a partition graph node.\n");
                }
            }
            graph_node -> Add_Related_BB(bb);
            BB_MAP_Set(_bb_node_map, bb, graph_node);
        }
    }else{//for normal region, we need computer control equivalent classes
        BB_SET* equal_classes = BB_SET_Create_Empty(PU_BB_Count+2, PRDB_pool); 
        for(iter = bbs.begin(); iter != bbs.end(); ++iter){
            bb = *iter;

            if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                fprintf(TFile, "  == collecting infomation about BB_%d.\n", BB_id(bb));
            PARTITION_GRAPH_NODE* graph_node = NULL;
            //
            // Find the equivalence class for bb.  If there is no existing 
            // equivalent class, then create a new one and insert bb into it.
            //
            BB* ec;
            FOR_ALL_BB_SET_members( equal_classes, ec){
                if ((BB_SET_MemberP(BB_dom_set(bb), ec)&&
                    BB_SET_MemberP(BB_pdom_set(ec), bb)) ||
                    (BB_SET_MemberP(BB_dom_set(ec), bb) && 
                    BB_SET_MemberP(BB_pdom_set(bb), ec)))
                {
                    graph_node = 
                        (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, ec);
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                        fprintf(TFile, 
                            "      it can share the partition graph node "
                            "with BB_%d.\n", BB_id(ec));    
                    }
                    break;
                }
            }
        
            if (! graph_node) {
                BB* eq_bb;
                INT bb_size = 0;//BB_SET_Size(BB_dom_set(bb));
                FOR_ALL_BB_SET_members(BB_dom_set(bb), eq_bb)
                {
                    if (Region_Contains_BB(region, eq_bb) && eq_bb != bb)
                    {
                        bb_size++;
                        break;
                    }
                }

                //map entry_bb or bb(without dom bb) to _root
                if ((entry_bb && bb == entry_bb) || !bb_size){
                    graph_node = _root;
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                        fprintf(TFile, "     it can share node with root. \n");
                }else {
                    graph_node = CXX_NEW(PARTITION_GRAPH_NODE(),PRDB_pool);
                    graph_node -> Index(partition_graph_node_number++);
                    Add_Node(graph_node);
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                        fprintf(TFile, 
                        "     create a new partition graph node for it.\n");
                }
                BB_SET_Union1D(equal_classes, bb, PRDB_pool);
            }
            graph_node -> Add_Related_BB(bb);
            BB_MAP_Set(_bb_node_map, bb, graph_node);

        }
    }

    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, "starting to collect info about OPs.\n");

    //step2: start to collect node for ops.
    BB_SET* visited_bb = BB_SET_Create_Empty(PU_BB_Count+2, PRDB_pool);
    for(iter = bbs.begin(); iter != bbs.end(); ++iter){
        bb = *iter;
        if(entry_bb && bb == entry_bb) continue;

        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
            fprintf(TFile, 
                "  == collecting infomation about def TNs in BB_%d.\n", 
                BB_id(bb));

        OP* br_op = NULL;
        TN  *true_tn = NULL, *false_tn = NULL;
        BB  *fall_thru_bb = NULL,*branch_targ_bb = NULL;

        //For branch bb, we try to map br_op's predicate to its target bb and
        //the other predicate in the same predicate definition to its fall_thru bb
        br_op = BB_branch_op(bb);
        if(br_op && OP_code(br_op) == TOP_br_cond) {
          if (!(BB_kind(bb)==BBKIND_VARGOTO || BB_kind(bb)==BBKIND_INDGOTO))
          {
            fall_thru_bb = BB_Fall_Thru_Successor(bb);
            if( !fall_thru_bb || BB_preds_len(fall_thru_bb) >1 
                || region != Home_Region(fall_thru_bb)) {
              false_tn = NULL;
              fall_thru_bb = NULL;
            }
            if(fall_thru_bb) {
              branch_targ_bb = BB_Other_Successor(bb, fall_thru_bb);
              if (!branch_targ_bb || BB_preds_len(branch_targ_bb) >1 
                  || region != Home_Region(branch_targ_bb)) {
                true_tn = NULL;
                branch_targ_bb = NULL;
              }
            }
          }
          if(fall_thru_bb && branch_targ_bb && BB_preds_len(fall_thru_bb) == 1
              && BB_preds_len(branch_targ_bb) == 1) {
            true_tn = OP_opnd(br_op, OP_PREDICATE_OPND);
            OP_CONTAINER def_set(PRDB_pool);
            Find_Reaching_Def_Use_Set(&def_set, true_tn, br_op, bb, visited_bb, TRUE, PRDB_pool);
            visited_bb = BB_SET_ClearD(visited_bb);
            if(def_set.size() == 1) {
              OP* def_op = *def_set.begin();
              false_tn = true_tn == OP_result(def_op, 1) ? OP_result(def_op, 0): OP_result(def_op, 1);
              PARTITION_GRAPH_NODE* succ_node = (PARTITION_GRAPH_NODE*)
                BB_MAP_Get(_bb_node_map, branch_targ_bb);
              succ_node -> Control_Pred(true_tn);
              succ_node = (PARTITION_GRAPH_NODE*) BB_MAP_Get(_bb_node_map, fall_thru_bb);
              succ_node -> Control_Pred(false_tn);
            } else if(def_set.size() == 2) {
              OP* def_op1 = *def_set.begin();
              OP* def_op2 = *(def_set.begin()+1);
              if((OP_result(def_op1, 0) == OP_result(def_op2, 0) && 
                OP_result(def_op1, 1) == OP_result(def_op2, 1)) ||
                (OP_result(def_op1, 0) == OP_result(def_op2, 1) && 
                OP_result(def_op1, 1) == OP_result(def_op2, 0))) {
                false_tn = true_tn == OP_result(def_op1, 1) ? OP_result(def_op1, 0): OP_result(def_op1, 1);
                PARTITION_GRAPH_NODE* succ_node = (PARTITION_GRAPH_NODE*)
                  BB_MAP_Get(_bb_node_map, branch_targ_bb);
                succ_node -> Control_Pred(true_tn);
                succ_node = (PARTITION_GRAPH_NODE*) BB_MAP_Get(_bb_node_map, fall_thru_bb);
                succ_node -> Control_Pred(false_tn);
              }
            } else { 
              true_tn = NULL;
            }
          }
        }
        
        //traverse BB to construct node for compare instruction.
        //First, reverse scan result-predicates; second, reverse scan opnd
        //predicates
        OP* op;
        FOR_ALL_BB_OPs_REV(bb, op)
        {
            
            PG_CONTAINER* nodes_of_tn = 
                CXX_NEW(PG_CONTAINER, PRDB_pool);
            OP_MAP_Set( _tn_node_map, op, nodes_of_tn);

            for(INT i=0; i<OP_results(op); i++)
            {   
                TN* tn = OP_result(op, i);
                if (!TN_is_register(tn)||
                    TN_register_class(tn) != ISA_REGISTER_CLASS_predicate)
                {
                    nodes_of_tn -> push_back(NULL);
                    continue;
                }
                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                {
                    fprintf(TFile, 
                        "  == collecting infomation about op : \n");
                        Print_OP_No_SrcLine(op);
                }

                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                    fprintf(TFile, 
                        "  == collecting infomation of \n");
                    fPrint_TN (TFile, "%s \n", tn);
                }
                
                PARTITION_GRAPH_NODE* graph_node = NULL;

                //predicate equal to true_tn is mapped to branch target bb
                if (tn == true_tn){
                    graph_node = 
                        (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, 
                        branch_targ_bb);
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                        fprintf(TFile, 
                            "    it can share the node with BB_%d \n",
                            BB_id(branch_targ_bb));
                    }

                //predicate equal to false_tn is mapped to fall thru bb
                }else if (tn == false_tn){
                    graph_node = 
                        (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, 
                        fall_thru_bb);
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                        fprintf(TFile, 
                            "    it can share the node with BB_%d \n",
                            BB_id(fall_thru_bb));
                    }
                    
                }else
                {
                    graph_node = CXX_NEW(PARTITION_GRAPH_NODE(), PRDB_pool);
                    graph_node -> Index(partition_graph_node_number++);
                    Add_Node(graph_node);
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                        fprintf(TFile, 
                            "    create a new code for it \n");
                    }
                }
                nodes_of_tn -> push_back(graph_node);
                TN_OP_PAIR* tn_op = CXX_NEW(TN_OP_PAIR, PRDB_pool);
                tn_op->first = tn;
                tn_op->second = op;
                graph_node->Add_Related_TN (tn_op);
            }
        }
        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
            fprintf(TFile, 
                "  == collecting infomation about use TNs in BB_%d.\n", 
                BB_id(bb));
        }

        //create partition graph node for opnd predicates
        FOR_ALL_BB_OPs_REV(bb, op)
        {   
            PG_CONTAINER* nodes_of_tn = (PG_CONTAINER*)
                OP_MAP_Get( _tn_node_map, op);

            for(INT i=0 ; i< OP_opnds(op); i++)
            {
                TN* tn = OP_opnd(op, i);

                PARTITION_GRAPH_NODE* graph_node = NULL;

                
                if (!TN_is_register(tn) 
                    || TN_register_class(tn) != ISA_REGISTER_CLASS_predicate)
                {
                    nodes_of_tn -> push_back(NULL);
                    continue;
                }
                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                {
                    fprintf(TFile, 
                        "  == collecting infomation about op : \n");
                        Print_OP_No_SrcLine(op);
                }

                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                    fprintf(TFile, 
                        "  == collecting infomation of \n");
                    fPrint_TN (TFile, "%s \n", tn);
                }

                if (TN_is_true_pred(tn))
                {
                    graph_node = (PARTITION_GRAPH_NODE*)
                        BB_MAP_Get(_bb_node_map, bb);
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                        fprintf(TFile, 
                            "    it can share the node with its home bb. \n");
                    }
                    
                }else { 
                    DEF_KIND kind;
                    OP_CONTAINER def_set(PRDB_pool);
                    Find_Reaching_Def_Use_Set(&def_set, tn, op, bb, visited_bb, TRUE, PRDB_pool);
                    visited_bb = BB_SET_ClearD(visited_bb);
                    OP* use = TN_Reaching_Value_At_Op(tn, op, &kind, FALSE);
                    if ( def_set.size() == 1 
                        && OP_bb((*def_set.begin())) == bb )
    
                    {//predicate's use shares same node with its def(same bb)
                        OP *def = *def_set.begin();
                        PG_CONTAINER* node_set = (PG_CONTAINER*)
                            OP_MAP_Get(_tn_node_map,def);

                        for (INT j=0; j<OP_results(def); j++) {
                            if (OP_result(def,j) == tn){
                                graph_node = (*node_set)[j];
                                break;
                            }
                        }

                        Is_True(graph_node, (" False op without def tn!\n"));
                        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                            fprintf(TFile, 
                            "    it can share the node with its def \n");
                        }
                    //without def(same bb), predicate's use can share the node
                    // of its equal use
                    }else if (use && use != op && OP_Precedes(op, use) && 
                        OP_bb(use) == bb){

                        PG_CONTAINER* node_set = (PG_CONTAINER*)
                            OP_MAP_Get(_tn_node_map,use);

                        for (INT j=0; j<OP_opnds(use); j++) {
                            if (OP_opnd(use, j) == tn){
                                graph_node = (*node_set)[j+OP_results(use)];
                                break;
                            }
                        }

                        Is_True(graph_node, (" False op without use tn!\n"));
                        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                            fprintf(TFile, 
                            "    it can share the node with its use \n");
                        }

                    }else {
                        // it can share node with the reaching use of it, which 
                        // has the same reaching def set of it. 
                        OP_CONTAINER use_set(PRDB_pool);
                        Find_Reaching_Def_Use_Set(&use_set, tn, NULL, bb, visited_bb, FALSE, PRDB_pool);
                        visited_bb = BB_SET_ClearD(visited_bb);
            
                        BOOL has_eq_use = ((use_set.size() == 1) && 
                                    OP_bb(*use_set.begin()) == bb);
                        
                        OP *r_use = has_eq_use ? *use_set.begin():NULL;
                        OP_CONTAINER def_set_of_use(PRDB_pool);
                        if (r_use && r_use != op) {
                            Find_Reaching_Def_Use_Set
                                (&def_set_of_use, tn, r_use, OP_bb(r_use), visited_bb, TRUE, PRDB_pool); 
                            visited_bb = BB_SET_ClearD(visited_bb);
                        }
                            
                        if ( r_use && r_use != op && Same_OPS(&def_set, &def_set_of_use)) {
                            PG_CONTAINER* node_set = (PG_CONTAINER*)
                                OP_MAP_Get(_tn_node_map,r_use);
                            for (INT j=0; j<OP_opnds(r_use); j++) {
                                if (OP_opnd(r_use, j) == tn){
                                    graph_node = (*node_set)[j+OP_results(r_use)];
                                    break;
                                }
                            }

                            Is_True(graph_node, (" False op without use tn!\n"));
                            if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                                fprintf(TFile, 
                                "    it can share the node with its use \n");
                            }

                        } else {
                            graph_node = 
                                CXX_NEW(PARTITION_GRAPH_NODE(), PRDB_pool);
                            graph_node -> Index(partition_graph_node_number++);
                            Add_Node(graph_node);
                            if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)){
                                fprintf(TFile, 
                                "    create a new code for it \n");
                            }
                        }
                    }
                }

                nodes_of_tn -> push_back(graph_node);
                TN_OP_PAIR* tn_op = CXX_NEW(TN_OP_PAIR, PRDB_pool);
                tn_op->first = tn;
                tn_op->second = op;
                graph_node->Add_Related_TN (tn_op);
            }

        }
    }
}

//according to cfg and predicates analysis theory to form partitions.
//  for control predicates,partitions are formed from successors and predecessors
//  of basic blocks. For compare instructions, partitions are formed as follows
//  (bp is control predicate of bb, qp is qulifying predicate of ins, p0 is TRUE
//  node, p1 and p2 stand for target predicates nodes):
//  (1)normal: bp = p1 | p2 if (qp == p0);
//  (2)unconditional: pp = p1 | p2, while pp = (qp == p0)?bp:qp);
//  (3)and/andcm: pp = p | dummy and p = p_use | dummy, (p_use is a use point)
//  (4)or/orcm: bp = p_use | dummy and p_use = p | dummy, p = pini | dummy

void PARTITION_GRAPH::Look_For_Partition(REGION *region)
{
    BB *bb, *tmp_bb;
    OP* op;
    PARTITION_GRAPH_NODE* node, *cur_node;
    PT_ALLOC par = PT_ALLOC(PRDB_pool);
    INT i;

    //member variables for step2
    PARTITION_GRAPH_NODE *pp_node, *bp_node, *qp_node, *p1_node, *p2_node;

    //Step1: look for the partition relation of the control predicate
    TOPOLOGICAL_REGIONAL_CFG_ITER top_rg_iter(region->Regional_Cfg());

    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, "\n\nLook for partitions of CFG nodes of region.\n");
    for(top_rg_iter; top_rg_iter != 0; ++top_rg_iter)
    {
        if ((*top_rg_iter) -> Is_Region() && (*top_rg_iter) -> Pred_Num() <= 1
          && (*top_rg_iter)->Succ_Num() <= 1)
            continue;

        if ((*top_rg_iter) -> Is_Region())
            bb = Find_Region_Entry_BB(region);
        else
            bb=(*top_rg_iter) -> BB_Node();
        
        //we find base node "cur_node" as a partition parent
        cur_node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb);


        //Handle predecessors. Each bb_node predecessor is cur_node's child,
        // and region_node is mapped to _dummy_node. And current partition
        // is invalid if all its children is dummy.
        BOOL dup_dummy = FALSE;
        if ((*top_rg_iter)->Pred_Num () > 1)
        {
            PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
            if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                fprintf(TFile, "\n\nLook for partitions of CFG node preds.");

            if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                fprintf(TFile, "\nPartition: Parent--%d, ", cur_node->Index ());
            CFG_PRED_EDGE_ITER pre_iter(*top_rg_iter);
            for(pre_iter; pre_iter != 0; ++pre_iter)
            {
                if(Is_Critical_Edge(*pre_iter)||(*pre_iter)->Src()->Is_Region()) 
                {
                    if(!dup_dummy)
                    {
                        children->push_back(_dummy_node);
                        dup_dummy = TRUE;
                        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                            fprintf(TFile, "child--0   ");
                    }
                    continue;
                }
                tmp_bb = (*pre_iter)-> Src()->BB_Node();
                node = (PARTITION_GRAPH_NODE*)
                    BB_MAP_Get(_bb_node_map, tmp_bb);
                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                    fprintf(TFile, "child--%d   ", node->Index ());
                children->push_back(node);
            }
            if(children->size()<=1) {
                CXX_DELETE(children, PRDB_pool);
            } else {
                Add_Partition(cur_node, children);
            }
        }//end if of pred iter loop


        //handle successors. The same method with handling predecessors.
        dup_dummy = FALSE;
        if ((*top_rg_iter)->Succ_Num () > 1)
        {
            PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
            if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                fprintf(TFile, "\n\nLook for partitions of CFG node succs.");

            if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                fprintf(TFile, "\nPartition:Parent--%d,  ",cur_node->Index ());
            CFG_SUCC_EDGE_ITER succ_iter(*top_rg_iter);
            for(succ_iter; succ_iter != 0; ++succ_iter)
            {
                if(Is_Critical_Edge(*succ_iter)||(*succ_iter)->Dest()->Is_Region()) 
                {
                    if(!dup_dummy)
                    {
                        children->push_back(_dummy_node);
                        dup_dummy = TRUE;
                        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                            fprintf(TFile, "child--0   ");
                    }
                    continue;
                }
                tmp_bb = (*succ_iter)->Dest()-> BB_Node();
                node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, tmp_bb);
                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                    fprintf(TFile, "child--%d   ", node->Index ());
                children->push_back(node);
            }
            if(children->size()<=1) {
                CXX_DELETE(children, PRDB_pool);
            } else {
                Add_Partition(cur_node, children);
            }
        }//end if of succ iter
    }//end for topological iter loop

    //Step2:  look the partition relation of the materialized predicate
    TOPOLOGICAL_REGIONAL_CFG_ITER top_rg_iter1(region->Regional_Cfg());

    COMPARE_TYPE cmp_type;
    //Outer loop, traverse region's BB
    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, "\n\nLook for partitions of material predicate.");
    for(top_rg_iter1; top_rg_iter1 != 0; ++top_rg_iter1)
    {
        //Take entry bb of a nested entry region into account
        if ((*top_rg_iter1) -> Is_Region())
            continue;

        bb=(*top_rg_iter1) -> BB_Node();

        bp_node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb);
        //traverse BB to construct node for compare instruction
        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
            fprintf(TFile, "\n\nCurrent BB is BB%d.", BB_id(bb));
        FOR_ALL_BB_OPs(bb, op)
        {
            if (!BB_exit(bb)    
                && (OP_code(op) == TOP_mov_t_pr || OP_code(op) == TOP_mov_t_pr_i))
            {//clear all partition graph nodes if predicate register is defined.
                for (i=0; i<partition_graph_node_number; i++)
                {
                    _nodes[i]->Clear_Child_Partitions();
                    _nodes[i]->Clear_Parent_Partitions();
                }
            }
            if (OP_results(op)==0 || !TN_is_register(OP_result(op,0))|| 
                TN_register_class(OP_result(op,0)) != 
                ISA_REGISTER_CLASS_predicate)
                continue;
            INT16 node_pos=OP_PREDICATE_OPND+OP_results(op);
            
            //qp_node maps to guarding predicate, while pp_node maps to 
            //its home bb if "qp" is TRUE, and qp_node otherwise.
            qp_node = (*((PG_CONTAINER*)
                OP_MAP_Get(_tn_node_map, op)))[node_pos];
            pp_node = TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND))
                ?bp_node:qp_node;
            
            Is_True(OP_results(op)>1, ("invalid compare instruction!"));

            //p1_node and p2_node map to target predicates' nodes respectively
            // if it is node true predicate, and dummy otherwise.
            p1_node = TN_is_true_pred(OP_result(op, 0))?_dummy_node:
                (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[0];
            p2_node = TN_is_true_pred(OP_result(op, 1))?_dummy_node:
                (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[1];

            PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
            cmp_type = Compare_Type(OP_code(op));

            //Form corresponding partition as described before the function, 
            // in accordance with compare type.
            switch(cmp_type) {
            case COMPARE_TYPE_unc:
                if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                    fprintf(TFile, 
                    "\nUnconditional partition: Parent--%d child1--%d child2--%d",
                    pp_node->Index (), p1_node->Index (),p2_node->Index ());
                if (p1_node == pp_node) children->push_back(_dummy_node);
                else children->push_back(p1_node);
                if (p2_node == pp_node) children->push_back(_dummy_node);
                else children->push_back(p2_node);
                Add_Partition(pp_node, children);
                break;
            case COMPARE_TYPE_normal:
                if (TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND)))
                {
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                        fprintf(TFile, 
                        "\nNormal partition: Parent--%d child1--%d child2--%d",
                        bp_node->Index (),p1_node->Index (),p2_node->Index ());
                    children->push_back(p1_node);
                    children->push_back(p2_node);
                    Add_Partition(bp_node, children);
                }
                else
                {
                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                        fprintf(TFile, 
                        "\nNormal partition: Parent--%d child1--%d child2--dummy",
                        bp_node->Index (), p1_node->Index ());
                    children->push_back(p1_node);
                    children->push_back(_dummy_node);
                    Add_Partition(bp_node, children);

                    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                        fprintf(TFile, 
                        "\nNormal partition: Parent--%d child1--%d child2--dummy",
                        bp_node->Index (), p2_node->Index ());
                    children->push_back(p2_node);
                    children->push_back(_dummy_node);
                    Add_Partition(bp_node, children);
                }
                break;
            case COMPARE_TYPE_and:
            case COMPARE_TYPE_andcm:
                CXX_DELETE(children, PRDB_pool);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[0];
                Look_Partition_For_And_Type(OP_result(op, 0), bp_node, node, 
                    _dummy_node, op);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[1];
                Look_Partition_For_And_Type(OP_result(op, 1), bp_node, node, 
                    _dummy_node, op);
                break;
            case COMPARE_TYPE_or:
            case COMPARE_TYPE_orcm:
                CXX_DELETE(children, PRDB_pool);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[0];
                Look_Partition_For_Or_Type(OP_result(op, 0), bp_node, node,
                    _dummy_node, op);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[1];
                Look_Partition_For_Or_Type(OP_result(op, 1), bp_node, node,
                    _dummy_node, op);
                break;
            case COMPARE_TYPE_and_orcm:
                CXX_DELETE(children, PRDB_pool);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[0];
                Look_Partition_For_And_Type(OP_result(op, 0), bp_node, node, 
                    _dummy_node, op);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[1];
                Look_Partition_For_Or_Type(OP_result(op, 1), bp_node, node,
                    _dummy_node, op);
                break;
            case COMPARE_TYPE_or_andcm:
                CXX_DELETE(children, PRDB_pool);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[1];
                Look_Partition_For_Or_Type(OP_result(op, 1), bp_node, node, 
                    _dummy_node, op);
                node = (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op)))[0];
                Look_Partition_For_And_Type(OP_result(op, 0), bp_node, node,
                    _dummy_node, op);
                break;
            default:
                Is_True(FALSE, ("unexpected compare type!"));
            }//end for switch
        }//end for FOR_ALL_BB_OPS
    }//end for topological iterator
}

//build partition for 'or' compare type
void 
PARTITION_GRAPH::Look_Partition_For_Or_Type(TN* tn, PARTITION_GRAPH_NODE*
    parent, PARTITION_GRAPH_NODE* child1, PARTITION_GRAPH_NODE* child2, OP* op)
{
    if(tn == True_TN) return;
    DEF_KIND kind;
    INT i;
    TN* ref_tn;
    OP* ref_op=TN_Reaching_Value_At_Op(tn, op, &kind, FALSE);
    if (!ref_op || OP_bb(ref_op) != OP_bb(op))
    {
        PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
        children->push_back(child1);
        children->push_back(child2);
        Add_Partition(parent, children);
    }
    else
    {
        while (ref_op && OP_bb(ref_op) == OP_bb(op))
        {
            PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
            for(i=0; i<OP_opnds(ref_op); i++)
            {
                ref_tn = OP_opnd(ref_op, i);
                if(TNs_Are_Equivalent(tn, ref_tn))
                    break;
            }
            PARTITION_GRAPH_NODE* node;
            node = (*((PG_CONTAINER*)OP_MAP_Get
                (_tn_node_map, ref_op)))[i+OP_results(ref_op)];
            children->push_back(node);
            children->push_back(child2);
            Add_Partition(parent, children);

            PG_CONTAINER* sub_children = CXX_NEW(PG_CONTAINER, PRDB_pool);
            sub_children->push_back(child1);
            sub_children->push_back(child2);
            Add_Partition(node, sub_children);
            op = ref_op;
            tn = ref_tn;
            ref_op = TN_Reaching_Value_At_Op(ref_tn, op, &kind, FALSE);
        }
    }
    OP* init_op = NULL;
    if(init_op_info) init_op = (OP*)hTN_MAP_Get(init_op_info, tn);
    if (init_op && OP_bb(init_op) == OP_bb(op))
    {
        PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
        children->push_back(
            (*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, init_op)))[0]);
        children->push_back(child2);
        Add_Partition(child1, children);
    }
}

//build partition for 'and' compare type
void 
PARTITION_GRAPH::Look_Partition_For_And_Type(TN* tn, PARTITION_GRAPH_NODE*
    parent, PARTITION_GRAPH_NODE* child1, PARTITION_GRAPH_NODE* child2, OP* op)
{
    if(tn == True_TN) return;
    PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
    OP* init_op = NULL;
    if(init_op_info) init_op = (OP*)hTN_MAP_Get(init_op_info, tn);
    children->push_back(child1);
    children->push_back(child2);
    if (init_op && OP_bb(init_op) == OP_bb(op))
    {
        Add_Partition((*((PG_CONTAINER*)OP_MAP_Get(_tn_node_map, init_op)))[0],
            children);
    }
    else
    {
        Add_Partition(parent, children);
    }
    DEF_KIND kind;
    PARTITION_GRAPH_NODE* node;
    INT i;
    TN* ref_tn;
    OP* ref_op=TN_Reaching_Value_At_Op(tn, op, &kind, FALSE);
    while (ref_op && OP_bb(ref_op) == OP_bb(op))
    {
        PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
        for(i=0; i<OP_opnds(ref_op); i++)
        {
            ref_tn = OP_opnd(ref_op, i);
            if(TNs_Are_Equivalent(tn, ref_tn))
                break;
        }
        node = (*((PG_CONTAINER*)OP_MAP_Get
            (_tn_node_map, ref_op)))[i+OP_results(ref_op)];
        children->push_back(node);
        children->push_back(child2);
        Add_Partition(child1, children);
        op = ref_op;
        tn = ref_tn;
        ref_op = TN_Reaching_Value_At_Op(ref_tn, op, &kind, FALSE);
    }
}

//construct partitions to make unreachable nodes from root to be reachable.
void 
PARTITION_GRAPH::Complete_Partition_Graph() 
{   
    fflush(TFile);
    PG_CONTAINER::iterator iter;
    for (iter = _nodes.begin (); iter != _nodes.end (); ++iter)
    {
        PARTITION_GRAPH_NODE* node = *iter;
        if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
            fprintf(TFile, "\n\nStart to scan unreachable node!");
        //don't care about reachable nodes and those who have parent node
        if ( !node -> Is_Reachable() && node->Index ()!=0
            && node ->Child_Partitions().empty())
        {
            if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                fprintf(TFile, "\nunreachable node--%d!",(node)->Index());
            PG_CONTAINER rch_des_set(PRDB_pool);
            BOOL success = Find_Reachable_Descendant(&rch_des_set,node);
            PG_CONTAINER::iterator rch_iter;
            if(!success)
            {
                PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
                children->push_back(node);
                children->push_back(_dummy_node);
                Add_Partition(_root, children);
                continue;
            }

            PARTITION_GRAPH_NODE* lca_node = NULL;
            for ( rch_iter = rch_des_set.begin();
            rch_iter != rch_des_set.end();
            rch_iter++) {
                lca_node = Get_Lca (lca_node,*rch_iter);
            }
            PG_CONTAINER comp(PRDB_pool);
            comp.clear();
            comp.push_back(lca_node);
            FmtAssert(Subtract(&comp, &rch_des_set), 
                ("Unreachable node!!"));
            PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
            children->push_back(node);
            for (rch_iter = comp.begin (); rch_iter!=comp.end (); rch_iter++)
            {
                children->push_back(*rch_iter);
            }
            if(children->size() == 1)
                children->push_back(_dummy_node);
            Add_Partition(lca_node, children);
            CXX_DELETE(&rch_des_set, PRDB_pool);
            CXX_DELETE(&comp, PRDB_pool);
        }
    }

}

//the result is from one partition, and all the nodes in result are reachable
BOOL 
PARTITION_GRAPH::Find_Reachable_Descendant(PG_CONTAINER* result,
                                           PARTITION_GRAPH_NODE* node)
{
    PT_CONTAINER partitions = node -> Parent_Partitions();
    if (!partitions.size()) return FALSE;
    PT_CONTAINER::iterator iter;
    for (iter = partitions.begin (); iter != partitions.end (); iter++)
    {
        BOOL success = TRUE;
        PG_CONTAINER tmp_res;
        PG_CONTAINER* children = &((*iter)-> Child());
        PG_CONTAINER::iterator iter1;
        for (iter1 = children->begin(); 
        iter1 != children->end ();
        iter1++)
        {
            
            PARTITION_GRAPH_NODE* child = *iter1;
            if(child == _dummy_node)
            {
                success = FALSE;
                break;
            }
            if( child ->Is_Reachable())
            {
                if(child->Child_Partitions().size() <= 1)
                {
                    success = FALSE;
                    break;
                }
                tmp_res.push_back(child);
            }else {
                success &= Find_Reachable_Descendant(&tmp_res,child);
                if (!success)  
                {
                    tmp_res.clear();
                    break;
                }
            }
        }  
        if (success) 
        {
            for(iter1 = tmp_res.begin(); iter1 != tmp_res.end(); iter1++)
                result->push_back(*iter1);
            if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                fprintf(TFile, "\nReachable descendants found!!");
            return TRUE;
        }
    }
    return FALSE;
}

//find greatest comman descendant and least comman ancestor.
PARTITION_GRAPH_NODE* 
PARTITION_GRAPH::Get_Gcd(PARTITION_GRAPH_NODE* node1, 
                              PARTITION_GRAPH_NODE* node2)
{
    if(node1 == _dummy_node || node2 == _dummy_node) return NULL;
    PT_CONTAINER partitions;
    PT_CONTAINER::iterator iter;
    partitions = node2->Child_Partitions();
    for(iter = partitions.begin();iter!=partitions.end();iter++)
    {
        if(node1 == (*iter)->Parent()) return node2;
    }
    partitions = node1->Child_Partitions();
    for(iter = partitions.begin();iter!=partitions.end();iter++)
    {
        if(node2 == (*iter)->Parent()) return node1;
    }
    partitions = node1->Parent_Partitions ();
    INT level = -1;
    PARTITION_GRAPH_NODE* gcd = NULL;
    for (iter = partitions.begin (); iter!=partitions.end (); iter++)
    {
        PG_CONTAINER node_set = (*iter)->Child();
        PG_CONTAINER::iterator pg_iter;
        for (pg_iter=node_set.begin (); pg_iter!=node_set.end (); pg_iter++)
        {
            PARTITION_GRAPH_NODE* child = *pg_iter;
            if ( !child->Is_Reachable ()) continue;
            PARTITION_GRAPH_NODE* descendant = Get_Gcd(node2, child);
            if(descendant)
            {
                if(level == -1 || descendant->Level ()<level)
                {
                    level = descendant->Level ();
                    gcd = descendant;
                }
            }
        }
    }
    if(partitions.size() == 0)
    {
        partitions = node2->Parent_Partitions();
        for (iter = partitions.begin (); iter!=partitions.end (); iter++)
        {
            PG_CONTAINER node_set = (*iter)->Child();
            PG_CONTAINER::iterator pg_iter;
            for (pg_iter=node_set.begin (); pg_iter!=node_set.end (); pg_iter++)
            {
                PARTITION_GRAPH_NODE* child = *pg_iter;
                if ( !child->Is_Reachable ()) continue;
                PARTITION_GRAPH_NODE* descendant = Get_Gcd(node1, child);
                if(descendant)
                {
                    if(level == -1 || descendant->Level ()<level)
                    {
                        level = descendant->Level ();
                        gcd = descendant;
                    }
                }
            }
        }
    }
    return gcd;
}

PARTITION_GRAPH_NODE* 
PARTITION_GRAPH::Get_Lca(PARTITION_GRAPH_NODE* node1, 
                              PARTITION_GRAPH_NODE* node2)
{
    if(node1 == NULL) return node2;
    if(node2 == NULL) return node1;
    if(node1 == node2) return node1;
    PT_CONTAINER partitions;
    PT_CONTAINER::iterator iter;
    partitions = node1->Child_Partitions();
    INT level = -1;
    PARTITION_GRAPH_NODE* lca = NULL;
    for (iter = partitions.begin (); iter!=partitions.end (); iter++)
    {
        PARTITION_GRAPH_NODE* node = (*iter)->Parent();
        if(!node->Is_Reachable()) continue;
        PARTITION_GRAPH_NODE* ancestor = Get_Lca(node2, node);
        if(ancestor)
        {
            if(ancestor->Level ()>level)
            {
                level = ancestor->Level ();
                lca = ancestor;
            }
        }
    }
    if(partitions.size() == 0)
    {
        partitions = node2->Child_Partitions();
        for (iter = partitions.begin (); iter!=partitions.end (); iter++)
        {
            PARTITION_GRAPH_NODE* node = (*iter)->Parent();
            if(!node->Is_Reachable()) continue;
            PARTITION_GRAPH_NODE* ancestor = Get_Lca(node1, node);
            if(ancestor)
            {
                if(ancestor->Level ()>level)
                {
                    level = ancestor->Level ();
                    lca = ancestor;
                }
            }
        }
    }
    return lca;
}

//add new partition if this partition doesn't exist
void
PARTITION_GRAPH::Add_Partition(PARTITION_GRAPH_NODE* parent,
                               PG_CONTAINER* children)
{
    PARTITION* partition = CXX_NEW(PARTITION(), PRDB_pool);
    partition->Parent(parent);
    PG_CONTAINER::iterator iter;
    for(iter = children->begin(); iter != children->end(); iter++)
    {
        Is_True(parent->Index()!=(*iter)->Index(), ("Illegal partition"));
        partition->Add_Child(*iter);
    }
    PT_CONTAINER par_list = parent->Parent_Partitions();
    for (INT i = 0; i<par_list.size (); i++)
    {
        if (*partition == par_list[i])
        {
            CXX_DELETE(partition, PRDB_pool);
            return;
        }
    }
    parent -> Add_Parent_Partition(partition);

    for (iter = children->begin(); iter != children->end(); iter++)
    {
        PARTITION_GRAPH_NODE* child = *iter;
        child -> Add_Child_Partition(partition);
        if(parent->Is_Reachable())
        {
            if ( child -> Level() < (parent -> Level() +1))
                child -> Level(parent -> Level() +1);
        }
    }
}

PG_CONTAINER* 
PARTITION_GRAPH::Get_Subset_Nodes(PARTITION_GRAPH_NODE* node)
{
    PG_CONTAINER* result = CXX_NEW(PG_CONTAINER, PRDB_pool);
    result->push_back(node);
    PT_CONTAINER partitions = node->Parent_Partitions();
    PT_CONTAINER::iterator iter;
    for(iter = partitions.begin(); iter != partitions.end();++iter)
    {
        PARTITION* part = *iter;
        PG_CONTAINER children = part->Child();
        PG_CONTAINER::iterator pg_iter;
        for(pg_iter = children.begin(); pg_iter != children.end();++pg_iter)
        {
            if(*pg_iter == _dummy_node) continue;
            PG_CONTAINER *tmp_res = Get_Subset_Nodes(*pg_iter);
            PG_CONTAINER::iterator tmp_iter;
            for(tmp_iter = tmp_res->begin(); tmp_iter != tmp_res->end(); ++tmp_iter)
                result->push_back(*tmp_iter);
        }
    }
    if(result->size() > 1) Reduce(result, FALSE);
    return result;//todo: how do I guarantee no redundant node?
}

//computing predicates relation database
void 
PARTITION_GRAPH::Pre_Computing(){

    // initialize disjoint and subset information

    for (INT i=0; i<partition_graph_node_number; i++)
    {
        BV_VECTOR bvector(PRDB_pool);
        for(INT j=0; j<partition_graph_node_number; j++)
        {
            bvector.push_back(FALSE);
        }
        _disjoint_relation.push_back (bvector);
        _subset_relation.push_back (bvector);

        //initialize node-i is subset of node-i
        if (i != 0) _subset_relation[i][i] = TRUE;
    }
    if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)) {
        fprintf(TFile, "number_of_ptn_node is %d!\n", partition_graph_node_number);
        fprintf(TFile, "\nInitialization accomplished!\n");
    }

    
    for (INT i=0; i<partition_graph_node_number; i++){
        PARTITION_GRAPH_NODE* node = _nodes[i];

        // step1: compute the subset and superset relation
        if ( node == _dummy_node) continue;
        if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
            fprintf(TFile, "\nReady to set subset relation node-%d!", node->Index());
        PG_CONTAINER* subset = Get_Subset_Nodes(node);
        PG_CONTAINER:: iterator iter;
        for(iter = subset->begin(), ++iter; iter != subset->end(); ++iter)
            Set_Subset(*iter, node);
        CXX_DELETE(subset, PRDB_pool);
        if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
            fprintf(TFile, "\nsubset relation accomplished !");
                
        PT_CONTAINER& partitions = node -> Parent_Partitions();
        PT_CONTAINER::iterator pt_iter;
        for (pt_iter = partitions.begin (); 
        pt_iter != partitions.end (); 
        pt_iter++)
        {
            PARTITION* partition = (*pt_iter);
            PG_CONTAINER& children = partition -> Child();
            PG_CONTAINER::iterator pg_iter;
            for (pg_iter = children.begin();
            pg_iter != children.end();
            pg_iter++){
                PARTITION_GRAPH_NODE* child1 = *pg_iter;
                if ( child1 == _dummy_node ) continue;

                // step2: compute the disjoint relation 
                PG_CONTAINER::iterator pg_iter1;
                for (pg_iter1 = pg_iter;
                pg_iter1 != children.end();
                pg_iter1++){
                    if (pg_iter == pg_iter1 || *pg_iter1 == _dummy_node) 
                        continue;
                    if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                        fprintf(TFile, "\nReady to set disjoint relation!");
                    Set_Disjoint(child1, *pg_iter1);
                    Rec_Set_Disjoint(child1, *pg_iter1);
                    if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
                        fprintf(TFile, "\nDisjoint relation accomplished!");
                }

            }
        }
    }
 
}

//Find the corresponding node for tn in node_vector mapping to op
PARTITION_GRAPH_NODE*
PARTITION_GRAPH::Find_Node_In_OP(TN_OP_PAIR* tn_op)
{
    Is_True(tn_op, ("Input is NULL tn_op and can't get its node"));
    TN* tn = tn_op->first;
    const OP* op = tn_op->second;
    PG_CONTAINER* node_set = (PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op);
    if(!node_set){
        // There are too much this DevWarn to be nuisance
        //DevWarn("Current OP-%s has no ptn node!", TOP_Name(OP_code(op)));
        return NULL;
    }
    //Is_True(node_set, "current OP has no ptn node");
    INT i;
    for(i=0;i<(OP_results(op)+OP_opnds(op));i++)
    {
        if(i<OP_results(op))
        {
            if(tn == OP_result(op, i))
                break;
        }
        else
        {
            if(tn == OP_opnd(op, i-OP_results(op)))
                break;
        }
    }
    Is_True(i<(OP_results(op)+OP_opnds(op)), ("Invalid OP and TN!"));
    return (*node_set)[i];
}

// set all descedents of child1 disjoint with child2
void 
PARTITION_GRAPH::Rec_Set_Disjoint(PARTITION_GRAPH_NODE* child1,
                              PARTITION_GRAPH_NODE* child2)
{
    PG_CONTAINER* nodes1 = Get_Subset_Nodes(child1);
    PG_CONTAINER* nodes2 = Get_Subset_Nodes(child2);
    PG_CONTAINER::iterator iter1, iter2;
    for(iter1 = nodes1->begin(); iter1 != nodes1->end(); ++iter1)
        {
        if(*iter1 == _dummy_node) continue;
        for(iter2 = nodes2->begin(); iter2 != nodes2->end();++iter2)
        {
            if(*iter2 == _dummy_node) continue;
            Set_Disjoint(*iter1, *iter2);
        }
    }
    CXX_DELETE(nodes1, PRDB_pool);
    CXX_DELETE(nodes2, PRDB_pool);
    
}

void
PARTITION_GRAPH::Set_Disjoint(PARTITION_GRAPH_NODE* child1,
                              PARTITION_GRAPH_NODE* child2)
{
    if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE)) {
        fprintf(TFile, "true_disjoint_relation found!\n");
        fprintf(TFile, "\tnode-%d and node-%d are disjoint!\t", 
            child1->Index (), child2->Index ());
    }
    _disjoint_relation[child1 -> Index()][child2 -> Index()] = TRUE;
    _disjoint_relation[child2 -> Index()][child1 -> Index()] = TRUE;
}

void 
PARTITION_GRAPH::Set_Subset(PARTITION_GRAPH_NODE* child,
                              PARTITION_GRAPH_NODE* node)
{
    if(Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, "\tnode-%d and node-%d are subset!\t",
            child->Index (), node->Index ());
    Is_True(!_subset_relation[node->Index()][child->Index()], 
            ("cycle partition!"));
    _subset_relation[child -> Index()][node -> Index()] = TRUE;
}

BOOL 
PARTITION_GRAPH::Subtract(PG_CONTAINER* result, PARTITION_GRAPH_NODE* des)
{
    PG_CONTAINER::iterator iter;
    PARTITION_GRAPH_NODE* node;
    for(iter = result->begin(); iter != result->end(); iter++)
    {
        if(*iter == des)
        {
            iter = result->erase(iter);
            return TRUE;
        }
    }
        PT_CONTAINER prt_set = des->Child_Partitions ();
        PT_CONTAINER::iterator pt_iter;
        for (pt_iter = prt_set.begin (); pt_iter!=prt_set.end (); pt_iter++)
        {
            node = (*pt_iter)->Parent();
            if(!node->Is_Reachable()) continue;
            if(Subtract(result, node))
            {
                PG_CONTAINER tmp_result;
                tmp_result.clear();
                Find_Sibling(&tmp_result, node, des);
                PG_CONTAINER::iterator pg_iter;
                for(pg_iter=tmp_result.begin();pg_iter!=tmp_result.end();pg_iter++)
                    result->push_back(*pg_iter);
                return TRUE;
            }
        }
    return FALSE;
}

BOOL 
PARTITION_GRAPH::Subtract(PG_CONTAINER* result, PG_CONTAINER* set)
{
    PG_CONTAINER::iterator iter;
    for (iter=set->begin (); iter!=set->end (); iter++)
    {
        if(!Subtract(result, *iter))
        {
            result->clear();
            if(iter == set->begin()) return FALSE;
            else {
                result->push_back(_dummy_node);
                return TRUE;
            }
        }
    }
    return TRUE;
}

void 
PARTITION_GRAPH::Find_Sibling(PG_CONTAINER *result,
                PARTITION_GRAPH_NODE *anc, PARTITION_GRAPH_NODE *des)
{
    if(des == _dummy_node) return;

    PT_CONTAINER part_set = des->Child_Partitions ();
    PT_CONTAINER::iterator iter;
    PARTITION_GRAPH_NODE* node;
    for (iter = part_set.begin (); iter != part_set.end (); iter++)
    {
        node = (*iter)->Parent();
        if (node == anc) break;
    }
    if (iter == part_set.end ()) return;//anc is not ancestor of des.
    PARTITION* part = *iter;
    PG_CONTAINER node_set = part->Child ();
    PG_CONTAINER::iterator pg_iter;
    for (pg_iter = node_set.begin (); pg_iter!=node_set.end (); pg_iter++)
    {
        if ((*pg_iter) == des || (*pg_iter) == _dummy_node) continue;
        result->push_back (*pg_iter);
    }
}

void PARTITION_GRAPH::Mark_Level(PARTITION_GRAPH_NODE *node)
{
    _visited[node->Index()] = TRUE;
    
    if(node == _dummy_node) return;
    PT_CONTAINER partitions = node->Child_Partitions ();
    PT_CONTAINER::iterator iter;
    PARTITION_GRAPH_NODE* pnode;
    //This is unreachable node if its level less than 0
    if (node->Level ()<0)
    {
        for(iter = partitions.begin (); iter!=partitions.end (); ++iter)
        {
            pnode = (*iter)->Parent();
            if(pnode->Level ()>node->Level ()+1)
                node->Level (pnode->Level ()+1);
        }
    }
    partitions = node->Parent_Partitions ();
    if(!partitions.size()) return;
    for (iter = partitions.begin (); iter!=partitions.end (); ++iter)
    {
        PG_CONTAINER nd_set = (*iter)->Child();
        PG_CONTAINER::iterator piter;
        for (piter = nd_set.begin (); piter != nd_set.end (); ++piter)
        {
            pnode = (*piter);
            if(pnode == _dummy_node) continue;
            if(pnode->Level ()<node->Level ()+1)
                pnode->Level (node->Level ()+1);
            if (!_visited[pnode->Index()])
                Mark_Level(pnode);
        }
    }

}

BOOL 
PARTITION_GRAPH::Is_Disjoint(TN_OP_PAIR tp1, TN_OP_PAIR tp2)
{
    Is_True(tp1.first&&tp2.first&&tp1.second&&tp2.second,
        ("Illegal input with NULL TN or OP for disjoint!"));
    if(tp1.first == True_TN || tp2.first == True_TN) return FALSE;//Rule out p0 op
    if(Compare_Type(OP_code(tp1.second)) == COMPARE_TYPE_unc ||
       Compare_Type(OP_code(tp2.second)) == COMPARE_TYPE_unc) return FALSE;
    INT index1,index2;
    PARTITION_GRAPH_NODE* node = NULL;
    node = Find_Node_In_OP(&tp1);
    if(!node) return FALSE;
    Is_True(node, ("Can't get tp1's tn node!"));
    index1 = node->Index ();

    node = Find_Node_In_OP(&tp2);
    if(!node) return FALSE;
    Is_True(node, ("Can't get tp2's tn node!"));
    index2 = node->Index ();
    if(Get_Trace(TP_A_PRDB, TT_PRDB_APP)) {
        const OP* op1, *op2;
        op1 = tp1.second;
        op2 = tp2.second;
        fprintf(TFile, "number_of_query_PU_%d\n", Current_PU_Count());
        if(OP_opnd(op1, OP_PREDICATE_OPND) != True_TN && OP_opnd(op2, OP_PREDICATE_OPND) != True_TN)
        {
            fprintf(TFile, "non_pr0_predicate_query_PU_%d\n", Current_PU_Count());
            if(_disjoint_relation[index1][index2])
                fprintf(TFile, "true_disjoint_PU_%d\n", Current_PU_Count());
            else
            {
                fprintf(TFile, "non_disjoint_PU_%d :", Current_PU_Count());
                Print_OP(op1);
                Print_OP(op2);
                fprintf(TFile, "\n");
            }
        }
    }

    return _disjoint_relation[index1][index2];

}

void 
PARTITION_GRAPH::Add_Relation(PARTITION_GRAPH_NODE* child,
                      PARTITION_GRAPH_NODE* parent)
    {
        INT index1, index2;
        index1 = parent->Index();
        index2 = child->Index();
        PG_CONTAINER::iterator iter;
        BV_VECTOR bvector(PRDB_pool);
        INT i = _disjoint_relation.size()-1;
        for(; i >= 0; i--)
        {
            _disjoint_relation[i].push_back(FALSE);
            _subset_relation[i].push_back(FALSE);
            bvector.push_back(FALSE);
        }
        bvector.push_back(TRUE);
        _disjoint_relation.push_back (bvector);
        _subset_relation.push_back (bvector);
        for(iter=_nodes.begin();iter!=_nodes.end();iter++)
        {
            if(_disjoint_relation[index1][(*iter)->Index()])
            {
                _disjoint_relation[index2][(*iter)->Index()]=TRUE;
                _disjoint_relation[(*iter)->Index()][index2]=TRUE;
            }
            if(_subset_relation[(*iter)->Index()][index1])
                _subset_relation[(*iter)->Index()][index2] = TRUE;
        }
        _subset_relation[index1][index2] = TRUE;
    }

BOOL 
PARTITION_GRAPH::Is_Disjoint(BB* bb1, BB* bb2)
{
    Is_True(bb1&&bb2,("Illegal input with NULL BB for disjoint!"));
    INT index1,index2;
    PARTITION_GRAPH_NODE* node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb1);
    index1 = node->Index ();
    node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb2);
    index2 = node->Index ();
    return _disjoint_relation[index1][index2];
}

BOOL 
PARTITION_GRAPH::Is_Subset(TN_OP_PAIR tp1, TN_OP_PAIR tp2)
{
    Is_True(tp1.first&&tp2.first&&tp1.second&&tp2.second,
        ("Illegal input with NULL TN or OP for subset!"));
    INT index1,index2;
    PARTITION_GRAPH_NODE* node = NULL;
    node = Find_Node_In_OP(&tp1);
    if(!node) return FALSE;
    Is_True(node, ("Can't get tp1's tn node!"));
    index1 = node->Index ();

    node = Find_Node_In_OP(&tp2);
    if(!node) return FALSE;
    Is_True(node, ("Can't get tp2's tn node!"));
    index2 = node->Index ();
    return _subset_relation[index1][index2];

}

BOOL 
PARTITION_GRAPH::Is_Subset(BB* bb1, BB* bb2)
{
    Is_True(bb1&&bb2,("Illegal input with NULL BB for subset!"));
    INT index1,index2;
    PARTITION_GRAPH_NODE* node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb1);
    index1 = node->Index ();
    node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb2);
    index2 = node->Index ();
    return _subset_relation[index1][index2];
}

BOOL 
PARTITION_GRAPH::Is_Superset(TN_OP_PAIR tp1, TN_OP_PAIR tp2)
{
    Is_True(tp1.first&&tp2.first&&tp1.second&&tp2.second,
        ("Illegal input with NULL TN or OP for superset!"));
    INT index1,index2;
    PARTITION_GRAPH_NODE* node = NULL;
    node = Find_Node_In_OP(&tp1);
    Is_True(node, ("Can't get tp1's tn node!"));
    index1 = node->Index ();

    node = Find_Node_In_OP(&tp2);
    Is_True(node, ("Can't get tp2's tn node!"));
    index2 = node->Index ();
    return _subset_relation[index2][index1];

}

BOOL 
PARTITION_GRAPH::Is_Superset(BB* bb1, BB* bb2)
{
    Is_True(bb1&&bb2,("Illegal input with NULL BB for superset!"));
    INT index1,index2;
    PARTITION_GRAPH_NODE* node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb1);
    index1 = node->Index ();
    node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb2);
    index2 = node->Index ();
    return _subset_relation[index2][index1];
}

BOOL 
PARTITION_GRAPH::Get_Complementary(TP_CONTAINER* result, TN_OP_PAIR tp,
                                   TN_OP_PAIR base_tp)
{
    Is_True(result&&tp.first&&tp.second&&base_tp.first&&base_tp.second, 
        ("Illegal input with NULL value for complementary!"));
    PG_CONTAINER::iterator iter;
    PARTITION_GRAPH_NODE* node, *base_node;
    node = Find_Node_In_OP(&tp);
    Is_True(node, ("Can't get tp's tn node!"));
    base_node = Find_Node_In_OP(&base_tp);
    Is_True(base_node, ("Can't get base_tp's tn node!"));
    PG_CONTAINER tmp_result;
    tmp_result.clear();
    tmp_result.push_back(base_node);
    if(Subtract(&tmp_result, node))
    {
        for(iter = tmp_result.begin ();iter!=tmp_result.end ();iter++)
        {
            node = *iter;
            if(node->Get_TNs().empty()) continue;
            result->push_back(node->Get_TNs ()[0]);
        }
        return TRUE;
    }
    return FALSE;
}

BOOL 
PARTITION_GRAPH::Get_Complementary(TP_CONTAINER* result, TN_OP_PAIR tp,
                                   BB* base_bb)
{
    PG_CONTAINER::iterator iter;
    PARTITION_GRAPH_NODE* node, *base_node;
    node = Find_Node_In_OP(&tp);
    Is_True(node, ("Can't get tp's tn node!"));
    base_node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, base_bb);
    PG_CONTAINER tmp_result;
    tmp_result.clear();
    tmp_result.push_back(base_node);
    if(Subtract(&tmp_result, node))
    {
        for(iter = tmp_result.begin ();iter!=tmp_result.end ();iter++)
        {
            node = *iter;
            if(node->Get_TNs().empty()) continue;
            result->push_back(node->Get_TNs ()[0]);
        }
        return TRUE;
    }
    return FALSE;
}

TN* 
PARTITION_GRAPH::Get_Complementary(TN_OP_PAIR tp, BB* base_bb)
{
    if(BB_succs_len(base_bb) == 2 && BB_kind(base_bb) == BBKIND_LOGIF) {
        TN* tn = tp.first;
        BBLIST* succs = BB_succs(base_bb);
        BB* fall_bb = BBLIST_item(succs);
        BB* targ_bb = BBLIST_item(BBLIST_next(succs));
        if(fall_bb && targ_bb && BB_preds_len(fall_bb) == 1 && BB_preds_len(targ_bb) == 1
          && Home_Region(base_bb) == Home_Region(fall_bb) &&
          Home_Region(base_bb) == Home_Region(targ_bb)) {
            PARTITION_GRAPH_NODE* fall_node = (PARTITION_GRAPH_NODE*) BB_MAP_Get(_bb_node_map, fall_bb);
            PARTITION_GRAPH_NODE* targ_node = (PARTITION_GRAPH_NODE*) BB_MAP_Get(_bb_node_map, targ_bb);
            if(tn == fall_node->Control_Pred()) return targ_node->Control_Pred();
            if(tn == targ_node->Control_Pred()) return fall_node->Control_Pred();
        }
    }
    PARTITION_GRAPH_NODE* node, *base_node;
    node = Find_Node_In_OP(&tp);
    Is_True(node, ("Can't get tp's tn node!"));
    base_node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, base_bb);
    PG_ALLOC res_mem(PRDB_pool);
    PG_CONTAINER tmp_result(res_mem);
    Find_Sibling(&tmp_result, base_node, node);
    if(tmp_result.size() != 1 ) return NULL;
    PARTITION_GRAPH_NODE* com_node = *tmp_result.begin();
    TN_OP_PAIR* tn_op = *com_node->Get_TNs().begin();
    Is_True(tn_op->second, ("TN's complement should exist!"));
    return tn_op->first;
}

BOOL 
PARTITION_GRAPH::Is_Complementary(TN_OP_PAIR tp1,TN_OP_PAIR tp2,
                                  TN_OP_PAIR tp3)
{
    Is_True(tp1.first&&tp2.first&&tp3.first&&tp1.second&&tp2.second&&tp3.second,
        ("Illegal input with NULL TN or OP for comlementary!"));
    PG_CONTAINER* node_set;
    PG_CONTAINER::iterator iter;
    PT_CONTAINER partitions;
    PT_CONTAINER::iterator pt_iter;
    PARTITION_GRAPH_NODE* node1, *node2, *base_node;
    node1 = Find_Node_In_OP(&tp1);
    Is_True(node1, ("Can't get tp1's tn node!"));
    node2 = Find_Node_In_OP(&tp2);
    Is_True(node2, ("Can't get tp2's tn node!"));
    base_node = Find_Node_In_OP(&tp3);
    Is_True(base_node, ("Can't get tp3's tn node!"));
    if(node1 == node2) return FALSE;
    partitions = base_node->Parent_Partitions ();
    for (pt_iter=partitions.begin ();  pt_iter!=partitions.end (); pt_iter++)
    {
        BOOL Ex_node1 = FALSE;
        BOOL Ex_node2 = FALSE;
        node_set = &((*pt_iter)->Child());
        for(iter = node_set->begin (); iter!=node_set->end (); iter++)
        {
            if ((*iter) == node1) Ex_node1 = TRUE;
            if ((*iter) == node2) Ex_node2 = TRUE;
        }
        if (Ex_node1 && Ex_node2) return TRUE;
    }
    return FALSE;
}


BOOL 
PARTITION_GRAPH::Is_Complementary(TN_OP_PAIR tp1,TN_OP_PAIR tp2, BB* bb)
{
    Is_True(tp1.first&&tp1.second&&tp2.first&&tp2.second&&bb, 
        ("Illegal input with NULL value for complementary!"));
    PG_CONTAINER* node_set;
    PG_CONTAINER::iterator iter;
    PT_CONTAINER partitions;
    PT_CONTAINER::iterator pt_iter;
    PARTITION_GRAPH_NODE* node1, *node2, *base_node;
    base_node = (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, bb);
    node1 = Find_Node_In_OP(&tp1);
    Is_True(node1, ("Can't get tp1's tn node!"));
    node2 = Find_Node_In_OP(&tp2);
    Is_True(node2, ("Can't get tp2's tn node!"));
    if(node1 == node2) return FALSE;
    partitions = base_node->Parent_Partitions ();
    for (pt_iter=partitions.begin ();  pt_iter!=partitions.end (); pt_iter++)
    {
        BOOL Ex_node1 = FALSE;
        BOOL Ex_node2 = FALSE;
        node_set = &((*pt_iter)->Child());
        for(iter = node_set->begin (); iter!=node_set->end (); iter++)
        {
            if ((*iter) == node1) Ex_node1 = TRUE;
            if ((*iter) == node2) Ex_node2 = TRUE;
        }
        if (Ex_node1 && Ex_node2) return TRUE;
    }
    return FALSE;
}

BOOL 
PARTITION_GRAPH::Is_Complementary(PARTITION_GRAPH_NODE* node1,
                                  PARTITION_GRAPH_NODE* node2,
                                  PARTITION_GRAPH_NODE* base_node)
{
    Is_True(node1&&node2&&base_node, ("Illegal input with NULL for complementary!"));
    if(node1 == node2) return FALSE;
    PG_CONTAINER* node_set;
    PG_CONTAINER::iterator iter;
    PT_CONTAINER partitions;
    PT_CONTAINER::iterator pt_iter;
    partitions = base_node->Parent_Partitions ();
    for (pt_iter=partitions.begin ();  pt_iter!=partitions.end (); pt_iter++)
    {
        BOOL Ex_node1 = FALSE;
        BOOL Ex_node2 = FALSE;
        node_set = &((*pt_iter)->Child());
        for(iter = node_set->begin (); iter!=node_set->end (); iter++)
        {
            if ((*iter) == node1) Ex_node1 = TRUE;
            if ((*iter) == node2) Ex_node2 = TRUE;
        }
        if (Ex_node1 && Ex_node2) return TRUE;
    }
    return FALSE;
}

BOOL 
PARTITION_GRAPH::Cycle_Detector()
{
    PG_CONTAINER::iterator iter;
    for (iter = _nodes.begin (); iter != _nodes.end (); iter++)
    {
        PARTITION_GRAPH_NODE* node = *iter;
        if(node == _dummy_node) continue;
        PT_CONTAINER* partitions = &(node->Child_Partitions ());
        PT_CONTAINER::iterator pt_iter;
        for (pt_iter=partitions->begin();pt_iter!=partitions->end();pt_iter++)
        {
            PARTITION_GRAPH_NODE* parent = (*pt_iter)->Parent();
            if (_subset_relation[parent->Index ()][node->Index ()])
            {
                printf("\nNode-%d and Node-%d are cycle!!\n",
                    node->Index(),parent->Index());
                return TRUE;
            }
        }
    }
    return FALSE;
}

BOOL
PARTITION_GRAPH::Illegal_Partition()
{
    PG_CONTAINER::iterator iter;
    for (iter = _nodes.begin (); iter != _nodes.end (); iter++)
    {
        PARTITION_GRAPH_NODE* node = *iter;
        if(node == _dummy_node) continue;
        PT_CONTAINER* partitions = &(node->Child_Partitions ());
        PT_CONTAINER::iterator pt_iter1, pt_iter2;
        for (pt_iter1=partitions->begin();pt_iter1!=partitions->end();pt_iter1++)
        {
            PARTITION_GRAPH_NODE* father = (*pt_iter1)->Parent();
            for(pt_iter2=pt_iter1;pt_iter2!=partitions->end ();pt_iter2++)
            {
                PARTITION_GRAPH_NODE* mother = (*pt_iter2)->Parent();
                if (_disjoint_relation[father->Index ()][mother->Index ()])
                {
                    printf("\nNode-%d and node-%d are illegal!!\n",
                        father->Index(),mother->Index());
                    return TRUE;
                }
            }
        }
    }
    return FALSE;
}

float
PARTITION_GRAPH::Get_Probability(TN* tn)
{
    float rt_freq,tn_freq;
    BB* bb = BB_SET_Choose(_root->Get_BBs());
    if(bb == BB_SET_CHOOSE_FAILURE) return 0.0;
    rt_freq = BB_freq(bb);
    if(!frequency_of_predicates)
        return 0.0;
    else
    {
        tn_freq = hTN_MAPf_Get(frequency_of_predicates, tn);
        return (tn_freq/rt_freq);
    }
}

float 
PARTITION_GRAPH::Get_Probability(TN* tn1, TN* tn2)
{
    float tn_prob1, tn_prob2;
    tn_prob1 = Get_Probability(tn1);
    tn_prob2 = Get_Probability(tn2);
    if(tn_prob2 > 0)
        return (tn_prob1/tn_prob2);
    else
        return 0.0;
}

void 
PARTITION_GRAPH::Update(OP* op, BB* tgt_BB,CODE_MOTION_TYPE type)
{
    TOP opcode = OP_code(op);
    if(Compare_Type(opcode) == -1 && !OP_has_predicate(op)) return;
    switch(type) {
    case COPY_TO:
        Copy_To(op, tgt_BB);
        break;
    case MOVE_TO:
        Move_To(op, tgt_BB);
        break;
    case DELETE:
        Delete(op);
        break;
    default:
        Is_True(FALSE, ("Unknown code motion type!"));
        return;
    }
}

void 
PARTITION_GRAPH::Copy_To(OP* op, BB* tgt_BB) 
{
    Is_True(tgt_BB, ("move op to NULL BB!"));
    TOP opcode = OP_code(op);
    PARTITION_GRAPH_NODE* qp_parent = NULL;
    PARTITION_GRAPH_NODE* tgt_node = 
        (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, tgt_BB);
    PG_CONTAINER* op_node_set = CXX_NEW(PG_CONTAINER, PRDB_pool);
    if(!OP_has_predicate(op)) return;
    if(OP_opnd(op, OP_PREDICATE_OPND) == True_TN)
        qp_parent = tgt_node;
    else
    {
        DEF_KIND kind;
        TN* qp = OP_opnd(op,OP_PREDICATE_OPND);
        OP* def_op = TN_Reaching_Value_At_Op(qp, op, &kind, TRUE);
        BB* bb = def_op ? OP_bb(def_op) : NULL;
        if(bb == tgt_BB || (bb && BB_MAP_Get(_bb_node_map, bb) == BB_MAP_Get(_bb_node_map, tgt_BB)))
        {
            TN_OP_PAIR* tn_op = CXX_NEW(TN_OP_PAIR, PRDB_pool);
            tn_op->first = qp;
            tn_op->second = def_op;
            qp_parent = Find_Node_In_OP(tn_op);
            Is_True(qp_parent, ("copy op's def_op should have partition graph node!!"));
            tn_op->second = op;
            qp_parent->Add_Related_TN(tn_op);
        }
        else
        {
            qp_parent = CXX_NEW(PARTITION_GRAPH_NODE(), PRDB_pool);
            qp_parent -> Index(partition_graph_node_number++);
            PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
            children->push_back(qp_parent);
            children->push_back(_dummy_node);
            Add_Partition(tgt_node, children);

            //maintain disjoint relations
            Add_Relation(qp_parent, tgt_node);
        }
    }

    if(Compare_Type(opcode) >= 0 && OP_result(op, 0) != True_TN
        && OP_result(op, 1) != True_TN)
    {
        PARTITION_GRAPH_NODE* p1_new = CXX_NEW(PARTITION_GRAPH_NODE(),
            PRDB_pool);
        p1_new-> Index(partition_graph_node_number++);
        TN_OP_PAIR* tn_op = CXX_NEW(TN_OP_PAIR, PRDB_pool);
        tn_op->first = OP_result(op, 0);
        tn_op->second = op;
        p1_new->Add_Related_TN (tn_op);
        PARTITION_GRAPH_NODE* p2_new = CXX_NEW(PARTITION_GRAPH_NODE(),
            PRDB_pool);
        p2_new-> Index(partition_graph_node_number++);
        tn_op = CXX_NEW(TN_OP_PAIR, PRDB_pool);
        tn_op->first = OP_result(op, 1);
        p2_new->Add_Related_TN (tn_op);
        PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
        children->push_back(p1_new);
        children->push_back(p2_new);
        Add_Partition(qp_parent, children);
        Add_Relation(p1_new, qp_parent);
        Add_Relation(p2_new, qp_parent);
        _disjoint_relation[p1_new->Index()][p2_new->Index()] = TRUE;
        op_node_set->push_back(p1_new);
        op_node_set->push_back(p2_new);
    }
    else {
      PG_CONTAINER* children = CXX_NEW(PG_CONTAINER, PRDB_pool);
      for(INT i = 0; i<OP_results(op); i++) {
        if(TN_is_constant(OP_result(op,i))||TN_is_true_pred(OP_result(op, i))||
          TN_register_class(OP_result(op,i)) != ISA_REGISTER_CLASS_predicate)
        {
            op_node_set->push_back(NULL);
            continue;
        }
        PARTITION_GRAPH_NODE* p_new = CXX_NEW(PARTITION_GRAPH_NODE(),
            PRDB_pool);
        p_new-> Index(partition_graph_node_number++);
        TN_OP_PAIR* tn_op = CXX_NEW(TN_OP_PAIR, PRDB_pool);
        tn_op->first = OP_result(op, i);
        tn_op->second = op;
        p_new->Add_Related_TN (tn_op);
        children->push_back(p_new);
        op_node_set->push_back(p_new);
      }
      if(children->size() == 1) {
        children->push_back(_dummy_node);
        Add_Partition(qp_parent, children);
        Add_Relation(*children->begin(), qp_parent);
      }
    }
    op_node_set->push_back(qp_parent);
    OP_MAP_Set(_tn_node_map, op, op_node_set);
    
}

void 
PARTITION_GRAPH::Move_To(OP* op, BB* tgt_BB) 
{
    BB* op_bb = OP_bb(op);
    PARTITION_GRAPH_NODE* src_node = 
        (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, op_bb);
    PARTITION_GRAPH_NODE* tgt_node = 
        (PARTITION_GRAPH_NODE*)BB_MAP_Get(_bb_node_map, tgt_BB);
    if(src_node == tgt_node) return;
    Copy_To(op, tgt_BB);
    Delete(op);
}

void 
PARTITION_GRAPH::Delete(OP* op) 
{
    TOP opcode = OP_code(op);
    INT index;
    PG_CONTAINER* node_set = (PG_CONTAINER*)OP_MAP_Get(_tn_node_map, op);
    if(Compare_Type(opcode) != -1)
    {
        PARTITION_GRAPH_NODE* p1_node = (*node_set)[0];
        PARTITION_GRAPH_NODE* p2_node = (*node_set)[1];
        TN* tn1 = OP_result(op, 0);
        TN* tn2 = OP_result(op, 1);
        p1_node->Del_Related_TN (tn1, op);
        if(BB_SET_EmptyP(p1_node->Get_BBs ())
            && p1_node->Get_TNs ().empty())
        {
            index = p1_node->Index ();
            Del_Relation(index);
            CXX_DELETE(p1_node, PRDB_pool);
        }
        p2_node->Del_Related_TN (tn2, op);
        if(BB_SET_EmptyP(p2_node->Get_BBs ())
            && p2_node->Get_TNs ().empty())
        {
            index = p2_node->Index ();
            Del_Relation(index);
            CXX_DELETE(p2_node, PRDB_pool);
        }
    }
    PARTITION_GRAPH_NODE* qp_node = (*node_set)[OP_results(op)];
    TN* qp = OP_opnd(op, OP_PREDICATE_OPND);
    qp_node->Del_Related_TN (qp, op);
}

void 
PARTITION_GRAPH::Sum(TN_OP_PAIR* tp,TP_CONTAINER* tp_set)
{
    TN* tn = tp->first;
    const OP* op = tp->second;
    PARTITION_GRAPH_NODE* node = Find_Node_In_OP(tp);
    TP_CONTAINER::iterator iter;
    for ( iter = tp_set->begin(); iter != tp_set->end(); iter++)
    {
        PARTITION_GRAPH_NODE* temp_nd = Find_Node_In_OP((*iter));
        if(_subset_relation[node->Index()][temp_nd->Index()]) return;
        if(_subset_relation[temp_nd->Index()][node->Index()])
        {
            iter = tp_set->erase(iter);
            iter = tp_set->insert(iter, tp);
            return;
        }
        if(Get_Lca(node, temp_nd)
            && Is_Complementary(node, temp_nd, Get_Lca(node, temp_nd)))
        {
            TN_OP_PAIR* ret_tp = CXX_NEW(TN_OP_PAIR, PRDB_pool);
            if(Get_Lca(node, temp_nd)->Get_TNs().empty())
                ret_tp->first = True_TN;
            else ret_tp = (Get_Lca(node, temp_nd)->Get_TNs())[0];
            iter = tp_set->erase(iter);
            iter = tp_set->insert(iter, ret_tp);
            return;
        }
    }
    tp_set->push_back(tp);
}

void 
PARTITION_GRAPH::Diff(TN_OP_PAIR tp,TP_CONTAINER* tp_set )
{
    TN* tn = tp.first;
    PARTITION_GRAPH_NODE* node = Find_Node_In_OP(&tp);
    TP_CONTAINER::iterator iter;
    for ( iter = tp_set->begin(); iter != tp_set->end(); iter++)
    {
        if(tn == (*iter)->first)
        {
            iter = tp_set->erase(iter);
            break;
        }
    }
    for ( iter = tp_set->begin(); iter != tp_set->end(); iter++)
    {
        PARTITION_GRAPH_NODE* temp_nd =Find_Node_In_OP((*iter));
        if(_disjoint_relation[temp_nd->Index()][node->Index()]) continue;
        if(_subset_relation[node->Index()][temp_nd->Index()])
        {
            iter = tp_set->erase(iter);
            PG_CONTAINER* result;
            result->clear();
            result->push_back(temp_nd);
            if(Subtract(result, node))
            {
                PG_CONTAINER::iterator pg_iter;
                for(pg_iter=result->begin();pg_iter!=result->end();pg_iter++)
                {
                    TN_OP_PAIR* ret_tp = CXX_NEW(TN_OP_PAIR, PRDB_pool);
                    if((*pg_iter)->Get_TNs().empty())
                        ret_tp->first = True_TN;
                    else ret_tp = (*pg_iter)->Get_TNs()[0];
                    iter = tp_set->insert(iter, ret_tp);
                }
            }
            continue;
        }
        if(_subset_relation[temp_nd->Index()][node->Index()])
        {
            iter = tp_set->erase(iter);
            continue;
        }
        if(Get_Gcd(node, temp_nd))
        {
            iter = tp_set->erase(iter);
            PG_CONTAINER* result;
            result->clear();
            result->push_back(temp_nd);
            if(Subtract(result, Get_Gcd(node, temp_nd)))
            {
                PG_CONTAINER::iterator pg_iter;
                for(pg_iter=result->begin();pg_iter!=result->end();pg_iter++)
                {
                    TN_OP_PAIR* ret_tp = CXX_NEW(TN_OP_PAIR, PRDB_pool);
                    if((*pg_iter)->Get_TNs().empty())
                        ret_tp->first = True_TN;
                    else ret_tp = (*pg_iter)->Get_TNs()[0];
                    iter = tp_set->insert(iter, ret_tp);
                }
            }
            continue;
        }
    }
}

void 
PARTITION_GRAPH::Lub_Diff(TN_OP_PAIR tp,TP_CONTAINER* tp_set )
{
    Is_True(tp_set&&tp.first,("Illegal input with NULL value for Lub_Diff!"));
    Diff(tp, tp_set);
    Reduce(tp_set, TRUE);
}

void 
PARTITION_GRAPH::Glb_Diff(TN_OP_PAIR tp,TP_CONTAINER* tp_set )
{
    Is_True(tp_set&&tp.first,("Illegal input with NULL value for Glb_Diff!"));
    Diff(tp, tp_set);
    Reduce(tp_set, FALSE);
}

void PARTITION_GRAPH::Lub_Sum(TN_OP_PAIR* tp, TP_CONTAINER* tp_set)
{
    Is_True(tp_set&&tp->first,("Illegal input with NULL value for Lub_Sum!"));
    PARTITION_GRAPH_NODE* node = Find_Node_In_OP(tp);
    if(!node)
    {
        tp_set->clear();
        TN_OP_PAIR* ret_tp = CXX_NEW(TN_OP_PAIR, PRDB_pool);
        ret_tp->first = True_TN;
        tp_set->push_back(ret_tp);
        return;
    }
    Sum(tp, tp_set);
    Reduce(tp_set, TRUE);
}

void PARTITION_GRAPH::Glb_Sum(TN_OP_PAIR* tp, TP_CONTAINER* tp_set)
{
    Is_True(tp_set&&tp->first,("Illegal input with NULL value for Glb_Sum!"));
    PARTITION_GRAPH_NODE* node = Find_Node_In_OP(tp);
    if(!node)
    {
        return;
    }
    Sum(tp, tp_set);
    Reduce(tp_set, FALSE);
}

//simplify a pred set to ensure an element having no child. Only one dummy
//at most is retained if is_dum is true, and no dummy otherwise
void PARTITION_GRAPH::Reduce(TP_CONTAINER* tp_set, BOOL is_dum)
{
    Is_True(!tp_set->empty(), ("Empty tp_set can't be reduced!"));
    TP_CONTAINER::iterator iter1, iter2;
    BOOL has_dum = FALSE;
    PARTITION_GRAPH_NODE *node1, *node2;
    for(iter1 = tp_set->begin(); iter1 != tp_set->end(); )
    {
        node1 = Find_Node_In_OP((*iter1));
        Is_True(node1, ("No ptn_node for tp1!"));
        if(node1 == _dummy_node)
        {
            if(is_dum && !has_dum) has_dum = TRUE;
            else  iter1 = tp_set->erase(iter1);
            continue;
        }
        for(iter2 = iter1++; iter2 != tp_set->end(); iter2++)
        {
            node2 = Find_Node_In_OP((*iter2));
            Is_True(node2, ("No ptn_node for tp2!"));
            if(node2 == _dummy_node)
            {
                if(is_dum && !has_dum)  has_dum = TRUE;
                else iter2 = tp_set->erase(iter2);
                continue;
            }
            if(_subset_relation[node1->Index()][node2->Index()])
            {
                iter1 = tp_set->erase(iter1);
            }else if(_subset_relation[node2->Index()][node1->Index()])
            {
               iter2 = tp_set->erase(iter2);
            }else if(Get_Lca(node1, node2) &&
                    Is_Complementary(node1, node2, Get_Lca(node1, node2)))
            {
                TN_OP_PAIR* ret_tp = CXX_NEW(TN_OP_PAIR, PRDB_pool);
                if(Get_Lca(node1,node2)->Get_TNs().empty())
                    ret_tp->first = True_TN;
                else ret_tp = Get_Lca(node1,node2)->Get_TNs()[0];
                iter1 = tp_set->erase(iter1);
                iter2 = tp_set->erase(iter2);
                iter2 = tp_set->insert(iter2, ret_tp);
            }
        }
    }
}

//simplify a partition_graph_node set to ensure no duplicate node. Only one dummy
//at most is retained if is_dum is true, and no dummy otherwise
void PARTITION_GRAPH::Reduce(PG_CONTAINER* pg_set, BOOL is_dum)
{
    Is_True(!pg_set->empty(), ("Empty pg_set can't be reduced!"));
    PG_CONTAINER::iterator iter1, iter2;
    BOOL has_dum = FALSE;
    PARTITION_GRAPH_NODE *node1, *node2;
    for(iter1 = pg_set->begin(); iter1 != pg_set->end(); )
    {
        if(*iter1 == _dummy_node)
        {
            if(is_dum && !has_dum) has_dum = TRUE;
            else  iter1 = pg_set->erase(iter1);
            continue;
        }
        for(iter2 = iter1++; iter2 != pg_set->end(); )
        {
            if(node2 == _dummy_node)
            {
                if(is_dum && !has_dum)  has_dum = TRUE;
                else iter2 = pg_set->erase(iter2);
                continue;
            }
            if(*iter1 == *iter2) {
                iter2 = pg_set->erase(iter2);
                continue;
            }
            iter2++;
        }
    }
}

//*********************************************************************
//Part 4: Class PRDB_GEN
//*********************************************************************
PRDB_GEN::PRDB_GEN(REGION_TREE* region_tree)
{
    const char* prev_phase = Get_Error_Phase();
    Set_Error_Phase("Ipfec predicate relation database");
    Start_Timer(T_Ipfec_PRDB_CU);

    if (Get_Trace(TKIND_IR, TP_A_PRDB, REGION_First_BB))
      Trace_IR(TP_A_PRDB, "PRDB", NULL, FALSE);

    PRDB_pool = &_m;

    if(VT_Enable_Global_CFG) draw_global_cfg("right before region Edge_Split");
    for (INNERMOST_REGION_FIRST_ITER iter(region_tree);
         iter != 0; 
         ++iter)
    {
        REGION* region = *iter;
        if(region->Region_Type() == IMPROPER ||
            region->Is_No_Further_Opt())
            continue;
        region -> Edge_Splitting();
    }

    if(VT_Enable_Global_CFG) draw_global_cfg("Right after region Edge_splitt");
    Calculate_Dominators();
    
    _region_graph = REGION_MAP_Create(region_tree->Seq_Num());

     for (INNERMOST_REGION_FIRST_ITER iter(region_tree);
         iter != 0; 
         ++iter)
    {
        REGION* region = *iter;
        if(region->Region_Type() == IMPROPER ||
            region->Is_No_Further_Opt())
            continue;
        if(Is_No_BB_Region(region)){
            DevWarn("A region doesn't have a bb node!");
            continue;
        }

        if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
            fprintf(TFile, " ===== region %d.\n", region -> Id());

        REGIONAL_CFG_NODE *node = region->Entries()[0];

        if(VT_Enable_Regional_CFG) draw_regional_cfg(region);
        PARTITION_GRAPH* graph = CXX_NEW(PARTITION_GRAPH(region), PRDB_pool);
        REGION_MAP_Set(_region_graph, region,graph);

        if(VT_Enable_Partition_Graph)
            draw_partition_graph(graph);
    }
    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        Print(TFile, region_tree);
    
    Free_Dominators_Memory();

    if (Get_Trace(TKIND_IR, TP_A_PRDB, REGION_First_BB))
      Trace_IR(TP_A_PRDB, "PRDB", NULL);

    Stop_Timer(T_Ipfec_PRDB_CU);

    Set_Error_Phase(prev_phase);
}

PRDB_GEN::PRDB_GEN(REGION* region)
{
    const char* prev_phase = Get_Error_Phase();
    Set_Error_Phase("Ipfec predicate relation database");
    Start_Timer(T_Ipfec_PRDB_CU);
    Is_True(!(region->Region_Type() == IMPROPER ||
            region->Is_No_Further_Opt()), ("unexpected region!"));

    if (Get_Trace(TKIND_IR, TP_A_PRDB, REGION_First_BB))
      Trace_IR(TP_A_PRDB, "PRDB", NULL, FALSE);

    PRDB_pool = &_m;

    region->Edge_Splitting();
    Calculate_Dominators();

    _region_graph = REGION_MAP_Create(1);
    if (Get_Trace(TP_A_PRDB, TT_PRDB_VERBOSE))
        fprintf(TFile, " ===== region %d.\n", region -> Id());
    Is_True(!Is_No_BB_Region(region), ("A region doesn't have a bb node!"));

    if(VT_Enable_Regional_CFG) draw_regional_cfg(region);
    PARTITION_GRAPH* graph = CXX_NEW(PARTITION_GRAPH(region), PRDB_pool);
    REGION_MAP_Set(_region_graph, region,graph);

    if(VT_Enable_Partition_Graph)
        draw_partition_graph(graph);
    Free_Dominators_Memory();

    if (Get_Trace(TKIND_IR, TP_A_PRDB, REGION_First_BB))
      Trace_IR(TP_A_PRDB, "PRDB", NULL);

    Stop_Timer(T_Ipfec_PRDB_CU);

    Set_Error_Phase(prev_phase);
}

PARTITION_GRAPH* 
PRDB_GEN::Partition_Graph(REGION* region)
{
    Is_True(!Is_No_BB_Region(region), ("A region doesn't have a bb node!"));
    Is_True(!(region->Region_Type() == IMPROPER ||
            region->Is_No_Further_Opt()), ("unexpected region!"));
    return((PARTITION_GRAPH*)REGION_MAP_Get(_region_graph, region));
}

inline PRDB_GEN* Generate_PRDB(REGION_TREE* region_tree)
{
    return CXX_NEW(PRDB_GEN(region_tree), &MEM_pu_pool);
}

inline PRDB_GEN* Generate_PRDB(REGION* region)
{
    return CXX_NEW(PRDB_GEN(region), &MEM_pu_pool);
}

void Delete_PRDB()
{
    if(prdb){
        CXX_DELETE(prdb, &MEM_pu_pool);
        prdb = NULL;
    }
}

PRDB_GEN* PRDB_Init(REGION_TREE* rgn_tree)
{
    Delete_PRDB();
    return prdb = Generate_PRDB(rgn_tree);    
}

PRDB_GEN* PRDB_Init(REGION* region)
{
    Delete_PRDB();
    return prdb = Generate_PRDB(region);    
}

PRDB_GEN* Get_PRDB()
{
    return prdb;
    
}

BOOL PRDB_Valid()
{
    return prdb != NULL;
}

void 
PARTITION::Print(FILE* file)
{
    fprintf(file, "  === partition ===\n");
    fprintf(file, "   = its parent is : %d \n", _parent -> Index());
    fprintf(file, "   = its children are :");
    PG_CONTAINER::iterator iter;
    for (iter = _child.begin(); iter != _child.end(); iter++)
    {
        fprintf(file," %d ,", (*iter) -> Index());
    }
    fprintf(file, "\n");
}
void
PARTITION_GRAPH_NODE::Print(FILE* file)
{
    fprintf(file, " PARTITION GRAPH NODE : %d \n", _index);
    fprintf(file, "  = its level is %d \n", _level);
    
    fprintf(file, "  = its related tns :");
    TP_CONTAINER::iterator iter;
    for (iter = _related_tns.begin(); 
    iter != _related_tns.end(); 
    iter++)
    {
        fPrint_TN(file, "  %s,", (*iter)->first);
    }
    fprintf(file, "\n");

    fprintf(file,"  = its related bbs:");
    BB* bb;
    FOR_ALL_BB_SET_members( _related_bbs, bb){
        fprintf(file,"  %d,", BB_id(bb));
    }
    fprintf(file, "\n");

    fprintf(file,"  = its predecessor :");
    PT_CONTAINER:: iterator iter1;
    for (iter1 = _child_partitions.begin(); 
    iter1 != _child_partitions.end(); 
    iter1++)
    {
        fprintf(file, "  %d,",(*iter1) -> Parent() -> Index());
    }
    fprintf(file, "\n");

    fprintf(file,"  = its successor :");
    for (iter1 = _parent_partitions.begin(); 
    iter1 != _parent_partitions.end(); 
    iter1++)
    {
        PARTITION  *par = *iter1;
        PG_CONTAINER::iterator iter2;
        for (iter2 = par -> Child().begin(); 
        iter2 != par -> Child().end();
        iter2++)
        {
            fprintf(file, "  %d,",(*iter2) -> Index());
        }
    }
    fprintf(file, "\n");
    
}


void
PARTITION_GRAPH::Print(FILE* file)
{
    fprintf(file, "======== included nodes of the graph\n");
    PG_CONTAINER::iterator iter;
    for ( iter = _nodes.begin(); iter != _nodes.end(); iter++){
        (*iter)-> Print(file);
    }

    fprintf(file, "======== the root node is %d\n", _root -> Index());
    fprintf(file, "======== the dummy node is %d\n", _dummy_node -> Index());

}

void     
PRDB_GEN::Print(FILE* file, REGION_TREE* region_tree)
{
    for (INNERMOST_REGION_FIRST_ITER iter(region_tree);
    iter != 0; 
    ++iter)
    {
        REGION* region = *iter;
        if(region->Region_Type() == IMPROPER ||
            region->Is_No_Further_Opt())
        {
            continue;
        }
        if(Is_No_BB_Region(region)){
            DevWarn("A region doesn't have a bb node!");
            continue;
        }
        
        PARTITION_GRAPH* graph =(PARTITION_GRAPH*)
            REGION_MAP_Get(_region_graph, region);
        graph -> Print(file);

    }
}


