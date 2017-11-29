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

//*****************************************************************************
//
// Module: if_conv.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/if_conv.cxx,v $
//
//
//
// Description: Implementation of Ipfec if-conversion.
// See if-conv.h for the description.
//
//*****************************************************************************
#include <vector>
#include <set>
#include <stdlib.h>
#include <stdio.h>
#include "bb.h"
#include "defs.h"
#include "mempool.h"
#include "error.h"
#include "bb_set.h"
#include "region.h"
#include "cgtarget.h"
#include "if_conv.h"
#include "timing.h"
#include "tracing.h"
#include "cg.h"
#include "profile_util.h"
#include "region_bb_util.h"
#include "ti_res_count.h"
#include "ti_latency.h"
#include "ipfec_defs.h"
#include "cg_dep_graph.h"
#include "dominate.h"
#include "vt_region.h"
#include "recovery.h"
#include <math.h>
#include "whirl2ops.h"
#include "tlog.h"
#include "glob.h"
#include "ipfec_options.h"
#include "be_util.h"
#include "freq.h"
#include "op.h"

#define MAX_NUM_PARA_CMP   2
#define COMP_TOP_NUM       32
#define PARA_COMP_TYPE_NUM 6

void  fPrint_TN ( FILE *f, char *fmt, TN *tn);
BOOL  CGTARG_Unconditional_Compare(OP *, TOP *);
void  BB_SET_Calculate_Dominators(BB_SET *, BOOL, BOOL);
void  Calculate_Dominators(void);
void  Free_Dominators_Memory(void);
void  Exp_True_False_Preds_For_Block(BB *, TN *&, TN *&);
void  Exp_Pred_Set(TN *dest, TN *cdest, INT val, OPS *ops);
void  Exp_Generic_Pred_Calc(TN*,TN *, COMPARE_TYPE, TN *, OPS*);
void  draw_classic_BB_dependence_graph(BB *bb);
void  Predicate_Block(BB* bb, TN *pred_tn, BB_SET*);
void  Print_BB (BB *);
void  Print_OPS(ops const *);
void  Print_OP_No_SrcLine(const OP *op);
void  GRA_LIVE_Compute_Liveness_For_BB(BB *bb);
BOOL  FREQ_Match(float f1, float f2);
BOOL  Is_In_Abnormal_Loop(REGION* r);
COMPARE_TYPE Compare_Type(TOP opcode);

hTN_MAPf frequency_of_predicates = 0;
TN_INFO_MEM info_mem;
hTN_MAP   init_op_info = 0;


BOOL 
Is_Para_Comp_May_Def(OP *op) 
{
    COMPARE_TYPE comp_type = Compare_Type(OP_code(op));
    return  ((comp_type != COMPARE_TYPE_unc)
                   && (comp_type != COMPARE_TYPE_normal)
                   && (comp_type != (COMPARE_TYPE)-1));
}

BOOL
Is_In_Infinite_Loop(REGION* region) 
{
    REGION* reg = region;
    while (reg) {
        if ( reg -> Region_Type() == LOOP 
            && reg ->Exits().empty())
            return TRUE;
        reg = reg -> Parent();
    }
    return FALSE;
}

//*****************************************************************************
// Function: Is_Abnormal_Loop
// Input : region
// Output : 
//     a boolean value which indicates if the region is an abnormal loop.
//     Here, an abnormal loop is a loop like :
//                        1 <------|
//                      /  \       |
//                     /    \      |
//                    2      3     |
//                   / \    /      |
//                  /   \  /       |
//               exit    4 --------|
//     in such a loop, 1 dom 2 and 2 pdom 1, as a result, 1 and 2 are put into
//     one control-equivalent-class. So some wrong results are caused in 
//     if-conversion and predicate analysis.
//  Description :
//     the conditions in which we judge if a region is an abnormal loop is :
//     1)  region is a loop region;
//     2)  the source of the back-edge is not an exit -node of the loop;
//     3)  the exit is not the entry of the loop
//*****************************************************************************
BOOL 
Is_Abnormal_Loop(REGION* region) 
{
    if ( region -> Region_Type() != LOOP) 
        return FALSE;

    // find out the source node of back-edge
    REGIONAL_CFG_NODE* loop_tail = NULL;
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter( region -> Regional_Cfg()); 
    iter!=0; 
    ++iter)
    {
        REGIONAL_CFG_NODE *node = *iter;
        if ( node -> Succ_Num() == 0) 
        {
            if(loop_tail || 
              (node -> Is_Region() && node->Region_Node()->Exits().size() >1 ) )
                return TRUE;
            loop_tail = node;
        }

    }

    NODE_VECTOR exits = region -> Exits();
    for (NODE_VECTOR_ITER iter1 = exits.begin();
    iter1 != exits.end();
    iter1++) 
    {
        if ((*iter1) == loop_tail) {
            return FALSE;
        }
    }
    

    if ( region -> Entries().size() != 1) {
        DevWarn("MEME region is illegal in Is_Abnormal_Loop!\n");
        return FALSE;
    }
    if ( region -> Exits().size() == 1) 
    {
        REGIONAL_CFG_NODE* entry = *(region -> Entries().begin());
        REGIONAL_CFG_NODE* exit = *(region -> Exits().begin());
        if (!entry->Is_Region() && entry == exit) 
            return FALSE;
    }

    return TRUE;
}


/* ====================================================================
 *
 *  TN_Defined_At_Op
 *
 *  This functions lookes for the definition of a given TN for
 *  a given OP. 
 *  
 *  If the definition is conditional and garded by the same
 *  predicate as the given OP, then the pointer to this OP is returned.
 *  If this is not the case, then the pointer to this function is put
 *  into the OP list(ops) and it continues the search until it finds a
 *  definition which is not conditional or the predicates are the
 *  same.
 *  i.e The given values are tn=TN1, op=OP3, ops=<some valid pointer>
 *  TN1 TN2 OP1
 *  TN1 TN2 OP2 (TN3) cond.
 *          OP3 (TN1) cond.
 *  
 *  The function would in this case return the pointer to OP1, because
 *  this is the first OP which definitely defines TN1. In the OP list
 *  would be the pointers to OP2 and OP1.
 *
 * ====================================================================
 */
OP *
TN_Defined_At_Op (TN *tn, OP *op, std::vector<OP *> *ops) {
  OP  *value_op;

  if (ops==NULL) {
    FmtAssert(0, ("Parameter ops is NULL pointer!"));
  }

  if (TN_register(tn) != REGISTER_UNDEFINED) {
    return NULL;
  }
  
  for (value_op=OP_prev(op); value_op!=NULL; value_op=OP_prev(value_op)) {
    if (OP_Defs_TN(value_op, tn)) {
      ops->push_back(value_op);
      if (Is_OP_Cond(value_op)) {
        if (OP_has_predicate(value_op) && OP_has_predicate(op)) {
          TN *p1 = OP_opnd((OP*) value_op, OP_PREDICATE_OPND);
          TN *p2 = OP_opnd((OP*) op, OP_PREDICATE_OPND);
              
          if (p1 == p2) {
            return value_op;
          }
        }
      } 
      else {
        return value_op;
      }
    }
  } 

  return NULL;
}


//*****************************************************************************
// Function : Find_BB_Predicates
// Input : 
//   - bb : the bb contains branch and compare
//   - first_pred: the first target of compare
//   - second_pred: the second target of compare
// Output :
//   < none >
// Description :
//   if a bb has a conditional branch, and the guarding predicate of the 
//   branch is defined by a compare instructin , the function is to find out 
//   the first and the second predicate register of the compare instruction.
//*****************************************************************************
void
Find_BB_Predicates(BB* bb, TN*& first_pred,TN*& second_pred)
{
    vector<OP *>::iterator op; 
    vector<OP *> ops;
    OP  *br = BB_branch_op(bb);

    first_pred  = NULL;
    second_pred = NULL;

    TN *pred_1;
    TN *pred_2;
    
    if (BB_succs_len(bb) != 2 || !br) {
      return;
    }
    
    pred_1 = OP_opnd(br, OP_PREDICATE_OPND);
    Is_True(pred_1, ("conditional branch has no guarded predicate!\n"));

    OP *compare_op = TN_Defined_At_Op(pred_1, br, &ops);
    
    if(!compare_op) {
      return;
    }
    
    Is_True(compare_op, 
        (" the predicate of br has reaching definition!\n"));
    Is_True(OP_results(compare_op), 
        (" compare_op must has result.\n"));
    
    first_pred = OP_result(compare_op,0);
    if (OP_results(compare_op) > 1)
    {
        second_pred = OP_result(compare_op,1);
    }
    
    // If we have more then one OP which defines the branch predicate,
    // we have to check if all predicate pairs are the same.
    // i.e
    // TN1 TN2 op1
    // TN2 TN1 op2

    BOOL create_neg_of_br_pred = FALSE;
    
    if (ops.size() < 2) {
      return;
    }
    
    for (op = ops.begin(); op != ops.end(); op++) {
      if (OP_results(*op) > 1) { 
        if (  !( (first_pred  == OP_result(*op,0)) && 
                 (second_pred == OP_result(*op,1)) 
              ) && 
              !( (first_pred  == OP_result(*op,1)) && 
                 (second_pred == OP_result(*op,0)) 
              ) 
           )
        {
          // predicate pair is different
          // we have to create and insert a predicate which is a 
          // negation of our branch predicate
          create_neg_of_br_pred = TRUE;
          break;
        }
      }
      else {
        // only one predicate
        // we have to create and insert a predicate which is a 
        // negation of our branch predicate
        create_neg_of_br_pred = TRUE;
        break;
      }
    }

    if (create_neg_of_br_pred) {
      OP *neg_op;

      // Lets check if we already have insert the negation op in a previous
      // function call of Find_BB_Predictae()
      neg_op = OP_prev(br);

      if ( (OP_code(neg_op)==TOP_cmp_ne_unc) &&
           (OP_Refs_TN(neg_op, pred_1))      &&
           (OP_Refs_TN(neg_op, Zero_TN))     &&
           (OP_Defs_TN(neg_op, True_TN)) ) 
      {
        pred_2 = OP_result(neg_op, 1);  
      }
      else {
        pred_2 = Gen_Predicate_TN(); 
        neg_op = Mk_OP(TOP_cmp_ne_unc, True_TN, pred_2, True_TN, pred_1, Zero_TN);
        OP_srcpos(neg_op) = OP_srcpos(br);
        BB_Insert_Op(bb, br, neg_op, TRUE);
      }
      
      first_pred  = pred_1;
      second_pred = pred_2;
    }
    
    return;
}


//=============================================================================
//    Part 1: implementation of the classes defined in this phase
//=============================================================================

//*****************************************************************************
// implementation for class IF_CONV_AREA
//*****************************************************************************

IF_CONV_AREA::IF_CONV_AREA(BB *bb, IF_CONVERTOR *convertor):
_included_blocks(BB_CONTAINER(&(convertor -> _m))),
_predecessors(IF_CONV_ALLOC(&(convertor -> _m))),
_successors(IF_CONV_ALLOC(&(convertor -> _m)))
{    
    _head = bb;
    
    // add the present bb in _included_blocks
    _included_blocks.push_back(bb);

    // if the present basic block is not suitable for if-conversion, 
    // we set the mark
    // if it is suitable, we compute the length of its critical path

    AREA_TYPE type = convertor -> Suitable_For_If_Conv(bb);
    _if_suitable = type;
    _need_if_convert = NO_IF_CONV;
    if (type != UNSUITABLE) 
    {
        _cycled_critical_length = convertor -> 
            Compute_Critical_Path_Len(bb, true);
        _critical_length = convertor -> 
            Compute_Critical_Path_Len(bb,false);
    } else {
        _cycled_critical_length = 0;
        _critical_length = 0;
    }

    _control_deps = 0; 
    _pred_assign_info = 0;
}

void     
IF_CONV_AREA::Combine_Blocks_With(IF_CONV_AREA *area, MEM_POOL *mem_pool)
{
    BB_CONTAINER& set = area -> Blocks();
    BB_CONTAINER::iterator iter;
    for (iter = set.begin(); 
    iter != set.end(); 
    iter++) 
    {
        _included_blocks.push_back(*iter);
    }
}

void     
IF_CONV_AREA::Init_Conversion_Info(MEM_POOL *mem_pool) 
{
    _control_deps = CXX_NEW(
        CNTL_DEP(_head, _included_blocks, mem_pool), mem_pool);
    _pred_assign_info = BB_MAP_Create();

    BB_CONTAINER::iterator iter;
    for (iter = _included_blocks.begin(); 
    iter != _included_blocks.end(); 
    iter++) 
    {
        BB_PREDICATE_INFO* info = 
            CXX_NEW(BB_PREDICATE_INFO(mem_pool), mem_pool);
        BB_MAP_Set(_pred_assign_info, *iter, info);
    }
}
void              
IF_CONV_AREA::Remove_BB(BB* bb)
{
    BB_CONTAINER::iterator iter;
    for (iter = _included_blocks.begin(); 
    iter != _included_blocks.end(); 
    iter++) 
    {
        if ( *iter == bb) 
        {
            _included_blocks.erase(iter);
            return;
        }
    }
}
EXIT_TARGET_INFO* 
IF_CONV_AREA::Exit_Target(BB* bb) 
{
    EXIT_CONTAINER::iterator iter;
    for (iter = _exit_targets.begin();
    iter != _exit_targets.end();
    iter++)
    {
        if ( (*iter) -> Target() == bb) {
            return *iter;
        }
    }
    return NULL;
}
void              
IF_CONV_AREA::Add_Exit_Target(BB* target, TN* predicate, MEM_POOL* mem_pool)
{
    EXIT_TARGET_INFO* info = 
        CXX_NEW(EXIT_TARGET_INFO(target, mem_pool), mem_pool);
    info -> Add_Main_Predicate(predicate);
    _exit_targets.push_back(info);
}

BOOL    
EXIT_TARGET_INFO::Is_Main_Predicate(TN* tn)
{
    TN_CONTAINER::iterator iter;
    for (iter = _main_predicates.begin();
    iter != _main_predicates.end();
    iter++)
    {
        if ( *iter == tn) return true;
    }
    return false;
}
void    
EXIT_TARGET_INFO::Assign_Aux_Predicates(BB* bb, OP *br)
{
    OPS ops = OPS_EMPTY;
 
    if ( _aux_predicates.size() == 0 || _main_predicates.size() ==0 ) 
        return;

    Is_True(_main_predicates.size() == 1, 
        ("Wrong number of main predicates!\n"));

    TN *main_tn = *(_main_predicates.begin());
    TN_CONTAINER::iterator iter;
    for (iter = _aux_predicates.begin();
    iter != _aux_predicates.end();
    iter++)
    {
        Is_True(*iter, ("NULL predicate.\n"));
        Build_OP(TOP_cmp_eq, main_tn, True_TN,
             *iter,Zero_TN, Zero_TN, &ops);
    }
        
    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
    {
        Print_OPS(&ops);
    }
    OP* new_op;
    FOR_ALL_OPS_OPs_FWD((&ops),new_op){
        Set_OP_cond_def_kind(new_op, OP_ALWAYS_COND_DEF); 
    }
    BB_Insert_Ops_Before (bb, br, &ops);

}
void    
EXIT_TARGET_INFO::Del_Main_Predicate(TN* tn){
    TN_CONTAINER::iterator iter;
    for (iter = _main_predicates.begin();
    iter != _main_predicates.end();
    iter++)
    {    
        Is_True(*iter, ("NULL predicate.\n"));
        if (*iter == tn) 
        {
            _main_predicates.erase(iter);
            break;
        }
    }
}
void    
EXIT_TARGET_INFO::Update_Predicate(TN *old_tn, TN* new_tn)
{
    if (old_tn == new_tn) return;

    TN_CONTAINER::iterator iter;
    for (iter = _main_predicates.begin();
    iter != _main_predicates.end();
    iter++)
    {    
        Is_True(*iter, ("NULL predicate.\n"));
        if (*iter == old_tn) 
        {
            _main_predicates.erase(iter);
            _main_predicates.push_back(new_tn);
            break;
        }
    }
    
    for (iter = _aux_predicates.begin();
    iter != _aux_predicates.end();
    iter++)
    {    
        Is_True(*iter, ("NULL predicate.\n"));
        if (*iter == old_tn) 
        {
            _aux_predicates.erase(iter);
            _aux_predicates.push_back(new_tn);
            break;
        }
    }

    return;

}
//*****************************************************************************
// implementation for class CNTL_DEP
//*****************************************************************************

//*****************************************************************************
// Function : CNTL::CNTL_DEP
// Input : 
//     - entry : the entry basic block of all BBs in set
//     - set : a set of basic blocks. To use CNTL_DEP, the set must be:
//           1) all BBs in it are connected;
//           2) all BBs in it have a unique entry 
//           3) there is no multiple-successor-bb in it
//           4) there is no such a case: a region is a predecessor of one bb 
//              in the set and, in the same time, it is a successor of another 
//              bb in the set
// Output : 
//     the control dependent tree of the input bb_set
// Description :
//     it computes the control dependent information of a bb_set
//
//*****************************************************************************
CNTL_DEP::CNTL_DEP(BB *entry, BB_CONTAINER& bbs, MEM_POOL *mem_pool)
{
    _entry = entry;
 
    _bb_set = BB_SET_Create_Empty(PU_BB_Count, mem_pool);
    BB_CONTAINER::iterator bb_iter;
    BB *bb;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        _bb_set = BB_SET_Union1D(_bb_set, bb, mem_pool);
    }
  
    // Initialize control dependencies bit sets
    _control_dependences = BB_MAP_Create();
    _true_edges = BB_MAP_Create();

    FOR_ALL_BB_SET_members(_bb_set, bb) 
    {
        BB_SET *deps = BB_SET_Create_Empty(PU_BB_Count, mem_pool);
        BB_SET *trues = BB_SET_Create_Empty(PU_BB_Count, mem_pool);
        BB_MAP_Set(_control_dependences, bb, deps);
        BB_MAP_Set(_true_edges, bb, trues);
    }
  
    // Find out the control dependences. Also, in this algorithms it's
    // easy to set the true edges at the same time.
    BBLIST *bl;
    BB_SET *bb_to_add = BB_SET_Create_Empty(PU_BB_Count+2, mem_pool);
    FOR_ALL_BB_SET_members(_bb_set, bb) 
    {
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
        {
            fprintf(TFile, " Computing BB%d:\n", BB_id(bb));
        }
        BB *fall_thru = BB_Fall_Thru_Successor(bb);
        TN *second_pred = NULL;
        TN *first_pred = NULL;
        OP *br = NULL;
        if ( BB_succs_len(bb) == 2) 
        {
            br = BB_branch_op(bb);
            Find_BB_Predicates(bb, first_pred, second_pred);
        }

        FOR_ALL_BB_SUCCS(bb, bl) 
        {
            BB *bb_dep;
            BB *bb_succ = BBLIST_item(bl);
            BOOL true_edge = FALSE;

            if ( br )
            {
                 true_edge = ((  first_pred == OP_opnd(br, OP_PREDICATE_OPND)
                             && bb_succ != fall_thru) 
                         || (   second_pred == OP_opnd(br, OP_PREDICATE_OPND) 
                             && bb_succ == fall_thru));
            }
            //
            // bb_to_add is set to (pdom(bb_succ) - pdom(bb)) & _bb_set. 
            // In other words, it is used to record what is in the bb_set
            // post-dominates bb_succ (which will include bb_succ) and
            // is not a post-dominator of bb. These are exactly the BB's which
            // are control-dependent on bb. If the successor is not in the 
            // bb_set, we are safe to assume that nothing else in the bb_set 
            // post-dominates it, so we can ignore it. Also, if the successor 
            // is the bb_set entry, we have a loop and hence we must not 
            // consider this arc when computing the control dependence.
            //
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
            {
                fprintf(TFile, "   ===== succ BB%d:", BB_id(bb_succ));
            }
            if (BB_SET_MemberP(_bb_set, bb_succ) && (bb_succ != entry)) 
            {  
                BB_SET_CopyD(bb_to_add, BB_pdom_set(bb_succ), mem_pool);
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
                {
                    BB* bb_tmp;
                    fprintf(TFile, "      ** pdoms:");
                    FOR_ALL_BB_SET_members(bb_to_add, bb_tmp) 
                    {
                        fprintf(TFile, " BB%d,  ", BB_id(bb_tmp));
                    }
                    fprintf(TFile, "\n");
                }

                bb_to_add = BB_SET_DifferenceD(bb_to_add, BB_pdom_set(bb));
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
                {
                    BB* bb_tmp;
                    fprintf(TFile, "      ** after -pdoms(bb):");
                    FOR_ALL_BB_SET_members(bb_to_add, bb_tmp) 
                    {
                        fprintf(TFile, " BB%d,  ", BB_id(bb_tmp));
                    }
                    fprintf(TFile, "\n");
                }

                bb_to_add = BB_SET_IntersectionD(bb_to_add, _bb_set);
                FOR_ALL_BB_SET_members(bb_to_add, bb_dep) 
                {
                    BB_SET  *deps, *trues, *new_deps, *new_trues;

                    deps = (BB_SET*)BB_MAP_Get(_control_dependences, bb_dep);
                    Is_True(deps != NULL, 
                        ("Can't find cntl deps for BB%d.\n", BB_id(bb_dep)));

                    trues = (BB_SET*)BB_MAP_Get(_true_edges, bb_dep);
                    Is_True(trues != NULL, 
                        ("Can't find true edges for BB%d.\n", BB_id(bb_dep)));

                    new_deps = BB_SET_Union1D(deps, bb, mem_pool);
                    if ( deps != new_deps) 
                    {
                        BB_MAP_Set(_control_dependences, bb_dep, new_deps);
                    }
                    if (true_edge) 
                    {
                        new_trues = BB_SET_Union1D(trues, bb, mem_pool);
                        if ( trues != new_trues )
                        {
                            BB_MAP_Set( _true_edges, bb_dep, mem_pool);
                        }
                    }
                }
            }
        }
    }
}
BOOL              
CNTL_DEP::Is_Cntl_Dep(BB *bb1, BB *bb2)
{
    BB *bb;
    if (bb1 == bb2) return true;

    BB_SET *set = Cntl_Dep_Parents(bb2);
    Is_True(set != NULL, 
        (" Can not find control dependent parents for BB%d", BB_id(bb2)));

    FOR_ALL_BB_SET_members(set, bb)
    {
        if (Is_Cntl_Dep(bb1, bb))
            return true;
    }
    return false;
}

void                 
CNTL_DEP::Cntl_Dep_Children(BB_SET*& result, BB *bb, MEM_POOL *mem_pool)
{
    BB *block;
    FOR_ALL_BB_SET_members(_bb_set, block)
    {
        BB_SET *cds = (BB_SET*)BB_MAP_Get(_control_dependences, block);
        Is_True(cds != NULL, 
            (" Can not find control dependent parents for BB%d", 
            BB_id(block)));

        if (BB_SET_MemberP(cds, bb))
        {
            result = BB_SET_Union1D( result, block, mem_pool);
        }
    }
}

void              
CNTL_DEP::_Post_Order_Helper(BB_CONTAINER& result, 
                             BB *bb, 
                             IF_CONVERTOR* convertor)
{
    BB *child;
    BB_SET *child_set;

    if (convertor -> Is_BB_Container_Member(result, bb))    
        return;

    child_set = BB_SET_Create_Empty(PU_BB_Count, &(convertor -> _m));
    Cntl_Dep_Children(child_set, bb, &(convertor -> _m));
    FOR_ALL_BB_SET_members(child_set, child)
    {
        _Post_Order_Helper(result, child, convertor);
    }
    result.push_back(bb);
}

void              
CNTL_DEP::Get_Post_Order(BB_CONTAINER& result, 
                         IF_CONVERTOR* convertor)
{
    BB *block;
    FOR_ALL_BB_SET_members(_bb_set, block)
    {
        _Post_Order_Helper(result, block, convertor);
    }
}

//=============================================================================
//    Part2: some tool functions for IF_CONVERTOR
//=============================================================================


//*****************************************************************************
// Function : Suitable_For_If_Conv
// Input : 
//     - bb : a basic block 
// Output : 
//     it indicate if it is legal to if_convert the bb
// Description :
//     Determine if the block has some characteristics that make it
//     undesirable or even illegal to be if-converted.
//
//*****************************************************************************

AREA_TYPE
IF_CONVERTOR::Suitable_For_If_Conv(BB *bb)
{
    // except the basic block consisting of vargoto
    if (BB_kind(bb) == BBKIND_VARGOTO || BB_kind(bb) == BBKIND_INDGOTO)
    {
        return UNSUITABLE;
    }

    if (BB_call(bb)) return UNSUITABLE;

    // The bb with exception label can not be if-converted
    if (BB_Has_Exc_Label(bb)) 
    {
        return UNSUITABLE;
    }

    // Blocks which have labels marked as address-taken cannot be if-converted.
    if (BB_Has_Addr_Taken_Label(bb)) 
    {
        return UNSUITABLE;
    }

    AREA_TYPE ty = SUITABLE_FOR_IF_CONV; 
    OP* op;
    FOR_ALL_BB_OPs_FWD(bb, op) 
    {

        // if an op doesn't have qualifying predicate operands, it may be
        // unsafe to employ predication. Exclude OP_xfer OPs as they
        // will be eliminated as a by-product of doing if-conversion, so need 
		// to check for those OPs. 
        if (!OP_has_predicate(op) && !OP_xfer(op)) 
        {
            ty = (AREA_TYPE)((INT)ty & UPWARD_UNSUITABLE);
        }

        // For now, we do not plan to deal with blocks containing predicated 
        // instructions created by previous phases. 
        if (OP_has_predicate(op) &&
            !TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND))) 
        {
            TN *pred_tn = OP_opnd(op, OP_PREDICATE_OPND);
            if (TN_is_global_reg(pred_tn)) 
            {
                return UNSUITABLE;
            }
            vector<OP *> ops;
            OP *def_op = TN_Defined_At_Op(pred_tn, op, &ops);

            // If it's not defined in the block, we will give up. 
            if (!def_op) 
            {    
                DevWarn("Predicated instruction with no reaching def in BB%d "
                        "in Suitable_For_If_Conv.", BB_id(bb));
                return UNSUITABLE;
            }    

            // If its reaching defination is not in the bb, we will 
            // give up because it is very complicated.
            if (OP_bb(def_op)!= bb)
            {    
                DevWarn("An up-exposed predicate use stops if-converter BB%d "
                        "in Suitable_For_If_Conv.",BB_id(bb));
                return UNSUITABLE;
            }
        }

        // the following part is to protect the following case from being 
        // if-converted: 
        //               p1, p2 = cmp a, b
        //                 (p2)  br
        //               /          |
        //              /           |
        //    p1, p2 = cmp a, b     |
        //              \           |
        //               \          |
        //                 (p2)  br
        //                  /     \
        //                 /       \
        //  note: the 'cmp' is normal-type (non-unconditional) 
        // for the case, 
        //     before if-convertion, if the condition of compare is false, 
        // both the two branches will all be taken; after if-convertion, it is 
        // changed to :
        //               p1, p2 = cmp a, b
        //          (p1) p1, p2 = cmp.unc a, b
        //          (p2) br
        // note: the second cmp is changed to unconditional type. As a result,
        // if the condition is false, the brance will not be taken. 
        // So, here, we skip the case. 
        COMPARE_TYPE ty = Compare_Type(OP_code(op));
        if (  ty != (COMPARE_TYPE)-1) {
             // compare type
             Is_True(OP_results(op) <=2, ("too many results!\n"));
             
             TN *pred_tn = OP_result(op, 0);
             if (Is_Partial_Redundant_Def(bb, op, pred_tn)) {
                 return UNSUITABLE;
             }
             pred_tn = OP_result(op, 1);
             if (Is_Partial_Redundant_Def(bb, op, pred_tn)) {
                 return UNSUITABLE;
             }
         }
    }
    return ty;
}
BOOL
IF_CONVERTOR::Is_Partial_Redundant_Def(BB* bb, OP* op, TN* tn)
{
    // check all bb's ancestor in the region to see 
    // if tn is defined previously.
    OP  *def_op = NULL;
    OP  *def_bb = NULL;
    vector<OP *> ops;
    
    def_op = TN_Defined_At_Op(tn, op, &ops);
    if (def_op)
    {
        BB *def_bb = OP_bb(def_op);
        if ( def_bb != bb && !BB_SET_MemberP(BB_pdom_set(def_bb), bb)) 
        {
            return TRUE;
        }
    } else { 
        BB_SET* bb_queue = BB_SET_Create_Empty(PU_BB_Count, &_m);
        bb_queue = BB_SET_Union1D(bb_queue, bb, &_m);
	BB_SET* bb_processed = BB_SET_Create_Empty(PU_BB_Count, &_m);

        while ( BB_SET_Size(bb_queue) ) 
        {
            BB* bb1 = BB_SET_Choose(bb_queue);
            BS_Difference1D(bb_queue, BB_id(bb1));
	    BB_SET_Union1D(bb_processed, bb1, &_m);

            REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb1);
            for (CFG_PRED_NODE_ITER pred_iter(node);
            pred_iter!=0; 
            ++pred_iter)
            {
                REGIONAL_CFG_NODE *pred = *pred_iter;
                if (pred -> Is_Region()) continue;

                BB* pred_bb = pred -> BB_Node();    
                if ( GTN_SET_MemberP(BB_live_in(pred_bb),tn) 
                    && !BB_SET_MemberP(BB_pdom_set(pred_bb), bb))
                {
                    return TRUE;
                } else if (!BB_SET_MemberP(bb_processed, pred_bb)){
                    bb_queue = BB_SET_Union1D(bb_queue, pred_bb, &_m);
                }
            }
        }
    }
    return FALSE;
}

//*****************************************************************************
// Function : Compute_Min_Cycle
// Input : 
//     - set : a set of basic blocks
// Output :
//     a number to indicate the mininal executive time of the input bb 
// Description : 
//     To compute the mininal execution cycle of the input BB_SET by only 
//     considering the resource usage.
//*****************************************************************************

void
IF_CONVERTOR::Compute_Min_Cycle(INT32& base, BB_CONTAINER& set)
{
    TI_RES_COUNT *counter = TI_RES_COUNT_Alloc(&_m);
    BB_CONTAINER::iterator iter;
    for (iter = set.begin(); 
    iter != set.end(); 
    iter++) 
    {
        OP *op;
        FOR_ALL_BB_OPs_FWD(*iter, op) 
        {
            TI_RES_COUNT_Add_Op_Resources(counter, OP_code(op));
        }
    }
    base += (INT32)ceil(TI_RES_COUNT_Min_Cycles(counter));
}

//*****************************************************************************
// Function : Prob_Of_Area
// Description : 
//     To find out the probablity of two IF_CONV_AREAs according to the 
//     probablity of BBs.
//*****************************************************************************

float
IF_CONVERTOR::Prob_Of_Area(IF_CONV_AREA *area1, 
                           IF_CONV_AREA *area2)
{
    float freq = 0.0;

    BB *head = area2 -> Entry_BB();

    BB *bb;
    BBLIST *bl;
    FOR_ALL_BB_PREDS(head, bl) 
    {
        BB *bb_pred = BBLIST_item(bl);
        BB_CONTAINER& bbs = area1 -> Blocks();
        if (Is_BB_Container_Member(bbs, bb_pred))
        {
            freq += Prob_Local(bb_pred, head) * BB_freq(bb_pred);
          
        }
    }
    float result = BB_freq(area1 -> Entry_BB()) ? 
                   freq / BB_freq(area1 -> Entry_BB()):0;

    return result <1.0 ||FREQ_Match(result, 1.0) ? result: 1.0;   
}

//*****************************************************************************
// Function : Compute_Critical_Path_Len
// Input : 
//     - bb : a basic block for which the function will figure out the length 
//       of its critical path
//     - cycled : it indicates if we need consider the latency of ops. if it 
//           is false, we will look all ops as one-cycle-latency ops.
// Output :
//     an INT value indicating the length of the critical path of the input
// Description : 
//     To compute the length of the critical path of the given basic block
//*****************************************************************************

INT32
IF_CONVERTOR::Compute_Critical_Path_Len(BB *bb, 
                                        BOOL cycled)
{
    // to record the earliest start time of all ops
    BB_OP_MAP earliest_time_table = BB_OP_MAP32_Create(bb, &_m); 
    INT32 critical_path_cycle = 0;

    // build dag for current bb
    if (CG_DEP_Has_Graph(bb)) 
    {
         CG_DEP_Delete_Graph(bb);
    }
    CG_DEP_Compute_Graph (
         bb,
         INCLUDE_ASSIGNED_REG_DEPS,
         NON_CYCLIC,
         NO_MEMREAD_ARCS,
         INCLUDE_MEMIN_ARCS,
         INCLUDE_CONTROL_ARCS,
         NULL);

     
    // search all ops of the bb and compute the earliest-start-time for them
    OP *op;
    FOR_ALL_BB_OPs_FWD(bb,op)
    {
        if ( OP_xfer(op)) 
            break;

        INT32 earliest_start_time = 0;
        INT32 latency;

        // the predecessors of an op are the ops, from which there are 
        // dependence edges pointing to the op;after the following loop, 
        // we can compute the earliest-start-time of an op;
        for (ARC_LIST* arcs = OP_preds(op);   
        arcs != NULL;
        arcs = ARC_LIST_rest(arcs)) 
        {
             ARC* arc = ARC_LIST_first(arcs);
             
             OP* pred = ARC_pred(arc); 
             if (OP_bb(pred) != bb)  continue;

             INT32 start_time;
             start_time = BB_OP_MAP32_Get(earliest_time_table, pred);
             latency = cycled ? ARC_latency(arc):1;
             start_time += latency;
             if ( start_time > earliest_start_time)
                 earliest_start_time = start_time;
        }

        // in the following, we update the length of the longest path 
        // - critical_path_cycle
        BB_OP_MAP32_Set(earliest_time_table, op, earliest_start_time);

        latency = cycled ? TI_LATENCY_Result_Available_Cycle(OP_code(op),0):1;
        INT32 path_time = earliest_start_time + latency;
        if (critical_path_cycle < path_time)  
        {
            critical_path_cycle = path_time;
        }
    }

    CG_DEP_Delete_Graph(bb);

    return critical_path_cycle;
}

//*****************************************************************************
// Function : Find_Area
// Description :
//      Find out the position of an given IF_CONV_AREA from an AREA_CONTAINER
//*****************************************************************************

AREA_CONTAINER::iterator
IF_CONVERTOR::Find_Area(AREA_CONTAINER& areas, 
                        IF_CONV_AREA* area)
{

    AREA_CONTAINER::iterator iter;
    for (iter = areas.begin(); 
    iter!= areas.end(); 
    iter++) 
    {
      if ( area == *iter) 
      {
          return iter;
      }
    }
    return  iter;
}
inline
void     
IF_CONVERTOR::Add_Edge(IF_CONV_AREA *area1, IF_CONV_AREA *area2) 
{
        area1 -> Add_Succ(area2, this);
        area2 -> Add_Pred(area1, this);
}
BOOL
IF_CONVERTOR::Is_BB_Container_Member(BB_CONTAINER& bbs, BB* bb)
{
    BB *block;
    BB_CONTAINER::iterator iter;
    for (iter = bbs.begin(); 
    iter != bbs.end(); 
    iter++) 
    {
        block = *iter;
        if (block == bb) return true;
    }
    return false;
}
//=============================================================================
//    Part 3: functions for if-conversion
//=============================================================================


//*****************************************************************************
// Function : If_Conversion_Init
// Input : 
//        - region: a regino to be if-converted
//        - area_list : it is the result of the function. In area_list, there 
//                    are several IF_CONV_AREAs, which are the candidates to 
//                    be if-converted.  After this function, there is only one 
//                    block in each IF_CONV_AREA. Some properties of those
//                    IF_CONV_AREAs are fingured out. 
// Output: 
//        < none>
// Description : 
//        The module will do something to initialize the data structure for 
//      if-conversion. The main steps of the module are shown as follows:
//            1) Construct an IF_CONV_AREA for each basis block in the input 
//               region. 
//            2) Check if it is legal to if-convert each bb.
//            3) Compute some properties for each IF_CONV_AREA, such as critical 
//               path length and resource usage. Th step 2) and 3) are 
//               implemented in the constructor of the class IF_CONV_AREA. 
//            4) Sort all IF_CONV_AREAs according to the post-DFS order.
//
//*****************************************************************************

void
IF_CONVERTOR::If_Conversion_Init(REGION *region, 
                                AREA_CONTAINER& area_list)
{    
    // create an IF_CONV_AREA for each basic block

    // area_table is used to record the map between basic blocks
    // and IF_CONV_AREAs
    BB_MAP  area_table = BB_MAP_Create();
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter( region -> Regional_Cfg()); 
    iter!=0; 
    ++iter)
    {
        REGIONAL_CFG_NODE *node = *iter;
        BB *pred_bb;
        IF_CONV_AREA *pred_area;

        if (node -> Is_Region()) 
        {    
            // if one area has a region successor, we do not 
            // if-convert it
            for (CFG_PRED_NODE_ITER pred_iter(node);
            pred_iter!=0; 
            ++pred_iter)
            {
                REGIONAL_CFG_NODE *pred = *pred_iter;
                if (pred -> Is_Region()) continue;

                pred_bb = pred -> BB_Node();        
                pred_area = (IF_CONV_AREA*)BB_MAP_Get(area_table, pred_bb);
                Is_True(pred_area != NULL, 
                    ("Can't find corresponding area for BB%d.\n", 
                    BB_id(pred_bb)));
                pred_area -> Area_Type(DOWNWARD_UNSUITABLE);
            }
            continue;
        }
         
        BB *bb = node -> BB_Node();
        IF_CONV_AREA *area;

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY))
        {
            fprintf(TFile, 
                " -- solving BB%d: \n", BB_id(bb));
        }
        // we do not generate IF_CONV_AREA for recovery block  .
        // instead, we add those blocks in the IF_CONV_AREA of their 
        // predecessors

        if ( BB_recovery(bb)) 
        {
            Is_True(node -> Pred_Num() == 1, 
                ("the basic block can only have one predecessor"));
            
            REGIONAL_CFG_NODE *pred = node -> First_Pred() -> Src();
            if (!pred -> Is_Region())
            {
                area = (IF_CONV_AREA*)BB_MAP_Get(area_table, 
                                                 pred -> BB_Node());
                Is_True(pred_area != NULL, 
                    ("Can't find corresponding area for BB%d.\n", 
                    BB_id(pred_bb)));
                area -> Blocks().push_back(bb);
                BB_MAP_Set(area_table, bb, area);
                continue;
            }
        }

        // create an IF_CONV_AREA for the bb and add it into area_list
        area = CXX_NEW(IF_CONV_AREA(bb, this), &_m);
        area_list.push_back(area);

        //record the relation of bb and area into area_table;
        BB_MAP_Set(area_table, bb, area);

        // add edge between IF_CONV_AREAs
        for (CFG_PRED_NODE_ITER pred_iter(node);
        pred_iter!=0; 
        ++pred_iter)
        {
            REGIONAL_CFG_NODE *pred = *pred_iter;
            if ( pred -> Is_Region()) 
            {
                area -> Area_Type(UPWARD_UNSUITABLE);
                continue;
            }

            pred_bb = pred -> BB_Node();
            pred_area = (IF_CONV_AREA*)BB_MAP_Get( area_table, pred_bb);
            Is_True(pred_area != NULL, 
                    ("Can't find corresponding area for BB%d.\n", 
                    BB_id(pred_bb)));
            Add_Edge( pred_area,area );
        }

        if ( node -> Is_Exit()) 
        {
            area -> Area_Type(DOWNWARD_UNSUITABLE);
        }
    }

    BB_MAP_Delete(area_table);
}

//*****************************************************************************
// Function : Detect_Type
// Input : 
//     - area : the head area
//     - cand_list : a container of IF_CONV_AREAs. In fact, it is a part of the 
//                   result. If the control flow pattern of the head area
//                   and its successors are suitable to be if-converted, the
//                   function will return the proper type of it, and add 
//                   related IF_CONV_AREAs into cand_list.
//                   Here, for each CFLOW_TYPE, the order of areas in
//                   cand_list is fixed. It is:
//                   SERIAL_TYPE: area1 -- area;
//                   IF_THEN_TYPE: head -- then -- tail
//                   IF_THEN_ELSE_TYPE: head - then - else - tail
//                                  or  head - else - then - tail
//     - forced : default is FALSE
//                If TRUE a more aggresive pattern matching is used.
//                This should be only used during forced/relaxed if conversion
// Output : 
//     the type of the control flow pattern of the head area and its successors
// Description :
//     The function will look for the candidate areas, in which the control 
//     flow can be removed by if-converting them. Here,  we only convert
//     three kinds of control flow patterns. They are: serial type, if-then 
//     type, and if-then-else. The detail of those types is shown in if-conv.h. 
// 
//*****************************************************************************

CFLOW_TYPE 
IF_CONVERTOR::Detect_Type(IF_CONV_AREA* area, 
                          AREA_CONTAINER& cand_list,
                          BOOL forced)
{
    if (area -> Area_Type() == UNSUITABLE
        || area -> Area_Type() == DOWNWARD_UNSUITABLE)
        return NO_TYPE;

    
    AREA_CONTAINER succs = area -> Succ_Set();
    AREA_CONTAINER::iterator iter;
    
    // check all its successor IF_CONV_AREAs
    for ( iter = succs.begin(); 
    iter != succs.end(); 
    iter++) 
    {
        IF_CONV_AREA  *succ = *iter;
        if (succ -> Area_Type() == UNSUITABLE
            || succ -> Area_Type() == UPWARD_UNSUITABLE)
            return NO_TYPE;
    }

    // decide if it belongs to SERIAL_TYPE
    if (area -> Succ_Num() == 1 )
    {
        IF_CONV_AREA  *succ = *(succs.begin());
        if (succ -> Pred_Num() == 1)
        {
            cand_list.push_back(area);
            cand_list.push_back(succ);
            return SERIAL_TYPE;
        }
    }

    if ( area -> Succ_Num() == 2 ) 
    {
        IF_CONV_AREA  *succ1, *succ2;
        iter = succs.begin();
        succ1 = *iter;
        iter ++;
        succ2 = *iter;
        
        // decide if it is IF_THEN_TYPE
        if (succ1 -> Pred_Num() == 1
            && Find_Area( succ1 -> Succ_Set(), succ2) != succ1->Succ_Set().end())
        {
            cand_list.push_back(area);
            cand_list.push_back(succ1);
            cand_list.push_back(succ2);

            return IF_THEN_TYPE; 
        }

        if (succ2 -> Pred_Num() == 1
            && Find_Area( succ2 -> Succ_Set(), succ1) != succ2->Succ_Set().end())
        {
            cand_list.push_back(area);
            cand_list.push_back(succ2);
            cand_list.push_back(succ1);

            return IF_THEN_TYPE; 
        }

        // decide if it is IF_THEN_ELSE_TYPE
        if ( succ1 -> Pred_Num() == 1
            && succ2 -> Pred_Num() == 1) 
        {
            IF_CONV_AREA* common_succ = NULL;

            for ( iter = succ1 -> Succ_Set().begin(); 
            iter != succ1 -> Succ_Set().end(); 
            iter++) 
            {
                IF_CONV_AREA* succ_of_succ1 = *iter;
                if ( Find_Area( succ2 -> Succ_Set(), succ_of_succ1) 
                    != succ2 -> Succ_Set().end()) 
                {
                    common_succ = succ_of_succ1;
                    break;
                }
            }

            if ( common_succ)
            {
                cand_list.push_back(area);
                cand_list.push_back(succ2);
                cand_list.push_back(succ1);
                cand_list.push_back(common_succ);
                return IF_THEN_ELSE_TYPE;
            }
        }
    }

    if (forced && area -> Succ_Num() == 3) {
      IF_CONV_AREA *area1, *area2, *area3;
      IF_CONV_AREA *succ1, *succ2, *common_succ;
      iter = succs.begin();
      area1 = *iter;
      iter++;
      area2 = *iter;
      iter++;
      area3 = *iter;

      if (   area1->Pred_Num() >= 3
          && area2->Pred_Num() == 1
          && area3->Pred_Num() == 1) 
      {
        // area1 might be common successor
        common_succ = area1;
        succ1       = area2;
        succ2       = area3;
      }
      else if (   area1->Pred_Num() == 1
               && area2->Pred_Num() >= 3
               && area3->Pred_Num() == 1) 
      {
        // area2 might be common successor
        common_succ = area2;
        succ1       = area1;
        succ2       = area3;
      }
      else if (   area1->Pred_Num() == 1
               && area2->Pred_Num() == 1
               && area3->Pred_Num() >= 3) 
      {
        // area3 might be common successor
        common_succ = area3;
        succ1       = area1;
        succ2       = area2;
      }
      else {
        return NO_TYPE;
      }
      
      if (Find_Area (succ1->Succ_Set(), common_succ) 
          == succ1->Succ_Set().end())
      {
        return NO_TYPE;
      }

      if (Find_Area (succ2->Succ_Set(), common_succ)
          == succ2->Succ_Set().end())
      {
        return NO_TYPE;
      }
      
      cand_list.push_back(area);
      cand_list.push_back(succ1);
      cand_list.push_back(succ2);
      cand_list.push_back(common_succ);
      
      return IF_THEN_ELSE_TYPE;
    }

    return NO_TYPE;
}

//*****************************************************************************
//  Function : Worth_If_Convert
//  Input : 
//      - cand_list : a list of candidate areas, which are candidates to be 
//                     reduced to one bigger area if they can pass the 
//                    test of this function
//      - type : the type of the comming bigger area
//  Output :
//      a boolean value which indicates if the candidates have pass the test
//  Description : 
//      The function will decide whether the recognized area is worth being 
//      if-converted. The criteria used here are mainly about: the length of 
//      critical path, the resource usage, the miss rate of branch predict, 
//      the miss-penalty of branch predict, the number of instructions.
//*****************************************************************************

BOOL
IF_CONVERTOR::Worth_If_Convert (AREA_CONTAINER& cand_list, 
                                CFLOW_TYPE type,
                                BOOL forced)
{
    if ( Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
    {
        fprintf(TFile, " \nConsider profitablity:");
    }

    // take out the head area and its successors
    AREA_CONTAINER::iterator iter = cand_list.begin();
    IF_CONV_AREA  *head = *iter;
    Is_True(head != NULL, (" All if-conv types have a head-BB.\n "));

    iter++;
    IF_CONV_AREA *succ1 = *iter;
    IF_CONV_AREA *succ2 = NULL;
    Is_True(succ1 != NULL, 
        (" All if-conv types have at least one successor.\n "));

    if ( type == IF_THEN_ELSE_TYPE ) 
    {
        iter++;
        succ2  = *iter;
    }

    // evaluate the execution time for unpredicated code
    // here, we assume the latency of a call instruction is a big value
    INT32  time = 0;
    float  exe_time = 0.0;
    INT32 min_cycle_1 = 0;
    Compute_Min_Cycle(min_cycle_1, succ1 -> Blocks());
    INT32 min_cycle_2 = 0;
    if ( succ2 ) 
        Compute_Min_Cycle(min_cycle_2, succ2 -> Blocks());

    if (succ1 -> Cycled_Critical_Len() > min_cycle_1)
    {
        time = succ1 -> Cycled_Critical_Len();
    } else {
        time = min_cycle_1;
    }

    float taken_rate_1 = Prob_Of_Area(head, succ1);
    float taken_rate_2 = 1 - taken_rate_1;
    exe_time += taken_rate_1 * time; 
    if (succ2) 
    {
        if (succ2 -> Cycled_Critical_Len() > min_cycle_2)
        {
            time = succ2 -> Cycled_Critical_Len();
        } else {
            time = min_cycle_2;
        }
        exe_time += taken_rate_2 * time;
    }

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
    {
        fprintf(TFile, " \n Prob : Area_%d ==> Area_%d: %f\n", 
            head -> Area_Label(),
            succ1 -> Area_Label(),
            taken_rate_1);
        if ( succ2 )
        {
            fprintf(TFile, " Prob : Area_%d ==> Area_%d: %f\n", 
                head -> Area_Label(),
                succ2 -> Area_Label(),
                taken_rate_2);
        }
        fprintf(TFile, " min_cycle_1 : %d, min_cycle_2 : %d \n", 
            min_cycle_1, min_cycle_2);
        fprintf(TFile, " critical_cycle_1 : %d ", 
            succ1 -> Cycled_Critical_Len());
        if ( succ2 ) 
        {
            fprintf(TFile, ", critical_cycle_2 : %d \n", 
                succ2 -> Cycled_Critical_Len());
        } else {
            fprintf(TFile, "\n");
        }
    }

    float branch_predict_miss_rate;
    if ( taken_rate_1 > taken_rate_2)
    {
        branch_predict_miss_rate = taken_rate_2;
    } else {
        branch_predict_miss_rate = taken_rate_1;
    }

    INT32 branch_predict_miss_panelty, fixed, brtaken;
    double factor;
    
    CGTARG_Compute_Branch_Parameters(&branch_predict_miss_panelty,
        &fixed, &brtaken, &factor);

    float unpredicated_time;
    unpredicated_time = exe_time + 
        branch_predict_miss_rate * branch_predict_miss_panelty;

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
    {
        fprintf(TFile, 
            " unpredicated %f cycles vs", unpredicated_time);
    }

    // evaluate the execution time for predicated code
    INT32 max_res_height = 0;
    exe_time= 0.0;
    if ( succ2 ) 
    { 
        // compute execution time
        if (succ1 -> Cycled_Critical_Len() > succ2 -> Critical_Len())
        {
            time = succ1 -> Cycled_Critical_Len();
        } else {
            time = succ2 -> Critical_Len();
        }
        exe_time += taken_rate_1* time; 

        if (succ2 -> Cycled_Critical_Len() > succ1 -> Critical_Len())
        {
            time = succ2 -> Cycled_Critical_Len();
        } else {
            time = succ1 -> Critical_Len();
        }
        exe_time += taken_rate_2* time; 

        // compute the minimal cycle only considering the resource usage
        Compute_Min_Cycle(max_res_height, succ1 -> Blocks());
        Compute_Min_Cycle(max_res_height, succ2 -> Blocks());

    } else {
        exe_time += succ1 -> Cycled_Critical_Len();
        Compute_Min_Cycle(max_res_height, succ1 -> Blocks());
    }

    float predicated_time;
    predicated_time = exe_time > max_res_height ? exe_time : max_res_height;

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
    {
        fprintf(TFile, 
            " predicated %f cycles\n", predicated_time);
    }

    // decide if it is worth if-convertion
    // here, if the execution time of predicated code is less then the 
    // execution time of unpredicated code, we think it should be if-converted
    if (unpredicated_time > predicated_time) {
      return TRUE;
    } 
    else if (forced &&
             ((predicated_time*100) < (_loop_length*IF_CONV_BIASE_THRESHOLD))) 
    {
      return TRUE;
    }
    else {
      return FALSE;
    }
}


//*****************************************************************************
// Function : Reduce_By_Type 
// Input :
//     -cand_list : a list of IF_CONV_AREAs, which are going to be reduced in
//                  one IF_CONV_AREA. And such a IF_CONV_AREA is a candidate 
//                  area for if-conversion.
//     -type : the type of the result IF_CONV_AREA.
//     -area_list:  the total IF_CONV_AREA list of the present region.
// Output : 
//     <none>
// Description : 
//      The function will reduce the selected areas into one IF_CONV_AREA. 
//      It mainly consists of three steps:
//      1)    add all blocks in cand_list into the first area;
//      2)    delete all areas except the first area from the area list; 
//      3)    maintain the predecessor and successor of the first area;
//      4)  maintain the related properties of the first area, 
//          such as critical_path_length; 
//
//*****************************************************************************

void
IF_CONVERTOR::Reduce_By_Type(AREA_CONTAINER& cand_list,
                             CFLOW_TYPE type, 
                             AREA_CONTAINER& area_list)
{
    AREA_CONTAINER::iterator iter= cand_list.begin();
    IF_CONV_AREA *head = *iter;
    Is_True(head != NULL, 
        (" All if-conv types have head-BB.\n "));
    iter++;

    INT32    max_length1 = 0;
    INT32    max_length2 = 0;

    IF_CONV_AREA *succ1 = NULL;
    IF_CONV_AREA *succ2 = NULL;
    IF_CONV_AREA *tail = NULL;

    succ1 = *iter;
    iter++;
    Is_True(succ1 != NULL, 
        (" All if-conv types have at least one successor.\n "));

    if (type == IF_THEN_ELSE_TYPE)
    {
        succ2  = *iter;
        iter++;
        Is_True(succ2 != NULL, 
            (" IF_THEN_ELSE_TYPE have at least two successor.\n "));
    }
    
    if (iter != cand_list.end())
    {
        tail = *iter;
    } 
    // maintain the if-conversion type
    if (type == SERIAL_TYPE) 
    {
        if (head -> Conv_Type() == FULLY_IF_CONV
            || succ1 -> Conv_Type() == FULLY_IF_CONV) 
        {
            head -> Conv_Type(FULLY_IF_CONV);
        }

    } else {
        head -> Conv_Type(FULLY_IF_CONV);
    }

    if ( succ1 -> Area_Type() == DOWNWARD_UNSUITABLE 
        || ( succ2 && succ2 -> Area_Type() == DOWNWARD_UNSUITABLE))
    {
        if ( head -> Area_Type() == UPWARD_UNSUITABLE)
        {
            head -> Area_Type(UNSUITABLE);
        } else {
            head -> Area_Type(DOWNWARD_UNSUITABLE);
        }
    }
    
    // combine the included blocks into head
    head -> Combine_Blocks_With(succ1, &_m);

    if ( type == IF_THEN_ELSE_TYPE )
    {
        head -> Combine_Blocks_With(succ2, &_m);
    }

    // maintain the edges
    AREA_CONTAINER successors = succ1 -> Succ_Set();
    for (iter = successors.begin(); 
    iter != successors.end(); 
    iter++)
    {
        IF_CONV_AREA *succ_of_succ1 = *iter;
        if (tail && succ_of_succ1 == tail) continue;

        head -> Add_Succ(succ_of_succ1, this);
        succ_of_succ1 -> Add_Pred(head, this);
        succ_of_succ1 -> Del_Pred(succ1, this);
    }
    if (type == SERIAL_TYPE)
    {
        head -> Del_Succ(succ1, this);
    }

    if (type == IF_THEN_TYPE)
    {
        head -> Del_Succ(succ1, this);
        tail -> Del_Pred(succ1, this);
    }

    if (type == IF_THEN_ELSE_TYPE)
    {
        AREA_CONTAINER successors = succ2 -> Succ_Set();
        for (iter = successors.begin(); 
            iter != successors.end(); 
            iter++)
        {
            IF_CONV_AREA* succ_of_succ2 = *iter;
            if (tail && succ_of_succ2 == tail) 
                continue;

            head -> Add_Succ(succ_of_succ2, this);
            succ_of_succ2 -> Add_Pred(head, this);
            succ_of_succ2 -> Del_Pred(succ2, this);
        }
        head -> Del_Succ(succ1, this);
        head -> Del_Succ(succ2, this);
        head -> Add_Succ(tail, this);
        
        tail -> Del_Pred(succ1, this);
        tail -> Del_Pred(succ2, this);
        tail -> Add_Pred(head, this);
    }

    // maintain some properties of head
    if (type == IF_THEN_ELSE_TYPE)
    {
        max_length1 = 
            succ1 -> Cycled_Critical_Len() > succ2 -> Cycled_Critical_Len()?
            succ1 -> Cycled_Critical_Len():succ2 -> Cycled_Critical_Len();
        max_length2 = 
            succ1 -> Critical_Len() > succ2 -> Critical_Len()?
            succ1 -> Critical_Len():succ2 -> Critical_Len();
    } else {
        max_length1 = succ1 -> Cycled_Critical_Len();
        max_length2 = succ1 -> Critical_Len() ;
    }
    head -> Cycled_Critical_Len(head-> Cycled_Critical_Len() + max_length1);
    head -> Critical_Len(head -> Critical_Len() + max_length2);

    // delete unnecessary areas
    area_list.erase(Find_Area(area_list, succ1));
    CXX_DELETE(succ1, &_m);
    
    if (type == IF_THEN_ELSE_TYPE)
    {
        area_list.erase( Find_Area(area_list, succ2));
        CXX_DELETE(succ2, &_m);
    }

}

//*****************************************************************************
// Function : Select_Candidates
// Input :
//        - area_list : a list of IF_CONV_AREAs
// Output : 
//        <none>
// Description: 
//        The function  is to find the candidates for if-conversion. 
//        Generally, the function will recognize the IF_CONV_AREAs which can not
//        be if-converted and combine the IF_CONV_AREAs, which are suitable to be
//        if converted, into larger ones. Before combination, we mainly do 
//        profitablity checking. The legality checking has been done in the 
//        process of initialization. 
//        Here, the partial if-conversion will be delayed to the second release. 
//        The function -- Worth_Partial_If_Convert will always return false.
//
//*****************************************************************************

void
IF_CONVERTOR::Select_Candidates (AREA_CONTAINER& area_list, BOOL forced) 
{ 
    IF_CONV_ALLOC temp_alloc(&_m);

    AREA_CONTAINER cand_list(temp_alloc);
    AREA_CONTAINER::iterator   iter;

    BOOL reduced = TRUE;
    INT time = 0;
    while (reduced) 
    {

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)){
            fprintf(TFile," **** iteration %d ****\n", time);
        }

        reduced = FALSE;
        for (iter = area_list.begin(); 
        iter!= area_list.end(); 
        iter++) 
        {
            IF_CONV_AREA  *area = *iter;
            cand_list.clear();

            if ( Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
            {
                fprintf(TFile, 
                    " -- solving AREA%d: \n", area -> Area_Label());
            }
            
            // first, we try to find out an if-conversion area, whose head is 
			// the head of area, according to the control flow
            CFLOW_TYPE type = Detect_Type (area, cand_list, forced);
            if (type == NO_TYPE)  continue;

            if ( Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY))
            {
                fprintf(TFile, " Selected areas:");
                AREA_CONTAINER::iterator  it;
                for (it = cand_list.begin(); 
                it != cand_list.end(); 
                it++)
                {
                    fprintf(TFile, "%d    ", (*it)->Area_Label());
                }
                fprintf(TFile,"\n");
            }
            
            // profitability checking
            if (type == SERIAL_TYPE || 
                Worth_If_Convert (cand_list, type, forced))
            {    
              reduced = TRUE;
            }
            else {
                continue;
            }
            Reduce_By_Type(cand_list, type, area_list);
        }
        time++;
    }
}

//*****************************************************************************
// Function : Find_Start_Node
// Input : 
//   - area : a candidate IF_CONV_AREA
//   - bb : the bb for which we will find starting node
// Output : 
//   <none>
// Description :
//   To find starting-nodes for a bb.
//
//   Here, If we decide to generate parallel compare for one node, the 
//   'starting nodes' of it are the earliest nodes with which we can start to 
//   generate parallel compare.  
//   For example: 
//                                      1
//                                     /  \
//                                    2    
//                                   / \
//                                  3
//                                 / \
//                                4
//   if we decide to generate parallel compare for BB_4. The control condition 
//   of it can be (cond_2 && cond3) or (cond1 && cond2 && cond3) <cond_x mean 
//   the compare condition of BB_x>. Here, if we take (cond2 && cond3), we call
//   start node of BB_4 is BB_2 and if we take (cond1 && cond2 && cond3), we 
//   call BB_1 the start_node.  
//
//   For multiple cntl-dep-parent node, we will look for multiple start-nodes 
//   for it corresponds to its all cntl-dep-parent. 
//
//   If there is no obstacle in BB2 and BB3 to block us to move generated 
//   parallel compare instruction up to BB1, we can move them to the start node
//   and all generated parallel compare instruction can be guarded by the 
//   predicates of starting nodes. In such conditions, we call BB2 and BB3 
//   is transitional. That is :
//   'transitional' means a bb will not block generated parallel compare in the
//    BB to be moved up.
//*****************************************************************************

void
IF_CONVERTOR::Find_Start_Node(IF_CONV_AREA* area, 
                              BB* bb)
{
    BB_PREDICATE_INFO *info = area ->  Pred_Assign_Info(bb);
    Is_True( info != NULL, 
        ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(bb)));

    CNTL_DEP *cntl_info = area -> Cntl_Dep_Info();
    BB_SET *cds = cntl_info -> Cntl_Dep_Parents(bb);

    // we do not generate parallel compare for the transitional node whose 
    // compare instruction will  be deleted.  Because that means there will 
    // be no instruction to be guarded by the qualifying predicates of it.
    if (!IPFEC_Force_Para_Comp_Gen)
    {
        if (info -> Transitional() && BB_SET_Size(cds) == 1) 
        {
            return;
        }
    }

    
    BB *cd;
    //look for the starting nodes from bottom to up
    FOR_ALL_BB_SET_members(cds, cd)
    {
        BB *sn = cd;
        if (!IPFEC_Force_Para_Comp_Gen && info -> Transitional()) 
        {
            // as above, for transitional node, we do not want to generate 
            // parallel compare for it. But for or-type node, we must 
            // generate parallel compare, so here, we just set the immediate
            // cntl_dep_parents as its starting node.
            info -> Start_Node(sn, cd);
            continue;
        }
        INT n = 0;
        while (n < MAX_NUM_PARA_CMP)
        {
            BB_PREDICATE_INFO *f = area ->  Pred_Assign_Info(sn);
            Is_True( f != NULL, 
                ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(sn)));

            // a un-transitional node can stop us to go further to 
            // look for the starting node
            if (!f -> Transitional()) break; 

            // the following conditions are to keep us from introducing 
            // too complicated conditions for parallel compare candidates

            BB_SET *cds_of_sn = cntl_info -> Cntl_Dep_Parents(sn);
            Is_True( cds_of_sn != NULL, 
                ("Can't find cntl deps for BB%d.\n", BB_id(sn)));

            // a multiple-cntl-dep-parent node can stop us to go further 
            // to look for the starting node 
            if (BB_SET_Size(cds_of_sn) != 1) break;  
            
            // when the present sn is not cntl-dep-parent of bb, the sn
            // will stop us go further
            // for example :
            //                      1
            //                    /  \
            //                   2
            //                 /  \
            //                3  
            //               / \  
            //              /   4
            //              \  / \
            //                5
            // when we look for the starting node for 5, if we find 3 is 
            // start-node. the condition of 5 is cond3 || cond4. Then, we can 
            // generate parallel compare for it. But, if we find that 2 is 
            // the start-node corresponding to 3, the condition is 
            // ( cond2 && cond3) || cond4
            // the condition is too complex to generate parallel compare. 
            if (BB_SET_Size(cds) > 1 
                && !BB_SET_MemberP(cds, BB_SET_Choose(cds_of_sn)) )
                break;

            sn = BB_SET_Choose(cds_of_sn);
            n ++;
        }

        info -> Start_Node(sn, cd);
    }
}

BOOL  
IF_CONVERTOR::Check_If_Gen_Useless_Predicate( BB_PREDICATE_INFO* info)
{
    // check if there is any useful predicate generated here 
    
    if ( info -> True_TN() || info -> False_TN())
        return false;

    TN_CONTAINER set;
    set = info -> Or_TNs();
    if (!set.empty())
        return false;
    set = info -> And_TNs();
    if (!set.empty())
        return false;
    set = info -> Orcm_TNs();
    if (!set.empty())
        return false;
    set = info -> Andcm_TNs();
    if (!set.empty())
        return false;   
    
    return true;
}  

//*****************************************************************************
// Function : Record_Para_Comp_Info
// Input : 
//   - area : the candidate IF_CONV_AREA
//   - bb : the basic block to be solved
// Output :
//   <none>
// Description :
//   The following function will record some information for the predicate 
//   of the basic block. The information includes: in a basic block, how many 
//   predicate assignment instructions should be inserted into, what the type 
//   they are, what are their target predicates and so on. 
//*****************************************************************************

void
IF_CONVERTOR::Record_Para_Comp_Info(IF_CONV_AREA *area, 
                                    BB *bb)
{
    CNTL_DEP *cntl_info = area -> Cntl_Dep_Info();
    BB_PREDICATE_INFO *pred_info = area -> Pred_Assign_Info(bb);
    Is_True(pred_info != NULL, 
        ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(bb)));

    TN *p = pred_info -> Predicate();
    BB *entry = area -> Entry_BB();

    BB_SET *cds = cntl_info -> Cntl_Dep_Parents(bb);
    BB_SET *true_edges = cntl_info -> True_Cntl_Dep_Parents(bb);

    // solve the non-parallel-compare-candidates
    if (!pred_info -> Has_Start_Node()) 
    {
        Is_True(BB_SET_Size(cds) <=1,("non-parallel-comparea-candidate "
            "can only has one control dependentor!\n"));

        if (BB_SET_Size(cds) == 0) return;
        TN *true_tn = NULL; 
        TN *false_tn = NULL;
        BB *bb_cd = BB_SET_Choose(cds);    
        BB_PREDICATE_INFO *pred_info_of_cd = area -> Pred_Assign_Info(bb_cd);
        Is_True(pred_info != NULL, 
            ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(bb_cd)));

        if (BB_SET_MemberP(true_edges, bb_cd))
        {
            pred_info_of_cd -> True_TN(p);
        } else {
            pred_info_of_cd -> False_TN(p);
        }
        return;
    }
    
    if (Get_Trace(TP_PTRACE1, TP_PTRACE1_CG))
    {
        char tmp_string[6]="";
        sprintf(tmp_string, "%d", BB_id(bb));
        Generate_Tlog("AIC", "parallel_compare_generation", (SRCPOS)0,
            tmp_string, "", "", "");
    }
    //solve the parallel compare candidates
    if ( BB_SET_Size(cds) > 1) 
    {
        // for or/orcm type

        // insert an initialization instruction for the or-type predicate
        OPS ops = OPS_EMPTY;
        Exp_Pred_Set(p, True_TN, 0, &ops);
        hTN_MAP_Set(init_op_info, p, ops.first);
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            Print_OPS(&ops);
        }

        // Insert the predicate initialization code just before the branch
        // (if it exists) or append it to the Entry block.
        OP *xfer_op;
        if (xfer_op = BB_xfer_op(entry)) 
        {
            BB_Insert_Ops_Before (entry, xfer_op, &ops);
        } else {
            BB_Append_Ops (entry, &ops);
        }

        // record the information about the predicates assignments in the 
        // corresponding start node
        BB *cd; 
        FOR_ALL_BB_SET_members(cds, cd)
        { 
            BB_PREDICATE_INFO* info_of_cd = area -> Pred_Assign_Info(cd);
   
            Is_True(info_of_cd != NULL, 
                ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(cd)));

            if (BB_SET_MemberP(true_edges, cd))
            {
                info_of_cd -> Add_Or_TNs(p);
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fPrint_TN(TFile, "add  or %s ", p);
                    fprintf(TFile, 
                        " into BB%d when solving BB%d\n", BB_id(cd), BB_id(bb));
                }
            } else {
                info_of_cd -> Add_Orcm_TNs(p);
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fPrint_TN(TFile, "add  orcm %s ", p);
                    fprintf(TFile, 
                        " into BB%d when solving BB%d\n", BB_id(cd), BB_id(bb));
                }
            }
        }
    } else { 
        Is_True( BB_SET_Size(cds) == 1, 
            (" and-type node can only have one cd.\n"));

        // for and/andcm type

        // assign a new predicate for the and/andcm-type candidate
        p = Gen_Predicate_TN();
        pred_info -> Orig_Pred(pred_info -> Predicate());
        pred_info -> Predicate(p);
        
        BB *cd = BB_SET_Choose(cds);

        // insert an initialization instruction for the or-type predicate
        OPS ops = OPS_EMPTY;
        Exp_Pred_Set(p, True_TN, 1, &ops);
        hTN_MAP_Set(init_op_info, p, ops.first);
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            Print_OPS(&ops);
        }

        BB* start_node = pred_info -> Start_Node(cd);
        OP *xfer_op;
        if (xfer_op = BB_xfer_op(start_node)) 
        {
            BB_Insert_Ops_Before (start_node, xfer_op, &ops);
        } else {
            BB_Append_Ops (start_node, &ops);
        }

        BB *sn = pred_info -> Start_Node(cd);
        BB_PREDICATE_INFO *info_of_cd; 
        Is_True(info_of_cd != NULL, 
            ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(cd)));

        BOOL is_start = false;
        while (!is_start)
        {
            is_start = (cd == sn);
            info_of_cd = area -> Pred_Assign_Info(cd);
            
            if (BB_SET_MemberP(true_edges, cd))
            {
                info_of_cd -> Add_And_TNs(p);
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fPrint_TN(TFile, "add  and %s ", p);
                    fprintf(TFile, 
                        " into BB%d when solving BB%d\n", BB_id(cd), BB_id(bb));
                }
            } else {
                info_of_cd -> Add_Andcm_TNs(p);
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fPrint_TN(TFile, "add  andcm %s ", p);
                    fprintf(TFile, 
                        " into BB%d when solving BB%d\n", BB_id(cd), BB_id(bb));
                }
            }
            true_edges = cntl_info -> True_Cntl_Dep_Parents(cd);
            cds = cntl_info -> Cntl_Dep_Parents(cd);
            cd = BB_SET_Choose(cds);
        }
    }
}

//*****************************************************************************
// Function : Detect_Para_Comp
// Input :
//   - area : a candidate IF_CONV_AREA
// Output :
//   <none>
// Description :
//   In the following two cases, we need to generate parallel compare: 
//   1)    when a basic block has multiple parents in the control dependent tree, 
//      we need to generate parallel compare for the basic block. In the case, 
//      the condition to invoke the basic block is the result of or-operation 
//      of all conditions of its control-dependence parents. So, we must 
//      generate parallel compare to predicate the block.
//   2)    generating parallel compare can shorten the dependent height of a 
//      serial of compares. In the case, we generate parallel compare for 
//      optimizing code.
//   The function is used to select the chance to generate parallel compare. 
//   'Parallel compare candidates' indicate those basic blocks, whose 
//   qualifying predicate of which we decide to generate parallel compare. 
//   Besides, we call the instructions to assign to the predicate as related 
//   predicated assignments.
//
//   In detail, the function will 
//   1) select the parallel compare candidates; 
//   2) compute the starting node for each parallel compare candidates. 
//      As a result, the starting node is saved in one member of 
//      BB_PREDICATE_INFO -- start_nodes. If its start_nodes is NULL, we will 
//      not generate parallel compare for the predicate of the node. 
//      Start_nodes is a BB_MAP. In the BB_MAP, we record the starting node 
//      for each control dependencer of the basic block. 
//   The conception of 'starting-node' is in the description of function 
//   'Find_Start_Node'.
//  
//*****************************************************************************

void 
IF_CONVERTOR::Detect_Para_Comp(IF_CONV_AREA* area)
{

    BB_CONTAINER& bbs = area -> Blocks();
    CNTL_DEP *cntl_info = area -> Cntl_Dep_Info();

    //
    // step1: mark transitional nodes
    //
    // Here, we should generate parallel compare only when  
    // the action can lead performance improvement. So we need a cost model 
    // to select parallel compare candidates. In the first release, the cost 
    // model will be very simple. If there is not a downward-exposed definition
    // in one basic block, we remark it and call it as a transitional node. 
    // That means, there is no obstacle for us to move the related predicate 
    // assignments up to the control dependent parent of it. If several 
    // predicate assignments can be moved to one place, they can run in 
    // one cycle and the performance will be better.
    //

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
    {
        fprintf(TFile, " Start to decide transitional!\n");
    }
    BB_CONTAINER::iterator bb_iter;
    BB *bb;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, " -- solving BB%d:", BB_id(bb));
        }

        if (BB_recovery(bb)) continue;

        OP *br_op = BB_branch_op(bb);
        OP *cmp_op = NULL;
        if (br_op && OP_has_predicate(br_op))
        {
            TN *pred_tn = OP_opnd(br_op, OP_PREDICATE_OPND);    
            vector<OP *> ops;
            cmp_op = TN_Defined_At_Op(pred_tn, br_op, &ops);
        }

        OP *op;
        BOOL is_transitional = TRUE;

        BB_SET* cd_children = BB_SET_Create_Empty(PU_BB_Count, &_m);
        cntl_info -> Cntl_Dep_Children(cd_children, bb, &_m);
        if (BB_SET_Size(cntl_info -> Cntl_Dep_Parents(bb)) ==0 
            || BB_SET_Size(cd_children) == 0)
        {
            is_transitional = FALSE;
        } else if ( cmp_op && !Has_Para_Comp_Top(cmp_op)) {
            is_transitional =FALSE;
        } else {
            if ( !br_op || !cmp_op ) {
                is_transitional = FALSE;
            } else {
                FOR_ALL_BB_OPs_FWD(bb, op)
                {
                    if ( op != br_op && op != cmp_op ) {
                        is_transitional = FALSE;
                        break;
                    }
                }
            }
        }

        if (IPFEC_Force_Para_Comp_Gen ||
            (is_transitional && IPFEC_Para_Comp_Gen))
        {
            BB_PREDICATE_INFO  *pred_info = area -> Pred_Assign_Info(bb);
            Is_True(pred_info != NULL, 
                ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(bb)));

            pred_info -> Set_Transitional();
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, "      transitional \n");
            }
        } else {
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, "      not transitional \n");
            }
        }
        
    } 

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
    {
        fprintf(TFile, 
            " Start to find starting node and collect info of pred! \n");
    }

    //step2: compute starting-nodes and collect information of predicates
    BB_CONTAINER result;
    cntl_info -> Get_Post_Order(result, this);
    for (bb_iter = result.begin();
    bb_iter != result.end();
    bb_iter++)
    {
        bb = *bb_iter;

        BB_PREDICATE_INFO  *info = area -> Pred_Assign_Info(bb);
        Is_True(info != NULL, 
            ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(bb)));

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, " -- solving BB%d:\n", BB_id(bb));
        }
        // we will ignore the recovery nodes during predicating
        if ( BB_recovery(bb)) continue;
        if (!info -> Eqv_Class_Head())
        {
            BB_PREDICATE_INFO  *head_info = 
                area -> Pred_Assign_Info(info -> Eqv_Class_Head_BB());
            info -> Orig_Pred( info -> Predicate() );
            info -> Predicate(head_info -> Predicate());
            continue;
        }

        // compute starting-nodes for parallel compare candidates
        BB_SET* cds = cntl_info -> Cntl_Dep_Parents(bb);
        Is_True( cds != NULL, 
            ("Can't find cntl deps for BB%d.\n", BB_id(bb)));

        BOOL cd_is_transitional = false;
        if (BB_SET_Size(cds) == 1)
        { 
            BB*  cd = BB_SET_Choose(cds);
            BB_PREDICATE_INFO  *info_of_cd = area -> Pred_Assign_Info(cd);
            cd_is_transitional = info_of_cd -> Transitional();
        }
        if (  BB_SET_Size(cds) >1 || cd_is_transitional)
            Find_Start_Node(area,bb);
        
        // collect the information of all predicates and their related 
        // assignments
        Record_Para_Comp_Info(area, bb);
    }
}

//*****************************************************************************
// Function : Gen_Para_Comp 
// Input :
//   - area : the candidate IF_CONV_AREA
// Output :
//   <none>
// Description :
//   In this function, we will generate the parallel compare for each basic 
//   block. Here, we will try to generate the instructions as few as possible. 
//   For example, we have an 'or' TN -- p and an 'andcm' TN - q, we will 
//   generate 'p,q = cmp. or. andcm cond  (qp)' instead of  
//   'p = cmp.or cond   (qp) ; q = cmp.andcm cond (qp)'. 
//   Even though, after this phase, some useless predicate assignments will 
//   appear. In the first release, we depend on the dead-code- elimination 
//   to eliminate those assignments.
//*****************************************************************************

void
IF_CONVERTOR::Gen_Para_Comp (IF_CONV_AREA *area)
{
    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
    {
        area -> Print(TFile);
    }
    BB_CONTAINER& bbs = area -> Blocks();
    BB_CONTAINER::iterator bb_iter;
    BB *bb;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, " -- solving BB%d:\n", BB_id(bb));
        }
        
        BB_PREDICATE_INFO  *info = area -> Pred_Assign_Info(bb);
        Is_True( info != NULL, 
            ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(bb)));
        if (BB_recovery(bb)) continue;
           
        // in the following steps, we try to combine the predicate assignments 
        // which fit the following conditions:
        // 1) their qualifying predicates are identical; Here, the compare 
        //    instructions to be inserted into one basic block will use the 
        //    qualifying predicate of the basic block. So their guarding 
        //    predicates are surely identical.
        // 2) their types are compatible;
        //   here the compatible types are: 
        //   and/and, or/or, andcm/andcm, orcm/orcm/, or/andcm, and/orcm
        // 
        OPS op_list = OPS_EMPTY;

        
        if (Check_If_Gen_Useless_Predicate(info)) continue;

        OP *br_op = BB_branch_op(bb);
        OP *cmp_op = NULL;
        if (br_op && OP_has_predicate(br_op))
        {
            TN *pred_tn = OP_opnd(br_op, OP_PREDICATE_OPND);
            vector<OP *> ops;
            cmp_op = TN_Defined_At_Op(pred_tn, br_op, &ops);
        }

        //
        // For example, if there is a compare instruction in the bb, such as:
        //   p, q = cmp_type, a>b (qp) 
        // now, we want to add assignment for two predicates: p1, q2, and they 
        // are or-type and they have the same quaulifing predicate 
		// -- qp_of_start. now , we have two approaches:
        // Approach1:  p, q = cmp_type a>b (qp) 
        //             p1, q2 = cmp_or 0!=0  (p)
        // Approach2:  p , q = cmp_type a>b (qp) 
        //             p1, q1 = cmp_or a>b (qp_of_start)
        // here, we adopt approach2 if it is legal. 
        // 
        TN  *p1, *p2, *start_qp;
        TOP pred_top;
        TN  *qp;
        COMPARE_TYPE type;
        while (Get_2_Pred_And_Erase(p1, p2, type, bb, info)) 
        {
            TN *qp1 = Get_Start_Node_Pred(p1, bb, area);
            TN* qp2 = NULL;
            if (p2 != True_TN)
            {
                qp2 = Get_Start_Node_Pred(p2, bb, area);
            }
            if (p2 == True_TN || qp1 == qp2) 
            {
                pred_top = Get_Para_Comp_Top(cmp_op, type);
                qp = qp1;
            } else {
                pred_top = TOP_UNDEFINED;
            }
            if (pred_top == TOP_UNDEFINED) 
            {
                TN *true_tn = NULL;
                TN *false_tn = NULL;
                Find_BB_Predicates(bb, true_tn, false_tn);

                if (type == COMPARE_TYPE_or 
                    || type == COMPARE_TYPE_andcm 
                    || type == COMPARE_TYPE_or_andcm)
                {
                    qp = true_tn ? true_tn : True_TN;
                } else if (type == COMPARE_TYPE_orcm 
                    || type == COMPARE_TYPE_and
                    || type == COMPARE_TYPE_and_orcm) 
                {
                    qp = false_tn ? false_tn: True_TN;
                } else {
                    Is_True(0, ("wrong compare type!\n"));
                }
            }
            Gen_Predicate_Assign(p1,p2, cmp_op, type, pred_top, qp, 
                &op_list);
        }

        OP* new_op;
        FOR_ALL_OPS_OPs_FWD((&op_list),new_op){
            Set_OP_cond_def_kind(new_op, OP_ALWAYS_COND_DEF); 
        }
        // Insert these just before the ending branch
        if (OPS_first(&op_list))
        {
            Is_True(br_op,
                ("parallel compare can only added into split bb.\n"));

            BB_Insert_Ops(bb, br_op, &op_list, true);
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                Print_OPS(&op_list);
            }
        }
    }
    
}
TOP
cmp_top_index[COMP_TOP_NUM] = 
{                          TOP_cmp_ne,       TOP_cmp_ne_unc,       
                           TOP_cmp4_ne,      TOP_cmp4_ne_unc, 
                           TOP_cmp_i_ne,     TOP_cmp_i_ne_unc,     
                           TOP_cmp4_i_ne,    TOP_cmp4_i_ne_unc,
                           TOP_cmp4_ge,      TOP_cmp4_ge_unc,      
                           TOP_cmp4_gt,      TOP_cmp4_gt_unc,
                           TOP_cmp4_le,      TOP_cmp4_le_unc,      
                           TOP_cmp4_lt,      TOP_cmp4_lt_unc, 
                           TOP_cmp_ge,       TOP_cmp_ge_unc,       
                           TOP_cmp_gt,       TOP_cmp_gt_unc, 
                           TOP_cmp_le,       TOP_cmp_le_unc,       
                           TOP_cmp_lt,       TOP_cmp_lt_unc,
                           TOP_cmp4_i_eq,    TOP_cmp4_i_eq_unc,    
                           TOP_cmp4_eq,      TOP_cmp4_eq_unc,
                           TOP_cmp_i_eq,     TOP_cmp_i_eq_unc,     
                           TOP_cmp_eq,       TOP_cmp_eq_unc
};
TOP
para_comp_top[COMP_TOP_NUM][PARA_COMP_TYPE_NUM] = {
    {  TOP_cmp_ne_or,        TOP_cmp_ne_orcm ,        TOP_cmp_ne_and, 
       TOP_cmp_ne_andcm,     TOP_cmp_ne_or_andcm,     TOP_cmp_ne_and_orcm    }, // cmp_ne
    {  TOP_cmp_ne_or,        TOP_cmp_ne_orcm ,        TOP_cmp_ne_and, 
       TOP_cmp_ne_andcm,     TOP_cmp_ne_or_andcm,     TOP_cmp_ne_and_orcm    }, // cmp_ne_unc
    {  TOP_cmp4_ne_or,       TOP_cmp4_ne_orcm,        TOP_cmp4_ne_and,
       TOP_cmp4_ne_andcm,    TOP_cmp4_ne_or_andcm,    TOP_cmp4_ne_and_orcm   }, // cmp4_ne
    {  TOP_cmp4_ne_or,       TOP_cmp4_ne_orcm,        TOP_cmp4_ne_and,
       TOP_cmp4_ne_andcm,    TOP_cmp4_ne_or_andcm,    TOP_cmp4_ne_and_orcm   }, // cmp4_ne_unc
    {  TOP_cmp_i_ne_or,      TOP_cmp_i_ne_orcm,       TOP_cmp_i_ne_and, 
       TOP_cmp_i_ne_andcm,   TOP_cmp_i_ne_or_andcm,   TOP_cmp_i_ne_and_orcm  }, // cmp_i_ne
    {  TOP_cmp_i_ne_or,      TOP_cmp_i_ne_orcm,       TOP_cmp_i_ne_and, 
       TOP_cmp_i_ne_andcm,   TOP_cmp_i_ne_or_andcm,   TOP_cmp_i_ne_and_orcm  }, // cmp_i_ne_unc
    {  TOP_cmp4_i_ne_or,     TOP_cmp4_i_ne_orcm,      TOP_cmp4_i_ne_and, 
       TOP_cmp4_i_ne_andcm,  TOP_cmp4_i_ne_or_andcm,  TOP_cmp4_i_ne_and_orcm }, //cmp4_i_ne:
    {  TOP_cmp4_i_ne_or,     TOP_cmp4_i_ne_orcm,      TOP_cmp4_i_ne_and, 
       TOP_cmp4_i_ne_andcm,  TOP_cmp4_i_ne_or_andcm,  TOP_cmp4_i_ne_and_orcm }, //cmp4_i_ne_unc
    {  TOP_cmp4_ge_or,       TOP_cmp4_ge_orcm,        TOP_cmp4_ge_and, 
       TOP_cmp4_ge_andcm,    TOP_cmp4_ge_or_andcm,    TOP_cmp4_ge_and_orcm   }, //cmp4_ge
    {  TOP_cmp4_ge_or,       TOP_cmp4_ge_orcm,        TOP_cmp4_ge_and, 
       TOP_cmp4_ge_andcm,    TOP_cmp4_ge_or_andcm,    TOP_cmp4_ge_and_orcm   }, //cmp4_ge_unc  
    {  TOP_cmp4_gt_or,       TOP_cmp4_gt_orcm,        TOP_cmp4_gt_and, 
       TOP_cmp4_gt_andcm,    TOP_cmp4_gt_or_andcm,    TOP_cmp4_gt_and_orcm   }, //cmp4_gt
    {  TOP_cmp4_gt_or,       TOP_cmp4_gt_orcm,        TOP_cmp4_gt_and, 
       TOP_cmp4_gt_andcm,    TOP_cmp4_gt_or_andcm,    TOP_cmp4_gt_and_orcm   }, //cmp4_gt_unc
    {  TOP_cmp4_le_or,       TOP_cmp4_le_orcm,        TOP_cmp4_le_and, 
       TOP_cmp4_le_andcm,    TOP_cmp4_le_or_andcm,    TOP_cmp4_le_and_orcm   }, //cmp4_le
    {  TOP_cmp4_le_or,       TOP_cmp4_le_orcm,        TOP_cmp4_le_and, 
       TOP_cmp4_le_andcm,    TOP_cmp4_le_or_andcm,    TOP_cmp4_le_and_orcm   }, //cmp4_le_unc
    {  TOP_cmp4_lt_or,       TOP_cmp4_lt_orcm,        TOP_cmp4_lt_and, 
       TOP_cmp4_lt_andcm,    TOP_cmp4_lt_or_andcm,    TOP_cmp4_lt_and_orcm   }, //cmp4_lt
    {  TOP_cmp4_lt_or,       TOP_cmp4_lt_orcm,        TOP_cmp4_lt_and, 
       TOP_cmp4_lt_andcm,    TOP_cmp4_lt_or_andcm,    TOP_cmp4_lt_and_orcm   }, //cmp4_lt_unc
    {  TOP_cmp_ge_or,        TOP_cmp_ge_orcm,         TOP_cmp_ge_and, 
       TOP_cmp_ge_andcm,     TOP_cmp_ge_or_andcm,     TOP_cmp_ge_and_orcm    }, //cmp_ge
    {  TOP_cmp_ge_or,        TOP_cmp_ge_orcm,         TOP_cmp_ge_and, 
       TOP_cmp_ge_andcm,     TOP_cmp_ge_or_andcm,     TOP_cmp_ge_and_orcm    }, //cmp_ge_unc
    {  TOP_cmp_gt_or,        TOP_cmp_gt_orcm,         TOP_cmp_gt_and, 
       TOP_cmp_gt_andcm,     TOP_cmp_gt_or_andcm,     TOP_cmp_gt_and_orcm    }, //cmp_gt
    {  TOP_cmp_gt_or,        TOP_cmp_gt_orcm,         TOP_cmp_gt_and, 
       TOP_cmp_gt_andcm,     TOP_cmp_gt_or_andcm,     TOP_cmp_gt_and_orcm    }, //cmp_gt_unc
    {  TOP_cmp_le_or,        TOP_cmp_le_orcm,         TOP_cmp_le_and,
       TOP_cmp_le_andcm,     TOP_cmp_le_or_andcm,     TOP_cmp_le_and_orcm    }, //cmp_le
    {  TOP_cmp_le_or,        TOP_cmp_le_orcm,         TOP_cmp_le_and,
       TOP_cmp_le_andcm,     TOP_cmp_le_or_andcm,     TOP_cmp_le_and_orcm    }, //cmp_le_unc
    {  TOP_cmp_lt_or,        TOP_cmp_lt_orcm,         TOP_cmp_lt_and,
       TOP_cmp_lt_andcm,     TOP_cmp_lt_or_andcm,     TOP_cmp_lt_and_orcm    }, //cmp_lt:
    {  TOP_cmp_lt_or,        TOP_cmp_lt_orcm,         TOP_cmp_lt_and,
       TOP_cmp_lt_andcm,     TOP_cmp_lt_or_andcm,     TOP_cmp_lt_and_orcm    }, //cmp_lt_unc
    {  TOP_cmp4_i_eq_or,     TOP_cmp4_i_eq_orcm,      TOP_cmp4_i_eq_and, 
       TOP_cmp4_i_eq_andcm,  TOP_cmp4_i_eq_or_andcm,  TOP_cmp4_i_eq_and_orcm }, //cmp4_i_eq
    {  TOP_cmp4_i_eq_or,     TOP_cmp4_i_eq_orcm,      TOP_cmp4_i_eq_and, 
       TOP_cmp4_i_eq_andcm,  TOP_cmp4_i_eq_or_andcm,  TOP_cmp4_i_eq_and_orcm }, //cmp4_i_eq_unc
    {  TOP_cmp4_eq_or,       TOP_cmp4_eq_orcm,        TOP_cmp4_eq_and, 
       TOP_cmp4_eq_andcm,    TOP_cmp4_eq_or_andcm,    TOP_cmp4_eq_and_orcm   }, //cmp4_eq
    {  TOP_cmp4_eq_or,       TOP_cmp4_eq_orcm,        TOP_cmp4_eq_and, 
       TOP_cmp4_eq_andcm,    TOP_cmp4_eq_or_andcm,    TOP_cmp4_eq_and_orcm   }, //cmp4_eq_unc
    {  TOP_cmp_i_eq_or,      TOP_cmp_i_eq_orcm,       TOP_cmp_i_eq_and, 
       TOP_cmp_i_eq_andcm,   TOP_cmp_i_eq_or_andcm,   TOP_cmp_i_eq_and_orcm  }, //cmp_i_eq:
    {  TOP_cmp_i_eq_or,      TOP_cmp_i_eq_orcm,       TOP_cmp_i_eq_and, 
       TOP_cmp_i_eq_andcm,   TOP_cmp_i_eq_or_andcm,   TOP_cmp_i_eq_and_orcm  }, //cmp_i_eq_unc:
    {  TOP_cmp_eq_or,        TOP_cmp_eq_orcm,         TOP_cmp_eq_and,
       TOP_cmp_eq_andcm,     TOP_cmp_eq_or_andcm,     TOP_cmp_eq_and_orcm    }, //cmp_eq
    {  TOP_cmp_eq_or,        TOP_cmp_eq_orcm,         TOP_cmp_eq_and,
       TOP_cmp_eq_andcm,     TOP_cmp_eq_or_andcm,     TOP_cmp_eq_and_orcm    }  //cmp_eq_unc
};
BOOL
IF_CONVERTOR::Has_Para_Comp_Top(OP* cmp_op) {

    if (OP_flop(cmp_op)) {
        return false;
    }
    
    TOP cmp_top = OP_code(cmp_op);
    if (OP_opnd(cmp_op, 1) != Zero_TN && OP_opnd(cmp_op, 2) != Zero_TN) 
    {
        switch (cmp_top) 
        {
        case TOP_cmp_ne:
        case TOP_cmp4_ne:
        case TOP_cmp_i_ne:
        case TOP_cmp4_i_ne:
        case TOP_cmp4_i_eq:
        case TOP_cmp4_eq:
        case TOP_cmp_i_eq:
        case TOP_cmp_eq:
        case TOP_cmp_ne_unc:
        case TOP_cmp4_ne_unc:
        case TOP_cmp_i_ne_unc:
        case TOP_cmp4_i_ne_unc:
        case TOP_cmp4_i_eq_unc:
        case TOP_cmp4_eq_unc:
        case TOP_cmp_i_eq_unc:
        case TOP_cmp_eq_unc:
            break;
        default:
            return false;
        }
    }
    return false;
}
TOP
IF_CONVERTOR::Get_Para_Comp_Top(OP* cmp_op, COMPARE_TYPE ctype)
{
    if ( !Has_Para_Comp_Top(cmp_op) )
        return TOP_UNDEFINED;
    
    TOP cmp_top = OP_code(cmp_op);
    INT index = 0;
    while (index < COMP_TOP_NUM) 
    {
        if ( cmp_top == cmp_top_index[index]) 
            break;
        index++;
    }
    if (index >= COMP_TOP_NUM) 
        return TOP_UNDEFINED;
    INT id = (int)ctype;
    Is_True( id>=1 && id <= PARA_COMP_TYPE_NUM, ("no such a ctype!"));

    return para_comp_top[index][id-1];
}


TN*
IF_CONVERTOR::Get_1_Pred_And_Erase(TN_CONTAINER& tn_set)
{
    TN_CONTAINER::iterator iter;
    TN *p1;    iter = tn_set.begin();
    p1 = *iter;
    tn_set.erase(iter);
    return p1;
}

BOOL 
IF_CONVERTOR::Get_2_Pred_And_Erase(TN*& p1, 
                                   TN*& p2, 
                                   COMPARE_TYPE& type, 
                                   BB* bb, 
                                   BB_PREDICATE_INFO  *info) 
{   
    // solve 'and_tns' and 'orcm_tns'
    TN_CONTAINER& tn_set1 = info -> And_TNs();
    TN_CONTAINER& tn_set2 = info -> Orcm_TNs();
    if (tn_set1.size() >=2) 
    {
        p1 = Get_1_Pred_And_Erase(tn_set1);
        p2 = Get_1_Pred_And_Erase(tn_set1);
        type = COMPARE_TYPE_and;
        return true;
    }

    if ( tn_set1.size()) 
    {
        p1 = Get_1_Pred_And_Erase(tn_set1);

        if (tn_set2.size()) 
        {
            p2 = Get_1_Pred_And_Erase(tn_set2);
            type = COMPARE_TYPE_and_orcm;
            return true;
        } else {
            p2 = True_TN;
            type = COMPARE_TYPE_and;
            return true;
        }
    } 
    
    if (tn_set2.size() >=1) 
    {
        p1 = Get_1_Pred_And_Erase(tn_set2);

        if (tn_set2.size()) 
        {
            p2 = Get_1_Pred_And_Erase(tn_set2);
        } else { 
            p2 = True_TN;
        }
        type = COMPARE_TYPE_orcm;
        return true;
    } 

    // solve 'or_tns' and 'andcm_tns'
    TN_CONTAINER& tn_set3 = info -> Or_TNs();
    TN_CONTAINER& tn_set4 = info -> Andcm_TNs();
    if (tn_set3.size() >=2) 
    {
        p1 = Get_1_Pred_And_Erase(tn_set3);
        p2 = Get_1_Pred_And_Erase(tn_set3);
        type = COMPARE_TYPE_or;
        return true;
    }

    if ( tn_set3.size()) 
    {
        p1 = Get_1_Pred_And_Erase(tn_set3);

        if (tn_set4.size()) 
        {
            p2 = Get_1_Pred_And_Erase(tn_set4);
            type = COMPARE_TYPE_or_andcm;
            return true;
        } else {
            p2 = True_TN;
            type = COMPARE_TYPE_or;
            return true;
        }
    } 
    
    if ( tn_set4.size() >=1) 
    {
        p1 = Get_1_Pred_And_Erase(tn_set4);
        if ( tn_set4.size()) 
        {
            p2 = Get_1_Pred_And_Erase(tn_set4);
        } else {
            p2 = True_TN;
        }
        type = COMPARE_TYPE_andcm;
        return true;
    } 
    return false;
}

TN* 
IF_CONVERTOR:: Get_Start_Node_Pred(TN *pred, 
                                   BB *present_solving_bb, 
                                   IF_CONV_AREA *area)
{
    BB *bb_of_pred;
    BB_PREDICATE_INFO  *info = NULL;
    BB_CONTAINER& bbs = area -> Blocks();
    BB_CONTAINER::iterator bb_iter;
    BB *bb;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        info = area -> Pred_Assign_Info(bb);
        if ( pred == info -> Predicate())
        {
            bb_of_pred = bb;
            break;
        }
    }
    Is_True( info, (" Generate useless predicate assignment.\n"));

    TN *result = NULL;
    CNTL_DEP *cd_info = area -> Cntl_Dep_Info();
    BB_SET *cds = cd_info -> Cntl_Dep_Parents(bb_of_pred);
    if (BB_SET_Size(cds) == 1)
    {
        BB* cd = BB_SET_Choose(cds);
        BB* start_node = info -> Start_Node(cd);
        result = area -> Pred_Assign_Info(start_node) -> Predicate();
    } else {
        Is_True(BB_SET_MemberP(cds, present_solving_bb), 
            ("predicate assign holder must in cds"));
        BB* start_node = info -> Start_Node(present_solving_bb);
        result = area -> Pred_Assign_Info(start_node) -> Predicate();

    }
    if ( !result ) 
         result = True_TN;
    return result;
}
void 
IF_CONVERTOR::Gen_Predicate_Assign(TN* result1, 
                                   TN *result2, 
                                   OP* cmp_op,
                                   COMPARE_TYPE ctype,
                                   TOP pred_top,
                                   TN *qual_pred, 
                                   OPS* ops)
{
    if ( pred_top != TOP_UNDEFINED) { 
        Build_OP(pred_top, 
            result1, 
            result2, 
            qual_pred,
            OP_opnd(cmp_op, OP_PREDICATE_OPND+1),
            OP_opnd(cmp_op, OP_PREDICATE_OPND+2), 
            ops);
    } else {
        TOP top;
        switch (ctype) {
        case COMPARE_TYPE_or:
            top = TOP_cmp_eq_or;
            break;
        case COMPARE_TYPE_andcm:
            top = TOP_cmp_eq_andcm;
            break;
        case COMPARE_TYPE_or_andcm:
            top = TOP_cmp_eq_or_andcm;
            break;
        case COMPARE_TYPE_and:
            top = TOP_cmp_ne_and;
            break;
        case COMPARE_TYPE_orcm:
            top = TOP_cmp_ne_orcm;
            break;
        case COMPARE_TYPE_and_orcm:
            top = TOP_cmp_ne_and_orcm;
            break;
        default:
            Is_True(0,("Illegal Compare Type.\n"));
        }
        Build_OP(top, result1, result2, qual_pred, Zero_TN, Zero_TN, 
            ops);
    }
}
TN* 
Predicate_Of_Succ(BB *bb, BB * succ, BB *fall_thru, BB_PREDICATE_INFO *info) {
  TN *pp = NULL;
  if ( BB_succs_len(bb) == 1 ) {
        pp = info -> Predicate();
  } 
  else if (BB_succs_len(bb) == 2) {
    TN *first_pred  = NULL;
    TN *second_pred = NULL;
    Find_BB_Predicates(bb, first_pred, second_pred); 
    Is_True(first_pred && second_pred, 
            (" lack of a predicate!\n"));

    OP *br = BB_branch_op(bb);
    Is_True(br, (" two-successor bb must have br\n"));

    if ((    first_pred == OP_opnd(br, OP_PREDICATE_OPND)
          && succ != fall_thru) 
        || (   second_pred == OP_opnd(br, OP_PREDICATE_OPND) 
          && succ == fall_thru))
    {
      pp = first_pred;
    } 
    else {
      pp = second_pred;
    }
  }
  
  return pp;
}

//*****************************************************************************
// Function : Insert_Predicate
// Input : 
//   - area : the candidate
// Output :
//   < none >
// Description :
//   change the selected candidates to predicated code
//*****************************************************************************

BOOL 
IF_CONVERTOR::Insert_Predicate(IF_CONV_AREA *area) 
{
    //
    // step1: compute the equivalence classes and assign predicates for each 
    //        equivalent class
    // here, we use a BB_SET to represent equivalence class
    // each bb in it represents a class of bbs who have the same control 
    // dependence
    //

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
    {
        fprintf(TFile, "\n Start to assign predicates!\n");
    }

    
    // we record  the found information into BB_PREDICATE_INFO of each
    // bb
    BB_SET *equal_classes = BB_SET_Create_Empty(PU_BB_Count, &_m);
    CNTL_DEP *cd_info = area -> Cntl_Dep_Info();
    BB_CONTAINER& bbs = area -> Blocks();

    BB_CONTAINER::iterator bb_iter;
    BB *bb;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, " -- solving BB%d: ", BB_id(bb));
        }

        if ( BB_recovery(bb))
        {
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
            {
                fprintf(TFile, "    it is a recovery bb.\n");
            }
            continue;
        }

        BB_PREDICATE_INFO *pred_info = area ->Pred_Assign_Info(bb);

        BB_SET *cds_of_bb = cd_info -> Cntl_Dep_Parents(bb);
        BB_SET *te_of_bb = cd_info -> True_Cntl_Dep_Parents(bb);
        //
        // Find out the equivalence class for bb.  If there is no existing 
        // equivalent class, create a new one and insert bb into it.
        //
        BOOL find_ec = false;
        BB *ec;
        FOR_ALL_BB_SET_members(equal_classes, ec)
        {
        
            BB_SET *cds_of_ec = cd_info -> Cntl_Dep_Parents(ec);
            Is_True( cds_of_ec != NULL, 
                ("Can't find cntl deps for BB%d.\n", BB_id(ec)));

            BB_SET *te_of_ec = cd_info -> True_Cntl_Dep_Parents(ec);
            Is_True( te_of_ec != NULL, 
                ("Can't find true edges for BB%d.\n", BB_id(ec)));
        
            if (BB_SET_EqualP(cds_of_bb, cds_of_ec) 
                &&BB_SET_EqualP(te_of_bb, te_of_ec)) 
            {
                BB_PREDICATE_INFO  *pred_of_ec = 
                    area ->Pred_Assign_Info (ec);
                Is_True(pred_of_ec != NULL, 
                    ("Can't find BB_PREDICATE_INFO for BB%d.\n", 
                    BB_id(bb)));

                pred_info -> Predicate(pred_of_ec -> Predicate());
                pred_info -> Eqv_Class_Head_BB(ec);

                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fPrint_TN(TFile, 
                        "     got(share): %s", pred_of_ec -> Predicate());
                }
                find_ec = true;
                break;
            }
        }
        
        if (!find_ec) 
        {
            equal_classes = BB_SET_Union1D(equal_classes, bb, &_m);
            pred_info -> Set_Eqv_Class_Head();

            // set predicate TN for them    
            INT num_deps = BB_SET_Size(cds_of_bb);

            if (num_deps == 1) 
            {
                // if bb has single dependence, set up the TRUE 
                // and FALSE tns for the block.
                TN *true_tn = NULL; 
                TN *false_tn = NULL;
                BB *bb_cd = BB_SET_Choose(cds_of_bb);
                Find_BB_Predicates(bb_cd, true_tn, false_tn);

                // set the predicate for the equivalance class
                if (BB_SET_MemberP(te_of_bb, bb_cd)) 
                {
                    pred_info ->Predicate(true_tn);
                    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                    {
                        fPrint_TN(TFile, "     got: %s\n", true_tn);
                    }
                } else {
                    pred_info ->Predicate(false_tn);
                    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                    {
                        fPrint_TN(TFile, "     got: %s\n", false_tn);
                    }
                }    
            } else if (num_deps > 1) 
            {
                // multiple dependencies. create a new predicate TN for them 
                TN *pred_tn;
                pred_tn = Gen_Predicate_TN();
                pred_info -> Predicate(pred_tn);
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fPrint_TN(TFile, "     got: %s\n", pred_tn);
                }
            }
        }
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            fprintf(TFile, "\n");
        }
    }

    // the following is to collect information of exits, including:
    // 1. how many exits in the area
    // 2. how many branches in the area
    // 3. the information of the targets of all exits
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        BB* fall_thru = BB_Fall_Thru_Successor(bb);

        BBLIST *succ_bl;
        FOR_ALL_BB_SUCCS(bb, succ_bl) 
        {
            BB* succ = BBLIST_item(succ_bl);
            if (!Is_BB_Container_Member(bbs, succ)
                || Regional_Cfg_Node(bb) -> First_Succ() == NULL) 
            {
                // record its information
                EXIT_TARGET_INFO  *tgt = area -> Exit_Target(succ);
                BB_PREDICATE_INFO *info = area -> Pred_Assign_Info(bb);
                TN *pp = Predicate_Of_Succ(bb, succ, fall_thru, info);
                if ( tgt != NULL)
                {
                    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                    {
                        fprintf(TFile, " BB%d target: BB%d ", 
                            BB_id(bb), BB_id(succ));
                        fPrint_TN(TFile, " %s => main  \n", pp);
                    }
                    if ( IPFEC_Combine_Exit ) 
                    {
                        Is_True(tgt -> Main_Predicate().size() == 1, 
                            ("Wrong number of main predicate!\n"));
                        TN *old_main = *(tgt -> Main_Predicate().begin());
                        tgt -> Add_Aux_Predicate(old_main);
                        tgt -> Del_Main_Predicate(old_main);   
                    } 
                    tgt -> Add_Main_Predicate(pp);
                } else {
                    area -> Add_Exit_Target(succ, pp, &_m);
                    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                    {
                        fprintf(TFile, " BB%d target(new): BB%d ", 
                            BB_id(bb), BB_id(succ));
                        fPrint_TN(TFile, " %s => main  \n", pp);
                    }
                }
            } 
        }
    }

    // check whether if-conversion can really reduce the number of 
    // branches. 
    // if exit_num <= inner_br_num , the answer is yes.
    INT exit_num = 0;
    INT inner_br_num = 0;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;

        BOOL all_targets_hidden = TRUE;
        BOOL has_inner_succ = FALSE;
        BB* fall_thru = BB_Fall_Thru_Successor(bb);

        BBLIST *succ_bl;
        FOR_ALL_BB_SUCCS(bb, succ_bl) 
        {
            BB* succ = BBLIST_item(succ_bl);
            if (!Is_BB_Container_Member(bbs, succ)
                || Regional_Cfg_Node(bb) -> First_Succ() == NULL) 
            {
                // record its information
                EXIT_TARGET_INFO  *tgt = area -> Exit_Target(succ);
                BB_PREDICATE_INFO *info = area -> Pred_Assign_Info(bb);

                TN *pp = Predicate_Of_Succ(bb, succ, fall_thru, info);
                Is_True(tgt, ("exit must have exit info.!\n"));
                             
                if ( tgt -> Is_Main_Predicate(pp))
                {
                    all_targets_hidden = FALSE;
                }
            } else {
                has_inner_succ = TRUE;
            }
        }
       
        if (! all_targets_hidden) exit_num ++;
        if ( has_inner_succ && BB_succs_len(bb) > 1 ) 
        {
            inner_br_num++;
        }
    }

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
    {
        fprintf(TFile, "\n Area_%d: exit_num : %d vs. br_num : %d\n", 
            area -> Area_Label(), exit_num, inner_br_num);
    }
    if ( (exit_num > inner_br_num) && !IPFEC_Force_If_Conv) return FALSE;


    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
    {
        fprintf(TFile, "\n Start to detect parallel compare!\n");
    }

    //step2: find candidates for parallel compares
    Detect_Para_Comp(area);

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
    {
        fprintf(TFile, "\n Start to generate parallel compare!\n");
    }

    //step3: generate predicate assignment, including parallel compare
    Gen_Para_Comp(area);
    return TRUE;
}

//*****************************************************************************
// Function : Merge_area
// Input : 
//   - area : the candicate IF_CONV_AREA
// Output :
//   <none>
// Description :
//   The function is to delete unnecessary branches and merge 
//   the one-exit-one-entry block pair into one basic block. 
//*****************************************************************************

void
IF_CONVERTOR::Merge_Area(IF_CONV_AREA *area)
{
    BB_SET *hold_bbs = BB_SET_Create_Empty(PU_BB_Count, &_m);
    BB_SET *area_bb_set = BB_SET_Create_Empty(PU_BB_Count, &_m);
    //
    // Next, we will do the following jobs:
    // 1. delete all unnecessary branches
    // 2. add some necessary goto
    // 3. maintain the profiling information
    // 4. predicate all blocks in the area
    // 5. merge blocks
    //
    

    BB_CONTAINER& bbs = area -> Blocks();
    BB_CONTAINER::iterator bb_iter;
    BB *bb;
    BB *last_bb = NULL;
    
    INT exit_num = 0;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        area_bb_set = BB_SET_Union1D(area_bb_set, bb, &_m);
        BBLIST *succ_bl;
        FOR_ALL_BB_SUCCS(bb, succ_bl) 
        {
            BB* succ = BBLIST_item(succ_bl);
            if (!Is_BB_Container_Member(bbs, succ)) 
            {
                exit_num++;
                break;
            }
        }
        last_bb =bb;
    }
    INT last_bb_type = 0;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile," -- solving BB%d:\n", BB_id(bb));
        }

        if ( BB_recovery(bb)) 
        {
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, " (recovery bb)");
            } 
            continue;
        }
        CNTL_DEP *cntl_info = area -> Cntl_Dep_Info();
        BB_SET *set = cntl_info -> Cntl_Dep_Parents(bb);
        Is_True( cntl_info != NULL, 
            ("Can't find cntl_info for BB%d.\n", BB_id(bb)));

        BB_PREDICATE_INFO *pred_info = area -> Pred_Assign_Info (bb);
        Is_True( pred_info != NULL, 
            ("Can't find BB_PREDICATE_INFO for BB%d.\n", BB_id(bb)));
        TN *pred = pred_info -> Predicate();
        TN *orig_pred = pred_info -> Orig_Pred()?pred_info -> Orig_Pred():pred;

        if (IPFEC_Disable_Merge_BB) 
        {
            Predicate_Block(bb, pred, area_bb_set);
            continue;
        }

        // record the frequency of bb in its corresponding predicate TN
        if (pred) 
        {
            float f = BB_freq(bb);
            hTN_MAPf_Set(frequency_of_predicates, pred, f);
        }

        BB_MERGE_TYPE bb_type = Classify_BB(bb, area);
        
        BBLIST *bl;
        BB *pred_bb = NULL;
        if (bb == area -> Entry_BB()) 
        {
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, "   (entry) ");
            } 
        } else {
            BOOL has_recovery_pred = false;
            BBLIST *pred_bl;
            FOR_ALL_BB_PREDS(bb, pred_bl) 
            {
                if (BB_recovery(BBLIST_item(pred_bl))) 
                {
                    has_recovery_pred = true;
                    break;
                }
            }
            // get out its predeccessor 
            if ( !has_recovery_pred) {
                Is_True(BB_preds_len(bb) == 1, 
                    ("one bb only has one predecessor during merging!\n")); 
                bl = BB_preds(bb);
                pred_bb = BBLIST_item(bl);
            } else {
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fprintf(TFile, "  => it is successor of recovery bb.\n");
                } 
            }
        }

        // when a block is solved, it and its predecessor will become straight
        // line code; but the other successors of its predecessor have 
        // not been solved, so record them in the set 'bb_to_solve'
        BB_SET* bb_to_solve = BB_SET_Create_Empty(PU_BB_Count, &_m);
        BB *succ_bb_of_pred;

        if (pred_bb && BB_succs_len(pred_bb) > 1) 
        {
            
            FOR_ALL_BB_SUCCS(pred_bb, bl) 
            {
                succ_bb_of_pred = BBLIST_item(bl);
                if (succ_bb_of_pred != bb ) 
                {
                    if (Is_BB_Container_Member(bbs, succ_bb_of_pred)) 
                    {
                        bb_to_solve = BB_SET_Union1D(bb_to_solve, 
                            succ_bb_of_pred, &_m);
                    }
                    
                }
            }
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED))
            {
                BB* bb_tmp;
                fprintf(TFile, "  (pred_bb:%d) ", BB_id(pred_bb));
                fprintf(TFile, "   (bb_to_solve:");
                FOR_ALL_BB_SET_members(bb_to_solve, bb_tmp) 
                {
                    fprintf(TFile, " BB%d,  ", BB_id(bb_tmp));
                }
                fprintf(TFile, ") ");
            }
        }
        
        BOOL can_be_merged_into = FALSE;
        BB *fall_thru = BB_Fall_Thru_Successor(bb);
        BB *fall_thru_goto = NULL;
 
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            Print_BB_Merge_Type(bb_type, TFile);
        } 
        switch ( bb_type) {
        case CASE_ALL_IN_AREA:
            {
                // Remove all the branches at the end of the block
                BB_Remove_Branch(bb);
                if(!pred_bb || !BB_SET_MemberP(hold_bbs, pred_bb)) 
                {
                    hold_bbs = BB_SET_Union1D(hold_bbs, bb, &_m);
                }
                can_be_merged_into = TRUE;
                break;
            }
        
        case CASE_CALL_IN:
            {
                break;
            }
        case CASE_CALL_OUT:
            {
                Is_True(BB_succs_len(bb) == 1, 
                            ("here call-inst can only have one succ!\n")); 
                bl = BB_succs(bb);
                BB* exit_target = BBLIST_item(bl);

                EXIT_TARGET_INFO* tgt_info = area -> Exit_Target(exit_target);
                BOOL is_main_exit = tgt_info -> Is_Main_Predicate(orig_pred);
                tgt_info -> Update_Predicate(orig_pred, pred);
                
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    if ( is_main_exit )
                    {
                        fprintf(TFile, 
                            "\n    exit-target(main): BB%d     ", 
                            BB_id(exit_target));
                    } else {
                        fprintf(TFile, 
                            "\n    exit-target(aux): BB%d     ", 
                            BB_id(exit_target));
                    }
                }

                // there are two situations in which a full_thru_goto need to 
                // be added:
                // 1) it is a main exit and there are some BBs to be appended 
		//    after it
                // 2) it is a main exit and there are some auxiliary predicates
                //    need to be assigned there
                if ( is_main_exit && 
                    (BB_SET_Size(bb_to_solve) 
                    || tgt_info -> Aux_Predicates().size())) 
                {
                    // add a new basic block and insert a goto into it 
                    REGIONAL_CFG* cfg = Home_Region(bb) -> Regional_Cfg();
                    fall_thru = exit_target;

                    fall_thru_goto = RGN_Gen_And_Insert_BB(bb, fall_thru, cfg);
                    Add_Goto_Op(fall_thru_goto, fall_thru);
                    

                    // update the prob of bb and its successors
                    Set_Prob(bb, fall_thru_goto, 1.0);
                    BB_freq(fall_thru_goto) = BB_freq(bb);

                    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                    {
                        fprintf(TFile, "       fall-thru-succ: BB%d\n", 
                            BB_id(fall_thru));
                        fprintf(TFile, "       gen fall-thru-goto: BB%d.\n", 
                            BB_id(fall_thru_goto));
                    }

                    Make_Branch_Conditional(fall_thru_goto);   
                    Predicate_Block(fall_thru_goto, pred, area_bb_set);

                    Move_BB(fall_thru_goto, bb);
                    GRA_LIVE_Compute_Liveness_For_BB(fall_thru_goto);

                    if (tgt_info -> Aux_Predicates().size()){
                        OP *br = BB_branch_op(fall_thru_goto);
                        // assign auxiliary predicates
                        tgt_info -> Assign_Aux_Predicates(fall_thru_goto, br);
                    }
                }

                if (!is_main_exit) {
                    RGN_Unlink_Pred_Succ(bb, exit_target);
                }
                break;
            }
        case CASE_UNCOND_BR:
            {
                Is_True(BB_succs_len(bb) == 1, 
                            ("here uncond-br can only have one succ!\n")); 
                bl = BB_succs(bb);
                BB* exit_target = BBLIST_item(bl);

                EXIT_TARGET_INFO* tgt_info = area -> Exit_Target(exit_target);
                BOOL is_main_exit = tgt_info -> Is_Main_Predicate(orig_pred);
                tgt_info -> Update_Predicate(orig_pred, pred);
                
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    if ( is_main_exit )
                    {
                        fprintf(TFile, 
                            "\n    exit-target(main): BB%d     ", 
                            BB_id(exit_target));
                    } else {
                        fprintf(TFile, 
                            "\n    exit-target(aux): BB%d     ", 
                            BB_id(exit_target));
                    }
                }

                if (BB_SET_Size(set)) 
                {
                    Make_Branch_Conditional(bb);
                } 
                if ( is_main_exit) 
                {
                    OP* br = BB_branch_op(bb);
                    Is_True(br, ("uncond-br must has a br-op!\n"));
                    tgt_info -> Assign_Aux_Predicates(bb, br);
                    
                } else {
                    BB_Remove_Branch(bb);
                    if(!pred_bb || !BB_SET_MemberP(hold_bbs, pred_bb)) 
                    {
                        hold_bbs = BB_SET_Union1D(hold_bbs, bb, &_m);
                    } 
                    can_be_merged_into = TRUE;
                    RGN_Unlink_Pred_Succ(bb, exit_target);
                }
                break;
            }
        case CASE_FALL_OUT:
            {
                Is_True(fall_thru, ("fall_thru can not be empty!\n"));

                EXIT_TARGET_INFO* tgt_info = area -> Exit_Target(fall_thru);
                BOOL is_main_exit = tgt_info -> Is_Main_Predicate(orig_pred);
                tgt_info -> Update_Predicate(orig_pred, pred);
                
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    if ( is_main_exit )
                    {
                        fprintf(TFile, 
                            "\n    exit-target(main): BB%d     \n", 
                            BB_id(fall_thru));
                    } else {
                        fprintf(TFile, 
                            "\n    exit-target(aux): BB%d     \n", 
                            BB_id(fall_thru));
                    }
                }
                 
                if ( is_main_exit)  
                {
                    if (tgt_info -> Aux_Predicates().size()) 
                    {
                        Add_Goto_Op(bb, fall_thru);
                        Make_Branch_Conditional(bb);
                        OP* br = BB_branch_op(bb);
                        tgt_info -> Assign_Aux_Predicates(bb, br);
                    
                        // update the prob of bb and its fall-thru-succ
                        float pb1 = 1.0;
                        if (pred_bb) 
                        {
                            BB *other_succ = NULL;
                            BBLIST *bl;
                            FOR_ALL_BB_SUCCS(bb, bl)
                            {  
                                BB *tmp_bb = BBLIST_item(bl);
                                if (tmp_bb != bb) 
                                {   
                                    other_succ = tmp_bb;
                                    break;
                                }
                            }
                            if ( other_succ) 
                                pb1 = 1.0 - Prob_Local(pred_bb,other_succ);
                        }
                        Set_Prob(bb, fall_thru, pb1);
                    } else {
                        if ( bb != last_bb) 
                        {
                            Add_Goto_Op(bb, fall_thru);
                            Make_Branch_Conditional(bb);
                        } 
                    }
                    
                } else {
                    if(!pred_bb || !BB_SET_MemberP(hold_bbs, pred_bb)) 
                    {
                        hold_bbs = BB_SET_Union1D(hold_bbs, bb, &_m);
                    } 
                    can_be_merged_into = TRUE;
                    RGN_Unlink_Pred_Succ(bb, fall_thru);
                }

                break;
            }
        case CASE_IF_FALL_OUT:
            {
                Is_True(fall_thru, ("fall_thru can not be empty!\n"));
 
                TN *first_pred = NULL;
                TN *second_pred = NULL;

                Find_BB_Predicates(bb, first_pred, second_pred); 
                Is_True(first_pred && second_pred, 
                    (" lack of a predicate!\n"));

                EXIT_TARGET_INFO* tgt_info = area -> Exit_Target(fall_thru);
                BOOL is_main_exit;  
                
                OP *br = BB_branch_op(bb);
                Is_True(br, (" two-successor bb must have br\n"));
                if (first_pred == OP_opnd(br, OP_PREDICATE_OPND))
                {
                    is_main_exit = tgt_info -> Is_Main_Predicate(second_pred);
                    
                } else {
                    Is_True(second_pred == OP_opnd(br, OP_PREDICATE_OPND), 
                        ("conditional br must have a predicate.\n"));
                    is_main_exit = tgt_info -> Is_Main_Predicate(first_pred);
                }
                             
                
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    if ( is_main_exit )
                    {
                        fprintf(TFile, 
                            "\n    exit-target(main): BB%d     \n", 
                            BB_id(fall_thru));
                    } else {
                        fprintf(TFile, 
                            "\n    exit-target(aux): BB%d     \n", 
                            BB_id(fall_thru));
                    }
                }
                // Invert the branch to make the fall through the one 
                //in the area
                BB_Remove_Branch(bb);

                if (is_main_exit) 
                {
                    // add a new goto into it 
                    Add_Goto_Op(bb, fall_thru);
                    RGN_Link_Pred_Succ_With_Prob(bb, fall_thru, 1.0);
                    Make_Branch_Conditional(bb);
                    OP* br = BB_branch_op(bb);
                    // update the prob of bb and its fall-thru-succ
                    float pb1 = pred_bb ? Prob_Local(pred_bb, bb):1;
                    float pb2 = Prob_Local(bb, fall_thru);
                    Set_Prob(bb, fall_thru, pb1 * pb2);

                    tgt_info -> Assign_Aux_Predicates(bb, br);
                } else {
                    if(!pred_bb || !BB_SET_MemberP(hold_bbs, pred_bb)) 
                    {
                        hold_bbs = BB_SET_Union1D(hold_bbs, bb, &_m);
                    } 
                    can_be_merged_into = TRUE;
                    BB* other_succ = BB_Other_Successor(bb, fall_thru);
                    RGN_Unlink_Pred_Succ(bb, other_succ);
                    
                }
                break;
            }
        case CASE_IF_FALL_IN:
            {
                Is_True(fall_thru, ("fall_thru can not be empty!\n"));
                
                BB* other_succ = BB_Other_Successor(bb, fall_thru);
                Is_True(other_succ, ("fall_thru can not be empty!\n"));
                EXIT_TARGET_INFO* tgt_info = area -> Exit_Target(other_succ);

                TN *first_pred = NULL;
                TN *second_pred = NULL;

                Find_BB_Predicates(bb, first_pred, second_pred); 
                Is_True(first_pred && second_pred, 
                    (" lack of a predicate!\n"));

                BOOL is_main_exit;  
                OP *br = BB_branch_op(bb);
                Is_True(br, (" two-successor bb must have br\n"));
                if (first_pred == OP_opnd(br, OP_PREDICATE_OPND))
                {
                    is_main_exit = tgt_info -> Is_Main_Predicate(first_pred);
                } else {
                    Is_True(second_pred == OP_opnd(br, OP_PREDICATE_OPND), 
                        ("conditional br must have a predicate.\n"));
                    is_main_exit = tgt_info -> Is_Main_Predicate(second_pred);
                }
                
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fprintf(TFile, "\n    exit-target");
                    if ( is_main_exit )
                    {
                        fprintf(TFile, "(main)");
                    } else {
                        fprintf(TFile, "(aux)");
                    }
                    fprintf(TFile,": BB%d     ", BB_id(other_succ));
                }
                if (is_main_exit) 
                {
                    OP* br = BB_branch_op(bb);
                    tgt_info -> Assign_Aux_Predicates(bb, br);

                } else {
                    BB_Remove_Branch(bb);
                    if(!pred_bb || !BB_SET_MemberP(hold_bbs, pred_bb)) 
                    {
                        hold_bbs = BB_SET_Union1D(hold_bbs, bb, &_m);
                    } 
                    can_be_merged_into = TRUE;
                    RGN_Unlink_Pred_Succ(bb, other_succ);
                    // Set_Prob(bb, fall_thru, 1.0);
                }
                break;
            }
        case CASE_IF_OUT:
            {
                Is_True(fall_thru, ("fall_thru can not be empty!\n"));

                TN *first_pred = NULL;
                TN *second_pred = NULL;

                Find_BB_Predicates(bb, first_pred, second_pred); 
                Is_True(first_pred && second_pred, 
                    (" lack of a predicate!\n"));

                BOOL is_main_exit;  
                OP *br = BB_branch_op(bb);
                Is_True(br, (" two-successor bb must have br\n"));
                

                // solving target-successor(non-fall-thru successor)
                TN *fall_thru_pred = NULL;
                BB* other_succ = BB_Other_Successor(bb, fall_thru);
                EXIT_TARGET_INFO* tgt_info = area -> Exit_Target(other_succ);
                if (first_pred == OP_opnd(br, OP_PREDICATE_OPND))
                {
                    is_main_exit = tgt_info -> Is_Main_Predicate(first_pred);
                    fall_thru_pred = second_pred;
                    
                } else {
                    Is_True(second_pred == OP_opnd(br, OP_PREDICATE_OPND), 
                        ("conditional br must have a predicate.\n"));
                    is_main_exit = tgt_info -> Is_Main_Predicate(second_pred);
                    fall_thru_pred = first_pred;
                }

                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fprintf(TFile, "\n    exit-target");
                    if ( is_main_exit )
                    {
                        fprintf(TFile, "(main)");
                    } else {
                        fprintf(TFile, "(aux)");
                    }
                    fprintf(TFile,": BB%d     ", BB_id(other_succ));
                }

                BOOL need_add_fall_thru = TRUE;
                if ( is_main_exit )
                {
                    OP* br = BB_branch_op(bb);
                    tgt_info -> Assign_Aux_Predicates(bb, br);
                } else {
                    BB_Remove_Branch(bb);
                    RGN_Unlink_Pred_Succ(bb, other_succ);
                    need_add_fall_thru = FALSE;
                }

                // solving fall through successor
                tgt_info = area -> Exit_Target(fall_thru);
                if (first_pred == OP_opnd(br, OP_PREDICATE_OPND))
                {
                    is_main_exit = tgt_info -> Is_Main_Predicate(second_pred);
                } else {
                    Is_True(second_pred == OP_opnd(br, OP_PREDICATE_OPND), 
                        ("conditional br must have a predicate.\n"));
                    is_main_exit = tgt_info -> Is_Main_Predicate(first_pred);
                }
                if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                {
                    fprintf(TFile, "\n    exit-target");
                    if ( is_main_exit )
                    {
                        fprintf(TFile, "(main)");
                    } else {
                        fprintf(TFile, "(aux)");
                    }
                    fprintf(TFile,": BB%d     ", BB_id(fall_thru));
                }
                // Need to insert a fall-through goto, then conditionalize 
                // it with the inverse conditional
                br = NULL;
                if ( is_main_exit) 
                {
                    if ( need_add_fall_thru && 
                        (tgt_info -> Aux_Predicates().size() || bb != last_bb)) 
                    {
                        // add a new basic block and insert a goto into it 
                        REGIONAL_CFG* cfg = BB_SET_Size(bb_to_solve) ? 
                            Home_Region(bb) -> Regional_Cfg(): NULL;
                        fall_thru_goto = RGN_Gen_And_Insert_BB(bb, fall_thru, cfg);
                        Add_Goto_Op(fall_thru_goto, fall_thru);
                        if (fall_thru_pred && fall_thru_pred != True_TN) 
                        {
                            Make_Branch_Conditional(fall_thru_goto);
                           Predicate_Block(fall_thru_goto, 
                                fall_thru_pred, area_bb_set);
                        }    

                        // assign auxiliary predicates
                        br = BB_branch_op(fall_thru_goto);
                        tgt_info -> Assign_Aux_Predicates(fall_thru_goto, br);

                        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
                        {
                            fprintf(TFile, "       fall-thru-succ: BB%d\n", 
                                BB_id(fall_thru));
                            fprintf(TFile, "       gen fall-thru-goto: BB%d.\n", 
                                BB_id(fall_thru_goto));
                        }
                        
                        Move_BB(fall_thru_goto, bb);
                        GRA_LIVE_Compute_Liveness_For_BB(fall_thru_goto);
                        float pb1 = Prob_Local(pred_bb, bb);
                        float pb2 = Prob_Local(bb, other_succ);
                    
                        Set_Prob(bb, other_succ, pb1 * pb2);
                        Set_Prob(bb, fall_thru_goto, 1 - pb1 * pb2);

                        BB_freq(fall_thru_goto) = 
                            BB_freq(pred_bb) * (1 -pb1 * pb2);
                        
                        
                    } else if (bb != last_bb || 
                        tgt_info -> Aux_Predicates().size())
                    {
                        Add_Goto_Op(bb, fall_thru);
                        Make_Branch_Conditional(bb);
                        br = BB_branch_op(bb);
                        if (fall_thru_pred && fall_thru_pred != True_TN) 
                        {
                            Set_OP_opnd(br, OP_PREDICATE_OPND, fall_thru_pred);
                        }
                        
                        // assign auxiliary predicates
                        tgt_info -> Assign_Aux_Predicates(bb, br);
                    }
                } else {
                    RGN_Unlink_Pred_Succ(bb, fall_thru);
                }
                
                if (!need_add_fall_thru && !br)
                {
                    if(!pred_bb || !BB_SET_MemberP(hold_bbs, pred_bb)) 
                    {
                        hold_bbs = BB_SET_Union1D(hold_bbs, bb, &_m);
                    } 
                    can_be_merged_into = TRUE;
                }
                break;
            }

            
        case CASE_CHECK_IN:
        case CASE_CHECK_OUT:
            {
                break;
            }
        default:
            Is_True(0,("Unknown block classification"));
        }

        // predicate the current basic block
        Predicate_Block(bb, pred, area_bb_set);

        // merge it into its predecessor
        // judge if it can be merged into its predecessor
        
        if (pred_bb && BB_SET_MemberP(hold_bbs, pred_bb)) 
        {
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, "    => merged.\n");
            }

            if (pred) 
            {
                if (!pred_info -> Eqv_Class_Head() 
                    && pred_info -> Eqv_Class_Head_BB() != pred_bb) 
                {
                    Set_TN_is_global_reg(pred);
                    Set_BB_has_globals(bb);
                    Set_BB_has_globals(pred_info -> Eqv_Class_Head_BB());
                } else if (pred_info -> Has_Start_Node()) 
                {
                    BB* st_n = pred_info -> Start_Node(BB_SET_Choose(set));
                    if (st_n != pred_bb) 
                    {
                        Set_TN_is_global_reg(pred);
                        Set_BB_has_globals(bb);
                        Set_BB_has_globals(st_n);
                    }

                }  
            }
            BB *succ_of_goto = NULL;
            if (fall_thru_goto) 
            {
                Is_True(BB_succs_len(fall_thru_goto) == 1, 
                    ("here fall-thru-goto can only have one succ!\n")); 
                bl = BB_succs(fall_thru_goto);
                succ_of_goto = BBLIST_item(bl);
            }
            FOR_ALL_BB_SET_members(bb_to_solve, succ_bb_of_pred)
            {
                if (fall_thru_goto) 
                {
                    // add succ_bb_of_pred as the fall-thru-successor of 
                    // fall_thru_goto
                    float pb = Prob_Local(pred_bb, succ_bb_of_pred);
                    RGN_Unlink_Pred_Succ(pred_bb, succ_bb_of_pred);
                    RGN_Link_Pred_Succ_With_Prob(fall_thru_goto, 
                        succ_bb_of_pred, pb);
                    Set_Fall_Thru(fall_thru_goto, succ_bb_of_pred);

                    Set_Prob(fall_thru_goto, succ_of_goto, 1 - pb);

                } else {
                    // add succ_bb_of_pred as a successor of bb
                    float pb = Prob_Local(pred_bb, succ_bb_of_pred);
                    RGN_Unlink_Pred_Succ(pred_bb, succ_bb_of_pred);
                    RGN_Link_Pred_Succ_With_Prob(bb, succ_bb_of_pred, pb);
                    
                }
            }

            if ( BB_call(bb) )
            {
                BB_Transfer_Callinfo( bb, pred_bb);
            }
            Merge_Blocks(pred_bb, bb);
            if (!can_be_merged_into) 
            {
                BB_SET_Difference1D(hold_bbs, pred_bb);
            }
        }else if (pred_bb){

            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, "    => appended .\n");
            }
            
            // maintain localization information of registers
            if (pred && 
                (!pred_info -> Eqv_Class_Head()|| pred_info -> Has_Start_Node())) 
            {
                Set_TN_is_global_reg(pred);
                Set_BB_has_globals(bb);
                if (!pred_info -> Eqv_Class_Head())
                    Set_BB_has_globals(pred_info -> Eqv_Class_Head_BB());
            }

            if ( BB_SET_Size(bb_to_solve) )
            {
                BB *succ_of_goto = NULL;
                if (fall_thru_goto) 
                {
                    Is_True(BB_succs_len(fall_thru_goto) == 1, 
                        ("here fall-thru-goto can only have one succ!\n")); 
                    bl = BB_succs(fall_thru_goto);
                    succ_of_goto = BBLIST_item(bl);
                }
                FOR_ALL_BB_SET_members(bb_to_solve, succ_bb_of_pred)
                {
                    if (fall_thru_goto) 
                    {
                        // add succ_bb_of_pred as the fall-thru-successor of 
                        // fall_thru_goto
                        float pb = Prob_Local(pred_bb, succ_bb_of_pred);
                        RGN_Unlink_Pred_Succ(pred_bb, succ_bb_of_pred);
                        RGN_Link_Pred_Succ_With_Prob(fall_thru_goto, 
                            succ_bb_of_pred, pb);
                        Set_Fall_Thru(fall_thru_goto, succ_bb_of_pred);

                        Set_Prob(fall_thru_goto, succ_of_goto, 1 - pb);

                    } else {
                        // add succ_bb_of_pred as a successor of bb
                        float pb = Prob_Local(pred_bb, succ_bb_of_pred);
                        RGN_Unlink_Pred_Succ(pred_bb, succ_bb_of_pred);
                        RGN_Link_Pred_Succ_With_Prob(bb, succ_bb_of_pred, pb);        
                    }
                }
                
                 
            } 
            if (last_bb_type == CASE_CALL_OUT) 
            {
                Set_Fall_Thru(pred_bb, bb);
            }
            float out_pb = 0.0;
            // update fall thru successor
            if (pred_bb && BB_succs_len(pred_bb) > 1) 
            {
                Is_True(BB_succs_len(pred_bb) == 2, 
                            ("a bb can not have more than 2 succs!\n")); 
                // for a bb, one succ is out of the area, the other succ(bb) 
                // should be fall-thru-succ
                FOR_ALL_BB_SUCCS(pred_bb, bl) 
                {
                    succ_bb_of_pred = BBLIST_item(bl);
                    if (succ_bb_of_pred != bb 
                        && !Is_BB_Container_Member(bbs, succ_bb_of_pred)) 
                    {
                        out_pb = Prob_Local(pred_bb, succ_bb_of_pred);
                        Set_Fall_Thru(pred_bb, bb);
                    }
                    
                }
            }


            // maintain frequency information
            BB_freq(bb) = BB_freq(pred_bb) * ( 1- out_pb);
            Set_Prob(pred_bb, bb, 1.0 - out_pb);
        } else {
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, "\n");
            }
        }
        last_bb_type = bb_type;
    }
    // get rid of useless 'br'. Sometime, after if-conversion, a bb with
    // a conditional 'br' will only has one successor. In this case, the
    // 'br' should be deleted.
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb = *bb_iter;
        OP *br = BB_branch_op(bb);
        if ( !br ) continue;
        if ( OP_cond(br) && BB_succs_len(bb) == 1 )
        {
            BB *fall_thru = BB_Fall_Thru_Successor(bb);
            if ( fall_thru && fall_thru == BB_next(bb) && OP_bb(br) )
            {
                BB_Remove_Op(bb, br);
            } else if (!fall_thru ) {
                //OSP_265
                //Here means the branch will always be taken.
                //Instead of using True_TN as predicate, we just revert br.cond back to br.
                //CGTARG_Predicate_OP(bb, br, True_TN);
                TOP new_top;            
                switch (OP_code(br)) {
                case TOP_br_cond:
                  new_top = TOP_br;
                  break;
                case TOP_br_r_cond:
                  new_top = TOP_br_r;
                  break;
                default:
                  FmtAssert(0, ("Weird branch instruction!"));
                  break;
                }
                OP* new_br = Mk_OP(new_top,
              		 Gen_Enum_TN(ECV_ph_few),
              		 Gen_Enum_TN(ECV_dh),
              		 OP_opnd(br,4));
                OP_srcpos(new_br) = OP_srcpos(br);
                BB_Insert_Op_After(bb, br, new_br);
                BB_Remove_Op(bb, br);
            }
        }
    }
    
}
BB_MERGE_TYPE
IF_CONVERTOR::Classify_BB(BB *bb,
                          IF_CONV_AREA *area) 
{
    BB_MERGE_TYPE result;
    BB *entry = area -> Entry_BB();
    
    BB *fall_thru;
    BB *other_succ;
    fall_thru = BB_Fall_Thru_Successor(bb);
    other_succ = NULL;

    BBLIST *bl;
    FOR_ALL_BB_SUCCS(bb, bl) 
    {
        other_succ = BBLIST_item(bl);
        if (other_succ != fall_thru) break;
    }

    // check for easy case, to see if all succs are in the area
    BOOL fall_thru_out = false;
    BOOL other_out = false;
    
    BB_CONTAINER& bbs_1 = area -> Blocks();
    if (fall_thru && 
        (!Is_BB_Container_Member(bbs_1, fall_thru) ||fall_thru == entry)) 
    {
        fall_thru_out = TRUE;
    }
    
    if (other_succ && 
        (!Is_BB_Container_Member(bbs_1, other_succ) || other_succ == entry)) 
    {
        other_out = TRUE;
    }

    if (!other_out && !fall_thru_out) 
    {
        if (BB_exit(bb) || BB_call(bb)) 
        {
            result = CASE_CALL_IN;
        } else if (BB_chk_op(bb)) 
        {
            result = CASE_CHECK_IN;
        } else {
            result = CASE_ALL_IN_AREA;
        }
        return result;
    }

    // In the case, at least one block is out of the area.
    if (BB_exit(bb) || BB_call(bb)) 
    {
        result = CASE_CALL_OUT;
        return result;
    }
    if (BB_chk_op(bb)) 
    {
        result = CASE_CHECK_OUT;
        return result;
    }
    
    OP *br = BB_branch_op(bb);
    if (!br) 
    {
        // No branch, just fall_thru. 
        result = CASE_FALL_OUT;
        return result;
    }
    
    if (!OP_cond(br)) 
    {
        // Unconditional branch
        result = CASE_UNCOND_BR;
        return result;
    }
    
    if (fall_thru_out && other_out) 
    {
        result = CASE_IF_OUT;
    } else if (fall_thru_out && !other_out) {
        result = CASE_IF_FALL_OUT;
    } else { 
        result = CASE_IF_FALL_IN;
    }
    return result;
}
void 
IF_CONVERTOR::Set_Fall_Thru(BB* bb, BB* fall_thru)
{
    BB* before_bb = bb;
    while (fall_thru) {
        BB* next = BB_next(fall_thru);

        // if the target of the branch of 'fall_thru' is the fall_thru_bb
        //  of 'fall_thru', stop iteration.
        BOOL stop_iter = FALSE;
        OP *br_op = BB_branch_op(fall_thru);
        if (next && br_op) {
            INT tfirst, tcount;
            CGTARG_Branch_Info(br_op, &tfirst, &tcount);
            if (tcount != 0) {
                TN *dest = OP_opnd(br_op, tfirst);
                Is_True(tcount == 1,  
                    ("%d branch targets, expected 1", tcount));
                Is_True(TN_is_label(dest), ("expected label"));
                if (Is_Label_For_BB(TN_label(dest), next)) {
                    stop_iter = TRUE;
                }
            }
        }

        if (stop_iter || next != BB_Fall_Thru_Successor(fall_thru))
        {
            next = NULL;
        }
        Move_BB(fall_thru, before_bb);
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            fprintf(TFile, "\n => move BB:%d after BB:%d\n", 
                BB_id(fall_thru), BB_id(before_bb));
        }    
        before_bb = fall_thru;
        fall_thru = next;
    }
}
void
IF_CONVERTOR::Merge_Blocks(BB *bb_first, 
                           BB *bb_second)
{
    // step 0: maintain fall-thrugh-information 
    BOOL need_maintain = TRUE;
    OP *br_op = BB_branch_op(bb_second);
    BB* next = BB_next(bb_second);
    if (next && br_op) {
        
        INT tfirst, tcount;
        CGTARG_Branch_Info(br_op, &tfirst, &tcount);
        if (tcount != 0) {
            TN *dest = OP_opnd(br_op, tfirst);
            Is_True(tcount == 1,  ("%d branch targets, expected 1", tcount));
            Is_True(TN_is_label(dest), ("expected label"));
            if (Is_Label_For_BB(TN_label(dest), next)) {
                need_maintain = FALSE;
            }
        }
    }

    if ( need_maintain && bb_second != BB_next(bb_first))
    {
        BB* fall_thru = BB_Fall_Thru_Successor(bb_second);
        Set_Fall_Thru(bb_first, fall_thru);
    }

    // step1: Move all ops of bb_second to bb_first.
    OP *op;
    OP *op_next;
    OP *last = BB_last_op(bb_first);
    for (op = BB_first_op(bb_second); 
    op; 
    op = op_next) 
    {
        op_next = OP_next(op);
        if (last) 
        {
            BB_Move_Op(bb_first, last, bb_second, op, FALSE);
        } else {
            BB_Append_Op (bb_first, op);
        }
        last = op;
    }
    
    // step2: Clean up the first pred-succ list
    RGN_Unlink_Pred_Succ(bb_first, bb_second);
    
    // step3:  move bb_second's successor arcs to bb_first.
    BB *succ;
    BBLIST *bl;    
    while (bl = BB_succs(bb_second))
    {
        succ = BBLIST_item(bl);
        float pb = Prob_Local(bb_second, succ);
        RGN_Unlink_Pred_Succ(bb_second, succ);
        RGN_Link_Pred_Succ_With_Prob(bb_first, succ, pb);
    }

    //step4:  take bb_second out of the list.
    RGN_Remove_BB_And_Edges(bb_second);  
    
    // step5:  maintain the call and exit infomation
    // If bb_second is an exit, move the relevant info to the
    // merged block.
    if (BB_exit(bb_second)) 
    {
        BB_Transfer_Exitinfo(bb_second, bb_first);
        Exit_BB_Head = BB_LIST_Delete(bb_second, Exit_BB_Head);
        Exit_BB_Head = BB_LIST_Push(bb_first, Exit_BB_Head, &MEM_pu_pool);
    }
    
    // Transfer call info if merged block will now contain a call.
    if (BB_call(bb_second)) 
    {
        BB_Copy_Annotations(bb_first, bb_second, ANNOT_CALLINFO);
    }
    
}

//*****************************************************************************
// Function : Convert_Candidates
// Input : 
//   - areas : a list of IF_CONV_AREAs. On them, there are obvious flags to 
//             indicate if the IF_CONV_AREA should be if-converted.
// Output :
//   <none>
// Description :
//   apply if-conversion on the selected candidates to predicated code.
// 
//*****************************************************************************

BOOL
IF_CONVERTOR::Convert_Candidates(AREA_CONTAINER&  areas)
{    
    AREA_CONTAINER:: iterator iter;
    BOOL converted = FALSE;
    for ( iter = areas.begin(); 
    iter != areas.end(); 
    iter++) 
    {
        IF_CONV_AREA* area = *iter;
        BB_CONTAINER& bbs = area -> Blocks();

        if (IPFEC_Query_Skiplist(if_conv_skip_area, area -> Area_Label())) 
            continue;
        if ( area -> Conv_Type() != FULLY_IF_CONV)
            continue;

        converted = TRUE;
        char if_conversion_key[6] = "";
        char input_string[100] = "";
        char output_string[60] = "";
        if (Get_Trace(TP_PTRACE1, TP_PTRACE1_CG))
        {
             BB_CONTAINER::iterator bb_iter;
             BB *bb;
             sprintf(if_conversion_key, "%d", area -> Area_Label());
             for (bb_iter = bbs.begin(); 
             bb_iter != bbs.end(); 
             bb_iter++)
             {
                 bb = *bb_iter;
                 char tmp_string[6];
                 sprintf(tmp_string, "%d", BB_id(bb));
                 strcat(input_string, tmp_string);
                 strcat(input_string, "||");
             }
        }
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, "\n IF_CONV_AREA %d:\n", area -> Area_Label());
        }
        // caculate control dependence and initialize the predicate info 
        area -> Init_Conversion_Info(&_m);

        if ( !Insert_Predicate(area)) {
            if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
            {
                fprintf(TFile, "\n too many exits, give up!!\n");
            }
            continue;
        }
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, "\n Start to merge basic blocks!\n");
        }

        Merge_Area(area); 

        if (Get_Trace(TP_PTRACE1, TP_PTRACE1_CG))
        {
             BB_CONTAINER& bbs = area -> Blocks();
             BB_CONTAINER::iterator bb_iter;
             BB *bb;
             sprintf(if_conversion_key, "%d", area -> Area_Label());
             for (bb_iter = bbs.begin(); 
             bb_iter != bbs.end(); 
             bb_iter++)
             {
                 bb = *bb_iter;

                 if (Regional_Cfg_Node(bb)) 
                 {
                     char tmp_string[6];
                     sprintf(tmp_string, "%d", BB_id(bb));
                     strcat(output_string, tmp_string);
                     strcat(output_string, "||");
                 }
             }
             Generate_Tlog("AIC", "if_conversion", (SRCPOS)0,
                if_conversion_key, input_string, output_string, "");
        }
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            fprintf(TFile, "\n after merging basic blocks:\n");
            area -> Print_IR(TFile);
        }
    }
    return converted;
}

//*****************************************************************************
// Function : If_Conversion
// Input : 
//   - region_tree : a region tree
// Output :
//   < none >
// Description :
//   the driver of if-conversion phase
//*****************************************************************************

IF_CONVERTOR::IF_CONVERTOR(REGION_TREE *region_tree)
{ 

    Start_Timer(T_Ipfec_If_Conv_CU);

    //trace before IF_CONV
    if (Get_Trace(TKIND_IR, TP_A_IFCONV, REGION_First_BB))
      Trace_IR(TP_A_IFCONV, "IF_CONV", NULL, FALSE);

    if (!frequency_of_predicates)
    {
        frequency_of_predicates = hTN_MAPf_Create(&(info_mem._m));
    }

     if (!init_op_info)
     {
         init_op_info = hTN_MAP_Create(&(info_mem._m));
     }

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_GRAPHIC))
    { 
        draw_global_cfg();
    }

    BOOL changed = TRUE;
    for (INNERMOST_REGION_FIRST_ITER iter(region_tree);
         iter != 0; 
         ++iter)
    {
        REGION* region = *iter;

        if (IPFEC_Query_Skiplist(if_conv_skip_rgn, region -> Id(), Current_PU_Count())) 
            continue;

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_GRAPHIC))
        { 
            draw_regional_cfg(region);
        }
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY))
        { 
            fprintf(TFile, " \nIF CONVERSION -- region %d\n", region -> Id());
        }

        if ( Is_In_Infinite_Loop(region) ) 
            continue;

        if (Is_In_Abnormal_Loop(region))
            continue;

        if (region->Region_Type () == IMPROPER) 
            continue;

        // Calculate_Dominators is time comsumption, only recalculate
        // when the cfg changed
        if( changed )
            Calculate_Dominators();
  
        // do something to initialize 
        IF_CONV_ALLOC temp_alloc(&_m);
        AREA_CONTAINER  areas(temp_alloc); 

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, " \n Start to initialize!\n");
        }

        If_Conversion_Init(region,areas);
        
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            fprintf(TFile, " IF_CONV_AREAs after initialization:\n\n");
            Print_All_Areas(areas, TFile);
        }
        
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, " \n Start to select proper areas!\n");
        }

        // select the if-conversion candidates
        Select_Candidates(areas);

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            fprintf(TFile, " IF_CONV_AREAs after selection:\n\n");
            Print_All_Areas(areas, TFile);
        }

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
        {
            fprintf(TFile, " \n Start to convert candidates!\n");
        }
        //convert the if-conversion candidates to predicated code
        changed = Convert_Candidates(areas);
        
        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_GRAPHIC)) 
        {
            draw_regional_cfg(region);
        }

        CXX_DELETE(&areas, &_m);
        if( changed )
            Free_Dominators_Memory();
    }
    if(!changed)
        Free_Dominators_Memory();

    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_GRAPHIC))
    { 
        draw_global_cfg();
    }
    
    //trace after IF_CONV
    if (Get_Trace(TKIND_IR, TP_A_IFCONV, REGION_First_BB))
      Trace_IR(TP_A_IFCONV, "IF_CONV", NULL);

    Stop_Timer(T_Ipfec_If_Conv_CU);
}
IF_CONVERTOR::IF_CONVERTOR()
{
     if (!frequency_of_predicates)
     {
         frequency_of_predicates = hTN_MAPf_Create(&(info_mem._m));
     }
     if (!init_op_info)
     {
         init_op_info = hTN_MAP_Create(&(info_mem._m));
     }

     _loop_length = 0;
}
//*****************************************************************************
// Function : Force_If_Conversion
// Input : 
//   - loop : a descriptor of a loop. here, we assume the loop is a SEME and 
//            innermost loop.
//   - allow_muti_bb : a boolean value to indicate if multiple bb in a loop is
//                     allowed
// Output :
//   - a basic block : If allow_multi_bb, it will always be converted if 
//     possible. If not possible, return NULL.
// Description :
//   in swp phase, a loop without control flow in it is suitable to apply 
//   swp. It is a driver to if-convert a loop body
//*****************************************************************************

BB *
IF_CONVERTOR::Force_If_Convert(LOOP_DESCR *loop, BOOL allow_multi_bb)
{
    BB     *bb_entry = LOOP_DESCR_loophead(loop);

    // possible quick exit if there is already only one block in the region
    if (BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1) {
        return bb_entry;
    }
    
    // check if it is safe to if-convert the loop
    IF_CONV_ALLOC temp_alloc(&_m);
    AREA_CONTAINER  areas(temp_alloc); 

    REGION* region = Home_Region(bb_entry);
    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_GRAPHIC)) 
    {
        draw_regional_cfg(region);
    }
    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
    {
        fprintf(TFile, " ======start to force_if_convert\n\n");
    }

    if ( Is_In_Infinite_Loop(region) ) 
    {
        return NULL;
    }

    If_Conversion_Init(region,areas);
    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
    {
        fprintf(TFile, " IF_CONV_AREAs after initialization:\n\n");
        Print_All_Areas(areas, TFile);
    }

    // check if it can be merged into one basic block
    BOOL is_legal = Can_Merge_Into_One_Area(areas);
    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) 
    {
        fprintf(TFile, " is_legal: %d\n", (int)is_legal);
    }
    if (!is_legal)
    {    
        return NULL;
    }
 
    // When relaxed if conversion is used, we don't blindly merge all areas to 
    // one singel area and apply if conversion. Instead we use a similar approch 
    // as the normal if conversion does, but the heuristics to calculate the 
    // profitability is more relaxed because we might gain additional performance
    // through SWP.
    if (IPFEC_Relaxed_If_Conv) {
      if (Get_Trace (TP_A_IFCONV, TT_IF_CONV_SUMMARY)) {
        fprintf(TFile, " ======use relaxed_if_conv\n\n");
      }
      
      // Calculate a rough estimate of the critical path of the loop 
      INT32 loop_length1, loop_length2;
      loop_length1 = Calculate_Loop_Critical_Length (areas[0]);
      loop_length2 = Calculate_Loop_Cycled_Critical_Length (areas[0]);
    
      _loop_length = loop_length1 > loop_length2 ? loop_length1 : loop_length2;

      if (Get_Trace (TP_A_IFCONV, TT_IF_CONV_DETAILED)) {
        fprintf (TFile, 
                 " Loop Length: %i\n",
                 _loop_length);
      }
    
      Select_Candidates (areas, /*forced=*/TRUE);
   
      if (Get_Trace (TP_A_IFCONV, TT_IF_CONV_DETAILED)) {
        fprintf (TFile, " IF_CONV_AREAs after selection:\n\n");
        Print_All_Areas (areas, TFile);
      }

      // Check if the loop has been reduced to one area.
      // If not we will not If Convert the loop
      if (areas.size()!=1) {
        return NULL;
      }
    }
    else {
      // merge all areas
      AREA_CONTAINER::iterator area_iter;
      area_iter = areas.begin();
      IF_CONV_AREA *head_area = *area_iter;
      area_iter++;
                         
      for (; area_iter != areas.end(); area_iter++) {
        head_area -> Combine_Blocks_With (*area_iter, &_m);
      }
      // We can now delete all areas except the first one,
      // because all the other areas have been merged into
      // the first area
      areas.erase (areas.begin()+1,areas.end());
      head_area -> Conv_Type(FULLY_IF_CONV);                                                     
    }

    BOOL can_one_bb = Can_Merge_Into_One_BB (*(areas.begin()));
    if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_SUMMARY)) {
        fprintf (TFile, " can_one_bb: %d\n", (int) can_one_bb);
    } 

    // Do the if-conversion
    if (can_one_bb || allow_multi_bb) {
      if (Get_Trace (TP_A_IFCONV, TT_IF_CONV_SUMMARY)) {
        fprintf (TFile, " \n Start to convert candidates!\n");
      }
      Convert_Candidates(areas);
      
      // Update the loop descriptor
      BB* fall_thru_bb = NULL;
      BB* bb;
      IF_CONV_AREA* area = *(areas.begin());
      BB_CONTAINER del_blks(&_m);
      BB_CONTAINER::iterator bb_iter;
      for (bb_iter  = area -> Blocks().begin();
           bb_iter != area -> Blocks().end();
           bb_iter++)
      {
        bb = *bb_iter;
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* home_region = node ? node -> Home_Region() : NULL;
        // home_region == NULL means the bb has been deleted
        if (!home_region) {
          // get rid of all the old blocks from the loop descriptors
          // Due to some bugs, when BB set in LOOP_DESCR is changed,
          // the "delete" operations should be performed after "add"
          // is done.
          // LOOP_DESCR_Delete_BB(loop, bb);
          del_blks.push_back (bb);
        } 
        else if (home_region != region) {
          FmtAssert (fall_thru_bb == NULL, 
                     (" can only have one fall_thru_bb\n"));
          fall_thru_bb = bb;
          // Add the fall_thru_block to the loop descriptors 
          // for all the enclosing loops
          LOOP_DESCR *enc_loop = LOOP_DESCR_Next_Enclosing_Loop(loop);
          if (enc_loop) {
            LOOP_DESCR_Add_BB (enc_loop, fall_thru_bb);
          }
        }
      }
      
      for (BB_CONTAINER::iterator iter = del_blks.begin();
	         iter != del_blks.end (); 
           iter++) 
      {
        LOOP_DESCR_Delete_BB (loop, *iter);
	    }

      BB* single_bb = NULL;
      if (can_one_bb) {
        FmtAssert (areas.size() ==1, (" loop should be shrinked to one bb"));
        IF_CONV_AREA* area = *(areas.begin());
        if (BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1) {
          single_bb = area -> Entry_BB();
        }
      }
      
      if (Get_Trace (TP_A_IFCONV, TT_IF_CONV_GRAPHIC)) {
        draw_regional_cfg(region);
      }
      
      return single_bb;
    }
    
    return NULL;
}


BOOL
IF_CONVERTOR::Can_Merge_Into_One_Area(AREA_CONTAINER& areas)
{
    INT break_num = 0;
    AREA_CONTAINER::iterator iter;
    for ( iter = areas.begin(); 
    iter != areas.end(); 
    iter++)
    {
        IF_CONV_AREA* area = *iter;
        if (area ->  Area_Type() == UNSUITABLE)
        {
            return false;
        }
        
        if (area ->  Area_Type() == UPWARD_UNSUITABLE
            && area -> Pred_Num())
        {
            return false;
        }
        
        if (area ->  Area_Type() == DOWNWARD_UNSUITABLE
            && area -> Succ_Num())
        {
            
            return false;
        }
    }
    return true;
}
BOOL
IF_CONVERTOR::Can_Merge_Into_One_BB(IF_CONV_AREA* area)
{
    BB_CONTAINER& bbs = area -> Blocks();
    BB_CONTAINER::iterator bb_iter;
    BB *bb;
    BOOL break_num = FALSE;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        bb =*bb_iter;
        BB_MERGE_TYPE merge_type = Classify_BB(bb, area);

        if (Get_Trace(TP_A_IFCONV, TT_IF_CONV_DETAILED)) 
        {
            fprintf(TFile, "  => BB_%d is",BB_id(bb)); 
            Print_BB_Merge_Type(merge_type,TFile);
            fprintf(TFile,"type.\n"); 
        }
        switch ( merge_type) {
        case CASE_ALL_IN_AREA:
            {
                break;
            }
        case CASE_CALL_IN:
        case CASE_IF_FALL_OUT:
        case CASE_IF_FALL_IN:
        case CASE_CHECK_IN:
            {
                return false;
            }
        case CASE_CALL_OUT:
        case CASE_UNCOND_BR:
        case CASE_FALL_OUT:
        case CASE_IF_OUT:
        case CASE_CHECK_OUT:
            {
                if (!break_num) {
                    break_num= TRUE;
                    break;
                } else 
                    return false;
            }
        default:
            Is_True(0,("Unknown block classification"));
        }
    }
    return true;
}

//=============================================================================
// Print functions
//=============================================================================
void
IF_CONVERTOR::Print_BB_Merge_Type(BB_MERGE_TYPE merge_type,FILE* file)
{ 
    switch ( merge_type) {
    case CASE_ALL_IN_AREA:
        {
            fprintf(file, "CASE_ALL_IN_AREA"); 
            break;
        }
    case CASE_CALL_IN:
        {
            fprintf(file, "CASE_CALL_IN"); 
            break;            
        }
    case CASE_CALL_OUT:
       {
           fprintf(file, "CASE_CALL_OUT"); 
           break;
       }
    case CASE_UNCOND_BR:
        {
            fprintf(file, "CASE_UNCOND_BR"); 
            break;
        }
    case CASE_FALL_OUT:
        {
            fprintf(file, "CASE_FALL_OUT"); 
            break;
        }
    case CASE_IF_FALL_OUT:
        {
            fprintf(file, "CASE_IF_FALL_OUT");
            break; 
        }
    case CASE_IF_FALL_IN:
        {
            fprintf(file, "CASE_IF_FALL_IN");
            break; 
        }
    case CASE_IF_OUT:
        {
            fprintf(file, "CASE_IF_OUT"); 
            break;
        }
    case CASE_CHECK_IN:
        {
            fprintf(file, "CASE_CHECK_IN"); 
            break;
        }
    case CASE_CHECK_OUT:
        {
            fprintf(file, "CASE_CHECK_OUT");
            break; 
        }
    default:
        {
            Is_True(0,("Unknown block classification"));
        }
    }
}
void
IF_CONVERTOR::Print_All_Areas(AREA_CONTAINER& areas, FILE* file)
{
    AREA_CONTAINER::iterator iter;
    for (iter = areas.begin(); 
    iter != areas.end(); 
    iter++) 
    {
        IF_CONV_AREA* area = *iter;
        area -> Print(TFile);
    }
}


// This function calculates a rough estimate of the loop critical path
INT32
IF_CONVERTOR::Calculate_Loop_Critical_Length (IF_CONV_AREA* area) {
  INT32 critical_length = 0;
  INT32 length1, length2;
  AREA_CONTAINER::iterator area_succs;
  
  FmtAssert(area!=NULL, ("Parameter area is NULL pointer!"));

  critical_length += area->Critical_Len ();

  area_succs = area->Succ_Set().begin();

  switch (area->Succ_Num ()) {
    case 0: break;

    case 1: critical_length += 
              Calculate_Loop_Critical_Length (*area_succs);
            break;

    case 2: length1 = 
              Calculate_Loop_Critical_Length (*area_succs);
            area_succs++;
            length2 = 
              Calculate_Loop_Critical_Length (*area_succs);

            if (length1 > length2 ) {
              critical_length += length1;
            }
            else {
              critical_length += length2;
            }
            break;

    default: FmtAssert (0, ("Unexpected number of successors!"));
             break;
    
  }

  return critical_length;
}


// This function calculates a rough estimate of the loop critical path under
// the assumption that one instruction takes one cycle
INT32
IF_CONVERTOR::Calculate_Loop_Cycled_Critical_Length (IF_CONV_AREA* area) {
  INT32 cycled_critical_length = 0;
  INT32 length1, length2;
  AREA_CONTAINER::iterator area_succs;
  
  FmtAssert(area!=NULL, ("Parameter area is NULL pointer!"));

  cycled_critical_length += area->Cycled_Critical_Len ();

  area_succs = area->Succ_Set().begin();

  switch (area->Succ_Num ()) {
    case 0: break;

    case 1: cycled_critical_length += 
              Calculate_Loop_Cycled_Critical_Length (*area_succs);
            break;

    case 2: length1 = 
              Calculate_Loop_Cycled_Critical_Length (*area_succs);
            area_succs++;
            length2 = 
              Calculate_Loop_Cycled_Critical_Length (*area_succs);

            if (length1 > length2 ) {
              cycled_critical_length += length1;
            }
            else {
              cycled_critical_length += length2;
            }
            break;

    default: FmtAssert (0, ("Unexpected number of successors!"));
             break;
    
  }

  return cycled_critical_length;
}


void     
IF_CONV_AREA::Print(FILE* file)
{
    fprintf(file, 
        "\nIF_CONV_AREA_%d: (", 
        Area_Label()); 
    fprintf(file, " suitable type : ");
    switch (_if_suitable) {
    case UPWARD_UNSUITABLE:
        {
            fprintf(file, " UPWARD_UNSUITABLE; ");
            break;
        }
    case DOWNWARD_UNSUITABLE:
        {
            fprintf(file, " DOWNWARD_UNSUITABLE; ");
            break;
        }
     case UNSUITABLE:
        {
            fprintf(file, " UNSUITABLE; ");
            break;
        }
     case SUITABLE_FOR_IF_CONV:
        {
            fprintf(file, " SUITABLE_FOR_IF_CONV; ");
            break;
        }
    default:
        {
            Is_True(0, (" Illegal AREA_TYPE.\n"));
        }
    }

    fprintf(file, " if-conv type : ");
    switch (_need_if_convert ) {
    case NO_IF_CONV:
        {
            fprintf(file, " NO_IF_CONV;");
            break;
        }
    case PARTIAL_IF_CONV:
        {
            fprintf(file, " PARTIAL_IF_CONV;");
            break;
        }
    case FULLY_IF_CONV:
        {
            fprintf(file, " FULLY_IF_CONV;");
            break;
        }
    default:
        {
            Is_True(0, (" Illegal IF_CONV_TYPE.\n"));
        }
    }

    fprintf(file, " length(cycled): %d; ",_cycled_critical_length);
    fprintf(file, " length : %d ", _critical_length);

    fprintf(file,")\n -- included blocks : ");
    BB_CONTAINER& bbs = _included_blocks;
    BB_CONTAINER::iterator bb_iter;
    BB *block;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        block = *bb_iter;
        fprintf(file,"BB%d,  ", block -> id);
    }    
    fprintf(file,"\n");
    
    AREA_CONTAINER::iterator iter;
    fprintf(file, " -- predecessors : ");
    for ( iter = _predecessors.begin(); 
    iter!=_predecessors.end(); 
    iter++)
    {
        IF_CONV_AREA* pred = *iter;
        fprintf(file,"AREA_%d,  ", pred->Area_Label());
    }

    fprintf(file, "\n -- successors : ");
    for ( iter = _successors.begin(); 
    iter!= _successors.end(); 
    iter++)
    {
        IF_CONV_AREA* succ = *iter;
        fprintf(file,"AREA_%d,  ", succ->Area_Label());
    }
    fprintf(file, "\n");

    if (_control_deps) 
    {
        Is_True(_pred_assign_info, (" _pred_true_edges must be initialized!"));

        _control_deps -> Print(file);

        fprintf(file, " -- more detailed information for each bb : \n");
        BB_CONTAINER::iterator bb_iter;
        for (bb_iter = bbs.begin(); 
        bb_iter != bbs.end(); 
        bb_iter++)
        {
            block = *bb_iter;
            BB_SET  *set; 
            BB  *bb;
            BB_PREDICATE_INFO  *info;

            fprintf(file, " \n**  BB%d : ", BB_id(block));
            info = (BB_PREDICATE_INFO*)BB_MAP_Get( _pred_assign_info, block);
            fPrint_TN(file, 
                " predicate: %s ; ", 
                info -> Predicate());
            fprintf(file, 
                " transitional:  %c ;", 
                info -> Transitional()?'y':'n');
            fPrint_TN(file, 
                " true pred: %s ;", 
                info -> True_TN());
            fPrint_TN(file, 
                " false pred: %s\n", 
                info -> False_TN());
            

            TN_CONTAINER::iterator iter;
            TN_CONTAINER  tn_set;

            fprintf(file,"     # or preds:");
            tn_set = info ->Or_TNs();
            for (iter = tn_set.begin(); 
            iter != tn_set.end(); 
            iter++)
            {
                fPrint_TN(file, "  %s,", *iter);
            }

            fprintf(file,"     # orcm preds:");
            tn_set = info -> Orcm_TNs();
            for (iter = tn_set.begin(); 
            iter != tn_set.end(); 
            iter++)
            {
                fPrint_TN(file, "  %s,", *iter);
            }

            fprintf(file,"     # and preds:\n");
            tn_set = info -> And_TNs();
            for (iter = tn_set.begin(); 
            iter != tn_set.end(); 
            iter++)
            {
                fPrint_TN(file, "  %s,", *iter);
            }
            
            fprintf(file,"     # andcm preds:");
            tn_set = info -> Andcm_TNs();
            for (iter = tn_set.begin(); 
            iter != tn_set.end(); 
            iter++)
            {
                fPrint_TN(file, "  %s,", *iter);
            }
            fprintf(file, "\n");

            if ( !info -> Has_Start_Node()) continue;
            fprintf(file, 
                "     # starting nodes for each control dependentor:\n");
            BB_SET* cds = _control_deps -> Cntl_Dep_Parents(block);
            BB* cd;
            FOR_ALL_BB_SET_members(cds, cd)
            {
                BB* sn = info -> Start_Node(cd);
                if (sn) 
                {
                    fprintf(file, "     BB%d ==> BB%d ; ", 
                        BB_id(cd), BB_id(sn));
                }
            }

            fprintf(file, "\n");
        }
    }
}

void     
IF_CONV_AREA::Print_IR(FILE *file)
{
    fprintf(file, 
        "\nIF_CONV_AREA_%d:========================\n", 
        Area_Label());   

    BB_CONTAINER& bbs = _included_blocks;
    BB_CONTAINER::iterator bb_iter;
    BB *block;
    for (bb_iter = bbs.begin(); 
    bb_iter != bbs.end(); 
    bb_iter++)
    {
        block = *bb_iter;
        Print_BB (block);
    }    
    fprintf(file,"\n");
}
void     
CNTL_DEP::Print(FILE* file)
{
    BB *block;

    fprintf(file, "\n -- control dependence information \n");
    FOR_ALL_BB_SET_members(_bb_set, block)
    {
        BB_SET *set; 
        BB *bb;

        fprintf(file, " ** BB_%d:  cds : ", block -> id);
        set = (BB_SET*)BB_MAP_Get(_control_dependences, block);
        FOR_ALL_BB_SET_members(set, bb) 
        {
            fprintf(file, " BB%d,  ", BB_id(bb));
        }

        fprintf(file, "  true edges : ");
        set = (BB_SET*)BB_MAP_Get(_true_edges, block);
        FOR_ALL_BB_SET_members(set, bb) 
        {
            fprintf(file, " BB%d,  ", BB_id(bb));
        }
        fprintf(file, "\n");
    }
}

