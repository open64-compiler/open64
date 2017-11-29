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

//-*-C++-*-

//=============================================================================
//=============================================================================
//
//  Module :  recovery.h
//  $Date  :  2001/02/20 21:12:34 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/recovery.cxx,v $
//
//  Description:
//  ============
//
//  Implementation of .
//
//=============================================================================
//=============================================================================

#include <list>
#include <map>
#include <set>
#include <utility>
#include <vector>
#include <unistd.h>

#include "region.h"
#include "region_bb_util.h"
#include "speculation.h"
#include "recovery.h"
#include "label_util.h"
#include "gra_live.h"
#include "op.h"
#include "vt_region.h"
#include "defs.h"
#include "cg_flags.h"
#include "tracing.h"
#include "ipfec_defs.h"
#include "scheduler.h"
#include "cgtarget.h"
#include "ipfec_options.h"
#include "whirl2ops.h"

typedef mempool_allocator<TN*> TN_ALLOC; 
typedef mempool_allocator<OP*> OP_ALLOC;    
std::vector<OP*, OP_ALLOC>  chk_vector;

//=============================================================================
//Function:  Match_Chk_Ld
//Input:
//    - OP* chk_op:
//    - OP* load_op:
//Output:
//    - BOOL:
//Description:
//    - Decide whether the chk_op is checking the load_op.
//    - ld.s vs chk.s
//    - ld.a and ld.sa vs chk.a
//=============================================================================
inline BOOL 
Match_Chk_Ld(OP *chk_op, OP *load_op)
{
    Is_True(OP_chk(chk_op),("Input a non-chk OP!"));
    Is_True(CGTARG_Is_OP_Speculative(load_op), ("Input a non-speculative load!"));
    Is_True((OP_float_chk(chk_op) ?  OP_Is_Float_Mem(load_op) : !OP_Is_Float_Mem(load_op)),("Doesn't match, no matter float/integer!"));
        
    if(!TNs_Are_Equivalent(Get_chk_reg(chk_op), OP_result(load_op,0))) 
        return FALSE;

    if( OP_chk_s(chk_op) && CGTARG_Is_OP_Speculative(load_op) && !CGTARG_Is_OP_Advanced_Load(load_op) )
        return TRUE;
    
    if( OP_chk_a(chk_op) && CGTARG_Is_OP_Advanced_Load(load_op) )
        return TRUE;

    return FALSE;
}

//=============================================================================
//Function:  Get_Recovery_BB
//Input:
//    - OP* chk 
//Output:
//    - BB* A recovery block.
//Description:
//    - Get chk's corresponding recovery block.
//=============================================================================
inline BB*
Get_Recovery_BB(OP* chk)
{
    Is_True(OP_chk(chk),("Input a non-chk OP!"));

    TN *target_tn;
    target_tn = Get_chk_tgt(chk);   

    Is_True(TN_is_label(target_tn),("Not a label!"));

    LABEL_IDX lab;
    lab = TN_label(target_tn);

    return Get_Label_BB(lab);
}


//=============================================================================
//Function:  Collect_Results
//Input:
//    - set<TN*,compare_tn>& result_set: We will put op's results in this TN set.
//    - OP* op: 
//Output:
//    - No explicit output.   
//Description:
//    - Put every result TN of op in the result_set.
//    - Assumption: every result of op should be a GFP register, otherwise, a 
//                  complile time fault will be signaled.
//    - GFP register: General purpose register
//                    Floating point register
//                    Predicate register   
//============================================================================= 
inline void
Collect_Results(std::set<TN*, compare_tn, TN_ALLOC>& result_set, OP* op)
{
    for(INT i = 0; i < OP_results(op); i++){
        TN* result = OP_result(op, i);
        Is_True(TN_is_GFP(result), ("Result TN is not a GFP register!"));
        if(!TN_is_const_reg(result))
            result_set.insert(result);
    }
}

//=============================================================================
//Function:  Divide_BB
//Input:
//    - BB* bb:     The BB that will be divided
//    - OP* point:  At which position the bb will be divided.
//Output:
//    - BB*  return bb2, as shown below. 
//Description:
//    - Divide one basic block into two, the boundary is indicated by point.
//
//      bb:  __________         bb1:  _________
//          |          |             |         |
//          |          |             |         |
//          |          |             |         |
//   point: |__________|     ----->  |_________|
//          |          |
//          |          |        bb2:  _________
//          |          |             |         |
//          |__________|             |         |
//                                   |         |
//                                   |_________|
//  
//    - Note: there is no control flow edge between bb1 and bb2. 
//=============================================================================
BB* Divide_BB(BB *bb, OP *point)
{
    Is_True(OP_bb(point) == bb,("op is not in bb!"));
    
    if(point == BB_last_op(bb))   
        return NULL;

    BB* bottom_bb = Gen_And_Insert_BB_After(bb);
    if (BB_exit(bb)) {
        BB_Transfer_Exitinfo(bb, bottom_bb);
        Exit_BB_Head = BB_LIST_Delete(bb, Exit_BB_Head);
        Exit_BB_Head = BB_LIST_Push(bottom_bb, Exit_BB_Head, &MEM_pu_pool);
    }
    if (BB_call(bb)) {
        BB_Transfer_Callinfo(bb, bottom_bb);
    }
    if (BB_asm(bb)) {
        BB_Transfer_Asminfo (bb, bottom_bb);
    }

    BBLIST* next;
    BBLIST* succ;
    for(succ = BB_succs(bb); succ; succ = next) {
        BB* bb_succ = BBLIST_item(succ);
        next = BBLIST_next(succ);
        Link_Pred_Succ_with_Prob(bottom_bb, bb_succ, BBLIST_prob(succ));
        Unlink_Pred_Succ(bb, bb_succ);
    }
    for(OP *op = OP_next(point); op; ) {
        OP *tmp = op;
        op = OP_next(op);
        BB_Move_Op_To_End(bottom_bb, bb, tmp);
    }
    return bottom_bb;
}


//=============================================================================
//Function:   Find_Path_in_Region 
//Input:
//    - REGIONAL_CFG_NODE* from:  The start regional cfg node.
//    - REGIONAL_CFG_NODE* to:    The target regional cfg node. 
//    - vector<REGIONAL_CFG_NODE *>& path:    The execution path from "from" to "to".
//    - vector<REGIONAL_CFG_NODE *>& visited: Record those already visited node.
//Output:
//    - BOOL: True means we have successfully found a path from "from" to "to".
//            And the path is recorded in a STL_BB_LIST: path. FALSE means there is 
//            no execution path from "from" to "to".
//Description:
//    - Find an execution path from the cfg node "from" to "to". 
//============================================================================= 
static BOOL
Find_Path_in_Region(REGIONAL_CFG_NODE* from, 
                    REGIONAL_CFG_NODE* to, 
                    std::vector<REGIONAL_CFG_NODE *>& path, 
                    std::vector<REGIONAL_CFG_NODE *>& visited)
{
    Is_True(to != NULL,("to node is NULL."));

    if(from == NULL)  
        return FALSE;
    for(std::vector<REGIONAL_CFG_NODE *>::iterator iter = visited.begin(); iter != visited.end(); iter++){
        if(from == *iter) 
            return FALSE;
    }
    visited.push_back(from);
    if (from == to){
        path.push_back(from);
        return TRUE;
    }
    for (CFG_SUCC_NODE_ITER succ_iter(from); succ_iter != 0; ++succ_iter) {
        REGIONAL_CFG_NODE* cfg_node = *succ_iter;
        if (Find_Path_in_Region(cfg_node, to, path, visited)){
            path.push_back(from);
            return TRUE;
        }
    }
    return FALSE;
}


//=============================================================================
//  Function:  Find_Execution_Path
//  Input:
//    - OP *from: The start op.
//    - OP *to: The end op. 
//  Output:
//    - list<OP *>: The execution path from "from" to "to", arrayed into a list.
//  Description:
//    - Find an execution path from the op "from" to the op "to". 
//      This requires op "to" is reachable from the op "from".
//    - Find a bb path from the bb where the op "from" reside 
//      to another bb where the op "to" reside.
//    - Push all ops on the bb path into a op list. 
//============================================================================= 
 
static std::list<OP *>
Find_Execution_Path(OP *from, OP *to)
{
    Is_True(CGTARG_Is_OP_Speculative(from), ("op is not a speculative ld!"));
    Is_True(OP_chk(to), ("op is not a chk!"));
    Is_True(Home_Region(OP_bb(from)) == Home_Region(OP_bb(to)),("not in the same region!"));
        
    REGIONAL_CFG_NODE* from_node = Regional_Cfg_Node(OP_bb(from));
    REGIONAL_CFG_NODE* to_node   = Regional_Cfg_Node(OP_bb(to));
    std::vector<REGIONAL_CFG_NODE *>  regional_cfg_path;
    std::vector<REGIONAL_CFG_NODE *>  visited_node;

    if( from_node == to_node ){
        regional_cfg_path.push_back(from_node);
    }else{
        if(!Find_Path_in_Region(from_node, to_node, regional_cfg_path, visited_node))
            FmtAssert(FALSE,("Can not find a regional cfg path!"));
    }
    
    std::list<OP *>  OP_path;
    for(std::vector<REGIONAL_CFG_NODE *>::reverse_iterator iter = regional_cfg_path.rbegin(); iter != regional_cfg_path.rend(); iter++){
        if ((*iter)->Is_Region()) continue;
        BB* bb = (*iter)->BB_Node();
        if(BB_length(bb) == 0)   continue;
        OP *start_op = bb == OP_bb(from) ? from : BB_first_op(bb);
        OP *end_op = bb == OP_bb(to) ? to : BB_last_op(bb);

        Is_True(start_op != NULL,("start_op can not be NULL!"));
        Is_True(end_op != NULL,("end_op can not be NULL!"));

        for(OP *op = start_op; op != OP_next(end_op); op = OP_next(op)){
            OP_path.push_back(op);
        }
    }
    return OP_path;
}


static inline BOOL
In_OP_Vector(std::vector<OP*, OP_ALLOC>& opv, OP* op)
{
    for(std::vector<OP*, OP_ALLOC>::iterator iter = opv.begin(); iter != opv.end(); iter++){
        if(op == *iter){
            return TRUE;
        }
    }
    return FALSE;
}


//=============================================================================
//Function:  Do_Build_Recovery_Block
//Input:
//    - OP_LIST *path:  The execution path from ld.(s/a/sa) to chk.(s/a).
//    - OP *chk: The current chk instruction.
//    - OP_LIST *spec_chain: The speculative chain that we will construct.
//    - OP_LIST *cas_lds: The cascaded lds that we will find out on the speculative chain. 
//Output:
//    - no explicit output.
//Description:
//    - Identify speculative chain from the execution path from 
//      ld.(s/a/sa) to chk.(s/a). Find every cascaded ld on the
//      chain and collect them into an op list.
//============================================================================= 

static BB *
Do_Build_Recovery_Block(std::list<OP*>& Exec_Path)
{
         
    OP* spec_ld = Exec_Path.front();
    OP* chk = Exec_Path.back();
        
    TN* spec_ld_ptn = OP_opnd(spec_ld,0);
    TN* chk_ptn = OP_opnd(chk,0);

    Is_True(CGTARG_Is_OP_Speculative(spec_ld),("op is not a speculative load!"));
    Is_True(OP_chk(chk),("op is not a chk op!"));
    Is_True(Match_Chk_Ld(chk,spec_ld),("chk and spec_ld do not match!"));

    Exec_Path.pop_front();
    Exec_Path.pop_back();

    std::vector<OP*, OP_ALLOC>  candidate_ops;
    std::vector<OP*, OP_ALLOC>  cascaded_ops;
    std::vector<OP*, OP_ALLOC>  cascaded_loads;
  
    std::set<TN*, compare_tn, TN_ALLOC> speculative_chain_def;
    std::set<TN*, compare_tn, TN_ALLOC> cascaded_chain_def;

    Collect_Results(speculative_chain_def, spec_ld);
    
    //==== Identify Flow Dependence Instructions On Speculative Chain ====//
    
    for(std::list<OP*, OP_ALLOC>::iterator iter = Exec_Path.begin(); iter != Exec_Path.end(); iter++){
        OP* op = *iter;
        BOOL on_cascaded_chain = FALSE;
        BOOL on_speculative_chain = FALSE;
        BOOL depend_by_predicate = FALSE;
        for(INT i = 0; i < OP_opnds(op); i++){
            TN* opnd = OP_opnd(op,i);
            if(!TN_is_register(opnd) || TN_is_const_reg(opnd)){
                continue;
            }
            if(cascaded_chain_def.find(opnd) != cascaded_chain_def.end()){
                on_cascaded_chain = TRUE;
            }
            if(speculative_chain_def.find(opnd) != speculative_chain_def.end()){
                on_speculative_chain = TRUE;
                if(i == 0 && OP_has_predicate(op)){ 
                    depend_by_predicate = TRUE;
                }
            }
        }
        if(on_cascaded_chain){
            // OSP, if op is speculative load, invalidate it
            if( CGTARG_Is_OP_Speculative_Load(op) ) {
                cascaded_loads.push_back(op);
            }

            cascaded_ops.push_back(op);
            Collect_Results(cascaded_chain_def,op);
            continue;
        }
        if(on_speculative_chain){
            TN* cur_ptn = OP_opnd(op,0);
            if(OP_baneful(op)){
                // Here we accomodate baneful op on speculative chain because we can not prevent
                // such kind of cases from occurring currently:
                // Suppose: P1 and P2 are disjoint.
                //           (P1)st  [ ]=r36
                //                ...
                //           (P2)ld  r36=[ ]
                // When building reg arcs in dag construction phase, prdb is used to analyze
                // the relationship between P1 and P2. Thus, we know P1 and P2 are disjoint.
                // So, no arc will be build between them. However, prdb is not used when
                // building mem arcs. So there is a memin(dotted) arc from to st to ld.
                // When global scheduler goes through this code, it will schedule ld across
                // st and deem it as a data speculation. So the code will be transformed to:
                //            (P2)ld  r36=[ ]
                //                 ...
                //            (P1)st  [ ]=r36
                //                 ...
                //                chk  r36 ...
                //  Hence, we will find a baneful op(the st) on speculative chain from data flow. 
                Is_True(    cur_ptn != spec_ld_ptn 
                         && cur_ptn != chk_ptn
                         && cur_ptn != True_TN,("find a baneful op on speculative chain!"));
            }else if(depend_by_predicate){
                candidate_ops.push_back(op);
                Collect_Results(speculative_chain_def,op);
            }else{
                if(    cur_ptn == spec_ld_ptn 
                    || cur_ptn == chk_ptn
                    || cur_ptn == True_TN){
                    if(OP_load(op) && IPFEC_Enable_Cascade){ 
                        Is_True(CGTARG_Is_OP_Speculative_Load(op),("cascaded load is not a speculative load!"));
                        cascaded_loads.push_back(op);
                        cascaded_ops.push_back(op);
                        Collect_Results(cascaded_chain_def,op);
                    }else{
                        candidate_ops.push_back(op);
                        Collect_Results(speculative_chain_def,op);
                    }
                }
            }
        }        
    }

    if(CGTARG_Is_OP_Speculative_Load(spec_ld)){
        for(std::set<TN*>::iterator iter = speculative_chain_def.begin(); iter != speculative_chain_def.end(); iter++){
            TN* tn = *iter;
            Set_TN_is_take_nat(tn);
        }
    }
   
    //==== Identify Output Dependence Instructions On Speculative Chain ====//

    for(std::list<OP*, OP_ALLOC>::iterator iter = Exec_Path.begin(); iter != Exec_Path.end(); iter++){
        OP* op = *iter;
        BOOL is_candidate = FALSE;
        if(In_OP_Vector(candidate_ops,op)){
            continue;
        }
        if(In_OP_Vector(cascaded_ops,op)){
            continue;
        }
        for(INT i = 0; i < OP_results(op); i++) {
            TN *rslt = OP_result(op, i);
            if(speculative_chain_def.find(rslt) != speculative_chain_def.end()){
                Is_True(!OP_baneful(op),("Find a baneful op in identify output dependence stage!"));    
                candidate_ops.push_back(op);
                Collect_Results(speculative_chain_def, op);                   
                break;
            }
        }
    }
    
    //========= Build Recovery Block=========//
            
    if( candidate_ops.empty() && cascaded_ops.empty() && CGTARG_Is_OP_Advanced_Load(spec_ld) ){
        OP* check_ld = Dup_OP(spec_ld);
        TN* pr_tn = OP_opnd(chk, 0);
        TN* ldtype_tn = OP_Is_Float_Mem(spec_ld) ? Gen_Enum_TN(ECV_fldtype_c_nc) : Gen_Enum_TN(ECV_ldtype_c_nc);
        Set_OP_opnd(check_ld, 0, pr_tn);
        Set_OP_opnd(check_ld, enum_ldtype_pos, ldtype_tn);
        Set_OP_cond_def_kind(check_ld, OP_ALWAYS_COND_DEF); 
        BB_Insert_Op_Before(OP_bb(chk), chk, check_ld);
        BB_Remove_Op(OP_bb(chk), chk);
        Copy_WN_For_Memory_OP(check_ld,spec_ld);
        Reset_BB_scheduled(OP_bb(check_ld));
        return NULL;
    }
    
    BB* last_bb = OP_bb(chk);
    while(BB_next(last_bb) != NULL)  
        last_bb = BB_next(last_bb);
    BB* recovery_bb = Gen_And_Insert_BB_After(last_bb);

    OP* recovery_op = Dup_OP ( spec_ld );
    TN* ldtype_tn = OP_Is_Float_Mem(recovery_op) ? Gen_Enum_TN(ECV_fldtype) : Gen_Enum_TN(ECV_ldtype);    
    Set_OP_opnd ( recovery_op, enum_ldtype_pos, ldtype_tn );
    Reset_OP_speculative (recovery_op);
    BB_Append_Op (recovery_bb, recovery_op);
 
    for(std::list<OP*>::iterator exec_path_iter = Exec_Path.begin(); exec_path_iter != Exec_Path.end(); exec_path_iter++){
        OP* op = *exec_path_iter;
        for(std::vector<OP*, OP_ALLOC>::iterator cand_iter = candidate_ops.begin(); cand_iter != candidate_ops.end(); cand_iter++){
            if(op == *cand_iter){
                for(INT i=0; i < OP_results(op); i++){
                    TN *result_tn = OP_result(op,i);
                    if(TN_Is_Allocatable(result_tn))
                         GTN_UNIVERSE_Add_TN(result_tn);
                }
                for(INT i=0; i < OP_opnds(op); i++){
                    TN *opnd_tn = OP_opnd(op,i);
                    if(TN_Is_Allocatable(opnd_tn))
                         GTN_UNIVERSE_Add_TN(opnd_tn);
                }
                recovery_op = Dup_OP(op);
                BB_Append_Op(recovery_bb, recovery_op);
            }
        }

        // OSP, make the cascade_loads Global TN: They are invalidated in this BB and reloaded in another recovery BB.
        for(std::vector<OP*, OP_ALLOC>::iterator casc_iter = cascaded_ops.begin(); casc_iter != cascaded_ops.end(); casc_iter++){
            if(op == *casc_iter){
                 for(INT i=0; i < OP_results(op); i++){
                     TN *result_tn = OP_result(op,i);
                     if(TN_Is_Allocatable(result_tn))
                         GTN_UNIVERSE_Add_TN(result_tn);
                 }
                 for(INT i=0; i < OP_opnds(op); i++){
                     TN *opnd_tn = OP_opnd(op,i);
                     if(TN_Is_Allocatable(opnd_tn))
                         GTN_UNIVERSE_Add_TN(opnd_tn);
                 }
             }
        }
    }

    if(!IPFEC_Enable_Cascade)
        return recovery_bb;

    if( CGTARG_Is_OP_Speculative(spec_ld) && !CGTARG_Is_OP_Advanced_Load(spec_ld))  // spec_ld == ld.s
        return  recovery_bb;

    // spec_ld == ld.a or ld.sa
    for(std::vector<OP *, OP_ALLOC>::iterator iter = cascaded_loads.begin(); iter != cascaded_loads.end(); iter++){        
        OP *op = *iter;               
        if( CGTARG_Is_OP_Speculative_Load(op) &&  CGTARG_Is_OP_Advanced_Load(op) ){  // ld.sa
            TN *reg_tn = OP_result(op, 0);
            TOP op_code = TN_is_float(reg_tn) ? TOP_invala_f_e : TOP_invala_e;
            OP *invala = Mk_OP(op_code, True_TN, reg_tn);       
            BB_Append_Op(recovery_bb, invala);    
            continue;
        }
        if( CGTARG_Is_OP_Speculative(op) && !CGTARG_Is_OP_Advanced_Load(op) ){  // ld.s
            // TODO: Only the first load is needed. We can rese the result TN with NAT.
            TN *reg_tn = OP_result(op, 0);
            TOP op_code = TN_is_float(reg_tn) ? TOP_ldf8 : TOP_ld8;
            TN *ldtype_tn = TN_is_float(reg_tn) ? Gen_Enum_TN(ECV_fldtype_s) : Gen_Enum_TN(ECV_ldtype_s);
            OP *load_from_gr_zero = Mk_OP(op_code, reg_tn, True_TN, ldtype_tn, Gen_Enum_TN(ECV_ldhint), Zero_TN);
            BB_Append_Op(recovery_bb, load_from_gr_zero);
            continue;
        }
        Is_True(FALSE,("Can not get to here!"));
    }
    return recovery_bb;
}

//=============================================================================
//Function:  Build_Recovery_Block
//Input:
//    - No input.                      
//Output:
//    - No output.
//Description:
//    - Traverse the region tree, generate recovery block for every chk that
//      has no associative recovery block.
//    - Update control flow graph for every incoming recovery block.
//=============================================================================
static void 
Build_Recovery_Block()
{

    std::vector< std::pair<OP*,OP*> >::iterator iter;
    for(iter = load_chk_pairs.begin(); iter != load_chk_pairs.end(); ++iter){

        OP* load_op = iter->first;
        OP* chk_op  = iter->second;
        Is_True(Match_Chk_Ld(chk_op,load_op),("chk and spec_ld do not match!"));

	std::list<OP *> OP_exec_path = Find_Execution_Path(load_op, chk_op); 
        BB *recovery_bb = Do_Build_Recovery_Block(OP_exec_path);
        if( recovery_bb ){
            Set_BB_recovery(recovery_bb);
            TN *label_tn = Gen_Label_TN(Gen_Label_For_BB(recovery_bb), 0);            
            Set_chk_tgt(chk_op, label_tn); 
            chk_vector.push_back(chk_op);     
        }
    }
    return;
}

//=============================================================================
//Function:    Update_CFG
//Input:
//    - No explicit input.
//Output:
//    - A number, indicates how many "cut actions" have been taken.
//Description:
//    - Add all recovery blocks into the control flow graph.
//=============================================================================
static INT16 
Update_CFG()
{
    INT16 count = 0;

    std::vector<OP*, OP_ALLOC>::iterator iter;
    for(iter = chk_vector.begin(); iter != chk_vector.end(); iter++){
        
        OP *chk_op = *iter;
        Is_True(OP_chk(chk_op),("chk_op is not a chk!"));

        // Get the associate recovery block of the chk.

        BB *recovery_bb = Get_Recovery_BB(chk_op);
        Is_True(recovery_bb,("can not find corresponding recovery block!"));

        BB* home_bb = OP_bb(chk_op); 
        BOOL home_intact = BB_scheduled(home_bb);
        BOOL home_chk_split = BB_chk_split(home_bb);
        BOOL home_split_head = BB_chk_split_head(home_bb);
        BOOL home_split_tail = BB_chk_split_tail(home_bb);//bug fix for OSP_212

        // Devide the basic block with the chk as the boundary.

        BOOL cut = TRUE;
        BB* bottom_bb = NULL; 
        bottom_bb = RGN_Divide_BB(home_bb, chk_op, TRUE);
        FmtAssert(bottom_bb != NULL,
			("RGN_Divide_BB() returned NULL bottom_bb"));

        // Build corresponding edges.

        RGN_Unlink_Pred_Succ(home_bb, bottom_bb);
        RGN_Link_Pred_Succ_With_Prob(home_bb, bottom_bb, btm_prob);
        
        RGN_Gen_And_Insert_Node(recovery_bb,home_bb,bottom_bb);
        RGN_Link_Pred_Succ_With_Prob(home_bb, recovery_bb, rec_prob);
        
        Add_Goto_Op(recovery_bb, bottom_bb);
        RGN_Link_Pred_Succ_With_Prob(recovery_bb, bottom_bb, 1.0F);

        BB_freq(recovery_bb) = BB_freq(home_bb) * rec_prob;
        BB_freq(bottom_bb) = BB_freq(home_bb);

        // Set various BB flags.

        if(cut) {
            if(IPFEC_Chk_Compact) {
                //begin bug fix for OSP_212
                if(!home_chk_split){
                    Set_BB_chk_split_head(home_bb);
                    Set_BB_chk_split_tail(bottom_bb);
                }
                if(home_split_head)
                    Set_BB_chk_split_head(home_bb);
                else if(home_split_tail){
                    Reset_BB_chk_split_tail(home_bb);
                    Set_BB_chk_split_tail(bottom_bb);
                }
                //end bug fix for OSP_212
                Set_BB_chk_split(home_bb);           
                Set_BB_chk_split(bottom_bb);
                if(home_intact){
                    Set_BB_scheduled(home_bb);
                    Set_BB_scheduled(bottom_bb);
                }else{
                    Reset_BB_scheduled(home_bb);
                    Reset_BB_scheduled(bottom_bb);
                }
            }else{
                Reset_BB_scheduled(home_bb);
                Reset_BB_scheduled(bottom_bb);
            }
        }
        count++;
    }
    return count;
}
        
//=============================================================================
//Function:  Generate_Recovery_Code
//Input:
//    - No input.
//Output:
//    - Return how many recovery blocks have been built.
//Description:
//    - Build recovery blocks for corresponding chks.
//    - Update control flow graph for every incoming recovery block.
//=============================================================================
INT16
Generate_Recovery_Code()
{
    INT16 count;
    Set_Error_Phase("recovery block generation");   
    chk_vector.clear();
    Build_Recovery_Block();
    count = Update_CFG();
    load_chk_pairs.clear();
    return count;   
}



//=============================================================================
//Function:  Adjust_Recovery_Block       
//Input:
//    - No input.
//Output:
//    - No explicit output.
//Description:
//    - For every recovery block which does not jump back to the boundary of 
//      a bundle, adjust the branch op and make it to jump back to the next 
//      bundle's boundary, then update the control flow graph.
//=============================================================================
void 
Adjust_Recovery_Block()
{

    BB* home_bb = NULL;        
    BB* recovery_bb = NULL;
    BB* bottom_bb = NULL;
    
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      if (BB_recovery(bb))  
          continue;
      for (OP* op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
        if (!OP_chk(op)) 
            continue;
        OP* chk_op = op;
        home_bb = bb;
        recovery_bb = Get_Recovery_BB(chk_op);        
        Is_True(BB_recovery(recovery_bb),("not a recovery block!"));

        Is_True(BB_succs_len(recovery_bb) == 1,("recovery block's successor num > 1 !"));

        bottom_bb = BB_Unique_Successor(recovery_bb); 
        while(BB_length(bottom_bb) == 0)  
            bottom_bb = BB_next(bottom_bb);

        OP* first_op = BB_first_op(bottom_bb);
        if(OP_dummy(first_op))
            first_op = OP_next(first_op);
        Is_True(first_op != NULL,("too many dummy OP!"));
        Is_True(!OP_dummy(first_op),("too many dummy OP!"));

        if(OP_start_bundle(first_op))    
            continue;

        Is_True(bottom_bb == BB_next(home_bb),("bottom_bb must be a fall through bb of home_bb!"));
        Is_True(BB_in_succs(home_bb,bottom_bb),("bottom_bb,must be a fall through bb of home_bb!"));
        Is_True(chk_op == BB_last_op(home_bb), ("chk_op should be the last op of home bb!"));

        BB_Remove_Op(recovery_bb, BB_last_op(recovery_bb));
        Unlink_Pred_Succ(recovery_bb, bottom_bb);

        BOOL need_jump_back = TRUE;
        BB* jump_back_bb = NULL;
        OP* prev_op = chk_op;
        OP* copy_op = first_op;       
        while(!OP_start_bundle(copy_op)){        
            OP* dup_op = Dup_OP(copy_op);
            BB_Append_Op(recovery_bb, dup_op);
            prev_op = copy_op;
            if(OP_next(prev_op) != NULL){
                copy_op = OP_next(prev_op);
            }else{        
                BB* prev_bb = OP_bb(prev_op);
                BB* next_bb = BB_next(prev_bb);
                if(!BB_in_succs(prev_bb,next_bb)){
                    need_jump_back = FALSE;
                    break;
                }else{
                    while(BB_length(next_bb) == 0) 
                        next_bb = BB_next(next_bb);
                    jump_back_bb = next_bb;
                    copy_op = BB_first_op(next_bb);
                }
            } 
        }

        if(need_jump_back){
            if(OP_next(prev_op) != NULL){
                jump_back_bb = Divide_BB(OP_bb(prev_op), prev_op);        
                Is_True(jump_back_bb,("jump_back can not be NULL!"));
                Is_True(OP_start_bundle(BB_first_op(jump_back_bb)),("jump_back's first op must start a bundle!"));
                BB_freq(jump_back_bb) = BB_freq(OP_bb(prev_op));
                Set_BB_chk_split(jump_back_bb);
                Set_BB_scheduled(jump_back_bb);
                Link_Pred_Succ_with_Prob(OP_bb(prev_op), jump_back_bb, btm_prob);
            }
            Add_Goto(recovery_bb, jump_back_bb);
        }
        Reset_BB_scheduled(recovery_bb);
        SCHEDULER local_scheduler(recovery_bb, FALSE);
        local_scheduler.Schedule_BB();
        Set_BB_scheduled(recovery_bb);
      }
    }   
}

//=============================================================================
//Function: BB_Check_Bundle_Integrity
//Input:
//    - A basic block
//Output:
//    - A boolean, indicate whether the bundles in the BB is integrated.
//Description:
//    - The same as in cgemit phase, here we check whether the BB has integrate
//      bundle.
//=============================================================================

BOOL BB_Check_Bundle_Integrity(BB* bb)
{


    if(BB_length(bb) == 0){
        return TRUE;
    }
    
    for (OP* op = BB_first_op(bb);;) {
        UINT64 slot_mask;
        UINT stop_mask;
        INT slot;
        INT ibundle;

        //Gather up the OPs for the bundle.
         
        stop_mask = 0;
        slot_mask = 0;
        for (slot = 0; op && slot < ISA_MAX_SLOTS; op = OP_next(op)) {
            INT words;
            INT w;
            
            if(!OP_dummy(op)){ 
                if (OP_simulated(op)) 
                    return FALSE;    
                if(slot == 0){
                    Set_OP_start_bundle(op);
                }else{
                    Reset_OP_start_bundle(op);
                }
                words = ISA_PACK_Inst_Words(OP_code(op));
                for (w = 0; w < words; ++w) {
                    if (slot >= ISA_MAX_SLOTS)  
                        return FALSE;    
                    slot++;
                    slot_mask = slot_mask << ISA_TAG_SHIFT;
                    if ( EXEC_PROPERTY_is_M_Unit(OP_code(op)) && 
                         EXEC_PROPERTY_is_I_Unit(OP_code(op)) ){
                         slot_mask |= OP_m_unit(op) ? ISA_EXEC_PROPERTY_M_Unit : ISA_EXEC_PROPERTY_I_Unit;
                    } else { 
                         slot_mask |= ISA_EXEC_Unit_Prop(OP_code(op)); 
                    }
                    stop_mask = stop_mask << 1;
                }
                PORT_SET b0b2;
                b0b2 = b0b2 + ip_B0;
                b0b2 = b0b2 + ip_B2;
                if (TSI_Issue_Ports(OP_code(op))== b0b2){
                    slot_mask = slot_mask | ISA_EXEC_PROPERTY_B_Unit;
                }
                stop_mask |= (OP_end_group(op) != 0);
            }
        }

        if (slot == 0)
            return TRUE;
    
        if (slot != ISA_MAX_SLOTS)
            return FALSE;    
    
        for (ibundle = 0; ibundle < ISA_MAX_BUNDLES; ++ibundle) {
            UINT64 this_slot_mask = ISA_EXEC_Slot_Mask(ibundle);
            UINT32 this_stop_mask = ISA_EXEC_Stop_Mask(ibundle);
            if ((slot_mask & this_slot_mask) == this_slot_mask && (stop_mask & ~1) == this_stop_mask) 
                break;
        } 
        if (ibundle == ISA_MAX_BUNDLES)
            return FALSE;    
    }
    Is_True(FALSE,("Can not get to here!"));
}

//=============================================================================
//Function: BB_Disconnect_All_Preds
//Input:
//    - A Basic Block
//Output:
//    - No
//Description:
//    - For all predecessors of the input bb, remove all fan out and fan in edges
//      between them.
//=============================================================================

void BB_Disconnect_All_Preds(BB* bb)
{
    BBLIST* next;
    BBLIST* pred;
    for(pred = BB_preds(bb); pred; pred = next){
        BB* pred_bb = BBLIST_item(pred);
        next = BBLIST_next(pred);
        Unlink_Pred_Succ(pred_bb,bb);
    }
}

//=============================================================================
//Function: BB_Disconnect_All_Succs
//Input:
//    - A Basic Block
//Output:
//    - No
//Description:
//    - For all succeccors of the input bb, remove all fan out and fan in edges
//      between them.
//=============================================================================

void BB_Disconnect_All_Succs(BB* bb)
{
    BBLIST* next;
    BBLIST* succ;
    for(succ = BB_succs(bb); succ; succ = next){
        BB* succ_bb = BBLIST_item(succ);
        next = BBLIST_next(succ);
        Unlink_Pred_Succ(bb, succ_bb);
    }
}


//=============================================================================
//Function: BB_Take_Over_All_Preds
//Input:
//    - to_bb and from_bb
//Output:
//    - No
//Description:
//    - to_bb will inherit all predecessors of from_bb. 
//=============================================================================

void BB_Take_Over_All_Preds(BB* to_bb, BB* from_bb)
{
    BBLIST* next;
    BBLIST* pred;
    for(pred = BB_preds(from_bb); pred; pred = next){
        BB* pred_bb = BBLIST_item(pred);
        next = BBLIST_next(pred);
        Link_Pred_Succ_with_Prob(pred_bb, to_bb, BBLIST_prob(pred));
        Unlink_Pred_Succ(pred_bb,from_bb);
    }
}

//=============================================================================
//Function: BB_Take_Over_All_Succs
//Input:
//    - to_bb and from_bb
//Output:
//    - No
//Description:
//    - to_bb will inherit all successors of from_bb. 
//=============================================================================

void BB_Take_Over_All_Succs(BB* to_bb, BB* from_bb)
{
    BBLIST* next;
    BBLIST* succ;
    for(succ = BB_succs(from_bb); succ; succ = next){
        BB* succ_bb = BBLIST_item(succ);
        next = BBLIST_next(succ);
        Link_Pred_Succ_with_Prob(to_bb, succ_bb, BBLIST_prob(succ));
        Unlink_Pred_Succ(from_bb, succ_bb);
    }
}

//=============================================================================
//Function: BB_Copy_All_Preds
//Input:
//    - to_bb and from_bb
//Output:
//    - No
//Description:
//    - Link to_bb with all predecessors of from_bb. 
//=============================================================================

void BB_Copy_All_Preds(BB* to_bb, BB* from_bb)
{
    BBLIST* next;
    BBLIST* pred;
    for(pred = BB_preds(from_bb); pred; pred = next){
        BB* pred_bb = BBLIST_item(pred);
        next = BBLIST_next(pred);
        Link_Pred_Succ_with_Prob(pred_bb, to_bb, BBLIST_prob(pred));
    }
}


//=============================================================================
//Function: BB_Copy_All_Succs
//Input:
//    - to_bb and from_bb
//Output:
//    - No
//Description:
//    - Link to_bb with all successors of from_bb. 
//=============================================================================

void BB_Copy_All_Succs(BB* to_bb, BB* from_bb)
{
    BBLIST* next;
    BBLIST* succ;
    for(succ = BB_succs(from_bb); succ; succ = next){
        BB* succ_bb = BBLIST_item(succ);
        next = BBLIST_next(succ);
        Link_Pred_Succ_with_Prob(to_bb, succ_bb, BBLIST_prob(succ));
    }
}



//=============================================================================
//Function:  Handle_Chk_Split_Bunch       
//Input:
//    - The first bb of a chk split bunch.
//Output:
//    - The last bb of a chk split bunch after local scheduling.
//Description:
//    - After global scheduling, we piece together all chk split BB and then
//      do local scheduling on it. In such a way, we can get a more compact 
//      scheduling.
//=============================================================================

BB*
Handle_Chk_Split_Bunch(BB* head_bb)
{

    // Find out all chk split BBs that are originally 
    // in the same BB.

    BB* tail_bb = head_bb;
    for(BB* bb = BB_next(head_bb); bb; bb = BB_next(bb)){
        if(BB_chk_split(bb) && !BB_chk_split_head(bb)){
            tail_bb = bb;
            continue;            
        }
        if(BB_length(bb) == 0){
            continue;
        }
        break;
    }

    // Put all fragment BBs into a big one.
    
    for(BB* bb = head_bb; bb != BB_next(tail_bb); bb = BB_next(bb)){
        for(OP* op = BB_first_op(bb); op; op = OP_next(op)){
            if(OP_chk(op)){
                Is_True(op == BB_last_op(bb),("chk is not the last op of an chk split BB!"));
                BB* rec_bb = Get_Recovery_BB(op);
                Is_True(BB_recovery(rec_bb),("not a recovery block!"));
                BB_Disconnect_All_Preds(rec_bb);
                BB_Disconnect_All_Succs(rec_bb);
                Is_True(OP_xfer(BB_last_op(rec_bb)),("rec_bb's last op should be an branch!"));
                BB_Remove_Op(rec_bb, BB_last_op(rec_bb));
            }
        }
        if(bb != tail_bb)
            BB_Disconnect_All_Succs(bb);
        if(bb != head_bb){
            BB_Disconnect_All_Preds(bb);
            BB_Append_All(head_bb,bb);
            if (BB_exit(bb)) {
               BB_Transfer_Exitinfo(bb, head_bb);
               Exit_BB_Head = BB_LIST_Delete(bb, Exit_BB_Head);
               Exit_BB_Head = BB_LIST_Push(head_bb, Exit_BB_Head, &MEM_pu_pool);
            }       
            if (BB_call(bb)) {
               BB_Transfer_Callinfo(bb, head_bb);
            }       
            if (BB_asm(bb)) {
               BB_Transfer_Asminfo (bb, head_bb);
            }
            if(bb == tail_bb)
                BB_Take_Over_All_Succs(head_bb,bb);
            BB_next(BB_prev(bb)) = BB_next(bb);
            BB_prev(BB_next(bb)) = BB_prev(bb); 
        }
        bb = head_bb;
    }


    SCHEDULER local_scheduler(head_bb, FALSE);
    local_scheduler.Schedule_BB();

    Set_BB_scheduled(head_bb);
    Set_BB_chk_split(head_bb);   
    BB* end_bb = head_bb;

    // Redevide it.

    OP* last_op = BB_last_op(head_bb);    
    for(OP* cur_op = BB_first_op(head_bb);;){
        if(OP_chk(cur_op)){
            OP* chk = cur_op;
            BB* home_bb = OP_bb(chk);
            BB* rec_bb  = Get_Recovery_BB(chk);
            BB* back_bb = NULL;
            BB* btm_bb  = NULL;
            BOOL has_chk = FALSE;
            BOOL has_br  = FALSE;
            OP* bar = chk;
            OP* tmp = chk;
            float fall_thru_prob = 0;
            
            // Try divide it at a bundle && group boundary. If fail, divide it at a bundle boundary 
            
            while (tmp && !OP_end_group(tmp)) tmp = OP_next(tmp);
            if(tmp && (!OP_next(tmp) ||OP_start_bundle(OP_next(tmp)))) {
                OP* op = chk;
                while(op != NULL && !OP_end_group(op)) {
                    op = OP_next(op);
                    bar = op;
                    OP* dup_op = Dup_OP(op);
                    BB_Append_Op(rec_bb,dup_op);
                    if(OP_chk(op))  has_chk = TRUE;
                    if(TOP_is_xfer(OP_code(op)) || OP_call(op)) has_br = TRUE;
                }	
            } else {
                for(OP* op = OP_next(chk); (op != NULL && !OP_start_bundle(op)); op = OP_next(op)) {
                    bar = op;
                    OP* dup_op = Dup_OP(op);
                    BB_Append_Op(rec_bb,dup_op);
                    if(OP_chk(op))  has_chk = TRUE;
                    if(TOP_is_xfer(OP_code(op)) || OP_call(op)) has_br = TRUE;
                }
            }
            
            back_bb = Divide_BB(home_bb,bar);
            if(back_bb == NULL){
                back_bb = BB_Fall_Thru_Successor(home_bb);
                if(back_bb != NULL){
                    BBLIST *succ = BB_Find_Succ(home_bb,back_bb);
                    fall_thru_prob = BBLIST_prob(succ);
                }
            }else{
                Is_True(has_br == FALSE,("Branch op should be in the last bundle of its' home BB!"));
                Link_Pred_Succ_with_Prob(home_bb,back_bb,btm_prob);
                BB_freq(back_bb) = BB_freq(home_bb);
                Set_BB_chk_split(back_bb);
                Set_BB_scheduled(back_bb);
            }
            if(back_bb != NULL){		
                Add_Goto(rec_bb,back_bb);
            }
            if(has_chk || has_br){
                btm_bb = Divide_BB(home_bb,chk);
                Link_Pred_Succ_with_Prob(home_bb,btm_bb,btm_prob);
                BB_freq(btm_bb) = BB_freq(home_bb);
                Set_BB_chk_split(btm_bb);
                Set_BB_scheduled(btm_bb);
            }
            Link_Pred_Succ_with_Prob(home_bb,rec_bb,rec_prob);
            if(has_br && back_bb != NULL){
	            BB* frag_bb = Divide_BB(rec_bb,OP_prev(BB_last_op(rec_bb)));
                BB_Copy_All_Succs(rec_bb,btm_bb);
                Change_Succ(rec_bb,back_bb,frag_bb);
                BB_freq(frag_bb) = BB_freq(rec_bb) * fall_thru_prob;
                Reset_BB_scheduled(rec_bb);
                Reset_BB_scheduled(frag_bb);
                Set_BB_recovery(frag_bb);
            }
        }
        if(cur_op == last_op){
            break;
        }else if(OP_next(cur_op)){
            cur_op = OP_next(cur_op);
        }else{
            Is_True(BB_next(OP_bb(cur_op)),("cur_op's next bb should not be NULL!"));
            Is_True(BB_chk_split(BB_next(OP_bb(cur_op))),("cur_op's next bb should be chk_split bb!"));
            cur_op = BB_first_op(BB_next(OP_bb(cur_op)));
            end_bb = OP_bb(cur_op);
        }
    }

    Is_True(end_bb,("end_bb should not be NULL!"));
    return end_bb;
}

//=============================================================================
//Function: Force_Chk_Fail
//Input: no
//Output: no
//Description: 
//    - If IPFEC_CHK_Fail is on, change all chk.s and chk.a to "chk.a r0,..."
//    - "chk.a r0, .." always fail.
//=============================================================================

void
Force_Chk_Fail()
{

    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
        for (OP* op = BB_first_op(bb); op != NULL; op = OP_next(op)){
            if(CGTARG_Is_OP_Check_Load(op)){
                TN* ldtype_tn = OP_Is_Float_Mem(op) ? Gen_Enum_TN(ECV_fldtype) : Gen_Enum_TN(ECV_ldtype);
                Set_OP_opnd(op, 1, ldtype_tn);
                continue;
            }
            if(OP_chk_s(op)){
                OP* chk_a;
                TN* pr_tn = OP_opnd(op,0);
                TN *aclr_tn   = Gen_Enum_TN(ECV_aclr_clr);
                TN *target_tn = OP_opnd(op,2);       
                if(OP_float_chk(op)){
                    chk_a = Mk_OP(TOP_chk_f_a, pr_tn, aclr_tn, FZero_TN, target_tn);    
                }else{
                    chk_a = Mk_OP(TOP_chk_a, pr_tn, aclr_tn, Zero_TN, target_tn);    
                }
                OP_flags(chk_a) = OP_flags(op);
                BB_Insert_Op_After(bb, op, chk_a);
                BB_Remove_Op(bb,op);
                continue;
            }
            if(OP_chk_a(op)){
                TN* reg_tn = OP_float_chk(op) ? FZero_TN : Zero_TN;
                Set_OP_opnd(op, 2, reg_tn);
                continue;
            }
        }
    }
}


