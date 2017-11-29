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
//
//  Module:  speculation.cxx
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/speculation.cxx,v $
//
//  Description:
//  ============
//
//  Implementation of Aurora control and data speculation.
//
//=============================================================================

#include <map>

#include "cgtarget.h"
#include "op_list.h"
#include "cg_dep_graph.h"
#include "bb.h"

#include "speculation.h"
#include "recovery.h"
#include "scheduler.h"
#include "dag.h"
#include "vt_region.h"
#include "tracing.h"
#include "ipfec_defs.h"
#include "vt_dag.h"
#include "vt_partition.h"
#include "op.h"
#include "cgtarget.h"
#include "ipfec_options.h"


std::vector< std::pair<OP*, OP*> >  load_chk_pairs;


OP*
Change_ld_Form(OP *load_op, ISA_ENUM_CLASS_VALUE target_form)
{
    Is_True(OP_load(load_op),("not a load OP!"));

    BOOL compatible = FALSE;
    BOOL float_op = OP_Is_Float_Mem(load_op) ? TRUE : FALSE;
    compatible = float_op ? ((target_form >= ECV_fldtype_s) && (target_form <= ECV_fldtype)) :
                            ((target_form >= ECV_ldtype_s)  && (target_form <= ECV_ldtype));
    FmtAssert(compatible,("load op and ldtype not compatible."));
        
    if(!float_op && (target_form < ECV_ldtype_s || target_form > ECV_ldtype_sa) ||
        float_op &&  (target_form < ECV_fldtype_s || target_form > ECV_fldtype_sa)) {
        return NULL;
    }

    if( TN_enum(OP_opnd(load_op, enum_ldtype_pos)) == target_form)
        return load_op;

    Set_OP_opnd(load_op, enum_ldtype_pos, Gen_Enum_TN(target_form));
    Set_OP_speculative(load_op);

    return load_op;
}


//=============================================================================
//Function: Build_Incoming_Edges
//Input:
//    - OP *spec_ld: current ld that has been speculated.
//    - OP *chk: corresponding chk that has been constructed.
//Output:
//    - No explicit output.
//Description:
//    - There are five kinds of dependent relationship.
//          spec_ld-----> chk     
//          cmp---------> chk      
//          store-------> chk      
//          chk1--------> chk2 
//          br----------> chk    
//    - A new kind of arc will be invented to represent these relationships.
//      Its name is CG_DEP_PRECHK(its counterpart is CG_DEP_POSTCHK).
//=============================================================================
static void
Build_Incoming_Edges(OP *spec_ld, OP *chk)
{
    UINT8 opnd = OP_chk_a(chk) ? 2 : 1;
    new_arc(CG_DEP_REGIN, spec_ld, chk, 0, opnd, FALSE);
    for(ARC_LIST* arcs = OP_preds(spec_ld); arcs; arcs = ARC_LIST_rest(arcs)) {
        ARC* arc = ARC_LIST_first(arcs);
        OP* op = ARC_pred(arc);            
        if(OP_bb(op) != OP_bb(chk))
          continue;
        if(ARC_is_spec(arc) && !OP_Scheduled(op)) {
            if(OP_store(op)){
                new_arc_with_latency(CG_DEP_PRECHK, op, chk, 1, 0, 0, FALSE);
                continue;
            }
            if(ARC_kind(arc) == CG_DEP_CTLSPEC) {
                new_arc_with_latency(CG_DEP_CTLSPEC, op, chk, 1, 0, 0, FALSE);
                continue;
            }
            if(OP_chk(op)){
                new_arc_with_latency(CG_DEP_PRECHK, op, chk, 0, 0, 0, FALSE);
                continue;
            }
            Is_True(!OP_br(op),("A branch can not get to here!"));
        }
    }
    for(OP* op = OP_prev(chk); op; op = OP_prev(op)){
        if(OP_chk(op)){
            new_arc_with_latency(CG_DEP_PRECHK, op, chk, 0, 0, 0, FALSE);
        }
    }    
}


//=============================================================================
//Function:  BOOL OP_baneful(OP *op)
//Input:
//    - OP *op
//Output:
//    - A BOOL variable.
//    - True indicates that op writes only to GFP registersers.
//    - False indicates that op writes to eles places.
//Description:
//    - GFP stands for General register, Floating point register and 
//      Predicate register.
//=============================================================================
BOOL
OP_baneful(OP *op)
{

    if(OP_like_store(op))
        return TRUE;
    
    if(OP_results(op) == 0) 
        return TRUE;

    for (INT i = 0; i < OP_results(op); i++) {
        TN *result = OP_result(op, i);
        if( !TN_is_GFP(result) )
            return TRUE;
    }

    switch (OP_code(op)) {   
    default:
        return FALSE; 
    case TOP_mov_f_cpuid:
    case TOP_mov_f_dbr:
    case TOP_mov_f_ibr:
    case TOP_mov_f_msr:
    case TOP_mov_f_pkr:
    case TOP_mov_f_pmc:
    case TOP_mov_f_pmd:
    case TOP_mov_f_rr:
    case TOP_tpa:
    case TOP_probe_r:
    case TOP_probe_w:
    case TOP_probe_i_r:
    case TOP_probe_i_w:
    case TOP_alloc_3:
    case TOP_alloc:
    case TOP_lfetch_r:
    case TOP_lfetch_r_excl:
    case TOP_lfetch_r_fault:
    case TOP_lfetch_r_fault_excl:
    case TOP_lfetch_i:
    case TOP_lfetch_i_excl:
    case TOP_lfetch_i_fault:
    case TOP_lfetch_i_fault_excl:
    case TOP_xchg1:
    case TOP_xchg2:
    case TOP_xchg4:
    case TOP_xchg8:
    case TOP_ldf_fill:
    case TOP_ld8_fill:
    case TOP_ldf_r_fill:
    case TOP_ld8_r_fill:
    case TOP_ldfp8_i:
    case TOP_ldfpd_i:
    case TOP_ldfps_i:
    case TOP_ldf8_r:
    case TOP_ldfd_r:
    case TOP_ldfe_r:
    case TOP_ldfs_r:
    case TOP_ldf8_i:
    case TOP_ldfd_i:
    case TOP_ldfe_i:
    case TOP_ldfs_i:
    case TOP_ldf_i_fill:
    case TOP_ld8_i_fill:
    case TOP_cmpxchg1:
    case TOP_cmpxchg2:
    case TOP_cmpxchg4:
    case TOP_cmpxchg8:
    case TOP_fetchadd4:
    case TOP_fetchadd8:
    case TOP_stf8_i:
    case TOP_stf_i_spill:
    case TOP_stfd_i:
    case TOP_stfe_i:
    case TOP_stfs_i:
    case TOP_st8_i_spill:
    case TOP_st1_i:
    case TOP_st2_i:
    case TOP_st4_i:
    case TOP_st8_i:
    case TOP_spadjust:
    case TOP_intrncall:
    case TOP_ifixup:
    case TOP_ffixup:
    case TOP_dfixup:   
        return TRUE;
    }
}


//=============================================================================
//Function:  Compute_Topological_Order
//Input:
//    - REGIONAL_CFG_NODE* root;
//    - list<REGIONAL_CFG_NODE* >& node_list;
//    - set<REGIONAL_CFG_NODE* >& visited;
//Output:
//    - <none>
//Description:
//    - Put all successors of a reginal cfg node in a list, in topological order.
//=============================================================================
static void
Compute_Topological_Order(REGIONAL_CFG_NODE* root, 
                          std::list<REGIONAL_CFG_NODE*>& node_list, 
                          std::set<REGIONAL_CFG_NODE*>& visited)
{
    if( root == NULL ) return;
    for (CFG_SUCC_NODE_ITER succ_iter(root); succ_iter != 0; ++succ_iter) {
        REGIONAL_CFG_NODE* node = *succ_iter;
        if(visited.find(node) != visited.end())
            continue;
        Compute_Topological_Order(node, node_list, visited);
    }
    node_list.push_front(root);
    visited.insert(root);
    return;
}


//=============================================================================
//Function: Build_Outgoing_Edges
//Input:
//    - OP *spec_ld: current ld that has been speculated.
//    - OP *chk: corresponding chk that has been constructed.
//Output:
//    - No explicit output.
//Description:
//    - A new kind of arc: CG_DEP_POSTCHK, is invented to represent those edges
//      that come out from the chk to the other OPs. Such a kind of OP is 
//      prevented from been scheduled across the chk to become a member of the
//      speculative chain.
//=============================================================================
static void
Build_Outgoing_Edges(OP *spec_ld, OP *chk)
{

    if( IPFEC_Hold_Uses && CGTARG_Is_OP_Speculative_Load(spec_ld)){
        for (ARC_LIST *arcs = OP_succs(spec_ld); arcs; arcs = ARC_LIST_rest(arcs)) {   
             ARC *arc = ARC_LIST_first(arcs);
             new_arc(CG_DEP_POSTCHK, chk, ARC_succ(arc), 0, 0, FALSE); 
        }
        return;
    }

    typedef mempool_allocator<TN*> TN_ALLOC;
    typedef std::set<TN*, compare_tn, TN_ALLOC> TNs;

    typedef mempool_allocator<OP*> OP_ALLOC;
    typedef std::set<OP*, compare_op, OP_ALLOC> OPs;

    typedef mempool_allocator< std::pair<REGIONAL_CFG_NODE* const, TNs> > NODE_TNs_ALLOC;
    typedef std::map<REGIONAL_CFG_NODE*, TNs, compare_node, NODE_TNs_ALLOC>  NODE_TNs_MAP;
    typedef mempool_allocator< std::pair<REGIONAL_CFG_NODE* const, OPs> > NODE_OPs_ALLOC;
    typedef std::map<REGIONAL_CFG_NODE*, OPs, compare_node, NODE_OPs_ALLOC>  NODE_OPs_MAP;

    NODE_TNs_MAP spec_chain_live;  // Record all live in TNs of the speculative chain.
    NODE_TNs_MAP spec_chain_def;   // Record all TNs that are defined on the speculative chain.
    NODE_OPs_MAP dependent_ops;
    
    BB* home_bb = OP_bb(chk);
    REGIONAL_CFG_NODE* root = Regional_Cfg_Node(home_bb);
 
    std::list<REGIONAL_CFG_NODE* /*REGIONAL_CFG_NODE_ALLOC*/>  node_list;
    std::set<REGIONAL_CFG_NODE* /*REGIONAL_CFG_NODE_ALLOC*/>  visited;
    
    Compute_Topological_Order(root, node_list, visited);   
    visited.clear();

    // Initialize the live in set.
    
    for (INT i = 0; i < OP_opnds(spec_ld); i++) {
        TN* opnd = OP_opnd(spec_ld, i);
        if (TN_is_register(opnd) && !TN_is_const_reg(opnd)){
            Is_True(!TN_Pair_In_OP(spec_ld, opnd, opnd), ("can not be a post-incr load!"));
            spec_chain_live[root].insert(opnd);
        }
    }
    
    // Initialize the definition set.
    
    TNs spec_ld_tgt;
    for (INT i = 0; i < OP_results(spec_ld); i++) {
        TN* rslt = OP_result(spec_ld, i);
        Is_True(TN_is_register(rslt), ("rslt tn must be a register tn!"));
        spec_chain_def[root].insert(rslt);
        spec_ld_tgt.insert(rslt);
    }

 
    TN* chk_ptn = OP_opnd(chk,0);
    TN* spec_ld_ptn = OP_opnd(chk,0); 
    // Iterate all successors, in topological order.
    
    for(std::list<REGIONAL_CFG_NODE*>::iterator iter = node_list.begin();
        iter != node_list.end();
        iter++) {
        REGIONAL_CFG_NODE* node=(*iter);
        if( !node->Is_Region() && BB_exit(node->BB_Node()) )  
            continue; 
    
        // Initialize the associative data structures of the current BB.
        bool the_first_iter = true;
        for (CFG_PRED_NODE_ITER pred_iter(node); pred_iter != 0; ++pred_iter) {
            if ( visited.find(*pred_iter) == visited.end()) 
                continue;
            spec_chain_def[node].insert(spec_chain_def[*pred_iter].begin(), spec_chain_def[*pred_iter].end());
            spec_chain_live[node].insert(spec_chain_live[*pred_iter].begin(), spec_chain_live[*pred_iter].end());
            if(the_first_iter){
                dependent_ops[node].insert(dependent_ops[*pred_iter].begin(), dependent_ops[*pred_iter].end());
                the_first_iter=false;
            }else{
                for(std::set<OP*, compare_op, OP_ALLOC>::iterator iter_op = dependent_ops[node].begin();
                    iter_op != dependent_ops[node].end(); ){
                    if(dependent_ops[*pred_iter].find(*iter_op) == dependent_ops[*pred_iter].end()){
                        std::set<OP*, compare_op, OP_ALLOC>::iterator iter_op_er;
                        iter_op_er=iter_op;
                        ++iter_op;
                        dependent_ops[node].erase(iter_op_er);
                    }else{
                        ++iter_op;
                    }
                }
            }
            
        }
       
        visited.insert(node);
        if(node->Is_Region())
             continue;
        BB* bb = node->BB_Node();

        OP* start_op = bb == OP_bb(chk) ? OP_next(chk): BB_first_op(bb);
        for(OP* op = start_op; op != NULL; op = OP_next(op)) {

            TNs  tmp_live_in;
            BOOL flow_dep = FALSE;
            BOOL flow_on_predicate = FALSE;
            BOOL flow_on_opnd = FALSE;
            BOOL output_dep = FALSE;            
            TN*  flow_dep_tn = NULL;
            TN*  cur_ptn = NULL; 
    
            // If current op is already dependent on the chk,
            // go directly to handle its' successors.
            if(dependent_ops[node].find(op) != dependent_ops[node].end()){
                goto handle_succs;
            }

            // C1:
            // If the current op is a baneful op that in the same BB
            // as the chk op, make sure it will not be scheduled 
            // across the chk. Why? I forgot it. This may originated 
            // from a bug.
    
            if(OP_bb(chk) == OP_bb(op) && OP_baneful(op))
                goto gen_arc;

            // C2 & C4:
            // Check to see whether the current op will change 
            // the value of live TNs or the target register of 
            // the speculative load.
    
            for(INT i = 0; i < OP_results(op); i++) {
                TN* rslt = OP_result(op, i);
                if(   spec_ld_tgt.find(rslt) != spec_ld_tgt.end() 
                   || spec_chain_live[node].find(rslt) != spec_chain_live[node].end()){
                    goto gen_arc;
                }
                if(spec_chain_def[node].find(rslt) != spec_chain_def[node].end()){
                    output_dep = TRUE;
                }
            }
            
            for(INT i = 0; i < OP_opnds(op); i++) {
                TN* opnd = OP_opnd(op, i);
                if(    TN_is_register(opnd) 
                    && !TN_is_const_reg(opnd) 
                    && spec_chain_def[node].find(opnd) != spec_chain_def[node].end()){ 
                    flow_dep = TRUE; 
                    if(OP_has_predicate(op) && i == OP_PREDICATE_OPND){
                        flow_on_predicate = TRUE;
                    }else{
                        flow_on_opnd = TRUE;
                    }
                }
            }
            
            // If the current op has nothing to do with the speculative load.
            // Nothing will be done.

            if(!flow_dep && !output_dep){
                // Here we include output_dep. Because in recovery block
                // generation phase, this kind of OPs will be included 
                // in recovery block. So we should carefully check it to 
                // make sure it is not a baneful op or other undesirded op. 
                continue;
            }
            
            // If it is a baneful op, make sure it will not be  scheduled across
            // the check.
    
            if(OP_baneful(op)){  
                goto gen_arc;
            }

            if(OP_has_predicate(op)){
                cur_ptn = OP_opnd(op, 0);
                if(    !flow_on_predicate
                    && cur_ptn != True_TN 
                    && cur_ptn != chk_ptn
                    && cur_ptn != spec_ld_ptn){
                    goto gen_arc;
                }                
            }
 
            // C3:
            //  - predicate opnd is not a TRUE tn, 
            //  - predicate opnd is defined on the speculative chain,
            //  - the speculative load is a data speculative load.
            //  prevent it from been schedule across the chk.

            if(flow_on_predicate && CGTARG_Is_OP_Advanced_Load(spec_ld)){
                Is_True(!TN_is_true_pred(cur_ptn),("flow dependent can't caused by predicate register!"));
                goto gen_arc;
             }
    
            // C5:
            // It is a cascaded load.
    
            if(flow_dep && flow_on_opnd && OP_load(op))
            {
                ARC *arc = new_arc_with_latency(CG_DEP_POSTCHK, chk, op, 0, 0, 0, FALSE);
                Set_ARC_is_dotted(arc, TRUE);
             }

            // update live in set.

            for (INT i = 0; i < OP_opnds(op); i++) {
                TN* opnd = OP_opnd(op, i);
                if(    TN_is_register(opnd) 
                    && !TN_is_const_reg(opnd) 
                    && spec_chain_def[node].find(opnd) == spec_chain_def[node].end())
                { 
                    if(TN_Pair_In_OP(op,opnd,opnd)){
                        // C2:
                        goto gen_arc;           
                    }else{
                        tmp_live_in.insert(opnd);
                    }
                }
            }
            spec_chain_live[node].insert(tmp_live_in.begin(),tmp_live_in.end());

            // update relative data structures.
            if(flow_dep){ 
                for (INT i = 0; i < OP_results(op); i++) {
                    TN *rslt = OP_result(op, i);
                    Is_True(TN_is_register(rslt),("result should be a register tn!"));
                    spec_chain_def[node].insert(rslt);
                }            
            }
            continue;

gen_arc:  
            new_arc_with_latency(CG_DEP_POSTCHK, chk, op, 0, 0, 0, FALSE);  
            
handle_succs:
            for (ARC_LIST *arcs = OP_succs(op); arcs; arcs = ARC_LIST_rest(arcs)){
                ARC *arc = ARC_LIST_first(arcs);
                OP  *succ = ARC_succ(arc);				  
                if(!OP_br(op) && !ARC_is_spec(arc))
                    dependent_ops[node].insert(succ);
            }
        }

    }
}

//=============================================================================
//Function:  Connect_Clones_with_CHK
//Input:
//      - Convert every ld.s on the op list to ld.sa.
//      - All OPs on the op list should be load .
//Output:
//      - <none>
//Description:
//      -
//=============================================================================
static void Connect_Clones_with_CHK(std::vector<OP *>& clones, OP *chk)
{

    std::vector<OP *>::iterator iter;
    for(iter = clones.begin() ; iter != clones.end(); ++iter) {

        OP *op = *iter;         
        Is_True(OP_load(op),("not a load OP"));

        if( CGTARG_Is_OP_Speculative(op) && !CGTARG_Is_OP_Advanced_Load(op)){
            //If load op is a ld.s/ldf.s , convert it to ld.sa/ldf.sa .
            TN *ldtype_tn = OP_Is_Float_Mem(op) ? Gen_Enum_TN(ECV_fldtype_sa) :
                                                  Gen_Enum_TN(ECV_ldtype_sa);
            Set_OP_opnd(op, enum_ldtype_pos, ldtype_tn);        
            new_arc(CG_DEP_PRECHK, op, chk, 0, 0, FALSE);
        }
    }
}

//=============================================================================
//Function:  Loacl_Insert_CHK
//Input:
//    - OP *spec_ld: The speculative ld. 
//    - OP *point: An OP in OPs list. It points to the position where we 
//                 will insert the chk.
//    - TN *pr_tn: The qualifying predicate of the speculative ld when it
//                 has not been scheduled.
//Output:
//    - No explicit output.
//Description:
//    - This function is dedicated to local scheduling.
//=============================================================================
OP*
Local_Insert_CHK(OP *spec_ld, OP *point, TN *pr_tn)
{        

    Is_True(CGTARG_Is_OP_Speculative(spec_ld),("not a speculative load!"));
    Is_True(point != NULL,("Insert position cannot be NULL!"));
    Is_True(OP_bb(spec_ld) == OP_bb(point),("not in the same BB!"));

    OP *chk;

    if( CGTARG_Is_OP_Advanced_Load(spec_ld) ) {
       
        TN *aclr_tn   = Gen_Enum_TN(ECV_aclr_clr);
        TN *reg_tn    = OP_result(spec_ld, 0);
        TN *target_tn = Gen_Label_TN(Gen_Label_For_BB(OP_bb(spec_ld)), 0);

        if(TN_is_float(reg_tn)){
            chk = Mk_OP(TOP_chk_f_a, pr_tn, aclr_tn, reg_tn, target_tn);    
        } else {
            chk = Mk_OP(TOP_chk_a, pr_tn, aclr_tn, reg_tn, target_tn);    
        }
    } else {

        TN *reg_tn    = OP_result(spec_ld, 0);
        TN *target_tn = Gen_Label_TN(Gen_Label_For_BB(OP_bb(spec_ld)), 0);

        if(TN_is_float(reg_tn)){
            chk = Mk_OP(TOP_chk_f_s, pr_tn, reg_tn, target_tn);    
        } else {
            chk = Mk_OP(TOP_chk_s, pr_tn, reg_tn, target_tn);    
        }
    }

    BB_Insert_Op_After(OP_bb(spec_ld), point, chk);  
 
    if (Get_Trace(TP_A_RBG, TT_RBG_DRAW_GLBL_CFG)) { 
        draw_global_cfg();
    }
 
    Build_Outgoing_Edges(spec_ld, chk);     
    Build_Incoming_Edges(spec_ld, chk);

    load_chk_pairs.push_back(std::pair<OP*,OP*>(spec_ld,chk));

    return chk;    
}


//=============================================================================
//Function: Insert_CHK
//Input:
//    - OP *spec_ld_list: the speculative ld and its' splitting clones. The
//      first element of the list is the speculative ld that has been 
//      scheduled. Others are clones of this ld. These OPs are newly added
//      into the DAG. Thus they have not been scheduled and they will be
//      scheduled at latter time.
//    - OP *orig_ld: the original ld. It points to the position where we 
//      will insert the chk.
//Output:
//    - No explicit output.
//Description:
//    - Insert chk for the speculative load and update DAG.
//=============================================================================
OP*
Insert_CHK(OP* primary_ld, std::vector<OP *>& copys, BB* home_bb, OP* pos, TN* pr_tn)
{

    Is_True(CGTARG_Is_OP_Speculative(primary_ld),("not a speculative load OP!"));

    OP *chk;
    if( !copys.empty() || CGTARG_Is_OP_Advanced_Load(primary_ld) ) {

        //If the "copys" is not empty, it means that the ld has been 
        //scheduled across a joint point. Thus several compensation OPs
        //have been added into the DAG. For this kind of case, we use 
        //chk.a to handle the work of check .
                        
        TN *aclr_tn   = Gen_Enum_TN(ECV_aclr_clr);
        TN *reg_tn    = OP_result(primary_ld, 0);
        TN *target_tn = Gen_Label_TN(Gen_Label_For_BB(home_bb), 0);

        if(TN_is_float(reg_tn)){
            chk = Mk_OP(TOP_chk_f_a, pr_tn, aclr_tn, reg_tn, target_tn);    
        } else {
            chk = Mk_OP(TOP_chk_a, pr_tn, aclr_tn, reg_tn, target_tn);    
        }
    } else {

        TN *reg_tn    = OP_result(primary_ld, 0);
        TN *target_tn = Gen_Label_TN(Gen_Label_For_BB(home_bb), 0);

        if(TN_is_float(reg_tn)){
            chk = Mk_OP(TOP_chk_f_s, pr_tn, reg_tn, target_tn);    
        } else {
            if(IPFEC_Force_CHK_Fail){
                chk = Mk_OP(TOP_chk_s_m, pr_tn, reg_tn, target_tn);    
            } else {
                chk = Mk_OP(TOP_chk_s, pr_tn, reg_tn, target_tn);                
            }
        }
    }

    OP_srcpos(chk) = OP_srcpos(primary_ld);
    
    if(pos != NULL){
       Is_True(OP_bb(pos) == home_bb,("Position error!"));
       BB_Insert_Op_After(home_bb, pos, chk);
    } else {
       BB_Prepend_Op(home_bb, chk);
    }

    Build_Outgoing_Edges(primary_ld, chk);        
    Build_Incoming_Edges(primary_ld, chk);

    if (!copys.empty())
        Connect_Clones_with_CHK(copys,chk);  


    load_chk_pairs.push_back(std::pair<OP*,OP*>(primary_ld,chk));
    return chk;    
}

void
Set_Speculative_Chain_Begin_Point(OP* chk_op, OP* load_op)
{
    std::vector< std::pair<OP*,OP*> >::iterator  iter;
    for(iter = load_chk_pairs.begin(); iter != load_chk_pairs.end(); ++iter){
        OP* second = iter->second;
        if(chk_op == second){
            iter->first = load_op;
            break;
        }
    }
    return;
}



/*
 *
 *  We suppose the load will be converted to a ld.s and will
 *  be inserted in the position that indicated by target_bb
 *  and pos.
 *
 */ 
 
BOOL
Is_Control_Speculation_Gratuitous(OP* load, BB* target_bb, OP* pos)
{
    TN* base_tn;
    TN* ofst_tn;
    OP_Base_Offset_TNs(load,&base_tn,&ofst_tn);
    BB* cur_bb = target_bb;
    for(OP* op = pos ? pos : BB_last_op(cur_bb);;){
      if(op == NULL){
        if(BB* pred = BB_Unique_Predecessor(cur_bb)){
          //if(BB_call(pred)) return FALSE;
          cur_bb = pred;
          op = BB_last_op(cur_bb);
          continue;
        }else{
          return FALSE;
        }
      }else{
        if(OP_Defs_TN(op,base_tn)){
          return FALSE;
        }else{
          if((OP_load(op) || OP_store(op)) && !CGTARG_Is_OP_Speculative_Load(op)){
            TN* base;
            TN* ofst;
            OP_Base_Offset_TNs(op,&base,&ofst);
            if((base_tn == base) && (OP_opnd(op,TOP_Find_Operand_Use(OP_code(op),OU_predicate)) == True_TN)){
              return TRUE;
            }
          }
          op = OP_prev(op);
          continue;
        }
      }
    }
}

//======================================================================
//
//  Delete_Recovery_Info_For_BB
//
//  Delete the chk op from recovery info if this BB is deleted in
//  cflow optimization.
//
//======================================================================
void
Delete_Recovery_Info_For_BB(BB *bb) 
{
    for (OP *op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
        if (OP_chk(op)) {
	    std::vector< std::pair<OP*,OP*> >::iterator iter;
            for(iter = load_chk_pairs.begin(); 
                iter != load_chk_pairs.end(); ++iter)
            {
                OP* second = iter->second;
                if(second == op){
                    load_chk_pairs.erase(iter);
                    break;
                }
            }
        }
    }
}

//======================================================================
//  
//  BB_Hold_Disjoint_Speculative_Code()
//
//  Judge where the BB hold a dangle speculative load or chk.
//  
//======================================================================
BOOL
BB_Hold_Disjoint_Speculative_Code(BB* bb)
{
    for(std::vector< std::pair<OP*, OP*> >::iterator iter = load_chk_pairs.begin();
        iter != load_chk_pairs.end(); ++iter)
    {
        OP* first = iter->first;
        OP* second = iter->second;
        if(first->bb != second->bb &&
           (first->bb == bb || second->bb == bb))
        {
            return TRUE;
        }
    }
    return FALSE;
}


