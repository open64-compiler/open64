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
//=============================================================================
//=============================================================================
//
//  Module :  multibranch.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/multi_branch.cxx,v $
//
//  Description:
//  ============
//
//  Implementation of multi-branch.
//
//=============================================================================
//=============================================================================

#include "defs.h"
#include "errors.h"

#include "tn.h"
#include "op.h"
#include "op_map.h"
#include "be_util.h"

#include "timing.h"
#include "cg_dep_graph.h"
#include "cgtarget.h"
#include "targ_isa_bundle.h"
#include "tracing.h"

#include "ipfec_options.h"
#include "cggrp_microsched.h"
#include "cggrp_ptn_table.h"
#include "scheduler.h"
#include "dag.h"
#include "multi_branch.h"

static MEM_POOL  MLBR_pool_struct;
MEM_POOL* const MLBR_pool = &MLBR_pool_struct;

/////////////////////////////////////
static void
Initialize_Memory(void)
/////////////////////////////////////
//
//  Prepare the MLBR_pool for memory allocation.
//
/////////////////////////////////////
{
  static BOOL did_init = FALSE;

  if ( ! did_init ) {
    MEM_POOL_Initialize(MLBR_pool,"multi-branch pool",FALSE);
    did_init = TRUE;
  }
  MEM_POOL_Push(MLBR_pool);
}
/////////////////////////////////////
static void
Finalize_Memory(void)
/////////////////////////////////////
//
//  Delete all the memory that we used for strictly private stuff.
//
/////////////////////////////////////
{
  MEM_POOL_Pop(MLBR_pool);
  MEM_POOL_Delete(MLBR_pool);
}

//------------------------------------------------------------
// BB_has_target_bb(BB *bb, BB *target_bb)
//   Decide BB has xfer op, which target address may be label of
//   target_bb
//------------------------------------------------------------
static BOOL
BB_has_target_bb(BB *bb, BB *target_bb)
{
    OP *op;
    FOR_ALL_BB_OPs_REV(bb, op) {
        if (OP_dummy(op)) continue;
        if (OP_xfer(op)){
             INT tfirst, tcount;
             CGTARG_Branch_Info(op, &tfirst, &tcount);
             if (tcount == 0 ) {
                 // indirect jump to target_bb
                 return TRUE;
             } else {
                 TN *dest = OP_opnd(op, tfirst);
                 if (Is_Label_For_BB(TN_label(dest), target_bb)) { 
                     return TRUE;
                 } 
             }
        } else {
            return FALSE;
        }
    }
    return FALSE;
}

//------------------------------------------------------------
// BB_has_goto_bb(BB *bb, BB *target_bb)
//   Decide BB has goto branch op(exclude br.cloop), which 
//   target address is label of target_bb , and it don't have 
//   other conditional branch to this label
//------------------------------------------------------------
static BOOL
BB_has_goto_bb(BB *bb, BB *target_bb) {
    OP *op;
    FOR_ALL_BB_OPs(bb, op) {
        if (OP_xfer(op) && !OP_chk(op)) {
            INT tfirst, tcount;
            CGTARG_Branch_Info(op, &tfirst, &tcount);
            if (tcount != 0) {
                TN *dest = OP_opnd(op, tfirst);
                if (Is_Label_For_BB(TN_label(dest), target_bb) &&
                    (CGTARG_OP_is_counted_loop(op) ||
                    OP_code(op) != TOP_br)) return FALSE;
                if (Is_Label_For_BB(TN_label(dest), target_bb) &&
                    !OP_cond_def(op) && op == BB_branch_op(bb))  return TRUE;
             }
         }
    }
    return FALSE;
}
//-------------------------------------------------------------
// BOOL Multi_Branch_Valid_BB(BB *bb)
//   OP can do mult_branch must meet following requirment:
//    1) prev_bb is the unique predecessor of bb or 
//      bb's other predecessors are all GOTO-kind 
//    2) prev_bb must not target to this bb
//    3) br_call cannot be middle of pattern group
//      
//-------------------------------------------------------------
BOOL Multi_Branch_Valid_BB(BB *bb, BB *partial_head=NULL)
{
    if (!IPFEC_Enable_Multi_Branch || 
        !LOCS_Enable_Bundle_Formation) return FALSE;

    if (partial_head && !IPFEC_Enable_Pre_Multi_Branch) 
        return FALSE;


    if (bb == NULL) return FALSE;

    // make sure that multiple branch can't act upon EH handler BB
    // as well as BBs corresponding to EH range
    if (BB_handler (bb) || BB_Has_Exc_Label (bb))
      return FALSE;

    // skip list
    if (IPFEC_Query_Skiplist(mlbr_skip_bb, BB_id(bb), 
                             Current_PU_Count())) 
        return FALSE;
    
    if (BB_cycle(bb) > 1) return FALSE;

    // Heur: If freq is very small, give up multi-branch, for we can
    // speculation some ops to these BB. But post_multi_branch still 
    // can process this problem, for after then compiler do nothing.
    // Here we consider partial_head as a flag to determine what we
    // are doing. (pre_multi_branch or post_multi_branch)
    // Now assumption: 
    //   if partial_head as NULL, it means post_multi_branch
    //   
    if (BB_freq(bb) <= 0.0 && partial_head != NULL) {
        return FALSE;
    }

    if (partial_head) { 
        if (!BB_last_op(partial_head) || 
            !OP_Scheduled(BB_last_op(partial_head))) return FALSE;
    }

    BB *prev_bb = BB_prev(bb);
    if (prev_bb==NULL) return FALSE;
    for (BBLIST * bblst = BB_preds(bb); bblst; bblst = BBLIST_next(bblst)) {
        BB* pred_bb = BBLIST_item(bblst);
        if (pred_bb == prev_bb) {
            while (BB_length(prev_bb) == 0) { 
                prev_bb=BB_prev(prev_bb);
                if (prev_bb == NULL || !BB_Is_Unique_Predecessor(BB_next(prev_bb),prev_bb) ||
                     BB_has_target_bb(prev_bb,BB_next(prev_bb))) { 
                     return FALSE;
                }
            }
        } else {
            // If bb's non-previous predecessors are all GOTO-kind, 
            // we also can do multi-branch through simple tail duplication
            //  if bb has goto branch as br.few, it is also meet requirment;
            if (BB_partial_bundle(pred_bb) || BB_chk_split(pred_bb) ||
               BB_chk_split_head(pred_bb) || BB_call(bb) || 
               (!BB_Is_Unique_Successor(pred_bb, bb) &&
	        !BB_has_goto_bb(pred_bb, bb))) 
            return FALSE;
            // if BB has call, it don't allow dup branch after call op.
	    if (BB_call(bb)) return FALSE;
            if (pred_bb == BB_next(bb)) return FALSE; //fix bug in vortex 1 compiler unexpected pr (25)
	    //if BB is a single block loop, then return false
	    if (pred_bb ==bb){
	      return false;
	    }
        }
    }  
         
    // BB can jump to op's bb(include indirect and direct)
    if (BB_has_target_bb(prev_bb, bb)) return FALSE;

    // prev_bb branch op cannot be br.call
    if (BB_call(prev_bb)) return FALSE;
    
    // BB first op should be start of bundle;
    if (!BB_first_op(bb) || !OP_start_bundle(BB_first_op(bb))) return FALSE;
  
    // partial_head is the last bundle's BB?
    if (partial_head) {
        INT ops_num=0;
        while (partial_head != prev_bb) {
            ops_num += BB_length(prev_bb); 

            // partial bb should not be scheduled.
            if (BB_scheduled(prev_bb) ||
                ops_num > ISA_MAX_SLOTS * ISA_MAX_ISSUE_BUNDLES)
                return FALSE;

            prev_bb = BB_prev(prev_bb);
            if (prev_bb == NULL) return FALSE;
        }
    }

    return TRUE;
}


//-------------------------------------------------------------
// BOOL Multi_Branch_Valid_OP(OP *op)
//   OP can do mult_branch must meet following requirment:
//    1) br_call cannot be middle of pattern group if op is call
//    2) OP can;t be ret branch,
//    3) for profit, we don't allow op as memory, chk_a etc op;
//      
//-------------------------------------------------------------
BOOL Multi_Branch_Valid_OP(OP *op, BB *partial_head=NULL) 
{
    // op can be A-type chk branch instruction
    // we check it seperately
    if (OP_xfer(op) && !OP_chk(op)) { // br.cond br.few br.call
        
        // skip special case : br_ret, br_cloop, br_call with many preds, etc
        if (OP_code(op) == TOP_br_ret) return FALSE;
	if (CGTARG_OP_is_counted_loop(op)) return FALSE;
	if (BB_preds_len(OP_bb(op)) > 1 && OP_call(op)) return FALSE;
        
    } else { // if it is not branch op;
        
        BB *bb = OP_bb(op);
        // To get prev_bb by considering check split BB, complex!
        while (bb = BB_prev(bb)) {
            // if check split bb, find the last bb not in check chain,
            if (BB_chk_split(bb) && !BB_chk_split_head(OP_bb(op))
                && BB_chk_split(OP_bb(op))) {
		    OP *tmp;
		    FOR_ALL_BB_OPs_REV(bb, tmp) {
                        if (OP_end_group(tmp)) break;
		    }
		    if (tmp && OP_end_group(tmp)) break;
		    continue; 
	    }
            if (BB_length(bb) != 0) break;
        }
        
        if (bb) {
            OP *last_br=BB_branch_op(bb);
            
            if (last_br && !OP_chk(last_br)) { // last bb has branch
                if (!OP_cond_def(op)) return FALSE;
                if (!OP_cond_def(last_br)) return FALSE; // can't mov before branch
                if (!OP_end_group(last_br)) return FALSE;
                TN *p_opnd = OP_opnd(last_br, OP_PREDICATE_OPND);
                // in fact, this work can be done in Add_OP, check dependence,
                // at here, it will save time.
                for(OP *prev_op=OP_prev(last_br); prev_op; prev_op = OP_prev(prev_op)) {
                    if (OP_end_group(prev_op)) break;
                    if (OP_Defs_TN(prev_op, p_opnd)) {
                        return FALSE;
                    }                
                }
                if (!OP_has_disjoint_predicate(op, last_br)) return FALSE;
		ISA_REGISTER_CLASS cl = TN_register_class(p_opnd);
                REGISTER   reg = TN_register(p_opnd);
                if (OP_Defs_Reg(op, cl, reg)) return FALSE;
						
     //           if (BB_partial_bundle(bb)) return FALSE; // it means multi-branch , and nop-br can't support it, fix bug for vortex
	    }
        }
        // TODO:: you can check other..
    }
    return TRUE; 
}

//-------------------------------------------------------------
// BOOL Add_Predicate_Valid_BB(BB *bb, OP **barrier)
//   Determine whether bb can be added predicatei, 
//   return last branch op in barrier;
//-------------------------------------------------------------
BOOL Add_Predicate_Valid_BB(BB *bb, OP **barrier) 
{
    if (!IPFEC_Enable_Pre_Multi_Branch ||
        !LOCS_Enable_Bundle_Formation) return FALSE; //guarded by option
    
    if (!bb || !Multi_Branch_Valid_BB(bb)) return FALSE;
   
    BB *prev=bb;
   
    if (BB_Unique_Predecessor(bb) == NULL) return FALSE; // we can do duplicate later::TODO 
   
    while (prev = BB_prev(prev)) {
        if (BB_length(prev) != 0) break;
        if (BB_Unique_Predecessor(prev) == NULL) return FALSE; // we can do duplicate later::TODO
    }
    
    // Because it is called before generate recovery block, 
    // then chk split op can be passed
    if (prev == NULL) return FALSE;
    if (prev) {
        OP *last_br=BB_branch_op(prev);
        if (last_br == NULL || !OP_cond_def(last_br)) return FALSE;
        if (!OP_end_group(last_br) || !BB_scheduled(prev)) return FALSE;
        for(OP *prev_op=OP_prev(last_br); prev_op; prev_op = OP_prev(prev_op)) {
            if (OP_end_group(prev_op)) break;
            if (OP_Defs_TN(prev_op, OP_opnd(last_br, OP_PREDICATE_OPND))) {
               return FALSE;
            }                
            if (OP_xfer(prev_op) && !OP_chk(prev_op)) return FALSE;
        }
        // last branch op, we call it barrier because we will move op before it.
        *barrier = last_br; 
    } 

   
    // caculate BB valid ops' number
    OP *tmp;
    INT num=0;
    FOR_ALL_BB_OPs(bb, tmp) {
        if (OP_dummy(tmp) || OP_simulated(tmp) || OP_noop(tmp)) continue;
        if (OP_br(tmp) && !OP_chk(tmp)) continue;
        num++;
    }
       
    // profit heuristic
    if (BB_cycle(bb)>1 || num>=2 || num==0) return FALSE;
    
    if (BB_freq(prev) > 0  && BB_freq(bb)/BB_freq(prev) >= 0.5) 
        return TRUE; 
    
    return FALSE;
}

//-------------------------------------------------------------
// BOOL Add_Predicate_Valid_OP(OP *op, OP *barrier)
//   Determine whether op can be added predicator, 
//   use last branch op in barrier;
//-------------------------------------------------------------
BOOL Add_Predicate_Valid_OP(OP *op, OP *barrier) 
{
    // if branch and dummy op return true, exec br_cloop, br_ctop;
    if (OP_dummy(op) || OP_simulated(op) || OP_noop(op)) return TRUE;
    if (OP_xfer(op) && !OP_chk(op)) return TRUE;
    if (CGTARG_Is_OP_Check_Load(op)) return FALSE;
    if (!OP_has_predicate(op)) return FALSE;

    // non-branch op, if it has predicator and not true TN, it can't be add 
    // by new predicator, OP_cond_def is not meet our requirement 
    // change to TN_is_true_pred&has_predicate.
    if (OP_cond_def(op)) {return FALSE;}
    if (!TN_is_true_pred(OP_opnd(op,OP_PREDICATE_OPND))) 
       return FALSE;
    // No dependence with last cycle, reduce useless add predicate
    for (INT i =0; i<(OP_opnds(op)+OP_results(op)); i++) {
        
        TN *tn;
        if (i < OP_opnds(op))  tn = OP_opnd(op, i);
        else  tn = OP_result(op, i-OP_opnds(op));

	INT num=0;
        for(OP *prev_op=OP_prev(barrier); prev_op; prev_op = OP_prev(prev_op)) {
           num++;
           if (OP_end_group(prev_op)) break;

           //if it has no end group we should let it skip loop soon;
	   if (num > ISA_MAX_SLOTS * ISA_MAX_ISSUE_BUNDLES) break; 
           if (OP_Defs_TN(prev_op, tn)) {
               return FALSE;
	   }
           //for twolf. padding problem . but it is difficult to solve
           if (OP_memory(prev_op) && OP_code(op)==TOP_mov) return FALSE;
        }
    }	   
    
    // profit heuristic
    // for benefit for perlbmk forbid BB 4, don't distroy predict
    if (OP_code(op) == TOP_mov_t_br) return FALSE; 
    if (OP_memory(op)) return FALSE;
    if (OP_chk_a(op)) return FALSE;

    return TRUE;
}
//-------------------------------------------------------------
// Post_Multi_Branch
//   Bundling multiple branch ops into one cycle, in such way
//   we can save several cycles between branch op. We can use
//   MBB and BBB template to hold these ops.But not any branch 
//   op can be fit into one cycle, we need meet some special 
//   requirement. 
//     -br.call must be in last slot of pattern
//     -Any op can't jump to middle of bundle, that is branch
//      target must be begining of bundle.
//   Method:
//    for each bb in all region;
//     Find one BB which only hold branch ops and nop, and 
//     it has only one precedent bb, which next bb just is this bb
//
//     Then, find all ops(preops) in last cycle of its precedent bb.
//
//     Using preops and branch ops to sought a suitable pattern, 
//       if find, then change stop bit and redo bundling;
//       if not find, then next;
//   Assumption:
//     each bb has only one branch op;
//--------------------------------------------------------------
void Post_Multi_Branch(void)
{
     Set_Error_Phase("Multiple Branch");
     Start_Timer(T_Ipfec_Multi_Branch_CU);    
 
     if (Get_Trace(TP_A_MLBR, 0x01)) {
         fprintf(TFile, "--Begining of PU %d-----\n", Current_PU_Count());
     }
     for  (BB *bb= REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
         if (BB_length(bb)==0) continue;
         if (!OP_end_group(BB_last_op(bb)) && !BB_chk_split_head(bb)) continue;
         if (IPFEC_Query_Skiplist(mlbr_skip_bb, BB_id(bb), Current_PU_Count())) continue;


         // Find bb is valid for multi branch?
         if (!Multi_Branch_Valid_BB(bb)) continue; 
         
         // Find all ops in bb are valid for multi-branch
         // branch is ok, non-br should have no dependence with
         // last cycle; 
         OP *op;
         BOOL invalid_op = FALSE;
         INT valid_ops_num=0;
	 
	 OP *next_op=op; BB *next_bb=bb;  //don't change bb value, it is head of multi-branch
         for(op = BB_first_op(next_bb); op; op = next_op) {
             // for check split bb
             if (BB_chk_split_head(bb) && BB_next(next_bb) && 
	         BB_chk_split(BB_next(next_bb)) && 
		 BB_preds_len(BB_next(next_bb)) == 1 &&
		 op == BB_last_op(next_bb)) {
                 
	          next_bb = BB_next(next_bb);
                  next_op = BB_first_op(next_bb);	     
	     }  else {
		 next_op = OP_next(op);
	     }
             if (OP_dummy(op) || OP_simulated(op) || OP_noop(op)) continue;
             if (!Multi_Branch_Valid_OP(op)) {
                  invalid_op = TRUE;
                  break;
             }
             // OSP, bb has more than one pred, which means at least, one pred contains a br
             // br.call can not reside in the middle multiway branch 
             //   unless all the branches (including call) are predicated with disjoint predidates
             // So, if the op is call, it's still invalid
             // TODO: check the predidate of the br in BB_preds
	     else if ( BB_preds_len(bb) > 1 && OP_call(op) ) {
	          invalid_op = TRUE;
		  break;
	     }
	     else {
                  valid_ops_num++;
             }
         }
         if (invalid_op) continue;

         // skip too many valid ops for save compile time;
         if (valid_ops_num >= 4)  continue;
      
         BB *prev_bb = BB_prev(bb);
         while(prev_bb && BB_length(prev_bb) == 0) {
             prev_bb = BB_prev(prev_bb);
         }
         if (prev_bb == NULL) continue;
                           
         // construct ops for last cycle
         OPS ops,new_ops,dup_new_ops;
         OPS_Init(&ops);
         OPS_Init(&new_ops);
         OPS_Init(&dup_new_ops);
         BOOL valid_ops =TRUE;
         BOOL bundle_succeed = FALSE;
         if (OP_end_group(BB_last_op(prev_bb))) {
             BOOL meet_stop_bit = FALSE;
             INT stop_idx = 0;
             INT ops_len = 0;

             // TO get last cylce ops in previous bb 
             // NOTE: may include ops just before the last cycle, 
             // to meet a bundle boundary
             for(prev_bb; prev_bb!=NULL; prev_bb=BB_prev(prev_bb)) {
                 OP *cur_op = BB_last_op(prev_bb);
                 while(cur_op) {
                     if ( !OP_noop(cur_op) &&
                         (OP_dummy(cur_op) || OP_simulated(cur_op))) {
                         cur_op = OP_prev(cur_op);
                         continue;
                     }
                     if (OP_end_group(cur_op) && 
                         OPS_length(&ops) != 0) {
                         meet_stop_bit = TRUE;
                         if (ops_len % ISA_MAX_SLOTS == 0) break;
                     }
                     // push to ops
                     OP *add_op = cur_op;
                     cur_op = OP_prev(cur_op);
                     BB_Remove_Op(prev_bb,add_op);
                     OPS_Prepend_Op(&ops, add_op); 
                     add_op->bb = prev_bb; 
                     if (prev_bb != BB_prev(bb) && 
                        !BB_Is_Unique_Predecessor(BB_next(prev_bb),prev_bb)) {
                         valid_ops = FALSE;
                     }
                     ops_len += ISA_PACK_Inst_Words(OP_code(add_op));
                     
                     if (meet_stop_bit) {stop_idx++;}
                     if (meet_stop_bit && 
                         ops_len % ISA_MAX_SLOTS == 0) break;
                 }
                 if (meet_stop_bit && 
                     ops_len % ISA_MAX_SLOTS == 0) break;
                 Is_True(OPS_length(&ops)<=(ISA_MAX_SLOTS*ISA_MAX_ISSUE_BUNDLES), 
                          ("too many ops %d in this cycle", OPS_length(&ops)));                 
             }

             if (OPS_length(&ops) == 0) return;

             prev_bb = OP_bb(OPS_first(&ops));
             
             // To get new op, push them to new_ops;
             // use dup_new_ops to add new ops to other preds
             // Because check split bb, we will use op in next bbs.
             // We will use src_end_bb to keep the end of source bb.
             OP *next_op; BB *next_bb = bb; 
             for(op = BB_first_op(next_bb); op; op=next_op){  
                 // for check split bb
                 if (BB_chk_split_head(bb) && BB_next(next_bb) && 
	             BB_chk_split(BB_next(next_bb)) && 
		     BB_preds_len(BB_next(next_bb)) == 1 &&
		     op == BB_last_op(next_bb)) {
                     BB_Remove_Op(next_bb,op);
                     op->bb =  next_bb;
	             next_bb = BB_next(next_bb);
		     next_op = BB_first_op(next_bb);
		 } else {
	             next_op = OP_next(op);
                     BB_Remove_Op(next_bb,op);
                     op->bb =  next_bb;
		 }
		 
                 OPS_Append_Op(&new_ops,op);
                 if (OP_dummy(op) || OP_simulated(op) || OP_noop(op)) continue;
                 OPS_Append_Op(&dup_new_ops, Dup_OP(op));
             }
             
             // bb as begin of source bb, it as end of source bb
             BB *src_end_bb = next_bb; 

	     Is_True(OPS_length(&dup_new_ops) == valid_ops_num, ("get new ops is not correct!"));

             // trace file
             if (Get_Trace(TP_A_MLBR, 0x01)) {
                 fprintf(TFile, "op list in last cycle is:\n");
                 Print_OPS(&ops);
                 fprintf(TFile, "\nop add in cur cycle is in BB %d:\n", BB_id(bb));
                 Print_OPS(&new_ops);
                 fprintf(TFile, "\nstop index: %d\n", stop_idx);
                 fprintf(TFile, "\n");
             }

             // call micro-scheduler to find pattern for op list;
             if (valid_ops) {
                 // save level;
                 INT32 save = IPFEC_sched_care_machine;
                 IPFEC_sched_care_machine = Sched_care_bundle;
                 if (CGGRP_Bundle_OPS(&ops, &new_ops, stop_idx)) {
                    
                    // retarget those GOTO-kind predecessor BBs' target.
                    // use tmplst to backup bb's predecessor list because the original one 
                    // may be destroyed during retargetation
                    BBLIST *tmplst = NULL;
                    BBLIST *succ = NULL;                    
                    BB *fall_thru = BB_Fall_Thru_Successor(src_end_bb); 
                    //we should use src_end_bb instead of bb

                    if (fall_thru) succ = BB_Find_Succ(src_end_bb, fall_thru);

                    for (BBLIST *bblst = BB_preds(bb); bblst; bblst = BBLIST_next(bblst)) {
                        BBlist_Add_BB(&tmplst, BBLIST_item(bblst));
                    }

                    // keep all new_ops's successor, we will use it when dup all ops to 
                    // other BB which has goto branch to source bbs;
                    BBLIST *tmpsuc = NULL;
                    for (BB *tmp=bb; tmp != BB_next(src_end_bb) ; tmp = BB_next(tmp)) {
                       for (BBLIST *bblst = BB_succs(tmp); bblst; bblst = BBLIST_next(bblst)) {
                          float prob = 1.0;
                          if (BBLIST_item(bblst) != BB_next(tmp)) 
                              BBlist_Add_BB_with_Prob(&tmpsuc, BBLIST_item(bblst), prob*BBLIST_prob(bblst));
                          else 
                              prob = prob*BBLIST_prob(bblst);
                              
                       }
                    }

                    for (; tmplst; tmplst = BBLIST_next(tmplst)) {
                        BB* pred_bb = BBLIST_item(tmplst);
                        if (pred_bb != BB_prev(bb)) {
                            // copy ops to other predecessors.
                            OP *cur_op; 
                            FOR_ALL_OPS_OPs(&dup_new_ops,cur_op) {
                                // br.call can not be here unless we checked the predicates of the branches in preds.
                                Is_True ( !OP_call(cur_op), ("call can not be here!") );

                                BB_Insert_Op_Before(pred_bb, BB_branch_op(pred_bb), Dup_OP(cur_op)); 
                                // multi-branch in one BB fix bug in vpr 2.o BB 44 PU 36
                            }

                            // this branch op must jump to bb br.few;
                            BBLIST *succlst;
                            FOR_ALL_BBLIST_ITEMS (tmpsuc, succlst) {
                                Link_Pred_Succ_with_Prob(pred_bb, BBLIST_item(succlst), BBLIST_prob(succlst));
                            }
                            if (fall_thru && fall_thru!=BB_next(src_end_bb)) 
                                Link_Pred_Succ_with_Prob(pred_bb, fall_thru, BBLIST_prob(succ));
                            if (BB_next(src_end_bb)) BB_Retarget_Branch(pred_bb, bb, BB_next(src_end_bb));
                            SCHEDULER local_scheduler(pred_bb, FALSE);
                            local_scheduler.Schedule_BB();

                            if (Get_Trace(TP_A_MLBR, 0x02)) {
                                fprintf(TFile, "Mutli_entry BB %d in PU %d need resched\n", 
                                        BB_id(pred_bb), Current_PU_Count());
                                Print_BB(pred_bb);
                            }
                        }
                        
                    }

                     // Find new bundle , clean bb in which the branch op resides
		     for (BB *tmp = bb; tmp; tmp = BB_next(tmp)) {
                         BB_Remove_All(tmp); // ??can't remove all op!!!! TODO:: we need't do it 
                         BB_cycle(tmp)=0;
                         Set_BB_partial_bundle(tmp);
                         if (tmp == src_end_bb) break;
		     }

                     for (BB *p_bb=prev_bb; p_bb!=bb; p_bb = BB_next(p_bb)){
                         Is_True(p_bb!=NULL, ("partial bb is null!"));
                         Set_BB_partial_bundle(p_bb);
                     }
                     
                     if (Get_Trace(TP_A_MLBR, 0x01)) {
                         fprintf(TFile, "op list After Bundle:\n");
                         Print_OPS(&ops);
                         fprintf(TFile, "\n");
                     }
                     bundle_succeed = true;

		     if (Get_Trace(TP_A_MLBR, 0x02) && OPS_length(&dup_new_ops) >1) {
		         fprintf(TFile, "change multi-branch succeed in BB %d PU %d\n", 
		             BB_id(bb),
			     Current_PU_Count());
		     }
                }
                // recover msched level
                IPFEC_sched_care_machine = save;
             } //valid op
    
             // Insert op to bb from ops;
             BB *insert_bb = prev_bb;
             Is_True(insert_bb != NULL, ("You should know where to insert OPS"));
             for(OP *cur_op=OPS_first(&ops);cur_op;) {
                 OP *next_op = OP_next(cur_op);
                 if (bundle_succeed) 
                    if(!OP_bb(cur_op) || !OP_xfer(cur_op) ||OP_chk(cur_op) ) {
                        BB_Append_Op(insert_bb, cur_op);
                        cur_op = next_op;
                        continue;
                     }
                  insert_bb = OP_bb(cur_op);
                  BB_Append_Op(insert_bb, cur_op);
                  cur_op = next_op;
             }
             
             // if new_ops has op. it will be failed , reinsert op 
             for(OP *cur_op=OPS_first(&new_ops);cur_op;) {
                 OP *next_op = OP_next(cur_op);
                 insert_bb = OP_bb(cur_op);
                 BB_Append_Op(insert_bb, cur_op);
                 cur_op = next_op;
             }

             // clean dup_new_ops
             OPS_Remove_All(&dup_new_ops);
         }
     }
     Stop_Timer(T_Ipfec_Multi_Branch_CU);
}
///////////////////////////////////////////////////////////////////
//  Check_Cross_Boundary
//    Assumption: every patial bundle has only one predecessor,
//    which just its previous bb.
//
void Check_Cross_Boundary()
{
     BOOL partial_bundle_start = FALSE;
     BOOL valid_multi_branch = TRUE;
     BB *last_bb;
     BB_SET *partial_bbs;
    
     Set_Error_Phase("Multiple Branch");
     Start_Timer(T_Ipfec_Multi_Branch_CU);    
     Initialize_Memory();
     partial_bbs = BB_SET_Create_Empty(PU_BB_Count+2, MLBR_pool);

     // get last bb in code layout, how can I get it?
     for  (BB *bb= REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
        last_bb = bb;
     }
     for (BB *bb=last_bb; bb != NULL; bb = BB_prev(bb)) {
        if (!BB_partial_bundle(bb) &&
            !partial_bundle_start) continue;
   
        // start bundle must end of bundle 
        if (!partial_bundle_start &&
            (BB_length(bb) ==0 ||
            !OP_end_group(BB_last_op(bb)))
            ) {
            valid_multi_branch = FALSE;  
        }
        partial_bundle_start  = TRUE;
        if (BB_prev(bb) == NULL) {
            partial_bundle_start = FALSE;
        } else {
            if (BB_partial_bundle(bb)) {
                if (BB_partial_bundle(BB_prev(bb))) {
                    if (!BB_scheduled(bb) ||
                        !BB_Is_Unique_Predecessor(bb, BB_prev(bb))) {
                        valid_multi_branch = FALSE;
                    }
                    // If prev bb is another partial bundle
                    BB *prev_partial_bb = BB_prev(bb);
                    BOOL end_bundle = BB_last_op(bb) ?
                                 OP_end_group(BB_last_op(prev_partial_bb)) : FALSE;
                    if (BB_scheduled(prev_partial_bb) &&
                        end_bundle) {
                        DevWarn("two successor partial bundle BB %d and BB %d.", 
                                BB_id(bb),
                                BB_id(prev_partial_bb));
                        partial_bundle_start = FALSE;
                    }
                } else {
                    BOOL start_bundle = BB_first_op(bb) ? 
                             OP_start_bundle(BB_first_op(bb)): FALSE;
                    if (!start_bundle ||
                        !BB_scheduled(bb)) {
                        valid_multi_branch = FALSE;
                    }
                    // partial bundle end
                    partial_bundle_start = FALSE;
                }
            }else {
                valid_multi_branch = FALSE;
            }
        }
        // push to bb set 
        partial_bbs = BB_SET_Union1(partial_bbs, bb, MLBR_pool);
        // set rescheduler for invalid partial bundle;
        if (!partial_bundle_start && !valid_multi_branch) {
            BB *bb;
            FOR_ALL_BB_SET_members(partial_bbs, bb) {
                Reset_BB_scheduled(bb);
                Reset_BB_partial_bundle(bb);
            }
            valid_multi_branch = TRUE;
            if (Get_Trace(TP_A_MLBR, 0x01)) {
                fprintf(TFile, "You should check BB set:");
                BB_SET_Print(partial_bbs, TFile);         
                fprintf(TFile, "\n");
            }
        }
        if (!partial_bundle_start) {
            partial_bbs = BB_SET_ClearD(partial_bbs);
        }
    }
    Finalize_Memory();
    Stop_Timer(T_Ipfec_Multi_Branch_CU);
}
// call at after Global scheduler 
BOOL Create_Chance_For_MLBR(BB *target_bb) 
{
    OP *barrier=NULL;
    OP *tmp;   
    
    if (!Add_Predicate_Valid_BB(target_bb, &barrier)) return FALSE;
    
    FOR_ALL_BB_OPs(target_bb, tmp) 
        if (!Add_Predicate_Valid_OP(tmp, barrier))  return FALSE;

    // We can add it predicate to each op in target_bb exclude branch op
    if (barrier) {

        TN *tn=NULL;
        TN *predicate_tn = OP_opnd(barrier, OP_PREDICATE_OPND); 
       
        // find define cmp op for this branch predicate;
        
        DEF_KIND kind;
        OP *def_op = TN_Reaching_Value_At_Op(predicate_tn, barrier, &kind, TRUE);
        if (def_op && kind == VAL_KNOWN && OP_icmp(def_op) && 
           OP_Defs_TN(def_op, predicate_tn)) {
           // find compare instrution, get the disjoint tn
           tn = OP_result(def_op,0);
           if (tn == predicate_tn) tn = OP_result(def_op,1);
           if (tn == NULL) return FALSE;
        }

        if (tn) {
            
            if (Get_Trace(TP_A_MLBR, 0x02))  fprintf(TFile, "currret BB's op is :\n");
            
            //Change all ops predicate 
            FOR_ALL_BB_OPs(target_bb, tmp) {
                if (OP_dummy(tmp) || OP_simulated(tmp) || OP_noop(tmp)) continue;
                if (OP_xfer(tmp) && !OP_chk(tmp)) continue;

                // do change predicate!
                Set_OP_opnd(tmp, OP_PREDICATE_OPND, tn);
                Set_TN_is_global_reg(tn);
                if (Get_Trace(TP_A_MLBR, 0x02)) Print_OP_No_SrcLine(tmp);
            }
            if (Get_Trace(TP_A_MLBR, 0x02)) {                    
                fprintf(TFile, "change op use predicate tn: in BB %d PU %d\n", 
                        BB_id(target_bb),
                        Current_PU_Count());
                Print_OP_No_SrcLine(def_op);
                Print_OP_No_SrcLine(barrier);
            } 
        }  
    } 
    return FALSE; 
}
void Post_Multi_Branch_Collect(void) 
{
     Set_Error_Phase("Multiple Branch");
     Start_Timer(T_Ipfec_Multi_Branch_CU);    
     
     if (!IPFEC_Enable_Pre_Multi_Branch || 
         !LOCS_Enable_Bundle_Formation) return;

     if (PROCESSOR_Version != 2)  return; // only do it for Itanium2
     
     for  (BB *bb= REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
         if (BB_length(bb)==0) continue;
         if (IPFEC_Query_Skiplist(mlbr_skip_bb, BB_id(bb),
                   Current_PU_Count())) continue;

         Create_Chance_For_MLBR(bb);
     }
}
