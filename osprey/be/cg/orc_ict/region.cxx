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

//=============================================================================
//
// Module: region.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $ 
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/region.cxx,v $
//
//=============================================================================
#include <stdio.h>
#include "stdlib.h"
#include "bb.h"
#include <stack>
#include <vector>
#include <list>
#include "defs.h"
#include "cxx_memory.h"  
#include "region.h"
#include "interval_processor.h"
#include "vt_region.h"
#include "gra_live.h"
#include "cgtarget.h"
#include "region_bb_util.h"
#include "region_verify.h"
#include "tracing.h"
#include "ipfec_defs.h"
#include "global_cycles_finder.h"
#include "cg_flags.h"
#include "profile_util.h"
#include "tlog.h"
#include "freq.h"
#include "ipfec_options.h"

BB_MAP bb_node_map;
GLOBAL_CYCLE_VECTOR global_cycles;

//=============================================================================
//  See region.h for interface description.
//=============================================================================
void Create_BB_Node_Map(void) {
    bb_node_map = BB_MAP_Create();
}

void Delete_BB_Node_Map(void) {
    BB_MAP_Delete(bb_node_map);
}

//=============================================================================
//  Initialize_Temp_Rgn
//  Description:
//    Initalize the temp rgn struct,set all its field empty.
//
//=============================================================================
void Initialize_Temp_Rgn(TEMP_RGN& temp_rgn,MEM_POOL _m) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR temp_nodes(temp_alloc); 
    temp_rgn.nodes = temp_nodes;
    temp_rgn.main_entry = NULL;
    temp_rgn.main_exit  = NULL;
    temp_rgn.seed       = NULL;
    temp_rgn.bb_num = 0;
    temp_rgn.op_num = 0;
    temp_rgn.dup_bb_num = 0;
    temp_rgn.dup_op_num = 0;
    temp_rgn.exit_prob  = 0;
    temp_rgn.seed_freq  = 0;
}

NODE_VECTOR_ITER 
Find_In_Vector(REGIONAL_CFG_NODE *node,NODE_VECTOR& nodes) {
    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *n = *iter;
        
        Is_True(n != NULL,("Node n NULL in Find_In_Vector."));

        if (n == node) return iter;
    }
    
    return nodes.end();
}    

BB_VECTOR_ITER
Find_In_BB_Vector(BB *bb,BB_VECTOR& bbs) {
    for (BB_VECTOR_ITER iter = bbs.begin();iter != bbs.end();iter++) {
        BB *b = *iter;
        
        Is_True(b != NULL,("BB NULL in Find_In_BB_Vector."));

        if (b == bb) return iter;
    }
    
    return bbs.end();
} 

//=============================================================================
// 
// REGIONAL_CFG_NODE class
//
//=============================================================================

BOOL 
REGIONAL_CFG_NODE::Is_Loop_Head(void) {
    if (_home_r->Region_Type() == LOOP) {

	    if (_first_pred == NULL) {
		    return TRUE;
	    }
	}

	return FALSE;
}

BOOL 
REGIONAL_CFG_NODE::Is_Loop_Tail(void) {
    if (_home_r->Region_Type() == LOOP) {

        if (_first_succ == NULL) {
		    return TRUE;
	    }
	}

	return FALSE;
}

//=============================================================================
// 
// REGIONAL_CFG class
//
//=============================================================================

//=============================================================================
//  See region.h for interface description.
//============================================================================= 
REGIONAL_CFG_NODE*
REGIONAL_CFG::Add_Node(REGION *r) {
    REGIONAL_CFG_NODE *p = CXX_NEW(REGIONAL_CFG_NODE(r),&_m);

    Is_True(p != NULL,("New node failed in Add_Node."));
    
    r->Regional_Cfg_Node(p);
    Insert(p);
        
    return p;
}

REGIONAL_CFG_NODE*
REGIONAL_CFG::Add_Node(BB *bb) {
    REGIONAL_CFG_NODE *p = CXX_NEW(REGIONAL_CFG_NODE(bb),&_m);

    Is_True(p != NULL,("New node failed in Add_Node."));
    
    BB_MAP_Set(bb_node_map,bb,p);
    REGIONAL_CFG_NODE *pget = (REGIONAL_CFG_NODE*)BB_MAP_Get(bb_node_map,bb);
    Insert(p);
        
    return p;
}

//=============================================================================
//  See region.h for interface description.
//============================================================================= 
void 
REGIONAL_CFG::Del_Node(REGIONAL_CFG_NODE *v)
{
    // Maintain the exits and entries info may be too difficult,
    // perhaps here I should add a function to recompute it?
    BB *pred;
    REGIONAL_CFG_NODE *node1=v;                          
    REGION *rgn,*root1,*par_rgn;
    BOOL has_single_bb;

    while (v->First_Succ()) {
        Del_Edge(v->First_Succ());
    }      

    while (v->First_Pred()) {
        Del_Edge(v->First_Pred());
    } 

    if (v->Is_Entry())
        Remove_From_Entries(v);
    if (v->Is_Exit()){
    	if (!(v->Is_Region())) { 
    	    pred=v->BB_Node();
            node1 = Regional_Cfg_Node(pred);   
    	    rgn = node1->Home_Region();    
    	    root1 = rgn->Tree()->Root();
        }else{
            rgn=v->Region_Node();
            //here needs to add code that define prod_node,
            //but i sovle this by add in the head,@@@@@
            root1 = rgn->Tree()->Root();
            REGIONAL_CFG *cfg = rgn->Regional_Cfg();
            BB_VECTOR exits(&(cfg->_m));
            Collect_Exit_BBs(node1->Region_Node(), &exits);
            for(BB_VECTOR_ITER bb_iter=exits.begin(); bb_iter!=exits.end(); bb_iter++){
                 BB *exit_bb = *bb_iter;
                 BBLIST *succ_list;
                 if(BB_succs_len(exit_bb) == 0) {
                     has_single_bb= TRUE;
                     break;//if has no single bb,it is simple,not do any thing
                 }	
                       
            }
        }
        
            if(v->Is_Exit())
               Remove_From_Exits(v);
            if(!(v->Is_Region()) && BB_succs_len(pred)==0 || v->Is_Region() && has_single_bb==TRUE ){
                
                rgn = v->Home_Region();    
                while (rgn != root1){ 
                    node1= rgn->Regional_Cfg_Node();
                    par_rgn =node1->Home_Region();
                    REGIONAL_CFG *cfg = par_rgn->Regional_Cfg();
                    BB_VECTOR exits(&(cfg->_m));
                    
                    if(node1->Is_Region()) {
                       Collect_Exit_BBs(rgn, &exits);
                    } else {
                       exits.push_back(node1->BB_Node());
                    }
                    int has_other_exit_edge = FALSE;
                    for (BB_VECTOR_ITER bb_iter=exits.begin(); 
                        bb_iter!=exits.end(); bb_iter++){
                        BB *exit_bb = *bb_iter;
                        BBLIST *succ_list;
                        if(BB_succs_len(exit_bb) == 0 && exit_bb != pred) {
                        	has_other_exit_edge= TRUE;
                        	break;
                        }	
                        
                        FOR_ALL_BB_SUCCS(exit_bb, succ_list) { 
                            BB *succ_bb = BBLIST_item(succ_list);
                            if(! Region_Contains_BB(par_rgn, succ_bb)){
                                has_other_exit_edge = TRUE;
                                break;
                            }
                       }
                       if(has_other_exit_edge)
                          break;
                   }
                   
                   if(!has_other_exit_edge){
                      cfg->Remove_From_Exits(node1);
                   }
                   rgn=par_rgn;
             }
        }
    }            

   
    // Set the pointer pointed to this regional cfg node to NULL.
    if (v->Is_Region()) {
       (v->Region_Node())->Regional_Cfg_Node(NULL);
    } else {
        BB_MAP_Set(bb_node_map,v->BB_Node(),NULL);
    }
   
    Erase(v);
    CXX_DELETE(v,&_m);
} 

REGIONAL_CFG_EDGE*
REGIONAL_CFG::Find_Edge(REGIONAL_CFG_NODE *v,REGIONAL_CFG_NODE *w) {
    for (CFG_SUCC_EDGE_ITER iter(v);iter != 0;++iter) {
        REGIONAL_CFG_EDGE *e = *iter;
    
        if (e->Dest() == w) {
            return e;
        }
    }
    
    return NULL;
}       

void 
REGIONAL_CFG::Add_To_Exits(REGIONAL_CFG_NODE *node) {
    if (node->Is_Exit()) {
        return;
    }

    _exits.push_back(node);
    node->Is_Exit(TRUE);
}

void 
REGIONAL_CFG::Add_To_Entries(REGIONAL_CFG_NODE *node) {
    if (node->Is_Entry()) {
        return;
    }
    
    _entries.push_back(node);
    node->Is_Entry(TRUE);
}

void 
REGIONAL_CFG::Remove_From_Exits(REGIONAL_CFG_NODE *node) {
    for (NODE_VECTOR_ITER iter = _exits.begin();iter != _exits.end();iter++) {
        if ((*iter) == node) {
            _exits.erase(iter);
            
            break;
        }
    }

    node->Is_Exit(FALSE);
}

void 
REGIONAL_CFG::Remove_From_Entries(REGIONAL_CFG_NODE *node) {
    for (NODE_VECTOR_ITER iter = _entries.begin();
        iter != _entries.end();iter++) {
        if ((*iter) == node) {
            _entries.erase(iter);
            
            break;
        }
    }

    node->Is_Entry(FALSE);
}

//========================================================================
//
//  Find_MEME_Nodes
//  
//  -Input: SIZE:The size control for the found meme scope.
//          BAD_NODES:Nodes not suitable to be included,which may cause 
//          a region contains only single region node to be formed.
//  -Output:MEME_RGN,return a struct which contain info of the selected 
//          scope.
//  Description:
//   1 Find out a seed node first.
//   2 Extend from the seed node in forward direction,i.e,
//     select the most frequently executed succ of it;then in 
//     backward direction,select preds;This step make a hot
//     path.
//   3 Extend the hot path,but do not extend from the last node
//     of hot path in order to avoid unlimit extension in length.
//
//========================================================================
void 
REGIONAL_CFG::Find_MEME_Nodes(TEMP_RGN& meme_rgn,RGN_SIZE size,
                              NODE_VECTOR bad_nodes) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR meme_nodes(temp_alloc);
    REGIONAL_CFG_NODE *main_entry = NULL;
    REGIONAL_CFG_NODE *node       = NULL;
    INT32 bb_num = 0;
    INT32 op_num = 0;
    
    REGIONAL_CFG_NODE *seed = Find_Seed(bad_nodes);
    Is_True(seed != NULL,("Can not find out a seed in Find_MEME_Node.")); 
    
    if (!seed->Is_Region()) {
        BB *bb = seed->BB_Node();
        op_num = BB_length(bb);
    }

    bb_num++;    
    meme_nodes.push_back(seed);
    
    main_entry = seed;
    REGIONAL_CFG_NODE *last_node = seed;
    float seed_freq = Node_Freq(seed);    
    meme_rgn.seed      = seed;
    meme_rgn.seed_freq = seed_freq;

    float t  = 0.49;
    float ts = 0.49;
    INT32 max_bb_num = size.max_bb_num;
    INT32 max_op_num = size.max_op_num;
    INT32 max_length = (INT32)(max_bb_num/1.5);  //Max length of the hot path.
    
    REGIONAL_CFG_EDGE *succ = Select_Freq_Succ(seed,meme_nodes);
    while ((succ != NULL) && (bb_num < max_length) && (op_num < max_op_num) 
          && Succ_Suit(succ,seed_freq,t,ts)) {
        bb_num++;
        node = succ->Dest();
        meme_nodes.push_back(node);
        last_node = node;

        if (!node->Is_Region()) {
            op_num += BB_length(node->BB_Node());
        } 

        succ = Select_Freq_Succ(node,meme_nodes);
    }

    t  = 0.8;
    ts = 0.8; 
    
    REGIONAL_CFG_EDGE *pred = Select_Freq_Pred(seed,meme_nodes,bad_nodes);
    while ((pred != NULL) && (bb_num < max_length) && (op_num < max_op_num) 
          && Pred_Suit(pred,seed_freq,t,ts)) {
        bb_num++;
        node = pred->Src();
        meme_nodes.push_back(node);
        main_entry = node;
        
        if (!node->Is_Region()) {
            op_num += BB_length(node->BB_Node());
        } 
      
        pred = Select_Freq_Pred(node,meme_nodes,bad_nodes);
    }
    //----------------------------------------------------------------
    //Extend the seed path by selecting frequently connected node.
    //----------------------------------------------------------------
    node = Select_Freq_Connected_Node(meme_nodes,last_node);
    while ((node != NULL) && (bb_num < max_bb_num) && (op_num < max_op_num)) {
        meme_nodes.push_back(node);  
        bb_num++;

        if (!node->Is_Region()) {
            op_num += BB_length(node->BB_Node());
        } 
      
        node = Select_Freq_Connected_Node(meme_nodes,last_node);
    }

    meme_rgn.nodes = meme_nodes; 
    meme_rgn.main_entry = main_entry;
    meme_rgn.bb_num = bb_num;
    meme_rgn.op_num = op_num;
}     

//========================================================================
//
//  Find_SEME_Nodes
//  
//  -Input: PAR:Parameters used to control the seme region formation,including
//          region size,main exit probability and duplicate ratio,etc.
//          BAD_NODES:Nodes not suitable to be included,which may cause 
//          a region contains only single region node to be formed.
//  -Output:SEME_RGN:Struct used to record the selected seme scope.
//  Description:
//    1 Using thealgorithm which used in finding MEME nodes to find out a 
//      base scope. 
//    2 If there is no exit prob requirement,do selective cut until the 
//      remained part meets the duplicate ratio requirement to eliminate its
//      side entries through tail duplication.
//    3 If exit prob is required,decide the main exit and scope first then
//      do step 2.
//
//========================================================================
void 
REGIONAL_CFG::Find_SEME_Nodes(TEMP_RGN& seme_rgn,RGN_FORM_PAR par,
                              NODE_VECTOR bad_nodes) 
{
   
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin find base MEME nodes.\n");
    }
    
    RGN_SIZE max_size = par.size;
    float dup_ratio   = par.dup_ratio;
    float exit_prob   = par.exit_prob;
    INT32 max_dup_num = par.max_dup_num;

    Find_MEME_Nodes(seme_rgn,max_size,bad_nodes);  //Find base meme rgn first.
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish find base MEME nodes.\n");
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"Found MEME nodes:\n");
        Print_Node_Vector(seme_rgn.nodes);
        fprintf(TFile,"Number of found base MEME nodes %d \n",seme_rgn.bb_num);
    }

    //--------------------------------------------------------------------------    // TODO: we can add some heuristic for whether do tail duplication,
    // for example,if the seed frequency lower than 0.8,set duplicate
    // ratio to 1.0.Otherwise do not change the value.
    //--------------------------------------------------------------------------
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR side_entries(temp_alloc);

    if (exit_prob == 0) {  // If there are no side entries.
        if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
            fprintf(TFile,"Begin compute side entries of MEME nodes.\n");
        }

        Compute_Side_Entries(side_entries,seme_rgn.nodes,seme_rgn.main_entry); 

        if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
            fprintf(TFile,"The main entry is :");
            seme_rgn.main_entry->Print(TFile);
            fprintf(TFile,"Found side entries are:\n");
            Print_Node_Vector(side_entries);
        }
        
        if (!side_entries.empty()) {  
            
            if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                fprintf(TFile,"Before selective cut.\n");
            }                                              
               
            seme_rgn.main_exit = NULL; //Be sure the main exit is set to NULL.
            BOOL cut_succ = Do_Selective_Cut(seme_rgn.nodes,
                            seme_rgn.main_entry,NULL,par);
            
            if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                fprintf(TFile,"Finish selective cut.\n");
            } 
             
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                fprintf(TFile,"Nodes after selective cut.\n");
                Print_Node_Vector(seme_rgn.nodes);
            } 

            if (cut_succ) {   //If the selective cut suceeded.                
                if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                    fprintf(TFile,"Begin tail duplication.\n");
                }                                              
                
                //Do tail duplication.
                Tail_Duplicate(seme_rgn.nodes,seme_rgn.main_entry,
                seme_rgn.dup_bb_num,seme_rgn.dup_op_num,FALSE);

                if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                    fprintf(TFile,"Finish tail duplication.\n"); 
                }
            } else {
                FmtAssert(FALSE,("Do selective cut failed even no main exit is set.Algorithm of selective cut is dead."));
            }
        }
    } else {  //Has exit prob requirement,therefore must select main exit node.
        float max_weight = 0;
        NODE_VECTOR meme_nodes(temp_alloc);
        
        meme_nodes = seme_rgn.nodes;
        seme_rgn.nodes.clear();
        
        REGIONAL_CFG_NODE *main_entry = seme_rgn.main_entry;
        for (NODE_VECTOR_ITER iter = meme_nodes.begin();
            iter != meme_nodes.end();iter++) {
            NODE_VECTOR temp_seme(temp_alloc);
            REGIONAL_CFG_NODE *node = *iter;

            Compute_Scope_Based_On_Main_Exit(temp_seme,meme_nodes,node); 
               
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                
                if (node->Is_Region()) {
                    fprintf(TFile,"The scope when REGION Node_%d is main exit :\n",
                    (node->Region_Node())->Id());
                } else {
                    fprintf(TFile,"The scope when BB Node_%d is main exit :\n",
                            BB_id(node->BB_Node()));
                }

                Print_Node_Vector(temp_seme);
            }
                
            float prob = Compute_Completion_Prob(temp_seme,node,main_entry);
                
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                fprintf(TFile,"Completion prob :%f\n",prob);
            }
                
            if (prob > exit_prob) {
                NODE_VECTOR temp_dup(temp_alloc);
                Compute_Nodes_To_Be_Duplicated(temp_dup,temp_seme,main_entry);
                float ratio  = Compute_Duplicate_Ratio(temp_seme,
                               temp_dup,max_size.max_bb_num,max_dup_num);
                
                if (ratio > dup_ratio) {
                    if (!Do_Selective_Cut(temp_seme,main_entry,node,par)) {
                        continue; //If selective cut failed,it is not a candidate main exit. 
                    }
                }
                    
                float weight = Compute_Nodes_Weight(temp_seme);
                if (weight > max_weight) {  
                    seme_rgn.nodes = temp_seme;
                    seme_rgn.main_exit = node;
                    seme_rgn.exit_prob = prob;
                    max_weight = weight;
                }
            }
        }
        
        if (!(seme_rgn.nodes).empty()) {
            
            side_entries.clear();
            
            if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                fprintf(TFile,"Begin compute side entries nodes of MEME nodes.\n");
            }

            Compute_Side_Entries(side_entries,seme_rgn.nodes,
            seme_rgn.main_entry);

            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                fprintf(TFile,"The main entry is :\n");
                main_entry->Print(TFile);
                fprintf(TFile,"Found side entries are:\n");
                Print_Node_Vector(side_entries);
            }

            if (!side_entries.empty()) {
        
                if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                    fprintf(TFile,"Begin tail duplication.\n");
                }
                
                //Do tail duplication. 
                Tail_Duplicate(seme_rgn.nodes,seme_rgn.main_entry,
                               seme_rgn.dup_bb_num,
                               seme_rgn.dup_op_num,FALSE); 

                if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                    fprintf(TFile,"Finished tail duplication.\n");
                } 
            } 
        } else {
            FmtAssert(FALSE,("The seme nodes is empty!This may cause error!")); 
        } 
    }
    //-------------------------------------------------------------------------     //  Recompute the bb num and op num,for in compute scope based on main exit,
    //  we did not maintain the BB and OP number info.
    //-------------------------------------------------------------------------
    INT32 bb_num = 0;
    INT32 op_num = 0;
    for (NODE_VECTOR_ITER iter = (seme_rgn.nodes).begin();
        iter != (seme_rgn.nodes).end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in find SEME nodes when count bb num."));
        if (!node->Is_Region()) {
            bb_num ++;
            op_num += BB_length(node->BB_Node());
        }
    }
    //-------------------------------------------------------------------------
    // Reset the bb num and op num.When finding meme nodes,they are set but now
    // they are invalid.
    //-------------------------------------------------------------------------
    seme_rgn.bb_num = bb_num;
    seme_rgn.op_num = op_num;
}

//========================================================================
//
//  Compute_Nodes_Weight
//   - Input:
//       nodes:the node vector whose weight to be computed
//  Description:
//   This is a heuristic function for region formation,here we just use
//   the number of nodes and number of operations to denote the weight.
//  
//========================================================================     
float 
REGIONAL_CFG::Compute_Nodes_Weight(NODE_VECTOR nodes) {
    float weight = 0.0;
    
    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;

        Is_True(node != NULL,("Node is NULL in compute nodes weight!"));
        
        float freq = Node_Freq(node);
        if (!node->Is_Region()) {
            //Freq plus 1.0 to avoid weight being zero when all node's freq
            //is zero.And sometimes the BB length is zero,how to deal with it?
            weight += (freq + 1.0)*(BB_length(node->BB_Node()) + 1.0);
        } else {
            //-----------------------------------------------------------------
            // The region node should  have lower weight value
            // than the BBs.
            //-----------------------------------------------------------------
            weight += freq*5*0.5;  //Deem it as a BB with 2.5 ops.
        }
    }   

    return weight;
}

//=============================================================================
//  Is_Unwanted_Node 
//  - Input:
//      node: A regional cfg node to be checked.
//  Description:
//    Check the regional cfg node to see whether it contains some 
//    special type of BBs,such as exit basic block,switch basic 
//    block and its targets,which may be difficult for tail duplication.
//
//=============================================================================
BOOL 
REGIONAL_CFG::Is_Unwanted_Node(REGIONAL_CFG_NODE *node) {
    //--------------------------------------------------------
    //Check whether it is a tail or head node of loop region.
    //--------------------------------------------------------
    if (node->Is_Loop_Head() || node->Is_Loop_Tail()) {
        return TRUE;
    }

    if (((node->Home_Region())->Region_Type() == LOOP) && node->Is_Exit()) {
        return TRUE;// Do not duplicate loop exit for this may cause loop has more than one tails.
    }

    if (node->Is_Region()) {
	    
        if ((node->Region_Node())->Region_Type() == LOOP) {
	    return TRUE; //Do not duplicate loop region node.
        }//For aggressive test first.Debug to make sure the correctness.
        
        BB_VECTOR bbs;
        Compute_BBs_In_Region_Node(bbs,node);

        for (BB_VECTOR_ITER bb_iter = bbs.begin();
            bb_iter != bbs.end();bb_iter++) {
            BB *bb = *bb_iter;
            //----------------------------------------------------------------  
            // Now the exit BB is checked as unwanted node.Switch BB,call BB 
            // will also be checked as unwanted BBs in tail duplication later.
            //----------------------------------------------------------------
            if (BB_Not_Suit_Duplicate(bb)) {
                return TRUE;
            } 
              
            //TODO:Should add whether the BB is a indirect or vargoto source
            //or target BB here. 
            //Better the check can be written as a small function.
        }
    } else {
        BB *bb = node->BB_Node();
        
        if (BB_Not_Suit_Duplicate(bb)) { //Exit BB is unwanted.
            return TRUE;          
        } 
    }
	return FALSE;
}


BOOL
REGIONAL_CFG::BB_Not_Suit_Duplicate(BB *bb) {
    if (BB_exit(bb)) { //BB is a global exit BB.
	    return TRUE;
    } 

    OP *br = BB_branch_op(bb); // Get the last OP and the number of successors.
    INT nsuccs = BBlist_Len(BB_succs(bb));
    if (br == NULL) {
        if (nsuccs <= 1) {
            return FALSE;
        } else {
            Is_True(FALSE,("The BB %d has more than one succ but no br op!",BB_id(bb)));
        }
    }

    INT32 tfirst,tcount;
    CGTARG_Branch_Info(br, &tfirst, &tcount);
    
    //If the bb is a indirect GOTO or VARGOTO return TRUE.
    if(tcount==0) {
        return TRUE;
    } 
	
    if (nsuccs > 2) {
        return TRUE;
    }
	
    //Do not duplicate target BB of a indirect or vargoto BB.
    BBLIST *preds;
    FOR_ALL_BB_PREDS(bb,preds) {
        BB *pred = BBLIST_item(preds);
        br = BB_branch_op(pred); // Get the last OP and the number of successors.
        nsuccs = BBlist_Len(BB_succs(pred));
	if (br == NULL) {
	    if (nsuccs <= 1) {
	         return FALSE;
            } else {
	         Is_True(FALSE,("The BB %d has more than one succ but no br op!",BB_id(pred)));
            }
        }
        CGTARG_Branch_Info(br, &tfirst, &tcount);
    
        //If the bb is a indirect GOTO or VARGOTO return TRUE.
        if(tcount==0) {
            return TRUE;
        } 
	
        if (nsuccs > 2) {
            return TRUE;
        }
    }	

    return FALSE;
}

//=============================================================================
//
//  Do_Selective_Add
//  - InputOutput:NODES:The node set to be processed.Return a node set with 
//    some selected nodes inserted.
//    LEAFNODES:The dangling nodes,when nodes inserted,the leafnode set is 
//    updated.   
//  - Input: DUPLICATE:Nodes should be duplicated.
//  - Output:Return a value indicate the number of inserted nodes,if no node 
//    inserted,return -1.
//  Description:
//    When the selected nodes' duplicate ratio can not satisfy the 
//    requirement and the cut nodes is too much,try to add some nodes 
//    in order to eliminate side entry.This method may add some nodes
//    which are not frequently executed but can give a better region
//    size and shape.
//    For a node not in nodes,if all its preds are not in duplicate node 
//    set,and all its succs in nodes,add it to nodes.For node with unique
//    succ and its succ with unique pred,we just deem the two nodes as
//    a single node and add all of them in.If added nodes will make the
//    main completion prob lower than required value,stop adding them.
//
//=============================================================================
INT32
REGIONAL_CFG::Do_Selective_Add(NODE_VECTOR& nodes,NODE_VECTOR duplicate,
              NODE_VECTOR& leaf_nodes,REGIONAL_CFG_NODE *main_exit,
              REGIONAL_CFG_NODE *main_entry,INT32 op_num,RGN_FORM_PAR par) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR exits(temp_alloc);

    INT32 max_bb_num = par.size.max_bb_num;
    INT32 max_op_num = par.size.max_op_num;
    float exit_prob  = par.exit_prob;
    INT32 added_op_num = 0;

    Compute_Exits(nodes,exits);

    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in Do Selective Add when iterate all nodes."));
        if ((node != main_exit)&&
		Find_In_Vector(node,duplicate) == duplicate.end()) {
	    //Do not add main exit's succ.
        
            for (CFG_SUCC_NODE_ITER succ_iter(node);
                succ_iter != 0;++succ_iter) {
                REGIONAL_CFG_NODE *succ = *succ_iter;
                            
                if (Find_In_Vector(succ,nodes) == nodes.end()) {
                    BOOL not_side_entry = TRUE;

                    for (CFG_PRED_NODE_ITER pred_iter(succ);
                        pred_iter != 0;++pred_iter) {
                        REGIONAL_CFG_NODE *pred = *pred_iter;

                        if (Find_In_Vector(pred,nodes) == nodes.end()) {
                            not_side_entry = FALSE;
                        }
                    }

                    if (not_side_entry) {
                        BOOL cand = TRUE;
                        if (succ->First_Succ() == NULL) continue;
                        
                        NODE_VECTOR single_path(temp_alloc);
                        single_path.push_back(succ);

                        if (!succ->Is_Region()) {
                            added_op_num += BB_length(succ->BB_Node());
                        }

                        for (CFG_SUCC_NODE_ITER nsucc_iter(succ);
                            nsucc_iter != 0;++nsucc_iter) {
                            REGIONAL_CFG_NODE *end = *nsucc_iter;
                        
                            if (Find_In_Vector(end,nodes) == nodes.end()) {
                                if ((succ->Unique_Succ() == end)
                                   &&(end->Unique_Pred() == succ)) {
                                 //---------------------------------------------
                                 // Continue this path until not single succ &
                                 // single pred,check whether the number of
                                 // nodes in single path not exceed
                                 // the region size and then add all of them in
                                 // .Otherwise,cand = FALSE and continue.
                                 //--------------------------------------------
                                 single_path.push_back(end);

                                 if (!end->Is_Region()) {
                                      added_op_num += BB_length(end->BB_Node());
                                 }

                                 while (end) {
                                 //-------------------------------------------- 
                                 //If that is a sinlge path,permit only single
                                 //succ and it is in the nodes vector.
                                 //---------------------------------------------
                                      Is_True(end != NULL,("The end node is NULL in Do Selective Add!\n"));

                                       end = end->Unique_Succ();
                                        
                                       if (end) {
                                            if (Find_In_Vector(end,nodes) != nodes.end()) {
                                                
                                                break;
                                            } else {
                                                single_path.push_back(end);

                                                if (!end->Is_Region()) {
                                                    added_op_num += BB_length(end->BB_Node());
                                                }
                                            }
                                        } else {
                                            cand = FALSE;
                                        } 
                                    }
                                } else {
                                    cand = FALSE;
                                }
                            } 

                            if (cand) {
                            //-------------------------------------------------
                            // If required exit prob is not zero,check whether 
                            // the completion prob is changed.
                            //-------------------------------------------------
                            if (!exit_prob == 0) {
                                 if ((end != main_exit)&&
					 Find_In_Vector(end,exits) != exits.end()) {
                                 //---------------------------------------------
                                 // Update the completion prob of main exit 
                                 // now very time recompute the completion
                                 // prob is not that good,try to update the
                                 // completion prob in a more efficient way.
                                 //--------------------------------------------
                                 float prob = Compute_Completion_Prob(nodes,
                                              main_exit,main_entry);

                                        if (prob < exit_prob) {
                                            cand = FALSE;    
                                        }
                                    }
                                }
                            } else {
                                break;
                            } 
                        }

                        if (cand) {
                            INT32 size = single_path.size()+nodes.size();
                            op_num += added_op_num;
                            if ((size < max_bb_num) && (op_num < max_op_num)) {//Check size limitaion.
                                
                                for (NODE_VECTOR_ITER path_iter = single_path.begin();path_iter != single_path.end();path_iter++) {
                                    REGIONAL_CFG_NODE *path_node = *path_iter; 
                                    nodes.push_back(path_node);
                                    if (!path_node->Is_Region()) {
                                        added_op_num += BB_length(path_node->BB_Node());
                                    }    

                                    for (CFG_PRED_NODE_ITER pred_node_iter(path_node);pred_node_iter != 0;++pred_node_iter) {
                                        REGIONAL_CFG_NODE *pred_node = *pred_node_iter;
                                        NODE_VECTOR_ITER leaf = Find_In_Vector(pred_node,leaf_nodes);
                                        
                                        if (leaf != leaf_nodes.end()) {
                                            leaf_nodes.erase(leaf);
                                        }
                                    }
                                }

                                return added_op_num; //Return the number of ops added.
                            }
                        }
                    }
                }
            }
        }
    }
    
    return -1; //If failed to add any BB,return -1.
}

//=====================================================================
//
//  Do_Selective_Cut
//
//  Function : REGIONAL_CFG::Do_Selective_Cut
//  Input : 
//     - nodes :  The node set to be selectively cut.It is a reference 
//                parameter.
//     - main_entry : Main entry node of the node set nodes.
//     - main_exit  : The main exit node of node set nodes,it can 
//       not be cut from the node set for it is a main exit.
//     - par : Region formation parameter.It is a RGN_FORM_PAR struct.          
//  Output : 
//     If return value is TRUE, the returned nodes is a node set which 
//     has been cut.If the returned BOOL value is FALSE,no way can
//     be found to cut the nodes for meeting the duplicate ratio
//     requirement on the condition that main exit can not be removed.
//  Description:
//     According to duplicate ratio requirement,
//     1 If user perfer to region size and shape but duplicate ratio 
//       is low and region size still does not reach the max size
//       requirement,try to selectivly add side entry's pred.To step 3.
//     2 If no node can be(or not prefer) added,try to find a least
//       frequently executed leafnode and erase it.
//     3 Then compute duplicate ratio to see whether it meets the 
//       requirement.If not,repeat previous steps.
//
//=====================================================================
BOOL 
REGIONAL_CFG::Do_Selective_Cut(NODE_VECTOR& nodes,REGIONAL_CFG_NODE *main_entry,
                               REGIONAL_CFG_NODE *main_exit,RGN_FORM_PAR par) {
    typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>   NODE_LIST;
    typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>   NODE_QUEUE;
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR dup(temp_alloc);

    INT32 node_nums = nodes.size();
    INT32 op_nums   = Compute_Num_Of_Ops(nodes,FALSE);
    
    INT32 max_bb_num  = par.size.max_bb_num;
    INT32 max_op_num  = par.size.max_op_num;
    INT32 max_dup_num = par.max_dup_num;
    float exit_prob = par.exit_prob;
    float dup_ratio = par.dup_ratio;

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin compute nodes to be duplicated.\n");
    }

    Compute_Nodes_To_Be_Duplicated(dup,nodes,main_entry);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"BBs should be duplicated are:\n");
        Print_Node_Vector(dup);
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish compute nodes to be duplicated.\n");
    }
    
    NODE_VECTOR leaf_nodes(temp_alloc);
    //-----------------------------------------------------
    // Find out leaf nodes which is not a main exit first.
    //-----------------------------------------------------
    for (NODE_VECTOR_ITER iter = dup.begin();iter != dup.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        if ((node->First_Succ() == NULL)&&(node != main_exit)) {
            leaf_nodes.push_back(node);
        } else {
            BOOL is_leaf = TRUE;
            
            for (CFG_SUCC_NODE_ITER succ_iter(node);
                succ_iter != 0;++succ_iter) {
                if (Find_In_Vector((*succ_iter),nodes) != nodes.end()) {
                    is_leaf = FALSE;
                }
            }

            if ((is_leaf)&&(node != main_exit)) {
                leaf_nodes.push_back((*iter));
            }
        }
    }    
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The leaf nodes are:");
        Print_Node_Vector(leaf_nodes);
    }

    NODE_VECTOR temp_dup(temp_alloc);
    Compute_Nodes_To_Be_Duplicated(temp_dup,nodes,main_entry);
    while (!(Compute_Duplicate_Ratio(nodes,temp_dup,max_bb_num,max_dup_num) <= dup_ratio)) {

        float min_freq = 0.0;
        REGIONAL_CFG_NODE *to_be_cutted = NULL;
        INT32 added = -1;
        node_nums = nodes.size();

        if ((node_nums < max_bb_num) && (op_nums < max_op_num)) {
            added = Do_Selective_Add(nodes,temp_dup,leaf_nodes,
                    main_exit,main_entry,op_nums,par);
            op_nums += added;
        } 
        
        if (added == -1) {
        
            for (NODE_VECTOR_ITER iter = leaf_nodes.begin();
                iter != leaf_nodes.end();iter++) {
          
                if (Is_Unwanted_Node(*iter)) {
                    to_be_cutted = *iter;

                    break;
                }

                float freq = Node_Freq(*iter);        
                if ((freq < min_freq)||(min_freq == 0.0)) {
                    to_be_cutted = *iter;
                    min_freq     = freq;
                }
            }    
         
            if (to_be_cutted != NULL) {  // Delete the node from nodes.
                for (NODE_VECTOR_ITER iter = nodes.begin();
                    iter != nodes.end();iter++) {
                    if ((*iter) == to_be_cutted) {
                        nodes.erase(iter);
                                               
                        if (!to_be_cutted->Is_Region()) {
                            op_nums = op_nums - BB_length(to_be_cutted->BB_Node());
                        }

                        break;
                    }
                }
                //-------------------------   
                // update leaf_nodes.
                //-------------------------
                for (NODE_VECTOR_ITER iter = leaf_nodes.begin();
                    iter != leaf_nodes.end();iter++) {
                    if ((*iter) == to_be_cutted) {
                        leaf_nodes.erase(iter);  // Remove it from leaf nodes.
                        
                        break;
                    }
                }
            
                for (CFG_PRED_NODE_ITER pred_iter(to_be_cutted);
                    pred_iter != 0;++pred_iter) {
                    REGIONAL_CFG_NODE *pred = *pred_iter;

                    Is_True(pred != NULL,("Pred node is NULL in do selective cut.\n")); 
                    
                    //--------------------------------------------------------  
                    // If the pred node in the selected node set,check whether
                    // it is a leaf node.We do not erase the node from dup and
                    // use pred because we believe there are no cycles in the
                    // reginal cfg.
                    //---------------------------------------------------------
                    if (Find_In_Vector(pred,temp_dup) != temp_dup.end()) {
                        BOOL is_leaf = TRUE; 

                        for (CFG_SUCC_NODE_ITER succ_iter(pred);
                             succ_iter != 0;++succ_iter) {
                            if (Find_In_Vector((*succ_iter),nodes) != nodes.end()) {
                                is_leaf = FALSE;
                    
                                break;
                            }
                        }

                        if ((is_leaf)&&(pred != main_exit)) {
                            leaf_nodes.push_back(pred);
                        }
                    }   
                }    
             
                if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                    fprintf(TFile,"The leaf nodes are:");
                    Print_Node_Vector(leaf_nodes);
                }
            } else {
                return FALSE;
            }
        }

        temp_dup.clear();
        //
        //This step may be optimized,if the selective add is not done,
        //nodes to be duplicated reduce only one.
        //
        Compute_Nodes_To_Be_Duplicated(temp_dup,nodes,main_entry);
    }    
   
    return TRUE;
}    

//========================================================================
//
//  Compute_Side_Entries
//  
//  Description:
//   Find out all nodes in the meme node set which has at least one pred
//   not in the meme node set.
//
//======================================================================== 
void
REGIONAL_CFG::Compute_Side_Entries(NODE_VECTOR& side_entries,NODE_VECTOR nodes,
              REGIONAL_CFG_NODE *main_entry) {
       
    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        
        Is_True(node != NULL,("Node is NULL in compute side entries.\n"));

        if (node != main_entry) {
        
            for (CFG_PRED_NODE_ITER pred_iter(node);
                 pred_iter != 0;++pred_iter) {
                
                if (Find_In_Vector((*pred_iter),nodes) == nodes.end()) {
                    side_entries.push_back(node);
                    break;
                }
            }
        }    
    }
}        

//=====================================================================
//  See region.h for interface description.
//=====================================================================
void 
REGIONAL_CFG::Compute_Exits(NODE_VECTOR nodes,NODE_VECTOR& exits) {
    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end(); iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in compute exits.\n"));

        if (node->Is_Exit()) {
            exits.push_back(node);
        } else {
            for (CFG_SUCC_NODE_ITER succ_iter(node);
                succ_iter != 0;++succ_iter) {
                REGIONAL_CFG_NODE *succ = *succ_iter;

                if (Find_In_Vector(succ,nodes) == nodes.end()) {
                    exits.push_back(node);
                    break;
                }
            }
        }
    }

}

//=====================================================================
//
//  Compute_Completion_Prob
//  - Input:
//       nodes:A node set selected for seme rgn formation.
//       exit:The main exit node.
//       main_entry:The node in node set without any pred in node set.
//  - Output:
//       Return a float value,which is the completion prob of
//       the exit node.
//  Description:
//    In order to precisely compute the exit probability,we must 
//    compute the nodes to be duplicated.We must figure out every
//    exit node's exit frequency after tail duplication.
//    Then compute the total freq and main exit's exit freq.Return
//    the ratio.
//=====================================================================
float 
REGIONAL_CFG::Compute_Completion_Prob(NODE_VECTOR nodes,REGIONAL_CFG_NODE *exit,
                                      REGIONAL_CFG_NODE *main_entry) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR exits(temp_alloc);

    if (nodes.size() == 1) { //If only one node in node set.
        float prob = 1.0;
        return prob;
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The exit nodes are:\n");
    }
    
    Compute_Exits(nodes,exits);

    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        Print_Node_Vector(exits);
    }
    
    NODE_VECTOR dup(temp_alloc);
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The nodes to be duplicated are:\n");
    }

    Compute_Nodes_To_Be_Duplicated(dup,nodes,main_entry);

    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        Print_Node_Vector(dup);
    }

    PROF_ALLOC prof_alloc(&_m);
    NODE_PROF_VECTOR node_profs(prof_alloc);//Define edge freq vector here.
    
    Compute_Node_Prof_Info(nodes,dup,main_entry,node_profs);
    //----------------------------------------------------------   
    // Compute the total exit probability of the node set.
    //----------------------------------------------------------
    float exit_freq = 0.0;
    Is_True(!exits.empty(),("No exit node of this node set,is it possible?"));
    for (NODE_VECTOR_ITER iter = exits.begin();iter != exits.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in compute completion prob.\n"));
        
        if (node->First_Succ() != NULL) {

            if (Find_In_Vector(node,dup) != dup.end()) {

                for (NODE_PROF_VECTOR_ITER prof_iter = node_profs.begin();
                    prof_iter != node_profs.end();prof_iter++) {
                    NODE_PROF p = *prof_iter;        
            
                    if ((p.src == node) &&
			    Find_In_Vector(p.dest,nodes) == nodes.end()) {
                        exit_freq += p.freq;
                    }
                }
            } else {
                for (CFG_SUCC_EDGE_ITER succ_iter(node);
                    succ_iter != 0;++succ_iter) {
                    REGIONAL_CFG_NODE *succ = (*succ_iter)->Dest();
            
                    if (Find_In_Vector(succ,nodes) == nodes.end()) {
                        exit_freq += (*succ_iter)->Freq();
                    }
                }
            } 
        } else if (node->First_Pred() != NULL){

            if (Find_In_Vector(node,dup) != dup.end()) {

                for (NODE_PROF_VECTOR_ITER prof_iter = node_profs.begin();
                    prof_iter != node_profs.end();prof_iter++) {
                    NODE_PROF p = *prof_iter;        

                    if (p.dest == node) {
                        exit_freq += p.freq;
                    }
                }
            } else {
                //--------------------------------------------------------------
                // If it has any pred has been duplicated,it can not be a node 
                // not in dup,therefore,it can just use the old freq info.
                // Warning:If there is only one node and it has no succs,this
                // is a bug here.
                //--------------------------------------------------------------
                for (CFG_PRED_EDGE_ITER pred_iter(node);
                    pred_iter != 0;++pred_iter) {
                    REGIONAL_CFG_NODE *pred = (*pred_iter)->Src();
                
                    if (Find_In_Vector(pred,nodes) != nodes.end()) {
                        exit_freq += (*pred_iter)->Freq();
                    }
                }
            }
        } else {
            FmtAssert(FALSE,("An exit without pred and succ will not be dealt with"));
        }
    } 

    float main_exit_freq = 0.0;    
    //-------------------------------------------
    // Compute the main exit's exit probability.
    //-------------------------------------------
    if (exit->First_Succ() != NULL) {
        if (Find_In_Vector(exit,dup) != dup.end()) {

            for (NODE_PROF_VECTOR_ITER prof_iter = node_profs.begin();
                prof_iter != node_profs.end();prof_iter++) {
                NODE_PROF p = *prof_iter;       

                if ((p.src == exit) && (Find_In_Vector(p.dest,nodes) == nodes.end())) {
                    main_exit_freq += p.freq;
                }
            }
        } else {
            for (CFG_SUCC_EDGE_ITER succ_iter(exit);
                succ_iter != 0;++succ_iter) {
                REGIONAL_CFG_NODE *succ = (*succ_iter)->Dest();
                main_exit_freq += (*succ_iter)->Freq();
            }
        }
    } else if (exit->First_Pred() != NULL) {
        if (Find_In_Vector(exit,dup) != dup.end()) {
            
            for (NODE_PROF_VECTOR_ITER prof_iter = node_profs.begin();
                prof_iter != node_profs.end();prof_iter++) {
                NODE_PROF p = *prof_iter;        

                if (p.dest == exit) {
                    main_exit_freq += p.freq;
                }
            }
        } else {
            for (CFG_PRED_EDGE_ITER pred_iter(exit);
                pred_iter != 0;++pred_iter) {
                REGIONAL_CFG_NODE *pred = (*pred_iter)->Src();

                if (Find_In_Vector(pred,nodes) != nodes.end()) {
                    main_exit_freq += (*pred_iter)->Freq();
                }
            }
        }
    } else {
        FmtAssert(FALSE,("An exit without pred and succ will not be dealt with"));
    }

    float prob = 0.0;
    if (exit_freq == 0.0) {
        DevWarn("Exit prob is zero for main exit %d.\n",exit->Id());
        prob = 1.0;
        return prob;
    }
    prob = main_exit_freq/exit_freq;
    return prob;
}

//========================================================================
//
//  Compute_Scope_Based_On_Main_Exit
//  
//  -Input:MEME_NODES:Selected base meme nodes.
//         EXIT:Tried main exit node.
//  -Output:TEMP_SEME:When using exit as main exit node,the remaining part.
//  Description:
//   1 Mark the exit node's all successors from the meme_nodes.
//   2 For every node in the temp_seme,if all its preds been marked,mark it.
//   3 Erase all nodes marked from temp_seme and return the remained set.
//
//=========================================================================
void
REGIONAL_CFG::Compute_Scope_Based_On_Main_Exit(NODE_VECTOR& scope,
              NODE_VECTOR nodes,REGIONAL_CFG_NODE *exit) {
    typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>   NODE_LIST;
    typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>   NODE_QUEUE;
    
    NODE_ALLOC temp_alloc(&_m);
    NODE_LIST  temp_list(temp_alloc);
    NODE_QUEUE queue(temp_list);

    BS *cutted  = BS_Create_Empty(2+_seq_num, &_m);
    BS *visited = BS_Create_Empty(2+_seq_num,&_m);

    //----------------------------------------------------
    // All succs of the main exit cutted.
    //----------------------------------------------------
    for (CFG_SUCC_NODE_ITER iter(exit);iter != 0;++iter) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in compute scope based on main exit."));
        BS_Union1D(cutted, node->Id(), &_m);
        queue.push(node);
    }
    //-----------------------------------------------------------
    // Traverse the succs in breadth first order.For those node 
    // with all preds cutted,being cut too.
    //-----------------------------------------------------------
    while (!queue.empty()) {
        REGIONAL_CFG_NODE *node = queue.front();
        Is_True(node != NULL,("Node is NULL in compute scope based on main exit."));
        queue.pop();
                
        for (CFG_SUCC_NODE_ITER succ_iter(node);succ_iter != 0;++succ_iter) { 
            REGIONAL_CFG_NODE *succ  = *succ_iter;
            Is_True(succ != NULL,("The succ of node is NULL."));   
            if ((!BS_MemberP(visited,succ->Id()))&&
		    Find_In_Vector(succ,nodes) != nodes.end()) {
                BS_Union1D(visited,node->Id(),&_m);
                BOOL is_cutted = TRUE;

                for (CFG_PRED_NODE_ITER pred_iter(succ);
                    pred_iter != 0;++pred_iter) {
                   Is_True(*pred_iter != NULL,("Pred of succ is NULL in compute scope based on main exit."));
                   //----------------------------------------------------------
                   // If its pred is a node in nodes and not been cutted,then 
                   // it will not be cutted.
                   //----------------------------------------------------------
                   if (!BS_MemberP(cutted,(*pred_iter)->Id())&&
			    Find_In_Vector((*pred_iter),nodes) != nodes.end()) {
                       is_cutted = FALSE;
                       break;
                   }
                }
                
                if (is_cutted) {
                    BS_Union1D(cutted,succ->Id(),&_m);
                    queue.push(succ);
                } 
            }
        }    
    }    

    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in compute scope based on main exit."));  

        if (!BS_MemberP(cutted,node->Id())) {
            scope.push_back(node);
        }
    }    
}

//=====================================================================
// Compute_Duplicate_Ratio
//
//  Function : REGIONAL_CFG::Compute_Duplicate_Ratio
//  Input : 
//     - nodes :  The node set for which to compute the number of ops in.
//     - duplicated : The input node set to get result for nodes should be
//       duplicated.
//                
//  Output : 
//     - The duplicated is a output of the nodes should be duplicated for the
//       elimination of nodes' side entries.
//       return value indicates the ratio of duplication.
//  Description:
//     The number of duplicated operations will count the operations of all
//     node in the duplicated node set.This will give the duplication of 
//     region nodes less opportunity.At the same time,the number of bbs will
//     be duplicated is also controlled.
//     
//=====================================================================
float 
REGIONAL_CFG::Compute_Duplicate_Ratio(NODE_VECTOR nodes,
              NODE_VECTOR& duplicated,INT32 max_bb_num,INT32 max_dup_num) {
    Is_True(!nodes.empty(),("Node set is empty in compute duplicate ratio."));
    
    float dup_ops_num = 0.0;
    
    if (!duplicated.empty()) {
        //---------------------------------------------------------------------
        //If there are bad nodes in the node vector,the returned value is -1.
        //---------------------------------------------------------------------
        dup_ops_num = (float)Compute_Num_Of_Ops(duplicated,TRUE);
    }
    
    float ratio = 0.0;

    if (dup_ops_num < 0.0) {
        ratio = 2.5;
    } else {
        INT32 dup_bbs_num = 0;
        for (NODE_VECTOR_ITER iter = duplicated.begin();
            iter != duplicated.end();iter++) {
            REGIONAL_CFG_NODE *node = *iter;
            if (!node->Is_Region()) {
                dup_bbs_num++;
            } else {
                BB_VECTOR bbs;
                Compute_BBs_In_Region_Node(bbs,node);
                dup_bbs_num += bbs.size();
            }
        }
        //------------------------------------------------------------------
        //Here using the max bb num to compute the bb ratio because using
        //node or compute all bbs are both unreasonable.For example,when 
        //a region contain a node which has much BBs,then that means much
        //BBs can be duplicated.On the contrary,if only nodes are counted,
        //a region node very large can be duplicated.
        //------------------------------------------------------------------
        float bb_ratio = 0;
        if (dup_bbs_num > max_dup_num) {
            //Assume the max dup ratio is 2.0,therefore 2.5 is always greater 
            //than possible max dup ratio.This is used to avoid some specific
            //BBs be duplicated.
            bb_ratio = 2.5; 
        } else {
            bb_ratio = (float)dup_bbs_num / max_bb_num;
            bb_ratio = bb_ratio + 1.0;
        } 
        
        if (bb_ratio > ratio) {
            ratio = bb_ratio;
        }
    }
    
    return ratio;
}

//=====================================================================
//
//  Compute_Nodes_To_Be_Duplicated
//
//  Description:
//    Compute the nodes should be duplicated based on main_entry.
//=====================================================================
void
REGIONAL_CFG::Compute_Nodes_To_Be_Duplicated(NODE_VECTOR& dup_nodes,
              NODE_VECTOR nodes,REGIONAL_CFG_NODE *main_entry) {
    typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>   NODE_LIST;
    typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>   NODE_QUEUE;  
    
    NODE_ALLOC temp_alloc(&_m);
    NODE_LIST  temp_list(temp_alloc);  
    NODE_QUEUE queue(temp_list);
    queue.push(main_entry);

    BS *visited = BS_Create_Empty(2+_seq_num,&_m);
    BS_Union1D(visited,main_entry->Id(),&_m);

    if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
        fprintf(TFile,"The main entry is:\n");
        main_entry->Print();
    }

    while (!queue.empty()) {
        REGIONAL_CFG_NODE *node = queue.front();
        Is_True(node != NULL,("Node is NULL in compute nodes to be duplicated."));
        queue.pop();
        
        for (CFG_SUCC_NODE_ITER succ_iter(node);succ_iter != 0;++succ_iter) {
            REGIONAL_CFG_NODE *succ = *succ_iter;
            Is_True(succ != NULL,("Succ is NULL in compute nodes to be duplicated."));
            BOOL all_pred_visited = TRUE;
            
            for (CFG_PRED_NODE_ITER iter(succ);iter != 0;++iter) {
            
                if (!BS_MemberP(visited,(*iter)->Id())
                   && Find_In_Vector((*iter),nodes) != nodes.end()) {
                    all_pred_visited = FALSE;
                }
            }
            //--------------------------------------------------------    
            // If the node not visited yet and in the selected nodes.
            //--------------------------------------------------------
            if ((!BS_MemberP(visited,succ->Id())) &&
		    Find_In_Vector(succ,nodes) != nodes.end() &&
		    all_pred_visited) {
                BS_Union1D(visited,succ->Id(),&_m);
                queue.push(succ);
            
                for (CFG_PRED_NODE_ITER pred_iter(succ);
                    pred_iter != 0;++pred_iter) {
 
                    if (Find_In_Vector((*pred_iter),nodes) == nodes.end()) {
                        dup_nodes.push_back(succ); 
                        break;
                    } else if (Find_In_Vector((*pred_iter),dup_nodes) != dup_nodes.end()) {
                        if (Find_In_Vector(succ,dup_nodes) == dup_nodes.end()) {
                            dup_nodes.push_back(succ);
                        }
                      
                        break;
                    }    
                }
            }
        }    
    }    
}

//=====================================================================
//
//  Compute_Num_Of_Ops
//
//  Input : 
//     - nodes :  The node set which are used to compute the number of
//                 ops in.
//     - is_dup : A boolean signal used to indicate the object. 
//                
//  Output : 
//     it indicates the number of operations in nodes
//  Description :
//     If is_dup is TRUE,all operations in the node set will be counted
//     including ops in region node,but when there are any unwanted bb in
//     the node set,the returned value is -1.Otherwise,only ops in BB 
//     node will be counted and unwanted bb is not cared about.
//
//=====================================================================
INT32
REGIONAL_CFG::Compute_Num_Of_Ops(NODE_VECTOR nodes,BOOL is_dup) {
    INT32 num_of_ops = 0;
    BOOL  has_unwanted_bb = FALSE;

    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in compute number of ops."));
        has_unwanted_bb = Is_Unwanted_Node(node);
        if (has_unwanted_bb) {
            break;    
        }
        
        if (!node->Is_Region()) {
            BB *bb = node->BB_Node();
            num_of_ops += BB_length(node->BB_Node());     
        } else if (is_dup) {
            BB_VECTOR bbs;
            
            Compute_BBs_In_Region_Node(bbs,node);

            for (BB_VECTOR_ITER iter = bbs.begin();iter != bbs.end();iter++) {
                Is_True(*iter != NULL,("The BB is NULL in compute number of ops."));
                num_of_ops += BB_length((*iter));
            }
        }
    }
    
    if (has_unwanted_bb) {
        num_of_ops = -1;
    }

    return num_of_ops;
}    
           
//========================================================================
//
//  Find_Seed
//   - Input:
//        badnode:node set which can not be a seed.
//  Description:
//     Find out a node which has the highest execution frequency
//     and not in the badnode vector.A improper region node can not
//     be a seed node.
//  
//========================================================================     
REGIONAL_CFG_NODE*
REGIONAL_CFG::Find_Seed(NODE_VECTOR bad_nodes) {
    REGIONAL_CFG_NODE *seed = NULL;
    
    float freq     = 0.0;
    float max_freq = -1; //Some nodes' freq maybe zero.A seed must be found.

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(this);iter != 0;++iter) {
        REGIONAL_CFG_NODE *node = *iter;
        freq = Node_Freq(node);
        BOOL is_improper = FALSE;
        if (node->Is_Region()) {
            REGION *r = node->Region_Node();

            if (r->Region_Type() == IMPROPER) { 
                is_improper = TRUE;
            }
        } 

        if ((freq > max_freq)&&(!is_improper) &&
		Find_In_Vector(node,bad_nodes) == bad_nodes.end() &&
		!node->Is_Loop_Tail() && !node->Is_Region()) {
            // Not include loop head and tail in SEME region
            // because of exit prob computation 
            seed = node;
            max_freq = freq;
        }     
    }    
    
    return seed;        
}    

//=============================================================================
//
//  Select_Freq_Succ
//
//  Get the most frequently executed succ of node,which is still not
//  selected into the region.Improper region node can not be selected.
//
//=============================================================================   
REGIONAL_CFG_EDGE*
REGIONAL_CFG::Select_Freq_Succ(REGIONAL_CFG_NODE *node,NODE_VECTOR nodes) {
    float freq;
    float              max_freq = -1;
    REGIONAL_CFG_EDGE *result   = NULL;

    for (CFG_SUCC_EDGE_ITER iter(node);iter != 0;++iter) {
         REGIONAL_CFG_EDGE *succ = *iter;
         REGIONAL_CFG_NODE *succ_node = succ->Dest();
         
         if (Find_In_Vector(succ_node,nodes) == nodes.end()) {

             freq = Edge_Freq(succ);
          
             BOOL is_improper = FALSE;

             if (succ_node->Is_Region()) {
                 REGION *r = succ_node->Region_Node();
                 if (r->Region_Type() == IMPROPER) {
                     is_improper = TRUE;
                 }
             } 
             
             if ((freq > max_freq)&& (!is_improper)
	        &&(!succ_node->Is_Loop_Tail())) {
                // Not include loop head and tail in SEME region
		// because of exit prob computation  
		max_freq = freq;
                result = succ;
                
             }
         }
     }
     
     return result;             
}

//=============================================================================
//
//  Select_Freq_Pred
//
//  Get the most frequently executed pred of input node.Improper
//  region node can not be seleteced.
//
//=============================================================================   
REGIONAL_CFG_EDGE*
REGIONAL_CFG::Select_Freq_Pred(REGIONAL_CFG_NODE *node,NODE_VECTOR nodes,NODE_VECTOR bad_nodes) {
    float freq;
    float              max_freq = -1; //Some nodes has freq of zero.
    REGIONAL_CFG_EDGE *result   = NULL;

    for (CFG_PRED_EDGE_ITER iter(node);iter != 0;++iter) {
         REGIONAL_CFG_EDGE *pred = *iter;
         Is_True(pred != NULL,("The pred edge is NULL in select freq pred."));

         REGIONAL_CFG_NODE *pred_node = pred->Src();
         Is_True(pred_node != NULL,("The src node of pred edge is NULL in select freq pred."));

         if (Find_In_Vector(pred_node,nodes) == nodes.end()) {
             freq = Edge_Freq(pred);
             
             BOOL is_improper = FALSE;
             
             if (pred_node->Is_Region()) {
                 REGION *r = pred_node->Region_Node();
                 Is_True(r != NULL,("Node's region node is NULL in select freq pred."));

                 if (r->Region_Type() == IMPROPER) {
                     is_improper = TRUE;
                 }
             } 

             if ((freq > max_freq)&&(!is_improper)
                &&(Find_In_Vector(pred_node,bad_nodes) == bad_nodes.end())
		&&(!pred_node->Is_Loop_Tail())) {
                 // Not include loop head and tail in SEME region 
                 // because of exit prob computation
                 max_freq = freq;
                 result   = pred;
             }
         }
     }
     
     return result;             
}

//=============================================================================
//
//  Select_Freq_Connected_Node
//  - Input:
//      nodes:The node set already been selected for a MEME region.
//      last node:The last node of the selected hot path during MEME region 
//      formation.
//  Description:
//    Select such a node,which has highest frequency to be executed from nodes,
//    and if this node has succ in nodes,it is given higher priority to be
//    selected.
//    A node with the last node as its pred will not be selected in order to
//    avoid unlimited extension of the hot path.
//=============================================================================   
REGIONAL_CFG_NODE*   
REGIONAL_CFG::Select_Freq_Connected_Node(NODE_VECTOR nodes,REGIONAL_CFG_NODE *last_node) {   
    // Consider many edge freq may be zero for static profile can not handle
    // when irreducible cfg exist.
    float              max_freq = -1;  
    REGIONAL_CFG_NODE *result   = NULL;
    
    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();++iter) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in select freq connected node."));

        for (CFG_SUCC_NODE_ITER succ_iter(node);succ_iter != 0;++succ_iter) {
            
            float pred_freq = -1;
            float succ_freq = -1;
            float freq = -1;
            
            REGIONAL_CFG_NODE *cand = *succ_iter;
            if (Find_In_Vector(cand,nodes) == nodes.end()) {
			     pred_freq = 0.0;
                 succ_freq = 0.0;
                 freq = 0.0;
             
                for (CFG_PRED_EDGE_ITER pred_edge_iter(cand);
                    pred_edge_iter != 0;++pred_edge_iter) {
                    REGIONAL_CFG_NODE *pred = (*pred_edge_iter)->Src();

                    if (Find_In_Vector(pred,nodes) != nodes.end()) {
                        pred_freq += Edge_Freq((*pred_edge_iter));
                    }
                }
            
                for (CFG_SUCC_EDGE_ITER succ_edge_iter(cand);
                    succ_edge_iter != 0;++succ_edge_iter) {
                    REGIONAL_CFG_NODE *succ = (*succ_edge_iter)->Dest();

                    if (Find_In_Vector(succ,nodes) != nodes.end()) {
                        succ_freq += Edge_Freq((*succ_edge_iter));
                    }
                }
            }

            freq = pred_freq;
            if ((pred_freq != -1)&&(succ_freq != -1)) { 
                freq += succ_freq;
                freq = freq*2.0;
            }

            BOOL is_improper = FALSE;
            //------------------------------------------------------------------
            // Exit BB,switch and call BB will not be excluded from region
            // formation to give more freedom to region formation,but they
            // are not permitted to be duplicated. Check the cand to make
            // it sure that it is not a improper region node or a succ of
            // the last node of the hot path.This is a choice.
            //------------------------------------------------------------------
            if (cand->Is_Region()) {
                REGION *r = cand->Region_Node();

                if (r->Region_Type() == IMPROPER) {
                    is_improper = TRUE;
                }
            }
            
            BOOL is_last = FALSE;
            for (CFG_PRED_NODE_ITER pred_iter(cand);
                pred_iter != 0;++pred_iter) {
                if (*pred_iter == last_node) {
                    is_last = TRUE;

                    break;
                }
            }
            //-------------------------------------------------
            // A improper region node will not be accepted.
            //-------------------------------------------------
            if ((freq > max_freq)&&(!is_improper)&&(!is_last)
	       &&(!cand->Is_Loop_Tail())) {
    	        // Not include loop head and tail in SEME region 
	        // because of exit prob computation
                max_freq = freq;
                result   = cand;
            }    
        }
    }

    return result;
}    

//=============================================================================
//
//  Succ_Suit 
//
//  Decide whether the chosen node's freq can satisfy the threshold values.
//
//=============================================================================   
BOOL 
REGIONAL_CFG::Succ_Suit(REGIONAL_CFG_EDGE *edge,float seed_freq,float T,float Ts) {
    REGIONAL_CFG_NODE *node;
    float t_succ  = 0.0;
	float ts_succ = 0.0;

    node = edge->Dest();
    float node_freq = Node_Freq(node);
    if (node_freq == 0.0) {
         t_succ = 1.0; 
    } else {
         t_succ = Edge_Freq(edge) / node_freq;
    }
    
    if (seed_freq == 0.0) {
        ts_succ = 1.0;
    } else {
        ts_succ = node_freq / seed_freq;
    }

    if ((t_succ > T) && (ts_succ > Ts)) {
        return TRUE;
    }
    
    return FALSE;
}   

//=============================================================================
//
//  Pred_Suit
//
//  Decide whether the chosen node can satisfy the threshold values.
//
//=============================================================================
BOOL 
REGIONAL_CFG::Pred_Suit(REGIONAL_CFG_EDGE *edge,float seed_freq,
              float T,float Ts) {
    REGIONAL_CFG_NODE *node;
    float t_pred  = 0.0;
    float ts_pred = 0.0;

    node = edge->Src();
    float node_freq = Node_Freq(node);
	if (node_freq ==0.0) {
            t_pred = 1.0; 
	} else {
            t_pred = Edge_Freq(edge) / node_freq;
    }
    
    if (seed_freq == 0.0) {
        ts_pred = 1.0;
    } else {
        ts_pred = node_freq / seed_freq;
    }

    if ((t_pred > T) && (ts_pred > Ts)) {
        return TRUE;
    }
    
    return FALSE;
}         

//========================================================================
//
//  Compute_Edges_Freq
//  
//  Compute the frequency of every regional cfg edge in this regional 
//  control flow graph.Because of the nested regions,this function is 
//  somewhat too long.
//  Topological traverse every node in cfg
//    for every succ edge of the node 
//      compute edge's frequency.
//
//========================================================================       
void 
REGIONAL_CFG::Compute_Edges_Freq(void) {
    typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>               NODE_LIST;
    typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>               NODE_QUEUE;  
    
    NODE_ALLOC temp_alloc(&_m);
    NODE_LIST  temp_list(temp_alloc);
    BS          *bb_exits = BS_Create_Empty(2+PU_BB_Count, &_m); 
    BS          *bb_entries = BS_Create_Empty(2+PU_BB_Count, &_m);
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(this);iter != 0; ++iter) {
        BS_ClearD(bb_exits);
        NODE_VECTOR  node_exits(temp_alloc);
        
        REGIONAL_CFG_NODE *node = *iter;
        float total_freq = 0.0;
        
        if (node->First_Succ() == NULL) {

            if (node->First_Pred() != NULL) {
                for (CFG_PRED_EDGE_ITER pred_iter(node);
                    pred_iter != 0;++pred_iter) {
                     total_freq += (*pred_iter)->Freq();             
                }

                node->Freq(total_freq);
            } else {
                if (node->Is_Region()) {
                    DevWarn("Will we meet a node with only iteself?");
                    node->Freq(1.0);
                } else {
                    node->Freq(BB_freq(node->BB_Node()));
                } 
            }

            continue;
        }
        
        if (node->Is_Region()) { //If the node is a region
            //------------------------------------------------------------------
            // Find out all exits and entries of the node,put them in vector.
            //------------------------------------------------------------------
            NODE_QUEUE   que(temp_list);
            que.push(node);
                
            while (!que.empty()) {
                REGIONAL_CFG_NODE *region_exit = que.front();
                que.pop();
                NODE_VECTOR exits =  (region_exit->Region_Node())->Exits();
                   
                for (NODE_VECTOR_ITER exit_iter = exits.begin();
                    exit_iter != exits.end();exit_iter++) {
                    REGIONAL_CFG_NODE *exit = *exit_iter;
                  
                    if (exit->Is_Region()) {
                        que.push(exit);
                    } else {
                        BS_Union1D(bb_exits, BB_id(exit->BB_Node()), &_m);
                        node_exits.push_back(exit);
                    }
                }
            }    
        } //End of if node is a region.
        
        //-------------------------------------------------------------------- 
        //  Compute total frequency first,after all,this method to compute 
        //  total frequency is only right for SEME regions.But,except
        //  IMPROPER regions,there are any MEME regions?
        //--------------------------------------------------------------------
        if (node->Is_Region()) {
            for (NODE_VECTOR_ITER iter =  node_exits.begin();
                iter != node_exits.end(); iter++) {
                BBLIST *succ_list;
                REGIONAL_CFG_NODE *exit = *iter;
                BB *src = exit->BB_Node();

                total_freq += BB_freq(src);
            }    
        } else {
            total_freq = BB_freq(node->BB_Node());
        }

        //-----------------------------------
        //  Compute edges frequency then.
        //-----------------------------------
        for (CFG_SUCC_EDGE_ITER succ_iter(node);succ_iter != 0; ++succ_iter) {
            
            REGIONAL_CFG_NODE *succ = (*succ_iter)->Dest();

            if (succ->Is_Region()) {
                NODE_QUEUE   que;
                BS_ClearD(bb_entries);
                
                que.push(succ);
                //--------------------------------------------------------------
                // If the succ node is a region,compute the bb entries of this
                // region.  
                //--------------------------------------------------------------
                while (!que.empty()) {
                    REGIONAL_CFG_NODE *region_entry = que.front();
                    que.pop();
                    
                    NODE_VECTOR entries =  (region_entry->Region_Node())->Entries();
                     
                    for (NODE_VECTOR_ITER entry_iter = entries.begin();
                        entry_iter != entries.end();entry_iter++) {
                        REGIONAL_CFG_NODE *entry = *entry_iter;
                        if (entry->Is_Region()) {
                            que.push(entry);
                        } else {
                            BS_Union1D(bb_entries, BB_id(entry->BB_Node()), &_m);
                        }
                    }
                }
                //-----------------------------------------------------------   
                // If node is not a region,compute the freq connected
                // between bb and the connected bb entries.
                //------------------------------------------------------------
                if (!node->Is_Region()) { //If node is not region but succ is region.
                     float   freq = 0;
                     BBLIST *succ_list;  
                     BB *src  = node->BB_Node();
                     
                     
                     for (succ_list = BB_succs(src);succ_list != NULL;
                         succ_list = BBLIST_next(succ_list)) {
                         BB *bb = BBLIST_item(succ_list);
                          
                         if (BS_MemberP(bb_entries,BB_id(bb))) {
                             freq += BB_freq(src)*BBLIST_prob(succ_list);
                         }
                     }  

                     (*succ_iter)->Freq(freq);
                  } else { //If both node and succ are region nodes.
                     float freq = 0;
                               
                     for (NODE_VECTOR_ITER iter =  node_exits.begin();
                         iter != node_exits.end(); iter++) {
                         BBLIST *succ_list;
                         REGIONAL_CFG_NODE *exit = *iter;
                         BB *src = exit->BB_Node();
                        
                         for (succ_list = BB_succs(src);succ_list != NULL;
                             succ_list = BBLIST_next(succ_list)) {
                             BB *bb = BBLIST_item(succ_list);
                           
                             if (BS_MemberP(bb_entries,BB_id(bb))) {
                                 freq += BB_freq(src)*BBLIST_prob(succ_list);
                             }
                         } 
                     }

                     (*succ_iter)->Freq(freq);
                 }    
            } else {  
                if (!node->Is_Region()) { //If both node and succ are BB nodes.
                    BB *src   = node->BB_Node();
                    BB *dest  = succ->BB_Node();
                    BBLIST *succ_list;         
                    
                    for (succ_list = BB_succs(src);succ_list != NULL;
                        succ_list = BBLIST_next(succ_list)) {
                        BB *bb = BBLIST_item(succ_list);
                        if (bb == dest) {
                            (*succ_iter)->Freq((BB_freq(src))*(BBLIST_prob(succ_list)));
                  
                            break;
                        }
                    }
                } else { //If the node is region but the succ is a bb.    
                     float freq = 0;
                     BB *dest = succ->BB_Node();

                     for (NODE_VECTOR_ITER iter = node_exits.begin();
                         iter != node_exits.end();iter++) {
                         BB *bb = (*iter)->BB_Node();
                         BBLIST *succ_list;         

                         for (succ_list = BB_succs(bb);succ_list != NULL;
                             succ_list = BBLIST_next(succ_list)) {
                             BB *succ = BBLIST_item(succ_list);
                          
                             if (succ == dest) {
                                 freq += BB_freq(bb)*BBLIST_prob(succ_list);

                                 break;
                             }
                         } 
                     }

                     (*succ_iter)->Freq(freq);
                  }   
             }               
        }
        
        if (total_freq == 0.0) {
            //TODO::Add a warning here.
        }
		    
        node->Freq(total_freq);
        //---------------------------------------------------
        //Compute the prob of every edge and set the prob. 
        //---------------------------------------------------
        for (CFG_SUCC_EDGE_ITER succ_iter(node);succ_iter != 0;++succ_iter) {
            float prob = 0.0;
            if (total_freq == 0.0) {
                prob = 0.0;
            } else {
                prob = (*succ_iter)->Freq()/total_freq;
            }    
            (*succ_iter)->Prob(prob);
        }
    } 
} 

//=============================================================================
//
//  Find_BBs_From_Nodes
//
//  It is used in tail duplication.When duplicating regional cfg nodes,find 
//  out the bbs contained by the node set first.
//
//=============================================================================
void 
REGIONAL_CFG::Find_BBs_From_Nodes(BB_VECTOR& bbs,NODE_VECTOR nodes) {
    
    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("The node is NULL in find bbs from nodes."));

        if (node->Is_Region()) {
            Compute_BBs_In_Region_Node(bbs,node);
        } else {
            bbs.push_back(node->BB_Node());
        } 
    }
}

//=============================================================================
//
//  Compute_BBs_In_Region_Node
//
//  Used in tail duplication,if a regional cfg node is a region,compute the bbs
//  contained by this region.
//
//=============================================================================
void 
REGIONAL_CFG::Compute_BBs_In_Region_Node(BB_VECTOR& bbs,REGIONAL_CFG_NODE *node) {
    Is_True(node->Is_Region(),("Not a region node!\n"));

    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR temp_vector(temp_alloc);
    NODE_STACK  region_nodes(temp_vector);
    region_nodes.push(node);

    while (!region_nodes.empty()) {
        REGION       *r   = (region_nodes.top())->Region_Node();
        REGIONAL_CFG *cfg = r->Regional_Cfg();
        Is_True(cfg != NULL,("Cfg is NULL in compute BBs in region node."));
        region_nodes.pop();

        for (SEQ_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter) {
            REGIONAL_CFG_NODE *n = *iter;
            Is_True(n != NULL,("Node is NULL in compute BBs in region node."));

            if (n->Is_Region()) {
                region_nodes.push(n);
            } else {
                bbs.push_back(n->BB_Node());
            }
        }
    }
}

//========================================================================
//
//  Tail_Duplicate
//  
//  -Input:
//      nodes:A connected node set,with multiple entries and will be 
//         tail duplicated to eliminate its side entries.
//      re_shrink:When the tail duplication is done,sometimes regions
//         are duplicated.This value indicates whether the duplicated
//         part should be formed into regions as its origin.THIS FUNC 
//         NOT IMPLEMENTED YET.
//
//========================================================================    
void 
REGIONAL_CFG::Tail_Duplicate(NODE_VECTOR& nodes,REGIONAL_CFG_NODE *main_entry,
                             INT32& dup_bb_num,INT32& dup_op_num,BOOL re_shrink) {
    typedef std::list<BB *,BB_ALLOC> BB_LIST;
    typedef std::queue<BB *,BB_LIST> BB_QUEUE; 
    
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR dup_nodes(temp_alloc);
    BB_ALLOC    bb_temp_alloc(&_m);
    BB_VECTOR   bbs(bb_temp_alloc);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin compute nodes to be duplicated.\n");
    }

    Compute_Nodes_To_Be_Duplicated(dup_nodes,nodes,main_entry);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The nodes should be duplicated are:\n");
        Print_Node_Vector(dup_nodes);
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish compute nodes to be duplicated.\n");
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin to find BBs from nodes.\n");
    }
    
    Find_BBs_From_Nodes(bbs,nodes);

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish find BBs from nodes.\n");
    }
   
    BB*     last_duplicated;
    BB*     entry; 

    BB_MAP     duplicate     = BB_MAP_Create();
    BB_MAP     rev_duplicate = BB_MAP_Create();
    BB_VECTOR  duplicate_bbs(bb_temp_alloc);
    BB_VECTOR  br_bbs(bb_temp_alloc);

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin find the main entry bb of the bbs.\n");
    }
    //----------------------------------------------------- 
    // Find out the place where to put the duplicated bbs. 
    //-----------------------------------------------------
    if (main_entry->Is_Region()) {
        REGIONAL_CFG_NODE *entry_node = main_entry;

        while (entry_node->Is_Region()) {
            NODE_VECTOR entries(temp_alloc);
            entries = (entry_node->Region_Node())->Entries();
            
            for (NODE_VECTOR_ITER iter = entries.begin();
                iter != entries.end();iter++) {
                if ((*iter)->First_Pred() == NULL) {
                    entry_node = *iter;
                    break;
                }
            }
        }

        entry = entry_node->BB_Node(); 
    } else {
        entry = main_entry->BB_Node();
    } 
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The main entry bb is: %d\n",BB_id(entry));
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish find main entry bb of the bbs.\n");
    }
    
    BB *bb;
    for (bb = entry; bb && Find_In_BB_Vector(bb,bbs) != bbs.end();
        last_duplicated = bb,bb = BB_next(bb));
    
    if (bb) {
        for (; bb && BB_Fall_Thru_Successor(bb); bb = BB_next(bb));
        last_duplicated = bb;
    }
    //---------------------------------------------------
    // Compute the bbs which should be duplicated first.
    //---------------------------------------------------
    BB_VECTOR need_duplicate(bb_temp_alloc);
    //Becuase use PU_BB_Count is not enough.
    BS       *unduplicated = BS_Create_Empty(PU_BB_Count+2,&_m);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin find bbs need to be duplicated.\n");
    }
    
    Compute_BBs_Need_Duplicate(need_duplicate,unduplicated,dup_nodes);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"BBs should be duplicated:\n");
        Print_BB_Vector(need_duplicate);
    }
     
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish find bbs need to be duplicated.\n");
    }
     
    BOOL has_cycle = FALSE;
    //-----------------------------------------------------
    // Compute the side entries in the bbs need duplicate.
    //-----------------------------------------------------
    BB_LIST  bb_temp_list(bb_temp_alloc);
    BB_QUEUE entries(bb_temp_list);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin compute side entries of bbs.\n");
    } 

    for (BB_VECTOR_ITER iter = need_duplicate.begin();
        iter != need_duplicate.end();iter++) {
        BB *bb = *iter;
        BBLIST *preds;
        BOOL is_side_entry = TRUE; 
        
        FOR_ALL_BB_PREDS(bb,preds) {
            BB *pred = BBLIST_item(preds);
            BOOL is_cycle = FALSE;

            for (GLOBAL_CYCLE_VECTOR_ITER cycle_iter = global_cycles.begin();
                 cycle_iter != global_cycles.end();cycle_iter++) {
                if ((bb == (*cycle_iter).dest)&&(pred == (*cycle_iter).src)) {
                    is_cycle  = TRUE;       
                    has_cycle = TRUE;
                }
            }

            if (BS_MemberP(unduplicated,BB_id(pred))&&(!is_cycle)) {
                is_side_entry = FALSE;
            } 
                
        } 

        if (is_side_entry) {
            entries.push(bb);
            
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                fprintf(TFile,"The side entry is %d\n",BB_id((*iter)));
            } 
        }
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish compute side entries.\n Begin to do duplicate.\n");
    }

    while (!entries.empty()) {
        BB *side_entry = entries.front();
        entries.pop();
        
        
        Duplicate(bbs,side_entry,entry,duplicate,rev_duplicate,
                 &last_duplicated,duplicate_bbs,br_bbs);
        dup_bb_num = dup_bb_num + br_bbs.size() + 1; 
        dup_op_num = dup_op_num + br_bbs.size() + BB_length(side_entry);
        
        BS_Difference1D(unduplicated,BB_id(side_entry));
        BBLIST *succs;
        FOR_ALL_BB_SUCCS(side_entry,succs) {
            BB *succ = BBLIST_item(succs);
             
            if (BS_MemberP(unduplicated,BB_id(succ))) {
                BOOL has_unduplicated_pred = FALSE; 
                BBLIST *preds;

                FOR_ALL_BB_PREDS(succ, preds) {
                    BB *pred = BBLIST_item(preds);
                    BOOL is_global_cycle = FALSE;
                    
                    if (has_cycle) {
                        for (GLOBAL_CYCLE_VECTOR_ITER iter = global_cycles.begin();iter != global_cycles.end();iter++) {
                            if (((*iter).src == pred)&&((*iter).dest == succ)) { 
                                is_global_cycle = TRUE; // Decide whether the pred is a cycle.

                            }
                        }
                    }

                    if (BS_MemberP(unduplicated,BB_id(pred))
                       &&!is_global_cycle) {
                        has_unduplicated_pred = TRUE;
                        
                        break;
                    }
                }
                
                if (!has_unduplicated_pred) {
                    entries.push(succ);
                } 
            }
        }
    }
     
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish do duplication.\n");
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin to do BB profiling information update.\n");
    }
    
    Update_BB_Prof_Info(bbs,need_duplicate,duplicate,
    rev_duplicate,br_bbs,entry);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finished doing BB profiling information update.\n");
    }
    
    for (NODE_VECTOR_ITER iter = dup_nodes.begin();
        iter != dup_nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
         
        for(CFG_PRED_EDGE_ITER pred_iter(node);pred_iter != 0;++pred_iter)  {
            REGIONAL_CFG_EDGE *pred = *pred_iter;
            if (Find_In_Vector(pred->Src(),nodes) == nodes.end()) {
                Del_Edge(pred);
            }
        }
    }
    
    Add_BBS_And_Edges(duplicate_bbs);
    REGION_TREE *tree = (this->_r)->Tree();
    
    if (has_cycle) {

        if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
            fprintf(TFile,"Duplicated loops,begin redo interval process.\n");
        }
    
        tree->Process_Intervals(this->_r);
        printf("Finished print intervals!");
        if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
            fprintf(TFile,"Duplicated loops,finish redo interval process.\n");
        }
    }
    
    if (re_shrink) {
        //TODO:Whether this function is needed? 
    }
}

//=============================================================================
//
//  Add_BBS_And_Edges
// 
//  Add some new bbs to a regional cfg and link the edges to these bbs in the 
//  regional cfg.
//
//=============================================================================
void 
REGIONAL_CFG::Add_BBS_And_Edges(BB_VECTOR bbs,NODE_VECTOR *new_nodes) {
    for (BB_VECTOR_ITER iter = bbs.begin();iter != bbs.end();iter++) {
        BB *new_bb = *iter;

        if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
            fprintf(TFile,"Newly added bb is %d",BB_id(new_bb));
        }
        
        REGIONAL_CFG_NODE *new_node = Add_Node(new_bb);
   
        Is_True(new_node != NULL,("The new node is null,add new node failed in add BBs and edges.\n",BB_id(new_bb)));
        
        if(new_nodes != NULL) {
            new_nodes->push_back(new_node); 
        }

        if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
            fprintf(TFile,"Add a new node!\n");
        } 
     }
     
     for (BB_VECTOR_ITER iter = bbs.begin();iter != bbs.end();iter++) {
        BB *new_bb = *iter;
        REGIONAL_CFG_NODE *new_node = (REGIONAL_CFG_NODE *)BB_MAP_Get(bb_node_map,new_bb);
        
        Is_True(new_node != NULL,("The new node is null,bb %d has no corresponding node.\n",BB_id(new_bb)));

        BBLIST *preds;
     
        FOR_ALL_BB_PREDS(new_bb,preds) {
            BB *pred = BBLIST_item(preds);
            RGN_Add_Regional_Cfg_Edge(pred,new_bb);
        }

        BBLIST *succs;      
        
        FOR_ALL_BB_SUCCS(new_bb,succs) {
            BB *succ = BBLIST_item(succs);
            RGN_Add_Regional_Cfg_Edge(new_bb,succ);
        }
    }
}

//=============================================================================
//
//  Duplicate
//  
//  Duplicate the bb.Originally it is a function in Pro64 and only slight
//  modification has been done to it.
//
//=============================================================================
void 
REGIONAL_CFG::Duplicate(BB_VECTOR bbs, BB* side_entrance,BB *entry,
                        BB_MAP duplicate,BB_MAP rev_duplicate,BB** last, 
                        BB_VECTOR& duplicate_bbs,BB_VECTOR& br_bbs) {
    BBLIST* bl;

	BB* dup =  Copy_BB_For_Tail_Duplication(NULL, side_entrance);
	if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
            fprintf(TFile,"The ops in the original bb is:\n");
            Print_Ops_In_BB(side_entrance);
        }
    
        if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
            fprintf(TFile,"The ops in the duplicated bb is:\n");
            Print_Ops_In_BB(dup);
        }
    }
    
    GRA_LIVE_Compute_Liveness_For_BB(dup);
    duplicate_bbs.push_back(dup);
    BB_MAP_Set(duplicate, side_entrance, dup);
    BB_MAP_Set(rev_duplicate,dup,side_entrance);
    //----------------------------------------------------------------------    
    // Find new block's place. If it's the fall thru successor of a
    // duplicated block or a block not included in the hyperblock, then
    // that's where it goes.  Otherwise, place it below the last block added.
    //-----------------------------------------------------------------------
    FOR_ALL_BB_PREDS(side_entrance, bl) {
        BB* pred = BBLIST_item(bl);
        if (side_entrance == BB_Fall_Thru_Successor(pred)) {
            BB* fall_dup = (BB*) BB_MAP_Get(duplicate, pred);
            
            if (fall_dup) {
                *last = fall_dup;
            } else if (Find_In_BB_Vector(pred,bbs) == bbs.end()) {
                *last = pred;
            }
            
            break;
        }
    }
    
    BB* fall_thru = NULL;
    if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
        fprintf(TFile,"Before do fix up arcs!");
    }

    Fixup_Arcs(bbs, side_entrance, dup, entry, duplicate,
    &fall_thru, duplicate_bbs,br_bbs);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
        fprintf(TFile,"finished do fix up arcs!");
    }
    
    Insert_BB(dup, *last);
  
    if (fall_thru) {
        Insert_BB(fall_thru, dup);
        *last = fall_thru;
    } else {
        *last = dup;
    }
}

//=============================================================================
//
//  ARN_Is_Log_If
//
//=============================================================================
BOOL
REGIONAL_CFG::ARN_Is_Log_If(BB *bb) {
    OP *br;
    br = BB_branch_op(bb);
    
    if (br != NULL) {

        if (OP_cond(br) && !OP_ijump(br)) {
            return TRUE;
        } 
    }
    
    return FALSE;
}

//=============================================================================
//
//  Fixup_Arcs
//
//  Adjust the arcs in global cfg between bbs.
//
//=============================================================================
void 
REGIONAL_CFG::Fixup_Arcs(BB_VECTOR& bbs,BB *old_bb,BB *new_bb,BB *entry,
                         BB_MAP duplicate,BB **fall_thru,
                         BB_VECTOR& duplicate_bbs,BB_VECTOR& br_bbs) {
    BBLIST *bl;
    float new_freq = 0.0;

    //---------------------------------------------------------------------
    // Move predecessor arcs from outside of the hyperblock to new block.
    //---------------------------------------------------------------------
    for (bl = BB_preds(old_bb); bl != NULL;) {
        BB *pred = BBLIST_item(bl);
        bl = BBLIST_next(bl);
        if (pred == old_bb) continue; //Old_bb is a single BB loop.
        //---------------------------------
        // Calculate block frequency.
        //---------------------------------
        BBLIST *blsucc = BB_Find_Succ(pred, old_bb);

        //--------------------------------------------------------------------
        // Now, it's either a block not selected for the hyperblock, or
        // a block that's already been duplicated, or it is an unduplicated
        // member of the hyperblock and we need do nothing.
        //--------------------------------------------------------------------
        BB *dup = (BB*) BB_MAP_Get(duplicate, pred);
        
        if (dup) {
            new_freq += BB_freq(pred) * BBLIST_prob(blsucc);

        //--------------------------------------------------------------
        // Need to make the appropriate arcs from the duplicated block
        // to this one.
        //--------------------------------------------------------------
            if (BB_Fall_Thru_Successor(pred) == old_bb) {
                Link_Pred_Succ_with_Prob(dup, new_bb, BBLIST_prob(blsucc));
                if (BB_kind(dup) == BBKIND_GOTO) {
                    BB_Remove_Branch(dup);
                }
            } else if (ARN_Is_Log_If(dup)) {
                //-----------------------------------------------------------
                // Here when the dup bb has not put its succs attached,
                // it will be deemed as a BB with UNKNOWN KIND,therefore
                // lead to mistake.Here ARN_Is_Log_If is written to fix this.
                //-----------------------------------------------------------
                Target_Cond_Branch(dup, new_bb, BBLIST_prob(blsucc));
            } else {
                //-------------------------------------------------------------
                // Here is the problem why the switch is replaced by a goto BB.
                //-------------------------------------------------------------
                BB_Remove_Branch(dup);
                Add_Goto(dup, new_bb);
            }
        } else if (Find_In_BB_Vector(pred,bbs) != bbs.end()) {
            continue;
        } else {
            new_freq += BB_freq(pred) * BBLIST_prob(blsucc);
            if (BB_Fall_Thru_Successor(pred) == old_bb) {
                Change_Succ(pred, old_bb, new_bb);
            } else {
                BB_Retarget_Branch(pred, old_bb, new_bb);
            }
        }
    }
    

    BB_freq(new_bb) = new_freq;
    //------------------------------------------------------------------
    // Must check the original block's successors.If they're outside
    // the hyperblock, we must add arcs here.
    //------------------------------------------------------------------
    FOR_ALL_BB_SUCCS(old_bb, bl) {
        BB* succ = BBLIST_item(bl);
        if (Find_In_BB_Vector(succ,bbs) == bbs.end() || succ == entry) {
           
            if (BB_Fall_Thru_Successor(old_bb) == succ) {
                //-----------------------------------------------------------
                // We insert a block here because it makes it easier if
                // this guy is a LOGIF, and both successors are outside
                // the hyperblock.  Cflow will clean up the unneeded block.
                // We add it to the duplicate bbs list so that it's liveness
                // will be calculated correctly later.
                //-----------------------------------------------------------
   
                BB *new_succ = Gen_And_Insert_BB_After(new_bb);
                
                if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                    fprintf(TFile,"New bb generated%d",BB_id(new_succ));
                }

                Link_Pred_Succ(new_bb, new_succ);

                if (BB_kind(new_bb) == BBKIND_GOTO) {
                    BB_Remove_Branch(new_bb);
                }
                
                Add_Goto(new_succ, succ);
                Link_Pred_Succ(new_succ, succ);
                GRA_LIVE_Compute_Liveness_For_BB(new_succ);
                duplicate_bbs.push_back(new_succ);
                br_bbs.push_back(new_succ);
                *fall_thru = new_succ;
            } else {
                Link_Pred_Succ_with_Prob(new_bb, succ, BBLIST_prob(bl));
            }
        } else {
             //--------------------------------------- 
             // This happened when cycle duplicated.
             //---------------------------------------  
             BB *dup = (BB*) BB_MAP_Get(duplicate, succ);
             
             if (dup) {//Should we check if fallthrough or not here?
                 //This place may cause bugs,be careful.
                 Target_Cond_Branch(new_bb,dup,BBLIST_prob(bl));
             }
        }
    }
}

//========================================================================
//  
//  Update_BB_Prof_Info    
//  
//  Description:
//   After tail duplication,the profiling information must be updated.This 
//   function just recomputes the profiling information of duplicated BBs
//   and their origins as well as the newly added br BBs.
// 
//=========================================================================
void 
REGIONAL_CFG::Update_BB_Prof_Info(BB_VECTOR bbs,BB_VECTOR dup,BB_MAP duplicate,
                                  BB_MAP rev_duplicate,BB_VECTOR br_bbs,BB *main_entry) {
    typedef std::list<BB *,BB_ALLOC>  BB_LIST;
    typedef std::queue<BB *,BB_LIST>    BB_QUEUE;
        
    BB_ALLOC temp_alloc(&_m);
    BB_LIST  temp_list(temp_alloc);
    BB_QUEUE queue(temp_list);
    queue.push(main_entry);
    
    BS *visited = BS_Create_Empty(PU_BB_Count+2, &_m);
    BS_Union1D(visited,BB_id(main_entry),&_m);

    if (dup.empty()) {
        return;
    }
    //-------------------------------------------------------------------------
    // If a loop is duplicated,then how to propagate the frequency?
    // This is a problem.
    //------------------------------------------------------------------------- 
    while (!queue.empty()) {
        BB *entry = queue.front();
        queue.pop();
            
        BBLIST *lists; 
        FOR_ALL_BB_SUCCS(entry,lists) {
            BB* bb = BBLIST_item(lists);
            
            if (Find_In_BB_Vector(bb,bbs) != bbs.end()) {
               
                if (!BS_MemberP(visited,BB_id(bb))) {
                    BOOL all_pred_visited = TRUE;

                    BBLIST *pred_list;
                    FOR_ALL_BB_PREDS(bb,pred_list) {
                        BB *pred_bb = BBLIST_item(pred_list);
                        //----------------------------------------------------- 
                        // Here the cycle is not considered.We must decide 
                        // whether the pred not visited is a BB connected
                        // to backedge.
                        //-----------------------------------------------------
                        BOOL is_cycle = FALSE;      
                        for (GLOBAL_CYCLE_VECTOR_ITER cycle_iter = global_cycles.begin();
                             cycle_iter != global_cycles.end();cycle_iter++) {
                             if ((bb == (*cycle_iter).dest)
                                &&(pred_bb == (*cycle_iter).src)) {
                                  is_cycle  = TRUE;       
                                  break;
                             }
                        }

                        if ((!BS_MemberP(visited,BB_id(pred_bb)))
                           &&(!is_cycle)) {
                            all_pred_visited = FALSE;

                            break;
                        }
                    }

                    if (all_pred_visited) {
                                                
                        BS_Union1D(visited,BB_id(bb),&_m); 
                        queue.push(bb);
                   
                        if (Find_In_BB_Vector(bb,dup) != dup.end()) {
                            float total_freq = 0.0;
                            BB *new_bb = (BB *)BB_MAP_Get(duplicate,bb);
                            //-------------------------------------------------
                            // It must be assured that the succ's all pred
                            // either not in the nodes,either their succ's
                            // freq has already been computed.This is very 
                            // important.
                            //-------------------------------------------------
                            BBLIST *preds;
                            FOR_ALL_BB_PREDS(bb,preds) {
                                BB *pred = BBLIST_item(preds);
                            
                                float freq = BB_freq(pred)*Prob_Local(pred,bb);
                                float p = Prob_Local(pred,bb);
                                if (freq == 0.0) {
                                    DevWarn("IN UPDATE BB PROFILE:Freq zero BB%d to BB%d.\n",BB_id(pred),BB_id(bb));
                                }
                                BB *old_bb = (BB *)BB_MAP_Get(rev_duplicate,pred);
                        
                                if (old_bb == NULL &&
					Find_In_BB_Vector(pred,br_bbs) == br_bbs.end()) {
                                    Set_Freq(pred,bb,freq);
                                }

                                total_freq += freq; 
                            }
                    
                            BB_freq(bb) = total_freq;

                            BBLIST *succs;
                            FOR_ALL_BB_SUCCS(bb,succs) {
                                BB *succ = BBLIST_item(succs);
                                float freq = 0.0;
                                
                                if (Prob_Local(bb,succ) == 0.0) {
                                    DevWarn("The prob is zero between BB%d and BB%d.\n",BB_id(bb),BB_id(succ));
				 }

                                float p = Prob_Local(bb,succ);
                                freq = total_freq*Prob_Local(bb,succ);
                                Set_Freq(bb,succ,freq);
                            }
                            
                            Is_True(new_bb != NULL,("The new bb is NULL for BB %d.\n",BB_id(bb)));
                            float new_bb_freq = 0.0;
                            //--------------------------------------------------
                            // Find out the edge connected new_bb and new_kid
                            // and set the freq
                            //--------------------------------------------------
                            BBLIST *new_preds;
                            FOR_ALL_BB_PREDS(new_bb,new_preds) {
                                BB *new_pred = BBLIST_item(new_preds);
                                //---------------------------------------------
                                // If the new pred is actually a BB which is
                                // not duplicated,the edge freq is already set
                                // to zero.Therefore,we must get the freq info
                                // from the pred's freq multiply the edge 
                                // probability.The edge probability is still 
                                //correct.
                                //---------------------------------------------
                                BB *old_pred = (BB *)BB_MAP_Get(rev_duplicate,new_pred);
                                if (old_pred != NULL) {
                                    if (Freq(new_pred,new_bb) == 0.0) {
                                        DevWarn("IN UPDATE BB PROFILE:Freq zero BB%d to BB%d.\n",BB_id(new_pred),BB_id(new_bb));
    				    }
                                 new_bb_freq += Freq(new_pred,new_bb);
                                } else if (Find_In_BB_Vector(new_pred,br_bbs) == br_bbs.end()) { 
                                    //-----------------------------------------
                                    // The br bbs do not have corresponding
                                    // old pred too,but it is not a old bb
                                    // and its succ edge frequency should have
                                    // already been computed.
                                    //-----------------------------------------
                                    float p = Prob_Local(new_pred,new_bb);
                                    float freq = Prob_Local(new_pred,new_bb)*BB_freq(new_pred);
                                    Set_Freq(new_pred,new_bb,freq);
                            
                                    if (freq == 0.0) {
                                        DevWarn("IN UPDATE BB PROFILE:Freq zero BB%d to BB%d.\n",BB_id(new_pred),BB_id(new_bb));
				    }
                                    new_bb_freq += freq;
                                }
                            }   

                            BB_freq(new_bb) = new_bb_freq; //Set the new bb's bb frequency.
                        
                            BBLIST *new_succs;
                            FOR_ALL_BB_SUCCS(new_bb,new_succs) {
                                BB *new_succ = BBLIST_item(new_succs);
                                BB *old_succ = NULL;
                                BOOL is_br_bb = FALSE;
                                //-------------------------------------------   
                                // If the succ is the newly added br bb,set
                                // the br bb's succ edge's freq and prob too.
                                //------------------------------------------- 
                                if (Find_In_BB_Vector(new_succ,br_bbs) != br_bbs.end()) {
                                    is_br_bb = TRUE;
                                    //-----------------------------------------
                                    // Find last BB which is also the only
                                    // succ of new_succ.
                                    //-----------------------------------------
                                    BBLIST *succ_list = new_succ->succs;
                                    old_succ = BBLIST_item(succ_list);
                                } else {
                                    BB *ori = (BB *)BB_MAP_Get(rev_duplicate,new_succ);

                                    if (ori == NULL) {
                                        old_succ = new_succ;
                                    } else {
                                        old_succ = ori;
                                    }
                                }
                               
                                float prob = Prob_Local(bb,old_succ);

                                if (prob == 0.0) {
                                     DevWarn("IN UPDATE BB PROFILE:Prob zero BB%d to BB%d.\n",BB_id(bb),BB_id(old_succ));
                                }
                                Set_Prob(new_bb,new_succ,prob);
                                float freq = BB_freq(new_bb);
                                
                                if (freq == 0.0) {
                                     DevWarn("IN UPDATE BB PROFILE:Freq zero of BB%d.\n",BB_id(new_bb));
                                }
                                
                                Set_Freq(new_bb,new_succ,prob*freq);
                                if (is_br_bb) {
                                    //----------------------------------------
                                    // Set the probability between the br bb
                                    // and old succ as prob is 1.0 and freq
                                    // the same.
                                    //----------------------------------------
                                    Set_Prob(new_succ,old_succ,1.0);
                                    Set_Freq(new_succ,old_succ,prob*freq);
                                    BB_freq(new_succ) = prob*freq;
                                }
                            }
                        }
                    } 
                }
            }
        }
    }
}

//========================================================================
//  
//  Compute_Node_Prof_Info
//
//  Description:
//   This function is used for computing completion probability.When finding
//   a node set,if it is tail duplicated,the main exit probability will be
//   changed.This function computes the node prof info if tail duplication
//   happpened.This is CONCEPTUAL tail duplication.
//
//=========================================================================
void 
REGIONAL_CFG::Compute_Node_Prof_Info(NODE_VECTOR nodes,NODE_VECTOR dup,
                                     REGIONAL_CFG_NODE *main_entry,
                                     NODE_PROF_VECTOR& node_profs) {
    typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>   NODE_LIST;
    typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>   NODE_QUEUE;
        
    NODE_ALLOC temp_alloc(&_m);
    NODE_LIST  temp_list(temp_alloc);
    NODE_QUEUE queue(temp_list);
    queue.push(main_entry);

    if (dup.empty()) {
        return;
    }
    
    for (CFG_SUCC_EDGE_ITER iter(main_entry);iter != 0;++iter) {
        REGIONAL_CFG_EDGE *e = *iter;
        NODE_PROF p;
        p.freq = e->Freq();
        p.prob = e->Prob();
        p.src  = e->Src();
        p.dest = e->Dest();
        node_profs.push_back(p); 
    }
    
    while (!queue.empty()) {
        REGIONAL_CFG_NODE *node = queue.front();
        queue.pop();

        for (CFG_SUCC_NODE_ITER iter(node);iter != 0;++iter) {
            if (Find_In_Vector(*iter,nodes) != nodes.end()) {
            
                if (Find_In_Vector(*iter,dup) != dup.end()) {
                    REGIONAL_CFG_NODE *succ = *iter;
                    float total_freq = 0.0;
                    
                    for (NODE_PROF_VECTOR_ITER prof_iter = node_profs.begin();
                        prof_iter != node_profs.end();prof_iter++) {
                        NODE_PROF p = *prof_iter;

                        if (p.dest == succ) {
                            total_freq += p.freq;
                        }
                    }

                    for (CFG_SUCC_EDGE_ITER succ_iter(succ);
                        succ_iter != 0;++succ_iter) {
                        REGIONAL_CFG_EDGE *e = *succ_iter;
                     
                        float freq = 0.0;
                        freq = total_freq*e->Prob();
                        NODE_PROF p;
                        Is_True(freq != 0.0,("Computed frequency is zero in compute node profile info!"));
                        p.freq = freq;
                        Is_True(e->Prob() != 0.0,("Probability is zero in compute node profile info!"));
                        p.prob = e->Prob();
                        p.src  = e->Src();
                        p.dest = e->Dest();
                        node_profs.push_back(p);
                    }
                } else {
                    for (CFG_SUCC_EDGE_ITER succ_iter(*iter);
                        succ_iter != 0;++succ_iter) {
                        REGIONAL_CFG_EDGE *e = *succ_iter;
                        NODE_PROF p;
                        if (e->Freq() == 0.0) {
                            DevWarn("Old frequency is zero in compute node profile info!");
                        }
                        p.freq = e->Freq();
                        if (e->Prob() == 0.0) {
                            DevWarn("Old prob is zero in compute node profile info!");
                        }

                        p.prob = e->Prob();
                        p.src  = e->Src();
                        p.dest = e->Dest();
                        node_profs.push_back(p);
                    }
                }

                queue.push(*iter);
            }
        }
    }
}

//=============================================================================
//
//  Compute_BBs_Need_Duplicate
//  
//=============================================================================
void 
REGIONAL_CFG::Compute_BBs_Need_Duplicate(BB_VECTOR& need_duplicate,BS *unduplicated,NODE_VECTOR dup) {
    Find_BBs_From_Nodes(need_duplicate,dup);
    
    for (BB_VECTOR_ITER iter = need_duplicate.begin();
        iter != need_duplicate.end();iter++) {
        BS_Union1D(unduplicated,BB_id(*iter),&_m);
    }
}

//=============================================================================
//
//                  Class TOPOLOGICAL_REGIONAL_CFG_ITER
//
//=============================================================================

//=============================================================================
// 
//  Set_Cur(REGIONAL_CFG *cfg)
//
//  this is the private member function, used in constructor function
//
//=============================================================================
void 
TOPOLOGICAL_REGIONAL_CFG_ITER::Set_Cur(REGIONAL_CFG *cfg){ 
    //----------------------------------------------------------------
    // find all nodes which has no preds, and added them in _candi_s 
    //----------------------------------------------------------------
    NODE_ALLOC  temp_alloc(&cfg->_m);
    NODE_VECTOR node_set(temp_alloc);
    node_set = cfg->_node_set; 
    
    NODE_VECTOR_ITER iter;  
    for(iter = node_set.begin(); iter != node_set.end(); iter++){ 
        REGIONAL_CFG_NODE *node = *iter; 
        
        if(node->First_Pred() == NULL) 
            _candi_s.push(node); 
    }
    _cur = _candi_s.front(); // pop _candi_s to _cur 
    _candi_s.pop(); 
    Set_Visited(_cur);       // set _cur is visited 
} 
 
 
TOPOLOGICAL_REGIONAL_CFG_ITER & 
TOPOLOGICAL_REGIONAL_CFG_ITER::operator ++(void){ 
    //--------------------------------------------------
    // scan all of succ node of _cur 
    // if all pred nodes of this succ node is Visited 
    // push this succ node in _candi_s 
    //--------------------------------------------------
    for(CFG_SUCC_NODE_ITER iter(_cur); iter!=0; ++iter){ 
        REGIONAL_CFG_NODE *node = *iter; 
        BOOL all_preds_visited = 1; 

        for(CFG_PRED_NODE_ITER pred_iter(node); pred_iter!=0; ++pred_iter){
        
            if(!Visited(*pred_iter)){ 
                all_preds_visited = 0; 
                break; 
            } 
        } 

        if(all_preds_visited) 
            _candi_s.push(node); 
    }

    if(_candi_s.empty()){
        _cur = NULL;
    }else{ 
        _cur = _candi_s.front(); // pop _candi_s to _cur 
        _candi_s.pop(); 
        Set_Visited(_cur);       // set _cur is visited 
    }
    return *this; 
} 

//=============================================================================
//
//                  Class REVERSE_TOPO_REGIONAL_CFG_ITER
//
//=============================================================================

//=============================================================================
// 
//  Set_Cur(REGIONAL_CFG *cfg)
//
//  this is the private member function, used in constructor function
//
//=============================================================================
void 
REVERSE_TOPO_REGIONAL_CFG_ITER::Set_Cur(REGIONAL_CFG *cfg){ 
    //----------------------------------------------------------------
    // find all nodes which has no succs, and added them in _candi_s 
    //----------------------------------------------------------------
    NODE_ALLOC  temp_alloc(&cfg->_m);
    NODE_VECTOR node_set(temp_alloc);
    node_set = cfg->_node_set; 
    
    NODE_VECTOR_ITER iter;  
    for(iter = node_set.begin(); iter != node_set.end(); iter++){ 
        REGIONAL_CFG_NODE *node = *iter; 
        
        if(node->First_Succ() == NULL) 
            _candi_s.push(node); 
    }
    
    _cur = _candi_s.front(); // pop _candi_s to _cur 
    _candi_s.pop(); 
    Set_Visited(_cur);       // set _cur is visited 

} 
 
//=============================================================================
// 
//  operator ++ 
//
//  The ++ operator get the next regional cfg node.
//
//=============================================================================
REVERSE_TOPO_REGIONAL_CFG_ITER & 
REVERSE_TOPO_REGIONAL_CFG_ITER::operator ++(void){ 
    //-------------------------------------------------
    // scan all of pred node of _cur 
    // if all succ nodes of pred succ node is Visited 
    // push this pred node in _candi_s 
    //-------------------------------------------------
    for(CFG_PRED_NODE_ITER iter(_cur); iter!=0; ++iter){ 
        REGIONAL_CFG_NODE *node = *iter; 
        BOOL all_succs_visited = 1; 

        for(CFG_SUCC_NODE_ITER succ_iter(node); succ_iter!=0; ++succ_iter){
        
            if(!Visited(*succ_iter)){ 
                all_succs_visited = 0; 
                break; 
            } 
        } 

        if(all_succs_visited) 
            _candi_s.push(node); 
    }
    if(_candi_s.empty()){
        _cur = NULL;
    }else{ 
        _cur = _candi_s.front();  // pop _candi_s to _cur 
        _candi_s.pop(); 
        Set_Visited(_cur);        // set _cur is visited 
    }
    return *this; 
}


//=============================================================================
//
//                  Class PREORDER_REGIONAL_CFG_ITER
//
//=============================================================================

//=============================================================================
// 
//  Set_Cur(REGIONAL_CFG *cfg)
//
//  This is a private member function, used in constructor function
//
//=============================================================================
void 
PREORDER_REGIONAL_CFG_ITER::Set_Cur(REGIONAL_CFG *cfg){ 
    NODE_ALLOC  temp_alloc(&cfg->_m);
    NODE_VECTOR node_set(temp_alloc);
    node_set = cfg->_node_set; 
    NODE_VECTOR_ITER iter;  

    for(iter = node_set.begin(); iter != node_set.end(); iter++){ 
        REGIONAL_CFG_NODE *node = *iter; 
        
        if(node->First_Pred() == NULL) 
            _candi_s.push(node); 
    }
    
    _cur = _candi_s.front(); 
    _candi_s.pop(); 
} 
 
//=============================================================================
// 
//  operator ++ 
//
//  The ++ operator get the next regional cfg node.
//
//=============================================================================
PREORDER_REGIONAL_CFG_ITER & 
PREORDER_REGIONAL_CFG_ITER::operator ++(void){ 
     
    if (_cur != NULL) {
        
        for(CFG_SUCC_NODE_ITER iter(_cur); iter != 0; ++iter){ 
            REGIONAL_CFG_NODE *node = *iter; 
        
            if (!Visited(node)) {
                _candi_s.push(node); 
                Set_Visited(node);
            }    
        }
    }

    if(_candi_s.empty()){
        _cur = NULL;
    }else{ 
        _cur = _candi_s.front();
        _candi_s.pop(); 
    }
    return *this; 
}    

//=============================================================================
// 
// REGION class
//
//=============================================================================
//=============================================================================
//
//  Find_Common_Parent(REGION *r)
//
//  See region.h for interface description.
//
//=============================================================================
REGION * 
REGION::Find_Common_Parent(REGION *r) {
    REGION *parent = this;
    
    while (parent){
        
        if (r->Is_Contained_By(parent) || parent == r) {
            return parent;
        }
        
        parent = parent->Parent();
    }
    
    Is_True((parent==NULL), ("there should be a common parent"));

    return NULL;     
}  

//=============================================================================
//
//  Is_Contained_By(REGION *r)
//
//  See region.h for interface description.
//
//=============================================================================
BOOL 
REGION::Is_Contained_By(REGION *r) {
        
    REGION *p = this->Parent();
    
    while (p) {
        if (p == r) return TRUE;
        p = p->Parent();
    }
    
    return FALSE;
}
     
//=============================================================================
//  See region.h for interface description.
//=============================================================================
BOOL 
REGION::Is_Kid_Region_Of(REGION *r) {
        
    for (REGION_KID_ITER iter(r); iter != 0; ++iter) {
        if (this == *iter) return TRUE;
    }
    
    return FALSE;
}

//=============================================================================
//  See region.h for interface description.
//=============================================================================
BB * 
REGION::Edge_Splitting(BB *from, BB *to){

    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile," from->Id is %d, to->Id is %d\n", BB_id(from),BB_id(to));
    }

    Is_True((from!=NULL && to!=NULL),("only to edge splitting for BBs"));
          
    OP *op = BB_last_op(from); // Get the last OP and the number of successors.
    INT nsuccs = BBlist_Len(BB_succs(from));
     
    INT tfirst, tcount;        // Get some info about the last op.
    Is_True(op != NULL,("BB %d to BB %d has no branch op.\n",BB_id(from),BB_id(to)));
    CGTARG_Branch_Info(op, &tfirst, &tcount);
    //---------------------------------------------
    // Do not splitting indirect goto and vargoto
    //---------------------------------------------
    if(tcount==0) {
        return NULL;
    }
    if (nsuccs != 2) {
	    return NULL;
    }
    Is_True(nsuccs == 2, ("Edge_Splitting:from_BB:%d has %d succs (should be 2 succs)",BB_id(from), nsuccs));
    Is_True(BB_has_label(to),("Edge_Splitting:joined bb should has a label here"));

    TN *tn = OP_opnd(op,tfirst);  // get the label of from
    Is_True(tn != NULL,("The tn is NULL in Edge Splitting."));
    LABEL_IDX label = TN_label(tn);

    BBLIST *edge = BBlist_Find_BB(BB_succs(from), to);
    REGIONAL_CFG *cfg = &_cfg;
    BB *new_bb ;
    
    if(BB_next(from)==to){//to is fall through of from
        
        if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
            fprintf(TFile,"BB%d is fall through of BB%d\n",BB_id(to),BB_id(from));
        } 
        //-----------------------------------------------------------
        // It can not hoisted before if, it effect the result of if
        //----------------------------------------------------------- 
        float prob = Prob_Local( from, to );
        new_bb = RGN_Gen_And_Insert_BB(from, to, cfg);
        BB_freq( new_bb ) = prob * BB_freq( from );
    
    }else{
        Is_True(Is_Label_For_BB(label,to),("edge_splitting:to node should has the same label with if here"));
        float prob = Prob_Local( from, to );
        new_bb = RGN_Gen_And_Insert_BB(from, to, cfg);
        BB_freq( new_bb ) = prob * BB_freq( from );
        Add_Goto_Op(new_bb,to);
        LABEL_IDX new_label=Gen_Label_For_BB(new_bb);
        TN *new_tn = Dup_TN(tn);
        Set_OP_opnd(op,tfirst,new_tn);
        Set_TN_label(new_tn,new_label);
    }

    if (!CG_localize_tns) {
        GRA_LIVE_Compute_Liveness_For_BB(new_bb);
    }

    Set_BB_edge_splitting (new_bb);

    return new_bb;
}

//=============================================================================
//  See region.h for interface description.
//=============================================================================
void
REGION::Edge_Splitting(){
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile, "*** Begin Edge_Splitting *** \n");
    }

    REGIONAL_CFG *cfg = &_cfg;
    
    for(TOPOLOGICAL_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter){
        REGIONAL_CFG_NODE *node = *iter;
        if(node->Is_Region())   continue;
        if(node->Succ_Num() < 1)  continue;
        if((node->Succ_Num() == 1) && (!node->Is_Exit())) continue;
	
	//------------------------------------------------------------
	// If node has one succ in regional cfg and is a exit node,it
        // must has more than one succ in global cfg,therefore,this 
        // edge maybe a critical edge.If it is not a exit node,just
        // skip this node.
        //------------------------------------------------------------

        for (REGIONAL_CFG_EDGE *edge = node->First_Succ();
            edge!=NULL; edge = edge->Next_Succ()){
            REGIONAL_CFG_NODE *succ_node = edge->Dest();
        
            if(succ_node->Is_Region())   continue;
            if(succ_node->Pred_Num()<=1) continue;
	    BB *new_bb = Edge_Splitting(node->BB_Node(),succ_node->BB_Node());
        }
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile, "*** Finish Edge_Splitting *** \n");
    }
}

//============================================================================= 
// 
// REGION_TREE class
//
//=============================================================================  

//=============================================================================
//
// Build_Regional_Cfg
// 
// This is a private function of class REGION_TREE, used in REGION_TREE(BB *)
// to build regional cfg according global cfg.It simply map global cfg to 
// regional cfg.The entries and exits also computed at the same time. 
//
//=============================================================================
INT32
REGION_TREE::Build_Regional_Cfg(BB *first_bb){
    typedef mempool_allocator<REGIONAL_CFG_NODE *>   NODE_ALLOC;
    typedef std::vector<REGIONAL_CFG_NODE *, NODE_ALLOC>  NODE_VECTOR;
    
    extern BB_NUM   PU_BB_Count;     
    NODE_VECTOR     node_container(PU_BB_Count+2, (REGIONAL_CFG_NODE *)NULL, 
                                  NODE_ALLOC(&_m));
    
    REGIONAL_CFG *local_cfg = _root->Regional_Cfg(); 
    Is_True((local_cfg != NULL), ("build_regional_cfg")); 
    //-----------------------------------------------------------------    
    // According global cfg,create and add all of regional_nodes and
    // regional_edges in regional_cfg
    //-----------------------------------------------------------------
    INT32 prev_bb_nums = 0;
    BB    *bb;

    for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {  
        prev_bb_nums++;   
        //-----------------------------------------------------------------
        // If there is no regional_cfg_node which BB is bb,create and add
        // this regional_cfg_node in regional_cfg
        //-----------------------------------------------------------------
        REGIONAL_CFG_NODE *node = node_container[BB_id(bb)];
        
        if(node == NULL) {
            node = local_cfg->Add_Node(bb); // this node is not existed
            node_container[BB_id(bb)] = node;
        }
        //--------------------------------------------------------------------
        // For each pred_bb of bb if there is no regional_cfg_node which BB
        // is pred_bb,create and add this regional_cfg_node in regional_cfg.
        // Create and add regional_cfg_edge(pred_node, node);
        //--------------------------------------------------------------------
        BBLIST *pred_list;

        FOR_ALL_BB_PREDS(bb, pred_list) { 
            BB *pred = BBLIST_item(pred_list);
            REGIONAL_CFG_NODE *pred_node = node_container[BB_id(pred)];
        
            if(pred_node == NULL) {
                pred_node = local_cfg->Add_Node(pred);
                node_container[BB_id(pred)] = pred_node;
            }
            
            local_cfg->Add_Edge(pred_node, node);
        } 
    } 
    //--------------------------------------------------------------------------
    // Here,assertion must be added to decide whether the global cfg has only 
    // one entry?We decide the entries as those nodes without preds and exits
    // as those nodes without succs. 
    //--------------------------------------------------------------------------
    for (PREORDER_REGIONAL_CFG_ITER iter(local_cfg);iter != 0;++iter) {
        REGIONAL_CFG_NODE *node = *iter;
      
        if (node->First_Pred() == NULL) {
            local_cfg->Add_To_Entries(node);
        } 
        
        if (node->First_Succ() == NULL)
        { 
            local_cfg->Add_To_Exits(node);
        }
    }

    return prev_bb_nums;
}

//=============================================================================
//
// REGION_TREE
// Function : REGION_TREE::REGION_TREE
//  Input : 
//     - BB first_bb,the first BB in function.
//  Output :
//     - A region tree with its every node represent a REGION.  
//  Description:
//     TO BE ADDED.
//
//=============================================================================
REGION_TREE::REGION_TREE(BB *first_bb):_region_set(REGION_ALLOC(&_m)) {
    INT32 prev_bb_nums = 0;
    INT32 after_bb_nums = 0;
    float total_exit_prob = 0.0;

    Create_BB_Node_Map();
    _seq_num = 0;
    //---------------------------------------
    // Add root region in this region_tree
    //---------------------------------------
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile, "*** Begin Build Regional Cfg *** \n");
    }

    _root = Add_Region(); 
    _root->Region_Type(ROOT); 
    //------------------------------------------    
    // Build local cfg according to glocal cfg
    //------------------------------------------
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        draw_global_cfg();
    }
    
    prev_bb_nums = Build_Regional_Cfg(first_bb);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile, "*** Finished construct root region *** \n*** Begin Interval Process");
    }
    //------------------------------------------
    // An assumption,the entry can be only one.
    //------------------------------------------
    Process_Intervals(_root);  //Deal with loops and irreducible parts.
    
    //-------------------------------------------
    // For overlapped loops,set them as NO
    // FURTHER OPTIMIZATION regions.
    //------------------------------------------
    REGIONAL_CFG_NODE *loop_head = NULL;
    REGIONAL_CFG_NODE *loop_tail = NULL;
    for (INNERMOST_REGION_FIRST_ITER iter (this) ; iter != 0; ++iter) {
        if ((*iter)->Regional_Cfg()->Num_Of_Entries()>1) {
             REGION *rgn = *iter;
             rgn->Attribute(NO_FURTHER_OPTIMIZATION);
             continue;
        }
 
        if ((*iter)->Region_Type() == LOOP) {
            LOOP_REGION *r = (LOOP_REGION *)(*iter);
            loop_head = r->Loop_Head();
            loop_tail = r->Loop_Tail();
            if (loop_head->Is_Region()) {
                 REGION *kid = loop_head->Region_Node();
                 if (kid->Region_Type() == LOOP) {
                      kid->Attribute(NO_FURTHER_OPTIMIZATION);
                      r->Attribute(NO_FURTHER_OPTIMIZATION);
                  }
            }
            if (loop_tail->Is_Region()) {
                REGION *kid = loop_tail->Region_Node();
                if (kid->Region_Type() == LOOP) {
                    kid->Attribute(NO_FURTHER_OPTIMIZATION);
                    r->Attribute(NO_FURTHER_OPTIMIZATION);
                }
            }
        }
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile, "*** Finished interval processing *** \n");
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        draw_region_tree(_root);
    }
} 

//=============================================================================
//
// REGION_TREE
// 
// Constructor function of class REGION_TREE.
//
//=============================================================================
REGION_TREE::~REGION_TREE() {
    Delete_BB_Node_Map();
}

//=============================================================================
//
// Process_Intervals
// 
// Process all intervals in a regional CFG,and form them regions.
// Then do find global cycles again.
//
//=============================================================================
void
REGION_TREE::Process_Intervals(REGION *r) {
    INTERVAL_PROCESSOR inter(r);
    inter.Process();  
	
    if (!global_cycles.empty()) {
        global_cycles.clear();
    }
   
    //-------------------------------------------
    // For overlapped loops,set them as NO 
    // FURTHER OPTIMIZATION regions.
    //------------------------------------------
    /*REGIONAL_CFG_NODE *loop_head = NULL;
    REGIONAL_CFG_NODE *loop_tail = NULL;
    for (INNERMOST_REGION_FIRST_ITER iter (this) ; iter != 0; ++iter) {
        if ((*iter)->Region_Type() == LOOP) {
            LOOP_REGION *r = (LOOP_REGION *)(*iter);  
            loop_head = r->Loop_Head();
            loop_tail = r->Loop_Tail();
            if (loop_head->Is_Region()) {
                REGION *kid = loop_head->Region_Node();
                if (kid->Region_Type() == LOOP) {
                    kid->Attribute(NO_FURTHER_OPTIMIZATION);
                    r->Attribute(NO_FURTHER_OPTIMIZATION);
                }
            }
 
            if (loop_tail->Is_Region()) {
                REGION *kid = loop_tail->Region_Node();
                if (kid->Region_Type() == LOOP) {
                    kid->Attribute(NO_FURTHER_OPTIMIZATION);
                    r->Attribute(NO_FURTHER_OPTIMIZATION);
                }
            } 
        }
    } */ 

    GLOBAL_CYCLES_FINDER cycle_finder;
    cycle_finder.Find_Global_Cycles(global_cycles);

    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The global cycles are:\n");
        cycle_finder.Print(global_cycles);
    }
}

//========================================================================
//  Add_Region
//
//  In this function,region tree will new a region and insert it to the 
//  region set.The region will be allocated a unique sequential id.Its 
//  region type will be set to UNKNOWN.
//  
//=========================================================================
REGION*
REGION_TREE::Add_Region() {
    REGION *r=CXX_NEW(REGION(),&_m);
    Insert(r);
    
    return r;
}  

//========================================================================
//  Add_Loop_Region
//
//  In this function,region tree will new a loop region and insert it to 
//  the region set.The region will be allocated a unique sequential id.Its 
//  region type will be set to LOOP_REGION.
//  
//=========================================================================
LOOP_REGION*
REGION_TREE::Add_Loop_Region(void) {
    LOOP_REGION *r=CXX_NEW(LOOP_REGION(),&_m);
    Insert(r);
    
    return r;
}  

//========================================================================
//  Shrink
//
//  Function : REGION_TREE::Shrink
//  Input : 
//     - REGION parent, the parent region from where nodes are selected.
//     - REGION kid,the newly formed region from the selected nodes. 
//     - NODE_VECTOR nodes,nodes selected out to shrink.
//
//  Description:
//     Add the nodes to kid region,shrink it a regional cfg node in parent 
//     region's regional cfg,and update cfg of kid and parent's cfg.At the 
//     same time,adjust the region tree.
//     Scan all nodes which will be inserted to the newly formed region and
//     delete all edges connected from kid region to nodes of parent region.
//     If the inserted node is a region node,adjust the region as the kid's
//     kid region.Update the entries and exits of parent region and kid 
//     region.
//  
//=========================================================================
void
REGION_TREE::Shrink (REGION *parent,REGION *kid,NODE_VECTOR nodes) {
    
    REGIONAL_CFG      *cfg       = parent->Regional_Cfg();
    REGIONAL_CFG      *kid_cfg   = kid->Regional_Cfg(); 
    REGIONAL_CFG_NODE *kid_node  = cfg->Add_Node(kid); 
    BOOL              is_exit;
    BOOL              is_entry;
    BOOL              kid_is_entry = FALSE;
    BOOL              kid_is_exit  = FALSE;

    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        is_exit  = FALSE;
        is_entry = FALSE; 
        
        for (CFG_PRED_EDGE_ITER pred_iter(node);pred_iter != 0;++pred_iter) {
            REGIONAL_CFG_EDGE *e    = *pred_iter;      
            REGIONAL_CFG_NODE *pred = e->Src();
            
            if (Find_In_Vector(pred,nodes) == nodes.end()) {
                is_entry = TRUE;
                cfg->Del_Edge(e);
                cfg->Add_Edge(pred,kid_node);
            }
        }        
                
        for (CFG_SUCC_EDGE_ITER succ_iter(node);succ_iter != 0;++succ_iter) {
            REGIONAL_CFG_EDGE *e    = *succ_iter;          
            REGIONAL_CFG_NODE *succ = e->Dest();
            
            if (Find_In_Vector(succ,nodes) == nodes.end()) {
                is_exit = TRUE;
                cfg->Del_Edge(e);
                cfg->Add_Edge(kid_node,succ);
            } 
         }  

         //-------------------------------------
         // If node is exit and its succ = NULL
         //-------------------------------------
         if (node->Is_Exit()) {
             kid_is_exit  = TRUE;
             is_exit      = TRUE;
             cfg->Remove_From_Exits(node);
         } 

         //-------------------------------------
         //If node is entry and its pred = NULL
         //-------------------------------------
         if (node->Is_Entry()) {
             kid_is_entry = TRUE;
             is_entry     = TRUE;
             cfg->Remove_From_Entries(node);
         }
         
         if (is_exit)  {
             kid_cfg->Add_To_Exits(node);
         }

         if (is_entry)  {
             kid_cfg->Add_To_Entries(node);
         }    

         if (node->Is_Region()) {
             REGION *r = node->Region_Node();
             parent->Del_Kid(r);
             kid->Add_Kid(r);
         }    
         
      cfg->Erase(node);
      kid_cfg->Insert(node);
    }  

    /*Because in loop region, back edge has been move, so if the parent region
    is a loop region, loop head and loop nodes must test to see if they are 
    exit or entry */ 
    if (parent->Region_Type() == LOOP){
        LOOP_REGION *loop;
        loop = (LOOP_REGION *) parent;
        if ((Find_In_Vector(loop->Loop_Head(),nodes) == nodes.end()) 
            && (Find_In_Vector(loop->Loop_Tail(),nodes) != nodes.end())){
            kid_cfg->Add_To_Exits(loop->Loop_Tail());
        }else if ((Find_In_Vector(loop->Loop_Head(),nodes) != nodes.end()) 
            && (Find_In_Vector(loop->Loop_Tail(),nodes) == nodes.end())){
            kid_cfg->Add_To_Entries(loop->Loop_Head());
        }
    }
   
    if (kid_is_entry) {
        cfg->Add_To_Entries(kid_node);
    } 
    
    if (kid_is_exit) {
        cfg->Add_To_Exits(kid_node);
    }
} 
 
//=====================================================================
//
//  Add_Loop_Region(REGION*,NODE_VECTOR)
//
//  See region.h for description.
//
//=====================================================================
LOOP_REGION*
REGION_TREE::Add_Loop_Region(REGION *parent,REGIONAL_CFG_EDGE *backedge,
             NODE_VECTOR nodes) {
    LOOP_REGION *loop = CXX_NEW(LOOP_REGION(),&_m);
    Insert(loop);
    
    Shrink(parent,loop,nodes);
    REGIONAL_CFG *cfg = loop->Regional_Cfg();
    REGIONAL_CFG_NODE *loop_head = backedge->Dest();
    REGIONAL_CFG_NODE *loop_tail = backedge->Src();
    loop->Loop_Head(loop_head);
    loop->Loop_Tail(loop_tail);
    cfg->Del_Edge(backedge);
    parent->Add_Kid(loop);
        
    return loop;
}

//=====================================================================
//
//  Add_MEME_Region
//
//  See region.h for description.
//
//=====================================================================
MEME_REGION*
REGION_TREE::Add_MEME_Region(REGION *parent,NODE_VECTOR nodes) {
    MEME_REGION *meme = CXX_NEW(MEME_REGION(),&_m);
    Insert(meme);
    
    Shrink(parent,meme,nodes);
    parent->Add_Kid(meme);
    
    return meme;
}

//=====================================================================
//
//  Add_SEME_Region
//
//  See region.h for description.
//
//=====================================================================
SEME_REGION*
REGION_TREE::Add_SEME_Region(REGION *parent,NODE_VECTOR nodes) {
    SEME_REGION *seme = CXX_NEW(SEME_REGION(),&_m);
    Insert(seme);

    Shrink(parent,seme,nodes);
    parent->Add_Kid(seme);

    return seme;
}

//=====================================================================
//
//  Add_Improper_Region(REGION*,NODE_VECTOR)
//
//  See region.h for description.
//
//=====================================================================
IMPROPER_REGION*
REGION_TREE::Add_Improper_Region(REGION *parent,NODE_VECTOR nodes) {
    IMPROPER_REGION *improper = CXX_NEW(IMPROPER_REGION(),&_m);
    Insert(improper);

    Shrink(parent,improper,nodes);
    parent->Add_Kid(improper);

    return improper;
}
//=====================================================================
//
//  Decomposition
//
//=====================================================================
void 
REGION_TREE::Decomposition() {
    RGN_FORM_PAR par;
    par.size.max_bb_num = 20;
    par.size.max_op_num = 120;
    par.exit_prob = 0.0;
    par.dup_ratio = 1.0;
    par.max_dup_num = 0;
    if (IPFEC_Enable_Tail_Duplication != 0) {
        par.dup_ratio = 1 + (float)(IPFEC_Enable_Tail_Duplication/10);
    } else {
        par.dup_ratio = 1.0;
    }
	  
    if (IPFEC_Enable_Exit_Probability != 0) {
        par.exit_prob = (float)(IPFEC_Enable_Exit_Probability/10);
    } else {
        par.exit_prob = 0.0;
    }
    
    REGION_VECTOR region_vector;
    for(INNERMOST_REGION_FIRST_ITER iter(this); iter!=0; ++iter) {
        if ((*iter)->Region_Type() != IMPROPER) {
            region_vector.push_back(*iter);
        }
    }

    FREQ_Check_Consistency("REGION FORMATION PROFILE CHECK");
   
    for (REGION_VECTOR::iterator iter = region_vector.begin();
        iter != region_vector.end();iter++) {
	REGION *r = *iter;
	//Before do decomposition,we must make it sure that the 
        Decompose_Region_To_SEME(r,par); 
    }
      
    FREQ_Check_Consistency("REGION FORMATION PROFILE CHECK");
    
    Statistic();
      
    Set_Loop_Head_Tail();
}


//=====================================================================
//
//  Decompose_To_MEME_Region
//
//=====================================================================
void 
REGION_TREE::Decompose_Region_To_MEME(REGION *r,RGN_SIZE size) {
    INT16 attribute;
    INT32 bb_num = 0;
    INT32 op_num = 0;
    
    attribute = r->Attribute(); 
    Is_True(!(attribute&RIGID&NO_FURTHER_OPTIMIZATION),("Can not decompose this region.\n"));
    
    REGIONAL_CFG *cfg = r->Regional_Cfg();
    //-------------------------------------------------------------------
    // Compute the number of ops and bbs of the region to be decomposed.
    //-------------------------------------------------------------------
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter) {
        REGIONAL_CFG_NODE *node = *iter;
        bb_num++;                        

        if (!node->Is_Region()) {
            op_num += BB_length((node->BB_Node()));
        }
    }

    INT32 max_bb_num = size.max_bb_num;
    INT32 max_op_num = size.max_op_num;
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR bad_nodes(temp_alloc);

    while ((bb_num > max_bb_num) && (op_num > max_op_num)) {    
        TEMP_RGN meme_rgn;
        meme_rgn.bb_num = 0;
        meme_rgn.op_num = 0;
        meme_rgn.dup_bb_num = 0;
        meme_rgn.dup_op_num = 0;
        meme_rgn.main_entry = NULL;
        meme_rgn.main_exit  = NULL;
        meme_rgn.exit_prob  = 0;
         
        NODE_VECTOR nodes(temp_alloc);
        meme_rgn.nodes = nodes;

        cfg->Find_MEME_Nodes(meme_rgn,size,bad_nodes);
        //-----------------------------------------------------------
        // Cycles may be generated in MEME region formation process
        // in the regional cfg because of multiple entries.
        //-----------------------------------------------------------
        Add_MEME_Region(r,nodes);

        bb_num = bb_num - meme_rgn.bb_num;
        op_num = op_num - meme_rgn.op_num;
    }    
}

//=====================================================================
//
//  Decompose_To_SEME_Region
//
//  Function : REGIONAL_CFG::Decompose_To_SEME_Region
//  Input : 
//     - REGION r,the region to be decomposed.
//     - RGN_FORM_PAR par,the parameters which control the formed regions'
//       size and shape.
//
//  Output : 
//     - No output.
//
//  Description:
//     Decompose region r into smaller SEME regions until the region r
//     satisfy the size requirement.The duplication ratio,exit probability
//     and region size(the number of ops in a region,the number of bbs in a
//     region) can be controlled by researchers for different requirement.
//     The required region size is decided by max_bb_num and max_op_num,main
//     exit probability by exit_prob and dup_ratio control the code expansion
//     of tail duplication for SEME region formation.
//
//=====================================================================
void 
REGION_TREE::Decompose_Region_To_SEME(REGION* r,RGN_FORM_PAR par) {
    INT16 attribute;
    INT32 bb_num = 0;
    INT32 op_num = 0;
    
    INT32 max_bb_num = par.size.max_bb_num;
    INT32 max_op_num = par.size.max_op_num;
    //-----------------------------------------------------------------------
    // Decide whether the region can be further decomposed or transformed 
    // according to its attributes set.
    //-----------------------------------------------------------------------
    attribute = r->Attribute(); 
    Is_True(!(attribute&RIGID&NO_FURTHER_OPTIMIZATION),("Can not decompose this region."));
    
    REGIONAL_CFG *cfg = r->Regional_Cfg();
    //------------------------------------------------------------------
    //Compute the number of ops and bbs of the region to be decomposed.
    //------------------------------------------------------------------
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter) {
        REGIONAL_CFG_NODE *node = *iter;
        
        bb_num++;                        
        if (!node->Is_Region()) {
            op_num += BB_length((node->BB_Node()));
        }
    }
    //----------------------------------------------
    // Set the max number of BBs can be duplicated.
    //----------------------------------------------
    par.max_dup_num = (INT32)((par.dup_ratio-1.0)*bb_num);
    
    INT32 n = 0;
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR bad_nodes(temp_alloc);
    //-----------------------------------------------------
    //Now the termination condition is not quite correct.
    //-----------------------------------------------------
    while ((bb_num > max_bb_num) && (op_num > max_op_num)) {    
        TEMP_RGN seme_rgn;
        Initialize_Temp_Rgn(seme_rgn,_m);

        if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
            fprintf(TFile,"Begin to find seme nodes.\n");
        }
    
        cfg->Find_SEME_Nodes(seme_rgn,par,bad_nodes);
        
        if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
            fprintf(TFile,"SEME nodes found.\n");
        }
        
        NODE_VECTOR_ITER iter = (seme_rgn.nodes).begin();
        if (((seme_rgn.nodes).size() == 1)&&(*iter)->Is_Region()) {
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                fprintf(TFile,"Found SEME nodes has bad node,discard.\n");
            }
            
            bad_nodes.push_back((*iter));
        } else {
            
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
                Print_Node_Vector(seme_rgn.nodes);
            }
            REGION *seme = Add_SEME_Region(r,seme_rgn.nodes);
            if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
                fprintf(TFile,"SEME region added,REGION ID:%d \n",seme->Id());
            }
      
            n++;
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {   
                fprintf(TFile,"THE %dTH SEME REGION ADDED.\n",n);
                draw_region_tree(_root);
            }

            bb_num = bb_num - seme_rgn.bb_num + seme_rgn.dup_bb_num;
            op_num = op_num - seme_rgn.op_num + seme_rgn.dup_op_num;
            par.max_dup_num -= seme_rgn.dup_bb_num;
            //----------------------------------------------- 
            // There is some possibility that the duplicated 
            // BB number greater than the max dup num for we
            // also insert some br BBs.
            //-----------------------------------------------
            if (par.max_dup_num < 0) {
                par.max_dup_num = 0;
            }
        }
    }   
}

//=====================================================================
//  See region.h for interface description.
//=====================================================================
void 
REGION_TREE::Del_Region(REGION *r) {
    Is_True(r!=_root, ("can not delete the root region"));
    //------------------------------------------------------
    // 1. cut all relations between this region and others
    //------------------------------------------------------
    REGION *parent = r->Parent();    
    REGIONAL_CFG_NODE *node = r->Regional_Cfg_Node();
    //------------------------------------------------------------------------ 
    // 1.1. del all incoming and outcoming edges about this
    // r->Regional_Cfg_Node  
    //-------------------------------------------------------------------------
    if(node->Is_Entry()){ // del incoming edges
        BB_VECTOR *entries = CXX_NEW(BB_VECTOR(), &_m);
        Collect_Entry_BBs(r, entries);
        
        for(BB_VECTOR_ITER iter=entries->begin(); iter!=entries->end(); iter++){
            BBLIST *pred_list;
        
            FOR_ALL_BB_PREDS(*iter, pred_list) { 
                BB *pred_bb = BBLIST_item(pred_list);  
                REGION *pred_rgn = Home_Region(pred_bb);
            
                if(! (pred_rgn==parent || pred_rgn->Is_Contained_By(parent))){
                    REGION *com_par = pred_rgn->Find_Common_Parent(parent);
                    REGIONAL_CFG_NODE *pred_node, *parent_node;
                
                    if(com_par == pred_rgn)
                        pred_node = Regional_Cfg_Node(pred_bb);
                    else
                        pred_node = Regional_Cfg_Node(pred_rgn->Regional_Cfg_Node(),com_par);
                    
                    if(com_par == parent)
                        parent_node = node;
                    else
                        parent_node = Regional_Cfg_Node(parent->Regional_Cfg_Node(), com_par);

                    Del_Regional_Cfg_Edge(pred_node, parent_node, com_par);
                }
            }
        }
        CXX_DELETE(entries, &_m);    
    }

    if(node->Is_Exit()){
        BB_VECTOR *exits = CXX_NEW(BB_VECTOR(), &_m);
        Collect_Exit_BBs(r, exits);
        
        for(BB_VECTOR_ITER iter=exits->begin(); iter!=exits->end(); iter++){
            BBLIST *succ_list;
            
            FOR_ALL_BB_SUCCS(*iter, succ_list) { 
                BB *succ_bb = BBLIST_item(succ_list);  
                REGION *succ_rgn = Home_Region(succ_bb);
                
                if(! (succ_rgn==parent || succ_rgn->Is_Contained_By(parent))){
                    REGION *com_par = succ_rgn->Find_Common_Parent(parent);
                    REGIONAL_CFG_NODE *succ_node, *parent_node;
                    
                    if(com_par == succ_rgn)
                        succ_node = Regional_Cfg_Node(succ_bb);
                    else
                        succ_node = Regional_Cfg_Node(succ_rgn->Regional_Cfg_Node(),com_par);
                        
                    if(com_par == parent)
                        parent_node = node;
                    else
                        parent_node = Regional_Cfg_Node(parent->Regional_Cfg_Node(), com_par);

                    Del_Regional_Cfg_Edge(parent_node, succ_node, com_par);
                }
            }
        }
        CXX_DELETE(exits, &_m);    
    }
    
    //---------------------------------------------------------
    // 1.2. remove the regional_cfg(r) from its parent region
    //---------------------------------------------------------
    parent->Regional_Cfg()->Del_Node(node); //r's kids need not be remove from r's cfg
    
    //--------------------------------------
    // 2. delete r and all kid regions of r
    //--------------------------------------
    for(INNERMOST_REGION_FIRST_ITER iter(r); iter!=0; ){
        REGION *region = *iter;
        ++iter;
        Is_True(region->First_Kid()==NULL, ("del region after its kid regions are deleted"));
        //---------------------------------------------------
        // 2.1 remove all regional_cfg_nodes from bb_node_map
        //---------------------------------------------------
        REGIONAL_CFG *cfg = region->Regional_Cfg();
        NODE_VECTOR *nodes = &(cfg->_node_set);
        for(NODE_VECTOR_ITER node_iter=nodes->begin(); node_iter!=nodes->end(); node_iter++){
            if (!(*node_iter)->Is_Region()) 
                BB_MAP_Set(bb_node_map,(*node_iter)->BB_Node(),NULL);
        }
        //--------------------------------------------
        // 2.2. remove region from its parent's kids
        //--------------------------------------------
		REGION *p = region->Parent();
        p->Del_Kid(region); 
		//--------------------------------
        // 2.3. erase it from _region_set
        //--------------------------------
        Erase(region);
        //-----------------
        // 2.4  delete it
        //-----------------
        CXX_DELETE(region, &_m);
    }
}

//=====================================================================
//
//  Set_Loop_Head_Tail
//
//  after region formation, Loop_Head and Loop_tail is regional cfg node
//  of backedge's src and dest
//  reset Loop_Head and Loop_Tail to regional cfg node which is in this 
//  loop region
//=====================================================================
void
REGION_TREE::Set_Loop_Head_Tail(void){
    for(REGION_VECTOR_ITER iter=_region_set.begin(); iter!=_region_set.end(); iter++){
        REGION *rgn = *iter;
        
        if(rgn->Region_Type()==LOOP){
            LOOP_REGION *loop=(LOOP_REGION *)rgn;
            
            for (SEQ_REGIONAL_CFG_ITER node_iter(rgn->Regional_Cfg());
                node_iter!=0; ++node_iter){
                REGIONAL_CFG_NODE *node = *node_iter;

                if(node->First_Pred()==NULL) {
                    loop->Loop_Head(node);
                }
                
                if(node->First_Succ()==NULL) {
                    loop->Loop_Tail(node);
                }    
            } 
        }
    }
}

//=====================================================================
//
//  Statistic 
//
//  Count the value and compute the average value of some aspects of 
//  region tree:
//   Number of Regions 
//   Nest depth(average and max)
//   Region Size
//     Number of instructions in a region
//     Number of nodes in a region
//   Region Frequency
//   Duplicate Ratio 
//   Exit probability
//   Chances for Speculation
//   Chances for If Conversion
//=====================================================================
void
REGION_TREE::Statistic(void) {
    REGION *r  = NULL;
    REGION *r2 = NULL;
    REGION *max_ops_r   = NULL;
    REGION *max_bbs_r   = NULL;
    REGION *max_nodes_r = NULL;

    float average_ops   = 0;
    float average_bbs   = 0;
    float average_nodes = 0;
    INT32 i = 0;
   
    INT32 node_num = 0; INT32 bb_num   = 0; INT32 op_num   = 0;
    INT32 bbs      = 0; INT32 nodes    = 0; INT32 ops      = 0;
    
    INT32 max_ops   = 0; 
    INT32 max_bbs   = 0; 
    INT32 max_nodes = 0;
    
    float aver_depth = 0;
    INT32 max_depth  = 1; INT32 depth_temp = 0;

    INT32 depth = 1;
    
    char *input ;  char tmp_str[100] = "";

    int size = 50000;

    input = (char *)malloc(size * sizeof(char));

    input[0]='\n';input[1]='\0';
    
    for(INNERMOST_REGION_FIRST_ITER region_iter(this);
       region_iter!=0;++region_iter) {
	if(strlen(input) > size - 1000) {
		size += 50000;
		input = (char *)realloc(input, size);
	}
        i++;   
        r   = *region_iter;
        bbs   = 0;
        ops   = 0;
        nodes = 0;        
        REGIONAL_CFG *cfg = r->Regional_Cfg();

        BOOL is_root = FALSE;
        if (r->Region_Type() == ROOT) {
            is_root = TRUE;
        }
        
        for (SEQ_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter) {
            REGIONAL_CFG_NODE *node = *iter;
        
            nodes++;
            node_num++; 

            if (!node->Is_Region()) {
                op_num += BB_length((node->BB_Node()));
                ops    += BB_length((node->BB_Node()));  
                bbs++;
                bb_num++;
            }
        }

        if (ops > max_ops) {
            max_ops   = ops;
            max_ops_r = r;
        } 
        
        if (nodes > max_nodes) {
            max_nodes   = nodes;
            max_nodes_r = r; 
        }
        
        if (bbs > max_bbs) {
            max_bbs   = bbs;
            max_bbs_r = r;
        }

        r2= *region_iter; 
        
        depth= 1;
        while (r2 != this->Root()) {
            r2=r2->Parent();
            depth += 1;
        }   
        
        depth_temp += depth;
        
        if  (depth > max_depth) {
            max_depth = depth;
        }
        
        char type[20] = "";
        if (r->Region_Type() == LOOP) {
            sprintf(type,"LOOP");
        } else if (r->Region_Type() == IMPROPER) {
            sprintf(type,"IMPR");             
        } else if (is_root) {
           sprintf(type,"ROOT");             
        } else {
           sprintf(type,"SEME");
        }

        sprintf(tmp_str,"REGION(%s) %d HAS %d NODES : %d BBS : %d OPS : DEPTH: %d\n",type,r->Id(),nodes,bbs,ops,depth);
        strcat(input,tmp_str);
    }

    average_bbs   = (float)bb_num/i;
    average_nodes = (float)node_num/i;
    average_ops   = (float)op_num/i;
    aver_depth    = (float)depth_temp/i;
    
    if (Get_Trace(TP_PTRACE1, TP_PTRACE1_CG)) {
        sprintf(tmp_str,"THE STATICS ARE:\n");
        strcat(input,tmp_str);
        sprintf(tmp_str,"AVERAGE DEPTH : %f\n",aver_depth);
        strcat(input,tmp_str);
        sprintf(tmp_str,"REGION %d HAS THE MAX ND_NUM %d.\n",max_nodes_r->Id(),max_nodes);
        strcat(input,tmp_str);
        sprintf(tmp_str,"REGION %d HAS THE MAX OP_NUM %d.\n",max_ops_r->Id(),max_ops);
        strcat(input,tmp_str);
        sprintf(tmp_str,"REGION %d HAS THE MAX BB_NUM %d.\n",max_bbs_r->Id(),max_bbs);
        strcat(input,tmp_str);
        sprintf(tmp_str,"AVERAGE ND_NUM : %f.  ",average_nodes);
        strcat(input,tmp_str);
        sprintf(tmp_str,"AVERAGE BB_NUM : %f.  ",average_bbs);
        strcat(input,tmp_str);
        sprintf(tmp_str,"AVERAGE OP_NUM : %f.\n",average_ops);
        strcat(input,tmp_str);
        Generate_Tlog("ARN", "Region formation statistics\n", (SRCPOS)0,
            input,"" , "", "");
    }
    free(input);
        
}

//===================================================================== 
// 
// INNERMOST_REGION_FIRST_ITER class
// 
//===================================================================== 
 
//===================================================================== 
// 
// input:   the root of one region_tree 
// 
// output:  _cur is the leftmost leaf node  
// 
//===================================================================== 
void          
INNERMOST_REGION_FIRST_ITER::Set_Cur(REGION *root) {
    _cur = root;
    REGION *kid = _cur->First_Kid();

    while(kid != NULL){
        _cur = kid; 
        kid = _cur->First_Kid();  
    }
} 

//=============================================================================
// 
//  operator ++ 
//
//  The ++ operator get the next region.
//
//============================================================================= 
INNERMOST_REGION_FIRST_ITER  & 
INNERMOST_REGION_FIRST_ITER::operator ++(void){
    if(_cur == _root){
        _cur = NULL;
    }else{
        if(_cur->Next_Sibling() == NULL){
            _cur = _cur->Parent();
        }else{
            _cur = _cur->Next_Sibling();
            REGION *kid = _cur->First_Kid();
    
            while(kid != NULL){
                _cur = kid;
                kid = _cur->First_Kid();
            }
        }
    }   
    return *this;
} 

void Print_Node_Vector(NODE_VECTOR nodes) {
    for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        
        Is_True(node != NULL,("Node NULL in Print_Node_Vector."));

        if (!node->Is_Region()) {
            fprintf(TFile,"BB Node_%d\n",BB_id(node->BB_Node()));     
        } else {
            fprintf(TFile,"REGION Node_%d\n",(node->Region_Node())->Id());
        }
    }
}

void Print_Ops_In_BB(BB *bb) {
    OPS op_list = bb->ops;

    for (OP* op = op_list.first ; op != NULL ; op = op->next) {
       printf("%s\n",TOP_Name((TOP)(op->opr))); // set the OP's label
    }
}

void Print_BB_Vector(BB_VECTOR bbs) {
    for (BB_VECTOR_ITER iter = bbs.begin();iter != bbs.end();iter++) {
        fprintf(TFile,"BB ID: %d\n",BB_id(*iter));
    }
}

//=============================================================================
//
// Print the regional cfg edge's src and dest node.
//
//=============================================================================
void 
REGIONAL_CFG_EDGE::Print(FILE *f) {
     if (Get_Trace(TP_A_REGION, TT_RGN_CFG_DUMP)){
        fprintf(f, "Src:%d ",_src->Id());
        fprintf(f, "Dest:%d",_dest->Id());
    }
} 

//=============================================================================
//
// Print all regional cfg node information in the regional cfg.
//
//=============================================================================
void 
REGIONAL_CFG_NODE::Print(FILE *f) {
    char *content = NULL;
        
    if ( Get_Trace(TP_A_REGION, TT_RGN_CFG_DUMP)){
        if (Is_Region()) {
            fprintf(f,"Id:%d type:%s ",_id,"REGION");
            fprintf(f,"RGN_id:%d ", this->Region_Node()->Id());
        }
        else {    
            fprintf(f,"Id:%d type:%s ",_id,"BB");
            fprintf(f,"BB_id:%d ",BB_id(this->BB_Node()));
        }

        if(_is_entry) fprintf(f, "entry  ");
        if(_is_exit)  fprintf(f, "exit   ");
        if(!_is_entry && !_is_exit) {
            fprintf(f, "plain  ");
        }    
 
        fprintf(f,"#succs:%d ",_n_succ);
        fprintf(f,"#preds:%d\n",_n_pred);
 
        INT i = 1;
        for (CFG_SUCC_NODE_ITER iter(this);iter != 0; ++iter) {
            REGIONAL_CFG_NODE *node = *iter;
            fprintf(f,"    Succ %d: ",i);
            if (node->Is_Region()) {
                fprintf(f,"type:%s Id:%d\n","REGION",node->Id());
            }
            else {    
                fprintf(f,"type:%s Id:%d\n","BB",node->Id());
            }
            i++;
        }
    }
} 

//=============================================================================
//
//  Print
//
//  Print all regional cfg node information in the regional cfg.
//
//=============================================================================
void 
REGIONAL_CFG::Print(FILE *f) {

     if ( Get_Trace(TP_A_REGION, TT_RGN_CFG_DUMP)){
        fprintf(f,"#REGIONAL_CFG_NODE:%d  ", _seq_num);
        fprintf(f,"#entries:%d  ", (INT)_entries.size());
        fprintf(f,"#exits:%d\n", (INT)_exits.size());
      
        for (NODE_VECTOR_ITER iter = _node_set.begin();
            iter != _node_set.end();iter++) {
            REGIONAL_CFG_NODE *node = *iter;    
            node->Print(f);
        }
    }
}

//=============================================================================
//
//  Print
//
//  Print the information of a region,include its regional cfg.
//
//=============================================================================
void 
REGION::Print(FILE *f) {
    if (Get_Trace(TP_A_REGION, TT_RGN_TREE_DUMP)){
        fprintf(f,"REGION ID:%d ",_id);
        fprintf(f,"#kids:%d ",_n_kids);
        if (_parent != NULL) {
            fprintf(f,"Parent_id:%d ",_parent->Id());
        }    
        else 
        {
            fprintf(f,"Root,has no parent. ");
        }    
        fprintf(f,"type:");
        switch(_type){
            case UNKNOWN:
                fprintf(f,"UNKNOWN ");
                break;
            case ROOT:
                fprintf(f,"ROOT ");
                break;
            case MEME:
                fprintf(f,"MEME ");
                break;
            case SEME:
                fprintf(f,"SEME ");
                break;
            case IMPROPER:
                fprintf(f,"IMPROPER ");
                break;
            case LOOP:
                fprintf(f,"LOOP ");
                fprintf(f,"head_id:%d ",((LOOP_REGION *)this)->Loop_Head()->Id());
                fprintf(f,"tail_id:%d ",((LOOP_REGION *)this)->Loop_Tail()->Id());
                break;
            default:
                fprintf(f,"**error*** ");
                break;
        }
        fprintf(f,"\n");
    }
        
    if ( Get_Trace(TP_A_REGION, TT_RGN_CFG_DUMP)){
        fprintf(f,"REGIONAL CFG :\n");
        _cfg.Print(f);
    }          
}

//=====================================================================
//
//  Print
//
//  Print all regions' information in region tree.
//
//=====================================================================
void
REGION_TREE::Print(FILE *f) {
    REGION *r;
    
    for (INNERMOST_REGION_FIRST_ITER iter(this);iter != 0;++iter) {
        r = *iter;
        r->Print(f);
    }
}
