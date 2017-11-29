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
// Module: region_bb_util.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $ 
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/region_bb_util.cxx,v $
//
//=============================================================================
#include "bb.h"
#include <stack>
#include <vector>
#include <list>
#include "defs.h"
#include "cxx_memory.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "region_bb_util.h"
#include "profile_util.h"
#include "tracing.h"
#include "ipfec_defs.h"

#include "vt_region.h"


//=============================================================================
//
//  Regional_Cfg_Node
//
//  See interface description in region_bb_util.h
//
//=============================================================================
REGIONAL_CFG_NODE *Regional_Cfg_Node(BB *bb){
    return (REGIONAL_CFG_NODE *)BB_MAP_Get(bb_node_map,bb);
}


//=============================================================================
//
//  Regional_Cfg_Node
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
REGIONAL_CFG_NODE *Regional_Cfg_Node(REGIONAL_CFG_NODE *node, REGION *rgn){
    REGION *home_rgn = node->Home_Region();
    while(home_rgn && home_rgn != rgn){
        node = home_rgn->Regional_Cfg_Node();
        home_rgn = home_rgn->Parent();
    }
    if(home_rgn)
        return node;
    else
        return NULL;
}


//=============================================================================
//
//  Home_Region
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
REGION *Home_Region(BB *bb){
    if(!bb_node_map) return NULL;
    REGIONAL_CFG_NODE *node = (REGIONAL_CFG_NODE *)BB_MAP_Get(bb_node_map,bb);
    if(node)
        return node->Home_Region();
    else 
        return NULL;
}


//=============================================================================
//
//  Region_Contains_BB
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
BOOL Region_Contains_BB(REGION *rgn, BB *bb){
    REGION *home_rgn = Home_Region(bb);
    if(home_rgn == rgn) return TRUE;
    if(home_rgn->Is_Contained_By(rgn)) return TRUE;
    return FALSE;
}


//=============================================================================
//
//  RGN_Gen_And_Insert_Node
//
//  see interface description in region_bb_util.h
//
//=============================================================================
void  RGN_Gen_And_Insert_Node(BB *new_bb, BB *pred_bb, BB *succ_bb,                          
                              REGIONAL_CFG *regional_cfg){
    if(regional_cfg == NULL){
        REGION *pred_rgn = Home_Region(pred_bb);
        REGION *succ_rgn = Home_Region(succ_bb);

        // if pred_bb and succ_bb is in the same region, add the new_bb in this region
        // else add the new_bb in their common parent region
        if(pred_rgn == succ_rgn){
            regional_cfg = pred_rgn->Regional_Cfg();
            regional_cfg->Add_Node(new_bb);
        }else{
            REGION *par_rgn = pred_rgn->Find_Common_Parent(succ_rgn);
            par_rgn->Regional_Cfg()->Add_Node(new_bb);
        }
    }else
        regional_cfg->Add_Node(new_bb);
}


//=============================================================================
//
//  RGN_Gen_And_Insert_BB
//
//  see interface description in region_bb_util.h
//
//=============================================================================
BB *RGN_Gen_And_Insert_BB(BB *pred_bb, BB *succ_bb,
                          REGIONAL_CFG *regional_cfg,
                          BOOL force_not_fall_through,
                          float prob){

    // assert pred_bb and succ_bb are not NULL 
    Is_True(pred_bb != NULL && succ_bb != NULL,
            ("RGN_Gen_And_Insert_BB : pred_bb or succ_bb is NULL"));

    // 1. gen and insert new_bb in global cfg
    BB *new_bb;
    if(force_not_fall_through == FALSE && succ_bb == BB_next(pred_bb)){
        // succ bb is fall through of pred_bb
        new_bb = Gen_And_Insert_BB_After(pred_bb);
    }else{
        BB *last_bb = succ_bb;
        while(BB_next(last_bb)) last_bb = BB_next(last_bb);
        new_bb = Gen_And_Insert_BB_After(last_bb);
    }

    // 2. insert new_bb in regional cfg
    RGN_Gen_And_Insert_Node(new_bb, pred_bb, succ_bb, regional_cfg);
    
    // 3. update the edges in global cfg and regional cfg
    if(prob == 0.0){
        BBLIST *edge = BBlist_Find_BB(BB_succs(pred_bb), succ_bb);
        prob = BBLIST_prob(edge);
    }
    if(force_not_fall_through == FALSE)
        RGN_Unlink_Pred_Succ(pred_bb, succ_bb);

    RGN_Link_Pred_Succ_With_Prob(pred_bb, new_bb, prob);
    RGN_Link_Pred_Succ_With_Prob(new_bb, succ_bb, 1.0);
    
    return new_bb;
}
 

//=============================================================================
//
//  RGN_Gen_And_Insert_BB_After
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
BB *RGN_Gen_And_Insert_BB_After(BB *point, REGIONAL_CFG *regional_cfg){
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** beginning of RGN_Gen_And_Insert_BB_After(bb_id:%d *** \n",
                BB_id(point));
    
    BB *new_bb = Gen_And_Insert_BB_After(point);
    
    if(regional_cfg == NULL)
        regional_cfg = Home_Region(point)->Regional_Cfg();

    regional_cfg->Add_Node(new_bb);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** end of RGN_Gen_And_Insert_BB_After ***\n");
    
    return new_bb;
}   


//=============================================================================
//
//  RGN_Gen_And_Insert_BB_Before
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
BB *RGN_Gen_And_Insert_BB_Before(BB *point, REGIONAL_CFG *regional_cfg){
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** beginning of RGN_Gen_And_Insert_BB_Before(bb_id:%d *** \n",
                BB_id(point));
    
    BB *new_bb = Gen_And_Insert_BB_Before(point);
    
    if(regional_cfg == NULL)
        regional_cfg = Home_Region(point)->Regional_Cfg();
    
    regional_cfg->Add_Node(new_bb);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** end of RGN_Gen_And_Insert_BB_Before ***\n");
    return new_bb;
}

void RGN_Unlink_BB_Edges(BB *bb, REGIONAL_CFG *regional_cfg) {
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** beginning of RGN_Unlink_BB_Edges(bb_id:%d *** \n",
                BB_id(bb));
    
    REGION *bb_rgn = Home_Region(bb);
    if(regional_cfg == NULL)
        regional_cfg = bb_rgn->Regional_Cfg();
    else
        Is_True(bb_rgn->Regional_Cfg()==regional_cfg, ("RGN_Remove_BB_And_Edges"));
  
    
    BBLIST *pred_list=NULL, *succ_list=NULL;

    // delete all incoming edges
    while(pred_list = BB_preds(bb)){
        BB *pred = BBLIST_item(pred_list);
        RGN_Unlink_Pred_Succ(pred, bb);
    }       
            
    // delete all outcoming edges
    while (succ_list = BB_succs(bb)){
        BB *succ = BBLIST_item(succ_list);
        RGN_Unlink_Pred_Succ(bb, succ);
    };

    // OSP: delete the node from the region,
    // otherwise, we'll get an assertion in verify region_cfg.
    REGIONAL_CFG_NODE *node = Regional_Cfg_Node(bb);
    REGION *root_region = node->Home_Region()->Tree()->Root();

    // delete corresponding regional_cfg_node,
    // if its home region's node set is empty, delete it also
    if( regional_cfg->_node_set.size() == 1 ){
        REGION *region = node->Home_Region();
        REGION *par_rgn = region->Parent();
        while( par_rgn->Regional_Cfg()->_node_set.size() == 1 ) {
            region = par_rgn;
            par_rgn = region->Parent();
        };
        region->Tree()->Del_Region(region);
    }
    else {
        regional_cfg->Del_Node(node);
    }

    // Add the deleted node to root region to avoid its home region is NULL
    root_region->Regional_Cfg()->Add_Node(bb);

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** end of RGN_Unlink_BB_Edges *** \n");

} 

//=============================================================================
//
//  RGN_Remove_BB_And_Edges
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
void RGN_Remove_BB_And_Edges(BB *bb, REGIONAL_CFG *regional_cfg){
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** beginning of RGN_Remove_BB_And_Edges(bb_id:%d *** \n",
                BB_id(bb));
    
    REGION *bb_rgn = Home_Region(bb);
    if(regional_cfg == NULL)
        regional_cfg = bb_rgn->Regional_Cfg();
    else
        Is_True(bb_rgn->Regional_Cfg()==regional_cfg, ("RGN_Remove_BB_And_Edges"));
  
    
    BBLIST *pred_list=NULL, *succ_list=NULL;

    // delete all incoming edges
    while(pred_list = BB_preds(bb)){
        BB *pred = BBLIST_item(pred_list);
        RGN_Unlink_Pred_Succ(pred, bb);
    }       
            
    // delete all outcoming edges
    while (succ_list = BB_succs(bb)){
        BB *succ = BBLIST_item(succ_list);
        RGN_Unlink_Pred_Succ(bb, succ);
    };

    // delete corresponding regional_cfg_node, 
    // if its home region's node set is empty, delete it also
    REGIONAL_CFG_NODE *node = Regional_Cfg_Node(bb);
    if(regional_cfg->_node_set.size() == 1){
        REGION *region = node->Home_Region();
        REGION *par_rgn = region->Parent();
        while(par_rgn->Regional_Cfg()->_node_set.size() == 1){
            region = par_rgn;
            par_rgn = region->Parent();
        };
        region->Tree()->Del_Region(region);
    }else
        regional_cfg->Del_Node(node);

    Remove_BB(bb);
        
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** end of RGN_Remove_BB_And_Edges *** \n");

}


//=============================================================================
//
//  Add_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, REGIONAL_CFG_NODE *succ, REGION *rgn)
//
//  try to add edge between pred and succ, 
//  if it is back edge
//      set pred as Loop_Tail
//  else
//      add it in rgn->Regional_Cfg
//
//=============================================================================
void Add_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, REGIONAL_CFG_NODE *succ, REGION *rgn){
    REGIONAL_CFG *cfg = rgn->Regional_Cfg();
    if(rgn->Region_Type() == LOOP){
        LOOP_REGION *loop = (LOOP_REGION *)rgn;
        if(succ != loop->Loop_Head()) // if this edge is not back edge, add it
            cfg->Add_Edge(pred,succ);
        else // if this edge is back edge, change pred_node as Loop_Tail
            loop->Loop_Tail(pred);
    }else
        cfg->Add_Edge(pred, succ);
}

  
//=============================================================================
//
//  RGN_Add_Regional_Cfg_Node
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
void RGN_Add_Regional_Cfg_Edge(BB *pred, 
                               BB *succ, 
                               REGIONAL_CFG *regional_cfg){

    REGIONAL_CFG_NODE *pred_node = Regional_Cfg_Node(pred);
    REGIONAL_CFG_NODE *succ_node = Regional_Cfg_Node(succ);
    
    REGION *pred_rgn = pred_node->Home_Region();
    REGION *succ_rgn = succ_node->Home_Region();
    REGION *root1 = pred_rgn->Tree()->Root(),*par_rgn;
    REGIONAL_CFG *cfg;
    if(regional_cfg == NULL)
        regional_cfg = pred_rgn->Find_Common_Parent(succ_rgn)->Regional_Cfg();
    else
        Is_True(pred_rgn->Find_Common_Parent(succ_rgn)->Regional_Cfg()==regional_cfg, 
                ("RGN_Link_Pred_Succ_With_Prob"));
    cfg=regional_cfg;
    if(pred_rgn == succ_rgn) {// pred and succ are in the same region
        // update the exit set of this region
        Is_True(pred_node,("BB: %d's regional_cfg_node doesn't exists.\n",pred));
        if(pred_node->Is_Exit())
            if(BB_succs_len(pred)==0 || BB_Unique_Successor(pred)==succ){ 
                REGIONAL_CFG *pred_cfg = pred_rgn->Regional_Cfg();
                pred_cfg->Remove_From_Exits(pred_node);
                
               while (pred_rgn != root1){ 

                    pred_node=pred_rgn->Regional_Cfg_Node();
                    Is_True(pred_node,("region %d's regional_cfg_node doesn't exists.\n",pred_rgn->Id()));
                    par_rgn =pred_node->Home_Region();
                    Is_True(par_rgn,("pred_node: %d's Home_Region doesn't exists.\n",pred_node->Id()));   
                        REGIONAL_CFG *cfg = par_rgn->Regional_Cfg();
                 	
                 	BB_VECTOR exits(&(cfg->_m));
                    if(pred_node->Is_Region())
                       Collect_Exit_BBs(pred_node->Region_Node(), &exits);
                    else
                       exits.push_back(pred_node->BB_Node());
    
                    BOOL has_other_exit_edge = FALSE;
                    for(BB_VECTOR_ITER bb_iter=exits.begin(); bb_iter!=exits.end(); bb_iter++){
                        BB *exit_bb = *bb_iter;
                        BBLIST *succ_list;
                        if(BB_succs_len(exit_bb) == 0 && exit_bb != pred) {
                        	has_other_exit_edge= TRUE;
                        	break;
                        }	
                        FOR_ALL_BB_SUCCS(exit_bb, succ_list) { 
                            BB *succ_bb = BBLIST_item(succ_list);
                            if(succ_bb == succ && exit_bb == pred) continue; // skip this edge
                            if(! Region_Contains_BB(par_rgn, succ_bb)){
                                has_other_exit_edge = TRUE;
                                break;
                            }
                       }
                       if(has_other_exit_edge)
                          break;
                   }
                   if(!has_other_exit_edge){
                      cfg->Remove_From_Exits(pred_node);
                   }
                   pred_rgn=par_rgn;
               }
        }
        REGIONAL_CFG_NODE *pred_node = Regional_Cfg_Node(pred);
        REGION *pred_rgn = pred_node->Home_Region();
        if (!succ_node->Is_Entry()) { 
           Add_Regional_Cfg_Edge(pred_node, succ_node, pred_rgn);
        }
    }       
    else{
        REGION *com_par = pred_rgn->Find_Common_Parent(succ_rgn);
        
        if(pred_node->Is_Exit())
            if(BB_succs_len(pred)==0 || BB_Unique_Successor(pred)==succ){ 
                REGIONAL_CFG *pred_cfg = pred_rgn->Regional_Cfg();
                pred_cfg->Remove_From_Exits(pred_node);
                while (pred_rgn != root1){ 
                    pred_node=pred_rgn->Regional_Cfg_Node();
                    par_rgn =pred_node->Home_Region();
                    Is_True(par_rgn,("The pred_node %d's Home_Region doesn't exists.\n",pred_node->Id()));
                        REGIONAL_CFG *cfg = par_rgn->Regional_Cfg();
                 	
                 	BB_VECTOR exits(&(cfg->_m));
                    if(pred_node->Is_Region())
                       Collect_Exit_BBs(pred_node->Region_Node(), &exits);
                    else
                       exits.push_back(pred_node->BB_Node());
    
                    BOOL has_other_exit_edge = FALSE;
                    for(BB_VECTOR_ITER bb_iter=exits.begin(); bb_iter!=exits.end(); bb_iter++){
                        BB *exit_bb = *bb_iter;
                        BBLIST *succ_list;
                        if(BB_succs_len(exit_bb) == 0 && exit_bb != pred) {
                        	has_other_exit_edge= TRUE;
                        	break;
                        }	
                        FOR_ALL_BB_SUCCS(exit_bb, succ_list) { 
                            BB *succ_bb = BBLIST_item(succ_list);
                            if(succ_bb == succ && exit_bb == pred) continue; // skip this edge
                            if(! Region_Contains_BB(par_rgn, succ_bb)){
                                has_other_exit_edge = TRUE;
                                break;
                            }
                       }
                       if(has_other_exit_edge)
                          break;
                   }
                   if(!has_other_exit_edge){
                      cfg->Remove_From_Exits(pred_node);
                   }
                   pred_rgn=par_rgn; 
               }
     	}

        REGIONAL_CFG_NODE *pred_node = Regional_Cfg_Node(pred);
        REGIONAL_CFG_NODE *succ_node = Regional_Cfg_Node(succ);
        REGION *pred_rgn = pred_node->Home_Region();
        // add regional cfg edge in common_parent of pred_rgn and succ_rgn 
        while(pred_rgn != com_par){
            // update the exit set of pred_rgn
            pred_rgn->Regional_Cfg()->Add_To_Exits(pred_node);
            pred_node = pred_rgn->Regional_Cfg_Node();
            pred_rgn = pred_node->Home_Region();
        };
 
        while(succ_rgn != com_par){
            // update the entry set of succ_rgn
	    succ_rgn->Regional_Cfg()->Add_To_Entries(succ_node);
	    if(succ_rgn->Regional_Cfg()->Num_Of_Entries() > 1 && succ_rgn->Region_Type() == SEME)
	      succ_rgn->Region_Type(MEME);
            succ_node = succ_rgn->Regional_Cfg_Node();
            succ_rgn = succ_node->Home_Region();
        };
        if (!succ_node->Is_Entry()) {   
            Add_Regional_Cfg_Edge(pred_node, succ_node, com_par);
        }
    }
}


//=============================================================================
//
//  RGN_Link_Pred_Succ_With_Prob
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
void RGN_Link_Pred_Succ_With_Prob(BB *pred, BB *succ, float prob,
                  REGIONAL_CFG *regional_cfg){
    if (Get_Trace(TP_A_REGION, TT_RGN_UTIL_DEBUG))
        fprintf(TFile, "*** beginning of RGN_link_Pred_Succ_With_Prob(pred_bb_id:%d, succ_bb_id:%d) *** \n",
                BB_id(pred), BB_id(succ));

    // 1. add global edge between pred and succ
    Link_Pred_Succ(pred,succ);
    Set_Prob(pred,succ,prob);

    // 2. add regional cfg edge and update entries and exits
    RGN_Add_Regional_Cfg_Edge(pred, succ, regional_cfg);
     
    if (Get_Trace(TP_A_REGION, TT_RGN_UTIL_DEBUG)){
        fprintf(TFile, "*** end of RGN_link_Pred_Succ_With_Prob *** \n");

    }
}


//=============================================================================
//
//  Collect_Entry_BBs
//
//  collect all bbs which may be the entry of rgn 
//
//=============================================================================
void Collect_Entry_BBs(REGION *rgn, BB_VECTOR *entries){
    NODE_VECTOR nodes(&(rgn->Regional_Cfg()->_m));
    nodes = rgn->Entries();
    for(NODE_VECTOR_ITER iter = nodes.begin(); iter!=nodes.end(); iter++){
        if((*iter)->Is_Region())
            Collect_Entry_BBs((*iter)->Region_Node(), entries);
        else
            entries->push_back((*iter)->BB_Node());
    }
}


//=============================================================================
//
//  Collect_Exit_BBs
//
//  collect all bbs which may be the exit of rgn 
//
//=============================================================================
void Collect_Exit_BBs(REGION *rgn, BB_VECTOR *exits){
    NODE_VECTOR nodes(&(rgn->Regional_Cfg()->_m));
    nodes = rgn->Exits();
    for(NODE_VECTOR_ITER iter = nodes.begin(); iter!=nodes.end(); iter++){
        if((*iter)->Is_Region())
            Collect_Exit_BBs((*iter)->Region_Node(), exits);
        else
            exits->push_back((*iter)->BB_Node());
    }
}


//=============================================================================
//
// INT Edge_Counter(REGIONAL_CFG_NODE *src, REGIONAL_CFG_NODE *dest, REGIONAL_CFG *cfg)
// 
// calculate the number of edges between src and dest in cfg
//
//=============================================================================
INT Edge_Counter(REGIONAL_CFG_NODE *src, REGIONAL_CFG_NODE *dest, REGIONAL_CFG *cfg){
    // Collect all exit bbs , record in exits
    BB_VECTOR exits(&(cfg->_m));
    if(src->Is_Region())
        Collect_Exit_BBs(src->Region_Node(), &exits);
    else
        exits.push_back(src->BB_Node());
            
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)){      
        fprintf(TFile,"exits are: ");
        for(BB_VECTOR_ITER iter=exits.begin(); iter!=exits.end(); iter++)
            fprintf(TFile,"bb_id:%d ", BB_id(*iter));
        fprintf(TFile,"\n");
    }

    REGION *dest_rgn;
    BB *dest_bb;
    if(dest->Is_Region())
        dest_rgn = dest->Region_Node();
    else
        dest_bb = dest->BB_Node();

    INT32 num = 0;
    for(BB_VECTOR_ITER iter=exits.begin(); iter!=exits.end(); iter++){
        BBLIST *succ_list;
        FOR_ALL_BB_SUCCS(*iter, succ_list) { 
            BB *succ_bb = BBLIST_item(succ_list);  
            if(dest->Is_Region()){
                REGION *succ_rgn = Home_Region(succ_bb);
                if(succ_rgn==dest_rgn || succ_rgn->Is_Contained_By(dest_rgn))
                    num++;
            }else
                if(succ_bb == dest_bb)
                    num++;
        }
    }
    
    return num++;
}


//=============================================================================
//
//  Del_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, REGIONAL_CFG_NODE *succ, REGION *rgn)
//
//  try to del edge between pred and succ, 
//  if it is not back edge and edge_counter>1
//      del it in rgn->Regional_Cfg
//
//=============================================================================
void Del_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, REGIONAL_CFG_NODE *succ, REGION *rgn){
    Is_True(pred->Home_Region()==succ->Home_Region() && pred->Home_Region()==rgn,
        ("pred_node_id:%d and succ_node_id:%d should in rgn_id:%d",
        pred->Id(), succ->Id(), rgn->Id()));

    REGIONAL_CFG *cfg = rgn->Regional_Cfg();
    if(rgn->Region_Type()==LOOP){
        LOOP_REGION *loop = (LOOP_REGION *)rgn;
        if(succ == loop->Loop_Head())
            return;
    }
    REGIONAL_CFG_EDGE *edge = cfg->Find_Edge(pred, succ);
    
    if(edge == NULL){
        DevWarn("can not find the edge between regional_cfg_node %d and %d.\n",pred->Id(),succ->Id());
        return;
    }

    INT32 counter = Edge_Counter(pred,succ,cfg);
    if(counter > 1)
        return;
    else
        cfg->Del_Edge(edge);
}



//=============================================================================
//
//  RGN_Del_Regional_Cfg_Edge
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
void RGN_Del_Regional_Cfg_Edge(BB *pred,
                               BB *succ,
                               REGIONAL_CFG *regional_cfg){
    REGIONAL_CFG_NODE *pred_node = Regional_Cfg_Node(pred);
    Is_True(pred_node, ("pred_bb_id:%d has no corresponding regional cfg node",
                        BB_id(pred)));
    REGIONAL_CFG_NODE *succ_node = Regional_Cfg_Node(succ);
    Is_True(pred_node, ("succ_bb_id:%d has no corresponding regional cfg node",
                        BB_id(succ)));        

    REGION *pred_rgn = pred_node->Home_Region();
    REGION *succ_rgn = succ_node->Home_Region();
    REGION *root1 = pred_rgn->Tree()->Root();
    INT32 succ_num=1;
    if(regional_cfg == NULL)
        regional_cfg = pred_rgn->Find_Common_Parent(succ_rgn)->Regional_Cfg();
    else
        Is_True(pred_rgn->Find_Common_Parent(succ_rgn)->Regional_Cfg()==regional_cfg, 
                ("RGN_Link_Pred_Succ_With_Prob"));
    REGIONAL_CFG *cfg = regional_cfg;
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)){
        if(!pred_node->Is_Region() && !succ_node->Is_Region())
            fprintf(TFile, "pred_bb id: %d  succ_bb id: %d  ", 
                            BB_id(pred_node->BB_Node()), BB_id(succ_node->BB_Node()));
        fprintf(TFile, "pred_rgn id: %d  succ_rgn id: %d \n", pred_rgn->Id(), succ_rgn->Id());
    }

    if(pred_rgn == succ_rgn){ // pred and succ are in the same region
        Del_Regional_Cfg_Edge(pred_node, succ_node, pred_rgn);
        if(BB_succs_len(pred)==1){              
            while (pred_rgn != root1){                          
                cfg = pred_rgn->Regional_Cfg();                  
            	cfg->Add_To_Exits(pred_node);                     
            	pred_node = pred_rgn->Regional_Cfg_Node();       
                pred_rgn = pred_node->Home_Region();            
            }                                                  
            cfg = pred_rgn->Regional_Cfg();                      
           	cfg->Add_To_Exits(pred_node);                        
        }   	
    }else{
        REGION *com_par = pred_rgn->Find_Common_Parent(succ_rgn);
        
        // because in Del_Regional_Cfg_Edge, need Edge_Counter
        // in Edge_Counter , need all exit bbs --> collect all exit bbs, 
        // --> if one region is exit regional_cfg_node
        // Del_Regional_Cfg_Edge is before update exit set 
                 
        // del regional cfg edge in common_parent of pred_rgn and succ_rgn
        Del_Regional_Cfg_Edge(Regional_Cfg_Node(pred_node,com_par), 
                              Regional_Cfg_Node(succ_node,com_par), 
                              com_par);
        
        while(pred_rgn != com_par){
            // update the exit set of pred_rgn
            REGIONAL_CFG *pred_cfg = pred_rgn->Regional_Cfg();
            
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED))
                pred_node->Print(TFile);
            Is_True(pred_node->Is_Exit(),
                ("RGN_Del_Regional_Cfg_Edge(pred_bb_id:%d, succ_bb_id:%d)::"
                  "pred_node Id:%d should be a exit node of region %d", 
                  BB_id(pred), BB_id(succ), pred_node->Id(), pred_rgn->Id()));

            BB_VECTOR exits(&(regional_cfg->_m));
            if(pred_node->Is_Region())
                Collect_Exit_BBs(pred_node->Region_Node(), &exits);
            else
                exits.push_back(pred_node->BB_Node());

            BOOL has_other_exit_edge = FALSE;
            for(BB_VECTOR_ITER bb_iter=exits.begin(); bb_iter!=exits.end(); bb_iter++){
                BB *exit_bb = *bb_iter;
                BBLIST *succ_list;
            if(BB_succs_len(exit_bb) == 0 && exit_bb != pred) {     
                        	has_other_exit_edge= TRUE;                  
                        	break;                                    
                }	                                                      

                FOR_ALL_BB_SUCCS(exit_bb, succ_list) { 
                    BB *succ_bb = BBLIST_item(succ_list);
                    if(succ_bb == succ && exit_bb == pred) continue; 
                    if(! Region_Contains_BB(pred_rgn, succ_bb)){
                        has_other_exit_edge = TRUE;
                        break;
                    }
                }
                if(has_other_exit_edge)
                    break;
            }
                        
            if(!has_other_exit_edge){
                pred_cfg->Remove_From_Exits(pred_node);
            }
            pred_node = pred_rgn->Regional_Cfg_Node();
            pred_rgn = pred_node->Home_Region();
        };

        pred_node = Regional_Cfg_Node(pred);   
        pred_rgn = pred_node->Home_Region();    
        if(BB_succs_len(pred)==1){             
            while (pred_rgn != root1){          
                cfg = pred_rgn->Regional_Cfg();  
            	cfg->Add_To_Exits(pred_node);    
            	pred_node = pred_rgn->Regional_Cfg_Node();
                pred_rgn = pred_node->Home_Region(); 
            }                                       
            cfg = pred_rgn->Regional_Cfg();           
           	cfg->Add_To_Exits(pred_node);             
        }                                          
        while(succ_rgn != com_par){
            // update the entry set of succ_rgn
            REGIONAL_CFG *succ_cfg = succ_rgn->Regional_Cfg();
            if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED))
                succ_node->Print(TFile);
            Is_True(succ_node->Is_Entry(),("succ_node should be a entry node"));
            BBLIST *pred_list;
            BOOL has_other_entry_edge = FALSE;;
            FOR_ALL_BB_PREDS(succ, pred_list) { 
                BB *pred_bb = BBLIST_item(pred_list);
                if(pred_bb == pred) continue;
                if(! Region_Contains_BB(succ_rgn, pred_bb)){
                    has_other_entry_edge = TRUE;
                    break;
                }
            }
            if(!has_other_entry_edge)
                succ_cfg->Remove_From_Entries(succ_node);
            succ_node = succ_rgn->Regional_Cfg_Node();
            succ_rgn = succ_node->Home_Region();
        };
    }
}


//=============================================================================
//
//  RGN_Unlink_Pred_Succ
//
//  See interface description in region_bb_util.h.
//
//=============================================================================
void RGN_Unlink_Pred_Succ(BB *pred, BB *succ, REGIONAL_CFG *regional_cfg){
    if (Get_Trace(TP_A_REGION, TT_RGN_UTIL_DEBUG))
        fprintf(TFile, "*** beginning of RGN_Unlink_Pred_Succ(pred_bb_id:%d, succ_bb_id:%d) *** \n",
                BB_id(pred), BB_id(succ));

    // 1. del regional cfg edge and update entries and exits
    RGN_Del_Regional_Cfg_Edge(pred, succ, regional_cfg);

    // 2. del the global edge
    Unlink_Pred_Succ(pred,succ);
        
    if (Get_Trace(TP_A_REGION, TT_RGN_UTIL_DEBUG)){
        fprintf(TFile, "*** end of RGN_Unlink_Pred_Succ *** \n");
        
    }
}


//=============================================================================
//
//  Region_Def_Reach_In
//
//  Calcute the def_reach_in of rgn
//
//============================================================================= 
GTN_SET *Region_Def_Reach_In(REGION *rgn, MEM_POOL *pool){
    BB_VECTOR entries(pool);
    Collect_Entry_BBs(rgn, &entries);
    GTN_SET *def_reach_in = NULL;
    for(BB_VECTOR_ITER iter=entries.begin(); iter!=entries.end(); iter++){
        if(def_reach_in == NULL)
            def_reach_in = BB_defreach_in( *iter );
        else
            def_reach_in = GTN_SET_Union(def_reach_in, BB_defreach_in(*iter), pool);
    }
    return def_reach_in;
}
//=============================================================================
//
//  Divide_BB: Divide one basic block into two, the boundary is indicated by point.
//
//  If force is TRUE, this routine will always create a new BB
//      even if the point is the last op in the bb. -- added by jianxin.lai@hp
//=============================================================================
BB *RGN_Divide_BB(BB *bb, OP *point, BOOL force)
{
    Is_True( OP_bb(point) == bb, ("Divide_BB: op is not in bb!"));
    
    // Alwayse create a new BB if force is ture 
    //   even if the point is the last op
    if( point == BB_last_op(bb) && ! force )   return NULL;

    BB* bottom_bb = RGN_Gen_And_Insert_BB_After(bb);
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

    BBLIST* nxt;
    BBLIST* succ;
    for(succ = BB_succs(bb); succ; succ = nxt) {
        BB* bb_succ = BBLIST_item(succ);
        nxt = BBLIST_next(succ);
        RGN_Link_Pred_Succ_With_Prob(bottom_bb, bb_succ, BBLIST_prob(succ));
        RGN_Unlink_Pred_Succ(bb, bb_succ);
    }

    for(OP *op = OP_next(point); op; ) {
        OP *tmp = op;
        op = OP_next(op);
        BB_Move_Op_To_End(bottom_bb, bb, tmp);
    }
    
    RGN_Link_Pred_Succ_With_Prob(bb, bottom_bb, 1.0);
    return bottom_bb;
}

