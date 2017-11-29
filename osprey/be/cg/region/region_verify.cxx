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
//=============================================================================
//
// Module: region_verify.cxx
// Author: lren
// $Revision: 1.1 $
// $Date: 2005/12/30 01:47:13 $
// $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/region_verify.cxx,v $
//
//=============================================================================


#include "bb.h"
#include "stack"
#include "vector"
#include "list"
#include "defs.h"
#include "cxx_memory.h"
#include "region.h"
#include "region_bb_util.h"
#include "region_verify.h"
#include "interval_processor.h"
#include "vt_region.h"
#include "ipfec_defs.h"
#include "tracing.h"


//=============================================================================
//
//  Verify_Node
//
//  verify the attributions of regional cfg node
//
//=============================================================================
void Verify_Node(REGIONAL_CFG_NODE *node, REGION *rgn){
    // verify Home_Region is rgn
    REGION *home_rgn = node->Home_Region();
    if(node->Is_Region())
        Is_True(home_rgn==rgn, ("regional cfg node (rgn_id:%d) should be in region_id:%d", 
                                node->Region_Node()->Id(), rgn->Id()));
    else
        Is_True(home_rgn==rgn, ("regional cfg node (bb_id:%d) should be in region_id:%d", 
                                BB_id(node->BB_Node()), rgn->Id()));

    // verify region_node or bb_node
    if(node->Is_Region()){
        REGION *rgn = node->Region_Node();
        Is_True(rgn->Parent()==home_rgn, 
                ("home region:%d of regional cfg node:%d should be its parent region%d",
                  home_rgn->Id(), node->Id(), rgn->Parent()->Id()));
    }else{
        BB *bb = node->BB_Node();
        Is_True(Regional_Cfg_Node(bb)==node,
                ("bb:%d should get its corresponding regional cfg node", BB_id(bb)));
    }

    // verify the edges which src is this node
    for(CFG_SUCC_EDGE_ITER iter(node); iter!=0; ++iter){
        REGIONAL_CFG_NODE *dest = (*iter)->Dest();
        Is_True(dest->Find_Pred_Edge(node), ("node is the pred of its succ")); 
    }

    // verify the _succ_num and _pred_num
    INT32 succ_num = 0;
    for(CFG_SUCC_EDGE_ITER iter(node); iter!=0; ++iter)
        succ_num++;
    Is_True(node->Succ_Num()==succ_num, ("_succ_num=%d, succ_num=%d", node->Succ_Num(),succ_num));
    INT32 pred_num = 0;
    for(CFG_PRED_EDGE_ITER iter(node); iter!=0; ++iter)
        pred_num++;
    Is_True(node->Pred_Num()==pred_num, ("_pred_num=%d, pred_num=%d", node->Pred_Num(),pred_num));
    
    // verify _is_entry and _is_exit
    BB_ALLOC alloc(&(home_rgn->Regional_Cfg()->_m));
    if(node->Is_Entry()){
        BOOL find_entry_edge = FALSE;
        BB_VECTOR bbs(alloc); 
        if(node->Is_Region())
            Collect_Entry_BBs(node->Region_Node(), &bbs);
        else
            bbs.push_back(node->BB_Node());
        Is_True(!bbs.empty(), ("#entry_bbs is %d", bbs.size())); 
        for(BB_VECTOR_ITER iter=bbs.begin(); iter!=bbs.end(); iter++){
            if(BB_preds_len(*iter)==0) // this bb is entry of PU
                find_entry_edge = TRUE;
            BBLIST *pred_list;
            FOR_ALL_BB_PREDS(*iter, pred_list) { 
                BB *pred = BBLIST_item(pred_list);
                if(!Region_Contains_BB(home_rgn, pred)){
                    find_entry_edge = TRUE;
                    break;
                }
            }
            if(find_entry_edge) break;
        }
        bbs.clear();

        Is_True(find_entry_edge, 
            ("entry node (id:%d) should has one incoming edge which src is not in region (id:%d)", 
                node->Id(),home_rgn->Id()));
    }

    if(node->Is_Exit()){
        INT32 find_exit_edge = 0;
        BB_VECTOR bbs(alloc);
        if(node->Is_Region())
            Collect_Exit_BBs(node->Region_Node(), &bbs);
        else
            bbs.push_back(node->BB_Node());
        Is_True(!bbs.empty(), ("#exit_bbs is %d", bbs.size())); 
        for(BB_VECTOR_ITER iter=bbs.begin(); iter!=bbs.end(); iter++){
            if(BB_succs_len(*iter)==0) // this bb is exit of PU
                find_exit_edge = TRUE;
            BBLIST *succ_list;
            FOR_ALL_BB_SUCCS(*iter, succ_list) { 
                BB *succ = BBLIST_item(succ_list);
                if(!Region_Contains_BB(home_rgn, succ)){
                        find_exit_edge = TRUE;
                        break;
                }
                if(find_exit_edge) break;
            }
        }
        bbs.clear();
                
#ifdef Is_True_On
        if(node->Is_Region())
            Is_True(find_exit_edge, 
                ("verify exit node (rgn_id:%d) should has one outcoming edge which dest is not in region (id:%d)",
                    node->Region_Node()->Id(), home_rgn->Id()));
        else
            Is_True(find_exit_edge, 
                ("verify exit node (bb_id:%d) should has one outcoming edge which dest is not in region (id:%d)",
                    BB_id(node->BB_Node()), home_rgn->Id()));
#endif
    }
}


//=============================================================================
//
//  Verify_Cfg
//
//  verify the attibutions and iterators of regional cfg
//
//=============================================================================
void Verify_Cfg(REGIONAL_CFG *cfg){
    // verify entries and exits, if connected and regional_cfg_node
    INT32 entry_num = 0;
    INT32 exit_num = 0;
    for(SEQ_REGIONAL_CFG_ITER iter(cfg); iter!=0; ++iter){
        REGIONAL_CFG_NODE *node = *iter;
        
        //verify regional cfg node;
        Verify_Node(node, cfg->_r);
        
        if(node->Is_Entry())
            entry_num++;
        else{
            // verify this region is connected
            // if one node is not entry and it has no pred, this region is not connected
            if(cfg->_r->Region_Type()==SEME)
                Is_True(node->First_Pred(), 
                    ("node_id:%d of region_id:%d has no pred ", node->Id(), cfg->_r->Id())); 
        }
        if(node->Is_Exit())    
            exit_num++;
                        
        //verify home_region
        Is_True(node->Home_Region()==cfg->_r, ("verify Home_Region"));

        // verify each regional_cfg_edge
        for(CFG_SUCC_NODE_ITER succ_iter(node); succ_iter!=0; ++succ_iter)
            Is_True(Edge_Counter(node, *succ_iter, cfg)>=1,("verify regional cfg edge"));
    }

    Is_True(entry_num == cfg->Num_Of_Entries(),
           ("verify entries of regional_cfg: region_id: %d, entry_num=%d, cfg->_entries.size()=%d",
            cfg->_r->Id(), entry_num, cfg->Num_Of_Entries()));
    Is_True(exit_num == cfg->Num_Of_Exits(), 
           ("verify exits of regional_cfg: region_id: %d, exit_num=%d, cfg->_exits.size()=%d",
            cfg->_r->Id(),exit_num, cfg->Num_Of_Exits()));
        
    if(cfg->_r->Region_Type()==IMPROPER)  return;

    // verify topological_regional_cfg_iter 
    NODE_ALLOC alloc(&(cfg->_m));
    NODE_VECTOR nodes(alloc);
    for(TOPOLOGICAL_REGIONAL_CFG_ITER iter(cfg); iter!=0; ++iter){
        REGIONAL_CFG_NODE *node = *iter;
        for(CFG_PRED_NODE_ITER pred_iter(node); pred_iter!=0; ++pred_iter)
            Is_True((Find_In_Vector(*pred_iter, nodes)!=(NODE_VECTOR_ITER)0),("verify topological_cfg_iter"));
        nodes.push_back(node);
    }

    nodes.clear();
    
    // verify reverse_topo_regional_cfg_iter
    for(REVERSE_TOPO_REGIONAL_CFG_ITER iter(cfg); iter!=0; ++iter){
        REGIONAL_CFG_NODE *node = *iter;
        for(CFG_SUCC_NODE_ITER succ_iter(node); succ_iter!=0; ++succ_iter)
            Is_True((Find_In_Vector(*succ_iter, nodes)!=(NODE_VECTOR_ITER)0),("verify topological_cfg_iter"));
        nodes.push_back(node);
    }
}


//=============================================================================
//
//  Verify_SEME_Region
//
//  verify the attributes of SEME region
//
//=============================================================================
void Verify_SEME_Region(REGION *region){
    Is_True(region->Region_Type()==SEME,
            ("verify SEME region's Region_Type should be SEME_REGION"));
    // seme region should only have one entry
    REGIONAL_CFG *cfg = region->Regional_Cfg();
    Is_True(cfg->Num_Of_Entries() == 1, 
        ("SEME REGION id:%d has more than one entries", region->Id()));

    // seme region should only have no cycles
    INTERVAL_PROCESSOR inter(region);
    inter.Find_Cycles();
    Is_True(inter._cycles.empty(), ("seme should have no cycles"));
  
}

    
//=============================================================================
//  
//  Find_Regional_Cfg_Edge
//
//  if it is back edge of loop region return TRUE
//  else return cfg->Find_Edge
//
//=============================================================================
static BOOL Find_Regional_Cfg_Edge(REGIONAL_CFG_NODE *src, 
                                   REGIONAL_CFG_NODE *dest, 
                                   REGION *rgn){
    if(rgn->Region_Type() == LOOP){
        LOOP_REGION *loop = (LOOP_REGION *)rgn;
        // if src is loop_tail and dest is loop_head
        if(src==loop->Loop_Tail() && dest==loop->Loop_Head())
            return TRUE;
    }
    if(rgn->Regional_Cfg()->Find_Edge(src,dest))
        return TRUE;
    else
        return FALSE;
}

//=============================================================================
//
//  Verify_Entry_Exit_BB(BB *bb)
//
//  Verify if one bb has a pred which come from out of its home_region or it has
//         no pred bb, it should be a entry node
//         if one bb has a succ which go out of its home_region or it has no succ 
//         bb, it should be a exit node
//
//=============================================================================
void Verify_Entry_Exit_BB(BB *bb){
    REGION *rgn = Home_Region(bb);
    REGIONAL_CFG_NODE *node = Regional_Cfg_Node(bb);
    Is_True(rgn && node, ("verify_entry_exit_bb"));
    
    if(BB_preds_len(bb)==0)
        Is_True(node->Is_Entry(), ("verify_entry_exit_bb"));
    if(BB_succs_len(bb)==0)
        Is_True(node->Is_Exit(), ("verify_entry_exit_bb"));

    BBLIST *pred_list;
    FOR_ALL_BB_PREDS(bb, pred_list){
        BB *pred = BBLIST_item(pred_list);
        Is_True(pred && Home_Region(pred), ("verify_entry_exit_bb")); 
        if(!(Region_Contains_BB(rgn, pred))){
            Is_True(node->Is_Entry(), ("verify_entry_exit_bb"));
            break;
        }
    }

    BBLIST *succ_list;
    FOR_ALL_BB_SUCCS(bb, succ_list){
        BB *succ = BBLIST_item(succ_list);
        Is_True(succ && Home_Region(succ), ("verify_entry_exit_bb"));
        if(!(Region_Contains_BB(rgn, succ))){
		    //---------------------------------------------------------------- 
		    // Here check whether the succ and bb is connected to a backedge,
			// if so,do not assert,for a loop tail may be not a exit,but in
			// region backedge is not visible therefore we do not think it a 
			// exit node.
			//----------------------------------------------------------------
			BOOL is_cycle = TRUE;
			for (GLOBAL_CYCLE_VECTOR_ITER cycle_iter = global_cycles.begin();
                cycle_iter != global_cycles.end();cycle_iter++) {
                if ((succ == (*cycle_iter).dest)&&(bb == (*cycle_iter).src)) {
                    is_cycle  = TRUE;       

					break;
                }
            }
            
			if (!is_cycle) {
	            Is_True(node->Is_Exit(), ("verify_entry_exit_bb"));
			}
        }
    }
}

//=============================================================================
//
//  Verify_Global_Cfg
//
//  Verify the corresponding regional cfg edge of global cfg edge(bb,succ)
//
//=============================================================================
void Verify_Global_Edge(BB *bb, BB *succ){
    REGION *rgn = Home_Region(bb);
    REGIONAL_CFG_NODE *node=Regional_Cfg_Node(bb);

    BBLIST *pred_list;
    BOOL find = FALSE;
    FOR_ALL_BB_PREDS(succ, pred_list){
        BB *pred = BBLIST_item(pred_list);
        if(pred == bb){ //Any strange condition.
            find = TRUE;
            break;
        }
    }
    Is_True(find, ("bb_id:%d has one succ_bb_id:%d, but bb is not succ_bb's pred bb",
                    BB_id(bb), BB_id(succ)));

    REGIONAL_CFG_NODE *succ_node = Regional_Cfg_Node(succ);
    Is_True(succ_node, ("succ bb_id:%d has no corresponding regional cfg node",BB_id(succ)));
    REGION *succ_rgn = succ_node->Home_Region();
           
    if(rgn == succ_rgn){
	    //In find regional cfg edge,only loop tail and head is tested.
        find = Find_Regional_Cfg_Edge(node, succ_node, rgn);
    }else{
      	if(Get_Trace(TP_A_REGION, TT_RGN_VERIFY_DEBUG))
          	printf("verify edge: rgn_id:%d  succ_rgn_id:%d\n", rgn->Id(), succ_rgn->Id());
        REGION *par_rgn = rgn->Find_Common_Parent(succ_rgn);
        Is_True(par_rgn, ("rgn_id:%d and succ_rgn_id:%d should has common parent",
                                rgn->Id(), succ_rgn->Id()));
                
        // get the corresponding node(tmp_pred_node) of pred_node in par_rgn
        REGION *tmp_pred_rgn = rgn;
        REGIONAL_CFG_NODE *tmp_pred_node = node;
        while(tmp_pred_rgn != par_rgn){
            tmp_pred_node = tmp_pred_rgn->Regional_Cfg_Node();
            tmp_pred_rgn = tmp_pred_node->Home_Region();
        }
              
        // get the corresponding node(tmp_succ_node) of succ_node in par_rgn
        REGION *tmp_succ_rgn = succ_rgn;
        REGIONAL_CFG_NODE *tmp_succ_node = succ_node;
        while(tmp_succ_rgn != par_rgn){
            tmp_succ_node = tmp_succ_rgn->Regional_Cfg_Node();
            tmp_succ_rgn = tmp_succ_node->Home_Region();
        }
   	    //In find regional cfg edge,only loop tail and head is tested. 
        find = Find_Regional_Cfg_Edge(tmp_pred_node, tmp_succ_node, par_rgn);
        if(!find && Get_Trace(TP_A_REGION, TT_RGN_VERIFY_DEBUG)){
            printf("there is no edge between pred_node_id:%d and succ_node_is:%d in rgn_id:%d \n",
                    tmp_pred_node->Id(), tmp_succ_node->Id(), par_rgn->Id());
            par_rgn->Print(TFile);
        }
    }
                
    if(find == FALSE){
        if (Get_Trace(TP_A_REGION, TT_RGN_VERIFY_DEBUG)){
            printf("rgn_id:%d  ", rgn->Id());
            printf("bb_id:%d  ", BB_id(bb));
            printf("succ_bb_id:%d\n", BB_id(succ));
                   
            draw_global_cfg();
            draw_regional_cfg(rgn->Tree()->Root());
        }
		//--------------------------------------------------------------------------
		// If did not find,must check whether this is a global cycle,for overlapped 
		// loops,this condition may happen.
		//--------------------------------------------------------------------------
		BOOL is_cycle = TRUE;
		for (GLOBAL_CYCLE_VECTOR_ITER cycle_iter = global_cycles.begin();
            cycle_iter != global_cycles.end();cycle_iter++) {
            if ((succ == (*cycle_iter).dest)&&(bb == (*cycle_iter).src)) {
                is_cycle  = TRUE;       
    
				break;
            }
        }
            
		if (!is_cycle) {
	        Is_True(FALSE, ("can not find corresponding edge bb_id:%d and succ_bb_id:%d",
                                BB_id(bb), BB_id(succ)));
		}
    }
}


//=============================================================================
//
//  Verify_Region_Tree
//
//  Verify the whole region tree including region, regional_cfg etc
//
//=============================================================================
void Verify_Region_Tree(REGION_TREE *tree, BB *first_bb){
    // verify each region;
    for(INNERMOST_REGION_FIRST_ITER iter(tree); iter!=0; ++iter){
        // verify _region_set;
        REGION *region = *iter;
        BOOL find = FALSE;
        for( INT32 i=0; i<tree->_region_set.size(); i++){
            if((tree->_region_set)[i]==region){
                find = TRUE;
                break;
            }
        }
        Is_True(find,("region %d should be in _region_set", region->Id()));
        
        //verify seme region
        if(region->Region_Type() == SEME)
            Verify_SEME_Region(region);
        
        // verify regional cfg
        Verify_Cfg(region->Regional_Cfg());


        // verify the relation of parent and kid
        REGION *par_rgn = region->Parent();
        if(par_rgn){
            Is_True(region->Is_Kid_Region_Of(par_rgn), 
                    ("region should be one kid region of its parent region"));
        }else
            Is_True(region->Region_Type()==ROOT, ("only root region has no parent region"));
    }

    // verify the relation of global cfg and regional cfg
    for(BB *bb = first_bb; bb!=0; bb = BB_next(bb)){
        Verify_Entry_Exit_BB(bb);

        REGIONAL_CFG_NODE *node = Regional_Cfg_Node(bb);
        Is_True(node, ("bb:%d has no corresponding regional cfg node", BB_id(bb)));
        
        REGION *rgn = node->Home_Region();
        BBLIST *succ_list;
        FOR_ALL_BB_SUCCS(bb, succ_list) { 
            BB *succ = BBLIST_item(succ_list);
            //verify pred and succ relation in global cfg
            Verify_Global_Edge(bb,succ);
        }
    }
}



