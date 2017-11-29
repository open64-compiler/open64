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

//======================================================================
//
// Module: interval_processor.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/interval_processor.cxx,v $
//
//======================================================================

#include "interval_processor.h"
#include "scc_finder.h"
#include "tracing.h"
#include "ipfec_defs.h"

//============================================================================
//  
//  Find Cycles
//
//  Main control function of find out all cycles.
//   
//============================================================================ 
void   
INTERVAL_PROCESSOR::Find_Cycles(void) {
    NODE_VECTOR_ITER iter;
    
    REGIONAL_CFG *cfg     = _root->Regional_Cfg();
    INT32        next_dfn = 0;
    INT_VECTOR   dfn(cfg->_seq_num+2, (INT32)0,INT_ALLOC(&_m)); 
    BS           *visited      = BS_Create_Empty(cfg->_seq_num+2, &_m); 

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("The node is NULL in Find Cycles"));
	if (node->First_Pred() == NULL) {
            Detect_Cycle(node,dfn,visited,next_dfn);
        }
    }   
}

//============================================================================
//  
//  Detect Cycle
//
//  Recursive function to detect all cycles in the cfg.
//   
//============================================================================ 
void 
INTERVAL_PROCESSOR::Detect_Cycle(REGIONAL_CFG_NODE *node,INT_VECTOR dfn,BS *visited,INT32 next_dfn) {
    REGIONAL_CFG_EDGE *e = NULL;
    
    dfn[node->Id()] = ++next_dfn;
    
    for (CFG_SUCC_NODE_ITER iter(node);iter != 0;++iter) {
        REGIONAL_CFG_NODE *succ = *iter;
        Is_True(succ != NULL,("The succ node is NULL in Detect Cycle."));

        if (!BS_MemberP(visited,node->Id())) {
        
            if (dfn[succ->Id()] == 0) {
                Detect_Cycle(succ,dfn,visited,next_dfn);
            }    
            else if (dfn[succ->Id()] <= dfn[node->Id()]) {
                e = node->Find_Succ_Edge(succ);
                Is_True(e != NULL,("The edge is NULL in Detect Cycle."));

                _cycles.push_back(e);
            }
        }
    }     

    BS_Union1D(visited,node->Id(),&_m);
}

//============================================================================
//  
//  Compute_Dominator
//
//  Compute out the dominator nodes of each node in a regional cfg.
//   
//============================================================================ 
void  
INTERVAL_PROCESSOR::Compute_Dominators(void) {
    BS *dom_init = NULL;
    BS *temp     = NULL;
       
    REGIONAL_CFG *cfg     = _root->Regional_Cfg();
    NODE_VECTOR  node_set = cfg->Node_Set(); 
    
    temp = BS_Create(2+cfg->_seq_num, &_m);
    _dom_init = BS_Create_Empty(2+cfg->_seq_num, &_m);
        
    for (NODE_VECTOR_ITER iter = node_set.begin();
        iter != node_set.end(); iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("The node is NULL in Compute Dominators."));

        BS_Union1D(_dom_init,node->Id() , &_m);
    }  
    
    Init_Dom_Set(cfg);
       
    BOOL changed = TRUE;
    
    while (changed) {
        changed = FALSE;
    
        for (NODE_VECTOR_ITER top_iter = node_set.begin();
            top_iter != node_set.end();top_iter++) {
            REGIONAL_CFG_NODE *node = *top_iter;
            Is_True(node != NULL,("The node is NULL in Compute Dominators."));

            BS *node_dom_set;
            BS *src;
            node_dom_set = _bs_vector[node->Id()];
            src = node_dom_set;
            
            for (CFG_PRED_NODE_ITER iter(node);iter != 0;++iter) {
                REGIONAL_CFG_NODE *pred = *iter;
                BS_IntersectionR(temp, src, _bs_vector[pred->Id()]);
                src = temp;
            } 
            
            BS_Union1D(src, node->Id(), &_m);
    
            if ( ! BS_EqualP(src, node_dom_set) ) {
                BS_CopyD(node_dom_set, src, NULL);
                changed = TRUE;
            }
        } 
    } 
}

//============================================================================
//  
//  Init_Dom_Set
//
//  Initialize every node's dominator set.
//   
//============================================================================ 
void 
INTERVAL_PROCESSOR::Init_Dom_Set(REGIONAL_CFG *cfg) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR node_set(temp_alloc);
	node_set = cfg->Node_Set();
        
    for (NODE_VECTOR_ITER iter = node_set.begin();
        iter != node_set.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        Is_True(node != NULL,("Node is NULL in Init Dom Set."));

        if ((node->First_Pred())==NULL) {
            Set_Node_Dom_Set(node, BS_Create_Empty(2+cfg->_seq_num, &_m));
            BS_Union1D(_bs_vector[node->Id()], node->Id(), &_m);
        } else {
            Set_Node_Dom_Set(node, BS_Copy(_dom_init, &_m));
        }
    }        
}

//============================================================================
//  
//  Set_Node_Dom_Set
//
//  Set the node's dominate set.
//   
//============================================================================ 
void 
INTERVAL_PROCESSOR::Set_Node_Dom_Set(REGIONAL_CFG_NODE *node,BS *bs) {
    _bs_vector[node->Id()] = bs;
}



//====================================================================
//
// Collect_Backedeges
// 
// Find out the backedges from the cycles.Backedges are those cycles
// whose dest node dominate its src node.
//
// NOTE: 
//
//====================================================================
void 
INTERVAL_PROCESSOR::Collect_Backedges(void) {

    for (EDGE_VECTOR_ITER iter = _cycles.begin();iter != _cycles.end();iter++) {
       
        REGIONAL_CFG_EDGE *e    = *iter;
        REGIONAL_CFG_NODE *src  = e->Src();
        REGIONAL_CFG_NODE *dest = e->Dest();
        BS *bs = _bs_vector[src->Id()];
    
        if (BS_MemberP(bs,dest->Id())) {
            _backedges.push_back(e);
        }        
        else {
            // bug fix for OSP_110
            _impedges.push_back(e);
           /*
            if (Find_In_Vector(src,_improper_node) == _improper_node.end()) {
                _improper_node.push_back(src);
            }
            if (Find_In_Vector(dest,_improper_node) == _improper_node.end()) {
                _improper_node.push_back(dest);
            }
           */
        }
    }
}

// bug fix for OSP_110
//====================================================================
//
// Collect_Improper_Nodes
// 
// Find out all the improper nodes.
//
// NOTE: 
//
//====================================================================
void
INTERVAL_PROCESSOR::Collect_Improper_Nodes(void){
    for (EDGE_VECTOR_ITER iter = _impedges.begin();iter != _impedges.end();iter++) {
        REGIONAL_CFG_EDGE *e    = *iter;
        REGIONAL_CFG_NODE *src  = e->Src();
        REGIONAL_CFG_NODE *dest = e->Dest();
 

        if (Find_In_Vector(src,_improper_node) == _improper_node.end()) {
            _improper_node.push_back(src);
        }
        if (Find_In_Vector(dest,_improper_node) == _improper_node.end()){
            _improper_node.push_back(dest);
        }
    }
}
//====================================================================
//
// Construct_Loops
// Description:
//   Find out all loops of the backedges.It is a reverse preorder 
//   traverse of the local cfg,when meeting a target of backedge,try
//   to form a loop region for it;according to the loop region's cfg,
//   adjust the region tree.After a loop region formed,update the 
//   _backedges set.That is,replace some backedges whose edge src
//   node is in the newly formed loop region with a new edge.The 
//   new edge's src node is the newly formed loop region's 
//   correspoding regional cfg node.When meeting a node which is 
//   loophead of more than one loops,we must update the tar_edges
//   queue after the loop region formed.Continue to form region until
//   all the backedges connected to the node has been processed.
//
// NOTE: 
//
//====================================================================
void 
INTERVAL_PROCESSOR::Construct_Loops(void) {
    NODE_ALLOC        temp_alloc(&_m);
    NODE_VECTOR       temp_vector(temp_alloc);
    NODE_STACK        node_stack(temp_vector);
    NODE_VECTOR       node_set(temp_alloc);
    NODE_VECTOR       loop_nodes(temp_alloc);
    EDGE_ALLOC        edge_temp_alloc(&_m);
    EDGE_LIST         edge_temp_list(edge_temp_alloc);

    REGIONAL_CFG_EDGE *backedge = NULL;
    REGION_TREE  *tree = _root->Tree();
    REGIONAL_CFG *cfg  = _root->Regional_Cfg();
    
    node_set = cfg->Node_Set();
    
        
    for (PREORDER_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter) {
        REGIONAL_CFG_NODE* node = *iter;
        node_stack.push(node);
    }
    
    while (!node_stack.empty()) {
    
        REGIONAL_CFG_NODE *top = node_stack.top();
        node_stack.pop();
        EDGE_QUEUE tar_edges(edge_temp_list);
		tar_edges = Is_Backedge_Target(top);
      
        while (!tar_edges.empty()) {
            BOOL              improper   = FALSE;
            REGIONAL_CFG_EDGE *e         = tar_edges.front();
            tar_edges.pop();

            REGION       *r   = (e->Dest())->Home_Region();
            Is_True(r != NULL,("The home region of edge e is NULL."));
            
            REGIONAL_CFG *cfg = r->Regional_Cfg();
            Is_True(cfg != NULL,("The cfg of region r is NULL."));

            NODE_VECTOR loop_nodes(temp_alloc);
            loop_nodes = Detect_Loop_Scope(e);
            Is_True(!loop_nodes.empty(),("The loop nodes found is empty."));

            LOOP_REGION *loop      = tree->Add_Loop_Region(r,e,loop_nodes);
            
            if (improper) {
                _imp_region.push_back(loop);
            }
            
            REGIONAL_CFG_NODE *r_node = loop->Regional_Cfg_Node();
            //------------------------------------------------------------ 
            // Find out all backedges whose source node in the loop nodes
	    // set and update them.But those edges which are contained by
	    // loop nodes are not considered.(With same loop head as the
	    // newly formed loop region)
            //------------------------------------------------------------
            for (NODE_VECTOR_ITER node_iter  = loop_nodes.begin();
                node_iter != loop_nodes.end();node_iter++) {
                REGIONAL_CFG_NODE *loop_node = *node_iter;
                Is_True(loop_node != NULL,("Loop node is NULL in Construct Loops."));

                EDGE_VECTOR       edges(edge_temp_alloc);
		edges = Is_Backedge_Source(loop_node);
                //----------------------------------------------------------
                // The backedge has been deleted when do loop construct,
		// but the pointer e is not deleted yet.It must be deleted
		// to be assure it is correct.
		//----------------------------------------------------------
                if (!edges.empty()) {
                    
                    for (EDGE_VECTOR_ITER src_iter = edges.begin();
                        src_iter != edges.end();src_iter++) {
                        REGIONAL_CFG_EDGE *edge    = *src_iter;
            
                        if (edge->Dest() != loop->Loop_Head()) {
			    //-----------------------------------------------
                            // Here a new_edge must be found for if the targ
			    // is not in the cfg,then this algorithm wrong.
			    // Because we shrink from inner to outer.
			    //-----------------------------------------------
                            REGIONAL_CFG_EDGE *new_edge = cfg->Find_Edge(r_node,edge->Dest());
                        
                            for (EDGE_VECTOR_ITER edge_iter = _backedges.begin();edge_iter != _backedges.end();edge_iter++) {
                
                                if (*edge_iter == edge) {
                                    _backedges.erase(edge_iter);
                                    break;
                                }
                            }        
                    
                            _backedges.push_back(new_edge);        
                        } 
                    }   
                }
            }   
            //---------------------------------------------------
            // For those edges with same loophead,try to update 
            // them once a region is constructed.
            //---------------------------------------------------
            EDGE_QUEUE temp;
            
            while (!tar_edges.empty()) {
                REGIONAL_CFG_EDGE *front = tar_edges.front();
                tar_edges.pop();

                if (Find_In_Vector(front->Src(),loop_nodes) == loop_nodes.end()) {
                    REGIONAL_CFG_EDGE *new_edge = cfg->Find_Edge(front->Src(),r_node);
                    
                    if (new_edge != NULL) {
                        front = new_edge;
                    }
                }

                temp.push(front);
            }
            
            tar_edges = temp;

            // bug fix for OSP_110
            //---------------------------------------------------
            // update the improper edges
            //---------------------------------------------------

            for (EDGE_VECTOR_ITER edge_iter = _impedges.begin();edge_iter != _impedges.end();edge_iter++) {
                REGIONAL_CFG_EDGE* e = *edge_iter;
                if(Find_In_Vector(e->Src(), loop_nodes) !=loop_nodes.end()){
                    if(Find_In_Vector(e->Dest(), loop_nodes) ==loop_nodes.end()){
                        REGIONAL_CFG_EDGE *new_edge = cfg->Find_Edge(r_node, e->Dest());
                        if(new_edge != NULL){
                            _impedges.erase(edge_iter);
                            _impedges.push_back(new_edge);
                        }
                    }
                }else if(Find_In_Vector(e->Dest(), loop_nodes) !=loop_nodes.end()){
                    REGIONAL_CFG_EDGE *new_edge = cfg->Find_Edge(e->Src(), r_node);
                    if(new_edge != NULL){
                        _impedges.erase(edge_iter);
                        _impedges.push_back(new_edge);
                    }
                }
            }

            loop_nodes.clear();

        }
    }
}    

//====================================================================
//
// Is_Backedge_Source
//
// Check whether the input node is a backedge's source node.If it is
// ,return the backedge connected to node,otherwise return NULL.  
//
// NOTE: 
//
//====================================================================
EDGE_VECTOR
INTERVAL_PROCESSOR::Is_Backedge_Source(REGIONAL_CFG_NODE *node) {
    EDGE_ALLOC  edge_temp_alloc(&_m);
    EDGE_VECTOR edges(edge_temp_alloc);

    for (EDGE_VECTOR_ITER iter = _backedges.begin();
        iter != _backedges.end();iter++) {
        REGIONAL_CFG_EDGE *e = *iter;
           
        if (e->Src() == node) {
            edges.push_back(e);
        }
    }
    
    return edges;            
} 

//====================================================================
//
// Is_Backedge_Target
// 
// Description:
//   Check whether the input node is a backedge's target node.If it is
//   ,return the backedge connected to node,otherwise return NULL.  
//   Sometimes a node may be edge target of many backedges.Therefore,the
//   return value is a edge queue but not a edge pointer.When finding the 
//   backedges connected to the node,the backedges are deleted from 
//   _backedges set.This is useful when we check Is Backedge Source 
//   after a loop region formed.
//
// NOTE: 
//
//====================================================================
EDGE_QUEUE   
INTERVAL_PROCESSOR::Is_Backedge_Target(REGIONAL_CFG_NODE *node) {
    EDGE_ALLOC  edge_temp_alloc(&_m);
	EDGE_LIST   edge_temp_list(edge_temp_alloc);
    EDGE_QUEUE  edges(edge_temp_list);
    EDGE_VECTOR temp(edge_temp_alloc);

    for (EDGE_VECTOR_ITER iter = _backedges.begin();
        iter != _backedges.end();iter++) {
        REGIONAL_CFG_EDGE *e = *iter;
        
        if (e->Dest() == node) {
            edges.push(e);
            temp.push_back(e);
        }
    }
    
    for (EDGE_VECTOR_ITER iter = temp.begin();iter != temp.end();iter++ )   {
        
        for (EDGE_VECTOR_ITER backedge_iter = _backedges.begin();
            backedge_iter != _backedges.end();backedge_iter++) {
            
            if ((*backedge_iter) == (*iter)) {
                _backedges.erase(backedge_iter);

                break; // The break must be added here.
            }   
        }
    }

    return edges;            
} 

//====================================================================
//
// Detect_Loop_Scope
//
// Caculate the loop scope of a backedge.
//
// NOTE: 
//
//====================================================================
NODE_VECTOR  
INTERVAL_PROCESSOR::Detect_Loop_Scope(REGIONAL_CFG_EDGE *e) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR temp_vector(temp_alloc);
    NODE_STACK  node_stack(temp_vector);
    BS          *bs;
    
    REGION *r = (e->Dest())->Home_Region();
    REGIONAL_CFG *cfg = r->Regional_Cfg();
    INT32 seq_num = cfg->Seq_Num();
    bs = BS_Create_Empty(2+seq_num, &_m);
    REGIONAL_CFG_NODE *loop_head = e->Dest();
    REGIONAL_CFG_NODE *loop_tail = e->Src();
    
    BS_Union1D(bs,loop_head->Id() , &_m);
    BS_Union1D(bs,loop_tail->Id() , &_m);
    
    if (!(loop_head == loop_tail)) {
        node_stack.push(loop_tail);
        
        while (!node_stack.empty()) {
            REGIONAL_CFG_NODE *node = node_stack.top();
        
            node_stack.pop();
        
            for (CFG_PRED_NODE_ITER iter(node);iter != 0; ++iter) {
                REGIONAL_CFG_NODE *pred = *iter;
            
                if (!BS_MemberP(bs,pred->Id())) {
                    BS_Union1D(bs,pred->Id() , &_m);
                    node_stack.push(pred);
                }    
            }
        }   
    }
    
    NODE_VECTOR node_set(temp_alloc);
    node_set = cfg->Node_Set();
    NODE_VECTOR nodes(temp_alloc);

    for (NODE_VECTOR_ITER iter = node_set.begin();
        iter != node_set.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
 
        if (BS_MemberP(bs,node->Id())) {
            nodes.push_back(node);
        }
    }
    
    return nodes; 
}

//============================================================================
//  
//  Process
//
//  Process the intervals in a regional cfg.Here the intervals means loop,
//  irreducible parts and others.Refer to the paper.
//   
//============================================================================ 
void
INTERVAL_PROCESSOR::Process(void) {
    // Find Cycles can be improved by not traverse a node already with 
    // all succs visited twice.
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin find cycles.\n");
    }
	    
    Find_Cycles();
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish find cycles.\n");
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"Cycles found,the cycles are:\n");

    	for (EDGE_VECTOR_ITER iter = _cycles.begin();
            iter != _cycles.end();iter++) {
            REGIONAL_CFG_EDGE *e = *iter;
            
            fprintf(TFile,"Cycle's source :%d ",BB_id((e->Src())->BB_Node()));
            fprintf(TFile,"Cycle's target :%d \n",BB_id((e->Dest())->BB_Node()));
        }
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
        fprintf(TFile,"Begin compute dominator.\n");
    }

    Compute_Dominators();
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish compute dominators.\n");
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"Dominator information:\n");
        Print_Dominators(_root->Regional_Cfg(),TFile);
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Begin collect backedges.\n");
    }
	
    Collect_Backedges();
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish collect backedges.\n");
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The backedges are:\n");
    
        for (EDGE_VECTOR_ITER iter = _backedges.begin();
            iter != _backedges.end();iter++) {
            REGIONAL_CFG_EDGE *e = *iter;

            fprintf(TFile,"Cycle's source :%d ",BB_id((e->Src())->BB_Node()));
            fprintf(TFile,"Cycle's target :%d \n",BB_id((e->Dest())->BB_Node()));
        }
    }
    
    Construct_Loops();
    
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish construct loops from backedges.\n Begin find sccs.\n");
    }

// bug fix for OSP_110
    Collect_Improper_Nodes();

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY)) {
        fprintf(TFile,"Finish collect Improper Nodes.\n");
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_DETAILED)) {
        fprintf(TFile,"The improper nodes are:\n");
    
        for (NODE_VECTOR_ITER iter = _improper_node.begin();
            iter != _improper_node.end();iter++) {
            REGIONAL_CFG_NODE *n = *iter;

            fprintf(TFile,"Node's BB Id :%d \n",BB_id(n->BB_Node()));
        }
    }
		
    if (!_improper_node.empty()) {
        SCC_FINDER  *finder = CXX_NEW(SCC_FINDER(),&_m);
        REGION_TREE *tree   = _root->Tree();
        REGION *r = NULL;
        REGIONAL_CFG *cfg = NULL;
        for (NODE_VECTOR_ITER iter = _improper_node.begin();
            iter != _improper_node.end();iter++ ) {
            REGIONAL_CFG_NODE *node = *iter; 
            r = node->Home_Region();
            Is_True(r != NULL,("Region is null in find improper region!"));
            if (r->Region_Type() == IMPROPER) 
                continue;
            cfg = r->Regional_Cfg();
            Is_True(cfg != NULL,("Regional CFG is null in find improper region!"));
            finder->Find_Scc(cfg);
            SCC_VECTOR scc_set = finder->Scc_Set();
            
            if (!scc_set.empty()) {
                for (SCC_VECTOR_ITER set_iter = scc_set.begin();
                    set_iter != scc_set.end();++set_iter) {
                    NODE_VECTOR nodes = *set_iter;
                    
                    if (nodes.size() > 1) {
                        tree->Add_Improper_Region(r,nodes); 
                    }
                }
            } 
        } 
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_DEBUG)) {
        fprintf(TFile,"Finished finding sccs.\n");
    }
    
}    

//============================================================================
//  
//  Print_Cycle
//
//  Print out all found cycles.
//   
//============================================================================ 
void
INTERVAL_PROCESSOR::Print_Cycle(FILE *f) {
    EDGE_VECTOR_ITER iter;
    
    for (iter = _cycles.begin();iter != _cycles.end();iter++) {
        REGIONAL_CFG_EDGE *e = *iter;
        e->Print(f);
    }
}        

//============================================================================
//  
//  Print_Dominators
//
//  Print out dominators.
//   
//============================================================================ 
void 
INTERVAL_PROCESSOR::Print_Dominators(REGIONAL_CFG *cfg, FILE *f) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR node_set(temp_alloc);
	node_set = cfg->Node_Set();
    
    for (NODE_VECTOR_ITER iter = node_set.begin();
        iter != node_set.end();iter++) {
        REGIONAL_CFG_NODE *node = *iter;
        
        fprintf(f,"The Regional cfg node id is :%d\n",node->Id());
        fprintf(f,"Its dominators are:\n");
        
        BS *bs = _bs_vector[node->Id()];
        
        for (NODE_VECTOR_ITER iterr = node_set.begin();
            iterr != node_set.end();iterr++) {
            REGIONAL_CFG_NODE *n = *iterr;
            
            if (BS_MemberP(bs,n->Id())) {
                fprintf(f,"Dominator:%d\n",n->Id());
            }
        }
    }            
        
}

//============================================================================
//  
//  Print_Loops
//
//  Print out Loops.
//   
//============================================================================ 
void 
INTERVAL_PROCESSOR::Print_Loops(REGIONAL_CFG *cfg, FILE *f) {
    NODE_ALLOC  temp_alloc(&_m);
    NODE_VECTOR node_set(temp_alloc);
	node_set = cfg->Node_Set();
    
    Collect_Backedges();
    fprintf(f,"The backedges are:\n");
    
    for (EDGE_VECTOR_ITER iter = _backedges.begin();
        iter != _backedges.end();iter++) {
        REGIONAL_CFG_EDGE *e = *iter;
        e->Print(f);
    }
    
    for (EDGE_VECTOR_ITER iter = _backedges.begin();
        iter != _backedges.end();iter++) {
        REGIONAL_CFG_EDGE *e = *iter;
        BOOL improper = FALSE;
        NODE_VECTOR nodes = Detect_Loop_Scope(e);
         
        fprintf(f, "Loop scope are:\n"); 
        
        for (NODE_VECTOR_ITER iter = nodes.begin();iter != nodes.end();iter++) {
            REGIONAL_CFG_NODE *node = *iter;
            fprintf(f,"Node in loop:%d\n",node->Id());
        }
    }            
} 
