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
// Module: scc_finder.cxx
// $Revision: 1.1 $
// $Date: 2005/12/30 01:47:13 $
// $Author: weitang $
// $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/scc_finder.cxx,v $
//
//======================================================================

#include "scc_finder.h"

//====================================================================
//
// Find_Scc
//
// Main control function of finding max strongly connected components.
//
// NOTE:  
//
//====================================================================
void 
SCC_FINDER::Find_Scc(REGIONAL_CFG *cfg) {
    INT32 next_dfn = 0;
    INT_VECTOR dfn(cfg->_seq_num+2, (INT32)0,INT_ALLOC(&(_m)));
    INT_VECTOR low_link(cfg->_seq_num+2, (INT32)0,INT_ALLOC(&(_m)));
    BS *bs   = BS_Create_Empty(cfg->_seq_num+2, &_m); 
    _scc_set.clear();
    //Suppose the first node is not connected to a irreducible edge.
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(cfg);iter != 0;++iter) {
        REGIONAL_CFG_NODE *node;
        Is_True(node != NULL,("The node is NULL in Find Scc."));
        node = *iter;

        if (!BS_MemberP(bs,node->Id())) {
            Strong_Components(node,dfn,low_link,bs,next_dfn);
        }    
    }    
}

//====================================================================
//
// Strong_Components
//
// Calculate the strongly connected components.
//
// NOTE:  
//
//====================================================================
void 
SCC_FINDER::Strong_Components(REGIONAL_CFG_NODE *node,INT_VECTOR& dfn,
            INT_VECTOR& low_link,BS *bs,INT32& next_dfn) {
    INT32 id = node->Id();
    next_dfn+=1;
    dfn[id]      = next_dfn;
    low_link[id] = dfn[id];
    if (Find_In_Vector(node,_in_stack)==(NODE_VECTOR_ITER)0) {
        _stack.push(node);
        _in_stack.push_back(node);
    }
    
    for (CFG_SUCC_NODE_ITER iter(node);iter != 0; ++iter) {
        REGIONAL_CFG_NODE *succ = *iter;
        Is_True(succ != NULL,("The succ node is NULL in Strong Components."));

        if (!BS_MemberP(bs,succ->Id())) {
            
            if (dfn[succ->Id()] == 0) {
                Strong_Components(succ,dfn,low_link,bs,next_dfn);
                
                if (low_link[id] > low_link[succ->Id()]) {
                    low_link[id] = low_link[succ->Id()];
                }
            } else if (dfn[succ->Id()] < dfn[id]) {
            
                if (low_link[id] > dfn[succ->Id()]) {
                    low_link[id] = dfn[succ->Id()];
                }
            }
        }
    }
    
    if (low_link[id] == dfn[id]) {
         _scc.clear();
         while (!_stack.empty()) {
             REGIONAL_CFG_NODE *top = _stack.top();
             
             if (dfn[top->Id()] < dfn[id]) {
                _scc_set.push_back(_scc);
                return;
             }

             _scc.push_back(top); 
             BS_Union1D(bs,top->Id(),&_m);
             _stack.pop();
         }
         //-------------------------------------------------------------------
         // Assert: This SCC FINDER will deem every sigle node as a SCC 
         // component.
	 //-------------------------------------------------------------------
         _scc_set.push_back(_scc);
     }         
}

//====================================================================
//
// Print
//
// Print out all SCCs
//
// NOTE:  
//
//====================================================================    
void    
SCC_FINDER::Print(FILE *f) {
    typedef SCC_VECTOR::iterator ITER;
    
    ITER iter;
    INT32 i = 0;  
    fprintf(f,"Do the print of scc!"); 
    for ( iter = _scc_set.begin();iter != _scc_set.end();iter++) {
        NODE_VECTOR node_set = *iter;
        i++;
        fprintf(f,"The SCC : No. %d is \n",i); 
        for (NODE_VECTOR_ITER node_iter = node_set.begin();
            node_iter != node_set.end();node_iter++) {
            REGIONAL_CFG_NODE *node = *node_iter;
            fprintf(f,"REGIONAL_CFG_NODE id: %d\n",node->Id());
        }
    }       
}    
