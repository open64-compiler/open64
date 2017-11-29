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

#ifndef  interval_processor_INCLUDED
#define  interval_processor_INCLUDED

//============================================================================
//  IntervalProcessor.h
//
//  This file contains the interface of process intervals in a region.
//
//============================================================================
#include "region.h"
#include "region_verify.h"

typedef std::list<REGIONAL_CFG_EDGE *,EDGE_ALLOC>   EDGE_LIST;
typedef std::queue<REGIONAL_CFG_EDGE *,EDGE_LIST>   EDGE_QUEUE;  


class INTERVAL_PROCESSOR_MEM {

protected:
    MEM_POOL _m;

    INTERVAL_PROCESSOR_MEM() {
         MEM_POOL_Initialize( &_m, "INTERVAL_PROCESSOR_MEM", true );
         MEM_POOL_Push( &_m );
    }
    
    ~INTERVAL_PROCESSOR_MEM() {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    }
};

//============================================================================
//
//   Class Name: INTERVAL_PROCESSOR
//
//   Base Class: INTERVAL_PROCESSOR_MEM
//
//   Derived Class: <none>
//
//   Class Description: This class find out all intervals in the input region's
//   regional cfg.
//
//============================================================================   

class INTERVAL_PROCESSOR : public INTERVAL_PROCESSOR_MEM {
typedef mempool_allocator<BS*>                 BS_ALLOC;
typedef std::vector<BS*,BS_ALLOC>                   BS_VECTOR;
typedef mempool_allocator<REGION*>             REGION_ALLOC;
typedef std::vector<REGION*,REGION_ALLOC>           REGION_VECTOR;
typedef REGION_VECTOR::iterator                REGION_VECTOR_ITER;
typedef mempool_allocator<NODE_VECTOR>         NODE_VECTOR_ALLOC;
typedef std::vector<NODE_VECTOR,NODE_VECTOR_ALLOC>  SCC_VECTOR;
typedef SCC_VECTOR::iterator                   SCC_VECTOR_ITER;

friend void Verify_SEME_Region(REGION *r);

private:   
    EDGE_VECTOR         _cycles;
    EDGE_VECTOR         _backedges;
    EDGE_VECTOR         _impedges;	// bug fix for OSP_110
    REGION_VECTOR       _imp_region;
    NODE_VECTOR         _improper_node;
    BS_VECTOR           _bs_vector; 
    BS                 *_dom_init;
    REGION             *_root;
    
    void                Find_Cycles(void);  
    
    void                Detect_Cycle(REGIONAL_CFG_NODE *node,INT_VECTOR dfn,
                        BS *visited,INT32 next_dfn);
    
    void                Compute_Dominators(void);
    
    void                Init_Dom_Set(REGIONAL_CFG *cfg);
    
    void                Set_Node_Dom_Set(REGIONAL_CFG_NODE *node,BS *bs);
    
    void                Collect_Backedges(void);

    void    	     Collect_Improper_Nodes(void);	// bug fix for OSP_110

    void                Construct_Loops(void);
    
    NODE_VECTOR         Detect_Loop_Scope(REGIONAL_CFG_EDGE *edge);
    
    EDGE_QUEUE          Is_Backedge_Target(REGIONAL_CFG_NODE *node); 
    EDGE_VECTOR         Is_Backedge_Source(REGIONAL_CFG_NODE *node);
    
    void                Update_Backedges(REGIONAL_CFG_NODE *r_node);
   
    void                Construct_Improper(void);
    
public:        
    INTERVAL_PROCESSOR(REGION *r) :   
    _imp_region(REGION_ALLOC(&_m)),
    _improper_node(NODE_ALLOC(&_m)),
    _cycles(EDGE_ALLOC(&_m)), 
    _backedges(EDGE_ALLOC(&_m)),
    _impedges(EDGE_ALLOC(&_m)),    // bug fix for OSP_110
    _bs_vector((r->Regional_Cfg())->_seq_num+2,(BS*)NULL,BS_ALLOC(&_m))
    {
        this->_root = r;
        this->_dom_init = NULL;
    }           
    
    ~INTERVAL_PROCESSOR() {} 
   
    void Process(void);
    
    void Print_Cycle(FILE *f = stderr);
    void Print_Dominators(REGIONAL_CFG *cfg, FILE *f = stderr);
    void Print_Loops(REGIONAL_CFG *cfg, FILE *f = stderr);
};

#endif
