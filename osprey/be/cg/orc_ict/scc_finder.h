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

#ifndef  scc_finder_INCLUDED
#define  scc_finder_INCLUDED

#include "region.h"

class SCC_FINDER_MEM {

protected:
    MEM_POOL _m;

    SCC_FINDER_MEM() {
         MEM_POOL_Initialize( &_m, "SCC_FINDER_MEM", true );
         MEM_POOL_Push( &_m );
    }
    
    ~SCC_FINDER_MEM() {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    }
};

//============================================================================
//
//   Class Name: SCC_FINDER
//
//   Base Class: SCC_FINDER_MEM
//
//   Derived Class: <none>
//
//   Class Description: Find out all Stronly connected components in a cfg.
//
//   Note:  
//
//============================================================================ 
class SCC_FINDER : public SCC_FINDER_MEM {
typedef mempool_allocator<NODE_VECTOR>         NODE_VECTOR_ALLOC;
typedef std::vector<NODE_VECTOR,NODE_VECTOR_ALLOC>  SCC_VECTOR;

private:
    NODE_VECTOR  _in_stack;
    NODE_VECTOR  _scc; 
    NODE_STACK   _stack;  
    SCC_VECTOR   _scc_set;
    REGIONAL_CFG *_cfg;
    
    void Strong_Components(REGIONAL_CFG_NODE *node,INT_VECTOR& dfn,
         INT_VECTOR& low_link,BS *bs,INT32& next_dfn);
    
public:
    SCC_FINDER() : 
    _in_stack(NODE_ALLOC(&(_m))),
    _scc(NODE_ALLOC(&(_m))),
    _scc_set(NODE_VECTOR_ALLOC(&(_m))),
    _stack(NODE_VECTOR(NODE_ALLOC(&(_m)))) {
        
    }   
    
    ~SCC_FINDER() {}
    
    void Find_Scc(REGIONAL_CFG *cfg);       
    SCC_VECTOR Scc_Set() { return _scc_set; }
    
    void Print(FILE *f = stderr);
};     

#endif
        
