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

#ifndef  global_cycles_finder_INCLUDED
#define  global_cycles_finder_INCLUDED

#include "region.h"

class GLOBAL_CYCLES_FINDER_MEM {

protected:
    MEM_POOL _m;

    GLOBAL_CYCLES_FINDER_MEM() {
         MEM_POOL_Initialize( &_m, "GLOBAL_CYCLES_MEM", true );
         MEM_POOL_Push( &_m );
    }
    
    ~GLOBAL_CYCLES_FINDER_MEM() {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    }
};

//============================================================================
//
//   Class Name: GLOBAL_CYCLES_FINDER
//
//   Base Class: GLOBAL_CYCLES_FINDER_MEM
//
//   Derived Class: <none>
//
//   Class Description: This class try to find out all cycles in a global cfg
//   and return a BBMAP,which store all cycles dest bb.
//
//   Note:  
//
//============================================================================   

class GLOBAL_CYCLES_FINDER : public GLOBAL_CYCLES_FINDER_MEM {
private:
    void  Detect_Global_Cycle(GLOBAL_CYCLE_VECTOR& cycles,BB *bb,
          INT_VECTOR dfn,BS *visited,INT32 next_dfn);

public:   
    void  Find_Global_Cycles(GLOBAL_CYCLE_VECTOR& cycles);  
    void  Print(GLOBAL_CYCLE_VECTOR cycles,FILE *f = stderr);
};

#endif
