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
//
//  Module : ekapi_properties.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_properties.cxx,v $
//
//  Description:
//  ============
//  hold all isa properties functions in access layer of IPFEC.
//  and encapsulate KAPI first layer opcode of access layer.
//=============================================================================

#include <stdio.h>
#include "ekapi_ia64.h"
#include "ekapi_util.h"

//  Return operate code properties count
int EKAPI_OppCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "opp_t");
}

//  Return operate code properties count which is never owned by any op
int EKAPI_FalseOppCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "false_opp_t");
}

//  Return op properties Name for an opp id
char *EKAPI_Oppid2Name(void *pknobs, int index)
{
    char *name;
    name = KAPI_EnumName(pknobs, index, "opp_t");
    FmtAssert(name, 
              ("KNOBS FILE Error: opp_t index %d is not existed\n", index)
              );
    return name;
}

//  Return op flase properties Name for an opp id
char *EKAPI_FalseOppid2Name(void *pknobs, int index)
{
    char *name;
    name = KAPI_EnumName(pknobs, index, "false_opp_t");
    FmtAssert(name, 
              ("KNOBS FILE Error: false_opp_t index %d is not existed\n", index)
              );
    return name;    
}

//  Return flag composed by op properties for an op
UINT64 EKAPI_OppMask4op(void *pknobs, int opid)
{
    bv_t *bv;
    char op[100]="%";
    BOOL b;
    int j, iknob;
    UINT64 flag=0;
    char errmsg[200];
        
    strcat(op,EKAPI_OpName4id(pknobs, opid));
 
    // here use function of pro64 because pass opcode test
    iknob = KAPI_ArrayIndex(pknobs, "opprop", op);
    sprintf( errmsg, "Op %s's properties not exist!\n", op);
    Is_True(iknob>=0, (errmsg));
    
    bv = KAPI_GetBvVariable(pknobs,"opprop", iknob);
        
    // print flag
    for ( j=0; j<bv->n32Chunks * 32; j++ ) {
        b = ( 0 != isbvBITSET( bv, j ) );
        if (b) {
            flag |= 1ULL << j ;
        }
    }
    return flag;
}
