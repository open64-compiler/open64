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
//  Module : ekapi_opcode_access.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_opcode_access.cxx,v $
//
//  Description:
//  ============
//  access opcode from knob file, and encapsulate KAPI 
//  first layer opcode of access layer.
//=============================================================================

#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "ekapi_ia64.h"
#include "ekapi_util.h"

// Function: EKAPI_OpCount
//     pknobs: A pointer to the variable initialized by KAPI_initialize().
// Return the count of all the opcodes
int EKAPI_OpCount(void *pknobs)
{
    return KAPI_count4attribute( pknobs , "opcode" );
};

// Function: EKAPI_Op2Fu
//     pknobs: A pointer to the variable initialized by KAPI_initialize().
//     index:  Index of the op.
// Return a function class of an op
char *EKAPI_Op2Fu( void *pknobs, int index)
{
    char  *str_knob, *str_fu,*fu_name;
    int  ID;
    char *name;

    str_knob = KAPI_attribute4index( pknobs, "opcode", index);
    FmtAssert(str_knob, ("opcode attribute should not be NULL"));
        
    //get the third field from whole attribute string str_knob.
    ID      = atoi(strtok( str_knob, "," ));
    Is_True(ID==index, ("Opcode %d not equal to index %d!\n", ID, index));
    strtok( NULL, "," );
    
    str_fu      = StrTrim(strtok( NULL, "," ));
    fu_name = StrTrim(strtok(str_fu, ":"));
    
    // fu_name[1] = StrTrim(strtok(NULL, ":"));
    
    name = strdup(fu_name);

    free(str_knob);
 
    return name;
}


// Function: EKAPI_OpName4id
//   pknobs: A pointer to the variable initialized by KAPI_initialize().
//   index:  Index of the op.
// Return an op's name according to the code
char * EKAPI_OpName4id(void *pknobs, int index)
{
    char *str_knob, *str_name, *name;
    int   ID;

    str_knob = KAPI_attribute4index( pknobs, "opcode", index);

    //get the second field from whole attribute string str_knob.
    ID      = atoi(strtok( str_knob, "," ));
    Is_True(ID==index, ("Opcode %d not equal to index %d!\n", ID, index));

    str_name = StrTrim(strtok( NULL, "," ));
    name = strdup(str_name);
    
    free(str_knob);
    return name;
}
// Function EKAPI_Op2FuIndex
//   pknobs:  A pointer to the variable initialized by KAPI_initialize().
//   index:   index of an op
// Return an op's function class identical
int EKAPI_Op2FuIndex(void *pknobs, int index)
{
    char *buf;
    int fu;
    
    buf = EKAPI_Op2Fu(pknobs, index);
    if ( (strcmp(buf, "fuUNKNOWN") == 0)  || 
         (strcmp(buf, "fuDUMMY") ==0) )
    {
        fu = -2;
    }
    else
    {
         //If not hold this function class ,it will return -1
         fu  = KAPI_fuName2fuIndex( pknobs, buf );
    }
    
   // free(buf);
    return fu;
}

