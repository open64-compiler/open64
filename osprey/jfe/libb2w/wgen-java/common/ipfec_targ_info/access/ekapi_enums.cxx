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
//  Module : ekapi_enum.cxx_
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_enums.cxx,v $
//
//  Description:
//  ============
//  hold all ENUM CLASS functions in access layer of ORC.
//  and encapsulate KAPI first layer opcode of access layer.
//  Enum Class hold completer in operands.
//=============================================================================

#include "ekapi_ia64.h"
#include "ekapi_util.h"

// Return Enum Class Count.
int EKAPI_EnumClassCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "ec_t");
}

// Return Enum Class name for each index
char *EKAPI_EnumClassName(void *pknobs, int index)
{
    char *name;
    name = KAPI_EnumName(pknobs, index, "ec_t");
    Is_True(name, 
            ("KNOBS FIEL: Enum Class not index %d \n", index)
            );
    return name;
}

// Return Enum Class id for given Enum Class name
int EKAPI_EnumClassid4name(void *pknobs, char *name)
{
    return KAPI_EnumIndex(pknobs, "ec_t", name);
}

// Return Enum Value Class Count
int EKAPI_EvClassCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "ecv_t");
}

// Return Enum Vlaue Class asm name for document
char *EKAPI_EvClassAsmName(void *pknobs, int index)
{
    char *name;
    name = KAPI_GetStringVariable(pknobs, "ecvname", index);
    Is_True(name, 
            ("KNOBS FIEL: Enum Class not index %d \n", index)
            );
    return name;
}


// Return Enum Value Class name
char *EKAPI_EvClassName(void *pknobs, int index)
{ 
    char *name;
    name = KAPI_EnumName(pknobs, index, "ecv_t");
    Is_True(name, 
            ("KNOBS FIEL: Enum Value Class not index %d \n", index)
            );
    return name;
}

// Return Enum Value Class Int Value
int EKAPI_EvClassValue(void *pknobs, int index)
{
    return KAPI_GetIntegerVariable(pknobs, "ecvval", index);
}

// Return Enum Class' first Enum value Class identical;
int EKAPI_EcFirstValue(void *pknobs, int ecid)
{
    int f_ecv, l_ecv=-1;
    int j, count, iknob;
    
    f_ecv = count = EKAPI_EvClassCount(pknobs);
    for (j=0; j<count; j++)
    {
        iknob = KAPI_GetEnumVariable(pknobs, "ecv_ec", j);
        if (iknob == ecid) {
            if (j < f_ecv) f_ecv = j;
            if (j > l_ecv) l_ecv = j;
        }
    }
    
    return f_ecv;
}

// Return Enum Class' last Enum value Class identical;
int EKAPI_EcLastValue(void *pknobs, int ecid)
{
    int f_ecv, l_ecv=-1;
    int j, count, iknob;
    
    f_ecv = count = EKAPI_EvClassCount(pknobs);
    for (j=0; j<count; j++)
    {
        iknob = KAPI_GetEnumVariable(pknobs, "ecv_ec", j);
        if (iknob == ecid) {
            if (j < f_ecv) f_ecv = j;
            if (j > l_ecv) l_ecv = j;
        }
    }
    
    return l_ecv;
}
