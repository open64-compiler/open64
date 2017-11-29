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
//  Module : ekapi_subset.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_subset.cxx,v $
//
//  Description:
//  ============
//  access subset info from knobs file, and encapsulate KAPI
//  first layer subset info of access layer.
//=============================================================================

#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "ekapi_ia64.h"
#include "ekapi_util.h"

char *EKAPI_Op2SubSet( void *pknobs, int index)
{
    char  *str_knob, *str_subset, *buf;

    str_knob = KAPI_attribute4index( pknobs, "opcode", index);

    //get the fourth field from whole attribute string str_knob.
    strtok( str_knob, "," );
    strtok( NULL, "," );
    strtok( NULL, "," );
    buf = strtok( NULL, "," );
    str_subset = strdup(StrTrim(buf));
    free(str_knob);

    return str_subset;
}

int EKAPI_SubsetCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "ISA_SUBSET");
}

char *EKAPI_SubsetName4id(void *pknobs, int index)
{
    return KAPI_EnumName(pknobs, index, "ISA_SUBSET");
}
