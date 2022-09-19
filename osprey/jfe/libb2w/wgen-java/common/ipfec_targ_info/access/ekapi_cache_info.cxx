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
//  Module : ekapi_cache_info.cxx_
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_cache_info.cxx,v $
//
//  Description:
//  ============
//  hold some many functions for cache information;
//  and encapsulate KAPI first layer opcode of access layer.
//=============================================================================

#include "ekapi_ia64.h"
#include "ekapi_util.h"

// Return Cache name count.
int EKAPI_CacheNameCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "cache_names_t");
}

// Return Enum Class name for each index
char *EKAPI_CacheName(void *pknobs, int index)
{
    return KAPI_EnumName(pknobs, index, "cache_names_t");
}
                                             
