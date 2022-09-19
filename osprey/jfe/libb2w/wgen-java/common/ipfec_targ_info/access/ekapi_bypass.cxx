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
//  Module : ekapi_bypass.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_bypass.cxx,v $
//
//  Description:
//  ============
//  hold all bypass functions in access layer of IPFEC.
//  and encapsulate KAPI first layer opcode of access layer.
//=============================================================================

#include "ekapi_ia64.h"
#include "ekapi_util.h"

static void
ParseLatency( char *pch, char **ppchFcSrc, char **ppchOppSrc, int *piLatency );


///////////////////////////////////////////////////////////////////
//  Return odd latency list by accessing latency attribute in KAPI
//  And put the number of list into integer pointer :listnum;
ODD_LATENCY *EKAPI_OddLatencyList(void *pknobs, kapi_fu_t fuid, int *listnum)
{
    char *opnd_name, *fu_name;
    int count, i, num;
    int base_lat, lat;
    ODD_LATENCY *latencys;
    
    num = 0;
    *listnum = 0;    
    fu_name = KAPI_fu2fuName(pknobs, fuid, 0);    
    count = KAPI_srcOppCount(pknobs, fuid);
    if (count <= 0 ) {
       return NULL;
    }
    
    kapi_latency_type_t lt = KAPI_fuGetLatencyType(pknobs, fuid);
    base_lat = KAPI_CoreLatency(pknobs, fuid, 0);    
    latencys = (ODD_LATENCY *)malloc(count*sizeof(ODD_LATENCY));
    if (latencys == NULL) {
        FmtAssert(false,
                  ("Malloc ODD_LATENCY failed!")
                  );
    }
  
    int count4latency = KAPI_count4attribute(pknobs, "LATENCY");
    
    //  Tranverse all the attribute LATENCY of KAPI to find odd latency
    //  information, that is not same with Primary latency that given
    //  in file itanium.c
    for(i=0; i<count4latency; i++)
    {
        char *fcsrc, *opnd;
        int ilatency;
        char *buf = KAPI_attribute4index(pknobs, "LATENCY", i);
        ParseLatency( buf, &fcsrc, &opnd, &ilatency );
        if ((strcmp(fcsrc,fu_name) == 0)&&(ilatency != base_lat)) {
            latencys[num].oddlatency = ilatency;
            latencys[num].opndname = opnd;
            num++;
        }
    }
    *listnum = num;
    
    
    return latencys;    
} 

static void
ParseLatency( char *pch, char **ppchFcSrc, char **ppchOppSrc, int *piLatency )
{
    char *pchSrc, *pchVal, *pchLatency;

    // ---- process RHS of string ( latency value ) ---- 

    pchVal = (char *)strchr( pch, '=' );
    if ( pchVal == NULL ) {   // error 
        Is_True(0, ("Malformed latency attribute '%s' -- '=' required", pch ));
        return;
    }
    *pchVal = '\0';   // cut string in half

    // point to first character after '='
    pchLatency = pchVal + 1;

    if ( strspn( pchLatency, "0123456789-" ) != strlen( pchLatency ) ) {
       if (pchLatency) {
           Is_True( 0, ("Badly formed bypass:  latency value '%s' not valid", 
                              pchLatency ));
       }
       else {
           Is_True( 0, ("Badly formed bypass:  latency value '%s' not valid", 
                              "NULL" ));
       }                          
       return;
    }
    *piLatency = atoi( pchLatency );


    // ---- now process LHS of string ----
   
    pchSrc = pch;

    // initialize return values
    *ppchFcSrc   = NULL;
    *ppchOppSrc  = NULL;

    // parse the source operand string
    *ppchFcSrc   = strtok( pchSrc, " /" );
    if ( *ppchFcSrc ) {
        *ppchOppSrc = strtok( NULL, " /" );
    }
}


