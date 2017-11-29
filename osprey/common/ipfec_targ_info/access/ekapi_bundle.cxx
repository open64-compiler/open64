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
//  Module : ekapi_bundle.cxx 
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_bundle.cxx,v $
//
//  Description:
//  ============
//    hold all bundle functions in access layer of IPFEC.
//    and encapsulate KAPI first layer opcode of access layer.
//=============================================================================

#include <malloc.h>
#include "ekapi_ia64.h"
#include "ekapi_util.h"


// Return maximum slot number
int EKAPI_GetMaxSlot(void *pknobs)
{
    return KAPI_GetIntegerVariable(pknobs, "MaxSlot",-1);
}

// Return Bundle bit width
int EKAPI_GetBundleWidth(void *pknobs)
{
    return KAPI_GetIntegerVariable(pknobs, "BundleWidth", -1);
}

// Return count of execute unit type
int EKAPI_ExecUnitCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "eun_t");
}

// Return Bundle template count, now is 16;
int EKAPI_TemplateCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "bid_t");
}

// Return the name of the bundle
char *EKAPI_TemplateName(void *pknobs, int index)
{
    // in knobs file, template are described as like: bidMI_I, but we need
    // "mi_i" here
    char *buf = KAPI_EnumName(pknobs, index, "bid_t");
    buf = StrLower(buf+3); // skip "bid" and do lower case
    return buf;
}

// Return the asm name of the bundle
char *EKAPI_TemplateAsmName(void *pknobs, int index)
{
    // in knobs file, template are described as like: bidMI_I, but we need
    // ".mii" here
    char *buf = KAPI_EnumName(pknobs, index, "bid_t");
    buf = StrLower(buf+2); // skip "bid" and do lower case
    *buf = '.';
    buf = StrClean(buf);
    return buf;
}

//  Return name of execute unit type
char *EKAPI_ExecUnitName(void *pknobs, int index)
{
    return KAPI_EnumName(pknobs, index, "eun_t");
}

//  Return id when given execute unit name 
int EKAPI_EunName2id(void *pknobs, char *name)
{
    return KAPI_EnumIndex(pknobs, "eun_t", name);
}

// Return slot type in execute type in extra knob file for bundle id
void EKAPI_BundleType4bid(void *pknobs, kapi_bid_t bid, int *slt)
{
    int max_slot;
    int i;
    kapi_syl_t *mpslt;
   
    max_slot = EKAPI_GetMaxSlot(pknobs);
    FmtAssert(max_slot>=3,
              ("max_slot:%d is  smaller than 3", max_slot) 
              );
    mpslt = (kapi_syl_t *)malloc(max_slot * sizeof(kapi_syl_t));
    FmtAssert(mpslt,
              ("Malloc mpslt space failed.") 
              );
    //  Determined reserved or not          
    if (KAPI_isReserved_bid(pknobs, bid)) {
        for(i=0; i<max_slot; i++)
        {
            slt[i] = EKAPI_EunName2id(pknobs, "R_Unit");            
        }
    }
    else {
        KAPI_SylOrder_bid(pknobs, bid, mpslt);
        // convert syl_t to eun_t
        for (i=0; i<max_slot; i++)
        {
            slt[i] = Syl2Eun(pknobs, mpslt[i]);
            //  Rule: the last slot use B2_Unit
            if ( (i==max_slot-1) && (slt[i]==EKAPI_EunName2id(pknobs, "B_Unit")) ) {
                slt[i] = EKAPI_EunName2id(pknobs, "B2_Unit");
            }
            //  Rule: template MLX, kapi describe it as MLI,
            // but we need MLL in pro64
            if ( (i==max_slot-1) && (slt[i-1]==EKAPI_EunName2id(pknobs, "L_Unit")) ){
                slt[i] = EKAPI_EunName2id(pknobs, "L_Unit");
            }            
        }
    }
    free(mpslt);
}

//  Comvert syl type to execute unit type ,
int Syl2Eun(void *pknobs, int sylid)
{
    int eunid = -1, order;
    char *sylname;
    char *buf;
    
    sylname = KAPI_EnumName(pknobs, sylid, "syl_t");
    buf = (char*)malloc(strlen(sylname)+2);
    FmtAssert(buf, ("Memory Allocation Failure!\n"));

    // Prepend % into sylname
    // I also don't know why KAPI added % into index of the array!!
    buf[0]='%';
    strcpy(buf+1, sylname);
    order = KAPI_ArrayIndex(pknobs, "eun_sylt", buf);
    eunid = KAPI_GetEnumVariable(pknobs, "eun_sylt", order);
    
    free(buf);
    free(sylname);
    return eunid;
}

//  Return bit vector of execute unit properties  for each op
bv32_t EKAPI_EunMask4op(void *pknobs, int opid)
{
     bv_t *bv;
    char op[100]="%";
    BOOL b;
    int j, iknob;
    UINT64 flag=0;
    char errmsg[200];
        
    strcat(op,EKAPI_OpName4id(pknobs, opid));
        
    // here use function of pro64 because pass opcode test
    iknob = KAPI_ArrayIndex(pknobs, "unitprop", op);
    sprintf( errmsg, "Op %s properties not exist!", op);
    Is_True(iknob>=0, (errmsg));
    
    bv = KAPI_GetBvVariable(pknobs,"unitprop", iknob);
        
    // print flag
    for ( j=0; j<bv->n32Chunks * 32; j++ ) {
        b = ( 0 != isbvBITSET( bv, j ) );
        if (b) {
            flag |= 1ULL << j ;
        }
    }
    return flag;
}

//  Return bundle component count
int EKAPI_BundleCompCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "bidcomp_t");
}

//  Return bundle component name for each index
char *EKAPI_BundleCompName(void *pknobs, int index)
{
    char *str;
    str  =  KAPI_EnumName(pknobs, index, "bidcomp_t");
    FmtAssert(str,
              ("bidcomp_t or index %d is not existed.", index )
             );
             
    return str;
}

//  Return bundle component bit width for an component
int EKAPI_BundleCompWidth(void *pknobs, int index)
{
    return KAPI_GetIntegerVariable(pknobs, "bidcomp_width", index);
}

//  Return special bundle template need add split Issue before bundle
BOOL EKAPI_Split_Before_Bundle(void *pknobs, int index)
{
    char *line, *line_bf, *line_af;
    int bid_id;
    
    Is_True(index<=EKAPI_TemplateCount(pknobs),
    	   ("index %d of bid_t is not existed", index));
    int count = KAPI_count4attribute(pknobs, "IMPLICIT_STOP_BEFORE");
    char *name = EKAPI_TemplateName(pknobs, index);
    
    for(int i=0; i<count; i++)
    {
    	line = KAPI_attribute4index(pknobs, "IMPLICIT_STOP_BEFORE", i);
    	line_bf = StrTrim(strtok(line, ":"));
    	line_af = StrTrim(strtok(NULL, ":"));
    	bid_id = KAPI_EnumIndex(pknobs, "bid_t", line_af);
    	if ((bid_id == index) && (strcmp(line_bf,"bid_t") == 0)) {
    		return true;
    	}
    }
    return false;
}

//  Return special bundle template need add split Issue after bundle
BOOL EKAPI_Split_After_Bundle(void *pknobs, int index)
{
    char *line, *line_bf, *line_af;
    int bid_id;
    
    Is_True(index<=EKAPI_TemplateCount(pknobs),
    	   ("index %d of bid_t is not existed", index));
    int count = KAPI_count4attribute(pknobs, "IMPLICIT_STOP_AFTER");
    char *name = EKAPI_TemplateName(pknobs, index);
    
    for(int i=0; i<count; i++)
    {
    	line = KAPI_attribute4index(pknobs, "IMPLICIT_STOP_AFTER", i);
    	line_bf = StrTrim(strtok(line, ":"));
    	line_af = StrTrim(strtok(NULL, ":"));
    	bid_id = KAPI_EnumIndex(pknobs, "bid_t", line_af);
    	if ((bid_id == index) && (strcmp(line_bf,"bid_t") == 0)) {
    		return true;
    	}
    }
    return false;
}
