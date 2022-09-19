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
//  Module : ekapi_lits.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_lits.cxx,v $
//
//  Description:
//  ============
//  hold all Literal CLASS functions in access layer of IPFEC.
//  and encapsulate KAPI first layer opcode of access layer.
//  Literal Class means immediate number of operands
//=============================================================================

#include "ekapi_ia64.h"
#include "ekapi_util.h"

// Return Literal Class count;
int EKAPI_LitClassCount(void *pknobs)
{
    return KAPI_count4attribute(pknobs, "literal");
}

// Return Literal Class name for each id;
char *EKAPI_LitClassName(void *pknobs, int lcid)
{
    char *str_knob, *str;
    char *name;
    str_knob = KAPI_attribute4index(pknobs, "literal", lcid);
    FmtAssert(str_knob, ("Literal Class string index %d is not exited!", lcid));
    str = StrTrim(strtok(str_knob, ","));
    name = strdup(str);
    
    free(str_knob);
    return name;    
}

// Return index when given literal class name;
int EKAPI_LitClassid4name(void *pknobs, char *name)
{
    int i;
    char *tempname;
    BOOL find=0;
    
    // compare name field recursively to get index
    for (i=0; i<EKAPI_LitClassCount(pknobs); i++)
    {
        tempname = EKAPI_LitClassName(pknobs, i);
        if (strcmp(name, tempname)==0) {
            find = 1;
            break;
        }
        free(tempname);        
    }
    
    if (find) { // find is true
        return i;
    }
    else {  // find is false
        return -1;
    }
}

// Return sign for this literal class
BOOL EKAPI_LitIsSigned(void *pknobs, int lcid)
{
    char *str_knob, *str;
    BOOL sign = -1;
    str_knob = KAPI_attribute4index(pknobs, "literal", lcid);
    FmtAssert(str_knob, ("Literal Class string index %d is not exited!", lcid));
    str = strtok(str_knob, ",");
    str = StrTrim(strtok(NULL, ","));
    
    if (strcmp(str, "SIGNED")==0) sign = 1;
    if (strcmp(str, "UNSIGNED") == 0) sign =0; 
     Is_True((strcmp(str, "SIGNED")==0) || (strcmp(str, "UNSIGNED") == 0),
            ("Literal Sign in line %d sytax Wrong!\n", lcid)
           );
           
    free(str_knob);
    return sign;    
}

// Function EKAPI_GetLcRange()
//    lcid : literal class index;
//    ranges: a struct array to keep multiple range;And the order of range 
//              is ascending;If you want to get the minimum value , you can
//              just use ranges[0].min; else ranges[count-1].max for maximum;
// AIM:
//    Get Literal Class Range in struct ranges[]
//    Result may be many.return number of ranges
//
int EKAPI_GetLcRange(void *pknobs, int lcid, EKAPI_RANGE ranges[])
{
    char *str_knob, *str;
    BOOL sign = 0;
    int  iknob, j;
    INT64 min, max;
    
    str_knob = KAPI_attribute4index(pknobs, "literal", lcid);
    FmtAssert(str_knob, ("Literal Class string is not exited!"));
    str = strtok(str_knob, ",");
    str = StrTrim(strtok(NULL, ","));
    
    if (strcmp(str, "SIGNED")==0) sign = 1;
    if (strcmp(str, "UNSIGNED") == 0) sign =0; 
    Is_True((strcmp(str, "SIGNED")==0) || (strcmp(str, "UNSIGNED") == 0),
            ("Literal Sign in Line %d sytax Wrong!", lcid)
           );

    j = 0;
    while (str = StrTrim(strtok(NULL, ",")))
    {
         min = 0; max = 0;
         switch (str[0])
         {
             case '*': //bit range
                       iknob = atoi(str+1);
                       if (sign) {
                           min = (1LL << (iknob-1)) * (-1);
                           max = (1LL << (iknob-1)) -1 ;
                       }
                       else {
                           min = 0;
                           max = (1ULL << iknob) -1;
                       }
                       break;
             case '@': //just a number
                       iknob = atol(str+1);
                       min = iknob;
                       max = iknob;
                       break;
             case '?': //range
                       iknob = strlen(str) - strlen(strstr(str, ":")) + 1;
                       assert(iknob < strlen(str));
                       max = (INT64)strtod(str+iknob, NULL); //Hex
                       str[iknob-1] = '\0'; // get the first field before ':'
                       min = (INT64)strtod(str+1, NULL);
                       
                       break;
             default: 
                       free(str_knob);
                       Is_True( 0, ("Literal Class: Unknown range!"));
                       break; 
        }
        ranges[j].min = min;
        ranges[j].max = max;
        j++;    
    }
    free(str_knob);
    return j;
}

