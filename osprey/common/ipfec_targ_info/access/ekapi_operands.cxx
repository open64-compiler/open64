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
//  Module : ekapi_operands.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_operands.cxx,v $
//
//  Description:
//  ============
//  hold all operands functions in access layer of IPFEC.
//  and encapsulate KAPI first layer opcode of access layer.
//=============================================================================

#include "ekapi_ia64.h"
#include "ekapi_util.h"


//  **************************************
//  This function return operands group id
//  when givem opcode index;
//  **************************************
int EKAPI_Op2Opndsgrp(void *pknobs, int index)
{ 
    char  *str_knob, *str;
    int  ID,grpid = -1;
   
    str_knob = KAPI_attribute4index( pknobs, "opcode", index);
    FmtAssert(str_knob, ("KNOBS Files opcode's operands group no %d line!", index));
    
    // get the fifth field from whole attribute string str_knob.
    // so skip four fields.
    ID      = atoi(strtok( str_knob, "," ));
    assert(ID == index);

    strtok( NULL, "," );
    strtok( NULL, "," );
    strtok( NULL, "," );

    // get the operand group id in fifth field
    str     = StrTrim(strtok( NULL, "," ));
    grpid   = atoi(str);

    free(str_knob);
    Is_True(grpid < KAPI_count4attribute(pknobs, "opndsgrp"),
              ("operands group id %d beyond bundary", grpid)
              );
    return grpid;
}

//  ********************************************
//  Return number of operand groups
int EKAPI_OpndGrpCount(void *pknobs)
{
    return KAPI_count4attribute(pknobs, "opndsgrp");
}

//  ********************************************
//  Return Operand group information in struct
//     EKAPI_OPNDGRP_INFO *info must initial three pointer(malloc) in first use
//  That include :
//  1)number of result operand; 2) number of source operand
//  3)source operand type identical; 
//  4)result operand type id;
//  ********************************************
void EKAPI_GetOperandInfo4Grp(void *pknobs, int grpindex,EKAPI_OPNDGRP_INFO *info)
{
    char *str_knob, *str;
    char str_source[20], str_use[20], str_result[20];
    int ID,iknob, max_src, max_dest;
    int num_source = 0, num_result=0;
    BOOL src_true =0;

    str_knob = KAPI_attribute4index(pknobs, "opndsgrp", grpindex);
    str      = StrTrim(strtok(str_knob, ","));
    ID       = atoi(str);
    Is_True(ID == grpindex, 
            ("KNOBS FILE ERROR:opndsgrp line %d is consist with ID %d", grpindex, ID)
            ); //confirm get the correct data
    
    max_src   = EKAPI_GetSrcOpndsMax(pknobs);
    max_dest  = EKAPI_GetDestOpndsMax(pknobs);
    
   
    while (str = StrTrim(strtok(NULL, ",")))
    {
        strcpy(str_use, ""); //initial string
        strcpy(str_source, "OT_");
        strcpy(str_result, "OT_");
        switch (str[0])
        {
            case '?': // predicate, belong to source operands
                      // because the item is separated by '/'
                      // the fonter is operand type name
                      // the latter is operand usage name
           
            case '-': // this is prefix of source operand expect predicate

                      iknob = strlen(str) - strlen(strstr(str, "/")) + 1;
                      Is_True(iknob < strlen(str), 
                              ("KNOBS FILE syntax Error:find not '/' in source operands %s",
                              str)
                              );
                      strcat(str_use, str+iknob); 
                      str[iknob-1] = '\0'; // get the first field before '/'
                      strcat(str_source, str+1);
                      num_source++;
                      src_true = 1;
                      break;

            case '+': // prefx of result
                      strcat(str_result, str+1);
                      num_result++;
                      src_true = 0;
                      break;
            default:  free(str_knob);
                      FmtAssert(0,
                                ("KNOBS FILE syntax Error: Unknown Charecter of opndsgrp in line %d", grpindex)
                               );
                   
                       
        }
        if (src_true) {
            ID = KAPI_EnumIndex(pknobs, "ot_t", str_source);
            Is_True(ID>=0, 
                    ("KNOBS FILE syntax Error:Unknown operand type id %d", ID)
                    );
            
            // Decide If source number is beyond bundary or not
            FmtAssert(num_source <= max_src, 
                      ("source operands number %d  violate MAX_OPND_SRC %d of knobfiles", num_source, max_src)
                      );
            info->source[num_source-1] = ID;
            info->opnduse[num_source-1]= KAPI_EnumIndex(pknobs, "ou_t", str_use);
        }
        else {
            FmtAssert(num_result <= max_dest, 
                      ("source operands number too many to violate MAX_OPND_DEST of knobfiles")
                      );
            info->result[num_result-1] = KAPI_EnumIndex(pknobs, "ot_t", str_result); 
        }
    }
    info->num_source = num_source;
    info->num_result = num_result;
    free(str_knob);    
}

// Return maximum of source operands
int EKAPI_GetSrcOpndsMax(void *pknobs)
{
    return KAPI_GetIntegerVariable(pknobs, "MAX_OPND_SRC", -1);
}

// Return maximum of Destinate operands;
int EKAPI_GetDestOpndsMax(void *pknobs)
{
    return KAPI_GetIntegerVariable(pknobs, "MAX_OPND_DEST", -1);
}

//  return src operand type id
int *EKAPI_Src4Opndgrp(EKAPI_OPNDGRP_INFO info)
{
    return info.source;
}

//  return src operand use id
int *EKAPI_Use4Opndgrp(EKAPI_OPNDGRP_INFO info)
{
    return info.opnduse;
}

// return count of source operand
int EKAPI_SrcOpndCount(EKAPI_OPNDGRP_INFO info )
{
    return info.num_source;
}

// return count of result operand
int EKAPI_ResultOpndCount(EKAPI_OPNDGRP_INFO info )
{
    return info.num_result;
}

//  return result operand type id 
int *EKAPI_Result4Opndgrp(EKAPI_OPNDGRP_INFO info )
{
    return info.result;
}

//  Return count of operand type
int EKAPI_OpndTypeCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "ot_t");
}

//  Return operand type name for an operand type id
char *EKAPI_OpndTypeName(void *pknobs, int index)
{
    return KAPI_EnumName(pknobs, index, "ot_t");
}

//  Return operand usage Count
int EKAPI_OpndUseCount(void *pknobs)
{
    return KAPI_EnumCardinality(pknobs, "ou_t");
}

//  Return operand usage name
char *EKAPI_OpndUseName(void *pknobs, int index)
{
    return KAPI_EnumName(pknobs, index, "ou_t");
}


//  Function : EKAPI_RegClass4otid()
//    pknobs: pointer initialized by KAPI_init();
//    otid:   operand type id;
//  Return register class id of operand type;
int EKAPI_RegClass4otid(void *pknobs, int otid)
{
    char *str_knob,*str;
    int count, rcid=-1;
    count = EKAPI_OpndTypeCount(pknobs);
    Is_True(otid<count, 
            ("KNOBS FILE ERROR:operand type id required beyond count of Operand Type ")
            );
    
    str_knob = KAPI_GetStringVariable(pknobs, "opndtype", otid);
    // Get field separated by ","
    str = StrTrim(strtok(str_knob, ","));
    if (strcmp(str, "UNDEFINED") != 0) {
        rcid = EKAPI_RegClassid4name(pknobs, str);
        free(str_knob);
        Is_True(rcid>=0, ("KNOBS FILE: Unknowned string in regclass field of opndtype;"));
    }
    else {
        free(str_knob);
        rcid = -1;
    }
    
    return rcid;    
}

//  Function :EKAPI_RegSubclass4otid()
//    pknobs: pointer initialized by KAPI_init();
//    otid:   operand type id;
//  Return register subclass id for an operand type;
int EKAPI_RegSubclass4otid(void *pknobs, int otid)
{
    char *str_knob,*str;
    int count, rsubcid=-1;
    
    count = EKAPI_OpndTypeCount(pknobs);
    Is_True(otid<count, ("operand type id required beyond count of Operand Type "));
    
    str_knob = KAPI_GetStringVariable(pknobs, "opndtype", otid);
    
    // Get field separated by ","
    strtok(str_knob, ",");
    str = StrTrim(strtok(NULL, ","));
    
    if (strcmp(str, "UNDEFINED") != 0) {
        rsubcid = EKAPI_RegSubclassid4name(pknobs, str);
        free(str_knob);
        Is_True(rsubcid>=0, ("KNOBS FILE: Unknowned string in regsubclass field of opndtype;"));
    }
    else {
        free(str_knob);
        rsubcid = -1;
    }
    return rsubcid;    
}

//  Function :EKAPI_LitClass4otid()
//    pknobs: pointer initialized by KAPI_init();
//    otid:   operand type id;
//  Return  Literal Class id for an operand type;
int EKAPI_LitClass4otid(void *pknobs, int otid)
{
    char *str_knob,*str;
    int count, lcid=-1;
    
    count = EKAPI_OpndTypeCount(pknobs);
    Is_True(otid<count, ("operand type id required beyond count of Operand Type "));
    
    str_knob = KAPI_GetStringVariable(pknobs, "opndtype", otid);
    
    // Get field separated by ","
    strtok(str_knob, ",");
    str = strtok(NULL, ",");
    str = StrTrim(strtok(NULL, ","));
    
    if (strcmp(str, "UNDEFINED") != 0) {
        lcid = EKAPI_LitClassid4name(pknobs, str);
        free(str_knob);
        Is_True(lcid>=0, ("KNOBS FILE: Unknowned string in lclass field of opndtype;"));
    }
    else {
        free(str_knob);
        lcid = -1;
    }
    return lcid;    
}

//  Function :EKAPI_EnumClass4otid()
//    pknobs: pointer initialized by KAPI_init();
//    otid:   operand type id;
//  Return Enum Class id for an operand type;
int EKAPI_EnumClass4otid(void *pknobs, int otid)
{
    char *str_knob,*str;
    int count, ecid=-1;
    
    count = EKAPI_OpndTypeCount(pknobs);
    Is_True(otid<count, ("operand type id required beyond count of Operand Type "));
    
    str_knob = KAPI_GetStringVariable(pknobs, "opndtype", otid);
    
    // Get field separated by ","
    strtok(str_knob, ",");
    strtok(NULL, ",");
    strtok(NULL, ",");
    str = StrTrim(strtok(NULL, ","));
    
    if (strcmp(str, "UNDEFINED") != 0) {
        ecid = EKAPI_EnumClassid4name(pknobs, str);
        free(str_knob);
        Is_True(ecid>=0, ("KNOBS FILE: Unknowned string in eclass field of opndtype;"));
    }
    else {
        free(str_knob);
        ecid = -1;
    }
    return ecid;    
}

//  Function :EKAPI_Size4otid()
//    pknobs: pointer initialized by KAPI_init();
//    otid:   operand type id;
//  Return size for an operand type;
int EKAPI_Size4otid(void *pknobs, int otid)
{
    char *str_knob,*str;
    int count, size=0;
    
    count = EKAPI_OpndTypeCount(pknobs);
    Is_True(otid<count, ("operand type id required beyond count of Operand Type "));
    
    str_knob = KAPI_GetStringVariable(pknobs, "opndtype", otid);
    
    // Get field separated by ","
    strtok(str_knob, ",");
    strtok(NULL, ",");
    strtok(NULL, ",");
    strtok(NULL, ",");
    str = StrTrim(strtok(NULL, ","));
    
    size = atoi(str);
    free(str_knob);
    return size;    
}


//  Function :EKAPI_Flag4otid()
//    pknobs: pointer initialized by KAPI_init();
//    otid:   operand type id;
//  Return flag indicate certain feature for an operand type;
//  flag:  010101 
//              ^first bit, that is count from right to left
//  the first bit means operand is register tyoe or not 
//  the second bit means this operand type Is signed or not;
//  the third bit means this operand type is FPU INT or not;
//  the forth bit means this operand type is PCREL or not;
int EKAPI_Flag4otid(void *pknobs, int otid)
{
    char *str_knob,*str=NULL;
    int count=0, flag=0, span;
    
    // Determined is register or not
    if ( (EKAPI_RegClass4otid(pknobs, otid)>=0) || 
         (EKAPI_RegSubclass4otid(pknobs, otid)>=0) ) {
          flag = 1;
    }
     
    
    count = EKAPI_OpndTypeCount(pknobs);
    Is_True(otid<count, ("operand type id required beyond count of Operand Type "));
    
    str_knob = KAPI_GetStringVariable(pknobs, "opndtype", otid);
    
    // Get field separated by ","
    strtok(str_knob, ",");
    strtok(NULL, ",");
    strtok(NULL, ",");
    strtok(NULL, ",");
    strtok(NULL, ",");
    
    span = 1;
    while( str = StrTrim(strtok(NULL, ",")))
    {
        // determined signed, FPU INT, PCREL is true or not;
        if (strcasecmp(str,"true")==0) {
            flag |= 1 << span;
        }
        span++;
        if ( (strcasecmp(str, "false")!=0) && (strcasecmp(str, "true")!=0) ) {
            free(str_knob);
            Is_True(0, ("KNOBS FILE: UNKNOWN String in flag component of operand type"));
        }
        
    }
    
    free(str_knob);
    return flag;    
}

// Return relocatable operand index for an op, If none , return -1
int EKAPI_GetRelocatableOpnd(void *pknobs, int opid)
{
    char op[100]="%";
    int index;
    char *name = EKAPI_OpName4id(pknobs, opid);
    strcat(op, name);
    free(name);
    
    index = KAPI_ArrayIndex(pknobs, "relocatable", op);
   
           
    if (index >= 0) {
        return KAPI_GetIntegerVariable(pknobs, "relocatable", index);
    }
    else {
        return -1;
    }    
}
