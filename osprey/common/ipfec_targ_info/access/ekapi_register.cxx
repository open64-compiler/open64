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
//  Module : ekapi_register.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_register.cxx,v $
//
//  Description:
//  ============
//  hold all register functions in access layer of IPFEC.
//  and encapsulate KAPI first layer opcode of access layer.
//=============================================================================

#include "ekapi_ia64.h"
#include "ekapi_util.h"


// ****************************************
// return register class count 
// *****************************************
int EKAPI_RegClassCount(void *pknobs)
{
     int count=0;
     count = KAPI_EnumCardinality(pknobs, "REGCLASS");
     Is_True(count>=0, ("[REGCLASS] type is not existed"));
     return count;
}

//**************************************************
// Return Register index for name
//**************************************************
int EKAPI_RegClassid4name(void *pknobs, char *name)
{
    return KAPI_EnumIndex(pknobs, "REGCLASS", name);
}
//*****************************************************************
//Get struct of register class properties for register class id 
//*******************************************************************
void EKAPI_RegStruct4id(void *pknobs, EKAPI_REGISTER_CLASS_INFO *reginfo,
                        int rcid)
{
    int  i,j;
    char *str_knob, *str;
    
    str_knob = KAPI_attribute4index(pknobs, "regclassprop", rcid);
    FmtAssert(str_knob != NULL, 
              ("regclassprop attribute line %d is not existed", rcid));
    
    // Get first field REGCLASS name;
    str = StrTrim(strtok(str_knob, ","));
    assert(KAPI_EnumIndex(pknobs, "REGCLASS", str)>=0);
    reginfo->name = str;
    
    // Get second field : isa_mask
    str = StrTrim(strtok(NULL, ","));
    if (strcasecmp(str, "all_isa_mask") == 0 ){
        reginfo->isa_mask = 0x01;
    }
    else {
        reginfo->isa_mask =0;
    }
    
    // Get third field : register number;
    str = StrTrim(strtok(NULL, ","));
    reginfo->min_regnum = 0;
    reginfo->max_regnum = atoi(str)-1; 
    
    // Get fourth field : bit size;
    str = StrTrim(strtok(NULL, ","));
    reginfo->bit_size = atoi(str);

    // Get fifth field : can_store;
    str = StrTrim(strtok(NULL, ","));
    reginfo->can_store = atoi(str);

    // Get sixth field : multiple_save
    str = StrTrim(strtok(NULL, ","));
    reginfo->multiple_save = atoi(str);
    
    // Get seventh field : register name
    str =  StrTrim(strtok(NULL, ","));
    reginfo->name = str;

    // Get eighth field: register rule;
    str = StrTrim(strtok(NULL, ","));
    reginfo->naming_rule = str;

    //Get ninth field : special reigster name
    str = StrTrim(strtok(NULL, ","));
    reginfo->special_array = str;
   
}

//*****************************************************************
// Get register number when given struct EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
int EKAPI_GetRegNum(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->max_regnum+1;
    }
    return 0;
}

//*****************************************************************
// Get register class isa mask when given EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
int EKAPI_GetRegIsaMask(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->isa_mask;
    }
    return 0;
}


//*****************************************************************
// Get register class name when given struct EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
char *EKAPI_GetRegClassName(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->name;
    }
    return NULL;
}

//*****************************************************************
// Get register min number when given struct EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
int EKAPI_GetRegMinNum(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->min_regnum;
    }
    return 0;
}

//*****************************************************************
// Get register max number when given struct EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
int EKAPI_GetRegMaxNum(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->max_regnum;
    }
    return 0;
}

//*****************************************************************
// Get register bit size when given struct EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
int EKAPI_GetRegBitSize(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->bit_size;
    }
    return 0;
}

//*****************************************************************
// Get register can_store is true or not 
// when given struct EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
int EKAPI_GetRegCanStore(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->can_store;
    }
    return 0;
}

//*****************************************************************
// Get register multiple_save is true or not 
// when given struct EKAPI_REGISTER_CLASS_INFO.
//*******************************************************************
int EKAPI_GetRegMultiSave(EKAPI_REGISTER_CLASS_INFO *reginfo)
{
    if (reginfo){
        return reginfo->multiple_save;
    }
    return 0;
}

//*****************************************************************
// Get register name when given struct and register index
//******************************************************************
char *EKAPI_GetRegName(void *pknobs, EKAPI_REGISTER_CLASS_INFO *reginfo,
                       int rid)
{
    int index;
    char *str;
    char *name;
    static char regname[100];
    
    if (reginfo){
        str = itos(rid);
        Is_True(str, ("Register index is valid!"));
        
        // <0 means this register name is abide by naming rule.
        if ((index = KAPI_ArrayIndex(pknobs, reginfo->special_array, str)) <0){
            sprintf(regname, reginfo->naming_rule, rid);
            name = regname;
        }
        else { //use the special array
            name = KAPI_GetStringVariable(pknobs,
                                          reginfo->special_array, index);
        }
        free(str);
    }
     
    return name;
}

//**************************************************
// Return Register SubClass count
//**************************************************
int EKAPI_RegSubclassCount(void *pknobs)
{
    int count;
    count = KAPI_EnumCardinality(pknobs, "REGSUBCLASS");
    Is_True(count>=0, ("[REGSUBCLASS] type of KNOBS FILE is not existed!"));
    return count;
}


//**************************************************
// Return Register SubClass name
//**************************************************
char *EKAPI_RegSubclassName(void *pknobs, int rsubid)
{
    char *name;
    name = KAPI_EnumName(pknobs, rsubid, "REGSUBCLASS");
    Is_True(name, ("[REGSUBCLASS] type or specified index of KNOBS is not existed!"));
    return name;
}

//**************************************************
// Return Register SubClass index for name
//**************************************************
int EKAPI_RegSubclassid4name(void *pknobs, char *name)
{
    return KAPI_EnumIndex(pknobs, "REGSUBCLASS", name);
}

//***************************************************************
//  This function put subclass memeber id in memeber[] by 
//  specified subclass id.And return number of memeber. 
//  regclass keeps the corresponding register class id.
//****************************************************************
int EKAPI_RegSubclassMember(void *pknobs, int regsubclass, int *regclass,
                            int member[])
{
    char *str_knob, *str;
    int i = 0;
    int cond;
    
    Is_True( regsubclass < KAPI_EnumCardinality(pknobs, "REGSUBCLASS"),
               ("regsubclass number beyond bundary!"));
               
    str_knob = KAPI_attribute4index(pknobs, "regsubclassprop", regsubclass);
    FmtAssert(str_knob != NULL,
              ("KNOBS FILE:line %d of regsubclassprop attribute be NULL",
              regsubclass));
    
    str = StrTrim(strtok(str_knob, ","));
    cond = strcmp(str, KAPI_EnumName(pknobs, regsubclass, "REGSUBCLASS")) == 0;
    Is_True( cond, 
             ("KNOB FILE- regsubclass properties order not same as enum order!")
            );
                           
    str = StrTrim(strtok(NULL, ","));
    *regclass = EKAPI_RegClassid4name(pknobs,str);
    Is_True( *regclass < KAPI_EnumCardinality(pknobs, "REGCLASS"), 
             ("Regclass id %d in regsubclassprop beyond bundary!", *regclass)
            );

    while (str = StrTrim(strtok(NULL, ","))) /*Get member id*/
    {
        if (atoi(str) == -1) break;
        member[i] = atoi(str);
        i++;
    }
    free(str_knob);
    return i;
}

//**********************************************************
// Return ABI count
//**********************************************************
int EKAPI_ABICount(void *pknobs)
{
    int count;
    count = KAPI_EnumCardinality(pknobs, "ABI");
    Is_True(count>=0, ("[ABI] enum type is not exited"));
    return count;
}

//**********************************************************
// Return ABI Name
//**********************************************************
char *EKAPI_ABIName(void *pknobs, int id)
{
    return KAPI_EnumName(pknobs, id, "ABI");
}

//**********************************************************
// Return ABI Properties count
//**********************************************************
int EKAPI_ABIPropCount(void *pknobs)
{
    int count;
    count = KAPI_EnumCardinality(pknobs, "ABIPROP");
    Is_True(count>=0, ("[ABI] enum type is not exited"));
    return count;
}

//**********************************************************
// Return ABI Properties Name
//**********************************************************
char *EKAPI_ABIPropName(void *pknobs, int id)
{
    return KAPI_EnumName(pknobs, id, "ABIPROP");
}

//*******************************************************************
//
//  This function can get register ABI properties, and composed mask 
//  by bit order, And return the number of register in fact.
//
//*******************************************************************
int EKAPI_RegPropMask(void *pknobs, int regclass, bv32_t flags[])
{
    int i;
    int propcount, propid, regnum;
    char *str_knob, *str, *rc_name;
    char errmsg[512];
    EKAPI_REGISTER_CLASS_INFO reginfo;
    
    str_knob = NULL;
    str      = NULL;
    rc_name  = NULL;
    
    propcount = KAPI_count4attribute(pknobs, "regprop");
    rc_name   = KAPI_EnumName(pknobs, regclass, "REGCLASS");
    FmtAssert(rc_name != NULL,
        ("KNOBS FILES: index %d of REGCLASS is not exited", regclass));
    
    // Get register struct information
    EKAPI_RegStruct4id(pknobs, &reginfo, regclass);
    regnum    = EKAPI_GetRegNum(&reginfo);
    memset(flags, 0, regnum * sizeof(bv32_t));

    // find regprop attribute relate with indicated regclass
    for(i=0; i<propcount; i++)
    {
        str_knob = KAPI_attribute4index(pknobs, "regprop", i);
        FmtAssert(str_knob != NULL, 
            ("KNOBS FILES: line %d of regprop attribute is not exited", i));
        
        str = StrTrim(strtok(str_knob, ","));
        // If true , choose attribute of correct register class name;
        //    else , skip;
        if (strcmp(str, rc_name) == 0) {  
        
            str    = StrTrim(strtok(NULL, ","));
            // Following is get properties order 
            propid = KAPI_EnumIndex(pknobs, "ABIPROP", str); 
            sprintf(errmsg, "prop %s not exist!", str);
            Is_True( propid >= 0, 
                     (errmsg)
                     );
            while (str = StrTrim(strtok(NULL, ","))) /*Get flags*/
            {
		int idx;
		idx = atoi(str);
                if (idx == -1) break;
                Is_True(idx >= 0 && idx < regnum,
                          ("register number violation boundary!"));
                Is_True(propid!=0, ("entry_ptr should not be used!\n"));
                flags[idx] |= 1ULL << (propid - 1);
            }
                    
        }
        free(str_knob);
    }
    //    free(rc_name);
    return regnum;
}

//***************************************************************
//  Return register class id and register index of this regiter 
//  class when given a register name
//
BOOL EKAPI_RegInfo4Name(void *pknobs, char *regname, int *regclassid, int *regid)
{
    int rc_count, reg_count;
    int rc_i;
    EKAPI_REGISTER_CLASS_INFO reginfo;
    
    rc_count = EKAPI_RegClassCount(pknobs);
    
    // Tranverse all the register name to find the same name register;
    // If find register then break ,else return 0;
    for(rc_i=0; rc_i< rc_count; rc_i++)
    {
        EKAPI_RegStruct4id(pknobs, &reginfo, rc_i);
        reg_count = EKAPI_GetRegNum(&reginfo);
        
        for(int r_i=0; r_i<reg_count; r_i++)
        {
            char *pname = EKAPI_GetRegName(pknobs, &reginfo, r_i);
            if (strcmp(pname, regname) == 0) {
                *regclassid = rc_i;
                *regid = r_i;
                return 1; /* find name */
            }            
        }
        
    }
    *regclassid = -1;
    *regid = -1;
    return 0; /* dont find name*/
}
