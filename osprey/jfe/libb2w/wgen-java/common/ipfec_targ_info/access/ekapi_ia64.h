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

//=============================================================================
//
//  Module : ekapi_ia64.h
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_ia64.h,v $
//
//  Description:
//  ============
//  hold all functions in access layer of IPFEC.
//  It is used for accessing extra knob file and encapsulate
//  the first layer of kapi.
//=============================================================================

//  ekapi_ia64.h
//
//  Access layer of IPFEC MD. similar with second layer of KAPI
/////////////////////////////////////////////////////////////////
//
//  pknobs: A pointer to the variable initialized by KAPI_initialize().
//
//  Char *EKAPI_OpName4id( void *pknobs, int index)
//  Get opcode name by specified ID from attribute "opcode"
//
//  char *EKAPI_Op2Fu( void *pknobs, int index);
//  Get opcode 's function class name by specified ID. The 
//  principle is like the above function.
//
//  Int EKAPI_OpCount(void *pknobs);
//  return opcode attribute count.
//
//  
//  int EKAPI_SubsetCount(void *pknobs);
//  Return subset type Count;
//
//  char *EKAPI_SubsetName4id(void *pknobs, int index);
//  Give Subset Name for each index.
//
//  Todo .... add new comment as following
//
#ifndef EKAPI_IA64_INCLUDED
#define EKAPI_IA64_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#include "kapi.h"
#include "kapi_ia64.h"

typedef signed long long INT64;
typedef unsigned long long UINT64;
typedef int		BOOL; /* Natural size Boolean value */
typedef unsigned char mUINT8; /* Use the natural integer */


// Struct definition
typedef struct{

    int isa_mask;       // the set mask, which is useful ,which is reserved, ignore¡­
    int min_regnum;     // the lowest register number in this set
    int max_regnum;     // the highest register number in this set
    int bit_size;       // specifies the size, in bits, of the registers in the class
    int can_store;      // flags if there have load and store instructions for registers of this class
    int multiple_save;  // possible to save/restore multiple registers of this class at once
    char *name;	        // register class name, for debugging and document
    char *naming_rule;  // give rule of name register 
    char *special_array;// give name of the array which holds special register

} EKAPI_REGISTER_CLASS_INFO; //class Information

typedef struct{
    INT64 min;  
    INT64 max;
}EKAPI_RANGE; // used in literal class for keeping range of immediate;

typedef struct {
    int *source;
    int *opnduse;
    int *result;
    int num_source;
    int num_result;
}EKAPI_OPNDGRP_INFO;// struct of Operands group Inforamtion;

typedef struct {
    char *opndname;
    int oddlatency;
}ODD_LATENCY;

#define Is_True_On 1

// Unconditional assertion checking with printf format, always fatal
#define FmtAssert(Cond, ParmList ) \
    ( Cond ? (void) 1 \
           : ( printf ParmList, \
               exit(0) ) )

#ifdef Is_True_On
#define Is_True FmtAssert
#else
#define Is_True(a, b) ((void) 1)
#endif


     
// Return the count of all the opcodes
extern int EKAPI_OpCount(void *pknobs);

// Return an op's name according to the code index
extern char * EKAPI_OpName4id(void *pknobs, int index);

// Return a function class name of an op
extern char *EKAPI_Op2Fu( void *pknobs, int index);

// Return an op's function class identical
extern int EKAPI_Op2FuIndex(void *pknobs, int index);

//******   Following: subset function   ******//

// Return subset of an op
extern char *EKAPI_Op2SubSet( void *pknobs, int index);

// Return subset type Count;
extern int EKAPI_SubsetCount(void *pknobs);

// Return subset name for subset index
extern char *EKAPI_SubsetName4id(void *pknobs, int index);


// ******  Following: register function ******//

// Return  register class count 
extern int EKAPI_RegClassCount(void *pknobs);

// Return Register index for name
extern int EKAPI_RegClassid4name(void *pknobs, char *name);

// Return structure of register class properties for register class id 
extern void EKAPI_RegStruct4id(void *pknobs, EKAPI_REGISTER_CLASS_INFO *reginfo, int rcid);

// Use EKAPI_RegStruct4id() to get struct reginfo to get Register Number
extern int EKAPI_GetRegNum(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Get register class isa mask when given EKAPI_REGISTER_CLASS_INFO.
  extern int EKAPI_GetRegIsaMask(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Get register class name when given struct EKAPI_REGISTER_CLASS_INFO.
extern char *EKAPI_GetRegClassName(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Get register min number when given struct EKAPI_REGISTER_CLASS_INFO.
extern int EKAPI_GetRegMinNum(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Get register max number when given struct EKAPI_REGISTER_CLASS_INFO.
extern int EKAPI_GetRegMaxNum(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Get register bit size when given struct EKAPI_REGISTER_CLASS_INFO.
extern int EKAPI_GetRegBitSize(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Get register can_store is true or not when given struct EKAPI_REGISTER_CLASS_INFO.
extern int EKAPI_GetRegCanStore(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Return register multiple_save is true or not 
extern int EKAPI_GetRegMultiSave(EKAPI_REGISTER_CLASS_INFO *reginfo);

// Get register name when given pknobs , struct and register index
extern char *EKAPI_GetRegName(void *pknobs, EKAPI_REGISTER_CLASS_INFO *reginfo, int rid);

// Return Register SubClass count
extern int EKAPI_RegSubclassCount(void *pknobs);

// Return Register SubClass name
extern char *EKAPI_RegSubclassName(void *pknobs, int rsubid);

// Return Register SubClass index for name
extern int EKAPI_RegSubclassid4name(void *pknobs, char *name);

// Get register Subclass member; Variable regclass holds which register 
// class  this regsubclass belongs to; And return member number
extern int EKAPI_RegSubclassMember(void *pknobs, int regsubclass,
                                   int *regclass, int member[]);

// Return ABI count
int EKAPI_ABICount(void *pknobs);

// Return ABI Name
char *EKAPI_ABIName(void *pknobs, int id);

// Return ABI Properties count
int EKAPI_ABIPropCount(void *pknobs);

// Return ABI Properties Name
char *EKAPI_ABIPropName(void *pknobs, int id);

// Get register ABI properties, return number of regclass
extern int EKAPI_RegPropMask(void *pknobs, int regclass, bv32_t flags[]);

/*******    BUNDLE INFORMARION    *******/

// Return maximum slot number
extern int EKAPI_GetMaxSlot(void *pknobs);

// Return Bundle width
extern int EKAPI_GetBundleWidth(void *pknobs);

// Return count of execute unit type
extern int EKAPI_ExecUnitCount(void *pknobs);

// Return Bundle template count, now is 16;
extern int EKAPI_TemplateCount(void *pknobs);

// Return the name of the bundle
extern char *EKAPI_TemplateName(void *pknobs, int index);

// Return the asm name of the bundle
extern char *EKAPI_TemplateAsmName(void *pknobs, int index);

//  Return name of execute unit type
extern char *EKAPI_ExecUnitName(void *pknobs, int index);

//  Return id when given execute unit name 
extern int EKAPI_EunName2id(void *pknobs, char *name);

//  Return execute unit properties for each op
extern bv32_t EKAPI_EunMask4op(void *pknbs, int opid);

// Return slot type in execute type in extra knob file for bundle id
extern void EKAPI_BundleType4bid(void *pknobs, kapi_bid_t bid, int *slt);

//  Comvert syl type to execute unit type ,
int Syl2Eun(void *pknobs, int sylid);

//  Return bundle component count
extern int EKAPI_BundleCompCount(void *pknobs);

//  Return bundle componet name
extern char *EKAPI_BundleCompName(void *pknobs, int index);

//  Return bundle component width for an component
extern int EKAPI_BundleCompWidth(void *pknobs, int index);

//  Return special bundle template need add split Issue before bundle
extern BOOL EKAPI_Split_Before_Bundle(void *pknobs, int index);

//  Return special bundle template need add split Issue after bundle
extern BOOL EKAPI_Split_After_Bundle(void *pknobs, int index);


/********  ENUM CLASS INFORMATION  ***********/

// Return Enum Class Count.
extern int EKAPI_EnumClassCount(void *pknob);

// Return Enum Class name for each enum class index
extern char *EKAPI_EnumClassName(void *pknobs, int index);

// Return Enum Class id for given Enum Class name
int EKAPI_EnumClassid4name(void *pknobs, char *name);

// Return Enum Value Class Count
extern int EKAPI_EvClassCount(void *pknob);

// Return Enum Value Class name when given enum value Class index
extern char *EKAPI_EvClassName(void *pknobs, int index);

// Return Enum Vlaue Class asm name for document
char *EKAPI_EvClassAsmName(void *pknobs, int index);

// Return Enum Value Class Int Value when given enum value Class index
extern int EKAPI_EvClassValue(void *pknobs, int index);

// Return Enum Class' first Enum value Class id when given enum class index
extern int EKAPI_EcFirstValue(void *pknob, int ecid);

// Return Enum Class' last Enum value Class id when given enum class index
extern int EKAPI_EcLastValue(void *pknobs, int ecid);

/**********   Literal Class Functions  ******************/

// Return Literal Class count;
extern int EKAPI_LitClassCount(void *pknobs);

// Return Literal Class name for each id;
extern char *EKAPI_LitClassName(void *pknobs, int lcid);

// Return index when given literal class name;
extern int EKAPI_LitClassid4name(void *pknobs, char *name);

// Return sign bit for this literal class
extern BOOL EKAPI_LitIsSigned(void *pknobs, int lcid);

// Get Literal Class Range in struct ranges[]
// Result may be many. Return number of ranges
extern int EKAPI_GetLcRange(void *pknobs, int lcid, EKAPI_RANGE ranges[]);

/***********    operands Functions ******************/

//  This function return operands group id
//  when givem opcode index;
extern int EKAPI_Op2Opndsgrp(void *pknobs, int index);

//  Return number of operand groups
extern int EKAPI_OpndGrpCount(void *pknobs);

//  Return Operand group information in struct
//     EKAPI_OPNDGRP_INFO *info must initial three pointer(malloc) in first use
extern void EKAPI_GetOperandInfo4Grp(void *pknobs, int grpindex,EKAPI_OPNDGRP_INFO *info);

// Return maximum of source operands
extern int EKAPI_GetSrcOpndsMax(void *pknobs);

// Return maximum of Destinate operands;
extern int EKAPI_GetDestOpndsMax(void *pknobs);

//  return src operand type id
extern int *EKAPI_Src4Opndgrp(EKAPI_OPNDGRP_INFO info);

// return the number of source operands
extern int EKAPI_SrcOpndCount(EKAPI_OPNDGRP_INFO info );

// return the number of result operands
extern int EKAPI_ResultOpndCount(EKAPI_OPNDGRP_INFO info );

//  return result operand type id 
extern int *EKAPI_Result4Opndgrp(EKAPI_OPNDGRP_INFO info );

//  return src operand use id
extern int *EKAPI_Use4Opndgrp(EKAPI_OPNDGRP_INFO info);

//  Return count of operand type
extern int EKAPI_OpndTypeCount(void *pknobs);

//  Return operand type name for an operand type id
extern char *EKAPI_OpndTypeName(void *pknobs, int index);

//  Return operand usage Count
extern int EKAPI_OpndUseCount(void *pknobs);

//  Return operand usage name
extern char *EKAPI_OpndUseName(void *pknobs, int index);

//  Return register class id of operand type;
extern int EKAPI_RegClass4otid(void *pknobs, int otid);

//  Return register subclass id for an operand type;
extern int EKAPI_RegSubclass4otid(void *pknobs, int otid);

//  Return  Literal Class id for an operand type;
extern int EKAPI_LitClass4otid(void *pknobs, int otid);

//  Return Enum Class id for an operand type;
extern int EKAPI_EnumClass4otid(void *pknobs, int otid);

//  Return size for an operand type;
extern int EKAPI_Size4otid(void *pknobs, int otid);

//  Return flag to indicate some features for an operand type;
extern int EKAPI_Flag4otid(void *pknobs, int otid);

// Return relocatable operand index for an op, If none , return -1
extern int EKAPI_GetRelocatableOpnd(void *pknobs, int opid);

// Return regclassid and reg index when given name;
// If find return 1; else is 0;
BOOL EKAPI_RegInfo4Name(void *pknobs, char *regname, int *regclassid, int *regid);

/*********************   properties function *******************/

//  Return operate code properties count
extern int EKAPI_OppCount(void *pknobs);

//  Return operate code properties count which is never owned by any op
extern int EKAPI_FalseOppCount(void *pknobs);

//  Return op properties Name for an opp id
extern char *EKAPI_Oppid2Name(void *pknobs, int index);

//  Return op flase properties Name for an opp id
extern char *EKAPI_FalseOppid2Name(void *pknobs, int index);

//  Return flag composed by op properties for an op
extern UINT64 EKAPI_OppMask4op(void *pknobs, int opid);



/**********  process and process properties function **********/
inline char *EKAPI_ProcessName(void *pknobs)
{
    return KAPI_GetStringVariable(pknobs, "processor_name", -1);
}
   

/*********** itanium function **********************/
extern void EKAPI_MapResource(void *pknobs, char *name, kapi_cluster_t cluster, ...);

// Because some resource are leakage in KAPI. but we plan to 
// consider them as resource, so we will use creat resource;
extern void EKAPI_CreatResource(char *name, int count, int is_issue = 0);

extern void EKAPI_ClearResource(void);

/*************  bypass functions  **********************/
// Return odd latency list of src operand and
// Place number of list into listnum;
extern ODD_LATENCY *EKAPI_OddLatencyList(void *pknobs, kapi_fu_t fuid, int *listnum);

/*************  bypass functions  **********************/
// Return Cache name count.
extern int EKAPI_CacheNameCount(void *pknobs);
// Return Cache name
extern char *EKAPI_CacheName(void *pknobs, int index);

#ifdef __cplusplus
}
#endif
#endif
