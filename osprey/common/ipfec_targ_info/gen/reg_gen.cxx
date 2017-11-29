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

//*********************************************************************
//
// Module: reg_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/reg_gen.cxx,v $
//
// Description:
//   Generate the definition of registers.
//
//*********************************************************************

#include "reg_gen.h"

static const char * const reg_descript[]= {
"/* ====================================================================",
" * ====================================================================",
" *",
" * Description:",
" *",
" *   A description of the ISA registers. The description exports",
" *   the following:",
" *",
" *   typedef (enum) ISA_REGISTER_CLASS",
" *       An enumeration of the register classes.",
" *",
" *   typedef mISA_REGISTER_CLASS",
" *       The most compact (integral) representation that can hold",
" *       all values of ISA_REGISTER_CLASS",
" *",
" *   typedef (struct) ISA_REGISTER_CLASS_INFO",
" *       Describes a particular register class. The contents are private.",
" *",
" *   const INT ISA_REGISTER_CLASS_UNDEFINED",
" *       A special register class that is out-of-range of valid",
" *       register clases.",
" *",
" *   const INT ISA_REGISTER_CLASS_MIN",
" *       The first register class. The range of register classes",
" *       is ISA_REGISTER_CLASS_MIN..ISA_REGISTER_CLASS_MAX",
" *	    (this range excludes ISA_REGISTER_CLASS_UNDEFINED). * ",
" *   const INT ISA_REGISTER_CLASS_MAX",
" *       The last register class. The range of register classes",
" *       is ISA_REGISTER_CLASS_MIN..ISA_REGISTER_CLASS_MAX",
" *	    (this range excludes ISA_REGISTER_CLASS_UNDEFINED). * ",
" *   const INT ISA_REGISTER_CLASS_COUNT",
" *       The number of register classes. The range of register classes",
" *       is ISA_REGISTER_CLASS_MIN..ISA_REGISTER_CLASS_MAX",
" *	    (this range excludes ISA_REGISTER_CLASS_UNDEFINED). * ",
" *   (macro) FOR_ALL_ISA_REGISTER_CLASS(cl)",
" *       Iterate over all the register class values using the",
" *       ISA_REGISTER_CLASS variable <cl>.",
" *",
" *   (macro) FOR_ALL_ISA_REGISTER_CLASS_IN_REVERSE(cl)",
" *       Iterate over all the register class values in reverse order using",
" *       the ISA_REGISTER_CLASS variable <cl>.",
" *",
" *   const INT ISA_REGISTER_MAX",
" *       The maximum (highest) register number of all classes.",
" *       NOTE: the lowest number register is implicitly 0.",
" *",
" *   typedef (enum) ISA_REGISTER_SUBCLASS",
" *       An enumeration of the register subclasses.",
" *",
" *   typedef mISA_REGISTER_SUBCLASS",
" *       The most compact (integral) representation that can hold",
" *       all values of ISA_REGISTER_SUBCLASS",
" *",
" *   typedef (struct) ISA_REGISTER_SUBCLASS_INFO",
" *       Describes a particular register subclass. The contents are private.",
" *",
" *   const INT ISA_REGISTER_SUBCLASS_UNDEFINED",
" *       A special register subclass that is out-of-range of valid",
" *       register subclases.",
" *",
" *   const INT ISA_REGISTER_SUBCLASS_MIN",
" *       The first register subclass. The range of register subclasses",
" *       is ISA_REGISTER_SUBCLASS_MIN..ISA_REGISTER_SUBCLASS_MAX",
" * ",
" *   const INT ISA_REGISTER_SUBCLASS_MAX",
" *       The last register subclass. The range of register subclasses",
" *       is ISA_REGISTER_SUBCLASS_MIN..ISA_REGISTER_SUBCLASS_MAX",
" * ",
" *   const INT ISA_REGISTER_SUBCLASS_COUNT",
" *       The number of register subclasses.",
" * ",
" *   (macro) FOR_ALL_ISA_REGISTER_SUBCLASS(sc)",
" *       Iterate over all the register subclass values using the",
" *       the ISA_REGISTER_SUBCLASS variable <sc>.",
" *",
" *   const ISA_REGISTER_CLASS_INFO *ISA_REGISTER_CLASS_Info(",
" *     ISA_REGISTER_CLASS rc",
" *   )",
" *       Return a pointer to the register class info for class 'rc'.",
" *",
" *   INT ISA_REGISTER_CLASS_INFO_First_Reg(",
" *     const ISA_REGISTER_CLASS_INFO *info",
" *   )",
" *       Get the first (lowest numbered) register for the class",
" *       described by 'info'.",
" *",
" *   INT ISA_REGISTER_CLASS_INFO_Last_Reg(",
" *     const ISA_REGISTER_CLASS_INFO *info",
" *   )",
" *       Get the last (highest numbered) register for the class",
" *       described by 'info'.",
" *",
" *   INT ISA_REGISTER_CLASS_INFO_Bit_Size(",
" *     const ISA_REGISTER_CLASS_INFO *info",
" *   )",
" *       Get the size, in bits, of the register in the class",
" *       described by 'info'.",
" *",
" *   BOOL ISA_REGISTER_CLASS_INFO_Can_Store(",
" *     const ISA_REGISTER_CLASS_INFO *info",
" *   )",
" *       Return a flag that indicates if the registers in the class",
" *       described by 'info' can be stored to memory, i.e. there",
" *       is a store instruction for the registers in the class.",
" *",
" *   BOOL ISA_REGISTER_CLASS_INFO_Multiple_Save(",
" *     const ISA_REGISTER_CLASS_INFO *info",
" *   )",
" *       Return a flag that indicates if the registers in the class",
" *       described by 'info' can be saved and restore to memory in",
" *       multiples, i.e. as a group.",
" *",
" *   const char *ISA_REGISTER_CLASS_INFO_Name(",
" *     const ISA_REGISTER_CLASS_INFO *info",
" *   )",
" *       Return the name of the class described by 'info'.",
" *",
" *   const char *ISA_REGISTER_CLASS_INFO_Reg_Name(",
" *     const ISA_REGISTER_CLASS_INFO *info,",
" *     INT reg_index",
" *   )",
" *       Return the name of the 'reg_index'th register in the",
" *       class described by 'info'. NOTE: reg_index==0 corresponds",
" *       to the first register of the class.",
" *",
" *   const ISA_REGISTER_SUBCLASS_INFO *ISA_REGISTER_SUBCLASS_Info(",
" *     ISA_REGISTER_SUBCLASS sc",
" *   )",
" *",
" *       Return a pointer to the register subclass info for the",
" *       subclass 'sc'.",
" *",
" *   const char *ISA_REGISTER_SUBCLASS_INFO_Name(",
" *     const ISA_REGISTER_SUBCLASS_INFO *info",
" *   )",
" *",
" *       Return the name of the subclass described by 'info'.",
" *",
" *   ISA_REGISTER_CLASS ISA_REGISTER_SUBCLASS_INFO_Class(",
" *     const ISA_REGISTER_SUBCLASS_INFO *info",
" *   )",
" *",
" *       Return the base register class for the subclass described",
" *       by 'info'.",
" *",
" *   INT ISA_REGISTER_SUBCLASS_INFO_Count(",
" *     const ISA_REGISTER_SUBCLASS_INFO *info",
" *   )",
" *",
" *       Return the number of registers in the subclass described",
" *       by 'info'.",
" *",
" *   UINT ISA_REGISTER_SUBCLASS_INFO_Member(",
" *     const ISA_REGISTER_SUBCLASS_INFO *info,",
" *     INT n",
" *   )",
" *",
" *       Return the 'n'th member (register) of the subclass described",
" *       by 'info'. The order of the registers returned is arbitrary.",
" *",
" *   const char *ISA_REGISTER_SUBCLASS_INFO_Reg_Name(",
" *     const ISA_REGISTER_SUBCLASS_INFO *info,",
" *     INT index",
" *   )",
" *",
" *       Return the 'n'th member's register name of the subclass",
" *       described by 'info'. If the member does not have a subclass",
" *       specific name, NULL is returned.",
" *",
" *   void ISA_REGISTER_Initialize(void)",
" *       Initialize the register package for use with the ISA specified",
" *       by ISA_SUBSET_Value.",
" *",
" * ====================================================================",
" * ====================================================================",
" */", NULL};


static const char reg_class_iter[]= "\
typedef mUINT8 mISA_REGISTER_CLASS;\n\
\n\
#define FOR_ALL_ISA_REGISTER_CLASS(cl) \\\n\
	for (cl = ISA_REGISTER_CLASS_MIN; \\\n\
	     cl <= ISA_REGISTER_CLASS_MAX; \\\n\
	     cl = (ISA_REGISTER_CLASS)(cl + 1))\n\
\n\
#define FOR_ALL_ISA_REGISTER_CLASS_IN_REVERSE(cl) \\\n\
	for (cl = ISA_REGISTER_CLASS_MAX; \\\n\
	     cl >= ISA_REGISTER_CLASS_MIN; \\\n\
	     cl = (ISA_REGISTER_CLASS)(cl - 1))\n\
\n\
typedef struct {\n\
  mUINT8 isa_mask;\n\
  mUINT8 min_regnum;\n\
  mUINT8 max_regnum;\n\
  mUINT8 bit_size;\n\
  mBOOL can_store;\n\
  mBOOL multiple_save;\n\
  const char *name;\n\
  const char *reg_name[ISA_REGISTER_MAX+1];\n\
} ISA_REGISTER_CLASS_INFO;\n\n";
	
static const char * const reg_subclass_query[]={
"typedef mUINT8 mISA_REGISTER_SUBCLASS;",
"",
"#define FOR_ALL_ISA_REGISTER_SUBCLASS(sc) \\",
"	for (sc = ISA_REGISTER_SUBCLASS_MIN; \\",
"	     sc <= ISA_REGISTER_SUBCLASS_MAX; \\",
"	     sc = (ISA_REGISTER_SUBCLASS)(sc + 1))",
"",
"typedef struct {",
"  const char *name;",
"  mISA_REGISTER_CLASS rclass;",
"  mUINT8 count;",
"  mUINT8 members[ISA_REGISTER_MAX+1];",
"  const char *reg_name[ISA_REGISTER_MAX+1];",
"} ISA_REGISTER_SUBCLASS_INFO;",
"",
"inline const ISA_REGISTER_CLASS_INFO *ISA_REGISTER_CLASS_Info(",
"  ISA_REGISTER_CLASS rc",
")",
"{",
"  extern const ISA_REGISTER_CLASS_INFO ISA_REGISTER_CLASS_info[];",
"  extern mUINT8 ISA_REGISTER_CLASS_info_index[];",
"  INT index = ISA_REGISTER_CLASS_info_index[(INT)rc];",
"  return &ISA_REGISTER_CLASS_info[index];",
"}",
"",
"inline INT ISA_REGISTER_CLASS_INFO_First_Reg(",
"  const ISA_REGISTER_CLASS_INFO *info",
")",
"{",
"  return info->min_regnum;",
"}",
"",
"inline INT ISA_REGISTER_CLASS_INFO_Last_Reg(",
"  const ISA_REGISTER_CLASS_INFO *info",
")",
"{",
"  return info->max_regnum;",
"}",
"",
"inline INT ISA_REGISTER_CLASS_INFO_Bit_Size(",
"  const ISA_REGISTER_CLASS_INFO *info",
")",
"{",
"  return info->bit_size;",
"}",
"",
"inline BOOL ISA_REGISTER_CLASS_INFO_Can_Store(",
"  const ISA_REGISTER_CLASS_INFO *info",
")",
"{",
"  return info->can_store;",
"}",
"",
"inline BOOL ISA_REGISTER_CLASS_INFO_Multiple_Save(",
"  const ISA_REGISTER_CLASS_INFO *info",
")",
"{",
"  return info->multiple_save;",
"}",
"",
"inline const char *ISA_REGISTER_CLASS_INFO_Name(",
"  const ISA_REGISTER_CLASS_INFO *info",
")",
"{",
"  return info->name;",
"}",
"",
"inline const char *ISA_REGISTER_CLASS_INFO_Reg_Name(",
"  const ISA_REGISTER_CLASS_INFO *info,",
"  INT reg_index",
")",
"{",
"  return info->reg_name[reg_index];",
"}",
"",
"inline const ISA_REGISTER_SUBCLASS_INFO *ISA_REGISTER_SUBCLASS_Info(",
"  ISA_REGISTER_SUBCLASS sc",
")",
"{",
"  extern const ISA_REGISTER_SUBCLASS_INFO ISA_REGISTER_SUBCLASS_info[];",
"  return &ISA_REGISTER_SUBCLASS_info[sc];",
"}",
"",
"inline const char *ISA_REGISTER_SUBCLASS_INFO_Name(",
"  const ISA_REGISTER_SUBCLASS_INFO *info",
")",
"{",
"  return info->name;",
"}",
"",
"inline ISA_REGISTER_CLASS ISA_REGISTER_SUBCLASS_INFO_Class(",
"  const ISA_REGISTER_SUBCLASS_INFO *info",
")",
"{",
"  return (ISA_REGISTER_CLASS)info->rclass;",
"}",
"",
"inline INT ISA_REGISTER_SUBCLASS_INFO_Count(",
"  const ISA_REGISTER_SUBCLASS_INFO *info",
")",
"{",
"  return info->count;",
"}",
"",
"inline UINT ISA_REGISTER_SUBCLASS_INFO_Member(",
"  const ISA_REGISTER_SUBCLASS_INFO *info,",
"  INT n",
")",
"{",
"  return info->members[n];",
"}",
"",
"inline const char *ISA_REGISTER_SUBCLASS_INFO_Reg_Name(",
"  const ISA_REGISTER_SUBCLASS_INFO *info,",
"  INT n",
")",
"{",
"  return info->reg_name[n];",
"}",
"",
"extern void ISA_REGISTER_Initialize(void);\n", NULL};

static const char init_reg_func[]= "\
void ISA_REGISTER_Initialize(void)\n\
{\n\
  INT rc;\n\
  INT mask = 1 << (INT)ISA_SUBSET_Value;\n\
  for (rc = ISA_REGISTER_CLASS_MIN; rc <= ISA_REGISTER_CLASS_MAX; ++rc) {\n\
    INT i = ISA_REGISTER_CLASS_info_index[rc];\n\
    const ISA_REGISTER_CLASS_INFO *info = &ISA_REGISTER_CLASS_info[i];\n\
    while ((info->isa_mask & mask) == 0) ++info, ++i;\n\
    ISA_REGISTER_CLASS_info_index[rc] = i;\n\
  }\n\
}\n";


static const char * const abi_descript[]= { 
"/* ====================================================================",
" * ====================================================================",
" *",
" * Description:",
" *",
" *   A description of the ABI properties. The description exports",
" *   the following:",
" *",
" *   typedef (enum) ABI_PROPERTIES_ABI",
" *",
" *       An enumeration of the ABIs described. The names have the form:",
" *",
" *          ABI_PROPERTIES_ABI_xxx",
" *",
" *       where 'xxx' is replaced with the ABI name.",
" *",
" *   const ABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_UNDEFINED",
" *       Useful value guaranteed not to be a valid ABI_PROPERTIES_ABI.",
" *",
" *   ABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_Value",
" *       A variable containing the current ABI value.",
" *",
" *   const char *ABI_PROPERTIES_ABI_Name(ABI_PROPERTIES_ABI abi)",
" *      Returns a name for the given 'abi'.",
" *",
" *   void ABI_PROPERTIES_Initialize(void)",
" *       Initialize for the target ABI specified by ABI_PROPERTIES_ABI_Value.",
" *",
" *   const char *ABI_PROPERTY_Reg_Name(",
" *     ISA_REGISTER_CLASS rc,",
" *     INT reg",
" *   )",
" *       Return the ABI specific name of register 'reg' in class 'rc'.",
" *",
" *   BOOL ABI_PROPERTY_Is_xxx(",
" *     ISA_REGISTER_CLASS rc,",
" *     INT reg",
" *   )",
" *       Return a boolean that indicates if register 'reg' in class",
" *       'rc' had the property 'xxx'.",
" *",
" * ====================================================================",
" * ====================================================================",
" */", NULL};

static const char abi_common[]="\
extern ABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_Value;\n\
\n\
extern const char *ABI_PROPERTIES_ABI_Name(ABI_PROPERTIES_ABI abi);\n\
\n\
extern void ABI_PROPERTIES_Initialize(void);\n\
\n\
inline const char *ABI_PROPERTY_Reg_Name(\n\
  ISA_REGISTER_CLASS rc,\n\
  INT reg)\n\
{\n\
  extern const ABI_PROPERTIES *ABI_PROPERTIES_target_props;\n\
  return ABI_PROPERTIES_target_props->reg_names[rc][reg];\n\
}\n\n";

const char abi_com_func[]="\
ABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_Value = ABI_PROPERTIES_ABI_UNDEFINED;\n\
\n\
const char *ABI_PROPERTIES_ABI_Name(ABI_PROPERTIES_ABI abi)\n\
{\n\
  return abi_names[(INT)abi];\n\
}\n\
\n\
const ABI_PROPERTIES *ABI_PROPERTIES_target_props = &abi_properties[ABI_PROPERTIES_ABI_UNDEFINED];\n\
\n\
void ABI_PROPERTIES_Initialize(void)\n\
{\n\
  ABI_PROPERTIES_target_props = &abi_properties[(INT)ABI_PROPERTIES_ABI_Value];\n\
}\n";


void Register_Generator(void *pknobs, GEN_MODE mode)
{


  FILE *c_file, *h_file, *export_file;
  int reg_class_count = EKAPI_RegClassCount(pknobs);
  int index;
  int max_reg_count = 0;
  EKAPI_REGISTER_CLASS_INFO *reg_class_info = (EKAPI_REGISTER_CLASS_INFO*)
      malloc(reg_class_count*sizeof(EKAPI_REGISTER_CLASS_INFO));

  for (index=0; index<reg_class_count; index++){
    EKAPI_RegStruct4id(pknobs, &reg_class_info[index], index);
    if (EKAPI_GetRegMaxNum(&reg_class_info[index]) > max_reg_count){
      max_reg_count = EKAPI_GetRegMaxNum(&reg_class_info[index]);
    }
  }

  // ===============================================================
  // Block Generating targ_isa_registers.*

  Init_Module_Files(mode, "targ_isa_registers", &c_file, &h_file,
                    &export_file);
  Emit_Header(h_file, "targ_isa_registers", reg_descript);
  fprintf(h_file, "#include \"targ_isa_subset.h\"\n\n");
  fprintf(c_file, "#include \"targ_isa_subset.h\"\n");
  fprintf(c_file, "#include \"targ_isa_registers.h\"\n\n");

  fprintf(h_file, "\n#define ISA_REGISTER_FIRST (%d)\n", 0);
  fprintf(h_file, "#define ISA_REGISTER_MAX (%d)\n\n", max_reg_count);

  ///////////////////////////////////////////////////////////////////
  // Emit enum of register class
  fprintf(h_file, "typedef enum {\n  ISA_REGISTER_CLASS_UNDEFINED,\n");
  for (index=0; index<reg_class_count; index++){
    fprintf(h_file, "  ISA_REGISTER_CLASS_%s,\n",
            EKAPI_GetRegClassName(&reg_class_info[index]));
  }
  fprintf(h_file, "  ISA_REGISTER_CLASS_MIN = ISA_REGISTER_CLASS_%s,\n",
          EKAPI_GetRegClassName(&reg_class_info[0]));
  fprintf(h_file, "  ISA_REGISTER_CLASS_MAX = ISA_REGISTER_CLASS_%s,\n",
          EKAPI_GetRegClassName(&reg_class_info[reg_class_count-1]));
  fprintf(h_file, "  ISA_REGISTER_CLASS_COUNT = ISA_REGISTER_CLASS_MAX - ISA_REGISTER_CLASS_MIN + 1\n");
  fprintf(h_file, "} ISA_REGISTER_CLASS;\n\n");

  fprintf(h_file, "%s", reg_class_iter);

  ///////////////////////////////////////////////////////////////////
  // Emit register class info init
  fprintf(export_file, "ISA_REGISTER_CLASS_info\n");
  fprintf(c_file, "const ISA_REGISTER_CLASS_INFO ISA_REGISTER_CLASS_info[] = {\n");
  fprintf(c_file, "  { 0x00,   0,  -1,  0, 0, 0, \"UNDEFINED\", { 0 } },\n");
  for (index=0; index<reg_class_count; index++){
    fprintf(c_file, "  { 0x%02x, %3d, %3d, %2d, %d, %d, \"%s\",",
            EKAPI_GetRegIsaMask(&reg_class_info[index]),
            EKAPI_GetRegMinNum(&reg_class_info[index]),
            EKAPI_GetRegMaxNum(&reg_class_info[index]),
            EKAPI_GetRegBitSize(&reg_class_info[index]),
            EKAPI_GetRegCanStore(&reg_class_info[index]),
            EKAPI_GetRegMultiSave(&reg_class_info[index]),
            EKAPI_GetRegClassName(&reg_class_info[index]));
    int len=fprintf(c_file, "\n    { ");
    for (int i=EKAPI_GetRegMinNum(&reg_class_info[index]);
         i<EKAPI_GetRegMaxNum(&reg_class_info[index]); i++){
      if (len > 70){ len = fprintf(c_file, "\n      ");}
      len += fprintf(c_file, "\"%s\", ",
                     EKAPI_GetRegName(pknobs,&reg_class_info[index],i));
    }
    if (len > 70){ len = fprintf(c_file, "\n      ");}
    fprintf(c_file, "\"%s\" } },\n",
            EKAPI_GetRegName(pknobs,&reg_class_info[index],
                          EKAPI_GetRegMaxNum(&reg_class_info[index])));
  }
  fprintf(c_file, "};\n\n");

  fprintf(export_file, "ISA_REGISTER_CLASS_info_index\n");
  fprintf(c_file, "mUINT8 ISA_REGISTER_CLASS_info_index[] = {\n");
  fprintf(c_file, "  0,  /* ISA_REGISTER_CLASS_UNDEFINED */\n");
  for (index=0; index<reg_class_count; index++){
    fprintf(c_file, "  %d,  /* ISA_REGISTER_CLASS_%s */\n",
            index+1, EKAPI_GetRegClassName(&reg_class_info[index]));
  }
  fprintf(c_file, "};\n\n");


  ///////////////////////////////////////////////////////////////////
  // Emit enum of register sub class
  int subclass_count = EKAPI_RegSubclassCount(pknobs);
  fprintf(h_file, "typedef enum {\n  ISA_REGISTER_SUBCLASS_UNDEFINED,\n");
  for (index=0; index<subclass_count; index++){
    fprintf(h_file, "  ISA_REGISTER_SUBCLASS_%s,\n",
            EKAPI_RegSubclassName(pknobs,index));
  }
  fprintf(h_file,"  ISA_REGISTER_SUBCLASS_MIN = ISA_REGISTER_SUBCLASS_%s,\n",
          EKAPI_RegSubclassName(pknobs, 0));
  fprintf(h_file,"  ISA_REGISTER_SUBCLASS_MAX = ISA_REGISTER_SUBCLASS_%s,\n",
          EKAPI_RegSubclassName(pknobs, subclass_count-1));
  fprintf(h_file,"  ISA_REGISTER_SUBCLASS_COUNT = ISA_REGISTER_SUBCLASS_MAX - ISA_REGISTER_SUBCLASS_MIN + 1\n");
  fprintf(h_file,"} ISA_REGISTER_SUBCLASS;\n\n");

  for (int i=0; reg_subclass_query[i] != NULL; i++)
      fprintf(h_file, "%s\n", reg_subclass_query[i]);


  ///////////////////////////////////////////////////////////////////
  // Emit register sub class info init
  fprintf(export_file, "ISA_REGISTER_SUBCLASS_info\n");
  fprintf(c_file, "const ISA_REGISTER_SUBCLASS_INFO ISA_REGISTER_SUBCLASS_info[] = {\n");
  fprintf(c_file, "  { \"UNDEFINED\", ISA_REGISTER_CLASS_UNDEFINED, 0, { 0 }, { 0 } },\n");

  int * members = (int*)malloc(max_reg_count*sizeof(int));
  FmtAssert(members, ("Memory Allocation Failure!\n"));
  for (index=0; index<EKAPI_RegSubclassCount(pknobs); index++){
    int class_id;
    int num = EKAPI_RegSubclassMember(pknobs, index, &class_id, members);
    fprintf(c_file, "  { \"%s\", ISA_REGISTER_CLASS_%s, %d,\n",
            EKAPI_RegSubclassName(pknobs,index),
            EKAPI_GetRegClassName(&reg_class_info[class_id]), num);
    int len = fprintf(c_file, "    { ");
    for (int i=0; i<num; i++){
      if (len >= 70) { len = fprintf(c_file, "\n      "); }
      len += fprintf(c_file, "%d", members[i]);
      len += fprintf(c_file, "%s", i!=(num-1) ? ", " : " },\n");
    }
    fprintf(c_file, "    { 0 } },\n");
  }
  free(members);
  fprintf(c_file, "};\n\n");

  fprintf(c_file, init_reg_func); 
  fprintf(export_file, "ISA_REGISTER_Initialize\n");

  Emit_Tailer(h_file);
  Close_Module_Files(mode, &c_file, &h_file, &export_file);

  // End block Generating targ_isa_register.*
  // ==================================================================

  // ==================================================================
  // Block Generating targ_abi_properites.*
  Init_Module_Files(mode, "targ_abi_properties", &c_file, &h_file,
                    &export_file);
  Emit_Header(h_file, "targ_abi_properties", abi_descript);
  fprintf(h_file, "#include \"targ_isa_registers.h\"\n\n");
  fprintf(c_file, "#include \"targ_abi_properties.h\"\n\n");

  fprintf(h_file, "typedef struct {\n  mUINT32 reg_flags[%d][%d];\n",
          reg_class_count+1, max_reg_count+1);
  fprintf(h_file, "  const char *reg_names[%d][%d];\n} ABI_PROPERTIES;\n\n",
          reg_class_count+1, max_reg_count+1);

  // Emit abi properties bit mask define
  for (index=0; index<EKAPI_ABIPropCount(pknobs); index++){
    fprintf(h_file, "#define ABI_PROPERTY_%-20s 0x%08xU\n",
            EKAPI_ABIPropName(pknobs,index),
            (index==0)? 0 : (unsigned int)(1ULL << index-1));
  }

  /////////////////////////////////////////////////////////////////////
  // Emit abi properties init
  fprintf(c_file, "\nstatic const ABI_PROPERTIES abi_properties[] = {\n");
  for (int abi_i=0; abi_i<EKAPI_ABICount(pknobs); abi_i++){
    fprintf(c_file, "  {\n    /* %s */\n    {\n",
            EKAPI_ABIName(pknobs,abi_i));

    // Emit bit vector values init
    bv32_t *reg_flags = (bv32_t*)malloc((max_reg_count+1) * sizeof(bv32_t));
    FmtAssert(reg_flags, ("Memeory Allocation Failure!\n"));

    // Add a dummy head first
    int reg_i;
    fprintf(c_file, "      /* ISA_REGISTER_CLASS_UNDEFINED */");
    int len=fprintf(c_file,"\n      {");
    for (reg_i=0; reg_i<=max_reg_count; reg_i++){
      if (len>=70) { len = fprintf(c_file, "\n       "); }
      len += fprintf(c_file, " 0x00000000U,");
    }
    fprintf(c_file, " },\n");


    int reg_class_i;
    for (reg_class_i=0; reg_class_i<reg_class_count; reg_class_i++){
      int reg_num = EKAPI_RegPropMask(pknobs, reg_class_i, reg_flags);

      fprintf(c_file, "      /* ISA_REGISTER_CLASS_%s */",
              EKAPI_GetRegClassName(&reg_class_info[reg_class_i]));

      len = fprintf(c_file,"\n      {");
      for (reg_i=0; reg_i<=max_reg_count; reg_i++){
        if (len>=70) { len = fprintf(c_file, "\n       "); }
        len += fprintf(c_file, " 0x");
        if (reg_i <= EKAPI_GetRegMaxNum(&reg_class_info[reg_class_i])){
          len += fprintf(c_file, "%08xU,", reg_flags[reg_i] );
        }
        else{
          len += fprintf(c_file, "00000000U,");
        }
      }
      fprintf(c_file, " },\n");

    }
    fprintf(c_file, "    },\n    {\n");

    // Emit register names array. still need a dummy head
    fprintf(c_file, "      /* ISA_REGISTER_CLASS_UNDEFINED */");
    len=fprintf(c_file,"\n      {");
    for (reg_i=0; reg_i<=max_reg_count; reg_i++){
      if (len>=80) { len = fprintf(c_file, "\n       "); }
      len += fprintf(c_file, " \"\",");
    }
    fprintf(c_file, " },\n");

    for (reg_class_i=0; reg_class_i<reg_class_count; reg_class_i++){
      fprintf(c_file, "      /* ISA_REGISTER_CLASS_%s */",
              EKAPI_GetRegClassName(&reg_class_info[reg_class_i]));

      len=fprintf(c_file,"\n      {");
      for (reg_i=0; reg_i<=max_reg_count; reg_i++){
        char * name = "";
        if (reg_i <= EKAPI_GetRegMaxNum(&reg_class_info[reg_class_i])){
          name= EKAPI_GetRegName(pknobs, &reg_class_info[reg_class_i], reg_i);
        }
        if ((len+strlen(name))>76) { len = fprintf(c_file, "\n       "); }
        len += fprintf(c_file, " \"%s\",", name);
      }

      fprintf(c_file, " },\n");
    }
    fprintf(c_file, "    },\n  },\n");
  }
  free(reg_class_info);
  fprintf(c_file, "};\n\n");

  ///////////////////////////////////////////////////////////////
  // Emit abi name enum define
  fprintf(h_file, "\ntypedef enum {\n");
  for (index=0; index<EKAPI_ABICount(pknobs); index++){
    fprintf(h_file, "  ABI_PROPERTIES_ABI_%s,\n",
            EKAPI_ABIName(pknobs,index));
  }
  fprintf(h_file, "  ABI_PROPERTIES_ABI_UNDEFINED,\n");
  fprintf(h_file, "  ABI_PROPERTIES_ABI_MAX=%d\n} ABI_PROPERTIES_ABI;\n\n",
          index-1);

  //////////////////////////////////////////////////////////////
  // Emit abi name array init
  fprintf(c_file, "static const char * const abi_names[] = {\n");
  for (index=0; index<EKAPI_ABICount(pknobs); index++){
    fprintf(c_file, "  \"%s\",\n", EKAPI_ABIName(pknobs,index));
  }
  fprintf(c_file, "  \"UNDEFINED\"\n};\n\n");

  ///////////////////////////////////////////////////////////////
  // Emit common variables and functions, no difference when
  // knobs value changed
  fprintf(h_file, "%s", abi_common);
  fprintf(export_file, "ABI_PROPERTIES_ABI_Value\n");
  fprintf(export_file, "ABI_PROPERTIES_Initialize\n");

  fprintf(c_file, "%s", abi_com_func);
  fprintf(export_file, "ABI_PROPERTIES_target_props\n");
  fprintf(export_file, "ABI_PROPERTIES_Initialize\n");


  ///////////////////////////////////////////////////////////////
  // Emit abi properties query functions
  for (index=0; index<EKAPI_ABIPropCount(pknobs); index++){
    if (index==0){ fprintf(h_file, "/*ARGSUSED*/\n"); }
    fprintf(h_file, "inline BOOL ABI_PROPERTY_Is_%s(\n",
            EKAPI_ABIPropName(pknobs,index));
    fprintf(h_file, "  ISA_REGISTER_CLASS rc,\n  INT reg)\n{\n");
    if (index==0){ fprintf(h_file, "  return FALSE;\n"); }
    else{
      fprintf(h_file, "  extern const ABI_PROPERTIES *ABI_PROPERTIES_target_props;\n");
      fprintf(h_file, "  return (  ABI_PROPERTIES_target_props->reg_flags[rc][reg]\n");
      fprintf(h_file, "      & ABI_PROPERTY_%s) != 0;\n",
              EKAPI_ABIPropName(pknobs,index));
    }
    fprintf(h_file, "}\n\n");
  }




  Emit_Tailer(h_file);
  Close_Module_Files(mode, &c_file, &h_file, &export_file);
  // End block Generating targ_abi_properties.*
  // ==================================================================
}
