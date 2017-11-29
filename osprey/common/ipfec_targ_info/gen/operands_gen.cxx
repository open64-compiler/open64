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
//  Module : operands_gen.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/operands_gen.cxx,v $
//
//  Description:
//  ============
//  Generate operands function
//=============================================================================

#include "operands_gen.h"

static const char* const description[] = {
"/* ====================================================================",
" * ====================================================================",
" *",
" * Description:",
" *",
" *   A description of the ISA instruction operands. The description",
" *   exports the following:",
" *",
" *   typedef (struct) ISA_OPERAND_VALTYP",
" *       Describes a particular operand/result type, including",
" *       the type of value it may contain and whether or not is",
" *       a register, literal or enum. The contents are private.",
" *",
" *   typedef (struct) ISA_OPERAND_INFO",
" *       Identifies the operand types of a particular instruction.",
" *       The contents are private.",
" *",
" *,  typedef (enum) ISA_OPERAND_USE",
" *       Identifies the useage of an operand of a particular instruction.",
" *       The names have the form OU_xxxx.",
" *",
" *   const INT OU_UNDEFINED",
" *       Identifies an undefined/unknown operand use.",
" *",
" *   const INT ISA_OPERAND_max_operands",
" *       The maximum number of operands of any instruction.",
" *",
" *   const INT ISA_OPERAND_max_results",
" *       The maximum number of results of any instruction.",
" *",
" *   const ISA_OPERAND_INFO *ISA_OPERAND_Info(TOP topcode)",
" *       Return a pointer to the operand info for the instruction",
" *       specified by 'topcode'.",
" *",
" *   INT ISA_OPERAND_INFO_Operands(const ISA_OPERAND_INFO *oinfo)",
" *       Return the number of operands specified by the operand",
" *       info 'oinfo'.",
" *",
" *   const ISA_OPERAND_VALTYP *ISA_OPERAND_INFO_Operand(",
" *     const ISA_OPERAND_INFO *oinfo,",
" *     int opnd",
" *   )",
" *       Get the operand type of operand 'opnd' specified by the",
" *       operand info 'oinfo'.",
" *",
" *   INT ISA_OPERAND_INFO_Results(const ISA_OPERAND_INFO *oinfo)",
" *       Return the number of results specified by the operand",
" *       info 'oinfo'.",
" *",
" *   const ISA_OPERAND_VALTYP *ISA_OPERAND_INFO_Result(",
" *     const ISA_OPERAND_INFO *oinfo,",
" *     int result",
" *   )",
" *       Get the operand type for the result 'result' specified by the",
" *       operand info 'oinfo'.",
" *",
" *   ISA_OPERAND_USE ISA_OPERAND_INFO_Use(",
" *     const ISA_OPERAND_INFO *oinfo,",
" *     INT opnd",
" *   )",
" *       Get the operand use type of operand 'opnd' specified by the",
" *       operand info 'oinfo'.",
" *",
" *   BOOL ISA_OPERAND_Any_Use(ISA_OPERAND_USE ouse)",
" *       Returns a boolean that indicates if any instruction in the",
" *       architecture has an an operand with usage 'use'. Useful",
" *       for omitting sections of code that aren't applicable to",
" *       some architectures.",
" *",
" *   ISA_REGISTER_CLASS ISA_OPERAND_VALTYP_Register_Class(",
" *     const ISA_OPERAND_VALTYP *otype",
" *   )",
" *       Get the register class for the operand specified by 'otype'.",
" *",
" *   ISA_REGISTER_SUBCLASS ISA_OPERAND_VALTYP_Register_Subclass(",
" *     const ISA_OPERAND_VALTYP *otype",
" *   )",
" *       Get the register subclass for the operand specified by 'otype'.",
" *",
" *   ISA_LIT_CLASS ISA_OPERAND_VALTYP_Literal_Class(const ISA_OPERAND_VALTYP *otype)",
" *       Get the literal class for the operand specified by 'otype'.",
" *",
" *   ISA_ENUM_CLASS ISA_OPERAND_VALTYP_Enum_Class(",
" *     const ISA_OPERAND_VALTYP *otype",
" *   )",
" *       Get the enum class for the operand specified by 'otype'.",
" *",
" *   INT ISA_OPERAND_VALTYP_Size(const ISA_OPERAND_VALTYP *otype)",
" *       Get the size for the operand specified by 'otype'.",
" *",
" *   BOOL ISA_OPERAND_VALTYP_Is_Register(const ISA_OPERAND_VALTYP *otype)",
" *       Return a boolean to specify if the operand specifed",
" *       by 'otype' is a register.",
" *",
" *   BOOL ISA_OPERAND_VALTYP_Is_Signed(const ISA_OPERAND_VALTYP *otype)",
" *       Return a boolean to specify if the operand specifed",
" *       by 'otype' is signed.",
" *",
" *   BOOL ISA_OPERAND_VALTYP_Is_FPU_Int(const ISA_OPERAND_VALTYP *otype)",
" *       Return a boolean to specify if the operand specifed",
" *       by 'otype' is an FPU integer.",
" *",
" *   BOOL ISA_OPERAND_VALTYP_Is_PCRel(const ISA_OPERAND_VALTYP *otype)",
" *       Return a boolean to specify if the operand specifed",
" *       by 'otype' is pc-relative.",
" *",
" *   BOOL ISA_OPERAND_VALTYP_Is_Literal (const ISA_OPERAND_VALTYP *otype)",
" *       Return a boolean to specify if the operand specifed",
" *       by 'otype' is a literal.",
" *",
" *   BOOL ISA_OPERAND_VALTYP_Is_Enum (const ISA_OPERAND_VALTYP *otype)",
" *       Return a boolean to specify if the operand specifed",
" *       by 'otype' is an enum.",
" *",
" *   BOOL TOP_Can_Have_Immediate(INT64 value, TOP topcode)",
" *       Return a boolean to specify if the 64-bit integer value can fit",
" *       in the literal field of an instruction with the given topcode.",
" *",
" *   INT TOP_Immediate_Operand(TOP topcode, ISA_LIT_CLASS *lclass)",
" *       If 'topcode' has an immediate operand, return its operand",
" *       number by value and literal class by reference through 'lclass'",
" *       (a null pointer can be passed for 'lclass' if the literal",
" *       class is not needed). If there is no immediate operand, return -1.",
" *",
" *   INT TOP_Relocatable_Operand(TOP topcode, ISA_LIT_CLASS *lclass)",
" *       If 'topcode' has a relocatable operand, return its operand",
" *       number by value and literal class by reference through 'lclass'",
" *       (a null pointer can be passed for 'lclass' if the literal",
" *       class is not needed). If there is no relocatable operand, return -1.",
" *",
" *   INT TOP_Find_Operand_Use(TOP topcode, ISA_OPERAND_USE use)",
" *       For the instruction specified by 'topcode', give the",
" *       operand number with the use 'use'. If there is no such",
" *       operand, return -1.",
" *",
" *   void TOP_Operand_Uses(TOP topcode, ISA_OPERAND_USE *uses)",
" *       For the instruction specified by 'topcode', return",
" *       the usage of all its operands in the array pointed to",
" *       by 'uses'. The use of operand n corresponds to 'uses'[n].",
" *",
" * ====================================================================",
" * ====================================================================",
" */", NULL};

static const char* const opnd_query[]= {
"inline const ISA_OPERAND_INFO *ISA_OPERAND_Info(TOP topcode)",
"{",
"  extern const mUINT8 ISA_OPERAND_info_index[];",
"  extern const ISA_OPERAND_INFO ISA_OPERAND_info[];",
"  INT index = ISA_OPERAND_info_index[(INT)topcode];",
"  return &ISA_OPERAND_info[index];",
"}",
"",
"inline INT ISA_OPERAND_INFO_Operands(const ISA_OPERAND_INFO *oinfo)",
"{",
"  return oinfo->opnds;",
"}",
"",
"inline const ISA_OPERAND_VALTYP *ISA_OPERAND_INFO_Operand(",
"  const ISA_OPERAND_INFO *oinfo,",
"  INT opnd)",
"{",
"  extern const ISA_OPERAND_VALTYP ISA_OPERAND_operand_types[];",
"  INT index = oinfo->opnd[opnd];",
"  return &ISA_OPERAND_operand_types[index];",
"}",
"",
"inline INT ISA_OPERAND_INFO_Results(const ISA_OPERAND_INFO *oinfo)",
"{",
"  return oinfo->results;",
"}",
"",
"inline const ISA_OPERAND_VALTYP *ISA_OPERAND_INFO_Result(",
"  const ISA_OPERAND_INFO *oinfo,",
"  INT result)",
"{",
"  extern const ISA_OPERAND_VALTYP ISA_OPERAND_operand_types[];",
"  INT index = oinfo->result[result];",
"  return &ISA_OPERAND_operand_types[index];",
"}",
"",
"inline ISA_REGISTER_CLASS ISA_OPERAND_VALTYP_Register_Class(",
"  const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (ISA_REGISTER_CLASS)otype->rclass;",
"}",
"",
"inline ISA_REGISTER_SUBCLASS ISA_OPERAND_VALTYP_Register_Subclass(",
"  const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (ISA_REGISTER_SUBCLASS)otype->rsubclass;",
"}",
"",
"inline ISA_LIT_CLASS ISA_OPERAND_VALTYP_Literal_Class(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (ISA_LIT_CLASS)otype->lclass;",
"}",
"",
"inline ISA_ENUM_CLASS ISA_OPERAND_VALTYP_Enum_Class(",
"  const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (ISA_ENUM_CLASS)otype->eclass;",
"}",
"",
"inline INT ISA_OPERAND_VALTYP_Size(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return otype->size;",
"}",
"",
"inline BOOL ISA_OPERAND_VALTYP_Is_Register(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (otype->flags & 0x01) != 0;",
"}",
"",
"inline BOOL ISA_OPERAND_VALTYP_Is_Literal(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (otype->lclass != LC_UNDEFINED);",
"}",
"",
"inline BOOL ISA_OPERAND_VALTYP_Is_Enum(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (otype->eclass != EC_UNDEFINED);",
"}",
"",
"inline BOOL ISA_OPERAND_VALTYP_Is_Signed(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (otype->flags & 0x02) != 0;",
"}",
"",
"/*ARGSUSED*/",
"inline BOOL ISA_OPERAND_VALTYP_Is_FPU_Int(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return FALSE;",
"}",
"",
"inline BOOL ISA_OPERAND_VALTYP_Is_PCRel(const ISA_OPERAND_VALTYP *otype)",
"{",
"  return (otype->flags & 0x08) != 0;",
"}",
"",
"inline ISA_OPERAND_USE ISA_OPERAND_INFO_Use(",
"  const ISA_OPERAND_INFO *oinfo,",
"  INT opnd)",
"{",
"  return (ISA_OPERAND_USE)oinfo->ouse[opnd];",
"}",
"",
"inline BOOL ISA_OPERAND_Any_Use(ISA_OPERAND_USE ouse)",
"{",
"  return (0x00000000000003fbULL & (1ULL << ouse)) != 0;",
"}",
"",
"extern INT TOP_Immediate_Operand(TOP topcode, ISA_LIT_CLASS *lclass);",
"",
"extern INT TOP_Relocatable_Operand(TOP topcode, ISA_LIT_CLASS *lclass);",
"",
"extern BOOL TOP_Can_Have_Immediate(INT64 value, TOP topcode);",
"",
"extern INT TOP_Find_Operand_Use(TOP topcode, ISA_OPERAND_USE use);",
"",
"extern void TOP_Operand_Uses(TOP topcode, ISA_OPERAND_USE *uses);\n",
NULL};


static const char * const top_opnd_query_func[]= {
"INT TOP_Immediate_Operand(TOP topcode, ISA_LIT_CLASS *lclass)",
"{",
"  INT iopnd;",
"  const ISA_OPERAND_INFO *opinfo = ISA_OPERAND_Info(topcode);",
"  INT opnds = ISA_OPERAND_INFO_Operands(opinfo);",
"  const INT first = 0;",
"",
"  for (iopnd = first; iopnd < opnds; ++iopnd) {",
"    const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Operand(opinfo, iopnd);",
"    ISA_LIT_CLASS lit_class = ISA_OPERAND_VALTYP_Literal_Class(vtype);",
"    if (lit_class != LC_UNDEFINED) {",
"      if (lclass) *lclass = lit_class;",
"      return iopnd;",
"    }",
"  }",
"",
"  return -1;",
"}",
"",
"INT TOP_Relocatable_Operand(TOP topcode, ISA_LIT_CLASS *lclass)",
"{",
"  extern const mINT8 ISA_OPERAND_relocatable_opnd[];",
"  INT iopnd = ISA_OPERAND_relocatable_opnd[(INT)topcode];",
"  if (lclass && iopnd >= 0) {",
"    const ISA_OPERAND_INFO *opinfo = ISA_OPERAND_Info(topcode);",
"    const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Operand(opinfo,iopnd);",
"    *lclass = (ISA_LIT_CLASS)ISA_OPERAND_VALTYP_Literal_Class(vtype);",
"  }",
"  return iopnd;",
"}",
"",
"BOOL TOP_Can_Have_Immediate(INT64 value, TOP topcode)",
"{",
"  ISA_LIT_CLASS lclass;",
"  if (TOP_Immediate_Operand(topcode, &lclass) < 0) return 0;",
"  return ISA_LC_Value_In_Class(value, lclass);",
"}",
"",
"INT TOP_Find_Operand_Use(TOP topcode, ISA_OPERAND_USE use)",
"{",
"  INT i;",
"  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(topcode);",
"  INT opnds = ISA_OPERAND_INFO_Operands(oinfo);",
"  for (i = 0; i < opnds; ++i) {",
"    ISA_OPERAND_USE this_use = ISA_OPERAND_INFO_Use(oinfo, i);",
"    if (this_use == use) return i;",
"  }",
"  return -1;",
"}",
"",
"void TOP_Operand_Uses(TOP topcode, ISA_OPERAND_USE *uses)",
"{",
"  INT i;",
"  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(topcode);",
"  INT opnds = ISA_OPERAND_INFO_Operands(oinfo);",
"  for (i = 0; i < opnds; ++i) {",
"    ISA_OPERAND_USE this_use = ISA_OPERAND_INFO_Use(oinfo, i);",
"    uses[i] = this_use;",
"  }",
"}", NULL};



void Operands_Generator(void *pknobs, GEN_MODE mode)
{
    FILE *c_file, *h_file, *export_file;
    int max=0;
    int max_src, max_dest;

    // Prepare for the register information.
    int reg_class_count = EKAPI_RegClassCount(pknobs);
    int max_reg_count = 0;
    EKAPI_REGISTER_CLASS_INFO *reg_class_info = (EKAPI_REGISTER_CLASS_INFO*)
      malloc(reg_class_count*sizeof(EKAPI_REGISTER_CLASS_INFO));

    for (int reg_i=0; reg_i<reg_class_count; reg_i++){
        EKAPI_RegStruct4id(pknobs, &reg_class_info[reg_i], reg_i);
        if (EKAPI_GetRegMaxNum(&reg_class_info[reg_i]) > max_reg_count){
            max_reg_count = EKAPI_GetRegMaxNum(&reg_class_info[reg_i]);
        }
    }

    Init_Module_Files(mode, "targ_isa_operands",
                      &c_file, &h_file, &export_file);
    Emit_Header(h_file, "targ_isa_operands", description);
    fprintf(h_file, "#include \"topcode.h\"\n"
                    "#include \"targ_isa_registers.h\"\n"
                    "#include \"targ_isa_enums.h\"\n"
                    "#include \"targ_isa_lits.h\"\n\n" );
    fprintf(c_file, "#include \"targ_isa_operands.h\"\n"
                    "#include \"targ_isa_registers.h\"\n"
                    "#include \"targ_isa_properties.h\"\n"
                    "#include \"targ_isa_lits.h\"\n\n" );

    // Emit enum type of operand use
    int use_i;
    fprintf(h_file, "typedef enum {\n");
    for (use_i=0; use_i<EKAPI_OpndUseCount(pknobs); use_i++){
        fprintf(h_file, "  %s,\n", EKAPI_OpndUseName(pknobs, use_i));
    }
    fprintf(h_file, "  OU_MAX = %d\n} ISA_OPERAND_USE;\n\n", use_i-1);

    // Emit operand value data type;
    fprintf(h_file, "typedef struct {\n"
                    "  mUINT8 rclass;\n"
                    "  mUINT8 rsubclass;\n"
                    "  mUINT8 lclass;\n"
                    "  mUINT8 eclass;\n"
                    "  mUINT8 size;\n"
                    "  mUINT8 flags;\n"
                    "} ISA_OPERAND_VALTYP;\n\n");
    /////////////////////////////////////////////////////////////////
    // Emit init and declaration of operand types array
    fprintf(export_file, "ISA_OPERAND_operand_types\n");
    fprintf(c_file, "\nconst ISA_OPERAND_VALTYP ISA_OPERAND_operand_types[] = {\n");
    for (int ty_i=0; ty_i<EKAPI_OpndTypeCount(pknobs); ty_i++){
        int reg_class_id = EKAPI_RegClass4otid(pknobs, ty_i);
        fprintf(c_file, "  { ISA_REGISTER_CLASS_%-10s,",
                (reg_class_id==-1) ? "UNDEFINED" :
                EKAPI_GetRegClassName(&reg_class_info[reg_class_id]));
        int reg_subc_id = EKAPI_RegSubclass4otid(pknobs, ty_i);
        fprintf(c_file, " ISA_REGISTER_SUBCLASS_%-10s,\n",
                (reg_subc_id==-1) ? "UNDEFINED" :
                EKAPI_RegSubclassName(pknobs, reg_subc_id));
        fprintf(c_file, "    %3d,",EKAPI_LitClass4otid(pknobs, ty_i)+1);
        int ec_id = EKAPI_EnumClass4otid(pknobs, ty_i);
        fprintf(c_file, " %s,",
                (ec_id==-1) ? "EC_UNDEFINED" :
                EKAPI_EnumClassName(pknobs, ec_id));
        fprintf(c_file, "%3d, 0x%02x }, /* %s */\n",
                EKAPI_Size4otid(pknobs, ty_i),
                EKAPI_Flag4otid(pknobs, ty_i),
                EKAPI_OpndTypeName(pknobs, ty_i)+strlen("OT_"));
    }
    fprintf(c_file, "};\n\n");

    max_src = EKAPI_GetSrcOpndsMax(pknobs);
    max_dest = EKAPI_GetDestOpndsMax(pknobs);

    // Emit operand number:
    fprintf(h_file, "enum {\n  ISA_OPERAND_max_operands=%d,\n"
                    "  ISA_OPERAND_max_results=%d\n};\n\n",
                    max_src,
                    max_dest);

    /////////////////////////////////////////////////////////////////
    // Emit operand info data type, and data init
    fprintf(h_file, "typedef struct {\n"
                    "  mUINT8 opnds;\n"
                    "  mUINT8 opnd[ISA_OPERAND_max_operands];\n"
                    "  mUINT8 ouse[ISA_OPERAND_max_operands];\n"
                    "  mUINT8 results;\n"
                    "  mUINT8 result[ISA_OPERAND_max_results];\n"
                    "} ISA_OPERAND_INFO;\n\n");

    fprintf(export_file, "ISA_OPERAND_info\n");

    EKAPI_OPNDGRP_INFO opnd_grp;
    opnd_grp.source = (int*)malloc(max_src*sizeof(int));
    opnd_grp.opnduse = (int*)malloc(max_src*sizeof(int));
    opnd_grp.result = (int*)malloc(max_dest*sizeof(int));
    FmtAssert( opnd_grp.source && opnd_grp.opnduse && opnd_grp.result,
               ("Memory Allocation Failure!\n"));

    fprintf(c_file, "const ISA_OPERAND_INFO ISA_OPERAND_info[] = {\n");

    for (int grp_i=0; grp_i<EKAPI_OpndGrpCount(pknobs); grp_i++){
        int opnd_i;

        EKAPI_GetOperandInfo4Grp(pknobs, grp_i, &opnd_grp);

        fprintf(c_file, "  {%2d, { ", opnd_grp.num_source);
        for (opnd_i=0; opnd_i<max_src; opnd_i++){
            fprintf(c_file, "%3d",
                    (opnd_i>=opnd_grp.num_source)?-1:opnd_grp.source[opnd_i]);
            fprintf(c_file, "%s", (opnd_i==(max_src-1))? " " : ", ");
        }
        fprintf(c_file, "},          /* O_%d */\n", grp_i);

        fprintf(c_file, "       { ");
        for (opnd_i=0; opnd_i<max_src; opnd_i++){
            fprintf(c_file, "%3d",
                    (opnd_i>=opnd_grp.num_source)?0:opnd_grp.opnduse[opnd_i]);
            fprintf(c_file, "%s", (opnd_i==(max_src-1))? " " : ", ");
        }
        fprintf(c_file, "},          /* O_%d */\n", grp_i);

        fprintf(c_file, "   %2d, { ", opnd_grp.num_result);
        for (opnd_i=0; opnd_i<max_dest; opnd_i++){
            fprintf(c_file, "%3d",
                    (opnd_i>=opnd_grp.num_result)?-1:opnd_grp.result[opnd_i]);
            fprintf(c_file, "%s", (opnd_i==(max_dest-1))? " " : ", ");
        }
        fprintf(c_file, "} },                       /* O_%d */\n", grp_i);
    }

    free(opnd_grp.source);
    free(opnd_grp.opnduse);
    free(opnd_grp.result);

    fprintf(c_file, "};\n\n");

    int op_i;
    /////////////////////////////////////////////////////////////////
    // Emit operand info index
    fprintf (export_file, "ISA_OPERAND_info_index\n");
    fprintf(c_file, "const mUINT8 ISA_OPERAND_info_index[] = {\n");

    for (op_i=0; op_i<EKAPI_OpCount(pknobs); op_i++){
        int opnd_grp = EKAPI_Op2Opndsgrp(pknobs, op_i);
        fprintf(c_file, "%5d,  /* %s: O_%d */\n",
                opnd_grp, EKAPI_OpName4id(pknobs, op_i), opnd_grp);
    }
    fprintf(c_file, "};\n\n");

    /////////////////////////////////////////////////////////////////
    // Emit rellocatable_opnd
    fprintf(export_file, "ISA_OPERAND_relocatable_opnd\n");
    fprintf(c_file, "const mINT8 ISA_OPERAND_relocatable_opnd[] = {\n");

    for (op_i=0; op_i<EKAPI_OpCount(pknobs); op_i++){
        int reloc = EKAPI_GetRelocatableOpnd(pknobs, op_i);
        fprintf(c_file, "%4d,  /* %s */\n",
                reloc, EKAPI_OpName4id(pknobs, op_i));
    }
    fprintf(c_file, "};\n\n");

    for (int i=0; opnd_query[i] != NULL; i++)
        fprintf(h_file, "%s\n", opnd_query[i]);
    for (int i=0; top_opnd_query_func[i] != NULL; i++)
        fprintf(c_file, "%s\n", top_opnd_query_func[i]);
    fprintf(export_file, "TOP_Immediate_Operand\n");
    fprintf(export_file, "TOP_Relocatable_Operand\n");
    fprintf(export_file, "TOP_Can_Have_Immediate\n");
    fprintf(export_file, "TOP_Find_Operand_Use\n");
    fprintf(export_file, "TOP_Operand_Uses\n");

    free(reg_class_info);

    Emit_Tailer(h_file);
    Close_Module_Files(mode, &c_file, &h_file, &export_file);
}
