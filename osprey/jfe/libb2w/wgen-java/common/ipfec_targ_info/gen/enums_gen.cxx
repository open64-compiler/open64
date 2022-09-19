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
// Module: enums_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/enums_gen.cxx,v $
//
// Description:
//   Generate the definition of enum class tables and functions
//
//*********************************************************************

#include "enums_gen.h"

static const char* const description[] = {"\
/* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 *\n\
 *   A list of all the enum classes used in an ISA.\n\
 *   It exports the following:\n\
 *",
" *   typedef (enum) ISA_ENUM_CLASS\n\
 *       An enumeration of the enum classes.\n\
 *\n\
 *   typedef (enum) ISA_ENUM_CLASS_VALUE\n\
 *       An enumeration of the enum class values.\n\
 *\n\
 *   typedef (struct) ISA_ENUM_CLASS_INFO\n\
 *       Contains info about first and last ECV in the EC.\n\
 *       The contents are private.\n\
 *",
" *   typedef (struct) ISA_ENUM_CLASS_VALUE_INFO\n\
 *       Contains info about name and int-value of the ECV.\n\
 *       The contents are private.\n\
 *\n\
 *   const char * ISA_EC_Name (ISA_ENUM_CLASS)\n\
 *       Returns name of EC.\n\
 *\n\
 *   ISA_ENUM_CLASS_VALUE ISA_EC_First_Value (ISA_ENUM_CLASS)\n\
 *       Returns the first ECV for the specified EC.\n\
 *" ,
" *   ISA_ENUM_CLASS_VALUE ISA_EC_Last_Value (ISA_ENUM_CLASS)\n\
 *       Returns the last ECV for the specified EC.\n\
 *       Note that it assumes all ECV for an EC are in the\n\
 *       first/last range given by the above two functions.\n\
 *\n\
 *   const char * ISA_ECV_Name (ISA_ENUM_CLASS_VALUE)\n\
 *       Returns name of ECV.\n\
 *" ,
" *   INT ISA_ECV_Intval (ISA_ENUM_CLASS_VALUE)\n\
 *       Returns int-value of ECV.\n\
 *\n\
 * ====================================================================\n\
 * ====================================================================\n\
 */", NULL};

 
void Enums_Generator(void *pknobs, GEN_MODE mode)
{
    FILE *c_file, *h_file, *export_file;
    int index, ec_count, ecv_count;
    
    
    Init_Module_Files(mode, "targ_isa_enums", &c_file, &h_file, &export_file);
    Emit_Header(h_file, "targ_isa_enums", description);
    fprintf(c_file, "#include \"targ_isa_enums.h\"\n");
    fprintf(export_file, "ISA_ENUM_CLASS_info\nISA_ENUM_CLASS_VALUE_info\n");

    
    // Print enum Class to  h_file
    fprintf(h_file, "\ntypedef enum {\n");
    fprintf(h_file, "\tEC_UNDEFINED,\n");
    ec_count = EKAPI_EnumClassCount(pknobs);
    for (index=0; index<ec_count; index++)
    {
        fprintf(h_file, "\t%s,\n", EKAPI_EnumClassName(pknobs, index));
    }
    fprintf(h_file, "\tEC_MAX\n} ISA_ENUM_CLASS;\n\n");
     
    // Print enum value class to h_file
    ecv_count = EKAPI_EvClassCount(pknobs);
    fprintf(h_file, "typedef enum {\n\tECV_UNDEFINED,\n");
    for (index=0; index<ecv_count; index++)
    {
        fprintf(h_file, "\t%s,\n", EKAPI_EvClassName(pknobs, index));
    }
    fprintf(h_file, "\tECV_MAX\n} ISA_ENUM_CLASS_VALUE;\n\n");

    // Print table ISA_ENUM_CLASS_info to c_file
    fprintf(c_file, "\nconst ISA_ENUM_CLASS_INFO ISA_ENUM_CLASS_info[] = {\n");
    fprintf(c_file, "\t{ \"EC_UNDEFINED\",\tECV_UNDEFINED,\tECV_UNDEFINED },\n");
    for (index=0; index<ec_count; index++)
    {
        int first = EKAPI_EcFirstValue(pknobs, index);
        int last  = EKAPI_EcLastValue(pknobs, index);
        fprintf(c_file, "\t{ \"%s\",\t%s,\t%s },\n",
                EKAPI_EnumClassName(pknobs, index),
                EKAPI_EvClassName(pknobs, first),
                EKAPI_EvClassName(pknobs, last)               
               );
    }
    fprintf(c_file, "};\n\n");

    // Print table ISA_ENUM_CLASS_VALUE_info to C_file
    fprintf(c_file, "const ISA_ENUM_CLASS_VALUE_INFO ISA_ENUM_CLASS_VALUE_info[] = {\n");
    fprintf(c_file, "\t{ \"UNDEFINED\",\t-1 },\n");
    for (index=0; index<ecv_count; index++)
    {
        fprintf(c_file, "\t{ \"%s\",\t%d },\n",
                EKAPI_EvClassAsmName(pknobs, index),
                EKAPI_EvClassValue(pknobs, index)
               );
    }
    fprintf(c_file, "};\n\n");

    // Print struct and function to h_file
    fprintf(h_file, "typedef struct {\n"
                    "  char *name;\n"
                    "  ISA_ENUM_CLASS_VALUE first;\n"
                    "  ISA_ENUM_CLASS_VALUE last;\n"
                    "} ISA_ENUM_CLASS_INFO;\n"
                    "extern const ISA_ENUM_CLASS_INFO ISA_ENUM_CLASS_info[];\n"
                    "\n"
                    "typedef struct {\n"
                    "  char *name;\n"
                    "  INT intval;\n"
                    "} ISA_ENUM_CLASS_VALUE_INFO;\n"
                    "extern const ISA_ENUM_CLASS_VALUE_INFO ISA_ENUM_CLASS_VALUE_info[];\n\n"
                    );
                    
    fprintf(h_file, "inline const char * ISA_EC_Name (ISA_ENUM_CLASS ec)\n"
                    "{\n"
                    "  return ISA_ENUM_CLASS_info[ec].name;\n"
                    "}\n"
                    "\n"
                    "inline ISA_ENUM_CLASS_VALUE ISA_EC_First_Value (ISA_ENUM_CLASS ec)\n"
                    "{\n"
                    "  return ISA_ENUM_CLASS_info[ec].first;\n"
                    "}\n"
                    "\n"
                    "inline ISA_ENUM_CLASS_VALUE ISA_EC_Last_Value (ISA_ENUM_CLASS ec)\n"
                    "{\n"
                    "  return ISA_ENUM_CLASS_info[ec].last;\n"
                    "}\n"
                    "\n"
                    "inline const char * ISA_ECV_Name (ISA_ENUM_CLASS_VALUE ecv)\n"
                    "{\n"
                    "  return ISA_ENUM_CLASS_VALUE_info[ecv].name;\n"
                    "}\n"
                    "\n"
                    "inline INT ISA_ECV_Intval (ISA_ENUM_CLASS_VALUE ecv)\n"
                    "{\n"
                    "  return ISA_ENUM_CLASS_VALUE_info[ecv].intval;\n"
                    "}\n\n"
                    ); 
     
    Emit_Tailer(h_file);
    Close_Module_Files(mode, &c_file, &h_file, &export_file);
}

