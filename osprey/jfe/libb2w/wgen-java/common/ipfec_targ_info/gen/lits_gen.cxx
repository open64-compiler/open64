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
//  Module : lits_gen.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/lits_gen.cxx,v $
//
//  Description:
//  ============
//  Generate lits function
//=============================================================================

#include "lits_gen.h"

static const char* const description[] = {"\
/* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 *\n\
 *   A list of all the lit classes used in an ISA.\n\
 *   It exports the following:\n\
 *",
" *   typedef (enum) ISA_LIT_CLASS\n\
 *       An enumeration of the lit classes.\n\
 *\n\
 *   typedef (struct) ISA_LIT_CLASS_INFO\n\
 *       Contains info about first and last ECV in the EC.\n\
 *       The contents are private.\n\
 *\n\
 *   typedef (struct) ISA_LIT_CLASS_VALUE_INFO\n\
 *       Contains info about name and min/max of the LC.\n\
 *       The contents are private.\n\
 *",
" *   const char * ISA_LC_Name (ISA_LIT_CLASS lc)\n\
 *       Returns name of <lc>.\n\
 *\n\
 *   INT64 ISA_LC_Min (ISA_LIT_CLASS lc)\n\
 *       Returns the minimum value for the specified <lc>. For classes\n\
 *       that have multiple sub-ranges, ISA_LC_Min returns the smallest\n\
 *       minimum of all the sub-ranges.\n\
 *",
" *   INT64 ISA_LC_Max (ISA_LIT_CLASS lc)\n\
 *       Returns the maximum value for the specified <lc>. For classes\n\
 *       that have multiple sub-ranges, ISA_LC_Max returns the largest\n\
 *       maximum of all the sub-ranges.\n\
 *\n\
 *   BOOL ISA_LC_Is_Signed (ISA_LIT_CLASS lc)\n\
 *       Returns whether the lit-class <lc> is signed.\n\
 *",
" *   BOOL ISA_LC_Value_In_Class (INT64 val, ISA_LIT_CLASS lc)\n\
 *       Returns whether <val> is a value that belongs to <lc>.\n\
 *\n\
 * ====================================================================\n\
 * ====================================================================\n\
 */", NULL};
 
void Lits_Generator(void *pknobs, GEN_MODE mode)
{
    FILE *c_file, *h_file, *export_file;
    int index, lc_count, r_index;
    char suffix_INT64[6] = "LL";
    #ifdef TARG_WIN
        strcpy(suffix_INT64, "mI64");
    #endif
    
    Init_Module_Files(mode, "targ_isa_lits", &c_file, &h_file, &export_file);    
    Emit_Header(h_file, "targ_isa_lits", description); 
    fprintf(c_file, "#include \"targ_isa_lits.h\"\n\n");
    fprintf(export_file, "ISA_LIT_CLASS_info\n");
    
    //  Print Literal Class enum tyep to h_file;
    lc_count = EKAPI_LitClassCount(pknobs);
    fprintf(h_file, "typedef enum {\n");
    fprintf(h_file, "\tLC_UNDEFINED,\n");
    for(index=0; index<lc_count; index++)
    {
        char *name = EKAPI_LitClassName(pknobs, index);
        fprintf(h_file, "\t%s,\n", 
                name
               );
        free(name);
    }
    fprintf(h_file, "\tLC_MAX\n} ISA_LIT_CLASS;\n\n");
    
    //  Print ISA_LIT_CLASS_info to c_file
    fprintf(c_file, "const ISA_LIT_CLASS_INFO ISA_LIT_CLASS_info[] = {\n");
    fprintf(c_file, 
            "  { { { 0x0000000000000000%s, 0x0000000000000000%s } }, 0, 0, \"LC_UNDEFINED\" },\n"
            , suffix_INT64, suffix_INT64);
    for(index=0; index<lc_count; index++)
    {
        EKAPI_RANGE ranges[20];
        int range_num = EKAPI_GetLcRange(pknobs, index, ranges);
        char *lc_name = EKAPI_LitClassName(pknobs, index);
        BOOL sign = EKAPI_LitIsSigned(pknobs, index);
        
        // print minimun and maximum of this ranges[]
        fprintf(c_file, "  { { { 0x%016llx%s, 0x%016llx%s }",
                ranges[0].min,
                suffix_INT64,
                ranges[range_num-1].max,  
                suffix_INT64     
               );
        // Print each range
        for(r_index=0; r_index<range_num; r_index++)
        {
            fprintf(c_file, ",\n      { 0x%016llx%s, 0x%016llx%s }", 
                    ranges[r_index].min,
                    suffix_INT64,
                    ranges[r_index].max,
                    suffix_INT64
                   );
        }
        // Print num, sign ,name
        fprintf(c_file, " }, %d, %d, \"%s\" },\n",
                range_num,
                sign, 
                lc_name);
        free(lc_name);
    }
    fprintf(c_file, "};\n");
    
    // Print struct and function to h_file
    fprintf(h_file, "typedef struct {\n"
                    "  struct { INT64 min; INT64 max; } range[9];\n"
                    "  mUINT8 num_ranges;\n"
                    "  mBOOL is_signed;\n"
                    "  const char *name;\n"
                    "} ISA_LIT_CLASS_INFO;\n\n"
            );
            
    fprintf(h_file, "inline const char * ISA_LC_Name (ISA_LIT_CLASS lc)\n"
                    "{\n"
                    "  extern const ISA_LIT_CLASS_INFO ISA_LIT_CLASS_info[];\n"
                    "  return ISA_LIT_CLASS_info[lc].name;\n"
                    "}\n\n"                    
                    "inline INT64 ISA_LC_Min (ISA_LIT_CLASS lc)\n"
                    "{\n"
                    "  extern const ISA_LIT_CLASS_INFO ISA_LIT_CLASS_info[];\n"
                    "  return ISA_LIT_CLASS_info[lc].range[0].min;\n"
                    "}\n\n"
                    "inline INT64 ISA_LC_Max (ISA_LIT_CLASS lc)\n"
                    "{\n"
                    "  extern const ISA_LIT_CLASS_INFO ISA_LIT_CLASS_info[];\n"
                    "  return ISA_LIT_CLASS_info[lc].range[0].max;\n"
                    "}\n\n"                    
                    "inline BOOL ISA_LC_Is_Signed (ISA_LIT_CLASS lc)\n"
                    "{\n"
                    "  extern const ISA_LIT_CLASS_INFO ISA_LIT_CLASS_info[];\n"
                    "  return ISA_LIT_CLASS_info[lc].is_signed;\n"
                    "}\n\n"
                    "inline BOOL ISA_LC_Value_In_Class (INT64 val, ISA_LIT_CLASS lc)\n"
                    "{\n"
                    "  extern const ISA_LIT_CLASS_INFO ISA_LIT_CLASS_info[];\n"
                    "  const ISA_LIT_CLASS_INFO *plc = ISA_LIT_CLASS_info + lc;\n"
                    "  INT i;\n"
                    "  for (i = 1; i <= plc->num_ranges; ++i) {\n"
                    "    INT64 min = plc->range[i].min;\n"
                    "    INT64 max = plc->range[i].max;\n"
                    "    if ( plc->is_signed ) {\n"
                    "      if (val >= min && val <= max) return TRUE;\n"
                    "    } else {\n"
                    "      if ((UINT64)val >= (UINT64)min && (UINT64)val <= (UINT64)max) return TRUE;\n"
                    "    }\n"
                    "  }\n"
                    "  return FALSE;\n"
                    "}\n"
            );
    
    Emit_Tailer(h_file);
    Close_Module_Files(mode, &c_file, &h_file, &export_file);
}
