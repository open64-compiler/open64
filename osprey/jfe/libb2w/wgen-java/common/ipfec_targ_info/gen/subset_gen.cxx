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
// Module: subset_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/subset_gen.cxx,v $
//
// Description:
//   Generate the definition of opcode and opcode name.
//
//*********************************************************************

#include "subset_gen.h"

static const char * const description[]= {"\
/* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 *\n\
 *   A description of the ISA subset hierarchy.  The description\n\
 *   exports the following:\n\
 *\n\
 *   typedef (enum) ISA_SUBSET\n\
 *       An enumberated type of the different subsets.\n\
 *\n\
 *   const ISA_SUBSET ISA_SUBSET_UNDEFINED\n\
 *       Useful value guaranteed not to be a valid ISA_SUBSET.\n\
 *\n\
 *   extern ISA_SUBSET ISA_SUBSET_Value\n\
 *       A variable containing the current subset value.\n\
 *\n\
 *   const char* ISA_SUBSET_Name( ISA_SUBSET subset )\n\
 *       Returns a name suitable for printing.\n\
 *\n\
 *   int ISA_SUBSET_Member( ISA_SUBSET subset, TOP opcode )\n\
 *       Is the given <opcode> a member of the given <subset>?\n\
 *\n\
 * ====================================================================\n\
 * ====================================================================\n\
 */", NULL};

static const char func_subset_name[]= "\
const char* ISA_SUBSET_Name( ISA_SUBSET subset ) {\n\
  return isa_subset_names[(INT)subset];\n\
}\n";

static const char* func_subset_member =
"int ISA_SUBSET_Member( ISA_SUBSET subset, TOP opcode )\n"
"{\n"
"  INT byte_index = ((UINT) opcode) / 8;\n"
"  INT bit_index = ((UINT) opcode) % 8;\n"
"  INT byte = isa_subset_opcode_table[(int) subset][byte_index];\n"
"  return (byte >> bit_index) & 1;\n"
"}\n";




void Subset_Generator(void *pknobs, GEN_MODE mode)
{
  FILE *c_file, *h_file, *export_file;
  int subset_index, op_index;

  Init_Module_Files(mode, "targ_isa_subset", &c_file, &h_file, &export_file);
  Emit_Header(h_file, "targ_isa_subset", description);
  fprintf(h_file, "#include \"topcode.h\"\n\n");
  fprintf(c_file, "#include \"topcode.h\"\n");
  fprintf(c_file, "#include \"targ_isa_subset.h\"\n\n");

  // Now generate the definition of subset name
  fprintf(h_file, "typedef enum {\n");
  fprintf(c_file, "static const char* const isa_subset_names[] = {\n");
  for (subset_index=0; subset_index<EKAPI_SubsetCount(pknobs); subset_index++){
    char * buf = EKAPI_SubsetName4id(pknobs, subset_index);
    fprintf(c_file, "  \"%s\",", buf);
    fprintf(h_file, "  ISA_SUBSET_%s,\n", buf);
    //? why not free buf? ???
  }
  fprintf(h_file, "  ISA_SUBSET_UNDEFINED,\n");
  fprintf(h_file, "  ISA_SUBSET_MIN=ISA_SUBSET_%s,\n",
                 EKAPI_SubsetName4id(pknobs,0));
  fprintf(h_file, "  ISA_SUBSET_MAX=ISA_SUBSET_%s\n} ISA_SUBSET;\n",
                 EKAPI_SubsetName4id(pknobs,EKAPI_SubsetCount(pknobs)-1));
  fprintf(c_file, "  \"UNDEFINED\"\n};\n");

  // Generate globle variable on current subset value:
  fprintf(h_file, "extern ISA_SUBSET ISA_SUBSET_Value;\n\n");
  fprintf(export_file, "ISA_SUBSET_Value\n");
  fprintf(c_file, "ISA_SUBSET ISA_SUBSET_Value = ISA_SUBSET_UNDEFINED;\n\n");

  // Generate two function definition
  fprintf(h_file, "extern const char* ISA_SUBSET_Name( ISA_SUBSET subset );\n");
  fprintf(export_file, "ISA_SUBSET_Name\n");
  fprintf(h_file, "extern INT ISA_SUBSET_Member( ISA_SUBSET subset,\n\
                              TOP opcode );\n");
  fprintf(export_file, "ISA_SUBSET_Member\n");


  fprintf(c_file, func_subset_name);

  //Generate array discribeing op's subset
  fprintf(c_file, "static const char isa_subset_opcode_table[%d][%d] = {\n",
                  EKAPI_SubsetCount(pknobs)+1, (EKAPI_OpCount(pknobs))/8 +1);
  for (subset_index=0; subset_index<EKAPI_SubsetCount(pknobs); subset_index++){
    char * cur_subset = EKAPI_SubsetName4id(pknobs, subset_index);
    fprintf(c_file, "  { /* %s */\n", cur_subset);

    char bitset = 0;
    char comment[20*8]=""; // I assue that an op name will not exceed 20!
    for (op_index=0; op_index<EKAPI_OpCount(pknobs); op_index++){
      char * this_set = EKAPI_Op2SubSet(pknobs, op_index);

      strcat(comment, EKAPI_OpName4id(pknobs, op_index));
      strcat(comment, " ");
      bitset = bitset << 1;
      bitset |= (strcmp(this_set, cur_subset)==0)? 1 : 0;

      if (op_index%8 == 7){
        fprintf(c_file, "    %#o, /* %s*/\n", bitset, comment);
        bitset = 0;
        comment[0]='\0';
      }
    }
    // Print the remaining.
    fprintf(c_file, "    %#o, /* %s*/\n", bitset, comment);
    bitset = 0;
    comment[0]='\0';
    fprintf(c_file, "  },\n");
  }

  // Print the ending null terminator
  fprintf(c_file, "  { /* UNDEFINED */\n    0\n  }\n};\n");
  fprintf(c_file, func_subset_member);

  Emit_Tailer(h_file);
  Close_Module_Files(mode, &c_file, &h_file, &export_file);
}
