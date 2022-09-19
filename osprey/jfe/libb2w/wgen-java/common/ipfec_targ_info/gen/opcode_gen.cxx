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
// Module: opcode_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/opcode_gen.cxx,v $
//
// Description:
//   Generate the definition of opcode and opcode name.
//
//*********************************************************************

#include "opcode_gen.h"

static const char* const description[]= {"\
/* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 *\n\
 *   A description of the ISA (actually just an enum of all the opcodes).\n\
 *   The description exports the following:\n\
 *", 
" *   TOPCODE stands for Target OPCODE; prefix is TOP.\n\
 *\n\
 *   typedef (enum) TOP\n\
 *      Contains all the target opcodes.  Their names have the form\n\
 *      TOP_<name>.\n\
 *\n\
 *   typedef mTOP\n\
 *      The smallest integer type that can contain all values of a TOP,\n\
 *      including TOP_UNDEFINED -- useful for conserving space in tables.\n\
 *",
" *   const TOP TOP_UNDEFINED\n\
 *      Useful value guaranteed not to be a valid TOP.\n\
 *\n\
 *   const int TOP_count\n\
 *      Gives the number of topcodes.\n\
 *\n\
 *   const char* TOP_Name(TOP topcode)\n\
 *      Returns an assembler style name for the given TOP.\n\
 *" ,
" * ====================================================================\n\
 * ====================================================================\n\
 */", NULL };





void Opcode_Generator(void *pknobs, GEN_MODE mode)
{
  FILE *c_file, *h_file, *export_file;
  int op_index;

  Init_Module_Files(mode, "topcode", &c_file, &h_file, &export_file);
  Emit_Header(h_file, "TOPCODE", description);
  fprintf(c_file, "#include \"topcode.h\"\n");

  fprintf(h_file, "typedef enum topcode {\n");
  fprintf(c_file, "static const char* const top_names[] = {\n");
  for (op_index=0; op_index<EKAPI_OpCount(pknobs); op_index++){
    char * buf = EKAPI_OpName4id(pknobs, op_index);
    fprintf(c_file, "  \"%s\",\n", buf);
    Dot2Line(buf);
    fprintf(h_file, "  TOP_%s,\n", buf);
    
  }
  fprintf(c_file, "  \"UNDEFINED\"\n};\n\n");
  fprintf(h_file, "  TOP_UNDEFINED\n} TOP;\n\n");

  fprintf(c_file, "const char* TOP_Name(TOP topcode)\n\
{\n\
  return top_names[(int)topcode];\n\
}\n");
  fprintf(h_file, "typedef mUINT16 mTOP;\n\n\
#define TOP_count 759\n\n\
extern const char* TOP_Name(TOP topcode);\n");
  fprintf(export_file, "TOP_Name\n");

  Emit_Tailer(h_file);
  Close_Module_Files(mode, &c_file, &h_file, &export_file);
}

