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
// Module: proc_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/proc_gen_new.cxx,v $
//
// Description:
//   Generate the definition of proc and it's properties;
//
//*********************************************************************

#include "proc_gen_new.h"

static const char* const description[]= {
"/* ====================================================================",
" * ====================================================================",
" *",
" * Description:",
" *",
" *   A description of the PROC (actually just an enum of all the processors).",
" *   The description exports the following:",
" *",
" *   typedef (enum) PROCESSOR",
" *      Contains all the target processors.  Their names have the form",
" *      PROCESSOR_<name>.",
" *",
" *   const PROCESSOR PROCESSOR_UNDEFINED",
" *      Useful value guaranteed not to be a valid PROCESSOR.",
" *",
" *   const int PROCESSOR_count",
" *      Gives the number of processors.",
" *",
" *   PROCESSOR PROCESSOR_Value",
" *      The current processor.",
" *",
" *   const char* PROCESSOR_Name(PROCESSOR topcode)",
" *      Returns a name for the given PROCESSOR.",
" *",
" * ====================================================================",
" * ====================================================================",
" */", NULL};

void Proc_Generator(void *pknobs, GEN_MODE mode)
{
    FILE *c_file, *h_file, *export_file;
    int op_index;

    Init_Module_Files(mode, "targ_proc", &c_file, &h_file, &export_file);
    Emit_Header(h_file, "targ_proc", description);
    fprintf(c_file, "#include \"targ_proc.h\"\n\n");

    fprintf(h_file, "typedef enum processor {\n");
    fprintf(c_file, "static const char* const processor_names[] = {\n");
    char * buf = EKAPI_ProcessName(pknobs);
    fprintf(c_file, "  \"%s\",\n", buf);
    fprintf(h_file, "  PROCESSOR_%s,\n", buf);   
    fprintf(c_file, "  \"UNDEFINED\"\n};\n\n");
    fprintf(h_file, "  PROCESSOR_UNDEFINED\n} PROCESSOR;\n\n");
    fprintf(h_file, "#define PROCESSOR_count %d\n\n", 1);
    fprintf(c_file, 
            "PROCESSOR PROCESSOR_Value = PROCESSOR_UNDEFINED;\n\n"
            "const char* PROCESSOR_Name(PROCESSOR proc)\n"
            "{\n"
            "  return processor_names[(int)proc];\n"
            "}\n");
    fprintf(h_file, 
            "extern PROCESSOR PROCESSOR_Value;\n\n"
	    "extern const char* PROCESSOR_Name(PROCESSOR proc);\n");
    fprintf(export_file, "PROCESSOR_Name\n");

    Emit_Tailer(h_file);
    Close_Module_Files(mode, &c_file, &h_file, &export_file);
}

