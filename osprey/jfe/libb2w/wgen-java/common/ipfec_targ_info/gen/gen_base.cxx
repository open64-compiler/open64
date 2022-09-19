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
// Module: gen_base.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/gen_base.cxx,v $
//
// Description:
//   Public utilities on generation of machine description files.
//
//*********************************************************************

#include "gen_base.h"

static const char type_defs[]= "\n\
#ifndef defs_INCLUDED\n\
#define defs_INCLUDED\n\
typedef signed int INT;\n\
typedef signed int INT32;\n\
typedef signed long long INT64;\n\
typedef signed char mINT8;\n\
typedef signed short mINT16;\n\
typedef signed int mINT32;\n\
typedef signed long long mINT64;\n\
typedef unsigned int UINT;\n\
typedef unsigned int UINT32;\n\
typedef unsigned long long UINT64;\n\
typedef unsigned char mUINT8;\n\
typedef unsigned short mUINT16;\n\
typedef unsigned int mUINT32;\n\
typedef unsigned long long mUINT64;\n\
typedef int BOOL;\n\
typedef unsigned char mBOOL;\n\
#ifndef TRUE\n\
#define TRUE    ((BOOL) 1)\n\
#endif\n\
#ifndef FALSE\n\
#define FALSE   ((BOOL) 0)\n\
#endif\n\
#if (defined(_LANGUAGE_C) || defined(__GNUC__)) && !defined(inline)\n\
#define inline static __inline\n\
#endif\n\
#endif\n\n";


void Emit_Header (FILE *hfile,
		  const char *name,
		  const char * const *interface_desc,
                  bool cplusplus)
{
  int i;

  for (i=0; interface_desc[i]!=NULL; i++)
  {      
      fprintf(hfile, "%s\n", interface_desc[i]);
  }
  fprintf(hfile, "\n#ifndef %s_INCLUDED\n", name);
  fprintf(hfile, "#define %s_INCLUDED\n", name);

  if (cplusplus == false)
      fprintf(hfile, "#ifdef __cplusplus\n"
         "extern \"C\" {\n"
         "#endif\n");
  fprintf(hfile, type_defs);
}//end of emit header;

void Emit_Tailer(FILE *hfile, bool cplusplus)
{
  if (cplusplus == false) {
  fprintf(hfile, "\n\
#ifdef __cplusplus\n\
}\n\
#endif\n");
}
fprintf(hfile, "#endif\n");
}

void Init_Module_Files(GEN_MODE mode, const char * module_name,
         FILE **c_file, bool cplusplus)
{
    if (mode == GEN_MODE_FILE)
    {
        char * buf_name = (char *)malloc(strlen(module_name)+strlen(".Exported")+1);
        FmtAssert(buf_name, ("Unable to alloc memory from system!!\n"));

        if (cplusplus){
            sprintf(buf_name, "%s.cxx", module_name);
        }
        else{
            sprintf(buf_name, "%s.c", module_name);
        }
        *c_file = fopen(buf_name, "w");
        FmtAssert( *c_file,
                   ("Creat files: %s failure!!\n", module_name));
        free(buf_name);
  }
  else
  {
     *c_file = stdout;
  }  
}


void Init_Module_Files(GEN_MODE mode, const char * module_name,
         FILE **c_file, FILE **h_file, FILE **export_file,
         bool cplusplus)
{
  if (mode == GEN_MODE_FILE)
  {
    char * buf_name = (char *)malloc(strlen(module_name)+strlen(".Exported")+1);
    FmtAssert(buf_name, ("Unable to alloc memory from system!!\n"));

    if (cplusplus){
      sprintf(buf_name, "%s.cxx", module_name);
    }
    else{
      sprintf(buf_name, "%s.c", module_name);
    }
    *c_file = fopen(buf_name, "w");
    sprintf(buf_name, "%s.h", module_name);
    *h_file = fopen(buf_name, "w");
    sprintf(buf_name, "%s.Exported", module_name);
    *export_file = fopen(buf_name, "w");

    FmtAssert( (*c_file)&&(*h_file)&&(*export_file),
               ("Creat files: %s failure!!\n", module_name));
    free(buf_name);
  }
  else
  {
     *c_file = stdout;
     *h_file = stdout;
     *export_file = stdout;
  }
  
}

void Close_Module_Files(GEN_MODE mode,
         FILE **c_file, FILE **h_file, FILE **export_file)
{
  if (mode == GEN_MODE_FILE)
  {
    fclose(*c_file);
    fclose(*h_file);
    fclose(*export_file);
  }

}

