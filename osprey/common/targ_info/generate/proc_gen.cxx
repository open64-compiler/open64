/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


// proc_gen.cxx
/////////////////////////////////////
//
//  Generate an interface to create a new PROC (actually just an enum of
//  all the processors).
//    
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/proc_gen.cxx,v $

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "gen_util.h"
#include "proc_gen.h"

static const char * const interface[] = {
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
  " */",
  NULL
};


/////////////////////////////////////
static char* Dot_To_Line(const char* str)
/////////////////////////////////////
//  Copy <str> to newly allocated memory, replacing "." with "_" and return
//  the result.
/////////////////////////////////////
{
  char *result = (char*) malloc(strlen(str)+1);
  const char *s;
  char *r;

  for (s = str, r = result; *s != 0; ++s, ++r) {
    if (*s == '.')
      *r = '_';
    else
      *r = *s;
  }

  *r = 0;

  return result;
}


/////////////////////////////////////
void PROC_Create (const char *proc_name, ...)
/////////////////////////////////////
//  Emit the targ_proc header and c files.
/////////////////////////////////////
{
  FILE* hfile = fopen("targ_proc.h","w");
  FILE* cfile = fopen("targ_proc.c","w");
  FILE* efile = fopen("targ_proc.Exported","w");
  char *instruction_name;
  int instruction_count = 0;
  va_list ap;

  fprintf(cfile,"#include \"targ_proc.h\"\n");

  Emit_Header (hfile, "targ_proc", interface);

  fprintf(hfile,"typedef enum processor {\n");
  fprintf(cfile,"\nstatic const char* const processor_names[] = {\n");

  va_start(ap,proc_name);
  while ((instruction_name = va_arg (ap, char *)) != NULL) {
    fprintf(hfile,"  PROCESSOR_%s,\n", Dot_To_Line(instruction_name));
    fprintf(cfile,"  \"%s\",\n", instruction_name);

    instruction_count++;
  }
  va_end(ap);

  fprintf(hfile,"  PROCESSOR_UNDEFINED\n"
		"} PROCESSOR;\n");
  fprintf(cfile,"  \"UNDEFINED\"\n"
		"};\n");

  fprintf(hfile,"\n#define PROCESSOR_count %d\n", instruction_count);
  fprintf(hfile,"\nextern PROCESSOR PROCESSOR_Value;\n");
  fprintf(cfile,"\nPROCESSOR PROCESSOR_Value = PROCESSOR_UNDEFINED;\n");
  fprintf(hfile,"\nextern const char* PROCESSOR_Name(PROCESSOR proc);\n");
  fprintf(efile,"PROCESSOR_Name\n");
  fprintf(cfile,"\nconst char* PROCESSOR_Name(PROCESSOR proc)\n"
		"{\n"
                "  return processor_names[(int)proc];\n"
		"}\n");

  Emit_Footer (hfile);

  fclose(hfile);
  fclose(cfile);
  fclose(efile);
}
