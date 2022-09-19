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


// isa_gen.cxx
/////////////////////////////////////
//
//  Generate an interface to create a new ISA (actually just an enum of
//  all the opcodes).
//    
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_gen.cxx,v $

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "gen_util.h"
#include "isa_gen.h"

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the ISA (actually just an enum of all the opcodes).",
  " *   The description exports the following:",
  " *",
  " *   TOPCODE stands for Target OPCODE; prefix is TOP.",
  " *",
  " *   typedef (enum) TOP",
  " *      Contains all the target opcodes.  Their names have the form",
  " *      TOP_<name>.",
  " *",
  " *   typedef mTOP",
  " *      The smallest integer type that can contain all values of a TOP,",
  " *      including TOP_UNDEFINED -- useful for conserving space in tables.",
  " *",
  " *   const TOP TOP_UNDEFINED",
  " *      Useful value guaranteed not to be a valid TOP.",
  " *",
  " *   const int TOP_count",
  " *      Gives the number of topcodes.",
  " *",
  " *   const char* TOP_Name(TOP topcode)",
  " *      Returns an assembler style name for the given TOP.",
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
void ISA_Create (const char *isa_name, ...)
/////////////////////////////////////
//  Emit the topcode header and c files.
/////////////////////////////////////
{
  FILE* hfile = fopen("topcode.h","w");
  FILE* cfile = fopen("topcode.c","w");
  FILE* efile = fopen("topcode.Exported","w");
  char *instruction_name;
  int instruction_count = 0;
  va_list ap;

  fprintf(cfile,"#include \"topcode.h\"\n");

  Emit_Header (hfile, "TOPCODE", interface);

  fprintf(hfile,"typedef enum topcode {");
  fprintf(cfile,"static const char* const top_names[] = {");

  bool is_first = true;
  va_start(ap,isa_name);
  while ((instruction_name = va_arg (ap, char *)) != NULL) {
    fprintf(hfile,"%s\n  TOP_%s",is_first ? "" : ",",
                                 Dot_To_Line(instruction_name));
    fprintf(cfile,"%s\n  \"%s\"",is_first ? "" : ",",
                                 instruction_name);
    if ( is_first )
      is_first = false;

    instruction_count++;
  }
  va_end(ap);

  fprintf(hfile,",\n  TOP_UNDEFINED");
  fprintf(cfile,",\n  \"UNDEFINED\"");

  fprintf(hfile,"\n} TOP;\n");
  fprintf(cfile,"\n};\n");

  fprintf(hfile,"\ntypedef %s mTOP;\n", 
		(instruction_count+1) <= 256 ? "mUINT8" : "mUINT16");

  fprintf(hfile,"\n#define TOP_count %d\n", instruction_count);

  fprintf(hfile,"\nextern const char* TOP_Name(TOP topcode);\n");
  fprintf(efile,"TOP_Name\n");
  fprintf(cfile,"\nconst char* TOP_Name(TOP topcode)\n{\n"
                "  return top_names[(int)topcode];\n"
		"}\n");

  Emit_Footer (hfile);
}
