/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


//  isa_subset_gen.cxx
/////////////////////////////////////
//
//  Generate an interface for a description of the ISA subset hierarchy.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_subset_gen.cxx,v $


#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <list>
#include <vector>
#include "topcode.h"
#include "gen_util.h"
#include "isa_subset_gen.h"


struct isa_subset {
  const char* name;         // Name given for documentation and debugging
  int index;                // value in enum
  ISA_SUBSET superset;      // Parent in subset tree, NULL for roots
  std::vector<unsigned char> members;
                            // Bitset of opcodes that are members of the subset
};

static int isa_subset_count = 0;    // How many subsets?
static std::list<ISA_SUBSET> subsets;    // All the subsets
static size_t bit_vector_sizeof;    // How many bytes in a bit set of all
                                    //  opcodes
static std::vector<ISA_SUBSET> opcode_subset;
                                    // Which subset introduces the opcode?


static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the ISA subset hierarchy.  The description",
  " *   exports the following:",
  " *",
  " *   typedef (enum) ISA_SUBSET",
  " *       An enumberated type of the different subsets.",
  " *",
  " *   const ISA_SUBSET ISA_SUBSET_UNDEFINED",
  " *       Useful value guaranteed not to be a valid ISA_SUBSET.",
  " *",
  " *   extern ISA_SUBSET ISA_SUBSET_Value",
  " *       A variable containing the current subset value.",
  " *",
  " *   const char* ISA_SUBSET_Name( ISA_SUBSET subset )",
  " *       Returns a name suitable for printing.",
  " *",
  " *   int ISA_SUBSET_Member( ISA_SUBSET subset, TOP opcode )",
  " *       Is the given <opcode> a member of the given <subset>?",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


/////////////////////////////////////
void ISA_Subset_Begin( const char* /* name */ )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  bit_vector_sizeof =   (TOP_count + 7) / 8;
  opcode_subset = std::vector<ISA_SUBSET>(TOP_count,(ISA_SUBSET)0);
  for ( int code =  0; code < TOP_count; ++code )
    opcode_subset[code] = NULL;
}

/////////////////////////////////////
ISA_SUBSET ISA_Subset_Create( ISA_SUBSET parent, const char* name )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_SUBSET result = new isa_subset;

  result->name = name;
  result->index = isa_subset_count++;
  result->superset = parent;
  result->members = std::vector<unsigned char>(bit_vector_sizeof,0);

  subsets.push_front(result);

  return result;
}

/////////////////////////////////////
void Instruction_Group( ISA_SUBSET subset, ... )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  TOP opcode;

  va_start(ap,subset);
  while ( (opcode = static_cast<TOP>(va_arg(ap,int))) != TOP_UNDEFINED ) {
    ISA_SUBSET ss;
    int byte_index = ((unsigned int) opcode) / 8;
    int bit_index = ((unsigned int) opcode) % 8;

    for ( ss = subset; ss != NULL; ss = ss->superset )
      ss->members[byte_index] |= (1 << bit_index);

    if ( opcode_subset[opcode] != NULL ) {
      fprintf(stderr,"### attempting to add %s to ISA subset %s but "
                     "already in %s\n",
              TOP_Name(opcode),
              subset->name,
              opcode_subset[opcode]->name);
      exit(EXIT_FAILURE);
    }
    opcode_subset[opcode] = subset;
  }
  va_end(ap);
}

/////////////////////////////////////
void ISA_Subset_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::list<ISA_SUBSET>::iterator isi;
  bool err;
  int code;

  for ( err = false, code = 0; code < TOP_count; ++code ) {
    if ( ! opcode_subset[code] ) {
      fprintf(stderr,"### Error: no opcode subset for %s\n",
                     TOP_Name((TOP)code));
      err = true;
    }
  }
  if (err) exit(EXIT_FAILURE);

#define FNAME	"targ_isa_subset"
  char filename[1000];
  sprintf(filename,"%s.h", FNAME);
  FILE* hfile = fopen(filename,"w");
  sprintf(filename,"%s.c", FNAME);
  FILE* cfile = fopen(filename,"w");
  sprintf(filename,"%s.Exported", FNAME);
  FILE* efile = fopen(filename,"w");

  fprintf(cfile,"#include \"topcode.h\"\n");
  fprintf(cfile,"#include \"%s.h\"\n", FNAME);

  sprintf (filename, "%s", FNAME);
  Emit_Header (hfile, filename, interface);
  fprintf(hfile,"#include \"topcode.h\"\n");

  fprintf(hfile,"\ntypedef enum {\n");
  fprintf(cfile,"\nstatic const char* const isa_subset_names[] = {\n");

  for ( isi = subsets.begin(); isi != subsets.end(); ++isi ) {
    ISA_SUBSET subset = *isi;
    fprintf(hfile,"  ISA_SUBSET_%s,\n", subset->name);
    fprintf(cfile,"  \"%s\",", subset->name);
  }
  fprintf(hfile,"  ISA_SUBSET_UNDEFINED,\n"
		"  ISA_SUBSET_MIN=ISA_SUBSET_%s,\n"
		"  ISA_SUBSET_MAX=ISA_SUBSET_%s\n"
		"} ISA_SUBSET;\n",
		(*subsets.begin())->name,
		(*subsets.rbegin())->name);
  fprintf(cfile,"  \"UNDEFINED\"\n"
		"};\n");

  fprintf(hfile,"extern ISA_SUBSET ISA_SUBSET_Value;\n\n");
  fprintf(efile,"ISA_SUBSET_Value\n");
  fprintf(cfile,"ISA_SUBSET ISA_SUBSET_Value = ISA_SUBSET_UNDEFINED;\n\n");

  fprintf(hfile,"extern const char* ISA_SUBSET_Name( ISA_SUBSET subset );\n");
  fprintf(efile,"ISA_SUBSET_Name\n");
  fprintf(cfile,"const char* ISA_SUBSET_Name( ISA_SUBSET subset ) {\n");
  fprintf(cfile,"  return isa_subset_names[(INT)subset];\n");
  fprintf(cfile,"}\n");

  fprintf(cfile,"static const char isa_subset_opcode_table[%d][%d] = {\n",
          isa_subset_count+1,bit_vector_sizeof);

  for ( isi = subsets.begin(); isi != subsets.end(); ++isi ) {
    ISA_SUBSET subset = *isi;

    fprintf(cfile,"  { /* %s */\n", subset->name);
    for ( int i = 0; i < bit_vector_sizeof; ++i ) {
      int members = subset->members[i];
      fprintf(cfile,"    0%03o, /* ",members);
      for (int j = 0; j < 8; ++j) {
	if (members & (1 << j)) {
	  TOP top = (TOP)((i * 8) + j);
	  fprintf(cfile,"%s ",TOP_Name(top));
	}
      }
      fprintf(cfile,"*/\n");
    }
    fprintf(cfile,"  },\n");
  }
  fprintf(cfile,"  { /* UNDEFINED */\n"
		"    0\n"
		"  }\n");
  fprintf(cfile,"};\n");

  fprintf(hfile,"extern INT ISA_SUBSET_Member( ISA_SUBSET subset,\n"
                "                              TOP opcode );\n");
  fprintf(efile,"ISA_SUBSET_Member\n");
  fprintf(cfile,
	  "int ISA_SUBSET_Member( ISA_SUBSET subset, TOP opcode )\n"
	  "{\n"
	  "  INT byte_index = ((UINT) opcode) / 8;\n"
	  "  INT bit_index = ((UINT) opcode) %% 8;\n"
	  "  INT byte = isa_subset_opcode_table[(int) subset][byte_index];\n"
	  "  return (byte >> bit_index) & 1;\n"
	  "}\n");

  Emit_Footer (hfile);
}
