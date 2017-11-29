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


//  proc_properties_gen.cxx
/////////////////////////////////////
//
//  Generate an interface for specifying properties (attributes) for 
//  various processors in the PROC.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/proc_properties_gen.cxx,v $


#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <list>
#include <vector>
#include "gen_util.h"
#include "targ_proc.h"
#include "proc_properties_gen.h"


struct proc_property {
  const char* name;         // Name given for documentation and debugging
  int bit_position;         // bit postion in flag word
  std::vector<bool> members;    // set of opcodes that have this property
};

// Define special bit position values to indicate properties which
// are constant for all processors.
enum {
  BIT_ALWAYS_TRUE = -1,
  BIT_ALWAYS_FALSE = -2
};


static int proc_property_count = 0;  // How many properties?
static std::list<PROC_PROPERTY> properties; // All the properties

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the properties (attributes) for the processors",
  " *   in the PROC. The description exports the following:",
  " *",
  " *   BOOL PROC_xxx(void)",
  " *       Return true/false if PROCESSOR_Value has/does-not-have the",
  " *       property 'xxx'.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


/////////////////////////////////////
void PROC_Properties_Begin( const char* /* name */ )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
}

/////////////////////////////////////
PROC_PROPERTY PROC_Property_Create( const char* name )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  PROC_PROPERTY result = new proc_property;

  proc_property_count++;

  result->name = name;
  result->members = std::vector<bool> (PROCESSOR_count, false);

  properties.push_back(result);

  return result;
}

/////////////////////////////////////
void Processor_Group( PROC_PROPERTY property, ... )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  PROCESSOR opcode;

  va_start(ap,property);
  while ( (opcode = static_cast<PROCESSOR>(va_arg(ap,int)))
          != PROCESSOR_UNDEFINED ) {
    property->members[(int)opcode] = true;
  }
  va_end(ap);
}

/////////////////////////////////////
void PROC_Properties_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::list<PROC_PROPERTY>::iterator isi;
  int bit_pos;
  char filename[1000];
  sprintf (filename, "targ_proc_properties.h");
  FILE* hfile = fopen(filename, "w");
  sprintf (filename, "targ_proc_properties.c");
  FILE* cfile = fopen(filename, "w");
  sprintf (filename, "targ_proc_properties.Exported");
  FILE* efile = fopen(filename, "w");

  fprintf(cfile,"#include \"targ_proc_properties.h\"\n\n");

  Emit_Header (hfile, "targ_proc_properties", interface);
  fprintf(hfile, "#include \"targ_proc.h\"\n");

  // Assign bit positions to all the properties, and note which ones,
  // if any, are constant for all processors.
  bit_pos = 0;
  for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
    int code;
    PROC_PROPERTY property = *isi;
    bool all_same = true;
    bool first_member = property->members[0];
    for (code = 1; code < PROCESSOR_count; code++) {
      if (property->members[code] != first_member) {
	all_same = false;
	break;
      }
    }

    if (all_same) {
      property->bit_position = first_member ? BIT_ALWAYS_TRUE : BIT_ALWAYS_FALSE;
    } else {
      property->bit_position = bit_pos++;
    }
  }

  const char *int_type;
  const char *int_suffix;
  int int_size;
  if (bit_pos <= 8) {
    int_type = "mUINT8";
    int_suffix = "";
    int_size = 8;
  } else if (bit_pos <= 16) {
    int_type = "mUINT16";
    int_suffix = "";
    int_size = 16;
  } else if (bit_pos <= 32) {
    int_type = "mUINT32";
    int_suffix = "U";
    int_size = 32;
  } else {
    assert (bit_pos <= 64);
    int_type = "mUINT64";
    int_suffix = "ULL";
    int_size = 64;
  }
  fprintf (hfile, "\nextern const %s PROC_PROPERTIES_flags[];\n\n", int_type);
  fprintf (efile, "PROC_PROPERTIES_flags\n");
  fprintf (cfile,"const %s PROC_PROPERTIES_flags[] = {\n", int_type);

  for (int code = 0; code < PROCESSOR_count; code++) {
    unsigned long long flag_value = 0;

    for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
      PROC_PROPERTY property = *isi;
      if (property->members[code] && property->bit_position >= 0) {
	flag_value |= (1ULL << property->bit_position);
      }
    }
    fprintf (cfile, "  0x%0*" LL_FORMAT "x%s, /* %s:", int_size / 4,
					    flag_value, 
					    int_suffix,
					    PROCESSOR_Name((PROCESSOR)code));
    for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
      PROC_PROPERTY property = *isi;
      if (property->members[code] && property->bit_position >= 0) {
	fprintf (cfile, " %s", property->name);
      }
    }
    fprintf (cfile, " */\n");
  }
  fprintf (cfile, "  0x%0*" LL_FORMAT "x%s  /* UNDEFINED */\n"
		  "};\n",
		  int_size / 4,
		  0ULL,
		  int_suffix);

  for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
    PROC_PROPERTY property = *isi;
    if (property->bit_position >= 0) {
      fprintf (hfile, "#define PROP_%-16s 0x%" LL_FORMAT "x%s\n",
	       property->name, 
	       (1ULL << property->bit_position),
	       int_suffix);
    }
  }

  fprintf (hfile, "\n");
  for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
    PROC_PROPERTY property = *isi;
    if (property->bit_position >= 0) {
      fprintf (hfile, 
	       "#define PROC_%s() \\\n"
	       "  (PROC_PROPERTIES_flags[(INT)PROCESSOR_Value] & PROP_%s)\n",
	       property->name,
	       property->name);
    } else {
      fprintf (hfile, 
	       "#define PROC_%s() (%d)\n",
	       property->name,
	       property->bit_position == BIT_ALWAYS_TRUE ? 1 : 0);
    }
  }

  Emit_Footer (hfile);

  fclose(hfile);
  fclose(cfile);
  fclose(efile);
}
