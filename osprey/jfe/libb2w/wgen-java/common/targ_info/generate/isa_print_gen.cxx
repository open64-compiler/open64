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


// isa_print_gen.cxx
/////////////////////////////////////
//
//  Generate an interface for printing the instructions in an ISA.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_print_gen.cxx,v $

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <list>
#include <vector>
#include "topcode.h"
#include "targ_isa_properties.h"
#include "gen_util.h"
#include "isa_print_gen.h"

/* The maximum number of operands and results used by ANY target.
 * (It would be better to get the max operands and results from the
 * generated targ_isa_operands.h file -- Ken)
 */
#define MAX_OPNDS 6
#define MAX_RESULTS 2

typedef enum {
	END	= 0,			// end of list marker
	NAME	= 1,			// instruction name/mnemonic
	OPND    = 2,			// OPND+n => operand n
   	RESULT  = OPND+MAX_OPNDS,	// RESULT+n => result n
} COMP_TYPE;

#define MAX_LISTING_OPERANDS (RESULT+MAX_RESULTS)

struct isa_print_type {
  const char *name;         // Name given for documentation and debugging
  const char *format_string; // format string to print this print type
};

struct list_info {
  isa_print_type *type;
  unsigned char	 args;  // Number of sprintf arguments
  unsigned char	 arg[MAX_LISTING_OPERANDS]; 
  int index;		
  bool have_name;
};

typedef list_info *LISTING_INFO;

// map to link TOPs_ with list_info properties
struct op_pr {
  list_info *desc;
  struct op_pr *next;
};

static std::list<LISTING_INFO> all_prints;  // all the different print formats
static LISTING_INFO current_print_desc;
static op_pr *op_prints[TOP_count+1];
static std::list<op_pr*> op_prints_list;
static int list_index;
static const char *(*asmname)(TOP topcode);
static bool top_specified[TOP_count];

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of how to print the operands of ISA instructions",
  " *   in ascii. The description exports the following:",
  " *",
  " *   typedef (enum) ISA_PRINT_COMP",
  " *       An enumeration of the instruction components to be printed.",
  " *",
  " *   typedef (struct) ISA_PRINT_INFO",
  " *       Describes how one particular instruction is printed.",
  " *       The contents are private.",
  " *",
  " *   const INT ISA_PRINT_COMP_MAX",
  " *       The maximum number of components to be printed for any instruction.",
  " *",
  " *   const ISA_PRINT_INFO *ISA_PRINT_Info(TOP topcode)",
  " *       Returns a pointer to the printing description for the",
  " *       instruction specified by 'topcode'.",
  " *",
  " *       The instruction is printed by calling one of the printf routines",
  " *       with the format string returned from ISA_PRINT_INFO_Format.",
  " *       Additional printf arguments, necessitated by the format string,",
  " *       are described by ISA_PRINT_INFO_Comp.",
  " *",
  " *   INT ISA_PRINT_INFO_Comp(const ISA_PRINT_INFO *info, INT index)",
  " *       Identifies a instruction component to be printed.",
  " *",
  " *       'index' specifies the component. The first component has index",
  " *       0; the end of the components is signalled by the return of",
  " *       ISA_PRINT_COMP_end.",
  " *",
  " *   const char *ISA_PRINT_INFO_Format(const ISA_PRINT_INFO *info)",
  " *       The printf format string for printing the instruction",
  " *       described by 'info'.",
  " *",
  " *   const char *ISA_PRINT_AsmName(TOP topcode)",
  " *       Returns the assembly language name for <topcode>.",
  " *",
  " *   BOOL ISA_PRINT_Operand_Is_Part_Of_Name(TOP topcode, INT opindex)",
  " *       Returns whether the operand is part of the full asm name.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};

/////////////////////////////////////
const char* Print_Name(int print_index)
/////////////////////////////////////
{
  static char *comp_name[MAX_LISTING_OPERANDS];
  static bool initialized;

  if (!initialized) {
    int i;
    for (i = 0; i < MAX_LISTING_OPERANDS; ++i) {
      char buf[80];
      if (i == END) {
	comp_name[i] = "ISA_PRINT_COMP_end";
      } else if (i == NAME) {
	comp_name[i] = "ISA_PRINT_COMP_name";
      } else if (i == OPND) {
	comp_name[i] = "ISA_PRINT_COMP_opnd";
      } else if (i > OPND && i < (OPND + MAX_OPNDS)) {
	sprintf(buf, "ISA_PRINT_COMP_opnd+%d", i - OPND);
	comp_name[i] = strdup(buf);
      } else if (i == RESULT) {
	comp_name[i] = "ISA_PRINT_COMP_result";
      } else {
	assert(i > RESULT && i < (RESULT + MAX_RESULTS));
	sprintf(buf, "ISA_PRINT_COMP_result+%d", i - RESULT);
	comp_name[i] = strdup(buf);
      }
    }
    initialized = true;
  }

  return comp_name[print_index];
}

/////////////////////////////////////
void ISA_Print_Begin( const char* /* name */ )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
}

/////////////////////////////////////
ISA_PRINT_TYPE ISA_Print_Type_Create ( 
  const char* name, 
  const char* format_string)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  LISTING_INFO result = new list_info;
  result->type = new isa_print_type;
  result->type->name = name;
  result->type->format_string = format_string;
  result->index = list_index;
  result->args = 0;
  result->have_name = false;
  current_print_desc = result;
  all_prints.push_back (current_print_desc);
  ++list_index;
  return result->type;
}

/////////////////////////////////////
void Instruction_Print_Group(ISA_PRINT_TYPE print_type, TOP top, ... )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  TOP opcode;

  if (!current_print_desc->have_name) {
    fprintf(stderr, "### Warning: no instruction name specified for %s\n",
		    current_print_desc->type->name);
    // exit(EXIT_FAILURE);
  }
  op_pr *op_print = new op_pr;
  op_prints_list.push_back(op_print);
  op_print->desc = current_print_desc;
  op_prints[(int)top] = op_print;
  top_specified[(int)top] = true;

  va_start(ap, top);
  while ( (opcode = static_cast<TOP>(va_arg(ap,int))) != TOP_UNDEFINED ) {
    op_print = new op_pr;
    op_prints_list.push_back(op_print);
    op_print->desc = current_print_desc;
    op_prints[(int)opcode] = op_print;
    top_specified[(int)opcode] = true;
  }
  va_end(ap);
}

/////////////////////////////////////
void Name (void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (current_print_desc->args == MAX_LISTING_OPERANDS) {
    fprintf(stderr, "### Error: too many listing operands for %s\n",
		    current_print_desc->type->name);
    exit(EXIT_FAILURE);
  }
  current_print_desc->arg[current_print_desc->args] = NAME;
  current_print_desc->args++;
  current_print_desc->have_name = true;
}

/////////////////////////////////////
void Operand (int operand_index)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (operand_index >= MAX_OPNDS) {
    fprintf(stderr, "### Error: operand index (%d) exceeds %d\n",
		    operand_index, MAX_OPNDS-1);
    exit(EXIT_FAILURE);
  }
  if (current_print_desc->args == MAX_LISTING_OPERANDS) {
    fprintf(stderr, "### Error: too many listing operands for %s\n",
		    current_print_desc->type->name);
    exit(EXIT_FAILURE);
  }
  current_print_desc->arg[current_print_desc->args] = OPND+operand_index;
  current_print_desc->args++;
}

/////////////////////////////////////
void Result (int result_index)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (result_index >= MAX_RESULTS) {
    fprintf(stderr, "### Error: result index (%d) exceeds %d\n",
		    result_index, MAX_RESULTS-1);
    exit(EXIT_FAILURE);
  }
  if (current_print_desc->args == MAX_LISTING_OPERANDS) {
    fprintf(stderr, "### Error: too many listing operands for %s\n",
		    current_print_desc->type->name);
    exit(EXIT_FAILURE);
  }
  current_print_desc->arg[current_print_desc->args] = RESULT+result_index;
  current_print_desc->args++;
}

/////////////////////////////////////
void Set_AsmName_Func(const char *(*asmname_func)(TOP topcode))
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  asmname = asmname_func;
}

/////////////////////////////////////
void ISA_Print_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::list<LISTING_INFO>::iterator isi;

#define FNAME "targ_isa_print"
  char buf[1000];
  sprintf (buf, "%s.h", FNAME);
  FILE* hfile = fopen(buf, "w");
  sprintf (buf, "%s.c", FNAME);
  FILE* cfile = fopen(buf, "w");
  sprintf (buf, "%s.Exported", FNAME);
  FILE* efile = fopen(buf, "w");
  const char comma = ',';
  const char space = ' ';
  const char * const isa_print_type_format = "\t/* %s[%d] */";
  const char * const isa_print_format_format = "  { %-14s ";
  const char * const isa_print_args_format = " %s%c";
  int top;
  bool err;

  for (err = false, top = 0; top < TOP_count; ++top) {
    bool is_dummy = TOP_is_dummy((TOP)top);
    bool is_simulated = TOP_is_simulated((TOP)top);
    if (!top_specified[top]) {
      if (!is_simulated && !is_dummy) {
	fprintf(stderr, "### Error: no print specification for %s\n",
		        TOP_Name((TOP)top));
	err = true;
      }
    } else if (is_dummy) {
      fprintf(stderr, "### Error: print specification for dummy op %s\n",
		      TOP_Name((TOP)top));
      err = true;
    } else if (is_simulated) {
      fprintf(stderr, "### Error: print specification for simulated op %s\n",
		      TOP_Name((TOP)top));
      err = true;
    }
  }
  if (err) exit(EXIT_FAILURE);

  fprintf(cfile,"#include <string.h>\n");
  fprintf(cfile,"#include \"topcode.h\"\n");
  fprintf(cfile,"#include \"%s.h\"\n\n", FNAME);

  sprintf (buf, "%s", FNAME);
  Emit_Header (hfile, buf, interface);
  fprintf(hfile,"#include \"topcode.h\"\n");

  Emit_Definitions (hfile, "ISA_PRINT_");

  fprintf(hfile, "\ntypedef enum {\n"
	"  %-21s = %d,  /* %s */\n"
	"  %-21s = %d,  /* %s */\n"
	"  %-21s = %d,  /* %s */\n"
   	"  %-21s = %d,  /* %s */\n"
   	"  %-21s = %d   /* %s */\n"
	"} ISA_PRINT_COMP;\n",
	Print_Name(END), END, "End of list marker",
	Print_Name(NAME), NAME, "Instruction name",
	Print_Name(OPND), OPND, "OPND+n => operand n",
	Print_Name(RESULT), RESULT, "RESULT+n => result n",
        "ISA_PRINT_COMP_MAX", MAX_LISTING_OPERANDS-1, "Last component");

  fprintf(hfile, "\ntypedef struct {\n"
		"  const char *format;\n"
  		"  mUINT8 comp[%d];\n" 
		"} ISA_PRINT_INFO;\n",MAX_LISTING_OPERANDS);

  fprintf(hfile, "\nextern const ISA_PRINT_INFO ISA_PRINT_info[%d];\n",
						list_index + 1);

  fprintf(efile, "ISA_PRINT_info\n");

  fprintf(cfile, "\nconst ISA_PRINT_INFO ISA_PRINT_info[%d] = {\n",
						list_index + 1);

  fprintf (cfile, isa_print_format_format, "\"\",");
  fprintf (cfile, isa_print_args_format, Print_Name(END), space);
  fprintf (cfile, "},");
  fprintf (cfile, isa_print_type_format, "print_NULL", 0);
  fprintf (cfile, "\n");
  for ( isi = all_prints.begin(); isi != all_prints.end(); ++isi ) {
  	LISTING_INFO curr_type = *isi;
	sprintf (buf, "\"%s\",", curr_type->type->format_string);
	fprintf (cfile, isa_print_format_format, buf);
	for (int i = 0; i < curr_type->args; i++) {
	    fprintf (cfile, isa_print_args_format,
			Print_Name(curr_type->arg[i]),
			comma);
	    fprintf (cfile, isa_print_type_format, curr_type->type->name, i);
	    fprintf (cfile, "\n%19s", "");
	}
	fprintf (cfile, isa_print_args_format, Print_Name(END), space);
	fprintf (cfile, "},");
	fprintf (cfile, isa_print_type_format, 
			curr_type->type->name,
			curr_type->args);
	fprintf (cfile, "\n");
  }
  fprintf (cfile, "};\n");

  fprintf(hfile, "\nextern const unsigned char ISA_PRINT_info_index[%d];\n", TOP_count);

  fprintf(efile, "ISA_PRINT_info_index\n");

  fprintf(cfile, "\nconst mUINT8 ISA_PRINT_info_index[%d] = {\n", TOP_count);
  for (top = 0; top < TOP_count; ++top ) {
  	op_pr *op_print = op_prints[top];
    	if (op_print) {
  	    fprintf(cfile, "  %3d,  /* %s: %s */\n", 
			op_print->desc->index+1,
			TOP_Name((TOP)top),
			op_print->desc->type->name);
	} else {
  	    fprintf(cfile, "  %3d,  /* %s */\n", 
			0,
			TOP_Name((TOP)top));
	}
  }
  fprintf(cfile, "};\n");

  fprintf(hfile, "\ninline const ISA_PRINT_INFO *ISA_PRINT_Info(TOP topcode)\n"
		 "{\n"
		 "  INT index = ISA_PRINT_info_index[(INT)topcode];\n"
		 "  return index == 0 ? 0 : &ISA_PRINT_info[index];\n"
		 "}\n");

  fprintf(hfile, "\ninline const char* ISA_PRINT_INFO_Format(const ISA_PRINT_INFO *info)\n"
		 "{\n"
		 "  return info->format;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_PRINT_INFO_Comp(const ISA_PRINT_INFO *info, INT index)\n"
		 "{\n"
		 "  return info->comp[index];\n"
		 "}\n");

  if (asmname) {
    fprintf(cfile, "\nconst char * const ISA_PRINT_asmname[] = {\n");
    for (top = 0; top < TOP_count; ++top) {
      fprintf(cfile, "  \"%s\",\n", asmname((TOP)top));
    }
    fprintf(cfile, "  \"UNDEFINED\"\n"
		   "};\n");

    fprintf(hfile, "\ninline const char *ISA_PRINT_AsmName(TOP topcode)\n"
		   "{\n"
		   "  extern const char * const ISA_PRINT_asmname[];\n"
		   "  return ISA_PRINT_asmname[(INT)topcode];\n"
		   "}\n");

    fprintf(efile, "ISA_PRINT_asmname\n");
  } else {
    fprintf(hfile, "\ninline const char *ISA_PRINT_AsmName(TOP topcode)\n"
		   "{\n"
		   "  return TOP_Name(topcode);\n"
		   "}\n");
  }

  fprintf(hfile, "\nextern BOOL ISA_PRINT_Operand_Is_Part_Of_Name(TOP topcode, INT opindex);\n");
  fprintf(efile, "ISA_PRINT_Operand_Is_Part_Of_Name\n");
  fprintf(cfile, "\nBOOL ISA_PRINT_Operand_Is_Part_Of_Name(TOP topcode, INT opindex)\n"
		"{\n"
  		"  const ISA_PRINT_INFO *info = ISA_PRINT_Info(topcode);\n"
  		"  const char *place_in_format = ISA_PRINT_INFO_Format(info);\n"
  		"  BOOL in_name_part = 0;\n"
  		"  INT comp;\n"
  		"  INT i = 0;\n"
  		"  for (;;) {\n"
  		"  	comp = ISA_PRINT_INFO_Comp(info,i);\n"
  		"  	if (comp == ISA_PRINT_COMP_end) break;\n"
  		"	place_in_format = strchr(place_in_format, '%%');\n"
		"	place_in_format += 2; /* assume %%s */\n"
  		"  	if (comp == ISA_PRINT_COMP_name) {\n"
		"	  if (*place_in_format == '\\0' || *place_in_format == ' ')\n"
		"		in_name_part = 0;\n"
		"	  else\n"
		"		in_name_part = 1;\n"
  		"  	}\n"
  		"  	if (comp >= ISA_PRINT_COMP_opnd && comp < ISA_PRINT_COMP_result) {\n"
  		"  	  if (in_name_part) {\n"
		"	    INT comp_opindex = comp - ISA_PRINT_COMP_opnd;\n"
		"	    if (comp_opindex == opindex)\n"
		"		return 1;\n"
		"	    if (*place_in_format == '\\0' || *place_in_format == ' ')\n"
		"		in_name_part = 0;\n"
  		"  	  }\n"
  		"  	}\n"
  		"  	++i;\n"
  		"  }\n"
  		"  return 0;\n"
		"}\n");

  Emit_Footer (hfile);
}
