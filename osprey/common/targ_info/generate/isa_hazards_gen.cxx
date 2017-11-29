/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


//  isa_hazards_gen.cxx
/////////////////////////////////////
//
//  Description:
//
//      Generate a description of the ISA hazards. 
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_hazards_gen.cxx,v $


#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <list>
#include "topcode.h"
#include "gen_util.h"
#include "targ_isa_subset.h"
#include "isa_hazards_gen.h"
#include "bstring.h"


struct isa_hazard {
  const char *name;         // hazard name
};

struct haz_desc {
  isa_hazard *type;
  int data;
  int pre_ops;
  int post_ops;
  int subsets[ISA_SUBSET_MAX+1];
};

struct op_haz {
  haz_desc *desc;
  struct op_haz *next;
  int index;
};

static std::list<ISA_HAZARD> hazards;    // All the hazards
static op_haz *op_hazards[TOP_count+1];
static std::list<op_haz *> op_hazards_list;
static haz_desc *current_haz_desc;
static int haz_index;

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the ISA hazards. The description exports",
  " *   the following:",
  " *",
  " *   typedef (enum) ISA_HAZARD",
  " *       An enumeration of the hazard types, and ISA_HAZARD_UNDEFINED.",
  " *",
  " *   typedef (struct) ISA_HAZARD_INFO",
  " *       Describes a particular hazard. The contents are private.",
  " *",
  " *   BOOL ISA_HAZARD_TOP_Has_Hazard(TOP topcode)",
  " *       Returns TRUE if the instruction specified by 'topcode'",
  " *       has a hazard.",
  " *",
  " *   ISA_HAZARD_INFO *ISA_HAZARD_First(TOP topcode)",
  " *       Get the first hazard description for 'topcode'.",
  " *",
  " *   ISA_HAZARD_INFO *ISA_HAZARD_Next(ISA_HAZARD_INFO *info)",
  " *       Gets the next hazard description when a 'topcode' has",
  " *       more than one hazard.",
  " *",
  " *   ISA_HAZARD ISA_HAZARD_Type(ISA_HAZARD_INFO *info)",
  " *       Returns the type of the hazard.",
  " *",
  " *   INT ISA_HAZARD_Data(ISA_HAZARD_INFO *info)",
  " *       Returns the hazard specific data.",
  " *",
  " *   INT ISA_HAZARD_Pre_Ops(ISA_HAZARD_INFO *info)",
  " *       Returns the number of OPs that must precede the instruction",
  " *       with the hazard.",
  " *",
  " *   INT ISA_HAZARD_Post_Ops(ISA_HAZARD_INFO *info)",
  " *       Returns the number of OPs that must follow the instruction",
  " *       with the hazard.",
  " *",
  " *   void ISA_HAZARD_Initialize(void)",
  " *       Initializes the hazard description data for ISA_SUBSET_Value."
  " *       This may only be called once (if not called at all the description",
  " *       contains the hazards for all ISAs).",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


/////////////////////////////////////
void ISA_Hazards_Begin( const char* /* name */ )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
}

/////////////////////////////////////
ISA_HAZARD Hazard_Create( const char *name )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_HAZARD result = new isa_hazard;
  memset(result, 0, sizeof(isa_hazard));
  hazards.push_back(result);
  result->name = name;
  return result;
}

/////////////////////////////////////
void Hazard_Group( TOP topcode, ... )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  TOP opcode;
  int count = 0;

  current_haz_desc = new haz_desc;
  memset(current_haz_desc, 0, sizeof(haz_desc));

  va_start(ap,topcode);
  for (opcode = topcode;
       opcode != TOP_UNDEFINED;
       opcode = static_cast<TOP>(va_arg(ap,int))) {
    op_haz *op_hazard = new op_haz;
    op_hazards_list.push_back(op_hazard);
    op_hazard->desc = current_haz_desc;
    op_hazard->next = op_hazards[(int)opcode];
    op_hazard->index = ++haz_index;
    op_hazards[(int)opcode] = op_hazard;
    ++count;
  }
  va_end(ap);

  if (count == 0) {
    fprintf(stderr, "### Warning: hazard group is empty\n");
  }
}


/////////////////////////////////////
void Hazard_Type( ISA_HAZARD isa_hazard )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  current_haz_desc->type = isa_hazard;
}


/////////////////////////////////////
void Hazard_Data( int data )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  current_haz_desc->data = data;
}


/////////////////////////////////////
void Hazard_Post_Ops( int ops )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  current_haz_desc->post_ops = ops;
}


/////////////////////////////////////
void Hazard_Pre_Ops( int ops )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  current_haz_desc->pre_ops = ops;
}


/////////////////////////////////////
void Hazard_ISA( ISA_SUBSET isa )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ((unsigned)isa > (unsigned)ISA_SUBSET_MAX) {
    fprintf(stderr, "### Error: isa value (%d) out of range (%d..%d)\n",
	(int)isa, ISA_SUBSET_MIN, ISA_SUBSET_MAX);
    exit(EXIT_FAILURE);
  }

  current_haz_desc->subsets[(int)isa] = true;
}


/////////////////////////////////////
void ISA_Hazards_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  int top;
  bool first;
  std::list<ISA_HAZARD>::iterator isi;
  std::list<op_haz *>::iterator ophaz_iter;
  const char * const isa_hazard_info_format = 
	"  { ISA_HAZARD_%-9s, %d, %d, %2d, 0x%02x, %d }, /* %2d */\n";

#define FNAME "targ_isa_hazards"
  char filename[1000];
  sprintf(filename,"%s.h",FNAME);
  FILE* hfile = fopen(filename,"w");
  sprintf(filename,"%s.c",FNAME);
  FILE* cfile = fopen(filename,"w");
  sprintf(filename,"%s.Exported",FNAME);
  FILE* efile = fopen(filename,"w");

  fprintf(cfile,"#include \"topcode.h\"\n");
  fprintf(cfile,"#include \"targ_isa_subset.h\"\n");
  fprintf(cfile,"#include \"%s.h\"\n",FNAME);

  sprintf (filename, "%s", FNAME);
  Emit_Header (hfile, filename, interface);
  fprintf(hfile,"#include \"targ_isa_subset.h\"\n");

  fprintf(hfile,"typedef enum {");
  first = true;
  for ( isi = hazards.begin(); isi != hazards.end(); ++isi ) {
    ISA_HAZARD hazard = *isi;
    fprintf(hfile,"%c\n  ISA_HAZARD_%s",first ? ' ' : ',',
                                        hazard->name);
    first = false;
  }
  fprintf(hfile,",\n  ISA_HAZARD_UNDEFINED");
  fprintf(hfile,"\n} ISA_HAZARD;\n");

  fprintf(hfile, "\ntypedef struct {\n"
  		 "  ISA_HAZARD type;\n"
  		 "  mUINT16 data;\n"
  		 "  mUINT16 pre_ops;\n"
  		 "  mUINT16 post_ops;\n"
		 "  mUINT8 isa_mask;\n"
		 "  mUINT8 next;\n"
  		 "} ISA_HAZARD_INFO;\n");

  fprintf(efile, "ISA_HAZARD_hazard_info\n");

  fprintf(cfile, "\nISA_HAZARD_INFO ISA_HAZARD_hazard_info[%d] = {\n", 
	  haz_index + 1);
  fprintf(cfile, isa_hazard_info_format,
	  "UNDEFINED", 0, 0, 0, 0, 0, 0);
  for ( ophaz_iter = op_hazards_list.begin();
	ophaz_iter != op_hazards_list.end();
	++ophaz_iter
  ) {
    int mask;
    ISA_SUBSET subset;
    op_haz *op_hazard = *ophaz_iter;
    haz_desc *haz = op_hazard->desc;
    op_haz *next = op_hazard->next;

    mask = 0;
    for (subset = ISA_SUBSET_MIN;
	 subset <= ISA_SUBSET_MAX; 
	 subset = (ISA_SUBSET)((int)subset + 1)
    ) {
      if ( haz->subsets[(int)subset] ) mask |= 1 << (int)subset;
    }

    fprintf(cfile, isa_hazard_info_format,
	    haz->type->name,
	    haz->data,
	    haz->pre_ops,
	    haz->post_ops,
	    mask,
	    next ? next->index : 0,
	    op_hazard->index);
  }
  fprintf(cfile, "};\n");

  fprintf(efile, "ISA_HAZARD_hazard_index\n");

  fprintf(cfile, "\nmUINT8 ISA_HAZARD_hazard_index[%d] = {\n", TOP_count);
  for ( top = 0; top < TOP_count; ++top ) {
    op_haz *op_hazard = op_hazards[top];
    fprintf(cfile, "  %3d, ", op_hazard ? op_hazard->index : 0);
    fprintf(cfile, "/* %-9s */\n", TOP_Name((TOP)top));
  }
  fprintf(cfile, "};\n");

  fprintf(hfile, "\ninline BOOL ISA_HAZARD_TOP_Has_Hazard(TOP topcode)\n"
		 "{\n"
		 "  extern mUINT8 ISA_HAZARD_hazard_index[%d];\n"
		 "  return ISA_HAZARD_hazard_index[(INT)topcode] != 0;\n"
		 "}\n",
		 TOP_count);

  fprintf(hfile, "\ninline ISA_HAZARD_INFO *ISA_HAZARD_First(TOP topcode)\n"
		 "{\n"
		 "  extern mUINT8 ISA_HAZARD_hazard_index[%d];\n"
		 "  extern ISA_HAZARD_INFO ISA_HAZARD_hazard_info[%d];\n"
		 "  INT index = ISA_HAZARD_hazard_index[(INT)topcode];\n"
		 "  return index ? ISA_HAZARD_hazard_info + index : (ISA_HAZARD_INFO *)0;\n"
		 "}\n",
		 TOP_count,
		 haz_index + 1);

  fprintf(hfile, "\ninline ISA_HAZARD_INFO *ISA_HAZARD_Next(ISA_HAZARD_INFO *info)\n"
		 "{\n"
		 "  extern ISA_HAZARD_INFO ISA_HAZARD_hazard_info[%d];\n"
		 "  INT index = info->next;\n"
		 "  return index ? ISA_HAZARD_hazard_info + index : (ISA_HAZARD_INFO *)0;\n"
		 "}\n",
		 haz_index + 1);

  fprintf(hfile, "\ninline ISA_HAZARD ISA_HAZARD_Type(ISA_HAZARD_INFO *info)\n"
		 "{\n"
		 "  return info->type;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_HAZARD_Data(ISA_HAZARD_INFO *info)\n"
		 "{\n"
		 "  return info->data;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_HAZARD_Pre_Ops(ISA_HAZARD_INFO *info)\n"
		 "{\n"
		 "  return info->pre_ops;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_HAZARD_Post_Ops(ISA_HAZARD_INFO *info)\n"
		 "{\n"
		 "  return info->post_ops;\n"
		 "}\n");

  fprintf(hfile, "\nextern void ISA_HAZARD_Initialize(void);\n");

  fprintf(efile, "ISA_HAZARD_Initialize\n");

  fprintf(cfile, "\nvoid ISA_HAZARD_Initialize(void)\n"
		 "{\n"
		 "  INT top;\n"
		 "  INT mask = 1 << (INT)ISA_SUBSET_Value;\n"
		 "  for ( top = 0; top < TOP_count; ++top ) {\n"
		 "    INT j, k;\n"
		 "    INT i = ISA_HAZARD_hazard_index[top];\n"
		 "    for (j = i; j != 0; j = k) {\n"
		 "      for (k = ISA_HAZARD_hazard_info[j].next;\n"
		 "           k != 0 && (ISA_HAZARD_hazard_info[k].isa_mask & mask) == 0;\n"
		 "           k = ISA_HAZARD_hazard_info[k].next\n"
		 "      );\n"
		 "      ISA_HAZARD_hazard_info[j].next = k;\n"
		 "    }\n"
		 "    if ((ISA_HAZARD_hazard_info[i].isa_mask & mask) == 0) {\n"
		 "      ISA_HAZARD_hazard_index[top] = ISA_HAZARD_hazard_info[i].next;\n"
		 "    }\n"
		 "  }\n"
		 "}\n");

  Emit_Footer (hfile);

  fclose(hfile);
  fclose(efile);
  fclose(cfile);
}
