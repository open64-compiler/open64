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


//  isa_registers_gen.cxx
/////////////////////////////////////
//
//  Description:
//
//      Generate a description of the ISA registers.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_registers_gen.cxx,v $


#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <list>
#include "gen_util.h"
#include "targ_isa_subset.h"
#include "isa_registers_gen.h"

typedef struct isa_register_set {
  int isa_mask;
  int min_regnum;
  int max_regnum;
  const char *def_name_format;
  const char **names;
} *ISA_REGISTER_SET;

typedef struct isa_register_subclass {
  const char *name;
  ISA_REGISTER_CLASS rclass;
  int count;
  const int *members;
  const char **names;
} *ISA_REGISTER_SUBCLASS;

struct isa_register_class {
  const char *name;
  int bit_size;
  bool can_store;
  bool multiple_save;
  int min_reg;
  int max_reg;
  std::list<ISA_REGISTER_SET> regsets;
  std::list<ISA_REGISTER_SUBCLASS> subclasses;
};

static std::list<ISA_REGISTER_CLASS> rclasses; // All the classes
static std::list<ISA_REGISTER_SUBCLASS> subclasses; // All the sub classes

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the ISA registers. The description exports",
  " *   the following:",
  " *",
  " *   typedef (enum) ISA_REGISTER_CLASS",
  " *       An enumeration of the register classes.",
  " *",
  " *   typedef mISA_REGISTER_CLASS",
  " *       The most compact (integral) representation that can hold",
  " *       all values of ISA_REGISTER_CLASS",
  " *",
  " *   typedef (struct) ISA_REGISTER_CLASS_INFO",
  " *       Describes a particular register class. The contents are private.",
  " *",
  " *   const INT ISA_REGISTER_CLASS_UNDEFINED",
  " *       A special register class that is out-of-range of valid",
  " *       register clases.",
  " *",
  " *   const INT ISA_REGISTER_CLASS_MIN",
  " *       The first register class. The range of register classes",
  " *       is ISA_REGISTER_CLASS_MIN..ISA_REGISTER_CLASS_MAX",
  " *	    (this range excludes ISA_REGISTER_CLASS_UNDEFINED)."
  " * ",
  " *   const INT ISA_REGISTER_CLASS_MAX",
  " *       The last register class. The range of register classes",
  " *       is ISA_REGISTER_CLASS_MIN..ISA_REGISTER_CLASS_MAX",
  " *	    (this range excludes ISA_REGISTER_CLASS_UNDEFINED)."
  " * ",
  " *   const INT ISA_REGISTER_CLASS_COUNT",
  " *       The number of register classes. The range of register classes",
  " *       is ISA_REGISTER_CLASS_MIN..ISA_REGISTER_CLASS_MAX",
  " *	    (this range excludes ISA_REGISTER_CLASS_UNDEFINED)."
  " * ",
  " *   (macro) FOR_ALL_ISA_REGISTER_CLASS(cl)",
  " *       Iterate over all the register class values using the",
  " *       ISA_REGISTER_CLASS variable <cl>.",
  " *",
  " *   (macro) FOR_ALL_ISA_REGISTER_CLASS_IN_REVERSE(cl)",
  " *       Iterate over all the register class values in reverse order using",
  " *       the ISA_REGISTER_CLASS variable <cl>.",
  " *",
  " *   const INT ISA_REGISTER_MAX",
  " *       The maximum (highest) register number of all classes.",
  " *       NOTE: the lowest number register is implicitly 0.",
  " *",
  " *   typedef (enum) ISA_REGISTER_SUBCLASS",
  " *       An enumeration of the register subclasses.",
  " *",
  " *   typedef mISA_REGISTER_SUBCLASS",
  " *       The most compact (integral) representation that can hold",
  " *       all values of ISA_REGISTER_SUBCLASS",
  " *",
  " *   typedef (struct) ISA_REGISTER_SUBCLASS_INFO",
  " *       Describes a particular register subclass. The contents are private.",
  " *",
  " *   const INT ISA_REGISTER_SUBCLASS_UNDEFINED",
  " *       A special register subclass that is out-of-range of valid",
  " *       register subclases.",
  " *",
  " *   const INT ISA_REGISTER_SUBCLASS_MIN",
  " *       The first register subclass. The range of register subclasses",
  " *       is ISA_REGISTER_SUBCLASS_MIN..ISA_REGISTER_SUBCLASS_MAX",
  " * ",
  " *   const INT ISA_REGISTER_SUBCLASS_MAX",
  " *       The last register subclass. The range of register subclasses",
  " *       is ISA_REGISTER_SUBCLASS_MIN..ISA_REGISTER_SUBCLASS_MAX",
  " * ",
  " *   const INT ISA_REGISTER_SUBCLASS_COUNT",
  " *       The number of register subclasses.",
  " * ",
  " *   (macro) FOR_ALL_ISA_REGISTER_SUBCLASS(sc)",
  " *       Iterate over all the register subclass values using the",
  " *       the ISA_REGISTER_SUBCLASS variable <sc>.",
  " *",
  " *   const ISA_REGISTER_CLASS_INFO *ISA_REGISTER_CLASS_Info(",
  " *     ISA_REGISTER_CLASS rc",
  " *   )",
  " *       Return a pointer to the register class info for class 'rc'.",
  " *",
  " *   INT ISA_REGISTER_CLASS_INFO_First_Reg(",
  " *     const ISA_REGISTER_CLASS_INFO *info",
  " *   )",
  " *       Get the first (lowest numbered) register for the class",
  " *       described by 'info'.",
  " *",
  " *   INT ISA_REGISTER_CLASS_INFO_Last_Reg(",
  " *     const ISA_REGISTER_CLASS_INFO *info",
  " *   )",
  " *       Get the last (highest numbered) register for the class",
  " *       described by 'info'.",
  " *",
  " *   INT ISA_REGISTER_CLASS_INFO_Bit_Size(",
  " *     const ISA_REGISTER_CLASS_INFO *info",
  " *   )",
  " *       Get the size, in bits, of the register in the class",
  " *       described by 'info'.",
  " *",
  " *   BOOL ISA_REGISTER_CLASS_INFO_Can_Store(",
  " *     const ISA_REGISTER_CLASS_INFO *info",
  " *   )",
  " *       Return a flag that indicates if the registers in the class",
  " *       described by 'info' can be stored to memory, i.e. there",
  " *       is a store instruction for the registers in the class.",
  " *",
  " *   BOOL ISA_REGISTER_CLASS_INFO_Multiple_Save(",
  " *     const ISA_REGISTER_CLASS_INFO *info",
  " *   )",
  " *       Return a flag that indicates if the registers in the class",
  " *       described by 'info' can be saved and restore to memory in",
  " *       multiples, i.e. as a group.",
  " *",
  " *   const char *ISA_REGISTER_CLASS_INFO_Name(",
  " *     const ISA_REGISTER_CLASS_INFO *info",
  " *   )",
  " *       Return the name of the class described by 'info'.",
  " *",
  " *   const char *ISA_REGISTER_CLASS_INFO_Reg_Name(",
  " *     const ISA_REGISTER_CLASS_INFO *info,",
  " *     INT reg_index",
  " *   )",
  " *       Return the name of the 'reg_index'th register in the",
  " *       class described by 'info'. NOTE: reg_index==0 corresponds",
  " *       to the first register of the class.",
  " *",
  " *   const ISA_REGISTER_SUBCLASS_INFO *ISA_REGISTER_SUBCLASS_Info(",
  " *     ISA_REGISTER_SUBCLASS sc",
  " *   )",
  " *",
  " *       Return a pointer to the register subclass info for the",
  " *       subclass 'sc'.",
  " *",
  " *   const char *ISA_REGISTER_SUBCLASS_INFO_Name(",
  " *     const ISA_REGISTER_SUBCLASS_INFO *info",
  " *   )",
  " *",
  " *       Return the name of the subclass described by 'info'.",
  " *",
  " *   ISA_REGISTER_CLASS ISA_REGISTER_SUBCLASS_INFO_Class(",
  " *     const ISA_REGISTER_SUBCLASS_INFO *info",
  " *   )",
  " *",
  " *       Return the base register class for the subclass described",
  " *       by 'info'.",
  " *",
  " *   INT ISA_REGISTER_SUBCLASS_INFO_Count(",
  " *     const ISA_REGISTER_SUBCLASS_INFO *info",
  " *   )",
  " *",
  " *       Return the number of registers in the subclass described",
  " *       by 'info'.",
  " *",
  " *   UINT ISA_REGISTER_SUBCLASS_INFO_Member(",
  " *     const ISA_REGISTER_SUBCLASS_INFO *info,",
  " *     INT n",
  " *   )",
  " *",
  " *       Return the 'n'th member (register) of the subclass described",
  " *       by 'info'. The order of the registers returned is arbitrary.",
  " *",
  " *   const char *ISA_REGISTER_SUBCLASS_INFO_Reg_Name(",
  " *     const ISA_REGISTER_SUBCLASS_INFO *info,",
  " *     INT index",
  " *   )",
  " *",
  " *       Return the 'n'th member's register name of the subclass",
  " *       described by 'info'. If the member does not have a subclass",
  " *       specific name, NULL is returned.",
  " *",
  " *   void ISA_REGISTER_Initialize(void)",
  " *       Initialize the register package for use with the ISA specified",
  " *       by ISA_SUBSET_Value.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


/////////////////////////////////////
void ISA_Registers_Begin( const char* /* name */ )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
}


/////////////////////////////////////
ISA_REGISTER_CLASS ISA_Register_Class_Create(
  const char *name,
  int bit_size,
  bool can_store,
  bool multiple_save
)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGISTER_CLASS result = new isa_register_class;
  rclasses.push_back(result);
  result->name = name;
  result->bit_size = bit_size;
  result->can_store = can_store;
  result->multiple_save = multiple_save;
  return result;
}


/////////////////////////////////////
void ISA_Register_Set(
  ISA_REGISTER_CLASS rclass,
  int min_regnum,
  int max_regnum,
  const char *def_name_format,
  const char **names,
  int isa_mask
)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGISTER_SET regset = new isa_register_set;

  regset->min_regnum = min_regnum;
  regset->max_regnum = max_regnum;
  regset->def_name_format = def_name_format;
  regset->names = names;
  regset->isa_mask = isa_mask;
  rclass->regsets.push_back(regset);
}


/////////////////////////////////////
void ISA_Register_Subclass_Create(
  const char *name,
  ISA_REGISTER_CLASS rclass,
  int count,
  const int *members,
  const char **names
)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGISTER_SUBCLASS result = new isa_register_subclass;
  subclasses.push_back(result);
  result->name = name;
  result->rclass = rclass;
  result->count = count;
  result->members = members;
  result->names = names;
  rclass->subclasses.push_back(result);
}


/////////////////////////////////////
void ISA_Registers_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::list <ISA_REGISTER_CLASS>::iterator rc_iter;
  std::list <ISA_REGISTER_SUBCLASS>::iterator rsc_iter;
  int i;

  int max_reg = 0;
  int first_reg = max_reg;
  for (rc_iter = rclasses.begin(); rc_iter != rclasses.end(); ++rc_iter) {
    ISA_REGISTER_CLASS rclass = *rc_iter;
    int class_max = 0;
    int class_min = 0;
    std::list<ISA_REGISTER_SET>::iterator reg_iter;
    for (reg_iter = rclass->regsets.begin();
	 reg_iter != rclass->regsets.end();
	 ++reg_iter
    ) {
      ISA_REGISTER_SET regset = *reg_iter;
      int this_max = regset->max_regnum;
      if (this_max > class_max) class_max = this_max;

      int this_min = regset->min_regnum;
      if (this_min > class_min) class_min = this_min;
    }
    rclass->max_reg = class_max;
    rclass->min_reg = class_min;
    if (class_max > max_reg) max_reg = class_max;
  }

#define FNAME "targ_isa_registers"
  char filename[1000];
  sprintf(filename,"%s.h",FNAME);
  FILE* hfile = fopen(filename,"w");
  sprintf(filename,"%s.c",FNAME);
  FILE* cfile = fopen(filename,"w");
  sprintf(filename,"%s.Exported",FNAME);
  FILE* efile = fopen(filename,"w");

  fprintf(cfile,"#include \"targ_isa_subset.h\"\n");
  fprintf(cfile,"#include \"%s.h\"\n",FNAME);

  sprintf (filename, "%s", FNAME);
  Emit_Header (hfile, filename, interface);
  fprintf(hfile,"#include \"targ_isa_subset.h\"\n");

  fprintf(hfile, "\n#define ISA_REGISTER_FIRST (%d)\n", first_reg);  
  fprintf(hfile, "\n#define ISA_REGISTER_MAX (%d)\n", max_reg);

  /**************************************************
   * classes:
   */

  fprintf(hfile, "\ntypedef enum {\n");
  fprintf(hfile, "  ISA_REGISTER_CLASS_UNDEFINED,\n");
  for (rc_iter = rclasses.begin(); rc_iter != rclasses.end(); ++rc_iter) {
    ISA_REGISTER_CLASS rclass = *rc_iter;
    fprintf(hfile, "  ISA_REGISTER_CLASS_%s,\n", rclass->name);
  }
  fprintf(hfile, "  ISA_REGISTER_CLASS_MIN = ISA_REGISTER_CLASS_%s,\n",
	  rclasses.front()->name);
  fprintf(hfile, "  ISA_REGISTER_CLASS_MAX = ISA_REGISTER_CLASS_%s,\n",
	  rclasses.back()->name);
  fprintf(hfile, "  ISA_REGISTER_CLASS_COUNT = "
		 "ISA_REGISTER_CLASS_MAX - ISA_REGISTER_CLASS_MIN + 1\n");

  fprintf(hfile, "} ISA_REGISTER_CLASS;\n");

  fprintf(hfile, "\ntypedef mUINT8 mISA_REGISTER_CLASS;\n");

  fprintf(hfile, "\n#define FOR_ALL_ISA_REGISTER_CLASS(cl) \\\n"
		 "\tfor (cl = ISA_REGISTER_CLASS_MIN; \\\n"
		 "\t     cl <= ISA_REGISTER_CLASS_MAX; \\\n"
		 "\t     cl = (ISA_REGISTER_CLASS)(cl + 1))\n");

  fprintf(hfile, "\n#define FOR_ALL_ISA_REGISTER_CLASS_IN_REVERSE(cl) \\\n"
		 "\tfor (cl = ISA_REGISTER_CLASS_MAX; \\\n"
		 "\t     cl >= ISA_REGISTER_CLASS_MIN; \\\n"
		 "\t     cl = (ISA_REGISTER_CLASS)(cl - 1))\n");

  fprintf(hfile, "\ntypedef struct {\n"
		 "  mUINT8 isa_mask;\n"
		 "  mUINT8 min_regnum;\n"
		 "  mUINT8 max_regnum;\n"
		 "  mUINT8 bit_size;\n"
		 "  mBOOL can_store;\n"
		 "  mBOOL multiple_save;\n"
		 "  const char *name;\n"
		 "  const char *reg_name[ISA_REGISTER_MAX+1];\n"
  		 "} ISA_REGISTER_CLASS_INFO;\n");

  fprintf(efile, "ISA_REGISTER_CLASS_info\n");

  fprintf(cfile, "\nconst ISA_REGISTER_CLASS_INFO"
		   " ISA_REGISTER_CLASS_info[] = {\n");
  fprintf(cfile, "  { 0x%02x, %3d, %3d, %2d, %1d, %1d, \"%s\", { 0 } },\n",
		 0, 0, -1, 0, 0, 0, "UNDEFINED");
  for (rc_iter = rclasses.begin(); rc_iter != rclasses.end(); ++rc_iter) {
    ISA_REGISTER_CLASS rclass = *rc_iter;
    std::list<ISA_REGISTER_SET>::iterator reg_iter;
    for (reg_iter = rclass->regsets.begin();
	 reg_iter != rclass->regsets.end();
	 ++reg_iter
    ) {
      ISA_REGISTER_SET regset = *reg_iter;
      fprintf(cfile, "  { 0x%02x, %3d, %3d, %2d, %1d, %1d, \"%s\",",
	      regset->isa_mask,
	      regset->min_regnum,
	      regset->max_regnum,
	      rclass->bit_size,
	      rclass->can_store,
	      rclass->multiple_save,
	      rclass->name);

      int len = fprintf(cfile, "\n    { ");
      for (i = regset->min_regnum; i <= regset->max_regnum; ++i) {
	if (len > 70) len = fprintf(cfile, "\n      ");
        len += fprintf(cfile, "\"");
	if (regset->names && regset->names[i - regset->min_regnum]) {
	  len += fputs(regset->names[i - regset->min_regnum], cfile);
	} else {
	  len += fprintf(cfile, regset->def_name_format, i);
	}
	len += fprintf(cfile, "\"%s", i != regset->max_regnum ? ", " : "");
      }
      fprintf(cfile, " } },\n");
    }
  }
  fprintf(cfile, "};\n");

  fprintf(efile, "ISA_REGISTER_CLASS_info_index\n");

  fprintf(cfile, "\nmUINT8 ISA_REGISTER_CLASS_info_index[] = {\n");
  fprintf(cfile, "  %d,  /* ISA_REGISTER_CLASS_%s */\n", 0, "UNDEFINED");
  int index = 1;
  for (rc_iter = rclasses.begin(); rc_iter != rclasses.end(); ++rc_iter) {
    ISA_REGISTER_CLASS rclass = *rc_iter;
    std::list<ISA_REGISTER_SET>::iterator reg_iter;
    fprintf(cfile, "  %d,  /* ISA_REGISTER_CLASS_%s */\n", index, rclass->name);
    for (reg_iter = rclass->regsets.begin();
	 reg_iter != rclass->regsets.end();
	 ++reg_iter
    ) ++index;
  };
  fprintf(cfile, "};\n");

  /**************************************************
   * subclasses:
   */

  fprintf(hfile, "\ntypedef enum {\n");
  fprintf(hfile, "  ISA_REGISTER_SUBCLASS_UNDEFINED,\n");
  for (rsc_iter = subclasses.begin(); rsc_iter != subclasses.end(); ++rsc_iter) {
    ISA_REGISTER_SUBCLASS subclass = *rsc_iter;
      fprintf(hfile, "  ISA_REGISTER_SUBCLASS_%s,\n", subclass->name);
    }
  if (subclasses.empty()) {
    fprintf(hfile, "  ISA_REGISTER_SUBCLASS_MIN = 1,\n");
    fprintf(hfile, "  ISA_REGISTER_SUBCLASS_MAX = 0,\n");
  } else {
    fprintf(hfile, "  ISA_REGISTER_SUBCLASS_MIN = ISA_REGISTER_SUBCLASS_%s,\n",
	    subclasses.front()->name);
    fprintf(hfile, "  ISA_REGISTER_SUBCLASS_MAX = ISA_REGISTER_SUBCLASS_%s,\n",
	    subclasses.back()->name);
  }
  fprintf(hfile, "  ISA_REGISTER_SUBCLASS_COUNT = "
		 "ISA_REGISTER_SUBCLASS_MAX - ISA_REGISTER_SUBCLASS_MIN + 1\n");

  fprintf(hfile, "} ISA_REGISTER_SUBCLASS;\n");

  fprintf(hfile, "\ntypedef mUINT8 mISA_REGISTER_SUBCLASS;\n");

  fprintf(hfile, "\n#define FOR_ALL_ISA_REGISTER_SUBCLASS(sc) \\\n"
		 "\tfor (sc = ISA_REGISTER_SUBCLASS_MIN; \\\n"
		 "\t     sc <= ISA_REGISTER_SUBCLASS_MAX; \\\n"
		 "\t     sc = (ISA_REGISTER_SUBCLASS)(sc + 1))\n");

  fprintf(hfile, "\ntypedef struct {\n"
		 "  const char *name;\n"
		 "  mISA_REGISTER_CLASS rclass;\n"
		 "  mUINT8 count;\n"
		 "  mUINT8 members[ISA_REGISTER_MAX+1];\n"
		 "  const char *reg_name[ISA_REGISTER_MAX+1];\n"
  		 "} ISA_REGISTER_SUBCLASS_INFO;\n");

  fprintf(efile, "ISA_REGISTER_SUBCLASS_info\n");

  fprintf(cfile, "\nconst ISA_REGISTER_SUBCLASS_INFO"
		   " ISA_REGISTER_SUBCLASS_info[] = {\n");
  fprintf(cfile, "  { \"%s\", ISA_REGISTER_CLASS_%s, 0, { 0 }, { 0 } },\n", 
		 "UNDEFINED", "UNDEFINED");
  for (rsc_iter = subclasses.begin(); rsc_iter != subclasses.end(); ++rsc_iter) {
    ISA_REGISTER_SUBCLASS subclass = *rsc_iter;
    fprintf(cfile, "  { \"%s\", ISA_REGISTER_CLASS_%s, %d,", 
		   subclass->name, subclass->rclass->name, subclass->count);
    int len = fprintf(cfile, "\n    { ");
    for (i = 0; i < subclass->count; ++i) {
      if (len > 70) len = fprintf(cfile, "\n      ");
      len += fprintf(cfile, "%d%s",
			    subclass->members[i],
			    i != (subclass->count - 1) ? ", " : "");
    }
    fprintf(cfile, " },\n");

    len = fprintf(cfile, "    { ");
    if (subclass->names) {
      for (i = 0; i < subclass->count; ++i) {
	if (len > 70) len = fprintf(cfile, "\n      ");
	if (subclass->names[i]) {
	  len += fprintf(cfile, "\"%s\"", subclass->names[i]);
	} else {
	  len += fputs("0", cfile);
	}
	len += fprintf(cfile, "%s", i != (subclass->count - 1) ? ", " : "");
      }
    } else {
      fputs("0", cfile);
    }
    fprintf(cfile, " } },\n");
  }
  fprintf(cfile, "};\n");

  /**************************************************
   * accessors:
   */

  fprintf(hfile, "\ninline const ISA_REGISTER_CLASS_INFO *ISA_REGISTER_CLASS_Info(\n"
		 "  ISA_REGISTER_CLASS rc\n"
		 ")\n"
		 "{\n"
		 "  extern const ISA_REGISTER_CLASS_INFO ISA_REGISTER_CLASS_info[];\n"
		 "  extern mUINT8 ISA_REGISTER_CLASS_info_index[];\n"
		 "  INT index = ISA_REGISTER_CLASS_info_index[(INT)rc];\n"
		 "  return &ISA_REGISTER_CLASS_info[index];\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_REGISTER_CLASS_INFO_First_Reg(\n"
		 "  const ISA_REGISTER_CLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->min_regnum;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_REGISTER_CLASS_INFO_Last_Reg(\n"
		 "  const ISA_REGISTER_CLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->max_regnum;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_REGISTER_CLASS_INFO_Bit_Size(\n"
		 "  const ISA_REGISTER_CLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->bit_size;\n"
		 "}\n");

  fprintf(hfile, "\ninline BOOL ISA_REGISTER_CLASS_INFO_Can_Store(\n"
		 "  const ISA_REGISTER_CLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->can_store;\n"
		 "}\n");

  fprintf(hfile, "\ninline BOOL ISA_REGISTER_CLASS_INFO_Multiple_Save(\n"
		 "  const ISA_REGISTER_CLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->multiple_save;\n"
		 "}\n");

  fprintf(hfile, "\ninline const char *ISA_REGISTER_CLASS_INFO_Name(\n"
		 "  const ISA_REGISTER_CLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->name;\n"
		 "}\n");

  fprintf(hfile, "\ninline const char *ISA_REGISTER_CLASS_INFO_Reg_Name(\n"
		 "  const ISA_REGISTER_CLASS_INFO *info,\n"
		 "  INT reg_index\n"
		 ")\n"
		 "{\n"
		 "  return info->reg_name[reg_index];\n"
		 "}\n");

  fprintf(hfile, "\ninline const ISA_REGISTER_SUBCLASS_INFO *ISA_REGISTER_SUBCLASS_Info(\n"
		 "  ISA_REGISTER_SUBCLASS sc\n"
		 ")\n"
		 "{\n"
		 "  extern const ISA_REGISTER_SUBCLASS_INFO ISA_REGISTER_SUBCLASS_info[];\n"
		 "  return &ISA_REGISTER_SUBCLASS_info[sc];\n"
		 "}\n");

  fprintf(hfile, "\ninline const char *ISA_REGISTER_SUBCLASS_INFO_Name(\n"
		 "  const ISA_REGISTER_SUBCLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->name;\n"
		 "}\n");

  fprintf(hfile, "\ninline ISA_REGISTER_CLASS ISA_REGISTER_SUBCLASS_INFO_Class(\n"
		 "  const ISA_REGISTER_SUBCLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return (ISA_REGISTER_CLASS)info->rclass;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_REGISTER_SUBCLASS_INFO_Count(\n"
		 "  const ISA_REGISTER_SUBCLASS_INFO *info\n"
		 ")\n"
		 "{\n"
		 "  return info->count;\n"
		 "}\n");

  fprintf(hfile, "\ninline UINT ISA_REGISTER_SUBCLASS_INFO_Member(\n"
		 "  const ISA_REGISTER_SUBCLASS_INFO *info,\n"
		 "  INT n\n"
		 ")\n"
		 "{\n"
		 "  return info->members[n];\n"
		 "}\n");

  fprintf(hfile, "\ninline const char *ISA_REGISTER_SUBCLASS_INFO_Reg_Name(\n"
		 "  const ISA_REGISTER_SUBCLASS_INFO *info,\n"
		 "  INT n\n"
		 ")\n"
		 "{\n"
		 "  return info->reg_name[n];\n"
		 "}\n");

  fprintf(hfile, "\nextern void ISA_REGISTER_Initialize(void);\n");

  fprintf(efile, "ISA_REGISTER_Initialize\n");

  fprintf(cfile, "\nvoid ISA_REGISTER_Initialize(void)\n"
		 "{\n"
		 "  INT rc;\n"
		 "  INT mask = 1 << (INT)ISA_SUBSET_Value;\n"
		 "  for (rc = ISA_REGISTER_CLASS_MIN; "
			 "rc <= ISA_REGISTER_CLASS_MAX; ++rc) {\n"
		 "    INT i = ISA_REGISTER_CLASS_info_index[rc];\n"
		 "    const ISA_REGISTER_CLASS_INFO *info = "
			    "&ISA_REGISTER_CLASS_info[i];\n"
		 "    while ((info->isa_mask & mask) == 0) ++info, ++i;\n"
		 "    ISA_REGISTER_CLASS_info_index[rc] = i;\n"
		 "  }\n"
		 "}\n");

  Emit_Footer(hfile);
}
