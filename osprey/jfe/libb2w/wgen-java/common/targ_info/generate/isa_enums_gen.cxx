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


// isa_enums_gen.cxx
/////////////////////////////////////
//
//  Generate a list of enum classes and their values.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_enums_gen.cxx,v $

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <vector>
#include "gen_util.h"
#include "isa_enums_gen.h"

typedef struct {
	const char *ecv_ecname;
	const char *ecv_name;
	int ecv_int;
} ECV_struct;

typedef struct {
	const char *ec_name;
	int first_ecv;
	int last_ecv;
} EC_struct;

static std::vector<ECV_struct> all_ecv;
static std::vector<EC_struct> all_ec;

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A list of all the enum classes used in an ISA.",
  " *   It exports the following:",
  " *",
  " *   typedef (enum) ISA_ENUM_CLASS",
  " *       An enumeration of the enum classes.",
  " *",
  " *   typedef (enum) ISA_ENUM_CLASS_VALUE",
  " *       An enumeration of the enum class values.",
  " *",
  " *   typedef (struct) ISA_ENUM_CLASS_INFO",
  " *       Contains info about first and last ECV in the EC.",
  " *       The contents are private.",
  " *",
  " *   typedef (struct) ISA_ENUM_CLASS_VALUE_INFO",
  " *       Contains info about name and int-value of the ECV.",
  " *       The contents are private.",
  " *",
  " *   const char * ISA_EC_Name (ISA_ENUM_CLASS)",
  " *       Returns name of EC.",
  " *",
  " *   ISA_ENUM_CLASS_VALUE ISA_EC_First_Value (ISA_ENUM_CLASS)",
  " *       Returns the first ECV for the specified EC.",
  " *",
  " *   ISA_ENUM_CLASS_VALUE ISA_EC_Last_Value (ISA_ENUM_CLASS)",
  " *       Returns the last ECV for the specified EC.",
  " *       Note that it assumes all ECV for an EC are in the",
  " *       first/last range given by the above two functions.",
  " *",
  " *   const char * ISA_ECV_Name (ISA_ENUM_CLASS_VALUE)",
  " *       Returns name of ECV.",
  " *",
  " *   INT ISA_ECV_Intval (ISA_ENUM_CLASS_VALUE)",
  " *       Returns int-value of ECV.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};

/////////////////////////////////////
void ISA_Enums_Begin (void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  // start with undefined value
  ECV_struct current_ecv = {"","UNDEFINED",UNDEFINED};
  all_ecv.push_back (current_ecv);
  EC_struct current_ec = {"UNDEFINED",0,0};
  all_ec.push_back (current_ec);
}

/////////////////////////////////////
void ISA_Create_Enum_Class ( const char* name, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  const char *ecv_name;
  va_list ap;
  EC_struct current_ec;
  ECV_struct current_ecv;
  current_ec.ec_name = name;
  current_ec.first_ecv = all_ecv.size();
  va_start(ap, name);
  do {	// loop through ecvs
    ecv_name = va_arg(ap, char*);
    current_ecv.ecv_ecname = name;
    current_ecv.ecv_name = ecv_name ? ecv_name : "";
    current_ecv.ecv_int = va_arg(ap, int);
    if (current_ecv.ecv_int == UNDEFINED)
	break;	// no default value
    all_ecv.push_back (current_ecv);
  } while (ecv_name != NULL);
  va_end(ap);
  current_ec.last_ecv = all_ecv.size() - 1;
  all_ec.push_back (current_ec);
}


static char*
Print_ECV_EName (const char *name)
{
  // will print direct to file, so can use temp buffers
  static char buf[80];
  char *p = (char*) name;
  int i = 0;
  if (name == NULL)
    return "_none";
  else if (name[0] != '\0' && name[0] != '.' && name[0] != '_') {
    // insert leading _
    buf[0] = '_';
    ++i;
  }
  for ( ; *p != '\0'; ++p) {
    switch (*p) {
    case '.':
      buf[i++] = '_';
      break;
    case '@':
      // remove from name
      break;
    default:
      buf[i++] = *p;
      break;
    }
  }
  buf[i] = '\0';
  return buf;
}

/////////////////////////////////////
void ISA_Enums_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::vector<EC_struct>::iterator iec;
  std::vector<ECV_struct>::iterator iecv;
  ECV_struct tecv;

#define FNAME "targ_isa_enums"
  char buf[1000];
  sprintf (buf, "%s.h", FNAME);
  FILE* hfile = fopen(buf, "w");
  sprintf (buf, "%s.c", FNAME);
  FILE* cfile = fopen(buf, "w");
  sprintf (buf, "%s.Exported", FNAME);
  FILE* efile = fopen(buf, "w");

  fprintf(cfile,"#include \"%s.h\"\n\n", FNAME);

  sprintf (buf, "%s", FNAME);
  Emit_Header (hfile, buf, interface);

  fprintf(hfile, "\ntypedef enum {\n");
  for ( iec = all_ec.begin(); iec != all_ec.end(); ++iec) {
  	fprintf(hfile, "\tEC%s,\n", Print_ECV_EName(iec->ec_name));
  }
  fprintf(hfile, "\tEC_MAX\n");
  fprintf(hfile, "} ISA_ENUM_CLASS;\n");
  fprintf(hfile, "\ntypedef enum {\n");
  for ( iecv = all_ecv.begin(); iecv != all_ecv.end(); ++iecv) {
	// have to use multiple calls since Print_ECV_EName uses a static bufr
  	fprintf(hfile, "\tECV%s", Print_ECV_EName (iecv->ecv_ecname));
  	fprintf(hfile, "%s,\n", Print_ECV_EName (iecv->ecv_name));
  }
  fprintf(hfile, "\tECV_MAX\n");
  fprintf(hfile, "} ISA_ENUM_CLASS_VALUE;\n");

  fprintf(hfile, "\ntypedef struct {\n"
		"  char *name;\n"
		"  ISA_ENUM_CLASS_VALUE first;\n"
		"  ISA_ENUM_CLASS_VALUE last;\n"
		"} ISA_ENUM_CLASS_INFO;\n");
  fprintf(hfile, "extern const ISA_ENUM_CLASS_INFO ISA_ENUM_CLASS_info[];\n");
  fprintf(efile, "ISA_ENUM_CLASS_info\n");
  fprintf(cfile, "const ISA_ENUM_CLASS_INFO ISA_ENUM_CLASS_info[] = {\n");
  for ( iec = all_ec.begin(); iec != all_ec.end(); ++iec) {
  	fprintf(cfile, "\t{ \"EC%s\",", Print_ECV_EName(iec->ec_name));
	tecv = all_ecv[iec->first_ecv];
	// have to use multiple calls since Print_ECV_EName uses a static bufr
  	fprintf(cfile, "\tECV%s", Print_ECV_EName(tecv.ecv_ecname));
  	fprintf(cfile, "%s,", Print_ECV_EName(tecv.ecv_name));
	tecv = all_ecv[iec->last_ecv];
	// have to use multiple calls since Print_ECV_EName uses a static bufr
  	fprintf(cfile, "\tECV%s", Print_ECV_EName(tecv.ecv_ecname));
  	fprintf(cfile, "%s },\n", Print_ECV_EName(tecv.ecv_name));
  }
  fprintf(cfile, "};\n\n");

  fprintf(hfile, "\ntypedef struct {\n"
		"  char *name;\n"
		"  INT intval;\n"
		"} ISA_ENUM_CLASS_VALUE_INFO;\n");
  fprintf(hfile, "extern const ISA_ENUM_CLASS_VALUE_INFO ISA_ENUM_CLASS_VALUE_info[];\n\n");
  fprintf(efile, "ISA_ENUM_CLASS_VALUE_info\n");
  fprintf(cfile, "const ISA_ENUM_CLASS_VALUE_INFO ISA_ENUM_CLASS_VALUE_info[] = {\n");
  for ( iecv = all_ecv.begin(); iecv != all_ecv.end(); ++iecv) {
  	fprintf(cfile, "\t{ \"%s\",\t%d },\n", iecv->ecv_name, iecv->ecv_int);
  }
  fprintf(cfile, "};\n\n");

  fprintf(hfile, "inline const char * ISA_EC_Name (ISA_ENUM_CLASS ec)\n"
		 "{\n"
		 "  return ISA_ENUM_CLASS_info[ec].name;\n"
		 "}\n\n");

  fprintf(hfile, "inline ISA_ENUM_CLASS_VALUE ISA_EC_First_Value (ISA_ENUM_CLASS ec)\n"
		 "{\n"
		 "  return ISA_ENUM_CLASS_info[ec].first;\n"
		 "}\n\n");

  fprintf(hfile, "inline ISA_ENUM_CLASS_VALUE ISA_EC_Last_Value (ISA_ENUM_CLASS ec)\n"
		 "{\n"
		 "  return ISA_ENUM_CLASS_info[ec].last;\n"
		 "}\n\n");

  fprintf(hfile, "inline const char * ISA_ECV_Name (ISA_ENUM_CLASS_VALUE ecv)\n"
		 "{\n"
		 "  return ISA_ENUM_CLASS_VALUE_info[ecv].name;\n"
		 "}\n\n");

  fprintf(hfile, "inline INT ISA_ECV_Intval (ISA_ENUM_CLASS_VALUE ecv)\n"
		 "{\n"
		 "  return ISA_ENUM_CLASS_VALUE_info[ecv].intval;\n"
		 "}\n\n");

  Emit_Footer (hfile);
}
