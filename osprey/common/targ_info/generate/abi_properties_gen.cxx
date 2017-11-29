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


//  abi_properties_gen.cxx
/////////////////////////////////////
//
//  Description:
//
//      Generate a description of the ABI properties.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/abi_properties_gen.cxx,v $

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <list>
#include "gen_util.h"
#include "targ_isa_registers.h"
#include "abi_properties_gen.h"
#include "bstring.h"

//
// Information about a property
//
struct abi_property {
  const char* name;		// Name given for documentation and debugging
  bool single_class;		// whether only has one regclass
  bool single_reg;		// whether only has one reg (per class)
  ISA_REGISTER_CLASS rc;	// first register class
  INT reg[ISA_REGISTER_CLASS_MAX+1]; // first register in class
  unsigned long long val;	// unique bit value of property
};

// If more than NUM_REGISTERS_LIMIT, then don't want to create static arrays
// of register data, as cygwin compiler complains; instead create sparse list
// of modified values.
#define NUM_REGISTERS_LIMIT 1000

typedef struct {
  ISA_REGISTER_CLASS rc;
  INT reg;
  const char *name;
} REG_NAME;

typedef struct {
  ISA_REGISTER_CLASS rc;
  // because properties often cover a large range (e.g. allocatable),
  // just use one info record for entire range.
  INT minreg;	
  INT maxreg;
  ABI_PROPERTY aprop;
} ABI_PROPERTY_INFO;

//
// Information about an ABI
//
typedef struct abi {
  const char *name;		// Name
#if ISA_REGISTER_MAX < NUM_REGISTERS_LIMIT
  std::list<ABI_PROPERTY> reg_flags[ISA_REGISTER_CLASS_MAX+1][ISA_REGISTER_MAX+1];
				// Register flag properties
  const char *reg_names[ISA_REGISTER_CLASS_MAX+1][ISA_REGISTER_MAX+1];
				// Register names
#else
  std::list<ABI_PROPERTY_INFO> reg_flags; // Register value properties
  std::list<REG_NAME> reg_names; // Register names
#endif
} *ABI;


static std::list<ABI_PROPERTY> props; // All the properties
static std::list<ABI> abis;		// All the ABIs
static ABI current_abi;		// The current ABI being described
static int prop_count = 0;	// number of properties

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the ABI properties. The description exports",
  " *   the following:",
  " *",
  " *   typedef (enum) ABI_PROPERTIES_ABI",
  " *",
  " *       An enumeration of the ABIs described. The names have the form:",
  " *",
  " *          ABI_PROPERTIES_ABI_xxx",
  " *",
  " *       where 'xxx' is replaced with the ABI name.",
  " *",
  " *   const ABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_UNDEFINED",
  " *       Useful value guaranteed not to be a valid ABI_PROPERTIES_ABI.",
  " *",
  " *   ABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_Value",
  " *       A variable containing the current ABI value.",
  " *",
  " *   const char *ABI_PROPERTIES_ABI_Name(ABI_PROPERTIES_ABI abi)",
  " *      Returns a name for the given 'abi'.",
  " *",
  " *   void ABI_PROPERTIES_Initialize(void)",
  " *       Initialize for the target ABI specified by ABI_PROPERTIES_ABI_Value.",
  " *",
  " *   const char *ABI_PROPERTY_Reg_Name(",
  " *     ISA_REGISTER_CLASS rc,",
  " *     INT reg",
  " *   )",
  " *       Return the ABI specific name of register 'reg' in class 'rc'.",
  " *",
  " *   BOOL ABI_PROPERTY_Is_xxx(",
  " *     ISA_REGISTER_CLASS rc,",
  " *     INT reg",
  " *   )",
  " *       Return a boolean that indicates if register 'reg' in class",
  " *       'rc' had the property 'xxx'.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


/////////////////////////////////////
void ABI_Properties_Begin(const char * /* name */)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
}


/////////////////////////////////////
ABI_PROPERTY Create_Reg_Property(const char *name)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ABI_PROPERTY result = new abi_property;

  result->name = name;
  result->val = 0;
  result->rc = ISA_REGISTER_CLASS_UNDEFINED;
  int rc;
  for (rc = 0; rc <= ISA_REGISTER_CLASS_MAX; ++rc)
    result->reg[rc] = -1;
  result->single_class = false;
  result->single_reg = false;

  props.push_back(result);
  return result;
}


/////////////////////////////////////
void Begin_ABI(const char *name)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ABI result = new abi;

  result->name = name;
#if ISA_REGISTER_MAX < NUM_REGISTERS_LIMIT
  memset(result->reg_names, 0, sizeof(result->reg_names));
#endif

  current_abi = result;

  abis.push_back(result);
}


/////////////////////////////////////
void Reg_Property(ABI_PROPERTY prop, ISA_REGISTER_CLASS rc, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  int reg_num;
  bool used = false;
  int count = 0;
  int firstreg = -1;

  va_start(ap,rc);
  while ( (reg_num = va_arg(ap,int)) != -1 ) {
    if (firstreg == -1) 
      firstreg = reg_num;
#if ISA_REGISTER_MAX < NUM_REGISTERS_LIMIT
    current_abi->reg_flags[rc][reg_num].push_back(prop);
#else
    {
    ABI_PROPERTY_INFO api = {rc, reg_num, reg_num, prop};
    current_abi->reg_flags.push_back(api);
    }
#endif
    used = true;
    ++count;
  }
  va_end(ap);

  if (used && prop->val == 0) {
    prop->val = 1ULL << prop_count;
    ++prop_count;
    prop->single_class = true;
    if (count == 1)
      prop->single_reg = true;
    else
      prop->single_reg = false;
    prop->rc = rc;
  }
  else {
    // in case multiple calls to this property, e.g. if in multiple regclass
    prop->single_class = false;
  }
  prop->reg[rc] = firstreg;
}

/////////////////////////////////////
void Reg_Property_Range(ABI_PROPERTY prop, ISA_REGISTER_CLASS rc, INT minreg, INT maxreg)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  bool used = false;

#if ISA_REGISTER_MAX < NUM_REGISTERS_LIMIT
  int reg_num;
  for (reg_num = minreg; reg_num <= maxreg; ++reg_num) {
    current_abi->reg_flags[rc][reg_num].push_back(prop);
    used = true;
  }
#else
  ABI_PROPERTY_INFO api = {rc, minreg, maxreg, prop};
  current_abi->reg_flags.push_back(api);
  used = true;
#endif

  if (used && prop->val == 0) {
    prop->val = 1ULL << prop_count;
    ++prop_count;
    prop->rc = rc;
    prop->single_class = true;
  }
  else {
    prop->single_class = false;
  }
  prop->reg[rc] = minreg;
}


void Set_Reg_Name(ISA_REGISTER_CLASS rc, INT reg, const char *name)
{
#if ISA_REGISTER_MAX < NUM_REGISTERS_LIMIT
    current_abi->reg_names[rc][reg] = name;
#else
    REG_NAME rn = {rc, reg, name};
    current_abi->reg_names.push_back(rn);
#endif
}

/////////////////////////////////////
void Reg_Names(ISA_REGISTER_CLASS rc, INT minreg, INT maxreg, const char **names)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  int reg_num;
  for (reg_num = minreg; reg_num <= maxreg; ++reg_num) {
    Set_Reg_Name (rc, reg_num, names[reg_num - minreg]);
  }
}

/////////////////////////////////////
static const char *Type_Name(int bits)
/////////////////////////////////////
//  Given a number of bits, return the name of the smallest unsigned
//  type that can hold values of that size.
/////////////////////////////////////
{
  if (bits <= 8) {
    return "mUINT8";
  } else if (bits <= 16) {
    return "mUINT16";
  } else if (bits <= 32) {
    return "mUINT32";
  } else {
    assert (bits <= 64);
    return "mUINT64";
  }
}


/////////////////////////////////////
static const char *Type_Suffix(int bits)
/////////////////////////////////////
//  Given a number of bits, return the suffix, for an integral constant
//  specification, that corresponds to the type used to hold values of
//  this size.
/////////////////////////////////////
{
  if (bits <= 8) {
    return "";
  } else if (bits <= 16) {
    return "";
  } else if (bits <= 32) {
    return "U";
  } else {
    assert (bits <= 64);
    return "ULL";
  }
}


/////////////////////////////////////
static int Type_Size(int bits)
/////////////////////////////////////
//  Given a number of bits, return the size in bits of the smallest
//  unsigned type can hold values of that size.
/////////////////////////////////////
{
  if (bits <= 8) {
    return 8;
  } else if (bits <= 16) {
    return 16;
  } else if (bits <= 32) {
    return 32;
  } else {
    assert (bits <= 64);
    return 64;
  }
}


/////////////////////////////////////
void ABI_Properties_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::list<ABI_PROPERTY>::iterator prop_iter;
  std::list<ABI>::iterator abi_iter;

  char filename[1000];
  sprintf (filename, "targ_abi_properties.h");
  FILE* hfile = fopen(filename, "w");
  sprintf (filename, "targ_abi_properties.c");
  FILE* cfile = fopen(filename, "w");
  sprintf(filename,"targ_abi_properties.Exported");
  FILE* efile = fopen(filename,"w");

  fprintf(cfile,"#include \"targ_abi_properties.h\"\n\n");

  sprintf (filename, "targ_abi_properties");
  Emit_Header (hfile, filename, interface);
  fprintf(hfile,"#include \"targ_isa_registers.h\"\n");

  //
  // Generate the ABI_PROPERTIES decl
  //
  fprintf(hfile, "\ntypedef struct {\n");

  if (prop_count != 0) {
    fprintf(hfile, "  %s reg_flags[%d][%d];\n", 
		   Type_Name(prop_count),
		   ISA_REGISTER_CLASS_MAX+1,
		   ISA_REGISTER_MAX+1);
  }
  fprintf(hfile, "  const char *reg_names[%d][%d];\n",
		 ISA_REGISTER_CLASS_MAX+1,
		 ISA_REGISTER_MAX+1);
  fprintf(hfile, "} ABI_PROPERTIES;\n");

  //
  // Generate the property flags decls
  //
  fprintf(hfile, "\n");
  for (prop_iter = props.begin(); prop_iter != props.end(); ++prop_iter) {
    ABI_PROPERTY prop = *prop_iter;
    fprintf(hfile, "#define ABI_PROPERTY_%-20s 0x%0*" LL_FORMAT "x%s\n",
		   prop->name,
		   Type_Size(prop_count) / 4,
		   prop->val,
		   Type_Suffix(prop_count));
  }

  //
  // Generate the properties data and the ABI enumeration
  //
  int num_abi = 0;
  fprintf(hfile, "\ntypedef enum {\n");
  fprintf(cfile, "\nstatic const ABI_PROPERTIES abi_properties[] = {\n");
  for (abi_iter = abis.begin(); abi_iter != abis.end(); ++abi_iter) {
    int rc;
    ABI abi = *abi_iter;
    ++num_abi;

    fprintf(hfile, "  ABI_PROPERTIES_ABI_%s,\n", abi->name);

    fprintf(cfile, "  {\n"
		   "    /* %s */\n",
		   abi->name);

    int count = prop_count;
    if (count != 0) {
      fprintf(cfile, "    {\n");
      for (rc = 0; rc <= ISA_REGISTER_CLASS_MAX; ++rc) {
	int reg;
	const ISA_REGISTER_CLASS_INFO *cinfo 
	  = ISA_REGISTER_CLASS_Info((ISA_REGISTER_CLASS)rc);

	fprintf(cfile, "      /* ISA_REGISTER_CLASS_%s */\n",
		       ISA_REGISTER_CLASS_INFO_Name(cinfo));
	int cursor = fprintf(cfile, "      {");
	for (reg = 0; reg <= ISA_REGISTER_MAX; ++reg) {
	  unsigned long long mask = 0;
#if ISA_REGISTER_MAX < NUM_REGISTERS_LIMIT
	  std::list<ABI_PROPERTY> props = abi->reg_flags[rc][reg];
	  for (prop_iter = props.begin(); prop_iter != props.end(); ++prop_iter) {
	    ABI_PROPERTY prop = *prop_iter;
	    mask |= prop->val;
	  }
#else
          std::list<ABI_PROPERTY_INFO>::iterator api_iter;
	  for (api_iter = abi->reg_flags.begin(); 
             api_iter != abi->reg_flags.end(); 
             ++api_iter) 
          {
	    ABI_PROPERTY_INFO api = *api_iter;
            if (api.rc == rc && reg >= api.minreg && reg <= api.maxreg) {
	      ABI_PROPERTY prop = api.aprop;
	      mask |= prop->val;
            }
          }
#endif
	  if (cursor >= 80 - (4 + Type_Size(count) / 4) - 3 - 1) {
	    fprintf(cfile, "\n");
	    cursor = fprintf(cfile, "       ");
	  }
	  cursor += fprintf(cfile, " 0x%0*" LL_FORMAT "x%s,",
				   Type_Size(count) / 4,
				   mask,
				   Type_Suffix(count));
	}
	fprintf(cfile, " },\n");
      }
      fprintf(cfile, "    },\n");
    }

    fprintf(cfile, "    {\n");
    for (rc = 0; rc <= ISA_REGISTER_CLASS_MAX; ++rc) {
      int reg;
      const ISA_REGISTER_CLASS_INFO *cinfo 
	= ISA_REGISTER_CLASS_Info((ISA_REGISTER_CLASS)rc);

      fprintf(cfile, "      /* ISA_REGISTER_CLASS_%s */\n",
		     ISA_REGISTER_CLASS_INFO_Name(cinfo));
      int cursor = fprintf(cfile, "      {");
      for (reg = 0; reg <= ISA_REGISTER_MAX; ++reg) {
        const char *name;
#if ISA_REGISTER_MAX < NUM_REGISTERS_LIMIT
	name = abi->reg_names[rc][reg];
#else
        name = NULL;
        std::list<REG_NAME>::iterator rn_iter;
	for (rn_iter = abi->reg_names.begin(); 
             rn_iter != abi->reg_names.end(); 
             ++rn_iter) 
        {
	    REG_NAME rn = *rn_iter;
            if (rn.rc == rc && rn.reg == reg) {
              name = rn.name;
              break;
            }
        }
#endif
	if (name == NULL) name = ISA_REGISTER_CLASS_INFO_Reg_Name(cinfo, reg);
	if (name == NULL) name = "";
	if (cursor >= 80 - (4 + strlen(name))) {
	  fprintf(cfile, "\n");
	  cursor = fprintf(cfile, "       ");
	}
	cursor += fprintf(cfile, " \"%s\",", name);
      }
      fprintf(cfile, " },\n");
    }
    fprintf(cfile, "    },\n");

    fprintf(cfile, "  },\n");
  }
  fprintf(cfile, "};\n");
  fprintf(hfile, "  ABI_PROPERTIES_ABI_UNDEFINED,\n"
		 "  ABI_PROPERTIES_ABI_MAX=%d\n"
		 "} ABI_PROPERTIES_ABI;\n",
		 num_abi-1);

  //
  // Generate the abi names
  //
  fprintf(cfile, "\nstatic const char * const abi_names[] = {\n");
  for (abi_iter = abis.begin(); abi_iter != abis.end(); ++abi_iter) {
    ABI abi = *abi_iter;
    fprintf(cfile, "  \"%s\",\n", abi->name);
  }
  fprintf(cfile, "  \"UNDEFINED\"\n"
		 "};\n");

  //
  // Generate decls for accessing the ABI data and initializiation
  //
  fprintf(hfile, "\nextern ABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_Value;\n");
  fprintf(cfile, "\nABI_PROPERTIES_ABI ABI_PROPERTIES_ABI_Value = ABI_PROPERTIES_ABI_UNDEFINED;\n");
  fprintf(efile, "ABI_PROPERTIES_ABI_Value\n");

  fprintf(hfile, "\nextern const char *ABI_PROPERTIES_ABI_Name(ABI_PROPERTIES_ABI abi);\n");
  fprintf(cfile, "\nconst char *ABI_PROPERTIES_ABI_Name(ABI_PROPERTIES_ABI abi)\n"
		 "{\n"
		 "  return abi_names[(INT)abi];\n"
		 "}\n");
  fprintf(efile, "ABI_PROPERTIES_Initialize\n");

  fprintf(cfile, "\nconst ABI_PROPERTIES *ABI_PROPERTIES_target_props"
		 " = &abi_properties[ABI_PROPERTIES_ABI_UNDEFINED];\n");
  fprintf(efile, "ABI_PROPERTIES_target_props\n");

  fprintf(hfile, "\nextern void ABI_PROPERTIES_Initialize(void);\n");
  fprintf(cfile, "\nvoid ABI_PROPERTIES_Initialize(void)\n"
		 "{\n"
		 "  ABI_PROPERTIES_target_props = &abi_properties[(INT)ABI_PROPERTIES_ABI_Value];\n"
		 "}\n");
  fprintf(efile, "ABI_PROPERTIES_Initialize\n");

  //
  // Generate the property accessors
  //
  fprintf(hfile, "\ninline const char *ABI_PROPERTY_Reg_Name(\n"
		 "  ISA_REGISTER_CLASS rc,\n"
		 "  INT reg)\n"
		 "{\n"
		 "  extern const ABI_PROPERTIES *ABI_PROPERTIES_target_props;\n"
		 "  return ABI_PROPERTIES_target_props->reg_names[rc][reg];\n"
		 "}\n");

  for (prop_iter = props.begin(); prop_iter != props.end(); ++prop_iter) {
    ABI_PROPERTY prop = *prop_iter;
    fputs(prop->val ? "\n" : "\n/*ARGSUSED*/\n", hfile);
      fprintf(hfile, "inline BOOL ABI_PROPERTY_Is_%s(\n"
		     "  ISA_REGISTER_CLASS rc,\n"
		     "  INT reg)\n"
		     "{\n",
		     prop->name);
      if (prop->val == 0) {
	fprintf(hfile, "  return FALSE;\n"
		       "}\n");
      } else {
	fprintf(hfile, "  extern const ABI_PROPERTIES *ABI_PROPERTIES_target_props;\n"
		       "  return (  ABI_PROPERTIES_target_props->reg_flags[rc][reg]\n"
		       "          & ABI_PROPERTY_%s) != 0;\n"
		       "}\n",
		       prop->name);
      }
  }

  // Generate accessors for abi properties so we don't have to add separate
  // accessors in cg.  When property maps to several registers, just give
  // the first register, which is useful if in a sequential range
  // (not useful in some cases, but doesn't hurt).
  fprintf(hfile, "\n");
  fprintf(hfile, "/* For properties that map to only one register,\n");
  fprintf(hfile, " * create accessor routines for those registers.\n");
  fprintf(hfile, " * If they map to multiple registers,\n");
  fprintf(hfile, " * create accessor routines for first register.\n");
  fprintf(hfile, " */\n");
  for (prop_iter = props.begin(); prop_iter != props.end(); ++prop_iter) {
    ABI_PROPERTY prop = *prop_iter;
    if (prop->rc == ISA_REGISTER_CLASS_UNDEFINED) {
      continue;
    }
    const ISA_REGISTER_CLASS_INFO *cinfo = ISA_REGISTER_CLASS_Info(prop->rc);
    if (prop->single_class) {
      fprintf(hfile, "#define ABI_PROPERTY_%s_Register_Class ISA_REGISTER_CLASS_%s\n", 
	prop->name, ISA_REGISTER_CLASS_INFO_Name(cinfo));
      if (prop->single_reg) 
        fprintf(hfile, "#define ABI_PROPERTY_%s_Register %d\n", 
	  prop->name, prop->reg[prop->rc]);
      else
        fprintf(hfile, "#define ABI_PROPERTY_%s_First_Register %d\n", 
	  prop->name, prop->reg[prop->rc]);
    }
    else { // !single_class
      int rc;
      for (rc = 1; rc <= ISA_REGISTER_CLASS_MAX; ++rc) {
        if (prop->reg[rc] != -1) {
          if (prop->single_reg) 
            fprintf(hfile, "#define ABI_PROPERTY_%s_%s_Register %d\n", 
	      ISA_REGISTER_CLASS_INFO_Name(
	        ISA_REGISTER_CLASS_Info((ISA_REGISTER_CLASS)rc)),
	      prop->name, prop->reg[rc]);
          else
            fprintf(hfile, "#define ABI_PROPERTY_%s_%s_First_Register %d\n", 
	      ISA_REGISTER_CLASS_INFO_Name(
	        ISA_REGISTER_CLASS_Info((ISA_REGISTER_CLASS)rc)),
	      prop->name, prop->reg[rc]);
        }
      }
    }
    fprintf(hfile, "\n");
  }

  Emit_Footer (hfile);

  fclose(hfile);
  fclose(cfile);
  fclose(efile);
}
