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


// isa_bundle_gen.cxx
/////////////////////////////////////
//
//  Gneerate an interface for specifying template encoding instructions 
//  within a bundle.
//
/////////////////////////////////////
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_bundle_gen.cxx,v $

#include <alloca.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <strings.h>
#include <list>
#include <vector>
#include "topcode.h"
#include "targ_isa_properties.h"
#include "gen_util.h"
#include "isa_bundle_gen.h"

#define MAX_SLOTS 3	// max # of slots the generator can handle
#define TAG_SHIFT 12    // max # of bits required to encode all the
                        // execution property types.

struct isa_exec_unit_type {
  const char *name; 	// Name given for documentation and debugging
  int bit_position;	// bit position in flag word
  std::vector<bool> members; // set of opcodes that have this property
  ISA_EXEC_UNIT_TYPE base_unit; // base exec unit type (or null if base)
};

struct isa_bundle_type {
  const char *name;
  const char *asm_name;
  int slot_count;
  ISA_EXEC_UNIT_TYPE slot[MAX_SLOTS];
  bool stop_bit[MAX_SLOTS];
  unsigned int pack_code;
};

static int isa_exec_property_count = 0; 

static int num_bundles = 0;
static int max_slots = 0;
static int bundle_bits;
static std::list<ISA_EXEC_UNIT_TYPE> all_exec_types; 
static std::list<ISA_BUNDLE_TYPE> all_bundles; 
static ISA_EXEC_UNIT_TYPE current_exec_type_desc;
static ISA_BUNDLE_TYPE current_bundle_desc;

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the bundling properties. The interface is",
  " *   divided into two pieces: scheduling, and packing. The scheduling",
  " *   interface exports the following:",
  " *",
  " *   const INT ISA_MAX_SLOTS",
  " *       An integer constant that indicates the maximum number of",
  " *       slots in a bundle.",
  " *",
  " *   const INT ISA_TAG_SHIFT",
  " *       Maximum number of bits required to encode all the execution",
  " *       property types.",
  " *",
  " *   typedef mUINTxx ISA_EXEC_UNIT_PROPERTY",
  " *       A single-bit mask of representing an execution unit.",
  " *",
  " *       The names have the form ISA_EXEC_PROPERTY_xxx",
  " *       where 'xxx' is replaced with the EXEC_UNIT_PROPERTY name.",
  " *",
  " *   typedef (enum) ISA_EXEC_UNIT",
  " *       An enumeration of the execution units.",
  " *",
  " *       The names have the form ISA_EXEC_xxx",
  " *       where 'xxx' is replaced with the EXEC_UNIT_PROPERTY name.",
  " *",
  " *       The values of ISA_EXEC_UNIT and ISA_EXEC_UNIT_PROPERTY are",
  " *       related in that the bit-mask value of an ISA_EXEC_UNIT_PROPERTY",
  " *       is equal to 2**ISA_EXEC_UNIT.",
  " *",
  " *   const INT ISA_EXEC_MAX",
  " *       The highest value ISA_EXEC_UNIT value.",
  " *",
  " *   BOOL ISA_EXEC_PROPERTY_is_xxx(TOP t) ",
  " *       Returns TRUE if EXEC_PROPERTY_is_xxx matches <t>'s property.",
  " *",
  " *   ISA_EXEC_UNIT_PROPERTY ISA_EXEC_Unit_Prop(TOP topcode)",
  " *       Returns exec_unit_property for the instruction specified",
  " *       by <topcode>.",
  " *",
  " *   ISA_BUNDLE_INFO ISA_EXEC_Bundle_Info(INT index)",
  " *       Return isa_bundle_info specified by <index>. ",
  " *",
  " *   ISA_EXEC_UNIT_PROPERTY ISA_EXEC_Slot_Prop(INT bundle, INT slot_index)",
  " *       Return exec_unit_property for the slot position <slot_index>",
  " *       in <bundle>.",
  " *",
  " *   UINT64 ISA_EXEC_Slot_Mask(INT bundle)",
  " *       Return slot_mask for <bundle>.",
  " *",
  " *   BOOL ISA_EXEC_Stop(INT bundle, INT slot_index)",
  " *       Return stop bit for the slot position <slot_index> in <bundle>.",
  " *",
  " *   ISA_EXEC_UNIT ISA_EXEC_Unit(INT bundle, INT slot_index)",
  " *       Return the execution unit slot position <slot_index> in <bundle>.",
  " *",
  " *   UINT32 ISA_EXEC_Stop_Mask(INT bundle)",
  " *       Return stop_mask for <bundle>.",
  " *",
  " *   const char *ISA_EXEC_Name(INT bundle)",
  " *       Return the name for <bundle>.",
  " *",
  " *   const char *ISA_EXEC_AsmName(INT bundle)",
  " *       Return the assembly language name for <bundle>.",
  " *",
  " * ====================================================================",
  " *",
  " *   The packing interface exports the following:",
  " *",
  " *   typedef ISA_BUNDLE",
  " *       A type large enough to hold a bundle. This type will always",
  " *       be a struct containing an array of either 32-, or 64-bit",
  " *       unsigned integers.",
  " *",
  " *   typedef (enum) ISA_BUNDLE_PACK_COMP",
  " *       An enumeration of the bundle components to be packed.",
  " *",
  " *   const INT ISA_BUNDLE_PACK_COMP_MAX",
  " *       The maximum number of components to be packed for a bundle.",
  " *",
  " *   typedef (struct) ISA_BUNDLE_PACK_INFO",
  " *       Describes how a the components of a bundle are packed.",
  " *       The contents are private.",
  " *",
  " *   const ISA_BUNDLE_PACK_INFO *ISA_BUNDLE_Pack_Info(void)",
  " *       Returns a pointer to the first packing component.",
  " *       Increment the returned pointer to access any additional packing",
  " *       components for the bundle. A component of ISA_PACK_COMP_end",
  " *       marks the end.",
  " *",
  " *   INT ISA_BUNDLE_PACK_INFO_Comp(const ISA_BUNDLE_PACK_INFO *info)",
  " *       Identifies the bundle component to be packed.",
  " *",
  " *   INT ISA_BUNDLE_PACK_INFO_Index(const ISA_BUNDLE_PACK_INFO *info)",
  " *       The index of the bundle word containing the component.",
  " *",
  " *       ISA_BUNDLE_PACK_INFO_Index is meaningless for ISA_BUNDLE_PACK_COMP_end.",
  " *",
  " *   INT ISA_BUNDLE_PACK_INFO_CompPos(const ISA_BUNDLE_PACK_INFO *info)",
  " *       The offset, in bits, to the start of the component in the",
  " *       component value.",
  " *",
  " *       ISA_BUNDLE_PACK_INFO_CompPos is meaningless for ISA_BUNDLE_PACK_COMP_end.",
  " *",
  " *   INT ISA_BUNDLE_PACK_INFO_BundlePos(const ISA_BUNDLE_PACK_INFO *info)",
  " *       The offset, in bits, to the start of the component in the",
  " *       bundle word.",
  " *",
  " *       ISA_BUNDLE_PACK_INFO_BundlePos is meaningless for ISA_BUNDLE_PACK_COMP_end.",
  " *",
  " *   UINT64 ISA_BUNDLE_PACK_INFO_Mask(const ISA_BUNDLE_PACK_INFO *info)",
  " *       A bit mask that is as wide as the bundle component being",
  " *       packed. The mask is shifted to match the field in the",
  " *       bundle word.",
  " *",
  " *       ISA_BUNDLE_PACK_INFO_Mask is meaningless for ISA_BUNDLE_PACK_COMP_end.",
  " *",
  " *   INT ISA_BUNDLE_Pack_Info_Index(ISA_BUNDLE_PACK_COMP comp)",
  " *       Index into bundle packing info array (see ISA_BUNDLE_Pack_Info)",
  " *       to the start of the info for the component <comp>. If there",
  " *       is no packing info for <comp>, the index is for the 'end'",
  " *       component.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};

/* ====================================================================
 *
 * This section handles bundle scheduling
 *
 * ====================================================================
 */

/////////////////////////////////////
ISA_EXEC_UNIT_TYPE ISA_Exec_Unit_Type_Create ( const char* name,
					       ISA_EXEC_UNIT_TYPE base_unit )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_EXEC_UNIT_TYPE cur_type = new isa_exec_unit_type;

  cur_type->name = name;
  cur_type->bit_position = isa_exec_property_count++;
  cur_type->members = std::vector<bool> (TOP_count, false);
  cur_type->base_unit = base_unit;

  current_exec_type_desc = cur_type;
  all_exec_types.push_back (current_exec_type_desc);
  return cur_type;
}

/////////////////////////////////////
void Instruction_Exec_Unit_Group(ISA_EXEC_UNIT_TYPE unit_type, ... )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  TOP opcode;

  if (!current_exec_type_desc->name) {
    fprintf(stderr,"### Error: no execution unit type name specified for %s\n",
                   current_exec_type_desc->name);
    exit(EXIT_FAILURE);
  }
 
  va_start(ap, unit_type);
  while ( (opcode = static_cast<TOP>(va_arg(ap, int))) != TOP_UNDEFINED) {
      unit_type->members[(int)opcode] = true;      
  }
  va_end(ap);  
}

/////////////////////////////////////
void ISA_Bundle_Type_Create (const char* name, const char* asm_name, 
			     int no_slots )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  int i;
  ISA_BUNDLE_TYPE cur_type = new isa_bundle_type;
  cur_type->name = name;
  cur_type->asm_name = asm_name;
  cur_type->slot_count = no_slots;
  cur_type->pack_code = num_bundles;
  for (i = 0; i < no_slots; ++i) cur_type->stop_bit[i] = false;

  current_bundle_desc = cur_type;
  all_bundles.push_back (current_bundle_desc);

  if (no_slots > max_slots) max_slots = no_slots;
  ++num_bundles;
}

/////////////////////////////////////
void Slot (int slot_index, ISA_EXEC_UNIT_TYPE type)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (slot_index > current_bundle_desc->slot_count) {
    fprintf(stderr, "### Error: slot index (%d) exceeds %d\n",
		    slot_index, current_bundle_desc->slot_count);
    exit(EXIT_FAILURE);
  }

  if (!type) {
    fprintf(stderr, "### Error: slot type have non NULL value \n");
    exit(EXIT_FAILURE);
  }

  current_bundle_desc->slot[slot_index] = type;
}

/////////////////////////////////////
void Stop (int slot_index)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (slot_index > current_bundle_desc->slot_count) {
    fprintf(stderr, "### Error: slot index (%d) exceeds %d\n",
		    slot_index, current_bundle_desc->slot_count);
    exit(EXIT_FAILURE);
  }

  current_bundle_desc->stop_bit[slot_index] = true;
}

/////////////////////////////////////
static void Emit_Bundle_Scheduling(FILE *hfile, FILE *cfile, FILE *efile)
/////////////////////////////////////
//  Emit the bundle scheduling interface.
/////////////////////////////////////
{
  std::list<ISA_EXEC_UNIT_TYPE>::iterator iei;
  std::list<ISA_BUNDLE_TYPE>::iterator ibi;
  int i;

  const char * const isa_exec_type_format = "  %3llu,  /* %s: ";
  const char *info_index_type;

  int index = 0;
  for (iei = all_exec_types.begin(); iei != all_exec_types.end(); ++index,
								  ++iei) {
  }

  char *int_suffix;
  // select the ISA_EXEC_unit_prop based on the number of exec info types.
  if (index <= 8) {
    info_index_type = "mUINT8";
    int_suffix = "";
  } else if (index <= 16) {
    info_index_type = "mUINT16";
    int_suffix = "";
  } else if (index <= 32) {
    info_index_type = "mUINT32";
    int_suffix = "U";
  } else {
    assert (index <= 64);
    info_index_type = "mUINT64";
    int_suffix = "ULL";
  }

  fprintf (hfile, "\n#define ISA_MAX_SLOTS (%d)\n", max_slots);
  fprintf (hfile, "#define ISA_TAG_SHIFT (%d)\n", TAG_SHIFT);

  fprintf (hfile, "\ntypedef %s ISA_EXEC_UNIT_PROPERTY;\n",
	   info_index_type);

  fprintf (hfile, "\n");
  for (iei = all_exec_types.begin(); iei != all_exec_types.end(); ++iei) {
    ISA_EXEC_UNIT_TYPE curr_exec_type = *iei;
    fprintf (hfile, "#define ISA_EXEC_PROPERTY_%-15s (0x%llx%s)\n",
		    curr_exec_type->name,
		    (1ULL << curr_exec_type->bit_position), int_suffix);
  }

  fprintf (hfile, "\ntypedef enum {\n");
  for (iei = all_exec_types.begin(); iei != all_exec_types.end(); ++iei) {
    ISA_EXEC_UNIT_TYPE curr_exec_type = *iei;
    fprintf (hfile, "  ISA_EXEC_%-15s = %d,\n",
		    curr_exec_type->name,
		    curr_exec_type->bit_position);
  }
  fprintf (hfile, "  ISA_EXEC_%-15s = %d\n"
		  "} ISA_EXEC_UNIT;\n",
		  "MAX", isa_exec_property_count - 1);

  fprintf (hfile, "\ntypedef struct {\n"
		  "  const char *name;\n"
		  "  const char *asm_name;\n"
		  "  int slot_count;\n"
		  "  ISA_EXEC_UNIT_PROPERTY slot[%d];\n"
		  "  mBOOL stop[%d];\n"
		  "  mUINT8 unit[%d];\n"
		  "  mUINT8 pack_code;\n"
		  "  mUINT8 stop_mask;\n"
		  "  mUINT64 slot_mask;\n"
		  "} ISA_BUNDLE_INFO;\n",
		  max_slots ? max_slots : 1,
		  max_slots ? max_slots : 1,
		  max_slots ? max_slots : 1);

  fprintf(efile, "ISA_BUNDLE_info\n");
  fprintf(cfile, "\nconst ISA_BUNDLE_INFO ISA_BUNDLE_info[] = {\n");

  int slot_mask_digits = ((TAG_SHIFT * max_slots) + 3) / 4;
  for (ibi = all_bundles.begin(); ibi != all_bundles.end(); ++ibi) {
    ISA_BUNDLE_TYPE curr_exec_type = *ibi;
    fprintf (cfile, " {\n    \"%s\",%*s \"%s\",%*s %d,", 
		    curr_exec_type->name, 
		    13 - strlen(curr_exec_type->name), "",
		    curr_exec_type->asm_name, 
		    8 - strlen(curr_exec_type->asm_name), "",
		    curr_exec_type->slot_count);

    unsigned long long slot_mask = 0;
    unsigned int stop_mask = 0;
    fprintf (cfile, "\n    {");
    for (i = 0; i < curr_exec_type->slot_count; i++) {
      unsigned int flag_value = 1 << curr_exec_type->slot[i]->bit_position;
      int shift_count = max_slots - i - 1;
      slot_mask |= ((unsigned long long)flag_value << (TAG_SHIFT * shift_count));
      stop_mask |= (curr_exec_type->stop_bit[i] << shift_count);
      fprintf (cfile, " %2d /* %7s */,", 
		      flag_value,
		      curr_exec_type->slot[i]->name);
    }
    fprintf (cfile, " },");

    fprintf (cfile, "\n    {");
    for (i = 0; i < max_slots; i++) {
      fprintf (cfile, " %5s,", curr_exec_type->stop_bit[i] ? "TRUE" : "FALSE");
    }
    fprintf (cfile, " },");

    fprintf (cfile, "\n    {");
    for (i = 0; i < curr_exec_type->slot_count; i++) {
      ISA_EXEC_UNIT_TYPE unit_type = curr_exec_type->slot[i];
      if (unit_type->base_unit) unit_type = unit_type->base_unit;
      fprintf (cfile, " ISA_EXEC_%5s,", unit_type->name);
    }
    fprintf (cfile, " },");

    fprintf(cfile, "\n    %2d,", curr_exec_type->pack_code);
    fprintf(cfile, " 0x%1x,", stop_mask);
    fprintf(cfile, " 0x%0*llx\n  },\n", slot_mask_digits, slot_mask);
  }
  fprintf (cfile, "  {\n    \"template_MAX\", \"\", -1,\n    { -1 /* ??????? */");
  for (i = 1; i < max_slots; ++i) fprintf (cfile, ", -1 /* ??????? */");
  fprintf (cfile, ",},\n    { FALSE");
  for (i = 1; i < max_slots; ++i) fprintf (cfile, ", FALSE");
  fprintf (cfile, ",},\n    -1, 0x0, 0x%0*x\n  }\n};\n", slot_mask_digits, 0);

  fprintf(hfile,"\n#define ISA_MAX_BUNDLES %d\n",num_bundles);

  fprintf (efile, "ISA_EXEC_unit_prop\n");
  fprintf (cfile, "\nconst ISA_EXEC_UNIT_PROPERTY ISA_EXEC_unit_prop[%d] = {\n",
	  TOP_count);

  for (int top = 0; top < TOP_count; ++top) {
    unsigned long long flag_value = 0;

    for (iei = all_exec_types.begin(); iei != all_exec_types.end(); ++iei) {
      ISA_EXEC_UNIT_TYPE exec_type = *iei;
      if (exec_type->members[top]) 
	flag_value |= (1ULL << exec_type->bit_position);
    }
    fprintf(cfile, 	isa_exec_type_format,
			flag_value,
			TOP_Name((TOP)top));
    for ( iei = all_exec_types.begin(); iei != all_exec_types.end(); ++iei ) {
      ISA_EXEC_UNIT_TYPE exec_type = *iei;
      if (exec_type->members[top]) 
	fprintf (cfile, " %s", exec_type->name);
    }
    fprintf (cfile, " */\n");
  }
  fprintf(cfile, "};\n");

  fprintf(hfile, "\nextern const ISA_EXEC_UNIT_PROPERTY ISA_EXEC_unit_prop[];\n");
  fprintf(hfile, "\n");
  for (iei = all_exec_types.begin(); iei != all_exec_types.end(); ++iei) {
    ISA_EXEC_UNIT_TYPE exec_type = *iei;
    fprintf(hfile,
             "#define EXEC_PROPERTY_is_%s(t)\t (ISA_EXEC_unit_prop[(INT)t] & ISA_EXEC_PROPERTY_%s)\n",
             exec_type->name, exec_type->name);
  }

  fprintf (hfile, "\ninline ISA_EXEC_UNIT_PROPERTY "
                   "ISA_EXEC_Unit_Prop(TOP topcode)\n"
                 "{\n"
                 "  return ISA_EXEC_unit_prop[(INT)topcode];\n"
                 "}\n");
		   
  fprintf (hfile, "\ninline ISA_BUNDLE_INFO "
                   "ISA_EXEC_Bundle_Info(INT index)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
                 "  return ISA_BUNDLE_info[index];\n"
                 "}\n");
		   
  fprintf (hfile, "\ninline ISA_EXEC_UNIT_PROPERTY "
                   "ISA_EXEC_Slot_Prop(INT bundle, INT slot_index)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
		 "  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n"
                 "  return info->slot[slot_index];\n"
                 "}\n");

  fprintf (hfile, "\ninline UINT64 "
                   "ISA_EXEC_Slot_Mask(INT bundle)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
		 "  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n"
                 "  return info->slot_mask;\n"
                 "}\n");

  fprintf (hfile, "\ninline BOOL "
                   "ISA_EXEC_Stop(INT bundle, INT slot_index)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
		 "  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n"
                 "  return info->stop[slot_index];\n"
                 "}\n");

  fprintf (hfile, "\ninline ISA_EXEC_UNIT "
                   "ISA_EXEC_Unit(INT bundle, INT slot_index)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
		 "  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n"
                 "  return (ISA_EXEC_UNIT)info->unit[slot_index];\n"
                 "}\n");

  fprintf (hfile, "\ninline UINT32 "
                   "ISA_EXEC_Stop_Mask(INT bundle)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
		 "  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n"
                 "  return info->stop_mask;\n"
                 "}\n");

  fprintf (hfile, "\ninline const char * "
                   "ISA_EXEC_Name(INT bundle)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
		 "  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n"
                 "  return info->name;\n"
                 "}\n");

  fprintf (hfile, "\ninline const char * "
                   "ISA_EXEC_AsmName(INT bundle)\n"
                 "{\n"
		 "  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n"
		 "  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n"
                 "  return info->asm_name;\n"
                 "}\n");
}

/* ====================================================================
 *
 * This section handles bundle packing
 *
 * ====================================================================
 */

typedef enum {
  END		= 0,		// end of list marker
  FTEMPLATE	= 1,		// template
  FSLOT		= 2,		// slot+n => slot n
} PACK_COMP_TYPE;

#define MAX_PACK_COMPS (FSLOT+MAX_SLOTS)

static const char * const pack_comp_type_name[] = {
  "END",
  "TEMPLATE",
  "SLOT0",
  "SLOT1",
  "SLOT2",
};

static const char * const pack_comp_name[] = {
  "ISA_BUNDLE_PACK_COMP_end",
  "ISA_BUNDLE_PACK_COMP_template",
  "ISA_BUNDLE_PACK_COMP_slot+0",
  "ISA_BUNDLE_PACK_COMP_slot+1",
  "ISA_BUNDLE_PACK_COMP_slot+2",
};

typedef struct {
  int comp_pos;
  int bundle_pos;
  int width;
} BUNDLE_FIELD;

typedef struct {
  ISA_BUNDLE_PACK_ENDIAN endian;
  BUNDLE_FIELD ftemplate;
  BUNDLE_FIELD fslot[MAX_SLOTS];
} BUNDLE_PACK_INFO;

static BUNDLE_PACK_INFO *bundle_pack_info = NULL;

/////////////////////////////////////
void ISA_Bundle_Pack_Create (ISA_BUNDLE_PACK_ENDIAN endian)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (bundle_pack_info) {
    fprintf(stderr, "### Error: ISA_Bundle_Pack_Create called multiple times\n");
    exit(EXIT_FAILURE);
  }

  bundle_pack_info = new(BUNDLE_PACK_INFO);
  bzero(bundle_pack_info, sizeof(*bundle_pack_info));
  bundle_pack_info->endian = endian;
}

/////////////////////////////////////
void Pack_Template (int comp_pos, int bundle_pos, int width)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (bundle_pack_info == NULL) {
    fprintf(stderr, "### Error: Missing call to ISA_Bundle_Pack_Create\n");
    exit(EXIT_FAILURE);
  }
  if (bundle_pos + width > bundle_bits) {
    fprintf(stderr, "### Error: field exceeds bundle boundaries\n");
    exit(EXIT_FAILURE);
  }

  bundle_pack_info->ftemplate.comp_pos = comp_pos;
  bundle_pack_info->ftemplate.bundle_pos = bundle_pos;
  bundle_pack_info->ftemplate.width = width;
}

/////////////////////////////////////
void Pack_Slot (int slot, int comp_pos, int bundle_pos, int width)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (bundle_pack_info == NULL) {
    fprintf(stderr, "### Error: Missing call to ISA_Bundle_Pack_Create\n");
    exit(EXIT_FAILURE);
  }
  if (slot >= MAX_SLOTS) {
    fprintf(stderr, "### Error: slot (%d) exceeds %d\n", slot, MAX_SLOTS);
    exit(EXIT_FAILURE);
  }
  if (bundle_pos + width > bundle_bits) {
    fprintf(stderr, "### Error: field exceeds bundle boundaries\n");
    exit(EXIT_FAILURE);
  }

  bundle_pack_info->fslot[slot].comp_pos = comp_pos;
  bundle_pack_info->fslot[slot].bundle_pos = bundle_pos;
  bundle_pack_info->fslot[slot].width = width;
}

/////////////////////////////////////
static unsigned long long Mask64(int width)
/////////////////////////////////////
//  Return a bit-mask of size <width>.
/////////////////////////////////////
{
  if ((unsigned)width > 64U) {
    fprintf(stderr, "### Error: field width (%d) exceeds 64\n", width);
    exit(EXIT_FAILURE);
  } else if (width == 64) {
    return -1ULL;
  }
  return (1ULL << width) - 1;
}

/////////////////////////////////////
static ISA_BUNDLE_PACK_ENDIAN Host_Endian(void)
/////////////////////////////////////
//  Return the endian-ness of the host machine.
/////////////////////////////////////
{
  int i = 0x12;
  int lowbyte = *(char *)&i;
  assert(sizeof(int) > sizeof(char));
  return (lowbyte == 0x12) ? ISA_Bundle_Pack_Little_Endian
			   : ISA_Bundle_Pack_Big_Endian;
}

/////////////////////////////////////
static void Emit_Pack_Component(
  FILE *cfile,
  BUNDLE_FIELD *field,
  int comp,
  int *first_comps,
  int *pack_index)
/////////////////////////////////////
//  Emit the packing info for a component. A single source specification
//  may result in multiple packing info entries depending on endian
//  specification and word boundary crossings.
/////////////////////////////////////
{
  if (first_comps[comp] < 0) first_comps[comp] = *pack_index;

  if (comp == END) {
    fprintf (cfile, "  { %-30s, %2d, %2d, %2d,   %16lld },  /* %s */\n",
		    pack_comp_name[comp],
		    -1,
		    -1,
		    -1, 
		    -1LL,
		    pack_comp_type_name[comp]);
    ++*pack_index;
  } else {
    bool wrong_endian = (bundle_pack_info->endian != Host_Endian());
    int incr;
    int flip_mask;
    int comp_pos = field->comp_pos;
    int bundle_pos = field->bundle_pos;
    int width = field->width;
    int word_size = bundle_bits >= 64 ? 64 : (bundle_bits + 7) & ~7;
    if (wrong_endian) {
      incr = 8;
      flip_mask = (word_size - 1) & 070;
    } else {
      incr = word_size;
      flip_mask = 0;
    }
    do {
      int bundle_word_pos = (bundle_pos % word_size) ^ flip_mask;
      int index = bundle_pos / word_size;
      int b = bundle_pos % incr;
      int w = width;
      if (b + width > incr) w = incr - b;
      fprintf (cfile, "  { %-30s, %2d, %2d, %2d, 0x%016llx },  /* %s */\n",
		      pack_comp_name[comp],
		      index,
		      comp_pos,
		      bundle_word_pos,
		      Mask64(w) << bundle_word_pos,
		      pack_comp_type_name[comp]);
      ++*pack_index;
      bundle_pos += w;
      comp_pos += w;
      width -= w;
    } while (width != 0);
  }
}

/////////////////////////////////////
static void Emit_Bundle_Packing(FILE *hfile, FILE *cfile, FILE *efile)
/////////////////////////////////////
//  Emit the bundle packing interface.
/////////////////////////////////////
{
  int i;
  int first_comps[MAX_PACK_COMPS];
  int max_pack_comps = FSLOT + max_slots;
  int word_size = bundle_bits >= 64 ? 64 : (bundle_bits + 7) & ~7;
  int pack_index = 0;

  if (bundle_pack_info == NULL) {
    fprintf(stderr, "### Error: no bundle packing specification!\n");
    exit(EXIT_FAILURE);
  }

  for (i = 0; i < MAX_PACK_COMPS; ++i) first_comps[i] = -1;

  fprintf(hfile, "\ntypedef struct {\n"
		 "  mUINT%d word[%d];\n"
		 "} ISA_BUNDLE;\n",
		 word_size, bundle_bits / word_size);

  fprintf(hfile, "\ntypedef enum {\n"
	"  %-30s = %d,  /* %s */\n"
   	"  %-30s = %d,  /* %s */\n"
   	"  %-30s = %d,  /* %s */\n"
   	"  %-30s = %d   /* %s */\n"
	"} ISA_BUNDLE_PACK_COMP;\n",
	"ISA_BUNDLE_PACK_COMP_end", END, "End of list marker",
	"ISA_BUNDLE_PACK_COMP_template", FTEMPLATE, "Template",
	"ISA_BUNDLE_PACK_COMP_slot", FSLOT, "SLOT+n => slot n",
        "ISA_BUNDLE_PACK_COMP_MAX", max_pack_comps-1, "Last component");

  fprintf(hfile, "\ntypedef struct {\n"
		"  mUINT8 comp;\n"
		"  mUINT8 index;\n"
  		"  mUINT8 comp_pos;\n"
  		"  mUINT8 bundle_pos;\n"
		"  UINT64 mask;\n" 
		"} ISA_BUNDLE_PACK_INFO;\n");

  fprintf(efile, "ISA_BUNDLE_pack_info\n");

  fprintf(cfile, "\nconst ISA_BUNDLE_PACK_INFO ISA_BUNDLE_pack_info[] = {\n");
  pack_index = 0;
  if (bundle_pack_info->ftemplate.width != 0) {
    Emit_Pack_Component(cfile,
			&bundle_pack_info->ftemplate,
			FTEMPLATE,
			first_comps,
			&pack_index);

  }
  for (i = 0; i < max_slots; ++i) {
    Emit_Pack_Component(cfile,
			&bundle_pack_info->fslot[i],
			FSLOT+i,
			first_comps,
			&pack_index);
  }
  Emit_Pack_Component(cfile,
		      NULL,
		      END,
		      first_comps,
		      &pack_index);

  fprintf (cfile, "};\n");

  fprintf(hfile, "\ninline const ISA_BUNDLE_PACK_INFO *ISA_BUNDLE_Pack_Info(void)\n"
		 "{\n"
		 "  extern const ISA_BUNDLE_PACK_INFO ISA_BUNDLE_pack_info[];\n"
		 "  return ISA_BUNDLE_pack_info;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_BUNDLE_PACK_INFO_Comp(const ISA_BUNDLE_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->comp;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_BUNDLE_PACK_INFO_Index(const ISA_BUNDLE_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->index;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_BUNDLE_PACK_INFO_CompPos(const ISA_BUNDLE_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->comp_pos;\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_BUNDLE_PACK_INFO_BundlePos(const ISA_BUNDLE_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->bundle_pos;\n"
		 "}\n");

  fprintf(hfile, "\ninline UINT64 ISA_BUNDLE_PACK_INFO_Mask(const ISA_BUNDLE_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->mask;\n"
		 "}\n");

  fprintf(efile, "ISA_BUNDLE_pack_info_index\n");

  fprintf(cfile, "\nconst mUINT8 ISA_BUNDLE_pack_info_index[%d] = {\n",
		 MAX_PACK_COMPS);
  for (i = 0; i < MAX_PACK_COMPS; ++i) {
    int index = first_comps[i];
    if (index < 0) index = first_comps[END];
    fprintf(cfile, "  %2d, /* %s */\n", index, pack_comp_name[i]);
  }
  fprintf(cfile, "};\n");

  fprintf(hfile, "\ninline INT ISA_BUNDLE_Pack_Info_Index(ISA_BUNDLE_PACK_COMP comp)\n"
		 "{\n"
		 "  extern const mUINT8 ISA_BUNDLE_pack_info_index[%d];\n"
		 "  return ISA_BUNDLE_pack_info_index[(INT)comp];\n"
		 "}\n",
		 MAX_PACK_COMPS);
}

/* ====================================================================
 *
 * This section handles the common interfaces
 *
 * ====================================================================
 */

/////////////////////////////////////
void ISA_Bundle_Begin( const char* /* name */, int bundle_width )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  bundle_bits = bundle_width;
}

/////////////////////////////////////
void ISA_Bundle_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{

#define FNAME "targ_isa_bundle"
  char buf[1000];
  sprintf (buf, "%s.h", FNAME);
  FILE* hfile = fopen(buf, "w");
  sprintf (buf, "%s.c", FNAME);
  FILE* cfile = fopen(buf, "w");
  sprintf (buf, "%s.Exported", FNAME);
  FILE* efile = fopen(buf, "w");

  fprintf(hfile, "#include \"topcode.h\"\n");
  fprintf(cfile,"#include \"%s.h\"\n\n", FNAME);

  sprintf (buf, "%s", FNAME);
  Emit_Header (hfile, buf, interface);

  Emit_Bundle_Scheduling(hfile, cfile, efile);
  fprintf(hfile, "\f");
  fprintf(cfile, "\f");
  Emit_Bundle_Packing(hfile, cfile, efile);
		   
  Emit_Footer (hfile);
}
