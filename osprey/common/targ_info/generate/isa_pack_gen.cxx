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


// isa_pack_gen.cxx
/////////////////////////////////////
//
//  Generate an interface for packing instructions into an instruction 
//  word for all instructions in the ISA.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_pack_gen.cxx,v $

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <list>
#include <vector>
#include <string.h>
#include "topcode.h"
#include "targ_isa_properties.h"
#include "gen_util.h"
#include "isa_pack_gen.h"

/* The maximum number of operands and results used by ANY target.
 * (It would be better to get the max operands and results from the
 * generated targ_isa_operands.h file -- Ken)
 */
#define MAX_OPNDS 6
#define MAX_RESULTS 2

typedef enum {
	END	= 0,			// end of list marker
	OPND    = 1,			// OPND+n => operand n
   	RESULT  = OPND+MAX_OPNDS,	// RESULT+n => result n
} COMP_TYPE;

#define MAX_LISTING_OPERANDS (RESULT+MAX_RESULTS)

typedef struct isa_args_type {
  unsigned char name;
  int   opnd_position;
  int   inst_position;
  int   width;
} *ISA_ARGS_TYPE;

typedef struct isa_adj_type {
  int opnd_index;
  OPND_ADJ_TYPE padj;
  OPND_ADJ_TYPE uadj;
} *ISA_ADJ_TYPE;

/* Define the maximum number of words an inst can have:
 */
#define MAX_WORDS 20

struct isa_pack_type {
  const char* name; 	// to represent the pack type
  struct {
    std::list<ISA_ARGS_TYPE> operands;
    std::list<ISA_ARGS_TYPE> results;
  } word[MAX_WORDS];
  std::list<ISA_ADJ_TYPE> oadj;
  int max_word;
  int index;		
  int adj_index;
};

// map to link TOPs_ with assmbly_info properties
struct op_assembly {
  isa_pack_type *desc;
  struct op_assembly *next;
  unsigned long long opcode_mask[MAX_WORDS];
};

struct opnd_adj_type {
  const char *name;
  const char *adj;
  int code;
};

static std::list<ISA_PACK_TYPE> all_packs;  // all the different print formats
static std::list<OPND_ADJ_TYPE> all_oadj;	// all the different opnd adjustments
static ISA_PACK_TYPE current_pack_desc;
static op_assembly *op_packs[TOP_count+1];
static std::list<op_assembly*> op_packs_list;
static bool top_specified[TOP_count];
static int inst_bits;
static int inst_words;
static int num_adj;
static int mask_width;

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of how to pack the operands of ISA instructions",
  " *   into binary object code. The description exports the following:",
  " *",
  " *   typedef ISA_PACK_INST",
  " *       A scalar type large enough to hold an instruction.",
  " *",
  " *   const INT ISA_PACK_INST_SIZE",
  " *       The size, in bits, of an instruction.",
  " *",
  " *   typedef (enum) ISA_PACK_COMP",
  " *       An enumeration of the instruction components to be packed.",
  " *",
  " *   typedef (struct) ISA_PACK_INFO",
  " *       Describes how the operands of a particular instruction",
  " *       are packed. The contents are private.",
  " *",
  " *   const INT ISA_PACK_COMP_MAX",
  " *       The maximum number of operands to be packed for any instruction.",
  " *",
  " *   UINT64 ISA_PACK_Init_Mask(TOP topcode, INT iword)",
  " *       Returns an initial bit mask to initialize the instruction word,",
  " *       with index 'iword', for the instruction specified by 'topcode'",
  " *",
  " *   INT ISA_PACK_Inst_Words(TOP topcode)",
  " *       Returns the number of instruction words for the specified topcode.",
  " *",
  " *   const ISA_PACK_INFO *ISA_PACK_Info(TOP topcode)",
  " *       Returns a pointer to the first packing descriptions for",
  " *       the instruction specified by 'topcode'. Increment",
  " *       the returned pointer to access any additional packing",
  " *       descriptions for the instruction. A packing description",
  " *       with component ISA_PACK_COMP_end marks the end.",
  " *",
  " *   INT ISA_PACK_INFO_Comp(const ISA_PACK_INFO *info)",
  " *       Identifies the instruction component to be packed.",
  " *",
  " *   INT ISA_PACK_INFO_OpndPos(const ISA_PACK_INFO *info)",
  " *       The offset, in bits, to the start of the component in the",
  " *       operand value.",
  " *",
  " *       ISA_PACK_INFO_OpndPos is meaningless for ISA_PACK_COMP_end.",
  " *",
  " *   INT ISA_PACK_INFO_InstPos(const ISA_PACK_INFO *info)",
  " *       The offset, in bits, to the start of the component in the",
  " *       instruction word.",
  " *",
  " *       ISA_PACK_INFO_InstPos is meaningless for ISA_PACK_COMP_end.",
  " *",
  " *   UINT64 ISA_PACK_INFO_Mask(const ISA_PACK_INFO *info)",
  " *       A bit-mask that is as wide as the instruction field being",
  " *       packed. The mask is normalized such that the lowest bit",
  " *       of the mask is in bit-0.",
  " *",
  " *       The mask is meaningless for ISA_PACK_COMP_end.",
  " *",
  " *   typedef (struct) ISA_PACK_ADJ_INFO",
  " *       Describes how the operands of a particular instruction",
  " *       are to be adjust. The contents are private.",
  " *",
  " *   const INT ISA_PACK_ADJ_END",
  " *       A reserved ISA_PACK_ADJ_INFO code which indicates the end",
  " *       of a sequence of adjustments.",
  " *",
  " *   const ISA_PACK_ADJ_INFO *ISA_PACK_Adj_Info(TOP topcode)",
  " *       Returns a pointer to the first operand adjustment description",
  " *       for the instruction specified by 'topcode'. Increment",
  " *       the returned pointer to access any additional adjustment",
  " *       descriptions for the instruction. An adjustment description",
  " *       with code ISA_PACK_ADJ_END marks the end.",
  " *",
  " *   INT ISA_PACK_ADJ_INFO_Code(const ISA_PACK_ADJ_INFO *info, BOOL invert)",
  " *       Identifies the adjustment to be made. If <invert> is FALSE,",
  " *       the adjustment is for packing; otherwise if <invert> is TRUE,",
  " *       the adjustment is for unpacking.",
  " *",
  " *   INT ISA_PACK_ADJ_INFO_OpndIdx(const ISA_PACK_ADJ_INFO *info)",
  " *       The index of the operand, i.e. the operand number, to be adjusted.",
  " *",
  " *   void ISA_PACK_Adjust_Operands(const ISA_PACK_ADJ_INFO *info,",
  " *                                 INT64 *opnd,",
  " *                                 BOOL invert)",
  " *       Apply adjustemnts to all operands of an instruction, as",
  " *       specified by <info>. <opnd> is a pointer to an array of",
  " *       of operand values for the instruction. If <invert> is FALSE,",
  " *       the adjustment is for packing; otherwise if <invert> is TRUE,",
  " *       the adjustment is for unpacking.",
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
	comp_name[i] = strdup("ISA_PACK_COMP_end");
      } else if (i == OPND) {
	comp_name[i] = strdup("ISA_PACK_COMP_opnd");
      } else if (i > OPND && i < (OPND + MAX_OPNDS)) {
	sprintf(buf, "ISA_PACK_COMP_opnd+%d", i - OPND);
	comp_name[i] = strdup(buf);
      } else if (i == RESULT) {
	comp_name[i] = strdup("ISA_PACK_COMP_result");
      } else {
	assert(i > RESULT && i < (RESULT + MAX_RESULTS));
	sprintf(buf, "ISA_PACK_COMP_result+%d", i - RESULT);
	comp_name[i] = strdup(buf);
      }
    }
    initialized = true;
  }

  return comp_name[print_index];
}

/////////////////////////////////////
void ISA_Pack_Begin( const char* /* name */, int inst_bit_size )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  inst_bits = inst_bit_size;
}

/////////////////////////////////////
ISA_PACK_TYPE ISA_Pack_Type_Create ( const char* name)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_PACK_TYPE result = new isa_pack_type;
  result->name = name;
  result->max_word = 0;
  current_pack_desc = result;
  all_packs.push_back (current_pack_desc);
  return result;
}

/////////////////////////////////////
OPND_ADJ_TYPE Create_Operand_Adjustment(const char *name, const char *adj)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  OPND_ADJ_TYPE oadj = new opnd_adj_type;
  oadj->name = name;
  oadj->adj = adj;
  oadj->code = ++num_adj;
  all_oadj.push_back (oadj);
  return oadj;
}

/////////////////////////////////////
void Instruction_Pack_Group(ISA_PACK_TYPE pack_type, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  TOP opcode;

  int words = current_pack_desc->max_word + 1;
  if (words > inst_words) inst_words = words;

  va_start(ap, pack_type);
  for (opcode = static_cast<TOP>(va_arg(ap, int)); 
       opcode != TOP_UNDEFINED; 
       opcode = static_cast<TOP>(va_arg(ap, int)))
  {
    int i;
    op_assembly *op_pack = new op_assembly;
    for (i = 0; i < words; ++i) {
      unsigned long long opcode_mask;
      opcode_mask = inst_bits > 32 ? va_arg(ap,unsigned long long)
				   : va_arg(ap,unsigned long);
      op_pack->opcode_mask[i] = opcode_mask;
    }
    for (i = words; i < MAX_WORDS; ++i) op_pack->opcode_mask[i] = 0;
    top_specified[(int)opcode] = true;
    op_packs_list.push_back(op_pack);
    op_pack->desc = current_pack_desc;
    op_packs[(int)opcode] = op_pack;
  }
  va_end(ap);
}

/////////////////////////////////////
void Adjust_Operand(int operand_index,
		    OPND_ADJ_TYPE pack_adj,
		    OPND_ADJ_TYPE unpack_adj)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_ADJ_TYPE curr_adj = new isa_adj_type;

  if (operand_index >= MAX_OPNDS) {
    fprintf(stderr, "### Error: operand index (%d) exceeds %d\n",
		    operand_index, MAX_OPNDS-1);
    exit(EXIT_FAILURE);
  }
  curr_adj->opnd_index = operand_index;
  curr_adj->padj = pack_adj;
  curr_adj->uadj = unpack_adj;
  current_pack_desc->oadj.push_back (curr_adj);
}

/////////////////////////////////////
void Operand (int operand_index, 
	      int opnd_position, 
	      int inst_position, 
	      int width)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_ARGS_TYPE curr_args_type = new isa_args_type;
  int w = current_pack_desc->max_word;

  if (operand_index >= MAX_OPNDS) {
    fprintf(stderr, "### Error: operand index (%d) exceeds %d\n",
		    operand_index, MAX_OPNDS-1);
    exit(EXIT_FAILURE);
  }
  curr_args_type->name = OPND + operand_index;
  curr_args_type->opnd_position = opnd_position;
  curr_args_type->inst_position = inst_position;
  curr_args_type->width = width;
  current_pack_desc->word[w].operands.push_back (curr_args_type);

  if (width > mask_width) mask_width = width;
}

/////////////////////////////////////
void Result (int result_index, int bit_position, int width)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_ARGS_TYPE curr_args_type = new isa_args_type;
  int w = current_pack_desc->max_word;

  if (result_index >= MAX_RESULTS) {
    fprintf(stderr, "### Error: result index (%d) exceeds %d\n",
		    result_index, MAX_RESULTS-1);
    exit(EXIT_FAILURE);
  }
  curr_args_type->name = RESULT + result_index;
  curr_args_type->inst_position = bit_position;
  curr_args_type->opnd_position = 0;
  curr_args_type->width = width;
  current_pack_desc->word[w].results.push_back (curr_args_type);

  if (width > mask_width) mask_width = width;
}


/////////////////////////////////////
void Next_Word (void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ++current_pack_desc->max_word;
  if (current_pack_desc->max_word >= MAX_WORDS) {
    fprintf(stderr, "### Error: number of inst words (%d) exceeds MAX_WORDS\n",
		    current_pack_desc->max_word+1);
    exit(EXIT_FAILURE);
  }
}

/////////////////////////////////////
void ISA_Pack_Is_Unused (void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  int i;
  int top;
  current_pack_desc = new isa_pack_type;
  current_pack_desc->name = "dummy";
  current_pack_desc->max_word = 0;
  inst_words = 1;
  all_packs.push_back (current_pack_desc);
  for (top = 0; top < TOP_count; ++top) {
    bool is_dummy = TOP_is_dummy((TOP)top);
    bool is_simulated = TOP_is_simulated((TOP)top);
    if (!is_dummy && !is_simulated) {
      top_specified[top] = true;
      op_assembly *op_pack = new op_assembly;
      for (i = 0; i < MAX_WORDS; ++i) {
        op_pack->opcode_mask[i] = 0;
      }
      op_packs_list.push_back(op_pack);
      op_pack->desc = current_pack_desc;
      op_packs[top] = op_pack;
    }
  }
}

/////////////////////////////////////
static unsigned long long Mask(int width)
{
  if (width > 64) {
    fprintf(stderr, "### Error: field width (%d) exceeds 64\n", width);
    exit(EXIT_FAILURE);
  } else if (width == 64) {
    return -1ULL;
  }
  return (1ULL << width) - 1;
}


/////////////////////////////////////
void ISA_Pack_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  enum { isa_pack_adj_end = 0 };
  std::list<ISA_PACK_TYPE>::iterator isi;
  std::list<ISA_ARGS_TYPE>::iterator iai;
  std::list<ISA_ADJ_TYPE>::iterator ioi;
  std::list<OPND_ADJ_TYPE>::iterator ioai;
#define FNAME "targ_isa_pack"
  char buf[1000];
  sprintf (buf, "%s.h", FNAME);
  FILE* hfile = fopen(buf, "w");
  sprintf (buf, "%s.c", FNAME);
  FILE* cfile = fopen(buf, "w");
  sprintf (buf, "%s.Exported", FNAME);
  FILE* efile = fopen(buf, "w");
  int w, i, index;
  const char * const isa_pack_index_format = "  %3d,  /* %s: %s */\n";
  const char * const isa_pack_words_format = "  %3d,  /* %s */\n";
  const char * const isa_pack_null_format = 
			"  { %-22s, %2d, %2d,   %*d },  /* %s */\n";
#ifdef TARG_IA64
  const char * const isa_pack_operand_format = 
			"  { %-22s, %2d, %2d, 0x%0*llxLL },  /* %s, OPND%d */\n";
  const char * const isa_pack_result_format = 
			"  { %-22s, %2d, %2d, 0x%0*llxLL },  /* %s, RESULT%d */\n";
#else
  const char * const isa_pack_operand_format = 
            "  { %-22s, %2d, %2d, 0x%0*llx },  /* %s, OPND%d */\n";
  const char * const isa_pack_result_format = 
            "  { %-22s, %2d, %2d, 0x%0*llx },  /* %s, RESULT%d */\n";
#endif

  int init_digits;
  int mask_digits;
  int top;
  bool err;
  bool only_zero_opndpos;
  const char *info_index_type;

#ifndef TARG_LOONGSON
  for (err = false, top = 0; top < TOP_count; ++top) {
    bool is_dummy = TOP_is_dummy((TOP)top);
    bool is_simulated = TOP_is_simulated((TOP)top);
    if (!top_specified[top]) {
      if (!is_simulated && !is_dummy) {
	fprintf(stderr, "### Error: no pack specification for %s\n",
		        TOP_Name((TOP)top));
	err = true;
      }
    } else if (is_dummy) {
      fprintf(stderr, "### Error: pack specification for dummy op %s\n",
		      TOP_Name((TOP)top));
      err = true;
    } else if (is_simulated) {
      fprintf(stderr, "### Error: pack specification for simulated op %s\n",
		      TOP_Name((TOP)top));
      err = true;
    }
  }
  if (err) exit(EXIT_FAILURE);
#endif

  // setup types and formats depending on instruction size.
  if (inst_bits > 32) {
    init_digits = 64 / 4;
  } else if (inst_bits > 16) {
    init_digits = 32 / 4;
  } else if (inst_bits > 8) {
    init_digits = 16 / 4;
  } else {
    init_digits = 8 / 4;
  }

  if (mask_width > 32) {
    mask_digits = 64 / 4;
  } else if (mask_width > 16) {
    mask_digits = 32 / 4;
  } else if (mask_width > 8) {
    mask_digits = 16 / 4;
  } else {
    mask_digits = 8 / 4;
  }

  // for some archs, opndpos will always be zero, so detect those
  // archs so we can optimize the interface.
  only_zero_opndpos = true;
  for ( isi = all_packs.begin(); isi != all_packs.end(); ++index, ++isi ) {
    ISA_PACK_TYPE curr_ptype = *isi;
    for (w = 0; w <= curr_ptype->max_word; ++w) {
      for ( iai = curr_ptype->word[w].operands.begin();
	    iai != curr_ptype->word[w].operands.end();
	    ++iai) 
      {
	ISA_ARGS_TYPE curr_atype =  *iai;
	if (curr_atype->opnd_position != 0) only_zero_opndpos = false;
      }
      for ( iai = curr_ptype->word[w].results.begin(); 
	    iai != curr_ptype->word[w].results.end(); 
	    ++iai)
      {
	ISA_ARGS_TYPE curr_atype =  *iai;
	if (curr_atype->opnd_position != 0) only_zero_opndpos = false;
      }
    }
  }

  fprintf(cfile,"#include \"topcode.h\"\n");
  fprintf(cfile,"#include \"%s.h\"\n\n", FNAME);

  sprintf (buf, "%s", FNAME);
  Emit_Header (hfile, buf, interface);
  fprintf(hfile,"#include \"topcode.h\"\n");
  if (inst_words == 1) fprintf(hfile,"#include \"targ_isa_properties.h\"\n");

  fprintf(hfile, "\ntypedef mUINT%d ISA_PACK_INST;\n", init_digits * 4);

  fprintf(hfile, "\n#define ISA_PACK_INST_SIZE (%d)\n", inst_bits);

  fprintf(hfile, "\n#define ISA_PACK_MAX_INST_WORDS (%d)\n", inst_words);

  fprintf(hfile, "\ntypedef enum {\n"
	"  %-20s = %d,  /* %s */\n"
	"  %-20s = %d,  /* %s */\n"
   	"  %-20s = %d,  /* %s */\n"
   	"  %-20s = %d   /* %s */\n"
	"} ISA_PACK_COMP;\n",
	Print_Name(END), END, "End of list marker",
	Print_Name(OPND), OPND, "OPND+n => operand n",
	Print_Name(RESULT), RESULT, "RESULT+n => result n",
        "ISA_PACK_COMP_MAX", MAX_LISTING_OPERANDS-1, "Last component");

  fprintf(hfile, "\ntypedef struct {\n"
		"  mUINT8 comp;\n"
  		"  mUINT8 opndpos;\n"
  		"  mUINT8 instpos;\n"
		"  mUINT%d mask;\n" 
		"} ISA_PACK_INFO;\n",
		mask_digits * 4);

  fprintf(efile, "ISA_PACK_info\n");

  fprintf(cfile, "\nconst ISA_PACK_INFO ISA_PACK_info[] = {\n");

  fprintf (cfile, isa_pack_null_format, Print_Name(END), -1, -1, mask_digits, -1, "UNDEFINED");
  index = 1;
  for ( isi = all_packs.begin(); isi != all_packs.end(); ++isi ) {
    ISA_PACK_TYPE curr_ptype = *isi;
    curr_ptype->index = index;
    for (w = 0; w <= curr_ptype->max_word; ++w, ++index) {
      i = 0;
      for ( iai = curr_ptype->word[w].operands.begin(); 
	    iai != curr_ptype->word[w].operands.end(); ++index, ++iai) {
	ISA_ARGS_TYPE curr_atype =  *iai;
	fprintf (cfile, isa_pack_operand_format,
			Print_Name(curr_atype->name), 
			curr_atype->opnd_position,
			curr_atype->inst_position,
			mask_digits, Mask(curr_atype->width),
			curr_ptype->name, 
			i++);
      }
      i = 0;
      for ( iai = curr_ptype->word[w].results.begin(); 
	    iai != curr_ptype->word[w].results.end(); ++index, ++iai) {
	ISA_ARGS_TYPE curr_atype =  *iai;
    	fprintf (cfile, isa_pack_result_format,
			Print_Name(curr_atype->name), 
			curr_atype->opnd_position,
			curr_atype->inst_position,
			mask_digits, Mask(curr_atype->width),
			curr_ptype->name,
			i++);
      }
      fprintf (cfile, isa_pack_null_format, 
		      Print_Name(END), 
		      -1,
		      -1, 
		      mask_digits, -1,
		      curr_ptype->name);
    }
  }
  fprintf (cfile, "};\n\n");

  // select the ISA_PACK_info_index based on the number of packing types.
  if (index < 1<<8) {
    info_index_type = "mUINT8";
  } else {
    assert (index < 1<<16);
    info_index_type = "mUINT16";
  }

  fprintf(efile, "ISA_PACK_info_index\n");

  fprintf(cfile, "\nconst %s ISA_PACK_info_index[%d] = {\n", 
		 info_index_type, TOP_count);
  for (top = 0; top < TOP_count; ++top ) {
  	op_assembly *op_pack = op_packs[top];
    	if (op_pack) {
  	    fprintf(cfile, isa_pack_index_format,
			op_pack->desc->index,
			TOP_Name((TOP)top),
			op_pack->desc->name);
	} else {
  	    fprintf(cfile, "  %3d,  /* %s */\n", 
			0,
			TOP_Name((TOP)top));
	}
  }
  fprintf(cfile, "};\n");

  fprintf(efile, "ISA_PACK_init_mask\n");

  fprintf(cfile, "\nconst mUINT%d ISA_PACK_init_mask[%d][%d] = {\n",
		 init_digits * 4,
		 TOP_count,
		 inst_words);
  for (top = 0; top < TOP_count; ++top ) {
    op_assembly *op_pack = op_packs[top];
    fprintf(cfile, "  {");
    for (w = 0; w < inst_words; ++w) {
      fprintf(cfile, " 0x%0*" LL_FORMAT "xLL,",
		     init_digits, op_pack ? op_pack->opcode_mask[w] : 0LL);
    }
    fprintf(cfile, " }, /* %s */\n", TOP_Name((TOP)top));

  }
  fprintf(cfile, "};\n");

  if (inst_words > 1) {
    fprintf(efile, "ISA_PACK_inst_words\n");

    fprintf(cfile, "\nconst mUINT8 ISA_PACK_inst_words[%d] = {\n", TOP_count);
    for (top = 0; top < TOP_count; ++top ) {
      op_assembly *op_pack = op_packs[top];
      int words = op_pack ? op_pack->desc->max_word + 1 
			  : (TOP_is_dummy(top) ? 0 : 1);
      fprintf(cfile, isa_pack_words_format,
		     words,
		     TOP_Name((TOP)top));
    }
    fprintf(cfile, "};\n");
  }

  fprintf(hfile, "\ninline UINT64 ISA_PACK_Init_Mask(TOP topcode, INT iword)\n"
		 "{\n"
		 "  extern const mUINT%d ISA_PACK_init_mask[%d][%d];\n"
		 "  return ISA_PACK_init_mask[(INT)topcode][iword];\n"
		 "}\n",
		 init_digits * 4,
		 TOP_count,
		 inst_words);

  fprintf(hfile, "\ninline INT ISA_PACK_Inst_Words(TOP topcode)\n"
		 "{\n");
  if (inst_words == 1) {
#if defined(TARG_SL)
    fprintf(hfile, "  return TOP_is_dummy(topcode) ? 0 : (TOP_is_instr16(topcode) ? 1 : 2);\n"); // SL inst are by hword count
#else
    fprintf(hfile, "  return TOP_is_dummy(topcode) ? 0 : 1;\n");
#endif
  } else {
    fprintf(hfile, "  extern const mUINT8 ISA_PACK_inst_words[%d];\n"
		   "  return ISA_PACK_inst_words[(INT)topcode];\n",
		   TOP_count);
  }
  fprintf(hfile, "}\n");

  fprintf(hfile, "\ninline const ISA_PACK_INFO *ISA_PACK_Info(TOP topcode)\n"
		 "{\n"
		 "  extern const %s ISA_PACK_info_index[];\n"
		 "  extern const ISA_PACK_INFO ISA_PACK_info[];\n"
		 "  INT index = ISA_PACK_info_index[(INT)topcode];\n"
		 "  return index == 0 ? 0 : &ISA_PACK_info[index];\n"
		 "}\n",
		 info_index_type);

  fprintf(hfile, "\ninline INT ISA_PACK_INFO_Comp(const ISA_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->comp;\n"
		 "}\n");

  fprintf(hfile, "\n%s"
		 "inline INT ISA_PACK_INFO_OpndPos(const ISA_PACK_INFO *info)\n"
		 "{\n"
		 "  return %s;\n"
		 "}\n",
		 only_zero_opndpos ? "/*ARGSUSED*/\n" : "",
		 only_zero_opndpos ? "0" : "info->opndpos");

  fprintf(hfile, "\ninline INT ISA_PACK_INFO_InstPos(const ISA_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->instpos;\n"
		 "}\n");

  fprintf(hfile, "\ninline UINT64 ISA_PACK_INFO_Mask(const ISA_PACK_INFO *info)\n"
		 "{\n"
		 "  return info->mask;\n"
		 "}\n");

  fprintf(hfile, "\ntypedef struct {\n"
		 "  mUINT8 code[2];\n"
		 "  mUINT8 opndidx;\n"
		 "} ISA_PACK_ADJ_INFO;\n");

  fprintf(hfile, "\nenum { ISA_PACK_ADJ_END = %d };\n",
		 isa_pack_adj_end);

  fprintf(efile, "ISA_PACK_adj_info\n");

  fprintf(cfile, "\nconst ISA_PACK_ADJ_INFO ISA_PACK_adj_info[] = {\n"
		 "  { { %2d, %2d }, -1 },  /* [ 0]: ISA_PACK_ADJ_END */\n",
		 isa_pack_adj_end, isa_pack_adj_end);
  index = 1;
  for ( isi = all_packs.begin(); isi != all_packs.end(); ++isi ) {
    ISA_PACK_TYPE curr_ptype = *isi;
    if (curr_ptype->oadj.begin() != curr_ptype->oadj.end()) {
      curr_ptype->adj_index = index;
      for ( ioi = curr_ptype->oadj.begin(); 
	    ioi != curr_ptype->oadj.end(); 
	    ++index, ++ioi)
      {
	ISA_ADJ_TYPE curr_adj = *ioi;
	fprintf(cfile, "  { { %2d, %2d }, %2d },  /* [%2d]: %s (%s) operand %d*/\n", 
		       curr_adj->padj->code, 
		       curr_adj->uadj->code, 
		       curr_adj->opnd_index,
		       index,
		       curr_adj->padj->name,
		       curr_adj->uadj->name,
		       curr_adj->opnd_index);
      }
      fprintf (cfile, "  { { %2d, %2d }, -1 },  /* [%2d]: ISA_PACK_ADJ_END */\n", 
		      isa_pack_adj_end, isa_pack_adj_end, index);
      ++index;
    } else {
      curr_ptype->adj_index = 0;
    }
  }
  fprintf(cfile, "};\n");

  fprintf(hfile, "\ninline INT ISA_PACK_ADJ_INFO_Code(const ISA_PACK_ADJ_INFO *info, BOOL invert)\n"
		 "{\n"
		 "  return info->code[invert];\n"
		 "}\n");

  fprintf(hfile, "\ninline INT ISA_PACK_ADJ_INFO_OpndIdx(const ISA_PACK_ADJ_INFO *info)\n"
		 "{\n"
		 "  return info->opndidx;\n"
		 "}\n");

  fprintf(efile, "ISA_PACK_adj_info_index\n");

  fprintf(cfile, "\nconst mUINT8 ISA_PACK_adj_info_index[] = {\n");
  for (top = 0; top < TOP_count; ++top ) {
    op_assembly *op_pack = op_packs[top];
    fprintf(cfile, "  %2d,  /* %s */\n",
		   op_pack ? op_pack->desc->adj_index : 0,
		   TOP_Name((TOP)top));
  }
  fprintf(cfile, "};\n");

  fprintf(hfile, "\ninline const ISA_PACK_ADJ_INFO *ISA_PACK_Adj_Info(TOP topcode)\n"
		 "{\n"
		 "  extern const ISA_PACK_ADJ_INFO ISA_PACK_adj_info[];\n"
		 "  extern const mUINT8 ISA_PACK_adj_info_index[];\n"
		 "  INT index = ISA_PACK_adj_info_index[(INT)topcode];\n"
		 "  return index == 0 ? 0 : &ISA_PACK_adj_info[index];\n"
		 "}\n");

  fprintf(hfile, "\nextern void ISA_PACK_Adjust_Operands(const ISA_PACK_ADJ_INFO *info,\n"
		 "                                       INT64 *opnd,\n"
		 "                                       BOOL invert);\n");

  fprintf(efile, "ISA_PACK_Adjust_Operands\n");

  fprintf(cfile, "\n"
		 "void ISA_PACK_Adjust_Operands(const ISA_PACK_ADJ_INFO *info,\n"
		 "                              INT64 *opnd,\n"
		 "                              BOOL invert)\n"
		 "{\n"
		 "  INT code;\n"
		 "\n"
		 "  for (; (code = ISA_PACK_ADJ_INFO_Code(info, invert)) != ISA_PACK_ADJ_END; ++info) {\n"
		 "    INT index = ISA_PACK_ADJ_INFO_OpndIdx(info);\n"
		 "    INT64 O_VAL = opnd[index];\n"
		 "    switch (code) {\n");
  for ( ioai = all_oadj.begin(); ioai != all_oadj.end(); ++ioai ) {
    OPND_ADJ_TYPE curr_oadj = *ioai;
    fprintf(cfile, "    case %2d:  /* %s */\n"
		   "      O_VAL = (%s);\n"
		   "      break;\n",
		   curr_oadj->code, curr_oadj->name,
		   curr_oadj->adj);
  }
  fprintf(cfile, "    }\n"
		 "    opnd[index] = O_VAL;\n"
		 "  }\n"
		 "}\n");

  Emit_Footer (hfile);

  fclose(hfile);
  fclose(cfile);
  fclose(efile);
}
