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


// isa_decode_gen.cxx
/////////////////////////////////////
//
//  Generate an interface for decoding instructions.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_decode_gen.cxx,v $

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <alloca.h>
#include <list>
#include "topcode.h"
#include "gen_util.h"
#include "isa_decode_gen.h"

typedef enum { INST_STATE, UNIT_STATE } STATE_TYPE;

typedef enum {
  VISIT_UNVISITED,
  VISIT_GEN_DATA,
  VISIT_GEN_CODE
} VISIT_KIND;

/* This struct is used to hold the information that describes
 * a state. The information is dependent on whether or not the
 * state is a final state or not.
 *
 * Final states are described completely by a topcode.
 *
 * Intermediate states, have one or more transitions to new states,
 * and a bitfield description of an instruction being decoded,
 * that determines which transition is taken.
 */
struct state {
  bool is_final;
  VISIT_KIND visit;
  union {
    TOP final;		// final state
    struct {		// intermediate state
      const char *tag;
      int idx;
      int pos;
      int width;
      STATE *transition;
      STATE_TYPE stype;
      int casenum;
    } i;
  } u;
};

static STATE initial_state;
static std::list<STATE> all_states;

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   Utilities for decoding binary instructions. The following",
  " *   are exported:",
  " *",
  " *   TOP ISA_Decode_Inst(const ISA_PACK_INST *pinst, ISA_EXEC_UNIT unit)",
  " *       Decode the instruction pointed to by <pinst> in execution unit",
  " *       <unit> and return its opcode by function return value.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


/////////////////////////////////////
void ISA_Decode_Begin(const char * /* name */)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
}

/////////////////////////////////////
STATE Create_Unit_State(const char *tag, int pos, int width)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  int i;
  STATE state = new struct state;
  state->is_final = false;
  state->visit = VISIT_UNVISITED;
  state->u.i.tag = tag;
  state->u.i.idx = 0;
  state->u.i.pos = pos;
  state->u.i.width = width;  
  state->u.i.transition = new STATE[1 << width];
  for (i = 0; i < (1 << width); ++i) state->u.i.transition[i] = NULL;
  state->u.i.stype = UNIT_STATE;
  all_states.push_back(state);
  return state;
}

/////////////////////////////////////
STATE Create_Inst_State(const char *tag, int idx, int pos, int width)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  int i;
  STATE state = new struct state;
  state->is_final = false;
  state->visit = VISIT_UNVISITED;
  state->u.i.tag = tag;
  state->u.i.idx = idx;
  state->u.i.pos = pos;
  state->u.i.width = width;  
  state->u.i.transition = new STATE[1 << width];
  for (i = 0; i < (1 << width); ++i) state->u.i.transition[i] = NULL;
  state->u.i.stype = INST_STATE;
  all_states.push_back(state);
  return state;
}

/////////////////////////////////////
void Transitions(STATE state, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  int n;
  va_list ap;
  STATE transition;

  if (state->is_final) {
    fprintf(stderr, "### Error: can't specify transistions for a final state\n");
    exit(EXIT_FAILURE);
  }

  va_start(ap, state);
  while ((n = va_arg(ap, int)) != END_TRANSITIONS) {
    if (n < 0 || n >= (1 << state->u.i.width)) {
      fprintf(stderr, "### Error: transition %d of %s is out-of-range\n",
		      n, state->u.i.tag);
      exit(EXIT_FAILURE);
    }
    if (state->u.i.transition[n]) {
      fprintf(stderr, "### Error: transition %d of %s multiply specified\n",
		      n, state->u.i.tag);
      exit(EXIT_FAILURE);
    }
    transition = va_arg(ap, STATE);
    if ((unsigned long)transition < TOP_count) {
      fprintf(stderr, "### Error: transition %d of %s looks like it should be Final()\n",
		      n, state->u.i.tag);
      exit(EXIT_FAILURE);
    }
    state->u.i.transition[n] = transition;
  }
  va_end(ap);
}

/////////////////////////////////////
void Initial_State(STATE state)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  initial_state = state;
}

/////////////////////////////////////
STATE Final(TOP topcode)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  STATE state = new struct state;
  state->is_final = true;
  state->u.final = topcode;
  all_states.push_back(state);
  return state;
}

/////////////////////////////////////
static int Compare_NonFinals(const void *p1, const void *p2)
/////////////////////////////////////
//  
/////////////////////////////////////
{
  STATE s1 = *(STATE *)p1;
  STATE s2 = *(STATE *)p2;

  return strcmp(s1->u.i.tag, s2->u.i.tag);
}

/////////////////////////////////////
static void Visit_State(STATE state, FILE *f, VISIT_KIND vk)
/////////////////////////////////////
//  Visit <state> and the states it leads to and perform the
//  actions necessary for the kind of visitation specified by <vk>.
/////////////////////////////////////
{
  static int indent;
  int i;
  int ntrans;
  int max_top;
  STATE *nonfinals;
  int n_nonfinals;

  assert(!state->is_final);

  /* If we've been here, then for data, just return so we
   * don't generate the same data twice. For code, we emit
   * the whole thing again. Alternatively we could have
   * generated a label on the first state and then just
   * goto it here.
   */
  if (state->visit == vk && vk == VISIT_GEN_DATA) return;
  state->visit = vk;

  indent += 2;
  ntrans = 1 << state->u.i.width;

  nonfinals = (STATE *)alloca(sizeof(struct state) * ntrans);
  n_nonfinals = 0;
  for (i = 0; i < ntrans; ++i) {
    STATE newstate = state->u.i.transition[i];
    if (newstate) {
      if (!newstate->is_final) {
	nonfinals[n_nonfinals++] = newstate;
      } else {
	newstate->visit = vk;  // only used for unused state detection
      }
    }
  }
  qsort(nonfinals, n_nonfinals, sizeof(STATE *), Compare_NonFinals);

  if (vk == VISIT_GEN_CODE) {
    fprintf(f, "%*sopc = (", indent, "");
    if (state->u.i.stype == INST_STATE) {
      fprintf(f, "pinst[%d]", state->u.i.idx);
    } else {
      fprintf(f, "unit");
    }
    fprintf(f, " >> %d) & 0x%x;\n", state->u.i.pos, ntrans - 1);

    fprintf(f, "%*stop = state_%s_tab[opc];\n", indent, "", state->u.i.tag);
    if (n_nonfinals) fprintf(f, "%*sswitch (top) {\n", indent, "");
  }

  max_top = TOP_UNDEFINED;
  for (i = 0; i < n_nonfinals; ++i) {
    STATE newstate = nonfinals[i];
    ++max_top;
    newstate->u.i.casenum = max_top;
    if (vk == VISIT_GEN_CODE) {
      fprintf(f, "%*scase %3d: /* %s */\n",
		 indent, "",
		 max_top,
		 newstate->u.i.tag);
    } 
    if (   i + 1 == n_nonfinals 
	|| strcmp(newstate->u.i.tag, nonfinals[i+1]->u.i.tag))
    {
      Visit_State(newstate, f, vk);
      if (vk == VISIT_GEN_CODE) fprintf(f, "%*sbreak;\n", indent + 2, "");
    }
  }

  if (vk == VISIT_GEN_DATA) {
    int col;
    const char *top_type;
    if (max_top < 256) {
      top_type = "mUINT8";
    } else if (max_top < 65536) {
      top_type = "mUINT16";
    } else {
      top_type = "mUINT32";
    }
    fprintf(f, "\nstatic const %s state_%s_tab[%d] = {",
	       top_type,
	       state->u.i.tag, 
	       ntrans);
    col = 8;
    for (i = 0; i < ntrans; ++i) {
      STATE newstate = state->u.i.transition[i];
      if (col == 8) {
	fprintf(f, "\n ");
	col = 0;
      }
      if (newstate == NULL) {
	fprintf(f, " %4d,", TOP_UNDEFINED);
      } else if (newstate->is_final) {
	fprintf(f, " %4d,", newstate->u.final);
      } else {
	fprintf(f, " %4d,", newstate->u.i.casenum);
      }

      ++col;
    }

    fprintf(f, "\n};\n");
  } else if (vk == VISIT_GEN_CODE) {
    if (n_nonfinals) fprintf(f, "%*s}\n", indent, "");
  }
  indent -= 2;
}

/////////////////////////////////////
void ISA_Decode_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::list<STATE>::iterator state_iter;
  char buf[1000];
#define FNAME "targ_isa_decode"
  sprintf (buf, "%s.h", FNAME);
  FILE* hfile = fopen(buf, "w");
  sprintf (buf, "%s.c", FNAME);
  FILE* cfile = fopen(buf, "w");
  sprintf (buf, "%s.Exported", FNAME);
  FILE* efile = fopen(buf, "w");

  fprintf(cfile, "#include \"topcode.h\"\n"
		 "#include \"targ_isa_bundle.h\"\n"
			 "#include \"targ_isa_pack.h\"\n"
	 "#include \"%s.h\"\n\n", FNAME);

  sprintf (buf, "%s", FNAME);
  Emit_Header (hfile, buf, interface);
  fprintf(hfile, "#include \"topcode.h\"\n"
		 "#include \"targ_isa_bundle.h\"\n"
		 "#include \"targ_isa_pack.h\"\n");

  if (initial_state == NULL) {
    fprintf(stderr, "### Error: no initial decode state specified\n");
    exit(EXIT_FAILURE);
  }

  Visit_State(initial_state, cfile, VISIT_GEN_DATA);

  fprintf(efile, "ISA_Decode_Inst\n");

  fprintf(hfile, "\nextern TOP ISA_Decode_Inst(const ISA_PACK_INST *pinst, ISA_EXEC_UNIT unit);\n");

  fprintf(cfile, "\nTOP ISA_Decode_Inst(const ISA_PACK_INST *pinst, ISA_EXEC_UNIT unit)\n"
		 "{\n"
		 "  INT top;\n"
		 "  INT opc;\n");
  Visit_State(initial_state, cfile, VISIT_GEN_CODE);
  fprintf(cfile, "  return (TOP)top;\n"
		 "}\n");

  for (state_iter = all_states.begin();
       state_iter != all_states.end();
       ++state_iter)
  {
    STATE state = *state_iter;
    if (state->visit == VISIT_UNVISITED) {
      if (state->is_final) {
	fprintf(stderr, "### Warning: final state \"%s\" is unused\n",
		TOP_Name(state->u.final));
      } else {
        fprintf(stderr, "### Warning: intermediate state \"%s\" is unused\n",
		state->u.i.tag);
      }
    }
  }

  Emit_Footer (hfile);
}
