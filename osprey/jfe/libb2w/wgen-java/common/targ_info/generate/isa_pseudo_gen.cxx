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


// isa_pseudo_gen.cxx
/////////////////////////////////////
//
//  Generate an interface for pseudo instructions.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_pseudo_gen.cxx,v $

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <strings.h>
#include <alloca.h>
#include <list>
#include <vector>
#include "topcode.h"
#include "gen_util.h"
#include "targ_isa_operands.h"
#include "isa_pseudo_gen.h"

#define MAX_REQUIRE 10
#define MAX_MAP (ISA_OPERAND_max_results + ISA_OPERAND_max_operands)

typedef enum { 
  UNKNOWN_DIRECTION,
  MACHINE_TO_PSEUDO, 
  PSEUDO_TO_MACHINE 
} DIRECTION;

typedef struct pseudo_op {
  DIRECTION dir;
  TOP to_opc;
  TOP from_opc;
  int index;
  int nrequire;
  int require[MAX_REQUIRE+1];
  int nmap;
  int map[2][MAX_MAP+1];
} PSEUDO_OP_INFO;

static int num_pseudos;
static std::list<PSEUDO_OP_INFO *> pseudos;
static PSEUDO_OP_INFO *cur_pseudo;
static int max_require;
static int max_map;

typedef enum expr_kind {
  EXPR_BOOL,
  EXPR_ARG_LVALUE,
  EXPR_ARG_RVALUE
} EXPR_KIND;

typedef struct expr {
  struct expr *next;
  EXPR_KIND kind;
  char s[1]; // must be last
} EXPR;

static EXPR exprs;
static EXPR *last_expr = &exprs;

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   Utilities for pseudo instructions. The following are exported:",
  " *",
  " *   typedef (enum) ISA_PSEUDO_DIRECTION",
  " *       Specifies the direction of a machine/pseudo instruction",
  " *       translation:",
  " *",
  " *       ISA_PSEUDO_to_pseudo -- translate from machine to pseudo instruction",
  " *       ISA_PSEUDO_to_machine -- translate from pseudo to machine instruction",
  " *",
  " *   TOP ISA_PSEUDO_Translate(TOP opc,",
  " *                            INT64 *r,",
  " *                            INT64 *o,",
  " *                            ISA_PSEUDO_DIRECTION dir)",
  " *       Translate the instruction with opcode <opc>, results",
  " *       array <r> and operands array <o>. The direction of the",
  " *       translation is controlled by <dir>. If there is a translation",
  " *       return the translated topcode by function return value, and",
  " *       the possibly modified operands and results by the <o> and <r>",
  " *       arrays. If there is no translation, return <opc> by function",
  " *       value, with unmodified operands and results. If an internal",
  " *       error is detected, TOP_UNDEFINED is returned rather than",
  " *       asserting.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};

/////////////////////////////////////
void ISA_Pseudo_Begin(const char * /* name */)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
}

/////////////////////////////////////
static int save_expr(const char *s, EXPR_KIND kind)
{
/////////////////////////////////////
//  Save a unique copy of <s> for the expression kind <kind> and
//  return it's identifier, which is a integer (it's really the
//  nth string of kind <kind> in the list of all expressions.
/////////////////////////////////////
  EXPR *e;
  int i = 1;
  for (e = exprs.next; e; e = e->next) {
    if (kind == e->kind) {
      if (strcmp(s, e->s) == 0) return i;
      ++i;
    }
  }
  e = (EXPR *)malloc(sizeof(EXPR) + strlen(s));
  e->next = NULL;
  e->kind = kind;
  strcpy(e->s, s);
  last_expr->next = e;
  last_expr = e;
  return i;
}


/////////////////////////////////////
void Machine_To_Pseudo(TOP pseudo, TOP machine)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  cur_pseudo = new PSEUDO_OP_INFO;
  pseudos.push_back(cur_pseudo);
  bzero(cur_pseudo, sizeof(PSEUDO_OP_INFO));
  cur_pseudo->from_opc = machine;
  cur_pseudo->to_opc = pseudo;
  cur_pseudo->dir = MACHINE_TO_PSEUDO;
  num_pseudos++;
}


/////////////////////////////////////
void Pseudo_To_Machine(TOP machine, TOP pseudo)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  cur_pseudo = new PSEUDO_OP_INFO;
  pseudos.push_back(cur_pseudo);
  bzero(cur_pseudo, sizeof(PSEUDO_OP_INFO));
  cur_pseudo->from_opc = pseudo;
  cur_pseudo->to_opc = machine;
  cur_pseudo->dir = PSEUDO_TO_MACHINE;
  num_pseudos++;
}


/////////////////////////////////////
void Require(const char *bool_expr)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (cur_pseudo == NULL) {
    fprintf(stderr, "### Error: must specify a translation prior to "
		    "Require(\"%s\")\n",
		    bool_expr);
    exit(EXIT_FAILURE);
  }
  if (cur_pseudo->dir != MACHINE_TO_PSEUDO) {
    fprintf(stderr, "### Error: Require() only valid for machine-to-pseudo "
		    "op translations\n");
    exit(EXIT_FAILURE);
  }

  int idx = cur_pseudo->nrequire;
  if (idx == MAX_REQUIRE) {
    fprintf(stderr, "too many requirements for pseudo %s\n", 
                    TOP_Name(cur_pseudo->to_opc));
    exit(EXIT_FAILURE);
  }
  cur_pseudo->require[idx] = save_expr(bool_expr, EXPR_BOOL);
  cur_pseudo->nrequire = ++idx;
  if (idx > max_require) max_require = idx;
}


/////////////////////////////////////
void Map_Arg(const char *lvalue, const char *expr)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (cur_pseudo == NULL) {
    fprintf(stderr, "### Error: must specify a translation prior to "
		    "Map_Arg(\"%s\", \"%s\")\n",
		    lvalue, expr);
    exit(EXIT_FAILURE);
  }
  if (strcmp(lvalue, expr) != 0) {
    int idx = cur_pseudo->nmap;
    if (idx == MAX_MAP) {
      fprintf(stderr, "too many arg mappings for pseudo %s\n", 
                      TOP_Name(cur_pseudo->to_opc));
      exit(EXIT_FAILURE);
    }
    cur_pseudo->map[0][idx] = save_expr(lvalue, EXPR_ARG_LVALUE);
    cur_pseudo->map[1][idx] = save_expr(expr, EXPR_ARG_RVALUE);
    cur_pseudo->nmap = ++idx;
    if (idx > max_map) max_map = idx;
  }
}


/////////////////////////////////////
static int compare_pseudos(const void *p1, const void *p2)
/////////////////////////////////////
//  qsort comparison function for sorting pseudos.
/////////////////////////////////////
{
  PSEUDO_OP_INFO *ps1 = *(PSEUDO_OP_INFO **)p1;
  PSEUDO_OP_INFO *ps2 = *(PSEUDO_OP_INFO **)p2;

  /* Sort so pseudo-to-machine translations come first.
   */
  if (ps1->dir != ps2->dir) {
    if (ps1->dir == PSEUDO_TO_MACHINE) return -1;
    return 1;
  }

  /* Sort so that machine-to-pseudo translations are grouped together
   * by "from" opcode; if same opcode then sort according to number 
   * of operands required to match such that pseudos with larger number 
   * of matches required come first.
   */
  int cmp = ps1->from_opc - ps2->from_opc;
  if (cmp == 0) cmp = ps2->nrequire - ps1->nrequire;
  return cmp;
}


/////////////////////////////////////
static void verify_machine_to_pseudo(PSEUDO_OP_INFO **vec, int i)
/////////////////////////////////////
//  vec[i] is the first of a sequence of one or more machine-to-pseudo
//  translations for the same "from" opcode. Verify that there aren't
//  duplicate translations.
/////////////////////////////////////
{
  int j;
  int k;
  int n;
  TOP from_opc = vec[i]->from_opc;
  for (j = i; j < num_pseudos && vec[j]->from_opc == from_opc; ++j) {
    for (k = j + 1; k < num_pseudos && vec[k]->from_opc == from_opc; ++k) {
      if (vec[j]->nrequire != vec[k]->nrequire) break;

      for (n = 0; n < vec[j]->nrequire; ++n) {
	if (vec[j]->require[n] != vec[k]->require[n]) goto next_k;
      }

      fprintf(stderr, "### Error: duplicate machine-to-pseudo translations "
		      "for %s (to %s and %s)\n",
		      TOP_Name(vec[j]->from_opc),
		      TOP_Name(vec[j]->to_opc),
		      TOP_Name(vec[k]->to_opc));
      exit(EXIT_FAILURE);
      /*NOTREACHED*/

    next_k:
      ;
    }
  }
}


/////////////////////////////////////
void order_pseudos(int *machine_to_pseudo_index, int *pseudo_to_machine_index)
/////////////////////////////////////
//  
/////////////////////////////////////
{
  int i;
  std::list<PSEUDO_OP_INFO *>::iterator ps_iter;
  PSEUDO_OP_INFO **vec = (PSEUDO_OP_INFO **)alloca(sizeof(PSEUDO_OP_INFO *) * num_pseudos);

  for (i = 0; i < TOP_count; ++i) {
    machine_to_pseudo_index[i] = num_pseudos;
    pseudo_to_machine_index[i] = num_pseudos;
  }

  for (i = 0, ps_iter = pseudos.begin();
       ps_iter != pseudos.end();
       ++i, ++ps_iter)
  {
    vec[i] = *ps_iter;
  }
  qsort(vec, num_pseudos, sizeof(PSEUDO_OP_INFO *), compare_pseudos);
  pseudos.clear();
  for (i = 0; i < num_pseudos; ++i) {
    PSEUDO_OP_INFO *ps = vec[i];
    ps->index = i;
    pseudos.push_back(ps);

    switch (ps->dir) {
    case MACHINE_TO_PSEUDO:
      if (machine_to_pseudo_index[(int)ps->from_opc] == num_pseudos) {
	machine_to_pseudo_index[(int)ps->from_opc] = i;
	verify_machine_to_pseudo(vec, i);
      }
      break;
    case PSEUDO_TO_MACHINE:
      if (pseudo_to_machine_index[(int)ps->from_opc] != num_pseudos) {
	fprintf(stderr, "### Error: multiple pseudo-to-machine translations for %s\n",
			TOP_Name(ps->from_opc));
	exit(EXIT_FAILURE);
	/*NOTREACHED*/
      }
      pseudo_to_machine_index[(int)ps->from_opc] = i;
      break;
    default:
      fprintf(stderr, "### Error: unexpected DIRECTION (%d)\n", ps->dir);
      exit(EXIT_FAILURE);
      /*NOTREACHED*/
    }
  }
}


/////////////////////////////////////
void ISA_Pseudo_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  DIRECTION last_dir = UNKNOWN_DIRECTION;
  int machine_to_pseudo_index[TOP_count];
  int pseudo_to_machine_index[TOP_count];
  std::list<PSEUDO_OP_INFO *>::iterator ps_iter;
  int i;
  EXPR *e;
  char buf[1000];
#define FNAME "targ_isa_pseudo"
  sprintf (buf, "%s.h", FNAME);
  FILE* hfile = fopen(buf, "w");
  sprintf (buf, "%s.c", FNAME);
  FILE* cfile = fopen(buf, "w");
  sprintf (buf, "%s.Exported", FNAME);
  FILE* efile = fopen(buf, "w");

  fprintf(cfile, "#include \"topcode.h\"\n"
		 "#include \"%s.h\"\n\n", FNAME);

  sprintf (buf, "%s", FNAME);
  Emit_Header (hfile, buf, interface);
  fprintf(hfile, "#include \"topcode.h\"\n");

  order_pseudos(machine_to_pseudo_index, pseudo_to_machine_index);

  ++max_require; // allow for terminator
  ++max_map; // allow for terminator

  fprintf(cfile, "typedef struct {\n"
		 "  mTOP to_opc;\n"
		 "  mTOP from_opc;\n"
		 "  mUINT8 require[%d];\n"
		 "  mUINT8 map[%d][2];\n"
		 "} PSEUDO;\n", 
		 max_require, 
		 max_map);

  fprintf(cfile, "\nstatic const PSEUDO pseudos[%d] = {\n", num_pseudos + 1);
  for (ps_iter = pseudos.begin();
       ps_iter != pseudos.end();
       ++ps_iter)
  {
    PSEUDO_OP_INFO *info = *ps_iter;
    if (last_dir != info->dir) {
      fprintf(cfile, "\n  /* %s */\n\n",
		       info->dir == MACHINE_TO_PSEUDO 
		     ? "machine => pseudo" : "pseudo => machine");
      last_dir = info->dir;
    }
    fprintf(cfile, "  /* %2d: %s => %s */\n",
		   info->index,
		   TOP_Name((TOP)info->from_opc),
		   TOP_Name((TOP)info->to_opc));
    fprintf(cfile, "  { %3d, %3d, {",
		   info->to_opc,
		   info->from_opc);
    for (i = 0; i < max_require; ++i) {
      fprintf(cfile, " %d,", (i < info->nrequire) ? info->require[i] : 0);
    }
    fprintf(cfile, " }, {");
    for (i = 0; i < max_map; ++i) {
      fprintf(cfile, " {%d, %d},",
		     (i < info->nmap) ? info->map[0][i] : 0,
		     (i < info->nmap) ? info->map[1][i] : 0);
    }
    fprintf(cfile, " } },\n");
  }
  fprintf(cfile, "  /* %2d: TOP_UNDEFINED => TOP_UNDEFINED */\n",
		 num_pseudos);
  fprintf(cfile, "  { %3d, %3d, { 0 }, { {0, 0} }}\n",
		 TOP_UNDEFINED,
		 TOP_UNDEFINED);
  fprintf(cfile, "};\n");

  fprintf(cfile, "\nstatic const mUINT8 pseudo_index[%d][2] = {\n", TOP_count);
  for (i = 0; i < TOP_count; ++i) {
    fprintf(cfile, "  { %2d, %2d }, /* %-9s */\n", 
		   machine_to_pseudo_index[i], 
		   pseudo_to_machine_index[i],
		   TOP_Name((TOP)i));
  }
  fprintf(cfile, "};\n");

  fprintf(hfile, "\ntypedef enum {\n"
		 "  ISA_PSEUDO_to_pseudo = 0,\n"
		 "  ISA_PSEUDO_to_machine = 1\n"
		 "} ISA_PSEUDO_DIRECTION;\n");

  fprintf(hfile, "\nextern TOP ISA_PSEUDO_Translate(TOP opc,\n"
		   "                                INT64 *r,\n"
		   "                                INT64 *o,\n"
		   "                                ISA_PSEUDO_DIRECTION dir);\n");

  fprintf(efile, "ISA_PSEUDO_Translate\n");

  fprintf(cfile, "\n"
		 "#define OPND(n)   (o[(n)])\n"
		 "#define RESULT(n) (r[(n)])\n");

  fprintf(cfile, "\nTOP ISA_PSEUDO_Translate(TOP opc, INT64 *r, INT64 *o, ISA_PSEUDO_DIRECTION dir)\n"
		 "{\n"
		 "  int i;\n"
		 "  int j;\n"
		 "  int n;\n"
		 "  int arg[%d];\n"
		 "  const INT idx = pseudo_index[(INT)opc][dir];\n"
		 "  const PSEUDO *pop = pseudos + idx;\n"
		 "\n"
		 "  if (pop->from_opc != opc) return opc;\n"
		 "\n"
		 "  if (dir == ISA_PSEUDO_to_pseudo) {\n"
		 "    do {\n"
		 "      for (i = 0;; ++i) {\n"
		 "        BOOL val;\n"
		 "        n = pop->require[i];\n"
		 "        switch (n) {\n"
		 "        case 0:\n"
		 "          goto xlate_pseudo;\n",
		 max_map - 1);
  for (e = exprs.next, i = 1; e; e = e->next) {
    if (e->kind == EXPR_BOOL) {
      fprintf(cfile, "        case %d:\n", i);
      fprintf(cfile, "          val = (%s);\n", e->s);
      fprintf(cfile, "          break;\n");
      ++i;
    }
  }
  fprintf(cfile, "        default:\n"
		 "          return TOP_UNDEFINED;\n"
		 "        }\n"
		 "        if (!val) break;\n"
		 "      }\n"
		 "    } while ((++pop)->from_opc == opc);\n"
		 "    return opc;\n"
		 "  }\n"
		 "\n"
		 "xlate_pseudo:\n");
  if (max_map > 1) {
    fprintf(cfile, "  for (j = 0, i = 0; (n = pop->map[i][1]); ++j, ++i) {\n"
		   "    switch (n) {\n");
    for (e = exprs.next, i = 1; e; e = e->next) {
      if (e->kind == EXPR_ARG_RVALUE) {
	fprintf(cfile, "    case %d:\n", i);
	fprintf(cfile, "      arg[j] = (%s);\n", e->s);
	fprintf(cfile, "      break;\n");
	++i;
      }
    }
    fprintf(cfile, "    default:\n"
		   "      return TOP_UNDEFINED;\n"
		   "    }\n"
		   "  }\n"
		   "  for (j = 0, i = 0; (n = pop->map[i][0]); ++j, ++i) {\n"
		   "    switch (n) {\n");
    for (e = exprs.next, i = 1; e; e = e->next) {
      if (e->kind == EXPR_ARG_LVALUE) {
	fprintf(cfile, "    case %d:\n", i);
	fprintf(cfile, "      (%s) = arg[j];\n", e->s);
	fprintf(cfile, "      break;\n");
	++i;
      }
    }
    fprintf(cfile, "    default:\n"
		   "      return TOP_UNDEFINED;\n"
		   "    }\n"
		   "  }\n");
  } else {
    fprintf(cfile, "  ;\n");
  }
  fprintf(cfile, "  return (TOP)pop->to_opc;\n"
		 "}\n");

  Emit_Footer (hfile);
}
