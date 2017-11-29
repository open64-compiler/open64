//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_whirlview.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_whirlview.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "errors.h"
#include "srcpos.h"
#include "opcode.h"
#include "wn.h"		/* Whirl Node descriptor */
#include "wn_simp.h"    /* Whirl simplifier (can be stubs) */

#ifdef TODO_IGNORE_SOURCE
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h"
#endif

#include "stab.h"
#include "const.h"
#include "targ_const.h"
#include "strtab.h"
#include "wio.h"
#include "wintrinsic.h"
#include "wutil.h"
#include "intrn_info.h"
#include "wn_pragmas.h"

#include "opt_base.h"
#include "opt_du.h"   /* for DU_MANAGER */
#include "targ_sim.h"

extern FILE *Init_daVinci(void);

/*========================================================================

  IR Ascii Writer using SPRINTF_ADV
    sir_put_wn
    sir_put_st

  Code for these three functions more or less stolen from
  common/com/ir_reader.c, with modifications to use sprintf in place
  of fprintf, and other very minor changes. Because some of this code
  owes its origins to functions written in C, the C++ compiler
  produces a few warnings about anachronisms.
 ========================================================================*/

/* A general-purpose output buffer for the IR ASCII writer, and an
 * index into the buffer to keep track of where we're appending.
 */

static char  ir_obuf[2048];
static char *ir_oidx = ir_obuf;

/* Macro to do sprintf and advance a buffer pointer (usually a pointer
 * to sprintf's output buffer. This isn't particularly efficient, so
 * don't use it if performance matters.
 */
#define SPRINTF_ADV(buf, args) { \
				   (void) sprintf args; \
				   buf += strlen(buf); \
				}


static void
sir_put_st (ST *st)
{
  char *name;
  char *p;
  
  if (st == NULL) {
    /* labels may have empty st */
    SPRINTF_ADV(ir_oidx, (ir_oidx, " <null-st>"));
  }
  else {
    name = ST_name(st);
    if (ST_class(st) == CLASS_CONST) {
      name = Targ_Print(NULL, STC_val(st));
      /* new lines and spaces in constant strings 
       * will mess up the ascii reader,
       * so replace with underlines */
      for (p = name; *p != '\0'; p++)
	switch (*p) {
	case ' ':
	case '\t':
	case '\n':
	  *p = '_';
	  break;
	case '"':      // embeded double quotes break DaVinci.
	  *p = '\'';
	  break;
	}
    }
    SPRINTF_ADV(ir_oidx, (ir_oidx, " <%d,%d,%s>", ST_level(st),
			  ST_index(st), ST_name(st)));
  }
}

/* Write a WN in ascii form on an individual line.
 */ 
static void sir_put_wn(WN *wn)
{
    OPCODE opcode = WN_opcode(wn);

    if (wn == NULL) {
    	/* null statement */
    	SPRINTF_ADV(ir_oidx, (ir_oidx, "### error: null WN pointer"));
   	 return;
    }

    SPRINTF_ADV(ir_oidx,
		(ir_oidx, "%s", OPCODE_name(WN_opcode(wn)) + strlen("OPC_")));
    if (OPCODE_has_offset(opcode)) {
      SPRINTF_ADV(ir_oidx, (ir_oidx, " %d", WN_offset(wn)));
    }

    if (OPCODE_operator(opcode) == OPR_INTRINSIC_OP
#ifdef KEY
	|| OPCODE_operator(opcode) == OPR_PURE_CALL_OP
#endif
       ) {
      SPRINTF_ADV(ir_oidx, (ir_oidx, " %d", WN_kid_count(wn)));
    }

    if (OPCODE_has_inumber(opcode)) {
	switch (opcode) {
	case OPC_IO:
		SPRINTF_ADV(ir_oidx,
			    (ir_oidx, " <%d,%s>",
			     WN_intrinsic(wn),
			     IOSTATEMENT_name((IOSTATEMENT) WN_intrinsic(wn))));
		break;
	case OPC_IO_ITEM:
		SPRINTF_ADV(ir_oidx,
			    (ir_oidx, " <%d,%s>",
			     WN_intrinsic(wn),
			     IOITEM_name((IOITEM) WN_intrinsic(wn))));
		break;
	default:
		/* intrinsic */
		SPRINTF_ADV(ir_oidx,
			    (ir_oidx, " <%d,%s>",
			     WN_intrinsic(wn),
			     INTRINSIC_name((INTRINSIC) WN_intrinsic(wn))));
		break;
	}
    }

    if (OPCODE_has_bits(opcode))
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " %d", WN_cvtl_bits(wn)));
    if (OPCODE_has_label(opcode))
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " L%d", WN_label_number(wn)));
    if (OPCODE_has_flags(opcode)) 
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " 0x%x", WN_flag(wn)));
    if (OPCODE_has_sym(opcode)) {
      sir_put_st (WN_st(wn));
    }

    if (OPCODE_has_1ty(opcode)) {
      if (WN_ty(wn) != 0) {
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " T<%d,%s>",
		     TY_id(WN_ty(wn)), 
		     TY_name(WN_ty(wn))));
      }
      else {
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " T<### ERROR: null ptr>"));
      }
    } else if (OPCODE_has_2ty(opcode)) {
      if (WN_ty(wn) != 0) { 
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " T<%d,%s>", 
		     TY_id(WN_ty(wn)), 
		     TY_name(WN_ty(wn))));
      }
      else {
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " T<### ERROR: null ptr>"));
      }
      if (WN_load_addr_ty(wn) != 0) {
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " T<%d,%s>", 
		     TY_id(WN_load_addr_ty(wn)), 
		     TY_name(WN_load_addr_ty(wn))));
      }
      else {
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " T<### ERROR: null ptr>"));
      }
    }

    if (OPCODE_has_ndim(opcode))
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " %d", WN_num_dim(wn)));
    if (OPCODE_has_esize(opcode))
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " %lld", WN_element_size(wn)));

    if (OPCODE_has_num_entries(opcode))
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " %d", WN_num_entries(wn)));

    if (OPCODE_has_value(opcode))
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " %lld", WN_const_val(wn)));
    
    if (opcode == OPC_COMMENT) {
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " # %s",
		     Index_To_Str(WN_offset(wn))));
    }

    if (OPCODE_has_sym(opcode) && OPCODE_has_offset(opcode)
	&& WN_st(wn) != NULL && (ST_class(WN_st(wn)) == CLASS_PREG)
	&& !Preg_Is_Dedicated(WN_offset(wn)) ) {
	/* reference to a non-dedicated preg */
      SPRINTF_ADV(ir_oidx,
		  (ir_oidx, " # %s", Preg_Name(WN_offset(wn))));
    }

    if (opcode == OPC_PRAGMA) {
	SPRINTF_ADV(ir_oidx,
		    (ir_oidx, " # %s", WN_pragmas[WN_offset(wn)].name));
    }

    SPRINTF_ADV(ir_oidx,
		(ir_oidx, " # <id %d:%d>",
		 OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn)));
}

/* Now the stuff to work with daVinci. Most of the code complexity
 * here arises from the need to get daVinci's input syntax right. For
 * a nearly comprehensible (though quite incomplete and somewhat
 * buggy) description of the syntax, see the appendices to the daVinci
 * document.
 *
 * The most annoying aspect of the syntax is that all the
 * arcs outgoing from a particular node v in the graph have to be
 * named along with node v. After we've "finished" with node v, we
 * can't come along later and add arcs going out of v. Not only that,
 * but while we're listing arcs incident to v, we can't define any
 * nodes. These facts conspire to defeat any sort of naive recursive
 * control flow to handle the output of the WHIRL tree. Finally, if
 * our output graph were guaranteed to be strictly a tree structure,
 * we could nest the definitions of a node v's kids inside the
 * definition for node v. But when we begin to output arbitrary arcs
 * incident to v (including "back," "forward," and "cross" arcs, if
 * you will -- D-U arcs, for example), the heirarchical output format
 * very quickly gets hard to understand and debug.
 *
 * Motivated by the desire to have easy-to-debug output, and
 * frustrated by the fact that the control flow couldn't be simple no
 * matter what, I decided to keep all the node definitions at the top
 * level of the heirarchy. Uncharitable translation: "Motivated by the
 * desire to have easy-to-debug output, I decided to write
 * hard-to-debug, obfuscated code." Does this decision make sense? You
 * be the judge. :-)
 */

/* daVinci gets the graph description from us through a pipe that
 * Init_daVinci() connects to this file pointer.
 */

static FILE *dV_fp;

/* To be able to traverse the WHIRL tree in the conventional node
 * order, we keep an explicit stack of nodes that we've seen but not
 * processed. The initial space allocated for the stack is
 * DV_INIT_STK_SZ, and it gets increased from there in steps of "times
 * two."
 */

#define DV_INIT_STK_SZ 2048

/* For each node w saved on the stack, we need to know whether to
 * treat w as an expression node, a statement node, or something
 * special. Generally the expr vs. stmt issue could be resolved by
 * looking at the node's opcode, but marking the stacked nodes makes
 * things substantially easier.
 */

typedef enum {
  DV_NODE_EXPR,
  DV_NODE_STMT,
  DV_NODE_NONEXP,
  DV_NODE_NULEXP,
  DV_NODE_UNKNOWN
} DV_NODE_TYPE;

/* For each arc we are to place, we say what type it is. The arc type
 * controls the color we request that the arc be displayed in.
 */

typedef enum {
  DV_ARC_KID,
  DV_ARC_DU,
  DV_ARC_NEXTPREV,
  DV_ARC_UNKNOWN
} DV_ARC_TYPE;

typedef struct wn_stk_ele {
  WN           *wn;
  DV_NODE_TYPE node_type;
} WN_STK_ELE;

static WN_STK_ELE  *dV_WN_stack;
static INT          dV_WN_sp;
static INT          dV_WN_stk_sz;

/* Each node in the output graph must have a unique identification
 * string. For nodes that have map id's a string containing the node's
 * opcode category and map id is enough, but those nodes that don't
 * have map id's need something else. So we maintain a set of nodes
 * that have supplementary id numbers. When a node with no map id
 * comes along for the first time, we assign it a supplementary id
 * number that will always get included in the string that identifies
 * the node to daVinci.
 */

#define SUP_INIT_SZ 2048

static WN         **id_supplement;
static INT          id_sup_sz;
static INT          next_sup_id;

/* Forward declarations for recursive calls.
 */

static void dV_tree_expr(WN *, DU_MANAGER *);
static void dV_tree_stmt(WN *, DU_MANAGER *);

/* Allocate space for internal structures, call Init_daVinci() to
 * start the daVinci process and open the pipe to it, and output the
 * introduction for the graph to come.
 */

static void
dV_WN_init()

{
dV_WN_stack = (WN_STK_ELE *) malloc(DV_INIT_STK_SZ *
				    sizeof(WN_STK_ELE));
dV_WN_stk_sz = DV_INIT_STK_SZ;
dV_WN_sp = 0;

id_supplement = (WN **) malloc(SUP_INIT_SZ * sizeof(WN *));
id_sup_sz = SUP_INIT_SZ;
next_sup_id = 0;

FmtAssert((dV_WN_stack != NULL) && (id_supplement != NULL),
	  ("Unable to allocate memory in dV_WN_init()"));

dV_fp = Init_daVinci();

(void) fprintf(dV_fp, "graph(new([");
}

/* Write the characters that signify the end of the graph description
 * to daVinci, and free resources used.
 */

static void
dV_WN_end()

{
(void) fprintf(dV_fp, "]))\n");
free((void *) id_supplement);
free((void *) dV_WN_stack);

#ifdef TODO_DONT_CLOSE_PIPE
fclose(dV_fp);
#endif
}

/* Place a dummy node corresponding to a spot in the tree where the
 * expected WHIRL node is missing. The dummy node is made a child of
 * wn, and the arc from wn to the dummy node is labeled according to
 * the argument label.
 */

static char *
dV_place_stub(char *label)

{
  static INT next_id = 1;

  /* Prepare the node label to pass back to the caller. The caller must
   * copy it if she wants to keep it around for a long time.
   */
  ir_oidx = ir_obuf;
  (void) sprintf(ir_obuf, "Stub%d", next_id++);

#ifdef VERTICAL_LABELS
  char       localbuf[4096], *p, *q;

  for (p = label, q = localbuf; *p; p++, q++) {
    *q++ = *p;
    *q++ = '\\';
    *q   = 'n';
  }
  if (p != label) {
    *(q-2) = '\0';
  }
#else
  char *localbuf = label;
#endif

  /* Output the dummy node */
  (void) fprintf(dV_fp, "l(\"%s\", n(\"\", [a(\"%s\",\"%s\")], [] ) ),",
		 ir_obuf, "OBJECT", localbuf);
  return ir_obuf;
}

/* Find or generate the supplementary id for a WHIRL node with no map
 * id.
 */

INT
id_sup_search(WN *wn)
{
  INT i = 0;

  while (i < next_sup_id) {
    if (wn == id_supplement[i]) {
      return i;
    }
    i++;
  }

  if (next_sup_id >= id_sup_sz) {
    id_sup_sz *= 2;
    id_supplement = (WN **) realloc((void *) id_supplement,
				    id_sup_sz * sizeof(WN *));
  }
  id_supplement[next_sup_id] = wn;
  return next_sup_id++;
}

/* Generate a string that uniquely identifies the given WHIRL node.
 */

static const char *
id_str(WN *wn)

{
  static char dv_id[64];

  if (wn) {
    if (WN_map_id(wn) == -1) {
      /* ID string need not be unique for this node; supplement it
       * with a unique integer.
       */
      INT sup = id_sup_search(wn);
      (void) sprintf(dv_id, "<id %d:%d>-%d",
		     OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn),
		     sup);
    }
    else {
      (void) sprintf(dv_id, "<id %d:%d>",
		     OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn));
    }
    return dv_id;
  }
  else {
    return "!!! NULL WN";
  }
}

/* Save a neighbor on the explicit stack; the neighbor will be dealt
 * with when all the edges incident to the current node have been
 * output.
 */

static void
dV_save_nbor(WN *wn, DV_NODE_TYPE node_type)

{
  /* If we're about to go over the top, realloc the stack. */
  if (dV_WN_sp == dV_WN_stk_sz) {
    dV_WN_stk_sz *= 2;
    dV_WN_stack = (WN_STK_ELE *) realloc((void *) dV_WN_stack,
					 dV_WN_stk_sz *
					 sizeof(WN_STK_ELE));
  }
  FmtAssert(dV_WN_sp < dV_WN_stk_sz, ("Stack pointer messed up"));

  dV_WN_stack[dV_WN_sp].wn = wn;
  dV_WN_stack[dV_WN_sp].node_type = node_type;
  dV_WN_sp++;
}

/* Pop an element off the stack and return a pointer to it.
 */

static WN_STK_ELE *
dV_get_nbor(void)

{
  FmtAssert(dV_WN_sp > 0, ("Pop when dV_WN_sp == 0"));
  dV_WN_sp--;
  return dV_WN_stack + dV_WN_sp;
}

/* Place a graph node corresponding to a WHIRL node. The visible text
 * on the node is more or less what would be printed by fdump_wn(wn).
 */

static void
dV_open_WN(WN *wn)

{
  ir_oidx = ir_obuf;
  sir_put_wn(wn);

#ifdef VERTICAL_LABELS
  char localbuf[4096], *p, *q;

  for (p = ir_obuf, q = localbuf; *p; p++, q++) {
    *q++ = *p;
    *q++ = '\\';
    *q   = 'n';
  }
  if (p != ir_obuf) {
    *(q-2) = '\0';
  }
#else
  char *localbuf = ir_obuf;
#endif

  (void) fprintf(dV_fp, "l(\"%s\", n(\"\", [a(\"%s\",\"%s\")], [",
		  id_str(wn), "OBJECT", localbuf);

  /* Save a marker on the WN stack so we know when the recursion has
   * finished with the kids of this node.
   */
  dV_save_nbor(NULL, DV_NODE_UNKNOWN);

  /* Now we're ready to output labeled edges into the daVinci pipe.
   */
}

/* Do the processing that comes after all the arcs outgoing from the
 * given node have been output. Mainly, we grab nodes one-by-one off
 * the explicit stack and them recursively. The type DV_NODE_UNKNOWN
 * serves as a barrier between "frames" on the stack, where a frame is
 * a collection of kids of one particular node.
 */

static void
dV_close_WN(WN *wn, DU_MANAGER *du_mgr)

{
  WN_STK_ELE *nbor;
  char buf[64];

  /* First close up the syntactic construct for the WHIRL node...
   */
  (void) fprintf(dV_fp, "] ) ),");

  /* Now spit out the saved neighbors of the node.
   */
  while ((nbor = dV_get_nbor())->node_type != DV_NODE_UNKNOWN) {
    switch (nbor->node_type) {
    case DV_NODE_EXPR:
      dV_tree_expr(nbor->wn, du_mgr);
      break;
    case DV_NODE_STMT:
      dV_tree_stmt(nbor->wn, du_mgr);
      break;
    case DV_NODE_NONEXP:
      (void) sprintf(buf, "!!! NONEXP(%d)", WN_opcode(nbor->wn));
      dV_place_stub(buf);
      break;
    case DV_NODE_NULEXP:
      dV_open_WN(nbor->wn);
      dV_close_WN(nbor->wn, du_mgr);
      break;
    default:
      FmtAssert(FALSE, ("Incorrect node type in daVinci WN stack"));
    }
  }
}

/* Place an arc from the current WHIRL node to the WHIRL node with
 * daVinci id *node_id.
 */

static void
dV_place_arc(const char *node_id, DV_ARC_TYPE arc_type, const char *label)
{
  const char *color;

  switch (arc_type) {
  case DV_ARC_KID:
    color = "red";
    break;
  case DV_ARC_NEXTPREV:
    color = "green";
    break;
  case DV_ARC_DU:
    color = "blue";
    break;
  default:
    color = "black";
  }
   
  (void) fprintf(dV_fp, "e(\"\", [a(\"%s\", \"%s\")], r(\"%s\")),",
		 "EDGECOLOR", color, node_id);
}

/*  Write an expression and its children to the daVinci pipe.
 */
static void
dV_tree_expr(WN *wn, DU_MANAGER *du_mgr)

{
  INT i;
  WN *wn2;
  char buf[64];

  dV_open_WN(wn);

  for (i = 0; i < WN_kid_count(wn); i++) {
    wn2 = WN_kid(wn,i);
    if (wn2) {
      OPCODE op = WN_opcode(wn2);
      if ((OPCODE_FIRST <= op && op <= OPCODE_LAST) &&
	  (OPCODE_is_expression(op) || OPCODE_is_call(op))) {
	dV_save_nbor(WN_kid(wn,i), DV_NODE_EXPR);
	(void) sprintf(buf, "kid%d", i);
	dV_place_arc(id_str(WN_kid(wn,i)), DV_ARC_KID, buf);
      }
      else {
	dV_save_nbor(wn2, DV_NODE_NONEXP);
	dV_place_arc(id_str(wn2), DV_ARC_KID, "NONEXP");
      }
    }
    else {
      dV_save_nbor(wn2, DV_NODE_NULEXP);
      dV_place_arc(id_str(wn2), DV_ARC_KID, "NULEXP");
    }
  }

  /* The WHIRL tree structure is now output for this node. Time to
   * output the DU-chain information (and any other information we
   * want to include).
   */
  if (du_mgr) {
    USE_LIST            *use_list = du_mgr->Du_Get_Use(wn);
    USE_LIST_CONST_ITER  use_lst_iter;
    const DU_NODE       *tmp;

    FOR_ALL_NODE(tmp, use_lst_iter, Init(use_list))
      dV_place_arc(id_str(tmp->Wn()), DV_ARC_DU, "USES");
  }

  dV_close_WN(wn, du_mgr);
}

/* Write a statement WN * and its children to the daVinci pipe.
 */
static void
dV_tree_stmt(WN *wn, DU_MANAGER *du_mgr)
{
  INT i;
  WN *wn2;
  char buf[64];

#ifdef TODO_IGNORE_SOURCE
  USRCPOS srcpos;
  OPCODE opc = WN_opcode(wn);
  
  USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);
  if (USRCPOS_srcpos(srcpos) != 0 && 
      USRCPOS_srcpos(srcpos) != USRCPOS_srcpos(last_srcpos)) {
    last_srcpos = srcpos;
#ifdef FRONT_END
    fprintf(ir_ofile, "%*sLOC %d %d\n", indent, "",
	    USRCPOS_filenum(srcpos), USRCPOS_linenum(srcpos));
#else
    print_source(USRCPOS_srcpos(srcpos));
#endif
  }
#endif

  dV_open_WN(wn);

  switch (WN_opcode(wn)) {
  case OPC_BLOCK: {
    INT kid_num = 1;

    wn2 = WN_first(wn);
    while (wn2) {
      dV_save_nbor(wn2, DV_NODE_STMT);
      (void) sprintf(buf, "blkkid%d", kid_num);
      kid_num++;
      dV_place_arc(id_str(wn2), DV_ARC_KID, buf);

#ifdef TODO_DV_SYNTAX_SNAG
      wn3 = WN_next(wn2);
      if (wn3) {
**-->	dV_place_arc(wn2, wn3, DV_ARC_NEXTPREV, "<-- PV/NX -->");
      }
      wn2 = wn3;
#else
      wn2 = WN_next(wn2);
#endif
    }
    break;
  }
  case OPC_IF:
    dV_save_nbor(WN_if_test(wn), DV_NODE_EXPR);
    dV_place_arc(id_str(WN_if_test(wn)), DV_ARC_KID, "COND");
    if (WN_then(wn)) {
      dV_save_nbor(WN_then(wn), DV_NODE_STMT);
      dV_place_arc(id_str(WN_then(wn)), DV_ARC_KID, "THEN");
    }
    if (WN_else(wn)) {
      dV_save_nbor(WN_else(wn), DV_NODE_STMT);
      dV_place_arc(id_str(WN_else(wn)), DV_ARC_KID, "ELSE");
    }
    break;

  case OPC_DO_LOOP:
    dV_save_nbor(WN_index(wn), DV_NODE_EXPR);
    dV_place_arc(id_str(WN_index(wn)), DV_ARC_KID, "INDX");
    dV_save_nbor(WN_start(wn), DV_NODE_STMT);
    dV_place_arc(id_str(WN_start(wn)), DV_ARC_KID, "INIT");
    dV_save_nbor(WN_end(wn), DV_NODE_EXPR);
    dV_place_arc(id_str(WN_end(wn)), DV_ARC_KID, "COMP");
    dV_save_nbor(WN_step(wn), DV_NODE_STMT);
    dV_place_arc(id_str(WN_step(wn)), DV_ARC_KID, "INCR");
    dV_save_nbor(WN_do_body(wn), DV_NODE_STMT);
    dV_place_arc(id_str(WN_do_body(wn)), DV_ARC_KID, "BODY");
    break;

  case OPC_COMPGOTO:
    dV_save_nbor(WN_kid(wn,0), DV_NODE_EXPR);
    dV_place_arc(id_str(WN_kid(wn,0)), DV_ARC_KID, "SWCH");
    dV_save_nbor(WN_kid(wn,1), DV_NODE_STMT);
    dV_place_arc(id_str(WN_kid(wn,1)), DV_ARC_KID, "JMPS");
    if (WN_kid_count(wn) > 2) {
      dV_save_nbor(WN_kid(wn,2), DV_NODE_STMT);
      dV_place_arc(id_str(WN_kid(wn,2)), DV_ARC_KID, "DFLT");
    }
    break;

  case OPC_XGOTO:
    dV_save_nbor(WN_kid(wn,0), DV_NODE_EXPR);
    dV_place_arc(id_str(WN_kid(wn,0)), DV_ARC_KID, "SWCH");
    dV_save_nbor(WN_kid(wn,1), DV_NODE_STMT);
    dV_place_arc(id_str(WN_kid(wn,1)), DV_ARC_KID, "JMPS");
    break;

  default: 
    {
      OPCODE opc2;
      for (i = 0; i < WN_kid_count(wn); i++) {
	wn2 = WN_kid(wn,i);
	FmtAssert(wn2, ("Null kid in dV_tree_stmt"));
	opc2 = WN_opcode(wn2);
	if (OPCODE_is_expression(opc2)) {
	  dV_save_nbor(wn2, DV_NODE_EXPR);
	  (void) sprintf(buf, "kid%d", i);
	  dV_place_arc(id_str(wn2), DV_ARC_KID, buf);
	}
	else if (OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2)) {
	  dV_save_nbor(wn2, DV_NODE_STMT);
	  (void) sprintf(buf, "kid%d", i);
	  dV_place_arc(id_str(wn2), DV_ARC_KID, buf);
	}
	else {
	  FmtAssert(FALSE, ("operator of unknown type"));
	}
      }
    }
  }

  /* The WHIRL tree structure is now output for this node. Time to
   * output the DU-chain information (and any other information we
   * want to include).
   */
  if (du_mgr) {
    USE_LIST            *use_list = du_mgr->Du_Get_Use(wn);
    USE_LIST_CONST_ITER  use_lst_iter;
    const DU_NODE       *tmp;

    FOR_ALL_NODE(tmp, use_lst_iter, Init(use_list))
      dV_place_arc(id_str(tmp->Wn()), DV_ARC_DU, "USES");
  }

  dV_close_WN(wn, du_mgr);
}

/* The external interface to the world. If du_mgr is NULL on entry,
 * only the WHIRL tree is displayed.
 */

extern "C" void
dV_show_whirl(WN *wn, DU_MANAGER *du_mgr)

{
  dV_WN_init();

  if (OPCODE_is_expression(WN_opcode(wn))) {
    dV_tree_expr(wn, du_mgr);
  }
  else if (OPCODE_is_stmt(WN_opcode(wn)) ||
	   OPCODE_is_scf(WN_opcode(wn))) {
    dV_tree_stmt(wn, du_mgr);
  }
  else {
    FmtAssert(FALSE, ("opcode of unknown type"));
  }

  dV_WN_end();
}
