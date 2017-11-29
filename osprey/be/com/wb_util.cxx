/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#include <stdint.h>
#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include "wn.h"
#include "wn_simp.h"
#include "strtab.h"
#include "opt_du.h"
#include "dep_graph.h"
#include "targ_const.h"
#include "wb_util.h"
#include "lnopt_main.h"
#include "wb_buffer.h" 
#include "wb_carray.h" 
#include "targ_sim.h"
#include "wutil.h"
#include "intrn_info.h"

//-----------------------------------------------------------------------
// NAME: WB_Bell
// FUNCTION: Sound the bell, indicating that an error has occurred.
//-----------------------------------------------------------------------

extern void WB_Bell()
{
  fprintf(stdout, "%c", '\007');
  fflush(stdout);
}

//-----------------------------------------------------------------------
// NAME: WB_Prompt
// FUNCTION: Print the whirl browser prompt symbol to 'stdout'.
//-----------------------------------------------------------------------

extern void WB_Prompt()
{
  fprintf(stdout, "WB> ");
} 

//-----------------------------------------------------------------------
// NAME: WB_Whirl_Symbol
// FUNCTION: Returns a pointer to a printable string of characters
//   describing the symbol of the node 'wn'.  For loads and stores,
//   the symbol is printed, if any.  For do loops, the symbol of the
//   do loop is printed.
//-----------------------------------------------------------------------

extern const char* WB_Whirl_Symbol(WN* wn)
{
  if (wn == NULL)
    return NULL;
  WN* wn_symbol = NULL;
  const char* name = NULL;
  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
  if (opc == OPC_PRAGMA || opc == OPC_XPRAGMA)
    return WN_pragmas[WN_pragma(wn)].name;
  if (opr == OPR_INTRINSIC_CALL)
    return INTRINSIC_name((INTRINSIC) WN_intrinsic(wn));
  wn_symbol = (opc == OPC_DO_LOOP) ? WN_index(wn) : (OPCODE_has_sym(opc))
    ? wn : NULL;
  if (wn_symbol == NULL)
    return NULL;
  if (WN_st(wn_symbol) == NULL)
    return NULL;
  name = ST_class(WN_st(wn_symbol)) != CLASS_PREG
    ? ST_name(WN_st(wn_symbol)) :
      WN_offset(wn_symbol) > Last_Dedicated_Preg_Offset
    ? Preg_Name(WN_offset(wn_symbol)) : "DEDICATED PREG";
  return name;
}

//-----------------------------------------------------------------------
// SUPPORT FUNCTIONS TO PRINT NODE REPRESENTATIONS IN THE WHIRL BROWSER.
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Is_Leaf
// FUNCTION: Return TRUE if the node is a leaf node, FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Is_Leaf(WN* wn)
{
  switch (WN_operator(wn)) {
  case OPR_INTCONST:
  case OPR_LDID:
    return TRUE;
  default:
    return FALSE;
  }
}

//-----------------------------------------------------------------------
// NAME: Dump_Whirl_Node
// FUNCTION: Write into 'buffer' a short but informative description of
//   the node 'wn'.  Start writing at buffer[cc].  Return the position
//   of the first empty character in 'buffer'.
//-----------------------------------------------------------------------

extern INT Dump_Whirl_Node(WN* wn,
                           char* buffer,
                           INT cc)
{
  BOOL printed = FALSE;

  if (wn == NULL) {
    cc += sprintf(buffer + cc, "<null>");
    return cc;
  }

  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  printed = TRUE;
  switch(opr) {
   case OPR_CONST:
    switch (OPCODE_rtype(opc)) {
     case MTYPE_F4:
      cc += sprintf(buffer + cc, "%g", STC_val(WN_st(wn)).vals.fval);
      break;
     case MTYPE_F8:
      cc += sprintf(buffer + cc, "%g", STC_val(WN_st(wn)).vals.dval);
      break;
     default:
      printed = FALSE;
      break;
    }
    break;
   case OPR_INTCONST:
    cc += sprintf(buffer + cc, "%lld", WN_const_val(wn));
    break;
   case OPR_LDID:
    cc += sprintf(buffer + cc, "%s", WB_Whirl_Symbol(wn));
    break;
   case OPR_ADD:
    cc += sprintf(buffer + cc, "+");
    break;
   case OPR_SUB:
    cc += sprintf(buffer + cc, "-");
    break;
   case OPR_MPY:
    cc += sprintf(buffer + cc, "*");
    break;
   case OPR_DIV:
    cc += sprintf(buffer + cc, "/");
    break;
   default:
    printed = FALSE;
    break;
  }

  if (!printed) {
    FmtAssert(strncmp(OPCODE_name(opc), "OPC_", 4) == 0,
              ("opname=%s", OPCODE_name(opc)));

    cc += sprintf(buffer + cc, "%s", OPCODE_name(opc) + 4);
    if (OPCODE_has_sym(opc))
      cc += sprintf(buffer + cc, " %s", WB_Whirl_Symbol(wn));
    if (OPCODE_has_label(opc))
      cc += sprintf(buffer + cc, " LAB%d", WN_offset(wn));
    if (opr == OPR_INTRINSIC_OP || opr == OPR_INTRINSIC_CALL) {
      INTRINSIC        i = (INTRINSIC) WN_intrinsic(wn);
      if (i >= INTRINSIC_FIRST && i <= INTRINSIC_LAST)
        cc += sprintf(buffer + cc, "<%s>", INTRINSIC_name(i));
      else
        cc += sprintf(buffer + cc, "<bad intr #=%d>", i);
    }
    else if (opr == OPR_IO)
      cc += sprintf(buffer + cc, "<io=%d>", WN_io_statement(wn));
    else if (opr == OPR_IO_ITEM)
      cc += sprintf(buffer + cc, "<io item=%d>", WN_io_item(wn));
  }
  return cc;
}

//-----------------------------------------------------------------------
// NAME: WB_Dump_Whirl_Expr
// FUNCTION: Write a short, but informative description of the tree rooted
//   at 'wn' which is part of the larger tree rooted at 'wn_root'.  Place
//   the description in the 'buffer' starting at 'buffer[cc]'.  Return
//   the value of first free index in 'buffer'.
//-----------------------------------------------------------------------

extern INT WB_Dump_Whirl_Expr(WN* wn_root,
                              WN* wn,
                              char* buffer,
                              INT cc)
{
  if (wn != wn_root && !Is_Leaf(wn))
    cc += sprintf(buffer + cc, "(");
  INT i = 0; 
  switch (WN_operator(wn)) {
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MPY:
  case OPR_DIV:
    for (i = 0; i < WN_kid_count(wn); i++) {
      cc = WB_Dump_Whirl_Expr(wn_root, WN_kid(wn, i), buffer, cc);
      if (i < WN_kid_count(wn) - 1)
        cc = Dump_Whirl_Node(wn, buffer, cc);
    }
    break;
  case OPR_ARRAY:
    cc = Dump_Whirl_Node(WN_array_base(wn), buffer, cc);
    cc += sprintf(buffer + cc, "[");
    for (i = 0; i < WN_num_dim(wn); i++) {
      cc = WB_Dump_Whirl_Expr(WN_array_index(wn, i), WN_array_index(wn, i),
        buffer, cc);
      if (i < WN_num_dim(wn) - 1)
        cc += sprintf(buffer + cc, ",");
    }
    cc += sprintf(buffer + cc, "]");
    break;
  case OPR_LDID:
  case OPR_INTCONST:
  case OPR_CONST:
    cc = Dump_Whirl_Node(wn, buffer, cc);
    break;
  default:
    cc = Dump_Whirl_Node(wn, buffer, cc);
    cc += sprintf(buffer + cc, "(");
    for (i = 0; i < WN_kid_count(wn); i++) {
      cc = WB_Dump_Whirl_Expr(wn_root, WN_kid(wn, i), buffer, cc);
      if (i < WN_kid_count(wn) - 1)
        cc += sprintf(buffer + cc, ",");
    }
    cc += sprintf(buffer + cc, ")");
    break;
  }
  if (wn != wn_root && !Is_Leaf(wn))
    cc += sprintf(buffer + cc, ")");
  return cc;
}

