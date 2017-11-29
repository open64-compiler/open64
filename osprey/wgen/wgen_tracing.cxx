/* 
 * Copyright 2007 Google, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation, either version 2, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 */

/************************************************************
 * Tracing framework for wgen.
 * Outputs GIMPLE/WHIRL nodes to the trace file as
 * they are read/created.
 ************************************************************/

extern "C" {
#include "gspin-wgen-interface.h"
}

#include <stdio.h>
#include "tracing.h"
#include "opcode.h"
#include "mtypes.h"
#include "wn.h"
#include "ir_reader.h"
#include "wgen_tracing.h"

int WGEN_TRACE::gimple_depth = 0;

static void print_indent(FILE *fp);

static void print_indent(FILE *fp)
{
  for (int i=0; i<WGEN_TRACE::gimple_depth; i++) {
    fprintf(fp, " ");
  }
}

void WGEN_TRACE::trace_gs(gs_t n)
{
  if (Get_Trace(TKIND_IR, TP_WGEN)) {
    gs_code_t c = gs_tree_code(n);
    print_indent(TFile);
    fprintf(TFile, "--- %s\n", gs_code_name(c));
  }
}

WN* WGEN_Trace_wn(WN *wn)
{
  if (Get_Trace(TKIND_IR, TP_WGEN)) {
    print_indent(TFile);
    const char * fmt_str = (sizeof(INTPTR) == 8) ? 
                            "+++ (0x%016x) " : "+++ (0x%08x) ";
    fprintf(TFile, fmt_str, (INTPTR) wn);
    fdump_wn(TFile, wn);
  }
  return wn;
}
