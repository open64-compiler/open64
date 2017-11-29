/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: xstats.c
 *
 * Description:
 *
 * This contains declarations for all statistics variables exported in
 * stats.h.  It also contains 1 public entry point to print out a
 * variety of statistics.  This public routine is called from the
 * main routine of each process.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#define USE_STANDARD_TYPES 1

#include "defs.h"
#include "tracing.h"
#include "glob.h"
#include "opcode.h"
#include "xstats.h"
#include "stab.h"
#include "wio.h"

UINT32 PU_Olimit;
UINT32 Max_Src_Olimit;   /* Maximum Olimit needed for all PU's in file */

/* Counters of important data structures */
INT32 PU_WN_Cnt;	/* number of whirl nodes in this PU */
INT32 Total_WN_Cnt;	/* total number of whirl nodes */
INT32 PU_WN_BB_Cnt;	/* number of whirl bbs in this PU */
INT32 PU_WN_Stmt_Cnt;	/* number of whirl stmts in this PU */
INT32 PU_WN_Call_Cnt;	/* number of whirl calls in this PU */
INT32 PU_WN_Loop_Cnt;	/* number of whirl loops in this PU */
INT32 PU_BB_Cnt;	/* number of cg basic-blocks in this PU */
INT32 Total_BB_Cnt;	/* total number of basic-blocks */
INT32 PU_OP_Cnt;	/* number of instructions in this PU */
INT32 Total_OP_Cnt;	/* total number of instructions */
INT32 PU_Size;		/* byte size of this PU */
INT32 Total_Code_Size;	/* total bytes size of all PUs */
INT32 PU_TN_Cnt;	/* number of TNs in this PU */
INT32 Total_TN_Cnt;	/* total number of TNs ever allocated */

/* Counters for back end: */
INT32 Misaligned_Cnt;	/* Number of misaligned memrefs expanded */
INT32 Temp_Var_Cnt;	/* Number of tempories created */
INT32 Total_Temp_Var_Cnt;
INT32 Spill_Var_Cnt;	/* Number of spill temporaries created */
INT32 Total_Spill_Var_Cnt;

#ifdef FRONT_END
#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
#define PHASE_NAME "c fe"
#else /* !FRONT_END_C */
#define PHASE_NAME "f77 fe"
#endif /* FRONT_END_C */
#else /* !FRONT_END */
#define PHASE_NAME "be"
#endif /* FRONT_END */


// KEY: Similar functions available in ipl_summarize_template.h and
// ipc_bread.cxx. This function is called by several phases while reading
// in a PU, for example, by lw_inline and ipl, to estimate parameters like
// olimit and weight.
//
/* determine # bbs and stmts to be counted for the operator */
void
Count_WN_Operator (OPERATOR opr, TYPE_ID rtype, INT32& bbs, INT32& stmts,
		   INT32& calls)
{
    /* count nscf stmts as bbs, not stmts */
    if (OPERATOR_is_non_scf(opr)) {
#ifdef KEY
        if (opr != OPR_RETURN && opr != OPR_RETURN_VAL)
#endif
	++bbs;
    } else if (OPERATOR_is_stmt(opr)) {
	if (OPERATOR_is_call(opr)) {
	    ++bbs;
	    ++calls;
	} else if (opr == OPR_IO) {
	    /* TODO:  ideally would look at values of IO_ITEMs,
	     * but then have to pass more than opcode. */
	    ++bbs;
	    ++calls;
	} else if (! OPERATOR_is_not_executable(opr)) {
	    ++stmts;
	    if (MTYPE_is_complex(rtype) && OPERATOR_is_store(opr)) {
		++stmts;
	    }
	}
    } else if (OPERATOR_is_scf(opr)) {
	if (opr != OPR_BLOCK
#ifdef KEY
	    && opr != OPR_FUNC_ENTRY
#endif
	   ) {
	    /* blocks are counted by parent node */
	    ++bbs;
	}
	/* if may create two blocks if else present,
	 * but can't tell just from opcode */
    } else if (MTYPE_is_quad(rtype) &&
	       OPERATOR_is_expression(opr) &&
	       !OPERATOR_is_load(opr) &&
	       !OPERATOR_is_leaf(opr) ) {
	/* quad operators get turned into calls */
	++bbs;
	++calls;
    } else if (opr == OPR_CAND || opr == OPR_CIOR) {
	/* these may get expanded to if-then-else sequences,
	 * or they may be optimized to logical expressions.
	 * use the halfway average of 1 bb */
	++bbs;
    }
}


void
Count_WN_Node (WN *node, INT32 *bbs, INT32 *stmts)
{
	Count_WN_Operator (WN_operator (node), WN_rtype (node), *bbs, *stmts,
			   PU_WN_Call_Cnt);
	if (WN_opcode(node) == OPC_IO) {
		INT i;
                for (i=0; i<WN_kid_count(node); i++) {
                        WN *kid = WN_kid(node, i);
                        if (WN_opcode(kid) == OPC_IO_ITEM
                            && (WN_io_item(kid) == IOC_END
                             || WN_io_item(kid)==IOC_ERR)
// assuming LDA of label will change to GOTO label
                            && WN_opcode(WN_kid0(kid)) == OPC_GOTO)
                        {
                                // found implicit branch to label
				(*bbs)++;
			}
		}
	}
}


/* ====================================================================
 *
 * Initialize_Stats
 *
 * Initialize statistics.
 *
 * ====================================================================
 */

void
Initialize_Stats ( void )
{
  Total_WN_Cnt = 0;
  PU_WN_Cnt = 0;
  Total_BB_Cnt = 0;
  PU_WN_BB_Cnt = 0;
  PU_WN_Stmt_Cnt = 0;
  PU_WN_Call_Cnt = 0;
  PU_WN_Loop_Cnt = 0;
  PU_BB_Cnt = 0;
  PU_OP_Cnt = 0;
  Total_OP_Cnt = 0;
  PU_Size = 0;
  Total_Code_Size = 0;
  PU_TN_Cnt = 0;
  Total_TN_Cnt = 0;

  Misaligned_Cnt = 0;	/* Number of misaligned memrefs expanded */
  Temp_Var_Cnt = 0;	/* Number of temporaries created */
  Total_Temp_Var_Cnt = 0;
  Spill_Var_Cnt = 0;	/* Number of spill temporaries created */
  Total_Spill_Var_Cnt = 0;
  Max_Src_Olimit = 0;
}

/* ====================================================================
 *
 * Initialize_PU_Stats
 *
 * Initialize only the PU statistics.
 *
 * ====================================================================
 */

void
Initialize_PU_Stats ( void )
{
  PU_WN_Cnt = 0;
  PU_WN_BB_Cnt = 0;
  PU_WN_Stmt_Cnt = 0;
  PU_WN_Call_Cnt = 0;
  PU_WN_Loop_Cnt = 0;
  PU_BB_Cnt = 0;
  PU_OP_Cnt = 0;
  PU_TN_Cnt = 0;
  PU_Size = 0;

  Temp_Var_Cnt = 0;	/* Number of temporaries created */
  Spill_Var_Cnt = 0;	/* Number of spill temporaries created */
}

/* ====================================================================
 *
 * Print_Stats
 *
 * Print statistics to the trace file.
 *
 * ====================================================================
 */

void
Print_PU_Stats ( void )
{
    /* only print stats at user's request */
    if ( Get_Trace(TKIND_INFO, TINFO_STATS) == 0 ) return;

    fprintf(TFile, "PU %s stats for %s:\n", PHASE_NAME, Orig_PU_Name);
    fprintf ( TFile, "WNs in PU:  %d\n", PU_WN_Cnt);
#ifdef BACK_END
    fprintf ( TFile, "WN BBs in PU:  %d\n", PU_WN_BB_Cnt);
    fprintf ( TFile, "WN Stmts in PU:  %d\n", PU_WN_Stmt_Cnt);
    fprintf ( TFile, "WN Calls in PU:  %d\n", PU_WN_Call_Cnt);
    fprintf ( TFile, "BBs in PU:  %d\n", PU_BB_Cnt);
    fprintf ( TFile, "OPs in PU:  %d\n", PU_OP_Cnt);
    fprintf ( TFile, "TNs in PU:  %d\n", PU_TN_Cnt);
    fprintf ( TFile, "STs in PU:  %d, PREGs in PU:  %d\n",
	     Scope_tab[CURRENT_SYMTAB].st_tab->Size (),
	     Scope_tab[CURRENT_SYMTAB].preg_tab->Size ());
    fprintf ( TFile, "%d temporary variables, %d spill temporaries\n", 
	     Temp_Var_Cnt, Spill_Var_Cnt);
    fprintf ( TFile, "Size of PU:  %d bytes\n", PU_Size);
#endif
    fprintf (TFile, "\n");

    /* accumulate PU counters */
    Total_WN_Cnt += PU_WN_Cnt;
    Total_BB_Cnt += PU_BB_Cnt;
    Total_OP_Cnt += PU_OP_Cnt;
    Total_TN_Cnt += PU_TN_Cnt;
    Total_Temp_Var_Cnt += Temp_Var_Cnt;
    Total_Spill_Var_Cnt += Spill_Var_Cnt;
    Total_Code_Size += PU_Size;
}

void
Print_Total_Stats ( void )
{
  /* only print stats at user's request */
  if ( Get_Trace(TKIND_INFO, TINFO_STATS) == 0 ) return;
  
  fprintf(TFile, "Total %s stats for compilation:\n", PHASE_NAME);
  fprintf ( TFile, "WNs in file:  %d\n", Total_WN_Cnt);
#ifdef BACK_END
  fprintf ( TFile, "BBs in file:  %d\n", Total_BB_Cnt);
  fprintf ( TFile, "OPs in file:  %d\n", Total_OP_Cnt);
  fprintf ( TFile, "TNs in file:  %d\n", Total_TN_Cnt);
  fprintf ( TFile, "Code size in file:  %d bytes\n", Total_Code_Size);

  fprintf ( TFile, "%d temporary variables, %d spill temporaries\n", 
	Total_Temp_Var_Cnt, Total_Spill_Var_Cnt);

  fprintf ( TFile, "Misaligned memory references: %d\n", Misaligned_Cnt);
#endif
}
