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


#ifndef xstats_INCLUDED
#define xstats_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: xstats.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Description:
 *
 * Interface for statistics collection in the compiler core.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *xstats_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "opcode.h"
#include "wn_core.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Compute the PU Olimit.
 * WARNING:  This computation may change!
 * The WOPT algorithm is #bbs * #stmts * #vars
 * CG has some algorithms that are n^2 on #bbs
 * As a simple approximation, and to keep things to a single number,
 * just use 2*#bbs + #stmts
 */
#define COMPUTE_OLIMIT(b,s)	(b + b + s)
#define COMPUTE_PU_OLIMIT	\
	PU_Olimit = COMPUTE_OLIMIT(PU_WN_BB_Cnt, PU_WN_Stmt_Cnt); \
	Max_Src_Olimit = MAX(Max_Src_Olimit,PU_Olimit)

extern UINT32 PU_Olimit;	/* Olimit computed for PU */
extern UINT32 Max_Src_Olimit;	/* Maximum Olimit needed for all PU's in file */

/* Counters of important data structures */
extern INT32 PU_WN_Cnt;		/* number of whirl nodes in this PU */
extern INT32 PU_WN_BB_Cnt;	/* number of whirl bbs in this PU */
extern INT32 PU_WN_Stmt_Cnt;	/* number of whirl stmts in this PU */
extern INT32 PU_WN_Call_Cnt;	/* number of whirl calls in this PU */
extern INT32 PU_WN_Loop_Cnt;	/* number of whirl loops in this PU */
extern INT32 PU_BB_Cnt;		/* number of cg basic-blocks in this PU */
extern INT32 PU_OP_Cnt;		/* number of instructions in this PU */
extern INT32 PU_Size;		/* number of bytes in this PU */
extern INT32 PU_TN_Cnt;		/* number of TNs in this PU */

/* Counters for back end: */
extern INT32 Misaligned_Cnt;	/* Number of misaligned memrefs expanded */
extern INT32 Temp_Var_Cnt;	/* Number of tempories created */
extern INT32 Spill_Var_Cnt;	/* Number of spill temporaries created */

/* Publicly visible functions */

extern void Initialize_Stats(void);
extern void Initialize_PU_Stats(void);
extern void Print_PU_Stats(void);
extern void Print_Total_Stats(void);

/* determine # bbs and stmts to be counted for the opcode;
 * the bbs and stmts parameters are incremented. */
extern void Count_WN_Node (WN *node, INT32 *bbs, INT32 *stmts);
extern void Count_WN_Operator (OPERATOR opr, TYPE_ID rtype, INT32& bbs,
			       INT32& stmts, INT32& calls);
inline void
Count_WN_Opcode (OPCODE opcode, INT32* bbs, INT32* stmts)
{
    Count_WN_Operator (OPCODE_operator (opcode), OPCODE_rtype (opcode),
		       *bbs, *stmts, PU_WN_Call_Cnt);
}


#ifdef __cplusplus
}
#endif
#endif /* xstats_INCLUDED */
