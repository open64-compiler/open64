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


#ifndef tracing_INCLUDED
#define tracing_INCLUDED

#ifndef defs_INCLUDED
/* Sort of bizarre, including from common/com to common/util, but it
 * seems it must be so.
 */
#include "defs.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: tracing.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/tracing.h,v $
 *
 * Revision history:
 *  08-Sep-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *  22-May-91 - Integrated additional Josie functionality
 *
 * Description:
 *
 * External interface for tracing support in the Muse compilers and
 * associated tools.  The basic methodology assumed is the use of
 * fprintf to a trace file (which may be, and defaults to, stdout).
 * The support provided by the tracing package is primarily managing
 * the trace file and flags controlling the traces.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *tracing_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/tracing.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

/* Is_Trace */
#ifndef Is_Trace
#ifdef Is_True_On
#define Is_Trace(Cond, Parmlist) { if (Cond) fprintf Parmlist ; }
#define Is_Trace_cmd(Cond, Cmd) { if (Cond) Cmd ; }
#else
#define Is_Trace(Cond, Parmlist) ((void) 1)
#define Is_Trace_cmd(Cond, Cmd)  ((void) 1)
#endif
#endif

/* The following strings are useful in creating trace messages: */
extern char *SBar;	/* Single-dash bar w/NL, full page width */
extern char *DBar;	/* Double-dash bar w/NL, full page width */
extern char *Sharps;	/* Sharps w/NL, full page width */


/* ====================================================================
 *
 * Trace Flag Literals
 *
 * This package supports several kinds of trace conditions, to allow
 * the programmer a great deal of control over the tracing which occurs
 * at runtime.  (Otherwise, good tracing would be discouraged by the
 * sheer volume which would result, or by the difficulty of setting it
 * up.)  The trace flag kinds supported are:
 *
 *  1)	INFO flags control general information about a compilation,
 *	e.g. timing, object code statistics, or failure tracing.
 *	They are represented by mask bits in a single word.
 *
 *  2)	DEBUG flags control options which affect the execution of the
 *	compiler for debugging purposes, e.g. to perform extra checks
 *	or to try alternate algorithms.  They are also represented by
 *	a word of mask bits.
 *
 *  3)	IR flags control the tracing of the current IR state after
 *	individual compiler phases.  They are represented by the phase
 *	number to which they apply.
 *
 *  4)	SYMTAB flags control the tracing of the current symbol table
 *	state after individual compiler phases.  They are represented
 *	by the phase number as well.
 *
 *  5)	TN flags control the tracing of the current set of TNs after
 *	individual compiler phases.  They are represented by the phase
 *	number as well.
 *
 *  6)	Phase-specific flags control the tracing in individual compiler
 *	components.  Each such unit has available a word of mask bits
 *	on which to base its tracing options, represented by the phase
 *	number and mask.  Note that the phase numbers used for this
 *	purpose are the same as are used for IR and SYMTAB traces.
 *
 * ====================================================================
 */

/* Negative integers represent the INFO, DEBUG, IR, and SYMTAB traces,
 * as well as miscellaneous trace options:
 */
#define TKIND_INFO	-1	/* Specify an information option */
#define TKIND_DEBUG	-2	/* Specify a debug option */
#define TKIND_IR	-3	/* Trace IR for the given pass */
#define TKIND_SYMTAB	-4	/* Trace symbol table for given pass */
#define TKIND_TN	-5	/* Trace TNs for given pass */
#define TKIND_BB	-6	/* Specify a BB number for tracing */
#define TKIND_XPHASE	-7	/* Specify final execution phase */
#define TKIND_CTRL	-8	/* Specify a control option */
#define TKIND_ALLOC	-9	/* Trace memory allocation */
#define TKIND_MIN	-10	/* Smallest valid function number */

/* Several predefined masks for TKIND_INFO cases: */
#define TINFO_TIME	1	/* Timing/resource information */
#define TINFO_CTIME	2	/* Compilation-only timing information */
#define TINFO_STATS	8	/* Code size statistics */
#define TINFO_SOURCE	32	/* Source line printing alongside IR dumps */
#define TINFO_TFLAGS	64	/* Print available trace options */
#define TINFO_PREFIXDUMP 128	/* Dump WHIRL trees in prefix order */

/* Positive integers represent phase numbers for per-phase traces:
 * WARNING:  If you change this list (adding or deleting entries), you
 * must change the Phases table in tracing.c.
 */
#define TP_MIN		1	/* Smallest valid phase number */

/* Miscellaneous "phases": */
/* note: for TP_PTRACE[1|2]_flags, see below */
#define TP_PTRACE1	1	/* Performance tracing */
#define TP_PTRACE2	2	/* Performance tracing */
#define TP_MISC		3	/* Miscellaneous */

/* Front end phases: */
#define TP_SEMANTICS	8	/* Semantic analyzer */
#define TP_IRB		10	/* IR (WHIRL) builder */

/* Intermediate utility phases: */
#define TP_IR_READ	11	/* IR (WHIRL) reader/writer */
#define TP_WHIRL2FC	12	/* WHIRL to Fortran/C */
#define TP_WHIRLSIMP    13	/* WHIRL simplifier */
#define TP_REGION	14	/* REGION related stuff */
#define TP_ORI		15	/* Olimit Region Insertion phase */
#define TP_FEEDBACK	16	/* Decorating WHIRL/CFG with feedback */

/* IPA/inlining phases: */
#define TP_INLINE	17	/* Inliner */
#define TP_IPL		18	/* IPA local (summary) phase */
#define TP_IPA		19	/* IPA main analysis phase */
#define TP_IPO		20	/* IPA main optimization phase */
#define TP_IPM		21	/* IPA miscellaneous */

/* Global optimizer phases: */
#define TP_ALIAS	24	/* Alias/mod/ref analysis */
#define TP_WOPT1	25	/* Global optimization */
#define TP_WOPT2	26	/* More global optimization */
#define TP_WOPT3	27	/* Even more global optimization */
#define TP_GLOBOPT	TP_WOPT1

/* Loop nest optimizer phases: */
#define TP_VECDD	30	/* Vector data dependency analysis */
#define TP_LNOPT	31	/* Loop Nest Optimization */
#define TP_LNOPT2	32	/* More Loop Nest Optimization */
#define TP_LNOPT3	33	/* Even more Loop Nest Optimization */

#define TP_VHO_LOWER	36	/* VHO lowering */
#define TP_LOWER	37	/* WHIRL lowering */
#define TP_LOWER90      38      /* F90 Lowering */

/* Code generator phases: */
#define TP_DATALAYOUT	39	/* Data layout */
#define TP_CG		40	/* Code generator miscellaneous */
#define TP_CGEXP	41	/* Code generator expansion */
#define TP_LOCALIZE	42	/* Localize TNs */
#define TP_FIND_GLOB	43	/* Find global register live ranges */
#define TP_EBO		44	/* Extended Block Optimizer */
#define TP_CGPREP	45	/* Code generator scheduling prep */
#define TP_FLOWOPT	47	/* Control flow optimization */
#define TP_GCM		48	/* Global code motion */
#define TP_CGLOOP	49	/* Code generator loop optimization */
#define TP_SWPIPE	50	/* Software pipelining */
#define TP_SRA		51	/* SWP register allocation */
#define TP_SCHED	52	/* Scheduling */
#define TP_GRA		53	/* Global register allocation */
#define TP_ALLOC	54	/* Local register allocation */
#define TP_PSGCM	55	/* Post Schedule Global code motion */
#define TP_EMIT		56	/* Code emission */
#define TP_HBF		57	/* Hyperblock formation */
#define TP_PQS		58	/* Predicate query system */
#define TP_THR		59	/* Tree-Height reduction */

#define TP_TEMP		60	/* Temporary use */

#define TP_IPFEC	61	/* Actualy several phases, but all IPFEC related */
#define TP_A_GSCHED 	62  	/* "AGS", Ipfec global scheduler" */
#define TP_A_LSCHED 	63  	/* "ALS", Ipfec local scheduler */
#define TP_A_PROF       64      /* "APF",  "Ipfec profiling"*/
#define TP_A_REGION     65      /* "ARN",  "Ipfec region formation"*/
#define TP_A_IFCONV     66      /* "AIC",  "Ipfec if conversion"*/
#define TP_A_PRDB       67      /* "APR",  "Ipfec predicate relation database"*/
#define TP_A_RBG        68      /* "ABG",  "Ipfec recovery block generation"*/

#define TP_CYCLE_COUNT  69      /* "TCC", */
#define TP_CYCLE_PU     70      /* "TCP", */  
#define TP_A_MLBR       71      /* "AMB",  "Ipfec post multiple branch"*/
#define TP_OUTLINING	72	/* OUTLINING^$ tracing */
#define TP_A_CANA	73	/* ACA "Ipfec cache analysis and opt" */

#define TP_EH		74	/* EH "Enable trace infos of EH range and EH entry" */
	
/* WARNING: TP_LAST must be at least as large as the largest phase
 * number above, and TP_COUNT must be at least one larger.
 */
#define TP_LAST		75	/* Largest valid phase number */
#define TP_COUNT	76	/* Number of valid phase numbers */
	
/* Extract the phase number from a trace option: */
extern INT32 Get_Trace_Phase_Number ( char **cp, char *arg );

/* List the trace phases to TFile: */
extern void List_Phase_Numbers ( void );

/* ====================================================================
 * TP_PTRACE[1|2]_flags - reserved PTRACE flags
 * ====================================================================
 */
#define TP_PTRACE1_ALL       0x001   /* get all performance tracing */
#define TP_PTRACE1_INL       0x002   /* get INLiner performance tracing */
#define TP_PTRACE1_IPA       0x004   /* get IPA performance tracing */
#define TP_PTRACE1_LNO       0x008   /* get LNO performance tracing */
#define TP_PTRACE1_OPT       0x010   /* get OPT performance tracing */
#define TP_PTRACE1_CG        0x020   /* get CG  performance tracing */
#define TP_PTRACE1_IPALNO    0x040   /* get IPA performance tracing */
#define TP_PTRACE1_IPA_CPROP 0x080   /* get IPA cprop tracing */
#define TP_PTRACE1_CALLINFO  0x100   /* get LNO call info tracing */ 
#define TP_PTRACE1_PARALLEL  0x200   /* get LNO parallel tracing  */ 
#define TP_PTRACE1_NOHDR     0x400   /* suppress printing tlog header */ 


/* ====================================================================
 *
 * Trace Flag Management
 *
 * ====================================================================
 */

/* Set a trace flag specified by:
 *  Function		Argument	Resulting action
 *  --------		--------	----------------
 *  TKIND_INFO		flag mask	Enable masked traces
 *  TKIND_DEBUG		flag mask	Enable masked options
 *  TKIND_IR		phase number	Enable IR trace for phase
 *  TKIND_SYMTAB	phase number	Enable SYMTAB trace for phase
 *  TKIND_TN		phase number	Enable TN trace for phase
 *  TKIND_BB		BB number	Restrict tracing to BB
 *  TKIND_XPHASE	phase number	Stop execution after phase
 *  TKIND_CNTL		control number	Set control option identified
 *  TKIND_ALLOC		phase number	Enable mem allocation traces
 *  phase number	flag mask	Enable masked per-phase traces
 */
extern void Set_Trace ( INT func, INT arg );

/* Note a PU to be traced: */
extern void Set_Trace_Pu ( char *name );
extern void Set_Trace_Pu_Number ( INT number );

/* Set current PU for pu tracing */
extern void Set_Current_PU_For_Trace ( char *name, INT number );

/* Tracing for a single region */
extern void Set_Trace_Region_Number ( INT number );
extern void Set_Current_Region_For_Trace ( INT number );

/* Determine whether a given trace option is enabled.
 */
extern BOOL Get_Trace ( INT func, INT arg );

/* Determine whether a given BB (pass in it's BB_id()) is enabled for 
 * tracing, independent of other trace options:
 */
extern BOOL Get_BB_Trace ( INT bb_id );

/* Determine whether execution should stop after this phase: */
extern BOOL Stop_Execution ( INT phase );


/* ====================================================================
 *
 * Trace File Management
 *
 * ====================================================================
 */

/* The trace file descriptor, to be used in fprintf tracing: */
extern FILE *Get_Trace_File ( void );

/* By default, traces are sent to stdout, which of course is open on
 * startup.  If tracing is to be directed to another file (recommended
 * for permanence), this routine should be called before any tracing is
 * done.  It will close the previous trace file (if not stdout) and
 * open the new one, setting TFile to the new file descriptor.
 */
extern void Set_Trace_File ( char *filename );

extern void Set_Trace_File_internal ( FILE *);

#define TFile Get_Trace_File()

/* Indent the given file according to the current execution stack
 * depth.  This routine is useful for tracing recursive algorithms.
 */
extern void Nest_Indent ( FILE *file );

extern INT Get_Current_Phase_Number( void );
extern void Set_Current_Phase_Number(INT);

#ifdef __cplusplus
}
#endif
#endif /* tracing_INCLUDED */
