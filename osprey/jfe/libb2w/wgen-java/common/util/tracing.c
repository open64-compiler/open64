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


/* ====================================================================
 * ====================================================================
 *
 * Module: tracing.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/tracing.c,v $
 *
 * Revision history:
 *  08-Sep-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *
 * Description:
 *
 * This module provides tracing support in the microcode compiler and
 * associated tools.  See tracing.h for the external interface.
 *
 * ====================================================================
 * ====================================================================
 */

static char *source_file = __FILE__;
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/tracing.c,v $ $Revision: 1.1.1.1 $";

#include <string.h>
#include "defs.h"
#include "tracing.h"
#include "erglob.h"
#include "flags.h"
#include "util.h"

/* The following strings are useful in creating trace messages: */
char *SBar =
"-----------------------------------------------------------------------\n";
char *DBar =
"=======================================================================\n";
char *Sharps =
"#######################################################################\n";

#define INDENT 2

/* The trace file descriptor, to be used in fprintf tracing: */
static FILE *TFile_internal = NULL;


#ifdef FRONT_END_F77
extern FILE *Kqqgso();
#endif

/* Flags controlling progress tracing: */
INT32 Progress_Flags = 0;

/* Local static data: files */
static BOOL Non_stdout_TFile = FALSE;	/* TFile other than stdout? */
static char *TFile_Name = "stdout";	/* TFile name */

/* Local static data: flags */
#define BB_COUNT	50		/* Number of traceable BBs */
#define PU_COUNT	10		/* Number of traceable PUs */
#define REGION_COUNT	10		/* Number of traceable REGIONs */
#define CTRL_COUNT	50		/* Number of control options */
static UINT TI_Mask;			/* Info mask */
static UINT TD_Mask;			/* Debug option mask */
static UINT TI_Phase[TP_COUNT];		/* IR trace flags */
static UINT TS_Phase[TP_COUNT];		/* SYMTAB trace flags */
static UINT TN_Phase[TP_COUNT];		/* TN trace flags */
static UINT TA_Phase[TP_COUNT];		/* Memory Allocation Trace */
static UINT TP_Mask[TP_COUNT];		/* Per-phase trace masks */
static UINT TB_Enable[BB_COUNT];	/* BB enable list */
static UINT BB_Cnt = 0;			/* Next BB traced */
static UINT Xstop_Phase = TP_LAST;	/* Last phase to execute */
static UINT TC_Enable[CTRL_COUNT];	/* Control option list */
static char *PU_Enable[PU_COUNT];	/* PUs to trace */
static INT PU_Num_Enable[PU_COUNT];	/* PUs to trace */
static UINT PU_Cnt = 0;			/* Next PU traced */
static UINT PU_NCnt = 0;		/* Next PU number traced */
static INT Region_Num_Enable[REGION_COUNT]; /* Regions to trace */
static UINT REGION_NCnt = 0;		/* Next REGION number traced */

static char *Current_PU_Name = NULL;
static INT Current_PU_Number = 0;
static INT Current_Region_Number = 0;  /* for debugging regions */

extern int trace_stack(int, int);


/* ====================================================================
 *
 * Phase_Descriptors
 *
 * This array provides 3-character names and full names for the trace
 * phases, for use by Get_Trace_Phase_Number and List_Phase_Numbers
 * below.
 *
 * ====================================================================
 */

typedef struct {
  INT32 num;	/* Phase number (TP_COUNT to terminate list) */
  char *id;	/* 3-character ID for phase */
  char *name;	/* Full descriptive name for phase */
} PDESC;

#define PD_num(p)	(p->num)
#define PD_id(p)	(p->id)
#define PD_name(p)	(p->name)

/* WARNING: The ID MUST BE a 3-character string, with the first
 * character alphabetic (or at least not a digit).  They are
 * interpreted case-insensitive, and therefore must be unique ignoring
 * case.  Also, there is no inherent reason that multiple IDs could
 * not be used for the same phase, except that the listing routine
 * will then print them all...
 */
static PDESC Phases[] = {
  /* Miscellaneous "phases": */
  { TP_PTRACE1,		"PT1",	"Performance #1" },
  { TP_PTRACE2,		"PT2",	"Performance #2" },
  { TP_MISC,		"MSC",	"Miscellaneous" },

  /* Front end phases: */
  { TP_SEMANTICS,	"SEM",	"Semantic analyzer" },
  { TP_IRB,		"IRB",	"IR (WHIRL) builder" },

  /* Intermediate/utility phases: */
  { TP_IR_READ,		"IRR",	"IR (WHIRL) reader/writer" },
  { TP_WHIRL2FC,	"WH2",	"WHIRL to Fortran/C" },
  { TP_WHIRLSIMP,   	"SMP",	"WHIRL simplifier" },
  { TP_REGION,		"RGN",	"Region support" },
  { TP_ORI,		"ORI",	"Olimit Region Insertion" },
  { TP_FEEDBACK,	"FDB",	"Feedback support" },
  { TP_VHO_LOWER,	"VHO",	"VHO lowering" },
  { TP_LOWER,		"LOW",	"WHIRL lowering" },
  { TP_LOWER90,		"L90",	"F90 WHIRL lowering" },

  /* IPA/inlining phases: */
  { TP_INLINE,		"INL",	"Inliner" },
  { TP_IPL,		"IPL",	"IPA local summary phase" },
  { TP_IPA,		"IPA",	"IPA analysis phase" },
  { TP_IPO,		"IPO",	"IPA optimization phase" },
  { TP_IPM,		"IPM",	"IPA miscellaneous" },

  /* Global optimizer: */
  { TP_ALIAS,		"ALI",	"Alias/mod/ref analysis" },
  { TP_WOPT1,		"OPT",	"Global optimization" },
  { TP_WOPT2,		"OP2",	"More global optimization" },
  { TP_WOPT3,		"OP3",	"Even more global optimization" },

  /* Loop nest optimizer: */
  { TP_VECDD,		"VDD",	"Vector data dependency analysis" },
  { TP_LNOPT,		"LNO",	"Loop Nest Optimization" },
  { TP_LNOPT2,		"LN2",	"More Loop Nest Optimization" },
  { TP_LNOPT3,		"LN3",	"Even more Loop Nest Optimization" },

  /* Code generator: */
  { TP_CG,		"CGM",	"Code Generator miscellaneous" },
  { TP_DATALAYOUT,	"LAY",	"Data layout" },
  { TP_CGEXP,		"EXP",	"Code generator expansion" },
  { TP_LOCALIZE,	"LOC",	"Localize TNs" },
  { TP_FIND_GLOB,	"GLR",	"Find global register live ranges" },
  { TP_EBO,		"EBO",	"Extended Block Optimizer" },
  { TP_FLOWOPT,		"FLW",	"Control flow optimization" },
  { TP_HBF,		"HBF",	"Hyperblock Formation" },
  { TP_PQS,		"PQS",	"Predicate query system" },
  { TP_CGPREP,		"PRP",	"Code generator scheduling prep" },
  { TP_CGLOOP,		"LOP",	"Code generator loop optimization" },
  { TP_SWPIPE,		"SWP",	"Software pipelining" },
  { TP_SRA,		"SRA",	"Software pipelining register allocation" },
  { TP_SCHED,		"SCH",	"Scheduling" },
  { TP_GCM,		"GCM",	"Global code motion" },
  { TP_GRA,		"GRA",	"Global register allocation" },
  { TP_ALLOC,		"LRA",	"Local register allocation" },
  { TP_PSGCM,		"PSG",	"Post Schedule Global code motion" },
  { TP_THR,		"THR",	"Tree-Height Reduction" },
  { TP_EMIT,		"EMT",	"Code emission" },

  { TP_TEMP,		"TMP",	"Temporary use" },

  /* This one must be last: */
  { TP_COUNT,		NULL,  NULL }
};

/* ====================================================================
 *
 * Get_Trace_Phase_Number
 *
 * Extract the phase number from a trace option.  It is either numeric,
 * in which case we return the value, or it is a 3-character string, in
 * which case we look up the phase number in the table above.  If we
 * find the number, cp is updated to point to the next character;
 * otherwise it is left unchanged and an error message is printed.
 *
 * ====================================================================
 */

INT32
Get_Trace_Phase_Number (
  char **cp,	/* Pointer to phase number string */
  char *arg )	/* Pointer to argument for error messages */
{
  /* First check whether the phase is given numerically: */
  if ( **cp >= '0' && **cp <= '9' ) {
    return Get_Numeric_Flag ( cp, 0, TP_LAST, 0, arg );

  /* Otherwise, go search the table for it: */
  } else {
    PDESC *phase = Phases;

    while ( PD_num(phase) != TP_COUNT ) {
      if ( strncasecmp ( *cp, PD_id(phase), 3 ) == 0 ) {
	*cp += 3;
	return PD_num(phase);
      }
      ++phase;
    }
    ErrMsg ( EC_Trace_Phase, -1, TP_MIN, TP_LAST );
    return 0;
  }
}

/* ====================================================================
 *
 * List_Phase_Numbers
 *
 * List the trace phases to TFile.
 *
 * ====================================================================
 */

void
List_Phase_Numbers ( void )
{
  PDESC *phase = Phases;

  fprintf ( Get_Trace_File(),
            "Trace phase numbers supported and their values:\n" );
  while ( PD_num(phase) != TP_COUNT ) {
    fprintf ( Get_Trace_File(), "  %3s: -tt%02d:0x%08x (%s)\n",
	      PD_id(phase), PD_num(phase), TP_Mask[PD_num(phase)],
	      PD_name(phase) );
    ++phase;
  }
}

/* ====================================================================
 *
 * Set_Trace
 *
 * Set a trace flag specified by:
 *  Function		Argument	Resulting action
 *  --------		--------	----------------
 *  TKIND_INFO		flag mask	Enable masked traces
 *  TKIND_DEBUG		flag mask	Enable masked options
 *  TKIND_IR		phase number	Enable IR trace for phase
 *  TKIND_SYMTAB	phase number	Enable SYMTAB trace for phase
 *  TKIND_TN		phase number	Enable TN trace for phase
 *  TKIND_BB		BB number	Restrict tracing to BB
 *  TKIND_XPHASE	phase number	Stop execution after phase
 *  TKIND_CTRL		control number	Enable control option
 *  phase number	flag mask	Enable masked per-phase traces
 *
 * ====================================================================
 */

void
Set_Trace ( INT func, INT arg )
{
  /* Check the function value: */
  if ( func < TKIND_MIN || func == 0 ) {
    ErrMsg ( EC_Trace_Func, func );
    return;
  } else if ( func > TP_LAST ) {
    ErrMsg ( EC_Trace_Phase, func, TKIND_MIN, TP_LAST );
    return;
  }

  /* Set the internal flag: */
  switch ( func ) {
    /* INFO mask: */
    case TKIND_INFO:
      TI_Mask |= arg;
      return;

    /* DEBUG mask: */
    case TKIND_DEBUG:
      TD_Mask |= arg;
      return;

    /* IR phase: */
    case TKIND_IR:
      if ( arg != Check_Range (arg, TP_MIN, TP_LAST, 0) ) {
	ErrMsg ( EC_Trace_Phase, arg, TP_MIN, TP_LAST );
      } else {
	TI_Phase[arg] = TRUE;
      }
      return;

    /* SYMTAB phase: */
    case TKIND_SYMTAB:
      if ( arg != Check_Range (arg, TP_MIN, TP_LAST, 0) ) {
	ErrMsg ( EC_Trace_Phase, arg, TP_MIN, TP_LAST );
      } else {
	TS_Phase[arg] = TRUE;
      }
      return;

    /* TN phase: */
    case TKIND_TN:
      if ( arg != Check_Range (arg, TP_MIN, TP_LAST, 0) ) {
	ErrMsg ( EC_Trace_Phase, arg, TP_MIN, TP_LAST );
      } else {
	TN_Phase[arg] = TRUE;
      }
      return;

    /* Alloc phase: */
    case TKIND_ALLOC:
      if ( arg != Check_Range (arg, TP_MIN, TP_LAST, 0) ) {
	ErrMsg ( EC_Trace_Phase, arg, TP_MIN, TP_LAST );
      } else {
	TA_Phase[arg] = TRUE;
      }
      return;

    /* BB number: */
    case TKIND_BB:
      if ( ++BB_Cnt >= BB_COUNT ) {
	ErrMsg ( EC_Trace_BBs, arg );
	--BB_Cnt;
      } else {
	TB_Enable[BB_Cnt] = arg;
      }
      return;

    /* Stop execution after phase: */
    case TKIND_XPHASE:
      if ( arg != Check_Range (arg, TP_MIN, TP_LAST, 0) ) {
	ErrMsg ( EC_Trace_Phase, arg, TP_MIN, TP_LAST );
      } else {
	Xstop_Phase = arg;
      }
      return;

    /* Control option: */
    case TKIND_CTRL:
      if ( arg != Check_Range (arg, 0, CTRL_COUNT-1, 0) ) {
	ErrMsg ( EC_Trace_Control, arg, CTRL_COUNT-1 );
      } else {
	TC_Enable[arg] = TRUE;
      }
      return;

    /* Per-phase mask: */
    default:
      TP_Mask[func] |= arg;
      return;
  }
}

/* Set current PU for pu tracing */
#define RID_CREATE_NEW_ID -1	/* see be/region/region_util.h */
void
Set_Current_PU_For_Trace ( char *name, INT number )
{
  Current_PU_Name = name;
  Current_PU_Number = number;

  Set_Current_Region_For_Trace(RID_CREATE_NEW_ID/* invalid region id */);
}
/* Set current REGION for region tracing */
void
Set_Current_Region_For_Trace ( INT number )
{
  Current_Region_Number = number;
}

/* ====================================================================
 *
 * Set_Trace_Pu
 *
 * Note a PU to be traced.
 *
 * ====================================================================
 */

void
Set_Trace_Pu ( char *name )
{
  if ( ++PU_Cnt >= PU_COUNT ) {
    ErrMsg ( EC_Trace_PUs, name );
    --PU_Cnt;
  } else {
    PU_Enable[PU_Cnt] = name;
  }
  return;
}

void
Set_Trace_Pu_Number ( INT number )
{
  if ( ++PU_NCnt >= PU_COUNT ) {
    ErrMsg ( EC_Trace_PUs, "<number>");
    --PU_NCnt;
  } else {
    PU_Num_Enable[PU_NCnt] = number;
  }
  return;
}

/* ====================================================================
 *
 * Set_Trace_Region
 *
 * Note a Region to be traced.
 *
 * ====================================================================
 */
void
Set_Trace_Region_Number ( INT number )
{
  if ( ++REGION_NCnt >= REGION_COUNT ) {
    ErrMsg ( EC_Trace_REGIONs, "<number>");
    --REGION_NCnt;
  } else
    Region_Num_Enable[REGION_NCnt] = number;
  return;
}

/* ====================================================================
 *
 * Get_BB_Trace
 *
 * Determine whether a given BB is enabled for tracing, independent of
 * other trace options.  If specific PUs are enabled, any BBs in that
 * PU are automatically enabled.
 *
 * ====================================================================
 */

BOOL
Get_BB_Trace ( INT32 bb_id )
{
  INT16 i;
  BOOL enabled = TRUE;

  if ( PU_Cnt > 0 ) {
    for ( i = 1; i <= PU_Cnt; i++ ) {
      if ( strcmp(PU_Enable[i],Current_PU_Name) == 0 ) return TRUE;
    }
    enabled = FALSE;
  }
  if ( PU_NCnt > 0 ) {
    for ( i = 1; i <= PU_NCnt; i++ ) {
      if ( PU_Num_Enable[i] == Current_PU_Number ) return TRUE;
    }
    enabled = FALSE;
  }

  if ( BB_Cnt > 0 ) {
    for ( i = 1; i <= BB_Cnt; i++ ) {
      if (TB_Enable[i] == bb_id) return TRUE;
    }
    return FALSE;
  }

  /* If there are no -tf or -tb flags, everything is enabled: */
  return enabled;
}

/* ====================================================================
 *
 * Get_Trace
 *
 * Determine whether the trace flag given by func/arg is set.
 *
 * ====================================================================
 */

BOOL
Get_Trace ( INT func, INT arg )
{
  BOOL result;
  INT16 i;

  /* Get the internal flag: */
  switch ( func ) {
    /* INFO mask: */
    case TKIND_INFO:
      result = (TI_Mask & arg) != 0;
      break;
    /* DEBUG mask: */
    case TKIND_DEBUG:
      result = (TD_Mask & arg) != 0;
      break;
    /* IR phase: */
    case TKIND_IR:
      result = TI_Phase[arg];
      break;
    /* SYMTAB phase: */
    case TKIND_SYMTAB:
      result = TS_Phase[arg];
      break;
    /* TN phase: */
    case TKIND_TN:
      result = TN_Phase[arg];
      break;
    /* Control option: */
    case TKIND_CTRL:
      result = TC_Enable[arg];
      break;
    /* Memory stats: */
    case TKIND_ALLOC:
      result = TA_Phase[arg];
      break;
    /* fall-through to default case */
    case TKIND_BB:
    case TKIND_XPHASE:
    /* Per-phase mask: */
    default:
      result = (TP_Mask[func] & arg) != 0;
      break;
  }

  if ( result && PU_Cnt > 0 ) { /* trace for certain PUs, by name */
#ifdef KEY
    if( Current_PU_Name == NULL ){
      result = FALSE;
    } else
#endif
    for ( i = 1; i <= PU_Cnt; i++ ) {
      if ( strcmp(PU_Enable[i], Current_PU_Name) == 0 )
	break;
    }
    if (i > PU_Cnt)
      result = FALSE;
  }

  if ( result && PU_NCnt > 0 ) { /* trace for certain PUs, by number */
    for ( i = 1; i <= PU_NCnt; i++ ) {
      if ( PU_Num_Enable[i] == Current_PU_Number )
	break;
    }
    if (i > PU_NCnt)
      result = FALSE;
  }

  if (result && REGION_NCnt > 0) { /* trace for certain regions, by number */
    for ( i = 1; i <= REGION_NCnt; i++ ) {
      if ( Region_Num_Enable[i] == Current_Region_Number )
	break;
    }
    if ( i > REGION_NCnt )
      result = FALSE;
  }

  return result;
}

/* ====================================================================
 *
 * Stop_Execution
 *
 * Determine whether the given phase number is beyond the specified
 * last execution phase.  This is currently a simple linear sequence
 * of phase numbers, with no complications.
 *
 * ====================================================================
 */

BOOL
Stop_Execution ( INT phase )
{
  return phase >= Xstop_Phase;
}

/* ====================================================================
 *
 * Set_Trace_File
 *
 * By default, traces are sent to stdout, which of course is open on
 * startup.  If tracing is to be directed to another file (recommended
 * for permanence), this routine should be called before any tracing is
 * done.  It will close the previous trace file (if not stdout) and
 * open the new one, setting TFile to the new file descriptor.
 *
 * ====================================================================
 */

void
Set_Trace_File ( 
  char *filename )	/* Name of new trace file */
{
  if ( Non_stdout_TFile && TFile_internal != NULL ) {
#ifndef FRONT_END_F77
    fclose (TFile_internal);
#endif
    Set_Error_Trace (NULL);
  }
  
  if ( filename != NULL ) {
#ifdef FRONT_END_F77
    TFile_internal = Kqqgso();
#else
    TFile_internal = fopen ( filename, "w" );	/* Truncate */
#endif
    if ( TFile_internal != NULL ) {
      TFile_Name = filename;
      Non_stdout_TFile = TRUE;
      Set_Error_Trace (TFile_internal);
      return;
    }
    ErrMsg ( EC_Trace_Open, filename, errno );
  }
  TFile_internal = stdout;
  TFile_Name = NULL;
  Non_stdout_TFile = FALSE;
  Set_Error_Trace (NULL);
}

void Set_Trace_File_internal(FILE *f) 
{
  TFile_internal = f;
}

FILE *Get_Trace_File(void) 
{
  if (TFile_internal == NULL)
    TFile_internal = stdout;

  return TFile_internal;
}

void
Trace_To_Stderr(void)
{
  if ( Non_stdout_TFile && TFile_internal != NULL ) {
#ifndef FRONT_END_F77
    fclose (TFile_internal);
#endif
    Set_Error_Trace (NULL);
  }

  TFile_internal = stderr;
  TFile_Name = NULL;
  Non_stdout_TFile = FALSE;
  Set_Error_Trace (NULL);
}
  

#ifndef MONGOOSE_BE
/* ====================================================================
 *
 * Nest_Indent
 *
 * Emit indenting spaces to the given file, based on the current
 * (compiler) stack depth.
 *
 * ====================================================================
 */

void
Nest_Indent ( FILE *fp )
{
  fprintf ( fp, "%*c", ( INDENT * ( trace_stack(0,0) - 1 ) ), ' ' );
}
#endif /* MONGOOSE_BE */
