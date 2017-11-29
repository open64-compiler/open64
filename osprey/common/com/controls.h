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


#ifndef controls_INCLUDED
#define controls_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: controls.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/controls.h,v $
 *
 * Revision history:
 *  17-Jun-91 - Integrated from Josie flags.h
 *
 * Description:
 *
 * Interface to a general package for uniform access to compiler
 * controls which may be set via command line flags or pragmas.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *controls_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

/* The following is a list of IDs for the available controls: */
typedef enum control {
  CONTROL_MIN_CONTROL = 0,	/* unused, bounds marker */
#define CONTROL_FIRST	CONTROL_ACIR
  CONTROL_ACIR,
  CONTROL_ALIAS,
  CONTROL_ALNDCL,
  CONTROL_ALNREF,
  CONTROL_ALNSTD,
  CONTROL_ARGOVERLAP,
  CONTROL_C,
  CONTROL_CALLMOD,
  CONTROL_CASE,
  CONTROL_CHAR,
  CONTROL_CHKARGS,
  CONTROL_CHKREC,
  CONTROL_CHKSUB,
  CONTROL_CONSTP,
  CONTROL_COMNAME,
  CONTROL_COPYP,
  CONTROL_DEFARGOVERLAP,
  CONTROL_DEFFUNC,
  CONTROL_DEFKEEPARGS,
  CONTROL_DEFLIB,
  CONTROL_DEFNEWMEM,
  CONTROL_DEFREC,
  CONTROL_DEFSEF,
  CONTROL_DEFSRC,
  CONTROL_DEFVOL,
  CONTROL_DIAG,
  CONTROL_DLINE,
  CONTROL_DOMAIN,
  CONTROL_EXITS,
  CONTROL_FBLANK,
  CONTROL_FCM,
  CONTROL_FCOLS,
  CONTROL_FERAL,
  CONTROL_FLOW,
  CONTROL_FP,
  CONTROL_FTAB,
  CONTROL_FUNC,
  CONTROL_G,
  CONTROL_INLINE,
  CONTROL_KEEPARGS,
  CONTROL_LEAF,
  CONTROL_MAP,
  CONTROL_MEMLIMIT,
  CONTROL_NEWMEM,
  CONTROL_NOARGOVERLAP,
  CONTROL_NOFUNC,
  CONTROL_NOINLINE,
  CONTROL_NOKEEPARGS,
  CONTROL_NONEWMEM,
  CONTROL_NOREC,
  CONTROL_NOSEF,
  CONTROL_NOVOL,
  CONTROL_ONETRIP,
  CONTROL_OFORM,
  CONTROL_MOPT,
  CONTROL_PROF,
  CONTROL_PTRVOL,
  CONTROL_QUIT,
  CONTROL_REAL,
  CONTROL_RECURSIVE,
  CONTROL_REG,
  CONTROL_RETPTS,
  CONTROL_SAVE,
  CONTROL_SCHED,
  CONTROL_SEF,
  CONTROL_STDDIAG,
  CONTROL_TAME,
  CONTROL_TARG,
  CONTROL_UNROLL,
  CONTROL_UNROLLEXACT,
  CONTROL_VOLATILE,
  CONTROL_WHOLE,
  CONTROL_WILD,
  CONTROL_XREF,
#define CONTROL_JFE_LAST	(CONTROL_XREF+1)

  /* New controls from Josie/92: */
  CONTROL_IVREP,
  CONTROL_XOPT,

  CONTROL_MAX_CONTROL		/* unused, bounds marker */
#define CONTROL_LAST	CONTROL_MAX_CONTROL
} CONTROL;	

/* The following flags are attached to control options when they are
 * processed, reflecting the context of their appearance:
 */
#define HCO_AAVAL	1  /* from -AA option */
#define HCO_IMPLICIT	2  /* Obtained from expansion of a group */
#define HCO_ONCE	4  /* appeared with %once */
#define HCO_PUSH	8  /* appeared with %push */
#define HCO_POP		16 /* appeared with %pop */
#define HCO_PRAGMA	32 /* from a pragma, as opposed to coomand line */

/* Initialize the controls table: */
extern void  Init_Controls_Tbl ( void );

/* Process a control option string: */
extern void  Handle_Control_Opt ( char *, BOOL );

/* Apply the current values of the controls: */
extern void  Apply_Controls ( void );

/* Get the current value of a control: */
extern INT32 Get_Int_Ctrl_Val  ( CONTROL a );
extern const char *Get_Name_Ctrl_Val ( CONTROL a );

/* Pop once values of all controls of given level: */
extern void Pop_Controls ( INT32 level );

/* Save and restore routine top control values: */
extern void Save_Routine_Top_Ctrls ( void );
extern void Restore_Routine_Top_Ctrls ( void );

/* Restore controls to their command line values: */
extern void Restore_Cmd_Line_Ctrls ( void );

/* Print the current values of the controls: */
extern void Print_Controls ( FILE *fp, const char *tag, BOOL def );

/* Process a pragma, entering it in the control table: */
extern INT Process_Pragma ( char *x );

/* Process a control options */
extern INT Process_Control_Opt (const char *save_a, INT flags );

/* Handle -g/-O combinations */
extern void Fix_g_O(void);

#ifdef __cplusplus
}
#endif
#endif /* controls_INCLUDED */
