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
 * Module: config_debug.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_debug.h,v $
 *
 * Revision history:
 *  02-Nov-96 - Original Version
 *
 * Description:
 *
 * Define the external interface to the internal flags representing the
 * -DEBUG group options.  It is a single struct, so that addition of
 * flags for new options does not require additions to the be Exported
 * file, and so that push/pop operations are simplified.
 *
 * NOTE:  Only the standard group option reader, and routines in the
 * associated file config_debug.c, should modify the structs declared
 * here.  By following this discipline, leaving a few undefined flags
 * at the end of the struct, and adding new flags there, we can avoid 
 * serious version incompatibilities between be.so and its clients.
 *
 * ====================================================================
 *
 * To add a new option:
 *
 *   1) In the DEBUG_FLAGS options struct defined below, add a field to
 *	receive the new option value.  If you need a flag indicating
 *	whether the option was set explicitly on the command line, add
 *	a BOOL for that as well, with an appended "_set" in its name.
 *	(You might also need another field if the option will be used
 *	in a different form after configuration, i.e. the option value
 *	is a string that is converted to a number.  If so, add another
 *	field.)
 *
 *	The fields are starting out in alphabetical order by option
 *	name.  When adding new ones, keep in mind that adding them in
 *	the middle will create a required sychronization between the
 *	new be.so and other be components (for purposes of using the
 *	later options).  That may be alright, but if you want to avoid
 *	it, add the new fields just before the buffer at the end (and
 *	you can move them into place later when the synchronization is
 *	tolerable, if you care).
 *
 *   2) Below the DEBUG_FLAGS definition are #defines for the
 *	"DEBUG_Option_Name" pseudo-variables that everyone will use to
 *	reference them.  Add #defines for your new ones.  Note that
 *	they all have DEBUG_ prefixes and are capitalized like global
 *	variables.
 *
 *   3) There are two static instances of DEBUG_FLAGS in config_debug.c.
 *	Default_DEBUG contains default values to be used when
 *	initializing new structs (when we implement pushing/popping
 *	for regions), and Initial_DEBUG contains the initial defaults.
 *	Add the correct default values for your options there.
 *
 *   4) The option group descriptor is also in config_debug.c.  Add
 *	your new option there.  Note that the option descriptors are
 *	specified using a small set of macros defined above the
 *	descriptor.
 *
 *   5) If any configuration is required after reading them in, add the
 *	required code to DEBUG_Configure in config_debug.c.
 *
 * NOTE:  It is NOT necessary to add anything to the be Exported list.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_debug_INCLUDED
#define config_debug_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_debug_h_rcs_id = "$Source: /home/bos/bk/kpro64-pending/common/com/SCCS/s.config_debug.h $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct skiplist;
struct option_list;

/* ====================================================================
 *
 * -DEBUG: option group
 *
 * Define the global structure containing -DEBUG option group flags.
 *
 * WARNING:  Most of the fields in this struct must be addressable by
 * an option group descriptor -- hence BOOL instead of mBOOL.
 *
 * ====================================================================
 */

typedef struct debug_flags {
  /* Support a stack of structs, e.g. for region support.
   * Each stack element points to its predecessor; the bottom to NIL.
   */
  struct debug_flags *next;	/* Next copy on stack */

  BOOL cmod_warn;		/* Storing values in const variables */
  BOOL cmod_warn_set;		/* ... option explicitly set ... */
  BOOL cray_port;		/* Enable all Cray porting options */
  BOOL cray_port_set;		/* ... option explicitly set ... */
  INT32 div_check;		/* Integer division by zero, overflow */
  BOOL div_check_set;		/* ... option explicitly set ... */
  BOOL equiv_warn;		/* Data size changes affect EQUIVALENCE */
  BOOL equiv_warn_set;		/* ... option explicitly set ... */
  struct option_list *error;	/* Warnings to treat as errors */
  BOOL error_set;		/* ... option explicitly set ... */
  BOOL fullwarn;		/* Emit all warning messages */
  BOOL fullwarn_set;		/* ... option explicitly set ... */
  BOOL full_iface_check;	/* Check all call interfaces */
  BOOL full_iface_check_set;	/* ... option explicitly set ... */
  BOOL i32_oflow_check;		/* 32-bit integer overflow checks */
  BOOL i32_oflow_check_set;	/* ... option explicitly set ... */
  BOOL int_oflow_check;		/* all integer overflow checks */
  BOOL int_oflow_check_set;	/* ... option explicitly set ... */
  BOOL optimize_space;		/* optimize space in debug info */
  BOOL optimize_space_set;	/* ... option explicitly set ... */
  BOOL parm_alias_check;	/* Fortran parameter aliasing violations */
  BOOL parm_alias_check_set;	/* ... option explicitly set ... */
  BOOL ptr_incr_warn;		/* Fortran ptr increments by odd amounts */
  BOOL ptr_incr_warn_set;	/* ... option explicitly set ... */
  BOOL printf_warn;		/* printf formats don't match arg sizes */
  BOOL printf_warn_set;		/* ... option explicitly set ... */
  struct option_list *remark;	/* Warnings/errors to treat as remarks */
  BOOL remark_set;		/* ... option explicitly set ... */
  BOOL shift_check;		/* Shift counts larger than object size */
  BOOL shift_check_set;		/* ... option explicitly set ... */
  BOOL shift_warn;		/* Shift counts larger than object size */
  BOOL shift_warn_set;		/* ... option explicitly set ... */
  BOOL subscript_check;		/* Subscripting out of range */
  BOOL subscript_check_set;	/* ... option explicitly set ... */
  BOOL trap_uv;			/* Enable init of local uninitialized vars */
  BOOL trap_uv_set;		/* ... option explicitly set ... */
  BOOL trap_uv_rjustify;	/* Right justify init of uninitialized vars */
  BOOL trap_uv_rjustify_set;	/* ... option explicitly set ... */
  BOOL trunc_check;		/* 64- to 32-bit assignments */
  BOOL trunc_check_set;		/* ... option explicitly set ... */
  BOOL trunc_warn;		/* 64- to 32-bit assignments */
  BOOL trunc_warn_set;		/* ... option explicitly set ... */
  BOOL varargs_iface_check;	/* Check varargs call interfaces */
  BOOL varargs_iface_check_set;	/* ... option explicitly set ... */
  BOOL varargs_prototypes;	/* Don't require varargs prototypes */
  BOOL varargs_prototypes_set;	/* ... option explicitly set ... */
  BOOL verbose_runtime;		/* Verbose runtime error reporting */
  BOOL verbose_runtime_set;	/* ... option explicitly set ... */
  struct option_list *warning;	/* Warnings/errors to treat as warnings */
  BOOL warning_set;		/* ... option explicitly set ... */
  struct option_list *woff;	/* Explicitly suppressed warnings */
  BOOL woff_set;		/* ... option explicitly set ... */
  BOOL ir_version_check;	/* check whether IR is of matching version */
  INT32 alignment;		/* controls generation of ldl/ldr */
  BOOL ipalno_version_check; 	/* check whether IPALNO file is of matching 
				   version */ 
  BOOL conform_check;		/* F90 Conformance check */
  BOOL conform_check_set;	/* ... option explicitly set ... */
#ifdef KEY
  BOOL emit_ehframe;            /* Emit .eh_frame section for backtrace. */
  BOOL zero_uv;			/* Enable setting local vars to 0 */
  BOOL zero_uv_set;		/* ... option explicitly set ... */
#endif

  /* This buffer area allows references to new fields to be added in
   * later revisions, from other DSOs, without requiring a new be.so
   * or running the risk of referencing illegal data.  Assuming that
   * the buffer is initialized to zeroes, any such references will
   * simply pick up FALSE values (for the Booleans):
   */
  INT32 buffer[16];		/* Buffer space -- initialize to FALSE */
} DEBUG_FLAGS;

/* ====================================================================
 *
 * -DEBUG: option group
 *
 * Global data objects and manipulation functions.
 *
 * ====================================================================
 */

/* This is always the current top of stack: */
extern DEBUG_FLAGS *Current_DEBUG;

/* And this is always the invariant bottom of stack: */
extern DEBUG_FLAGS Initial_DEBUG;

/* Values for div_check option -- independent bits (3 => both): */
#define DIV_ZERO_CHECK	1	/* Divide-by-zero check */
#define DIV_OFLOW_CHECK	2	/* Divide overflow (MAX/-1) check */
#define DEFAULT_DIV_CHECK	DIV_ZERO_CHECK

/* Values for generation of ld/st alignment */
#define	ALIGN_NORMAL	0	/* generate ldl/ldr as needed 	*/
#define	ALIGN_FIXADE	1	/* kernel will fix unaligned memory instruction */
#define	ALIGN_COMPOSE	2	/* compile generated instructions for non-aligned data */
#define	DEFAULT_ALIGN	ALIGN_NORMAL

/* Access to the current TOS struct is via pseudo-global variables: */
/* bounds_check same as subscript_check */
#define DEBUG_Const_Mod_Warning		(Current_DEBUG->cmod_warn)
#define DEBUG_Const_Mod_Warning_Set	(Current_DEBUG->cmod_warn_set)
#define DEBUG_Cray_Port			(Current_DEBUG->cray_port)
#define DEBUG_Cray_Port_Set		(Current_DEBUG->cray_port_set)
#define DEBUG_Div_Check			(Current_DEBUG->div_check)
#define DEBUG_Div_Check_Set		(Current_DEBUG->div_check_set)
# define DEBUG_Div_Zero_Check	(Current_DEBUG->div_check & DIV_ZERO_CHECK)
# define DEBUG_Div_Oflow_Check	(Current_DEBUG->div_check & DIV_OFLOW_CHECK)
#define DEBUG_Equiv_Warning		(Current_DEBUG->equiv_warn)
#define DEBUG_Equiv_Warning_Set		(Current_DEBUG->equiv_warn_set)
/* error=... not externally visible */
#define DEBUG_Fullwarn			(Current_DEBUG->fullwarn)
#define DEBUG_Fullwarn_Set		(Current_DEBUG->fullwarn_set)
#define DEBUG_Full_Interface_Check	(Current_DEBUG->full_iface_check)
#define DEBUG_Full_Interface_Check_Set	(Current_DEBUG->full_iface_check_set)
#define DEBUG_Int32_Overflow_Check	(Current_DEBUG->i32_oflow_check)
#define DEBUG_Int32_Overflow_Check_Set	(Current_DEBUG->i32_oflow_check_set)
#define DEBUG_Int_Overflow_Check	(Current_DEBUG->int_oflow_check)
#define DEBUG_Int_Overflow_Check_Set	(Current_DEBUG->int_oflow_check_set)
#define DEBUG_Optimize_Space		(Current_DEBUG->optimize_space)
#define DEBUG_Parameter_Alias_Check	(Current_DEBUG->parm_alias_check)
#define DEBUG_Parameter_Alias_Check_Set	(Current_DEBUG->parm_alias_check_set)
#define DEBUG_Pointer_Increment_Warning	(Current_DEBUG->ptr_incr_warn)
#define DEBUG_Pointer_Increment_Warning_Set (Current_DEBUG->ptr_incr_warn_set)
#define DEBUG_Printf_Warning		(Current_DEBUG->printf_warn)
#define DEBUG_Printf_Warning_Set	(Current_DEBUG->printf_warn_set)
/* remark=... not externally visible */
#define DEBUG_Shift_Check		(Current_DEBUG->shift_check)
#define DEBUG_Shift_Check_Set		(Current_DEBUG->shift_check_set)
#define DEBUG_Shift_Warning		(Current_DEBUG->shift_warn)
#define DEBUG_Shift_Warning_Set		(Current_DEBUG->shift_warn_set)
#define DEBUG_Subscript_Check		(Current_DEBUG->subscript_check)
#define DEBUG_Subscript_Check_Set	(Current_DEBUG->subscript_check_set)
#define DEBUG_Trap_Uv			(Current_DEBUG->trap_uv)
#define DEBUG_Trap_Uv_Set		(Current_DEBUG->trap_uv_set)
#define DEBUG_Trap_Uv_Rjustify		(Current_DEBUG->trap_uv_rjustify)
#define DEBUG_Trap_Uv_Rjustify_Set	(Current_DEBUG->trap_uv_rjustify_set)
#define DEBUG_Trunc_Check		(Current_DEBUG->trunc_check)
#define DEBUG_Trunc_Check_Set		(Current_DEBUG->trunc_check_set)
#define DEBUG_Trunc_Warning		(Current_DEBUG->trunc_warn)
#define DEBUG_Trunc_Warning_Set		(Current_DEBUG->trunc_warn_set)
#define DEBUG_Varargs_Interface_Check	(Current_DEBUG->varargs_iface_check)
#define DEBUG_Varargs_Interface_Check_Set (Current_DEBUG->varargs_iface_check_set)
#define DEBUG_Varargs_Prototypes	(Current_DEBUG->varargs_prototypes)
#define DEBUG_Varargs_Prototypes_Set	(Current_DEBUG->varargs_prototypes_set)
#define DEBUG_Verbose_Runtime		(Current_DEBUG->verbose_runtime)
#define DEBUG_Verbose_Runtime_Set	(Current_DEBUG->verbose_runtime_set)
/* warning=... not externally visible */
/* woff=...:suppress=... not externally visible */
#define DEBUG_Ir_Version_Check		(Current_DEBUG->ir_version_check)
#define DEBUG_IPALNO_Version_Check	(Current_DEBUG->ipalno_version_check)

#define DEBUG_Alignment			(Current_DEBUG->alignment)
#define DEBUG_Alignment_Normal		(Current_DEBUG->alignment==ALIGN_NORMAL)
#define DEBUG_Alignment_Fixade		(Current_DEBUG->alignment==ALIGN_FIXADE)
#define DEBUG_Alignment_Compose		(Current_DEBUG->alignment==ALIGN_COMPOSE)
#define DEBUG_Conform_Check		(Current_DEBUG->conform_check)
#define DEBUG_Conform_Check_Set	        (Current_DEBUG->conform_check_set)
#ifdef KEY
#define DEBUG_Emit_Ehframe              (Current_DEBUG->emit_ehframe)
#define DEBUG_Zero_Uv			(Current_DEBUG->zero_uv)
#define DEBUG_Zero_Uv_Set		(Current_DEBUG->zero_uv_set)
#endif


/* Initialize the current top of stack to defaults: */
extern void DEBUG_Init_Config ( void );

/* Push a new struct on top of stack, either a copy of the current
 * TOS, or the defaults:
 */
extern void DEBUG_Push_Config ( BOOL use_default );

/* Pop a struct from top of stack and return TRUE if the old TOS was
 * not the original TOS, or do nothing and return FALSE:
 */
extern BOOL DEBUG_Pop_Config ( void );

/* Configure the current top of stack struct: */
extern void DEBUG_Configure ( void );


#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* config_debug_INCLUDED */
