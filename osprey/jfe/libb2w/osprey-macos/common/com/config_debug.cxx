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
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_debug.cxx,v $
 *
 * Revision history:
 *  02-Nov-96 - Original Version.
 *
 * Description:
 *
 * Configure the -DEBUG option group (included in config.c).
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_debug_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_debug.cxx,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_debug.h"

/* ====================================================================
 *
 * -DEBUG: option group
 *
 * Define the defaults for -DEBUG option group flags.
 *
 * NOTE:  There are two copies of this defined -- one to copy when
 * initializing a new struct to the defaults, and one to serve as the
 * initial bottom of the Current_DEBUG stack.  Fix both of them when
 * adding/changing things.
 *
 * ====================================================================
 */

static DEBUG_FLAGS Default_DEBUG = {
  NULL,		/* next */

  /* default	set? */
  FALSE,	FALSE,		/* cmod_warn */
  FALSE,	FALSE,		/* cray_port */
  DIV_ZERO_CHECK, FALSE,	/* div_check */
  FALSE,	FALSE,		/* equiv_warn */
  NULL,		FALSE,		/* error */
  FALSE,	FALSE,		/* fullwarn */
  FALSE,	FALSE,		/* full_iface_check */
  FALSE,	FALSE,		/* i32_oflow_check */
  FALSE,	FALSE,		/* int_oflow_check */
  FALSE,	FALSE,		/* optimize space */
  FALSE,	FALSE,		/* parm_alias_check */
  FALSE,	FALSE,		/* ptr_incr_warn */
  TRUE,		FALSE,		/* printf_warn */
  NULL,		FALSE,		/* remark */
  FALSE,	FALSE,		/* shift_check */
  FALSE,	FALSE,		/* shift_warn */
  FALSE,	FALSE,		/* subscript_check */
  FALSE,	FALSE,		/* trap_uv */
  FALSE,	FALSE,		/* trap_uv_rjustify */
  FALSE,	FALSE,		/* trunc_check */
  FALSE,	FALSE,		/* trunc_warn */
  TRUE,		FALSE,		/* varargs_iface_check */
  TRUE,		FALSE,		/* varargs_prototypes */
  FALSE,	FALSE,		/* verbose_runtime */
  NULL,		FALSE,		/* warning */
  NULL, 	FALSE,		/* woff */
  TRUE,				/* ir version check */
  ALIGN_NORMAL,			/* control ldl/ldr generation */
  TRUE,				/* ipalno version check */
  FALSE,        FALSE,          /* conformance_check */
#ifdef KEY
  TRUE,                         /* emit .eh_frame for backtrace */
  FALSE,	FALSE,		/* zero_uv */
#endif
#ifdef TARG_SL
  FALSE,                        /* stack_check for SL*/
#endif
  { 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 }	/* buffer[16] */
};

DEBUG_FLAGS Initial_DEBUG = {
  NULL,		/* next */

  /* default	set? */
  FALSE,	FALSE,		/* cmod_warn */
  FALSE,	FALSE,		/* cray_port */
  DIV_ZERO_CHECK, FALSE,	/* div_check */
  FALSE,	FALSE,		/* equiv_warn */
  NULL,		FALSE,		/* error */
  FALSE,	FALSE,		/* fullwarn */
  FALSE,	FALSE,		/* full_iface_check */
  FALSE,	FALSE,		/* i32_oflow_check */
  FALSE,	FALSE,		/* int_oflow_check */
  FALSE,	FALSE,		/* optimize space */
  FALSE,	FALSE,		/* parm_alias_check */
  FALSE,	FALSE,		/* ptr_incr_warn */
  TRUE,		FALSE,		/* printf_warn */
  NULL,		FALSE,		/* remark */
  FALSE,	FALSE,		/* shift_check */
  FALSE,	FALSE,		/* shift_warn */
  FALSE,	FALSE,		/* subscript_check */
  FALSE,	FALSE,		/* trap_uv */
  FALSE,	FALSE,		/* trap_uv_rjustify */
  FALSE,	FALSE,		/* trunc_check */
  FALSE,	FALSE,		/* trunc_warn */
  TRUE,		FALSE,		/* varargs_iface_check */
  TRUE,		FALSE,		/* varargs_prototypes */
  FALSE,	FALSE,		/* verbose_runtime */
  NULL, 	FALSE,		/* warning */
  NULL, 	FALSE,		/* woff */
  TRUE,				/* ir version check */
  ALIGN_NORMAL,			/* control ldl/ldr generation */
  TRUE,				/* ipalno version check */
  FALSE,        FALSE,          /* conformance_check */
#ifdef KEY
  TRUE,                         /* emit .eh_frame for backtrace */
  FALSE,	FALSE,		/* zero_uv */
#endif
#ifdef TARG_SL
  FALSE,                        /* stack_check for SL */
#endif
  { 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 }	/* buffer[16] */
};

static char *Debug_Alignment_Name=	NULL;

DEBUG_FLAGS *Current_DEBUG = &Initial_DEBUG;

/* ====================================================================
 * Descriptor for the -DEBUG option group.
 * ====================================================================
 */

#define ID	Initial_DEBUG

static OPTION_DESC Options_DEBUG[] = {

    { OVK_BOOL, OV_INTERNAL,	TRUE, "const_mod_warning",	"",
      0, 0, 0, &ID.cmod_warn,		&ID.cmod_warn_set,
	"Warn about attempts to store into a constant" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "cray_port",		"",
      0, 0, 0, &ID.cray_port,		&ID.cray_port_set,
	"Turn on all Cray porting options" },

    { OVK_INT32, OV_VISIBLE,	TRUE, "div_check",	"",
      DEFAULT_DIV_CHECK, 0, 3, &ID.div_check,		&ID.div_check_set,
	"Check for divide by zero and/or division overflow" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "equiv_warning",	"",
      0, 0, 0, &ID.equiv_warn,		&ID.equiv_warn_set,
	"Warn about EQUIVALENCE affected by 32- vs. 64-bit compilation" },

    { OVK_LIST, OV_VISIBLE,	TRUE, "error",		"",
      0, 0, 0, &ID.error,		&ID.error_set,
	"Convert given warning messages to errors" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "fullwarn",	"",
      0, 0, 0, &ID.fullwarn,		&ID.fullwarn_set,
	"Turn on all warning messages" },

    { OVK_BOOL, OV_SHY,		TRUE, "full_interface_check",	"",
      0, 0, 0, &ID.full_iface_check,	&ID.full_iface_check_set,
	"Check all subprogram call interfaces" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "int32_overflow_check",	"",
      0, 0, 0, &ID.i32_oflow_check,	&ID.i32_oflow_check_set,
	"Check 32-bit integer code for overflow" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "int_overflow_check",	"",
      0, 0, 0, &ID.int_oflow_check,	&ID.int_oflow_check_set,
	"Check all integer code for overflow" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "optimize_space",	"",
      0, 0, 0, &ID.optimize_space,		&ID.optimize_space_set,
	"Optimize debug space" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "parameter_alias_check", "",
      0, 0, 0, &ID.parm_alias_check,	&ID.parm_alias_check_set,
	"Check Fortran subprogram parameters for aliasing" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "pointer_increment_warning", "",
      0, 0, 0, &ID.ptr_incr_warn,	&ID.ptr_incr_warn_set,
	"Check increments of Fortran pointers against pointee size" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "printf_warning",	"",
      0, 0, 0, &ID.printf_warn,	&ID.printf_warn_set,
	"Check literal printf format strings against parameter types" },

    { OVK_LIST, OV_VISIBLE,	TRUE, "remark",		"",
      0, 0, 0, &ID.remark,		&ID.remark_set,
	"Convert given warning messages to remarks" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "shift_check",	"",
      0, 0, 0, &ID.shift_check,	&ID.shift_check_set,
	"Check for variable shift counts larger than object" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "shift_warning",	"",
      0, 0, 0, &ID.shift_warn,		&ID.shift_warn_set,
	"Warn about constant shift counts larger than object" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "subscript_check", "",
      0, 0, 0, &ID.subscript_check,	&ID.subscript_check_set,
	"Check for subscripts out of range" },

    { OVK_LIST, OV_VISIBLE,	TRUE, "suppress",	"",
      0, 0, 0, &ID.woff,		&ID.woff_set,
	"Suppress given warning messages" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "trap_uninitialized",	"",
      0, 0, 0, &ID.trap_uv,		&ID.trap_uv_set,
	"Trap references to uninitialized variables" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "trapuv_right_justify",	"",
      0, 0, 0, &ID.trap_uv_rjustify,	&ID.trap_uv_rjustify_set,
	"" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "zero_uninitialized",	"",
      0, 0, 0, &ID.zero_uv,		&ID.zero_uv_set,
	"Set uninitialized variables to zero" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "trunc_check",		"",
      0, 0, 0, &ID.trunc_check,	&ID.trunc_check_set,
	"Check 64- to 32-bit assignments for truncation" },

    { OVK_BOOL, OV_INTERNAL,	TRUE, "trunc_warning",	"",
      0, 0, 0, &ID.trunc_warn,		&ID.trunc_warn_set,
	"Warn about truncation on 64- to 32-bit assignments" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "varargs_interface_check", "",
      0, 0, 0, &ID.varargs_iface_check,&ID.varargs_iface_check_set,
	"Check interfaces on calls to varargs routines" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "varargs_prototypes",	"",
      0, 0, 0, &ID.varargs_prototypes,	&ID.varargs_prototypes_set,
	"Assume ANSI guarantee that varargs routines are prototyped" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "verbose_runtime",	"",
      0, 0, 0, &ID.verbose_runtime,	&ID.verbose_runtime_set,
	"Generate verbose messages for runtime errors" },

    { OVK_LIST, OV_VISIBLE,	TRUE, "warning",		"",
      0, 0, 0, &ID.warning,		&ID.warning_set,
	"Convert given messages to warnings" },

    { OVK_LIST, OV_VISIBLE,	TRUE, "woff",			"",
      0, 0, 0, &ID.woff,		&ID.woff_set,
	"Suppress given warning messages" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "ir_version_check",	"",
      0, 0, 0, &ID.ir_version_check,	NULL,
	"Check whether version of IR matches expected version" },

    { OVK_NAME, OV_VISIBLE,	TRUE, "alignment",		"", 	
      0, 0, 0, &Debug_Alignment_Name,	NULL,
	"Do unaligned load/stores either as normal, compose, or fixade" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "ipa_version_check",	"",
      0, 0, 0, &ID.ipalno_version_check,	NULL,
	"Check whether version of IPALNO matches expected version" },

    { OVK_BOOL, OV_VISIBLE,	TRUE, "conform_check", "",
      0, 0, 0, &ID.conform_check,	&ID.conform_check_set,
	"Check for conforming array assignments" },

    /* Obsolete/unsupported options: */
    { OVK_REPLACED, OV_INTERNAL,	TRUE, "bounds_check",	NULL,
      0, 0, 0,
      const_cast<char*>("-DEBUG:subscript_check"),	NULL,
      "Check array subscripts against bounds" },

    { OVK_REPLACED, OV_INTERNAL,	TRUE, "trapuv",		NULL,
      0, 0, 0,	const_cast<char*>("-DEBUG:trap_uninitialized"),	NULL,
      "Trap references to uninitialized variables" },

    { OVK_REPLACED, OV_INTERNAL,	TRUE, "zerouv",		NULL,
      0, 0, 0,	const_cast<char*>("-DEBUG:zero_uninitialized"),	NULL,
      "Set uninitialized variables to zero" },

#ifdef KEY
    { OVK_BOOL, OV_VISIBLE,	FALSE, "eh_frame",	"",
      0, 0, 0, &ID.emit_ehframe,	NULL,
	"Emit .eh_frame section even for non-C++ programs if this flag is set" },
#endif

#ifdef TARG_SL
    { OVK_INT32, OV_VISIBLE, TRUE, "stack_check", "",
      0, 0, 15, &ID.stack_check, NULL,
      "stack overflow check"   },
#endif

    { OVK_COUNT }		    /* List terminator -- must be last */
};

#undef ID

/* ====================================================================
 *
 * DEBUG_Init_Config
 *
 * Initialize the current top of stack to defaults, by copying the
 * Default_DEBUG struct to it.  Be careful not to lose the next
 * pointer.
 *
 * ====================================================================
 */

void
DEBUG_Init_Config ( void )
{
  DEBUG_FLAGS *next = Current_DEBUG->next;

  *Current_DEBUG = Default_DEBUG;
  Current_DEBUG->next = next;
}

/* ====================================================================
 *
 * DEBUG_Push_Config
 *
 * Push a new struct on top of stack, either a copy of the current
 * TOS, or the defaults.
 *
 * TODO:  To make the option group printing functionality work once the
 * configuration structs start changing, this function must also update
 * the addresses in the group descriptor to point to the current
 * top-of-stack structure.
 *
 * ====================================================================
 */

void
DEBUG_Push_Config ( BOOL use_default )
{
  DEBUG_FLAGS *new_flags =
	(DEBUG_FLAGS *) malloc ( sizeof(DEBUG_FLAGS) );

  if ( new_flags == NULL ) {
    ErrMsg ( EC_No_Mem, "DEBUG_Push" );
  }

  *new_flags = use_default ? Default_DEBUG : *Current_DEBUG;
  new_flags->next = Current_DEBUG;
  Current_DEBUG = new_flags;
}



/* ====================================================================
 *
 * DEBUG_Pop_Config
 *
 * Pop a struct from top of stack and return TRUE if the old TOS was
 * not the original TOS, or do nothing and return FALSE.
 *
 * TODO:  To make the option group printing functionality work once the
 * configuration structs start changing, this function must also update
 * the addresses in the group descriptor to point to the current
 * top-of-stack structure.
 *
 * ====================================================================
 */

BOOL
DEBUG_Pop_Config ( void )
{
  if ( Current_DEBUG->next == NULL ) {
    /* This is the bottom of the stack: */
    return FALSE;
  } else {
    /* Deallocate the top element and pop it: */
    DEBUG_FLAGS *new_flags = Current_DEBUG->next;
    free ( Current_DEBUG );
    Current_DEBUG = new_flags;
    return TRUE;
  }
}



/* ====================================================================
 *
 * DEBUG_Configure_Alignment
 *
 * Configure generation of memory alignment instructions  (ldl/ldr):
 *
 * normal:
 *	generate ldl/ldr as needed
 * fixade:
 *	mark .mips.options as needing kernel fixup traps enabled
 * compose:
 *	generate composite compiler generated instructions to simulate
 *	the unaligned memory access
 *
 * ====================================================================
 */
static void DEBUG_Configure_Alignment(char *val)
{
  if (val)
  {
    INT32  len = strlen(val);

    if (strncasecmp(val, "normal", len)==0)
      DEBUG_Alignment = ALIGN_NORMAL;
    else if (strncasecmp(val, "fixade", len)==0)
      DEBUG_Alignment = ALIGN_FIXADE;
    else if (strncasecmp(val, "compose", len)==0)
      DEBUG_Alignment= ALIGN_COMPOSE;
    else
    {
      ErrMsg ( EC_Inv_OPT, "DEBUG:alignment", val);
    }
    if (!DEBUG_Alignment_Normal)
      DevWarn("-DEBUG:alignment set to %s", val);
  }
}




/* ====================================================================
 *
 * DEBUG_Configure
 *
 * Configure the current top of stack struct.
 *
 * ====================================================================
 */

void
DEBUG_Configure ( void )
{
  OPTION_LIST *ol = Current_DEBUG->woff;

#ifdef FRONT_F90
    extern void add_cray_args (const char * );
    extern void Cray_Woff ( char * );
#endif /* FRONT_F90 */

  /* TODO:  Remove once everything has been rolled over: */
  /* Process -DEBUG:div_check */

  /* -DEBUG:trap_uv_rjustify implies -DEBUG:trap_uv : */
  if ( DEBUG_Trap_Uv_Rjustify ) {
    if ( ! DEBUG_Trap_Uv_Set )
		DEBUG_Trap_Uv = TRUE;
  }

  /* -DEBUG:int_overflow_check implies int32_overflow_check and
   * div_check=3:
   */
  if ( DEBUG_Int_Overflow_Check ) {
    if ( ! DEBUG_Int32_Overflow_Check_Set )
		DEBUG_Int32_Overflow_Check = TRUE;
    if ( ! DEBUG_Div_Check_Set )
		DEBUG_Div_Check = DIV_ZERO_CHECK | DIV_OFLOW_CHECK;
  }

  /* -DEBUG:Cray_port implies all the Cray porting options: */
  if ( DEBUG_Cray_Port ) {
    if ( ! DEBUG_Equiv_Warning_Set )
		DEBUG_Equiv_Warning = TRUE;
    if ( ! DEBUG_Full_Interface_Check_Set )
		DEBUG_Full_Interface_Check = TRUE;
    if ( ! DEBUG_Int32_Overflow_Check_Set )
		DEBUG_Int32_Overflow_Check = TRUE;
    if ( ! DEBUG_Pointer_Increment_Warning_Set )
		DEBUG_Pointer_Increment_Warning = TRUE;
    if ( ! DEBUG_Printf_Warning_Set )
		DEBUG_Printf_Warning = TRUE;
    if ( ! DEBUG_Shift_Check_Set )
		DEBUG_Shift_Check = TRUE;
    if ( ! DEBUG_Shift_Warning_Set )
		DEBUG_Shift_Warning = TRUE;
    if ( ! DEBUG_Trunc_Check_Set )
		DEBUG_Trunc_Check = TRUE;
    if ( ! DEBUG_Trunc_Warning_Set )
		DEBUG_Trunc_Warning = TRUE;
  }

  // DEBUG:subscript_check implies conform_check unless the latter
  // has been explicitly turned off
  if (DEBUG_Subscript_Check && !DEBUG_Conform_Check_Set) {
     DEBUG_Conform_Check = TRUE;
  }

  /* Process -DEBUG:fullwarn */
  if ( DEBUG_Fullwarn ) {
    Min_Error_Severity = ES_ADVISORY;
#ifdef FRONT_END
#ifdef FRONT_F90
    add_cray_args ( "-m2" );
#else
#ifdef EDGSRC
    error_threshold = es_remark;
#endif /* EDGSRC */
#endif /* FRONT_F90 */
#endif /* FRONT_END */
  }

  /* Process -DEBUG:suppress=...:woff=... */
  while ( ol != NULL ) {
    char *msg = OLIST_val(ol);

#ifdef FRONT_END
#ifdef FRONT_F90
    Cray_Woff ( msg );
#else /* FRONT_F90 */
#ifdef EDGSRC
    process_diag_override_option ( optk_diag_suppress, msg );
#endif /* EDGSRC */
#endif /* ~FRONT_F90 */
#endif /* FRONT_END */
    
    Rag_Handle_Woff_Args ( msg );

    ol = OLIST_next(ol);
  }

#ifdef FRONT_END
#ifndef FRONT_F90
#ifdef EDGSRC
  /* Process -DEBUG:remark=... */
  for ( ol = Current_DEBUG->remark; ol != NULL; ol = OLIST_next(ol) ) {
    char *msg = OLIST_val(ol);

    process_diag_override_option ( optk_diag_remark, msg );
  }

  /* Process -DEBUG:warning=... */
  for ( ol = Current_DEBUG->warning; ol != NULL; ol = OLIST_next(ol) ) {
    char *msg = OLIST_val(ol);

    process_diag_override_option ( optk_diag_warning, msg );
  }

  /* Process -DEBUG:error=... */
  for ( ol = Current_DEBUG->error; ol != NULL; ol = OLIST_next(ol) ) {
    char *msg = OLIST_val(ol);

    process_diag_override_option ( optk_diag_error, msg );
  }
#endif /* EDGSRC */
#endif /* ~FRONT_F90 */
#endif /* FRONT_END */

  DEBUG_Configure_Alignment(Debug_Alignment_Name);

}

