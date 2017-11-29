
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

/*
 * ==========================================================================
 *
 * Module   : unwind.h -- IA64 unwind consumer header file
 *	      Used by debuggers, exception handling, stack trace
 *
 * ==========================================================================
 */

#ifndef __SYS_IA64_UNWIND_H
#define __SYS_IA64_UNWIND_H

#define VERSION_NUMBER 1

#include <sys/ia64/unwind_ia64.h>

#ifndef _LP64
/* Structure modified to reflect IA64 sizes and alignment.  GMB */
typedef struct unw_sigaltstack {
  uint64_t /* void * */	ss_sp;
  uint32_t /* int */ 		ss_flags;
  uint32_t 			ss_empty;		/* added. GMB. */
  uint64_t /* size_t */	ss_size;
} unw_stack_t;

#define __UNW_STRUCT_FPREG  long double


struct _Unwind_Exception;

struct sigcontext {
        uint64_t           sc_flags;       /* see manifest constants above */
        uint64_t           sc_nat;         /* bit i == 1 iff gr[i] is a NaT */
        unw_stack_t        sc_stack;       /* previously active stack */

        uint64_t           sc_ip;          /* instruction pointer */
        uint64_t           sc_cfm;         /* current frame marker */
        uint64_t           sc_psr;         /* (some bits of the) processor status reg. */
        uint64_t           sc_ar_rsc;      /* register stack configuration register */
        uint64_t           sc_ar_bsp;      /* backing store pointer */
        uint64_t           sc_ar_rnat;     /* RSE NaT collection register */
        uint64_t           sc_ar_ccv;      /* compare and exchange compare value register */
        uint64_t           sc_ar_unat;     /* ar.unat of interrupted context */
        uint64_t           sc_ar_fpsr;     /* floating-point status register */
        uint64_t           sc_ar_pfs;      /* previous function state */
        uint64_t           sc_ar_ec;       /* epilog count register */
        uint64_t           sc_ar_lc;       /* loop count register */
        uint64_t           sc_pr;          /* predicate registers */
        uint64_t           sc_br[8];       /* branch registers */
	uint64_t	   sc_gr[128];
        __UNW_STRUCT_FPREG sc_fr[128];     /* floating-point registers */

        /*
         * The mask must come last so we can increase _NSIG_WORDS
         * without breaking binary compatibility.
         */
        uint64_t                sc_mask;        /* signal mask to restore after handler returns */
};

#else
#include <signal.h>
/* #include <asm/sigcontext.h> */
#define __UNW_STRUCT_FPREG  struct ia64_fpreg

#endif

struct _Unwind_Context
{
  uint64_t unwind_table_addr;	/* cached pointer to unwind table for ip */
  uint64_t unwind_info_addr;	/* cached pointer to unwind info for ip */
  uint64_t sc_priunat;		/* primary unat collection */
  uint64_t sc_ar_ec;		/* epilog counter */
  struct sigcontext context;
};

typedef struct _Unwind_Context unw_sigcontext_t;

#ifdef __cplusplus
extern "C" {
#endif


typedef enum {
  
	/*
	 * Indicates that this is not the desired destination frame.
	 */
  _URC_NO_REASON		= 0,

	/*
	 * Exception caught by a different runtime.  Warning: nested
	 * foreign exceptions or rethrowing a foreign exception, result
	 * in undefined behavior.
	 */
  _URC_FOREIGN_EXCEPTION_CAUGHT = 1,

	/*
	 * The personality routine encountered an unexpected error during
	 * phase 2, e.g., stack corruption caused by landing pad code.
	 */
  _URC_FATAL_PHASE2_ERROR	= 2,

	/*
	 * The personality routine encountered an unexpected error during
	 * phase 1.  The unwind runtime has not modified the stack.
	 * Normally, the C++ runtime will call terminate().
	 */
  _URC_FATAL_PHASE1_ERROR	= 3,

	/*
	 * 
	 */
  _URC_NORMAL_STOP		= 4,

	/*
	 * The unwinder encountered the end of the stack during phase 1,
	 * without finding a handler.   The unwind runtime will not have
	 * modified the stack.  The C++ runtime will normally call
	 * uncaught_exception() in this case.
	 */
  _URC_END_OF_STACK		= 5,

	/*
	 */
  _URC_HANDLER_FOUND		= 6,

	/*
	 */
  _URC_INSTALL_CONTEXT		= 7
} _Unwind_Reason_Code;


/*
 *	The following flags control the behavior of the personality
 * routines which are recovered from the unwind tables.
 *
 *	_UA_SEARCH_PHASE: does the current routine contain a handler?
 *	      true  => _URC_HANDLER_FOUND
 *	      false => _URC_CONTINUE_UNWIND
 *      _UA_CLEANUP_PHASE: cleansup and returns _URC_CONTINUE_UNWIND
 *	      or uses a landing pad and returns _URC_INSTALL_CONTEXT
 *
 * _UA_SEARCH_PHASE and _UA_CLEANUP_PHASE are mutually exclusive.
 *
 *	_UA_HANDLER_FRAME: Phase 1 determined that this frame was
 *	      the one with the handler, Phase 2 must handle the
 *	      exception in this frame.
 *
 *	_UA_FORCE_UNWIND: Exception may not be caught, used by
 *	      longjmp or thread cancellation.  Note that _Unwind_Resume
 *	      may be called rather than having the personality
 *	      routine return.
 */

typedef int _Unwind_Action;
#define _UA_SEARCH_PHASE 1
#define _UA_CLEANUP_PHASE 2
#define _UA_HANDLER_FRAME 4
#define _UA_FORCE_UNWIND 8

struct _Unwind_Exception;

typedef void (*_Unwind_Exception_Cleanup_Fn)
  (_Unwind_Reason_Code reason,
   struct _Unwind_Exception *exc);

struct _Unwind_Exception {
  uint64_t                       exception_class;
  _Unwind_Exception_Cleanup_Fn   exception_cleanup;
  uint64_t                       private_1;
  uint64_t                       private_2;
};

typedef _Unwind_Reason_Code (* __personality_routine)
  (int version,
   _Unwind_Action actions,
   uint64_t exceptionClass,
   struct _Unwind_Exception *exceptionObject,
   struct _Unwind_Context *context);

typedef _Unwind_Reason_Code (* _Unwind_Stop_Fn)
  (int version,
   _Unwind_Action actions,
   uint64_t exceptionClass,
   struct _Unwind_Exception *exceptionObject,
   struct _Unwind_Context *context,
   void* stop_parameter);

   
_Unwind_Reason_Code
_Unwind_RaiseException(
    struct _Unwind_Exception *exception_object);

_Unwind_Reason_Code
_Unwind_ForceUnwind(
    struct _Unwind_Exception *exception_object,
    _Unwind_Stop_Fn stop,
    void* stop_parameter);

void
_Unwind_Resume(
    struct _Unwind_Exception *exception_object);

void
_Unwind_DeleteException(
    struct _Unwind_Exception *exception_object);


uint64_t
_Unwind_GetGR(
    struct _Unwind_Context *context,
    int index);

void
_Unwind_SetGR(
    struct _Unwind_Context *context,
    int index,
    uint64_t new_value);

uint64_t
_Unwind_GetIP(
    struct _Unwind_Context *context);

void
_Unwind_SetIP(
    struct _Unwind_Context *context,
    uint64_t new_value);

uint64_t
_Unwind_GetLanguageSpecificData(
    struct _Unwind_Context *context);

uint64_t
_Unwind_GetRegionStart(
    struct _Unwind_Context *context);

int __cxa_personality_routine(
    int version,
    int phase,
    uint64_t exceptionClass,
    struct _Unwind_Exception *exceptionObject,
    struct _Unwind_Context *context);
    


/***********************************************************************
 ***********************************************************************
 *
 *	The following routines are part of the original unwind library
 * and the gdb interface to the unwind library.  They are not part of
 * the standard API.
 */


/* consumer API */

/* functionality for general operations */
__unw_error_t unwind_init(void);
__unw_error_t unwind_fini(void);

/* consumer function to unwind past one activation record,
   except in simulator it is OK to pass NULL for pid */
__unw_error_t unwind_frame(unw_sigcontext_t *scp);

/* initialize callback routines for debugger interface */
void unwind_debugger_init(int (*dbg_unwind_table_addr_arg)(uint64_t,
						uint64_t *, uint64_t *),
			int (*dbg_unwind_info_addr_arg)(uint64_t,
						uint64_t *, uint64_t *),
			int (*dbg_unwind_info_target_addr_arg)(uint64_t,
								uint64_t *),
			int (*dbg_text_segment_target_addr_arg)(uint64_t,
								uint64_t *),
			int (*dbg_restore_gp_arg)(uint64_t,
								uint64_t *),
			int (*dbg_addr_read_arg)(uint64_t,
						uint64_t, void *));



int
trace_back_stack(
     int num,
     uint64_t *ips,
     const char **names,
     int ips_sz,
     int names_sz);


#ifdef __cplusplus
}
#endif

#endif
