/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


#ifndef ERRORS_INCLUDED
#define ERRORS_INCLUDED
#ifndef ERRDESC_INCLUDED
#include "errdesc.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: errors.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:57 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/errors.h,v $
 *
 * Revision history:
 *  21-Aug-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *  25-Sep-91 - Fixed documentation, added prototypes
 *  13-Nov-92 - Integrated Josie/92 functionality
 *
 * Description:
 *
 * This file defines the external interface to a general-purpose error
 * reporting mechanism for compilers.  Its objectives are:
 *
 *  1)	To provide a simple error reporting in the source (i.e. minimal
 *	code), with adequate options to fully describe the error.
 *
 *  2)	To require minimal effort to modify the error's description and
 *	handling.  This implies central descriptors driving the error
 *	handling, especially to deal with frequent reuse of error
 *	information (e.g. "Illegal tree node").
 *
 *  3)	To keep the error handler almost completely independent of the
 *	compiler of which it is a part, so that it can also be used in
 *	other tools, e.g. the linker and librarian.  This is achieved
 *	by using external components for program-specific material.
 *
 * ====================================================================
 *
 * The Error Reporting Interface
 *
 * In order to make use of an already-defined error message, the
 * interface defined consists of the following calls:
 *
 *  void ErrMsg (
 *      INT Error_Code,
 *      ...
 *  )
 *
 *	produces the error message associated with Error_Code, with
 *	up to six parameters.  Error_Code and parameters will be
 *	discussed further below.  The error is presumed to result from
 *	the current file and line number (discussed below), where the
 *	current line number will generally be undefined for non-front
 *	end passes.
 *
 *  void ErrMsgLine (
 *      INT Error_Code,
 *      INT line_number,
 *      ...
 *  )
 *
 *	is identical to ErrMsg, except that the relevant source line
 *	number is passed.
 *
 *  void ErrMsgSrcpos (
 *      INT Error_Code,
 *      SRCPOS srcpos,
 *      ...
 *  )
 *
 *	is identical to ErrMsg, except that the relevant source position
 *	is passed.
 *
 *  void Assert (
 *      BOOL condition,
 *      ( INT Error_Code, ... )
 *  )
 *
 *	checks for truth of the condition and, if false, invokes the
 *	equivalent of ErrMsg to report a (presumably) compiler error
 *	along with the compiler source file and line number.  The
 *	extra set of parentheses is necessary to allow cpp to process
 *	the call with arbitrary parameter count -- the condition is
 *	checked in-line.  Note that we assume that calls to Assert
 *	will be kept in production code.
 *
 *  BOOL Is_True (
 *      BOOL condition,
 *      ( const char *Format_String, ... )
 *  )
 *
 *	Similar to Assert, but performs a check expected to be removed (using
 *      cpp) in production code.  Also, notice that a format string is given
 *      instead of an error code.  Because these checks will be removed from
 *      producetion code, we decided that consistency of error reporting was
 *      less important than ease of use (in this case).
 *
 *  void FmtAssert (
 *      BOOL condition,
 *      ( const char *Format_String, ... )
 *  )
 *
 *      Just like Is_True, except will check in the production compiler as
 *      well as during development.
 *
 *  void DevAssert (
 *      BOOL condition,
 *      ( const char *Format_String, ... )
 *  )
 *
 * 	DevAssert is the same as IsTrue. Please use that instead.
 *
 * WARNING: The parameter lists for the above are currently are limited to 6
 * parameters after the error code or format string.
 *
 *  void DevWarn ( const char *Format_String, ... )
 *
 *      Issue a warning to stderr only when executed in a compiler built for
 *      development (not in MR'ed compilers).  This is a good thing to use if
 *      you have an unexpected condition from which there is a valid (though
 *      perhaps suboptimal) recovery.  For example:
 *
 *          if ( Unexpected_Condition_That_Precludes_Optimization() ) {
 *              DevWarn("Can't optimize because I was a hopeless loser");
 *              Generate_Code();
 *          }
 *          else {
 *              Optimize();
 *              Generate_Code();
 *          }
 *
 *      The warnings are issued by default if the errors package is built with
 *      horrible Is_True_On; otherwise, they are not issued.  This behavior can
 *      be changed with:
 *
 *  void DevWarn_Toggle ( void )
 *
 *      If the current behavior of DevWarn is to print the warnings, calling
 *      this supresses the warnings.  If the current behavior is to supress
 *      the warnings, calling this will enable printing them.
 *
 *  BOOL DevWarn_Enable ( void )
 *
 *      If the current behavior of DevWarn is to print the warnings, this function
 *      will return true, and fals else.
 *
 *  Lmt_DevWarn(const UINT limit, args)
 *
 *      This is a macro designed to suppress printing of DevWarn
 *      messages that get repeated a lot. At most limit copies of the
 *      DevWarn message will be printed; args is the parenthesized
 *      argument list for the function DevWarn(). In the present
 *      implementation, the position (file, line) in the compiler
 *      source is presumed to identify the message. For example, if
 *      you are a hopeless loser hundreds of times per compilation,
 *      and you feel a need to tolerate this situation for a time, and
 *      you don't wish to see hundreds of copies of the "hopeless
 *      loser" DevWarn message every time you compile, you might use:
 *
 *          if ( Unexpected_Condition_That_Precludes_Optimization() ) {
 *              Lmt_DevWarn(5,
 *                          "Can't optimize because I was a hopeless loser");
 *              Generate_Code();
 *          }
 *          else {
 *              Optimize();
 *              Generate_Code();
 *          }
 *
 *      In this example, you will see the "hopeless loser" DevWarn
 *      message at most five times per compilation.
 *
 *
 * Note that a number of predefined error codes are available,
 * including one which allows arbitrary formatting.  See erglob.h.
 *
 * The parameters passed to the error reporting routines must match the
 * message descriptor for the error code; they are used to provide
 * values inserted in the message.  The parameters are passed in a form
 * which allows them to be passed further without type knowledge, with
 * only the assumption that standard integers and pointers are the same
 * size.  The following possibilities are currently supported:
 *
 *  ET_INT:	Generic scaled integer: size of host register (INTSC).
 *  ET_INT32:	32-bit integer.
 *  ET_INT64:	64-bit integer.
 *  ET_FLOAT:	Pointer to a standard float number.
 *  ET_DOUBLE:	Pointer to a standard double float number.
 *  ET_POINTER:	Arbitrary pointer, printed as a hex number.
 *  ET_STRING:	Pointer to a standard C string.
 *  ET_SYSERR:	Unix error number.
 *  ET_STRTAB:	Pointer to a compiler string table entry.
 *  ET_SYMTAB:	Pointer to a compiler symbol table entry.
 *  ET_TN:	Pointer to a compiler temporary name struct.
 *  ET_NODE:	Pointer to a compiler tree node, representing its name.
 *
 * ====================================================================
 *
 * Error Code Definition
 *
 * In order to define a new error code for use, its value must be
 * defined in one of the error code header files erXXX.h, and a message
 * descriptor added to the descriptor file erXXX.desc.  For the
 * compiler, the possibilities for these files are:
 *
 *   erglob:	Global error codes, common to both compiler phases or
 *		other tools.
 *   erfe:	Front end error codes.
 *   ercg:	Code generator error codes.
 *   erlib:	Program library error codes.
 *   erlink:	Linker/object file error codes.
 *
 * The error message descriptors provide a printf format for the
 * message and parameter types from the above list.  They also specify
 * a severity level, one of:
 *
 *   ES_IGNORE:		To be ignored by the error message generator.
 *   ES_ADVISORY:	For user information only; not usually an
 *			indication of a problem.
 *   ES_WARNING:	A likely problem, but not a definite error.
 *   ES_CONFORMANCE:	The program does not conform in some way to the
 *			source language standard (not relevant to the
 *			first implementation).
 *   ES_ERROR:		Minimum error severity level.  All errors at or
 *			above this level prevent successful completion.
 *   ES_ERRBENIGN:	Benign error: processing continues except for
 *			code emission.
 *   ES_ERRPHASE:	Error: stop processing this source file after
 *			the current compiler phase is complete, but
 *			process any other source files before quitting.
 *   ES_ERRABORT:	Error: abort processing after minimal cleanup.
 *
 * The descriptor also identifies user vs. compiler errors.
 * See the discussion in erglob.desc for more detail.
 *
 * ====================================================================
 *
 * Error Reporting Management
 *
 * A number of global parameters must be maintained to support the
 * model assumed above.
 *
 * Error database initialization should occur very early in any process
 * that will use the facilities.  This basically points the error
 * library at the process' error table structures.
 *
 *   Set_Error_Tables(
 *     ERROR_DESC_TABLE *error_descriptor_table,
 *     char *error_list )
 *
 *  To set the descript of a particular error phase:
 *
 *   Set_Error_Descriptor (
 *     INT phase,
 *     ERROR_DESC *descriptor)
 *
 * Errors will always be reported to stderr.  In addition, they should
 * be reported to a default error file where they will be preserved
 * beyond the lifetime of a screen image.  The following routine
 * defines the file which serves this purpose, and should be called
 * when initializing a new source file for processing:
 *
 *   Set_Error_File ( const char *filename );
 *
 * Note that this routine has the side effects of closing any error
 * file which is currently open, and removing any existing file of the
 * given name.  However, the new error file is not created until a
 * reportable message occurs.  Error message emission to the trace
 * file is disabled by setting the file descriptor to NULL.
 *
 * In addition, if tracing is enabled, it is useful to have a copy of
 * all error messages embedded in the trace file.  This is enabled by
 * the call:
 *
 *   Set_Error_Trace ( FILE *stream );
 *
 * The trace file is assumed to have been opened elsewhere before any
 * error message is reported.  Note that, if tracing is directed to
 * the same file as stderr (e.g. the terminal), the extra copy will
 * be suppressed.  Error message emission to the trace file is
 * disabled by setting the file descriptor to NULL.
 *
 * All errors are reported with a source file name and line number.
 * They are maintained by calling the following routines:
 *
 *   Set_Error_Source ( char *filename );
 *   Set_Error_Line   ( INT linenum );
 *   Set_Error_Srcpos ( SRCPOS srcpos );
 *
 * To suppress line number reporting, e.g. during phases where errors
 * cannot be attributed to particular source lines, set the current
 * line to ERROR_LINE_UNKNOWN.  Note that ErrMsgLine uses the line
 * number passed by the caller instead of the current line number.
 *
 * Compiler errors (as opposed to user errors) are reported with the
 * compiler phase in which they occur.  To support this, the following
 * routine should be called at the beginning of each phase:
 *
 *   Set_Error_Phase ( const char *phasename );
 *
 * To save the current error phase for temporary changes, use:
 *
 *   char_variable = Get_Error_Phase ();
 *
 * Two global variables control the reporting of errors based on their
 * severity level:
 *
 *  Min_Error_Severity:	The minimum severity level errors to be
 *			reported (ES_WARNING by default).
 *  Conformance_Level:	The severity level of conformance errors
 *			(ES_ADVISORY by default; may be set anywhere
 *			between ES_IGNORE and ES_ERROR to control
 *			treatment).
 *
 * In order to determine whether compilation-terminating errors have
 * been detected, and in order to get an error count to report, the
 * following routine is used:
 *
 *   BOOL Get_Error_Count ( INT *Error_Count, INT *Warning_Count );
 *
 * This routine returns the number of errors and warnings up to this
 * point in its two parameters.  If any errors of level ES_ERRPHASE or
 * above have been seen, it returns TRUE, and compilation of the
 * current source file should terminate.
 *
 * Prior to other processing, and between source files, the error
 * handler should be initialized by the following routine, which
 * clears error counts, closes open error files, etc.
 *
 *   Init_Error_Handler ( INT Max_Allowed_Errors );
 *
 * Finally, as early as possible during processing, signal catching
 * with associated cleanup may be enabled by calling:
 *
 *   Handle_Signals ();
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * Internal Assertion Checking: These routines are invoked via macros
 * defined below, and should not be called directly, as their
 * interfaces are allowed to change.
 *
 * ====================================================================
 */

#ifdef MONGOOSE_BE
#ifndef srcpos_INCLUDED
#include "srcpos.h"
#endif
#endif

extern void Abort_Compiler_Location (
  const char* file_name,
  INT line_number
);
#pragma mips_frequency_hint NEVER Abort_Compiler_Location

/* Simple abort report with error code: */
extern void Fail_Assertion ( INT ecode, ... );
#pragma mips_frequency_hint NEVER Fail_Assertion

/* Simple abort report with format instead of code: */
/*PRINTFLIKE1*/
extern void Fail_FmtAssertion ( const char *fmt, ... );
#pragma mips_frequency_hint NEVER Fail_FmtAssertion

/* Simple fatal error report with format instead of code: */
/*PRINTFLIKE1*/
extern void Fatal_Error ( const char *fmt, ... );
#pragma mips_frequency_hint NEVER Fatal_Error

/* ====================================================================
 *
 * Error Reporting Interface
 *
 * ====================================================================
 */

/* Simple error report: */
extern void ErrMsg ( INT ErrCode, ... );
#pragma mips_frequency_hint NEVER ErrMsg

/* Error report with specified source line number: */
extern void ErrMsgLine ( INT ErrCode, INT LineNo, ... );
#pragma mips_frequency_hint NEVER ErrMsgLine

/* Error report with specified source position: */
#ifdef MONGOOSE_BE
extern void ErrMsgSrcpos ( INT ErrCode, SRCPOS SrcPos, ... );
#pragma mips_frequency_hint NEVER ErrMsgSrcpos
#endif

/* Unconditional assertion checking with error code: */
#define Assert(Cond,ParmList)					\
    ( Cond ? (void) 1						\
           : ( Abort_Compiler_Location ( __FILE__, __LINE__ ),	\
	       Fail_Assertion ParmList ) )

/* Unconditional assertion checking with printf format, always fatal: */
#define FmtAssert(Cond,ParmList)				\
    ( Cond ? (void) 1						\
           : ( Abort_Compiler_Location ( __FILE__, __LINE__ ),	\
	       Fail_FmtAssertion ParmList) )

/* Still fatal checking with printf format, but more polite: */
#define Require(Cond,ParmList)				\
    ( Cond ? (void) 1						\
           : ( Abort_Compiler_Location ( __FILE__, __LINE__ ),	\
	       Fatal_Error ParmList) )


/* Check assertion only if Insist_On flag is set: */
#ifdef Insist_On
# define Insist FmtAssert
#else
# define Insist(a, b) ((void) 1)
#endif

/* Check assertion only if Is_True_On flag is set: */
#ifdef Is_True_On
# define Is_True FmtAssert
#else
# define Is_True(a, b) ((void) 1)
#endif

/*PRINTFLIKE1*/
extern void DevWarn( const char* FormatString,... )
#ifdef __GNUC__
	__attribute__((format(printf,1,2)))
#endif
	;
#pragma mips_frequency_hint NEVER DevWarn

extern BOOL DevWarn_Enabled( void );

extern void DevWarn_Toggle( void );
#pragma mips_frequency_hint NEVER DevWarn_Toggle

extern BOOL Count_Limit_DevWarn( const char *const src_fname,
				 const UINT        src_line,
				 const UINT        limit );
#pragma mips_frequency_hint NEVER Count_Limit_DevWarn

#define Lmt_DevWarn(limit, args) \
	( Count_Limit_DevWarn(__FILE__, __LINE__, limit) ? \
	  DevWarn args : \
	  (void) 1 )


/* DevAssert is just a synonym for Is_True. It's use is deprecated.
 * Please use Is_True instead.
 */
#define DevAssert Is_True
#define DevAssert_On Is_True_On



/* ====================================================================
 *
 * Error Reporting Management
 *
 * ====================================================================
 */

/* Define the severity levels: */
#define	ES_IGNORE	0	/* Ignore completely */
#define ES_ADVISORY	1	/* Advisory only */
#define ES_WARNING	2	/* Warning - potential problem */
#define ES_CONFORMANCE	3	/* May not conform to standard */
#define ES_ERROR	4	/* Minimum error level */
#define ES_ERRBENIGN	4	/* Error: finish processing */
#define ES_ERRPHASE	5	/* Error: finish phase, other files */
#define ES_ERRABORT	6	/* Error: terminate immediately */
#define ES_MAX		6	/* Maximum severity */

/* Predefined phase numbers.  These are more coarse than the phases
 * named by calls to Set_Error_Phase, and represent the collections
 * of error codes into header files.  All phases in use in a program
 * should be defined either here or in err_host.h, to avoid conflicts.
 * The symbol EP_LAST should also be defined in err_host.h.
 */
#ifndef MONGOOSE_BE
#define EP_UNIX		0	/* Unix error codes */
#endif /* MONGOOSE_BE */

#define EP_GLOBAL	1	/* Global, general-purpose codes */
#define EP_LIB		2	/* Program librarian codes */

#ifndef MONGOOSE_BE
#define EP_LINK		3	/* Linker, object file codes */
/* The following are compiler-specific, but predefined because they
 * are common to all phases:
 */
#define EP_FE		4	/* Compiler front end codes */
#endif /* MONGOOSE_BE */

#define EP_BE		5	/* Compiler back end codes (not CG) */
#define EP_CG		6	/* Code generator codes */
#define EP_PREDEFINED	6	/* Last predefined phase */

/* Predefined error parameter types: */
#define ET_UNKNOWN	0	/* Mistake */
#define ET_INT		1	/* Scaled integer */
#define ET_INT32	2	/* 32-bit integer */
#define ET_INT64	3	/* 64-bit integer */
#define ET_FLOAT	4	/* Pointer to float */
#define ET_DOUBLE	5	/* Pointer to double float */
#define ET_POINTER	6	/* Arbitrary pointer */
#define ET_STRING	7	/* Standard character string */
#define ET_SYSERR	8	/* Unix error code */
/* The following are predefined, but compiler specific: */
#define ET_STRTAB	9	/* Pointer to string table */
#define ET_SYMTAB	10	/* Pointer to symbol table */
#define ET_TN		11	/* Pointer to TN */
#define ET_NODE		12	/* Pointer to tree node */
#define ET_PREDEFINED	13	/* Last predefined kind */

/* Include definitions specific to the host program: */
#include "err_host.h"

/* The following contains Unix error code after system call errors: */
#ifndef KEY
extern INT errno;
#else
#include <errno.h>
#endif

/* Control reporting by severity level: */
extern INT Min_Error_Severity;
extern INT Conformance_Level;

/* Error table initialization.  All users of this package must call
 * this routine before emitting any error messages.
 */
/* Incomplete type to keep ANSI happy: */
struct error_desc_table;
extern void Set_Error_Tables(
  struct error_desc_table *edt,
  const char *errlist[] );

extern void
Set_Error_Descriptor (INT, ERROR_DESC *);

/* Control files to report errors to: */
extern void Set_Error_File (
    const char *filename
);
extern void Set_Error_Trace (
    FILE *stream
);

/* Notify error reporter of current source file name: */
extern void Set_Error_Source (
    const char *filename
);

/* Notify error reporter of current source file line number: */
#define ERROR_LINE_UNKNOWN	0
extern void Set_Error_Line (
    INT LineNo
);

/* Notify error reporter of current source position: */
#ifdef MONGOOSE_BE
extern void Set_Error_Srcpos (
    SRCPOS SrcPos
);
#endif

/* Notify error reporter of current compiler phase: */
extern void Set_Error_Phase (
    const char *phasename
);
extern const char *Get_Error_Phase (void);

/* Determine whether compilation should terminate: */
extern BOOL Get_Error_Count (	/* Return whether to stop now */
    INT *Error_Count,	/* Number of errors to this point */
    INT *Warning_Count	/* Number of warnings to this point */
);

/* Initialize the error handler: */
extern void Init_Error_Handler ( INT Max_Allowed_Errors );

/* Initialize signal catching: */
extern void Handle_Signals ( void );

/* ====================================================================
 *
 * Miscellaneous
 *
 * ====================================================================
 */

/* Ignore bad/illegal values: */
extern void Rag_Handle_Woff_Args(char	*wstring);

/* had any internal errors */
extern BOOL Had_Internal_Error (void);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

class Temporary_Error_Phase
{
private:
  const char* saved_error_phase;

public:
  Temporary_Error_Phase(const char* new_error_phase) {
    saved_error_phase = Get_Error_Phase();
    Set_Error_Phase(new_error_phase);
  }

  ~Temporary_Error_Phase() {
    if (saved_error_phase)
      Set_Error_Phase(saved_error_phase);
  }
};

#endif /* __cplusplus */

#endif /* ERRORS_INCLUDED */
