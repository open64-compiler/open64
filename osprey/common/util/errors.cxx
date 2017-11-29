/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: errors.c
 *
 * Revision history:
 *  05-Sep-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *  26-May-91 - Integrated Josie functionality
 *  13-Nov-92 - Integrated Josie/92 functionality
 *
 * Description:
 *
 * This module implements the core of the microcode compiler error
 * reporting mechanism, i.e. that part which is not specific to the
 * host program.  Host program-specific portions are done elsewhere to
 * allow reuse of this module in the linker and librarian (or other
 * as-yet-unknown tools).
 *
 * The host program-specific facilities should be provided in a file
 * satisfying the interface defined in err_host.h.  They will include
 * the actual phase and error descriptor table and a routine for
 * formatting error message parameters which are not one of the simple
 * types supported in this module.  This module should provide the
 * external interface to all other users, however.
 *
 * ====================================================================
 * ====================================================================
 */

#define USE_STANDARD_TYPES
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#if (__GNUC__==2)
extern "C" char *strsignal (int __sig);
#endif
#include <ctype.h>

#if defined(IRIX) 
#define _LANGUAGE_C			/* work around system header bug */
extern "C" {
#include <sys/fpu.h>			/* we don't have a C++ sys/fpu.h */
}
#undef _LANGUAGE_C
#include <sys/syssgi.h>
#endif

#if defined(KEY) && defined(linux)
#include <execinfo.h>
#endif

#include <errno.h>
#include <cmplrs/rcodes.h>
#include "defs.h"
#ifdef AWAITING_NEWCOMP /* needed? */
#include "config.h"
#endif /* AWAITING_NEWCOMP */
#define IN_ERRORS_C
#include "errors.h"
#undef IN_ERRORS_C
#include "err_host.h"
#include "erglob.h"
#include "file_util.h"
#include "tracing.h"
#include "glob.h"
#include "errdesc.h"
#include "vstring.h"

#ifdef MONGOOSE_BE
#include "wn.h"
#include "wn_map.h"
#include "ir_reader.h"
#endif


/* ====================================================================
 *
 * Global Data
 *
 * ====================================================================
 */

INT Min_Error_Severity = ES_ADVISORY;	/* report advisory msgs or worse */
INT Conformance_Level  = ES_IGNORE;	/* ignore conformance error */

/* Compiler error location: */
static const char *Compiler_File = NULL;/* errors.c internal use only */
static INT   Compiler_Line = 0;		/* errors.c internal use only */

/* Error file support: */
static FILE       *Error_File = NULL;		/* File descriptor */
static const char *Error_File_Name = NULL;	/* Error file name */

/* Trace file support: */
static FILE *Trace_File = NULL;		/* File descriptor */

/* Source file location: */
#ifdef KEY /* Bug 4469 */
static char  source_file_name[FILENAME_MAX + 1];
#else
static char  source_file_name[256];
#endif /* KEY Bug 4469 */
static char *Source_File_Name = &source_file_name[0];
static INT   Source_Line = ERROR_LINE_UNKNOWN;	/* Line number */

/* Current compiler/tool phase: */
static const char *Current_Phase = "<unknown phase>";

/* Error counts: */
static INT   Error_Counts[ES_MAX+1];
static INT   Error_Count = 0;		/* Number of all errors */
static BOOL  Phase_Error = FALSE;	/* ES_ERRPHASE or worse seen */
static INT   Max_Errors = 100;		/* Maximum errors allowed */

static BOOL Had_Compiler_Error = FALSE;

/* ====================================================================
 *
 * Severity Description Table
 *
 * ====================================================================
 */

/* Severity descriptors: */
typedef struct {
    INT  level;		/* Severity level */
    char symbol[5];	/* Symbol used to prefix messages */
    const char *name;	/* Name used in message headers */
} SEVERITY_DESCRIPTOR;

static SEVERITY_DESCRIPTOR Severities[] = {
    {ES_IGNORE,		"??? ",	"Ignore"},
    {ES_ADVISORY,	"--- ", "Advisory"},
    {ES_WARNING,	"!!! ", "Warning"},
    {ES_CONFORMANCE,	"!!! ", "Conformance warning"},
    {ES_ERRBENIGN,	"### ", "Error"},
    {ES_ERRPHASE,	"### ", "Error"},
    {ES_ERRABORT,	"### ", "Error"}
};

/* Access functions: */
#define SEV_level(n)	(Severities[n].level)
#define SEV_symbol(n)	(Severities[n].symbol)
#define SEV_name(n)	(Severities[n].name)

/* ====================================================================
 *
 * Error message descriptors
 *
 * ====================================================================
 */

#if defined(MONGOOSE_BE) && defined(SHARED_BUILD)
#include "err_host.tab"		    /* for error tables */
#else /* MONGOOSE_BE */
/* these table pointers get pointed at the client's tables */
static ERROR_DESC_TABLE *Phases = NULL;
static const char **host_errlist = NULL;
#endif /* MONGOOSE_BE */

static bool do_traceback = false;

/* The access functions: */
#define Phase_Num(n)	(Phases[n].phase)
#define Phase_List(n)	(Phases[n].descriptors)
#define Phase_Name(n)	(Phases[n].name)

#if !defined(linux) && !defined(__APPLE__) && !defined(BUILD_OS_DARWIN)
extern char *sys_siglist[];
#endif

/* WOFF handling */
static char dont_print[RAG_EN_LAST-RAG_EN_FIRST+1];
#define Dont_Print_Warning(i)	dont_print[i-RAG_EN_FIRST]

#define Heed_Woff(rag_errnum, severity) \
	(Dont_Print_Warning(rag_errnum) && (severity < ES_ERRBENIGN) )

extern void Rag_Handle_Woff_Args(char	*wstring);

static void dump_backtrace(FILE *fp = stderr, size_t start_frame = 1)
{
#if defined(KEY) && defined(linux) /* "backtrace" is unique to GNU/Linux libc */
    const int nframes = 32;
    void *buf[nframes];
    char **strings;
    size_t size;

    size = backtrace(buf, nframes);
    strings = backtrace_symbols(buf, size);

    fprintf(fp, "*** Internal stack backtrace:\n");

    for (size_t i = start_frame; i < size; i++) {
	fprintf(fp, "    %s\n", strings[i]);
    }
    fflush(fp);
#endif
}


/* ====================================================================
 *
 * Catch_Signal
 *
 * Catch a signal that's been raised.  Print a message to stdout, call
 * the Signal_Cleanup routine defined in err_host.h, and generate a
 * fatal error message.
 *
 * ====================================================================
 */

static void
catch_signal (INT sig, INT error_num)
{
    signal ( sig, SIG_DFL );

    switch (sig) {
#ifndef __MINGW32__
    case SIGBUS:
#endif /* __MINGW32__ */
    case SIGSEGV:
	if (error_num == ENXIO || error_num == ENOSPC)
	    /* special case for I/O error on mmapped object: report as an
	       ordinary fatal error */ 
	    Fatal_Error ("I/O error in mmapped object: %s",
			 strerror(error_num));
    }
    
#ifdef __MINGW32__
    fprintf (stderr,  "Signal: %s", "caught" );
#else /* __MINGW32__ */
    fprintf (stderr,  "Signal: %s", strsignal(sig) );
#endif /* __MINGW32__ */
    fflush ( stderr );
    fprintf ( stderr, " in %s phase.\n",
	      Current_Phase ? Current_Phase : "startup" );
    
    do_traceback = true;
    
    Signal_Cleanup ( sig );	    /* Must be provided in err_host */
#ifdef __MINGW32__
    if ( sig == SIGINT || sig == SIGTERM ) {
	raise ( sig);	/* pass signal on to driver */
    	/*NOTREACHED*/
	exit(RC_INTERNAL_ERROR);
    }
#else
    if ( sig == SIGHUP ||  sig == SIGINT || sig == SIGTERM ) {
	kill ( getpid(), sig);	/* pass signal on to driver */
    	/*NOTREACHED*/
	exit(RC_INTERNAL_ERROR);
    }
#endif /* __MINGW32__ */
    signal ( SIGILL, SIG_DFL );
#ifdef __MINGW32__
    ErrMsgLine ( EC_Signal, ERROR_LINE_UNKNOWN,
		"caught", Current_Phase );
#else
    signal ( SIGBUS, SIG_DFL );
    ErrMsgLine ( EC_Signal, ERROR_LINE_UNKNOWN,
		strsignal(sig), Current_Phase );
#endif /* __MINGW32__ */
    /*NOTREACHED*/
    exit(RC_INTERNAL_ERROR);
}


/* ====================================================================
 *
 * Handle_Signals
 *
 * Initialize signal handling.
 * Also set FP_PRECISE mode and no FLUSH_ZERO to handle denorm values.
 *
 * ====================================================================
 */

inline static void
setup_signal_handler (int s)
{
    if (signal (s, SIG_IGN) != SIG_IGN)
	signal (s,  reinterpret_cast <void (*)(int)> (catch_signal));
}


void
Handle_Signals ( void )
{
#ifndef __MINGW32__
    setup_signal_handler (SIGHUP);
    setup_signal_handler (SIGQUIT);
    setup_signal_handler (SIGTRAP);
    setup_signal_handler (SIGBUS);
#endif /* __MINGW32__ */
    setup_signal_handler (SIGINT);
    setup_signal_handler (SIGILL);
    setup_signal_handler (SIGIOT);
    setup_signal_handler (SIGABRT);
#if defined(IRIX)
    setup_signal_handler (SIGEMT);
#endif
    setup_signal_handler (SIGFPE);
    setup_signal_handler (SIGSEGV);
    setup_signal_handler (SIGTERM);
#if defined(IRIX)
    syssgi(SGI_SET_FP_PRECISE, 1);
    set_fpc_csr(get_fpc_csr() & ~FPCSR_FLUSH_ZERO);
    syssgi(SGI_SET_FP_PRESERVE, 1);
#endif
}

/* ====================================================================
 *
 * Set_Error_File
 *
 * Prepare to write error messages to the given file, using the given
 * file number.  If there is an open error file, close it.  If the
 * given file exists, delete it.  Note that the error file is not
 * opened here.
 *
 * ====================================================================
 */

void
Set_Error_File ( const char *fname )
{
  /* Close any open error file: */
  if ( Error_File ) {
    fclose ( Error_File );
    Error_File = NULL;	/* So we don't try to write messages to it */
  }

  /* Delete the indicated file if it exists: */
  Error_File_Name = fname;
  if ( Is_File(Error_File_Name) ) {
    unlink ( Error_File_Name );
  }
}


/* ====================================================================
 *
 * Init_Error_File
 *
 * Prepare the error file for writing, i.e. open it if it's not open.
 * Return TRUE iff successful.
 *
 * ====================================================================
 */

static BOOL
Init_Error_File (void)
{
  if ( Error_File != NULL ) return TRUE;	/* Already open */
  if ( Error_File_Name != NULL ) {
    Error_File = fopen ( Error_File_Name, "a" );
    if ( Error_File == NULL ) return FALSE;
    if ( Same_File (Error_File, stderr) ) {
      fclose ( Error_File );
      Error_File = NULL;
      Error_File_Name = NULL;
      return FALSE;
    }
    return TRUE;
  }
  return FALSE;		/* No error file specified */
}

/* ====================================================================
 *
 * Set_Error_Trace
 *
 * Prepare to write error messages to the trace file, using the given
 * stream descriptor.  If the trace file is the same as stderr,
 * suppress error output via the trace file.
 *
 * ====================================================================
 */

void
Set_Error_Trace ( FILE *stream )
{
  if ( Same_File (stream, stderr) )
    Trace_File = NULL;
  else
    Trace_File = stream;
}

/* ====================================================================
 *
 * Set_Error_Source
 *
 * Note the current source file name for subsequent error messages.
 *
 * ====================================================================
 */

void
Set_Error_Source ( const char *filename )
{
  if ( filename == NULL ) {
    Source_File_Name = NULL;
  } else {
    Source_File_Name = &source_file_name[0];
#ifdef KEY /* Bug 4469 */
    strncpy ( Source_File_Name, filename, sizeof source_file_name );
#else
    strcpy ( Source_File_Name, filename );
#endif /* KEY Bug 4469 */
  }
}


#ifdef MONGOOSE_BE
/* ====================================================================
 *
 * Set_Error_Srcpos
 *
 * Set Source_Line and Source_File from SRCPOS
 *
 * ====================================================================
 */

void
Set_Error_Srcpos ( SRCPOS srcpos )
{
  const char *fname = NULL;
  const char *dname;

  Set_Error_Line(Srcpos_To_Line(srcpos));
  IR_Srcpos_Filename(srcpos, &fname, &dname);
  Set_Error_Source(fname);
}
#endif


/* ====================================================================
 *
 * Set_Error_Line
 *
 * Note the current source line number for subsequent error messages.
 *
 * ====================================================================
 */

void
Set_Error_Line ( INT lineno )
{
  Source_Line = lineno;
}


/* ====================================================================
 *
 * Set_Error_Phase / Get_Error_Phase
 *
 * Note the current phase name for subsequent compiler error messages.
 *
 * ====================================================================
 */

void
Set_Error_Phase ( const char *phase )
{
    Current_Phase = phase;
#ifdef BACK_END
    if (Get_Trace(TP_MISC, 4)) {
	fprintf(stderr, "Entering PU %s phase %s\n",
		Cur_PU_Name ? Cur_PU_Name : "(None)", phase);
	fprintf(TFile, "Entering PU %s phase %s\n",
		Cur_PU_Name ? Cur_PU_Name : "(None)", phase);
    }	    
#endif
    switch (phase[0]) {
    case 'A':
	if (strcmp(phase, "Alias Analysis") == 0)
	    Set_Current_Phase_Number(TP_ALIAS);
	else if (strcmp(phase, "Assembly") == 0)
	    Set_Current_Phase_Number(TP_EMIT);
	break;

    case 'C':
	if (strcmp(phase, "Code Generation") == 0)
	    Set_Current_Phase_Number(TP_CGEXP);
	break;

    case 'H':
	if (strcmp(phase, "Hyperblock formation") == 0)
	  Set_Current_Phase_Number(TP_HBF);
	break;
    case 'G':
	if (strcmp(phase, "Global Live Range Analysis") == 0)
	    Set_Current_Phase_Number(TP_FIND_GLOB);
	else if (strcmp(phase, "Global Code Motion") == 0)
	    Set_Current_Phase_Number(TP_GCM);
	break;

    case 'O':
	if (strcmp(phase, "Optimizer") == 0)
	    Set_Current_Phase_Number(TP_WOPT1);
	break;

    case 'P':
	if (strcmp(phase, "Pre-Optimizer") == 0)
	    Set_Current_Phase_Number(TP_GLOBOPT);
	break;

    case 'R':
	if (strcmp(phase, "Reading IR file") == 0)
	    Set_Current_Phase_Number(TP_IR_READ);
	else if (strcmp(phase, "Reorder Blocks") == 0)
	    Set_Current_Phase_Number(TP_FLOWOPT);
	else if (strcmp(phase, "Register Allocation") == 0)
	    Set_Current_Phase_Number(TP_GRA);
	break;

    case 'S':
	if (strcmp(phase, "Software Pipelining") == 0)
	    Set_Current_Phase_Number(TP_SWPIPE);
	break;
    case 'T':
	if (strcmp(phase, "Tree-Height Reduction") == 0)
	  Set_Current_Phase_Number(TP_THR);
	break;
    }
   
}

const char *
Get_Error_Phase ( void )
{
  return Current_Phase;
}

/* ====================================================================
 *
 * Get_Error_Count
 *
 * Determine the number of errors and warnings reported to this point
 * (since the last initialization).  Return whether compilation should
 * stop after this phase.
 *
 * ====================================================================
 */

BOOL
Get_Error_Count ( INT *ErrCount, INT *WarnCount )
{
  *ErrCount = Error_Count;
  *WarnCount = Error_Counts[ES_WARNING];
  return Phase_Error;
}


/* ====================================================================
 *
 * Init_Error_Handler
 *
 * Initialize the error message handler's internal data structures,
 * either at the beginning of compilation, or between source files.
 *
 * ====================================================================
 */

void
Init_Error_Handler ( INT Max_Errors_Allowed )
{
  INT i;

  /* Initialize error counts: */
  Max_Errors = Max_Errors_Allowed;
  Phase_Error = FALSE;
  Error_Count = 0;
  for ( i=0; i<=ES_MAX; i++ ) {
    Error_Counts[i] = 0;
  }

  /* Initialize error file state: */
  if ( Error_File ) {
    fclose ( Error_File );
  }
  Error_File = NULL;
  Error_File_Name = NULL;

  /* Initialize current position: */
  Source_File_Name = NULL;
  Source_Line = ERROR_LINE_UNKNOWN;
  Current_Phase = "<unknown phase>";
}

/* ====================================================================
 *
 * Find_Error_Desc
 *
 * Description
 *
 * ====================================================================
 */

static ERROR_DESC *
Find_Error_Desc ( INT ecode )
{
  INT phase = ecode/1000;
  INT i;
  ERROR_DESC *edesc;

  /* Find the right list, i.e. from the matching phase: */
  for ( i=0; Phase_Num(i) != -1; i++ ) {
    if ( Phase_Num(i) == phase ) {
      /* This is the right list -- find the error code: */
      for ( edesc = Phase_List(i); ED_code(edesc) != -1; edesc++ ) {
	if ( ED_code(edesc) == ecode ) return edesc;
      }
      break;	/* Not found */
    }
  }

  /* We didn't find it -- return the global unknown error code: */
  return Find_Error_Desc ( EC_Undef_Code );
}


static FILE *Crash_File;

static BOOL
Init_Crash_Report (void)
{
  if (Crash_File != NULL)
    return TRUE;

#if defined(VENDOR_OSP)  
  char *name = getenv("OPEN64_CRASH_REPORT");
#elif defined(VENDOR_SL)
  char *name = getenv("SL_CRASH_REPORT");
#else
  char *name = getenv("PSC_CRASH_REPORT");
#endif

  if (name == NULL)
    return FALSE;

  if ((Crash_File = fopen(name, "a")) == NULL)
    return FALSE;

  return TRUE;
}


/* ====================================================================
 *
 * Emit_Message
 *
 * Write a two-line message to stderr and to the error and trace files
 * if enabled.  If Compiler_File is not NULL, report the compiler
 * location as well.
 *
 * ====================================================================
 */

static void
Emit ( FILE *File,
       char *msg,
       char *hmsg,
       char *emsg,
       BOOL report_location )
{
  if ( report_location ) {
    fputs ( msg, File );
  }
  fputs ( hmsg, File );
  fputs ( emsg, File );
  fflush ( File );
  if ( do_traceback ) {
    dump_backtrace ( File );
  }
}

static void
Emit_Message (
  char *hmsg,		/* Header line of message */
  char *emsg )		/* Error line of message */
{
  char msg[1024];
  BOOL report_location = FALSE;

  /* Report the assertion failure location: */
  if ( Compiler_File != NULL ) {
    sprintf ( msg, "\n### Assertion failure at line %d of %s:\n",
	      Compiler_Line, Compiler_File );
    report_location = TRUE;
  }

  /* Write to standard error first: */
  Emit ( stderr, msg, hmsg, emsg, report_location );
  
  /* Then dump to crash report file if enabled: */
  if ( Init_Crash_Report() ) {
    Emit ( Crash_File, msg, hmsg, emsg, report_location );
  }
  
  if ( Compiler_File != NULL ) {
    Compiler_File = NULL;
  }
  
  /* Then write to error file if enabled: */
  if ( Init_Error_File() ) {
    Emit ( Error_File, msg, hmsg, emsg, report_location );
  }

  /* Finally write to trace file: */
  if ( Trace_File != NULL ) {
    Emit ( Trace_File, msg, hmsg, emsg, report_location );
  }

  if ( do_traceback )
    do_traceback = false;
}

/* ====================================================================
 *
 * ErrMsg_Report
 *
 * Report an error at the given line number.
 *
 * ====================================================================
 */

static void
ErrMsg_Report_Nonuser ( ERROR_DESC *edesc, INT ecode, INT line,
#ifdef TARG_NVISA
/* NVIDIA-specific: The format of error and warning messages below has been
   changed so that they are parsable by the Visual Studio IDE (and, presumably,
   Emacs).  We now print the full path of the offending file, immediately
   followed by a line number in parentheses.
*/
                        const char *file, const char *dname, va_list vp )
#else
                        const char *file, va_list vp )
#endif
{
  INT dlevel = ED_severity(edesc);	/* Declared severity */
  INT mlevel = dlevel;			/* Mapped severity */
# define BUFLEN 1024
# define BUFLEN_NONUSER 1024
  char hmsg[BUFLEN];
  vstring emsg;
  INTPS mparm[MAX_ERR_PARMS];

  /* Formatting buffer: */
  INT loc = 0;
  static char buf[BUFLEN_NONUSER];
  const char *result;
  INT kind;

  INT pnum;
  INTPS parm;

  /* Count error: */
  ++Error_Counts[dlevel];
  if ( dlevel >= ES_ERROR ) ++Error_Count;
  if ( dlevel >= ES_ERRPHASE) Phase_Error = TRUE;

  /* Convert conformance error severity level: */
  if ( dlevel == ES_CONFORMANCE ) mlevel = Conformance_Level; 

  /* Filter out errors with severity lower than threshhold: */
  if ( mlevel < Min_Error_Severity ) return;
  if (Heed_Woff( ED_rag_errnum(edesc) , mlevel)) return;

  /* Prepare header line: */
  if ( ! ED_continuation(edesc) ) {
#ifdef TARG_NVISA
    if (dname && *dname) {
      loc += sprintf (&hmsg[0], "%s/", dname);
    }  
    loc += sprintf ( &hmsg[loc], "%s(%d): ", file && *file? file: "<input>", line);
    loc += sprintf ( &hmsg[loc], "%s%s%s", SEV_symbol(mlevel),
		    ED_unknown(edesc) ? "Unknown Compiler " :
			(ED_compiler(edesc) ? "Compiler " : ""),
		    SEV_name(mlevel) );
#else   
    loc = sprintf ( &hmsg[0], "%s%s%s", SEV_symbol(mlevel),
		    ED_unknown(edesc) ? "Unknown Compiler " :
			(ED_compiler(edesc) ? "Compiler " : ""),
		    SEV_name(mlevel) );
    if ( line != ERROR_LINE_UNKNOWN ) {
      loc += sprintf ( &hmsg[loc], " at line %d", line );
    }
    if ( file != NULL && *file != 0 ) {
      loc += sprintf ( &hmsg[loc], " in file %s", file );
    }
#endif  /* TARG_NVISA */
#ifndef METAKAP
    if ( Cur_PU_Name != NULL ) {
      INT n = snprintf ( &hmsg[loc], 300, " (user routine '%s')", Cur_PU_Name );
      loc += MIN(300, n);
    }
    if ( ED_compiler(edesc) && Current_Phase != NULL ) {
      loc += sprintf ( &hmsg[loc], " during %s phase", Current_Phase );
    }
#endif /* METAKAP */
    sprintf ( &hmsg[loc], ":\n" );
  } else {
    hmsg[0] = 0;
  }

  /* Prepare message parameters: */
  loc = 0;
  for ( pnum = 0; pnum < MAX_ERR_PARMS; pnum++ ) {

    /* If this is the unknown error message, make ecode first parm: */
    if ( ED_unknown(edesc) ) {
      mparm[0] = (INTPS) ecode;
      break;
    }

    /* If this is beyond the required parameters, we're done: */
    if ( pnum >= ED_parms(edesc) ) break;

    /* Otherwise base processing on descriptor's type: */
    switch ( kind = ED_kind(edesc,pnum) ) {
      /* The following parameter kinds are standard and require no
       * host program-specific data structures to interpret:
       */
      case ET_UNKNOWN:	mparm[pnum] = 0;
			break;

      case ET_INT:	mparm[pnum] = (INTPS) va_arg ( vp, INTSC );
			break;

      case ET_INT32:	mparm[pnum] = (INTPS) va_arg ( vp, mINT32 );
			break;

      case ET_INT64:	/* will output as a string */
			result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%lld",
					 va_arg(vp,INT64) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_STRING:	mparm[pnum] = (INTPS) va_arg ( vp, char * );
			break;

      case ET_FLOAT:	result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%6e",
					 *(va_arg(vp,float *)) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_DOUBLE:	result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%14e",
					 *(va_arg(vp,double *)) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_POINTER:	result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%#8lX",
					 (INTPS) va_arg(vp,char *) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_SYSERR:	parm = (INTPS) va_arg(vp,int);
      			if (parm < 0) {
			  mparm[pnum] = (INTPS) host_errlist[-parm];
			} else {
			  errno = 0;
			  char *err_str = strerror(parm);
			  
			  if ( errno == 0 ) {
			    mparm[pnum] = (INTPS) err_str;
			  } else {
			    result = &buf[++loc];
			    loc += sprintf ( &buf[loc],
					     "Unix error %ld", parm );
			    mparm[pnum] = (INTPS) result;
			  }
			}
			break;

      /* The default case takes care of all host program-specific
       * parameter kinds, which must be pointers:
       */
      default:	result = Host_Format_Parm ( kind, va_arg(vp,char *) );
		/* Copy the string from the result into buffer: */
		++loc;
		strncpy ( &buf[loc], result, BUFLEN_NONUSER-loc );
		result = &buf[loc];
		loc += strlen (result);
		mparm[pnum] = (INTPS) result;
		break;
    }
    
  }

  /* Prepare main error message: */
  emsg = vstr_begin(BUFLEN);
  emsg = vstr_concat (emsg, SEV_symbol(mlevel));
  loc = vstr_sprintf (&emsg, vstr_len(emsg), ED_format(edesc), mparm[0], 
		mparm[1], mparm[2], mparm[3], mparm[4], mparm[5] );
  emsg = vstr_concat (emsg, "\n");

  /* Produce the message: */
  Emit_Message ( hmsg, vstr_str(emsg) );
  vstr_end(emsg);

  if (ED_compiler(edesc)) Had_Compiler_Error = TRUE;

  /* Abort at highest severity level: */
  if ( mlevel >= ES_ERRABORT ) {
    Signal_Cleanup( 0 );
#ifdef __MINGW32__
    if ( ecode == EC_Signal )	raise ( SIGILL );
#else /* __MINGW32__ */
    if ( ecode == EC_Signal )	kill ( getpid(), SIGILL );
#endif /* __MINGW32__ */
    exit(RC_INTERNAL_ERROR);
  }

  /* If we've seen too many errors, abort: */
  if ( Error_Count > Max_Errors ) {
    ErrMsgLine ( EC_Too_Many, ERROR_LINE_UNKNOWN, Error_Count );
    /* Won't return */
  }

  return;
}


static void
ErrMsg_Report_User (ERROR_DESC *edesc, INT ecode, INT line,
#ifdef TARG_NVISA
                    const char *file, const char *dname, va_list vp )
#else
                    const char *file, va_list vp )
#endif
{
  INT dlevel = ED_severity(edesc);	/* Declared severity */
  INT mlevel = dlevel;			/* Mapped severity */
  char hmsg[BUFLEN];
  vstring emsg;
  INTPS mparm[MAX_ERR_PARMS];

  /* Formatting buffer: */
# define BUFLEN_USER 512
  INT loc = 0;
  static char buf[BUFLEN_USER];
  const char *result;
  INT kind;

  INT pnum;
  INTPS parm;

  /* Count error: */
  ++Error_Counts[dlevel];
  if ( dlevel >= ES_ERROR ) ++Error_Count;
  if ( dlevel >= ES_ERRPHASE) Phase_Error = TRUE;

  /* Convert conformance error severity level: */
  if ( dlevel == ES_CONFORMANCE ) mlevel = Conformance_Level; 

  /* Filter out errors with severity lower than threshhold: */
  if ( mlevel < Min_Error_Severity ) return;
  if (Heed_Woff( ED_rag_errnum(edesc) , mlevel)) return;

  /* Prepare header line: */
  if ( ! ED_continuation(edesc) ) {
#ifdef TARG_NVISA
    if (dname && *dname) {
      loc += sprintf (&hmsg[0], "%s/", dname);
    }  
    loc += sprintf ( &hmsg[loc], "%s(%d): ", file && *file? file: "<input>", line);
#else    
    if ( file != NULL && *file != 0 && line != ERROR_LINE_UNKNOWN ) {
      loc = sprintf ( &hmsg[0], "\"%s\", line %d: ", file, line );
    }
    else if ( file != NULL && *file != 0 ) {
      loc = sprintf ( &hmsg[0], "\"%s\": ", file );
    }
    else if ( line != ERROR_LINE_UNKNOWN ) {
      loc = sprintf ( &hmsg[0], "line %d: ", line );
    }
    else {
      loc = 0;
    }
#endif  /* TARG_NVISA */    
    sprintf ( &hmsg[loc], "%s%s: ",
	      ED_unknown(edesc) ? "unknown compiler "
			        : (ED_compiler(edesc) ? "compiler " : ""),
	      SEV_name(mlevel) );
  } else {
    hmsg[0] = 0;
  }

  loc = 0;
  /* Prepare message parameters: */
  for ( pnum = 0; pnum < MAX_ERR_PARMS; pnum++ ) {
    /* If this is beyond the required parameters, we're done: */
    if ( pnum >= ED_parms(edesc) ) break;

    /* Otherwise base processing on descriptor's type: */
    switch ( kind = ED_kind(edesc,pnum) ) {
      /* The following parameter kinds are standard and require no
       * host program-specific data structures to interpret:
       */
      case ET_UNKNOWN:	mparm[pnum] = 0;
			break;

      case ET_INT:	mparm[pnum] = (INTPS) va_arg ( vp, INTSC );
			break;

      case ET_INT32:	mparm[pnum] = (INTPS) va_arg ( vp, mINT32 );
			break;

      case ET_INT64:	/* will output as a string */
			result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%lld",
					 va_arg(vp,INT64) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_STRING:	mparm[pnum] = (INTPS) va_arg ( vp, char * );
			break;

      case ET_FLOAT:	result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%6e",
					 *(va_arg(vp,float *)) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_DOUBLE:	result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%14e",
					 *(va_arg(vp,double *)) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_POINTER:	result = &buf[++loc];
			loc += sprintf ( &buf[loc], "%#8lX",
					 (INTPS) va_arg(vp,char *) );
			mparm[pnum] = (INTPS) result;
			break;

      case ET_SYSERR:	parm = (INTPS) va_arg(vp,int);
      			if (parm < 0) {
			  mparm[pnum] = (INTPS) host_errlist[-parm];
			} else {
			  errno = 0;
			  char *err_str = strerror(parm);
			  
			  if ( errno == 0 ) {
			    mparm[pnum] = (INTPS) err_str;
			  } else {
			    result = &buf[++loc];
			    loc += sprintf ( &buf[loc],
					     "Unix error %ld", parm );
			    mparm[pnum] = (INTPS) result;
			  }
			}
			break;

      /* The default case takes care of all host program-specific
       * parameter kinds, which must be pointers:
       */
      default:	result = Host_Format_Parm ( kind, va_arg(vp,char *) );
		/* Copy the string from the result into buffer: */
		++loc;
		strncpy ( &buf[loc], result, BUFLEN_USER-loc );
		result = &buf[loc];
		loc += strlen (result);
		mparm[pnum] = (INTPS) result;
		break;
    }
    
  }

  /* Prepare main error message: */
  emsg = vstr_begin(512);
  loc = vstr_sprintf (&emsg, 0, ED_format(edesc), mparm[0], mparm[1],
		   mparm[2], mparm[3], mparm[4], mparm[5] );
  emsg = vstr_concat (emsg, "\n");

  /* TODO: user error messages usually have a second line that is a copy of */
  /* the source line, and a third line with blanks and a ^ pointing to the */
  /* character of interest.  We are not printing that out.  The easiest */
  /* way to remedy that would be to fopen the file and find the desired line */
  /* (fgets()).  It's feasible, but if there are lots of warnings and errors */
  /* it becomes O(n^2) in the number of characters, which may or may not be */
  /* a problem.  By reading the file the first time this is called with a */
  /* given file name, and keeping an extensible array (or even linked list) */
  /* of offsets to the beginnings of lines, we could access the file */
  /* efficiently enough.  A minor headache. */

  /* Produce the message: */
  Emit_Message ( hmsg, vstr_str(emsg) );
  vstr_end(emsg);

  if (ED_compiler(edesc)) Had_Compiler_Error = TRUE;

  /* Abort at highest severity level: */
  if ( mlevel >= ES_ERRABORT ) {
    Signal_Cleanup( 0 );
#ifdef __MINGW32__
    if ( ecode == EC_Signal )	raise ( SIGILL );
#else /* __MINGW32__ */
    if ( ecode == EC_Signal )	kill ( getpid(), SIGILL );
#endif /* __MINGW32__ */
    exit(RC_NORECOVER_USER_ERROR);
  }

  /* If we've seen too many errors, abort: */
  if ( Error_Count > Max_Errors ) {
    ErrMsgLine ( EC_Too_Many, ERROR_LINE_UNKNOWN, Error_Count );
    /* Won't return */
  }

  return;
}

static void
#ifdef TARG_NVISA
ErrMsg_Report ( INT ecode, INT line, const char *file, const char *dname, va_list vp )
#else
ErrMsg_Report ( INT ecode, INT line, const char *file, va_list vp )
#endif
{
  ERROR_DESC *edesc = Find_Error_Desc (ecode);

  if ( ED_user(edesc) )
#ifdef TARG_NVISA
    ErrMsg_Report_User ( edesc, ecode, line, file, dname, vp );
#else  
    ErrMsg_Report_User ( edesc, ecode, line, file, vp );
#endif
  else
#ifdef TARG_NVISA
    ErrMsg_Report_Nonuser ( edesc, ecode, line, file, dname, vp );
#else  
    ErrMsg_Report_Nonuser ( edesc, ecode, line, file, vp );
#endif
}

#ifdef TARG_NVISA
static void
ErrMsg_Report ( INT ecode, INT line, const char *file, va_list vp )
{
  ErrMsg_Report (ecode, line, file, NULL, vp);
}
#endif  /* TARG_NVISA */


/* ====================================================================
 *
 * ErrMsg / ErrMsgLine / ErrMsgSrcpos
 *
 * Report an error at the current (passed) line number.
 *
 * ====================================================================
 */

void
ErrMsg ( INT ecode, ... )
{
  va_list vp;
  
  va_start ( vp, ecode);
  ErrMsg_Report ( ecode, Source_Line, Source_File_Name, vp );
  va_end ( vp );
}

/* ================================================================= */

void
ErrMsgLine ( INT ecode, INT line, ... )
{
  va_list vp;
  
  va_start ( vp, line );
  ErrMsg_Report ( ecode, line, Source_File_Name, vp );
  va_end ( vp );
}


#ifdef MONGOOSE_BE
/* ================================================================= */

void
ErrMsgSrcpos ( INT ecode, SRCPOS srcpos, ... )
{
  INT32  line = Srcpos_To_Line(srcpos);
  const char   *fname = NULL;
  const char   *dname;

  va_list vp;
  va_start ( vp, srcpos );

  IR_Srcpos_Filename(srcpos, &fname, &dname);
#ifdef TARG_NVISA  
  ErrMsg_Report ( ecode, line, fname, dname, vp );
#else  
  ErrMsg_Report ( ecode, line, fname, vp );
#endif
  va_end ( vp );
}
#endif

/* ====================================================================
 *
 *  Abort_Compiler_Location
 *
 *  An assertion failure is in progress.  Receive the compiler file
 *  and line number for later reporting.
 *
 *  This replaces in line assignments to global variables in the
 *  assertion.  The problem with that way of doing things is that it
 *  can cause lint (other checkers?) to complain when assertions are
 *  included in access macros and used in contexts with undefined
 *  order of evaluation.  For example:
 *
 *    if ( ND_fld(x) == ND_fld(y) )
 *
 *  would cause a lint warning with Is_True_On.
 *
 * ====================================================================
 */

void Abort_Compiler_Location (
  const char * file_name,
  INT    line_number )
{
  Compiler_File = file_name;
  Compiler_Line = line_number;
}

/* ====================================================================
 *
 * Fail_Assertion
 *
 * An assertion has failed -- report the fact.
 *
 * ====================================================================
 */

void
Fail_Assertion ( INT ecode, ... )
{
  va_list vp;
  
  va_start ( vp, ecode);
  do_traceback = true;
  ErrMsg_Report ( ecode, Source_Line, Source_File_Name, vp );
  va_end ( vp );
}

/* ====================================================================
 *
 * Fail_FmtAssertion
 *
 * A FmtAssertion has failed -- report the fact.
 *
 * Report a failure with the given printf format and parameters to the
 * error file(s), and then abort.
 *
 * ====================================================================
 */

void
Fail_FmtAssertion ( const char *fmt, ... )
{
  va_list vp;
  
  INT dlevel = ES_ERRABORT;	/* Severity level */
  INT mlevel = dlevel;
  char hmsg[512], emsg[512];
  INT loc;

  /* Count error: */
  ++Error_Counts[dlevel];

  /* Prepare header line: */
  loc = sprintf ( &hmsg[0], "%s%s%s", SEV_symbol(mlevel),
		  "Compiler ", SEV_name(mlevel) );
  if ( Source_File_Name != NULL && *Source_File_Name != 0 ) {
    loc += sprintf ( &hmsg[loc], " in file %s", Source_File_Name );
  }
  if ( Current_Phase != NULL ) {
    loc += sprintf ( &hmsg[loc], " during %s phase", Current_Phase );
  }
  sprintf ( &hmsg[loc], ":\n" );

  /* Prepare main error message: */
  va_start ( vp, fmt );
  loc = sprintf ( &emsg[0], "%s", SEV_symbol(mlevel) );
  loc += vsprintf ( &emsg[loc], fmt, vp );
  sprintf ( &emsg[loc], "\n" );
  va_end ( vp );

  /* Report the error: */
  Emit_Message ( hmsg, emsg );

  /* Abort: */
  Signal_Cleanup( 0 );
  exit(RC_INTERNAL_ERROR);
}

/* ====================================================================
 *
 * Fatal_Error
 *
 * A Require has failed -- report the fact.
 *
 * Report a failure with the given printf format and parameters to the
 * error file(s), and then abort.  This routine is similar to
 * Fail_FmtAssertion, but reports a user error instead of a compiler
 * error.
 *
 * ====================================================================
 */

void
Fatal_Error ( const char *fmt, ... )
{
  va_list vp;
  INT dlevel = ES_ERRABORT;	/* Severity level */
  INT mlevel = dlevel;
  char hmsg[512], emsg[512];
  INT loc;

  /* Count error: */
  ++Error_Counts[dlevel];

  /* Prepare header line: */
  loc = sprintf ( &hmsg[0], "%s%s", SEV_symbol(mlevel),
		  SEV_name(mlevel) );
  if ( Source_File_Name != NULL && *Source_File_Name != 0 ) {
    loc += sprintf ( &hmsg[loc], " in file %s", Source_File_Name );
  }
  if ( Current_Phase != NULL ) {
    loc += sprintf ( &hmsg[loc], " during %s phase", Current_Phase );
  }
  sprintf ( &hmsg[loc], ":\n" );

  /* Prepare main error message: */
  va_start ( vp, fmt );
  loc = sprintf ( &emsg[0], "%s", SEV_symbol(mlevel) );
  loc += vsprintf ( &emsg[loc], fmt, vp );
  sprintf ( &emsg[loc], "\n" );
  va_end ( vp );

  /* Report the error: */
  Emit_Message ( hmsg, emsg );

  /* Abort: */
  Signal_Cleanup( 0 );
  exit(RC_INTERNAL_ERROR);
}

/* ====================================================================
 *
 * Rag_Handle_Woff_Args
 *
 * Process a -woff argument:  Mark the given error codes to be ignored.
 * Ignore bad/illegal values.
 *
 * ====================================================================
 */

extern void Rag_Handle_Woff_Args ( char *wstring )
{
  INT i=0;
  INT inp_size = strlen(wstring);
  INT msgnum1, msgnum2;

  /* check that argument string is correct and set woff */
  while ( i < inp_size ) {
    /* get next warning number */
    if ( isdigit ( wstring[i] ) ) {
      /* get first number of the range */
      msgnum1 = atoi(&wstring[i]);
      while ( isdigit ( wstring[++i] ) );
      /* get second number of the range */
      if ((wstring[i] == '-') && isdigit(wstring[++i])) {
	msgnum2 = atoi(&wstring[i]);
	while ( isdigit ( wstring[++i] ) );
      } else {
	msgnum2 = msgnum1;
      }

      msgnum1 = MAX ( msgnum1, RAG_EN_FIRST );
      msgnum2 = MIN ( msgnum2, RAG_EN_LAST );
      /* set the warning within the range (inclusive) */
      for (; msgnum1 <= msgnum2; msgnum1++)
	Dont_Print_Warning(msgnum1) = TRUE;
    }

    /* skip past next comma */
    while ( i < inp_size && wstring[i++] != ',' ) {};

  } /* while i < inp_size */
}

static INT Current_Phase_Number = 0;

void
Set_Current_Phase_Number( INT phase )
{
  Current_Phase_Number = phase;
}

INT
Get_Current_Phase_Number( void )
{
  return Current_Phase_Number;
}


#if !defined(MONGOOSE_BE) || !defined(SHARED_BUILD)
/* be dynamically sets the tables, but static builds must call this */
/* ====================================================================
 *
 * Set_Error_Tables
 *
 * Error table initialization.  All users of this package must call
 * this routine before emitting any error messages.
 *
 * ====================================================================
 */

extern void Set_Error_Tables( 
  ERROR_DESC_TABLE *edt,
  const char *errlist[] )
{
  Phases = edt;
  host_errlist = errlist;
}
#endif /* MONGOOSE_BE */

extern void
Set_Error_Descriptor (INT phase, ERROR_DESC *descriptor)
{
    register INT i = 0;

    while (Phases[i].phase != -1) {
	if (Phases[i].phase == phase) {
	    Phases[i].descriptors = descriptor;
	    return;
	}
	i++;
    }

    FmtAssert (FALSE, ("Error Phase %d not initialized", phase));
} /* Set_Error_Descriptor */

/* ====================================================================
 *
 * DevWarn
 *
 * See interface description
 *
 * ====================================================================
 */

static BOOL dev_warn_enabled = FALSE;

extern BOOL 
DevWarn_Enabled()
{
  return dev_warn_enabled;
}

extern void
DevWarn( const char *fmt, ... )
{
  va_list args;


  const char *phase_name = (Current_Phase != NULL) ? Current_Phase : "unknown phase";

  if ( dev_warn_enabled ) {
    /* Write to standard error first: */
    /* with newer gcc need to do start/end around each vprintf */
    va_start ( args, fmt );
    fprintf ( stderr, "!!! DevWarn during %s: ", phase_name );
    vfprintf ( stderr, fmt, args );
    fprintf ( stderr, "\n" );
    fflush ( stderr );
    va_end(args);
  }
 
  /* Then write to error file if enabled: */
  if ( Init_Error_File() ) {
    va_start ( args, fmt );
    fprintf ( Error_File, "!!! DevWarn during %s: ", phase_name );
    vfprintf ( Error_File, fmt, args );
    fprintf ( Error_File, "\n" );
    fflush ( Error_File );
    va_end(args);
  }

  /* Finally write to trace file: */
  if ( Trace_File != NULL ) {
    va_start ( args, fmt );
    fprintf ( Trace_File, "!!! DevWarn during %s: ", phase_name );
    vfprintf ( Trace_File, fmt, args );
    fprintf ( Trace_File, "\n" );
    fflush ( Trace_File );
    va_end(args);
  }

}

/* ====================================================================
 *
 * DevWarn_limit_search
 *
 * For use only by Count_Limit_DevWarn. Ultimately this linear search
 * should probably be replaced by a faster hash lookup.
 *
 * ====================================================================
 */
typedef struct {
  const char *fname;
  UINT	      line;
  UINT	      count;
} LIMIT_STRUCT;

static LIMIT_STRUCT *
DevWarn_limit_search(const char *const src_fname,
		     const UINT        src_line)
{
  static LIMIT_STRUCT *dw_ls_buf     = NULL;
  static UINT          dw_ls_buf_siz = 0;
  static UINT          dw_ls_num     = 0;
  static LIMIT_STRUCT  dummy_ls = { NULL, 0, 0 }; // In case of realloc failure
  UINT i;

  for (i = 0; i < dw_ls_num; i++) {
    if (dw_ls_buf[i].line == src_line &&
	(dw_ls_buf[i].fname == src_fname ||	/* speed hack */
	 (strcmp(dw_ls_buf[i].fname, src_fname) == 0))) {
      return dw_ls_buf + i;
    }
  }

  /* Didn't find the entry we're looking for. We're going to have to
   * add an entry.
   */
  if (dw_ls_num >= dw_ls_buf_siz) {
    UINT new_ls_buf_siz;
    LIMIT_STRUCT *new_ls_buf;

    /* Expand the buffer of LIMIT_STRUCTs if possible. If not
     * possible, return value points to a LIMIT_STRUCT that always
     * says we should emit the error message.
     */
    if (dw_ls_buf_siz == 0) {
      new_ls_buf_siz = 1024;
    }
    else {
      new_ls_buf_siz *= 2;
    }
    new_ls_buf = (LIMIT_STRUCT *)
	realloc(dw_ls_buf, new_ls_buf_siz * sizeof(LIMIT_STRUCT));
    if (new_ls_buf != NULL) {
      dw_ls_buf     = new_ls_buf;
      dw_ls_buf_siz = new_ls_buf_siz;
    }
    else {
      /* realloc() failed.
       */
      dummy_ls.count = 0;
      return &dummy_ls;
    }
  }

  dw_ls_buf[dw_ls_num].line  = src_line;
  dw_ls_buf[dw_ls_num].fname = src_fname;
  dw_ls_buf[dw_ls_num].count = 0;

  return dw_ls_buf + (dw_ls_num++);
}

/* ====================================================================
 *
 * Count_Limit_DevWarn
 *
 * For use through Lmt_DevWarn macro; see interface description
 *
 * ====================================================================
 */

extern BOOL
Count_Limit_DevWarn(const char *const src_fname,
		    const UINT        src_line,
		    const UINT        limit)
{
  /* Check conditions used by DevWarn that result in output. If
   * DevWarn will not generate output, don't bother wasting time or
   * memory in DevWarn_limit_search.
   */
  if (!(dev_warn_enabled || Init_Error_File() || Trace_File != NULL)) {
    return TRUE;
  }

  LIMIT_STRUCT *s = DevWarn_limit_search(src_fname, src_line);
  s->count++;
  if (s->count == limit) {
    DevWarn("Count limit reached on the following DevWarn:");
  }
  return s->count <= limit;
}

/* ====================================================================
 *
 * DevWarn_Toggle
 *
 * See interface description
 *
 * ====================================================================
 */
extern void
DevWarn_Toggle(void)
{
  dev_warn_enabled = ! dev_warn_enabled;
}

extern BOOL
Had_Internal_Error (void)
{
	return Had_Compiler_Error;
}
