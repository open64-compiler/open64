/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* ====================================================================
 * ====================================================================
 *
 * Module: opt_error.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_error.c,v $
 *
 * Revision history:
 *  30-AUG-94 - Original Version
 *
 * ====================================================================
 *
 * Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it
 * is free of the rightful claim of any third person regarding
 * infringement  or the like.  Any license provided herein, whether
 * implied or otherwise, applies only to this software file.  Patent
 * licenses, if any, provided herein do not apply to combinations of
 * this program with other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 * Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
 * Mountain View, CA 94043, or:
 *
 * http://www.sgi.com
 *
 * For further information regarding this notice, see:
 *
 * http://oss.sgi.com/projects/GenInfo/NoticeExplan
 *
 * ====================================================================
 * ====================================================================
 */

static char *source_file = __FILE__;
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/opt/SCCS/s.opt_error.c $ $Revision: 1.5 $";

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "defs.h"
#include "opt_defs.h"
#include "opt_error.h"


/*
 *   err_info is indexed by the error_number.   
 *   The first field of err_info also stores the error_number.
 *   we should assert the expression (err_info[i].errnum != i).
 */
struct err_info {
  INT  errnum;
  char *str;
};


/*  
 *  The abort location is set when the error type is ER_FATAL.
 */
char *opt_Abort_File = NULL;         /* name of the file that asserted */
INT   opt_Abort_Loc  = 0;            /* line number that asserted */

static INT   opt_num_errors;
static BOOL  infoflag = TRUE;
static BOOL  verbose  = TRUE;

struct err_info err_str[MAX_ERN_NUMBER]= {
  {ERN_IGNORED,       0},
  {ERN_INTERNAL,      "Internal error"},
  {ERN_OUT_OF_MEM,    "Out of memory"},
  {ERN_BAD_OPTION,    "Unknown option: %s (ignored)."}
};


static void error_routine(unsigned doexit, const char *err_type, INT err_num, const char *fmt, va_list ap)
{
  if (doexit && opt_Abort_File && opt_Abort_Loc)
    fprintf(stderr, "Abort at line %d of %s\n", opt_Abort_Loc, opt_Abort_File);
  
  if (err_type) {
    if (err_num > 0)
      fprintf (stderr, "%s %d: ", err_type, err_num);
    else
      fputs (err_type, stderr);
  }
  
  vfprintf(stderr, fmt, ap);
  
  if (fmt[strlen(fmt)-1] != '\n')
    fputc ('\n', stderr);
  fflush(stderr);
  
  if (doexit)
    exit(1);
} /* error_routine */


static void opt_fatal(INT errnum, va_list ap)
{
  /* do NOT use OPT_ASSERT for (err_str[errnum].errnum != errnum) because
     OPT_ASSERT calls opt_fatal() eventually.  It might loop! */
  if ((err_str[errnum].errnum == errnum)
      || (err_str[errnum].errnum == ERN_IGNORED)) {
    error_routine(TRUE, "FATAL", errnum, err_str[errnum].str, ap);
  } else {
    error (TRUE, err_str[ERN_INTERNAL].errnum, "opt_fatal()");
  }
}


static void opt_error(INT errnum, va_list ap)
{
  opt_num_errors++;
  OPT_ASSERT (err_str[errnum].errnum == errnum || err_str[errnum].errnum == ERN_IGNORED,
	      "opt_error()");
  error_routine(FALSE, "ERROR", errnum, err_str[errnum].str, ap);
}


static void opt_verbose (INT errnum, va_list ap)
{
  OPT_ASSERT (err_str[errnum].errnum == errnum || err_str[errnum].errnum == ERN_IGNORED,
	      "opt_verbose()");
  error_routine(FALSE, 0, errnum, err_str[errnum].str, ap);
}


static void opt_info(INT errnum, va_list ap)
{
  if (infoflag) {
    OPT_ASSERT (err_str[errnum].errnum == errnum || err_str[errnum].errnum == ERN_IGNORED,
		"opt_info()");
    error_routine(FALSE, "INFO", errnum, err_str[errnum].str, ap);
  }
}


void OPT_Error(INT type, INT errnum, ...)
{
  va_list ap;
  
  va_start(ap, errnum);
  switch (type) {
  case ER_FATAL:
    opt_fatal(errnum, ap);
    break;
  case ER_INFO:
    opt_info(errnum, ap);
    break;
  case ER_ERROR:
    opt_error(errnum, ap);
    break;
  case ER_VERBOSE:
    opt_verbose (errnum, ap);
    break;
  }
  va_end (ap);
}


void OPT_Error_Fmt(INT type, const char *fmt_str, ...)
{
  va_list ap;
  char *err_type_str;
  BOOL doexit;
  
  va_start(ap, fmt_str);
  switch (type) {
  case ER_FATAL:
    err_type_str = "FATAL";
    doexit = TRUE;
    break;
  case ER_INFO:
    err_type_str = "INFO";
    doexit = FALSE;
    break;
  case ER_ERROR:
    err_type_str = "ERROR";
    doexit = FALSE;
    break;
  case ER_VERBOSE:
    err_type_str = "VERBOSE";
    doexit = FALSE;
    break;
  }
  error_routine(doexit, err_type_str, ERN_IGNORED, fmt_str, ap);
  va_end(ap);
}


static void
signal_cleanup(void)
{
}


static void
catch (int sig, int error_num)
{
    switch (sig) {
        register string msg;
    case SIGBUS:
    case SIGSEGV:
        msg = error_num ? strerror(error_num) : strsignal(sig);
        fprintf (stderr, "%s: Signal: %s.\n", myname, msg);
        fflush (stderr);
        break;
    }
    signal (sig, SIG_IGN);
    signal_cleanup ();
    exit (1);
} /* catch */


void Opt_Catch_Signals (void)
{
    if (signal(SIGHUP , SIG_IGN) != SIG_IGN) signal(SIGHUP , catch);
    if (signal(SIGINT , SIG_IGN) != SIG_IGN) signal(SIGINT , catch);
    if (signal(SIGQUIT, SIG_IGN) != SIG_IGN) signal(SIGQUIT, catch);
    if (signal(SIGILL , SIG_IGN) != SIG_IGN) signal(SIGILL , catch);
    if (signal(SIGTRAP, SIG_IGN) != SIG_IGN) signal(SIGTRAP, catch);
    if (signal(SIGIOT , SIG_IGN) != SIG_IGN) signal(SIGIOT , catch);
    if (signal(SIGEMT , SIG_IGN) != SIG_IGN) signal(SIGEMT , catch);
    if (signal(SIGFPE , SIG_IGN) != SIG_IGN) signal(SIGFPE , catch);
    if (signal(SIGBUS , SIG_IGN) != SIG_IGN) signal(SIGBUS , catch);
    if (signal(SIGSEGV, SIG_IGN) != SIG_IGN) signal(SIGSEGV, catch);
    if (signal(SIGTERM, SIG_IGN) != SIG_IGN) signal(SIGTERM, catch);
} /* Opt_Catch_Signals */
