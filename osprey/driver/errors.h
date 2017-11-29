/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
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

#include <stdarg.h>
#include "basic.h"

/* return codes for driver */
typedef int status_codes;
extern int internal_error_occurred;	/* for return/exit */
extern status_codes error_status;	/* for return/exit */

extern char *program_name;		/* name of invoked program */

extern boolean print_warnings;		/* whether to print warning msgs */
extern boolean fullwarn;		/* whether to print all warnings */
extern boolean pass_exit_codes;         /* If true, remember the highest
					 * exit code. */

extern void init_error_list ();

extern void error (char *format, ...)
     __attribute__((format (printf, 1, 2)));

extern void parse_error (const char *name, const char *msg);

extern void warning (char *format, ...)
     __attribute__((format (printf, 1, 2)));

extern void warn_ignored (char *name);

extern void warn_nyi (char *name);

extern void warn_no_longer_needed (char *name);
extern void warn_no_longer_supported (char *name);
extern void warn_no_longer_supported2 (char *name, char *newname);

extern void internal_error (char *format, ...)
     __attribute__((format (printf, 1, 2)));

/* squirrel away error info for bug reporting */
extern void vlog_error (char *format, va_list ap);
extern void log_error (char *format, ...)
     __attribute__((format (printf, 1, 2)));

/* to signal that an error occured but trust previous error messages */
extern void nomsg_error (int);

/* has_errors returns true if were any errors anywhere */
extern boolean has_errors (void);
/* has_current_errors returns true if were any errors in current compile */
extern boolean has_current_errors (void);
/* clear_current_errors resets the error count for a new current compile */
extern void clear_current_errors (void);

