/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdio.h>
#include <cmplrs/rcodes.h>
#include "errors.h"
#include "string_utils.h"

status_codes error_status = RC_OKAY;
int internal_error_occurred;
char *program_name;
boolean print_warnings = TRUE;
boolean fullwarn = FALSE;
boolean pass_exit_codes = FALSE;

static int errors = 0;
static int previous_errors = 0;

string_list_t *error_list = NULL;

void
init_error_list(void)
{
	error_list = init_string_list();
}

static void
set_error_status (status_codes e)
{
	if (pass_exit_codes) {
		if (e > error_status) {
			error_status = e;
		}
	}
	else {
		if (error_status == RC_OKAY) {
			error_status = e;
		}
	}
}

void
vlog_error(char *format, va_list ap)
{
	char *msg;

	if (vasprintf(&msg, format, ap) == -1)
		return;

	add_string(error_list, msg);
}

void
log_error(char *format, ...)
{
	va_list ap;

	va_start(ap, format);
	vlog_error(format, ap);
	va_end(ap);
}

void
error(char *format, ...)
{
	va_list args;
	va_start (args, format);
	fprintf(stderr, "%s ERROR: ", program_name);
	vfprintf(stderr, format, args);
	fprintf(stderr, "\n");
	va_end (args);
	set_error_status(RC_USER_ERROR);
	errors++;
}

void
parse_error (const char *name, const char *msg)
{
	fprintf(stderr, "%s ERROR parsing %s: %s\n", 
		program_name, name, msg);
	set_error_status(RC_USER_ERROR);
	errors++;
}

void
warning (char *format, ...)
{
	va_list args;
	if (!print_warnings) return;
	va_start (args, format);
	fprintf(stderr, "%s WARNING: ", program_name);
	vfprintf(stderr, format, args);
	fprintf(stderr, "\n");
	va_end (args);
}

void
warn_ignored (char *name)
{
	warning("%s is ignored", name);
}

void
warn_nyi (char *name)
{
	warning("%s is not yet implemented", name);
}

void
warn_no_longer_needed (char *name)
{
	warning("%s is no longer needed", name);
}

void
warn_no_longer_supported (char *name)
{
	warning("%s is no longer supported", name);
}

void
warn_no_longer_supported2 (char *name, char *newname)
{
	warning("%s is no longer supported, use %s instead", name, newname);
}

void
internal_error (char *format, ...)
{
	va_list args;
	va_start (args, format);
	fprintf(stderr, "%s INTERNAL ERROR: ", program_name);
	vfprintf(stderr, format, args);
	fprintf(stderr, "\n");
	va_end (args);

	va_start (args, format);
	vlog_error (format, args);
	va_end (args);

	internal_error_occurred = 1;
	set_error_status(RC_INTERNAL_ERROR);
	errors++;
}

/* to signal that an error occured but trust previous error messages */
void
nomsg_error (int status)
{
	set_error_status(status);
	errors++;
}

boolean
has_errors(void)
{
	return (errors > 0 || previous_errors > 0);
}

boolean
has_current_errors(void)
{
	return (errors > 0);
}

void 
clear_current_errors(void)
{
	previous_errors = errors;
	errors = 0;
}
