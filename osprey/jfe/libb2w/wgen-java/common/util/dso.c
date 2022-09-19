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


#include <alloca.h>
#include <stdio.h>		    /* for fprintf() */
#include <string.h>
#include <dlfcn.h>		    /* for sgidladd(), dlerror() */
#include <cmplrs/rcodes.h>

#include "defs.h"
#include "dso.h"

/*
 * WARNING:  dso is used outside of main mongoose compiler (via targ_info)
 * and should not depend on other mongoose-specific files 
 * (e.g. this is why verbose is a parameter rather than a global variable).
 */

/*
 * Assume that LD_LIBRARY_PATH has already been set up correctly.
 */
void
load_so (char *soname, char *path, BOOL verbose)
{
    register char *full_path;

    if (path != 0) {
	full_path = (char *) alloca (strlen(path)+strlen(soname)+2);
	strcpy (full_path, path);
	strcat (full_path, "/");
	strcat (full_path, soname);

        if (verbose) {
	    fprintf (stderr, "\nReplacing default %s with %s\n", soname, full_path);
        }
    } else {
        full_path = soname;
    }

#ifndef linux
    if (sgidladd (full_path, RTLD_LAZY) == NULL)
#else
    if (dlopen (full_path, /*RTLD_NOW |*/ RTLD_LAZY|RTLD_GLOBAL) == NULL)
#endif
    {
	fprintf (stderr, "Error loading %s: %s\n", full_path, dlerror());
	exit (RC_SYSTEM_ERROR);
    }
} /* load_so */
