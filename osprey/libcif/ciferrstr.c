/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


static char USMID[] = "@(#) libcif/ciferrstr.c	30.2	07/26/96 07:19:13";


/* -------------------------------------------------------------------------
 * Cif_Errstring returns a pointer to a character string that describes the
 * error condition associated with a CIF status.
 * -------------------------------------------------------------------------
 */

#define CIF_VERSION 3

#include <string.h>
#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <errno.h>

static char *strings[] = {
	"",
	"The file is not a CIF format file",
	"The maximum number of CIF files are already open",
	"The CIF descriptor is not attached to an open CIF file",
	"No more memory can be acquired",
	"The end of a CIF file has been encountered",
	"An internal error in the CIF interface routines has occurred",
	"The requested CIF function cannot be performed",
	"The CIF file contains invalid data",
	"",
	"The CIF version specified to Cif_Open is greater than allowed",
	"The CIF version encountered in the file is greater than allowed",
	"The CIF version specified to Cif_Open is less than allowed",
	"libcif.a does not match the cif.h file compiled into this application",
};
static char *unknown = "Unknown CIF status code";

char *Cif_Errstring
#ifdef __STDC___
(int status)
#else
(status)
int status;			/* cif error status code */
#endif
{
	if (status >= 0 || status < CIF_MAXERROR)
		return (unknown);
	else if (status == CIF_SYSERR) {
		int err_num = errno;
		int ok;
		char *err_str;
		
		errno = 0;
		err_str = strerror(err_num);
		ok = errno == 0;
		errno = err_num;
	
		if (ok)
			return (err_str);
		else
			return (unknown);
	}
	else
		return (strings[-status]);
}
