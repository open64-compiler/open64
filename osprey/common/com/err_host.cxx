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



#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source$ $Revision$";
#endif

#include "defs.h"
#include "errors.h"
#include "err_host.h"
#include "stab.h"
#include "strtab.h"
#include "glob.h"

/* ====================================================================
 *
 * Host_Format_Parm
 *
 * Format host program-specific error message parameters.
 *
 * ====================================================================
 */

/* Formatting buffer: */
#define BUF_SIZE 1024
static char buf[BUF_SIZE];
static INT idx = 0;

const char *
Host_Format_Parm (
  INT kind,	/* One of the parameter types in errors.h or above */
  MEM_PTR parm)	/* A message parameter passed by the reporter */
{
  INT16 loc = idx;

  /* Handle compiler-specific cases: */
  switch ( kind ) {
    case ET_SYMTAB:	if ( parm == NULL ) return "<Null symbol>";
			return (char *) ST_name(((ST *)parm));
    default:
      idx += 1 + sprintf ( &buf[loc], "<Unknown parm:%p, kind:%d>",
			   parm, kind );
      if ( idx > BUF_SIZE - 64 ) idx = 0;
      return &buf[loc];
  }
}



/* ====================================================================
 *
 * Signal_Cleanup
 *
 * Clean up before dying in response to a signal.
 *
 * ====================================================================
 */

void
Signal_Cleanup ( INT sig )
{
  Cleanup_Files ( FALSE, TRUE );
#ifdef FRONT_END_FORTRAN
#ifdef DEBUG
  Kqqtrc();
#endif
  { INT i = 4;
    Kqqsrc(&i);
  }
  Kqqexi();
#endif
}
