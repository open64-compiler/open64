/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fort/task_dv_getfirst.c	92.1	06/24/99 10:18:36"
#include <liberrno.h>
#include <stddef.h>

/*
 *  Error handler for an array-allocation/pointer-association
 *  error when tasking is on.
 *  This entry is called by the cf90 compiler only.
 *
 *  Input Arguments:
 *    file     - File name in which error occurred.
 *    line     - Line number in file.
 *    variable - variable name of allocatable array or Fortran pointer.
 */

void
_TASK_DV_GETFIRST_ERROR(
        char    *variable,      /* allocatable array or Fortran pointer */
        char    *file,          /* Fortran routine containing error */
        int     *lineno)        /* Line number in Fortran routine */
{
        (void) _lerror(_LELVL_ABORT, FENGFLCL, variable, file, *lineno);
        return;
}
