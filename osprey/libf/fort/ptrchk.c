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



#pragma ident "@(#) libf/fort/ptrchk.c	92.2	06/24/99 10:18:36"
#include <fortran.h>
#include <liberrno.h>
#include <stddef.h>
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#include <stdlib.h>
#endif

/*
 *  Error handler for an unassociated POINTER reference warning.
 *  This entry is called by the cf90 compiler only.
 *
 *  Input Arguments:
 *    file     - File name in which error occurred.
 *    line     - Line number in file.
 *    variable - Name of pointer that is unassociated.
 *    dv_desc  - determines either "POINTER", "ALLOCATABLE array"
 *                or "ASSUMED SHAPE array".
 *    count    - Static count/flag to indicate if this message was
 *               already given for this statement.
 */

void
_POINTER_ERROR(
	char	*file,		/* Fortran routine containing error	*/
	int	*line,		/* Line number in Fortran routine	*/
	char	*variable,	/* unassociated pointer variable name	*/
	int	*dv_desc,	/* either "POINTER", "ALLOCATABLE array" */
				/* or "ASSUMED SHAPE array".		*/
	int	*count)		/* Count/flag for number of messages	*/
{
	int	*retcnt;	/* ptr to static arg count word		*/
	int	intcnt = 0;	/* local count if no count passed	*/
#ifdef	_UNICOS
	/* Use local variable if count argument not passed. */
	if (_numargs() < 5)
		retcnt	= &intcnt;
	else
#endif
		retcnt	= count;
	if ((*retcnt)++ == 0) {
		if (*dv_desc == 1) {
                	(void) _fwarn(FWASOCPT, variable, *line, file);
                } else if (*dv_desc == 2) {
			(void) _fwarn(FWALOCAR, variable, *line, file);
                } else if (*dv_desc == 3) {
			(void) _fwarn(FWASASAR, variable, *line, file);
		}
	}
	return;
}
