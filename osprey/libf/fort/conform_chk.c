/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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



#pragma ident "@(#) libf/fort/conform_chk.c	92.3	06/24/99 10:18:36"
#include <liberrno.h>
#include <stddef.h>
#if	defined(__mips)	|| defined(_LITTLE_ENDIAN)
#include <stdlib.h>
#endif
#include "fio.h"

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
/*
 *  Error handler for an array syntax conformance warning.
 *  This entry is called by the cf90 compiler only.
 *
 *  Input Arguments:
 *    file     - File name in which error occurred.
 *    line     - Line number in file.
 *    dim      - Dimension number which has a size mismatch.
 *    count    - address of static word to contain flag and count
 *               of number of times this statement called.  Count
 *               will be one by default.
 */

void
_CONFORM_ERROR(
	char	*file,		/* Fortran routine containing error */
	int	*line,		/* Line number in Fortran routine */
	int	*dim,		/* Dimension number with size mismatch */
	int	*count)		/* count/flag to give message once per
				 * statement. */
{
	int	intcnt = 0;	/* local count if no count passed */
	int	*retcnt;	/* ptr to count word */
#ifdef	_UNICOS
	/* Use local variable if count argument not passed. */
	if (_numargs() < 4)
		retcnt = &intcnt;
	else
#endif
		retcnt = count;
	if (*retcnt == 0) {
		*retcnt = 1;
		(void) _fwarn(FWARGDIM, *dim, *line, file);
	}
	return;
}

#else	/* otherwise __mips */

/*
 *  Error handler for an array syntax conformance warning.
 *  This entry is called by the f90 compiler on IRIX only.
 *
 *  Input Arguments:
 *    file     - File name in which error occurred.
 *    line     - Line number in file.
 *    dim      - Dimension number which has a size mismatch.
 *    extent1  - One of two mismatched 64-bit extents of dimension dim.
 *    extent2  - One of two mismatched 64-bit extents of dimension dim.
 *
 * ON MIPS, if the environment varaiable is set to Y(ES), produce an
 * error message with the information and then abort.  Otherwise,
 * produce a warning for the conformity check.
 * 
 * When the input dimension is zero, this routine is being called
 * from an inline version of a transformational function such as
 * MATMUL and the use of a dimension would be confusing since the
 * first dimension of one argument and the second dimension of the
 * other argument are mismatched.
 *
 * When the input dimension is nonzero, this routine is being called
 * when the specified dimension is the same for both arguments.
 *
 * The message contains the name of this routine for debugging.
 */
void
__f90_conform_check(
	char		*file,
	int		line,
	int		dim,
	long long	extent1,
	long long	extent2)
{
	char	*abort_now = NULL;
	char	*rtn_nm = "__f90_conform_check";

	abort_now	= getenv("F90_CONFORM_CHECK_ABORT");
	if (extent1 < 0)
		extent1 = 0;
	if (extent2 < 0)
		extent2 = 0;


	/* abort only if environment variable is present and is Y(es) */
	if (abort_now && (*abort_now == 'y' || *abort_now == 'Y')) {

		/* Use proper message depending on value of dimension. */
		if (dim != 0)
			(void) _lerror(_LELVL_MSG,FWARGDMD, dim, line,
			   file, extent1, extent2, rtn_nm);
		else
			(void) _lerror(_LELVL_MSG,FWARGDMZ, line,
			   file, extent1, extent2, rtn_nm);
		_fcleanup();
		abort();
	}

	/* Use proper message depending on value of dimension.  */
	if (dim != 0)
		(void) _fwarn(FWARGDMD, dim, line, file, extent1,
		   extent2, rtn_nm);
	else
		(void) _fwarn(FWARGDMZ, line, file, extent1, extent2,
		   rtn_nm);
}
#endif /* end __mips */
