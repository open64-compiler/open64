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

#pragma ident "@(#) libu/clib/clibcall.c	92.1	07/01/99 13:42:20"
#include <fortran.h>
#include <stdio.h>

/*
 *	_clib_call	- driver routine to call a utility routine or 
 *			  system call after translating one _fcd to
 *			  a C character pointer.
 */
int
_clib_call(
int	(*func)(),
_fcd	arg1,
long	arg2,
long	arg3,
long	arg4,
long	arg5)

{
	char	*charptr;
	int	ret;
	int	num_syscall_args;

#ifndef	_ADDR64
	if (!_isfcd(arg1))	/* If not Fortran character */
		charptr	= *(char **)&arg1;
	else			
#endif
	{
		charptr	= _f2ccpy(arg1);

		if (charptr == NULL)
			return(-1);
	}

	num_syscall_args = _numargs() - sizeof(_fcd)/sizeof(long);

	switch( num_syscall_args ) {
		case 1:  ret = (*func)(charptr);			break;
		case 2:  ret = (*func)(charptr,arg2);			break;
		case 3:  ret = (*func)(charptr,arg2,arg3);		break;
		case 4:  ret = (*func)(charptr,arg2,arg3,arg4);		break;
		case 5:  ret = (*func)(charptr,arg2,arg3,arg4,arg5);	break;
		default: abort();
	}

#ifndef	_ADDR64
	if (_isfcd(arg1))
#endif
		free(charptr);

	return(ret);
}
