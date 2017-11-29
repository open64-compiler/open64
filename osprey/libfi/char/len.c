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


#pragma ident "@(#) libfi/char/len.c	92.1	07/08/99 10:41:51"
#include <fortran.h>

/*
 *	Duplicate names
 *
 *	_LEN_	- for f90 intrinsic
 *	_LEN_	- for cf77 intrinsic
 *	_LEN	- for f90 3.0? and previous on PVP systems 
 */
#ifdef _UNICOS
#pragma	_CRI duplicate _LEN_ as $LEN
#pragma	_CRI duplicate _LEN_ as _LEN
#endif

_f_int
_LEN_(_fcd f)
{
	return ((_f_int) _fcdlen(f));
}


#ifdef	_F_INT4
/*
 *	Duplicate names
 *
 *	_LEN_4_	- for f90 intrinsic
 *	_LEN_4_	- for cf77 intrinsic
 *	_LEN_4	- for f90 3.0? and previous on PVP systems 
 */
#ifdef _UNICOS
#pragma	_CRI duplicate _LEN_4_ as $LEN_4
#pragma	_CRI duplicate _LEN_4_ as _LEN_4
#endif

_f_int4
_LEN_4_(_fcd f)
{
	return ((_f_int4) _fcdlen(f));
}
#endif


#ifdef	_F_INT8
/*
 *	Duplicate names
 *
 *	_LEN_8_	- for f90 intrinsic
 *	_LEN_8_	- for cf77 intrinsic
 *	_LEN_8	- for f90 3.0? and previous on PVP systems 
 */
#ifdef _UNICOS
#pragma	_CRI duplicate _LEN_8_ as $LEN_8
#pragma	_CRI duplicate _LEN_8_ as _LEN_8
#endif

_f_int8
_LEN_8_(_fcd f)
{
	return ((_f_int8) _fcdlen(f));
}
#endif
