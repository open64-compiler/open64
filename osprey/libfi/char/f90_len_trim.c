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


#pragma ident "@(#) libfi/char/f90_len_trim.c	92.1	07/08/99 10:41:51"

#include <fortran.h>

extern _f_int _LEN_TRIM_( _fcd f );

int
_F90_LEN_TRIM_( char *ptr, int len )
{
    return( _LEN_TRIM_( _cptofcd(ptr, len) ) );
}


#ifdef	_F_INT4
extern _f_int4 _LEN_TRIM_4_( _fcd f );

_f_int4
_F90_LEN_TRIM_4_( char *ptr, int len )
{
    return( _LEN_TRIM_4_( _cptofcd(ptr, len) ) );
}
#endif


#ifdef	_F_INT8
extern _f_int _LEN_TRIM_8_( _fcd f );

_f_int8
_F90_LEN_TRIM_8_( char *ptr, int len )
{
    return( _LEN_TRIM_8_( _cptofcd(ptr, len) ) );
}
#endif
