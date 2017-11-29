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


#pragma ident "@(#) libfi/char/f90_verify.c	92.2	07/30/99 10:09:59"

#include <fortran.h>

typedef struct
{
    char *ptr;   		/* character portion */
    unsigned long   len;	/* its length */
} fcd_t;
  
extern _f_int _VERIFY( fcd_t  str1,
		       fcd_t  str2,
		       _f_log *back );

_f_int _F90_VERIFY( char    *str1,
	            char    *str2,
	            _f_int  *back,
	            _f_int  len1,
	            _f_int  len2 )
{
    fcd_t  fcd1, fcd2;

    fcd1.ptr = str1;
    fcd1.len = len1;
    fcd2.ptr = str2;
    fcd2.len = len2;
    
    return( _VERIFY( fcd1, fcd2, back ) );
}


#ifdef	_F_INT4
extern _f_int4 _VERIFY_4( fcd_t  str1,
		          fcd_t  str2,
		          _f_log *back );

_f_int4 _F90_VERIFY_4( char    *str1,
	               char    *str2,
	               _f_int  *back,
	               _f_int  len1,
	               _f_int  len2 )
{
    fcd_t  fcd1, fcd2;

    fcd1.ptr = str1;
    fcd1.len = len1;
    fcd2.ptr = str2;
    fcd2.len = len2;
    
    return( _VERIFY_4( fcd1, fcd2, back ) );
}
#endif


#ifdef	_F_INT8
extern _f_int8 _VERIFY_8( fcd_t  str1,
		          fcd_t  str2,
		          _f_log *back );

_f_int8 _F90_VERIFY_8( char    *str1,
	               char    *str2,
	               _f_int  *back,
	               _f_int  len1,
	               _f_int  len2 )
{
    fcd_t  fcd1, fcd2;

    fcd1.ptr = str1;
    fcd1.len = len1;
    fcd2.ptr = str2;
    fcd2.len = len2;
    
    return( _VERIFY_8( fcd1, fcd2, back ) );
}
#endif

#ifdef	_F_INT2
extern _f_int2 _VERIFY_2( fcd_t  str1,
		          fcd_t  str2,
		          _f_log *back );

_f_int2 _F90_VERIFY_2( char    *str1,
	               char    *str2,
	               _f_int  *back,
	               _f_int  len1,
	               _f_int  len2 )
{
    fcd_t  fcd1, fcd2;

    fcd1.ptr = str1;
    fcd1.len = len1;
    fcd2.ptr = str2;
    fcd2.len = len2;
    
    return( _VERIFY_2( fcd1, fcd2, back ) );
}
#endif

#ifdef	_F_INT1
extern _f_int1 _VERIFY_1( fcd_t  str1,
		          fcd_t  str2,
		          _f_log *back );

_f_int1 _F90_VERIFY_1( char    *str1,
	               char    *str2,
	               _f_int  *back,
	               _f_int  len1,
	               _f_int  len2 )
{
    fcd_t  fcd1, fcd2;

    fcd1.ptr = str1;
    fcd1.len = len1;
    fcd2.ptr = str2;
    fcd2.len = len2;
    
    return( _VERIFY_1( fcd1, fcd2, back ) );
}
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)

extern _f_int4 _verify90(char *str1, _f_int4 len1, char *str2, _f_int4 len2, _f_int4 back);

_f_int4
_verify90(char *str1,
	_f_int4 len1,
	char *str2,
	_f_int4 len2,
	_f_int4 back)
{
	_f_int4 back1 = back;
	return (_F90_VERIFY(str1, str2, &back1, len1, len2));
}

#endif	/* __mips */
