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


#pragma ident "@(#) libfi/mathlb/ieee_unordered.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include "inline.h"

#if _F_REAL16 == 1
extern _f_log4 _IEEE_UNORDERED_L4_D_D( _f_real16 x, _f_real16 y);

/* Function returns TRUE if either 128-bit real argument is a NaN.
 * Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_D_D( _f_real16 x, _f_real16 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan128(x) || isnan128(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_D_D( _f_real16 x, _f_real16 y);

_f_log8 _IEEE_UNORDERED_L8_D_D( _f_real16 x, _f_real16 y)
{
	/* if x is NaN , return TRUE */
	return ((_f_log8) _btol(isnan128(x) || isnan128(y)));
}


extern _f_log4 _IEEE_UNORDERED_L4_D_R( _f_real16 x, _f_real8 y);

/* Function returns TRUE if one of the 128-bit or 64-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_D_R( _f_real16 x, _f_real8 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan128(x) || isnan64(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_D_R( _f_real16 x, _f_real8 y);

_f_log8 _IEEE_UNORDERED_L8_D_R( _f_real16 x, _f_real8 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan128(x) || isnan64(y)));
}


extern _f_log4 _IEEE_UNORDERED_L4_D_H( _f_real16 x, _f_real4 y);

/* Function returns TRUE if one of the 128-bit or 32-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_D_H( _f_real16 x, _f_real4 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan128(x) || isnan32(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_D_H( _f_real16 x, _f_real4 y);

_f_log8 _IEEE_UNORDERED_L8_D_H( _f_real16 x, _f_real4 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan128(x) || isnan32(y)));
}


extern _f_log4 _IEEE_UNORDERED_L4_R_D( _f_real8 x, _f_real16 y);

/* Function returns TRUE if one of the 64-bit or 128-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_R_D( _f_real8 x, _f_real16 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan64(x) || isnan128(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_R_D( _f_real8 x, _f_real16 y);

_f_log8 _IEEE_UNORDERED_L8_R_D( _f_real8 x, _f_real16 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan64(x) || isnan128(y)));
}


extern _f_log4 _IEEE_UNORDERED_L4_H_D( _f_real4 x, _f_real16 y);

/* Function returns TRUE if one of the 32-bit or 128-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_H_D( _f_real4 x, _f_real16 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan32(x) || isnan128(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_H_D( _f_real4 x, _f_real16 y);

_f_log8 _IEEE_UNORDERED_L8_H_D( _f_real4 x, _f_real16 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan32(x) || isnan128(y)));
}

#endif  /* _F_REAL16 */


extern _f_log4 _IEEE_UNORDERED_L4_R_R( _f_real8 x, _f_real8 y);

/* Function returns TRUE if one of two 64-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_R_R( _f_real8 x, _f_real8 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan64(x) || isnan64(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_R_R( _f_real8 x, _f_real8 y);

_f_log8 _IEEE_UNORDERED_L8_R_R( _f_real8 x, _f_real8 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan64(x) || isnan64(y)));
}


extern _f_log4 _IEEE_UNORDERED_L4_R_H( _f_real8 x, _f_real4 y);

/* Function returns TRUE if one of the 64-bit or 32-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_R_H( _f_real8 x, _f_real4 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan64(x) || isnan32(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_R_H( _f_real8 x, _f_real4 y);

_f_log8 _IEEE_UNORDERED_L8_R_H( _f_real8 x, _f_real4 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan64(x) || isnan32(y)));
}


extern _f_log4 _IEEE_UNORDERED_L4_H_R( _f_real4 x, _f_real8 y);

/* Function returns TRUE if one of the 32-bit or 64-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_H_R( _f_real4 x, _f_real8 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan32(x) || isnan64(y)));
}


extern _f_log8 _IEEE_UNORDERED_L8_H_R( _f_real4 x, _f_real8 y);

_f_log8 _IEEE_UNORDERED_L8_H_R( _f_real4 x, _f_real8 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan32(x) || isnan64(y)));
}


extern _f_log4 _IEEE_UNORDERED_L4_H_H( _f_real4 x, _f_real4 y);

/* Function returns TRUE if one of two 32-bit real arguments
 * is a NaN.  Else, it returns FALSE.
 */
_f_log4 _IEEE_UNORDERED_L4_H_H( _f_real4 x, _f_real4 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log4) _btol(isnan32(x) || isnan32(y)));
}

extern _f_log8 _IEEE_UNORDERED_L8_H_H( _f_real4 x, _f_real4 y);

_f_log8 _IEEE_UNORDERED_L8_H_H( _f_real4 x, _f_real4 y)
{
	/* if x is NaN, return TRUE */
	return ((_f_log8) _btol(isnan32(x) || isnan32(y)));
}
