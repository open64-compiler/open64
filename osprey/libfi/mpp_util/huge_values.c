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


#pragma ident "@(#) libfi/mpp_util/huge_values.c	92.1	07/13/99 10:45:02"


/**************************************************************************
 *
 * This file contains utility subroutines that are used by the
 * MPP version of the Fortran 90 array intrinsics.
 *
 *************************************************************************/

#include <fortran.h>
#include <fmath.h>


/****************************************************************************
*
*       HUGE_INT returns the largest largest legal value for an integer.
*
****************************************************************************/

int
HUGE_INT()
{
        return (HUGE_INT8_F90);
}

/****************************************************************************
*
*       HUGE_FLT returns the largest largest legal value for an integer.
*
****************************************************************************/

float
HUGE_FLT()
{
        return (HUGE_REAL8_F90);
}
