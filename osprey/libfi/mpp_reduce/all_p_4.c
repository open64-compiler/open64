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


#pragma ident "@(#) libfi/mpp_reduce/all_p_4.c	92.1	07/09/99 17:05:42"


#include <stdlib.h>
#include <liberrno.h>
#include <fmath.h>
#include <cray/dopevec.h>
#include "f90_macros.h"

#define SIGN_BIT	(1 << 63)
#define RANK		4

/*
 *	Compiler generated call: CALL _ALL_LP4(RESULT, SOURCE, DIM)
 *
 *	Purpose: Determine whether all values are true in SOURCE along
 *	         dimension DIM. This particular routine handles
 *		 of source arrays of rank 4.
 *
 *	Arguments:
 *	        SOURCE - Dope vector for logical source array
 *	        DIM    - Dimension to operate along
 *
 *      Description:
 *		This is the MPP version of ALL. This particular
 *		file contains the the intermediate type-specific
 *		routines. These routines parse and update the dope
 *		vectors, allocate either shared or private space for
 *		the result temporary, and possibly update the shared
 *		data desriptor (sdd) for the result temporary. Once
 *		this set-up work is complete, a Fortran subroutine
 *		is called which uses features from the Fortran
 *		Programming Model to distribute the work across all
 *		processors.
 *
 *		Include file all_p.h contains the rank independent
 *		source code for this routine.
 */

#include "all_p.h"
