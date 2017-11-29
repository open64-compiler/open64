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


#pragma ident "@(#) libfi/mpp_shift/eoshift_s_wd1_6.c	92.1	07/13/99 10:44:06"


#include <stdlib.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include "f90_macros.h"

#define RANK		6

/*
 *	Compiler generated call:
 *		CALL _EOSHIFT_JS6(RESULT, SOURCE, SHIFT, BOUNDARY, DIM)
 *		CALL _EOSHIFT_LS6(RESULT, SOURCE, SHIFT, BOUNDARY, DIM)
 *		CALL _EOSHIFT_SS6(RESULT, SOURCE, SHIFT, BOUNDARY, DIM)
 *
 *	Purpose: Perform an end-off shift of the SOURCE array along the DIM
 *		 dimension.  The magnitude of the shift is found in SHIFT.
 *		 The value to be shifted in is contained in BOUNDARY
 *
 *	Arguments:
 *		RESULT   - Dope vector for result temporary array
 *		SOURCE   - Dope vector for user source array
 *		SHIFT    - Dope vector for shift count
 *		BOUNDARY - Dope vector for boundary (optional)
 *		DIM	 - Dimension along which to shift (optional)
 *
 *	Description:
 *		This is the MPP version of EOSHIFT.  This particular file
 *		contains the intermediate routines.  These routines parse
 *		and update the dope vectors, allocate either shared or
 *		private space for the result temporary, and possibly update
 *		the shared data descriptor (sdd) for the result temporary.
 *		Once this set-up work is complete, a Fortran subroutine is
 *		called which uses features from the Fortran Programming
 *		Model to perform the actual shift.
 *
 *		Include file eoshift_p.h contains the rank independent
 *		source code for this routine.
 */

#pragma duplicate _EOSHIFT_JS6 as _EOSHIFT_LS6
#pragma duplicate _EOSHIFT_JS6 as _EOSHIFT_SS6

void
_EOSHIFT_JS6 (
		DopeVectorType  *result,
		DopeVectorType  *source,
		DopeVectorType  *shift,
		DopeVectorType  *boundary,
		int		*dim)

{
#include "eoshift_s.h"

/*
 *      Call the Fortran work routine
 */

        if (_is_shared_pointer(source->base_addr.a.ptr)) {
	    EOSHIFT_WD1_S6@ ( result_base_ptr, source_sdd_ptr, shift_sdd_ptr,
		bound_sdd_ptr, &dim_val, src_extents, shft_extents,
		bnd_extents, &shflag, &shftval, &bndflag, &bndval);
	}
}
