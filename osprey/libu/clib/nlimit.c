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

#pragma ident "@(#) libu/clib/nlimit.c	92.1	07/01/99 13:42:20"

#include <errno.h>
#include <sys/resource.h>

/*
 * NLIMIT is the fortran library interface to nlimit(3).  It expects two
 * parameters: an id (pid, sid, or uid) and a fortran array.  The code 
 * translates the array into the resclim structure and calls nlimit(3).
 * Upon successful completion the structure is translated back into the
 * array and returns a 0 return code.  If the nlimit(3) call fails, errno
 * is returned.
 */

long
NLIMIT(tid, res_array, errstat)
long *tid;
long res_array[10];
long *errstat;
{
	struct resclim rs;

	rs.resc_resource = res_array[0];
	rs.resc_category = res_array[1];
	rs.resc_type     = res_array[2];
	rs.resc_action   = res_array[3];
	rs.resc_used     = res_array[4];
	rs.resc_value[L_T_ABSOLUTE] = res_array[5];
	rs.resc_value[L_T_HARD] = res_array[6];
	rs.resc_value[L_T_SOFT] = res_array[7];

	if (nlimit(*tid, &rs) == -1) {
		*errstat = errno;
		return(-1);
	}

	res_array[0] = rs.resc_resource;
	res_array[1] = rs.resc_category;
	res_array[2] = rs.resc_type;
	res_array[3] = rs.resc_action;
	res_array[4] = rs.resc_used;
	res_array[5] = rs.resc_value[L_T_ABSOLUTE];
	res_array[6] = rs.resc_value[L_T_HARD];
	res_array[7] = rs.resc_value[L_T_SOFT];
	return(0);
}
