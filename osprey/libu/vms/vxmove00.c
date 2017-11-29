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


#pragma ident "@(#) libu/vms/vxmove00.c	92.1	06/23/99 13:56:22"
#include <string.h>

/*
 *	ISTAT = VXMOVE00(SOURCE,ISB,NUM,DEST,IDB)
 *
 *	Parameters:
 *
 *		SOURCE	Word address of source string.
 *
 *		ISB	Address of source byte offset.  Byte 1 is the
 *			first byte of SOURCE.
 *
 *		NUM	Address of byte count.
 *
 *		DEST	Word address of destination string.
 *
 *		IDB	Address of destination byte offset.  Byte 1 is
 *			the first byte of DEST.
 *
 *	ISTAT:
 *
 *		-1	if called with less than 5 arguments, if
 *			NUM < 0, if ISB < 1, or if IDB < 1.
 *
 *		0	if data moved
 */

long
VXMOVE00(
	long	*source,	/* Address of source data	*/
	long	*isb,		/* Starting byte of source data	*/
	long	*num,		/* Number of bytes to move	*/
	long	*dest,		/* Address of destination data	*/
	long	*idb		/* First byte of destination	*/
	)
{
	char	*from, *to;
	int	count;
	long	status;

	status	= -1;

	if (_numargs() >= 5 && *num >= 0 && *isb > 0 && *idb > 0) {

		from	= (char *)source + (*isb - 1);
		to	= (char *)  dest + (*idb - 1);
		count	= *num;

		(void) memcpy(to, from, count);

		status	= 0;
	}

	return(status);
}
