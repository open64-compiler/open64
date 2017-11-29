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


static char USMID[] = "@(#) libcif/cifsetpos.c	30.2	07/26/96 07:19:13";


/*
 * Cif_Setpos positions a compiler input input file to the indicated location.
 */

#define CIF_VERSION 3


#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdio.h>

#include "cif_int.h"

int Cif_Setpos
#ifdef __STDC__
(int cifd, long pos)
#else
(cifd, pos)
int cifd;		/* CIF file descriptor */
long pos;		/* file position */
#endif
{

	if (cifd < 0 || cifd >= CIF_FT_SIZE || _Cif_filetbl[cifd].form == NOT_A_CIF)
		return (CIF_NOTOPEN);
	else if (_Cif_filetbl[cifd].seek == NO)
		return (CIF_BADREQ);
	else {
		_Cif_filetbl[cifd].ifull = NO;
		if (pos == CIF_FIRST_RECORD) {
			if (_Cif_filetbl[cifd].form == BINARY_CIF)
				pos = BINARY_HDR_LEN;
			else
				pos = 0;
		}
		if (fseek (_Cif_filetbl[cifd].fd, pos, 0))
			return (CIF_SYSERR);
	}
	return (0);
}
