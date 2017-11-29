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


#pragma ident "@(#) libu/ffio/globgrshmem.c	92.3	10/14/99 15:22:06"

#include <fortran.h>
#include "globio.h"

extern void _shmem_group_inquire(int handle, shmem_group_t *sgs);

extern glio_group_t _glio_curgroup;

void
glio_group_shmem(int handle)
{

	glio_group_t *gg;
        shmem_group_t shg;

	gg = &_glio_curgroup;

	_shmem_group_inquire(handle, &shg);

	gg->groupsz = shg.groupsz;
	gg->myrank = shg.myrank;
	gg->grtype = GR_SHMEM;

	gg->u.shmem.group = handle;
}

void
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
glio_group_shmem_(
#else
GLIO_GROUP_SHMEM(
#endif
_f_int *handlep)
{
	glio_group_shmem(*handlep);
}
