/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


static char USMID[] = "@(#) libcif/cifprint.c	30.2	07/26/96 07:19:13";


/* -------------------------------------------------------------------------
 * Cif_Printinfo prints out statistics on the current state of interface
 * internal tables.
 * -------------------------------------------------------------------------
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdio.h>

#include "cif_int.h"

void Cif_Printinfo
#ifdef __STDC___
(FILE *fd)
#else
(fd)
FILE *fd;			/* file descriptor where output should go */
#endif
{

	register int fi,me;

	for (fi = 0; fi < CIF_FT_SIZE; fi++) {
		if (_Cif_filetbl[fi].form == NOT_A_CIF) continue;
		(void) fprintf (fd, "CIF file entry %2d:  form= %1d  ifull= %1d  seek= %1d  mode= %1d  fme= %3d  lme= %3d\n    rmask= %o  fd= %p  ip= %p  optype= %c\n",
			fi,
			_Cif_filetbl[fi].form,
			_Cif_filetbl[fi].ifull,
			_Cif_filetbl[fi].seek,
			_Cif_filetbl[fi].mode,
			_Cif_filetbl[fi].lme,
			_Cif_filetbl[fi].fme,
			_Cif_filetbl[fi].rmask[0],
			_Cif_filetbl[fi].fd,
			_Cif_filetbl[fi].ip,
			_Cif_filetbl[fi].optype);
	}

	for (me = 0; me < _Cif_memasize; me++) {
		if (_Cif_memarea[me].used == 0 && _Cif_memarea[me].mbp == 0 && _Cif_memarea[me].msize == 0) continue;
		(void) fprintf (fd, "CIF memory entry %3d:  used=%d  nme=%3d  mused= %4d  msize= %4d  mbp= %p\n",
			me,
			_Cif_memarea[me].used,
			_Cif_memarea[me].nme,
			_Cif_memarea[me].mused,
			_Cif_memarea[me].msize,
			_Cif_memarea[me].mbp);
	}

}
