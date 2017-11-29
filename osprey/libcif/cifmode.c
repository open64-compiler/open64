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


static char USMID[] = "@(#) libcif/cifmode.c	30.2	07/26/96 07:19:13";


/* -------------------------------------------------------------------------
 * Cif_Memmode sets the memmory management mode for a CIF file.  This return
 * must be called after Cif_Open but before any calls to Cif_Getrecord to
 * be effective.  The mode may be one of three values:
 *
 *   CIF_MEM_INDIV   = Each record structure is returned as an individually
 *                     malloc'd block
 *   CIF_MEM_FIXED   = Each record structure is returned using the same
 *                     internally malloc'd space
 *   CIF_MEM_MANAGED = Space for each record is allocated from larger
 *                     internally malloc'd buffers.  The entire amount of
 *                     space can be free'd at one time.
 * -------------------------------------------------------------------------
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <stdio.h>

#include "cif_int.h"

int Cif_Memmode
#ifdef __STDC___
(int cifd, int mode)
#else
(cifd, mode)
int cifd;			/* cif descriptor of file */
int mode;			/* memory management mode selector */
#endif
{

	int me;

	if (cifd < 0 || cifd >= CIF_FT_SIZE || _Cif_filetbl[cifd].form == NOT_A_CIF)
		return (CIF_NOTOPEN);
	else if (mode < 1 || mode >= CIF_MEM_MAX ||
	         _Cif_filetbl[cifd].mode != CIF_MEM_DEFAULT ||
				_Cif_filetbl[cifd].optype != 'r')
	{
		return (CIF_BADREQ);
	}

	if (mode != CIF_MEM_INDIV) {

		/* If memarea table not present, get it. */

		if (_Cif_memasize == 0) {
			if ((me =_Cif_memtbl()) != 0)
				return (me);
		}

		/* Get the first memory entry and buffer for the file */

		if ((me = _Cif_mementry (CIF_BUFSIZE)) < 0)
			return (me);
		_Cif_filetbl[cifd].fme = _Cif_filetbl[cifd].lme = me;
	}
	_Cif_filetbl[cifd].mode = (short) mode;
	return (0);

}
