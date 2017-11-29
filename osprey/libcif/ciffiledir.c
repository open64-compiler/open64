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


static char USMID[] = "@(#) libcif/ciffiledir.c	30.3	07/26/96 07:19:13";


/*
 *	Cif_Getfiledir returns the file directory from an open sorted CIF.
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <memory.h>
#include <stdio.h>

#include "cif_int.h"

int Cif_Getfiledir
#ifdef __STDC__
(int cifd, struct Cif_filedir **fdir)
#else
(cifd, fdir)
int cifd;
struct Cif_filedir **fdir;
#endif
{

	char buf[128], *cp;
	int i, mode, n, rtype;						
	struct Cif_filedir *fp;
	struct Cif_unittbl *ut;
	FILE *fd;
	int status = 0;				/* status value */


	if (cifd < 0 || cifd > CIF_FT_SIZE || _Cif_filetbl[cifd].form == NOT_A_CIF)
		return (CIF_NOTOPEN);
	else if (_Cif_filetbl[cifd].optype == 'w' || _Cif_filetbl[cifd].form ==
			ASCII_CIF || _Cif_filetbl[cifd].seek == NO)
		return (CIF_BADREQ);

	/* Read a byte.  If not a header or file directory, reset to the start
	 * of the file and read again.  If a header, skip the record and read
	 * again.  If file directory, read it.
	 */

	_Cif_filetbl[cifd].ifull = NO; 
	fd = _Cif_filetbl[cifd].fd;
	if (fread (buf, sizeof(char), 1, fd) != 1) {
		if (feof(fd))
			rtype = 0;
		else return (CIF_SYSERR);
	}
	rtype = ((struct Cif_generic *)buf)->rectype;
	if (rtype != CIF_CIFHDR && rtype != CIF_FILEDIR) {
		if (fseek (fd, 0L, 0))
			return (CIF_SYSERR);
		if (fread (buf, sizeof(char), BINARY_HDR_LEN, fd) != BINARY_HDR_LEN)
			return (CIF_SYSERR);
		if (fread (buf, sizeof(char), 1, fd) != 1) IO_ERROR;
		rtype = ((struct Cif_generic *)buf)->rectype;
	}
	if (rtype == CIF_CIFHDR) {
		if (fread(&buf[1], _Cif_shortsize[CIF_CIFHDR][_Cif_filetbl[cifd].version]-1, 1, fd) != 1) IO_ERROR;
		if (fread (buf, sizeof(char), 1, fd) != 1) IO_ERROR;
		rtype = ((struct Cif_generic *)buf)->rectype;
	}
	if (rtype == CIF_FILEDIR) {
		mode = _Cif_filetbl[cifd].mode;

		/*
		 * If memory management mode isn't set, then set to FIXED.  If the
		 * mode is FIXED, reset the amount of buffer used.
		 */

		if (mode == CIF_MEM_DEFAULT) {
		  if ((status = Cif_Memmode (cifd, CIF_MEM_FIXED)) != 0)
		    return (status);
		  mode = _Cif_filetbl[cifd].mode;
		}
		if (mode == CIF_MEM_FIXED)
		  _Cif_memarea[_Cif_filetbl[cifd].fme].mused = 0;

		/* _Cif_filetbl[cifd].return_version is what the user wants */
		/* _Cif_filetbl[cifd].version is what the cif on disk is */

		fp = *fdir = (struct Cif_filedir *)_Cif_space[mode]
		                                     (_Cif_structsize[CIF_FILEDIR][_Cif_filetbl[cifd].return_version], cifd);
		if (fp == NULL)
			return (CIF_NOMEM);
		cp = (char *)fp;
		(void) memset (cp, '\0', _Cif_structsize[CIF_FILEDIR][_Cif_filetbl[cifd].return_version]);
		if (fread (++cp, _Cif_shortsize[CIF_FILEDIR][_Cif_filetbl[cifd].version]-1, 1, fd) != 1) IO_ERROR;
		n = fp->nunits;
		fp->rectype = rtype;
		ut = fp->ut = (struct Cif_unittbl *) _Cif_space[mode]
		                                     (sizeof(struct Cif_unittbl)*n, cifd);
		if (ut == NULL)
			return (CIF_NOMEM);
		(void) memset((char *)ut, 0, n * sizeof(struct Cif_unittbl));
		for (i = 0; i < n; i++, ut++) {
			if (fread ((char *)ut, UNITTBL_SSIZE, 1, fd) != 1) IO_ERROR;
			cp = ut->name = _Cif_space[mode] (ut->nlen+1, cifd);
			if (cp == NULL)
				return (CIF_NOMEM);
			if (fread (cp, sizeof(char), ut->nlen, fd) != ut->nlen) IO_ERROR;
			cp[ut->nlen] = '\0';
		}
	}
	else
		return (CIF_BADFORM);
	return (0);
}
