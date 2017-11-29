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


#pragma ident "@(#) libu/util/prealloc.c	92.1	07/07/99 13:18:33"

#include <liberrno.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <cray/assign.h>

#define BLKSIZE		4096		/* bytes per disk block */

/*
 *	_prealloc - performs preallocation of file if requested by assign
 *
 *	Returns:   0 - ok
 *		  -1 - error occurred. errno will be set.
 */

#ifdef	_UNICOS

_prealloc(
int		fd,			/* File descriptor */
int		aifound,		/* 1 if assign record found, else 0 */
assign_info	*aip,			/* assign info, or NULL if none */
struct stat	*statp)			/* from stat(2) system call.  If NULL,
					 * stat(2) will be called here. */
{
	int	sz;
	long	nb;
	long	bytpp;		/* bytes per partition */
	int	cbits;
	int	part;
	long	rsize;
	int	numparts;
	int	mode;
	struct stat lstatbuf;

/*
 *	If assign was not found for this file, simply return now.
 */
	if (!aifound) return(0);

/*
 *	If no -n option, then return now.
 */
	if (aip->n_preall_flg == 0) return(0);
/*
 *	If preallocation has already been handled, then return now.
 */
	if (aip->n_preall_flg & ATTR_USED) return(0);

/*
 *	Set the bit to indicate that preallocation has been handled.
 */
	aip->n_preall_flg |= ATTR_USED;
/*
 *	Set up parameters for striping and pre-allocation from the fields in
 *	the assign structure(s)
 */
	sz = 0;

	if (aip->n_preall_flg)
		sz = aip->n_preall * BLKSIZE;		/* alloc size in bytes*/
/*
 *	See if preallocation is necessary
 */
	if (sz == 0)
		return(0);	/* no preallocation requested */

/*
 *	Call fstat(2) if a NULL statp was passed in.
 */
	if (statp == NULL) {
		statp = &lstatbuf;
		if (fstat(fd, statp) == -1)
			return(-1);
	}

/*
 *	Get the bytpp, cbits, and mode.
 */
	bytpp = 0;

	if (aip->q_ocblks_flg) 
		bytpp = aip->q_ocblks * BLKSIZE;
	else if (aip->n_stride_flg) 
		bytpp = aip->n_stride * BLKSIZE;

	cbits = 0;
	if (aip->pr_partit_flg)
		cbits = aip->pr_partit;

	mode = 0;
	if (aip->c_contig_flg)
		mode = IA_CONT;

	numparts = _popcnt(cbits);	/* Count number of partitions */
	if (numparts > 0)
		mode = mode | IA_PART;

/*
 *	If bytpp was not implied by -n or in st_cblks, pick a good default.
 *
 *	If the user has requested one or no file partitions with -p, then
 *	all space may be requested with one ialloc.  Otherwise if more than
 *	one partition have been selected, a default cblks is used.
 */

	if (numparts <= 1)
		bytpp = sz;
	else if (bytpp == 0) {
		if (statp->st_cblks != 0) 
			bytpp = statp->st_cblks * BLKSIZE;
		else if (statp->st_oblksize > 0)
			bytpp = statp->st_oblksize;	/* Track size */
		else
			bytpp = BLKSIZE;
	}

/*
 *	Allocation loop.
 */
	rsize = sz;	/* remaining amount to allocate */
	part = 0;
	while (rsize > 0) {
		/* Round robin through the partitions */
		if (cbits)		/* skip any unused partitions */
			while ((cbits & (1<<part)) == 0)
				part = (part + 1) & 077;

		if (bytpp > rsize)
			bytpp = rsize;
		if ((nb = ialloc(fd, bytpp, mode, part))== -1) {
			/* Don't use this partition anymore */
			if (cbits)
				cbits &=~(1<<part);

			if (--numparts <= 0) {
				/*
				 * Failed to allocate the specified amount
				 * of file space.  Remove the space we
				 * have allocated so far.
				 */
				(void) lseek(fd, statp->st_size, 0);
				(void) trunc(fd);
				return(-1); /* ialloc will set errno */
			}

		}
		else
			rsize -=nb;
		if (cbits)		/* cycle to next partition number */
			part = (part + 1) & 077;
	}	/* while */

	return(0);
}
#endif	/* _UNICOS */

#ifndef	_UNICOS
_prealloc(int fd, int aifound, void *aip, void *p)
{
	return(0);
}
#endif
