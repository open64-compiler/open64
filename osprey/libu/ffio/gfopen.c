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


#pragma ident "@(#) libu/ffio/gfopen.c	92.2	06/29/99 13:16:47"

#include <stdio.h>
#include <malloc.h>
#include <ffio.h>
#include "gfio.h"
#include "fxlist.h"

DECLARE(F_XLIST);
struct xtr_s F_XLIST_P = { F_XLIST };

extern struct f_rec_limit_s _F_limits[];

/*
 * Define parameters for F class records
 */

static struct gen_ff _Frec_def_tab[NUM_F_TYPES] =

/*		padd?	char?				*/
{
	{	NO,	NONE },		/* illegal */
	{	NO,	NONE },		/* IBM F */
	{	NO,	NONE },		/* IBM FB */
	{	NO,	NONE },		/* VMS F_DSK */
	{	YES,	'^' },		/* VMS F_TP */
	{	NO,	NONE },		/* VMS F_TR */
};

/*
 * Initialize the state of a foreign dataset.  Allocate buffers
 * and initialize pointers.
 */

_ffopen_t
_gen_fopen(
const char	*name,
int		flags,
mode_t		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*stat,
long		cbits,
int		cblks,
struct gl_o_inf *oinf)
	{
	char		*ptr;
	union spec_u	*nspec;
	long		recsize, blksize;	/* bits */
	long		rsz, mbs;		/* bytes */
	_ffopen_t	nextfio;
	int		rtype;
	struct gen_ff	*ff_dat;

/*
 *	convert 8-bit bytes to bits
 */
	rsz = spec->fld.recsize;
	mbs = spec->fld.mbs;
	rtype = spec->fld.recfmt;

	if (rtype < 0 || rtype >= NUM_F_TYPES)
		{
		_SETERROR(stat, FDC_ERR_BADSPC, 0);
		return(_FFOPEN_ERR);
		}
/*
 *	General limit checks from table.
 */
	if (rsz == 0)
		{
		_SETERROR(stat, FDC_ERR_BADSPC, 0);
		goto badret;
		}
	if (rsz < _F_limits[rtype].min_rsz ||
	    rsz > _F_limits[rtype].max_rsz)
		{
		_SETERROR(stat, FDC_ERR_BADSPC, 0);
		goto badret;
		}
	if (mbs != 0)
		if (mbs < _F_limits[rtype].min_mbs ||
		    mbs > _F_limits[rtype].max_mbs)
			{
			_SETERROR(stat, FDC_ERR_BADSPC, 0);
			goto badret;
			}
	switch(rtype)
		{
		case TR_IBM_F:
/*
 *			if mbs and rsz specified with
 *			F format and mbs != rsz then error
 */
			if (mbs != rsz && mbs != 0)
				{
				_SETERROR(stat, FDC_ERR_BADSPC, 0);
				goto badret;
				}
		case TR_IBM_FB:
			if (mbs == 0)
				mbs = rsz; /* dflt mbs = rsz */

			/* must be exact multiple */
			if ((mbs % rsz) != 0)
				{
				_SETERROR(stat, FDC_ERR_BADSPC, 0);
				goto badret;
				}
			break;
		case TR_VMS_F_DSK:
		case TR_VMS_F_TP:
		case TR_VMS_F_TR:
			if (mbs == 0) /* unspecified */
				{
				/* deflt mbs=rsz */
				if (rtype != TR_VMS_F_TP)
				/* deflt mbs=rsz */
					mbs = rsz;
				else if(rtype == TR_VMS_F_TP)
					{
					/* dflt mbs=2048 */
					mbs = 2048;
					if (rsz > mbs) mbs = rsz;
					}
				}
			if (rsz > mbs)
				{
				_SETERROR(stat, FDC_ERR_BADSPC, 0);
				goto badret;
				}
			break;
		default:
			_SETERROR(stat, FDC_ERR_BADSPC, 0);
			goto badret;
		}

	recsize = rsz << 3;
	blksize = mbs << 3;
/*
 *	Internally, both blksize and recsize are in bits!
 */
	fio->maxrecsize = recsize;
	fio->maxblksize = blksize;
	fio->_ffbufsiz = blksize;	/* bit size of buffer */
/*
 *	Allocate buffer
 */
	ptr = malloc((blksize >> 3) + 16);
	if (ptr == NULL) goto nomem;
/*
 *	Allocate private data area
 */
	fio->lyr_info = (char *)calloc(sizeof(struct gen_ff), 1);
	if (fio->lyr_info == NULL) goto nomem;

	/* load up record characteristics */
	ff_dat = (struct gen_ff *)fio->lyr_info;
	*ff_dat = _Frec_def_tab[rtype];

	SET_BPTR(fio->_base, CPTR2BP(ptr));
	fio->rwflag = POSITIN;
	fio->segbits = 0;
	fio->_cnt = 0;
	fio->_ptr = fio->_base;
/*
 *	First, open the lower layers
 */
	nspec = spec;
	NEXT_SPEC(nspec);
	nextfio = _ffopen(name, flags, mode, nspec, stat, cbits, cblks, NULL,
			oinf);
	if (nextfio == _FFOPEN_ERR) goto badret;

	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

nomem:
	_SETERROR(stat, FDC_ERR_NOMEM, 0);
badret:
	if (BPTR2CP(fio->_base) != NULL) free(BPTR2CP(fio->_base));
	if (fio->lyr_info != NULL) free(fio->lyr_info);
	return(_FFOPEN_ERR);
	}
