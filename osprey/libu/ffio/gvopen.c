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


#pragma ident "@(#) libu/ffio/gvopen.c	92.2	06/29/99 13:16:47"

#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <ffio.h>
#include "gvio.h"
#include "fxlist.h"

DECLARE(V_XLIST);
struct xtr_s V_XLIST_P = { V_XLIST };

/*
 * The following table describes the record formats that belong to the
 * CLASS_V class.  They share the common characteristic that they all
 * delimit their blocks and/or records by means of a simple prefix code,
 * some sort of segmentation scheme, and never split a segment across a
 * block.  The table describes the parameters of these prefix control
 * words.  The BDW is the Block Descriptor Word.  The SDW, the Segment
 * Descriptor Word.  The SCC is the Segment Control Code.  The segment is
 * a construct common to all of the se formats.  A logical record consists
 * of one or more segments.  Each segment, if it is not always
 * a complete record, (such as IBM U format) has some sort of SCC code
 * telling whether it is the first, last, or middle segment, or a complete
 * record.
 * For the SDW description, the SCC * is considered to be a part
 * of the SDW, as it is usually (always?) contined within it.
 * The pos is the position
 * of the SCC (bit position from left) in the SDW.  The SDW len is
 * the total length of the SDW, including the SCC.
 */

static struct gen_vf _Vrec_def_tab[NUM_V_TYPES] = 

/*   BDW	  SDW		  SCC		*/
/* len,type	len,type	len,type,pos	*/
{
 { NONE, NONE,	NONE, NONE,	NONE, NONE, NONE}, /* illegal */
 { NONE, NONE,	NONE, NONE,	NONE, NONE, NONE}, /* U */
 { 32, BDW_IBM,	32, SDW_IBM,	8, SCC_IBM, 16	}, /* V */
 { 32, BDW_IBM,	32, SDW_IBM,	8, SCC_IBM, 16	}, /* VB */
 { 32, BDW_IBM,	32, SDW_IBM,	8, SCC_IBM, 16	}, /* VBS */

 { NONE, NONE,	NONE, NONE, 	NONE, NONE, NONE}, /* V_DSK */
 { NONE, NONE,	32, SDW_D,	NONE, NONE, NONE}, /* V_TP */
 { NONE, NONE,	16, SDW_V,	NONE, NONE, NONE}, /* V_TR */

 { NONE, NONE,	16, NONE, 	8, SCC_VMS, 0	}, /* S_DSK */
 { NONE, NONE,	48, SDW_D, 	8, SCC_VMS, 32	}, /* S_TP */
 { NONE, NONE,	32, SDW_V, 	8, SCC_VMS, 16	}, /* S_TR */
 { NONE, NONE,	32, SDW_D,	NONE, NONE, NONE}, /* NVE_D */
 { NONE, NONE,	40, SDW_NVE_S, 	8, SCC_NVE, 0	}  /* NVE_S */
};

/*
 * Note: length of VMS SCC codes is really 16 bits.  However,
 * for coding simplicity, the length is set at 8.  This is because
 * the SCCs are byte swapped.  Look at get_segment() in gvread.c
 * for more information.
 */

extern struct v_rec_limit_s _V_limits[];
/*
 * Initialize the state of a foreign dataset.  Allocate buffers
 * and initialize pointers.
 */

_ffopen_t
_gen_vopen(
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
	char *ptr;
	union spec_u *nspec;
	long recsize, blksize;	/* in bits */
	long rsz, mbs;		/* in bytes */
	int bdwlen, sdwlen;	/* bytes again... */
	_ffopen_t nextfio = _FFOPEN_ERR;
	int rtype;
	int ll_blocked;
	struct gen_vf *vf_info;
	struct ffsw dumstat;	/* Dummy status word */

/*
 *	convert 8-bit bytes to bits
 */
	rsz = spec->fld.recsize;
	mbs = spec->fld.mbs;
        rtype = spec->fld.recfmt;
/*
 *	check recfmt in range
 */
        if (rtype < 0 || rtype >= NUM_V_TYPES)
		_FFOPEN_ERETURN(stat, FDC_ERR_BADSPC, 0);
/*
 *	_chk_n_set should check these, but check 'em again, just for safety.
 *	We need to set up defaults anyway.
 */
	if (mbs != 0)
		if (mbs < _V_limits[rtype].min_mbs ||
		    mbs > _V_limits[rtype].max_mbs)
			{
			_SETERROR(stat, FDC_ERR_BADSPC, 0);
			goto badret;
			}
	if (rsz != 0)
		if (rsz < _V_limits[rtype].min_rsz ||
		    rsz > _V_limits[rtype].max_rsz)
			{
			_SETERROR(stat, FDC_ERR_BADSPC, 0);
			goto badret;
			}
	bdwlen = 0;
	sdwlen = 0;	/* bytes */
	switch(rtype)
		{
		case TR_IBM_V:
		case TR_IBM_VB:
			bdwlen = 4;
			sdwlen = 4;	/* bytes, for IBM */
				/* fall thru... */
		case TR_IBM_U:
			/* neither specified,  let both default*/
			if (rsz == 0 && mbs == 0) break;
			/* both specified */
			if (rsz != 0 && mbs != 0)
				{
				if (rsz + bdwlen > mbs)
					{
					_SETERROR(stat, FDC_ERR_BADSPC, 0);
					goto badret;
					}
				}
			/* one specified */
			else if (mbs != 0)
				rsz = mbs - bdwlen;
			/* if rs alone, let mbs default */

			rsz = rsz - sdwlen; /* chop SDW len */
			break;
		case TR_VMS_V_TR:
			sdwlen += 2;	/* 2 byte SDW on V V_TR */
		case TR_VMS_V_DSK:
			/* neither specified */
			if (rsz == 0 && mbs == 0) break;
			/* both specified */
			if (rsz != 0 && mbs != 0)
				{
				if (mbs < (rsz + sdwlen))
					{
					_SETERROR(stat, FDC_ERR_BADSPC, 0);
					goto badret;
					}
				}
			/* one specified */
			else if (mbs != 0)
				rsz = mbs - sdwlen;
			else /* rsz specified, mbs not */
				mbs = rsz + sdwlen;
			break;
		case TR_VMS_V_TP:
		case TR_NVE_D:
			/* neither specified */
			if (rsz == 0 && mbs == 0) break;
			/* both specified */
			if (rsz != 0 && mbs != 0)
				{
				if (rsz + sdwlen > mbs)
					{
					_SETERROR(stat, FDC_ERR_BADSPC, 0);
					goto badret;
					}
				}
			/* one specified */
			else if (mbs != 0)
				rsz = mbs - sdwlen;
			else /* rsz specified, mbs not */
				{
				if (rsz > _V_limits[rtype].def_mbs)
					mbs = rsz + sdwlen;
				}
			/* if rs < dflt mbs, let mbs default */
			break;

/*				Not much to do for S fmts */
		case TR_IBM_VBS:
		case TR_VMS_S_DSK:
		case TR_VMS_S_TR:
		case TR_VMS_S_TP:
		case TR_NVE_S:
			break;

		default:
			_SETERROR(stat, FDC_ERR_BADSPC, 0);
			goto badret;
		}
/*
 *	Set up defaults for those that survive.
 */
	if (mbs == 0)
		mbs = _V_limits[rtype].def_mbs;
	if (rsz == 0)
		rsz = _V_limits[rtype].def_rsz;
			
/*
 *	Limits checked, defaults set.  Convert to bits
 */
	recsize = rsz << 3;
	blksize = mbs << 3;

	fio->scc = SCCFULL;
	fio->lastscc = SCCFULL;
/*
 *	Internally, both blksize and recsize are in bits!
 */
	fio->maxrecsize = recsize;

	fio->maxblksize = blksize;
	fio->_ffbufsiz = DEFVBSIZE;	/* bit size of buffer */
	ptr = malloc((blksize >> 3) + 16);
	if (ptr == NULL) goto nomem;
/*
 *	Allocate private storage for V class handling.
 */
	vf_info = (struct gen_vf *)calloc(sizeof(struct gen_vf), 1);
	if (vf_info == NULL) goto nomem;
	fio->lyr_info = (char *)vf_info;
/*
 *	load up record characteristics
 */
	*vf_info = _Vrec_def_tab[rtype];

	SET_BPTR(fio->_base, CPTR2BP(ptr));
	fio->rwflag = POSITIN;
	fio->segbits = 0;
	fio->_cnt = 0;
	fio->_ptr = fio->_base;
/*
 *	Now, open the lower layers
 */
	nspec = spec;
	NEXT_SPEC(nspec);
	nextfio = _ffopen(name, flags, mode, nspec, stat, cbits, cblks, NULL,
			oinf);
	if (nextfio == _FFOPEN_ERR) goto badret;
	fio->fioptr = (struct fdinfo *)nextfio;

	XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_GETINFO,
		&vf_info->ffci, &dumstat);
	ll_blocked = vf_info->ffci.ffc_flags & FFC_REC;
	switch(rtype)
		{
		case TR_IBM_V:
		case TR_IBM_VB:
		case TR_IBM_VBS:
			/* either record or stream below is OK. */
			break;
		case TR_VMS_V_TR:
		case TR_VMS_S_TR:
			/* underlying blocking not allowed. */
			/* allow it for now... */
			break;
		case TR_IBM_U:
		case TR_VMS_V_DSK:
		case TR_VMS_V_TP:
		case TR_VMS_S_DSK:
		case TR_VMS_S_TP:
		case TR_NVE_D:
		case TR_NVE_S:
			if (ll_blocked == 0)
				{
				_SETERROR(stat, FDC_ERR_NOBDRY, 0);
				goto badret;
				}
			break;

		default:
			_SETERROR(stat, FDC_ERR_BADSPC, 0);
			goto badret;
		}

	DUMP_IOB(fio); /* debugging only */

#if	DEBUG
	GVTRACE(TRACE_OPEN)
		_xrt_putf("gvopen: maxrec=0x%x, maxblk=0x%x (bytes)\n",
			blksize >> 3, recsize >> 3);
#endif

	return(nextfio);

nomem:
	_SETERROR(stat, FDC_ERR_NOMEM, 0);
badret:
	if (nextfio != _FFOPEN_ERR)
		XRCALL(fio->fioptr, closertn) fio->fioptr, &dumstat);

	if (BPTR2CP(fio->_base) != NULL)
		free(BPTR2CP(fio->_base));
	if (fio->lyr_info != NULL)
		free(fio->lyr_info);

	return(_FFOPEN_ERR);
	}
