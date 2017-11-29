/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/rb.c	92.2	06/21/99 10:37:55"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <cray/nassert.h>
#ifdef	_CRAYT3D
#include <cray/mppsdd.h>
#endif
#include "fio.h"
#include "f90io.h"

static void
_rb(FIOSPTR css, unit *cup, _f_int *recmode, gfptr_t bloc, gfptr_t eloc,
	type_packet *tip);

#ifdef	_CRAYT3D
#define MAXSH		4096
#else
#define	MAXSH		1
#endif

#ifdef	_UNICOS
/*
 *	$RB$	CFT77 BUFFER IN wrapper
 */

void
$RB$(
	_f_int	*biunit,	/* Unit			*/
	_f_int	*recmode,	/* Mode			*/
	gfptr_t	bloc,		/* Beginning location	*/
	gfptr_t	eloc,		/* Ending location	*/
	int	*type)		/* Data type		*/
{
	register short	type77;
	register unum_t	unum;
	type_packet	tip;
	struct f90_type	ts;
	unit		*cup;
	struct fiostate cfs;

	unum	= *biunit;

	STMT_BEGIN(unum, 0, T_BUFIN, NULL, &cfs, cup);
/*
 *	If not connected, do an implicit open.  Abort if the open fails.
 */
	if (cup == NULL)
		cup	= _imp_open77(&cfs, SEQ, UNF, unum, 0, NULL); 

	type77		= *type & 017;

	CREATE_F90_INFO(ts, tip, type77);

#if	NUMERIC_DATA_CONVERSION_ENABLED

	if (cup->unumcvrt || cup->ucharset) {
		register int	ret;

		ret	= _get_dc_param(&cfs, cup, ts, &tip);

		if (ret != 0)
			_ferr(&cfs, ret);
	}

#endif

_PRAGMA_INLINE(_rb)
	_rb(&cfs, cup, recmode, bloc, eloc, &tip);

	return;
}
#endif	/* _UNICOS */

/*
 *	_BUFFERIN	f90 BUFFER IN wrapper (also a headache remedy)
 */
void
_BUFFERIN(struct bio_spec_list *bisl)
{
	register unum_t	unum;
	type_packet	tip;
	struct f90_type	ts;
	unit		*cup;
	struct fiostate	cfs;

	assert ( bisl->version == 0 );

	unum	= *bisl->unit;
	ts	= *bisl->tiptr;

	STMT_BEGIN(unum, 0, T_BUFIN, NULL, &cfs, cup);
/*
 *	If not connected, do an implicit open.  Abort if the open fails.
 */
	if (cup == NULL)
		cup	= _imp_open(&cfs, SEQ, UNF, unum, 0, NULL); 

	tip.type77	= -1;
	tip.type90	= ts.type;
	tip.intlen	= ts.int_len;
	tip.extlen	= ts.int_len;
	tip.elsize	= ts.int_len >> 3;
	tip.stride	= 1;

#if	NUMERIC_DATA_CONVERSION_ENABLED

	if (cup->unumcvrt || cup->ucharset) {
		register int	ret;

		ret	= _get_dc_param(&cfs, cup, ts, &tip);

		if (ret != 0)
			_ferr(&cfs, ret);
	}

#endif

_PRAGMA_INLINE(_rb);
	_rb(	&cfs, cup, bisl->recmode, bisl->bloc, bisl->eloc, &tip);

	return;
}

/*
 *	_RB	"Old" f90 BUFFER IN wrapper (not used by f90 2.0 and
 *		later compilers).  This routine can be deprecated one
 *		of these millenia.
 */
void
_RB(
	_f_int		*biunit,	/* Unit			*/
	_f_int		*recmode,	/* Mode			*/
	gfptr_t		bloc,		/* Beginning location	*/
	gfptr_t		eloc,		/* Ending location	*/
	f90_type_t	*tiptr)		/* Data type word	*/
{
	struct bio_spec_list	bsl;

	bsl.version	= 0;
	bsl.unit	= biunit;
	bsl.recmode	= recmode;
	bsl.bloc	= bloc;
	bsl.eloc	= eloc;
	bsl.tiptr	= tiptr;

	_BUFFERIN(&bsl);

	return;
}

static void 
_rb(
	FIOSPTR		css,		/* Current Fortran I/O state	*/
	unit		*cup,		/* Unit pointer			*/
	_f_int		*recmode,	/* Mode				*/
	gfptr_t		bloc,		/* Beginning location		*/
	gfptr_t		eloc,		/* Ending location		*/
	type_packet	*tip)		/* Type information packet	*/
{
	register int	bytshft;
	register int	mode;
	register long	bytes;
	register long	elsize;
	register long	itemlen;
	register long	items;
	register long 	stat;
	register ftype_t type90;
	int		state;
	char		*uda, *udax;
#ifdef	_CRAYT3D
	register short	shared;
	register long	ntot;	
	register long	numleft;	
	long		shrd[MAXSH];
#endif

	if (cup->useq == 0)	/* If direct access file */
		_ferr(css, FEBIONDA, "BUFFER IN");

	if (cup->ufmt)		/* If formatted file */
		_ferr(css, FEBIONFM, "BUFFER IN");

	if (cup->uerr && !cup->unitchk)
		_ferr(css, cup->uffsw.sw_error);

/*
 *	This check taken out temporarily because we'd like to be able to
 *	follow an ENDFILE statement or a READ which encounters an endfile
 *	record with a BUFFER IN statement.  The sticky EOF principle should
 *	permit such a BUFFER IN to simply return an EOF status.  But what
 *	really happens is the preceding ENDFILE or READ statement sets 
 *	cup->uend, triggering an error here.  We really need a flag to
 *	store the status of the previous BUFFER IN/OUT statement which is
 *	separate from cup->uend.
 *
 *	if (cup->uend && !cup->unitchk)
 *		_ferr(css, FERDPEOF);
 */

	cup->unitchk	= 0;
	cup->uerr	= 0;
	elsize		= tip->elsize;	/* Data size in bytes */
	type90		= tip->type90;

/*
 *	Adjust the word count depending on the type.
 */
	bytshft	= ((sizeof(elsize) << 3) - 1) - _leadz(elsize); /* log2(elsize) */

	if (type90 == DVTYPE_ASCII) {	/* If character item */
		uda	= _fcdtocp(bloc.fcd);
		udax	= _fcdtocp(eloc.fcd);
		itemlen	= _fcdlen (eloc.fcd);
	}
	else {
#ifdef	_CRAYT3D
		shared	= 0;

		if (_issddptr(bloc.v)) {
			int	*tmpptr;

			/* Shared data */

			if (!_issddptr(eloc.v)) {
				_ferr(css, FEINTUNK);
			}

			shared	= 1;
			ntot	= 0;

			if ((cup->ufs == FS_FDC) && 
				(cup->uflagword & FFC_ASYNC)) {
				/* When we can do I/O from shared memory */
				/* we can support this. */
				_ferr(css, FESHRSUP);
			}
/*
 * When compiler spr 76429 (on T3D) is closed, we can try replacing 
 * the lines that use tmpptr with this.
 *			items	= _sdd_read_offset((void *)eloc.v) -
 *				_sdd_read_offset((void *)bloc.v) + 1;
 */
			uda	= bloc.v;	/* temporary */
			udax	= eloc.v;
			tmpptr	= (int *)((int)udax & 0x7fffffffffffffff);
			items	= *(tmpptr + 1);
			tmpptr	= (int *)((int)uda & 0x7fffffffffffffff);
			items	= items - *(tmpptr + 1) + 1;
		}
		else
#endif	/* _CRAYT3D */
		{
			uda	= bloc.v;
			udax	= eloc.v;
		}

		itemlen	= elsize;
	}

#ifdef	_CRAYT3D
	if (shared) {
		bytes	= items << bytshft;
	}
	else 
#endif
	{
		bytes	= (udax - uda) + itemlen;
		items	= bytes >> bytshft;
	}

	if (bytes < 0)
		_ferr(css, FEBIOFWA, "BUFFER IN");

	mode		= (*recmode < 0) ? PARTIAL : FULL;
	cup->urecmode	= mode;
	cup->uwrt	= 0;
	state		= CNT;

	if ((items << bytshft) != bytes)
		_ferr(css, FEBIOFWD);

#ifdef	_CRAYT3D
	if ( !shared && cup->uasync ) {
#else
	if (cup->uasync) {
#endif
		int	ubc = 0;

		WAITIO(cup, _ferr(css, cup->uffsw.sw_error));

#if	defined(_UNICOS) || defined(NUMERIC_DATA_CONVERSION_ENABLED)
/*
 *		Pad word-aligned numeric data on word boundaries within
 *		the record for CRI and some foreign data formats. 
 */
		if ((cup->urecpos & cup->ualignmask) != 0 &&
		    type90 != DVTYPE_ASCII && 
		    elsize > 4 ) {
			int		padubc;
			register int	pbytes;
			int		padval;

			COMPADD(cup, pbytes, padubc, padval);

			if (pbytes != 0) {
				stat	= XRCALL(cup->ufp.fdc, readrtn)
						cup->ufp.fdc,
						WPTR2BP(&padval),
						pbytes,
						&cup->uffsw,
						PARTIAL,
						&padubc);
				if (stat != pbytes ||
				    FFSTAT(cup->uffsw) != FFCNT) {
					cup->uerr	= 1;
					goto badpart;
				}
				cup->urecpos	+= (stat << 3) - padubc;
			}
		}
#endif	/* _UNICOS || NUMERIC_DATA_CONVERSION_ENABLED */

		CLRSTAT(cup->uffsw);		/* clear status word */
		FFSTAT(cup->uffsw)	= 0;	/* flag no status */

		stat	= XRCALL(cup->ufp.fdc, readartn) cup->ufp.fdc,
					CPTR2BP(uda),
					bytes,
					&cup->uffsw,
					mode,
					&ubc);

		cup->uasync	= ASYNC_ACTIVE;	/* flag last op was async */

		if (stat < 0)
			cup->uerr	= 1;
	}
	else {
#ifdef	_CRAYT3D
		register long	chunk;

		if (shared) {
			chunk	= (MAXSH / elsize) * sizeof(long);
			uda	= (char *)shrd;
			numleft	= items;
		}
		do {
			if (shared) {
				items	= MIN(chunk, numleft);
				numleft	= numleft - items;

				if (numleft == 0)
					mode	= cup->urecmode;
				else
					mode	= PARTIAL;
			}
#endif

			tip->count	= items;
		
			stat	= _frwd(cup, uda, tip, mode, (int *) NULL,
					(long *) NULL, &state);

#ifdef	_CRAYT3D
			if (stat > 0)
				if (shared) {
					_cpytosdd(bloc.v, (long *)uda, stat,
						elsize / sizeof(long), 1, ntot);	
					ntot	= ntot + stat;
				}

		} while (shared && (stat == items) && (numleft > 0));
#endif

		cup->ulrecl	= 0;

		if ( stat == IOERR ) {
			cup->uerr		= 1;
			cup->uffsw.sw_error	= errno;
		}
		else if (state == EOF) 
			cup->uend	= PHYSICAL_ENDFILE;
		else if (state == EOD) {
			if (cup->uend == 0)
				cup->uend	= LOGICAL_ENDFILE;
		}
		else {
			/*
			 * Set ulrecl to returned item count -> bits
			 */
#ifdef	_CRAYT3D
			if (shared)
				cup->ulrecl	= ntot << (bytshft + 3);
			else
#endif
				cup->ulrecl	= stat << (bytshft + 3);

			cup->uend	= BEFORE_ENDFILE;
		}

	}

/*
 *	If mode is FULL or status is BOD, EOR, EOF, EOD, ERR then
 *	clear ulastyp to avoid padd on next operation.
 */
	cup->ulastyp	= type90;

	if (cup->urecmode == FULL || state != CNT) {
badpart:
		cup->ulastyp	= DVTYPE_TYPELESS;
		cup->urecpos	= 0;
	}

#ifdef	_CRAYT3D
	if (shared)
		_remote_write_barrier();
#endif

	STMT_END(cup, T_BUFIN, NULL, css);

	return;
}
