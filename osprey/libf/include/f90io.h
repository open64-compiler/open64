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


/* USMID @(#) libf/include/f90io.h	92.3	10/29/99 21:41:49 */


#ifndef _F90IO_H
#define _F90IO_H

/*******************************************************************************
 *
 *	This header file contains declarations of compiler-library interface 
 *	routines, constants, and information packets.
 *
 *	Header files which must be included in addition to this header file:
 *
 *		"fio.h"
 *
 ******************************************************************************/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <cray/dopevec.h>
#include "util/utildefs.h"

/*******************************************************************************
 *
 *	Constants
 *
 ******************************************************************************/

/*
 *	The following values are returned by the single call I/O interface
 *	routines for use in condition handling, i.e., END=, ERR=, and EOR= 
 *	specifiers in I/O statements.
 */

#define IO_OKAY 0	/* Normal completion	*/
#define IO_ERR	1	/* Error status		*/
#define IO_END	2	/* End status		*/
#define IO_EOR	3	/* End of record status */

/*******************************************************************************
 *
 *	General structures 
 *
 ******************************************************************************/

/*
 *	gfptr_t represents all forms of pointers to Fortran data.
 */

typedef union {
	_fcd		fcd;		/* Fortran character descriptor */
	void		*v;		/* pointer to byte address */
	_f_int		*wa;		/* pointer to word address */
	struct DopeVector *dv;		/* pointer to dope vector */
} gfptr_t;


extern int _cntig_chk(DopeVectorType *dv,
           void **newar,
           int *nocontig,
           long *extent,
           long *nbytes);

/*******************************************************************************
 *
 *	Packets for OPEN, CLOSE, INQUIRE, BUFFER IN, and BUFFER OUT
 *
 ******************************************************************************/

/*
 *	In open_spec_list, inquire_spec_list, close_spec_list structures,
 *	some common conventions exist.
 *
 *	1) all specifier fields are in the order as listed in the ANSI standard
 *
 *	2) For fields of type (_f_int *), NULL implies that the specifier is 
 *	not passed.  For fields x of type _fcd, the absence of the specifier is
 *	implied when _fcdtocp(x) == NULL, 
 *
 *	3) All version numbers are 0 in initial release.  The numbers are
 *	incremented when incompatible changes are made to the structures.
 */

/*
 *	open_spec_list is the interface packet passed to _OPEN.
 */
struct open_spec_list {
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned int		:32;	/* reserved */
#endif
	unsigned int		:24;	/* reserved */
	unsigned int	version :8;	/* version number */
	_f_int	*unit;
	_f_int	*iostat;
	long	err;		/* -1 if ERR= specified; 0 otherwise	*/
	_fcd	file;
	_fcd	status;
	_fcd	access;
	_fcd	form;
	_f_int	*recl;
	_fcd	blank;
	_fcd	position;
	_fcd	action;
	_fcd	delim;
	_fcd	pad;
};
	
/*
 *	inquire_spec_list is the interface packet passed to _INQUIRE.
 */
struct inquire_spec_list {
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned int		:32;	/* reserved */
#endif
	unsigned int		:24;	/* reserved */
	unsigned int	version :8;	/* version number */
	_f_int	*unit;
	_fcd	file;
	_f_int	*iostat;
	long	err;		/* -1 if ERR= specified; 0 otherwise	*/
	_f_log	*exist;
	_f_log	*opened;
	_f_int	*number;
	_f_log	*named;
	_fcd	name;
	_fcd	access;
	_fcd	sequential;
	_fcd	direct;
	_fcd	form;
	_fcd	formatted;
	_fcd	unformatted;
	_f_int	*recl;
	_f_int	*nextrec;
	_fcd	blank;
	_fcd	position;
	_fcd	action;
	_fcd	read;
	_fcd	write;
	_fcd	readwrite;
	_fcd	delim;
	_fcd	pad;
};
	
/*
 *	close_spec_list is the interface packet passed to _OPEN.
 */
struct close_spec_list {
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned int		:32;	/* reserved */
#endif
	unsigned int		:24;	/* reserved */
	unsigned int	version :8;	/* version number */
	_f_int	*unit;
	_f_int	*iostat;
	long	err;		/* -1 if ERR= specified; 0 otherwise */
	_fcd	status;
};

/*
 *	bio_spec_list is the interface packet passed to _BUFFERIN and 
 *	_BUFFEROUT.
 */
struct bio_spec_list {
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned int		:32;	/* reserved */
#endif
	unsigned int		:24;	/* reserved */
	unsigned int	version :8;	/* version number */
	_f_int		*unit;		/* Unit */
	_f_int		*recmode;	/* Mode */
	gfptr_t		bloc;		/* Beginning location */
	gfptr_t		eloc;		/* Ending location */
	f90_type_t	*tiptr;		/* Data type word */
};

/*******************************************************************************
 *
 *	Packets for _FRF, _FWF, _FRU, and _FWU
 *
 ******************************************************************************/

/*
 *	The cilist describes all specifiers passed to a READ or WRITE
 *	statement.
 */

#ifndef CILIST_VERSION
#define CILIST_VERSION	1		/* cilist version number */
#endif

typedef struct ControlList {
	unsigned int	version	:8;	/* contains CILIST_VERSION */
	enum uflag_spec {
		CI_UNITNUM 	= 0,	/* Unit number			*/
		CI_UNITASTERK	= 1,	/* Asterisk			*/
		CI_UNITCHAR	= 2,	/* Character variable (_fcd) 	*/
					/* internal file or ENCODE/DECODE */
		CI_UNITDOPEVEC	= 3	/* character array 		*/
	}		uflag	:8;	/* type of unit identifier */
	unsigned int		:4;	/* unused */
	unsigned int	iostatflg:1;	/* iostat= present flag */
	unsigned int	eorflag	:1;	/* eor= present flag */
	unsigned int	endflag	:1;	/* end= present flag */
	unsigned int	errflag	:1;	/* err= present flag */
	unsigned int		:2;	/* unused */
	enum advan_spec {
		CI_ADVYES 	= 0,	/* ADVANCE=YES or not specified */
		CI_ADVNO 	= 1,	/* ADVANCE=NO 			*/
		CI_ADVVAR	= 2	/* ADVANCE=variable 		*/
	}		advcode :3;	/* ADVANCE= specifier value 	*/
	unsigned int	edcode	:1;	/* 1 if ENCODE/DECODE flag */
	unsigned int	internal :1;	/* 1 if internal file */
					/* must be 1 if edcode is 1 */
	unsigned int	dflag	:1;	/* 1 if direct access */
	enum fmtflag_spec {
		CI_LISTDIR	= 0,	/* List-directed formatting	*/
		CI_EDITCHAR	= 1,	/* Format in character variable	*/
		CI_EDITCHARAY	= 2,	/* Format in char array section	*/
		CI_EDITHOL	= 3,	/* Format in Hollerith		*/
		CI_EDITHOLARAY	= 4,	/* Format in Hollerith array	*/
		CI_NAMELIST	= 5	/* Namelist formatting 		*/
	}	 	fmt	:8;	/* type of format (or list-directed) */
	unsigned int	stksize	:8;	/* size in words of stack space	*/
					/* passed as 3rd arg to		*/
					/* _FRF/_FWF/_FRU/_FWU		*/
	unsigned int		:8;	/* unused */
	unsigned int	icount	:8;	/* size of struct control list in */
					/* words */
	
	gfptr_t 		unit;	/* pointer to unit */

	_f_int		*iostat_spec;	/* address of IOSTAT= variable */
	_f_int		*rec_spec;	/* address of REC= variable */
	struct fmt_entry *parsfmt;	/* pointer to parsed fmt */

	gfptr_t		fmtsrc;		/* pointer to format text */

	_fcd		advance_spec;	/* addr of ADVANCE= variable */
	_f_int		*size_spec;	/* addr of SIZE= variable */
} ControlListType;

/*
 *	The IO item list passed to a Fortran 90 single call data transfer (IO) 
 *	interface routine can take any of these forms:
 *
 *	One IO item list is passed with each call to an interface routine.
 *	IO item lists from a sequence of one or more library calls are needed 
 *	to completely process each data transfer (READ or WRITE) statements.
 *
 *
 *	An IO item list has the following structure (using grammar notation):
 *
 *		IO-item-list is
 *			iolist_header compound-item
 *
 *		compound-item is
 *			compound-item iolist-item
 *		     or	iolist-item
 *
 *		iolist-item is	
 *			ioentry_header ioscalar_entry
 *		     or	ioentry_header ioarray_entry
 *		     or	ioentry_header implieddo-item 
 *
 *		implieddo-item 	is
 *			ioimplieddo_entry iolist_header compound-item
 */

#ifndef IOLIST_VERSION
#define IOLIST_VERSION	1		/* current iolist version number */
#endif

/*
 *	iolist_header is the first structure of an I/O item list.  One I/O
 *	item list is passed with each call to a compiler-library interface
 *	routine.  Several calls, and several I/O item lists, may be needed
 *	to completely process a data transfer (READ or WRITE) statement.
 *
 *	iolist_header is also passed immediately following the ioimplieddo_entry
 *	in an implied do control list.
 */

typedef struct {
	unsigned int	version	:3;	/* contains IOLIST_VERSION */
	unsigned int		:27;	/* unused */

	/*
	 * Iolist table entry bits indicate whether data transfer statement
	 * contains more than one iolist table.  If iolfirst=iollast=1, then
	 * table is entire iolist.  If iolfirst=iollast=0, then table is
	 * middle iolist table.
	 */

	unsigned int	iolfirst:1;	/* 1 if first IO item list for current*/
					/* statment IO statement */
	unsigned int	iollast	:1;	/* 1 if last IO item list for current */
					/* statment IO statement */
	unsigned int	icount	:16;	/* number of iolist-items in this */
					/* IO item list.  If zero and it is */
					/* both first and last io list, there */
					/* is no io list in statement */
	unsigned int	ioetsize:16;	/* number of words in the current */
					/* IO item list, including this */
					/* iolist_header */
					/* On SGI systems, in 32-bit mode */
					/* this is the number of 32-bit */
					/* words, and in 64-bit mode this is */
					/* the number of 64-bit words. */
} iolist_header;

/*
 *	ioentry_header describes the type of iolist item.
 */
typedef struct {
	enum valtype_spec {
		IO_VALUNUSED	= 0,
		IO_SCALAR	= 1,	/* scalar */
		IO_DOPEVEC	= 2,	/* dopevector */
		IO_LOOP		= 3,	/* implied-DO loop */
		IO_STRUC_A	= 4,	/* struc for namelist array */
		IO_STRUC_S	= 5	/* struc for namelist scalar */
	}		valtype	:8;	/* type of iolist entry */
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned int		:24;	/* unused */
	unsigned int		:16;	/* unused */
#else
	unsigned int		:8;	/* unused */
#endif
	unsigned int	ioentsize:16;	/* number of words of the current */
					/* iolist item, including this */
					/* ioentry_header */
					/* On SGI systems, in 32-bit mode */
					/* this is the number of 32-bit */
					/* words, and in 64-bit mode this is */
					/* the number of 64-bit words. */
} ioentry_header;

/*
 *	ioscalar_entry describes a scalar IO list item.  Pointers to scalars
 *	are processed with the ioarray_entry type of IO list item.
 */
typedef struct {
	f90_type_t	tinfo;		/* type information for variable */
	gfptr_t		iovar_address;	/* pointer to variable */
} ioscalar_entry;

/*
 *	ioarray_entry describes an array section IO list item.  It contains an
 *	implied-DO multiplier address for each dimension of the array iff 
 *	indflag is set.
 */
typedef struct {
	struct DopeVector	*dv;	/* pointer to dope vector */
	unsigned int	indflag	:1;	/* 1 if indexed array */ 
	unsigned int	boundchk:1; 	/* Array bounds checking flag */
					/* Not used for F90 release 1. */
					/* 0=no bounds checking on array */
					/* 1=bounds checking on array */

	unsigned int		:30;	/* pad to end of word */
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned int		:32;
#endif

	int		*dovar[MAXDIM];	/* array of pointers to indices.  A */
					/* NULL index pointer references the */
					/* entire extent of a dimension. */
} ioarray_entry;

/*
 *	ioimplieddo_entry describes an implied-DO loop IO list item.  This
 *	structure is followed by idcount iolist-items, which each consist
 *	of an ioentry_header structure followed by an ioscalar_entry,
 *	an ioarray_entry structure, or a nested implieddo-item.
 */
typedef struct {
	int		*ioloopvar; 	/* address of loop variable */
	int		*iobegcnt;	/* address of beginning count of loop */
	int		*ioendcnt;	/* address of ending count of loop */
	int		*ioinccnt;	/* address of increment of loop */
} ioimplieddo_entry;

/******************************************************************************
 *
 *	Inline function definitions.
 *
 ******************************************************************************/

/*
 *	_is_nonadv returns
 *
 *		 0	ADVANCE='YES'
 *		 1	ADVANCE='NO'
 *		-1	invalid ADVANCE= specifier
 */
_PRAGMA_INLINE(_is_nonadv)
static int
_is_nonadv(ControlListType *cilist)
{
	if (cilist->advcode == CI_ADVYES) {
		return(0);				/* ADVANCE='YES' */
	}
	else if (cilist->advcode == CI_ADVNO) {
		return(1);				/* ADVANCE='NO' */
	}
	else {				/* (cilist->advcode == CI_ADVVAR) */
		if (_string_cmp("YES", _fcdtocp(cilist->advance_spec),
			_fcdlen(cilist->advance_spec)))
			return(0);			/* ADVANCE='YES' */
		else if (_string_cmp("NO", _fcdtocp(cilist->advance_spec),
			_fcdlen(cilist->advance_spec)))
			return(1);			/* ADVANCE='NO' */
	}
	return(-1);
}

/*
 *	setup_format	Initialize unit table fields and obtain the
 *			parsed format.
 *
 *	Returns
 *		 0	on normal return
 *		>0	error status
 */
_PRAGMA_INLINE(setup_format)
static int
setup_format(
	struct fiostate *css,
	unit		*cup,
	ControlListType *cilist)
{
	register long	flen;
	register int	fnum;
	register int	stsz;
	char		*fptr;		/* Pointer to unparsed format */
	fmt_type	*ppfmt;		/* Pointer to parsed format */

	/*
	 * For formats passed as hollerith (integer) variables,
	 * cft90 guarantees that they are terminated by a zero byte.
	 * We use strlen() to obtain the length.
	 *
	 * For static formats (FORMAT statements) or formats
	 * which are character constants or simple character
	 * variables, the length of the format is the length of
	 * the character string.
	 *
	 * For formats passed as character or Hollerith arrays, the
	 * length of the format is the length of the entire array.
	 * We compute this by multiplying the length of the element
	 * passed times the dimension of the array.
	 */

	switch (cilist->fmt) {

	case CI_EDITCHAR:		/* character variable */
		fptr	= _fcdtocp(cilist->fmtsrc.fcd);
		flen	= _fcdlen (cilist->fmtsrc.fcd);
		break;

	case CI_EDITCHARAY:		/* dopevector */
	case CI_EDITHOLARAY:
		{
		register int	errn;
		int		nocontig = 0;
		long		extent = 0;
		long		nbytes = 0;
		void		*newar;
		DopeVectorType	*dv = cilist->fmtsrc.dv;

		if (dv->p_or_a && (dv->assoc == 0))
			_ferr(css, FEFMTPAL); /* array or ptr not alloc/assoc */

		/* Check for contiguous array */

		errn	= _cntig_chk(dv, &newar, &nocontig, &extent, &nbytes);

		if (errn > 0)
			_ferr(css, errn);	/* No memory available */

		css->u.fmt.freefmtbuf = nocontig;

		fptr	= (nocontig) ? newar : _fcdtocp(dv->base_addr.charptr);

		 /* Zero length array or character is bad format */

		if (extent == 0)
			_ferr(css, FEFMTNUL);

		/*
		 * flen is the element length in bytes times the number
		 * of elements in the array
		 */

		flen	= nbytes;
		break;
		}

	case CI_EDITHOL:		/* Null-terminated hollerith */
		fptr	= (char *) cilist->fmtsrc.wa;
		flen	= (long) strlen(fptr);
		break;

	default:
		_ferr(css, FEINTUNK);	/* Deep weeds... */
	}

	/*
	 * For compatibility with ancient compilers, pull an optional
	 * statement number off of the beginning of the format and save
	 * it.  If a statement number is found, update the format string
	 * pointer and length.  Someday, Obi-wan, we'll do this only for
	 * static formats; or not at all.
	 */

	fnum	= 0;

	while (isdigit(*fptr) && flen-- > 0)
		fnum	= (fnum << 3) + (fnum << 1) +
			  ((int) *fptr++ - (int) '0');

	css->u.fmt.u.fe.fmtbuf	= fptr;
	css->u.fmt.u.fe.fmtlen	= flen;
	css->u.fmt.u.fe.fmtnum	= fnum;

	/*
	 * If the format is a variable format, or if it has not yet
	 * been parsed, or if it was parsed by an incompatible version
	 * of the format parser, then parse it.
	 */

	ppfmt	= cilist->parsfmt;

	if (ppfmt == NULL || ppfmt->offset != PARSER_LEVEL) { /* not parsed */
		register int	errn;

		errn	= _parse(css, cup, (fmt_type **) ppfmt);

		/*
		 * If the parsed format was of an old version, store the
		 * new version of the parsed format in the cilist for 
		 * subsequent executions of this I/O statement.
		 */

		if (ppfmt != NULL)
			cilist->parsfmt	= ppfmt;

		if (errn != 0)
			return(errn);
	}
	else
		css->u.fmt.u.fe.pfmt	= ppfmt;

	/*
	 * Ensure that the format count stack is allocated and is
	 * large enough to accomodate the maximum nesting depth of
	 * this format.
	 */

	stsz	= css->u.fmt.u.fe.pfmt->rep_count;

	if (stsz > cup->upfcstsz) {

		cup->upfcstsz	= stsz;		/* Set new depth */

		if (cup->upfcstk != NULL)
			free(cup->upfcstk);	/* Free old stack */

		cup->upfcstk	= (int *) malloc(sizeof(int) * stsz);

		if (cup->upfcstk == NULL) 
			return(FENOMEMY);	/* No memory */

	}

	css->u.fmt.u.fe.pftocs	= cup->upfcstk;	/* Set top of count stack */

	/* Skip first entry of parsed format */

	css->u.fmt.u.fe.pfcp	= css->u.fmt.u.fe.pfmt + 1;

	/* Set initial repeat count */

	*css->u.fmt.u.fe.pftocs	= css->u.fmt.u.fe.pfcp->rep_count;

	return(0);
}

/******************************************************************************
 *
 *	Function prototypes and declarations.
 *
 ******************************************************************************/

extern int _FRF(ControlListType *cilist, iolist_header *iolist, void *stck);
extern int _FWF(ControlListType *cilist, iolist_header *iolist, void *stck);
extern int _FRU(ControlListType *cilist, iolist_header *iolist, void *stck);
extern int _FWU(ControlListType *cilist, iolist_header *iolist, void *stck);
extern int _OPEN(struct open_spec_list *osl);
extern int _CLOSE(struct close_spec_list *csl);
extern int _INQUIRE(struct inquire_spec_list *isl);
extern void _BUFFERIN(struct bio_spec_list *bisl);
extern void _BUFFEROUT(struct bio_spec_list *bosl);

extern int _xfer_iolist(FIOSPTR css, unit *cup, iolist_header *iolist,
			xfer_func *func);

/******************************************************************************
 *
 *	External symbols
 *
 ******************************************************************************/

typedef enum valtype_spec entrycode_t;	/* io or namelist entry codes	*/

#endif /* !_F90IO_H */
