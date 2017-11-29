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


/* USMID @(#) libf/include/rnl.h	92.0	10/08/98 14:30:10 */


/*
 *	Namelist defines and structures
 */

/*
 * These types are output from nicv
 */

#define	NVLESS	077	/* Typeless	*/
#define NV32I	04027	/* Short integer*/
#define NV64I	04077	/* Integer	*/
#define NVREAL	06077	/* Real		*/
#define NVDOUB	06177	/* Double	*/

/* Types passed to nocv to indicate type of conversion */

#define WLESS	23
#define W32I	11
#define W64I	23
#define WREAL	23
#define WDOUB	40
#define WCOMP	50

#define MAXDIMS	10	/* Maximum number of dimensions for an array	*/
#ifdef KEY /* Bug 14479 */
#define MAXNAML	63	/* Maximum length of namelist name		*/
#else /* KEY Bug 14479 */
#define MAXNAML	31	/* Maximum length of namelist name		*/
#endif /* KEY Bug 14479 */

/* Error processing */

#define	RNL_ERROR	-3

#define RERR(css, n)	{	\
	if (errf) {		\
		Nreturn(IO_ERR);\
	}			\
	else			\
		_ferr(css,n);	\
}

#define RERR2(css, n,m) {	\
	if (errf) {		\
		Nreturn(IO_ERR);\
	}			\
	else			\
		_ferr(css,n,m);	\
}

#define RERR3(css, n,m,t) {	\
	if (errf) {		\
		Nreturn(IO_ERR);\
	}			\
	else			\
		_ferr(css,n,m,t);	\
}

#define	RNLERROR(s) {		\
	errno	= s;		\
	return(RNL_ERROR);	\
}

#define ISSPTB(c)	(((c) == ' ') || ((c) == '\t'))

/*
 *      MIN
 *              Prevent compiler warnings by removing any definition added
 *              previously by other header files.
 */
#undef MIN
#define MIN(a,b)        ((a) < (b) ? (a) : (b))


/*
 * MATCH(c,a,b) determines whether the bit for character 'c' is set.
 *
 * a[b] and a[b+1] are bit masks for each ASCII character 
 */

#define MATCH(c,a,b)	(a[(c >= 0x3f) ? b+1 : b] & (1 << (IND(c))))

/*
 * TOGGLE_CHAR sets or clears the bit for a particular character.
 *
 *	TOGGLE_CHAR(c,a,b)
 *
 *	where:	c	is the character
 *		cat	is the category
 *		val	set bit if != 0, clear bit if 0
 */
#define TOGGLE_CHAR(c,cat,val) { \
	if (val)							\
 		_MASKS[(c >= 0x3f) ? cat+1 : cat] |= (1 << (IND(c)));	\
	else								\
 		_MASKS[(c >= 0x3f) ? cat+1 : cat] &= ~(1 << (IND(c)));	\
}

/* IND computes the bit index of a character */

#define IND(c)	((c >= 0x3f) ? 0x7f - (unsigned)c : (unsigned)(0x40 - c - 1))

#define Nreturn(n) {	\
	ss	= n;	\
	goto ret;	\
}	

/* End of file processing */

#define NEND(css, n) {		\
	if (endf) {		\
		Nreturn(IO_END);\
	}			\
	else			\
		_ferr(css,n);	\
}

/* Logical equates */

#undef	TRUE
#define TRUE	(_btol(1))

#undef	FALSE
#define FALSE	(_btol(0))

/*
 * LGET and CMNTLGET return a character from the current record.
 * If we are at EOR, they call _rnl_fillrec() to read the next
 * record.  The two routines differ in the way they treat
 * comment characters: LGET does not look for comment characters.
 * It would be used when comment characters do not have a special
 * meaning (e.g., when inside a quoted string).  CMNTLGET
 * recognizes comment characters and treats them as EOR.
 */

#define LGET(x) { \
	if (!inptr->incnt)					\
		if (ss = _rnl_fillrec(cup, echoptr, inptr))	\
			return(ss);				\
	x	= (char) *inptr->inptr++;			\
	inptr->incnt--;						\
}

#define CMNTLGET(x) { \
	if (!inptr->incnt)					\
		if (ss = _rnl_fillrec(cup, echoptr, inptr))	\
			return(ss);				\
	x	= (char) *inptr->inptr++;			\
	if (MATCH(x, _MASKS, MRNLCOMM)) {			\
		x		= ' ';				\
		/* start new record next time*/			\
		inptr->incnt	= 1;				\
	}							\
	inptr->incnt--;						\
}

/* 
 * These are identical to LGET and CMNTLGET, except in the way they
 * treat end of file.  They are to be used from the main routine,
 * where eof must be handled according to how RNL was called.
 */

#define MAINLGET(x) { \
	if (!inptr->incnt)					\
		if (ss = _rnl_fillrec(cup, echoptr, inptr)) {	\
			if (ss == EOF) {			\
				NEND(css, FERDNLEF);			\
			}					\
			else {					\
				RERR(css, ss);			\
			}					\
		}						\
	x	= (char) *inptr->inptr++;			\
	inptr->incnt--;						\
}

#define MAINCMNTLGET(x) { \
	if (!inptr->incnt)					\
		if (ss = _rnl_fillrec(cup, echoptr, inptr)) {	\
			if (ss == EOF) {			\
				NEND(css, FERDNLEF);			\
			}					\
			else {					\
				RERR(css, ss);			\
			}					\
		}						\
	x	= (char) *inptr->inptr++;			\
	if (MATCH(x, _MASKS, MRNLCOMM)) {			\
		x		= ' ';				\
		inptr->incnt	= 1;				\
	}							\
	inptr->incnt--;						\
}

/* Indicies into _MASKS */

#define	MRNLDELIM	0
#define	MRNLREP		4
#define	MRNLSEP		6
#define	MRNLFLAG	8
#define	MRNLCOMM	10

struct Inpinfo {
	long	*inbuff;	/* Input records are read into this buffer, 
				 * starting at inbuff[1].  inbuff[0] is 
				 * reserved as a flag character to indicate
				 * when we need to echo input */
	long	*instart;
	long	*inptr;
	long	incnt;
};

typedef struct {			/* dimension structure */
	signed int lower : 32,		/* lower bound */
 		    span : 32;		/* span of the dimension */
} Dims;

typedef struct {		/* Variable descriptor - 1 per var/array */

	char	varname[MAXNAML+1];	/* name of variable */

	struct	{
		unsigned nels : 32,	/* number of elements */
		       unused :  1,
			 type :  4,	/* type (0 = typeless,
					 * 1 = 32 bit integer,
					 * 2 = 64 bit integer,
					 * 3 = real, 4 = double,
					 * 5 = complex,
					 * 6 = character, 9 = logical*/
		      munused :  2,	/* unused */
		       taskcm :  1,	/* task common flag (not used on X/Y) */
			ndims :  3,	/* number of dimensions */
		       newfmt :  3,	/* Indicates new namelist table format*/
					/* (not used on X-MP/Y-MP) */
			  lmf :  1,	/* local memory flag (not used on X/Y)*/
			 stkf :  1,	/* stack flag */
		       offdim : 16;	/* offset from base of Namelist */
					/* structure to dimension structure */
	} na;

	struct	{
#ifdef _ADDR64
		unsigned long  varaddr ;	/* address of the variable or array.
					 * If the variable is of type
					 * character, this is an offset from
					 * the base of the Namelist structure
					 */
#else
		unsigned reserved : 16,	/* reserved for use by loader */
			   lmaddr : 16,	/* if task common, address in local
					 * memory that contains address in
					 * common memory of the start of the
					 * task common block (unused on X/Y) */
			  varaddr : 32;	/* address of the variable or array.
					 * If the variable is of type
					 * character, this is an offset from
					 * the base of the Namelist structure
					 * to the character descriptor */
#endif
	} va;

} Nlentry;

typedef	struct	{			/* Namelist table */
	char	nlname[MAXNAML+1];	/* name of namelist group */
	Nlentry	nlvnames[1];		/* array of variable descriptors */
} Namelist;

/* External declarations */

extern	long	_wnlrecsiz;
extern	long	_BLNKSEP;
extern	long	_MASKS[];
extern	long	OUT_CHAR;
extern	long	OUT_ECHO;
extern	long	OUT_EQ;
extern	long	OUT_LINE;
extern	long	OUT_SEP;
extern	unum_t	_OUT_UNIT;
extern	long	_SKP_MESS;	
extern	long	_TYP_CONV;
