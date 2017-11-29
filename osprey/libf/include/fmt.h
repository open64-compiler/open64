/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* USMID @(#) libf/include/fmt.h	92.1	11/10/99 13:40:53 */

#include <cray/dopevec.h>
#include <cray/format.h>

/*
 *	Mode bits for calling NICV routine (used by rnly.c and wnly.c)
 */
 
#define MDPART  040000	/* D part of D/E/F/Gw.d is present */
#define MCFT    020000	/* not called in compiler scan mode */
#define MPLS    010000	/* force output of the '+' in front of positive #'s */
#define MEXPS    04000	/* E part of D/E/F/Gw.dEe is present */
#define MSEXP    02000	/* signed exponent requested */
#define MMAN     01000	/* a double precision number is passed */
#define MD        0400	/* D format */
#define MG        0200	/* G format */
#define ME        0100	/* E format */
#define MF         040	/* F format */
#define MI         020	/* I format */
#define MX         010	/* X format */
#define MO          04	/* O format */
#define MBZ         02	/* blanks treated as zeroes */
#define MBN         01	/* blanks treated as nulls (ignored) */

/*
 *	Constants
 */

/*
 * INVALID_INTLEN is the mask value which indicates that an internal 
 * length is not supported for some edit descriptor.
 */

#define INVALID_INTLEN	((signed char) -1)

/*
 * MAX_SUP_INTLEN is the largest supported internal length (measured in 
 * bytes).
 */

#define MAX_SUP_INTLEN  16	/* best if this is a power of 2 */

/*
 * For historical reasons, use the '$' names on CRI systems.
 */
#ifdef	_CRAY

#define _RCHK	$RCHK
#define _RNOCHK	$RNOCHK
#define _WCHK	$WCHK
#define _WNOCHK	$WNOCHK

#endif

/*
 *	Macros
 */

/*
 * These macros check an edit-descriptor against a data type.
 */

#if defined(TARG_IA32) || defined(TARG_X8664)
#define INVALID_WTYPE(op, type) (*(int *)(&_WCHK[type-1]) & (1 << (31-(op-1))))
#define INVALID_RTYPE(op, type) (*(int *)(&_RCHK[type-1]) & (1 << (31-(op-1))))
#else
#define INVALID_WTYPE(op, type) (*(int *)(&_WCHK[type-1]) & (1 << (op-1)))
#define INVALID_RTYPE(op, type) (*(int *)(&_RCHK[type-1]) & (1 << (op-1)))
#endif

/*
 *	Structures
 */

typedef struct fmtchk {
	unsigned int	  	:18;		/* right justify */
	unsigned int	q 	:1;
	unsigned int	z 	:1;
	unsigned int	r 	:1;
	unsigned int	o 	:1;
	unsigned int	l 	:1;
	unsigned int	i 	:1;
	unsigned int	g 	:1;
	unsigned int	f 	:1;
	unsigned int	es	:1;
	unsigned int	en	:1;
	unsigned int	e 	:1;
	unsigned int	d 	:1;
	unsigned int	b 	:1;
	unsigned int	a 	:1;
} fmtchk_t;

/*
 *	Externals
 */

extern signed char	_wr_ilchk[LAST_DATA_ED][MAX_SUP_INTLEN];
extern signed char	_rd_ilchk[LAST_DATA_ED][MAX_SUP_INTLEN];
extern signed char	_rw_mxdgt[LAST_DATA_ED][MAX_SUP_INTLEN];
extern fmtchk_t		_RCHK[DVTYPE_ASCII];
extern fmtchk_t		_RNOCHK[DVTYPE_ASCII];
extern fmtchk_t		_RCHK77[DVTYPE_ASCII];
extern fmtchk_t		_RCHK90[DVTYPE_ASCII];
extern fmtchk_t		_WCHK[DVTYPE_ASCII];
extern fmtchk_t		_WNOCHK[DVTYPE_ASCII];
extern fmtchk_t		_WCHK77[DVTYPE_ASCII];
extern fmtchk_t		_WCHK90[DVTYPE_ASCII];
