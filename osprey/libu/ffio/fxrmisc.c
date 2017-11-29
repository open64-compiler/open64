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


#pragma ident "@(#) libu/ffio/fxrmisc.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <fdcconfig.h>
#include "cdcio.h"
#include "gxio.h"
#include "gvio.h"
#include "gfio.h"

/* define this here to avoid loading stuff */
/* Note that the value is negative, but not -1.  */
#if defined(_WORD32) || (_MIPS_SZINT == 32)
int __fdctrace_enable = (signed)0xC0000000;
#else
int __fdctrace_enable = 0xC000000000000000;
#endif


/*
 * The following is a variable that is also a FORTRAN common block
 * by the same name.  It is intended to allow the user to break out
 * of the loop we always execute around I/O syscalls.
 * It is preset to zero, which means to re-issue syscalls when
 * interrupted.  If the user sets it to one, the re-issue of syscalls
 * will not take place.
 */
#ifdef	_CRAY
int G@INTIO = 0;
#else
int _intio = 0;
#endif


/*
 * The following tables contain various parameters and information necessary
 * for opening and processing ffio files
 */

/*
 * The following serve to define the record formats for the
 * CLASS_V, and CLAS_CDC (IW and CW) formats.
 */

int _scc_tab[NUM_SCC_TYPES] [4] =
		{
			{ ISCCFULL, ISCCFRST, ISCCLAST, ISCCMIDL },
			{ VSCCFULL, VSCCFRST, VSCCLAST, VSCCMIDL },
			{ CSCCFULL, CSCCFRST, CSCCLAST, CSCCMIDL },
			{ NVSCCFULL,NVSCCFRST,NVSCCLAST,NVSCCMIDL }
		};
/*
 * Parameters for FDC record types.
 */

#define INCODE	-2
#if defined(_WORD32) || (_MIPS_SZINT == 32)
#define NOMAX	0xFFFFFF	/* big integer */
#else
#define NOMAX   0xFFFFFFFFFFF	/* big integer */
#endif


/*
 *	The following are limits that are imposed on the lower end by logical
 *	necessity.  For example, VMS D format must have a block size of at
 *	least 5 in order to put at least one data byte in each block.
 *
 *	The max numbers are the foreign numbers imposed on the foreign systems.
 *
 *	The default numbers are also derived from the native environment, and
 *	match, to the extent possible, the default behavior of that environment.
 *
 *	Some limits include the control bytes, and some do not.
 *	To the extent possible, this choice depends on the behavior of the
 *	number on the native system.
 */
struct f_rec_limit_s
_F_limits[] =
/*	    record size		   max block size  	*/
/*	 min	max	def	min	max	def	*/
{
 {	0,	0,	0,	0,	0,	0	}, /* illegal */
 {	1,	32760,	INCODE,	1,	32760,	INCODE	}, /* IBM F */
 {	1,	32760,	INCODE,	1,	32760,	INCODE	}, /* IBM FB */
 {	1,	65536,	INCODE,	1,	65536,	INCODE	}, /* VMS F_DSK */
 {	1,	65536,	INCODE,	1,	65536,	INCODE	}, /* VMS F_TAPE */
 {	1,	65536,	INCODE,	1,	65536,	INCODE	}  /* VMS F_TR */
};
/*
 * Default limits of zero are actually the absence of a limit.
 *	This is because the spec word only has 20 bits for the
 *	size.  The zero is translated at run-time...
 */
struct v_rec_limit_s
_V_limits[] =
/*	    record size		   max block size  	*/
/*	 min	max	def	min	max	def	*/
{
 {	0,	0,	0,	0,	0,	0	}, /* illegal */
 {	1,	32760,	32760,	1,	32760,	32760	}, /* IBM U */
 {	5,	32756,	32752,	9,	32760,	32760	}, /* IBM V */
 {	5,	32756,	32752,	9,	32760,	32760	}, /* IBM VB */
 {	1,	NOMAX,	0,	9,	32760,	32760	}, /* IBM VBS */
 {	1,	32767,	32767,	1,	32767,	32767	}, /* VMS V_DSK */
 {	1,	9995,	2043,	6,	32767,	2048	}, /* VMS V_TAPE */
 {	1,	32767,	2044,	3,	32767,	32767	}, /* VMS V_TR */
 {	1,	NOMAX,	0,	5,	32767,	2046	}, /* VMS S_DSK */
 {	1,	NOMAX,	0,	7,	32767,	2048	}, /* VMS S_TAPE */
 {	1,	NOMAX,	0,	5,	32767,	2046	}, /* VMS S_TR */
 {	1,	9995,	4123,	5,	32767,	4128	}, /* NOS/VE D */
 {	1,	NOMAX,	0,	6,	32767,	4128	}, /* NOS/VE S */
};
