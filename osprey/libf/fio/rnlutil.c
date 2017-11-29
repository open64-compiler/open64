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



#pragma ident "@(#) libf/fio/rnlutil.c	92.1	06/21/99 10:37:55"

#include <liberrno.h>
#include <fortran.h>
#include "fio.h"
#ifndef	_UNICOS
#include "rnl90def.h"
#else
#include "rnl.h"
#endif

extern char _getfchar();

/*
 * RNLSKIP determines action if the NAMELIST group encountered is not the
 * desired group.
 *
 *	CALL RNLSKIP(mode)
 *
 *	mode	>0 Skips the record and issues a message (default)
 *		=0 Skips the record
 *		<0 Aborts the job or goes to the optional ERR= branch
 */
#ifdef	_UNICOS
void
RNLSKIP(_f_int *mode)
{
	_SKP_MESS	= *mode;
}
#else
void
rnlskip_(_f_int *mode)
{
	_SKP_MESS	= (long) *mode;
}
#endif

/*
 * RNLTYPE Determines action if type mismatch occurs across equal sign on
 * namelist input record.
 *
 *	CALL RNLTYPE(mode)
 *
 *	mode	!=0 Converts the constant to the type of the variable (default)
 *		 =0 Aborts the program or goes to the optional ERR= branch
 */
#ifdef	_UNICOS
void RNLTYPE(_f_int *mode)
{
	_TYP_CONV	= *mode;
}
#else
void rnltype_(_f_int *mode)
{
	_TYP_CONV	= (long) *mode;
}
#endif

/*
 * RNLECHO Specifies output unit for NAMELIST error messages and echo lines.
 *
 *	CALL RNLECHO(unum)
 *
 *	unum	Output unit to which error messages and echo lines are sent.
 *		If unum=0, error messages and lines echoed because of an E in
 *		column 1 go to stdout.
 *
 *		If unum != 0, error messages and input lines are echoed to unum,
 *		regardless of any echo flags present.  If unum=6 or unum=101,
 *		stdout is implied.
 *
 */
#ifdef	_UNICOS
void
RNLECHO(_f_int *unum)
{
	_OUT_UNIT	= *unum;

	return;
}
#else
void
rnlecho_(_f_int *unum)
{
	_OUT_UNIT	= *unum;

	return;
}
#endif

/*
 * The following routines all have this calling sequence:
 *
 *	CALL RNL____(char, mode)
 *
 *		mode	=0  delete character
 *			!=0 add character
 */

/*
 * RNLFLAG adds or removes char from the set of characters that, if found in
 * column 1, initiates echoing of the input lines to stdout.
 */
#ifdef	_UNICOS
void
RNLFLAG(_fcd chr, _f_int *mode)
{
	int	thechar;

	if (_numargs() != (sizeof(_fcd) + sizeof(long*))/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST, "RNLFLAG");
	thechar	= _getfchar(chr);
	TOGGLE_CHAR(thechar, MRNLFLAG, *mode);

	return;
}
#else
void
rnlflag_(char *chr, _f_int *mode, _f_int clen)
{
	_f_int	thechar;

	thechar	= (_f_int) _getfchar(_cptofcd(chr, (long)clen));
	TOGGLE_CHAR(thechar, MRNLFLAG, *mode);

	return;
}
#endif

/*
 * RNLDELM adds or removes char from the set of characters that precede the
 * NAMELIST group name and signal end-of-input.
 */
#ifdef	_UNICOS
void
RNLDELM(_fcd chr, long *mode)
{
	int	thechar;

	if (_numargs() != (sizeof(_fcd) + sizeof(long*))/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST, "RNLDELM");
	thechar	= _getfchar(chr);
	TOGGLE_CHAR(thechar, MRNLDELIM, *mode);

	return;
}
#else
void
rnldelm_(char *chr, _f_int *mode, _f_int clen)
{
	_f_int	thechar;

	thechar	= (_f_int) _getfchar(_cptofcd(chr, (long)clen));
	TOGGLE_CHAR(thechar, MRNLDELIM, *mode);

	return;
}
#endif

/*
 * RNLSEP adds or removes char from the set of characters that must
 * follow each constant to act as a separator.
 */
#ifdef	_UNICOS
void
RNLSEP(_fcd chr, _f_int *mode)
{
	int	thechar;

	if (_numargs() != (sizeof(_fcd) + sizeof(long*))/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST, "RNLSEP");
	thechar	= _getfchar(chr);
	if (thechar == ' ')
		_BLNKSEP	= *mode;
	TOGGLE_CHAR(thechar, MRNLSEP, *mode);

	return;
}
#else
void
rnlsep_(char *chr, _f_int *mode, _f_int clen)
{
	int	thechar;

	thechar	= (_f_int) _getfchar(_cptofcd(chr, (long)clen));
	if (thechar == ' ')
		_BLNKSEP	= (long) *mode;
	TOGGLE_CHAR(thechar, MRNLSEP, *mode);

	return;
}
#endif

/*
 * RNLREP adds or removes char from the set of characters that occur between
 * the variable name and the value.
 */
#ifdef	_UNICOS
void
RNLREP(_fcd chr, _f_int *mode)
{
	int	thechar;

	if (_numargs() != (sizeof(_fcd) + sizeof(long*))/sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST, "RNLREP");
	thechar	= _getfchar(chr);
	TOGGLE_CHAR(thechar, MRNLREP, *mode);

	return;
}
#else
void
rnlrep_(char *chr, _f_int *mode, _f_int clen)
{
	_f_int	thechar;

	thechar	= (_f_int) _getfchar(_cptofcd(chr, (long)clen));
	TOGGLE_CHAR(thechar, MRNLREP, *mode);

	return;
}
#endif

/*
 * RNLCOMM adds or removes char from the set of characters that initiate
 * trailing comments on a line.
 */
#ifdef	_UNICOS
void
RNLCOMM(_fcd chr, _f_int *mode)
{
	int	thechar;

	if (_numargs() != (sizeof(_fcd) + sizeof(long *))/ sizeof(long))
		_lerror(_LELVL_ABORT,FEARGLST, "RNLCOMM");
	thechar	= _getfchar(chr);
	TOGGLE_CHAR(thechar, MRNLCOMM, *mode);

	return;
}
#else
void
rnlcomm_(char *chr, _f_int *mode, _f_int clen)
{
	_f_int	thechar;

	thechar	= (_f_int) _getfchar(_cptofcd(chr, (long)clen));
	TOGGLE_CHAR(thechar, MRNLCOMM, *mode);

	return;
}
#endif
