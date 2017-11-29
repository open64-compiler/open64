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



#include "ecvt_mp.h"
#include <stdio.h>

#define	NDIG 82
#define _ROUNDUP_RM 1

char *
ecvt_mp (double arg, int ndigits, int *decpt, int *sign, char *buffer)
{
    /*
    extern FILE *debugfile;
    */
    if (ndigits > 17) {
	register char *p, *e;
	*decpt = __dtoa (buffer, 17, arg, 0, _ROUNDUP_RM) + 1;
        for (p = buffer+18, e = buffer + 1 + (ndigits > NDIG ? NDIG : ndigits);
             p != e; ) *p++ = '0';
        *p++ = '\0';
    }
    else if (ndigits <= 0) {
	*decpt = __dtoa (buffer, 1, arg, 0, _ROUNDUP_RM) + 1;
	buffer[1] = '\0';
    }
    else {
	*decpt = __dtoa (buffer, ndigits, arg, 0, _ROUNDUP_RM) + 1;
    }
    *sign = buffer[0] == '-';
    return buffer+1;
}

char *
fcvt_mp (double arg, int ndigits, int *decpt, int *sign, char *buffer)
{
    *decpt = __dtoa (buffer, ndigits, arg, 1, _ROUNDUP_RM) + 1;
    if (*decpt + ndigits <= 0 && buffer[1] >= '5' && buffer[1] <= '9') {
	/* need to round up the digit beyond 'ndigits' */
	buffer[1] = '1';
	buffer[2] = '0';
	buffer[3] = '\0';
	(*decpt)++;
    }
    *sign = buffer[0] == '-';
    return buffer+1;
}

char *
qecvt_mp (long double arg, int ndigits, int *decpt, int *sign, char *buffer)
{
    if (ndigits > 34) {
	register char *p, *e;
	*decpt = _qtoa (buffer, 34, arg, 0) + 1;
        for (p = buffer+35, e = buffer + 1 + (ndigits > NDIG ? NDIG : ndigits);
             p != e; ) *p++ = '0';
        *p++ = '\0';
    }
    else if (ndigits <= 0) {
	*decpt = _qtoa (buffer, 1, arg, 0) + 1;
	buffer[1] = '\0';
    }
    else {
	*decpt = _qtoa (buffer, ndigits, arg, 0) + 1;
    }
    *sign = buffer[0] == '-';
    return buffer+1;
}

char *
qfcvt_mp (long double arg, int ndigits, int *decpt, int *sign, char *buffer)
{
    *decpt = _qtoa (buffer, ndigits, arg, 1) + 1;
    if (*decpt + ndigits <= 0 && buffer[1] >= '5' && buffer[1] <= '9') {
	/* need to round up the digit beyond 'ndigits' */
	buffer[1] = '1';
	buffer[2] = '0';
	buffer[3] = '\0';
	(*decpt)++;
    }
    *sign = buffer[0] == '-';
    return buffer+1;
}
