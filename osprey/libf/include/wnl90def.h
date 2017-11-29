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


/* USMID @(#) libf/include/wnl90def.h	92.0	10/08/98 14:30:10 */
#ifndef _WNL90DEF_H
#define _WNL90DEF_H

#define ERROR0(cond, css, n) {                          \
	if (!(cond))                                    \
		_ferr(css,n);                           \
	else                                            \
		goto finalization;                      \
}

#define ERROR1(cond, css, n, p) {                       \
	if (!(cond))                                    \
		_ferr(css,(n), p);                      \
	else                                            \
		goto finalization;                      \
}

/*
 * NLWFLUSH writes what's in line buffer.
 * Reset pointers and counters so we start at the beginning of the buffer.
 * The first character in line buffer is used for carriage control.
 */

#define NLWFLUSH() {                            \
        if (_fwch(cup, cup->ulinebuf, cup->ulinemax, 1) < 0) { \
                ERROR0(errf, css, errno);       \
        }                                       \
        cup->ulineptr   = cup->ulinebuf;        \
        cup->ulinemax   = 0;                    \
}

/*
 * NLCHAR(X) writes a character to the line buffer
 */

#define NLCHAR(x) {                             \
	if (cup->ulinemax++ > cup->unmlsize) {  \
		ERROR0(errf, css, FEWRLONG); /* output record too long */ \
	} 					\
        *(cup->ulineptr++)      = (long) x;     \
}

/*
 * NLINE determines whether user specified new line for each variable.
 */

#define NLINE() { \
	if (OUT_LINE) {                                                 \
		NLWFLUSH();     /* Write out what's in line buffer */   \
		NLCHAR(' ');    /* write blank                  */      \
		NLCHAR(' ');    /* write blank                  */      \
	}                                                               \
}

extern  long    _wnlrecsiz;
extern  long    OUT_CHAR;
extern  long    OUT_ECHO;
extern  long    OUT_LINE;
extern  long    OUT_EQ;
extern  long    OUT_SEP;

#endif /* !_WNL90DEF_H */
