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


#pragma ident "@(#) libu/util/remark.c	92.1	07/07/99 13:18:33"

#include "fortran.h"

#ifdef _CRAY2
#define MAX_RMK_LEN 152 /* Maximum message length for REMARK */
#define MAX_RMK2_LEN 152 /* Maximum message length for REMARK2 */
#else
#define MAX_RMK_LEN 71 /* Maximum message length for REMARK */
#define MAX_RMK2_LEN 79 /* Maximum message length for REMARK2 */
#endif

static void __remark();
/*
 * REMARK and REMARK2 are Fortran-callable routines which print a 
 * message to stderr.
 */
void
REMARK(s)
_fcd s;		/* Message to be printed as a Fortran character pointer */
{
  
#ifdef _ADDR64
	if (_numargs() == 1) {
		char *saddr;
		saddr = *(char **)&s;
		__remark(_cptofcd(saddr,strlen(saddr)), MAX_RMK_LEN);
	}
	else
#endif
	__remark(s, MAX_RMK_LEN);
}
void
REMARK2(s)
_fcd s;		/* Message to be printed as a Fortran character pointer */
{
#ifdef _ADDR64
	if (_numargs() == 1) {
		char *saddr;
		saddr = *(char **)&s;
		__remark(_cptofcd(saddr,strlen(saddr)), MAX_RMK2_LEN);
	}
	else
#endif
	__remark(s, MAX_RMK2_LEN);
}

static void
__remark(s, maxlen)
_fcd s;		/* Message to be printed as a Fortran character pointer */
int maxlen;	/* Maximum length of message */
{
	char	msg[MAX_RMK2_LEN+1];
	int	len;
  
	len = _fcdlen(s);
	if (len == 0)
		len = strlen(_fcdtocp(s));	/* assume NULL-terminated */
	if (len > maxlen)
		len = maxlen;
  
	/* Copy the message, adding a trailing '\n' */
	strncpy(msg,_fcdtocp(s),len);
	msg[len] = '\n';
	
	write(2,msg,len+1);	/* write message to stderr */
}

