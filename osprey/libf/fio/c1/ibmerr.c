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


static char USMID[] = "@(#) libf/fio/c1/ibmerr.c	92.0	10/08/98 14:30:10";

#include <stdio.h>
#include <foreign.h>
#include "fio.h"

/*
 * This routine is called by WRITIBM under UNICOS 
 * to write a message to STDERR. It formats the 
 * current unit number into the message at the
 * specified position.
 */

void
_ibmerr(len, unitpos, ibmerrmsg)
int len;	/* length of message */
int unitpos;	/* position for UNIT number */
char *ibmerrmsg; /* error message */
{
	int	unum;		/* unit number */
	int	i;
	int	pr, d;
	char	*unit_pos;	/* pointer into message */
	FIOSPTR fiosp;

	GET_FIOS_PTR(fiosp);
	unum = fiosp->f_curun;	/* find current unit number */

	unit_pos = ibmerrmsg+unitpos;

	/* Format unit number into message */

        pr = 0;
        for (i = 1000000; i != 1; i /= 10)
        {
                if ((pr |= (d = unum / i)))
                        *unit_pos++ = d + '0';
                unum %= i;
        }
        *unit_pos++ = unum + '0';

	/* Replace last characters of message with newline and null*/

	*(ibmerrmsg+len-2) = '\n';
	*(ibmerrmsg+len-1) = '\0';
        fprintf(stderr,"%s", ibmerrmsg);
}
