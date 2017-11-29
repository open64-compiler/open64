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


#pragma ident "@(#) libu/trbk/c1/printvec.c	92.1	07/01/99 13:48:28"
#include  <stdio.h>

/*
 *	_printvec	Prints a formatted print of the contents of the
 *			requested vector registers.
 *
 *	Arguments:
 *		unit	Output stream (e.g., stdout, stderr)
 *		regv	Array containing all of the vector register
 *			contents (64 bits)
 *		regfrst	First vector register to display (0 to 7)
 *		reglast	Last vector register to display (regfrst to 7)
 *		veclen	Contents of the VL register
 */
void
_printvec(
	FILE	*unit,
	long	regv[],
	long	regfirst,
	long	reglast,
	long	veclen
)
{
	register short	i, vl, indx;

	if (veclen == 0)
		vl	= _MAXVL - 1;
	else
		vl	= veclen - 1;

	indx	= 0;

	for (i = regfirst; i <= reglast; ++i) {
		register short	j, repeat;
		register long	sreg, vreg;
		char		string[20];

		for (j = 0; j < _MAXVL; ++j) {

			vreg	= regv[indx++];

			if (j == 0 || j == vl || j == _MAXVL-1 || vreg != sreg) {

				sreg	= vreg;

				(void) _interpret(vreg, string);

				fprintf(unit, " V%d(%3d): %022o %s ", i, j, vreg, string);

				repeat	= 1;

				if (j == vl)
					fprintf(unit, "  (VL: %d)\n", vl + 1);
				else
					putc('\n', unit);

			}
			else
				if (repeat) {
					fprintf(unit, "             (Same as above)\n");
					repeat	= 0;
				}
		}

		putc('\n', unit);

	}

	return;
}
