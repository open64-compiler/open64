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


#pragma ident "@(#) libu/trbk/c1/printreg.c	92.1	07/01/99 13:48:28"
#include  <stdio.h>

/* Masks and shifts for A & B registers */

#if	_ADDR64
#define	MASK	-1
#define	SHIFT	63
#elif	_ADDR32
#define	MASK	037777777777
#define	SHIFT	31
#else
#define	MASK	077777777
#define	SHIFT	23
#endif

/*
 *	_printreg	Prints a formatted copy of the contents of the
 *			A and S (and VL, VM) registers, or the B and T
 *			registers only.
 *
 *	Arguments:
 *		unit	Output stream (e.g., stdout, stderr)
 *		rega	Array of A (or B) registers (24, 32 or 64 bits)
 *		regs	Array of S (or T) registers (64 bits)
 *		btflag	Print out B/T register only flag
 *		veclen	VL register
 *		vecmask	VM register
 *		la	Current LA register (optional)
 *		vecmsk1	VM1 register on VL128 systems (optional)
 */
void
_printreg(
	FILE	*unit,
	long	rega[],
	long	regs[],
	long	btflag,
	long	veclen,
	long	vecmask,
	long	la,
	long	vecmsk1
)
{
	register short	i, numregs, prflag;
	char		*reg1;
	char		*reg2;

#if	_ADDR64
	static char *fmt = { " %s%o: %022o  %s%o: %022o  %s\n" };
#elif	_ADDR32
	static char *fmt = { " %s%o: %011o %-11d  %s%o: %022o  %s\n" };
#else
	static char *fmt = { " %s%o: %08o %-9d  %s%o: %022o  %s\n" };
#endif

	if (btflag) {
		prflag	= 0;
		reg1	= "B0";
		reg2	= "T0";
		numregs	= 64;
		putc('\n', unit);
	} else {
		prflag	= -1;
		reg1	= "A";
		reg2	= "S";
		numregs	= 8;
	}

	for (i = 0; i < numregs; ++i) {
		register long	areg, dla, sareg, sreg;
		char		string[20];

		areg	= rega[i] & MASK;
		sareg	= areg | ((areg >> SHIFT) ? ~MASK : 0);
		sreg	= regs[i];

		if (i == 8) {
			reg1	= "B";
			reg2	= "T";
		}

		if (btflag && i > 0 && i < 63 && areg == 0 && sreg == 0) {
			++prflag;
			continue;
		}

		(void) _interpret(sreg, string);

#if	_ADDR64
		fprintf(unit, fmt, reg1, i, areg, reg2, i, sreg, string);
#else
		fprintf(unit, fmt, reg1, i, areg, sareg, reg2, i, sreg, string);
#endif
	}

	if (prflag > 1)
		fprintf(unit, " <<< All %d B-T register pairs not printed contain zero >>>\n", prflag);
	else if (prflag == 1)
		fprintf(unit, " <<< 1 B-T register pair not printed (contents zero) >>>\n");

	if (btflag == 0) {

		if (veclen >= 0) {

#if	(_MAXVL > 64)
			fprintf(unit, " VM0: %022o  VL: %d", vecmask, veclen);
#else
			fprintf(unit, " VM: %022o   VL: %d", vecmask, veclen);
#endif

			if (veclen == 0)
				fprintf(unit, " (%d)", _MAXVL);
			else
				if (veclen > 7)
					fprintf(unit, " (%#o)", veclen);
		}

		if (_numargs() > 6) {
			register long	dla;

			dla	= la & MASK;

			if (dla != 0)
				fprintf(unit, "  LA: %d (%#o)", dla, dla);
#if	(_MAXVL > 64)

			if (_numargs() > 7)
				fprintf(unit, "\n VM1: %022o", vecmsk1);
#endif
		}
	}

	putc('\n', unit);

	return;
}
