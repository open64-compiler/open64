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


#pragma ident "@(#) libu/util/c1/mtts.c	92.1	07/07/99 13:22:26"


extern int __cycle;     /* hardware clock cycle time in picoseconds */
extern int __hertz;     /* System hertz value (clock/seconds)       */
extern int _numargs();  /* function to determine number of args     */

/*
 *      MTTS - converts the real time clock value from clock ticks to
 *             system timestamp (expressed as nanoseconds).  A version
 *             of MTTS existed on COS.  The libCOS version returned a
 *             timestamp value in nanoseconds.
 *
 *      Called from Fortran:
 *
 *              INTEGER MTTS, ts, irtc, cytype, cpcyl
 *
 *              ts = MTTS(irtc [, cptype, cpcyl] )
 *
 *      INPUT ARGUMENTS:
 *
 *            irtc = 64-bit integer containing a real-time clock value
 *            cptype = not implemented under UNICOS
 *            cpcycl = integer containing CPU cycle time in picoseconds
 *
 *      RESULT:
 *
 *            ts = 64-bit integer containing the timestamp
 *      
 *      If icpcyl is not present, the system value __cycle is used.
 *      
 *      ALGORITHM:
 *          1.  Convert value of RT to float
 *          2.  Convert the value of cpcycl to float, if present.  Otherwise,
 *      	convert the __cycle to float.
 *          3.  Multiply values from steps 1 and 2 to get time in picoseconds
 *          4.  Convert from picoseconds to nanoseconds with multiply by
 *              1.0e-3 to get ts.
 *          5.  Add the number of nanoseconds since January 1, 1973 since RT
 *              is based on January 1, 1990.
 *      
 *      A timestamp is the number of nanoseconds from January 1, 1973, to a 
 *      later date.
 *      
 *      RT is the number of clock ticks since base date January 1, 1990 for
 *      CX/CEA systems after UNICOS 6.1.  Otherwise, rt is the number of
 *      clock ticks since boot time, i.e., rt is booted as zero.  On CRAY-2
 *      systems, rt is booted as zero for all UNICOS levels.  COS systems
 *      used base date January 1, 1973.
 *      
 */
long
MTTS(irtc, cptype, cpcycl)
long *irtc;
int *cptype;
int *cpcycl;
{
	long cyp;
        long value;
        long tsadjust=536457600000000000;

	if (_numargs() < 3)
		cyp = __cycle;
	else
		cyp = *cpcycl;

	value = (((long)( (float) *irtc * (float) cyp * 1.0e-3)) + tsadjust);
	return (value);
}
