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


#pragma ident "@(#) libu/util/tremain.c	92.1	07/07/99 13:18:33"
#include <fortran.h>
#include <time.h>
#include <sys/types.h>
#include <sys/jtab.h>
#include <sys/category.h>
#include <sys/resource.h>
#include <sys/times.h>

/*
 *	TREMAIN - Returns the lesser of:
 *		1) CPU time remaining for job (-lT 1200)
 *		2) CPU time remaining for current process (-lt 400) in seconds.
 *
 *	Returns 0 after time limit has been exceeded, returns a large number
 *	if no limits are in effect.
 */

void
TREMAIN(cpu_left)
_f_real	*cpu_left;
{
	extern long limit(), times();
	extern int getjtab();
	double	proc_limit, cpu_used, jtime_left, ptime_left;
	struct tms b_1;
	struct jtab b_2;

#ifdef _CRAYT3E
	/* limit returns seconds on the T3E */
	proc_limit = (double) limit(C_PROC, 0, L_CPU, -1);
#else
	proc_limit = (double) limit(C_PROC, 0, L_CPU, -1) / (double) CLK_TCK;
#endif
	if (proc_limit > 0.) {
		/* Get the cpu time left for this process */
		(void) times(&b_1);
		cpu_used = (double) (b_1.tms_utime + b_1.tms_stime) / (double) CLK_TCK;
		ptime_left = proc_limit - cpu_used;
	}
	else
		ptime_left = 315576000.;

	/* Get the cpu time left for the entire job  */
	if( (getjtab(&b_2) > 0) && (b_2.j_cpulimit > 0) ) {
		/*
		* There is a job limit.  See if it is smaller than
		* the process limit
		*/
#ifdef _CRAYT3E
		/* getjtab returns seconds on the T3E */
		jtime_left = (double) (b_2.j_cpulimit - b_2.j_scputime
				- b_2.j_ucputime);
#else
		jtime_left = (double) (b_2.j_cpulimit - b_2.j_scputime
				- b_2.j_ucputime) / (double) CLK_TCK;
#endif

		if (jtime_left < ptime_left) {
			/*
			* The cpu_time left for the entire job is
			* smaller than this process
			*/
			ptime_left = jtime_left;
		}
	}

	if (ptime_left < 0) {
		/* CPU time has exceeded  */
		ptime_left = 0;
	}

	*cpu_left = (_f_real) ptime_left;

	return;
}
