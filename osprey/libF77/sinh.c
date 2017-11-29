/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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


/*   VMS Compatibility Version  @(#)sinh.c	1.1    7/18/87
 */
/*
	sinh(arg) returns the hyperbolic sine of its floating-
	point argument.

	The exponential function is called for arguments
	greater in magnitude than 0.5.

	A series is used for arguments smaller in magnitude than 0.5.
	The coefficients are #2029 from Hart & Cheney. (20.36D)

	cosh(arg) is computed from the exponential function for
	all arguments.
*/
#include "cmplrs/host.h"
#include <math.h>
#include "moremath.h"

static double p0  = -0.6307673640497716991184787251e+6;
static double p1  = -0.8991272022039509355398013511e+5;
static double p2  = -0.2894211355989563807284660366e+4;
static double p3  = -0.2630563213397497062819489e+2;
static double q0  = -0.6307673640497716991212077277e+6;
static double q1   = 0.1521517378790019070696485176e+5;
static double q2  = -0.173678953558233699533450911e+3;

double
sinh(double arg)
{
	double temp, argsq;
	register int32 sign;

	sign = 1;
	if(arg < 0) {
		arg = - arg;
		sign = -1;
	}

	if(arg > 21.) {
		temp = exp(arg)/2;
		if (sign>0)
			return(temp);
		else
			return(-temp);
	}

	if(arg > 0.5) {
		return(sign*(exp(arg) - exp(-arg))/2);
	}

	argsq = arg*arg;
	temp = (((p3*argsq+p2)*argsq+p1)*argsq+p0)*arg;
	temp /= (((argsq+q2)*argsq+q1)*argsq+q0);
	return(sign*temp);
}

double
cosh(double arg)
{
	if(arg < 0)
		arg = - arg;
	if(arg > 21.) {
		return(exp(arg)/2);
	}

	return((exp(arg) + exp(-arg))/2);
}
