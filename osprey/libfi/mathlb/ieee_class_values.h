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


/* USMID @(#) libfi/mathlb/ieee_class_values.h	92.0	10/08/98 14:37:14 */

#ifndef IEEE_CLASS_VALUES_H
#define IEEE_CLASS_VALUES_H

/* classify values to be used from c but to match Fortran values. */

#ifndef IEEE_CLASS_SIGNALING_NAN
#define IEEE_CLASS_SIGNALING_NAN	1
#endif

#ifndef IEEE_CLASS_QUIET_NAN
#define IEEE_CLASS_QUIET_NAN		2
#endif

#ifndef IEEE_CLASS_NEG_INFINITY
#define IEEE_CLASS_NEG_INFINITY		3
#endif

#ifndef IEEE_CLASS_NEG_NORM_NONZERO
#define IEEE_CLASS_NEG_NORM_NONZERO	4
#endif

#ifndef IEEE_CLASS_NEG_DENORM
#define IEEE_CLASS_NEG_DENORM		5
#endif

#ifndef IEEE_CLASS_NEG_ZERO
#define IEEE_CLASS_NEG_ZERO		6
#endif

#ifndef IEEE_CLASS_POS_ZERO
#define IEEE_CLASS_POS_ZERO		7
#endif

#ifndef IEEE_CLASS_POS_DENORM
#define IEEE_CLASS_POS_DENORM		8
#endif

#ifndef IEEE_CLASS_POS_NORM_NONZERO
#define IEEE_CLASS_POS_NORM_NONZERO	9
#endif

#ifndef IEEE_CLASS_POS_INFINITY
#define IEEE_CLASS_POS_INFINITY		10
#endif

#endif	/* !IEEE_CLASS_VALUES_H */
