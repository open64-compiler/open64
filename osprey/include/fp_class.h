/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#ifndef __FP_CLASS_H__
#define __FP_CLASS_H__

#ifndef __GNUC__
#ident "$Revision$"
#endif

#ifdef __cplusplus
extern "C" {
#endif
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Revision$ */



#if ((defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)) && !defined(__GNUC__))
extern int fp_class_d(double);
extern int fp_class_f(float);
#if _COMPILER_VERSION >= 400
extern int fp_class_l(long double);
#define	fp_class_q fp_class_l
#endif
#endif /* LANGUAGE_C */

/*
 * Constants returned by the floating point fp_class_[fdq]() functions.
 */
#define	FP_SNAN		0
#define	FP_QNAN		1
#define	FP_POS_INF	2
#define	FP_NEG_INF	3
#define	FP_POS_NORM	4
#define	FP_NEG_NORM	5
#define	FP_POS_DENORM	6
#define	FP_NEG_DENORM	7
#define	FP_POS_ZERO	8
#define	FP_NEG_ZERO	9

#ifdef __cplusplus
}
#endif
#endif /* !__FP_CLASS_H__ */
