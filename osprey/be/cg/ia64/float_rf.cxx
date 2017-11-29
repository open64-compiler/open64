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

/* ====================================================================
 * ====================================================================
 *
 * Module: float_rf.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/float_rf.cxx,v $
 *
 * 82-bit register format floating point utilities interface.
 *
 * ====================================================================
 * ==================================================================== */

#include "float_rf.h"

/*************************** defines.h ***************************/

#ifndef _DEFINES_H_
#define _DEFINES_H_

#define EXP_INF		0x1ffff
#define EXP_NAN		0x1ffff
#define EXP_NATVAL	0x1ffff
#define EXP_BIAS	0xffff
#define SIGN_SHIFT	17
#ifndef ULONGLONG_MAX
#define ULONGLONG_MAX	0xffffffffffffffffull
#endif
#ifndef LONGLONG_MAX
#define LONGLONG_MAX	0x7fffffffffffffffll
#endif
#define EXP_MIN	-65534

#define	GUARD_BIT	0x8000000000000000ull
#define	STICKY_BIT	0x4000000000000000ull
#define	HIGH_BIT	0x8000000000000000ull
#define BIT23		(1ull << 23)
#define	BIT52		(1ull << 52)
#define FP_RN	0	/* round to nearest representable number, tie -> even */
#define FP_RM	1	/* round toward minus infinity                        */
#define FP_RP	2	/* round toward plus infinity                         */
#define FP_RZ	3	/* round toward zero (truncate)                       */

#define	INFINITY	1
#define MAXVAL		2
#define MINFINITY	3
#define MMAXVAL		4

/* defines for precision */

#define	UNDEF	-1
#define	SNGL	0
#define	DBL	2
#define DBL_EXT	3

#define	NATVAL	1
#define	SIGNALING_NAN	2
#define	QUIET_NAN	4

#define STICKY_MASK	0x7fffffffffffffffull

#ifndef FALSE
#define	FALSE	0
#endif
#ifndef TRUE
#define	TRUE	1
#endif

#define	SEXP_MIN	-126
#define	SEXP_MAX	127

#define	DEXP_MIN	-1022
#define	DEXP_MAX	1023

#define	DEEXP_MIN	-16382
#define DEEXP_MAX	16383

#define	WEXP_MIN	-65534
#define	WEXP_MAX	65535

#endif

/*************************** fpswa.h ***************************/

/*
** |-----------------------------------------------------------|
** | Copyright (c) 2000 Silicon Graphics, Inc.                 |
** | All Rights Reserved                                       |
** |-----------------------------------------------------------|
** |          Restricted Rights Legend                         |
** | Use, duplication, or disclosure by the Government is      |
** | subject to restrictions as set forth in                   |
** | subparagraph (c)(1)(ii) of the Rights in Technical        |
** | Data and Computer Software Clause of DFARS 52.227-7013.   |
** |         Silicon Graphics, Inc.                            |
** |         2011 N. Shoreline Blvd.                           |
** |         Mountain View, CA 94039                           |
** |-----------------------------------------------------------|
*/

#ifndef _FPSWA_H_
#define _FPSWA_H_


typedef enum {
  op_fcmp,
  op_fpcmp,
  op_fcvt_fx,
  op_fpcvt_fx,
  op_fcvt_fxu,
  op_fpcvt_fxu,
  op_fma,
  op_fpma,
  op_fminmax,
  op_fpminmax,
  op_fms_fnma,
  op_fpms_fpnma,
  op_fnorm,
  op_frcpa,
  op_fprcpa,
  op_frsqrta,
  op_fprsqrta
} fp_op_type;

/* rickc: */ 
/* added to match the Intel simulator w.r.t. software assist faults */
int fp_software_assistance_required(fp_op_type opcode,const float_rf *fr2,
				    const float_rf *fr3, const float_rf *fr4,
				    int sf_dd);

inline int fp_is_natval(const float_rf *freg);
inline int fp_is_inf(const float_rf *freg);
inline int fp_is_nan(const float_rf *freg);
inline int fp_is_qnan(const float_rf *freg);
inline int fp_is_snan(const float_rf *freg);
inline int fp_is_neg_inf(const float_rf *freg);
inline int fp_is_neg_non_zero(const float_rf *freg);
inline int fp_is_normal(const float_rf *freg);
inline int fp_is_pos_inf(const float_rf *freg);
inline int fp_is_pos_non_zero(const float_rf *freg);
inline int fp_is_pseudo_zero(const float_rf *freg);
inline int fp_is_unorm(const float_rf *freg);
inline int fp_is_unorm_not_pseudo_denorm(const float_rf *freg);
inline int fp_is_unsupported(const float_rf *freg);
inline int fp_is_zero(const float_rf *freg);


#endif

/*************************** fpswa.inline.h ***************************/

/*
** |-----------------------------------------------------------|
** | Copyright (c) 2000 Silicon Graphics, Inc.                 |
** | All Rights Reserved                                       |
** |-----------------------------------------------------------|
** |          Restricted Rights Legend                         |
** | Use, duplication, or disclosure by the Government is      |
** | subject to restrictions as set forth in                   |
** | subparagraph (c)(1)(ii) of the Rights in Technical        |
** | Data and Computer Software Clause of DFARS 52.227-7013.   |
** |         Silicon Graphics, Inc.                            |
** |         2011 N. Shoreline Blvd.                           |
** |         Mountain View, CA 94039                           |
** |-----------------------------------------------------------|
*/


/*inline*/ int
fp_is_zero(const float_rf *freg){

  if((freg->exp == 0) && (freg->frac == 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_natval(const float_rf *freg){

  if((freg->sign == 0)
       && (freg->exp == 0x1fffe)
       && (freg->frac == 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_inf(const float_rf *freg){

  if((freg->exp == 0x1ffff) && (freg->frac == 0x8000000000000000ull)) {
    return(1);
  } 
  else {
    return(0);
  }
}


/*inline*/ int
fp_is_nan(const float_rf *freg){

  if((freg->exp == 0x1ffff)
	&& ((freg->frac & 0x8000000000000000ull) != 0)
	&& ((freg->frac & 0x7fffffffffffffffull) != 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_snan(const float_rf *freg){

  if((freg->exp == 0x1ffff)
     && ((freg->frac & 0xc000000000000000ull) == 0xc000000000000000ull)
     && ((freg->frac & 0x3fffffffffffffffull) != 0)){
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_qnan(const float_rf *freg){

  if((freg->exp == 0x1ffff)
     && ((freg->frac & 0xc000000000000000ull) == 0xc000000000000000ull)){
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_neg_inf(const float_rf *freg){

  if((freg->sign == 1)
       && (freg->exp == 0x1ffff)
       && (freg->frac == 0x8000000000000000ull)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_neg_non_zero(const float_rf *freg){

  if((freg->sign == 1) && !fp_is_zero(freg)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_normal(const float_rf *freg){

  if((freg->exp != 0)
       && (freg->exp != 0x1ffff)
       && ((freg->frac & 0x8000000000000000ull) != 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_pos_inf(const float_rf *freg){

  if((freg->sign == 0)
       && (freg->exp == 0x1ffff)
       && (freg->frac == 0x8000000000000000ull)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_pos_non_zero(const float_rf *freg){

  if((freg->sign == 0) && !fp_is_zero(freg)) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_pseudo_zero(const float_rf *freg){

  if((freg->exp != 0)
       && (freg->exp != 0x1ffff)
       && (freg->frac == 0)
       && (!fp_is_natval(freg))) {
    return(1);
  } 
  else {
    return(0);
  }
}

/*inline*/ int
fp_is_unorm(const float_rf *freg){

  if((((freg->exp != 0)
       && (freg->exp != 0x1ffff)
       && ((freg->frac & 0x8000000000000000ull) == 0))
       && (!fp_is_natval(freg)))
       /* double-extended pseudo-denormal or double-extended denormal */
       || ((freg->exp == 0) && (freg->frac != 0))) {
    return(1);
  } 
  else {
    return(0);
  }
}


/*inline*/ int
fp_is_unorm_not_pseudo_denorm(const float_rf *freg){

  if(((freg->exp != 0)
	 && (freg->exp != 0x1ffff)
	 &&((freg->frac & 0x8000000000000000ull) == 0))
         /* double-extended pseudo-denormal or double-extended denormal */
         || ((freg->exp == 0) &&
	     (freg->frac & 0x8000000000000000ull)  == 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}


/*inline*/ int
fp_is_unsupported(const float_rf *freg){
    
  if(fp_is_natval(freg) || fp_is_nan(freg)   || fp_is_inf(freg)
	|| fp_is_normal(freg) || fp_is_unorm(freg) || fp_is_zero(freg)) {
    return(0);
  } 
  else {
    return(1);
  }
}


#ifdef BARF
int
fp_software_assistance_required(fp_op_type opcode,const float_rf *fr2,
				const float_rf *fr3, const float_rf *fr4,
				int sf_dd){

  int do_swa_fault=0;

  /*****************************************************************************
   Filter those cases for each instruction that do NOT cause swa.
  *****************************************************************************/


  switch (opcode) {
    case op_fcmp:
      if((fp_is_unsupported(fr2) || fp_is_unsupported(fr3))   ||
	 (fp_is_nan(fr2) || fp_is_nan(fr3))){
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpcmp:
      if(fp_is_nan(fr2) || fp_is_nan(fr3)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fcvt_fx:
      if(fp_is_unsupported(fr2) || fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpcvt_fx:
      if(fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fcvt_fxu:
      if(fp_is_unsupported(fr2) || fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpcvt_fxu:
      if(fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fma:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3) || fp_is_unsupported(fr4)) || 
	 (fp_is_nan(fr2)         || fp_is_nan(fr3)         || fp_is_nan(fr4))         ||
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_neg_inf(fr2))
	  
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_neg_inf(fr2)))     ||
	 
	 ((fp_is_inf(fr3) && fp_is_zero(fr4)) || (fp_is_zero(fr3) && fp_is_inf(fr4)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpma:
      if(
	 (fp_is_nan(fr2) || fp_is_nan(fr3) || fp_is_nan(fr4)) ||
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4)
	   && fp_is_neg_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_neg_inf(fr2))
	  
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_neg_inf(fr2)))             || 
	 
	 ((fp_is_inf(fr3) && fp_is_zero(fr4)) 
	  || (fp_is_zero(fr3) && fp_is_inf(fr4)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;

    case op_fminmax:
      if((fp_is_unsupported(fr2) || fp_is_unsupported(fr3))   ||
	 (fp_is_nan(fr2) || fp_is_nan(fr3))                ){
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpminmax:
      if(fp_is_nan(fr2) || fp_is_nan(fr3)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fms_fnma:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3) || fp_is_unsupported(fr4)) ||
	 (fp_is_nan(fr2) || fp_is_nan(fr3) || fp_is_nan(fr4))                         || 
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_pos_inf(fr2)))    ||
	 
	 ((fp_is_inf(fr3) && fp_is_zero(fr4)) || (fp_is_zero(fr3) && fp_is_inf(fr4)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpms_fpnma:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3) || fp_is_unsupported(fr4))   ||
	 (fp_is_nan(fr2)     || fp_is_nan(fr3)     || fp_is_nan(fr4))       ||
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4)
	   && fp_is_pos_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_pos_inf(fr2)))                           ||
	 
	 ((fp_is_inf(fr3)  && fp_is_zero(fr4))
	  || (fp_is_zero(fr3) && fp_is_inf(fr4))  )) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fnorm:
      if(fp_is_unsupported(fr3) || fp_is_nan(fr3)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr3)) {
	if(((fr3->exp > 0) && (fr3->exp < 0x1ffff)) &&
	   (sf_dd == 1)) {
	  do_swa_fault = 0;
	}
	else {
	  do_swa_fault     = 1;
	}
      }
      break;
      
    case op_frcpa:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3))   ||
	 (fp_is_nan(fr2)         || fp_is_nan(fr3))           ||
	 
	 ((fp_is_inf(fr2)  && fp_is_inf(fr3))
	  || ((fp_is_zero(fr2) || fp_is_pseudo_zero(fr2))
	      && (fp_is_zero(fr3) || fp_is_pseudo_zero(fr3)))) || 
	 
	 (((fp_is_normal(fr2) && !fp_is_zero(fr2))
	   || (fp_is_unorm(fr2)  && !fp_is_pseudo_zero(fr2)))
	  && (fp_is_zero(fr3)   ||  fp_is_pseudo_zero(fr3)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fprcpa:
      if((fp_is_nan(fr2) || fp_is_nan(fr3))      ||              
	 ((fp_is_inf(fr2)  && fp_is_inf(fr3))
	  || (fp_is_zero(fr2) && fp_is_zero(fr3))) || 
	 (!fp_is_zero(fr2) && fp_is_zero(fr3))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      
      break;
      
    case op_frsqrta:
      if(fp_is_unsupported(fr2)  || 
	 fp_is_nan(fr2)          || 
	 fp_is_neg_inf(fr2)      ||
	 (fp_is_neg_non_zero(fr2) && !fp_is_pseudo_zero(fr2) &&
	  !fp_is_unorm_not_pseudo_denorm(fr2))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;

    case op_fprsqrta:
      if(fp_is_nan(fr2) || fp_is_neg_inf(fr2) || 
	 (fp_is_neg_non_zero(fr2) && !fp_is_unorm(fr2))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      
      break;
  }
  
  return(do_swa_fault);

}
#endif


/*************************** fpglobals.C ***************************/



float_rf
Infinity_rf =
{
0, 0, 0x1ffff,
0x8000000000000000ll,
};

float_rf
Quiet_nan_rf =
{
0, 1, 0x1ffff,
0xc000000000000000ll,
};

float_rf
NaTVal_rf =
{
0, 0, 0x1ffff,
0x4000000000000000ll,
};

float_rf
Zero_rf =
{
0, 0, 0x00000,
0x0000000000000000ll,
};


/*************************** fpinst.C ***************************/

#include <stdio.h>

#if !defined(linux)
#include <inttypes.h>
#endif


static
unsigned short
recip_table[256] = {
	0x3fc, 0x3f4, 0x3ec, 0x3e4, 0x3dd, 0x3d5, 0x3cd, 0x3c6,
	0x3be, 0x3b7, 0x3af, 0x3a8, 0x3a1, 0x399, 0x392, 0x38b,
	0x384, 0x37d, 0x376, 0x36f, 0x368, 0x361, 0x35b, 0x354,
	0x34d, 0x346, 0x340, 0x339, 0x333, 0x32c, 0x326, 0x320,
	0x319, 0x313, 0x30d, 0x307, 0x300, 0x2fa, 0x2f4, 0x2ee,
	0x2e8, 0x2e2, 0x2dc, 0x2d7, 0x2d1, 0x2cb, 0x2c5, 0x2bf,
	0x2ba, 0x2b4, 0x2af, 0x2a9, 0x2a3, 0x29e, 0x299, 0x293,
	0x28e, 0x288, 0x283, 0x27e, 0x279, 0x273, 0x26e, 0x269,
	0x264, 0x25f, 0x25a, 0x255, 0x250, 0x24b, 0x246, 0x241,
	0x23c, 0x237, 0x232, 0x22e, 0x229, 0x224, 0x21f, 0x21b,
	0x216, 0x211, 0x20d, 0x208, 0x204, 0x1ff, 0x1fb, 0x1f6,
	0x1f2, 0x1ed, 0x1e9, 0x1e5, 0x1e0, 0x1dc, 0x1d8, 0x1d4,
	0x1cf, 0x1cb, 0x1c7, 0x1c3, 0x1bf, 0x1bb, 0x1b6, 0x1b2,
	0x1ae, 0x1aa, 0x1a6, 0x1a2, 0x19e, 0x19a, 0x197, 0x193,
	0x18f, 0x18b, 0x187, 0x183, 0x17f, 0x17c, 0x178, 0x174,
	0x171, 0x16d, 0x169, 0x166, 0x162, 0x15e, 0x15b, 0x157,
	0x154, 0x150, 0x14d, 0x149, 0x146, 0x142, 0x13f, 0x13b,
	0x138, 0x134, 0x131, 0x12e, 0x12a, 0x127, 0x124, 0x120,
	0x11d, 0x11a, 0x117, 0x113, 0x110, 0x10d, 0x10a, 0x107,
	0x103, 0x100, 0x0fd, 0x0fa, 0x0f7, 0x0f4, 0x0f1, 0x0ee,
	0x0eb, 0x0e8, 0x0e5, 0x0e2, 0x0df, 0x0dc, 0x0d9, 0x0d6,
	0x0d3, 0x0d0, 0x0cd, 0x0ca, 0x0c8, 0x0c5, 0x0c2, 0x0bf,
	0x0bc, 0x0b9, 0x0b7, 0x0b4, 0x0b1, 0x0ae, 0x0ac, 0x0a9,
	0x0a6, 0x0a4, 0x0a1, 0x09e, 0x09c, 0x099, 0x096, 0x094,
	0x091, 0x08e, 0x08c, 0x089, 0x087, 0x084, 0x082, 0x07f,
	0x07c, 0x07a, 0x077, 0x075, 0x073, 0x070, 0x06e, 0x06b,
	0x069, 0x066, 0x064, 0x061, 0x05f, 0x05d, 0x05a, 0x058,
	0x056, 0x053, 0x051, 0x04f, 0x04c, 0x04a, 0x048, 0x045,
	0x043, 0x041, 0x03f, 0x03c, 0x03a, 0x038, 0x036, 0x033,
	0x031, 0x02f, 0x02d, 0x02b, 0x029, 0x026, 0x024, 0x022,
	0x020, 0x01e, 0x01c, 0x01a, 0x018, 0x015, 0x013, 0x011,
	0x00f, 0x00d, 0x00b, 0x009, 0x007, 0x005, 0x003, 0x001
};

unsigned short recip_sqrt_table[256] = {
0x1a5, 0x1a0, 0x19a, 0x195, 0x18f, 0x18a, 0x185, 0x180,
0x17a, 0x175, 0x170, 0x16b, 0x166, 0x161, 0x15d, 0x158,
0x153, 0x14e, 0x14a, 0x145, 0x140, 0x13c, 0x138, 0x133,
0x12f, 0x12a, 0x126, 0x122, 0x11e, 0x11a, 0x115, 0x111,
0x10d, 0x109, 0x105, 0x101, 0x0fd, 0x0fa, 0x0f6, 0x0f2,
0x0ee, 0x0ea, 0x0e7, 0x0e3, 0x0df, 0x0dc, 0x0d8, 0x0d5,
0x0d1, 0x0ce, 0x0ca, 0x0c7, 0x0c3, 0x0c0, 0x0bd, 0x0b9,
0x0b6, 0x0b3, 0x0b0, 0x0ad, 0x0a9, 0x0a6, 0x0a3, 0x0a0,
0x09d, 0x09a, 0x097, 0x094, 0x091, 0x08e, 0x08b, 0x088,
0x085, 0x082, 0x07f, 0x07d, 0x07a, 0x077, 0x074, 0x071,
0x06f, 0x06c, 0x069, 0x067, 0x064, 0x061, 0x05f, 0x05c,
0x05a, 0x057, 0x054, 0x052, 0x04f, 0x04d, 0x04a, 0x048,
0x045, 0x043, 0x041, 0x03e, 0x03c, 0x03a, 0x037, 0x035,
0x033, 0x030, 0x02e, 0x02c, 0x029, 0x027, 0x025, 0x023,
0x020, 0x01e, 0x01c, 0x01a, 0x018, 0x016, 0x014, 0x011,
0x00f, 0x00d, 0x00b, 0x009, 0x007, 0x005, 0x003, 0x001,
0x3fc, 0x3f4, 0x3ec, 0x3e5, 0x3dd, 0x3d5, 0x3cd, 0x3c7,
0x3bf, 0x3b8, 0x3b1, 0x3aa, 0x3a3, 0x39c, 0x395, 0x38e,
0x388, 0x381, 0x37a, 0x374, 0x36d, 0x367, 0x361, 0x35a,
0x354, 0x34e, 0x348, 0x342, 0x33c, 0x336, 0x330, 0x32b,
0x325, 0x31f, 0x31a, 0x314, 0x30f, 0x309, 0x304, 0x2fe,
0x2f9, 0x2f4, 0x2ee, 0x2e9, 0x2e4, 0x2df, 0x2da, 0x2d5,
0x2d0, 0x2cb, 0x2c6, 0x2c1, 0x2bd, 0x2b8, 0x2b3, 0x2ae,
0x2aa, 0x2a5, 0x2a1, 0x29c, 0x298, 0x293, 0x28f, 0x28a,
0x286, 0x282, 0x27d, 0x279, 0x275, 0x271, 0x26d, 0x268,
0x264, 0x260, 0x25c, 0x258, 0x254, 0x250, 0x24c, 0x249,
0x245, 0x241, 0x23d, 0x239, 0x235, 0x232, 0x22e, 0x22a,
0x227, 0x223, 0x220, 0x21c, 0x218, 0x215, 0x211, 0x20e,
0x20a, 0x207, 0x204, 0x200, 0x1fd, 0x1f9, 0x1f6, 0x1f3,
0x1f0, 0x1ec, 0x1e9, 0x1e6, 0x1e3, 0x1df, 0x1dc, 0x1d9,
0x1d6, 0x1d3, 0x1d0, 0x1cd, 0x1ca, 0x1c7, 0x1c4, 0x1c1,
0x1be, 0x1bb, 0x1b8, 0x1b5, 0x1b2, 0x1af, 0x1ac, 0x1aa,
};

/* Notice that float_rf stands for a register format floating point value */

/* floating point multiply and add */

float_rf
fma( const float_rf *rs, const float_rf *rt, const float_rf *rr, status_field *sf )
{
uint32_t rssign, rtsign, rrsign;
int32_t	rsexp, rtexp, rrexp;
uint64_t rsfrac, rtfrac, rrfrac;
int32_t	op_type;
float_rf result_rf;

#ifdef FP_DEBUG
	fprintf(stderr, "fma:  rs.sign = %x\n", (*rs).sign);
	fprintf(stderr, "      rs.exp = %x\n", (*rs).exp);
	fprintf(stderr, "      rs.frac = %016llx\n", (*rs).frac);
	fprintf(stderr, "      rt.sign = %x\n", (*rt).sign);
	fprintf(stderr, "      rt.exp = %x\n", (*rt).exp);
	fprintf(stderr, "      rt.frac = %016llx\n", (*rt).frac);
	fprintf(stderr, "      rr.sign = %x\n", (*rr).sign);
	fprintf(stderr, "      rr.exp = %x\n", (*rr).exp);
	fprintf(stderr, "      rr.frac = %016llx\n", (*rr).frac);
	fprintf(stderr, "      sf = %016llx\n\n", sf->i);
#endif

	if(fp_software_assistance_required(op_fma,rs,rt,rr,sf->b.den_unn_op_trap_disabled)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}
	  
	if ( op_type = breakout_and_test3(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, 
				rr, &rrsign, &rrexp, &rrfrac, 
				sf)
	   )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "      op_type = %x\n", op_type);
#endif
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = Quiet_nan_rf;
			return ( result_rf );
		}
		else if ( op_type == QUIET_NAN )
		{

		        // rickc add: 
			sf->b.invalid_flag = 1; /* set invalid flag */

			if ( (rsexp == EXP_NAN) && (rsfrac > (1ull << 63)) )
			{
				result_rf = *rs;
				return ( result_rf );
			}
			else if ( (rtexp == EXP_NAN) && (rtfrac > (1ull << 63)) )
			{
				result_rf = *rt;
				return ( result_rf );
			}
			else
			{
				result_rf = *rr;
				return ( result_rf );
			}
		}
	}

	return ( madd(rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac,
		 rrsign, rrexp, rrfrac, sf) );
}


// note:  
//    fnorm is identical to fma except for the parameters passed to 
//    fp_software_assistance_required().  fnorm is an exception to the
//    usual swa rules which forced us to create an fnorm function

float_rf
fnorm( const float_rf *rs, const float_rf *rt, const float_rf *rr, status_field *sf )
{
uint32_t rssign, rtsign, rrsign;
int32_t	rsexp, rtexp, rrexp;
uint64_t rsfrac, rtfrac, rrfrac;
int32_t	op_type;
float_rf result_rf;

#ifdef FP_DEBUG
	fprintf(stderr, "fma:  rs.sign = %x\n", (*rs).sign);
	fprintf(stderr, "      rs.exp = %x\n", (*rs).exp);
	fprintf(stderr, "      rs.frac = %016llx\n", (*rs).frac);
	fprintf(stderr, "      rt.sign = %x\n", (*rt).sign);
	fprintf(stderr, "      rt.exp = %x\n", (*rt).exp);
	fprintf(stderr, "      rt.frac = %016llx\n", (*rt).frac);
	fprintf(stderr, "      rr.sign = %x\n", (*rr).sign);
	fprintf(stderr, "      rr.exp = %x\n", (*rr).exp);
	fprintf(stderr, "      rr.frac = %016llx\n", (*rr).frac);
	fprintf(stderr, "      sf = %016llx\n\n", sf->i);
#endif

	if(fp_software_assistance_required(op_fnorm,rs,rt,rr,sf->b.den_unn_op_trap_disabled)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}
	  
	if ( op_type = breakout_and_test3(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, 
				rr, &rrsign, &rrexp, &rrfrac, 
				sf)
	   )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "      op_type = %x\n", op_type);
#endif
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = Quiet_nan_rf;
			return ( result_rf );
		}
		else if ( op_type == QUIET_NAN )
		{

		        // rickc add: 
			sf->b.invalid_flag = 1; /* set invalid flag */

			if ( (rsexp == EXP_NAN) && (rsfrac > (1ull << 63)) )
			{
				result_rf = *rs;
				return ( result_rf );
			}
			else if ( (rtexp == EXP_NAN) && (rtfrac > (1ull << 63)) )
			{
				result_rf = *rt;
				return ( result_rf );
			}
			else
			{
				result_rf = *rr;
				return ( result_rf );
			}
		}
	}

	return ( madd(rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac,
		 rrsign, rrexp, rrfrac, sf) );
}


/* floating point multiply and subtract */

float_rf
fms( const float_rf *rs, const float_rf *rt, const float_rf *rr, status_field *sf )
{
uint32_t rssign, rtsign, rrsign;
int32_t	rsexp, rtexp, rrexp;
uint64_t rsfrac, rtfrac, rrfrac;
int32_t	op_type;
float_rf result_rf;

	if(fp_software_assistance_required(op_fms_fnma,rs,rt,rr,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	if ( op_type = breakout_and_test3(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, 
				rr, &rrsign, &rrexp, &rrfrac, 
				sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = Quiet_nan_rf;
			return ( result_rf );
		}
		else if ( op_type == QUIET_NAN )
		{
		        // rickc add: 
			sf->b.invalid_flag = 1; /* set invalid flag */

			if ( (rsexp == EXP_NAN) && (rsfrac > (1ull << 63)) )
			{
				result_rf = *rs;
				return ( result_rf );
			}
			else if ( (rtexp == EXP_NAN) && (rtfrac > (1ull << 63)) )
			{
				result_rf = *rt;
				return ( result_rf );
			}
			else
			{
				result_rf = *rr;
				return ( result_rf );
			}
		}
	}

	return ( madd(rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac,
		 rrsign ^ 1, rrexp, rrfrac, sf) );
}

/* floating point negative multiply and add */

float_rf
fnma( const float_rf *rs, const float_rf *rt, const float_rf *rr, status_field *sf )
{
uint32_t rssign, rtsign, rrsign;
int32_t	rsexp, rtexp, rrexp;
uint64_t rsfrac, rtfrac, rrfrac;
int32_t	op_type;
float_rf result_rf;

	if(fp_software_assistance_required(op_fms_fnma,rs,rt,rr,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	if ( op_type = breakout_and_test3(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, 
				rr, &rrsign, &rrexp, &rrfrac, 
				sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = Quiet_nan_rf;
			return ( result_rf );
		}
		else if ( op_type == QUIET_NAN )
		{

		        // rickc add: 
			sf->b.invalid_flag = 1; /* set invalid flag */

			if ( (rsexp == EXP_NAN) && (rsfrac > (1ull << 63)) )
			{
				result_rf = *rs;
				return ( result_rf );
			}
			else if ( (rtexp == EXP_NAN) && (rtfrac > (1ull << 63)) )
			{
				result_rf = *rt;
				return ( result_rf );
			}
			else
			{
				result_rf = *rr;
				return ( result_rf );
			}
		}
	}

	return ( madd(rssign ^ 1, rsexp, rsfrac, rtsign, rtexp, rtfrac,
		 rrsign, rrexp, rrfrac, sf) );
}


/* floating point minimum */

float_rf
fmin( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign, rtsign;
int32_t	rsexp, rtexp;
uint64_t rsfrac, rtfrac;
int32_t	op_type;
float_rf result_rf;

	if(fp_software_assistance_required(op_fminmax,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( (op_type == SIGNALING_NAN) || (op_type == QUIET_NAN) )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = *rt;
			return ( result_rf );
		}
	}

	/* denormal, unnormal, natval, and nan inputs have been screened */

	/* screen zero inputs next */

	if ( (rsexp == 0) && (rsfrac == 0) &&
	     (rtexp == 0) && (rtfrac == 0)
	   )
	{
		/* both inputs zero */

		result_rf = *rt;
		return ( result_rf );
	}

	if ( (rsexp == 0) && (rsfrac == 0) )
	{
		if ( rtsign == 1 )
		{
			/* rs == 0.0 && rt < 0.0 */

			result_rf = *rt;
			return ( result_rf );
		}
		else
		{
			result_rf = *rs;
			return ( result_rf );
		}
	}

	if ( (rtexp == 0) && (rtfrac == 0) )
	{
		if ( rssign == 1 )
		{
			/* rt == 0.0 && rs < 0.0 */

			result_rf = *rs;
			return ( result_rf );
		}
		else
		{
			result_rf = *rt;
			return ( result_rf );
		}
	}

	if ( (rssign == 0) && (rtsign == 1) )
	{
		result_rf = *rt;
		return ( result_rf );
	}

	if ( (rssign == 1) && (rtsign == 0) )
	{
		result_rf = *rs;
		return ( result_rf );
	}

	/* same signs */

	if ( (rsexp == rtexp) && (rsfrac == rtfrac) )
	{
		result_rf = *rt;
		return ( result_rf );
	}

	if ( rssign == 0 )
	{
		if ( rsexp < rtexp )
		{
			result_rf = *rs;
			return ( result_rf );
		}
		else if ( rsexp > rtexp )
		{
			result_rf = *rt;
			return ( result_rf );
		}
		else if ( rsfrac < rtfrac )
		{
			result_rf = *rs;
			return ( result_rf );
		}
		else
		{
			result_rf = *rt;
			return ( result_rf );
		}
	}
	else	/* rssign == 1 */
	{
		if ( rsexp < rtexp )
		{
			result_rf = *rt;
			return ( result_rf );
		}
		else if ( rsexp > rtexp )
		{
			result_rf = *rs;
			return ( result_rf );
		}
		else if ( rsfrac < rtfrac )
		{
			result_rf = *rt;
			return ( result_rf );
		}
		else
		{
			result_rf = *rs;
			return ( result_rf );
		}
	}
}

/* floating point maximum */

float_rf
fmax( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign, rtsign;
int32_t	rsexp, rtexp;
uint64_t rsfrac, rtfrac;
int32_t	op_type;
float_rf result_rf;

	if(fp_software_assistance_required(op_fminmax,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( (op_type == SIGNALING_NAN) || (op_type == QUIET_NAN) )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = *rt;
			return ( result_rf );
		}
	}

	/* denormal, unnormal, natval, and nan inputs have been screened */

	/* screen zero inputs here */

	if ( (rsexp == 0) && (rsfrac == 0) &&
	     (rtexp == 0) && (rtfrac == 0)
	   )
	{
		/* both inputs zero */

		result_rf = *rt;
		return ( result_rf );
	}

	if ( (rsexp == 0) && (rsfrac == 0) )
	{
		if ( rtsign == 1 )
		{
			/* rsexp == 0 && rt < 0.0 */

			result_rf = *rs;
			return ( result_rf );
		}
		else
		{
			result_rf = *rt;
			return ( result_rf );
		}
	}

	if ( (rtexp == 0) && (rtfrac == 0) )
	{
		if ( rssign == 1 )
		{
			/* rtexp == 0 && rs < 0.0 */

			result_rf = *rt;
			return ( result_rf );
		}
		else
		{
			result_rf = *rs;
			return ( result_rf );
		}
	}

	if ( (rssign == 0) && (rtsign == 1) )
	{
		result_rf = *rs;
		return ( result_rf );
	}

	if ( (rssign == 1) && (rtsign == 0) )
	{
		result_rf = *rt;
		return ( result_rf );
	}

	/* same signs */

	if ( (rsexp == rtexp) && (rsfrac == rtfrac) )
	{
		result_rf = *rt;
		return ( result_rf );
	}

	if ( rssign == 0 )
	{
		if ( rsexp < rtexp )
		{
			result_rf = *rt;
			return ( result_rf );
		}
		else if ( rsexp > rtexp )
		{
			result_rf = *rs;
			return ( result_rf );
		}
		else if ( rsfrac < rtfrac )
		{
			result_rf = *rt;
			return ( result_rf );
		}
		else
		{
			result_rf = *rs;
			return ( result_rf );
		}
	}
	else	/* rssign == 1 */
	{
		if ( rsexp < rtexp )
		{
			result_rf = *rs;
			return ( result_rf );
		}
		else if ( rsexp > rtexp )
		{
			result_rf = *rt;
			return ( result_rf );
		}
		else if ( rsfrac < rtfrac )
		{
			result_rf = *rs;
			return ( result_rf );
		}
		else
		{
			result_rf = *rt;
			return ( result_rf );
		}
	}
}


/* floating point absolute minimum */

float_rf
famin( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign, rtsign;
int32_t	rsexp, rtexp;
uint64_t rsfrac, rtfrac;
int32_t	op_type;
float_rf result_rf;

	if(fp_software_assistance_required(op_fminmax,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( (op_type == SIGNALING_NAN) || (op_type == QUIET_NAN) )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = *rt;
			return ( result_rf );
		}
	}

	/* denormal, unnormal, natval, and nan inputs have been screened */

	/* screen zero inputs next */

	if ( (rsexp == 0) && (rsfrac == 0) &&
	     (rtexp == 0) && (rtfrac == 0)
	   )
	{
		/* both inputs zero */

		result_rf = *rt;
		return ( result_rf );
	}

	if ( (rsexp == rtexp) && (rsfrac == rtfrac) )
	{
		result_rf = *rt;
		return ( result_rf );
	}

	if ( (rsexp < rtexp) || ((rsexp == rtexp) && (rsfrac < rtfrac)) )
	{
		result_rf = *rs;
		return ( result_rf );
	}
	else
	{
		result_rf = *rt;
		return ( result_rf );
	}
}

/* floating point maximum */

float_rf
famax( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign, rtsign;
int32_t	rsexp, rtexp;
uint64_t rsfrac, rtfrac;
int32_t	op_type;
float_rf result_rf;

	if(fp_software_assistance_required(op_fminmax,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;

			return ( result_rf );
		}
		else if ( (op_type == SIGNALING_NAN) || (op_type == QUIET_NAN) )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = *rt;
			return ( result_rf );
		}
	}

	/* denormal, unnormal, natval, and nan inputs have been screened */

	/* screen zero inputs here */

	if ( (rsexp == 0) && (rsfrac == 0) &&
	     (rtexp == 0) && (rtfrac == 0)
	   )
	{
		/* both inputs zero */

		result_rf = *rt;
		return ( result_rf );
	}

	/* screen zero inputs next */

	if ( (rsexp == 0) && (rsfrac == 0) &&
	     (rtexp == 0) && (rtfrac == 0)
	   )
	{
		/* both inputs zero */

		result_rf = *rt;
		return ( result_rf );
	}

	if ( (rsexp == rtexp) && (rsfrac == rtfrac) )
	{
		result_rf = *rt;
		return ( result_rf );
	}

	if ( (rsexp < rtexp) || ((rsexp == rtexp) && (rsfrac < rtfrac)) )
	{
		result_rf = *rt;
		return ( result_rf );
	}
	else
	{
		result_rf = *rs;
		return ( result_rf );
	}
}

/* convert floating point to signed integer */

float_rf
fcvtfx( const float_rf *rs, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
int32_t	op_type;
float_rf result_rf;
uint64_t guard_bits;
int32_t shift_count;

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfx: rs.sign = %x\n", rs->sign);
	fprintf(stderr, "        rs.exp = %x\n", rs->exp);
	fprintf(stderr, "        rs.frac = %016llx\n", rs->frac);
	fprintf(stderr, "        sf = %016llx\n", sf->i);
#endif

	if(fp_software_assistance_required(op_fcvt_fx,rs,NULL,NULL,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	op_type = breakout_and_test( rs, &rssign, &rsexp, &rsfrac, sf );

	/* special case -2**63 */

	if ( (rssign == 1) && (rsexp == (63 + EXP_BIAS)) &&
	     (rsfrac == HIGH_BIT)
	   )
	{
		/* return -2**63 in integer format */

		result_rf = *rs;
		result_rf.sign = 0;

		return ( result_rf );
	}

	/* check for natvals, nans, infinities;
	   check for out of range values
	*/

	if ( op_type == NATVAL )
	{
		result_rf = NaTVal_rf;

		return ( result_rf );
	}
	else if ( (op_type == SIGNALING_NAN) ||
		  (op_type == QUIET_NAN) ||
		  (rsexp >= 63 + EXP_BIAS)
		)
	{
		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		result_rf.frac = LONGLONG_MAX;

		return ( result_rf );
	}

	/* take care of zeroes */

	if ( (rsexp == 0) && (rsfrac == 0ull) )
	{
		/* rs is a (signed) zero */

		result_rf.sign = 0;
		result_rf.exp = 0;
		result_rf.frac = 0;

		return ( result_rf );
	}

	rsexp -= EXP_BIAS; /* subtract exponent bias */

	if ( rsexp < -1 )
	{
		rsfrac = 0;
		guard_bits = STICKY_BIT;
	}
	else if ( rsexp == -1 )
	{
		guard_bits = rsfrac;
		rsfrac = 0;
	}
	else
	{
		shift_count = 63 - rsexp;

		guard_bits = (rsfrac << (64 - shift_count));
		rsfrac >>= shift_count;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "fpcvtfx calling round_l\n");
	fprintf(stderr, "	 rssign = %x\n", rssign);
	fprintf(stderr, "	 rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, "	 guard_bits = %016llx\n\n", guard_bits);
#endif

	result_rf = round_l(rssign, rsfrac, guard_bits, sf);

#ifdef FP_DEBUG
	fprintf(stderr, "fpcvtfx:  returning\n");
	fprintf(stderr, "          result_rf.sign = %x\n", result_rf.sign);
	fprintf(stderr, "          result_rf.exp = %x\n", result_rf.exp);
	fprintf(stderr, "          result_rf.frac = %016llx\n", result_rf.frac);
#endif

	return ( result_rf );
}

/* convert floating point to signed integer by truncating */

float_rf
fcvtfxtrunc( const float_rf *rs, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
int32_t	op_type;
float_rf result_rf;
uint64_t guard_bits;
int32_t shift_count;

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxtrunc: rs.sign = %x\n", rs->sign);
	fprintf(stderr, "             rs.exp = %x\n", rs->exp);
	fprintf(stderr, "             rs.frac = %016llx\n", rs->frac);
	fprintf(stderr, "             sf = %016llx\n", sf->i);
#endif

	if(fp_software_assistance_required(op_fcvt_fx,rs,NULL,NULL,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	op_type = breakout_and_test( rs, &rssign, &rsexp, &rsfrac, sf );

	/* special case -2**63 */

	if ( (rssign == 1) && (rsexp == (63 + EXP_BIAS)) &&
	     (rsfrac == HIGH_BIT)
	   )
	{
		/* return -2**63 in integer format */

		result_rf = *rs;
		result_rf.sign = 0;

		return ( result_rf );
	}

	/* check for natvals, nans, infinities;
	   check for out of range values
	*/

	if ( op_type == NATVAL )
	{
		result_rf = NaTVal_rf;

		return ( result_rf );
	}
	else if ( (op_type == SIGNALING_NAN) ||
		  (op_type == QUIET_NAN) ||
		  (rsexp >= 63 + EXP_BIAS)
		)
	{
		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		result_rf.frac = LONGLONG_MAX;

		return ( result_rf );
	}

	/* take care of zeroes */

	if ( (rsexp == 0) && (rsfrac == 0ull) )
	{
		/* rs is a (signed) zero */

		result_rf.sign = 0;
		result_rf.exp = 0;
		result_rf.frac = 0;

		return ( result_rf );
	}

	rsexp -= EXP_BIAS; /* subtract exponent bias */

	if ( rsexp <= -1 )
	{
		sf->b.inexact_flag = 1; /* set inexact flag */

		return ( Zero_rf );
	}
	else
	{
		shift_count = 63 - rsexp;

		guard_bits = (rsfrac << (64 - shift_count));
		rsfrac >>= shift_count;

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		if ( rssign == 0 )
			result_rf.frac = rsfrac;
		else
			result_rf.frac = -rsfrac;

		if ( guard_bits != 0 )
			sf->b.inexact_flag = 1; /* set inexact flag */

		return ( result_rf );
	}
}

/* convert floating point to an unsigned integer */

float_rf
fcvtfxu( const float_rf *rs, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
int32_t	op_type;
float_rf result_rf;
uint64_t guard_bits;
int32_t shift_count;

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxu: rs.sign = %x\n", rs->sign);
	fprintf(stderr, "         rs.exp = %x\n", rs->exp);
	fprintf(stderr, "         rs.frac = %016llx\n", rs->frac);
	fprintf(stderr, "         sf = %016llx\n\n", sf->i);
#endif

	if(fp_software_assistance_required(op_fcvt_fxu,rs,NULL,NULL,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	op_type = breakout_and_test( rs, &rssign, &rsexp, &rsfrac, sf );

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxu: rssign = %x\n", rssign);
	fprintf(stderr, "         rsexp = %x\n", rsexp);
	fprintf(stderr, "         rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, "         sf = %016llx\n", sf->i);
	fprintf(stderr, "         op_type = %x\n\n", op_type);
#endif
	

	/* check for natvals, nans, infinities;
	   check for out of range values
	*/

	if ( op_type == NATVAL )
	{
		result_rf = NaTVal_rf;

		return ( result_rf );
	}
	else if ( (op_type == SIGNALING_NAN) ||
		  (op_type == QUIET_NAN) ||
		  (rsexp >= 64 + EXP_BIAS)
		)
	{
#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxu: setting invalid flag\n\n");
#endif
		sf->b.invalid_flag = 1; /* set invalid flag */

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxu:  sf = %016llx\n", sf->i);
	fprintf(stderr, "          sf->b.invalid_flag = %llx\n", sf->b.invalid_flag);
#endif

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		result_rf.frac = ULONGLONG_MAX;

		return ( result_rf );
	}

	/* take care of zeroes */

	if ( (rsexp == 0) && (rsfrac == 0ull) )
	{
		/* rs is a (signed) zero */

		return ( Zero_rf );
	}

	/* negative values are invalid */

	if ( rssign != 0 )
	{
		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		result_rf.frac = ULONGLONG_MAX;

		return ( result_rf );
	}

	rsexp -= EXP_BIAS; /* subtract exponent bias */

	if ( rsexp < -1 )
	{
		rsfrac = 0;
		guard_bits = STICKY_BIT;
	}
	else if ( rsexp == -1 )
	{
		guard_bits = rsfrac;
		rsfrac = 0;
	}
	else
	{
		guard_bits = 0;
		shift_count = 63 - rsexp;

		if ( shift_count != 0 )
		{
			guard_bits = (rsfrac << (64 - shift_count));
			rsfrac >>= shift_count;
		}
	}

#ifdef FP_DEBUG
	fprintf(stderr, "fpcvtfxu calling round_l\n");
	fprintf(stderr, " 	 rssign = %x\n", rssign);
	fprintf(stderr, " 	 rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, " 	 guard_bits = %016llx\n\n", guard_bits);
#endif

	result_rf = round_l(rssign, rsfrac, guard_bits, sf);

#ifdef FP_DEBUG
	fprintf(stderr, "fpcvtfxu:  returning\n");
	fprintf(stderr, "           result_rf.sign = %x\n", result_rf.sign);
	fprintf(stderr, "           result_rf.exp = %x\n", result_rf.exp);
	fprintf(stderr, "           result_rf.frac = %016llx\n", result_rf.frac);
#endif

	return ( result_rf );
}

/* convert floating point to an unsigned integer by truncating */

float_rf
fcvtfxutrunc( const float_rf *rs, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
int32_t	op_type;
float_rf result_rf;
uint64_t guard_bits;
int32_t shift_count;

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxutrunc: rs.sign = %x\n", rs->sign);
	fprintf(stderr, "              rs.exp = %x\n", rs->exp);
	fprintf(stderr, "              rs.frac = %016llx\n", rs->frac);
	fprintf(stderr, "              sf = %016llx\n\n", sf->i);
#endif

	if(fp_software_assistance_required(op_fcvt_fxu,rs,NULL,NULL,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	op_type = breakout_and_test( rs, &rssign, &rsexp, &rsfrac, sf );

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxutrunc: rssign = %x\n", rssign);
	fprintf(stderr, "              rsexp = %x\n", rsexp);
	fprintf(stderr, "              rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, "              sf = %016llx\n", sf->i);
	fprintf(stderr, "              op_type = %x\n\n", op_type);
#endif
	

	/* check for natvals, nans, infinities;
	   check for out of range values
	*/

	if ( op_type == NATVAL )
	{
		result_rf = NaTVal_rf;

		return ( result_rf );
	}
	else if ( (op_type == SIGNALING_NAN) ||
		  (op_type == QUIET_NAN) ||
		  (rsexp >= 64 + EXP_BIAS)
		)
	{
#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxuftrunc: setting invalid flag\n\n");
#endif
		sf->b.invalid_flag = 1; /* set invalid flag */

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtfxutrunc:  sf = %016llx\n", sf->i);
	fprintf(stderr, "               sf->b.invalid_flag = %llx\n", sf->b.invalid_flag);
#endif

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		result_rf.frac = ULONGLONG_MAX;

		return ( result_rf );
	}

	/* take care of zeroes */

	if ( (rsexp == 0) && (rsfrac == 0ull) )
	{
		/* rs is a (signed) zero */

		return ( Zero_rf );
	}

	/* negative values are invalid */

	if ( rssign != 0 )
	{
		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		result_rf.frac = ULONGLONG_MAX;

		return ( result_rf );
	}

	rsexp -= EXP_BIAS; /* subtract exponent bias */

	if ( rsexp <= -1 )
	{
		sf->b.inexact_flag = 1; /* set inexact flag */

		result_rf.sign = 0;
		result_rf.exp = 0;
		result_rf.frac = 0;

		return ( result_rf );
	}
	else
	{
		guard_bits = 0;
		shift_count = 63 - rsexp;

		if ( shift_count != 0 )
		{
			guard_bits = (rsfrac << (64 - shift_count));
			rsfrac >>= shift_count;
		}

		result_rf.sign = 0;
		result_rf.exp = 0x1003e;
		result_rf.frac = rsfrac;

		if ( guard_bits != 0 )
			sf->b.inexact_flag = 1; /* set inexact flag */

#ifdef FP_DEBUG
	fprintf(stderr, "fpcvtfxutrunc:  returning\n");
	fprintf(stderr, "           result_rf.sign = %x\n", result_rf.sign);
	fprintf(stderr, "           result_rf.exp = %x\n", result_rf.exp);
	fprintf(stderr, "           result_rf.frac = %016llx\n", result_rf.frac);
#endif

		return ( result_rf );
	}
}

/* convert signed integer to floating point */

float_rf
fcvtxf( const float_rf *rs )
{
int32_t	rsexp;
uint64_t rsfrac;
float_rf result_rf;
int32_t shift_count;

#ifdef FP_DEBUG
	fprintf(stderr, "fcvtxf: rs.sign = %x\n", rs->sign);
	fprintf(stderr, "        rs.exp = %x\n", rs->exp);
	fprintf(stderr, "        rs.frac = %016llx\n", rs->frac);
#endif

	rsexp = rs->exp;
	rsfrac = rs->frac;

	/* check for a NaTVal input */

	if ( (rsexp == EXP_NATVAL) && (rsfrac == (1ull << 61)) )
	{
		result_rf = *rs;

		return ( result_rf );
	}

	/* special case zero */

	if ( rsfrac == 0ull )
	{
		result_rf.sign = 0;
		result_rf.exp = 0;
		result_rf.frac = 0ull;

		return ( result_rf );
	}

	/* special case -2**63 */

	if ( rsfrac == HIGH_BIT )
	{
		result_rf.sign = 1;
		result_rf.exp = 0x1003e;
		result_rf.frac = HIGH_BIT;

		return ( result_rf );
	}

	result_rf.sign = 0;

	if ( rsfrac & HIGH_BIT )
	{
		result_rf.sign = 1;

		rsfrac = ~rsfrac;
		rsfrac += 1;
	}
		
	shift_count = renorm( &rsfrac );

	rsexp -= shift_count;

	result_rf.exp = rsexp;
	result_rf.frac = rsfrac;

	return ( result_rf );
}

int32_t
fcmp_eq( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
uint32_t rtsign;
int32_t	rtexp;
uint64_t rtfrac;
int32_t op_type;
int32_t result;

	if(fp_software_assistance_required(op_fcmp,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result = 0;

			return ( result );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result = 0;
		}
		else /* op_type == QUIET_NAN */
		{
			result = 0;
		}
	}
	else if ( (rsfrac == 0) && (rtfrac == 0) ) /* take care of zeroes next */
	{
		/* rs and rt are both zero */

		result = 1;
	}
	else if ( rssign != rtsign )
	{
		result = 0;
	}
	else
	{
		if ( (rsexp == rtexp) && (rsfrac == rtfrac) )
			result = 1;
		else
			result = 0;
	}

	return ( result );
}

int32_t
fcmp_lt( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
uint32_t rtsign;
int32_t	rtexp;
uint64_t rtfrac;
int32_t op_type;
int32_t result;

	if(fp_software_assistance_required(op_fcmp,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result = 0;

			return ( result );
		}
		else /* operand is a NaN */
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result = 0;
		}
	}
	else if ( (rsfrac == 0) && (rtfrac == 0) ) /* take care of zeroes next */
	{
		/* rs and rt are both zero */

		result = 0;
	}
	else if ( rsfrac == 0 )
	{
		if ( rtsign == 0 )
			result = 1; /* rs == 0 and rt > 0 */
		else
			result = 0; /* rs == 0 and rt < 0 */
	}
	else if ( rtfrac == 0 )
	{
		if ( rssign == 0 )
			result = 0; /* rs > 0 and rt == 0 */
		else
			result = 1; /* rs < 0 and rt == 0 */
	}
	else /* both rs and rt are non-zero */
	{
		if ( rssign != rtsign )
			result = rssign;
		else if ( rssign == 0 ) /* rs > 0, rt > 0 */
		{
			if ( rsexp < rtexp )
				result = 1;
			else if ( (rsexp == rtexp) && (rsfrac < rtfrac) )
				result = 1;
			else
				result = 0;
		}
		else /* rs < 0, rt < 0 */
		{
			if ( (rsexp > rtexp) ||
			     ((rsexp == rtexp) && (rsfrac > rtfrac))
			   )
				result = 1;
			else
				result = 0;
		}
	}

	return ( result );
}

int32_t
fcmp_le( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
uint32_t rtsign;
int32_t	rtexp;
uint64_t rtfrac;
int32_t op_type;
int32_t result;

	if(fp_software_assistance_required(op_fcmp,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "fcmp_le:  op_type = %d\n\n", op_type);
#endif

		if ( op_type == NATVAL )
		{
			result = 0;

			return ( result );
		}
		else /* operand is a NaN */
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result = 0;

			return ( result );
		}
	}

#ifdef FP_DEBUG
	fprintf(stderr, "fcmp_le:  rssign = %x\n", rssign);
	fprintf(stderr, "          rsexp = %x\n", rsexp);
	fprintf(stderr, "          rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, "          rtsign = %x\n", rtsign);
	fprintf(stderr, "          rtexp = %x\n", rtexp);
	fprintf(stderr, "          rtfrac = %016llx\n", rtfrac);
	fprintf(stderr, "          sf = %016llx\n\n", sf->i);
	fprintf(stderr, "          op_type = %d\n\n", op_type);
#endif

	if ( (rsfrac == 0) && (rtfrac == 0) ) /* take care of zeroes next */
	{
#ifdef FP_DEBUG
	fprintf(stderr, "fcmp_le:  rs and rt both zero; returning 1\n\n");
#endif
		/* rs and rt are both zero */

		result = 1;

		return ( result );
	}

	if ( rssign < rtsign )
	{
		result = 0; /* rs > 0, rt < 0 */
	}
	else if ( rssign > rtsign )
	{
		result = 1; /* rs < 0, rt > 0 */
	}
	else if ( rssign == 0 )
	{	/* rs > 0, rt > 0 */

		if ( (rsexp < rtexp) ||
		     ((rsexp == rtexp) && (rsfrac <= rtfrac))
		   )
			result = 1;
		else
			result = 0;
	}
	else /* rs < 0, rt < 0 */
	{
		if ( (rsexp > rtexp) ||
		     ((rsexp == rtexp) && (rsfrac >= rtfrac))
		   )
			result = 1;
		else
			result = 0;
	}

	return ( result);
}

int32_t
fcmp_unord( const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
uint32_t rtsign;
int32_t	rtexp;
uint64_t rtfrac;
int32_t op_type;
int32_t result;

	if(fp_software_assistance_required(op_fcmp,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result = 0;

			return ( result );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result = 1;
		}
		else /* op_type == QUIET_NAN */
		{
			result = 1;
		}
	}
	else
	{
		result = 0;
	}

	return ( result );
}

/* floating point reciprocal approximation */

float_rf
frcpa( int32_t *pr, const float_rf *rs, const float_rf *rt, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
uint32_t rtsign;
int32_t	rtexp;
uint64_t rtfrac;
int32_t op_type;
int32_t	precision;
int32_t wre;
int32_t exp_min;
int32_t exp_max;
int32_t	index;
float_rf result_rf;

#ifdef FP_DEBUG
	fprintf(stderr, "frcpa:  rs.sign = %x\n", (*rs).sign);
	fprintf(stderr, "        rs.exp = %x\n", (*rs).exp);
	fprintf(stderr, "        rs.frac = %016llx\n", (*rs).frac);
	fprintf(stderr, "        rt.sign = %x\n", (*rt).sign);
	fprintf(stderr, "        rt.exp = %x\n", (*rt).exp);
	fprintf(stderr, "        rt.frac = %016llx\n", (*rt).frac);
	fprintf(stderr, "        sf = %016llx\n\n", sf->i);
#endif

	if(fp_software_assistance_required(op_frcpa,rs,rt,NULL,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	if ( op_type = breakout_and_test2(rs, &rssign, &rsexp, &rsfrac, 
				rt, &rtsign, &rtexp, &rtfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;
			*pr = 0;

			return ( result_rf );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = Quiet_nan_rf;
			*pr = 0;

			return ( result_rf );
		}
		else /* op_type == QUIET_NAN */
		{
			/* propagate the first NaN */

			if ( (rsexp == EXP_NAN) && (rsfrac > (1ull << 63)) )
			{
				result_rf = *rs;
				*pr = 0;

				// rickc add:
				// according to Sphinx encountering a QuietNAN will
				// generate an fp exception
				sf->b.invalid_flag = 1; /* set invalid flag */
				return ( result_rf );
			}
			else if ( (rtexp == EXP_NAN) && (rtfrac > (1ull << 63)) )
			{
				result_rf = *rt;
				*pr = 0;

				// rickc add:
				// according to Sphinx encountering a QuietNAN will
				// generate an fp exception
				sf->b.invalid_flag = 1; /* set invalid flag */

				return ( result_rf );
			}
		}
	}

	if ( (rsexp == EXP_INF) && (rtexp == EXP_INF) )
	{
		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf = Quiet_nan_rf;
		*pr = 0;

		return ( result_rf );
	}
	else if ( rtexp == EXP_INF )
	{
		result_rf.sign = (rssign ^ rtsign);
		result_rf.exp = 0;
		result_rf.frac = 0;
		*pr = 0;

		return ( result_rf );
	}
	else if ( rsexp == EXP_INF )
	{
		result_rf.sign = (rssign ^ rtsign);
		result_rf.exp = EXP_INF;
		result_rf.frac = HIGH_BIT;
		*pr = 0;

		return ( result_rf );
	}
	else if ( (rsexp == 0) && (rtexp == 0) )
	{
		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf = Quiet_nan_rf;
		*pr = 0;

		return ( result_rf );
	}
	else if ( rsexp == 0 )
	{
		result_rf.sign = (rssign ^ rtsign);
		result_rf.exp = 0;
		result_rf.frac = 0;
		*pr = 0;

		return ( result_rf );
	}
	else if ( rtexp == 0 )
	{
		sf->b.zero_div_flag = 1;

		result_rf = Quiet_nan_rf;
		*pr = 0;

		return ( result_rf );
	}

	precision = sf->b.pc;

	wre = sf->b.wre;	/* widest range exponent */

	if ( precision == SNGL )
	{
		exp_min = (wre) ? WEXP_MIN : SEXP_MIN;
		exp_max = (wre) ? WEXP_MAX : SEXP_MAX;
	}
	else if ( precision == DBL )
	{
		exp_min = (wre) ? WEXP_MIN : DEXP_MIN;
		exp_max = (wre) ? WEXP_MAX : DEXP_MAX;
	}
	else
	{
		exp_min = (wre) ? WEXP_MIN : DEEXP_MIN;
		exp_max = (wre) ? WEXP_MAX : DEEXP_MAX;
	}

	/* determine whether we can use the Newton-Raphson method
	 * according to the following criteria:
	 * 1. Quotient rs/rt must be within 2**(exp_min+2) and 2**(exp_max-2).
	 * 2. Reciprocal of rt must be within 2**(exp_min+2) and 2**(exp_max-2).
	 * 3. Ulp(1/rt) must be within 2**(exp_min+2) and 2**(exp_max-2).
	 * 4. Ulp(rs) must be within 2**(exp_min+2) and 2**(exp_max-2).
	 * 5. Ulp(rs/rt) must be within 2**(exp_min+2) and 2**(exp_max-2).
	 */

	/* first check that the quotient is easily representable */

	if ( (rsexp - rtexp - 1 <= exp_min + 2) || (rsexp - rtexp + 1 >= exp_max - 2) )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "frcpa:  pt. 1\n");
#endif
		*pr = 0;
		result_rf = soft_div( rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac, sf );
		return ( result_rf );
	}

	/* next check that the reciprocal of rt is easily representable */

	if ( (-rtexp + EXP_BIAS - 1 <= exp_min + 2) || (-rtexp + EXP_BIAS >= exp_max - 2) )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "frcpa:  pt. 2\n");
#endif
		*pr = 0;
		result_rf = soft_div( rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac, sf );
		return ( result_rf );
	}

	/* check that ulp(1/rt) is not too small */

	if ( (-rtexp + EXP_BIAS - 1 ) <= exp_min + 2 )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "frcpa:  pt. 3\n");
#endif
		*pr = 0;
		result_rf = soft_div( rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac, sf );
		return ( result_rf );
	}

	/* check that ulp(rs) is not too small */

	if ( (rsexp - EXP_BIAS) <= exp_min + 2 )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "frcpa:  pt. 4\n");
#endif
		*pr = 0;
		result_rf = soft_div( rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac, sf );
		return ( result_rf );
	}
	/* check that ulp(rs/rt) is not too small */

	if ( (rsexp - rtexp - 1) <= exp_min + 2 )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "frcpa:  pt. 5\n");
#endif
		*pr = 0;
		result_rf = soft_div( rssign, rsexp, rsfrac, rtsign, rtexp, rtfrac, sf );
		return ( result_rf );
	}

	/* generate reciprocal approximation by a table lookup */

	result_rf.sign = rtsign;
	result_rf.exp = 0x1ffff - 2 - rtexp;
	index = ((rtfrac >> 55) & 0xff);

	result_rf.frac = (HIGH_BIT | ((uint64_t)recip_table[index] << 53));

	*pr = 1;

	return ( result_rf );
}

float_rf
soft_div( uint32_t rssign, int32_t rsexp, uint64_t rsfrac,
	  uint32_t rtsign, int32_t rtexp, uint64_t rtfrac,
	  status_field *sf )
{
int32_t	precision;
int32_t mantwidth;
uint64_t carry;
float_rf result_rf;
int32_t	shift_count;
int32_t	i;
uint32_t qsign;
int32_t	qexp;
uint64_t qfrac;
uint64_t guard_bits;

#ifdef FP_DEBUG
	fprintf(stderr, "soft_div: rssign = %x\n", rssign);
	fprintf(stderr, "          rsexp = %x\n", rsexp);
	fprintf(stderr, "          rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, "          rtsign = %x\n", rtsign);
	fprintf(stderr, "          rtexp = %x\n", rtexp);
	fprintf(stderr, "          rtfrac = %016llx\n\n", rtfrac);
#endif

	qsign = (rssign ^ rtsign);

	qexp = rsexp - rtexp;

	if ( rsfrac < rtfrac )
	{
		/* we need to shift the dividend to ensure enough bits
		 * in quotient
		 */

		carry = 1;
		rsfrac <<= 1;
		qexp--;
	}

	precision = sf->b.pc;

	if ( precision == SNGL )
		mantwidth = 24;
	else if ( precision == DBL )
		mantwidth = 53;
	else
		mantwidth = 64;

#ifdef FP_DEBUG
	fprintf(stderr, "soft_div: mantwidth = %d\n\n", mantwidth);
#endif

	/* the standard division algorithm using repeated subtraction */

	qfrac = 0ull;

	for ( i=0; i<mantwidth; i++ )
	{
		qfrac <<= 1;

		if ( carry == 1 )
		{
			qfrac |= 1ull;
			rsfrac -= rtfrac;
			carry = 0;
		}
		else if ( rtfrac <= rsfrac )
		{
			qfrac |= 1ull;
			rsfrac -= rtfrac;
		}

		if ( rsfrac & HIGH_BIT )
			carry = 1;

		rsfrac <<= 1;

#ifdef FP_DEBUG
	fprintf(stderr, "soft_div: i = %d\n", i);
	fprintf(stderr, "          qfrac = %016llx\n", qfrac);
	fprintf(stderr, "          rsfrac = %016llx\n\n", rsfrac);
#endif
	}

#ifdef FP_DEBUG
	fprintf(stderr, "soft_div: after loop qfrac = %016llx\n\n", qfrac);
#endif

	guard_bits = 0ull;

	if ( (carry == 1) || (rtfrac <= rsfrac) )
	{
		guard_bits = GUARD_BIT;
		rsfrac -= rtfrac;
	}

	if ( rsfrac != 0ull )
		guard_bits |= STICKY_BIT;

	shift_count = renorm( &qfrac );

	if ( shift_count != 0 )
	{
		qfrac |= (guard_bits >> (64 - shift_count));
		guard_bits <<= shift_count;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "soft_div: qsign = %x\n", qsign);
	fprintf(stderr, "          qexp = %x\n", qexp);
	fprintf(stderr, "          qfrac = %016llx\n", qfrac);
	fprintf(stderr, "          guard_bits = %016llx\n", guard_bits);
#endif

	result_rf = round(qsign, qexp, qfrac, guard_bits, sf);

	return ( result_rf );
}

/* floating point reciprocal square root approximation */

float_rf
frsqrta( int32_t *pr, const float_rf *rs, status_field *sf )
{
uint32_t rssign;
int32_t	rsexp;
uint64_t rsfrac;
int32_t op_type;
int32_t	precision;
int32_t wre;
int32_t exp_min;
int32_t exp_max;
float_rf result_rf;

	if(fp_software_assistance_required(op_frsqrta,rs,NULL,NULL,0)){
	  sf->b.ISR_fpa = 1;
	  return(Zero_rf);
	}

	if ( op_type = breakout_and_test(rs, &rssign, &rsexp, &rsfrac, sf)
	   )
	{
		if ( op_type == NATVAL )
		{
			result_rf = NaTVal_rf;
			*pr = 0;

			return ( result_rf );
		}
		else if ( op_type == SIGNALING_NAN )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			result_rf = Quiet_nan_rf;
			*pr = 0;

			return ( result_rf );
		}
		else /* op_type == QUIET_NAN */
		{
			/* propagate the NaN */

		        // rickc add:
			// according to Sphinx encountering a QuietNAN will
			// generate an fp exception
			sf->b.invalid_flag = 1; /* set invalid flag */

			result_rf = *rs;
			*pr = 0;

			return ( result_rf );
		}
	}

	if ( rsexp == EXP_INF )
	{
		if ( rssign == 0 )
		{
			result_rf = *rs;
			*pr = 0;

			return ( result_rf );
		}

		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf = Quiet_nan_rf;
		*pr = 0;

		return ( result_rf );
	}
	else if ( rsexp == 0 )
	{
		result_rf = *rs;
		*pr = 0;

		return ( result_rf );
	}
	else if ( rssign == 1 )
	{
		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf = Quiet_nan_rf;
		*pr = 0;

		return ( result_rf );
	}

	precision = sf->b.pc;

	wre = sf->b.wre;	/* widest range exponent */

	if ( precision == SNGL )
	{
		exp_min = (wre) ? WEXP_MIN : SEXP_MIN - 23;
		exp_max = (wre) ? WEXP_MAX : SEXP_MAX;
	}
	else if ( precision == DBL )
	{
		exp_min = (wre) ? WEXP_MIN : DEXP_MIN - 52;
		exp_max = (wre) ? WEXP_MAX : DEXP_MAX;
	}
	else
	{
		exp_min = (wre) ? WEXP_MIN : DEEXP_MIN - 63;
		exp_max = (wre) ? WEXP_MAX : DEEXP_MAX;
	}

	if ( (rsexp - EXP_BIAS) > exp_max )
	{
		if ( rssign == 0 )
		{
			result_rf = Infinity_rf;
			*pr = 0;

			return ( result_rf );
		}

		sf->b.invalid_flag = 1; /* set invalid flag */

		result_rf = Quiet_nan_rf;
		*pr = 0;

		return ( result_rf );
	}
	else if ( (rsexp - EXP_BIAS) < exp_min )
	{
		result_rf.sign = rssign;
		result_rf.exp = 0;
		result_rf.frac = 0;

		return ( result_rf );
	}

	result_rf = recip_sqrt(rssign, rsexp, rsfrac);

	if ( (result_rf.exp == 0) && (result_rf.frac == 0) )
		*pr = 0;
	else if ( (result_rf.exp == EXP_INF) && (result_rf.frac == HIGH_BIT) )
		*pr = 0;

	*pr = 1;

	return ( result_rf );

}

float_rf
recip_sqrt( uint32_t rssign, int32_t rsexp, uint64_t rsfrac )
{
float_rf result_rf;
int32_t	index;

	result_rf.sign = rssign;
	result_rf.exp = 0xfffe - ((rsexp - EXP_BIAS) >> 1);
	index = ((rsexp & 1) << 7) | ((rsfrac >> 56) & 0x7f);
	result_rf.frac = HIGH_BIT | ((uint64_t)recip_sqrt_table[index] << 53);

	return ( result_rf );
}

float_rf
madd( uint32_t rssign, int32_t rsexp, uint64_t rsfrac,
	uint32_t rtsign, int32_t rtexp, uint64_t rtfrac,
	uint32_t rrsign, int32_t rrexp, uint64_t rrfrac,
	status_field *sf )
{
float_rf result;
int32_t shift_count;
uint32_t prdsign;
int32_t prdexp;
uint64_t prdfrac[2];
uint32_t sumsign;
int32_t sumexp;
uint64_t guard_bits;
uint32_t carry;
uint32_t carry_in, carry_out;
uint64_t sfrac[4], tfrac[4];
uint32_t neg;

#ifdef FP_DEBUG
	fprintf(stderr, "madd: rssign = %x\nrsexp = %x\nrsfrac = %016llx\n", 
			rssign, rsexp, rsfrac);
	fprintf(stderr, "      rtsign = %x\nrtexp = %x\nrtfrac = %016llx\n", 
			rtsign, rtexp, rtfrac);
	fprintf(stderr, "      rrsign = %x\nrrexp = %x\nrrfrac = %016llx\n", 
			rrsign, rrexp, rrfrac);
#endif

	if ( (rsexp == EXP_INF) && (rtexp == EXP_INF) && (rrexp == EXP_INF) )
	{
		/* rs == rt == rr == +/-inf */

		if ( (rssign ^ rtsign) == rrsign )
		{
			result.sign = rrsign;
			result.exp = rrexp;

			result.frac = HIGH_BIT;

			return ( result );
		}

		sf->b.invalid_flag = 1; /* set invalid flag */

		/* return a quiet NaN */

		result = Quiet_nan_rf;

		return ( result );
	}

	if ( (rsexp == EXP_INF) && (rtexp == EXP_INF) )
	{
		/* rs and rt are infinite; rr is finite */

		result.sign = (rssign ^ rtsign);
		result.exp = rsexp;

		result.frac = HIGH_BIT;

		return ( result );
	}

	/* interchange rs and rt if rs is finite and rt is infinite */

	if ( (rsexp != EXP_INF) && (rtexp == EXP_INF) )
	{
		rssign ^= rtsign;
		rtsign ^= rssign;
		rssign ^= rtsign;

		rsexp ^= rtexp;
		rtexp ^= rsexp;
		rsexp ^= rtexp;

		rsfrac ^= rtfrac;
		rtfrac ^= rsfrac;
		rsfrac ^= rtfrac;
	}

	if ( (rsexp == EXP_INF) && (rrexp == EXP_INF) )
	{
		/* rs and rr are infinite; rt is finite */

		if ( ((rtexp == 0) && (rtfrac == 0ll)) ||
		     ((rssign ^ rtsign) != rrsign)
		   )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			/* return a quiet NaN */
	
			result = Quiet_nan_rf;
	
			return ( result );
		}
		else
		{
			/* return rr */

			result.sign = rrsign;
			result.exp = rrexp;

			result.frac = rrfrac;

			return ( result );
		}
	}

	if ( rsexp == EXP_INF )
	{
		if ( (rtexp == 0) && (rtfrac == 0ll) )
		{
			sf->b.invalid_flag = 1; /* set invalid flag */
	
			/* return a quiet NaN */
	
			result = Quiet_nan_rf;
	
			return ( result );
		}

		result.sign = (rssign ^ rtsign);
		result.exp = rsexp;

		result.frac = rsfrac;

		return ( result );
	}

	if ( rrexp == EXP_INF )
	{
		/* return rr */

		result.sign = rrsign;
		result.exp = rrexp;

		result.frac = rrfrac;

		return ( result );
	}

	if ( ((rsexp == 0) && (rsfrac == 0ull)) ||
	     ((rtexp == 0) && (rtfrac == 0ull))
	   )
	{
		/* rs == +/-0.0 or rt == +/-0.0 */

		if ( (rrexp == 0) && (rrfrac == 0ull) )
		{
			/* rr == +/-0.0 */

			if ( sf->b.rc == FP_RM )
			{
				/* rounding mode is round to minus infinity */

				result.sign = ((rssign ^ rtsign) | rrsign);
				result.exp = 0;
				result.frac = 0;
			}
			else
			{
				result.sign = ((rssign ^ rtsign) & rrsign);
				result.exp = 0;
				result.frac = 0;
			}

			return ( result );
		}
		else
		{
		  // some changes here to handle the Intel PAL tricks
		  guard_bits = 0x0;
		  /*unsigned long long sf_pc = sf->b.pc;
		    sf->b.pc = DBL_EXT;*/
		  rrexp -= EXP_BIAS;
		  result = round(rrsign,rrexp,rrfrac,guard_bits,sf);
		  //sf->b.pc = sf_pc;
			/*result.sign = rrsign;
			result.exp = rrexp;
			result.frac = rrfrac;*/

			return ( result );
		}
	}

	/* at this point, both rs and rt are non-zero, finite numbers */

	rsexp -= EXP_BIAS;   /* subtract exponent bias */
	rtexp -= EXP_BIAS;

	prdsign = (rssign ^ rtsign);
	prdexp = rsexp + rtexp;
	prdfrac[0] = mul64(rsfrac, rtfrac, &prdfrac[1]);

	/* prdfrac[] contains the product of rsfrac and rtfrac */

	/* renormalize prdfrac[0] so that the high bit is set */

	shift_count = renorm( &prdfrac[0] );

	/* the shift count is either 0 or 1, since the product of
	 * rsfrac and rtfrac contains either 123 or 124 bits */

	if ( shift_count == 0 )
	{
		/* adjust exponent of product, since the product of the
		 * fractions is >= 2.
		 */

		prdexp++;
	}
	else
	{
		prdfrac[0] |= (prdfrac[1] >> (64 - shift_count));
		prdfrac[1] <<= shift_count;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "madd: prdsign = %x\n", prdsign);
	fprintf(stderr, "      prdexp = %x\n", prdexp);
	fprintf(stderr, "      prdfrac[0] = %016llx\n", prdfrac[0]);
	fprintf(stderr, "      prdfrac[1] = %016llx\n", prdfrac[1]);
#endif

	if ( (rrexp == 0) && (rrfrac == 0ull) )
	{
		/* rr = +/-0    */

		result = round(prdsign, prdexp, prdfrac[0], prdfrac[1], sf);

		return ( result );
	}

	rrexp -= EXP_BIAS;   /* subtract exponent bias */

	if ( (prdsign != rrsign) && (prdexp == rrexp) &&
	     (prdfrac[0] == rrfrac) && (prdfrac[1] == 0ull)
	   )
	{
		/* prd = -rr */

		if ( sf->b.rc == FP_RM )
		{
			/* rounding mode is round to minus infinity */

			result.sign = (prdsign | rrsign);
			result.exp = 0;
			result.frac = 0;
		}
		else
		{
			result.sign = (prdsign & rrsign);
			result.exp = 0;
			result.frac = 0;
		}

		return ( result );
	}

	/* in extreme cases, the smaller operand only affects the sticky
	 * bits, so we can simply readjust the exponent of the smaller term
	 */

	if ( rrexp < prdexp - 130 )
		rrexp = prdexp - 130;

	if ( prdexp < rrexp - 66 )
		prdexp = rrexp - 66;

	/* copy the larger of |prd| and |rr| to sfrac   */

	if ( (prdexp > rrexp) ||
	     ((prdexp == rrexp) && (prdfrac[0] >= rrfrac))
	   )
	{
		sfrac[0] = prdfrac[0];
		sfrac[1] = prdfrac[1];
		sfrac[2] = 0ull;
		sfrac[3] = 0ull;

		sumsign = prdsign;
		sumexp = prdexp;

		tfrac[0] = rrfrac;
		tfrac[1] = 0ull;
		tfrac[2] = 0ull;
		tfrac[3] = 0ull;

		shift_count = prdexp - rrexp;
	}
	else
	{
		sfrac[0] = rrfrac;
		sfrac[1] = 0ull;
		sfrac[2] = 0ull;
		sfrac[3] = 0ull;

		sumsign = rrsign;
		sumexp = rrexp;

		tfrac[0] = prdfrac[0];
		tfrac[1] = prdfrac[1];
		tfrac[2] = 0ull;
		tfrac[3] = 0ull;

		shift_count = rrexp - prdexp;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "sumsign = %x\n", sumsign);
	fprintf(stderr, "sumexp = %x\n", sumexp);
	fprintf(stderr, "sfrac[0] = %016llx\n", sfrac[0]);
	fprintf(stderr, "sfrac[1] = %016llx\n", sfrac[1]);
	fprintf(stderr, "sfrac[2] = %016llx\n", sfrac[2]);
	fprintf(stderr, "sfrac[3] = %016llx\n", sfrac[3]);
	fprintf(stderr, "tfrac[0] = %016llx\n", tfrac[0]);
	fprintf(stderr, "tfrac[1] = %016llx\n", tfrac[1]);
	fprintf(stderr, "tfrac[2] = %016llx\n", tfrac[2]);
	fprintf(stderr, "tfrac[3] = %016llx\n\n", tfrac[3]);
#endif

	/*
	 * line up the binary points of sfrac and tfrac by shifting
	 * the smaller to the right
	 */

	if ( shift_count >= 128 )
	{
		tfrac[3] = tfrac[1];
		tfrac[2] = tfrac[0];
		tfrac[1] = 0ull;
		tfrac[0] = 0ull;

		shift_count -= 128;
	}
	else if ( shift_count >= 64 )
	{
		tfrac[2] = tfrac[1];
		tfrac[1] = tfrac[0];
		tfrac[0] = 0ull;

		shift_count -= 64;
	}

	if ( shift_count > 0 )
	{
		tfrac[3] >>= shift_count;
		tfrac[3] |= (tfrac[2] << (64 - shift_count));
		tfrac[2] >>= shift_count;
		tfrac[2] |= (tfrac[1] << (64 - shift_count));
		tfrac[1] >>= shift_count;
		tfrac[1] |= (tfrac[0] << (64 - shift_count));
		tfrac[0] >>= shift_count;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "sfrac[0] = %016llx\n", sfrac[0]);
	fprintf(stderr, "sfrac[1] = %016llx\n", sfrac[1]);
	fprintf(stderr, "sfrac[2] = %016llx\n", sfrac[2]);
	fprintf(stderr, "sfrac[3] = %016llx\n", sfrac[3]);
	fprintf(stderr, "tfrac[0] = %016llx\n", tfrac[0]);
	fprintf(stderr, "tfrac[1] = %016llx\n", tfrac[1]);
	fprintf(stderr, "tfrac[2] = %016llx\n", tfrac[2]);
	fprintf(stderr, "tfrac[3] = %016llx\n\n", tfrac[3]);
#endif

	neg = 0;

	if ( prdsign != rrsign )
	{
		/* form two's complement of tfrac */

		neg = 1;

		carry = 0;

		if ( (tfrac[1] == 0ull) && (tfrac[2] == 0ull) && (tfrac[3] == 0ull) )
			carry = 1;

		tfrac[0] = ~tfrac[0] + carry;

		carry = 0;

		if ( (tfrac[2] == 0ull) && (tfrac[3] == 0ull) )
			carry = 1;

		tfrac[1] = ~tfrac[1] + carry;

		carry = 0;

		if ( tfrac[3] == 0ull )
			carry = 1;

		tfrac[2] = ~tfrac[2] + carry;

		tfrac[3] = ~tfrac[3] + 1ull;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "sfrac[0] = %016llx\n", sfrac[0]);
	fprintf(stderr, "sfrac[1] = %016llx\n", sfrac[1]);
	fprintf(stderr, "sfrac[2] = %016llx\n", sfrac[2]);
	fprintf(stderr, "sfrac[3] = %016llx\n", sfrac[3]);
	fprintf(stderr, "tfrac[0] = %016llx\n", tfrac[0]);
	fprintf(stderr, "tfrac[1] = %016llx\n", tfrac[1]);
	fprintf(stderr, "tfrac[2] = %016llx\n", tfrac[2]);
	fprintf(stderr, "tfrac[3] = %016llx\n\n", tfrac[3]);
#endif

	/* just add up sfrac and tfrac */

	carry_out = 0;

	if ( sfrac[3] > ~tfrac[3] )
	        carry_out = 1;

	tfrac[3] += sfrac[3];

	carry_in = carry_out;

	carry_out = 0;

	if ( (tfrac[2] == ULONGLONG_MAX) && (carry_in != 0ull) )
	        carry_out = 1;

	tfrac[2] += carry_in;

	if ( sfrac[2] > ~tfrac[2] )
	        carry_out += 1;

	tfrac[2] += sfrac[2];

	carry_in = carry_out;

	carry_out = 0;

	if ( (tfrac[1] == ULONGLONG_MAX) && (carry_in != 0ull) )
	        carry_out = 1;

	tfrac[1] += carry_in;

	if ( sfrac[1] > ~tfrac[1] )
	        carry_out += 1;

	tfrac[1] += sfrac[1];

	carry_in = carry_out;

	carry_out = 0;

	if ( (tfrac[0] == ULONGLONG_MAX) && (carry_in != 0ull) )
	        carry_out = 1;

	tfrac[0] += carry_in;

	if ( sfrac[0] > ~tfrac[0] )
	        carry_out += 1;

	tfrac[0] += sfrac[0];

#ifdef FP_DEBUG
	fprintf(stderr, "neg = %x\n", neg);
	fprintf(stderr, "carry_out = %x\n\n", carry_out);
#endif

	carry_out -= neg;

	if ( carry_out == 1 )
	{
		tfrac[3] >>= 1;
		tfrac[3] |= (tfrac[2] << 63);
		tfrac[2] >>= 1;
		tfrac[2] |= (tfrac[1] << 63);
		tfrac[1] >>= 1;
		tfrac[1] |= (tfrac[0] << 63);
		tfrac[0] >>= 1;
		tfrac[0] |= (1ull << 63);

		sumexp++;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "sumexp = %x\n", sumexp);
	fprintf(stderr, "sfrac[0] = %016llx\n", sfrac[0]);
	fprintf(stderr, "sfrac[1] = %016llx\n", sfrac[1]);
	fprintf(stderr, "sfrac[2] = %016llx\n", sfrac[2]);
	fprintf(stderr, "sfrac[3] = %016llx\n", sfrac[3]);
	fprintf(stderr, "tfrac[0] = %016llx\n", tfrac[0]);
	fprintf(stderr, "tfrac[1] = %016llx\n", tfrac[1]);
	fprintf(stderr, "tfrac[2] = %016llx\n", tfrac[2]);
	fprintf(stderr, "tfrac[3] = %016llx\n\n", tfrac[3]);
#endif

	/* now normalize result */

	if ( tfrac[0] == 0 )
	{
		tfrac[0] = tfrac[1];
		tfrac[1] = tfrac[2];
		tfrac[2] = tfrac[3];
		tfrac[3] = 0;

		sumexp -= 64;
	}

	shift_count = renorm( &tfrac[0] );

	if ( shift_count != 0 )
	{
		tfrac[0] |= (tfrac[1] >> (64 - shift_count));
		tfrac[1] <<= shift_count;
		tfrac[1] |= (tfrac[2] >> (64 - shift_count));
		tfrac[2] <<= shift_count;
		tfrac[2] |= (tfrac[3] >> (64 - shift_count));
		tfrac[3] <<= shift_count;

		sumexp -= shift_count;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "sumexp = %x\n", sumexp);
	fprintf(stderr, "sfrac[0] = %016llx\n", sfrac[0]);
	fprintf(stderr, "sfrac[1] = %016llx\n", sfrac[1]);
	fprintf(stderr, "sfrac[2] = %016llx\n", sfrac[2]);
	fprintf(stderr, "sfrac[3] = %016llx\n", sfrac[3]);
	fprintf(stderr, "tfrac[0] = %016llx\n", tfrac[0]);
	fprintf(stderr, "tfrac[1] = %016llx\n", tfrac[1]);
	fprintf(stderr, "tfrac[2] = %016llx\n", tfrac[2]);
	fprintf(stderr, "tfrac[3] = %016llx\n\n", tfrac[3]);
#endif

	/* now set up guard and sticky bits prior to rounding */

	guard_bits = tfrac[1];

	if ( tfrac[2] != 0 )
	        guard_bits |= STICKY_BIT;

	if ( tfrac[3] != 0 )
	        guard_bits |= STICKY_BIT;

	result = round(sumsign, sumexp, tfrac[0], guard_bits, sf);

	return ( result );
}


/*************************** fpround.C ***************************/

/**************************************************************************
 *                                                                        *
 *               Copyright (C) 1994, Silicon Graphics, Inc.               *
 *                                                                        *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *                                                                        *
 **************************************************************************/

/* ====================================================================
 * ====================================================================
 *
 * Module: fpround.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/float_rf.cxx,v $
 *
 * Revision history:
 *  12-Jun-94 - Original Version
 *
 * Description:	contains subroutines round_l, round
 *
 * ====================================================================
 * ====================================================================
 */

#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>

#if !defined(linux)
#include <inttypes.h>
#else
#include <sys/types.h>
#endif


/*
	round to nearest


sign	round	guard	sticky	   add to round
	 bit	 bit	 bit	      bit
  0	  0	  0	  0	  	0
  0	  0	  0	  1	  	0
  0	  0	  1	  0	  	0
  0	  0	  1	  1	  	1
  0	  1	  0	  0	  	0
  0	  1	  0	  1	  	0
  0	  1	  1	  0	  	1
  0	  1	  1	  1	  	1
  1	  0	  0	  0	  	0
  1	  0	  0	  1	  	0
  1	  0	  1	  0	  	0
  1	  0	  1	  1	  	1
  1	  1	  0	  0	  	0
  1	  1	  0	  1	  	0
  1	  1	  1	  0	  	1
  1	  1	  1	  1	  	1


	round to -infinity


sign	round	guard	sticky	   add to round
	 bit	 bit	 bit	      bit
  0	  0	  0	  0	  	0
  0	  0	  0	  1	  	0
  0	  0	  1	  0	  	0
  0	  0	  1	  1	  	0
  0	  1	  0	  0	  	0
  0	  1	  0	  1	  	0
  0	  1	  1	  0	  	0
  0	  1	  1	  1	  	0
  1	  0	  0	  0	  	0
  1	  0	  0	  1	  	1
  1	  0	  1	  0	  	1
  1	  0	  1	  1	  	1
  1	  1	  0	  0	  	0
  1	  1	  0	  1	  	1
  1	  1	  1	  0	  	1
  1	  1	  1	  1	  	1


	round to +infinity


sign	round	guard	sticky	   add to round
	 bit	 bit	 bit	      bit
  0	  0	  0	  0	  	0
  0	  0	  0	  1	  	1
  0	  0	  1	  0	  	1
  0	  0	  1	  1	  	1
  0	  1	  0	  0	  	0
  0	  1	  0	  1	  	1
  0	  1	  1	  0	  	1
  0	  1	  1	  1	  	1
  1	  0	  0	  0	  	0
  1	  0	  0	  1	  	0
  1	  0	  1	  0	  	0
  1	  0	  1	  1	  	0
  1	  1	  0	  0	  	0
  1	  1	  0	  1	  	0
  1	  1	  1	  0	  	0
  1	  1	  1	  1	  	0


	round to zero


sign	round	guard	sticky	   add to round
	 bit	 bit	 bit	      bit
  0	  0	  0	  0	  	0
  0	  0	  0	  1	  	0
  0	  0	  1	  0	  	0
  0	  0	  1	  1	  	0
  0	  1	  0	  0	  	0
  0	  1	  0	  1	  	0
  0	  1	  1	  0	  	0
  0	  1	  1	  1	  	0
  1	  0	  0	  0	  	0
  1	  0	  0	  1	  	0
  1	  0	  1	  0	  	0
  1	  0	  1	  1	  	0
  1	  1	  0	  0	  	0
  1	  1	  0	  1	  	0
  1	  1	  1	  0	  	0
  1	  1	  1	  1	  	0

*/

/* the following table is used to calculate the carry bit
 * based on rounding-mode, sign, 64th bit of mantissa
 * (round bit), 65th bit of mantissa (guard bit), and the
 * "or" of the remaining mantissa bits (sticky bit)
 */

static const int8_t	carry_bit[4][2][2][2][2] = {
0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, /* round to nearest */
0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, /* round to -infinity */
0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, /* round to +infinity */
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  /* round to zero */
};

/* overflow table is indexed by rounding mode and sign bit.
 */

static int64_t	overflow_result[4][2] =
{
INFINITY,	MINFINITY, /* round to nearest */
MAXVAL,		MINFINITY, /* round to -inf */
INFINITY,	MMAXVAL,   /* round to +inf */
MAXVAL,		MMAXVAL    /* round to zero */
};

/* ====================================================================
 *
 * FunctionName		round_l
 *
 * Description		rounds a 64 bit integer value
 *
 * ====================================================================
 */

/* This routine rounds a 64 bit integer value, using the IEEE 754
 * rounding rules.
 * Guard and sticky bits are bits 63 and 62-0, respectively, of guard_bits.
 * Sticky bits may or may not be consolidated to a single bit (bit 62)
 * prior to input to this routine.
 * (see restrictions below, however)
 */

float_rf
round_l( uint32_t sign, int64_t rs, uint64_t guard_bits, status_field *sf )
{
int32_t	rm, rb, gb, sb;
int32_t	carry;
float_rf result_rf;

	/* consolidate sticky bits into a single bit */

	if ( (guard_bits & STICKY_MASK) != 0 )
	{
		guard_bits &= ~STICKY_MASK;
		guard_bits |= STICKY_BIT;
	}

	rm = sf->b.rc;	/* rounding mode */
	rb = rs & 1;	/* round bit */
	gb = (guard_bits >> 63);	/* guard bit */
	sb = (guard_bits >> 62) & 1;	/* sticky bit */

	carry = carry_bit[rm][sign][rb][gb][sb];

	rs += carry;

	/* in this implementation, values which will not fit into a
	 * long long without overflowing have already been screened.
	 */

	/* negate result if necessary */

	if ( sign )
		rs = -rs;

	result_rf.sign = 0;
	result_rf.exp = 0x1003e;
	result_rf.frac = rs;

	if ( guard_bits )
	{
		/* set the inexact flag */

                sf->b.inexact_flag = 1;
	}

	return ( result_rf );
}

/* ====================================================================
 *
 * FunctionName		round
 *
 * Description		rounds a register format f.p. value
 *
 * ====================================================================
 */

/* This routine rounds a floating point value, using the IEEE 754
 * rounding rules; rsfrac is the (nonzero) fractional value, normalized so
 * that bit 63 is set, and the unbiased exponent (rsexp) adjusted accordingly.
 * Guard and sticky bits are bits 63 and 62-0, respectively, of guard_bits.
 * Sticky bits may or may not be consolidated to a single bit (bit 62)
 * prior to input to this routine.
 */

float_rf
round( uint32_t rssign, int32_t rsexp, uint64_t rsfrac,
	uint64_t guard_bits, status_field *sf )
{
int32_t	precision;
int32_t	rm, rb, gb, sb;
int32_t	shift;
uint32_t rtsign;
int32_t	rtexp;
uint64_t rtfrac;
int32_t	shift_count;
uint32_t carry;
uint64_t sticky;
int32_t	tininess;
int32_t	inexact;
uint64_t guard_bits2;
int64_t	return_value;
float_rf biased_result_rf;
int32_t carry_out;
int32_t ftz;
int32_t wre;
int32_t exp_min;
int32_t exp_max;
float_rf result_rf;

#ifdef FP_DEBUG
	fprintf(stderr, "round:  rssign = %08x\n", rssign);
	fprintf(stderr, "rsexp = %08x\n", rsexp);
	fprintf(stderr, "rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, "guard_bits = %016llx\n\n", guard_bits);
#endif

	/* consolidate sticky bits into a single bit */

	if ( (guard_bits & STICKY_MASK) != 0 )
	{
		guard_bits &= ~STICKY_MASK;
		guard_bits |= STICKY_BIT;
	}

	precision = sf->b.pc;

	rm = sf->b.rc;	/* rounding mode */

	ftz = sf->b.ftz; /* Flush-to-Zero mode */

	wre = sf->b.wre;	/* widest range exponent */

	/* save rssign, rsexp, rsfrac, and guard_bits  in case of underflow */

	rtsign = rssign;
	rtexp = rsexp;
	rtfrac = rsfrac;
	guard_bits2 = guard_bits;

	tininess = FALSE;
	inexact = FALSE;

	// rickc temp hack
	int inexact_fix = 0;

	/* denormalize arg prior to rounding */

	if ( precision == SNGL )
	{
		shift = 40;
		exp_min = (wre) ? WEXP_MIN : SEXP_MIN;
		exp_max = (wre) ? WEXP_MAX : SEXP_MAX;
		guard_bits >>= 40;
		guard_bits |= (rsfrac << 24);
		rsfrac >>= 40;

		if ( (guard_bits & STICKY_MASK) != 0 )
		{
			guard_bits &= ~STICKY_MASK;
			guard_bits |= STICKY_BIT;
		}

		if ( rsexp < (exp_min - 24) )
		{
			/* set rsfrac = 0, gb = 0, sticky bit = 1
			 * (rs is non-zero)
			 */

			guard_bits = STICKY_BIT;
			rsfrac = 0;
			rsexp = exp_min;
		}
		else if ( rsexp < exp_min )
		{
			shift_count = exp_min - rsexp;
			guard_bits >>= shift_count;
			guard_bits |= (rsfrac << (64 - shift_count));
			rsfrac >>= shift_count;
			rsexp = exp_min;
		}
	}
	else if ( precision == DBL )
	{
		shift = 11;
		exp_min = (wre) ? WEXP_MIN : DEXP_MIN;
		exp_max = (wre) ? WEXP_MAX : DEXP_MAX;
		guard_bits >>= 11;
		guard_bits |= (rsfrac << 53);
		rsfrac >>= 11;

		if ( (guard_bits & STICKY_MASK) != 0 )
		{
			guard_bits &= ~STICKY_MASK;
			guard_bits |= STICKY_BIT;
		}

		if ( rsexp < (exp_min - 53) )
		{
			/* set rsfrac = 0, gb = 0, sticky bit = 1
			 * (rs is non-zero)
			 */

			guard_bits = STICKY_BIT;
			rsfrac = 0;
			rsexp = exp_min;
		}
		else if ( rsexp < exp_min )
		{
			shift_count = exp_min - rsexp;
			guard_bits >>= shift_count;
			guard_bits |= (rsfrac << (64 - shift_count));
			rsfrac >>= shift_count;
			rsexp = exp_min;
		}
	}
	else
	{
		shift = 0;
		exp_min = (wre) ? WEXP_MIN : DEEXP_MIN;
		exp_max = (wre) ? WEXP_MAX : DEEXP_MAX;

		if ( (guard_bits & STICKY_MASK) != 0 )
		{
			guard_bits &= ~STICKY_MASK;
			guard_bits |= STICKY_BIT;
		}

		if ( rsexp < (exp_min - 64) )
		{
			/* set rsfrac = 0, gb = 0, sticky bit = 1
			 * (rs is non-zero)
			 */

		         // rickc hack
		         // according to Sphinx, if bit 63 is the only bit set in the 
		         // fraction part of the number we still do not set the inexact flag
		         // on an underflow.  Something about the explicit bit I presume.
		        if(rsfrac == 0x8000000000000000ull){
		          inexact_fix = 1;
		        }

			guard_bits = STICKY_BIT;
			rsfrac = 0;
			rsexp = exp_min;
		}
		else if ( rsexp == (exp_min - 64) )
		{
			/* shift rsfrac right 64 bits */

			if ( guard_bits != 0 )
				rsfrac |= 1; /* preserve sticky bits */

			guard_bits = rsfrac;
			rsfrac = 0;
			rsexp = exp_min;
		}
		else if ( rsexp < exp_min )
		{
			/* shift rsfrac right (exp_min - rsexp) bits */

			shift_count = exp_min - rsexp;
			sticky = (guard_bits & STICKY_MASK);
			guard_bits >>= shift_count;
			guard_bits |= (rsfrac << (64 - shift_count));
			if ( sticky )
				guard_bits |= STICKY_BIT;

			rsfrac >>= shift_count;
			rsexp = exp_min;
		}
	}

	// original method by vegas
	//	if ( guard_bits )
	//		inexact = TRUE;

	// new hack (not sure if correct) to match what Sphinx
	// does for inexact exceptions
	if((guard_bits) && !inexact_fix){
	  inexact = TRUE;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "inexact = %x\n", inexact);
#endif

	/* consolidate sticky bits into a single bit */

	if ( (guard_bits & STICKY_MASK) != 0ull )
	{
		guard_bits &= ~STICKY_MASK;
		guard_bits |= STICKY_BIT;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "round:  rssign = %08x\n", rssign);
	fprintf(stderr, "rsexp = %08x\n", rsexp);
	fprintf(stderr, "rsfrac = %016llx\n", rsfrac);
	fprintf(stderr, "guard_bits = %016llx\n\n", guard_bits);
#endif

	rb = rsfrac & 1;		/* round bit */
	gb = (guard_bits >> 63);	/* guard bit */
	sb = (guard_bits >> 62) & 1;	/* sticky bit */

	carry = carry_bit[rm][rssign][rb][gb][sb];

#ifdef FP_DEBUG
	fprintf(stderr, "carry = %x\n\n", carry);
#endif

	carry_out = 0;

	if ( precision == SNGL )
	{
		rsfrac += carry;

		if ( rsfrac == (1ull << 24) )
		{
			rsexp += 1;
			rsfrac >>= 1;
		}
	}
	else if ( precision == DBL )
	{
		rsfrac += carry;

		if ( rsfrac == (1ull << 53) )
		{
			rsexp += 1;
			rsfrac >>= 1;
		}
	}
	else
	{
		if ( (rsfrac == ULONGLONG_MAX) && (carry == 1) )
			carry_out = 1;
	
		rsfrac += carry;
	
		if ( carry_out == 1 )
		{
			rsexp += 1;
			rsfrac = (1ull << 63);
		}
	}

#ifdef FP_DEBUG
	fprintf(stderr, "round:  rssign = %08x\n", rssign);
	fprintf(stderr, "rsexp = %08x\n", rsexp);
	fprintf(stderr, "rsfrac = %016llx\n\n", rsfrac);
#endif

	if ( rsexp > exp_max )
	{
#ifdef FP_DEBUG
	fprintf(stderr, "overflow\n\n");
#endif
		/* overflow has occurred */

		return_value = overflow_result[rm][rssign];

		sf->b.overflow_flag = 1;

		if ( sf->b.overflow_trap_disabled == 0 )
		{
			/* overflow trap is enabled */

			/* deliver biased result to trap handler, per IEEE 754
			 * spec.
			 */

			rsexp += EXP_BIAS;
			biased_result_rf.sign = rssign;
			biased_result_rf.exp = (rsexp & 0x1ffff);
			biased_result_rf.frac = (rsfrac << shift);

			/* set ISR_ebc bit if necessary */

			if ( rsexp != (rsexp & 0x1ffff) )
				sf->b.ISR_ebc = 1;

			/* set ISR_fpa bit if carry bit was 1 */

			// rickc change: fpa should not be raised here
			//               at least according to the Intel simulator
			if ( carry == 1 )
			  sf->b.ISR_fpa = 1;

			/* check for inexactness */

			if ( guard_bits || (rsexp != (rsexp & 0x1ffff)) )
				sf->b.inexact_flag = 1;

			return ( biased_result_rf );
		}

		/* set the inexact flag */

		sf->b.inexact_flag = 1;

		/* return appropriate value based on overflow table,
		 * precision, and rounding mode
		 */
#ifdef FP_DEBUG
	fprintf(stderr, "return_value = %016llx\n", return_value);
#endif

		if ( return_value == INFINITY )
		{
			result_rf.sign = 0;
			result_rf.exp = EXP_INF;
			result_rf.frac = (1ull << 63);
		}
		else if ( return_value == MINFINITY )
		{
			result_rf.sign = 1;
			result_rf.exp = EXP_INF;
			result_rf.frac = (1ull << 63);
		}
		else if ( return_value == MAXVAL )
		{
			result_rf.sign = 0;
			result_rf.exp = exp_max;
			result_rf.frac = (1ull << 63);
		}
		else
		{
			result_rf.sign = 1;
			result_rf.exp = exp_max;
			result_rf.frac = (1ull << 63);
		}

		return ( result_rf );
	}

	/* now test for tininess */

	tininess = FALSE;

	if ( rsexp == exp_min )
	{
		if ( (precision == SNGL) && ((rsfrac & BIT23) == 0) )
			tininess = TRUE;
		else if ( (precision == DBL) && ((rsfrac & BIT52) == 0) )
			tininess = TRUE;
		else if ( (rsfrac & HIGH_BIT) == 0 )
			tininess = TRUE;
	}

#ifdef FP_DEBUG
	fprintf(stderr, "tininess = %x\n\n", tininess);
#endif

	/* now check for flush-to-zero mode */

	if ( ftz )
	{
		/* flush-to-zero mode enabled */

		if ( tininess == TRUE )
		{
			result_rf.sign = rssign;
			result_rf.exp = 0;
			result_rf.frac = 0ull;

			sf->b.inexact_flag = 1;
			sf->b.underflow_flag = 1;

			return ( result_rf );
		}
	}

	if ( tininess == FALSE )
	{
		/* no underflow */

		if ( inexact == TRUE )
			sf->b.inexact_flag = 1;

		rsexp += EXP_BIAS;

		if ( precision == SNGL )
		{
			rsfrac <<= 40;
		}
		else if ( precision == DBL )
		{
			rsfrac <<= 11;
		}

		result_rf.sign = rssign;
		result_rf.exp = rsexp;
		result_rf.frac = rsfrac;

		return ( result_rf );
	}

	/* check for underflow */

	if ( sf->b.underflow_trap_disabled == 0 )
	{
		/* underflow trap is enabled */

		sf->b.underflow_flag = 1;

		/* recalculate rounded result to full precision */

		if ( precision == SNGL )
		{
			guard_bits2 >>= 40;
			guard_bits2 |= (rtfrac << 24);
			rtfrac >>= 40;
	
		}
		else if ( precision == DBL )
		{
			guard_bits2 >>= 11;
			guard_bits2 |= (rtfrac << 53);
			rtfrac >>= 11;
		}

		/* consolidate sticky bits into a single bit */

		if ( (guard_bits2 & STICKY_MASK) != 0 )
		{
			guard_bits2 &= ~STICKY_MASK;
			guard_bits2 |= STICKY_BIT;
		}

		rb = rtfrac & 1;		/* round bit */
		gb = (guard_bits2 >> 63);	/* guard bit */
		sb = (guard_bits2 >> 62) & 1;	/* sticky bit */
	
		carry = carry_bit[rm][rtsign][rb][gb][sb];
	
		/* recalculate rounded value to indicated precision */

		carry_out = 0;
	
		if ( precision == SNGL )
		{
			rtfrac += carry;
	
			if ( rtfrac == (1ull << 24) )
			{
				rtexp += 1;
				rtfrac >>= 1;
			}
		}
		else if ( precision == DBL )
		{
			rtfrac += carry;
	
			if ( rtfrac == (1ull << 53) )
			{
				rtexp += 1;
				rtfrac >>= 1;
			}
		}
		else
		{
			carry_out = 0;
	
			if ( (rtfrac == ULONGLONG_MAX) && (carry == 1) )
				carry_out = 1;
		
			rtfrac += carry;
		
			if ( carry_out == 1 )
			{
				rtexp += 1;
				rtfrac = (1ull << 63);
			}
		}

		rtexp += EXP_BIAS;
		biased_result_rf.sign = rtsign;
		biased_result_rf.exp = (rtexp & 0x1ffff);
		biased_result_rf.frac = (rtfrac << shift);

		/* set ISR_ebc bit if necessary */

		if ( rtexp != (rtexp & 0x1ffff) )
			sf->b.ISR_ebc = 1;

		/* set ISR_fpa bit if carry bit was 1 */
		// rickc change: fpa should not be raised here
		//               at least according to the Intel simulator
		if ( carry == 1 )
		  sf->b.ISR_fpa = 1;

		/* check for inexactness */

		// rickc hack
		if (( guard_bits || (rtexp != (rtexp & 0x1ffff))) && !inexact_fix)
			sf->b.inexact_flag = 1;

		return ( biased_result_rf );

	}
	else	/* underflow trap not enabled */
	{
		if ( inexact == TRUE )
		{
			sf->b.underflow_flag = 1;

			sf->b.inexact_flag = 1;
		}
	}

	if ( rsfrac == 0 )
	{
		rsexp = 0;
	}
	else
	{
		rsexp += EXP_BIAS;
	
		rsfrac <<= shift;
	
		if ( precision == DBL_EXT )
		{
			if ( rsexp == 0x0c001 )
				rsexp = 0;
		}
	}

	result_rf.sign = rssign;
	result_rf.exp = rsexp;
	result_rf.frac = rsfrac;

	return ( result_rf );
}


/*************************** fpswa.C ***************************/

/*
** |-----------------------------------------------------------|
** | Copyright (c) 2000 Silicon Graphics, Inc.                 |
** | All Rights Reserved                                       |
** |-----------------------------------------------------------|
** |          Restricted Rights Legend                         |
** | Use, duplication, or disclosure by the Government is      |
** | subject to restrictions as set forth in                   |
** | subparagraph (c)(1)(ii) of the Rights in Technical        |
** | Data and Computer Software Clause of DFARS 52.227-7013.   |
** |         Silicon Graphics, Inc.                            |
** |         2011 N. Shoreline Blvd.                           |
** |         Mountain View, CA 94039                           |
** |-----------------------------------------------------------|
*/

#ifndef _FPSWA_H_
#endif

#ifdef BARF
inline int
fp_is_natval(const float_rf *freg){

  if((freg->sign == 0)
       && (freg->exp == 0x1fffe)
       && (freg->frac == 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_inf(const float_rf *freg){

  if((freg->exp == 0x1ffff) && (freg->frac == 0x8000000000000000ull)) {
    return(1);
  } 
  else {
    return(0);
  }
}


inline int
fp_is_nan(const float_rf *freg){

  if((freg->exp == 0x1ffff)
	&& ((freg->frac & 0x8000000000000000ull) != 0)
	&& ((freg->frac & 0x7fffffffffffffffull) != 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_snan(const float_rf *freg){

  if((freg->exp == 0x1ffff)
     && ((freg->frac & 0xc000000000000000ull) == 0xc000000000000000ull)
     && ((freg->frac & 0x3fffffffffffffffull) != 0)){
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_qnan(const float_rf *freg){

  if((freg->exp == 0x1ffff)
     && ((freg->frac & 0xc000000000000000ull) == 0xc000000000000000ull)){
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_neg_inf(const float_rf *freg){

  if((freg->sign == 1)
       && (freg->exp == 0x1ffff)
       && (freg->frac == 0x8000000000000000ull)) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_neg_non_zero(const float_rf *freg){

  if((freg->sign == 1) && !fp_is_zero(freg)) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_normal(const float_rf *freg){

  if((freg->exp != 0)
       && (freg->exp != 0x1ffff)
       && ((freg->frac & 0x8000000000000000ull) != 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_pos_inf(const float_rf *freg){

  if((freg->sign == 0)
       && (freg->exp == 0x1ffff)
       && (freg->frac == 0x8000000000000000ull)) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_pos_non_zero(const float_rf *freg){

  if((freg->sign == 0) && !fp_is_zero(freg)) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_pseudo_zero(const float_rf *freg){

  if((freg->exp != 0)
       && (freg->exp != 0x1ffff)
       && (freg->frac == 0)
       && (!fp_is_natval(freg))) {
    return(1);
  } 
  else {
    return(0);
  }
}

inline int
fp_is_unorm(const float_rf *freg){

  if((((freg->exp != 0)
       && (freg->exp != 0x1ffff)
       && ((freg->frac & 0x8000000000000000ull) == 0))
       && (!fp_is_natval(freg)))
       /* double-extended pseudo-denormal or double-extended denormal */
       || ((freg->exp == 0) && (freg->frac != 0))) {
    return(1);
  } 
  else {
    return(0);
  }
}


inline int
fp_is_unorm_not_pseudo_denorm(const float_rf *freg){

  if(((freg->exp != 0)
	 && (freg->exp != 0x1ffff)
	 &&((freg->frac & 0x8000000000000000ull) == 0))
         /* double-extended pseudo-denormal or double-extended denormal */
         || ((freg->exp == 0) &&
	     (freg->frac & 0x8000000000000000ull)  == 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}


inline int
fp_is_unsupported(const float_rf *freg){
    
  if(fp_is_natval(freg) || fp_is_nan(freg)   || fp_is_inf(freg)
	|| fp_is_normal(freg) || fp_is_unorm(freg) || fp_is_zero(freg)) {
    return(0);
  } 
  else {
    return(1);
  }
}


inline int
fp_is_zero(const float_rf *freg){

  if((freg->exp == 0) && (freg->frac == 0)) {
    return(1);
  } 
  else {
    return(0);
  }
}

#endif

int
fp_software_assistance_required(fp_op_type opcode,const float_rf *fr2,
				const float_rf *fr3, const float_rf *fr4,
				int sf_dd){

  int do_swa_fault=0;

  /*****************************************************************************
   Filter those cases for each instruction that do NOT cause swa.
  *****************************************************************************/


  switch (opcode) {
    case op_fcmp:
      if((fp_is_unsupported(fr2) || fp_is_unsupported(fr3))   ||
	 (fp_is_nan(fr2) || fp_is_nan(fr3))){
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpcmp:
      if(fp_is_nan(fr2) || fp_is_nan(fr3)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fcvt_fx:
      if(fp_is_unsupported(fr2) || fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpcvt_fx:
      if(fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fcvt_fxu:
      if(fp_is_unsupported(fr2) || fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpcvt_fxu:
      if(fp_is_nan(fr2)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fma:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3) || fp_is_unsupported(fr4)) || 
	 (fp_is_nan(fr2)         || fp_is_nan(fr3)         || fp_is_nan(fr4))         ||
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_neg_inf(fr2))
	  
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_neg_inf(fr2)))     ||
	 
	 ((fp_is_inf(fr3) && fp_is_zero(fr4)) || (fp_is_zero(fr3) && fp_is_inf(fr4)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpma:
      if(
	 (fp_is_nan(fr2) || fp_is_nan(fr3) || fp_is_nan(fr4)) ||
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4)
	   && fp_is_neg_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_neg_inf(fr2))
	  
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_pos_inf(fr2))
	  
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_neg_inf(fr2)))             || 
	 
	 ((fp_is_inf(fr3) && fp_is_zero(fr4)) 
	  || (fp_is_zero(fr3) && fp_is_inf(fr4)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;

    case op_fminmax:
      if((fp_is_unsupported(fr2) || fp_is_unsupported(fr3))   ||
	 (fp_is_nan(fr2) || fp_is_nan(fr3))                ){
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpminmax:
      if(fp_is_nan(fr2) || fp_is_nan(fr3)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fms_fnma:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3) || fp_is_unsupported(fr4)) ||
	 (fp_is_nan(fr2) || fp_is_nan(fr3) || fp_is_nan(fr4))                         || 
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4) && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4) && fp_is_pos_inf(fr2)))    ||
	 
	 ((fp_is_inf(fr3) && fp_is_zero(fr4)) || (fp_is_zero(fr3) && fp_is_inf(fr4)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fpms_fpnma:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3) || fp_is_unsupported(fr4))   ||
	 (fp_is_nan(fr2)     || fp_is_nan(fr3)     || fp_is_nan(fr4))       ||
	 
	 ((fp_is_pos_inf(fr3) && fp_is_pos_non_zero(fr4)
	   && fp_is_pos_inf(fr2))
	  || (fp_is_pos_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_pos_non_zero(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_inf(fr3) && fp_is_neg_non_zero(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_pos_inf(fr2))
	  || (fp_is_pos_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_pos_inf(fr4)
	      && fp_is_neg_inf(fr2))
	  || (fp_is_neg_non_zero(fr3) && fp_is_neg_inf(fr4)
	      && fp_is_pos_inf(fr2)))                           ||
	 
	 ((fp_is_inf(fr3)  && fp_is_zero(fr4))
	  || (fp_is_zero(fr3) && fp_is_inf(fr4))  )) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3) || fp_is_unorm(fr4)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fnorm:
      if(fp_is_unsupported(fr3) || fp_is_nan(fr3)) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr3)) {
	if(((fr3->exp > 0) && (fr3->exp < 0x1ffff)) &&
	   (sf_dd == 1)) {
	  do_swa_fault = 0;
	}
	else {
	  do_swa_fault     = 1;
	}
      }
      break;
      
    case op_frcpa:
      if(
	 (fp_is_unsupported(fr2) || fp_is_unsupported(fr3))   ||
	 (fp_is_nan(fr2)         || fp_is_nan(fr3))           ||
	 
	 ((fp_is_inf(fr2)  && fp_is_inf(fr3))
	  || ((fp_is_zero(fr2) || fp_is_pseudo_zero(fr2))
	      && (fp_is_zero(fr3) || fp_is_pseudo_zero(fr3)))) || 
	 
	 (((fp_is_normal(fr2) && !fp_is_zero(fr2))
	   || (fp_is_unorm(fr2)  && !fp_is_pseudo_zero(fr2)))
	  && (fp_is_zero(fr3)   ||  fp_is_pseudo_zero(fr3)))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      break;
      
    case op_fprcpa:
      if((fp_is_nan(fr2) || fp_is_nan(fr3))      ||              
	 ((fp_is_inf(fr2)  && fp_is_inf(fr3))
	  || (fp_is_zero(fr2) && fp_is_zero(fr3))) || 
	 (!fp_is_zero(fr2) && fp_is_zero(fr3))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2) || fp_is_unorm(fr3)) {
	do_swa_fault     = 1;
      }
      
      break;
      
    case op_frsqrta:
      if(fp_is_unsupported(fr2)  || 
	 fp_is_nan(fr2)          || 
	 fp_is_neg_inf(fr2)      ||
	 (fp_is_neg_non_zero(fr2) && !fp_is_pseudo_zero(fr2) &&
	  !fp_is_unorm_not_pseudo_denorm(fr2))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      break;

    case op_fprsqrta:
      if(fp_is_nan(fr2) || fp_is_neg_inf(fr2) || 
	 (fp_is_neg_non_zero(fr2) && !fp_is_unorm(fr2))) {
	do_swa_fault     = 0;
      }
      else if(fp_is_unorm(fr2)) {
	do_swa_fault     = 1;
      }
      
      break;
  }
  
  return(do_swa_fault);

}



/*************************** fputil.C ***************************/

#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

static	int8_t	shift_count[16] =
{
0, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
};

	/* shift denormal fraction so that bit 63 is set, returning
	 * the shift count; assumes at least one bit among bits 63-0
	 * is non-zero
	 */

int32_t
renorm( uint64_t *p_val )
{
int32_t	count;
int32_t	index;

	if ( (*p_val) == 0ull )
	{
		/* fatal error, abort */

		fprintf(stderr, "renorm:  mantissa is zero, aborting\n");
		abort();

		return ( 0 );
	}

	if ( (*p_val) & (1ull << 63) )
		return ( 0 );

	count = 0;

	while ( ((*p_val) & 0xfff0000000000000ull) == 0ull )
	{
		*p_val <<= 12;
		count += 12;
	}

	while ( ((*p_val) & 0xf000000000000000ull) == 0ull )
	{
		*p_val <<= 4;
		count += 4;
	}

	index = ((*p_val >> 60) & 0xf);

	*p_val <<= shift_count[index];
	count += shift_count[index];

	return ( count );
}

void
breakout( const float_rf *rs, uint32_t *rssign, int32_t *rsexp, 
	  uint64_t *rsfrac, status_field *sf )
{
int32_t	shift_count;

	*rssign = rs->sign;
	*rsexp = rs->exp;
	*rsfrac = rs->frac;

	if ( ((*rsexp) == 0) && ((*rsfrac) == 0ull) )
		return;

	if ( ((*rsexp) == 0) && (((*rsfrac) & HIGH_BIT) == 0) )
	{
		/* double extended real denormal; switch
		 * representation
		 */

		(*rsexp) = 0x0c001;
	}

	/* take care of pseudo-zeroes */

	if ( (0x00001 <= (*rsexp)) && ((*rsexp) <= 0x1fffe) &&
	     (*rsfrac == 0)
	   )
	{
		(*rsexp) = 0; /* set rs = 0 */

		return;
	}

	/* take care of pseudo-infinities */

	if ( ((*rsexp) == 0x1ffff) && (*rsfrac == 0) )
	{
		(*rsfrac) = HIGH_BIT; /* set rs = infinity */
	}
	else if ( ((*rsexp) == 0x1ffff) && (((*rsfrac) & HIGH_BIT) == 0) )
	{
		/* take care of pseudo-NaNs */

		(*rsfrac) = (3ull << 62); /* set rs to a quiet NaN */
	}
	else if ( ((*rsfrac) & HIGH_BIT) == 0 ) 
	{
		/* unnormalized number; normalize it */

		shift_count = renorm( rsfrac );
		*rsexp -= shift_count;

		sf->b.den_unn_op_flag = 1; /* set den_unn_op flag */
	}

	return;
}
	
int32_t
breakout_and_test( const float_rf *rs, uint32_t *rssign, int32_t *rsexp, 
		   uint64_t *rsfrac, status_field *sf )
{
int32_t	result;

	breakout(rs, rssign, rsexp, rsfrac, sf);

	if ( (*rsexp == EXP_NATVAL) && (*rsfrac == (1ull << 61)) )
	{
		result = NATVAL;
		return ( result );
	}

	if ( (*rsexp == EXP_NAN) && (*rsfrac > (1ull << 63)) &&
	     (*rsfrac < (0x3ull << 62))
	   )
	{
		result = SIGNALING_NAN;
		return ( result );
	}

	if ( (*rsexp == EXP_NAN) && (*rsfrac >= (0x3ull << 62)) )
	{
		result = QUIET_NAN;
		return ( result );
	}

	return ( 0 );
}

int32_t
breakout_and_test2( const float_rf *rs, uint32_t *rssign, int32_t *rsexp, uint64_t *rsfrac,
		    const float_rf *rt, uint32_t *rtsign, int32_t *rtexp, uint64_t *rtfrac,
		    status_field *sf )
{
int32_t	result;

	breakout(rs, rssign, rsexp, rsfrac, sf);

	breakout(rt, rtsign, rtexp, rtfrac, sf);

	if ( (*rsexp == EXP_NATVAL) && (*rsfrac == (1ull << 61)) ||
	     (*rtexp == EXP_NATVAL) && (*rtfrac == (1ull << 61)) 
	   )
	{
		result = NATVAL;
		return ( result );
	}

	if ( (*rsexp == EXP_NAN) && (*rsfrac > (1ull << 63)) &&
	     (*rsfrac < (0x3ull << 62))
	   )
	{
		result = SIGNALING_NAN;
		return ( result );
	}

	if ( (*rtexp == EXP_NAN) && (*rtfrac > (1ull << 63)) &&
	     (*rtfrac < (0x3ull << 62))
	   )
	{
		result = SIGNALING_NAN;
		return ( result );
	}

	if ( (*rsexp == EXP_NAN) && (*rsfrac >= (0x3ull << 62)) )
	{
		result = QUIET_NAN;
		return ( result );
	}

	if ( (*rtexp == EXP_NAN) && (*rtfrac >= (0x3ull << 62)) )
	{
		result = QUIET_NAN;
		return ( result );
	}

	return ( 0 );
}

int32_t
breakout_and_test3( const float_rf *rs, uint32_t *rssign, int32_t *rsexp, uint64_t *rsfrac,
		    const float_rf *rt, uint32_t *rtsign, int32_t *rtexp, uint64_t *rtfrac,
		    const float_rf *rr, uint32_t *rrsign, int32_t *rrexp, uint64_t *rrfrac,
		    status_field *sf )
{
int32_t	result;

	breakout(rs, rssign, rsexp, rsfrac, sf);

	breakout(rt, rtsign, rtexp, rtfrac, sf);

	breakout(rr, rrsign, rrexp, rrfrac, sf);

	if ( (*rsexp == EXP_NATVAL) && (*rsfrac == (1ull << 61)) ||
	     (*rtexp == EXP_NATVAL) && (*rtfrac == (1ull << 61)) ||
	     (*rrexp == EXP_NATVAL) && (*rrfrac == (1ull << 61)) 
	   )
	{
		result = NATVAL;
		return ( result );
	}

	if ( (*rsexp == EXP_NAN) && (*rsfrac > (1ull << 63)) &&
	     (*rsfrac < (0x3ull << 62))
	   )
	{
		result = SIGNALING_NAN;
		return ( result );
	}

	if ( (*rtexp == EXP_NAN) && (*rtfrac > (1ull << 63)) &&
	     (*rtfrac < (0x3ull << 62))
	   )
	{
		result = SIGNALING_NAN;
		return ( result );
	}

	if ( (*rrexp == EXP_NAN) && (*rrfrac > (1ull << 63)) &&
	     (*rrfrac < (0x3ull << 62))
	   )
	{
		result = SIGNALING_NAN;
		return ( result );
	}

	if ( (*rsexp == EXP_NAN) && (*rsfrac >= (0x3ull << 62)) )
	{
		result = QUIET_NAN;
		return ( result );
	}

	if ( (*rtexp == EXP_NAN) && (*rtfrac >= (0x3ull << 62)) )
	{
		result = QUIET_NAN;
		return ( result );
	}

	if ( (*rrexp == EXP_NAN) && (*rrfrac >= (0x3ull << 62)) )
	{
		result = QUIET_NAN;
		return ( result );
	}

	return ( 0 );
}



/*************************** mul64c.c ***************************/

#ifdef _MUL64_DEBUG
#include <stdio.h>
#endif

#ifndef _LP64
typedef unsigned long long  uint64_t;
#endif /* _LP64 */

#define	LOW32BITS	0xffffffffull

/* computes the 128 bit product of two unsigned 64 bit integers,
   returning the high 64 bits, and storing the low 64 bits
   as *low_bits.
*/


uint64_t
mul64( uint64_t x, uint64_t y, uint64_t *low_bits )
{
int	i, j;
uint64_t	u[2], v[2], w[4];
uint64_t	t, k;
uint64_t	z[2];

#ifdef _MUL64_DEBUG

	printf("mul64: x = %016llx\n", x);
	printf("mul64: y = %016llx\n", y);

#endif 

	/* put x and y into arrays, 32 bits at a time */

	u[0] = (x & LOW32BITS);
	x >>= 32;

	u[1] = (x & LOW32BITS);

	v[0] = (y & LOW32BITS);
	y >>= 32;

	v[1] = (y & LOW32BITS);

#ifdef _MUL64_DEBUG

	printf("u[0] = %016llx\n", u[0]);
	printf("u[1] = %016llx\n", u[1]);
	printf("v[0] = %016llx\n", v[0]);
	printf("v[1] = %016llx\n\n", v[1]);

#endif

	/* The following algorithm is from Knuth, Third edition, 
	   vol. 2, p. 268, algorithm M.
	 */

	w[0] = w[1] = w[2] = w[3] = 0;

	for ( j=0; j<2; j++ )
	{
		if ( v[j] == 0 )
		{
			w[j+2] = 0;
			continue;
		}

		k = 0;

		for ( i=0; i<2; i++ )
		{
			t = u[i]*v[j] + w[i+j] + k;
			w[i+j] = (t & LOW32BITS);
			k = (t >> 32);
		}

		w[j+2] = k;
	}

#ifdef _MUL64_DEBUG

	printf("w[0] = %016llx\n", w[0]);
	printf("w[1] = %016llx\n", w[1]);
	printf("w[2] = %016llx\n", w[2]);
	printf("w[3] = %016llx\n\n", w[3]);

#endif

	/* combine the 4 pieces of the product into 2. */

	z[0] = (w[0] | (w[1] << 32));
	z[1] = (w[2] | (w[3] << 32));

#ifdef _MUL64_DEBUG

	printf("z[0] = %016llx\n", z[0]);
	printf("z[1] = %016llx\n", z[1]);

#endif
	*low_bits = z[0];
	return ( z[1] );

}


/******************************************************************/

