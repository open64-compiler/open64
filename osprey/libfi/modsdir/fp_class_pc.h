/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


/* USMID @(#) libfi/modsdir/fp_class_pc.h	92.1     07/28/99 11:07:31 */


#ifndef FP_CLASS_PC_H
#define FP_CLASS_PC_H

/* classify values to be returned by fp_class function */

#ifndef FOR_K_FP_SNAN
#define FOR_K_FP_SNAN 1
#endif

#ifndef FOR_K_FP_QNAN
#define FOR_K_FP_QNAN 2
#endif

#ifndef FOR_K_FP_POS_INF
#define FOR_K_FP_POS_INF 3
#endif

#ifndef FOR_K_FP_NEG_INF
#define FOR_K_FP_NEG_INF 4
#endif

#ifndef FOR_K_FP_POS_NORM
#define FOR_K_FP_POS_NORM 5
#endif

#ifndef FOR_K_FP_NEG_NORM
#define FOR_K_FP_NEG_NORM 6
#endif

#ifndef FOR_K_FP_POS_DENORM
#define FOR_K_FP_POS_DENORM 7
#endif

#ifndef FOR_K_FP_NEG_DENORM
#define FOR_K_FP_NEG_DENORM 8
#endif

#ifndef FOR_K_FP_POS_ZERO
#define FOR_K_FP_POS_ZERO 9
#endif

#ifndef FOR_K_FP_NEG_ZERO
#define FOR_K_FP_NEG_ZERO 10
#endif

#define IEEE_128_EXPO_BITS 15
#define IEEE_128_EXPO_MAX 0X7FFF
#define IEEE_128_MANT_BTS_UP1 16
#define IEEE_128_MANT_BTS_UP2 32
#define IEEE_128_MANT_BTS_LO1 32
#define IEEE_128_MANT_BTS_LO2 32

#define IEEE_64_EXPO_BITS 11
#define IEEE_64_EXPO_MAX 0X7FF
#define IEEE_64_MANT_BTS1 20
#define IEEE_64_MANT_BTS2 32

#define IEEE_32_EXPO_BITS 8
#define IEEE_32_EXPO_MAX 0X7F8
#define IEEE_32_MANT_BTS 23

#if defined(_LITTLE_ENDIAN) && !defined(__sv2)

union _uval_d {
  _f_real16 dwd;
  struct {
    unsigned int sign   : 1;
    unsigned int exp    : IEEE_128_EXPO_BITS;
    unsigned int q_bit  : 1;
    unsigned int up1    : IEEE_128_MANT_BTS_UP1-1;
    unsigned int up2    : IEEE_128_MANT_BTS_UP2;
    unsigned int lo1    : IEEE_128_MANT_BTS_LO1;
    unsigned int lo2    : IEEE_128_MANT_BTS_LO2;
  } parts;
};

#else
/* This is the Linux-Specific bit pattern for 128 bit real */

union _uval_d {
  _f_real16 dwd;
  struct {
    unsigned int sign   : 1;
    unsigned int exp    : 15;
    unsigned int empty  : 16;
    unsigned int one    : 1;
    unsigned int q_bit  : 1;
    unsigned int up1    : 30;
    unsigned int lo1    : 32;
  } parts;
};

#endif

union _uval_r {
  _f_real8 dwd;
  struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
    unsigned int lo     : IEEE_64_MANT_BTS2;
    unsigned int up     : IEEE_64_MANT_BTS1-1;
    unsigned int q_bit  : 1;
    unsigned int exp    : IEEE_64_EXPO_BITS;
    unsigned int sign   : 1;
#else
    unsigned int sign   : 1;
    unsigned int exp    : IEEE_64_EXPO_BITS;
    unsigned int q_bit  : 1;
    unsigned int up     : IEEE_64_MANT_BTS1-1;
    unsigned int lo     : IEEE_64_MANT_BTS2;
#endif
  } parts;
};

union _uval_h {
  _f_real4 dwd;
  struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
    unsigned int mant  : IEEE_32_MANT_BTS-1;
    unsigned int q_bit : 1;
    unsigned int exp   : IEEE_32_EXPO_BITS;
    unsigned int sign  : 1;
#else
    unsigned int sign  : 1;
    unsigned int exp   : IEEE_32_EXPO_BITS;
    unsigned int q_bit : 1;
    unsigned int mant  : IEEE_32_MANT_BTS-1;
#endif
  } parts;
};

#endif /* FP_CLASS_PC_H */
