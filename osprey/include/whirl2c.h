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


#ifndef __WHIRL2C_H__
#define __WHIRL2C_H__


#include <string.h> /* Declares memmove */
#ifdef _FORTRAN2C
#include <libftn.h> /* Declares math and io functions */
#else
#include <math.h> /* Declares math functions */
#endif /* _FORTRAN2C */

/*----------- Types used in the whirl2c output files ---------- */

typedef void __UNKNOWN_TYPE;
typedef char _BOOLEAN;
typedef signed char _INT8;
typedef signed short _INT16;
typedef signed int _INT32;
typedef signed long long _INT64;
typedef unsigned char _UINT8;
typedef unsigned short _UINT16;
typedef unsigned int _UINT32;
typedef unsigned long long _UINT64;
typedef float _IEEE32;
typedef double _IEEE64;
typedef long double _QUAD;

typedef char _STRING[];

#ifndef _FORTRAN2C

     /* Declare the complex types, since they are considered builtin
      * by whirl2c, even when the original source was a C program.
      */
typedef struct {float r, i;} _COMPLEX32;
typedef struct {double r, i;} _COMPLEX64;
typedef struct {long double r, i;} _COMPLEXQD;

#else /* defined(_FORTRAN2C) */

     /* Declare the complex types in terms of the libftn.h declarations
      * for the same.
      */
typedef struct _cpx_float _COMPLEX32;
typedef struct _cpx_double _COMPLEX64;
typedef struct _cpx_long_double _COMPLEXQD;

     /* Declare a few temporary variables to be used for complex
      * arithmetics.
      */
static _COMPLEXQD _Tmp_CQ, _Tmp1_CQ, _Tmp2_CQ;
static _COMPLEX64 _Tmp_C8, _Tmp1_C8, _Tmp2_C8;
static _COMPLEX32 _Tmp_C4, _Tmp1_C4, _Tmp2_C4;

static const _COMPLEXQD _One_CQ = {1.0L, 0.0L};
static const _COMPLEX64 _One_C8 = {1.0, 0.0};
static const _COMPLEX32 _One_C4 = {1.0F, 0.0F};

#endif /*_FORTRAN2C*/

/*----------- Operator definitions ---------- */

#define __MSTORE(cpy_from, from_offset, cpy_to, to_offset, number_of_bytes) \
   (void)memmove((char *)cpy_to + from_offset, \
		 (char *)cpy_from + to_offset, number_of_bytes)

/* Complex negation is the negation of the real and imaginary
 * parts respectively.  Use the temporary variable for this.
 */
#define _C4NEG(v) \
   (_Tmp_C4 = (v), \
    _Tmp_C4.realpart = -_Tmp_C4.realpart, \
    _Tmp_C4.imagpart = -_Tmp_C4.imagpart, \
    _Tmp_C4)
#define _C8NEG(v) \
   (_Tmp_C8 = (v), \
    _Tmp_C8.realpart = -_Tmp_C8.realpart, \
    _Tmp_C8.imagpart = -_Tmp_C8.imagpart, \
    _Tmp_C8)
#define _CQNEG(v) \
   (_Tmp_CQ = (v), \
    _Tmp_CQ.realpart = -_Tmp_CQ.realpart, \
    _Tmp_CQ.imagpart = -_Tmp_CQ.imagpart, \
    _Tmp_CQ)

#define _I4ABS(v) ((v) > 0? (v) : -(v))
#define _I8ABS(v) ((v) > 0LL? (v) : -(v))
#define _F4ABS(v) ((v) > 0.0F? (v) : -(v))
#define _F8ABS(v) ((v) > 0.0? (v) : -(v))
#define _FQABS(v) ((v) > 0.0L? (v) : -(v))
#define _C4ABS(v) (_Tmp1_C4 = (v), c_abs_(&_Tmp1_C4))
#define _C8ABS(v) (_Tmp1_C8 = (v), z_abs_(&_Tmp1_C8))
#define _CQABS(v) (_Tmp1_CQ = (v), __cq_abs(&_Tmp1_CQ))

#define _F4SQRT(v) fsqrt(v)
#define _F8SQRT(v) sqrtf(v)
#define _FQSQRT(v) qsqrt(v)  /* libm extension (need to compile -xansi) */
#define _C4SQRT(v) (_Tmp1_C4 = (v), c_sqrt(&_Tmp_C4, &_Tmp1_C4), _Tmp_C4)
#define _C8SQRT(v) (_Tmp1_C8 = (v), z_sqrt(&_Tmp_C8, &_Tmp1_C8), _Tmp_C8)
#define _CQSQRT(v) (_Tmp1_CQ = (v), __cq_sqrt(&_Tmp_CQ, &_Tmp1_CQ), _Tmp_CQ)

/* In converting floating point numbers into integral numbers we
 * employ the same algorithm the compilers use when constant folding
 * these operators.  The operations are described in terms of more
 * generic _T1T2<op> macros where this is simpler.
 */
#define _T1F4RND(v, t1) ((v) >= 0.0F? (t1)((v)+0.5F) : (t1)((v)-0.5F))
#define _T1F8RND(v, t1) ((v) >= 0.0? (t1)((v)+0.5) : (t1)((v)-0.5))
#define _T1FQRND(v, t1) ((v) >= 0.0L? (t1)((v)+0.5L) : (t1)((v)-0.5L))
#define _I4F4RND(v) _T1F4RND(v, _INT32)
#define _I4F8RND(v) _T1F8RND(v, _INT32)
#define _I4FQRND(v) _T1FQRND(v, _INT32)
#define _U4F4RND(v) _T1F4RND(v, _UINT32)
#define _U4F8RND(v) _T1F8RND(v, _UINT32)
#define _U4FQRND(v) _T1FQRND(v, _UINT32)
#define _I8F4RND(v) _T1F4RND(v, _INT64)
#define _I8F8RND(v) _T1F8RND(v, _INT64)
#define _I8FQRND(v) _T1FQRND(v, _INT64)
#define _U8F4RND(v) _T1F4RND(v, _UINT64)
#define _U8F8RND(v) _T1F8RND(v, _UINT64)
#define _U8FQRND(v) _T1FQRND(v, _UINT64)

#define _I4F4TRUNC(v) (_INT32)(v)
#define _I4F8TRUNC(v) (_INT32)(v)
#define _I4FQTRUNC(v) (_INT32)(v)
#define _U4F4TRUNC(v) (_UINT32)(v)
#define _U4F8TRUNC(v) (_UINT32)(v)
#define _U4FQTRUNC(v) (_UINT32)(v)
#define _I8F4TRUNC(v) (_INT64)(v)
#define _I8F8TRUNC(v) (_INT64)(v)
#define _I8FQTRUNC(v) (_INT64)(v)
#define _U8F4TRUNC(v) (_UINT64)(v)
#define _U8F8TRUNC(v) (_UINT64)(v)
#define _U8FQTRUNC(v) (_UINT64)(v)

#define _T1T2CEIL(v, t1, t2) ((t2)(t1)(v) < (v)? (t1)(v)+1 : (t1)(v))
#define _I4F4CEIL(v) _T1T2CEIL(v, _INT32, _IEEE32)
#define _I4F8CEIL(v) _T1T2CEIL(v, _INT32, _IEEE64)
#define _I4FQCEIL(v) _T1T2CEIL(v, _INT32, _QUAD)
#define _U4F4CEIL(v) _T1T2CEIL(v, _UINT32, _IEEE32)
#define _U4F8CEIL(v) _T1T2CEIL(v, _UINT32, _IEEE64)
#define _U4FQCEIL(v) _T1T2CEIL(v, _UINT32, _QUAD)
#define _I8F4CEIL(v) _T1T2CEIL(v, _INT64, _IEEE32)
#define _I8F8CEIL(v) _T1T2CEIL(v, _INT64, _IEEE64)
#define _I8FQCEIL(v) _T1T2CEIL(v, _INT64, _QUAD)
#define _U8F4CEIL(v) _T1T2CEIL(v, _UINT64, _IEEE32)
#define _U8F8CEIL(v) _T1T2CEIL(v, _UINT64, _IEEE64)
#define _U8FQCEIL(v) _T1T2CEIL(v, _UINT64, _QUAD)

#define _T1T2FLOOR(v, t1, t2) ((t2)(t1)(v) > (v)? (t1)(v)-1 : (t1)(v))
#define _I4F4FLOOR(v) _T1T2FLOOR(v, _INT32, _IEEE32)
#define _I4F8FLOOR(v) _T1T2FLOOR(v, _INT32, _IEEE64)
#define _I4FQFLOOR(v) _T1T2FLOOR(v, _INT32, _QUAD)
#define _U4F4FLOOR(v) _T1T2FLOOR(v, _UINT32, _IEEE32)
#define _U4F8FLOOR(v) _T1T2FLOOR(v, _UINT32, _IEEE64)
#define _U4FQFLOOR(v) _T1T2FLOOR(v, _UINT32, _QUAD)
#define _I8F4FLOOR(v) _T1T2FLOOR(v, _INT64, _IEEE32)
#define _I8F8FLOOR(v) _T1T2FLOOR(v, _INT64, _IEEE64)
#define _I8FQFLOOR(v) _T1T2FLOOR(v, _INT64, _QUAD)
#define _U8F4FLOOR(v) _T1T2FLOOR(v, _UINT64, _IEEE32)
#define _U8F8FLOOR(v) _T1T2FLOOR(v, _UINT64, _IEEE64)
#define _U8FQFLOOR(v) _T1T2FLOOR(v, _UINT64, _QUAD)

#define _C4ADD(v1, v2) \
   (_Tmp_C4 = (v1), \
    _Tmp_C4.realpart += (v2).realpart, \
    _Tmp_C4.imagpart += (v2).imagpart, \
    _Tmp_C4)
#define _C8ADD(v1, v2) \
   (_Tmp_C8 = (v1), \
    _Tmp_C8.realpart += (v2).realpart, \
    _Tmp_C8.imagpart += (v2).imagpart, \
    _Tmp_C8)
#define _CQADD(v1, v2) \
   (_Tmp_CQ = (v1), \
    _Tmp_CQ.realpart += (v2).realpart, \
    _Tmp_CQ.imagpart += (v2).imagpart, \
    _Tmp_CQ)

#define _C4SUB(v1, v2) \
   (_Tmp_C4 = (v1), \
    _Tmp_C4.realpart -= (v2).realpart, \
    _Tmp_C4.imagpart -= (v2).imagpart, \
    _Tmp_C4)
#define _C8SUB(v1, v2) \
   (_Tmp_C8 = (v1), \
    _Tmp_C8.realpart -= (v2).realpart, \
    _Tmp_C8.imagpart -= (v2).imagpart, \
    _Tmp_C8)
#define _CQSUB(v1, v2) \
   (_Tmp_CQ = (v1), \
    _Tmp_CQ.realpart -= (v2).realpart, \
    _Tmp_CQ.imagpart -= (v2).imagpart, \
    _Tmp_CQ)

#define _C4MPY(v1, v2) \
   (_Tmp_C4.realpart = (v1).realpart*(v2).realpart - (v1).imagpart*(v2).imagpart, \
    _Tmp_C4.imagpart = (v1).realpart*(v2).imagpart + (v1).imagpart*(v2).realpart, \
    _Tmp_C4)
#define _C8MPY(v1, v2) \
   (_Tmp_C8.realpart = (v1).realpart*(v2).realpart - (v1).imagpart*(v2).imagpart, \
    _Tmp_C8.imagpart = (v1).realpart*(v2).imagpart + (v1).imagpart*(v2).realpart, \
    _Tmp_C8)
#define _CQMPY(v1, v2) \
   (_Tmp_CQ.realpart = (v1).realpart*(v2).realpart - (v1).imagpart*(v2).imagpart, \
    _Tmp_CQ.imagpart = (v1).realpart*(v2).imagpart + (v1).imagpart*(v2).realpart, \
    _Tmp_CQ)

#define _C4DIV(v1, v2) \
   (_Tmp1_C4 = (v1), _Tmp2_C4 = (v2), \
    c_div(&_Tmp_C4, &_Tmp1_C4, &_Tmp2_C4), \
    _Tmp_C4)
#define _C8DIV(v1, v2) \
   (_Tmp1_C8 = (v1), _Tmp2_C8 = (v2), \
    z_div(&_Tmp_C8, &_Tmp1_C8, &_Tmp2_C8), \
    _Tmp_C8)
#define _CQDIV(v1, v2) \
   (_Tmp1_CQ = (v1), _Tmp2_CQ = (v2), \
    __cq_div(&_Tmp_CQ, &_Tmp1_CQ, &_Tmp2_CQ), \
    _Tmp_CQ)

/* This should only occur for Fortran programs.  The result is 
 * undefined for v2==0.  Note that when either operand (but not
 * both) is negative, the remainder (%) will be negative while
 * the modulus should be positive ((v1%v2) + v2).  For all
 * other cases, the modulus operation is equivalent to the 
 * remainder operation.
 */
#define _I4MOD(v1, v2) \
   ((((v1)%(v2) != 0) && (v1)>0 ^ (v2)>0)? (((v1)%(v2)) + (v2)) : ((v1)%(v2)))
#define _I8MOD(v1, v2) \
   ((((v1)%(v2) != 0LL) && (v1)>0LL ^ (v2)>0LL)? (((v1)%(v2)) + (v2)) : ((v1)%(v2)))

/*
 * INTRN_DIVFLOOR(x,y)
 * INTRN_DIVCEIL(x,y)
 * 	x,y are integers
 * 
 * Definition
 * x y             INTRN_DIVFLOOR          INTRN_DIVCEIL
 * ---             --------------          -------------
 * + +                 x / y                (x+y-1) / y
 * 
 * - -                 x / y                (x+y+1) / y
 * 
 * + -              (x+ -1-y)/y                x / y
 * 
 * - +              (x+  1-y)/y                x / y
 *
 * Evaulate (divfloor) without branch code, using:
 *
 *    f(y) => ((y<0)? -1 : +1) => ((y>>31)<<1) + 1
 *
 *    MASK(x,y,v) => (x>=0 && y>=0) || (x<0 && y<0)? 0 : v => ((x^y)>>31) & v
 *
 * The cleverness (Shapiro's) was the composition of these functions
 * to evaluate divfloor:
 *
 *    DIVFLOOR(x,y) = (x + MASK(x, y, f(y) - y)) / y
 *
 * where:
 *
 *    (f(y) - y) => (-1-y) [+-],   (+1-y) [-+]
 */
#define _I4DIVFLOOR_SIGN(y) ((((y)>>31)<<1) + 1) /* ((y<0)? -1 : +1) */
#define _I4DIVFLOOR_MASK(x, y, v) ((((x)^(y))>>31) & (v))
#define _I8DIVFLOOR_SIGN(y) ((((y)>>63)<<1) + 1LL) /* ((y<0)? -1 : +1) */
#define _I8DIVFLOOR_MASK(x, y, v) ((((x)^(y))>>63) & (v))

#define _I4DIVFLOOR(v1, v2) \
   ((v1) + _I4DIVFLOOR_MASK(v1, v2, _I4DIVFLOOR_SIGN(v2) - (v2))) / (v2)
#define _I8DIVFLOOR(v1, v2) \
   ((v1) + _I8DIVFLOOR_MASK(v1, v2, _I8DIVFLOOR_SIGN(v2) - (v2))) / (v2)
#define _U4DIVFLOOR(v1, v2) ((v1)/(v2))
#define _U8DIVFLOOR(v1, v2) ((v1)/(v2))

#define _I4DIVCEIL(v1, v2) -_I4DIVFLOOR(-(v1), (v2))
#define _I8DIVCEIL(v1, v2) -_I8DIVFLOOR(-(v1), (v2))
#define _U4DIVCEIL(v1, v2) ((v1)+(v2)-1)/(v2)
#define _U8DIVCEIL(v1, v2) ((v1)+(v2)-1)/(v2)

#define _I4MODFLOOR(v1, v2) ((v1) - (v2)*_I4DIVFLOOR((v1), (v2)))
#define _I8MODFLOOR(v1, v2) ((v1) - (v2)*_I8DIVFLOOR((v1), (v2)))
#define _U4MODFLOOR(v1, v2) ((v1) - (v2)*((v1)/(v2)))
#define _U8MODFLOOR(v1, v2) ((v1) - (v2)*((v1)/(v2)))

#define _I4MODCEIL(v1, v2) ((v1) - (v2)*_I4DIVCEIL((v1), (v2)))
#define _I8MODCEIL(v1, v2) ((v1) - (v2)*_I8DIVCEIL((v1), (v2)))
#define _U4MODCEIL(v1, v2) ((v1) - (v2)*_U4DIVCEIL((v1), (v2)))
#define _U8MODCEIL(v1, v2) ((v1) - (v2)*_U8DIVCEIL((v1), (v2)))

#define _I4MAX(v1, v2) ((v1) > (v2)? (v1) : (v2))
#define _I8MAX(v1, v2) ((v1) > (v2)? (v1) : (v2))
#define _U4MAX(v1, v2) ((v1) > (v2)? (v1) : (v2))
#define _U8MAX(v1, v2) ((v1) > (v2)? (v1) : (v2))
#define _F4MAX(v1, v2) ((v1) > (v2)? (v1) : (v2))
#define _F8MAX(v1, v2) ((v1) > (v2)? (v1) : (v2))
#define _FQMAX(v1, v2) ((v1) > (v2)? (v1) : (v2))

#define _I4MIN(v1, v2) ((v1) < (v2)? (v1) : (v2))
#define _I8MIN(v1, v2) ((v1) < (v2)? (v1) : (v2))
#define _U4MIN(v1, v2) ((v1) < (v2)? (v1) : (v2))
#define _U8MIN(v1, v2) ((v1) < (v2)? (v1) : (v2))
#define _F4MIN(v1, v2) ((v1) < (v2)? (v1) : (v2))
#define _F8MIN(v1, v2) ((v1) < (v2)? (v1) : (v2))
#define _FQMIN(v1, v2) ((v1) < (v2)? (v1) : (v2))

#define _I4SHL(v1, v2) ((v1) << ((v2) & 31))
#define _I8SHL(v1, v2) ((v1) << ((v2) & 63LL))
#define _U4SHL(v1, v2) ((v1) << ((v2) & 31))
#define _U8SHL(v1, v2) ((v1) << ((v2) & 63LL))

/* For right shifts we make the non-strict ANSI assumption
 * that the '>>' operator sign-extends signed numbers, and
 * zero extends unsigned numbers.
 */
#define _I4ASHR(v1, v2) ((v1) >> ((v2) & 31))
#define _I8ASHR(v1, v2) ((v1) >> ((v2) & 63LL))
#define _U4ASHR(v1, v2) (_UINT32)((_INT32)(v1) >> ((v2) & 31))
#define _U8ASHR(v1, v2) (_UINT64)((_INT64)(v1) >> ((v2) & 63LL))

#define _I4LSHR(v1, v2) (_INT32)((_UINT32)(v1) >> ((v2) & 31))
#define _I8LSHR(v1, v2) (_INT64)((_UINT64)(v1) >> ((v2) & 63LL))
#define _U4LSHR(v1, v2) ((v1) >> ((v2) & 31))
#define _U8LSHR(v1, v2) ((v1) >> ((v2) & 63LL))

#define _F4RECIP(v) (1.0F/v)
#define _F8RECIP(v) (1.0/v)
#define _FQRECIP(v) (1.0L/v)
#define _C4RECIP(v) _C4DIV(_One_C4, v)
#define _C8RECIP(v) _C8DIV(_One_C8, v)
#define _CQRECIP(v) _CQDIV(_One_CQ, v)

#define _F4RSQRT(v) (1.0F/_F4SQRT(v))
#define _F8RSQRT(v) (1.0/_F8SQRT(v))
#define _FQRSQRT(v) (1.0L/_FQSQRT(v))
#define _C4RSQRT(v) (_Tmp_C4 = _C4SQRT(v), _C4DIV(_One_C4, _Tmp_C4))
#define _C8RSQRT(v) (_Tmp_C8 = _C8SQRT(v), _C8DIV(_One_C8, _Tmp_C8))
#define _CQRSQRT(v) (_Tmp_CQ = _CQSQRT(v), _CQDIV(_One_CQ, _Tmp_CQ))

/* Macros for operators like "I4CVTL n", "U8CVTL n" where n is not 8|16|32|64.
 * *NOT* all combinations are defined here. It would otherwise makes 
 * the header file unwieldy.
 */

/* NOTE: <rty_bits> and <cvtl_len> should not have side-effect. */
#define INT_CVTL_N(val, rty, cvtl_len) \
    (((val) << (sizeof(rty)*8-(cvtl_len))) >> (sizeof(rty)*8-(cvtl_len)))

#define UINT_CVTL_N(val, rty, cvtl_len) \
    ((val) & (((rty)1)<<cvtl_len)-1)

#define INT32_CVTL_2(x) (INT_CVTL_N((x), _INT32, 2))  /*for "I4CVTL 2" */ 
#define UINT32_CVTL_3(x) (UINT_CVTL_N((x), _UINT64, 3)) /*for "U8CVTL 3"*/

/* Vector type */
#if defined(__x86_64) || defined(__i386)

#ifdef __GNUC__
    #define VECT16_ATTR __attribute__ ((vector_size (16)))
#else 
    #error need GCC compatible front-end
#endif 

typedef _INT8   V16I1 VECT16_ATTR ;
typedef _INT16  V16I2 VECT16_ATTR ;
typedef _INT32  V16I4 VECT16_ATTR ;
typedef _INT64  V16I8 VECT16_ATTR ;
typedef _IEEE32 V16F4 VECT16_ATTR ;
typedef _IEEE64 V16F8 VECT16_ATTR ;

#endif

#endif /* __WHIRL2C_H__ */
