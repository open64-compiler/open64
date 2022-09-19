/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/**
***		Intrinsics, Intrinsic Types, and Intrinsic Flags
***		------------------------------------------------
***
*** Description:
***
***	This interface describes all the intrinsic names, operators,
***	types associated with intrinsics, and properties associated with
***	intrinsics.
***
*** Reserved Prefixes:
***
***	INTRN		for INTRINSIC members only.
***
*** Exported types:
***
***	INTRINSIC
***
***	    An enumerated type.  The members are a partial set of all language
***	    defined intrinsics.  Those language intrinsics not included in this
***	    enumerated type are already present in WHIRL as OPC nodes.  There
***	    are usually two separate flavors of each fortran intrinsic - one
***	    named INTRN_XXX and one named INTRN_XXXe.  The former name
***	    represents the version called directly by generated code and usually
***	    has call by value semantics.  These intrinsics might eventually be
***	    turned into calls, be inlined or have direct code generated for
***	    them.  The INTRN_XXXe version is always an external routine with
***	    call by reference semantics.  It is needed to support passing an
***	    intrinsic function itself to another subprogram.
***
***	    All INTRINSICs are prefixed with INTRN.
***
*** Exported data:
***
***	    none
***
**/

#ifndef wintrinsic_INCLUDED
#define wintrinsic_INCLUDED "wintrinsic.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {

  INTRINSIC_INVALID = -1,
  INTRINSIC_NONE = 0,
  INTRINSIC_FIRST = 1,

    /* F77 type conversions - INT, SHORT, LONG, REAL, FLOAT, SNGL, DBLE, CMPLX,
			      CHAR, ZEXT, etc. */

	/* All of these are already represented by existing WHIRL nodes.  F77
	   semantics preclude them from being passed, so external versions
	   aren't needed. */

    /* F77 '**' expressions */

	/* These functions are needed to support ** in expressions.  They are
	   not directly callable, so external versions aren't needed.  F77
	   semantic rules treat f**i specially.  Here are the details:

	   a**b   \  b
	         a \  I1,I2,I4  I8    F4   F8   FQ   C4   C8   CQ
	            +--------------------------------------------
	   I1,I2,I4 |    I4     I8    F4   F8   FQ   C4   C8   CQ
		 I8 |    I8     I8    F4   F8   FQ   C4   C8   CQ
		 F4 |   F4I4   F4I8   F4   F8   FQ   C4   C8   CQ
		 F8 |   F8I4   F8I8   F8   F8   FQ   C8   C8   CQ
		 FQ |   FQI4   FQI8   FQ   FQ   FQ   CQ   CQ   CQ
		 C4 |   C4I4   C4I8   C4   C8   CQ   C4   C8   CQ
		 C8 |   C8I4   C8I8   C8   C8   CQ   C8   C8   CQ
		 CQ |   CQI4   CQI8   CQ   CQ   CQ   CQ   CQ   CQ   */

  INTRN_I4EXPEXPR = 1,
  INTRN_I8EXPEXPR = 2,
  INTRN_F4EXPEXPR = 3,
  INTRN_F8EXPEXPR = 4,
  INTRN_FQEXPEXPR = 5,
  INTRN_C4EXPEXPR = 6,
  INTRN_C8EXPEXPR = 7,
  INTRN_CQEXPEXPR = 8,
  INTRN_F4I4EXPEXPR = 9,
  INTRN_F4I8EXPEXPR = 10,
  INTRN_F8I4EXPEXPR = 11,
  INTRN_F8I8EXPEXPR = 12,
  INTRN_FQI4EXPEXPR = 13,
  INTRN_FQI8EXPEXPR = 14,
  INTRN_C4I4EXPEXPR = 15,
  INTRN_C4I8EXPEXPR = 16,
  INTRN_C8I4EXPEXPR = 17,
  INTRN_C8I8EXPEXPR = 18,
  INTRN_CQI4EXPEXPR = 19,
  INTRN_CQI8EXPEXPR = 20,

    /* F77 character relational expressions */

	/* These functions are needed to support character relational logical
	   expressions.  They are not directly callable, so external versions
	   aren't needed. */

  INTRN_CEQEXPR = 21,
  INTRN_CNEEXPR = 22,
  INTRN_CGEEXPR = 23,
  INTRN_CGTEXPR = 24,
  INTRN_CLEEXPR = 25,
  INTRN_CLTEXPR = 26,

    /* F77 substring 'var(e1:e2)' expressions */

	/* These functions are needed to support character substrings in
	   expressions.  They are not directly callable, so external versions
	   aren't needed.  */

  INTRN_SUBSTRINGEXPR = 27,

    /* F77 concat '//' expressions */

	/* These functions are needed to support // in expressions.  They are
	   not directly callable, so external versions aren't needed.  */

  INTRN_CONCATEXPR = 28,

    /* F77 character assignment statements */

	/* These functions are needed to support character assignment
	   statements.  The normal OPC_MLOAD and OPC_MSTORE don't handle the
	   blank padding F77 requires.  They are not directly callable, so
	   external versions aren't needed.  */

  INTRN_CASSIGNSTMT = 29,


    /* F77 max/min intrinsics - MAX, AMAX, MIN, AMIN, etc. */

	/* All of these differ from the existing WHIRL nodes in that they
	   take an arbitrary number of arguments.  However; they can all be
	   transformed into a cascaded set of WHIRL OPC_MAX/MIN nodes with (if
	   needed) an OPC_CVT node on top.  F77 semantics preclude them from
	   being passed, so external versions aren't needed. */

    /* F77 abs intrinsics - ABS, etc. */

	/* All of these are already represented by existing WHIRL nodes.
	   However; F77 semantics allow them to be passed, so external versions
	   are needed. */

  INTRN_I2ABSe = 30,
  INTRN_I4ABSe = 31,
  INTRN_I8ABSe = 32,
  INTRN_F4ABSe = 33,
  INTRN_F8ABSe = 34,
  INTRN_FQABSe = 35,
  INTRN_F4C4ABS = 36,
  INTRN_F4C4ABSe = 37,
  INTRN_F8C8ABS = 38,
  INTRN_F8C8ABSe = 39,
  INTRN_FQCQABS = 40,
  INTRN_FQCQABSe = 41,

    /* F77 mod intrinsics - MOD, etc. */

	/* The integer cases are already represented by existing WHIRL nodes.
	   F77 semantics allow them to be passed, so external versions are
	   needed. */

  INTRN_I2MODe = 42,
  INTRN_I4MODe = 43,
  INTRN_I8MODe = 44,
  INTRN_F4MOD = 45,
  INTRN_F4MODe = 46,
  INTRN_F8MOD = 47,
  INTRN_F8MODe = 48,
  INTRN_FQMOD = 49,
  INTRN_FQMODe = 50,

    /* F77 sqrt intrinsics - SQRT, etc. */

	/* All of these are already represented by existing WHIRL nodes
	   However; F77 semantics allow them to be passed, so external versions
	   are needed. */

  INTRN_F4SQRTe = 51,
  INTRN_F8SQRTe = 52,
  INTRN_FQSQRTe = 53,
  INTRN_C4SQRTe = 54,
  INTRN_C8SQRTe = 55,
  INTRN_CQSQRTe = 56,

    /* F77 misc. math intrinsics - CONJG, DIM, PROD, SIGN, etc. */

	/* None of these have corresponding, single WHIRL nodes.  F77 semantics
	   allow them to be passed, so external versions are needed. */

  INTRN_C4CONJG = 57,
  INTRN_C4CONJGe = 58,
  INTRN_C8CONJG = 59,
  INTRN_C8CONJGe = 60,
  INTRN_CQCONJG = 61,
  INTRN_CQCONJGe = 62,

  INTRN_I1DIM = 63,
  INTRN_I2DIM = 64,
  INTRN_I2DIMe = 65,
  INTRN_I4DIM = 66,
  INTRN_I4DIMe = 67,
  INTRN_I8DIM = 68,
  INTRN_I8DIMe = 69,
  INTRN_F4DIM = 70,
  INTRN_F4DIMe = 71,
  INTRN_F8DIM = 72,
  INTRN_F8DIMe = 73,
  INTRN_FQDIM = 74,
  INTRN_FQDIMe = 75,

  INTRN_F8F4PROD = 76,
  INTRN_F8F4PRODe = 77,
  INTRN_FQF8PROD = 78,
  INTRN_FQF8PRODe = 79,

  INTRN_I1SIGN = 80,
  INTRN_I2SIGN = 81,
  INTRN_I2SIGNe = 82,
  INTRN_I4SIGN = 83,
  INTRN_I4SIGNe = 84,
  INTRN_I8SIGN = 85,
  INTRN_I8SIGNe = 86,
  INTRN_F4SIGN = 87,
  INTRN_F4SIGNe = 88,
  INTRN_F8SIGN = 89,
  INTRN_F8SIGNe = 90,
  INTRN_FQSIGN = 91,
  INTRN_FQSIGNe = 92,

    /* F77 misc. math intrinsics - AIMAG, AINT, ANINT, IDINT, NINT, etc. */

	/* Some of these are already represented by existing WHIRL nodes.
	   F77 semantics allow these to be passed, so external versions are
	   needed.
	   Note that some of these (e.g., IDINT) are explicitly disallowed
	   as actual arguments by the F77 standard, but are allowed
	   historically on our platform (as well as on others, e.g., DEC VAX).
	   */

  INTRN_F4IMAGe = 93,
  INTRN_F8IMAGe = 94,
  INTRN_FQIMAGe = 95,

  INTRN_F4AINT = 96,
  INTRN_F4AINTe = 97,
  INTRN_F8AINT = 98,
  INTRN_F8AINTe = 99,
  INTRN_FQAINT = 100,
  INTRN_FQAINTe = 101,

  INTRN_I2F4INTe = 102,
  INTRN_I4F4INTe = 103,
  INTRN_I8F4INTe = 104,

  INTRN_I2F8IDINTe = 105,
  INTRN_I4F8IDINTe = 106,
  INTRN_I8F8IDINTe = 107,

  INTRN_I2FQIQINTe = 108,
  INTRN_I4FQIQINTe = 109,
  INTRN_I8FQIQINTe = 110,

  INTRN_I2F4NINT = 111,
  INTRN_I2F4NINTe = 112,
  INTRN_I4F4NINT = 113,
  INTRN_I4F4NINTe = 114,
  INTRN_I8F4NINT = 115,
  INTRN_I8F4NINTe = 116,

  INTRN_I2F8IDNINT = 117,
  INTRN_I2F8IDNINTe = 118,
  INTRN_I4F8IDNINT = 119,
  INTRN_I4F8IDNINTe = 120,
  INTRN_I8F8IDNINT = 121,
  INTRN_I8F8IDNINTe = 122,

  INTRN_I2FQIQNINT = 123,
  INTRN_I2FQIQNINTe = 124,
  INTRN_I4FQIQNINT = 125,
  INTRN_I4FQIQNINTe = 126,
  INTRN_I8FQIQNINT = 127,
  INTRN_I8FQIQNINTe = 128,

  INTRN_F4ANINT = 129,
  INTRN_F4ANINTe = 130,
  INTRN_F8ANINT = 131,
  INTRN_F8ANINTe = 132,
  INTRN_FQANINT = 133,
  INTRN_FQANINTe = 134,

    /* F77 bit intrisics - BNOT, BAND, BIOR, BXOR, BITS, BSET, BCLR, BTEST,
			   MVBITS, etc. */

	/* Some of these are already represented by existing WHIRL nodes.
	   F77 semantics allow them to be passed, so external versions are
	   needed. */

  INTRN_I2BNOTe = 135,
  INTRN_I4BNOTe = 136,
  INTRN_I8BNOTe = 137,

  INTRN_I2BANDe = 138,
  INTRN_I4BANDe = 139,
  INTRN_I8BANDe = 140,

  INTRN_I2BIORe = 141,
  INTRN_I4BIORe = 142,
  INTRN_I8BIORe = 143,

  INTRN_I2BXORe = 144,
  INTRN_I4BXORe = 145,
  INTRN_I8BXORe = 146,

  INTRN_I1BITS = 147,
  INTRN_I2BITS = 148,
  INTRN_I2BITSe = 149,
  INTRN_I4BITS = 150,
  INTRN_I4BITSe = 151,
  INTRN_I8BITS = 152,
  INTRN_I8BITSe = 153,

  INTRN_I1BSET = 154,
  INTRN_I2BSET = 155,
  INTRN_I2BSETe = 156,
  INTRN_I4BSET = 157,
  INTRN_I4BSETe = 158,
  INTRN_I8BSET = 159,
  INTRN_I8BSETe = 160,

  INTRN_I1BCLR = 161,
  INTRN_I2BCLR = 162,
  INTRN_I2BCLRe = 163,
  INTRN_I4BCLR = 164,
  INTRN_I4BCLRe = 165,
  INTRN_I8BCLR = 166,
  INTRN_I8BCLRe = 167,

  INTRN_I1BTEST = 168,
  INTRN_I2BTEST = 169,
  INTRN_I2BTESTe = 170,
  INTRN_I4BTEST = 171,
  INTRN_I4BTESTe = 172,
  INTRN_I8BTEST = 173,
  INTRN_I8BTESTe = 174,

  INTRN_I1MVBITS = 175,
  INTRN_I2MVBITS = 176,
  INTRN_I4MVBITS = 177,
  INTRN_I8MVBITS = 178,

    /* Fortran shift intrinsics - LSHIFT, RSHIFT, SHIFT, SHIFTC, etc. */

	/* Some of these are already represented by existing WHIRL nodes
	   F77 semantics allow them to be passed, so external versions are
	   needed. */

  INTRN_I1SHL = 179,
  INTRN_I2SHL = 180,

  INTRN_I1SHR = 181,
  INTRN_I2SHR = 182,

  INTRN_I1SHFT = 183,
  INTRN_I2SHFT = 184,
  INTRN_I2SHFTe = 185,
  INTRN_I4SHFT = 186,
  INTRN_I4SHFTe = 187,
  INTRN_I8SHFT = 188,
  INTRN_I8SHFTe = 189,

  INTRN_I1SHFTC = 190,
  INTRN_I2SHFTC = 191,
  INTRN_I2SHFTCe = 192,
  INTRN_I4SHFTC = 193,
  INTRN_I4SHFTCe = 194,
  INTRN_I8SHFTC = 195,
  INTRN_I8SHFTCe = 196,

    /* F77 character intrinsics - LEN, INDEX, LGE, LGT, LLE, LLT, etc. */

	/* None of these have corresponding, single WHIRL nodes.  F77 semantics
	   allow them to be passed, so external versions are needed. */

  INTRN_I4CLEN = 197,
  INTRN_I4CLENe = 198,

  INTRN_I4CINDEX = 199,
  INTRN_I4CINDEXe = 200,

  INTRN_CLGE = 201,
  INTRN_CLGEe = 202,

  INTRN_CLGT = 203,
  INTRN_CLGTe = 204,

  INTRN_CLLE = 205,
  INTRN_CLLEe = 206,

  INTRN_CLLT = 207,
  INTRN_CLLTe = 208,

    /* F77 transcendental intrinsics - EXP, LOG, LOG10, etc. */

	/* None of these have corresponding, single WHIRL nodes.  F77 semantics
	   allow them to be passed, so external versions are needed. */

  INTRN_F4EXP = 209,
  INTRN_F4EXPe = 210,
  INTRN_F8EXP = 211,
  INTRN_F8EXPe = 212,
  INTRN_FQEXP = 213,
  INTRN_FQEXPe = 214,
  INTRN_C4EXP = 215,
  INTRN_C4EXPe = 216,
  INTRN_C8EXP = 217,
  INTRN_C8EXPe = 218,
  INTRN_CQEXP = 219,
  INTRN_CQEXPe = 220,

  INTRN_F4LOG = 221,
  INTRN_F4LOGe = 222,
  INTRN_F8LOG = 223,
  INTRN_F8LOGe = 224,
  INTRN_FQLOG = 225,
  INTRN_FQLOGe = 226,
  INTRN_C4LOG = 227,
  INTRN_C4LOGe = 228,
  INTRN_C8LOG = 229,
  INTRN_C8LOGe = 230,
  INTRN_CQLOG = 231,
  INTRN_CQLOGe = 232,

  INTRN_F4LOG10 = 233,
  INTRN_F4LOG10e = 234,
  INTRN_F8LOG10 = 235,
  INTRN_F8LOG10e = 236,
  INTRN_FQLOG10 = 237,
  INTRN_FQLOG10e = 238,

    /* F77 trigonometic intrinsics - COS, SIN, CIS, TAN, COSD, SIND, TAND,
				     COSH, SINH, TANH, ACOS, ASIN, ATAN, ACOSD,
				     ASIND, ATAND, ATAN2, ATAN2D, etc. */

	/* None of these have corresponding, single WHIRL nodes.  F77 semantics
	   allow them to be passed, so external versions are needed. */

  INTRN_F4COS = 239,
  INTRN_F4COSe = 240,
  INTRN_F8COS = 241,
  INTRN_F8COSe = 242,
  INTRN_FQCOS = 243,
  INTRN_FQCOSe = 244,
  INTRN_C4COS = 245,
  INTRN_C4COSe = 246,
  INTRN_C8COS = 247,
  INTRN_C8COSe = 248,
  INTRN_CQCOS = 249,
  INTRN_CQCOSe = 250,

  INTRN_F4SIN = 251,
  INTRN_F4SINe = 252,
  INTRN_F8SIN = 253,
  INTRN_F8SINe = 254,
  INTRN_FQSIN = 255,
  INTRN_FQSINe = 256,
  INTRN_C4SIN = 257,
  INTRN_C4SINe = 258,
  INTRN_C8SIN = 259,
  INTRN_C8SINe = 260,
  INTRN_CQSIN = 261,
  INTRN_CQSINe = 262,

  INTRN_F4CIS = 263,
  INTRN_F4CISe = 264,
  INTRN_F8CIS = 265,
  INTRN_F8CISe = 266,
  INTRN_FQCIS = 267,
  INTRN_FQCISe = 268,

  INTRN_F4TAN = 269,
  INTRN_F4TANe = 270,
  INTRN_F8TAN = 271,
  INTRN_F8TANe = 272,
  INTRN_FQTAN = 273,
  INTRN_FQTANe = 274,

  INTRN_F4COSD = 275,
  INTRN_F4COSDe = 276,
  INTRN_F8COSD = 277,
  INTRN_F8COSDe = 278,
  INTRN_FQCOSD = 279,
  INTRN_FQCOSDe = 280,

  INTRN_F4SIND = 281,
  INTRN_F4SINDe = 282,
  INTRN_F8SIND = 283,
  INTRN_F8SINDe = 284,
  INTRN_FQSIND = 285,
  INTRN_FQSINDe = 286,

  INTRN_F4TAND = 287,
  INTRN_F4TANDe = 288,
  INTRN_F8TAND = 289,
  INTRN_F8TANDe = 290,
  INTRN_FQTAND = 291,
  INTRN_FQTANDe = 292,

  INTRN_F4COSH = 293,
  INTRN_F4COSHe = 294,
  INTRN_F8COSH = 295,
  INTRN_F8COSHe = 296,
  INTRN_FQCOSH = 297,
  INTRN_FQCOSHe = 298,

  INTRN_F4SINH = 299,
  INTRN_F4SINHe = 300,
  INTRN_F8SINH = 301,
  INTRN_F8SINHe = 302,
  INTRN_FQSINH = 303,
  INTRN_FQSINHe = 304,

  INTRN_F4TANH = 305,
  INTRN_F4TANHe = 306,
  INTRN_F8TANH = 307,
  INTRN_F8TANHe = 308,
  INTRN_FQTANH = 309,
  INTRN_FQTANHe = 310,

  INTRN_F4ACOS = 311,
  INTRN_F4ACOSe = 312,
  INTRN_F8ACOS = 313,
  INTRN_F8ACOSe = 314,
  INTRN_FQACOS = 315,
  INTRN_FQACOSe = 316,

  INTRN_F4ASIN = 317,
  INTRN_F4ASINe = 318,
  INTRN_F8ASIN = 319,
  INTRN_F8ASINe = 320,
  INTRN_FQASIN = 321,
  INTRN_FQASINe = 322,

  INTRN_F4ATAN = 323,
  INTRN_F4ATANe = 324,
  INTRN_F8ATAN = 325,
  INTRN_F8ATANe = 326,
  INTRN_FQATAN = 327,
  INTRN_FQATANe = 328,

  INTRN_F4ACOSD = 329,
  INTRN_F4ACOSDe = 330,
  INTRN_F8ACOSD = 331,
  INTRN_F8ACOSDe = 332,
  INTRN_FQACOSD = 333,
  INTRN_FQACOSDe = 334,

  INTRN_F4ASIND = 335,
  INTRN_F4ASINDe = 336,
  INTRN_F8ASIND = 337,
  INTRN_F8ASINDe = 338,
  INTRN_FQASIND = 339,
  INTRN_FQASINDe = 340,

  INTRN_F4ATAND = 341,
  INTRN_F4ATANDe = 342,
  INTRN_F8ATAND = 343,
  INTRN_F8ATANDe = 344,
  INTRN_FQATAND = 345,
  INTRN_FQATANDe = 346,

  INTRN_F4ATAN2 = 347,
  INTRN_F4ATAN2e = 348,
  INTRN_F8ATAN2 = 349,
  INTRN_F8ATAN2e = 350,
  INTRN_FQATAN2 = 351,
  INTRN_FQATAN2e = 352,

  INTRN_F4ATAN2D = 353,
  INTRN_F4ATAN2De = 354,
  INTRN_F8ATAN2D = 355,
  INTRN_F8ATAN2De = 356,
  INTRN_FQATAN2D = 357,
  INTRN_FQATAN2De = 358,

    /* F77 sizeof intrinsic - SIZEOF, etc. */

	/* All of these will have been converted to either constant nodes or
	   the LEN intrinsic. */

    /* F77 addressing intrinsics - %VAL, %REF, %LOC, etc. */

	/* All of these will have been converted to corresponding WHIRL
	   nodes. */

    /* C misc. intrinsics - currently, just alloca(). */

  INTRN_U4I4ALLOCA = 359,
  INTRN_U8I8ALLOCA = 360,

    /* F77 misc. intrinsics - MALLOC, FREE, DATE, ERRSNS, EXIT, TIME,
			      SECNDS, etc. */

	/* None of these have corresponding, single WHIRL nodes.  F77 semantics
	   do not allow them to be passed, so external versions aren't
	   needed. */

  INTRN_U4I4MALLOC = 361,
  INTRN_U8I8MALLOC = 362,

  INTRN_U4FREE = 363,
  INTRN_U8FREE = 364,

  INTRN_MDATE = 365,
  INTRN_I1DATE = 366,
  INTRN_I2DATE = 367,
  INTRN_I4DATE = 368,
  INTRN_I8DATE = 369,

  INTRN_I1ERRSNS = 370,
  INTRN_I2ERRSNS = 371,
  INTRN_I4ERRSNS = 372,
  INTRN_I8ERRSNS = 373,

  INTRN_VEXIT = 374,
  INTRN_I1EXIT = 375,
  INTRN_I2EXIT = 376,
  INTRN_I4EXIT = 377,
  INTRN_I8EXIT = 378,

  INTRN_TIME = 379,

  INTRN_F4SECNDS = 380,
  INTRN_F8SECNDS = 381,

    /* Fortran pause/stop statement - PAUSE, STOP, etc. */

	/* These are made intrinsics so that the optimizer can make use of
	   their special properties. */

  INTRN_PAUSE = 382,
  INTRN_STOP = 383,

    /* Fortran 90 intrinsics */

  INTRN_F4I4RAN = 384,
  INTRN_F4I8RAN = 385,
  INTRN_F8I4RAN = 386,
  INTRN_F8I8RAN = 387,
  INTRN_FQI4RAN = 388,
  INTRN_FQI8RAN = 389,

    /* C intrinsics */

    /* C++ intrinsics */

    /* LNO generated intrinsics */

	/* These additional intrinsics are potentially generated by LNO. */

  INTRN_I4DIVFLOOR = 390,
  INTRN_I8DIVFLOOR = 391,
  INTRN_U4DIVFLOOR = 392,
  INTRN_U8DIVFLOOR = 393,

  INTRN_I4DIVCEIL = 394,
  INTRN_I8DIVCEIL = 395,
  INTRN_U4DIVCEIL = 396,
  INTRN_U8DIVCEIL = 397,

  INTRN_I4MODFLOOR = 398,
  INTRN_I8MODFLOOR = 399,
  INTRN_U4MODFLOOR = 400,
  INTRN_U8MODFLOOR = 401,

  INTRN_I4MODCEIL = 402,
  INTRN_I8MODCEIL = 403,
  INTRN_U4MODCEIL = 404,
  INTRN_U8MODCEIL = 405,

    /* Internal to alloca */
  INTRN_U4I4SETSTACKPOINTER = 406,
  INTRN_U8I8SETSTACKPOINTER = 407,
  INTRN_U4READSTACKPOINTER = 408,
  INTRN_U8READSTACKPOINTER = 409,

    /* OS kernel intrinsics */

  INTRN_ADD_AND_FETCH_I4 = 410,
  INTRN_SUB_AND_FETCH_I4 = 411,
  INTRN_OR_AND_FETCH_I4 = 412,
  INTRN_XOR_AND_FETCH_I4 = 413,
  INTRN_AND_AND_FETCH_I4 = 414,
  INTRN_NAND_AND_FETCH_I4 = 415,

  INTRN_FETCH_AND_ADD_I4 = 416,
  INTRN_FETCH_AND_SUB_I4 = 417,
  INTRN_FETCH_AND_OR_I4 = 418,
  INTRN_FETCH_AND_XOR_I4 = 419,
  INTRN_FETCH_AND_AND_I4 = 420,
  INTRN_FETCH_AND_NAND_I4 = 421,

  INTRN_ADD_AND_FETCH_I8 = 422,
  INTRN_SUB_AND_FETCH_I8 = 423,
  INTRN_OR_AND_FETCH_I8 = 424,
  INTRN_XOR_AND_FETCH_I8 = 425,
  INTRN_AND_AND_FETCH_I8 = 426,
  INTRN_NAND_AND_FETCH_I8 = 427,

  INTRN_FETCH_AND_ADD_I8 = 428,
  INTRN_FETCH_AND_SUB_I8 = 429,
  INTRN_FETCH_AND_OR_I8 = 430,
  INTRN_FETCH_AND_XOR_I8 = 431,
  INTRN_FETCH_AND_AND_I8 = 432,
  INTRN_FETCH_AND_NAND_I8 = 433,

  INTRN_LOCK_TEST_AND_SET_I4 = 434,
  INTRN_LOCK_TEST_AND_SET_I8 = 435,

  INTRN_LOCK_RELEASE_I4 = 436,
  INTRN_LOCK_RELEASE_I8 = 437,

  INTRN_COMPARE_AND_SWAP_I4 = 438,
  INTRN_COMPARE_AND_SWAP_I8 = 439,

  INTRN_SYNCHRONIZE = 440,

  INTRN_RETURN_ADDRESS = 441,

    /* F77 (internal only) I/O intrinsics */

  INTRN_U4I1ADRTMP = 442,
  INTRN_U4I2ADRTMP = 443,
  INTRN_U4I4ADRTMP = 444,
  INTRN_U4I8ADRTMP = 445,
  INTRN_U4F4ADRTMP = 446,
  INTRN_U4F8ADRTMP = 447,
  INTRN_U4FQADRTMP = 448,
  INTRN_U4C4ADRTMP = 449,
  INTRN_U4C8ADRTMP = 450,
  INTRN_U4CQADRTMP = 451,
  INTRN_U4VADRTMP = 452,
  INTRN_U8I1ADRTMP = 453,
  INTRN_U8I2ADRTMP = 454,
  INTRN_U8I4ADRTMP = 455,
  INTRN_U8I8ADRTMP = 456,
  INTRN_U8F4ADRTMP = 457,
  INTRN_U8F8ADRTMP = 458,
  INTRN_U8FQADRTMP = 459,
  INTRN_U8C4ADRTMP = 460,
  INTRN_U8C8ADRTMP = 461,
  INTRN_U8CQADRTMP = 462,
  INTRN_U8VADRTMP = 463,

  INTRN_I4VALTMP = 464,
  INTRN_I8VALTMP = 465,
  INTRN_U4VALTMP = 466,
  INTRN_U8VALTMP = 467,
  INTRN_F4VALTMP = 468,
  INTRN_F8VALTMP = 469,
  INTRN_FQVALTMP = 470,
  INTRN_C4VALTMP = 471,
  INTRN_C8VALTMP = 472,
  INTRN_CQVALTMP = 473,

    /* C intrinsics */

  INTRN_BCOPY = 474,
  INTRN_BCMP = 475,
  INTRN_BZERO = 476,

  INTRN_MEMCCPY = 477,
  INTRN_MEMCHR = 478,
  INTRN_MEMCMP = 479,
  INTRN_MEMCPY = 480,
  INTRN_MEMMOVE = 481,
  INTRN_MEMSET = 482,

  INTRN_STRCMP = 483,
  INTRN_STRNCMP = 484,
  INTRN_STRCPY = 485,
  INTRN_STRNCPY = 486,
  INTRN_STRLEN = 487,

  INTRN_PRINTF = 488,
  INTRN_FPRINTF = 489,
  INTRN_SPRINTF = 490,
  INTRN_PRINTW = 491,
  INTRN_SCANF = 492,
  INTRN_FSCANF = 493,
  INTRN_SSCANF = 494,
  INTRN_FPUTC = 495,
  INTRN_FPUTS = 496,
  INTRN_FGETC = 497,
  INTRN_FGETS = 498,

  INTRN_F4VACOS = 499,
  INTRN_F8VACOS = 500,
  INTRN_F4VASIN = 501,
  INTRN_F8VASIN = 502,
  INTRN_F4VATAN = 503,
  INTRN_F8VATAN = 504,
  INTRN_F4VCOS = 505,
  INTRN_F8VCOS = 506,
  INTRN_F4VEXP = 507,
  INTRN_F8VEXP = 508,
  INTRN_F4VLOG = 509,
  INTRN_F8VLOG = 510,
  INTRN_F4VSIN = 511,
  INTRN_F8VSIN = 512,
  INTRN_F4VSQRT = 513,
  INTRN_F8VSQRT = 514,
  INTRN_F4VTAN = 515,
  INTRN_F8VTAN = 516,

  INTRN_NARY_ADD = 517,
  INTRN_NARY_MPY = 518,

    /* F77 intrinsics needed for -trapuv - MALLOC */

  INTRN_U4I4TRAPUV_MALLOC = 519,
  INTRN_U8I8TRAPUV_MALLOC = 520,

    /* F77 intrinsics needed for -check_bounds */

  INTRN_F77_BOUNDS_ERR = 521,

    /* F77 intrinsics needed for DSM */

  INTRN_DSM_NUMTHREADS = 522,
  INTRN_DSM_CHUNKSIZE = 523,
  INTRN_DSM_THIS_CHUNKSIZE = 524,
  INTRN_DSM_REM_CHUNKSIZE = 525,
  INTRN_DSM_NUMCHUNKS = 526,
  INTRN_DSM_THIS_THREADNUM = 527,
  INTRN_DSM_DISTRIBUTION_BLOCK = 528,
  INTRN_DSM_DISTRIBUTION_STAR = 529,
  INTRN_DSM_ISRESHAPED = 530,
  INTRN_DSM_ISDISTRIBUTED = 531,
  INTRN_DSM_THIS_STARTINDEX = 532,
  INTRN_DSM_DISTRIBUTION_CYCLIC = 533,

    /* More OS kernel intrinsics -- added here so we wouldn't have to
       make an incompatible Whirl change */

  INTRN_MPY_AND_FETCH_I4 = 534,
  INTRN_MIN_AND_FETCH_I4 = 535,
  INTRN_MAX_AND_FETCH_I4 = 536,
  INTRN_FETCH_AND_MPY_I4 = 537,
  INTRN_FETCH_AND_MIN_I4 = 538,
  INTRN_FETCH_AND_MAX_I4 = 539,
  INTRN_MPY_AND_FETCH_I8 = 540,
  INTRN_MIN_AND_FETCH_I8 = 541,
  INTRN_MAX_AND_FETCH_I8 = 542,
  INTRN_FETCH_AND_MPY_I8 = 543,
  INTRN_FETCH_AND_MIN_I8 = 544,
  INTRN_FETCH_AND_MAX_I8 = 545,

  INTRN_ADD_AND_FETCH_F4 = 546,
  INTRN_SUB_AND_FETCH_F4 = 547,
  INTRN_OR_AND_FETCH_F4 = 548,
  INTRN_XOR_AND_FETCH_F4 = 549,
  INTRN_AND_AND_FETCH_F4 = 550,
  INTRN_NAND_AND_FETCH_F4 = 551,
  INTRN_MPY_AND_FETCH_F4 = 552,
  INTRN_MIN_AND_FETCH_F4 = 553,
  INTRN_MAX_AND_FETCH_F4 = 554,

  INTRN_FETCH_AND_ADD_F4 = 555,
  INTRN_FETCH_AND_SUB_F4 = 556,
  INTRN_FETCH_AND_OR_F4 = 557,
  INTRN_FETCH_AND_XOR_F4 = 558,
  INTRN_FETCH_AND_AND_F4 = 559,
  INTRN_FETCH_AND_NAND_F4 = 560,
  INTRN_FETCH_AND_MPY_F4 = 561,
  INTRN_FETCH_AND_MIN_F4 = 562,
  INTRN_FETCH_AND_MAX_F4 = 563,

  INTRN_ADD_AND_FETCH_F8 = 564,
  INTRN_SUB_AND_FETCH_F8 = 565,
  INTRN_OR_AND_FETCH_F8 = 566,
  INTRN_XOR_AND_FETCH_F8 = 567,
  INTRN_AND_AND_FETCH_F8 = 568,
  INTRN_NAND_AND_FETCH_F8 = 569,
  INTRN_MPY_AND_FETCH_F8 = 570,
  INTRN_MIN_AND_FETCH_F8 = 571,
  INTRN_MAX_AND_FETCH_F8 = 572,

  INTRN_FETCH_AND_ADD_F8 = 573,
  INTRN_FETCH_AND_SUB_F8 = 574,
  INTRN_FETCH_AND_OR_F8 = 575,
  INTRN_FETCH_AND_XOR_F8 = 576,
  INTRN_FETCH_AND_AND_F8 = 577,
  INTRN_FETCH_AND_NAND_F8 = 578,
  INTRN_FETCH_AND_MPY_F8 = 579,
  INTRN_FETCH_AND_MIN_F8 = 580,
  INTRN_FETCH_AND_MAX_F8 = 581,

  INTRN_LOCK_ACQUIRE_I4 = 582,
  INTRN_LOCK_ACQUIRE_I8 = 583,
  
    /* Start of F90 specific intrinsics */

    /* The next four are for allocations produced by the F90 lowerer, and
       are internal to the lowerer */
  INTRN_F90_STACKTEMPALLOC = 584,
  INTRN_F90_HEAPTEMPALLOC = 585,
  INTRN_F90_STACKTEMPFREE = 586,
  INTRN_F90_HEAPTEMPFREE = 587,

  INTRN_FIRST_F90_INTRINSIC = 588,
#include "wintrinsic90.h" /* INTRN_LAST_F90_INTRINSIC = 685 */

  INTRN_MP_IN_PARALLEL_REGION	= INTRN_LAST_F90_INTRINSIC + 1,
  INTRN_RT_ERR			= INTRN_LAST_F90_INTRINSIC + 2,
  INTRN_OMP_DO_WORKSHARING      = INTRN_LAST_F90_INTRINSIC + 3,
  INTRN_OMP_TEST_LOCK		= INTRN_LAST_F90_INTRINSIC + 4,
  INTRN_OMP_GET_NUM_THREADS	= INTRN_LAST_F90_INTRINSIC + 5,
  INTRN_OMP_GET_MAX_THREADS	= INTRN_LAST_F90_INTRINSIC + 6,
  INTRN_OMP_GET_THREAD_NUM	= INTRN_LAST_F90_INTRINSIC + 7,
  INTRN_OMP_GET_NUM_PROCS	= INTRN_LAST_F90_INTRINSIC + 8,
  INTRN_OMP_IN_PARALLEL		= INTRN_LAST_F90_INTRINSIC + 9,
  INTRN_OMP_GET_DYNAMIC		= INTRN_LAST_F90_INTRINSIC + 10,
  INTRN_OMP_GET_NESTED		= INTRN_LAST_F90_INTRINSIC + 11,

/* Hand-added F90 intrinsics */

    INTRN_I1IEEE_INT = INTRN_LAST_F90_INTRINSIC + 12,
    INTRN_I2IEEE_INT = INTRN_LAST_F90_INTRINSIC + 13,
    INTRN_I4IEEE_INT = INTRN_LAST_F90_INTRINSIC + 14,
    INTRN_I8IEEE_INT = INTRN_LAST_F90_INTRINSIC + 15,
    INTRN_F4IEEE_INT = INTRN_LAST_F90_INTRINSIC + 16,
    INTRN_F8IEEE_INT = INTRN_LAST_F90_INTRINSIC + 17,
    INTRN_FQIEEE_INT = INTRN_LAST_F90_INTRINSIC + 18,
    INTRN_F90BOUNDS_CHECK = INTRN_LAST_F90_INTRINSIC + 19,
 
/* Two more intrisics used only by the F90 lowerer/front-end */
    INTRN_F90_DYNAMICTEMPALLOC = INTRN_LAST_F90_INTRINSIC + 20,
    INTRN_F90_DYNAMICTEMPFREE = INTRN_LAST_F90_INTRINSIC + 21,

/* More checking */
    INTRN_F90CONFORM_CHECK = INTRN_LAST_F90_INTRINSIC + 22,
  

    /* End of intrinsics list */
/* More C intrinsics */

  INTRN_C_F4FLOOR = 708,
  INTRN_C_F8FLOOR = 709,
  INTRN_C_FQFLOOR = 710,
  INTRN_C_F4CEIL  = 711,
  INTRN_C_F8CEIL  = 712,
  INTRN_C_FQCEIL  = 713,
  INTRN_C_F4TRUNC = 714,
  INTRN_C_F8TRUNC = 715,
  INTRN_C_FQTRUNC = 716,

/* Super Computing ABI Intrinsics */

  INTRN_I4DSHIFTL = 717,
  INTRN_I8DSHIFTL = 718,
  INTRN_I4DSHIFTR = 719,
  INTRN_I8DSHIFTR = 720,
  INTRN_I4GBIT    = 721,
  INTRN_I8GBIT    = 722,
  INTRN_I4GBITS   = 723,
  INTRN_I8GBITS   = 724,
  INTRN_I4MASK    = 725,
  INTRN_I8MASK    = 726,
  INTRN_I4MASKL   = 727,
  INTRN_I8MASKL   = 728,
  INTRN_I4MASKR   = 729,
  INTRN_I8MASKR   = 730,
  INTRN_I4PBIT    = 731,
  INTRN_I8PBIT    = 732,
  INTRN_I4PBITS   = 733,
  INTRN_I8PBITS   = 734,
  INTRN_I4POPPAR  = 735,
  INTRN_I8POPPAR  = 736,
  INTRN_I4RTC     = 737,
  INTRN_I8RTC     = 738,

  INTRN_GETF_EXP  = 739,
  INTRN_SETF_EXP  = 740,
  INTRN_GETF_SIG  = 741,
  INTRN_SETF_SIG  = 742,

  INTRN_FMERGE_NS = 743,
  INTRN_FMERGE_S  = 744,
  INTRN_FMERGE_SE = 745,

  /* F90 stop */
  INTRN_STOP_F90  = 746,

  /* log10 */
  INTRN_F4VLOG10  = 747,
  INTRN_F8VLOG10  = 748,

  /* gcc divide intrinsics */
  INTRN_MODSI3    = 749,
  INTRN_UMODSI3   = 750,
  INTRN_DIVSI3    = 751,
  INTRN_UDIVSI3   = 752,
  INTRN_MODDI3    = 753,
  INTRN_UMODDI3   = 754,
  INTRN_DIVDI3    = 755,
  INTRN_UDIVDI3   = 756,
  INTRN_DIVSF3    = 757,
  INTRN_DIVDF3    = 758,

  /* gcc extensions */
  INTRN_I4FFS     = 759,

  /* linux intrinsics */
  INTRN_SINCOSF   = 760,
  INTRN_SINCOS    = 761,
  INTRN_SINCOSL   = 762,

#ifdef KEY
  /* gcc stuff */
  INTRN_U4READFRAMEPOINTER = 763,
  INTRN_U8READFRAMEPOINTER = 764,
  INTRN_APPLY_ARGS = 765,
  INTRN_APPLY      = 766,
  INTRN_RETURN     = 767,

  /* x86-64 vararg support */
  INTRN_VA_START   = 768,
  INTRN_SAVE_XMMS  = 769,

  /* builtins */
  INTRN_CONSTANT_P = 770,

  /* C99 builtins */
  INTRN_ISGREATER       = 771,
  INTRN_ISGREATEREQUAL  = 772,
  INTRN_ISLESS          = 773,
  INTRN_ISLESSEQUAL     = 774,
  INTRN_ISLESSGREATER   = 775,
  INTRN_ISORDERED       = 776,
  INTRN_ISUNORDERED     = 777,

  /* Saturation arithmetic */
  INTRN_SUBSU2          = 778,
  INTRN_SUBSV16I2       = 779,

  /* g++ 3.4 builtins */
  INTRN_POPCOUNT        = 780,
  INTRN_PARITY          = 781,
  INTRN_CLZ             = 782,
  INTRN_CTZ64		= 783,
  INTRN_CLZ32           = 784,
  INTRN_CTZ             = 785,

  /* in GNU libm */
  INTRN_F4CBRT          = 786,
  INTRN_F8CBRT          = 787,

#define LAST_COMMON_ID 787
#ifdef TARG_X8664
  /* Shorter vector math functions */
  INTRN_V16F4SIN        = LAST_COMMON_ID+1,
  INTRN_V16F4EXP        = LAST_COMMON_ID+2,
  INTRN_V16F4EXPEXPR    = LAST_COMMON_ID+3,
  INTRN_V16F4LOG        = LAST_COMMON_ID+4,
  INTRN_V16F4COS        = LAST_COMMON_ID+5,
  INTRN_V16F8SIN        = LAST_COMMON_ID+6,
  INTRN_V16F8EXP        = LAST_COMMON_ID+7,
  INTRN_V16F8LOG        = LAST_COMMON_ID+8,
  INTRN_V16F8COS        = LAST_COMMON_ID+9,
  INTRN_V16F8EXPEXPR    = LAST_COMMON_ID+10,
  INTRN_V16F8LOG10      = LAST_COMMON_ID+11,
  INTRN_V16F8SINCOS     = LAST_COMMON_ID+12,

  /* Intrinsic to represent a complex C8 multiply (multiply and addsub) */
  INTRN_V16C8MPY_ADDSUB = LAST_COMMON_ID+13,
  
  /* Intrinsic to represent complex C8 conjugate */
  INTRN_V16C8CONJG      = LAST_COMMON_ID+14,

  /* GNU x8664 builtins */
  INTRN_PADDSB	        = LAST_COMMON_ID+15,
  INTRN_PADDSW          = LAST_COMMON_ID+16,
  INTRN_PSUBSB          = LAST_COMMON_ID+17,
  INTRN_PSUBSW          = LAST_COMMON_ID+18,
  INTRN_PADDUSB         = LAST_COMMON_ID+19,
  INTRN_PADDUSW         = LAST_COMMON_ID+20,
  INTRN_PSUBUSB         = LAST_COMMON_ID+21,
  INTRN_PSUBUSW         = LAST_COMMON_ID+22,
  INTRN_PMULLW          = LAST_COMMON_ID+23,
  INTRN_PMULHW          = LAST_COMMON_ID+24,
  INTRN_PAND            = LAST_COMMON_ID+25,
  INTRN_PCMPEQB         = LAST_COMMON_ID+26,
  INTRN_PCMPEQW         = LAST_COMMON_ID+27,
  INTRN_PCMPEQD         = LAST_COMMON_ID+28,
  INTRN_PCMPGTB         = LAST_COMMON_ID+29,
  INTRN_PCMPGTW         = LAST_COMMON_ID+30,
  INTRN_PCMPGTD         = LAST_COMMON_ID+31,
  INTRN_PUNPCKHBW       = LAST_COMMON_ID+32,
  INTRN_PUNPCKHWD       = LAST_COMMON_ID+33,
  INTRN_PUNPCKHDQ       = LAST_COMMON_ID+34,
  INTRN_PUNPCKLBW       = LAST_COMMON_ID+35,
  INTRN_PUNPCKLWD       = LAST_COMMON_ID+36,
  INTRN_PUNPCKLDQ       = LAST_COMMON_ID+37,
  INTRN_PACKSSWB        = LAST_COMMON_ID+38,
  INTRN_PACKSSDW        = LAST_COMMON_ID+39,
  INTRN_PACKUSWB        = LAST_COMMON_ID+40,
  INTRN_PMULHUW         = LAST_COMMON_ID+41,
  INTRN_PAVGB           = LAST_COMMON_ID+42,
  INTRN_PAVGW           = LAST_COMMON_ID+43,
  INTRN_PSADBW          = LAST_COMMON_ID+44,
  INTRN_PMAXUB          = LAST_COMMON_ID+45,
  INTRN_PMAXSW          = LAST_COMMON_ID+46,
  INTRN_PMINUB          = LAST_COMMON_ID+47,
  INTRN_PMINSW          = LAST_COMMON_ID+48,
  INTRN_PEXTRW0         = LAST_COMMON_ID+49,
  INTRN_PEXTRW1         = LAST_COMMON_ID+50,
  INTRN_PEXTRW2         = LAST_COMMON_ID+51,
  INTRN_PEXTRW3         = LAST_COMMON_ID+52,
  INTRN_PINSRW0         = LAST_COMMON_ID+53,
  INTRN_PINSRW1         = LAST_COMMON_ID+54,
  INTRN_PINSRW2         = LAST_COMMON_ID+55,
  INTRN_PINSRW3         = LAST_COMMON_ID+56,
  INTRN_PMOVMSKB        = LAST_COMMON_ID+57,
  INTRN_MOVNTQ          = LAST_COMMON_ID+58,
  INTRN_SFENCE          = LAST_COMMON_ID+59,
  INTRN_COMIEQSS        = LAST_COMMON_ID+60,
  INTRN_ADDPS           = LAST_COMMON_ID+61,
  INTRN_SUBPS           = LAST_COMMON_ID+62,
  INTRN_MULPS           = LAST_COMMON_ID+63,
  INTRN_DIVPS           = LAST_COMMON_ID+64,
  INTRN_ADDSS           = LAST_COMMON_ID+65,
  INTRN_SUBSS           = LAST_COMMON_ID+66,
  INTRN_MULSS           = LAST_COMMON_ID+67,
  INTRN_DIVSS           = LAST_COMMON_ID+68,
  INTRN_CMPEQPS         = LAST_COMMON_ID+69,
  INTRN_CMPLTPS         = LAST_COMMON_ID+70,
  INTRN_CMPLEPS         = LAST_COMMON_ID+71,
  INTRN_CMPGTPS         = LAST_COMMON_ID+72,
  INTRN_CMPGEPS         = LAST_COMMON_ID+73,
  INTRN_CMPUNORDPS      = LAST_COMMON_ID+74,
  INTRN_CMPNEQPS        = LAST_COMMON_ID+75,
  INTRN_CMPNLTPS        = LAST_COMMON_ID+76,
  INTRN_CMPNLEPS        = LAST_COMMON_ID+77,
  INTRN_CMPNGTPS        = LAST_COMMON_ID+78,
  INTRN_CMPNGEPS        = LAST_COMMON_ID+79,
  INTRN_CMPORDPS        = LAST_COMMON_ID+80,
  INTRN_CMPEQSS         = LAST_COMMON_ID+81,
  INTRN_CMPLTSS         = LAST_COMMON_ID+82,
  INTRN_CMPLESS         = LAST_COMMON_ID+83,
  INTRN_CMPUNORDSS      = LAST_COMMON_ID+84,
  INTRN_CMPNEQSS        = LAST_COMMON_ID+85,
  INTRN_CMPNLTSS        = LAST_COMMON_ID+86,
  INTRN_CMPNLESS        = LAST_COMMON_ID+87,
  INTRN_CMPORDSS        = LAST_COMMON_ID+88,
  INTRN_MAXPS           = LAST_COMMON_ID+89,
  INTRN_MAXSS           = LAST_COMMON_ID+90,
  INTRN_MINPS           = LAST_COMMON_ID+91,
  INTRN_MINSS           = LAST_COMMON_ID+92,
  INTRN_ANDPS           = LAST_COMMON_ID+93,
  INTRN_ANDNPS          = LAST_COMMON_ID+94,
  INTRN_ORPS            = LAST_COMMON_ID+95,
  INTRN_XORPS           = LAST_COMMON_ID+96,
  INTRN_MOVSS           = LAST_COMMON_ID+97,
  INTRN_MOVHLPS         = LAST_COMMON_ID+98,
  INTRN_MOVLHPS         = LAST_COMMON_ID+99,
  INTRN_UNPCKHPS        = LAST_COMMON_ID+100,
  INTRN_UNPCKLPS        = LAST_COMMON_ID+101,
  INTRN_RCPPS           = LAST_COMMON_ID+102,
  INTRN_RSQRTPS         = LAST_COMMON_ID+103,
  INTRN_SQRTPS          = LAST_COMMON_ID+104,
  INTRN_RCPSS           = LAST_COMMON_ID+105,
  INTRN_RSQRTSS         = LAST_COMMON_ID+106,
  INTRN_SQRTSS          = LAST_COMMON_ID+107,
  INTRN_SHUFPS          = LAST_COMMON_ID+108,
  INTRN_EMMS            = LAST_COMMON_ID+109,
  INTRN_PADDQ           = LAST_COMMON_ID+110,
  INTRN_PSUBQ           = LAST_COMMON_ID+111,
  INTRN_UNIMP_PURE      = LAST_COMMON_ID+112,
  INTRN_UNIMP           = LAST_COMMON_ID+113,
  INTRN_LOADAPS         = LAST_COMMON_ID+114,
  INTRN_STOREAPS        = LAST_COMMON_ID+115,
  INTRN_COSL            = LAST_COMMON_ID+116,
  INTRN_SINL            = LAST_COMMON_ID+117,
  INTRN_TAN             = LAST_COMMON_ID+118,
  INTRN_PSLLDQ          = LAST_COMMON_ID+119,
  INTRN_PSLLW           = LAST_COMMON_ID+120,
  INTRN_PSLLD           = LAST_COMMON_ID+121,
  INTRN_PSLLQ           = LAST_COMMON_ID+122,
  INTRN_PSRLW           = LAST_COMMON_ID+123,
  INTRN_PSRLD           = LAST_COMMON_ID+124,
  INTRN_PSRLQ           = LAST_COMMON_ID+125,
  INTRN_PSRAW           = LAST_COMMON_ID+126,
  INTRN_PSRAD           = LAST_COMMON_ID+127,
  INTRN_V16F4SINH       = LAST_COMMON_ID+128,
  INTRN_V16F4COSH       = LAST_COMMON_ID+129,
  INTRN_V16F8SINH       = LAST_COMMON_ID+130,
  INTRN_V16F8COSH       = LAST_COMMON_ID+131,
  INTRN_MOVNTDQ         = LAST_COMMON_ID+132,
  INTRN_LOADD           = LAST_COMMON_ID+133,
  INTRN_MOVNTPS         = LAST_COMMON_ID+134,
  INTRN_SSE_ZERO        = LAST_COMMON_ID+135,
  INTRN_CLRTI           = LAST_COMMON_ID+136,
  INTRN_PSHUFD          = LAST_COMMON_ID+137,
  INTRN_LOADSS          = LAST_COMMON_ID+138,
  INTRN_SIGNV16F4       = LAST_COMMON_ID+139,
  INTRN_SIGNV16F8       = LAST_COMMON_ID+140,
  INTRN_SHUFPD          = LAST_COMMON_ID+141,
  INTRN_XORPD           = LAST_COMMON_ID+142,
  INTRN_ANDPD           = LAST_COMMON_ID+143,
  INTRN_ORPD            = LAST_COMMON_ID+144,
  INTRN_STORELPD        = LAST_COMMON_ID+145,
  INTRN_STOREHPD        = LAST_COMMON_ID+146,
  INTRN_LOADLPD         = LAST_COMMON_ID+147,
  INTRN_LOADHPD         = LAST_COMMON_ID+148,
  INTRN_UNPCKLPD        = LAST_COMMON_ID+149,
  INTRN_UNPCKHPD        = LAST_COMMON_ID+150,
  INTRN_LFENCE          = LAST_COMMON_ID+151,
  INTRN_MFENCE          = LAST_COMMON_ID+152,
  INTRN_PSHUFW          = LAST_COMMON_ID+153,
  INTRN_PSRLDQ          = LAST_COMMON_ID+154,
  INTRN_LOADDQA         = LAST_COMMON_ID+155,
  INTRN_LOADDQU         = LAST_COMMON_ID+156,
  INTRN_STOREDQA        = LAST_COMMON_ID+157,
  INTRN_STOREDQU        = LAST_COMMON_ID+158,
  INTRN_F8F8I4EXPEXPR   = LAST_COMMON_ID+159,
  INTRN_F4F4I4EXPEXPR   = LAST_COMMON_ID+160,
  INTRN_FQFQI4EXPEXPR   = LAST_COMMON_ID+161,
  INTRN_EXPECT          = LAST_COMMON_ID+162,
    
  #define INTRINSIC_LAST_TMP  LAST_COMMON_ID+162
#else // Anything target-independent needs to be added on all branches of
      // this ifdef.
  INTRN_TAN           = LAST_COMMON_ID+1,
  INTRN_F8F8I4EXPEXPR = LAST_COMMON_ID+2,
  INTRN_F4F4I4EXPEXPR = LAST_COMMON_ID+3,
  INTRN_FQFQI4EXPEXPR = LAST_COMMON_ID+4,
  INTRN_EXPECT        = LAST_COMMON_ID+5,
  #define  INTRINSIC_LAST_TMP  LAST_COMMON_ID+5
#endif // TARG_X8664
#else
  #define INTRINSIC_LAST_TMP  762
#endif // KEY

  INTRN_CTYPE_B_LOC       = INTRINSIC_LAST_TMP+1,
  INTRN_CTYPE_TOUPPER_LOC = INTRINSIC_LAST_TMP+2,
  INTRN_CTYPE_TOLOWER_LOC = INTRINSIC_LAST_TMP+3,
  INTRINSIC_LAST          = INTRN_CTYPE_TOLOWER_LOC,
} INTRINSIC;

#ifdef __cplusplus
}
#endif

#endif
