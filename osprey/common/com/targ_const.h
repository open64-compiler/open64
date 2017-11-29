/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef targ_const_INCLUDED
#define targ_const_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 * Module: targ_const.h
 * $Source: common/com/SCCS/s.targ_const.h $
 *
 * Revision history:
 *  12-Jun-91 - Integrated from Josie
 *  24-Jun-91 - Moved to tp/com/targ_const.h from Josie target.h
 *
 * Description:
 *
 * The purpose of this file is to isolate target constant handling
 * in one file.  The philosophy is that all constants intended to be
 * referenced on the target be handled here, from creation (from
 * strings in source code or by operation on other target constants)
 * to final creation of assembly strings or object code words for
 * those which actually end up in the generated code.
 *
 * Where possible, we implement the philosophy by keeping constant
 * values in the host format (i.e. when doing so will not compromise
 * the range or accuracy w.r.t. the target), as this approach maximizes
 * the efficiency of constant manipulation on the host.  However, this
 * will not always be possible, e.g. where the host formats are not
 * rich enough to represent a value.  We can expect this situation,
 * for example, when we attempt to cross-compile for a 64-bit target
 * from a 32-bit target.
 *
 * All target constants are represented by struct TCON.  The struct 
 * records the type of the constant as well as its value.  The fields 
 * of struct TCON must not be accessed anywhere other than in
 * targ_const.c.  All constant operations are done in targ_const.c.
 * Any difference between representation of numbers in host and target
 * is handled here, including floating point representation and
 * byte-order differences.
 *
 * See also common/com/const.[hc] for operations on constants which
 * are not target representation dependent.  Because the constants
 * record their types within themselves, many routines of const.c have
 * been integrated here.
 *
 * NOTE:  The semantics of all operations are to be interpreted as
 * though the given operations were being performed on the target
 * machine on the target representation of the constants.  The only
 * exception to this rule is that this package is allowed to cope with
 * precision and range in excess of the target's; only when a value is
 * converted to the target representation need it be restricted to the
 * target's format.
 *
 * ====================================================================
 * ====================================================================
 */


#include "config_host.h"
/* Include the target machine type IDs: */
#include "mtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Incomplete types to keep ANSI happy: */
struct ty;

/* Define a quad-precision floating point type appropriate to host: */
#ifdef HOST_SUPPORTS_QUAD_FLOAT
#ifdef TARG_LOONGSON
struct QUAD_TYPE
{
    INT32 qval[4];
    /* As loongson's long double type is of 128 bit, hence longer than x86 and x86_64's 
     * long double type which are only 80-bit long, we have to overload operators for 
     * 128-bit operations.
     */
    QUAD_TYPE operator=(const double);
    QUAD_TYPE operator+=(QUAD_TYPE);	
    QUAD_TYPE operator-=(QUAD_TYPE);		
    QUAD_TYPE operator*=(QUAD_TYPE);	
    QUAD_TYPE operator/=(QUAD_TYPE);		
    QUAD_TYPE operator+(QUAD_TYPE);		
    QUAD_TYPE operator+(double);		
    QUAD_TYPE operator-(QUAD_TYPE);		
    QUAD_TYPE operator-(double);		
    QUAD_TYPE operator-();		
    QUAD_TYPE operator*(QUAD_TYPE);	
    QUAD_TYPE operator*(double);			
    QUAD_TYPE operator/(QUAD_TYPE);	
    QUAD_TYPE operator/(double);		
    INT operator>(QUAD_TYPE);		
    INT operator>(double);			
    INT operator<(QUAD_TYPE);			
    INT operator<(double);			
    INT operator==(QUAD_TYPE);	
    INT operator==(double);			
    INT operator!=(QUAD_TYPE);		
    INT operator!=(double);			
    INT operator>=(QUAD_TYPE);			
    INT operator>=(double);			
    INT operator<=(QUAD_TYPE);			
    INT operator<=(double);			
};
#else
typedef QUADFP QUAD_TYPE;
#endif
#else
typedef struct { INT32 qval[4]; } QUAD_TYPE;
#endif

/* ====================================================================
 *
 * The target constant (TCON) structure.
 *
 * This structure contains the type of the constant (as an MTYPE value
 * from mtypes.h) and the value.  The only direct access to a TCON
 * allowed outside the targ_const package is a type query.  It should
 * be assumed that the constant values are in an unknown format --
 * direct references to the value fields are therefore dangerous.
 *
 * NOTE:  The MTYPE field 'ty' only needs to be 8 bits, but if it were,
 * there will be 3 bytes of padding between 'ty' and the union 'vals',
 * below.  Since we search for constants in the Const_Table by casting
 * a TCON as a pointer to a string and then doing string-search
 * assuming strings of length sizeof(TCON), padding is being avoided.
 * (This is to avoid potential problems due to uninitialized data in
 * the padding bytes screwing up the string search.)  -- Warren
 *
 * WARNING:  With respect to the above comment, note that the end of
 * the structure contains padding as well for values shorter than the
 * maximum (e.g. float).  Furthermore, since the union contains a
 * double, the padding is probably actually 7 bytes.  -- Jim
 * (TODO:  Perhaps we should worry about this.)
 *
 * ====================================================================
 */


struct TCON { 
    mTYPE_ID ty;			/* The type of this TCON: MTYPE_xx */
    UINT32 flags;			/* flag bits */
#ifdef _LP64
    INT64  pad;                         /* align at 64 bytes */
#endif /* _LP64 */

    union {
	struct {
#if HOST_IS_LITTLE_ENDIAN
	    mINT32 v0, v1, v2, v3, v4, v5, v6, v7;	/* Individual signed words */
#else
	    mINT32 v1, v0, v3, v2, v5, v4, v7, v6;	/* Individual signed words */
#endif
	} ival;
	struct {
#if HOST_IS_LITTLE_ENDIAN
	    mUINT32 u0, u1, u2, u3, u4, u5, u6, u7;	/* Individual unsigned words */
#else
	    mUINT32 u1, u0, u3, u2, u5, u4, u7, u6;	/* Individual unsigned words */
#endif
	} uval;
#ifdef KEY 
	struct {
	    mINT64 ll0;
	    mINT64 ll1;
            mINT64 ll2;
            mINT64 ll3;
	} llval;
#endif
	mINT32 word0;			/* for getting the first integer word */
	mINT64 i0;			/* Signed integer */
	mUINT64 k0;			/* Unsigned integer */
	float fval;			/* 32-bit floating point */
	double dval;			/* 64-bit floating point */
	long double ldval;		/* 80-bit floating point */
	QUAD_TYPE qval;			/* 128-bit floating point */
	struct {			/* string literal */
	    mUINT32 cp;			/* STR_IDX to string table */
	    mUINT32 len;
	} sval;
    } vals;
    union {
	struct {
#if HOST_IS_LITTLE_ENDIAN
	    mINT32 v0, v1, v2, v3, v4, v5, v6, v7;	/* Individual signed words */
#else
	    mINT32 v1, v0, v3, v2, v5, v4, v7, v6;	/* Individual signed words */
#endif
	} ival;
#ifdef KEY
	mINT32 word0;
	mINT64 i0;
#endif // key
	float fival;
	double dival;
	long double ldival;		/* 80-bit floating point */
	QUAD_TYPE qival;
    } cmplxval;
};

#ifdef __cplusplus
inline TYPE_ID
TCON_ty (const TCON& tcon)		{ return tcon.ty; }
inline void
Set_TCON_ty (TCON& tcon, TYPE_ID mtype)	{ tcon.ty = mtype; }    
inline INT32
TCON_ival (const TCON& tcon)		{ return tcon.vals.word0; }    
#ifdef KEY
inline INT32
TCON_cival (const TCON& tcon)		{ return tcon.cmplxval.word0; }
inline INT64
TCON_ci0 (const TCON& tcon)		{ return tcon.cmplxval.i0; }
#endif // KEY
inline UINT32
TCON_uval (const TCON& tcon)		{ 
#if HOST_IS_LITTLE_ENDIAN
  return (UINT32) tcon.vals.word0; 
#else
  return (UINT32) tcon.vals.i0; 
#endif
}    
inline INT64
TCON_i0 (const TCON& tcon)		{ return tcon.vals.i0; }    
inline UINT64
TCON_k0 (const TCON& tcon)		{ return tcon.vals.k0; }
inline float
TCON_fval (const TCON& tcon)		{ return tcon.vals.fval; }
inline double
TCON_dval (const TCON& tcon)		{ return tcon.vals.dval; }    
inline long double
TCON_ldval (const TCON& tcon)		{ return tcon.vals.ldval; }    
inline QUAD_TYPE
TCON_qval (const TCON& tcon)		{ return tcon.vals.qval; }
inline mUINT32
TCON_str_idx (const TCON& tcon)		{ return tcon.vals.sval.cp; }
inline mUINT32
TCON_str_len (const TCON& tcon)		{ return tcon.vals.sval.len; }
#else /* __cplusplus */
typedef struct TCON TCON;
#endif /* __cplusplus */

/* The only field that is to be accessed outside of "targ_const.c" is
 * the type field.  Macros to reference the other fields of the TCON
 * structure are in "targ_const.c".
 *
 * Moreover, modification of TCON_ty is prohibited except in
 * targ_const.c, though accessing it is fine.  TCON_ty is being defined
 * below in a way that can not be used to modify it.  In targ_const.c
 * it is undefined and redefined.
 *
 */

#define TCON_ADD_NULL		0x1
#define TCON_add_null(c)	((c).flags & TCON_ADD_NULL)
#define Set_TCON_add_null(c)	((c).flags |= TCON_ADD_NULL)

#ifdef really_call_bzero
#define TCON_clear(c)	really_call_bzero (&c, sizeof(TCON))
#else
#define TCON_clear(c)	BZERO (&c, sizeof(TCON))
#endif


/* ====================================================================
 *
 * Procedural interface to the target constant (TCON) structure.
 *
 * ====================================================================
 */


#ifdef opcode_INCLUDED
/* Perform a target op on target constants: */
extern TCON Targ_WhirlOp ( OPCODE op, TCON left_opnd, TCON right_opnd,
		     BOOL *folded /* Was operator folded? */ );
#endif

extern TCON Targ_IntrinsicOp ( UINT32 intrinsic, TCON c[],
		     BOOL *folded /* Was operator folded? */ );

/* Perform target exponentiation: */
extern TCON Targ_Pow ( TCON mantissa, TCON exponent );

/* Convert between target constants: */
extern TCON Targ_Conv ( TYPE_ID newtype, TCON cvalue );

/* Typecast between target constants (must be of same sizes): */
extern TCON Targ_Cast ( TYPE_ID newtype, TCON cvalue );

/* Convert (char *) representation of host to target constant: */
extern TCON Targ_Atoc ( TYPE_ID ctype, char *ascii );

/* Convert (char *) representation of floating-point hexadecimal constant
 * to target constant:
 */
extern TCON Targ_Hexfptoc ( const TYPE_ID ty, const char * const str );

/* Convert to host integer.  The TCON must be of a type reasonably
 * convertible to an integer type, e.g. float types are not allowed:
 */
extern INT64 Targ_To_Host ( TCON cvalue );

/* Convert to host signed integer.  The TCON must be of a type reasonably
 * convertible to an integer type, e.g. float types are not allowed:
 */
extern INT64 Targ_To_Signed_Host ( TCON cvalue );

/* Convert host integer value to a TCON of given type: */
extern TCON Host_To_Targ ( TYPE_ID ctype, INT64 ivalue );

/* Convert host floating point value to a TCON of given type: */
extern TCON Host_To_Targ_Float ( TYPE_ID ctype, double fvalue );
extern TCON Host_To_Targ_Float_10 ( TYPE_ID ctype, long double fvalue );
extern TCON Host_To_Targ_Float_4 ( TYPE_ID ctype, float fvalue );
extern TCON Host_To_Targ_Quad  ( QUAD_TYPE fvalue );
#ifdef KEY
extern TCON Create_Simd_Const ( TYPE_ID ctype, TCON t );
#endif
#ifdef TARG_X8664
extern TCON Create_Simd_Prog_Const ( TYPE_ID ctype, INT64 val );
#endif 

/* Convert a TCON to a double or a quad: */
extern double Targ_To_Host_Float ( TCON fvalue );
extern QUAD_TYPE Targ_To_Host_Quad ( TCON fvalue );

/* Convert host complex value to a TCON of given type: */
extern TCON Host_To_Targ_Complex ( TYPE_ID ctype, double real, double imag );
extern TCON Host_To_Targ_Complex_10 ( TYPE_ID ctype, long double, long double );
extern TCON Host_To_Targ_Complex_4 ( TYPE_ID ctype, float real, float imag );
extern TCON Host_To_Targ_Complex_Quad ( QUAD_TYPE real, QUAD_TYPE imag );

/* Make complex TCON from two TCONs representing real and imaginary parts. */
extern TCON Make_Complex ( TYPE_ID ctype, TCON real, TCON imag );

/* Create Tcon for unitialized variable */
extern TCON Host_To_Targ_UV(TYPE_ID ctype);

/* Extract complex real/imag TCON to a TCON: */
extern TCON Extract_Complex_Real( TCON complex);
extern TCON Extract_Complex_Imag( TCON complex);

/* Extract hi/lo of quad TCON to a TCON: */
extern TCON Extract_Quad_Hi( TCON q);
extern TCON Extract_Quad_Lo( TCON q);

#ifdef KEY
/* Extract hi/lo of Double TCON to a TCON: */
extern TCON Extract_Double_Hi( TCON q);
extern TCON Extract_Double_Lo( TCON q);

/* Extract hi/lo of LongLong TCON to a TCON: */
extern TCON Extract_LongLong_Hi(TCON q);
extern TCON Extract_LongLong_Lo( TCON q);
#endif

/*
 * For C and C++ String TCONs should include the trailing NULL.
 * For Fortran they may not include the NULL (in fact it may be
 * wrong to include it if have array of these strings). 
 * So the user must be careful about whether the "len" that is
 * passed includes the NULL.
 * Targ_String_Length always returns the "len" that was initially passed in.
 */
extern TCON Host_To_Targ_String ( TYPE_ID ctype, const char *cp, UINT32 len );
extern char *Targ_String_Address ( TCON cvalue );
extern mUINT32 Targ_String_Length ( TCON cvalue );

/* Print constant according to given printf format.  The return string
 * is a static string which gets recycled after 8 calls, so it must
 * be used immediately or copied:
 */
extern char *Targ_Print (const char *fmt, TCON cvalue );

/* Format the given string as a printable string, by replacing special
 * characters by the C source codes, e.g. "\n" for newline, "\003" for
 * '^C', etc.
 */
extern BOOL Targ_Format_String (
  char	*s,	/* String to format, */
  INT32	slen,	/* ... of this length, */
  char	*buf,	/* ... into this buffer, */
  INT32	blen,	/* ... with at most this many characters, */
  INT32	line,	/* ... with lines at most this long (0 = no limit), */
  char *divider	/* ... divided by this string. */
);

/* Emit a constant string of exactly length len to the given file.
 * If str[len-1] is not a null byte, make it one.  loc is an offset
 * from the relevant symbol where output begins, used only for
 * a comment.
 */
extern void Targ_Emit_String ( FILE *fl, char *str, INT32 len, INTSC loc );

/* Emit a target constant to the assembly file, with repeat count rc,
 * beginning at offset loc (for comment only).
 * The add_null field only applies to string constants.
 */
extern void Targ_Emit_Const ( FILE *fl, TCON tvalue, BOOL add_null, INTSC rc, INTSC loc );
#ifdef KEY
extern void Targ_Emit_EH_Const ( FILE *fl, TCON tvalue, BOOL add_null, INTSC rc, INTSC loc, INT format=0 );
#endif // KEY

#if defined(BACK_END) || defined(QIKKI_BE)
/* Emit a target constant to the object file into the given section */
extern void Em_Targ_Emit_Const (void *scn, TCON tvalue, BOOL add_null, INTSC rc);
#endif

/* Emit uninitialized storage space of the given length to the assembly
 * file, beginning at offset loc (for comment only):
 */
extern void Targ_Emit_Space ( FILE *fl, INT len, INT loc );

/* Emit an assembly language comment to the assembly file: */
extern void Targ_Emit_Cmt ( FILE *fl, char *cmt );

/* Emit the length of the given dimension of the given type to the
 * assembly file, or '1' if it is not a compile-time constant:
 */
extern void Targ_Emit_Dim ( FILE *fl, struct ty *ty, INT dim );

/* Put the given constant into the given buffer in target (i.e. object
 * file) format, independent of the host format, and return a pointer
 * to it:
 */
extern char *Tcon_To_Str ( char *buf, TCON cval );

/* Given a constant in target format in the given buffer, of the given
 * type, convert it to a TCON:
 */
extern TCON Str_To_Tcon ( TYPE_ID ctype, char *buf );
extern TCON Bit_Str_To_Tcon ( TYPE_ID ctype, char *buf );

/* A target constant with value zero: */
extern TCON Zero_I4_Tcon;
extern TCON Zero_I8_Tcon;
extern TCON Quad_Zero_Tcon;

/* Is the given constant a zero? */
extern BOOL Targ_Is_Zero ( TCON t );
extern BOOL Targ_Is_Neg_Zero ( TCON t );

/* Determine whether a TCON represents an integral value, and if so
 * return its value in *iv:
 */
extern BOOL Targ_Is_Integral ( TCON t, INT64 *iv );

/* Determine whether a TCON represents a power of two: */
extern BOOL Targ_Is_Power_Of_Two ( TCON t );

/* Determin whether a TCON contains only one bit set to 1
 * If onebit is not NULL then return the bit number which is on
 */
extern BOOL Targ_Contains_One_Bit_On ( TCON t, INT32 *onebit);

/* Determine the most significant bit that is on, with 0 being the
 * least-sig bit, and type's_size - 1 being the most-sig bit.
 * If no bits are on, return FALSE.
 */
extern BOOL Targ_Determine_High_Bit ( TCON t, INT32 *highbit );

/* Hash a TCON into a 32-bit integer modulo another integer: */
extern UINT32 Hash_TCON ( TCON * t, UINT32 modulus );

extern INT32 Targ_fp_class(TCON fvalue);

#ifdef Is_True_On
/* Check the TCON for validity. */
extern void Check_TCON (TCON *tc);
#endif /* Is_True_On */

#ifdef __cplusplus
}
#endif
#endif /* targ_const_INCLUDED */
