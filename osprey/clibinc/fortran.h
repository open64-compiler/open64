/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* USMID @(#) clibinc/fortran.h	92.5	10/06/99 12:10:46 */


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

#ifndef _FORTRAN_H
#define _FORTRAN_H


/*
 * C declarations for Fortran data types and for interlanguage data and
 * descriptor conversions.
 *
 * This header file defines a C type for each of the supported Fortran-90
 * types and kinds.   Descriptor conversion macros are defined to map
 * between C character pointers and the FORTRAN Character Descriptor format.
 * Data conversion macros are available for converting between Fortran
 * LOGICAL and C boolean (integer).
 */

/*
 * Fortran data types:   
 *
 * The mappings listed below match a Fortran data type with a corresponding
 * C data type.   The related data types agree in:
 *
 *	1) Memory packing size.
 *	2) Range of values which can be operated on or stored.
 *
 * Each data type has a macro of the form _F_INT4 which is defined only
 * on platforms which support the data type.  In cases where there is a
 * set of data types which all have the same memory packing size, and the
 * same range of values, only the one with the largest declared kind is
 * defined.
 */

#if	defined(_CRAY1)

/*
 * The non-declaration of _F_INT4 is an exception to our rule of defining
 * the macros for data types.  While int kind=4 is defined for YMP, the
 * compiler does not treat it differently from int kind=6, and does not
 * create any new entry points for it.  Therefore, it was decided not to
 * define _F_INT4.  The declared value for these macros is dependent upon
 * whether the data type is really supported, or whether the type is defined
 * as an array of another type, or is a single entity.  Those which are
 * a single entity are defined as 1, which those which are defined as array
 * are defined to be (-1).
 *
 * 6/13/95 - typedef statements are being added for non-supported data
 *	     types for use in the array intrinsics.  These data types are
 *	     valid for PVP, but they are treated by the compiler as instances
 *	     of the next larger distinct type.  (i.e. _f_int4 is valid, but
 *	     is treated as an _f_int6).  These new entry points are being
 *	     added for the MAXVAL/MINVAL routines, which can have distinct
 *	     entry points for each valid data type.
 */

#define	_F_INT6		1
#define	_F_INT8		1

typedef	int		_f_int6;	/* integer kind=6 (46-bit on CX/CEA) */
typedef	long		_f_int8;	/* integer kind=8 */
typedef	_f_int6		_f_int;		/* integer of default kind */
typedef	_f_int8		_f_int4;	/* integer kind=4 */
typedef	_f_int8		_f_int2;	/* integer kind=2 */
typedef	_f_int8		_f_int1;	/* integer kind=1 */

#define	_F_LOG8		1

typedef	long		_f_log8;	/* logical kind=8 */
typedef	_f_log8		_f_log;		/* logical of default kind */
typedef	_f_log8		_f_log4;	/* logical kind=4 */
typedef	_f_log8		_f_log2;	/* logical kind=2 */
typedef	_f_log8		_f_log1;	/* logical kind=1 */

#define	_F_REAL8	1
#define	_F_REAL16	1

typedef	double		_f_real8;	/* real kind=8 */
typedef	long double	_f_real16;	/* real kind=16 */

typedef	_f_real8	_f_real;	/* real of default kind */
typedef	_f_real16	_f_dble;	/* double precision */
typedef	_f_real8	_f_real4;	/* real kind=4 */

#define	_F_COMP8	1
#define	_F_COMP16	1

#if !defined(__cplusplus)
#if __STDC__ == 1
#include <complex.h>
#endif
typedef	_Complex double	_f_comp8;	/* complex kind=8 */
/* typedef _Complex long double	_f_comp16; */	/* complex kind=16 */
typedef	_f_comp8	_f_comp;	/* complex of default kind */
#endif

#endif	/* defined(_CRAY1) */


#if	defined(_CRAYMPP)

/*
 * 6/13/95 - typedef statements are being added for non-supported data
 *	     types for use in the array intrinsics.  These data types are
 *	     valid for PVP, but they are treated by the compiler as instances
 *	     of the next larger distinct type.  (i.e. _f_int2 is valid, but
 *	     is treated as an _f_int4).  These new entry points are being
 *	     added for the MAXVAL/MINVAL routines, which can have distinct
 *	     entry points for each valid data type.  */

#define	_F_INT4		1
#define	_F_INT8		1

typedef	short		_f_int4;	/* integer kind=4 */
typedef	int		_f_int8;	/* integer kind=8 */
typedef	_f_int8		_f_int;		/* integer of default kind */
typedef	_f_int4		_f_int2;	/* integer kind=2 */
typedef _f_int4		_f_int1;	/* integer kind=1 */

#define	_F_LOG4		1
#define	_F_LOG8		1

typedef	short		_f_log4;	/* logical kind=4 */
typedef	long		_f_log8;	/* logical kind=8 */
typedef	_f_log8		_f_log;		/* logical of default kind */
typedef	_f_log4		_f_log2;	/* logical kind=2 */
typedef	_f_log4		_f_log1;	/* logical kind=1 */

#define	_F_REAL4	1
#define	_F_REAL8	1

typedef float		_f_real4;	/* real kind=4 */
typedef	double		_f_real8;	/* real kind=8 */
#if _LD64
typedef	struct {long x[2];} _f_real16;	/* real kind=16 */
#define	_F_REAL16	(-1)
#else
typedef	long double	_f_real16;	/* real kind=16 */
#define	_F_REAL16	1
#endif
typedef	_f_real8	_f_real;	/* real of default kind */
typedef	_f_real16	_f_dble;	/* double precision */
typedef _f_real4	_f_real2;	/* real kind=2 */

#define _F_COMP4	1
#define	_F_COMP8	1

#if !defined(__cplusplus)
#include <complex.h>
typedef _Complex float	_f_comp4;	/* complex kind=4 */
typedef	_Complex double	_f_comp8;	/* complex kind=8 */
/* typedef _Complex long double	_f_comp16; */	/* complex kind=16 */
typedef	_f_comp8	_f_comp;	/* complex of default kind */
#endif

#endif	/* _CRAYMPP */


#if	defined(__mips)

/*
 *	MIPS n32 and 64 ABIs.
 */

#define	_F_INT1		1
#define	_F_INT2		1
#define	_F_INT4		1
#define	_F_INT8		1

typedef signed char	_f_int1;	 /* integer kind=1 */
typedef	short		_f_int2;	/* integer kind=2 */
typedef	int		_f_int4;	/* integer kind=4 */
typedef	long long	_f_int8;	/* integer kind=8 */
typedef	_f_int4		_f_int;		/* integer of default kind */

#define	_F_LOG1		1
#define	_F_LOG2		1
#define	_F_LOG4		1
#define	_F_LOG8		1

typedef	signed char	_f_log1;	/* logical kind=1 */
typedef	short		_f_log2;	/* logical kind=2 */
typedef	int		_f_log4;	/* logical kind=4 */
typedef	long long	_f_log8;	/* logical kind=8 */
typedef	_f_log4		_f_log;		/* logical of default kind */

#define	_F_REAL4	1
#define	_F_REAL8	1
#define	_F_REAL16	1

typedef	float		_f_real4;	/* real kind=4 */
typedef	double		_f_real8;	/* real kind=8 */
typedef	long double	_f_real16;	/* real kind=16 */
typedef	_f_real4	_f_real;	/* real of default kind */
typedef	_f_real8	_f_dble;	/* double precision */

#define	_F_COMP4	(-1)
#define	_F_COMP8	(-1)
#define	_F_COMP16	(-1)

typedef struct {_f_real4 x[2];} _f_comp4;  /* complex kind=4 */
typedef struct {_f_real8 x[2];} _f_comp8;  /* complex kind=8 */
typedef struct {_f_real16 x[2];} _f_comp16;  /* complex kind=16 */
typedef	_f_comp4	_f_comp;	/* complex of default kind */

#endif	/* __mips */

#if	defined(_LITTLE_ENDIAN)
/*
 *	32- and 64-bit ABIs.
 */

#define	_F_INT1		1
#define	_F_INT2		1
#define	_F_INT4		1
#define	_F_INT8		1

typedef signed char	_f_int1;	 /* integer kind=1 */
typedef	short		_f_int2;	/* integer kind=2 */
typedef	int		_f_int4;	/* integer kind=4 */
typedef	long long	_f_int8;	/* integer kind=8 */
typedef	_f_int4		_f_int;		/* integer of default kind */

#define	_F_LOG1		1
#define	_F_LOG2		1
#define	_F_LOG4		1
#define	_F_LOG8		1

typedef	signed char	_f_log1;	/* logical kind=1 */
typedef	short		_f_log2;	/* logical kind=2 */
typedef	int		_f_log4;	/* logical kind=4 */
typedef long long	_f_log8;	/* logical kind=8 */
typedef	_f_log4		_f_log;		/* logical of default kind */

#define	_F_REAL4	1
#define	_F_REAL8	1
#define	_F_REAL16	1

typedef	float		_f_real4;	/* real kind=4 */
typedef	double		_f_real8;	/* real kind=8 */
typedef	long double	_f_real16;	/* real kind=16 */
typedef	_f_real4	_f_real;	/* real of default kind */
typedef	_f_real8	_f_dble;	/* double precision */

/* For cray sv2 with a specific complex data type */
#ifdef _CRAY
#define	_F_COMP4	1
#define	_F_COMP8	1
#define	_F_COMP16	1

#if !defined(__cplusplus)
#if __STDC__ == 1
#include <complex.h>
#endif
typedef	_Complex float	_f_comp4;	/* complex kind=8 */
typedef	_Complex double	_f_comp8;	/* complex kind=8 */
typedef _Complex long double	_f_comp16; 	/* complex kind=16 */
typedef	_f_comp8	_f_comp;	/* complex of default kind */
#endif

#else	/* _CRAY */
#define	_F_COMP4	(-1)
#define	_F_COMP8	(-1)
#define	_F_COMP16	(-1)

typedef struct {_f_real4 x[2];} _f_comp4;  /* complex kind=4 */
typedef struct {_f_real8 x[2];} _f_comp8;  /* complex kind=8 */
typedef struct {_f_real16 x[2];} _f_comp16;  /* complex kind=16 */
typedef	_f_comp4	_f_comp;	/* complex of default kind */
#endif	/* _CRAY */

#endif	/* _LITTLE_ENDIAN */

#if	!defined(__mips) && defined(_WORD32) && !defined(_LITTLE_ENDIAN)

/*
 * 6/13/95 - typedef statements are being added for non-supported data
 *	     types for use in the array intrinsics.  These data types are
 *	     valid for PVP, but they are treated by the compiler as instances
 *	     of the next larger distinct type.  (i.e. _f_int1 is valid, but
 *	     is treated as an _f_int2).  These new entry points are being
 *	     added for the MAXVAL/MINVAL routines, which can have distinct
 *	     entry points for each valid data type.
 */

#define	_F_INT2		1
#define	_F_INT4		1
#define	_F_INT8		1

typedef	int		_f_int2;	/* integer kind=2 */
typedef	int		_f_int4;	/* integer kind=4 */
typedef	long long	_f_int8;	/* integer kind=8 */
typedef	_f_int4		_f_int;		/* integer of default kind */
typedef _f_int2		_f_int1;	/* integer kind=1 */

#define	_F_LOG2		1
#define	_F_LOG4		1
#define	_F_LOG8		1

typedef	int		_f_log2;	/* logical kind=2 */
typedef	int		_f_log4;	/* logical kind=4 */
typedef	long long	_f_log8;	/* logical kind=8 */
typedef	_f_log4		_f_log;		/* logical of default kind */
typedef	_f_log2		_f_log1;	/* logical kind=1 */

#define	_F_REAL4	1
#define	_F_REAL8	1
#if defined(_SOLARIS)	/* only Solaris is 128-bit for now */
#define	_F_REAL16	1
#endif

typedef	float		_f_real4;	/* real kind=4 */
typedef	double		_f_real8;	/* real kind=8 */
typedef	long double	_f_real16;	/* real kind=16 */
typedef	_f_real4	_f_real;	/* real of default kind */
typedef	_f_real8	_f_dble;	/* double precision */

#define	_F_COMP4	(-1)
#define	_F_COMP8	(-1)
#define	_F_COMP16	(-1)

typedef struct {_f_real4 x[2];} _f_comp4;  /* complex kind=4 */
typedef struct {_f_real8 x[2];} _f_comp8;  /* complex kind=8 */
typedef struct {_f_real16 x[2];} _f_comp16;  /* complex kind=16 */
typedef	_f_comp4	_f_comp;	/* complex of default kind */

#endif	/* NOT __mips, _WORD32, _LITTLE_ENDIAN */


/* Conversion between Fortran character descriptors and C character pointers */

/*
 * The "_fcd" type defines a Fortran character variable of default kind.
 * The use of this type is intended for interlanguage communication
 * between C and FORTRAN.  The type _fcd must not be used for any other 
 * purposes.  
 * 
 * The format of Fortran character descriptors on 24-bit A-register systems:
 * 
 *     | 17 bits          | 17 bits       |6 bits|       24 bits       |
 *     -----------------------------------------------------------------
 *     |                  | length        | BO   |     word address    |
 *     -----------------------------------------------------------------
 * 
 * The format of Fortran character descriptors on 32-bit A-register systems:
 * 
 *     |6 bits|         26 bits         |         32 bits              | 
 *     -----------------------------------------------------------------
 *     | BO   |         length          |       word address           |
 *     -----------------------------------------------------------------
 *
 * The format of Fortran character descriptors on 64-bit A-register systems
 * is defined as a two-word structure.  Note that in this instance, there
 * is no need to be concerned about the format of a C character pointer.
 * 
 *     |                             64 bits                           | 
 *     -----------------------------------------------------------------
 *     |                      C Character Pointer                      |
 *     -----------------------------------------------------------------
 *     |   Length (in bits on PVP and T3D; bytes on IRIX/Solaris/T3E )  |
 *     -----------------------------------------------------------------
 * 
 * 
 * The format of C character pointers on 24-bit A-register systems:
 * 
 *     |6 bits|         34 bits                 |       24 bits        | 
 *     -----------------------------------------------------------------
 *     | BO   |                                 |     word address     |
 *     -----------------------------------------------------------------
 * 
 * The format of C character pointers on 32-bit A-register systems:
 * 
 *     |6 bits|         26 bits         |         32 bits              | 
 *     -----------------------------------------------------------------
 *     | BO   |                         |       word address           |
 *     -----------------------------------------------------------------
 * 
 * where BO = bit offset.
 *
 * 
 * _fcdtocp() converts a Fortran character descriptor to a C character 
 * pointer.
 *
 * _fcdlen() extracts the length (in bytes) of a Fortran character
 * descriptor.
 *
 * _isfcd() returns true if an object (of type long) is an _fcd.  This
 * is NOT available on _ADDR64 systems and its use is being deprecated.
 *
 * _cptofcd() converts a C character pointer and a length into a Fortran 
 * character descriptor.
 *
 * The functions _fcdlen and _fcdtocp are guaranteed to work if their 
 * parameters have type _fcd.
 *
 * A Fortran character descriptor converted to a C charactor pointer and back
 * to a Fortran character descriptor:
 *
 *	f = _cptofcd(_fcdtocp(f), _fcdlen(f))
 * 
 * _lvtob() converts a Fortran LOGICAL value to a zero or a one.  The
 * parameter must be of type _f_log.
 *
 * _ltob() converts a Fortran LOGICAL value to a zero or a one.  The
 * parameter must be a pointer.
 *
 * _btol() converts a zero or nonzero value to a Fortran LOGICAL value.
 * The parameter must have integral type.
 *
 * _dvel_len() extracts the element length (in bytes) of an array from
 * a Fortran dope vector.
 *
 * All these functions have both function implementations (so that the address
 * of it can be taken) and macro or inline implementations (for speed).
 */ 
/*
 * The C 6.0 (and later) compiler on Unicos and Unicos/MK systems 
 * recognizes a struct, class, or union with the name "_FCD" as a
 * Fortran character descriptor.
 * It allows an object of this type to be passed to a Fortran function
 * by value instead of taking its address.  (Normally, an object with
 * non-pointer type will have its address taken before being passed to
 * Fortran function.)  Typedefs can be used to rename the type, but the
 * but the underlying struct, class, or union must have the name 
 * "_FCD". The C 4.0 and 5.0 compilers recognize a struct with
 * the name "_FCD" on Cray-T90 and Cray-MPP machines.
 */
#if	!defined(_UNICOS)		/* IRIX, Solaris */
 
typedef	struct	_FCD	{
	char		*c_pointer;	/* C character pointer		*/
	unsigned long	byte_len;	/* Length of item (in bytes)	*/
} _fcd;
 
typedef	_fcd	_dcf;			/* Equal on this architecture	*/

/* Define a temporary function to map _dcf's to _fcd's */

#define	_fcdtodcf(f)	(f)
#define	_dcftofcd(f)	(f)


#elif	defined(_ADDR64) 		/* CRAY T3D, CRAY TS, Solaris */
 
typedef	struct	_FCD	{
	char		*c_pointer;	/* C character pointer	*/
	unsigned long	fcd_len;	/* Length of item	*/
} _fcd;
 
typedef	_fcd	_dcf;			/* Equal on this architecture	*/

/* Define a temporary function to map _dcf's to _fcd's */

#define	_fcdtodcf(f)	(f)
#define	_dcftofcd(f)	(f)


#elif	_ADDR32 && ! defined(_WORD32)	/* CRAY Y-MP and CRAY-2 */
 
typedef	union	_FCD	{
	char	*c_pointer;		/* C character pointer		*/

	struct	{
	unsigned bit_offset	:  6,	/* Starting bit offset		*/
		 fcd_len	: 26,	/* Length of item (in bits)	*/
		 word_addr	: 32;	/* Word address			*/
	} _F;
} _dcf;		/* This can be _fcd when compiler knows about _FCD	*/

typedef	void	*_fcd;	/* Temporary, to ensure fortran functions work	*/

/* Define a temporary function to map _dcf's to _fcd's */

#define	_dcftofcd(f)	(*(_fcd *) &f)
#define	_fcdtodcf(f)	(*(_dcf *) &f)


#elif	_ADDR32 == 0			/* CRAY X-MP */

typedef	union	_FCD	{

	struct	{
	unsigned bit_offset	:  6,	/* Starting bit offset		*/
		 _Unused	: 34,	/* Unused			*/
		 word_addr	: 24;	/* Word address			*/
	} _C;

	struct	{
	unsigned _Unused	: 17,	/* Unused			*/
		 fcd_len	: 17,	/* Length of item (in bits)	*/
		 bit_offset	:  6,	/* Starting bit offset		*/
		 word_addr	: 24;	/* Word address			*/
	} _F;
} _dcf;	/* This can be _fcd when compiler knows about _FCD */

typedef	void	*_fcd;	/* Temporary, to ensure fortran functions work	*/

/* Define a temporary function to map _dcf's to _fcd's */

#define	_dcftofcd(f)	(*(_fcd *) &f)
#define	_fcdtodcf(f)	(*(_dcf *) &f)

#endif	/* CRAY X-MP */

#include <sys/cdefs.h>

__BEGIN_DECLS
extern	_fcd		_cptofcd (char *_Ccp, unsigned _Len);
extern	char *		_fcdtocp (_fcd _Fcd);
extern	unsigned long	_fcdlen (_fcd _Fcd);
extern	_f_log		_btol (long _BV);
extern	long		_lvtob (_f_log _LV);
extern	long		_ltob (_f_log *_LP);
#ifdef	_UNICOS
extern	char *		_f2ccpy (_fcd f, ...);
#endif
extern	char *		_fc_copy (_fcd f, char *s, int slen);
extern	char *		_fc_acopy (_fcd f);
extern  int		_c2fcpy(char *c, _fcd f);

#if	defined(_UNICOS) && !defined(_ADDR64) && !defined(_WORD32) 
extern	int		_isfcd (long _P);
#endif

#ifdef	_CRAYMPP
extern	void		*_S2PC (void *_SDP, unsigned int _ELSZ);
#endif

extern	unsigned long	_dvel_len (long _DVEL);

__END_DECLS

#if	!defined(_UNICOS)		/* IRIX, Solaris */

static	_fcd
__cptofcd(char *c, unsigned long l);
static	_fcd
__cptofcd(char *c, unsigned long l)
{
	_dcf	f;
 
	f.c_pointer	= c;
	f.byte_len	= l;
 
	return (_dcftofcd(f));
}

#define	__fcdtocp(f)	((_fcdtodcf(f)).c_pointer)

#define	__fcdlen(f)	((unsigned long) (_fcdtodcf(f)).byte_len)


#elif	defined(_ADDR64) 		/* CRAY T3D, CRAY TS */

static	_fcd
__cptofcd(char *c, unsigned long l);
static	_fcd
__cptofcd(char *c, unsigned long l)
{
	_dcf	f;
 
	f.c_pointer	= c;
#ifdef	_CRAYT3E
	f.fcd_len	= l;
#else
	f.fcd_len	= l << 3;
#endif
 
	return (_dcftofcd(f));
}

#define	__fcdtocp(f)	((_fcdtodcf(f)).c_pointer)

#ifdef	_CRAYT3E
#define	__fcdlen(f)	((unsigned long) (_fcdtodcf(f)).fcd_len)
#else
#define	__fcdlen(f)	((unsigned long) ((_fcdtodcf(f)).fcd_len) >> 3)
#endif


#elif	_ADDR32				/* CRAY Y-MP and CRAY-2 */

static	_fcd
__cptofcd(char *c, unsigned long l);
#pragma _CRI inline __cptofcd
static	_fcd
__cptofcd(char *c, unsigned long l)
{
	_dcf	f;
 
	f.c_pointer	= c;
	f._F.fcd_len   	= l << 3;
 
	return (_dcftofcd(f));
}

static	char *
__fcdtocp(_fcd f);
#pragma _CRI inline __fcdtocp
static	char *
__fcdtocp(_fcd f)
{
	char	*c;
	_dcf	d;

	d		= _fcdtodcf(f);
	d._F.fcd_len	= 0;
	c		= d.c_pointer;

	return (c);
}

#define	__fcdlen(f)	((unsigned int) ((_fcdtodcf(f))._F.fcd_len) >> 3)

#define	__isfcd(f)	(__fcdlen(f) != 0)

#elif	_ADDR32 == 0			/* CRAY X-MP */

static	_fcd
__cptofcd(char *c, unsigned long l);
#pragma _CRI inline __cptofcd
static	_fcd
__cptofcd(char *c, unsigned long l)
{
	_dcf	d;
 
	d		= *(_dcf *) &c;
	d._F.bit_offset	= d._C.bit_offset;
	d._F._Unused	= 0;
	d._F.fcd_len	= l << 3;
 
	return (_dcftofcd(d));
}

static	char *
__fcdtocp(_fcd f);
#pragma _CRI inline __fcdtocp
static	char *
__fcdtocp(_fcd f)
{
	char	*c;
	_dcf	d;

	d		= _fcdtodcf(f);
	d._C.bit_offset	= d._F.bit_offset;
	d._C._Unused	= 0;
	c		= *(char **) &d;

	return (c);
}

#define	__fcdlen(f)	((unsigned long) ((_fcdtodcf(f))._F.fcd_len) >> 3)

#define	__isfcd(f)	(__fcdlen(f) != 0)

#endif	/* CRAY X-MP */

/* Conversion functions between Fortran LOGICAL data and C boolean data */

#if	defined(__mips) || defined(_CRAYIEEE) || defined(_SOLARIS) || \
	defined(_SUNOS)  || defined(_ABSOFT) || defined(_LITTLE_ENDIAN)

#define __lvtob(l)	((long)((l) == 0 ? 0 : 1))
#define __btol(b)	((_f_log)((b) == 0 ? 0 : 1))

#elif	defined(_CRAY1)

#define __lvtob(l)	((long)((l) >> 63))
#define __btol(b)	((_f_log)((b) == 0 ? 0 : -1))

#endif /* __mips, _CRAYIEEE, _SOLARIS, _SUNOS, _ABSOFT, _LITTLE_ENDIAN */

#define __devl_len(dvl)	((long)((dvl) >> 3))

/* Equivalence inline functions to user-level functions */

#define	_cptofcd(_C, _L)	__cptofcd(_C, _L)
#define	_fcdtocp(_F)		__fcdtocp(_F)
#define	_fcdlen(_F)		__fcdlen(_F)
#define	_btol(_BV)		__btol(_BV)
#define _lvtob(_LV)		__lvtob(_LV)
#define _ltob(_LP)		__lvtob(*(_f_log *)_LP)
#define __dvel_len(_DVEL)	__dvel_len(long _DVEL)

#if	!defined(_ADDR64) && !defined(_WORD32) && !defined(__mips) && \
	!defined(_LITTLE_ENDIAN)
#define	_isfcd(_U)		__isfcd(_U)
#endif /* NOT _ADDR64,_WORD32, __mips, and _LITTLE_ENDIAN */

/*
 *	Determine whether a pointer points to a shared data descriptor.
 *	Note that a shared data descriptor presupposes the centrifuge
 *	function which presupposes CRAFT; which is available only on
 *	the CRAY-T3D.
 */

#ifdef	_CRAYT3D
#define	_issddptr(_P)		((unsigned)(_P) >> 63)
#else
#define	_issddptr(_P)		0
#define	_S2PC(_PTR, _SIZE)	((void *)(_PTR))
#endif

#endif /* !_FORTRAN_H */
