/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#ifndef AR_INTERNAL_H
#define AR_INTERNAL_H

#include "arith.h"

#include <limits.h>

#if !defined __STDC__ || __STDC__ == 0
#error Must be compiled with a Standard C compiler.
#endif

#if UINT_MAX < 0177777
#error The size of an int must be at least 16 bits.
#endif

#if ULONG_MAX < 017777777777
#error The size of a long must be at least 32 bits.
#endif

/* Internal AR_TYPE specifications (originally in arith.h) */


/**************************************************************************

	Bit representation of AR_TYPE

Integer:			cccssssUS			c = class (enum ar_class)
(bit pattern)		876543210			s = size (enum int_size)
										U = UNUSED
										S = signedness (enum ar_signedness)

Pointer:			cccffffss			c = class (enum ar_class)
(bit pattern)		876543210			f = format (enum pointer_format)
										s = size (enum ar_pointer_size)

Logical:			cccUUUUUU			c = class (enum ar_class)
(bit pattern)		876543210			U = UNUSED

Floating:			cccUUUssC			c = class (enum ar_class)
(bit pattern)		876543210			U = UNUSED
										s = size (enum ar_float_size)
										C = is simple/complex (0/1)

**************************************************************************/

/* Generic type declarations */

#define AR_CLASS(type)				(type & 0x1c0)


/* Integer type declarations */

typedef struct {
	unsigned int part1: 16;
	unsigned int part2: 16;
	unsigned int part3: 16;
	unsigned int part4:  8;
	unsigned int part5:  8;
} AR_INT_8_64;  /* used only for 8-bit integers */

typedef struct {
	unsigned int part1: 16;
	unsigned int part2: 16;
	unsigned int part3: 16;
	unsigned int part4: 16;
} AR_INT_64;  /* used for both int and pointer representation */

typedef struct {
	unsigned int part1: 16;
	unsigned int part2: 16;
	unsigned int part3: 16;
	unsigned int part4: 16;
	unsigned int part5: 16;
	unsigned int part6: 16;
	unsigned int part7: 16;
	unsigned int part8: 16;
} AR_INT_128;

#define AR_INT_SIZE(type)			(type & 0x3c)

#define AR_POINTER_FORMAT(type) 	(type & 0x3c)

#define AR_POINTER_SIZE(type)		(type & 0x3)

#define AR_SIGNEDNESS(type) 		(type & 0x1)


/* Floating point type declarations */

#define AR_FLOAT_FORMAT(type)		(type & 0x20)

#define AR_FLOAT_ROUND(type)		(type & 0x18)

#define AR_FLOAT_SIZE(type) 		(type & 0x6)

#define AR_FLOAT_IS_COMPLEX(type)	(type & 0x1)


/* IEEE floating point declarations */

#define AR_IEEE32_ZERO_BITS 	32
#define AR_IEEE32_EXPO_BITS 	8
#define AR_IEEE32_C0_BITS		7
#define AR_IEEE32_C1_BITS		16

#define AR_IEEE64_EXPO_BITS 	11
#define AR_IEEE64_C0_BITS		4
#define AR_IEEE64_C1_BITS		16
#define AR_IEEE64_C2_BITS		16
#define AR_IEEE64_C3_BITS		16

#define AR_IEEE128_EXPO_BITS	15
#define AR_IEEE128_C0_BITS		16
#define AR_IEEE128_C1_BITS		16
#define AR_IEEE128_C2_BITS		16
#define AR_IEEE128_C3_BITS		16
#define AR_IEEE128_C4_BITS		16
#define AR_IEEE128_C5_BITS		16
#define AR_IEEE128_C6_BITS		16

#define AR_IEEE32_MIN_EXPO		0000
#define AR_IEEE32_EXPO_BIAS 	0177
#define AR_IEEE32_MAX_EXPO		0376
#define AR_IEEE32_COEFF_BITS	23

#define AR_IEEE64_MIN_EXPO		00000
#define AR_IEEE64_EXPO_BIAS 	01777
#define AR_IEEE64_MAX_EXPO		03776
#define AR_IEEE64_COEFF_BITS	52

#define AR_IEEE128_MIN_EXPO 	000000
#define AR_IEEE128_EXPO_BIAS	037777
#define AR_IEEE128_MAX_EXPO 	077776
#define AR_IEEE128_COEFF_BITS	112		/* For extended-double format */

#define AR_IEEE32_ROUND_BITS	3		/* guard, round, and sticky */
#define AR_IEEE64_ROUND_BITS	3		/* guard, round, and sticky */
#define AR_IEEE128_ROUND_BITS	3		/* guard, round, and sticky */

#define AR_MIPS128_MIN_EXPO		AR_IEEE64_MIN_EXPO
#define AR_MIPS128_MAX_EXPO		AR_IEEE64_MAX_EXPO
#define AR_MIPS128_EXPO_BIAS	AR_IEEE64_EXPO_BIAS

typedef struct {
	unsigned int zero		: AR_IEEE32_ZERO_BITS;
	unsigned int sign		: 1;
	unsigned int expo		: AR_IEEE32_EXPO_BITS;
	unsigned int coeff0 	: AR_IEEE32_C0_BITS;
	unsigned int coeff1 	: AR_IEEE32_C1_BITS;
} AR_IEEE_32;

typedef struct {
	unsigned int sign		: 1;
	unsigned int expo		: AR_IEEE64_EXPO_BITS;
	unsigned int coeff0 	: AR_IEEE64_C0_BITS;
	unsigned int coeff1 	: AR_IEEE64_C1_BITS;
	unsigned int coeff2 	: AR_IEEE64_C2_BITS;
	unsigned int coeff3 	: AR_IEEE64_C3_BITS;
} AR_IEEE_64;

typedef struct {
	unsigned int sign		: 1;
	unsigned int expo		: AR_IEEE128_EXPO_BITS;
	unsigned int coeff0 	: AR_IEEE128_C0_BITS;
	unsigned int coeff1 	: AR_IEEE128_C1_BITS;
	unsigned int coeff2 	: AR_IEEE128_C2_BITS;
	unsigned int coeff3 	: AR_IEEE128_C3_BITS;
	unsigned int coeff4 	: AR_IEEE128_C4_BITS;
	unsigned int coeff5 	: AR_IEEE128_C5_BITS;
	unsigned int coeff6 	: AR_IEEE128_C6_BITS;
} AR_IEEE_128;

typedef struct {
	unsigned int sign		: 1;
	unsigned int expo		: AR_IEEE64_EXPO_BITS;
	unsigned int coeff0 	: AR_IEEE64_C0_BITS;
	unsigned int coeff1 	: AR_IEEE64_C1_BITS;
	unsigned int coeff2 	: AR_IEEE64_C2_BITS;
	unsigned int coeff3 	: AR_IEEE64_C3_BITS;
	unsigned int signl		: 1;
	unsigned int expol		: AR_IEEE64_EXPO_BITS;
	unsigned int coeff0l 	: AR_IEEE64_C0_BITS;
	unsigned int coeff1l 	: AR_IEEE64_C1_BITS;
	unsigned int coeff2l 	: AR_IEEE64_C2_BITS;
	unsigned int coeff3l 	: AR_IEEE64_C3_BITS;
} AR_MIPS_128;

typedef struct {
	unsigned int rsign		: 1;
	unsigned int rexpo		: AR_IEEE32_EXPO_BITS;
	unsigned int rcoeff0	: AR_IEEE32_C0_BITS;
	unsigned int rcoeff1	: AR_IEEE32_C1_BITS;
	unsigned int isign		: 1;
	unsigned int iexpo		: AR_IEEE32_EXPO_BITS;
	unsigned int icoeff0	: AR_IEEE32_C0_BITS;
	unsigned int icoeff1	: AR_IEEE32_C1_BITS;
} AR_CPLX_IEEE_32;

typedef struct {
	AR_IEEE_64 real;
	AR_IEEE_64 imag;
} AR_CPLX_IEEE_64;

typedef struct {
	AR_IEEE_128 real;
	AR_IEEE_128 imag;
} AR_CPLX_IEEE_128;

typedef struct {
	AR_MIPS_128 real;
	AR_MIPS_128 imag;
} AR_CPLX_MIPS_128;


/* Cray floating point declarations */

#define AR_CRAY_EXPO_BITS	15
#define AR_CRAY_C0_BITS 	16		/* must be smallest, if different */
#define AR_CRAY_C1_BITS 	16
#define AR_CRAY_C2_BITS 	16
#define AR_CRAY_ZERO_BITS	16
#define AR_CRAY_C3_BITS 	16
#define AR_CRAY_C4_BITS 	16
#define AR_CRAY_C5_BITS 	16

#define AR_CRAY_MIN_EXPO		020000
#define AR_CRAY_EXPO_BIAS		040001
#define AR_CRAY_MAX_EXPO		057777
#define AR_CRAY64_COEFF_BITS	48
#define AR_CRAY128_COEFF_BITS	96

typedef struct {
	unsigned int sign		: 1;
	unsigned int expo		: AR_CRAY_EXPO_BITS;
	unsigned int coeff0 	: AR_CRAY_C0_BITS;
	unsigned int coeff1 	: AR_CRAY_C1_BITS;
	unsigned int coeff2 	: AR_CRAY_C2_BITS;
} AR_CRAY_64;

typedef struct {
	unsigned int sign		: 1;
	unsigned int expo		: AR_CRAY_EXPO_BITS;
	unsigned int coeff0 	: AR_CRAY_C0_BITS;
	unsigned int coeff1 	: AR_CRAY_C1_BITS;
	unsigned int coeff2 	: AR_CRAY_C2_BITS;
	unsigned int zero		: AR_CRAY_ZERO_BITS;
	unsigned int coeff3 	: AR_CRAY_C3_BITS;
	unsigned int coeff4 	: AR_CRAY_C4_BITS;
	unsigned int coeff5 	: AR_CRAY_C5_BITS;
} AR_CRAY_128;

typedef struct {
	AR_CRAY_64 real;
	AR_CRAY_64 imag;
} AR_CPLX_CRAY_64;

typedef struct {
	AR_CRAY_128 real;
	AR_CRAY_128 imag;
} AR_CPLX_CRAY_128;


/* Generic type declarations */

typedef union {
	AR_INT_64			ar_i64;
	AR_INT_8_64			ar_i8;
	AR_INT_128			ar_i128;
	AR_CRAY_64			ar_f64;
	AR_CRAY_128			ar_f128;
	AR_IEEE_32			ar_ieee32;
	AR_IEEE_64			ar_ieee64;
	AR_IEEE_128			ar_ieee128;
	AR_MIPS_128			ar_mips128;
	AR_CPLX_CRAY_64		ar_cplx_f64;
	AR_CPLX_CRAY_128	ar_cplx_f128;
	AR_CPLX_IEEE_32		ar_cplx_ieee32;
	AR_CPLX_IEEE_64		ar_cplx_ieee64;
	AR_CPLX_IEEE_128	ar_cplx_ieee128;
	AR_CPLX_MIPS_128	ar_cplx_mips128;
} ar_data;


/* Internal global objects and support macros */

typedef struct {
	unsigned int ar_high_mode_bits1	: 16;
	unsigned int ar_high_mode_bits2	: 16;
	unsigned int ar_unused_mode_bits: 19;
	unsigned int ar_truncate_bits	: 6;	/* # of bits to truncate */
	unsigned int ar_128bit_format	: 1;	/* Extended/Double-Double Format */
	unsigned int ar_denorms_trap	: 1;	/* Denorm opnds trap if set */
	unsigned int ar_underflow_mode	: 2;	/* Underflow/denorm value mode */
	unsigned int ar_rounding_mode	: 2;	/* Rounding mode */
	unsigned int ar_float_format	: 1;	/* IEEE/Cray Floating Point Format */
} ar_state_info;

extern ar_state_info ar_state_register;

extern int ar_simulate;					/* Simulation off/on */
extern int ar_rounding_modes;			/* Rounding modes available */
extern int ar_underflow_modes;			/* Underflow modes available */

/*
 * These build left, right, and middle masks.
 */
#define MASKL(bits)			((unsigned long)							\
							 (~((~(unsigned long) 0) >> (bits))))
#define MASKR(bits) 		((unsigned long)							\
							 (~((~(unsigned long) 0) << (bits))))
#define MASKM(bits,where) 	((unsigned long) (MASKR(bits) << (where)))


/* Diagnostic message levels */
/* Do not change unless negotiated with other compiler sections
   (e.g. pdgcs, cmcs, cft-77, cft-90, etc.).  */
enum message_types {
	Comment,			Note,				Caution,			Warning,
	Error,				Internal,			Vector_Info,		Scalar_Info,
	Table,				Ansi,				Logfile_Warning,	Inline_Info,
	Info,				Tasking_Info,		Limit,				Logfile_Error,
	Logfile_Summary,	F77_ANSI,			Unknown};


/******************** Internal prototypes ***********************/

/* Integer prototypes */

int ar_add_integer(ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

void ar_dblshift(ar_data *,
				const AR_TYPE *resulttype,
				const ar_data *,
				const ar_data *,
				int);

int ar_divide_integer(ar_data *,
				const AR_TYPE *,
				ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

int ar_negate_integer(ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

int ar_multiply_integer(ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

int ar_subtract_integer(ar_data *,
				const AR_TYPE *,
				int *,
				const ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

/* IEEE floating point prototypes */

int ar_i32norm (signed int,
				unsigned long,
				unsigned long,
				AR_IEEE_32 *,
				int);

int ar_i64norm (signed int,
				unsigned long,
				unsigned long,
				AR_IEEE_64 *,
				int);

int ar_i128norm(signed int,
				unsigned long,
				unsigned long,
				AR_IEEE_128 *,
				int);

int ar_i32to64 (AR_IEEE_64 *,
				const AR_IEEE_32 *);

int ar_i64to32 (AR_IEEE_32 *,
				const AR_IEEE_64 *,
				const int);

int ar_i64to128(AR_IEEE_128 *,
				const AR_IEEE_64 *);

int ar_i128to64(AR_IEEE_64 *,
				const AR_IEEE_128 *,
				const int);

int ar_ifadd32 (AR_IEEE_32 *,
				const AR_IEEE_32 *,
				const AR_IEEE_32 *,
				int);

int ar_ifadd64 (AR_IEEE_64 *,
				const AR_IEEE_64 *,
				const AR_IEEE_64 *,
				int);

int ar_ifadd128(AR_IEEE_128 *,
				const AR_IEEE_128 *,
				const AR_IEEE_128 *,
				int);

int ar_ifdiv32 (AR_IEEE_32 *,
				const AR_IEEE_32 *,
				const AR_IEEE_32 *,
				int);

int ar_ifdiv64 (AR_IEEE_64 *,
				const AR_IEEE_64 *,
				const AR_IEEE_64 *,
				int);

int ar_ifdiv128(AR_IEEE_128 *,
				const AR_IEEE_128 *,
				const AR_IEEE_128 *,
				int);

int ar_ifix32  (AR_INT_64 *,
				const AR_IEEE_32 *,
				int,
				int);

int ar_ifix64  (AR_INT_64 *,
				const AR_IEEE_64 *,
				int,
				int);

int ar_ifix128 (AR_INT_64 *,
				const AR_IEEE_128 *,
				int,
				int);

int ar_iflt32  (AR_IEEE_32 *,
				const AR_INT_64 *,
				int,
				int);

int ar_iflt64  (AR_IEEE_64 *,
				const AR_INT_64 *,
				int,
				int);

int ar_iflt128 (AR_IEEE_128 *,
				const AR_INT_64 *,
				int,
				int);

int ar_ifmul32 (AR_IEEE_32 *,
				const AR_IEEE_32 *,
				const AR_IEEE_32 *,
				int);

int ar_ifmul64 (AR_IEEE_64 *,
				const AR_IEEE_64 *,
				const AR_IEEE_64 *,
				int);

int ar_ifmul128(AR_IEEE_128 *,
				const AR_IEEE_128 *,
				const AR_IEEE_128 *,
				int);

int ar_ifsub32 (AR_IEEE_32 *,
				const AR_IEEE_32 *,
				const AR_IEEE_32 *,
				int);

int ar_ifsub64 (AR_IEEE_64 *,
				const AR_IEEE_64 *,
				const AR_IEEE_64 *,
				int);

int ar_ifsub128(AR_IEEE_128 *,
				const AR_IEEE_128 *,
				const AR_IEEE_128 *,
				int);

int ar_isqrt64 (AR_IEEE_64 *,
				const AR_IEEE_64 *,
				int);

/* Cray floating point prototypes */

int ar_c64to128(AR_CRAY_128 *,
				const AR_CRAY_64 *);

int ar_c128to64(AR_CRAY_64 *,
				const AR_CRAY_128 *);

int ar_c1frecip(AR_CRAY_64 *,
				const AR_CRAY_64 *);

int ar_cfadd64 (AR_CRAY_64 *,
				const AR_CRAY_64 *,
				const AR_CRAY_64 *);

int ar_cfadd128(AR_CRAY_128 *,
				const AR_CRAY_128 *,
				const AR_CRAY_128 *);

int ar_cfdiv64 (AR_CRAY_64 *,
				const AR_CRAY_64 *,
				const AR_CRAY_64 *,
				int);

int ar_cfdiv128(AR_CRAY_128 *,
				const AR_CRAY_128 *,
				const AR_CRAY_128 *,
				int);

int ar_cfix64  (AR_INT_64 *,
				const AR_CRAY_64 *,
				int);

int ar_cfix128 (AR_INT_64 *,
				const AR_CRAY_128 *,
				int);

int ar_cflt64  (AR_CRAY_64 *,
				const AR_INT_64 *,
				int);

int ar_cflt128 (AR_CRAY_128 *,
				const AR_INT_64 *,
				int);

int ar_cfmul64 (AR_CRAY_64 *,
				const AR_CRAY_64 *,
				const AR_CRAY_64 *,
				int);

int ar_cfmul128(AR_CRAY_128 *,
				const AR_CRAY_128 *,
				const AR_CRAY_128 *,
				int);

int ar_crnd64  (AR_CRAY_64 *,
				const AR_CRAY_64 *);

int ar_crnd128 (AR_CRAY_128 *,
				const AR_CRAY_128 *);

int ar_cfsub64 (AR_CRAY_64 *,
				const AR_CRAY_64 *,
				const AR_CRAY_64 *);

int ar_cfsub128(AR_CRAY_128 *,
				const AR_CRAY_128 *,
				const AR_CRAY_128 *);

void ar_CRAY_64_trunc(AR_CRAY_64 *opnd);

/* Conversion prototypes */

int ar_convert_to_integral(ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

int ar_compose_complex(ar_data *,
				AR_TYPE *,
				const ar_data *,
				const ar_data *,
				const AR_TYPE *);

int ar_decompose_complex(ar_data *,
				ar_data *,
				AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

int ar_negate_float(ar_data *,
				const AR_TYPE *,
				const ar_data *,
				const AR_TYPE *);

/* IEEE <-> Cray floating point conversion prototypes */

int ar_c128toi64(AR_IEEE_64 *,
				const AR_CRAY_128 *);

int ar_i64toc128(AR_CRAY_128 *,
				const AR_IEEE_64 *);

int ar_ctoi64  (AR_IEEE_64 *,
				const AR_CRAY_64 *);

int ar_ctoi128 (AR_IEEE_128 *,
				const AR_CRAY_128 *);

int ar_itoc64  (AR_CRAY_64 *,
				const AR_IEEE_64 *,
				int);

int ar_itoc128 (AR_CRAY_128 *,
				const AR_IEEE_128 *,
				int);

#ifdef __mips
/* IEEE <-> MIPS 128 bit floating point conversion prototypes */
 
int ar_m128toi128(AR_IEEE_128 *out, long double *in);
 
int ar_i128tom128(long double *out, AR_IEEE_128 *in);
 
#endif

/* Random functions in need of prototypes */

extern int ar_ifcmp64 (const AR_IEEE_64 *a, const AR_IEEE_64 *b);

extern void ar_clear_unused_bits (ar_data *opnd, const AR_TYPE *opndtype);

/* Fortran 90 functions */

extern int ar_index (ar_data *result, const AR_TYPE *resulttype,
		     const char *str1, long len1, const char *str2,
		     long len2, long backward);

extern int ar_scan (ar_data *result, const AR_TYPE *resulttype,
		    const char *str1, long len1, const char *str2,
		    long len2, long backward);

extern int ar_verify (ar_data *result, const AR_TYPE *resulttype,
		      const char *str1, long len1, const char *str2,
		      long len2, long backward);

extern int ar_reshape (void *result, const void *source, const void *shape,
		       const void *pad, const void *order);

extern int ar_transfer (void *result, const void *source, const void *mold,
			long *length);

extern int ar_modulo (ar_data *result, const AR_TYPE *resulttype,
		      const ar_data *opnd1, const AR_TYPE *opnd1type,
		      const ar_data *opnd2, const AR_TYPE *opnd2type);

extern int ar_selected_real_kind (ar_data *result, const AR_TYPE *resulttype,
				  const ar_data *opnd1,
				  const AR_TYPE *opnd1type,
				  const ar_data *opnd2,
				  const AR_TYPE *opnd2type);

extern int ar_sqrt (ar_data *result, const AR_TYPE *resulttype,
		    const ar_data *opnd, const AR_TYPE *opndtype);

extern int ar_log (ar_data *result, const AR_TYPE *resulttype,
		   const ar_data *opnd, const AR_TYPE *opndtype);

extern int ar_exp (ar_data *result, const AR_TYPE *resulttype,
		   const ar_data *opnd, const AR_TYPE *opndtype);

extern int ar_cabs (ar_data *result, const AR_TYPE *resulttype,
		    const ar_data *opnd, const AR_TYPE *opndtype);

extern int ar_power(ar_data *result, const AR_TYPE *resulttype,
		    const ar_data *base, const AR_TYPE *basetype,
		    const ar_data *power, const AR_TYPE *powertype);

extern int ar_convert_str_to_float (ar_data *result, const AR_TYPE *resulttype,
				    const char *str);

extern int ar_divide_complex (ar_data *result, const AR_TYPE *resulttype,
			      const ar_data *opnd1, const AR_TYPE *opnd1type,
			      const ar_data *opnd2, const AR_TYPE *opnd2type);

extern int ar_cvt_str_to_float (ar_data *result, const AR_TYPE *resulttype,
				const char *str);

/* Miscellaneous support prototypes */

void ar_internal_error(int msgnum,
					   char *file,
					   int line);

void ar_set_invalid_result(ar_data *result,
						   const AR_TYPE *resulttype);

extern void PRINTMSG(int pseudo_line_num,
					 int msg_number,
					 enum message_types msg_severity,
					 int column_num,
					 ...);

/*
 * Architectures.
 */
typedef enum {
	AR_Arch_Unknown,
	AR_Arch_PVP,
	AR_Arch_PVP_IEEE,
	AR_Arch_T3D,
	AR_Arch_T3E,
	AR_Arch_SPARC,
	AR_Arch_MIPS
} AR_ARCHITECTURE;

extern AR_ARCHITECTURE ar_host(void);

#define HOST_IS_UNKNOWN			(ar_host() == AR_Arch_Unknown )
#define HOST_IS_PVP				(ar_host() == AR_Arch_PVP     )
#define HOST_IS_PVP_IEEE		(ar_host() == AR_Arch_PVP_IEEE)
#define HOST_IS_T3D				(ar_host() == AR_Arch_T3D     )
#define HOST_IS_T3E				(ar_host() == AR_Arch_T3E     )
#define HOST_IS_SPARC			(ar_host() == AR_Arch_SPARC   )
#define HOST_IS_MIPS			(ar_host() == AR_Arch_MIPS    )

#define HOST_IS_MPP				(HOST_IS_T3D || HOST_IS_T3E)

#define HOST_IS_CRAY_FLOAT		(HOST_IS_PVP)
#define HOST_IS_IEEE_FLOAT		(HOST_IS_PVP_IEEE ||	\
								 HOST_IS_MPP      ||	\
								 HOST_IS_SPARC    ||	\
								 HOST_IS_MIPS)


/* Macros of dubious value ... */

#define _Solaris		(__sparc__ && __svr4__)

#define ROUND_MODE(t)		((t >> 3) & 3)
#define UNROUNDED_TYPE(t)	(t &~ 0x18)

#define IS_ERROR_STATUS(s)	(((s) & AR_ERROR_STATUS) != 0)

/* evaluates to true if a signed int exceeds 46 bits of precision */
#define INT_OVERFLOWS_46_BITS(OPND) 								\
		((((OPND).ar_i64.part1 & 0xffff) != 0xffff ||				\
		  ((OPND).ar_i64.part2 & 0xe000) != 0xe000) &&				\
		 (((OPND).ar_i64.part1 & 0xffff) != 0 ||					\
		  ((OPND).ar_i64.part2 & 0xe000) != 0))


/* Macros supporting integer algorithms */

#define IS_INT8_ZERO(i)							\
		((i)->ar_i8.part5 == 0)
#define IS_INT16_ZERO(i)						\
		((i)->ar_i64.part4 == 0)
#define IS_INT24_ZERO(i)						\
		(((i)->ar_i64.part3 & 0xFF) == 0 &&		\
		 (i)->ar_i64.part4 == 0)
#define IS_INT32_ZERO(i)						\
		((i)->ar_i64.part3 == 0 &&				\
		 (i)->ar_i64.part4 == 0)
#define IS_INT46_ZERO(i)						\
		(((i)->ar_i64.part2 & 0x3FFF) == 0 &&	\
		 (i)->ar_i64.part3 == 0 &&				\
		 (i)->ar_i64.part4 == 0)
#define IS_INT64_ZERO(i)						\
		((i)->ar_i64.part1 == 0 &&				\
		 (i)->ar_i64.part2 == 0 &&				\
		 (i)->ar_i64.part3 == 0 &&				\
		 (i)->ar_i64.part4 == 0)
#define IS_INT128_ZERO(i)						\
		((i)->ar_i128.part1 == 0 &&				\
		 (i)->ar_i128.part2 == 0 &&				\
		 (i)->ar_i128.part3 == 0 &&				\
		 (i)->ar_i128.part4 == 0 &&				\
		 (i)->ar_i128.part5 == 0 &&				\
		 (i)->ar_i128.part6 == 0 &&				\
		 (i)->ar_i128.part7 == 0 &&				\
		 (i)->ar_i128.part8 == 0)

#define IS_INT8_UPPER_ZERO(i)					\
		((i)->ar_i8.part1 == 0 &&				\
		 (i)->ar_i8.part2 == 0 &&				\
		 (i)->ar_i8.part3 == 0 &&				\
		 (i)->ar_i8.part4 == 0)
#define IS_INT16_UPPER_ZERO(i)					\
		((i)->ar_i64.part1 == 0 &&				\
		 (i)->ar_i64.part2 == 0 &&				\
		 (i)->ar_i64.part3 == 0)
#define IS_INT24_UPPER_ZERO(i)					\
		((i)->ar_i64.part1 == 0 &&				\
		 (i)->ar_i64.part2 == 0 &&				\
		 ((i)->ar_i64.part3 & 0xFF00) == 0)
#define IS_INT32_UPPER_ZERO(i)					\
		((i)->ar_i64.part1 == 0 &&				\
		 (i)->ar_i64.part2 == 0)
#define IS_INT46_UPPER_ZERO(i)					\
		((i)->ar_i64.part1 == 0 &&				\
		 ((i)->ar_i64.part2 & 0xC000) == 0)

#define ZERO_INT8(i)							\
		(i)->ar_i8.part5 = 0
#define ZERO_INT16(i)							\
		(i)->ar_i64.part4 = 0
#define ZERO_INT24(i)							\
		do {									\
				(i)->ar_i64.part3 &= 0xFF00;	\
				(i)->ar_i64.part4 = 0;			\
		} while(0)
#define ZERO_INT32(i)							\
		(i)->ar_i64.part3 =						\
			(i)->ar_i64.part4 = 0
#define ZERO_INT64(i)							\
		(i)->ar_i64.part1 =						\
			(i)->ar_i64.part2 =					\
			(i)->ar_i64.part3 =					\
			(i)->ar_i64.part4 = 0
#define ZERO_INT128(i)							\
		(i)->ar_i128.part1 =					\
			(i)->ar_i128.part2 =				\
			(i)->ar_i128.part3 =				\
			(i)->ar_i128.part4 =				\
			(i)->ar_i128.part5 =				\
			(i)->ar_i128.part6 =				\
			(i)->ar_i128.part7 =				\
			(i)->ar_i128.part8 = 0

#define ZERO_INT8_UPPER(i)						\
		(i)->ar_i8.part1 =						\
			(i)->ar_i8.part2 =					\
			(i)->ar_i8.part3 =					\
			(i)->ar_i8.part4 = 0
#define ZERO_INT16_UPPER(i)						\
		(i)->ar_i64.part1 =						\
			(i)->ar_i64.part2 =					\
			(i)->ar_i64.part3 = 0
#define ZERO_INT24_UPPER(i)						\
		do {									\
				(i)->ar_i64.part1 =				\
					(i)->ar_i64.part2 = 0;		\
				(i)->ar_i64.part3 &= 0xFF;		\
		} while(0)
#define ZERO_INT32_UPPER(i)						\
		(i)->ar_i64.part1 =						\
			(i)->ar_i64.part2 = 0

#define ZERO_INT8_ALL(i)						\
		(i)->ar_i8.part1 =						\
			(i)->ar_i8.part2 =					\
			(i)->ar_i8.part3 =					\
			(i)->ar_i8.part4 =					\
			(i)->ar_i8.part5 = 0
#define ZERO_INT16_ALL(i)						\
		(i)->ar_i64.part1 =						\
			(i)->ar_i64.part2 =					\
			(i)->ar_i64.part3 =					\
			(i)->ar_i64.part4 = 0
#define ZERO_INT24_ALL(i)						\
			ZERO_INT16_ALL(i)
#define ZERO_INT32_ALL(i)						\
			ZERO_INT16_ALL(i)
#define ZERO_INT64_ALL(i)						\
			ZERO_INT16_ALL(i)
#define ZERO_INT128_ALL(i)						\
		(i)->ar_i128.part1 =					\
			(i)->ar_i128.part2 =				\
			(i)->ar_i128.part3 =				\
			(i)->ar_i128.part4 =				\
			(i)->ar_i128.part5 =				\
			(i)->ar_i128.part6 =				\
			(i)->ar_i128.part7 =				\
			(i)->ar_i128.part8 = 0

#define IS_INT8_UPPER_ONES(i)					\
		((i)->ar_i8.part1 == 0xFFFF &&			\
		 (i)->ar_i8.part2 == 0xFFFF &&			\
		 (i)->ar_i8.part3 == 0xFFFF &&			\
		 (i)->ar_i8.part4 == 0xFF)
#define IS_INT16_UPPER_ONES(i)					\
		((i)->ar_i64.part1 == 0xFFFF &&			\
		 (i)->ar_i64.part2 == 0xFFFF &&			\
		 (i)->ar_i64.part3 == 0xFFFF)
#define IS_INT24_UPPER_ONES(i)					\
		((i)->ar_i64.part1 == 0xFFFF &&			\
		 (i)->ar_i64.part2 == xxFFFF &&			\
		 ((i)->ar_i64.part3 & 0xFF00) == 0xFF00)
#define IS_INT32_UPPER_ONES(i)					\
		((i)->ar_i64.part1 == 0xFFFF &&			\
		 (i)->ar_i64.part2 == 0xFFFF)
#define IS_INT46_UPPER_ONES(i)					\
		((i)->ar_i64.part1 == 0xFFFF &&			\
		 ((i)->ar_i64.part2 & 0xC000) == 0xC000)

#define INT8_SIGN(i)							\
		((i)->ar_i8.part5  & 0x80  )
#define INT16_SIGN(i)							\
		((i)->ar_i64.part4 & 0x8000)
#define INT24_SIGN(i)							\
		((i)->ar_i64.part3 & 0x80  )
#define INT32_SIGN(i)							\
		((i)->ar_i64.part3 & 0x8000)
#define INT64_SIGN(i)							\
		((i)->ar_i64.part1 & 0x8000)
#define INT128_SIGN(i)							\
		((i)->ar_i128.part1 & 0x8000)

#define INT_SIGN(t, i)								\
		((AR_INT_SIZE(t) == AR_INT_SIZE_8   &&		\
		  INT8_SIGN(i))							||	\
		 (AR_INT_SIZE(t) == AR_INT_SIZE_16  &&		\
		  INT16_SIGN(i))						||	\
		 (AR_INT_SIZE(t) == AR_INT_SIZE_24  &&		\
		  INT24_SIGN(i))						||	\
		 (AR_INT_SIZE(t) == AR_INT_SIZE_32  &&		\
		  INT32_SIGN(i))						||	\
		 (AR_INT_SIZE(t) == AR_INT_SIZE_46  &&		\
		  INT64_SIGN(i))						||	\
		 (AR_INT_SIZE(t) == AR_INT_SIZE_64  &&		\
		  INT64_SIGN(i))						||	\
		 (AR_INT_SIZE(t) == AR_INT_SIZE_128 &&		\
		  INT128_SIGN(i)))

#define COPY_INT8(i, j)													\
		(i)->ar_i8.part5 = (j)->ar_i8.part5
#define COPY_INT16(i, j)												\
		(i)->ar_i64.part4 = (j)->ar_i64.part4
#define COPY_INT24(i, j)												\
		do {															\
				(i)->ar_i64.part3 = (j)->ar_i64.part3 & 0xFF00;			\
				(i)->ar_i64.part4 = (j)->ar_i64.part4;					\
		} while(0)
#define COPY_INT32(i, j)												\
		do {															\
				(i)->ar_i64.part3 = (j)->ar_i64.part3;					\
				(i)->ar_i64.part4 = (j)->ar_i64.part4;					\
		} while(0)
#define COPY_INT46(i, j)												\
		do {															\
				(i)->ar_i64.part2 = (j)->ar_i64.part2 & 0x3FFF;			\
				(i)->ar_i64.part3 = (j)->ar_i64.part3;					\
				(i)->ar_i64.part4 = (j)->ar_i64.part4;					\
		} while(0)
#define COPY_INT64(i, j)												\
		do {															\
				(i)->ar_i64.part1 = (j)->ar_i64.part1;					\
				(i)->ar_i64.part2 = (j)->ar_i64.part2;					\
				(i)->ar_i64.part3 = (j)->ar_i64.part3;					\
				(i)->ar_i64.part4 = (j)->ar_i64.part4;					\
		} while(0)
#define COPY_INT128(i, j)												\
		do {															\
				(i)->ar_i128.part1 = (j)->ar_i128.part1;				\
				(i)->ar_i128.part2 = (j)->ar_i128.part2;				\
				(i)->ar_i128.part3 = (j)->ar_i128.part3;				\
				(i)->ar_i128.part4 = (j)->ar_i128.part4;				\
				(i)->ar_i128.part5 = (j)->ar_i128.part5;				\
				(i)->ar_i128.part6 = (j)->ar_i128.part6;				\
				(i)->ar_i128.part7 = (j)->ar_i128.part7;				\
				(i)->ar_i128.part8 = (j)->ar_i128.part8;				\
		} while(0)

#define INT8_TO_HOST_SINT64(i,a)										\
		do {															\
			if (INT8_SIGN(a))											\
				i = -(((~((AR_HOST_SINT64) ((a)->ar_i8.part5))) +		\
					   1												\
					  ) &												\
					  ~(-((AR_HOST_SINT64) 1) << 8)						\
					 );													\
			else														\
				i = (AR_HOST_SINT64) (a)->ar_i8.part5;					\
		} while (0)
#define INT16_TO_HOST_SINT64(i,a)										\
		do {															\
			if (INT16_SIGN(a))											\
				i = -(((~((AR_HOST_SINT64) ((a)->ar_i64.part4))) +		\
					   1												\
					  ) &												\
					  ~(-((AR_HOST_SINT64) 1) << 16)					\
					 );													\
			else														\
				i = (AR_HOST_SINT64) (a)->ar_i64.part4;					\
		} while (0)
#define INT32_TO_HOST_SINT64(i,a)										\
		do {															\
			if (INT32_SIGN(a))											\
				i = -(((~((((AR_HOST_SINT64) (a)->ar_i64.part3) << 16)	\
						  |												\
						  (((AR_HOST_SINT64) (a)->ar_i64.part4)      )	\
						 )												\
					   ) +												\
					   1												\
					  ) &												\
					  ~(-((AR_HOST_SINT64) 1) << 32)					\
					 );													\
			else														\
				i = (((AR_HOST_SINT64) (a)->ar_i64.part3) << 16) |		\
					(((AR_HOST_SINT64) (a)->ar_i64.part4)      );		\
		} while (0)
#define INT64_TO_HOST_SINT64(i,a)										\
		do {															\
			i = (((AR_HOST_SINT64) (a)->ar_i64.part1) << 48) |			\
				(((AR_HOST_SINT64) (a)->ar_i64.part2) << 32) |			\
				(((AR_HOST_SINT64) (a)->ar_i64.part3) << 16) |			\
				(((AR_HOST_SINT64) (a)->ar_i64.part4)      );			\
		} while (0)

/* Macros supporting IEEE floating point algorithms */

#define IS_IEEE32_NZ_COEFF(x)										\
		((x)->coeff0 != 0 ||										\
		 (x)->coeff1 != 0)

#define IS_IEEE64_NZ_COEFF(x)										\
		((x)->coeff0 != 0 ||										\
		 (x)->coeff1 != 0 ||										\
		 (x)->coeff2 != 0 ||										\
		 (x)->coeff3 != 0)

#define IS_IEEE128_NZ_COEFF(x)										\
		((x)->coeff0 != 0 ||										\
		 (x)->coeff1 != 0 ||										\
		 (x)->coeff2 != 0 ||										\
		 (x)->coeff3 != 0 ||										\
		 (x)->coeff4 != 0 ||										\
		 (x)->coeff5 != 0 ||										\
		 (x)->coeff6 != 0)

#define IS_MIPS128_NZ_COEFF(x) \
		((x)->coeff0  != 0 || \
		 (x)->coeff1  != 0 || \
		 (x)->coeff2  != 0 || \
		 (x)->coeff3  != 0 || \
		 (x)->coeff0l != 0 || \
		 (x)->coeff1l != 0 || \
		 (x)->coeff2l != 0 || \
		 (x)->coeff3l != 0 )


#define IS_IEEE32_NaN(x)											\
		((x)->expo > AR_IEEE32_MAX_EXPO && IS_IEEE32_NZ_COEFF(x))

#define IS_IEEE64_NaN(x)											\
		((x)->expo > AR_IEEE64_MAX_EXPO && IS_IEEE64_NZ_COEFF(x))

#define IS_IEEE128_NaN(x)											\
		((x)->expo > AR_IEEE128_MAX_EXPO && IS_IEEE128_NZ_COEFF(x))

#define IS_MIPS128_NaN(x) \
		(((x)->expo > AR_MIPS128_MAX_EXPO || \
		  (x)->expol > AR_MIPS128_MAX_EXPO) && IS_MIPS128_NZ_COEFF(x))


#define ADDIEEE32(sum,carry,a,b) do {								\
		(sum).coeff1 = (carry) += (a).coeff1 + (b).coeff1;			\
		(carry) >>= AR_IEEE32_C1_BITS;								\
		(sum).coeff0 = (carry) += (a).coeff0 + (b).coeff0;			\
		(carry) >>= AR_IEEE32_C0_BITS;								\
} while (0)

#define ADDIEEE64(sum,carry,a,b) do {								\
		(sum).coeff3 = (carry) += (a).coeff3 + (b).coeff3;			\
		(carry) >>= AR_IEEE64_C3_BITS;								\
		(sum).coeff2 = (carry) += (a).coeff2 + (b).coeff2;			\
		(carry) >>= AR_IEEE64_C2_BITS;								\
		(sum).coeff1 = (carry) += (a).coeff1 + (b).coeff1;			\
		(carry) >>= AR_IEEE64_C1_BITS;								\
		(sum).coeff0 = (carry) += (a).coeff0 + (b).coeff0;			\
		(carry) >>= AR_IEEE64_C0_BITS;								\
} while (0)

#define ADDIEEE128(sum,carry,a,b) do {								\
		(sum).coeff6 = (carry) += (a).coeff6 + (b).coeff6;			\
		(carry) >>= AR_IEEE128_C6_BITS; 							\
		(sum).coeff5 = (carry) += (a).coeff5 + (b).coeff5;			\
		(carry) >>= AR_IEEE128_C5_BITS; 							\
		(sum).coeff4 = (carry) += (a).coeff4 + (b).coeff4;			\
		(carry) >>= AR_IEEE128_C4_BITS; 							\
		(sum).coeff3 = (carry) += (a).coeff3 + (b).coeff3;			\
		(carry) >>= AR_IEEE128_C3_BITS; 							\
		(sum).coeff2 = (carry) += (a).coeff2 + (b).coeff2;			\
		(carry) >>= AR_IEEE128_C2_BITS; 							\
		(sum).coeff1 = (carry) += (a).coeff1 + (b).coeff1;			\
		(carry) >>= AR_IEEE128_C1_BITS; 							\
		(sum).coeff0 = (carry) += (a).coeff0 + (b).coeff0;			\
		(carry) >>= AR_IEEE128_C0_BITS; 							\
} while (0)


#define CPLX32_IMAG_TO_IEEE32(s, c) (								\
		(s).sign = (c).isign,										\
		(s).expo = (c).iexpo,										\
		(s).coeff0 = (c).icoeff0,									\
		(s).coeff1 = (c).icoeff1,									\
		(s).zero = 0 )

#define CPLX32_REAL_TO_IEEE32(s, c) (								\
		(s).sign = (c).rsign,										\
		(s).expo = (c).rexpo,										\
		(s).coeff0 = (c).rcoeff0,									\
		(s).coeff1 = (c).rcoeff1,									\
		(s).zero = 0 )

#define IEEE32_TO_CPLX32_REAL(c, s) (								\
		(c).rsign = (s).sign,										\
		(c).rexpo = (s).expo,										\
		(c).rcoeff0 = (s).coeff0,									\
		(c).rcoeff1 = (s).coeff1 )

#define IEEE32_TO_CPLX32_IMAG(c, s) (								\
		(c).isign = (s).sign,										\
		(c).iexpo = (s).expo,										\
		(c).icoeff0 = (s).coeff0,									\
		(c).icoeff1 = (s).coeff1 )

#define IEEE32TOINT64(i,f) (										\
		(i).part1 = (i).part2 = 0,									\
		(i).part3 = (f).sign << 15 | (f).expo << 7 | (f).coeff0,	\
		(i).part4 = (f).coeff1 )

#define IEEE64TOINT64(i,f) (										\
		(i).part1 = (f).sign << 15 | (f).expo << 4 | (f).coeff0,	\
		(i).part2 = (f).coeff1, 									\
		(i).part3 = (f).coeff2, 									\
		(i).part4 = (f).coeff3 )


#define INCIEEE32(sum,carry) do {									\
		(sum).coeff1 = (carry) += (sum).coeff1; 					\
		(carry) >>= AR_IEEE32_C1_BITS;								\
		(sum).coeff0 = (carry) += (sum).coeff0; 					\
		(carry) >>= AR_IEEE32_C0_BITS;								\
} while (0)

#define INCIEEE64(sum,carry) do {									\
		(sum).coeff3 = (carry) += (sum).coeff3; 					\
		(carry) >>= AR_IEEE64_C3_BITS;								\
		(sum).coeff2 = (carry) += (sum).coeff2; 					\
		(carry) >>= AR_IEEE64_C2_BITS;								\
		(sum).coeff1 = (carry) += (sum).coeff1; 					\
		(carry) >>= AR_IEEE64_C1_BITS;								\
		(sum).coeff0 = (carry) += (sum).coeff0; 					\
		(carry) >>= AR_IEEE64_C0_BITS;								\
} while (0)

#define INCIEEE128(sum,carry) do {									\
		(sum).coeff6 = (carry) += (sum).coeff6; 					\
		(carry) >>= AR_IEEE128_C6_BITS; 							\
		(sum).coeff5 = (carry) += (sum).coeff5; 					\
		(carry) >>= AR_IEEE128_C5_BITS; 							\
		(sum).coeff4 = (carry) += (sum).coeff4; 					\
		(carry) >>= AR_IEEE128_C4_BITS; 							\
		(sum).coeff3 = (carry) += (sum).coeff3; 					\
		(carry) >>= AR_IEEE128_C3_BITS; 							\
		(sum).coeff2 = (carry) += (sum).coeff2; 					\
		(carry) >>= AR_IEEE128_C2_BITS; 							\
		(sum).coeff1 = (carry) += (sum).coeff1; 					\
		(carry) >>= AR_IEEE128_C1_BITS; 							\
		(sum).coeff0 = (carry) += (sum).coeff0; 					\
		(carry) >>= AR_IEEE128_C0_BITS; 							\
} while (0)


#define INT64TOIEEE32(f,i) (										\
		(f).zero = 0,												\
		(f).sign = (i).part3 >> 15, 								\
		(f).expo = (i).part3 >> 7,									\
		(f).coeff0 = (i).part3, 									\
		(f).coeff1 = (i).part4 )

#define INT64TOIEEE64(f,i) (										\
		(f).sign = (i).part1 >> 15, 								\
		(f).expo = (i).part1 >> 4,									\
		(f).coeff0 = (i).part1, 									\
		(f).coeff1 = (i).part2, 									\
		(f).coeff2 = (i).part3, 									\
		(f).coeff3 = (i).part4 )

#define INT64TOIEEE128(f,i) (										\
		(f).sign = (i).part1 >> 15, 								\
		(f).expo = (i).part1,										\
		(f).coeff0 = (i).part2, 									\
		(f).coeff1 = (i).part3, 									\
		(f).coeff2 = (i).part4 )


#define NOTIEEE32(x) ( (x).coeff0 ^= MASKR (AR_IEEE32_C0_BITS),		\
					   (x).coeff1 ^= MASKR (AR_IEEE32_C1_BITS) )

#define NOTIEEE64(x) ( (x).coeff0 ^= MASKR (AR_IEEE64_C0_BITS),		\
					   (x).coeff1 ^= MASKR (AR_IEEE64_C1_BITS),		\
					   (x).coeff2 ^= MASKR (AR_IEEE64_C2_BITS),		\
					   (x).coeff3 ^= MASKR (AR_IEEE64_C3_BITS) )

#define NOTIEEE128(x) ( (x).coeff0 ^= MASKR (AR_IEEE128_C0_BITS),	\
						(x).coeff1 ^= MASKR (AR_IEEE128_C1_BITS), 	\
						(x).coeff2 ^= MASKR (AR_IEEE128_C2_BITS), 	\
						(x).coeff3 ^= MASKR (AR_IEEE128_C3_BITS), 	\
						(x).coeff4 ^= MASKR (AR_IEEE128_C4_BITS), 	\
						(x).coeff5 ^= MASKR (AR_IEEE128_C5_BITS), 	\
						(x).coeff6 ^= MASKR (AR_IEEE128_C6_BITS) )


#define SHLEFTIEEE32(x) do {										\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_IEEE32_C1_BITS - 1));		\
		(x).coeff1 <<= 1;											\
} while (0)

#define SHLEFTIEEE64(x) do {										\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_IEEE64_C1_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 << 1) |							\
					 ((x).coeff2 >> (AR_IEEE64_C2_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 << 1) |							\
					 ((x).coeff3 >> (AR_IEEE64_C3_BITS - 1));		\
		(x).coeff3 <<= 1;											\
} while (0)

#define SHLEFTIEEE128(x) do {										\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_IEEE128_C1_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 << 1) |							\
					 ((x).coeff2 >> (AR_IEEE128_C2_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 << 1) |							\
					 ((x).coeff3 >> (AR_IEEE128_C3_BITS - 1));		\
		(x).coeff3 = ((x).coeff3 << 1) |							\
					 ((x).coeff4 >> (AR_IEEE128_C4_BITS - 1));		\
		(x).coeff4 = ((x).coeff4 << 1) |							\
					 ((x).coeff5 >> (AR_IEEE128_C5_BITS - 1));		\
		(x).coeff5 = ((x).coeff5 << 1) |							\
					 ((x).coeff6 >> (AR_IEEE128_C6_BITS - 1));		\
		(x).coeff6 <<= 1;											\
} while (0)


#define SHLEFTIEEE32_2(x,y) do {									\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_IEEE32_C1_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 << 1) |							\
					 ((y).coeff0 >> (AR_IEEE32_C0_BITS - 1));		\
		(y).coeff0 = ((y).coeff0 << 1) |							\
					 ((y).coeff1 >> (AR_IEEE32_C1_BITS - 1));		\
		(y).coeff1 <<= 1;											\
} while (0)

#define SHLEFTIEEE64_2(x,y) do {									\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_IEEE64_C1_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 << 1) |							\
					 ((x).coeff2 >> (AR_IEEE64_C2_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 << 1) |							\
					 ((x).coeff3 >> (AR_IEEE64_C3_BITS - 1));		\
		(x).coeff3 = ((x).coeff3 << 1) |							\
					 ((y).coeff0 >> (AR_IEEE64_C0_BITS - 1));		\
		(y).coeff0 = ((y).coeff0 << 1) |							\
					 ((y).coeff1 >> (AR_IEEE64_C1_BITS - 1));		\
		(y).coeff1 = ((y).coeff1 << 1) |							\
					 ((y).coeff2 >> (AR_IEEE64_C2_BITS - 1));		\
		(y).coeff2 = ((y).coeff2 << 1) |							\
					 ((y).coeff3 >> (AR_IEEE64_C3_BITS - 1));		\
		(y).coeff3 <<= 1;											\
} while (0)

#define SHLEFTIEEE128_2(x,y) do {									\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_IEEE128_C1_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 << 1) |							\
					 ((x).coeff2 >> (AR_IEEE128_C2_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 << 1) |							\
					 ((x).coeff3 >> (AR_IEEE128_C3_BITS - 1));		\
		(x).coeff3 = ((x).coeff3 << 1) |							\
					 ((x).coeff4 >> (AR_IEEE128_C4_BITS - 1));		\
		(x).coeff4 = ((x).coeff4 << 1) |							\
					 ((x).coeff5 >> (AR_IEEE128_C5_BITS - 1));		\
		(x).coeff5 = ((x).coeff5 << 1) |							\
					 ((x).coeff6 >> (AR_IEEE128_C6_BITS - 1));		\
		(x).coeff6 = ((x).coeff6 << 1) |							\
					 ((y).coeff0 >> (AR_IEEE128_C0_BITS - 1));		\
		(y).coeff0 = ((y).coeff0 << 1) |							\
					 ((y).coeff1 >> (AR_IEEE128_C1_BITS - 1));		\
		(y).coeff1 = ((y).coeff1 << 1) |							\
					 ((y).coeff2 >> (AR_IEEE128_C2_BITS - 1));		\
		(y).coeff2 = ((y).coeff2 << 1) |							\
					 ((y).coeff3 >> (AR_IEEE128_C3_BITS - 1));		\
		(y).coeff3 = ((y).coeff3 << 1) |							\
					 ((y).coeff4 >> (AR_IEEE128_C4_BITS - 1));		\
		(y).coeff4 = ((y).coeff4 << 1) |							\
					 ((y).coeff5 >> (AR_IEEE128_C5_BITS - 1));		\
		(y).coeff5 = ((y).coeff5 << 1) |							\
					 ((y).coeff6 >> (AR_IEEE128_C6_BITS - 1));		\
		(y).coeff6 <<= 1;											\
} while (0)


#define SHRIGHTIEEE32(x) do {										\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_IEEE32_C1_BITS - 1));		\
		(x).coeff0 >>= 1;											\
} while (0)

#define SHRIGHTIEEE64(x) do {										\
		(x).coeff3 = ((x).coeff3 >> 1) |							\
					 ((x).coeff2 << (AR_IEEE64_C3_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 >> 1) |							\
					 ((x).coeff1 << (AR_IEEE64_C2_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_IEEE64_C1_BITS - 1));		\
		(x).coeff0 >>= 1;											\
} while (0)

#define SHRIGHTIEEE128(x) do {										\
		(x).coeff6 = ((x).coeff6 >> 1) |							\
					 ((x).coeff5 << (AR_IEEE128_C6_BITS - 1));		\
		(x).coeff5 = ((x).coeff5 >> 1) |							\
					 ((x).coeff4 << (AR_IEEE128_C5_BITS - 1));		\
		(x).coeff4 = ((x).coeff4 >> 1) |							\
					 ((x).coeff3 << (AR_IEEE128_C4_BITS - 1));		\
		(x).coeff3 = ((x).coeff3 >> 1) |							\
					 ((x).coeff2 << (AR_IEEE128_C3_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 >> 1) |							\
					 ((x).coeff1 << (AR_IEEE128_C2_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_IEEE128_C1_BITS - 1));		\
		(x).coeff0 >>= 1;											\
} while (0)


#define SHRIGHTIEEE32_2(x,y) do {									\
		(y).coeff1 = ((y).coeff1 >> 1) |							\
					 ((y).coeff0 << (AR_IEEE32_C1_BITS - 1));		\
		(y).coeff0 = ((y).coeff0 >> 1) |							\
					 ((x).coeff1 << (AR_IEEE32_C0_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_IEEE32_C1_BITS - 1));		\
		(x).coeff0 >>= 1;											\
} while (0)

#define SHRIGHTIEEE64_2(x,y) do {									\
		(y).coeff3 = ((y).coeff3 >> 1) |							\
					 ((y).coeff2 << (AR_IEEE64_C3_BITS - 1));		\
		(y).coeff2 = ((y).coeff2 >> 1) |							\
					 ((y).coeff1 << (AR_IEEE64_C2_BITS - 1));		\
		(y).coeff1 = ((y).coeff1 >> 1) |							\
					 ((y).coeff0 << (AR_IEEE64_C1_BITS - 1));		\
		(y).coeff0 = ((y).coeff0 >> 1) |							\
					 ((x).coeff3 << (AR_IEEE64_C0_BITS - 1));		\
		(x).coeff3 = ((x).coeff3 >> 1) |							\
					 ((x).coeff2 << (AR_IEEE64_C3_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 >> 1) |							\
					 ((x).coeff1 << (AR_IEEE64_C2_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_IEEE64_C1_BITS - 1));		\
		(x).coeff0 >>= 1;											\
} while (0)

#define SHRIGHTIEEE128_2(x,y) do {									\
		(y).coeff6 = ((y).coeff6 >> 1) |							\
					 ((y).coeff5 << (AR_IEEE128_C6_BITS - 1));		\
		(y).coeff5 = ((y).coeff5 >> 1) |							\
					 ((y).coeff4 << (AR_IEEE128_C5_BITS - 1));		\
		(y).coeff4 = ((y).coeff4 >> 1) |							\
					 ((y).coeff3 << (AR_IEEE128_C4_BITS - 1));		\
		(y).coeff3 = ((y).coeff3 >> 1) |							\
					 ((y).coeff2 << (AR_IEEE128_C3_BITS - 1));		\
		(y).coeff2 = ((y).coeff2 >> 1) |							\
					 ((y).coeff1 << (AR_IEEE128_C2_BITS - 1));		\
		(y).coeff1 = ((y).coeff1 >> 1) |							\
					 ((y).coeff0 << (AR_IEEE128_C1_BITS - 1));		\
		(y).coeff0 = ((y).coeff0 >> 1) |							\
					 ((x).coeff6 << (AR_IEEE128_C0_BITS - 1));		\
		(x).coeff6 = ((x).coeff6 >> 1) |							\
					 ((x).coeff5 << (AR_IEEE128_C6_BITS - 1));		\
		(x).coeff5 = ((x).coeff5 >> 1) |							\
					 ((x).coeff4 << (AR_IEEE128_C5_BITS - 1));		\
		(x).coeff4 = ((x).coeff4 >> 1) |							\
					 ((x).coeff3 << (AR_IEEE128_C4_BITS - 1));		\
		(x).coeff3 = ((x).coeff3 >> 1) |							\
					 ((x).coeff2 << (AR_IEEE128_C3_BITS - 1));		\
		(x).coeff2 = ((x).coeff2 >> 1) |							\
					 ((x).coeff1 << (AR_IEEE128_C2_BITS - 1));		\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_IEEE128_C1_BITS - 1));		\
		(x).coeff0 >>= 1;											\
} while (0)


#define ZEROIEEE32(x) ((x).zero = (x).sign = (x).expo = (x).coeff0 =\
					   (x).coeff1 = 0)

#define ZEROIEEE64(x) ((x).sign = (x).expo = (x).coeff0 =			\
					   (x).coeff1 = (x).coeff2 = (x).coeff3 = 0)

#define ZEROIEEE128(x) ((x).sign = (x).expo = (x).coeff0 =			\
						(x).coeff1 = (x).coeff2 = (x).coeff3 =		\
						(x).coeff4 = (x).coeff5 = (x).coeff6 = 0)

#define ZEROIEEE128M(x) ((x).sign = (x).expo = (x).coeff0 =				\
						 (x).coeff1 = (x).coeff2 = (x).coeff3 =			\
						 (x).signl = (x).expol = (x).coeff0l =			\
						 (x).coeff1l = (x).coeff2l = (x).coeff3l = 0)


#define QNaNIEEE32(x) do {										\
		ZEROIEEE32(*x);											\
		NOTIEEE32(*x);											\
		if (HOST_IS_MIPS) {										\
			(x)->coeff0 ^= (1 << (AR_IEEE32_C0_BITS - 1));		\
		}														\
		(x)->expo = AR_IEEE32_MAX_EXPO + 1;						\
} while (0)

#define QNaNIEEE64(x) do {										\
		ZEROIEEE64(*x);											\
		NOTIEEE64(*x);											\
		if (HOST_IS_MIPS) {										\
			(x)->coeff0 ^= (1 << (AR_IEEE64_C0_BITS - 1));		\
		}														\
		(x)->expo = AR_IEEE64_MAX_EXPO + 1;						\
} while (0)

#define QNaNIEEE128(x) do {													\
		if (HOST_IS_MIPS) {													\
			ZEROIEEE128M(*((AR_MIPS_128 *) x));								\
			((AR_MIPS_128 *) x)->coeff0 = MASKR (AR_IEEE64_C0_BITS - 1);	\
			((AR_MIPS_128 *) x)->coeff1 = MASKR (AR_IEEE64_C1_BITS    );	\
			((AR_MIPS_128 *) x)->coeff2 = MASKR (AR_IEEE64_C2_BITS    );	\
			((AR_MIPS_128 *) x)->coeff3 = MASKR (AR_IEEE64_C3_BITS    );	\
			((AR_MIPS_128 *) x)->expo = AR_IEEE64_MAX_EXPO + 1;				\
		}																	\
		else {																\
			ZEROIEEE128(*x);												\
			NOTIEEE128(*x);													\
			(x)->expo = AR_IEEE128_MAX_EXPO + 1;							\
		}																	\
} while (0)


/* Macros supporting Cray floating point algorithms */

#define ADDCRAY64(sum,carry,a,b) do {								\
		(sum).coeff2 = (carry) += (a).coeff2 + (b).coeff2;			\
		(carry) >>= AR_CRAY_C2_BITS;								\
		(sum).coeff1 = (carry) += (a).coeff1 + (b).coeff1;			\
		(carry) >>= AR_CRAY_C1_BITS;								\
		(sum).coeff0 = (carry) += (a).coeff0 + (b).coeff0;			\
		(carry) >>= AR_CRAY_C0_BITS;								\
} while (0)

#define ADDCRAY128(sum,carry,a,b) do {								\
		(sum).coeff5 = (carry) += (a).coeff5 + (b).coeff5;			\
		(carry) >>= AR_CRAY_C5_BITS;								\
		(sum).coeff4 = (carry) += (a).coeff4 + (b).coeff4;			\
		(carry) >>= AR_CRAY_C4_BITS;								\
		(sum).coeff3 = (carry) += (a).coeff3 + (b).coeff3;			\
		(carry) >>= AR_CRAY_C3_BITS;								\
		(sum).coeff2 = (carry) += (a).coeff2 + (b).coeff2;			\
		(carry) >>= AR_CRAY_C2_BITS;								\
		(sum).coeff1 = (carry) += (a).coeff1 + (b).coeff1;			\
		(carry) >>= AR_CRAY_C1_BITS;								\
		(sum).coeff0 = (carry) += (a).coeff0 + (b).coeff0;			\
		(carry) >>= AR_CRAY_C0_BITS;								\
} while (0)

#define CRAY64TO128(d,s) (											\
		(d).sign = (s).sign,										\
		(d).expo = (s).expo,										\
		(d).coeff0 = (s).coeff0,									\
		(d).coeff1 = (s).coeff1,									\
		(d).coeff2 = (s).coeff2,									\
		(d).zero = (d).coeff3 = (d).coeff4 = (d).coeff5 = 0 )

#define CRAY128TO64(s,d) (											\
		(s).sign = (d).sign,										\
		(s).expo = (d).expo,										\
		(s).coeff0 = (d).coeff0,									\
		(s).coeff1 = (d).coeff1,									\
		(s).coeff2 = (d).coeff2 )

#define CRAY64TOINT64(i,c) (										\
		(i).part1 = (c).sign << 15 | (c).expo,						\
		(i).part2 = (c).coeff0, 									\
		(i).part3 = (c).coeff1, 									\
		(i).part4 = (c).coeff2 )

#define INCCRAY64(sum,carry) do {									\
		(sum).coeff2 = (carry) += (sum).coeff2; 					\
		(carry) >>= AR_CRAY_C2_BITS;								\
		(sum).coeff1 = (carry) += (sum).coeff1; 					\
		(carry) >>= AR_CRAY_C1_BITS;								\
		(sum).coeff0 = (carry) += (sum).coeff0; 					\
		(carry) >>= AR_CRAY_C0_BITS;								\
} while (0)

#define INCCRAY128(sum,carry) do {									\
		(sum).coeff5 = (carry) += (sum).coeff5; 					\
		(carry) >>= AR_CRAY_C5_BITS;								\
		(sum).coeff4 = (carry) += (sum).coeff4; 					\
		(carry) >>= AR_CRAY_C4_BITS;								\
		(sum).coeff3 = (carry) += (sum).coeff3; 					\
		(carry) >>= AR_CRAY_C3_BITS;								\
		(sum).coeff2 = (carry) += (sum).coeff2; 					\
		(carry) >>= AR_CRAY_C2_BITS;								\
		(sum).coeff1 = (carry) += (sum).coeff1; 					\
		(carry) >>= AR_CRAY_C1_BITS;								\
		(sum).coeff0 = (carry) += (sum).coeff0; 					\
		(carry) >>= AR_CRAY_C0_BITS;								\
} while (0)

#define INT64TOCRAY64(c,i) (										\
		(c).sign = (i).part1 >> 15, 								\
		(c).expo = (i).part1,										\
		(c).coeff0 = (i).part2, 									\
		(c).coeff1 = (i).part3, 									\
		(c).coeff2 = (i).part4 )

#define NOTCRAY64(x) ( (x).coeff0 ^= MASKR (AR_CRAY_C0_BITS),		\
					   (x).coeff1 ^= MASKR (AR_CRAY_C1_BITS),		\
					   (x).coeff2 ^= MASKR (AR_CRAY_C2_BITS) )

#define NOTCRAY128(x) ( (x).coeff0 ^= MASKR (AR_CRAY_C0_BITS),		\
						(x).coeff1 ^= MASKR (AR_CRAY_C1_BITS),		\
						(x).coeff2 ^= MASKR (AR_CRAY_C2_BITS),		\
						(x).coeff3 ^= MASKR (AR_CRAY_C3_BITS),		\
						(x).coeff4 ^= MASKR (AR_CRAY_C4_BITS),		\
						(x).coeff5 ^= MASKR (AR_CRAY_C5_BITS) )

#if _CRAY
#define SHLEFTCRAY64(x) 											\
		(*((long*)&(x)) =											\
				(*((long*)&(x))&~MASKR(AR_CRAY64_COEFF_BITS)) |		\
				((*((long*)&(x))<<1)&MASKR(AR_CRAY64_COEFF_BITS)))
#else
#define SHLEFTCRAY64(x) do {										\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_CRAY_C1_BITS - 1)); 		\
		(x).coeff1 = ((x).coeff1 << 1) |							\
					 ((x).coeff2 >> (AR_CRAY_C2_BITS - 1)); 		\
		(x).coeff2 <<= 1;											\
} while (0)
#endif

#define SHLEFTCRAY128(x) do {										\
		(x).coeff0 = ((x).coeff0 << 1) |							\
					 ((x).coeff1 >> (AR_CRAY_C1_BITS - 1)); 		\
		(x).coeff1 = ((x).coeff1 << 1) |							\
					 ((x).coeff2 >> (AR_CRAY_C2_BITS - 1)); 		\
		(x).coeff2 = ((x).coeff2 << 1) |							\
					 ((x).coeff3 >> (AR_CRAY_C3_BITS - 1)); 		\
		(x).coeff3 = ((x).coeff3 << 1) |							\
					 ((x).coeff4 >> (AR_CRAY_C4_BITS - 1)); 		\
		(x).coeff4 = ((x).coeff4 << 1) |							\
					 ((x).coeff5 >> (AR_CRAY_C5_BITS - 1)); 		\
		(x).coeff5 <<= 1;											\
} while (0)

#if _CRAY
#define SHRIGHTCRAY64(x)											\
		(*((long*)&(x)) =											\
				(*((long*)&(x))&~MASKR(AR_CRAY64_COEFF_BITS)) |		\
				((*((long*)&(x))&MASKR(AR_CRAY64_COEFF_BITS))>>1))
#else
#define SHRIGHTCRAY64(x) do {										\
		(x).coeff2 = ((x).coeff2 >> 1) |							\
					 ((x).coeff1 << (AR_CRAY_C2_BITS - 1)); 		\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_CRAY_C1_BITS - 1)); 		\
		(x).coeff0 >>= 1;											\
} while (0)
#endif

#define SHRIGHTCRAY128(x) do {										\
		(x).coeff5 = ((x).coeff5 >> 1) |							\
					 ((x).coeff4 << (AR_CRAY_C5_BITS - 1)); 		\
		(x).coeff4 = ((x).coeff4 >> 1) |							\
					 ((x).coeff3 << (AR_CRAY_C4_BITS - 1)); 		\
		(x).coeff3 = ((x).coeff3 >> 1) |							\
					 ((x).coeff2 << (AR_CRAY_C3_BITS - 1)); 		\
		(x).coeff2 = ((x).coeff2 >> 1) |							\
					 ((x).coeff1 << (AR_CRAY_C2_BITS - 1)); 		\
		(x).coeff1 = ((x).coeff1 >> 1) |							\
					 ((x).coeff0 << (AR_CRAY_C1_BITS - 1)); 		\
		(x).coeff0 >>= 1;											\
} while (0)

#define ZEROCRAY64(x)													\
		((x).sign = (x).expo = (x).coeff0 = (x).coeff1 = (x).coeff2 = 0)

#define ZEROCRAY128(x)													\
		((x).sign = (x).expo = (x).coeff0 = (x).coeff1 = (x).coeff2 =	\
					(x).zero = (x).coeff3 = (x).coeff4 = (x).coeff5 = 0)


#endif	/* AR_INTERNAL_H */
