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


#ifndef AR_H
#define AR_H

/* Integer type declarations */

enum ar_int_size
{
	ar_int_size_8,
	ar_int_size_24,
	ar_int_size_32,
	ar_int_size_46,
	ar_int_size_64,
	ar_int_size_128,
	ar_int_size_16
};

#define AR_INT_SIZE_8				(ar_int_size_8 << 2)
#define AR_INT_SIZE_24				(ar_int_size_24 << 2)
#define AR_INT_SIZE_32				(ar_int_size_32 << 2)
#define AR_INT_SIZE_46				(ar_int_size_46 << 2)
#define AR_INT_SIZE_64				(ar_int_size_64 << 2)
#define AR_INT_SIZE_128 			(ar_int_size_128 << 2)
#define AR_INT_SIZE_16				(ar_int_size_16 << 2)

enum ar_pointer_format
{
	ar_pointer_byte,		/* Cray T3D, etc. byte pointers */
	ar_pointer_char,		/* Cray Y-MP, etc. character/void pointers */
	ar_pointer_fctn,
	ar_pointer_word
};

#define AR_POINTER_BYTE 			(ar_pointer_byte << 2)
#define AR_POINTER_CHAR 			(ar_pointer_char << 2)
#define AR_POINTER_FCTN 			(ar_pointer_fctn << 2)
#define AR_POINTER_WORD 			(ar_pointer_word << 2)

enum ar_pointer_size
{
	ar_pointer_size_24,
	ar_pointer_size_32,
	ar_pointer_size_64
};

#define AR_POINTER_24				ar_pointer_size_24
#define AR_POINTER_32				ar_pointer_size_32
#define AR_POINTER_64				ar_pointer_size_64

enum ar_signedness
{
	ar_signed,
	ar_unsigned
};

#define AR_SIGNED					ar_signed
#define AR_UNSIGNED 				ar_unsigned

#ifdef _CRAY
	typedef long					AR_HOST_SINT64;
	typedef unsigned long			AR_HOST_UINT64;
#else
	typedef long long				AR_HOST_SINT64;
	typedef unsigned long long		AR_HOST_UINT64;
#endif


/* Floating point type declarations */

enum ar_float_format
{
	ar_float_format_ieee,
	ar_float_format_cray
};

#define AR_FLOAT_IEEE				(ar_float_format_ieee << 5)
#define AR_FLOAT_CRAY				(ar_float_format_cray << 5)

#define AR_IEEE_FLOATING_POINT		ar_float_format_ieee
#define AR_CRAY_FLOATING_POINT		ar_float_format_cray

enum ar_float_size
{
	ar_float_32,
	ar_float_64,
	ar_float_128
};
#define AR_FLOAT_32 				(ar_float_32  << 1)
#define AR_FLOAT_64 				(ar_float_64  << 1)
#define AR_FLOAT_128				(ar_float_128 << 1)


enum ar_float_complexity
{
	ar_simple,
	ar_complex
};
#define AR_FLOAT_SIMPLE				ar_simple
#define AR_FLOAT_COMPLEX			ar_complex

enum ar_float_ieee_round
{
	ar_float_ieee_round_nearest,
	ar_float_ieee_round_up,
	ar_float_ieee_round_down,
	ar_float_ieee_round_zero
};

#define AR_FLOAT_IEEE_ROUND_NEAREST (ar_float_ieee_round_nearest << 3)
#define AR_FLOAT_IEEE_ROUND_ZERO	(ar_float_ieee_round_zero	 << 3)
#define AR_FLOAT_IEEE_ROUND_UP		(ar_float_ieee_round_up 	 << 3)
#define AR_FLOAT_IEEE_ROUND_DOWN	(ar_float_ieee_round_down	 << 3)

#define AR_ROUND_NEAREST			ar_float_ieee_round_nearest
#define AR_ROUND_ZERO				ar_float_ieee_round_zero
#define AR_ROUND_PLUS_INFINITY		ar_float_ieee_round_up
#define AR_ROUND_MINUS_INFINITY		ar_float_ieee_round_down

enum ar_float_cray_round
{
	ar_float_cray_round_c1,
	ar_reciprocal_iteration,
	ar_float_cray_round_c1f
};

#define AR_FLOAT_CRAY_ROUND_C1		(ar_float_cray_round_c1 	 << 3)
#define AR_FLOAT_CRAY_ROUND_C1F 	(ar_float_cray_round_c1f	 << 3)

#define AR_ROUNDED					ar_float_cray_round_c1
#define AR_UNROUNDED				ar_float_cray_round_c1f
#define AR_RECIPROCAL_ITERATION		ar_reciprocal_iteration

enum ar_float_underflow
{
	ar_underflow_to_denorm,
	ar_underflow_to_plus_zero,
	ar_underflow_to_signed_zero,
	ar_underflow_to_signed_tiny
};

#define AR_UNDERFLOW_TO_DENORM		ar_underflow_to_denorm
#define AR_UNDERFLOW_TO_PLUS_ZERO	ar_underflow_to_plus_zero
#define AR_UNDERFLOW_TO_SIGNED_ZERO	ar_underflow_to_signed_zero
#define AR_UNDERFLOW_TO_SIGNED_TINY	ar_underflow_to_signed_tiny

enum ar_float_128_bit
{
	ar_128_bit_extended_double,
	ar_128_bit_double_double
};

#define AR_128BIT_EXTENDED_DOUBLE	ar_128_bit_extended_double
#define AR_128BIT_DOUBLE_DOUBLE		ar_128_bit_double_double


/* Generic type declarations */

typedef struct {
	AR_HOST_SINT64	ar_internal_data_item1;
	AR_HOST_SINT64	ar_internal_data_item2;
	AR_HOST_SINT64	ar_internal_data_item3;
	AR_HOST_SINT64	ar_internal_data_item4;
} AR_DATA;

enum ar_class
{
	ar_class_int,
	ar_class_float,
	ar_class_pointer,
	ar_class_logical,
	ar_class_error
};

#define AR_CLASS_INT				(ar_class_int	  << 6)
#define AR_CLASS_FLOAT				(ar_class_float   << 6)
#define AR_CLASS_POINTER			(ar_class_pointer << 6)
#define AR_CLASS_LOGICAL			(ar_class_logical << 6)
#define AR_CLASS_ERROR				(ar_class_error   << 6)

	/******************************  NOTICE  ****************************/
	/*                                                                  */
	/*   THE FOLLOWING MACROS WILL BE ELIMINATED IN THE NEAR FUTURE     */
	/*                                                                  */
	/********************************************************************/

#ifndef AR_INTERNAL_H
#define AR_CLASS(type)				(type & 0x1c0)

#define AR_INT_SIZE(type)			(type & 0x3c)

#define AR_SIGNEDNESS(type) 		(type & 0x1)
#endif

typedef enum
{
	AR_Int_8_S				= AR_CLASS_INT | AR_INT_SIZE_8	 | AR_SIGNED,
	AR_Int_8_U				= AR_CLASS_INT | AR_INT_SIZE_8	 | AR_UNSIGNED,
	AR_Int_16_S 			= AR_CLASS_INT | AR_INT_SIZE_16  | AR_SIGNED,
	AR_Int_16_U 			= AR_CLASS_INT | AR_INT_SIZE_16  | AR_UNSIGNED,
	AR_Int_24_S 			= AR_CLASS_INT | AR_INT_SIZE_24  | AR_SIGNED,
	AR_Int_24_U 			= AR_CLASS_INT | AR_INT_SIZE_24  | AR_UNSIGNED,
	AR_Int_32_S 			= AR_CLASS_INT | AR_INT_SIZE_32  | AR_SIGNED,
	AR_Int_32_U 			= AR_CLASS_INT | AR_INT_SIZE_32  | AR_UNSIGNED,
	AR_Int_46_S 			= AR_CLASS_INT | AR_INT_SIZE_46  | AR_SIGNED,
	AR_Int_46_U 			= AR_CLASS_INT | AR_INT_SIZE_46  | AR_UNSIGNED,
	AR_Int_64_S 			= AR_CLASS_INT | AR_INT_SIZE_64  | AR_SIGNED,
	AR_Int_64_U 			= AR_CLASS_INT | AR_INT_SIZE_64  | AR_UNSIGNED,
	AR_Int_128_S			= AR_CLASS_INT | AR_INT_SIZE_128 | AR_SIGNED,
	AR_Int_128_U			= AR_CLASS_INT | AR_INT_SIZE_128 | AR_UNSIGNED,

	AR_Logical				= AR_CLASS_LOGICAL,

	AR_Pointer_Byte 		= AR_CLASS_POINTER | AR_POINTER_BYTE,
	AR_Pointer_Char_24		= AR_CLASS_POINTER | AR_POINTER_CHAR | AR_POINTER_24,
	AR_Pointer_Char_32		= AR_CLASS_POINTER | AR_POINTER_CHAR | AR_POINTER_32,
	AR_Pointer_Char_64		= AR_CLASS_POINTER | AR_POINTER_CHAR | AR_POINTER_64,
	AR_Pointer_Fctn_24		= AR_CLASS_POINTER | AR_POINTER_FCTN | AR_POINTER_24,
	AR_Pointer_Fctn_32		= AR_CLASS_POINTER | AR_POINTER_FCTN | AR_POINTER_32,
	AR_Pointer_Fctn_64		= AR_CLASS_POINTER | AR_POINTER_FCTN | AR_POINTER_64,
	AR_Pointer_Word_24		= AR_CLASS_POINTER | AR_POINTER_WORD | AR_POINTER_24,
	AR_Pointer_Word_32		= AR_CLASS_POINTER | AR_POINTER_WORD | AR_POINTER_32,
	AR_Pointer_Word_64		= AR_CLASS_POINTER | AR_POINTER_WORD | AR_POINTER_64,

	AR_Float_32				= AR_CLASS_FLOAT | AR_FLOAT_32 | AR_FLOAT_SIMPLE,
	AR_Float_64				= AR_CLASS_FLOAT | AR_FLOAT_64 | AR_FLOAT_SIMPLE,
	AR_Float_128			= AR_CLASS_FLOAT | AR_FLOAT_128 | AR_FLOAT_SIMPLE,
	AR_Complex_32			= AR_CLASS_FLOAT | AR_FLOAT_32 | AR_FLOAT_COMPLEX,
	AR_Complex_64			= AR_CLASS_FLOAT | AR_FLOAT_64 | AR_FLOAT_COMPLEX,
	AR_Complex_128			= AR_CLASS_FLOAT | AR_FLOAT_128 | AR_FLOAT_COMPLEX,

	/******************************  NOTICE  ****************************/
	/*                                                                  */
	/*   THE FOLLOWING FLOATING POINT TYPES WILL BE ELIMINATED IN THE   */
	/*   3.0 VERSION OF ARITH.  SEE arith.doc MORE INFORMATION.         */
	/*                                                                  */
	/********************************************************************/

	AR_Float_Cray1_64		= AR_CLASS_FLOAT | AR_FLOAT_CRAY |
							  AR_FLOAT_CRAY_ROUND_C1 | AR_FLOAT_64 |
							  AR_FLOAT_SIMPLE,

	AR_Float_Cray1_64_F 	= AR_CLASS_FLOAT | AR_FLOAT_CRAY |
							  AR_FLOAT_CRAY_ROUND_C1F | AR_FLOAT_64 |
							  AR_FLOAT_SIMPLE,

	AR_Float_Cray1_128		= AR_CLASS_FLOAT | AR_FLOAT_CRAY |
							  AR_FLOAT_CRAY_ROUND_C1 | AR_FLOAT_128 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_NR_32 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_NEAREST | AR_FLOAT_32 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_NR_64 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_NEAREST | AR_FLOAT_64 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_NR_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_NEAREST | AR_FLOAT_128 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_ZE_32 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_ZERO | AR_FLOAT_32 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_ZE_64 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_ZERO | AR_FLOAT_64 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_ZE_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_ZERO | AR_FLOAT_128 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_UP_32 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_UP | AR_FLOAT_32 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_UP_64 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_UP | AR_FLOAT_64 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_UP_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_UP | AR_FLOAT_128 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_DN_32 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_DOWN | AR_FLOAT_32 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_DN_64 	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_DOWN | AR_FLOAT_64 |
							  AR_FLOAT_SIMPLE,

	AR_Float_IEEE_DN_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_DOWN | AR_FLOAT_128 |
							  AR_FLOAT_SIMPLE,

	AR_Complex_Cray1_64 	= AR_CLASS_FLOAT | AR_FLOAT_CRAY |
							  AR_FLOAT_CRAY_ROUND_C1 | AR_FLOAT_64 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_Cray1_64_F	= AR_CLASS_FLOAT | AR_FLOAT_CRAY |
							  AR_FLOAT_CRAY_ROUND_C1F | AR_FLOAT_64 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_Cray1_128	= AR_CLASS_FLOAT | AR_FLOAT_CRAY |
							  AR_FLOAT_CRAY_ROUND_C1 | AR_FLOAT_128 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_NR_32	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_NEAREST | AR_FLOAT_32 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_NR_64	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_NEAREST | AR_FLOAT_64 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_NR_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_NEAREST | AR_FLOAT_128 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_ZE_32	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_ZERO | AR_FLOAT_32 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_ZE_64	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_ZERO | AR_FLOAT_64 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_ZE_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_ZERO | AR_FLOAT_128 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_UP_32	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_UP | AR_FLOAT_32 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_UP_64	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_UP | AR_FLOAT_64 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_UP_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_UP | AR_FLOAT_128 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_DN_32	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_DOWN | AR_FLOAT_32 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_DN_64	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_DOWN | AR_FLOAT_64 |
							  AR_FLOAT_COMPLEX,

	AR_Complex_IEEE_DN_128	= AR_CLASS_FLOAT | AR_FLOAT_IEEE |
							  AR_FLOAT_IEEE_ROUND_DOWN | AR_FLOAT_128 |
							  AR_FLOAT_COMPLEX,

	AR_Error				= AR_CLASS_ERROR

} AR_TYPE;


/* Return values from AR_ functions */

#define AR_STAT_OK				0000
#define AR_STAT_OVERFLOW		0001	/* error condition */
#define AR_STAT_UNDERFLOW		0002	/* FYI; often also zero */
#define AR_STAT_UNDEFINED		0004	/* possible error: IEEE NaN result
											 or undefined operation */
#define AR_STAT_INEXACT 		0010	/* FYI: precision lost */
#define AR_STAT_ZERO			0020	/* FYI: result is zero */
#define AR_STAT_NEGATIVE		0040	/* FYI: result sign bit set */
#define AR_STAT_INVALID_TYPE	0100	/* invalid type for operation */
#define AR_STAT_SEMIVALID		0200	/* FYI: result is algorithmically
												  precise but incorrect */

#define AR_ZERO 				0020	/* FYI: result is zero */
#define AR_NEGATIVE 			0040	/* FYI: result sign bit set */
#define AR_ERROR_STATUS 		0105	/* Generally fatal error flags */


/* Return values for function AR_compare */

typedef enum
{
	AR_Compare_LT,				/* opnd1 less than opnd2 */
	AR_Compare_EQ,				/* opnd1 greater than opnd2 */
	AR_Compare_GT,				/* opnd1 equal to opnd2 */
	AR_Compare_Unord,			/* comparison unordered */
	AR_Compare_NE,				/* opnd1 not equal to opnd2 (complex only) */
	AR_Compare_Invalid			/* compares can't be done on operand type */
} AR_COMPARE_TYPE;


/*********************** AR_ function declarations ************************/

/* Basic arithmetic and logical functions */

int AR_abs	   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_add	   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_add_ptr_int(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type,
				const AR_DATA *opnd3,
				const AR_TYPE *opnd3type);

int AR_bitor   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_bitand  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_bitxor  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_bitcomplement(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_cimag   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

AR_COMPARE_TYPE AR_compare(const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_conj    (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_creal   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_divide  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_dshiftl (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type,
				const AR_DATA *opnd3,
				const AR_TYPE *opnd3type);

int AR_dshiftr (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type,
				const AR_DATA *opnd3,
				const AR_TYPE *opnd3type);

int AR_mask    (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_leadz   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_make_complex(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_make_imag(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_mod	   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_multiply(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_negate  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_one	   (AR_DATA *result,
				const AR_TYPE *type);

int AR_popcnt  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_poppar  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_round_int_div (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd,
				const AR_TYPE *opndtype);

int AR_shiftl  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_shiftr  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_ishft   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_ishftc  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type,
				const AR_DATA *opnd3,
				const AR_TYPE *opnd3type);

int AR_ibits   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type,
				const AR_DATA *opnd3,
				const AR_TYPE *opnd3type);

int AR_status  (const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_subtract(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_subtract_ptr_ptr(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type,
				const AR_DATA *opnd3,
				const AR_TYPE *opnd3type);


/* Miscellaneous support Functions */

int AR_CRAY_64_trunc_bits(int truncbits);

const char *arith_vers_name(void);

const char *arith_vers_ID(void);

const char *arith_vers_number(void);


/* Conversion functions (between types) */

int AR_convert (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_convert_float_to_str(char *resultstr,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_convert_hex_str_to_float(AR_DATA *result,
				const AR_TYPE *resulttype,
				const char *ar_str);

int AR_convert_host_sint64_to_int(AR_DATA *result,
				const AR_TYPE *resulttype,
				AR_HOST_SINT64 i64val);

int AR_convert_int_to_host_sint64(AR_HOST_SINT64 *i64val,
				const AR_DATA *opnd,
				const AR_TYPE *opndtype);

int AR_convert_int_to_str(char *resultstr,
				const int *base,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_convert_str_to_int(AR_DATA *result,
				const AR_TYPE *resulttype,
				int *bits_used,
				const char *str,
				const int *base);

int AR_convert_str_to_float(AR_DATA *result,
				const AR_TYPE *resulttype,
				const char *str);


/* Mathematical (libm) intrinsic functions */

int AR_cabs    (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_exp	   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_Modulo  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_modulo  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_log	   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type);

int AR_power   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_sqrt    (AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd,
				const AR_TYPE *opndtype);


/* Language-specific (e.g., Fortran-90) intrinsic functions */

int AR_index   (AR_DATA *result,
				const AR_TYPE *resulttype,
				const char* str1,
				const AR_DATA *str1len,
				const AR_TYPE *str1lentype,
				const char* str2,
				const AR_DATA *str2len,
				const AR_TYPE *str2lentype,
				const AR_DATA *backward,
				const AR_TYPE *backwardtype);

int AR_reshape (void *result,
				const void *source,
				const void *shape,
				const void *pad,
				const void *order);

int AR_scan    (AR_DATA *result,
				const AR_TYPE *resulttype,
				const char* str1,
				const AR_DATA *str1len,
				const AR_TYPE *str1lentype,
				const char* str2,
				const AR_DATA *str2len,
				const AR_TYPE *str2lentype,
				const AR_DATA *backward,
				const AR_TYPE *backwardtype);

int AR_selected_real_kind(AR_DATA *result,
				const AR_TYPE *resulttype,
				const AR_DATA *opnd1,
				const AR_TYPE *opnd1type,
				const AR_DATA *opnd2,
				const AR_TYPE *opnd2type);

int AR_transfer(void *result,
				const void *source,
				const void *mold,
				const AR_DATA *size,
				const AR_TYPE *sizetype);

int AR_verify  (AR_DATA *result,
				const AR_TYPE *resulttype,
				const char* str1,
				const AR_DATA *str1len,
				const AR_TYPE *str1lentype,
				const char* str2,
				const AR_DATA *str2len,
				const AR_TYPE *str2lentype,
				const AR_DATA *backward,
				const AR_TYPE *backwardtype);


/* Special global constants */

extern AR_DATA AR_const_zero;
extern AR_DATA AR_const_one;
extern AR_DATA AR_const_two;

extern AR_DATA AR_const_false;
extern AR_DATA AR_const_true;

#endif	/* AR_H */

