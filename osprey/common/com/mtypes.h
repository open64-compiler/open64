/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


#ifndef mtypes_INCLUDED
#define mtypes_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: mtypes.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/mtypes.h,v $
 *
 * Revision history:
 *  11-Oct-89 - Original Version
 *
 * Description:
 *
 * Define IDs for the types supported by the target machine.  Not all
 * of the predefined types will be supported on a given machine.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *mtypes_rcs_id = "$Source: common/com/SCCS/s.mtypes.h $ $Revision: 1.16 $";
#endif /* _KEEP_RCS_ID */

/* The predefined machine data types, present on many machines: */
enum MTYPE_t {
    MTYPE_UNKNOWN=0,
    MTYPE_FIRST=1,
    MTYPE_B=1,           /* BOOL */
    MTYPE_I1=2,          /*   8-bit integer */
    MTYPE_I2=3,          /*  16-bit integer */
    MTYPE_I4=4,          /*  32-bit integer */
    MTYPE_I8=5,          /*  64-bit integer */
    MTYPE_U1=6,          /*   8-bit unsigned integer */
    MTYPE_U2=7,          /*  16-bit unsigned integer */
    MTYPE_U4=8,          /*  32-bit unsigned integer */
    MTYPE_U8=9,          /*  64-bit unsigned integer */
    MTYPE_F4=10,         /*  32-bit IEEE floating point */
    MTYPE_F8=11,         /*  64-bit IEEE floating point */
    MTYPE_F10=12,        /*  80-bit IEEE floating point */
    MTYPE_F16=13,        /* 128-bit IEEE floating point */

    MTYPE_STR=14,        /* char strings - TCONs only */
    MTYPE_STRING=MTYPE_STR,
    MTYPE_FQ=15,         /* for SGI long double */
    MTYPE_M=16,          /* memory chunk, for structures */
    MTYPE_C4=17,         /* for 32-bit complex */
    MTYPE_C8=18,         /* for 64-bit complex */
    MTYPE_CQ=19,         /* for quad complex */
    MTYPE_V=20,          /* for void type */

    MTYPE_BS=21,         /* Bits */
    MTYPE_A4=22,         /* 32-bit address */
    MTYPE_A8=23,         /* 64-bit address */
    MTYPE_C10=24,        /*  80-bit IEEE floating point complex */
    MTYPE_C16=25,        /* 128-bit IEEE floating point complex */
    MTYPE_I16=26,        /* 128-bit signed integer              */
    MTYPE_U16=27,        /* 128-bit unsigned integer            */

#ifdef TARG_X8664
    MTYPE_V16C4=28,       /* vector type for C4 */
    MTYPE_V16C8=29,       /* vector type for C8 */
    MTYPE_V16I1=30,       /* 128-bit vector of signed bytes            */
    MTYPE_V16I2=31,       /* 128-bit vector of signed short ints       */
    MTYPE_V16I4=32,       /* 128-bit vector of signed ints             */
    MTYPE_V16I8=33,       /* 128-bit vector of signed long long ints   */
    MTYPE_V16F4=34,       /* 128-bit vector of signed floats           */
    MTYPE_V16F8=35,       /* 128-bit vector of signed doubles          */
 
    MTYPE_V8I1=36,        /* 64-bit vector of signed bytes */
    MTYPE_V8I2=37,        /* 64-bit vector of signed short ints */
    MTYPE_V8I4=38,        /* 64-bit vector of signed ints */
    MTYPE_V8I8=39,        /* 64-bit vector of signed ints */
    MTYPE_V8F4=40,        /* 64-bit vector of signed floats */

    MTYPE_M8I1=41,        /* 64-bit MMX vector of signed bytes */
    MTYPE_M8I2=42,        /* 64-bit MMX vector of signed short ints */
    MTYPE_M8I4=43,        /* 64-bit MMX vector of signed ints */
    MTYPE_M8F4=44,        /* 64-bit MMX vector of signed floats */

    MTYPE_V32C4=45,       /* 256-bit vector of C4                    */
    MTYPE_V32C8=46,       /* 256-bit vector of C8                    */
    MTYPE_V32I1=47,       /* 256-bit vector of signed bytes          */
    MTYPE_V32I2=48,       /* 256-bit vector of signed short ints     */
    MTYPE_V32I4=49,       /* 256-bit vector of signed ints           */
    MTYPE_V32I8=50,       /* 256-bit vector of signed long long ints */
    MTYPE_V32F4=51,       /* 256-bit vector of floats                */
    MTYPE_V32F8=52,       /* 256-bit vector of doubles               */
    MTYPE_LAST = MTYPE_V32F8
#elif defined(TARG_SL)
  /* mtype list for sbuf type */
    MTYPE_SB1=28,
    MTYPE_SB2=29,
    MTYPE_SB4=30,
    MTYPE_SB8=31,
    MTYPE_SBU1=32,
    MTYPE_SBU2=33,
    MTYPE_SBU4=34,
    MTYPE_SBU8=35,
 /* mtype list for sdram type */
    MTYPE_SD1=36,
    MTYPE_SD2=37,
    MTYPE_SD4=38,
    MTYPE_SD8=39,
    MTYPE_SDU1=40,
    MTYPE_SDU2=41,
    MTYPE_SDU4=42,
    MTYPE_SDU8=43,
  /* mtype list for vbuf type */  
    MTYPE_VBUF1=44,
    MTYPE_VBUF2=45,
    MTYPE_VBUF4=46,
    MTYPE_LAST = MTYPE_VBUF4
#else
    MTYPE_LAST = MTYPE_U16
#endif // TARG_X8664
};

/* Define the type: */
typedef UINT8	TYPE_ID;
typedef UINT8	mTYPE_ID;


/* Type_class_bits */
#define MTYPE_CLASS_INTEGER	0x01
#define MTYPE_CLASS_FLOAT	0x02
#define MTYPE_CLASS_COMPLEX	0x04
#define MTYPE_CLASS_UNSIGNED	0x08
#define MTYPE_CLASS_STR		0x10
#define MTYPE_CLASS_VECTOR	0x20
#ifdef TARG_X8664
#define MTYPE_CLASS_SVECTOR	0x60  // 2 bits for short vector (64-bit vector)
#define MTYPE_CLASS_MVECTOR	0xa0  // 2 bits for MMX vector (64-bit vector)
#define MTYPE_CLASS_AVECTOR	0x120 // 2 bits for AVX vector (256-bit vector)
#endif
#define MTYPE_CLASS_UNSIGNED_INTEGER (MTYPE_CLASS_UNSIGNED|MTYPE_CLASS_INTEGER)
#define MTYPE_CLASS_COMPLEX_FLOAT (MTYPE_CLASS_COMPLEX|MTYPE_CLASS_FLOAT)

/* Define the type descriptor: */
typedef struct type_desc {
  mCLASS_INDEX	id;		/* Type index -- MTYPE_xxx above */
  mUINT16	bit_size;	/* bit size */
  mUINT16	byte_size;	/* byte size (NYI) */
  mUINT16	dummy1;		/* remove when incompatible change */
  mUINT8	alignment;	/* byte alignment */
  mUINT8	dummy2;		/* remove when incompatible change */
  mUINT8	dummy3;		/* remove when incompatible change */

  mBOOL		signed_type;	/* Signed numeric type? */
  mBOOL		float_type;	/* Floating point type? */
  mCLASS_INDEX	dummy4;		/* remove when incompatible change */
  const char   *name;		/* Print name */
  mUINT16       type_class_bits;/* The classification bits used by the simplifier */
  mUINT8        type_order;	/* The order of types (I8 > I4 for example) */
  mCLASS_INDEX	complement;	/* complementary signed partner (ex. U1 -> I1) */
} TYPE_DESC;
/* Types which are not supported in memory should have memory sizes 
 * and alignment values of 0. */

/* Declare the type descriptor table: */
extern TYPE_DESC Machine_Types[];

/* Define the access functions: */
#define MTYPE_id(n)		((TYPE_ID)(Machine_Types[n].id))
#define MTYPE_bit_size(n)	(Machine_Types[n].bit_size)
#define MTYPE_byte_size(n)	(MTYPE_bit_size(n) >> 3)
#define MTYPE_size_reg(n)	MTYPE_bit_size(n)
#define MTYPE_size_min(n)	MTYPE_bit_size(n)
#define MTYPE_size_best(n)	MTYPE_bit_size(n)
#define MTYPE_alignment(n)	(Machine_Types[n].alignment)
#define MTYPE_align_min(n)	MTYPE_alignment(n)
#define MTYPE_align_req(n)	MTYPE_alignment(n)
#define MTYPE_align_best(n)	MTYPE_alignment(n)
#define MTYPE_signed(n)		(Machine_Types[n].signed_type)
#define MTYPE_float(n)		(Machine_Types[n].float_type)
#define MTYPE_name(n)		(Machine_Types[n].name)
#define MTYPE_type_class(n)     (Machine_Types[n].type_class_bits)
#define MTYPE_type_order(n)     (Machine_Types[n].type_order)
#define MTYPE_complement(n)     (Machine_Types[n].complement)

/* define register classes */
#define MTYPE_is_integral(n)	(MTYPE_type_class(n) & MTYPE_CLASS_INTEGER)
#define MTYPE_is_signed(n)	(MTYPE_type_class(n)==MTYPE_CLASS_INTEGER)
#define MTYPE_is_unsigned(n)	(MTYPE_type_class(n)==MTYPE_CLASS_UNSIGNED_INTEGER)
#define MTYPE_is_float(n)	(MTYPE_type_class(n) & MTYPE_CLASS_FLOAT)
#define MTYPE_is_complex(n)	(MTYPE_type_class(n) & MTYPE_CLASS_COMPLEX)
#define MTYPE_is_vector(n)	(MTYPE_type_class(n) & MTYPE_CLASS_VECTOR)
#ifdef TARG_X8664
#define MTYPE_is_short_vector(n) ((MTYPE_type_class(n) & MTYPE_CLASS_SVECTOR) == MTYPE_CLASS_SVECTOR)
#define MTYPE_is_mmx_vector(n) ((MTYPE_type_class(n) & MTYPE_CLASS_MVECTOR) == MTYPE_CLASS_MVECTOR)
#endif // TARG_X8664
#ifdef KEY
#ifndef MTYPE_is_short_vector
#define MTYPE_is_short_vector(n) (MTYPE_type_class(n)==MTYPE_CLASS_SVECTOR)
#endif
#define MTYPE_is_str(n)		(MTYPE_type_class(n)==MTYPE_CLASS_STR)
#else
#define MTYPE_is_str(n)		(MTYPE_type_class(n)==MTYPE_STR)
#endif // KEY
#define MTYPE_is_m(n)		((n)==MTYPE_M)
#define MTYPE_is_void(n)	((n)==MTYPE_V)

#define MTYPE_is_F10(n)	        ((n)==MTYPE_F10)
#define MTYPE_is_F10_or_C10(n)	((n)==MTYPE_F10 ||(n)==MTYPE_C10 )
#define MTYPE_is_F16(n)	        ((n)==MTYPE_F16)
#define MTYPE_is_quad(n)	((n)==MTYPE_FQ || (n)==MTYPE_CQ)
#define MTYPE_is_pointer(n)	((n)==Pointer_type || (n)==Pointer_type2)
#define MTYPE_is_boolean(n)	((n)==Boolean_type || (n)==Boolean_type2)
#define MTYPE_refs_x87(n)       (MTYPE_is_F10_or_C10( n ) || \
	                         ( MTYPE_is_float( n ) &&    \
	                           Is_Target_32bit()))
/* Register size in bytes */
#define	MTYPE_RegisterSize(n)	MTYPE_byte_size(n)

#define MTYPE_is_size_double(n)	(MTYPE_bit_size(n) == MTYPE_bit_size(MTYPE_I8))

#if defined(TARG_SL) || defined(TARG_PPC32)
#define MTYPE_is_longlong(n) (MTYPE_is_integral(n) && (MTYPE_bit_size(n) == 64))
#define MTYPE_is_double(n) (MTYPE_is_float(n) && (MTYPE_bit_size(n) == 64))
#endif

/* Define a mask of machine types, for use in register descriptors: */
typedef UINT32 MTYPE_MASK;

/* Convert TYPE_ID to MTYPE_MASK: */
#define Type_Mask(t)	( ((MTYPE_MASK) 1) << (t) )

/* Operations on type masks: */
#define TMASK_Element(m,k)	(((m) & Type_Mask(k))!=0)
#define TMASK_And(m1,m2)	((m1) & (m2))
#define TMASK_Or(m1,m2)		((m1) | (m2))
#define TMASK_Included(m1,m2)	(TMASK_And(m1,m2)==(m1))

/* Define which types are available on the target: */
extern MTYPE_MASK Machine_Types_Available;
#define MTYPE_Avail(k)	TMASK_Element(Machine_Types_Available,k)

/* =======================================================================
 *
 *  Exported Functions
 *
 *    char * Mtype_Name ( TYPE_ID b)
 *
 *	Routine to provide printable type name.
 *
 *
 *    TYPE_ID Mtype_AlignmentClass(INT32 align, mUINT8 class)
 *
 *	Routine to return a type that matches exact byte alignment
 *	and class
 *
 *
 *    TYPE_ID Mtype_TransferSign(TYPE_ID type)
 *
 *	Routine to return the unsigned type corresponding to type
 *
 */

extern const char *Mtype_Name ( TYPE_ID );
extern TYPE_ID	Mtype_AlignmentClass( INT32 , mUINT8 );
extern TYPE_ID	Mtype_Promote_to_A4A8( TYPE_ID );
extern TYPE_ID	Mtype_TransferSign( TYPE_ID, TYPE_ID );
extern TYPE_ID	Mtype_TransferSize( TYPE_ID, TYPE_ID );
extern TYPE_ID	Mtype_complex_to_real( TYPE_ID);
extern TYPE_ID  Mtype_comparison( TYPE_ID );
extern TYPE_ID  Mtype_next_alignment( TYPE_ID);
extern TYPE_ID  Mtype_prev_alignment( TYPE_ID);

extern TYPE_ID  Mtype_vector_elemtype( TYPE_ID);

#ifdef __cplusplus
}
#endif
#endif /* mtypes_INCLUDED */
