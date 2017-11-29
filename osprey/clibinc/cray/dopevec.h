/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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

/* USMID @(#) clibinc/cray/dopevec.h	92.3	06/16/99 15:48:24 */
#ifndef _DOPEVEC_H                      /* prevent multiple inclusions */
#define _DOPEVEC_H

/*
 * Header file describing the Fortran 90 and CF77 Dope Vector structure
 */

#include <fortran.h>

/*
 *	STRUCTURES
 */

/*
 * Word 4 contains type and length information so the libraries can perform
 * conversion when specified through FFIO.  int_len is the container bit
 * size used on a cray.  el_len and int_len fields are the same for integer,
 * real, complex, and logical.  For ASCII character, int_len is 8 to specify
 * the length of a single character.  int_len and dec_len are not used for
 * derived type entities.  F90 array intrinsics use type and int_len.
 */

typedef struct f90_type {
#if defined(__mips) || !defined(_WORD32) || defined(_LITTLE_ENDIAN)
    unsigned int		:32;     /* used for future development */
#endif

    enum typecodes {
	DVTYPE_UNUSED	   = 0,
	DVTYPE_TYPELESS    = 1,
	DVTYPE_INTEGER	   = 2,
	DVTYPE_REAL	   = 3,
	DVTYPE_COMPLEX	   = 4,
	DVTYPE_LOGICAL	   = 5,
	DVTYPE_ASCII	   = 6,
	DVTYPE_DERIVEDBYTE = 7,
	DVTYPE_DERIVEDWORD = 8
    } 	       		type    :8;	/* type code */
    unsigned int	dpflag  :1;	/* set if declared double precision
					 * or double complex */
    enum dec_codes {
	DVD_DEFAULT	= 0,		/* KIND= and *n absent, or 
					 * KIND=expression which evaluates to
					 * the default KIND, ie.:
					 *      KIND(0) for integer 
					 *      KIND(0.0) for real 
					 *      KIND((0,0)) for complex 
					 *      KIND(.TRUE.) for logical 
					 *      KIND('A') for character 
					 * across on all ANSI-conformant
					 *  implementations. */
	DVD_KIND	= 1,		/* KIND=expression which does not 
					 * qualify to be DVD_DEFAULT or 
					 * DVD_KIND_CONST or DVD_KIND_DOUBLE */
	DVD_STAR	= 2,		/* *n is specified (example: REAL*8 */
	DVD_KIND_CONST	= 3,		/* KIND=expression constant across
					 * all implementations. */
	DVD_KIND_DOUBLE	= 4		/* KIND=expression which evaluates to
					 * KIND(1.0D0) for real across all
					 * implementations.  This code may be
					 * passed for real or complex type.  */
    } kind_or_star		:3;	/* Set if KIND= or *n appears in the
                                         * variable declaration.  Values 
					 * are from enum dec_codes */
    unsigned int	int_len :12;	/* internal length in bits of iolist
                                         * entity. 8 for character data to
                                         * indicate size of each character */
    unsigned int	dec_len :8; 	/* declared length in bytes for *n
					 * or KIND value. Ignored if
					 * kind_or_star==DVD_DEFAULT */
} f90_type_t;

#  ifdef KEY /* Bug 6845 */
/*
 * If DopeVectorType.alloc_cpnt is true, then following the last actual
 * dimension or codimension (not necessarily MAXDIM) there is a count of the
 * number of allocatable components, followed by an array of byte offsets
 * from the beginning of the structure to each allocatable component. If
 * DopeVectorType.alloc_cpnt is false, neither of these appears.
 */
typedef struct {
  unsigned long n_alloc_cpnt;
  unsigned long alloc_cpnt_offset[0];
  } DopeAllocType;
#  endif /* KEY Bug 6845 */

/* Header
 *
 *  The three types of data objects described by this dope vector are listed
 *  here with the valid address/size fields which may be referenced.
 *
 *
 *  1.	Word aligned data types.  
 *
 *	Type Codes
 *	----------
 *	    DVTYPE_TYPELESS 
 *	    DVTYPE_INTEGER 
 *	    DVTYPE_REAL 
 *	    DVTYPE_COMPLEX 
 *	    DVTYPE_LOGICAL 
 *	    DVTYPE_DERIVEDWORD 
 *
 *	This includes:
 *	a.  All intrinsic noncharacter data types
 *	b.  All word-aligned sequence derived data types. This includes
 *         noncharacter sequence derived data types and mixed character
 *         and noncharacter sequence derived data types.
 *
 *	If the object is a mixed character and noncharacter sequence derived
 *	type, the element length includes any padding to word boundaries.
 *
 *	Valid base_addr Fields
 *	----------------------
 *
 *	    base_addr.a.ptr    - address of the first element
 *	    base_addr.a.el_len - the bit length of each data element
 *
 *
 *  2.	Character sequence derived type.  
 *
 *	Type Codes
 *	----------
 *	    DVTYPE_DERIVEDBYTE 
 *
 *	Valid base_addr Fields
 *	----------------------
 *
 *	    base_addr.a.ptr    - address of the first element
 *	    base_addr.a.el_len - the bit length of each data element
 *
 *
 *  3. The array is intrinsic character and byte-aligned. 
 *
 *	Type Codes
 *	----------
 *
 *	    DVTYPE_ASCII 
 *
 *	Valid base_addr Fields
 *	----------------------
 *
 *	    base_addr.a.charptr - fcd which describes the first element 
 *
 *	  YMP/C90 only:
 *	    base_addr.a.el_len  - the bit length of each data element
 *
 *
 *	    Note that for an array, the _fcdlen() value corresponds to
 *	    one element of the array, not the entire array.
 *
 *	    Also note that on YMP or C90 systems with one-word FCDs, 
 *	    the el_len field is also valid.  By convention, the libraries do 
 *	    not read this field for DVTYPE_ASCII type, but the field is set
 *	    because the YMP/C90 compiler needs the element length information to
 *	    be in both _fcdlen(base_addr.a.charptr) and in base_addr.a.el_len.
 */

/*
 * PROPOSED CHANGE:
 *
 * fcd must contain the void pointer and a 64-bit element length.
 * The length is still in bits but is in a 64-bit container.
 *
 *  union       {
 *	_fcd    charptr;
 *		struct {
 *			void	       *ptr;
 *			unsigned long long  el_len;
 *		} a;
 *  }   base_addr;
 */
typedef struct DopeVector {
    union       {
		_fcd    charptr;	/* Fortran character descriptor */
		struct {
			void	       *ptr;    /* pointer to base address */
                    			        /* or shared data desc     */
			unsigned long  el_len;	/* element length in bits  */
		} a;
    }   base_addr;

    /*
     * flags and information fields within word 3 of the header
     */
    unsigned int 	assoc     :1;	/* associated flag */
    unsigned int	ptr_alloc :1;	/* set if allocated by pointer */
    enum ptrarray {
	NOT_P_OR_A	= 0,
	POINTTR		= 1,
	ALLOC_ARRY	= 2
    } 			p_or_a    :2;	/* pointer or allocatable array. Use */
					/* enum ptrarray values.  */
    unsigned int	a_contig  :1;	/* array storage contiguous flag */
/*
 * PROPOSED CHANGE:
 *
 * Add version and coarray n_dim fields.  Update the n_dim to all
 * up to 255 dimensions although only 20 may be the actual MAXDIM.
 * The f95 standard is still only 7.
 *
 *  unsigned long	dv_versn  :6;
 *  unsigned long		  :21;
 *  unsigned long	          :8;
 *  unsigned long	n_codim   :8;
 *  unsigned long	          :8;
 *  unsigned long	n_dim	  :8;
 */
#if defined(__mips) || defined(_LITTLE_ENDIAN)
#  ifdef KEY /* Bug 6845 */
    unsigned int alloc_cpnt       :1;   /* this is an allocatable array whose
                                         * element type is a derived type
					 * having component(s) which are
					 * themselves allocatable       */
    unsigned int	          :26;	/* pad for first 32 bits	*/
#  else /* KEY Bug 6845 */
    unsigned int	          :27;	/* pad for first 32 bits	*/
#  endif /* KEY Bug 6845 */
    unsigned int	          :29;	/* pad for second 32-bits	*/
#elif defined( _WORD32)
    unsigned long	unused1   :24;  /* unused */
#else
    unsigned long	unused1   :56;  /* unused */
#endif
    unsigned int	n_dim     :3;	/* number of dimensions */
    f90_type_t		type_lens;	/* data type and lengths */
    void           	*orig_base;	/* original base address */
/*
 * PROPOSED CHANGE:
 *
 * orig_size must be 64 bits always and is in bits.
 *
 *  unsigned long long orig_size;
 */
    unsigned long 	orig_size;	/* original size */
    /*
     * Per Dimension Information
     */
#define MAXDIM 7
/*
 * PROPOSED CHANGE:
 *
 * The maximum number of dimensions may be 255 with 8 bits but
 * allow only 20 for now.  See Section 5.1.2.4 of f95 standard. 
 *
 * define MAXDIM 20
 *
 * The dimension structure must contain 64-bit fields with the
 * changes to n_dim to 8 bits or 255.  Therefore, low_bound, extent,
 * and stride_mult will be long long or int64 in the future.
 *
 *  struct DvDimen {
 *      signed long long	low_bound;
 *      signed long long	extent;
 *      signed long long 	stride_mult;
 *  }dimension[MAXDIM];
 *
 * The stride multiplier will be in terms of elements in the future.
 * In terms of the library on some systems, the library will need to
 * change from multiplying the stride mult by 2 for 32-bit sizes.
 * The version flag is needed to allow the library to handle this
 * change.
 *
 * The number of elements in the dimension structure array will match
 * the number of dimensions in n_dim, i.e., if n_dim = 3, then
 * dimension will have only three elements (dimension[3]).
 *
 * The coarray dimension information will follow the non-coarray
 * dimension array.  The number of elements in the codimension
 * structure array will match the number of dimensions in n_codim,
 * i.e., if n_codim = 2, then codimension will have only two elements.
 *
 *  struct CoDvDimen {
 *      signed long long	low_bound;
 *      signed long long	extent;
 *      signed long long 	stride_mult;
 *  }codimension[MAXDIM];
 */
    struct DvDimen {
        signed long	low_bound;	/* lower bound for ith dimension */
                                        /* may be negative */
        signed long	extent;		/* number of elts for ith dimension */
        /*
         * The stride mult is not defined in constant units so that address
         * calculations do not always require a divide by 8 or 64.  For
 	 * double and complex, stride mult has a factor of 2 in it.  For
 	 * double complex, stride mult has a factor of 4 in it.
         */
        signed long 	stride_mult;    /* stride multiplier */
    }dimension[MAXDIM];
#  ifdef KEY /* Bug 6845 */
   /* DopeAllocType alloc_info; appears following the last actual dimension
    * (which may be less than MAXDIM) */
#  endif /* KEY Bug 6845 */
} DopeVectorType;


/*
 *	MACROS and TYPEDEFS
 */

#define	DVTYPE_NTYPES	9		/* number of data type codes	*/
typedef enum typecodes ftype_t;		/* Fortran 90 type code		*/

/*
 *	SMSCALE computes the scale of stride_mult for a non-character
 *	dopevector.  This is the number of bytes per stride_mult.  On some
 *	architectures, this always evaluates to sizeof(long).
 *
 *	Parameter:
 *		dv	- pointer to dope vector.
 *
 *	Return value:
 *		The size of one stride_mult in bytes.
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define SMSCALE(dv) \
	(((dv)->type_lens.int_len >= 8*sizeof(_f_int4)) ? 4 : \
	(((dv)->type_lens.int_len == 8*sizeof(_f_int2)) ? 2 : \
	(((dv)->type_lens.int_len == 8*sizeof(_f_int1)) ? 1 : 4)))
#elif     !defined(_WORD32) && (defined(_F_INT4) || defined(_F_REAL4))
#define SMSCALE(dv) \
	(((dv)->type_lens.int_len < 8*sizeof(long)) ? 4 : sizeof(long))
#else
#define SMSCALE(dv) (sizeof(long))
#endif

#define CPPLOG2(x) \
	(x == 1 ? 0 : \
	(x == 2 ? 1 : \
	(x == 4 ? 2 : \
	(x == 8 ? 3 : \
	(x == 16 ? 4 : \
	(x == 32 ? 5 : \
	-1))))))

#define SMSHIFT(dv) CPPLOG2(SMSCALE(dv)/sizeof(bcont))

#endif /* !_DOPEVEC_H */

