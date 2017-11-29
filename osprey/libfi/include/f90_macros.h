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


/* USMID @(#) libfi/include/f90_macros.h	92.0	10/08/98 14:37:14 */


/************************************************************************
 *
 * This file contains C macros that are used by the MPP version
 * of the Fortran 90 array intrinsics.
 *
 ************************************************************************/


/*
 * LOG2(n) - Find the log base 2 of a power of 2
 */

#define BITS_PER_INT ( sizeof ( int ) * 8 )
#define LOG2(num)      ( BITS_PER_INT - _leadz(num) - 1 )


/************************************************************************
 *
 * EXTENT_CHK() - Check the extents for an array. If any of the
 *		extents are zero, then doctor up a NULL result
 *		and return to the caller.
 *
 * Arguments:     DopeVectorType * array;
 *
 * Variables used: DopeVectorType * result;
 *
 ************************************************************************/

#define EXTENT_CHK(array)						\
_Pragma("shortloop");							\
	for (i=0; i < array->n_dim; i++) {				\
	    if (array->dimension[i].extent == 0) {			\
		result->ptr_alloc = 1;					\
		result->assoc = 1;					\
		result->n_dim = 0;					\
		result->dimension[0].extent = 0;			\
		result->dimension[0].stride_mult = 1;			\
		return;							\
	    }								\
	}



/************************************************************************
 *
 * SETUP_EXTENTS() - Set up an array of extents for the source array,
 *		   and the result array based on dim.
 *
 * Variables used:  DopeVectorType * result;
 *		  DopeVectorType * source;
 *		  long src_extents[RANK];
 *		  long res_extents[RANK-1];
 *		  long blkcnts[RANK];
 *		  long dim;
 *		  long dim_bcnt;
 *
 ************************************************************************/

#define SETUP_EXTENTS()							\
	for (i=0; i < RANK; i++) {					\
	    src_extents[i] = source->dimension[i].extent;		\
	}								\
_Pragma("shortloop");							\
	for (i=0; i < *dim-1; i++) {					\
	    res_extents[i] =  src_extents[i];				\
	}								\
	for (i=*dim-1; i < RANK-1; i++) {				\
	    res_extents[i] = src_extents[i+1];				\
	}


/************************************************************************
 *
 * SETUP_BLKCNTS() - Set up an array of block counts for each
 *		   dimension of the source array.
 *
 * Variables used: long blkcnts[RANK];
 *		 long dim_bcnt;
 *		 DopeVectorType * source;
 *
 ************************************************************************/

#define SETUP_BLKCNTS()							\
	if (_in_parallel()) {						\
	    long *sdd_ptr;						\
	    sdd_ptr = source->base_addr.a.ptr;				\
	    for (i=0; i < RANK; i++) {					\
		blkcnts[i] = _blkct(sdd_ptr,i+1,_MY_PE());		\
	    }								\
	    for (i=0; i < *dim-1; i++) {				\
		dim_bcnt[i] = blkcnts[i];				\
	    }								\
	    for (i = *dim-1;  i < RANK-1; i++) {			\
		dim_bcnt[i] = blkcnts[i+1];				\
	    }								\
	}



/************************************************************************
 *
 * INIT_SDD() -    Initialize the shared data descriptor for the
 *		 shared result temporary so that it describes
 *		 one distributed array that is dimensioned the
 *		 same as the source array excluding DIM and has
 *		 each dimension distributed :BLOCK.
 *
 * Variables used: DopeVectorType * result;
 *		 void * result_base_ptr;
 *		 long weights[result->n_dim];
 *		 long blknums[result->n_dim];
 *		 long res_extents[result->n_dim];
 *
 ************************************************************************/

#define INIT_SDD()							\
	for (i=0; i < result->n_dim; i++) {				\
	    blknums[i] = result->dimension[i].extent;			\
	    weights[i] = 2;						\
	}								\
	_init_sdd(result->base_addr.a.ptr, result_base_ptr,		\
		result->n_dim, weights, blknums, res_extents);
	

/*
 * INIT_LOC_SDD() - Initialize the shared data descriptor for the
 *                  shared temporary result so that it describes
 *                  one distributed array that is dimensioned
 *                  (_N_PES,RANK) and distributed across the
 *                  processors (:BLOCK(1),:).
 *
 *                  Used in the distributed versions of MAXLOC
 *                  and MINLOC.
 *
 * Variables uses:  DopeVectorType * result;
 *                  DopeVectorType * source;
 *                  long dim1_tmp, dim2_tmp;
 *                  void * result_base_ptr;
 */

#define INIT_LOC_SDD()                                                  \
                                                                        \
        /*                                                              \
         * Set the base address field in the sdd to the base pointer.   \
         */                                                             \
                                                                        \
        _sdd_write_base(result->base_addr.a.ptr,result_base_ptr);       \
                                                                        \
        /*                                                              \
         * Calculate cyc_ebp and pe_bcnt for the 1st dimension as       \
         * log2(_N_PES). blk_ebp for the first dimension is always 0.   \
         */                                                             \
                                                                        \
        dim1_tmp = LOG2(_N_PES);                                        \
        _sdd_write_cyc_ebp(result->base_addr.a.ptr,1,dim1_tmp);         \
        _sdd_write_pe_bcnt(result->base_addr.a.ptr,1,dim1_tmp);         \
        _sdd_write_blk_ebp(result->base_addr.a.ptr,1,0);



/************************************************************************
 *
 * CHECK_MASK(sdd_ptr, flag) - Check to see if a mask argument
 *		was provided by the user, then check to see if it
 *		is an array or a scalar. If the mask is a non-zero
 *		scalar, then treat as if no mask was provided. If
 *		the mask is a scalar with a value of zero, then set
 *		the result to 0 and return to caller. If the mask
 *		is a shared array, then set up the mask's sdd
 *		pointer.
 *
 * Arguments:     void * sdd_ptr;
 *		long flag;
 *
 * Variables used: void * result_base_ptr;
 *		 DopeVectorType * source;
 *		 DopeVectorType * mask;
 * 
 ************************************************************************/

#define USE_IT		1L
#define DONT_USE_IT	0L
#define FALSE_MASK	-1L

#define CHECK_MASK(sdd_ptr, flag)					\
	flag = DONT_USE_IT;						\
	if (mask) {							\
	    if (mask->n_dim > 0) {					\
		sdd_ptr = mask->base_addr.a.ptr;			\
		flag = USE_IT;						\
	    } else {							\
		if (_ltob((int *)mask->base_addr.a.ptr) == 0) {		\
		    flag = FALSE_MASK;					\
		}							\
	    }								\
	} else {							\
	    sdd_ptr = 0;						\
	}


/************************************************************************
 *
 * CHECK_STOP(sdd_ptr, flag) - Check to see if the stop argument
 *		provided by the user is an array or a scalar. If the mask
 *		is a non-zero scalar, then treat as if no mask was provided.
 *		If the mask is a scalar with a value of zero, then set
 *		the result to 0 and return to caller. If the mask
 *		is a shared array, then set up the mask's sdd
 *		pointer.
 *
 * Arguments:     void * sdd_ptr;
 *		long flag;
 *
 * Variables used: void * result_base_ptr;
 *		 DopeVectorType * source;
 *		 DopeVectorType * mask;
 * 
 ************************************************************************/

#define USE_IT		1L
#define FALSE_MASK	-1L

#define CHECK_STOP(sdd_ptr, flag)					\
	if (stop->n_dim > 0) {						\
	    sdd_ptr = stop->base_addr.a.ptr;				\
	    flag = USE_IT;						\
	} else {							\
	    if (_ltob((int *)stop->base_addr.a.ptr) == 0) {		\
		flag = FALSE_MASK;					\
	    }								\
	}


/************************************************************************
 *
 * DIM_CHK(dim) - Check to insure that the DIM arguments to the
 *		array intrinsics contains a legal value from 1
 *		through the extent of the source array.
 *
 * Arguments:     long dim;
 *
 ************************************************************************/

#define DIM_CHK(dim)							\
	if ((*dim < 1) || (*dim > RANK)) {				\
	    _lerror(_LELVL_ABORT, FESCIDIM);				\
	}


/************************************************************************
 *
 * INIT_DIM_COUNT() - Check to insure that the DIM argument to the
 *		shift arrary intrinsics contains a legal value from 1
 *		through the extent of the source array.  If no DIM is
 *		  present, make it default to 1.
 *
 * Arguments:     long dim;
 *
 ************************************************************************/

#define INIT_DIM_COUNT()						\
	if (dim == NULL) {						\
	    dim_val = 1;						\
	} else if ((*dim < 1) || (*dim > RANK)) {			\
	    _lerror(_LELVL_ABORT, FESCIDIM);				\
	} else {							\
	    dim_val = *dim;						\
	}

/***********************************************************************
 * INIT_SHFT_SDD()      - Fill in the sdd fields for the result sdd.
 *			Since the result will always be the same shape
 *			as the source, the distribution will also be
 *			the same, so the sdd fields will be copied.
 **********************************************************************/

#define INIT_SHFT_SDD()							\
									\
/*									\
 *      Set the base address field in the sdd to the base pointer.	\
 */									\
									\
	_sdd_write_base(result_sdd_ptr, result->base_addr.a.ptr);	\
									\
/*									\
 *      Copy the block fields from the source sdd to the result sdd	\
 */									\
									\
_Pragma ("shortloop"); \
	for (i = 0; i < RANK; i++) {					\
	    tmp = _sdd_read_cyc_ebp (source->base_addr.a.ptr, i);	\
	    _sdd_write_cyc_ebp (result->base_addr.a.ptr, i, tmp);	\
	    tmp = _sdd_read_pe_bcnt (source->base_addr.a.ptr, i);	\
	    _sdd_write_pe_bcnt (result->base_addr.a.ptr, i, tmp);	\
	    tmp = _sdd_read_blk_ebp (source->base_addr.a.ptr, i);	\
	    _sdd_write_blk_ebp (result->base_addr.a.ptr, i, tmp);	\
	    tmp = _sdd_read_canon (source->base_addr.a.ptr);		\
	    _sdd_write_canon (result->base_addr.a.ptr, tmp);		\
	    tmp = _sdd_read_offset (source->base_addr.a.ptr);		\
	    _sdd_write_offset (result->base_addr.a.ptr, tmp);		\
	}								\


/***********************************************************************
 *      The shift count can be a scalar or an array.  If scalar, set the
 *      shift scalar flag, and set the shift value counter to the shift
 *      value.  If shift is an array, clear the shift scalar flag.  In
 *      either case, the shift array pointer will be passed to the work
 *      routine, but it will not be used if the scalar flag is set.
 **********************************************************************/

#define INIT_SHIFT_COUNT()						\
	if (shift->n_dim == 0) {					\
	    shflag = _btol (1);						\
	    i = (int) shift->base_addr.a.ptr;				\
	    if (i > 0)							\
		shptr = (int *) shift->base_addr.a.ptr;			\
	    else {							\
		shptr = (int *) _sdd_read_base(shift->base_addr.a.ptr);	\
	    }								\
	    shftval = shptr[0];						\
	} else {							\
	    shflag = _btol (0);						\
	}								\


/***********************************************************************
 *      The boundary value for an eoshift call can be non-existent, a
 *	scalar value, or an array.  If non-existent, we need to set the
 *	boundary flag to indicate a scalar, and set the scalar boundary
 *	variable to the default value.  If the bound is a scalar, we
 *	need to set the scalar flag, and put the value in the scalar
 *	boundary variable.  If the bound argument is an array, clear
 *	the boundary scalar flag.
 **********************************************************************/

#define	INIT_BOUND_1WORD()						\
	if (boundary) {							\
	    if (boundary->n_dim == 0) {					\
		bndflag = 1;						\
		i = (int) boundary->base_addr.a.ptr;			\
		if (i > 0)						\
		    vptr = (void *) boundary->base_addr.a.ptr;		\
		else							\
		    vptr = _sdd_read_base(boundary->base_addr.a.ptr);	\
		ptr = (int *) vptr;					\
		bndval = ptr[0];					\
	    } else {							\
		bndflag = 0;						\
	    }								\
	} else {							\
	    bndflag = 1;						\
	    if (source->type_lens.type == DVTYPE_INTEGER)		\
		bndval = 0;						\
	    else if (source->type_lens.type == DVTYPE_REAL)		\
		bndval = *(int *) &defaultr;				\
	    else							\
		bndval = _btol (0);					\
	}

#define	INIT_BOUND_2WORD()						\
	if (boundary) {							\
	    if (boundary->n_dim == 0) {					\
		bndflag = 1;						\
		i = (int) boundary->base_addr.a.ptr;			\
		if (i > 0)						\
		    vptr = (void *) boundary->base_addr.a.ptr;		\
		else							\
		    vptr = _sdd_read_base(boundary->base_addr.a.ptr);	\
		ptr = (_f_comp *) vptr;					\
		bndval = ptr[0];					\
	    } else {							\
		bndflag = 0;						\
	    }								\
	} else {							\
	    bndflag = 1;						\
	    bndval = 0.0 + 0.0i;					\
	}

/************************************************************************
 *
 * SETUP_PRE_BLKCNTS() - Set up an array of block counts for each
 *                 dimension of the source array.
 *
 * Variables used: long blkcnts[RANK];
 *               long dim_bcnt;
 *               DopeVectorType * source;
 *
 ************************************************************************/

#define SETUP_PRE_BLKCNTS()                                             \
        if (_in_parallel()) {                                           \
            long *sdd_ptr;                                              \
            sdd_ptr = source->base_addr.a.ptr;                          \
            for (i=0; i < RANK; i++) {                                  \
                blkcnts[i] = _blkct(sdd_ptr,i+1,_MY_PE());              \
            }                                                           \
        }




                                                                        
#ifdef	NOMORE
        /*                                                              \
         * Calculate cyc_ebp and blk_ebp for the 2nd dimension as       \
         * log2(source->n_dim) + cyc_ebp for dimension 1. The pe_cnt    \
         * is always 0 for the 2nd dimension.                           \
         */                                                             \
                                                                        \
        dim2_tmp = LOG2(RANK);                                          \
        if (_popcnt(RANK) != 1) {                                       \
            dim2_tmp++; /* adjust since n_dim not a power of 2 */       \
        }                                                               \
        dim2_tmp += dim1_tmp;                                           \
        _sdd_write_cyc_ebp(result->base_addr.a.ptr,2,dim2_tmp);         \
        _sdd_write_pe_bcnt(result->base_addr.a.ptr,2,0);                \
        _sdd_write_blk_ebp(result->base_addr.a.ptr,2,dim2_tmp);
#endif
