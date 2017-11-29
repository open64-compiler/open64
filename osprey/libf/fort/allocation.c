/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fort/allocation.c	92.6	10/29/99 21:39:27"

#include <fortran.h>
#include <stddef.h>
#include <string.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
/* for malloc */
#ifdef	_LITTLE_ENDIAN
#include <stdlib.h>
#endif
#include "defalias.h"
#ifdef KEY /* Bug 6845 */
# ifdef _DEBUG
#  include <stdio.h>
# endif /* _DEBUG */
#endif /* KEY Bug 6845 */

extern long     _zero_entity;   /* nonzero addr for PRESENT func */

#define NaN32 0xffa5a5a5
#define NaN64 0xffa5a5a5fff5a5a5ll

static short ps_debug_alloc = -1; /* -1 is uninitialized
				   * 0 is no allocation debugging
				   * 1 to fill allocations with 0s
				   * 2 to fill allocations with 32 bit NaNs
				   * 3 to fill allocations with 64 bit NaNs */

/*
 *      The alloc list describes all the items in the allocation list
 *      for the ALLOCATE and DEALLOCATE statements.
 */

#if	defined(__mips)
/* externs for special sma memory allocation/deallocation. */
extern void *_sma_fortran_allocate_global(size_t sz, int lstat);
#pragma weak _sma_fortran_allocate_global
extern void _sma_fortran_deallocate_global(void *p);
#pragma weak _sma_fortran_deallocate_global
#endif		/* end MIPS */

#ifndef ALLOC_VERSION
#define ALLOC_VERSION  1                /* alloc version number */
#endif

typedef struct AllocHead {
	unsigned int	version :8;	/* contains ALLOC_VERSION */
#ifdef _UNICOS
	unsigned int	        :32;	/* unused */
#elif defined(__mips) || defined(_LITTLE_ENDIAN)
	unsigned int	        :24;	/* unused */
	unsigned int	        :8;	/* unused */
#endif
	unsigned int	        :7;	/* unused */
	unsigned int	imalloc	:1;	/* call special malloc */
	unsigned int	icount  :16;	/* size of struct alloclist in */
					/* words. */
	DopeVectorType *dv[1];		/* array of pointers to dope vects */
} AllocHeadType;

short get_debug_alloc_state()
{
	char * debugenv;

	debugenv = getenv ("OPEN64_FDEBUG_ALLOC");
	if (!debugenv) {
		return 0;
	}
	else if (strcasecmp (debugenv, "ZERO") == 0) {
		return 1;
	}
	else if (strcasecmp (debugenv, "NaN") == 0) {
		return 2;
	}
	else if (strcasecmp (debugenv, "NaN4") == 0) {
		return 2;
	}
	else if (strcasecmp (debugenv, "NaN32") == 0) {
		return 2;
	}
	else if (strcasecmp (debugenv, "NaN8") == 0) {
		return 3;
	}
	else if (strcasecmp (debugenv, "NaN64") == 0) {
		return 3;
	}
	
	return 0;
}

/*    _ALLOCATE - called by compiled Fortran programs to allocate space
 *                for objects in the allocation list.  The status is
 *                returned in the optional stat_var.
 *      Synopsis
 *              void _ALLOCATE(AllocListType *aloclist,
 *                              _f_int *statvar);
 *              Where
 *                     aloclist - pointer to the allocation list. A header
 *                                word contains a version number and nwords,
 *                                the number of dope vector entries in the
 *                                list.  nwords of addresses of dope vectors
 *                                follow the header word.
 *                      statvar - (optional) pointer to a stat_var.  It is
 *                                set by the library for success or error.
 *                                The routine aborts if not successful and
 *                                statvar is not present.
 *      Return value
 *              A flag indicating success/failure is returned in stat_var. 
 */

void
_ALLOCATE(AllocHeadType *aloclist,
	  _f_int *statvar)
{
	int 		loopcount = 0;	/* icount from aloclist */
	int 		lstat = 0;	/* status variable present */
	int 		errflag = 0;	/* error flag */
	DopeVectorType	*dva;		/* pointer to dope vector */
	int 		i, j;		/* Loop counter */
	int 		iarray=0;	/* array of dv counter */
	int 		bytalign=0; 	/* byte aligned flag */
	long 		nbytes;		/* Number bytes in array */
	long 		fcdleng = 0;	/* fcdlen in array */
	ptrdiff_t	*base;		/* ptr to result array */
	int 		imalocflg = 0;	/* imalloc from aloclist */

	/* Check for allocation debugging */
	if (ps_debug_alloc == -1) {
		ps_debug_alloc = get_debug_alloc_state();
	}

	/* setup counter for number of addresses in allocation list */
	loopcount	= aloclist->icount;
	imalocflg	= aloclist->imalloc;

	/* check for presence of statvar */
	if(statvar != NULL)
		lstat	= 1;

	/* Pick up a dope vector for each item in the allocation list. */
	while(loopcount--) {
		dva	= aloclist->dv[iarray];
		switch(dva->p_or_a) {
			case POINTTR:
			{
				/* set allocation by pointer flag */
				dva->ptr_alloc	= 1;
				break;
			}

			case ALLOC_ARRY:
			{
				/* error if already allocated */
				if(dva->assoc) {
					if(lstat) {
						*statvar = FEALALLO;
						return;
					}
					_lerror (_LELVL_ABORT, FEALALLO);
				}
				break;
			}
		}

		/* if byte-aligned data type, set flag and byte length */
		if (dva->type_lens.type == DVTYPE_ASCII) {
			bytalign	= 1;
#if	defined(_ADDR64) || defined(_WORD32) || defined(__mips) || \
	defined(_LITTLE_ENDIAN)
			nbytes	= _fcdlen (dva->base_addr.charptr);
#else
			/* Use fcdlen on ymp/c90 only if f90 changes since
			 * length of an element is only in the second word,
			 * not in the fcd.  The internal length field is
			 * the length of a character byte, not an element.
			 */
			nbytes	= dva->base_addr.a.el_len >> 3;
#endif
			/* byte length for cptofcd after malloc */
			fcdleng	= nbytes;
		} else if (dva->type_lens.type == DVTYPE_DERIVEDWORD ||
	 	    dva->type_lens.type == DVTYPE_DERIVEDBYTE) {
			/* The length of an element is not in the internal
			 * length field.  It is in the el_len field for
			 * derived type.
			 */
			nbytes	= dva->base_addr.a.el_len >> 3;
		} else
			/* The length of an element is in the internal
			 * length field for noncharacter and nonderived type.
			 */
			nbytes	= dva->type_lens.int_len >> 3;

		/* clear base address */
		base	= (void *) &_zero_entity;

		/* calculate allocation size in bytes */
		for (i = 0; i < dva->n_dim; i++)
			nbytes *= dva->dimension[i].extent;

		/* Allocate size in bytes if not zero.  Zero size is */
		/* legal and should not cause an error. */
		if (nbytes != 0) {
#if	defined(_CRAYT3E)
			/* allocate from symmetric or private heap? */
			if (imalocflg != 0 ) {
				base	= (void *) _shmalloc (nbytes);
			} else
#endif
				base	= (void *) malloc (nbytes);
			/* if no memory assigned, error */
			if (base == NULL) {
				if(lstat) {
					*statvar	= FENOMEMY;
					return;
				}
				_lerror (_LELVL_ABORT, FENOMEMY);
			}
			/* If fortran malloc debugging is on, initialize the memory. */
			
			if (ps_debug_alloc > 0) {
				if (ps_debug_alloc == 1) {
					memset (base, 0, nbytes);
				}
				else if (ps_debug_alloc == 2) {
					unsigned * surrogate = (unsigned *) &(base[0]);
					if (nbytes % sizeof (unsigned) == 0) {
						for (i = 0, j = 0; j < nbytes; i ++, j += sizeof (unsigned)) {
							surrogate[i] = NaN32;
						}
					}
				}
				else if (ps_debug_alloc == 3) {
					uint64 * surrogate = (uint64 *) &(base[0]);
					if (nbytes % sizeof (uint64) == 0) {
						for (i = 0, j = 0; j < nbytes; i ++, j += sizeof (uint64)) {
							surrogate[i] = NaN64;
						}
					}
				}
			}
		}
#ifdef KEY /* Bug 6845 */
# ifdef _DEBUG
                if (ps_debug_alloc > 0) {
		  fprintf(stderr, "allocation.c malloc: %p\n",
		    (void *) base);
		}
# endif /* _DEBUG */
#endif /* KEY Bug 6845 */

		/* Set base address for allocated area.  If character, set */
		/* pointer as fcd.  Clear address if zero allocation. */
		/* Bytlen is nonzero for structures on addr64 machines. */
		if (bytalign)
			dva->base_addr.charptr =
			   _cptofcd( (char *) base, fcdleng);
		else
			dva->base_addr.a.ptr	= base;

		/* set associated/allocated flag in result dope vector,
		 * even for zero-sized allocation. */
		dva->assoc	= 1;

		/* set allocated address in result dope vector */
		dva->orig_base	= dva->base_addr.a.ptr;

		/* set allocated size in bits in result dope vector */
		dva->orig_size	= nbytes << 3;

		iarray++;
	}
	/* set status variable before returning */
	if(lstat)
		*statvar	= errflag;
}

#ifdef KEY /* Bug 6845 */
extern void _DEALLOC(AllocHeadType *);

/*
 * dva		Dope vector describing an allocatable array whose element
 *		type is a derived type containing allocatable components
 * return	Pointer to DopeAllocType, which follows the last actual
 *		dimension (which may be less than MAXDIM) inside the dope
 *		vector
 */
#define DOPE_ALLOC_INFO(dva) ((DopeAllocType *) (dva->dimension + dva->n_dim))

/*
 * dva		Dope vector describing an allocatable array
 * return	Size in bytes of the dope vector
 */
#define DOPE_VECTOR_SIZE(dva) \
    ((sizeof *dva) - (sizeof dva->dimension) + \
    (dva->n_dim * sizeof(struct DvDimen)) + \
    (dva->alloc_cpnt ? \
      (sizeof(DopeAllocType) + \
        alloc_info->n_alloc_cpnt * sizeof(unsigned long)) : \
	0))

/*
 * dva		dope vector describing an array
 * return	number of elements in array
 */
static unsigned long
count_elements(DopeVectorType *dva) {
  unsigned long n_elements = 1;
  int d = 0;
  for (; d < dva->n_dim; d += 1) {
    n_elements *= (unsigned long)
      (dva->dimension[d].extent - dva->dimension[d].low_bound + 1);
    }
  return n_elements;
}

/* Assuming that element type of allocatable array contains components which
 * are themselves allocatable, recursively delete them. We iterate over the
 * elements but recurse through the subobjects, so the stack depth is a
 * function of the depth of the tree of types, not the size of the arrays.
 *
 * I can't find explicit permission in the standard, but both Intel and PGI
 * compilers take the attitude that it's not an error if a component is not
 * allocated, so we do likewise.
 *
 * dva		Dope vector for allocatable array (dva->alloc_cpnt must be 1)
 * version	Version from AllocHeadType
 * imalloc	imalloc flag from AllocHeadType
 */
static void
recursive_dealloc(DopeVectorType *dva, int version, int imalloc) {
  /* Existing code doesn't define this symbolically, sigh */
# define BITSTOBYTES(x) ((x) >> 3)
  DopeAllocType *alloc_info = DOPE_ALLOC_INFO(dva);
  unsigned long n_allocatable_cpnt = alloc_info->n_alloc_cpnt;

  AllocHeadType *alist = alloca((sizeof *alist) +
    (sizeof alist->dv) * (n_allocatable_cpnt - 1));
  memset(alist, 0, sizeof *alist); /* For sanity */
  alist->version = version;
  alist->imalloc = imalloc;

  int n_elements = count_elements(dva);
  char *element = (char *) dva->base_addr.a.ptr;
  int bytes_per_element = BITSTOBYTES(dva->base_addr.a.el_len);
  int e = 0;
  for (; e < n_elements; e += 1) {
    int i = 0;
    alist->icount = 0;
    for (; i < n_allocatable_cpnt; i += 1) {
      DopeVectorType *d = (DopeVectorType *)
	(element + (BITSTOBYTES(alloc_info->alloc_cpnt_offset[i])));
      if (d->assoc) {
	alist->dv[alist->icount++] = d;
      }
    }
    _DEALLOC(alist);
    element += bytes_per_element;
  }
}

/*
 * Assign one allocatable array to another. Assumes the type and rank of src
 * matches the type and rank of dest.
 *
 * src		Dope vector representing an allocatable array
 * dest		Dope vector representing an allocatable array
 */
void
_ASSIGN_ALLOCATABLE(DopeVectorType *dest, DopeVectorType *src, int version,
  int imalloc) {

#ifdef _DEBUG
  /* Current function refers to ps_debug_alloc only inside _DEBUG */
  if (ps_debug_alloc == -1) {
    ps_debug_alloc = get_debug_alloc_state();
  }
#endif /* _DEBUG */

  /* Someday should recognize special case where dest and src shapes match
   * so we don't need to call free and malloc */
  unsigned int src_nelements = count_elements(src);

  /* Deallocate array and any subcomponents */
  AllocHeadType list;
  list.version = version;
  list.imalloc = imalloc;
  list.icount = 1;
  list.dv[0] = dest;
  _DEALLOC(&list);

  /* Make dest dope vector match src; both now temporarily point to same
   * dynamic storage */
  DopeAllocType *alloc_info = DOPE_ALLOC_INFO(src);
  int save_alloc_cpnt = dest->alloc_cpnt;
  memcpy(dest, src, DOPE_VECTOR_SIZE(src));
  /* If source wasn't allocatable, correct the fields that just got clobbered */
  if (src->p_or_a != ALLOC_ARRY) {
    dest->assoc = 1;
    dest->ptr_alloc = 0;
    dest->p_or_a = ALLOC_ARRY;
    dest->a_contig = 1;
    dest->alloc_cpnt = save_alloc_cpnt;
  }

  int el_len_bytes = (src->type_lens.type == DVTYPE_ASCII) ?
    src->base_addr.a.el_len :
    (src->base_addr.a.el_len / 8);
  size_t nbytes = el_len_bytes * src_nelements;

  /* Allocate separate storage for dest */
  if (src->assoc) {
    /* Doesn't seem to be a named constant available for bits-per-byte, sigh */
    dest->base_addr.a.ptr = malloc(nbytes);
    if (dest->base_addr.a.ptr == NULL) {
      _lerror (_LELVL_ABORT, FENOMEMY);
    }
#ifdef KEY /* Bug 6845 */
# ifdef _DEBUG
    if (ps_debug_alloc > 0) {
      fprintf(stderr, "allocation.c malloc: %p\n",
        (void *) dest->base_addr.a.ptr);
    }
# endif /* _DEBUG */
#endif /* KEY Bug 6845 */

    /* Copy all elements */
    if (src->a_contig) {
      memcpy(dest->base_addr.a.ptr, src->base_addr.a.ptr, nbytes);
    }
    else {
      _Copyin(dest->base_addr.a.ptr, src);
    }

    /* If element type is a derived type with allocatable components, then
     * recursively allocate and copy */
    if (src->alloc_cpnt) {
      unsigned int n_alloc_cpnt = alloc_info->n_alloc_cpnt;
      char *src_element = src->base_addr.a.ptr;
      char *dest_element = dest->base_addr.a.ptr;
      int e = 0;
      for (; e < src_nelements; e += 1) {
	int i = 0;
	for (; i < n_alloc_cpnt; i += 1) {
	  unsigned int alloc_cpnt_offset =
	    BITSTOBYTES(alloc_info->alloc_cpnt_offset[i]);
	  DopeVectorType *src_cpnt =
	    (DopeVectorType *) (src_element + alloc_cpnt_offset);
	  DopeVectorType *dest_cpnt =
	    (DopeVectorType *) (dest_element + alloc_cpnt_offset);
	  /* Dest must not point to same dynamically allocated memory as src */
	  dest_cpnt->base_addr.a.ptr = 0;
	  dest_cpnt->assoc = 0;
	  _ASSIGN_ALLOCATABLE(dest_cpnt, src_cpnt, version, imalloc);
	}
      src_element += el_len_bytes;
      dest_element += el_len_bytes;
      }
    }
  }

  else {
    dest->base_addr.a.ptr = 0;
  }

}
#endif /* KEY Bug 6845 */

/*  _DEALLOCATE - called by compiled Fortran programs to deallocate space
 *                for objects in the allocation list.  The status is
 *                returned in the optional stat_var.
 *      Synopsis
 *              void _DEALLOCATE(AllocListType *aloclist,
 *                              _f_int *statvar);
 *              Where
 *                     aloclist - pointer to the allocation list. A header
 *                                word contains a version number and nwords,
 *                                the number of dope vector entries in the
 *                                list.  nwords of addresses of dope vectors 
 *                                follow the header word.
 *                      statvar - (optional) pointer to a stat_var.  It is
 *                                set by the library for success or error.
 *                                The routine aborts if not successful and
 *                                statvar is not present.
 *      Return value
 *              A flag indicating success/failure is returned in stat_var. 
 */

void
_DEALLOCATE(AllocHeadType *aloclist,
	  _f_int *statvar)
{
	int 		bytalign = 0; 	/* byte aligned flag */
	int 		errflag = 0;	/* error flag */
	int 		i;		/* Loop counter */
	int 		iarray = 0;	/* array of dv counter */
	int 		imalocflg = 0;	/* imalloc from aloclist */
	int 		loopcount = 0;	/* icount from aloclist */
	int 		lstat = 0;	/* status variable present */
	long 		fcdleng = 0;	/* fcdlen in array in bytes */
	long 		nsize;		/* Number bits/bytes in array */
	DopeVectorType	*dva;		/* pointer to dope vector */
	ptrdiff_t	*base;		/* ptr to result array */

	/* setup counter for number of addresses in allocation list */
	loopcount	= aloclist->icount;
	imalocflg	= aloclist->imalloc;

	/* check for presence of statvar */
	if(statvar != NULL)
		lstat	= 1;

	/* Pick up a dope vector for each item in the allocation list. */
	while(loopcount--) {
		dva	= aloclist->dv[iarray];

		/* error if:
		 *  1) pointer is not allocated by pointer,
		 *  2) allocatable array is not allocated (associated),
		 *  3) or pointer allocated by pointer is disassociated
		 */
		if (((dva->p_or_a == POINTTR) && (!dva->ptr_alloc)) ||
		((dva->p_or_a == POINTTR) && (dva->ptr_alloc) &&
		     (!dva->assoc)) ||
		    ((dva->p_or_a ==  ALLOC_ARRY) && (!dva->assoc))) {
			/* error if not allocated by pointer */
			if(lstat) {
				*statvar	= FENODEAL;
				return;
			}
			_lerror (_LELVL_ABORT, FENODEAL);
		}

		/* set flag to indicate byte-aligned data type.
		 * set address pointer according to data type. */
		if (dva->type_lens.type == DVTYPE_ASCII) {

			/* character data types */
			bytalign	= 1;
			base = (void *) _fcdtocp (dva->base_addr.charptr);
			fcdleng	= _fcdlen(dva->base_addr.charptr);
			nsize	= fcdleng << 3;

		} else if (dva->type_lens.type == DVTYPE_DERIVEDWORD ||
	 	    dva->type_lens.type == DVTYPE_DERIVEDBYTE) {

			/* Length is only in el_len for derived type */
			nsize	= dva->base_addr.a.el_len;

			/* address is a C pointer with possible offset */
			base	= (void*) dva->base_addr.a.ptr;

		} else {
			/* noncharacter data types */
			base	= (void*) dva->base_addr.a.ptr;
			nsize	= dva->type_lens.int_len;
		}

		/* Calculate size in bits of current array. */
		for (i = 0; i < dva->n_dim; i++)
			nsize *= dva->dimension[i].extent;

		/* error if current size not same as original size */
#ifdef KEY /* Bug 4933 */
		/* If we don't know the original size (for example, because
		 * this is a pointer to a dummy argument and we have no dope
		 * information for the actual argument) then optimistically
		 * assume it is correct. We believe that there's no other
		 * case where the original size could be zero and the actual
		 * size nonzero. */
		if (dva->orig_size && dva->orig_size != nsize ) {
#else
		if (dva->orig_size != nsize ) {
#endif /* KEY Bug 4933 */
			if(lstat) {
				*statvar	= FEDEASIZ;
				return;
			}
			_lerror (_LELVL_ABORT, FEDEASIZ, dva->orig_size, nsize);
#ifdef KEY /* Bug 4933 */
		}
#else
		}
#endif /* KEY Bug 4933 */

#ifdef KEY /* Bug 6845 */
                if (dva->alloc_cpnt) {
		  recursive_dealloc(dva, aloclist->version, aloclist->imalloc);
		}
# ifdef _DEBUG
                if (ps_debug_alloc > 0) {
		  fprintf(stderr, "allocation.c free: %p\n", (void *) base);
		}
# endif /* _DEBUG */
#endif /* KEY Bug 6845 */

		/* free space when size not zero */
		if (nsize != 0)
#if	defined(_CRAYT3E)
			if (imalocflg != 0) {
				/* free from symmetric heap, not private heap */
				_shfree (base);
			} else
#endif
#if	!defined(__mips)
				free (base);
#else		/* else of NOT mips */
		{
			/* check for sma */
			if (!_sma_fortran_deallocate_global)
				free (base);
			else
				_sma_fortran_deallocate_global(base);
		}
#endif		/* endif of NOT mips */

		/* clear fields to indicate unallocated/unassociated */
		dva->assoc	= 0;
		dva->ptr_alloc	= 0;
		/* if byte, set pointer as fcd */
		if (bytalign)
			dva->base_addr.charptr =
			   _cptofcd( (void *) NULL, fcdleng);
		else
			dva->base_addr.a.ptr	= (void *) NULL;
		dva->orig_base	= dva->base_addr.a.ptr;
		dva->orig_size	= 0;

		iarray++;
	}

	/* set status variable before returning */
	if(lstat)
		*statvar	= errflag;
}

/*    _DEALLOC  - called by CF90 compiler only to deallocate space
 *                for objects in the allocation list.  There is no
 *                error returned for this function.
 *      Synopsis
 *              void _DEALLOC(AllocListType *aloclist)
 *              Where
 *                     aloclist - pointer to the allocation list. A header
 *                                word contains a version number and nwords,
 *                                the number of dope vector entries in the
 *                                list.  nwords of addresses of dope vectors
 *                                follow the header word.
 */

void
_DEALLOC(AllocHeadType *aloclist)
{
	int 		loopcount = 0;	/* icount from aloclist */
	int 		lstat = 0;	/* status variable present */
	DopeVectorType	*dva;		/* pointer to dope vector */
	int 		iarray=0;	/* array of dv counter */
	long 		fcdleng = 0;	/* fcdlen in array */
	int 		bytalign = 0; 	/* byte aligned flag */
	ptrdiff_t	*base;		/* ptr to result array */
	int 		imalocflg = 0;	/* imalloc from aloclist */

	/* setup counter for number of addresses in allocation list */
	loopcount	= aloclist->icount;
	imalocflg	= aloclist->imalloc;

	/* Pick up a dope vector for each item in the allocation list. */
	while(loopcount--) {
		dva	= aloclist->dv[iarray];

		/* return if not associated */
		if (!dva->assoc)
			return;
		/* set flag to indicate byte-aligned data type.
		 * set address pointer according to data type. */
		if (dva->type_lens.type == DVTYPE_ASCII ) {
			bytalign	= 1;
			base = (void *) _fcdtocp (dva->base_addr.charptr);
			fcdleng	= _fcdlen(dva->base_addr.charptr);
		} else
			base	= (void*) dva->base_addr.a.ptr;

#ifdef KEY /* Bug 6845 */
                if (dva->alloc_cpnt) {
		  recursive_dealloc(dva, aloclist->version, aloclist->imalloc);
		}
# ifdef _DEBUG
                if (ps_debug_alloc > 0) {
		  fprintf(stderr, "allocation.c free: %p\n", (void *) base);
		}
# endif /* _DEBUG */
#endif /* KEY Bug 6845 */

		/* free space when size not zero */
		if (dva->orig_size != 0) {
#if	defined(_CRAYMPP) && !defined (_CRAYT3E)
			if (_issddptr(base)) {
				extern void _shfree(void *);
				_shfree(_sdd_read_base((void *) base));
			} else
#elif	defined(_CRAYT3E)
			if (imalocflg != 0) {
				/* free from symmetric heap, not private heap */
				_shfree (base);
			} else
#endif
				free (base);
		}

		/* clear fields to indicate unallocated/unassociated */
		dva->assoc	= 0;
		dva->ptr_alloc	= 0;

		/* if byte, set pointer as fcd */
		if (bytalign)
			dva->base_addr.charptr =
			   _cptofcd( (void *) NULL, fcdleng);
		else
			dva->base_addr.a.ptr	= (void *) NULL;

		dva->orig_base	= dva->base_addr.a.ptr;
		dva->orig_size	= 0;

		iarray++;
	}
}

/*    _REALLOC  - called by CF90 compiler only to extend the length of the
 *                allocatable array.  The space allocated for the array 
 *                constructor was not large enough for the actual size
 *                returned from a function call.  The array is a single
 *                dimension array and the dopevector must be updated with
 *                a new extent and possibly a new base address, a new
 *                orig_size and new orig_base.  The routine libc realloc
 *                must be used because part of the array will already be
 *                filled in and that part must be moved if the array cannot
 *                extended in place.
 *      Synopsis
 *              void _REALLOC(DopeVectorType *array,*length)
 *              Where
 *                     array    - pointer to dope vector for one-dimension
 *                                allocatable array to be extended.
 *                     length   - address of new size for the array.  The new
 *                                size is in bits.
 */
void
_REALLOC(DopeVectorType *array,
#if	(defined(__mips) && (_MIPS_SZLONG == 64)) || defined(_LITTLE_ENDIAN)
         _f_int8 *length)
#else
         _f_int *length)
#endif
{
	long 		fcdleng = 0;	/* fcdlen in array */
	int 		bytalign = 0; 	/* byte aligned flag */
	ptrdiff_t	*base;		/* ptr to array */
	long 		newlen; 	/* new byte size for array */
	long 		bitlen; 	/* new bitlen size for array */
	int 		debyteflag = 0; /* derived byte flag for el_len */
	long            oldlen;         /* previous byte size for array */
	int             i, j;

	bitlen	= *length;
	newlen	= bitlen >> 3;
	oldlen  = array->orig_size >> 3;

	/* Check for allocation debugging */
	if (ps_debug_alloc == -1) {
		ps_debug_alloc = get_debug_alloc_state();
	}

	/* set flag to indicate byte-aligned data type.
	 * set address pointer according to data type. */
	if (array->type_lens.type == DVTYPE_ASCII) {
		bytalign	= 1;
		base	= (void *) _fcdtocp (array->base_addr.charptr);
		fcdleng	= _fcdlen(array->base_addr.charptr);
	} else if (array->type_lens.type == DVTYPE_DERIVEDWORD ||
	           array->type_lens.type == DVTYPE_DERIVEDBYTE) {
		base	= (void *) array->base_addr.a.ptr;
		debyteflag	= 1;
	} else
		base	= (void *) array->base_addr.a.ptr;

	/* if empty string from library */
	if (base == (ptrdiff_t *) &_zero_entity)
		base	= (ptrdiff_t *) NULL;

	/* allocate additional space or deallocate space */
	base	= (void *) realloc (base,newlen);
	/* if no memory assigned, error */
	if (base == NULL && newlen != 0)
		_lerror (_LELVL_ABORT, FENOMEMY);
	/* If the array has grown, fill it appropriately. */
	if (ps_debug_alloc > 0 && newlen > oldlen) {		
		if (ps_debug_alloc == 1) {
			memset ((char *) base + oldlen, 0, newlen - oldlen);
		}
		else if (ps_debug_alloc == 2) {
			if ((newlen-oldlen) % sizeof (unsigned) == 0) {
				unsigned * surrogate = (unsigned *) &(base[0]) + (oldlen / sizeof (unsigned));
				for (i = 0, j = 0;
				     j < newlen-oldlen;
				     i ++, j += sizeof (unsigned)) {
					surrogate[i] = NaN32;
				}
			}
		}
		else if (ps_debug_alloc == 3) {
			if ((newlen-oldlen) % sizeof (uint64) == 0) {
				uint64 * surrogate = (uint64 *) &(base[0]) + (oldlen / sizeof (uint64));
				for (i = 0, j = 0;
				     j < newlen - oldlen;
				     i ++, j += sizeof (uint64)) {
					surrogate[i] = NaN64;
				}
			}
		}
	}

	array->assoc	= (newlen == 0) ? 0 : 1;

	/* if byte, set pointer as fcd */
	if (bytalign)
		array->base_addr.charptr =
		    _cptofcd( (char *) base, fcdleng);
	else
		array->base_addr.a.ptr = base;

	array->orig_base	= array->base_addr.a.ptr;
	array->orig_size	= bitlen;
	if (debyteflag)
		array->base_addr.a.el_len	= bitlen;
}

#if	defined(__mips) || defined(_LITTLE_ENDIAN)

/* _F90_ALLOCATE_B - called by compiled Fortran programs to allocate
 *		space for objects in the allocation list.  The status
 *		is returned in the optional stat_var.
 *
 *	Synopsis
 *
 *	void *_F90_ALLOCATE_B(long size, int assoc, int flags, int *statvar)
 *
 *	Where
 *		size - signed size_t-width value with number of bytes to
 *			allocate
 *		assoc - the current association status
 *		flags - bit flags
 *			1 - is a pointer
 *			4 - initialize for debugging with trap_uv
 *		statvar - (optional) pointer to a stat_var.  It is
 *			set by the library for success or error.
 *			The routine aborts if not successful and
 *			statvar is not present.
 *		oldval - if already associated, no new pointer returned.
 *		         if no memory available, return null pointer.
 *		         if zero size, return address of value 1.
 *
 *	Return value
 *		Address of the allocated memory
 */

#define FLAG_POINTER(x) ((x&1)!=0)
#define FLAG_TRAPUV(x) ((x&4)!=0)

void*
_F90_ALLOCATE_B(long size,
		int assoc,
		int flags,
		int *statvar,
		void *oldval)
{
	int 	lstat = 0;	/* status variable present */
	int 	errflag = 0;	/* error flag */
	ptrdiff_t	*base;	/* ptr to result array */
	char	*p;
	size_t	nbytes;
	long	i, j;

	/* Check for allocation debugging */
	if (ps_debug_alloc == -1) {
		ps_debug_alloc = get_debug_alloc_state();
	}

	/* check for presence of statvar */
	if(statvar != NULL)
		lstat	= 1;

	if (!FLAG_POINTER(flags)) {
		if(assoc) {
			if(lstat) {
				*statvar	= FEALALLO;

				/* return the old value unchanged */
				return(oldval);
			}
			_lerror (_LELVL_ABORT, FEALALLO);
		}
	}

#ifdef KEY // bug 8994: if size wraps around and become -ve under -m32, we want
	   // 	malloc to return 0 so we know to set statvar; if 0 is passed
	   //   to malloc, it will not return 0
	nbytes = size;
#else
	nbytes = size > 0 ? size : 0;
#endif

	/* replace the use of address 1 with globals.c entity
	 *   base	= (void *) 1;
	 */
	base	= (void *) &_zero_entity;

	/* Allocate size in bytes if not zero.  Zero size is legal
 	 * and should not cause an error.
	 */

	if (size != 0) {

#if	defined(_LITTLE_ENDIAN)
		base	= (void *) malloc (nbytes);
#else
		/* check for sma */
		if (!_sma_fortran_allocate_global)
			base	= (void *) malloc (nbytes);
		else
			base	= (void *)
				   _sma_fortran_allocate_global(nbytes,lstat);
#endif

		/* if no memory assigned, error */
		if (base == NULL) {
			if(lstat) {
				*statvar	= FENOMEMY;
				return(base);
			}
			_lerror (_LELVL_ABORT, FENOMEMY);
		}

		/* If fortran malloc debugging is on, initialize the memory. */
		
		if (ps_debug_alloc > 0) {
			if (ps_debug_alloc == 1) {
				memset (base, 0, nbytes);
			}
			else if (ps_debug_alloc == 2) {
				unsigned * surrogate = (unsigned *) &(base[0]);
				if (nbytes % sizeof (unsigned) == 0) {
					for (i = 0, j = 0; j < nbytes; i ++, j += sizeof (unsigned)) {
						surrogate[i] = NaN32;
					}
				}
			}
			else if (ps_debug_alloc == 3) {
				uint64 * surrogate = (uint64 *) &(base[0]);
				if (nbytes % sizeof (uint64) == 0) {
					for (i = 0, j = 0; j < nbytes; i ++, j += sizeof (uint64)) {
						surrogate[i] = NaN64;
					}
				}
			}
		}
	}

#ifdef KEY /* Bug 6845 */
# ifdef _DEBUG
	if (ps_debug_alloc > 0) {
	  fprintf(stderr, "allocation.c malloc: %p\n", (void *) base);
	}
# endif /* _DEBUG */
#endif /* KEY Bug 6845 */

	if (FLAG_TRAPUV(flags)) {

		/* Initialize the memory with bad fp values.
		 * The size of a block is 0%4 on irix.
		 */
		for (p = (void *) base, i = 0; i < nbytes; i += 4) {
			*p	= 0xff; ++p;
			*p	= 0xfa; ++p;
			*p	= 0x5a; ++p;
			*p	= 0x5a; ++p;
		}
		/* leave whetever remains uninitialized */
	}

	return (base);
}

#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
void *
_F90_ALLOCATE(long size, int assoc, int flags, int *statvar, void *oldval) {
  return _F90_ALLOCATE_B(size, assoc, flags, statvar, oldval);
}
#else /* defined(BUILD_OS_DARWIN) */
defalias(_F90_ALLOCATE_B, _F90_ALLOCATE);
#endif /* defined(BUILD_OS_DARWIN) */

#endif	/* __mips */
#ifdef _DEBUG

/*
 * This is designed to be called from the debugger to display dope vector
 * information.
 */
void
print_dope_vector(DopeVectorType *dv, FILE *f)
{
  static char *P_OR_A[] = { "NOT_P_OR_A", "POINTTR", "ALLOC_ARRY" };
  if (0 == f)
  {
    f = stderr;
  }

  fprintf(f, "%p: DopeVectorType:\n", (void *) dv);
  fprintf(f, "+%u: ptr=%p\n",
    (unsigned int) (((char *) &dv->base_addr.a.ptr) - (char *) dv),
    dv->base_addr.a.ptr);
  fprintf(f, "+%u: el_len=%lu\n",
    (unsigned int) (((char *) &dv->base_addr.a.el_len) - (char *) dv),
    dv->base_addr.a.el_len);
#ifdef KEY /* Bug 6845 */
  fprintf(f, "assoc, ptr_alloc, p_or_a, a_contig, alloc_cpnt=%d %d %s %d %d\n",
    dv->assoc, dv->ptr_alloc, P_OR_A[dv->p_or_a], dv->a_contig, dv->alloc_cpnt);
#else /* KEY Bug 6845 */
  fprintf(f, "assoc, ptr_alloc, p_or_a, a_contig=%d %d %s %d\n",
    dv->assoc, dv->ptr_alloc, P_OR_A[dv->p_or_a], dv->a_contig);
#endif /* KEY Bug 6845 */
  fprintf(f, "n_dim=%u\n", dv->n_dim);
  fprintf(f, "+%u: type_lens.type/dpflag/kind/int_len/dec_len=%d/%d/%d/%d/%d\n",
    (unsigned int) (((char *)&dv->type_lens) - (char *) dv),
    (int) dv->type_lens.type,
    (int) dv->type_lens.dpflag,
    (int) dv->type_lens.kind_or_star,
    (int) dv->type_lens.int_len,
    (int) dv->type_lens.dec_len);
  fprintf(f, "+%u: orig_base=%p\n",
    (unsigned int) (((char *)&dv->orig_base) - (char *) dv),
    dv->orig_base);
  fprintf(f, "+%u: orig_size=%lu\n",
    (unsigned int) (((char *)&dv->orig_size) - (char *) dv),
    dv->orig_size);
  int i;
  for (i = 0; i < dv->n_dim; i += 1)
  {
    struct DvDimen *dimen  = &(dv->dimension[i]);
    fprintf(f, "+%u: low_bound, extent, stride_mult=%ld %ld %ld\n",
      (unsigned int) (((char*)dimen) - (char *)dv),
      dimen->low_bound, dimen->extent, dimen->stride_mult);
  }
#ifdef KEY /* Bug 6845 */
  if (dv->alloc_cpnt) {
    DopeAllocType *alloc_info = DOPE_ALLOC_INFO(dv);
    unsigned long n_alloc_cpnt = alloc_info->n_alloc_cpnt;
    printf("+%u: n_alloc_cpnt=%lu\n",
      (unsigned int) (((char*)alloc_info) - (char *)dv), n_alloc_cpnt);
    for (i = 0; i < n_alloc_cpnt; i += 1)
    {
      printf("+%u: alloc_cpnt_offset=%lu\n",
        (unsigned int) (((char*)&(alloc_info->alloc_cpnt_offset[i])) -
	  (char *)dv),
	alloc_info->alloc_cpnt_offset[i]);
    }
  }
#endif /* KEY Bug 6845 */
}
#endif /* _DEBUG */
