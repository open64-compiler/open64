/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */

/* Support for procedures in ISO_C_BINDING intrinsic module */

#include <stdint.h>
#include "cray/dopevec.h"

/*
 * Make fptr have the same target as cptr, with the shape of "shape"
 *
 * cptr		type(c_ptr) by reference
 * fptr		Fortran array pointer by reference
 * shape	Fortran rank-1 integer array by reference
 */
void
_C_f_pointera_(void **cptr, DopeVectorType *fptr, int32_t *shape) {

  fptr->base_addr.a.ptr = *cptr;
  fptr->orig_base = *cptr;
  fptr->orig_size = 0; /* Alas, we don't know */

  /* As I read the standard, cptr shouldn't be a null pointer, but other
   * compilers allow this. */
  fptr->assoc = !!*cptr;

  /* We don't actually know whether the target was allocated via a pointer
   * (versus being an allocatable variable, or even a variable which wasn't
   * dynamically allocated at all.) If we implemented association status
   * by means of a list of addresses, rather than by means of bits inside
   * the dope vector, then we would know. The issue is whether the user can
   * pass this pointer to "deallocate". To be safe, we say "yes", although
   * we can't conform completely to the standard (if the user passes the
   * pointer to "deallocate" and the target wasn't dynamically allocated,
   * we may crash inside "free" instead of setting the stat= variable.)
   *
   * The same issue arises in a case like this:
   *
   *     integer, pointer:: pa
   *     integer :: i
   *     allocate(pa)
   *     call s(pa)
   *     call s(i)
   *   contains
   *     subroutine s(a)
   *       integer, target :: a
   *       integer, pointer :: spa
   *       spa => a
   *       deallocate(spa, stat=i)
   *       print *, i .eq. 0
   *     end subroutine s
   *   end
   *
   * The program ought to print "T" then "F", but it is likely to crash
   * instead of printing "F"
   *
   * Note that we don't have a problem if the user tries to deallocate a
   * pointer whose target is an allocatable variable: the standard says the
   * user shall not do that, and doesn't specifically say that the
   * implementation must detect it (though one would like to.)
   */
  fptr->ptr_alloc = 1;
  fptr->p_or_a = POINTTR;
  fptr->a_contig = 1;
  /* fptr->alloc_cpnt,
   * fptr->type_lens: fptr is supposed to be type-compatible with
   * target of cptr, so hopefully these are already set correctly */
  if (fptr->n_dim) {
    /* shape could be assumed-size, so no way to check that its length
     * matches rank of fptr */
    unsigned stride_mult = 1;
    int i = 0;
    for (; i < fptr->n_dim; i += 1) {
      int32_t extent = shape[i];
      fptr->dimension[i].low_bound = 1;
      fptr->dimension[i].extent = extent;
      fptr->dimension[i].stride_mult = stride_mult;
      stride_mult = stride_mult * extent;
    }
  }
}

/*
 * Like _C_f_pointera_, but for scalar pointers
 */
void
_C_f_pointers_(void **cptr, DopeVectorType *fptr) {
  fptr->base_addr.a.ptr = *cptr;
  fptr->orig_base = *cptr;
  fptr->orig_size = 0; /* Alas, we don't know */
  fptr->assoc = !!*cptr;
  fptr->ptr_alloc = 1;
  fptr->p_or_a = POINTTR;
  fptr->a_contig = 1;
}

/* Keep around for backwards compatibility after pathf90 learns to emit "loc"
  operator for this */
void *
_C_loc_(void *p) {
  return p;
  }

/* Keep around for backwards compatibility after pathf90 learns to emit "loc"
  operator for this */
void *
_C_funloc_(void *p) {
  return p;
  }

int
_C_associated_ptr_(void *p, void **q) {
  return q ? (p && (p == *q)) : !!p;
  }

int
_C_associated_funptr_(void *p, void **q) {
  return q ? (p && (p == *q)) : !!p;
  }

