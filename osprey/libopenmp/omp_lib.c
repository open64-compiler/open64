/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*

  OpenMP runtime library to be used in conjunction with Open64 Compiler Suites.

  Copyright (C) 2003 - 2009 Tsinghua University.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
  
  Contact information: HPC Institute, Department of Computer Science and Technology,
  Tsinghua University, Beijing 100084, CHINA, or:

  http://hpc.cs.tsinghua.edu.cn
  
*/

/*
 * File: omp_lib.c
 * Abstract: implementation of OpenMP run-time library subroutines
 *          for OpenMP programmer
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 * 
 */

#include <stdlib.h>

#include "omp_rtl.h"
#include "omp_lock.h" 
#include "omp_util.h"

#include <sys/time.h>

void
__omp_fatal(char *msg)
{
  fprintf(stderr, "OpenMP Run-time Library FATAL error:\n %s\n", msg);
  exit(-1);
}


/*
 * OpenMP standard library function
 */

void
omp_set_num_threads(omp_int_t num)
{
  __ompc_set_num_threads(num);
}

void omp_set_num_threads_(omp_int_t * num)
{
    __ompc_set_num_threads(*num);
}

omp_int_t
omp_get_num_threads(void)
{
  return (omp_int_t)__ompc_get_num_threads();
}

omp_int_t omp_get_num_threads_(void);
#pragma weak omp_get_num_threads_ = omp_get_num_threads

omp_int_t
omp_get_max_threads(void)
{
  return (omp_int_t)__ompc_get_max_threads();
}


omp_int_t omp_get_max_threads_(void);
#pragma weak omp_get_max_threads_ = omp_get_max_threads

omp_int_t
omp_get_thread_num(void)
{
  return (omp_int_t)__ompc_get_local_thread_num();
}


omp_int_t omp_get_thread_num_(void);
#pragma weak omp_get_thread_num_ = omp_get_thread_num

omp_int_t
omp_get_num_procs(void)
{
  return (omp_int_t)__ompc_get_num_procs();
}


omp_int_t omp_get_num_procs_(void);
#pragma weak omp_get_num_procs_ = omp_get_num_procs
omp_int_t
omp_in_parallel(void)
{
  return (omp_int_t)__ompc_in_parallel();
}


omp_int_t omp_in_parallel_(void);
#pragma weak omp_in_parallel_ = omp_in_parallel

void
omp_set_dynamic(omp_int_t dynamic)
{
  __ompc_set_dynamic(dynamic);
}


void omp_set_dynamic_(omp_int_t* dynamic)
{
    __ompc_set_dynamic(*dynamic);
}

omp_int_t
omp_get_dynamic(void)
{
  return (omp_int_t)__ompc_get_dynamic();
}


omp_int_t omp_get_dynamic_(void);
#pragma weak omp_get_dynamic_ = omp_get_dynamic

void
omp_set_nested(omp_int_t nested)
{
  __ompc_set_nested(nested);
}


void omp_set_nested_(omp_int_t* nested)
{
    __ompc_set_nested(*nested);
}

omp_int_t
omp_get_nested(void)
{
  return (omp_int_t)__ompc_get_nested();
}

omp_int_t omp_get_nested_(void);
#pragma weak omp_get_nested_ = omp_get_nested
/*
 * Lock Functions
 */
extern int __omp_spin_user_lock;
void
omp_init_lock(volatile omp_lock_t *lock)
{
  // put the lock aligned to cache line size and
  // malloc a cache line for the lock to avoid
  // false sharing
  ompc_lock_t *tmp_lp;
  tmp_lp = aligned_malloc(sizeof(ompc_lock_t), CACHE_LINE_SIZE);
  Is_True(tmp_lp != NULL, "can not allocate tmp_lp");

  tmp_lp->flag = __omp_spin_user_lock;
  __ompc_init_lock(tmp_lp);
  (*lock) = (omp_lock_t)tmp_lp; 
}


void omp_init_lock_(volatile omp_lock_t *);
#pragma weak omp_init_lock_ = omp_init_lock

void
omp_init_nest_lock(volatile omp_nest_lock_t *lock)
{
  // put the lock aligned to cache line size
  ompc_nest_lock_t * tmp_lp;
  tmp_lp = aligned_malloc(sizeof(ompc_nest_lock_t), CACHE_LINE_SIZE); 
  Is_True(tmp_lp != NULL, "can not allocate tmp_lp");
  __ompc_init_nest_lock(tmp_lp);
  (*lock) = (omp_nest_lock_t)tmp_lp; 
}


void omp_init_nest_lock_(volatile omp_nest_lock_t *);
#pragma weak omp_init_nest_lock_ = omp_init_nest_lock

void
omp_destroy_lock(volatile omp_lock_t *lock)
{
  __ompc_destroy_lock((ompc_lock_t*)(*lock));
  aligned_free((ompc_lock_t*)(*lock)); 
}


void omp_destroy_lock_(volatile omp_lock_t *);
#pragma weak omp_destroy_lock_ = omp_destroy_lock
void
omp_destroy_nest_lock(volatile omp_nest_lock_t *lock)
{
  __ompc_destroy_nest_lock((ompc_nest_lock_t*)(*lock));
  aligned_free((ompc_nest_lock_t*)(*lock)); 
}


void omp_destroy_nest_lock_(volatile omp_nest_lock_t *);
#pragma weak omp_destroy_nest_lock_ = omp_destroy_nest_lock
void
omp_set_lock(volatile omp_lock_t *lock)
{
  __ompc_lock((ompc_lock_t*)(*lock));
}


void omp_set_lock_(volatile omp_lock_t *);
#pragma weak omp_set_lock_ = omp_set_lock
void
omp_set_nest_lock(volatile omp_nest_lock_t *lock)
{
  __ompc_nest_lock((ompc_nest_lock_t*)(*lock));
}


void omp_set_nest_lock_(volatile omp_nest_lock_t *);
#pragma weak omp_set_nest_lock_ = omp_set_nest_lock
void
omp_unset_lock(volatile omp_lock_t *lock)
{
  __ompc_unlock((ompc_lock_t*)(*lock));
}


void omp_unset_lock_(volatile omp_lock_t *);
#pragma weak omp_unset_lock_ = omp_unset_lock
void
omp_unset_nest_lock(volatile omp_nest_lock_t *lock)
{
  __ompc_nest_unlock((ompc_nest_lock_t*)(*lock));
}


void omp_unset_nest_lock_(volatile omp_nest_lock_t *);
#pragma weak omp_unset_nest_lock_ = omp_unset_nest_lock
int
omp_test_lock(volatile omp_lock_t *lock)
{
  return __ompc_test_lock((ompc_lock_t*)(*lock));
}


int omp_test_lock_(volatile omp_lock_t *);
#pragma weak omp_test_lock_ = omp_test_lock
int
omp_test_nest_lock(volatile omp_nest_lock_t *lock){
  return __ompc_test_nest_lock((ompc_nest_lock_t*)(*lock));
}
	

int omp_test_nest_lock_(volatile omp_nest_lock_t *);
#pragma weak omp_test_nest_lock_ = omp_test_nest_lock
/*
 * Timer function
 */
omp_wtime_t
omp_get_wtime(void)
{
  struct timeval tval;

  gettimeofday(&tval, NULL);
  return (omp_wtime_t)( (double)tval.tv_sec + 1.0e-6 * (double)tval.tv_usec );
}

omp_wtime_t omp_get_wtime_(void);
#pragma weak omp_get_wtime_ = omp_get_wtime


omp_wtime_t omp_get_wtime__(void);
#pragma weak omp_get_wtime__ = omp_get_wtime

omp_wtime_t
omp_get_wtick(void)
{
  double t1, t2;
	
  t1 = omp_get_wtime();
  do {
    t2 = omp_get_wtime();
  } while(t1 == t2);

  return (omp_wtime_t)(t2 - t1);
}


omp_wtime_t omp_get_wtick_(void);
#pragma weak omp_get_wtick_ = omp_get_wtick


omp_wtime_t omp_get_wtick__(void);
#pragma weak omp_get_wtick__ = omp_get_wtick
