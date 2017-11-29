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
 * File: omp.h
 * Abstract: the Application Programming Interface of OpenMP
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 *          06/09/2005, updated by Liao Chunhua, Univ. of Houston
 * 
 */

#ifndef _OMP_H
#define _OMP_H

#include <pthread.h>

typedef int omp_int_t;
typedef double omp_wtime_t;

typedef void *omp_lock_t;
typedef void *omp_nest_lock_t;

#ifdef __cplusplus
extern "C"{
#endif

/*
 * Excution Environment Functions
 */
extern void omp_set_num_threads(omp_int_t num);
extern omp_int_t omp_get_num_threads(void);
extern omp_int_t omp_get_max_threads(void);
extern omp_int_t omp_get_thread_num(void);
extern omp_int_t omp_get_num_procs(void);

extern omp_int_t omp_in_parallel(void);

extern void omp_set_dynamic(omp_int_t dynamic);
extern omp_int_t omp_get_dynamic(void);

extern void omp_set_nested(omp_int_t nested);
extern omp_int_t omp_get_nested(void);

/*
 * Lock Functions
 */
extern void omp_init_lock(volatile omp_lock_t *lock);
extern void omp_init_nest_lock(volatile omp_nest_lock_t *lock);

extern void omp_destroy_lock(volatile omp_lock_t *lock);
extern void omp_destroy_nest_lock(volatile omp_nest_lock_t *lock);

extern void omp_set_lock(volatile omp_lock_t *lock);
extern void omp_set_nest_lock(volatile omp_nest_lock_t *lock);

extern void omp_unset_lock(volatile omp_lock_t *lock);
extern void omp_unset_nest_lock(volatile omp_nest_lock_t *lock);

extern int omp_test_lock(volatile omp_lock_t *lock);
extern int omp_test_nest_lock(volatile omp_nest_lock_t *lock);
	
extern omp_wtime_t omp_get_wtick(void);
extern omp_wtime_t omp_get_wtime(void);

#ifdef __cplusplus
}
#endif

#endif /* _OMP_H */
