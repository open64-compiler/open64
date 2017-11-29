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
 * File: omp_lock.h
 * Abstract: implementation of OpenMP lock and critical sections
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 * 
 */

#ifndef __omp_lock_included
#define __omp_lock_included

#define ALIGN_SIZE 64

#include <pthread.h>
#include "omp_sys.h"
#include "omp_util.h"

typedef struct {
  pthread_spinlock_t data;
}__attribute__ ((__aligned__(ALIGN_SIZE))) ompc_spinlock_t;

typedef struct {
  int __attribute__ ((__aligned__(ALIGN_SIZE))) flag;
  union{
    pthread_spinlock_t spin_data;
    pthread_mutex_t mutex_data;
  }lock;
}__attribute__ ((__aligned__(ALIGN_SIZE))) ompc_lock_t;

#ifndef __OPENMP_LOCK_TYPE_DEFINED_
#define __OPENMP_LOCK_TYPE_DEFINED_

typedef struct {
   ompc_lock_t      lock, wait;
   pthread_t       thread_id;
   int             count;
} ompc_nest_lock_t;

#endif

static inline void
__ompc_init_spinlock(ompc_spinlock_t *lck_p)
{
  pthread_spin_init(&(lck_p->data),PTHREAD_PROCESS_PRIVATE);
}

static inline void
__ompc_destroy_spinlock(ompc_spinlock_t *lck)
{
  pthread_spin_destroy(&(lck->data));
}

static inline void
__ompc_lock_spinlock(ompc_spinlock_t *lck)
{
  pthread_spin_lock(&(lck->data));
}

static inline void
__ompc_unlock_spinlock(ompc_spinlock_t *lck)
{
  pthread_spin_unlock(&(lck->data));
}

static inline int
__ompc_try_spinlock(ompc_spinlock_t *lck)
{
  return pthread_spin_trylock(&(lck->data));
}

extern void __ompc_init_lock (volatile ompc_lock_t *);
extern void __ompc_lock (volatile ompc_lock_t *);
extern void __ompc_unlock (volatile ompc_lock_t *);
extern void __ompc_destroy_lock (volatile ompc_lock_t *);
extern int __ompc_test_lock (volatile ompc_lock_t *);

extern void __ompc_init_nest_lock (volatile ompc_nest_lock_t *);
extern void __ompc_nest_lock (volatile ompc_nest_lock_t *);
extern void __ompc_nest_unlock (volatile ompc_nest_lock_t *);
extern void __ompc_destroy_nest_lock (volatile ompc_nest_lock_t *);
extern int __ompc_test_nest_lock (volatile ompc_nest_lock_t *);

extern void __ompc_critical(int gtid, volatile ompc_lock_t **lck);
extern void __ompc_end_critical(int gtid, volatile ompc_lock_t **lck);

#endif

