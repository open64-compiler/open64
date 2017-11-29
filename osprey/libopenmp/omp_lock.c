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
 * File: omp_lock.c
 * Abstract: implementation of OpenMP lock and critical sections
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 *          06/09/2005, updated by Liao Chunhua, Univ. of Houston
 * 
 */
 
#include <stdlib.h>
#include "omp_lock.h"
#include "omp_rtl.h"
#include "omp_sys.h"

extern int __omp_spin_user_lock;
inline void 
__ompc_init_lock (volatile ompc_lock_t *lp)
{
  if (__omp_spin_user_lock == 0)
    pthread_mutex_init(&(lp->lock.mutex_data), NULL);
  else
    pthread_spin_init(&(lp->lock.spin_data), PTHREAD_PROCESS_PRIVATE);
}


inline void 
__ompc_lock (volatile ompc_lock_t *lp)
{
  if (__omp_spin_user_lock == 0)
    pthread_mutex_lock(&(lp->lock.mutex_data));
  else 
    pthread_spin_lock(&(lp->lock.spin_data));
}


inline void 
__ompc_unlock (volatile ompc_lock_t *lp)
{
  if (__omp_spin_user_lock == 0)
    pthread_mutex_unlock(&(lp->lock.mutex_data));
  else 
    pthread_spin_unlock(&(lp->lock.spin_data));
}


inline void 
__ompc_destroy_lock (volatile ompc_lock_t *lp)
{
  if (__omp_spin_user_lock == 0)
    pthread_mutex_destroy(&(lp->lock.mutex_data));
  else 
    pthread_spin_destroy(&(lp->lock.spin_data));
}


inline int 
__ompc_test_lock (volatile ompc_lock_t *lp)
{
  if (__omp_spin_user_lock == 0)
    return (pthread_mutex_trylock(&(lp->lock.mutex_data)) == 0);
  else
    return (pthread_spin_trylock(&(lp->lock.spin_data)) == 0);
}


void 
__ompc_init_nest_lock (volatile ompc_nest_lock_t *lp)
{
  __ompc_init_lock (&lp->lock);
  __ompc_init_lock (&lp->wait);
  lp->count = 0;
}


void 
__ompc_nest_lock (volatile ompc_nest_lock_t *lp)
{
  pthread_t id = pthread_self();
  int nest;

  if( (lp->count > 0) && (lp->thread_id == id) ) {
    nest = 1;
  } else {
  wait_nest_lock:
    __ompc_lock(&lp->wait); /* be blocked here */
    nest = 0;
  }
  __ompc_lock(&lp->lock);
  if(nest) {
    if( lp->count == 0 ) { /* the 'wait' lock be released */
      if(!__ompc_test_lock(&lp->wait)) {
	__ompc_unlock(&lp->lock);
	goto wait_nest_lock;
      }
      lp->thread_id = id;
    } else { /* lp->count > 0 */
      if(lp->thread_id != id) {
	__ompc_unlock(&lp->lock);
	goto wait_nest_lock;
      }
    }
    lp->count++;
  }  else { /* get the 'wait' lock. Assert:( lp->count == 0 ) */
    lp->thread_id = id;
    lp->count = 1;
  }
  __ompc_unlock(&lp->lock);
}


void 
__ompc_nest_unlock (volatile ompc_nest_lock_t *lp)
{
  __ompc_lock (&lp->lock);
  if(lp->count > 0){
    lp->count--;
    if(lp->count == 0){
      __ompc_unlock(&lp->wait);
    }
  }
  __ompc_unlock (&lp->lock);
}


void 
__ompc_destroy_nest_lock (volatile ompc_nest_lock_t *lp)
{
  __ompc_destroy_lock (&lp->wait);
  __ompc_destroy_lock (&lp->lock);
}


int 
__ompc_test_nest_lock (volatile ompc_nest_lock_t *lp)
{
  pthread_t id = pthread_self();
    
  __ompc_lock(&lp->lock);
  if(lp->count > 0){
    if(lp->thread_id == id){
      lp->count++;
    }
    else{
      __ompc_unlock(&lp->lock);
      return 0;
    }
  }
  else{
    if(!__ompc_test_lock(&lp->wait)){
      __ompc_unlock(&lp->lock);
      return 0;
    }
    lp->thread_id = id;
    lp->count = 1;
  }
  __ompc_unlock(&lp->lock);
  return lp->count;
}

/* for Critical directive */
/*Changed by Liao, the work of init lock has been moved to runtime */

inline void
__ompc_critical(int gtid, volatile ompc_lock_t **lck)
{
  if (*lck == NULL) {
    __ompc_lock_spinlock(&_ompc_thread_lock);
    if ((ompc_lock_t*)*lck == NULL){
      // put the shared data aligned with cache line
      volatile ompc_lock_t* new_lock = aligned_malloc(sizeof(ompc_lock_t), CACHE_LINE_SIZE);
      Is_True(new_lock!=NULL, 
	      ("Cannot allocate lock memory for critical"));
      __ompc_init_lock (new_lock);
      *lck = new_lock;
    }
    __ompc_unlock_spinlock(&_ompc_thread_lock);
  }
  __ompc_lock((volatile ompc_lock_t *)*lck);
}

inline void
__ompc_end_critical(int gtid, volatile ompc_lock_t **lck)
{
  __ompc_unlock((volatile ompc_lock_t *)*lck);
}


inline void
__ompc_critical_light(int gtid, volatile ompc_lock_t **lck)
{
  __ompc_spin_lock((void*)lck);
}

inline void
__ompc_end_critical_light(int gtid, volatile ompc_lock_t **lck)
{
  __ompc_spin_unlock((void*)lck);
}
