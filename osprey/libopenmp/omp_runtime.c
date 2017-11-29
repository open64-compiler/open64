/*
 * Copyright (C) 2009, 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * File: omp_runtime.c
 * Abstract: routines for loop scheduling etc.
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 *          06/09/2005, updated by Liao Chunhua, Univ. of Houston
 *          06/20/2007, updated by He Jiangzhou, Tsinghua Univ.
 * 
 */
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "omp_rtl.h"

/* a system level lock, used for malloc in __ompc_get_thdprv ,by Liao*/
ompc_spinlock_t _ompc_thread_lock;

/* RTL API related with schedule */

/* Static schedule type, not including OMP_SCHEDULE specified
 * static schedule, and Ordered schedule with static type is
 * also not included here.
 * The typical framework for Static schedule type is:
 *
 * For STATIC_EVEN(without chunk)
 * 	__ompc_static_init(...); 
 * 	__adjust_upper_bound__;
 *	__do_my_chunk__;
 *	__ompc_static_fini(...);
 *
 * For STATIC(with a chunk)
 * 	__ompc_static_init(...);
 * 	while(__more_chunk_for_me__)
 * 	{
 * 		__adjust_upper_bound__;
 * 		__do_current_chunk__;
 * 		chunk_upper += *pstride;
 * 		chunk_lower += *pstride;
 * 	}
 * 	__ompc_static_fini(...);
 *
 * Warning: current implementation doesn't return a right
 * plastiter flag, user should infer the last iteration case
 * by themselves.
 */

/* do we should guarantee that every iteration we
 * returned is really a legal one? Consider the following example:
 *
 * 	chunk = 7; thread = 4; stride = 3; lower = 1; upper = 2100;
 * 	iteration space:
 * 	1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31 ...
 *	thread 0: (1, 4, 7)
 *	thread 1: (8, 11,14) wrong!
 *	thread 2: (15,18,21) wrong!
 *	thread 3: (22,25,28)
 *
 * 	No longer a probblem
 */

extern volatile int __omp_level_1_exit_count;
void
__ompc_static_init_4 (omp_int32 global_tid, omp_sched_t schedtype,
		      omp_int32 *plower, 
		      omp_int32 *pupper, omp_int32 *pstride, 
		      omp_int32 incr, omp_int32 chunk) 
{
  omp_int32 my_lower, my_upper;
  omp_int32 block_size;
  omp_int32 team_size;
  omp_int32 trip_count;
  omp_int32 adjustment;
  omp_int32 stride;
  omp_v_thread_t *p_vthread;  


  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL) {
    /* in sequential part*/
    block_size = (incr > 0) ? (*pupper - *plower + 1) :
      (*pupper - *plower - 1);
    /* plower, pupper are not changed*/
    *pstride = block_size;
    //*plastiter = 1;
    return;
  } 

  p_vthread  = __ompc_get_v_thread_by_num(global_tid);
  team_size = p_vthread->team_size;
  if (team_size == 1) {
    /* single thread schdule*/
    block_size = (incr > 0) ? (*pupper - *plower + 1) :
      (*pupper - *plower - 1);
    /* plower, pupper are not changed*/
    *pstride = block_size;
    //*plastiter = 1;
    return;
  }

  /* Maybe we should make sure that:
   * When incr < 0, *plower > *pupper.
   */
  /* TODO:The schedule algorithm should be carefully tuned*/
  if (schedtype == OMP_SCHED_STATIC_EVEN) {
    /* What if the iteration can not be evenly distributed?*/
    trip_count = (*pupper - *plower) / incr + 1;
		
    adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
    block_size = (trip_count / team_size + adjustment) * incr;
    stride = (trip_count / team_size + adjustment + 1) * incr;

    my_lower = *plower + global_tid * stride;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;
    /* For Guide, the *plastiter is not set*/
    return;
  } else { /* OMP_SCHED_STATIC*/
    Is_Valid( chunk > 0, ("chunk size must be a positive number"));

    block_size = (chunk - 1) * incr;
    stride = chunk * incr;
    my_lower = *plower + global_tid * stride;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = stride * team_size;
    /* For Guide, the *plastiter is not set for STATIC*/
    return;
  }
}



void
__ompc_static_init_8 (omp_int32 global_tid, omp_sched_t schedtype,
		      /*  omp_int32 *plastiter,*/ omp_int64 *plower, 
		      omp_int64 *pupper, omp_int64 *pstride, 
		      omp_int64 incr, omp_int64 chunk) 
{
  omp_int64 my_lower, my_upper;
  omp_int64 block_size;
  omp_int64 team_size;
  omp_int64 trip_count;
  omp_int64 adjustment;
  omp_int64 stride;
  omp_v_thread_t *p_vthread;  

  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL) {
    /* in sequential part*/
    block_size = (incr > 0) ? (*pupper - *plower + 1) :
      (*pupper - *plower - 1);
    /* plower, pupper are not changed*/
    *pstride = block_size;
    //*plastiter = 1;
    return;
  } 

  p_vthread  = __ompc_get_v_thread_by_num(global_tid);
  team_size = p_vthread->team_size;
  if (team_size == 1) {
    /* single thread schdule*/
    block_size = (incr > 0) ? (*pupper - *plower + 1) :
      (*pupper - *plower - 1);
    /* plower, pupper are not changed*/
    *pstride = block_size;
    //*plastiter = 1;
    return;
  }

  /* Maybe we should make sure that:
   * When incr < 0, *plower > *pupper.
   */
  /* TODO:The schedule algorithm should be carefully tuned*/
  if (schedtype == OMP_SCHED_STATIC_EVEN) {
    /* What if the iteration can not be evenly distributed?*/
    trip_count = (*pupper - *plower) / incr + 1;
		
    adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
    block_size = (trip_count / team_size + adjustment) * incr;
    stride = (trip_count / team_size + adjustment + 1) * incr;

    my_lower = *plower + global_tid * stride;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;
    /* For Guide, the *plastiter is not set*/
    return;
  } else { /* OMP_SCHED_STATIC*/
    Is_Valid( chunk > 0, ("chunk size must be a positive number"));

    block_size = (chunk - 1) * incr;
    stride = chunk * incr;
    my_lower = *plower + global_tid * stride;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = stride * team_size;
    /* For Guide, the *plastiter is not set for STATIC*/
    return;
  }
}						      

void 
__ompc_static_fini (omp_int32 global_tid) 
{
  /* Do nothing at current time*/
}

/* return 1 if it's ordered schedule, else
 * return 0;
 */
int
__ompc_is_ordered (omp_sched_t schedtype)
{
  switch(schedtype)
    {
    case OMP_SCHED_STATIC_EVEN:
    case OMP_SCHED_STATIC:
    case OMP_SCHED_DYNAMIC:
    case OMP_SCHED_GUIDED:
    case OMP_SCHED_RUNTIME:
    case OMP_SCHED_UNKNOWN:
      return 0;
      break;
    case OMP_SCHED_ORDERED_STATIC_EVEN:
    case OMP_SCHED_ORDERED_STATIC:
    case OMP_SCHED_ORDERED_DYNAMIC:
    case OMP_SCHED_ORDERED_GUIDED:
    case OMP_SCHED_ORDERED_RUNTIME:
      return 1;
      break;
    default:
      return 0;
    }
}

/* Other schedule type, including dynamic, guided,
 * runtime, OMP_SCHEDULE specified
 * static schedule, and Ordered schedule types.
 * The typical framework for Static schedule type is:
 *
 * 	__ompc_scheduler_init(...);
 * 	while(__ompc_schedule_next(...))
 * 	{
 * 		__adjust_upper_bound__;
 * 		__do_current_chunk__;
 * 	}
 * 	__ompc_scheduler_fini(...);
 *
 */
/* Maybe we should provide an unique implementation of the scheduler,
 * rather than 4/8*/
void 
__ompc_scheduler_init_4 (omp_int32 global_tid, omp_sched_t schedtype,
			 omp_int32 lower, omp_int32 upper,
			 omp_int32 stride, omp_int32 chunk) 
{
  omp_team_t     *p_team;
  omp_v_thread_t *p_vthread;

  /* TODO: The validity of the parameters should be checked here*/
  if (schedtype == OMP_SCHED_RUNTIME) {
    /* The logic is still not complete*/
    schedtype = __omp_rt_sched_type;
    chunk = __omp_rt_sched_size;
		
  } else if (schedtype == OMP_SCHED_ORDERED_RUNTIME) {
    schedtype = __omp_rt_sched_type + OMP_SCHED_ORDERED_GAP;
    chunk = __omp_rt_sched_size;
  }

  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL) {
    /* Need a place to hold the information*/
    __omp_root_team.loop_lower_bound = lower;
    __omp_root_team.loop_upper_bound = upper;
    __omp_root_team.loop_increament = stride;

    __omp_root_team.schedule_count = 0;
    return;
  } else if (__omp_exe_mode & OMP_EXE_MODE_NORMAL) {
    p_team = &__omp_level_1_team_manager;
    p_vthread = &__omp_level_1_team[global_tid];
  } else {
    p_vthread = __ompc_get_v_thread_by_num(global_tid);
    p_team  = p_vthread->team;
  }
  p_vthread->schedule_count = 0;

  if (p_team->team_size == 1) {
    p_team->loop_lower_bound = lower;
    p_team->loop_upper_bound = upper;
    p_team->loop_increament = stride;
    p_team->schedule_count = 0;
    if (__ompc_is_ordered(schedtype))
      p_team->ordered_count = 0;

    /* TODO: all set ready?*/
    return;

  }

  /* Warning: Does threads in the same team request for different 
   * loop scheduling and thus cause the scheduler collision?
   */

  p_vthread->loop_count++;

  __ompc_lock_spinlock(&(p_team->schedule_lock));
  if (p_team->loop_count >= p_vthread->loop_count) {
    /* We assume that the initialization has already OK */
    __ompc_unlock_spinlock(&(p_team->schedule_lock));
    return;
  } else {
    /* The first one call schedule_init do the initialization work */
    /* Initializing */
    p_team->loop_lower_bound = lower;
    p_team->loop_upper_bound = upper;
    p_team->loop_increament = stride;
    p_team->schedule_type = schedtype;
    p_team->chunk_size = chunk;
    p_team->schedule_count = 0;
    if (__ompc_is_ordered(schedtype))
      p_team->ordered_count = 0;
    /* Initialization finished */
    p_team->loop_count++;

    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    return;
  }
}         


void
__ompc_scheduler_init_8 (omp_int32 global_tid, omp_sched_t schedtype,
			 omp_int64 lower, omp_int64 upper,
			 omp_int64 stride, omp_int64 chunk) 
{
  omp_team_t     *p_team;
  omp_v_thread_t *p_vthread;

  /* TODO: The validity of the parameters should be checked here*/
  if (schedtype == OMP_SCHED_RUNTIME) {
    /* The logic is still not complete*/
    schedtype = __omp_rt_sched_type;
    chunk = __omp_rt_sched_size;
  } else if (schedtype == OMP_SCHED_ORDERED_RUNTIME) {
    schedtype = __omp_rt_sched_type + OMP_SCHED_ORDERED_GAP;
    chunk = __omp_rt_sched_size;
  }

  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL) {
    /* Need a place to hold the information*/
    __omp_root_team.loop_lower_bound = lower;
    __omp_root_team.loop_upper_bound = upper;
    __omp_root_team.loop_increament = stride;

    __omp_root_team.schedule_count = 0;
    return;
  } else if (__omp_exe_mode & OMP_EXE_MODE_NORMAL) {
    p_team = &__omp_level_1_team_manager;
    p_vthread = &__omp_level_1_team[global_tid];
  } else {
    p_vthread = __ompc_get_v_thread_by_num(global_tid);
    p_team  = p_vthread->team;
  }
  p_vthread->schedule_count = 0;

  if (p_team->team_size == 1) {
    p_team->loop_lower_bound = lower;
    p_team->loop_upper_bound = upper;
    p_team->loop_increament = stride;
    p_team->schedule_count = 0;
    if (__ompc_is_ordered(schedtype))
      p_team->ordered_count = 0;

    /* TODO: all set ready?*/
    return;

  }

  /* Warning: Does threads in the same team request for different 
   * loop scheduling and thus cause the scheduler collision?
   */

  p_vthread->loop_count++;

  __ompc_lock_spinlock(&(p_team->schedule_lock));
  if (p_team->loop_count >= p_vthread->loop_count) {
    /* We assume that the initialization has already OK */
    __ompc_unlock_spinlock(&(p_team->schedule_lock));
    return;
  } else {
    /* The first one call schedule_init do the initialization work */
    /* Initializing */
    p_team->loop_lower_bound = lower;
    p_team->loop_upper_bound = upper;
    p_team->loop_increament = stride;
    p_team->schedule_type = schedtype;
    p_team->chunk_size = chunk;
    p_team->schedule_count = 0;
    if (__ompc_is_ordered(schedtype))
      p_team->ordered_count = 0;
    /* Initialization finished */
    p_team->loop_count++;

    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    return;
  }
}                              


/* bugs: do we should guarantee that every iteration we
 * returned is really a legal one? Consider the following example:
 *
 * 	chunk = 7; thread = 4; stride = 3; lower = 1; upper = 2100;
 * 	iteration space:
 * 	1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31 ...
 *	thread 0: (1, 4, 7)
 *	thread 1: (8, 11,14) wrong!
 *	thread 2: (15,18,21) wrong!
 *	thread 3: (22,25,28)
 *
 *  No longer a problem
 */
omp_int32 __ompc_schedule_next_4 (omp_int32 global_tid, 
				  omp_int32 *plower, 
				  omp_int32 *pupper, omp_int32 *pstride) 
{
  omp_team_t	*p_team;
  omp_v_thread_t  *p_vthread;
  omp_int32	team_size;
  omp_int32	trip_count;
  omp_int32	adjustment;
  omp_int32	block_size;
  omp_int32	stride;
  omp_int32	chunk;
  omp_int32	incr;
  omp_int32	my_lower, my_upper;
  omp_int32	global_lower, global_upper;
  omp_int32	my_trip, schedule_count;
  float		trip_flag;

  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL) {
    /*Judge whether there are more iterations*/
    if ( __omp_root_team.schedule_count != 0) {
      /* No more iterations */
      return 0;
    }

    __omp_root_team.schedule_count = 1;

    *plower = __omp_root_team.loop_lower_bound;
    *pupper = __omp_root_team.loop_upper_bound;
    /* Warning: Don't know how pstride should be properly set*/
    *pstride = __omp_root_team.loop_increament;
    //*plastiter = 1;
    __omp_root_v_thread.ordered_count = 0;

    return 1;
  }

  if (__omp_exe_mode & OMP_EXE_MODE_NORMAL) {
    p_vthread = &__omp_level_1_team[global_tid];
    p_team = &__omp_level_1_team_manager;
  } else {
    p_vthread = __ompc_get_v_thread_by_num(global_tid);
    p_team = p_vthread->team;
  }

  team_size = p_team->team_size;
  if (team_size == 1) {
    /* Single thread team running: for sequentialized nested team*/
    /*Judge whether there are more iterations*/
    if (p_team->schedule_count != 0) {
      /* No more iterations*/
      return 0;
    }
    p_team->schedule_count = 1;

    *plower = p_team->loop_lower_bound;
    *pupper = p_team->loop_upper_bound;
    /* Warning: Don't know how pstride should be properly set*/
    *pstride = p_team->loop_increament;
    p_vthread->ordered_count = 0;

    return 1;
  }

  /* Normal multi-thread multi-time schedule*/
  switch (p_team->schedule_type) {
  case OMP_SCHED_STATIC_EVEN:
    /* specified by OMP_SCHEDULE */
    if (p_vthread->schedule_count != 0) {
      /* no more iteration*/
      return 0;
    }
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_count =  (global_upper - global_lower) / incr + 1;
		
    adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
    block_size = (trip_count / team_size + adjustment) * incr;
    stride = (trip_count / team_size + adjustment + 1) * incr;

    my_lower = global_lower + global_tid * stride;
    my_upper = my_lower + block_size;
    *plower = my_lower;
    *pupper = my_upper;

    *pstride = incr;

    p_vthread->schedule_count = 1;
    /* Need to set plastiter right*/
    if (trip_count >= team_size) {
      return 1;
    } else {
      if (global_tid <= (trip_count - 1))
	return 1;
      else
	return 0;
    } 

    return 1;
    break;
  case OMP_SCHED_STATIC:
    /* specified by OMP_SCHEDULE */
    /* Temporary implementation: use STATIC_EVEN schedule*/
    /* fix RUNTIME assigned STATIC schedule*/
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    chunk = p_team->chunk_size;
    /* Warning, the upper, lower, and incr should be valid*/
    trip_count =  (global_upper - global_lower) / incr + 1;

    block_size = (chunk - 1) * incr;
    stride = chunk * incr;


    my_trip = p_vthread->schedule_count * chunk * team_size 
      + global_tid * chunk + 1;

    if ( my_trip > trip_count ) {
      /* No more iterations*/
      return 0;
    }
    adjustment = p_vthread->schedule_count * stride * team_size;
    p_vthread->schedule_count += 1;

    my_lower = global_lower + global_tid * stride + adjustment;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;

    *pstride = incr;
    return 1;
    break;
  case OMP_SCHED_GUIDED:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;
    trip_count = (global_upper - global_lower) / incr ;

    if ( trip_flag < 0) {
      __ompc_unlock_spinlock(&(p_team->schedule_lock));
      /* No more iterations */
      return 0;
    }

    trip_count += 1;
    my_lower = global_lower;
    block_size = trip_count / (2 * team_size);
    chunk = p_team->chunk_size;
    if (block_size > chunk) 
      chunk = block_size;
    my_upper = my_lower + (chunk - 1) * incr;
    p_team->loop_lower_bound = my_lower + chunk * incr;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;
    break;

  case OMP_SCHED_DYNAMIC:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;
    trip_count = (global_upper - global_lower) / incr;
    /* Need to fix the trip_count for trip_count = 1 case */

    if ( trip_flag < 0) {
      __ompc_unlock_spinlock(&(p_team->schedule_lock));
      /* No more iterations */
      return 0;
    }

    trip_count += 1;
    my_lower = global_lower;
    chunk = p_team->chunk_size;
    my_upper = my_lower + (chunk - 1) * incr;
    p_team->loop_lower_bound = my_lower + chunk * incr;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;
    break;
  case OMP_SCHED_ORDERED_STATIC_EVEN:
    /* specified by OMP_SCHEDULE */
    if (p_vthread->schedule_count != 0) {
      /* no more iteration*/
      return 0;
    }
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_count =  (global_upper - global_lower) / incr + 1;
		
    adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
    block_size = (trip_count / team_size + adjustment) * incr;
    stride = (trip_count / team_size + adjustment + 1) * incr;

    my_lower = global_lower + global_tid * stride;
    my_upper = my_lower + block_size;
    *plower = my_lower;
    *pupper = my_upper;
			
    *pstride = incr;

    p_vthread->schedule_count = 1;
    p_vthread->ordered_count = global_tid;
    p_vthread->rest_iter_count = (my_upper - my_lower) / incr + 1;
    /* Need to set plastiter right*/
    if (trip_count >= team_size) {
      return 1;
    } else {
      if (global_tid <= (trip_count - 1))
	return 1;
      else
	return 0;
    }

    return 1;
    break;
  case OMP_SCHED_ORDERED_STATIC:
    /* specified by OMP_SCHEDULE */
    /* Temporary implementation: use STATIC_EVEN schedule*/
    /* fix RUNTIME assigned STATIC schedule*/
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    chunk = p_team->chunk_size;
    /* Warning, the upper, lower, and incr should be valid*/
    trip_count =  (global_upper - global_lower) / incr + 1;

    block_size = (chunk - 1) * incr;
    stride = chunk * incr;

    /* Determine whether there are more iterations*/

			
    my_trip = p_vthread->schedule_count * chunk * team_size 
      + global_tid * chunk + 1;

    if ( my_trip > trip_count ) {
      /* No more iterations*/
      return 0;
    }
    adjustment = p_vthread->schedule_count * stride * team_size;
    p_vthread->ordered_count = p_vthread->schedule_count * team_size 
      + global_tid;
    p_vthread->schedule_count += 1;
    p_vthread->rest_iter_count = chunk;

    my_lower = global_lower + global_tid * stride + adjustment;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;

    *pstride = incr;
    return 1;

    break;
  case OMP_SCHED_ORDERED_DYNAMIC:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    schedule_count = p_team->schedule_count;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;

    if ( trip_flag < 0) {
      __ompc_unlock_spinlock(&(p_team->schedule_lock));
      /* No more iterations */
      return 0;
    }
    my_lower = global_lower;
    chunk = p_team->chunk_size;

    p_team->loop_lower_bound = my_lower + chunk * incr;
    p_team->schedule_count++;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    trip_count = (global_upper - global_lower) / incr + 1;
    my_upper = my_lower + (chunk - 1) * incr;

    p_vthread->ordered_count = schedule_count;
    p_vthread->rest_iter_count = chunk;

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;
    break;
  case OMP_SCHED_ORDERED_GUIDED:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;
    trip_count = (global_upper - global_lower) / incr ;
    schedule_count = p_team->schedule_count;

    if ( trip_flag < 0)
      {
	__ompc_unlock_spinlock(&(p_team->schedule_lock));
	/* No more iterations */
	return 0;
      }
    trip_count += 1;
    my_lower = global_lower;
    block_size = trip_count / team_size;
    chunk = p_team->chunk_size;
    if (block_size > chunk) 
      chunk = block_size;
    my_upper = my_lower + (chunk - 1) * incr;
    p_team->loop_lower_bound = my_lower + chunk * incr;
    p_team->schedule_count +=1;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    p_vthread->ordered_count = schedule_count;
    p_vthread->rest_iter_count = chunk;

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;
    break;
  default:
    /* runtime schedule type should have been resolved yet*/
    Not_Valid(" unknown schedule type specified");
  }

  return 0;
}


omp_int32 __ompc_schedule_next_8 (omp_int32 global_tid, 
				  /*	omp_int64 *plastiter, */ omp_int64 *plower, 
				  omp_int64 *pupper, omp_int64 *pstride) 
{
  omp_team_t	*p_team;
  omp_v_thread_t  *p_vthread;
  omp_int32	team_size;
  omp_int64	trip_count;
  omp_int64	adjustment;
  omp_int64	block_size;
  omp_int64	stride;
  omp_int64	chunk;
  omp_int64	incr;
  omp_int64	my_lower, my_upper;
  omp_int64	global_lower, global_upper;
  omp_int64	my_trip, schedule_count;
  float		  trip_flag;
  
  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL) {
    /*Judge whether there are more iterations*/
    if ( __omp_root_team.schedule_count != 0) {
      /* No more iterations */
      return 0;
    }
    __omp_root_team.schedule_count = 1;
    
    *plower = __omp_root_team.loop_lower_bound;
    *pupper = __omp_root_team.loop_upper_bound;
    /* Warning: Don't know how pstride should be properly set*/
    *pstride = __omp_root_team.loop_increament;
    //*plastiter = 1;
    __omp_root_v_thread.ordered_count = 0;
    /* no need to schedule anymore iterations*/
    return 0;
  }

  if (__omp_exe_mode & OMP_EXE_MODE_NORMAL) {
    p_vthread = &__omp_level_1_team[global_tid];
    p_team = &__omp_level_1_team_manager;
  } else {
    p_vthread = __ompc_get_v_thread_by_num(global_tid);
    p_team = p_vthread->team;
  }

  team_size = p_team->team_size;
  if (team_size == 1) {
    /* Single thread team running: for sequentialized nested team*/
    /* Judge whether there are more iterations*/
    if (p_team->schedule_count != 0) {
      /* No more iterations*/
      return 0;
    }
    p_team->schedule_count = 1;

    *plower = p_team->loop_lower_bound;
    *pupper = p_team->loop_upper_bound;
    /* Warning: Don't know how pstride should be properly set*/
    *pstride = p_team->loop_increament;
    p_vthread->ordered_count = 0;

    return 0;
  }

  /* Normal multi-thread multi-time schedule*/
  switch (p_team->schedule_type) {
  case OMP_SCHED_STATIC_EVEN:
    /* specified by OMP_SCHEDULE */
    if (p_vthread->schedule_count != 0) {
      /* no more iteration*/
      return 0;
    }
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_count =  (global_upper - global_lower) / incr + 1;
		
    adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
    block_size = (trip_count / team_size + adjustment) * incr;
    stride = (trip_count / team_size + adjustment + 1) * incr;

    my_lower = global_lower + global_tid * stride;
    my_upper = my_lower + block_size;
    *plower = my_lower;
    *pupper = my_upper;

    *pstride = incr;

    p_vthread->schedule_count = 1;
    /* Need to set plastiter right*/
    if (trip_count >= team_size) {
      return 1;
    } else {
      if (global_tid <= (trip_count - 1))
	return 1;
      else
	return 0;
    } 

    return 1;
    break;
  case OMP_SCHED_STATIC:
    /* specified by OMP_SCHEDULE */
    /* Temporary implementation: use STATIC_EVEN schedule*/
    /* fix RUNTIME assigned STATIC schedule*/
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    chunk = p_team->chunk_size;
    /* Warning, the upper, lower, and incr should be valid*/
    trip_count =  (global_upper - global_lower) / incr + 1;

    block_size = (chunk - 1) * incr;
    stride = chunk * incr;

    /* Determine whether there are more iterations*/

			
    my_trip = p_vthread->schedule_count * chunk * team_size 
      + global_tid * chunk + 1;

    if ( my_trip > trip_count ) {
      /* No more iterations*/
      return 0;
    }
    adjustment = p_vthread->schedule_count * stride * team_size;
    p_vthread->schedule_count += 1;

    my_lower = global_lower + global_tid * stride + adjustment;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;

    *pstride = incr;
    return 1;
    break;
  case OMP_SCHED_GUIDED:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;
    trip_count = (global_upper - global_lower) / incr ;

    if ( trip_flag < 0) {
      __ompc_unlock_spinlock(&(p_team->schedule_lock));
      /* No more iterations */
      return 0;
    }
    trip_count += 1;
    my_lower = global_lower;
    block_size = trip_count / (2 * team_size);
    chunk = p_team->chunk_size;
    if (block_size > chunk) 
      chunk = block_size;
    my_upper = my_lower + (chunk - 1) * incr;
    p_team->loop_lower_bound = my_lower + chunk * incr;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;

    break;
  case OMP_SCHED_DYNAMIC:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;
    trip_count = (global_upper - global_lower) / incr;

    if ( trip_flag < 0) {
      __ompc_unlock_spinlock(&(p_team->schedule_lock));
      /* No more iterations */
      return 0;
    }
    trip_count += 1;
    my_lower = global_lower;
    chunk = p_team->chunk_size;
    my_upper = my_lower + (chunk - 1) * incr;
    p_team->loop_lower_bound = my_lower + chunk * incr;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;

    break;
  case OMP_SCHED_ORDERED_STATIC_EVEN:
    /* specified by OMP_SCHEDULE */
    if (p_vthread->schedule_count != 0) {
      /* no more iteration*/
      return 0;
    }
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_count =  (global_upper - global_lower) / incr + 1;
		
    adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
    block_size = (trip_count / team_size + adjustment) * incr;
    stride = (trip_count / team_size + adjustment + 1) * incr;

    my_lower = global_lower + global_tid * stride;
    my_upper = my_lower + block_size;
    *plower = my_lower;
    *pupper = my_upper;

    *pstride = incr;

    p_vthread->schedule_count = 1;
    p_vthread->ordered_count = global_tid;
    p_vthread->rest_iter_count = (my_upper - my_lower) / incr + 1;
    /* Need to set plastiter right*/
    if (trip_count >= team_size) {
      return 1;
    } else {
      if (global_tid <= (trip_count - 1))
	return 1;
      else
	return 0;
    }

    return 1;
    break;
  case OMP_SCHED_ORDERED_STATIC:
    /* specified by OMP_SCHEDULE */
    /* Temporary implementation: use STATIC_EVEN schedule*/
    /* fix RUNTIME assigned STATIC schedule*/
    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    chunk = p_team->chunk_size;
    /* Warning, the upper, lower, and incr should be valid*/
    trip_count =  (global_upper - global_lower) / incr + 1;

    block_size = (chunk - 1) * incr;
    stride = chunk * incr;

    /* Determine whether there are more iterations*/

    my_trip = p_vthread->schedule_count * chunk * team_size 
      + global_tid * chunk + 1;

    if ( my_trip > trip_count ) {
      /* No more iterations*/
      return 0;
    }
    adjustment = p_vthread->schedule_count * stride * team_size;
    p_vthread->ordered_count = p_vthread->schedule_count * team_size 
      + global_tid;
    p_vthread->schedule_count += 1;
    p_vthread->rest_iter_count = chunk;

    my_lower = global_lower + global_tid * stride + adjustment;
    my_upper = my_lower + block_size;

    *plower = my_lower;
    *pupper = my_upper;

    *pstride = incr;
    return 1;

    break;
  case OMP_SCHED_ORDERED_DYNAMIC:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    schedule_count = p_team->schedule_count;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;
    trip_count = (global_upper - global_lower) / incr;

    if ( trip_flag < 0) {
      __ompc_unlock_spinlock(&(p_team->schedule_lock));
      /* No more iterations */
      return 0;
    }
    trip_count += 1;
    my_lower = global_lower;
    chunk = p_team->chunk_size;
    my_upper = my_lower + (chunk - 1) * incr;
    p_team->loop_lower_bound = my_lower + chunk * incr;
    p_team->schedule_count += 1;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    p_vthread->ordered_count = schedule_count;
    p_vthread->rest_iter_count = chunk;

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;

    break;
  case OMP_SCHED_ORDERED_GUIDED:
    __ompc_lock_spinlock(&(p_team->schedule_lock));

    global_lower = p_team->loop_lower_bound;
    global_upper = p_team->loop_upper_bound;
    incr = p_team->loop_increament;
    trip_flag = (global_upper - global_lower) * 1.0 / (float)incr;
    trip_count = (global_upper - global_lower) / incr ;
    schedule_count = p_team->schedule_count;

    if ( trip_flag < 0) {
      __ompc_unlock_spinlock(&(p_team->schedule_lock));
      /* No more iterations */
      return 0;
    }
    trip_count += 1;
    my_lower = global_lower;
    block_size = trip_count / team_size;
    chunk = p_team->chunk_size;
    if (block_size > chunk) 
      chunk = block_size;
    my_upper = my_lower + (chunk - 1) * incr;
    p_team->loop_lower_bound = my_lower + chunk * incr;
    p_team->schedule_count +=1;
    p_vthread->rest_iter_count = chunk;
			
    __ompc_unlock_spinlock(&(p_team->schedule_lock));

    p_vthread->ordered_count = schedule_count;

    *plower = my_lower;
    *pupper = my_upper;
    *pstride = incr;
    return 1;

    break;
  default:
    /* runtime schedule type should have been resolved yet*/
    Not_Valid(" unknown schedule type specified");
  }

  return 0;
}

/* Must be called after the schedule*/
void
__ompc_scheduler_fini(omp_int32 global_tid)
{
  /* Do nothing */
}

/* ordered schedule should use the framework of dynamic scheduling:
 * __ompc_scheduler_init(...)
 * while(__ompc_schedule_next)
 * {
 * 	...
 * }
 */
/* This is essentially a waiting-for semphore case*/
void 
__ompc_ordered (omp_int32 global_tid) 
{
  omp_v_thread_t	*p_vthread;
  omp_team_t	*p_team;
	
  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL)
    return;

  p_vthread = __ompc_get_v_thread_by_num(global_tid);
  p_team = p_vthread->team;

  if (p_team->team_size == 1)
    return;

  pthread_mutex_lock(&(p_team->ordered_mutex));
  while (p_team->ordered_count != p_vthread->ordered_count)
    pthread_cond_wait(&(p_team->ordered_cond), &(p_team->ordered_mutex));
  pthread_mutex_unlock(&(p_team->ordered_mutex));
}

/* Inrease the global ordered semphore*/
void 
__ompc_end_ordered (omp_int32 global_tid) 
{
  omp_v_thread_t	*p_vthread;
  omp_team_t	*p_team;
  
  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL)
    return;
  
  p_vthread = __ompc_get_v_thread_by_num(global_tid);
  
  if (--p_vthread->rest_iter_count > 0)
    return;
  
  p_team = p_vthread->team;
  
  if (p_team->team_size == 1)
    return;
  
  pthread_mutex_lock(&(p_team->ordered_mutex));
  p_team->ordered_count++;
  pthread_cond_broadcast(&(p_team->ordered_cond));
  pthread_mutex_unlock(&(p_team->ordered_mutex));
}

/* Return 1 for the first one to enter single gate,
 * 0 for others.
 * consider the following sample:
 *
 * #omp single
 * ...
 * #omp end single nowait
 * ...
 * #omp single
 * ...
 * #omp end single
 *
 * How to implement single to ensure the right semantics?
 */

omp_int32 
__ompc_single (omp_int32 global_tid) 
{
  omp_team_t *p_team;
  omp_v_thread_t *p_vthread;
  int	is_first = 0;

  if (__omp_exe_mode & OMP_EXE_MODE_SEQUENTIAL)
    return 1;
  if (__omp_exe_mode & OMP_EXE_MODE_NORMAL) {
    p_team = &__omp_level_1_team_manager;
    p_vthread = &__omp_level_1_team[global_tid];
  } else {
    p_vthread = __ompc_get_v_thread_by_num(global_tid);
    p_team = p_vthread->team;
  }

  if (p_team->team_size == 1) {
    /* Single member team*/
    return 1;
  }

  p_vthread->single_count++;

  __ompc_lock(&(p_team->single_lock));
  if (p_team->single_count < p_vthread->single_count) {
    p_team->single_count++;
    is_first = 1;
  }
  __ompc_unlock(&(p_team->single_lock));

  return is_first;
}


void
__ompc_end_single (omp_int32 global_tid) 
{
  /* The single flags should be reset here*/
  /* Do nothing*/
}


omp_int32
__ompc_master (omp_int32 global_tid) 
{

  if (global_tid == 0) return 1;
  return 0;	
}

void 
__ompc_end_master (omp_int32 global_tid) 
{
  /* Do nothing*/
}

/* Other APIs show up here*/
/*allocate and get threadprivate variables for original global variable
 *  thdprv_p: the start address for heap block storing threadprivate variables
 *  size: the size of each variable
 *  datap: the address of the original global scope variable
 *  global_tid: thread id of current thread which tries to get its own threadprivate variable
 *
 * 
 * By Chunhua Liao
 */
omp_int32 
__ompc_get_thdprv(void *** thdprv_p, omp_int64 size, void *datap,omp_int32 global_tid)
{
  void **pp,*p;

  int num_threads;

  num_threads=OMP_MAX_NUM_THREADS;

  if((pp = *thdprv_p) == NULL) {
    __ompc_lock_spinlock(&_ompc_thread_lock);

    if((pp = *thdprv_p) == NULL) {
      // put the shared data aligned with the cache line size
      pp = aligned_malloc(sizeof(void *)*num_threads, CACHE_LINE_SIZE);
      Is_True (pp !=NULL, "cannot allocate memory");
      bzero(pp,sizeof(void *)*num_threads);
      *thdprv_p = pp;
    }
    __ompc_unlock_spinlock(&_ompc_thread_lock);
  }
  if((p = pp[global_tid]) == NULL) {
    if(global_tid == 0)
      p = datap;
    else {
      p = aligned_malloc((int)size, CACHE_LINE_SIZE);
      Is_True (p !=NULL, "cannot allocate memory");
      memcpy(p, datap, size);
    }
    pp[global_tid] = p;
  }
  return 1;
}

omp_int32 
__ompc_copyin_thdprv(int num,... )
     /*  __ompc_copyin_thdprv(int unknown, char* dst, char*src, int nbyte)*/
{
  char* dst;
  char* src;
  int nbyte,iter;
  va_list arguments;
  int x;

  va_start (arguments,num);
  iter=num/3;
  for (x=0;x<iter;x++)  {
    dst = va_arg (arguments, char*);
    src = va_arg (arguments, char*);
    nbyte = va_arg (arguments, int);  
    if(dst != src) bcopy(src,dst,nbyte);
  }
  va_end (arguments);

  /*        void bcopy(const void *src, void *dest, size_t n);*/
  /*   __ompc_barrier();  the barrier is moved out of this scope*/
}

/* runtime library call to help implement copyprivate
 *arguments: 
 *  mpsp_status:  value 1 means the current thread is the one 
 *               who carried out the SINGLE task
 *  cppriv: the pointer to the structure containing all pointers to 
 *          the copyprivate variables in current thread
 *  cp: the pointer to a compiler-generated function which copy values
 *      from the SINGLE thread to other threads
 * Chunhua Liao, July 2005
 */
omp_int32 
__ompc_copyprivate(omp_int32 mpsp_status, void *cppriv,\
		   void(*cp)(void* src, void* dst))
{
  omp_v_thread_t *temp_v_thread; /*user thread*/
  omp_team_t *p_team;        

  temp_v_thread = __ompc_get_current_v_thread();
  p_team = temp_v_thread->team;
  p_team->cppriv_counter= p_team->team_size;

  if (mpsp_status==1)
    p_team->cppriv=(void *)cppriv;
  __ompc_barrier(); //make sure the single thread set team value first
  if (mpsp_status!=1)
    cp(p_team->cppriv, cppriv);

  //This one is a must because the single thread may die before other threads copy their values 
  __ompc_barrier();
  /* if it is the last thread, free the shared copyprivate data in the team structure */
}
