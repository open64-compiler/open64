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
 * File: omp_rtl.h
 * Abstract: implementation of OpenMP run-time library subroutines
 *          for OpenMP programmer
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 *          06/09/2005, updated by Liao Chunhua, Univ. of Houston
 *          06/20/2007, updated by He Jiangzhou, Tsinghua Univ.
 * 
 */
#ifndef __omp_rtl_basic_included
#define __omp_rtl_basic_included

#include "omp_util.h"
#include "omp_type.h"
#include <pthread.h>
#include "omp_lock.h"

/* machine dependent values*/
#define CACHE_LINE_SIZE		64	// L1D cache line size 

#ifdef TARG_IA64
#define CACHE_LINE_SIZE_L2L3    128     // L2L3 cache line size
#else
#define CACHE_LINE_SIZE_L2L3     64      // L2L3 cache line size
#endif

/* default setting values*/
#define OMP_NESTED_DEFAULT	0	
#define OMP_DYNAMIC_DEFAULT	0
#define OMP_NUM_THREADS_DEFAULT 4  	
/* The limit of current pThread lib imposed.
 * Will be eliminated using NPTL
 */
#define OMP_MAX_NUM_THREADS 	256
#define OMP_STACK_SIZE_DEFAULT	0x400000L /* 4MB*/

#define OMP_POINTER_SIZE	8

/* Maybe the pthread_creation should be done in a unit way,
 * currently not implemented*/
/* unit used to allocate date structure */
/*
  #define OMP_ALLOC_UNIT		CACHE_LINE_SIZE_L2L3 / 8 
*/
// The following def should not be changed
// It should be consistent with def in wn_mp.cxx
typedef enum {
  OMP_SCHED_UNKNOWN     = 0,
  OMP_SCHED_STATIC      = 1,
  OMP_SCHED_STATIC_EVEN         = 2,
  OMP_SCHED_DYNAMIC     = 3,
  OMP_SCHED_GUIDED      = 4,
  OMP_SCHED_RUNTIME     = 5,

  OMP_SCHED_ORDERED_UNKNOWN     = 32,
  OMP_SCHED_ORDERED_STATIC      = 33,
  OMP_SCHED_ORDERED_STATIC_EVEN         = 34,
  OMP_SCHED_ORDERED_DYNAMIC     = 35,
  OMP_SCHED_ORDERED_GUIDED      = 36,
  OMP_SCHED_ORDERED_RUNTIME     = 37,

  OMP_SCHED_DEFAULT = OMP_SCHED_STATIC_EVEN
} omp_sched_t;

#define OMP_SCHEDULE_DEFAULT	2
#define OMP_CHUNK_SIZE_DEFAULT	1
#define OMP_SCHED_ORDERED_GAP	32


/*  micro_task prototype.
 *  omp_micro(*gtid, NULL, frame_pointer)
 *  Maybe need to revise.
 */
#define frame_pointer_t char*
/* The entry function prototype is nolonger 
 * the seem as GUIDE*/
typedef void (*omp_micro)(int, frame_pointer_t);
typedef void* (*pthread_entry)(void *);

/* global variables used in RTL, declared in omp_thread.c */
extern volatile int __omp_nested;	  /* nested enable/disable */
extern volatile int __omp_dynamic;	  /* dynamic enable/disable */
/* max num of thread available*/
extern volatile int __omp_max_num_threads;
/* stores the number of threads requested for future parallel regions. */
extern volatile int __omp_nthreads_var;
/* num of hardware processors */
extern int __omp_num_hardware_processors;
/* num of processors available*/
extern int 	    __omp_num_processors; 
/* size of core list in affinty setting*/
extern int 	    __omp_core_list_size; 
/* list of processors available*/
extern int *        __omp_list_processors;

/* default schedule type and chunk size of runtime schedule*/
extern omp_sched_t  __omp_rt_sched_type;
extern int  	    __omp_rt_sched_size;
/* flag, whether the RTL data structure has been initializaed yet*/
extern int 	    __omp_rtl_initialized;

/* Can only be set through environment variable OMP_sTACK_SIZE*/
extern volatile unsigned long int __omp_stack_size;

/* a system level lock, used for malloc in __ompc_get_thdprv ,by Liao*/
extern ompc_spinlock_t _ompc_thread_lock;

/* The OMP_EXE_MODE_NESTED_SEQUENTIAL is of no use any longer*/
typedef enum {
  OMP_EXE_MODE_SEQUENTIAL 	= 1,
  OMP_EXE_MODE_NORMAL		= 2,
  OMP_EXE_MODE_NESTED		= 4,
  OMP_EXE_MODE_NESTED_SEQUENTIAL	= 8, /* should equal to 0?*/
	
  OMP_EXE_MODE_DEFAULT = OMP_EXE_MODE_SEQUENTIAL,
  OMP_EXE_MODE_EXE_SEQUENTIAL 	= 9,
  OMP_EXE_MODE_EXE_PARALLEL 	= 6,
  OMP_EXE_MODE_IN_PARALLEL 	= 14
} omp_exe_mode_t;

/* current execution mode*/
extern volatile omp_exe_mode_t __omp_exe_mode;

typedef struct omp_u_thread omp_u_thread_t;
typedef struct omp_v_thread omp_v_thread_t;
typedef struct omp_team	    omp_team_t;

/* kernel thread*/
struct omp_u_thread{
  pthread_t uthread_id;		/* pthread id*/
  omp_u_thread_t *hash_next;	/* hash link*/
  omp_v_thread_t *task;		/* task(vthread)*/
  char *stack_pointer;
} __attribute__ ((__aligned__(CACHE_LINE_SIZE))) ;

/* team*/
struct omp_team{
  volatile int barrier_flag; // To indicate all arrived
  //	int	team_id;
  int	team_size;
  int	team_level;	/* team_level is not currently used yet*/
  int	is_nested;	

  /* for loop schedule*/

  ompc_spinlock_t schedule_lock;
  volatile long loop_lower_bound;
  long	loop_upper_bound;
  long	loop_increament;
	
  int	schedule_type;
  long	chunk_size;
  /* For static schedule*/
  //	long	loop_stride;
  /* For ordered dynamic schedule*/
  volatile long schedule_count;
  /* We still need a semphore for scheduler initialization */
  volatile int loop_count;
  /* For scheduler initialization count. */
  //	volatile int schedule_in_count;

  /* for ordered schedule*/
  /* Using schedule_lock as the ordered lock*/
  volatile long	ordered_count;
  // using a dummy field to make the following layout better
  int dummy11;
  // offset = 128, when -m64 
  pthread_mutex_t barrier_lock;
  pthread_cond_t barrier_cond;
  pthread_cond_t ordered_cond;

  /* for single*/
  // offset = 264
  ompc_lock_t	single_lock;
  volatile int	single_count; 
  //	volatile int	single_open; /* Single section protector*/
  /* for copyprivate*/
  volatile int	cppriv_counter;
  void * cppriv;

  /* for team barrier*/
  /* TODO: optimize the barrier implementation, test the performance */
  // offset = 320
  volatile int barrier_count;

  /* Still need a flag to indicate there are new tasks for level_1 team,
   * To avoid pthread allowed spurious wake up, and for nested teams,
   * use this as a semphore to synchronize all thread before they really start*/
  volatile int new_task;
  pthread_mutex_t ordered_mutex;

} __attribute__ ((__aligned__(CACHE_LINE_SIZE_L2L3)));

/* user thread*/
struct omp_v_thread {
  int	vthread_id;
  int	team_size;	/* redundant with team->team_size */
  
  omp_u_thread_t *executor;	/* needed? used anywhere?*/
  //	omp_v_thread_t *creator;	/* needed? used anywhere?*/
  omp_team_t     *team;
	
  volatile omp_micro entry_func;
  volatile frame_pointer_t frame_pointer;

  /* For RUNTIME assigned STATIC schedule only*/
  long schedule_count;
  /* For ordered schedule*/
  long ordered_count;
  long rest_iter_count;
  /* For single sections*/
  int	single_count;
  /* For Dynamic scheduler initialization*/
  int	loop_count;
  /* for 'lastprivate'? used ?*/
  //	int is_last;
} __attribute__ ((__aligned__(CACHE_LINE_SIZE)));

/* The array for level 1 thread team, 
 * using vthread_id to index them
 */
extern omp_v_thread_t *  __omp_level_1_team; 
extern omp_u_thread_t *  __omp_level_1_pthread;
extern int		 __omp_level_1_team_size;
extern volatile omp_team_t	 __omp_level_1_team_manager;
extern int		 __omp_level_1_team_alloc_size;
extern omp_u_thread_t *  __omp_uthread_hash_table[UTHREAD_HASH_SIZE]; 

/* Where do they should be initialized? */
extern pthread_t 	 __omp_root_thread_id;
extern omp_v_thread_t	 __omp_root_v_thread; /* necessary?*/
extern omp_u_thread_t *	 __omp_root_u_thread; /* Where do theyshould be initialized somewhere */
extern omp_team_t	 __omp_root_team;     /* hold information for sequential part*/

/* prototypes, implementations are defined in omp_thread.h
 *
 * Note with newer versions of GCC, no function will be generated
 * unless there is a normal function prototype when the actual
 * function definition is marked inline.
 */
extern int __ompc_can_fork(void);
extern void __ompc_set_nested(const int __nested);
extern void __ompc_set_dynamic(const int __dynamic);
extern int __ompc_get_dynamic(void);
extern int __ompc_get_nested(void);
extern int __ompc_get_max_threads(void);
extern int __ompc_get_num_procs(void);
extern void __ompc_set_num_threads(const int __num_threads);
extern int __ompc_in_parallel(void);

extern omp_u_thread_t * __ompc_get_current_u_thread();
extern omp_v_thread_t * __ompc_get_current_v_thread();
extern omp_v_thread_t * __ompc_get_v_thread_by_num(int vthread_id);
extern int __ompc_get_local_thread_num(void);
extern int __ompc_get_num_threads(void);
extern omp_team_t * __ompc_get_current_team(void);
extern void __ompc_barrier_wait(omp_team_t *team);
extern void __ompc_barrier(void);
extern void __ompc_flush(void *p);
extern int __ompc_ok_to_fork(void);
extern void __ompc_begin(void);
extern void __ompc_end(void);

extern void __ompc_fork(const int num_threads, omp_micro micro_task,
			frame_pointer_t frame_pointer);
/* copied from omp_lock.h*/
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

/* TODO:Not implemented yet*/
extern void __ompc_serialized_parallel(int vthread_id);
extern void __ompc_end_serialized_parallel(int vthread_id);
/* Other stuff fuctions*/

/* Maybe we should use libhoard for Dynamic memory 
 * allocation and deallocation,
 * The APIs should lie here. NEED TEST */

/* Support Pathscale OpenMP lowering, CWG */
extern int __ompc_sug_numthreads;
extern int __ompc_cur_numthreads;

#endif /* __omp_rtl_basic_included */
