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
 * File: omp_util.h
 * Abstract: some utilities of the OpenMP Runtime Library
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 * 
 */
#ifndef __omp_rtl_utility_included
#define __omp_rtl_utility_included
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
//#include <sched.h>

#define Is_True_On
/* Assertion stuff */
#ifdef Is_True_On
#define Is_True(Cond, ParmList)\
    ( Cond ? (void) 1 : \
    ( fprintf(stderr, "Assertion Failed at %s:%d ", __FILE__, __LINE__), \
      fprintf(stderr, ParmList), \
      fprintf(stderr, "\n"), \
      fflush(stderr), \
      abort(), (void) 0))

#define DevWarning(Cond, ParmList)\
    ( Cond ? (void) 1 : \
    ( fprintf(stderr, "DevWaring: at %s: %d", __FILE__, __LINE__), \
      fprintf(stderr, ParmList), \
      fprintf(stderr, "\n"), \
      fflush(stderr), (void) 0)) 

#define DebugLog(ParmList) \
	fprintf(stderr, "Debug Log at %s:%d", __FILE__, __LINE__); \
	fprintf(stderr, ParmList);
#else

#define Is_True(Cond, ParmList) ()
#define Dev_Warning(Cond, ParmList) ()
#define DebugLog(ParmList) ()

#endif
	
#define Is_Valid(Cond, ParmList)\
    ( Cond ? (void) 1 : \
    ( fprintf(stderr, "Invalid setting : "), \
      fprintf(stderr, ParmList), \
      fprintf(stderr, "\n"), \
      fflush(stderr), \
      abort(), (void) 0))

void
Not_Valid (char * error_message) __attribute__ ((__noreturn__));

void
Warning (char * warning_message);

/* Waiting while condition is true */
//#define MAX_COUNT 50000
#define MAX_COUNT 1000000000
#define OMPC_WAIT_WHILE(condition) \
      { \
          if (condition) { \
              int count = 0; \
              while (condition) { \
                   if (count > MAX_COUNT) { \
	                count = 0; \
                   } \
		   count++; \
              } \
          } \
      }

/* Hash stuff*/
#define UTHREAD_HASH_SIZE  0x100L
#define UTHREAD_HASH_MASK (UTHREAD_HASH_SIZE-1)
#define HASH_IDX(ID) ((int)((unsigned long int)(ID) & (UTHREAD_HASH_MASK)))

/* string stuff*/
char *
Trim_Leading_Spaces (char *);

/* system stuff*/
unsigned long int
Get_System_Stack_Limit(void);

int
Get_SMP_CPU_num(void);

int
Get_CPU_Cores(void);

int 
Get_Affinity_Map(int**, int);

void
Get_Ordered_Corelist(int *, int);

void *
aligned_malloc(size_t, size_t);

void
aligned_free(void*);

void *
aligned_realloc(void *, size_t, size_t, size_t);

void __ompc_do_nothing(void);
#endif
