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
 * File: omp_type.h
 * Abstract: basic type definitions
 * History: 04/23/2003, built by Jiang Hongshan, Tsinghua Univ.
 * 
 */
#ifndef __omp_rtl_type_included
#define __omp_rtl_type_included

/* This header file defines all the type alias used in
 * the RTL.
 */

typedef int 	  omp_int32;
typedef long long omp_int64;
typedef float	  omp_real32;
typedef double	  omp_real64;
typedef int	  omp_bool;

typedef int       omp_int_t;
typedef double    omp_wtime_t;
typedef void     *omp_lock_t; 
typedef void     *omp_nest_lock_t; 

#define TRUE	1
#define FALSE	0

#endif /* end __omp_rtl_type_included*/
