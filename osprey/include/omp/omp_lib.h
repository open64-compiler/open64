!  OpenMP runtime library to be used in conjunction with Open64 Compiler Suites.
!
!  Copyright (C) 2003 - 2009 Tsinghua University.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation; either
!  version 2.1 of the License, or (at your option) any later version.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
!  
!  Contact information: HPC Institute, Department of Computer Science and Technology,
!  Tsinghua University, Beijing 100084, CHINA, or:
!
!  http://hpc.cs.tsinghua.edu.cn

C the "C" of this comment starts in column 1
	integer omp_lock_kind
	parameter ( omp_lock_kind = 8 )

	integer omp_nest_lock_kind
	parameter ( omp_nest_lock_kind = 8 )

C default integer type assumed below
C default logical type assumed below
C OpenMP Fortran API v1.1
	integer openmp_version
	parameter ( openmp_version = 200011 )

	external omp_destroy_lock
	external omp_destroy_nest_lock

	external omp_get_dynamic
	logical omp_get_dynamic

	external omp_get_max_threads
	integer omp_get_max_threads

	external omp_get_nested
	logical omp_get_nested

	external omp_get_num_procs
	integer omp_get_num_procs

	external omp_get_num_threads
	integer omp_get_num_threads

	external omp_get_thread_num
	integer omp_get_thread_num

	external omp_get_wtick
	double precision omp_get_wtick

	external omp_get_wtime
	double precision omp_get_wtime

	external omp_init_lock
	external omp_init_nest_lock

	external omp_in_parallel
	logical omp_in_parallel

	external omp_set_dynamic

	external omp_set_lock	
	external omp_set_nest_lock

	external omp_set_nested

	external omp_set_num_threads

	external omp_test_lock
	logical omp_test_lock

	external omp_test_nest_lock
	integer omp_test_nest_lock

	external omp_unset_lock
	external omp_unset_nest_lock

