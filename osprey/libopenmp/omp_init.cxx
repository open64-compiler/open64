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
 * File: omp_init.cxx
 * Abstract: initialize the internal data structures used in libopenmp by C++ ctor
 * History: 21/10/2008, created by Lai JianXin
 */

/* 
 * To align with the Pathscale OMP lowering 
 * Put '__ompc_sug_numthreads' here so that this file can be linked into the executable.
 */
int __ompc_sug_numthreads = 0;
int __ompc_cur_numthreads = 0;

extern "C" int  __ompc_init_rtl(int num_threads);

/*
 * class __ompc_rtl_initializer
 * initialize the libopenmp by a static instance.
 */
class __ompc_rtl_initializer {
private:
    __ompc_rtl_initializer() {
        // initialize the openmp rtl
	__ompc_init_rtl(0);
    }
    ~__ompc_rtl_initializer() {
        // Do nothing since __ompc_fini_rtl is added to atexit.
    }
    static __ompc_rtl_initializer inst_;
};
__ompc_rtl_initializer __ompc_rtl_initializer::inst_;

