/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libfi/matrix/mmx_dlutil.c	92.2	07/09/99 15:18:08"

#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

#include "mmx_dlutil.h"

void (*__f90_sgemm_blas)=NULL;
void (*__f90_dgemm_blas)=NULL;
void (*__f90_cgemm_blas)=NULL;
void (*__f90_zgemm_blas)=NULL;

#define TRUE 1
#define FALSE 0
typedef int BOOL;

static BOOL try_blas_open=TRUE;
static BOOL allow_parallel_blas=FALSE;

/* 
   This routine attempts to load a symbol from the BLAS library. It first looks in the application, 
   to see if the user has loaded a DSO with the appropriate symbol in it. If it finds the symbol, 
   it uses it. Otherwise, it looks in libblas_mp.so. The lookup will fail if libmp is not linked in, 
   which means the user is not using the "MP" kind of parallelism so we don't want it. 
   Finally, it looks in libblas.so. If the libraries are missing, you don't get the symbols, so we will 
   use the internal routines instead. 

   Two environment variables can also be set:

   F90_NO_MATMUL_BLAS - if set, don't ever use the BLAS routine.
   F90_NO_MATMUL_PARALLEL_BLAS - if set, don't ever use the parallel BLAS routine. 

*/

#ifndef DLLDEBUG
#define DLLDEBUG 0
#endif

static void * get_blas_routine(char * rname)
{
  void * mmfunc=NULL;
  void * blas_handle;

  /* First, see if the routines are already linked in */
  blas_handle = dlopen(NULL, RTLD_LAZY);
  if (blas_handle) {
    mmfunc = dlsym(blas_handle,rname);
    if (mmfunc) {
#if DLLDEBUG
      printf("resolving %s from linked in libraries\n",rname);
#endif    
      return (mmfunc);
    }
  }

  /* Look in libblas_mp.so */
  if (allow_parallel_blas) {
    blas_handle = dlopen("libblas_mp.so", RTLD_LAZY);
    if (blas_handle) {
      mmfunc = dlsym(blas_handle,rname);
      if (mmfunc) {
#if DLLDEBUG
	printf("resolving %s from libblas_mp\n",rname);
#endif    
	return (mmfunc);
      }	
    }
  }
  
  /* Look in libblas.so */
  blas_handle = dlopen("libblas.so", RTLD_LAZY);
  if (blas_handle) {
    mmfunc = dlsym(blas_handle,rname);
    if (mmfunc) {
#if DLLDEBUG
      printf("resolving %s from libblas\n",rname);
#endif    
      return (mmfunc);
    }
  }
  return (NULL);
}

void __f90_open_blas_lib(void)
{
  if (!try_blas_open) return;
  try_blas_open = FALSE;

  if (!getenv("F90_MATMUL_BLAS")) return;

  if (getenv("F90_MATMUL_PARALLEL_BLAS")) {
    allow_parallel_blas = TRUE;
  }
  
  __f90_sgemm_blas = get_blas_routine("sgemm_");
  __f90_dgemm_blas = get_blas_routine("dgemm_");
  __f90_cgemm_blas = get_blas_routine("cgemm_");
  __f90_zgemm_blas = get_blas_routine("zgemm_");
}

