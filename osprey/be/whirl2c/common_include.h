/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#ifndef common_include_INCLUDED
#define common_include_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: common_include.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:59-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.common_include.h $
 *
 * Revision history:
 *  07-Mar-95 - Original Version
 *
 * Description:
 *
 *    Groups together the files to be included by virtually all
 *    files translating WHIRL to another language.  We also 
 *    provide macros for dynamic memory allocation in this header
 *    file.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdio.h>

#include "defs.h"          /* Basic types, such as INT32 */
#include "config.h"
#include "erglob.h"        /* For errors.h and EC definitions */
#include "wn.h"            /* WN nodes */
#include "symtab.h"        /* TY and ST nodes */
#include "be_symtab.h"     /* BE_ST flags */
#include "glob.h"          /* Irb_File_Name */
/* Ensure that we never refer to Cur_PU_Name, since this will not be
 * initialized, but instead use the PUinfo facilities.
 */
#define Cur_PU_Name You_should_not_use("Cur_PU_Name")

#include "diagnostics.h"   /* For diagnostics */
#include "wn_attr.h"       /* Attributes of WN nodes (beyond wn_core.h) */
#include "stab_attr.h"     /* Attributes of TY and ST nodes (beyond stab.h) */
#include "token_buffer.h"  /* formatting tokenized output */
#include "w2cf_symtab.h"   /* Identifier name-table */


// W2FC flag accessors..

extern void Set_TY_is_translated_to_c(const TY_IDX ty) ;
extern void Reset_TY_is_translated_to_c(const TY_IDX ty) ;
extern BOOL TY_is_translated_to_c(const TY_IDX ty) ;      
extern void Clear_w2fc_flags(void)  ;


/* Some general utility routines for memory allocation.  Include
 * "mempool.h" when using these!
 */
#define TYPE_ALLOC_N(type, count)\
   TYPE_MEM_POOL_ALLOC_N(type, Malloc_Mem_Pool, count)

#define TYPE_REALLOC_N(type, old_ptr, old_count, new_count)\
   TYPE_MEM_POOL_REALLOC_N(type, Malloc_Mem_Pool, old_ptr,\
			   old_count, new_count)

#define FREE(ptr) MEM_POOL_FREE(Malloc_Mem_Pool, ptr)

#endif /* common_include_INCLUDED */


