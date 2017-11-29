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


//-*-C++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: array_lower.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description:
 * 
 * 
 *                      Array Lowering
 *                      --------------
 * 
 *    This file lowers all accesses to reshaped array variables.  
 *    
 *    Each access to a distributed dimension is translated as follows:
 *    
 *        Cyclic(b) distributions, the general case: block size = b, procs = p
 * 
 *            A[j] ==> A[(j/b)%p][j/pb][j%b] 
 *        
 *            j/b%p = processor on which element "j" is allocated
 *            j/pb  = block within the processor 
 *            j%b   = offset within the block
 * 
 *    We can optimize the array index expressions for the following 
 *    special cases:
 * 
 *        Block distributions: block size b = divceil(N,p), where N is the 
 *        size of the dimension
 * 
 *            A[j] ==> A[j/b][j%b]
 * 
 *            j/b = processor on which element "j" is allocated
 *            j%b = offset within the block
 *    
 *        Cyclic(1) distributions: block size = 1
 * 
 *            A[j] ==> A[j%p][j/p]
 * 
 *            j%p = processor on which element "j" is allocated
 *            j/p = offset within the block
 *    
 * 
 *  Exported types and functions:
 *
 *      void Lower_Array_Accesses(WN *func_nd);
 *
 *          Lower all the accesses to reshaped arrays in func_nd, where
 *          func_nd is an OPR_FUNC_ENTRY node.
 *
 * ====================================================================
 * ====================================================================
 */
#ifndef array_lower_INCLUDED
#define array_lower_INCLUDED

#ifdef _KEEP_RCS_ID
static char *array_lower_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "wn.h"

extern void Lower_Array_Accesses(WN *func_nd);

#endif /* array_lower_INCLUDED */

