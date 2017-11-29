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


// -*-C++-*-
// ====================================================================
// ====================================================================
//
// Module: cxx_hash.cxx
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:35-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_hash.cxx $
//
// Revision history:
//  07-Dec-95 - Merged user-hash version from IPA
//
// Description:
//
// Template member function bodies for template hash map
// implementations.
//
// ====================================================================
// ====================================================================

#ifdef _KEEP_RCS_ID
#define cxx_hash_CXX      "cxx_hash.cxx"
static char *rcs_id = cxx_hash_CXX" $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "erglob.h"
#include "cxx_hash.h"

// ====================================================================
// ====================================================================
//
// HASH_TABLE
//
// This is a simple hash map, with the following attributes:
//
//  1)	The size of the hash table is determined at constructor time.
//
//  2)	The hash table elements are lists of objects.
//
//  3)	The objects in the table's lists are pairs consisting of a
//	signature (key) and a data element to which the key is mapped.
//
//  4)	The hash function is built in as the signature modulo the table
//	size.  Therefore, for example, it will be the pointer value for
//	a string key, and will produce different entries for distinct
//	pointers to the same character string.
//
// ====================================================================
// ====================================================================

