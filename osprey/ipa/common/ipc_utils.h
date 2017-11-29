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


/* ====================================================================
 * ====================================================================
 *
 * Module: ipc_utils.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /hosts/bonnie.engr/depot/cmplrs.src/mongoose/ipa/common/RCS/ipc_utils.h,
v $
 *
 * Revision history:
 *  31-Jul-95 - Original Version
 *
 * Description:
 *
 * Supporting routines for processing libraries in cross_file STANDALONE inlining
 *
 * ====================================================================
 * ====================================================================
 */


#ifndef ipc_utils_INCLUDED
#define ipc_utils_INCLUDED


#include <ar.h>
#include "mempool.h"

extern void *Digest_Archive (void* handle, MEM_POOL* m, INT64 file_size);
extern void Cleanup_Archive_Handle (void *handle, MEM_POOL*);
extern off_t Defined_By_Archive (char *name, void* handle);
extern char *Read_Member_Name (struct ar_hdr *header, void* handle, MEM_POOL* m);
extern off_t Next_Archive_Member (char* base, off_t offset, INT64 size);

#endif // ipc_utils_INCLUDED
