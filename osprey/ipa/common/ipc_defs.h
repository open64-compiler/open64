/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#ifndef __IPC_DEFS_H__
#define __IPC_DEFS_H__

#ifdef _64BIT_OBJECTS
#define NAME(n) n##64
#define QUOTENAME(n) n "64"
#else
#define NAME(n) n##32
#define QUOTENAME(n) n "32"
#endif

/* used in ipc_link.c */
#define DEFAULT_MAX_COMMA_LIST 32

/* used in ipc_compile.c */
#define IPACOM_NAME "/ipacom"

typedef enum ip_target_type {
    IP_32_bit_ABI,
    IP_64_bit_ABI
} IP_TARGET_TYPE;

extern IP_TARGET_TYPE IPA_Target_Type;

#endif /* __IPC_DEFS_H__ */
