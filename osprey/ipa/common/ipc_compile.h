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


#ifndef __IPC_COMPILE_H__
#define __IPC_COMPILE_H__

#ifndef __IPC_LINK_H__
#include "ipc_link.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

void ipa_compile_init ();
void ipacom_process_symtab (char* symtab_file);
size_t ipacom_process_file (char* input_file, const PU_Info* pu,
			    UINT32 ProMP_id);
void ipacom_add_comment(size_t n, const char* comment);
void ipacom_doit ( char *ipa_filename );

#ifdef __cplusplus
} // Close extern "C"
#endif

#endif /* __IPC_COMPILE_H__ */
