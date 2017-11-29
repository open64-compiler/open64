/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


#ifndef ip_bwrite_INCLUDED
#define ip_bwrite_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

// The procedure for binary output: call IP_WRITE_pu for every pu in
// the program, and then, at some point before IPA terminates, call
// IP_flush_output and IP_write_global_symtab.  

// Forward declaration of a struct defined in ipc_file.h
struct IP_FILE_HDR;

extern void IP_WRITE_pu(IP_FILE_HDR *s, INT pindex);
extern void IP_flush_output(void);
extern char* IP_global_symtab_name(void);
extern void IP_write_global_symtab(void);

#ifndef _LIGHTWEIGHT_INLINER
extern void IP_build_global_filelists(IP_FILE_HDR& ip_fhdr,
                                      incl_name_map_t& incl_map,
                                      incl_name_map_t& fn_map);

#endif

#ifdef __cplusplus
}
#endif

#endif /* ip_bwrite_INCLUDED */
