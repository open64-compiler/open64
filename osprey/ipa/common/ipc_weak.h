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


/* ====================================================================
 * ====================================================================
 *
 * Module: ipc_weak.h
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/common/ipc_weak.h,v $
 *
 * Revision history:
 *  14-Jun-95 - Original Version
 *  12-Apr-96 - Suppress C++ warnings
 *
 * Description:
 *
 *  Declare symbols defined in ld and referenced in ipa.so weak so that
 *  ipa.so will link cleanly.
 *
 * TODO:  C++ doesn't yet recognize #pragma weak, so we suppress this
 * all for C++ compilations.  Reinstate when C++ supports them.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ipc_weak_INCLUDED
#define ipc_weak_INCLUDED

#ifdef __PASS1_H__
#pragma weak used_gp_area
#endif /* __PASS1_H__ */

#ifdef __IPA_OPTION_H__
#pragma weak ld_ipa_opt
#pragma weak ipacom_flags
#pragma weak WB_flags
#pragma weak Y_flags
#endif /* __OPTION_H__ */

#ifdef __LD_ERROR_H__
#pragma weak msg
#endif /* __LD_ERROR_H__ */

#ifdef __EXT_TBL_H__
#pragma weak merge_ext
#pragma weak enter_mext
#pragma weak slookup_mext
#pragma weak slookup_mext_idx
#pragma weak get_mext
#pragma weak ext_tbl

#pragma weak ld_slookup_mext

#endif /* __EXT_TBL_H__ */

#ifdef __OBJ_FILE_H__
#pragma weak num_ir
#pragma weak get_next_ir
#pragma weak is_archive_member
#endif /* __OBJ_FILE_H__ */

#ifdef __PROCESS_H__
extern int create_tmpdir (int) __attribute__((weak));
extern string create_unique_file (const string, char) __attribute__((weak));
extern string create_tmp_file (const string) __attribute__((weak));
extern void add_to_tmp_file_list (string) __attribute__((weak));
extern string tmpdir __attribute__((weak));
extern string *get_command_line (an_object_file_ptr, string, string, int*) __attribute__((weak));
extern int make_link (const string, const string) __attribute__((weak));
#endif /* __PROCESS_H__ */

#ifdef __LD_UTIL_H__
#pragma weak concat_names
#endif /* __LD_UTIL_H__ */

#ifdef __LD_MAIN_H__
extern int arg_count __attribute__((weak));	// TK debug
#pragma weak arg_vector
#pragma weak environ_vars
#pragma weak max_gpa_size
#endif /* __LD_MAIN_H__ */

#ifdef __READ_H__
#pragma weak read_one_section
#pragma weak read_headers
#pragma weak unread_sections
#pragma weak unread_obj
#pragma weak objs_mapped_total_size
#pragma weak copy_section
#endif /* __READ_H__ */

#ifdef __DEM_H__
#pragma weak always_demangle
#endif /* __DEM_H__ */

#ifdef __ELFHASH_H
#pragma weak elfhash
#endif /* __ELFHASH_H */

#endif /* ipc_weak_INCLUDED */
