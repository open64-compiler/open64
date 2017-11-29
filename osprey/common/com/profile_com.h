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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: profile_com.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/profile_com.h,v $
//
// Revision history:
//  24-Jul-98 - Original Version
//
// Description:
//
// ====================================================================
// ====================================================================

#ifndef profile_com_INCLUDED
#define profile_com_INCLUDED

#include "defs.h"

#if defined(defs_INCLUDED) && ! defined(USE_STANDARD_TYPES)
#undef short                            // get around bogus type defs.
#undef int
#undef long
#endif // defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)

#define ULONG mUINT32

#include "profile_type.h"

/* Feedback File Format */

#define FB_NIDENT       16

#define INSTR_MAG          "\177INS"

#define INSTR_CURRENT      2

struct Fb_Hdr {
  char fb_ident[FB_NIDENT];     /* ident bytes */
  ULONG fb_version;             /* file version */
  ULONG fb_profile_offset;	/* file offset for profile data */
  ULONG fb_pu_hdr_offset;       /* PU header file offset */
  ULONG fb_pu_hdr_ent_size;     /* PU header entry size */ 
  ULONG fb_pu_hdr_num;          /* Number of PU header entries */
  ULONG fb_str_table_offset;
  ULONG fb_str_table_size;
  PROFILE_PHASE phase_num;

  Fb_Hdr() {}
  Fb_Hdr(Fb_Hdr& x) 
  {
    memcpy((void *)fb_ident,(void *)x.fb_ident, FB_NIDENT);
    fb_version = x.fb_version;
    fb_profile_offset = x.fb_profile_offset;
    fb_pu_hdr_offset = x.fb_pu_hdr_offset;
    fb_pu_hdr_ent_size = x.fb_pu_hdr_ent_size;
    fb_pu_hdr_num = x.fb_pu_hdr_num;
    fb_str_table_offset = x.fb_str_table_offset;
    fb_str_table_size = x.fb_str_table_size;
    phase_num = x.phase_num;
  }
  void Print( FILE *fp ) const {
  	fprintf(fp, "\n**********      FILE HEADER     **************\n");
  	fprintf(fp, "fb_ident = %s\n", fb_ident);
  	fprintf(fp, "fb_version = %u\n",fb_version);
  	fprintf(fp, "fb_profile_offset = %u\n",fb_profile_offset);
  	fprintf(fp, "fb_pu_hdr_offset = %u\n",fb_pu_hdr_offset);
  	fprintf(fp, "fb_pu_hdr_ent_size = %u\n",fb_pu_hdr_ent_size);
  	fprintf(fp, "fb_pu_hdr_num = %u\n",fb_pu_hdr_num);
  	fprintf(fp, "fb_str_table_offset = %u\n",fb_str_table_offset);
  	fprintf(fp, "fb_str_table_size = %u\n",fb_str_table_size);
  	fprintf(fp, "phase_num = %u\n",phase_num);
  };

}; 

struct Pu_Hdr {
  INT32 pu_checksum;
  INT32 pu_size;
#if defined(TARG_SL)
  UINT32 runtime_fun_address;
#else
  UINT64 runtime_fun_address;
#endif
  ULONG pu_name_index;
  ULONG pu_file_offset;
  ULONG pu_inv_offset;
  ULONG pu_num_inv_entries;
  ULONG pu_br_offset;
  ULONG pu_num_br_entries;
  ULONG pu_switch_offset;
  ULONG pu_switch_target_offset;	// # of targets for each swtich stmt
  ULONG pu_num_switch_entries;
  ULONG pu_cgoto_offset;
  ULONG pu_cgoto_target_offset;		// # of targets for each compgoto
  ULONG pu_num_cgoto_entries;
  ULONG pu_loop_offset;
  ULONG pu_num_loop_entries;
  ULONG pu_scircuit_offset;
  ULONG pu_num_scircuit_entries;
  ULONG pu_call_offset;
  ULONG pu_num_call_entries;
#ifdef KEY
  ULONG pu_value_offset;
  ULONG pu_num_value_entries;
  ULONG pu_value_fp_bin_offset;
  ULONG pu_num_value_fp_bin_entries;
#endif

  ULONG pu_icall_offset;
  ULONG pu_num_icall_entries;
  ULONG pu_handle;
  ULONG pu_edge_offset;
  ULONG pu_num_edge_entries;
  ULONG pu_instr_count;
  ULONG pu_instr_exec_count;
#ifdef KEY
#ifndef TARG_LOONGSON
  ULONG pu_values_offset;
  ULONG pu_values_fp_bin_offset;
#else
  ULONG pu_mem_count;
  ULONG pu_cache_offset;
#endif
#else  // KEY
  ULONG pu_value_offset;
#endif
  ULONG pu_ld_count;   //prefetch count
  ULONG pu_stride_offset;
#ifdef KEY
  // This header file will be compiled into the 64-bit instrumentation library
  // as well as the 32-bit compiler. So, we should pad it to a 8-byte boundary.
  ULONG pad;
#endif
  
  Pu_Hdr() {
    pu_size=54321;
    pu_handle=11111;
    pu_edge_offset=222;
    pu_num_edge_entries=0;
    pu_instr_count=0;
    pu_instr_exec_count=0;
    pu_icall_offset=333;
    pu_num_icall_entries=0;
#ifdef KEY
#ifndef TARG_LOONGSON
    pu_values_offset=444;
    pu_values_fp_bin_offset=666;
#endif
#else
    pu_value_offset=444;
#endif
    pu_ld_count=0;
    pu_stride_offset=555;
#ifdef KEY
    pad = 0;
#endif
  }

  void Print( FILE * fp, int id=-1) const {
  	fprintf(fp, "\n**********   PU Header No %d   **************\n", id);
  	fprintf(fp, "pu_checksum = %d\n", pu_checksum);
  	fprintf(fp, "pu_size = %d\n", pu_size);
#ifdef TARG_SL
  	fprintf(fp, "runtime_fun_address= %u\n", runtime_fun_address);
#else
  	fprintf(fp, "runtime_fun_address= %llu\n", runtime_fun_address);
#endif
  	fprintf(fp, "pu_name_index = %u\n", pu_name_index);
  	fprintf(fp, "pu_file_offset = %u\n", pu_file_offset);
  	fprintf(fp, "pu_inv_offset = %u\n", pu_inv_offset);
  	fprintf(fp, "pu_num_inv_entries = %u\n", pu_num_inv_entries);
  	fprintf(fp, "pu_br_offset = %u\n", pu_br_offset);
  	fprintf(fp, "pu_num_br_entries = %u\n", pu_num_br_entries);
  	fprintf(fp, "pu_switch_offset = %u\n", pu_switch_offset);
  	fprintf(fp, "pu_switch_target_offset = %u\n", pu_switch_target_offset);
  	fprintf(fp, "pu_num_switch_entries = %u\n", pu_num_switch_entries);
  	fprintf(fp, "pu_cgoto_offset = %u\n", pu_cgoto_offset);
  	fprintf(fp, "pu_cgoto_target_offset = %u\n", pu_cgoto_target_offset);
  	fprintf(fp, "pu_num_cgoto_entries = %u\n", pu_num_cgoto_entries);
  	fprintf(fp, "pu_loop_offset = %u\n", pu_loop_offset);
  	fprintf(fp, "pu_num_loop_entries = %u\n", pu_num_loop_entries);
  	fprintf(fp, "pu_scircuit_offset = %u\n", pu_scircuit_offset);
  	fprintf(fp, "pu_num_scircuit_entries = %u\n", pu_num_scircuit_entries);
  	fprintf(fp, "pu_call_offset = %u\n", pu_call_offset);
  	fprintf(fp, "pu_num_call_entries = %u\n", pu_num_call_entries);
#ifdef KEY
  	fprintf(fp, "pu_value_offset = %u\n",      pu_value_offset);
  	fprintf(fp, "pu_num_value_entries = %u\n", pu_num_value_entries);
  	fprintf(fp, "pu_value_fp_bin_offset = %u\n",  pu_value_fp_bin_offset);
  	fprintf(fp, "pu_num_value_fp_bin_entries = %u\n", 
		pu_num_value_fp_bin_entries);
#endif
  	fprintf(fp, "pu_icall_offset = %u\n", pu_icall_offset);
  	fprintf(fp, "pu_num_icall_entries = %u\n", pu_num_icall_entries);
  	fprintf(fp, "pu_handle = %u\n", pu_handle);
  	fprintf(fp, "pu_edge_offset = %u\n", pu_edge_offset);
  	fprintf(fp, "pu_num_edge_entries = %u\n", pu_num_edge_entries);
  	fprintf(fp, "pu_instr_count = %u\n", pu_instr_count);
  	fprintf(fp, "pu_instr_exec_count = %u\n", pu_instr_exec_count);
#ifdef KEY
#ifndef TARG_LOONGSON
  	fprintf(fp, "pu_values_offset = %u\n", pu_values_offset);
  	fprintf(fp, "pu_values_fp_bin_offset = %u\n", pu_values_fp_bin_offset);
#else
	fprintf(fp, "pu_mem_count = %u\n", pu_mem_count);
  	fprintf(fp, "pu_cache_offset = %u\n", pu_cache_offset);
#endif
#else
  	fprintf(fp, "pu_value_offset = %u\n", pu_value_offset);
#endif
  	fprintf(fp, "pu_ld_count = %u\n", pu_ld_count);
  	fprintf(fp, "pu_stride_offset = %u\n", pu_stride_offset);
  };
};
#endif /* profile_com_INCLUDED */
