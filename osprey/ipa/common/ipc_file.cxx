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


#include "ipc_file.h"
#include "pu_info.h"                    

IP_FILE_HDR_TABLE IP_File_header;	// array of IP_FILE_HDR, which
					// holds per file information

	/*******************************************************
		Function: Setup_File_Header

		
	 *******************************************************/
IP_FILE_HDR&
Setup_File_Header (const char *file_name, void *mmap_addr, off_t mmap_size)
{
    UINT index;

    IP_FILE_HDR& file_header = IP_File_header.New_entry (index);
    new (&file_header) IP_FILE_HDR (file_name, mmap_addr, mmap_size);

#ifdef TODO
    NAME(IP_get_malloced_info)(p_obj, &(s[i].str_table),
			       (void **)&(s[i].pu_list),
			       (void **)&(s[i].summary)); 
    if (p_obj->ipa_info.gp_status == GP_UNIQUE) {
	s[i].sp_partition_group = p_obj->ipa_info.partition_grp;
	if (p_obj->ipa_info.flags & IPA_USER_SET_FILE_PARTITION)
	    tag_ext = TRUE;
    }
    else
	s[i].sp_partition_group = COMMON_PARTITION;
#endif // TODO
    
    return file_header;
} // Setup_File_Header

	/*******************************************************
		Namespace: 

		
	 *******************************************************/
namespace {                     // Begin unnamed namespace
  // Function object used by IP_FILE_HDR_PUs_in_state.
  struct proc_info_state_is {
    IPA_STATE_TYPE state;
    proc_info_state_is(IPA_STATE_TYPE s) : state(s) {}

    bool operator()(const IP_PROC_INFO& info) const {
      return IP_PROC_INFO_state(info) == state;
    }
  };
} // End unnamed namespace

	/*******************************************************
		Function: IP_FILE_HDR_PUs_in_state

		
	 *******************************************************/
// Counts the number of pu's in a particular state.  This is 
// O(num_procs), and is used only for debug builds.  We use it
// to verify that the counts in an IP_FILE_HDR are sane.
UINT32 IP_FILE_HDR_PUs_in_state(const IP_FILE_HDR& hdr, IPA_STATE_TYPE state)
{
  Is_True(hdr.proc_info != 0,
          ("File header %s has no proc_info array", hdr.file_name));
  Is_True(state >= IPA_ORIG && state <= IPA_DELETED,
          ("Bad value for enum IPA_STATE_TYPE: %d", state));

  const IP_PROC_INFO* first = hdr.proc_info;
  const IP_PROC_INFO* last  = hdr.proc_info + hdr.num_procs;

  return std::count_if(first, last, proc_info_state_is(state));
}

	/*******************************************************
		Function: Destroy_File_Header

		
	 *******************************************************/
// Reclaim all of hdr's memory, by destroying its mempool.
void Destroy_File_Header(IP_FILE_HDR& hdr) 
{
  Is_True(IP_FILE_HDR_all_procs_processed(hdr),
          ("Can't destroy file hdr %s.  Num procs = %u, processed = %u.",
           hdr.file_name, hdr.num_procs, hdr.num_procs_processed));

  MEM_POOL_Delete(&hdr.mem_pool);
}

  
	/*******************************************************
		Function: IP_FILE_HDR_Add_New_PU

		
	 *******************************************************/
// Used for adding PUs to IP_FILE_HDR during cloning
PU_Info* 
IP_FILE_HDR_Add_New_PU (IP_FILE_HDR& hdr)
{
  // reallocate proc_info array if necessary
  if (hdr.num_procs >= hdr.max_size) {
    hdr.proc_info = (IP_PROC_INFO*) 
      MEM_POOL_Realloc (&hdr.mem_pool,
                        hdr.proc_info,
                        hdr.max_size * sizeof(IP_PROC_INFO),
                        (hdr.max_size + 16) * sizeof(IP_PROC_INFO));
    hdr.max_size += 16;
  }
  
  // allocate and initialize a new PU_Info struct
  PU_Info* pu = (PU_Info*) MEM_POOL_Alloc (Malloc_Mem_Pool, sizeof(PU_Info));
  BZERO(pu, sizeof(PU_Info));
  PU_Info_init(pu);
  
  // connect it to the list of IPA-created PUs
  PU_Info_next(pu) = hdr.new_pu_list;
  hdr.new_pu_list = pu;
  
  // set IP_PROC_INFO struct for new PU
  hdr.proc_info[hdr.num_procs].info = pu;
  hdr.proc_info[hdr.num_procs].state = IPA_MODIFIED;

  hdr.num_procs++;

  return pu;
}
