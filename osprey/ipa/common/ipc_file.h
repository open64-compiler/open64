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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef ipc_file_INCLUDED
#define ipc_file_INCLUDED

#ifndef symtab_INCLUDED
#include "symtab.h"			// for FILE_INFO
#endif

#ifndef dwarf_DST_mem_INCLUDED
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"
#endif

#include <map>

// used to map debug directory names and file names
typedef std::map<INT32, INT32, std::less<INT32>, std::allocator<INT32> > incl_name_map_t;

// forward declaration

class SUMMARY_FILE_HEADER;		// see ipl_summary.h
class IPC_GLOBAL_IDX_MAP;		// see ipc_symtab_merge.h

/* 
   ipa state information is related to whirl tree nodes and local symbol
   tables. If a whirl tree is read in and modified as a side effect of
   the read process, then the flag is set to IPA_MODIFIED. Originally,
   everything is set to IPA_ORIG. When a whirl tree node or local
   symbol table is written out to the temporary file then the flag
   is set to IPA_WRITTEN
*/

enum IPA_STATE_TYPE {
    IPA_ORIG = 1,
    IPA_MODIFIED = 2,
    IPA_WRITTEN = 3,
    IPA_UNUSED = 4,		    /* to be deleted */
    IPA_DELETED = 5		    /* already deleted */
};

struct IP_PROC_INFO {

    IPA_STATE_TYPE state;
    struct pu_info *info;
    
};

inline IPA_STATE_TYPE
IP_PROC_INFO_state (const IP_PROC_INFO& proc)
{
    return proc.state;
}
inline void
Set_IP_PROC_INFO_state (IP_PROC_INFO& proc, IPA_STATE_TYPE st)
{
    proc.state = st;
}

inline struct pu_info *
IP_PROC_INFO_pu_info (const IP_PROC_INFO& proc) {
    return proc.info;
}

inline void
Set_IP_PROC_INFO_pu_info (IP_PROC_INFO& proc, struct pu_info *pu)
{
    proc.info = pu;
}

class SECTION_FILE_ANNOT;

// Flags for IP_FILE_HDR
#define FILE_HAS_NESTED_PU	0x00000001	// File contains nested PU

struct IP_FILE_HDR
{
    void *input_map_addr;
    const char *file_name;		// input file name.
                                        // There is no unique output name.

    IP_PROC_INFO *proc_info;		// array of procedure info
    UINT32 max_size;			// max size of the proc_info array
    struct pu_info *pu_list;		// pu list from the whirl file header
    struct pu_info *new_pu_list;	// new PUs added during IPA
    UINT32 num_procs;			// number of procedures in file
    UINT32 num_procs_processed;		// number of procedures processed
    UINT32 num_written;                 // number of Pus written back to disk

    MEM_POOL mem_pool;			// file mem pool (kept till written)

    // symbol table related
    FILE_INFO file_info;		// file scope symtab attributes
    DST_TYPE dst;                       // The debug symbol table.
    incl_name_map_t *incl_map;		// map original file pathnames to new
    incl_name_map_t *fn_map;		// map original filenames to new
    char *summary;			// section base of summary info
    SUMMARY_FILE_HEADER *file_header;	// header for the summary info
    const IPC_GLOBAL_IDX_MAP *idx_maps;	// maps for merged global symtab

    SECTION_FILE_ANNOT* section_annot;  // per file section info
    UINT32 flags;			// Special flags for this file
    off_t mapped_size;                  // Size of mmap memory size.

    // constructor

    IP_FILE_HDR (const char *name, void *mmap_addr, off_t mmap_size) {
	BZERO (this, sizeof(IP_FILE_HDR));
	file_name = name;
	input_map_addr = mmap_addr;
        incl_map = new incl_name_map_t;
        fn_map = new incl_name_map_t;
	MEM_POOL_Initialize (&mem_pool, const_cast<char *> (file_name),
			     FALSE /* non-zero mempool */);
	MEM_POOL_Push (&mem_pool);
    }


}; // IP_FILE_HDR

// access functions for IP_FILE_HDR

// Returns number of PUs with a given state.  This is O(num_procs),
// and is used only for debugging.
UINT32 IP_FILE_HDR_PUs_in_state(const IP_FILE_HDR& hdr, IPA_STATE_TYPE state);

inline void *
IP_FILE_HDR_input_map_addr (const IP_FILE_HDR& hdr) {
    return hdr.input_map_addr;
}
inline void
Set_IP_FILE_HDR_input_map_addr (IP_FILE_HDR& hdr, void* base) {
    hdr.input_map_addr = base;
}

inline const char *
IP_FILE_HDR_file_name (const IP_FILE_HDR& hdr) {
    return hdr.file_name;
}
inline void
Set_IP_FILE_HDR_file_name (IP_FILE_HDR& hdr, const char* name) {
    hdr.file_name = name;
}

inline IP_PROC_INFO *
IP_FILE_HDR_proc_info (const IP_FILE_HDR& hdr) {
    return hdr.proc_info;
}
inline void
Set_IP_FILE_HDR_proc_info (IP_FILE_HDR& hdr, IP_PROC_INFO *p) {
    hdr.proc_info = p;
}

inline UINT32
IP_FILE_HDR_max_size (const IP_FILE_HDR& hdr) {
    return hdr.max_size;
}
inline void
Set_IP_FILE_HDR_max_size (IP_FILE_HDR& hdr, UINT32 size) {
    hdr.max_size = size;
}

inline struct pu_info *
IP_FILE_HDR_pu_list (const IP_FILE_HDR& hdr) {
    return hdr.pu_list;
}

inline void
Set_IP_FILE_HDR_pu_list (IP_FILE_HDR& hdr, struct pu_info* pu)
{
    hdr.pu_list = pu;
}

inline struct pu_info *
IP_FILE_HDR_new_pu_list (const IP_FILE_HDR& hdr) {
    return hdr.new_pu_list;
}
inline void
Set_IP_FILE_HDR_new_pu_list (IP_FILE_HDR& hdr, struct pu_info* pu)
{
    hdr.new_pu_list = pu;
}

inline UINT32
IP_FILE_HDR_num_procs (const IP_FILE_HDR& hdr) {
    return hdr.num_procs;
}
inline void
Set_IP_FILE_HDR_num_procs (IP_FILE_HDR& hdr, UINT32 num) {
    hdr.num_procs = num;
}
inline void
Inc_IP_FILE_HDR_num_procs (IP_FILE_HDR& hdr) {
    ++hdr.num_procs;
}

inline UINT32
IP_FILE_HDR_num_procs_processed (const IP_FILE_HDR& hdr) {
    return hdr.num_procs_processed;
}

// Precondition: the PU that has been processed must be flagged as
// WRITTEN or DELETED before this function is called.  Postcondition:
// number of num_procs_proccessed equals number in state WRITTEN plus
// number in state DELETED, and this is less than or equal to total
// number in the file.
inline void
Inc_IP_FILE_HDR_num_procs_processed (IP_FILE_HDR& hdr) {
    ++(hdr.num_procs_processed);
    Is_True(hdr.num_procs_processed <= hdr.num_procs,
            ("File header %s has %u procs, %u processed",
             hdr.file_name,
             hdr.num_procs,
             hdr.num_procs_processed));
    Is_True(hdr.num_procs_processed ==
            IP_FILE_HDR_PUs_in_state(hdr, IPA_WRITTEN) +
            IP_FILE_HDR_PUs_in_state(hdr, IPA_DELETED),
            ("File hdr %s has inconsistent processed count %d, should be %d",
             hdr.file_name,
             hdr.num_procs_processed,
             IP_FILE_HDR_PUs_in_state(hdr, IPA_WRITTEN) +
             IP_FILE_HDR_PUs_in_state(hdr, IPA_DELETED)));
}

// Returns true if all PUs in the file have been written or deleted.
inline bool 
IP_FILE_HDR_all_procs_processed (const IP_FILE_HDR& hdr) {
  return IP_FILE_HDR_num_procs(hdr) == IP_FILE_HDR_num_procs_processed(hdr);
}

inline MEM_POOL *
IP_FILE_HDR_mem_pool (const IP_FILE_HDR& hdr) {
    return const_cast<MEM_POOL*>(&hdr.mem_pool);
}
inline void
Set_IP_FILE_HDR_mem_pool (IP_FILE_HDR& hdr, MEM_POOL& m) {
    hdr.mem_pool = m;
}

inline FILE_INFO &
IP_FILE_HDR_file_info (IP_FILE_HDR& hdr) {
    return hdr.file_info;
}

inline void
Set_IP_FILE_HDR_file_info (IP_FILE_HDR& hdr, FILE_INFO& file_info) {
    hdr.file_info = file_info;
}

inline incl_name_map_t& IP_FILE_HDR_incl_map( IP_FILE_HDR& h) {
        return (*h.incl_map);
}


inline incl_name_map_t& IP_FILE_HDR_fn_map( IP_FILE_HDR& h) {
  return (*h.fn_map);
}

inline DST_TYPE IP_FILE_HDR_dst(const IP_FILE_HDR& h) {
  return h.dst;
}

inline void Set_IP_FILE_HDR_dst(IP_FILE_HDR& h, DST_TYPE dst) {
  h.dst = dst;
}

inline char *
IP_FILE_HDR_summary (const IP_FILE_HDR& hdr) {
    return hdr.summary;
}
inline void
Set_IP_FILE_HDR_summary (IP_FILE_HDR& hdr, char *sum) {
    hdr.summary = sum;
}
    
inline SUMMARY_FILE_HEADER *
IP_FILE_HDR_file_header (const IP_FILE_HDR& hdr) {
    return hdr.file_header;
}
inline void
Set_IP_FILE_HDR_file_header (IP_FILE_HDR& hdr, SUMMARY_FILE_HEADER *f_hdr)
{
    hdr.file_header = f_hdr;
}

inline const IPC_GLOBAL_IDX_MAP*
IP_FILE_HDR_idx_maps (const IP_FILE_HDR& hdr)
{
    return hdr.idx_maps;
}

inline void
Set_IP_FILE_HDR_idx_maps (IP_FILE_HDR& hdr, const IPC_GLOBAL_IDX_MAP* maps)
{
    hdr.idx_maps = maps;
}

inline SECTION_FILE_ANNOT*
IP_FILE_HDR_section_annot (const IP_FILE_HDR& hdr) {
    return hdr.section_annot;
}

inline void
Set_IP_FILE_HDR_section_annot (IP_FILE_HDR& hdr, SECTION_FILE_ANNOT* annot) {
    hdr.section_annot = annot;
}

inline BOOL
IP_FILE_HDR_has_nested_pu (IP_FILE_HDR& hdr) {
    return (hdr.flags & FILE_HAS_NESTED_PU);
}

inline void
Set_IP_FILE_HDR_has_nested_pu (IP_FILE_HDR& hdr) {
    hdr.flags |= FILE_HAS_NESTED_PU;
}

typedef SEGMENTED_ARRAY<IP_FILE_HDR, 32> IP_FILE_HDR_TABLE;
extern IP_FILE_HDR_TABLE IP_File_header; 


extern IP_FILE_HDR &
Setup_File_Header (const char *file_name, void *mmap_addr, off_t);

// Reclaims all of the resources associated with hdr, by destroying
// its mempool.  Precondition: all PUs have been processed.
void Destroy_File_Header(IP_FILE_HDR& hdr);

// Used for adding PUs to IP_FILE_HDR during cloning
extern struct pu_info* IP_FILE_HDR_Add_New_PU (IP_FILE_HDR& hdr);

// Delete a PROC_INFO entry for a given function (i.e., don't write it 
// to output file
inline void
Delete_Function_In_File (IP_FILE_HDR& hdr, UINT index)
{
    IP_PROC_INFO* proc_info = IP_FILE_HDR_proc_info (hdr);
    
    Is_True (IP_PROC_INFO_state (proc_info[index]) != IPA_WRITTEN &&
	     IP_PROC_INFO_state (proc_info[index]) != IPA_DELETED,
	     ("Delete_Proc:  function already deleted"));
    
    Set_IP_PROC_INFO_state (proc_info[index], IPA_DELETED);
    Inc_IP_FILE_HDR_num_procs_processed (hdr);
    hdr.num_written++;
}

#endif /* ipc_file_INCLUDED */
