/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h> 

#ifdef KEY
#include <linux/limits.h>
#endif

#include "defs.h"
#include "symtab.h"			// symbol table
#include "wn.h"				// for WHIRL 
#define USE_DST_INTERNALS		// needed by pu_info.h
#include "pu_info.h"			// needed by ir_bwrite.h

#ifndef BACK_END
#define BACK_END			// needed by ir_bwrite.h
#endif
#include "ir_bwrite.h"			// low-level WHIRL output routines

#include "erglob.h"			// error codes

#include "config_opt.h"
#include "ipa_option.h"			// option flags
#include "ipa_cg.h"			// call graph
#include "ipaa.h"			// for Mod_Ref_Set
#include "ipc_compile.h"		// for ipacom_add_comment
#include "ipc_dst_merge.h"		// for IPC_merge_DSTs
#include "ipc_dst_utils.h"		// for DST_create
#include "ipo_alias_class.h"
#include "ipo_defs.h"			// IPA_NODE_CONTEXT

#include "ipo_main.h"           // for ipisr_cg

#include <cmplrs/host.h>        // for typedef string
#include "ld_ipa_option.h"      // For ld_ipa_opt 
#include "ipc_weak.h"           

#ifdef KEY
#include "ipa_builtins.h"
#include "ir_bcom.h"
#include "be_ipa_util.h"
#endif

#include "ipa_nystrom_alias_analyzer.h"

extern "C" void add_to_tmp_file_list (char*);
#pragma weak add_to_tmp_file_list

extern "C" char* create_unique_file (const char*, char);
#pragma weak create_unique_file

extern "C" char* create_tmp_file (char*);
#pragma weak create_tmp_file

/************************************************************************/

// list of dummy PU's holding global pragmas.  They need to be written out
// to the symtab.I file instead.
typedef vector<PU_Info*> DUMMY_GLOBAL_PRAGMA_PU;
DUMMY_GLOBAL_PRAGMA_PU dummy_pu_list;

static PU_Info *
Write_Dummy_PUs (Output_File* output_file);

namespace {

  struct externize {
    void operator()(UINT32 /* idx */, ST* st) const {
      if (ST_sym_class(*st) == CLASS_FUNC &&
	  ! PU_has_global_pragmas (Pu_Table[ST_pu (st)]))
        Set_ST_storage_class(*st, SCLASS_EXTERN);
    }
  };

}

extern "C" char* IP_global_symtab_name(void)
{
  static char* symtab_file_name = 0;

  if (!symtab_file_name)
    symtab_file_name = create_unique_file("symtab", 'I');

  Is_True(symtab_file_name != 0 && strlen(symtab_file_name) != 0,
          ("IP_global_symtab_name failed to create unique filename"));

  return symtab_file_name;
}

extern "C" void IP_write_global_symtab(void)
{
  Temporary_Error_Phase ephase("Writing WHIRL file");

  static bool already_written = false;
  Is_True(!already_written,
          ("IP_write_global_symtab may only be called once"));
  already_written = true;

  // Change the global symtab so that all functions are marked as extern.
  For_all(St_Table, 1, externize());

  char* const symtab_file_name = IP_global_symtab_name();
  Output_File* symtab_file = WN_open_output(symtab_file_name);
  if (!symtab_file)
    ErrMsg ( EC_IR_Create, symtab_file_name, errno);

  PU_Info *pu_tree = NULL;
  if (! dummy_pu_list.empty ()) {
    pu_tree = Write_Dummy_PUs (symtab_file);
    DST_TYPE merged_dst = IPC_merge_DSTs(pu_tree, NULL, Malloc_Mem_Pool);
    // Write a the pu_info tree
    WN_write_PU_Infos (pu_tree, symtab_file);
    WN_write_dst(merged_dst, symtab_file);
  } else {
    // Write a dummy pu_info
    WN_write_PU_Infos (NULL, symtab_file);
    // Create and write an empty DST.  All it contains is a dummy compile unit.
    DST_TYPE empty_dst = DST_create(Malloc_Mem_Pool, 10);
    DST_mk_compile_unit(empty_dst, Malloc_Mem_Pool, DST_INVALID_IDX,
			"", "", "",
			DW_LANG_lo_user,
			DW_ID_case_sensitive);
    WN_write_dst(empty_dst, symtab_file);
  }

  WN_write_globals(symtab_file);
  WN_write_strtab(Index_To_Str(0), STR_Table_Size(), symtab_file);
#ifdef KEY
  if (Mod_Ref_Info_Table_Size() != 0)
    IPA_write_summary (IPA_irb_write_mod_ref_info, symtab_file);
#endif
  WN_write_revision(symtab_file);  

  add_to_tmp_file_list(symtab_file_name);
  WN_close_output(symtab_file);
  free(symtab_file);
}

void
IP_WRITE_pu_internal (PU_Info* pu, Output_File *outfile)
{
  Is_True(pu != 0, ("IP_WRITE_pu_internal: pu must not be null"));
  
  WN_MAP off_map = WN_MAP_UNDEFINED;

  /* Set up the current context */
  IPA_NODE_CONTEXT context (Get_Node_From_PU(pu));

  Is_True(CURRENT_SYMTAB > GLOBAL_SYMTAB,
          ("Lexical level of pu %s is %d, shouldn't be less than %d.",
           ST_name(St_Table[PU_Info_proc_sym(pu)]),
           CURRENT_SYMTAB,
           GLOBAL_SYMTAB + 1));

  WN_write_symtab (pu, outfile);

  if (PU_Info_state(pu, WT_AC_INTERNAL) == Subsect_InMem) {
    Is_True(PU_Info_state(pu, WT_ALIAS_CLASS) == Subsect_Missing,
	    ("IP_WRITE_pu_internal: Only one alias class subsection "
	     "at a time, please."));
    Write_AC_INTERNAL_Map = TRUE;
    Write_ALIAS_CLASS_Map = FALSE;
  }
  if (PU_Info_state(pu, WT_ALIAS_CLASS) == Subsect_InMem) {
    Is_True(PU_Info_state(pu, WT_AC_INTERNAL) == Subsect_Written,
	    ("IP_WRITE_pu_internal: AC_INTERNAL subsection must be "
	     "written."));
    Write_AC_INTERNAL_Map = FALSE;
    Write_ALIAS_CLASS_Map = TRUE;
  }

  MEM_POOL_Push(MEM_local_nz_pool_ptr);
  off_map = WN_MAP32_Create(MEM_local_nz_pool_ptr);

  if (IPA_Enable_Alias_Class) {
    Is_True(PU_Info_state(pu, WT_AC_INTERNAL) == Subsect_InMem,
	    ("IP_WRITE_pu_internal: AC_INTERNAL subsection "
	     "must be in memory"));
    Is_True((Write_ALIAS_CLASS_Map != 0) != (Write_AC_INTERNAL_Map != 0),
	    ("IP_WRITE_pu_internal: Exactly one ALIAS_CLASS mapping must "
	     "be written"));
    Is_True(((PU_Info_state(pu, WT_ALIAS_CLASS) == Subsect_InMem) ==
	     (Write_ALIAS_CLASS_Map != 0)),
	    ("IP_WRITE_pu_internal: Write_ALIAS_CLASS_Map inconsistent "
	     "with internal state"));
  }

  if (PU_Info_state (pu, WT_FEEDBACK) == Subsect_InMem)
      WN_write_feedback (pu, outfile);

#if defined(BACK_END) || defined(IR_TOOLS)
  if (PU_Info_state (pu, WT_SSA) == Subsect_InMem) {
      // We should call
      //    WN_write_SSA (pu, outfile);
      // But since WSSA doesn't support IPA so far. Only set the status
      Set_PU_Info_state(pu, WT_SSA, Subsect_Missing);
      PU_Info_subsect_size(pu, WT_SSA) = 0;
      PU_Info_subsect_offset(pu, WT_SSA) = 0;
  }
#endif

  WN_write_tree (pu, off_map, outfile);
   
  if (Write_ALIAS_CLASS_Map || Write_AC_INTERNAL_Map || 
      Write_ALIAS_CGNODE_Map) {

    if (Write_AC_INTERNAL_Map) {
      WN_write_voidptr_map(pu, off_map, outfile, WT_AC_INTERNAL,
			   WN_MAP_AC_INTERNAL,
			   "alias class intermediate map");
    }

    if (Write_ALIAS_CLASS_Map) {
      WN_write_INT32_map(pu, off_map, outfile, WT_ALIAS_CLASS,
			 WN_MAP_ALIAS_CLASS, "alias class map");
    }

    if (Write_ALIAS_CGNODE_Map) {
      WN_write_INT32_map(pu, off_map, outfile, WT_ALIAS_CGNODE,
			 WN_MAP_ALIAS_CGNODE, "alias cgnode map");
    }

    WN_MAP_Delete(off_map);
    MEM_POOL_Pop(MEM_local_nz_pool_ptr);
  }
}

// Write a PU, and all of its children.  (But not its siblings.)
void IP_write_PU_tree(Output_File* f, PU_Info* pu) {
  IP_WRITE_pu_internal(pu, f);

  PU_Info* prev_child = NULL;
  PU_Info* child = PU_Info_child(pu);
  while (child) {
    IPA_NODE* node = Get_Node_From_PU(child);
    IP_FILE_HDR& hdr = node->File_Header();
    IP_PROC_INFO& info = IP_FILE_HDR_proc_info(hdr)[node->Proc_Info_Index()];
    IPA_STATE_TYPE state = IP_PROC_INFO_state(info);

    Is_True(state == IPA_ORIG || state == IPA_MODIFIED ||
            state == IPA_WRITTEN || state == IPA_DELETED,
            ("PU %s has unexpected state %d",
             ST_name(St_Table[PU_Info_proc_sym(child)]),
             IP_PROC_INFO_state(info)));

    if (state != IPA_DELETED) {
      IP_write_PU_tree(f, child);
      Set_IP_PROC_INFO_state(info, IPA_WRITTEN);
      prev_child = child;
    }
    else {
      if (prev_child == NULL) {	// Skip this 1st child of this pu
	  PU_Info_child(pu) =  PU_Info_next(child);
      }
      else {
          PU_Info_next(prev_child) = PU_Info_next(child);
      }
    }
    child = PU_Info_next(child);
  }
}

// Create a copy of a pu tree.  Note that we copy old_pu and all of its
// children, but we do not copy old_pu's siblings.
PU_Info* copy_pu_tree(PU_Info* old_pu, MEM_POOL* p) {
  Is_True(old_pu != 0, ("copy_pu_tree: null pu pointer"));
  Is_True(p != 0, ("copy_pu_tree: null mempool pointer"));

  PU_Info* result = CXX_NEW(PU_Info(*old_pu), p);
  PU_Info_next(result) = 0;

  if (PU_Info_child(old_pu)) {
    PU_Info* cur = PU_Info_child(old_pu);
    PU_Info* tail = copy_pu_tree(cur, p);
    PU_Info_child(result) = tail;

    cur = PU_Info_next(cur);
    while (cur) {
      PU_Info_next(tail) = copy_pu_tree(cur, p);
      cur = PU_Info_next(cur);
      tail = PU_Info_next(tail);
    }
  }

  return result;
}


static PU_Info*
Write_Dummy_PUs (Output_File* output_file)
{
  PU_Info *pu_tree = NULL;
  for (DUMMY_GLOBAL_PRAGMA_PU::const_iterator it (dummy_pu_list.begin ());
       it != dummy_pu_list.end (); ++it) {

    PU_Info* pu = *it;
    Is_True(pu != 0, ("IPA output queue: pu must not be null"));
    Is_True(PU_lexical_level(&St_Table[PU_Info_proc_sym(pu)]) ==
	    GLOBAL_SYMTAB + 1,
	    ("IPA output queue is only for global pu's"));
    // Write pu to the file.
    IP_write_PU_tree(output_file, pu);
    
    // Add a copy of this pu to the list of pu infos.
    PU_Info* new_pu = copy_pu_tree(pu, Malloc_Mem_Pool);
    PU_Info_next(new_pu) = pu_tree;
    pu_tree = new_pu;
  }

  return pu_tree;
}


extern void WN_free_input (void *handle, off_t mapped_size);

// Free the resources associated with a pu tree.  There are two kinds
// of resources: ones associated with cg nodes, and ones associated with
// file headers.  The former is trivial: just get the call graph node and
// delete the mempool.  The latter is harder: for each file header, we 
// have to check whether we have already written out all pu's.  If so,
// then free it.

// This function loops through the pu tree, destroying every cg node
// mempool.  It also collects a list of all file headers associated
// with the tree.  (The list is not sorted and may contain duplicates.)

// This function processes pu, its children, and its siblings.
template <class Vector>
void free_pu_cg_resources(PU_Info* pu, Vector& file_hdr_list)
{
  Is_True(pu != 0, ("destroy_pu_tree: null pu pointer"));

  // Process children, if any.
  if (PU_Info_child(pu))
    free_pu_cg_resources(PU_Info_child(pu), file_hdr_list);

  // Process this pu and its siblings.
  for ( ; pu ; pu = PU_Info_next(pu)) {
    IPA_NODE* node = Get_Node_From_PU(pu);

#ifdef KEY
    // Builtins don't have file headers and their mem pool is not initialized.
    if (node->Is_Builtin()) {
      if (node->Mod_Ref_Info ())
	  node->Mod_Ref_Info ()->Free_Ref_Sets ();
      node->Set_Processed();
      continue;
    }
#endif

    // Save a pointer to the header.
    IP_FILE_HDR& hdr = node->File_Header();
    Is_True(IP_PROC_INFO_state(
                IP_FILE_HDR_proc_info(hdr)[node->Proc_Info_Index()])
            == IPA_WRITTEN,
            ("pu %s is not flagged as written",
             ST_name(St_Table[PU_Info_proc_sym(pu)])));
    file_hdr_list.push_back(&hdr);

    // Destroy the node's mempool.
    if (node->Mod_Ref_Info ())
	node->Mod_Ref_Info ()->Free_Ref_Sets ();
    MEM_POOL_Delete(node->Mem_Pool());
    node->File_Header().num_written++;
    if (node->File_Header().num_written == IP_FILE_HDR_num_procs(node->File_Header()))
      WN_free_input(IP_FILE_HDR_input_map_addr(node->File_Header()), node->File_Header().mapped_size);
    node->Clear_Mempool_Initialized();
    node->Set_Processed();
  }
}
  
// Should we destroy the file header?  

struct file_hdr_ptr_cmp {
  bool operator()(IP_FILE_HDR* x, IP_FILE_HDR* y) const {
    return reinterpret_cast<unsigned long>(x) <
           reinterpret_cast<unsigned long>(y);
  }
};

// Free all resources associated with a pu tree: top-level function.
void free_pu_resources(PU_Info* pu_tree, MEM_POOL* pool) {
  typedef std::vector<IP_FILE_HDR*, mempool_allocator<IP_FILE_HDR*> > 
          FILE_HDR_LIST_TYPE;
  FILE_HDR_LIST_TYPE file_hdr_list(pool);

  // Destroy all of the call graph mempools, and collect a list of file
  // headers.
 free_pu_cg_resources(pu_tree, file_hdr_list);
  Is_True(file_hdr_list.size() >= 1, ("free_pu_resources: no file headers"));

  // Remove duplicates from the list.
  sort(file_hdr_list.begin(), file_hdr_list.end(), file_hdr_ptr_cmp());
  FILE_HDR_LIST_TYPE::iterator new_end =
    std::unique(file_hdr_list.begin(), file_hdr_list.end());
  file_hdr_list.erase(new_end, file_hdr_list.end());
  Is_True(file_hdr_list.size() >= 1, ("free_pu_resources: no file headers"));

  Is_True(std::find(file_hdr_list.begin(), file_hdr_list.end(),
                    static_cast<IP_FILE_HDR*>(0)) == file_hdr_list.end(),
          ("free_pu_resources: file hdr list contains a null pointer"));

  // Loop through the list of file headers.  For each one, destroy its
  // mempool if appropriate.
  for (FILE_HDR_LIST_TYPE::iterator it = file_hdr_list.begin();
       it != file_hdr_list.end();
       ++it) 
    if (IP_FILE_HDR_all_procs_processed(**it))
      Destroy_File_Header(**it);
}


// This is an internal class that represents the IPA's binary output
// queue.  The major operations are adding a pu to the queue, and flushing
// the queue.  After the queue has been flushed, it is empty.

// Note that the queue's destructor asserts that the queue is empty.  That
// is, the queue must be flushed before it is destroyed.  

// There is a single global variable Output_Queue of type output_queue.
// It is automatically destroyed upon program termination; at that point,
// it will verify that it has been properly flushed.

// The output queue has its own mempool.  It is pushed when a new output file
// is begun, and it is popped when an output file is flushed.

class output_queue {
private:
  PU_Info* head;
  PU_Info* tail;
  Output_File* out_file;
  UINT32 nfiles;
  UINT32 ProMP_next_idx;

  DST_TYPE gbl_file_list;

  BOOL pool_initd;
  MEM_POOL pool;

public:
  output_queue();
  ~output_queue();

  // Does the queue have any entries?
  bool empty() const;

  // Called after a pu is added to the output queue.  Two extreme cases.
  // If it always returns true, then we will have one pu per output file.
  // If it always returns false, we will have all pu's in one file.  In
  // principle it ought not to do either; it should return true when the
  // output file reaches some size threshold.
  bool should_flush(const IPA_NODE* node);

  // Adds a pu to the output queue.  Preconditions: the pu is not a 
  // nested pu.
  void push(PU_Info* pu);

  // Flushes the queue.  This frees all resources associated with any
  // pu's that have been written since the queue was last flushed, and
  // it closes the output file (if any).
  void flush();

#ifndef _LIGHTWEIGHT_INLINER
  void build_global_filelists(IP_FILE_HDR& ip_fhdr,
                              incl_name_map_t& incl_map,
                              incl_name_map_t& fn_map);
#endif

private:
  // Helper function for flush().
  size_t pu_tree_add_comments(size_t index, size_t count, PU_Info* head);

  // Open the output file and push the mempool.
  void open_output_file();

  // Close the output file and pop the mempool.
  void close_output_file();
};  

// Definitions of output_queue member functions.

output_queue::output_queue()
  : head(0), tail(0), out_file(0), nfiles(0), ProMP_next_idx (1), gbl_file_list(NULL),
    pool_initd(FALSE),  pool()
{}

output_queue::~output_queue()
{
  if( pool_initd ) MEM_POOL_Pop_Unfreeze(&pool);   // pop global filelist
  Is_True(head == 0, ("IPA output queue has not been flushed"));
}

// Does the queue have any entries?
bool output_queue::empty() const {
  Is_True(head == 0 || out_file != 0,
          ("IPA output queue is not empty, but no output file exists"));
  Is_True(out_file == 0 || head != 0,
          ("IPA output file exists, but output queue is empty"));

  return head == 0;
}

// Called after a pu is added to the output queue.  Two extreme cases.
// If it always returns true, then we will have one pu per output file.
// If it always returns false, we will have all pu's in one file.  In
// principle it ought not to do either; it should return true when the
// output file reaches some size threshold.
//
// Note that this implementation is somewhat cheesebag because it
// doesn't check how many PU's have been queued up since the last
// flush. For its check to correspond to the right thing, we depend on
// the fact that every time should_flush() returns TRUE, a flush
// actually gets done. It seems like a bad idea to depend on that. Not
// only that, but it appears we don't count nested PU's in our tally
// of the current queue length.
// -- RK 980618
bool output_queue::should_flush(const IPA_NODE* node) 
{
  static UINT32 promp_id = 1;		// 0 is not a valid promp id
  static UINT32 count = 0;
  count += node->Weight ();

#ifdef KEY
  if (IPA_Enable_Source_PU_Order || Opt_Options_Inconsistent)
  {
      if (IPA_NODE::next_file_id != -1 &&
          IPA_NODE::next_file_id != node->File_Id())
      {
      // The next node we would write belongs to a different file, so close
      // the current file.
        if (ProMP_Listing) {
  	  ProMP_next_idx = promp_id;
	  promp_id = count;
        }
	count = 0;
	return TRUE;
      }
  }
#endif

  if (
#ifdef KEY
      // Do not consider output file size if following source code order.
      !IPA_Enable_Source_PU_Order && !Opt_Options_Inconsistent &&
#endif
      count >= IPA_Max_Output_File_Size) {
      if (ProMP_Listing) {
	ProMP_next_idx = promp_id;
	promp_id = count;
      }
      count = 0;
      return TRUE;
  } else
      return FALSE;
}

// Adds a pu to the output queue.  Preconditions: the pu is not a 
// nested pu.
void output_queue::push(PU_Info* pu) {
  Temporary_Error_Phase ephase("Writing WHIRL file");

  Is_True(pu != 0, ("IPA output queue: pu must not be null"));
  Is_True(PU_lexical_level(&St_Table[PU_Info_proc_sym(pu)]) ==
          GLOBAL_SYMTAB + 1,
          ("IPA output queue is only for global pu's"));

  // Open a new output file, if necessary.
  if (!out_file) {
    Is_True(head == 0 && tail == 0,
            ("No output file, but output queue is not empty"));
    if (nfiles == 0)            // This is the first time we've opened a file
      MEM_POOL_Initialize(&pool, "IPA bwrite mempool", FALSE);
    this->open_output_file();
    MEM_POOL_Push_Freeze(&pool);
    pool_initd = TRUE;
  }

  // Write pu to the file.
  IP_write_PU_tree(out_file, pu);

  // Add a copy of this pu to the list of pu infos.
  PU_Info* new_pu = copy_pu_tree(pu, &pool);
  if (head == 0)
    head = tail = new_pu;
  else {
    PU_Info_next(tail) = new_pu;
    tail = new_pu;
  }

#ifdef KEY
  // Write out any builtin function created by IPA.  Don't put the builtin at
  // the head of the pu infos list because output_queue::flush passes the head
  // pu to ipacom_process_file, which passes it to get_command_line, which then
  // accesses the file header with the pu.  Since the builtins don't have file
  // headers, this breaks the compiler.
  static int builtins_written = 0;
  if (!builtins_written) {
    builtins_written = 1;

    std::vector<IPA_BUILTIN*>::iterator it;
    for (it = IPA_builtins_list.begin(); it != IPA_builtins_list.end(); it++) {
      IPA_BUILTIN *ipa_builtin = *it;
      PU_Info *pu_info = ipa_builtin->Get_PU_Info();

      /* Set up the current context */
      IPA_NODE_CONTEXT context (Get_Node_From_PU(pu_info));

      WN_write_symtab (pu_info, out_file);

      WN_MAP off_map = WN_MAP_UNDEFINED;
      MEM_POOL_Push(MEM_local_nz_pool_ptr);
      off_map = WN_MAP32_Create(MEM_local_nz_pool_ptr);

      WN_write_tree (pu_info, off_map, out_file);

      WN_MAP_Delete(off_map);
      MEM_POOL_Pop(MEM_local_nz_pool_ptr);

      // Add a copy of this pu to the list of pu infos.
      Is_True(head, ("output_queue::push() head is 0"));
      PU_Info* new_pu = copy_pu_tree(pu_info, &pool);
      PU_Info_next(tail) = new_pu;
      tail = new_pu;
    }
  }
#endif

  Is_True(!this->empty(), ("output_queue::push() failed"));
          
}

#ifdef KEY
// Return the number of '\n' in pu_name.
static int
count_new_lines (const char * pu_name)
{
  int count = 0;

  for (char c = *pu_name; c != '\0'; c = *(++pu_name))
    if (c == '\n')
      count++;

  return count;
}


// BUF has '\n' characters, COUNT is the count of this PU_NAME.
// After each new-line character, insert "## " so that each new
// line starts with a comment character.
static void
fill_in_buffer (char * buf, int count, const char * pu_name)
{
  sprintf(buf, " %d: ", count);
  buf = strchr(buf, '\0');

  while (*pu_name != '\0')
  {
    *buf = *pu_name;
    if (*pu_name == '\n')
    {
      *(++buf) = '#';
      *(++buf) = '#';
      *(++buf) = ' ';
    }
    buf++;
    pu_name++;
  }
  *buf = '\0';
}
#endif

size_t
output_queue::pu_tree_add_comments(size_t index, size_t count, PU_Info* head)
{
  Is_True(head != 0, ("pu_tree_add_comments: no head"));
  while (head) {
    const size_t bufsize = 2048;
    const size_t padding = 16;
    static char static_buf[bufsize];

    const char* pu_name = ST_name(St_Table[PU_Info_proc_sym(head)]);
    Is_True(pu_name != 0, ("pu_tree_add_comments: null pu name"));
    
    char* buf = 0;
    const size_t pu_len = strlen(pu_name);
#ifdef KEY
    // Bug 14465: Count new-lines.
    int num_new_lines = count_new_lines(pu_name);
    const int total_len = pu_len + padding + num_new_lines * 5;
#else
    const int total_len = pu_len + padding;
#endif

    if (total_len < bufsize)
      buf = static_buf;
    else {
      buf = static_cast<char*>(malloc(total_len));
      if (!buf)
        ErrMsg (EC_No_Mem, "pu_tree_add_comment"); 
    }

#ifdef KEY
    // Bug 14465: Update buffer with makefile comments in
    // multiple names.
    if (num_new_lines)
      fill_in_buffer (buf, count++, pu_name);
    else
#endif

    sprintf(buf, " %lu: %s", (unsigned long)(count++), pu_name);
    ipacom_add_comment(index, buf);

    if (buf != static_buf)
      free(buf);

    if (PU_Info_child(head))
      count = pu_tree_add_comments(index, count, PU_Info_child(head));
    head = PU_Info_next(head);
  }

  return count;
}

// Flushes the queue.  This frees all resources associated with any
// pu's that have been written since the queue was last flushed, and
// it closes the output file (if any).
void output_queue::flush() {
  Temporary_Error_Phase ephase("Writing WHIRL file");

  if (!this->empty()) {

    if (IPA_Enable_ipacom) {
      const size_t index = ipacom_process_file(out_file->file_name, head,
					       ProMP_next_idx);
      ipacom_add_comment(index, "pu's contained in this file:");
      pu_tree_add_comments(index, 0, head);
    }

    // Must call IPC_merge_DSTs before WN_write_PU_Infos, because some
    // indices in the pu tree are updated in the DST merge.
    DST_TYPE merged_dst = IPC_merge_DSTs(head, gbl_file_list, &pool);
    WN_write_PU_Infos(head, out_file);
    WN_write_dst(merged_dst, out_file);

#if defined(TARG_SL)
    // The ipisr cg section may be rewritten in Perform_Alias_Class_Annotation
    if (ld_ipa_opt[LD_IPA_IPISR].flag)
        WN_write_isr_cg(ipisr_cg, out_file);
#endif // TARG_SL

    WN_write_revision(out_file);

    close_output_file();
    free_pu_resources(head, &pool);

    // Pop the mempool.
    MEM_POOL_Pop_Unfreeze(&pool);

    // Zero the head and tail pointers.
    head = 0;
    tail = 0;
  }
}

void output_queue::open_output_file() {
  Temporary_Error_Phase ephase("Writing WHIRL file");
  Is_True(out_file == 0, ("ipa output queue: file is already open"));

  char buffer[PATH_MAX];
  sprintf(buffer, "%u", ++nfiles);
  char* new_file_name = create_unique_file(buffer, 'I');
  Output_File* new_file = WN_open_output(new_file_name);
  if (!new_file) {
    ErrMsg ( EC_IR_Create, new_file_name, errno);
  }
  add_to_tmp_file_list(new_file->file_name);
  if (IPA_Enable_Alias_Class) {
    // Remember that the annotation phase will need to go through the
    // new file.
    Ip_alias_class_files.push_back(new_file->file_name);
  }
  this->out_file = new_file;
}

void output_queue::close_output_file() {
  Is_True(out_file != 0, ("ipa output queue: no file to close"));
  Is_True (nfiles > 0,
           ("ipa output queue: nfiles == %u, should be >= 1", nfiles));

  WN_close_output(out_file);
  free(out_file);
  out_file = 0;
}

#ifndef _LIGHTWEIGHT_INLINER
void
output_queue::build_global_filelists(IP_FILE_HDR& ip_fhdr,
                                     incl_name_map_t& incl_map,
                                     incl_name_map_t& fn_map)
{
  if (!pool_initd ) {
    MEM_POOL_Initialize(&pool, "IPA bwrite mempool", FALSE);
    pool_initd = TRUE;
  }

  if( gbl_file_list== NULL ) {
    DST_Type* gbl_flist;
    MEM_POOL_Push_Freeze(&pool);
    gbl_file_list = DST_create(&pool);
    gbl_flist = (DST_Type*)gbl_file_list;
    gbl_flist->block_list[DST_include_dirs_block] = DST_include_dirs_block;
    DST_new_block(gbl_file_list, &pool, DST_include_dirs_block, sizeof(block_header));
    gbl_flist->block_list[DST_file_names_block] = DST_file_names_block;
    DST_new_block(gbl_file_list, &pool, DST_file_names_block,sizeof(block_header));
  }

  Add_files_to_global_file_list(IP_FILE_HDR_dst(ip_fhdr), gbl_file_list, incl_map, fn_map, &pool);
}
#endif


// The global IPA binary output queue.
output_queue Output_Queue;

static void
clean_up_deleted_nested_pu_info(PU_Info* pu)
{
  PU_Info* prev_child = NULL;  
  PU_Info* child = PU_Info_child(pu);
  while (child) {
    ST* child_st = &St_Table[PU_Info_proc_sym(child)];
    if (ST_is_not_used (child_st) ) {
      if (prev_child == NULL) { // Skip this 1st child of this pu
          PU_Info_child(pu) =  PU_Info_next(child);
      }
      else {
          PU_Info_next(prev_child) = PU_Info_next(child);
      }
    }
    else 
	prev_child = child;
    child = PU_Info_next(child);
  }
}


// Writes a pu, and enters it in the binary output queue.  The pu itself
// is written immediately, but the pu_info and any DST information is
// deferred until the output queue is flushed.  This function ignores any
// nested pu's; a nested pu is always written out at the same time as
// its parent.

extern "C" void IP_WRITE_pu (IP_FILE_HDR *s , INT pindex)
{
  Temporary_Error_Phase ephase("Writing WHIRL file");

  Is_True(s != 0, ("IP_WRITE_pu: file header must not be null"));
  Is_True(pindex >= 0 && pindex < IP_FILE_HDR_num_procs(*s),
          ("IP_WRITE_pu: index %d out of range.  Should be 0 <= index < %d",
           pindex, IP_FILE_HDR_num_procs(*s)));

  IP_PROC_INFO* proc_info = IP_FILE_HDR_proc_info(*s) + pindex;
  Is_True(proc_info != 0, ("IP_WRITE_pu: proc_info must not be null"));
  FmtAssert(IP_PROC_INFO_state(*proc_info) != IPA_WRITTEN,
	     ("IP_WRITE_pu: pu has already been written"));
  FmtAssert(IP_PROC_INFO_state(*proc_info) != IPA_UNUSED &&
	     IP_PROC_INFO_state(*proc_info) != IPA_DELETED,
	     ("IP_WRITE_pu: pu has bad state flag %d",
	      IP_PROC_INFO_state(*proc_info)));
  
                                // Flag the pu as written.
  Set_IP_PROC_INFO_state(*proc_info, IPA_WRITTEN);
  Inc_IP_FILE_HDR_num_procs_processed(*s);

  PU_Info* pu = IP_PROC_INFO_pu_info(*proc_info);

  if (pu) {                     // Skip alternate entry points.

    // Clean up nested procedures that have been deleted
    clean_up_deleted_nested_pu_info(pu);

    IPA_NODE* node = Get_Node_From_PU (pu);

    if (Alias_Nystrom_Analyzer)
      IPA_NystromAliasAnalyzer::aliasAnalyzer()->updateCGForBE(node);

    if (IPA_Enable_Alias_Class) {
      Temporary_Error_Phase ephase("Alias class analysis");

      /* Set up the current context */
      IPA_NODE_CONTEXT context (node);

      Is_True(CURRENT_SYMTAB > GLOBAL_SYMTAB,
	      ("Lexical level of pu %s is %d, shouldn't be less than %d.",
	       ST_name(St_Table[PU_Info_proc_sym(pu)]),
	       CURRENT_SYMTAB,
	       GLOBAL_SYMTAB + 1));

#ifdef KEY
      // Bug 14465: dummy function for global-scope ASM?
      if (ST_class(St_Table[PU_Info_proc_sym(pu)]) != CLASS_NAME)
#endif
      Ip_alias_class->Classify_memops(PU_Info_tree_ptr(pu));

      // Classify the initialized data after seeing the code so we get
      // the benefit of all the available function arity information.
      Ip_alias_class->
	Classify_initialized_data(Scope_tab[CURRENT_SYMTAB].inito_tab);

      // The alias class intermediate map is set now. Note that the
      // mapping is in memory.
      Set_PU_Info_state(pu, WT_AC_INTERNAL, Subsect_InMem);
    }

    // Skip nested pu's.
    int lexical_level = PU_lexical_level(&St_Table[PU_Info_proc_sym(pu)]);
    Is_True(lexical_level >= GLOBAL_SYMTAB + 1,
            ("Lexical level of pu %s is %d, shouldn't be less than %d.",
             ST_name(St_Table[PU_Info_proc_sym(pu)]),
             lexical_level,
             GLOBAL_SYMTAB + 1));
    if (lexical_level == GLOBAL_SYMTAB + 1) {
      if (PU_has_global_pragmas (Get_Node_From_PU (pu)->Get_PU ())) {
	dummy_pu_list.push_back (pu);
      } else {
	Output_Queue.push(pu);
	if (Output_Queue.should_flush(node))
	  IP_flush_output();
      }
    }
  }
}

extern "C" void IP_flush_output(void) 
{
  Output_Queue.flush();
}

#ifndef _LIGHTWEIGHT_INLINER
void
IP_build_global_filelists(IP_FILE_HDR& ip_fhdr, incl_name_map_t& incl_map, incl_name_map_t& fn_map)
{
  Output_Queue.build_global_filelists(ip_fhdr, incl_map, fn_map);
}
#endif
