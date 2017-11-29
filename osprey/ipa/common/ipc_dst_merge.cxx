/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#include <stdint.h>
#include <dwarf.h>
#include <vector>
#include <map>
#include <list>
#include <algorithm>

#include <ext/hash_map>

#include "mempool.h"
#include "mempool_allocator.h"
#define USE_STANDARD_TYPES
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"      // Basic definition of DST data structure.
#include "dwarf_DST.h"
#include "pu_info.h"
#include "ipc_dst_utils.h"
#include "ipc_file.h"           // For IP_FILE_HDR
#include "ipa_cg.h"             // For Get_Node_From_PU

#include "ipc_dst_merge.h"

// Merge DSTs.  The steps are
// ** Determine which DSTs, and which subprograms within the DSTs,
//    we're going to merge.  Within each DST, we need to merge the
//    subprograms that are explicitly mentioned in the pu list, and
//    the transitive closure of subprograms that they refer to.
//    The data structure we will use is a map from a DST_TYPE to
//    vector of DST_IDXs within that DST.  The vector is sorted by
//    DST_IDX.
// ** Merge all of the include directories, and merge the filenames.
//    To merge the filenames from a particular input DST, we need to
//    know the number of include dirs merged in all previous input DSTs.
//    (As an offset to the include directories' ordinal numbers.)
// ** Merge the compile unit nodes and the subprogram nodes.
//    For each input DST:
//      -- Copy its compile unit node (assumed to be only one) to the
//         merged DST.  Copy all of its subprogram nodes to the merged
//         DST, as children of the newly created subprogram nodes.  
//         The compile unit nodes in the merged DST form a singly
//         linked list, and the subprogram nodes underneath any particular
//         compile unit also form a singly linked list.
//      -- After entering all of the DST's subprogram nodes in the merged
//         DST, loop through the newly entered nodes to update cross-
//         references.
// ** Update the references to DST indices in the pu list.

// Typedefs for the data structures used in the merge process:
// a vector of pairs (old index, new index), and a map from DST_TYPE
// to such a vector.

typedef std::pair<DST_IDX, DST_IDX> idx_pair;
struct idx_pair_less {
  bool operator()(const idx_pair& x, const idx_pair& y) const {
    return x.first < y.first;
  }
};
typedef mempool_allocator<idx_pair> idx_pair_allocator;
typedef std::vector<idx_pair, idx_pair_allocator> idx_pair_vector;

typedef std::pair<const DST_TYPE, idx_pair_vector> dst_hash_value_type;
typedef mempool_allocator<dst_hash_value_type> dst_hash_allocator;
struct DST_TYPE_hash {
  size_t operator()(DST_TYPE p) const {
    return reinterpret_cast<size_t>(p);
  }
};
typedef __gnu_cxx::hash_map<DST_TYPE, idx_pair_vector,
                      DST_TYPE_hash, std::equal_to<DST_TYPE>,
                      dst_hash_allocator>
        dst_hash_map;


namespace {                     // Unnamed namespace: internal functions
                                // used in the DST merge.

// Initialize the map from DST_TYPE to vector.  For each pu,
// enter its DST in the map (if it's not there already) and
// enter the pu's DST_IDX in the appropriate vector.  This 
// function does not compute the transitive closure.

void initialize_dst_map(dst_hash_map& M, pu_info* pu) 
{
  Is_True(pu != 0, ("No pu"));

  while (pu) {
    IPA_NODE* cg_node = Get_Node_From_PU(pu);

#ifdef KEY
    // IPA builtins don't have dst.
    if (cg_node->Is_Builtin()) {
      pu = PU_Info_next(pu);
      continue;
    }
#endif

    DST_TYPE dst = IP_FILE_HDR_dst(cg_node->File_Header());
    Is_True(dst != 0, ("No dst"));

    // This is necessary to prevent a memory leak: make sure that the
    // new vector is created with the correct mempool allocator.
    if (M.find(dst) == M.end())
      M.insert(std::make_pair(dst, idx_pair_vector(M.get_allocator())));
    
    if( !DST_IS_NULL(PU_Info_pu_dst(pu) ))
      M[dst].push_back(idx_pair(PU_Info_pu_dst(pu), DST_INVALID_IDX));

    if (PU_Info_child(pu))
      initialize_dst_map(M, PU_Info_child(pu));

    pu = PU_Info_next(pu);
  }
}

// Each vector in the map M contains a list of DST_IDX's that point to 
// subprogram nodes.  Those subprogram nodes may refer to other subprogram
// nodes that aren't yet on the list.  Add all such nodes (and the nodes
// that they in turn refer to, etc.) to the list.  At the end, every 
// vector in M will be sorted. 
void construct_transitive_closure(dst_hash_map& M) 
{
  for (dst_hash_map::iterator map_iter = M.begin();
       map_iter != M.end();
       ++map_iter) {

    DST_TYPE dst = map_iter->first;
    idx_pair_vector& V = map_iter->second;
    idx_pair_vector tmp(V.get_allocator());

    std::sort(V.begin(), V.end(), idx_pair_less());
    do {
      tmp.clear();              // Vector of indices that aren't yet in V
                                // but that should be.
      for (idx_pair_vector::const_iterator i = V.begin();
           i != V.end();
           ++i) {
        Is_True( !DST_IS_NULL(i->first) ,
                ("DST index vector contains an invalid index"));
        DST_INFO* info = DST_get_info(dst, i->first);
        DST_SUBPROGRAM* subpr = DST_get_subprogram_attr(dst, info);

        if (DST_SUBPROGRAM_has_spec(info)) {
          DST_IDX spec = DST_SUBPROGRAM_spec(info, subpr);
          if( !DST_IS_NULL(spec) ) {
            idx_pair spec_pair(spec, DST_INVALID_IDX);
            if (!std::binary_search(V.begin(), V.end(), spec_pair,
                                    idx_pair_less()))
              tmp.push_back(spec_pair);
          }
        }
        if (DST_SUBPROGRAM_has_origin(info)) {
          DST_IDX origin = DST_SUBPROGRAM_origin(info, subpr);
          if( !DST_IS_NULL(origin) ) {
            idx_pair origin_pair(origin, DST_INVALID_IDX);
            if (!std::binary_search(V.begin(), V.end(), origin_pair,
                                    idx_pair_less()))
              tmp.push_back(origin_pair);
          }
        }
      }
      std::sort(tmp.begin(), tmp.end(), idx_pair_less());
      idx_pair_vector::size_type N = V.size();
      V.insert(V.end(), tmp.begin(), tmp.end());
      std::inplace_merge(V.begin(), V.begin() + N, V.end(),
                         idx_pair_less());
    } while (!tmp.empty());
  }
}

static void global_file_index_update(mUINT64& srcpos, incl_name_map_t& fn_map)
{
  UINT32 mapped_fn = fn_map[SRCPOS_filenum(srcpos)];

  FmtAssert(mapped_fn != 0,
            ("mapped file number is zero, filenum=%u, linenum=%d",
             SRCPOS_filenum(srcpos),  SRCPOS_linenum(srcpos)));
  SRCPOS_filenum(srcpos) = mapped_fn;
}

// fix-up the file nbr part of the USRCPOS in DST nodes that contain USRCPOSs
void
fix_up_dst_srcpos(DST_TYPE dst, PU_Info *pu_tree)
{
  PU_Info* pu = pu_tree;
  
  while (pu) {
    if( !DST_IS_NULL(PU_Info_pu_dst(pu)) ) {

      IP_FILE_HDR& ip_fhdr = Get_Node_From_PU(pu)->File_Header();
      incl_name_map_t& fn_map = IP_FILE_HDR_fn_map(ip_fhdr);
      DST_INFO* info = DST_get_info(dst, PU_Info_pu_dst(pu));
      DST_SUBPROGRAM* subprog = DST_get_subprogram_attr(dst, info);

      // See comments on the typedef for DST_SUBPROGRAM, the logic
      // below matches the comments associated with each union member.
      if( DST_IS_memdef(DST_INFO_flag(info)) ) {
        global_file_index_update(DST_SUBPROGRAM_memdef_decl(subprog).srcpos, fn_map);
      }
      else if( DST_IS_declaration(DST_INFO_flag(info)) ) {
        global_file_index_update(DST_SUBPROGRAM_decl_decl(subprog).srcpos, fn_map);
      }
      else {
        global_file_index_update(DST_SUBPROGRAM_def_decl(subprog).srcpos, fn_map);
      }
    }      
    pu = pu->next;
  }
}

// Merge all of the compile units, and all of the mentioned subprogram nodes.
void merge_subprograms(dst_hash_map& M, MEM_POOL* p, DST_TYPE output)
{
  using std::binary_search;
  using std::lower_bound;

  DST_IDX cu_idx = DST_INVALID_IDX;

  for (dst_hash_map::iterator dst_iter = M.begin();
       dst_iter != M.end();
       ++dst_iter) {
    DST_TYPE src = dst_iter->first;
    idx_pair_vector& V = dst_iter->second;

    DST_IDX src_cu_idx = DST_get_compile_unit(src);
    Is_True( DST_IS_NULL( DST_INFO_sibling(DST_get_info(src, src_cu_idx)) ),
            ("Input dst should have only one compile unit"));

    // Copy the compile unit.
    cu_idx = DST_copy_compile_unit(src, output, p, cu_idx, src_cu_idx);
                             
    // Copy all of the mentioned subprogram units.
    DST_IDX subpr_idx = DST_INVALID_IDX;
    idx_pair_vector::iterator i;

    for (i = V.begin(); i < V.end(); ++i) {
      i->second = subpr_idx =
        DST_copy_subprogram(src, output, p, cu_idx, subpr_idx, i->first);
      Is_True( !DST_IS_NULL(subpr_idx),
              ("Index of new subprogram unit is invalid"));
    }

    // Fix up cross-references.
    for (i = V.begin(); i < V.end(); ++i) {
      DST_INFO* src_sub_info = DST_get_info(src, i->first);
      DST_INFO* sub_info = DST_get_info(output, i->second);
      DST_SUBPROGRAM* src_sub = DST_get_subprogram_attr(src, src_sub_info);
      DST_SUBPROGRAM* sub = DST_get_subprogram_attr(output, sub_info);

      if (DST_SUBPROGRAM_has_spec(src_sub_info)) {
        DST_IDX src_spec = DST_SUBPROGRAM_spec(src_sub_info, src_sub);
        if( !DST_IS_NULL(src_spec) ) {
          idx_pair p(src_spec, DST_INVALID_IDX);
          Is_True(binary_search(V.begin(), V.end(), p, idx_pair_less()),
                  ("Input spec index not found in index vector"));
          Is_True( DST_ARE_EQUAL( lower_bound(V.begin(), V.end(), p,
                                              idx_pair_less())->first,
                                  src_spec),
                  ("Inconsistent index vector"));
          Is_True( !DST_ARE_EQUAL( lower_bound(V.begin(), V.end(), p,
                                               idx_pair_less())->second, 
                                   DST_INVALID_IDX),
                  ("Input spec index maps to invalid index"));
          DST_SUBPROGRAM_spec(sub_info, sub) = 
            lower_bound(V.begin(), V.end(), p, idx_pair_less())->second;
        }
      }

      if (DST_SUBPROGRAM_has_origin(src_sub_info)) {
        DST_IDX src_origin = DST_SUBPROGRAM_origin(src_sub_info, src_sub);
        if( !DST_IS_NULL(src_origin) ) {
          idx_pair p(src_origin, DST_INVALID_IDX);
          Is_True(binary_search(V.begin(), V.end(), p, idx_pair_less()),
                  ("Input origin index not found in index vector"));
          Is_True( DST_ARE_EQUAL( lower_bound(V.begin(), V.end(), p,
                                              idx_pair_less())->first,
                                  src_origin),
                  ("Inconsistent index vector"));
          Is_True( !DST_ARE_EQUAL( lower_bound(V.begin(), V.end(), p,
                                               idx_pair_less())->second,
                                   DST_INVALID_IDX),
                  ("Input origin index maps to invalid index"));
          DST_SUBPROGRAM_origin(sub_info, sub) = 
            lower_bound(V.begin(), V.end(), p, idx_pair_less())->second;
        }
      }
    }
  }
}

void update_pu_dst_indices(dst_hash_map& M, pu_info* pu) 
{
  Is_True(pu != 0, ("No pu"));

  while (pu) {
    if( !DST_IS_NULL(PU_Info_pu_dst(pu)) ) {
      IPA_NODE* cg_node = Get_Node_From_PU(pu);
      DST_TYPE dst = IP_FILE_HDR_dst(cg_node->File_Header());
      Is_True(dst != 0, ("No dst"));

      Is_True(M.find(dst) != M.end() && M.find(dst)->first == dst,
              ("DST not present in map"));

      idx_pair_vector& V = M.find(dst)->second;
      idx_pair p(PU_Info_pu_dst(pu), DST_INVALID_IDX);
      Is_True(std::binary_search(V.begin(), V.end(), p, idx_pair_less()),
              ("DST index not present in map"));
      Is_True( DST_ARE_EQUAL( std::lower_bound(V.begin(), V.end(), p,
                                               idx_pair_less())->first,
                               PU_Info_pu_dst(pu) ),
              ("DST index not present in map"));
      Is_True( !DST_ARE_EQUAL( std::lower_bound(V.begin(), V.end(), p,
                                                idx_pair_less())->second,
                               DST_INVALID_IDX ),
              ("DST index maps to incorrect value"));
      PU_Info_pu_dst(pu) = std::lower_bound(V.begin(), V.end(), p,
                                         idx_pair_less())->second;
    }

    if (PU_Info_child(pu))
      update_pu_dst_indices(M, PU_Info_child(pu));

    pu = PU_Info_next(pu);
  }
}

} // Close the unnamed namespace.

typedef std::vector<const char*> incl_dir_list_t;
incl_dir_list_t dir_list;
typedef std::pair<const char*,INT32> fn_dirNbr_t;
typedef std::vector<fn_dirNbr_t> fn_list_t;
fn_list_t fn_list;

// NOTE that if the path string is found, we return the entry+1 because the returned
// value will be used (via the maps) to update the DST structures and the DSTs begin
// counting at 1
static INT32
find_dir_path_in_list(const char* str)
{
    incl_dir_list_t::iterator i;
    INT32 cnt = 0;

    for(i = dir_list.begin(); i != dir_list.end(); ++i ) {
      if( strcmp(str, *i) == 0 ) return cnt+1;
      cnt++;
    }
    return -1;
}

static INT32
add_dir_to_list(const char* str)
{
    dir_list.push_back(str);
    return dir_list.size();
}

// NOTE that if the fn_str is found, we return the entry+1 because the returned value
// will be used (via the maps) to update the DST structures and the DSTs begin counting
// at 1
static INT32
find_fn_in_list(const char* fn_str,  const char* orig_dir_str)
{
    fn_list_t::iterator i;
    INT32 cnt = 0;

    for(i = fn_list.begin(); i != fn_list.end(); ++i ) {
      if( strcmp(fn_str, i->first) == 0 ) {
        const char* new_dir_str = dir_list[i->second-1];
        if( strcmp(orig_dir_str, new_dir_str) == 0 ) {
          return cnt+1;
        }
      }
      cnt++;
    }
    return -1;
}

static INT32
add_fn_to_list(const char* fn, INT32 dir_nbr)
{
    fn_list.push_back(fn_dirNbr_t(fn, dir_nbr));
    return fn_list.size();
}

void
Add_files_to_global_file_list(DST_TYPE src, DST_TYPE dest,
                              incl_name_map_t& incl_map,
                              incl_name_map_t& fn_map,
                              MEM_POOL* p)
{
  Is_True( src != NULL, ("Input DST is NULL"));
  Is_True( dest != NULL, ("Output DST is NULL"));

  DST_IDX wrk_idx = DST_INVALID_IDX;
  DST_IDX cur_include_dir = DST_INVALID_IDX;
  DST_IDX cur_filename = DST_INVALID_IDX;
  INT32 old_fn_nbr = 1;
  INT32 new_fn_nbr;
  INT32 old_dir_nbr = 1;
  INT32 new_dir_nbr;

  // get the last entry in the incl dir list
  for( wrk_idx = DST_get_include_dirs(dest);
       !DST_IS_NULL(wrk_idx);
       wrk_idx = DST_INCLUDE_DIR_next(DST_get_include_dir(dest,wrk_idx)) ){
    cur_include_dir = wrk_idx;
  }

  // Add these include paths to the global include paths.
  DST_IDX old_dir = DST_get_include_dirs(src);
  if( DST_IS_NULL(DST_get_include_dirs(dest)) ) {
    DST_set_current_block(dest, ((DST_Type*)dest)->block_list[DST_include_dirs_block]);
  }
  while( !DST_IS_NULL(old_dir) ) {
    DST_INCLUDE_DIR* dir = DST_get_include_dir(src, old_dir);
    const char* dir_str = DST_IDX_to_string(src, DST_INCLUDE_DIR_path(dir));

    if( (new_dir_nbr = find_dir_path_in_list(dir_str)) == -1 ) {
      cur_include_dir = DST_copy_include_dir(src, dest, p, cur_include_dir, old_dir);
      new_dir_nbr = add_dir_to_list(dir_str);
    }
    old_dir = DST_INCLUDE_DIR_next(DST_get_include_dir(src, old_dir));
    incl_map[old_dir_nbr] = new_dir_nbr;
    old_dir_nbr++;
  }

  // get the last entry in the file list
  for( wrk_idx = DST_get_file_names(dest);
       !DST_IS_NULL(wrk_idx);
       wrk_idx = DST_FILE_NAME_next(DST_get_file_name(dest, wrk_idx)) ) {
    cur_filename = wrk_idx;
  }

  // Add these files to the global files
  DST_IDX old_file = DST_get_file_names(src);
  if( DST_IS_NULL(DST_get_file_names(dest)) ) {
    DST_set_current_block(dest, ((DST_Type*)dest)->block_list[DST_file_names_block]);
  }
  while( !DST_IS_NULL(old_file) ) {
    DST_FILE_NAME* fn = DST_get_file_name(src, old_file);
    const char* fn_str = DST_IDX_to_string(src, DST_FILE_NAME_name(fn));
    UINT16 new_dir_nbr = incl_map[DST_FILE_NAME_dir(fn)];
    const char* src_dir_str = dir_list[new_dir_nbr-1];

    if( (new_fn_nbr = find_fn_in_list(fn_str, src_dir_str)) == -1 ) {
      cur_filename = DST_copy_filename(src, dest, p, cur_filename, old_file, new_dir_nbr);
      new_fn_nbr = add_fn_to_list(fn_str, new_dir_nbr);
    }
    old_file = DST_FILE_NAME_next(DST_get_file_name(src, old_file));
    fn_map[old_fn_nbr] = new_fn_nbr;
    old_fn_nbr++;
  }
}

void
dump_path_file_lists(DST_TYPE dst)
{
  DST_IDX idx;
  UINT64 file_size;
  UINT64 fmod_time;
  const char *dirname;
  int i;

  i= 1;
  idx = DST_get_include_dirs(dst);
  while( !DST_IS_NULL(idx) ) {
    DST_INCLUDE_DIR *dir = DST_get_include_dir(dst, idx);
    fprintf(stderr, "indir[%d]: %s\n", i,  DST_IDX_to_string(dst,
                                                             DST_INCLUDE_DIR_path(dir)));
    idx = DST_INCLUDE_DIR_next(dir);

    i++;
  }
  i = 1;
  idx = DST_get_file_names(dst);
  while( !DST_IS_NULL(idx) ) {
    DST_FILE_NAME *f = DST_get_file_name(dst, idx);
    fprintf(stderr, "filename[%d]: %s\n", i, DST_IDX_to_string(dst,
                                                               DST_FILE_NAME_name(f)));
    idx = DST_FILE_NAME_next(f);
    i++;
  }
}

// Preconditions: pu_tree isn't null, output is empty.
DST_TYPE IPC_merge_DSTs(pu_info* pu_tree, DST_TYPE gbl_dst,  MEM_POOL* p) 

{
  dst_hash_map M(100, DST_TYPE_hash(), std::equal_to<DST_TYPE>(),
                 dst_hash_allocator(p));

  // Get a list of the DSTs we are to merge, and the subprograms within
  // those DSTs.
  initialize_dst_map(M, pu_tree);
  construct_transitive_closure(M);

  // Create the output DST.
  DST_TYPE output = DST_create(p);

  if (gbl_dst != NULL) {
    // copy the global incl paths and filenames to the output file
    DST_Type* g =  static_cast<DST_Type*>(gbl_dst);
    DST_Type* o =  static_cast<DST_Type*>(output);
    copy_DST_Type(g, o, p);
  }

  // Merge the compile unit and subprogram nodes, updating the
  // entries in the DST map.
  merge_subprograms(M, p, output);

  // Update the DST indices in the pu tree.
  update_pu_dst_indices(M, pu_tree);

  // Update the file component of all USRCPOSs in the DSTs
  fix_up_dst_srcpos(output, pu_tree);

  return output;
}

void dump_incl_name_map_t(incl_name_map_t *ptm)
{
  printf("size=%lu\n", (unsigned long)ptm->size());
  for (std::map<int, int, std::less<int>, std::allocator<int> >::iterator it = ptm->begin();
      it != ptm->end(); ++it)
    printf("%d/%d\n", it->first, it->second);
}
