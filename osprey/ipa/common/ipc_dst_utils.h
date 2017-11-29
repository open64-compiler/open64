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
#ifndef __cplusplus
#error This header must be compiled as C++
#endif

// Internally, a DST is a struct of type DST_Type.  The external interface,
// however, instead uses parameters of type DST_TYPE.  A parameter of
// type DST_TYPE is a DST_Type* cast to void*.  In order to refer to
// the type DST_Type, you must define the macro USE_DST_INTERNALS before
// including dwarf_dst_mem.h.

#ifndef IPC_DST_UTILS_included
#define IPC_DST_UTILS_included

#define USE_STANDARD_TYPES
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"      // Basic definition of DST data structure.
#include "dwarf_DST.h"          // Definitions of symbolic constants.
#include "mempool.h"
#include "errors.h"

#include <iterator>

//-----------------------------------------------------------------
// Functions for access to information in a DST.


// Convert a DST_IDX to a pointer of type void*.  This is similar
// to DST_idx_to_string from dwarf_DST_mem.h.  The main difference 
// is that in DST_idx_to_string the DST_IDX always points into 
// Current_DST, but in DST_IDX_to_ptr the DST is an explicit parameter
// of type DST_TYPE.

inline void* DST_IDX_to_ptr(DST_TYPE dst_vptr, DST_IDX idx) {
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);

  Is_True(dst != 0, ("DST pointer is null"));
  Is_True(idx.block_idx != DST_INVALID_BLOCK_IDX &&
          idx.block_idx >= 0 &&
          idx.block_idx <= dst->last_block_header,
          ("Invalid DST index %d:%d", idx.byte_idx, idx.block_idx));
  block_header& block = dst->dst_blocks[idx.block_idx];
  Is_True(idx.byte_idx != DST_INVALID_BYTE_IDX &&
          idx.byte_idx >= 0 &&
          idx.byte_idx < block.size,
          ("Invalid DST index %d:%d", idx.byte_idx, idx.block_idx));
  return block.offset + idx.byte_idx;
}

inline char* DST_IDX_to_string(DST_TYPE dst, DST_IDX idx) {
  return static_cast<char*>(DST_IDX_to_ptr(dst, idx));
}

// Given the index of a DST block, return that block.
inline block_header& DST_get_block(DST_TYPE dst_vptr, DST_BLOCK_IDX block_idx)
{
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);

  Is_True(dst != 0, ("DST pointer is null"));
  Is_True(block_idx >= 0 && block_idx <= dst->last_block_header,
          ("Invalid block idx %d, should be in range 0 <= idx <= %d",
           block_idx, dst->last_block_header));
  return dst->dst_blocks[block_idx];
}

// Has anything been allocated within this block?
inline bool DST_block_is_empty(const block_header& block) {
  Is_True(block.size >= 0,
          ("Block size is %d, should be nonnegative", block.size));
  return block.size == 0;
}

// Does a block of this kind exist?
inline bool DST_has_block_of_kind(DST_TYPE dst_vptr, DST_BLOCK_KIND kind) {
  Is_True(dst_vptr != 0, ("DST pointer is null"));
  Is_True(kind >= DST_include_dirs_block && kind < DST_noblock,
          ("Invalid block kind"));
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  Is_True(dst->block_list[kind] == DST_INVALID_BLOCK_IDX ||
          DST_get_block(dst_vptr, dst->block_list[kind]).kind == kind,
           ("Inconsistent block list: kind is %d, should be %d",
           DST_get_block(dst_vptr, dst->block_list[kind]).kind, kind));
  return dst->block_list[kind] != DST_INVALID_BLOCK_IDX;
}

// Find the first block of this kind.  Precondition: at least one block
// of this kind exists.
inline block_header& 
DST_first_block_of_kind(DST_TYPE dst_vptr, DST_BLOCK_KIND kind) {
  Is_True(dst_vptr != 0, ("DST pointer is null"));
  Is_True(kind >= DST_include_dirs_block && kind < DST_noblock,
          ("Invalid block kind"));
  Is_True(DST_has_block_of_kind(dst_vptr, kind),
          ("No block of kind %d", kind));

  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  return DST_get_block(dst, dst->block_list[kind]);
}

// Does the DST have at least one compile unit?
inline bool DST_has_compile_unit(DST_TYPE dst_vptr) {
  return DST_has_block_of_kind(dst_vptr, DST_file_scope_block) &&
         !DST_block_is_empty(DST_first_block_of_kind(dst_vptr,
                                                     DST_file_scope_block));
}

// Get the index of the DST's first compile unit node.  Precondition: 
// a compile unit node exists.
DST_IDX DST_get_compile_unit(DST_TYPE dst);

// Get the index of the DST's first include dir node.  No such node need
// exist; if none does, the return value is DST_INVALID_IDX.
DST_IDX DST_get_include_dirs(DST_TYPE);

// Get the index of the DST's first file names node.  No such node need
// exist; if none does, the return value is DST_INVALID_IDX.
DST_IDX DST_get_file_names(DST_TYPE);

// copy the source DST_Type to the dest DST+_Type
void copy_DST_Type(DST_TYPE src, DST_TYPE dest, MEM_POOL* p);

// build a new dest block and copy the contents of the src block to it
void copy_block(DST_TYPE src, DST_TYPE dest, DST_BLOCK_KIND kind, MEM_POOL* p);

// Converts a DST_IDX into a pointer to DST_INFO.  
inline DST_INFO* DST_get_info(DST_TYPE dst, DST_IDX info_idx) {
  Is_True(dst != 0, ("DST pointer is null"));
  return static_cast<DST_INFO*>(DST_IDX_to_ptr(dst, info_idx));
}

// Given a DST_INFO*, extracts its compile unit node.  Precondition: 
// the attributes node must actually be a DST_COMPILE_UNIT.
inline DST_COMPILE_UNIT*
DST_get_compile_unit_attr(DST_TYPE dst, DST_INFO* info) {
  DST_IDX attr_idx = DST_INFO_attributes(info);
  Is_True(DST_INFO_tag(info) == DW_TAG_compile_unit, ("Not a compile unit"));
  Is_True( !DST_ARE_EQUAL(attr_idx, DST_INVALID_IDX), ("Compile unit attr idx is null"));
  return static_cast<DST_COMPILE_UNIT*>(DST_IDX_to_ptr(dst, attr_idx));
}

// Given a DST_INFO*, extracts its subprogram node.  Precondition: 
// the attributes node must actually be a DST_SUBPROGRAM.
inline DST_SUBPROGRAM*
DST_get_subprogram_attr(DST_TYPE dst, DST_INFO* info) {
  DST_IDX attr_idx = DST_INFO_attributes(info);
  Is_True(DST_INFO_tag(info) == DW_TAG_subprogram, ("Not a subprogram"));
  Is_True( !DST_ARE_EQUAL(attr_idx, DST_INVALID_IDX), ("Subprogram unit attr idx is null"));
  return static_cast<DST_SUBPROGRAM*>(DST_IDX_to_ptr(dst, attr_idx));
}

// Converts a DST_IDX to an include dir node.  
inline DST_INCLUDE_DIR* DST_get_include_dir(DST_TYPE dst, DST_IDX idx) {
  Is_True(dst != 0, ("DST_get_include_dir: null dst pointer"));
  Is_True( !DST_ARE_EQUAL(idx, DST_INVALID_IDX), ("DST_get_include_dir: bad dst index"));
  return static_cast<DST_INCLUDE_DIR*>(DST_IDX_to_ptr(dst, idx));
}

// Converts a DST_IDX to a file name node.
inline DST_FILE_NAME* DST_get_file_name(DST_TYPE dst, DST_IDX idx) {
  Is_True(dst != 0, ("DST_get_file_name: null dst pointer"));
  Is_True( !DST_ARE_EQUAL(idx, DST_INVALID_IDX), ("DST_get_file_name: bad dst index"));
  return static_cast<DST_FILE_NAME*>(DST_IDX_to_ptr(dst, idx));
}

inline bool DST_SUBPROGRAM_has_spec(DST_INFO* info) {
  Is_True(info != 0, ("DST_INFO pointer must not be null"));
  Is_True(DST_INFO_tag(info) == DW_TAG_subprogram, ("Not a subprogram node"));
  return DST_IS_memdef(DST_INFO_flag(info)) ||
         !DST_IS_declaration(DST_INFO_flag(info));
}

inline bool DST_SUBPROGRAM_has_origin(DST_INFO* info) {
  Is_True(info != 0, ("DST_INFO pointer must not be null"));
  Is_True(DST_INFO_tag(info) == DW_TAG_subprogram, ("Not a subprogram node"));
  return DST_IS_declaration(DST_INFO_flag(info)) ||
         DST_IS_memdef(DST_INFO_flag(info));
}

inline DST_IDX& DST_SUBPROGRAM_spec(DST_INFO* info, DST_SUBPROGRAM* attr) {
  Is_True(info != 0, ("DST_INFO pointer must not be null"));
  Is_True(DST_INFO_tag(info) == DW_TAG_subprogram, ("Not a subprogram node"));
  Is_True(DST_SUBPROGRAM_has_spec(info),
          ("Not a memdef or subprogram definition node"));
  if (DST_IS_memdef(DST_INFO_flag(info)))
    return DST_SUBPROGRAM_memdef_spec(attr);
  else
    return DST_SUBPROGRAM_def_specification(attr);
}

inline DST_IDX& DST_SUBPROGRAM_origin(DST_INFO* info, DST_SUBPROGRAM* attr) {
  Is_True(info != 0, ("DST_INFO pointer must not be null"));
  Is_True(DST_INFO_tag(info) == DW_TAG_subprogram, ("Not a subprogram node"));
  Is_True(DST_SUBPROGRAM_has_origin(info),
          ("Not a subprogram declaration or subprogram definition node"));
  if (DST_IS_declaration(DST_INFO_flag(info)))
    return DST_SUBPROGRAM_decl_origin(attr);
  else
    return DST_SUBPROGRAM_def_clone_origin(attr);
}


//-----------------------------------------------------------------
// Low-level functions for DST initialization, block creation, and 
// storage allocation.  (There are no functions for deallocation.)
// The fundamental differences between these functions and the ones in
// dwarf_DST_mem.h are that
//   -- The functions in dwarf_DST_mem.h all operate on Current_DST; 
//      these take an explicit parameter of type DST_TYPE.
//   -- The functions in dwarf_DST_mem.h use a global mempool; these
//      take an explicit parameter of type MEM_POOL*.


// Create an empty DST, reserving space for at least num_block_headers
// blocks.  
DST_TYPE DST_create(MEM_POOL*, UINT32 num_block_headers = 1024);

// Set the DST's internal current block header.
block_header* DST_set_current_block(DST_TYPE, DST_BLOCK_IDX);

// Create a new block of a specified kind, and a minimum blocksize.
DST_BLOCK_IDX
DST_new_block(DST_TYPE, MEM_POOL*, DST_BLOCK_KIND, INT32 blocksize);

// Reserve at least size bytes, aligned on an align byte boundary,
// in a block of a specified kind.  Automatically creates a new block
// if no block of that kind exists, or if no block of that kind has
// sufficient space.
DST_IDX DST_allocate(DST_TYPE dst, MEM_POOL* p,
                     INT32 size, INT32 align, DST_BLOCK_KIND kind);

// Copy a null-terminated string into the DST, allocating it in the
// current block.
DST_IDX DST_mk_string(DST_TYPE dst, MEM_POOL* p, const char* s);


//-----------------------------------------------------------------
// Functions to create specific objects in a DST.  We do not have 
// a function for every type of object that can appear in a DST, only
// the objects that we actually need for IPA.  

// Create an include dir node.  Precondition: prev is DST_INVALID_IDX
// if and only if this is the first include dir node to be created.
// If prev is not DST_INVALID_IDX, then this function sets prev's next
// pointer to the newly created node.
DST_IDX DST_mk_include_dir(DST_TYPE dst, MEM_POOL* p, const char* path,
                           DST_IDX prev);

// Create a filename node.  Precondition: prev is DST_INVALID_IDX
// if and only if this is the first filename node to be created.
// If prev is not DST_INVALID_IDX, then this function sets prev's next
// pointer to the newly created node.
DST_IDX DST_mk_filename(DST_TYPE dst, MEM_POOL* p, const char* name,
                        UINT16 dir, UINT64 size, UINT64 modt,
                        DST_IDX prev);

// Copy an include dir node from one DST to another.
DST_IDX DST_copy_include_dir(DST_TYPE src, DST_TYPE dest,
                             MEM_POOL* p,
                             DST_IDX prev, 
                             DST_IDX old_index);

// Copy a filename node from one DST to another.  The parameter 
// 'offset' is added to the ordinal number of references to directory
// nodes.  (Necessary because ordinal numbers in the merged DST aren't
// the same as ordinal numbers in an input DST.)
DST_IDX DST_copy_filename(DST_TYPE src, DST_TYPE dest,
                          MEM_POOL* p,
                          DST_IDX prev, 
                          DST_IDX old_index,
                          UINT16 offset);


// Initialize an info node.  We have to separate construction from
// initialization because the order is important: to construct an object
// in the DST, the usual pattern is (1) Allocate storage for the info node,
// using DST_allocate.  (2) Allocate storage for the attribute node, and 
// initialize its fields.  (3) Initialize the info node, using (among other
// things) the attribute node's index.
// Unless prev_info is DST_INVALID_IDX, this function will set 
// prev_info's next pointer to point to info_idx.
void DST_init_info(DST_TYPE dst, DST_IDX info_idx,
                   DST_DW_tag tag, DST_flag flag,
                   DST_ATTR_IDX attr,
                   DST_INFO_IDX prev_info);

// Create a compile unit.  The compile units for a singly-linked list,
// and 'prev' is the previous compile unit in that list.  
// Precondition: 'prev' is DST_INVALID_IDX if and only if this is the
// first compile unit.
DST_IDX DST_mk_compile_unit(DST_TYPE dst,
                            MEM_POOL* p, 
                            DST_IDX prev,
                            const char* name,
                            const char* comp_dir,
                            const char* comp_info,
                            DST_language language,
                            DST_identifier_case id_case);

// Copy a compile unit from one DST to another.  Return value is 
// index in the destination dst.  
// Precondition: 'prev' is DST_INVALID_IDX if and only if this is the
// first compile unit.
DST_IDX DST_copy_compile_unit(DST_TYPE src, DST_TYPE dest,
                              MEM_POOL* p,
                              DST_IDX prev, 
                              DST_IDX old_index);
                              

// Create one of the three kinds of subprogram unit nodes.  
// prev points to the previous subprogram unit  (DST_INVALID_IDX if 
// this is the first), and cu_idx points to the parent compile
// unit node.

DST_IDX DST_mk_subpr_decl(DST_TYPE dst,
                          MEM_POOL* p,
                          DST_IDX cu_idx,
                          DST_IDX prev,
                          USRCPOS decl,
                          const char* name,
                          const char* mangled_name,
                          DST_inline inlin,
                          DST_virtuality virtuality,
                          DST_vtable_elem_location loc,
                          DST_flag flag);

DST_IDX DST_mk_subpr_def(DST_TYPE dst,
                         MEM_POOL* p,
                         DST_IDX cu_idx,
                         DST_IDX prev,
                         USRCPOS decl,
                         const char* name,
                         const char* mangled_name,
                         const char* pubname,
                         DST_ASSOC_INFO st,
                         DST_inline inlin,
                         DST_virtuality virtuality,
                         DST_vtable_elem_location loc,
                         DST_flag flag);

DST_IDX DST_mk_memdef(DST_TYPE dst,
                      MEM_POOL* p,
                      DST_IDX cu_idx,
                      DST_IDX prev,
                      USRCPOS decl,
                      DST_ASSOC_INFO st,
                      DST_flag flag);

// Copy a subprogram node from one DST to another.  Return value is 
// the index in the destination dst.  
// Precondition: 'prev' is DST_INVALID_IDX if and only if this is the
// first child of the compilation unit.
DST_IDX DST_copy_subprogram(DST_TYPE src, DST_TYPE dest,
                            MEM_POOL* p,
                            DST_IDX cu, DST_IDX prev, 
                            DST_IDX old_idx);


//-----------------------------------------------------------------
// Iterate through a DST's file-scope nodes.  The file-scope nodes are
// all children of a compile unit node, and they are arranged as a
// singly linked list; meanwhile, the compile unit nodes themselves are
// also a singly linked list. This is accordingly a forward iterator.  Its
// operator* returns a DST_IDX.

struct DST_file_scope_iter {
  // Boilerplate: required typedefs.
  typedef std::forward_iterator_tag iterator_category;
  typedef DST_IDX                   value_type;
  typedef UINT32                    difference_type;
  typedef const DST_IDX*            pointer;
  typedef const DST_IDX&            reference;

  // Constructors and creation helper functions.
  DST_file_scope_iter() : dst(0), idx(DST_INVALID_IDX) {}
  DST_file_scope_iter(DST_TYPE dst_vptr, DST_IDX cu_index, DST_IDX index) {
    dst = static_cast<DST_Type*>(dst_vptr);
    cu_idx = cu_index;
    idx = index;
    Is_True( DST_ARE_EQUAL(cu_idx, DST_INVALID_IDX) ||
            DST_INFO_tag(DST_get_info(dst, cu_idx)) == DW_TAG_compile_unit,
            ("Not a compile unit"));
  }

  static DST_file_scope_iter begin(DST_TYPE dst) {
    DST_IDX cu_info_idx = DST_get_compile_unit(dst);
    DST_INFO* cu_info = DST_get_info(dst, cu_info_idx);
    DST_COMPILE_UNIT* cu = DST_get_compile_unit_attr(dst, cu_info);
    DST_IDX idx = DST_COMPILE_UNIT_first_child(cu); 
    return DST_file_scope_iter(dst, cu_info_idx, idx);
  }

  static DST_file_scope_iter end(DST_TYPE dst) {
    Is_True(dst != 0, ("Can't construct iterator with null DST"));
    return DST_file_scope_iter(dst, DST_INVALID_IDX, DST_INVALID_IDX);
  }

  // Increment and defererence.
  const DST_IDX& operator*() const {
    Is_True(dst != 0, ("Iterator is singular"));
    Is_True( DST_ARE_EQUAL(idx, DST_INVALID_IDX), ("Iterator is past-the-end"));
    return idx;
  }

  const DST_IDX* operator->() const {
    return &(operator*());
  }

  DST_INFO* to_info_ptr() const {
    DST_IDX info_idx = *(*this);
    return static_cast<DST_INFO*>(DST_IDX_to_ptr(dst, info_idx));
  }

  DST_file_scope_iter& operator++() {
    idx = DST_INFO_sibling(this->to_info_ptr());
    if( DST_ARE_EQUAL(idx,DST_INVALID_IDX) ){
      DST_INFO* old_cu_info = DST_get_info(dst, cu_idx);
      cu_idx = DST_INFO_sibling(old_cu_info);
      if( !DST_ARE_EQUAL(cu_idx, DST_INVALID_IDX)) {
        DST_INFO* cu_info = DST_get_info(dst, cu_idx);
        DST_COMPILE_UNIT* cu = DST_get_compile_unit_attr(dst, cu_info);
        idx = DST_COMPILE_UNIT_first_child(cu);
      }
    }
    return *this;
  }

  DST_file_scope_iter operator++(int) {
    DST_file_scope_iter tmp(*this);
    ++*this;
    return tmp;
  }

  bool operator== (const DST_file_scope_iter& x) const {
    return dst == x.dst && DST_ARE_EQUAL(cu_idx, x.cu_idx) && DST_ARE_EQUAL(idx, x.idx);
  }
  bool operator!= (const DST_file_scope_iter& x) const {
    return dst != x.dst || !DST_ARE_EQUAL(cu_idx, x.cu_idx) || !DST_ARE_EQUAL(idx, x.idx);
  }

private:
  DST_Type* dst;
  DST_IDX cu_idx;
  DST_IDX idx;
};

#endif /* IPC_DST_UTILS_included */
