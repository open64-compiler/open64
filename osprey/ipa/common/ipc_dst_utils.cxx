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

#include "cxx_memory.h"
#include "errors.h"
#include <algorithm>

#include "ipc_dst_utils.h"

static const INT32 min_block_size = 256;
static const INT32 DST_64_align   = 8; 
static const INT32 DST_32_align   = 4; 
static const INT32 DST_char_align = 1; 
static const INT32 DST_default_align = DST_64_align; 

//------------------------------------------------------------
// Utilities for DST inspection.

DST_IDX DST_get_compile_unit(DST_TYPE dst_vptr)
{
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  Is_True(dst != 0, ("DST pointer may not be void"));
  Is_True(DST_has_compile_unit(dst), ("No compile unit"));
  DST_IDX cu_idx = make_DST_IDX(0, dst->block_list[DST_file_scope_block]);
  Is_True(DST_INFO_tag(DST_get_info(dst, cu_idx)) == DW_TAG_compile_unit,
          ("Not a DST compile unit"));
  return cu_idx;
}

DST_IDX DST_get_include_dirs(DST_TYPE dst_vptr)
{
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  DST_BLOCK_IDX idx = dst->block_list[DST_include_dirs_block];
  if (idx != DST_INVALID_BLOCK_IDX &&
      !DST_block_is_empty(DST_get_block(dst_vptr, idx)))
    return make_DST_IDX(0, idx);
  else
    return DST_INVALID_IDX;
}

DST_IDX DST_get_file_names(DST_TYPE dst_vptr)
{
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  DST_BLOCK_IDX idx = dst->block_list[DST_file_names_block];
  if (idx != DST_INVALID_BLOCK_IDX &&
      !DST_block_is_empty(DST_get_block(dst_vptr, idx)))
    return make_DST_IDX(0, idx);
  else
    return DST_INVALID_IDX;
}

void
copy_DST_Type(DST_TYPE src, DST_TYPE dest, MEM_POOL* p)
{
  DST_Type* s = static_cast<DST_Type*>(src);
  DST_Type* d = static_cast<DST_Type*>(dest);
  block_header* s_blkhdr;
  block_header* d_blkhdr;
  int i;

  for( i = 0; i <= s->last_block_header; i++ ) {
    s_blkhdr = &s->dst_blocks[i];
    DST_new_block(dest, p, s_blkhdr->kind, s_blkhdr->allocsize);
    d_blkhdr = &d->dst_blocks[d->current_block_header];
    memcpy(d_blkhdr->offset, s_blkhdr->offset, s_blkhdr->size);
    d_blkhdr->size = s_blkhdr->size;
  }
}

//------------------------------------------------------------
// Low-level functions for initialization and allocation.

DST_TYPE DST_create(MEM_POOL* p, UINT32 num_block_headers) {
  Is_True(p != 0, ("IPC_DST::IPC_DST: invalid mempool pointer"));
  Is_True(num_block_headers > 0,
          ("IPC_DST::IPC_DST: invalid initial number of block headers"));

  // Allocate the DST header and its array of block headers.
  DST_Type*dst  = TYPE_MEM_POOL_ALLOC(DST_Type, p);
  dst->dst_blocks = TYPE_MEM_POOL_ALLOC_N(block_header, p, num_block_headers);
  dst->current_dst = 0;

  // Note that last_block_header is the index of the last used block
  // header: incrementing last_block_header gives you the index of the
  // first unused block header.  That is, it is a symmetric bound rather
  // than an STL-style asymmetric bound.
  dst->last_block_header = -1;
  dst->max_block_header = num_block_headers;
  dst->current_block_header = DST_INVALID_BLOCK_IDX;

  // Initialize the list of first blocks of each type
  for (UINT32 i = 0; i < DST_noblock; ++i) 
    dst->block_list[i] = DST_INVALID_BLOCK_IDX;

  return dst;
}

block_header* DST_set_current_block(DST_TYPE dst_vptr, DST_BLOCK_IDX idx)
{
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  Is_True(dst != 0, ("DST pointer is void"));
  Is_True(idx >= 0 && idx <= dst->last_block_header,
          ("Block index %d is not in range 0 <= idx <= %d",
           idx, dst->last_block_header));

  dst->current_block_header = idx;
  dst->current_dst = dst->dst_blocks + idx;
  return dst->current_dst;
}

// Create a new block.  The new block becomes the current block.
DST_BLOCK_IDX
DST_new_block(DST_TYPE dst_vptr, MEM_POOL* p,
              DST_BLOCK_KIND kind, INT32 blocksize) {
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  Is_True(dst != 0, ("DST pointer is void"));
  Is_True(kind >= DST_include_dirs_block && kind < DST_noblock,
          ("Invalid block kind"));
  Is_True(blocksize >= 0, ("Invalid block size"));

  // We have a list of preallocated block headers.  Do we need any more?
  ++dst->last_block_header;     // Index of first unused header.
  Is_True (dst->last_block_header >= 0 &&
           dst->last_block_header <= dst->max_block_header,
           ("last_block_header %ld should be in range 0 <= idx <= %ld",
            dst->last_block_header, dst->max_block_header));

  if (dst->last_block_header == dst->max_block_header) {
    dst->dst_blocks = TYPE_MEM_POOL_REALLOC_N(block_header, p,
                                              dst->dst_blocks,
                                              dst->max_block_header,
                                              2 * dst->max_block_header);
    dst->max_block_header *= 2;
  }

  block_header* block = DST_set_current_block(dst, dst->last_block_header);

  // Allocate the block itself, and fill in the fields of the header.
  const INT32 capacity = std::max(blocksize, min_block_size);
  block->kind = kind;
  block->size = 0;
  block->allocsize = capacity;
  block->offset = TYPE_MEM_POOL_ALLOC_N(char, p, capacity);

  // If this is the first block of its kind, stick it in the blocklist.
  if (dst->block_list[kind] == DST_INVALID_BLOCK_IDX)
    dst->block_list[kind] = dst->current_block_header;

  Is_True(block == &dst->dst_blocks[dst->current_block_header],
          ("Block index mismatch"));
  return dst->current_block_header;  
}

// Allocate storage in a block of type kind.  The block in which we
// allocate storage becomes the current block.
DST_IDX DST_allocate(DST_TYPE dst_vptr, MEM_POOL* p,
                     INT32 size, INT32 align, DST_BLOCK_KIND kind)
{
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  Is_True(dst != 0, ("Null DST pointer"));
  Is_True(kind >= DST_include_dirs_block && kind < DST_noblock,
          ("Invalid block kind"));
  Is_True(size > 0, ("Invalid allocation size"));
  Is_True(align > 0, ("Invalid alignment"));
  Is_True((dst->current_dst == 0 ||
           dst->current_dst == dst->dst_blocks + dst->current_block_header),
          ("DST current block does not match its index"));
  Is_True((dst->current_block_header == DST_INVALID_BLOCK_IDX) ==
          (dst->current_dst == 0),
          ("status of current_dst and current_block _header inconsistent"));

  DST_BYTE_IDX  offset;         // Location of new object within the block.

  // If there is no current block, or if it's the wrong kind, start a new one.
  if (dst->current_block_header == DST_INVALID_BLOCK_IDX ||
      dst->current_dst->kind != kind) {
    DST_new_block(dst, p, kind, size);
    offset = 0;                 // New object is at beginning of block.
  }

  else {
    // The current block is the right kind.  Does it have enough space?
    INT32 align_mod = dst->current_dst->size % align;
    offset = dst->current_dst->size + (align_mod ? (align - align_mod) : 0);

    // If we don't have enough space, grab a new block.
    if (offset + size > dst->current_dst->allocsize) {
      DST_new_block(dst, p, kind, size);
      offset = 0;                 
    }
  }

  // dst->current_dst->size is the index of the first free byte.
  dst->current_dst->size = offset + size;
  Is_True(dst->current_dst->size <= dst->current_dst->allocsize,
          ("Allocation overruns the current block"));

  return make_DST_IDX(offset, dst->current_block_header);
}

DST_IDX DST_mk_string(DST_TYPE dst_vptr, MEM_POOL* p, const char* s)
{
  DST_Type* dst = static_cast<DST_Type*>(dst_vptr);
  Is_True(s != 0, ("DST_mk_string: string pointer may not be null"));
  Is_True(dst != 0, ("DST_mk_string: DST pointer may not be null"));
  Is_True(dst->current_dst != 0, ("DST_mk_string: DST has no current block"));

  size_t n = strlen(s);
  DST_IDX result = DST_allocate(dst, p, n + 1, DST_char_align,
                                dst->current_dst->kind);
  memcpy(DST_IDX_to_ptr(dst, result), s, n + 1);

  return result;
}

//-----------------------------------------------------------------
// Functions to create specific objects in a DST.  

DST_IDX DST_mk_include_dir(DST_TYPE dst, MEM_POOL* p, const char* path,
                           DST_IDX prev_idx)
{
  Is_True((DST_IS_NULL(prev_idx)) ||
          (!DST_IS_NULL(DST_get_include_dirs(dst))),
          ("For first DST include dir, idx of prev node must be null"));
  Is_True((!DST_IS_NULL(prev_idx)) ||
          (DST_IS_NULL(DST_get_include_dirs(dst))),
          ("This is not the first DST include dir, but idx of prev is null"));

  DST_IDX idx = DST_allocate(dst, p,
                             sizeof(DST_INCLUDE_DIR), DST_default_align,
                             DST_include_dirs_block);
  DST_INCLUDE_DIR* dir = DST_get_include_dir(dst, idx);
  DST_INCLUDE_DIR_path(dir) = DST_mk_string(dst, p, path);
  DST_INCLUDE_DIR_next(dir) = DST_INVALID_IDX;

  if(!DST_IS_NULL(prev_idx)) {
    DST_INCLUDE_DIR* prev = DST_get_include_dir(dst, prev_idx);
    DST_INCLUDE_DIR_next(prev) = idx;
  }

  return idx;
}

DST_IDX DST_mk_filename(DST_TYPE dst, MEM_POOL* p, const char* name,
                        UINT16 dir, UINT64 size, UINT64 modt,
                        DST_IDX prev_idx)
{
  Is_True((DST_IS_NULL(prev_idx)) ||
          (!DST_IS_NULL(DST_get_file_names(dst))),
          ("For first DST include dir, idx of prev node must be null"));
  Is_True((!DST_IS_NULL(prev_idx)) ||
          (DST_IS_NULL(DST_get_file_names(dst))),
          ("This is not the first DST include dir, but idx of prev is null"));

  DST_IDX idx = DST_allocate(dst, p,
                             sizeof(DST_FILE_NAME), DST_default_align,
                             DST_file_names_block);
  DST_FILE_NAME* file = DST_get_file_name(dst, idx);
  DST_FILE_NAME_name(file) = DST_mk_string(dst, p, name);
  DST_FILE_NAME_dir(file)  = dir;
  DST_FILE_NAME_size(file) = size;
  DST_FILE_NAME_modt(file) = modt;
  DST_FILE_NAME_next(file) = DST_INVALID_IDX;

  if (!DST_IS_NULL(prev_idx)) {
    DST_FILE_NAME* prev = DST_get_file_name(dst, prev_idx);
    DST_FILE_NAME_next(prev) = idx;
  }

  return idx;
}

DST_IDX DST_copy_include_dir(DST_TYPE src, DST_TYPE dest,
                             MEM_POOL* p,
                             DST_IDX prev, 
                             DST_IDX old_index)
{
  DST_INCLUDE_DIR* dir = DST_get_include_dir(src, old_index);
  return DST_mk_include_dir(dest, p,
                            DST_IDX_to_string(src, DST_INCLUDE_DIR_path(dir)),
                            prev);
}

DST_IDX DST_copy_filename(DST_TYPE src, DST_TYPE dest,
                          MEM_POOL* p,
                          DST_IDX prev, 
                          DST_IDX old_index,
                          UINT16 incl_dir_nbr)
{
  DST_FILE_NAME* f = DST_get_file_name(src, old_index);
  return DST_mk_filename(dest, p,
                         DST_IDX_to_string(src, DST_FILE_NAME_name(f)),
                         incl_dir_nbr,
                         DST_FILE_NAME_size(f),
                         DST_FILE_NAME_modt(f),
                         prev);
}


void DST_init_info(DST_TYPE dst, DST_IDX info_idx,
                   DST_DW_tag tag, DST_flag flag,
                   DST_ATTR_IDX attr,
                   DST_INFO_IDX prev_info_idx)
{
  DST_INFO* const info = DST_get_info(dst, info_idx);
  DST_INFO_tag(info)        = tag;
  DST_INFO_flag(info)       = flag;
  DST_INFO_sibling(info)    = DST_INVALID_IDX;
  DST_INFO_attributes(info) = attr;
  DST_INFO_dieptr(info)     = NULL;
  if (!DST_IS_NULL(prev_info_idx)) {
    DST_INFO* const prev_info = DST_get_info(dst, prev_info_idx);
    DST_INFO_sibling(prev_info) = info_idx;
  }
}

DST_IDX DST_mk_compile_unit(DST_TYPE dst,
                            MEM_POOL* p, 
                            DST_IDX prev,
                            const char* name,
                            const char* comp_dir,
                            const char* comp_info,
                            DST_language language,
                            DST_identifier_case id_case)
{
  Is_True(name != 0 && comp_info != 0, ("Strings may not be null pointers"));
  // Note that comp_dir may be a null pointer.

  Is_True((DST_IS_NULL(prev) && !DST_has_compile_unit(dst)) ||
          (!DST_IS_NULL(prev) && DST_has_compile_unit(dst)),
          ("Must create first compile unit once and only once"));

  // Always put a new compile unit in a new block.
  if (DST_IS_NULL(prev)) 
    DST_new_block(dst, p, DST_file_scope_block, sizeof(DST_INFO));

  DST_IDX info_idx = DST_allocate(dst, p, sizeof(DST_INFO),
                                  DST_default_align, DST_file_scope_block);
  DST_IDX attr_idx = DST_allocate(dst, p, sizeof(DST_COMPILE_UNIT),
                                  DST_default_align, DST_file_scope_block);
  DST_COMPILE_UNIT* cu =
    static_cast<DST_COMPILE_UNIT*>(DST_IDX_to_ptr(dst, attr_idx));

  DST_COMPILE_UNIT_name(cu)     = DST_mk_string(dst, p, name);
  DST_COMPILE_UNIT_comp_dir(cu) =
    comp_dir ? DST_mk_string(dst, p, comp_dir) : DST_INVALID_IDX;
  DST_COMPILE_UNIT_producer(cu) = DST_mk_string(dst, p, comp_info);
  DST_COMPILE_UNIT_language(cu) = language;
  DST_COMPILE_UNIT_identifier_case(cu) = id_case;
  DST_COMPILE_UNIT_first_child(cu) = DST_INVALID_IDX;
  DST_COMPILE_UNIT_last_child(cu) = DST_INVALID_IDX;

  DST_init_info(dst, info_idx,
                DW_TAG_compile_unit, DST_no_flag,
                attr_idx, prev);
  return info_idx;
}

DST_IDX DST_copy_compile_unit(DST_TYPE src, DST_TYPE dest,
                              MEM_POOL* p,
                              DST_IDX prev, 
                              DST_IDX old_idx)
{
  DST_INFO* cu_info = DST_get_info(src, old_idx);
  DST_COMPILE_UNIT* cu = DST_get_compile_unit_attr(src, cu_info);

  DST_IDX name_idx = DST_COMPILE_UNIT_name(cu);
  DST_IDX comp_dir_idx = DST_COMPILE_UNIT_comp_dir(cu);
  DST_IDX producer_idx = DST_COMPILE_UNIT_producer(cu);

  Is_True(!DST_IS_NULL(name_idx),
          ("Compile unit name is invalid"));
  char* comp_dir = (!DST_IS_NULL(comp_dir_idx)) ?
                   DST_IDX_to_string(src, comp_dir_idx) :
                   0;

  Is_True(!DST_IS_NULL(producer_idx),
          ("Compile unit compile line is invalid"));

  return DST_mk_compile_unit(dest, p, prev,
                             DST_IDX_to_string(src, name_idx),
                             comp_dir,
                             DST_IDX_to_string(src, producer_idx),
                             DST_COMPILE_UNIT_language(cu),
                             DST_COMPILE_UNIT_identifier_case(cu));
}

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
                          DST_flag flag)
{
  DST_INFO* cu_info = DST_get_info(dst, cu_idx);
  DST_COMPILE_UNIT* cu = DST_get_compile_unit_attr(dst, cu_info);

  Is_True(name != 0, ("Subprogram declaration name may not be null pointer"));
  // Note that the mangled name may be a null pointer.

  Is_True((DST_IS_NULL(prev) ==
          DST_IS_NULL(DST_COMPILE_UNIT_first_child(cu))),
          ("Every subprogram except the first must have a previous node"));
  Is_True(DST_IS_declaration(flag), ("Not a declaration"));
  
  DST_IDX info_idx = DST_allocate(dst, p,
                                  sizeof(DST_INFO), DST_default_align,
                                  DST_file_scope_block);
  DST_IDX attr_idx = DST_allocate(dst, p,
                                  sizeof(DST_SUBPROGRAM), DST_default_align,
                                  DST_file_scope_block);
  DST_SUBPROGRAM* attr =
    static_cast<DST_SUBPROGRAM*>(DST_IDX_to_ptr(dst, attr_idx));
  
  DST_SUBPROGRAM_decl_decl(attr) = decl;
  DST_SUBPROGRAM_decl_name(attr) = DST_mk_string(dst, p, name);
  DST_SUBPROGRAM_decl_linkage_name(attr) =
    mangled_name != 0 ? DST_mk_string(dst, p, mangled_name) : DST_INVALID_IDX;
  DST_SUBPROGRAM_decl_type(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_decl_origin(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_decl_inline(attr) = inlin;
  DST_SUBPROGRAM_decl_virtuality(attr) = virtuality;
  DST_SUBPROGRAM_decl_vtable_elem_location(attr) = loc;
  DST_SUBPROGRAM_decl_first_child(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_decl_last_child(attr) = DST_INVALID_IDX;

  DST_init_info(dst, info_idx, DW_TAG_subprogram, flag, attr_idx, prev);
  DST_COMPILE_UNIT_last_child(cu) = info_idx;
  if (DST_IS_NULL(prev))
    DST_COMPILE_UNIT_first_child(cu) = info_idx;

  return info_idx;
}

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
                         DST_flag flag)
{
  DST_INFO* cu_info = DST_get_info(dst, cu_idx);
  DST_COMPILE_UNIT* cu = DST_get_compile_unit_attr(dst, cu_info);

  Is_True(name != 0, ("Subprogram definition name may not be null pointer"));
  // Note that the other two names may be null pointers.

  Is_True((DST_IS_NULL(prev)) ==
          (DST_IS_NULL(DST_COMPILE_UNIT_first_child(cu))),
          ("Every subprogram except the first must have a previous node"));
  Is_True(!DST_IS_declaration(flag) && !DST_IS_memdef(flag),
          ("Not a definition"));

  DST_IDX info_idx = DST_allocate(dst, p,
                                  sizeof(DST_INFO), DST_default_align,
                                  DST_file_scope_block);
  DST_IDX attr_idx = DST_allocate(dst, p,
                                  sizeof(DST_SUBPROGRAM), DST_default_align,
                                  DST_file_scope_block);
  DST_SUBPROGRAM* attr =
    static_cast<DST_SUBPROGRAM*>(DST_IDX_to_ptr(dst, attr_idx));
  
  DST_SUBPROGRAM_def_decl(attr) = decl;
  DST_SUBPROGRAM_def_name(attr) = DST_mk_string(dst, p, name);
  DST_SUBPROGRAM_def_linkage_name(attr) =
    mangled_name != 0 ? DST_mk_string(dst, p, mangled_name) : DST_INVALID_IDX;
  DST_SUBPROGRAM_def_pubname(attr) = 
    pubname != 0 ? DST_mk_string(dst, p, pubname) : DST_INVALID_IDX;
  DST_SUBPROGRAM_def_type(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_def_st(attr) = st;
  DST_SUBPROGRAM_decl_inline(attr) = inlin;
  DST_SUBPROGRAM_def_virtuality(attr) = virtuality;
  DST_SUBPROGRAM_def_vtable_elem_location(attr) = loc,
  DST_SUBPROGRAM_def_specification(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_def_first_child(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_def_last_child(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_def_clone_origin(attr) = DST_INVALID_IDX;

  DST_init_info(dst, info_idx, DW_TAG_subprogram, flag, attr_idx, prev);
  DST_COMPILE_UNIT_last_child(cu) = info_idx;
  if (DST_IS_NULL(prev))
    DST_COMPILE_UNIT_first_child(cu) = info_idx;

  return info_idx;
}

DST_IDX DST_mk_memdef(DST_TYPE dst,
                      MEM_POOL* p,
                      DST_IDX cu_idx,
                      DST_IDX prev,
                      USRCPOS decl,
                      DST_ASSOC_INFO st,
                      DST_flag flag)
{
  DST_INFO* cu_info = DST_get_info(dst, cu_idx);
  DST_COMPILE_UNIT* cu = DST_get_compile_unit_attr(dst, cu_info);

  Is_True( DST_IS_NULL(prev) ==
           DST_IS_NULL(DST_COMPILE_UNIT_first_child(cu)),
          ("Every subprogram except the first must have a previous node"));
  Is_True(DST_IS_memdef(flag), ("Not a memdef"));

  DST_IDX info_idx = DST_allocate(dst, p,
                                  sizeof(DST_INFO), DST_default_align,
                                  DST_file_scope_block);
  DST_IDX attr_idx = DST_allocate(dst, p,
                                  sizeof(DST_SUBPROGRAM), DST_default_align,
                                  DST_file_scope_block);
  DST_SUBPROGRAM* attr =
    static_cast<DST_SUBPROGRAM*>(DST_IDX_to_ptr(dst, attr_idx));
  
  DST_SUBPROGRAM_memdef_decl(attr) = decl;
  DST_SUBPROGRAM_memdef_spec(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_memdef_st(attr) = st;
  DST_SUBPROGRAM_def_first_child(attr) = DST_INVALID_IDX;
  DST_SUBPROGRAM_def_last_child(attr) = DST_INVALID_IDX;

  DST_init_info(dst, info_idx, DW_TAG_subprogram, flag, attr_idx, prev);
  DST_COMPILE_UNIT_last_child(cu) = info_idx;
  if (DST_IS_NULL(prev))
    DST_COMPILE_UNIT_first_child(cu) = info_idx;

  return info_idx;
}

DST_IDX DST_copy_subprogram(DST_TYPE src, DST_TYPE dest,
                            MEM_POOL* p,
                            DST_IDX cu, DST_IDX prev, 
                            DST_IDX old_idx)
{
  DST_INFO* info = DST_get_info(src, old_idx);
  DST_SUBPROGRAM* sub = DST_get_subprogram_attr(src, info);
  DST_flag flag = DST_INFO_flag(info);

  if (DST_IS_memdef(flag)) {
    return DST_mk_memdef(dest, p, cu, prev,
                         DST_SUBPROGRAM_memdef_decl(sub),
                         DST_SUBPROGRAM_memdef_st(sub),
                         flag);
  }
  else if (DST_IS_declaration(flag)) {
    DST_IDX name_idx         = DST_SUBPROGRAM_decl_name(sub);
    DST_IDX linkage_name_idx = DST_SUBPROGRAM_decl_linkage_name(sub);
    Is_True(!DST_IS_NULL(name_idx),
            ("Subprogram declaration has no name"));
    const char* name = DST_IDX_to_string(src, name_idx);
    const char* linkage_name = !DST_IS_NULL(linkage_name_idx) ?
                               DST_IDX_to_string(src, linkage_name_idx) :
                               0;

    return DST_mk_subpr_decl(dest, p, cu, prev,
                             DST_SUBPROGRAM_decl_decl(sub),
                             name,
                             linkage_name,
                             DST_SUBPROGRAM_decl_inline(sub),
                             DST_SUBPROGRAM_decl_virtuality(sub),
                             DST_SUBPROGRAM_decl_vtable_elem_location(sub),
                             flag);
  }
  else {
    DST_IDX name_idx         = DST_SUBPROGRAM_def_name(sub);
    DST_IDX linkage_name_idx = DST_SUBPROGRAM_def_linkage_name(sub);
    DST_IDX pubname_idx      = DST_SUBPROGRAM_def_pubname(sub);
    if (DST_IS_NULL(name_idx))
	DevWarn ("Subprogram definition has no name");
    const char* name = (!DST_IS_NULL(name_idx)) ?
	DST_IDX_to_string(src, name_idx) : "";
    const char* linkage_name = (!DST_IS_NULL(linkage_name_idx)) ?
                               DST_IDX_to_string(src, linkage_name_idx) :
                               0;
    const char* pubname = (!DST_IS_NULL(pubname_idx)) ?
                          DST_IDX_to_string(src, pubname_idx) :
                          0;


    return DST_mk_subpr_def(dest, p, cu, prev,
                            DST_SUBPROGRAM_def_decl(sub),
                            name,
                            linkage_name,
                            pubname,
                            DST_SUBPROGRAM_def_st(sub),
                            DST_SUBPROGRAM_def_inline(sub),
                            DST_SUBPROGRAM_def_virtuality(sub),
                            DST_SUBPROGRAM_def_vtable_elem_location(sub),
                            flag);
  }
}




