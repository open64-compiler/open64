/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//
// Created by xc5 on 22/8/2018.
//
#include "b2w_common.h"
#include "dwarf_DST_producer.h"

//=============================================================================
UINT32 B2W_new_scope(MPPTR pool_ptr) {
    Is_Valid(Current_Map_Tab != NULLPTR,
            ("[B2W_new_scope] Please call b2w_map_tab_create"
             " before calling new_scope\n"));

    Is_Valid(CURRENT_SYMTAB == 1,
            ("%s","[B2W_new_scope] [ERROR] CURRENT_SYMTAB not 1, function nested?,"
                  " or forget to finish previous function? \n"));

    Is_Valid(pool_ptr != NULLPTR,
            ("[B2W_new_scope] [ERROR] given a null pool ptr."));

    New_Scope(CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);   // Always for level 2

    return CURRENT_SYMTAB;                                  // Always return 1
}

UINT B2W_get_dir_dst_info (CHPTR name)
{
  std::vector< std::pair < CHPTR, UINT > >::iterator found;
  // assume linear search is okay cause list will be small?
  for (found  = B2W_CONTEXT::Get_dir_dst_list()->begin();
       found != B2W_CONTEXT::Get_dir_dst_list()->end();
     ++found)
  {
    if (strcmp ((*found).first,  name) == 0) {
      return    (*found).second;
    }
  }
  // not found, so append path to dst list
  // We have to create a new home for name because memory
  // will be freed once the caller exits.
  CHPTR  new_name    = (CHPTR) malloc((strlen(name)+1) * sizeof(char));
  INT    last_index  = B2W_CONTEXT::Get_dir_dst_list()->size() + 1;
  strcpy(new_name, name);
  name               = new_name;
  B2W_CONTEXT::Get_dir_dst_list()->push_back (std::make_pair (name, last_index));
  DST_mk_include_dir (name);
  return last_index;
}

/**
 *  Internal Use, please migrate to B2W_get_file_dst_info
 * */
static void B2W_enter_file(CHPTR file_name_enter, UINT dir) {
  UINT64 file_size = 0;
  UINT64 fmod_time = 0;
  struct stat fstat;
  if (stat(file_name_enter, &fstat) == 0) {
    // File was found, so set to non-zero values
    file_size = (UINT64)fstat.st_size;
    fmod_time = (UINT64)fstat.st_mtime;
  }
  DST_mk_file_name(file_name_enter, (mUINT16) dir, file_size, fmod_time);
}

/**
 * Create a DST file info object for file name
 * @param name File name
 * @param dir Directory DST ID
 * @return
 */
UINT B2W_get_file_dst_info(CHPTR name, UINT dir) {

  Is_Valid(name != NULLPTR && dir > 0,
      ("[B2W_get_file_dst_info] Trying to save a nullptr or 0 dir as path."));

  CUUPAIRVEC  *list      = B2W_CONTEXT::Get_file_dst_list();
  CUUPAIRVEC::iterator found;
  CHPTR new_name;
  // assume linear search is okay cause list will be small?
  for (found = list->begin(); found != list->end(); ++found) {
    // Copy permitted
    CUPAIR file_name_dir_pair = found->first;
    if (strcmp(file_name_dir_pair.first, name) == 0 && file_name_dir_pair.second == dir) {
      return found->second;
    }
  }

  new_name = (CHPTR) malloc((strlen(name) + 1) * sizeof(char)); // not found, so append file to dst list
  strcpy(new_name, name);  // We have to create a new home for name because memory
  name     = new_name;     // will be freed once the caller exits.

  list->push_back(std::make_pair(std::make_pair(name, dir), list->size() + 1));
  B2W_enter_file(name, dir);
  return (UINT) list->size();
}
