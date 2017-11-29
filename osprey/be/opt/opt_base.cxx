//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_base.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_base.cxx,v $
//
// Revision history:
//  8-SEP-94 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// Implements the class MAP, which provides a hash-table based mapping
// from one type to another.
//
// Currently, MAP is only used in opt_cfg.h to map labels to BBs.
// Perhaps it should be replaced by STL hash_map.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_base_CXX	"opt_base.cxx"
static char *rcs_id = 	opt_base_CXX"$Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "wn.h"
#include "errors.h"
#include "erglob.h"
#include "cxx_memory.h"

#include "opt_sys.h"
#include "opt_base.h"


// ====================================================================


void
MAP::Alloc_hash_vec()
{
  hash_vec = CXX_NEW_ARRAY(MAP_LIST_P, size+1, mem_pool);
  if ( hash_vec == NULL ) ErrMsg ( EC_No_Mem, "MAP::Alloc_hash_vec" );
  BZERO(hash_vec, sizeof(MAP_LIST_P) * (size+1));
}

void
MAP::Free_hash_vec()
{
  CXX_DELETE_ARRAY(hash_vec, mem_pool);
}

MAP::MAP(mUINT32 hash_size, MEM_POOL *pool)
{
  mem_pool = pool;
  size = hash_size;
  Alloc_hash_vec();
}

MAP::~MAP()
{
  Free_hash_vec();
}

mUINT32
MAP::Hash(POINTER k)
{
  mUINT32      retval;

  retval = UINTPS(k) % size;
  return retval;
}

MAP_LIST *
MAP::Find_map_list(POINTER k)
{
  mUINT32 idx = Hash(k);
  MAP_LIST *map_lst;

  map_lst = hash_vec[idx];

  if (map_lst == NULL) {
    // create a new map_lst
    map_lst = CXX_NEW(MAP_LIST(k, 0), mem_pool);
    hash_vec[idx] = map_lst;
    return map_lst;
  }

  MAP_LIST_ITER map_lst_iter;
  MAP_LIST *tmp;
  FOR_ALL_NODE(tmp, map_lst_iter, Init(map_lst)) {
    if (tmp->Key() == k)
      return tmp;
  }

  SLIST map_lst_container(map_lst);
  map_lst = CXX_NEW(MAP_LIST(k, 0), mem_pool);
  map_lst_container.Append(map_lst);
  return map_lst;
}

void
MAP::Add_map(POINTER k, POINTER v)
{
  MAP_LIST *map_lst = Find_map_list(k);
  if (map_lst->Val() == NULL)
    map_lst->Set_val(v);
  FmtAssert(map_lst->Val() == v, ("MAP::Add_map: key %d is not unique", k));
}

// Add value to the map, override existing values.
void
MAP::Override_add_map(POINTER k, POINTER v)
{
  MAP_LIST *map_lst = Find_map_list(k);
  map_lst->Set_val(v);
}

POINTER
MAP::Get_val(POINTER k)
{
  MAP_LIST *map_lst = Find_map_list(k);
  return map_lst->Val();
}

#ifdef Is_True_On
void	      
MAP::Dump_map(FILE *fp)
{
  if ( hash_vec == NULL ) 
    return;
  MAP_LIST *tmp;
  MAP_LIST_ITER map_lst_iter;
  for (UINT32 idx = 0; idx < size; idx++) {
    fprintf(fp, "----- idx=%d ----\n", idx);
    FOR_ALL_NODE(tmp, map_lst_iter, Init(hash_vec[idx]))
      fprintf(fp, "key=%p val=%p\n", tmp->Key(), tmp->Val());
  }
}
#endif

// ====================================================================
