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

#ifndef cxx_ipa_cprop_INCLUDED
#define cxx_ipa_cprop_INCLUDED

#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif 

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif

#ifndef cxx_ipa_df_INCLUDED
#include "ipa_df.h"
#endif

extern INT IPA_Constant_Count;
extern void Init_Cprop_Annotations (IPA_NODE *);

extern MEM_POOL Ipa_cprop_pool;
extern MEM_POOL local_cprop_pool;
extern MEM_POOL Global_mem_pool;

// Maximum and count for the total number of clone nodes
extern UINT32 IPA_Max_Total_Clones;
extern UINT32 IPA_Num_Total_Clones;

typedef DYN_ARRAY<SUMMARY_VALUE> VALUE_DYN_ARRAY;

//-------------------------------------------------------
// global value for a specific element in a common block
//-------------------------------------------------------
class GLOBAL_VALUE
{
private:
  mUINT64        _offset; 
  mUINT32        _size;
  SUMMARY_VALUE* _value;
  
public:
  // constructor
  GLOBAL_VALUE (UINT64 offset, UINT32 size, SUMMARY_VALUE* value) :
    _offset (offset),
    _size   (size),
    _value  (value)
  {}

  UINT64         Offset () const { return _offset; }
  UINT32         Size ()   const { return _size; }
  SUMMARY_VALUE* Value ()  const { return _value; }
 
  BOOL Union (SUMMARY_VALUE* value, BOOL overwrite);
};

typedef DYN_ARRAY<GLOBAL_VALUE> GLOBAL_DYN_ARRAY;


// auxiliary data structures for mapping (offset,size) pairs
// for each common block into ST_IDXs from the global symtab
struct OFFSET_SIZE
{
  OFFSET_SIZE (UINT64 o, UINT32 s) : offset(o), size(s) {}

  UINT64 offset;
  UINT32 size;
};

namespace __gnu_cxx {

template<> struct hash<OFFSET_SIZE> 
{
  size_t operator()(OFFSET_SIZE x) const { return x.offset + x.size; }
};

}

struct OFFSET_SIZE_EQ
{
  bool operator()(OFFSET_SIZE os1, OFFSET_SIZE os2) const
  {
    return (os1.offset == os2.offset && os1.size == os2.size);
  }
};

typedef __gnu_cxx::hash_map<OFFSET_SIZE, ST_IDX, __gnu_cxx::hash<OFFSET_SIZE>, OFFSET_SIZE_EQ> 
OFFSET_SIZE_TO_ST_IDX_MAP;


//----------------------------------------------
// global annotation with global values for all 
// common/global symbols per procedure
//----------------------------------------------
class GLOBAL_ANNOT
{
private:
  MEM_POOL* _pool;
  GLOBAL_DYN_ARRAY** _gvals_array;

public:
  // static data members
  static UINT32 Size;
  static ST_IDX* Common_ST;
  static OFFSET_SIZE_TO_ST_IDX_MAP* Offset_Size_To_ST;

  // static member function
  static INT32 Index (ST_IDX);
  
  // constructors
  GLOBAL_ANNOT (MEM_POOL* mem_pool) : 
    _pool (mem_pool)
  {
    _gvals_array = CXX_NEW_ARRAY (GLOBAL_DYN_ARRAY*, Size, mem_pool);
    BZERO (_gvals_array, Size * sizeof(GLOBAL_DYN_ARRAY*));
  }

  GLOBAL_ANNOT (const GLOBAL_ANNOT* gannot, MEM_POOL* mem_pool) : 
    _pool (mem_pool)
  {
    _gvals_array = CXX_NEW_ARRAY (GLOBAL_DYN_ARRAY*, Size, mem_pool);
    BZERO (_gvals_array, Size * sizeof(GLOBAL_DYN_ARRAY*));

    for (UINT32 i = 0; i < Size; ++i) {
      if (!gannot->Top(i)) {
        if (gannot->Bottom(i)) {
          Set_Bottom(i);
        }
        else {
          _gvals_array[i] = CXX_NEW(GLOBAL_DYN_ARRAY(_pool), _pool);
          *(_gvals_array[i]) = gannot->Global_Value_Array(i);
        }
      }
    }
  }

  // accessor functions
  const GLOBAL_DYN_ARRAY& Global_Value_Array(INT32 idx) const { 
    return *(_gvals_array[idx]);
  }
  
  BOOL Top (UINT32 idx)    const { return _gvals_array[idx] == 0; }
  BOOL Bottom (UINT32 idx) const { return _gvals_array[idx] == (void*) -1; }
  void Set_Bottom (UINT32 idx) { 
    if (_gvals_array[idx] != 0 && _gvals_array[idx] != (void*) -1) {
      CXX_DELETE(_gvals_array[idx], _pool);
    }
    _gvals_array[idx] = (GLOBAL_DYN_ARRAY*) -1; 
  }

  BOOL Bottom () const { 
    for (UINT32 i = 0; i < Size; ++i) {
      if (!Bottom(i)) {
        return FALSE;
      }
    }
    return TRUE;
  }

  void Set_Bottom () { 
    for (UINT32 i = 0; i < Size; ++i) {
      Set_Bottom(i);
    }
  }

  // operations
  const GLOBAL_VALUE* Find(INT32 common_idx, UINT64 offset, UINT32 size) const;

  BOOL Union (UINT32 common_idx, 
              const GLOBAL_VALUE& gval, 
              BOOL overwrite_when_found,
              BOOL add_when_not_found);
  
  BOOL Union (const GLOBAL_ANNOT* gannot);

  void Print (FILE* fp);
};


class IPA_CPROP_DF_FLOW : public IPA_DATA_FLOW
{
protected:
  virtual void* Meet(void* in, void* vertex, INT *change);
  virtual void* Trans(void* in, void* out, void* vertex, INT *change);

public:
  IPA_CPROP_DF_FLOW (DF_DIRECTION ddf, MEM_POOL* m);
  
  virtual void InitializeNode(void *n);
  virtual void Print_entry(FILE *fp, void* out, void* n);
  virtual void PostProcessIO(void *node);

#if _UPDATEMODINFO_IS_NON_EMPTY_
  void InitializeEdge(IPA_EDGE*);
  void UpdateModInfo(IPA_EDGE*);
#endif // _UPDATEMODINFO_IS_NON_EMPTY_

  BOOL Valid_predecessors(IPA_NODE*);
  BOOL Valid_successors_without_cprop(IPA_NODE*);
  void Delete_dead_call(IPA_NODE*);
  void PerformCloning(IPA_NODE*);
  void Print_constants(FILE*, IPA_NODE*);

};

#endif // cxx_ipa_cprop_INCLUDED
