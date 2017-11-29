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
#ifndef cxx_ipa_pad_INCLUDED
#define cxx_ipa_pad_INCLUDED
#define USE_STANDARD_TYPES
#ifndef defs_INCLUDED
#include "defs.h"
#endif

#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif

#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif

// ===========================================================
// A simple FLD to ST map, created from a common block
// declaration.
// ===========================================================
class ST_TO_FLD_MAP
{
private:
  ST  *_st;
  FLD_HANDLE _fld;

public:
  ST_TO_FLD_MAP() {};
  ~ST_TO_FLD_MAP() {};

  ST * Get_ST() { return _st; };
  FLD_HANDLE Get_FLD() { return _fld;};

  void Init(FLD_HANDLE fld, ST *s) { _fld = fld; _st = s;};
}; // class ST_TO_FLD_MAP

typedef DYN_ARRAY<ST_TO_FLD_MAP> ST_TO_FLD_MAP_ARRAY;

//======================================================
// 
// And array describing the bounds information
//
//======================================================
class BOUNDS
{
private:
 static const UINT8 IS_CONSTANT_BOUNDS = 0x1;
 INT64 _upper, _lower, _stride;
 UINT8 _flags;

public:
 void Init() { _upper = _lower = _stride = 0;};
 void Init(INT64 upper, INT64 lower, INT64 stride) {
    _upper = upper; _lower = lower; _stride = stride;
  };

 void Set_Constant() { _flags = _flags | IS_CONSTANT_BOUNDS; };
 BOOL Is_Constant() { return _flags & IS_CONSTANT_BOUNDS;};

 INT64 Get_Upper() { return _upper;};
 INT64 Get_Lower() { return _lower;};
 INT64 Get_Stride() { return _stride;};
 void Set_Upper(INT64 upper) { _upper = upper;};
 void Set_Lower(INT64 lower) { _lower = lower;};
 void Set_Stride(INT64 stride) { _stride = stride;};

};

typedef DYN_ARRAY<BOUNDS> BOUNDS_ARRAY;


// ===========================================================
//
// An array describing how to split the common
//
// ===========================================================
class SPLIT_COMMON
{
private:
  INT64 _offset;
  INT64 _size;
  INT _element_size;
  INT _split_position; // which split common this element belongs to
  INT _group_position; // which group of equivalences this element
		       // belongs to
  
public:    
  INT64 Get_offset() const { return _offset;};
  INT64 Get_size() const { return _size;};
  INT Get_element_size() const { return _element_size;};
  INT Get_split_position() const { return _split_position;};
  INT Get_group_position() const { return _group_position;};

  void Set_offset(INT offset) { _offset = offset;};
  void Set_size (INT size) { _size = size;};
  void Set_element_size(INT size ) { _element_size = size;};
  void Set_split_position(INT position) { _split_position =
					    position;};
  void Set_group_position(INT position) { _group_position = 
					    position;};
  SPLIT_COMMON () { _offset = 0; _size = 0; _element_size = 0; 
		    _split_position = 0; _group_position = 0;};
  void Init() { _offset = 0; _size = 0; _element_size = 0;};
  void Set_Vals(INT64 offset, INT64 size, INT element_size, INT
		split_pos, INT group_pos) { 
    _offset = offset; 
    _size = size; 
    _element_size = element_size;
    _split_position = split_pos;
    _group_position = group_pos;
  };
};


// ===========================================================
// information about an element of a common block 
// ===========================================================
class COMMON_SNODE : public SLIST_NODE 
{
private:
  static const mUINT8 _set_pad = 0x1;    
  ST* _st;
  UINT8 _flags;
  BOUNDS_ARRAY* _bounds_array;

public:
  DECLARE_SLIST_NODE_CLASS (COMMON_SNODE);
  COMMON_SNODE (MEM_POOL *m, ST* st);
  ~COMMON_SNODE() {};

  
  void Set_ST(ST* st) { _st = st;};
  ST* Get_ST() { return _st;};

  void Set_Pad() { _flags = _flags  | _set_pad; };
  BOOL Pad() {  return _flags & _set_pad;};
  
  BOUNDS_ARRAY* 
    Get_Bounds_Array() { return _bounds_array;};

}; // COMMON_SNODE

typedef DYN_ARRAY<SPLIT_COMMON> SPLIT_COMMON_DYN_ARRAY;

// ===========================================================
// a list of elements belonging to a common block
// ===========================================================
class COMMON_SNODE_LIST : public SLIST 
{
  static const UINT8 _set_no_pad =  0x1;
  static const UINT8 _set_no_split = 0x2;
  UINT8 _flags;
  ST_TO_FLD_MAP_ARRAY *_map;

  ST *_st;
  SPLIT_COMMON_DYN_ARRAY *_split_array;

  DECLARE_SLIST_CLASS (COMMON_SNODE_LIST, COMMON_SNODE);

public:
  void New_Append (ST* s, MEM_POOL *m);

  void Set_No_Pad() { _flags = _flags | _set_no_pad; };
  BOOL No_Pad() { return _flags & _set_no_pad;};

  void Set_No_Split() { _flags = _flags | _set_no_split;};
  BOOL No_Split() {return _flags & _set_no_split;};

  void Free_Nodes(MEM_POOL *m);

  ST_TO_FLD_MAP_ARRAY *Get_Map() const { return _map;};
  COMMON_SNODE_LIST(MEM_POOL *m, ST* s);
  
  void Set_ST(ST *st) { _st = st;}
  ST* Get_ST() { return _st;};

  SPLIT_COMMON_DYN_ARRAY* 
    Get_Split_Array() const { return _split_array;};
};

class COMMON_SNODE_LIST_ITER: public SLIST_ITER {

public:
    DECLARE_SLIST_ITER_CLASS(COMMON_SNODE_LIST_ITER, COMMON_SNODE,
			     COMMON_SNODE_LIST); 
};

typedef HASH_TABLE<STR_IDX, COMMON_SNODE_LIST*> COMMON_SNODE_TBL;

typedef HASH_TABLE_ITER<STR_IDX, COMMON_SNODE_LIST*> COMMON_SNODE_TBL_ITER;




extern 
BOOL Is_Common_Based_Symbol(const ST* s);

extern INT64 
Common_Array_Pad_Size(INT);

extern void
Padding_Analysis (INT num_ir);

extern INT IPO_Pad_Count;

extern COMMON_SNODE_TBL* IPA_Common_Table;

#endif /* ipa_pad_INCLUDED */
