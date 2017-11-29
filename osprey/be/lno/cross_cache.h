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


//         This is -*- C++ -*-
//
//         cross_cache.h  :- models the contents of a cache across loop nests
//
//         Currently assumes :-
//                   Cache size is infinite and fully associative
//


#ifndef _cross_cache_INCLUDED
#define _cross_cache_INCLUDED


typedef enum {DISTRIBUTED, REPLICATED, EXCLUSIVE} CACHE_REGION_TYPE;
typedef enum {CACHE_READ_ONLY, CACHE_WRITE_ONLY, CACHE_READ_WRITE} ACCESS_TYPE;

class CACHE_REGION : public SLIST_NODE {
  CACHE_REGION_TYPE  _type;            // type of the cache region
  ARA_REF           *_reg;             // the region itself
  INT32              _dims;            // number of dimensions of the region
  INT32             *_dist;            // which dimensions were distributed
  INT32             *_offsets;         // offsets for each dimensions
  INT32             *_ranges;          // ranges for each dimensions
  INT32              _ndist;           // number of distributed dimensions
  BOOL               _is_messy;        // is this messy or not

public:
  CACHE_REGION(CACHE_REGION *c);
  CACHE_REGION(CACHE_REGION *c, ARA_REF *ref);
  CACHE_REGION(ARA_REF_INFO *ref, ARRAY_SNL_INFO *asi, UINT32 parallel_loop);
  CACHE_REGION *Clone_Region(CACHE_REGION *c);
  ARA_REF *Get_Ref(void) { return _reg; }
  BOOL Is_Messy(void) { return _is_messy; }
  void Set_Messy(BOOL flag) { _is_messy = flag; }
  REGION *Get_Region(void) { REGION_ITER iter(&_reg->Image()); return iter.First(); }
  INT32 Region_Size(void);
  void Print(FILE *file);
  CACHE_REGION_TYPE Type(void) { return _type; }
  INT32 N_Dist() { return _ndist; }
  INT32 *Dist(void) { return _dist; }
  INT32 Intersect_Region(CACHE_REGION *c, ARA_LOOP_INFO *ara_info);

  ~CACHE_REGION() {
  }

};
  
class CACHE_REGION_LIST : public SLIST {
  DECLARE_SLIST_CLASS(CACHE_REGION_LIST, CACHE_REGION);

public:
  ~CACHE_REGION_LIST() {
    while (!Is_Empty()) CXX_DELETE(Remove_Headnode(), &LNO_local_pool);
  }
  
};

typedef enum {DIRECT_MAPPED, FULLY_ASSOCIATIVE, SET_ASSOCATIVE} CACHE_TYPE;

class CACHE_CONTENTS {
  CACHE_TYPE         _type;            // type of the cache
  UINT64             _size;            // size of the cache
  CACHE_REGION_LIST  _reg_list;        // list of cache regions
  ARA_LOOP_INFO      *_ara_info;       // ara_loop_info of the outermost loop
  INT32               _nprocs;

  void Add_Region_Distributed(CACHE_REGION *c, ACCESS_TYPE atype);
  void Add_Region_Replicated(CACHE_REGION *c, ACCESS_TYPE atype);
  void Add_Region_Exclusive(CACHE_REGION *c, ACCESS_TYPE atype);

public:
  CACHE_CONTENTS(CACHE_TYPE type, UINT64 size, INT32 nprocs, ARA_LOOP_INFO *_ara_info);
  CACHE_CONTENTS(CACHE_CONTENTS *cc);
  INT32 N_Procs(void) { return _nprocs; }
  void Add_Region(CACHE_REGION *c, ACCESS_TYPE atype);
  INT32 Intersect_Region(CACHE_REGION *c);
  void Compact_Cache(void);
  void Print(FILE *file);
};

class CACHE_REGION_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(CACHE_REGION_ITER, CACHE_REGION, CACHE_REGION_LIST);
};

class CACHE_REGION_CONST_ITER : public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS(CACHE_REGION_CONST_ITER, CACHE_REGION, CACHE_REGION_LIST);
};

#endif












