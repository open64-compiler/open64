//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company, 
//  All Rights Reserved.
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
// ====================================================================

#ifndef BE_MEMOP_ANNOT_included
#define BE_MEMOP_ANNOT_included

#include "opt_alias_rule.h" // for LMV_ALIAS_GROUP

// forward decl
class MEMOP_ANNOT_ITEM;     // depict individual anntotation 
class MEMOP_ANNOT;          // all annotations associated with a single mem-op
class WN_MEMOP_ANNOT_MGR;   // manage ld/st WN and their mem-opannotations 
class WOPT_MEMOP_ANNOT_MGR; // manage ld/st CODEREP/STMTREP and their annotations 
class PT_MEM_ANNOT;         // bind annotation and POINTS_TO 

///////////////////////////////////////////////////////////////////////////
//
//  MEMOP_ANNOT_ITEM describe a single annotation of a WN/CODEREP/STMTREP.
//  MEMOP_ANNOT is used to depict all annotations of a WN/CODEREP/STMTREP.
// 
///////////////////////////////////////////////////////////////////////////
// 
typedef enum {
  MEM_ANNOT_INVALID             = 0, 
  MEM_ANNOT_MALLOC_ID           = 1,/* = 2**0 */ 
  MEM_ANNOT_LMV_ALIAS_GROUP     = 2 /* = 2**1 */,
                                /* next item should have value 4*/
  MEM_ANNOT_LAST_KIND           =2,
} MEM_ANNOT_KIND;

// An item with this value will be eliminated
//
#define INVALID_ITEM_VALUE (0)

class MEMOP_ANNOT_ITEM {
friend class MEMOP_ANNOT;
private:
  MEM_ANNOT_KIND _kind;
  union { UINT64 u64; UINT32 u32; void* ptr; } _v; 

  void Set_kind (MEM_ANNOT_KIND kind) { _kind = kind; };

public:
  // No constructor since it will used as a union member
  void Init (void) { _kind = MEM_ANNOT_INVALID; }

  MEM_ANNOT_KIND Kind(void) const { return _kind; }

  UINT64 Malloc_id (void) const 
    { return _kind == MEM_ANNOT_MALLOC_ID ? _v.u64 : INVALID_ITEM_VALUE; } 
  void Set_malloc_id (UINT64 id) { _kind = MEM_ANNOT_MALLOC_ID; _v.u64 = id; }

  LMV_ALIAS_GROUP LMV_alias_group (void) const { 
       return _kind == MEM_ANNOT_LMV_ALIAS_GROUP ? 
       _v.u32 : INVALID_ITEM_VALUE;
    }
  void Set_LMV_alias_group (LMV_ALIAS_GROUP g) 
    { _kind = MEM_ANNOT_LMV_ALIAS_GROUP; _v.u32 = g; }

  // return TRUE either kind is invalid or the data is useless
  BOOL Is_invalid (void) const {
       switch (_kind) {
       case MEM_ANNOT_MALLOC_ID : return Malloc_id () == INVALID_ITEM_VALUE;
       case MEM_ANNOT_LMV_ALIAS_GROUP: 
            return LMV_alias_group() == INVALID_ITEM_VALUE;
       default: return TRUE;
       }
     }

  BOOL operator == (const MEMOP_ANNOT_ITEM& that) const; 
  BOOL operator != (const MEMOP_ANNOT_ITEM& that) const 
    { return !operator==(that); }

  void Print (FILE* f) const;
};

typedef mempool_allocator<MEMOP_ANNOT_ITEM>      MEMOP_ANNOT_ITEM_ALLOC;
typedef std::vector<MEMOP_ANNOT_ITEM, MEMOP_ANNOT_ITEM_ALLOC> MEMOP_ANNOT_ITEM_VECT;
typedef MEMOP_ANNOT_ITEM_VECT::iterator          MEMOP_ANNOT_ITEM_ITER;
typedef MEMOP_ANNOT_ITEM_VECT::const_iterator    MEMOP_ANNOT_ITEM_CITER;


class MEMOP_ANNOT {
// so, these classes are able to call private constructor 
friend class MEMOP_ANNOT_MGR;
friend class WN_MEMOP_ANNOT_MGR;
friend class WOPT_MEMOP_ANNOT_MGR; 

private:
  MEMOP_ANNOT_ITEM_VECT _vect;
  MEM_ANNOT_KIND _kinds; // collect kinds of all items
  INT _id; 

  void Clear_kinds (void) { _kinds = (MEM_ANNOT_KIND)0; }
  void Add_kind (MEM_ANNOT_KIND k) { _kinds = MEM_ANNOT_KIND(UINT(_kinds) | UINT(k)); }
  void Remove_kind (MEM_ANNOT_KIND k) { _kinds = MEM_ANNOT_KIND(UINT(_kinds) & ~UINT(k));}

  // This was discarded and is now recycled as a new annotation with <new_id>.
  void Recycle (INT new_id) 
    { _vect.clear () ; Clear_kinds (); _id = new_id; }

  // private constructor guarantees that only {WN|WOPT}_MEMOP_ANNOT_MAP 
  // can allocate a instance.
  //
  MEMOP_ANNOT (MEM_POOL* mp, INT id) : _vect(mp), _id(id) 
    { Clear_kinds (); }

public:
  INT Id (void) const { return _id; } 
  MEMOP_ANNOT_ITEM_VECT&  All_items (void) { return _vect; }

  void operator = (const MEMOP_ANNOT& that) {
    // copy all fields except _id.
    _vect = that._vect; _kinds = that._kinds; 
  }

  BOOL Lookup (MEM_ANNOT_KIND kind, MEMOP_ANNOT_ITEM& res) const;
  void Replace_or_add (const MEMOP_ANNOT_ITEM& annot);
  void Remove (MEM_ANNOT_KIND kind);
  void Remove_all (void) { _vect.clear (); Clear_kinds(); }

  // Meet the two annotation sets. This function is useful when propagating 
  // data flow information at joint node. The net result is to get rid of 
  // all <this>'s annotations that does not show up in <that>.
  //
  void Meet (const MEMOP_ANNOT* that); 
  void Meet (const MEMOP_ANNOT_ITEM* that); 

  // combine <that> into <this>
  void Union (const MEMOP_ANNOT* that);

  INT Item_count (void) const { return _vect.size (); }

  MEMOP_ANNOT_ITEM& operator[] (INT idx) { 
    Is_True (idx >= 0 && idx < _vect.size(), ("index is out of range"));
    return _vect[idx];
  }

  void Print (FILE*) const;
};

typedef mempool_allocator<MEMOP_ANNOT*>  MOA_ALLOC;
typedef std::vector<MEMOP_ANNOT*, MOA_ALLOC>  MEMOP_ANNOT_VECT;
typedef MEMOP_ANNOT_VECT::iterator       MOA_VECT_ITER;
typedef MEMOP_ANNOT_VECT::const_iterator MOA_VECT_CITER;

class MEMOP_ANNOT_MGR {
protected:
  static MEMOP_ANNOT_MGR* active_mgr; 

  WN_MAP _wn_map;
  // last id of MEMOP_ANNOT; 
  INT _last_id;

  MEMOP_ANNOT_VECT _all_annot;
  MEMOP_ANNOT_VECT _recycled_annot;
 
  MEM_POOL* _mp;

public:
  INT Annot_last_id (void) const { return _last_id; }
  MEM_POOL* Mem_pool (void) const { return _mp; }

  MEMOP_ANNOT* Get_annot (INT id) {
    return (id > 0 && id <= _last_id) ? _all_annot[id] : NULL;
  }

  MEMOP_ANNOT* Alloc_annot (void);
  // virtual function provided for class PT_MEM_ANNOT 
  virtual MEMOP_ANNOT* Alloc_annot_v (void) { Alloc_annot (); }

  BOOL Alloc_by_this_class (MEMOP_ANNOT* a) {
    return a->Id() > 0 && a->Id() <= _last_id && _all_annot[a->Id()] == a;
  }

  void Init (void);
  void Invalidate (void) { Init(); }

  void Set_active_mgr (void) { active_mgr = this; }
  void Reset_active_mgr (void) { active_mgr = NULL; }
  static MEMOP_ANNOT_MGR* Active_mgr (void) { return active_mgr; }

  MEMOP_ANNOT_MGR (MEM_POOL* mp);
  ~MEMOP_ANNOT_MGR (void) { if (this == active_mgr) active_mgr = NULL; } 
};

///////////////////////////////////////////////////////////////////////////
//
// MEMOP_ANNOT_WN_MGR is a map between WN and their corresponding annotations
//
///////////////////////////////////////////////////////////////////////////
//
class WN_MEMOP_ANNOT_MGR:public MEMOP_ANNOT_MGR {
private:
  WN_MAP _wn_map;
  static WN_MEMOP_ANNOT_MGR* _unique_instance;

public:
  static WN_MEMOP_ANNOT_MGR* WN_mem_annot_mgr (void) 
      { return _unique_instance; }

  MEMOP_ANNOT* Get_annot (WN* wn) const 
    { return (MEMOP_ANNOT*)WN_MAP_Get (_wn_map, wn); }
 
  void Set_annot (WN* wn, MEMOP_ANNOT* annot) {
    Is_True (Alloc_by_this_class (annot), 
             ("annotation is not allocated by this class"));
    WN_MAP_Set (_wn_map, wn, (void*)annot);
  }

  // maintain the map during the lowering phase
  void Copy_annot (WN* orig, WN* lowed) {
    MEMOP_ANNOT* p = Get_annot (orig);  
    if (p) WN_MAP_Set (_wn_map, lowed, (void*)p);
  }

  // Import the annotation from another "manager". This class should 
  // duplicate the annotation instance and establish the map between 
  // <wn> and the duplicated annotation. 
  //
  void Import_annot (WN* wn, MEMOP_ANNOT* annot) 
    { MEMOP_ANNOT* t = Alloc_annot (); *t = *annot; Set_annot (wn,t); }

  void Init (void);
  void Invalidate (void) { Init(); }
   
  WN_MEMOP_ANNOT_MGR (MEM_POOL* mp);
  ~WN_MEMOP_ANNOT_MGR (void);

  void Print (FILE* f) const;
};

/////////////////////////////////////////////////////////////////////////////
//
//   Some handy interafaces about WN_MEMOP_ANNOT_MGR 
//
/////////////////////////////////////////////////////////////////////////////
//
class WN_MEMOP_ANNOT_MGR_Constructor {
private:
  MEM_POOL* _mp;
public:
  WN_MEMOP_ANNOT_MGR_Constructor (void) { 
    _mp = MEM_pu_nz_pool_ptr;
    WN_MEMOP_ANNOT_MGR* p = CXX_NEW (WN_MEMOP_ANNOT_MGR(_mp), _mp);
    p->Init();  
  }

  ~WN_MEMOP_ANNOT_MGR_Constructor (void) {
    WN_MEMOP_ANNOT_MGR* p = WN_MEMOP_ANNOT_MGR::WN_mem_annot_mgr ();
    if (p) CXX_DELETE (p, _mp); 
  }
};

/////////////////////////////////////////////////////////////////////////////
//
//  PT_MEM_ANNOT is employed to associcate POINTS_TO with the corresponding 
// annotation. This data structure takes into account following factors:
//   o. efficency: most memory operations has at most one annotation 
//      (at least, currently it is so). In this case, the annotation should 
//      be accessible directly from POINTS_TO structure.
//   o. compaction: the data structure should be compact enough since there
//      are large number of POINTS_TO structures.
//   o. flexibility: it is possible to associate POINTS_TO with more than 
//      one annotatios. 
// 
/////////////////////////////////////////////////////////////////////////////
//
class PT_MEM_ANNOT {
private:
  enum PT_MEM_ANNOT_SELECTOR {
    USE_INLINE_ITEM = 1, 
    USE_ANNOT_PTR   = 2, 
  };
  PT_MEM_ANNOT_SELECTOR _flags;
  union {
    MEMOP_ANNOT_ITEM _inlined_annot;
    MEMOP_ANNOT* _annot_ptr;
  };

public:
  PT_MEM_ANNOT (void) { _flags = (PT_MEM_ANNOT_SELECTOR)0; }
  BOOL Has_annotation (void) const { return _flags != 0; }
  INT Annot_count (void) {
    if (!_flags) return 0;
    if (_flags == USE_INLINE_ITEM && !_inlined_annot.Is_invalid()) return 1;
    if (_flags == USE_ANNOT_PTR) return _annot_ptr->Item_count();
    return 0;
  }
  void Init (void) { _flags = (PT_MEM_ANNOT_SELECTOR)0 ; _annot_ptr = NULL; }
  void Invalidate (void) { Init(); }

  BOOL Item_is_inlined (void) const { return _flags == USE_INLINE_ITEM; }
  const MEMOP_ANNOT_ITEM& Get_inlined_item (void) const 
         { return  _inlined_annot; }
  MEMOP_ANNOT* Get_annots_ptr (void) const { return _annot_ptr; } 

  void Replace_or_add_annot  (const MEMOP_ANNOT_ITEM& item);
  void Replace_or_add_annots (MEMOP_ANNOT* annots);
  void Set_annots (MEMOP_ANNOT* annots) 
        {_flags = USE_ANNOT_PTR; _annot_ptr = annots; }
  void Remove_annot (MEM_ANNOT_KIND kind);
  void Remove_all_annot (void) { Init (); }

  BOOL Lookup (MEM_ANNOT_KIND kind, MEMOP_ANNOT_ITEM& t) const;

  UINT64 Malloc_id (void) const {
    if (!_flags) return INVALID_ITEM_VALUE; 
    MEMOP_ANNOT_ITEM t; 
    return Lookup (MEM_ANNOT_MALLOC_ID, t) ? t.Malloc_id() : 0;           
  }

  LMV_ALIAS_GROUP LMV_alias_group (void) const {
    if (!_flags) return INVALID_ITEM_VALUE;
    MEMOP_ANNOT_ITEM t; 
    return Lookup (MEM_ANNOT_LMV_ALIAS_GROUP, t) ? t.LMV_alias_group () : 0;           
  }

  void Set_malloc_id (UINT64 id);
  void Set_LMV_alias_group (LMV_ALIAS_GROUP grp_id);

  void Meet (const PT_MEM_ANNOT& that);
  
  void Print (FILE* f, BOOL verbose) const;
};

#endif
