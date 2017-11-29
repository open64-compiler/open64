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

// ====================================================================
// 
// Description:
// ============
// 
//   This header file defines some data structure regarding points-to 
//   summary.
// 
//  o. UNIFORM_NAME: uniform representation for the name of different 
//       kind of entities in a program. The entity can be a concrete 
//       named variable or some imaginary stuff. 
// 
//  o. UNAME_SPACE : a collection of UNIFORM_NAME.
//  o. PT_SET      : set of points-tos
//  o. PT_SET_MGR  : It host a UNAME_SPACE and all PT_SETs the 
//       pointers (represented by UNIFORM_NAME) in UNAME_SPACE points-to. 
//       Also, it associates the pointers and the PT_SET that pointers 
//       points-to.
//   
//  o. PU_POINTS_TO_SUMMARY: hosts set of points-to at the entry and exit
//       of given PU.
//  
//  o. - extern PU_POINTS_TO_SUMMARY* Allocate_PU_Points_To_Summary (ST* pu);
//     - extern PU_POINTS_TO_SUMMARY* Allocate_PU_Points_To_Summary (void);
//     - extern PU_POINTS_TO_SUMMARY* Get_points_to_summary (ST* pu);
//     - extern PU_POINTS_TO_SUMMARY* Get_points_to_summary (void);
//
//    Allocate, query PU_POINTS_TO_SUMMARY of given PU (or current pu if 
//    pu is not specified).
// 
// ====================================================================
//
#ifndef opt_points_to_summary_INCLUDED
#define opt_points_to_summary_INCLUDED

#include <vector>
#include "id_map.h"

using idmap::ID_MAP;

// some forward decl
class ALIAS_RULE;

typedef enum {
  UN_NAMED_GLOBAL, 
  UN_MALLOC,
  UN_ALLOCA,
  UN_FORMAL,
  UN_CALLER_LOCAL,
  UN_RET_VAL, 
} UNAME_TYPE;

// the ID of name of different categories. 
class SYMBOL_NAME {
public:
  union {
    ST* sym;           // has symbol 
    mUINT64 line_num;  // line-num is used to distringuish malloc/alloca etc.
    INT idx;           // index, say, the index of formal argument 
  }; 
  
  SYMBOL_NAME (ST* symbol) : sym(symbol) {};
  // formal "not_used" is actually used by compiler to choose desiriable 
  // overloading constructor 
  SYMBOL_NAME (mUINT64 ln, BOOL not_used) : line_num(ln) {not_used=not_used;};
  SYMBOL_NAME (INT index)  : idx(index) {};
  SYMBOL_NAME (void) : sym(NULL) {};
};

// UNIFORM_NAME: uniform names for following variable/entities in 
//   a program: named variable visible to current function, invisible
//   variables, memory block allocated via malloc/alloca, imaginary 
//   pointer returned from callee ...
// 
class UNAME_SPACE;
class UNIFORM_NAME {
friend class UNAME_SPACE;
private:

  UNAME_TYPE _type;  // interpret the previous union
  INT32 _unique_id;  // id of the name in the given name space. It is zero if 
                     // the name in question does not belong to any name space

  SYMBOL_NAME _name; // "name" of the symbol in question  
  void Set_unique_id (INT32 i) { _unique_id = i; }

public:
  UNAME_TYPE Type (void) const { return _type; }
  BOOL Is_named_global (void) const { return _type == UN_NAMED_GLOBAL; } 
  void Set_is_named_global (ST* st) {
         _type = UN_NAMED_GLOBAL; _name.sym = st;
       }
  ST*  ST_for_named_global (void) const {
          Is_True (Is_named_global(), ("should be named global var"));
          return _name.sym;
       }

  BOOL Is_malloc (void) const { return _type == UN_MALLOC; }
  void Set_is_malloc (mUINT64 id) { _type = UN_MALLOC; _name.line_num = id; }
  mUINT64 Malloc_id (void) const {
         Is_True (Is_malloc (), ("should be a symbol associated with malloc"));
         return _name.line_num;
       }
 
  BOOL Is_alloca (void) const { return _type == UN_ALLOCA; }
  void Set_is_alloca (mUINT64 id) { _type = UN_ALLOCA; _name.line_num = id; }
  mUINT64 Alloca_id (void) const {
         Is_True (Is_alloca(), ("should be a symbol associated with alloca"));
         return _name.line_num;
       }

  BOOL Is_formal (void) const { return _type == UN_FORMAL; }
  void Set_is_formal (INT idx) { _type = UN_FORMAL; _name.idx = idx;}
  INT  Formal_index (void) const {
         Is_True (Is_formal(), ("Should be corresponding to a formal")); 
         return _name.idx; 
       }

  // It is from callee's perspective. These functions are normally calleed 
  // in the forward pass of interprocedural points-to analysis when a caller 
  // summerizes points-to info at the callee's entry. Since caller local 'ST' 
  // is not visible to callee, the callee cannot check the caller local ST's 
  // attribute. However, they can be used to distinguish different caller 
  // local STs.
  BOOL Is_caller_local (void) const { return _type == UN_CALLER_LOCAL; }  
  void Set_is_caller_local (ST* st) {
         _type = UN_CALLER_LOCAL;
         // toggle the least significant bit so that if unaligned access 
         // will be signaled when the _caller_local is dereferenced.
         // Hint: the _caller_local is not visible to callee. 
         _name.sym = (ST*)((INTPTR)st ^ 1);
       }

  ST* Caller_local_st (void) {
        Is_True (Is_caller_local(), ("Should be caller's local symbol")); 
        return _name.sym;
      }

  INT32 Unique_id (void) const { return _unique_id; }

  // Construction 
  UNIFORM_NAME (ST* st, UNAME_TYPE type) {
    if (type == UN_NAMED_GLOBAL) Set_is_named_global (st);
    else if (type == UN_CALLER_LOCAL) Set_is_caller_local (st);
    else {
      FmtAssert (FALSE, ("Unexpected type %d", (INT)type));
    }
  }
  
  UNIFORM_NAME (mUINT64 line_num, UNAME_TYPE ty) {
    if (ty == UN_MALLOC) Set_is_malloc (line_num);
    else if (ty == UN_ALLOCA) Set_is_alloca (line_num);
    else {
      FmtAssert (FALSE, ("unexpected type %d", (INT)ty));
    }
  }

  void Copy (const UNIFORM_NAME& that) {
    *this = that;
  }

  UNIFORM_NAME (const UNIFORM_NAME& that) {
    *this = that;
  }
  
  void Print (FILE* f);
};

// define the type of vector<UNIFORM_NAME*>  
typedef mempool_allocator<UNIFORM_NAME*>  UNAME_ALLOC ;
typedef std::vector<UNIFORM_NAME*, UNAME_ALLOC>  UNAME_VECTOR;
typedef UNAME_VECTOR::iterator UNAME_VECTOR_ITER;

// the space for "symbolic name"
class UNAME_SPACE {
private:
  MEM_POOL* _mp;
  ID_MAP<UNIFORM_NAME*, UINT32> _global_map;
  UNAME_VECTOR _all_names;
  UNAME_VECTOR _malloc_alloca;
  UNAME_VECTOR _formals; 
  UNAME_VECTOR _caller_locals; 
  INT32 _next_id;

  void Append_new_name (UNIFORM_NAME* name);

public:
  UNAME_SPACE (INT32 estimated_sz, MEM_POOL* mp) : 
    _mp(mp), _global_map(estimated_sz, NULL, mp, FALSE), 
    _malloc_alloca(mp), _formals(mp), _caller_locals (mp) {

    _global_map.Init ();  
    _next_id = 1; // zero is reserved
  };
  void Copy (UNAME_SPACE& that); 

  ~UNAME_SPACE (void) {}

  UNAME_VECTOR& All_names (void) { return _all_names; }

  UNIFORM_NAME* Get_name_by_id (INT32 id) { 
    return (id > 0 && id <= _all_names.size()) ? _all_names[id-1] : NULL;
  }

  UNIFORM_NAME* Get_global (ST* st) { return _global_map.Lookup(ST_index(st)); }
  UNIFORM_NAME* Get_malloc (mUINT64 mallocid) {
    for (UNAME_VECTOR_ITER iter = _malloc_alloca.begin (); 
         iter != _malloc_alloca.end (); iter++) {
      UNIFORM_NAME* n = *iter; 
      if (n->Is_malloc() && n->Malloc_id() == mallocid) return n;
    }
    return NULL;
  }

  UNIFORM_NAME* Get_alloca (mUINT64 allocaid) {
    for (UNAME_VECTOR_ITER iter = _malloc_alloca.begin (); 
         iter != _malloc_alloca.end (); iter++) {
      UNIFORM_NAME* n = *iter; 
      if (n->Is_alloca () && n->Alloca_id() == allocaid) return n;
    }
    return NULL;
  }

  UNIFORM_NAME* Get_formal (INT32 idx) {
    for (UNAME_VECTOR_ITER iter = _formals.begin (); 
         iter != _formals.end (); iter++) {
      UNIFORM_NAME* n = *iter; 
      if (n->Is_formal() && n->Formal_index() == idx) return n;
    }
    return NULL;
  }

  UNIFORM_NAME* Get_uniform_name (const SYMBOL_NAME& name, UNAME_TYPE ty) {
    switch (ty) {
    case UN_NAMED_GLOBAL: return Get_global (name.sym);
    case UN_MALLOC:       return Get_malloc (name.line_num);
    case UN_ALLOCA:       return Get_alloca (name.line_num);
    case UN_FORMAL:       return Get_formal (name.idx);
    default: Is_True (FALSE, ("ty is unknown %d", ty)); 
    }
    return NULL;
  }

  // NOTE: if the name exist, the existing name is returned directly to 
  //       prevent duplication.
  UNIFORM_NAME* Add_global (ST*);
  UNIFORM_NAME* Add_malloc (mUINT64 mallocid);
  UNIFORM_NAME* Add_alloca (mUINT64 allocaid);
  UNIFORM_NAME* Add_formal (INT idx);
  UNIFORM_NAME* Add_caller_local (ST* localst);

  INT Cardinality (void) const { return _all_names.size (); }


  void Print (FILE*, BOOL verbose=FALSE);
};

// It applies to both individual POINTS_TO and PT_SET. For POINTS_TO, 
// PT_CERTAINTY suggests the degree of the certainty of that particular
// points-to relationship. PT_CERTAINTY for PT_SET is the "and" operations
// upon all the POINTS_TOs it contains.
typedef enum {
  PTC_POSSIBLE = 0, 
  PTC_DEFINITE = 1,
} PT_CERTAINTY;

// It applies to both individual POINTS_TO as well as PT_SET. For 
// POINTS_TO, PT_MAY_OR_MUST is a standard "may" and/or "must" as 
// well articulated in the literatures; PT_MAY_OR_MUST for PT_SET 
// indicates whether the given set of complete or not -- i.e, 
// if PT_SET for a pointer contains all possible points-tos at a 
// given point, that PT_SET is complete, or "must" points-to set,
// otherwise, it is "may" points-to set. 
typedef enum {
  PT_MAY_POINTS_TO = 0, 
  PT_MUST_POINTS_TO = 1,
} PT_MAY_OR_MUST;

typedef enum {
  PTAS_INVALID = 0,
  PTAS_UNKNOWN = 1, 
  PTAS_TEXT    = 2,    // points to code 
  PTAS_GLOBAL  = 4,    // points to global data
  PTAS_FSTATIC = 8,    // points to file scope static
  PTAS_LOCAL   = 16,   // points to local data (inc static)
  PTAS_INVISIBLE = 32, // points to data invisible to callee 
                       // (e.g local data of caller)
} PT_ADDR_SPACE;

// define vector<POINTS_TO*>
typedef mempool_allocator<POINTS_TO*>  PT_PTR_ALLOC ;
typedef std::vector<POINTS_TO*, PT_PTR_ALLOC>  PT_VECTOR; 
typedef PT_VECTOR::iterator PT_VECTOR_ITER;

// class PT_SET is used to describe a set of POINTS_TOs. 
class PT_SET {
private:
  MEM_POOL* _mp;
  PT_VECTOR _all_points_to; // all POINTS_TOs. 

  // summary of <_all_points_to>
  PT_ADDR_SPACE _addr_space;
  UINT32 _certainty:2; 
  UINT32 _may_or_must:2; 
  UINT32 _has_unknown_pt:2;

public:
  PT_SET (MEM_POOL* mp) : _mp(mp), _all_points_to(mp) {
    _addr_space = PTAS_INVALID; 
    _certainty = PTC_DEFINITE;
    _may_or_must = PT_MUST_POINTS_TO;
    _has_unknown_pt = FALSE;
  }

  PT_SET (PT_SET& pts) {
    FmtAssert (FALSE, ("MEMPOOL is not specified"));
  }

  PT_SET (PT_SET& pts, MEM_POOL* mp);
  
  void Copy (PT_SET& that);

  PT_SET& operator = (PT_SET& that) 
    { Copy (that); return *this; }
  
  PT_VECTOR& Points_to_set (void) { return _all_points_to; }
  UINT32 Cardinality (void) const { return _all_points_to.size (); }
  void Add_points_to (POINTS_TO* pt, PT_CERTAINTY, PT_MAY_OR_MUST);
  PT_ADDR_SPACE Addr_Space (void) const { return _addr_space; }

  // Accessors  
  void Set_addr_space (PT_ADDR_SPACE addr_space) { _addr_space = addr_space; }
  PT_ADDR_SPACE Addr_space (void) const { return _addr_space; }

  BOOL Has_unknown_pt (void) const { return _has_unknown_pt; } 
  void Set_has_unkown_pt (void) { _has_unknown_pt = TRUE; }
  void Reset_has_unknown_pt (void) { _has_unknown_pt = FALSE; }

  PT_CERTAINTY Certainty (void) const { return (PT_CERTAINTY)_certainty; }
  void Set_certainty (PT_CERTAINTY certainty) { _certainty = (UINT32)certainty; }

  // Query the alias relationship 
  BOOL Aliased (ALIAS_RULE* al, PT_SET* );
  BOOL Aliased (ALIAS_RULE* al, POINTS_TO*);

  // return TRUE iff <this> is more precise than <that>  
  BOOL More_Precise (PT_SET* that);

  // Meet all POINTS_TO into <pt> 
  void Meet (POINTS_TO* pt);

  void Print (FILE*);
};

typedef mempool_allocator<PT_SET*>  PT_SET_ALLOC ;
typedef std::vector<PT_SET*, PT_SET_ALLOC>  PT_SET_VECTOR; 
typedef PT_SET_VECTOR::iterator PT_SET_VECTOR_ITER;

// PT_SET_MGR host a name space and associate each name in the 
// name space with their corresponding points-to set.
class PT_SET_MGR {
private:
  MEM_POOL* _mp;
  UNAME_SPACE* _name_space; 
  ID_MAP<PT_SET*, INT32> _nameidx_ptset_map; 

public:
  PT_SET_MGR (MEM_POOL* mp, UNAME_SPACE* name_space = NULL) : 
    _mp(mp), _name_space(name_space), 
    _nameidx_ptset_map ((INT32)(name_space != NULL ? 
                                name_space->Cardinality () : 256), 
                                NULL, mp, FALSE)
  {
     if (_name_space == NULL) {
       _name_space = CXX_NEW (UNAME_SPACE(256, _mp), _mp);
     }
     _nameidx_ptset_map.Init ();
  }

  void Copy (PT_SET_MGR& that); 

  ~PT_SET_MGR (void) {};

  MEM_POOL* Mem_pool (void) const { return _mp; }

  UNAME_SPACE* Name_space (void) const { return _name_space; } 

  // return the points-to set associated with given <name>
  PT_SET* Points_to_set (UNIFORM_NAME* name) {
    Is_True (name == _name_space->Get_name_by_id(name->Unique_id()),
             ("name is not belong to the the name space"));
    return  _nameidx_ptset_map.Lookup (name->Unique_id());
  }

  // associate <name> with <pt_set>
  void Associate (UNIFORM_NAME* name, PT_SET* pt_set) {
    Is_True (name == _name_space->Get_name_by_id(name->Unique_id()),
             ("name is not belong to the the name space"));
    _nameidx_ptset_map.Insert (name->Unique_id(), pt_set); 
  }

  // if <name> is not associated with any points-to set, or the 
  // existing set is not as precise as <new_pt_set>, bind <name> 
  // with <new_pt_set>.
  void Substitute_If_More_Precise (UNIFORM_NAME* name, PT_SET* new_pt_set);

  UNIFORM_NAME* Add_global (ST* st) { _name_space->Add_global (st);}
  UNIFORM_NAME* Add_malloc (mUINT64 id) { _name_space->Add_malloc(id);}
  UNIFORM_NAME* Add_alloca (mUINT64 id) { _name_space->Add_alloca(id);}
  UNIFORM_NAME* Add_formal (INT idx){ _name_space->Add_formal(idx);}
  UNIFORM_NAME* Add_caller_local (ST* localst) {_name_space->Add_caller_local(localst);}

  void Print (FILE* f);
};

class PU_POINTS_TO_SUMMARY {
private: 
  MEM_POOL* _mp;

  // the points-to hold at the entry/exit points of given PU 
  PT_SET_MGR _in_set, _out_set; 

  // return TRUE iff <pt> points-to named object or a block 
  // of memory allocated by malloc() or alloca().
  // 
  // a points-to set with a element satisifying Pt_known_obj()
  // does not help memory disambiguation.


public:
  static BOOL Pt_known_obj (POINTS_TO* pt) {
     return pt->Base_is_fixed() && pt->Base() || 
            pt->Malloc_id () != 0;
  }

  PU_POINTS_TO_SUMMARY (MEM_POOL* mp): 
    _mp(mp), _in_set(mp), _out_set(mp) {}
  PU_POINTS_TO_SUMMARY (const PU_POINTS_TO_SUMMARY& summary);

  PT_SET_MGR& In_set (void) { return _in_set; }
  PT_SET_MGR& Out_set (void) { return _out_set; }
 
  void Print (FILE* f);
};

extern PU_POINTS_TO_SUMMARY* Allocate_PU_Points_To_Summary (ST* pu);

//Allocate_PU_Points_To_Summary () for current PU
extern PU_POINTS_TO_SUMMARY* Allocate_PU_Points_To_Summary (void);

// return the summary associated with <pu> 
extern PU_POINTS_TO_SUMMARY* Get_points_to_summary (ST* pu);

// return Get_points_to_summary() for current PU 
extern PU_POINTS_TO_SUMMARY* Get_points_to_summary (void);

#endif  /* opt_points_to_summary_INCLUDED */
