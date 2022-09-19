/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

#ifndef CLASS_HIERARCHY_INCLUDED
#define CLASS_HIERARCHY_INCLUDED

#include <vector>
#include <ext/hash_map>
#include <ext/hash_set>
#include <algorithm>
#include <stdio.h>

#include "defs.h"
#include "symtab.h"
#include "tracing.h"
#include "mempool_allocator.h"
#include "glob.h"
#include "j_aux_info.h"
#include "symtab_access_global.h"

using namespace std;
#define VTABLE_MARK  "_ZTV"

// virtual functions start at vtable+ (2*pointer_size)
#define VF_START_OFFSET Pointer_Size * 2
#define VTABLE_HASH_TABLE_SIZE      3
#define CAND_CALL_HASH_TABLE_SIZE   13
#define CLASS_LIST_HASH_TABLE_SIZE  7
#define INVALID_FILE_IDX            0
#define INVALID_VTABLE_OFFSET       (-0x7FFFFFFF)

class STRING_BUFFER;
class CLASS_INFO;
class JAVA_AUX_INFO;
class JAVA_METH_INFO;
struct VIRFUNC_INFO;
class CLASS_HIERARCHY;

using __gnu_cxx::hash_set;

// =============================================================================
// hash function for VIRFUNC_INFO
// =============================================================================
namespace __gnu_cxx {
  template<>
  struct hash<VIRFUNC_INFO*> {
    size_t operator()(const VIRFUNC_INFO* ptr) const { return (size_t)(ptr); }
  };
}


typedef const char* NAME;
typedef UINT32 TY_INDEX;  // the higher bits of type index

typedef mempool_allocator<VIRFUNC_INFO *> VIRFUNC_INFO_ALLOCATOR;
typedef vector<VIRFUNC_INFO*, VIRFUNC_INFO_ALLOCATOR> VIRFUNC_INFO_VEC;
typedef VIRFUNC_INFO_VEC::iterator VIRFUNC_INFO_VEC_ITER;



// map call offset to all possible virtual functions
typedef pair<CALL_OFF, VIRFUNC_INFO_VEC*> CALL_OFF2VIRFUNC_INFO;
typedef mempool_allocator<CALL_OFF2VIRFUNC_INFO>  CALL_VEC_MAP_ALLOCATOR;
typedef hash_map<CALL_OFF, VIRFUNC_INFO_VEC*, __gnu_cxx::hash<CALL_OFF>, std::equal_to<CALL_OFF>, CALL_VEC_MAP_ALLOCATOR> CALL_VEC_MAP;
typedef CALL_VEC_MAP::iterator CALL_VEC_MAP_ITER;
typedef CALL_VEC_MAP::const_iterator CALL_VEC_MAP_CONST_ITER;
typedef CLASS_HIERARCHY CHA;

typedef hash_set<C_STR, __gnu_cxx::hash<C_STR>, equal_str, C_STR_ALLOCATOR> CLASS_SET;
typedef CLASS_SET::iterator CLASS_SET_ITER;

typedef pair<C_STR, CLASS_INFO *> TY2CLASS_INFO;
typedef mempool_allocator<TY2CLASS_INFO> CLASS_INFO_ALLOCATOR;
typedef hash_map<C_STR, CLASS_INFO*, __gnu_cxx::hash<C_STR>, equal_str, CLASS_INFO_ALLOCATOR> CLASS_INFO_MAP;
typedef CLASS_INFO_MAP::const_iterator CLASS_INFO_MAP_CONST_ITER;
typedef CLASS_INFO_MAP::iterator CLASS_INFO_MAP_ITER;

typedef mempool_allocator<NAME> NAME_ALLOCATOR;
typedef hash_set<NAME, __gnu_cxx::hash<NAME>, equal_str, NAME_ALLOCATOR> NAME_SET;
typedef NAME_SET::iterator NAME_SET_ITER;

struct VIRFUNC_INFO {
  INT32     _offset;     // offset in vtable
  ST_IDX    _fun_st;     // virtual function symbol idx
  ST_IDX    _vtable_sym; // virtual table symbol idx
  UINT32    _file_idx;   // the file index of the function

  VIRFUNC_INFO(INT32 off, ST_IDX st, ST_IDX vtbl, UINT32 file_idx = INVALID_FILE_IDX)
  {
    _offset     = off;
    _fun_st     = st;
    _vtable_sym = vtbl;
    _file_idx   = file_idx;
  }
  ~VIRFUNC_INFO() {}

  static BOOL   Is_equal(VIRFUNC_INFO *f1, VIRFUNC_INFO *f2);

  BOOL   operator== (const VIRFUNC_INFO &other) const
  {
    if(_offset == other._offset &&
       _fun_st == other._fun_st &&
       _file_idx == other._file_idx) {
      return TRUE;
    }
    return FALSE;
  }

  UINT32 File_idx()                  { return _file_idx; }
  ST_IDX Fun_st()                    { return _fun_st;   }
  INT32  Ofst()                      { return _offset; }
  NAME   Fun_name() const            { return ST_name(_file_idx, _fun_st); }

  void   Print(FILE *fp = TFile);
};

enum CLASS_INFO_KIND {
  CLASS_KIND_NONE = 0x0,
  CXX_CLASS       = 0x1,
  JAVA_CLASS      = 0x2,
  LINK_CLASS      = 0x4,  // fake class info created to connect parent/child
  CXX_LINK_CLASS  = CXX_CLASS  | LINK_CLASS,
  JAVA_LINK_CLASS = JAVA_CLASS | LINK_CLASS,
};
//==============================================================================
//
// CLASS_INFO class is used to store the information of class node in class hierarchy
//
//==============================================================================
class CLASS_INFO {
  protected:
    CLASS_INFO_KIND  _kind;
    UINT32           _file_idx;
    INT32            _max_vtable_ofst;
    MEM_POOL*        _pool;
    C_STR_VEC        _parents;
    CLASS_SET        _children;
    CALL_VEC_MAP     _vtable;
    CALL_VEC_MAP     _cand_calls;
    JAVA_AUX_INFO*   _aux_info;
    C_STR            _class_name;
    TY_IDX           _class_ty;
    // C_STR_VEC     _virtual_bases;         // not used

    CLASS_INFO(void);                           // REQUIRED UNDEFINED UNWANTED methods
    CLASS_INFO(const CLASS_INFO&);              // REQUIRED UNDEFINED UNWANTED methods
    CLASS_INFO& operator = (const CLASS_INFO&); // REQUIRED UNDEFINED UNWANTED methods

  public:
    CLASS_INFO(C_STR name, MEM_POOL *pool, CLASS_INFO_KIND kind = CXX_CLASS, UINT32 file_idx = INVALID_FILE_IDX) :
      _kind(kind),
      _class_name(name),
      _pool(pool),
      _file_idx(file_idx),
      _max_vtable_ofst(-1),
      _parents(C_STR_VEC::allocator_type(pool)),
      _children(4, __gnu_cxx::hash<C_STR>(), equal_str(), CLASS_SET::allocator_type(pool)),
      // _virtual_bases(C_STR_VEC::allocator_type(pool)),
      _vtable(VTABLE_HASH_TABLE_SIZE,
              __gnu_cxx::hash<CALL_OFF>(),
              std::equal_to<CALL_OFF>(),
              CALL_VEC_MAP_ALLOCATOR(pool)),
      _cand_calls(CAND_CALL_HASH_TABLE_SIZE,
                  __gnu_cxx::hash<CALL_OFF>(),
                  std::equal_to<CALL_OFF>(),
                  CALL_VEC_MAP_ALLOCATOR(pool)),
      _aux_info(NULL),
      _class_ty(TY_IDX_ZERO)
    {
      Is_True(!(Is_java_class() & Is_cxx_class()), ("CLASS_INFO: both cxx and java kind set"));
    }

    ~CLASS_INFO()     {}
    BOOL              operator == (const CLASS_INFO &) const;
    void              Clear_call_info(CALL_VEC_MAP *call_map);

    void              Set_class_ty(TY_IDX cls_ty) { _class_ty = cls_ty; }
    BOOL              Has_annot()                 { return _aux_info ? _aux_info->Class_has_annot() : FALSE; }
    void              Add_child(C_STR idx);
    void              Add_parent(C_STR idx);
    // void           Add_virtual_base(C_STR idx) { _virtual_bases.push_back(idx);}
    void              Add_aux_info(JAVA_AUX_INFO *info) { _aux_info = info; }
    void              Add_method(INT32 offset, INT32 call_offset, 
                                 ST_IDX method_sym, ST_IDX vtable_st);
    void              Add_parents(C_STR_VEC *parents);
    void              Add_children(CLASS_SET *children);
    void              Add_vtable_to_candidate(CLASS_INFO *info, 
                                              BOOL force_add = FALSE);
    void              Add_interface_to_candidates(CLASS_INFO *info);
    void              Add_interface_vtable_to_candidates();
    void              Add_candidates(VIRFUNC_INFO_VEC *cands, 
                                     INT32 offset, BOOL force_add);

    UINT32            File_idx()       { return _file_idx; }
    CLASS_INFO_KIND   Kind()           { return _kind; }
    BOOL              Is_java_class()  { return _kind & JAVA_CLASS; }
    BOOL              Is_cxx_class()   { return _kind & CXX_CLASS;  }
    BOOL              Is_link_class()  { return _kind & LINK_CLASS; }
    TY_IDX            Get_class_ty()   { return _class_ty; }
    C_STR_VEC*        Get_parents()    { return &_parents; }
    CLASS_SET*        Get_children()   { return &_children; }
    JAVA_AUX_INFO*    Get_aux_info()   { return _aux_info; }
    C_STR             Get_class_name() { return _class_name; }
    C_STR_VEC*        Get_interfaces() { return _aux_info ? _aux_info->Get_interfaces() : NULL; }
    UINT16            Get_class_acc_flag();
    VIRFUNC_INFO_VEC* Get_cand_calls(int offset);
    VIRFUNC_INFO*     Get_vtable_entry(int offset);
    VIRFUNC_INFO*     Get_vtable_entry(const char *vtbl_name, int vtbl_ofst);
    INT32             Get_vtable_ofst(const char *fname);
    BOOL              Get_meth_annots(const char *fname, vector<STRING> &annot_names);

    // utility functions
    void              Get_meth_by_fname(const char *, vector<JAVA_METH_INFO*> &);
    BOOL              Is_thunk(ST_IDX st_idx);
    BOOL              Is_base_class(C_STR ty);

    void              Print(FILE *fp = TFile);
    void              Print_vtable__offsetmap(FILE *fp = TFile) const;
    void              Print_cand_calls(FILE *fp = TFile) const;
};


//==============================================================================
//
// CLASS_HIERARCHY class chained all CLASS_INFO node together to specify a class
// hierarchy
// CXX_CLASS_HIERARCHY_BUILDER and JAVA_CLASS_HIERARCHY_BUILDER extends this class
// to generate hierarchy for C++ and Java
//==============================================================================
class CLASS_HIERARCHY {
  friend class CAND_CALL_ITER;
protected:
  MEM_POOL *_pool;   // global pool
  MEM_POOL *_lpool;  // local pool
  CLASS_INFO_MAP _class_info_map;

  CLASS_HIERARCHY();                                    // REQUIRED UNDEFINED UNWANTED methods
  CLASS_HIERARCHY(const CLASS_HIERARCHY&);              // REQUIRED UNDEFINED UNWANTED methods
  CLASS_HIERARCHY& operator = (const CLASS_HIERARCHY&); // REQUIRED UNDEFINED UNWANTED methods

public:
  CLASS_HIERARCHY(MEM_POOL *pool, MEM_POOL *loc_pool) : 
    _pool(pool),
    _lpool(loc_pool),
    _class_info_map(CLASS_LIST_HASH_TABLE_SIZE,
                      __gnu_cxx::hash<C_STR>(),
                      equal_str(),
                      CLASS_INFO_ALLOCATOR(loc_pool))
  {
  }
  //CLASS_HIERARCHY(CLASS_HIERARCHY *cha, MEM_POOL *pool);
  ~CLASS_HIERARCHY();

  // To allow mix language link, class hierarchy should be able to mixed link
  // for creation methods, virutal function is allowed as only one kind of class
  // hierarchy for current file. 
  // for visit methods, there should be no virtual functions, as we have no idea
  // what is the language of current class info
  MEM_POOL           *Mem_pool() { return _pool; }
  MEM_POOL           *Loc_pool() { return _lpool; }

  // Creation methods, allow virtual function
  virtual void        Build_class_info();
  // Visit Methods for Building the Hierarchy
  void                Merge(CLASS_HIERARCHY *cha);
  void                Connect_classes();
  void                Build_trav_order(vector<CLASS_INFO *> *seq);

  // Seaching for candidate function(s)
  VIRFUNC_INFO_VEC*   Find_candidate_functions(C_STR idx, INT32 offset, BOOL is_vfunc);
  VIRFUNC_INFO_VEC*   Find_candidate_functions(const char *signature, MEM_POOL *temp_pool);
  void                Find_candidate_functions_in_subclasses(C_STR class_name, const char *sig,
                                                             std::vector<VIRFUNC_INFO *> &meth_vec); // skip finding in java.lang.Object
  INT32               Find_function_offset(const char *signature, MEM_POOL *temp_pool);

  // Accessing VIRFUNC_INFO data
  VIRFUNC_INFO*       Get_interface_entry(C_STR def_class_name,
                                          C_STR if_name, INT32 if_off);
  VIRFUNC_INFO*       Get_vtable_entry(C_STR class_name, INT32 offset);
  VIRFUNC_INFO*       Get_vtable_entry(C_STR class_name, const char *vtbl_name, INT32 vtbl_ofst);
  VIRFUNC_INFO*       Get_meth_by_sig(C_STR name, const char *sig); // this searches the method table for result
  CLASS_INFO*         Get_class_info(C_STR idx);
  C_STR               Get_parent(C_STR);
  C_STR_VEC*          Get_parents(C_STR);
  CLASS_SET*          Get_children(C_STR);
  UINT32              Get_meth_flag(const char *fname);
  UINT16              Get_class_acc_flag(C_STR cls_name);
  JAVA_AUX_INFO*      Get_java_aux_info(const char *fname);
  BOOL                Get_clinit_by_glob(const char *vname, UINT32 &def_file, ST_IDX &def_st);
  BOOL                Is_base_class(C_STR name1, C_STR name2);
  BOOL                Is_candidate_call(const char *fun1, const char *fun2, INT32 offset);
  BOOL                Class_is_interface(C_STR ty);
  void                Print(FILE *fp = TFile) const;

  // static method
  static const char  *Extract_class_name(const char *mangled_name, STRING_BUFFER *buf);
  static const char  *Extract_fun_sig(const char *fun_name, STRING_BUFFER *buf);
  static char        *Demangle_java_sig(char *sig, STRING_BUFFER *buf);
  static char        *Demangle(const char *sym_name);

protected:
  CLASS_INFO         *Add_class(C_STR ty);
  void                Add_parent(C_STR ty, C_STR parent);
  // void             Add_virtual_base(C_STR ty, C_STR parent);
  void                Add_method(TY_IDX ty, INT32 offset, 
                                 CALL_OFF call_offset, ST_IDX method_sym);
  void                Add_child(C_STR ty, C_STR child);
  void                Add_interface(C_STR ty, C_STR interf);
  void                Add_vtable_to_candidate(C_STR ty);
  void                Add_interface_to_candidates(CLASS_INFO *info);

  void                Connect_parent(CLASS_INFO *info, C_STR_VEC *parent);
  void                Connect_children(C_STR idx, CLASS_INFO *info);
  void                Connect_interfaces_parent(C_STR class_name, C_STR_VEC *interfaces);
  void                Connect_interfaces_children(C_STR class_name, CLASS_INFO *info);

  // Internal searching utilities
  VIRFUNC_INFO_VEC*   Find_candidate_functions_in_all_superclasses(INT32 offset);
  BOOL                Find_function_def_class_and_offset(const char *signature, char *class_name, INT32 &ofst);  // this extracts the class name then search the vtable

  BOOL                Is_vtable(char *st_name);
  C_STR               Copy_ty_name(C_STR type_name);
  virtual CLASS_INFO* New_class_info(C_STR name);
};

class CAND_CALL_ITER {
  private:
    VIRFUNC_INFO_VEC *     _cand_calls;
    VIRFUNC_INFO_VEC_ITER  _curr_fun_iter;
    BOOL                   _is_empty;

    CAND_CALL_ITER(void);                               // REQUIRED UNDEFINED UNWANTED methods
    CAND_CALL_ITER(const CAND_CALL_ITER&);              // REQUIRED UNDEFINED UNWANTED methods
    //CAND_CALL_ITER& operator = (const CAND_CALL_ITER&); // REQUIRED UNDEFINED UNWANTED methods
  
  public:
    CAND_CALL_ITER(CLASS_HIERARCHY *ch, C_STR class_ty, CALL_OFF offset, BOOL is_vfunc) {
      CLASS_INFO *info = ch->Get_class_info(class_ty);
      if(info == NULL) {
        _is_empty = TRUE;
        return;
      }
      // if the call is virtual call and class type is an interface, 
      // it can only call "java.lang.Object" virtual function
      if(ch->Class_is_interface(class_ty) && is_vfunc) {
        _cand_calls = ch->Find_candidate_functions_in_all_superclasses(offset);
      } else {
        _cand_calls = info->Get_cand_calls(offset);
      }
      if(_cand_calls == NULL) {
        _is_empty = TRUE;
        return;
      }
      _curr_fun_iter = _cand_calls->begin();
      _is_empty = FALSE;
    }

    void Reset() {
      if(!_is_empty) {
        _curr_fun_iter = _cand_calls->begin();
      }
    }

    void Next()     { ++_curr_fun_iter; }
    BOOL Is_end()   { return (_is_empty || _curr_fun_iter == _cand_calls->end()); }

    ST_IDX    Curr_cand_st()        { return (*_curr_fun_iter)->_fun_st; }
    INT32     Curr_cand_offset()    { return (*_curr_fun_iter)->_offset; }
    ST_IDX    Curr_cand_vtable()    { return (*_curr_fun_iter)->_vtable_sym; }
    UINT32    Curr_file_idx()       { return (*_curr_fun_iter)->_file_idx; }
};

#endif
