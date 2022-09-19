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

#ifndef J_AUX_INFO_H
#define J_AUX_INFO_H

#include "symtab_defs.h"
#include "opt_vsa_util.h"
#include "java_defs.h"

typedef INT32  CALL_OFF;  // offset of call

// [C_STR, CALL_OFF]
// call_offset is the offset of METH table
// if offset > 0, the offset is vtable offset
// if offset < 0, the offset is METH table offset
typedef pair<C_STR, INT32> C_STR_INT32_PAIR;
typedef mempool_allocator<C_STR_INT32_PAIR> C_STR_INT32_ALLOCATOR;
typedef hash_map<C_STR, INT32, __gnu_cxx::hash<C_STR>, equal_str, C_STR_INT32_ALLOCATOR> C_STR_INT32_MAP;
typedef C_STR_INT32_MAP::iterator C_STR_INT32_MAP_ITER;

// typedefs
class JAVA_METH_INFO;

typedef pair<C_STR, JAVA_METH_INFO*> METH_INFO_ENTRY;
typedef mempool_allocator<pair<C_STR, JAVA_METH_INFO*> > NAME2MT_ALLOCATOR;
typedef hash_map<C_STR, JAVA_METH_INFO*, __gnu_cxx::hash<C_STR>, equal_str, NAME2MT_ALLOCATOR> METH_TAB_MAP;
typedef METH_TAB_MAP::iterator METH_TAB_ITER;

class ANNOTATIONS_ATTR;

typedef mempool_allocator<ANNOTATIONS_ATTR *> ANNOS_ATTR_ALLOCATOR;
typedef vector<ANNOTATIONS_ATTR *, ANNOS_ATTR_ALLOCATOR> ANNOS_ATTR_VEC;


class JAVA_METH_INFO {
private:
  MEM_POOL*      _pool;
  INT32          _idx;      // method table index, start from 0
  CALL_OFF       _off;      // vtable offset
  UINT32         _flag;
  UINT32         _st_idx;
  C_STR          _name;
  C_STR          _signature;
  C_STR_VEC      _exceptions;

  JAVA_METH_INFO(void);
  JAVA_METH_INFO(const JAVA_METH_INFO&);
  JAVA_METH_INFO& operator=(const JAVA_METH_INFO&);

public:
  JAVA_METH_INFO(MEM_POOL *pool) : _pool(pool), _idx(-1), _off(0), _flag(0),
                                    _st_idx(ST_IDX_ZERO), _name(NULL), _signature(NULL),
                                    _exceptions(C_STR_VEC::allocator_type(pool)) {}
  ~JAVA_METH_INFO();

  static BOOL Is_equal(METH_INFO_ENTRY entry1, METH_INFO_ENTRY entry2);
  void        Set_idx(INT32 idx)                    { _idx = idx; }
  INT32       Get_idx()                             { return _idx; }
  void        Set_off(CALL_OFF off)                 { _off = off; }
  CALL_OFF    Get_off()                             { return _off; }
  void        Set_flag(UINT32 flag)                 { _flag = flag; }
  UINT32      Get_flag()                            { return _flag; }
  void        Set_name(C_STR name)                  { _name = name; }
  C_STR       Get_name()                            { return _name; }
  UINT32      St_idx()                              { return _st_idx; }
  void        Set_st_idx(UINT32 idx)                { _st_idx = idx; }
  void        Set_signature(C_STR signature)        { _signature = signature; }
  C_STR       Get_signature()                       { return _signature; }
  void        Add_exception(C_STR exception)        { _exceptions.push_back(exception); }
  void        Print(FILE* fp);
};

class JAVA_AUX_INFO {
private:
  MEM_POOL*      _pool;
  INT32          _flags;
  UINT16         _acc_flag;
  C_STR_VEC      _interfaces;
  C_STR_INT32_MAP   _meth_sig_map;
  METH_TAB_MAP   _meth_tab_map;
  ANNOS_ATTR_VEC _annos_attr_vec;

  JAVA_AUX_INFO(void);                              // REQUIRED UNDEFINED UNWANTED METHs
  JAVA_AUX_INFO(const JAVA_AUX_INFO&);              // REQUIRED UNDEFINED UNWANTED METHs
  JAVA_AUX_INFO& operator = (const JAVA_AUX_INFO&); // REQUIRED UNDEFINED UNWANTED METHs
public:
  JAVA_AUX_INFO(MEM_POOL *pool):
    _pool(pool),
    _flags(0),
    _acc_flag(0),
    _interfaces(C_STR_VEC::allocator_type(pool)),
    _meth_sig_map(DEFAULT_HASH_TABLE_SIZE,
                  __gnu_cxx::hash<C_STR>(),
                  equal_str(),
                  C_STR_INT32_ALLOCATOR(pool)),
    _meth_tab_map(DEFAULT_HASH_TABLE_SIZE,
                    __gnu_cxx::hash<C_STR>(),
                    equal_str(),
                    NAME2MT_ALLOCATOR(pool)),
    _annos_attr_vec(ANNOS_ATTR_VEC::allocator_type(pool))
                    {}
  ~JAVA_AUX_INFO();

  BOOL              operator== (const JAVA_AUX_INFO &) const;
  void              Add_interface(C_STR ty_name);
  void              Add_interfaces(C_STR_VEC* interf_vec);
  void              Add_meth_sig(C_STR sig,
                                  CALL_OFF off)   { _meth_sig_map[sig] = off; }
  void              Set_class_has_interface()     { _flags |= HAS_INTERFACE; }
  void              Set_class_is_interface()      { _flags |= IS_INTERFACE;  }
  void              Set_class_has_annot()         { _flags |= HAS_ANNOT; }
  BOOL              Class_has_interface()         { return _flags & HAS_INTERFACE; }
  BOOL              Class_is_interface()          { return _flags & IS_INTERFACE; }
  BOOL              Class_has_annot()             { return _flags & HAS_ANNOT; }

  UINT16            Get_class_acc_flag()          { return _acc_flag; }
  void              Set_class_acc_flag(UINT16 f)  { _acc_flag = f; }
  C_STR_VEC*        Get_interfaces()              { return &_interfaces; }
  C_STR_INT32_MAP  &Get_meth_sig_map()            { return _meth_sig_map; }
  METH_TAB_MAP     &Get_meth_tab_map()            { return _meth_tab_map; }
  void              Add_meth_table(C_STR name, JAVA_METH_INFO *mt_info) {
                                                    _meth_tab_map[name] = mt_info;
                                                  }
  CALL_OFF          Get_meth_off_by_fun_name(const char *name);
  C_STR             Get_meth_sig(CALL_OFF off);
  CALL_OFF          Get_meth_off(C_STR sig);
  UINT32            Get_meth_flag(const char *name);
  ST_IDX            Get_clinit_meth();
  JAVA_METH_INFO*   Get_java_meth_info(const char *name);
  ANNOS_ATTR_VEC*   Get_annos_attr_vec()          { return &_annos_attr_vec; }
  void              Print(FILE *fp=TFile);
};

#endif
