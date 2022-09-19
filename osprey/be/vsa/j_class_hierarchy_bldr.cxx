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

#include  "j_class_hierarchy_bldr.h"
#include <string.h>
#include <assert.h>
#include "symtab.h"
#include "irbdata.h"
#include "cxx_memory.h"
#include "gnu/demangle.h"
#include "opt_defs.h"
#include "config_vsa.h"
#include "targ_const_private.h"
#include "opt_vsa_util.h"
#include <wchar.h>
#include "j_aux_info.h"
#include "vsa_defs.h"

const char *JV_ATTR_TYPE_NAME[] = {
  "JV_CLASS_ATTR",
  "JV_METHOD_ATTR",
  "JV_FIELD_ATTR",
  "JV_DONE_ATTR",
  "JV_CODE_ATTR",
};

const char *JV_ATTR_KIND_NAME[] = {
  "JV_INNER_CLASSES_KIND",
  "JV_ENCLOSING_METHOD_KIND",
  "JV_SIGNATURE_KIND",
  "JV_ANNOTATIONS_KIND",
  "JV_PARAMETER_ANNOTATIONS_KIND",
  "JV_ANNOTATION_DEFAULT_KIND",
  "JV_TYPE_ANNOTATIONS_KIND",
};

const char *JV_CONSTANT_TAG_NAME[] = {
  "JV_CONSTANT_Undefined",
  "JV_CONSTANT_Utf8",
  "JV_CONSTANT_Unicode",
  "JV_CONSTANT_Integer",
  "JV_CONSTANT_Float",
  "JV_CONSTANT_Long",
  "JV_CONSTANT_Double",
  "JV_CONSTANT_Class",
  "JV_CONSTANT_String",
  "JV_CONSTANT_Fieldref",
  "JV_CONSTANT_Methodref",
  "JV_CONSTANT_InterfaceMethodref",
  "JV_CONSTANT_NameAndType",
  "JV_CONSTANT_ResolvedFlag",
  "JV_CONSTANT_LazyFlag",
  "JV_CONSTANT_ResolvedString",
  "JV_CONSTANT_ResolvedClass",
};

JAVA_METH_INFO::~JAVA_METH_INFO()
{
  #if 0
  // memory will be cleaned by pool pop
  MEM_POOL_FREE(_pool, _name);
  MEM_POOL_FREE(_pool, _signature);
  #endif
}

BOOL
JAVA_METH_INFO::Is_equal(METH_INFO_ENTRY entry1, METH_INFO_ENTRY entry2)
{
  if(&entry1 == &entry2) {
    return TRUE;
  }
  if(strcmp(entry1.first, entry2.first)) {
    // key not equal
    return FALSE;
  }
  JAVA_METH_INFO *info1 = entry1.second;
  JAVA_METH_INFO *info2 = entry2.second;
  if(info1 == NULL || info2 == NULL) {
    return FALSE;
  } else if(info1->_off == info2->_off &&
            info1->_flag == info2->_flag &&
            info1->_exceptions.size() == info2->_exceptions.size() &&
            strcmp(info1->_name, info2->_name) == 0 &&
            strcmp(info1->_signature, info2->_signature) == 0 &&
            std::equal(info1->_exceptions.begin(), info1->_exceptions.end(),
                       info2->_exceptions.begin(), equal_str())) {
    return TRUE;
  } else {
    return FALSE;
  }
}

void
JAVA_METH_INFO::Print(FILE* fp)
{
  fprintf(fp, "idx: %d, off:%d, flag:%d, st_idx:%d, name:%s, sig:%s\n",
          _idx, _off, _flag, _st_idx, _name, _signature);
}

JAVA_AUX_INFO::~JAVA_AUX_INFO()
{
  #if 0
  // memory will be cleaned by pool pop
  C_STR_INT32_MAP_ITER iter = _meth_sig_map.begin();
  C_STR_INT32_MAP_ITER itEnd = _meth_sig_map.end();
  for(; iter != itEnd; iter++) {
    MEM_POOL_FREE(_pool, iter->first);
  }
  METH_TAB_ITER mt_iter = _meth_tab_map.begin();
  METH_TAB_ITER mt_iter_end = _meth_tab_map.end();
  for(; mt_iter != mt_iter_end; mt_iter++) {
    MEM_POOL_FREE(_pool, mt_iter->first);
    CXX_DELETE(mt_iter->second, _pool);
  }
  #endif
}

BOOL
JAVA_AUX_INFO::operator== (const JAVA_AUX_INFO &other) const
{
  if(this == &other) {
    return TRUE;
  } else if(_flags == other._flags &&
            _meth_tab_map.size() == other._meth_tab_map.size() &&
            _interfaces.size() == other._interfaces.size() &&
            std::equal(_interfaces.begin(), _interfaces.end(),
                       other._interfaces.begin(), equal_str()) &&
            std::equal(_meth_tab_map.begin(), _meth_tab_map.end(),
                       other._meth_tab_map.begin(),
                       JAVA_METH_INFO::Is_equal)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

void
JAVA_AUX_INFO::Add_interface(C_STR ty_name)
{
  C_STR_VEC_ITER iter = _interfaces.begin();
  C_STR_VEC_ITER iterEnd = _interfaces.end();
  for(; iter != iterEnd; iter++) {
    if(!strcmp(ty_name, *iter)) {
      return;
    }
  }
  _interfaces.push_back(ty_name);
}

void
JAVA_AUX_INFO::Add_interfaces(C_STR_VEC *iterf_vec)
{
  if(iterf_vec) {
    C_STR_VEC_ITER iter = iterf_vec->begin();
    C_STR_VEC_ITER iterEnd = iterf_vec->end();
    for(; iter != iterEnd; iter++) {
      Add_interface(*iter);
    }
  }
}

// =============================================================================
//
// JAVA_AUX_INFO::Get_meth_sig - return method signature by given offset
//
// =============================================================================
C_STR
JAVA_AUX_INFO::Get_meth_sig(CALL_OFF off)
{
  C_STR_INT32_MAP_ITER meth_iter = _meth_sig_map.begin();
  C_STR_INT32_MAP_ITER meth_iter_end = _meth_sig_map.end();
  for(; meth_iter != meth_iter_end; meth_iter++) {
    if(meth_iter->second == off) {
      return meth_iter->first;
    }
  }
  return NULL;
}

// =============================================================================
//
// JAVA_AUX_INFO::Get_meth_off - return offset by given method signature
//
// =============================================================================
CALL_OFF
JAVA_AUX_INFO::Get_meth_off(C_STR sig)
{
  C_STR_INT32_MAP_ITER meth_iter = _meth_sig_map.find(sig);
  if(meth_iter != _meth_sig_map.end()) {
    return meth_iter->second;
  } else {
    return INVALID_VTABLE_OFFSET;
  }
}

CALL_OFF
JAVA_AUX_INFO::Get_meth_off_by_fun_name(const char *name)
{
  METH_TAB_ITER meth_iter = _meth_tab_map.find(name);
  if(meth_iter != _meth_tab_map.end()) {
    return meth_iter->second->Get_off();
  } else {
    return INVALID_VTABLE_OFFSET;
  }
}

UINT32
JAVA_AUX_INFO::Get_meth_flag(const char *name)
{
  METH_TAB_ITER meth_iter = _meth_tab_map.find(name);
  if(meth_iter != _meth_tab_map.end()) {
    return meth_iter->second->Get_flag();
  } else {
    return ACC_INVALID;
  }
}

// =============================================================================
//
// JAVA_AUX_INFO::Get_clinit_meth - return method st with name "<clinit>"
// return 0 if not found
//
// =============================================================================
ST_IDX
JAVA_AUX_INFO::Get_clinit_meth()
{
  METH_TAB_ITER meth_iter = _meth_tab_map.begin();
  METH_TAB_ITER meth_iter_end = _meth_tab_map.end();
  for(; meth_iter != meth_iter_end; meth_iter++) {
    if(strcmp(meth_iter->second->Get_name(), "<clinit>") == 0) {
      return meth_iter->second->St_idx();
    }
  }
  return 0;
}

// =============================================================================
//
// JAVA_AUX_INFO::Get_meth_info - Get java method info
//
// =============================================================================
JAVA_METH_INFO*
JAVA_AUX_INFO::Get_java_meth_info(const char *name)
{
  if (_meth_tab_map.find(name) != _meth_tab_map.end()) {
    return _meth_tab_map[name];
  }
  return NULL;
}

// =============================================================================
//
// JAVA_AUX_INFO::Print - Display method
//
// =============================================================================
void
JAVA_AUX_INFO::Print(FILE *fp)
{
  fprintf(fp, "\n acc flags: %d\n", _acc_flag);
  fprintf(fp, "\n interfaces: %ld\n", _interfaces.size());
  C_STR_VEC_ITER interf_iter;
  for(interf_iter = _interfaces.begin(); interf_iter != _interfaces.end(); interf_iter++) {
    fprintf(fp, "    %s\n", *interf_iter);
  }
  fprintf(fp, "\n meth table: %ld\n", _meth_sig_map.size());
  C_STR_INT32_MAP_ITER meth_iter = _meth_sig_map.begin();
  C_STR_INT32_MAP_ITER meth_iter_end = _meth_sig_map.end();
  for(; meth_iter != meth_iter_end; meth_iter++) {
    fprintf(fp,"  Offset:%d Sig:%s\n", meth_iter->second, meth_iter->first); //cplus_demangle(meth_iter->first, DMGL_PARAMS|DMGL_ANSI|DMGL_TYPES|DMGL_JAVA));
  }

  fprintf(fp, "\n meth table 2: %ld\n", _meth_tab_map.size());
  METH_TAB_ITER meth_tab_iter = _meth_tab_map.begin();
  METH_TAB_ITER meth_tab_iter_end = _meth_tab_map.end();
  for(; meth_tab_iter != meth_tab_iter_end; meth_tab_iter++) {
    fprintf(fp, "name:%s", meth_tab_iter->first);
    meth_tab_iter->second->Print(fp);
  }

  if (_annos_attr_vec.size() > 0) {
    fprintf(fp, "\nAll annotations:\n");
    ANNOS_ATTR_VEC::iterator anno_iter;
    for (anno_iter = _annos_attr_vec.begin(); anno_iter != _annos_attr_vec.end(); ++anno_iter) {
      (*anno_iter)->Print(fp);
    }
  }
}

void
TYPE_NODE::Set_parent(TYPE_NODE *parent)
{
  TN_VEC *children = parent->Get_children();
  if (children == NULL) {
    children = CXX_NEW(
      TN_VEC(TN_ALLOCATOR(_pool)), _pool
    );
    parent->Set_children(children);
  }
  children->push_back(this);
  this->_parent = parent;
}

void
TYPE_NODE::Add_interface(TYPE_NODE *interface)
{
  if(_interfaces == NULL) {
    _interfaces = CXX_NEW(TN_VEC(TN_ALLOCATOR(_pool)), _pool);
  }
  _interfaces->push_back(interface);
}

void
JAVA_CLASS_HIERARCHY_BUILDER::Find_related_class(TYPE_NODE *node, hash_set<ST_IDX> &related_class_sym)
{
  // already added
  if (related_class_sym.find(node->Get_st_idx()) != related_class_sym.end())
    return;
  // add node
  related_class_sym.insert(node->Get_st_idx());
  // add parent
  TYPE_NODE *p_node = node;
  while((p_node = p_node->Get_parent()) != NULL) {
    related_class_sym.insert(p_node->Get_st_idx());
    Is_Trace(Get_Trace(TP_VSA, VSA_CHA_TRACE_FLAG),
             (TFile, "rt.o: add parent class %s: %s\n",
                      ST_name(node->Get_st_idx()),
                      ST_name(p_node->Get_st_idx())));
  }
  // add children
  if(node->Get_children()) {
    vector<TYPE_NODE *> node_stack;
    node_stack.push_back(node);
    while (!node_stack.empty()) {
      TYPE_NODE *tmp_node = node_stack.back();
      node_stack.pop_back();
      for (TN_VEC_ITER iter = tmp_node->Get_children()->begin(); iter != tmp_node->Get_children()->end(); ++iter) {
        related_class_sym.insert((*iter)->Get_st_idx());

        Is_Trace(Get_Trace(TP_VSA, VSA_CHA_TRACE_FLAG),
                (TFile, "rt.o: add child class %s: %s\n",
                        ST_name(node->Get_st_idx()),
                        ST_name((*iter)->Get_st_idx())));
        if ((*iter)->Get_children() != NULL) {
          node_stack.push_back(*iter);
        }
      }
    }
  }
  // add interfaces
  TN_VEC *itfs = node->Get_interfaces();
  if(itfs) {
    for (TN_VEC_ITER iter =itfs->begin(); iter != itfs->end(); ++iter) {
      related_class_sym.insert((*iter)->Get_st_idx());

      Is_Trace(Get_Trace(TP_VSA, VSA_CHA_TRACE_FLAG),
               (TFile, "rt.o: add interface class %s: %s\n",
                       ST_name(node->Get_st_idx()),
                       ST_name((*iter)->Get_st_idx())));
    }
  }
}

// =============================================================================
//
// Build_class_from_vtable_file: selective read class info from vtable whirl file
// (only contains INITOs) to connect user rna with rbc rule dna
// Algorithm:
// [1] iterate Global symbol table, add rbc class, build TYPE TREE for each class
// [2] iterate each rbc classes, add related classes from TYPE TREE
// The related class includes:
// 1) parent classes
// 2) child classes
// 3) interfaces base class
// TODO: for rbc interface base class, add all implemented classes and extends
//       interfaces, so that rule added on base class can be applied to its children
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Build_class_from_vtable_file()
{
  NAME_SET* rbc_class_name_set = JAVA_CLASS_HIERARCHY_HELPER::Get_class_sym_set();
  hash_map<ST_IDX, INITO_IDX> class_sym_inito_map;
  hash_map<ST_IDX, INITO_IDX> vtable_sym_inito_map;
  vector<ST_IDX> rbc_hint_class;
  ST_TN_MAP node_map(DEFAULT_HASH_TABLE_SIZE, __gnu_cxx::hash<ST_IDX>(),
    __gnu_cxx::equal_to<ST_IDX>(), TN_ALLOCATOR(_lpool));
  ST_IDX root_class;
  BOOL find_root = FALSE;
  for (UINT i = 1; i < INITO_Table_Size(GLOBAL_SYMTAB); ++i) {
    INITO* entry = &Inito_Table(GLOBAL_SYMTAB, i);
    ST *st_entry = &St_Table[entry->st_idx];
    if (ST_is_class_symbol(st_entry)) {
      class_sym_inito_map[entry->st_idx] = i;
      if (rbc_class_name_set->find(ST_name(st_entry)) != rbc_class_name_set->end()) {
        rbc_hint_class.push_back(ST_st_idx(st_entry));
        Is_Trace(Get_Trace(TP_VSA, VSA_CHA_TRACE_FLAG), (TFile, "rt.o: add class %s\n", ST_name(st_entry)));
      }
      // build type tree
      TYPE_NODE *node;
      if (node_map.find(entry->st_idx) != node_map.end()) {
        node = node_map[entry->st_idx];
      } else {
        node = CXX_NEW(TYPE_NODE(entry->st_idx, _lpool), _lpool);
        node_map[entry->st_idx] = node;
      }
      INITV_ENTRIES symbol_entries;
      Get_initv_entry(entry->val, &symbol_entries);
      Is_True_Ret(symbol_entries.Size() == ENTRY_KIND_MAX,
                  ("class symbol inito incomplete"));
      // read super class
      if(symbol_entries.Get_kind(SUPER_CLASS_SYM) == INITVKIND_SYMOFF) {
        ST_IDX parent_st_idx = symbol_entries.Get_initv_st(SUPER_CLASS_SYM);
        TYPE_NODE *parent = NULL;
        if (node_map.find(parent_st_idx) != node_map.end()) {
          parent = node_map[parent_st_idx];
        } else {
          parent = CXX_NEW(TYPE_NODE(parent_st_idx, _lpool), _lpool);
          node_map[parent_st_idx] = parent;
        }
        node->Set_parent(parent);
      } else {
        Is_True_Ret(find_root == FALSE, ("Multi root object class found."));
        Is_True_Ret(strcmp(ST_name(entry->st_idx), "_ZN4java4lang6Object6class$E") == 0,
          ("Root class is not Object, class name : %s\n", ST_name(entry->st_idx)));
        find_root = TRUE;
        root_class = entry->st_idx;
      }
      // read interface table
      if(symbol_entries.Get_initv_data(INTERFACE_CNT) > 0) {
        INITV_ENTRIES intf_entries;
        ST_IDX intf_tab_st = symbol_entries.Get_initv_st(INTERFACE_TABLE_SYM, TRUE);
        Get_initv_entry(Get_initv_idx_by_st_idx(intf_tab_st), &intf_entries);

        for(int i = 0; i < intf_entries.Size(); i++) {
          ST_IDX inf_st_idx = intf_entries.Get_initv_st(i, TRUE);
          if(inf_st_idx == ST_IDX_ZERO) {
            continue;
          }
          TYPE_NODE *inf_node = NULL;
          ST_TN_MAP_ITER iter = node_map.find(inf_st_idx);
          if(iter != node_map.end()) {
            inf_node = iter->second;
          } else {
            inf_node = CXX_NEW(TYPE_NODE(inf_st_idx, _lpool), _lpool);
            node_map[inf_st_idx] = inf_node;
          }
          node->Add_interface(inf_node);
        }
      }
    }
    else if(ST_is_vtable(st_entry))
      vtable_sym_inito_map[entry->st_idx] = i;
  }
  hash_set<ST_IDX> related_class_sym;
  for (vector<ST_IDX>::iterator iter = rbc_hint_class.begin(); iter != rbc_hint_class.end(); ++iter) {
    // INITO* entry = &Inito_Table(GLOBAL_SYMTAB, class_sym_inito_map(*iter));
    // ST *st_entry = &St_Table[entry->st_idx];
    TYPE_NODE *node = node_map[*iter];
    Find_related_class(node, related_class_sym);
  }
  // read related class symbol first
  for (hash_set<ST_IDX>::iterator iter = related_class_sym.begin(); iter != related_class_sym.end(); ++iter) {
    hash_map<ST_IDX, INITO_IDX>::iterator inito_it = class_sym_inito_map.find(*iter);
    if(inito_it != class_sym_inito_map.end()) {
      INITO *entry = &Inito_Table(GLOBAL_SYMTAB, inito_it->second);
      Read_class_symbol(entry);
    }
  }
  // read all vtable for now
  for (UINT i = 1; i < INITO_Table_Size(GLOBAL_SYMTAB); ++i) {
    INITO* entry = &Inito_Table(GLOBAL_SYMTAB, i);
    ST *st_entry = &St_Table[entry->st_idx];
    if(ST_is_vtable(st_entry)) {
      Read_vtable(entry, TRUE);
    }
  }
}

void
JAVA_CLASS_HIERARCHY_BUILDER::Build_tmp_st_inito_cache()
{
  for (UINT i = 1; i < INITO_Table_Size(GLOBAL_SYMTAB); ++i) {
    INITO* entry = &Inito_Table(GLOBAL_SYMTAB, i);
    _st_inito_cache[entry->st_idx] = entry->val;
  }
}

INITV_IDX
JAVA_CLASS_HIERARCHY_BUILDER::Get_initv_idx_by_st_idx(ST_IDX st_idx)
{
  Is_True_Ret(_st_inito_cache.find(st_idx) != _st_inito_cache.end(),
    ("Can't find st in cache, ST_name : %s\n", ST_name(st_idx)), INITV_IDX_ZERO);
  return _st_inito_cache[st_idx];
}

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Build_class_info - Entry function to build
// java class info
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Build_class_info()
{
  bool is_vtable_file = FILE_INFO_is_vtable(File_info);
  if (is_vtable_file && !VSA_Load_All_Sym) {
    NAME_SET* rbc_class_sym_set = JAVA_CLASS_HIERARCHY_HELPER::Get_class_sym_set();
    NAME_SET* rbc_vtable_sym_set = JAVA_CLASS_HIERARCHY_HELPER::Get_vtable_sym_set();
    Is_True_Ret(rbc_class_sym_set != NULL,
      ("java chb rbc_class_sym_set is NULL."));
    Is_True_Ret(rbc_vtable_sym_set != NULL,
      ("java chb rbc_class_sym_set is NULL."));
    Build_class_from_vtable_file();
  } else {
    vector<UINT> vtable_idx_vector;
    for (UINT i = 1; i < INITO_Table_Size(GLOBAL_SYMTAB); ++i) {
      INITO* entry = &Inito_Table(GLOBAL_SYMTAB, i);
      ST *st_entry = &St_Table[entry->st_idx];
      if(ST_is_class_symbol(st_entry)) {
        Read_class_symbol(entry);
      } else if (ST_is_vtable(st_entry)) {
        vtable_idx_vector.push_back(i);
      }
    }
    for (vector<UINT>::iterator iter = vtable_idx_vector.begin(); iter != vtable_idx_vector.end(); ++iter) {
      INITO* entry = &Inito_Table(GLOBAL_SYMTAB, *iter);
      Read_vtable(entry);
    }
  }
}

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Read_class_symbol - read inito entry of class
// symbol, add parent, interface, method info to class_info
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Read_class_symbol(INITO *entry)
{
  INITV_ENTRIES symbol_entries;
  Get_initv_entry(entry->val, &symbol_entries);
  Is_True_Ret(symbol_entries.Size() == ENTRY_KIND_MAX, ("class symbol inito incomplete"));

  TY_IDX ty_idx = ST_vtable_ty_idx(entry->st_idx);
  C_STR class_ty = TY_name(ty_idx);
  // add this
  CLASS_INFO *info = Add_class(Copy_ty_name(class_ty));
  Is_True_Ret(info, ("Failed create Class info\n"));
  info->Set_class_ty(ty_idx);
  // read super calss
  if(symbol_entries.Get_kind(SUPER_CLASS_SYM) == INITVKIND_SYMOFF) {
    ST_IDX super_st = symbol_entries.Get_initv_st(SUPER_CLASS_SYM);
    Is_True(super_st != ST_IDX_ZERO, ("no super class st"));
    if(super_st != ST_IDX_ZERO) {
      C_STR super_ty = TY_name(ST_vtable_ty_idx(super_st));
      Add_parent(Copy_ty_name(class_ty), Copy_ty_name(super_ty));
    }
  }
  UINT16 acc_flag = (UINT16) symbol_entries.Get_initv_data(CLASS_ACC_FLAG);
  if(acc_flag != 0) {
    Set_class_acc_flag(info, acc_flag);
  }

  // read interface table
  if(symbol_entries.Get_initv_data(INTERFACE_CNT) > 0) {
    Set_class_has_interface(class_ty);
    Read_interface_table(class_ty, symbol_entries.Get_initv_st(INTERFACE_TABLE_SYM, TRUE));
  }

  if(symbol_entries.Get_initv_data(VTABLE_METH_CNT) == 0) {
    Set_class_is_interface(class_ty);
  }

  // read meth table
  if(symbol_entries.Get_initv_data(METH_CNT) > 0) {
    Read_meth_table(class_ty, symbol_entries.Get_initv_st(METH_TABLE_SYM, TRUE));
  }

  // this option control the java annotation handler
  if (VSA_Enable_Java_Anno && symbol_entries.Get_initv_data(REFLECTION_DATA) != 0) {
    JAVA_AUX_INFO *aux_info = info->Get_aux_info();
    Is_True_Ret(aux_info != NULL, ("Java aux info is NULL, class info: %s", info->Get_class_name()));
    aux_info->Set_class_has_annot();
    INT32 constant_size = symbol_entries.Get_initv_i32(CONSTANT_SIZE, TRUE);
    INITV_ENTRIES constant_data;
    ST_IDX data_st = symbol_entries.Get_initv_st(CONSTANT_DATA, TRUE);
    Get_initv_entry(Get_initv_idx_by_st_idx(data_st), &constant_data);
    Is_True_Ret(constant_data.Size() == constant_size,
      ("The size of const_data is not equal with constant_size, const data length: %d, const_size: %d",
        constant_data.Size(), constant_size));
    INITV_ENTRIES constant_tags;
    ST_IDX tag_st = symbol_entries.Get_initv_st(CONSTANT_TAGS, TRUE);
    Get_initv_entry(Get_initv_idx_by_st_idx(tag_st), &constant_tags);
    ST_IDX reflection_init_st = symbol_entries.Get_initv_st(REFLECTION_DATA, TRUE);
    Read_annotations_attr(class_ty, reflection_init_st, constant_data, constant_tags);
  }
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Read_vtable - read vtable symbol inito entry
// set the vtable info, add vtable to candidates
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Read_vtable(INITO *entry, BOOL filt)
{
  TY_IDX class_ty = ST_vtable_ty_idx(entry->st_idx);
  if (filt) {
    char *class_name = TY_name(class_ty);
    // if filt, class name already added to list, just add related vtable
    if (!Get_class_info(class_name)) {
      return;
    }
  }
  INITV_ENTRIES vtable_entries;
  Get_initv_entry(entry->val, &vtable_entries);
  Is_True_Ret(vtable_entries.Size() >= VTABLE_FIRST_METH, ("vtable inito incomplete"));
  for (int i = VTABLE_FIRST_METH; i < vtable_entries.Size(); i++) {
    if(vtable_entries.Get_kind(i) == INITVKIND_SYMOFF) {
      ST_IDX meth_st = vtable_entries.Get_initv_st(i);
      CALL_OFF call_offset = (i - CLASS_SYM) * Pointer_Size;
      Add_method(class_ty, i * Pointer_Size, call_offset, meth_st);
    }
  }
  Add_vtable_to_candidate(TY_name(class_ty));
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Read_interface_table - read interface symbol
// inito entry, add interface info
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Read_interface_table(C_STR class_ty, ST_IDX intf_st)
{
  INITV_ENTRIES intf_entries;
  Get_initv_entry(Get_initv_idx_by_st_idx(intf_st), &intf_entries);

  for(int i = 0; i < intf_entries.Size(); i++) {
    ST_IDX st_idx = intf_entries.Get_initv_st(i, TRUE);
    if(st_idx != ST_IDX_ZERO) {
      C_STR intf_ty = TY_name(ST_vtable_ty_idx(st_idx));
      Add_interface(Copy_ty_name(class_ty), Copy_ty_name(intf_ty));
    }
  }
}

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Read_meth_table - Read method table,
// Create method sig and method table
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Read_meth_table(C_STR class_ty, ST_IDX meth_st_idx)
{
  INITV_ENTRIES meth_entries;
  Get_initv_entry(Get_initv_idx_by_st_idx(meth_st_idx), &meth_entries);
  int meth_idx = 1;
  for(int i = 0; i < meth_entries.Size(); i+=6 ) {
    ST_IDX meth_name_st = meth_entries.Get_initv_st(i + METH_NAME);
    ST_IDX meth_sig_st  = meth_entries.Get_initv_st(i + METH_SIGNATURE);

    INITV_ENTRIES meth_name_entries;
    Get_initv_entry(Get_initv_idx_by_st_idx(meth_name_st), &meth_name_entries);
    Is_True_Ret(meth_name_entries.Size() == STRING_END, ("Method name initv corrupt"));
    STR_IDX name_idx = meth_name_entries.Get_initv_str_idx(STRING_IDX, TRUE);

    INITV_ENTRIES meth_sig_entries;
    Get_initv_entry(Get_initv_idx_by_st_idx(meth_sig_st), &meth_sig_entries);
    Is_True_Ret(meth_sig_entries.Size() == STRING_END, ("Method sig initv corrupt"));
    STR_IDX sig_idx = meth_sig_entries.Get_initv_str_idx(STRING_IDX, TRUE);

    C_STR sig = Create_meth_sig(name_idx, sig_idx);
    CALL_OFF off = (CALL_OFF) meth_entries.Get_initv_data(i + METH_OFF);
    if(off == -1) {
      off = -1 * meth_idx;
    }
    Add_meth_sig(class_ty, sig, off);
    JAVA_METH_INFO* mt_info = New_meth_table();
    mt_info->Set_idx(meth_idx - 1);
    mt_info->Set_name(Copy_ty_name(Index_to_char_array(name_idx)));
    mt_info->Set_signature(Copy_ty_name(Index_to_char_array(sig_idx)));
    mt_info->Set_off(off);
    mt_info->Set_flag(meth_entries.Get_initv_u32(i + METH_FLAG));
    ST_IDX meth_sym_idx = meth_entries.Get_initv_st(i + METH_SYM);
    mt_info->Set_st_idx(meth_sym_idx);
    // interface meth or abstract meth idx is 0
    if (meth_sym_idx != 0) {
      const char *mangle_name = Copy_ty_name(ST_name(&St_Table[meth_sym_idx]));
      CLASS_INFO *info = Get_class_info(class_ty);
      if(info && info->Get_aux_info()) {
        info->Get_aux_info()->Add_meth_table(mangle_name, mt_info);
        // interfaces method default implementation, add to vtable
        if(info->Get_aux_info()->Class_is_interface()) {
          info->Add_method(0, off, meth_sym_idx, 0);
        }
      }
    }
    meth_idx++;
  }
  // interfaces's abstract method have no implementation, there is requirement
  // to add rules to those method, which requires interface call can be resolved
  // through class hierarchy. The solution is:
  // 1. generate function symbol for the abstract method in FE
  // 2. vtable entry of the interface method will be added(impl in above loop)
  // 3. these interface method should add to the candidates
  CLASS_INFO *info = Get_class_info(class_ty);
  if(info && info->Get_aux_info() && info->Get_aux_info()->Class_is_interface()) {
    info->Add_interface_vtable_to_candidates();
  }
}

// this table describe how to read element const value
// how to store and how to check the const tag
CONST_VALUE_DESC ELEMENT_VALUE::const_value_desc[9] = {
  {'B',     MTYPE_I1,     JV_CONSTANT_Integer },
  {'C',     MTYPE_U2,     JV_CONSTANT_Integer },
  {'D',     MTYPE_I8,     JV_CONSTANT_Double  },
  {'F',     MTYPE_I4,     JV_CONSTANT_Float   },
  {'I',     MTYPE_I4,     JV_CONSTANT_Integer },
  {'J',     MTYPE_I8,     JV_CONSTANT_Long    },
  {'S',     MTYPE_I2,     JV_CONSTANT_Integer },
  {'Z',     MTYPE_U1,     JV_CONSTANT_Integer },
  {'s',     MTYPE_STR,    JV_CONSTANT_Utf8    },
};

void
ELEMENT_VALUE::Write_to_real_value(TYPE_ID mtype, INT64 value,
  UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  if (mtype == MTYPE_I1) {
    real_value.const_value.byte_value = (INT8) value;
  } else if (mtype == MTYPE_I2) {
    real_value.const_value.short_value = (INT16) value;
  } else if (mtype == MTYPE_I4) {
    real_value.const_value.integer_value = (INT32) value;
  } else if (mtype == MTYPE_I8) {
    real_value.const_value.long_value = (INT64) value;
  } else if (mtype == MTYPE_U1) {
    real_value.const_value.boolean_value = (UINT8) value;
  } else if (mtype == MTYPE_U2) {
      real_value.const_value.character_value = (UINT16) value;
  } else if (mtype == MTYPE_STR) {
    real_value.const_value.string_value = Get_utf_string_from_inito(st_inito_cache, value, pool);
  } else {
    Is_True_Ret(FALSE, ("Unsupported mtype, mtype: %s", MTYPE_name(mtype)));
  }
}

ELEMENT_VALUE::ELEMENT_VALUE(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  UINT8 tag = Read_UINT8(buf);
  this->tag = tag;
  BOOL match = FALSE;
  for (INT i = 0; i < sizeof(const_value_desc) / sizeof(CONST_VALUE_DESC); i++) {
    CONST_VALUE_DESC desc = const_value_desc[i];
    if ((char) tag != desc.tag) {
      continue;
    }
    UINT16 const_value_index = Read_UINT16(buf);
    value.const_value_index = const_value_index;
    INT constant_tag = constant_tags.Get_initv_i32(const_value_index);
    match = TRUE;
    TYPE_ID mtype = desc.mtype;
    Write_to_real_value(mtype, constant_data.Get_initv_data(const_value_index), st_inito_cache, pool);
    Is_True_Ret(constant_tag == desc.constant_tag,
      ("Constant tag mismatch, expected: %s, real tag: %s",
        Jv_constant_tag_name((JV_CONSTANT_TAG) desc.constant_tag), Jv_constant_tag_name((JV_CONSTANT_TAG) constant_tag)));
    break;
  }
  if (!match) {
    if ((char) tag == 'e') {
      UINT16 type_name_index = Read_UINT16(buf);
      value.enum_const_value.type_name_index = type_name_index;
      UINT16 const_name_index = Read_UINT16(buf);
      value.enum_const_value.const_name_index = const_name_index;
      ST_IDX type_name_st = constant_data.Get_initv_st(type_name_index, TRUE);
      real_value.enum_const_value.type_name = Get_utf_string_from_inito(st_inito_cache, type_name_st, pool);

      ST_IDX const_name_st = constant_data.Get_initv_st(const_name_index, TRUE);
      real_value.enum_const_value.const_name = Get_utf_string_from_inito(st_inito_cache, const_name_st, pool);
    } else if ((char) tag == 'c') {
      UINT16 class_info_index = Read_UINT16(buf);
      value.class_info_index = class_info_index;
      ST_IDX class_info_st = constant_data.Get_initv_st(class_info_index, TRUE);
      real_value.class_info_value = Get_utf_string_from_inito(st_inito_cache, class_info_st, pool);
    } else if ((char) tag == '@') {
      value.annotation_value = CXX_NEW(ANNOTATION(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
      real_value.annotation_value = value.annotation_value;
    } else if ((char) tag == '[') {
      UINT16 num_elements = Read_UINT16(buf);
      value.array_value.num_elements = num_elements;
      ELEMENT_VALUE **elements = NULL;
      if (num_elements > 0) {
        elements = (ELEMENT_VALUE **) MEM_POOL_Alloc(pool, num_elements * sizeof(ELEMENT_VALUE *));
        for (int i = 0; i < num_elements; i++) {
          elements[i] = CXX_NEW(ELEMENT_VALUE(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
        }
      }
      value.array_value.elements = elements;
      real_value.array_value.num_elements = num_elements;
      real_value.array_value.elements = elements;
    } else {
      Is_True_Ret(FALSE, ("Not supported ELEMENT_VALUE tag, tag: %d", tag));
    }
  }
}

void
ELEMENT_VALUE::Print(FILE *fp)
{
  switch ((char) tag) {
  case 'B': {
    fprintf(fp, "(byte)%d", (INT32) real_value.const_value.byte_value);
    break;
  }
  case 'C': {
    wchar_t wc_str[2];
    wc_str[0] = (wchar_t) real_value.const_value.character_value;
    wc_str[1] = L'\0';
    char c_str[16];
    wcstombs(c_str, wc_str, sizeof(c_str));
    fprintf(fp, "'%s'", c_str);
    break;
  }
  case 'D': {
    fprintf(fp, "(double)%f", real_value.const_value.double_value);
    break;
  }
  case 'F': {
    fprintf(fp, "(float)%f", real_value.const_value.float_value);
    break;
  }
  case 'I': {
    fprintf(fp, "(integer)%d", real_value.const_value.integer_value);
    break;
  }
  case 'J': {
    fprintf(fp, "(long)%lld", real_value.const_value.long_value);
    break;
  }
  case 'S': {
    fprintf(fp, "(short)%d", real_value.const_value.short_value);
    break;
  }
  case 'Z': {
    fprintf(fp, "(boolean)%u", real_value.const_value.boolean_value);
    break;
  }
  case 's': {
    fprintf(fp, "\"%s\"", real_value.const_value.string_value);
    break;
  }
  case 'e': {
    fprintf(fp, "<\"%s\":\"%s\">", real_value.enum_const_value.type_name, real_value.enum_const_value.const_name);
    break;
  }
  case 'c': {
    fprintf(fp, "(class info)%s", real_value.class_info_value);
    break;
  }
  case '@': {
    real_value.annotation_value->Print(fp);
    break;
  }
  case '[': {
    INT num_elements = real_value.array_value.num_elements;
    fprintf(fp, "[");
    for (INT i = 0; i < num_elements; i++) {
      if (i != 0) {
        fprintf(fp, ", ");
      }
      real_value.array_value.elements[i]->Print(fp);
    }
    fprintf(fp, "]");
    break;
  }
  default: {
    Is_True(FALSE, ("Wrong ELEMENT_VALUE tag, tag: %c", (char) tag));
  }
  }
}

ELEMENT_VALUE_PAIR::ELEMENT_VALUE_PAIR(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  UINT16 element_name_index = Read_UINT16(buf);
  this->element_name_index = element_name_index;
  this->element_value = CXX_NEW(ELEMENT_VALUE(buf, constant_data, constant_tags, st_inito_cache, pool), pool);

  ST_IDX element_name_st = constant_data.Get_initv_st(element_name_index, TRUE);
  this->element_name = Get_utf_string_from_inito(st_inito_cache, element_name_st, pool);
}

void
ELEMENT_VALUE_PAIR::Print(FILE *fp)
{
  fprintf(fp, "%s=", element_name);
  element_value->Print(fp);
}

ANNOTATION::ANNOTATION(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  UINT16 type_index = Read_UINT16(buf);
  this->type_index = type_index;
  UINT16 num_element_value_pairs = Read_UINT16(buf);
  this->num_element_value_pairs = num_element_value_pairs;
  ELEMENT_VALUE_PAIR **value_pairs = NULL;
  if (num_element_value_pairs > 0) {
    value_pairs = (ELEMENT_VALUE_PAIR **) MEM_POOL_Alloc(pool, num_element_value_pairs * sizeof(ELEMENT_VALUE_PAIR *));
    for (int i = 0; i < num_element_value_pairs; i++) {
      value_pairs[i] = CXX_NEW(ELEMENT_VALUE_PAIR(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
    }
  }
  this->element_value_pairs = value_pairs;

  ST_IDX type_name_st = constant_data.Get_initv_st(type_index, TRUE);
  Is_True_Ret(type_name_st != ST_IDX_ZERO, ("ANNOTATION type name st invalid"));
  char *type_name = Get_utf_string_from_inito(st_inito_cache, type_name_st, pool);
  Is_True_Ret(type_name, ("Annotation: null type_name"));
  UINT32 len = strlen(type_name);
  STRING_BUFFER demangled_buf(type_name, len + 1);
  this->type_name = CLASS_HIERARCHY::Demangle_java_sig(type_name, &demangled_buf);
}

void
ANNOTATION::Print(FILE *fp)
{
  fprintf(fp, "%s{", type_name);
  for (INT i = 0; i < num_element_value_pairs; i++) {
    if (i != 0) {
      fprintf(fp, ", ");
    }
    element_value_pairs[i]->Print(fp);
  }
  fprintf(fp, "}");
}

RUNTIME_VISIABLE_ANNOTATIONS::RUNTIME_VISIABLE_ANNOTATIONS(char *&buf, UINT8 type, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  if (type != JV_CLASS_ATTR) {
    UINT16 index = Read_UINT16(buf);
    this->member_index = index;
  } else {
    this->member_index = 0;
  }
  UINT8 num_annotations = Read_UINT16(buf);
  this->num_annotations = num_annotations;
  ANNOTATION **annotations = NULL;
  if (num_annotations > 0) {
    annotations = (ANNOTATION **) MEM_POOL_Alloc(pool, num_annotations * sizeof(ANNOTATION *));
    for (int i = 0; i< num_annotations; i++) {
      annotations[i] = CXX_NEW(ANNOTATION(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
    }
  }
  this->annotations = annotations;
}

void
RUNTIME_VISIABLE_ANNOTATIONS::Print(FILE *fp)
{
  for (INT i = 0; i < num_annotations; i++) {
    fprintf(fp, "  ");
    annotations[i]->Print(fp);
    fprintf(fp, "\n");
  }
  fprintf(fp, "\n");
}

PARAMETER_ANNOTATION::PARAMETER_ANNOTATION(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  UINT16 num_annotations = Read_UINT16(buf);
  this->num_annotations = num_annotations;
  ANNOTATION **annotations = NULL;
  if (num_annotations > 0) {
    annotations = (ANNOTATION **) MEM_POOL_Alloc(pool, num_annotations * sizeof(ANNOTATION *));
    for (int i = 0; i < num_annotations; i++) {
      annotations[i] = CXX_NEW(ANNOTATION(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
    }
  }
  this->annotations = annotations;
}

void
PARAMETER_ANNOTATION::Print(FILE *fp)
{
  for (INT i = 0; i < num_annotations; i++) {
    fprintf(fp, "    ");
    annotations[i]->Print(fp);
    fprintf(fp, "\n");
  }
  if (num_annotations == 0) {
    fprintf(fp, "    No annotations.\n");
  }
  fprintf(fp, "\n");
}

RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS::RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS(char *&buf, UINT8 type, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  if (type != JV_CLASS_ATTR) {
    UINT16 index = Read_UINT16(buf);
    this->member_index = index;
  } else {
    this->member_index = 0;
  }
  UINT8 num_parameters = Read_UINT8(buf);
  this->num_parameters = num_parameters;
  PARAM_ANNO **param_annos = NULL;
  if (num_parameters > 0) {
    param_annos = (PARAM_ANNO **) MEM_POOL_Alloc(pool, num_parameters * sizeof(PARAM_ANNO *));
    for (int i = 0; i < num_parameters; i++) {
      param_annos[i] = CXX_NEW(PARAM_ANNO(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
    }
  }
  this->parameter_annotations = param_annos;
}

void
RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS::Print(FILE *fp)
{
  for (INT i = 0; i < num_parameters; i++) {
    fprintf(fp, "  parameter%d:\n", i + 1);
    parameter_annotations[i]->Print(fp);
  }
}

ANNO_LOCAL_TYPE::ANNO_LOCAL_TYPE(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  this->start_pc = Read_UINT16(buf);
  this->length = Read_UINT16(buf);
  this->index = Read_UINT16(buf);
}

TYPE_PATH_INFO::TYPE_PATH_INFO(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  this->type_path_kind = Read_UINT8(buf);
  this->type_argument_index = Read_UINT8(buf);
}

TYPE_ANNOTATION::TYPE_ANNOTATION(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  UINT8 target_type = Read_UINT8(buf);
  this->target_type = target_type;
  switch (target_type) {
  case 0x00:
  case 0x01: {
    this->target_info.type_parameter_target.type_parameter_index = Read_UINT8(buf);
    break;
  }
  case 0x10: {
    this->target_info.supertype_target.supertype_index = Read_UINT16(buf);
    break;
  }
  case 0x11:
  case 0x12: {
    this->target_info.type_parameter_bound_target.type_parameter_index = Read_UINT8(buf);
    this->target_info.type_parameter_bound_target.bound_index = Read_UINT8(buf);
    break;
  }
  case 0x13:
  case 0x14:
  case 0x15:
    // empty_target
    break;
  case 0x16: {
    this->target_info.formal_parameter_target.formal_parameter_index = Read_UINT8(buf);
    break;
  }
  case 0x17: {
    this->target_info.throws_target.throws_type_index = Read_UINT16(buf);
    break;
  }
  case 0x40:
  case 0x41: {
    UINT16 table_length = Read_UINT16(buf);
    this->target_info.localvar_target.table_length = table_length;
    ANNO_LOCAL_TYPE **table = NULL;
    if (table_length > 0) {
      table = (ANNO_LOCAL_TYPE **) MEM_POOL_Alloc(pool, table_length * sizeof(ANNO_LOCAL_TYPE *));
      for (int i = 0; i < table_length; i++) {
        table[i] = CXX_NEW(ANNO_LOCAL_TYPE(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
      }
    }
    this->target_info.localvar_target.table = table;
    break;
  }
  case 0x42: {
    this->target_info.catch_target.exception_table_index = Read_UINT16(buf);
    break;
  }
  case 0x43:
  case 0x44:
  case 0x45:
  case 0x46: {
    this->target_info.offset_target.offset = Read_UINT16(buf);
    break;
  }
  case 0x47:
  case 0x48:
  case 0x49:
  case 0x4A:
  case 0x4B: {
    this->target_info.type_argument_target.offset = Read_UINT16(buf);
    this->target_info.type_argument_target.type_argument_index = Read_UINT8(buf);
    break;
  }
  default:
    Is_True_Ret(FALSE, ("Not supported target type, target type: %d", target_type));
  }
  UINT8 path_length = Read_UINT8(buf);
  this->target_path.path_length = path_length;
  TYPE_PATH_INFO **path = NULL;
  if (path_length > 0) {
    path = (TYPE_PATH_INFO **) MEM_POOL_Alloc(pool, path_length * sizeof(TYPE_PATH_INFO *));
    for (INT i = 0; i < path_length; i++) {
      path[i] = CXX_NEW(TYPE_PATH_INFO(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
    }
  }
  this->target_path.path = path;
  UINT16 type_index = Read_UINT16(buf);
  this->type_index = type_index;
  UINT16 num_element_value_pairs = Read_UINT16(buf);
  ELEMENT_VALUE_PAIR **element_value_pairs = NULL;
  if (num_element_value_pairs > 0) {
    element_value_pairs = (ELEMENT_VALUE_PAIR **) MEM_POOL_Alloc(pool, num_element_value_pairs * sizeof(ELEMENT_VALUE_PAIR *));
    for (INT i = 0; i < num_element_value_pairs; i++) {
      element_value_pairs[i] = CXX_NEW(ELEMENT_VALUE_PAIR(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
    }
  }
  this->element_value_pairs = element_value_pairs;
}

RUNTIME_VISIABLE_TYPE_ANNOTATIONS::RUNTIME_VISIABLE_TYPE_ANNOTATIONS(char *&buf, UINT8 type, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  Is_True(FALSE, ("Not supported yet."));
  if (type != JV_CLASS_ATTR) {
    UINT16 index = Read_UINT16(buf);
    this->member_index = index;
  } else {
    this->member_index = 0;
  }
  UINT16 num_annotations = Read_UINT16(buf);
  this->num_annotations = num_annotations;
  TYPE_ANNOTATION **type_annotations = NULL;
  if (num_annotations > 0) {
    type_annotations = (TYPE_ANNOTATION **) MEM_POOL_Alloc(pool, num_annotations * sizeof(TYPE_ANNOTATION *));
    for (int i = 0; i< num_annotations; i++) {
      type_annotations[i] = CXX_NEW(TYPE_ANNOTATION(buf, constant_data, constant_tags, st_inito_cache, pool), pool);
    }
  }
  this->type_annotations = type_annotations;
}

ANNOTATIONS_ATTR::ANNOTATIONS_ATTR(char *&buf, INITV_ENTRIES &constant_data,
  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool)
{
  UINT8 type = Read_UINT8(buf);
  this->type = type;
  // finished
  if (type == JV_DONE_ATTR) {
    return;
  }
  UINT32 len = Read_UINT32(buf);
  this->len = len;
  UINT8 kind = Read_UINT8(buf);
  this->kind = kind;
  if (kind == JV_ANNOTATIONS_KIND) {
    this->annos_data.r_vis_annos = CXX_NEW(R_VIS_ANNOS(buf, type, constant_data, constant_tags, st_inito_cache, pool), pool);
  } else if (kind == JV_PARAMETER_ANNOTATIONS_KIND) {
    this->annos_data.r_vis_param_annos = CXX_NEW(R_VIS_PARAM_ANNOS(buf, type, constant_data, constant_tags, st_inito_cache, pool), pool);
  } else if (kind == JV_TYPE_ANNOTATIONS_KIND) {
    // ignore TYPE_ANNOTATIONS for now
    // this->annos_data.r_vis_type_annos = R_VIS_TYPE_ANNOS::Parse(buf, type, constant_data, constant_tags, st_inito_cache, pool);
    buf += len - 1;
  } else {
    Is_True(FALSE, ("Not supported kind for now, kind: %d", kind));
  }
}

void
ANNOTATIONS_ATTR::Print(FILE *fp)
{
  fprintf(fp, "ANNOTATIONS type: %s, kind: %s",
    Jv_attr_type_name((JV_ATTR_TYPE) type), Jv_attr_kind_name((JV_ATTR_KIND) kind));
  if (kind == JV_ANNOTATIONS_KIND) {
    fprintf(fp, ", member_index:%d\n", annos_data.r_vis_annos->member_index);
    annos_data.r_vis_annos->Print(fp);
  } else if (kind == JV_PARAMETER_ANNOTATIONS_KIND) {
    fprintf(fp, ", member_index:%d\n", annos_data.r_vis_param_annos->member_index);
    annos_data.r_vis_param_annos->Print(fp);
  } else if (kind == JV_TYPE_ANNOTATIONS_KIND) {

  }
}

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Read_annotations - Read method table,
// Create method sig and method table
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Read_annotations_attr(C_STR class_ty, ST_IDX reflection_st,
  INITV_ENTRIES &constant_data, INITV_ENTRIES &constant_tags)
{
  if(reflection_st == ST_IDX_ZERO) return;
  CLASS_INFO *info = Get_class_info(class_ty);
  Is_True_Ret(info && info->Get_aux_info(), ("Don't have class info."));

  INITV_ENTRIES buf_data;
  Get_initv_entry(Get_initv_idx_by_st_idx(reflection_st), &buf_data);
  Is_True_Ret(buf_data.Size() == ANNOT_END,
              ("Reflection data inito length is not 1, length: %d", buf_data.Size()));
  IDTYPE str_idx = buf_data.Get_initv_str_idx(ANNOT_DATA, TRUE);
  mUINT32 str_len = buf_data.Get_initv_str_len(ANNOT_DATA, TRUE);
  char *buffer = Index_to_char_array(str_idx);
  char *p = buffer;

  ANNOS_ATTR_VEC *annos_attr_vec = info->Get_aux_info()->Get_annos_attr_vec();
  while (TRUE) {
    ANNOTATIONS_ATTR *annos_attr = CXX_NEW(ANNOTATIONS_ATTR(p, constant_data, constant_tags,
                                            Get_st_inito_cache(), Mem_pool()), Mem_pool());
    // finished parse
    if (annos_attr->type == JV_DONE_ATTR) {
      break;
    }
    annos_attr_vec->push_back(annos_attr);
  }
  Is_True(p <= buffer + str_len, ("Parse error, buffer overflow."));
}

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Create_meth_sig - Create method sig
// compsed by method name and signature name
//
// =============================================================================
C_STR
JAVA_CLASS_HIERARCHY_BUILDER::Create_meth_sig(STR_IDX name_idx, STR_IDX sig_idx)
{
  char *meth_name = Index_to_char_array (name_idx);
  UINT32 meth_len = Index_to_length(name_idx) - 1;
  char *sig_name = Index_to_char_array(sig_idx);
  UINT32 sig_len = Index_to_length(sig_idx) - 1;
  UINT32 sum_len = meth_len + sig_len;
  char* buffer = (char *)MEM_POOL_Alloc(_pool, sum_len + 1);
  strncpy(buffer, meth_name, meth_len);
  strncpy(buffer + meth_len, sig_name, sig_len);
  buffer[sum_len] = '\0';
  return buffer;
}

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::New_class_info - Create new java class info
//
// =============================================================================
CLASS_INFO*
JAVA_CLASS_HIERARCHY_BUILDER::New_class_info(C_STR name)
{
  CLASS_INFO *cls_info = CXX_NEW(CLASS_INFO(name, _pool, JAVA_CLASS, File_Index), _pool);
  JAVA_AUX_INFO *aux_info = CXX_NEW(JAVA_AUX_INFO(_pool), _pool);
  cls_info->Add_aux_info(aux_info);
  return cls_info;
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::New_meth_table - Create new method table
//
// =============================================================================
JAVA_METH_INFO*
JAVA_CLASS_HIERARCHY_BUILDER::New_meth_table()
{
  return CXX_NEW(JAVA_METH_INFO(_pool), _pool);
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Add_interface - Add interface class
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Add_interface(C_STR class_ty, C_STR interface)
{
  CLASS_INFO *info = Get_class_info(class_ty);
  Is_True_Ret(info && info->Get_aux_info(),
              ("no class info found for type %s\n", class_ty));
  if (info && info->Get_aux_info())
    info->Get_aux_info()->Add_interface(interface);
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Add_meth_sig - Add method signature
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Add_meth_sig(C_STR class_ty, C_STR sig, CALL_OFF off)
{
  CLASS_INFO *info = Get_class_info(class_ty);
  Is_True_Ret(info && info->Get_aux_info(),
              ("no java class info found for type %s\n", class_ty));
  if (info && info->Get_aux_info())
    info->Get_aux_info()->Add_meth_sig(sig, off);
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Set_class_is_interface - set class is an interface
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Set_class_is_interface(C_STR class_ty)
{
  CLASS_INFO *info = Get_class_info(class_ty);
  Is_True_Ret(info && info->Get_aux_info(),
              ("no java class info found for type %s\n", class_ty));
  if (info && info->Get_aux_info())
    info->Get_aux_info()->Set_class_is_interface();
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Set_class_is_interface - set class inherites
// from an interface
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Set_class_has_interface(C_STR class_ty)
{
  CLASS_INFO *info = Get_class_info(class_ty);
  Is_True_Ret(info && info->Get_aux_info(),
          ("no java class info found for type %s\n", class_ty));
  if (info && info->Get_aux_info())
    info->Get_aux_info()->Set_class_has_interface();
}


// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Set_class_acc_flag - set class access flags
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_BUILDER::Set_class_acc_flag(CLASS_INFO *info, UINT16 flags)
{
  Is_True_Ret(info && info->Get_aux_info(), ("no java class info\n"));
  info->Get_aux_info()->Set_class_acc_flag(flags);
}

NAME_SET *JAVA_CLASS_HIERARCHY_HELPER::rbc_class_sym_set = NULL;
NAME_SET *JAVA_CLASS_HIERARCHY_HELPER::rbc_vtable_sym_set = NULL;

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Build_rbc_dna_set: collect all rbc dna, and
//     save vtable symbol name and class symbol name in set
//     for finding those symbol in vtable file, and build related class type
//        hierarchy
//
// =============================================================================
void
JAVA_CLASS_HIERARCHY_HELPER::Init(MEM_POOL* pool)
{
  rbc_class_sym_set = CXX_NEW(
    NAME_SET(DEFAULT_HASH_TABLE_SIZE, __gnu_cxx::hash<NAME>(),
      equal_str(), NAME_ALLOCATOR(pool)), pool
  );
  rbc_vtable_sym_set = CXX_NEW(
    NAME_SET(DEFAULT_HASH_TABLE_SIZE, __gnu_cxx::hash<NAME>(),
      equal_str(), NAME_ALLOCATOR(pool)), pool
  );
}
