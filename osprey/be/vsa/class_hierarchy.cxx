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

#include "class_hierarchy.h"
#include <string.h>
#include <assert.h>
#include <utility>
#include <queue>
#include <map>
#include "irbdata.h"
#include "gnu/demangle.h"
#include "cxx_memory.h"
#include "dwarf_DST.h"
#include "opt_defs.h"
#include "config_vsa.h"
#include "j_class_hierarchy_bldr.h"
#include "cxx_class_hierarchy_bldr.h"
#include "opt_vsa_util.h"

#define CLASS_SYMBOL_MARK  "class$E"
#define DEMANGLE_OPTION DMGL_PARAMS|DMGL_VERBOSE|DMGL_ANSI|DMGL_TYPES

// =============================================================================
//
// VIRFUNC_INFO::Is_equal - Check if two virfun info are same
//
// =============================================================================
BOOL VIRFUNC_INFO::Is_equal(VIRFUNC_INFO *f1, VIRFUNC_INFO *f2)
{
  if(f1 == f2) {
    return TRUE;
  } else if(f1 == NULL || f2 == NULL) {
    return FALSE;
  }
  if(f1->_offset == f2->_offset &&
     f1->_vptr_ofst == f2->_vptr_ofst &&
     !strcmp(ST_name(f1->_file_idx, f1->_fun_st),
             ST_name(f2->_file_idx, f2->_fun_st))) {
    return TRUE;
  }
  return FALSE;
}

// =============================================================================
//
// VIRFUNC_INFO::Print - display virutal function info
//
// =============================================================================
void
VIRFUNC_INFO::Print(FILE *fp) {
  fprintf(fp, "     %s: [%s+%d] at this+%d\n",
          cplus_demangle(ST_name(_file_idx, _fun_st), DEMANGLE_OPTION),
          _vtable_sym ? cplus_demangle(ST_name(_file_idx, _vtable_sym), DEMANGLE_OPTION) : "null",
          _offset, _vptr_ofst);
}

// =============================================================================
//
// CLASS_INFO::Clear_call_info - clean up memory
//
// =============================================================================
void
CLASS_INFO::Clear_call_info(CALL_VEC_MAP *call_map) {
  CALL_VEC_MAP_ITER it = call_map->begin();
  CALL_VEC_MAP_ITER itEnd = call_map->end();
  for(; it != itEnd; it++) {
    VIRFUNC_INFO_VEC *info_list = it->second;
    for(int idx = 0; idx<info_list->size(); idx++) {
      VIRFUNC_INFO *info = info_list->at(idx);
      CXX_DELETE(info, _pool);
    }
    CXX_DELETE(info_list, _pool);
  }
}

// =============================================================================
//
// CLASS_INFO::operator== check if two CLASS_INFO contents are same
//
// =============================================================================
BOOL
CLASS_INFO::operator==(const CLASS_INFO &other) const
{
  if(this == &other) {
    return TRUE;
  }
  if(_kind == other._kind &&
     _max_vtable_ofst == other._max_vtable_ofst &&
     _vtable.size() == other._vtable.size() &&
     _parents.size() == other._parents.size() &&
     _children.size() == other._children.size() &&
     ((_aux_info && other._aux_info) || (!_aux_info && !other._aux_info)) &&
     strcmp(_class_name, other._class_name) == 0 &&
     std::equal(_parents.begin(), _parents.end(),
                other._parents.begin(), equal_str()) &&
     std::equal(_children.begin(), _children.end(),
                other._children.begin(), equal_str())) {
    // compare vtables
    CALL_VEC_MAP_CONST_ITER vec_iter = _vtable.begin();
    CALL_VEC_MAP_CONST_ITER vec_iter_end = _vtable.end();
    for(; vec_iter != vec_iter_end; vec_iter++) {
      CALL_VEC_MAP_CONST_ITER other_iter = other._vtable.find(vec_iter->first);
      if(other_iter == other._vtable.end()) {
        return FALSE;
      } else if (vec_iter->second->size() != other_iter->second->size()) {
        return FALSE;
      } else if(!std::equal(vec_iter->second->begin(),
                            vec_iter->second->end(),
                            other_iter->second->begin(),
                            VIRFUNC_INFO::Is_equal)) {
        return FALSE;
      }
    }
    // compare aux_info
    if(_aux_info && !(*_aux_info == *(other._aux_info))) {
      return FALSE;
    }
    return TRUE;
  } else {
    return FALSE;
  }
  // add default return value
  return TRUE;
}

// =============================================================================
//
// CLASS_INFO::Add_method - add virtual function to vtable list
//
// =============================================================================
void
CLASS_INFO::Add_method(INT32 offset, CALL_OFF call_offset, ST_IDX method_sym,
                       ST_IDX vtable_st, INT32 vptr_ofst)
{
  VIRFUNC_INFO *new_func = CXX_NEW(VIRFUNC_INFO(offset, method_sym, vtable_st,
                                                vptr_ofst, File_Index), _pool);
  VIRFUNC_INFO_VEC *func_list = NULL;
  if(_vtable.find(call_offset) == _vtable.end()) {
    func_list = CXX_NEW(VIRFUNC_INFO_VEC(VIRFUNC_INFO_ALLOCATOR(_pool)), _pool);
    _vtable[call_offset] = func_list;
    _max_vtable_ofst = _max_vtable_ofst > call_offset ? _max_vtable_ofst : call_offset;
  } else {
    func_list = _vtable[call_offset];
  }
  Is_True_Ret(func_list != NULL, ("memory not allocated for vtable entries"));
  func_list->push_back(new_func);
}

// =============================================================================
//
// CLASS_INFO::Add_parent - add parent classe
//
// =============================================================================
void
CLASS_INFO::Add_parent(C_STR parent)
{
  C_STR_VEC_ITER iter = _parents.begin();
  C_STR_VEC_ITER iterEnd = _parents.end();
  for(; iter != iterEnd; iter++) {
    if(!strcmp(parent, *iter)) {
      return;
    }
  }
  _parents.push_back(parent);
}

// =============================================================================
//
// CLASS_INFO::Add_parents - add parent classes
//
// =============================================================================
void
CLASS_INFO::Add_parents(C_STR_VEC *parents)
{
  if(parents != NULL) {
    for (int idx = 0; idx < parents->size(); idx++) {
      C_STR parent = (*parents)[idx];
      Add_parent(parent);
    }
  }
}

// =============================================================================
//
// CLASS_INFO::Add_child - add child classes
//
// =============================================================================
void
CLASS_INFO::Add_child(C_STR child)
{
  CLASS_SET_ITER iter = _children.begin();
  if (_children.find(child) == _children.end()) {
    _children.insert(child);
  }
}

// =============================================================================
//
// CLASS_INFO::Add_children - add children classes
//
// =============================================================================
void
CLASS_INFO::Add_children(CLASS_SET *children)
{
  if(children != NULL) {
    _children.insert(children->begin(), children->end());
  }
}

// =============================================================================
//
// CLASS_INFO::Is_thunk - return true if pu is thunk
//
// =============================================================================
BOOL
CLASS_INFO::Is_thunk(ST_IDX st_idx)
{
  PU pu = Pu_Table[ST_pu(&St_Table[st_idx])];
  if(PU_is_thunk(pu)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

// =============================================================================
//
// CLASS_INFO::Is_base_class - check if this class is base class of input class
//
// =============================================================================
BOOL
CLASS_INFO::Is_base_class(C_STR ty)
{
  if(ty == NULL) {
    return FALSE;
  }
  return Get_children()->find(ty) != Get_children()->end();
}

// =============================================================================
//
// CLASS_INFO::Get_cand_calls - return candidate call list at given offset
// if not found return NULL
//
// =============================================================================
VIRFUNC_INFO_VEC*
CLASS_INFO::Get_cand_calls(int offset)
{
  CALL_VEC_MAP_ITER iter = _cand_calls.find(offset);
  if(iter != _cand_calls.end()) {
    return iter->second;
  } else {
    return NULL;
  }
}

// =============================================================================
//
// CLASS_INFO::Get_vtable_entry - return vtable entry at given offset
//
// =============================================================================
VIRFUNC_INFO *
CLASS_INFO::Get_vtable_entry(int offset, int vptr_ofst)
{
  CALL_VEC_MAP_ITER iter = _vtable.find(offset);
  if(iter != _vtable.end()) {
    VIRFUNC_INFO_VEC *vec = iter->second;
    if(vec == NULL) {
      return NULL;
    }
    VIRFUNC_INFO_VEC_ITER v;
    for (v = vec->begin(); v != vec->end(); ++v) {
      if ((*v)->Vptr_ofst() == vptr_ofst) {
        return *v;
      }
    }
  }
  return NULL;
}

// =============================================================================
//
// CLASS_INFO::Get_vtable_entry - return vtable entry at vtable_name + vtable_ofst
//
// =============================================================================
VIRFUNC_INFO *
CLASS_INFO::Get_vtable_entry(const char* vtbl_name, INT32 vtbl_ofst)
{
  CALL_VEC_MAP_ITER iter = _vtable.begin();
  CALL_VEC_MAP_ITER iter_end = _vtable.end();
  for(; iter != iter_end; iter++) {
    VIRFUNC_INFO_VEC *vec = iter->second;
    if(vec != NULL) {
      for(int i = 0; i < vec->size(); i++) {
        VIRFUNC_INFO *item = (*vec)[i];
        if(item && item->_offset == vtbl_ofst &&
           strcmp(vtbl_name, ST_name(item->_file_idx, item->_vtable_sym)) == 0) {
          return item;
        }
      }
    }
  }
  return NULL;
}

// =============================================================================
//
// CLASS_INFO::Get_vtable_ofst - return vtable ofst with given fname
//
// =============================================================================
INT32
CLASS_INFO::Get_vtable_ofst(const char *fname)
{
  CALL_VEC_MAP_ITER iter = _vtable.begin();
  CALL_VEC_MAP_ITER iterEnd = _vtable.end();
  for(; iter != iterEnd; iter++) {
    VIRFUNC_INFO_VEC *vec = iter->second;
    if(vec == NULL) {
      return INVALID_VTABLE_OFFSET;
    } else {
      for (int i = 0; i < vec->size(); i++) {
        VIRFUNC_INFO *vinfo = (*vec)[i];
        const char *vfname = ST_name(vinfo->File_idx(), vinfo->Fun_st());
        if(!strcmp(vfname, fname)) {
          return iter->first;
        }
      }
    }
  }
  return INVALID_VTABLE_OFFSET;
}

// =============================================================================
//
// CLASS_INFO::Add_vtable_to_candidate - add input class's vtable to current
// class's candidates.
// if force_add = true, always add to candidates
// if force_add = false, only add when current class already contain an entry
// at the offset
//
// =============================================================================
void
CLASS_INFO::Add_vtable_to_candidate(CLASS_INFO *info, BOOL force_add)
{
  // do not add interfaces vtable to parent
  // interfaces's vtable is used to store methods implemented in current interface
  // they should not add to parent candidates as offset is not uniq for same signature
  if(info->Get_aux_info() && info->Get_aux_info()->Class_is_interface()) {
    return;
  }
  CALL_VEC_MAP vtable = info->_vtable;
  CALL_VEC_MAP_ITER call_iter;
  for(call_iter = vtable.begin(); call_iter != vtable.end(); call_iter++) {
    INT32 call_offset = call_iter->first;
    VIRFUNC_INFO_VEC* call_list_other = call_iter->second;
    if(force_add) {
      Is_True_Ret( !Get_cand_calls(call_offset),
                  ("vtable entry already exists in candidate"));
    }
    Add_candidates(call_list_other, call_offset, force_add);
  }
}

// =============================================================================
//
// CLASS_INFO::Add_interface_to_candidates
// Parameters:
//    this: interface class
//    info: class inherit from this
// Check each method signatures in info, if the signature was found in interface
// class, add the candidate calls to interface class.
// For ex:   Interface If { void foo(); }
//           Class IfA implements If { void foo() {} }
// IfA::foo() will be added to If's vtable candidates
//
// =============================================================================
void
CLASS_INFO::Add_interface_to_candidates(CLASS_INFO *info)
{
  Is_True_Ret(Is_java_class() && info->Is_java_class(),
              ("should be java class info"));
  Is_True_Ret(Get_aux_info() &&
              info->Get_aux_info() &&
              info->Get_aux_info()->Class_has_interface(),
              ("class should have interfaces"));
  C_STR_INT32_MAP &my_sigs = Get_aux_info()->Get_meth_sig_map();
  C_STR_INT32_MAP_ITER it = my_sigs.begin();
  C_STR_INT32_MAP_ITER itEnd = my_sigs.end();
  C_STR_INT32_MAP &cand_sigs = info->Get_aux_info()->Get_meth_sig_map();
  for(; it != itEnd; it++) {
    C_STR sig = it->first;
    INT32 interf_off = it->second  * -1;
    Is_True_Ret(interf_off > 0, ("interface call offset should > 0"));
    if(cand_sigs.find(sig) != cand_sigs.end()) {
      CALL_OFF offset = cand_sigs[sig] * Pointer_Size;
      VIRFUNC_INFO_VEC *other_cand_calls = info->Get_cand_calls(offset);
      if(other_cand_calls) {
        Add_candidates(other_cand_calls, interf_off, TRUE);
      }
    }
  }
}

// =============================================================================
//
// CLASS_INFO::Add_interface_vtable_to_candidates
// Parameters:
//    this: interface class
// Add current interface's vtable entries to candidates, for rt.o rule resolving
//
// =============================================================================
void
CLASS_INFO::Add_interface_vtable_to_candidates()
{
  Is_True_Ret(Kind() == JAVA_CLASS, ("should be java class info"));
  Is_True_Ret(Get_aux_info() &&
              Get_aux_info()->Class_is_interface(),
              ("class should be interfaces"));
  CALL_VEC_MAP_ITER vt_it;
  for(vt_it = _vtable.begin(); vt_it != _vtable.end(); vt_it++) {
    VIRFUNC_INFO_VEC* virfuns = vt_it->second;
    INT32 interf_off = vt_it->first * -1;
    Is_True_Ret(interf_off > 0, ("interface call offset should > 0"));
    Add_candidates(virfuns, interf_off, TRUE);
  }
}

// =============================================================================
//
// CLASS_INFO::Add_candidates - candidates may come from:
// 1. current class info's vtable or interface table, force_add is true,
//    candidates will be always added
// 2. From other class info(parent, child), force_add is false, candidates will only
//    be added when offset didn't exceed current class's vtable limit
//
// =============================================================================
void
CLASS_INFO::Add_candidates(VIRFUNC_INFO_VEC *cands, INT32 offset, BOOL force_add)
{
  Is_True_Ret(cands, ("candidates is empty"));
  VIRFUNC_INFO_VEC *cand_calls = Get_cand_calls(offset);
  if((force_add && !cand_calls) ||
     (!force_add && !cand_calls &&
       offset <= _max_vtable_ofst)) {
    cand_calls = CXX_NEW(VIRFUNC_INFO_VEC(VIRFUNC_INFO_ALLOCATOR(_pool)), _pool);
    _cand_calls[offset] = cand_calls;
  }
  // only add candidates with differet fun symbol
  if(cand_calls) {
    VIRFUNC_INFO_VEC_ITER cand_iter = cands->begin();
    for(; cand_iter != cands->end(); cand_iter++) {
      VIRFUNC_INFO_VEC_ITER parent_iter = cand_calls->begin();
      BOOL found = FALSE;
      for(; parent_iter != cand_calls->end(); parent_iter++) {
        if(*(*parent_iter) == *(*cand_iter)) {
          found = TRUE;
          break;
        }
      }
      if(!found) {
        cand_calls->push_back(*cand_iter);
      }
    }
  }
}

// =============================================================================
//
// CLASS_INFO::Get_class_acc_flag - Get class access flag, only for Java
//
// =============================================================================
UINT16
CLASS_INFO::Get_class_acc_flag()
{
  JAVA_AUX_INFO *aux_info = Get_aux_info();
  if(aux_info) {
    return aux_info->Get_class_acc_flag();
  }
  return ACC_INVALID;
}

// =============================================================================
//
// CLASS_INFO::Get_meth_by_fname
//   return method info list with given function name(no parameter and return type)
// Ex: Get_meth_by_fname("clone")
//
// =============================================================================
void
CLASS_INFO::Get_meth_by_fname(const char *fname, vector<JAVA_METH_INFO *> &methods)
{
  JAVA_AUX_INFO *aux_info = Get_aux_info();
  if(aux_info) {
    METH_TAB_MAP meth_table = aux_info->Get_meth_tab_map();
    METH_TAB_ITER iter;
    for(iter = meth_table.begin(); iter != meth_table.end(); iter++) {
      JAVA_METH_INFO *meth = iter->second;
      if(meth && strcmp(meth->Get_name(), fname) == 0) {
        methods.push_back(meth);
      }
    }
  }
}

// =============================================================================
//
// CLASS_INFO::Get_meth_annots - get annotate name list recorded on the method
// return FALSE if no annotation found
//
// =============================================================================
BOOL
CLASS_INFO::Get_meth_annots(const char *fname, vector<STRING> &annot_names)
{
  BOOL ret = FALSE;
  JAVA_AUX_INFO *aux_info  = Get_aux_info();
  if(aux_info == NULL) {
    return FALSE;
  }
  JAVA_METH_INFO *mt_info = aux_info->Get_java_meth_info(fname);
  if(mt_info == NULL) {
    return FALSE;
  }
  ANNOS_ATTR_VEC *annots = aux_info->Get_annos_attr_vec();
  ANNOS_ATTR_VEC::iterator annot_it;
  for(annot_it = annots->begin(); annot_it != annots->end(); annot_it++) {
    ANNOTATIONS_ATTR *annot_attr = *annot_it;
    if(annot_attr == NULL) {
      Is_True(FALSE, ("ANNOTATIONS_ATTR is null"));
      continue;
    }
    if(annot_attr->type == JV_METHOD_ATTR &&
       annot_attr->kind == JV_ANNOTATIONS_KIND) {
      R_VIS_ANNOS *r_annots = annot_attr->annos_data.r_vis_annos;
      if(r_annots == NULL) {
        continue;
      }
      // skip method with different index
      UINT16 meth_idx = r_annots->member_index;
      if(mt_info->Get_idx() != meth_idx) {
        continue;
      }
      for(int idx = 0; idx < r_annots->num_annotations; idx++) {
        ANNOTATION *annot = (r_annots->annotations)[idx];
        if(annot == NULL) {
          continue;
        }
        if(annot->type_name == NULL) {
          Is_True(FALSE, ("annotation name is null"));
          continue;
        }
        annot_names.push_back(annot->type_name);
        ret = TRUE;
      }
    }
  }
  return ret;
}

// =============================================================================
//
// CLASS_INFO::Print - Display class info
//
// =============================================================================
void
CLASS_INFO::Print(FILE *fp)
{
  fprintf(fp, " file_idx: %d\n", _file_idx);
  fprintf(fp, "\n vtable: %ld\n", _vtable.size());
  CALL_VEC_MAP_ITER call_map_iter;
  for(call_map_iter = _vtable.begin(); call_map_iter != _vtable.end(); call_map_iter++) {
    fprintf(fp, "   call offset:%d\n", call_map_iter->first);
    VIRFUNC_INFO_VEC* info = call_map_iter->second;
    for(int idx = 0; idx < info->size(); idx++) {
      info->at(idx)->Print(fp);
    }
  }

  fprintf(fp, "\n candidate calls: %d\n", (int)_cand_calls.size());
  for(call_map_iter = _cand_calls.begin(); call_map_iter != _cand_calls.end(); call_map_iter++) {
    fprintf(fp, "   call offset:%d\n", call_map_iter->first);
    VIRFUNC_INFO_VEC* info = call_map_iter->second;
    for(int idx = 0; idx < info->size(); idx++) {
      info->at(idx)->Print(fp);
    }
  }

  C_STR_VEC_ITER parent_iter;
  fprintf(fp, "  Parents: \n");
  for (parent_iter = _parents.begin(); parent_iter != _parents.end(); parent_iter++) {
    fprintf(fp, "    %s\n", *parent_iter);
  }

  CLASS_SET_ITER child_iter;
  fprintf(fp, "  Children: \n");
  for (child_iter = _children.begin(); child_iter != _children.end(); child_iter++) {
    fprintf(fp, "    %s\n", *child_iter);
  }

  if(Get_aux_info()) {
    Get_aux_info()->Print(fp);
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::~CLASS_HIERARCHY
//
// =============================================================================
CLASS_HIERARCHY::~CLASS_HIERARCHY()
{
  #if 0
  CLASS_INFO_MAP_CONST_ITER it = _class_info_map.begin();
  CLASS_INFO_MAP_CONST_ITER itEnd = _class_info_map.end();
  for(; it != itEnd; it++) {
    CLASS_INFO *info = it->second;
    CXX_DELETE(info, _pool);
  }
  #endif
}

// =============================================================================
//
// CLASS_HIERARCHY::Merge - merge class infos from input cha
//
// =============================================================================
void
CLASS_HIERARCHY::Merge(CLASS_HIERARCHY *cha)
{
  if (!cha) {
    return;
  }
  CLASS_INFO_MAP_CONST_ITER it = cha->_class_info_map.begin();
  for(; it != cha->_class_info_map.end(); it++) {
    CLASS_INFO_MAP_CONST_ITER class_iter = _class_info_map.find(it->first);
    // same class name, but different content
    if(VSA_Java_Tmp && class_iter != _class_info_map.end() &&
       !(*(class_iter->second) == *(it->second))) {
      // log warning for C++, assertion for Java class
      // mysql c++ app contains the code of same class name, but different content, don't
      // want to assert for that, just log the information
      if(class_iter->second->Is_cxx_class()) {
        Is_Trace(
          Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
          (TFile,
           "***WARNING CLASS HIERARCHY: same class name with different content: %s\n",
           class_iter->first));
      } else if (class_iter->second->Is_java_class()) {
        Is_Trace(
          Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
          (TFile,
           "***WARNING CLASS HIERARCHY: same class name with different content: %s\n",
           class_iter->first));
        CLASS_INFO *nw_info = class_iter->second;
        WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
        if (mgr != NULL) {
          WHIRL_FILE_INFO &nw_fi = mgr->Get_file(nw_info->File_idx());
          // if a new class info is from vtable file or rbc file, just ignore
          // user can define a class override the original class from library
          if (FILE_INFO_is_vtable(nw_fi.Whirl_file_info())) {
            continue;
          }
          // if the new class info is from source file, replace the old one
        }
      } else {
        Is_True_Ret(FALSE, ("DO not support this class kind, class kind: %d", class_iter->second->Kind(), FALSE));
      }
    }
    _class_info_map.insert(*it);
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Build_class_info - should not be called
//
// =============================================================================
void
CLASS_HIERARCHY::Build_class_info()
{
  Is_True_Ret(FALSE, ("Class hierarchy: unexpect code"));
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_interface_entry - return virtual function info for
// method at given interface offset
//
// =============================================================================
VIRFUNC_INFO*
CLASS_HIERARCHY::Get_interface_entry(C_STR def_class_name, C_STR if_name, INT32 if_off)
{
  // the offset in meth table is negative
  if_off = if_off * -1;
  CLASS_INFO *info = (CLASS_INFO *)Get_class_info(if_name);
  CLASS_INFO *def_info = (CLASS_INFO *)Get_class_info(def_class_name);
  if (!info || info->Is_link_class())
    return NULL;
  if (!def_info || def_info->Is_link_class())
    return NULL;
  Is_True_Ret(def_info->Is_java_class() && info->Is_java_class(),
              ("invalid class type"), NULL);
  JAVA_AUX_INFO *aux_info = info->Get_aux_info();
  JAVA_AUX_INFO *def_aux_info = def_info->Get_aux_info();
  if(!aux_info || !def_aux_info) {
    return NULL;
  }
  C_STR sig = aux_info->Get_meth_sig(if_off);
  Is_True_Ret(sig,
    ("Class hierarchy: unable to find meth sig, define class name : %s, interface : %s, offset : %d",
      def_class_name, if_name, if_off), NULL);
  CLASS_INFO *current_info = def_info;
  CALL_OFF off = INVALID_VTABLE_OFFSET;
  // Try to find method from parent's method table
  do {
    off = current_info->Get_aux_info()->Get_meth_off(sig);
    if (off != INVALID_VTABLE_OFFSET) {
      break;
    } else {
      C_STR_VEC *parents = current_info->Get_parents();
      if (parents->size() >= 1) {
        current_info = (CLASS_INFO *) Get_class_info(parents->at(0));
      } else if (parents->size() == 0) {
        break;
      }
    }
  } while (current_info);
  VIRFUNC_INFO *virf_info = NULL;
  if(off == INVALID_VTABLE_OFFSET) {
    // try to get fun info from interface's default method implementation
    virf_info = info->Get_vtable_entry(if_off, 0);
    Is_True_Ret(virf_info, ("CLASS HIERARCHY ERROR: unable to find interface method in class %s", if_name), NULL);
  } else {
    virf_info = def_info->Get_vtable_entry(off * Pointer_Size, 0);
  }
  return virf_info;
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_class_info - return class info with given class name
//
// =============================================================================
CLASS_INFO *
CLASS_HIERARCHY::Get_class_info(C_STR ty_name)
{
  if(ty_name == NULL) {
    return NULL;
  }
  CLASS_INFO_MAP_CONST_ITER it = _class_info_map.find(ty_name);
  if(it != _class_info_map.end()) {
    return it->second;
  } else {
    return NULL;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_parent - return the base class name
//
// =============================================================================
C_STR
CLASS_HIERARCHY::Get_parent(C_STR ty_name)
{
  CLASS_INFO *info = Get_class_info(ty_name);
  if(info) {
    C_STR_VEC *parents = info->Get_parents();
    if(!parents->empty()) {
      return parents->at(0);
    } else {
      return NULL;
    }
  } else {
    return NULL;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_parents - return parent classes
//
// =============================================================================
C_STR_VEC *
CLASS_HIERARCHY::Get_parents(C_STR ty_name)
{
  CLASS_INFO *info = Get_class_info(ty_name);
  if(info) {
    C_STR_VEC *parents = info->Get_parents();
    return parents;
  } else {
    return NULL;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_children - return children classes
//
// =============================================================================
CLASS_SET *
CLASS_HIERARCHY::Get_children(C_STR ty_name)
{
  CLASS_INFO *info = Get_class_info(ty_name);
  if(info) {
    CLASS_SET *children = info->Get_children();
    return children;
  } else {
    return NULL;
  }
}

// =============================================================================
//
//  Find_function_def_class_and_offset
//  find the function in its defined class and get the class name and offset.
//  the search is done by finding the class by the signature first
//  @param class_name, for returning the value of the offset found,
//                         should be a buffer at least as long as fname,
//  @param ofst,       for returning the value of the offset found
//  @return TRUE if success
//
// =============================================================================
BOOL CLASS_HIERARCHY::Find_function_def_class_and_offset(const char *signature, char *class_name, INT32 &ofst) {
  // First extract class name from function signature.
  STRING_BUFFER buf(class_name, strlen(signature) + 1);
  if(!Extract_class_name(signature, &buf)) {
    return FALSE;
  }
  CLASS_INFO *info = Get_class_info(class_name);
  if(info == NULL || info->Is_link_class()) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "-DEVIRT Cannot find class info for class %s \n", class_name));
    // This is okay since we are not loading every V-Table from rt.o
    // We're only loading those bound with an existing rule.
    return FALSE;
  }
  // Find v-table offset,
  // TODO: this may fail due to signature polymorphism calls, i.e. java.lang.invoke.MethodHandle
  // Failing would result in ofst = INVALID_VTABLE_OFFSET.
  ofst = info->Get_vtable_ofst(signature);
  if (ofst == INVALID_VTABLE_OFFSET) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "-DEVIRT Cannot find "
              "function definition in vtable, fname = %s, class_name = %s\n",
              signature, class_name));
    return FALSE;
  }
  return TRUE;
}

// =============================================================================
//
// CLASS_HIERARCHY::Find_candidate_functions_in_all_superclasses
// @return virtual function list for given class at
//         given offset
//
// =============================================================================
VIRFUNC_INFO_VEC*
CLASS_HIERARCHY::Find_candidate_functions(C_STR ty_name, INT32 offset, BOOL is_vfunc) {
  CLASS_INFO *info = Get_class_info(ty_name);
  if(info == NULL || info->Is_link_class()) {
    return NULL;
  } else {
    // if trying to get virtual function with a interface type,
    // it can only call "java.lang.Object" virtual function
    if(Class_is_interface(ty_name) && is_vfunc) {
      return Find_candidate_functions_in_all_superclasses(offset);
    } else {
      return info->Get_cand_calls(offset);
    }
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Find_candidate_functions_in_all_superclasses
// @return virtual function list for given fun name
//
// =============================================================================
VIRFUNC_INFO_VEC*
CLASS_HIERARCHY::Find_candidate_functions(const char *signature, MEM_POOL *temp_pool) {
  // java interface, call Object's method, is virtual call, need to use java.lang.Object class info
  char *class_name = (char *) MEM_POOL_Alloc(temp_pool, strlen(signature) + 1);
  memset(class_name, 0, strlen(signature) + 1);
  INT32 ofst = INVALID_VTABLE_OFFSET;
  if (Find_function_def_class_and_offset(signature, class_name, ofst)) {
    VIRFUNC_INFO_VEC  *info_vec = Find_candidate_functions(class_name, ofst > 0 ? ofst : -1 * ofst, FALSE);
    return info_vec;
  } else {
    return NULL;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Find_function_def_class_and_offset
// This searches the vtable in the class this function was defined
// by extracting the class name from the signature provided
//
// @param signature, the signature of the method to search for.
// @return INVALID_VTABLE_OFFSET if no function was found.
//
// =============================================================================
INT32
CLASS_HIERARCHY::Find_function_offset(const char *signature, MEM_POOL *temp_pool) {
  // java interface, call Object's method, is virtual call, need to use java.lang.Object class info
  char *class_name = (char *) MEM_POOL_Alloc(temp_pool, strlen(signature) + 1);
  memset(class_name, 0, strlen(signature) + 1);
  INT32 ofst = INVALID_VTABLE_OFFSET;
  BOOL result = Find_function_def_class_and_offset(signature, class_name, ofst);
  return result ? ofst : INVALID_VTABLE_OFFSET;
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_vtable_entry - return virtual function for given class
// at given offset
//
// =============================================================================
VIRFUNC_INFO*
CLASS_HIERARCHY::Get_vtable_entry(C_STR name, INT32 offset, INT32 vptr_ofst) {
  CLASS_INFO *info = Get_class_info(name);
  if(info == NULL || info->Is_link_class()) {
    return NULL;
  } else {
    return info->Get_vtable_entry(offset, vptr_ofst);
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_vtable_entry - return virtual function for given class
// at given vtable + offset
//
// =============================================================================
VIRFUNC_INFO*
CLASS_HIERARCHY::Get_vtable_entry(C_STR name, const char* vtbl_name, INT32 vtbl_ofst) {
  CLASS_INFO *info = Get_class_info(name);
  if(info == NULL || info->Is_link_class()) {
    return NULL;
  } else {
    return info->Get_vtable_entry(vtbl_name, vtbl_ofst);
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Find_candidate_functions_in_all_superclasses
// return virtual function for given offset in java.lang.Object
// at given offset, can only be parent's vtable(java.lang.Object)
//
// =============================================================================
VIRFUNC_INFO_VEC*
CLASS_HIERARCHY::Find_candidate_functions_in_all_superclasses(INT32 offset)
{
  C_STR parent_name = "java.lang.Object";
  CLASS_INFO *info = Get_class_info(parent_name);
  if(info == NULL || info->Is_link_class()) {
    return NULL;
  } else {
    return info->Get_cand_calls(offset);
  }
}


// =============================================================================
//
// CLASS_HIERARCHY::Get_meth_by_sig - return virtual function info by signature
//
// =============================================================================
VIRFUNC_INFO*
CLASS_HIERARCHY::Get_meth_by_sig(C_STR name, const char *sig)
{
  CLASS_INFO *info = Get_class_info(name);
  if(info && info->Get_aux_info()) {
    CALL_OFF off = info->Get_aux_info()->Get_meth_off(sig);
    if(off != INVALID_VTABLE_OFFSET) {
      // for off < 0 (interface), no need to * -1
      off = off > 0 ? off * Pointer_Size : off;
      return info->Get_vtable_entry(off, 0);
    }
  }
  return NULL;
}

// ==================================================
//
// CLASS_HIERARCHY::Find_candidate_functions_in_subclasses
// find the candidate functions
// skip finding in java.lang.Object
// @Deprecated, please use Find_candidate_functions_in_subclasses instead
//
// ==================================================
void
CLASS_HIERARCHY::Find_candidate_functions_in_subclasses(C_STR class_name, const char *sig, std::vector<VIRFUNC_INFO *> &meth_vec)
{
  CLASS_INFO *info = Get_class_info(class_name);
  if (!info || !info->Get_aux_info())
    return;
  // ignore root class
  if (strcmp(class_name, "java.lang.Object") == 0)
    return;
  CALL_OFF off = info->Get_aux_info()->Get_meth_off(sig);
  if (off != INVALID_VTABLE_OFFSET) {
    // for off < 0 (interface), no need to * -1
    off = off > 0 ? off * Pointer_Size : off;
    VIRFUNC_INFO *virinfo = info->Get_vtable_entry(off, 0);
    if(virinfo) {
      meth_vec.push_back(virinfo);
    }
  }
  CLASS_SET *children = info->Get_children();
  CLASS_SET_ITER iter;
  for (iter = children->begin(); iter != children->end(); ++iter) {
    CLASS_INFO *info = Get_class_info(*iter);
    if (info) {
      JAVA_AUX_INFO *aux_info = info->Get_aux_info();
      if (aux_info) {
        CALL_OFF off = aux_info->Get_meth_off(sig);
        if (off != INVALID_VTABLE_OFFSET) {
          // for off < 0 (interface), no need to * -1
          off = off > 0 ? off * Pointer_Size : off;
          VIRFUNC_INFO *virinfo = info->Get_vtable_entry(off, 0);
          if(virinfo) {
            meth_vec.push_back(virinfo);
          }
        }
      }
    }
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Is_base_class - return TRUE if ty1 is base class of ty2
//
// =============================================================================
BOOL
CLASS_HIERARCHY::Is_base_class(C_STR ty1, C_STR ty2)
{
  CLASS_INFO *info1 = Get_class_info(ty1);
  if(info1 == NULL) {
    return FALSE;
  } else {
    return info1->Is_base_class(ty2);
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_meth_flag
//
// =============================================================================
UINT32
CLASS_HIERARCHY::Get_meth_flag(const char *fname)
{
  STRING_BUFFER buf(strlen(fname) + 1);
  const char *class_name = Extract_class_name(fname, &buf);
  if(!class_name) {
    return ACC_INVALID;
  }
  CLASS_INFO *info = Get_class_info(class_name);
  if(info == NULL || info->Is_link_class()) {
    return ACC_INVALID;
  }
  JAVA_AUX_INFO *aux_info = info->Get_aux_info();
  if(aux_info) {
    return aux_info->Get_meth_flag(fname);
  }
  return ACC_INVALID;
}

JAVA_AUX_INFO*
CLASS_HIERARCHY::Get_java_aux_info(const char *fname)
{
  STRING_BUFFER buf(strlen(fname) + 1);
  const char *class_name = Extract_class_name(fname, &buf);
  if(!class_name) {
    return NULL;
  }
  CLASS_INFO *info = Get_class_info(class_name);
  if(info == NULL || info->Is_link_class()) {
    return NULL;
  }
  return info->Get_aux_info();
}

// =============================================================================
//
// CLASS_HIERARCHY::Get_class_acc_flag - Get class access flag, only for Java
//
// =============================================================================
UINT16
CLASS_HIERARCHY::Get_class_acc_flag(C_STR cls_name)
{
  CLASS_INFO *info = Get_class_info(cls_name);
  if(info) {
    return info->Get_class_acc_flag();
  }
  return ACC_INVALID;
}


// =============================================================================
//
// CLASS_HIERARCHY::Get_clinit_by_glob (return TRUE on success)
//  get clinit function st and file index by global variable name
//
// =============================================================================
BOOL
CLASS_HIERARCHY::Get_clinit_by_glob(const char *gname, UINT32 &def_file, ST_IDX &def_st)
{
  STRING_BUFFER buf(strlen(gname) + 1);
  const char *class_name = Extract_class_name(gname, &buf);
  if(class_name != NULL) {
    CLASS_INFO *info = Get_class_info(class_name);
    if(info && !info->Is_link_class() && info->Get_aux_info()) {
      def_st = info->Get_aux_info()->Get_clinit_meth();
      def_file = info->File_idx();
      if(def_st) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

// =============================================================================
//
// CLASS_HIERARCHY::Is_candidate_call - return TRUE if fun2 is candidates of
// fun1, fun1 may be interface call or abstract call
//
// =============================================================================
BOOL
CLASS_HIERARCHY::Is_candidate_call(const char *fun1, const char *fun2, INT32 offset)
{
  if(offset == 0) {
    return FALSE;
  }
  STRING_BUFFER buf1(strlen(fun1) + 1);
  const char *cls1_name = Extract_class_name(fun1, &buf1);
  if(cls1_name == NULL) {
    return FALSE;
  }
  // if same class return false, assume fun1 != fun2
  STRING_BUFFER buf2(strlen(fun2) + 1);
  const char *cls2_name = Extract_class_name(fun2, &buf2);
  if(cls2_name != NULL && strcmp(cls1_name, cls2_name) == 0) {
    return FALSE;
  }

  CLASS_INFO *info = Get_class_info(cls1_name);
  if(info == NULL || info->Is_link_class()) {
    return FALSE;
  } else {
    BOOL found_fun1 = FALSE;
    BOOL found_fun2 = FALSE;
    VIRFUNC_INFO_VEC *cand_calls = info->Get_cand_calls(offset);
    if(cand_calls) {
      VIRFUNC_INFO_VEC::iterator it;
      for (it = cand_calls->begin(); it != cand_calls->end(); ++it) {
        VIRFUNC_INFO* func = *it;
        Is_True_Ret(func != NULL, ("null vfunc entry in class hierarchy"), FALSE);
        char *fname = ST_name(func->_file_idx,  func->_fun_st);
        if(!strcmp(fun2, fname)) {
          found_fun2 = TRUE;
        } else if(!strcmp(fun1, fname)) {
          found_fun1 = TRUE;
        }
      }
      if(found_fun1 && found_fun2) {
        return TRUE;
      } else if(found_fun2 && info->Get_aux_info()) {
        // fun1 is abstract fun or interface method, there is no method symbol.
        // compare the method signature in method table
        if(Class_is_interface(cls1_name)) {
          offset = -1 * offset;
        } else {
          offset = offset / Pointer_Size;
        }
        C_STR sig = info->Get_aux_info()->Get_meth_sig(offset);
        if(sig == NULL) {
          return FALSE;
        }
        UINT32 sig_len = strlen(sig);
        char sig_copy[sig_len + 1];
        strncpy(sig_copy, sig, sig_len);
        sig_copy[sig_len] = '\0';
        if(sig_copy) {
          char meth_name[sig_len + 1];
          char *split = strstr(sig_copy, "(");
          if(split != NULL) {
            strncpy(meth_name, sig_copy, split - sig_copy);
            meth_name[split - sig_copy] = '\0';
            // only check if meth name match for now, didn't check parameters
            // TODO: retrive method name and sig from mangled fun name
            STRING_BUFFER buf(strlen(fun1) + 1);
            const char *fun_sig = Extract_fun_sig(fun1, &buf);
            if(fun_sig != NULL && strstr(fun_sig, meth_name) != NULL) {
              return TRUE;
            }
          }
        }
      }
    }
  }
  return FALSE;
}


// =============================================================================
//
// CLASS_HIERARCHY::Demangle_java_sig - Demangle for java Signature
//
// Refer: MangleTool:mangleForSignature
//
// =============================================================================
char *
CLASS_HIERARCHY::Demangle_java_sig(char *sig, STRING_BUFFER *buf)
{
  if(sig == NULL) {
    return NULL;
  }

  char *ret_str = sig;
  UINT32 len = strlen(sig);
  // Java Class Signature has pattern "L<class_name>;"
  if(*sig == 'L' && sig[len - 1] == ';' && len > 2) {
    if(buf->Current() == sig) {
      // if buf at same location with sig, directly extract from sig
      sig[len - 1] = '\0';
      ret_str = sig + 1;
    } else {
      buf->Append_n(sig + 1, len - 2);
      ret_str = (char*)buf->To_string();
    }
    // replace '/' with '.' for package separator
    char *p = ret_str;
    while(p && *p != '\0') {
      if(*p == '/') {
        *p = '.';
      }
      p++;
    }
  }
  return ret_str;
}

char *
CLASS_HIERARCHY::Demangle(const char *sym_name)
{
  int options = DMGL_PARAMS|DMGL_ANSI|DMGL_TYPES;
  if (PU_java_lang(Get_Current_PU()))
    options |= DMGL_JAVA;

  char *p = NULL;
  if (sym_name[0] == '_' && sym_name[1] == 'Z')
    p = cplus_demangle(sym_name, options);
  if (p) {
    if (PU_java_lang(Get_Current_PU())) {
      char * pch;
      pch = strstr (p, ")");
      if (pch && strlen(pch) > 1) {
        strncpy (pch + 1, "\000\000", 1);
      }
    }
    return p;
  }
  return NULL;
}

const char *
CLASS_HIERARCHY::Extract_class_name(const char *mangle_name, STRING_BUFFER *buf)
{
  Is_True_Ret(buf, ("null string buffer"), NULL);
  char *demangled_name = Demangle(mangle_name);
  if(!demangled_name) {
    return NULL;
  }
  char *src = strstr(demangled_name, "(");
  if(src && strlen(src) > 1) {
    *src = '\0';
  }
  src = demangled_name;
  char *src_end = src;
  char *pos;
  while((pos = strstr(src, ".")) != 0 ||
        (pos = strstr(src, "::")) != 0) {
    src_end = pos;
    src = (*pos == '.') ? pos + 1 : pos + 2;
  }

  if(src_end > demangled_name) {
    buf->Append_n(demangled_name, src_end - demangled_name);
  }
  free(demangled_name);
  return buf->To_string();
}

const char *
CLASS_HIERARCHY::Extract_fun_sig(const char *fun_name, STRING_BUFFER *buf)
{
  char *demangled_name = Demangle(fun_name);
  if(!demangled_name) {
    return NULL;
  }
  char *src = strstr(demangled_name, "(");
  if(src && strlen(src) > 1) {
    *src = '\0';
  }
  src = demangled_name;
  char *src_end = src;
  while((src = strstr(src, ".")) != 0) {
    src_end = src;
    src++;
  }
  if(src_end) {
    if(*src_end == '.') {
      src_end++;
    }
    buf->Append(src_end);
  }
  free(demangled_name);
  return buf->To_string();
}

// =============================================================================
//
// CLASS_HIERARCHY::Build_trav_order - Build topologic order from child to parent
// Input is a digraph in which node is CLASS_INFO, edge is child -> parent
// Output order is a sequence in which child visited before parent
//
// =============================================================================
void
CLASS_HIERARCHY::Build_trav_order(vector<CLASS_INFO *> *seq)
{
  std::map<CLASS_INFO*, INT32> in_degree;
  CLASS_INFO_MAP_CONST_ITER iter;

  // Initialize in_degree to zero
  for(iter = _class_info_map.begin(); iter !=  _class_info_map.end(); iter++)
  {
    in_degree[iter->second] = 0;
  }
  // Calculate in_degree, create missing parent CLASS_INFO
  // The missing parent/interface class_info added to a new map, and insert
  // later to avoid _class_info_map iterator invalid due to rehash
  CLASS_INFO_MAP new_infos;
  for(iter = _class_info_map.begin(); iter !=  _class_info_map.end(); iter++)
  {
    CLASS_INFO *info = iter->second;
    C_STR_VEC *parents = info->Get_parents();
    C_STR_VEC *interfaces = info->Get_interfaces();
    int pcnt = parents ? parents->size() : 0;
    int ifcnt = interfaces ? interfaces->size() : 0;
    for(int pidx = 0; pidx < pcnt; pidx++) {
      C_STR parent_name = parents->at(pidx);
      CLASS_INFO_MAP_ITER class_it = _class_info_map.find(parent_name);
      CLASS_INFO *pInfo = NULL;
      if(class_it != _class_info_map.end()) {
        pInfo = class_it->second;
      } else {
        CLASS_INFO_MAP_ITER new_class_it = new_infos.find(parent_name);
        if(new_class_it != new_infos.end()) {
          pInfo = new_class_it->second;
        }
      }
      if(pInfo) {
        in_degree[pInfo]++;
      } else {
        // After all classes been merged, we can now add the missing parent class info
        // to connect with the child
        pInfo = CXX_NEW(CLASS_INFO(parent_name, Mem_pool(),
                                   (CLASS_INFO_KIND)(info->Kind() | LINK_CLASS),
                                   INVALID_FILE_IDX),
                                   Mem_pool());
        new_infos[parent_name] = pInfo;
        if (info->Is_java_class()) {
          JAVA_AUX_INFO *aux_info = CXX_NEW(JAVA_AUX_INFO(Mem_pool()), Mem_pool());
          pInfo->Add_aux_info(aux_info);
        }
        pInfo->Add_child(info->Get_class_name());
        in_degree[pInfo] = 1;
      }
    }
    for(int ifidx = 0; ifidx < ifcnt; ifidx++) {
      C_STR if_name = interfaces->at(ifidx);
      CLASS_INFO_MAP_ITER class_it = _class_info_map.find(if_name);
      CLASS_INFO *if_info = NULL;
      if (class_it != _class_info_map.end()) {
        if_info = class_it->second;
      } else {
        CLASS_INFO_MAP_ITER new_class_it = new_infos.find(if_name);
        if(new_class_it != new_infos.end()) {
          if_info = new_class_it ->second;
        }
      }
      if(if_info) {
        in_degree[if_info]++;
      } else {
        // Connect missing interface with its implemented classes
        if_info = CXX_NEW(CLASS_INFO(if_name, Mem_pool(),
                                     (CLASS_INFO_KIND)(JAVA_CLASS | LINK_CLASS),
                                     INVALID_FILE_IDX),
                                     Mem_pool());
        new_infos[if_name] = if_info;
        JAVA_AUX_INFO *aux_info = CXX_NEW(JAVA_AUX_INFO(Mem_pool()), Mem_pool());
        aux_info->Set_class_is_interface();
        if_info->Add_aux_info(aux_info);
        if_info->Add_child(info->Get_class_name());
        in_degree[if_info] = 1;
      }
    }
  }

  // add missing parent/interface info
  for(iter = new_infos.begin(); iter != new_infos.end(); iter++)
  {
    Is_True(_class_info_map.find(iter->first) == _class_info_map.end(),
            ("class %s already added to class hierarchy", iter->first));
    _class_info_map[iter->first] = iter->second;
  }

  std::queue<CLASS_INFO *> class_queue;
  for(iter = _class_info_map.begin(); iter !=  _class_info_map.end(); iter++)
  {
    if(in_degree[iter->second] == 0)
      class_queue.push(iter->second);
  }

  while(!class_queue.empty()) {
    CLASS_INFO *cur_node = class_queue.front();
    seq->push_back(cur_node);
    class_queue.pop();
    C_STR_VEC *parents = cur_node->Get_parents();
    if(parents) {
      C_STR_VEC_ITER it = parents->begin();
      for(; it != parents->end(); it++) {
        CLASS_INFO *parent = Get_class_info(*it);
        Is_True(parent, ("CHA: null parent info"));
        if(parent) {
          in_degree[parent]--;
          if(in_degree[parent] == 0) {
            class_queue.push(parent);
          }
        }
      }
    }
    C_STR_VEC *interfaces = cur_node->Get_interfaces();
    if(interfaces) {
      C_STR_VEC_ITER it = interfaces->begin();
      for(; it != interfaces->end(); it++) {
        CLASS_INFO *if_info = Get_class_info(*it);
        Is_True(if_info, ("CHA: null interface info"));
        if(if_info) {
          in_degree[if_info]--;
          if(in_degree[if_info] == 0) {
            class_queue.push(if_info);
          }
        }
      }
    }
  }
  Is_True(seq->size() == _class_info_map.size(), ("CHA build topological order failed"));
}

// =============================================================================
//
// CLASS_HIERARCHY::Connect_classes - iterate all class info, connect the parent
// children and interfaces
//
// =============================================================================
void
CLASS_HIERARCHY::Connect_classes()
{
  vector<CLASS_INFO *> trav_order;
  Build_trav_order(&trav_order);

  // reverse traverse to make sure parent always visited before child
  vector<CLASS_INFO *>::reverse_iterator class_iter;
  for(class_iter = trav_order.rbegin(); class_iter != trav_order.rend(); class_iter++) {
    CLASS_INFO *info = *class_iter;
    Connect_parent(info, info->Get_parents());
    JAVA_AUX_INFO *aux = info->Get_aux_info();
    if(aux && aux->Class_has_interface()) {
      Connect_interfaces_parent(info->Get_class_name(), aux->Get_interfaces());
    }
  }
  for(class_iter = trav_order.rbegin(); class_iter != trav_order.rend(); class_iter++) {
    CLASS_INFO *info = *class_iter;
    Connect_children(info->Get_class_name(), info);
  }

  // Connect interface only when parent/children info updated
  for(class_iter = trav_order.rbegin(); class_iter != trav_order.rend(); class_iter++) {
    CLASS_INFO *info = *class_iter;
    if(info->Is_java_class()) {
      JAVA_AUX_INFO *aux = info->Get_aux_info();
      Is_True_Ret(aux, ("CLASS HIERARCHY ERROR: null aux info, class : %s", info->Get_class_name()));
      // add parent class's interfaces to current class
      C_STR_VEC *parents = info->Get_parents();
      if(parents) {
        C_STR_VEC_ITER parent_iter = parents->begin();
        for(; parent_iter != parents->end(); parent_iter++) {
          CLASS_INFO *parent_info = Get_class_info(*parent_iter);
          if(parent_info) {
            JAVA_AUX_INFO *parent_aux_info = parent_info->Get_aux_info();
            if(parent_aux_info && parent_aux_info->Class_has_interface()) {
              aux->Add_interfaces(parent_aux_info->Get_interfaces());
              aux->Set_class_has_interface();
            }
          }
        }
        if(aux->Class_has_interface()) {
          Connect_interfaces_children(info->Get_class_name(), info);
        }
      }
    }
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Connect_parent - update class info's parent lists
//
// =============================================================================
void
CLASS_HIERARCHY::Connect_parent(CLASS_INFO *info, C_STR_VEC *parent)
{
  for (int i=0; i < parent->size(); i++) {
    C_STR parent_name = parent->at(i);
    if (_class_info_map.find(parent_name) != _class_info_map.end()) {
      CLASS_INFO *parent_info = _class_info_map[parent_name];
      parent_info->Add_child(info->Get_class_name());
      C_STR_VEC  *parents  = parent_info->Get_parents();
      if(parents != NULL) {
        info->Add_parents(parents);
      }
    } else {
      Is_Trace(
        Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
        (TFile, "CHA: class %s's parent %s not add to global class map\n",
         info->Get_class_name(), parent_name));
    }
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Connect_children - update class info's children list, add
// child's vtable to candidates
//
// =============================================================================
void
CLASS_HIERARCHY::Connect_children(C_STR idx, CLASS_INFO *info)
{
  C_STR_VEC *parent = info->Get_parents();
  C_STR_VEC_ITER pit;
  for(pit = parent->begin(); pit != parent->end(); ++pit) {
    C_STR parent_name = *pit;
    CLASS_INFO_MAP_ITER pclass_it = _class_info_map.find(parent_name);
    if (pclass_it == _class_info_map.end()) {
      Is_Trace(
        Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
        (TFile, "CHA: class %s's parent %s not add to global class map\n",
         info->Get_class_name(), parent_name));
      continue;
    }
    CLASS_INFO *pInfo = pclass_it->second;
    pInfo->Add_child(idx);
    pInfo->Add_vtable_to_candidate(info);
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Connect interfaces with parents
// interface if1
// interface if2 extends if1
// class cls1 implements if2
// Add if1 to cls1's interfaces list
//
// =============================================================================
void
CLASS_HIERARCHY::Connect_interfaces_parent(C_STR class_name, C_STR_VEC *interfaces)
{
  CLASS_INFO *class_info = Get_class_info(class_name);
  Is_True_Ret(class_info && class_info->Get_aux_info(), ("should be java class"));
  for (int i = 0; i < interfaces->size(); i++) {
    C_STR interf_name = interfaces->at(i);
    CLASS_INFO *interf_class = Get_class_info(interf_name);
    if(interf_class && interf_class->Get_aux_info()) {
      interf_class->Add_child(class_name);
      C_STR_VEC *p_interfs = interf_class->Get_aux_info()->Get_interfaces();
      class_info->Get_aux_info()->Add_interfaces(p_interfs);
    } else {
      Is_Trace(
        Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
        (TFile, "CHA: class %s's interface %s not add to global class map\n",
         class_info->Get_class_name(), interf_name));
      continue;
    }
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Connect_interfaces - update interface children list
// add info's method to interface candidates
//
// =============================================================================
void
CLASS_HIERARCHY::Connect_interfaces_children(C_STR class_name, CLASS_INFO *info)
{
  CLASS_INFO *class_info = Get_class_info(class_name);
  Is_True_Ret(class_info && class_info->Get_aux_info(), ("should be java class"));
  C_STR_VEC *interfaces = class_info->Get_aux_info()->Get_interfaces();
  for (int i = 0; i < interfaces->size(); i++) {
    C_STR interf_name = interfaces->at(i);
    CLASS_INFO *interf_class = Get_class_info(interf_name);
    if(interf_class) {
      // add child class
      interf_class->Add_child(class_name);
      // add child's children class
      // interf_class->Add_children(info->Get_children());
      interf_class->Add_interface_to_candidates(info);
    }
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::New_class_info - create new class info
//
// =============================================================================
CLASS_INFO *
CLASS_HIERARCHY::New_class_info(C_STR name)
{
  return CXX_NEW(CLASS_INFO(name, _pool, CXX_CLASS, File_Index), _pool);
}

// =============================================================================
//
// CLASS_HIERARCHY::Add_parent - add parent to class "ty", if "ty" class not yet
// created, create a new entry for it
//
// =============================================================================
CLASS_INFO *
CLASS_HIERARCHY::Add_class(C_STR ty)
{
  CLASS_INFO *info = Get_class_info(ty);
  if (!info) {
    info = New_class_info(ty);
    _class_info_map[ty] = info;
  }
  return info;
}

// =============================================================================
//
// CLASS_HIERARCHY::Add_parent - add parent to class "ty", if "ty" class not yet
// created, create a new entry for it
//
// =============================================================================
void
CLASS_HIERARCHY::Add_parent(C_STR ty, C_STR parent)
{
  CLASS_INFO *info = Get_class_info(ty);
  if(info) {
    info->Add_parent(parent);
  } else {
    CLASS_INFO *new_info = New_class_info(ty);
    new_info->Add_parent(parent);
    _class_info_map[ty] = new_info;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Add_child - add child to class "ty"
//
// =============================================================================
void
CLASS_HIERARCHY::Add_child(C_STR ty, C_STR child) {
  CLASS_INFO *info = Get_class_info(ty);
  if(info) {
    info->Add_child(child);
  } else {
    CLASS_INFO *new_info = New_class_info(ty);
    new_info->Add_child(child);
    _class_info_map[ty] = new_info;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Add_virtual_base - add virtual base to class "ty"
//
// =============================================================================
#if 0
void
CLASS_HIERARCHY::Add_virtual_base(C_STR ty, C_STR parent)
{
  CLASS_INFO *info = Get_class_info(ty);
  if(info) {
    info->Add_virtual_base(parent);
  } else {
    CLASS_INFO *new_info = New_class_info();
    new_info->Add_virtual_base(parent);
    _class_info_map[Copy_ty_name(ty)] = new_info;
  }
}
#endif

// =============================================================================
//
// CLASS_HIERARCHY::Add_interface - should not be called
//
// =============================================================================
void
CLASS_HIERARCHY::Add_interface(C_STR ty, C_STR interf)
{
  Is_True_Ret(FALSE, ("only java class hierarchy can have interfaces"));
}

// =============================================================================
//
// CLASS_HIERARCHY::Copy_ty_name - duplidate "ty" name in _pool
//
// =============================================================================
C_STR
CLASS_HIERARCHY::Copy_ty_name(C_STR ty)
{
  INT32 len = strlen(ty);
  char* copy_ty = (char*)MEM_POOL_Alloc(_pool, len + 1);
  strncpy(copy_ty, ty, len);
  copy_ty[len] = '\0';
  return copy_ty;
}

// =============================================================================
//
// JAVA_CLASS_HIERARCHY_BUILDER::Read_interface_table - read interface symbol
// inito entry, add interface info
//
// =============================================================================
void
CLASS_HIERARCHY::Add_method(TY_IDX ty, INT32 offset, INT32 call_offset,
                            ST_IDX method_sym, INT32 vptr_ofst)
{
  C_STR ty_name = TY_name(ty);
  if(_class_info_map.find(ty_name) != _class_info_map.end()) {
    _class_info_map[ty_name]->Add_method(offset, call_offset, method_sym,
                                         TY_vtable(ty), vptr_ofst);
  } else {
    C_STR cp_name = Copy_ty_name(ty_name);
    CLASS_INFO *new_info = New_class_info(cp_name);
    new_info->Add_method(offset, call_offset, method_sym,
                         TY_vtable(ty), vptr_ofst);
    new_info->Set_class_ty(ty);
    _class_info_map[cp_name] = new_info;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Add_vtable_to_candidate - add "ty" class's vtable to its
// candidates
//
// =============================================================================
void
CLASS_HIERARCHY::Add_vtable_to_candidate(C_STR ty)
{
  CLASS_INFO *info = Get_class_info(ty);
  if(info) {
    info->Add_vtable_to_candidate(info, TRUE);
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Is_vtable - check if symbol is vtable
//
// =============================================================================
BOOL
CLASS_HIERARCHY::Is_vtable(char *st_name)
{
  if(strncmp(st_name, VTABLE_MARK, strlen(VTABLE_MARK)) == 0) {
    return TRUE;
  } else {
    return FALSE;
  }
}

// =============================================================================
//
// CLASS_HIERARCHY::Class_is_interface - check if symbol is interface class
//
// =============================================================================
BOOL
CLASS_HIERARCHY::Class_is_interface(C_STR class_name)
{
  CLASS_INFO *info = Get_class_info(class_name);
  if(info && info->Get_aux_info()) {
    return info->Get_aux_info()->Class_is_interface();
  }
  return FALSE;
}

// =============================================================================
//
// CLASS_HIERARCHY::Print - Dump class hierarchy
//
// =============================================================================
void
CLASS_HIERARCHY::Print(FILE *fp) const
{
  fprintf(fp, "\nDump of class hierarchy [%ld]...\n", _class_info_map.size());
  CLASS_INFO_MAP_CONST_ITER iter;
  for(iter = _class_info_map.begin(); iter != _class_info_map.end(); iter++) {
    fprintf(fp, "\n%s", iter->first);
    CLASS_INFO *info = iter->second;
    info->Print(fp);
  }
}
