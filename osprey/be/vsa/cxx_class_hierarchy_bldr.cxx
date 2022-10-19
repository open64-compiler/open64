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

#include "cxx_class_hierarchy_bldr.h"

#include <assert.h>
#include "wn_util.h"
#include "wn_core.h"
#include "irbdata.h"


// =============================================================================
//
// CXX_CLASS_HIERARCHY_BUILDER::Build_class_info - build cxx class info by 
// traverse inito table
//
// =============================================================================
void CXX_CLASS_HIERARCHY_BUILDER::Build_class_info()
{
  for (UINT i = 1; i < INITO_Table_Size(GLOBAL_SYMTAB); ++i) {
    INITO* ino = &Inito_Table(GLOBAL_SYMTAB,i);
    ST *st_entry = &St_Table[ino->st_idx];
    if(ST_is_vtable(st_entry)) {
      TY_IDX class_ty = ST_vtable_ty_idx( st_entry);
      C_STR class_name = TY_name(class_ty);
      Add_vtable_method(ino, class_ty);
      TY &ty = Ty_Table[class_ty];
      if (Is_Structure_Type(ty) && !TY_is_union(ty) && ty.Fld() > 0) {
        // find every base class
        FLD_IDX fld_idx = ty.Fld();
        do {
          FLD_HANDLE fld(fld_idx);
          if (FLD_is_base_class(fld)) {
            C_STR base_name = TY_name(FLD_type(fld));
            Add_parent(Copy_ty_name(class_name), Copy_ty_name(base_name));
          }
          if (FLD_last_field(fld))
            break;
          fld_idx++;
        } while (1);
      }
    }
    if(ST_is_rtti(st_entry)) {
      TY_IDX class_ty = ST_rtti_ty_idx(*st_entry);
      Is_True(class_ty != TY_IDX_ZERO, ("rtti didn't set class ty"));
      if(class_ty == TY_IDX_ZERO) {
        continue;
      }
      // only process rtti class with no virtual function
      // rule ERR54-CPP need class hierarchy info for classes with no virtual function
      if(TY_kind(class_ty) != KIND_STRUCT) {
        continue;
      }

      TY_IDX st_entry_ty = ST_type(st_entry);
      C_STR type_name = TY_name(st_entry_ty);
      // read base class from inito
      BOOL is_class = FALSE;
      BOOL is_si = FALSE;
      BOOL is_vmi = FALSE;
      if(strncmp(type_name, SI_TI_PREFIX, strlen(SI_TI_PREFIX)) == 0) {
        is_si = TRUE;
      } else if(strncmp(type_name, VMI_TI_PREFIX, strlen(VMI_TI_PREFIX)) == 0) {
        is_vmi = TRUE;
      } else {
        is_class = TRUE;
        Is_True(strncmp(type_name, CLASS_TI_PREFIX, strlen(CLASS_TI_PREFIX)) == 0,
                ("invalid rtti type name"));
      }
      // skip class type, no base
      if(is_class) {
        continue;
      }
      INITV_ENTRIES rtti_entries;
      Get_initv_entry(ino->val, &rtti_entries);
      int rtti_size = rtti_entries.Size();
      C_STR class_name = Copy_ty_name(TY_name(class_ty));
      if(is_si) {
        Is_True(rtti_size == BASE_TYPE_INFO_SYM + 1, ("invalid si rtti inito"));
        if(rtti_size == BASE_TYPE_INFO_SYM + 1) {
          if(rtti_entries.Get_kind(BASE_TYPE_INFO_SYM) == INITVKIND_SYMOFF) {
            ST_IDX base_st = rtti_entries.Get_initv_st(BASE_TYPE_INFO_SYM);
            if(base_st != ST_IDX_ZERO && ST_is_rtti(ST_ptr(base_st))) {
              TY_IDX base_type = ST_rtti_ty_idx(base_st);
              Add_parent(class_name, Copy_ty_name(TY_name(base_type)));
            }
          }
        }
      } else if (is_vmi) {
        Is_True(rtti_size > VMI_BASE_CNT, ("invalid vmi rtti inito"));
        if(rtti_size > VMI_BASE_CNT) {
          UINT32 base_cnt_kind = rtti_entries.Get_kind(VMI_BASE_CNT);
          int base_cnt = rtti_entries.Get_initv_i32(VMI_BASE_CNT);
          int exp_size = VMI_BASE_CNT + base_cnt * 2 + 1;
          Is_True(Is_initv_integer(base_cnt_kind) &&
                  rtti_size == exp_size, ("invalid vmi base cnt"));
          if(Is_initv_integer(base_cnt_kind) && rtti_size == exp_size) {
            int base_initv_off = VMI_BASE_TYPE_SYM_BEGIN;
            for(int idx = 0; idx < base_cnt; idx++) {
              if(rtti_entries.Get_kind(base_initv_off) == INITVKIND_SYMOFF) {
                ST_IDX base_st = rtti_entries.Get_initv_st(base_initv_off);
                if(base_st != ST_IDX_ZERO && ST_is_rtti(ST_ptr(base_st))) {
                  TY_IDX base_type = ST_rtti_ty_idx(base_st);
                  Add_parent(class_name, Copy_ty_name(TY_name(base_type)));
                }
              }
              base_initv_off += 2; // VMI_BASE_TYPE_SYM and VMI_BASE_TYPE_OFF
            }
          }
        }
      } // is_si
    } // ST_is_rtti
  }
}


// =============================================================================
//
// CXX_CLASS_HIERARCHY_BUILDER::Add_vtable_method - add virtual method to class
// info
//
// =============================================================================
void CXX_CLASS_HIERARCHY_BUILDER::Add_vtable_method(INITO *entry, TY_IDX idx)
{
  assert(Is_vtable(ST_name(entry->st_idx)));
  INITV_ENTRIES vtable_entries;
  Get_initv_entry(entry->val, &vtable_entries);

  int top_offset = 0;
  int call_offset = 0;
  int vptr_offset = 0;
  C_STR class_name = TY_name(idx);
  for(int i = 0 ; i < vtable_entries.Size(); i++) {
    UINT32 entry_kind = vtable_entries.Get_kind(i);
    ST_IDX value = vtable_entries.Get_initv_st(i);
    if(vtable_entries.Get_kind(i) == INITVKIND_SYMOFF && value != ST_IDX_ZERO &&
       ST_class(value) == CLASS_FUNC ) {
      if(!ST_is_pure_vfunc(&St_Table[value])) {
        Is_True(vptr_offset >= 0 && vptr_offset < TY_size(ST_vtable_ty_idx(entry->st_idx)),
                ("bad vptr offset"));
        Add_method(idx, i * Pointer_Size, call_offset, value, vptr_offset);
      }
      call_offset = call_offset + Pointer_Size;
    } else if(Is_initv_val(entry_kind)) {
      call_offset = 0;
      vptr_offset = - vtable_entries.Get_initv_i32(i);  // offset in vtable is negative
    }
  }
  Add_vtable_to_candidate(class_name);
}
