/*
   Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

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

#ifndef symtab_access_global_h
#define symtab_access_global_h "symtab_access_global.h"

#include "whirl_file_mgr.h"
#include "symtab_compatible.h"
#include "symtab_utils.h"
#include "irbdata.h"

typedef uint64_t FILE_ST_IDX;

inline FILE_ST_IDX
File_st_idx(UINT32 file_id, STR_IDX st) { return ((uint64_t)file_id << 32) | (uint64_t)st; }

inline UINT32
FST_file_idx(FILE_ST_IDX st) { return st >> 32; }

inline ST_IDX
FST_st_idx(FILE_ST_IDX st) { return (UINT32)st; }

// STR_IDX to char*
inline char*
Str_ptr(UINT32 file_id, STR_IDX idx)
{
  if (file_id == 0)
    return Index_To_Str(idx);
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Str_ptr(idx);
}

// PU_IDX to PU*
inline PU*
Pu_ptr(UINT32 file_id, PU_IDX pu)
{
  if (file_id == 0)
    return &Pu_Table[pu];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Pu_ptr(pu);
}

// ST_IDX to ST*
inline ST*
St_ptr(UINT32 file_id, ST_IDX st)
{
  Is_True(ST_IDX_level(st) == GLOBAL_SYMTAB,
          ("only work for global symtab"));
  if (file_id == 0)
    return ST_ptr(st);
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).St_ptr(st);
}

// TY_IDX to TY*
inline TY*
Ty_ptr(UINT32 file_id, TY_IDX ty)
{
  if (file_id == 0)
    return &Ty_Table[ty];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Ty_ptr(ty);
}

// FLD_IDX to FLD*
inline FLD*
Fld_ptr(UINT32 file_id, FLD_IDX fld)
{
  if (file_id == 0)
    return &Fld_Table[fld];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Fld_ptr(fld);
}

// TYLIST_IDX to TYLIST*
inline TYLIST*
Tylist_ptr(UINT32 file_id, TYLIST_IDX tylist)
{
  if (file_id == 0)
    return &Tylist_Table[tylist];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Tylist_ptr(tylist);
}

// ARB_IDX to ARB*
inline ARB*
Arb_ptr(UINT32 file_id, ARB_IDX arb)
{
  if (file_id == 0)
    return &Arb_Table[arb];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Arb_ptr(arb);
}

// TCON_IDX to TCON*
inline TCON*
Tcon_ptr(UINT32 file_id, TCON_IDX tcon)
{
  if (file_id == 0)
    return &Tcon_Table[tcon];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Tcon_ptr(tcon);
}

inline char*
Tcon_str_ptr(UINT32 file_id, mUINT32 str)
{
  if (file_id == 0)
    return Index_to_char_array(str);
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Tcon_str_ptr(str);
}

// INITV_IDX to INITV*
inline INITV*
Initv_ptr(UINT32 file_id, INITV_IDX initv)
{
  if (file_id == 0)
    return &Initv_Table[initv];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Initv_ptr(initv);
}

// INITO_IDX to INITO*
inline INITO*
Inito_ptr(UINT32 file_id, INITO_IDX inito)
{
  if (file_id == 0)
    return &Inito_Table[inito];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Inito_ptr(inito);
}

// ST_ATTR_IDX to ST_ATTR*
inline ST_ATTR*
St_attr_ptr(UINT32 file_id, ST_ATTR_IDX st_attr)
{
  if (file_id == 0)
    return &St_Attr_Table[st_attr];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).St_attr_ptr(st_attr);
}

// BLK_IDX to BLK*
inline BLK*
Blk_ptr(INT32 file_id, BLK_IDX blk_idx)
{
  if (file_id == 0)
    return &Blk_Table[blk_idx];
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Blk_ptr(blk_idx);
}

inline char *
STR_idx_str(UINT32 file_id, const STR_IDX str_idx)
{
  if (file_id == 0)
    return Index_To_Str(str_idx);
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Str_ptr(str_idx);
}

// ST_name
inline char *
ST_name(UINT32 file_id, const ST *s)
{
  Is_True(ST_level(s) == GLOBAL_SYMTAB,
          ("only work for global symtab"));
  if (file_id == 0)
    return ST_name(s);
  STR_IDX idx = ST_name_idx(s);
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Str_ptr(idx);
}

// ST_name
inline char *
ST_name(UINT32 file_id, ST_IDX st)
{
  Is_True(ST_IDX_level(st) == GLOBAL_SYMTAB,
          ("only work for global symtab"));
  ST* s = St_ptr(file_id, st);
  return ST_name(file_id, s);
}

inline INT64
ST_size(UINT32 file_id, ST_IDX st)
{
  Is_True(ST_IDX_level(st) == GLOBAL_SYMTAB,
          ("only work for global symtab"));
  ST* s = St_ptr(file_id, st);
  switch (ST_class(s)) {
  case CLASS_VAR:
  case CLASS_PREG:
    return Ty_ptr(file_id, ST_type(s))->size; 

  case CLASS_CONST:
    // TODO
  case CLASS_BLOCK:
    // STB_size;
  case CLASS_FUNC:
    return 0;
  }
  return 0;
}

// find inito for st
struct symtab_find_inito_predicate {
  ST_IDX st_idx;
  symtab_find_inito_predicate(ST_IDX st) : st_idx(st) { }
  BOOL operator()(UINT, const INITO* inito) const {
    return INITO_st_idx(*inito) == st_idx;
  }
};

// ST_inito
inline const INITO*
ST_inito(UINT32 file_id, ST_IDX st)
{
  Is_True(ST_IDX_level(st) == GLOBAL_SYMTAB,
          ("only work for global symtab"));
  INITO_TAB* inito_tab = file_id == 0 ?
      Scope_tab[GLOBAL_SYMTAB].inito_tab :
      WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Inito_tab();
  symtab_find_inito_predicate pred(st);
  INITO_IDX ino = Find_entry_if(*inito_tab, pred);
  Is_True(ino != INITO_IDX_ZERO, ("not find inito"));
  return &(*inito_tab)[ino];
}

// TY_name
inline char *
TY_name(UINT32 file_id, const TY* t)
{
  if (file_id == 0)
    return TY_name(*t);
  STR_IDX idx = TY_name_idx(*t);
  return WHIRL_FILE_MANAGER::Get()->Get_file(file_id).Str_ptr(idx);
}

// TY_name
inline char *
TY_name(UINT32 file_id, TY_IDX ty)
{
  if (file_id == 0)
    return TY_name(ty);
  TY* t = Ty_ptr(file_id, ty);
  return TY_name(file_id, t);
}

// TY_kind
inline TY_KIND
TY_kind(UINT32 file_id, TY_IDX ty)
{
  TY* ptr = Ty_ptr(file_id, ty);
  return ptr->kind;
}

#endif /* symtab_access_global_h */

