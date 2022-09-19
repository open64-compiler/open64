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

#ifndef whirl_file_ctx_h
#define whirl_file_ctx_h "whirl_file_ctx.h"

#include "defs.h"
#include "symtab.h"          // *_TAB
#include "printsrc.h"
#include <ext/hash_map>

class PU;
class SCOPE;
class BE_ST;
class BE_PREG;
class FEEDBACK;
struct BE_SCOPE;
class WHIRL_FILE_INFO;

typedef INT32 WN_MAP;
typedef void* DST_TYPE;
typedef struct pu_info PU_Info;
typedef struct wn_map_tab WN_MAP_TAB;
typedef RELATED_SEGMENTED_ARRAY<BE_ST> BE_ST_TAB;
typedef RELATED_SEGMENTED_ARRAY<BE_PREG> BE_PREG_TAB;
typedef hash_map<ST_IDX, INITO_IDX> ST_INITO_MAP;

struct WHIRL_FILE_CONTEXT
{
  friend class WHIRL_FILE_INFO;
private:
  char *_irb_file_name;
  char *_src_file_name;
  char *_obj_file_name;
  char *_ipsa_file_name;
  //char *_err_file_name;
  //char *_tlog_file_name;
  //char *_trc_file_name;
  //char *_tim_file;
  char *_lst_file_name;
  FILE *_lst_file;
  FILE *_vsa_rpt_file;
  FILE *_vsa_txt_file;

  FILE_INFO_CONTEXT _file_info_ctx;
  LANGUAGE   _language;

  SCOPE      *_scope_tab;
  ST        **_mtype_to_preg_array;
  ST         *_int_preg;
  ST         *_float_preg;
  ST         *_return_val_preg;
  ST         *_branch_preg;
#ifdef TARG_X8664
  ST         *_x87_preg;
#endif
  TY_IDX     *_mtype_to_ty_array;
  BE_SCOPE   *_be_scope_tab;
  FILE_INFO  *_file_info;
  PU_TAB     *_pu_tab;
  TY_TAB     *_ty_tab;
  FLD_TAB    *_fld_tab;
  TYLIST_TAB *_tylist_tab;
  ARB_TAB    *_arb_tab;
  TCON_TAB   *_tcon_tab;
  INITV_TAB  *_initv_tab;
  BLK_TAB    *_blk_tab;
  void       *_str_tab;
  void       *_tcon_str_tab;
  void       *_tcon_merge_map;
  DST_TYPE    _DST;
  DST_TYPE    _dst;
  void       *_pu_tree;
  TY_IDX      _char_star_type;
  UINT32      _be_next_level;
  ST_INITO_MAP _st_inito_map;     // st idx -> inito idx
  BOOL        _st_inito_map_inited;

public:
  const FILE_INFO_CONTEXT* Get_file_info() const
  {
    return &_file_info_ctx;
  }
  const ST_INITO_MAP* Get_st_inito_map() const
  {
    return &_st_inito_map;
  }

public:
  void Create_file_context();
  void Save_context();
  void Restore_context() const;
  void Update_context(WHIRL_FILE_INFO* fi, BOOL syslib);
  void Save_be_scope_tab();
  void Destroy_file_context();
}; /* WHIRL_FILE_CONTEXT */

class WHIRL_PU_CONTEXT
{
  friend class DNA_NODE;
private:
  PU           *_current_pu;
  SCOPE        *_scope_tab;
  ST_TAB       *_st_tab;
  LABEL_TAB    *_label_tab;
  PREG_TAB     *_preg_tab;
  INITO_TAB    *_inito_tab;
  ST_ATTR_TAB  *_st_attr_tab;
  BE_ST_TAB    *_be_st_tab;
  BE_PREG_TAB  *_be_preg_tab;
  PU_Info      *_current_pu_info;
  MEM_POOL     *_src_pool;
  MEM_POOL     *_wn_pool;
  MEM_POOL     *_pu_pool;
  WN_MAP        _parent_map;
  WN_MAP        _region_map;
  WN_MAP_TAB   *_map_tab;
  DST_TYPE      _dst;
  FEEDBACK     *_feedback;
  // wopt to extent the class to add comp_unit
  //COMP_UNIT    *_comp_unit;

public:
  void Save_context();
  void Restore_context() const;
}; /* WHIRL_PU_CONTEXT */

#endif /* whirl_file_ctx.h */

