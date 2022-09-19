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

#include "whirl_file_ctx.h"
#include "be_symtab.h"       // Be_scope_tab
#include "pu_info.h"         // Current_PU_Info
#include "wn.h"              // WN_mem_pool_ptr
#include "const.h"           // TCON_...
#include "region_util.h"     // RID_map
#include "fb_whirl.h"        // Cur_PU_Feedback
#include "glob.h"            // Irb...
#include "report.h"          // VsaRpt_File
#include "config_vsa.h"      // VSA_Single_Report
extern void *current_DST;    // hack of current_DST

/*
 * FILE CONTEXT
 */
void
WHIRL_FILE_CONTEXT::Create_file_context()
{
  _irb_file_name = Irb_File_Name;
  _src_file_name = Src_File_Name;
  _obj_file_name = Obj_File_Name;
  _ipsa_file_name = Ipsa_File_Name;
  //_trc_file_name = Trc_File_Name;
  _lst_file_name = Lst_File_Name;
  _lst_file = Lst_File;
  _vsa_rpt_file = VsaRpt_File;
  _vsa_txt_file = VsaTxt_File;
  memset(&_file_info_ctx, 0, sizeof(_file_info_ctx));
  _language = Language;

  // _scope_tab: wait for BE to create it
  _scope_tab = NULL;
  // _mtype_to_preg_array: wait for BE to create it
  _mtype_to_preg_array = NULL;
  // _int_preg, _float_preg, _return_val_preg: wait for BE to create it
  _int_preg = NULL;
  _float_preg = NULL;
  _return_val_preg = NULL;
  _branch_preg = NULL;
#ifdef TARG_X8664
  // _x87_preg: wait for BE to create it
  _x87_preg = NULL;
#endif
  // _mtype_to_ty_array: wait for BE to create it
  _mtype_to_ty_array = NULL;
  // _be_scope_tab: wait for BE to create it
  _be_scope_tab = NULL;
  _file_info = CXX_NEW(FILE_INFO(), Malloc_Mem_Pool);
  _pu_tab = CXX_NEW(PU_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _ty_tab = CXX_NEW(TY_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _fld_tab = CXX_NEW(FLD_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _tylist_tab = CXX_NEW(TYLIST_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _arb_tab = CXX_NEW(ARB_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _tcon_tab = CXX_NEW(TCON_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _initv_tab = CXX_NEW(INITV_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _blk_tab = CXX_NEW(BLK_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  _str_tab = New_Strtab();
  _tcon_merge_map = New_TCON_Merge_Map(Malloc_Mem_Pool);
  _tcon_str_tab = New_TCON_Strtab();
  //_dst: wait for BE to create it
  _DST = NULL;
  _dst = NULL;
  //_pu_tree: wait BE to create it
  _pu_tree = NULL;
  // _char_star_type: wait BE to create it
  _char_star_type = TY_IDX_ZERO;
  //_be_next_level: wait BE to set it
  _be_next_level = 0;
  _st_inito_map_inited = FALSE;
}

void
WHIRL_FILE_CONTEXT::Save_context()
{
  _irb_file_name = Irb_File_Name;
  _src_file_name = Src_File_Name;
  _obj_file_name = Obj_File_Name;
  _ipsa_file_name = Ipsa_File_Name;
  //_trc_file_name = Trc_File_Name;
  _lst_file_name = Lst_File_Name;
  _lst_file = Lst_File;
  // nothing to do with _file_info_ctx
  _language = Language;

  _scope_tab = Scope_tab;
  _mtype_to_preg_array = MTYPE_TO_PREG_array;
  _int_preg = Int_Preg;
  _float_preg = Float_Preg;
  _return_val_preg = Return_Val_Preg;
  _branch_preg = Branch_Preg;
#ifdef TARG_X8664
  _x87_preg = X87_Preg;
#endif
  _mtype_to_ty_array = MTYPE_TO_TY_array;
  _be_scope_tab = Be_scope_tab;
  _file_info = File_info_ptr;
  _pu_tab = Pu_Table_ptr;
  _ty_tab = Ty_tab_ptr;
  _fld_tab = Fld_Table_ptr;
  _tylist_tab = Tylist_Table_ptr;
  _arb_tab = Arb_Table_ptr;
  _tcon_tab = Tcon_Table_ptr;
  _initv_tab = Initv_Table_ptr;
  _blk_tab = Blk_Table_ptr;
  _str_tab = Get_Strtab();
  _tcon_merge_map = Get_TCON_Merge_Map();
  _tcon_str_tab = Get_TCON_Strtab();
  _DST = Current_DST;
  _dst = current_DST;
  _pu_tree = Global_PU_Tree;
  _char_star_type = Char_Star_Type;
  _be_next_level = BE_get_next_level();
  if (!_st_inito_map_inited) {
    // save info about st index to inito index
    for (UINT i = 0; i < INITO_Table_Size(GLOBAL_SYMTAB); i++) {
      INITO *entry = &Inito_Table(GLOBAL_SYMTAB, i);
      _st_inito_map[entry->st_idx] = make_INITO_IDX(i, 1);
    }
    _st_inito_map_inited = TRUE;
  }
}


void
WHIRL_FILE_CONTEXT::Restore_context() const
{
  Irb_File_Name = _irb_file_name;
  Src_File_Name = _src_file_name;
  Obj_File_Name = _obj_file_name;
  Ipsa_File_Name = _ipsa_file_name;
  //Trc_File_Name = _trc_file_name;
  Lst_File_Name = _lst_file_name;
  if (!VSA_Single_Report) {
    VsaRpt_File = _vsa_rpt_file;
    VsaTxt_File = _vsa_txt_file;
  }
  Lst_File = _lst_file;
  Set_file_info_context(&_file_info_ctx);
  Language = _language;

  Scope_tab = _scope_tab;
  MTYPE_TO_PREG_array = _mtype_to_preg_array;
  Int_Preg = _int_preg;
  Float_Preg = _float_preg;
  Return_Val_Preg = _return_val_preg;
  Branch_Preg = _branch_preg;
#ifdef TARG_X8664
  X87_Preg = _x87_preg;
#endif
  MTYPE_TO_TY_array = _mtype_to_ty_array;
  Be_scope_tab = _be_scope_tab;
  File_info_ptr = _file_info;
  Pu_Table_ptr = _pu_tab;
  Ty_tab_ptr = _ty_tab;
  Fld_Table_ptr = _fld_tab;
  Tylist_Table_ptr = _tylist_tab;
  Arb_Table_ptr = _arb_tab;
  Tcon_Table_ptr = _tcon_tab;
  Initv_Table_ptr = _initv_tab;
  Blk_Table_ptr = _blk_tab;
  Set_Strtab(_str_tab);
  Set_TCON_Merge_Map(_tcon_merge_map);
  Set_TCON_Strtab(_tcon_str_tab);
  Current_DST = _DST;
  current_DST = _dst;
  Global_PU_Tree = _pu_tree;
  Char_Star_Type = _char_star_type;
  BE_set_next_level(_be_next_level);
}

void
WHIRL_FILE_CONTEXT::Save_be_scope_tab()
{
  _be_scope_tab = Be_scope_tab;
  _be_next_level = BE_get_next_level();
}

/*
 * PU CONTEXT
 */
//extern COMP_UNIT *g_comp_unit;

void
WHIRL_PU_CONTEXT::Save_context()
{
  _current_pu      = Current_pu;
  _scope_tab       = Scope_tab;
  _st_tab          = Scope_tab[CURRENT_SYMTAB].st_tab;
  _label_tab       = Scope_tab[CURRENT_SYMTAB].label_tab;
  _preg_tab        = Scope_tab[CURRENT_SYMTAB].preg_tab;
  _be_preg_tab     = Be_preg_tab_ptr;
  _inito_tab       = Scope_tab[CURRENT_SYMTAB].inito_tab;
  _st_attr_tab     = Scope_tab[CURRENT_SYMTAB].st_attr_tab;

  _be_st_tab       = Be_scope_tab[CURRENT_SYMTAB].be_st_tab;
  _current_pu_info = Current_PU_Info;
  _src_pool        = MEM_src_pool_ptr;
  _wn_pool         = WN_mem_pool_ptr;
  _pu_pool         = MEM_pu_pool_ptr;
  //_parent_map      = Parent_Map;
  _region_map      = RID_map;
  _map_tab         = Current_Map_Tab;
  _dst             = Current_DST;
  _feedback        = Cur_PU_Feedback;

  //_comp_unit       = g_comp_unit;
}

void
WHIRL_PU_CONTEXT::Restore_context() const
{
  Current_pu       = _current_pu;
  CURRENT_SYMTAB   = PU_lexical_level (*Current_pu);

  Scope_tab        = _scope_tab;
  Scope_tab[CURRENT_SYMTAB].st_tab      = _st_tab;
  Scope_tab[CURRENT_SYMTAB].label_tab   = _label_tab;
  Scope_tab[CURRENT_SYMTAB].preg_tab    = _preg_tab;
  Be_preg_tab_ptr  = _be_preg_tab;
  Scope_tab[CURRENT_SYMTAB].inito_tab   = _inito_tab;
  Scope_tab[CURRENT_SYMTAB].st_attr_tab = _st_attr_tab;

  Be_scope_tab[CURRENT_SYMTAB].be_st_tab= _be_st_tab;

  Current_PU_Info  = _current_pu_info;
  MEM_src_pool_ptr = _src_pool;
  WN_mem_pool_ptr  = _wn_pool;
  MEM_pu_pool_ptr  = _pu_pool;
  //Parent_Map       = _parent_map;
  RID_map          = _region_map;
  Current_Map_Tab  = _map_tab;
  Current_DST      = _dst;
  Cur_PU_Feedback  = _feedback;

  //g_comp_unit      = _comp_unit;
}

