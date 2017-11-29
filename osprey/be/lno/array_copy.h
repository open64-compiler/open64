/*

  Copyright (C) 2009, 2011 Advanced Micro Devices, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef array_copy_INCLUDED
#define array_copy_INCLUDED

#include "wn.h"

typedef struct SAC_FLD_INFO_
{
  bool is_read;
  bool is_written;
  int new_fld_id;
  int new_offset;
  ST_IDX st_idx;
} SAC_FLD_INFO;

typedef struct SAC_INFO_
{
  WN* func_entry;
  WN* wn_loop; // wn corresponding to the DO_LOOP
  SAC_FLD_INFO* fld_info;
  TY_IDX orig_ty;
  TY_IDX new_ty;
  int orig_num_fields;
  int new_num_fields;
  int new_struct_size;
  ST* old_stride_sym;
  ST* new_stride_sym;
  WN_OFFSET new_stride_preg;
  WN* array_copy_wn;
  WN* copy_insertion_block;
  WN* copy_insertion_wn;
  WN* saved_start_wn;
  WN* saved_end_wn;
  WN* saved_int_stride;
  WN* saved_struct_stride;
  WN* saved_num_chunks_wn;
  OPCODE end_comp_op;
  bool end_is_kid0;
} SAC_INFO;

extern void Perform_Structure_Array_Copy_Opt(WN* func_node);

void Delete_SAC_Info(SAC_INFO*& sac_info);
void Find_Struct_Array_Copy_Candidate(SAC_INFO*& sac_info,
                                      WN* wn,
                                      bool collect_field_info,
                                      int& depth);

bool Check_Candidate_Legality(WN* wn_tree, SAC_INFO* sac_info);
void Traverse_WN_Tree_For_SAC_Legality(WN* wn, 
                                       SAC_INFO* sac_info, 
                                       bool& is_legal);
bool Routine_Is_Inlined_And_Safe(WN* tree, ST* callee, SAC_INFO* sac_info);
void Check_For_Inlined_Routine_Safety(WN* wn, ST* callee, 
                                      SAC_INFO* sac_info,
                                      bool& is_inlined);

void Collect_Loop_Field_Refs(WN* loop_body, SAC_INFO*& sac_info);
void Check_WN_For_Field_Refs(WN* wn, SAC_INFO*& sac_info);
void Create_New_Struct_Type(SAC_INFO* sac_info);

void Setup_Common_Info(SAC_INFO*& sac_info, WN* copy_block);
void Allocate_Struct_Copy_Array(SAC_INFO* sac_info,
                                WN* insertion_block,
                                WN* insertion_wn);
void Free_Struct_Copy_Array(SAC_INFO* sac_info,
                            WN* insertion_block);

void Insert_Array_Copy_Code(SAC_INFO*& sac_info);
BOOL Find_Insertion_Point(SAC_INFO* sac_info);
void Find_Def_Block(WN* wn, SAC_INFO* sac_info, WN*& def_block, WN*& def_node);
void Copy_Bounds_Defs(WN* expr, SAC_INFO*& sac_info, WN* insertion_block, 
                      WN* insertion_wn, ST* old_sym, ST* new_sym);
WN* Find_Definition(ST* sym, WN* block, WN* stop);
WN* Create_Copy_Loop_Code(SAC_INFO* sac_info, WN* copy_block);
void Do_DU_Update(WN* wn);

void Traverse_WN_Tree_For_Struct_Copy_Opt(SAC_INFO* sac_info);

void Insert_Sync_Copy_Code(SAC_INFO* sac_info);
void Find_Writes_To_Struct_Type(WN* wn, 
                                SAC_INFO* sac_info, 
                                bool found_insertion_pt);
WN* Generate_Copy_Code_For_Write(WN* wn, SAC_INFO* sac_info);

void Walk_And_Replace_Refs(WN* wn, SAC_INFO* sac_info, WN* idx_expr,
                           WN* parent, int kidno);

typedef std::vector<WN *>  WN_VECTOR_SAC;

class SAC
{
private:
   WN* func_node;
public:
  SAC(WN* f_node)
  {
    func_node=f_node;
  }
  struct ALIAS_CHECK
  {
    WN* alias_wn;
    BOOL check_in_loop;
  };
  struct SAC_FLD_INFORM
  {
    bool is_read;
    ST_IDX st_idx;
    int inner_depth;
    WN* inner_field_arr_ofs;  // for base[i].field[j], it's j
    WN* wn_stid; // the new place that store the splited field 
    WN_VECTOR_SAC  cands;
    WN_VECTOR_SAC  ac_cands;
  } ;

  struct SAC_INFORM
  {
    WN* wn_loop; // wn corresponding to the DO_LOOP
    WN* out_loop; // the first level loop
    SAC_FLD_INFORM* fld_info;
    std::vector<ALIAS_CHECK> acs;
    TY_IDX orig_ty;
    int orig_num_fields;
    ST* sym;
    ST* size_st;
    char* size_stname;
    inline BOOL operator() (UINT32 i, const ST* st) const
    {
      return !strcmp(size_stname, ST_name(st));
    }

    inline BOOL find_def_once()
    {
      size_stname = (char*) malloc( strlen(ST_name(sym))+20);
      sprintf(size_stname, "__temp_sac_size_%s", ST_name(sym));
      ST_IDX st_idx = For_all_until(St_Table, GLOBAL_SYMTAB, *this);
      free(size_stname);
      size_stname = NULL;
      if ( st_idx == 0 )
      {
	size_st=NULL;
	return FALSE;
      }else
      {
	size_st = &St_Table[st_idx];
	return TRUE;
      }
    }
    void transform_candidate();
    BOOL check_legality_and_collect_node(WN* , INT32);
    BOOL collect_alias_wn(WN*, WN_VECTOR_SAC&);
  private:
    WN* get_real_field_offset(WN*);
    void replace_splited_field(WN*, ST*, WN*);
    void replace_splited_field(WN*, ST*, WN*, WN*);
    BOOL check_addr_of_ldid(WN*, WN*);
    BOOL check_base_st(WN*);
    INT32 loop_depth(WN*);
    INT32 loop_distant(WN*, WN*);
    BOOL def_by_calloc_or_free(WN*);
    BOOL select_alias_check(WN*, BOOL check_in_loop=FALSE);
    BOOL split_array_element(INT32);
    BOOL check_ty_recur(TY_IDX, TY_IDX);
  };
  std::vector<SAC_INFORM *> sac_vec;
  void find_struct_split_candidate(WN*, WN*, INT32 );
  void collect_splited_field(WN*, WN*);
  static INT32 check_wn_for_field_refs(ST*&, WN*, INT32);
  static WN* get_base_wn(WN*);
  static INT32 get_base_and_offset(WN*, WN*&);
  static void get_base_and_offset(WN*, WN*&, WN*&);
  static BOOL get_base_and_offset(WN*, WN*&, WN*&, WN*&, WN*&);
public:
  void Perform_Structure_Split_Opt(void);
};

typedef std::vector<SAC::SAC_INFORM *> SAC_INFORM_VECTOR;
typedef std::vector<SAC::ALIAS_CHECK> ALIAS_CHECK_VECTOR;

#endif // array_Copy_INCLUDED
