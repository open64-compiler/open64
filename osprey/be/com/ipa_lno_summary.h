/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#ifndef cxx_ipa_lno_summary_INCLUDED
#define cxx_ipa_lno_summary_INCLUDED

#ifndef mtypes_INCLUDED
#include "mtypes.h"
#endif

#ifndef symtab_idx_INCLUDED
#include "symtab_idx.h" 
#endif 

class IPA_LNO_SUMMARY_PROCEDURE {
private:
  ST_IDX _st_idx;
  mINT32 _formal_index;
  mINT32 _global_index; 
  mINT32 _value_index; 
  mINT32 _expr_index : 31; 
  mUINT32 _has_incomplete_array_info : 1;
  mUINT16 _formal_count; 
  mUINT16 _global_count; 
  mUINT16 _value_count; 
  mUINT16 _expr_count; 

public: 
  IPA_LNO_SUMMARY_PROCEDURE() :
    _st_idx (ST_IDX_ZERO),
    _formal_index (-1), 
    _global_index (-1),
    _value_index (-1),
    _expr_index (-1),
    _has_incomplete_array_info (FALSE),
    _formal_count (0),
    _global_count (0),
    _value_count (0),
    _expr_count (0) 
  {}

  ST_IDX St_Idx() const { return _st_idx; }
  INT Formal_Index() const { return _formal_index; }
  INT Global_Index() const { return _global_index; }
  INT Value_Index() const { return _value_index; }
  INT Expr_Index() const { return _expr_index; }
  INT Formal_Count() const { return _formal_count; }
  INT Global_Count() const { return _global_count; }
  INT Value_Count() const { return _value_count; }
  INT Expr_Count() const { return _expr_count; }
  BOOL Has_Incomplete_Array_Info() const 
    { return _has_incomplete_array_info; }
  void Set_St_Idx(ST_IDX v) { _st_idx = v; }
  void Set_Formal_Index(INT v) { _formal_index = v; }
  void Set_Global_Index(INT v) { _global_index = v; }
  void Set_Value_Index(INT v) { _value_index = v; }
  void Set_Expr_Index(INT v) { _expr_index = v; }
  void Set_Formal_Count(INT v) { _formal_count = v; }
  void Set_Global_Count(INT v) { _global_count = v; }
  void Set_Value_Count(INT v) { _value_count = v; }
  void Set_Expr_Count(INT v) { _expr_count = v; }
  void Reset_Has_Incomplete_Array_Info() { _has_incomplete_array_info = FALSE; }
  void Set_Has_Incomplete_Array_Info() { _has_incomplete_array_info = TRUE; }

  void Print(FILE* fp, INT procedure_index = -1); 
}; 

class IPA_LNO_SUMMARY_FORMAL {
private: 
#define IPA_LNO_FORMAL_MUST_KILL	0x00000001
#define IPA_LNO_FORMAL_MAY_KILL		0x00000002
#define IPA_LNO_FORMAL_EXP_USE		0x00000004
#define IPA_LNO_FORMAL_USE		0x00000008
#define IPA_LNO_FORMAL_MUST_REDUC	0x00000010
#define IPA_LNO_FORMAL_MAY_REDUC	0x00000020
#define IPA_LNO_FORMAL_SCALAR		0x00000040
#define IPA_LNO_FORMAL_ARRAY		0x00000080
#define IPA_LNO_FORMAL_UNKNOWN		0x00000100
  mUINT32 _state; 
  mINT32 _position; 
  mINT32 _machine_type;
  mINT32 _mod_array_section_index;
  mINT32 _ref_array_section_index; 
  mINT32 _decl_array_section_index;
public: 
  IPA_LNO_SUMMARY_FORMAL() {_state = 0; _machine_type = MTYPE_UNKNOWN; 
    _position = -1; _mod_array_section_index = -1; 
    _ref_array_section_index = -1;};
  INT Position() const {return _position;};
  INT Machine_Type() const {return _machine_type;};
  INT Mod_Array_Section_Index() const {return _mod_array_section_index;};
  INT Ref_Array_Section_Index() const {return _ref_array_section_index;};
  INT Decl_Array_Section_Index() const {return _decl_array_section_index;};
  void Clear_State() {_state = 0;};
  void Set_Position(INT v) {_position = v;};
  void Set_Machine_Type(TYPE_ID v) {_machine_type = v;};
  void Set_Mod_Array_Section_Index(INT v) {_mod_array_section_index = v;};
  void Set_Ref_Array_Section_Index(INT v) {_ref_array_section_index = v;};
  void Set_Decl_Array_Section_Index(INT v) {_decl_array_section_index = v;};
  // IPA_LNO_FORMAL_MUST_KILL
  BOOL Is_Must_Kill() const {return _state & IPA_LNO_FORMAL_MUST_KILL;}; 
  void Set_Must_Kill() {_state |= IPA_LNO_FORMAL_MUST_KILL;}; 
  void Reset_Must_Kill() {_state &= ~IPA_LNO_FORMAL_MUST_KILL;};
  // IPA_LNO_FORMAL_MAY_KILL
  BOOL Is_May_Kill() const {return _state & IPA_LNO_FORMAL_MAY_KILL;}; 
  void Set_May_Kill() {_state |= IPA_LNO_FORMAL_MAY_KILL;}; 
  void Reset_May_Kill() {_state &= ~IPA_LNO_FORMAL_MAY_KILL;};
  // IPA_LNO_FORMAL_EXP_USE
  BOOL Is_Exp_Use() const {return _state & IPA_LNO_FORMAL_EXP_USE;}; 
  void Set_Exp_Use() {_state |= IPA_LNO_FORMAL_EXP_USE;}; 
  void Reset_Exp_Use() {_state &= ~IPA_LNO_FORMAL_EXP_USE;};
  // IPA_LNO_FORMAL_USE
  BOOL Is_Use() const {return _state & IPA_LNO_FORMAL_USE;}; 
  void Set_Use() {_state |= IPA_LNO_FORMAL_USE;}; 
  void Reset_Use() {_state &= ~IPA_LNO_FORMAL_USE;};
  // IPA_LNO_FORMAL_MUST_REDUC
  BOOL Is_Must_Reduction() const {return _state & IPA_LNO_FORMAL_MUST_REDUC;}; 
  void Set_Must_Reduction() {_state |= IPA_LNO_FORMAL_MUST_REDUC;}; 
  void Reset_Must_Reduction() {_state &= ~IPA_LNO_FORMAL_MUST_REDUC;};
  // IPA_LNO_FORMAL_MAY_REDUC
  BOOL Is_May_Reduction() const {return _state & IPA_LNO_FORMAL_MAY_REDUC;}; 
  void Set_May_Reduction() {_state |= IPA_LNO_FORMAL_MAY_REDUC;}; 
  void Reset_May_Reduction() {_state &= ~IPA_LNO_FORMAL_MAY_REDUC;};
  // IPA_LNO_FORMAL_SCALAR
  BOOL Is_Scalar() const {return _state & IPA_LNO_FORMAL_SCALAR;}; 
  BOOL Is_Array() const {return _state & IPA_LNO_FORMAL_ARRAY;}; 
  BOOL Is_Unknown() const {return _state & IPA_LNO_FORMAL_UNKNOWN;}; 
  void Set_Scalar() {_state |= IPA_LNO_FORMAL_SCALAR;}; 
  void Set_Array() {_state |= IPA_LNO_FORMAL_ARRAY;}; 
  void Set_Unknown() {_state |= IPA_LNO_FORMAL_UNKNOWN;}; 
  void Reset_Scalar() {_state &= ~IPA_LNO_FORMAL_SCALAR;};
  void Reset_Array() {_state &= ~IPA_LNO_FORMAL_ARRAY;};
  void Reset_Unknown() {_state &= ~IPA_LNO_FORMAL_UNKNOWN;};
  void Print(FILE* fp, INT formal_index = -1); 
};

class IPA_LNO_SUMMARY_GLOBAL { 
private:
  ST_IDX _st_idx; 
  mINT32 _mod_array_section_index : 31;
  mUINT32 _may_kill : 1;
  mINT32 _ref_array_section_index : 31; 
  mUINT32 _use : 1;
  
public: 
  IPA_LNO_SUMMARY_GLOBAL (ST_IDX st_idx,
                          INT32 array_mod,
                          INT32 array_ref,
                          BOOL scalar_mod,
                          BOOL scalar_ref) :
    _st_idx (st_idx),
    _mod_array_section_index (array_mod),
    _may_kill (scalar_mod),
    _ref_array_section_index (array_ref),
    _use (scalar_ref) 
  {}
  
  ST_IDX St_Idx() const { return _st_idx; }
  INT Mod_Array_Section_Index() const { return _mod_array_section_index; }
  INT Ref_Array_Section_Index() const { return _ref_array_section_index; }
  BOOL Is_May_Kill() const { return _may_kill; }
  BOOL Is_Use() const { return _use; }

  void Set_St_Idx(INT v) { _st_idx = v; }
  void Set_Mod_Array_Section_Index(INT v) { _mod_array_section_index = v; }
  void Set_Ref_Array_Section_Index(INT v) { _ref_array_section_index = v; }
  void Set_May_Kill() { _may_kill = TRUE; }
  void Set_Use() { _use = TRUE; }
  
  BOOL Is_Scalar() const { return TY_kind(ST_type(_st_idx)) != KIND_ARRAY; }
  
  void Print(FILE* fp, INT common_index = -1); 
};

#endif // cxx_ipa_lno_summary_INCLUDED
