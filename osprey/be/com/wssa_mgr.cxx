/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

  Open64 is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA  02110-1301, USA.

*/

//====================================================================
//
// Module: wssa_mgr.cxx
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Implementation of WHIRL SSA MANAGER
//
// Exported classes:
//  WSSA::WHIRL_SSA_MANAGER
//
// SEE ALSO:
//  be/com/wssa_mgr.h
//  be/com/wssa_core.h (PHI_NODE, CHI_NODE, MU_NODE)
//  be/com/wssa_sym.h  (WST_Symbol_Entry, WST_Version_Entry)
//
// ====================================================================

#include <stdio.h>
#include <stdarg.h>
#include "wssa_utils.h"
#include "wssa_mgr.h"
#include "wssa_wn.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_lower.h"
#include "symtab.h"
#include "opt_points_to.h"

namespace WSSA {

//====================================================================
// WSSA constructor/destructor/common routines
//====================================================================

WHIRL_SSA_MANAGER::WHIRL_SSA_MANAGER(MEM_POOL* mpool)
  : _root(NULL), _ssa_pool(mpool) {
  _stat = STAT_INIT;        // internal status
}

WHIRL_SSA_MANAGER::~WHIRL_SSA_MANAGER() {
}

void
WHIRL_SSA_MANAGER::Clear() {
  _stat = STAT_INIT;
  // clear ssa tables and maps
  _phi_table.clear();
  _chi_table.clear();
  _mu_table.clear();

  _phi_map.clear();
  _chi_map.clear();
  _mu_map.clear();
  _wn_ver_map.clear();

  _ver_table.clear();

  // clear sym tables
  _field_table.clear();
  _vsym_table.clear();
  _sym_table.clear();

  // clear wn idx map
  _wn_idx_map.clear();

  // reset default vsym
  _ret_vsym_wst = WST_INVALID;
  _default_vsym_wst = WST_INVALID;

  // reset the status
  _stat = STAT_INIT;
}

//===================================================================
// Specialized version to get TABLEs and MAPs
//   template parameter:
//     WSSA_NODE_KIND: WSSA_PHI, CHI or MU
//===================================================================
template<>
PHI_TABLE& WHIRL_SSA_MANAGER::Get_table<WSSA_PHI>() {
  return _phi_table;
}

template<>
const PHI_TABLE& WHIRL_SSA_MANAGER::Get_table<WSSA_PHI>() const {
  return _phi_table;
}

template<>
CHI_TABLE& WHIRL_SSA_MANAGER::Get_table<WSSA_CHI>() {
  return _chi_table;
}

template<>
const CHI_TABLE& WHIRL_SSA_MANAGER::Get_table<WSSA_CHI>() const {
  return _chi_table;
}

template<>
MU_TABLE& WHIRL_SSA_MANAGER::Get_table<WSSA_MU>() {
  return _mu_table;
}

template<>
const MU_TABLE& WHIRL_SSA_MANAGER::Get_table<WSSA_MU>() const {
  return _mu_table;
}

template<>
WN_PHI_MAP& WHIRL_SSA_MANAGER::Get_map<WSSA_PHI>() {
  return _phi_map;
}

template<>
const WN_PHI_MAP& WHIRL_SSA_MANAGER::Get_map<WSSA_PHI>() const {
  return _phi_map;
}

template<>
WN_CHI_MAP& WHIRL_SSA_MANAGER::Get_map<WSSA_CHI>() {
  return _chi_map;
}

template<>
const WN_CHI_MAP& WHIRL_SSA_MANAGER::Get_map<WSSA_CHI>() const {
  return _chi_map;
}

template<>
WN_MU_MAP& WHIRL_SSA_MANAGER::Get_map<WSSA_MU>() {
  return _mu_map;
}

template<>
const WN_MU_MAP& WHIRL_SSA_MANAGER::Get_map<WSSA_MU>() const {
  return _mu_map;
}

//====================================================================
// WSSA node operations
//  Create_phi, Create_chi, Create_mu
//  Add_phi, Add_chi, Add_mu
//  WN_has_phi, WN_has_chi, WN_has_mu
//  WN_phi_node, WN_chi_node, WN_mu_node
//====================================================================
PHI_NODE*
WHIRL_SSA_MANAGER::Create_phi(INT32 opnd_num) {
  return Create_node<WSSA_PHI>(opnd_num);;
}

CHI_NODE*
WHIRL_SSA_MANAGER::Create_chi() {
  return Create_node<WSSA_CHI>();
}

MU_NODE*
WHIRL_SSA_MANAGER::Create_mu() {
  return Create_node<WSSA_MU>();
}

WSSA_NODE_IDX
WHIRL_SSA_MANAGER::Add_phi(const WN* wn, PHI_NODE* phi) {
  return Add_node(wn, phi);
}

WSSA_NODE_IDX
WHIRL_SSA_MANAGER::Add_chi(const WN* wn, CHI_NODE* chi) {
  return Add_node(wn, chi);
}

WSSA_NODE_IDX
WHIRL_SSA_MANAGER::Add_mu(const WN* wn, MU_NODE* mu) {
  return Add_node(wn, mu);
}

BOOL
WHIRL_SSA_MANAGER::WN_has_phi(const WN* wn) const {
  return WN_has_node<WSSA_PHI>(wn);
}

BOOL
WHIRL_SSA_MANAGER::WN_has_chi(const WN* wn) const {
  return WN_has_node<WSSA_CHI>(wn);
}

BOOL
WHIRL_SSA_MANAGER::WN_has_mu(const WN* wn) const {
  return WN_has_node<WSSA_MU>(wn);
}

const PHI_NODE*
WHIRL_SSA_MANAGER::WN_phi_node(const WN* wn, WST_IDX wst_idx) const {
  return WN_ssa_node<WSSA_PHI>(wn, wst_idx);
}

const CHI_NODE*
WHIRL_SSA_MANAGER::WN_chi_node(const WN* wn, WST_IDX wst_idx) const {
  return WN_ssa_node<WSSA_CHI>(wn, wst_idx);
}

const MU_NODE*
WHIRL_SSA_MANAGER::WN_mu_node(const WN* wn, WST_IDX wst_idx) const {
  return WN_ssa_node<WSSA_MU>(wn, wst_idx);
}

PHI_NODE*
WHIRL_SSA_MANAGER::WN_phi_node(const WN* wn, WST_IDX wst_idx) {
  return WN_ssa_node<WSSA_PHI>(wn, wst_idx);
}

CHI_NODE*
WHIRL_SSA_MANAGER::WN_chi_node(const WN* wn, WST_IDX wst_idx) {
  return WN_ssa_node<WSSA_CHI>(wn, wst_idx);
}

MU_NODE*
WHIRL_SSA_MANAGER::WN_mu_node(const WN* wn, WST_IDX wst_idx) {
  return WN_ssa_node<WSSA_MU>(wn, wst_idx);
}

//====================================================================
// WSSA node iterators
//  phi_iterator, chi_iterator, mu_iterator
//  const_phi_iterator, const_chi_iterator, const_mu_iterator
//====================================================================
WHIRL_SSA_MANAGER::phi_iterator
WHIRL_SSA_MANAGER::WN_phi_begin(const WN* wn) {
  return WN_node_begin<PHI_NODE>(wn);
}

WHIRL_SSA_MANAGER::phi_iterator
WHIRL_SSA_MANAGER::WN_phi_end(const WN* wn) {
  return phi_iterator(this, INVALID_IDX);
}

WHIRL_SSA_MANAGER::chi_iterator
WHIRL_SSA_MANAGER::WN_chi_begin(const WN* wn) {
  return WN_node_begin<CHI_NODE>(wn);
}

WHIRL_SSA_MANAGER::chi_iterator
WHIRL_SSA_MANAGER::WN_chi_end(const WN* wn) {
  return chi_iterator(this, INVALID_IDX);
}

WHIRL_SSA_MANAGER::mu_iterator
WHIRL_SSA_MANAGER::WN_mu_begin(const WN* wn) {
  return WN_node_begin<MU_NODE>(wn);
}

WHIRL_SSA_MANAGER::mu_iterator
WHIRL_SSA_MANAGER::WN_mu_end(const WN* wn) {
  return mu_iterator(this, INVALID_IDX);
}

WHIRL_SSA_MANAGER::const_phi_iterator
WHIRL_SSA_MANAGER::WN_phi_begin(const WN* wn) const {
  return WN_node_begin<PHI_NODE>(wn);
}

WHIRL_SSA_MANAGER::const_phi_iterator
WHIRL_SSA_MANAGER::WN_phi_end(const WN* wn) const {
  return const_phi_iterator(this, INVALID_IDX);
}

WHIRL_SSA_MANAGER::const_chi_iterator
WHIRL_SSA_MANAGER::WN_chi_begin(const WN* wn) const {
  return WN_node_begin<CHI_NODE>(wn);
}

WHIRL_SSA_MANAGER::const_chi_iterator
WHIRL_SSA_MANAGER::WN_chi_end(const WN* wn) const {
  return const_chi_iterator(this, INVALID_IDX);
}

WHIRL_SSA_MANAGER::const_mu_iterator
WHIRL_SSA_MANAGER::WN_mu_begin(const WN* wn) const {
  return WN_node_begin<MU_NODE>(wn);
}

WHIRL_SSA_MANAGER::const_mu_iterator
WHIRL_SSA_MANAGER::WN_mu_end(const WN* wn) const {
  return const_mu_iterator(this, INVALID_IDX);
}

//====================================================================
// Get the iterator to traverse U-D chain
//   WN_ud_begin: return the iterator at the beginning
//   WN_ud_end: return the iterator at the end
//====================================================================
WSSA_UD_ITERATOR
WHIRL_SSA_MANAGER::WN_ud_begin(WN* wn) const {
  return WSSA_UD_ITERATOR(this, wn);
}

WSSA_UD_ITERATOR
WHIRL_SSA_MANAGER::WN_ud_end(WN* wn) const {
  return WSSA_UD_ITERATOR(this, NULL);
}

//====================================================================
// Get the number of the ssa nodes
//   Phi_count: return the count of phi nodes
//   Chi_count: return the count of chi nodes
//   Mu_count:  return the count of mu  nodes
//====================================================================
UINT32
WHIRL_SSA_MANAGER::Phi_count() const {
  return _phi_table.size();
}

UINT32
WHIRL_SSA_MANAGER::Chi_count() const {
  return _chi_table.size();
}

UINT32
WHIRL_SSA_MANAGER::Mu_count() const {
  return _mu_table.size();
}

//====================================================================
// WN version operations
//   WN_has_ver: return TRUE if wn already has version
//   Get_wn_ver: get version info attached to the WN
//   Set_wn_ver: attach the version into to the WN
//   Get_wn_ver_wst: get WST index attached to the WN
//   Get_wn_ver_num: get version number attached to the WN
//====================================================================
BOOL
WHIRL_SSA_MANAGER::WN_has_ver(const WN* wn) const {
  if (WN_map_id(wn) == -1)
    return FALSE;
  if (WSSA::WN_has_ver(wn) &&
      _wn_ver_map.find(WN_idx(wn)) != _wn_ver_map.end()) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}

VER_IDX
WHIRL_SSA_MANAGER::Get_wn_ver(const WN* wn) const {
  WN_VER_MAP::const_iterator it = _wn_ver_map.find(WN_idx(wn));
  Is_True(it != _wn_ver_map.end(), ("Can not find wn_ver"));
  return it->second;
}

void
WHIRL_SSA_MANAGER::Set_wn_ver(const WN* wn, VER_IDX ver) {
  WN_MAP_Set_ID(Current_Map_Tab, const_cast<WN*>(wn));
  WN_VER_MAP::const_iterator it = _wn_ver_map.find( WN_idx(wn) );
  //Is_True(it == _wn_ver_map.end(), ("WN already has a ver"));
  _wn_ver_map[WN_idx(wn)] = ver;
}

WST_IDX
WHIRL_SSA_MANAGER::Get_wn_ver_wst(const WN* wn) const {
  VER_IDX ver = Get_wn_ver(wn);
  Is_True(ver >=0 && ver < _ver_table.size(), ("ver_idx out of bounds"));
  return Get_ver_wst(ver);
}

UINT32
WHIRL_SSA_MANAGER::Get_wn_ver_num(const WN* wn) const {
  VER_IDX ver = Get_wn_ver(wn);
  Is_True(ver >=0 && ver < _ver_table.size(), ("ver_idx out of bounds"));
  return Get_ver_num(ver);
}

//====================================================================
// WN index operations
//  WN_idx: return the wn_idx combine the MAPCAT and map_id
//  Add_wn: Add WN to the hashtable, key is the wn_idx
//  Get_wn: Get the WN by the wn_idx
//  Clear_wn_map(): clear the wn_idx_map to release memory
//====================================================================
WSSA_WN_IDX
WHIRL_SSA_MANAGER::WN_idx(const WN* wn) const {
  // OPERATOR_MAPCAT: 3bit
  // MAP_ID: 29bit (it's less than the map_id in WN, which is 30bit
  FmtAssert(WN_map_id(wn) != -1, ("map_id is not initialized"));
  FmtAssert(WN_map_id(wn) < (1<<29), ("Fix me, map_id is more than 29bit"));
  FmtAssert(OPCODE_mapcat(WN_opcode(wn)) < (1<<3), ("Fix me, map_cat is more than 3bit"));
  WSSA_WN_IDX idx = OPCODE_mapcat(WN_opcode(wn)) << 29 | WN_map_id(wn);
  return idx;
}

void
WHIRL_SSA_MANAGER::Add_wn(WN* wn) {
  OPERATOR opr = WN_operator(wn);
  if (OPERATOR_is_stmt(opr) || OPERATOR_is_scf(opr)) {
    WSSA_WN_IDX idx = WN_idx(wn);
    _wn_idx_map[idx] = wn;
  }
}

WN*
WHIRL_SSA_MANAGER::Get_wn(WSSA_WN_IDX wn_idx) const {
  WN_IDX_MAP::const_iterator it = _wn_idx_map.find(wn_idx);
  if (it != _wn_idx_map.end())
    return it->second;
  else {
    FmtAssert(FALSE, ("can not find wn by wn_idx"));
    return NULL;
  }
}

void
WHIRL_SSA_MANAGER::Clear_wn_map() {
  _wn_idx_map.clear();
}

//====================================================================
// WSSA symbol operations
//  Add_wst_to_map: add wst to st -> wst map
//  Find_wst: find the wst
//====================================================================
void
WHIRL_SSA_MANAGER::Add_wst_to_map(ST_TO_WST_MAP& map, ST_IDX st_idx, WST_IDX wst_idx) {
  WST_Symbol_Entry& sym = _sym_table[wst_idx];
  ST_TO_WST_MAP::iterator it = map.find(st_idx);
  if (it != map.end()) {
#ifdef Is_True_On
    const WST_Symbol_Entry& prev_sym = Get_wst(it->second);
    // verify sym and prev_sym
    Is_True(prev_sym.St_idx() == st_idx, ("prev st_idx mismatch"));
    Is_True(sym.St_idx() == st_idx, ("sym st_idx mismatch"));
    Is_True(prev_sym.Sym_type() == sym.Sym_type(), ("sym type mismatch"));
#endif
    sym.Set_next_wst(it->second);
  }
  else {
    Is_True(sym.Next_wst() == WST_INVALID, ("Next_wst is wrong"));
  }
  map[st_idx] = wst_idx;
}

WST_IDX
WHIRL_SSA_MANAGER::Find_wst(ST* st, PREG_NUM preg_num) {
  Is_True(st != NULL && ST_class(st) == CLASS_PREG,
          ("st is not PREG"));
  ST_TO_WST_MAP::iterator it = _st_to_rwst_map.find(ST_st_idx(st));
  if (it != _st_to_rwst_map.end()) {
    WST_IDX idx = it->second;
    do {
      const WST_Symbol_Entry& sym = Get_wst(idx);
      Is_True(sym.St_idx() == ST_st_idx(st), ("ST_idx mismatch"));
      Is_True(sym.Sym_type() == WST_PREG, ("WST type mismatch"));
      if (sym.Preg_num() == preg_num) {
        return idx;
      }
      idx = sym.Next_wst();
    } while (idx != WST_INVALID);
  }
  return WST_INVALID;
}

WST_IDX
WHIRL_SSA_MANAGER::Find_wst(ST* st, INT64 byte_ofst, INT64 byte_size,
                            UINT8 bit_ofst, UINT8 bit_size) {
  Is_True(st != NULL && ST_class(st) != CLASS_PREG,
          ("st can not be PREG"));
  ST_TO_WST_MAP::iterator it = _st_to_rwst_map.find(ST_st_idx(st));
  if (it != _st_to_rwst_map.end()) {
    WST_IDX idx = it->second;
    do {
      const WST_Symbol_Entry& sym = Get_wst(idx);
      Is_True(sym.St_idx() == ST_st_idx(st), ("ST_idx mismatch"));
      Is_True(sym.Sym_type() == WST_FIELD, ("WST type mismatch"));
      const WST_Field_Info& field = Get_field(sym.Field_idx());
      if (field.Byte_offset() == byte_ofst &&
          field.Byte_size() == byte_size &&
          field.Bit_offset() == bit_ofst &&
          field.Bit_size() == bit_size) {
        return idx;
      }
      idx = sym.Next_wst();
    } while (idx != WST_INVALID);
  }
  return WST_INVALID;
}

WST_IDX
WHIRL_SSA_MANAGER::Find_wst(OPERATOR opr, ST* base, 
                            INT64 byte_ofst, INT64 byte_size,
                            UINT8 bit_ofst, UINT8 bit_size) {
  FmtAssert(FALSE, ("TODO"));
}

//====================================================================
//  New_wst: create new WSSA symbol
//  New_field_info: Create field info for struct field symbol
//  New_vsym_info: Create vsym info for virtual symbol
//====================================================================
WST_IDX
WHIRL_SSA_MANAGER::New_wst(ST* st) {
  FmtAssert(FALSE, ("TODO: convert into FIELD?"));
  Is_True(ST_class(st) == CLASS_VAR, ("Only used for VAR ST"));
  WST_IDX wst = (WST_IDX)_sym_table.size();
  _sym_table.push_back(WST_Symbol_Entry());
  WST_Symbol_Entry& sym = _sym_table.back();
  sym.Set_sym_type(WST_WHIRL);
  sym.Set_st_idx(ST_st_idx(st));
  // update st_to_rwst_map
  Add_wst_to_map(_st_to_rwst_map, ST_st_idx(st), wst);
  return wst;
}

WST_IDX
WHIRL_SSA_MANAGER::New_wst(ST* st, PREG_NUM preg) {
  Is_True(ST_class(st) == CLASS_PREG, ("Only used for PREG ST"));
  WST_IDX wst = Find_wst(st, preg);
  if (wst != WST_INVALID)
    return wst;
  wst = (WST_IDX)_sym_table.size();
  _sym_table.push_back(WST_Symbol_Entry());
  WST_Symbol_Entry& sym = _sym_table.back();
  sym.Set_sym_type(WST_PREG);
  sym.Set_st_idx(ST_st_idx(st));
  sym.Set_preg_num(preg);
  // update st_to_rwst_map
  Add_wst_to_map(_st_to_rwst_map, ST_st_idx(st), wst);
  return wst;
}

WST_IDX
WHIRL_SSA_MANAGER::New_wst(ST* st, const WST_Field_Info& field) {
  Is_True(st != NULL, ("ST can not be null"));
  WST_IDX wst = Find_wst(st, field.Byte_offset(), field.Byte_size(),
                         field.Bit_offset(), field.Bit_size());
  if (wst != WST_INVALID)
    return wst;
  FIELD_INFO_IDX info_idx = New_field(field);
  wst = (WST_IDX)_sym_table.size();
  _sym_table.push_back(WST_Symbol_Entry());
  WST_Symbol_Entry& sym = _sym_table.back();
  sym.Set_sym_type(WST_FIELD);
  sym.Set_st_idx(ST_st_idx(st));
  sym.Set_field_idx(info_idx);
  // update st_to_rwst_map
  Add_wst_to_map(_st_to_rwst_map, ST_st_idx(st), wst);
  return wst;
}

WST_IDX
WHIRL_SSA_MANAGER::New_wst(ST* st, const WST_Vsym_Info& vsym) {
  VSYM_INFO_IDX info_idx = New_vsym(vsym);
  WST_IDX wst = (WST_IDX)_sym_table.size();
  _sym_table.push_back(WST_Symbol_Entry());
  WST_Symbol_Entry& sym = _sym_table.back();
  sym.Set_sym_type(WST_VSYM);
  sym.Set_st_idx(st == NULL ? ST_INVALID : ST_st_idx(st));
  sym.Set_vsym_idx(info_idx);
  // update st_to_vwst_map
  Add_wst_to_map(_st_to_vwst_map, 
                 st == NULL ? ST_INVALID : ST_st_idx(st), wst);
  return wst;
}

FIELD_INFO_IDX 
WHIRL_SSA_MANAGER::New_field(const WST_Field_Info& field_info) {
  FIELD_INFO_IDX idx = _field_table.size();
  _field_table.push_back(field_info);
  return idx;
} 

VSYM_INFO_IDX
WHIRL_SSA_MANAGER::New_vsym(const WST_Vsym_Info& vsym_info) {
  VSYM_INFO_IDX idx = _vsym_table.size();
  _vsym_table.push_back(vsym_info);
  return idx;
}

//====================================================================
// Get information for WSSA symbols
//  Get_wst: return WST_Symbol_Entry for the wst index
//  Get_wst_type: return the WST TYPE for the wst index
//  Get_st: return the ST for the wst index
//  Get_st_idx: return the ST_IDX for the wst_idx
//  Get_field: return the WST_Field_Info for the field index
//  Get_vsym: return the WST_Vsym_Info fot the vsym index
//  WST_name: return the name of the wst
//====================================================================
const WST_Symbol_Entry& 
WHIRL_SSA_MANAGER::Get_wst(WST_IDX wst_idx) const {
  Is_True(wst_idx >= 0 && wst_idx < _sym_table.size(), ("wst_idx out of bounds"));
  return _sym_table[wst_idx];
}

WSSA_SYM_TYPE
WHIRL_SSA_MANAGER::Get_wst_type(WST_IDX st_idx) const {
  const WST_Symbol_Entry& sym = Get_wst(st_idx);
  return sym.Sym_type();
}

ST_IDX
WHIRL_SSA_MANAGER::Get_st_idx(WST_IDX wst_idx) const {
  const WST_Symbol_Entry& sym = Get_wst(wst_idx);
  return sym.St_idx();
}

ST*
WHIRL_SSA_MANAGER::Get_st(WST_IDX wst_idx) const {
  const WST_Symbol_Entry& sym = Get_wst(wst_idx);
  ST_IDX st_idx = 0;
  switch (sym.Sym_type()) {
  case WST_WHIRL:
  case WST_PREG:
  case WST_FIELD:
  case WST_VSYM:
    st_idx = sym.St_idx();
    break;
  default:
    FmtAssert(FALSE, ("bad sym type for Get_WHIRL_ST"));  
  }
  return &(St_Table[st_idx]);
}

const WST_Field_Info&
WHIRL_SSA_MANAGER::Get_field(FIELD_INFO_IDX idx) const {
  Is_True(idx >= 0 && idx < _field_table.size(), ("field idx out of bounds"));
  return _field_table[idx];
}

const WST_Vsym_Info&
WHIRL_SSA_MANAGER::Get_vsym(VSYM_INFO_IDX idx) const {
  Is_True(idx >= 0 && idx < _vsym_table.size(), ("vsym idx out of bounds"));
  return _vsym_table[idx];
}

const char*
WHIRL_SSA_MANAGER::WST_name(WST_IDX idx) const {
  const WST_Symbol_Entry& sym = Get_wst(idx);
  STR_IDX str_idx = 0;
  switch (sym.Sym_type()) {
  case WST_WHIRL:
  case WST_PREG:
    str_idx = ST_name_idx(St_Table[sym.St_idx()]);
    break;
  case WST_FIELD:
    str_idx = Get_field(sym.Field_idx()).Name_idx();
    break;
  case WST_VSYM:
    str_idx = Get_vsym(sym.Vsym_idx()).Name_idx();
    break;
  default:
    FmtAssert(FALSE, ("bad wssa symbol type"));
    return NULL;
  }
  return (const char*)(&(Str_Table[str_idx]));
}

//====================================================================
// const iterators for traversing the WSSA symbol table
//====================================================================
WHIRL_SSA_MANAGER::const_sym_iterator
WHIRL_SSA_MANAGER::Sym_begin() const {
  return _sym_table.begin();
}

WHIRL_SSA_MANAGER::const_sym_iterator
WHIRL_SSA_MANAGER::Sym_end() const {
  return _sym_table.end();
}

//====================================================================
// return the count of wssa symbols
//====================================================================
UINT32
WHIRL_SSA_MANAGER::WST_count() const {
  return _sym_table.size();
}

//====================================================================
// Version operations
//   New_ver: create new WST_Version_Entry
//   Update_ver: update the existing WST_Version_Entry
//   Update_ver_num: update the number of existing WST_Version_Entry
//   Get_ver: get the version info by ver_idx
//   Get_ver_wst: get the wst_idx for the ver_idx
//   Get_ver_num: get the version number for the ver_idx
//====================================================================
VER_IDX
WHIRL_SSA_MANAGER::New_ver(const WST_Version_Entry& ver) {
  // update max_ver for PREG since the max_ver is updated after constructing HSSA
  WST_Symbol_Entry& wst = _sym_table[ver.Get_wst()];
  if (wst.Max_ver() < ver.Get_ver()) {
    wst.Set_max_ver(ver.Get_ver());
  }
  VER_IDX idx = (VER_IDX)_ver_table.size();
  _ver_table.push_back(ver);

  // update def chain
  if (ver.Get_def_wn() != NULL) {
    VER_IDX last_ver = wst.Last_ver();
    _ver_table[idx].Set_prev_ver(last_ver);
    wst.Set_last_ver(idx);
  }

#ifdef Is_True_On
  Verify_ver(idx);
#endif

  return idx;
}

VER_IDX
WHIRL_SSA_MANAGER::New_ver(WST_IDX wst_idx, const WN* def_wn, WSSA_NODE_KIND def_type) {
  // update max_ver for PREG since the max_ver is updated after constructing HSSA
  WST_Symbol_Entry& wst = _sym_table[wst_idx];
  VER_IDX idx = (VER_IDX)_ver_table.size();
  _ver_table.push_back(
      WST_Version_Entry(wst_idx, Next_ver(wst_idx), def_wn, def_type));

  // update def chain
  if (def_wn != NULL) {
    VER_IDX last_ver = wst.Last_ver();
    _ver_table[idx].Set_prev_ver(last_ver);
    wst.Set_last_ver(idx);
  }

#ifdef Is_True_On
  Verify_ver(idx);
#endif

  return idx;
}

const WST_Version_Entry& 
WHIRL_SSA_MANAGER::Get_ver(VER_IDX ver_idx) const {
  Is_True(ver_idx >= 0 && ver_idx < _ver_table.size(), ("ver_idx out of bounds"));
  return _ver_table[ver_idx];
}

WST_IDX
WHIRL_SSA_MANAGER::Get_ver_wst(VER_IDX ver_idx) const {
  const WST_Version_Entry& ver = Get_ver(ver_idx);
  return ver.Get_wst();
}

UINT32
WHIRL_SSA_MANAGER::Get_ver_num(VER_IDX ver_idx) const {
  const WST_Version_Entry& ver = Get_ver(ver_idx);
  return ver.Get_ver();
}

void
WHIRL_SSA_MANAGER::Update_ver(VER_IDX idx, WN* def_wn, WSSA_NODE_KIND def_type) {
  Is_True(idx >= 0 && idx < _ver_table.size(), ("ver_idx out of bounds"));
  Is_True(def_wn != NULL && WSSA::WN_has_node(def_wn, def_type),
          ("bad wn and def type"));
  WST_Version_Entry& ver = _ver_table[idx];
  WST_Symbol_Entry& wst = _sym_table[ver.Get_wst()];
  Is_True(ver.Get_ver() == 0 || ver.Is_zero() || ver.Get_def_wn() == NULL, ("ver already has a def wn"));
  Is_True(ver.Get_ver() == 0 || ver.Is_zero() || ver.Get_def_type() == WSSA_UNKNOWN, ("ver already has a def type"));
  // update version
  ver.Set_def_wn(def_wn);
  ver.Set_def_type(def_type);

  // update def chain
  VER_IDX last_ver = wst.Last_ver();
  _ver_table[idx].Set_prev_ver(last_ver);
  wst.Set_last_ver(idx);

#ifdef Is_True_On
  // verify the def wn of this version
  Verify_ver(idx);
#endif
}

void
WHIRL_SSA_MANAGER::Update_ver_num(VER_IDX idx, UINT32 ver_num) {
  Is_True(idx >= 0 && idx < _ver_table.size(), ("ver_idx out of bounds"));
  WST_Version_Entry& ver = _ver_table[idx];
#ifdef Is_True_On 
  if (ver_num == 0) {
    Is_True(ver.Get_def_wn() == NULL, ("ver 0 can not have a def"));
    Is_True(ver.Get_def_type() == WSSA_UNKNOWN, ("ver 0 def type wrong"));
  }
#endif
  ver.Set_ver(ver_num);
}

//====================================================================
// Manage the version numbers for the wst
//   Get_max_ver: get the max version number for the wst
//   Set_max_ver: set the max version number for the wst
//   Next_ver: get the next version number for the wst
//====================================================================
UINT32
WHIRL_SSA_MANAGER::Get_max_ver(WST_IDX st_idx) const {
  const WST_Symbol_Entry& sym = Get_wst(st_idx);
  return sym.Max_ver();
}

void
WHIRL_SSA_MANAGER::Set_max_ver(WST_IDX wst_idx, UINT32 max_ver) {
  Is_True(wst_idx >= 0 && wst_idx < _sym_table.size(), ("wst_idx out of bounds"));
  _sym_table[wst_idx].Set_max_ver(max_ver);
}

UINT32
WHIRL_SSA_MANAGER::Next_ver(WST_IDX wst_idx) {
  Is_True(wst_idx >= 0 && wst_idx < _sym_table.size(), ("wst_idx out of bounds"));
  return _sym_table[wst_idx].Next_ver();
}

//====================================================================
// const iterators for traversing the WSSA version table
//====================================================================
WHIRL_SSA_MANAGER::const_ver_iterator
WHIRL_SSA_MANAGER::Ver_begin() const {
  return _ver_table.begin();
}

WHIRL_SSA_MANAGER::const_ver_iterator
WHIRL_SSA_MANAGER::Ver_end() const {
  return _ver_table.end();
}

//====================================================================
// return the count of wssa versions
//====================================================================
UINT32
WHIRL_SSA_MANAGER::Ver_count() const {
  return _ver_table.size();
}

//====================================================================
// Update interface for WHIRL SSA
// Copy_wn_ssa
//   copy ssa information for wn node from dest to src.
//   the operator of two WN must be the same
// Copy_tree_ssa
//   copy ssa information from dest to src
//   the structure of two wn trees must be the same
// Create_entry_chi
//   create_entry_chi for preg when trying to use an uninited preg
//====================================================================
void
WHIRL_SSA_MANAGER::Copy_wn_ssa(WN* dest, const WN* src) {
  Is_True(dest != NULL && src != NULL,
          ("WN_operator does not match, can not copy"));
  Is_True(WN_operator(dest) == WN_operator(src),
          ("WN_operator does not match, can not copy"));

  // make sure the dest node has map_id
  WN_MAP_Set_ID(Current_Map_Tab, dest);
  if (WN_has_phi(src)) {
    for (phi_iterator it = WN_phi_begin(src);
         it != WN_phi_end(src);
         ++it) {
      PHI_NODE* phi = Create_phi(it->Opnd_count());
      phi->Set_res(it->Res());
      for (int i=0; i<it->Opnd_count(); i++) {
        phi->Set_opnd(i, it->Opnd(i));
      }
      Add_phi(dest, phi);
    }
  }
  if (WN_has_chi(src)) {
    for (chi_iterator it = WN_chi_begin(src);
          it != WN_chi_end(src);
          ++it) {
      CHI_NODE* chi = Create_chi();
      chi->Set_res(it->Res());
      chi->Set_opnd(it->Opnd());
      Add_chi(dest, chi);
    }
  }
  if (WN_has_mu(src)) {
    for (mu_iterator it = WN_mu_begin(src);
          it != WN_mu_end(src);
          ++it) {
      MU_NODE* mu = Create_mu();
      mu->Set_opnd(it->Opnd());
      Add_mu(dest, mu);
    }
  }
  if (WN_has_ver(src) ) {
    VER_IDX ver = Get_wn_ver(src);
    Set_wn_ver(dest, ver); 
  }
}
  
void
WHIRL_SSA_MANAGER::Copy_tree_ssa(WN* dest, const WN* src) {
  Is_True(dest != NULL && src != NULL,
          ("WN_operator does not match, can not copy"));
  Is_True(WN_operator(dest) == WN_operator(src),
          ("WN_operator does not match, can not copy"));

  if (WN_operator(src) == OPR_BLOCK) {
    WN* dest_kid = WN_first(dest);
    for (WN* src_kid = WN_first(src); 
         src_kid != NULL; 
         src_kid = WN_next(src_kid)) {
      Is_True(dest_kid != NULL, ("dest kid is NULL"));
      Copy_tree_ssa(dest_kid, src_kid);
      dest_kid = WN_next(dest_kid);
    }
    return;
  }

  Copy_wn_ssa(dest, src);

  for (INT i = 0; i < WN_kid_count(src); ++i) {
    Is_True(WN_kid(dest, i) != 0, ("dest kid is NULL"));
    Copy_tree_ssa(WN_kid(dest, i), WN_kid(src, i));
  }
}

VER_IDX
WHIRL_SSA_MANAGER::Create_entry_chi(ST* preg_st, PREG_NUM preg_num) {
  Is_True(preg_st != NULL && ST_class(preg_st) == CLASS_PREG,
          ("st is not a preg st"));
  WST_IDX idx = Find_wst(preg_st, preg_num);
  Is_True(idx == WST_INVALID, ("preg already used before"));
  idx = New_wst(preg_st, preg_num);
  VER_IDX opnd_ver = New_ver(idx, NULL, WSSA_UNKNOWN);
  VER_IDX res_ver = New_ver(idx, _root, WSSA_CHI);
  CHI_NODE* chi = Create_chi();
  chi->Set_opnd(opnd_ver);
  chi->Set_res(res_ver);
  Add_chi(_root, chi);
  return res_ver;
}

//====================================================================
// Enter_stmt
//   add new stmt. 
//   rhs of stmt should have the correct ssa information
//   if lhs of stmt doesn't have ssa, new version will be created
// Remove_stmt
//   remove the ssa info for the stmt
//====================================================================
#ifndef IR_TOOLS
void
WHIRL_SSA_MANAGER::Enter_stmt(WN* tree) {
  OPERATOR opr = WN_operator(tree);
  Is_True(tree != NULL && (opr == OPR_STID || opr == OPR_ISTORE),
          ("only support STID or ISTORE"));

  if (opr == OPR_STID) {
    WN_VER_MAP::const_iterator it = _wn_ver_map.find(WN_idx(tree));
    if (it == _wn_ver_map.end() ||
        ST_class(WN_st(tree)) == CLASS_PREG) {
      WST_IDX wst_idx = Create_wst_for_wn(tree);
      VER_IDX def_ver = New_ver(wst_idx, tree, WSSA_OCC);
      // make sure the dest node has map_id
      WN_MAP_Set_ID(Current_Map_Tab, tree);
      Set_wn_ver(tree, def_ver);
      return;
    }
    else {
      // rename existing version
      VER_IDX old_ver = it->second;
      WST_IDX wst_idx = Get_ver_wst(old_ver);
      VER_IDX new_ver = New_ver(wst_idx, tree, WSSA_OCC);
      Set_wn_ver(tree, new_ver);
      // fall through since it may have chi node
    }
  }
  // rename the chi node
  if (WN_has_chi(tree)) {
    for (chi_iterator it = WN_chi_begin(tree);
          it != WN_chi_end(tree);
          ++it) {
      VER_IDX old_ver = it->Res();
      WST_IDX wst_idx = Get_ver_wst(old_ver);
      VER_IDX new_ver = New_ver(wst_idx, tree, WSSA_CHI);
      it->Set_res(new_ver);
    }
  }
  // TODO: maintain def-def chain
  // TODO: verify the tree/rename map/ssa
}

void
WHIRL_SSA_MANAGER::Remove_stmt(WN* tree) {
  OPERATOR opr = WN_operator(tree);
  Is_True(opr == OPR_STID || opr == OPR_ISTORE ||
          OPERATOR_is_call(opr),
          ("unsupported OP: %s", OPCODE_name(WN_opcode(tree))));

  // reset ssa related entries
  if (WN_has_chi(tree)) {
    Clear_list<WSSA_CHI>(tree);
  }
  if (WN_has_mu(tree)) {
    Clear_list<WSSA_MU>(tree);
  }
  if (WN_has_ver(tree)) {
    //Set_wn_ver(tree, VER_INVALID);
    _wn_ver_map.erase(WN_idx(tree));
  }
  // TODO: maintain def-def chain
}
#endif

//====================================================================
// Print_table_with_map: print the information from wssa table and map
//   template parameter:
//     _Ttable: type of the tables, like PHI_TABLE
//     _Tmap: type of the maps, line WN_PHI_MAP
//====================================================================
template<typename _Ttable, typename _Tmap>
void WHIRL_SSA_MANAGER::Print_table_with_map(const _Ttable& tbl, const _Tmap& map, FILE* fp) const {
  typedef typename _Tmap::const_iterator const_iterator;
  typedef typename _Tmap::data_type data_type;
  int used_count = 0;
  std::vector<bool> chk_vec;
  chk_vec.resize( tbl.size() );
  for (const_iterator it = map.begin();
       it != map.end();
       ++it) {
    data_type idx = it->second;
    Is_True(idx < tbl.size(), ("map value out of range"));
    Is_True(chk_vec[idx] == false, ("map value has been outputed"));
    chk_vec[idx] = true;
    ++used_count;
    tbl[idx]->Print(fp, 2);
  }
  if (used_count != tbl.size()) {
    for (std::size_t i=0; i<tbl.size(); ++i) {
      if (chk_vec[i] == false) {
        fprintf(fp, "  (unused)");
        tbl[i]->Print(fp, 0);
      }
    }
  }
}

//====================================================================
// Printing routines
//   Print: dump all info
//   Print_ssa: dump all ssa node info
//   Print_wst_table: dump all wssa symbol
//   Print_wst: dump the wst info
//   Print_ver_table: dump all version info
//   Print_ver: dump the version info
//====================================================================
void
WHIRL_SSA_MANAGER::Print(FILE* fp) const {
  fprintf(fp, "SSA Symbols:\n");
  Print_wst_table(fp);
  fprintf(fp, "SSA Versions:\n");
  Print_ver_table(fp);
  Print_ssa(fp);
}

void
WHIRL_SSA_MANAGER::Print_ssa(FILE* fp) const {
  fprintf(fp, "PHIs\n");
  Print_table_with_map(_phi_table, _phi_map, fp); 
  fprintf(fp, "CHIs\n");
  Print_table_with_map(_chi_table, _chi_map, fp); 
  fprintf(fp, "MUs\n");
  Print_table_with_map(_mu_table, _mu_map, fp); 
}

void 
WHIRL_SSA_MANAGER::Print_wst_table(FILE* fp) const {
  for(INT32 wst_idx = 0; wst_idx < _sym_table.size(); ++wst_idx) {
    Print_wst((WST_IDX)wst_idx, fp);
  }
}

void
WHIRL_SSA_MANAGER::Print_wst(WST_IDX wst_idx, FILE* fp) const {
  const WST_Symbol_Entry& sym = Get_wst(wst_idx);
  fprintf(fp,"Index [%d], WST type: ", wst_idx);
  switch (sym.Sym_type()) {
    case WST_WHIRL:
      fprintf(fp,"WST_WHIRL\n");
      Print_ST(fp, &St_Table[sym.St_idx()], TRUE);
      break;
    case WST_PREG:
      fprintf(fp,"WST_PREG\n");
      Print_ST(fp, &St_Table[sym.St_idx()], TRUE);
      fprintf(fp, "preg num %d\n", sym.Preg_num());
      break;
    case WST_FIELD:
      {
        fprintf(fp,"WST_FIELD\n");
        Print_ST(fp, &St_Table[sym.St_idx()], TRUE);
        const WST_Field_Info& field = Get_field(sym.Field_idx());
        field.Print(fp);
        break;
      }
    case WST_VSYM:
      {
        fprintf(fp,"WST_VSYM\n");
        const WST_Vsym_Info& vsym = Get_vsym(sym.Vsym_idx());
        vsym.Print(fp);
        break;
      }
    default:
      break;
  }
}

void 
WHIRL_SSA_MANAGER::Print_ver_table(FILE* fp) const {
  for(INT32 ver_idx = 0; ver_idx < _ver_table.size(); ++ver_idx) {
    Print_ver((VER_IDX)ver_idx, fp);
  }
}

void
WHIRL_SSA_MANAGER::Print_ver(VER_IDX ver_idx, FILE* fp) const {
  const WST_Version_Entry& ver = Get_ver(ver_idx);
  ver.Print(fp);
}

//====================================================================
// Verify routines
//   Verify_ver: verify the version
//   Verify: verify all info in the manager
//====================================================================
void
WHIRL_SSA_MANAGER::Verify_ver(VER_IDX ver_idx) const {
  const WST_Version_Entry& ver = Get_ver(ver_idx);
  const WN* def_wn = ver.Get_def_wn();
  WSSA_NODE_KIND def_type = ver.Get_def_type();
  if (def_wn != NULL) {
    FmtAssert(WSSA::WN_has_node(def_wn, def_type), ("wn opr and def type mismatch"));
  }
  else {
    // use of undefined version
    FmtAssert(ver.Get_def_type() == WSSA_UNKNOWN, ("invalid def type"));
  }
}

void
WHIRL_SSA_MANAGER::Verify() const {
}

} /* namespace WSSA */

