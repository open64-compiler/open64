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
// Module: wssa_mgr_template.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Implementation for template member methods in WHIRL_SSA_MANAGER
//
// Exported classes:
//  WSSA::WHIRL_SSA_MANAGER
//
// SEE ALSO:
//  be/com/wssa_mgr.h (WHIRL_SSA_MANAGER)
//
//====================================================================

#ifndef wssa_mgr_INCLUDED
#error This file can only be included by wssa_mgr.h
#endif

#ifndef wssa_mgr_template_INCLUDED
#define wssa_mgr_template_INCLUDED

//===================================================================
// This file contains the implementation of template functions
// declared in wssa_mgr.h
// Do NOT use file standalone. This file is always included by
// wssa_mgr.h.
//===================================================================
namespace WSSA {

//===================================================================
// Get_node
//  Get the node by node index
//  template parameter:
//    WSSA_NODE_KIND: WSSA_PHI, WSSA_CHI or WSSA_MU
//===================================================================
template<WSSA_NODE_KIND _Tkind>
typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* WHIRL_SSA_MANAGER::Get_node(WSSA_NODE_IDX idx) {
  typedef typename NODE_TO_TYPES<_Tkind>::TABLE_TYPE TABLE_TYPE;
  TABLE_TYPE& table = Get_table<_Tkind>();
  Is_True(idx >= 0 && idx < table.size(), ("index out of bounds"));
  return table[idx];
}

template<WSSA_NODE_KIND _Tkind>
const typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* WHIRL_SSA_MANAGER::Get_node(WSSA_NODE_IDX idx) const {
  typedef typename NODE_TO_TYPES<_Tkind>::TABLE_TYPE TABLE_TYPE;
  const TABLE_TYPE& table = Get_table<_Tkind>();
  Is_True(idx >= 0 && idx < table.size(), ("index out of bounds"));
  return table[idx];
}


//===================================================================
// WN_first_node_idx:
//   Get the first node index attached on the WN
//   template parameter:
//     WSSA_NODE_KIND: WSSA_PHI, WSSA_CHI or WSSA_MU
// WN_next_node
//   Get the next node attached on the same WN
//   template parameter:
//     _Tnode: type of the node
// WN_has_node
//   check if wn has the given kind of node
//   template parameter:
//     WSSA_NODE_KIND: WSSA_PHI, WSSA_CHI or WSSA_MU
//===================================================================
template<WSSA_NODE_KIND _Tkind>
WSSA_NODE_IDX WHIRL_SSA_MANAGER::WN_first_node_idx(const WN* wn) const {
  typedef typename NODE_TO_TYPES<_Tkind>::WN_MAP_TYPE WN_MAP_TYPE;
  const WN_MAP_TYPE& map = Get_map<_Tkind>();
  WSSA_WN_IDX wn_idx = WN_idx(wn);
  typename WN_MAP_TYPE::const_iterator it = map.find(wn_idx);
  if (it != map.end())
    return it->second;
  else
    return INVALID_IDX;
}

template<typename _Tnode>
_Tnode* WHIRL_SSA_MANAGER::WN_next_node(_Tnode* node) {
  Is_True(node != NULL, ("node is NULL"));
  if (node->Next() != INVALID_IDX)
    return Get_node<_Tnode::NODE_KIND>(node->Next());
  else
    return NULL; 
}

template<typename _Tnode>
const _Tnode* WHIRL_SSA_MANAGER::WN_next_node(_Tnode* node) const {
  Is_True(node != NULL, ("node is NULL"));
  if (node->Next() != INVALID_IDX)
    return Get_node<_Tnode::NODE_KIND>(node->Next());
  else
    return NULL; 
}

template<WSSA_NODE_KIND _Tkind>
BOOL WHIRL_SSA_MANAGER::WN_has_node(const WN* wn) const {
  if (WN_map_id(wn) == -1)
    return FALSE;
  if (WSSA::WN_has_node(wn, _Tkind) &&
      WN_first_node_idx<_Tkind>(wn) != INVALID_IDX)
    return TRUE;
  else
    return FALSE;
}

//===================================================================
// Create_node
//   create WSSA nodes
//   template parameter:
//     WSSA_NODE_KIND: WSSA_PHI, WSSA_CHI or WSSA_MU
// Add_node
//   attach node to wn
//   template parameter:
//     _Tnode: type of node
// Clear_list
//   clear the whole list attached to wn
//   template parameter:
//     WSSA_NODE_KIND: WSSA_PHI, WSSA_CHI or WSSA_MU
//===================================================================
template<WSSA_NODE_KIND _Tkind>
typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* WHIRL_SSA_MANAGER::Create_node(INT32 opnd_num) {
  INT32 node_size;
  typedef typename NODE_TO_TYPES<_Tkind>::NODE_TRAITS NODE_TRAITS;
  typedef typename NODE_TO_TYPES<_Tkind>::NODE_TYPE NODE_TYPE;
  if (NODE_TRAITS::OPND_COUNT != -1) {
    node_size = sizeof(NODE_TYPE) + 
                (NODE_TRAITS::RES_COUNT + NODE_TRAITS::OPND_COUNT) * sizeof(INT32);
  }
  else {
    node_size = sizeof(NODE_TYPE) + 
                (NODE_TRAITS::RES_COUNT + 1 + opnd_num) * sizeof(INT32);
  }
  NODE_TYPE* node = (NODE_TYPE*) MEM_POOL_Alloc(_ssa_pool, node_size);
  memset(node, 0, node_size);
  if (NODE_TRAITS::OPND_COUNT == -1)
    node->Set_opnd_count(opnd_num);
  return node;
}

template<typename _Tnode>
WSSA_NODE_IDX WHIRL_SSA_MANAGER::Add_node(const WN* wn, _Tnode* node) {
#ifdef Is_True_On
  Is_True(node != NULL, ("node is NULL"));
  Is_True(WSSA::WN_has_node(wn, _Tnode::NODE_KIND), ("WN can not have this kind of node"));
#endif

  WN_MAP_Set_ID(Current_Map_Tab, const_cast<WN*>(wn));
  typedef typename NODE_TO_TYPES<_Tnode::NODE_KIND>::TABLE_TYPE TABLE_TYPE;
  typedef typename NODE_TO_TYPES<_Tnode::NODE_KIND>::WN_MAP_TYPE WN_MAP_TYPE;
  TABLE_TYPE& table = Get_table<_Tnode::NODE_KIND>();
  WN_MAP_TYPE& map = Get_map<_Tnode::NODE_KIND>();
  WSSA_WN_IDX wn_idx = WN_idx(wn);
  WSSA_NODE_IDX node_idx = table.size();
  table.push_back(node);
  WSSA_NODE_IDX first_idx = WN_first_node_idx<_Tnode::NODE_KIND>(wn);
  node->Set_next(first_idx);
  map[wn_idx] = node_idx;
  return node_idx;
}

template<WSSA_NODE_KIND _Tkind>
void WHIRL_SSA_MANAGER::Clear_list(WN* wn) {
#ifdef Is_True_On
  Is_True(wn != NULL, ("wn is NULL"));
  Is_True(WSSA::WN_has_node(wn, _Tkind), ("WN can not have this kind of node"));
#endif

  // TODO: add node to free list
  // TODO: add entry in vector to free list
  typedef typename NODE_TO_TYPES<_Tkind>::WN_MAP_TYPE WN_MAP_TYPE;
  WN_MAP_TYPE& map = Get_map<_Tkind>();
  map.erase(WN_idx(wn));
}

//===================================================================
// WN_ssa_node
//   Get ssa node of given WST_IDX attached on the WN
//   template parameter:
//     WSSA_NODE_KIND: WSSA_PHI, WSSA_CHI or WSSA_MU
//===================================================================
template<WSSA_NODE_KIND _Tkind>
typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* WHIRL_SSA_MANAGER::WN_ssa_node(const WN* wn, WST_IDX wst) {
  Is_True(WSSA::WN_has_node(wn, _Tkind), ("WN can not have this kind of node"));
  WSSA_NODE_IDX idx = WN_first_node_idx<_Tkind>(wn);
  while (idx != INVALID_IDX) {
    typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* node = Get_node<_Tkind>(idx);
    if (Get_ver_wst(node->Get_opnd(0)) == wst)
      return node;
    idx = node->Next();
  }
  return NULL;
}

template<WSSA_NODE_KIND _Tkind>
const typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* WHIRL_SSA_MANAGER::WN_ssa_node(const WN* wn, WST_IDX wst) const {
  Is_True(WSSA::WN_has_node(wn, _Tkind), ("WN can not have this kind of node"));
  WSSA_NODE_IDX idx = WN_first_node_idx<_Tkind>(wn);
  while (idx != INVALID_IDX) {
    const typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* node = Get_node<_Tkind>(idx);
    if (Get_ver_wst(node->Get_opnd(0)) == wst)
      return node;
    idx = node->Next();
  }
  return NULL;
}

//===================================================================
// WN_node_begin
//  Begin the iterator to traverse the node list
//  template parameter
//    _Tnode: type of the ssa node
//===================================================================
template<typename _Tnode>
WSSA_NODE_ITERATOR<WHIRL_SSA_MANAGER, _Tnode> WHIRL_SSA_MANAGER::WN_node_begin(const WN* wn) {
  Is_True(wn != NULL, ("wn is NULL"));
  typedef WSSA_NODE_ITERATOR<WHIRL_SSA_MANAGER, _Tnode> iterator;
  WSSA_NODE_IDX idx = WN_first_node_idx<_Tnode::NODE_KIND>(wn);
  return iterator(this, idx);
}

template<typename _Tnode>
WSSA_NODE_ITERATOR<const WHIRL_SSA_MANAGER, const _Tnode> WHIRL_SSA_MANAGER::WN_node_begin(const WN* wn) const {
  Is_True(wn != NULL, ("wn is NULL"));
  typedef WSSA_NODE_ITERATOR<const WHIRL_SSA_MANAGER, const _Tnode> const_iterator;
  WSSA_NODE_IDX idx = WN_first_node_idx<_Tnode::NODE_KIND>(wn);
  return const_iterator(this, idx);
}

//====================================================================
// Verify_node: verify the node with the def wn
//   template parameter:
//     _Tnode: type of the node
//====================================================================
template<typename _Tnode>
void WHIRL_SSA_MANAGER::Verify_node(const WN* wn, const _Tnode* node) const {
  WST_IDX wst_idx = Get_ver_wst(node->Get_opnd(0));
  UINT32 max_ver = Get_max_ver(wst_idx);
  int i;
  for (i = 0; i<node->Res_count(); i++) {
    const WST_Version_Entry& ver_info = Get_ver(node->Get_res(i));
    FmtAssert(ver_info.Get_wst() == wst_idx, ("WST idx mismatch"));
    FmtAssert(ver_info.Get_ver() <= max_ver, ("Ver num out of bounds"));
    FmtAssert(ver_info.Get_def_wn() == wn, ("WN mismatch"));
    FmtAssert(ver_info.Get_def_type() == _Tnode::NODE_KIND, ("def type mismatch"));
  }
  for (i=0; i<node->Opnd_count(); i++) {
    const WST_Version_Entry& ver_info = Get_ver(node->Get_opnd(i));
    FmtAssert(ver_info.Get_wst() == wst_idx, ("WST idx mismatch"));
    FmtAssert(ver_info.Get_ver() <= max_ver, ("Ver num out of bounds"));
    //FmtAssert(ver_info.Is_flag_set(VER_IS_ZERO) || ver_info.Get_def_wn() != wn, 
    //          ("operand def WN mismatch"));
  }
}


} /* namespace WSSA */

#endif /* wssa_mgr_template_INCLUDED */
