/*

  Copyright (C) 2011, Hewlett-Packard Development Company, L.P. All Rights Reserved.

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
// Module: cg_cfg.cxx
//
// Revision history:
//  May-11-2011 - Original Version
//
// Description:
//  Implementation of CG CFG
//
// Exported classes:
//  CFG_UTIL::CG_CFG
//  CFG_UTIL::CG_CFG_BUILDER
//
// SEE ALSO:
//  be/cg/cg_cfg.h (CG_CFG)
//
//====================================================================

#include "cg_cfg.h"
#include "cfg_util.h"
#include "ir_reader.h"
#include <ext/hash_set>
using namespace __gnu_cxx;

// VCG helper utilities
#include <sstream>
extern void help_image_op(std::stringstream &ss, OP *op, INT indent);
extern void help_image_stmt(std::stringstream &ss, OP * op, INT indent);

namespace CFG_UTIL {
//===================================================================
// CG_STMT_CONTAINER
//===================================================================
void
CG_STMT_CONTAINER::Add_stmt(OP* stmt) {
  Is_True(stmt != NULL, ("can not add NULL stmt"));
  if (_first_stmt == NULL) {
    _first_stmt = stmt;
    _last_stmt = stmt;
  }
  else if (OP_next(_last_stmt) == stmt) {
    _last_stmt = stmt;
  }
  else {
    FmtAssert(OP_next(_last_stmt) == NULL, ("OP_next(_last_stmt) must be NULL"));
  }
}

OP*
CG_STMT_CONTAINER::First_stmt() const {
  return _first_stmt;
}

OP*
CG_STMT_CONTAINER::Next_stmt(OP* stmt) const {
  Is_True(stmt != NULL, ("stmt is NULL"));
  if (stmt == _last_stmt)
    return NULL;  // no next
  else
    return OP_next(stmt);
}

OP*
CG_STMT_CONTAINER::Prev_stmt(OP* stmt) const {
  Is_True(stmt != NULL, ("stmt is NULL"));
  if (stmt == _first_stmt)
    return NULL;
  else
    return OP_prev(stmt);
}

OP*
CG_STMT_CONTAINER::Last_stmt() const {
  return _last_stmt;
}

BOOL
CG_STMT_CONTAINER::Is_empty() const {
  return _first_stmt == NULL;
}

void
CG_STMT_CONTAINER::Insert_before(OP* before, OP* stmt) {
  Is_True(before != NULL, ("invalid before stmt"));
  Is_True(stmt != NULL, ("invalid stmt"));

#ifdef Is_True_On
  OP* op = NULL;
  for (op = _first_stmt; op != _last_stmt; op = OP_next(op)) {
    if (op == before)
      break;
  }
  Is_True(before == op, ("can not find insert position"));
#endif

  if (before == _first_stmt) {
    _first_stmt = stmt;
  }
}

void
CG_STMT_CONTAINER::Insert_after(OP* after, OP* stmt) {
  Is_True(after != NULL, ("invalid after stmt"));
  Is_True(stmt != NULL, ("invalid stmt"));

#ifdef Is_True_On
  OP* op = NULL;
  for (op = _first_stmt; op != _last_stmt; op = OP_next(op)) {
    if (op == after)
      break;
  }
  Is_True(after == op, ("can not find insert position"));
#endif

  if (after == _last_stmt) {
    _last_stmt = stmt;
  }
}

void
CG_STMT_CONTAINER::Remove_stmt(OP* stmt) {
  Is_True(stmt != NULL, ("invalid stmt"));

#ifdef Is_True_On
  OP* op = NULL;
  for (op = _first_stmt; op != _last_stmt; op = OP_next(op)) {
    if (op == stmt)
      break;
  }
  Is_True(stmt == op || stmt == _last_stmt, ("can not find op"));
#endif

  if (_first_stmt != _last_stmt) {
    if (stmt == _first_stmt)
      _first_stmt = OP_next(_first_stmt);
    if (stmt == _last_stmt)
      _last_stmt = OP_prev(_last_stmt);
  }
  else {
    FmtAssert(stmt == _first_stmt, ("can not find op"));
    _first_stmt = _last_stmt = NULL;
  }
}

void
CG_STMT_CONTAINER::Print(FILE* fp, INT32 dump_flag) const {
  if (dump_flag & DUMP_AIR) {
    Print_OPs(_first_stmt);
  }
}

VCGNode*
CG_STMT_CONTAINER::VCG_dump(MEM_POOL* mpool, VCGGraph& vcg, INT32 dump_flag) const {
  VCGNode* bb_node = NULL;
  if (dump_flag & DUMP_AIR) {
    std::stringstream ss;
    for (OP* op = _first_stmt; op != NULL; op = Next_stmt(op)) {
      if (dump_flag & DUMP_EXPR) {
        help_image_stmt(ss, op, 0);
      }
      else {
        help_image_op(ss, op, 0);
      }
    }
    char* label = (char *) MEM_POOL_Alloc(mpool, ss.str().size()+1);
    strcpy(label, ss.str().c_str());
    bb_node = CXX_NEW(VCGNode(NULL, label), mpool); // title will be set later
    vcg.addNode(*bb_node);
  }
  return bb_node;
}

//===================================================================
// CG_STMT_MAPPER
//===================================================================
void
CG_STMT_MAPPER::Connect_stmt_node(STMT stmt, BB_NODE* node) {
  Is_True(_stmt_map.find((INTPTR)stmt) == _stmt_map.end(), ("stmt has been associated with a node"));
  _stmt_map[(INTPTR)stmt] = node;
}

void
CG_STMT_MAPPER::Disconnect_stmt_node(STMT stmt, BB_NODE* node) {
  STMT_MAP::iterator it = _stmt_map.find((INTPTR)stmt);
  Is_True(it != _stmt_map.end(), ("stmt does not have a node"));
  _stmt_map.erase(it);
}

void
CG_STMT_MAPPER::Disconnect_all_stmt_in_node(BB_NODE* node) {
  for (BB_NODE::stmt_iterator it = node->Stmt_begin();
       it != node->Stmt_end();
       ++it) {
    Disconnect_stmt_node(&(*it), node);
  }
}

CG_STMT_MAPPER::BB_NODE*
CG_STMT_MAPPER::Get_stmt_node(STMT stmt) const {
  STMT_MAP::const_iterator it = _stmt_map.find((INTPTR)stmt);
  if (it != _stmt_map.end())
    return it->second;
  else
    return NULL;
}

CG_CFG::BB_NODE*
CG_CFG::Get_BB(OP* op) const {
  return Get_stmt_node(op);
}

CG_CFG::BB_NODE*
CG_CFG::Get_BB(UINT32 id) const {
  BB_ID_MAP::const_iterator i = _bb_id_map.find(id);
  if (i == _bb_id_map.end())
    return NULL;

  return i->second;
}

BB*
CG_CFG::Get_CG_BB(OP* op) {
  CG_CFG::BB_NODE* node = Get_BB(op);
  Is_True(node != NULL, ("Given OP cannot be found in any BB"));
  return Get_BB_from_BB_NODE(node);
}

void
CG_CFG::Build() {
  // mapping between CFG_BASE BB_NODEs and CG BBs
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    // entry / exit
    CG_CFG::BB_NODE* node = Get_BB_NODE_from_BB(bb);
    if (BB_entry(bb)) Add_entry_node(node);
    if (BB_exit(bb))  Add_exit_node(node);

    for (BBLIST* slist = BB_succs(bb); slist; slist = BBLIST_next(slist)) {

      BB* succ = BBLIST_item(slist);
      CG_CFG::BB_NODE* succ_node = Get_BB_NODE_from_BB(succ);

      Connect_predsucc(node, succ_node);
    }
  }

  std::vector<bool> visited(Get_max_id()+1, false);
  std::vector<bool> onstack(Get_max_id()+1, false);
  Connect_Dangling_Exits(Get_dummy_entry(), visited, onstack);
} 

// mapping between CFG_BASE BB_NODEs and CG BBs
void
CG_CFG::Map_BB_and_BB_NODE(BB* bb, CG_CFG::BB_NODE* bbn) {
  Is_True(bbn&&bb, ("NULL bbn(%x) or bb(%x)\n", bbn, bb));

  _bb_node_map[(INTPTR)bbn] = bb;
  _bb_map[(INTPTR)bb] = bbn;
  _bb_id_map[bbn->Get_id()] = bbn;
}

bool 
CG_CFG::Is_in_BB_map(BB* succ) {
  return _bb_map.find((INTPTR)succ) != _bb_map.end();
}

CG_CFG::BB_NODE* 
CG_CFG::Get_BB_NODE_from_BB(BB* bb) {
  if (_bb_map.find((INTPTR)bb) == _bb_map.end()) {
    CG_CFG::BB_NODE* node = Create_node();
    Map_BB_and_BB_NODE(bb, node);
    
    // keep only the statements in the container
    Add_All_Stmts_in_BB(node, bb);
    return node;
  }
  else
    return _bb_map[(INTPTR)bb];
}


BB* 
CG_CFG::Get_BB_from_BB_NODE(const CG_CFG::BB_NODE* bbn) {
  if (_bb_node_map.find((INTPTR)bbn) == _bb_node_map.end())
    return NULL;
  else
    return _bb_node_map[(INTPTR)bbn];
}

void
CG_CFG::Add_All_Stmts_in_BB(BB_NODE* node, BB* bb) {
  Is_True(node && bb, ("node(%x) or bb(%x) is null", node, bb));

  for (OP* stmt = BB_first_op(bb); stmt != NULL; stmt = OP_next(stmt)) {
    CFG::Add_stmt(node, stmt);
    if (stmt == BB_last_op(bb)) break;
  }
}


// connect all reachable nodes that do not have any successor to
// the dummy exit. This function is similar to Connect_noexit() of
// cfg_util.h.
void
CG_CFG::Connect_Dangling_Exits(BB_NODE* node, 
			       std::vector<bool>& visited,
			       std::vector<bool>& onstack)
{
  if (visited[node->Get_id()]) return;
  visited[node->Get_id()] = true;
  onstack[node->Get_id()] = true;

  int succs_visited = 0;
  BB_NODE::const_bb_iterator it = node->Succ_begin();
  for (;it != node->Succ_end(); ++it)
    if (onstack[(*it)->Get_id()] == false) {
      Connect_Dangling_Exits(*it, visited, onstack);
      ++succs_visited;
    }
  onstack[node->Get_id()] = false;

  // no successors visited == dangling exit
  if (succs_visited == 0 && node != Get_dummy_exit())
    Connect_predsucc(node, Get_dummy_exit());
}


} /* namespace CFG_UTIL */

