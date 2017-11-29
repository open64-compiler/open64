// ====================================================================
//
// Copyright (C) 2011, Hewlett-Packard Development Company, L.P.
// All Rights Reserved.
//
// Open64 is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// Open64 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
// MA  02110-1301, USA.
//
// ====================================================================
//
// Module: cgssa_core.cxx
//
// Description:
//   Implementation for core data structures for CGSSA
//   
// ====================================================================

#include "cgssa_core.h"

using namespace CGSSA_NAME;

//===================================================================
// Implementation for functions in VERSION class
//===================================================================
VERSION::VERSION(OP* def, int ith_result, VER_ID ver, TN* tn, 
  VERSION_KIND kind, CGSSA* cgssa)
  : _ver_num(ver), _tn(tn), _kind(kind) {
  Is_True(kind == VERSION_OCC_DEF, ("not the real def"));
  _def = cgssa->Result_Ref_ID(def, ith_result);
  _uses.clear();
}

VERSION::VERSION(PHI_NODE* def, VER_ID ver, TN* tn, 
  VERSION_KIND kind, CGSSA* cgssa)
  : _ver_num(ver), _tn(tn), _kind(kind) {
  Is_True(kind == VERSION_PHI_DEF, ("not the phi def"));
  _def = cgssa->Result_Ref_ID(def);
  _uses.clear();
}

VERSION::VERSION(CHI_NODE* def, VER_ID ver, TN* tn,  
  VERSION_KIND kind, CGSSA* cgssa)
  : _ver_num(ver), _tn(tn), _kind(kind) {
  Is_True(kind == VERSION_CHI_DEF, ("not the chi def"));
  _def = cgssa->Result_Ref_ID(def);
  _uses.clear();
}

// this is for updater interface
// generate a version, whose def is not known
VERSION::VERSION(VER_ID ver, TN* tn, VERSION_KIND kind)
  : _ver_num(ver), _tn(tn), _kind(kind) {
  Is_True(kind == VERSION_UNKNOWN || kind == VERSION_ENTRY_DEF, 
    ("not right def kind"));
  if (kind == VERSION_UNKNOWN)
    _def = INVALID_REF_ID;
  else
    _def = ENTRY_REF_ID;
  _uses.clear();
}

void
VERSION::Add_Use(REF_ID use) {
  _uses.insert(use);
}

void
VERSION::Delete_Use(REF_ID use) {
  Is_True(_uses.find(use) != _uses.end(), ("no such uses"));
  _uses.erase(use);
}

void
VERSION::Print_Use(FILE *fp) {
  fprintf(fp, "Uses:");
  for (std::set<REF_ID>::iterator uit= _uses.begin();
    uit != _uses.end(); ++uit) {
    fprintf(fp, " %d", *uit);
  }
  fprintf(fp, "\n");
}

//===================================================================
// Implementation for public functions in CGSSA class
//===================================================================

// The print utility functions
void 
CGSSA::Print_Ref_ID_map() 
{
  hash_map<INTPTR, std::vector<REF_ID> >::iterator iter = _ref_id_map.begin();
  for (;iter != _ref_id_map.end(); iter++) {
    fprintf(TFile, "%lx:", iter->first);
    int num_operands = iter->second.size();
    for (int i=0; i<num_operands; i++)
      fprintf(TFile, " %d(%d)", i, iter->second[i]);
    fprintf(TFile, "\n");
  }
}

void 
CGSSA::Print(FILE * file) 
{
  fprintf(file, "=============== CG SSA Dump ===============\n");

  CG_CFG::bb_iterator bb_it;
  for (bb_it = _cfg->BB_begin();
       bb_it != _cfg->BB_end(); ++bb_it) {
    BB_NODE * bb = (*bb_it);

    bb->Print(file, DUMP_ACFG);

    // print the phi_node
    PHI_ITERATOR phi_iter = Phi_Begin(bb);
    for (; phi_iter != Phi_End(); ++phi_iter) {
      phi_iter->Print(file, 0);
      fprintf(file, "\n");
    }

    //print the stmt in SSA format
    for (BB_NODE::stmt_iterator sit = bb->Stmt_begin();
        sit != bb->Stmt_end();
        ++sit) {
      OP* stmt = &(*sit);
      Print_OP_SSA(file, stmt);
      fprintf(file, "\n");
    }
  }
  fprintf(file, "============== CG SSA Dump End ============\n");
}  

void
CGSSA::Print_OP_SSA(FILE * file, OP* stmt) 
{
  for (int i = 0; i < OP_results(stmt); i++) {
    fPrint_TN(file, "%s ", OP_result(stmt,i));
    Print_ver(file,Result_Ver(stmt, i));
  }
  fprintf(file, ":- ");
  fprintf(file, "%s ", TOP_Name(OP_code(stmt)));
  for (int i = 0; i < OP_opnds(stmt); i++) {
    fPrint_TN(file, "%s ", OP_opnd(stmt,i));
    Print_ver(file, Opnd_Ver(stmt, i));
  }
  // print the mu
  MU_ITERATOR cur_mu = Mu_Begin(stmt);
  for (; cur_mu != Mu_End(); ++cur_mu) {
    cur_mu->Print(file, 0);
    fprintf(file, "\t");
  }

  // print the chi 
  CHI_ITERATOR cur_chi = Chi_Begin(stmt);
  for (; cur_chi != Chi_End(); ++cur_chi) {
    fprintf(file, "\n");
    cur_chi->Print(file, 0);
  }
}

void
CGSSA::Print_ver(FILE * file, VER_ID idx) 
{
  fprintf(file, "[v%d] ", idx);
}

// interface functions for the SSA-based optimizations 
PHI_NODE*
CGSSA::Get_Phi_List(BB_NODE* bb) {
  BB_PHI_map_iterator phi_map_iter = _BB_PHI_map.find(bb->Get_id());
  if (phi_map_iter == _BB_PHI_map.end())
    return NULL;

  return phi_map_iter->second.first;
}

CHI_NODE*
CGSSA::Get_Chi_List(OP* op) {
  OP_CHI_map_iterator chi_map_iter = _OP_CHI_map.find((INTPTR) op); 
  if (chi_map_iter == _OP_CHI_map.end())
    return NULL;

  return chi_map_iter->second.first;
}

MU_NODE*
CGSSA::Get_Mu_List(OP* op) {
  OP_MU_map_iterator mu_map_iter = _OP_MU_map.find((INTPTR) op);
  if (mu_map_iter == _OP_MU_map.end())
    return NULL;

  return mu_map_iter->second.first;
}

//Get version id for an op's opnd 
VER_ID
CGSSA::Opnd_Ver(OP* op, INT opnd_idx) {
  Is_True(OP_opnds(op) > opnd_idx, ("operand index is out of range."));
  return Opnd_Ver_((INTPTR)op, opnd_idx);
}

//Get version id for a phi's opnd
VER_ID
CGSSA::Opnd_Ver(PHI_NODE* phi, INT opnd_idx) {
  Is_True(phi->Opnd_count() > opnd_idx, ("operand index is out of range."));
  return Opnd_Ver_((INTPTR)phi, opnd_idx);
}

//Get version id for a chi's opnd
VER_ID
CGSSA::Opnd_Ver(CHI_NODE* chi) {
  return Opnd_Ver_((INTPTR)chi, 0);
}

//Get version id for a mu's opnd
VER_ID
CGSSA::Opnd_Ver(MU_NODE* mu) {
  return Opnd_Ver_((INTPTR)mu, 0);
}

//Get version id for an op's result
VER_ID
CGSSA::Result_Ver(OP* op, INT res_idx) {
  Is_True(OP_results(op) > res_idx, ("result index is out of range."));
  return Result_Ver_((INTPTR)op, res_idx+OP_opnds(op));
}

//Get version id for a phi's result
VER_ID
CGSSA::Result_Ver(PHI_NODE* phi) {
  return Result_Ver_((INTPTR)phi, phi->Opnd_count());
}

//Get version id for a chi's result
VER_ID
CGSSA::Result_Ver(CHI_NODE* chi) {
  return Result_Ver_((INTPTR)chi, 1);
}

// Get the kind (occ, phi, or chi) for a version
VERSION_KIND
CGSSA::Def_kind(VER_ID ver_id) {
  VERSION * version = Get_Version(ver_id);
  Is_True(version != NULL, ("invalid version"));
  return  version->Kind();
}

VERSION_KIND
CGSSA::Def_kind(OP* op, INT opnd_idx) {
  return Def_kind(Opnd_Ver(op, opnd_idx));
}

VERSION_KIND
CGSSA::Def_kind(PHI_NODE * phi, INT opnd_idx) {
  return Def_kind(Opnd_Ver(phi, opnd_idx));
}

VERSION_KIND
CGSSA::Def_kind(CHI_NODE * chi) {
  return Def_kind(Opnd_Ver(chi));
}

VERSION_KIND
CGSSA::Def_kind(MU_NODE* mu) {
  return Def_kind(Opnd_Ver(mu));
}

// Functions to get the defs for a version
void
CGSSA::Get_Occ_Def(VER_ID ver_id, OP** def, INT* def_idx) {
  Is_True(def != NULL, ("NULL def"));

  VERSION * version = Get_Version(ver_id);
  if (version == NULL || version->Kind() != VERSION_OCC_DEF) {
    *def = NULL;
    if (def_idx != NULL)
      *def_idx = 0;
    return;  
  }

  Get_Ref_OP_Result(version->Def(), def, def_idx);  
}

OP* 
CGSSA::Get_Occ_Def(VER_ID ver_id) {
  OP* op;
  Get_Occ_Def(ver_id, &op, NULL);
  return op;
}

void
CGSSA::Get_Occ_Def(OP* op, INT opnd_idx, OP** def, INT * def_idx) {
  Get_Occ_Def(Opnd_Ver(op, opnd_idx), def, def_idx);
}

OP*
CGSSA::Get_Occ_Def(OP* op, INT opnd_idx) {
  return Get_Occ_Def(Opnd_Ver(op, opnd_idx));
}

void 
CGSSA::Get_Occ_Def(PHI_NODE* phi, INT opnd_idx, OP** def, INT * def_idx) {
  Get_Occ_Def(Opnd_Ver(phi, opnd_idx), def, def_idx);
}

OP*
CGSSA::Get_Occ_Def(PHI_NODE* phi, INT opnd_idx) {
  return Get_Occ_Def(Opnd_Ver(phi, opnd_idx));
}

void
CGSSA::Get_Occ_Def(CHI_NODE* chi, OP** def, INT * def_idx) {
  Get_Occ_Def(Opnd_Ver(chi), def, def_idx);
}

void
CGSSA::Get_Occ_Def(MU_NODE* mu, OP** def, INT * def_idx) {
  Get_Occ_Def(Opnd_Ver(mu), def, def_idx);
}

OP*
CGSSA::Get_Occ_Def(CHI_NODE* chi) {
  return Get_Occ_Def(Opnd_Ver(chi));
}

OP*
CGSSA::Get_Occ_Def(MU_NODE* mu) {
  return Get_Occ_Def(Opnd_Ver(mu));
}

PHI_NODE* 
CGSSA::Get_Phi_Def(VER_ID ver_id) {
  VERSION * version = Get_Version(ver_id);
  if (version == NULL || version->Kind() != VERSION_PHI_DEF) {
    return NULL;
  }  

  REF_ID ref_id = version->Def();
  REF_OP_iterator ref_op_iter = _op_idx_map.find(version->Def());
  Is_True(ref_op_iter != _op_idx_map.end(), ("cannot find def"));
  return (PHI_NODE*) ref_op_iter->second.first;
}

PHI_NODE*
CGSSA::Get_Phi_Def(OP* op, INT opnd_idx) {
  return Get_Phi_Def(Opnd_Ver(op, opnd_idx));
}

PHI_NODE*
CGSSA::Get_Phi_Def(PHI_NODE* phi, INT opnd_idx) {
  return Get_Phi_Def(Opnd_Ver(phi, opnd_idx));
}

PHI_NODE*
CGSSA::Get_Phi_Def(CHI_NODE* chi) {
  return Get_Phi_Def(Opnd_Ver(chi));
}

PHI_NODE*
CGSSA::Get_Phi_Def(MU_NODE * mu) {
  return Get_Phi_Def(Opnd_Ver(mu));
}

CHI_NODE*
CGSSA::Get_Chi_Def(VER_ID ver_id) {
  VERSION * version = Get_Version(ver_id);
  if (version == NULL || version->Kind() != VERSION_CHI_DEF) {
    return NULL;
  }

  REF_ID ref_id = version->Def();
  REF_OP_iterator ref_op_iter = _op_idx_map.find(version->Def());
  Is_True(ref_op_iter != _op_idx_map.end(), ("cannot find def"));
  return (CHI_NODE*) ref_op_iter->second.first;
}

CHI_NODE*
CGSSA::Get_Chi_Def(OP* op, INT opnd_idx) {
  return Get_Chi_Def(Opnd_Ver(op, opnd_idx));
}

CHI_NODE*
CGSSA::Get_Chi_Def(PHI_NODE* phi, INT opnd_idx) {
  return Get_Chi_Def(Opnd_Ver(phi, opnd_idx));
}

CHI_NODE*
CGSSA::Get_Chi_Def(CHI_NODE* chi) {
  return Get_Chi_Def(Opnd_Ver(chi));
}

CHI_NODE*
CGSSA::Get_Chi_Def(MU_NODE* mu) {
  return Get_Chi_Def(Opnd_Ver(mu));
}

// Get the use count for a version
UINT32
CGSSA::Use_Count(VER_ID ver_id) {
  VERSION* ver = Get_Version(ver_id);
  Is_True(ver != NULL, ("invalid version"));
  return ver->Use_Cnt();
}

UINT32
CGSSA::Use_Count(OP* op, INT res_idx) {
  return Use_Count(Result_Ver(op, res_idx));
}

UINT32
CGSSA::Use_Count(PHI_NODE* phi) {
  return Use_Count(Result_Ver(phi));
}

UINT32
CGSSA::Use_Count(CHI_NODE* chi) {
  return Use_Count(Result_Ver(chi));
}

// utility functions for a version id
// to consider different cases (op, phi, chi)
UINT32
CGSSA::Opnd_Count(VER_ID id)
{
  VERSION_KIND kind = Def_kind(id);
  if (kind != VERSION_OCC_DEF && kind != VERSION_PHI_DEF)
    return 0;

  if (kind == VERSION_OCC_DEF) {
    OP* op = Get_Occ_Def(id);
    return OP_opnds(op);
  }

  Is_True (kind == VERSION_PHI_DEF, ("Unexpected version kind"));
  PHI_NODE* phi = Get_Phi_Def(id);
  return phi->Opnd_count();
}


CFG_UTIL::CG_CFG::BB_NODE *
CGSSA::Get_Ver_BB(VER_ID ver_id) {
  Is_True(ver_id < _ver_table.size(), ("invalid ver id"));
  switch (Def_kind(ver_id)) {
    case VERSION_OCC_DEF: 
      return Cfg()->Get_BB(Get_Occ_Def(ver_id));
    case VERSION_PHI_DEF: 
      return Cfg()->Get_BB(Get_Phi_Def(ver_id)->BB());
    case VERSION_CHI_DEF:
      return Cfg()->Get_BB(Get_Chi_Def(ver_id)->BB());
    case VERSION_ENTRY_DEF:
      return Cfg()->Get_dummy_entry();
    default:
      Is_True(false, ("invalid ver def"));
      break;
  }
  return NULL;
}

// return the reaching def of a tn at op
// the tn should be a register tn
VER_ID
CGSSA::Reaching_Def(OP* op, TN* tn) {

  Is_True(TN_is_register(tn), ("expect a register tn"));

  BB_NODE * op_bb = Cfg()->Get_BB(op);
  VER_ID last_ver = Get_TN_Last_Ver(TN_number(tn));
  std::vector<VER_ID> dom_defs;
  std::set<VER_ID> save_defs;
  dom_defs.clear();
  save_defs.clear();
  save_defs.insert(last_ver);
  // find all the dominating defs
  while (last_ver != INVALID_VER) {
    BB_NODE * def_bb = Get_Ver_BB(last_ver);
    if ((def_bb == op_bb && Is_Before_(last_ver, op)) 
      || (def_bb != op_bb && def_bb->Dominate(op_bb)))
      dom_defs.push_back(last_ver);
    Is_True(save_defs.find(Get_Version(last_ver)->Prev_ver()) == save_defs.end(), 
      ("def chain"));
    last_ver = Get_Version(last_ver)->Prev_ver();
    save_defs.insert(last_ver);
  }
  // reaching def is the latest dominating def
  return GetLastestDef_(dom_defs);
}

//====================================================================
// Implementation for protected functions in CGSSA
//====================================================================

// functions for phi insertion
// create phi node
PHI_NODE*
CGSSA::Create_Phi(TN_NUM tn_num, INT32 opnd_num) {
  PHI_NODE* phi = Create_node_<CGSSA_PHI>(opnd_num);
  phi->Set_TN_num(tn_num);
  phi->Set_next(NULL);

  // assign Ref_ID for phi
  for (int i = 0; i < opnd_num; i++) {
    New_Opnd_Ref_ID(phi,i);
  }
  New_Result_Ref_ID(phi);

  return phi;
}

void 
CGSSA::Insert_a_Phi_into_a_DF(BB_NODE* df, TN* def) {
    UINT32 bb_id = df->Get_id();
    INT32  tn_num = TN_number(def);

    // whether current TN is already has phi
    pair<BB_TN_map_iterator, BB_TN_map_iterator> tn_range =
        _BB_TN_num_map.equal_range(bb_id);
    for (; tn_range.first != tn_range.second; ++tn_range.first) {
        if (tn_range.first->second == tn_num)
            return;        
    }
    _BB_TN_num_map.insert(BB_TN_map::value_type(bb_id, tn_num));
    PHI_NODE* phi = Create_Phi(tn_num, df->Get_preds_count());

    // insert phi into bb_id
    Insert_a_Phi_into_BB(bb_id, phi);
    // insert PHIs in the dominance frontiers of df for def
    Insert_Phis_for_a_Def(df, def);
}

void 
CGSSA::Insert_a_Phi_into_BB(UINT32 bb_id, PHI_NODE* phi) {
  BB_PHI_map_iterator phi_map_iter = _BB_PHI_map.find(bb_id);
  if (phi_map_iter == _BB_PHI_map.end()) 
    _BB_PHI_map[bb_id] = std::pair<PHI_NODE*, PHI_NODE*>(phi, phi);
  else {
    PHI_NODE* last_phi = phi_map_iter->second.second;
    last_phi->Set_next(phi);
    phi_map_iter->second.second = phi;
  }
  
  phi->Set_BB(bb_id);
}

void 
CGSSA::Delete_a_Phi_from_BB(UINT32 bb_id, PHI_NODE* phi) {
  BB_PHI_map_iterator phi_map_iter = _BB_PHI_map.find(bb_id);
  Is_True (phi_map_iter != _BB_PHI_map.end(), ("PHI not found in _BB_PHI_map"));

  std::pair<PHI_NODE*, PHI_NODE*>* list = &phi_map_iter->second;
  if (list->first == phi) {
    if (list->second == phi) {
      _BB_PHI_map.erase(bb_id);
      return;
    }

    list->first = (PHI_NODE*) phi->Next();
    return;
  }

  PHI_NODE* prev = list->first;
  for (PHI_NODE* i=(PHI_NODE*)prev->Next(); ; i=(PHI_NODE*)i->Next()) {
    if (i == phi) {              // found it
      prev->Set_next(i->Next()); // delete it
      if (i == list->second)
  list->second = prev;
      return;
    }
    if (i == list->second) break;
  }
  
  Is_True(0, ("The PHI is not found in the given BB"));
}

void 
CGSSA::Insert_Phis_for_a_Def(BB_NODE* bb, TN* def) {
  for (BB_NODE::bb_iterator df_it = bb->Df_begin();
       df_it != bb->Df_end(); ++df_it) {
    BB_NODE* df_bb = *df_it;
    Is_True(df_bb != NULL && (bb == df_bb || !bb->Dominate(df_bb)),
            ("dom bb is wrong"));
    Insert_a_Phi_into_a_DF(df_bb, def);
  }
}

// functions for chi insertion
// create chi node
CHI_NODE*
CGSSA::Create_Chi(TN_NUM tn_num) {
  CHI_NODE* chi = Create_node_<CGSSA_CHI>(0);
  chi->Set_next(NULL);
  chi->Set_TN_num(tn_num);

  // assign Ref_ID for chi
  New_Opnd_Ref_ID(chi);
  New_Result_Ref_ID(chi);

  return chi;
}

void
CGSSA::Insert_Chi_into_OP(OP* op, CHI_NODE* chi) {

  OP_CHI_map_iterator chi_map_iter = _OP_CHI_map.find((INTPTR)op);
  if (chi_map_iter == _OP_CHI_map.end())
    _OP_CHI_map[(INTPTR)op] = std::pair<CHI_NODE*, CHI_NODE*>(chi, chi);
  else {
    CHI_NODE* last_chi = chi_map_iter->second.second;
    last_chi->Set_next(chi);
    chi_map_iter->second.second = chi;
  }

  chi->Set_BB(Cfg()->Get_BB(op)->Get_id());

}

CHI_NODE*
CGSSA::Get_Chi_from_OP(OP* op, TN_NUM tn_num) {
  CHI_ITERATOR cur_chi = Chi_Begin(op);
  for (; cur_chi != Chi_End(); ++cur_chi) {
    if (cur_chi->Get_TN_num() == tn_num)
      return &(*cur_chi);
  }

  return NULL;
}

// functions for mu insertion
// create mu node
MU_NODE*
CGSSA::Create_Mu(TN_NUM tn_num) {
  MU_NODE* mu = Create_node_<CGSSA_MU>(0);
  mu->Set_next(NULL);
  mu->Set_TN_num(tn_num);

  // assign Ref_ID for mu 
  New_Opnd_Ref_ID(mu);

  return mu;
}

void
CGSSA::Insert_Mu_into_OP(OP* op, MU_NODE* mu) {

  OP_MU_map_iterator mu_map_iter = _OP_MU_map.find((INTPTR)op);
  if (mu_map_iter == _OP_MU_map.end())
    _OP_MU_map[(INTPTR)op] = std::pair<MU_NODE*, MU_NODE*>(mu, mu);
  else {
    MU_NODE* last_mu = mu_map_iter->second.second;
    last_mu->Set_next(mu);
    mu_map_iter->second.second = mu;
  }

  mu->Set_BB(Cfg()->Get_BB(op)->Get_id());

}

MU_NODE*
CGSSA::Get_Mu_from_OP(OP* op, TN_NUM tn_num) {
  MU_ITERATOR cur_mu = Mu_Begin(op);
  for (; cur_mu!= Mu_End(); ++cur_mu) {
    if (cur_mu->Get_TN_num() == tn_num)
      return &(*cur_mu);
  }
  return NULL;
}

// VERSION related functions
VER_ID 
CGSSA::New_Ver(TN* tn, OP* def, INT32 ith_result) {
  Is_True(def != NULL, ("empty def"));
  Is_True(tn != NULL && TN_is_register(tn), ("invalid tn"));

  VER_ID idx = (VER_ID) _ver_table.size();
  VERSION * new_ver = 
    new VERSION(def, ith_result, idx, tn, VERSION_OCC_DEF, this);
  _ver_table.push_back(new_ver);

  // update def-def chain
  VER_ID last_ver = Get_TN_Last_Ver(TN_number(tn));
  _ver_table[idx]->Set_prev_ver(last_ver);
  Set_TN_Last_Ver(TN_number(tn), idx);

  return idx;
}

VER_ID
CGSSA::New_Ver(TN* tn, PHI_NODE * phi) {
  Is_True(phi != NULL, ("empty def"));
  Is_True(tn != NULL && TN_is_register(tn), ("invalid tn"));

  VER_ID idx = (VER_ID) _ver_table.size();
  VERSION * new_ver = new VERSION(phi, idx, tn, VERSION_PHI_DEF, this);
  _ver_table.push_back(new_ver);

  // update def-def chain
  VER_ID last_ver = Get_TN_Last_Ver(TN_number(tn));
  _ver_table[idx]->Set_prev_ver(last_ver);
  Set_TN_Last_Ver(TN_number(tn), idx);

  return idx;
}

// new version for tn that is defined in chi (entry chi and op chi) 
// it could be either a register tn or a constant tn
VER_ID
CGSSA::New_Ver(TN* tn, CHI_NODE * chi) {
  Is_True(chi != NULL, ("empty def"));

  VER_ID idx = (VER_ID) _ver_table.size();
  VERSION * new_ver = new VERSION(chi, idx, tn, VERSION_CHI_DEF, this);
  _ver_table.push_back(new_ver);

  // update def-def chain
  if (TN_is_register(tn)) {
    Set_TN_Last_Ver(TN_number(tn), idx);
  }  
  return idx;
}

// new version for tn whose def is unknown
// it could be either a register tn or a constant tn
VER_ID
CGSSA::New_Ver(TN* tn, bool entry) {

  VER_ID idx = (VER_ID) _ver_table.size();
  VERSION * new_ver = new VERSION(idx, tn, 
    (entry ? VERSION_ENTRY_DEF:VERSION_UNKNOWN));
  _ver_table.push_back(new_ver);

  // update def-def chain
  if (TN_is_register(tn)) {
    if (entry) {
      VER_ID last_ver = Get_TN_Last_Ver(TN_number(tn));
      _ver_table[idx]->Set_prev_ver(last_ver);
    }
    Set_TN_Last_Ver(TN_number(tn), idx);
  }
  return idx;
}


VERSION*
CGSSA::Get_Version(VER_ID ver_idx) {
  Is_True(ver_idx != INVALID_VER && ver_idx < _ver_table.size(), 
    ("invalid ver idx"));
  return _ver_table[ver_idx];
}

VERSION*
CGSSA::Get_REF_ID_Version(REF_ID ref_id) {
  VER_ID ver_id = Get_Ref_Ver(ref_id);
  if (ver_id == INVALID_VER) return NULL;

  return Get_Version(ver_id);
}

TN*
CGSSA::Get_Version_TN(VER_ID ver_idx) {
  VERSION * version = Get_Version(ver_idx);
  Is_True(version != NULL, ("null version"));

  return version->Get_TN();
}

// Reference id related functions
REF_ID
CGSSA::Opnd_Ref_ID(OP* op, INT opnd_idx) {
  return Ref_ID_((INTPTR)op, opnd_idx);
}

REF_ID
CGSSA::Opnd_Ref_ID(PHI_NODE* phi, INT opnd_idx) {
  return Ref_ID_((INTPTR)phi, opnd_idx);
}

REF_ID
CGSSA::Opnd_Ref_ID(CHI_NODE* chi) {
  return Ref_ID_((INTPTR)chi, 0);
}

REF_ID
CGSSA::Opnd_Ref_ID(MU_NODE* mu) {
  return Ref_ID_((INTPTR)mu, 0);
}

REF_ID
CGSSA::Opnd_Ref_ID(VER_ID ver_id, INT opnd_idx) {
  if (Def_kind(ver_id) == VERSION_PHI_DEF) {
    PHI_NODE* phi = Get_Phi_Def(ver_id);
    return Ref_ID_((INTPTR)phi, opnd_idx);
  }
  
  if (Def_kind(ver_id) == VERSION_OCC_DEF) {
    OP* op = Get_Occ_Def(ver_id);
    return Ref_ID_((INTPTR)op, opnd_idx);
  }
  
  if (Def_kind(ver_id) == VERSION_CHI_DEF) {
    Is_True(opnd_idx == 0, ("Chi has only one opnd"));
    CHI_NODE* chi = Get_Chi_Def(ver_id);
    return Ref_ID_((INTPTR) chi, opnd_idx);
  }  
  return INVALID_REF_ID;
}

REF_ID
CGSSA::Result_Ref_ID(OP* op, INT res_idx) {
  return Ref_ID_((INTPTR)op, res_idx+OP_opnds(op));
}

REF_ID
CGSSA::Result_Ref_ID(PHI_NODE* phi) {
  return Ref_ID_((INTPTR)phi, phi->Opnd_count());
}

REF_ID
CGSSA::Result_Ref_ID(CHI_NODE* chi) {
  return Ref_ID_((INTPTR)chi, 1);
}

REF_ID
CGSSA::New_Opnd_Ref_ID(OP* op, INT opnd_idx) {
  return New_Ref_ID_((INTPTR)op, opnd_idx, OP_opnds(op)+OP_results(op));
}

REF_ID
CGSSA::New_Opnd_Ref_ID(PHI_NODE* phi, INT opnd_idx) {
  return New_Ref_ID_((INTPTR)phi, opnd_idx, phi->Opnd_count()+1);
}

REF_ID
CGSSA::New_Opnd_Ref_ID(CHI_NODE* chi) {
  return New_Ref_ID_((INTPTR)chi, 0, 2);
}

REF_ID
CGSSA::New_Opnd_Ref_ID(MU_NODE* mu) {
  return New_Ref_ID_((INTPTR)mu, 0, 1);
}

REF_ID
CGSSA::New_Result_Ref_ID(OP* op, INT res_idx) {
  return New_Ref_ID_((INTPTR)op, res_idx+OP_opnds(op), 
    OP_opnds(op)+OP_results(op));
}

REF_ID
CGSSA::New_Result_Ref_ID(PHI_NODE*phi) {
  return New_Ref_ID_((INTPTR)phi, phi->Opnd_count(), phi->Opnd_count()+1);
}

REF_ID
CGSSA::New_Result_Ref_ID(CHI_NODE*chi) {
  return New_Ref_ID_((INTPTR)chi, 1, 2);
}

void 
CGSSA::Delete_Opnd_Ref_ID(OP* op, INT opnd_idx)
{
  _op_idx_map.erase(Opnd_Ref_ID(op, opnd_idx));
}

void 
CGSSA::Delete_Opnd_Ref_ID(PHI_NODE* phi, INT opnd_idx)
{
  _op_idx_map.erase(Opnd_Ref_ID(phi, opnd_idx));
}

void 
CGSSA::Delete_Result_Ref_ID(OP* op, INT res_idx)
{
  _op_idx_map.erase(Result_Ref_ID(op, res_idx));
}

void 
CGSSA::Delete_Result_Ref_ID(PHI_NODE* phi)
{
  _op_idx_map.erase(Result_Ref_ID(phi));
}

void
CGSSA::Delete_Ref_ID_map(INTPTR op) {
  // assert
  _ref_id_map.erase(op);
}

void
CGSSA::Set_Ref_Ver(REF_ID ref_id, VER_ID ver_idx) {
  Is_True(ref_id < _num_ref_ids, ("invalid ref id"));
  _ver_map[ref_id] = ver_idx;
}

VER_ID 
CGSSA::Get_Ref_Ver(REF_ID ref_id) {
  Is_True(ref_id < _num_ref_ids, ("invalid ref id"));
  if (_ver_map.find(ref_id) == _ver_map.end())
    return INVALID_VER;
  return _ver_map[ref_id];
}

// Get the op and its defnum (ref_id is a result of op)
void
CGSSA::Get_Ref_OP_Result(REF_ID ref_id, OP**op, INT* defnum) {

  Is_True(op != NULL, ("NULL op"));

  REF_OP_iterator ref_op_iter = _op_idx_map.find(ref_id);
  Is_True(ref_op_iter != _op_idx_map.end(), ("cannot find ref_id"));
  *op = (OP*) ref_op_iter->second.first;
  if (defnum != NULL )
    *defnum = ref_op_iter->second.second - OP_opnds(*op);
}

// Get the op and its opndnum (ref_id is an opnd of op)
void
CGSSA::Get_Ref_OP_Opnd(REF_ID ref_id, OP**op, INT* opndnum) {

  Is_True(op != NULL, ("NULL op"));

  REF_OP_iterator ref_op_iter = _op_idx_map.find(ref_id);
  Is_True(ref_op_iter != _op_idx_map.end(), ("cannot find ref_id"));
  *op = (OP*) ref_op_iter->second.first;
  if (opndnum != NULL )
    *opndnum = ref_op_iter->second.second; 
}

void
CGSSA::Get_Stmt(REF_ID ref_id, INTPTR* stmt, INT* idx, bool* is_OP) {
  Is_True(_op_idx_map.find(ref_id) != _op_idx_map.end(),
    ("ref_id not found(%d)", ref_id));
  std::pair<INTPTR, INT> stmt_idx = _op_idx_map[ref_id];
  *stmt = stmt_idx.first;

  if (stmt_idx.second >= 1000) { // PHI
    *idx = stmt_idx.second - 1000;
    *is_OP = false;
  }
  else { // OP
    *idx = stmt_idx.second;
    *is_OP = true;
  }
}

// maintain the last def (def-def chain)
void
CGSSA::Set_TN_Last_Ver(TN_NUM tn_id, VER_ID ver_id) {
  Is_True(tn_id < _last_ver_table.size(), ("invalid tn id"));
  _last_ver_table[tn_id] = ver_id;
};

VER_ID
CGSSA::Get_TN_Last_Ver(TN_NUM tn_id) {
  Is_True(tn_id < _last_ver_table.size(), ("invalid tn id"));
  return _last_ver_table[tn_id];
};

// functions to support SSA updater
void
CGSSA::Delete_Ver(VER_ID ver_idx) {
  Is_True(ver_idx != INVALID_VER && ver_idx < _ver_table.size(), 
    ("invalid ver idx"));
  free(_ver_table[ver_idx]);
  _ver_table[ver_idx] = NULL;
}

void
CGSSA::Delete_Uses(PHI_NODE* phi) {
  for (int i = 0; i < phi->Opnd_count(); ++i ) {
    VER_ID ver_id = phi->Get_opnd(i);
    VERSION* ver = Get_Version(ver_id);
    REF_ID ref_id = Opnd_Ref_ID(phi, i);
    ver->Delete_Use(ref_id);
    Delete_Opnd_Ref_ID(phi, i);
  }
}

void
CGSSA::Delete_Uses(OP* op) {
  // delete source operands
  for (int i = 0; i < OP_opnds(op); ++i ) {
    VER_ID ver_id = Opnd_Ver(op, i);
    VERSION* ver = Get_Version(ver_id);
    REF_ID ref_id = Opnd_Ref_ID(op, i);
    ver->Delete_Use(ref_id);
    Delete_Opnd_Ref_ID(op, i);
  }
}

VER_ID
CGSSA::SSA_Gen_TN(TN* new_tn) {

  if (TN_is_register(new_tn)) {
    // resize several data structure for register tn 
    _last_ver_table.push_back(INVALID_VER);
    Set_Register_TN_count(Register_TN_count() +1);
  }

  return New_Ver(new_tn);
}

void
CGSSA::SSA_Replace_OP_opnd(OP* op, INT opndnum, VER_ID new_ver_id) {

  Is_True(opndnum < OP_opnds(op), ("invalid opnd"));
  // if the old_ver and new_ver are the same
  if (Opnd_Ver(op, opndnum) == new_ver_id)
    return;

  // SSA information update
  VERSION * new_version = Get_Version(new_ver_id);
  REF_ID old_ref_id = Opnd_Ref_ID(op, opndnum);
  VERSION * old_version = Get_Version(Opnd_Ver(op, opndnum));
  Is_True(new_version != NULL && old_version != NULL,
    ("invalid version"));
  new_version->Add_Use(old_ref_id);
  old_version->Delete_Use(old_ref_id);
  Set_Ref_Ver(old_ref_id, new_ver_id);
}

void
CGSSA::SSA_Replace_OP_result(OP* op, INT defnum, VER_ID new_ver_id) {
  Is_True(defnum < OP_results(op), ("invalid result"));

  if (Result_Ver(op, defnum) == new_ver_id)
    return;

  // SSA information update for def's each use 
  VERSION* old_version = Get_Version(Result_Ver(op, defnum));
  for (VER_USE_iterator use_it = old_version->Use_Begin();
    use_it != old_version->Use_End(); ++ use_it) {
    REF_ID use_ref_id = * (use_it);
    OP* op;
    INT opndnum;
    Get_Ref_OP_Opnd(use_ref_id, &op, &opndnum);
    SSA_Replace_OP_opnd(op, opndnum, new_ver_id);
  }
  // remove the old result version
  SSA_Remove_OP_result(op, defnum);
}



void
CGSSA::SSA_Remove_OP_opnd(OP* op, INT opndnum) {
  Is_True(opndnum < OP_opnds(op), ("invalid opnd"));
  REF_ID old_ref_id = Opnd_Ref_ID(op, opndnum);

  VERSION * old_version = Get_Version(Get_Ref_Ver(old_ref_id));
  Is_True(old_version != NULL, ("null version"));
  old_version->Delete_Use(old_ref_id);

  // clean up maps
  _op_idx_map.erase(old_ref_id);
  _ver_map.erase(old_ref_id);
}

void
CGSSA::SSA_Remove_OP_result(OP* op, INT defnum) {
  Is_True(defnum < OP_results(op), ("invalid def"));
  REF_ID old_ref_id = Result_Ref_ID(op, defnum);

  VERSION * old_version = Get_Version(Get_Ref_Ver(old_ref_id));
  Is_True(old_version != NULL, ("null version"));

  // clean up the maps and delete the version
  _ver_map.erase(old_ref_id);
  _op_idx_map.erase(old_ref_id);
}

void
CGSSA::SSA_New_OP_opnd(OP* op, INT opndnum, VER_ID new_ver) {
  Is_True(opndnum < OP_opnds(op), ("invalid opnd"));
  Is_True(new_ver < _ver_table.size() && new_ver != INVALID_VER, 
    ("invalid version"));

  REF_ID ref_id = New_Opnd_Ref_ID(op, opndnum);
  Set_Ref_Ver(ref_id, new_ver);
  VERSION * new_version = Get_Version(new_ver);
  new_version->Add_Use(ref_id);
}

void
CGSSA::SSA_New_OP_result(OP* op, INT defnum, VER_ID new_ver) {
  Is_True(defnum < OP_results(op), ("invalid def"));
  Is_True(new_ver < _ver_table.size() && new_ver != INVALID_VER, 
    ("invalid version"));
  REF_ID ref_id = New_Result_Ref_ID(op, defnum);
  Set_Ref_Ver(ref_id, new_ver);
  VERSION * new_version = Get_Version(new_ver);

  // change the def to be the current ref_id 
  new_version->Set_Def(ref_id);
  new_version->Set_Kind(VERSION_OCC_DEF);
  // def-def chain update, it is already done at new_ver
  
}

TN_NUM 
CGSSA::Dedicated_TN_Reg_Num(TN_NUM tn_num) {
  Is_True(tn_num <= Last_Dedicated_TN, ("should less than last dedicated TN"));

  return _dedicated_tn_table[tn_num];
}

TN_NUM
CGSSA::Reg_TN_number(TN* tn) {
  Is_True(TN_is_register(tn), ("should be register TN"));

  if (TN_is_dedicated(tn))
    return Dedicated_TN_Reg_Num(TN_number(tn));

  return TN_number(tn);  
}

//===================================================================
// Implementation for private functions in CGSSA class
//===================================================================
void
CGSSA::Clear_Tabs_() {
  _ver_table.clear();

}

// map the same dedicated TNs with different TN_number (e.g., rax with eax) 
// to a same register number
void
CGSSA::Init_Dedicated_TN_() {
  TN_NUM tnum;
  ISA_REGISTER_CLASS rclass;

  for (int i = REGISTER_MIN; i <= Last_Dedicated_TN; i++) {
    TN* tn = TNvec(i);
    tnum = 0;
    FOR_ALL_ISA_REGISTER_CLASS(rclass) {
      if (rclass == TN_register_class(tn))
        break;
      tnum += REGISTER_CLASS_last_register(rclass);  
    }
    _dedicated_tn_table[i] = tnum + TN_register(tn);
  }
}

VER_ID
CGSSA::Opnd_Ver_(INTPTR stmt, INT opnd_idx) {
  REF_ID ref_id = Ref_ID_(stmt, opnd_idx);
  return Get_Ref_Ver(ref_id);
}

VER_ID
CGSSA::Result_Ver_(INTPTR stmt, INT idx) {
  REF_ID ref_id = Ref_ID_(stmt, idx);
  return Get_Ref_Ver(ref_id);
}

REF_ID
CGSSA::Ref_ID_(INTPTR stmt, INT idx) {
  Is_True (_ref_id_map.find(stmt) != _ref_id_map.end(), ("invalid op"));
  std::vector<REF_ID>& opnds = _ref_id_map[stmt];

  return opnds[idx];
}

REF_ID
CGSSA::New_Ref_ID_(INTPTR stmt, INT idx, INT size) {
  REF_ID new_ref_id = _num_ref_ids++;

  if (_ref_id_map.find(stmt) == _ref_id_map.end())
    _ref_id_map[stmt].resize(size);

  _ref_id_map[stmt][idx] = new_ref_id;
  _op_idx_map[new_ref_id] = std::pair<INTPTR, INT>(stmt, idx);

  return new_ref_id;
}

// utility functions for getting reaching def
// return true if the first_ver is before the second_ver
// first_ver and second_ver should be in the same bb
bool
CGSSA::Is_Before_(VER_ID first, VER_ID second) {

  BB_NODE* bb = Get_Ver_BB(first); 
  Is_True(bb == Get_Ver_BB(second), ("must be in same BB"));

  if (bb == Cfg()->Get_dummy_entry())
    return true;

  // phi are first stmts in the block
  if (Def_kind(first) == VERSION_PHI_DEF)
    return true;

  if (Def_kind(second) == VERSION_PHI_DEF)
    return false;

  OP* second_op = Get_Occ_Def(second);
  return Is_Before_(first, second_op);
}

// return true if the first's def is before the op
// first's def and op should be in the same bb
bool
CGSSA::Is_Before_(VER_ID first, OP* op) {

  BB_NODE* bb = Get_Ver_BB(first);
  Is_True(bb == Cfg()->Get_BB(op), ("must be in same BB"));
  Is_True(bb != Cfg()->Get_dummy_entry(), ("could not be entry"));

  // phi are first stmts in the block
  if (Def_kind(first) == VERSION_PHI_DEF)
    return true;

  OP* first_op = Get_Occ_Def(first);
  for (BB_NODE::stmt_iterator sit = bb->Stmt_begin();
    sit != bb->Stmt_end();
    ++sit) {
    OP* stmt = &(*sit);
    if (stmt == first_op) return true;
    if (stmt == op) return false;
  }

  Is_True(false, ("inconsistent in SSA"));
  return false;
}

VER_ID
CGSSA::GetLastestDef_(std::vector<VER_ID> & dom_defs) {
  if (dom_defs.size() == 1)
    return (dom_defs.back());

  VER_ID latest_def = dom_defs.back();
  dom_defs.pop_back();
  for (INT32 i = 0; i < dom_defs.size(); i++) {
    BB_NODE * latest_def_bb = Get_Ver_BB(latest_def);
    BB_NODE * dom_bb = Get_Ver_BB(dom_defs[i]);
    if ((latest_def_bb == dom_bb && Is_Before_(latest_def, dom_defs[i])) 
      || ( latest_def_bb != dom_bb && latest_def_bb->Dominate(dom_bb)))
      latest_def = dom_defs[i];
  }
  return latest_def;
}  

