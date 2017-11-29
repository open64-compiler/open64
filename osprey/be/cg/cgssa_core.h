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
// Module: cgssa_core.h
//
// Description:
//   Core data structures for CGSSA
// 
// Exported Classes:
//   CGSSA_NAME::PHI_NODE 
//   CGSSA_NAME::CHI_NODE 
//   CGSSA_NAME::MU_NODE 
//   CGSSA_NAME::VERSION
//   CGSSA_NAME::CGSSA   
//
// ====================================================================

#ifndef cgssa_core_INCLUDED
#define cgssa_core_INCLUDED

#include <stdio.h>
#include "cg_cfg.h"
#include "tn.h"
#include <ext/hash_map>
#include "cgexp.h"
using __gnu_cxx::hash_multimap;

using namespace CFG_UTIL;

namespace CGSSA_NAME {

typedef UINT32 REF_ID;
typedef UINT32 VER_ID;

#define INVALID_VER     ((UINT32)-1)
#define ZERO_VER        0 

#define INVALID_REF_ID  ((UINT32)-1)
#define ENTRY_REF_ID  0

// forward declaration
class PHI_NODE;
class CHI_NODE;
class MU_NODE;
class CGSSA;

class PREG_INTEREST;
template<typename _Tinterest>
class CGSSA_PHI_INSERT;
template<typename _Tinterest>
class CGSSA_RENAME;
template<typename _Tinterest>
class CGSSA_LEAVE;

enum CGSSA_NODE_KIND {
  CGSSA_UNKNOWN,    /* Unknown */
  CGSSA_PHI,        /* phi node */
  CGSSA_CHI,        /* chi node */
  CGSSA_MU,         /* mu node */
};

enum VERSION_KIND {
  VERSION_UNKNOWN,
  VERSION_OCC_DEF,  /* version is defined in OP */
  VERSION_PHI_DEF,  /* version is defined in PHI */
  VERSION_CHI_DEF,  /* version is defined in CHI */
  VERSION_ENTRY_DEF,  /* version is defined in the entry*/
};

enum CGSSA_DEL_USES {
  CGSSA_DELETE_USES,    /* default */
  CGSSA_USES_DELETED,   /* use counts are not decreased with this */
};

class PHI_NODE_TRAITS {
public:
  enum { NODE_KIND = CGSSA_PHI };
  enum { RES_COUNT = 1, OPND_COUNT = -1 };
};

class CHI_NODE_TRAITS {
public:
  enum { NODE_KIND = CGSSA_CHI };
  enum { RES_COUNT = 1, OPND_COUNT = 1 };
};

class MU_NODE_TRAITS {
public:
  enum { NODE_KIND = CGSSA_MU };
  enum { RES_COUNT = 0, OPND_COUNT = 1 };
};

template<CGSSA_NODE_KIND _Tkind> 
struct NODE_TO_TYPES;

template<> 
struct NODE_TO_TYPES<CGSSA_PHI> {
   typedef PHI_NODE_TRAITS NODE_TRAITS;
   typedef PHI_NODE   NODE_TYPE;
};

template<>
struct NODE_TO_TYPES<CGSSA_CHI> {
   typedef CHI_NODE_TRAITS NODE_TRAITS;
   typedef CHI_NODE   NODE_TYPE;
};

template<>
struct NODE_TO_TYPES<CGSSA_MU> {
   typedef MU_NODE_TRAITS NODE_TRAITS;
   typedef MU_NODE   NODE_TYPE;
};

//===================================================================
// CGSSA NODE
// SSA_NODE_BASE is the base class, which PHI_NODE and CHI_NODE are
// derived from.
// Currently, WSSA and CGSSA both have SSA_NODE_BASE, which should be
// combined into one base class
//===================================================================
template<typename _Ttraits>
class SSA_NODE_BASE {
private:
  SSA_NODE_BASE<_Ttraits>* _next;  // pointer for the next node on the list
  TN_NUM  _tn_num;      // tn number for this phi/chi
  UINT32 _bb_id;        // BB to which this PHI belongs
  INT32 _opnds[0];      // variant length
                        // phi: 1 for res + 1 for num of opnds + num
                        // chi: 1 for res + 1 for opnd

private:
  // convert operand index to location in _opnds array
  INT32 opnd_idx_to_store_loc(INT32 idx) const {
    Is_True(idx >= 0 && idx < Opnd_count(), ("bad opnd index"));
    if (_Ttraits::OPND_COUNT != -1)
      return _Ttraits::RES_COUNT + idx;
    else
      return _Ttraits::RES_COUNT + 1 /* num of opnds */ + idx;
  }

  // no implementation for the following methods
  SSA_NODE_BASE();
  SSA_NODE_BASE(const SSA_NODE_BASE<_Ttraits>& lhs);
  SSA_NODE_BASE<_Ttraits>& operator=(const SSA_NODE_BASE<_Ttraits>& lhs);
  void* operator new(std::size_t size);
  void* operator new[] (std::size_t size);
  template<CGSSA_NODE_KIND _Tkind>
  typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* Create_node(INT32 opnd_num = 0);

public:
  static const CGSSA_NODE_KIND NODE_KIND = (CGSSA_NODE_KIND)_Ttraits::NODE_KIND;

public:
  UINT32 BB(){ return _bb_id; }
  void Set_BB(UINT32 bb_id) {
    _bb_id = bb_id;
  }

  // number of result and operands
  INT32 Res_count() const { return _Ttraits::RES_COUNT; }
  INT32 Opnd_count() const {
    if (_Ttraits::OPND_COUNT != -1)
      return _Ttraits::OPND_COUNT;
    else
      return _opnds[_Ttraits::RES_COUNT];
  }
  void Set_opnd_count(INT32 opnds) {
    Is_True(_Ttraits::OPND_COUNT == -1, ("can not set opnd count"));
    _opnds[_Ttraits::RES_COUNT] = opnds;
  }

  // result
  VER_ID Get_res(INT32 idx) const {
    Is_True(idx >= 0 && idx < Res_count(), ("bad res index"));
    return (VER_ID)_opnds[idx];
  }
  void Set_res(INT32 idx, VER_ID ver) {
    Is_True(idx >= 0 && idx < Res_count(), ("bad res index"));
    _opnds[idx] = ver;
  }

  // operands
  VER_ID Get_opnd(INT32 idx) const {
    INT32 loc = opnd_idx_to_store_loc(idx);
    return (VER_ID)_opnds[loc];
  }
  void Set_opnd(INT32 idx, VER_ID ver) {
    INT32 loc = opnd_idx_to_store_loc(idx);
    _opnds[loc] = ver;
  }

  // next node
  SSA_NODE_BASE<_Ttraits>* Next() const {
    return _next;
  }
  void Set_next(SSA_NODE_BASE<_Ttraits>* next) {
    _next = next;
  }

  void Set_TN_num(TN_NUM tn_num) {_tn_num = tn_num;}
  TN_NUM Get_TN_num()           { return _tn_num; }

public:
  // routines for debugging
  void Print(FILE* fp, int indent) const {
    if (Res_count() > 0) {
       Print_ver(fp, Get_res(0));
       fprintf(fp, " = ");
    }
    if (NODE_KIND == CGSSA_PHI)
      fprintf(fp, "PHI(");
    else if (NODE_KIND == CGSSA_CHI)
      fprintf(fp, "CHI(");
    else if (NODE_KIND == CGSSA_MU)
      fprintf(fp, "MU(");
    else
      fprintf(fp, "unknow\n");

    int i = 0;
    for ( ; i < Opnd_count() - 1; ++i) {
       Print_ver(fp, Get_opnd(i));
       fprintf(fp, ", ");
    }
    if (i < Opnd_count()) {
       Print_ver(fp, Get_opnd(i));
    }
    fprintf(fp, ") // TN#: %d", _tn_num);
  }

  void Print_ver(FILE* fp, VER_ID idx) const {
    fprintf(fp, "[v%d]", idx);
  }

  void Verify() const {
  }
}; /* SSA_NODE_BASE */

class PHI_NODE : public SSA_NODE_BASE<PHI_NODE_TRAITS> {
//private:
  //UINT32 _bb_id;        // BB to which this PHI belongs

public:
  typedef SSA_NODE_BASE<PHI_NODE_TRAITS> SSA_NODE;

  VER_ID Res() const       { return SSA_NODE::Get_res(0); }
  void Set_res(VER_ID ver) { SSA_NODE::Set_res(0, ver); }

  VER_ID Opnd(int idx) const { return SSA_NODE::Get_opnd(idx); }
  void Set_opnd(int idx, VER_ID ver) { SSA_NODE::Set_opnd(idx, ver); }

}; /* PHI_NODE */

class CHI_NODE : public SSA_NODE_BASE<CHI_NODE_TRAITS> {
public:
  typedef SSA_NODE_BASE<CHI_NODE_TRAITS> SSA_NODE;

  VER_ID Res() const        { return SSA_NODE::Get_res(0); }
  void Set_res(VER_ID ver) { SSA_NODE::Set_res(0, ver); }

  VER_ID Opnd() const       { return SSA_NODE::Get_opnd(0);}
  void Set_opnd(VER_ID ver) { SSA_NODE::Set_opnd(0, ver); }
}; /* CHI_NODE */

class MU_NODE : public SSA_NODE_BASE<MU_NODE_TRAITS> {
public:
  typedef SSA_NODE_BASE<MU_NODE_TRAITS> SSA_NODE;

  VER_ID Opnd() const       { return SSA_NODE::Get_opnd(0);}
  void Set_opnd(VER_ID ver) { SSA_NODE::Set_opnd(0, ver); }
}; /* MU_NODE */

// Iterate the ssa nodes following the _next field
template <typename _Tnode>
class CGSSA_NODE_ITERATOR {
private:
  _Tnode* _cur_node;

public:
  CGSSA_NODE_ITERATOR(_Tnode* first_node)
    : _cur_node(first_node) { }
  // using default copy constructor

  _Tnode* Get_node() {
    return _cur_node;
  }
  _Tnode* operator->() {
    return _cur_node;
  }
  _Tnode& operator*() {
    return *_cur_node;
  }
  CGSSA_NODE_ITERATOR& operator++() {
    Is_True(_cur_node != NULL, ("cur node is invalid"));
    _cur_node = (_Tnode*)_cur_node->Next();
  }
  bool operator==(const _Tnode* node) {
    return _cur_node == node;
  }
  bool operator!=(const _Tnode* node) {
    return _cur_node != node;
  }
  void operator=(const _Tnode* node) {
    _cur_node = node;
  }
};

//===================================================================
// SSA Version definition
//===================================================================
class VERSION {
private:
  VER_ID _ver_num;        // version id, unique accross all TNs
  REF_ID _def;            // to maintain use-def chain 
  std::set<REF_ID> _uses; // to maintain def-use chain
  VER_ID _prev_ver;       // to maintain def-def chain
  VERSION_KIND _kind;
  TN*   _tn;              // which TN this version belongs to

public:
  typedef std::set<REF_ID>::iterator  use_iterator;

  VERSION(OP*, INT32, VER_ID, TN*, VERSION_KIND, CGSSA*);
  VERSION(PHI_NODE*, VER_ID, TN*, VERSION_KIND, CGSSA*);
  VERSION(CHI_NODE*, VER_ID, TN*, VERSION_KIND, CGSSA*);
  VERSION(VER_ID, TN*, VERSION_KIND);

  ~VERSION(){};

  void    Set_Def(REF_ID def) { _def = def; }
  REF_ID  Def()               { return _def; }

  void    Set_TN(TN* tn)      { _tn = tn; }
  TN*     Get_TN()            { return _tn; }

  void    Set_prev_ver(VER_ID ver)  {_prev_ver = ver;};
  VER_ID  Prev_ver()                { return _prev_ver;}

  void    Set_Kind(VERSION_KIND kind) { _kind = kind; }
  VERSION_KIND  Kind()                { return _kind; }

  VER_ID  Get_Ver()      { return _ver_num; }

  UINT32  Use_Cnt() { return _uses.size(); }

  void    Add_Use(REF_ID);
  void    Delete_Use(REF_ID);
  void    Print_Use(FILE *);

  use_iterator  Use_Begin() { return _uses.begin(); };
  use_iterator  Use_End()   { return _uses.end(); };
}; /* VERSION */

//===================================================================
// CGSSA class definition
//===================================================================
class CGSSA {
public:
  typedef CFG_UTIL::CG_CFG::BB_NODE BB_NODE;
  typedef CGSSA_NODE_ITERATOR<PHI_NODE> PHI_ITERATOR;
  typedef CGSSA_NODE_ITERATOR<CHI_NODE> CHI_ITERATOR;
  typedef CGSSA_NODE_ITERATOR<MU_NODE> MU_ITERATOR;
  typedef hash_multimap<UINT32, TN_NUM> BB_TN_map;
  typedef BB_TN_map::iterator BB_TN_map_iterator;
  typedef hash_map<UINT32, std::pair<PHI_NODE*, PHI_NODE*> > BB_PHI_map;
  typedef BB_PHI_map::iterator BB_PHI_map_iterator;
  typedef std::vector<VERSION*> VERSION_TABLE;
  typedef VERSION_TABLE::iterator VER_iterator;
  typedef VERSION::use_iterator VER_USE_iterator;

  typedef hash_map<REF_ID, std::pair<INTPTR, INT> > REF_OP_map;
  typedef REF_OP_map::iterator REF_OP_iterator;

  typedef hash_map<INTPTR, std::pair<CHI_NODE*, CHI_NODE*> > OP_CHI_map;
  typedef OP_CHI_map::iterator OP_CHI_map_iterator;

  typedef hash_map<INTPTR, std::pair<MU_NODE*, MU_NODE*> > OP_MU_map;
  typedef OP_MU_map::iterator OP_MU_map_iterator;

private:
  CFG_UTIL::CG_CFG* _cfg;
  MEM_POOL& _ssa_pool;

  // the number of register tn
  UINT32  _register_tn_count;

  // the next ref_id to be assigned to
  REF_ID _num_ref_ids;

  // the followings are all kinds of tables and maps to maintain
  // version, phi/chi information

  // - Map from a BB_NODE ID to a set of TN numbers 
  //   being defined by the PHIs in the BB
  // - Used to prevent inserting multiple PHIs for the same TN number
  //   during the PHI insertion phase
  BB_TN_map _BB_TN_num_map;

  // - Map from a BB_NODE ID to the first and last PHIs in the BB_NODE
  // - Used to iterate through the PHIs in a given BB
  BB_PHI_map _BB_PHI_map;

  // - Map from a OP to the first and last CHIs in the OP
  OP_CHI_map _OP_CHI_map;

  // - Map from a OP to the first and last MUs in the OP
  OP_MU_map _OP_MU_map;

  // REF_ID -> VER_ID
  hash_map<REF_ID, VER_ID> _ver_map;

  // Reference ID map:
  // OP* -> std::vector<reference ID>
  // - provides a unique ID for each TN operand of an OP
  // - also will add a reference ID for may use if OP has 
  // (OP is conditional def or def is a dedicated register)
  hash_map<INTPTR, std::vector<REF_ID> > _ref_id_map;
  // REF_ID -> <OP*/PHI_NODE*, operand index>
  REF_OP_map _op_idx_map;

  // the last ver for a TN (def-def chain)
  std::vector<VER_ID> _last_ver_table;

  // for version entries
  VERSION_TABLE _ver_table;

  std::vector<TN_NUM> _dedicated_tn_table;

  bool _build_dedicated_reg;

public:
  // constructor
  CGSSA(CFG_UTIL::CG_CFG* cfg, MEM_POOL& pool) :
    _cfg(cfg), _ssa_pool(pool), _num_ref_ids(ENTRY_REF_ID+1) {

    Set_Register_TN_count(Last_TN+1);
    _last_ver_table.resize(Register_TN_count());
    _dedicated_tn_table.resize(Last_Dedicated_TN+1);

    // initialize the last_tn to be invalid_ver
    for(int i= 0; i <= Last_TN; i++) {
      Set_TN_Last_Ver(i, INVALID_VER);
    }

    // initialize the dedicated tn table
    Init_Dedicated_TN_();
  }
  // deconstructor
  ~CGSSA(){}

  // The entry function to build CGSSA
  void Build(bool dedicated_reg=true); 
  // The entry function to leave CGSSA - remove overlapping live range
  void Leave(); 

  // The print utility functions
  void Print(FILE*);
  void Print_OP_SSA(FILE*, OP*);
  void Print_ver(FILE*, VER_ID);
  void Print_Ref_ID_map();

  // data member accessing functions
  CFG_UTIL::CG_CFG* Cfg() { return _cfg; }

  UINT32 Register_TN_count() { return _register_tn_count; }
  void   Set_Register_TN_count(UINT32 count) { _register_tn_count = count; }

  // interface functions for the SSA-based optimizations
  VER_ID Last_Vid() { return _ver_table.size()-1; }

  // Phi Node iterator for a BB
  PHI_NODE* Phi_Begin(BB_NODE* bb) { return Get_Phi_List(bb); };
  PHI_NODE* Phi_End() { return NULL; };
  PHI_NODE* Get_Phi_List(BB_NODE* bb);

  // Chi Node iterator for an OP
  CHI_NODE* Chi_Begin(OP* op) { return Get_Chi_List(op); };
  CHI_NODE* Chi_End() { return NULL; };
  CHI_NODE* Get_Chi_List(OP* op);

  // Mu Node iterator for an OP
  MU_NODE* Mu_Begin(OP* op) { return Get_Mu_List(op); };
  MU_NODE* Mu_End() { return NULL; };
  MU_NODE* Get_Mu_List(OP* op);

  // Get Version id for an op's opnd or result
  VER_ID Opnd_Ver(OP* op, INT opnd_idx);
  VER_ID Opnd_Ver(PHI_NODE* phi, INT opnd_idx);
  VER_ID Opnd_Ver(CHI_NODE* chi);
  VER_ID Opnd_Ver(MU_NODE* mu);
  VER_ID Result_Ver(OP* op, INT res_idx);
  VER_ID Result_Ver(PHI_NODE* phi);
  VER_ID Result_Ver(CHI_NODE* chi);

  // Get the kind (occ, phi, or chi) for a version
  // A version can be represented by version id, or opnd in op or 
  // opnd in phi or opnd in chi. 
  VERSION_KIND Def_kind(VER_ID ver_id);
  VERSION_KIND Def_kind(OP* op, INT opnd_idx);
  VERSION_KIND Def_kind(PHI_NODE* phi, INT opnd_idx);
  VERSION_KIND Def_kind(CHI_NODE* chi);
  VERSION_KIND Def_kind(MU_NODE* chi);

  // Get the version entry for a version id
  VERSION*  Get_Version(VER_ID ver_id);
  // Get the TN associated with a version 
  TN*   Get_Version_TN(VER_ID ver_id);

  // Functions to get the defs for a version.
  // Def can be real op, can be phi, or chi. Use different functions
  // accordingly. 

  // Get the op def for a version (not care idx in the op)
  // If the def is not an op (i.e., phi or chi), return NULL 
  OP*  Get_Occ_Def(VER_ID ver_id);
  OP*  Get_Occ_Def(OP* op, INT opnd_idx);
  OP*  Get_Occ_Def(PHI_NODE* phi, INT opnd_idx);
  OP*  Get_Occ_Def(CHI_NODE* chi);
  OP*  Get_Occ_Def(MU_NODE* mu);

  // Get the op def and idx for a version
  void Get_Occ_Def(VER_ID ver_id, OP**def, INT* def_idx);
  void Get_Occ_Def(OP* op, INT opnd_idx, OP**def, INT* def_idx);
  void Get_Occ_Def(PHI_NODE* op, INT opnd_idx, OP**def, INT* def_idx);
  void Get_Occ_Def(CHI_NODE* op, OP**def, INT* def_idx);
  void Get_Occ_Def(MU_NODE* op, OP**def, INT* def_idx);

  // Get phi def for a version 
  // if the def is not a phi, return NULL
  PHI_NODE* Get_Phi_Def(VER_ID ver_id);
  PHI_NODE* Get_Phi_Def(OP*op, INT opnd_idx);
  PHI_NODE* Get_Phi_Def(PHI_NODE* phi, INT opnd_idx);
  PHI_NODE* Get_Phi_Def(CHI_NODE* chi);
  PHI_NODE* Get_Phi_Def(MU_NODE* mu);

  CHI_NODE* Get_Chi_Def(VER_ID ver_id);
  CHI_NODE* Get_Chi_Def(OP*op, INT opnd_idx);
  CHI_NODE* Get_Chi_Def(PHI_NODE* phi, INT opnd_idx);
  CHI_NODE* Get_Chi_Def(CHI_NODE* chi);
  CHI_NODE* Get_Chi_Def(MU_NODE* mu);
  
  // Get the use count for a version
  // The version here is represented by a version id, or
  // a def in an op, or the def in a phi 
  UINT32 Use_Count(VER_ID ver_id);
  UINT32 Use_Count(OP* op, INT res_idx);
  UINT32 Use_Count(PHI_NODE* phi);
  UINT32 Use_Count(CHI_NODE* chi);

  // utility functions for a version id
  // to consider different cases (op, phi, chi)
  UINT32 Opnd_Count(VER_ID id);
  BB_NODE* Get_Ver_BB(VER_ID id);

  // Reference id related functions
  REF_ID Opnd_Ref_ID(OP* op, INT opnd_idx);
  REF_ID Opnd_Ref_ID(PHI_NODE* op, INT opnd_idx);
  REF_ID Opnd_Ref_ID(CHI_NODE* op);
  REF_ID Opnd_Ref_ID(MU_NODE* op);
  REF_ID Opnd_Ref_ID(VER_ID ver_id, INT opnd_idx);

  // utility function to get the reaching def of tn at op point
  // the version id is returned
  VER_ID  Reaching_Def(OP* op, TN* tn);

  bool  Build_Dedicated_Reg() { return _build_dedicated_reg; };

  friend class VERSION;
  friend class CGSSA_PHI_INSERT<PREG_INTEREST>;
  friend class CGSSA_RENAME<PREG_INTEREST>;
  friend class CGSSA_LEAVE<PREG_INTEREST>;
  friend class CGSSA_UPDATER;

protected:
  // protected functions are for friend class of CGSSA

  // functions for phi insertion
  PHI_NODE* Create_Phi(TN_NUM tn_num, INT32 kids);
  void Insert_a_Phi_into_a_DF(BB_NODE* df, TN* def);
  void Insert_a_Phi_into_BB(UINT32 bb_id, PHI_NODE* phi);
  void Delete_a_Phi_from_BB(UINT32 bb_id, PHI_NODE* phi);
  void Insert_Phis_for_a_Def(BB_NODE* bb, TN* def);
  
  // functions for chi insertion
  CHI_NODE* Create_Chi(TN_NUM tn_num);
  void Insert_Chi_into_OP(OP* op, CHI_NODE* chi);
  CHI_NODE* Get_Chi_from_OP(OP* op, TN_NUM tn_num);

   // functions for mu insertion
  MU_NODE* Create_Mu(TN_NUM tn_num);
  void Insert_Mu_into_OP(OP* op, MU_NODE* chi);
  MU_NODE* Get_Mu_from_OP(OP* op, TN_NUM tn_num);
 
  // VERSION related functions
  VER_ID New_Ver(TN* tn, OP *op, INT32 opnd_idx);
  VER_ID New_Ver(TN* tn, PHI_NODE* phi);
  VER_ID New_Ver(TN* tn, CHI_NODE* chi);
  VER_ID New_Ver(TN* tn, bool entry=false);

  VERSION*  Get_REF_ID_Version(REF_ID);

  // Reference id is used to represent a position in op, phi, or chi,
  // which can be a opnd or result in op, phi or chi.
  // It is a handy representation for a pair (op/phi/chi, idx).
  // Currently reference id is not exposed to the optimization clients. 
  // We can do that if necessary.

  // Reference id related functions
  //REF_ID Opnd_Ref_ID(OP* op, INT opnd_idx);
  //REF_ID Opnd_Ref_ID(PHI_NODE* op, INT opnd_idx);
  //REF_ID Opnd_Ref_ID(VER_ID ver_id, INT opnd_idx);
  REF_ID Result_Ref_ID(OP* op, INT res_idx);
  REF_ID Result_Ref_ID(PHI_NODE* op);
  REF_ID Result_Ref_ID(CHI_NODE* op);

  REF_ID New_Opnd_Ref_ID(OP* op, INT opnd_idx);
  REF_ID New_Opnd_Ref_ID(PHI_NODE* op, INT opnd_idx);
  REF_ID New_Opnd_Ref_ID(CHI_NODE* op);
  REF_ID New_Opnd_Ref_ID(MU_NODE* op);
  REF_ID New_Result_Ref_ID(OP* op, INT res_idx);
  REF_ID New_Result_Ref_ID(PHI_NODE* op);
  REF_ID New_Result_Ref_ID(CHI_NODE* op);

  void Delete_Opnd_Ref_ID(OP* op, INT opnd_idx);
  void Delete_Opnd_Ref_ID(PHI_NODE* phi, INT opnd_idx);
  void Delete_Result_Ref_ID(OP* op, INT res_idx);
  void Delete_Result_Ref_ID(PHI_NODE* phi);

  void Delete_Ref_ID_map(INTPTR op);

  void   Set_Ref_Ver(REF_ID ref_id, VER_ID ver_idx);
  VER_ID Get_Ref_Ver(REF_ID ref_id);

  void Get_Ref_OP_Result(REF_ID ref_id, OP**op, INT* defnum);
  void Get_Ref_OP_Opnd(REF_ID ref_id, OP**op, INT* opndnum);

  void Get_Stmt(REF_ID, INTPTR*, INT*, bool*);

  // maintain the last def (def-def chain)
  void    Set_TN_Last_Ver(TN_NUM tn_id, VER_ID ver_id);
  VER_ID  Get_TN_Last_Ver(TN_NUM tn_id);

  // functions to support SSA updater
  void Delete_Ver(VER_ID ver_id);
  void Delete_Use(VER_ID, INT);
  void Delete_Uses(PHI_NODE* phi);
  void Delete_Uses(OP* op);

  VER_ID SSA_Gen_TN(TN* new_tn);

  void  SSA_Remove_OP_opnd(OP* op, INT opndnum);
  void  SSA_Remove_OP_result(OP* op, INT defnum);

  void  SSA_New_OP_opnd(OP* op, INT opndnum, VER_ID ver_id);
  void  SSA_New_OP_result(OP* op, INT defnum, VER_ID ver_id);

  void  SSA_Replace_OP_opnd(OP* op, INT opndnum, VER_ID new_ver_id);
  void  SSA_Replace_OP_result(OP* op, INT defnum, VER_ID new_ver_id);

  TN_NUM Dedicated_TN_Reg_Num(TN_NUM tn_num);
  TN_NUM Reg_TN_number(TN* tn);

  void Build_Dedicated_Reg(bool flag) { _build_dedicated_reg = flag; };

private:

  // clean up the data structure 
  void Clear_Tabs_();

  // initialize the dedicated tn table
  void Init_Dedicated_TN_();

  // function to create phi/chi/mu node
  template<CGSSA_NODE_KIND _Tkind>
  typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* Create_node_(INT32 opnd_num) {
    INT32 node_size;
    typedef typename NODE_TO_TYPES<_Tkind>::NODE_TRAITS NODE_TRAITS;
    typedef typename NODE_TO_TYPES<_Tkind>::NODE_TYPE NODE_TYPE;
    if (NODE_TRAITS::OPND_COUNT != -1) {
      node_size = sizeof(NODE_TYPE) + sizeof(TN_NUM) +
        (NODE_TRAITS::RES_COUNT + NODE_TRAITS::OPND_COUNT) * sizeof(INT32);
    }
    else {
      node_size = sizeof(NODE_TYPE) + sizeof(TN_NUM) +
        (NODE_TRAITS::RES_COUNT + opnd_num +1) * sizeof(INT32);
      }
    NODE_TYPE* node = (NODE_TYPE*) MEM_POOL_Alloc(&_ssa_pool, node_size);
    memset(node, 0, node_size);
    if (NODE_TRAITS::OPND_COUNT == -1)
      node->Set_opnd_count(opnd_num);
      return node;
  }

  // functions to get version id for a stmt, which can be an op, phi, chi
  VER_ID Opnd_Ver_(INTPTR stmt, INT opnd_idx);
  VER_ID Result_Ver_(INTPTR stmt, INT res_idx);

  REF_ID Ref_ID_(INTPTR stmt, INT idx);
  REF_ID New_Ref_ID_(INTPTR stmt, INT idx, INT size);

  // functions to get reaching def
  VER_ID GetLastestDef_(std::vector<VER_ID> &dom_defs);
  bool  Is_Before_(VER_ID, VER_ID);
  bool  Is_Before_(VER_ID, OP*);

}; /* CGSSA */

} /* namespace CGSSA_NAME */
#endif /* cgssa_core_INCLUDED */

