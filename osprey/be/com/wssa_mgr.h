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
// Module: wssa_mgr.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Interface for WHIRL SSA MANAGER
//
// Exported classes:
//  WSSA::WHIRL_SSA_MANAGER
//  WSSA::WSSA_UD_ITERATOR
//
// SEE ALSO:
//  be/com/wssa_core.h (PHI_NODE, CHI_NODE, MU_NODE)
//  be/com/wssa_sym.h  (WST_Symbol_Entry, WST_Version_Entry)
//
//====================================================================

#ifndef wssa_mgr_INCLUDED
#define wssa_mgr_INCLUDED

#include "wssa_defs.h"
#include "wssa_core.h"
#include "wssa_sym.h"
#include "wssa_wn.h"
#include "wn.h"
#include "symtab.h"
#include "opt_points_to.h"
#include "cxx_memory.h"
#include <list>

namespace WSSA {

struct Input_Buffer;
class  WSSA_UD_ITERATOR;
typedef hash_map<ST_IDX, WST_IDX> ST_TO_WST_MAP;

//===================================================================
// MGR_STAT: WHIRL_SSA_MANAGER internal status
//   if internal status is not STAT_OK, can not query the SSA info
//===================================================================
enum MGR_STAT {
  STAT_INIT,    // just initialized
  STAT_EMIT,    // in the Pre_OPT emitter
  STAT_UPDATE,  // in the SSA Updater
  STAT_VERIFY,  // in the SSA verifier
  STAT_DUMP,    // in dump_tree/wn without ssa
  STAT_DIRTY,   // SSA information is dirty
  STAT_OK,      // OK, ready for normal operations
};


//===================================================================
// WHIRL_SSA_MANAGER
//   manage all SSA nodes, symbols, versions
//   manage mappings between WHIRL nodes and SSA nodes
//===================================================================
class WHIRL_SSA_MANAGER {
public:
  friend class WHIRL_SSA_IO;
  friend class WSSA_UPDATER;
  template<typename _Tmgr, typename _Tnode> friend class WSSA_NODE_ITERATOR;
  template<typename _Tnode> friend void read_from_buffer(Input_Buffer *ib, _Tnode*& node);
  typedef hash_map<WSSA_WN_IDX, WN*> WN_IDX_MAP;

  typedef WSSA_NODE_ITERATOR<WHIRL_SSA_MANAGER, PHI_NODE> phi_iterator;
  typedef WSSA_NODE_ITERATOR<WHIRL_SSA_MANAGER, CHI_NODE> chi_iterator;
  typedef WSSA_NODE_ITERATOR<WHIRL_SSA_MANAGER, MU_NODE>  mu_iterator;
  typedef WSSA_NODE_ITERATOR<const WHIRL_SSA_MANAGER, const PHI_NODE> const_phi_iterator;
  typedef WSSA_NODE_ITERATOR<const WHIRL_SSA_MANAGER, const CHI_NODE> const_chi_iterator;
  typedef WSSA_NODE_ITERATOR<const WHIRL_SSA_MANAGER, const MU_NODE>  const_mu_iterator;

  typedef WST_SYM_TABLE::iterator sym_iterator;
  typedef WST_SYM_TABLE::const_iterator const_sym_iterator;
  typedef WST_VER_TABLE::iterator ver_iterator;
  typedef WST_VER_TABLE::const_iterator const_ver_iterator;

private:
  BOOL _trace;
  MGR_STAT _stat;
  WN* _root;
  MEM_POOL*  _ssa_pool;
  WN_IDX_MAP _wn_idx_map;  // wn_idx -> WN*

private:
  // these fields need to write to file
  PHI_TABLE _phi_table;
  CHI_TABLE _chi_table;
  MU_TABLE  _mu_table;

  WN_PHI_MAP _phi_map;     // wn -> phi nodes
  WN_CHI_MAP _chi_map;     // wn -> chi nodes
  WN_MU_MAP  _mu_map;      // wn -> mu nodes
  WN_VER_MAP _wn_ver_map;  // wn -> version

  WST_FIELD_TABLE _field_table;
  WST_VSYM_TABLE  _vsym_table;
  WST_SYM_TABLE   _sym_table;
  WST_VER_TABLE   _ver_table;

  WST_IDX _ret_vsym_wst;     // return vsym
  WST_IDX _default_vsym_wst; // default vsym

protected:
  // generic interface to unify the management of phi, chi and mu nodes
  template<WSSA_NODE_KIND _Tkind>
  typename NODE_TO_TYPES<_Tkind>::TABLE_TYPE& Get_table();

  template<WSSA_NODE_KIND _Tkind>
  const typename NODE_TO_TYPES<_Tkind>::TABLE_TYPE& Get_table() const;

  template<WSSA_NODE_KIND _Tkind>
  typename NODE_TO_TYPES<_Tkind>::WN_MAP_TYPE& Get_map();

  template<WSSA_NODE_KIND _Tkind>
  const typename NODE_TO_TYPES<_Tkind>::WN_MAP_TYPE& Get_map() const;

  template<WSSA_NODE_KIND _Tkind>
  typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* Get_node(WSSA_NODE_IDX idx);

  template<WSSA_NODE_KIND _Tkind>
  const typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* Get_node(WSSA_NODE_IDX idx) const;

  template<WSSA_NODE_KIND _Tkind>
  WSSA_NODE_IDX WN_first_node_idx(const WN* wn) const;
  
  template<typename _Tnode>
  _Tnode* WN_next_node(_Tnode* node);

  template<typename _Tnode>
  const _Tnode* WN_next_node(_Tnode* node) const;

  template<WSSA_NODE_KIND _Tkind>
  BOOL WN_has_node(const WN* wn) const;
  
  template<WSSA_NODE_KIND _Tkind>
  typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* Create_node(INT32 opnd_num = 0);

  template<typename _Tnode>
  WSSA_NODE_IDX Add_node(const WN* wn, _Tnode* node);

  template<WSSA_NODE_KIND _Tkind>
  void Clear_list(WN* wn);

  template<WSSA_NODE_KIND _Tkind>
  typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* WN_ssa_node(const WN* wn, WST_IDX wst);

  template<WSSA_NODE_KIND _Tkind>
  const typename NODE_TO_TYPES<_Tkind>::NODE_TYPE* WN_ssa_node(const WN* wn, WST_IDX wst) const;

  template<typename _Tnode>
  WSSA_NODE_ITERATOR<WHIRL_SSA_MANAGER, _Tnode> WN_node_begin(const WN* wn);

  template<typename _Tnode>
  WSSA_NODE_ITERATOR<const WHIRL_SSA_MANAGER, const _Tnode> WN_node_begin(const WN* wn) const;

protected:
  // for I/O and updater
  PHI_TABLE& PHI_table()         { return _phi_table;     }
  CHI_TABLE& CHI_table()         { return _chi_table;     }
  MU_TABLE&  MU_table()          { return _mu_table;      }

  WN_PHI_MAP& WN_phi_map()       { return _phi_map;       }
  WN_CHI_MAP& WN_chi_map()       { return _chi_map;       }
  WN_MU_MAP&  WN_mu_map()        { return _mu_map;        }
  WN_VER_MAP& WN_ver_map()       { return _wn_ver_map;    }

  WST_FIELD_TABLE& Field_table() { return _field_table;   }
  WST_VSYM_TABLE& Vsym_table()   { return _vsym_table;    }
  WST_SYM_TABLE& Sym_table()     { return _sym_table;     }

  WST_VER_TABLE& Ver_table()     { return _ver_table;     }

public:
  WHIRL_SSA_MANAGER(MEM_POOL* mpool);
  ~WHIRL_SSA_MANAGER();

public:
  void Clear();

  WN* Root() const { return _root; }
  void Set_root(WN* wn) { _root = wn; }

  MGR_STAT Stat() const { return _stat; }
  void Set_stat(MGR_STAT stat) { _stat = stat; }
  BOOL Is_stat_OK() const { return _stat == STAT_OK; }

public:
  // manage the ssa nodes
  PHI_NODE* Create_phi(INT32 kids);
  CHI_NODE* Create_chi();
  MU_NODE*  Create_mu ();

  WSSA_NODE_IDX Add_phi(const WN* wn, PHI_NODE* phi);
  WSSA_NODE_IDX Add_chi(const WN* wn, CHI_NODE* chi);
  WSSA_NODE_IDX Add_mu (const WN* wn, MU_NODE*  mu );

  BOOL WN_has_phi(const WN* wn) const;
  BOOL WN_has_chi(const WN* wn) const;
  BOOL WN_has_mu (const WN* wn) const;

  const PHI_NODE* WN_phi_node(const WN* wn, WST_IDX wst) const;
  const CHI_NODE* WN_chi_node(const WN* wn, WST_IDX wst) const;
  const MU_NODE*  WN_mu_node (const WN* wn, WST_IDX wst) const;
  PHI_NODE* WN_phi_node(const WN* wn, WST_IDX wst);
  CHI_NODE* WN_chi_node(const WN* wn, WST_IDX wst);
  MU_NODE*  WN_mu_node (const WN* wn, WST_IDX wst);

  phi_iterator WN_phi_begin(const WN* wn);
  phi_iterator WN_phi_end(const WN* wn);
  chi_iterator WN_chi_begin(const WN* wn);
  chi_iterator WN_chi_end(const WN* wn);
  mu_iterator  WN_mu_begin(const WN* wn);
  mu_iterator  WN_mu_end(const WN* wn);
  const_phi_iterator WN_phi_begin(const WN* wn) const;
  const_phi_iterator WN_phi_end(const WN* wn) const;
  const_chi_iterator WN_chi_begin(const WN* wn) const;
  const_chi_iterator WN_chi_end(const WN* wn) const;
  const_mu_iterator  WN_mu_begin(const WN* wn) const;
  const_mu_iterator  WN_mu_end(const WN* wn) const;

  UINT32 Phi_count() const;
  UINT32 Chi_count() const;
  UINT32 Mu_count()  const;

public:
  // U-D chain for WN
  WSSA_UD_ITERATOR WN_ud_begin(WN* wn) const;
  WSSA_UD_ITERATOR WN_ud_end(WN* wn) const;  

public:
  // interfaces for version of WHIRL node
  BOOL WN_has_ver(const WN* wn) const;
  VER_IDX Get_wn_ver(const WN* wn) const;
  void Set_wn_ver(const WN* wn, VER_IDX ver);
  WST_IDX Get_wn_ver_wst(const WN* wn) const;
  UINT32 Get_wn_ver_num(const WN* wn) const;

public:
  // interfaces for the mapping between WN_MAP_ID and WN*
  WSSA_WN_IDX WN_idx(const WN* wn) const;
  void Add_wn(WN* wn);
  WN*  Get_wn(WSSA_WN_IDX wn_idx) const;
  void Clear_wn_map();

private:
  ST_TO_WST_MAP _st_to_rwst_map;   // st -> real var in wst
  ST_TO_WST_MAP _st_to_vwst_map;   // st -> virtual var in wst
  void Add_wst_to_map(ST_TO_WST_MAP& map, ST_IDX st_idx, WST_IDX wst_idx);
  WST_IDX Find_wst(ST* st, PREG_NUM preg);
  WST_IDX Find_wst(ST* st, INT64 byte_ofst, INT64 byte_size,
                   UINT8 bit_ofst, UINT8 bit_size);
  WST_IDX Find_wst(OPERATOR opr, ST* base, 
                   INT64 byte_ofst, INT64 byte_size,
                   UINT8 bit_ofst, UINT8 bit_size);
  WN*  Find_addr_base(WN* tree);
  void Analyze_range(WN* expr, POINTS_TO* pt);
  void Analyze_lda_base(WN* expr, POINTS_TO* pt);
  void Analyze_ldid_base(WN* expr, POINTS_TO* pt);
  void Analyze_addr_arith(WN* expr, POINTS_TO* pt);
  void Analyze_addr_expr(WN* expr, POINTS_TO* pt);
  void Analyze_addr_for_memop(WN* memop, POINTS_TO* pt);
  UINT64 Desc_byte_size(WN* wn);
  WST_IDX Create_wst_for_direct_memop(WN* wn);
  WST_IDX Create_wst_for_indirect_memop(WN* wn);

public:
  // manage symbols and max version of the symbol
  WST_IDX New_wst(ST* st);
  WST_IDX New_wst(ST* st, PREG_NUM preg);
  WST_IDX New_wst(ST* st, const WST_Field_Info& field);
  WST_IDX New_wst(ST* st, const WST_Vsym_Info& vsym);
  FIELD_INFO_IDX New_field(const WST_Field_Info& field_info);
  VSYM_INFO_IDX New_vsym(const WST_Vsym_Info& vsym_info);
  WST_IDX Create_wst_for_wn(WN* wn);

  const WST_Symbol_Entry& Get_wst(WST_IDX wst_idx) const;
  WSSA_SYM_TYPE Get_wst_type(WST_IDX wst_idx) const;
  ST_IDX Get_st_idx(WST_IDX wst_idx) const;
  ST* Get_st(WST_IDX wst_idx) const;
  const WST_Field_Info& Get_field(FIELD_INFO_IDX idx) const;
  const WST_Vsym_Info& Get_vsym(VSYM_INFO_IDX idx) const;
  const char* WST_name(WST_IDX idx) const;

  const_sym_iterator Sym_begin() const;
  const_sym_iterator Sym_end() const;
  UINT32 WST_count() const;

  void Set_default_vsym(WST_IDX idx) { _default_vsym_wst = idx;  }
  WST_IDX Default_vsym()             { return _default_vsym_wst; }
  void Set_return_vsym(WST_IDX idx)  { _ret_vsym_wst = idx;      }
  WST_IDX Return_vsym()              { return _ret_vsym_wst;     }

public:
  // manage the versions
  VER_IDX New_ver(const WST_Version_Entry& ver);
  VER_IDX New_ver(WST_IDX wst_idx, const WN* def_wn, WSSA_NODE_KIND def_type);
  const WST_Version_Entry& Get_ver(VER_IDX ver_idx) const;
  WST_IDX Get_ver_wst(VER_IDX ver) const;
  UINT32  Get_ver_num(VER_IDX ver) const;
  void Update_ver(VER_IDX ver_idx, WN* def_wn, WSSA_NODE_KIND def_type);
  void Update_ver_num(VER_IDX ver_idx, UINT32 ver_num);

  UINT32 Get_max_ver(WST_IDX wst_idx) const;
  void Set_max_ver(WST_IDX wst_idx, UINT32 max_ver);
  UINT32 Next_ver(WST_IDX wst_idx);

  const_ver_iterator Ver_begin() const;
  const_ver_iterator Ver_end() const;
  UINT32 Ver_count() const;

protected:
  // for update
  void Enter_stmt(WN* tree);
  void Remove_stmt(WN* tree);

public:
  void Copy_wn_ssa(WN* dest, const WN* src);
  void Copy_tree_ssa(WN* dest, const WN* src);
  VER_IDX Create_entry_chi(ST* preg_st, PREG_NUM preg_num);

private:
  // helper functions for dump
  template<typename _Ttable, typename _Tmap>
  void Print_table_with_map(const _Ttable& tbl, const _Tmap& map, FILE* fp) const;

public:
  // dump routines
  void Print(FILE* fp) const;
  void Print_ssa(FILE* fp) const;
  void Print_wst_table(FILE* fp) const;
  void Print_wst(WST_IDX wst_idx, FILE* fp) const;
  void Print_ver_table(FILE* fp) const;
  void Print_ver(VER_IDX ver_idx, FILE* fp) const;

private: 
  // helper functions for verification
  template<typename _Tnode>
  void Verify_node(const WN* wn, const _Tnode* node) const;
  void Verify_ver(VER_IDX ver_idx) const;

public:
  // verification routines
  void Verify() const;

}; /* WHIRL_SSA_MANAGER */

//===================================================================
// WSSA_UD_ITERATOR
//  iterate the U-D chain for given WN*
//===================================================================
class WSSA_UD_ITERATOR {
private:
  const WHIRL_SSA_MANAGER* _mgr;
  std::list<VER_IDX> _ver_list;
  hash_map<INTPTR /* WN */, BOOL> _wn_visited;
  VER_IDX _cur_ver;
  const WN* _def_wn;
  WSSA_NODE_KIND _def_type;

private:
  void Next_def() {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    
    do {
      if (_ver_list.empty()) {
        _cur_ver = VER_INVALID;
        _def_wn = NULL;
        _def_type = WSSA_UNKNOWN;
        break;
      }

      _cur_ver = _ver_list.front();
      _ver_list.pop_front();
      const WST_Version_Entry& ver = _mgr->Get_ver(_cur_ver);
      _def_type = ver.Get_def_type();
      _def_wn = ver.Get_def_wn();
      if (_def_wn == NULL) {
        Is_True(_def_type == WSSA_UNKNOWN, ("invalid def type"));
        // volatile use does not have def
        // if the previous def is volatile, there is no def for current use
        VER_IDX prev_ver = ver.Prev_ver();
        Is_True(ver.Is_volatile() == TRUE ||
                (prev_ver != VER_INVALID && 
                 _mgr->Get_ver(prev_ver).Is_volatile() == TRUE), 
                ("cur use or prev def is not volatile"));
        continue;
      }
      Is_True(_def_wn != NULL && _def_type != WSSA_UNKNOWN, ("invalid def wn"));

      if (_wn_visited[(INTPTR)_def_wn] == true)
        continue;
      _wn_visited[(INTPTR)_def_wn] = true;
    
      if (_def_type == WSSA_PHI) {
        const PHI_NODE* node = _mgr->WN_phi_node(_def_wn, ver.Get_wst());
        Is_True(_cur_ver == node->Res(), ("version and phi result mismatch"));
        for (int i = 0; i < node->Opnd_count(); ++i) {
          _ver_list.push_front(node->Opnd(i));
        }
        continue;
      }
      else if (_def_type == WSSA_CHI) {
        if (WN_operator(_def_wn) == OPR_FUNC_ENTRY ||
            WN_operator(_def_wn) == OPR_ALTENTRY) {
          continue;
        }
        const CHI_NODE* node = _mgr->WN_chi_node(_def_wn, ver.Get_wst());
        Is_True(_cur_ver == node->Res(), ("version and chi result mismatch"));
        _ver_list.push_front(node->Opnd());
        break;
      }
      else {
        Is_True(_def_type == WSSA_OCC, ("invalid def type"));
        break;
      }
    } while(1);
#ifdef Is_True_On
    if (_cur_ver == VER_INVALID) {
      // reach the end of UD
      FmtAssert(_def_wn == NULL, ("invalid def wn"));
      FmtAssert(_def_type == WSSA_UNKNOWN, ("invalid def type"));
      FmtAssert(_ver_list.size() == 0, ("version list is not empty"));
    }
    else {
      FmtAssert(_def_wn != NULL, ("invalid def wn"));
      FmtAssert(_def_type == WSSA_OCC || _def_type == WSSA_CHI,
                ("invalid def type"));
      if (_def_type == WSSA_OCC) {
        // def by whirl
        FmtAssert(WSSA::WN_def_ver(_def_wn), ("invalid def wn"));
        FmtAssert(_cur_ver == _mgr->Get_wn_ver(_def_wn), 
                  ("ver and wn mismatch"));
      }
      else {
        // def by chi
        FmtAssert(WSSA::WN_has_chi(_def_wn), ("invalid def chi"));
        WST_IDX wst_idx = _mgr->Get_ver_wst(_cur_ver);
        const CHI_NODE* chi_node = _mgr->WN_chi_node(_def_wn, wst_idx);
        FmtAssert(chi_node != NULL && chi_node->Res() == _cur_ver,
                  ("ver and chi result mismatch"));
      }
    }
#endif
  }
  void Init_use(WN* wn) {
    _cur_ver = VER_INVALID;
    _def_wn = NULL;
    _def_type = WSSA_UNKNOWN;
    if (wn != NULL) {
      // disable phi node
      Is_True(!WSSA::WN_has_phi(wn), ("invalid wn node"));
      if (WSSA::WN_use_ver(wn)) {
        _ver_list.push_back(_mgr->Get_wn_ver(wn));
      }
      if (_mgr->WN_has_mu(wn)) {
        for (WHIRL_SSA_MANAGER::const_mu_iterator mit = _mgr->WN_mu_begin(wn);
             mit != _mgr->WN_mu_end(wn);
             ++mit) {
          _ver_list.push_back(mit->Opnd());
        }
      }
      FmtAssert(_ver_list.size() > 0, ("no def for wn"));
      Next_def();
    }
  }

public:
  WSSA_UD_ITERATOR(const WHIRL_SSA_MANAGER* mgr, WN* wn)
    : _mgr(mgr) {
    Init_use(wn);
  }
  WSSA_UD_ITERATOR(const WSSA_UD_ITERATOR& rit) :
    _mgr(rit._mgr), _ver_list(rit._ver_list), _wn_visited(rit._wn_visited),
    _cur_ver(rit._cur_ver), _def_wn(rit._def_wn), _def_type(rit._def_type) {
  }

  const WN* Def_wn() const {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid current version"));
    Is_True(_def_wn, ("invalid def wn"));
    Is_True(_def_type, ("invalid def type"));
    return _def_wn;
  }
  WSSA_NODE_KIND Def_type() {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid current version"));
    Is_True(_def_wn, ("invalid def wn"));
    Is_True(_def_type, ("invalid def type"));
    return _def_type;
  }
  void Skip_cur_wst() {
    Is_True(_cur_ver != VER_INVALID, ("invalid current version"));
    WST_IDX cur_wst = _mgr->Get_ver_wst(_cur_ver);
    do {
      VER_IDX next_ver = _ver_list.front();
      WST_IDX next_wst = _mgr->Get_ver_wst(next_ver);
      if (next_wst != cur_wst) {
        break;
      }
      _ver_list.pop_front();
    } while(_ver_list.size() > 0);
    if (_ver_list.size() == 0) {
      // end of the iterator
      _cur_ver = VER_INVALID;
      _def_wn = NULL;
      _def_type = WSSA_UNKNOWN;
    }
      
#ifdef Is_True_On
    for (std::list<VER_IDX>::const_iterator it = _ver_list.begin(); 
         it != _ver_list.end();
         ++it) {
      FmtAssert(cur_wst != _mgr->Get_ver_wst(*it), ("errors in ver list"));
    }
#endif
  }
  WN* operator*() const {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid current version"));
    Is_True(_def_wn, ("invalid def wn"));
    Is_True(_def_type, ("invalid def type"));
    return const_cast<WN*>(_def_wn);
  }
  WSSA_UD_ITERATOR& operator++() {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("version list is empty"));
    Is_True(_def_wn != NULL, ("def wn is NULL"));
    Is_True(_def_type != WSSA_UNKNOWN, ("def type is wrong"));
    Next_def();
    return *this;
  }
  bool operator==(const WSSA_UD_ITERATOR& rit) {
    return (_mgr == rit._mgr) && (_ver_list == rit._ver_list) && 
           (_cur_ver == rit._cur_ver) &&
           (_def_wn == rit._def_wn) && (_def_type == rit._def_type);
  }
  bool operator!=(const WSSA_UD_ITERATOR& rit) {
    return (_mgr != rit._mgr) || (_ver_list != rit._ver_list) ||
           (_cur_ver != rit._cur_ver) ||
           (_def_wn != rit._def_wn) || (_def_type != rit._def_type);
  }
  WSSA_UD_ITERATOR& operator=(const WSSA_UD_ITERATOR& rit) {
    _mgr = rit._mgr;
    _ver_list = rit._ver_list;
    _wn_visited = rit._wn_visited;
    _cur_ver = rit._cur_ver;
    _def_wn = rit._def_wn;
    _def_type = rit._def_type;
  }
};

} /* namespace WSSA */

// include the implementation of the template functions
#include "wssa_mgr_template.h"

#endif /* wssa_mgr_INCLUDED */
